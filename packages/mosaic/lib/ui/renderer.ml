(* ───── Render Commands ───── *)

(* A flat array of commands built by depth-first tree traversal, executed
   sequentially. This separates layout extraction (pass 2) from drawing (pass
   3), so traversal and drawing stay decoupled. *)
module Grid = Matrix_grid
module Input = Matrix_input
module Screen = Matrix_screen
module Layout = Matrix.Text.Layout

type render_command =
  | Render of Renderable.t
  | Render_inert of Renderable.t  (** Drawn, and not found under the pointer. *)
  | Push_scissor of Grid.region
  | Pop_scissor
  | Push_opacity of float
  | Pop_opacity

(* A press held until the release of its button: the cell and the node
   pressed, which a left drag captures; whether the pointer left the pressed
   cell; and the node its release clicks, until a drag. *)
type press = {
  button : Event.Mouse.button;
  press_x : int;
  press_y : int;
  pressed : Renderable.t option;
  mutable moved : bool;
  mutable click : Renderable.t option;
}

type pointer_state = {
  mutable position : (int * int) option;
  mutable modifiers : Event.Mouse.modifier;
  mutable held : press option;
  (* The button of a refused press, held or not: its drag and release go to
     no node. *)
  mutable refused : Event.Mouse.button option;
  (* The series of presses that [Down]'s [clicks] counts: the last press's
     button, time and count, and the cell of the series' first press. *)
  mutable series_button : Event.Mouse.button;
  mutable series_time : int;
  mutable series_clicks : int;
  mutable series_x : int;
  mutable series_y : int;
}

(* ───── Text Selection ─────

   The view selection is an anchor span and a focus, in the source of text
   nodes or on cells, within the scope its press found. Each node reads what
   the selection takes of it, nothing, its text or every cell it paints, from
   the nearest declaration on it or an ancestor. A scope is the root of a
   layer, a scroll box that does not read [`Cells], or a node that reads
   [`Cells] whose parent does not. Between the ends lies the terminal's
   row-major band, derived from layout whenever a node paints or a copy
   runs. *)

(* An end: a node and the boundaries of a span of its text, counted in
   [source], the text they were found in, which an edit replaces. *)
type end_ = {
  node : Renderable.t;
  first : int;
  last : int;
  source : Layout.Source.t;
  cell : (int * int) option;
      (* An end on a cell that shows no grapheme, such as a border or the
         blanks past a line's end, of [node]: the cell, from [node]'s origin.
         It moves with [node], not with its children, and is held within
         [node]'s box. *)
}

(* The pointer gesture that drags a selection: the cell it last reported, on
   the screen, and whether it is still a single click, which the pointer has
   not left and which selects nothing. *)
type gesture = {
  mutable pointer_x : int;
  mutable pointer_y : int;
  mutable click : bool;
}

(* The anchor is fixed at the press, and follows an edit of its text alone; a
   Shift+press makes a new selection. *)
type selection = {
  scope : Renderable.t;
      (* The nearest scope at or above the node its press found. *)
  box : Renderable.t;
      (* The scroll box the press found, or the scope: a drag past its top or
         bottom edge scrolls it. *)
  mutable anchor : end_;
  mutable focus : end_;
      (* The focus at rest. While a gesture drags the selection, the focus
         is the pointer's cell, and the span of the unit there. *)
  unit : Text_unit.t;
  mutable gesture : gesture option;
}

(* The band, in the coordinates of the scope whose positions count from
   [origin]: the cells from [low_x, low_y] to the one before
   [high_x, high_y], in row-major order, and the ends whose spans it holds
   whole, the anchor's and the focus's, unless the focus is the pointer's
   cell. [forward] says whether the focus starts at or after the anchor. A
   single click is [empty]. *)
type band = {
  origin : Renderable.t;
  low_x : int;
  low_y : int;
  high_x : int;
  high_y : int;
  anchor_span : end_;
  focus_span : end_ option;
  forward : bool;
  empty : bool;
  low_box : (Renderable.t * int * int) option;
  high_box : (Renderable.t * int * int) option;
      (* An end that a window has scrolled out of view stands, for the band,
         at the edge of that window's view: the window and the end's own
         cell, for the text of the window between the two. *)
}

type selection_change = Settled of string | Cleared

type selection_state = {
  mutable current : selection option;
  (* Whether the selection held text when it was last made, settled or
     extended: one that held text and holds none is [Cleared]. A
     gesture's motion does not set it. *)
  mutable held : bool;
  (* The changes not yet taken, oldest first. *)
  changes : selection_change Queue.t;
  mutable painting : bool;
  (* The band of the paint in progress, derived once for all its nodes. *)
  mutable band : band option;
}

(* Scroll movements reported since the last presentation. A hardware scroll
   hint is only safe when exactly one scroll container moved, purely
   vertically, within one stable row region — anything else disqualifies the
   whole cycle and presentation falls back to the ordinary cell diff. *)
type scroll_hint_state =
  | Hint_none
  | Hint_one of { num : int; region : Grid.region; mutable delta : int }
  | Hint_disqualified

(* ───── Types ───── *)

type t = {
  ctx : Renderable.Private.context;
  screen : Screen.t;
  (* [true] when the screen was adopted from a host runtime; presentation
     then belongs to the host, and [render] refuses to run. *)
  adopted : bool;
  root : Renderable.t;
  tree : unit Toffee.tree;
  (* Node registry: maps numeric IDs to renderables for hit testing. *)
  node_map : (int, Renderable.t) Hashtbl.t;
  (* Reverse index: maps Toffee.Node_id index to renderable for O(1) measure
     lookups. *)
  toffee_map : (int, Renderable.t) Hashtbl.t;
  (* Focus — shared ref between record and context closures (single source of
     truth). *)
  focused : Renderable.t option ref;
  (* Whether the node [build_commands] is at is inert. *)
  mutable walk_inert : bool;
  (* Lifecycle *)
  lifecycle_set : (int, Renderable.t) Hashtbl.t;
  (* Worker notifications only mutate this scheduling flag. *)
  dirty : bool Atomic.t;
  (* Available space of the last layout compute, as (width, height).
     Layout is recomputed only when this changes or the Toffee root is dirty
     (any set_style/measure/mark_dirty/attach/detach marks ancestors up to
     the root), skipping the full sizing and rounding passes on clean
     frames. *)
  mutable last_layout : (int * int) option;
  (* The screen's width method when the layout was last computed. *)
  mutable layout_width_method : Matrix.Text.width_method;
  mutable layout_cell_size : (int * int) option;
  (* The frame laid out by [layout_frame] and not yet painted. *)
  mutable frame : frame option;
  (* Render command buffer — reused across frames. *)
  mutable commands : render_command array;
  mutable cmd_len : int;
  mutable input_geometry : (int * int) option;
  (* Hover tracking *)
  mutable hover_node : Renderable.t option;
  mutable hover_num : int;
  pointer : pointer_state;
  (* Drag capture *)
  mutable captured : Renderable.t option;
  (* Selection *)
  selection_state : selection_state;
  (* Scroll hint accumulation — shared ref with the context closure, reset
     when a presentation consumes it. *)
  scroll_hint : scroll_hint_state ref;
  (* Frame callbacks *)
  mutable frame_callbacks : (dt:float -> unit) list;
  (* Size reports of the current frame — shared with the context closure. *)
  size_round : size_round;
}

(* Size reports settle by the depth rule of CSS Resize Observer (W3C, §3.4.1
   and §3.6.1): once a layout pass delivers reports, later passes of the same
   frame deliver only reports of nodes deeper than the shallowest node already
   reported. Depth grows with each pass that delivers, so a frame's passes
   terminate even when an observer's response keeps resizing its own node.
   Refused reports stay pending; the next frame's layout starts a new round
   and delivers them. *)
and frame = { width : int; height : int; dt : float }

and size_round = {
  mutable limit : int;
      (* Reports are admitted from nodes deeper than [limit]; [-1] admits all. *)
  mutable shallowest : int;
      (* Depth of the shallowest node reported in the current pass. *)
}

module Pending = struct
  type t = { node : Renderable.t; work : Renderable.Pending.t }

  let node t = t.node
  let work t = t.work
end

type settle_result = [ `Settled | `Pending of Pending.t list ]

(* ───── Helpers ───── *)

let toffee_exn = function
  | Ok x -> x
  | Error e -> invalid_arg (Toffee.Error.to_string e)

let clip_rect_intersect (a : Grid.region) (b : Grid.region) : Grid.region =
  let x1 = max a.x b.x and y1 = max a.y b.y in
  let x2 = min (a.x + a.width) (b.x + b.width) in
  let y2 = min (a.y + a.height) (b.y + b.height) in
  { x = x1; y = y1; width = max 0 (x2 - x1); height = max 0 (y2 - y1) }

(* ───── Command Buffer ───── *)

let ensure_cmd_capacity t needed =
  let len = Array.length t.commands in
  if needed > len then (
    let cap =
      let rec grow c = if c >= needed then c else grow (c * 2) in
      grow (max 64 (len * 2))
    in
    let arr = Array.make cap Pop_scissor in
    Array.blit t.commands 0 arr 0 t.cmd_len;
    t.commands <- arr)

let emit_cmd t cmd =
  ensure_cmd_capacity t (t.cmd_len + 1);
  t.commands.(t.cmd_len) <- cmd;
  t.cmd_len <- t.cmd_len + 1

(* ───── Node Lookup ───── *)

let find_node t num = Hashtbl.find_opt t.node_map num

let rec find_focusable node =
  if Renderable.focusable node then Some node
  else
    match Renderable.parent node with
    | Some p -> find_focusable p
    | None -> None

let top_layer t = Renderable.Private.top_layer t.root

(* A node is inert under an inert node, itself included, and outside the
   modal node while there is one. *)
let inert_under modal node =
  let rec up node =
    Renderable.inert node
    ||
    match modal with
    | Some m when m == node -> false
    | Some _ | None -> (
        match Renderable.parent node with
        | Some parent -> up parent
        | None -> Option.is_some modal)
  in
  up node

(* ───── Focus ───── *)

let blur_current t =
  match !(t.focused) with
  | None -> ()
  | Some node ->
      Renderable.Private.blur_direct node;
      t.focused := None

let focus_node t node =
  if (not (Renderable.focusable node)) || inert_under (top_layer t) node then
    false
  else (
    if match !(t.focused) with Some f -> not (f == node) | None -> true then (
      blur_current t;
      ignore (Renderable.Private.focus_direct node : bool);
      t.focused := Some node);
    true)

(* ───── Paths ───── *)

(* [node] and its ancestors, innermost first. *)
let rec ancestry node =
  node
  :: (match Renderable.parent node with Some p -> ancestry p | None -> [])

(* Whether [ancestor] is [node] or one of its ancestors. *)
let rec ancestor_of ancestor node =
  ancestor == node
  ||
  match Renderable.parent node with
  | None -> false
  | Some parent -> ancestor_of ancestor parent

(* Runs each node's handlers on [event], innermost first, until propagation
   stops; a node destroyed since [nodes] was taken is skipped. *)
let deliver ~stopped emit nodes event =
  let rec go = function
    | [] -> ()
    | node :: rest ->
        if not (stopped event) then begin
          if not (Renderable.destroyed node) then emit node event;
          go rest
        end
  in
  go nodes

let deliver_mouse nodes event =
  deliver ~stopped:Event.Mouse.propagation_stopped Renderable.Private.emit_mouse
    nodes event

(* ───── Hover Tracking ───── *)

let update_hover t ~x ~y ~modifiers ~target_node ~target_num kind =
  let is_move_like =
    match kind with Event.Mouse.Move | Event.Mouse.Drag _ -> true | _ -> false
  in
  if is_move_like && Option.is_none t.captured && target_num <> t.hover_num then (
    (* Fire Out on old hover target *)
    (match t.hover_node with
    | Some old ->
        let ev = Event.Mouse.make ~x ~y ~modifiers Event.Mouse.Out in
        deliver_mouse (ancestry old) ev
    | None -> ());
    t.hover_node <- target_node;
    t.hover_num <- target_num;
    (* Fire Over on new hover target *)
    match target_node with
    | Some node ->
        let ev =
          Event.Mouse.make ~x ~y ~modifiers (Event.Mouse.Over { source = None })
        in
        deliver_mouse (ancestry node) ev
    | None -> ())

(* Recheck hover against the hit grid of the frame just built (the next hit
   grid), so it runs before the host presents the frame. The data is identical
   to what [Screen.query_hit] returns after the swap. *)
let recheck_hover t =
  if Option.is_some t.captured then ()
  else
    match t.pointer.position with
    | None -> ()
    | Some (x, y) ->
        let hit_num =
          Screen.Hit_grid.get (Screen.next_hit_grid t.screen) ~x ~y
        in
        let target_node = if hit_num > 0 then find_node t hit_num else None in
        let target_num =
          match target_node with Some n -> Renderable.uid n | None -> 0
        in
        if target_num <> t.hover_num then (
          let modifiers = t.pointer.modifiers in
          (match t.hover_node with
          | Some old ->
              let ev = Event.Mouse.make ~x ~y ~modifiers Event.Mouse.Out in
              deliver_mouse (ancestry old) ev
          | None -> ());
          t.hover_node <- target_node;
          t.hover_num <- target_num;
          match target_node with
          | Some node ->
              let ev =
                Event.Mouse.make ~x ~y ~modifiers
                  (Event.Mouse.Over { source = None })
              in
              deliver_mouse (ancestry node) ev
          | None -> ())

let rec alive_visible node =
  (not (Renderable.destroyed node))
  && Renderable.visible node
  &&
  match Renderable.parent node with
  | None -> true
  | Some parent -> alive_visible parent

(* Between frames, a point hits the last node the command list draws over it,
   as in a hit grid built from the commands: the node's box and every
   enclosing scissor contain the point. Walking the commands costs one test
   per command, where filling a grid would cost one store per covered cell. *)
let query_input_hit t ~x ~y =
  match t.input_geometry with
  | None -> Screen.query_hit t.screen ~x ~y
  | Some (width, height) ->
      if x < 0 || y < 0 || x >= width || y >= height then 0
      else begin
        let contains ~rx ~ry ~rw ~rh =
          x >= rx && x < rx + rw && y >= ry && y < ry + rh
        in
        (* [depth] counts open scissors; [outside] is the depth of the first
           one that excludes the point, or 0. *)
        let hit = ref 0 and depth = ref 0 and outside = ref 0 in
        for i = 0 to t.cmd_len - 1 do
          match t.commands.(i) with
          | Render_inert _ -> ()
          | Render node ->
              if
                !outside = 0
                && contains ~rx:(Renderable.x node) ~ry:(Renderable.y node)
                     ~rw:(Renderable.width node) ~rh:(Renderable.height node)
              then hit := Renderable.uid node
          | Push_scissor clip ->
              incr depth;
              if
                !outside = 0
                && not
                     (contains ~rx:clip.x ~ry:clip.y ~rw:clip.width
                        ~rh:clip.height)
              then outside := !depth
          | Pop_scissor ->
              if !outside = !depth then outside := 0;
              decr depth
          | Push_opacity _ | Pop_opacity -> ()
        done;
        !hit
      end

let focus_request_accepted requested target =
  match requested with
  | None -> true
  | Some focus -> (
      alive_visible focus && Renderable.focused focus
      &&
      match target with
      | None -> false
      | Some node -> (
          match find_focusable node with
          | Some ancestor -> ancestor == focus
          | None -> false))

(* ───── Text Selection ───── *)

let[@inline] cell v = int_of_float (Float.round v)
let layer_root t = Option.value (top_layer t) ~default:t.root

let is_top_layer t node =
  match top_layer t with Some m -> m == node | None -> false

(* [node]'s region: the nearest node from [node] up that declares one, else
   the root of its layer. *)
let rec region_of t node =
  if Option.is_some (Renderable.Private.region node) || is_top_layer t node then
    node
  else
    match Renderable.parent node with Some p -> region_of t p | None -> node

(* [node]'s scope: the nearest node from [node] up that is the root of its
   layer, a scroll box that does not read [`Cells], or a node that reads
   [`Cells] whose parent does not. *)
let rec scope_of t node =
  if is_top_layer t node then node
  else
    match Renderable.parent node with
    | None -> node
    | Some parent ->
        let reading = Renderable.selectable node in
        if
          (Option.is_some (Renderable.Private.region node) && reading <> `Cells)
          || (reading = `Cells && Renderable.selectable parent <> `Cells)
        then node
        else scope_of t parent

(* The node a scope's positions count from: a scroll box's content, which
   scrolls with it, or the scope itself. *)
let region_origin region =
  match Renderable.Private.region region with
  | Some decl -> decl.content
  | None -> region

(* A scope's view, on the screen: a scroll box's viewport, or the scope's own
   box. A selection's pointer is held in it, and its band clipped to it. *)
let region_view region =
  match Renderable.Private.region region with
  | Some decl -> Renderable.bounds decl.viewport
  | None -> Renderable.bounds region

(* The scroll boxes a gesture of [sel] scrolls: the one its press found,
   and the scope too when that box is a window in a scope that is a scroll
   box. *)
let scrolled sel =
  if
    (not (sel.box == sel.scope))
    && Option.is_some (Renderable.Private.region sel.scope)
    && ancestor_of sel.scope sel.box
  then [ sel.box; sel.scope ]
  else [ sel.box ]

(* The gesture of [sel] ended: the boxes it scrolled stop. *)
let settle_gesture sel =
  List.iter
    (fun box ->
      match Renderable.Private.region box with
      | Some decl -> decl.settle ()
      | None -> ())
    (scrolled sel)

let layout_size t node =
  match Toffee.layout t.tree (Renderable.Private.toffee_node node) with
  | Ok (layout : Toffee.Layout.t) ->
      (cell layout.size.width, cell layout.size.height)
  | Error _ -> (0, 0)

(* [offset t node origin] is [node]'s position from [origin]'s, in cells,
   read from the layout tree, where a node out of view has the position its
   last paint did not give it: the locations and translations of the nodes
   from [node] up to [origin], whose own translation scrolls its region and
   counts for nothing. *)
let offset t node origin =
  let rec up node x y =
    if node == origin then Some (x, y)
    else
      match Renderable.parent node with
      | None -> None
      | Some parent -> (
          match Toffee.layout t.tree (Renderable.Private.toffee_node node) with
          | Error _ -> None
          | Ok layout ->
              let tx, ty = Renderable.translate node in
              up parent
                (x + cell (Renderable.Private.location_x node layout) + tx)
                (y + cell (Renderable.Private.location_y node layout) + ty))
  in
  up node 0 0

(* [node]'s position from [origin]'s, in cells, where either holds the
   other. *)
let relative t node origin =
  match offset t node origin with
  | Some _ as at -> at
  | None -> Option.map (fun (x, y) -> (-x, -y)) (offset t origin node)

(* The rows of [node]'s text, laid out at the width its layout gives it
   now: a text no paint reached since its layout changed, out of view, is
   laid out again first, so that a copy slices it as a paint shows it. *)
let current_rows t node =
  (match Toffee.layout t.tree (Renderable.Private.toffee_node node) with
  | Ok layout when cell layout.size.width <> Renderable.width node ->
      Renderable.Private.update_from_layout node
  | Ok _ | Error _ -> ());
  Renderable.text_rows node

(* The rows of a text the view selection reaches: a node with text rows,
   that does not read [`Off] and owns neither its text nor its selection as
   an editor does. *)
let text_rows_of t node =
  match current_rows t node with
  | Some _ as rows
    when (not (Renderable.destroyed node))
         && Option.is_none (Renderable.Private.owned_text node)
         && Renderable.selectable node <> `Off ->
      rows
  | Some _ | None -> None

(* The span of [node]'s text from boundary [first] to [last], in the text
   [rows] show, as an end. *)
let end_in node (rows : Renderable.text_rows) ~first ~last =
  { node; first; last; source = Layout.source rows.layout; cell = None }

let no_source = Layout.Source.of_string ""

(* An end on the cell [x, y] of [node], counted from [node]'s origin. *)
let on_cell node ~x ~y =
  { node; first = 0; last = 0; source = no_source; cell = Some (x, y) }

(* An end on the cell [x, y] of the scope whose positions count from
   [origin], a cell [node] paints without a grapheme; a cell of [origin]
   when [node]'s position from it is unknown. *)
let cell_end t origin node ~x ~y =
  match relative t node origin with
  | Some (nx, ny) -> on_cell node ~x:(x - nx) ~y:(y - ny)
  | None -> on_cell origin ~x ~y

(* Whether the cell [x, y] is at or before [x', y'] in row-major order. *)
let[@inline] precedes x y x' y' = y < y' || (y = y' && x <= x')

(* The cell, in scope coordinates, of boundary [g] of a text whose origin is
   at [ox, oy]. *)
let locate_cell (rows : Renderable.text_rows) ~ox ~oy ~affinity g =
  let row, column = Layout.locate rows.layout { grapheme = g; affinity } in
  (ox + column, oy + row - rows.scroll_y)

(* Below a scope that does not read [`Cells], the outermost scroll box or
   node that declares [`Cells] on a node's path is a scope nested in it,
   which a selection takes whole or not at all, and which holds the node. *)
let nested node =
  Option.is_some (Renderable.Private.region node)
  || Renderable.Private.declared_selectable node = Some `Cells

(* Where a node lies for a selection in [scope]: directly in it, or through
   the windows a scope that reads [`Cells] shows of its scroll boxes; inside
   a nested scope that a scope reading otherwise takes whole or not at all;
   or outside it, or in a top layer below it. *)
type placement = Direct | Inside of Renderable.t | Outside

let placement t scope node =
  let atomic = Renderable.selectable scope <> `Cells in
  let rec up node box =
    if node == scope then match box with None -> Direct | Some b -> Inside b
    else if is_top_layer t node then Outside
    else
      match Renderable.parent node with
      | None -> Outside
      | Some parent ->
          up parent (if atomic && nested node then Some node else box)
  in
  up node None

let is_direct t scope node =
  match placement t scope node with
  | Direct -> true
  | Inside _ | Outside -> false

(* The view of [node], a scroll box's or its own box, as [x, y, w, h] in the
   coordinates of the scope that counts from [origin]. *)
let view_of t origin node =
  let viewport =
    match Renderable.Private.region node with
    | Some decl -> decl.viewport
    | None -> node
  in
  match offset t viewport origin with
  | None -> None
  | Some (x, y) ->
      let w, h = layout_size t viewport in
      Some (x, y, w, h)

(* Whether the band covers every cell of [x, y, w, h]. *)
let covers band (x, y, w, h) =
  w > 0 && h > 0
  && precedes band.low_x band.low_y x y
  && precedes (x + w) (y + h - 1) band.high_x band.high_y

let clamp v lo n = if n <= 0 then v else max lo (min v (lo + n - 1))

(* A culling node lists the children on rows [lo, hi] alone, in its own
   coordinates. Its cross axis is left unbounded. *)
let unbounded = 1 lsl 40

(* The windows between [node] and [scope], innermost first, with their views
   in the coordinates that count from [origin]: below a scope that reads
   [`Cells], the scroll boxes, which show some of what they hold. *)
let windows_of t scope origin node =
  let rec up n acc =
    if n == scope then List.rev acc
    else
      match Renderable.parent n with
      | None -> List.rev acc
      | Some p ->
          let acc =
            match Renderable.Private.region p with
            | Some _ when not (p == scope) -> (
                match view_of t origin p with
                | Some view -> (p, view) :: acc
                | None -> acc)
            | Some _ | None -> acc
          in
          up p acc
  in
  if Renderable.selectable scope = `Cells then up node [] else []

(* The rows the scroll boxes [boxes] show of what they hold. *)
let clip_rows boxes =
  List.fold_left
    (fun (top, bottom) (_, (_, y, _, h)) -> (max top y, min bottom (y + h - 1)))
    (min_int, max_int) boxes

(* Where the cell [x, y] of an end stands for the band: at the edge of each
   scroll box of [boxes] whose view it has scrolled out of, the corner its
   view starts at when above and the cell after its view when below, and,
   for an end on a cell ([columns]), the view's first column when left of it
   and the column after it when right of it; with the innermost such box. *)
let clamp_end ~columns boxes x y =
  let rec go boxes x y clamped =
    match boxes with
    | [] -> (x, y, clamped)
    | (box, (vx, vy, vw, vh)) :: rest ->
        let first = match clamped with None -> Some box | Some _ -> clamped in
        if y < vy then go rest vx vy first
        else if y > vy + vh - 1 then go rest (vx + vw) (vy + vh - 1) first
        else if columns && (x < vx || x > vx + vw) then
          go rest (max vx (min x (vx + vw))) y first
        else go rest x y clamped
  in
  go boxes x y None

(* The range the band selects in a text directly in its scope whose origin
   is at [ox, oy]: from the cover of its first cell to that of its last, and
   the span of each end in the text whole. *)
let range_in band node (rows : Renderable.text_rows) ~ox ~oy ~clip:(top, bottom)
    =
  let cover ~x ~y =
    Layout.coverage rows.layout ~row:(y - oy + rows.scroll_y) ~column:(x - ox)
  in
  (* In the window an end has scrolled out of, the band reaches the end
     itself; elsewhere it stops at the rows the text's windows show. *)
  let inside = function
    | Some (box, x, y) when ancestor_of box node -> Some (x, y)
    | Some _ | None -> None
  in
  let low_x, low_y, top =
    match inside band.low_box with
    | Some (x, y) -> (x, y, min_int)
    | None -> (band.low_x, band.low_y, top)
  and high_x, high_y, bottom =
    match inside band.high_box with
    | Some (x, y) -> (x, y, max_int)
    | None -> (band.high_x, band.high_y, bottom)
  in
  if high_y < top || low_y > bottom then None
  else
    let low_x, low_y = if low_y < top then (ox - 1, top) else (low_x, low_y) in
    let high_x, high_y =
      if high_y > bottom then (ox + unbounded, bottom) else (high_x, high_y)
    in
    let first = fst (cover ~x:low_x ~y:low_y)
    and last = snd (cover ~x:(high_x - 1) ~y:high_y) in
    (* A cell end spans no text. *)
    let widen (e : end_) (first, last) =
      if e.node == node && Option.is_none e.cell then
        (min first e.first, max last e.last)
      else (first, last)
    in
    let first, last =
      match band.focus_span with
      | Some focus -> widen focus (widen band.anchor_span (first, last))
      | None -> widen band.anchor_span (first, last)
    in
    if first < last then Some (first, last) else None

let whole (rows : Renderable.text_rows) =
  let n = Layout.Source.length (Layout.source rows.layout) in
  if n > 0 then Some (0, n) else None

(* The band over the view of [scope], in the coordinates that count from its
   origin: what a selection takes of a nested scope that reads [`Cells] when
   it takes all of it, every cell it shows. *)
let view_band scope =
  let origin = region_origin scope and view = region_view scope in
  let x = view.x - Renderable.x origin and y = view.y - Renderable.y origin in
  let corner = on_cell origin ~x ~y in
  {
    origin;
    low_x = x;
    low_y = y;
    high_x = x + view.width;
    high_y = y + view.height - 1;
    anchor_span = corner;
    focus_span = None;
    forward = true;
    empty = false;
    low_box = None;
    high_box = None;
  }

(* The band that takes [scope] whole: none, for all of its text, rows out of
   view included, when it does not read [`Cells]; else its view's. *)
let whole_band scope =
  match Renderable.selectable scope with
  | `Cells -> Some (view_band scope)
  | `Off | `Text -> None

(* The rows a node spans, in scope coordinates. *)
type extent = { node : Renderable.t; top : int; bottom : int }

(* What a scope holds of a band: a text's selected range, or a nested scope
   it covers, which is taken whole. [chain] is the extents of the nodes from
   it up to the scope, innermost first. [order] is its place in the tree. *)
type item =
  | Text_item of {
      node : Renderable.t;
      rows : Renderable.text_rows;
      ox : int;
      oy : int;
      first : int;
      last : int;
      chain : extent list;
      order : int;
    }
  | Box_item of {
      box : Renderable.t;
      view : int * int * int * int;
      chain : extent list;
      order : int;
    }

(* [items t ~scope band] is what [scope] holds of [band] in tree order, or of
   all of its text when [band] is [None]. A text in a window holds the rows
   the window shows, and, in the window an end has scrolled out of, the rows
   between the end and the window's view. Only the children of a culling node
   that reach the band's rows are visited, so the cost grows with what the
   band crosses. *)
let items t ~scope band =
  let origin = region_origin scope in
  let atomic = Renderable.selectable scope <> `Cells in
  let acc = ref [] and order = ref 0 in
  let rec visit node ~ox ~oy ~clip chain =
    if
      Renderable.visible node
      && (not (Renderable.destroyed node))
      && not ((not (node == origin)) && is_top_layer t node)
    then begin
      let _, h = layout_size t node in
      let chain = { node; top = oy; bottom = oy + h } :: chain in
      if atomic && (not (node == scope)) && nested node then
        match view_of t node node with
        | Some (vx, vy, vw, vh) ->
            let view = (ox + vx, oy + vy, vw, vh) in
            let covered =
              match band with None -> true | Some b -> covers b view
            in
            if covered then begin
              incr order;
              acc :=
                Box_item { box = node; view; chain; order = !order } :: !acc
            end
        | None -> ()
      else begin
        (match text_rows_of t node with
        | None -> ()
        | Some rows -> (
            let range =
              match band with
              | None -> whole rows
              | Some b -> range_in b node rows ~ox ~oy ~clip
            in
            match range with
            | None -> ()
            | Some (first, last) ->
                incr order;
                acc :=
                  Text_item
                    { node; rows; ox; oy; first; last; chain; order = !order }
                  :: !acc));
        let clip =
          match Renderable.Private.region node with
          | Some _ when not (node == scope) -> (
              match view_of t node node with
              | Some (_, vy, _, vh) ->
                  (max (fst clip) (oy + vy), min (snd clip) (oy + vy + vh - 1))
              | None -> clip)
          | Some _ | None -> clip
        in
        visit_children node ~ox ~oy ~clip chain
      end
    end
  and visit_children node ~ox ~oy ~clip chain =
    let child c =
      match Toffee.layout t.tree (Renderable.Private.toffee_node c) with
      | Error _ -> ()
      | Ok layout ->
          let tx, ty = Renderable.translate c in
          visit c
            ~ox:(ox + cell (Renderable.Private.location_x c layout) + tx)
            ~oy:(oy + cell (Renderable.Private.location_y c layout) + ty)
            ~clip chain
    in
    match band with
    | Some b when Renderable.viewport_cull node ->
        let reach default = function Some (_, _, y) -> y | None -> default in
        let lo = min b.low_y (reach b.low_y b.low_box)
        and hi = max b.high_y (reach b.high_y b.high_box) in
        let viewport : Grid.region =
          {
            x = Renderable.x node - unbounded;
            y = Renderable.y node + (lo - oy);
            width = 2 * unbounded;
            height = hi - lo + 1;
          }
        in
        Renderable.Private.iter_children_in_viewport ~parent:node ~viewport
          child
    | Some _ | None -> Renderable.Private.iter_children_z node child
  in
  visit origin ~ox:0 ~oy:0 ~clip:(min_int, max_int) [];
  List.rev !acc

(* The first and last cells of the box [x, y, w, h] at or after, and before,
   the cell [cx, cy], in row-major order. *)
let first_from ~cx ~cy x y w h =
  if y > cy then Some (x, y)
  else if y + h - 1 < cy then None
  else if cx <= x + w - 1 then Some (max cx x, cy)
  else if cy + 1 <= y + h - 1 then Some (x, cy + 1)
  else None

let last_before ~cx ~cy x y w h =
  if y + h - 1 < cy then Some (x + w - 1, y + h - 1)
  else if y > cy then None
  else if cx - 1 >= x then Some (min (cx - 1) (x + w - 1), cy)
  else if cy - 1 >= y then Some (x + w - 1, cy - 1)
  else None

(* Of [texts], given as their boxes [(node, x, y, w, h)] in scope
   coordinates, the first with a cell at or after the cell [cx, cy] in
   row-major order, and the last with a cell before it, each with that
   cell; the earlier in [texts] on a tie. *)
let first_after ~cx ~cy texts =
  List.fold_left
    (fun best ((_, x, y, w, h) as text) ->
      if w <= 0 || h <= 0 then best
      else
        match (first_from ~cx ~cy x y w h, best) with
        | Some (x, y), Some (_, bx, by) when precedes bx by x y -> best
        | Some (x, y), _ -> Some (text, x, y)
        | None, _ -> best)
    None texts

let last_before_of ~cx ~cy texts =
  List.fold_left
    (fun best ((_, x, y, w, h) as text) ->
      if w <= 0 || h <= 0 then best
      else
        match (last_before ~cx ~cy x y w h, best) with
        | Some (x, y), Some (_, bx, by) when precedes x y bx by -> best
        | Some (x, y), _ -> Some (text, x, y)
        | None, _ -> best)
    None texts

(* The text a press on the cell [cx, cy] of a scope finds among [texts]:
   the first at or after the cell, else the last before it. *)
let nearest ~cx ~cy texts =
  match first_after ~cx ~cy texts with
  | Some ((node, _, _, _, _), _, _) -> Some node
  | None -> (
      match last_before_of ~cx ~cy texts with
      | Some ((node, _, _, _, _), _, _) -> Some node
      | None -> None)

(* The texts directly in [scope] the last paint drew, as their boxes in scope
   coordinates: those a press can find without a walk of the scope. *)
let painted_texts t scope =
  let origin = region_origin scope in
  let ox = Renderable.x origin and oy = Renderable.y origin in
  let texts = ref [] in
  for i = t.cmd_len - 1 downto 0 do
    match t.commands.(i) with
    | Render node
      when Option.is_some (text_rows_of t node) && is_direct t scope node ->
        texts :=
          ( node,
            Renderable.x node - ox,
            Renderable.y node - oy,
            Renderable.width node,
            Renderable.height node )
          :: !texts
    | Render _ | Render_inert _ | Push_scissor _ | Pop_scissor | Push_opacity _
    | Pop_opacity ->
        ()
  done;
  !texts

(* Every text directly in [scope] that takes the pointer, for a press whose
   view shows none. *)
let scope_texts t scope =
  List.filter_map
    (function
      | Text_item { node; ox; oy; _ } when not (inert_under (top_layer t) node)
        ->
          let w, h = layout_size t node in
          Some (node, ox, oy, w, h)
      | Text_item _ | Box_item _ -> None)
    (items t ~scope None)

(* The nearest text of [scope] to its cell [cx, cy], as a press resolves one
   where no text is hit: the texts the last paint drew first, and the
   scope's others only when it drew none. A nested scope taken whole is
   passed by. *)
let resolve_cell t scope ~cx ~cy =
  match nearest ~cx ~cy (painted_texts t scope) with
  | Some _ as node -> node
  | None -> nearest ~cx ~cy (scope_texts t scope)

(* A press resolves to a text: the one under the pressed cell, else the
   nearest in its scope. *)
let resolve_press t scope ~cx ~cy hit =
  match hit with
  | Some node
    when Option.is_some (text_rows_of t node) && is_direct t scope node ->
      Some node
  | Some _ | None -> resolve_cell t scope ~cx ~cy

(* The end that the span of [unit] around the cell [cx, cy] gives in [node],
   a text directly in the scope whose positions count from [origin]: the
   unit around the graphemes the cell shows, or around the boundary nearest
   it. *)
let unit_end t origin node unit ~cx ~cy =
  match (offset t node origin, current_rows t node) with
  | Some (ox, oy), Some rows ->
      let layout = rows.layout in
      let first, last =
        Layout.coverage layout ~row:(cy - oy + rows.scroll_y) ~column:(cx - ox)
      in
      let first, last =
        Text_unit.span unit (Layout.source layout) ~first ~last
      in
      Some (end_in node rows ~first ~last)
  | _ -> None

(* The first cell of an end's span and the cell after its last, in the
   coordinates of [scope], whose positions count from [origin]. An end inside
   a nested scope taken whole spans that scope's view, and an end on a cell
   is held within its node's box. *)
let end_cells t scope origin (e : end_) =
  if not (alive_visible e.node) then None
  else
    match e.cell with
    | Some (x, y) -> (
        match relative t e.node origin with
        | Some (nx, ny) ->
            let w, h = layout_size t e.node in
            let x = nx + clamp x 0 w and y = ny + clamp y 0 h in
            Some (x, y, x + 1, y)
        | None -> None)
    | None -> (
        match placement t scope e.node with
        | Outside -> None
        | Direct -> (
            match (offset t e.node origin, current_rows t e.node) with
            | Some (ox, oy), Some rows ->
                let x, y =
                  locate_cell rows ~ox ~oy ~affinity:Layout.Downstream e.first
                in
                let x', y' =
                  locate_cell rows ~ox ~oy ~affinity:Layout.Upstream e.last
                in
                Some (x, y, x', y')
            | _ -> None)
        | Inside box -> (
            match view_of t origin box with
            | Some (x, y, w, h) -> Some (x, y, x + w, y + h - 1)
            | None -> None))

let hit_node t ~x ~y =
  let num = query_input_hit t ~x ~y in
  if num > 0 then find_node t num else None

(* The selection's band: from the earlier of its ends' first cells to the later
   of the cells after their spans, in its scope. While a gesture drags the
   selection, its focus is the pointer's cell, held in the scope's view, and
   for a word or a line the span of that unit in the text the cell resolves
   to as a press does, which the band reaches, or the cell alone over a
   nested scope taken whole. An end that a window has scrolled out of view
   stands at the edge of the window's view. An end whose node is not shown
   gives no band, though the selection clears before one is hidden or
   unmounted. *)
let band_of t sel =
  let scope = sel.scope in
  let origin = region_origin scope in
  let ends (e : end_) =
    Option.map
      (fun cells -> (cells, windows_of t scope origin e.node))
      (end_cells t scope origin e)
  in
  match ends sel.anchor with
  | None -> None
  | Some ((ax, ay, ax', ay'), anchor_boxes) -> (
      let focus =
        match sel.gesture with
        | Some g ->
            let view = region_view scope in
            let px = clamp g.pointer_x view.x view.width
            and py = clamp g.pointer_y view.y view.height in
            let x = px - Renderable.x origin and y = py - Renderable.y origin in
            let span =
              match sel.unit with
              | Grapheme -> None
              | Word | Line -> (
                  let hit = hit_node t ~x:px ~y:py in
                  match Option.map (placement t scope) hit with
                  | Some (Inside _) -> None
                  | Some (Direct | Outside) | None -> (
                      match resolve_press t scope ~cx:x ~cy:y hit with
                      | Some node -> unit_end t origin node sel.unit ~cx:x ~cy:y
                      | None -> None))
            in
            let cells =
              match span with
              | Some e -> (
                  match end_cells t scope origin e with
                  | Some (sx, sy, ex, ey) ->
                      let sx, sy =
                        if precedes sx sy x y then (sx, sy) else (x, y)
                      and ex, ey =
                        if precedes ex ey (x + 1) y then (x + 1, y) else (ex, ey)
                      in
                      (sx, sy, ex, ey)
                  | None -> (x, y, x + 1, y))
              | None -> (x, y, x + 1, y)
            in
            Some (cells, [], span)
        | None ->
            Option.map
              (fun (cells, boxes) -> (cells, boxes, Some sel.focus))
              (ends sel.focus)
      in
      match focus with
      | None -> None
      | Some ((fx, fy, fx', fy'), focus_boxes, focus_span) ->
          let columns = Option.is_some sel.anchor.cell in
          let a_lo = clamp_end ~columns anchor_boxes ax ay
          and a_hi = clamp_end ~columns anchor_boxes ax' ay' in
          let columns = Option.is_some sel.focus.cell in
          let f_lo = clamp_end ~columns focus_boxes fx fy
          and f_hi = clamp_end ~columns focus_boxes fx' fy' in
          (* The order of two ends that stand at one cell is their own. *)
          let before (x, y, _) (x', y', _) (rx, ry) (rx', ry') =
            if x = x' && y = y' then precedes rx ry rx' ry'
            else precedes x y x' y'
          in
          let forward = before a_lo f_lo (ax, ay) (fx, fy) in
          let box_of (_, _, clamped) rx ry =
            Option.map (fun box -> (box, rx, ry)) clamped
          in
          let (low_x, low_y, _), low_box =
            if forward then (a_lo, box_of a_lo ax ay)
            else (f_lo, box_of f_lo fx fy)
          in
          let (high_x, high_y, _), high_box =
            if before a_hi f_hi (ax', ay') (fx', fy') then
              (f_hi, box_of f_hi fx' fy')
            else (a_hi, box_of a_hi ax' ay')
          in
          let empty =
            match sel.gesture with Some g -> g.click | None -> false
          in
          Some
            {
              origin;
              low_x;
              low_y;
              high_x;
              high_y;
              anchor_span = sel.anchor;
              focus_span;
              forward;
              empty;
              low_box;
              high_box;
            })

(* The range [band] selects in [node], a text directly in [scope] whose rows
   are [rows]. *)
let range_under t scope band node rows =
  match offset t node band.origin with
  | Some (ox, oy) ->
      let clip = clip_rows (windows_of t scope band.origin node) in
      range_in band node rows ~ox ~oy ~clip
  | None -> None

(* The range of [node], a text inside the nested scope [box], that taking
   [box] whole selects: all of its text, or, for a nested scope that reads
   [`Cells], what [box] shows of it. *)
let rec range_taken_whole t box node rows =
  match whole_band box with
  | Some band -> range_under t box band node rows
  | None -> (
      match placement t box node with
      | Outside -> None
      | Direct -> whole rows
      | Inside inner -> range_taken_whole t inner node rows)

(* The band of the paint in progress is derived once for all its nodes. *)
let current_band t sel =
  let state = t.selection_state in
  if not state.painting then band_of t sel
  else
    match state.band with
    | Some _ as band -> band
    | None ->
        let band = band_of t sel in
        state.band <- band;
        band

let selected_range_of t node =
  match t.selection_state.current with
  | None -> None
  | Some sel -> (
      match text_rows_of t node with
      | None -> None
      | Some rows -> (
          match current_band t sel with
          | None -> None
          | Some band when band.empty -> None
          | Some band -> (
              match placement t sel.scope node with
              | Outside -> None
              | Direct -> range_under t sel.scope band node rows
              | Inside box -> (
                  match view_of t band.origin box with
                  | Some view when covers band view ->
                      range_taken_whole t box node rows
                  | Some _ | None -> None))))

(* ───── Copy ─────

   A copy reproduces the lines the user saw: the fragments of the
   texts the band selects, row by row, joined by spaces, a line break, or a
   blank line where the layout leaves a margin or gap. *)

(* A fragment: its text, the cell of its first cluster, the cell after its
   last, the nodes that enclose it, and its place in the tree. *)
type piece = {
  text : string;
  row : int;
  col : int;
  end_row : int;
  end_col : int;
  enclosed : extent list;
  rank : int;
}

let is_terminator src g =
  g < Layout.Source.length src
  &&
  match (Layout.Source.text src).[Layout.Source.byte_offset src g] with
  | '\n' | '\r' -> true
  | _ -> false

(* The nodes that enclose either of two fragments below their common
   ancestor: the extents of their chains before the tail they share. Both
   chains count up to one origin, so, aligned at that end, the first node
   they share has every node after it in common. *)
let below_common a b =
  let rec drop n l =
    if n <= 0 then l else match l with _ :: l -> drop (n - 1) l | [] -> []
  in
  let rec take n l =
    if n <= 0 then []
    else match l with e :: l -> e :: take (n - 1) l | [] -> []
  in
  let la = List.length a and lb = List.length b in
  let rec shared x y =
    match (x, y) with
    | e :: _, f :: _ when e.node == f.node -> List.length x
    | _ :: x', _ :: y' -> shared x' y'
    | _ -> 0
  in
  let k = shared (drop (la - lb) a) (drop (lb - la) b) in
  take (la - k) a @ take (lb - k) b

(* Whether a row strictly between [from] and [until] lies outside every
   extent of [extents]: a margin or a gap. *)
let gap_between extents ~from ~until =
  let sorted = List.sort (fun a b -> Int.compare a.top b.top) extents in
  let rec sweep row = function
    | [] -> row < until
    | e :: rest ->
        if row >= until then false
        else if e.top > row then true
        else sweep (max row e.bottom) rest
  in
  sweep (from + 1) sorted

(* What joins two fragments in row-major order: as many spaces as cells
   between them on one row; else a line break, and one more for each row
   strictly between them that holds a highlighted cell of the band, as a
   terminal copies a blank row, or, when none does, one more for a margin or
   a gap between them. [lit ~from ~until] is how many rows strictly between
   [from] and [until] hold a highlighted cell. *)
let separator buf ~lit (prev : piece) (next : piece) =
  if next.row = prev.end_row then
    for _ = 1 to next.col - prev.end_col do
      Buffer.add_char buf ' '
    done
  else begin
    Buffer.add_char buf '\n';
    if next.row > prev.end_row + 1 then
      match lit ~from:prev.end_row ~until:next.row with
      | 0 ->
          if
            gap_between
              (below_common prev.enclosed next.enclosed)
              ~from:prev.end_row ~until:next.row
          then Buffer.add_char buf '\n'
      | rows ->
          for _ = 1 to rows do
            Buffer.add_char buf '\n'
          done
  end

(* The rows of a text item, in scope coordinates. *)
let item_rows (rows : Renderable.text_rows) ~oy ~first ~last =
  let row g =
    fst (Layout.locate rows.layout { grapheme = g; affinity = Downstream })
  in
  (oy + row first - rows.scroll_y, oy + row (last - 1) - rows.scroll_y)

(* [iter_band band ~grid ~view f] calls [f ~x ~y idx] for each cell of
   [band] in [grid] that lies in [view], row by row, in screen coordinates,
   the tail of a wide grapheme left to its head. *)
let iter_band band ~grid ~(view : Grid.region) f =
  let ox = Renderable.x band.origin and oy = Renderable.y band.origin in
  let width = Grid.width grid and height = Grid.height grid in
  let left = max 0 view.x
  and right = min (width - 1) (view.x + view.width - 1) in
  let top = band.low_y + oy and bottom = band.high_y + oy in
  for
    y = max (max 0 view.y) top
    to min (min (height - 1) (view.y + view.height - 1)) bottom
  do
    let first = if y = top then max left (band.low_x + ox) else left
    and last = if y = bottom then min right (band.high_x + ox - 1) else right in
    for x = first to last do
      let idx = (y * width) + x in
      if not (Grid.is_continuation grid idx) then f ~x ~y idx
    done
  done

(* Whether a selection takes the cells [node] paints: it reads [`Cells], is
   no editor, whose cells belong to its own selection, and is not inert. *)
let takes_cells t node =
  Renderable.selectable node = `Cells
  && Option.is_none (Renderable.Private.owned_text node)
  && not (inert_under (top_layer t) node)

(* [iter_highlighted t ~scope band ~grid ~owner f] calls [f ~x ~y idx node]
   for each cell of [band] in [scope]'s view that the selection highlights
   as a terminal does, row by row, in [grid] whose cells' nodes [owner]
   gives: a cell of a node whose cells the selection takes, directly in
   [scope] or inside a nested scope the band covers, within that scope's
   view; and, when the selection takes [scope]'s cells, a cell no node
   paints, which reads as [scope], the [node] given for it. *)
let iter_highlighted t ~scope band ~grid ~owner f =
  let view = region_view scope in
  let unowned = takes_cells t scope in
  (* The box on the screen in which the band takes the cells of the node
     last met. *)
  let last = ref None and within = ref None in
  iter_band band ~grid ~view (fun ~x ~y idx ->
      match owner ~x ~y with
      | Some node -> (
          (match !last with
          | Some seen when seen == node -> ()
          | Some _ | None -> (
              last := Some node;
              within :=
                if not (takes_cells t node) then None
                else
                  match placement t scope node with
                  | Outside -> None
                  | Direct -> Some view
                  | Inside box -> (
                      match view_of t band.origin box with
                      | Some v when covers band v -> Some (region_view box)
                      | Some _ | None -> None)));
          match !within with
          | Some (r : Grid.region)
            when x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height
            ->
              f ~x ~y idx node
          | Some _ | None -> ())
      | None -> if unowned then f ~x ~y idx scope)

(* The extents of [node] and its ancestors up to [origin], innermost first,
   as an item's chain holds them. *)
let chain_of t origin node =
  let rec up n acc =
    let acc =
      match offset t n origin with
      | Some (_, y) ->
          { node = n; top = y; bottom = y + snd (layout_size t n) } :: acc
      | None -> acc
    in
    if n == origin then List.rev acc
    else
      match Renderable.parent n with Some p -> up p acc | None -> List.rev acc
  in
  up node []

let owner_in t hits ~x ~y =
  let num = Screen.Hit_grid.get hits ~x ~y in
  if num = Screen.Hit_grid.empty_id then None else find_node t num

(* [assemble t ~scope band] is the text of [scope]'s [items]: each text's
   range is split before each row on which another fragment also lies and
   after it, so that the fragments read row by row; a nested scope taken
   whole is one fragment at its view's first cell; a line terminator that
   begins or ends a fragment is dropped, the separator standing for it.
   When the fragments hold nothing but terminators, no separator stands for
   them: the text is all of them, as their highlight shows. *)
let rec assemble t ~scope band =
  let items = items t ~scope band in
  (* How many items lie on each row. *)
  let lo = ref max_int and hi = ref min_int in
  let span = function
    | Text_item i -> item_rows i.rows ~oy:i.oy ~first:i.first ~last:i.last
    | Box_item { view = _, y, _, h; _ } -> (y, y + h - 1)
  in
  List.iter
    (fun item ->
      let r0, r1 = span item in
      lo := min !lo r0;
      hi := max !hi r1)
    items;
  let counts = if !lo > !hi then [||] else Array.make (!hi - !lo + 2) 0 in
  List.iter
    (fun item ->
      let r0, r1 = span item in
      counts.(r0 - !lo) <- counts.(r0 - !lo) + 1;
      counts.(r1 + 1 - !lo) <- counts.(r1 + 1 - !lo) - 1)
    items;
  for i = 1 to Array.length counts - 1 do
    counts.(i) <- counts.(i) + counts.(i - 1)
  done;
  let shared r = r >= !lo && r <= !hi && counts.(r - !lo) > 1 in
  let pieces = ref [] in
  let breaks = Buffer.create 2 and only_breaks = ref true in
  let add_piece ~src ~rows ~ox ~oy ~enclosed ~rank p0 p1 =
    if !only_breaks then begin
      let g = ref p0 in
      while !g < p1 && is_terminator src !g do
        incr g
      done;
      if !g < p1 then only_breaks := false
      else Buffer.add_string breaks (Layout.Source.slice src ~first:p0 ~last:p1)
    end;
    let p0 = if is_terminator src p0 then p0 + 1 else p0 in
    let p1 = if p1 > p0 && is_terminator src (p1 - 1) then p1 - 1 else p1 in
    if p0 < p1 then begin
      let col, row = locate_cell rows ~ox ~oy ~affinity:Layout.Downstream p0 in
      let end_col, end_row =
        locate_cell rows ~ox ~oy ~affinity:Layout.Upstream p1
      in
      (* A space that hangs past its row's edge at a soft wrap takes no
         column, though it is copied: it counts as the cell it stands for,
         so that the separator does not count it again. *)
      let rec hanging g n =
        if
          g > p0
          && Layout.Source.slice src ~first:(g - 1) ~last:g = " "
          && fst (locate_cell rows ~ox ~oy ~affinity:Layout.Upstream (g - 1))
             = end_col
        then hanging (g - 1) (n + 1)
        else n
      in
      let end_col = end_col + hanging p1 0 in
      pieces :=
        {
          text = Layout.Source.slice src ~first:p0 ~last:p1;
          row;
          col;
          end_row;
          end_col;
          enclosed;
          rank;
        }
        :: !pieces
    end
  in
  List.iter
    (function
      | Text_item i ->
          let src = Layout.source i.rows.layout in
          let r0, r1 = item_rows i.rows ~oy:i.oy ~first:i.first ~last:i.last in
          let from = ref i.first in
          for r = r0 + 1 to r1 do
            if shared r || shared (r - 1) then begin
              let g =
                Layout.row_start i.rows.layout (r - i.oy + i.rows.scroll_y)
              in
              add_piece ~src ~rows:i.rows ~ox:i.ox ~oy:i.oy ~enclosed:i.chain
                ~rank:i.order !from g;
              from := g
            end
          done;
          add_piece ~src ~rows:i.rows ~ox:i.ox ~oy:i.oy ~enclosed:i.chain
            ~rank:i.order !from i.last
      | Box_item { box; view = x, y, w, h; chain; order } ->
          let text = assemble t ~scope:box (whole_band box) in
          if text <> "" then begin
            only_breaks := false;
            pieces :=
              {
                text;
                row = y;
                col = x;
                end_row = y + h - 1;
                end_col = x + w;
                enclosed = chain;
                rank = order;
              }
              :: !pieces
          end)
    items;
  (* The cells that nodes directly in the scope paint without text, as the
     frame on the screen shows them: each run of cells that are not blank is
     a fragment, so the separators keep the blanks between them and none at
     a row's end. Whether each row holds a highlighted cell, blank or not, is
     [lit_rows], from the row [lit_top]. *)
  let lit_rows = ref [||] and lit_top = ref 0 in
  (match band with
  | None -> ()
  | Some band ->
      let grid = Screen.current_grid t.screen in
      let ox = Renderable.x band.origin and oy = Renderable.y band.origin in
      let view = region_view scope in
      let top = max (max 0 view.y) (band.low_y + oy)
      and bottom =
        min
          (min (Grid.height grid - 1) (view.y + view.height - 1))
          (band.high_y + oy)
      in
      lit_rows := Array.make (max 0 (bottom - top + 1)) false;
      lit_top := top - oy;
      let run = Buffer.create 16 in
      let run_y = ref 0 and run_x = ref 0 and next_x = ref (-1) in
      let run_chain = ref [] in
      let flush () =
        if Buffer.length run > 0 then begin
          only_breaks := false;
          pieces :=
            {
              text = Buffer.contents run;
              row = !run_y - oy;
              col = !run_x - ox;
              end_row = !run_y - oy;
              end_col = !next_x - ox;
              enclosed = !run_chain;
              rank = 0;
            }
            :: !pieces;
          Buffer.clear run
        end;
        next_x := -1
      in
      (* Whether the node last met paints cells the copy takes as drawn. *)
      let last = ref None and drawn = ref false in
      iter_highlighted t ~scope band ~grid
        ~owner:(fun ~x ~y ->
          let num = Screen.query_hit t.screen ~x ~y in
          if num = Screen.Hit_grid.empty_id then None else find_node t num)
        (fun ~x ~y idx node ->
          !lit_rows.(y - top) <- true;
          (match !last with
          | Some seen when seen == node -> ()
          | Some _ | None ->
              last := Some node;
              drawn :=
                Option.is_none (Renderable.text_rows node)
                && is_direct t scope node);
          if !drawn then
            let text = Grid.get_text grid idx in
            if text = "" || text = " " then flush ()
            else begin
              if not (y = !run_y && x = !next_x) then begin
                flush ();
                run_y := y;
                run_x := x;
                run_chain := chain_of t band.origin node
              end;
              Buffer.add_string run text;
              next_x := x + max 1 (Grid.cell_width grid idx)
            end);
      flush ());
  let lit ~from ~until =
    let rows = !lit_rows and top = !lit_top in
    let n = ref 0 in
    for
      r = max (from + 1) top to min (until - 1) (top + Array.length rows - 1)
    do
      if rows.(r - top) then incr n
    done;
    !n
  in
  let pieces =
    List.sort
      (fun a b ->
        match Int.compare a.row b.row with
        | 0 -> (
            match Int.compare a.col b.col with
            | 0 -> Int.compare a.rank b.rank
            | c -> c)
        | c -> c)
      !pieces
  in
  let buf = Buffer.create 256 in
  ignore
    (List.fold_left
       (fun prev piece ->
         Option.iter (fun prev -> separator buf ~lit prev piece) prev;
         Buffer.add_string buf piece.text;
         Some piece)
       None pieces
      : piece option);
  if !only_breaks then Buffer.contents breaks else Buffer.contents buf

(* The text of [sel], if it holds any. *)
let text_of t sel =
  match band_of t sel with
  | Some band when not band.empty -> (
      match assemble t ~scope:sel.scope (Some band) with
      | "" -> None
      | text -> Some text)
  | Some _ | None -> None

(* Whether [sel] holds text, that is whether [text_of] is not [None],
   without the copy where it can: a grapheme other than a line terminator,
   which no fragment drops, is text; a band of terminators alone, which its
   fragments may drop, is copied to see. *)
let holds_text t sel =
  match band_of t sel with
  | Some band when not band.empty ->
      let found = items t ~scope:sel.scope (Some band) in
      let rec has_grapheme = function
        | Text_item { rows; first; last; _ } ->
            let src = Layout.source rows.layout in
            let rec from g =
              g < last && ((not (is_terminator src g)) || from (g + 1))
            in
            from first
        | Box_item { box; _ } ->
            List.exists has_grapheme (items t ~scope:box (whole_band box))
      in
      List.exists has_grapheme found
      || assemble t ~scope:sel.scope (Some band) <> ""
  | Some _ | None -> false

let view_text t =
  match t.selection_state.current with
  | None -> None
  | Some sel -> text_of t sel

let selection_text t =
  match view_text t with
  | Some _ as text -> text
  | None -> (
      match !(t.focused) with
      | Some node when Renderable.focused node -> (
          match Renderable.Private.owned_text node with
          | Some selected -> selected ()
          | None -> None)
      | Some _ | None -> None)

(* A selection behind the modal node is inert: no key acts on it. *)
let inert_selection t sel = inert_under (top_layer t) sel.anchor.node

let selection_live t =
  match t.selection_state.current with
  | Some sel -> (not (inert_selection t sel)) && holds_text t sel
  | None -> false

let take_selection_changes t =
  let changes = t.selection_state.changes in
  if Queue.is_empty changes then []
  else begin
    let taken = List.of_seq (Queue.to_seq changes) in
    Queue.clear changes;
    taken
  end

(* ───── Gestures ───── *)

(* [replace t next ~holds] makes [next] the view selection, [holds] saying
   whether it holds text, and ends the gesture of the one it replaces. The
   change is [Cleared] when [next] holds no text and the one it replaces
   held some, when it was last made, settled or extended, or under its
   gesture now. *)
let replace t next ~holds =
  let state = t.selection_state in
  let held =
    match state.current with
    | None -> false
    | Some sel ->
        let held =
          state.held || (Option.is_some sel.gesture && holds_text t sel)
        in
        if Option.is_some sel.gesture then settle_gesture sel;
        held
  in
  state.current <- next;
  state.held <- holds;
  if held && not holds then Queue.push Cleared state.changes;
  t.ctx.schedule ()

let clear_selection t =
  if Option.is_some t.selection_state.current then replace t None ~holds:false

(* What an end takes on a cell of [scope] that shows no text, painted by
   [owner] or by no node: the nearest node at or above the owner, in
   [scope], that reads [`Text] or [`Cells] and is no editor, with that
   reading. A nested scope taken whole stands for its parent, and a cell no
   node paints reads as [scope]. [None] when no node is such. *)
let cell_reader t scope owner =
  let rec up node =
    match Renderable.selectable node with
    | (`Text | `Cells) as reading
      when Option.is_none (Renderable.Private.owned_text node) ->
        Some (node, reading)
    | `Off | `Text | `Cells -> (
        if node == scope then None
        else match Renderable.parent node with Some p -> up p | None -> None)
  in
  match owner with
  | None -> up scope
  | Some node -> (
      match placement t scope node with
      | Outside -> None
      | Direct -> up node
      | Inside box -> Option.bind (Renderable.parent box) up)

(* Whether an end on the cell [x, y] of [scope], whose positions count from
   [origin], lies in the text of [node], which paints the cell: [node] is a
   text directly in [scope] that does not read [`Cells], or one that does
   and shows a grapheme there. Past a line's end, a text that reads [`Cells]
   shows blanks, as a terminal's row does. *)
let on_text t scope origin node ~x ~y =
  match text_rows_of t node with
  | Some rows when is_direct t scope node -> (
      Renderable.selectable node <> `Cells
      ||
      match offset t node origin with
      | Some (ox, oy) ->
          let first, last =
            Layout.coverage rows.layout
              ~row:(y - oy + rows.scroll_y)
              ~column:(x - ox)
          in
          first < last
      | None -> false)
  | Some _ | None -> false

(* A press of [clicks] starts a selection in the scope of the node it finds,
   or in the top layer when it finds none. One click on a cell that shows no
   grapheme, of a node that reads [`Cells], or no node's in a scope that
   reads [`Cells], anchors at the cell, as a terminal's does. Otherwise the
   press resolves to a text, its anchor the span of the unit those clicks
   select. A single click the pointer does not leave selects nothing; a
   double or triple click selects its unit at once. *)
let start_selection t hit ~x ~y ~clicks =
  let scope, box =
    match hit with
    | Some node -> (scope_of t node, region_of t node)
    | None -> (layer_root t, layer_root t)
  in
  let origin = region_origin scope in
  let cx = x - Renderable.x origin and cy = y - Renderable.y origin in
  let unit = Text_unit.of_clicks clicks in
  let on_cell =
    match (unit, hit) with
    | Grapheme, Some node ->
        Renderable.selectable node = `Cells
        && not (on_text t scope origin node ~x:cx ~y:cy)
    | Grapheme, None -> Renderable.selectable scope = `Cells
    | (Word | Line), _ -> false
  in
  let next =
    if on_cell then
      let node = Option.value hit ~default:scope in
      let anchor = cell_end t origin node ~x:cx ~y:cy in
      Some
        {
          scope;
          box;
          anchor;
          focus = anchor;
          unit;
          gesture = Some { pointer_x = x; pointer_y = y; click = true };
        }
    else
      match resolve_press t scope ~cx ~cy hit with
      | None -> None
      | Some node -> (
          match unit_end t origin node unit ~cx ~cy with
          | None -> None
          | Some anchor ->
              let click =
                match unit with Grapheme -> true | Word | Line -> false
              in
              Some
                {
                  scope;
                  box;
                  anchor;
                  focus = anchor;
                  unit;
                  gesture = Some { pointer_x = x; pointer_y = y; click };
                })
  in
  let holds =
    match next with
    | Some ({ unit = Word | Line; _ } as sel) -> holds_text t sel
    | Some { unit = Grapheme; _ } | None -> false
  in
  replace t next ~holds

(* A Shift+press moves the end of [sel] nearer the press, in row-major distance
   and the focus on a tie, and makes the other end the anchor. The press starts
   a gesture, whose pointer is held in the scope's view, so the selection takes
   the unit at the press. The unit is kept, unless the press is a double or
   triple click, which sets it. *)
let move_nearer_end t sel band ~x ~y ~clicks =
  let view = region_view sel.scope in
  let x = clamp x view.x view.width and y = clamp y view.y view.height in
  let origin = band.origin in
  let width = max 1 (fst (layout_size t origin)) in
  let at x y = (y * width) + x in
  let press = at (x - Renderable.x origin) (y - Renderable.y origin) in
  let to_low = abs (press - at band.low_x band.low_y)
  and to_high = abs (press - (at band.high_x band.high_y - 1)) in
  (* The focus is the low end of a band that runs backward. *)
  let moves_focus =
    to_low = to_high || Bool.equal (to_low < to_high) (not band.forward)
  in
  let anchor = if moves_focus then sel.anchor else sel.focus in
  let unit = if clicks >= 2 then Text_unit.of_clicks clicks else sel.unit in
  let next =
    {
      scope = sel.scope;
      box = sel.box;
      anchor;
      focus = anchor;
      unit;
      gesture = Some { pointer_x = x; pointer_y = y; click = false };
    }
  in
  replace t (Some next) ~holds:(holds_text t next)

(* A left press on a node that does not read [`Off] starts a selection, or,
   with Shift, moves the nearer end of the selection there is; a press on an
   editor clears the view's, whose selection an editor takes no part in; a
   press on a node that reads [`Off], an editor's included, changes nothing.
   Ctrl changes nothing. *)
let press_selection t ~x ~y ~shift ~clicks =
  let hit = hit_node t ~x ~y in
  match hit with
  | Some node when Renderable.selectable node = `Off -> ()
  | Some node when Option.is_some (Renderable.Private.owned_text node) ->
      clear_selection t
  | Some _ | None ->
      let extended =
        match t.selection_state.current with
        | Some sel when shift && not (inert_selection t sel) -> (
            match band_of t sel with
            | Some band ->
                move_nearer_end t sel band ~x ~y ~clicks;
                true
            | None -> false)
        | Some _ | None -> false
      in
      if not extended then start_selection t hit ~x ~y ~clicks

(* The pointer of a gesture moves its selection's focus, and the boxes the
   gesture scrolls move while the pointer holds near or past their edges:
   each up or down while the pointer, held in the scope's view, lies within
   its columns, and sideways while it lies within its rows, so that past its
   corner it scrolls on neither axis. A gesture the pointer has left is no
   longer a click. *)
let drag_selection t ~x ~y =
  match t.selection_state.current with
  | Some ({ gesture = Some g; _ } as sel) ->
      g.pointer_x <- x;
      g.pointer_y <- y;
      g.click <- false;
      let view = region_view sel.scope in
      let held_x = clamp x view.x view.width
      and held_y = clamp y view.y view.height in
      List.iter
        (fun box ->
          match Renderable.Private.region box with
          | Some decl ->
              let b = Renderable.bounds box in
              let within v lo n = v >= lo && v < lo + n in
              decl.drag
                ~x:(if within held_y b.y b.height then Some x else None)
                ~y:(if within held_x b.x b.width then Some y else None)
          | None -> ())
        (scrolled sel);
      t.ctx.schedule ()
  | Some { gesture = None; _ } | None -> ()

(* The deepest text of [box], a nested scope taken whole, first or last in
   its order. *)
let extreme_text t box ~last =
  let ends =
    List.filter_map
      (function
        | Text_item { node; rows; ox; oy; first; last = stop; _ } ->
            let g = if last then stop else first in
            let x, y =
              locate_cell rows ~ox ~oy
                ~affinity:(if last then Upstream else Downstream)
                g
            in
            Some (end_in node rows ~first:g ~last:g, x, y)
        | Box_item _ -> None)
      (items t ~scope:box (whole_band box))
  in
  List.fold_left
    (fun best (e, x, y) ->
      match best with
      | Some (_, bx, by) when last && precedes x y bx by -> best
      | Some (_, bx, by) when (not last) && precedes bx by x y -> best
      | Some _ | None -> Some (e, x, y))
    None ends
  |> Option.map (fun (e, _, _) -> e)

(* When the gesture settles, its focus becomes the band's far end, in its
   fragment's node, so the highlight stays where it is: the end of the
   last fragment when the focus was after the anchor, else the start of the
   first. A nested scope covered at the far end gives the end of its own
   last or first text, which stands at the corner of its view; one that
   reads [`Cells] and shows no text, such as a chart, gives the cell at
   that corner. *)
let far_end t sel band =
  let last = band.forward in
  let ends =
    List.map
      (function
        | Text_item { node; rows; ox; oy; first; last = stop; _ } ->
            let g = if last then stop else first in
            let x, y =
              locate_cell rows ~ox ~oy
                ~affinity:(if last then Upstream else Downstream)
                g
            in
            (`Text (end_in node rows ~first:g ~last:g), x, y)
        | Box_item { box; view = x, y, w, h; _ } ->
            if last then (`Box (box, x + w - 1, y + h - 1), x + w, y + h - 1)
            else (`Box (box, x, y), x, y))
      (items t ~scope:sel.scope (Some band))
  in
  let farthest_first (_, x, y) (_, x', y') =
    if x = x' && y = y' then 0 else if precedes x y x' y' = last then 1 else -1
  in
  List.find_map
    (function
      | `Text e, _, _ -> Some e
      | `Box (box, x, y), _, _ -> (
          match extreme_text t box ~last with
          | Some _ as e -> e
          | None when Renderable.selectable box = `Cells ->
              Some (cell_end t band.origin box ~x ~y)
          | None -> None))
    (List.stable_sort farthest_first ends)

(* A selection's gesture ends with the press that drags it, released or
   lost, and the selection settles with the text it holds: a
   single click the pointer never left, or a selection that holds no text,
   clears. *)
let end_selection_drag t =
  let state = t.selection_state in
  match state.current with
  | Some ({ gesture = Some g; _ } as sel) -> (
      let band = band_of t sel in
      sel.gesture <- None;
      settle_gesture sel;
      (* A grapheme gesture released over a cell that shows no text, past a
         line's end in a text that reads [`Cells] included, keeps the
         pointer's cell as its focus where that cell reads [`Cells], on the
         node that reads so, or where nothing reads [`Text] or [`Cells] for
         it, on the scope's origin; otherwise the focus is the band's far
         end. *)
      let pointer_cell () =
        let scope = sel.scope in
        let origin = region_origin scope and view = region_view scope in
        let px = clamp g.pointer_x view.x view.width
        and py = clamp g.pointer_y view.y view.height in
        let x = px - Renderable.x origin and y = py - Renderable.y origin in
        match sel.unit with
        | Word | Line -> None
        | Grapheme -> (
            match hit_node t ~x:px ~y:py with
            | Some node when on_text t scope origin node ~x ~y -> None
            | hit -> (
                match cell_reader t scope hit with
                | Some (node, `Cells) -> Some (cell_end t origin node ~x ~y)
                | Some (_, `Text) -> None
                | None -> Some (cell_end t origin origin ~x ~y)))
      in
      let settled =
        match band with
        | Some band when not band.empty -> (
            let focus =
              match pointer_cell () with
              | Some _ as cell -> cell
              | None -> far_end t sel band
            in
            match focus with
            | Some focus ->
                sel.focus <- focus;
                text_of t sel
            | None -> None)
        | Some _ | None -> None
      in
      match settled with
      | Some text ->
          state.held <- true;
          Queue.push (Settled text) state.changes;
          t.ctx.schedule ()
      | None -> replace t None ~holds:false)
  | Some { gesture = None; _ } | None -> ()

(* ───── Keyboard Extension ───── *)

(* Boundary [g] of [node], a text whose origin's row in scope coordinates
   is [oy], as an end of no extent, with the row it lies on at [affinity]. *)
let boundary node (rows : Renderable.text_rows) ~oy ~affinity g =
  let row, _ = Layout.locate rows.layout { grapheme = g; affinity } in
  Some (end_in node rows ~first:g ~last:g, oy + row - rows.scroll_y)

(* The boundary of [node] nearest the cell [cx, cy], the cell's row held in
   the text's rows. *)
let boundary_at t node ~ox ~oy ~cx ~cy =
  match current_rows t node with
  | None -> None
  | Some rows ->
      let row =
        max 0 (min (cy - oy + rows.scroll_y) (Layout.row_count rows.layout - 1))
      in
      let g = (Layout.hit rows.layout ~row ~column:(cx - ox)).grapheme in
      Some (end_in node rows ~first:g ~last:g, oy + row - rows.scroll_y)

(* Where a focus goes past the edge of its text toward [direction], from
   the cell [cx, cy] for [`Left] and [`Right] and from the rows [top] to
   [bottom] at column [cx] for [`Up] and [`Down]: to the boundary, in
   another text of [scope] than [except], at the cell a press finds that way
   in row-major order. That is the boundary after the last cell of the
   other texts before [cx, cy], or before the first at or after it, or at
   column [cx] in the row above or below, held in the rows of the last text
   at or before the cell there or of the first at or after it. *)
let cross t scope ~box ?except ~cx ~cy ~top ~bottom direction =
  let target texts =
    let texts =
      match except with
      | Some except ->
          List.filter (fun (node, _, _, _, _) -> not (node == except)) texts
      | None -> texts
    in
    match direction with
    | `Left ->
        Option.map
          (fun ((node, ox, oy, _, _), x, y) -> (node, ox, oy, x + 1, y))
          (last_before_of ~cx ~cy texts)
    | `Right ->
        Option.map
          (fun ((node, ox, oy, _, _), x, y) -> (node, ox, oy, x, y))
          (first_after ~cx ~cy texts)
    | `Up ->
        Option.map
          (fun ((node, ox, oy, _, _), _, _) -> (node, ox, oy, cx, top - 1))
          (last_before_of ~cx:(cx + 1) ~cy:(top - 1) texts)
    | `Down ->
        Option.map
          (fun ((node, ox, oy, _, _), _, _) -> (node, ox, oy, cx, bottom + 1))
          (first_after ~cx ~cy:(bottom + 1) texts)
    | `Row_start | `Row_end -> None
  in
  (* The focus moves on in its own window [box] first, whose content goes on
     past its view where other texts may lie on the screen, and then among
     the texts of the scope. A painted text that lies that way comes before
     every text the view hides, which lie past its edge, so the scope is
     walked only when the paint drew none there. *)
  let painted = painted_texts t scope in
  let walked = lazy (scope_texts t scope) in
  let in_box texts =
    List.filter (fun (node, _, _, _, _) -> ancestor_of box node) texts
  in
  let candidates =
    (if box == scope then []
     else [ (fun () -> in_box painted); (fun () -> in_box (Lazy.force walked)) ])
    @ [ (fun () -> painted); (fun () -> Lazy.force walked) ]
  in
  let found = List.find_map (fun texts -> target (texts ())) candidates in
  Option.bind found (fun (node, ox, oy, cx, cy) ->
      boundary_at t node ~ox ~oy ~cx ~cy)

(* The innermost window [node] lies in below [scope], else [scope]. *)
let window_of t scope node =
  match windows_of t scope (region_origin scope) node with
  | (box, _) :: _ -> box
  | [] -> scope

(* Where a keyboard extension toward [direction] moves the focus of [sel],
   whose band is [band], with the focus's row in scope coordinates. In its
   text, the focus moves by a grapheme, to its column in the row above or
   below, or to its row's ends; past the edge of its text it crosses to
   another. A focus in a nested scope taken whole crosses out of it, from
   its view. *)
let extension_target t sel band direction =
  let scope = sel.scope in
  let focus = sel.focus in
  let forward = band.forward in
  match focus.cell with
  | Some _ -> (
      (* A focus on a cell moves by cells over the scope's view, and takes
         the boundary of a text it lands on. On a cell that shows no text,
         it stays a cell where that cell reads [`Cells], and crosses to the
         next text where it reads [`Text]. *)
      let origin = band.origin in
      match end_cells t scope origin focus with
      | None -> None
      | Some (x, y, _, _) -> (
          let view = region_view scope in
          let ox = Renderable.x origin and oy = Renderable.y origin in
          let sx, sy =
            match direction with
            | `Left -> (x - 1, y)
            | `Right -> (x + 1, y)
            | `Up -> (x, y - 1)
            | `Down -> (x, y + 1)
            | `Row_start -> (view.x - ox, y)
            | `Row_end -> (view.x + view.width - 1 - ox, y)
          in
          let px = clamp (sx + ox) view.x view.width
          and py = clamp (sy + oy) view.y view.height in
          let cx = px - ox and cy = py - oy in
          match hit_node t ~x:px ~y:py with
          | Some node when on_text t scope origin node ~x:cx ~y:cy -> (
              match offset t node origin with
              | Some (nx, ny) -> boundary_at t node ~ox:nx ~oy:ny ~cx ~cy
              | None -> None)
          | hit -> (
              match cell_reader t scope hit with
              | Some (node, `Cells) ->
                  Some (cell_end t origin node ~x:cx ~y:cy, cy)
              | Some (_, `Text) ->
                  cross t scope ~box:scope ~cx:x ~cy:y ~top:y ~bottom:y
                    direction
              | None -> Some (cell_end t origin origin ~x:cx ~y:cy, cy))))
  | None -> (
      match placement t scope focus.node with
      | Outside -> None
      | Inside box -> (
          match view_of t band.origin box with
          | None -> None
          | Some (x, y, w, h) ->
              let cx, cy = if forward then (x + w, y + h - 1) else (x, y) in
              cross t scope ~box:scope ~except:focus.node ~cx ~cy ~top:y
                ~bottom:(y + h - 1)
                direction)
      | Direct -> (
          match
            (offset t focus.node band.origin, current_rows t focus.node)
          with
          | Some (ox, oy), Some rows -> (
              let layout = rows.layout in
              let g = if forward then focus.last else focus.first in
              let affinity = if forward then Layout.Upstream else Downstream in
              let row, column =
                Layout.locate layout { grapheme = g; affinity }
              in
              let cx = ox + column and cy = oy + row - rows.scroll_y in
              let here = boundary focus.node rows ~oy ~affinity in
              let across () =
                cross t scope
                  ~box:(window_of t scope focus.node)
                  ~except:focus.node ~cx ~cy ~top:cy ~bottom:cy direction
              in
              let length = Layout.Source.length (Layout.source layout) in
              match direction with
              | `Left -> if g > 0 then here (g - 1) else across ()
              | `Right -> if g < length then here (g + 1) else across ()
              | `Up ->
                  if row > 0 then
                    boundary_at t focus.node ~ox ~oy ~cx ~cy:(cy - 1)
                  else across ()
              | `Down ->
                  if row < Layout.row_count layout - 1 then
                    boundary_at t focus.node ~ox ~oy ~cx ~cy:(cy + 1)
                  else across ()
              | `Row_start -> here (Layout.row_start layout row)
              | `Row_end -> here (Layout.row_end layout row))
          | _ -> None))

(* The scroll box [region] scrolls to show the row [row] of the coordinates
   that count from [origin], as little as it can. *)
let reveal region origin row =
  match Renderable.Private.region region with
  | None -> ()
  | Some decl ->
      let view = Renderable.bounds decl.viewport in
      let top = view.y - Renderable.y origin in
      let bottom = top + view.height - 1 in
      if row < top then decl.scroll (row - top)
      else if row > bottom then decl.scroll (row - bottom)

(* The anchor of a selection whose unit drops to the grapheme: its word or
   line collapses to the grapheme it starts the selection with, the first
   when the focus lies after it and the last otherwise, as a grapheme
   anchor's own grapheme stays selected. *)
let grapheme_anchor (anchor : end_) ~forward =
  if anchor.last - anchor.first <= 1 then anchor
  else if forward then { anchor with last = anchor.first + 1 }
  else { anchor with first = anchor.last - 1 }

(* The focus of a selection at rest moves, the anchor
   stays, and the unit becomes the grapheme. A selection that then holds no
   text clears; one that does is not settled again. *)
let extend_selection t direction =
  match t.selection_state.current with
  | Some ({ gesture = None; _ } as sel) when not (inert_selection t sel) -> (
      match band_of t sel with
      | Some band when not band.empty -> (
          match extension_target t sel band direction with
          | None -> ()
          | Some (focus, row) ->
              reveal (window_of t sel.scope focus.node) band.origin row;
              let anchor =
                match sel.unit with
                | Grapheme -> sel.anchor
                | Word | Line ->
                    grapheme_anchor sel.anchor ~forward:band.forward
              in
              let next = { sel with anchor; focus; unit = Grapheme } in
              let holds = holds_text t next in
              replace t (if holds then Some next else None) ~holds)
      | Some _ | None -> ())
  | Some _ | None -> ()

(* ───── Change under a Selection ─────

   Scrolling, streaming and rewrapping leave the ends where they are, since
   they name text. An edit of an end's text maps the end through the
   one span it changed; an end whose node is hidden or unmounted clears the
   selection. *)

(* The bytes [a] and [b] share from their start. *)
let common_prefix a b =
  let n = min (String.length a) (String.length b) in
  let i = ref 0 in
  while !i < n && String.unsafe_get a !i = String.unsafe_get b !i do
    incr i
  done;
  !i

(* The bytes [a] and [b] share from their end, [limit] at most. *)
let common_suffix a b ~limit =
  let la = String.length a and lb = String.length b in
  let i = ref 0 in
  while
    !i < limit
    && String.unsafe_get a (la - 1 - !i) = String.unsafe_get b (lb - 1 - !i)
  do
    incr i
  done;
  !i

(* The one span an edit changed, from the text [old] to [next]: the
   boundaries [(p, stop, stop')] such that the clusters before [p], and
   those from [stop] in [old] and from [stop'] in [next], are the same,
   with the shared prefix and suffix as long as they can be.
   Each is a boundary of both texts at the same bytes, so a cluster that
   the edit extends or splits falls inside the span. *)
let changed_span old next =
  let module S = Layout.Source in
  let a = S.text old and b = S.text next in
  let la = String.length a and lb = String.length b in
  let shared = common_prefix a b in
  let shared_end = common_suffix a b ~limit:(min la lb - shared) in
  let rec prefix g =
    let o = S.byte_offset old g in
    if
      g = 0
      || S.grapheme_of_byte next ~affinity:Upstream o = g
         && S.byte_offset next g = o
    then g
    else prefix (g - 1)
  in
  let p = prefix (S.grapheme_of_byte old ~affinity:Upstream shared) in
  let n = S.length old and n' = S.length next in
  let rec suffix g =
    if g >= n then n
    else
      let o = S.byte_offset old g + lb - la and g' = g + n' - n in
      if
        g' >= p
        && S.grapheme_of_byte next ~affinity:Downstream o = g'
        && S.byte_offset next g' = o
      then g
      else suffix (g + 1)
  in
  let stop =
    suffix (S.grapheme_of_byte old ~affinity:Downstream (la - shared_end))
  in
  (p, stop, stop + n' - n)

(* Boundary [g] through the span [(p, stop, stop')]: one before the span or
   at its start stays, so an insertion at it leaves it; one at its old end
   or after moves by the change in length; one strictly inside moves to its
   start. *)
let through (p, stop, stop') g =
  if g <= p then g else if g >= stop then g + stop' - stop else p

(* The text of [node] changed: an end in it follows the edit, from the text
   it counts in to the one [node] holds now. A selection at
   rest whose end moved clears when it holds no text, its ends met; one
   that a gesture drags keeps its anchor, followed. *)
let text_changed_of t node =
  match t.selection_state.current with
  | Some sel when sel.anchor.node == node || sel.focus.node == node -> (
      match current_rows t node with
      | None -> ()
      | Some rows ->
          let next = Layout.source rows.layout in
          let span = ref None in
          (* An end on a cell names no text, and stays on its cell. *)
          let follow (e : end_) =
            if
              (not (e.node == node))
              || Option.is_some e.cell || e.source == next
            then e
            else
              let s =
                match !span with
                | Some (from, s) when from == e.source -> s
                | Some _ | None ->
                    let s = changed_span e.source next in
                    span := Some (e.source, s);
                    s
              in
              {
                e with
                first = through s e.first;
                last = through s e.last;
                source = next;
              }
          in
          let moved (e : end_) (e' : end_) =
            e.first <> e'.first || e.last <> e'.last
          in
          let anchor = follow sel.anchor and focus = follow sel.focus in
          let changed = moved sel.anchor anchor || moved sel.focus focus in
          sel.anchor <- anchor;
          sel.focus <- focus;
          if changed && Option.is_none sel.gesture && not (holds_text t sel)
          then replace t None ~holds:false
          else t.ctx.schedule ())
  | Some _ | None -> ()

(* [node] and its descendants are about to stop being shown: a selection
   with an end among them clears, and its gesture ends. *)
let hiding_of t node =
  match t.selection_state.current with
  | Some sel
    when ancestor_of node sel.anchor.node || ancestor_of node sel.focus.node ->
      clear_selection t
  | Some _ | None -> ()

(* ───── Creation ───── *)

let create ?width_method ?screen ?style ?(request_render = fun () -> ())
    ?(executor = fun work -> ignore (Thread.create work () : Thread.t)) () =
  let tree = Toffee.new_tree () in
  let adopted = Option.is_some screen in
  let screen =
    match screen with
    | Some screen ->
        (* Adopted screens come from a host runtime; the renderer requires
           alpha blending on the grids it paints. *)
        screen
    | None -> Screen.create ?width_method ()
  in
  let node_map = Hashtbl.create 256 in
  let toffee_map = Hashtbl.create 256 in
  let lifecycle_set = Hashtbl.create 16 in
  let next_num = ref 0 in
  let dirty = Atomic.make true in
  let focused = ref None in
  let pointer =
    {
      position = None;
      modifiers = Event.Mouse.no_modifier;
      held = None;
      refused = None;
      series_button = Event.Mouse.Left;
      series_time = 0;
      series_clicks = 0;
      series_x = 0;
      series_y = 0;
    }
  in
  let selection_state =
    {
      current = None;
      held = false;
      changes = Queue.create ();
      painting = false;
      band = None;
    }
  in
  (* The renderer reads what the selection holds of a node as it paints,
     and hears of the changes that move or clear its ends. *)
  let selected_range = ref (fun _ -> None) in
  let text_changed = ref ignore and hiding = ref ignore in
  let scroll_hint = ref Hint_none in
  let report_scroll node ~(region : Grid.region) ~dx ~dy =
    (* Horizontal movement and degenerate regions cannot be expressed as a
       row shift; either disqualifies the cycle outright. *)
    if dx <> 0 || region.height <= 1 then scroll_hint := Hint_disqualified
    else
      let num = Renderable.uid node in
      match !scroll_hint with
      | Hint_none -> scroll_hint := Hint_one { num; region; delta = dy }
      | Hint_one one when one.num = num && one.region = region ->
          one.delta <- one.delta + dy
      | Hint_one _ | Hint_disqualified -> scroll_hint := Hint_disqualified
  in
  let size_round = { limit = -1; shallowest = max_int } in
  let admit_size_report node =
    let rec depth node acc =
      match Renderable.parent node with
      | Some parent -> depth parent (acc + 1)
      | None -> acc
    in
    let depth = depth node 0 in
    if depth > size_round.limit then begin
      size_round.shallowest <- min size_round.shallowest depth;
      true
    end
    else begin
      (* The paint that delivers the refused report must happen. *)
      if not (Atomic.exchange dirty true) then request_render ();
      false
    end
  in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule =
        (fun () -> if not (Atomic.exchange dirty true) then request_render ());
      off_loop = executor;
      focus =
        (fun node ->
          if
            (not (Renderable.focusable node))
            || inert_under (Renderable.Private.top_layer node) node
          then false
          else (
            (match !focused with
            | Some f when not (f == node) ->
                Renderable.Private.blur_direct f;
                focused := None
            | Some _ | None -> ());
            ignore (Renderable.Private.focus_direct node : bool);
            focused := Some node;
            true));
      blur =
        (fun node ->
          match !focused with
          | Some f when f == node ->
              Renderable.Private.blur_direct node;
              focused := None
          | _ -> ());
      selected_range = (fun node -> !selected_range node);
      text_changed = (fun node -> !text_changed node);
      hiding = (fun node -> !hiding node);
      register_lifecycle =
        (fun node -> Hashtbl.replace lifecycle_set (Renderable.uid node) node);
      unregister_lifecycle =
        (fun node -> Hashtbl.remove lifecycle_set (Renderable.uid node));
      alloc_num =
        (fun () ->
          let n = !next_num in
          incr next_num;
          n);
      register =
        (fun node ->
          Hashtbl.replace node_map (Renderable.uid node) node;
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.replace toffee_map toffee_idx node);
      unregister =
        (fun node ->
          Hashtbl.remove node_map (Renderable.uid node);
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.remove toffee_map toffee_idx);
      width_method = (fun () -> Screen.width_method screen);
      cell_size = (fun () -> Screen.cell_size screen);
      report_scroll;
      admit_size_report;
      regroups = [];
      top_layer = None;
      theme = Theme.plain;
    }
  in
  let root_style =
    match style with
    | Some s -> s
    | None ->
        Toffee.Style.set_size
          (Toffee.Style.Size_dim.pct ~w:100. ~h:100.)
          Toffee.Style.default
  in
  let root =
    Renderable.Private.create_root ctx ~style:root_style ~id:"root" ()
  in
  Renderable.Private.set_is_root root true;
  Hashtbl.replace node_map (Renderable.uid root) root;
  Hashtbl.replace toffee_map
    (Toffee.Node_id.index (Renderable.Private.toffee_node root))
    root;
  let t =
    {
      ctx;
      screen;
      adopted;
      root;
      tree;
      node_map;
      toffee_map;
      dirty;
      last_layout = None;
      layout_width_method = Screen.width_method screen;
      layout_cell_size = Screen.cell_size screen;
      frame = None;
      focused;
      walk_inert = false;
      lifecycle_set;
      commands = Array.make 64 Pop_scissor;
      cmd_len = 0;
      input_geometry = None;
      hover_node = None;
      hover_num = 0;
      pointer;
      captured = None;
      selection_state;
      scroll_hint;
      frame_callbacks = [];
      size_round;
    }
  in
  selected_range := selected_range_of t;
  text_changed := text_changed_of t;
  hiding := hiding_of t;
  t

(* ───── Accessors ───── *)

let root t = t.root
let screen t = t.screen
let focused t = !(t.focused)
let blur t = blur_current t
let focus t node = focus_node t node
let modal t = top_layer t
let theme t = t.ctx.theme

(* Roles resolve as nodes are drawn, so a new theme needs a frame and no
   other work. *)
let set_theme t theme =
  if not (t.ctx.theme == theme) then begin
    t.ctx.theme <- theme;
    Renderable.request_render t.root
  end

let set_modal t m =
  let same =
    match (top_layer t, m) with
    | None, None -> true
    | Some a, Some b -> a == b
    | Some _, None | None, Some _ -> false
  in
  if not same then begin
    (* The node that leaves the top layer, and the one that enters it, are
       laid out again where they now belong. *)
    let relayout = function
      | Some node when not (Renderable.destroyed node) ->
          Renderable.mark_dirty node
      | Some _ | None -> ()
    in
    relayout (top_layer t);
    relayout m;
    Renderable.Private.set_top_layer t.root m;
    t.last_layout <- None;
    (match !(t.focused) with
    | Some node when inert_under m node -> blur_current t
    | Some _ | None -> ());
    Renderable.request_render t.root
  end

(* Stops come in the tree's pre-order, as HTML's sequential focus navigation
   order follows the document's. One walk finds the stops on either side of
   the focused node and the first and last, so the order wraps. *)
let request_focus t direction =
  let focused =
    match !(t.focused) with
    | Some node when Renderable.focused node -> Some node
    | Some _ | None -> None
  in
  let first = ref None and last = ref None in
  let before = ref None and after = ref None and passed = ref false in
  let rec walk node =
    if Renderable.visible node && not (Renderable.inert node) then begin
      (match focused with
      | Some f when f == node -> passed := true
      | Some _ | None ->
          if Renderable.Private.has_focus_request node then begin
            if Option.is_none !first then first := Some node;
            last := Some node;
            if not !passed then before := Some node
            else if Option.is_none !after then after := Some node
          end);
      List.iter walk (Renderable.children node)
    end
  in
  walk (Option.value (top_layer t) ~default:t.root);
  let stop =
    match direction with
    | `Next -> if Option.is_some !after then !after else !first
    | `Prev -> if Option.is_some !before then !before else !last
  in
  match stop with
  | Some node -> Renderable.Private.request_focus node
  | None -> false

let captured t = t.captured
let hover t = t.hover_node

(* ───── Pending Render Work ───── *)

let pending_work t =
  let rec collect acc node =
    if Renderable.destroyed node || not (Renderable.visible node) then acc
    else
      let acc =
        match Renderable.Private.pending_work node with
        | None -> acc
        | Some work -> { Pending.node; work } :: acc
      in
      Array.fold_left collect acc (Renderable.Private.children_z node)
  in
  List.rev (collect [] t.root)

let is_settled t =
  (not (Atomic.get t.dirty))
  && match pending_work t with [] -> true | _ :: _ -> false

(* ───── Measure Function ───── *)

(* O(1) lookup via toffee_map reverse index. *)
let measure_fn (t : t) (known_dimensions : float option Toffee.Geometry.Size.t)
    (available_space : Toffee.Available_space.t Toffee.Geometry.Size.t)
    (node_id : Toffee.Node_id.t) (_context : unit option)
    (style : Toffee.Style.t) : Toffee.Geometry.Sizef.t =
  match Hashtbl.find_opt t.toffee_map (Toffee.Node_id.index node_id) with
  | None -> Toffee.Geometry.Sizef.zero
  | Some node -> (
      match Renderable.measure node with
      | None -> Toffee.Geometry.Sizef.zero
      | Some m ->
          Toffee.Geometry.Sizef.of_size
            (m ~known_dimensions ~available_space ~style))

(* ───── Pass 2: Layout Extraction + Command Generation ───── *)

(* Depth-first walk from a node, computing absolute positions and emitting
   render commands. [parent_x] and [parent_y] are the absolute position of the
   parent's content box origin. *)
let rec build_commands (t : t) (node : Renderable.t) ~parent_x ~parent_y
    ~(clip : Grid.region) ~frame ~(dt : float) : unit =
  if not (Renderable.visible node) then ()
  else
    let toffee_node = Renderable.Private.toffee_node node in
    let layout = toffee_exn (Toffee.layout t.tree toffee_node) in
    (* Absolute position = parent content box origin + node's relative location
       (which is relative to parent's content box). *)
    let abs_x = parent_x +. Renderable.Private.location_x node layout in
    let abs_y = parent_y +. Renderable.Private.location_y node layout in
    let abs_w = layout.size.width in
    let abs_h = layout.size.height in
    Renderable.Private.update_layout node ~x:abs_x ~y:abs_y ~width:abs_w
      ~height:abs_h;
    Renderable.Private.pre_render_update ~frame node ~dt;
    (* Inertness holds for the subtree of an inert node, and for the whole
       tree while a modal node is set; the modal node's subtree is walked on
       its own, in the top layer. *)
    let outer_inert = t.walk_inert in
    let inert = outer_inert || Renderable.inert node in
    t.walk_inert <- inert;
    let opacity = Renderable.opacity node in
    let has_opacity = opacity < 1.0 in
    if has_opacity then emit_cmd t (Push_opacity opacity);
    (* Emit render command for this node *)
    emit_cmd t (if inert then Render_inert node else Render node);
    (* Child clipping — only applied when overflow is not Visible, following
       CSS-style overflow semantics where "visible" means no clipping. *)
    let overflow = Toffee.Style.overflow (Renderable.style node) in
    let should_clip =
      overflow.x <> Toffee.Style.Overflow.Visible
      || overflow.y <> Toffee.Style.Overflow.Visible
    in
    let child_clip = if should_clip then Renderable.child_clip node else None in
    let render_children child_clip =
      (* Recurse into children in z-index order. Toffee's layout.location
         already positions children relative to the parent's border box (i.e.
         it includes border+padding offsets), so we pass abs_x/abs_y directly —
         no additional inset is needed. Viewport-culling nodes (scroll-box
         content) restrict the walk to children intersecting the effective
         clip, so offscreen children skip layout extraction, frame hooks,
         rendering, and hit registration entirely. *)
      let recurse child =
        match top_layer t with
        | Some m when m == child -> ()
        | Some _ | None ->
            build_commands t child ~parent_x:abs_x ~parent_y:abs_y
              ~clip:child_clip ~frame ~dt
      in
      if Renderable.viewport_cull node then
        Renderable.Private.iter_children_in_viewport ~parent:node
          ~viewport:child_clip recurse
      else Renderable.Private.iter_children_z node recurse
    in
    (match child_clip with
    | None -> render_children clip
    | Some child_scissor ->
        let child_clip = clip_rect_intersect clip child_scissor in
        if child_clip.width > 0 && child_clip.height > 0 then begin
          emit_cmd t (Push_scissor child_scissor);
          render_children child_clip;
          emit_cmd t Pop_scissor
        end);
    t.walk_inert <- outer_inert;
    if has_opacity then emit_cmd t Pop_opacity

(* ───── Pass 3: Execute Commands ───── *)

let execute_commands (t : t) ~(grid : Grid.t) ~(hits : Screen.Hit_grid.t)
    ~(dt : float) : unit =
  for i = 0 to t.cmd_len - 1 do
    match t.commands.(i) with
    | Render node ->
        Renderable.Private.render_full node ~grid ~dt;
        let bounds = Renderable.bounds node in
        Screen.Hit_grid.add hits ~x:bounds.x ~y:bounds.y ~width:bounds.width
          ~height:bounds.height ~id:(Renderable.uid node)
    | Push_scissor clip ->
        Grid.push_clip grid clip;
        Screen.Hit_grid.push_clip hits clip
    | Pop_scissor ->
        Grid.pop_clip grid;
        Screen.Hit_grid.pop_clip hits
    | Push_opacity opacity -> Grid.push_opacity grid opacity
    | Pop_opacity -> Grid.pop_opacity grid
    | Render_inert node -> Renderable.Private.render_full node ~grid ~dt
  done

(* ───── Render Frame ───── *)

let layout_dirty t =
  Renderable.Private.flush_regroups t.root;
  toffee_exn (Toffee.dirty t.tree (Renderable.Private.toffee_node t.root))

(* Text is measured by the screen's width method, and pictures by its cell
   size, which the screen can change while the tree lives. Every measurement
   then counted cells by the old ones, so every measured node is measured
   again. *)
let follow_width_method t =
  let width_method = Screen.width_method t.screen in
  let cell_size = Screen.cell_size t.screen in
  if width_method <> t.layout_width_method || cell_size <> t.layout_cell_size
  then begin
    t.layout_width_method <- width_method;
    t.layout_cell_size <- cell_size;
    Hashtbl.iter
      (fun _ node ->
        if Option.is_some (Renderable.measure node) then
          Renderable.mark_dirty node)
      t.node_map
  end

let compute_layout t ~width ~height =
  follow_width_method t;
  Renderable.Private.flush_regroups t.root;
  let root_toffee = Renderable.Private.toffee_node t.root in
  let needs_layout =
    (match t.last_layout with
      | Some (w, h) -> w <> width || h <> height
      | None -> true)
    ||
    match Toffee.dirty t.tree root_toffee with
    | Ok dirty -> dirty
    | Error _ -> true
  in
  if needs_layout then begin
    let available_space =
      Toffee.Geometry.Size.
        {
          width = Toffee.Available_space.Definite (Float.of_int width);
          height = Toffee.Available_space.Definite (Float.of_int height);
        }
    in
    toffee_exn
      (Toffee.compute_layout_with_measure t.tree root_toffee available_space
         (measure_fn t));
    t.last_layout <- Some (width, height);
    (* The modal node is laid out again against the root, as the root of
       its own layout, which replaces the one its parent gave it: it is out
       of its parent's flow, so nothing else depends on it. *)
    match top_layer t with
    | Some m when not (Renderable.destroyed m) ->
        let root = toffee_exn (Toffee.layout t.tree root_toffee) in
        toffee_exn
          (Toffee.compute_layout_with_measure t.tree
             (Renderable.Private.toffee_node m)
             Toffee.Geometry.Size.
               {
                 width = Toffee.Available_space.Definite root.size.width;
                 height = Toffee.Available_space.Definite root.size.height;
               }
             (measure_fn t))
    | Some _ | None -> ()
  end

(* A layout callback may resize a sibling (for example, reveal a scrollbar).
   Resolve those dependencies before hit testing or painting. Animations run
   only on the first pass of a frame, never while preparing input geometry. *)
let prepare_commands t ~width ~height ~frame ~dt =
  let clip : Grid.region = { x = 0; y = 0; width; height } in
  let rec settle remaining frame dt =
    compute_layout t ~width ~height;
    t.cmd_len <- 0;
    t.walk_inert <- Option.is_some (top_layer t);
    build_commands t t.root ~parent_x:0. ~parent_y:0. ~clip ~frame ~dt;
    (* The top layer comes last, so it paints over the tree and the pointer
       finds it first, under no scissor of its ancestors. *)
    (match top_layer t with
    | Some m when not (Renderable.destroyed m) ->
        t.walk_inert <- false;
        build_commands t m ~parent_x:0. ~parent_y:0. ~clip ~frame ~dt
    | Some _ | None -> ());
    if layout_dirty t then begin
      if remaining = 0 then
        invalid_arg "Mosaic.Renderer: layout callbacks did not converge";
      settle (remaining - 1) false 0.
    end
  in
  settle 16 frame dt

let prepare_layout t ~width ~height =
  t.frame <- None;
  t.size_round.shallowest <- max_int;
  prepare_commands t ~width ~height ~frame:false ~dt:0.;
  if t.size_round.shallowest < max_int then
    t.size_round.limit <- t.size_round.shallowest;
  t.input_geometry <- Some (width, height)

let layout_frame ?(animate = true) t ~width ~height ~dt =
  t.input_geometry <- None;
  (* A frame starts a new round of size reports and admits every pending one,
     including those its layout passes refused. *)
  t.size_round.limit <- -1;
  (* Consume the request that led to this pass before running any code that can
     schedule its successor. In particular, widgets may discover geometry in
     their render function and change a sibling's visibility after the command
     list has already been built. That change must survive for another pass. *)
  Atomic.set t.dirty false;
  (* Pass 0: Lifecycle passes *)
  Hashtbl.iter
    (fun _num node -> Renderable.Private.run_lifecycle_pass node)
    t.lifecycle_set;
  (* Frame callbacks *)
  if animate then List.iter (fun f -> f ~dt) t.frame_callbacks;
  prepare_commands t ~width ~height ~frame:animate ~dt;
  t.frame <- Some { width; height; dt }

(* Every cell of the selection's band that it highlights as a terminal
   does is drawn again with the [Highlight] role of its node's theme over
   its own style, blanks included. Other cells keep their style. *)
let highlight_cells t ~grid ~hits =
  match t.selection_state.current with
  | None -> ()
  | Some sel -> (
      match current_band t sel with
      | Some band when not band.empty ->
          iter_highlighted t ~scope:sel.scope band ~grid
            ~owner:(owner_in t hits) (fun ~x ~y idx node ->
              let style =
                Ansi.Style.merge ~base:(Grid.get_style grid idx)
                  ~overlay:(Theme.style (Renderable.theme node) Highlight)
              in
              let text = Grid.get_text grid idx in
              Grid.draw_text ~style grid ~x ~y
                ~text:(if text = "" then " " else text))
      | Some _ | None -> ())

let paint_frame t ~height =
  let frame =
    match t.frame with
    | Some frame -> frame
    | None -> invalid_arg "Renderer.paint_frame: no frame is laid out"
  in
  t.frame <- None;
  let width = frame.width in
  (* A paint owns the complete next frame, including blank cells:
     Screen.build guarantees cleared buffers before drawing, so when
     settlement performs another pass before presenting the first one,
     transparent text blanks do not preserve glyphs drawn by that superseded
     pass. *)
  let rec paint remaining dt =
    (* The texts read the selection's band as they paint, derived once. *)
    t.selection_state.painting <- true;
    t.selection_state.band <- None;
    Fun.protect
      ~finally:(fun () ->
        t.selection_state.painting <- false;
        t.selection_state.band <- None)
      (fun () ->
        Screen.build t.screen ~width ~height (fun grid hits ->
            Screen.Hit_grid.clear_clip hits;
            execute_commands t ~grid ~hits ~dt;
            highlight_cells t ~grid ~hits));
    if layout_dirty t then begin
      if remaining = 0 then
        invalid_arg "Mosaic.Renderer: rendering repeatedly invalidated layout";
      prepare_commands t ~width ~height:frame.height ~frame:false ~dt:0.;
      paint (remaining - 1) 0.
    end
  in
  paint 16 frame.dt;
  (* Update cursor from focused node. Visibility is derived from renderable
     intent — a cursor is visible iff a focused renderable exposes one — so
     the state is authoritative regardless of the screen's previous policy
     (a host runtime's screen starts hidden). *)
  (match !(t.focused) with
  | Some node when Renderable.focused node -> (
      match Renderable.cursor node with
      | Some c ->
          let color =
            match Ansi.Color.intent c.color with
            | Default -> None
            | Indexed _ | Rgb -> Some (Ansi.Color.to_rgb c.color)
          in
          Screen.set_cursor t.screen
            {
              position = Some (c.x, c.y);
              style = c.style;
              blinking = c.blinking;
              color;
              visible = true;
            }
      | None ->
          let cursor = Screen.cursor t.screen in
          Screen.set_cursor t.screen
            { cursor with position = None; visible = false })
  | _ ->
      let cursor = Screen.cursor t.screen in
      Screen.set_cursor t.screen
        { cursor with position = None; visible = false });
  recheck_hover t

let render_frame ?animate t ~width ~height ~dt =
  layout_frame ?animate t ~width ~height ~dt;
  paint_frame t ~height

(* A hint is only forwarded when the scrolled region owns the full grid
   width: DECSTBM shifts entire terminal rows, so a narrower container would
   drag unrelated side-by-side content and the diff would rewrite it anyway.
   A shift of the whole region height reveals every row, which the plain diff
   handles just as well. *)
let take_scroll_hint t =
  let hint = !(t.scroll_hint) in
  t.scroll_hint := Hint_none;
  match hint with
  | Hint_one { region; delta; _ }
    when delta <> 0
         && abs delta < region.height
         && region.x <= 0
         && region.x + region.width >= Grid.width (Screen.next_grid t.screen) ->
      Some
        { Screen.top = region.y; bottom = region.y + region.height - 1; delta }
  | Hint_none | Hint_one _ | Hint_disqualified -> None

let render ?full t =
  if t.adopted then
    invalid_arg
      "Renderer.render: this renderer adopted a host screen; presentation \
       belongs to the host runtime";
  let scroll_hint = take_scroll_hint t in
  Screen.render ?full ?scroll_hint t.screen

let needs_render t =
  Atomic.get t.dirty || Renderable.Private.live_count t.root > 0

let live t = Renderable.Private.live_count t.root > 0

let set_on_live_change t callback =
  Renderable.Private.set_on_live_count_change t.root callback

let render_frame_until_settled ?(max_passes = 4) t ~width ~height ~dt =
  let max_passes = max 1 max_passes in
  let rec loop remaining =
    render_frame t ~width ~height ~dt;
    if is_settled t then `Settled
    else if remaining = 1 then `Pending (pending_work t)
    else loop (remaining - 1)
  in
  loop max_passes

(* ───── Event Dispatch ───── *)

(* An event's path: its target and the target's ancestors, innermost first,
   fixed when the path is taken. A mouse path also carries what its defaults
   need: the held press a press starts, the node a release clicks, and, for the release that ends a capturing
   drag, the captured node and the node under the pointer, which receives the
   drop. *)
type path = {
  nodes : Renderable.t list;
  holds : press option;
  click : Renderable.t option;
  drag_end : Renderable.t option;
  drop : Renderable.t option;
}

let no_path =
  { nodes = []; holds = None; click = None; drag_end = None; drop = None }

let focus_path t =
  match !(t.focused) with
  | Some node when Renderable.focused node ->
      { no_path with nodes = ancestry node }
  | Some _ | None -> no_path

(* A behavior runs on the node focused when it runs, unless the default is
   prevented. One that acts prevents the default: it consumed the event,
   which then goes no further. *)
let run_behavior t ~prevented ~stop run event =
  if not (prevented event) then
    match !(t.focused) with
    | Some node when Renderable.focused node ->
        run node event;
        if prevented event then stop event
    | Some _ | None -> ()

let dispatch_focused t path ~stopped ~prevented ~stop emit behavior event =
  let target, ancestors =
    match path.nodes with [] -> ([], []) | n :: rest -> ([ n ], rest)
  in
  deliver ~stopped emit target event;
  run_behavior t ~prevented ~stop behavior event;
  deliver ~stopped emit ancestors event

let dispatch_key t path event =
  dispatch_focused t path ~stopped:Event.Key.propagation_stopped
    ~prevented:Event.Key.default_prevented ~stop:Event.Key.stop_propagation
    Renderable.Private.emit_key Renderable.Private.run_key_behavior event

let dispatch_paste t path event =
  dispatch_focused t path ~stopped:Event.Paste.propagation_stopped
    ~prevented:Event.Paste.default_prevented ~stop:Event.Paste.stop_propagation
    Renderable.Private.emit_paste Renderable.Private.run_paste_behavior event

let map_button : Input.Mouse_report.button -> Event.Mouse.button = function
  | Left -> Left
  | Right -> Right
  | Middle -> Middle
  | Extra n -> Button n

let map_scroll :
    [ `Up | `Down | `Left | `Right ] -> Event.Mouse.scroll_direction = function
  | `Up -> Scroll_up
  | `Down -> Scroll_down
  | `Left -> Scroll_left
  | `Right -> Scroll_right

(* Win32's default double-click time, which Ghostty's and Claude Code's
   share, in nanoseconds. *)
let click_window = 500_000_000

(* A press continues the series of the previous press when it is of the same
   button, at most [click_window] later, and within one cell of the series'
   first press on each axis, as Win32's double-click rectangle is around the
   first click. A drag between them does not matter. *)
let count_clicks (p : pointer_state) ~now ~x ~y button =
  let elapsed = now - p.series_time in
  let clicks =
    if
      p.series_clicks > 0
      && Event.Mouse.equal_button button p.series_button
      && elapsed >= 0 && elapsed <= click_window
      && abs (x - p.series_x) <= 1
      && abs (y - p.series_y) <= 1
    then p.series_clicks + 1
    else begin
      p.series_x <- x;
      p.series_y <- y;
      1
    end
  in
  p.series_button <- button;
  p.series_time <- now;
  p.series_clicks <- clicks;
  clicks

(* A held press whose release was lost ends where the pointer was last
   reported: the node it captured gets [Drag_end] and no [Drop], the
   selection it drags settles, and nothing is clicked. *)
let lose_press t (held : press) =
  t.pointer.held <- None;
  (match t.captured with
  | None -> ()
  | Some captured ->
      t.captured <- None;
      Atomic.set t.dirty true;
      if not (Renderable.destroyed captured) then
        let x, y =
          match t.pointer.position with
          | Some position -> position
          | None -> (held.press_x, held.press_y)
        in
        deliver_mouse (ancestry captured)
          (Event.Mouse.make ~x ~y ~modifiers:t.pointer.modifiers
             ~target:(Renderable.uid captured)
             (Event.Mouse.Drag_end { button = held.button })));
  end_selection_drag t

let pointer_lost t =
  t.pointer.refused <- None;
  Option.iter (lose_press t) t.pointer.held

let is_refused t button =
  match t.pointer.refused with
  | Some refused -> Event.Mouse.equal_button refused button
  | None -> false

let mouse_path t ~now (mouse : Input.Mouse_report.t) =
  let x = mouse.x and y = mouse.y in
  let modifiers =
    {
      Event.Mouse.no_modifier with
      shift = mouse.shift;
      alt = mouse.alt;
      ctrl = mouse.ctrl;
    }
  in
  (* A press of the held or refused button, or a motion with no button, says
     that its release was lost, as one is outside the window. *)
  (match mouse.kind with
  | Press button -> (
      let button = map_button button in
      if is_refused t button then t.pointer.refused <- None;
      match t.pointer.held with
      | Some held when Event.Mouse.equal_button button held.button ->
          lose_press t held
      | Some _ | None -> ())
  | Motion None -> pointer_lost t
  | Release _ | Motion (Some _) | Scroll _ -> ());
  t.pointer.position <- Some (x, y);
  t.pointer.modifiers <- modifiers;
  let held = t.pointer.held in
  let on ?(path = no_path) kind node =
    let target = Option.map Renderable.uid node in
    ( { path with nodes = Option.fold ~none:[] ~some:ancestry node },
      Event.Mouse.make ~x ~y ~modifiers ?target kind )
  in
  let nowhere kind =
    let event = Event.Mouse.make ~x ~y ~modifiers kind in
    Event.Mouse.prevent_default event;
    (no_path, event)
  in
  (* Hover follows the pointer, and a capture takes every event but the
     wheel; a captured node that unmounts takes them with it. *)
  let route kind target =
    let target_num = Option.fold ~none:0 ~some:Renderable.uid target in
    update_hover t ~x ~y ~modifiers ~target_node:target ~target_num kind;
    match (t.captured, kind) with
    | Some _, Event.Mouse.Scroll _ | None, _ -> on kind target
    | Some captured, _ ->
        if Renderable.destroyed captured then
          (no_path, Event.Mouse.make ~x ~y ~modifiers kind)
        else on kind (Some captured)
  in
  match mouse.kind with
  | Press button ->
      let button = map_button button in
      let kind =
        Event.Mouse.Down
          { button; clicks = count_clicks t.pointer ~now ~x ~y button }
      in
      (* A press is held until its button's release, unless another press
         is held, which it leaves alone. *)
      let hold pressed =
        match held with
        | Some _ -> None
        | None ->
            let press =
              {
                button;
                press_x = x;
                press_y = y;
                pressed;
                moved = false;
                click = None;
              }
            in
            t.pointer.held <- Some press;
            Some press
      in
      let target = hit_node t ~x ~y in
      (* Accept declarative focus before anything else sees the press. The
         request may change layout, so hit again and require the same focus
         target afterwards. *)
      let requested =
        match (button, target) with
        | Left, Some node when not modifiers.shift -> (
            match find_focusable node with
            | Some focus when inert_under (top_layer t) focus -> None
            | Some focus when not (Renderable.focused focus) ->
                if Renderable.Private.request_focus focus then Some focus
                else None
            | Some focus -> Some focus
            | None -> None)
        | _ -> None
      in
      let target =
        match requested with None -> target | Some _ -> hit_node t ~x ~y
      in
      if not (focus_request_accepted requested target) then begin
        (* A refused press takes its drag and release with it. *)
        t.pointer.refused <- Some button;
        ignore (hold None : press option);
        nowhere kind
      end
      else
        let path, event = route kind target in
        ({ path with holds = hold target }, event)
  | Release button -> (
      (* A release that does not say which button (X10, URXVT) is the held
         button's. *)
      let button =
        match (button, held) with
        | Some button, _ -> map_button button
        | None, Some held -> held.button
        | None, None -> Event.Mouse.Left
      in
      let refused = is_refused t button in
      if refused then t.pointer.refused <- None;
      let target = hit_node t ~x ~y in
      match held with
      | Some held when Event.Mouse.equal_button held.button button -> (
          t.pointer.held <- None;
          let dragged = held.moved || x <> held.press_x || y <> held.press_y in
          let kind = Event.Mouse.Up { button; dragged } in
          if refused then nowhere kind
          else
            match t.captured with
            | None -> on ~path:{ no_path with click = held.click } kind target
            | Some captured ->
                t.captured <- None;
                Atomic.set t.dirty true;
                if Renderable.destroyed captured then
                  (no_path, Event.Mouse.make ~x ~y ~modifiers kind)
                else
                  on
                    ~path:
                      { no_path with drag_end = Some captured; drop = target }
                    kind (Some captured))
      | Some _ | None ->
          let kind = Event.Mouse.Up { button; dragged = false } in
          if refused then nowhere kind else route kind target)
  | Motion (Some button) -> (
      let button = map_button button in
      let kind = Event.Mouse.Drag { button } in
      if is_refused t button then nowhere kind
      else
        match held with
        | Some held when Event.Mouse.equal_button held.button button ->
            (* Motion at the pressed cell before the pointer has left it is
               no drag: xterm reports motion only to another cell, and the
               terminals that report it there make a tremor of a click. *)
            if (not held.moved) && x = held.press_x && y = held.press_y then
              nowhere kind
            else begin
              held.moved <- true;
              held.click <- None;
              (* A left drag captures the node pressed. *)
              (match (button, t.captured) with
              | Left, None -> t.captured <- held.pressed
              | _ -> ());
              route kind (hit_node t ~x ~y)
            end
        | Some _ | None -> route kind (hit_node t ~x ~y))
  | Motion None -> route Event.Mouse.Move (hit_node t ~x ~y)
  | Scroll direction ->
      (* A scroll over dead space still means "scroll": fall back to the
         focused renderable, whose ancestors reach the nearest scrollable. *)
      let target =
        match hit_node t ~x ~y with
        | Some _ as target -> target
        | None -> (
            match !(t.focused) with
            | Some node when Renderable.focused node -> Some node
            | Some _ | None -> None)
      in
      route
        (Event.Mouse.Scroll { direction = map_scroll direction; delta = 1 })
        target

(* A click goes to the pressed node, and to its ancestors that are also
   ancestors of the node released over. *)
let dispatch_click t pressed event =
  if alive_visible pressed then
    let rec bubble node =
      if
        alive_visible node
        && (not (Event.Mouse.default_prevented event))
        && not (Event.Mouse.propagation_stopped event)
      then begin
        if ancestor_of node pressed then
          Renderable.Private.emit_click node event;
        if alive_visible node then Option.iter bubble (Renderable.parent node)
      end
    in
    Option.iter bubble
      (hit_node t ~x:(Event.Mouse.x event) ~y:(Event.Mouse.y event))

(* What a mouse event does when nothing prevents it. *)
let mouse_defaults t path event =
  let x = Event.Mouse.x event and y = Event.Mouse.y event in
  let modifiers = Event.Mouse.modifiers event in
  let kind = Event.Mouse.kind event in
  (match kind with
  | Event.Mouse.Down { button = Left; clicks } ->
      press_selection t ~x ~y ~shift:modifiers.shift ~clicks
  | Event.Mouse.Drag { button = Left } -> drag_selection t ~x ~y
  | _ -> ());
  (match (kind, path.drag_end, path.drop) with
  | Event.Mouse.Up { button; _ }, Some source, Some node ->
      let source = Some (Renderable.uid source) in
      deliver_mouse (ancestry node)
        (Event.Mouse.make ~x ~y ~modifiers ~target:(Renderable.uid node)
           (Event.Mouse.Drop { button; source }))
  | _ -> ());
  if not (Event.Mouse.default_prevented event) then
    match kind with
    | Event.Mouse.Down { button = Left; _ } -> (
        match (path.holds, Event.Mouse.target event) with
        | Some held, Some num when num = query_input_hit t ~x ~y -> (
            match find_node t num with
            | Some node when alive_visible node -> held.click <- Some node
            | _ -> ())
        | _ -> ())
    | Event.Mouse.Up { button = Left; _ } ->
        Option.iter (fun node -> dispatch_click t node event) path.click
    | _ -> ()

let dispatch_mouse t path event =
  (match (Event.Mouse.kind event, path.drag_end) with
  | Event.Mouse.Up { button; _ }, Some captured ->
      deliver_mouse (ancestry captured)
        (Event.Mouse.make ~x:(Event.Mouse.x event) ~y:(Event.Mouse.y event)
           ~modifiers:(Event.Mouse.modifiers event)
           ~target:(Renderable.uid captured)
           (Event.Mouse.Drag_end { button }))
  | _ -> ());
  deliver_mouse path.nodes event;
  (* The release ends a selection's drag, prevented or not. *)
  (match Event.Mouse.kind event with
  | Event.Mouse.Up { button = Left; _ } -> end_selection_drag t
  | _ -> ());
  if not (Event.Mouse.default_prevented event) then mouse_defaults t path event

(* ───── Frame Callbacks ───── *)

let add_frame_callback t f = t.frame_callbacks <- t.frame_callbacks @ [ f ]

let remove_frame_callback t f =
  t.frame_callbacks <- List.filter (fun e -> not (e == f)) t.frame_callbacks

let clear_frame_callbacks t = t.frame_callbacks <- []
