(* ───── Types ───── *)

module Grid = Matrix_grid

type text_rows = { layout : Matrix.Text.Layout.t; scroll_y : int }

type cursor = {
  x : int;
  y : int;
  style : [ `Block | `Line | `Underline ];
  color : Ansi.Color.t;
  blinking : bool;
}

module Pending = struct
  type t = { kind : string; label : string option }

  let make ?label ~kind () = { kind; label }
end

let equal_cursor a b =
  Int.equal a.x b.x && Int.equal a.y b.y && a.style = b.style
  && Ansi.Color.equal a.color b.color
  && Bool.equal a.blinking b.blinking

let pp_cursor ppf c =
  let style_str =
    match c.style with
    | `Block -> "Block"
    | `Line -> "Line"
    | `Underline -> "Underline"
  in
  Format.fprintf ppf "Cursor(%d, %d, %s, %a, blink=%b)" c.x c.y style_str
    Ansi.Color.pp c.color c.blinking

type selection_handler = {
  should_start : x:int -> y:int -> bool;
  on_change : Selection.t option -> bool;
  clear : unit -> unit;
  get_text : unit -> string;
}

type measure =
  known_dimensions:float option Toffee.Geometry.Size.t ->
  available_space:Toffee.Available_space.t Toffee.Geometry.Size.t ->
  style:Toffee.Style.t ->
  float Toffee.Geometry.Size.t

type context = {
  tree : unit Toffee.tree;
  schedule : unit -> unit;
  off_loop : (unit -> unit) -> unit;
  focus : node -> bool;
  blur : node -> unit;
  get_selection : unit -> Selection.t option;
  request_selection_update : unit -> unit;
  register_lifecycle : node -> unit;
  unregister_lifecycle : node -> unit;
  alloc_num : unit -> int;
  register : node -> unit;
  unregister : node -> unit;
  width_method : unit -> Matrix.Text.width_method;
  cell_size : unit -> (int * int) option;
  report_scroll : node -> region:Grid.region -> dx:int -> dy:int -> unit;
  admit_size_report : node -> bool;
  mutable regroups : node list;
  mutable top_layer : node option;
  mutable theme : Theme.t;
}

(* The region a subtree may paint, in cells relative to its node's layout
   origin, and the layout value it was computed from. *)
and paint_region = {
  layout : Toffee.Layout.t;
  x0 : int;
  y0 : int;
  x1 : int;
  y1 : int;
}

(* A visible child of a culling node, with its paint region relative to the
   parent's origin: [p0, p1) on the primary axis, [c0, c1) on the cross axis.
   [rank] is its index among the parent's children, which orders equal
   z-indices. In a chunked column's index an entry may stand for a chunk
   ([stands_for]), with the region its members' join and [child] its first member
   (see "Chunks"). *)
and cull_entry = {
  child : node;
  stands_for : chunk option;
  rank : int;
  p0 : int;
  p1 : int;
  c0 : int;
  c1 : int;
}

(* A culling node's visible children sorted by [p0], with [reach.(i)] the
   largest [p1] among the first [i + 1] entries, built from the parent's layout
   value [built_from]. *)
and cull_index = {
  built_from : Toffee.Layout.t;
  entries : cull_entry array;
  reach : int array;
  column : bool;
}

(* A run of a culling column's children that the layout tree holds under a
   node of its own, [chunk_node] (see "Chunks"). [index] is the cull index of
   the visible [members], built from the first member's layout value with
   [base] the first member's rank; [claimed] is the regroup that last kept the
   chunk. *)
and chunk = {
  chunk_node : Toffee.Node_id.t;
  mutable members : node array;
  mutable claimed : int;
  mutable base : int;
  mutable index : cull_index option;
}

(* A child of a culling node in the layout tree. *)
and layout_child = Direct of node | Chunk of chunk

(* A node's absolute box, in floats only so that updating it allocates
   nothing. A node shares [unplaced] until its first layout, as many are
   never shown. *)
and abs = {
  mutable abs_x : float;
  mutable abs_y : float;
  mutable abs_w : float;
  mutable abs_h : float;
}

and node = {
  ctx : context;
  toffee_node : Toffee.Node_id.t;
  mutable id : string;
  num : int;
  mutable destroyed : bool;
  mutable cleanup : (unit -> unit) option;
  (* Layout *)
  mutable style : Toffee.Style.t;
  mutable original_display : Toffee.Style.display;
  mutable layout_dirty : bool;
  mutable abs : abs;
  mutable layout_valid : bool;
  mutable measure : measure option;
  mutable translate_x : int;
  mutable translate_y : int;
  (* Hierarchy *)
  mutable parent : node option;
  mutable child_target : node option;
  mutable children : node option array;
  mutable child_count : int;
  mutable z_sorted : node array;
  mutable z_len : int;
  mutable z_dirty : bool;
  (* Viewport culling: when [true], the renderer restricts this node's child
     traversal to children whose paint region intersects the effective clip.
     [cull_dirty] records a change the layout tree does not show: children
     added, removed or translated, or this node's style. *)
  mutable viewport_cull : bool;
  mutable cull_index : cull_index option;
  mutable cull_dirty : bool;
  mutable paint_region : paint_region option;
  (* Chunks (see "Chunks"): [chunk] holds this node in the layout tree, if
     any. A culling node's [layout_children] are its children in the layout
     tree, and [regroup_from] is the index of its first child changed since
     they were set, [max_int] for none. *)
  mutable chunk : chunk option;
  mutable layout_children : layout_child array;
  mutable regroup_from : int;
  (* Rendering *)
  mutable render : render;
  mutable render_before : render option;
  mutable render_after : render option;
  mutable frame_buffer : Grid.t option;
  (* Visual *)
  mutable visible : bool;
  mutable z_index : int;
  mutable opacity : float;
  mutable buffered : bool;
  mutable live : bool;
  mutable self_live : int;
  mutable live_count : int;
  (* Focus *)
  mutable focusable : bool;
  mutable focused : bool;
  mutable inert : bool;
  mutable cursor_provider : (node -> cursor option) option;
  mutable focus_request : (unit -> unit) option;
  mutable focus_handler : (node -> unit) option;
  mutable blur_handler : (node -> unit) option;
  (* Events *)
  mutable mouse_handlers : (Event.mouse -> unit) list;
  mutable click_handler : (Event.mouse -> unit) option;
  mutable key_handlers : (Event.key -> unit) list;
  mutable paste_handlers : (Event.paste -> unit) list;
  mutable key_behavior : (Event.key -> unit) option;
  mutable paste_behavior : (Event.paste -> unit) option;
  (* Selection *)
  mutable selection : selection_handler option;
  (* Child clipping *)
  mutable child_clip : (node -> Grid.region option) option;
  (* Text rows *)
  mutable text_rows_provider : (unit -> text_rows) option;
  (* Pending render work *)
  mutable pending_provider : (unit -> Pending.t option) option;
  (* Lifecycle *)
  mutable on_update : (node -> dt:float -> unit) option;
  mutable on_frame : (node -> dt:float -> unit) option;
  mutable on_resize : (node -> unit) option;
  mutable size_observer : (node -> unit) option;
  mutable on_lifecycle_pass : (node -> unit) option;
  mutable last_width : int;
  mutable last_height : int;
  mutable reported_width : int;
  mutable reported_height : int;
  mutable lifecycle_registered : bool;
  mutable is_root : bool;
  mutable live_count_change : (node -> unit) option;
  (* Theme *)
  mutable themes : themes option;
}

(* A node's theme functions: [place], the scope its place in a view
   declares, which a reconciler sets, and [own], the node's own
   ([set_theme]), applied after it; one of them at least is set. [applied]
   is what they gave last, for the theme [around] the node. [fresh] is
   [false] until they are applied, and again once either changes. *)
and themes = {
  mutable place : (Theme.t -> Theme.t) option;
  mutable own : (Theme.t -> Theme.t) option;
  mutable around : Theme.t;
  mutable applied : Theme.t;
  mutable fresh : bool;
}

and render = node -> Grid.t -> dt:float -> unit

type t = node

(* ───── Helpers ───── *)

let toffee_exn = function
  | Ok x -> x
  | Error e -> invalid_arg (Toffee.Error.to_string e)

let check_alive t =
  if t.destroyed then invalid_arg "Renderable: node is destroyed"

let render_noop _ _ ~dt:_ = ()

(* Auto flex_shrink: terminal layout convention.

   In CSS flexbox, flex_shrink defaults to 1.0 so items shrink to fit. In
   terminal UIs, nodes with explicit pixel dimensions (width/height in cells)
   should not shrink — a 40-column text box must stay 40 columns. We
   automatically set flex_shrink to 0.0 when explicit dimensions are present and
   the caller has not overridden flex_shrink from its default. This avoids
   surprising layout compression that would be correct on the web but wrong in a
   terminal. *)
let prepare_style style =
  let sz = Toffee.Style.size style in
  let has_explicit =
    Toffee.Style.Dimension.is_length sz.width
    || Toffee.Style.Dimension.is_length sz.height
  in
  if has_explicit && Float.equal (Toffee.Style.flex_shrink style) 1.0 then
    Toffee.Style.set_flex_shrink 0.0 style
  else style

(* ───── Live Count ───── *)

(* Live count: bottom-up aggregation for render scheduling.

   Each node contributes 1 to the live count when both [live] and [visible] are
   true. The count propagates to every ancestor, so the root's [live_count]
   reflects how many descendants need continuous rendering. The renderer checks
   this single value to decide whether to keep the render loop running —
   avoiding a full tree walk each frame. Attach/detach adjust ancestors' counts
   in O(depth). *)

let rec adjust_live_count node delta =
  if delta <> 0 then (
    node.live_count <- node.live_count + delta;
    Option.iter (fun f -> f node) node.live_count_change;
    Option.iter (fun p -> adjust_live_count p delta) node.parent)

let compute_self_live node = if node.live && node.visible then 1 else 0

let update_self_live node =
  let new_self = compute_self_live node in
  let delta = new_self - node.self_live in
  if delta <> 0 then (
    node.self_live <- new_self;
    adjust_live_count node delta;
    node.ctx.schedule ())

(* ───── Lifecycle Registration ───── *)

let is_attached node = node.is_root || Option.is_some node.parent

let register_lifecycle_if_needed node =
  if not node.lifecycle_registered then (
    node.lifecycle_registered <- true;
    node.ctx.register_lifecycle node)

let unregister_lifecycle_if_needed node =
  if node.lifecycle_registered then (
    node.lifecycle_registered <- false;
    node.ctx.unregister_lifecycle node)

let update_lifecycle_registration node =
  match node.on_lifecycle_pass with
  | Some _ when is_attached node -> register_lifecycle_if_needed node
  | _ -> unregister_lifecycle_if_needed node

(* ───── Children Array ───── *)

(* Children are stored in a flat [node option array] with geometric growth
   (doubling from an initial capacity of 4). This gives O(1) amortized insert
   and O(n) shift for mid-array insertion — a good trade-off for UI trees where
   child counts are typically small and cache locality matters for iteration
   during rendering. *)

let ensure_capacity node needed =
  let len = Array.length node.children in
  if needed > len then (
    let cap =
      let rec grow c = if c >= needed then c else grow (c * 2) in
      if len = 0 then grow 4 else grow (len * 2)
    in
    let arr = Array.make cap None in
    Array.blit node.children 0 arr 0 len;
    node.children <- arr)

let insert_child_at parent idx child =
  let idx =
    if idx < 0 then 0
    else if idx > parent.child_count then parent.child_count
    else idx
  in
  ensure_capacity parent (parent.child_count + 1);
  for i = parent.child_count downto idx + 1 do
    parent.children.(i) <- parent.children.(i - 1)
  done;
  parent.children.(idx) <- Some child;
  parent.child_count <- parent.child_count + 1;
  parent.z_dirty <- true;
  parent.cull_dirty <- true;
  adjust_live_count parent child.live_count;
  parent.ctx.schedule ()

let find_child_index parent child =
  let count = parent.child_count in
  let rec loop i =
    if i >= count then None
    else
      match parent.children.(i) with
      | Some c when c == child -> Some i
      | _ -> loop (i + 1)
  in
  loop 0

let remove_child_ref parent child =
  match find_child_index parent child with
  | None -> ()
  | Some idx ->
      for i = idx to parent.child_count - 2 do
        parent.children.(i) <- parent.children.(i + 1)
      done;
      if parent.child_count > 0 then
        parent.children.(parent.child_count - 1) <- None;
      parent.child_count <- max 0 (parent.child_count - 1);
      parent.z_dirty <- true;
      parent.cull_dirty <- true;
      adjust_live_count parent (-child.live_count);
      parent.ctx.schedule ()

(* ───── Node Creation ───── *)

(* Never written: [update_layout] gives a node a box of its own first. *)
let unplaced = { abs_x = 0.; abs_y = 0.; abs_w = 0.; abs_h = 0. }

let make_node ctx ~toffee_node ~id ~num ?(style = Toffee.Style.default)
    ?(visible = true) ?(z_index = 0) ?(opacity = 1.0) ?(live = false)
    ?(render = render_noop) () =
  let self_live = if live && visible then 1 else 0 in
  {
    ctx;
    toffee_node;
    id;
    num;
    destroyed = false;
    cleanup = None;
    style;
    original_display = Toffee.Style.display style;
    layout_dirty = true;
    abs = unplaced;
    layout_valid = false;
    measure = None;
    translate_x = 0;
    translate_y = 0;
    parent = None;
    child_target = None;
    children = [||];
    child_count = 0;
    z_sorted = [||];
    z_len = 0;
    z_dirty = false;
    viewport_cull = false;
    cull_index = None;
    cull_dirty = false;
    paint_region = None;
    chunk = None;
    layout_children = [||];
    regroup_from = max_int;
    render;
    render_before = None;
    render_after = None;
    frame_buffer = None;
    visible;
    z_index;
    opacity;
    buffered = false;
    live;
    self_live;
    live_count = self_live;
    focusable = false;
    focused = false;
    inert = false;
    cursor_provider = None;
    focus_request = None;
    focus_handler = None;
    blur_handler = None;
    mouse_handlers = [];
    click_handler = None;
    key_handlers = [];
    paste_handlers = [];
    key_behavior = None;
    paste_behavior = None;
    selection = None;
    child_clip = None;
    text_rows_provider = None;
    pending_provider = None;
    on_update = None;
    on_frame = None;
    on_resize = None;
    size_observer = None;
    on_lifecycle_pass = None;
    last_width = 0;
    last_height = 0;
    reported_width = 0;
    reported_height = 0;
    lifecycle_registered = false;
    is_root = false;
    live_count_change = None;
    themes = None;
  }

let apply_initial_visibility t =
  if not t.visible then (
    let hidden = Toffee.Style.set_display Toffee.Style.Display.None t.style in
    toffee_exn (Toffee.set_style t.ctx.tree t.toffee_node hidden);
    t.layout_valid <- false;
    update_self_live t)

(* ───── Identity ───── *)

let id t = t.id
let set_id t id = t.id <- id
let parent t = t.parent

let children t =
  let rec loop acc idx =
    if idx < 0 then acc
    else
      match t.children.(idx) with
      | Some child -> loop (child :: acc) (idx - 1)
      | None -> loop acc (idx - 1)
  in
  loop [] (t.child_count - 1)

(* Child target: transparent child routing for composite widgets.

   Composite widgets like ScrollBox have internal node hierarchies (root →
   wrapper → viewport → content). User children must be routed to the content
   node, not the root. We model this explicitly with [child_target]: [create]
   and [attach] follow the redirect pointer on the parent before attaching.

   Construction order is safe: widgets build internal nodes first (when
   child_target defaults to self), then call [set_child_target root (Some
   content)] to redirect future children. *)

let child_target t = Option.value t.child_target ~default:t

let set_child_target t target =
  match target with
  | None -> t.child_target <- None
  | Some target ->
      check_alive t;
      check_alive target;
      if not (target.ctx.tree == t.ctx.tree) then
        invalid_arg
          "Renderable.set_child_target: target node belongs to a different tree";
      let rec is_descendant node =
        if node == t then true
        else match node.parent with Some p -> is_descendant p | None -> false
      in
      if not (is_descendant target) then
        invalid_arg
          "Renderable.set_child_target: target must be the node or one of its \
           descendants";
      t.child_target <- Some target

(* ───── Focus ───── *)

let focusable t = t.focusable

let set_focusable t v =
  if t.focusable <> v then (
    t.focusable <- v;
    if (not v) && t.focused then t.ctx.blur t)

let focused t = t.focused

let focus_direct t =
  if not t.focusable then false
  else if t.focused then true
  else (
    t.focused <- true;
    Option.iter (fun handler -> handler t) t.focus_handler;
    t.ctx.schedule ();
    true)

let blur_direct t =
  if t.focused then (
    t.focused <- false;
    Option.iter (fun handler -> handler t) t.blur_handler;
    t.ctx.schedule ())

let focus t = if not t.focusable then false else t.ctx.focus t
let blur t = if t.focused then t.ctx.blur t
let inert t = t.inert

let set_cursor_provider t provider =
  t.cursor_provider <- Some provider;
  t.ctx.schedule ()

let clear_cursor_provider t =
  if Option.is_some t.cursor_provider then (
    t.cursor_provider <- None;
    t.ctx.schedule ())

let cursor t = Option.bind t.cursor_provider (fun f -> f t)
let set_on_focus t handler = t.focus_handler <- handler
let set_on_blur t handler = t.blur_handler <- handler

(* ───── Rendering ───── *)

let request_render t = t.ctx.schedule ()

(* ───── Theme ───── *)

(* The theme in scope is resolved from the root down at each read: no node
   stores the theme of its scope, so a node attached elsewhere, or under a
   theme that changed, reads its new scope at once. A node with a function
   of its own applies it again only when the theme around it is another
   value, so reading an unchanged scope allocates nothing. *)
let apply f theme = match f with Some f -> f theme | None -> theme

let rec theme t =
  let around = match t.parent with Some p -> theme p | None -> t.ctx.theme in
  match t.themes with
  | None -> around
  | Some th ->
      if th.fresh && th.around == around then th.applied
      else
        let applied = apply th.own (apply th.place around) in
        th.around <- around;
        th.applied <- applied;
        th.fresh <- true;
        applied

let same f g =
  match (f, g) with
  | None, None -> true
  | Some f, Some g -> f == g
  | Some _, None | None, Some _ -> false

(* Sets the node's place function ([~place:true]) or its own. *)
let set_themes t ~place f =
  match t.themes with
  | None -> (
      match f with
      | None -> ()
      | Some _ ->
          t.themes <-
            Some
              {
                place = (if place then f else None);
                own = (if place then None else f);
                around = Theme.plain;
                applied = Theme.plain;
                fresh = false;
              };
          request_render t)
  | Some th ->
      if not (same (if place then th.place else th.own) f) then begin
        if place then th.place <- f else th.own <- f;
        th.fresh <- false;
        if Option.is_none th.place && Option.is_none th.own then
          t.themes <- None;
        request_render t
      end

let set_theme t f = set_themes t ~place:false f
let off_loop t work = t.ctx.off_loop work

let set_render t fn =
  t.render <- fn;
  request_render t

let set_render_before t hook =
  t.render_before <- hook;
  request_render t

let set_render_after t hook =
  t.render_after <- hook;
  request_render t

let set_child_clip t fn =
  t.child_clip <- fn;
  request_render t

(* ───── Chunks ─────

   A culling column holds its children in the layout tree in chunks: each run
   of [contained] children sits under a column node of its own, at most
   [chunk_size] to a chunk, and every other child sits directly under the
   column. The renderable tree is unchanged. A clean chunk is measured from
   its layout cache and laid out from its members', so laying the column out
   again lays out the members of the chunks that changed and of no other
   (Flutter's layout cut-off over CodeMirror's tree of line chunks). A chunk
   resolves no percentage against the column's height, which the layout tree
   then leaves out of the key of its cached layouts, so when the column's
   height changes a clean chunk returns its cached final layout.

   Every child keeps the box it has in a flat column. The column has one line,
   no gap, no space distribution and an automatic height with no maximum, so
   its free space is never negative ([chunkable]). A contained child is in
   flow, takes no free space and resolves no percentage against the column,
   so its size depends only on itself and the width it is given, which a
   chunk stretched to the column's width passes on, and its position is the
   sum of the outer heights before it. Children whose own style breaks this
   are laid out flat. The layout tree rounds a location as the difference of
   the rounded positions of the node and its parent in the root's
   coordinates, so the chunk's location plus the child's is the location the
   flat column gives the child ([location_x], [location_y]).

   [regroup] alone sets the layout tree's children of a culling node. Every
   change to its child list, and every change of a child's style that changes
   whether it is contained, notes the first index changed ([note_regroup]),
   and the renderer runs the pending regroups before each layout
   ([flush_regroups]). *)

let chunk_size = 256
let regroup_passes = Atomic.make 0

let chunkable p =
  let open Toffee.Style in
  let s = p.style in
  let gap = (gap s).height in
  (match display s with
    | Display.Flex | Display.None -> true
    | Block | Grid -> false)
  && flex_direction s = Flex_direction.Column
  && flex_wrap s = Flex_wrap.No_wrap
  && Length_percentage.is_length gap
  && Length_percentage.value gap = 0.
  && (match justify_content s with
    | Some (Space_between | Space_around | Space_evenly | Stretch) -> false
    | Some _ | None -> true)
  && Dimension.is_auto (size s).height
  && Dimension.is_auto (max_size s).height

(* A child is contained when its box depends on nothing in the column but the
   column's width: it stays in flow, takes no free space and resolves no
   percentage of its size or its vertical offset against the column. *)
let contained c =
  let open Toffee.Style in
  let s = c.style in
  let no_percent (d : Dimension.t Toffee.Geometry.Size.t) =
    not (Dimension.uses_percentage d.width || Dimension.uses_percentage d.height)
  in
  let margin = margin s and inset = inset s in
  position s = Position.Relative
  && flex_grow s = 0.
  && (not (Length_percentage_auto.is_auto margin.top))
  && (not (Length_percentage_auto.is_auto margin.bottom))
  && no_percent (size s)
  && no_percent (min_size s)
  && no_percent (max_size s)
  && (not (Dimension.uses_percentage (flex_basis s)))
  && (not (Length_percentage_auto.uses_percentage inset.top))
  && not (Length_percentage_auto.uses_percentage inset.bottom)

let layout_child_length = function
  | Direct _ -> 1
  | Chunk k -> Array.length k.members

let note_regroup p i =
  if p.regroup_from = max_int then p.ctx.regroups <- p :: p.ctx.regroups;
  p.regroup_from <- min p.regroup_from i

let chunk_style p =
  let open Toffee.Style in
  default
  |> set_flex_direction Flex_direction.Column
  |> set_align_self (Some Align_items.Stretch)
  |> set_align_items (align_items p.style)
  |> set_size { width = Dimension.percent 1.; height = Dimension.auto }

let same_layout_children a b =
  Array.length a = Array.length b
  && Array.for_all2
       (fun a b ->
         match (a, b) with
         | Chunk a, Chunk b -> a == b
         | Direct a, Direct b -> a == b
         | Chunk _, Direct _ | Direct _, Chunk _ -> false)
       a b

(* [regroup p] sets [p]'s children in the layout tree from its children.
   The layout children that end before the first changed child are kept; from
   the one before it on, runs of contained children form chunks. A run breaks
   where a chunk that is kept began, and a chunk keeps its layout node, and so
   its cache, while its members are the same. A run of new children joins the
   chunk that follows it when they fit. *)
let regroup p =
  let tree = p.ctx.tree in
  let chunked = chunkable p in
  let from = if chunked then p.regroup_from else 0 in
  p.regroup_from <- max_int;
  let pass = Atomic.fetch_and_add regroup_passes 1 + 1 in
  let old = p.layout_children in
  let kept = ref 0 and start = ref 0 in
  while
    !kept < Array.length old && !start + layout_child_length old.(!kept) < from
  do
    (match old.(!kept) with Chunk k -> k.claimed <- pass | Direct _ -> ());
    start := !start + layout_child_length old.(!kept);
    incr kept
  done;
  let rev_tail = ref [] and run = ref [] and length = ref 0 in
  let owner = ref None in
  let flush () =
    if !length > 0 then (
      let members = Array.of_list (List.rev !run) in
      let k =
        match !owner with
        | Some k -> k
        | None ->
            {
              chunk_node = toffee_exn (Toffee.new_leaf tree (chunk_style p));
              members = [||];
              claimed = 0;
              base = 0;
              index = None;
            }
      in
      k.claimed <- pass;
      if
        not
          (Array.length members = Array.length k.members
          && Array.for_all2 ( == ) members k.members)
      then (
        k.members <- members;
        k.index <- None;
        toffee_exn
          (Toffee.set_children tree k.chunk_node
             (Array.map (fun c -> c.toffee_node) members)));
      Array.iter (fun c -> c.chunk <- Some k) members;
      rev_tail := Chunk k :: !rev_tail;
      run := [];
      length := 0;
      owner := None)
  in
  for i = !start to p.child_count - 1 do
    match p.children.(i) with
    | Some c when chunked && contained c ->
        let own =
          match c.chunk with
          | Some k when k.claimed <> pass -> Some k
          | Some _ | None -> None
        in
        (match (own, !owner) with
        | Some k, Some o when k != o -> flush ()
        | Some k, None when !length + Array.length k.members > chunk_size ->
            flush ()
        | _ -> ());
        if !length >= chunk_size then flush ();
        (* A flush may have claimed [own]. *)
        (if Option.is_none !owner then
           match own with
           | Some k when k.claimed <> pass -> owner := own
           | Some _ | None -> ());
        run := c :: !run;
        incr length
    | Some c ->
        flush ();
        c.chunk <- None;
        rev_tail := Direct c :: !rev_tail
    | None -> ()
  done;
  flush ();
  for i = !kept to Array.length old - 1 do
    match old.(i) with
    | Chunk k when k.claimed <> pass -> ignore (Toffee.remove tree k.chunk_node)
    | Chunk _ | Direct _ -> ()
  done;
  let tail = Array.of_list (List.rev !rev_tail) in
  let old_tail = Array.sub old !kept (Array.length old - !kept) in
  if not (same_layout_children tail old_tail) then (
    p.layout_children <- Array.append (Array.sub old 0 !kept) tail;
    p.cull_dirty <- true;
    toffee_exn
      (Toffee.set_children tree p.toffee_node
         (Array.map
            (function Chunk k -> k.chunk_node | Direct c -> c.toffee_node)
            p.layout_children)))

let flush_regroups t =
  let nodes = t.ctx.regroups in
  t.ctx.regroups <- [];
  List.iter
    (fun p ->
      if p.regroup_from < max_int && p.viewport_cull && not p.destroyed then
        regroup p)
    nodes

(* The rounded layout of a chunk's node, read without a result to allocate. *)
let chunk_layout c (k : chunk) =
  Toffee.Print_tree.get_final_layout c.ctx.tree k.chunk_node

(* [location_x c layout] is [c]'s horizontal position relative to its
   parent's layout origin, where [layout] is [c]'s layout. *)
let[@inline] location_x c (layout : Toffee.Layout.t) =
  match c.chunk with
  | None -> layout.location.x
  | Some k -> (chunk_layout c k).location.x +. layout.location.x

let[@inline] location_y c (layout : Toffee.Layout.t) =
  match c.chunk with
  | None -> layout.location.y
  | Some k -> (chunk_layout c k).location.y +. layout.location.y

(* Turning culling on chunks the children; turning it off lays them out flat
   again. *)
let set_viewport_cull t v =
  if v <> t.viewport_cull then (
    t.viewport_cull <- v;
    if v then (
      (* The chunks take the children from the layout tree's node of [t]
         after it lets them go, each found there at once. *)
      ignore (Toffee.set_children t.ctx.tree t.toffee_node [||]);
      note_regroup t 0)
    else (
      ignore
        (Toffee.set_children t.ctx.tree t.toffee_node
           (Array.init t.child_count (fun i ->
                match t.children.(i) with
                | Some c ->
                    c.chunk <- None;
                    c.toffee_node
                | None -> assert false)));
      Array.iter
        (function
          | Chunk k -> ignore (Toffee.remove t.ctx.tree k.chunk_node)
          | Direct _ -> ())
        t.layout_children;
      t.layout_children <- [||];
      t.regroup_from <- max_int;
      t.cull_index <- None))

(* ───── Layout ───── *)

let mark_dirty t =
  t.layout_dirty <- true;
  toffee_exn (Toffee.mark_dirty t.ctx.tree t.toffee_node)

let apply_style t ~preserve_hidden new_style =
  let was_contained = contained t in
  let new_style = prepare_style new_style in
  let display = Toffee.Style.display new_style in
  if not (Toffee.Style.Display.is_none display) then
    t.original_display <- display;
  let effective_style =
    if
      preserve_hidden && (not t.visible)
      && not (Toffee.Style.Display.is_none display)
    then Toffee.Style.set_display Toffee.Style.Display.None new_style
    else new_style
  in
  toffee_exn (Toffee.set_style t.ctx.tree t.toffee_node effective_style);
  t.style <- effective_style;
  let now_visible =
    not (Toffee.Style.Display.is_none (Toffee.Style.display effective_style))
  in
  if t.visible <> now_visible then (
    t.visible <- now_visible;
    update_self_live t;
    if (not now_visible) && t.focused then blur t);
  t.layout_dirty <- true;
  t.cull_dirty <- true;
  (match t.parent with
  | Some p when p.viewport_cull && was_contained <> contained t ->
      note_regroup p 0
  | Some _ | None -> ());
  if t.viewport_cull then note_regroup t 0;
  t.ctx.schedule ()

let set_style t new_style = apply_style t ~preserve_hidden:true new_style

let style t =
  match Toffee.style t.ctx.tree t.toffee_node with
  | Ok s -> s
  | Error _ -> t.style

let set_measure t m =
  t.measure <- m;
  t.layout_dirty <- true;
  toffee_exn (Toffee.mark_dirty t.ctx.tree t.toffee_node)

(* The node in the top layer is placed against the root: its ancestors'
   translations do not move it. *)
let in_top_layer t =
  match t.ctx.top_layer with Some top -> top == t | None -> false

let translate_acc t =
  let rec loop node ax ay =
    let ax = ax + node.translate_x and ay = ay + node.translate_y in
    match node.parent with
    | Some p when not (in_top_layer node) -> loop p ax ay
    | Some _ | None -> (ax, ay)
  in
  loop t 0 0

let[@inline] terminal_edge value = int_of_float (Float.round value)

(* Quantize both ends of a Toffee interval. Rounding its length independently
   can overlap an adjacent interval whose shared edge rounds down. *)
let[@inline] terminal_span start length =
  max 0 (terminal_edge (start +. length) - terminal_edge start)

let x t =
  let ox, _ = translate_acc t in
  if t.layout_valid then terminal_edge t.abs.abs_x + ox else ox

let y t =
  let _, oy = translate_acc t in
  if t.layout_valid then terminal_edge t.abs.abs_y + oy else oy

let width t =
  if t.layout_valid && t.visible then terminal_span t.abs.abs_x t.abs.abs_w
  else 0

let height t =
  if t.layout_valid && t.visible then terminal_span t.abs.abs_y t.abs.abs_h
  else 0

let bounds t : Grid.region =
  { x = x t; y = y t; width = width t; height = height t }

(* A node clips its children to its box when either axis's overflow is not
   visible. *)
let clips t =
  let overflow = Toffee.Style.overflow t.style in
  overflow.x <> Toffee.Style.Overflow.Visible
  || overflow.y <> Toffee.Style.Overflow.Visible

(* A translation moves where [t] paints without changing the layout tree, so
   forget the paint regions and cull indexes that include it, up to the first
   ancestor that clips. *)
let rec forget_paint_region t =
  match t.parent with
  | None -> ()
  | Some parent ->
      parent.cull_dirty <- true;
      (match t.chunk with Some k -> k.index <- None | None -> ());
      if not (clips parent) then (
        parent.paint_region <- None;
        forget_paint_region parent)

let set_translate t ~x:ox ~y:oy =
  if t.translate_x <> ox || t.translate_y <> oy then (
    t.translate_x <- ox;
    t.translate_y <- oy;
    forget_paint_region t;
    t.ctx.schedule ())

let translate t = (t.translate_x, t.translate_y)

(* ───── Tree Operations ───── *)

let rec blur_focused_subtree t =
  if t.focused then blur t;
  for i = 0 to t.child_count - 1 do
    match t.children.(i) with
    | Some child -> blur_focused_subtree child
    | None -> ()
  done

(* What the pointer hits changes with inertness, so the frame's hit regions
   are built again. *)
let set_inert t v =
  if t.inert <> v then (
    t.inert <- v;
    if v then blur_focused_subtree t;
    t.ctx.schedule ())

let detach_impl child =
  match child.parent with
  | None -> ()
  | Some parent ->
      if parent.viewport_cull then
        note_regroup parent
          (Option.value ~default:0 (find_child_index parent child));
      remove_child_ref parent child;
      child.parent <- None;
      let holder =
        match child.chunk with
        | Some k -> k.chunk_node
        | None -> parent.toffee_node
      in
      child.chunk <- None;
      ignore (Toffee.remove_child child.ctx.tree holder child.toffee_node);
      update_lifecycle_registration child;
      child.layout_valid <- false;
      parent.layout_dirty <- true

let attach_impl ~parent ~index child =
  let idx =
    if index < 0 then 0
    else if index > parent.child_count then parent.child_count
    else index
  in
  if parent.viewport_cull then note_regroup parent idx
  else if idx >= parent.child_count then
    toffee_exn
      (Toffee.add_child parent.ctx.tree parent.toffee_node child.toffee_node)
  else
    toffee_exn
      (Toffee.insert_child_at_index parent.ctx.tree parent.toffee_node idx
         child.toffee_node);
  child.parent <- Some parent;
  child.is_root <- false;
  insert_child_at parent idx child;
  update_lifecycle_registration child;
  child.layout_valid <- false;
  parent.layout_dirty <- true;
  child.layout_dirty <- true

let create ~parent ?index ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(live = false) ?(render = render_noop) () =
  let parent = child_target parent in
  check_alive parent;
  let ctx = parent.ctx in
  let num = ctx.alloc_num () in
  let prepared = prepare_style style in
  let toffee_node = toffee_exn (Toffee.new_leaf ctx.tree prepared) in
  let id = Option.value id ~default:(Printf.sprintf "node-%d" num) in
  let t =
    make_node ctx ~toffee_node ~id ~num ~style:prepared ~visible ~z_index
      ~opacity ~live ~render ()
  in
  ctx.register t;
  apply_initial_visibility t;
  attach_impl ~parent ~index:(Option.value index ~default:parent.child_count) t;
  t

(* Attaches [t] to [parent] itself, whatever its child target. *)
let attach_to parent ~index t =
  check_alive parent;
  check_alive t;
  if parent == t then
    invalid_arg "Renderable.attach: cannot attach node to itself";
  if not (parent.ctx.tree == t.ctx.tree) then
    invalid_arg "Renderable.attach: nodes belong to different trees";
  if Option.is_some t.parent then detach_impl t;
  attach_impl ~parent ~index t

let attach ~parent ?index t =
  let parent = child_target parent in
  attach_to parent ~index:(Option.value index ~default:parent.child_count) t

let detach t =
  blur_focused_subtree t;
  detach_impl t

(* Detaches [nodes] as [detach] does, removing each parent's departing
   children in one pass over its children: detaching them one by one would
   shift the children after each, which is quadratic in their number. A
   departing child is one whose parent is unset. A kept child keeps its
   option as it moves down, and the layout tree compacts its children in the
   same order ([Toffee.retain_children]), before the array moves, so the pass
   allocates nothing per kept child. A culling node regroups instead. *)
let detach_all = function
  | [] -> ()
  | nodes ->
      List.iter blur_focused_subtree nodes;
      let parents = Hashtbl.create 8 in
      List.iter
        (fun child ->
          match child.parent with
          | None -> ()
          | Some parent ->
              child.parent <- None;
              Hashtbl.replace parents parent.num parent)
        nodes;
      Hashtbl.iter
        (fun _ parent ->
          if not parent.viewport_cull then
            toffee_exn
              (Toffee.retain_children parent.ctx.tree parent.toffee_node
                 (fun i ->
                   match parent.children.(i) with
                   | Some child -> Option.is_some child.parent
                   | None -> true));
          let kept = ref 0 and live = ref 0 and first = ref max_int in
          for i = 0 to parent.child_count - 1 do
            match parent.children.(i) with
            | Some child as kept_child when Option.is_some child.parent ->
                parent.children.(!kept) <- kept_child;
                incr kept
            | Some child ->
                first := min !first i;
                (match child.chunk with
                | Some k ->
                    ignore
                      (Toffee.remove_child parent.ctx.tree k.chunk_node
                         child.toffee_node);
                    child.chunk <- None
                | None -> ());
                live := !live + child.live_count;
                update_lifecycle_registration child;
                child.layout_valid <- false
            | None -> ()
          done;
          if parent.viewport_cull then note_regroup parent !first;
          Array.fill parent.children !kept (parent.child_count - !kept) None;
          parent.child_count <- !kept;
          parent.z_dirty <- true;
          parent.cull_dirty <- true;
          parent.layout_dirty <- true;
          adjust_live_count parent (- !live);
          parent.ctx.schedule ())
        parents

(* Moving the nodes one by one would shift the children between each node's
   old place and its new one, which is quadratic for a rotation; this is one
   pass over the children, whatever their order.

   Once every node is a child, each is unparented while it waits for its
   place, as [detach_all] marks the departing children, so a child whose
   parent is set is one to keep. Those from [index] on move, in their order,
   to the end of the array, and the nodes are written before them. A node
   written where it already was keeps its option, so that a run of children
   in place allocates nothing. In the layout tree the children from [index]
   on are removed and added again in their new order; a culling node
   regroups them instead. *)
let place_children t index iter =
  let parent = child_target t in
  check_alive parent;
  if index < 0 || index > parent.child_count then
    invalid_arg "Renderable.place_children: index out of bounds";
  iter (fun n ->
      match n.parent with
      | Some p when p == parent -> ()
      | Some _ | None -> attach_to parent ~index:parent.child_count n);
  iter (fun n -> n.parent <- None);
  let placed = Some parent in
  for i = 0 to index - 1 do
    match parent.children.(i) with
    | Some c when Option.is_none c.parent -> c.parent <- placed
    | Some _ | None -> ()
  done;
  let count = parent.child_count in
  let last = ref (count - 1) in
  for i = count - 1 downto index do
    match parent.children.(i) with
    | Some c as child when Option.is_some c.parent ->
        if !last <> i then (
          parent.children.(!last) <- child;
          c.layout_valid <- false);
        decr last
    | Some _ | None -> ()
  done;
  let next = ref index in
  iter (fun n ->
      if Option.is_none n.parent then (
        n.parent <- placed;
        (match parent.children.(!next) with
        | Some c when c == n -> ()
        | Some _ | None ->
            parent.children.(!next) <- Some n;
            n.layout_valid <- false);
        incr next));
  if parent.viewport_cull then note_regroup parent index
  else if index < count then (
    let tree = parent.ctx.tree in
    toffee_exn
      (Toffee.remove_children_range tree parent.toffee_node (index, count - 1));
    for i = index to count - 1 do
      match parent.children.(i) with
      | Some c ->
          toffee_exn (Toffee.add_child tree parent.toffee_node c.toffee_node)
      | None -> ()
    done);
  parent.z_dirty <- true;
  parent.cull_dirty <- true;
  parent.layout_dirty <- true;
  parent.ctx.schedule ()

(* Detaches every child of [t] in one pass. Detaching them one by one would
   shift the children that follow each one, in [t] and in the layout tree,
   which is quadratic in their number. *)
let detach_children t =
  if t.child_count > 0 then (
    ignore (Toffee.set_children t.ctx.tree t.toffee_node [||]);
    Array.iter
      (function
        | Chunk k -> ignore (Toffee.remove t.ctx.tree k.chunk_node)
        | Direct _ -> ())
      t.layout_children;
    t.layout_children <- [||];
    t.regroup_from <- max_int;
    let live = ref 0 in
    for i = 0 to t.child_count - 1 do
      match t.children.(i) with
      | Some child ->
          t.children.(i) <- None;
          child.parent <- None;
          child.chunk <- None;
          live := !live + child.live_count;
          unregister_lifecycle_if_needed child;
          child.layout_valid <- false
      | None -> ()
    done;
    t.child_count <- 0;
    t.z_dirty <- true;
    t.cull_dirty <- true;
    t.layout_dirty <- true;
    adjust_live_count t (- !live);
    t.ctx.schedule ())

let destroy t =
  if not t.destroyed then (
    blur_focused_subtree t;
    t.destroyed <- true;
    detach_children t;
    t.z_sorted <- [||];
    t.z_len <- 0;
    t.z_dirty <- false;
    t.cull_index <- None;
    t.paint_region <- None;
    detach_impl t;
    t.ctx.unregister t;
    ignore (Toffee.remove t.ctx.tree t.toffee_node);
    t.frame_buffer <- None;
    t.child_target <- None;
    t.mouse_handlers <- [];
    t.click_handler <- None;
    t.key_handlers <- [];
    t.paste_handlers <- [];
    t.key_behavior <- None;
    t.paste_behavior <- None;
    t.focus_handler <- None;
    t.blur_handler <- None;
    t.selection <- None;
    t.child_clip <- None;
    t.text_rows_provider <- None;
    t.pending_provider <- None;
    t.on_update <- None;
    t.on_frame <- None;
    t.on_resize <- None;
    t.size_observer <- None;
    t.on_lifecycle_pass <- None;
    t.cursor_provider <- None;
    let cleanup = t.cleanup in
    t.cleanup <- None;
    Option.iter (fun f -> f ()) cleanup)

(* The children are detached together before any is destroyed, so that each
   one's destruction finds no parent to be removed from. *)
let rec destroy_recursively t =
  if not t.destroyed then (
    let kids = Array.make (max 1 t.child_count) t in
    let n = ref 0 in
    for i = 0 to t.child_count - 1 do
      match t.children.(i) with
      | Some c ->
          kids.(!n) <- c;
          incr n
      | None -> ()
    done;
    detach_children t;
    for i = 0 to !n - 1 do
      destroy_recursively kids.(i)
    done;
    destroy t)

let destroyed t = t.destroyed

(* ───── Sorted Children Caches ───── *)

(* Build a sorted snapshot of [t]'s children using insertion sort. Insertion
   sort is efficient here: child arrays are typically small and nearly sorted
   between frames. *)
let rebuild_sorted_cache (t : t) ~(sort_key : t -> int) : t array =
  let cache = Array.make t.child_count t in
  for i = 0 to t.child_count - 1 do
    match t.children.(i) with Some c -> cache.(i) <- c | None -> ()
  done;
  for i = 1 to t.child_count - 1 do
    let key = cache.(i) in
    let v = sort_key key in
    let j = ref (i - 1) in
    while !j >= 0 && sort_key cache.(!j) > v do
      cache.(!j + 1) <- cache.(!j);
      decr j
    done;
    cache.(!j + 1) <- key
  done;
  cache

let children_z (t : t) : t array =
  if t.child_count = 0 then (
    if t.z_len <> 0 || Array.length t.z_sorted <> 0 then (
      t.z_sorted <- [||];
      t.z_len <- 0;
      t.z_dirty <- false);
    [||])
  else if t.z_dirty || t.z_len <> t.child_count then (
    t.z_sorted <- rebuild_sorted_cache t ~sort_key:(fun n -> n.z_index);
    t.z_len <- t.child_count;
    t.z_dirty <- false;
    t.z_sorted)
  else t.z_sorted

let iter_children_z (t : t) (f : t -> unit) : unit =
  let arr = children_z t in
  for i = 0 to t.z_len - 1 do
    f arr.(i)
  done

(* ───── Visual Properties ───── *)

let visible t = t.visible

(* Visibility via display:none on the layout node.

   Rather than a separate visibility flag in the layout engine, we toggle the
   Toffee display property to [None] when hiding. [original_display] saves the
   pre-hide value (Flex, Block, etc.) so we can restore it on show. This mirrors
   how CSS visibility works and ensures hidden nodes take zero space in layout
   without needing special-case logic in the layout engine. *)

let set_visible t v =
  if t.visible <> v then
    let current = style t in
    let display = if v then t.original_display else Toffee.Style.Display.None in
    let updated = Toffee.Style.set_display display current in
    apply_style t ~preserve_hidden:false updated

let z_index t = t.z_index

let set_z_index t z =
  if t.z_index <> z then (
    t.z_index <- z;
    Option.iter (fun p -> p.z_dirty <- true) t.parent;
    t.ctx.schedule ())

let opacity t = t.opacity

let set_opacity t v =
  let clamped = Float.max 0.0 (Float.min 1.0 v) in
  if t.opacity <> clamped then (
    t.opacity <- clamped;
    t.ctx.schedule ())

let buffered t = t.buffered

let set_buffered t v =
  if t.buffered <> v then (
    t.buffered <- v;
    if not v then t.frame_buffer <- None;
    t.ctx.schedule ())

let live t = t.live

let set_live t v =
  if t.live <> v then (
    t.live <- v;
    update_self_live t)

(* ───── Events ───── *)

(* A node's handlers run newest first, and all of them run: stopping
   propagation keeps an event from other nodes, never from the rest of the
   node that stopped it. A handler that destroys its node ends the node's
   run. The renderer walks an event's path and runs the behaviors. *)

let on_mouse t handler = t.mouse_handlers <- handler :: t.mouse_handlers
let on_key t handler = t.key_handlers <- handler :: t.key_handlers
let on_paste t handler = t.paste_handlers <- handler :: t.paste_handlers
let set_key_behavior t behavior = t.key_behavior <- behavior
let set_paste_behavior t behavior = t.paste_behavior <- behavior

let run_handlers t handlers event =
  let rec run = function
    | [] -> ()
    | handler :: rest ->
        handler event;
        if not t.destroyed then run rest
  in
  run handlers

(* A handler reads the event in its node's cells (Event.Mouse.local). *)
let at_node t event =
  Event.Mouse.Private.at_node event ~x:(x t) ~y:(y t) ~width:(width t)
    ~height:(height t)

(* Only a node with handlers pays for its copy of the event. *)
let emit_mouse t event =
  match t.mouse_handlers with
  | [] -> ()
  | handlers -> run_handlers t handlers (at_node t event)

let emit_key t event = run_handlers t t.key_handlers event
let emit_paste t event = run_handlers t t.paste_handlers event
let run_key_behavior t event = Option.iter (fun f -> f event) t.key_behavior
let run_paste_behavior t event = Option.iter (fun f -> f event) t.paste_behavior

(* ───── Selection ───── *)

let set_selection t ~should_start ~on_change ~clear ~get_text =
  t.selection <- Some { should_start; on_change; clear; get_text }

let unset_selection t = t.selection <- None
let selectable t = Option.is_some t.selection

let emit_selection_changed t sel =
  Option.fold ~none:false ~some:(fun h -> h.on_change sel) t.selection

let clear_selection t = Option.iter (fun h -> h.clear ()) t.selection

let should_start_selection t ~x ~y =
  Option.fold ~none:false ~some:(fun h -> h.should_start ~x ~y) t.selection

let get_selected_text t =
  Option.fold ~none:"" ~some:(fun h -> h.get_text ()) t.selection

(* ───── Text Rows ───── *)

let set_text_rows_provider t provider = t.text_rows_provider <- provider
let text_rows t = Option.map (fun f -> f ()) t.text_rows_provider

(* ───── Pending Render Work ───── *)

let set_pending_provider t provider = t.pending_provider <- provider

let pending_work t =
  match t.pending_provider with None -> None | Some provider -> provider ()

(* ───── Lifecycle ───── *)

let set_on_update t cb = t.on_update <- cb
let set_on_frame t cb = t.on_frame <- cb
let set_on_resize t cb = t.on_resize <- cb

let set_lifecycle_pass t cb =
  t.on_lifecycle_pass <- cb;
  update_lifecycle_registration t

let run_lifecycle_pass t = Option.iter (fun f -> f t) t.on_lifecycle_pass

(* ───── Render Pipeline ───── *)

let[@inline] update_layout t ~x ~y ~width ~height =
  if t.abs == unplaced then
    t.abs <- { abs_x = x; abs_y = y; abs_w = width; abs_h = height }
  else (
    t.abs.abs_x <- x;
    t.abs.abs_y <- y;
    t.abs.abs_w <- width;
    t.abs.abs_h <- height);
  t.layout_valid <- true

(* Refresh direct children's cached layout from the layout tree, mirroring
   OpenTUI's updateFromLayout pass before [_getVisibleChildren].
   [t.abs] is current because the renderer updates a node's layout
   before descending into its children. The node in the top layer is laid
   out as a root, at the root's origin. *)
let update_children_layout t =
  for i = 0 to t.child_count - 1 do
    match t.children.(i) with
    | Some child when child.visible -> (
        match Toffee.layout t.ctx.tree child.toffee_node with
        | Ok (l : Toffee.Layout.t) ->
            let ox, oy =
              if in_top_layer child then (0., 0.) else (t.abs.abs_x, t.abs.abs_y)
            in
            update_layout child
              ~x:(ox +. location_x child l)
              ~y:(oy +. location_y child l)
              ~width:l.size.width ~height:l.size.height
        | Error _ -> ())
    | Some _ | None -> ()
  done

(* A culling node's index refreshes its direct children alone; below them,
   only a paint does. *)
let rec refresh_positions t =
  update_children_layout t;
  for i = 0 to t.child_count - 1 do
    match t.children.(i) with
    | Some child when child.visible -> refresh_positions child
    | Some _ | None -> ()
  done

(* ───── Viewport Culling ─────

   A scroll container's content node culls: the renderer descends only into
   the children that can paint inside the effective clip. Culling works in the
   parent's coordinates, where scrolling moves nothing, from an index of the
   children sorted by where their subtrees may paint. The layout tree replaces
   a node's layout value whenever the layout, the style or the children of the
   node or of any node below it change, so an index, and a subtree's paint
   region, stay valid while the layout value they were built from is the same
   value. Rebuilding is linear in the children; a query costs a binary search
   and the children it keeps. *)

let toffee_layout t =
  match Toffee.layout t.ctx.tree t.toffee_node with
  | Ok layout -> layout
  | Error _ -> Toffee.Layout.default

(* The region [t]'s subtree may paint, relative to its layout origin: its box,
   grown on both axes by its visible children's regions unless [t] clips them.
   A node's own paint and its clip lie within its box. *)
let rec paint_region t =
  let layout = toffee_layout t in
  match t.paint_region with
  | Some region when region.layout == layout -> region
  | Some _ | None ->
      let x1 = terminal_edge layout.size.width
      and y1 = terminal_edge layout.size.height in
      let region = ref { layout; x0 = 0; y0 = 0; x1; y1 } in
      if not (clips t) then
        for i = 0 to t.child_count - 1 do
          match t.children.(i) with
          | Some child when child.visible && not (in_top_layer child) ->
              let r = !region and c = child_region child in
              region :=
                {
                  r with
                  x0 = min r.x0 c.x0;
                  y0 = min r.y0 c.y0;
                  x1 = max r.x1 c.x1;
                  y1 = max r.y1 c.y1;
                }
          | Some _ | None -> ()
        done;
      t.paint_region <- Some !region;
      !region

(* [child]'s paint region relative to its parent's layout origin. *)
and child_region child =
  let layout = toffee_layout child and r = paint_region child in
  let x = terminal_edge (location_x child layout) + child.translate_x
  and y = terminal_edge (location_y child layout) + child.translate_y in
  { r with x0 = x + r.x0; y0 = y + r.y0; x1 = x + r.x1; y1 = y + r.y1 }

(* [entries], built from the layout value [built_from], sorted by [p0]
   (children are usually in that order already, and an insertion sort is
   then linear), with the reach of each prefix. *)
let sorted_index ~built_from ~column entries =
  let entries = Array.of_list entries in
  for i = 1 to Array.length entries - 1 do
    let e = entries.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && entries.(!j).p0 > e.p0 do
      entries.(!j + 1) <- entries.(!j);
      decr j
    done;
    entries.(!j + 1) <- e
  done;
  let reach = Array.make (Array.length entries) min_int in
  Array.iteri
    (fun i e -> reach.(i) <- (if i = 0 then e.p1 else max reach.(i - 1) e.p1))
    entries;
  { built_from; entries; reach; column }

let column_entry ?stands_for child ~rank (r : paint_region) =
  { child; stands_for; rank; p0 = r.y0; p1 = r.y1; c0 = r.x0; c1 = r.x1 }

(* The index of [k]'s visible members, ranked from [base]. The layout tree
   replaces the chunk's layout value whenever the chunk moves or a member
   changes, and a translation below a member forgets the index. *)
let chunk_index k ~base =
  let built_from =
    match Toffee.layout k.members.(0).ctx.tree k.chunk_node with
    | Ok layout -> layout
    | Error _ -> Toffee.Layout.default
  in
  match k.index with
  | Some index when index.built_from == built_from && k.base = base -> index
  | Some _ | None ->
      let entries = ref [] in
      for j = Array.length k.members - 1 downto 0 do
        let c = k.members.(j) in
        if c.visible then
          entries :=
            column_entry c ~rank:(base + j) (child_region c) :: !entries
      done;
      let index = sorted_index ~built_from ~column:true !entries in
      k.index <- Some index;
      k.base <- base;
      index

let cull_index t =
  let layout = toffee_layout t in
  match t.cull_index with
  | Some index when index.built_from == layout && not t.cull_dirty -> index
  | (Some _ | None)
    when Array.exists
           (function Chunk _ -> true | Direct _ -> false)
           t.layout_children ->
      (* An entry for each visible direct child, and one for each chunk with
         a visible member, whose region joins its members'. *)
      let entries = ref [] and rank = ref 0 in
      Array.iter
        (fun item ->
          (match item with
          | Direct c ->
              if c.visible then
                entries :=
                  column_entry c ~rank:!rank (child_region c) :: !entries
          | Chunk k ->
              let index = chunk_index k ~base:!rank in
              let n = Array.length index.entries in
              if n > 0 then (
                let y0 = index.entries.(0).p0 and y1 = index.reach.(n - 1) in
                let x0 = ref max_int and x1 = ref min_int in
                Array.iter
                  (fun e ->
                    x0 := min !x0 e.c0;
                    x1 := max !x1 e.c1)
                  index.entries;
                entries :=
                  {
                    child = k.members.(0);
                    stands_for = Some k;
                    rank = !rank;
                    p0 = y0;
                    p1 = y1;
                    c0 = !x0;
                    c1 = !x1;
                  }
                  :: !entries));
          rank := !rank + layout_child_length item)
        t.layout_children;
      let index =
        sorted_index ~built_from:layout ~column:true (List.rev !entries)
      in
      t.cull_index <- Some index;
      t.cull_dirty <- false;
      index
  | Some _ | None ->
      update_children_layout t;
      let column =
        match Toffee.Style.flex_direction t.style with
        | Toffee.Style.Flex_direction.Column | Column_reverse -> true
        | Row | Row_reverse -> false
      in
      let entries = ref [] in
      for rank = t.child_count - 1 downto 0 do
        match t.children.(rank) with
        | Some child when child.visible ->
            let r = child_region child in
            let entry =
              if column then column_entry child ~rank r
              else
                {
                  child;
                  stands_for = None;
                  rank;
                  p0 = r.x0;
                  p1 = r.x1;
                  c0 = r.y0;
                  c1 = r.y1;
                }
            in
            entries := entry :: !entries
        | Some _ | None -> ()
      done;
      let index = sorted_index ~built_from:layout ~column !entries in
      t.cull_index <- Some index;
      t.cull_dirty <- false;
      index

(* A region overlaps [lo, hi) strictly. One with zero extent on an axis has no
   interior there, but its subtree can still paint, and it sits exactly on an
   edge in the common case (every child of a column starts at the container's
   left edge), so zero extent keeps the closed interval. *)
let overlaps a0 a1 lo hi =
  if a0 = a1 then a0 >= lo && a0 <= hi else a1 > lo && a0 < hi

(* [query_index index ~lo ~hi ~cross_lo ~cross_hi kept] is [kept] with the
   entries of [index] whose regions overlap [lo, hi) and [cross_lo, cross_hi)
   added. Every entry before the first whose reach meets [lo] ends above it.
   An entry for a chunk adds the members its own index finds. *)
let rec query_index index ~lo ~hi ~cross_lo ~cross_hi kept =
  let kept = ref kept in
  let n = Array.length index.entries in
  let first = ref 0 and last = ref n in
  while !first < !last do
    let mid = (!first + !last) / 2 in
    if index.reach.(mid) < lo then first := mid + 1 else last := mid
  done;
  let i = ref !first in
  while !i < n && index.entries.(!i).p0 <= hi do
    let e = index.entries.(!i) in
    (if overlaps e.p0 e.p1 lo hi && overlaps e.c0 e.c1 cross_lo cross_hi then
       match e.stands_for with
       | None -> kept := e :: !kept
       | Some k ->
           (* The parent's index was built after the chunk's last layout, and
              the chunk's index with it. *)
           let index =
             match k.index with
             | Some index when k.base = e.rank -> index
             | Some _ | None -> chunk_index k ~base:e.rank
           in
           kept := query_index index ~lo ~hi ~cross_lo ~cross_hi !kept);
    incr i
  done;
  !kept

let children_in_viewport ~(parent : t) ~(viewport : Grid.region) : t list =
  let index = cull_index parent in
  let px = x parent and py = y parent in
  let lo, hi, cross_lo, cross_hi =
    if index.column then
      ( viewport.y - py,
        viewport.y + viewport.height - py,
        viewport.x - px,
        viewport.x + viewport.width - px )
    else
      ( viewport.x - px,
        viewport.x + viewport.width - px,
        viewport.y - py,
        viewport.y + viewport.height - py )
  in
  let kept = query_index index ~lo ~hi ~cross_lo ~cross_hi [] in
  List.sort
    (fun a b ->
      match Int.compare a.child.z_index b.child.z_index with
      | 0 -> Int.compare a.rank b.rank
      | order -> order)
    kept
  |> List.map (fun e -> e.child)

let iter_children_in_viewport ~parent ~viewport f =
  List.iter f (children_in_viewport ~parent ~viewport)

let pre_render_update ?(frame = true) t ~dt =
  Option.iter (fun f -> f t ~dt) t.on_update;
  if frame && t.live then Option.iter (fun f -> f t ~dt) t.on_frame;
  let lw = width t and lh = height t in
  if lw <> t.last_width || lh <> t.last_height then (
    t.last_width <- lw;
    t.last_height <- lh;
    Option.iter (fun f -> f t) t.on_resize);
  (* A refused size report stays pending: it is offered again at the next
     traversal until the renderer admits it. *)
  match t.size_observer with
  | Some f
    when (lw <> t.reported_width || lh <> t.reported_height)
         && t.ctx.admit_size_report t ->
      t.reported_width <- lw;
      t.reported_height <- lh;
      f t
  | Some _ | None -> ()

let render_node t grid ~dt = t.render t grid ~dt

let ensure_frame_buffer (t : t) ~(parent : Grid.t) : Grid.t option =
  let w = width t and h = height t in
  if w <= 0 || h <= 0 then None
  else
    match t.frame_buffer with
    | Some buf ->
        if Grid.width buf <> w || Grid.height buf <> h then
          Grid.resize buf ~width:w ~height:h;
        Some buf
    | None ->
        let buf =
          Grid.create ~width:w ~height:h
            ~width_method:(Grid.width_method parent) ()
        in
        t.frame_buffer <- Some buf;
        Some buf

let blit_frame_buffer (t : t) ~(dst : Grid.t) : unit =
  match t.frame_buffer with
  | None -> ()
  | Some buf ->
      let w = width t and h = height t in
      if w > 0 && h > 0 then
        Grid.draw_region ~src:buf ~dst ~src_x:0 ~src_y:0 ~width:w ~height:h
          ~dst_x:(x t) ~dst_y:(y t)

let render_full (t : t) ~(grid : Grid.t) ~(dt : float) : unit =
  let render_target, local_origin =
    if t.buffered then
      match ensure_frame_buffer t ~parent:grid with
      | Some buf -> (buf, true)
      | None -> (grid, false)
    else (grid, false)
  in
  let original_tx = t.translate_x and original_ty = t.translate_y in
  if local_origin then (
    let ox = x t and oy = y t in
    (* Render buffered subtrees in local coordinates to avoid double offsets. *)
    t.translate_x <- t.translate_x - ox;
    t.translate_y <- t.translate_y - oy);
  Fun.protect
    ~finally:(fun () ->
      if local_origin then (
        t.translate_x <- original_tx;
        t.translate_y <- original_ty);
      if t.buffered then blit_frame_buffer t ~dst:grid)
    (fun () ->
      Option.iter (fun f -> f t render_target ~dt) t.render_before;
      t.render t render_target ~dt;
      Option.iter (fun f -> f t render_target ~dt) t.render_after)

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Node(%s, %dx%d@%d,%d)" t.id (width t) (height t) (x t)
    (y t)

(* ───── Private ───── *)

module Private = struct
  let set_focus_request t callback = t.focus_request <- callback

  let request_focus t =
    match t.focus_request with
    | Some callback when not t.destroyed ->
        callback ();
        true
    | None | Some _ -> false

  let has_focus_request t = Option.is_some t.focus_request
  let top_layer t = t.ctx.top_layer
  let set_top_layer t top = t.ctx.top_layer <- top

  type nonrec context = context = {
    tree : unit Toffee.tree;
    schedule : unit -> unit;
    off_loop : (unit -> unit) -> unit;
    focus : t -> bool;
    blur : t -> unit;
    get_selection : unit -> Selection.t option;
    request_selection_update : unit -> unit;
    register_lifecycle : t -> unit;
    unregister_lifecycle : t -> unit;
    alloc_num : unit -> int;
    register : t -> unit;
    unregister : t -> unit;
    width_method : unit -> Matrix.Text.width_method;
    cell_size : unit -> (int * int) option;
    report_scroll : t -> region:Grid.region -> dx:int -> dy:int -> unit;
    admit_size_report : t -> bool;
    mutable regroups : t list;
    mutable top_layer : t option;
    mutable theme : Theme.t;
  }

  let create_root ctx ?id ?style () =
    let num = ctx.alloc_num () in
    let prepared =
      Option.fold ~none:Toffee.Style.default ~some:prepare_style style
    in
    let toffee_node = toffee_exn (Toffee.new_leaf ctx.tree prepared) in
    let id = Option.value id ~default:(Printf.sprintf "node-%d" num) in
    make_node ctx ~toffee_node ~id ~num ~style:prepared ()

  let set_cleanup t f =
    check_alive t;
    t.cleanup <- Some f

  let num t = t.num
  let toffee_node t = t.toffee_node
  let detach_all = detach_all
  let place_children = place_children

  let child_at t i =
    if i < 0 || i >= t.child_count then None else t.children.(i)

  let set_place_theme t f = set_themes t ~place:true f
  let width_method t = t.ctx.width_method ()
  let cell_size t = t.ctx.cell_size ()

  let set_is_root t v =
    if t.is_root <> v then (
      t.is_root <- v;
      update_lifecycle_registration t)

  let layout_dirty t = t.layout_dirty
  let clear_layout_dirty t = t.layout_dirty <- false
  let update_layout = update_layout
  let refresh_positions = refresh_positions
  let measure t = t.measure
  let pre_render_update = pre_render_update
  let render = render_node
  let render_before t = t.render_before
  let render_after t = t.render_after
  let ensure_frame_buffer = ensure_frame_buffer
  let blit_frame_buffer = blit_frame_buffer
  let render_full = render_full
  let pending_work = pending_work
  let children_z = children_z
  let iter_children_z = iter_children_z
  let children_in_viewport = children_in_viewport
  let iter_children_in_viewport = iter_children_in_viewport
  let set_viewport_cull = set_viewport_cull
  let flush_regroups = flush_regroups
  let location_x = location_x
  let location_y = location_y
  let viewport_cull t = t.viewport_cull
  let focus_direct = focus_direct
  let blur_direct = blur_direct
  let live_count t = t.live_count
  let set_size_observer t callback = t.size_observer <- callback
  let set_on_live_count_change t cb = t.live_count_change <- cb
  let set_lifecycle_pass = set_lifecycle_pass
  let run_lifecycle_pass = run_lifecycle_pass
  let set_click_handler t handler = t.click_handler <- handler

  let emit_click t event =
    Option.iter (fun handler -> handler (at_node t event)) t.click_handler

  let emit_mouse = emit_mouse
  let emit_key = emit_key
  let run_key_behavior = run_key_behavior
  let emit_paste = emit_paste
  let run_paste_behavior = run_paste_behavior
  let emit_selection_changed = emit_selection_changed
  let clear_selection = clear_selection
  let should_start_selection = should_start_selection
  let get_selected_text = get_selected_text
  let get_selection t = t.ctx.get_selection ()
  let request_selection_update t = t.ctx.request_selection_update ()
  let report_scroll t ~region ~dx ~dy = t.ctx.report_scroll t ~region ~dx ~dy

  (* A clipping node always yields a region — possibly empty — so the
     renderer can skip its children entirely instead of painting them
     unclipped when the node quantizes to zero extent. *)
  let child_clip t =
    match t.child_clip with Some f -> f t | None -> Some (bounds t)
end
