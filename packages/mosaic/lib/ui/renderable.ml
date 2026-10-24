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
   z-indices. *)
and cull_entry = {
  child : node;
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
  mutable abs_x : float;
  mutable abs_y : float;
  mutable abs_w : float;
  mutable abs_h : float;
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
    abs_x = 0.;
    abs_y = 0.;
    abs_w = 0.;
    abs_h = 0.;
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

(* ───── Layout ───── *)

let mark_dirty t =
  t.layout_dirty <- true;
  toffee_exn (Toffee.mark_dirty t.ctx.tree t.toffee_node)

let apply_style t ~preserve_hidden new_style =
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

let translate_acc t =
  let rec loop node ax ay =
    let ax = ax + node.translate_x and ay = ay + node.translate_y in
    match node.parent with None -> (ax, ay) | Some p -> loop p ax ay
  in
  loop t 0 0

let terminal_edge value = int_of_float (Float.round value)

(* Quantize both ends of a Toffee interval. Rounding its length independently
   can overlap an adjacent interval whose shared edge rounds down. *)
let terminal_span start length =
  max 0 (terminal_edge (start +. length) - terminal_edge start)

let x t =
  let ox, _ = translate_acc t in
  if t.layout_valid then terminal_edge t.abs_x + ox else ox

let y t =
  let _, oy = translate_acc t in
  if t.layout_valid then terminal_edge t.abs_y + oy else oy

let width t =
  if t.layout_valid && t.visible then terminal_span t.abs_x t.abs_w else 0

let height t =
  if t.layout_valid && t.visible then terminal_span t.abs_y t.abs_h else 0

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

let detach_impl child =
  match child.parent with
  | None -> ()
  | Some parent ->
      remove_child_ref parent child;
      child.parent <- None;
      ignore
        (Toffee.remove_child child.ctx.tree parent.toffee_node child.toffee_node);
      update_lifecycle_registration child;
      child.layout_valid <- false;
      parent.layout_dirty <- true

let attach_impl ~parent ~index child =
  let idx =
    if index < 0 then 0
    else if index > parent.child_count then parent.child_count
    else index
  in
  if idx >= parent.child_count then
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

let attach ~parent ?index t =
  let parent = child_target parent in
  check_alive parent;
  check_alive t;
  if parent == t then
    invalid_arg "Renderable.attach: cannot attach node to itself";
  if not (parent.ctx.tree == t.ctx.tree) then
    invalid_arg "Renderable.attach: nodes belong to different trees";
  if Option.is_some t.parent then detach_impl t;
  attach_impl ~parent ~index:(Option.value index ~default:parent.child_count) t

let detach t =
  blur_focused_subtree t;
  detach_impl t

let destroy t =
  if not t.destroyed then (
    blur_focused_subtree t;
    t.destroyed <- true;
    (* Clean up children: clear parent pointers, remove from layout tree,
       unregister lifecycle. Live counts propagate correctly because
       t.live_count still includes children's contributions when detach_impl
       runs — the parent gets the full subtree decrement. *)
    for i = 0 to t.child_count - 1 do
      match t.children.(i) with
      | Some child ->
          ignore
            (Toffee.remove_child t.ctx.tree t.toffee_node child.toffee_node);
          child.parent <- None;
          unregister_lifecycle_if_needed child;
          child.layout_valid <- false;
          t.children.(i) <- None
      | None -> ()
    done;
    t.child_count <- 0;
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

let emit_mouse t event = run_handlers t t.mouse_handlers event
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

let update_layout t ~x ~y ~width ~height =
  t.abs_x <- x;
  t.abs_y <- y;
  t.abs_w <- width;
  t.abs_h <- height;
  t.layout_valid <- true

(* Refresh direct children's cached layout from the layout tree, mirroring
   OpenTUI's updateFromLayout pass before [_getVisibleChildren].
   [t.abs_x]/[t.abs_y] are current because the renderer updates a node's layout
   before descending into its children. *)
let update_children_layout t =
  for i = 0 to t.child_count - 1 do
    match t.children.(i) with
    | Some child when child.visible -> (
        match Toffee.layout t.ctx.tree child.toffee_node with
        | Ok (l : Toffee.Layout.t) ->
            update_layout child ~x:(t.abs_x +. l.location.x)
              ~y:(t.abs_y +. l.location.y) ~width:l.size.width
              ~height:l.size.height
        | Error _ -> ())
    | Some _ | None -> ()
  done

(* ───── Viewport Culling ─────

   A scroll container's content node culls: the renderer descends only into
   the children that can paint inside the effective clip. Culling works in the
   parent's coordinates, where scrolling moves nothing, from an index of the
   children sorted by where their subtrees may paint. The layout tree replaces
   a node's layout value whenever the layout of the node or of any node below
   it changes, so an index, and a subtree's paint region, stay valid while the
   layout value they were built from is the same value. Rebuilding is linear in
   the children; a query costs a binary search and the children it keeps. *)

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
          | Some child when child.visible ->
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
  let x = terminal_edge layout.location.x + child.translate_x
  and y = terminal_edge layout.location.y + child.translate_y in
  { r with x0 = x + r.x0; y0 = y + r.y0; x1 = x + r.x1; y1 = y + r.y1 }

let cull_index t =
  let layout = toffee_layout t in
  match t.cull_index with
  | Some index when index.built_from == layout && not t.cull_dirty -> index
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
              if column then
                { child; rank; p0 = r.y0; p1 = r.y1; c0 = r.x0; c1 = r.x1 }
              else { child; rank; p0 = r.x0; p1 = r.x1; c0 = r.y0; c1 = r.y1 }
            in
            entries := entry :: !entries
        | Some _ | None -> ()
      done;
      (* Children are usually already in primary order: an insertion sort is
         linear then. *)
      let entries = Array.of_list !entries in
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
        (fun i e ->
          reach.(i) <- (if i = 0 then e.p1 else max reach.(i - 1) e.p1))
        entries;
      let index = { built_from = layout; entries; reach; column } in
      t.cull_index <- Some index;
      t.cull_dirty <- false;
      index

(* A region overlaps [lo, hi) strictly. One with zero extent on an axis has no
   interior there, but its subtree can still paint, and it sits exactly on an
   edge in the common case (every child of a column starts at the container's
   left edge), so zero extent keeps the closed interval. *)
let overlaps a0 a1 lo hi =
  if a0 = a1 then a0 >= lo && a0 <= hi else a1 > lo && a0 < hi

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
  (* Every entry before the first whose reach meets [lo] ends above it. *)
  let n = Array.length index.entries in
  let first = ref 0 and last = ref n in
  while !first < !last do
    let mid = (!first + !last) / 2 in
    if index.reach.(mid) < lo then first := mid + 1 else last := mid
  done;
  let kept = ref [] in
  let i = ref !first in
  while !i < n && index.entries.(!i).p0 <= hi do
    let e = index.entries.(!i) in
    if overlaps e.p0 e.p1 lo hi && overlaps e.c0 e.c1 cross_lo cross_hi then
      kept := e :: !kept;
    incr i
  done;
  List.sort
    (fun a b ->
      match Int.compare a.child.z_index b.child.z_index with
      | 0 -> Int.compare a.rank b.rank
      | order -> order)
    !kept
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

  let child_at t i =
    if i < 0 || i >= t.child_count then None else t.children.(i)

  let width_method t = t.ctx.width_method ()
  let cell_size t = t.ctx.cell_size ()

  let set_is_root t v =
    if t.is_root <> v then (
      t.is_root <- v;
      update_lifecycle_registration t)

  let layout_dirty t = t.layout_dirty
  let clear_layout_dirty t = t.layout_dirty <- false
  let update_layout = update_layout
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
  let set_viewport_cull t v = t.viewport_cull <- v
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
    Option.iter (fun handler -> handler event) t.click_handler

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
