(* ───── Types ───── *)

type line_info = {
  line_count : int;
  display_line_count : int;
  line_sources : int array;
  line_wrap_indices : int array;
  scroll_y : int;
}

type cursor = {
  x : int;
  y : int;
  style : [ `Block | `Line | `Underline ];
  color : Ansi.Color.t;
  blinking : bool;
}

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
  focus : node -> bool;
  blur : node -> unit;
  register_lifecycle : node -> unit;
  unregister_lifecycle : node -> unit;
  alloc_num : unit -> int;
  register : node -> unit;
  unregister : node -> unit;
}

and node = {
  ctx : context;
  toffee_node : Toffee.Node_id.t;
  id : string;
  num : int;
  mutable destroyed : bool;
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
  mutable primary_sorted : node array;
  mutable primary_len : int;
  mutable primary_dirty : bool;
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
  (* Events *)
  mutable mouse_handlers : (Event.mouse -> unit) list;
  mutable key_handlers : (Event.key -> unit) list;
  mutable default_key_handler : (Event.key -> unit) option;
  mutable paste_handler : (Event.paste -> unit) option;
  (* Selection *)
  mutable selection : selection_handler option;
  (* Child clipping *)
  mutable child_clip : (node -> Grid.region option) option;
  (* Line information *)
  mutable line_info_provider : (unit -> line_info) option;
  (* Lifecycle *)
  mutable on_frame : (node -> delta:float -> unit) option;
  mutable on_resize : (node -> unit) option;
  mutable on_lifecycle_pass : (node -> unit) option;
  mutable last_width : int;
  mutable last_height : int;
  mutable lifecycle_registered : bool;
  mutable is_root : bool;
  mutable live_count_change : (node -> unit) option;
}

and render = node -> Grid.t -> delta:float -> unit

type t = node

(* ───── Helpers ───── *)

let toffee_exn = function
  | Ok x -> x
  | Error e -> invalid_arg (Toffee.Error.to_string e)

let check_alive t =
  if t.destroyed then invalid_arg "Renderable: node is destroyed"

let render_noop _ _ ~delta:_ = ()

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
  parent.primary_dirty <- true;
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
      parent.primary_dirty <- true;
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
    primary_sorted = [||];
    primary_len = 0;
    primary_dirty = false;
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
    mouse_handlers = [];
    key_handlers = [];
    default_key_handler = None;
    paste_handler = None;
    selection = None;
    child_clip = None;
    line_info_provider = None;
    on_frame = None;
    on_resize = None;
    on_lifecycle_pass = None;
    last_width = 0;
    last_height = 0;
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
    t.ctx.schedule ();
    true)

let blur_direct t =
  if t.focused then (
    t.focused <- false;
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

(* ───── Rendering ───── *)

let request_render t = t.ctx.schedule ()

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
  t.primary_dirty <- true;
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

let x t =
  let ox, _ = translate_acc t in
  if t.layout_valid then int_of_float (Float.round t.abs_x) + ox else ox

let y t =
  let _, oy = translate_acc t in
  if t.layout_valid then int_of_float (Float.round t.abs_y) + oy else oy

let width t =
  if t.layout_valid && t.visible then max 1 (int_of_float (Float.round t.abs_w))
  else 0

let height t =
  if t.layout_valid && t.visible then max 1 (int_of_float (Float.round t.abs_h))
  else 0

let bounds t : Grid.region =
  { x = x t; y = y t; width = width t; height = height t }

let set_translate t ~x:ox ~y:oy =
  if t.translate_x <> ox || t.translate_y <> oy then (
    t.translate_x <- ox;
    t.translate_y <- oy;
    Option.iter (fun p -> p.primary_dirty <- true) t.parent;
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
    t.primary_sorted <- [||];
    t.primary_len <- 0;
    t.primary_dirty <- false;
    detach_impl t;
    t.ctx.unregister t;
    ignore (Toffee.remove t.ctx.tree t.toffee_node);
    t.frame_buffer <- None;
    t.child_target <- None;
    t.mouse_handlers <- [];
    t.key_handlers <- [];
    t.default_key_handler <- None;
    t.paste_handler <- None;
    t.selection <- None;
    t.child_clip <- None;
    t.line_info_provider <- None;
    t.on_frame <- None;
    t.on_resize <- None;
    t.on_lifecycle_pass <- None;
    t.cursor_provider <- None)

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

(* ───── Primary-Sorted Children (Viewport Culling) ───── *)

let children_sorted_by_primary (t : t) : t array =
  if t.child_count = 0 then (
    if t.primary_len <> 0 || Array.length t.primary_sorted <> 0 then (
      t.primary_sorted <- [||];
      t.primary_len <- 0;
      t.primary_dirty <- false);
    [||])
  else if t.primary_dirty || t.primary_len <> t.child_count then (
    let module FD = Toffee.Style.Flex_direction in
    let sort_key =
      match Toffee.Style.flex_direction t.style with
      | FD.Row | FD.Row_reverse -> fun n -> x n
      | FD.Column | FD.Column_reverse -> fun n -> y n
    in
    t.primary_sorted <- rebuild_sorted_cache t ~sort_key;
    t.primary_len <- t.child_count;
    t.primary_dirty <- false;
    t.primary_sorted)
  else t.primary_sorted

(* ───── Viewport Culling ───── *)

(* Viewport culling for scroll containers.

   Children sorted by primary axis position enable binary search for the first
   visible child. We skip binary search for fewer than 16 children (linear scan
   is faster at that scale). After finding the entry point, walk_left extends
   backwards with a gap tolerance of 50 non-overlapping children to handle
   sparse layouts where large gaps separate visible clusters. Both primary and
   cross-axis intersection tests are applied before including a child. Results
   are z-sorted for correct paint order. *)

let children_in_viewport ~(parent : t) ~(viewport : Grid.region)
    ~(padding : int) : t list =
  let module FD = Toffee.Style.Flex_direction in
  let arr = children_sorted_by_primary parent in
  let total = Array.length arr in
  if total = 0 then []
  else
    (* Project a clip_rect onto primary/cross axes based on flex direction. *)
    let primary_pos, primary_size, cross_pos, cross_size =
      match Toffee.Style.flex_direction (style parent) with
      | FD.Row | FD.Row_reverse ->
          ( (fun (r : Grid.region) -> r.x),
            (fun (r : Grid.region) -> r.width),
            (fun (r : Grid.region) -> r.y),
            fun (r : Grid.region) -> r.height )
      | FD.Column | FD.Column_reverse ->
          ( (fun (r : Grid.region) -> r.y),
            (fun (r : Grid.region) -> r.height),
            (fun (r : Grid.region) -> r.x),
            fun (r : Grid.region) -> r.width )
    in
    let vp_start = primary_pos viewport - padding in
    let vp_end = primary_pos viewport + primary_size viewport + padding in
    let cross_start = cross_pos viewport - padding in
    let cross_end = cross_pos viewport + cross_size viewport + padding in
    let left, right =
      if total < 16 then (0, total)
      else
        (* Binary search for first child overlapping the viewport. *)
        let rec bin lo hi candidate =
          if lo > hi then candidate
          else
            let mid = (lo + hi) lsr 1 in
            let cb = bounds arr.(mid) in
            let start = primary_pos cb in
            let stop = start + primary_size cb in
            if stop < vp_start then bin (mid + 1) hi candidate
            else if start > vp_end then bin lo (mid - 1) candidate
            else bin lo (mid - 1) (Some mid)
        in
        let start_index =
          match bin 0 (total - 1) None with None -> 0 | Some i -> i
        in
        (* Walk left to catch children that overlap but sort earlier. *)
        let rec walk_left i gaps =
          if i <= 0 then 0
          else
            let cb = bounds arr.(i - 1) in
            let prev_end = primary_pos cb + primary_size cb in
            if prev_end <= vp_start then
              if gaps + 1 >= 50 then i else walk_left (i - 1) (gaps + 1)
            else walk_left (i - 1) 0
        in
        let left = walk_left start_index 0 in
        (* Walk right to find the end of the visible range. *)
        let rec walk_right i =
          if i >= total then total
          else
            let start = primary_pos (bounds arr.(i)) in
            if start >= vp_end then i else walk_right (i + 1)
        in
        let right = walk_right (start_index + 1) in
        (left, right)
    in
    (* Collect children that pass both primary and cross-axis tests. *)
    let result = ref [] in
    for i = left to right - 1 do
      let child = arr.(i) in
      let cb = bounds child in
      let p0 = primary_pos cb in
      let p1 = p0 + primary_size cb in
      if p1 > vp_start && p0 < vp_end then
        let c0 = cross_pos cb in
        let c1 = c0 + cross_size cb in
        if c1 > cross_start && c0 < cross_end then result := child :: !result
    done;
    List.sort (fun a b -> compare a.z_index b.z_index) !result

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

(* Event dispatch model: - Mouse: handlers run in registration order (newest
   first), then bubble to parent unless [stop_propagation] is called. Bubbling
   matches DOM semantics and lets containers handle events their children
   ignore. - Key: handlers run newest-first, stopping when one calls
   [prevent_default]. No bubbling — keys target the focused node only. A
   separate [default_key_handler] runs after all regular handlers as a fallback
   (used by widgets to provide base key behavior that can be overridden by user
   handlers). - Paste: single handler per node (last wins). No bubbling. *)

let on_mouse t handler = t.mouse_handlers <- handler :: t.mouse_handlers
let on_key t handler = t.key_handlers <- handler :: t.key_handlers
let set_default_key_handler t handler = t.default_key_handler <- handler
let set_paste_handler t handler = t.paste_handler <- handler

let rec emit_mouse t event =
  List.iter (fun handler -> handler event) t.mouse_handlers;
  if not (Event.Mouse.propagation_stopped event) then
    match t.parent with Some p -> emit_mouse p event | None -> ()

let emit_key t event =
  let rec run = function
    | [] -> ()
    | handler :: rest ->
        handler event;
        if not (Event.Key.default_prevented event) then run rest
  in
  run t.key_handlers

let emit_default_key t event =
  Option.iter (fun handler -> handler event) t.default_key_handler

let emit_paste t event =
  Option.iter (fun handler -> handler event) t.paste_handler

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

(* ───── Line Information ───── *)

let set_line_info_provider t provider = t.line_info_provider <- provider
let line_info t = Option.map (fun f -> f ()) t.line_info_provider

(* ───── Lifecycle ───── *)

let set_on_frame t cb = t.on_frame <- cb
let set_on_resize t cb = t.on_resize <- cb

let set_lifecycle_pass t cb =
  t.on_lifecycle_pass <- cb;
  update_lifecycle_registration t

let run_lifecycle_pass t = Option.iter (fun f -> f t) t.on_lifecycle_pass

(* ───── Render Pipeline ───── *)

let update_layout t ~x ~y ~width ~height =
  let prev_x = t.abs_x and prev_y = t.abs_y in
  let was_valid = t.layout_valid in
  t.abs_x <- x;
  t.abs_y <- y;
  t.abs_w <- width;
  t.abs_h <- height;
  t.layout_valid <- true;
  let pos_changed =
    was_valid
    && (Float.round prev_x <> Float.round x
       || Float.round prev_y <> Float.round y)
  in
  if pos_changed then Option.iter (fun p -> p.primary_dirty <- true) t.parent

let pre_render_update t ~delta =
  if t.live then Option.iter (fun f -> f t ~delta) t.on_frame;
  let lw = width t and lh = height t in
  if lw <> t.last_width || lh <> t.last_height then (
    t.last_width <- lw;
    t.last_height <- lh;
    Option.iter (fun f -> f t) t.on_resize)

let render_node t grid ~delta = t.render t grid ~delta

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
            ~width_method:(Grid.width_method parent) ~respect_alpha:true ()
        in
        t.frame_buffer <- Some buf;
        Some buf

let blit_frame_buffer (t : t) ~(dst : Grid.t) : unit =
  match t.frame_buffer with
  | None -> ()
  | Some buf ->
      let w = width t and h = height t in
      if w > 0 && h > 0 then
        Grid.blit_region ~src:buf ~dst ~src_x:0 ~src_y:0 ~width:w ~height:h
          ~dst_x:(x t) ~dst_y:(y t)

let render_full (t : t) ~(grid : Grid.t) ~(delta : float) : unit =
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
      Option.iter (fun f -> f t render_target ~delta) t.render_before;
      t.render t render_target ~delta;
      Option.iter (fun f -> f t render_target ~delta) t.render_after)

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Node(%s, %dx%d@%d,%d)" t.id (width t) (height t) (x t)
    (y t)

(* ───── Private ───── *)

module Private = struct
  type nonrec context = context = {
    tree : unit Toffee.tree;
    schedule : unit -> unit;
    focus : t -> bool;
    blur : t -> unit;
    register_lifecycle : t -> unit;
    unregister_lifecycle : t -> unit;
    alloc_num : unit -> int;
    register : t -> unit;
    unregister : t -> unit;
  }

  let create_root ctx ?id ?style () =
    let num = ctx.alloc_num () in
    let prepared =
      Option.fold ~none:Toffee.Style.default ~some:prepare_style style
    in
    let toffee_node = toffee_exn (Toffee.new_leaf ctx.tree prepared) in
    let id = Option.value id ~default:(Printf.sprintf "node-%d" num) in
    make_node ctx ~toffee_node ~id ~num ~style:prepared ()

  let num t = t.num
  let toffee_node t = t.toffee_node

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
  let children_z = children_z
  let iter_children_z = iter_children_z
  let children_in_viewport = children_in_viewport
  let focus_direct = focus_direct
  let blur_direct = blur_direct
  let live_count t = t.live_count
  let set_on_live_count_change t cb = t.live_count_change <- cb
  let set_lifecycle_pass = set_lifecycle_pass
  let run_lifecycle_pass = run_lifecycle_pass
  let emit_mouse = emit_mouse
  let emit_key = emit_key
  let emit_default_key = emit_default_key
  let emit_paste = emit_paste
  let emit_selection_changed = emit_selection_changed
  let clear_selection = clear_selection
  let should_start_selection = should_start_selection
  let get_selected_text = get_selected_text

  let child_clip t =
    match t.child_clip with
    | Some f -> f t
    | None ->
        let b = bounds t in
        if b.width > 0 && b.height > 0 then Some b else None
end
