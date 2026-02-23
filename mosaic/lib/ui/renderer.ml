type selection_state = {
  selection : Selection.t;
  mutable touched_nodes : Renderable.t list;
  selected_nodes_ref : Renderable.t list ref;
}

type handler_id = Event_dispatcher.handler_id

type t = {
  layout : unit Toffee.tree;
  mutable root : Renderable.t option;
  mutable focused_node : Renderable.t option;
  nodes : (Toffee.Node_id.t, Renderable.t) Hashtbl.t;
  renderables : (int, Renderable.t) Hashtbl.t;
  mutable next_renderable_num : int;
  lifecycle_nodes : (int, Renderable.t) Hashtbl.t;
  event_dispatcher : Event_dispatcher.t;
  screen : Screen.t;
  mutable last_frame : Screen.t option;
  mutable last_hits : Screen.Hit_grid.t option;
  mutable width : int;
  mutable height : int;
  mutable last_layout_width : int;
  mutable last_layout_height : int;
  mutable dirty : bool;
  mutable rendering : bool;
  auto_focus_on_mouse_down : bool;
  mutable current_selection : selection_state option;
  mutable selection_containers : Renderable.t list;
  mutable selection_listeners :
    (anchor_x:int -> anchor_y:int -> focus_x:int -> focus_y:int -> unit) list;
  mutable last_over : Renderable.t option;
  mutable captured : Renderable.t option;
  mutable last_pointer_x : int;
  mutable last_pointer_y : int;
  mutable live_active : bool;
  mutable on_live_change : (bool -> unit) option;
}

type error =
  | Layout_error of Toffee.Error.t
  | Tree_mismatch
  | Root_already_assigned

let set_live_active t now_live =
  if now_live <> t.live_active then (
    t.live_active <- now_live;
    match t.on_live_change with Some cb -> cb now_live | None -> ())

let create ?glyph_pool ?width_method ?respect_alpha ?mouse_enabled
    ?cursor_visible ?auto_focus_on_mouse_down ?explicit_width () =
  let resolved_mouse_enabled = Option.value ~default:true mouse_enabled in
  let resolved_cursor_visible = Option.value ~default:true cursor_visible in
  let resolved_explicit_width = Option.value ~default:false explicit_width in

  let screen =
    Screen.create ?glyph_pool ?width_method ?respect_alpha
      ~mouse_enabled:resolved_mouse_enabled
      ~cursor_visible:resolved_cursor_visible
      ~explicit_width:resolved_explicit_width ()
  in
  {
    layout = Toffee.new_tree ();
    root = None;
    focused_node = None;
    nodes = Hashtbl.create 128;
    renderables = Hashtbl.create 128;
    next_renderable_num = 1;
    lifecycle_nodes = Hashtbl.create 32;
    event_dispatcher = Event_dispatcher.create ();
    screen;
    last_frame = None;
    last_hits = None;
    width = 0;
    height = 0;
    last_layout_width = -1;
    last_layout_height = -1;
    dirty = false;
    rendering = false;
    auto_focus_on_mouse_down =
      Option.value ~default:true auto_focus_on_mouse_down;
    current_selection = None;
    selection_containers = [];
    selection_listeners = [];
    last_over = None;
    captured = None;
    last_pointer_x = 0;
    last_pointer_y = 0;
    live_active = false;
    on_live_change = None;
  }

let alloc_num t =
  let n = t.next_renderable_num in
  t.next_renderable_num <- n + 1;
  n

let gen_id t = Printf.sprintf "_fiber_%d" (alloc_num t)

let register_node t (node : Renderable.t) =
  Hashtbl.replace t.nodes (Renderable.Internal.node_id node) node;
  Hashtbl.replace t.renderables (Renderable.Internal.number node) node

let register_lifecycle_node t (node : Renderable.t) =
  Hashtbl.replace t.lifecycle_nodes (Renderable.Internal.number node) node

let unregister_lifecycle_node t (node : Renderable.t) =
  Hashtbl.remove t.lifecycle_nodes (Renderable.Internal.number node)

let root t = t.root

let clear_root t =
  (match t.root with
  | Some node ->
      Renderable.Internal.set_is_root node false;
      Renderable.Internal.set_on_live_count_change node None
  | None -> ());
  t.root <- None;
  set_live_active t false;
  t.dirty <- true

let request_render t = if not t.rendering then t.dirty <- true

let with_rendering t f =
  let prev = t.rendering in
  t.rendering <- true;
  Fun.protect ~finally:(fun () -> t.rendering <- prev) f

let run_lifecycle_passes t =
  Hashtbl.iter
    (fun _ node -> Renderable.Internal.run_lifecycle_pass node)
    t.lifecycle_nodes

let blur_node t node =
  let was_focused =
    match t.focused_node with Some prev when prev == node -> true | _ -> false
  in
  Renderable.Internal.blur_direct node;
  if was_focused then (
    t.focused_node <- None;
    Event_dispatcher.set_focused_renderable t.event_dispatcher None;
    t.dirty <- true)

let focus_node t node =
  if not (Renderable.focusable node) then false
  else (
    (match t.focused_node with
    | Some prev when prev == node -> ()
    | Some prev -> blur_node t prev
    | None -> ());
    if Renderable.Internal.focus_direct node then (
      t.focused_node <- Some node;
      Event_dispatcher.set_focused_renderable t.event_dispatcher (Some node);
      t.dirty <- true;
      true)
    else false)

let create_node t ?id ?props ?on_frame ?on_size_change ?render () =
  let num = alloc_num t in
  match
    Renderable.Internal.create ~layout:t.layout ?id ?props ?on_frame
      ?on_size_change ?render
      ~glyph_pool:(Screen.glyph_pool t.screen)
      ~num ()
  with
  | Error _ as e -> e
  | Ok node ->
      Renderable.Internal.set_schedule node (fun () -> request_render t);
      Renderable.Internal.set_focus_controller node
        ~focus:(fun n -> focus_node t n)
        ~blur:(fun n -> blur_node t n);
      Renderable.Internal.set_lifecycle_controllers node
        ~register:(fun n -> register_lifecycle_node t n)
        ~unregister:(fun n -> unregister_lifecycle_node t n);
      Renderable.Internal.set_registration_hooks node
        ~register:(fun n -> register_node t n)
        ~alloc_num:(fun () -> alloc_num t);
      Renderable.Internal.register_with_renderer node;
      Ok node

let set_root t node =
  match t.root with
  | Some _ -> Error Root_already_assigned
  | None -> (
      match Toffee.style t.layout (Renderable.Internal.node_id node) with
      | Ok _ ->
          t.root <- Some node;
          Renderable.Internal.set_is_root node true;
          Renderable.Internal.set_on_live_count_change node
            (Some
               (fun n ->
                 let live = Renderable.Internal.live_count n > 0 in
                 set_live_active t live));
          set_live_active t (Renderable.Internal.live_count node > 0);
          (if t.width > 0 && t.height > 0 then
             let open Toffee.Style in
             let size =
               Toffee.Geometry.Size.make
                 (Dimension.length (float t.width))
                 (Dimension.length (float t.height))
             in
             let (_ : (unit, Renderable.error) Stdlib.result) =
               Renderable.set_style node (set_size size (Renderable.style node))
             in
             ());
          t.dirty <- true;
          Ok ()
      | Error _ -> Error Tree_mismatch)

let rec update_layout_cache t parent_abs (node : Renderable.t) =
  match Toffee.layout t.layout (Renderable.Internal.node_id node) with
  | Error _ -> ()
  | Ok layout ->
      let loc = Toffee.Layout.location layout in
      let size = Toffee.Layout.size layout in
      let abs_x = parent_abs.Toffee.Geometry.Point.x +. loc.x in
      let abs_y = parent_abs.y +. loc.y in
      Renderable.Internal.update_cached_layout node ~x:abs_x ~y:abs_y
        ~width:size.width ~height:size.height;
      List.iter
        (update_layout_cache t Toffee.Geometry.Point.{ x = abs_x; y = abs_y })
        (Renderable.children node)

let rec clear_layout_flags (node : Renderable.t) =
  Renderable.Internal.clear_layout_dirty node;
  List.iter clear_layout_flags (Renderable.children node)

let rec any_layout_dirty (node : Renderable.t) : bool =
  if Renderable.Internal.layout_dirty node then true
  else List.exists any_layout_dirty (Renderable.children node)

let compute_and_update_layout t : (unit, error) result =
  match t.root with
  | None -> Ok ()
  | Some root -> (
      let width_changed = t.width <> t.last_layout_width in
      let height_changed = t.height <> t.last_layout_height in
      let needs_layout =
        width_changed || height_changed || any_layout_dirty root
      in
      if not needs_layout then Ok ()
      else
        let w = if t.width > 0 then t.width else 80 in
        let h = if t.height > 0 then t.height else 24 in
        let available_space =
          Toffee.Geometry.Size.
            {
              width = Toffee.Available_space.of_float (float w);
              height = Toffee.Available_space.of_float (float h);
            }
        in
        let measure_fn known_dimensions available_space node_id _ctx style =
          match Hashtbl.find_opt t.nodes node_id with
          | Some n -> (
              match Renderable.Internal.measure_of n with
              | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
              | Some m -> m ~known_dimensions ~available_space ~style)
          | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
        in
        match
          Toffee.compute_layout_with_measure t.layout
            (Renderable.Internal.node_id root)
            available_space measure_fn
        with
        | Ok () ->
            update_layout_cache t Toffee.Geometry.Point.{ x = 0.; y = 0. } root;
            clear_layout_flags root;
            t.last_layout_width <- t.width;
            t.last_layout_height <- t.height;
            Ok ()
        | Error e -> Error (Layout_error e))

let resize t ~width ~height =
  let width = max 0 width and height = max 0 height in
  let size_changed = width <> t.width || height <> t.height in
  t.width <- width;
  t.height <- height;
  Screen.resize t.screen ~width ~height;
  (match t.root with
  | Some root ->
      let open Toffee.Style in
      let size =
        Toffee.Geometry.Size.make
          (Dimension.length (float width))
          (Dimension.length (float height))
      in
      let new_style = set_size size (Renderable.style root) in
      let (_ : (unit, Renderable.error) Stdlib.result) =
        Renderable.set_style root new_style
      in
      ()
  | None -> ());
  t.captured <- None;
  t.last_over <- None;
  t.last_frame <- None;
  t.last_hits <- None;
  if size_changed then (
    t.last_pointer_x <- 0;
    t.last_pointer_y <- 0);
  t.dirty <- true;
  Ok ()

let notify_selection_change t =
  match t.current_selection with
  | None -> ()
  | Some sel_state -> (
      let container_opt =
        match List.rev t.selection_containers with
        | hd :: _ -> Some hd
        | [] -> t.root
      in
      match container_opt with
      | None -> ()
      | Some container ->
          let bounds = Selection.bounds sel_state.selection in
          let new_touched = ref [] in
          let new_selected = ref [] in
          let grid_viewport =
            Grid.
              {
                x = bounds.x;
                y = bounds.y;
                width = bounds.width;
                height = bounds.height;
              }
          in

          let rec walk (n : Renderable.t) =
            let children =
              Renderable.Internal.children_in_viewport ~parent:n
                ~viewport:grid_viewport ~padding:0
            in
            List.iter
              (fun child ->
                walk child;
                if Renderable.selectable child then (
                  new_touched := child :: !new_touched;
                  let has_sel =
                    Renderable.Internal.emit_selection_changed child
                      (Some sel_state.selection)
                  in
                  if has_sel then new_selected := child :: !new_selected))
              children
          in
          walk container;

          let was_touched = sel_state.touched_nodes in
          let is_touched_set = Hashtbl.create 32 in
          List.iter
            (fun n ->
              Hashtbl.replace is_touched_set (Renderable.Internal.number n) ())
            !new_touched;
          List.iter
            (fun n ->
              if not (Hashtbl.mem is_touched_set (Renderable.Internal.number n))
              then
                let _ = Renderable.Internal.emit_selection_changed n None in
                Renderable.Internal.clear_selection_by_node n)
            was_touched;

          sel_state.touched_nodes <- !new_touched;
          sel_state.selected_nodes_ref := !new_selected;
          t.dirty <- true)

let clear_selection t =
  (match t.current_selection with
  | Some sel_state ->
      Selection.set_is_active sel_state.selection false;
      List.iter
        (fun node ->
          ignore (Renderable.Internal.emit_selection_changed node None);
          Renderable.Internal.clear_selection_by_node node)
        sel_state.touched_nodes;
      sel_state.touched_nodes <- [];
      sel_state.selected_nodes_ref := []
  | None -> ());
  t.current_selection <- None;
  t.selection_containers <- [];
  t.dirty <- true

let start_selection t (node : Renderable.t) ~(x : int) ~(y : int) =
  clear_selection t;
  let container =
    match Renderable.parent node with
    | Some p -> p
    | None -> ( match t.root with Some r -> r | None -> node)
  in
  let point = Selection.{ x; y } in
  let anchor_provider =
    let rel_x = x - Renderable.x node in
    let rel_y = y - Renderable.y node in
    fun () ->
      let base_x = Renderable.x node in
      let base_y = Renderable.y node in
      Selection.{ x = base_x + rel_x; y = base_y + rel_y }
  in
  let selected_nodes_ref : Renderable.t list ref = ref [] in
  t.selection_containers <- [ container ];

  let selected_text_fn () =
    let nodes = !selected_nodes_ref in
    let sorted =
      List.sort
        (fun a b ->
          let ay = Renderable.y a and by = Renderable.y b in
          if ay <> by then compare ay by
          else compare (Renderable.x a) (Renderable.x b))
        nodes
    in
    let pieces =
      List.filter_map
        (fun n ->
          let txt = Renderable.Internal.get_selected_text n in
          if txt = "" then None else Some txt)
        sorted
    in
    String.concat "\n" pieces
  in

  let selection =
    Selection.create ~anchor:point ~focus:point ~anchor_provider
      ~get_selected_text:selected_text_fn ()
  in
  t.current_selection <-
    Some { selection; touched_nodes = []; selected_nodes_ref };
  notify_selection_change t

let list_take lst n =
  let rec aux i acc = function
    | _ when i = 0 -> List.rev acc
    | [] -> List.rev acc
    | h :: tl -> aux (i - 1) (h :: acc) tl
  in
  if n <= 0 then [] else aux n [] lst

let rec is_within_container (node : Renderable.t) (container : Renderable.t) :
    bool =
  if node == container then true
  else
    match Renderable.parent node with
    | None -> false
    | Some p -> is_within_container p container

let update_selection t ~(current_node : Renderable.t option) ~(x : int)
    ~(y : int) =
  match t.current_selection with
  | None -> ()
  | Some sel_state ->
      Selection.set_focus sel_state.selection Selection.{ x; y };

      (match t.selection_containers with
      | [] -> ()
      | containers -> (
          let current_container = List.hd (List.rev containers) in
          match current_node with
          | None ->
              let parent =
                match Renderable.parent current_container with
                | Some p -> p
                | None -> (
                    match t.root with Some r -> r | None -> current_container)
              in
              t.selection_containers <- containers @ [ parent ]
          | Some n -> (
              if not (is_within_container n current_container) then
                let parent =
                  match Renderable.parent current_container with
                  | Some p -> p
                  | None -> (
                      match t.root with
                      | Some r -> r
                      | None -> current_container)
                in
                t.selection_containers <- containers @ [ parent ]
              else if List.length containers > 1 then
                let rec index_of needle i = function
                  | [] -> None
                  | h :: tl ->
                      if h == needle then Some i else index_of needle (i + 1) tl
                in
                let idx_opt =
                  match index_of n 0 containers with
                  | Some i -> Some i
                  | None -> (
                      match Renderable.parent n with
                      | None -> None
                      | Some p -> index_of p 0 containers)
                in
                match idx_opt with
                | Some i when i < List.length containers - 1 ->
                    t.selection_containers <- list_take containers (i + 1)
                | _ -> ())));
      notify_selection_change t

let finish_selection t =
  match t.current_selection with
  | None -> ()
  | Some sel_state ->
      Selection.set_is_selecting sel_state.selection false;
      let anchor = Selection.anchor sel_state.selection in
      let focus = Selection.focus sel_state.selection in
      let ax, ay, fx, fy = (anchor.x, anchor.y, focus.x, focus.y) in
      List.iter
        (fun cb -> cb ~anchor_x:ax ~anchor_y:ay ~focus_x:fx ~focus_y:fy)
        t.selection_listeners

let intersect_rect (a : Grid.region) (b : Grid.region) : Grid.region option =
  let x0 = max a.x b.x in
  let y0 = max a.y b.y in
  let x1 = min (a.x + a.width) (b.x + b.width) in
  let y1 = min (a.y + a.height) (b.y + b.height) in
  let w = x1 - x0 and h = y1 - y0 in
  if w <= 0 || h <= 0 then None
  else Some Grid.{ x = x0; y = y0; width = w; height = h }

let overflow_scissor (node : Renderable.t) ~(target_is_buffered : bool) :
    Grid.region option =
  let open Toffee in
  let st = Renderable.style node in
  let ov = Style.overflow st in
  match ov with
  | { Geometry.Point.x = Style.Overflow.Visible; y = Style.Overflow.Visible } ->
      None
  | _ ->
      let w = Renderable.width node and h = Renderable.height node in
      if w <= 0 || h <= 0 then None
      else if target_is_buffered then
        Some Grid.{ x = 0; y = 0; width = w; height = h }
      else
        let x = Renderable.x node and y = Renderable.y node in
        Some Grid.{ x; y; width = w; height = h }

let rec render_node_shared (captured_num : int option) (node : Renderable.t)
    (grid : Grid.t) (hits : Screen.Hit_grid.t) ~delta : unit =
  if Renderable.visible node then (
    (* Capture layout values BEFORE pre_render_update, as callbacks like
       on_size_change can mark layout dirty and invalidate the cache. *)
    let w = Renderable.width node
    and h = Renderable.height node in
    let hx = Renderable.x node and hy = Renderable.y node in
    Renderable.Internal.pre_render_update node ~delta;
    if w > 0 && h > 0 then (
      let nid = Renderable.Internal.number node in

      let self_target, using_buffer =
        match Renderable.buffer node with
        | `Self ->
            ( Option.value
                (Renderable.Internal.ensure_frame_buffer node ~parent:grid)
                ~default:grid,
              true )
        | `None -> (grid, false)
      in

      let overflow =
        overflow_scissor node
          ~target_is_buffered:(Renderable.buffer node = `Self)
      in
      let child_clip = Renderable.Internal.child_clip_rect node in
      let clip =
        match (overflow, child_clip) with
        | None, None -> None
        | Some r, None -> Some r
        | None, Some r -> Some r
        | Some a, Some b -> intersect_rect a b
      in

      (match Renderable.Internal.render_before_hook node with
      | Some hook -> hook node self_target ~delta
      | None -> ());
      Renderable.Internal.render node self_target ~delta;
      (match Renderable.Internal.render_after_hook node with
      | Some hook -> hook node self_target ~delta
      | None -> ());

      (match captured_num with
      | Some cn when cn = nid -> ()
      | _ -> Screen.Hit_grid.add hits ~x:hx ~y:hy ~width:w ~height:h ~id:nid);

      if using_buffer then Renderable.Internal.blit_frame_buffer node ~dst:grid;

      let child_visibility =
        match Renderable.Internal.visible_children node with
        | `All -> `All
        | `Subset ids ->
            let tbl = Hashtbl.create (List.length ids + 1) in
            List.iter (fun id -> Hashtbl.replace tbl id ()) ids;
            `Subset tbl
      in
      let render_children () =
        Renderable.Internal.iter_sorted_children node (fun child ->
            let should_render =
              match child_visibility with
              | `All -> true
              | `Subset tbl ->
                  Hashtbl.mem tbl (Renderable.Internal.number child)
            in
            if should_render then
              render_node_shared captured_num child grid hits ~delta)
      in
      match clip with
      | None -> render_children ()
      | Some rect -> Grid.clip grid rect render_children))

let clamp v ~min ~max = if v < min then min else if v > max then max else v

let resolve_hardware_cursor t =
  match t.focused_node with
  | None -> None
  | Some node when not (Renderable.visible node) -> None
  | Some node -> Renderable.Internal.hardware_cursor node

let apply_cursor_to_screen t ~width ~height cursor =
  match cursor with
  | None -> Screen.set_cursor_visible t.screen false
  | Some Renderable.{ x; y; color; style; blinking } ->
      let col = clamp x ~min:1 ~max:(max 1 width) in
      let row = clamp y ~min:1 ~max:(max 1 height) in
      let r, g, b, _ = Ansi.Color.to_rgba color in
      Screen.set_cursor_style t.screen ~style ~blinking;
      Screen.set_cursor_color t.screen ~r ~g ~b;
      Screen.set_cursor_position t.screen ~row ~col;
      Screen.set_cursor_visible t.screen true

let render_frame t ~delta =
  with_rendering t (fun () ->
      run_lifecycle_passes t;
      (match t.root with
      | None -> ()
      | Some _ -> ignore (compute_and_update_layout t));
      (match t.focused_node with
      | Some n when not (Renderable.visible n) -> blur_node t n
      | _ -> ());

      let bw = if t.width > 0 then t.width else 80 in
      let bh = if t.height > 0 then t.height else 24 in
      let captured_num = Option.map Renderable.Internal.number t.captured in
      let cursor = resolve_hardware_cursor t in
      apply_cursor_to_screen t ~width:bw ~height:bh cursor;

      let render_once () =
        Screen.build t.screen ~width:bw ~height:bh (fun grid hits ->
            match t.root with
            | None -> ()
            | Some root -> render_node_shared captured_num root grid hits ~delta)
      in

      t.dirty <- false;
      let frame = render_once () in

      t.last_hits <- Some (Screen.hit_grid frame);
      t.last_frame <- Some frame;

      (match t.current_selection with
      | Some sel_state when Selection.is_active sel_state.selection ->
          notify_selection_change t
      | _ -> ());

      Screen.render frame)

let snapshot_frame t ~delta =
  with_rendering t (fun () ->
      run_lifecycle_passes t;
      (match t.root with
      | None -> ()
      | Some _ -> ignore (compute_and_update_layout t));
      (match t.focused_node with
      | Some n when not (Renderable.visible n) -> blur_node t n
      | _ -> ());

      let bw = if t.width > 0 then t.width else 80 in
      let bh = if t.height > 0 then t.height else 24 in
      let captured_num = Option.map Renderable.Internal.number t.captured in
      let cursor = resolve_hardware_cursor t in
      apply_cursor_to_screen t ~width:bw ~height:bh cursor;

      let build () =
        Screen.build t.screen ~width:bw ~height:bh (fun grid hits ->
            match t.root with
            | None -> ()
            | Some root -> render_node_shared captured_num root grid hits ~delta)
      in

      t.dirty <- false;
      let frame = build () in
      t.last_hits <- Some (Screen.hit_grid frame);
      t.last_frame <- Some frame;
      Grid.to_ansi (Screen.grid frame))

let render_into t grid hits ~delta =
  with_rendering t (fun () ->
      run_lifecycle_passes t;
      (match t.root with
      | None -> ()
      | Some _ -> ignore (compute_and_update_layout t));
      (match t.focused_node with
      | Some n when not (Renderable.visible n) -> blur_node t n
      | _ -> ());
      let captured_num = Option.map Renderable.Internal.number t.captured in
      (match t.root with
      | None -> ()
      | Some root -> render_node_shared captured_num root grid hits ~delta);
      t.last_hits <- Some hits;
      let cursor =
        match resolve_hardware_cursor t with
        | None -> None
        | Some Renderable.{ x; y; color; style; blinking } ->
            let gw = Grid.width grid in
            let gh = Grid.height grid in
            if gw <= 0 || gh <= 0 then None
            else
              Some
                {
                  Renderable.x = clamp x ~min:1 ~max:(max 1 gw);
                  y = clamp y ~min:1 ~max:(max 1 gh);
                  color;
                  style;
                  blinking;
                }
      in
      (match t.current_selection with
      | Some sel_state when Selection.is_active sel_state.selection ->
          notify_selection_change t
      | _ -> ());
      cursor)

let query_hit t ~x ~y =
  match t.last_frame with
  | Some frame -> Screen.query_hit frame ~x ~y
  | None -> (
      match t.last_hits with
      | Some hits -> Screen.Hit_grid.get hits ~x ~y
      | None -> 0)

let set_target_metadata event node_opt =
  match node_opt with
  | None -> Event.Mouse.Internal.set_target event ~id:None ~number:None
  | Some node ->
      Event.Mouse.Internal.set_target event
        ~id:(Some (Renderable.id node))
        ~number:(Some (Renderable.Internal.number node))

let set_source_metadata event node_opt =
  match node_opt with
  | None -> Event.Mouse.Internal.set_source event ~id:None ~number:None
  | Some node ->
      Event.Mouse.Internal.set_source event
        ~id:(Some (Renderable.id node))
        ~number:(Some (Renderable.Internal.number node))

let dispatch_event node evt =
  set_target_metadata evt (Some node);
  Renderable.Internal.emit_mouse_event node evt

let handle_mouse_focus t target is_left =
  if t.auto_focus_on_mouse_down && is_left then
    match target with
    | Some node when Renderable.focusable node -> ignore (focus_node t node)
    | _ -> ()

let handle_hover_state t target x y modifiers =
  let same_target =
    match (t.last_over, target) with
    | Some a, Some b -> a == b
    | None, None -> true
    | _ -> false
  in
  if not same_target then (
    (match t.last_over with
    | Some prev when match t.captured with Some c -> c != prev | None -> true ->
        let ev = Event.Mouse.out ~x ~y ~modifiers in
        set_target_metadata ev (Some prev);
        Renderable.Internal.emit_mouse_event prev ev
    | _ -> ());
    t.last_over <- target;
    match target with
    | Some node ->
        let over_event = Event.Mouse.over ~x ~y ~modifiers in
        set_target_metadata over_event (Some node);
        set_source_metadata over_event t.captured;
        Renderable.Internal.emit_mouse_event node over_event
    | None -> ())

let handle_mouse t (event : Event.mouse) =
  let x = Event.Mouse.x event and y = Event.Mouse.y event in
  t.last_pointer_x <- x;
  t.last_pointer_y <- y;

  let id = query_hit t ~x ~y in
  let target = Hashtbl.find_opt t.renderables id in
  let is_left =
    match Event.Mouse.button event with
    | Some Input.Mouse.Left -> true
    | _ -> false
  in
  let modifiers = Event.Mouse.modifiers event in

  let selection_is_selecting () =
    match t.current_selection with
    | Some sel -> Selection.is_selecting sel.selection
    | None -> false
  in

  let is_up =
    match Event.Mouse.kind event with Up | Drag_end -> true | _ -> false
  in

  match Event.Mouse.kind event with
  | Scroll -> (
      match target with
      | Some n -> dispatch_event n event
      | None -> set_target_metadata event None)
  | _ -> (
      try
        (match Event.Mouse.kind event with
        | Down -> handle_mouse_focus t target is_left
        | _ -> ());

        (match Event.Mouse.kind event with
        | Down
          when is_left
               && (not (selection_is_selecting ()))
               && not modifiers.ctrl -> (
            match target with
            | Some node
              when Renderable.selectable node
                   && Renderable.Internal.should_start_selection node ~x ~y ->
                start_selection t node ~x ~y;
                dispatch_event node event;
                raise Exit
            | _ -> ())
        | _ -> ());

        (match Event.Mouse.kind event with
        | Drag when selection_is_selecting () ->
            update_selection t ~current_node:target ~x ~y;
            (match target with
            | Some node ->
                Event.Mouse.Internal.set_is_selecting event true;
                dispatch_event node event
            | None -> ());
            raise Exit
        | _ -> ());

        (match Event.Mouse.kind event with
        | Up when selection_is_selecting () ->
            (match target with
            | Some node ->
                Event.Mouse.Internal.set_is_selecting event true;
                dispatch_event node event
            | None -> ());
            finish_selection t;
            raise Exit
        | _ -> ());

        (match Event.Mouse.kind event with
        | Down when is_left && modifiers.ctrl -> (
            match t.current_selection with
            | Some sel ->
                Selection.set_is_selecting sel.selection true;
                update_selection t ~current_node:target ~x ~y;
                raise Exit
            | None -> ())
        | _ -> ());

        (match Event.Mouse.kind event with
        | Move | Drag -> handle_hover_state t target x y modifiers
        | _ -> ());

        (match t.captured with
        | Some c when not is_up ->
            dispatch_event c event;
            raise Exit
        | Some c ->
            let button =
              match Event.Mouse.button event with
              | Some b -> b
              | None -> Input.Mouse.Left
            in
            let drag_end = Event.Mouse.drag_end ~x ~y ~button ~modifiers in
            set_target_metadata drag_end (Some c);
            Renderable.Internal.emit_mouse_event c drag_end;
            dispatch_event c event;
            (match target with
            | Some n ->
                let drop = Event.Mouse.drop ~x ~y ~button ~modifiers in
                set_target_metadata drop (Some n);
                set_source_metadata drop (Some c);
                Renderable.Internal.emit_mouse_event n drop
            | None -> ());
            t.last_over <- Some c;
            t.captured <- None;
            request_render t;
            raise Exit
        | None -> ());

        (match Event.Mouse.kind event with
        | Drag -> (
            match Event.Mouse.button event with
            | Some Input.Mouse.Left -> t.captured <- target
            | _ -> t.captured <- None)
        | _ -> ());

        (match target with
        | Some n -> dispatch_event n event
        | None ->
            t.captured <- None;
            t.last_over <- None;
            set_target_metadata event None);

        match Event.Mouse.kind event with
        | Down when Option.is_some t.current_selection ->
            if not (Event.Mouse.default_prevented event) then clear_selection t
        | _ -> ()
      with Exit -> ())

let add_global_key_handler t handler =
  Event_dispatcher.add_global_key_handler t.event_dispatcher handler

let remove_global_key_handler t handler_id =
  Event_dispatcher.remove_global_key_handler t.event_dispatcher handler_id

let add_global_paste_handler t handler =
  Event_dispatcher.add_global_paste_handler t.event_dispatcher handler

let remove_global_paste_handler t handler_id =
  Event_dispatcher.remove_global_paste_handler t.event_dispatcher handler_id

let handle_key t (key : Event.key) =
  Event_dispatcher.dispatch_key t.event_dispatcher key

let handle_paste t (event : Event.paste) =
  Event_dispatcher.dispatch_paste t.event_dispatcher event

let focused_node t = t.focused_node

let blur_focused t =
  match t.focused_node with None -> () | Some n -> blur_node t n

let focus t (node : Renderable.t) = ignore (focus_node t node)
let find_by_number t n = Hashtbl.find_opt t.renderables n

let find_by_id t id =
  Hashtbl.to_seq_values t.renderables
  |> Seq.find (fun node -> String.equal (Renderable.id node) id)

let on_selection t cb = t.selection_listeners <- cb :: t.selection_listeners
let has_selection t = Option.is_some t.current_selection

let get_selection t : (int * int * int * int) option =
  match t.current_selection with
  | None -> None
  | Some sel_state ->
      let anchor = Selection.anchor sel_state.selection in
      let focus = Selection.focus sel_state.selection in
      Some (anchor.x, anchor.y, focus.x, focus.y)

let get_selected_text t : string =
  match t.current_selection with
  | None -> ""
  | Some sel_state -> Selection.get_selected_text sel_state.selection

let has_live_requests t =
  match t.root with
  | None -> false
  | Some r -> Renderable.Internal.live_count r > 0

let request_selection_update t =
  match t.current_selection with
  | Some sel_state when Selection.is_selecting sel_state.selection ->
      let x = t.last_pointer_x and y = t.last_pointer_y in
      let id = query_hit t ~x ~y in
      let current = Hashtbl.find_opt t.renderables id in
      update_selection t ~current_node:current ~x ~y
  | _ -> ()

let on_live_change t f = t.on_live_change <- Some f
let needs_render t = t.dirty
