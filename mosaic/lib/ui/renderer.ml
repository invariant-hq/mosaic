(* ───── Render Commands ───── *)

(* A flat array of commands built by depth-first tree traversal, executed
   sequentially. This separates layout extraction (pass 2) from drawing (pass
   3), so traversal and drawing stay decoupled. *)
type render_command =
  | Render of Renderable.t
  | Push_scissor of Grid.region
  | Pop_scissor
  | Push_opacity of float
  | Pop_opacity

(* ───── Types ───── *)

type t = {
  screen : Screen.t;
  root : Renderable.t;
  tree : unit Toffee.tree;
  glyph_pool : Glyph.Pool.t;
  (* Node registry: maps numeric IDs to renderables for hit testing. *)
  node_map : (int, Renderable.t) Hashtbl.t;
  (* Reverse index: maps Toffee.Node_id index to renderable for O(1) measure
     lookups. *)
  toffee_map : (int, Renderable.t) Hashtbl.t;
  (* Focus — shared ref between record and context closures (single source of
     truth). *)
  focused : Renderable.t option ref;
  (* Lifecycle *)
  lifecycle_set : (int, Renderable.t) Hashtbl.t;
  (* Dirty tracking — shared ref, same as focused. *)
  dirty : bool ref;
  (* Render command buffer — reused across frames. *)
  mutable commands : render_command array;
  mutable cmd_len : int;
  (* Hit scissor stack — maintained during pass 3 for clipping hit regions. *)
  mutable hit_scissors : Grid.region list;
  (* Hover tracking *)
  mutable hover_node : Renderable.t option;
  mutable hover_num : int;
  mutable pointer : (int * int) option;
  mutable pointer_modifiers : Input.Modifier.t;
  (* Drag capture *)
  mutable captured : Renderable.t option;
  (* Selection *)
  mutable selection : Selection.t option;
  mutable selection_containers : Renderable.t list;
  mutable touched_selectables : Renderable.t list;
  (* Frame callbacks *)
  mutable frame_callbacks : (float -> unit) list;
}

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

(* ───── Focus ───── *)

let blur_current t =
  match !(t.focused) with
  | None -> ()
  | Some node ->
      Renderable.Private.blur_direct node;
      t.focused := None

let focus_node t node =
  if not (Renderable.focusable node) then false
  else (
    if match !(t.focused) with Some f -> not (f == node) | None -> true then (
      blur_current t;
      ignore (Renderable.Private.focus_direct node : bool);
      t.focused := Some node);
    true)

(* ───── Selection Helpers ───── *)

let rec iter_selectables node f =
  if Renderable.selectable node then f node;
  Renderable.Private.iter_children_z node (fun child ->
      iter_selectables child f)

let rec is_within_container node container =
  if node == container then true
  else
    match Renderable.parent node with
    | Some p -> is_within_container p container
    | None -> false

let list_phys_index x lst =
  let rec go i = function
    | [] -> -1
    | hd :: _ when hd == x -> i
    | _ :: tl -> go (i + 1) tl
  in
  go 0 lst

let list_take n lst =
  let rec go acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | hd :: tl -> go (hd :: acc) (n - 1) tl
  in
  go [] n lst

let notify_selectables t sel =
  match t.selection_containers with
  | [] -> ()
  | container :: _ ->
      let new_touched = ref [] in
      iter_selectables container (fun node ->
          ignore (Renderable.Private.emit_selection_changed node sel : bool);
          new_touched := node :: !new_touched);
      List.iter
        (fun prev ->
          if
            (not (List.exists (fun n -> n == prev) !new_touched))
            && not (Renderable.destroyed prev)
          then
            ignore (Renderable.Private.emit_selection_changed prev None : bool))
        t.touched_selectables;
      t.touched_selectables <- !new_touched

let clear_active_selection t =
  match t.selection with
  | None -> ()
  | Some _ ->
      List.iter
        (fun node ->
          if not (Renderable.destroyed node) then (
            ignore (Renderable.Private.emit_selection_changed node None : bool);
            Renderable.Private.clear_selection node))
        t.touched_selectables;
      t.selection <- None;
      t.selection_containers <- [];
      t.touched_selectables <- []

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
        Renderable.Private.emit_mouse old ev
    | None -> ());
    t.hover_node <- target_node;
    t.hover_num <- target_num;
    (* Fire Over on new hover target *)
    match target_node with
    | Some node ->
        let ev =
          Event.Mouse.make ~x ~y ~modifiers (Event.Mouse.Over { source = None })
        in
        Renderable.Private.emit_mouse node ev
    | None -> ())

let recheck_hover t =
  if Option.is_some t.captured then ()
  else
    match t.pointer with
    | None -> ()
    | Some (x, y) ->
        let hit_num = Screen.query_hit t.screen ~x ~y in
        let target_node = if hit_num > 0 then find_node t hit_num else None in
        let target_num =
          match target_node with
          | Some n -> Renderable.Private.num n
          | None -> 0
        in
        if target_num <> t.hover_num then (
          let modifiers = t.pointer_modifiers in
          (match t.hover_node with
          | Some old ->
              let ev = Event.Mouse.make ~x ~y ~modifiers Event.Mouse.Out in
              Renderable.Private.emit_mouse old ev
          | None -> ());
          t.hover_node <- target_node;
          t.hover_num <- target_num;
          match target_node with
          | Some node ->
              let ev =
                Event.Mouse.make ~x ~y ~modifiers
                  (Event.Mouse.Over { source = None })
              in
              Renderable.Private.emit_mouse node ev
          | None -> ())

(* ───── Selection Event Handling ───── *)

(* Returns [true] if the selection state machine consumed the event. *)
let handle_selection t ~x ~y ~(modifiers : Input.Modifier.t) ~target_node
    ~target_id kind =
  match kind with
  | Event.Mouse.Down { button = Left } when not modifiers.ctrl -> (
      match target_node with
      | Some node
        when Renderable.selectable node
             && Renderable.Private.should_start_selection node ~x ~y ->
          clear_active_selection t;
          let p = Selection.{ x; y } in
          let sel = Selection.create ~anchor:p ~focus:p () in
          t.selection <- Some sel;
          t.selection_containers <-
            [
              (match Renderable.parent node with Some p -> p | None -> t.root);
            ];
          t.touched_selectables <- [];
          notify_selectables t (Some sel);
          let ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:target_id
              (Event.Mouse.Down { button = Left })
          in
          Renderable.Private.emit_mouse node ev;
          true
      | _ -> false)
  | Event.Mouse.Drag { button = Left; _ } -> (
      match t.selection with
      | Some sel when Selection.is_dragging sel ->
          Selection.set_focus sel { x; y };
          Selection.set_is_start sel false;
          (* Dynamic container expansion/contraction *)
          (match t.selection_containers with
          | [] -> ()
          | current_container :: _ as containers -> (
              match target_node with
              | None ->
                  let parent_container =
                    match Renderable.parent current_container with
                    | Some p -> p
                    | None -> t.root
                  in
                  if not (parent_container == current_container) then
                    t.selection_containers <- parent_container :: containers
              | Some node ->
                  if not (is_within_container node current_container) then (
                    let parent_container =
                      match Renderable.parent current_container with
                      | Some p -> p
                      | None -> t.root
                    in
                    if not (parent_container == current_container) then
                      t.selection_containers <- parent_container :: containers)
                  else if List.length containers > 1 then
                    let idx = list_phys_index node t.selection_containers in
                    let idx =
                      if idx >= 0 then idx
                      else
                        let p =
                          match Renderable.parent node with
                          | Some p -> p
                          | None -> t.root
                        in
                        list_phys_index p t.selection_containers
                    in
                    if idx >= 0 then
                      t.selection_containers <-
                        list_take (idx + 1) t.selection_containers));
          notify_selectables t (Some sel);
          let ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:target_id
              (Event.Mouse.Drag { button = Left; is_dragging = true })
          in
          (match target_node with
          | Some node -> Renderable.Private.emit_mouse node ev
          | None -> ());
          true
      | _ -> false)
  | Event.Mouse.Up { button = Left; _ } -> (
      match t.selection with
      | Some sel when Selection.is_dragging sel ->
          let ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:target_id
              (Event.Mouse.Up { button = Left; is_dragging = true })
          in
          (match target_node with
          | Some node -> Renderable.Private.emit_mouse node ev
          | None -> ());
          Selection.set_is_dragging sel false;
          notify_selectables t (Some sel);
          true
      | _ -> false)
  | _ -> false

(* ───── Drag Capture Handling ───── *)

(* Returns [true] if drag capture consumed the event. *)
let handle_drag_capture t ~x ~y ~modifiers ~target_node ~target_id kind =
  match t.captured with
  | None -> false
  | Some captured_node -> (
      let captured_id = Some (Renderable.Private.num captured_node) in
      match kind with
      | Event.Mouse.Up { button; _ } ->
          let drag_end_ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:captured_id
              (Event.Mouse.Drag_end { button })
          in
          Renderable.Private.emit_mouse captured_node drag_end_ev;
          let up_ev =
            Event.Mouse.make ~x ~y ~modifiers ?target:captured_id
              (Event.Mouse.Up { button; is_dragging = false })
          in
          Renderable.Private.emit_mouse captured_node up_ev;
          (match target_node with
          | Some node ->
              let drop_ev =
                Event.Mouse.make ~x ~y ~modifiers ?target:target_id
                  (Event.Mouse.Drop { button; source = captured_id })
              in
              Renderable.Private.emit_mouse node drop_ev
          | None -> ());
          t.captured <- None;
          t.dirty := true;
          true
      | _ ->
          let ev = Event.Mouse.make ~x ~y ~modifiers ?target:captured_id kind in
          Renderable.Private.emit_mouse captured_node ev;
          true)

(* ───── Mouse Dispatch Pipeline ───── *)

let map_button = function
  | Input.Mouse.Left -> Event.Mouse.Left
  | Input.Mouse.Right -> Event.Mouse.Right
  | Input.Mouse.Middle -> Event.Mouse.Middle
  | Input.Mouse.Button n -> Event.Mouse.Button n
  | Input.Mouse.Wheel_up | Input.Mouse.Wheel_down | Input.Mouse.Wheel_left
  | Input.Mouse.Wheel_right ->
      Event.Mouse.Left

(* Full mouse dispatch pipeline in a fixed order: 1. Update pointer →
   2. Hit test → 3. Selection → 4. Hover → 5. Drag capture → 6. Normal dispatch
   → 7. Selection clear *)
let dispatch_mouse_internal t ~x ~y ~modifiers kind =
  t.pointer <- Some (x, y);
  t.pointer_modifiers <- modifiers;
  let hit_num = Screen.query_hit t.screen ~x ~y in
  let target_node = if hit_num > 0 then find_node t hit_num else None in
  let target_num =
    Option.fold ~none:0 ~some:Renderable.Private.num target_node
  in
  let target_id = if target_num > 0 then Some target_num else None in
  (* Selection state machine *)
  if handle_selection t ~x ~y ~modifiers ~target_node ~target_id kind then ()
  else (
    (* Hover tracking *)
    update_hover t ~x ~y ~modifiers ~target_node ~target_num kind;
    (* Drag capture *)
    if handle_drag_capture t ~x ~y ~modifiers ~target_node ~target_id kind then
      ()
    else begin
      (* Set up new capture on left drag *)
      (match kind with
      | Event.Mouse.Drag { button = Left; _ } -> (
          match target_node with
          | Some node -> t.captured <- Some node
          | None -> ())
      | _ -> ());
      (* Normal dispatch *)
      let ev = Event.Mouse.make ~x ~y ~modifiers ?target:target_id kind in
      (match target_node with
      | Some node -> Renderable.Private.emit_mouse node ev
      | None -> ());
      (* Auto-focus on left click *)
      (match kind with
      | Event.Mouse.Down { button = Left } -> (
          match target_node with
          | Some node -> (
              match find_focusable node with
              | Some focusable -> ignore (focus_node t focusable : bool)
              | None -> ())
          | None -> ())
      | _ -> ());
      (* Clear selection on left click if not prevented *)
      match kind with
      | Event.Mouse.Down { button = Left } when Option.is_some t.selection ->
          if not (Event.Mouse.default_prevented ev) then
            clear_active_selection t
      | _ -> ()
    end)

(* ───── Creation ───── *)

let create ?glyph_pool ?width_method ?style () =
  let tree = Toffee.new_tree () in
  let screen = Screen.create ?glyph_pool ?width_method ~respect_alpha:true () in
  let pool =
    match glyph_pool with Some p -> p | None -> Screen.glyph_pool screen
  in
  let node_map = Hashtbl.create 256 in
  let toffee_map = Hashtbl.create 256 in
  let lifecycle_set = Hashtbl.create 16 in
  let next_num = ref 0 in
  let dirty = ref true in
  let focused = ref None in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule = (fun () -> dirty := true);
      focus =
        (fun node ->
          if not (Renderable.focusable node) then false
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
      register_lifecycle =
        (fun node ->
          Hashtbl.replace lifecycle_set (Renderable.Private.num node) node);
      unregister_lifecycle =
        (fun node -> Hashtbl.remove lifecycle_set (Renderable.Private.num node));
      alloc_num =
        (fun () ->
          let n = !next_num in
          incr next_num;
          n);
      register =
        (fun node ->
          Hashtbl.replace node_map (Renderable.Private.num node) node;
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.replace toffee_map toffee_idx node);
      unregister =
        (fun node ->
          Hashtbl.remove node_map (Renderable.Private.num node);
          let toffee_idx =
            Toffee.Node_id.index (Renderable.Private.toffee_node node)
          in
          Hashtbl.remove toffee_map toffee_idx);
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
    Renderable.Private.create_root ctx ~style:root_style ~glyph_pool:pool
      ~id:"root" ()
  in
  Renderable.Private.set_is_root root true;
  Hashtbl.replace node_map (Renderable.Private.num root) root;
  Hashtbl.replace toffee_map
    (Toffee.Node_id.index (Renderable.Private.toffee_node root))
    root;
  {
    screen;
    root;
    tree;
    glyph_pool = pool;
    node_map;
    toffee_map;
    dirty;
    focused;
    lifecycle_set;
    commands = Array.make 64 Pop_scissor;
    cmd_len = 0;
    hit_scissors = [];
    hover_node = None;
    hover_num = 0;
    pointer = None;
    pointer_modifiers = Input.Modifier.none;
    captured = None;
    selection = None;
    selection_containers = [];
    touched_selectables = [];
    frame_callbacks = [];
  }

(* ───── Accessors ───── *)

let root t = t.root
let screen t = t.screen
let glyph_pool t = t.glyph_pool
let focused t = !(t.focused)
let blur t = blur_current t
let focus t node = focus_node t node
let selection t = t.selection
let captured t = t.captured
let hover t = t.hover_node

(* ───── Measure Function ───── *)

(* O(1) lookup via toffee_map reverse index. *)
let measure_fn (t : t) (known_dimensions : float option Toffee.Geometry.Size.t)
    (available_space : Toffee.Available_space.t Toffee.Geometry.Size.t)
    (node_id : Toffee.Node_id.t) (_context : unit option)
    (style : Toffee.Style.t) : float Toffee.Geometry.Size.t =
  match Hashtbl.find_opt t.toffee_map (Toffee.Node_id.index node_id) with
  | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
  | Some node -> (
      match Renderable.Private.measure node with
      | None -> Toffee.Geometry.Size.{ width = 0.; height = 0. }
      | Some m -> m ~known_dimensions ~available_space ~style)

(* ───── Pass 2: Layout Extraction + Command Generation ───── *)

(* Depth-first walk from a node, computing absolute positions and emitting
   render commands. [parent_x] and [parent_y] are the absolute position of the
   parent's content box origin. *)
let rec build_commands (t : t) (node : Renderable.t) ~parent_x ~parent_y
    ~(delta : float) : unit =
  if not (Renderable.visible node) then ()
  else
    let toffee_node = Renderable.Private.toffee_node node in
    let layout = toffee_exn (Toffee.layout t.tree toffee_node) in
    (* Absolute position = parent content box origin + node's relative location
       (which is relative to parent's content box). *)
    let abs_x = parent_x +. layout.location.x in
    let abs_y = parent_y +. layout.location.y in
    let abs_w = layout.size.width in
    let abs_h = layout.size.height in
    Renderable.Private.update_layout node ~x:abs_x ~y:abs_y ~width:abs_w
      ~height:abs_h;
    Renderable.Private.pre_render_update node ~delta;
    (* Opacity *)
    let opacity = Renderable.opacity node in
    let has_opacity = opacity < 1.0 in
    if has_opacity then emit_cmd t (Push_opacity opacity);
    (* Emit render command for this node *)
    emit_cmd t (Render node);
    (* Child clipping — only applied when overflow is not Visible, following
       CSS-style overflow semantics where "visible" means no clipping. *)
    let overflow = Toffee.Style.overflow (Renderable.style node) in
    let should_clip =
      overflow.x <> Toffee.Style.Overflow.Visible
      || overflow.y <> Toffee.Style.Overflow.Visible
    in
    let child_clip =
      if should_clip then Renderable.Private.child_clip node else None
    in
    let has_clip = Option.is_some child_clip in
    (match child_clip with
    | Some clip -> emit_cmd t (Push_scissor clip)
    | None -> ());
    (* Recurse into children in z-index order. Toffee's layout.location already
       positions children relative to the parent's border box (i.e. it includes
       border+padding offsets), so we pass abs_x/abs_y directly — no additional
       inset needed. *)
    Renderable.Private.iter_children_z node (fun child ->
        build_commands t child ~parent_x:abs_x ~parent_y:abs_y ~delta);
    if has_clip then emit_cmd t Pop_scissor;
    if has_opacity then emit_cmd t Pop_opacity

(* ───── Pass 3: Execute Commands ───── *)

let execute_commands (t : t) ~(grid : Grid.t) ~(hits : Screen.Hit_grid.t)
    ~(delta : float) : unit =
  for i = 0 to t.cmd_len - 1 do
    match t.commands.(i) with
    | Render node ->
        Renderable.Private.render_full node ~grid ~delta;
        (* Add to hit grid, clipped by the hit scissor stack *)
        let bounds = Renderable.bounds node in
        let clipped =
          List.fold_left clip_rect_intersect bounds t.hit_scissors
        in
        if clipped.width > 0 && clipped.height > 0 then
          Screen.Hit_grid.add hits ~x:clipped.x ~y:clipped.y
            ~width:clipped.width ~height:clipped.height
            ~id:(Renderable.Private.num node)
    | Push_scissor clip ->
        Grid.push_clip grid clip;
        t.hit_scissors <- clip :: t.hit_scissors
    | Pop_scissor -> (
        Grid.pop_clip grid;
        match t.hit_scissors with
        | _ :: rest -> t.hit_scissors <- rest
        | [] -> ())
    | Push_opacity opacity -> Grid.push_opacity grid opacity
    | Pop_opacity -> Grid.pop_opacity grid
  done

(* ───── Render Frame ───── *)

let render_frame (t : t) ~width ~height ~delta =
  (* Pass 0: Lifecycle passes *)
  Hashtbl.iter
    (fun _num node -> Renderable.Private.run_lifecycle_pass node)
    t.lifecycle_set;
  (* Frame callbacks *)
  List.iter (fun f -> f delta) t.frame_callbacks;
  (* Build the frame via Screen.build *)
  Screen.build t.screen ~width ~height (fun grid hits ->
      (* Pass 1: Layout computation *)
      let available_space =
        Toffee.Geometry.Size.
          {
            width = Toffee.Available_space.Definite (Float.of_int width);
            height = Toffee.Available_space.Definite (Float.of_int height);
          }
      in
      let root_toffee = Renderable.Private.toffee_node t.root in
      toffee_exn
        (Toffee.compute_layout_with_measure t.tree root_toffee available_space
           (measure_fn t));
      (* Pass 2: Build render command list *)
      t.cmd_len <- 0;
      t.hit_scissors <- [];
      build_commands t t.root ~parent_x:0. ~parent_y:0. ~delta;
      (* Pass 3: Execute render commands *)
      execute_commands t ~grid ~hits ~delta);
  (* Update cursor from focused node *)
  (match !(t.focused) with
  | Some node when Renderable.focused node -> (
      match Renderable.cursor node with
      | Some c ->
          let r, g, b = Ansi.Color.to_rgb c.color in
          let cursor = Screen.cursor t.screen in
          Screen.set_cursor t.screen
            {
              cursor with
              position = Some (c.x, c.y);
              style = c.style;
              blinking = c.blinking;
              color = Some (r, g, b);
            }
      | None ->
          let cursor = Screen.cursor t.screen in
          Screen.set_cursor t.screen { cursor with position = None })
  | _ ->
      let cursor = Screen.cursor t.screen in
      Screen.set_cursor t.screen { cursor with position = None });
  t.dirty := false

let render ?full t =
  let output = Screen.render ?full t.screen in
  recheck_hover t;
  output

let needs_render t = !(t.dirty) || Renderable.Private.live_count t.root > 0

(* ───── Event Dispatch ───── *)

let dispatch_key t (key : Input.Key.event) =
  let ev = Event.Key.of_input key in
  (match !(t.focused) with
  | None -> ()
  | Some node ->
      Renderable.Private.emit_key node ev;
      if not (Event.Key.default_prevented ev) then
        Renderable.Private.emit_default_key node ev);
  ev

let dispatch_mouse t (mouse : Input.Mouse.event) =
  match mouse with
  | Input.Mouse.Button_press (x, y, button, modifiers) ->
      dispatch_mouse_internal t ~x ~y ~modifiers
        (Event.Mouse.Down { button = map_button button })
  | Input.Mouse.Button_release (x, y, button, modifiers) ->
      dispatch_mouse_internal t ~x ~y ~modifiers
        (Event.Mouse.Up { button = map_button button; is_dragging = false })
  | Input.Mouse.Motion (x, y, button_state, _modifiers) ->
      let no_mod = Input.Modifier.none in
      if button_state.left || button_state.middle || button_state.right then
        let button =
          if button_state.left then Event.Mouse.Left
          else if button_state.middle then Event.Mouse.Middle
          else Event.Mouse.Right
        in
        dispatch_mouse_internal t ~x ~y ~modifiers:no_mod
          (Event.Mouse.Drag { button; is_dragging = true })
      else dispatch_mouse_internal t ~x ~y ~modifiers:no_mod Event.Mouse.Move

let dispatch_paste t text =
  match !(t.focused) with
  | None -> ()
  | Some node ->
      let ev = Event.Paste.of_text text in
      Renderable.Private.emit_paste node ev

let dispatch_scroll t ~x ~y ~direction ~delta ~modifiers =
  let hit_num = Screen.query_hit t.screen ~x ~y in
  let target_node = if hit_num > 0 then find_node t hit_num else None in
  let target_id = Option.map Renderable.Private.num target_node in
  let ev =
    Event.Mouse.make ~x ~y ~modifiers ?target:target_id
      (Event.Mouse.Scroll { direction; delta })
  in
  match target_node with
  | Some node -> Renderable.Private.emit_mouse node ev
  | None -> ()

(* ───── Selection ───── *)

let clear_selection t = clear_active_selection t

(* ───── Frame Callbacks ───── *)

let add_frame_callback t f = t.frame_callbacks <- t.frame_callbacks @ [ f ]

let remove_frame_callback t f =
  t.frame_callbacks <- List.filter (fun e -> not (e == f)) t.frame_callbacks

let clear_frame_callbacks t = t.frame_callbacks <- []

(* ───── Post-Processing ───── *)

let add_post_process t f = Screen.post_process f t.screen
let remove_post_process t id = Screen.remove_post_process id t.screen
let clear_post_processes t = Screen.clear_post_processes t.screen
