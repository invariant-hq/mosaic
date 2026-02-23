open Matrix
module Color = Ansi.Color
module Style = Ansi.Style

type draggable = {
  id : int;
  label : string;
  mutable x : int;
  mutable y : int;
  width : int;
  height : int;
  color : Ansi.Color.t;
}

type drop_zone = {
  label : string;
  x : int;
  y : int;
  width : int;
  height : int;
  color : Ansi.Color.t;
  mutable active : bool;
  mutable last_drop : string option;
}

type dragging = {
  item : draggable;
  start_item_x : int;
  start_item_y : int;
  start_mouse_x : int;
  start_mouse_y : int;
}

type state = {
  items : draggable array;
  zones : drop_zone array;
  mutable dragging : dragging option;
  mutable status : string;
  mutable mouse_x : int;
  mutable mouse_y : int;
  mutable mouse_left : bool;
  workspace_x : int;
  workspace_y : int;
  workspace_width : int;
  workspace_height : int;
}

let clamp lo hi v = max lo (min hi v)

let point_in_rect ~px ~py ~x ~y ~w ~h =
  px >= x && px < x + w && py >= y && py < y + h

let center_point (item : draggable) =
  (item.x + (item.width / 2), item.y + (item.height / 2))

let zone_contains (zone : drop_zone) (item : draggable) =
  let cx, cy = center_point item in
  point_in_rect ~px:cx ~py:cy ~x:zone.x ~y:zone.y ~w:zone.width ~h:zone.height

let find_item_at state ~x ~y =
  (* Search in reverse to find topmost (last drawn) item first *)
  let rec loop i =
    if i < 0 then None
    else
      let item = state.items.(i) in
      if
        point_in_rect ~px:x ~py:y ~x:item.x ~y:item.y ~w:item.width
          ~h:item.height
      then Some item
      else loop (i - 1)
  in
  loop (Array.length state.items - 1)

let move_item_to_front (state : state) (item : draggable) =
  let items = state.items in
  let n = Array.length items in
  let idx = ref (-1) in
  for i = 0 to n - 1 do
    if items.(i).id = item.id then idx := i
  done;
  if !idx >= 0 && !idx < n - 1 then (
    let tmp = items.(!idx) in
    for i = !idx to n - 2 do
      items.(i) <- items.(i + 1)
    done;
    items.(n - 1) <- tmp)

let color_with_alpha color alpha =
  let r, g, b, _ = Ansi.Color.to_rgba color in
  Ansi.Color.of_rgba r g b (int_of_float (alpha *. 255.))

let draw_cell_alpha grid ~x ~y ~bg ~alpha =
  let bg_with_alpha = color_with_alpha bg alpha in
  Grid.set_cell grid ~x ~y ~glyph:Glyph.space ~fg:Ansi.Color.white
    ~bg:bg_with_alpha ~attrs:Ansi.Attr.empty ~blend:true ()

let draw_box grid ~x ~y ~w ~h ~bg ~border_color ~title ~subtitle ~alpha =
  let border_style = Style.make ~fg:border_color () in
  let title_style = Style.make ~fg:(Color.of_rgb 255 255 255) ~bold:true () in
  let subtitle_style = Style.make ~fg:(Color.of_rgb 200 200 200) ~dim:true () in
  (* Fill background with alpha *)
  for row = y to y + h - 1 do
    for col = x to x + w - 1 do
      draw_cell_alpha grid ~x:col ~y:row ~bg ~alpha
    done
  done;
  (* Draw border on top *)
  Grid.draw_text ~style:border_style grid ~x ~y ~text:"\xe2\x94\x8c";
  Grid.draw_text ~style:border_style grid ~x:(x + w - 1) ~y ~text:"\xe2\x94\x90";
  Grid.draw_text ~style:border_style grid ~x ~y:(y + h - 1) ~text:"\xe2\x94\x94";
  Grid.draw_text ~style:border_style grid
    ~x:(x + w - 1)
    ~y:(y + h - 1)
    ~text:"\xe2\x94\x98";
  for col = x + 1 to x + w - 2 do
    Grid.draw_text ~style:border_style grid ~x:col ~y ~text:"\xe2\x94\x80";
    Grid.draw_text ~style:border_style grid ~x:col
      ~y:(y + h - 1)
      ~text:"\xe2\x94\x80"
  done;
  for row = y + 1 to y + h - 2 do
    Grid.draw_text ~style:border_style grid ~x ~y:row ~text:"\xe2\x94\x82";
    Grid.draw_text ~style:border_style grid
      ~x:(x + w - 1)
      ~y:row ~text:"\xe2\x94\x82"
  done;
  (* Draw title centered *)
  let title_x = x + ((w - String.length title) / 2) in
  let title_y = y + ((h - 1) / 2) in
  Grid.draw_text ~style:title_style grid ~x:title_x ~y:title_y ~text:title;
  (* Draw subtitle if provided *)
  if subtitle <> "" then
    let sub_x = x + ((w - String.length subtitle) / 2) in
    Grid.draw_text ~style:subtitle_style grid ~x:sub_x ~y:(title_y + 1)
      ~text:subtitle

let draw_zone grid zone =
  let border_color =
    if zone.active then Color.of_rgb 255 214 0 else Color.of_rgb 120 120 170
  in
  let bg = if zone.active then Color.of_rgb 60 60 100 else zone.color in
  let alpha = if zone.active then 0.9 else 0.6 in
  let subtitle =
    match zone.last_drop with
    | None -> ""
    | Some label -> Printf.sprintf "Last: %s" label
  in
  draw_box grid ~x:zone.x ~y:zone.y ~w:zone.width ~h:zone.height ~bg
    ~border_color ~title:zone.label ~subtitle ~alpha

let draw_item grid (state : state) (item : draggable) =
  let is_dragging =
    match state.dragging with Some d -> d.item.id = item.id | None -> false
  in
  let border_color =
    if is_dragging then Color.of_rgb 255 214 0 else Color.of_rgb 30 30 30
  in
  let alpha = if is_dragging then 0.85 else 0.75 in
  draw_box grid ~x:item.x ~y:item.y ~w:item.width ~h:item.height ~bg:item.color
    ~border_color ~title:item.label ~subtitle:"Drag me" ~alpha

let draw_workspace grid (state : state) =
  let ws_bg = Color.of_rgb 20 25 34 in
  let ws_style = Style.make ~bg:ws_bg () in
  (* Fill workspace background *)
  for
    row = state.workspace_y to state.workspace_y + state.workspace_height - 1
  do
    for
      col = state.workspace_x to state.workspace_x + state.workspace_width - 1
    do
      Grid.draw_text ~style:ws_style grid ~x:col ~y:row ~text:" "
    done
  done;
  (* Draw zones first (below items) *)
  Array.iter (fun zone -> draw_zone grid zone) state.zones;
  (* Draw items *)
  Array.iter (fun item -> draw_item grid state item) state.items

let draw_header grid =
  let title_style = Style.make ~fg:(Color.of_rgb 180 200 255) ~bold:true () in
  let sub_style = Style.make ~fg:(Color.of_rgb 140 160 210) ~dim:true () in
  Grid.draw_text ~style:title_style grid ~x:1 ~y:0
    ~text:"Matrix Drag & Drop Demo";
  Grid.draw_text ~style:sub_style grid ~x:28 ~y:0
    ~text:"Mouse interaction example"

let draw_instructions grid ~y =
  let style = Style.make ~fg:(Color.of_rgb 210 210 210) () in
  Grid.draw_text ~style grid ~x:1 ~y
    ~text:
      "Left click on a tile to start dragging. Drop onto a zone to record it.";
  Grid.draw_text ~style grid ~x:1 ~y:(y + 1) ~text:"Press Esc or q to quit."

let draw_status grid state ~y =
  let bg = Color.of_rgb 18 20 30 in
  let style = Style.make ~fg:(Color.of_rgb 200 220 255) ~bg () in
  let mouse_style = Style.make ~fg:(Color.of_rgb 180 200 255) ~bg () in
  (* Status background *)
  let cols, _ = (Grid.width grid, Grid.height grid) in
  for col = 0 to cols - 1 do
    Grid.draw_text ~style:(Style.make ~bg ()) grid ~x:col ~y ~text:" ";
    Grid.draw_text ~style:(Style.make ~bg ()) grid ~x:col ~y:(y + 1) ~text:" "
  done;
  Grid.draw_text ~style grid ~x:1 ~y ~text:state.status;
  let drag_info =
    match state.dragging with
    | None -> "dragging: no"
    | Some d -> Printf.sprintf "dragging: %s" d.item.label
  in
  let mouse_info =
    Printf.sprintf "mouse: (%d,%d) left=%b - %s" state.mouse_x state.mouse_y
      state.mouse_left drag_info
  in
  Grid.draw_text ~style:mouse_style grid ~x:1 ~y:(y + 1) ~text:mouse_info

let update_zones_highlight state =
  match state.dragging with
  | None -> Array.iter (fun z -> z.active <- false) state.zones
  | Some d ->
      Array.iter (fun z -> z.active <- zone_contains z d.item) state.zones

let start_drag (state : state) (item : draggable) ~mouse_x ~mouse_y =
  move_item_to_front state item;
  state.dragging <-
    Some
      {
        item;
        start_item_x = item.x;
        start_item_y = item.y;
        start_mouse_x = mouse_x;
        start_mouse_y = mouse_y;
      };
  state.status <- Printf.sprintf "Dragging %s" item.label

let update_drag state ~mouse_x ~mouse_y =
  match state.dragging with
  | None -> ()
  | Some d ->
      let dx = mouse_x - d.start_mouse_x in
      let dy = mouse_y - d.start_mouse_y in
      let max_x = state.workspace_x + state.workspace_width - d.item.width in
      let max_y = state.workspace_y + state.workspace_height - d.item.height in
      d.item.x <- clamp state.workspace_x max_x (d.start_item_x + dx);
      d.item.y <- clamp state.workspace_y max_y (d.start_item_y + dy);
      update_zones_highlight state;
      let zone_name =
        Array.fold_left
          (fun acc z -> if z.active then Some z.label else acc)
          None state.zones
      in
      state.status <-
        (match zone_name with
        | None -> Printf.sprintf "Dragging %s" d.item.label
        | Some zn -> Printf.sprintf "Dragging %s over %s" d.item.label zn)

let end_drag state =
  match state.dragging with
  | None -> ()
  | Some d ->
      let dropped_zone =
        Array.fold_left
          (fun acc z ->
            if zone_contains z d.item then (
              z.last_drop <- Some d.item.label;
              Some z.label)
            else acc)
          None state.zones
      in
      state.status <-
        (match dropped_zone with
        | None -> Printf.sprintf "Dropped %s on workspace" d.item.label
        | Some zn -> Printf.sprintf "Dropped %s into %s" d.item.label zn);
      state.dragging <- None;
      Array.iter (fun z -> z.active <- false) state.zones

let cancel_drag state =
  match state.dragging with
  | None -> ()
  | Some d ->
      d.item.x <- d.start_item_x;
      d.item.y <- d.start_item_y;
      state.dragging <- None;
      Array.iter (fun z -> z.active <- false) state.zones;
      state.status <- "Drag cancelled"

let handle_input state event =
  match event with
  | Input.Key { key = Input.Key.Escape; _ } ->
      if Option.is_some state.dragging then (
        cancel_drag state;
        `Continue)
      else `Quit
  | Input.Key { key = Input.Key.Char u; _ }
    when Uchar.to_int u = Char.code 'q' || Uchar.to_int u = Char.code 'Q' ->
      `Quit
  | Input.Mouse (Input.Mouse.Button_press (x, y, Input.Mouse.Left, _)) ->
      state.mouse_x <- x;
      state.mouse_y <- y;
      state.mouse_left <- true;
      (match find_item_at state ~x ~y with
      | Some item -> start_drag state item ~mouse_x:x ~mouse_y:y
      | None -> ());
      `Continue
  | Input.Mouse (Input.Mouse.Button_release (x, y, Input.Mouse.Left, _)) ->
      state.mouse_x <- x;
      state.mouse_y <- y;
      state.mouse_left <- false;
      end_drag state;
      `Continue
  | Input.Mouse (Input.Mouse.Motion (x, y, btns, _)) ->
      state.mouse_x <- x;
      state.mouse_y <- y;
      state.mouse_left <- btns.Input.Mouse.left;
      if btns.Input.Mouse.left then update_drag state ~mouse_x:x ~mouse_y:y;
      `Continue
  | _ -> `Continue

let create_state () =
  let workspace_x = 1 in
  let workspace_y = 4 in
  let workspace_width = 68 in
  let workspace_height = 20 in
  {
    items =
      [|
        {
          id = 1;
          label = "Docs";
          x = workspace_x + 3;
          y = workspace_y + 2;
          width = 12;
          height = 5;
          color = Color.of_rgb 42 156 252;
        };
        {
          id = 2;
          label = "Tasks";
          x = workspace_x + 19;
          y = workspace_y + 6;
          width = 12;
          height = 5;
          color = Color.of_rgb 118 201 68;
        };
        {
          id = 3;
          label = "Ideas";
          x = workspace_x + 35;
          y = workspace_y + 4;
          width = 12;
          height = 5;
          color = Color.of_rgb 244 165 96;
        };
      |];
    zones =
      [|
        {
          label = "Inbox";
          x = workspace_x + 3;
          y = workspace_y + 12;
          width = 20;
          height = 6;
          color = Color.of_rgb 80 80 120;
          active = false;
          last_drop = None;
        };
        {
          label = "Review";
          x = workspace_x + 31;
          y = workspace_y + 12;
          width = 20;
          height = 6;
          color = Color.of_rgb 90 70 140;
          active = false;
          last_drop = None;
        };
      |];
    dragging = None;
    status = "Click and drag a tile to move it between drop zones";
    mouse_x = 0;
    mouse_y = 0;
    mouse_left = false;
    workspace_x;
    workspace_y;
    workspace_width;
    workspace_height;
  }

let () =
  let app =
    Matrix.create ~target_fps:(Some 60.) ~mouse_enabled:true ~respect_alpha:true
      ~debug_overlay:true ()
  in
  let state = create_state () in
  Matrix.run app
    ~on_input:(fun app event ->
      match handle_input state event with
      | `Quit -> Matrix.stop app
      | `Continue -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let _, rows = Matrix.size app in
      Grid.clear ~color:(Color.of_rgb 12 14 22) grid;
      draw_header grid;
      draw_instructions grid ~y:1;
      draw_workspace grid state;
      draw_status grid state ~y:(rows - 2))
