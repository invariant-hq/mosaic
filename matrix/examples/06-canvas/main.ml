open Matrix

(* Drawing canvas: mouse-driven paint program with alpha blending *)

type brush = { size : int; color : Ansi.Color.t; alpha : float }
type state = { brush : brush; show_palette : bool; show_help : bool }

let palette_colors =
  [|
    Ansi.Color.red;
    Ansi.Color.bright_red;
    Ansi.Color.yellow;
    Ansi.Color.bright_yellow;
    Ansi.Color.green;
    Ansi.Color.bright_green;
    Ansi.Color.cyan;
    Ansi.Color.bright_cyan;
    Ansi.Color.blue;
    Ansi.Color.bright_blue;
    Ansi.Color.magenta;
    Ansi.Color.bright_magenta;
    Ansi.Color.white;
    Ansi.Color.bright_white;
    Ansi.Color.black;
    Ansi.Color.bright_black;
  |]

let initial_state =
  {
    brush = { size = 1; color = Ansi.Color.white; alpha = 1.0 };
    show_palette = true;
    show_help = true;
  }

let color_with_alpha color alpha =
  let r, g, b, _ = Ansi.Color.to_rgba color in
  Ansi.Color.of_rgba r g b (int_of_float (alpha *. 255.))

let draw_pixel grid ~x ~y ~color ~alpha =
  if alpha >= 0.999 then
    (* Fully opaque - just set the background *)
    let style = Ansi.Style.make ~bg:color () in
    Grid.draw_text ~style grid ~x ~y ~text:" "
  else
    (* Use alpha blending *)
    let bg = color_with_alpha color alpha in
    Grid.set_cell grid ~x ~y ~glyph:Glyph.space ~fg:Ansi.Color.white ~bg
      ~attrs:Ansi.Attr.empty ~blend:true ()

let draw_brush grid ~cx ~cy ~brush =
  let half = brush.size / 2 in
  for dy = -half to half do
    for dx = -half to half do
      let x = cx + dx in
      let y = cy + dy in
      (* Simple circle check for larger brushes *)
      if brush.size <= 1 || (dx * dx) + (dy * dy) <= half * half then
        draw_pixel grid ~x ~y ~color:brush.color ~alpha:brush.alpha
    done
  done

let draw_palette grid ~cols ~rows:_ state =
  if not state.show_palette then ()
  else
    let panel_width = 20 in
    let panel_height = Array.length palette_colors + 5 in
    let px = cols - panel_width - 2 in
    let py = 1 in
    (* Draw panel background *)
    let bg_style = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
    for y = py to py + panel_height - 1 do
      for x = px to px + panel_width - 1 do
        Grid.draw_text ~style:bg_style grid ~x ~y ~text:" "
      done
    done;
    (* Title *)
    let title_style =
      Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true ()
    in
    Grid.draw_text ~style:title_style grid ~x:(px + 1) ~y:py ~text:"Palette [P]";
    (* Colors *)
    let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 1)
      ~text:"Colors (1-9,0):";
    for i = 0 to min 9 (Array.length palette_colors - 1) do
      let color = palette_colors.(i) in
      let style = Ansi.Style.make ~bg:color () in
      let marker =
        if Ansi.Color.equal color state.brush.color then "*" else " "
      in
      Grid.draw_text ~style grid ~x:(px + 1 + (i * 2)) ~y:(py + 2) ~text:marker
    done;
    (* Alpha *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 4)
      ~text:"Alpha (A/Z):";
    let alpha_str = Printf.sprintf "%.0f%%" (state.brush.alpha *. 100.) in
    Grid.draw_text ~style:label_style grid ~x:(px + 13) ~y:(py + 4)
      ~text:alpha_str;
    (* Brush size *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 5)
      ~text:"Size (+/-):";
    let size_str = string_of_int state.brush.size in
    Grid.draw_text ~style:label_style grid ~x:(px + 13) ~y:(py + 5)
      ~text:size_str;
    (* Current brush preview *)
    Grid.draw_text ~style:label_style grid ~x:(px + 1) ~y:(py + 7)
      ~text:"Brush:";
    let preview_style = Ansi.Style.make ~bg:state.brush.color () in
    Grid.draw_text ~style:preview_style grid ~x:(px + 8) ~y:(py + 7) ~text:"   "

let draw_help grid ~rows state =
  if not state.show_help then ()
  else
    let help_lines =
      [
        "Click/drag to draw";
        "1-0: colors";
        "A/Z: alpha +/-";
        "+/-: brush size";
        "C: clear canvas";
        "P: toggle palette";
        "H: toggle help";
        "Esc: quit";
      ]
    in
    let style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
    List.iteri
      (fun i line ->
        Grid.draw_text ~style grid ~x:2
          ~y:(rows - List.length help_lines + i - 1)
          ~text:line)
      help_lines

let handle_key state key modifier =
  let open Input.Key in
  match key with
  | Char u -> (
      let code = Uchar.to_int u in
      match Char.chr code with
      | '1' .. '9' as c ->
          let idx = Char.code c - Char.code '1' in
          if idx < Array.length palette_colors then
            {
              state with
              brush = { state.brush with color = palette_colors.(idx) };
            }
          else state
      | '0' ->
          if 9 < Array.length palette_colors then
            {
              state with
              brush = { state.brush with color = palette_colors.(9) };
            }
          else state
      | 'a' | 'A' ->
          let new_alpha = min 1.0 (state.brush.alpha +. 0.1) in
          { state with brush = { state.brush with alpha = new_alpha } }
      | 'z' | 'Z' ->
          let new_alpha = max 0.1 (state.brush.alpha -. 0.1) in
          { state with brush = { state.brush with alpha = new_alpha } }
      | ('c' | 'C') when not modifier.ctrl ->
          state (* Clear is handled separately *)
      | 'p' | 'P' -> { state with show_palette = not state.show_palette }
      | 'h' | 'H' -> { state with show_help = not state.show_help }
      | '+' | '=' ->
          let new_size = min 10 (state.brush.size + 1) in
          { state with brush = { state.brush with size = new_size } }
      | '-' | '_' ->
          let new_size = max 1 (state.brush.size - 1) in
          { state with brush = { state.brush with size = new_size } }
      | _ -> state)
  | _ -> state

let () =
  let app =
    Matrix.create ~target_fps:(Some 60.) ~mouse_enabled:true ~respect_alpha:true
      ~debug_overlay:false ()
  in
  let state = ref initial_state in
  let canvas_grid = ref None in
  Matrix.run app
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; modifier; _ }
        when (Uchar.to_int u = Char.code 'c' || Uchar.to_int u = Char.code 'C')
             && not modifier.ctrl -> (
          (* Clear canvas *)
          match !canvas_grid with
          | Some cg -> Grid.clear ~color:Ansi.Color.black cg
          | None -> ())
      | Input.Key { key; modifier; _ } ->
          state := handle_key !state key modifier
      | Input.Mouse (Input.Mouse.Button_press (x, y, Input.Mouse.Left, _))
      | Input.Mouse
          (Input.Mouse.Motion (x, y, { Input.Mouse.left = true; _ }, _)) -> (
          match !canvas_grid with
          | Some cg -> draw_brush cg ~cx:x ~cy:y ~brush:!state.brush
          | None -> ())
      | Input.Mouse (Input.Mouse.Button_press (x, y, Input.Mouse.Right, _))
      | Input.Mouse
          (Input.Mouse.Motion (x, y, { Input.Mouse.right = true; _ }, _)) -> (
          (* Erase with right click *)
          match !canvas_grid with
          | Some cg ->
              let erase_brush =
                { !state.brush with color = Ansi.Color.black; alpha = 1.0 }
              in
              draw_brush cg ~cx:x ~cy:y ~brush:erase_brush
          | None -> ())
      | _ -> ())
    ~on_resize:(fun _ ~cols ~rows ->
      (* Preserve canvas content on resize *)
      let old_grid = !canvas_grid in
      let new_grid =
        Grid.create ~width:cols ~height:rows ~respect_alpha:true ()
      in
      Grid.clear ~color:Ansi.Color.black new_grid;
      (match old_grid with
      | Some og ->
          Grid.blit_region ~src:og ~dst:new_grid ~src_x:0 ~src_y:0
            ~width:(Grid.width og) ~height:(Grid.height og) ~dst_x:0 ~dst_y:0
      | None -> ());
      canvas_grid := Some new_grid)
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      (* Copy canvas to render grid *)
      (match !canvas_grid with
      | Some cg -> Grid.blit ~src:cg ~dst:grid
      | None -> Grid.clear ~color:Ansi.Color.black grid);
      (* Draw UI overlays *)
      draw_palette grid ~cols ~rows !state;
      draw_help grid ~rows !state)
