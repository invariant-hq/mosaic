open Matrix

type draw_context = { grid : Grid.t; cols : int; rows : int }

let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
let empty_attrs = Ansi.Attr.empty
let tau = 2.0 *. Float.pi
let palette_size = 1024
let baseline_span = 3.5
let min_scale = 5e-12
let max_scale = 0.2
let zoom_in_factor = 0.85
let pan_fraction = 0.15
let scroll_pan_fraction = 0.05
let braille_cols = 2
let braille_rows = 4
let inside_color = Ansi.Color.black

let braille_bit row col =
  match (row, col) with
  | 0, 0 -> 0x01
  | 1, 0 -> 0x02
  | 2, 0 -> 0x04
  | 3, 0 -> 0x40
  | 0, 1 -> 0x08
  | 1, 1 -> 0x10
  | 2, 1 -> 0x20
  | 3, 1 -> 0x80
  | _ -> 0

let braille_code mask = 0x2800 + mask
let clamp_scale scale = clamp min_scale max_scale scale
let initial_iterations = 256
let initial_center_x = -0.75
let initial_center_y = 0.0
let pixel_aspect = 0.55

type dims = { cols : int; rows : int }
type viewport = { center_x : float; center_y : float; scale : float }

type state = {
  dims : dims;
  viewport : viewport;
  max_iter : int;
  show_help : bool;
  palette_shift : int;
  pointer : int * int;
}

let make_viewport dims =
  let cols = max 1 dims.cols in
  let scale = baseline_span /. float cols in
  { center_x = initial_center_x; center_y = initial_center_y; scale }

let layout_for dims show_help =
  let rows = max 0 dims.rows in
  if rows = 0 then (0, 0)
  else
    let desired_overlay = if show_help then 3 else 2 in
    let overlay_cap = rows / 2 in
    let overlay = min desired_overlay overlay_cap in
    let fractal_rows = max 1 (rows - overlay) in
    (fractal_rows, overlay)

let layout st = layout_for st.dims st.show_help

let clamp_pointer_with_rows dims fractal_rows (x, y) =
  let max_x = max 0 (dims.cols - 1) in
  let max_y = max 0 (fractal_rows - 1) in
  (clamp 0 max_x x, clamp 0 max_y y)

let pointer_in_fractal dims fractal_rows x y =
  dims.cols > 0 && fractal_rows > 0 && x >= 0 && x < dims.cols && y >= 0
  && y < fractal_rows

let normalize_pointer st =
  let fractal_rows, _ = layout st in
  let pointer = clamp_pointer_with_rows st.dims fractal_rows st.pointer in
  { st with pointer }

let make_state ~cols ~rows =
  let dims = { cols; rows } in
  let base =
    {
      dims;
      viewport = make_viewport dims;
      max_iter = initial_iterations;
      show_help = true;
      palette_shift = 0;
      pointer = (0, 0);
    }
  in
  let fractal_rows, _ = layout base in
  let center_x = if dims.cols <= 0 then 0 else dims.cols / 2 in
  let center_y = if fractal_rows <= 0 then 0 else fractal_rows / 2 in
  let pointer =
    clamp_pointer_with_rows dims fractal_rows (center_x, center_y)
  in
  { base with pointer }

let mandelbrot ~max_iter cx cy =
  let rec loop iter zr zi =
    if iter >= max_iter then float max_iter
    else
      let zr2 = zr *. zr in
      let zi2 = zi *. zi in
      let magnitude = zr2 +. zi2 in
      if magnitude > 4.0 then
        let log_zn = Float.log magnitude /. 2.0 in
        let nu = Float.log (log_zn /. Float.log 2.) /. Float.log 2. in
        float iter +. 1. -. nu
      else
        let zi' = (2.0 *. zr *. zi) +. cy in
        let zr' = zr2 -. zi2 +. cx in
        loop (iter + 1) zr' zi'
  in
  loop 0 0.0 0.0

let palette_phase st normalized =
  let shift =
    ((st.palette_shift mod palette_size) + palette_size) mod palette_size
  in
  let shift_phase = tau *. (float shift /. float palette_size) in
  (normalized *. tau) +. shift_phase

let palette_color st normalized =
  if normalized >= 0.999 then inside_color
  else
    let phase = palette_phase st (normalized ** 0.85) in
    let component offset =
      let v = (sin (phase +. offset) *. 0.5) +. 0.5 in
      int_of_float (Float.round (v *. 255.))
    in
    let r = component (tau /. 3.) in
    let g = component 0. in
    let b = component (-.(tau /. 3.)) in
    Ansi.Color.of_rgb r g b

let draw_overlay st (ctx : draw_context) fractal_rows overlay =
  if overlay <= 0 then ()
  else
    let status_row = min (ctx.rows - 1) fractal_rows in
    if status_row < 0 then ()
    else
      let span_x = st.viewport.scale *. float (max 1 st.dims.cols) in
      let zoom = baseline_span /. span_x in
      let status_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
      let status =
        Printf.sprintf "Zoom %5.2fx  Iter %d  Center %.4f%+.4fi" zoom
          st.max_iter st.viewport.center_x st.viewport.center_y
      in
      Grid.draw_text ~style:status_style ctx.grid ~x:0 ~y:status_row
        ~text:status;
      let controls_row = status_row + 1 in
      if controls_row < ctx.rows then (
        let controls_text =
          if st.show_help then
            "Arrows: pan  +/- or wheel: zoom  scroll horiz: pan  [ ]: iter  "
            ^ "C: cycle colors  R: reset  H: hide help"
          else
            "Arrows pan  +/- zoom  wheel zoom focus  [] iter  C colors  R \
             reset  H help"
        in
        Grid.draw_text ~style:status_style ctx.grid ~x:0 ~y:controls_row
          ~text:controls_text;
        if st.show_help then
          let hint_row = controls_row + 1 in
          if hint_row < ctx.rows then
            Grid.draw_text ~style:status_style ctx.grid ~x:0 ~y:hint_row
              ~text:"Tip: zoom on a point with the mouse wheel to dive into it.")

let draw_fractal st (ctx : draw_context) =
  let cols = ctx.cols in
  let rows = ctx.rows in
  if cols <= 0 || rows <= 0 then ()
  else
    let fractal_rows, overlay = layout st in
    let usable_rows = min rows fractal_rows in
    Grid.fill_rect ctx.grid ~x:0 ~y:0 ~width:cols ~height:rows
      ~color:Ansi.Color.black;
    (if usable_rows > 0 then
       let width = float (max 1 cols) in
       let height = float (max 1 usable_rows) in
       let sub_cols = float braille_cols in
       let sub_rows = float braille_rows in
       let max_iter_f = float st.max_iter in
       for y = 0 to usable_rows - 1 do
         for x = 0 to cols - 1 do
           let mask = ref 0 in
           let r_sum = ref 0.0 in
           let g_sum = ref 0.0 in
           let b_sum = ref 0.0 in
           let count = ref 0 in
           for sub_y = 0 to braille_rows - 1 do
             let sample_y =
               (float y +. ((float sub_y +. 0.5) /. sub_rows) -. (height /. 2.))
               *. st.viewport.scale *. pixel_aspect
             in
             for sub_x = 0 to braille_cols - 1 do
               let sample_x =
                 (float x +. ((float sub_x +. 0.5) /. sub_cols) -. (width /. 2.))
                 *. st.viewport.scale
               in
               let cx = st.viewport.center_x +. sample_x in
               let cy = st.viewport.center_y +. sample_y in
               let sample = mandelbrot ~max_iter:st.max_iter cx cy in
               let normalized = clamp 0.0 1.0 (sample /. max_iter_f) in
               if normalized < 0.999 then (
                 mask := !mask lor braille_bit sub_y sub_x;
                 let color = palette_color st normalized in
                 let r, g, b, _ = Ansi.Color.to_rgba color in
                 r_sum := !r_sum +. float r;
                 g_sum := !g_sum +. float g;
                 b_sum := !b_sum +. float b;
                 incr count)
             done
           done;
           if !mask <> 0 then
             let fg =
               if !count = 0 then inside_color
               else
                 let inv = 1. /. float !count in
                 let r = int_of_float (Float.round (!r_sum *. inv)) in
                 let g = int_of_float (Float.round (!g_sum *. inv)) in
                 let b = int_of_float (Float.round (!b_sum *. inv)) in
                 Ansi.Color.of_rgb r g b
             in
             Grid.set_cell ctx.grid ~x ~y
               ~glyph:(Glyph.of_uchar (Uchar.of_int (braille_code !mask)))
               ~fg ~bg:inside_color ~attrs:empty_attrs ~blend:true ()
         done
       done);
    draw_overlay st ctx usable_rows overlay

let draw _app ctx st = draw_fractal st ctx

let with_redraw app st =
  request_redraw app;
  `Continue st

let resize st ~cols ~rows =
  if cols = st.dims.cols && rows = st.dims.rows then st
  else
    let cols = max 1 cols in
    let prev_cols = max 1 st.dims.cols in
    let scale = st.viewport.scale *. (float prev_cols /. float cols) in
    let updated =
      {
        st with
        dims = { cols; rows };
        viewport = { st.viewport with scale = clamp_scale scale };
      }
    in
    normalize_pointer updated

let pan ?(fraction = pan_fraction) st dx dy =
  let fractal_rows, _ = layout st in
  let span_x = st.viewport.scale *. float (max 1 st.dims.cols) in
  let span_y =
    st.viewport.scale *. pixel_aspect *. float (max 1 fractal_rows)
  in
  let center_x = st.viewport.center_x +. (dx *. span_x *. fraction) in
  let center_y = st.viewport.center_y +. (dy *. span_y *. fraction) in
  { st with viewport = { st.viewport with center_x; center_y } }

let zoom_at st ~x ~y ~factor =
  let fractal_rows, _ = layout st in
  if st.dims.cols <= 0 || fractal_rows <= 0 then st
  else
    let clamped_x = clamp 0 (st.dims.cols - 1) x in
    let clamped_y = clamp 0 (fractal_rows - 1) y in
    let width = float (max 1 st.dims.cols) in
    let height = float (max 1 fractal_rows) in
    let px = float clamped_x +. 0.5 -. (width /. 2.) in
    let py = float clamped_y +. 0.5 -. (height /. 2.) in
    let old_scale = st.viewport.scale in
    let new_scale = clamp_scale (old_scale *. factor) in
    let unit_y = old_scale *. pixel_aspect in
    let new_unit_y = new_scale *. pixel_aspect in
    let target_x = st.viewport.center_x +. (px *. old_scale) in
    let target_y = st.viewport.center_y +. (py *. unit_y) in
    let center_x = target_x -. (px *. new_scale) in
    let center_y = target_y -. (py *. new_unit_y) in
    { st with viewport = { center_x; center_y; scale = new_scale } }

let adjust_iterations st delta =
  let iter = clamp 32 4096 (st.max_iter + delta) in
  { st with max_iter = iter }

let cycle_palette st amount =
  let shift = (st.palette_shift + amount) mod palette_size in
  { st with palette_shift = shift }

let reset st =
  {
    st with
    viewport = make_viewport st.dims;
    max_iter = initial_iterations;
    palette_shift = 0;
  }

let handle_char app st uchar modifier =
  let code = Uchar.to_int uchar in
  let ctrl_c =
    modifier.Input.Modifier.ctrl
    && (code = Char.code 'c' || code = Char.code 'C')
  in
  if ctrl_c then (
    stop app;
    `Stop)
  else if code > 255 then `Continue st
  else
    let focus_x = st.dims.cols / 2 in
    let fractal_rows, _ = layout st in
    let focus_y = fractal_rows / 2 in
    match Char.chr code with
    | '+' | '=' ->
        with_redraw app
          (zoom_at st ~x:focus_x ~y:focus_y ~factor:zoom_in_factor)
    | '-' | '_' ->
        let factor = 1. /. zoom_in_factor in
        with_redraw app (zoom_at st ~x:focus_x ~y:focus_y ~factor)
    | '[' -> with_redraw app (adjust_iterations st (-32))
    | ']' -> with_redraw app (adjust_iterations st 32)
    | 'r' | 'R' -> with_redraw app (reset st)
    | 'h' | 'H' ->
        let toggled =
          normalize_pointer { st with show_help = not st.show_help }
        in
        with_redraw app toggled
    | 'c' | 'C' -> with_redraw app (cycle_palette st 37)
    | _ -> `Continue st

let update app event st =
  match event with
  | Input.Mouse mouse_ev -> (
      let coord =
        match mouse_ev with
        | Input.Mouse.Button_press (x, y, _, _)
        | Input.Mouse.Button_release (x, y, _, _)
        | Input.Mouse.Motion (x, y, _, _) ->
            Some (x, y)
      in
      match coord with
      | Some (x, y) ->
          let fractal_rows, _ = layout st in
          if pointer_in_fractal st.dims fractal_rows x y then
            `Continue { st with pointer = (x, y) }
          else `Continue st
      | None -> `Continue st)
  | Input.Key { key = Input.Key.Escape; _ } ->
      stop app;
      `Stop
  | Input.Key { key = Input.Key.Char uchar; modifier; _ } ->
      handle_char app st uchar modifier
  | Input.Key { key = Input.Key.Left; _ } -> with_redraw app (pan st (-1.) 0.)
  | Input.Key { key = Input.Key.Right; _ } -> with_redraw app (pan st 1. 0.)
  | Input.Key { key = Input.Key.Up; _ } -> with_redraw app (pan st 0. (-1.))
  | Input.Key { key = Input.Key.Down; _ } -> with_redraw app (pan st 0. 1.)
  | Input.Scroll (x, y, dir, delta, _) ->
      let fractal_rows, _ = layout st in
      if fractal_rows <= 0 then `Continue st
      else
        let focus_x, focus_y, st =
          if pointer_in_fractal st.dims fractal_rows x y then
            (x, y, { st with pointer = (x, y) })
          else
            let fallback_x, fallback_y =
              clamp_pointer_with_rows st.dims fractal_rows st.pointer
            in
            ( fallback_x,
              fallback_y,
              { st with pointer = (fallback_x, fallback_y) } )
        in
        let steps = max 1 (abs delta) in
        let factor_pow base = Float.pow base (float steps) in
        let new_state =
          match dir with
          | Input.Mouse.Scroll_up ->
              zoom_at st ~x:focus_x ~y:focus_y
                ~factor:(factor_pow zoom_in_factor)
          | Input.Mouse.Scroll_down ->
              zoom_at st ~x:focus_x ~y:focus_y
                ~factor:(factor_pow (1. /. zoom_in_factor))
          | Input.Mouse.Scroll_left ->
              pan ~fraction:scroll_pan_fraction st (float (-steps)) 0.
          | Input.Mouse.Scroll_right ->
              pan ~fraction:scroll_pan_fraction st (float steps) 0.
        in
        with_redraw app new_state
  | Input.Resize (cols, rows) -> with_redraw app (resize st ~cols ~rows)
  | _ -> `Continue st

let () =
  let app = Matrix.create ~mouse_enabled:true () in
  let state = ref (make_state ~cols:1 ~rows:1) in
  Matrix.run app
    ~on_input:(fun app event ->
      match update app event !state with
      | `Continue st' -> state := st'
      | `Stop -> ())
    ~on_resize:(fun _ ~cols ~rows -> state := resize !state ~cols ~rows)
    ~on_render:(fun app ->
      let cols, rows = Matrix.size app in
      let ctx = { grid = Matrix.grid app; cols; rows } in
      draw app ctx !state)
