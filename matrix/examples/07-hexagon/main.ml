open Matrix

(* Spinning hexagon with bouncing ball inside *)

let pi = Float.pi

type state = {
  hex_angle : float;
  ball_x : float;
  ball_y : float;
  ball_vx : float;
  ball_vy : float;
}

let hex_rotation_speed = 0.6
let hex_radius = 8.0
let gravity = 120.0
let bounce_damping = 0.85
let ball_radius = 0.5
let min_speed = 20.0

let initial_state =
  {
    hex_angle = 0.0;
    ball_x = 0.0;
    ball_y = -2.0;
    ball_vx = 25.0;
    ball_vy = 0.0;
  }

(* Get hexagon vertices at given angle, centered at origin with given radius *)
let hex_vertices ~radius ~angle =
  Array.init 6 (fun i ->
      let a = angle +. (float_of_int i *. pi /. 3.0) in
      (radius *. cos a, radius *. sin a))

(* Check if point is inside the hexagon and compute bounce if needed *)
let constrain_to_hexagon ~radius ~angle ~omega ~x ~y ~vx ~vy =
  let vertices = hex_vertices ~radius ~angle in
  (* Check each edge of the hexagon *)
  let rec check_edges i new_x new_y new_vx new_vy bounced =
    if i >= 6 then (new_x, new_y, new_vx, new_vy, bounced)
    else
      let x1, y1 = vertices.(i) in
      let x2, y2 = vertices.((i + 1) mod 6) in
      (* Edge vector *)
      let ex, ey = (x2 -. x1, y2 -. y1) in
      let len = sqrt ((ex *. ex) +. (ey *. ey)) in
      (* Inward normal (pointing toward center of hexagon) *)
      let nx, ny = (-.ey /. len, ex /. len) in
      (* Vector from edge start to ball *)
      let dx, dy = (new_x -. x1, new_y -. y1) in
      (* Signed distance from ball to edge line (positive = inside hexagon) *)
      let dist = (dx *. nx) +. (dy *. ny) in
      if dist < ball_radius then
        (* Ball is too close to edge - push it inward and bounce *)
        let penetration = ball_radius -. dist in
        let pushed_x = new_x +. (nx *. penetration) in
        let pushed_y = new_y +. (ny *. penetration) in
        (* Calculate wall velocity at contact point due to rotation *)
        (* For rotation around origin: v = omega * r, perpendicular to radius *)
        (* Wall velocity at point (x,y) is (-omega*y, omega*x) *)
        let wall_vx = -.omega *. pushed_y in
        let wall_vy = omega *. pushed_x in
        (* Relative velocity of ball to wall *)
        let rel_vx = new_vx -. wall_vx in
        let rel_vy = new_vy -. wall_vy in
        (* Reflect relative velocity off the edge *)
        let dot = (rel_vx *. nx) +. (rel_vy *. ny) in
        if dot < 0.0 then
          (* Moving toward edge - reflect relative velocity *)
          let refl_rel_vx = rel_vx -. (2.0 *. dot *. nx) in
          let refl_rel_vy = rel_vy -. (2.0 *. dot *. ny) in
          (* Add wall velocity back and apply damping *)
          let final_vx = (refl_rel_vx +. wall_vx) *. bounce_damping in
          let final_vy = (refl_rel_vy +. wall_vy) *. bounce_damping in
          check_edges (i + 1) pushed_x pushed_y final_vx final_vy true
        else check_edges (i + 1) pushed_x pushed_y new_vx new_vy bounced
      else check_edges (i + 1) new_x new_y new_vx new_vy bounced
  in
  check_edges 0 x y vx vy false

let update ~dt state =
  (* Rotate hexagon *)
  let hex_angle = state.hex_angle +. (hex_rotation_speed *. dt) in
  let hex_angle =
    if hex_angle > 2.0 *. pi then hex_angle -. (2.0 *. pi) else hex_angle
  in
  (* Apply gravity to ball *)
  let ball_vy = state.ball_vy +. (gravity *. dt) in
  (* Move ball *)
  let ball_x = state.ball_x +. (state.ball_vx *. dt) in
  let ball_y = state.ball_y +. (ball_vy *. dt) in
  (* Constrain ball to hexagon, passing rotation speed for energy transfer *)
  let ball_x, ball_y, ball_vx, ball_vy, _ =
    constrain_to_hexagon ~radius:hex_radius ~angle:hex_angle
      ~omega:hex_rotation_speed ~x:ball_x ~y:ball_y ~vx:state.ball_vx
      ~vy:ball_vy
  in
  (* Keep the ball moving if it gets too slow *)
  let speed = sqrt ((ball_vx *. ball_vx) +. (ball_vy *. ball_vy)) in
  let ball_vx, ball_vy =
    if speed < min_speed && speed > 0.001 then
      let scale = min_speed /. speed in
      (ball_vx *. scale, ball_vy *. scale)
    else (ball_vx, ball_vy)
  in
  { hex_angle; ball_x; ball_y; ball_vx; ball_vy }

(* Convert world coordinates to screen coordinates *)
let world_to_screen ~cx ~cy ~scale x y =
  let sx = cx +. (x *. scale) in
  let sy = cy +. (y *. scale *. 0.5) in
  (* 0.5 because terminal chars are ~2x tall *)
  (int_of_float sx, int_of_float sy)

(* Draw a line using Bresenham's algorithm *)
let draw_line grid ~style ~x0 ~y0 ~x1 ~y1 =
  let dx = abs (x1 - x0) in
  let dy = abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let rec loop x y err =
    if x >= 0 && x < Grid.width grid && y >= 0 && y < Grid.height grid then
      Grid.draw_text ~style grid ~x ~y ~text:"*";
    if x = x1 && y = y1 then ()
    else
      let e2 = 2 * err in
      let err, x = if e2 > -dy then (err - dy, x + sx) else (err, x) in
      let err, y = if e2 < dx then (err + dx, y + sy) else (err, y) in
      loop x y err
  in
  loop x0 y0 (dx - dy)

let draw_hexagon grid ~cx ~cy ~scale ~angle =
  let vertices = hex_vertices ~radius:8.0 ~angle in
  let style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  for i = 0 to 5 do
    let x1, y1 = vertices.(i) in
    let x2, y2 = vertices.((i + 1) mod 6) in
    let sx1, sy1 = world_to_screen ~cx ~cy ~scale x1 y1 in
    let sx2, sy2 = world_to_screen ~cx ~cy ~scale x2 y2 in
    draw_line grid ~style ~x0:sx1 ~y0:sy1 ~x1:sx2 ~y1:sy2
  done

let draw_ball grid ~cx ~cy ~scale ~x ~y =
  let sx, sy = world_to_screen ~cx ~cy ~scale x y in
  let style =
    Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bg:Ansi.Color.yellow
      ~bold:true ()
  in
  if sx >= 0 && sx < Grid.width grid && sy >= 0 && sy < Grid.height grid then
    Grid.draw_text ~style grid ~x:sx ~y:sy ~text:"O"

let draw_ui grid ~cols ~rows =
  let title_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_magenta ~bold:true ()
  in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  let title = "Spinning Hexagon" in
  Grid.draw_text ~style:title_style grid
    ~x:((cols - String.length title) / 2)
    ~y:0 ~text:title;
  let help = "Esc to quit" in
  Grid.draw_text ~style:help_style grid
    ~x:((cols - String.length help) / 2)
    ~y:(rows - 1) ~text:help

let () =
  let app =
    Matrix.create ~target_fps:(Some 60.) ~mouse_enabled:false
      ~debug_overlay:true ()
  in
  let state = ref initial_state in
  Matrix.run app
    ~on_frame:(fun _ ~dt -> state := update ~dt !state)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; _ }
        when Uchar.to_int u = Char.code ' ' ->
          (* Reset ball with random velocity *)
          state :=
            {
              !state with
              ball_x = 0.0;
              ball_y = 0.0;
              ball_vx = -50.0 +. Random.float 100.0;
              ball_vy = -50.0 +. Random.float 100.0;
            }
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      let cx = float_of_int cols /. 2.0 in
      let cy = float_of_int rows /. 2.0 in
      let scale =
        Float.min (float_of_int cols /. 25.0) (float_of_int rows /. 12.0)
      in
      draw_hexagon grid ~cx ~cy ~scale ~angle:!state.hex_angle;
      draw_ball grid ~cx ~cy ~scale ~x:!state.ball_x ~y:!state.ball_y;
      draw_ui grid ~cols ~rows)
