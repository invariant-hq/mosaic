open Matrix
module Color = Ansi.Color
module Style = Ansi.Style

type particle = {
  mutable x : float;
  mutable y : float;
  vx : float;
  mutable vy : float;
  mutable life : float;
  max_life : float;
  symbol : string;
  mutable color : Color.t;
}

type emitter_mode = Burst | Continuous | Fountain | Explosion | Firework
type emitter = { x : float; y : float; mode : emitter_mode; active : bool }

type state = {
  mutable particles : particle list;
  emitters : emitter list;
  mutable current_mode : emitter_mode;
  mutable gravity : float;
  mutable spawn_rate : float;
  mutable time_acc : float;
  mutable mouse_x : int;
  mutable mouse_y : int;
  mutable mouse_emitter : bool;
  mutable paused : bool;
}

let symbols = [| "●"; "◆"; "★"; "♦"; "♥"; "◉"; "⬤"; "✦"; "✧"; "○" |]

let spark_symbols = [| "·"; "∙"; "•"; "°"; "'" |]
let random_float min max = min +. Random.float (max -. min)

let color_for_life life max_life mode =
  let t = life /. max_life in
  match mode with
  | Burst ->
      let r = int_of_float (255. *. t) in
      let g = int_of_float (200. *. (1. -. t)) in
      let b = int_of_float (50. +. (150. *. (1. -. t))) in
      Color.of_rgb r g b
  | Continuous ->
      let hue = t *. 360. in
      let h = hue /. 60. in
      let i = int_of_float h mod 6 in
      let f = h -. Float.floor h in
      let q = 1. -. f in
      let r, g, b =
        match i with
        | 0 -> (1., f, 0.)
        | 1 -> (q, 1., 0.)
        | 2 -> (0., 1., f)
        | 3 -> (0., q, 1.)
        | 4 -> (f, 0., 1.)
        | _ -> (1., 0., q)
      in
      Color.of_rgb
        (int_of_float (r *. 255.))
        (int_of_float (g *. 255.))
        (int_of_float (b *. 255.))
  | Fountain ->
      let r = int_of_float (100. +. (155. *. t)) in
      let g = int_of_float (150. +. (105. *. t)) in
      let b = 255 in
      Color.of_rgb r g b
  | Explosion ->
      let r = 255 in
      let g = int_of_float (255. *. t *. t) in
      let b = int_of_float (50. *. t) in
      Color.of_rgb r g b
  | Firework ->
      let r = int_of_float (200. +. (55. *. Random.float 1.)) in
      let g = int_of_float (100. +. (155. *. t)) in
      let b = int_of_float (255. *. (1. -. t)) in
      Color.of_rgb r g b

let spawn_particle ~x ~y ~mode =
  let vx, vy, max_life, symbol =
    match mode with
    | Burst ->
        let angle = Random.float (2. *. Float.pi) in
        let speed = random_float 5. 15. in
        let vx = speed *. Float.cos angle in
        let vy = speed *. Float.sin angle in
        (vx, vy, random_float 1. 2., symbols.(Random.int (Array.length symbols)))
    | Continuous ->
        let vx = random_float (-2.) 2. in
        let vy = random_float (-8.) (-4.) in
        (vx, vy, random_float 2. 4., symbols.(Random.int (Array.length symbols)))
    | Fountain ->
        let vx = random_float (-3.) 3. in
        let vy = random_float (-15.) (-10.) in
        (vx, vy, random_float 1.5 3., "●")
    | Explosion ->
        let angle = Random.float (2. *. Float.pi) in
        let speed = random_float 10. 25. in
        let vx = speed *. Float.cos angle in
        let vy = speed *. Float.sin angle in
        ( vx,
          vy,
          random_float 0.5 1.5,
          spark_symbols.(Random.int (Array.length spark_symbols)) )
    | Firework ->
        let angle = random_float (-0.3) 0.3 -. (Float.pi /. 2.) in
        let speed = random_float 15. 25. in
        let vx = speed *. Float.cos angle in
        let vy = speed *. Float.sin angle in
        (vx, vy, random_float 0.8 1.2, "▲")
  in
  {
    x;
    y;
    vx;
    vy;
    life = max_life;
    max_life;
    symbol;
    color = color_for_life max_life max_life mode;
  }

let spawn_firework_burst ~x ~y =
  let count = 30 + Random.int 30 in
  List.init count (fun _ ->
      let angle = Random.float (2. *. Float.pi) in
      let speed = random_float 5. 15. in
      let vx = speed *. Float.cos angle in
      let vy = speed *. Float.sin angle in
      let max_life = random_float 1. 2. in
      {
        x;
        y;
        vx;
        vy;
        life = max_life;
        max_life;
        symbol = symbols.(Random.int (Array.length symbols));
        color = color_for_life max_life max_life Firework;
      })

let update_particle state (p : particle) dt =
  p.x <- p.x +. (p.vx *. dt);
  p.y <- p.y +. (p.vy *. dt);
  p.vy <- p.vy +. (state.gravity *. dt);
  p.life <- p.life -. dt;
  p.color <- color_for_life p.life p.max_life state.current_mode

let update_particles state dt =
  if state.paused then ()
  else (
    List.iter (fun p -> update_particle state p dt) state.particles;
    let firework_bursts =
      List.filter_map
        (fun (p : particle) ->
          if
            state.current_mode = Firework
            && p.life <= 0. && p.symbol = "▲" && p.vy > 0.
          then Some (spawn_firework_burst ~x:p.x ~y:p.y)
          else None)
        state.particles
    in
    state.particles <-
      List.filter (fun p -> p.life > 0.) state.particles
      @ List.concat firework_bursts)

let spawn_from_emitters state dt =
  if state.paused then ()
  else (
    state.time_acc <- state.time_acc +. dt;
    let spawn_interval = 1. /. state.spawn_rate in
    while state.time_acc >= spawn_interval do
      state.time_acc <- state.time_acc -. spawn_interval;
      List.iter
        (fun e ->
          if e.active then
            let p = spawn_particle ~x:e.x ~y:e.y ~mode:e.mode in
            state.particles <- p :: state.particles)
        state.emitters;
      if state.mouse_emitter then
        let p =
          spawn_particle
            ~x:(Float.of_int state.mouse_x)
            ~y:(Float.of_int state.mouse_y)
            ~mode:state.current_mode
        in
        state.particles <- p :: state.particles
    done)

let draw_particle grid (p : particle) =
  let ix = int_of_float p.x in
  let iy = int_of_float p.y in
  let alpha = p.life /. p.max_life in
  let r, g, b, _ = Color.to_rgba p.color in
  let color = Color.of_rgba r g b (int_of_float (alpha *. 255.)) in
  let style = Style.make ~fg:color () in
  Grid.draw_text ~style grid ~x:ix ~y:iy ~text:p.symbol

let draw_particles grid state =
  List.iter (fun p -> draw_particle grid p) state.particles

let draw_ui grid state ~rows =
  let bg = Color.of_rgb 20 20 30 in
  let fg = Color.of_rgb 200 200 220 in
  let highlight = Color.of_rgb 100 200 255 in
  let style = Style.make ~fg ~bg () in
  let hl_style = Style.make ~fg:highlight ~bg ~bold:true () in
  for x = 0 to Grid.width grid - 1 do
    Grid.draw_text ~style grid ~x ~y:0 ~text:" ";
    Grid.draw_text ~style grid ~x ~y:(rows - 1) ~text:" ";
    Grid.draw_text ~style grid ~x ~y:(rows - 2) ~text:" "
  done;
  Grid.draw_text ~style:hl_style grid ~x:1 ~y:0 ~text:"Matrix Particle System";
  let mode_name =
    match state.current_mode with
    | Burst -> "Burst"
    | Continuous -> "Rainbow"
    | Fountain -> "Fountain"
    | Explosion -> "Explosion"
    | Firework -> "Firework"
  in
  let status =
    Printf.sprintf "Mode: %s | Particles: %d | Gravity: %.1f | Rate: %.0f/s"
      mode_name
      (List.length state.particles)
      state.gravity state.spawn_rate
  in
  Grid.draw_text ~style grid ~x:1 ~y:(rows - 2) ~text:status;
  let controls =
    "1-5: Mode | G/H: Gravity | +/-: Rate | Space: Burst | Click: Emitter | P: \
     Pause | Q: Quit"
  in
  Grid.draw_text ~style grid ~x:1 ~y:(rows - 1) ~text:controls

let burst_at state ~x ~y ~count =
  let new_particles =
    List.init count (fun _ ->
        spawn_particle ~x:(Float.of_int x) ~y:(Float.of_int y)
          ~mode:state.current_mode)
  in
  state.particles <- new_particles @ state.particles

let handle_input state event =
  match event with
  | Input.Key { key = Input.Key.Escape; _ } -> `Quit
  | Input.Key { key = Input.Key.Char u; _ } -> (
      let c = Uchar.to_int u in
      match Char.chr c with
      | 'q' | 'Q' -> `Quit
      | '1' ->
          state.current_mode <- Burst;
          `Continue
      | '2' ->
          state.current_mode <- Continuous;
          `Continue
      | '3' ->
          state.current_mode <- Fountain;
          `Continue
      | '4' ->
          state.current_mode <- Explosion;
          `Continue
      | '5' ->
          state.current_mode <- Firework;
          `Continue
      | 'g' ->
          state.gravity <- state.gravity +. 5.;
          `Continue
      | 'h' ->
          state.gravity <- state.gravity -. 5.;
          `Continue
      | '+' | '=' ->
          state.spawn_rate <- Float.min 200. (state.spawn_rate +. 10.);
          `Continue
      | '-' | '_' ->
          state.spawn_rate <- Float.max 1. (state.spawn_rate -. 10.);
          `Continue
      | ' ' ->
          burst_at state ~x:state.mouse_x ~y:state.mouse_y ~count:50;
          `Continue
      | 'p' | 'P' ->
          state.paused <- not state.paused;
          `Continue
      | 'c' | 'C' ->
          state.particles <- [];
          `Continue
      | _ -> `Continue)
  | Input.Mouse (Input.Mouse.Button_press (x, y, Input.Mouse.Left, _)) ->
      state.mouse_x <- x;
      state.mouse_y <- y;
      state.mouse_emitter <- true;
      burst_at state ~x ~y ~count:30;
      `Continue
  | Input.Mouse (Input.Mouse.Button_release (_, _, Input.Mouse.Left, _)) ->
      state.mouse_emitter <- false;
      `Continue
  | Input.Mouse (Input.Mouse.Motion (x, y, btns, _)) ->
      state.mouse_x <- x;
      state.mouse_y <- y;
      state.mouse_emitter <- btns.Input.Mouse.left;
      `Continue
  | _ -> `Continue

let create_state ~cols ~rows =
  let center_x = Float.of_int (cols / 2) in
  let center_y = Float.of_int (rows / 2) in
  {
    particles = [];
    emitters =
      [ { x = center_x; y = center_y; mode = Continuous; active = true } ];
    current_mode = Continuous;
    gravity = 15.;
    spawn_rate = 30.;
    time_acc = 0.;
    mouse_x = cols / 2;
    mouse_y = rows / 2;
    mouse_emitter = false;
    paused = false;
  }

let () =
  Random.self_init ();
  let app =
    Matrix.create ~target_fps:(Some 60.) ~mouse_enabled:true ~debug_overlay:true
      ()
  in
  let state = create_state ~cols:1 ~rows:1 in
  Matrix.run app
    ~on_frame:(fun _ ~dt ->
      let dt = Float.min dt 0.1 in
      spawn_from_emitters state dt;
      update_particles state dt)
    ~on_input:(fun app event ->
      match handle_input state event with
      | `Quit -> Matrix.stop app
      | `Continue -> ())
    ~on_resize:(fun _ ~cols ~rows ->
      List.iter
        (fun e ->
          let e' =
            { e with x = Float.of_int (cols / 2); y = Float.of_int (rows / 2) }
          in
          ignore e')
        state.emitters)
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let _, rows = Matrix.size app in
      Grid.clear ~color:(Color.of_rgb 10 10 20) grid;
      draw_particles grid state;
      draw_ui grid state ~rows)
