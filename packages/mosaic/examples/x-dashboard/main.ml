(** ML Dashboard demo: a "terminal TensorBoard" style UI. Shows: non-trivial
    layout, scrollboxes, text inputs, tables, code highlight, and multiple
    charts via Matrix_charts on a canvas. *)

open Mosaic
module Charts = Matrix_charts

(* ---------- Small utils ---------- *)

let clamp lo hi x = if x < lo then lo else if x > hi then hi else x

let rec take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl

let string_contains ~needle hay =
  if needle = "" then true
  else
    let hay = String.lowercase_ascii hay in
    let needle = String.lowercase_ascii needle in
    let n = String.length needle in
    let m = String.length hay in
    let rec loop i =
      if i + n > m then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

(* Deterministic "noise" (0..1) from seed + step. *)
let frac x = x -. Float.floor x

let noise seed step =
  frac (Float.sin (Float.of_int (step + (seed * 7919)) *. 12.9898) *. 43758.5453)

type point = { x : float; y : float }

let smooth ~window (pts : point list) : point list =
  if window <= 1 then pts
  else
    let ys = Array.of_list (List.map (fun p -> p.y) pts) in
    let xs = Array.of_list (List.map (fun p -> p.x) pts) in
    let n = Array.length ys in
    let out = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let j0 = max 0 (i - window + 1) in
      let sum = ref 0.0 in
      for j = j0 to i do
        sum := !sum +. ys.(j)
      done;
      out.(i) <- !sum /. Float.of_int (i - j0 + 1)
    done;
    let rec build i acc =
      if i < 0 then acc else build (i - 1) ({ x = xs.(i); y = out.(i) } :: acc)
    in
    build (n - 1) []

(* ---------- Domain ---------- *)

type status = Running | Finished | Crashed

let status_label = function
  | Running -> "RUNNING"
  | Finished -> "FINISHED"
  | Crashed -> "CRASHED"

let status_color = function
  | Running -> Ansi.Color.green
  | Finished -> Ansi.Color.cyan
  | Crashed -> Ansi.Color.red

type tab = Scalars | Heatmap | Embeddings | Logs | Config | Help

let all_tabs = [ Scalars; Heatmap; Embeddings; Logs; Config; Help ]

let tab_label = function
  | Scalars -> "Scalars"
  | Heatmap -> "Heatmap"
  | Embeddings -> "Embeddings"
  | Logs -> "Logs"
  | Config -> "Config"
  | Help -> "Help"

let tab_hint = function
  | Scalars -> "loss/acc/lr"
  | Heatmap -> "confusion matrix"
  | Embeddings -> "2D scatter"
  | Logs -> "streamed output"
  | Config -> "hparams + json"
  | Help -> "readme"

let next_in_cycle xs x =
  let rec go = function
    | [] -> x
    | [ y ] -> if y = x then x else y
    | y :: (z :: _ as tl) -> if y = x then z else go tl
  in
  if List.mem x xs then go xs else List.hd xs

let prev_in_cycle xs x =
  let rec last = function [] -> x | [ y ] -> y | _ :: tl -> last tl in
  let rec find_prev prev = function
    | [] -> prev
    | y :: tl -> if y = x then prev else find_prev y tl
  in
  if not (List.mem x xs) then List.hd xs else find_prev (last xs) xs

type run = {
  id : string;
  name : string;
  seed : int;
  status : status;
  step : int;
  loss : point list; (* newest-first *)
  val_loss : point list; (* newest-first *)
  acc : point list; (* newest-first *)
  lr : point list; (* newest-first *)
  logs : string list; (* newest-first *)
  hparams : (string * string) list;
}

let max_points = 450
let max_logs = 350

let loss_of seed step =
  let s = Float.of_int step in
  let base = (2.4 *. exp (-.s /. 220.0)) +. (0.08 *. sin (s /. 7.0)) in
  let n = (noise seed step -. 0.5) *. 0.16 in
  max 0.03 (base +. n)

let val_loss_of seed step =
  let s = Float.of_int step in
  let base = (2.7 *. exp (-.s /. 250.0)) +. (0.10 *. sin (s /. 9.0)) in
  let n = (noise (seed + 7) step -. 0.5) *. 0.18 in
  max 0.03 (base +. 0.15 +. n)

let acc_of seed step =
  let s = Float.of_int step in
  clamp 0.0 1.0
    (0.18
    +. (0.82 *. (1.0 -. exp (-.s /. 260.0)))
    +. ((noise (seed + 13) step -. 0.5) *. 0.06)
    +. (0.02 *. sin (s /. 13.0)))

let lr_of seed step =
  let s = Float.of_int step in
  let cyc = 0.5 *. (1.0 +. cos ((s /. 60.0) +. Float.of_int seed)) in
  0.0002 +. (0.0008 *. cyc)

let cons_take n x xs = take n (x :: xs)

let mk_log_line run ~step ~loss ~vloss ~acc ~lr =
  let phase =
    if step mod 97 = 0 then "eval"
    else if step mod 53 = 0 then "ckpt"
    else "train"
  in
  Printf.sprintf "[%s] %s/%s step=%05d loss=%.3f val=%.3f acc=%.3f lr=%.4f"
    run.id run.name phase step loss vloss acc (lr *. 1000.)

let add_step_to_run (r : run) : run =
  match r.status with
  | Finished | Crashed -> r
  | Running ->
      let step = r.step + 1 in
      let loss = loss_of r.seed step in
      let vloss = val_loss_of r.seed step in
      let acc = acc_of r.seed step in
      let lr = lr_of r.seed step in
      let p_loss = { x = Float.of_int step; y = loss } in
      let p_vloss = { x = Float.of_int step; y = vloss } in
      let p_acc = { x = Float.of_int step; y = acc } in
      let p_lr = { x = Float.of_int step; y = lr *. 1000. } in
      let logs =
        cons_take max_logs (mk_log_line r ~step ~loss ~vloss ~acc ~lr) r.logs
      in
      let status =
        if step > 1600 then Finished
        else if step mod 777 = 0 && noise r.seed step > 0.92 then Crashed
        else Running
      in
      {
        r with
        step;
        status;
        loss = cons_take max_points p_loss r.loss;
        val_loss = cons_take max_points p_vloss r.val_loss;
        acc = cons_take max_points p_acc r.acc;
        lr = cons_take max_points p_lr r.lr;
        logs;
      }

let gen_series ~n f =
  let rec loop i acc =
    if i >= n then acc else loop (i + 1) ({ x = Float.of_int i; y = f i } :: acc)
  in
  loop 0 []

let mk_run ~id ~name ~seed ~status ~n0 : run =
  let step = max 0 (n0 - 1) in
  let loss = gen_series ~n:n0 (loss_of seed) in
  let vloss = gen_series ~n:n0 (val_loss_of seed) in
  let acc = gen_series ~n:n0 (acc_of seed) in
  let lr = gen_series ~n:n0 (fun i -> lr_of seed i *. 1000.) in
  let logs =
    List.init (min 40 n0) (fun i ->
        let s = max 0 (step - min 40 n0 + i + 1) in
        let loss = loss_of seed s in
        let vloss = val_loss_of seed s in
        let acc = acc_of seed s in
        let lr = lr_of seed s in
        mk_log_line
          {
            id;
            name;
            seed;
            status;
            step;
            loss = [];
            val_loss = [];
            acc = [];
            lr = [];
            logs = [];
            hparams = [];
          }
          ~step:s ~loss ~vloss ~acc ~lr)
    |> List.rev (* newest-first *)
  in
  let hparams =
    [
      ("optimizer", "adamw");
      ("lr", "1e-3");
      ("batch_size", "64");
      ("weight_decay", "0.01");
      ("model", "resnet18");
      ("dataset", "cifar10");
      ("augment", "randaugment");
      ("seed", string_of_int seed);
    ]
  in
  {
    id;
    name;
    seed;
    status;
    step;
    loss;
    val_loss = vloss;
    acc;
    lr;
    logs;
    hparams;
  }

type focus =
  | No_focus
  | Focus_run_filter
  | Focus_log_filter
  | Focus_scalar_view
  | Focus_config

type sys = {
  cpu : float;
  mem : float;
  gpu : float;
  net : float;
  temp : float;
  top : (string * float) list;
  s_cpu : float list; (* newest first *)
  s_mem : float list;
  s_gpu : float list;
  s_net : float list;
}

type scalar_view = All_scalars | Training | Validation | Custom

type model = {
  time : float;
  sample_acc : float;
  streaming : bool;
  tab : tab;
  focus : focus;
  run_filter : Editor.t;
  log_filter : Editor.t;
  show_grid : bool;
  show_axes : bool;
  smoothing : int;
  window : int;
  selected_run : string;
  runs : run list;
  sys : sys;
  scrolls : (string * Scroll.t) list;
  scalar_view : scalar_view option;
  config_row : string option;
}

type msg =
  | Quit
  | Tick of float
  | Toggle_stream
  | Next_tab
  | Prev_tab
  | Set_tab of tab
  | Next_run
  | Prev_run
  | Select_run of string
  | Focus of focus
  | Blur_focus
  | Run_filter of Editor.t
  | Log_filter of Editor.t
  | Scrolled of string * Scroll.t
  | Scalar_view of scalar_view
  | Config_row of string
  | Smoothing of float
  | Toggle_grid
  | Toggle_axes
  | Smoothing_up
  | Smoothing_down
  | Cycle_window
  | Reset_selected

(* ---------- Init ---------- *)

let make_sys () =
  {
    cpu = 0.;
    mem = 0.;
    gpu = 0.;
    net = 0.;
    temp = 0.;
    top = [];
    s_cpu = [];
    s_mem = [];
    s_gpu = [];
    s_net = [];
  }

let init () =
  let runs =
    [
      mk_run ~id:"run-a12f" ~name:"resnet18" ~seed:1 ~status:Running ~n0:120;
      mk_run ~id:"run-b07c" ~name:"vit-tiny" ~seed:2 ~status:Running ~n0:80;
      mk_run ~id:"run-c91e" ~name:"mlp-mixer" ~seed:3 ~status:Finished ~n0:540;
      mk_run ~id:"run-d33a" ~name:"unet" ~seed:4 ~status:Crashed ~n0:260;
      mk_run ~id:"run-e2d9" ~name:"llama-mini" ~seed:5 ~status:Finished ~n0:900;
      mk_run ~id:"run-f8aa" ~name:"convnext" ~seed:6 ~status:Finished ~n0:700;
      mk_run ~id:"run-g5b0" ~name:"gru" ~seed:7 ~status:Running ~n0:40;
      mk_run ~id:"run-h19b" ~name:"transformer" ~seed:8 ~status:Finished
        ~n0:1000;
    ]
  in
  let selected_run = (List.hd runs).id in
  ( {
      time = 0.;
      sample_acc = 0.;
      streaming = true;
      tab = Scalars;
      focus = No_focus;
      run_filter = Editor.empty;
      log_filter = Editor.empty;
      show_grid = true;
      show_axes = true;
      smoothing = 8;
      window = 240;
      selected_run;
      runs;
      sys = make_sys ();
      scrolls = [];
      scalar_view = Some All_scalars;
      config_row =
        (match (List.hd runs).hparams with
        | (key, _) :: _ -> Some key
        | [] -> None);
    },
    Cmd.none )

(* ---------- Update helpers ---------- *)

let visible_runs (m : model) =
  let needle = Editor.text m.run_filter in
  List.filter
    (fun r -> string_contains ~needle r.id || string_contains ~needle r.name)
    m.runs

let select_adjacent_run (m : model) ~dir =
  let runs = visible_runs m in
  match runs with
  | [] -> m
  | _ ->
      let ids = List.map (fun r -> r.id) runs in
      let next =
        if dir > 0 then next_in_cycle ids m.selected_run
        else prev_in_cycle ids m.selected_run
      in
      { m with selected_run = next }

let update_sys (s : sys) : sys =
  let push v l = take 30 (v :: l) in
  {
    s with
    s_cpu = push s.cpu s.s_cpu;
    s_mem = push s.mem s.s_mem;
    s_gpu = push s.gpu s.s_gpu;
    s_net = push s.net s.s_net;
  }

let refresh_sys (m : model) : model =
  let t = m.time in
  let cpu =
    clamp 0.0 100.0
      (45.0 +. (25.0 *. sin (t *. 0.72)) +. (10.0 *. sin (t *. 1.9)))
  in
  let mem =
    clamp 0.0 100.0
      (62.0 +. (12.0 *. sin (t *. 0.31)) +. (6.0 *. cos (t *. 0.9)))
  in
  let gpu =
    clamp 0.0 100.0 (35.0 +. (35.0 *. (0.5 +. (0.5 *. sin (t *. 0.55)))))
  in
  let net =
    clamp 0.0 100.0 (15.0 +. (45.0 *. (0.5 +. (0.5 *. sin ((t *. 1.2) +. 0.8)))))
  in
  let temp =
    clamp 30.0 95.0 (55.0 +. (20.0 *. (0.5 +. (0.5 *. sin ((t *. 0.4) +. 1.3)))))
  in

  let top =
    [
      ("train.py", clamp 0.0 100.0 (cpu *. 0.62));
      ("dataloader", clamp 0.0 100.0 (cpu *. 0.18));
      ("logger", clamp 0.0 100.0 (cpu *. 0.07));
      ("tensor-ops", clamp 0.0 100.0 (gpu *. 0.55));
      ("render", clamp 0.0 100.0 (gpu *. 0.12));
      ("sysmon", clamp 0.0 100.0 (cpu *. 0.04));
      ("zstd", clamp 0.0 100.0 (net *. 0.15));
    ]
  in

  { m with sys = update_sys { m.sys with cpu; mem; gpu; net; temp; top } }

let update_runs_if_streaming (m : model) : model =
  if not m.streaming then m
  else
    let runs =
      List.map
        (fun r -> if r.status = Running then add_step_to_run r else r)
        m.runs
    in
    { m with runs }

let get_selected_run (m : model) : run option =
  List.find_opt (fun r -> r.id = m.selected_run) m.runs

let reset_selected_run (m : model) : model =
  match get_selected_run m with
  | None -> m
  | Some r ->
      let fresh =
        mk_run ~id:r.id ~name:r.name ~seed:r.seed ~status:Running ~n0:20
      in
      let runs = List.map (fun x -> if x.id = r.id then fresh else x) m.runs in
      { m with runs }

let set_tab m tab =
  if tab = m.tab then m
  else
    let focus =
      match m.focus with
      | Focus_scalar_view | Focus_config -> No_focus
      | focus -> focus
    in
    { m with tab; focus }

(* ---------- Update ---------- *)

let update msg m =
  match msg with
  | Quit -> (m, Cmd.quit)
  | Tick dt ->
      (* sample at ~10Hz regardless of actual tick rate *)
      let time = m.time +. dt in
      let sample_acc = m.sample_acc +. dt in
      let m = { m with time; sample_acc } in
      if m.sample_acc < 0.10 then (m, Cmd.none)
      else
        let m =
          { m with sample_acc = 0.0 } |> refresh_sys |> update_runs_if_streaming
        in
        (m, Cmd.none)
  | Toggle_stream -> ({ m with streaming = not m.streaming }, Cmd.none)
  | Next_tab -> (set_tab m (next_in_cycle all_tabs m.tab), Cmd.none)
  | Prev_tab -> (set_tab m (prev_in_cycle all_tabs m.tab), Cmd.none)
  | Set_tab tab -> (set_tab m tab, Cmd.none)
  | Next_run -> (select_adjacent_run m ~dir:1, Cmd.none)
  | Prev_run -> (select_adjacent_run m ~dir:(-1), Cmd.none)
  | Select_run id -> ({ m with selected_run = id }, Cmd.none)
  | Focus f -> ({ m with focus = f }, Cmd.none)
  | Blur_focus -> ({ m with focus = No_focus }, Cmd.none)
  | Run_filter run_filter -> ({ m with run_filter }, Cmd.none)
  | Log_filter log_filter -> ({ m with log_filter }, Cmd.none)
  | Scrolled (id, scroll) ->
      ( { m with scrolls = (id, scroll) :: List.remove_assoc id m.scrolls },
        Cmd.none )
  | Scalar_view scalar_view ->
      ({ m with scalar_view = Some scalar_view }, Cmd.none)
  | Config_row config_row -> ({ m with config_row = Some config_row }, Cmd.none)
  | Smoothing value -> ({ m with smoothing = int_of_float value }, Cmd.none)
  | Toggle_grid -> ({ m with show_grid = not m.show_grid }, Cmd.none)
  | Toggle_axes -> ({ m with show_axes = not m.show_axes }, Cmd.none)
  | Smoothing_up -> ({ m with smoothing = min 50 (m.smoothing + 2) }, Cmd.none)
  | Smoothing_down -> ({ m with smoothing = max 1 (m.smoothing - 2) }, Cmd.none)
  | Cycle_window ->
      let window =
        match m.window with 120 -> 240 | 240 -> 360 | 360 -> 480 | _ -> 120
      in
      ({ m with window }, Cmd.none)
  | Reset_selected -> (reset_selected_run m, Cmd.none)

let scroller m id =
  scroll_box
    ~scroll:(Option.value (List.assoc_opt id m.scrolls) ~default:(Scroll.at 0))
    ~on_scroll:(fun value -> Scrolled (id, value))

(* ---------- Charts ---------- *)

let theme =
  Charts.Theme.
    {
      dark with
      axis = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:9) ();
      grid = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:3) ~dim:true ();
    }

(* A canvas that lays [fig] out at its size and draws it. *)
let chart ?size fig =
  canvas ?size (fun c ~dt:_ ->
      Canvas.clear c;
      let width = Canvas.width c and height = Canvas.height c in
      Charts.draw
        (Charts.layout ~theme ~width ~height fig)
        (Canvas.grid c) ~x:0 ~y:0)

let points_recent ~window (newest_first : point list) =
  newest_first |> take window |> List.rev

let x_of p = p.x
let y_of p = p.y

let axes ?format (m : model) =
  let grid = if m.show_grid then `Ticks else `None in
  Charts.
    [
      axis ~show:m.show_axes ~grid "x"; axis ~show:m.show_axes ~grid ?format "y";
    ]

let loss_plot (m : model) (r : run) =
  let loss =
    points_recent ~window:m.window r.loss
    |> smooth ~window:m.smoothing |> Array.of_list
  in
  let vloss =
    points_recent ~window:m.window r.val_loss
    |> smooth ~window:m.smoothing |> Array.of_list
  in
  Charts.(
    layer
      ([
         line ~color:(const Ansi.Color.cyan) ~x:x_of ~y:y_of loss;
         line ~color:(const Ansi.Color.magenta) ~x:x_of ~y:y_of vloss;
         hline ~color:(Ansi.Color.grayscale ~level:6) 1.0 |> attrs Ansi.Attr.dim;
       ]
      @ axes m))

let acc_lr_plot (m : model) (r : run) =
  let acc =
    points_recent ~window:m.window r.acc
    |> smooth ~window:(max 1 (m.smoothing / 2))
    |> Array.of_list
  in
  let lr = points_recent ~window:m.window r.lr |> Array.of_list in
  Charts.(
    layer
      ([
         line ~color:(const Ansi.Color.green) ~x:x_of ~y:y_of acc;
         line ~color:(const Ansi.Color.yellow) ~x:x_of ~y:y_of lr;
       ]
      @ axes m ~format:(fun _ v -> Printf.sprintf "%.1f" v)))

let confusion_points seed =
  (* 8x8 confusion-ish matrix *)
  let n = 8 in
  Array.concat
    (List.map Array.of_list
       (List.init n (fun y ->
            List.init n (fun x ->
                let base = if x = y then 0.8 else 0.15 in
                let wobble =
                  (noise (seed + x + (y * 17)) ((x * 19) + (y * 23)) -. 0.5)
                  *. 0.25
                in
                let v = clamp 0.0 1.0 (base +. wobble) in
                (Float.of_int x, Float.of_int y, v)))))

let confusion_plot (r : run) =
  let whole _ v = string_of_int (int_of_float v) in
  Charts.(
    layer
      [
        raster ~reduce:`Mean
          ~x:(fun (x, _, _) -> x)
          ~y:(fun (_, y, _) -> y)
          ~value:(fun (_, _, v) -> v)
          (confusion_points r.seed);
        axis ~format:whole "x";
        axis ~format:whole "y";
      ])

type embed = { ex : float; ey : float }

let embeddings seed =
  let n = 260 in
  Array.init n (fun i ->
      let t = Float.of_int i /. Float.of_int n *. (Float.pi *. 2.0) in
      let wob = (noise seed (i * 31) -. 0.5) *. 0.35 in
      let r = 0.9 +. wob in
      {
        ex = r *. cos (t +. (Float.of_int seed *. 0.2));
        ey = r *. sin (t *. 1.1);
      })

let embeddings_plot (r : run) =
  Charts.(
    layer
      [
        dot ~dots:`Braille ~color:(const Ansi.Color.cyan)
          ~x:(fun p -> p.ex)
          ~y:(fun p -> p.ey)
          (embeddings r.seed);
        axis ~grid:`Ticks "x";
        axis ~grid:`Ticks "y";
      ])

(* ---------- View building blocks ---------- *)

let header_style = Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ()
let subheader_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) ()
let dim_style = Ansi.Style.make ~dim:true ()
let muted_bg = Ansi.Color.grayscale ~level:3

let pill ~label ~color =
  box ~padding:(padding 0) ~background:color
    [
      text
        ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.black ())
        (Printf.sprintf " %s " label);
    ]

let tab_button (m : model) tab =
  let selected = m.tab = tab in
  let bg = if selected then Ansi.Color.blue else Ansi.Color.transparent in
  let fg = if selected then Ansi.Color.white else Ansi.Color.bright_white in
  box ~border:true ~padding:(padding 1) ~background:bg
    ~on_mouse:(fun ev ->
      match Event.Mouse.kind ev with Down _ -> Some (Set_tab tab) | _ -> None)
    [
      box ~flex_direction:Column
        [
          text ~style:(Ansi.Style.make ~bold:true ~fg ()) (tab_label tab);
          text
            ~style:
              (Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:9) ~dim:true ())
            (tab_hint tab);
        ];
    ]

let run_row (m : model) (r : run) =
  let selected = r.id = m.selected_run in
  let bg = if selected then Ansi.Color.blue else Ansi.Color.transparent in
  let fg = if selected then Ansi.Color.white else Ansi.Color.bright_white in
  box ~padding:(padding 1) ~background:bg ~key:r.id
    ~on_mouse:(fun ev ->
      match Event.Mouse.kind ev with
      | Down _ -> Some (Select_run r.id)
      | _ -> None)
    [
      box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
        [
          text ~style:(Ansi.Style.make ~fg:(status_color r.status) ()) "●";
          text ~style:(Ansi.Style.make ~bold:true ~fg ()) r.name;
          text
            ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) ())
            (Printf.sprintf "#%d" r.step);
          box ~flex_grow:1. [];
          text
            ~style:
              (Ansi.Style.make
                 ~fg:
                   (if selected then Ansi.Color.white else status_color r.status)
                 ~dim:(r.status <> Running) ())
            (status_label r.status);
        ];
    ]

let run_panel (m : model) =
  let focused = m.focus = Focus_run_filter in
  let runs = visible_runs m in
  box ~border:true ~padding:(padding 1)
    ~size:{ width = px 32; height = pct 100 }
    ~title:"Runs"
    [
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = pct 100 }
        [
          (* filter input *)
          box ~border:true ~padding:(padding 1)
            ~background:(if focused then muted_bg else Ansi.Color.transparent)
            [
              box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                [
                  text ~style:dim_style "Filter:";
                  input
                    ~size:{ width = pct 100; height = px 1 }
                    ~placeholder:"run id or model..." ~cursor_blinking:focused
                    ~state:m.run_filter ~focused
                    ~on_change:(fun state -> Run_filter state)
                    ~on_focus_request:(Focus Focus_run_filter) ();
                ];
            ];
          (* run list *)
          box ~flex_grow:1. ~border:true
            ~title:(Printf.sprintf "Visible: %d" (List.length runs))
            [
              scroller m "runs"
                ~size:{ width = pct 100; height = pct 100 }
                ~scroll_y:true ~scroll_x:false
                (List.map (run_row m) runs);
            ];
          (* quick actions *)
          box ~flex_direction:Row ~gap:(gap 1)
            [
              box ~border:true ~padding:(padding 1)
                ~on_mouse:(fun ev ->
                  match Event.Mouse.kind ev with
                  | Down _ -> Some Toggle_stream
                  | _ -> None)
                [
                  text
                    ~style:(Ansi.Style.make ~bold:true ())
                    (if m.streaming then "Pause" else "Resume");
                ];
              box ~border:true ~padding:(padding 1)
                ~on_mouse:(fun ev ->
                  match Event.Mouse.kind ev with
                  | Down _ -> Some Reset_selected
                  | _ -> None)
                [
                  text
                    ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                    "Reset run";
                ];
            ];
        ];
    ]

let sys_metric_card ~title ~value ~unit_ ~color ~spark =
  box ~border:true ~padding:(padding 1) ~title
    [
      box ~flex_direction:Column ~gap:(gap 1)
        [
          text
            ~style:(Ansi.Style.make ~bold:true ~fg:color ())
            (Printf.sprintf "%5.1f%s" value unit_);
          chart
            ~size:{ width = pct 100; height = px 4 }
            (Charts.spark ~color spark
            |> Charts.scale "y" (Charts.Scale.linear ~domain:(0., 100.) ()));
        ];
    ]

let sys_panel (m : model) =
  box ~border:true ~padding:(padding 1)
    ~size:{ width = px 34; height = pct 100 }
    ~title:"System"
    [
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = pct 100 }
        [
          box ~flex_direction:Row ~justify_content:Space_between
            [
              text ~style:dim_style "Collector";
              box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                [
                  spinner ~live:m.streaming ~color:Ansi.Color.bright_white
                    ~frame_set:Spinner.dots ();
                  text
                    ~style:
                      (Ansi.Style.make ~bold:true
                         ~fg:
                           (if m.streaming then Ansi.Color.green
                            else Ansi.Color.yellow)
                         ())
                    (if m.streaming then "LIVE" else "PAUSED");
                ];
            ];
          box ~flex_direction:Row ~gap:(gap 1)
            [
              sys_metric_card ~title:"CPU" ~value:m.sys.cpu ~unit_:"%"
                ~color:Ansi.Color.cyan ~spark:m.sys.s_cpu;
              sys_metric_card ~title:"MEM" ~value:m.sys.mem ~unit_:"%"
                ~color:Ansi.Color.magenta ~spark:m.sys.s_mem;
            ];
          box ~flex_direction:Row ~gap:(gap 1)
            [
              sys_metric_card ~title:"GPU" ~value:m.sys.gpu ~unit_:"%"
                ~color:Ansi.Color.green ~spark:m.sys.s_gpu;
              sys_metric_card ~title:"NET" ~value:m.sys.net ~unit_:"%"
                ~color:Ansi.Color.yellow ~spark:m.sys.s_net;
            ];
          box ~border:true ~flex_grow:1.
            ~title:(Printf.sprintf "Temp: %.1f°C" m.sys.temp)
            [
              scroller m "system"
                ~size:{ width = pct 100; height = pct 100 }
                ~scroll_y:true ~scroll_x:false
                (List.mapi
                   (fun i (name, cpu) ->
                     box ~padding:(padding 1)
                       ~background:
                         (if i mod 2 = 0 then Ansi.Color.transparent
                          else muted_bg)
                       ~key:(string_of_int i)
                       [
                         box ~flex_direction:Row ~justify_content:Space_between
                           [
                             text name;
                             text
                               ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
                               (Printf.sprintf "%4.1f%%" cpu);
                           ];
                       ])
                   m.sys.top);
            ];
        ];
    ]

let summary_card ~title ~value ~style =
  box ~border:true ~padding:(padding 1) ~title [ text ~style value ]

let scalars_tab (m : model) (r : run) =
  let hidden_overflow : Overflow.t Mosaic.point =
    { x = Overflow.Hidden; y = Overflow.Hidden }
  in
  let latest xs = match xs with p :: _ -> p.y | [] -> 0.0 in
  let l = latest r.loss in
  let vl = latest r.val_loss in
  let a = latest r.acc in
  let lr = latest r.lr in
  box ~flex_direction:Column ~gap:(gap 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Row ~gap:(gap 1)
        [
          summary_card ~title:"Run"
            ~value:(Printf.sprintf "%s (%s)" r.name r.id)
            ~style:(Ansi.Style.make ~bold:true ());
          summary_card ~title:"Step" ~value:(string_of_int r.step)
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ());
          summary_card ~title:"loss" ~value:(Printf.sprintf "%.3f" l)
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ());
          summary_card ~title:"val_loss" ~value:(Printf.sprintf "%.3f" vl)
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ());
          summary_card ~title:"acc" ~value:(Printf.sprintf "%.3f" a)
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ());
          summary_card ~title:"lr(x1000)" ~value:(Printf.sprintf "%.3f" lr)
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ());
        ];
      box ~flex_direction:Row ~gap:(gap 1) ~flex_grow:1.
        ~min_size:{ width = px 0; height = px 0 }
        [
          (* left charts *)
          box ~flex_direction:Column ~gap:(gap 1) ~flex_grow:1.
            ~min_size:{ width = px 0; height = px 0 }
            [
              box ~border:true ~padding:(padding 1) ~flex_grow:1.
                ~title:"Loss (cyan) + Val Loss (magenta)"
                [
                  chart
                    ~size:{ width = pct 100; height = pct 100 }
                    (loss_plot m r);
                ];
              box ~border:true ~padding:(padding 1)
                ~size:{ width = pct 100; height = px 10 }
                ~title:"Accuracy (green) + LR x1000 (yellow)"
                [
                  chart
                    ~size:{ width = pct 100; height = pct 100 }
                    (acc_lr_plot m r);
                ];
            ];
          (* controls *)
          box ~border:true ~padding:(padding 1)
            ~size:{ width = px 32; height = pct 100 }
            ~min_size:{ width = px 0; height = px 0 }
            ~overflow:hidden_overflow ~title:"Controls"
            [
              scroller m "scalars"
                ~size:{ width = pct 100; height = pct 100 }
                ~scroll_y:true ~scroll_x:false
                [
                  box ~flex_direction:Column ~gap:(gap 1)
                    ~size:{ width = pct 100; height = auto }
                    [
                      text ~style:(Ansi.Style.make ~bold:true ()) "Charts";
                      box ~flex_direction:Row ~justify_content:Space_between
                        [
                          text (Printf.sprintf "Window: %d" m.window);
                          box ~border:true ~padding:(padding 1)
                            ~on_mouse:(fun ev ->
                              match Event.Mouse.kind ev with
                              | Down _ -> Some Cycle_window
                              | _ -> None)
                            [ text ~style:dim_style "Cycle (w)" ];
                        ];
                      box ~flex_direction:Row ~justify_content:Space_between
                        [
                          text (Printf.sprintf "Smoothing: %d" m.smoothing);
                          box ~flex_direction:Row ~gap:(gap 1)
                            [
                              box ~border:true ~padding:(padding 1)
                                ~on_mouse:(fun ev ->
                                  match Event.Mouse.kind ev with
                                  | Down _ -> Some Smoothing_down
                                  | _ -> None)
                                [
                                  text
                                    ~style:(Ansi.Style.make ~bold:true ())
                                    "-";
                                ];
                              box ~border:true ~padding:(padding 1)
                                ~on_mouse:(fun ev ->
                                  match Event.Mouse.kind ev with
                                  | Down _ -> Some Smoothing_up
                                  | _ -> None)
                                [
                                  text
                                    ~style:(Ansi.Style.make ~bold:true ())
                                    "+";
                                ];
                            ];
                        ];
                      slider
                        ~size:{ width = pct 100; height = px 1 }
                        ~orientation:`Horizontal ~min:1. ~max:50.
                        ~state:(Float.of_int m.smoothing)
                        ~on_change:(fun value -> Smoothing value)
                        ~viewport_size:10.
                        ~track_color:(Ansi.Color.grayscale ~level:5)
                        ~thumb_color:Ansi.Color.cyan ();
                      box ~flex_direction:Row ~gap:(gap 1)
                        [
                          box ~border:true ~padding:(padding 1)
                            ~on_mouse:(fun ev ->
                              match Event.Mouse.kind ev with
                              | Down _ -> Some Toggle_grid
                              | _ -> None)
                            [
                              text
                                (Printf.sprintf "[%s] Grid"
                                   (if m.show_grid then "x" else " "));
                            ];
                          box ~border:true ~padding:(padding 1)
                            ~on_mouse:(fun ev ->
                              match Event.Mouse.kind ev with
                              | Down _ -> Some Toggle_axes
                              | _ -> None)
                            [
                              text
                                (Printf.sprintf "[%s] Axes"
                                   (if m.show_axes then "x" else " "));
                            ];
                        ];
                      box ~border:true ~padding:(padding 1)
                        ~title:"(Bonus) Select widget"
                        [
                          (let opts =
                             [
                               Select.item All_scalars
                                 ~description:"loss/acc/lr" "All scalars";
                               Select.item Training ~description:"hide val"
                                 "Training only";
                               Select.item Validation ~description:"val/loss"
                                 "Validation only";
                               Select.item Custom ~description:"demo"
                                 "Custom view";
                             ]
                           in
                           select ~equal:( = ) ~selected:m.scalar_view
                             ~focused:(m.focus = Focus_scalar_view)
                             ~on_focus_request:(Focus Focus_scalar_view)
                             ~on_change:(fun value -> Scalar_view value)
                             ~size:{ width = pct 100; height = px 6 }
                             ~show_description:true ~show_scroll_indicator:true
                             ~wrap_selection:true
                             ~selected_background:Ansi.Color.blue
                             ~selected_text_color:Ansi.Color.white opts);
                        ];
                      text ~style:dim_style ~wrap:`Word
                        "Tip: / focuses run filter. ]/[ changes tab.";
                    ];
                ];
            ];
        ];
    ]

let heatmap_tab (_m : model) (r : run) =
  box ~flex_direction:Column ~gap:(gap 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Row ~gap:(gap 1) ~flex_grow:1.
        [
          box ~border:true ~padding:(padding 1) ~flex_grow:1.
            ~title:"Confusion Matrix (heatmap)"
            [
              chart
                ~size:{ width = pct 100; height = pct 100 }
                (confusion_plot r);
            ];
          box ~border:true ~padding:(padding 1)
            ~size:{ width = px 32; height = pct 100 }
            ~title:"Notes"
            [
              box ~flex_direction:Column ~gap:(gap 1)
                [
                  text ~style:(Ansi.Style.make ~bold:true ()) "Heatmap demo";
                  text ~wrap:`Word
                    "Use this panel to validate: heatmap aggregation, color \
                     scales, axes labels, and embedding multiple views in the \
                     same app.";
                  text ~style:dim_style ~wrap:`Word
                    "(In a real ML dashboard: attention maps, confusion \
                     matrices, per-layer activations.)";
                ];
            ];
        ];
    ]

let embeddings_tab (_m : model) (r : run) =
  box ~flex_direction:Column ~gap:(gap 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~border:true ~padding:(padding 1) ~flex_grow:1.
        ~title:"Embeddings (braille scatter)"
        [
          chart ~size:{ width = pct 100; height = pct 100 } (embeddings_plot r);
        ];
      box ~border:true ~padding:(padding 1)
        [
          text ~style:dim_style
            "This is a synthetic embedding ring. In a real app: points colored \
             by label, hover tooltips, selection + metadata inspector.";
        ];
    ]

let logs_tab (m : model) (r : run) =
  let focused = m.focus = Focus_log_filter in
  let logs =
    r.logs |> List.rev (* oldest-first *)
    |> List.filter (fun line ->
        string_contains ~needle:(Editor.text m.log_filter) line)
  in
  box ~flex_direction:Column ~gap:(gap 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~border:true ~padding:(padding 1)
        [
          box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
            [
              text ~style:dim_style "Log filter:";
              input
                ~size:{ width = pct 100; height = px 1 }
                ~placeholder:"substring..." ~cursor_blinking:focused
                ~state:m.log_filter ~focused
                ~on_change:(fun state -> Log_filter state)
                ~on_focus_request:(Focus Focus_log_filter) ();
              box ~border:true ~padding:(padding 1)
                ~on_mouse:(fun ev ->
                  match Event.Mouse.kind ev with
                  | Down _ -> Some (Focus Focus_log_filter)
                  | _ -> None)
                [
                  text ~style:dim_style (if focused then "Focused" else "Focus");
                ];
            ];
        ];
      box ~border:true ~flex_grow:1.
        ~title:(Printf.sprintf "Logs (%d)" (List.length logs))
        [
          scroller m "logs"
            ~size:{ width = pct 100; height = pct 100 }
            ~scroll_y:true ~scroll_x:false
            (List.mapi
               (fun i line ->
                 box ~padding:(padding 1)
                   ~background:
                     (if i mod 2 = 0 then Ansi.Color.transparent else muted_bg)
                   ~key:(string_of_int i)
                   [ text ~wrap:`Char line ])
               logs);
        ];
    ]

let config_json (r : run) =
  let hp =
    r.hparams
    |> List.map (fun (k, v) -> Printf.sprintf "    %S: %S" k v)
    |> String.concat ",\n"
  in
  Printf.sprintf
    "{\n\
    \  \"run_id\": %S,\n\
    \  \"name\": %S,\n\
    \  \"status\": %S,\n\
    \  \"step\": %d,\n\
    \  \"hparams\": {\n\
     %s\n\
    \  }\n\
     }\n"
    r.id r.name (status_label r.status) r.step hp

let config_tab (m : model) (r : run) =
  let hidden_overflow : Overflow.t Mosaic.point =
    { x = Overflow.Hidden; y = Overflow.Hidden }
  in
  let json = config_json r in
  let highlights = Toy_highlighter.highlight ~language:"json" ~content:json in
  let syntax =
    Code.syntax ~language:"json" ~style:Syntax_style.default highlights
  in
  let columns =
    [
      Table.column ~width:(`Fixed 16) ~alignment:`Left "Key";
      Table.column ~width:`Auto ~alignment:`Left "Value";
    ]
  in
  let rows =
    List.map
      (fun (k, v) -> Table.row k [| Table.cell k; Table.cell v |])
      r.hparams
  in
  box ~flex_direction:Row ~gap:(gap 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~border:true ~padding:(padding 1) ~overflow:hidden_overflow
        ~size:{ width = px 40; height = pct 100 }
        ~title:"Hyperparameters"
        [
          table ~equal:String.equal ~selected:m.config_row
            ~focused:(m.focus = Focus_config)
            ~on_focus_request:(Focus Focus_config)
            ~on_change:(fun value -> Config_row value)
            ~border:true ~border_style:Border.rounded
            ~min_size:{ width = px 0; height = px 0 }
            ~size:{ width = pct 100; height = pct 100 }
            ~columns ~rows ~show_header:true ~show_row_separator:true
            ~cell_padding:1
            ~row_styles:[ Ansi.Style.default; Ansi.Style.make ~bg:muted_bg () ]
            ();
        ];
      box ~border:true ~padding:(padding 1) ~flex_grow:1.
        ~title:"config.json (syntax highlighted)"
        [ code ~size:{ width = pct 100; height = pct 100 } ~syntax json ];
    ]

let help_markdown =
  {|# Mosaic ML Dashboard Demo

This is a single executable that tries to feel like a "terminal TensorBoard".

## Layout

- **Left:** runs list + run filter
- **Center:** tabbed main content (scalars, heatmap, embeddings, logs, config)
- **Right:** system metrics panel w/ sparklines + top processes

## Controls

- `j`/`k`: select run
- `[`/`]`: change tab
- `Space`: pause/resume streaming
- `/`: focus run filter
- `g`: toggle grid
- `a`: toggle axes
- `s`/`S`: smoothing down/up
- `w`: cycle x-window

## Why this exists

We want to validate that Mosaic can support a rich ML dashboard in the terminal:
graphs, scrolling, inputs, composition, and consistent styling.

Next steps for a real product:
- real metrics transport (files, sockets, grpc)
- tag browser + grouping
- pan/zoom on charts + cursor hover
- artifacts browser (images/audio/text)
- multi-run overlays + comparisons
|}

let help_tab m =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_grow:1. ~border:true ~padding:(padding 1) ~title:"Help"
        [
          scroller m "help"
            ~size:{ width = pct 100; height = pct 100 }
            ~scroll_y:true ~scroll_x:false
            [ markdown help_markdown ];
        ];
    ]

let main_panel (m : model) =
  let r = get_selected_run m in
  let r = match r with Some r -> r | None -> List.hd m.runs in
  let content =
    match m.tab with
    | Scalars -> scalars_tab m r
    | Heatmap -> heatmap_tab m r
    | Embeddings -> embeddings_tab m r
    | Logs -> logs_tab m r
    | Config -> config_tab m r
    | Help -> help_tab m
  in
  box ~border:true ~padding:(padding 1)
    ~size:{ width = pct 100; height = pct 100 }
    ~title:(Printf.sprintf "%s — %s" (tab_label m.tab) r.id)
    [ content ]

let header (m : model) =
  let sel = get_selected_run m in
  let run_line =
    match sel with
    | None -> "No run selected"
    | Some r ->
        Printf.sprintf "%s  |  %s  |  step %d" r.name (status_label r.status)
          r.step
  in
  box ~padding:(padding 1) ~background:Ansi.Color.blue
    [
      box ~flex_direction:Row ~justify_content:Space_between ~align_items:Center
        [
          box ~flex_direction:Row ~gap:(gap 2) ~align_items:Center
            [
              text ~style:header_style "Mosaic ML Dashboard";
              text ~style:subheader_style run_line;
            ];
          box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
            [
              pill
                ~label:(Printf.sprintf "tab: %s" (tab_label m.tab))
                ~color:(Ansi.Color.grayscale ~level:10);
              pill
                ~label:(if m.streaming then "LIVE" else "PAUSED")
                ~color:
                  (if m.streaming then Ansi.Color.green else Ansi.Color.yellow);
            ];
        ];
    ]

let footer (m : model) =
  let focus =
    match m.focus with
    | No_focus -> "focus: none"
    | Focus_run_filter -> "focus: run filter"
    | Focus_log_filter -> "focus: log filter"
    | Focus_scalar_view -> "focus: scalar view (Esc to leave)"
    | Focus_config -> "focus: config (Esc to leave)"
  in
  box ~padding:(padding 1)
    ~background:(Ansi.Color.grayscale ~level:3)
    [
      box ~flex_direction:Row ~justify_content:Space_between
        [
          text
            "[j/k] run  [[]/[]] tab  [Space] pause  [/] run filter  [g] grid  \
             [a] axes  [s/S] smooth  [w] window  [q] quit";
          text ~style:dim_style focus;
        ];
    ]

let view (m : model) =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      header m;
      box ~flex_grow:1. ~flex_direction:Row ~gap:(gap 1) ~padding:(padding 1)
        [
          run_panel m;
          box ~flex_grow:1. ~flex_direction:Column ~gap:(gap 1)
            [
              (* tab bar *)
              box ~flex_direction:Row ~gap:(gap 1)
                (List.map (tab_button m) all_tabs);
              (* main content *)
              box ~flex_grow:1. [ main_panel m ];
            ];
          sys_panel m;
        ];
      footer m;
    ]

(* ---------- Subscriptions ---------- *)

let subscriptions (m : model) =
  Sub.batch
    [
      Sub.on_tick ~id:"animation" (fun ~dt -> Tick dt);
      (* Escape and Enter leave a filter input before it can act on them. *)
      Sub.on_key ~phase:Capture (fun ev ->
          let key = (Event.Key.data ev).key in
          match m.focus with
          | Focus_run_filter | Focus_log_filter -> (
              match key with
              | Escape | Enter ->
                  Event.Key.prevent_default ev;
                  Some Blur_focus
              | _ -> None)
          | No_focus | Focus_scalar_view | Focus_config -> (
              match key with
              | Escape when m.focus <> No_focus ->
                  Event.Key.prevent_default ev;
                  Some Blur_focus
              | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
              | Escape -> Some Quit
              | Char c when Uchar.equal c (Uchar.of_char ' ') ->
                  Some Toggle_stream
              | Char c when Uchar.equal c (Uchar.of_char ']') -> Some Next_tab
              | Char c when Uchar.equal c (Uchar.of_char '[') -> Some Prev_tab
              | Char c
                when m.focus = No_focus && Uchar.equal c (Uchar.of_char 'j') ->
                  Some Next_run
              | Char c
                when m.focus = No_focus && Uchar.equal c (Uchar.of_char 'k') ->
                  Some Prev_run
              | Char c when Uchar.equal c (Uchar.of_char '/') ->
                  Some (Focus Focus_run_filter)
              | Char c when Uchar.equal c (Uchar.of_char 'g') ->
                  Some Toggle_grid
              | Char c when Uchar.equal c (Uchar.of_char 'a') ->
                  Some Toggle_axes
              | Char c when Uchar.equal c (Uchar.of_char 'w') ->
                  Some Cycle_window
              | Char c when Uchar.equal c (Uchar.of_char 's') ->
                  Some Smoothing_down
              | Char c when Uchar.equal c (Uchar.of_char 'S') ->
                  Some Smoothing_up
              | _ -> None));
    ]

let () = run { init; update; view; subscriptions }
