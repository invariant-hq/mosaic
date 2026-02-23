(** Async task runner demonstrating [Cmd.perform] with Eio fibers. *)

open Mosaic

type job_status = Pending | Running of float | Done
type job = { id : int; name : string; duration : float; status : job_status }

type model = {
  jobs : job list;
  next_id : int;
  now : unit -> float;
  sleep : float -> unit;
}

type msg = Add_job | Job_started of int | Job_done of int | Tick | Quit

let job_names =
  [|
    "Compile assets";
    "Run migrations";
    "Fetch metadata";
    "Sync cache";
    "Index records";
    "Generate report";
    "Compress logs";
    "Validate schema";
  |]

let random_job_name () =
  let i = Random.int (Array.length job_names) in
  job_names.(i)

let random_duration () = 1.0 +. Random.float 4.0

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let init ~clock () =
  let now () = Eio.Time.now clock in
  let sleep d = Eio.Time.sleep clock d in
  ({ jobs = []; next_id = 1; now; sleep }, Cmd.none)

let update_job id f jobs = List.map (fun j -> if j.id = id then f j else j) jobs

let update msg model =
  match msg with
  | Add_job ->
      let id = model.next_id in
      let duration = random_duration () in
      let job = { id; name = random_job_name (); duration; status = Pending } in
      let cmd =
        Cmd.perform (fun dispatch ->
            dispatch (Job_started id);
            model.sleep duration;
            dispatch (Job_done id))
      in
      ({ model with jobs = model.jobs @ [ job ]; next_id = id + 1 }, cmd)
  | Job_started id ->
      let jobs =
        update_job id
          (fun j -> { j with status = Running (model.now ()) })
          model.jobs
      in
      ({ model with jobs }, Cmd.none)
  | Job_done id ->
      let jobs = update_job id (fun j -> { j with status = Done }) model.jobs in
      ({ model with jobs }, Cmd.none)
  | Tick -> (model, Cmd.none)
  | Quit -> (model, Cmd.quit)

let status_indicator now job =
  match job.status with
  | Pending -> text ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ()) "○"
  | Running started ->
      let elapsed = now -. started in
      let pct = Float.min 1.0 (elapsed /. job.duration) *. 100.0 in
      text
        ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
        (Printf.sprintf "◑ %2.0f%%" pct)
  | Done -> text ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "●"

let view model =
  let now = model.now () in
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Async Tasks (Eio)";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~padding:(padding 2)
        [
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 100; height = pct 100 }
            [
              box ~flex_direction:Column ~gap:(gap 1)
                (if model.jobs = [] then
                   [
                     text
                       ~style:(Ansi.Style.make ~dim:true ())
                       "No jobs yet. Press 'a' to launch one.";
                   ]
                 else
                   List.map
                     (fun job ->
                       box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                         ~border:true ~border_color ~padding:(padding 1)
                         ~size:{ width = pct 100; height = auto }
                         [
                           status_indicator now job;
                           text
                             ~style:(Ansi.Style.make ~bold:true ())
                             (Printf.sprintf "#%d" job.id);
                           text job.name;
                           text ~style:muted
                             (Printf.sprintf "(%.1fs)" job.duration);
                         ])
                     model.jobs);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "a add job  •  q quit" ];
    ]

let has_running_jobs model =
  List.exists
    (fun j -> match j.status with Running _ -> true | _ -> false)
    model.jobs

let subscriptions model =
  Sub.batch
    [
      (if has_running_jobs model then Sub.on_tick (fun ~dt:_ -> Tick)
       else Sub.none);
      Sub.on_key (fun ev ->
          match (Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 'a') -> Some Add_job
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
    ]

let () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let matrix =
    Matrix_eio.create ~sw ~clock ~stdin:env#stdin ~stdout:env#stdout ()
  in
  let process_perform thunk =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        thunk ();
        `Stop_daemon)
  in
  Mosaic.run ~matrix ~process_perform
    { init = init ~clock; update; view; subscriptions }
