(** Async operations with Cmd.perform. *)

open Mosaic

type task_status =
  | Pending
  | Running
  | Done of float (* duration *)
  | Failed of string

type task = { name : string; status : task_status }
type model = { tasks : task array; running_count : int }

type msg =
  | Launch of int
  | Launch_all
  | Task_complete of int * float
  | Task_failed of int * string
  | Quit

let task_definitions =
  [|
    "Fetching users...";
    "Loading config...";
    "Syncing data...";
    "Building index...";
    "Validating schema...";
    "Compressing logs...";
  |]

let delay_for_task i = 0.5 +. (float_of_int (((i * 37) + 13) mod 16) /. 10.0)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let green = Ansi.Color.green
let red = Ansi.Color.red
let cyan = Ansi.Color.cyan
let yellow = Ansi.Color.yellow

let init () =
  let tasks =
    Array.map (fun name -> { name; status = Pending }) task_definitions
  in
  ({ tasks; running_count = 0 }, Cmd.set_title "Async Tasks")

let launch_task model i =
  let task = model.tasks.(i) in
  match task.status with
  | Running -> (model, Cmd.none)
  | _ ->
      let tasks = Array.copy model.tasks in
      tasks.(i) <- { task with status = Running };
      let running_count = model.running_count + 1 in
      let title_cmd =
        Cmd.set_title
          (Printf.sprintf "Running %d task%s..." running_count
             (if running_count > 1 then "s" else ""))
      in
      let perform_cmd =
        Cmd.perform (fun dispatch ->
            let delay = delay_for_task i in
            Unix.sleepf delay;
            if i = 4 then dispatch (Task_failed (i, "connection timeout"))
            else dispatch (Task_complete (i, delay)))
      in
      ({ tasks; running_count }, Cmd.batch [ perform_cmd; title_cmd ])

let update msg model =
  match msg with
  | Launch i ->
      if i >= 0 && i < Array.length model.tasks then launch_task model i
      else (model, Cmd.none)
  | Launch_all ->
      let ref_model = ref model in
      let cmds = ref [] in
      Array.iteri
        (fun i task ->
          match task.status with
          | Running -> ()
          | _ ->
              let m, cmd = launch_task !ref_model i in
              ref_model := m;
              cmds := cmd :: !cmds)
        model.tasks;
      (!ref_model, Cmd.batch (List.rev !cmds))
  | Task_complete (i, duration) ->
      let tasks = Array.copy model.tasks in
      tasks.(i) <- { (tasks.(i)) with status = Done duration };
      let running_count = max 0 (model.running_count - 1) in
      let title_cmd =
        if running_count = 0 then Cmd.set_title "Async Tasks - All done"
        else
          Cmd.set_title
            (Printf.sprintf "Running %d task%s..." running_count
               (if running_count > 1 then "s" else ""))
      in
      ({ tasks; running_count }, title_cmd)
  | Task_failed (i, reason) ->
      let tasks = Array.copy model.tasks in
      tasks.(i) <- { (tasks.(i)) with status = Failed reason };
      let running_count = max 0 (model.running_count - 1) in
      let title_cmd =
        if running_count = 0 then Cmd.set_title "Async Tasks - Complete"
        else
          Cmd.set_title
            (Printf.sprintf "Running %d task%s..." running_count
               (if running_count > 1 then "s" else ""))
      in
      ({ tasks; running_count }, title_cmd)
  | Quit -> (model, Cmd.quit)

let status_icon task =
  match task.status with
  | Pending -> text ~style:(Ansi.Style.make ~fg:yellow ()) "○ pending "
  | Running ->
      box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
        [
          spinner ~frame_set:Spinner.dots ~live:true ~color:cyan ();
          text ~style:(Ansi.Style.make ~fg:cyan ()) "running ";
        ]
  | Done duration ->
      text
        ~style:(Ansi.Style.make ~fg:green ())
        (Printf.sprintf "● done     %.1fs" duration)
  | Failed reason ->
      text
        ~style:(Ansi.Style.make ~fg:red ())
        (Printf.sprintf "✗ error    %s" reason)

let view model =
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Async";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~padding:(padding 2)
        [
          box ~flex_direction:Column ~gap:(gap 1)
            ~size:{ width = pct 100; height = auto }
            (Array.to_list
               (Array.mapi
                  (fun i task ->
                    box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                      ~border:true ~border_color ~padding:(padding 1)
                      ~size:{ width = pct 100; height = auto }
                      [
                        text
                          ~style:(Ansi.Style.make ~bold:true ())
                          (Printf.sprintf "%d." (i + 1));
                        text task.name;
                        box ~flex_grow:1. [];
                        status_icon task;
                      ])
                  model.tasks));
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "1-6 launch task  •  Enter launch all  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | Enter -> Some Launch_all
      | Char c when Uchar.equal c (Uchar.of_char '1') -> Some (Launch 0)
      | Char c when Uchar.equal c (Uchar.of_char '2') -> Some (Launch 1)
      | Char c when Uchar.equal c (Uchar.of_char '3') -> Some (Launch 2)
      | Char c when Uchar.equal c (Uchar.of_char '4') -> Some (Launch 3)
      | Char c when Uchar.equal c (Uchar.of_char '5') -> Some (Launch 4)
      | Char c when Uchar.equal c (Uchar.of_char '6') -> Some (Launch 5)
      | _ -> None)

let () =
  Random.self_init ();
  run { init; update; view; subscriptions }
