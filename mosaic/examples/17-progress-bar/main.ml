(** Progress bars with animated fill and orientations. *)

open Mosaic

type task = {
  name : string;
  progress : float;
  speed : float;
  color : Ansi.Color.t;
  done_ : bool;
}

type model = { tasks : task array; running : bool; elapsed : float }
type msg = Tick of float | Toggle | Quit

let tasks_init =
  [|
    {
      name = "Downloading assets";
      progress = 0.0;
      speed = 12.0;
      color = Ansi.Color.cyan;
      done_ = false;
    };
    {
      name = "Compiling sources";
      progress = 0.0;
      speed = 8.5;
      color = Ansi.Color.green;
      done_ = false;
    };
    {
      name = "Fetching dependencies";
      progress = 0.0;
      speed = 18.0;
      color = Ansi.Color.magenta;
      done_ = false;
    };
    {
      name = "Building index";
      progress = 0.0;
      speed = 5.0;
      color = Ansi.Color.yellow;
      done_ = false;
    };
    {
      name = "Optimizing output";
      progress = 0.0;
      speed = 14.0;
      color = Ansi.Color.of_rgb 255 140 60;
      done_ = false;
    };
  |]

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let init () =
  ({ tasks = Array.copy tasks_init; running = true; elapsed = 0.0 }, Cmd.none)

let update msg model =
  match msg with
  | Toggle -> ({ model with running = not model.running }, Cmd.none)
  | Quit -> (model, Cmd.quit)
  | Tick dt ->
      if not model.running then (model, Cmd.none)
      else
        let elapsed = model.elapsed +. dt in
        let tasks =
          Array.map
            (fun task ->
              if task.done_ then task
              else
                let progress =
                  Float.min 100.0 (task.progress +. (task.speed *. dt))
                in
                let done_ = progress >= 100.0 in
                { task with progress; done_ })
            model.tasks
        in
        ({ tasks; running = model.running; elapsed }, Cmd.none)

let all_done tasks = Array.for_all (fun t -> t.done_) tasks

let view model =
  let completed =
    Array.fold_left (fun n t -> if t.done_ then n + 1 else n) 0 model.tasks
  in
  let total = Array.length model.tasks in
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Progress Bar";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            ~size:{ width = px 60; height = auto }
            (List.append
               (* Horizontal task bars *)
               (List.mapi
                  (fun i task ->
                    box
                      ~key:(Printf.sprintf "task-%d" i)
                      ~flex_direction:Column ~gap:(gap 0)
                      [
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~size:{ width = pct 100; height = auto }
                          [
                            text
                              ~style:
                                (Ansi.Style.make
                                   ~fg:
                                     (if task.done_ then Ansi.Color.green
                                      else Ansi.Color.white)
                                   ())
                              (if task.done_ then task.name ^ " ✓"
                               else task.name);
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true ~fg:task.color ())
                              (Printf.sprintf "%3.0f%%" task.progress);
                          ];
                        progress_bar
                          ~size:{ width = pct 100; height = px 1 }
                          ~value:task.progress ~min:0.0 ~max:100.0
                          ~orientation:`Horizontal ~filled_color:task.color
                          ~empty_color:(Ansi.Color.grayscale ~level:4)
                          ();
                      ])
                  (Array.to_list model.tasks))
               [
                 (* Status line *)
                 box ~flex_direction:Row ~justify_content:Center
                   [
                     (if all_done model.tasks then
                        text
                          ~style:
                            (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                          "All tasks complete!"
                      else
                        text ~style:muted
                          (Printf.sprintf "%d/%d complete  •  %s" completed
                             total
                             (if model.running then "running" else "paused")));
                   ];
                 (* Vertical bars summary *)
                 box ~flex_direction:Row ~justify_content:Center ~gap:(gap 2)
                   ~align_items:Flex_end
                   (List.mapi
                      (fun i task ->
                        box
                          ~key:(Printf.sprintf "vbar-%d" i)
                          ~flex_direction:Column ~align_items:Center
                          ~gap:(gap 0)
                          [
                            text
                              ~style:(Ansi.Style.make ~dim:true ())
                              (Printf.sprintf "%.0f" task.progress);
                            progress_bar
                              ~size:{ width = px 2; height = px 6 }
                              ~value:task.progress ~min:0.0 ~max:100.0
                              ~orientation:`Vertical ~filled_color:task.color
                              ~empty_color:(Ansi.Color.grayscale ~level:4)
                              ();
                          ])
                      (Array.to_list model.tasks));
               ]);
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "Space start/pause  •  q quit" ];
    ]

let subscriptions _model =
  Sub.batch
    [
      Sub.on_tick (fun ~dt -> Tick dt);
      Sub.on_key (fun ev ->
          match (Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char ' ') -> Some Toggle
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
    ]

let () = run { init; update; view; subscriptions }
