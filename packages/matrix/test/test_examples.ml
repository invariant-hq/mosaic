(* The examples of matrix.mli and matrix_test.mli, as written there, run. *)

open Windtrap

(* [Matrix.next]'s loop, as its doc writes it. *)
let rec next app =
  match Matrix.step app ~now:(Matrix.clock ()) with
  | Some step -> step
  | None ->
      let timeout =
        match Matrix.deadline app with
        | None -> -1.
        | Some d -> Float.max 0. (Float.of_int (d - Matrix.clock ()) *. 1e-9)
      in
      (try ignore (Unix.select (Matrix.fds app) [] [] timeout)
       with Unix.Unix_error (EINTR, _, _) -> ());
      next app

(* The documented loop draws the first frame, then waits for the input,
   whose end it returns as [Stopped]. *)
let test_next () =
  if Sys.win32 then
    skip ~reason:"Device.unix, which needs a wake pipe Windows lacks" ();
  let input, writer = Unix.pipe ~cloexec:true () in
  let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ input; output ])
    (fun () ->
      let frames =
        Matrix.run ~input ~output (fun app ->
            let rec loop frames =
              match next app with
              | Frame ->
                  Matrix.draw app ignore;
                  if frames = 0 then Unix.close writer;
                  loop (frames + 1)
              | Stopped -> frames
              | Event _ | Timer _ | Posted _ -> loop frames
            in
            loop 0)
      in
      equal ~msg:"one frame, then the end of the input" int 1 frames)

(* [Matrix_test]'s preamble example. *)
let test_matrix_test () =
  let term = Matrix_test.Terminal.create ~cols:20 ~rows:1 () in
  Matrix_test.run term (fun s ->
      Matrix.run ~config:(Matrix_test.config s) @@ fun app ->
      let rec loop keys =
        match Matrix.next app with
        | Stopped -> ()
        | Event (Key _) ->
            Matrix.request_redraw app;
            loop (keys + 1)
        | Frame ->
            Matrix.draw app (fun grid ->
                Matrix.Grid.draw_text grid ~x:0 ~y:0
                  ~text:(Printf.sprintf "%d keys" keys));
            loop keys
        | Event _ | Timer _ | Posted _ -> loop keys
      in
      loop 0)
  @@ fun s ->
  Matrix_test.input s (Matrix_test.Terminal.text term "ab");
  print_string (Matrix_test.Screen.text (Matrix_test.Terminal.screen term));
  equal ~msg:"the screen" string "2 keys"
    (Matrix_test.Screen.row (Matrix_test.Terminal.screen term) 0)

(* A program whose [Matrix.next] loop draws each frame with [on_frame app],
   run on a session; [f] gets the session, the terminal and the run. *)
let with_run ?(cols = 20) ?(rows = 3) ?(setup = ignore) on_frame f =
  let term = Matrix_test.Terminal.create ~cols ~rows () in
  let app_ref = ref None in
  Matrix_test.run term (fun s ->
      Matrix.run ~config:(Matrix_test.config s) @@ fun app ->
      app_ref := Some app;
      setup app;
      let rec loop () =
        match Matrix.next app with
        | Stopped -> ()
        | Frame ->
            on_frame app;
            loop ()
        | Event _ | Timer _ | Posted _ -> loop ()
      in
      loop ())
  @@ fun s -> f s term (Option.get !app_ref)

let hello grid = Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"hello"

(* The diagnostics section's dump of every thirtieth frame: its files hold
   the frame drawn, with styles and as plain text. *)
let test_dump_every_thirtieth_frame () =
  let dir = Filename.temp_dir "matrix_dumps" "" in
  let frames = ref 0 in
  let on_frame app view =
    incr frames;
    Matrix.draw app (fun grid ->
        view grid;
        if !frames mod 30 = 0 then Matrix.dump_frame ~dir app)
  in
  ( with_run (fun app -> on_frame app hello) @@ fun s _ app ->
    for _ = 2 to 30 do
      Matrix.request_redraw app;
      Matrix_test.settle s
    done );
  let files = Sys.readdir dir in
  Array.sort compare files;
  match files with
  | [| ansi; txt |] ->
      let read file =
        In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all
      in
      let dump = read ansi in
      is_true ~msg:"the dump holds the frame"
        (String.length dump >= 5
        && Seq.exists
             (fun i -> String.sub dump i 5 = "hello")
             (Seq.init (String.length dump - 4) Fun.id));
      equal ~msg:"and as text" string "hello\n\n" (read txt)
  | files -> failf "%d files, expected two" (Array.length files)

(* [Debug_overlay]'s preamble example draws the panel over the view. *)
let draw_frame overlay app view =
  let start = Matrix.clock () in
  Matrix.draw app (fun grid ->
      view grid;
      Matrix.Debug_overlay.draw overlay
        ~metrics:(Matrix.Screen.last_metrics (Matrix.screen app))
        ~gc:(Gc.quick_stat ()) grid);
  Matrix.Debug_overlay.record overlay ~start ~ns:(Matrix.clock () - start)

let test_debug_overlay () =
  let overlay = Matrix.Debug_overlay.create () in
  with_run ~cols:40 ~rows:30 (fun app -> draw_frame overlay app hello)
  @@ fun _ term _ ->
  let screen = Matrix_test.Terminal.screen term in
  let shows sub = Matrix_test.Screen.find screen sub <> [] in
  is_true ~msg:"the view" (shows "hello");
  is_true ~msg:"the panel" (shows "frames: 0")

(* [set_cursor]'s example: a cursor shown from the first frame. *)
let test_cursor_from_the_first_frame () =
  with_run
    ~setup:(fun app -> Matrix.set_cursor app ~visible:true)
    (fun app -> Matrix.draw app hello)
  @@ fun _ term app ->
  is_true ~msg:"shown" (Matrix.Screen.cursor (Matrix.screen app)).visible;
  is_true ~msg:"the terminal shows it"
    (Matrix_test.Screen.cursor (Matrix_test.Terminal.screen term) <> None)

let () =
  run "matrix.examples"
    [
      group "Examples"
        [
          test "Matrix.next's loop" test_next;
          test "Matrix_test's preamble" test_matrix_test;
          test "a dump of every thirtieth frame" test_dump_every_thirtieth_frame;
          test "Debug_overlay's preamble" test_debug_overlay;
          test "a cursor shown from the first frame"
            test_cursor_from_the_first_frame;
        ];
    ]
