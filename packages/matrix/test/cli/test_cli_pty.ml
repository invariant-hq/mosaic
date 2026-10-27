(* Command-line output on a pseudo-terminal: cli_child.exe runs on matrix's
   emulator (Matrix_test.Child), or with its outputs piped. *)

open Windtrap
module T = Matrix_test

let child = "./cli_child.exe"
let job = "../runtime_job.exe"

(* The scrollback and the screen, oldest first, without the blank rows at
   the end. *)
let lines term =
  let s = T.Terminal.screen term in
  let all =
    T.Terminal.scrollback term @ List.init (T.Screen.rows s) (T.Screen.row s)
  in
  let rec drop = function "" :: rest -> drop rest | l -> l in
  List.rev (drop (List.rev all))

let shows text term = T.Screen.find (T.Terminal.screen term) text <> []
let has_line l term = List.mem l (lines term)

let spawn ?(cols = 40) ?(rows = 10) ?(env = []) mode f =
  let term = T.Terminal.create ~cols ~rows () in
  T.Child.with_spawn term ~env ~prog:child ~args:[ mode ] (fun c -> f term c)

let exited code = function
  | Unix.WEXITED n when n = code -> ()
  | WEXITED n -> failf "exited %d" n
  | WSIGNALED n -> failf "killed by signal %d" n
  | WSTOPPED n -> failf "stopped by signal %d" n

let signaled s = function
  | Unix.WSIGNALED n when n = s -> ()
  | WSIGNALED n -> failf "killed by signal %d" n
  | WEXITED n -> failf "exited %d" n
  | WSTOPPED n -> failf "stopped by signal %d" n

let strings = Testable.(list string)

(* What every ending leaves: autowrap on, the cursor shown, no update open. *)
let usable term =
  let m = T.Terminal.modes term in
  is_true ~msg:"autowrap on" m.auto_wrap;
  is_true ~msg:"cursor shown" m.cursor_visible;
  is_false ~msg:"no update open" m.synchronized_output

let test_progress () =
  spawn "progress" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"lines above, the last frame stays, output after it" strings
    [ "SHELL"; "line 1"; "line 2"; "line 3"; "P done"; "AFTER" ]
    (lines term);
  usable term

(* A frame taller than the screen shows its last rows while it lives, and
   goes to the scrollback whole when the scope ends. *)
let test_tall () =
  spawn ~rows:10 "tall" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"every row of the last frame" strings
    (List.init 12 (Printf.sprintf "row %d"))
    (lines term)

let test_partial () =
  spawn "partial" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"a line shows whole once its end comes" strings
    [ "half whole"; "R end" ] (lines term)

let test_sigint () =
  spawn "wait" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "TWO");
  T.Child.send c "\003";
  signaled Sys.sigint (T.Child.exit c ~within:10.);
  usable term;
  let rows = lines term in
  equal ~msg:"the frame stays, under the echo of ^C on its origin" strings
    [ "^C"; "WAITING"; "TWO" ] rows;
  match T.Screen.cursor (T.Terminal.screen term) with
  | Some { row; col; _ } ->
      equal ~msg:"the cursor below the frame"
        Testable.(pair int int)
        (3, 0) (row, col)
  | None -> fail "the cursor is hidden"

(* A live inside a live on the same terminal joins its region: its rows show
   below the outer ones, its last rows go above when it ends, and the outer
   region stays until its own end. *)
let test_nested () =
  spawn "nested" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "INNER 1");
  (match
     ( T.Screen.find (T.Terminal.screen term) "OUTER",
       T.Screen.find (T.Terminal.screen term) "INNER 1" )
   with
  | [ (o, _) ], [ (i, _) ] -> equal ~msg:"inner below outer" int (o + 1) i
  | _ -> fail "both regions once");
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"each scope's last rows stay, in order" strings
    [ "line"; "INNER done"; "OUTER done" ]
    (lines term)

let test_exit () =
  spawn "exit" @@ fun term c ->
  exited 3 (T.Child.exit c ~within:10.);
  usable term;
  is_true ~msg:"the frame stays" (has_line "EXITING" term)

let test_resize () =
  spawn ~cols:40 "resize" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "W=40");
  T.Child.resize c ~cols:25 ~rows:10;
  T.Child.wait c ~within:10. (shows "W=25");
  T.Child.resize c ~cols:60 ~rows:6;
  T.Child.wait c ~within:10. (shows "W=60");
  T.Child.send c "\003";
  signaled Sys.sigint (T.Child.exit c ~within:10.);
  is_false ~msg:"no stale width shown" (shows "W=40" term || shows "W=25" term)

let test_suspend () =
  spawn "suspend" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"the child's lines above, the region below" strings
    [ "from child"; "unended"; "R2" ]
    (lines term)

let test_raw () =
  spawn "raw" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"the title" Testable.string "TITLE" (T.Terminal.title term);
  equal ~msg:"the region kept its place" strings [ "after raw"; "R" ]
    (lines term)

let red_bold term =
  match T.Screen.find (T.Terminal.screen term) "BOLDRED" with
  | (row, col) :: _ ->
      let _, style = T.Screen.cell (T.Terminal.screen term) ~row ~col in
      style
  | [] -> fail "no text"

let test_color () =
  ( spawn "color" @@ fun term c ->
    exited 0 (T.Child.exit c ~within:10.);
    let style = red_bold term in
    is_true ~msg:"bold" (Matrix.Ansi.Attr.mem Bold style.attrs);
    is_true ~msg:"a color" (Option.is_some style.fg) );
  spawn ~env:[ ("NO_COLOR", "1") ] "color" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  let style = red_bold term in
  is_true ~msg:"NO_COLOR keeps bold" (Matrix.Ansi.Attr.mem Bold style.attrs);
  is_true ~msg:"and drops the color"
    (match style.fg with
    | None -> true
    | Some c -> Matrix.Ansi.Color.equal c Matrix.Ansi.Color.default_fg)

let test_fmt () =
  spawn "fmt" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"Format's lines above the region, in order" strings
    [ "out 1"; "err 2"; "out 3"; "FRAME" ]
    (lines term)

let test_notify () =
  spawn "notify" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"a notification and the bell"
    (match T.Terminal.requests term with
    | [ Notification { body; _ }; Bell ] ->
        String.ends_with ~suffix:"built" body
    | _ -> false)

let test_line () =
  spawn "line" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Name?");
  T.Child.send c "Ada\r";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"the answer" (has_line "HELLO Ada" term)

let test_secret () =
  spawn "secret" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Password:");
  is_false ~msg:"the region is off the screen" (shows "BUSY" term);
  T.Child.send c "hunter2\r";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"the answer" (has_line "GOT hunter2" term);
  is_true ~msg:"never echoed" (List.mem "Password:" (lines term));
  is_true ~msg:"the region came back and stays" (has_line "BUSY" term)

let test_secret_interrupted () =
  spawn "secret-int" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Password:");
  T.Child.send c "hun\003";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"Ctrl+C ended the prompt, and the echo is back"
    (has_line "ENDED SIGINT ECHO true" term)

let test_keys () =
  spawn "keys" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "READY");
  T.Child.send c "ab\r\027[Aq";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"each key, as typed" (has_line "KEYS ab<enter><up>" term)

(* Ctrl+Z under a job-control shell stops the job with the region off the
   screen; continued, it draws the region again. *)
let test_stop () =
  let term = T.Terminal.create ~cols:40 ~rows:10 () in
  T.Child.with_spawn term
    ~env:[ ("CLI_CHILD", "tstp") ]
    ~prog:job ~args:[ child ]
  @@ fun c ->
  T.Child.wait c ~within:10. (shows "LIVE");
  T.Child.send c "\026";
  T.Child.wait c ~within:10. (shows "JOB-STOPPED TSTP");
  is_false ~msg:"the region is off the screen" (shows "LIVE" term);
  T.Child.send c "f\r";
  T.Child.wait c ~within:10. (shows "LIVE");
  T.Child.send c "\003";
  T.Child.wait c ~within:10. (shows "JOB-SIGNALED");
  usable term

(* Continued in the background, the job draws nothing until its end, where
   its last frame is printed as a line. *)
let test_background () =
  let term = T.Terminal.create ~cols:40 ~rows:10 () in
  T.Child.with_spawn term
    ~env:[ ("CLI_CHILD", "tstp-bg") ]
    ~prog:job ~args:[ child ]
  @@ fun c ->
  T.Child.wait c ~within:10. (shows "LIVE");
  T.Child.send c "\026";
  T.Child.wait c ~within:10. (shows "JOB-STOPPED TSTP");
  T.Child.send c "b\r";
  Unix.sleepf 0.5;
  is_false ~msg:"no frame from the background" (shows "LIVE" term);
  T.Child.wait c ~within:10. (shows "JOB-EXIT 0");
  is_true ~msg:"the last frame once" (shows "LIVE END" term)

(* Ctrl+Z in a secret puts the echo back while the job is stopped, and asks
   again once it continues. *)
let test_secret_stop () =
  let term = T.Terminal.create ~cols:40 ~rows:10 () in
  T.Child.with_spawn term
    ~env:[ ("CLI_CHILD", "secret") ]
    ~prog:job ~args:[ child ]
  @@ fun c ->
  T.Child.wait c ~within:10. (shows "Password:");
  T.Child.send c "\026";
  T.Child.wait c ~within:10. (shows "JOB-STOPPED TSTP");
  T.Child.send c "xyz f\r";
  (* Asked again: a question below the stop's line. Where the shell's echo
     lands on the rows above depends on how the two writers interleave. *)
  T.Child.wait c ~within:10. (fun t ->
      let screen = T.Terminal.screen t in
      match T.Screen.find screen "JOB-STOPPED" with
      | (stopped, _) :: _ ->
          List.exists
            (fun (row, _) -> row > stopped)
            (T.Screen.find screen "Password:")
      | [] -> false);
  is_true ~msg:"the shell's line was echoed" (shows "xyz f" term);
  T.Child.send c "pw\r";
  T.Child.wait c ~within:10. (shows "JOB-EXIT 0");
  is_true ~msg:"the answer" (shows "GOT pw" term);
  is_false ~msg:"never echoed" (shows "Password: pw" term)

(* A terminal that stops taking output (Ctrl+S) does not keep Ctrl+C from
   ending the process. *)
let test_flow_control () =
  spawn "flood" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "FRAME");
  T.Child.send c "\019";
  Unix.sleepf 0.3;
  let t0 = Unix.gettimeofday () in
  T.Child.send c "\003";
  signaled Sys.sigint (T.Child.exit c ~within:10.);
  is_true ~msg:"ended within 2 s" (Unix.gettimeofday () -. t0 < 2.);
  ignore term

(* What [mode] writes on its standard outputs, both on one pipe. *)
let piped mode =
  let r, w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env child [| child; mode |]
      [| "TERM=xterm-256color" |]
      Unix.stdin w w
  in
  Unix.close w;
  let out = In_channel.input_all (Unix.in_channel_of_descr r) in
  Unix.close r;
  ignore (Unix.waitpid [] pid : int * Unix.process_status);
  out

(* A grid printed on an output: its rows at the output's styling, each
   ended where it shows nothing more. *)
let test_print_grid () =
  let out = piped "table" in
  equal ~msg:"plain rows, no trailing blanks" Testable.string
    "file \226\148\130 size\na.ml \226\148\130   12\n" out

(* Piped, the output gets no escape, its lines as they are printed and the
   last frame once. *)
let test_pipe () =
  let out = piped "progress" in
  equal ~msg:"plain lines, the last frame once" Testable.string
    "SHELL\nline 1\nline 2\nline 3\nP done\nAFTER\n" out

(* Lines printed through Format's standard formatters and left in their
   buffers go out before an output's next write, whether the formatters are
   adapted or not. *)
let test_format_order () =
  List.iter
    (fun mode ->
      equal ~msg:mode Testable.string
        "result 1\nstep a\nresult 2\nnote 3\nstep b\nFRAME\n" (piped mode))
    [ "fmt-order"; "fmt-order-adapted" ]

let () =
  run "matrix.cli.pty"
    [
      group "live"
        [
          test "lines above, the last frame stays" test_progress;
          test "a frame taller than the screen" test_tall;
          test "a line waits for its end" test_partial;
          test "SIGINT" test_sigint;
          test "exit" test_exit;
          test "a live inside a live" test_nested;
          test "resizes" test_resize;
          test "a child's output" test_suspend;
          test "raw bytes" test_raw;
          test "Ctrl+Z and fg" test_stop;
          test "in the background" test_background;
          test "Ctrl+S, then Ctrl+C" test_flow_control;
          test "a pipe" test_pipe;
        ];
      group "outputs"
        [
          test "colors and NO_COLOR" test_color;
          test "a grid" test_print_grid;
          test "Format's formatters" test_fmt;
          test "Format's buffers keep their order" test_format_order;
          test "a notification" test_notify;
        ];
      group "prompts"
        [
          test "a line" test_line;
          test "a secret" test_secret;
          test "Ctrl+C in a secret" test_secret_interrupted;
          test "Ctrl+Z in a secret" test_secret_stop;
          test "keys" test_keys;
        ];
    ]
