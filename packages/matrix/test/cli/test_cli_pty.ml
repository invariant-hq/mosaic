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

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

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

(* What [mode] writes on its standard outputs, both on one pipe. *)
let piped ?(env = []) mode =
  let r, w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process_env child [| child; mode |]
      (Array.of_list
         ("TERM=xterm-256color" :: List.map (fun (k, v) -> k ^ "=" ^ v) env))
      Unix.stdin w w
  in
  Unix.close w;
  let out = In_channel.input_all (Unix.in_channel_of_descr r) in
  Unix.close r;
  ignore (Unix.waitpid [] pid : int * Unix.process_status);
  out

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

(* A region with a tick draws its rows again on the clock, calling the frame
   between two shows. *)
let test_tick () =
  spawn "tick" @@ fun _ c ->
  let calls t =
    let s = T.Terminal.screen t in
    List.fold_left
      (fun n (row, col) ->
        let text = T.Screen.row s row in
        let from = col + String.length "CALLS " in
        match
          int_of_string_opt
            (String.trim (String.sub text from (String.length text - from)))
        with
        | Some k -> max n k
        | None -> n)
      0 (T.Screen.find s "CALLS ")
  in
  T.Child.wait c ~within:10. (fun t -> calls t >= 4);
  exited 0 (T.Child.exit c ~within:10.)

(* A frame is given the rows the region shows of it: the terminal's height
   less the origin's row, less the rows of the scopes above. On a pipe each
   scope's last frame is given 24 rows. *)
let test_height () =
  ( spawn ~rows:10 "height" @@ fun term c ->
    T.Child.wait c ~within:10. (fun t -> shows "H=9" t && shows "IH=8" t);
    T.Child.resize c ~cols:40 ~rows:6;
    T.Child.wait c ~within:10. (fun t -> shows "H=5" t && shows "IH=4" t);
    T.Child.send c "\003";
    signaled Sys.sigint (T.Child.exit c ~within:10.);
    usable term );
  equal ~msg:"on a pipe" Testable.string "IH=24\nH=24\n" (piped "height")

(* A frame's grids show one under another, and a frame of none shows no
   row. *)
let test_stack () =
  ( spawn "stack" @@ fun term c ->
    exited 0 (T.Child.exit c ~within:10.);
    equal ~msg:"on a terminal" strings
      [ "line"; "TITLE"; "ROW 1"; "ROW 2"; "AFTER" ]
      (lines term) );
  equal ~msg:"on a pipe" Testable.string "line\nTITLE\nROW 1\nROW 2\nAFTER\n"
    (piped "stack")

let test_partial () =
  spawn "partial" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  equal ~msg:"a line shows whole once its end comes" strings
    [ "half whole"; "R end" ] (lines term)

let cursor_visible term = (T.Terminal.modes term).cursor_visible

let test_sigint () =
  spawn "wait" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "TWO");
  is_false ~msg:"the cursor hidden while the region shows" (cursor_visible term);
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

let cursor_at term =
  match T.Screen.cursor (T.Terminal.screen term) with
  | Some { row; col; _ } -> (row, col)
  | None -> fail "the cursor is hidden"

(* [exit] ends the region as the scope's end would: the frame stays from
   its origin, with the cursor below it, and what an [at_exit] function
   prints then goes below as plain lines. *)
let test_exit () =
  ( spawn "exit" @@ fun term c ->
    exited 3 (T.Child.exit c ~within:10.);
    usable term;
    equal ~msg:"the frame stays, with no blank row" strings [ "EXITING" ]
      (lines term);
    equal ~msg:"the cursor below it"
      Testable.(pair int int)
      (1, 0) (cursor_at term) );
  spawn "exit-at-exit" @@ fun term c ->
  exited 3 (T.Child.exit c ~within:10.);
  usable term;
  equal ~msg:"the at_exit line below the frame, once" strings
    [ "EXITING"; "printed at exit" ]
    (lines term);
  equal ~msg:"the cursor below it"
    Testable.(pair int int)
    (2, 0) (cursor_at term)

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
  ( spawn "suspend" @@ fun term c ->
    exited 0 (T.Child.exit c ~within:10.);
    equal ~msg:"the child's lines above, the region below" strings
      [ "from child"; "unended"; "R2" ]
      (lines term) );
  spawn "suspend-wait" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "IN CHILD");
  is_true ~msg:"the cursor shown for the child" (cursor_visible term);
  exited 0 (T.Child.exit c ~within:10.);
  usable term

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
  ( spawn "line" @@ fun term c ->
    T.Child.wait c ~within:10. (shows "Name?");
    T.Child.send c "Ada\r";
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:"the answer" (has_line "HELLO Ada" term);
    is_true ~msg:"the prompt's space" (has_line "Name? Ada" term) );
  ( spawn "line-default" @@ fun term c ->
    T.Child.wait c ~within:10. (shows "Name? [demo]");
    T.Child.send c "\r";
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:"an empty line is the default" (has_line "HELLO demo" term) );
  equal ~msg:"nobody to ask: the default, and no question" Testable.string
    "HELLO demo\n" (piped "line-default");
  equal ~msg:"no default" Testable.string "NOT INTERACTIVE\n" (piped "line")

(* A confirmation reads y or n first, as the POSIX locale does, and asks
   again otherwise. *)
let test_confirm () =
  (* Each line once the question shows again. *)
  let answer ?(mode = "confirm") keys expected =
    spawn mode @@ fun term c ->
    List.iteri
      (fun i k ->
        T.Child.wait c ~within:10. (fun t ->
            List.length (T.Screen.find (T.Terminal.screen t) "Publish?") > i);
        T.Child.send c k)
      keys;
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:(String.concat "," keys) (has_line expected term);
    term
  in
  ignore (answer [ "y\r" ] "YES" : T.Terminal.t);
  ignore (answer [ "  Yes!\r" ] "YES" : T.Terminal.t);
  ignore (answer [ "n\r" ] "NO" : T.Terminal.t);
  ignore (answer [ "\r" ] "NO" : T.Terminal.t);
  let term = answer [ "maybe\r"; "N\r" ] "NO" in
  equal ~msg:"asked again" (list string)
    [ "Publish? [y/N] maybe"; "Publish? [y/N] N" ]
    (List.filter (String.starts_with ~prefix:"Publish?") (lines term));
  let term = answer ~mode:"confirm-nodefault" [ "\r"; "y\r" ] "YES" in
  equal ~msg:"no default: an empty line asks again" (list string)
    [ "Publish? [y/n]"; "Publish? [y/n] y" ]
    (List.filter (String.starts_with ~prefix:"Publish?") (lines term));
  equal ~msg:"nobody to ask" Testable.string "NO\n" (piped "confirm")

(* A masked secret shows a star for each character, reads keys decoded, and
   erases its stars when the line ends. *)
let test_secret_mask () =
  let answer ?(cols = 40) keys expected =
    spawn ~cols "secret-mask" @@ fun term c ->
    T.Child.wait c ~within:10. (shows "Password:");
    T.Child.send c keys;
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:(String.escaped keys) (has_line ("GOT " ^ expected) term);
    is_false ~msg:"no star left" (shows "*" term)
  in
  answer "hunter2\r" "\"hunter2\"";
  answer "huntx\127er2\r" "\"hunter2\"";
  answer "ab\027[Dc\r" "\"abc\"";
  answer "xyz\021ab cd\023\r" "\"ab \"";
  answer "\027[200~pw\027[201~\r" "\"pw\"";
  answer "\027[200~pw\nrest\027[201~" "\"pw\"";
  answer "caf\233\r" "\"caf\\233\"";
  answer "pw\027a\r" "\"pwa\"";
  ( spawn ~cols:20 "secret-mask" @@ fun term c ->
    T.Child.wait c ~within:10. (shows "Password:");
    T.Child.send c (String.make 30 'x');
    T.Child.wait c ~within:10. (shows "**********");
    T.Child.send c "\r";
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:"stars that wrap are erased"
      (List.mem "Password:" (lines term) && not (shows "*" term)) );
  spawn "secret-mask" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Password:");
  T.Child.send c "ab";
  T.Child.wait c ~within:10. (shows "Password: **");
  T.Child.send c "\004";
  is_false ~msg:"Ctrl+D on a line goes on" (has_line "EOF" term);
  T.Child.send c "\021\004";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"Ctrl+D on an empty line ends the input" (has_line "EOF" term)

(* A menu on the controlling terminal: arrows, k and j move, Enter picks,
   Escape gives the default, and the answered question stays. *)
let test_choose () =
  let pick ?(mode = "choose") ?(env = []) keys expected =
    spawn ~env mode @@ fun term c ->
    T.Child.wait c ~within:10. (shows "? License");
    List.iter
      (fun k ->
        T.Child.send c k;
        Unix.sleepf 0.15)
      keys;
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:(String.concat "," keys) (has_line expected term);
    term
  in
  let term = pick [ "\027[B"; "\r" ] "PICKED 2" in
  is_true ~msg:"the answer stays" (has_line "? License Apache-2.0" term);
  is_false ~msg:"the menu goes" (shows "LGPL" term);
  ignore (pick [ "j"; "j"; "k"; "\r" ] "PICKED 2" : T.Terminal.t);
  ignore (pick [ "\027[A"; "\027[A"; "\r" ] "PICKED 0" : T.Terminal.t);
  ignore (pick [ "\027[B"; "\027" ] "PICKED 1" : T.Terminal.t);
  ignore (pick ~mode:"choose-nodefault" [ "\r" ] "PICKED 0" : T.Terminal.t);
  ( spawn ~env:[ ("CI", "true") ] "choose" @@ fun term c ->
    T.Child.wait c ~within:10. (shows "\u{276F} MIT");
    T.Child.send c "\027[B";
    T.Child.wait c ~within:10. (shows "\u{276F} Apache-2.0");
    T.Child.send c "\r";
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg:"redrawn in CI" (has_line "PICKED 2" term) );
  equal ~msg:"no controlling terminal: the default, and nothing written"
    Testable.string "PICKED 1\n" (piped "choose-detached")

(* A question is written at the styling of the terminal it is asked on. *)
let test_secret_styled () =
  spawn ~env:[ ("NO_COLOR", "1") ] "secret-styled" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Password:");
  T.Child.send c "pw\r";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"the question" (shows "? Password:" term);
  is_false ~msg:"no color" (contains (T.Terminal.output term) "\027[31m")

(* What a thread prints while a question is asked waits for the answer. *)
let test_line_held () =
  spawn "line-held" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Name?");
  Unix.sleepf 0.2;
  T.Child.send c "Ada\r";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"the question whole" (has_line "Name? Ada" term);
  let ls = lines term in
  let rec after_question = function
    | "Name? Ada" :: rest -> rest
    | _ :: rest -> after_question rest
    | [] -> []
  in
  is_true ~msg:"lines printed while asked come after"
    (List.length
       (List.filter (String.starts_with ~prefix:"TICK") (after_question ls))
    >= 2)

(* A prompt blocks only the thread that asks: another keeps running, which
   is what an Eio program relies on when it asks through
   [Eio_unix.run_in_systhread]. *)
let test_line_thread () =
  spawn "line-thread" @@ fun term c ->
  let contains sub s =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
    in
    go 0
  in
  let has sub t = List.exists (contains sub) (lines t) in
  T.Child.wait c ~within:10. (fun t -> has "Name?" t && has "TICK 3" t);
  T.Child.send c "Ada\r";
  exited 0 (T.Child.exit c ~within:10.);
  is_true ~msg:"the answer" (has_line "HELLO Ada" term)

let test_secret () =
  spawn "secret" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "Password:");
  is_false ~msg:"the region is off the screen" (shows "BUSY" term);
  is_true ~msg:"the cursor shown at the question" (cursor_visible term);
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
  is_true ~msg:"the cursor shown while the job is stopped" (cursor_visible term);
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

(* How progress shows on an output: rows on a terminal outside CI, lines on
   a pipe, and rows in CI too when the program asks for them. *)
let test_progress_of () =
  let on_terminal msg env expected =
    spawn ~env "progress-of" @@ fun term c ->
    exited 0 (T.Child.exit c ~within:10.);
    is_true ~msg (has_line ("PROGRESS " ^ expected) term)
  in
  on_terminal "on a terminal" [] "tty";
  on_terminal "in CI" [ ("CI", "true") ] "plain";
  on_terminal "asked for in CI"
    [ ("CI", "true"); ("CLI_PROGRESS", "tty") ]
    "tty";
  on_terminal "quiet" [ ("CLI_PROGRESS", "quiet") ] "quiet";
  equal ~msg:"on a pipe" Testable.string "PROGRESS plain\n"
    (piped "progress-of");
  equal ~msg:"asked for on a pipe" Testable.string "PROGRESS plain\n"
    (piped ~env:[ ("CLI_PROGRESS", "tty") ] "progress-of")

(* [s] with the value of each "time" member made 0. *)
let untimed s =
  let b = Buffer.create (String.length s) in
  let key = "\"time\":" in
  let n = String.length key in
  let rec go i =
    if i < String.length s then
      if i + n <= String.length s && String.sub s i n = key then (
        Buffer.add_string b (key ^ "0");
        let j = ref (i + n) in
        while !j < String.length s && s.[!j] <> ',' && s.[!j] <> '}' do
          incr j
        done;
        go !j)
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Under quiet and json a region draws and prints no frame, and under json
   what the program prints on the output is output events. *)
let test_quiet_json () =
  equal ~msg:"quiet: the lines, no frame" Testable.string
    "SHELL\nline 1\nline 2\nline 3\nAFTER\n"
    (piped ~env:[ ("CLI_PROGRESS", "quiet") ] "progress");
  equal ~msg:"json: the lines as events" Testable.string
    "SHELL\n\
     {\"time\":0,\"event\":\"output\",\"text\":\"line 1\"}\n\
     {\"time\":0,\"event\":\"output\",\"text\":\"line 2\"}\n\
     {\"time\":0,\"event\":\"output\",\"text\":\"line 3\"}\n\
     AFTER\n"
    (untimed (piped ~env:[ ("CLI_PROGRESS", "json") ] "progress"));
  spawn ~cols:100 ~env:[ ("CLI_PROGRESS", "json") ] "progress" @@ fun term c ->
  exited 0 (T.Child.exit c ~within:10.);
  is_false ~msg:"no frame on a terminal" (shows "P done" term);
  is_true ~msg:"an event" (shows "\"text\":\"line 3\"" term)

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
    "SHELL\nline 1\nline 2\nline 3\nP done\nAFTER\n" out;
  equal ~msg:"the last frame at exit" Testable.string "EXITING\n" (piped "exit");
  equal ~msg:"the last frame, then what at_exit prints" Testable.string
    "EXITING\nprinted at exit\n" (piped "exit-at-exit");
  equal ~msg:"the last frame when the scope raises" Testable.string
    "RAISED\nCAUGHT\n" (piped "raise")

(* Lines printed through Format's standard formatters and left in their
   buffers go out before an output's next write, whether the formatters are
   adapted or not. *)
let test_format_order () =
  List.iter
    (fun mode ->
      equal ~msg:mode Testable.string
        "result 1\nstep a\nresult 2\nnote 3\nstep b\nFRAME\n" (piped mode))
    [ "fmt-order"; "fmt-order-adapted" ]

(* ───── Tasks ───── *)

let progress mode name = piped ~env:[ ("CLI_PROGRESS", mode) ] name

let build_lines =
  [
    "\u{2022} RUN make";
    "  \u{203A} RUN make:   CC src/file0.c";
    "  \u{203A} RUN make:   CC src/file1.c";
    "  \u{203A} RUN make:   CC src/file2.c";
    "  \u{203A} RUN make:   CC src/file3.c";
    "  \u{203A} RUN make:   CC src/file4.c";
    "  \u{203A} RUN make:   CC src/file5.c";
    "\u{2022} RUN make \u{25B8} sha256:abcd";
  ]

let test_tasks_plain () =
  equal ~msg:"succeeded" strings
    (build_lines
    @ [
        "\u{2713} RUN make \u{25B8} sha256:abcd  2.00 GiB / 2.00 GiB  0.0s";
        "\u{2713} RUN make  5/5  0.0s";
        "\u{2713} Building  0.0s  2/2";
        "DONE";
        "";
      ])
    (String.split_on_char '\n' (progress "plain" "tasks"));
  equal ~msg:"failed" strings
    (build_lines
    @ [
        "  \u{203A} RUN make \u{25B8} sha256:abcd: checksum mismatch";
        "\u{2717} RUN make \u{25B8} sha256:abcd  2.00 GiB / 2.00 GiB  0.0s  \
         failed";
        "\u{2717} RUN make  5/5  0.0s  failed";
        "\u{2717} Building  0.0s  2/2";
        "DONE";
        "";
      ])
    (String.split_on_char '\n' (progress "plain" "tasks-fail"))

let test_tasks_quiet () =
  equal ~msg:"succeeded" Testable.string "DONE\n" (progress "quiet" "tasks");
  equal ~msg:"each failure, then all its output" strings
    [
      "\u{2717} RUN make \u{25B8} sha256:abcd  2.00 GiB / 2.00 GiB  0.0s  \
       failed";
      "  \u{203A} checksum mismatch";
      "\u{2717} RUN make  5/5  0.0s  failed";
      "  \u{203A}   CC src/file0.c";
      "  \u{203A}   CC src/file1.c";
      "  \u{203A}   CC src/file2.c";
      "  \u{203A}   CC src/file3.c";
      "  \u{203A}   CC src/file4.c";
      "  \u{203A}   CC src/file5.c";
      "DONE";
      "";
    ]
    (String.split_on_char '\n' (progress "quiet" "tasks-fail"))

let test_tasks_json () =
  let task id path =
    Printf.sprintf "\"id\":%d,\"task\":[%s]" id
      (String.concat "," (List.map (Printf.sprintf "\"%s\"") path))
  in
  let ev s = "{\"time\":0," ^ s ^ "}" in
  let step = task 0 [ "RUN make" ]
  and layer = task 1 [ "RUN make"; "sha256:abcd" ] in
  let items i =
    [
      ev
        (Printf.sprintf
           "\"event\":\"progress\",%s,\"count\":%d,\"total\":5,\"unit\":\"items\""
           step (min i 5));
      ev
        (Printf.sprintf "\"event\":\"output\",%s,\"text\":\"  CC src/file%d.c\""
           step i);
    ]
  in
  let bytes i =
    ev
      (Printf.sprintf
         "\"event\":\"progress\",%s,\"count\":%d,\"total\":2147483648,\"unit\":\"bytes\""
         layer (i * 268435456))
  in
  equal ~msg:"events" strings
    ([ ev ("\"event\":\"start\"," ^ step) ]
    @ List.concat_map items [ 0; 1; 2; 3; 4; 5 ]
    @ [ ev ("\"event\":\"start\"," ^ layer ^ ",\"parent\":0") ]
    @ List.map bytes [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
    @ [
        ev ("\"event\":\"output\"," ^ layer ^ ",\"text\":\"checksum mismatch\"");
        ev
          ("\"event\":\"end\"," ^ layer ^ ",\"status\":\"failed\",\"elapsed\":0");
        ev ("\"event\":\"end\"," ^ step ^ ",\"status\":\"failed\",\"elapsed\":0");
        ev "\"event\":\"end\",\"status\":\"failed\",\"elapsed\":0";
        "DONE";
        "";
      ])
    (String.split_on_char '\n' (progress "json" "tasks-fail"))

(* Two works on one output, one inside the other, number their tasks
   apart. *)
let test_tasks_ids () =
  equal ~msg:"events" strings
    [
      "{\"time\":0,\"event\":\"start\",\"id\":0,\"task\":[\"outer\"]}";
      "{\"time\":0,\"event\":\"start\",\"id\":1,\"task\":[\"inner\"]}";
      "{\"time\":0,\"event\":\"end\",\"id\":1,\"task\":[\"inner\"],\"status\":\"succeeded\",\"elapsed\":0}";
      "{\"time\":0,\"event\":\"end\",\"status\":\"succeeded\",\"elapsed\":0}";
      "{\"time\":0,\"event\":\"end\",\"id\":0,\"task\":[\"outer\"],\"status\":\"succeeded\",\"elapsed\":0}";
      "{\"time\":0,\"event\":\"end\",\"status\":\"succeeded\",\"elapsed\":0}";
      "";
    ]
    (String.split_on_char '\n' (progress "json" "tasks-nested"))

(* Tasks told from four threads: each task's lines in the order of its
   changes. *)
let test_tasks_threads () =
  let lines = String.split_on_char '\n' (progress "plain" "tasks-threads") in
  let index l =
    let rec go i = function
      | [] -> failf "no line %S" l
      | x :: _ when x = l -> i
      | _ :: rest -> go (i + 1) rest
    in
    go 0 lines
  in
  for k = 0 to 3 do
    for i = 1 to 20 do
      let label = Printf.sprintf "t%d-%d" k i in
      let start = index ("\u{2022} " ^ label)
      and out = index ("  \u{203A} " ^ label ^ ": x")
      and stop = index ("\u{2713} " ^ label ^ "  0.0s") in
      if not (start < out && out < stop) then failf "%s out of order" label
    done
  done

let test_tasks_endings () =
  equal ~msg:"told after it ended" strings
    [
      "\u{2022} a";
      "\u{2713} a  0.0s";
      "UNDER CANCELLED";
      "\u{2713} 0.0s  2/2";
      "";
    ]
    (String.split_on_char '\n' (progress "plain" "tasks-after"));
  equal ~msg:"transient and cancelled" strings
    [
      "\u{2022} skipped";
      "- skipped  0.0s  cancelled";
      "\u{2717} broken  Failure(\"broken\")  0.0s  failed";
      "  \u{203A} why";
      "\u{2713} 0.0s  3/3";
      "";
    ]
    (String.split_on_char '\n' (progress "plain" "tasks-transient"));
  equal ~msg:"quiet: the failure, not the cancelled" strings
    [
      "\u{2717} broken  Failure(\"broken\")  0.0s  failed"; "  \u{203A} why"; "";
    ]
    (String.split_on_char '\n' (progress "quiet" "tasks-transient"));
  equal ~msg:"a raise fails the work" strings
    [ "\u{2717} 0.0s  0/0"; "RAISED"; "" ]
    (String.split_on_char '\n' (progress "plain" "tasks-raise"));
  equal ~msg:"an ending f gave the work counts" strings
    [ "\u{2713} early  0.0s  0/0"; "DONE"; "" ]
    (String.split_on_char '\n' (progress "plain" "tasks-ended"));
  is_true ~msg:"a count that goes back starts the rate again"
    (List.mem "RATE true false"
       (String.split_on_char '\n' (progress "plain" "tasks-back")))

(* The index of the first line of [term] that starts with [prefix]. *)
let line_index term prefix =
  let rec go i = function
    | [] -> failf "no line starts with %S" prefix
    | l :: _ when String.starts_with ~prefix l -> i
    | _ :: rest -> go (i + 1) rest
  in
  go 0 (lines term)

let test_tasks_tty () =
  ( spawn ~cols:80 ~rows:24 "tasks-fail" @@ fun term c ->
    exited 0 (T.Child.exit c ~within:10.);
    let at = line_index term in
    let order =
      [
        at "\u{2717} RUN make ";
        at "\u{2514}\u{2500}\u{2500} \u{2717} sha256:abcd ";
        at "  RUN make \u{203A}   CC src/file0.c";
        at "  RUN make \u{203A}   CC src/file5.c";
        at "  RUN make \u{25B8} sha256:abcd \u{203A} checksum mismatch";
        at "\u{2717} Building";
        at "DONE";
      ]
    in
    equal ~msg:"the tree, then each failure's output, then the summary"
      (list int) (List.sort compare order) order;
    usable term );
  spawn ~cols:60 ~rows:12 "tasks-tree" @@ fun term c ->
  T.Child.wait c ~within:10. (fun t ->
      shows "\u{2717} one" t && shows " two" t && shows "Testing" t);
  is_true ~msg:"the window shows the latest lines" (shows "\u{203A} c" term);
  is_false ~msg:"and no older one" (shows "\u{203A} a" term);
  is_false ~msg:"no transient task" (shows "quick" term);
  T.Child.send c "\n";
  exited 0 (T.Child.exit c ~within:10.);
  let at = line_index term in
  is_true ~msg:"the suite above its summary"
    (at "\u{2713} suite" < at "\u{2713} Testing");
  is_true ~msg:"a failed test's output after it"
    (at "  suite \u{25B8} one \u{203A} expected 1" > at "\u{2713} suite");
  is_false ~msg:"no transient task"
    (List.exists (fun l -> contains l "quick") (lines term))

let test_tasks_fold () =
  spawn ~cols:60 ~rows:6 "tasks-fold" @@ fun term c ->
  T.Child.wait c ~within:10. (shows "10 done, 2 failed");
  is_true ~msg:"the running test" (shows " last" term);
  T.Child.send c "\n";
  exited 0 (T.Child.exit c ~within:10.)

let () =
  run "matrix.cli.pty"
    [
      group "live"
        [
          test "lines above, the last frame stays" test_progress;
          test "a frame taller than the screen" test_tall;
          test "a line waits for its end" test_partial;
          test "a frame of grids" test_stack;
          test "a tick" test_tick;
          test "the height of a frame" test_height;
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
          test "how progress shows" test_progress_of;
          test "quiet and json" test_quiet_json;
        ];
      group "tasks"
        [
          test "plain" test_tasks_plain;
          test "quiet" test_tasks_quiet;
          test "json" test_tasks_json;
          test "ids of nested works" test_tasks_ids;
          test "lines from threads" test_tasks_threads;
          test "endings" test_tasks_endings;
          test "rows" test_tasks_tty;
          test "a fold" test_tasks_fold;
        ];
      group "prompts"
        [
          test "a line" test_line;
          test "a line asked on a thread" test_line_thread;
          test "a confirmation" test_confirm;
          test "lines held while asked" test_line_held;
          test "a secret" test_secret;
          test "a masked secret" test_secret_mask;
          test "a choice" test_choose;
          test "a styled question" test_secret_styled;
          test "Ctrl+C in a secret" test_secret_interrupted;
          test "Ctrl+Z in a secret" test_secret_stop;
          test "keys" test_keys;
        ];
    ]
