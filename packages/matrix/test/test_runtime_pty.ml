(* The runtime in a pseudo-terminal: signals, job control and handing the
   terminal to a child, with a real terminal driver. The program under test
   is runtime_child.ml. *)

open Windtrap

let child =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_child.exe"

type session = { pty : Pty.t; pid : int; output : Buffer.t }

let spawn ?(args = []) ?(rows = 24) ?(cols = 80) () =
  let pty =
    Pty.spawn ~prog:child ~args
      ~winsize:Pty.{ rows; cols; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }

(* Reads what the program writes until [sub] appears after [from]. The
   timeout guards against a program that never writes it; it measures
   nothing, so a loaded host only makes a test slower. *)
let contains ~from out sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length out && (String.sub out i n = sub || go (i + 1))
  in
  go from

(* Reads until one of [subs] appears after [from], and is the first of them
   that has. *)
let await_any ?(timeout = 60.) ?(from = 0) s subs =
  let deadline = Unix.gettimeofday () +. timeout in
  let buf = Bytes.create 4096 in
  let found () =
    let out = Buffer.contents s.output in
    List.find_opt (contains ~from out) subs
  in
  while found () = None && Unix.gettimeofday () < deadline do
    match Unix.select [ Pty.in_fd s.pty ] [] [] 0.05 with
    | [], _, _ -> ()
    | _ -> (
        match Pty.read s.pty buf 0 4096 with
        | n when n > 0 -> Buffer.add_subbytes s.output buf 0 n
        | _ -> ()
        | exception Unix.Unix_error _ -> ())
  done;
  match found () with
  | Some sub -> sub
  | None ->
      failf "timed out waiting for %s in %S"
        (String.concat " or " subs)
        (Buffer.contents s.output)

let await ?timeout ?from s sub =
  ignore (await_any ?timeout ?from s [ sub ] : string)

let send s bytes =
  ignore (Pty.write_string s.pty bytes 0 (String.length bytes) : int)

let mark s = Buffer.length s.output

(* Waits for the program to end, reading what it writes to its terminal and
   to [also]'s: on macOS, a terminal's settings are put back (TCSADRAIN)
   only once what was written to it has been read. *)
let rec wait_exit ?(timeout = 60.) ?also s =
  match Unix.waitpid [ Unix.WNOHANG ] s.pid with
  | 0, _ ->
      if timeout <= 0. then (
        Unix.kill s.pid Sys.sigkill;
        fail "the program did not end")
      else (
        Unix.sleepf 0.02;
        (* Keep draining what it writes. *)
        (try await ~timeout:0.01 s "\000\001" with _ -> ());
        Option.iter
          (fun t -> try await ~timeout:0.01 t "\000\001" with _ -> ())
          also;
        wait_exit ~timeout:(timeout -. 0.03) ?also s)
  | _, status -> status

let cooked s =
  let t = Unix.tcgetattr (Pty.file_descr s.pty) in
  t.c_icanon && t.c_echo && t.c_isig

let finish s = try Pty.close s with _ -> ()

let with_session ?args ?rows ?cols f =
  let s = spawn ?args ?rows ?cols () in
  Fun.protect ~finally:(fun () -> finish s.pty) (fun () -> f s)

(* A termination signal ends the run: the terminal is given back, then the
   signal is raised again, so the process ends with its status. *)
let test_sigterm () =
  with_session @@ fun s ->
  await s "READY";
  let from = mark s in
  Unix.kill s.pid Sys.sigterm;
  match wait_exit s with
  | WSIGNALED n ->
      equal ~msg:"ends by SIGTERM" int Sys.sigterm n;
      await ~from s "\027[?1049l";
      await ~from s "\027[?1004l";
      is_true ~msg:"cooked" (cooked s)
  | _ -> fail "expected the process to end by its signal"

(* A program in its own session is an orphaned process group, which a stop
   signal must not stop: the terminal is given back and taken again, and the
   run goes on. *)
let test_sigtstp_in_an_orphaned_group () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  Unix.kill s.pid Sys.sigtstp;
  await ~from s "\027[?1049l";
  await ~from s "\027[?1049h";
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> is_true ~msg:"cooked after" (cooked s)
  | _ -> fail "expected a normal exit"

(* A SIGCONT the run did not cause: the modes are set again and the screen
   drawn whole. *)
let test_unsolicited_sigcont () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  Unix.kill s.pid Sys.sigcont;
  await ~from s "\027[?1004h";
  await ~from s "READY";
  send s "q";
  ignore (wait_exit s : Unix.process_status)

(* A child given the terminal finds it cooked, on the primary screen, and
   no reply typed for it; the application's screen is drawn whole after. *)
let test_handoff () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "e";
  await ~from s "CHILD";
  let out = Buffer.contents s.output in
  let after = String.sub out from (String.length out - from) in
  let contains sub =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length after && (String.sub after i n = sub || go (i + 1))
    in
    go 0
  in
  is_true ~msg:"the alternate screen is left first" (contains "\027[?1049l");
  is_true ~msg:"icanon" (contains " icanon");
  is_false ~msg:"not -icanon" (contains "-icanon");
  is_false ~msg:"not -echo " (contains "-echo ");
  is_false ~msg:"not -isig" (contains "-isig");
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with WEXITED 0 -> () | _ -> fail "expected a normal exit"

(* SIGINT received while the terminal is given away was the child's: the
   runtime discards it. *)
let test_sigint_during_release () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "i";
  await ~from s "\027[?1049l";
  Unix.kill s.pid Sys.sigint;
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> ()
  | _ -> fail "SIGINT during the release ended the run"

(* abort() returns to no point where OCaml runs: the handler restores the
   terminal itself. *)
let test_sigabrt_restores_the_terminal () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "a";
  match wait_exit s with
  | WSIGNALED n ->
      equal ~msg:"ends by SIGABRT" int Sys.sigabrt n;
      await ~from s "\027[?1049l";
      is_true ~msg:"cooked" (cooked s)
  | _ -> fail "expected the process to end by SIGABRT"

(* Job control: the program runs as a job of its own process group under a
   shell (runtime_job.ml) in the same session, so a stop signal stops it. *)

let job =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_job.exe"

let spawn_job () =
  let pty =
    Pty.spawn ~prog:job ~args:[ child ]
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }

(* The job's process, which the shell reports. *)
(* A job that stops before its first frame fails the test at once, saying
   by which signal: no test stops it before then. *)
let job_pid s =
  await s "JOB ";
  if await_any s [ "READY 1"; "JOB-STOPPED" ] <> "READY 1" then (
    ignore (await_any s [ "\r\n" ] : string);
    failf "the job stopped before its first frame: %S"
      (Buffer.contents s.output));
  let out = Buffer.contents s.output in
  let i = Option.get (String.index_from_opt out 0 'J') in
  Scanf.sscanf (String.sub out i (String.length out - i)) "JOB %d" Fun.id

(* What a command prints, with [stdin] as its input. *)
let output_of ?(stdin = Unix.stdin) prog args =
  let r, w = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process prog (Array.of_list (prog :: args)) stdin w Unix.stderr
  in
  Unix.close w;
  let ic = Unix.in_channel_of_descr r in
  let out = In_channel.input_all ic in
  close_in ic;
  ignore (Unix.waitpid [] pid : int * Unix.process_status);
  out

(* The state of a process, ["T"] when it is stopped, and the CPU time it
   has used: from /proc where there is one (Linux), in clock ticks, and
   from ps(1) elsewhere, such as macOS. *)
let stat pid =
  let proc = Printf.sprintf "/proc/%d/stat" pid in
  if Sys.file_exists proc then
    let line = In_channel.with_open_text proc In_channel.input_all in
    let after =
      String.sub line
        (String.rindex line ')' + 2)
        (String.length line - String.rindex line ')' - 2)
    in
    let fields = String.split_on_char ' ' after in
    ( List.nth fields 0,
      string_of_int
        (int_of_string (List.nth fields 11) + int_of_string (List.nth fields 12))
    )
  else
    match
      String.split_on_char ' '
        (String.trim
           (output_of "ps"
              [ "-o"; "state="; "-o"; "time="; "-p"; string_of_int pid ]))
      |> List.filter (( <> ) "")
    with
    | [ state; time ] -> (String.sub state 0 1, time)
    | _ -> failf "ps: no process %d" pid

(* A stop in a job: while stopped the terminal is cooked and on the primary
   screen; continued in the foreground, the modes are set again and the
   screen drawn whole. *)
let test_sigtstp_stops_the_job () =
  let s = spawn_job () in
  Fun.protect ~finally:(fun () -> finish s.pty) @@ fun () ->
  let pid = job_pid s in
  let from = mark s in
  Unix.kill pid Sys.sigtstp;
  await ~from s "JOB-STOPPED TSTP";
  equal ~msg:"stopped" string "T" (fst (stat pid));
  await ~from s "\027[?1049l";
  is_true ~msg:"cooked while stopped" (cooked s);
  let from = mark s in
  send s "f\n";
  await ~from s "\027[?1049h";
  await ~from s "\027[?1004h";
  await ~from s "READY 2";
  send s "q";
  await s "JOB-EXIT 0"

(* Continued in the background, the job stops again when it sets the
   terminal (SIGTTOU), without spinning, until it is in the foreground. *)
let test_background_continue_stops_again () =
  let s = spawn_job () in
  Fun.protect ~finally:(fun () -> finish s.pty) @@ fun () ->
  let pid = job_pid s in
  let from = mark s in
  Unix.kill pid Sys.sigtstp;
  await ~from s "JOB-STOPPED TSTP";
  let from = mark s in
  send s "b\n";
  await ~from s "JOB-STOPPED TTOU";
  let state, cpu = stat pid in
  equal ~msg:"stopped again" string "T" state;
  Unix.sleepf 0.3;
  equal ~msg:"no CPU while stopped" string cpu (snd (stat pid));
  let from = mark s in
  send s "f\n";
  await ~from s "READY 2";
  send s "q";
  await s "JOB-EXIT 0"

(* Two runs in one process: the terminal's, and a session served on another
   pseudo-terminal, which the test holds. A termination signal ends both:
   whichever acts on it gives its terminal back, the other's terminal is
   reset by the path an abort takes, and the process ends by the signal. An
   abort resets both. *)

let served_terminal ?(mode = "served") f =
  let master, slave = Pty.open_pty () in
  Pty.set_nonblock master;
  let path = String.trim (output_of ~stdin:(Pty.file_descr slave) "tty" []) in
  let session =
    let pty =
      Pty.spawn ~prog:child ~args:[ mode; path ]
        ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
        ()
    in
    Pty.set_nonblock pty;
    { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }
  in
  let served =
    { pty = master; pid = session.pid; output = Buffer.create 4096 }
  in
  Fun.protect
    ~finally:(fun () ->
      finish session.pty;
      (try Pty.close master with _ -> ());
      try Pty.close slave with _ -> ())
    (fun () -> f session served slave)

let served_cooked slave =
  let t = Unix.tcgetattr (Pty.file_descr slave) in
  t.c_icanon && t.c_echo

let test_termination_ends_every_run () =
  served_terminal @@ fun s served slave ->
  await s "READY 1";
  await served "SERVED";
  is_false ~msg:"the served terminal is raw" (served_cooked slave);
  let from = mark s and served_from = mark served in
  Unix.kill s.pid Sys.sigterm;
  (match wait_exit ~also:served s with
  | WSIGNALED n -> equal ~msg:"ends by SIGTERM" int Sys.sigterm n
  | _ -> fail "expected the process to end by its signal");
  await ~from s "\027[?1049l";
  is_true ~msg:"the terminal cooked" (cooked s);
  await ~from:served_from served "\027[?1049l";
  is_true ~msg:"the served terminal cooked" (served_cooked slave)

(* A process whose only run is a session it serves, as a server's: a
   termination signal resets that session's terminal before the process
   ends. *)
let test_termination_resets_a_served_session () =
  served_terminal ~mode:"only-served" @@ fun s served slave ->
  await served "SERVED";
  is_false ~msg:"the served terminal is raw" (served_cooked slave);
  let served_from = mark served in
  Unix.kill s.pid Sys.sigterm;
  (match wait_exit ~also:served s with
  | WSIGNALED n -> equal ~msg:"ends by SIGTERM" int Sys.sigterm n
  | _ -> fail "expected the process to end by its signal");
  await ~from:served_from served "\027[?1049l";
  is_true ~msg:"the served terminal cooked" (served_cooked slave)

let test_abort_resets_every_run () =
  served_terminal @@ fun s served slave ->
  await s "READY 1";
  await served "SERVED";
  let from = mark s and served_from = mark served in
  send s "a";
  (match wait_exit ~also:served s with
  | WSIGNALED n -> equal ~msg:"ends by SIGABRT" int Sys.sigabrt n
  | _ -> fail "expected the process to end by SIGABRT");
  await ~from s "\027[?1049l";
  is_true ~msg:"the terminal cooked" (cooked s);
  await ~from:served_from served "\027[?1049l";
  is_true ~msg:"the served terminal cooked" (served_cooked slave)

(* The probe measures text sizing (OSC 66) by writing spaces and asking
   where the cursor went, and must write only on cells the program owns.
   The test is the terminal: a model of it shows the shell's text, draws
   what the program writes, with each OSC 66 space as the cells it fills on
   its first row, as kitty draws it, and answers the program's queries. *)

let replace_all ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then
      Buffer.add_substring b s i (String.length s - i)
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let rec find_from s i sub =
  let n = String.length sub in
  if i > String.length s - n then None
  else if String.sub s i n = sub then Some i
  else find_from s (i + 1) sub

(* Bytes up to the last OSC still open are drawn; the rest waits for more. *)
let drawable s =
  let rec last_osc i found =
    match find_from s i "\027]" with
    | Some j -> last_osc (j + 1) (Some j)
    | None -> found
  in
  let held =
    match last_osc 0 None with
    | Some i when find_from s i "\027\\" = None && find_from s i "\007" = None
      ->
        i
    | _ ->
        let n = String.length s in
        if n > 0 && s.[n - 1] = '\027' then n - 1 else n
  in
  (String.sub s 0 held, String.sub s held (String.length s - held))

let kitty_sizing s =
  s
  |> replace_all ~sub:"\027]66;w=1; \027\\" ~by:" "
  |> replace_all ~sub:"\027]66;s=2; \027\\" ~by:"  "

let shell_text = [ "$ one"; "$ two"; "$ three" ]

let test_probe_leaves_the_shell_text mode () =
  let pty =
    Pty.spawn ~prog:child ~args:mode
      ~env:[| "TERM=xterm-256color" |]
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  let s =
    { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }
  in
  Fun.protect ~finally:(fun () -> finish s.pty) @@ fun () ->
  let vte = Vte.create ~scrollback:100 ~rows:24 ~cols:80 () in
  ignore
    (Vte.feed_string vte (String.concat "\r\n" shell_text ^ "\r\n")
      : Matrix_input.Reply.t list);
  let pending = ref "" in
  let buf = Bytes.create 4096 in
  let pump () =
    match Unix.select [ Pty.in_fd s.pty ] [] [] 0.05 with
    | [], _, _ -> ()
    | _ -> (
        match Pty.read s.pty buf 0 4096 with
        | n when n > 0 ->
            let chunk = Bytes.sub_string buf 0 n in
            Buffer.add_string s.output chunk;
            let now, rest = drawable (!pending ^ chunk) in
            pending := rest;
            let replies = Vte.feed_string vte (kitty_sizing now) in
            if replies <> [] then send s (Matrix_input.Reply.encode replies)
        | _ -> ()
        | exception Unix.Unix_error _ -> ())
  in
  let until ?(timeout = 60.) what f =
    let deadline = Unix.gettimeofday () +. timeout in
    while (not (f ())) && Unix.gettimeofday () < deadline do
      pump ()
    done;
    if not (f ()) then failf "timed out waiting for %s" what
  in
  let shows sub () = find_from (Buffer.contents s.output) 0 sub <> None in
  (* The shell's rows, in the scrollback and on the screen. In [`Primary]
     mode the live region may scroll one into the scrollback, for its
     origin. *)
  let shell () =
    let n = Vte.scrollback_size vte in
    List.filteri
      (fun i _ -> i < List.length shell_text)
      (List.init (n + 24) (fun i -> String.trim (Vte.text vte (i - n))))
  in
  until "the first frame" (shows "READY 1");
  if mode <> [] then
    equal ~msg:"the shell's text while the program runs" (list string)
      shell_text (shell ());
  send s "q";
  let status = ref None in
  until "the end" (fun () ->
      (if !status = None then
         match Unix.waitpid [ Unix.WNOHANG ] s.pid with
         | 0, _ -> ()
         | _, st -> status := Some st);
      !status <> None);
  (* What the program wrote last. *)
  for _ = 1 to 5 do
    pump ()
  done;
  equal ~msg:"the shell's text after the program" (list string) shell_text
    (shell ())

(* Keys typed faster than the frame interval each ask for a frame before the
   last one's interval has passed: the run waits until that frame's
   deadline, and every key is painted. *)
let test_keys_faster_than_frames () =
  with_session ~args:[ "keys" ] @@ fun s ->
  await s "KEYS:.";
  let letters = "abcdefghijklmnoprstuvwxyz" in
  String.iter
    (fun c ->
      send s (String.make 1 c);
      Unix.sleepf 0.002)
    letters;
  let shown () =
    let vte = Vte.create ~rows:24 ~cols:80 () in
    ignore
      (Vte.feed_string vte (Buffer.contents s.output)
        : Matrix_input.Reply.t list);
    String.trim (Vte.text vte 0)
  in
  let expected = "KEYS:" ^ letters ^ "." in
  let deadline = Unix.gettimeofday () +. 10. in
  while shown () <> expected && Unix.gettimeofday () < deadline do
    try await ~timeout:0.05 s "\000\001" with _ -> ()
  done;
  equal ~msg:"every key painted" string expected (shown ());
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> is_true ~msg:"cooked after" (cooked s)
  | _ -> fail "expected a normal exit"

(* What a terminal of [cols] by [rows] shows of the output so far. *)
let shown ?(rows = 24) ?(cols = 80) s =
  let vte = Vte.create ~rows ~cols () in
  ignore
    (Vte.feed_string vte (Buffer.contents s.output) : Matrix_input.Reply.t list);
  vte

(* A run on the controlling terminal takes SIGWINCH, which the kernel sends
   when the terminal's size changes, and draws its next frame at the new
   size. *)
let test_sigwinch () =
  with_session ~args:[ "goodbye" ] @@ fun s ->
  await s "SIZE 80x24";
  let from = mark s in
  Pty.resize s.pty ~rows:5 ~cols:30;
  await ~from s "SIZE 30x5";
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> is_true ~msg:"cooked after" (cooked s)
  | _ -> fail "expected a normal exit"

(* A terminal that takes no output holds the run in its write: frames
   larger together than what the terminal buffers wait until it reads, and
   none is lost or cut. *)
let test_back_pressure () =
  let rows = 100 and cols = 300 in
  with_session ~args:[ "fill" ] ~rows ~cols @@ fun s ->
  await s "FILLED .";
  let from = mark s in
  (* Each frame rewrites every cell, 30,000 bytes and more: one outgrows
     the terminal's buffer (about 20 KB on Linux, less on macOS), which
     nothing reads meanwhile, and the keys typed meanwhile wait for the run.
     How many frames the run draws depends on when its writes are held. *)
  String.iter
    (fun c ->
      send s (String.make 1 c);
      Unix.sleepf 0.05)
    "abcde";
  Unix.sleepf 0.3;
  (match Unix.waitpid [ Unix.WNOHANG ] s.pid with
  | 0, _ -> ()
  | _ -> fail "the program ended while its output was held");
  let deadline = Unix.gettimeofday () +. 10. in
  let complete () =
    let vte = shown ~rows ~cols s in
    Vte.text vte 0 = "FILLED e"
    && Vte.text vte (rows - 1) = String.make cols 'e'
  in
  while (not (complete ())) && Unix.gettimeofday () < deadline do
    try await ~timeout:0.05 s " " with _ -> ()
  done;
  let written = Buffer.length s.output - from in
  is_true
    ~msg:(Printf.sprintf "%d bytes, more than the terminal buffers" written)
    (written > 30_000);
  let vte = shown ~rows ~cols s in
  equal ~msg:"the last frame, whole" string "FILLED e" (Vte.text vte 0);
  for y = 1 to rows - 1 do
    equal
      ~msg:(Printf.sprintf "row %d" y)
      string (String.make cols 'e') (Vte.text vte y)
  done;
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> is_true ~msg:"cooked after" (cooked s)
  | _ -> fail "expected a normal exit"

(* A program that calls [exit] inside a run ends with the terminal given
   back: its exit path restores it, as an abort does. *)
let test_exit_inside_a_run () =
  with_session @@ fun s ->
  await s "READY";
  let from = mark s in
  send s "x";
  match wait_exit s with
  | WEXITED 3 ->
      await ~from s "\027[?1049l";
      is_true ~msg:"cooked" (cooked s)
  | _ -> fail "expected the program's exit status"

let supported =
  Sys.os_type = "Unix" && Sys.file_exists child && Sys.file_exists job

let () =
  run "matrix.runtime.pty"
    [
      group "Signals"
        (if not supported then []
         else
           [
             test "SIGTERM ends with its status" test_sigterm;
             test "SIGTSTP in an orphaned group"
               test_sigtstp_in_an_orphaned_group;
             test "SIGTSTP stops the job" test_sigtstp_stops_the_job;
             test "continued in the background, it stops again"
               test_background_continue_stops_again;
             test "unsolicited SIGCONT" test_unsolicited_sigcont;
             test "SIGINT during a release is discarded"
               test_sigint_during_release;
             test "SIGABRT restores the terminal"
               test_sigabrt_restores_the_terminal;
           ]);
      group "Several runs"
        (if not supported then []
         else
           [
             test "a termination signal ends every run"
               test_termination_ends_every_run;
             test "an abort resets every run" test_abort_resets_every_run;
             test "a termination signal resets a served session"
               test_termination_resets_a_served_session;
           ]);
      group "Exit"
        (if not supported then []
         else [ test "exit inside a run" test_exit_inside_a_run ]);
      group "Frames"
        (if not supported then []
         else
           [
             test "keys faster than frames are painted"
               test_keys_faster_than_frames;
             test "SIGWINCH has the next frame drawn at the new size"
               test_sigwinch;
             test "a terminal that takes no output holds the run"
               test_back_pressure;
           ]);
      group "Handoff"
        (if not supported then []
         else [ test "a child's terminal" test_handoff ]);
      group "Probe"
        (if not supported then []
         else
           [
             test "the alternate screen leaves the shell's text"
               (test_probe_leaves_the_shell_text []);
             test "the primary screen leaves the shell's text"
               (test_probe_leaves_the_shell_text [ "primary" ]);
           ]);
    ]
