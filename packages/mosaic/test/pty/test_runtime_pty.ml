(* Mosaic's runtime in a pseudo-terminal, on a real clock: timers, latency,
   wakeups, floods, exceptions off the loop, and handing the terminal to a
   child. The programs are program.ml's. The terminal answers the runtime's
   device attributes and cursor queries, as a terminal does.

   Run with the argument [measure] to print what each scenario measured
   instead of testing it. *)

open Windtrap

let here = Filename.dirname Sys.executable_name
let program = Filename.concat here "program.exe"

let job =
  Filename.concat here
    (Filename.concat Filename.parent_dir_name
       (Filename.concat Filename.parent_dir_name
          (Filename.concat Filename.parent_dir_name
             "matrix/test/runtime_job.exe")))

type session = {
  pty : Pty.t;
  pid : int;
  output : Buffer.t;
  (* When the output reached each length. *)
  mutable arrivals : (float * int) list;
  mutable pending : string;
}

let spawn ?(prog = program) name =
  Unix.putenv "MOSAIC_PTY_PROGRAM" name;
  let pty =
    Pty.spawn ~prog
      ~args:(if prog == program then [ name ] else [ program ])
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  {
    pty;
    pid = Option.get (Pty.pid pty);
    output = Buffer.create 4096;
    arrivals = [];
    pending = "";
  }

let send s bytes =
  ignore (Pty.write_string s.pty bytes 0 (String.length bytes) : int)

let find ?(from = 0) haystack sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length haystack then None
    else if String.sub haystack i n = sub then Some i
    else go (i + 1)
  in
  go from

(* Reads what is ready, answering the queries a terminal answers. *)
let pump ?(timeout = 0.01) s =
  let buf = Bytes.create 65536 in
  match Unix.select [ Pty.in_fd s.pty ] [] [] timeout with
  | [], _, _ -> ()
  | _ -> (
      match Pty.read s.pty buf 0 65536 with
      | n when n > 0 ->
          let data = Bytes.sub_string buf 0 n in
          Buffer.add_string s.output data;
          s.arrivals <-
            (Unix.gettimeofday (), Buffer.length s.output) :: s.arrivals;
          let text = s.pending ^ data in
          let rec answer i =
            let da1 = find ~from:i text "\027[c"
            and cpr = find ~from:i text "\027[6n" in
            match (da1, cpr) with
            | Some a, Some b when a < b ->
                send s "\027[?62;22c";
                answer (a + 3)
            | Some a, None ->
                send s "\027[?62;22c";
                answer (a + 3)
            | _, Some b ->
                send s "\027[1;1R";
                answer (b + 4)
            | None, None -> i
          in
          let consumed = answer 0 in
          let keep = String.length text - consumed in
          s.pending <-
            String.sub text (String.length text - min keep 8) (min keep 8)
      | _ -> ()
      | exception Unix.Unix_error _ -> ())

let mark s = Buffer.length s.output

(* The timeouts of the waits below guard against a program that never does
   what is waited for; they are no measure of time, so a loaded host only
   makes a test slower. *)
let await ?(timeout = 60.) ?(from = 0) s sub =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    match find ~from (Buffer.contents s.output) sub with
    | Some i -> i
    | None ->
        if Unix.gettimeofday () > deadline then
          failf "timed out waiting for %S" sub
        else (
          pump s;
          go ())
  in
  go ()

let pause s seconds =
  let until = Unix.gettimeofday () +. seconds in
  while Unix.gettimeofday () < until do
    pump ~timeout:(until -. Unix.gettimeofday ()) s
  done

let wait_exit ?(timeout = 60.) s =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    match Unix.waitpid [ Unix.WNOHANG ] s.pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then (
          Unix.kill s.pid Sys.sigkill;
          fail "the program did not end")
        else (
          pump s;
          go ())
    | _, status ->
        pump s;
        status
  in
  go ()

let finish s = try Pty.close s.pty with _ -> ()

let with_session ?prog name f =
  let s = spawn ?prog name in
  Fun.protect ~finally:(fun () -> finish s) (fun () -> f s)

(* The program's results, once it has ended. *)
let results s =
  Buffer.contents s.output |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.index_opt line 'R' with
      | Some i
        when String.length line > i + 7 && String.sub line i 7 = "RESULT " ->
          let rest =
            String.trim (String.sub line (i + 7) (String.length line - i - 7))
          in
          let k = String.index rest ' ' in
          Some
            ( String.sub rest 0 k,
              String.sub rest (k + 1) (String.length rest - k - 1) )
      | _ -> None)

let result s name = List.assoc name (results s)

(* Context switches of all of a process's threads, from /proc. *)
let switches pid =
  let dir = Printf.sprintf "/proc/%d/task" pid in
  Array.fold_left
    (fun total task ->
      let status =
        try
          In_channel.with_open_text
            (Printf.sprintf "%s/%s/status" dir task)
            In_channel.input_all
        with Sys_error _ -> ""
      in
      String.split_on_char '\n' status
      |> List.fold_left
           (fun total line ->
             match
               Scanf.sscanf_opt line "voluntary_ctxt_switches: %d" Fun.id
             with
             | Some n -> total + n
             | None -> (
                 match
                   Scanf.sscanf_opt line "nonvoluntary_ctxt_switches: %d" Fun.id
                 with
                 | Some n -> total + n
                 | None -> total))
           total)
    0
    (try Sys.readdir dir with Sys_error _ -> [||])

let percentile p xs =
  let a = Array.of_list xs in
  Array.sort Float.compare a;
  a.(min
       (Array.length a - 1)
       (int_of_float (p *. Float.of_int (Array.length a))))

(* Scenarios: each returns what it measured. *)

(* Wakeups per second of an idle program, over [seconds] after it settled. *)
let idle_wakeups ?(seconds = 3.) name =
  with_session name @@ fun s ->
  ignore (await s "=" : int);
  pause s 1.;
  let before = switches s.pid in
  pause s seconds;
  let rate = Float.of_int (switches s.pid - before) /. seconds in
  send s "q";
  ignore (wait_exit s : Unix.process_status);
  rate

(* A 0.25 s timer's fires: the periods ended so far, and the time since a
   point before the timer's origin. *)
let timer_fires () =
  with_session "timer" @@ fun s ->
  ignore (wait_exit s : Unix.process_status);
  List.filter_map
    (fun (name, value) ->
      if name = "fire" then Scanf.sscanf_opt value "%d %f" (fun n at -> (n, at))
      else None)
    (results s)

(* How late each fire is, after the grid point of the periods it counts. *)
let offsets fires =
  List.map (fun (n, at) -> at -. (0.25 *. Float.of_int n)) fires

(* The cell program.ml's counter shows for a count: painting a new count
   writes it. *)
let mark_of n = if n mod 2 = 0 then "\u{25CB}" else "\u{25CF}"

(* Seconds from typing [a] to the count it changes being painted, for each
   of [keys] keys typed [gap] seconds apart. *)
let key_to_paint ?(keys = 40) ?(gap = 0.05) name =
  with_session name @@ fun s ->
  ignore (await s "N=0" : int);
  pause s 0.5;
  let latencies =
    List.init keys (fun i ->
        let from = mark s in
        let t0 = Unix.gettimeofday () in
        send s "a";
        let painted = mark_of (i + 1) in
        let at = await ~from s painted in
        let arrived =
          List.find
            (fun (_, length) -> length > at)
            (List.rev s.arrivals |> List.filter (fun (_, l) -> l > from))
        in
        let latency = fst arrived -. t0 in
        pause s gap;
        latency)
  in
  send s "q";
  ignore (wait_exit s : Unix.process_status);
  latencies

type flood = {
  key_to_paint : float list;
  views : int;
  seconds : float;
  longest_gap : float;
  peak_kb : int;
}

(* Ten keys typed into a flood that lasts as long as the run, each 0.1 s
   after the previous one was painted. *)
let flood name =
  with_session name @@ fun s ->
  ignore (await s "N=0" : int);
  let started = Unix.gettimeofday () in
  pause s 0.5;
  let latencies = ref [] in
  let key n =
    let from = mark s in
    let t0 = Unix.gettimeofday () in
    send s "a";
    let at =
      try await ~from s (mark_of n)
      with e ->
        (* Whether the run went on drawing, and what it drew last: the view
           shows the flood's count as B. *)
        let out = Buffer.contents s.output in
        let last = match s.arrivals with (t, _) :: _ -> t | [] -> t0 in
        failf
          "key %d unpainted (%s); the keys before painted after %s s; %d bytes \
           since it was sent, the last %.1f s after it; the output ends %S"
          n (Printexc.to_string e)
          (String.concat ", " (List.rev_map (Printf.sprintf "%.3f") !latencies))
          (String.length out - from)
          (last -. t0)
          (String.sub out
             (max 0 (String.length out - 160))
             (min 160 (String.length out)))
    in
    let painted =
      fst
        (List.find
           (fun (_, length) -> length > at)
           (List.rev s.arrivals |> List.filter (fun (_, l) -> l > from)))
    in
    pause s 0.1;
    latencies := (painted -. t0) :: !latencies;
    painted -. t0
  in
  let key_to_paint = List.init 10 (fun i -> key (i + 1)) in
  let ended = Unix.gettimeofday () in
  send s "q";
  ignore (wait_exit s : Unix.process_status);
  let times =
    List.rev_map fst s.arrivals
    |> List.filter (fun t -> t >= started && t <= ended)
  in
  let longest_gap =
    fst
      (List.fold_left
         (fun (gap, last) t -> (max gap (t -. last), t))
         (0., started) times)
  in
  {
    key_to_paint;
    views = int_of_string (result s "views");
    seconds = float_of_string (result s "seconds");
    longest_gap;
    peak_kb = int_of_string (result s "peak_kb");
  }

(* Tests *)

let test_idle_costs_no_wakeups () =
  let rate = idle_wakeups "idle" in
  is_true ~msg:(Printf.sprintf "%.1f wakeups/s, at most 1" rate) (rate <= 1.)

(* The executor's domain ends with its work: a program that ran a perform
   idles as one that never did. *)
let test_idle_after_perform () =
  let rate = idle_wakeups "idle-after-perform" in
  is_true ~msg:(Printf.sprintf "%.1f wakeups/s, at most 1" rate) (rate <= 1.)

let test_idle_matrix () =
  let rate = idle_wakeups "idle-matrix" in
  is_true ~msg:(Printf.sprintf "%.1f wakeups/s, at most 1" rate) (rate <= 1.)

let test_idle_every () =
  let rate = idle_wakeups "idle-every" in
  is_true ~msg:(Printf.sprintf "%.1f wakeups/s, about 1" rate) (rate <= 3.)

(* At most a frame's tick a frame: a loaded host draws fewer frames, never
   more. *)
let test_idle_tick () =
  let rate = idle_wakeups "idle-tick" in
  is_true
    ~msg:(Printf.sprintf "%.1f wakeups/s, at most twice 60" rate)
    (rate > 0. && rate <= 120.)

(* Ticks while keys are typed 30 times a second: the frames drawn early for
   the keys tick nothing, so the ticks keep to one a frame interval, at 60
   frames a second; ticking in those frames too would add one a key. A
   loaded host ticks less, never more. *)
let test_ticks_while_typing () =
  with_session "tick-typing" @@ fun s ->
  ignore (await s "N=0" : int);
  pause s 0.3;
  for _ = 1 to 30 do
    send s "a";
    pause s 0.033
  done;
  pause s 0.2;
  send s "q";
  ignore (wait_exit s : Unix.process_status);
  let ticks = int_of_string (result s "ticks")
  and seconds = float_of_string (result s "seconds") in
  let most = Float.to_int (seconds *. 60.) + 6 in
  is_true
    ~msg:(Printf.sprintf "%d ticks in %.3f s, at most %d" ticks seconds most)
    (ticks <= most)

(* A timer's fires land on its grid: none comes before the grid point of
   the periods it counts, and the counts reach twelve periods. How late
   they come is the host's doing; "measure" prints it. *)
let test_timer_grid () =
  let fires = timer_fires () in
  equal ~msg:"twelve periods" int 12
    (fst (List.nth fires (List.length fires - 1)));
  ignore
    (List.fold_left
       (fun previous (n, at) ->
         is_true ~msg:"each fire counts a period at least" (n > previous);
         is_true
           ~msg:
             (Printf.sprintf "period %d not before its grid point (%.4f s)" n at)
           (at >= 0.25 *. Float.of_int n);
         n)
       0 fires
      : int)

(* Every key is painted; how soon is the host's doing, and "measure" prints
   it. *)
let test_key_to_paint name () =
  let latencies = key_to_paint name in
  equal ~msg:"every key painted" int 40 (List.length latencies)

let test_storm () =
  with_session "storm" @@ fun s ->
  (match wait_exit ~timeout:60. s with
  | WEXITED 0 -> ()
  | _ -> fail "the storm did not end normally");
  equal ~msg:"every message" string "200000" (result s "received");
  equal ~msg:"each producer's in order" string "0" (result s "disorder")

(* A flood that never ends cannot hold the keys back: each is painted
   while it goes on, a frame for each at least. *)
let test_flood name () =
  let f = flood name in
  equal ~msg:"every key painted" int 10 (List.length f.key_to_paint);
  is_true ~msg:"a frame for each key at least" (f.views > 10)

(* An exception in a perform ends the run once the terminal is given back,
   and nothing is written over the screen meanwhile. *)
let test_perform_exception () =
  with_session "perform-exn" @@ fun s ->
  let painted = await s "N=0" in
  ignore (wait_exit s : Unix.process_status);
  let out = Buffer.contents s.output in
  let restored = Option.get (find ~from:painted out "\027[?1049l") in
  is_true ~msg:"nothing written over the screen"
    (find ~from:painted (String.sub out 0 restored) "Thread" = None
    && find ~from:painted (String.sub out 0 restored) "perform-thread" = None);
  equal ~msg:"the run raised it" string "raised perform-thread"
    (result s "outcome")

(* An exception in [init] leaves the terminal as it was: [init] runs before
   the run takes it. *)
let test_init_exception () =
  with_session "init-exn" @@ fun s ->
  ignore (wait_exit s : Unix.process_status);
  equal ~msg:"the run raised it" string "raised init" (result s "outcome");
  equal ~msg:"cooked" string "true" (result s "cooked");
  is_true ~msg:"the alternate screen never entered"
    (find (Buffer.contents s.output) "\027[?1049h" = None)

let cooked s =
  let t = Unix.tcgetattr (Pty.file_descr s.pty) in
  t.c_icanon && t.c_echo && t.c_isig

(* A child given the terminal by Cmd.exec finds it cooked and on the primary
   screen; the program draws its screen whole after. *)
let test_exec_handoff () =
  with_session "handoff" @@ fun s ->
  ignore (await s "BACK 0" : int);
  let from = mark s in
  send s "e";
  let child = await ~from s "CHILD" in
  let during = String.sub (Buffer.contents s.output) from (child - from) in
  is_true ~msg:"the alternate screen is left first"
    (find during "\027[?1049l" <> None);
  is_true ~msg:"icanon" (find during " icanon" <> None);
  is_true ~msg:"not -icanon" (find during "-icanon" = None);
  is_true ~msg:"not -isig" (find during "-isig" = None);
  ignore (await ~from:child s "BACK 1" : int);
  send s "q";
  match wait_exit s with WEXITED 0 -> () | _ -> fail "expected a normal exit"

(* Cmd.suspend stops the job, as Ctrl+Z in a shell; continued in the
   foreground, the program draws again. *)
let test_suspend () =
  with_session ~prog:job "handoff" @@ fun s ->
  ignore (await s "BACK 0" : int);
  let from = mark s in
  send s "z";
  ignore (await ~from s "JOB-STOPPED TSTP" : int);
  is_true ~msg:"cooked while stopped" (cooked s);
  let from = mark s in
  send s "f\n";
  ignore (await ~from s "\027[?1049h" : int);
  ignore (await ~from s "BACK 0" : int);
  send s "q";
  ignore (await s "JOB-EXIT 0" : int)

(* A program whose terminal goes away (the emulator closed, the session
   hung up) ends: its reads fail, or its input ends, and it does not spin. *)
let test_terminal_gone name () =
  let s = spawn name in
  ignore (await s "N=0" : int);
  pause s 0.2;
  (* The emulator closes its side: the kernel hangs the terminal up. *)
  Unix.close (Pty.file_descr s.pty);
  let deadline = Unix.gettimeofday () +. 60. in
  let rec reap () =
    match Unix.waitpid [ Unix.WNOHANG ] s.pid with
    | 0, _ when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.01;
        reap ()
    | 0, _ ->
        Unix.kill s.pid Sys.sigkill;
        ignore (Unix.waitpid [] s.pid : int * Unix.process_status);
        fail "the program outlived its terminal"
    | _ -> ()
  in
  reap ()

let supported =
  Sys.os_type = "Unix" && Sys.file_exists program && Sys.file_exists job

(* The idle tests count a process's context switches in /proc, which only
   Linux has. *)
let counts_switches f () =
  if Sys.file_exists "/proc/self/status" then f ()
  else skip ~reason:"counts context switches in /proc, which only Linux has" ()

let measure () =
  let ms x = x *. 1000. in
  List.iter
    (fun name ->
      Printf.printf "idle %-20s %.2f wakeups/s\n%!" name
        (idle_wakeups ~seconds:10. name))
    [ "idle"; "idle-after-perform"; "idle-every"; "idle-tick"; "idle-matrix" ];
  let offsets = offsets (timer_fires ()) in
  Printf.printf "timer: %d fires, %.2f to %.2f ms from the grid\n%!"
    (List.length offsets)
    (ms (List.fold_left min infinity offsets))
    (ms (List.fold_left max neg_infinity offsets));
  List.iter
    (fun name ->
      let l = key_to_paint name in
      Printf.printf "%s: median %.2f ms, p90 %.2f ms, max %.2f ms\n%!" name
        (ms (percentile 0.5 l))
        (ms (percentile 0.9 l))
        (ms (List.fold_left max 0. l)))
    [ "latency"; "latency-busy" ];
  with_session "storm" (fun s ->
      ignore (wait_exit ~timeout:60. s : Unix.process_status);
      Printf.printf "storm: %s received, %s out of order, %s s, %s writes\n%!"
        (result s "received") (result s "disorder") (result s "seconds")
        (result s "writes"));
  List.iter
    (fun name ->
      let f = flood name in
      Printf.printf
        "%s: %d keys painted after p50 %.1f ms, p99 %.1f ms; %.0f views/s, \
         longest output gap %.1f ms, peak RSS %d MB\n\
         %!"
        name
        (List.length f.key_to_paint)
        (ms (percentile 0.5 f.key_to_paint))
        (ms (percentile 0.99 f.key_to_paint))
        (Float.of_int f.views /. f.seconds)
        (ms f.longest_gap) (f.peak_kb / 1024))
    [ "flood"; "flood-domain" ]

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "measure" then measure ()
  else
    run "mosaic.runtime.pty"
      [
        group "Idle"
          (if not supported then []
           else
             [
               test "an idle program does not wake"
                 (counts_switches test_idle_costs_no_wakeups);
               test "an idle Matrix run does not wake"
                 (counts_switches test_idle_matrix);
               test "a program that ran a perform does not wake"
                 (counts_switches test_idle_after_perform);
               test "a 1 s timer wakes once a second"
                 (counts_switches test_idle_every);
               test "a tick wakes once a frame" (counts_switches test_idle_tick);
             ]);
        group "Ticks"
          (if not supported then []
           else [ test "keys typed add no tick" test_ticks_while_typing ]);
        group "Time"
          (if not supported then []
           else [ test "R-T: a timer keeps its grid" test_timer_grid ]);
        group "Latency"
          (if not supported then []
           else
             [
               test "R-L: every key is painted" (test_key_to_paint "latency");
               test "R-L: with a busy worker" (test_key_to_paint "latency-busy");
             ]);
        group "Threads"
          (if not supported then []
           else
             [
               test "R-S: a storm of dispatches" test_storm;
               test "R-F: a flood from a systhread" (test_flood "flood");
               test "R-F: a flood from a domain" (test_flood "flood-domain");
               test "R-X: an exception in a perform" test_perform_exception;
               test "R-X2: an exception in init" test_init_exception;
             ]);
        group "Terminal gone"
          (if not supported then []
           else
             [
               test "a program ends with its terminal"
                 (test_terminal_gone "idle");
               test "one that ignores SIGHUP too"
                 (test_terminal_gone "idle-nohup");
             ]);
        group "Handoff"
          (if not supported then []
           else
             [
               test "Cmd.exec gives a child the terminal" test_exec_handoff;
               test "Cmd.suspend stops the job" test_suspend;
             ]);
      ]
