open Windtrap

let with_pty ?winsize f =
  let master, slave = Pty.open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      Pty.close master;
      Pty.close slave)
    (fun () -> f master (Pty.file_descr slave))

let with_pipe f =
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ r; w ])
    (fun () -> f r w)

(* The device *)

let test_is_tty () =
  with_pty (fun _ fd -> is_true ~msg:"a pseudo-terminal" (Tty.is_tty fd));
  with_pipe (fun r _ -> is_false ~msg:"a pipe" (Tty.is_tty r))

external set_controlling : Unix.file_descr -> unit = "test_tty_set_controlling"

(* A pseudo-terminal the process opened is not the one its job runs in, on
   either side. A process whose session the slave controls has the slave as
   its controlling terminal, and the master still is not. *)
let test_is_controlling () =
  with_pty (fun master fd ->
      is_false ~msg:"an opened pseudo-terminal" (Tty.is_controlling fd);
      is_false ~msg:"its master" (Tty.is_controlling (Pty.file_descr master)));
  with_pipe (fun r _ -> is_false ~msg:"a pipe" (Tty.is_controlling r));
  with_pty @@ fun master slave ->
  match Unix.fork () with
  | 0 ->
      let code =
        try
          ignore (Unix.setsid () : int);
          set_controlling slave;
          (if Tty.is_controlling slave then 0 else 1)
          + if Tty.is_controlling (Pty.file_descr master) then 2 else 0
        with _ -> 4
      in
      Unix._exit code
  | pid -> (
      match Unix.waitpid [] pid with
      | _, WEXITED 0 -> ()
      | _, WEXITED 1 -> fail "the slave is the child's controlling terminal"
      | _, WEXITED 2 -> fail "the master is not a controlling terminal"
      | _, WEXITED 3 -> fail "both answers are wrong"
      | _ -> fail "the child could not take the slave")

external new_group : unit -> unit = "test_tty_new_group"

(* A job in the foreground group of its controlling terminal is not in the
   background; one in another group of the session is. A terminal that
   controls no session of the process, and a pipe, are never the
   background's. *)
let test_background () =
  with_pty (fun _ fd ->
      is_false ~msg:"an opened pseudo-terminal" (Tty.background fd));
  with_pipe (fun r _ -> is_false ~msg:"a pipe" (Tty.background r));
  with_pty @@ fun _ slave ->
  match Unix.fork () with
  | 0 ->
      let code =
        try
          ignore (Unix.setsid () : int);
          set_controlling slave;
          if Tty.background slave then 1
          else
            match Unix.fork () with
            | 0 ->
                new_group ();
                Unix._exit (if Tty.background slave then 0 else 2)
            | pid -> (
                match Unix.waitpid [] pid with _, WEXITED c -> c | _ -> 4)
        with _ -> 4
      in
      Unix._exit code
  | pid -> (
      match Unix.waitpid [] pid with
      | _, WEXITED 0 -> ()
      | _, WEXITED 1 -> fail "the session's leader is in the foreground"
      | _, WEXITED 2 -> fail "a group of its own is in the background"
      | _ -> fail "the child could not take the slave")

let size = Testable.(option (pair int int))

(* The window as the system has it, and no guess where it has none. *)
let test_window () =
  with_pty ~winsize:{ rows = 30; cols = 100; xpixel = 0; ypixel = 0 }
    (fun master fd ->
      equal ~msg:"the size set" size (Some (100, 30)) (Tty.window fd);
      Pty.resize master ~rows:12 ~cols:40;
      equal ~msg:"a resize" size (Some (40, 12)) (Tty.window fd));
  with_pty ~winsize:{ rows = 0; cols = 0; xpixel = 0; ypixel = 0 } (fun _ fd ->
      equal ~msg:"a size never set" size None (Tty.window fd));
  with_pipe (fun r _ -> equal ~msg:"a pipe" size None (Tty.window r))

let test_readable () =
  with_pipe (fun r w ->
      is_false ~msg:"an empty pipe" (Tty.readable r);
      ignore (Unix.write_substring w "x" 0 1 : int);
      is_true ~msg:"a byte" (Tty.readable r);
      is_true ~msg:"asked again, still there" (Tty.readable r);
      ignore (Unix.read r (Bytes.create 1) 0 1 : int);
      is_false ~msg:"read" (Tty.readable r));
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.close w;
  Fun.protect
    ~finally:(fun () -> Unix.close r)
    (fun () -> is_true ~msg:"the end of the input" (Tty.readable r))

let test_now () =
  let a = Tty.now () in
  Unix.sleepf 0.01;
  let b = Tty.now () in
  is_true ~msg:"10 ms later, by at least 10 ms" (b - a >= 10_000_000)

(* Settings *)

(* [Unix.terminal_io] cannot express IEXTEN: the stub reads it. *)
external get_iexten : Unix.file_descr -> bool = "matrix_tty_get_iexten"

let test_raw_and_restore () =
  with_pty (fun _ fd ->
      (* A fresh line discipline has IEXTEN set: the kernel then takes
         VDISCARD (^O) and VLNEXT (^V) even with ICANON off. *)
      is_true ~msg:"IEXTEN set at first" (get_iexten fd);
      let saved = Tty.raw fd in
      let t = Unix.tcgetattr fd in
      is_false ~msg:"no echo" t.c_echo;
      is_false ~msg:"no line editing" t.c_icanon;
      is_false ~msg:"no signal characters" t.c_isig;
      is_false ~msg:"no IEXTEN" (get_iexten fd);
      Tty.restore fd saved;
      let t = Unix.tcgetattr fd in
      is_true ~msg:"echo again" t.c_echo;
      is_true ~msg:"line editing again" t.c_icanon;
      is_true ~msg:"IEXTEN again" (get_iexten fd))

(* The key interval keeps the signal characters, and returns each key as
   typed, a carriage return as one. *)
let test_keys () =
  with_pty (fun master fd ->
      let saved = Tty.keys fd in
      let t = Unix.tcgetattr fd in
      is_false ~msg:"no echo" t.c_echo;
      is_false ~msg:"no line editing" t.c_icanon;
      is_true ~msg:"signal characters" t.c_isig;
      is_true ~msg:"IEXTEN kept" (get_iexten fd);
      is_true ~msg:"output processed" t.c_opost;
      ignore (Pty.write_string master "a\r" 0 2 : int);
      let buf = Bytes.create 8 in
      let n = Unix.read fd buf 0 8 in
      equal ~msg:"each byte as typed" string "a\r" (Bytes.sub_string buf 0 n);
      Tty.restore fd saved;
      let t = Unix.tcgetattr fd in
      is_true ~msg:"echo again" t.c_echo;
      is_true ~msg:"line editing again" t.c_icanon;
      is_true ~msg:"CR to LF again" t.c_icrnl)

(* getpass's settings: no echo, lines and signals kept, and what was typed
   before discarded. *)
let test_no_echo () =
  with_pty (fun master fd ->
      ignore (Pty.write_string master "typed before\r" 0 13 : int);
      Unix.sleepf 0.05;
      let saved = Tty.no_echo fd in
      let t = Unix.tcgetattr fd in
      is_false ~msg:"no echo" t.c_echo;
      is_false ~msg:"no echo of a line end" t.c_echonl;
      is_true ~msg:"lines" t.c_icanon;
      is_true ~msg:"signal characters" t.c_isig;
      is_false ~msg:"typeahead discarded" (Tty.readable fd);
      ignore (Pty.write_string master "secret\r" 0 7 : int);
      let buf = Bytes.create 32 in
      let n = Unix.read fd buf 0 32 in
      equal ~msg:"a line" string "secret\n" (Bytes.sub_string buf 0 n);
      Tty.restore fd saved;
      is_true ~msg:"echo again" (Unix.tcgetattr fd).c_echo)

let test_raw_not_a_terminal () =
  with_pipe (fun r _ ->
      match Tty.raw r with
      | _ -> fail "a pipe has no settings"
      | exception Unix.Unix_error _ -> ())

(* Elsewhere than on Windows a terminal has no console mode: enable_vt
   changes nothing, and restoring what it returns changes nothing either. *)
let test_enable_vt_elsewhere () =
  if not Sys.win32 then
    with_pty (fun _ fd ->
        let before = Unix.tcgetattr fd in
        let console = Tty.enable_vt fd in
        Tty.restore_console fd console;
        is_true ~msg:"the settings as they were" (Unix.tcgetattr fd = before))

(* Terminal descriptions *)

let fixture name =
  In_channel.with_open_bin ("data/terminfo/" ^ name) In_channel.input_all

(* The process's environment for [f], each variable set or emptied (an
   empty variable is unset), then put back. *)
let with_env vars f =
  let saved = List.map (fun (v, _) -> (v, Sys.getenv_opt v)) vars in
  List.iter (fun (v, x) -> Unix.putenv v (Option.value x ~default:"")) vars;
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (v, x) -> Unix.putenv v (Option.value x ~default:"")) saved)
    f

let rec remove path =
  if Sys.is_directory path then (
    Array.iter (fun e -> remove (Filename.concat path e)) (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

(* A database in a fresh directory, with [entries] as (leaf directory,
   name, bytes). *)
let with_database entries f =
  let dir = Filename.temp_dir "matrix-terminfo" "" in
  List.iter
    (fun (leaf, name, bytes) ->
      let d = Filename.concat dir leaf in
      if not (Sys.file_exists d) then Sys.mkdir d 0o755;
      Out_channel.with_open_bin (Filename.concat d name) (fun oc ->
          Out_channel.output_string oc bytes))
    entries;
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let found what = function
  | Ok t -> List.hd (Matrix_terminfo.names t)
  | Error `Not_found -> failf "%s: not found" what
  | Error (`Invalid e) -> failf "%s: invalid: %s" what e

let not_found what = function
  | Error `Not_found -> ()
  | Ok _ -> failf "%s: found" what
  | Error (`Invalid e) -> failf "%s: invalid: %s" what e

let name = "matrix-test-terminal"

(* TERMINFO, then ~/.terminfo, then TERMINFO_DIRS, then the system. *)
let test_terminfo_order () =
  with_database [ ("m", name, fixture "d/dumb") ] @@ fun terminfo ->
  with_database [] @@ fun home ->
  with_database [ ("m", name, fixture "x/xterm") ] @@ fun dirs ->
  let dot = Filename.concat home ".terminfo" in
  Sys.mkdir dot 0o755;
  Sys.mkdir (Filename.concat dot "m") 0o755;
  Out_channel.with_open_bin
    (Filename.concat (Filename.concat dot "m") name)
    (fun oc -> Out_channel.output_string oc (fixture "l/linux"));
  let look vars = with_env vars (fun () -> Tty.terminfo name) in
  let dirs = ("TERMINFO_DIRS", Some ("/nonexistent::" ^ dirs)) in
  equal ~msg:"TERMINFO first" string "dumb"
    (found "all"
       (look [ ("TERMINFO", Some terminfo); ("HOME", Some home); dirs ]));
  equal ~msg:"then ~/.terminfo" string "linux"
    (found "home" (look [ ("TERMINFO", None); ("HOME", Some home); dirs ]));
  equal ~msg:"then TERMINFO_DIRS" string "xterm"
    (found "dirs" (look [ ("TERMINFO", None); ("HOME", None); dirs ]));
  not_found "none of them"
    (look [ ("TERMINFO", None); ("HOME", None); ("TERMINFO_DIRS", None) ])

(* A database on a filesystem that ignores case names its directories by
   the first character's code. *)
let test_terminfo_hex_directory () =
  with_database [ ("6d", name, fixture "d/dumb") ] @@ fun dir ->
  with_env [ ("TERMINFO", Some dir) ] @@ fun () ->
  equal ~msg:"6d for m" string "dumb" (found "hex" (Tty.terminfo name))

let hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02X" (Char.code c))
       (List.of_seq (String.to_seq s)))

(* As tic -Q 2 writes it: [-_] for [+/], no padding. *)
let b64 s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let b = Buffer.create 64 and n = String.length s in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    let chars = if n - !i >= 3 then 4 else n - !i + 1 in
    for k = 0 to chars - 1 do
      Buffer.add_char b alphabet.[(v lsr (18 - (6 * k))) land 63]
    done;
    i := !i + 3
  done;
  Buffer.contents b

(* TERMINFO may hold the description itself, used for a name it has. *)
let test_terminfo_encoded () =
  let dumb = fixture "d/dumb" in
  List.iter
    (fun (form, value) ->
      with_env
        [ ("TERMINFO", Some value); ("HOME", None); ("TERMINFO_DIRS", None) ]
      @@ fun () ->
      equal ~msg:form string "dumb" (found form (Tty.terminfo "dumb"));
      equal
        ~msg:(form ^ ", by another of its names")
        string "dumb"
        (found form (Tty.terminfo "80-column dumb tty"));
      not_found (form ^ " for another name") (Tty.terminfo name))
    [ ("hex", "hex:" ^ hex dumb); ("b64", "b64:" ^ b64 dumb) ]

(* The first description that reads; a broken one is skipped. *)
let test_terminfo_invalid () =
  with_database [ ("m", name, "not an entry") ] @@ fun broken ->
  with_database [ ("m", name, fixture "d/dumb") ] @@ fun good ->
  with_env
    [ ("TERMINFO", Some broken); ("HOME", None); ("TERMINFO_DIRS", Some good) ]
    (fun () ->
      equal ~msg:"the next one" string "dumb" (found "skip" (Tty.terminfo name)));
  with_env
    [ ("TERMINFO", Some broken); ("HOME", None); ("TERMINFO_DIRS", None) ]
    (fun () ->
      match Tty.terminfo name with
      | Error (`Invalid _) -> ()
      | _ -> fail "only a broken description: invalid")

let test_terminfo_names () =
  List.iter
    (fun n -> not_found n (Tty.terminfo n))
    [ ""; "."; ".."; "../x/xterm"; "x/xterm" ]

(* Signals *)

module Signal = Tty.Signal

(* Linux's view of what the process catches, from /proc/self/status:
   [Sys.signal] reads a handler installed in C as the default, and putting
   that back would remove it. *)
let linux = Sys.file_exists "/proc/self/status"

let caught signo =
  let line =
    In_channel.with_open_text "/proc/self/status" In_channel.input_lines
    |> List.find (String.starts_with ~prefix:"SigCgt:")
  in
  let mask =
    Int64.of_string
      ("0x" ^ String.trim (String.sub line 7 (String.length line - 7)))
  in
  Int64.logand mask (Int64.shift_left 1L (signo - 1)) <> 0L

let sighup = 1

let with_handler s f =
  let called = ref 0 in
  let previous = Sys.signal s (Sys.Signal_handle (fun _ -> incr called)) in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.signal s previous : Sys.signal_behavior))
    (fun () -> f called)

let with_holder ?output ~controlling f =
  let h = Signal.hold ~controlling ~output in
  Fun.protect ~finally:(fun () -> Signal.release h) (fun () -> f h)

let readable_now fd =
  match Unix.select [ fd ] [] [] 0. with [], _, _ -> false | _ -> true

let readable fd =
  match Unix.select [ fd ] [] [] 1. with [], _, _ -> false | _ -> true

let drain fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | 0 -> ()
    | _ -> go ()
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
  in
  go ()

(* Which signals a holder takes: the window and job control only on the
   controlling terminal. *)
let test_signals () =
  let names = List.map string_of_int in
  let ending =
    [ Sys.sigabrt; Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]
  in
  equal ~msg:"another terminal" (list string) (names ending)
    (names (Signal.signals ~controlling:false));
  equal ~msg:"the controlling terminal" (list string)
    (names ([ Sys.sigwinch; Sys.sigcont; Sys.sigtstp ] @ ending))
    (names (Signal.signals ~controlling:true))

(* A signal that ends the process is taken only at its default action, and
   given back to it. *)
let test_take_at_default () =
  if linux && not (caught sighup) then (
    is_true ~msg:"taken at its default" (Signal.take Sys.sighup);
    is_true ~msg:"caught" (caught sighup);
    is_true ~msg:"a second holder takes it too" (Signal.take Sys.sighup);
    Signal.give_back Sys.sighup;
    is_true ~msg:"still caught for the other" (caught sighup);
    Signal.give_back Sys.sighup;
    is_false ~msg:"the default again" (caught sighup);
    with_handler Sys.sighup (fun _ ->
        is_false ~msg:"the program's own action is kept"
          (Signal.take Sys.sighup)))

(* SIGWINCH is taken whatever its action, and the handler installed before
   is still called. *)
let test_take_always () =
  if Sys.unix then
    with_handler Sys.sigwinch (fun called ->
        with_holder ~controlling:true (fun h ->
            is_true ~msg:"taken over a handler" (Signal.take Sys.sigwinch);
            Unix.kill (Unix.getpid ()) Sys.sigwinch;
            is_true ~msg:"the holder is woken" (readable (Signal.wake_fd h));
            drain (Signal.wake_fd h);
            is_true ~msg:"received" (Signal.received h Sys.sigwinch);
            is_false ~msg:"received once" (Signal.received h Sys.sigwinch);
            Signal.give_back Sys.sigwinch;
            (* The OCaml handler runs at the next safe point. *)
            Unix.sleepf 0.01;
            equal ~msg:"the handler before was called" int 1 !called))

(* A signal that lands while [received] takes the one before it is kept.
   One domain takes the signal as fast as it can while the other raises it,
   each time once the one before has been taken; a signal lost between the
   read of the record and its clearing would never be taken. *)
let test_received_keeps_a_signal_that_lands () =
  if Sys.unix then (
    with_holder ~controlling:true @@ fun h ->
    is_true ~msg:"taken" (Signal.take Sys.sigwinch);
    Fun.protect ~finally:(fun () -> Signal.give_back Sys.sigwinch) @@ fun () ->
    let taken = Atomic.make 0 and stop = Atomic.make false in
    let taker =
      Domain.spawn (fun () ->
          while not (Atomic.get stop) do
            if Signal.received h Sys.sigwinch then Atomic.incr taken
          done)
    in
    let lost = ref None in
    Fun.protect ~finally:(fun () ->
        Atomic.set stop true;
        Domain.join taker)
    @@ fun () ->
    let i = ref 1 in
    while !lost = None && !i <= 20_000 do
      Unix.kill (Unix.getpid ()) Sys.sigwinch;
      let deadline = Unix.gettimeofday () +. 5. in
      while Atomic.get taken < !i && Unix.gettimeofday () < deadline do
        Domain.cpu_relax ()
      done;
      if Atomic.get taken < !i then lost := Some !i;
      incr i
    done;
    match !lost with Some i -> failf "signal %d was lost" i | None -> ())

let test_take_outside_the_policy () =
  if Sys.unix then
    match Signal.take Sys.sigusr1 with
    | _ -> fail "SIGUSR1 is no terminal's signal"
    | exception Invalid_argument _ -> ()

(* The window signal concerns the controlling terminal's holders alone; a
   signal that ends the process concerns every holder, and says so. *)
let test_holders_concerned () =
  if linux && not (caught sighup) then
    with_holder ~controlling:false (fun served ->
        with_holder ~controlling:true (fun own ->
            ignore (Signal.take Sys.sigwinch : bool);
            ignore (Signal.take Sys.sighup : bool);
            Fun.protect
              ~finally:(fun () ->
                Signal.give_back Sys.sigwinch;
                Signal.give_back Sys.sighup)
              (fun () ->
                Unix.kill (Unix.getpid ()) Sys.sigwinch;
                is_true ~msg:"woken" (readable (Signal.wake_fd own));
                is_true ~msg:"the controlling terminal's"
                  (Signal.received own Sys.sigwinch);
                is_false ~msg:"not a served one's"
                  (Signal.received served Sys.sigwinch);
                Unix.kill (Unix.getpid ()) Sys.sighup;
                is_true ~msg:"woken" (readable (Signal.wake_fd served));
                is_true ~msg:"terminating" (Signal.terminating served);
                is_true ~msg:"received by every holder"
                  (Signal.received served Sys.sighup
                  && Signal.received own Sys.sighup);
                is_false ~msg:"taken" (Signal.terminating served))))

(* A holder used after its release is known for what it is, though the next
   holder has its slot: its calls raise, and its wake wakes nobody. *)
let test_released_holder () =
  if Sys.unix then (
    let old = Signal.hold ~controlling:true ~output:None in
    Signal.release old;
    with_holder ~controlling:true @@ fun next ->
    is_true ~msg:"taken" (Signal.take Sys.sigwinch);
    Fun.protect ~finally:(fun () -> Signal.give_back Sys.sigwinch) @@ fun () ->
    Unix.kill (Unix.getpid ()) Sys.sigwinch;
    is_true ~msg:"the next holder is woken" (readable (Signal.wake_fd next));
    drain (Signal.wake_fd next);
    let released what f =
      match f () with
      | _ -> failf "%s: a released holder is used" what
      | exception Invalid_argument _ -> ()
    in
    released "received" (fun () -> Signal.received old Sys.sigwinch);
    released "terminating" (fun () -> Signal.terminating old);
    released "wake_fd" (fun () -> Signal.wake_fd old);
    released "set_reset" (fun () -> Signal.set_reset old "x");
    released "forget_settings" (fun () -> Signal.forget_settings old);
    released "release" (fun () -> Signal.release old);
    Signal.wake old;
    is_false ~msg:"its wake wakes nobody" (readable_now (Signal.wake_fd next));
    is_true ~msg:"the next holder's signal is its own"
      (Signal.received next Sys.sigwinch))

let test_wake () =
  if Sys.unix then
    with_holder ~controlling:false (fun h ->
        is_false ~msg:"quiet" (readable_now (Signal.wake_fd h));
        Signal.wake h;
        is_true ~msg:"woken" (readable (Signal.wake_fd h)))

(* A signal that ends the process cuts a write the terminal is not taking
   for good: the write returns, the rest unwritten. *)
let test_write_cut_by_termination () =
  if linux && not (caught sighup) then
    with_holder ~controlling:false (fun h ->
        with_pipe (fun r w ->
            ignore (Signal.take Sys.sighup : bool);
            let mask = Thread.sigmask SIG_BLOCK [ Sys.sighup ] in
            Fun.protect
              ~finally:(fun () ->
                ignore (Thread.sigmask SIG_SETMASK mask : int list);
                Signal.give_back Sys.sighup)
              (fun () ->
                let n = 200_000 in
                let writer =
                  Thread.create
                    (fun () ->
                      ignore
                        (Thread.sigmask SIG_UNBLOCK [ Sys.sighup ] : int list);
                      Signal.write h w (Bytes.make n 'x') 0 n)
                    ()
                in
                Thread.delay 0.2;
                Unix.kill (Unix.getpid ()) Sys.sighup;
                Thread.join writer;
                Unix.set_nonblock r;
                let got = ref 0 and buf = Bytes.create 65536 in
                (try
                   while true do
                     got := !got + Unix.read r buf 0 65536
                   done
                 with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ());
                equal ~msg:"what the pipe took, and no more" int 65536 !got)))

(* What an abort writes and puts back, for a holder whose process ends where
   no OCaml code runs. *)
let test_restore_all () =
  if Sys.unix then
    with_pty (fun _ input ->
        with_pipe (fun r output ->
            with_holder ~output ~controlling:false (fun h ->
                Signal.keep_settings h ~input;
                let _ = Tty.raw input in
                Signal.set_reset h "\027[?25h";
                Signal.restore_all ();
                let buf = Bytes.create 64 in
                let n = Unix.read r buf 0 64 in
                equal ~msg:"the reset bytes" string "\027[?25h"
                  (Bytes.sub_string buf 0 n);
                is_true ~msg:"cooked again" (Unix.tcgetattr input).c_icanon;
                Signal.set_reset h "";
                Signal.forget_settings h)))

(* Reset bytes have no length but the terminal's: a holder's teardown can
   be as long as what it set. *)
let test_restore_all_long () =
  if Sys.unix then
    with_pty (fun _ input ->
        with_pipe (fun r output ->
            with_holder ~output ~controlling:false (fun h ->
                Signal.keep_settings h ~input;
                let reset =
                  String.init 20_000 (fun i -> Char.chr (65 + (i mod 26)))
                in
                Signal.set_reset h reset;
                Signal.set_reset h (reset ^ reset);
                Signal.restore_all ();
                let got = Buffer.create 40_000 and buf = Bytes.create 65536 in
                while Buffer.length got < 40_000 do
                  let n = Unix.read r buf 0 65536 in
                  Buffer.add_subbytes got buf 0 n
                done;
                equal ~msg:"the last reset bytes, whole" string (reset ^ reset)
                  (Buffer.contents got);
                Signal.set_reset h "")))

let () =
  run "matrix.tty"
    [
      group "device"
        [
          test "is_tty" test_is_tty;
          test "is_controlling" test_is_controlling;
          test "window" test_window;
          test "background" test_background;
          test "readable" test_readable;
          test "now" test_now;
        ];
      group "settings"
        [
          test "raw and restore" test_raw_and_restore;
          test "raw on a pipe" test_raw_not_a_terminal;
          test "the key interval" test_keys;
          test "no echo" test_no_echo;
          test "enable_vt elsewhere than on Windows" test_enable_vt_elsewhere;
        ];
      group "descriptions"
        [
          test "the order of the databases" test_terminfo_order;
          test "hexadecimal directories" test_terminfo_hex_directory;
          test "a description in TERMINFO" test_terminfo_encoded;
          test "a broken description" test_terminfo_invalid;
          test "names that are no file" test_terminfo_names;
        ];
      group "signals"
        [
          test "which a holder takes" test_signals;
          test "taken at the default" test_take_at_default;
          test "taken over a handler" test_take_always;
          test "outside the policy" test_take_outside_the_policy;
          test "a signal that lands while one is taken"
            test_received_keeps_a_signal_that_lands;
          test "the holders a signal concerns" test_holders_concerned;
          test "wake" test_wake;
          test "a released holder" test_released_holder;
          test "a write cut by termination" test_write_cut_by_termination;
          test "restore_all" test_restore_all;
          test "restore_all with long reset bytes" test_restore_all_long;
        ];
    ]
