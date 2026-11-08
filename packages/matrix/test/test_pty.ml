open Windtrap

(* Test Helpers *)

let read_with_select pty buffer offset length timeout =
  let start_time = Unix.gettimeofday () in
  let total_read = ref 0 in
  let continue = ref true in
  while !continue && Unix.gettimeofday () -. start_time < timeout do
    let read_fds = [ Pty.in_fd pty ] in
    let ready_read, _, _ = Unix.select read_fds [] [] 0.1 in
    if ready_read <> [] then
      try
        let n =
          Pty.read pty buffer (offset + !total_read) (length - !total_read)
        in
        if n > 0 then total_read := !total_read + n
        else if n = 0 then continue := false (* EOF *)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
          ()
      | Unix.Unix_error (Unix.EIO, _, _) ->
          (* EIO on PTY master means child process exited - treat as EOF *)
          continue := false
  done;
  !total_read

let write_all_with_select pty data timeout =
  Pty.set_nonblock pty;
  let written = ref 0 in
  let start_time = Unix.gettimeofday () in
  while
    !written < String.length data
    && Unix.gettimeofday () -. start_time < timeout
  do
    let write_fds = [ Pty.out_fd pty ] in
    let _, ready_write, _ = Unix.select [] write_fds [] 0.1 in
    if ready_write <> [] then
      try
        let n =
          Pty.write_string pty data !written (String.length data - !written)
        in
        if n > 0 then written := !written + n
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _)
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
      ->
        ()
  done;
  !written

(* Blocking reads in these tests wait at most [timeout] seconds, so that a
   regression which hides the end of output fails instead of hanging. *)
let readable ~timeout fd =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    let left = deadline -. Unix.gettimeofday () in
    left > 0.
    &&
    match Unix.select [ fd ] [] [] left with
    | [], _, _ -> wait ()
    | _ -> true
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  wait ()

(* Spawns a child that writes "hello\r\n" and returns once it has exited,
   without reaping it. The child inherits no descriptor to signal its exit
   with, so its last act is to remove a file: [exec rm], which exits right
   after. *)
let spawn_hello_and_await_exit () =
  let marker = Filename.temp_file "matrix_pty" ".exit" in
  let pty =
    Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo hello; exec rm \"$0\""; marker ] ()
  in
  let deadline = Unix.gettimeofday () +. 2. in
  while Sys.file_exists marker && Unix.gettimeofday () < deadline do
    Unix.sleepf 0.001
  done;
  if Sys.file_exists marker then (
    Sys.remove marker;
    fail "the child did not exit within 2s");
  pty

let read_to_eof pty =
  let buf = Bytes.create 256 and out = Buffer.create 64 in
  let rec loop () =
    if not (readable ~timeout:2. (Pty.in_fd pty)) then
      failf "no end of output within 2s, after %S" (Buffer.contents out);
    match Pty.read pty buf 0 (Bytes.length buf) with
    | 0 | (exception Unix.Unix_error (Unix.EIO, _, _)) -> Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n;
        loop ()
  in
  loop ()

let pty_test_supported =
  if Sys.os_type = "Win32" then false
  else
    try
      let master, slave = Pty.open_pty () in
      Pty.close master;
      Pty.close slave;
      true
    with
    | Unix.Unix_error (Unix.ENOENT, "open_pty", _)
    | Unix.Unix_error (Unix.ENOSYS, "open_pty", _)
    | Unix.Unix_error (Unix.EPERM, "open_pty", _)
    | Unix.Unix_error (Unix.EACCES, "open_pty", _)
    ->
      false

let test_pty_unavailable () =
  is_true
    ~msg:
      "PTY backend unavailable on this platform/environment; PTY integration \
       tests skipped"
    true

(* Basic PTY Operations *)

let test_open_pty () =
  let master, slave = Pty.open_pty () in
  is_true ~msg:"master fd valid" Unix.(Pty.file_descr master <> stdin);
  is_true ~msg:"slave fd valid" Unix.(Pty.file_descr slave <> stdin);
  is_true ~msg:"fds different" (Pty.file_descr master <> Pty.file_descr slave);
  Pty.close master;
  Pty.close slave

let test_open_pty_with_winsize () =
  let ws = Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 } in
  let master, slave = Pty.open_pty ~winsize:ws () in
  let actual_ws = Pty.get_winsize slave in
  equal ~msg:"rows set" int 24 actual_ws.rows;
  equal ~msg:"cols set" int 80 actual_ws.cols;
  Pty.close master;
  Pty.close slave

let test_with_pty () =
  let result =
    Pty.with_pty (fun master _slave ->
        is_true ~msg:"master valid" Unix.(Pty.file_descr master <> stdin);
        42)
  in
  equal ~msg:"result returned" int 42 result

(* Window Size Management *)

let test_winsize_operations () =
  let master, slave = Pty.open_pty () in
  let ws = Pty.{ rows = 30; cols = 120; xpixel = 0; ypixel = 0 } in
  Pty.set_winsize slave ws;
  let actual = Pty.get_winsize slave in
  equal ~msg:"rows match" int 30 actual.rows;
  equal ~msg:"cols match" int 120 actual.cols;
  Pty.close master;
  Pty.close slave

let test_resize () =
  let master, slave = Pty.open_pty () in
  Pty.resize slave ~rows:25 ~cols:100;
  let ws = Pty.get_winsize slave in
  equal ~msg:"rows resized" int 25 ws.rows;
  equal ~msg:"cols resized" int 100 ws.cols;
  Pty.close master;
  Pty.close slave

let test_inherit_size () =
  let master1, slave1 = Pty.open_pty () in
  let master2, slave2 = Pty.open_pty () in
  Pty.resize slave1 ~rows:40 ~cols:160;
  Pty.inherit_size ~src:slave1 ~dst:slave2;
  let ws2 = Pty.get_winsize slave2 in
  equal ~msg:"inherited rows" int 40 ws2.rows;
  equal ~msg:"inherited cols" int 160 ws2.cols;
  Pty.close master1;
  Pty.close slave1;
  Pty.close master2;
  Pty.close slave2

(* Process Spawning *)

let test_spawn_echo () =
  let pty = Pty.spawn ~prog:"echo" ~args:[ "Hello, PTY!" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  satisfies ~claim:"read some data (> 0 bytes)" int (fun bytes -> bytes > 0) n;
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"contains Hello" (String.contains output 'H');
  Pty.close pty

let test_spawn_cat () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;
  let test_data = "Hello, world!\n" in
  let written = write_all_with_select pty test_data 1.0 in
  equal ~msg:"wrote all data" int (String.length test_data) written;

  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  satisfies
    ~claim:
      (Printf.sprintf "read at least the %d bytes written"
         (String.length test_data))
    int
    (fun bytes -> bytes >= String.length test_data)
    n;
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"contains test data" (String.contains output 'H');
  Pty.close pty

let test_spawn_sh () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo 'Shell test'" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  satisfies ~claim:"read some data (> 0 bytes)" int (fun bytes -> bytes > 0) n;
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"contains Shell" (String.contains output 'S');
  Pty.close pty

let test_spawn_with_env () =
  let env = [| "TEST_VAR=hello" |] in
  let pty = Pty.spawn ~env ~prog:"sh" ~args:[ "-c"; "echo $TEST_VAR" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"env var in output" (String.contains output 'h');
  Pty.close pty

let test_spawn_with_cwd () =
  let pty = Pty.spawn ~cwd:"/tmp" ~prog:"pwd" ~args:[] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  let output = String.trim (Bytes.sub_string buffer 0 n) in
  (* /tmp may be a symlink to /private/tmp on macOS *)
  is_true ~msg:"working directory is tmp"
    (output = "/tmp" || output = "/private/tmp");
  Pty.close pty

(* Regression: the program led the terminal's session, and its exit lost the
   output the master had not read: macOS discards it about 0.6 s after the
   slave's last close, and FreeBSD at once when the session leader exits. The
   keeper now holds the terminal until the master has read it, so the sleep
   only matters without the fix. *)
let test_spawn_output_read_late () =
  let pty = spawn_hello_and_await_exit () in
  Unix.sleepf 1.;
  let output = read_to_eof pty in
  Pty.close pty;
  equal ~msg:"output read 1s after the child exited" string "hello\r\n" output

(* Regression: the slave was opened without close-on-exec, so a child that
   the program started while holding it, here [sleep], inherited it. The
   terminal then stayed open, and the master read no end of output until
   that child exited. *)
let test_slave_not_inherited () =
  let master, slave = Pty.open_pty () in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let other = Unix.create_process "sleep" [| "sleep"; "10" |] null null null in
  Unix.close null;
  Pty.close slave;
  let buf = Bytes.create 16 in
  let n = read_with_select master buf 0 16 2.0 in
  let ended =
    match Unix.select [ Pty.in_fd master ] [] [] 0. with
    | [], _, _ -> false
    | _ -> (
        match Pty.read master buf 0 16 with
        | 0 -> true
        | _ -> false
        | exception Unix.Unix_error (Unix.EIO, _, _) -> true)
  in
  Unix.kill other Sys.sigkill;
  ignore (Unix.waitpid [] other);
  Pty.close master;
  equal ~msg:"no output" int 0 n;
  is_true ~msg:"master reads the end of output once the slave is closed" ended

(* Regression: with the slave close-on-exec, a [dup2] of the slave onto the
   std fd it already is did nothing, so exec closed that fd in the child.
   The slave lands on a std fd when the program runs with them closed: here
   stdout and stderr are closed, so the master is fd 1 and the slave fd 2. *)
let test_slave_on_std_fd () =
  flush stdout;
  flush stderr;
  let saved_out = Unix.dup ~cloexec:true Unix.stdout in
  let saved_err = Unix.dup ~cloexec:true Unix.stderr in
  Unix.close Unix.stdout;
  Unix.close Unix.stderr;
  let pty = ref None in
  let output =
    Fun.protect
      ~finally:(fun () ->
        Option.iter Pty.close !pty;
        Unix.dup2 saved_out Unix.stdout;
        Unix.dup2 saved_err Unix.stderr;
        Unix.close saved_out;
        Unix.close saved_err)
      (fun () ->
        let p =
          Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo out; echo err >&2" ] ()
        in
        pty := Some p;
        let buf = Bytes.create 256 in
        let n = read_with_select p buf 0 256 2.0 in
        Bytes.sub_string buf 0 n)
  in
  equal ~msg:"the child writes to its stdout and stderr" string "out\r\nerr\r\n"
    output

(* The master reads the end of output once the program has exited, on every
   platform: the keeper exits with the program, and a keeper that held the
   slave on, as on Linux, where nothing hangs up a terminal whose session
   leader exits, would hide the end of output. *)
let test_spawn_output_then_eof () =
  let pty = Pty.spawn ~prog:"echo" ~args:[ "hello" ] () in
  let output = read_to_eof pty in
  Pty.close pty;
  equal ~msg:"output, then the end of output" string "hello\r\n" output

(* Regression: spawn forked with [Unix.fork], which OCaml 5 refuses while
   another domain runs, as when an application runs work on a helper
   domain. *)
let test_spawn_while_a_domain_runs () =
  let stop = Atomic.make false in
  let domain =
    Domain.spawn (fun () ->
        while not (Atomic.get stop) do
          Unix.sleepf 0.001
        done)
  in
  (* The child stays until the handle closes: FreeBSD may discard the output
     of a child that exited before it was read. *)
  let rec read_into pty buf off =
    if off = Bytes.length buf || not (readable ~timeout:2. (Pty.in_fd pty)) then
      off
    else
      match Pty.read pty buf off (Bytes.length buf - off) with
      | 0 | (exception Unix.Unix_error (Unix.EIO, _, _)) -> off
      | n -> read_into pty buf (off + n)
  in
  let output =
    Fun.protect
      ~finally:(fun () ->
        Atomic.set stop true;
        Domain.join domain)
      (fun () ->
        let pty =
          Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo hello; exec cat" ] ()
        in
        let buf = Bytes.create 7 in
        let n = read_into pty buf 0 in
        Pty.close pty;
        Bytes.sub_string buf 0 n)
  in
  equal ~msg:"the child ran" string "hello\r\n" output

(* A terminal emulator reads in one domain while another writes, resizes and
   signals the child. Here [cat] runs on a raw terminal, which passes bytes
   through unchanged, and the main domain must read back exactly the 8 MB
   the writer sends in 4,000 writes of varying sizes, then the end of output
   once the writer has terminated the child. *)
let test_read_while_another_domain_writes () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "stty raw -echo && echo ready && exec cat" ]
      ()
  in
  let buf = Bytes.create 4096 in
  let rec await_ready seen =
    if not (readable ~timeout:5. (Pty.in_fd pty)) then
      failf "no ready line within 5s, after %S" seen;
    let n = Pty.read pty buf 0 (Bytes.length buf) in
    let seen = seen ^ Bytes.sub_string buf 0 n in
    if not (String.ends_with ~suffix:"ready\n" seen) then await_ready seen
  in
  await_ready "";
  let chunks =
    List.init 4000 (fun i ->
        String.init
          (1 + (i * 37 mod 4096))
          (fun j -> Char.chr (Char.code 'a' + ((i + j) mod 26))))
  in
  let expected = String.concat "" chunks in
  let read_all = Atomic.make false in
  (* The writer ends the child even when it fails, so that the reader
     reaches the end of output and the failure is raised by the join. *)
  let writer =
    Domain.spawn (fun () ->
        Fun.protect
          ~finally:(fun () -> Pty.terminate pty)
          (fun () ->
            List.iteri
              (fun i chunk ->
                let n = Pty.write_string pty chunk 0 (String.length chunk) in
                assert (n = String.length chunk);
                if i mod 50 = 0 then
                  Pty.resize pty ~rows:(20 + (i mod 7)) ~cols:80)
              chunks;
            while not (Atomic.get read_all) do
              Domain.cpu_relax ()
            done))
  in
  let out = Buffer.create (String.length expected) in
  let rec read_until_eof () =
    if Buffer.length out >= String.length expected then Atomic.set read_all true;
    match Pty.read pty buf 0 (Bytes.length buf) with
    | 0 | (exception Unix.Unix_error (Unix.EIO, _, _)) -> ()
    | n ->
        Buffer.add_subbytes out buf 0 n;
        read_until_eof ()
  in
  read_until_eof ();
  Domain.join writer;
  Pty.close pty;
  equal ~msg:"read back what was written" int (String.length expected)
    (Buffer.length out);
  is_true ~msg:"in order" (String.equal expected (Buffer.contents out))

(* As with execvp, a file that the kernel cannot execute, here a script
   without #!, runs under /bin/sh. *)
let test_spawn_script_without_shebang () =
  let script = Filename.temp_file "matrix_pty" ".sh" in
  Out_channel.with_open_bin script (fun oc ->
      output_string oc "echo script \"$1\"\n");
  Unix.chmod script 0o755;
  let output =
    Fun.protect
      ~finally:(fun () -> Sys.remove script)
      (fun () ->
        let pty = Pty.spawn ~prog:script ~args:[ "ran" ] () in
        let output = read_to_eof pty in
        Pty.close pty;
        output)
  in
  equal ~msg:"the script ran with its argument" string "script ran\r\n" output

(* Regression: the child inherited the parent's ignored signals, and the
   program, here a shell, could not get their default action back: the
   shell ignored on entry stays ignored, so [kill] did not end it. *)
let test_spawn_resets_ignored_signals () =
  let previous = Sys.signal Sys.sigusr1 Sys.Signal_ignore in
  let output =
    Fun.protect
      ~finally:(fun () -> Sys.set_signal Sys.sigusr1 previous)
      (fun () ->
        let pty =
          Pty.spawn ~prog:"sh" ~args:[ "-c"; "kill -s USR1 $$; echo alive" ] ()
        in
        let output = read_to_eof pty in
        Pty.close pty;
        output)
  in
  equal ~msg:"SIGUSR1 ended the child" string "" output

(* Regression: the child inherited the parent's signal mask, so a signal
   the parent blocked stayed pending in the program. *)
let test_spawn_unblocks_signals () =
  let previous = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigusr1 ] in
  let output =
    Fun.protect
      ~finally:(fun () -> ignore (Unix.sigprocmask Unix.SIG_SETMASK previous))
      (fun () ->
        let pty =
          Pty.spawn ~prog:"sh" ~args:[ "-c"; "kill -s USR1 $$; echo alive" ] ()
        in
        let output = read_to_eof pty in
        Pty.close pty;
        output)
  in
  equal ~msg:"SIGUSR1 ended the child" string "" output

(* Regression: the child inherited every descriptor the parent opened
   without close-on-exec. Here [cat] held the write end of a pipe, so its
   reader read no end of file while [cat] ran. *)
let test_spawn_inherits_only_std_fds () =
  let r, w = Unix.pipe () in
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Unix.close w;
  let ended = readable ~timeout:2. r && Unix.read r (Bytes.create 1) 0 1 = 0 in
  Unix.close r;
  Pty.close pty;
  is_true ~msg:"the pipe's reader reads its end while the child runs" ended

(* Regression: a child that could not exec exited with status 127, so
   spawn returned a handle on a dead child and the caller saw no error. *)
let test_spawn_missing_program_raises () =
  List.iter
    (fun prog ->
      match Pty.spawn ~prog ~args:[] () with
      | pty ->
          Pty.close pty;
          failf "spawn returned for the missing program %s" prog
      | exception Unix.Unix_error (err, fn, arg) ->
          is_true ~msg:"ENOENT" (err = Unix.ENOENT);
          equal ~msg:"the step that failed" (pair string string)
            ("execvp", prog) (fn, arg))
    [ "/definitely/not/a/matrix-program"; "definitely-not-a-matrix-program" ]

let test_spawn_missing_cwd_raises () =
  let cwd = "/definitely/not/a/matrix-directory" in
  match Pty.spawn ~cwd ~prog:"echo" ~args:[] () with
  | pty ->
      Pty.close pty;
      fail "spawn returned for a missing directory"
  | exception Unix.Unix_error (err, fn, arg) ->
      is_true ~msg:"ENOENT" (err = Unix.ENOENT);
      equal ~msg:"the step that failed" (pair string string) ("chdir", cwd)
        (fn, arg)

(* A child that fails runs nothing of the parent's: ordinary OCaml
   termination in it would run the parent's [at_exit] handlers. *)
let test_failed_spawn_does_not_run_parent_at_exit () =
  let parent_pid = Unix.getpid () in
  let marker_read, marker_write = Unix.pipe () in
  let marker = Bytes.of_string "child-at-exit" in
  at_exit (fun () ->
      if Unix.getpid () <> parent_pid then
        try
          ignore (Unix.write marker_write marker 0 (Bytes.length marker) : int)
        with Unix.Unix_error _ -> ());
  (try
     Pty.close (Pty.spawn ~prog:"/definitely/not/a/matrix-program" ~args:[] ())
   with Unix.Unix_error _ -> ());
  Unix.close marker_write;
  let received = Bytes.create (Bytes.length marker) in
  let received_len = Unix.read marker_read received 0 (Bytes.length received) in
  Unix.close marker_read;
  equal ~msg:"failed child skips inherited parent at_exit handlers" int 0
    received_len

(* Non-blocking I/O *)

let test_nonblocking_io () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;

  (* Read when no data available - should return EAGAIN *)
  let buffer = Bytes.create 256 in
  let result =
    try
      let n = Pty.read pty buffer 0 256 in
      Some n
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
    ->
      None
  in
  equal ~msg:"read returns EAGAIN" (option int) None result;

  (* Write some data *)
  let test_data = "Test\n" in
  let _ = Pty.write_string pty test_data 0 (String.length test_data) in

  (* Now read should work after brief delay *)
  Unix.sleepf 0.1;
  let n =
    try Pty.read pty buffer 0 256
    with Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
  in
  satisfies ~claim:"read some data (> 0 bytes)" int (fun bytes -> bytes > 0) n;
  Pty.close pty

let test_multiple_writes () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.set_nonblock pty;

  let lines = [ "First line\n"; "Second line\n"; "Third line\n" ] in
  List.iter
    (fun line ->
      let _ = write_all_with_select pty line 1.0 in
      ())
    lines;

  let buffer = Bytes.create 1024 in
  let expected_total =
    List.fold_left (fun acc s -> acc + String.length s) 0 lines
  in
  let n = read_with_select pty buffer 0 1024 1.0 in

  satisfies
    ~claim:(Printf.sprintf "read at least the %d bytes written" expected_total)
    int
    (fun bytes -> bytes >= expected_total)
    n;
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"contains all lines"
    (String.contains output 'F' && String.contains output 'S'
   && String.contains output 'T');
  Pty.close pty

(* Edge Cases *)

let test_close_idempotent () =
  let master, slave = Pty.open_pty () in
  Pty.close master;
  Pty.close master;
  (* Should not crash *)
  Pty.close slave

(* Regression: close used to sleep 100ms before its first waitpid even when
   the child had already exited — several dropped frames per closed pane. *)
let test_close_reaps_exited_child_without_delay () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "exit 0" ] () in
  (* EOF (or EIO) on the master means the child exited and is reapable. *)
  ignore (read_to_eof pty : string);
  let start = Unix.gettimeofday () in
  Pty.close pty;
  let elapsed = Unix.gettimeofday () -. start in
  is_true
    ~msg:
      (Printf.sprintf "close reaped an exited child in %.0fms" (elapsed *. 1000.))
    (elapsed < 0.05)

(* A keeper that waits for the program's output to be read must not make close
   wait in turn: close hangs up first. *)
let test_close_with_unread_output () =
  let pty = spawn_hello_and_await_exit () in
  Pty.close pty;
  equal ~msg:"close reaped the child" (option int) None (Pty.pid pty)

(* The keeper exits as the program did: by its signal, where sh's own exit
   code for a signal would be 128 + the signal. *)
let test_wait_gives_the_program_s_signal () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "kill -s USR1 $$" ] () in
  ignore (read_to_eof pty : string);
  is_true ~msg:"the program's signal" (Pty.wait pty = Unix.WSIGNALED Sys.sigusr1);
  Pty.close pty

(* The program leads the foreground process group, so the terminal's
   signals are its: the interrupt character written to the master sends
   SIGINT to the foreground group, which the program traps. *)
let test_program_is_the_foreground_group () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap 'echo int; exit 5' INT; echo ready; while :; do :; done" ]
      ()
  in
  let buf = Bytes.create 64 in
  let n = read_with_select pty buf 0 64 2.0 in
  equal ~msg:"ready" string "ready\r\n" (Bytes.sub_string buf 0 n);
  ignore (Pty.write_string pty "\003" 0 1 : int);
  let output = read_to_eof pty in
  is_true ~msg:"the program trapped SIGINT" (output = "^Cint\r\n" || output = "int\r\n");
  is_true ~msg:"its exit code" (Pty.wait pty = Unix.WEXITED 5);
  Pty.close pty

(* terminate signals the program's process group: here [sleep], which the
   shell started in it and which holds the slave, ends with the shell, and
   the master reads the end of output at once. A SIGTERM to the shell alone
   would leave [sleep] holding the terminal open for 10 s. *)
let test_terminate_signals_the_program_s_group () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "sleep 10 & wait" ] () in
  Unix.sleepf 0.1;
  Pty.terminate pty;
  equal ~msg:"no output" string "" (read_to_eof pty);
  is_true ~msg:"ended by SIGTERM" (Pty.wait pty = Unix.WSIGNALED Sys.sigterm);
  Pty.close pty

(* The kernel sends the hangup's SIGHUP to the session's leader, the keeper,
   which passes it on to the program. The master is hung up by replacing
   its descriptor with /dev/null, so that close still has one of its own to
   close. *)
let test_hangup_reaches_the_program () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap 'exit 7' HUP; echo ready; while :; do :; done" ]
      ()
  in
  let buf = Bytes.create 64 in
  ignore (read_with_select pty buf 0 64 2.0 : int);
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Unix.dup2 null (Pty.file_descr pty);
  Unix.close null;
  is_true ~msg:"the program exited on SIGHUP" (Pty.wait pty = Unix.WEXITED 7);
  Pty.close pty

(* A hangup ends a stopped program too: the keeper passes the hangup's
   SIGHUP on with a SIGCONT, as a shell does to its stopped jobs, where the
   SIGHUP alone would stay pending in the stopped program. *)
let test_hangup_ends_a_stopped_program () =
  let pty = Pty.spawn ~prog:"sleep" ~args:[ "10" ] () in
  Unix.kill (Option.get (Pty.pid pty)) Sys.sigstop;
  Unix.sleepf 0.05;
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Unix.dup2 null (Pty.file_descr pty);
  Unix.close null;
  is_true ~msg:"ended by SIGHUP" (Pty.wait pty = Unix.WSIGNALED Sys.sighup);
  Pty.close pty

(* Regression: a handle kept the process id of a child that the program had
   reaped itself, and terminate, kill and close signalled it, though the
   system may have given that id to another process. The handle now checks
   first and forgets the id. The program reaps the keeper with [Unix.wait],
   as the keeper is the only child of this process just then. *)
let test_reaped_child_is_never_signalled () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "exit 0" ] () in
  ignore (read_to_eof pty : string);
  ignore (Unix.wait () : int * Unix.process_status);
  Pty.terminate pty;
  equal ~msg:"terminate forgets the id" (option int) None (Pty.pid pty);
  raises_match ~msg:"the handle has no child" (Exn.invalid_arg ~sub:"Pty.kill")
    (fun () -> Pty.kill pty);
  Pty.close pty

let test_wait_reaps_the_child () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "exit 3" ] () in
  ignore (read_to_eof pty : string);
  is_true ~msg:"the child's status" (Pty.wait pty = Unix.WEXITED 3);
  equal ~msg:"wait forgets the id" (option int) None (Pty.pid pty);
  raises_match ~msg:"a second wait" (Exn.invalid_arg ~sub:"Pty.wait") (fun () ->
      ignore (Pty.wait pty : Unix.process_status));
  Pty.close pty

let test_poll () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "read line; exit 3" ] () in
  is_true ~msg:"running" (Pty.poll pty = None);
  ignore (Pty.write_string pty "\n" 0 1 : int);
  ignore (read_to_eof pty : string);
  let rec exited n =
    match Pty.poll pty with
    | Some status -> status
    | None when n > 0 ->
        Unix.sleepf 0.01;
        exited (n - 1)
    | None -> fail "the child did not exit within a second"
  in
  is_true ~msg:"the child's status" (exited 100 = Unix.WEXITED 3);
  equal ~msg:"wait forgets the id" (option int) None (Pty.pid pty);
  raises_match ~msg:"a second wait" (Exn.invalid_arg ~sub:"Pty.wait")
    (fun () -> ignore (Pty.wait pty : Unix.process_status));
  Pty.close pty

(* A child that ignores SIGTERM exits with its own status, whether the handle
   reaps it in terminate or in wait. *)
let test_wait_after_terminate () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "trap '' TERM; exit 4" ] () in
  ignore (read_to_eof pty : string);
  Pty.terminate pty;
  is_true ~msg:"the child's status" (Pty.wait pty = Unix.WEXITED 4);
  Pty.close pty

(* close reaps the child and keeps the program's status for wait. *)
let test_wait_after_close () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "exit 3" ] () in
  ignore (read_to_eof pty : string);
  Pty.close pty;
  equal ~msg:"the child is reaped" (option int) None (Pty.pid pty);
  is_true ~msg:"the program's status" (Pty.wait pty = Unix.WEXITED 3);
  raises_match ~msg:"a second wait" (Exn.invalid_arg ~sub:"Pty.wait") (fun () ->
      ignore (Pty.wait pty : Unix.process_status))

(* Runs [f] while another process sends SIGUSR1 to this one every
   millisecond or so, which interrupts the system calls that block. *)
let with_interrupts f =
  let previous = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> ())) in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let script =
    Printf.sprintf "while kill -USR1 %d 2>/dev/null; do sleep 0.001; done"
      (Unix.getpid ())
  in
  let sender =
    Unix.create_process "sh" [| "sh"; "-c"; script |] null null null
  in
  Unix.close null;
  (* A signal the sender sent before it was killed can interrupt the reap. *)
  let rec reap () =
    try ignore (Unix.waitpid [] sender : int * Unix.process_status)
    with Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.kill sender Sys.sigkill;
      reap ();
      Sys.set_signal Sys.sigusr1 previous)
    f

(* Regression: close's reap took EINTR from waitpid for the child's exit,
   and returned with the child unreaped when a signal arrived while it
   waited. Close and wait now reap through one wait that retries. *)
let test_reap_through_signals () =
  let pty = Pty.spawn ~prog:"sleep" ~args:[ "0.2" ] () in
  let status = with_interrupts (fun () -> Pty.wait pty) in
  is_true ~msg:"wait gives the exit status" (status = Unix.WEXITED 0);
  Pty.close pty;
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap '' TERM HUP; echo ready; while :; do :; done" ]
      ()
  in
  ignore (Unix.select [ Pty.in_fd pty ] [] [] 2. : _ * _ * _);
  with_interrupts (fun () -> Pty.close pty);
  equal ~msg:"close reaped the child" (option int) None (Pty.pid pty)

(* Regression: close ~wait:false sent SIGTERM alone, so a child that
   ignores it and the hangup's SIGHUP ran on, though close promises SIGKILL
   about 100 ms later. The test's timeout bounds the wait. *)
let test_close_without_wait_kills () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap '' TERM HUP; echo ready; while :; do :; done" ]
      ()
  in
  (* The traps are set once the child has written. *)
  ignore (Unix.select [ Pty.in_fd pty ] [] [] 2. : _ * _ * _);
  Pty.close ~wait:false pty;
  is_true ~msg:"SIGKILL ended the program"
    (Pty.wait pty = Unix.WSIGNALED Sys.sigkill)

(* Regression: close sent SIGTERM right behind the hangup's SIGHUP, which
   ended a shell before its trap for SIGHUP ran. *)
let test_close_lets_the_program_act_on_the_hangup () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap 'exit 7' HUP; echo ready; while :; do :; done" ]
      ()
  in
  ignore (Unix.select [ Pty.in_fd pty ] [] [] 2. : _ * _ * _);
  Pty.close pty;
  is_true ~msg:"the program's trap ran" (Pty.wait pty = Unix.WEXITED 7)

(* Regression: when another process held the master too, closing the
   handle's copy hung nothing up, so the program received no SIGHUP, only the
   SIGKILL 100 ms later. Here the other holder is a copy in this process. *)
let test_close_hangs_up_a_shared_master () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:[ "-c"; "trap 'exit 7' HUP; echo ready; while :; do :; done" ]
      ()
  in
  ignore (Unix.select [ Pty.in_fd pty ] [] [] 2. : _ * _ * _);
  let copy = Unix.dup (Pty.file_descr pty) in
  Pty.close pty;
  Unix.close copy;
  is_true ~msg:"the program's trap ran" (Pty.wait pty = Unix.WEXITED 7)

(* Regression: on the BSDs and macOS, close waited for the keeper, which
   waited for the master to read the program's output, when another process
   held the master and did not read it. The test's timeout bounds the wait. *)
let test_close_with_a_shared_master_and_unread_output () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo unread" ] () in
  Unix.sleepf 0.2;
  let copy = Unix.dup (Pty.file_descr pty) in
  let start = Unix.gettimeofday () in
  Pty.close pty;
  let elapsed = Unix.gettimeofday () -. start in
  Unix.close copy;
  is_true
    ~msg:(Printf.sprintf "close returned in %.0f ms" (elapsed *. 1000.))
    (elapsed < 1.);
  is_true ~msg:"the program's status" (Pty.wait pty = Unix.WEXITED 0)

(* Regression: once the program had exited, nothing ended a process it left
   in its group that ignores SIGHUP and SIGTERM, and that process held the
   terminal open. The keeper now kills the group once the program's output
   is read, so the master reads the end of output at once. *)
let test_job_ends_with_the_program () =
  let pty =
    Pty.spawn ~prog:"sh"
      ~args:
        [
          "-c"; "trap '' HUP TERM; (trap '' HUP TERM; exec sleep 30) & echo $!";
        ]
      ()
  in
  let start = Unix.gettimeofday () in
  let straggler = int_of_string (String.trim (read_to_eof pty)) in
  is_true ~msg:"the end of output came at once"
    (Unix.gettimeofday () -. start < 5.);
  is_true ~msg:"the program's status" (Pty.wait pty = Unix.WEXITED 0);
  (* Init reaps the straggler, which is not this process's child. *)
  let rec gone n =
    match Unix.kill straggler 0 with
    | () when n > 0 ->
        Unix.sleepf 0.01;
        gone (n - 1)
    | () -> false
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true
  in
  is_true ~msg:"the straggler is gone" (gone 200);
  Pty.close pty

(* The program's PATH, not the parent's, is where [prog] is searched for. *)
let test_spawn_searches_the_path_of_env () =
  let dir = Filename.temp_dir "pty" "" in
  let script = Filename.concat dir "pty-test-found" in
  Out_channel.with_open_text script (fun oc ->
      output_string oc "#!/bin/sh\necho found\n");
  Unix.chmod script 0o755;
  let env = [| "PATH=" ^ dir |] in
  let output =
    Pty.with_spawn ~env ~prog:"pty-test-found" ~args:[] read_to_eof
  in
  Sys.remove script;
  Sys.rmdir dir;
  equal ~msg:"found in env's PATH" string "found\r\n" output

(* Regression: a second close used to call Unix.close on the stored fd number
   again. The kernel reallocates freed numbers to the next opened descriptor,
   so the second close destroyed an unrelated fd. *)
let test_double_close_does_not_touch_reused_fd () =
  let master, slave = Pty.open_pty () in
  Pty.close master;
  Pty.close slave;
  (* The freed fd numbers are the lowest available, so the pipe reuses them. *)
  let probe_r, probe_w = Unix.pipe () in
  Pty.close master;
  Pty.close slave;
  let alive =
    try
      ignore (Unix.write probe_w (Bytes.of_string "x") 0 1 : int);
      true
    with Unix.Unix_error (Unix.EBADF, _, _) -> false
  in
  (try Unix.close probe_r with Unix.Unix_error _ -> ());
  (try Unix.close probe_w with Unix.Unix_error _ -> ());
  is_true ~msg:"second close does not close unrelated fds" alive

let test_eof_on_close () =
  let master, slave = Pty.open_pty () in
  Pty.close master;
  let buf = Bytes.create 10 in
  let n = Pty.read slave buf 0 10 in
  equal ~msg:"EOF on master close" int 0 n;
  Pty.close slave

let test_pty_close () =
  let pty = Pty.spawn ~prog:"cat" ~args:[] () in
  Pty.close pty;
  let buffer = Bytes.create 16 in
  let failed =
    try
      let _ = Pty.read pty buffer 0 16 in
      false
    with Unix.Unix_error (Unix.EBADF, _, _) -> true
  in
  is_true ~msg:"read after close fails" failed

(* Test Suite *)

let tests =
  if not pty_test_supported then
    [ test "pty unavailable (skipped)" test_pty_unavailable ]
  else
    [
      (* Basic Operations *)
      test "open pty" test_open_pty;
      test "open pty with winsize" test_open_pty_with_winsize;
      test "with_pty" test_with_pty;
      (* Window Size *)
      test "winsize operations" test_winsize_operations;
      test "resize" test_resize;
      test "inherit size" test_inherit_size;
      (* Process Spawning *)
      test "spawn echo" test_spawn_echo;
      test "spawn cat" test_spawn_cat;
      test "spawn sh" test_spawn_sh;
      test "spawn with env" test_spawn_with_env;
      test "spawn with cwd" test_spawn_with_cwd;
      test "spawn output then eof" test_spawn_output_then_eof;
      test "spawn output read late" test_spawn_output_read_late;
      test "slave not inherited" test_slave_not_inherited;
      test "slave on a std fd" test_slave_on_std_fd;
      test "spawn while a domain runs" test_spawn_while_a_domain_runs;
      test ~timeout:30. "read while another domain writes"
        test_read_while_another_domain_writes;
      test "spawn script without shebang" test_spawn_script_without_shebang;
      test "spawn resets ignored signals" test_spawn_resets_ignored_signals;
      test "spawn unblocks signals" test_spawn_unblocks_signals;
      test "spawn inherits only std fds" test_spawn_inherits_only_std_fds;
      test "spawn missing program raises" test_spawn_missing_program_raises;
      test "spawn missing cwd raises" test_spawn_missing_cwd_raises;
      test "failed spawn skips parent at_exit"
        test_failed_spawn_does_not_run_parent_at_exit;
      (* Non-blocking I/O *)
      test "nonblocking io" test_nonblocking_io;
      test "multiple writes" test_multiple_writes;
      (* Edge Cases *)
      test "close idempotent" test_close_idempotent;
      test "close reaps exited child without delay"
        test_close_reaps_exited_child_without_delay;
      (* The timeout bounds close, which would block in its reap. *)
      test ~timeout:5. "close with unread output" test_close_with_unread_output;
      test "double close does not touch reused fd"
        test_double_close_does_not_touch_reused_fd;
      test "a reaped child is never signalled"
        test_reaped_child_is_never_signalled;
      test "wait gives the program's signal" test_wait_gives_the_program_s_signal;
      test "the program is the foreground group"
        test_program_is_the_foreground_group;
      test "terminate signals the program's group"
        test_terminate_signals_the_program_s_group;
      test "hangup reaches the program" test_hangup_reaches_the_program;
      test ~timeout:5. "hangup ends a stopped program"
        test_hangup_ends_a_stopped_program;
      test "wait reaps the child" test_wait_reaps_the_child;
      test "poll" test_poll;
      test "wait after terminate" test_wait_after_terminate;
      test "wait after close" test_wait_after_close;
      test "reap through signals" test_reap_through_signals;
      test ~timeout:5. "close without wait kills" test_close_without_wait_kills;
      test ~timeout:5. "close hangs up a shared master"
        test_close_hangs_up_a_shared_master;
      test ~timeout:5. "close with a shared master and unread output"
        test_close_with_a_shared_master_and_unread_output;
      test ~timeout:5. "close lets the program act on the hangup"
        test_close_lets_the_program_act_on_the_hangup;
      test ~timeout:10. "the job ends with the program"
        test_job_ends_with_the_program;
      test "spawn searches the PATH of env" test_spawn_searches_the_path_of_env;
      test "eof on close" test_eof_on_close;
      test "pty close" test_pty_close;
    ]

let () = exit (run "matrix.pty" [ group "pty" tests ])
