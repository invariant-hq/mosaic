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
  is_true ~msg:"read some data" (n > 0);
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
  is_true ~msg:"read at least the data" (n >= String.length test_data);
  let output = Bytes.sub_string buffer 0 n in
  is_true ~msg:"contains test data" (String.contains output 'H');
  Pty.close pty

let test_spawn_sh () =
  let pty = Pty.spawn ~prog:"sh" ~args:[ "-c"; "echo 'Shell test'" ] () in
  Pty.set_nonblock pty;
  let buffer = Bytes.create 256 in
  let n = read_with_select pty buffer 0 256 1.0 in
  is_true ~msg:"read some data" (n > 0);
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
  is_true ~msg:"read some data" (n > 0);
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

  is_true ~msg:"read at least expected data" (n >= expected_total);
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
      (* Non-blocking I/O *)
      test "nonblocking io" test_nonblocking_io;
      test "multiple writes" test_multiple_writes;
      (* Edge Cases *)
      test "close idempotent" test_close_idempotent;
      test "eof on close" test_eof_on_close;
      test "pty close" test_pty_close;
    ]

let () = run "matrix.pty" [ group "pty" tests ]
