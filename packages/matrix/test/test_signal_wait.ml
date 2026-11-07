(* A holder's wait and read, on every system: on Windows its wake is an
   event and its deadline a high-resolution timer, on POSIX a pipe and
   select(2)'s timeout. *)

open Windtrap
module Signal = Tty.Signal

let ms = 1_000_000

let with_holder f =
  let h = Signal.hold ~controlling:false ~output:None in
  Fun.protect ~finally:(fun () -> Signal.release h) (fun () -> f h)

(* A file holding [contents], open for reading. *)
let with_file contents f =
  let path = Filename.temp_file "test_signal_wait" ".in" in
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  let fd = Unix.openfile path [ O_RDONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close fd;
      Sys.remove path)
    (fun () -> f fd)

let test_deadline () =
  with_holder @@ fun h ->
  let start = Tty.now () in
  is_false ~msg:"nothing to read"
    (Signal.wait h ~input:None ~until:(Some (start + (20 * ms))));
  is_true ~msg:"not before its deadline" (Tty.now () - start >= 20 * ms);
  is_false ~msg:"a deadline past only looks"
    (Signal.wait h ~input:None ~until:(Some start))

let test_wake () =
  with_holder @@ fun h ->
  Signal.wake h;
  is_false ~msg:"a wake owed ends the wait"
    (Signal.wait h ~input:None ~until:None);
  let start = Tty.now () in
  ignore (Signal.wait h ~input:None ~until:(Some (start + (10 * ms))) : bool);
  is_true ~msg:"the wait took the wake" (Tty.now () - start >= 10 * ms)

let test_wake_from_a_thread () =
  with_holder @@ fun h ->
  let waker =
    Thread.create
      (fun () ->
        Thread.delay 0.05;
        Signal.wake h)
      ()
  in
  let start = Tty.now () in
  ignore
    (Signal.wait h ~input:None ~until:(Some (start + (10_000 * ms))) : bool);
  Thread.join waker;
  is_true ~msg:"woken before the deadline" (Tty.now () - start < 5_000 * ms)

let test_file () =
  with_file "hello" @@ fun fd ->
  with_holder @@ fun h ->
  is_true ~msg:"a file is readable" (Signal.wait h ~input:(Some fd) ~until:None);
  let buf = Bytes.create 16 in
  let n = Signal.read h fd buf 0 16 in
  equal ~msg:"its contents" string "hello" (Bytes.sub_string buf 0 n);
  equal ~msg:"its end" int 0 (Signal.read h fd buf 0 16)

let test_released () =
  let h = Signal.hold ~controlling:false ~output:None in
  Signal.release h;
  raises ~msg:"wait" (Invalid_argument "Tty.Signal: a released holder")
    (fun () -> ignore (Signal.wait h ~input:None ~until:(Some 0) : bool));
  Signal.wake h

let () =
  exit
    (run "matrix.signal_wait"
       [
         group "Signal"
           [
             test "a deadline" test_deadline;
             test "a wake" test_wake;
             test "a wake from another thread" test_wake_from_a_thread;
             test "a file" test_file;
             test "a released holder" test_released;
           ];
       ])
