open Eio.Std

(* Returns once one of the program's descriptors is readable or its deadline
   has come. Cancelling the fiber ends the wait. The deadline is on the run's
   clock, [Matrix.clock], so only the time left until it is handed to Eio,
   whose own monotonic clock measures it. *)
let wait program =
  let deadline () =
    match Mosaic.deadline program with
    | None -> Fiber.await_cancel ()
    | Some deadline ->
        let seconds = Float.of_int (deadline - Matrix.clock ()) *. 1e-9 in
        if seconds > 0. then Eio_unix.sleep seconds
  in
  let readable fd () = Eio_unix.await_readable fd in
  Fiber.any (deadline :: List.map readable (Mosaic.fds program))

(* Windows has no descriptor to wait for a console with, so there the
   program's domain runs with no Eio loop and waits as [Mosaic.run] does.
   [run_raw] cannot be cancelled: the second fiber turns the cancellation of
   the caller into the run's stop, and the first returns once the terminal is
   given back. *)
let run_alone ?input ?output ?env ?config ~process_perform domain_mgr app =
  let stop = Matrix.Stop.create () in
  Fiber.first
    (fun () ->
      Eio.Domain_manager.run_raw domain_mgr (fun () ->
          Mosaic.run ?input ?output ?env ~stop ?config ~process_perform app))
    (fun () ->
      try Fiber.await_cancel ()
      with Eio.Cancel.Cancelled _ as e ->
        Matrix.Stop.request stop;
        raise e)

(* The work of the program, run as fibers of the calling domain. A switch
   belongs to the domain that made it, so the program's domain hands the
   work over through a stream, which any domain may add to, even one with no
   Eio loop, as the stream never fills, and a fiber of the calling domain
   forks it. The fibers are daemons: they end with the switch, once the
   program has ended. *)
let run ?input ?output ?env ?config domain_mgr app =
  Switch.run @@ fun sw ->
  let work = Eio.Stream.create max_int in
  Fiber.fork_daemon ~sw (fun () ->
      while true do
        let f = Eio.Stream.take work in
        Fiber.fork_daemon ~sw (fun () ->
            f ();
            `Stop_daemon)
      done;
      `Stop_daemon);
  let process_perform = Eio.Stream.add work in
  if Sys.win32 then
    run_alone ?input ?output ?env ?config ~process_perform domain_mgr app
  else
    Eio.Domain_manager.run domain_mgr @@ fun () ->
    Mosaic.start ?input ?output ?env ?config ~process_perform app
    @@ fun program ->
    let rec loop () =
      match Mosaic.step program ~now:(Matrix.clock ()) with
      | `Stepped -> loop ()
      | `Idle ->
          wait program;
          loop ()
      | `Stopped -> ()
    in
    loop ()
