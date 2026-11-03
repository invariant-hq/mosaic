(* A worker runs as long as the program declares it, so it cannot run on the
   session's queue, whose work must return. Under Eio it runs on a fiber of
   the program's switch. With Eio's mock scheduler, a 1 ns sleep returns
   only once every other fiber waits, which is the session's quiesce: the
   test settles the program with no thread and no wall clock. *)

open Windtrap

type msg = Line of string | Stopped

let app lines =
  {
    Mosaic.init = (fun () -> ([], Mosaic.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Line line -> (line :: model, Mosaic.Cmd.none)
        | Stopped -> (model, Mosaic.Cmd.quit));
    view = (fun model -> Mosaic.text (String.concat "," (List.rev model)));
    subscriptions =
      (fun _ ->
        Mosaic.Sub.worker ~id:"lines" (fun _stop (mailbox : _ Mosaic.mailbox) ->
            let rec loop () =
              match Eio.Stream.take lines with
              | Some line ->
                  (* Work that takes turns with the scheduler's other
                     fibers. *)
                  Eio.Fiber.yield ();
                  mailbox.dispatch (Line line);
                  loop ()
              | None -> mailbox.dispatch Stopped
            in
            loop ()));
  }

let test_worker_on_fibers () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let process_perform job =
    Eio.Fiber.fork_daemon ~sw (fun () ->
        job ();
        `Stop_daemon)
  in
  (* The mock clock's trace line, one a sleep, is not the test's output. *)
  let quiesce () =
    Eio.Fiber.with_binding Eio.Private.Debug.v#traceln
      {
        traceln =
          (fun ?__POS__:_ fmt ->
            Format.ikfprintf ignore Format.err_formatter fmt);
      }
      (fun () -> Eio.Time.Mono.sleep env#mono_clock 1e-9)
  in
  let lines = Eio.Stream.create max_int in
  let term = Matrix_test.Terminal.create ~cols:20 ~rows:1 () in
  let shown () =
    String.trim (Matrix_test.Screen.row (Matrix_test.Terminal.screen term) 0)
  in
  Mosaic_test.run ~process_perform ~quiesce term (app lines) @@ fun s ->
  equal ~msg:"nothing yet" string "" (shown ());
  Eio.Stream.add lines (Some "one");
  Eio.Stream.add lines (Some "two");
  Matrix_test.settle s;
  equal ~msg:"settled with the worker's lines" string "one,two" (shown ());
  equal ~msg:"no time passed" int 0 (Matrix_test.now s);
  Eio.Stream.add lines None;
  Matrix_test.settle s;
  is_true ~msg:"the worker's message ended the run" (Matrix_test.ended s)

let () =
  exit
    (run "mosaic.eio_mock"
       [
         group "Workers"
           [
             test "a worker on Eio's mock fibers settles" test_worker_on_fibers;
           ];
       ])
