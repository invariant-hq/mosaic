(** Mosaic programs run as Lwt promises.

    {!run} runs a program in the terminal as {!Mosaic.val-run} does, and is a
    promise that resolves once the program has ended. The program runs on a
    domain of its own, so that its steps never hold up the thread that runs
    Lwt's promises, {e Lwt's thread}. The work of {!Mosaic.Cmd.val-perform} and
    {!Mosaic.Sub.val-worker} runs on Lwt's thread
    ([Lwt_preemptive.run_in_main_dont_wait]), where it may start promises. Work
    that runs there holds up every promise until it returns, so it must start
    its promises and return. A worker that returns keeps its mailbox until its
    stop is requested ({!Mosaic.Sub.val-worker}).

    The promise resolves once the program has ended as {!Mosaic.val-run}'s does
    and given the terminal back, and is rejected with what the program raised.
    Cancelling it ends the program by its stop request ({!Matrix.Stop}). Lwt
    rejects a cancelled promise with [Lwt.Canceled] immediately, and [Lwt.pick]
    resolves without waiting for the promises it cancels, so the program gives
    the terminal back on its domain after the promise is rejected, once it has
    waited for the replies the terminal owes ({!Matrix.Stop.request}). If the
    process exits first, its exit path gives the terminal back. Code that must
    wait for the terminal, such as code that writes to it after the program,
    ends the program by requesting the stop given to {!run} and waits for the
    promise.

    This program runs [app] until it ends, or for at most a minute.
    {[
    let () = Lwt_main.run (Lwt.pick [ Mosaic_lwt.run app; Lwt_unix.sleep 60. ])
    ]}

    {!run} uses only {!Mosaic.val-run}, and its source is a worked example of
    running a program beside another event loop. *)

val run :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Matrix.Terminal.Caps.env ->
  ?stop:Matrix.Stop.t ->
  ?config:Matrix.config ->
  ('model, 'msg) Mosaic.app ->
  unit Lwt.t
(** [run app] is a promise that runs [app] in the terminal on a domain of its
    own, and resolves once the program has ended and given the terminal back.
    Cancelling it requests [stop] ({!Matrix.Stop.request}), a stop of its own
    when [stop] is absent, and rejects it with [Lwt.Canceled] before the
    terminal is given back. A stop request made before the program has begun
    ends it as it begins. Work that sends with [mailbox.send]
    ({!Mosaic.type-mailbox}) blocks Lwt's thread until the program has applied
    the message, and work that dispatches while 65,536 posts to the program's
    run are pending ({!Matrix.post}) blocks it until the program has taken half
    of them. The optional arguments are {!Mosaic.val-run}'s.

    The promise is rejected with what {!Mosaic.val-run} raises. [run] itself
    raises [Failure] if no domain can be spawned for the program, and
    [Unix.Unix_error] if no pipe can be opened for its end. *)
