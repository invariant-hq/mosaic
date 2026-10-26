(** Mosaic programs run from a fiber of an Eio program.

    {!run} runs a program in the terminal as {!Mosaic.val-run} does. The program
    takes its steps on a domain of its own, which the domain manager given to
    {!run} creates, and waits there with Eio until one of its descriptors is
    readable or its deadline comes ({!Mosaic.fds}, {!Mosaic.deadline}). The
    calling domain goes on running its other fibers meanwhile, and runs the work
    of {!Mosaic.Cmd.val-perform} and {!Mosaic.Sub.val-worker}, each as a fiber
    of its own, so that this work may call Eio. Neither domain waits for the
    other, except where work sends with [mailbox.send], or dispatches while
    65,536 of the program's posts are pending ({!run}).

    The program reads its time from {!Matrix.clock}, as {!Mosaic.val-run} does,
    and Eio's own monotonic clock measures the waits until its deadlines. A
    clock the Eio program uses instead, such as a mock clock in a test, moves
    neither.

    The program ends as {!Mosaic.val-run}'s does, or when the fiber that called
    {!run} is cancelled. Either way it gives the terminal back on its domain
    before {!run} returns or raises. Work still running when the program ends is
    cancelled with the fibers that run it, as a switch cancels its daemon
    fibers; what it gives the program by then is dropped.

    This program runs [app] until it ends. Other fibers of [Eio_main.run] would
    run beside it.
    {[
    let () =
      Eio_main.run @@ fun env -> Mosaic_eio.run (Eio.Stdenv.domain_mgr env) app
    ]}

    {!run} uses only {!Mosaic.val-start} and the functions of the program it
    gives, so its source is a worked example of running a program under another
    event loop. *)

val run :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Matrix.Terminal.Caps.env ->
  ?config:Matrix.config ->
  _ Eio.Domain_manager.t ->
  ('model, 'msg) Mosaic.app ->
  unit
(** [run domain_mgr app] runs [app] in the terminal until it ends, on a domain
    that [domain_mgr] creates. It blocks the calling fiber until the program has
    ended and given the terminal back. Cancelling the calling fiber ends the
    program, and [run] then raises [Eio.Cancel.Cancelled]. A program still
    taking the terminal sees the cancellation once that is done, as it sees a
    stop request ({!Matrix.Stop.request}). Work that sends with [mailbox.send]
    ({!Mosaic.type-mailbox}) blocks the calling domain until the program has
    applied the message, and work that dispatches while 65,536 posts to the
    program's run are pending ({!Matrix.post}) blocks it until the program has
    taken half of them. The optional arguments are {!Mosaic.val-run}'s, and
    cancelling the calling fiber takes the place of its [stop].

    Raises what {!Mosaic.val-run} raises. *)
