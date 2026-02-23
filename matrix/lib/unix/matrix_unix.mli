(** Unix runtime for Matrix applications.

    Provides Unix-based I/O (file descriptors, select, signals) to drive a
    {!Matrix.app}. Handles raw mode, capability probing, wakeup pipe, SIGWINCH,
    and shutdown signal handlers.

    {[
      let config = Matrix.create ~mode:`Alt () in
      Matrix_unix.run config ~on_render:(fun app ->
          Grid.put_string (Matrix.grid app) ~row:1 ~col:1 "Hello")
    ]} *)

val create :
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  Matrix.config ->
  Matrix.app
(** [create config] wires Unix I/O to [config] and returns a live application.

    Creates the terminal, enters raw mode, probes capabilities, installs signal
    handlers, and calls {!Matrix.attach}.

    @param output Output target for rendered frames. Defaults to [`Stdout].
    @param signal_handlers
      Whether to install SIGTERM/SIGINT/SIGQUIT handlers. Defaults to [true].
    @param initial_caps Seed capabilities passed to {!Matrix.Terminal.make}. *)

val run :
  ?on_frame:(Matrix.app -> dt:float -> unit) ->
  ?on_input:(Matrix.app -> Matrix.Input.t -> unit) ->
  ?on_resize:(Matrix.app -> cols:int -> rows:int -> unit) ->
  on_render:(Matrix.app -> unit) ->
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  Matrix.config ->
  unit
(** [run config ~on_render] calls {!create}, fires [on_resize] with the initial
    terminal dimensions, runs the event loop via {!Matrix.run}, and cleans up on
    exit.

    @param output Output target for rendered frames. Defaults to [`Stdout].
    @param signal_handlers
      Whether to install SIGTERM/SIGINT/SIGQUIT handlers. Defaults to [true].
    @param initial_caps Seed capabilities passed to {!Matrix.Terminal.make}. *)

val install_signal_handlers : unit -> unit
(** [install_signal_handlers ()] installs shutdown signal handlers (SIGTERM,
    SIGINT, SIGQUIT, SIGABRT) manually. Idempotent. *)
