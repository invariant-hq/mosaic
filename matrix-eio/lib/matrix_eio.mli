(** Eio-based runtime for {!Matrix}.

    [Matrix_eio] provides an Eio-native event loop for terminal applications.
    Use {!create} to build an {!Matrix.app} with Eio-backed I/O, then pass it to
    {!Matrix.run}. All other Matrix functions ({!Matrix.prepare},
    {!Matrix.submit}, {!Matrix.grid}, etc.) work unchanged.

    {1:quick_start Quick start}

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let app =
        Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:env#stdin
          ~stdout:env#stdout ()
      in
      Matrix.run app ~on_render:(fun app ->
          let g = Matrix.grid app in
          (* draw … *)
          ())
    ]} *)

(** {1:creating Creating} *)

val create :
  ?mode:Matrix.mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?mouse:Matrix.Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?focus_reporting:bool ->
  ?kitty_keyboard:Matrix.kitty_keyboard ->
  ?exit_on_ctrl_c:bool ->
  ?debug_overlay:bool ->
  ?debug_overlay_corner:Matrix.debug_overlay_corner ->
  ?debug_overlay_capacity:int ->
  ?frame_dump_every:int ->
  ?frame_dump_dir:string ->
  ?frame_dump_pattern:string ->
  ?frame_dump_hits:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  ?min_tui_height:int ->
  ?signal_handlers:bool ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  stdin:_ Eio_unix.source ->
  stdout:_ Eio_unix.sink ->
  unit ->
  Matrix.app
(** [create ~sw ~clock ~stdin ~stdout ()] is a live {!Matrix.app} with
    Eio-backed I/O.

    Equivalent to {!Matrix.create} but integrates with the Eio scheduler: the
    event loop yields to other Eio fibers while waiting for terminal input. The
    application is automatically closed when [sw] is released.

    The mandatory parameters are:
    - [sw] — Eio switch controlling the application lifetime. The app is closed
      when [sw] is released.
    - [clock] — Eio clock for frame timing and input timeouts. Typically
      [Eio.Stdenv.clock env].
    - [stdin] — Eio source for terminal input (typically [env#stdin]).
    - [stdout] — Eio sink for terminal output (typically [env#stdout]).

    All optional parameters match {!Matrix.create}; see its documentation for
    details. *)
