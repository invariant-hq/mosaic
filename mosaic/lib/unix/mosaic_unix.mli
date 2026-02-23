(** Unix convenience runner for Mosaic TEA applications.

    Creates a {!Matrix.app}, attaches Unix I/O via {!Matrix_unix}, and runs the
    TEA event loop. All Matrix configuration parameters are forwarded to
    {!Matrix.create}.

    {[
      let () = Mosaic_unix.run { init; update; view; subscriptions }
    ]} *)

val run :
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
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?initial_caps:Matrix.Terminal.capabilities ->
  ('model, 'msg) Mosaic.app ->
  unit
(** [run app] creates a Matrix application, sets up Unix I/O, and runs the TEA
    event loop until the application exits.

    Defaults to 60 FPS, alternate screen, raw mode, mouse and bracketed paste
    enabled, and Ctrl+C exits ([exit_on_ctrl_c] defaults to [true]). *)
