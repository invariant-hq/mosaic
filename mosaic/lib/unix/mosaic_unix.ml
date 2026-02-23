let run ?mode ?raw_mode ?target_fps ?respect_alpha ?mouse_enabled ?mouse
    ?bracketed_paste ?focus_reporting ?kitty_keyboard ?exit_on_ctrl_c
    ?debug_overlay ?debug_overlay_corner ?debug_overlay_capacity
    ?frame_dump_every ?frame_dump_dir ?frame_dump_pattern ?frame_dump_hits
    ?cursor_visible ?explicit_width ?input_timeout ?resize_debounce ?output
    ?signal_handlers ?initial_caps app =
  let target_fps = Option.value target_fps ~default:(Some 60.) in
  let exit_on_ctrl_c = Option.value exit_on_ctrl_c ~default:true in
  let config =
    Matrix.create ?mode ?raw_mode ~target_fps ?respect_alpha ?mouse_enabled
      ?mouse ?bracketed_paste ?focus_reporting ?kitty_keyboard ~exit_on_ctrl_c
      ?debug_overlay ?debug_overlay_corner ?debug_overlay_capacity
      ?frame_dump_every ?frame_dump_dir ?frame_dump_pattern ?frame_dump_hits
      ?cursor_visible ?explicit_width ?input_timeout ?resize_debounce ()
  in
  let matrix =
    Matrix_unix.create ?output ?signal_handlers ?initial_caps config
  in
  Mosaic.run ~matrix app
