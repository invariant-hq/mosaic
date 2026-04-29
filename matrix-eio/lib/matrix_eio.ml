(* ───── SIGWINCH Handling ───── *)

(* Signal handlers cannot use Eio primitives, so we poll an atomic flag at most
   every 50ms — imperceptible for resize events. *)
let sigwinch_poll_interval = 0.05
let winch_flag = Atomic.make false

(* CR: I think it would be fine to have this handler in the loop throwing the
   on_event directly without going through an atomic *)
let install_winch_handler () =
  try
    Sys.set_signal Sys.sigwinch
      (Sys.Signal_handle (fun _ -> Atomic.set winch_flag true))
  with Invalid_argument _ -> ()

(* ───── Application Setup ───── *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt)
    ?(explicit_width = false) ?(input_timeout = None)
    ?(resize_debounce = Some 0.1) ?(min_tui_height = 1) ?(start_idle = false)
    ?(signal_handlers = true) ?initial_caps ~sw ~clock ~stdin ~stdout () =
  let input_eio_fd = Eio_unix.Resource.fd stdin in
  let output_eio_fd = Eio_unix.Resource.fd stdout in
  let output_is_tty =
    Eio_unix.Fd.use_exn "is_tty" output_eio_fd Matrix.Terminal.is_tty
  in
  let input_is_tty =
    Eio_unix.Fd.use_exn "is_tty" input_eio_fd Matrix.Terminal.is_tty
  in
  let wakeup = Eio.Condition.create () in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Eio.Flow.copy_string s stdout)
      ~tty:output_is_tty ?initial_caps ()
  in
  let parser = Matrix.Input.Parser.create () in
  let original_termios = ref None in
  if input_is_tty && raw_mode then
    original_termios :=
      Some (Eio_unix.Fd.use_exn "set_raw" input_eio_fd Matrix.Terminal.set_raw);
  let input_buffer = Bytes.create 4096 in
  let input_cs = Cstruct.create 4096 in
  let await_readable ~timeout =
    match
      Eio.Time.with_timeout clock timeout (fun () ->
          Eio_unix.Fd.use_exn "await" input_eio_fd (fun fd ->
              Eio_unix.await_readable fd);
          Ok ())
    with
    | Ok () -> true
    | Error `Timeout -> false
  in
  let read_stdin () =
    let n = Eio.Flow.single_read stdin input_cs in
    Cstruct.blit_to_bytes input_cs 0 input_buffer 0 n;
    n
  in
  if input_is_tty && raw_mode then
    Matrix.Terminal.probe ~timeout:0.5
      ~on_event:(fun _ -> ())
      ~read_into:(fun buf off len ->
        let cs =
          if len <= Cstruct.length input_cs then Cstruct.sub input_cs 0 len
          else Cstruct.create len
        in
        try
          let n = Eio.Flow.single_read stdin cs in
          Cstruct.blit_to_bytes cs 0 buf off n;
          n
        with End_of_file -> 0)
      ~wait_readable:await_readable ~parser terminal;
  let terminal_size () =
    Eio_unix.Fd.use_exn "size" output_eio_fd Matrix.Terminal.size
  in
  let width, height =
    let cols, rows = terminal_size () in
    (max 1 cols, max 1 rows)
  in
  let query_cursor_position ~timeout =
    (* CR: don't hardcode ansi sequences. use Ansi *)
    Matrix.Terminal.send terminal "\027[6n";
    let result = ref None in
    let on_response = function
      | Matrix.Input.Response.Capability
          (Matrix.Input.Response.Cursor_position (row, col)) ->
          result := Some (row, col)
      | Matrix.Input.Response.Capability event ->
          Matrix.Terminal.apply_capability_event terminal event
      | Matrix.Input.Response.Clipboard _ | Matrix.Input.Response.Osc _
      | Matrix.Input.Response.Unknown _ ->
          ()
    in
    let deadline = Eio.Time.now clock +. timeout in
    let rec loop () =
      if Option.is_some !result then ()
      else
        let remaining = deadline -. Eio.Time.now clock in
        if remaining <= 0. then ()
        else if await_readable ~timeout:remaining then (
          (try
             let n = read_stdin () in
             let now = Eio.Time.now clock in
             Matrix.Input.Parser.feed parser input_buffer 0 n ~now
               ~on_event:(fun _ -> ())
               ~on_response
           with End_of_file -> ());
          loop ())
    in
    loop ();
    !result
  in
  let render_offset_of_cursor ~height row col =
    let row = max 1 (min height row) in
    let col = max 1 col in
    if row >= height then (
      Matrix.Terminal.send terminal "\r\n";
      (height - 1, false))
    else if col = 1 then (max 0 (row - 1), true)
    else (row, true)
  in
  let render_offset, static_needs_newline =
    if mode = `Primary && input_is_tty && raw_mode then
      match query_cursor_position ~timeout:0.1 with
      | Some (row, col) -> render_offset_of_cursor ~height row col
      | None -> (0, true)
    else if mode = `Primary then (0, true)
    else (0, false)
  in
  (* ───── IO Callbacks ───── *)
  let now () = Eio.Time.now clock in
  let wake () = Eio.Condition.broadcast wakeup in
  let set_raw_mode enabled =
    match (enabled, !original_termios) with
    | true, Some _ | false, None -> ()
    | true, None ->
        if input_is_tty then
          original_termios :=
            Some
              (Eio_unix.Fd.use_exn "set_raw" input_eio_fd
                 Matrix.Terminal.set_raw)
    | false, Some saved ->
        Eio_unix.Fd.use_exn "restore" input_eio_fd (fun fd ->
            Matrix.Terminal.restore fd saved);
        original_termios := None
  in
  let flush_input () =
    if input_is_tty then
      Eio_unix.Fd.use_exn "flush_input" input_eio_fd Matrix.Terminal.flush_input
  in
  let write_output buf off len =
    Eio.Flow.write stdout [ Cstruct.of_bytes ~off ~len buf ]
  in
  let read_events ~timeout ~on_event =
    let on_response = function
      | Matrix.Input.Response.Capability event ->
          Matrix.Terminal.apply_capability_event terminal event
      | Matrix.Input.Response.Clipboard _ | Matrix.Input.Response.Osc _
      | Matrix.Input.Response.Unknown _ ->
          ()
    in
    let effective_timeout =
      match timeout with
      | None -> sigwinch_poll_interval
      | Some t -> Float.min t sigwinch_poll_interval
    in
    let got =
      let wait () =
        Eio.Fiber.first
          (fun () ->
            Eio_unix.Fd.use_exn "await" input_eio_fd (fun fd ->
                Eio_unix.await_readable fd);
            `Input)
          (fun () ->
            Eio.Condition.await_no_mutex wakeup;
            `Wakeup)
      in
      match
        Eio.Time.with_timeout clock effective_timeout (fun () -> Ok (wait ()))
      with
      | Ok v -> v
      | Error `Timeout -> `Timeout
    in
    if Atomic.get winch_flag then (
      Atomic.set winch_flag false;
      let cols, rows = terminal_size () in
      on_event (Matrix.Input.Resize (cols, rows)));
    (match got with
    | `Input -> (
        try
          let n = read_stdin () in
          let now = Eio.Time.now clock in
          Matrix.Input.Parser.feed parser input_buffer 0 n ~now ~on_event
            ~on_response
        with End_of_file -> ())
    | `Wakeup | `Timeout -> ());
    let now = Eio.Time.now clock in
    Matrix.Input.Parser.drain parser ~now ~on_event ~on_response
  in
  let app =
    Matrix.attach ~mode ~raw_mode ~target_fps ~respect_alpha ~mouse_enabled
      ~mouse ~bracketed_paste ~focus_reporting ~kitty_keyboard ~exit_on_ctrl_c
      ~debug_overlay ~debug_overlay_corner ~debug_overlay_capacity
      ~frame_dump_every ?frame_dump_dir ?frame_dump_pattern ~frame_dump_hits
      ~cursor_visible ~explicit_width ~input_timeout ~resize_debounce
      ~min_tui_height ~start_idle ~write_output ~now ~wake ~terminal_size
      ~set_raw_mode ~flush_input ~read_events ~query_cursor_position
      ~cleanup:ignore ~parser ~terminal ~width ~height ~render_offset
      ~static_needs_newline ()
  in
  if signal_handlers then Matrix.install_signal_handlers ();
  (* CR: Doesn't matrix already do that for us? *)
  at_exit (fun () -> Matrix.close app);
  Eio.Switch.on_release sw (fun () -> Matrix.close app);
  install_winch_handler ();
  Matrix.Terminal.query_pixel_resolution terminal;
  app
