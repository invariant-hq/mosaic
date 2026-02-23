(* I/O helpers *)

let write_all fd buf off len =
  let rec go off remaining =
    if remaining > 0 then
      let n =
        try Unix.write fd buf off remaining with
        | Unix.Unix_error (Unix.EINTR, _, _) -> 0
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
            ignore (Unix.select [] [ fd ] [] (-1.));
            0
      in
      go (off + n) (remaining - n)
  in
  go off len

let write_string fd s =
  write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)

let wake_byte = Bytes.of_string "w"

let wake_fd fd =
  let (_ : int) =
    try Unix.write fd wake_byte 0 1 with Unix.Unix_error _ -> 0
  in
  ()

let drain_wakeup_fd fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | n when n > 0 -> go ()
    | _ -> ()
    | exception Unix.Unix_error _ -> ()
  in
  go ()

(* Shutdown handler registry *)

let shutdown_handlers : (unit -> unit) list ref = ref []
let shutdown_triggered = ref false

let run_shutdown_handlers () =
  if !shutdown_triggered then ()
  else (
    shutdown_triggered := true;
    List.iter (fun f -> try f () with _ -> ()) !shutdown_handlers)

let register_shutdown_handler fn = shutdown_handlers := fn :: !shutdown_handlers

let deregister_shutdown_handler fn =
  shutdown_handlers := List.filter (fun f -> f != fn) !shutdown_handlers

let shutdown_signal_handler signum =
  run_shutdown_handlers ();
  exit (128 + signum)

let signal_handlers_installed = ref false

let try_set_signal sig_num handler =
  try Sys.set_signal sig_num handler with Invalid_argument _ -> ()

let install_signal_handlers () =
  if not !signal_handlers_installed then (
    signal_handlers_installed := true;
    try_set_signal Sys.sigterm (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigint (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigquit (Sys.Signal_handle shutdown_signal_handler);
    try_set_signal Sys.sigabrt (Sys.Signal_handle shutdown_signal_handler);
    Printexc.set_uncaught_exception_handler (fun exn ->
        prerr_endline (Printexc.to_string exn);
        run_shutdown_handlers ();
        exit 1))

let () = at_exit run_shutdown_handlers

(* SIGWINCH: global flag + wakeup pipe fd *)

let winch_received = ref false

let install_winch_handler wakeup_w =
  let handler _ =
    winch_received := true;
    wake_fd wakeup_w
  in
  try_set_signal Sys.sigwinch (Sys.Signal_handle handler)

(* Low-level I/O *)

let wait_readable_fds ~input_fd ~wakeup_r ~timeout =
  let fds = [ input_fd; wakeup_r ] in
  let timeout_f = Option.value ~default:(-1.) timeout in
  let readable, _, _ =
    try Unix.select fds [] [] timeout_f
    with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
  in
  readable <> []

let read_events ~terminal ~input_fd ~wakeup_r ~output_fd ~input_buffer ~timeout
    ~on_event =
  let parser = Matrix.Terminal.parser terminal in
  let on_caps event = Matrix.Terminal.apply_capability_event terminal event in
  let has_input = wait_readable_fds ~input_fd ~wakeup_r ~timeout in
  if has_input then (
    drain_wakeup_fd wakeup_r;
    if !winch_received then (
      winch_received := false;
      let cols, rows = Matrix.Terminal.size output_fd in
      on_event (Matrix.Input.Resize (cols, rows)));
    match Unix.read input_fd input_buffer 0 (Bytes.length input_buffer) with
    | n when n > 0 ->
        let now = Unix.gettimeofday () in
        Matrix.Input.Parser.feed parser input_buffer 0 n ~now ~on_event ~on_caps
    | _ -> ()
    | exception Unix.Unix_error _ -> ());
  let now = Unix.gettimeofday () in
  Matrix.Input.Parser.drain parser ~now ~on_event ~on_caps

let query_cursor_position ~terminal ~input_fd ~wakeup_r ~input_buffer ~timeout =
  Matrix.Terminal.send terminal "\027[6n";
  let result = ref None in
  let on_caps = function
    | Matrix.Input.Caps.Cursor_position (row, col) -> result := Some (row, col)
    | event -> Matrix.Terminal.apply_capability_event terminal event
  in
  let parser = Matrix.Terminal.parser terminal in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Option.is_some !result then ()
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then ()
      else if wait_readable_fds ~input_fd ~wakeup_r ~timeout:(Some remaining)
      then (
        drain_wakeup_fd wakeup_r;
        (match
           Unix.read input_fd input_buffer 0 (Bytes.length input_buffer)
         with
        | n when n > 0 ->
            let now = Unix.gettimeofday () in
            Matrix.Input.Parser.feed parser input_buffer 0 n ~now
              ~on_event:(fun _ -> ())
              ~on_caps
        | _ -> ()
        | exception Unix.Unix_error _ -> ());
        loop ())
  in
  loop ();
  !result

(* Create: wire Unix I/O to a config, return a live app *)

let create ?(output = `Stdout) ?(signal_handlers = true) ?initial_caps
    (config : Matrix.config) =
  let output_fd = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let input_fd = Unix.stdin in
  let output_is_tty = Matrix.Terminal.is_tty output_fd in
  let input_is_tty = Matrix.Terminal.is_tty input_fd in
  let wakeup_r, wakeup_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock wakeup_r;
  Unix.set_nonblock wakeup_w;
  let output_fn = write_string output_fd in
  let terminal =
    Matrix.Terminal.make ~output:output_fn ~tty:output_is_tty ?initial_caps ()
  in
  let original_termios = ref None in
  if input_is_tty then
    original_termios := Some (Matrix.Terminal.set_raw input_fd);
  let input_buffer = Bytes.create 4096 in
  if input_is_tty then
    Matrix.Terminal.probe ~timeout:0.5
      ~on_event:(fun _ -> ())
      ~read_into:(fun buf off len ->
        try Unix.read input_fd buf off len with Unix.Unix_error _ -> 0)
      ~wait_readable:(fun ~timeout ->
        let readable, _, _ =
          try Unix.select [ input_fd ] [] [] timeout
          with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
        in
        readable <> [])
      terminal;
  let cols, rows = Matrix.Terminal.size output_fd in
  let width = max 1 cols in
  let height = max 1 rows in
  let render_offset, static_needs_newline =
    if Matrix.mode_of_config config = `Primary && input_is_tty then
      match
        query_cursor_position ~terminal ~input_fd ~wakeup_r ~input_buffer
          ~timeout:0.1
      with
      | Some (row, col) ->
          Matrix.render_offset_of_cursor ~terminal ~height row col
      | None -> (0, true)
    else (0, false)
  in
  let shutdown_fn_ref = ref None in
  let io : Matrix.io =
    {
      write_output = write_all output_fd;
      now = Unix.gettimeofday;
      wake = (fun () -> wake_fd wakeup_w);
      terminal_size = (fun () -> Matrix.Terminal.size output_fd);
      set_raw_mode =
        (fun enabled ->
          if enabled then (
            match !original_termios with
            | Some _ -> ()
            | None ->
                if input_is_tty then
                  original_termios := Some (Matrix.Terminal.set_raw input_fd))
          else
            match !original_termios with
            | Some saved ->
                Matrix.Terminal.restore input_fd saved;
                original_termios := None
            | None -> ());
      flush_input =
        (fun () -> if input_is_tty then Matrix.Terminal.flush_input input_fd);
      read_events =
        (fun ~timeout ~on_event ->
          read_events ~terminal ~input_fd ~wakeup_r ~output_fd ~input_buffer
            ~timeout ~on_event);
      query_cursor_position =
        (fun ~timeout ->
          query_cursor_position ~terminal ~input_fd ~wakeup_r ~input_buffer
            ~timeout);
      cleanup =
        (fun () ->
          (match !shutdown_fn_ref with
          | Some fn -> deregister_shutdown_handler fn
          | None -> ());
          (try Unix.close wakeup_r with _ -> ());
          try Unix.close wakeup_w with _ -> ());
    }
  in
  let app =
    Matrix.attach config ~io ~terminal ~width ~height ~render_offset
      ~static_needs_newline ()
  in
  let shutdown_fn () = Matrix.close app in
  shutdown_fn_ref := Some shutdown_fn;
  if signal_handlers then install_signal_handlers ();
  register_shutdown_handler shutdown_fn;
  install_winch_handler wakeup_w;
  Matrix.Terminal.query_pixel_resolution terminal;
  app

(* Entry point *)

let run ?on_frame ?on_input ?on_resize ~on_render ?output ?signal_handlers
    ?initial_caps (config : Matrix.config) =
  let app = create ?output ?signal_handlers ?initial_caps config in
  (* Fire on_resize with initial dimensions so callers can initialize state *)
  Option.iter
    (fun f ->
      let cols, rows = Matrix.size app in
      f app ~cols ~rows)
    on_resize;
  Fun.protect
    (fun () -> Matrix.run ?on_frame ?on_input ?on_resize ~on_render app)
    ~finally:(fun () ->
      if not (Matrix.running app) then () else Matrix.close app)
