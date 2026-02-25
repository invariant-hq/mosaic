module Ansi = Ansi
module Glyph = Glyph
module Grid = Grid
module Input = Input
module Screen = Screen
module Terminal = Terminal
module Image = Image

(* Types *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
type mode = [ `Alt | `Primary ]
type debug_overlay_corner = Debug_overlay.corner

type control_state =
  [ `Idle
  | `Auto_started
  | `Explicit_started
  | `Explicit_paused
  | `Explicit_suspended
  | `Explicit_stopped ]

type config = {
  mode : mode;
  raw_mode : bool;
  mouse_mode : Terminal.mouse_mode option;
  bracketed_paste : bool;
  focus_reporting : bool;
  kitty_keyboard : kitty_keyboard;
  exit_on_ctrl_c : bool;
  target_fps : float option;
  explicit_width : bool;
  input_timeout : float option;
  resize_debounce : float option;
  respect_alpha : bool;
  mouse_enabled : bool;
  cursor_visible : bool;
  debug_overlay_corner : debug_overlay_corner;
  debug_overlay_capacity : int;
  min_tui_height : int;
  start_idle : bool;
}

type app = {
  terminal : Terminal.t;
  parser : Input.Parser.t;
  config : config;
  (* IO callbacks *)
  write_output : bytes -> int -> int -> unit;
  now : unit -> float;
  wake : unit -> unit;
  terminal_size : unit -> int * int;
  set_raw_mode : bool -> unit;
  flush_input : unit -> unit;
  read_events : timeout:float option -> on_event:(Input.t -> unit) -> unit;
  query_cursor_position : timeout:float -> (int * int) option;
  cleanup : unit -> unit;
  screen : Screen.t;
  render_buffer : bytes;
  mutable running : bool;
  mutable redraw_requested : bool;
  mutable width : int;
  mutable height : int;
  (* Primary mode layout *)
  mutable render_offset : int;
  mutable tui_height : int;
  mutable static_needs_newline : bool;
  mutable static_queue : (string * int) list;
  mutable needs_region_clear : bool;
  (* Resize and frame timing *)
  mutable last_resize_apply_time : float;
  mutable pending_resize : (int * int) option;
  mutable force_full_next_frame : bool;
  mutable next_frame_deadline : float option;
  (* Diagnostics *)
  mutable debug_overlay_enabled : bool;
  mutable debug_overlay_cb : Screen.t -> unit;
  mutable frame_dump_every : int;
  mutable frame_dump_dir : string option;
  mutable frame_dump_pattern : string option;
  mutable frame_dump_hits : bool;
  mutable frame_dump_counter : int;
  mutable last_frame_callback_ms : float;
  mutable closed : bool;
  mutable loop_active : bool;
  mutable control_state : control_state;
  mutable previous_control_state : control_state;
  mutable live_requests : int;
}

(* Unix I/O helpers *)

let write_all fd buf off len =
  let rec wait_writable () =
    try ignore (Unix.select [] [ fd ] [] (-1.))
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait_writable ()
  in
  let rec go off remaining =
    if remaining > 0 then
      let n =
        try Unix.write fd buf off remaining with
        | Unix.Unix_error (Unix.EINTR, _, _) -> 0
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
            wait_writable ();
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

(* Unix low-level I/O *)

let wait_readable_fds ~input_fd ~wakeup_r ~timeout =
  let fds = [ input_fd; wakeup_r ] in
  let timeout_f = Option.value ~default:(-1.) timeout in
  let readable, _, _ =
    try Unix.select fds [] [] timeout_f
    with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
  in
  readable <> []

let read_events_unix ~terminal ~parser ~input_fd ~wakeup_r ~output_fd
    ~input_buffer ~timeout ~on_event =
  let on_caps event = Terminal.apply_capability_event terminal event in
  let has_input = wait_readable_fds ~input_fd ~wakeup_r ~timeout in
  if has_input then (
    drain_wakeup_fd wakeup_r;
    if !winch_received then (
      winch_received := false;
      let cols, rows = Terminal.size output_fd in
      on_event (Input.Resize (cols, rows)));
    match Unix.read input_fd input_buffer 0 (Bytes.length input_buffer) with
    | n when n > 0 ->
        let now = Unix.gettimeofday () in
        Input.Parser.feed parser input_buffer 0 n ~now ~on_event ~on_caps
    | _ -> ()
    | exception Unix.Unix_error _ -> ());
  let now = Unix.gettimeofday () in
  Input.Parser.drain parser ~now ~on_event ~on_caps

let query_cursor_position_unix ~terminal ~parser ~input_fd ~wakeup_r
    ~input_buffer ~timeout =
  Terminal.send terminal "\027[6n";
  let result = ref None in
  let on_caps = function
    | Input.Caps.Cursor_position (row, col) -> result := Some (row, col)
    | event -> Terminal.apply_capability_event terminal event
  in
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
            Input.Parser.feed parser input_buffer 0 n ~now
              ~on_event:(fun _ -> ())
              ~on_caps
        | _ -> ()
        | exception Unix.Unix_error _ -> ());
        loop ())
  in
  loop ();
  !result

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)

(* Interpret a CPR response as (render_offset, static_needs_newline). Emits a
   CRLF to scroll if the cursor is at the bottom row. *)
let render_offset_of_cursor ~terminal ~height row col =
  let row = clamp 1 height row in
  let col = max 1 col in
  if row >= height then (
    Terminal.send terminal "\r\n";
    (height - 1, false))
  else if col = 1 then (max 0 (row - 1), true)
  else (row, true)

(* Layout *)

let apply_primary_region t ~render_offset ~resize =
  let height = max 1 t.height in
  let min_h = max 1 t.config.min_tui_height in
  let render_offset = clamp 0 (height - min_h) render_offset in
  let tui_height = max min_h (height - render_offset) in
  t.render_offset <- render_offset;
  t.tui_height <- tui_height;
  if render_offset > 1 then
    Terminal.set_scroll_region t.terminal ~top:1 ~bottom:render_offset
  else Terminal.clear_scroll_region t.terminal;
  Screen.set_row_offset t.screen render_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:tui_height

let invalidate_inline_state t =
  t.force_full_next_frame <- true;
  t.needs_region_clear <- true

(* Accessors *)

let size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, t.tui_height)

let full_size t = (t.width, t.height)

let mode t = t.config.mode
let mouse_offset t = if t.config.mode = `Primary then t.render_offset else 0
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = Terminal.capabilities t.terminal
let running t = t.running

let request_redraw t =
  if t.closed then ()
  else if t.control_state <> `Explicit_suspended then (
    t.redraw_requested <- true;
    t.wake ())

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks

let refresh_render_region t =
  match t.config.mode with
  | `Alt ->
      Terminal.clear_scroll_region t.terminal;
      t.render_offset <- 0;
      t.tui_height <- t.height;
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      apply_primary_region t ~render_offset:t.render_offset ~resize:true

(* Frame timing *)

let compute_loop_interval t =
  match t.config.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let update_loop_active t =
  let active =
    match t.control_state with
    | `Explicit_started | `Auto_started -> true
    | `Idle | `Explicit_paused | `Explicit_suspended | `Explicit_stopped ->
        false
  in
  if t.loop_active <> active then (
    t.loop_active <- active;
    if active then t.next_frame_deadline <- Some (t.now ())
    else t.next_frame_deadline <- None)

(* Static output (primary mode only) *)

let crlf = "\r\n"
let erase_entire_line = Ansi.(to_string (erase_line ~mode:`All))
let sgr_reset = Ansi.(to_string reset)

let starts_with_newline s =
  let len = String.length s in
  if len = 0 then false
  else if Char.equal (String.unsafe_get s 0) '\n' then true
  else
    len > 1
    && Char.equal (String.unsafe_get s 0) '\r'
    && Char.equal (String.unsafe_get s 1) '\n'

let ends_with_newline s =
  let len = String.length s in
  len > 0 && Char.equal (String.unsafe_get s (len - 1)) '\n'

let normalize_newlines s =
  let len = String.length s in
  let rec count i acc =
    if i >= len then acc
    else
      let c = String.unsafe_get s i in
      if Char.equal c '\n' then
        if i > 0 && Char.equal (String.unsafe_get s (i - 1)) '\r' then
          count (i + 1) acc
        else count (i + 1) (acc + 1)
      else count (i + 1) acc
  in
  let extra = count 0 0 in
  if extra = 0 then s
  else
    let bytes = Bytes.create (len + extra) in
    let rec fill i j =
      if i >= len then ()
      else
        let c = String.unsafe_get s i in
        if
          Char.equal c '\n'
          && not (i > 0 && Char.equal (String.unsafe_get s (i - 1)) '\r')
        then (
          Bytes.unsafe_set bytes j '\r';
          Bytes.unsafe_set bytes (j + 1) '\n';
          fill (i + 1) (j + 2))
        else (
          Bytes.unsafe_set bytes j c;
          fill (i + 1) (j + 1))
    in
    fill 0 0;
    Bytes.unsafe_to_string bytes

let static_write_immediate t ~rows text =
  if t.config.mode = `Alt || String.length text = 0 then ()
  else begin
    let prev_render_offset = t.render_offset in
    let prev_tui_height = t.tui_height in
    let base_render_offset =
      if t.config.mode = `Primary && t.render_offset = 0 then 1
      else t.render_offset
    in
    let terminal = t.terminal in
    let text = if t.config.raw_mode then normalize_newlines text else text in
    let needs_leading_newline =
      t.static_needs_newline && not (starts_with_newline text)
    in
    let payload_text = if needs_leading_newline then crlf ^ text else text in
    let payload_rows = rows + (if needs_leading_newline then 1 else 0) in
    let min_h = max 1 t.config.min_tui_height in
    let max_offset = max 0 (t.height - min_h) in
    let grow_by =
      min payload_rows (max 0 (max_offset - base_render_offset))
    in
    let render_offset = base_render_offset + grow_by in
    (* Clear the rows being claimed from the dynamic region before extending
       the scroll region, so stale dynamic content cannot be scrolled into
       the static area. *)
    if render_offset > base_render_offset then
      for row = base_render_offset + 1 to render_offset do
        Terminal.move_cursor terminal ~row ~col:1
          ~visible:(Terminal.cursor_visible terminal);
        Terminal.send terminal erase_entire_line
      done;
    apply_primary_region t ~render_offset ~resize:false;
    if t.render_offset <> prev_render_offset || t.tui_height <> prev_tui_height
    then (
      t.needs_region_clear <- true;
      t.force_full_next_frame <- true);
    let payload = sgr_reset ^ payload_text in
    Terminal.move_cursor terminal ~row:base_render_offset ~col:1
      ~visible:(Terminal.cursor_visible terminal);
    Terminal.send terminal payload;
    t.static_needs_newline <- not (ends_with_newline text)
  end

let flush_static_queue t =
  match t.static_queue with
  | [] -> ()
  | rev ->
      t.static_queue <- [];
      List.iter
        (fun (text, rows) -> static_write_immediate t ~rows text)
        (List.rev rev)

let effective_size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (
      match t.static_queue with
      | [] -> (t.width, t.tui_height)
      | rev ->
          let min_h = max 1 t.config.min_tui_height in
          let max_offset = max 0 (t.height - min_h) in
          let offset, _ =
            List.fold_left
              (fun (offset, snl) (text, rows) ->
                let base = if offset = 0 then 1 else offset in
                let needs_nl = snl && not (starts_with_newline text) in
                let payload_rows = rows + (if needs_nl then 1 else 0) in
                let grow = min payload_rows (max 0 (max_offset - base)) in
                (base + grow, not (ends_with_newline text)))
              (t.render_offset, t.static_needs_newline)
              (List.rev rev)
          in
          (t.width, max min_h (t.height - offset)))

let static_write t ~rows text =
  if t.config.mode = `Alt || String.length text = 0 then ()
  else (
    t.static_queue <- (text, rows) :: t.static_queue;
    request_redraw t)

let static_clear t =
  if t.config.mode = `Alt then ()
  else (
    Terminal.send t.terminal Ansi.(to_string clear_and_home);
    let cols, rows = t.terminal_size () in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    t.render_offset <- 0;
    t.tui_height <- t.height;
    t.static_queue <- [];
    t.static_needs_newline <- false;
    invalidate_inline_state t;
    refresh_render_region t;
    request_redraw t)

(* Protocol configuration *)

let apply_config t =
  refresh_capabilities t;
  let terminal = t.terminal in
  let caps = Terminal.capabilities terminal in
  t.set_raw_mode t.config.raw_mode;
  if t.config.mode = `Alt then (
    Terminal.enter_alternate_screen terminal;
    Screen.set_cursor_visible t.screen false)
  else Terminal.leave_alternate_screen terminal;
  t.force_full_next_frame <- true;
  (match t.config.mouse_mode with
  | Some mode -> Terminal.set_mouse_mode terminal mode
  | None -> Terminal.set_mouse_mode terminal `Off);
  Terminal.enable_bracketed_paste terminal t.config.bracketed_paste;
  Terminal.enable_focus_reporting terminal
    (caps.focus_tracking && t.config.focus_reporting);
  (match t.config.kitty_keyboard with
  | `Disabled -> Terminal.enable_kitty_keyboard terminal false
  | `Auto -> Terminal.enable_kitty_keyboard terminal caps.kitty_keyboard
  | `Enabled flags -> Terminal.enable_kitty_keyboard ~flags terminal true);
  Terminal.enable_modify_other_keys terminal
    (not (Terminal.kitty_keyboard_enabled terminal));
  Terminal.set_unicode_width terminal
    (if caps.unicode_width = `Unicode && not t.config.explicit_width then
       `Unicode
     else `Wcwidth);
  refresh_render_region t

(* Lifecycle: Prepare / Grid / Submit *)

let prepare t =
  refresh_capabilities t;
  t.redraw_requested <- false;
  refresh_render_region t;
  Grid.clear (Screen.grid t.screen);
  Screen.Hit_grid.clear (Screen.hit_grid t.screen)

let grid t = Screen.grid t.screen
let hits t = Screen.hit_grid t.screen

(* Recompute render_offset and tui_height for primary mode. Returns
   (render_height_limit, row_offset_changed, clipped). *)
let recompute_primary_layout t ~is_tty ~allow_scroll_up ~required_rows_hint =
  let height = max 1 t.height in
  let active_rows = max 1 (Screen.active_height t.screen) in
  let hinted_rows =
    match required_rows_hint with Some rows -> max 1 rows | None -> 0
  in
  let required_rows = max active_rows hinted_rows in
  let render_height_limit = ref None in
  let row_offset_changed = ref false in
  let clipped = ref false in
  let min_h = max 1 t.config.min_tui_height in
  let max_ui_rows = max min_h height in
  let target_rows = min required_rows max_ui_rows in
  if required_rows > max_ui_rows then (
    clipped := true;
    render_height_limit := Some max_ui_rows);
  let render_offset = clamp 0 (height - min_h) t.render_offset in
  if render_offset <> t.render_offset then (
    t.render_offset <- render_offset;
    t.tui_height <- max min_h (height - render_offset);
    if render_offset > 1 then
      Terminal.set_scroll_region t.terminal ~top:1 ~bottom:render_offset
    else Terminal.clear_scroll_region t.terminal;
    Screen.set_row_offset t.screen render_offset;
    row_offset_changed := true);
  if target_rows > t.tui_height then
    if allow_scroll_up then (
      let new_render_offset = height - target_rows in
      let delta = t.render_offset - new_render_offset in
      if delta > 0 && is_tty && t.render_offset > 1 then (
        Terminal.set_scroll_region t.terminal ~top:1 ~bottom:t.render_offset;
        Terminal.send t.terminal Ansi.(to_string (scroll_up ~n:delta)));
      t.render_offset <- new_render_offset;
      t.tui_height <- target_rows;
      if new_render_offset > 1 then
        Terminal.set_scroll_region t.terminal ~top:1 ~bottom:new_render_offset
      else Terminal.clear_scroll_region t.terminal;
      Screen.set_row_offset t.screen new_render_offset;
      row_offset_changed := true)
    else (
      clipped := true;
      render_height_limit := Some t.tui_height);
  (!render_height_limit, !row_offset_changed, !clipped)

(* Apply cursor position, style, and color after rendering. *)
let apply_cursor_state t ~(cursor : Screen.cursor_info) ~cursor_max_row =
  if cursor.has_position then
    let row = clamp 1 cursor_max_row cursor.row in
    Terminal.move_cursor t.terminal ~row:(t.render_offset + row)
      ~col:(max 1 cursor.col) ~visible:cursor.visible
  else if cursor.visible && t.config.mode = `Primary then
    Terminal.move_cursor t.terminal
      ~row:(t.render_offset + cursor_max_row)
      ~col:1 ~visible:true
  else Terminal.set_cursor_visible t.terminal cursor.visible;
  Terminal.set_cursor_style t.terminal cursor.style ~blinking:cursor.blinking;
  match cursor.color with
  | Some (r, g, b) ->
      let to_float v = Float.of_int v /. 255. in
      Terminal.set_cursor_color t.terminal ~r:(to_float r) ~g:(to_float g)
        ~b:(to_float b) ~a:1.
  | None -> Terminal.reset_cursor_color t.terminal

let submit ?primary_required_rows t =
  if not t.running then ()
  else
    let overall_start = t.now () in
    let is_tty = Terminal.tty t.terminal in
    if t.debug_overlay_enabled then t.debug_overlay_cb t.screen;
    let cursor = Screen.cursor_info t.screen in
    let caps = Terminal.capabilities t.terminal in
    let use_sync = is_tty && caps.sync in
    let send s = Terminal.send t.terminal s in
    if use_sync then send Ansi.(to_string (enable Sync_output));
    if Terminal.cursor_visible t.terminal then
      Terminal.set_cursor_visible t.terminal false;

    let pre_submit_render_offset = t.render_offset in
    let pre_submit_tui_height = t.tui_height in
    flush_static_queue t;
    let static_layout_changed =
      t.render_offset <> pre_submit_render_offset
      || t.tui_height <> pre_submit_tui_height
    in

    let render_height_limit, row_offset_changed0, clipped =
      match t.config.mode with
      | `Primary ->
          recompute_primary_layout t ~is_tty
            ~allow_scroll_up:(not static_layout_changed)
            ~required_rows_hint:primary_required_rows
      | `Alt -> (None, false, false)
    in
    let row_offset_changed =
      row_offset_changed0
      || t.render_offset <> pre_submit_render_offset
      || t.tui_height <> pre_submit_tui_height
    in

    if row_offset_changed then t.needs_region_clear <- true;

    (match t.config.mode with
    | `Primary when t.needs_region_clear ->
        if is_tty then (
          let clear_lines ~start ~count =
            for row = start to start + count - 1 do
              Terminal.move_cursor t.terminal ~row ~col:1
                ~visible:(Terminal.cursor_visible t.terminal);
              send erase_entire_line
            done
          in
          if
            (not static_layout_changed)
            && pre_submit_tui_height > 0
            && t.render_offset > pre_submit_render_offset
          then
            clear_lines
              ~start:(pre_submit_render_offset + 1)
              ~count:(t.render_offset - pre_submit_render_offset);
          if t.tui_height > 0 then (
            Terminal.move_cursor t.terminal ~row:(t.render_offset + 1) ~col:1
              ~visible:(Terminal.cursor_visible t.terminal);
            send Ansi.(to_string erase_below_cursor)));
        Screen.invalidate_presented t.screen;
        t.needs_region_clear <- false
    | _ -> ());

    let forced_full =
      let ff = t.force_full_next_frame in
      if ff then t.force_full_next_frame <- false;
      ff || row_offset_changed || clipped
    in
    let len =
      Screen.render_to_bytes ~full:forced_full ?height_limit:render_height_limit
        t.screen t.render_buffer
    in

    let stdout_start = t.now () in
    if len > 0 then t.write_output t.render_buffer 0 len;

    let cursor_max_row =
      match render_height_limit with
      | Some limit -> max 1 limit
      | None -> t.tui_height
    in
    apply_cursor_state t ~cursor ~cursor_max_row;

    if use_sync then send Ansi.(to_string (disable Sync_output));

    let stdout_end = t.now () in
    let stdout_ms = Float.max 0. ((stdout_end -. stdout_start) *. 1000.) in
    let overall_frame_ms =
      Float.max 0. ((stdout_end -. overall_start) *. 1000.)
    in

    Screen.record_runtime_metrics t.screen
      ~frame_callback_ms:t.last_frame_callback_ms ~overall_frame_ms ~stdout_ms;
    if t.frame_dump_every > 0 then (
      t.frame_dump_counter <- t.frame_dump_counter + 1;
      if t.frame_dump_counter mod t.frame_dump_every = 0 then
        Frame_dump.snapshot ?dir:t.frame_dump_dir ?pattern:t.frame_dump_pattern
          ~hits:t.frame_dump_hits t.screen)

(* Control state *)

let stop t =
  if not t.closed then (
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    update_loop_active t;
    t.redraw_requested <- false;
    t.wake ())

let request_live t =
  if t.closed then ()
  else (
    t.live_requests <- t.live_requests + 1;
    if t.control_state = `Idle && t.live_requests > 0 then (
      t.control_state <- `Auto_started;
      update_loop_active t;
      t.redraw_requested <- true;
      t.wake ()))

let drop_live t =
  t.live_requests <- max 0 (t.live_requests - 1);
  if t.control_state = `Auto_started && t.live_requests = 0 then (
    t.control_state <- `Idle;
    update_loop_active t)

let start t =
  if not t.closed then (
    t.control_state <- `Explicit_started;
    update_loop_active t;
    if not t.running then t.running <- true;
    t.redraw_requested <- true;
    t.wake ())

let pause t =
  t.control_state <- `Explicit_paused;
  update_loop_active t

let suspend t =
  t.previous_control_state <- t.control_state;
  t.control_state <- `Explicit_suspended;
  update_loop_active t;
  t.redraw_requested <- false;
  invalidate_inline_state t;
  (try Terminal.set_mouse_mode t.terminal `Off with _ -> ());
  (try Terminal.enable_bracketed_paste t.terminal false with _ -> ());
  (try Terminal.enable_focus_reporting t.terminal false with _ -> ());
  (try Terminal.enable_kitty_keyboard t.terminal false with _ -> ());
  (try Terminal.enable_modify_other_keys t.terminal false with _ -> ());
  (try t.set_raw_mode false with _ -> ());
  try t.flush_input () with _ -> ()

let resume t =
  if t.control_state <> `Explicit_suspended then ()
  else (
    (if t.config.raw_mode then try t.set_raw_mode true with _ -> ());
    (try t.flush_input () with _ -> ());
    (if t.config.mode = `Primary then
       let height = max 1 t.height in
       match t.query_cursor_position ~timeout:0.1 with
       | Some (row, col) ->
           let render_offset, static_needs_newline =
             render_offset_of_cursor ~terminal:t.terminal ~height row col
           in
           let tui_height = max 1 (height - render_offset) in
           t.render_offset <- render_offset;
           t.tui_height <- tui_height;
           t.static_needs_newline <- static_needs_newline
       | None -> ());
    invalidate_inline_state t;
    apply_config t;
    Grid.clear (Screen.grid t.screen);
    Screen.Hit_grid.clear (Screen.hit_grid t.screen);
    t.control_state <- t.previous_control_state;
    if t.control_state = `Auto_started && t.live_requests = 0 then
      t.control_state <- `Idle;
    update_loop_active t;
    if t.loop_active then (
      t.redraw_requested <- true;
      t.wake ())
    else request_redraw t)

(* Cursor *)

let set_cursor ?visible ?style t =
  Option.iter
    (fun v ->
      Terminal.set_cursor_visible t.terminal v;
      Screen.set_cursor_visible t.screen v)
    visible;
  Option.iter
    (fun style ->
      let _, blinking = Terminal.cursor_style_state t.terminal in
      Terminal.set_cursor_style t.terminal style ~blinking;
      Screen.set_cursor_style t.screen ~style ~blinking)
    style

let set_cursor_style t ~style ~blinking =
  Terminal.set_cursor_style t.terminal style ~blinking;
  Screen.set_cursor_style t.screen ~style ~blinking

let set_cursor_position t ~row ~col =
  let max_row =
    if t.config.mode = `Primary then max 1 t.tui_height else max 1 t.height
  in
  let row = clamp 1 max_row row in
  let target_row = mouse_offset t + row in
  let target_col = max 1 col in
  Screen.set_cursor_position t.screen ~row ~col:target_col;
  Terminal.move_cursor t.terminal ~row:target_row ~col:target_col
    ~visible:(Terminal.cursor_visible t.terminal)

let set_cursor_color t ~r ~g ~b ~a =
  let clamp_01 f = Float.max 0. (Float.min 1. f) in
  let r_f = clamp_01 r and g_f = clamp_01 g and b_f = clamp_01 b in
  Terminal.set_cursor_color t.terminal ~r:r_f ~g:g_f ~b:b_f ~a:(clamp_01 a);
  let to_byte f = int_of_float (Float.round (f *. 255.)) |> clamp 0 255 in
  Screen.set_cursor_color t.screen ~r:(to_byte r_f) ~g:(to_byte g_f)
    ~b:(to_byte b_f)

(* Resize *)

let apply_resize t cols rows now =
  if cols <= 0 || rows <= 0 then ()
  else if t.width = cols && t.height = rows then ()
  else (
    t.width <- cols;
    t.height <- rows;
    Terminal.query_pixel_resolution t.terminal;
    invalidate_inline_state t;
    refresh_render_region t;
    t.last_resize_apply_time <- now;
    t.pending_resize <- None;
    request_redraw t)

let handle_resize t cols rows =
  let now = t.now () in
  match t.config.resize_debounce with
  | None -> apply_resize t cols rows now
  | Some window_s ->
      if
        t.last_resize_apply_time = 0.
        || now -. t.last_resize_apply_time >= window_s
      then apply_resize t cols rows now
      else t.pending_resize <- Some (cols, rows)

let maybe_apply_pending_resize t =
  match (t.pending_resize, t.config.resize_debounce) with
  | Some (cols, rows), Some window_s ->
      let now = t.now () in
      if now -. t.last_resize_apply_time >= window_s then
        apply_resize t cols rows now
  | _ -> ()

(* Event dispatch *)

let adjust_event_for_offset t =
  if t.config.mode = `Alt then Fun.id
  else
    let offset = mouse_offset t in
    let map_y y = if y <= offset then -1 else y - offset in
    fun event ->
      match event with
      | Input.Mouse (Input.Mouse.Button_press (x, y, button, modifiers)) ->
          Input.Mouse (Input.Mouse.Button_press (x, map_y y, button, modifiers))
      | Input.Mouse (Input.Mouse.Button_release (x, y, button, modifiers)) ->
          Input.Mouse
            (Input.Mouse.Button_release (x, map_y y, button, modifiers))
      | Input.Mouse (Input.Mouse.Motion (x, y, state, modifiers)) ->
          Input.Mouse (Input.Mouse.Motion (x, map_y y, state, modifiers))
      | Input.Scroll (x, y, dir, delta, mods) ->
          Input.Scroll (x, map_y y, dir, delta, mods)
      | event -> event

let classify_event t evt =
  match evt with
  | Input.Resize (cols, rows) ->
      handle_resize t cols rows;
      Some (`Resize (cols, rows))
  | Input.Focus ->
      Terminal.restore_modes ~skip_focus:true t.terminal;
      let adjusted = adjust_event_for_offset t evt in
      Some (`Input adjusted)
  | evt ->
      let adjusted = adjust_event_for_offset t evt in
      if t.config.exit_on_ctrl_c then
        match adjusted with
        | Input.Key { key = Input.Key.Char u; modifier; _ } ->
            let cp = Uchar.to_int u in
            if
              (modifier.ctrl && (cp = Char.code 'c' || cp = Char.code 'C'))
              || cp = 0x03
            then None
            else Some (`Input adjusted)
        | _ -> Some (`Input adjusted)
      else Some (`Input adjusted)

(* Close *)

let close t =
  if t.closed then ()
  else (
    t.closed <- true;
    t.running <- false;
    t.control_state <- `Explicit_stopped;
    update_loop_active t;
    let is_tty = Terminal.tty t.terminal in
    if t.config.mode = `Primary && is_tty then (
      let height = max 1 t.height in
      let render_offset = clamp 0 (height - 1) t.render_offset in
      let start_row =
        if t.static_needs_newline then render_offset + 1
        else max 1 render_offset
      in
      Terminal.clear_scroll_region t.terminal;
      for row = start_row to height do
        Terminal.move_cursor t.terminal ~row ~col:1
          ~visible:(Terminal.cursor_visible t.terminal);
        Terminal.send t.terminal erase_entire_line
      done;
      Terminal.move_cursor t.terminal ~row:start_row ~col:1 ~visible:true);
    (* Flush pending mouse/input bytes both before and after mode teardown. This
       avoids leaking trailing SGR mouse payloads back to the shell. *)
    (try t.flush_input () with _ -> ());
    Terminal.close t.terminal;
    (try t.flush_input () with _ -> ());
    (try t.set_raw_mode false with _ -> ());
    (try t.flush_input () with _ -> ());
    try t.cleanup () with _ -> ())

(* Internal config builder *)

let make_config ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(cursor_visible = mode = `Alt) ?(explicit_width = false)
    ?(input_timeout = None) ?(resize_debounce = Some 0.1)
    ?(min_tui_height = 1) ?(start_idle = false) () =
  let effective_mouse_mode =
    if mouse_enabled then Some (Option.value ~default:`Sgr_any mouse) else None
  in
  {
    mode;
    raw_mode;
    mouse_mode = effective_mouse_mode;
    bracketed_paste;
    focus_reporting;
    kitty_keyboard;
    exit_on_ctrl_c;
    target_fps;
    explicit_width;
    input_timeout;
    resize_debounce;
    respect_alpha;
    mouse_enabled;
    cursor_visible;
    debug_overlay_corner;
    debug_overlay_capacity;
    min_tui_height;
    start_idle;
  }

(* Initialize a live app (internal) *)

let init_app (c : config) ~write_output ~now ~wake ~terminal_size ~set_raw_mode
    ~flush_input ~read_events ~query_cursor_position ~cleanup ~debug_overlay
    ~frame_dump_every ~frame_dump_dir ~frame_dump_pattern ~frame_dump_hits
    ~parser ~terminal ~width ~height ~render_offset ~static_needs_newline =
  let width = max 1 width in
  let height = max 1 height in
  let min_h = max 1 c.min_tui_height in
  let render_offset = min render_offset (max 0 (height - min_h)) in
  let tui_height = max min_h (height - render_offset) in
  let screen =
    Screen.create ~width_method:`Wcwidth ~respect_alpha:c.respect_alpha
      ~mouse_enabled:c.mouse_enabled ~cursor_visible:c.cursor_visible
      ~explicit_width:c.explicit_width ()
  in
  let render_buffer = Bytes.create (1024 * 1024 * 2) in
  let caps = Terminal.capabilities terminal in
  let width_method : Glyph.width_method =
    match caps.unicode_width with `Unicode -> `Unicode | `Wcwidth -> `Wcwidth
  in
  Screen.set_width_method screen width_method;
  Screen.apply_capabilities screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks;
  Screen.resize screen ~width ~height:tui_height;
  let t =
    {
      terminal;
      parser;
      config = c;
      write_output;
      now;
      wake;
      terminal_size;
      set_raw_mode;
      flush_input;
      read_events;
      query_cursor_position;
      cleanup;
      screen;
      render_buffer;
      running = true;
      redraw_requested = false;
      width;
      height;
      render_offset;
      tui_height;
      static_needs_newline;
      static_queue = [];
      needs_region_clear = false;
      last_resize_apply_time = 0.;
      pending_resize = None;
      force_full_next_frame = true;
      next_frame_deadline = None;
      debug_overlay_enabled = debug_overlay;
      debug_overlay_cb =
        Debug_overlay.on_frame ~corner:c.debug_overlay_corner
          ~capacity:c.debug_overlay_capacity ();
      frame_dump_every = max 0 frame_dump_every;
      frame_dump_dir;
      frame_dump_pattern;
      frame_dump_hits;
      frame_dump_counter = 0;
      last_frame_callback_ms = 0.;
      closed = false;
      loop_active = false;
      control_state =
        (if c.start_idle then `Idle
         else
           match c.target_fps with
           | Some fps when fps > 0. -> `Explicit_started
           | _ -> `Idle);
      previous_control_state = `Idle;
      live_requests = 0;
    }
  in
  apply_config t;
  t

(* Constructor: create a live app with Unix I/O *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt)
    ?(explicit_width = false) ?(input_timeout = None)
    ?(resize_debounce = Some 0.1) ?(output = `Stdout) ?(signal_handlers = true)
    ?initial_caps ?(min_tui_height = 1) ?(start_idle = false) () =
  let config =
    make_config ~mode ~raw_mode ~target_fps ~respect_alpha ~mouse_enabled ~mouse
      ~bracketed_paste ~focus_reporting ~kitty_keyboard ~exit_on_ctrl_c
      ~debug_overlay_corner ~debug_overlay_capacity ~cursor_visible
      ~explicit_width ~input_timeout ~resize_debounce ~min_tui_height
      ~start_idle ()
  in
  let output_fd = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let input_fd = Unix.stdin in
  let output_is_tty = Terminal.is_tty output_fd in
  let input_is_tty = Terminal.is_tty input_fd in
  let wakeup_r, wakeup_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock wakeup_r;
  Unix.set_nonblock wakeup_w;
  let output_fn = write_string output_fd in
  let terminal =
    Terminal.make ~output:output_fn ~tty:output_is_tty ?initial_caps ()
  in
  let parser = Input.Parser.create () in
  let original_termios = ref None in
  if input_is_tty && raw_mode then
    original_termios := Some (Terminal.set_raw input_fd);
  let input_buffer = Bytes.create 4096 in
  if input_is_tty && raw_mode then
    Terminal.probe ~timeout:0.5
      ~on_event:(fun _ -> ())
      ~read_into:(fun buf off len ->
        try Unix.read input_fd buf off len with Unix.Unix_error _ -> 0)
      ~wait_readable:(fun ~timeout ->
        let readable, _, _ =
          try Unix.select [ input_fd ] [] [] timeout
          with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
        in
        readable <> [])
      ~parser terminal;
  let cols, rows = Terminal.size output_fd in
  let width = max 1 cols in
  let height = max 1 rows in
  let render_offset, static_needs_newline =
    if mode = `Primary && input_is_tty && raw_mode then
      match
        query_cursor_position_unix ~terminal ~parser ~input_fd ~wakeup_r
          ~input_buffer ~timeout:0.1
      with
      | Some (row, col) -> render_offset_of_cursor ~terminal ~height row col
      | None -> (0, true)
    else if mode = `Primary then (0, true)
    else (0, false)
  in
  let shutdown_fn_ref = ref None in
  let app =
    init_app config ~write_output:(write_all output_fd) ~now:Unix.gettimeofday
      ~wake:(fun () -> wake_fd wakeup_w)
      ~terminal_size:(fun () -> Terminal.size output_fd)
      ~set_raw_mode:(fun enabled ->
        if enabled then (
          match !original_termios with
          | Some _ -> ()
          | None ->
              if input_is_tty then
                original_termios := Some (Terminal.set_raw input_fd))
        else
          match !original_termios with
          | Some saved ->
              Terminal.restore input_fd saved;
              original_termios := None
          | None -> ())
      ~flush_input:(fun () ->
        if input_is_tty then Terminal.flush_input input_fd)
      ~read_events:(fun ~timeout ~on_event ->
        read_events_unix ~terminal ~parser ~input_fd ~wakeup_r ~output_fd
          ~input_buffer ~timeout ~on_event)
      ~query_cursor_position:(fun ~timeout ->
        query_cursor_position_unix ~terminal ~parser ~input_fd ~wakeup_r
          ~input_buffer ~timeout)
      ~cleanup:(fun () ->
        (match !shutdown_fn_ref with
        | Some fn -> deregister_shutdown_handler fn
        | None -> ());
        (try Unix.close wakeup_r with _ -> ());
        try Unix.close wakeup_w with _ -> ())
      ~debug_overlay ~frame_dump_every ~frame_dump_dir ~frame_dump_pattern
      ~frame_dump_hits ~parser ~terminal ~width ~height ~render_offset
      ~static_needs_newline
  in
  let shutdown_fn () = close app in
  shutdown_fn_ref := Some shutdown_fn;
  if signal_handlers then install_signal_handlers ();
  register_shutdown_handler shutdown_fn;
  install_winch_handler wakeup_w;
  Terminal.query_pixel_resolution terminal;
  app

(* Attach: wire custom I/O for testing *)

let attach ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(respect_alpha = false) ?(mouse_enabled = true) ?(mouse = None)
    ?(bracketed_paste = true) ?(focus_reporting = true)
    ?(kitty_keyboard = `Auto) ?(exit_on_ctrl_c = true) ?(debug_overlay = false)
    ?(debug_overlay_corner = `Bottom_right) ?(debug_overlay_capacity = 120)
    ?(frame_dump_every = 0) ?frame_dump_dir ?frame_dump_pattern
    ?(frame_dump_hits = false) ?(cursor_visible = mode = `Alt)
    ?(explicit_width = false) ?(input_timeout = None)
    ?(resize_debounce = Some 0.1) ?(min_tui_height = 1) ?(start_idle = false)
    ~write_output ~now ~wake ~terminal_size ~set_raw_mode ~flush_input
    ~read_events ~query_cursor_position ~cleanup ~parser ~terminal ~width ~height
    ?(render_offset = 0) ?(static_needs_newline = false) () =
  let config =
    make_config ~mode ~raw_mode ~target_fps ~respect_alpha ~mouse_enabled ~mouse
      ~bracketed_paste ~focus_reporting ~kitty_keyboard ~exit_on_ctrl_c
      ~debug_overlay_corner ~debug_overlay_capacity ~cursor_visible
      ~explicit_width ~input_timeout ~resize_debounce ~min_tui_height
      ~start_idle ()
  in
  init_app config ~write_output ~now ~wake ~terminal_size ~set_raw_mode
    ~flush_input ~read_events ~query_cursor_position ~cleanup ~debug_overlay
    ~frame_dump_every ~frame_dump_dir ~frame_dump_pattern ~frame_dump_hits
    ~parser ~terminal ~width ~height ~render_offset ~static_needs_newline

(* Event loop helpers *)

let min_timeout a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some x, Some y -> Some (Float.min x y)

let should_honor_immediate_redraw t = t.redraw_requested && not t.loop_active

let compute_timeout t ~now =
  let pending_timeout =
    match (t.pending_resize, t.config.resize_debounce) with
    | Some _, Some window_s ->
        let remaining = t.last_resize_apply_time +. window_s -. now in
        if remaining > 0. then Some remaining else Some 0.
    | _ -> None
  in
  let deadline_timeout =
    match t.next_frame_deadline with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let parser_timeout =
    match Input.Parser.deadline t.parser with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let immediate = if should_honor_immediate_redraw t then Some 0. else None in
  min_timeout immediate
    (min_timeout
       (min_timeout deadline_timeout t.config.input_timeout)
       (min_timeout pending_timeout parser_timeout))

let should_render_now t ~now =
  should_honor_immediate_redraw t
  || t.loop_active
     && match t.next_frame_deadline with Some dl -> now >= dl | None -> false

(* Event loop *)

let run ?on_frame ?on_input ?on_resize ?primary_required_rows ~on_render t =
  if not t.running then t.running <- true;
  (match t.control_state with
  | `Auto_started when t.live_requests = 0 ->
      t.control_state <- `Idle;
      update_loop_active t
  | _ -> update_loop_active t);
  if
    t.control_state <> `Explicit_suspended
    && t.control_state <> `Explicit_stopped
  then (
    t.redraw_requested <- true;
    t.wake ());
  Option.iter
    (fun f ->
      let cols, rows = size t in
      f t ~cols ~rows)
    on_resize;

  let render_cycle ~now ~last_time =
    Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
    prepare t;
    let user_start = t.now () in
    on_render t;
    let user_end = t.now () in
    t.last_frame_callback_ms <- Float.max 0. ((user_end -. user_start) *. 1000.);
    let required_rows_hint =
      match primary_required_rows with
      | Some f -> (
          match f t with Some rows when rows > 0 -> Some rows | _ -> None)
      | None -> None
    in
    submit ?primary_required_rows:required_rows_hint t;
    user_end
  in

  let handle_event evt =
    if not t.running then ()
    else
      match classify_event t evt with
      | None -> close t
      | Some (`Resize (cols, rows)) ->
          Option.iter (fun f -> f t ~cols ~rows) on_resize;
          Option.iter (fun f -> f t (Input.Resize (cols, rows))) on_input;
          request_redraw t
      | Some (`Input event) ->
          Option.iter (fun f -> f t event) on_input;
          request_redraw t
  in

  let rec loop last_time =
    if not (running t) then ()
    else (
      maybe_apply_pending_resize t;
      let now = t.now () in
      if should_render_now t ~now then (
        let frame_interval = compute_loop_interval t in
        let last_time = render_cycle ~now ~last_time in
        (* Schedule next frame deadline after rendering: delay =
           target_frame_time - frame_elapsed. *)
        let render_end = t.now () in
        t.next_frame_deadline <-
          (match frame_interval with
          | Some iv ->
              let elapsed = render_end -. now in
              let delay = Float.max 0. (iv -. elapsed) in
              Some (render_end +. delay)
          | None -> None);
        let timeout = compute_timeout t ~now:render_end in
        t.read_events ~timeout ~on_event:handle_event;
        loop last_time)
      else
        let timeout = compute_timeout t ~now in
        t.read_events ~timeout ~on_event:handle_event;
        loop last_time)
  in
  let start_time = t.now () in
  Fun.protect
    (fun () -> loop start_time)
    ~finally:(fun () -> if not t.closed then close t)

(* Diagnostics *)

let set_debug_overlay ?corner t ~enabled =
  let previous = t.debug_overlay_enabled in
  t.debug_overlay_enabled <- enabled;
  Option.iter
    (fun c ->
      t.debug_overlay_cb <-
        Debug_overlay.on_frame ~corner:c
          ~capacity:t.config.debug_overlay_capacity ())
    corner;
  if previous <> enabled || Option.is_some corner then request_redraw t

let toggle_debug_overlay ?corner t =
  set_debug_overlay ?corner t ~enabled:(not t.debug_overlay_enabled)

let configure_frame_dump ?every ?dir ?pattern ?hits t =
  Option.iter
    (fun ev ->
      t.frame_dump_every <- max 0 ev;
      t.frame_dump_counter <- 0)
    every;
  Option.iter (fun d -> t.frame_dump_dir <- Some d) dir;
  Option.iter (fun p -> t.frame_dump_pattern <- Some p) pattern;
  Option.iter (fun h -> t.frame_dump_hits <- h) hits

let dump_frame ?hits ?dir ?pattern t =
  let or_default a b = match a with Some _ -> a | None -> b in
  Frame_dump.snapshot
    ?dir:(or_default dir t.frame_dump_dir)
    ?pattern:(or_default pattern t.frame_dump_pattern)
    ~hits:(Option.value ~default:t.frame_dump_hits hits)
    t.screen
