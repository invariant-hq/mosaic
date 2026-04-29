open Windtrap

type fake_state = {
  mutable now_s : float;
  mutable read_calls : int;
  mutable cleanup_calls : int;
  mutable raw_restore_calls : int;
  mutable flush_input_calls : int;
  mutable wake_calls : int;
  mutable pending_events : Matrix.Input.t list;
  output : Buffer.t;
  terminal_output : Buffer.t;
}

let make_state events =
  {
    now_s = 0.;
    read_calls = 0;
    cleanup_calls = 0;
    raw_restore_calls = 0;
    flush_input_calls = 0;
    wake_calls = 0;
    pending_events = events;
    output = Buffer.create 256;
    terminal_output = Buffer.create 256;
  }

let output state = Buffer.contents state.output

let contains_substring needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec matches_at i j =
    j = needle_len
    || i + j < haystack_len
       && Char.equal
            (String.unsafe_get haystack (i + j))
            (String.unsafe_get needle j)
       && matches_at i (j + 1)
  in
  let rec loop i =
    i + needle_len <= haystack_len && (matches_at i 0 || loop (i + 1))
  in
  needle_len = 0 || loop 0

let substring_index needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec matches_at i j =
    j = needle_len
    || i + j < haystack_len
       && Char.equal
            (String.unsafe_get haystack (i + j))
            (String.unsafe_get needle j)
       && matches_at i (j + 1)
  in
  let rec loop i =
    if i + needle_len > haystack_len then None
    else if matches_at i 0 then Some i
    else loop (i + 1)
  in
  if needle_len = 0 then Some 0 else loop 0

let count_substring needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec matches_at i j =
    j = needle_len
    || i + j < haystack_len
       && Char.equal
            (String.unsafe_get haystack (i + j))
            (String.unsafe_get needle j)
       && matches_at i (j + 1)
  in
  let rec loop i count =
    if needle_len = 0 || i + needle_len > haystack_len then count
    else if matches_at i 0 then loop (i + needle_len) (count + 1)
    else loop (i + 1) count
  in
  loop 0 0

let is_before ~first ~second s =
  match (substring_index first s, substring_index second s) with
  | Some i, Some j -> i < j
  | _ -> false

let make_app ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(input_timeout = Some 0.) ?(terminal_tty = false) ?(advance_now = true)
    ?(sleep_until_timeout = false) ?(read_quantum_s = 0.0001)
    ?(emit_event_each_read = None) ?(events = []) ?stop_after_reads
    ?render_offset ?static_needs_newline ?(min_tui_height = 1)
    ?(resize_debounce = Some 0.)
    ?(query_cursor_position = fun ~timeout:_ -> None) () =
  let state = make_state events in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Buffer.add_string state.terminal_output s)
      ~tty:terminal_tty ()
  in
  let parser = Matrix.Input.Parser.create () in
  let app_ref = ref None in
  let now () =
    if advance_now then state.now_s <- state.now_s +. 0.001;
    state.now_s
  in
  let read_events ~timeout ~on_event =
    state.read_calls <- state.read_calls + 1;
    let dt =
      if sleep_until_timeout then
        match timeout with
        | Some t when t > read_quantum_s -> t
        | _ -> read_quantum_s
      else read_quantum_s
    in
    state.now_s <- state.now_s +. dt;
    Option.iter on_event emit_event_each_read;
    let events = state.pending_events in
    state.pending_events <- [];
    List.iter on_event events;
    match (stop_after_reads, !app_ref) with
    | Some max_reads, Some app when state.read_calls >= max_reads ->
        Matrix.stop app
    | _ -> ()
  in
  let app =
    Matrix.attach ~mode ~raw_mode ~target_fps ~input_timeout ~min_tui_height
      ~resize_debounce ?render_offset ?static_needs_newline
      ~write_output:(fun buf off len ->
        Buffer.add_string state.output (Bytes.sub_string buf off len))
      ~now
      ~wake:(fun () -> state.wake_calls <- state.wake_calls + 1)
      ~terminal_size:(fun () -> (80, 24))
      ~set_raw_mode:(fun enabled ->
        if not enabled then
          state.raw_restore_calls <- state.raw_restore_calls + 1)
      ~flush_input:(fun () ->
        state.flush_input_calls <- state.flush_input_calls + 1)
      ~read_events ~query_cursor_position
      ~cleanup:(fun () -> state.cleanup_calls <- state.cleanup_calls + 1)
      ~parser ~terminal ~width:80 ~height:24 ()
  in
  app_ref := Some app;
  (app, state)

let mouse_press x y =
  Matrix.Input.Mouse
    (Matrix.Input.Mouse.Button_press
       (x, y, Matrix.Input.Mouse.Left, Matrix.Input.Modifier.none))

let mouse_release x y =
  Matrix.Input.Mouse
    (Matrix.Input.Mouse.Button_release
       (x, y, Matrix.Input.Mouse.Left, Matrix.Input.Modifier.none))

let mouse_motion x y =
  let buttons =
    { Matrix.Input.Mouse.left = true; middle = false; right = false }
  in
  Matrix.Input.Mouse
    (Matrix.Input.Mouse.Motion (x, y, buttons, Matrix.Input.Modifier.none))

let mouse_scroll x y =
  Matrix.Input.Scroll
    (x, y, Matrix.Input.Mouse.Scroll_down, 1, Matrix.Input.Modifier.none)

let set_sync_capable app =
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = true }

let test_request_redraw_while_paused () =
  let app, _state =
    make_app ~target_fps:(Some 30.) ~input_timeout:(Some 0.)
      ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      match !frames with
      | 1 ->
          Matrix.pause app;
          Matrix.request_redraw app
      | 2 -> Matrix.stop app
      | _ -> Matrix.stop app);
  equal ~msg:"redraw triggers one-shot frame while paused" int 2 !frames

let test_idle_does_not_force_live_loop () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:1 ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun _app -> incr frames);
  equal ~msg:"idle renders only the initial frame" int 1 !frames

let test_run_closes_on_exception () =
  let app, state = make_app () in
  let raised =
    try
      Matrix.run app ~on_render:(fun _ -> failwith "boom");
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  is_true ~msg:"exception propagated" raised;
  equal ~msg:"cleanup called on exception" int 1 state.cleanup_calls

let test_close_restores_raw_mode_if_terminal_close_raises () =
  let fail_output = ref false in
  let terminal =
    Matrix.Terminal.make ~tty:true
      ~output:(fun _ -> if !fail_output then failwith "closed output")
      ()
  in
  let parser = Matrix.Input.Parser.create () in
  let cleanup_calls = ref 0 in
  let raw_restore_calls = ref 0 in
  let app =
    Matrix.attach ~mode:`Primary ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~focus_reporting:false ~kitty_keyboard:`Disabled
      ~target_fps:None ~input_timeout:(Some 0.)
      ~write_output:(fun _buf _off _len -> ())
      ~now:(fun () -> 0.)
      ~wake:(fun () -> ())
      ~terminal_size:(fun () -> (80, 24))
      ~set_raw_mode:(fun enabled -> if not enabled then incr raw_restore_calls)
      ~flush_input:(fun () -> ())
      ~read_events:(fun ~timeout:_ ~on_event:_ -> ())
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:(fun () -> incr cleanup_calls)
      ~parser ~terminal ~width:80 ~height:24 ()
  in
  fail_output := true;
  Matrix.close app;
  equal ~msg:"raw mode restored" int 1 !raw_restore_calls;
  equal ~msg:"cleanup called" int 1 !cleanup_calls

let test_focus_restore_runs_only_after_blur_once () =
  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Focus ] ()
  in
  let first_focus_output = ref (-1) in
  Buffer.clear state.terminal_output;
  Matrix.run app
    ~on_input:(fun app _event ->
      first_focus_output := Buffer.length state.terminal_output;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  equal ~msg:"focus without prior blur does not restore terminal modes" int 0
    !first_focus_output;

  let app, state =
    make_app ~terminal_tty:true ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Blur; Matrix.Input.Focus; Matrix.Input.Focus ]
      ()
  in
  let seen = ref [] in
  Buffer.clear state.terminal_output;
  Matrix.run app
    ~on_input:(fun app event ->
      seen := (event, Buffer.length state.terminal_output) :: !seen;
      match List.length !seen with 3 -> Matrix.stop app | _ -> ())
    ~on_render:(fun _app -> ());
  match List.rev !seen with
  | [
   (Matrix.Input.Blur, after_blur);
   (Matrix.Input.Focus, after_first_focus);
   (Matrix.Input.Focus, after_second_focus);
  ] ->
      equal ~msg:"blur itself does not restore terminal modes" int 0 after_blur;
      is_true ~msg:"first focus after blur restores terminal modes"
        (after_first_focus > after_blur);
      equal ~msg:"second focus does not restore modes again" int
        after_first_focus after_second_focus
  | _ -> fail "expected blur, focus, focus input trace"

let test_resume_reanchors_primary_from_bottom_cursor () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~target_fps:None
      ~input_timeout:(Some 0.)
      ~query_cursor_position:(fun ~timeout:_ -> Some (24, 1))
      ()
  in
  Matrix.suspend app;
  Buffer.clear state.terminal_output;
  Matrix.resume app;
  is_true ~msg:"bottom-row resume writes newline before reanchoring"
    (contains_substring "\r\n" (Buffer.contents state.terminal_output));
  equal ~msg:"bottom-row cursor anchors one-row live viewport" (pair int int)
    (80, 1) (Matrix.size app)

let test_target_fps_respected_without_input_storm () =
  let app, state =
    make_app ~target_fps:(Some 13.) ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:true ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"13fps target should stay near cadence without input"
    (!frames >= 8 && !frames <= 20)

let test_target_fps_respected_with_input_storm () =
  let app, state =
    make_app ~target_fps:(Some 13.) ~input_timeout:None ~advance_now:false
      ~sleep_until_timeout:false ~emit_event_each_read:(Some Matrix.Input.Focus)
      ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"input events must not bypass active fps cap during input storms"
    (!frames >= 8 && !frames <= 20)

let test_unchanged_submit_emits_no_bytes () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  equal ~msg:"unchanged frame skips terminal write" int 0
    (String.length (output state))

let test_cursor_only_submit_emits_output () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"stable";
  Matrix.set_cursor_position app ~row:1 ~col:3;
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"cursor-only frame emits terminal output"
    (String.length output > 0);
  is_true ~msg:"cursor-only frame moves the cursor"
    (contains_substring "\027[1;3H" output)

let test_initial_resize_fires_before_first_render () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:5000 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun _app ~cols ~rows ->
      events := ("resize", cols, rows) :: !events)
    ~on_render:(fun app ->
      events := ("render", 0, 0) :: !events;
      Matrix.stop app);
  match List.rev !events with
  | [ ("resize", 80, 24); ("render", 0, 0) ] -> ()
  | got ->
      failf "expected resize(80,24) before first render, got: %d event(s)"
        (List.length got)

let test_resize_zero_dimensions_are_ignored () =
  let app, _state =
    make_app ~resize_debounce:None ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Resize (0, 0) ]
      ~stop_after_reads:1 ()
  in
  let events = ref [] in
  Matrix.run app
    ~on_resize:(fun app ~cols ~rows ->
      events := (cols, rows, Matrix.full_size app, Matrix.size app) :: !events)
    ~on_render:(fun _app -> ());
  match List.rev !events with
  | [ (80, 24, (80, 24), (80, 24)); (0, 0, (80, 24), (80, 24)) ] -> ()
  | got -> failf "unexpected resize trace: %d event(s)" (List.length got)

let test_resize_clamps_primary_render_offset () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:20 ~resize_debounce:None
      ~target_fps:None ~input_timeout:(Some 0.)
      ~events:[ Matrix.Input.Resize (80, 12) ]
      ~stop_after_reads:1 ()
  in
  Matrix.run app ~on_render:(fun _app -> ());
  equal ~msg:"terminal size follows resize event" (pair int int) (80, 12)
    (Matrix.full_size app);
  equal ~msg:"primary offset clamps to preserve minimum live height"
    (pair int int) (80, 1) (Matrix.size app)

let test_resize_debounce_applies_latest_pending_resize () =
  let app, _state =
    make_app ~resize_debounce:(Some 0.5) ~advance_now:false
      ~sleep_until_timeout:true ~target_fps:None ~input_timeout:None
      ~events:[ Matrix.Input.Resize (100, 30); Matrix.Input.Resize (120, 40) ]
      ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if Matrix.full_size app = (120, 40) || !frames > 5 then Matrix.stop app);
  equal ~msg:"debounced resize applies latest pending dimensions" (pair int int)
    (120, 40) (Matrix.full_size app)

let test_primary_required_rows_expands_primary_region () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:22 ~target_fps:(Some 30.)
      ~input_timeout:(Some 0.) ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run app
    ~primary_required_rows:(fun _ -> Some 8)
    ~on_render:(fun app ->
      incr frames;
      if !frames >= 3 then Matrix.stop app);
  let _w, h = Matrix.size app in
  equal ~msg:"primary required rows hint grows dynamic viewport" int 8 h

let test_primary_required_rows_ignored_in_alt_mode () =
  let app, _state =
    make_app ~mode:`Alt ~target_fps:(Some 30.) ~input_timeout:(Some 0.)
      ~stop_after_reads:5000 ()
  in
  Matrix.run app
    ~primary_required_rows:(fun _ -> Some 3)
    ~on_render:(fun app -> Matrix.stop app);
  let _w, h = Matrix.size app in
  equal ~msg:"alt mode size remains full terminal height" int 24 h

let test_primary_effective_size_tracks_pending_static () =
  let app, _state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"initial primary height" int 24 (snd (Matrix.size app));
  Matrix.static_write app ~rows:2 "alpha\nbeta\n";
  let effective = Matrix.effective_size app in
  equal ~msg:"pending static write reduces effective height" (pair int int)
    (80, 21) effective;
  Matrix.submit app;
  equal ~msg:"submit applies pending effective size" (pair int int) effective
    (Matrix.size app)

let test_static_writes_are_flushed_fifo () =
  let app, state =
    make_app ~mode:`Primary ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "first\n";
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  let output = output state in
  match (substring_index "first" output, substring_index "second" output) with
  | Some first, Some second ->
      is_true ~msg:"first static write appears before second" (first < second)
  | _ -> fail "expected both static writes in frame output"

let test_static_write_normalizes_lf_in_raw_mode () =
  let app, state =
    make_app ~mode:`Primary ~raw_mode:true ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "raw\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"raw mode normalizes lone LF to CRLF"
    (contains_substring "raw\r\n" output);
  is_false ~msg:"raw mode does not emit lone LF"
    (contains_substring "raw\n" output)

let test_static_write_preserves_lf_outside_raw_mode () =
  let app, state =
    make_app ~mode:`Primary ~raw_mode:false ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:1 "plain\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"non-raw mode preserves LF" (contains_substring "plain\n" output);
  is_false ~msg:"non-raw mode does not normalize LF to CRLF"
    (contains_substring "plain\r\n" output)

let test_static_write_tracks_mid_line_continuation () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  is_true ~msg:"mid-line static output anchors next write on a fresh row"
    (contains_substring "\r\nsecond" (output state));

  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first\n";
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  is_false ~msg:"line-ended static output does not add an extra leading CRLF"
    (contains_substring "\r\nsecond" (output state))

let test_pinned_static_write_uses_bounded_scroll_region () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"pinned append sets upper-pane scroll region"
    (contains_substring "\027[1;23r" output);
  is_true ~msg:"pinned append resets scroll region"
    (contains_substring "\027[r" output);
  is_true ~msg:"scroll region is set before payload"
    (is_before ~first:"\027[1;23r" ~second:"pinned" output);
  is_true ~msg:"scroll region reset follows payload"
    (is_before ~first:"pinned" ~second:"\027[r" output);
  is_false ~msg:"pinned append does not use broad erase"
    (contains_substring "\027[J" output)

let test_pinned_static_write_resets_scroll_region_before_live_render () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"static payload appears before live render"
    (is_before ~first:"pinned" ~second:"live" output);
  is_true ~msg:"scroll region resets before live render"
    (is_before ~first:"\027[r" ~second:"live" output)

let test_multiline_static_write_uses_scroll_region_when_it_reaches_pin () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:22 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:2 "line-a\nline-b\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"multiline append reaches pinned live viewport" (pair int int)
    (80, 1) (Matrix.size app);
  is_true ~msg:"pinning append sets bounded scroll region"
    (contains_substring "\027[1;23r" output);
  is_true ~msg:"pinning append resets bounded scroll region"
    (contains_substring "\027[r" output);
  is_true ~msg:"bounded scroll region is active before payload"
    (is_before ~first:"\027[1;23r" ~second:"line-a" output);
  is_true ~msg:"bounded scroll region resets before live repaint"
    (is_before ~first:"\027[r" ~second:"live" output)

let test_unpinned_static_write_settles_without_scroll_region () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:1 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "settle\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  equal ~msg:"static output moves live region downward" (pair int int) (80, 22)
    (Matrix.size app);
  is_true ~msg:"static payload appears before live repaint"
    (is_before ~first:"settle" ~second:"live" output);
  is_false ~msg:"settling static output avoids pinned DECSTBM"
    (contains_substring "\027[1;23r" output);
  is_false ~msg:"settling static output does not reset DECSTBM"
    (contains_substring "\027[r" output)

let test_static_write_and_live_repaint_share_sync_frame () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~render_offset:23
      ~min_tui_height:1 ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  set_sync_capable app;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "pinned\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"synchronized output begins before static payload"
    (is_before ~first:"\027[?2026h" ~second:"pinned" output);
  is_true ~msg:"static payload appears before DECSTBM reset"
    (is_before ~first:"pinned" ~second:"\027[r" output);
  is_true ~msg:"DECSTBM resets before live repaint"
    (is_before ~first:"\027[r" ~second:"live" output);
  is_true ~msg:"live repaint is inside synchronized output"
    (is_before ~first:"live" ~second:"\027[?2026l" output)

let test_pinned_static_write_keeps_live_size () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:23 ~min_tui_height:1 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  let before = Matrix.size app in
  Matrix.static_write app ~rows:3 "a\nb\nc\n";
  equal ~msg:"pinned pending output keeps effective live size" (pair int int)
    before
    (Matrix.effective_size app);
  Matrix.submit app;
  equal ~msg:"pinned output keeps live size after submit" (pair int int) before
    (Matrix.size app)

let test_full_height_static_write_scrolls_into_scrollback () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:24 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  let before = Matrix.size app in
  equal ~msg:"full-height live viewport starts at terminal height"
    (pair int int) (80, 24) before;
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:2 "INSERTED00\nINSERTED01\n";
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"live";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"full-height static payload is emitted"
    (contains_substring "INSERTED00" output);
  is_true ~msg:"full-height static rows are scrolled into history"
    (contains_substring "\027[2S" output);
  is_true ~msg:"payload is emitted before the hardware scroll"
    (is_before ~first:"INSERTED01" ~second:"\027[2S" output);
  is_true ~msg:"live viewport repaints after static rows are scrolled"
    (is_before ~first:"\027[2S" ~second:"live" output);
  is_false ~msg:"full-height path does not use bounded DECSTBM"
    (contains_substring "\027[r" output);
  is_false ~msg:"full-height path does not use the old broad-erase preflush"
    (contains_substring "\027[1;1H\027[J" output);
  equal ~msg:"full-height static write keeps live size" (pair int int) before
    (Matrix.size app)

let test_full_height_consecutive_static_writes_scroll_once_per_row () =
  let app, state =
    make_app ~mode:`Primary ~min_tui_height:24 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.submit app;
  Buffer.clear state.output;
  Matrix.static_write app ~rows:1 "first";
  Matrix.static_write app ~rows:1 "second\n";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"consecutive full-height writes stay ordered"
    (is_before ~first:"first" ~second:"second" output);
  equal ~msg:"full-height writes scroll once per consumed row" int 2
    (count_substring "\027[1S" output)

let test_static_write_ignored_in_alt_mode () =
  let app, state =
    make_app ~mode:`Alt ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  Matrix.static_write app ~rows:2 "ignored\n";
  equal ~msg:"alt effective size unchanged" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Matrix.submit app;
  is_false ~msg:"alt mode does not emit static text"
    (contains_substring "ignored" (output state))

let test_static_clear_resets_primary_layout () =
  let app, state =
    make_app ~mode:`Primary ~terminal_tty:true ~render_offset:10
      ~target_fps:None ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"attached primary starts below transcript" (pair int int) (80, 14)
    (Matrix.size app);
  Matrix.static_write app ~rows:1 "pending\n";
  equal ~msg:"pending output affects effective size" (pair int int) (80, 13)
    (Matrix.effective_size app);
  Buffer.clear state.terminal_output;
  Matrix.static_clear app;
  is_true ~msg:"static clear is an immediate terminal reset"
    (contains_substring "\027[H\027[2J" (Buffer.contents state.terminal_output));
  equal ~msg:"static clear restores full primary height" (pair int int) (80, 24)
    (Matrix.size app);
  equal ~msg:"effective size also reset" (pair int int) (80, 24)
    (Matrix.effective_size app);
  Buffer.clear state.output;
  Matrix.prepare app;
  Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0 ~text:"after-clear";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"next live frame renders after static clear"
    (contains_substring "after-clear" output);
  is_false ~msg:"static clear discards pending static output"
    (contains_substring "pending" output)

let test_primary_full_redraw_does_not_erase_past_terminal_bottom () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  let _w, live_height = Matrix.size app in
  equal ~msg:"test setup uses 14 live rows" int 14 live_height;
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:live_height;
  Matrix.Grid.draw_text grid ~x:0 ~y:(live_height - 1) ~text:"Elapsed: 5.5s";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"bottom live row is rendered"
    (contains_substring "Elapsed: 5.5s" output);
  is_false ~msg:"full redraw must not erase from one row past terminal bottom"
    (contains_substring "\027[25;1H\027[J" output)

let test_primary_full_redraw_erases_stale_rows_below_content () =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:13;
  Matrix.Grid.draw_text grid ~x:0 ~y:12 ~text:"Press Q to quit";
  Matrix.submit app;
  let output = output state in
  is_true ~msg:"content row is rendered"
    (contains_substring "Press Q to quit" output);
  is_true ~msg:"full redraw erases rows below active content"
    (contains_substring "\027[24;1H\027[J" output)

let test_primary_required_rows_to_terminal_height_does_not_erase_past_bottom ()
    =
  let app, state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  Matrix.prepare app;
  let grid = Matrix.grid app in
  Matrix.Grid.resize grid ~width:(Matrix.Grid.width grid) ~height:24;
  Matrix.Grid.draw_text grid ~x:0 ~y:23 ~text:"bottom";
  Matrix.submit app ~primary_required_rows:24;
  let output = output state in
  equal ~msg:"primary required rows can claim the whole terminal" (pair int int)
    (80, 24) (Matrix.size app);
  is_true ~msg:"bottom row is rendered after primary growth"
    (contains_substring "bottom" output);
  is_false ~msg:"primary growth must not erase from row past terminal bottom"
    (contains_substring "\027[25;1H\027[J" output)

let test_primary_mouse_event_is_offset_into_live_region () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.)
      ~events:[ mouse_press 4 12 ]
      ()
  in
  let inputs = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      inputs := event :: !inputs;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  match List.rev !inputs with
  | [
   Matrix.Input.Mouse (Matrix.Input.Mouse.Button_press (_x, y, _button, _mods));
  ] ->
      equal ~msg:"mouse y is relative to live viewport" int 2 y
  | _ -> fail "expected one mouse press event"

let test_primary_mouse_event_above_live_region_maps_to_minus_one () =
  let app, _state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.)
      ~events:[ mouse_press 4 5 ]
      ()
  in
  let inputs = ref [] in
  Matrix.run app
    ~on_input:(fun app event ->
      inputs := event :: !inputs;
      Matrix.stop app)
    ~on_render:(fun _app -> ());
  match List.rev !inputs with
  | [
   Matrix.Input.Mouse (Matrix.Input.Mouse.Button_press (_x, y, _button, _mods));
  ] ->
      equal ~msg:"mouse y above live region maps outside UI" int (-1) y
  | _ -> fail "expected one mouse press event"

let test_primary_adjusts_all_mouse_like_events () =
  let cases =
    [
      ( "release",
        mouse_release 4 12,
        Matrix.Input.Mouse
          (Matrix.Input.Mouse.Button_release
             (4, 2, Matrix.Input.Mouse.Left, Matrix.Input.Modifier.none)) );
      ( "motion",
        mouse_motion 4 12,
        Matrix.Input.Mouse
          (Matrix.Input.Mouse.Motion
             ( 4,
               2,
               { Matrix.Input.Mouse.left = true; middle = false; right = false },
               Matrix.Input.Modifier.none )) );
      ( "scroll",
        mouse_scroll 4 12,
        Matrix.Input.Scroll
          (4, 2, Matrix.Input.Mouse.Scroll_down, 1, Matrix.Input.Modifier.none)
      );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let app, _state =
        make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
          ~input_timeout:(Some 0.) ~events:[ input ] ()
      in
      let got = ref None in
      Matrix.run app
        ~on_input:(fun app event ->
          got := Some event;
          Matrix.stop app)
        ~on_render:(fun _app -> ());
      match !got with
      | Some event when Matrix.Input.equal event expected -> ()
      | Some _ -> failf "unexpected adjusted %s event" name
      | None -> failf "missing adjusted %s event" name)
    cases

let () =
  run "matrix.runtime"
    [
      group "Control"
        [
          test "request_redraw while paused" test_request_redraw_while_paused;
          test "idle does not force live loop"
            test_idle_does_not_force_live_loop;
        ];
      group "Lifecycle"
        [
          test "run closes on exception" test_run_closes_on_exception;
          test "close restores raw mode if terminal close raises"
            test_close_restores_raw_mode_if_terminal_close_raises;
          test "focus restore runs only after blur once"
            test_focus_restore_runs_only_after_blur_once;
          test "resume reanchors primary from bottom cursor"
            test_resume_reanchors_primary_from_bottom_cursor;
        ];
      group "Frame pacing"
        [
          test "target fps respected without input storm"
            test_target_fps_respected_without_input_storm;
          test "target fps respected with input storm"
            test_target_fps_respected_with_input_storm;
        ];
      group "Frame output"
        [
          test "unchanged submit emits no bytes"
            test_unchanged_submit_emits_no_bytes;
          test "cursor-only submit emits output"
            test_cursor_only_submit_emits_output;
        ];
      group "Resize"
        [
          test "initial resize fires before first render"
            test_initial_resize_fires_before_first_render;
          test "zero dimensions are ignored"
            test_resize_zero_dimensions_are_ignored;
          test "primary render offset clamps after resize"
            test_resize_clamps_primary_render_offset;
          test "debounce applies latest pending resize"
            test_resize_debounce_applies_latest_pending_resize;
        ];
      group "Primary sizing"
        [
          test "primary required rows expands primary region"
            test_primary_required_rows_expands_primary_region;
          test "primary required rows ignored in alt mode"
            test_primary_required_rows_ignored_in_alt_mode;
          test "effective_size tracks pending static writes"
            test_primary_effective_size_tracks_pending_static;
          test "static_clear resets primary layout"
            test_static_clear_resets_primary_layout;
          test "primary full redraw does not erase past terminal bottom"
            test_primary_full_redraw_does_not_erase_past_terminal_bottom;
          test "primary full redraw erases stale rows below content"
            test_primary_full_redraw_erases_stale_rows_below_content;
          test
            "primary required rows to terminal height does not erase past \
             bottom"
            test_primary_required_rows_to_terminal_height_does_not_erase_past_bottom;
        ];
      group "Primary static output"
        [
          test "static writes are flushed FIFO"
            test_static_writes_are_flushed_fifo;
          test "static write normalizes LF in raw mode"
            test_static_write_normalizes_lf_in_raw_mode;
          test "static write preserves LF outside raw mode"
            test_static_write_preserves_lf_outside_raw_mode;
          test "static write tracks mid-line continuation"
            test_static_write_tracks_mid_line_continuation;
          test "pinned static write uses bounded scroll region"
            test_pinned_static_write_uses_bounded_scroll_region;
          test "pinned static write resets scroll region before live render"
            test_pinned_static_write_resets_scroll_region_before_live_render;
          test "multiline static write uses scroll region when it reaches pin"
            test_multiline_static_write_uses_scroll_region_when_it_reaches_pin;
          test "unpinned static write settles without scroll region"
            test_unpinned_static_write_settles_without_scroll_region;
          test "static write and live repaint share sync frame"
            test_static_write_and_live_repaint_share_sync_frame;
          test "pinned static write keeps live size"
            test_pinned_static_write_keeps_live_size;
          test "full-height static write scrolls into scrollback"
            test_full_height_static_write_scrolls_into_scrollback;
          test "full-height consecutive static writes scroll once per row"
            test_full_height_consecutive_static_writes_scroll_once_per_row;
          test "static_write is ignored in alt mode"
            test_static_write_ignored_in_alt_mode;
        ];
      group "Primary input"
        [
          test "mouse event is offset into live region"
            test_primary_mouse_event_is_offset_into_live_region;
          test "mouse event above live region maps to -1"
            test_primary_mouse_event_above_live_region_maps_to_minus_one;
          test "adjusts all mouse-like events"
            test_primary_adjusts_all_mouse_like_events;
        ];
    ]
