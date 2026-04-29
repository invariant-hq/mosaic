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

let is_before ~first ~second s =
  match (substring_index first s, substring_index second s) with
  | Some i, Some j -> i < j
  | _ -> false

let make_app ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(input_timeout = Some 0.) ?(terminal_tty = false) ?(advance_now = true)
    ?(sleep_until_timeout = false) ?(read_quantum_s = 0.0001)
    ?(emit_event_each_read = None) ?(events = []) ?stop_after_reads
    ?render_offset ?static_needs_newline ?(min_tui_height = 1) () =
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
      ?render_offset ?static_needs_newline
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
      ~read_events
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:(fun () -> state.cleanup_calls <- state.cleanup_calls + 1)
      ~parser ~terminal ~width:80 ~height:24 ()
  in
  app_ref := Some app;
  (app, state)

let mouse_press x y =
  Matrix.Input.Mouse
    (Matrix.Input.Mouse.Button_press
       (x, y, Matrix.Input.Mouse.Left, Matrix.Input.Key.no_modifier))

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
  let app, _state =
    make_app ~mode:`Primary ~render_offset:10 ~target_fps:None
      ~input_timeout:(Some 0.) ()
  in
  equal ~msg:"attached primary starts below transcript" (pair int int) (80, 14)
    (Matrix.size app);
  Matrix.static_clear app;
  equal ~msg:"static clear restores full primary height" (pair int int) (80, 24)
    (Matrix.size app);
  equal ~msg:"effective size also reset" (pair int int) (80, 24)
    (Matrix.effective_size app)

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
        ];
      group "Frame pacing"
        [
          test "target fps respected without input storm"
            test_target_fps_respected_without_input_storm;
          test "target fps respected with input storm"
            test_target_fps_respected_with_input_storm;
        ];
      group "Resize"
        [
          test "initial resize fires before first render"
            test_initial_resize_fires_before_first_render;
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
        ];
      group "Primary static output"
        [
          test "static writes are flushed FIFO"
            test_static_writes_are_flushed_fifo;
          test "pinned static write uses bounded scroll region"
            test_pinned_static_write_uses_bounded_scroll_region;
          test "pinned static write resets scroll region before live render"
            test_pinned_static_write_resets_scroll_region_before_live_render;
          test "pinned static write keeps live size"
            test_pinned_static_write_keeps_live_size;
          test "static_write is ignored in alt mode"
            test_static_write_ignored_in_alt_mode;
        ];
      group "Primary input"
        [
          test "mouse event is offset into live region"
            test_primary_mouse_event_is_offset_into_live_region;
          test "mouse event above live region maps to -1"
            test_primary_mouse_event_above_live_region_maps_to_minus_one;
        ];
    ]
