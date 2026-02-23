open Windtrap

type fake_state = {
  mutable now_s : float;
  mutable read_calls : int;
  mutable cleanup_calls : int;
}

let make_app ?(mode = `Alt) ?(target_fps = Some 30.) ?(input_timeout = Some 0.)
    ?stop_after_reads ?render_offset () =
  let output_buf = Buffer.create 256 in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Buffer.add_string output_buf s)
      ~tty:false ()
  in
  let parser = Matrix.Input.Parser.create () in
  let state = { now_s = 0.; read_calls = 0; cleanup_calls = 0 } in
  let app_ref = ref None in
  let app =
    Matrix.attach ~mode ~target_fps ~input_timeout ?render_offset
      ~write_output:(fun _buf _off _len -> ())
      ~now:(fun () ->
        state.now_s <- state.now_s +. 0.001;
        state.now_s)
      ~wake:(fun () -> ())
      ~terminal_size:(fun () -> (80, 24))
      ~set_raw_mode:(fun _enabled -> ())
      ~flush_input:(fun () -> ())
      ~read_events:(fun ~timeout:_ ~on_event:_ ->
        state.read_calls <- state.read_calls + 1;
        match (stop_after_reads, !app_ref) with
        | Some max_reads, Some app when state.read_calls >= max_reads ->
            Matrix.stop app
        | _ -> ())
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:(fun () -> state.cleanup_calls <- state.cleanup_calls + 1)
      ~parser ~terminal ~width:80 ~height:24 ()
  in
  app_ref := Some app;
  (app, state)

let make_simulated_app ?(target_fps = Some 30.) ?(emit_event_each_read = false)
    ?(sleep_until_timeout = true) ?(read_quantum_s = 0.0001) () =
  let output_buf = Buffer.create 256 in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun s -> Buffer.add_string output_buf s)
      ~tty:false ()
  in
  let parser = Matrix.Input.Parser.create () in
  let state = { now_s = 0.; read_calls = 0; cleanup_calls = 0 } in
  let app_ref = ref None in
  let app =
    Matrix.attach ~target_fps ~input_timeout:None
      ~write_output:(fun _buf _off _len -> ())
      ~now:(fun () -> state.now_s)
      ~wake:(fun () -> ())
      ~terminal_size:(fun () -> (80, 24))
      ~set_raw_mode:(fun _enabled -> ())
      ~flush_input:(fun () -> ())
      ~read_events:(fun ~timeout ~on_event ->
        state.read_calls <- state.read_calls + 1;
        let dt =
          if sleep_until_timeout then
            match timeout with
            | Some t when t > read_quantum_s -> t
            | _ -> read_quantum_s
          else read_quantum_s
        in
        state.now_s <- state.now_s +. dt;
        if emit_event_each_read then on_event Matrix.Input.Focus)
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:(fun () -> state.cleanup_calls <- state.cleanup_calls + 1)
      ~parser ~terminal ~width:80 ~height:24 ()
  in
  app_ref := Some app;
  (app, state)

let test_request_redraw_while_paused () =
  let app, _state =
    make_app ~target_fps:(Some 30.) ~input_timeout:(Some 0.)
      ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run
    ~on_render:(fun app ->
      incr frames;
      match !frames with
      | 1 ->
          Matrix.pause app;
          Matrix.request_redraw app
      | 2 -> Matrix.stop app
      | _ -> Matrix.stop app)
    app;
  equal ~msg:"redraw triggers one-shot frame while paused" int 2 !frames

let test_idle_does_not_force_live_loop () =
  let app, _state =
    make_app ~target_fps:None ~input_timeout:(Some 0.) ~stop_after_reads:5000 ()
  in
  let frames = ref 0 in
  Matrix.run ~on_render:(fun _app -> incr frames) app;
  equal ~msg:"idle renders only the initial frame" int 1 !frames

let test_run_closes_on_exception () =
  let app, state = make_app () in
  let raised =
    try
      Matrix.run ~on_render:(fun _ -> failwith "boom") app;
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  is_true ~msg:"exception propagated" raised;
  equal ~msg:"cleanup called on exception" int 1 state.cleanup_calls

let test_target_fps_respected_without_input_storm () =
  let app, state = make_simulated_app ~target_fps:(Some 13.) () in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"13fps target should stay near cadence without input"
    (!frames >= 8 && !frames <= 20)

let test_target_fps_respected_with_input_storm () =
  let app, state =
    make_simulated_app ~target_fps:(Some 13.) ~emit_event_each_read:true
      ~sleep_until_timeout:false ()
  in
  let frames = ref 0 in
  Matrix.run app ~on_render:(fun app ->
      incr frames;
      if state.now_s >= 1. then Matrix.stop app);
  is_true ~msg:"input events must not bypass active fps cap during input storms"
    (!frames >= 8 && !frames <= 20)

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
        [ test "run closes on exception" test_run_closes_on_exception ];
      group "Frame pacing"
        [
          test "target fps respected without input storm"
            test_target_fps_respected_without_input_storm;
          test "target fps respected with input storm"
            test_target_fps_respected_with_input_storm;
        ];
      group "Primary sizing"
        [
          test "primary required rows expands primary region"
            test_primary_required_rows_expands_primary_region;
          test "primary required rows ignored in alt mode"
            test_primary_required_rows_ignored_in_alt_mode;
        ];
      group "Resize"
        [
          test "initial resize fires before first render"
            test_initial_resize_fires_before_first_render;
        ];
    ]
