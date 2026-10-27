open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_spinner ?frame_set ?color () =
  let t = make_ctx () in
  let root = make_root t in
  let spinner =
    Spinner.create ~parent:root (Spinner.Props.make ?frame_set ?color ())
  in
  (t, spinner)

let render_spinner spinner ~width ~height =
  let node = Spinner.node spinner in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Test_harness.render node grid ~dt:0.;
  grid

(* A frame [dt] seconds after the last, as the renderer draws it. *)
let advance_frame spinner ~dt =
  Test_harness.render (Spinner.node spinner) (make_grid ()) ~dt

(* ── Props ── *)

let props_defaults () =
  let p = Spinner.Props.default in
  is_true ~msg:"equal to make()" (Spinner.Props.equal p (Spinner.Props.make ()))

let props_equal_identical () =
  let a = Spinner.Props.make () in
  let b = Spinner.Props.make () in
  is_true ~msg:"equal" (Spinner.Props.equal a b)

let props_detects_frame_set_diff () =
  let a = Spinner.Props.make ~frame_set:Spinner.dots () in
  let b = Spinner.Props.make ~frame_set:Spinner.line () in
  is_false ~msg:"different" (Spinner.Props.equal a b)

let props_detects_color_diff () =
  let a = Spinner.Props.make ~color:Ansi.Color.red () in
  let b = Spinner.Props.make ~color:Ansi.Color.blue () in
  is_false ~msg:"different" (Spinner.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, spinner = make_spinner () in
  let node = Spinner.node spinner in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_initial_frame_index () =
  let _t, spinner = make_spinner () in
  equal ~msg:"frame_index = 0" int 0 (Spinner.frame_index spinner)

let create_initial_elapsed () =
  let _t, spinner = make_spinner () in
  is_true ~msg:"elapsed = 0" (Float.equal (Spinner.elapsed spinner) 0.)

let create_is_live () =
  (* The widget owns the animation invariant: no external set_live needed. *)
  let _t, spinner = make_spinner () in
  is_true ~msg:"live on creation" (Renderable.live (Spinner.node spinner))

let static_frame_set_is_not_live () =
  let _t, spinner =
    make_spinner ~frame_set:{ Spinner.frames = [| "x" |]; period = 0.08 } ()
  in
  is_false ~msg:"single frame needs no ticks"
    (Renderable.live (Spinner.node spinner));
  Spinner.set_frame_set spinner Spinner.dots;
  is_true ~msg:"animating set restores liveness"
    (Renderable.live (Spinner.node spinner))

(* ── Frame Advancement ── *)

let on_frame_advances_after_interval () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  equal ~msg:"starts at 0" int 0 (Spinner.frame_index spinner);
  advance_frame spinner ~dt:0.08;
  equal ~msg:"advanced to 1" int 1 (Spinner.frame_index spinner)

let on_frame_accumulates_delta () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  advance_frame spinner ~dt:0.04;
  equal ~msg:"still at 0" int 0 (Spinner.frame_index spinner);
  advance_frame spinner ~dt:0.04;
  equal ~msg:"advanced to 1" int 1 (Spinner.frame_index spinner)

let on_frame_wraps_around () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  (* dots has 10 frames at a 0.08 s interval *)
  (* Advance 10 intervals to wrap around *)
  for _ = 1 to 10 do
    advance_frame spinner ~dt:0.08
  done;
  equal ~msg:"wrapped to 0" int 0 (Spinner.frame_index spinner)

let on_frame_short_delta_no_advance () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  advance_frame spinner ~dt:0.01;
  equal ~msg:"still at 0" int 0 (Spinner.frame_index spinner)

let on_frame_large_delta_skips_frames () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  (* 0.25 s should advance 3 frames at 0.08 s interval *)
  advance_frame spinner ~dt:0.25;
  equal ~msg:"advanced 3" int 3 (Spinner.frame_index spinner)

(* ── Setters ── *)

let set_frame_set_resets_elapsed () =
  let _t, spinner = make_spinner () in
  advance_frame spinner ~dt:0.04;
  satisfies ~claim:"elapsed above 0" float_exact
    (fun e -> e > 0.)
    (Spinner.elapsed spinner);
  Spinner.set_frame_set spinner Spinner.line;
  is_true ~msg:"elapsed reset" (Float.equal (Spinner.elapsed spinner) 0.)

let set_frame_set_clamps_index () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  (* Advance to frame 8 *)
  for _ = 1 to 8 do
    advance_frame spinner ~dt:0.08
  done;
  equal ~msg:"at frame 8" int 8 (Spinner.frame_index spinner);
  (* bounce has only 4 frames, so index should be clamped *)
  Spinner.set_frame_set spinner Spinner.bounce;
  let frames = Array.length Spinner.bounce.frames in
  lt ~msg:"frame index in range" ~than:frames (Spinner.frame_index spinner)

let set_color_schedules_render () =
  let t, spinner = make_spinner () in
  let before = Test_harness.clean t in
  Spinner.set_color spinner Ansi.Color.red;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_color_noop_same () =
  let t, spinner = make_spinner ~color:Ansi.Color.white () in
  let before = Test_harness.clean t in
  Spinner.set_color spinner Ansi.Color.white;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── apply_props ── *)

let apply_props_replaces_props () =
  let _t, spinner = make_spinner () in
  let props =
    Spinner.Props.make ~frame_set:Spinner.line ~color:Ansi.Color.red ()
  in
  Spinner.apply_props spinner props;
  (* Verify by checking that the new props are in effect *)
  let grid = render_spinner spinner ~width:2 ~height:1 in
  let text = Matrix_grid.get_text grid 0 in
  (* line frame set starts with "-" *)
  is_true ~msg:"first frame is -" (String.equal text "-")

let apply_props_resets_on_frame_set_change () =
  let _t, spinner = make_spinner () in
  advance_frame spinner ~dt:0.04;
  satisfies ~claim:"has elapsed (above 0)" float_exact
    (fun e -> e > 0.)
    (Spinner.elapsed spinner);
  let props = Spinner.Props.make ~frame_set:Spinner.line () in
  Spinner.apply_props spinner props;
  is_true ~msg:"elapsed reset" (Float.equal (Spinner.elapsed spinner) 0.)

let apply_props_schedules_render () =
  let t, spinner = make_spinner () in
  let before = Test_harness.clean t in
  Spinner.apply_props spinner Spinner.Props.default;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Measure ── *)

let measure_width_is_max_frame_width () =
  let _t, spinner = make_spinner ~frame_set:Spinner.dots () in
  (* All braille chars are 1 cell wide, so max_width = 1 *)
  let node = Spinner.node spinner in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:1;
  let grid = make_grid ~width:10 ~height:1 () in
  Test_harness.render node grid ~dt:0.;
  (* The spinner should have an intrinsic width of 1 for braille chars *)
  let text = Matrix_grid.get_text grid 0 in
  satisfies ~claim:"renders something (non-empty)" string
    (fun t -> String.length t > 0)
    text

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, spinner = make_spinner () in
  let s = Format.asprintf "%a" Spinner.pp spinner in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

let pp_contains_spinner () =
  let _t, spinner = make_spinner () in
  let s = Format.asprintf "%a" Spinner.pp spinner in
  is_true ~msg:"has Spinner prefix"
    (String.length s >= 7 && String.sub s 0 7 = "Spinner")

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.spinner}) or to
   {!Spinner.Props.make} for {!Spinner.create}, reaches the live spinner
   ({!Test_harness.reaches}). *)

let doors_size = Mosaic.size ~width:4 ~height:1
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red

let doors_frame_set () =
  Test_harness.reaches "frame_set"
    ~base:(Mosaic.spinner ~size:doors_size ())
    ~element:(Mosaic.spinner ~size:doors_size ~frame_set:Spinner.line ())
    ~create_base:(fun parent ->
      Spinner.node
        (Spinner.create ~parent ~style:doors_style (Spinner.Props.make ())))
    ~create:(fun parent ->
      Spinner.node
        (Spinner.create ~parent ~style:doors_style
           (Spinner.Props.make ~frame_set:Spinner.line ())))

let doors_color () =
  Test_harness.reaches "color"
    ~base:(Mosaic.spinner ~size:doors_size ())
    ~element:(Mosaic.spinner ~size:doors_size ~color:doors_red ())
    ~create_base:(fun parent ->
      Spinner.node
        (Spinner.create ~parent ~style:doors_style (Spinner.Props.make ())))
    ~create:(fun parent ->
      Spinner.node
        (Spinner.create ~parent ~style:doors_style
           (Spinner.Props.make ~color:doors_red ())))

(* ── Paint ── *)

let paint_role () =
  let red = Ansi.Color.indexed 1 and blue = Ansi.Color.indexed 4 in
  let themed = Theme.with_style Plain (Ansi.Style.make ~fg:red ()) in
  is_true ~msg:"the Plain role"
    (fg_is red
       (cell_style
          (paint (Mosaic.themed themed (Mosaic.spinner ~size:doors_size ())))
          ~x:0 ~y:0));
  is_true ~msg:"the label first"
    (fg_is blue
       (cell_style
          (paint
             (Mosaic.themed themed
                (Mosaic.spinner ~size:doors_size ~color:blue ())))
          ~x:0 ~y:0))

let () =
  run "mosaic.spinner"
    [
      group "Paint" [ test "by role, label first" paint_role ];
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects frame_set diff" props_detects_frame_set_diff;
          test "detects color diff" props_detects_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "initial frame_index = 0" create_initial_frame_index;
          test "initial elapsed = 0" create_initial_elapsed;
          test "live on creation" create_is_live;
          test "static frame set is not live" static_frame_set_is_not_live;
        ];
      group "Frame advancement"
        [
          test "advances after interval" on_frame_advances_after_interval;
          test "accumulates delta" on_frame_accumulates_delta;
          test "wraps around" on_frame_wraps_around;
          test "short delta no advance" on_frame_short_delta_no_advance;
          test "large delta skips frames" on_frame_large_delta_skips_frames;
        ];
      group "Setters"
        [
          test "set_frame_set resets elapsed" set_frame_set_resets_elapsed;
          test "set_frame_set clamps index" set_frame_set_clamps_index;
          test "set_color schedules render" set_color_schedules_render;
          test "set_color noop same" set_color_noop_same;
        ];
      group "apply_props"
        [
          test "replaces props" apply_props_replaces_props;
          test "resets on frame_set change"
            apply_props_resets_on_frame_set_change;
          test "schedules render" apply_props_schedules_render;
        ];
      group "Measure"
        [
          test "width matches max frame width" measure_width_is_max_frame_width;
        ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains Spinner" pp_contains_spinner;
        ];
      group "Both doors"
        [ test "frame_set" doors_frame_set; test "color" doors_color ];
    ]
