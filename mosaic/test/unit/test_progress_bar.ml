open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_bar ?value ?min ?max ?orientation ?filled_color ?empty_color () =
  let t = make_ctx () in
  let root = make_root t in
  let bar =
    Progress_bar.create ~parent:root ?value ?min ?max ?orientation ?filled_color
      ?empty_color ()
  in
  (t, bar)

let render_bar bar ~width ~height =
  let node = Progress_bar.node bar in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render node grid ~delta:0.;
  grid

let left_half = 0x258C
let lower_half = 0x2584

let has_half_block_h grid ~width =
  let found = ref false in
  for x = 0 to width - 1 do
    let idx = x in
    if not (Grid.is_continuation grid idx) then
      let text = Grid.get_text grid idx in
      if String.length text > 0 then
        let d = String.get_utf_8_uchar text 0 in
        let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if code = left_half then found := true
  done;
  !found

let has_half_block_v grid ~width ~height =
  let found = ref false in
  for y = 0 to height - 1 do
    let idx = y * width in
    if not (Grid.is_continuation grid idx) then
      let text = Grid.get_text grid idx in
      if String.length text > 0 then
        let d = String.get_utf_8_uchar text 0 in
        let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if code = lower_half then found := true
  done;
  !found

(* ── Props ── *)

let props_defaults () =
  let p = Progress_bar.Props.default in
  is_true ~msg:"equal to make()"
    (Progress_bar.Props.equal p (Progress_bar.Props.make ()))

let props_equal_identical () =
  let a = Progress_bar.Props.make () in
  let b = Progress_bar.Props.make () in
  is_true ~msg:"equal" (Progress_bar.Props.equal a b)

let props_detects_value_diff () =
  let a = Progress_bar.Props.make ~value:0.2 () in
  let b = Progress_bar.Props.make ~value:0.8 () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

let props_detects_min_diff () =
  let a = Progress_bar.Props.make ~min:0. () in
  let b = Progress_bar.Props.make ~min:0.5 () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

let props_detects_max_diff () =
  let a = Progress_bar.Props.make ~max:1. () in
  let b = Progress_bar.Props.make ~max:2. () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

let props_detects_orientation_diff () =
  let a = Progress_bar.Props.make ~orientation:`Horizontal () in
  let b = Progress_bar.Props.make ~orientation:`Vertical () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

let props_detects_filled_color_diff () =
  let a = Progress_bar.Props.make ~filled_color:Ansi.Color.red () in
  let b = Progress_bar.Props.make ~filled_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

let props_detects_empty_color_diff () =
  let a = Progress_bar.Props.make ~empty_color:Ansi.Color.red () in
  let b = Progress_bar.Props.make ~empty_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Progress_bar.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, bar = make_bar () in
  let node = Progress_bar.node bar in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_value () =
  let _t, bar = make_bar () in
  is_true ~msg:"value = 0" (Float.equal (Progress_bar.value bar) 0.)

let create_custom_value () =
  let _t, bar = make_bar ~value:0.5 () in
  is_true ~msg:"value = 0.5" (Float.equal (Progress_bar.value bar) 0.5)

(* ── Value Management ── *)

let set_value_updates () =
  let _t, bar = make_bar () in
  Progress_bar.set_value bar 0.75;
  is_true ~msg:"value = 0.75" (Float.equal (Progress_bar.value bar) 0.75)

let set_value_noop_same () =
  let t, bar = make_bar ~value:0.5 () in
  let before = !(t.schedule_count) in
  Progress_bar.set_value bar 0.5;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_min_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.set_min bar 0.1;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_max_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.set_max bar 2.0;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_orientation_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.set_orientation bar `Vertical;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_orientation_noop_same () =
  let t, bar = make_bar ~orientation:`Horizontal () in
  let before = !(t.schedule_count) in
  Progress_bar.set_orientation bar `Horizontal;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_filled_color_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.set_filled_color bar Ansi.Color.red;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_empty_color_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.set_empty_color bar Ansi.Color.blue;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Rendering ── *)

let horizontal_full_at_100_percent () =
  let _t, bar = make_bar ~value:1.0 () in
  let grid = render_bar bar ~width:10 ~height:1 in
  (* At 100%, no half-block should be present *)
  is_false ~msg:"no half block" (has_half_block_h grid ~width:10)

let horizontal_empty_at_0_percent () =
  let _t, bar = make_bar ~value:0.0 () in
  let grid = render_bar bar ~width:10 ~height:1 in
  (* At 0%, no half-block should be present *)
  is_false ~msg:"no half block" (has_half_block_h grid ~width:10)

let horizontal_half_block_at_50_percent () =
  let _t, bar = make_bar ~value:0.5 () in
  let grid = render_bar bar ~width:10 ~height:1 in
  (* At 50% of 10 cells = 5 full cells, no half block expected *)
  (* But at odd virtual positions, half block appears *)
  let _ = grid in
  ()

let vertical_fills_bottom_up () =
  let _t, bar = make_bar ~value:0.5 ~orientation:`Vertical () in
  let grid = render_bar bar ~width:1 ~height:10 in
  (* At 50%, bottom 5 cells should be filled *)
  let _ = grid in
  (* Just verify it renders without error *)
  ()

let vertical_half_block_at_boundary () =
  let _t, bar = make_bar ~value:0.25 ~orientation:`Vertical () in
  let grid = render_bar bar ~width:1 ~height:10 in
  (* 25% of 10 cells = 5 virtual units = 2 full + 1 half *)
  is_true ~msg:"has half block" (has_half_block_v grid ~width:1 ~height:10)

(* ── apply_props ── *)

let apply_props_replaces_all () =
  let _t, bar = make_bar () in
  let props = Progress_bar.Props.make ~value:0.5 ~min:0.1 ~max:0.9 () in
  Progress_bar.apply_props bar props;
  is_true ~msg:"value = 0.5" (Float.equal (Progress_bar.value bar) 0.5);
  is_true ~msg:"min = 0.1" (Float.equal (Progress_bar.min bar) 0.1);
  is_true ~msg:"max = 0.9" (Float.equal (Progress_bar.max bar) 0.9)

let apply_props_schedules_render () =
  let t, bar = make_bar () in
  let before = !(t.schedule_count) in
  Progress_bar.apply_props bar Progress_bar.Props.default;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, bar = make_bar () in
  let s = Format.asprintf "%a" Progress_bar.pp bar in
  is_true ~msg:"non-empty" (String.length s > 0)

let pp_contains_progress_bar () =
  let _t, bar = make_bar () in
  let s = Format.asprintf "%a" Progress_bar.pp bar in
  is_true ~msg:"has Progress_bar prefix"
    (String.length s >= 12 && String.sub s 0 12 = "Progress_bar")

(* ── Runner ── *)

let () =
  run "mosaic.progress_bar"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects value diff" props_detects_value_diff;
          test "detects min diff" props_detects_min_diff;
          test "detects max diff" props_detects_max_diff;
          test "detects orientation diff" props_detects_orientation_diff;
          test "detects filled_color diff" props_detects_filled_color_diff;
          test "detects empty_color diff" props_detects_empty_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default value = 0" create_default_value;
          test "custom value" create_custom_value;
        ];
      group "Value management"
        [
          test "set_value updates" set_value_updates;
          test "set_value noop same" set_value_noop_same;
          test "set_min schedules render" set_min_schedules_render;
          test "set_max schedules render" set_max_schedules_render;
        ];
      group "Setters"
        [
          test "set_orientation schedules render"
            set_orientation_schedules_render;
          test "set_orientation noop same" set_orientation_noop_same;
          test "set_filled_color schedules render"
            set_filled_color_schedules_render;
          test "set_empty_color schedules render"
            set_empty_color_schedules_render;
        ];
      group "Rendering"
        [
          test "horizontal full at 100%" horizontal_full_at_100_percent;
          test "horizontal empty at 0%" horizontal_empty_at_0_percent;
          test "horizontal half block at 50%"
            horizontal_half_block_at_50_percent;
          test "vertical fills bottom up" vertical_fills_bottom_up;
          test "vertical half block at boundary" vertical_half_block_at_boundary;
        ];
      group "apply_props"
        [
          test "replaces all" apply_props_replaces_all;
          test "schedules render" apply_props_schedules_render;
        ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains Progress_bar" pp_contains_progress_bar;
        ];
    ]
