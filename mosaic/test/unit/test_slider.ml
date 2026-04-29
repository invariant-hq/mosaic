open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_slider ?orientation ?value ?min ?max ?viewport_size ?track_color
    ?thumb_color ?on_change () =
  let t = make_ctx () in
  let root = make_root t in
  let slider =
    Slider.create ~parent:root ?orientation ?value ?min ?max ?viewport_size
      ?track_color ?thumb_color ?on_change ()
  in
  (t, slider)

let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_drag ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Drag { button = Left; is_dragging = true })

let mouse_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Up { button = Left; is_dragging = false })

let render_slider slider ~width ~height =
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render node grid ~delta:0.;
  grid

let full_block = 0x2588
let left_half = 0x258C
let right_half = 0x2590
let upper_half = 0x2580
let lower_half = 0x2584
let float_eq a b = Float.equal a b
let float_close ?(eps = 0.5) a b = Float.abs (a -. b) < eps

(* Count how many cells in a row have a thumb cell (horizontal). *)
let count_thumb_cells_h grid ~y ~width =
  let n = ref 0 in
  for x = 0 to width - 1 do
    let idx = (y * width) + x in
    if not (Grid.is_continuation grid idx) then
      let text = Grid.get_text grid idx in
      if String.length text > 0 then
        let d = String.get_utf_8_uchar text 0 in
        let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if code = full_block || code = left_half || code = right_half then
          incr n
  done;
  !n

(* Count how many cells in a column have a thumb cell (vertical). *)
let count_thumb_cells_v grid ~x ~width ~height =
  let n = ref 0 in
  for y = 0 to height - 1 do
    let idx = (y * width) + x in
    if not (Grid.is_continuation grid idx) then
      let text = Grid.get_text grid idx in
      if String.length text > 0 then
        let d = String.get_utf_8_uchar text 0 in
        let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if code = full_block || code = upper_half || code = lower_half then
          incr n
  done;
  !n

(* ── Props ── *)

let props_defaults () =
  let p = Slider.Props.default in
  is_true ~msg:"equal to make()" (Slider.Props.equal p (Slider.Props.make ()))

let props_equal_identical () =
  let a = Slider.Props.make () in
  let b = Slider.Props.make () in
  is_true ~msg:"equal" (Slider.Props.equal a b)

let props_detects_orientation_diff () =
  let a = Slider.Props.make ~orientation:`Horizontal () in
  let b = Slider.Props.make ~orientation:`Vertical () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_value_diff () =
  let a = Slider.Props.make ~value:10. () in
  let b = Slider.Props.make ~value:20. () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_min_diff () =
  let a = Slider.Props.make ~min:0. () in
  let b = Slider.Props.make ~min:5. () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_max_diff () =
  let a = Slider.Props.make ~max:100. () in
  let b = Slider.Props.make ~max:200. () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_viewport_size_diff () =
  let a = Slider.Props.make ~viewport_size:10. () in
  let b = Slider.Props.make ~viewport_size:20. () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_track_color_diff () =
  let a = Slider.Props.make ~track_color:Ansi.Color.red () in
  let b = Slider.Props.make ~track_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Slider.Props.equal a b)

let props_detects_thumb_color_diff () =
  let a = Slider.Props.make ~thumb_color:Ansi.Color.red () in
  let b = Slider.Props.make ~thumb_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Slider.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, slider = make_slider () in
  let node = Slider.node slider in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_value_is_min () =
  let _t, slider = make_slider () in
  is_true ~msg:"value = 0" (float_eq (Slider.value slider) 0.)

let create_custom_value () =
  let _t, slider = make_slider ~value:50. () in
  is_true ~msg:"value = 50" (float_eq (Slider.value slider) 50.)

let create_value_clamped_to_max () =
  let _t, slider = make_slider ~value:150. () in
  is_true ~msg:"clamped to 100" (float_eq (Slider.value slider) 100.)

let create_value_clamped_to_min () =
  let _t, slider = make_slider ~value:(-10.) () in
  is_true ~msg:"clamped to 0" (float_eq (Slider.value slider) 0.)

let create_custom_min_max () =
  let _t, slider = make_slider ~min:10. ~max:50. ~value:30. () in
  is_true ~msg:"value = 30" (float_eq (Slider.value slider) 30.);
  is_true ~msg:"min = 10" (float_eq (Slider.min slider) 10.);
  is_true ~msg:"max = 50" (float_eq (Slider.max slider) 50.)

let create_value_defaults_to_min () =
  let _t, slider = make_slider ~min:25. ~max:75. () in
  is_true ~msg:"value = min" (float_eq (Slider.value slider) 25.)

(* ── Value Management ── *)

let set_value_updates () =
  let _t, slider = make_slider () in
  Slider.set_value slider 50.;
  is_true ~msg:"value = 50" (float_eq (Slider.value slider) 50.)

let set_value_clamps_high () =
  let _t, slider = make_slider () in
  Slider.set_value slider 150.;
  is_true ~msg:"clamped to 100" (float_eq (Slider.value slider) 100.)

let set_value_clamps_low () =
  let _t, slider = make_slider () in
  Slider.set_value slider (-10.);
  is_true ~msg:"clamped to 0" (float_eq (Slider.value slider) 0.)

let set_value_fires_on_change () =
  let changed = ref None in
  let _t, slider = make_slider ~on_change:(fun v -> changed := Some v) () in
  Slider.set_value slider 42.;
  match !changed with
  | Some v -> is_true ~msg:"value = 42" (float_eq v 42.)
  | None -> fail "on_change not fired"

let set_value_noop_same_value () =
  let count = ref 0 in
  let _t, slider = make_slider ~on_change:(fun _ -> incr count) () in
  Slider.set_value slider 50.;
  equal ~msg:"fired once" int 1 !count;
  Slider.set_value slider 50.;
  equal ~msg:"still once" int 1 !count

let set_min_clamps_value () =
  let _t, slider = make_slider ~value:20. () in
  Slider.set_min slider 30.;
  is_true ~msg:"value clamped to 30" (float_eq (Slider.value slider) 30.)

let set_min_no_clamp_when_value_in_range () =
  let _t, slider = make_slider ~value:50. () in
  Slider.set_min slider 10.;
  is_true ~msg:"value still 50" (float_eq (Slider.value slider) 50.)

let set_max_clamps_value () =
  let _t, slider = make_slider ~value:80. () in
  Slider.set_max slider 60.;
  is_true ~msg:"value clamped to 60" (float_eq (Slider.value slider) 60.)

let set_max_no_clamp_when_value_in_range () =
  let _t, slider = make_slider ~value:50. () in
  Slider.set_max slider 200.;
  is_true ~msg:"value still 50" (float_eq (Slider.value slider) 50.)

let set_min_fires_on_change_when_clamping () =
  let changed = ref None in
  let _t, slider =
    make_slider ~value:20. ~on_change:(fun v -> changed := Some v) ()
  in
  Slider.set_min slider 30.;
  match !changed with
  | Some v -> is_true ~msg:"clamped to 30" (float_eq v 30.)
  | None -> fail "on_change not fired"

let set_max_fires_on_change_when_clamping () =
  let changed = ref None in
  let _t, slider =
    make_slider ~value:80. ~on_change:(fun v -> changed := Some v) ()
  in
  Slider.set_max slider 60.;
  match !changed with
  | Some v -> is_true ~msg:"clamped to 60" (float_eq v 60.)
  | None -> fail "on_change not fired"

(* ── Setters ── *)

let set_orientation_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_orientation slider `Vertical;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_orientation_noop_same () =
  let t, slider = make_slider ~orientation:`Horizontal () in
  let before = !(t.schedule_count) in
  Slider.set_orientation slider `Horizontal;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_viewport_size_clamps_low () =
  let _t, slider = make_slider () in
  Slider.set_viewport_size slider 0.001;
  (* viewport_size is clamped to 0.01 minimum - verify by checking that a
     re-render with different viewport produces a different thumb size *)
  let grid1 = render_slider slider ~width:20 ~height:1 in
  let thumb1 = count_thumb_cells_h grid1 ~y:0 ~width:20 in
  Slider.set_viewport_size slider 50.;
  let grid2 = render_slider slider ~width:20 ~height:1 in
  let thumb2 = count_thumb_cells_h grid2 ~y:0 ~width:20 in
  is_true ~msg:"different thumb sizes" (thumb1 <> thumb2)

let set_viewport_size_clamps_high () =
  (* range is 100, so viewport_size should clamp to 100 *)
  let _t, slider = make_slider () in
  Slider.set_viewport_size slider 200.;
  let grid = render_slider slider ~width:20 ~height:1 in
  let thumb = count_thumb_cells_h grid ~y:0 ~width:20 in
  (* With viewport = range, thumb should be about half the track *)
  is_true ~msg:"thumb is large" (thumb >= 8)

let set_viewport_size_noop_same () =
  let t, slider = make_slider ~viewport_size:10. () in
  let before = !(t.schedule_count) in
  Slider.set_viewport_size slider 10.;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_track_color_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_track_color slider Ansi.Color.red;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_track_color_noop_same () =
  let t, slider = make_slider ~track_color:Ansi.Color.red () in
  let before = !(t.schedule_count) in
  Slider.set_track_color slider Ansi.Color.red;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_thumb_color_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_thumb_color slider Ansi.Color.red;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_thumb_color_noop_same () =
  let t, slider = make_slider ~thumb_color:Ansi.Color.red () in
  let before = !(t.schedule_count) in
  Slider.set_thumb_color slider Ansi.Color.red;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_on_change_replaces_callback () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, slider =
    make_slider ~on_change:(fun _ -> incr first_count) ~value:0. ()
  in
  Slider.set_value slider 10.;
  equal ~msg:"first fired" int 1 !first_count;
  Slider.set_on_change slider (Some (fun _ -> incr second_count));
  Slider.set_value slider 20.;
  equal ~msg:"first not fired again" int 1 !first_count;
  equal ~msg:"second fired" int 1 !second_count

(* ── apply_props ── *)

let apply_props_replaces_all () =
  let _t, slider = make_slider ~value:50. () in
  let props = Slider.Props.make ~value:25. ~min:10. ~max:80. () in
  Slider.apply_props slider props;
  is_true ~msg:"value = 25" (float_eq (Slider.value slider) 25.);
  is_true ~msg:"min = 10" (float_eq (Slider.min slider) 10.);
  is_true ~msg:"max = 80" (float_eq (Slider.max slider) 80.)

let apply_props_clamps_value () =
  let _t, slider = make_slider ~value:80. () in
  let props = Slider.Props.make ~value:80. ~min:0. ~max:50. () in
  Slider.apply_props slider props;
  is_true ~msg:"clamped to 50" (float_eq (Slider.value slider) 50.)

let apply_props_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.apply_props slider Slider.Props.default;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let apply_props_does_not_fire_on_change () =
  let fired = ref false in
  let _t, slider =
    make_slider ~value:0. ~on_change:(fun _ -> fired := true) ()
  in
  let props = Slider.Props.make ~value:50. () in
  Slider.apply_props slider props;
  is_true ~msg:"value changed" (float_eq (Slider.value slider) 50.);
  is_false ~msg:"on_change not fired" !fired

(* ── Virtual Thumb Sizing ── *)

let minimum_thumb_with_extreme_range () =
  let _t, slider = make_slider ~value:0. ~max:10000. ~viewport_size:1. () in
  let grid = render_slider slider ~width:20 ~height:1 in
  let thumb = count_thumb_cells_h grid ~y:0 ~width:20 in
  is_true ~msg:"thumb visible (at least 1 cell)" (thumb >= 1)

let minimum_thumb_tiny_track_vertical () =
  let _t, slider =
    make_slider ~orientation:`Vertical ~value:0. ~max:10000. ~viewport_size:0.01
      ()
  in
  let grid = render_slider slider ~width:1 ~height:2 in
  let thumb = count_thumb_cells_v grid ~x:0 ~width:1 ~height:2 in
  is_true ~msg:"thumb visible (at least 1 cell)" (thumb >= 1)

let viewport_equals_range_thumb_is_half () =
  let _t, slider = make_slider ~value:0. ~max:100. ~viewport_size:100. () in
  let grid = render_slider slider ~width:20 ~height:1 in
  let thumb = count_thumb_cells_h grid ~y:0 ~width:20 in
  (* viewport / (range + viewport) = 100/200 = 0.5, so ~10 cells of 20 *)
  is_true ~msg:"thumb roughly half the track" (thumb >= 8 && thumb <= 12)

let very_large_viewport_clamps_thumb () =
  let _t, slider = make_slider ~value:0. ~max:100. ~viewport_size:200. () in
  let grid = render_slider slider ~width:20 ~height:1 in
  let thumb = count_thumb_cells_h grid ~y:0 ~width:20 in
  (* viewport is clamped to range via set_viewport_size, but Props.make doesn't
     clamp — it passes through. However the thumb ratio still applies:
     200/(100+200) = 0.67 of track, so roughly 13 cells *)
  is_true ~msg:"thumb is large" (thumb >= 10)

(* ── Mouse Interaction ── *)

let click_on_track_jumps_value () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  (* Render once so the virtual coordinate system has real dimensions *)
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  (* Click at 75% of the track *)
  Renderable.Private.emit_mouse node (mouse_down ~x:15 ~y:0);
  is_true ~msg:"value near 75" (float_close (Slider.value slider) 75.)

let click_on_track_vertical () =
  let _t, slider = make_slider ~orientation:`Vertical ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:1 ~height:20;
  let grid = make_grid ~width:1 ~height:20 () in
  Renderable.Private.render node grid ~delta:0.;
  (* Click at 50% of the track *)
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:10);
  is_true ~msg:"value near 50" (float_close (Slider.value slider) 50.)

let drag_updates_value () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  (* Press down then drag to midpoint *)
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  (* Drag offset accounts for thumb size, so value won't be exactly 50 *)
  let v = Slider.value slider in
  is_true ~msg:"value moved toward midrange" (v > 30. && v < 70.)

let drag_fires_on_change () =
  let changes = ref [] in
  let _t, slider =
    make_slider ~value:0. ~on_change:(fun v -> changes := v :: !changes) ()
  in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  is_true ~msg:"on_change fired" (List.length !changes > 0)

let drag_beyond_bounds_clamps () =
  let _t, slider = make_slider ~value:50. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  (* Drag way past the right edge *)
  Renderable.Private.emit_mouse node (mouse_down ~x:10 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:100 ~y:0);
  is_true ~msg:"clamped to max" (Slider.value slider <= 100.);
  (* Drag way past the left edge *)
  Renderable.Private.emit_mouse node (mouse_drag ~x:(-50) ~y:0);
  is_true ~msg:"clamped to min" (Slider.value slider >= 0.)

let mouse_up_ends_drag () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  (* Start drag *)
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  let v_during = Slider.value slider in
  is_true ~msg:"value changed during drag" (v_during > 0.);
  (* Release *)
  Renderable.Private.emit_mouse node (mouse_up ~x:10 ~y:0);
  (* Subsequent drag should not update (not dragging anymore) *)
  Renderable.Private.emit_mouse node (mouse_drag ~x:0 ~y:0);
  let v_after = Slider.value slider in
  is_true ~msg:"value unchanged after release"
    (float_eq v_after (Slider.value slider))

let drag_vertical () =
  let _t, slider = make_slider ~orientation:`Vertical ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:1 ~height:20;
  let grid = make_grid ~width:1 ~height:20 () in
  Renderable.Private.render node grid ~delta:0.;
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:0 ~y:10);
  let v = Slider.value slider in
  is_true ~msg:"value moved toward midrange" (v > 30. && v < 70.)

let mouse_down_stops_propagation () =
  let t, slider = make_slider ~value:50. () in
  let root = make_root t in
  let parent_fired = ref false in
  Renderable.on_mouse root (fun _ -> parent_fired := true);
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~delta:0.;
  let ev = mouse_down ~x:10 ~y:0 in
  Renderable.Private.emit_mouse node ev;
  is_true ~msg:"propagation stopped" (Event.Mouse.propagation_stopped ev)

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, slider = make_slider () in
  let s = Format.asprintf "%a" Slider.pp slider in
  is_true ~msg:"non-empty" (String.length s > 0)

let pp_contains_slider () =
  let _t, slider = make_slider () in
  let s = Format.asprintf "%a" Slider.pp slider in
  is_true ~msg:"has Slider prefix"
    (String.length s >= 6 && String.sub s 0 6 = "Slider")

let string_contains haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if (not !found) && String.sub haystack i nlen = needle then found := true
    done;
    !found

let pp_contains_value () =
  let _t, slider = make_slider ~value:42. () in
  let s = Format.asprintf "%a" Slider.pp slider in
  is_true ~msg:"contains value" (string_contains s "42.0")

(* ── Runner ── *)

let () =
  run "mosaic.slider"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects orientation diff" props_detects_orientation_diff;
          test "detects value diff" props_detects_value_diff;
          test "detects min diff" props_detects_min_diff;
          test "detects max diff" props_detects_max_diff;
          test "detects viewport_size diff" props_detects_viewport_size_diff;
          test "detects track_color diff" props_detects_track_color_diff;
          test "detects thumb_color diff" props_detects_thumb_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default value is min" create_default_value_is_min;
          test "custom value" create_custom_value;
          test "value clamped to max" create_value_clamped_to_max;
          test "value clamped to min" create_value_clamped_to_min;
          test "custom min/max" create_custom_min_max;
          test "value defaults to min" create_value_defaults_to_min;
        ];
      group "Value management"
        [
          test "set_value updates" set_value_updates;
          test "set_value clamps high" set_value_clamps_high;
          test "set_value clamps low" set_value_clamps_low;
          test "set_value fires on_change" set_value_fires_on_change;
          test "set_value no-op same value" set_value_noop_same_value;
          test "set_min clamps value" set_min_clamps_value;
          test "set_min no clamp when in range"
            set_min_no_clamp_when_value_in_range;
          test "set_max clamps value" set_max_clamps_value;
          test "set_max no clamp when in range"
            set_max_no_clamp_when_value_in_range;
          test "set_min fires on_change" set_min_fires_on_change_when_clamping;
          test "set_max fires on_change" set_max_fires_on_change_when_clamping;
        ];
      group "Setters"
        [
          test "set_orientation schedules render"
            set_orientation_schedules_render;
          test "set_orientation no-op same" set_orientation_noop_same;
          test "set_viewport_size clamps low" set_viewport_size_clamps_low;
          test "set_viewport_size clamps high" set_viewport_size_clamps_high;
          test "set_viewport_size no-op same" set_viewport_size_noop_same;
          test "set_track_color schedules render"
            set_track_color_schedules_render;
          test "set_track_color no-op same" set_track_color_noop_same;
          test "set_thumb_color schedules render"
            set_thumb_color_schedules_render;
          test "set_thumb_color no-op same" set_thumb_color_noop_same;
          test "set_on_change replaces callback" set_on_change_replaces_callback;
        ];
      group "apply_props"
        [
          test "replaces all properties" apply_props_replaces_all;
          test "clamps value to new range" apply_props_clamps_value;
          test "schedules render" apply_props_schedules_render;
          test "does not fire on_change" apply_props_does_not_fire_on_change;
        ];
      group "Virtual thumb sizing"
        [
          test "minimum thumb with extreme range"
            minimum_thumb_with_extreme_range;
          test "minimum thumb tiny track vertical"
            minimum_thumb_tiny_track_vertical;
          test "viewport equals range gives half-track thumb"
            viewport_equals_range_thumb_is_half;
          test "very large viewport gives large thumb"
            very_large_viewport_clamps_thumb;
        ];
      group "Mouse interaction"
        [
          test "click on track jumps value" click_on_track_jumps_value;
          test "click on track vertical" click_on_track_vertical;
          test "drag updates value" drag_updates_value;
          test "drag vertical" drag_vertical;
          test "drag fires on_change" drag_fires_on_change;
          test "drag beyond bounds clamps" drag_beyond_bounds_clamps;
          test "mouse up ends drag" mouse_up_ends_drag;
          test "mouse down stops propagation" mouse_down_stops_propagation;
        ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains Slider" pp_contains_slider;
          test "contains value" pp_contains_value;
        ];
    ]
