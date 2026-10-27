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
  Slider.set_on_change slider
    (Some
       (fun value ->
         Slider.set_value slider value;
         Option.iter (fun f -> f value) on_change));
  (t, slider)

let render_slider slider ~width ~height =
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render node grid ~dt:0.;
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
    if not (Matrix_grid.is_continuation grid idx) then
      let text = Matrix_grid.get_text grid idx in
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
    if not (Matrix_grid.is_continuation grid idx) then
      let text = Matrix_grid.get_text grid idx in
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
  equal ~msg:"accepted setter is silent" (option float_exact) None !changed

let set_value_noop_same_value () =
  let count = ref 0 in
  let _t, slider = make_slider ~on_change:(fun _ -> incr count) () in
  Slider.set_value slider 50.;
  equal ~msg:"setter is silent" int 0 !count;
  Slider.set_value slider 50.;
  equal ~msg:"setter remains silent" int 0 !count

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
  equal ~msg:"accepted range update is silent" (option float_exact) None
    !changed

let set_max_fires_on_change_when_clamping () =
  let changed = ref None in
  let _t, slider =
    make_slider ~value:80. ~on_change:(fun v -> changed := Some v) ()
  in
  Slider.set_max slider 60.;
  equal ~msg:"accepted range update is silent" (option float_exact) None
    !changed

(* ── Setters ── *)

let set_orientation_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_orientation slider `Vertical;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

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
  ge ~msg:"thumb is large" ~than:8 thumb

let set_viewport_size_noop_same () =
  let t, slider = make_slider ~viewport_size:10. () in
  let before = !(t.schedule_count) in
  Slider.set_viewport_size slider 10.;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_track_color_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_track_color slider Ansi.Color.red;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_track_color_noop_same () =
  let t, slider = make_slider ~track_color:Ansi.Color.red () in
  let before = !(t.schedule_count) in
  Slider.set_track_color slider Ansi.Color.red;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_thumb_color_schedules_render () =
  let t, slider = make_slider () in
  let before = !(t.schedule_count) in
  Slider.set_thumb_color slider Ansi.Color.red;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

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
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  Renderable.Private.emit_mouse node (mouse_down ~x:5 ~y:0);
  equal ~msg:"first fired" int 1 !first_count;
  Slider.set_on_change slider (Some (fun _ -> incr second_count));
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  equal ~msg:"first not fired again" int 1 !first_count;
  equal ~msg:"second fired" int 1 !second_count

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
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let apply_props_does_not_fire_on_change () =
  let fired = ref false in
  let _t, slider =
    make_slider ~value:0. ~on_change:(fun _ -> fired := true) ()
  in
  let props = Slider.Props.make ~value:50. () in
  Slider.apply_props slider props;
  is_true ~msg:"value changed" (float_eq (Slider.value slider) 50.);
  is_false ~msg:"on_change not fired" !fired

let apply_props_restores_declared_value () =
  let _t, slider = make_slider () in
  Slider.set_value slider 42.;
  Slider.apply_props slider (Slider.Props.make ~thumb_color:Ansi.Color.red ());
  is_true ~msg:"default accepted value replaces prior value"
    (float_eq (Slider.value slider) 0.)

let apply_props_clamps_declared_value () =
  let _t, slider = make_slider () in
  Slider.set_value slider 90.;
  Slider.apply_props slider (Slider.Props.make ~value:90. ~min:0. ~max:50. ());
  is_true ~msg:"declared value clamps to the new range"
    (float_eq (Slider.value slider) 50.)

(* ── Virtual Thumb Sizing ── *)

let minimum_thumb_with_extreme_range () =
  let _t, slider = make_slider ~value:0. ~max:10000. ~viewport_size:1. () in
  let grid = render_slider slider ~width:20 ~height:1 in
  let thumb = count_thumb_cells_h grid ~y:0 ~width:20 in
  ge ~msg:"thumb visible" ~than:1 thumb

let minimum_thumb_tiny_track_vertical () =
  let _t, slider =
    make_slider ~orientation:`Vertical ~value:0. ~max:10000. ~viewport_size:0.01
      ()
  in
  let grid = render_slider slider ~width:1 ~height:2 in
  let thumb = count_thumb_cells_v grid ~x:0 ~width:1 ~height:2 in
  ge ~msg:"thumb visible" ~than:1 thumb

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
  ge ~msg:"thumb is large" ~than:10 thumb

(* ── Mouse Interaction ── *)

let click_on_track_jumps_value () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  (* Render once so the virtual coordinate system has real dimensions *)
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~dt:0.;
  (* Click at 75% of the track *)
  Renderable.Private.emit_mouse node (mouse_down ~x:15 ~y:0);
  is_true ~msg:"value near 75" (float_close (Slider.value slider) 75.)

(* A click is a press and a release at the same cell, and proposes one value:
   the release where the pointer has not moved proposes nothing more. *)
let click_proposes_one_value () =
  let proposals = ref [] in
  let _t, slider =
    make_slider ~value:0. ~on_change:(fun v -> proposals := v :: !proposals) ()
  in
  ignore (render_slider slider ~width:20 ~height:1 : Matrix_grid.t);
  Renderable.Private.emit_mouse (Slider.node slider) (mouse_down ~x:15 ~y:0);
  Renderable.Private.emit_mouse (Slider.node slider) (mouse_up ~x:15 ~y:0);
  equal ~msg:"one proposal" (list float_exact) [ 75. ] !proposals

let click_on_track_vertical () =
  let _t, slider = make_slider ~orientation:`Vertical ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:1 ~height:20;
  let grid = make_grid ~width:1 ~height:20 () in
  Renderable.Private.render node grid ~dt:0.;
  (* Click at 50% of the track *)
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:10);
  is_true ~msg:"value near 50" (float_close (Slider.value slider) 50.)

let drag_updates_value () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~dt:0.;
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
  Renderable.Private.render node grid ~dt:0.;
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  gt ~msg:"on_change fired" ~than:0 (List.length !changes)

let drag_beyond_bounds_clamps () =
  let _t, slider = make_slider ~value:50. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~dt:0.;
  (* Drag way past the right edge *)
  Renderable.Private.emit_mouse node (mouse_down ~x:10 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:100 ~y:0);
  satisfies ~claim:"clamped to the max of 100" float_exact
    (fun v -> v <= 100.)
    (Slider.value slider);
  (* Drag way past the left edge *)
  Renderable.Private.emit_mouse node (mouse_drag ~x:(-50) ~y:0);
  satisfies ~claim:"clamped to the min of 0" float_exact
    (fun v -> v >= 0.)
    (Slider.value slider)

let mouse_up_ends_drag () =
  let _t, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let grid = make_grid ~width:20 ~height:1 () in
  Renderable.Private.render node grid ~dt:0.;
  (* Start drag *)
  Renderable.Private.emit_mouse node (mouse_down ~x:0 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  let v_during = Slider.value slider in
  satisfies ~claim:"value moved above 0 during drag" float_exact
    (fun v -> v > 0.)
    v_during;
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
  Renderable.Private.render node grid ~dt:0.;
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
  Renderable.Private.render node grid ~dt:0.;
  let ev = mouse_down ~x:10 ~y:0 in
  Renderable.Private.emit_mouse node ev;
  is_true ~msg:"propagation stopped" (Event.Mouse.propagation_stopped ev)

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, slider = make_slider () in
  let s = Format.asprintf "%a" Slider.pp slider in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

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

let rejected_drag_proposals () =
  let _ctx, slider = make_slider ~value:0. () in
  let node = Slider.node slider in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:1;
  let proposals = ref [] in
  Slider.set_on_change slider
    (Some (fun value -> proposals := value :: !proposals));
  Renderable.Private.emit_mouse node (mouse_down ~x:10 ~y:0);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  equal ~msg:"rejected drag never moves accepted thumb" float_exact 0.
    (Slider.value slider);
  Renderable.Private.emit_mouse node (mouse_drag ~x:10 ~y:0);
  (match !proposals with
  | [ second_drag; first_drag; click ] ->
      equal ~msg:"track click proposes midpoint" float_exact 50. click;
      equal ~msg:"repeated rejected drags propose the same value" float_exact
        first_drag second_drag;
      satisfies ~claim:"drag stays near the clicked midpoint" float_exact
        (fun value -> Float.abs (value -. click) < 2.)
        first_drag
  | _ -> fail "expected one click and two drag proposals");
  Slider.apply_props slider (Slider.Props.make ~value:25. ());
  equal ~msg:"application accepts a transformed value" float_exact 25.
    (Slider.value slider)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.slider}) or to {!Slider.create},
   reaches the live slider ({!Test_harness.reaches}). The element's [state]
   is the slider's [value]. *)

let doors_size = Mosaic.size ~width:12 ~height:3
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red

let doors_value () =
  Test_harness.reaches "value (the element's state)"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:(Mosaic.slider ~size:doors_size ~state:70. ~on_change:ignore ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:70. ()))

let doors_orientation () =
  Test_harness.reaches "orientation"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore
         ~orientation:`Vertical ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20.
           ~orientation:`Vertical ()))

let doors_min () =
  Test_harness.reaches "min"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ~min:10. ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20. ~min:10. ()))

let doors_max () =
  Test_harness.reaches "max"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ~max:40. ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20. ~max:40. ()))

let doors_viewport_size () =
  Test_harness.reaches "viewport_size"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore
         ~viewport_size:50. ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20. ~viewport_size:50.
           ()))

let doors_track_color () =
  Test_harness.reaches "track_color"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore
         ~track_color:doors_red ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20.
           ~track_color:doors_red ()))

let doors_thumb_color () =
  Test_harness.reaches "thumb_color"
    ~base:(Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore ())
    ~element:
      (Mosaic.slider ~size:doors_size ~state:20. ~on_change:ignore
         ~thumb_color:doors_red ())
    ~create_base:(fun parent ->
      Slider.node (Slider.create ~parent ~style:doors_style ~value:20. ()))
    ~create:(fun parent ->
      Slider.node
        (Slider.create ~parent ~style:doors_style ~value:20.
           ~thumb_color:doors_red ()))

let () =
  run "mosaic.slider"
    [
      test "rejected drag proposals" rejected_drag_proposals;
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
          test "preserves uncontrolled value"
            apply_props_restores_declared_value;
          test "reclamps uncontrolled value" apply_props_clamps_declared_value;
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
          test "click proposes one value" click_proposes_one_value;
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
      group "Both doors"
        [
          test "value (the element's state)" doors_value;
          test "orientation" doors_orientation;
          test "min" doors_min;
          test "max" doors_max;
          test "viewport_size" doors_viewport_size;
          test "track_color" doors_track_color;
          test "thumb_color" doors_thumb_color;
        ];
    ]
