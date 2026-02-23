open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_scroll_bar ?orientation ?show_arrows ?track_color ?thumb_color
    ?arrow_fg ?arrow_bg ?on_change () =
  let t = make_ctx () in
  let root = make_root t in
  let bar =
    Scroll_bar.create ~parent:root ?orientation ?show_arrows ?track_color
      ?thumb_color ?arrow_fg ?arrow_bg ?on_change ()
  in
  (t, bar)

(* ── Props ── *)

let props_defaults () =
  let p = Scroll_bar.Props.default in
  is_true ~msg:"equal to make()"
    (Scroll_bar.Props.equal p (Scroll_bar.Props.make ()))

let props_equal_identical () =
  let a = Scroll_bar.Props.make () in
  let b = Scroll_bar.Props.make () in
  is_true ~msg:"equal" (Scroll_bar.Props.equal a b)

let props_detects_orientation_diff () =
  let a = Scroll_bar.Props.make ~orientation:`Horizontal () in
  let b = Scroll_bar.Props.make ~orientation:`Vertical () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_show_arrows_diff () =
  let a = Scroll_bar.Props.make ~show_arrows:true () in
  let b = Scroll_bar.Props.make ~show_arrows:false () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_track_color_diff () =
  let a = Scroll_bar.Props.make ~track_color:Ansi.Color.red () in
  let b = Scroll_bar.Props.make ~track_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_thumb_color_diff () =
  let a = Scroll_bar.Props.make ~thumb_color:Ansi.Color.red () in
  let b = Scroll_bar.Props.make ~thumb_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, bar = make_scroll_bar () in
  let node = Scroll_bar.node bar in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_position_zero () =
  let _t, bar = make_scroll_bar () in
  equal ~msg:"pos = 0" int 0 (Scroll_bar.scroll_position bar)

let create_default_sizes_zero () =
  let _t, bar = make_scroll_bar () in
  equal ~msg:"scroll_size = 0" int 0 (Scroll_bar.scroll_size bar);
  equal ~msg:"viewport_size = 0" int 0 (Scroll_bar.viewport_size bar)

(* ── Position Clamping ── *)

let set_position_clamps_to_range () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 50;
  equal ~msg:"pos = 50" int 50 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_position bar 200;
  equal ~msg:"clamped to 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_position bar (-10);
  equal ~msg:"clamped to 0" int 0 (Scroll_bar.scroll_position bar)

let set_position_fires_on_change () =
  let changed = ref None in
  let _t, bar = make_scroll_bar ~on_change:(fun v -> changed := Some v) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 42;
  match !changed with
  | Some v -> equal ~msg:"value = 42" int 42 v
  | None -> fail "on_change not fired"

let set_position_noop_same_value () =
  let count = ref 0 in
  let _t, bar = make_scroll_bar ~on_change:(fun _ -> incr count) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 10;
  equal ~msg:"fired once" int 1 !count;
  Scroll_bar.set_scroll_position bar 10;
  equal ~msg:"still once" int 1 !count

(* ── Scroll Size / Viewport Size ── *)

let set_scroll_size_reclamps_position () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 80;
  equal ~msg:"pos = 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_size bar 50;
  equal ~msg:"clamped to 30" int 30 (Scroll_bar.scroll_position bar)

let set_viewport_size_reclamps_position () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 80;
  Scroll_bar.set_viewport_size bar 50;
  equal ~msg:"clamped to 50" int 50 (Scroll_bar.scroll_position bar)

(* ── scroll_by ── *)

let scroll_by_absolute () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 10. ~unit:`Absolute;
  equal ~msg:"pos = 10" int 10 (Scroll_bar.scroll_position bar)

let scroll_by_viewport () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 0.5 ~unit:`Viewport;
  equal ~msg:"pos = 10" int 10 (Scroll_bar.scroll_position bar)

let scroll_by_content () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 0.5 ~unit:`Content;
  equal ~msg:"pos = 50" int 50 (Scroll_bar.scroll_position bar)

let scroll_by_step_default () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 5. ~unit:`Step;
  equal ~msg:"pos = 5" int 5 (Scroll_bar.scroll_position bar)

let scroll_by_step_custom () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_step bar (Some 3);
  Scroll_bar.scroll_by bar 2. ~unit:`Step;
  equal ~msg:"pos = 6" int 6 (Scroll_bar.scroll_position bar)

let scroll_by_clamps () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 200. ~unit:`Absolute;
  equal ~msg:"clamped to 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.scroll_by bar (-200.) ~unit:`Absolute;
  equal ~msg:"clamped to 0" int 0 (Scroll_bar.scroll_position bar)

(* ── Auto-visibility ── *)

let auto_visibility_hidden_when_no_overflow () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  is_false ~msg:"hidden" (Renderable.visible (Scroll_bar.node bar))

let auto_visibility_visible_when_overflow () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  is_true ~msg:"visible" (Renderable.visible (Scroll_bar.node bar))

let manual_visibility_overrides () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_visible_override bar true;
  is_true ~msg:"forced visible" (Renderable.visible (Scroll_bar.node bar))

let reset_visibility_restores_auto () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_visible_override bar true;
  is_true ~msg:"forced visible" (Renderable.visible (Scroll_bar.node bar));
  Scroll_bar.reset_visibility_control bar;
  is_false ~msg:"auto hidden" (Renderable.visible (Scroll_bar.node bar))

(* ── Show Arrows ── *)

let show_arrows_default_false () =
  let _t, bar = make_scroll_bar () in
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  (* With show_arrows=false, only the slider should be visible *)
  equal ~msg:"1 visible child (slider)" int 1 visible_count

let show_arrows_true () =
  let _t, bar = make_scroll_bar ~show_arrows:true () in
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  (* With show_arrows=true, slider + 2 arrows = 3 *)
  equal ~msg:"3 visible children" int 3 visible_count

let set_show_arrows_toggles () =
  let _t, bar = make_scroll_bar () in
  let count_visible () =
    let children = Renderable.Private.children_z (Scroll_bar.node bar) in
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  equal ~msg:"1 initially" int 1 (count_visible ());
  Scroll_bar.set_show_arrows bar true;
  equal ~msg:"3 after show" int 3 (count_visible ());
  Scroll_bar.set_show_arrows bar false;
  equal ~msg:"1 after hide" int 1 (count_visible ())

(* ── apply_props ── *)

let apply_props_updates_appearance () =
  let _t, bar = make_scroll_bar () in
  let props =
    Scroll_bar.Props.make ~show_arrows:true ~track_color:Ansi.Color.red
      ~thumb_color:Ansi.Color.blue ()
  in
  Scroll_bar.apply_props bar props;
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  equal ~msg:"arrows visible" int 3 visible_count

(* ── set_on_change ── *)

let set_on_change_replaces_callback () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, bar = make_scroll_bar ~on_change:(fun _ -> incr first_count) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 10;
  equal ~msg:"first fired" int 1 !first_count;
  Scroll_bar.set_on_change bar (Some (fun _ -> incr second_count));
  Scroll_bar.set_scroll_position bar 20;
  equal ~msg:"first not again" int 1 !first_count;
  equal ~msg:"second fired" int 1 !second_count

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, bar = make_scroll_bar () in
  let s = Format.asprintf "%a" Scroll_bar.pp bar in
  is_true ~msg:"non-empty" (String.length s > 0)

let pp_contains_scroll_bar () =
  let _t, bar = make_scroll_bar () in
  let s = Format.asprintf "%a" Scroll_bar.pp bar in
  is_true ~msg:"has ScrollBar prefix"
    (String.length s >= 9 && String.sub s 0 9 = "ScrollBar")

(* ── Runner ── *)

let () =
  run "mosaic.scroll_bar"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects orientation diff" props_detects_orientation_diff;
          test "detects show_arrows diff" props_detects_show_arrows_diff;
          test "detects track_color diff" props_detects_track_color_diff;
          test "detects thumb_color diff" props_detects_thumb_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default position is 0" create_default_position_zero;
          test "default sizes are 0" create_default_sizes_zero;
        ];
      group "Position clamping"
        [
          test "clamps to range" set_position_clamps_to_range;
          test "fires on_change" set_position_fires_on_change;
          test "no-op same value" set_position_noop_same_value;
        ];
      group "Scroll size / viewport size"
        [
          test "set_scroll_size reclamps" set_scroll_size_reclamps_position;
          test "set_viewport_size reclamps" set_viewport_size_reclamps_position;
        ];
      group "scroll_by"
        [
          test "absolute" scroll_by_absolute;
          test "viewport" scroll_by_viewport;
          test "content" scroll_by_content;
          test "step default" scroll_by_step_default;
          test "step custom" scroll_by_step_custom;
          test "clamps" scroll_by_clamps;
        ];
      group "Auto-visibility"
        [
          test "hidden when no overflow" auto_visibility_hidden_when_no_overflow;
          test "visible when overflow" auto_visibility_visible_when_overflow;
          test "manual override" manual_visibility_overrides;
          test "reset restores auto" reset_visibility_restores_auto;
        ];
      group "Show arrows"
        [
          test "default false" show_arrows_default_false;
          test "true shows 3 children" show_arrows_true;
          test "toggle" set_show_arrows_toggles;
        ];
      group "apply_props"
        [ test "updates appearance" apply_props_updates_appearance ];
      group "set_on_change"
        [ test "replaces callback" set_on_change_replaces_callback ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains ScrollBar" pp_contains_scroll_bar;
        ];
    ]
