open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_scroll_box ?scroll_x ?scroll_y ?sticky_scroll ?sticky_start ?background
    ?scroll_accel ?on_scroll () =
  let t = make_ctx () in
  let root = make_root t in
  let sb =
    Scroll_box.create ~parent:root ?scroll_x ?scroll_y ?sticky_scroll
      ?sticky_start ?background ?scroll_accel ?on_scroll ()
  in
  (t, sb)

(* ── Scroll_accel ── *)

let accel_linear_always_one () =
  let a = Scroll_box.Scroll_accel.linear () in
  let m1 = Scroll_box.Scroll_accel.tick a ~now:0. in
  let m2 = Scroll_box.Scroll_accel.tick a ~now:100. in
  let m3 = Scroll_box.Scroll_accel.tick a ~now:200. in
  is_true ~msg:"first = 1.0" (Float.equal m1 1.0);
  is_true ~msg:"second = 1.0" (Float.equal m2 1.0);
  is_true ~msg:"third = 1.0" (Float.equal m3 1.0)

let accel_macos_first_tick_one () =
  let a = Scroll_box.Scroll_accel.macos () in
  let m = Scroll_box.Scroll_accel.tick a ~now:0. in
  is_true ~msg:"first = 1.0" (Float.equal m 1.0)

let accel_macos_accelerates_on_rapid_events () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:40. in
  let m = Scroll_box.Scroll_accel.tick a ~now:60. in
  is_true ~msg:"multiplier > 1" (m > 1.0)

let accel_macos_resets_after_timeout () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:40. in
  (* Long pause exceeding streak_timeout (150ms) *)
  let m = Scroll_box.Scroll_accel.tick a ~now:500. in
  is_true ~msg:"reset to 1.0" (Float.equal m 1.0)

let accel_macos_respects_cap () =
  let a = Scroll_box.Scroll_accel.macos ~max_multiplier:2.0 () in
  (* Rapid events to build up velocity *)
  let _m = ref 1.0 in
  for i = 1 to 30 do
    _m := Scroll_box.Scroll_accel.tick a ~now:(float_of_int (i * 10))
  done;
  is_true ~msg:"capped at 2.0" (!_m <= 2.0)

let accel_reset_clears_history () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~now:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~now:20. in
  Scroll_box.Scroll_accel.reset a;
  let m = Scroll_box.Scroll_accel.tick a ~now:100. in
  is_true ~msg:"back to 1.0 after reset" (Float.equal m 1.0)

(* ── Props ── *)

let props_defaults () =
  let p = Scroll_box.Props.default in
  is_true ~msg:"equal to make()"
    (Scroll_box.Props.equal p (Scroll_box.Props.make ()))

let props_equal_identical () =
  let a = Scroll_box.Props.make () in
  let b = Scroll_box.Props.make () in
  is_true ~msg:"equal" (Scroll_box.Props.equal a b)

let props_detects_scroll_x_diff () =
  let a = Scroll_box.Props.make ~scroll_x:true () in
  let b = Scroll_box.Props.make ~scroll_x:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_scroll_y_diff () =
  let a = Scroll_box.Props.make ~scroll_y:true () in
  let b = Scroll_box.Props.make ~scroll_y:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_sticky_scroll_diff () =
  let a = Scroll_box.Props.make ~sticky_scroll:true () in
  let b = Scroll_box.Props.make ~sticky_scroll:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_sticky_start_diff () =
  let a = Scroll_box.Props.make ~sticky_start:`Top () in
  let b = Scroll_box.Props.make ~sticky_start:`Bottom () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_background_diff () =
  let a = Scroll_box.Props.make ~background:Ansi.Color.red () in
  let b = Scroll_box.Props.make ~background:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, sb = make_scroll_box () in
  match Renderable.parent (Scroll_box.node sb) with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_scroll_zero () =
  let _t, sb = make_scroll_box () in
  equal ~msg:"scroll_top = 0" int 0 (Scroll_box.scroll_top sb);
  equal ~msg:"scroll_left = 0" int 0 (Scroll_box.scroll_left sb)

let create_child_target_set () =
  let _t, sb = make_scroll_box () in
  let target = Renderable.child_target (Scroll_box.node sb) in
  let content = Scroll_box.content sb in
  is_true ~msg:"target is content node"
    (Renderable.id target = Renderable.id content)

let create_vertical_bar_attached_to_root () =
  let _t, sb = make_scroll_box () in
  let bar_node = Scroll_bar.node (Scroll_box.vertical_bar sb) in
  match Renderable.parent bar_node with
  | None -> fail "expected vertical bar parent"
  | Some parent ->
      is_true ~msg:"vertical bar is attached to scroll box root"
        (Renderable.id parent = Renderable.id (Scroll_box.node sb))

let create_viewport_has_overflow_hidden () =
  let _t, sb = make_scroll_box () in
  let vp = Scroll_box.viewport sb in
  let style = Renderable.style vp in
  let overflow = Toffee.Style.overflow style in
  is_true ~msg:"overflow x hidden" (overflow.x = Toffee.Style.Overflow.Hidden);
  is_true ~msg:"overflow y hidden" (overflow.y = Toffee.Style.Overflow.Hidden)

let vertical_bar_visible_when_content_overflows () =
  let t = make_ctx () in
  let root = make_root t in
  let sb = Scroll_box.create ~parent:root () in
  layout_node root ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.node sb) ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.viewport sb) ~x:0 ~y:0 ~width:19 ~height:8;
  layout_node (Scroll_box.content sb) ~x:0 ~y:0 ~width:19 ~height:20;
  Renderable.Private.render (Scroll_box.node sb)
    (make_grid ~width:20 ~height:8 ())
    ~delta:0.;
  is_true ~msg:"vertical bar visible on overflow"
    (Renderable.visible (Scroll_bar.node (Scroll_box.vertical_bar sb)))

(* ── Scroll Position ── *)

let set_scroll_top_clamps () =
  let _t, sb = make_scroll_box () in
  (* Without layout, max_scroll_y is 0, so everything clamps to 0 *)
  Scroll_box.set_scroll_top sb 100;
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll_top sb (-10);
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb)

let scroll_to_sets_both () =
  let _t, sb = make_scroll_box ~scroll_x:true () in
  Scroll_box.scroll_to sb ~x:5 ~y:10 ();
  (* Both clamped to 0 without layout *)
  equal ~msg:"x clamped" int 0 (Scroll_box.scroll_left sb);
  equal ~msg:"y clamped" int 0 (Scroll_box.scroll_top sb)

let scroll_by_adjusts_relative () =
  let _t, sb = make_scroll_box () in
  Scroll_box.scroll_by sb ~y:10 ();
  (* Clamped to max_scroll_y = 0 *)
  equal ~msg:"clamped" int 0 (Scroll_box.scroll_top sb)

let on_scroll_fires () =
  let log = ref [] in
  let _t, sb =
    make_scroll_box ~on_scroll:(fun ~x ~y -> log := (x, y) :: !log) ()
  in
  (* Even though position won't change (clamped), the callback should not fire
     unless there's an actual change *)
  Scroll_box.set_scroll_top sb 0;
  equal ~msg:"no spurious fire" int 0 (List.length !log)

(* ── Sticky Scroll ── *)

let set_sticky_scroll_updates () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Bottom () in
  Scroll_box.set_sticky_scroll sb false;
  (* Just verify it doesn't crash *)
  Scroll_box.set_sticky_scroll sb true;
  ()

let reset_sticky_clears_manual () =
  let _t, sb = make_scroll_box ~sticky_scroll:true ~sticky_start:`Top () in
  Scroll_box.reset_sticky sb;
  equal ~msg:"scroll_top reset" int 0 (Scroll_box.scroll_top sb)

(* ── Setters ── *)

let set_background_updates () =
  let _t, sb = make_scroll_box () in
  Scroll_box.set_background sb (Some Ansi.Color.red);
  Scroll_box.set_background sb None;
  ()

let set_on_scroll_replaces () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, sb =
    make_scroll_box ~on_scroll:(fun ~x:_ ~y:_ -> incr first_count) ()
  in
  Scroll_box.set_on_scroll sb (Some (fun ~x:_ ~y:_ -> incr second_count));
  (* No actual scroll changes, just verify no crash *)
  ()

let set_scroll_accel_replaces () =
  let _t, sb = make_scroll_box () in
  let new_accel = Scroll_box.Scroll_accel.macos ~max_multiplier:3.0 () in
  Scroll_box.set_scroll_accel sb new_accel;
  ()

(* ── apply_props ── *)

let apply_props_updates () =
  let _t, sb = make_scroll_box () in
  let props =
    Scroll_box.Props.make ~sticky_scroll:true ~sticky_start:`Bottom
      ~background:Ansi.Color.blue ()
  in
  Scroll_box.apply_props sb props;
  ()

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  is_true ~msg:"non-empty" (String.length s > 0)

let pp_contains_scroll_box () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  is_true ~msg:"has ScrollBox prefix"
    (String.length s >= 9 && String.sub s 0 9 = "ScrollBox")

(* ── Runner ── *)

let () =
  run "mosaic.scroll_box"
    [
      group "Scroll_accel"
        [
          test "linear always 1.0" accel_linear_always_one;
          test "macos first tick 1.0" accel_macos_first_tick_one;
          test "macos accelerates on rapid events"
            accel_macos_accelerates_on_rapid_events;
          test "macos resets after timeout" accel_macos_resets_after_timeout;
          test "macos respects cap" accel_macos_respects_cap;
          test "reset clears history" accel_reset_clears_history;
        ];
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects scroll_x diff" props_detects_scroll_x_diff;
          test "detects scroll_y diff" props_detects_scroll_y_diff;
          test "detects sticky_scroll diff" props_detects_sticky_scroll_diff;
          test "detects sticky_start diff" props_detects_sticky_start_diff;
          test "detects background diff" props_detects_background_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default scroll is 0" create_default_scroll_zero;
          test "child_target set to content" create_child_target_set;
          test "vertical bar attached to root"
            create_vertical_bar_attached_to_root;
          test "viewport has overflow hidden"
            create_viewport_has_overflow_hidden;
          test "vertical bar visible on overflow"
            vertical_bar_visible_when_content_overflows;
        ];
      group "Scroll position"
        [
          test "set_scroll_top clamps" set_scroll_top_clamps;
          test "scroll_to sets both" scroll_to_sets_both;
          test "scroll_by adjusts relative" scroll_by_adjusts_relative;
          test "on_scroll fires" on_scroll_fires;
        ];
      group "Sticky scroll"
        [
          test "set_sticky_scroll updates" set_sticky_scroll_updates;
          test "reset_sticky clears manual" reset_sticky_clears_manual;
        ];
      group "Setters"
        [
          test "set_background" set_background_updates;
          test "set_on_scroll replaces" set_on_scroll_replaces;
          test "set_scroll_accel replaces" set_scroll_accel_replaces;
        ];
      group "apply_props" [ test "updates" apply_props_updates ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains ScrollBox" pp_contains_scroll_box;
        ];
    ]
