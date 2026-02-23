open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let sample_items =
  Select.
    [
      { label = "Alpha"; description = Some "First item" };
      { label = "Beta"; description = Some "Second item" };
      { label = "Gamma"; description = None };
      { label = "Delta"; description = Some "Fourth item" };
      { label = "Epsilon"; description = Some "Fifth item" };
    ]

let make_select ?options ?selected_index ?wrap_selection ?show_description
    ?show_scroll_indicator ?item_spacing ?fast_scroll_step () =
  let t = make_ctx () in
  let root = make_root t in
  let sel =
    Select.create ~parent:root ?options ?selected_index ?wrap_selection
      ?show_description ?show_scroll_indicator ?item_spacing ?fast_scroll_step
      ()
  in
  (t, sel)

let make_key ?(shift = false) key : Input.Key.event =
  {
    key;
    modifier =
      {
        ctrl = false;
        alt = false;
        shift;
        super = false;
        hyper = false;
        meta = false;
        caps_lock = false;
        num_lock = false;
      };
    event_type = Press;
    associated_text = "";
    shifted_key = None;
    base_key = None;
  }

let emit_key sel key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Select.node sel) ev

let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })

let emit_mouse sel ev = Renderable.Private.emit_mouse (Select.node sel) ev

let with_layout sel ~width ~height =
  layout_node (Select.node sel) ~x:0 ~y:0 ~width ~height

(* ── Props ── *)

let props_defaults () =
  let p = Select.Props.default in
  is_true ~msg:"equal to make()" (Select.Props.equal p (Select.Props.make ()))

let props_equal_identical () =
  let a = Select.Props.make () in
  let b = Select.Props.make () in
  is_true ~msg:"equal" (Select.Props.equal a b)

let props_detects_options_diff () =
  let a = Select.Props.make ~options:sample_items () in
  let b = Select.Props.make () in
  is_false ~msg:"different" (Select.Props.equal a b)

let props_detects_selected_index_diff () =
  let a = Select.Props.make ~selected_index:0 () in
  let b = Select.Props.make ~selected_index:1 () in
  is_false ~msg:"different" (Select.Props.equal a b)

let props_detects_wrap_diff () =
  let a = Select.Props.make ~wrap_selection:true () in
  let b = Select.Props.make () in
  is_false ~msg:"different" (Select.Props.equal a b)

let props_detects_color_diff () =
  let a = Select.Props.make ~selected_background:Ansi.Color.red () in
  let b = Select.Props.make () in
  is_false ~msg:"different" (Select.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, sel = make_select ~options:sample_items () in
  let node = Select.node sel in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_is_focusable () =
  let _t, sel = make_select () in
  is_true ~msg:"focusable" (Renderable.focusable (Select.node sel))

let create_is_buffered () =
  let _t, sel = make_select () in
  is_true ~msg:"buffered" (Renderable.buffered (Select.node sel))

let create_clamps_initial_index () =
  let _t, sel = make_select ~options:sample_items ~selected_index:100 () in
  equal ~msg:"clamped" int 4 (Select.selected_index sel)

let create_empty_list_index_zero () =
  let _t, sel = make_select ~selected_index:5 () in
  equal ~msg:"zero" int 0 (Select.selected_index sel)

(* ── Selection ── *)

let set_selected_index_clamps () =
  let _t, sel = make_select ~options:sample_items () in
  Select.set_selected_index sel 100;
  equal ~msg:"clamped high" int 4 (Select.selected_index sel);
  Select.set_selected_index sel (-5);
  equal ~msg:"clamped low" int 0 (Select.selected_index sel)

let set_selected_index_fires_on_change () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.set_selected_index sel 2;
  equal ~msg:"fired" (list int) [ 2 ] !log

let set_selected_index_noop_same () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.set_selected_index sel 0;
  equal ~msg:"no fire" (list int) [] !log

let selected_item_returns_item () =
  let _t, sel = make_select ~options:sample_items ~selected_index:1 () in
  match Select.selected_item sel with
  | Some it -> equal ~msg:"label" string "Beta" it.label
  | None -> fail "expected item"

let selected_item_empty_list () =
  let _t, sel = make_select () in
  match Select.selected_item sel with
  | Some _ -> fail "expected None"
  | None -> ()

(* ── Navigation ── *)

let move_down_basic () =
  let _t, sel = make_select ~options:sample_items () in
  emit_key sel (make_key Down);
  equal ~msg:"index" int 1 (Select.selected_index sel)

let move_up_basic () =
  let _t, sel = make_select ~options:sample_items ~selected_index:2 () in
  emit_key sel (make_key Up);
  equal ~msg:"index" int 1 (Select.selected_index sel)

let move_down_j () =
  let _t, sel = make_select ~options:sample_items () in
  emit_key sel (make_key (Char (Uchar.of_char 'j')));
  equal ~msg:"index" int 1 (Select.selected_index sel)

let move_up_k () =
  let _t, sel = make_select ~options:sample_items ~selected_index:2 () in
  emit_key sel (make_key (Char (Uchar.of_char 'k')));
  equal ~msg:"index" int 1 (Select.selected_index sel)

let move_down_no_wrap () =
  let _t, sel = make_select ~options:sample_items ~selected_index:4 () in
  emit_key sel (make_key Down);
  equal ~msg:"stays at end" int 4 (Select.selected_index sel)

let move_up_no_wrap () =
  let _t, sel = make_select ~options:sample_items ~selected_index:0 () in
  emit_key sel (make_key Up);
  equal ~msg:"stays at start" int 0 (Select.selected_index sel)

let move_down_wrap () =
  let _t, sel =
    make_select ~options:sample_items ~selected_index:4 ~wrap_selection:true ()
  in
  emit_key sel (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Select.selected_index sel)

let move_up_wrap () =
  let _t, sel =
    make_select ~options:sample_items ~selected_index:0 ~wrap_selection:true ()
  in
  emit_key sel (make_key Up);
  equal ~msg:"wraps to end" int 4 (Select.selected_index sel)

let fast_scroll_down () =
  let _t, sel = make_select ~options:sample_items ~fast_scroll_step:3 () in
  emit_key sel (make_key ~shift:true Down);
  equal ~msg:"jumped" int 3 (Select.selected_index sel)

let fast_scroll_up () =
  let _t, sel =
    make_select ~options:sample_items ~selected_index:4 ~fast_scroll_step:3 ()
  in
  emit_key sel (make_key ~shift:true Up);
  equal ~msg:"jumped" int 1 (Select.selected_index sel)

let enter_fires_on_activate () =
  let _t, sel = make_select ~options:sample_items ~selected_index:2 () in
  let log = ref [] in
  Select.set_on_activate sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key Enter);
  equal ~msg:"activated" (list int) [ 2 ] !log

let kp_enter_fires_on_activate () =
  let _t, sel = make_select ~options:sample_items ~selected_index:1 () in
  let log = ref [] in
  Select.set_on_activate sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key KP_enter);
  equal ~msg:"activated" (list int) [ 1 ] !log

let on_change_fires_on_key_navigation () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key Down);
  equal ~msg:"fired" (list int) [ 1 ] !log

let on_activate_empty_list () =
  let _t, sel = make_select () in
  let fired = ref false in
  Select.set_on_activate sel (Some (fun _ -> fired := true));
  emit_key sel (make_key Enter);
  is_false ~msg:"not fired" !fired

let unhandled_key_ignored () =
  let _t, sel = make_select ~options:sample_items ~selected_index:2 () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key (Char (Uchar.of_char 'a')));
  equal ~msg:"no change" (list int) [] !log;
  equal ~msg:"index unchanged" int 2 (Select.selected_index sel)

let navigation_on_empty_list () =
  let _t, sel = make_select () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key Down);
  emit_key sel (make_key Up);
  equal ~msg:"no callbacks" (list int) [] !log;
  equal ~msg:"index zero" int 0 (Select.selected_index sel)

let single_option_navigation () =
  let single = Select.[ { label = "Only"; description = None } ] in
  let _t, sel = make_select ~options:single () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  emit_key sel (make_key Down);
  emit_key sel (make_key Up);
  equal ~msg:"no callbacks" (list int) [] !log;
  equal ~msg:"stays at 0" int 0 (Select.selected_index sel)

let fast_scroll_clamps_past_end () =
  let _t, sel = make_select ~options:sample_items ~fast_scroll_step:10 () in
  emit_key sel (make_key ~shift:true Down);
  equal ~msg:"clamped to last" int 4 (Select.selected_index sel)

let fast_scroll_clamps_past_start () =
  let _t, sel =
    make_select ~options:sample_items ~selected_index:4 ~fast_scroll_step:10 ()
  in
  emit_key sel (make_key ~shift:true Up);
  equal ~msg:"clamped to 0" int 0 (Select.selected_index sel)

(* ── Mouse ── *)

let mouse_click_selects_item () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  (* show_description=true so lines_per_item=2; click at y=2 -> item 1 *)
  emit_mouse sel (mouse_down ~x:5 ~y:2);
  equal ~msg:"selected" int 1 (Select.selected_index sel)

let mouse_click_fires_on_change () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  emit_mouse sel (mouse_down ~x:5 ~y:4);
  equal ~msg:"fired" (list int) [ 2 ] !log

let mouse_click_same_item_noop () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  (* Click item 0 which is already selected *)
  emit_mouse sel (mouse_down ~x:5 ~y:0);
  equal ~msg:"no fire" (list int) [] !log

let mouse_click_beyond_items_ignored () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  (* Click at y=18 with lines_per_item=2 -> index 9, beyond 5 items *)
  emit_mouse sel (mouse_down ~x:5 ~y:18);
  equal ~msg:"no fire" (list int) [] !log

let mouse_scroll_down_moves () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  emit_mouse sel (mouse_scroll_down ~x:5 ~y:5);
  equal ~msg:"moved down" int 1 (Select.selected_index sel)

let mouse_scroll_up_moves () =
  let _t, sel = make_select ~options:sample_items ~selected_index:3 () in
  with_layout sel ~width:30 ~height:20;
  emit_mouse sel (mouse_scroll_up ~x:5 ~y:5);
  equal ~msg:"moved up" int 2 (Select.selected_index sel)

let mouse_click_stops_propagation () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout sel ~width:30 ~height:20;
  let ev = mouse_down ~x:5 ~y:2 in
  emit_mouse sel ev;
  is_true ~msg:"propagation stopped" (Event.Mouse.propagation_stopped ev)

(* ── Options ── *)

let set_options_replaces () =
  let _t, sel = make_select ~options:sample_items () in
  let new_opts =
    Select.
      [
        { label = "One"; description = None };
        { label = "Two"; description = None };
      ]
  in
  Select.set_options sel new_opts;
  equal ~msg:"count" int 2 (List.length (Select.options sel))

let set_options_clamps_index () =
  let _t, sel = make_select ~options:sample_items ~selected_index:4 () in
  Select.set_options sel [ Select.{ label = "Only"; description = None } ];
  equal ~msg:"clamped" int 0 (Select.selected_index sel)

let set_options_empty () =
  let _t, sel = make_select ~options:sample_items () in
  Select.set_options sel [];
  equal ~msg:"zero" int 0 (Select.selected_index sel);
  match Select.selected_item sel with
  | Some _ -> fail "expected None"
  | None -> ()

let set_options_preserves_valid_index () =
  let _t, sel = make_select ~options:sample_items ~selected_index:1 () in
  let extended =
    sample_items
    @ Select.[ { label = "Zeta"; description = Some "Sixth item" } ]
  in
  Select.set_options sel extended;
  equal ~msg:"preserved" int 1 (Select.selected_index sel)

let set_options_does_not_fire_on_change () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.set_options sel
    Select.
      [
        { label = "One"; description = None };
        { label = "Two"; description = None };
      ];
  equal ~msg:"no fire" (list int) [] !log

(* ── Setter No-ops ── *)

let set_background_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_background sel (Ansi.Color.of_rgba 0 0 0 0);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_wrap_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_wrap_selection sel false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_show_description_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_show_description sel true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_item_spacing_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_item_spacing sel 0;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_text_color_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_text_color sel (Ansi.Color.of_rgb 255 255 255);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_focused_background_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_focused_background sel (Ansi.Color.of_rgb 26 26 26);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_selected_text_color_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_selected_text_color sel (Ansi.Color.of_rgb 255 255 0);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_fast_scroll_step_noop () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_fast_scroll_step sel 5;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── Setter Positive ── *)

let set_show_description_toggle () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_show_description sel false;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_show_scroll_indicator_toggle () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_show_scroll_indicator sel true;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_wrap_selection_enables_wrapping () =
  let _t, sel = make_select ~options:sample_items ~selected_index:4 () in
  Select.set_wrap_selection sel true;
  emit_key sel (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Select.selected_index sel)

let set_item_spacing_schedules () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_item_spacing sel 2;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_fast_scroll_step_changes_behavior () =
  let _t, sel = make_select ~options:sample_items () in
  Select.set_fast_scroll_step sel 2;
  emit_key sel (make_key ~shift:true Down);
  equal ~msg:"jumped by 2" int 2 (Select.selected_index sel)

(* ── apply_props ── *)

let apply_props_updates () =
  let t, sel = make_select ~options:sample_items () in
  let props =
    Select.Props.make ~options:sample_items ~selected_index:3
      ~wrap_selection:true ()
  in
  let before = !(t.schedule_count) in
  Select.apply_props sel props;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  equal ~msg:"index applied" int 3 (Select.selected_index sel)

let apply_props_same_options_no_extra_render () =
  let t, sel = make_select ~options:sample_items () in
  let props = Select.Props.make ~options:sample_items () in
  Select.apply_props sel props;
  let before = !(t.schedule_count) in
  Select.apply_props sel props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

(* ── Runner ── *)

let () =
  run "mosaic.select"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects options difference" props_detects_options_diff;
          test "detects selected_index difference"
            props_detects_selected_index_diff;
          test "detects wrap difference" props_detects_wrap_diff;
          test "detects color difference" props_detects_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "is focusable" create_is_focusable;
          test "is buffered" create_is_buffered;
          test "clamps initial index" create_clamps_initial_index;
          test "empty list index zero" create_empty_list_index_zero;
        ];
      group "Selection"
        [
          test "set_selected_index clamps" set_selected_index_clamps;
          test "fires on_change" set_selected_index_fires_on_change;
          test "no-op on same index" set_selected_index_noop_same;
          test "selected_item returns item" selected_item_returns_item;
          test "selected_item empty list" selected_item_empty_list;
        ];
      group "Navigation"
        [
          test "move down" move_down_basic;
          test "move up" move_up_basic;
          test "j moves down" move_down_j;
          test "k moves up" move_up_k;
          test "no wrap at end" move_down_no_wrap;
          test "no wrap at start" move_up_no_wrap;
          test "wrap at end" move_down_wrap;
          test "wrap at start" move_up_wrap;
          test "fast scroll down" fast_scroll_down;
          test "fast scroll up" fast_scroll_up;
          test "enter fires on_activate" enter_fires_on_activate;
          test "KP_enter fires on_activate" kp_enter_fires_on_activate;
          test "on_change fires on key navigation"
            on_change_fires_on_key_navigation;
          test "on_activate on empty list" on_activate_empty_list;
          test "unhandled key ignored" unhandled_key_ignored;
          test "navigation on empty list" navigation_on_empty_list;
          test "single option navigation" single_option_navigation;
          test "fast scroll clamps past end" fast_scroll_clamps_past_end;
          test "fast scroll clamps past start" fast_scroll_clamps_past_start;
        ];
      group "Mouse"
        [
          test "click selects item" mouse_click_selects_item;
          test "click fires on_change" mouse_click_fires_on_change;
          test "click same item no-op" mouse_click_same_item_noop;
          test "click beyond items ignored" mouse_click_beyond_items_ignored;
          test "scroll down moves" mouse_scroll_down_moves;
          test "scroll up moves" mouse_scroll_up_moves;
          test "click stops propagation" mouse_click_stops_propagation;
        ];
      group "Options"
        [
          test "set_options replaces" set_options_replaces;
          test "set_options clamps index" set_options_clamps_index;
          test "set_options empty" set_options_empty;
          test "set_options preserves valid index"
            set_options_preserves_valid_index;
          test "set_options does not fire on_change"
            set_options_does_not_fire_on_change;
        ];
      group "Setter no-ops"
        [
          test "set_background no-op" set_background_noop;
          test "set_wrap_selection no-op" set_wrap_noop;
          test "set_show_description no-op" set_show_description_noop;
          test "set_item_spacing no-op" set_item_spacing_noop;
          test "set_text_color no-op" set_text_color_noop;
          test "set_focused_background no-op" set_focused_background_noop;
          test "set_selected_text_color no-op" set_selected_text_color_noop;
          test "set_fast_scroll_step no-op" set_fast_scroll_step_noop;
        ];
      group "Setter positive"
        [
          test "toggle show_description" set_show_description_toggle;
          test "toggle show_scroll_indicator" set_show_scroll_indicator_toggle;
          test "wrap_selection enables wrapping"
            set_wrap_selection_enables_wrapping;
          test "item_spacing schedules render" set_item_spacing_schedules;
          test "fast_scroll_step changes behavior"
            set_fast_scroll_step_changes_behavior;
        ];
      group "apply_props"
        [
          test "updates all properties" apply_props_updates;
          test "same options no extra render"
            apply_props_same_options_no_extra_render;
        ];
    ]
