open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let sample_items =
  Tab_select.
    [
      { label = "Home"; description = "Main dashboard" };
      { label = "Files"; description = "Browse files" };
      { label = "Settings"; description = "Configure options" };
      { label = "Help"; description = "Documentation" };
      { label = "About"; description = "Version info" };
    ]

let make_tab_select ?options ?selected ?tab_width ?wrap_selection
    ?show_description ?show_underline ?show_scroll_arrows () =
  let t = make_ctx () in
  let root = make_root t in
  let options = match options with Some o -> o | None -> sample_items in
  let ts =
    Tab_select.create ~parent:root ~options ?selected ?tab_width ?wrap_selection
      ?show_description ?show_underline ?show_scroll_arrows ()
  in
  (t, ts)

let make_key key : Input.Key.event =
  {
    key;
    modifier =
      {
        ctrl = false;
        alt = false;
        shift = false;
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

let emit_key ts key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Tab_select.node ts) ev

(* ── Props ── *)

let props_defaults () =
  let p = Tab_select.Props.default in
  is_true ~msg:"equal to make()"
    (Tab_select.Props.equal p (Tab_select.Props.make ~options:[] ()))

let props_equal_identical () =
  let a = Tab_select.Props.make ~options:[] () in
  let b = Tab_select.Props.make ~options:[] () in
  is_true ~msg:"equal" (Tab_select.Props.equal a b)

let props_detects_options_diff () =
  let a = Tab_select.Props.make ~options:sample_items () in
  let b = Tab_select.Props.make ~options:[] () in
  is_false ~msg:"different" (Tab_select.Props.equal a b)

let props_detects_selected_diff () =
  let a = Tab_select.Props.make ~options:[] ~selected:0 () in
  let b = Tab_select.Props.make ~options:[] ~selected:1 () in
  is_false ~msg:"different" (Tab_select.Props.equal a b)

let props_detects_wrap_diff () =
  let a = Tab_select.Props.make ~options:[] ~wrap_selection:true () in
  let b = Tab_select.Props.make ~options:[] () in
  is_false ~msg:"different" (Tab_select.Props.equal a b)

let props_detects_color_diff () =
  let a =
    Tab_select.Props.make ~options:[] ~selected_background:Ansi.Color.red ()
  in
  let b = Tab_select.Props.make ~options:[] () in
  is_false ~msg:"different" (Tab_select.Props.equal a b)

let props_detects_selected_description_color_diff () =
  let a =
    Tab_select.Props.make ~options:[]
      ~selected_description_color:(Ansi.Color.of_rgb 255 0 0)
      ()
  in
  let b = Tab_select.Props.make ~options:[] () in
  is_false ~msg:"different" (Tab_select.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, ts = make_tab_select () in
  let node = Tab_select.node ts in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_is_focusable () =
  let _t, ts = make_tab_select () in
  is_true ~msg:"focusable" (Renderable.focusable (Tab_select.node ts))

let create_is_buffered () =
  let _t, ts = make_tab_select () in
  is_true ~msg:"buffered" (Renderable.buffered (Tab_select.node ts))

let create_clamps_initial_index () =
  let _t, ts = make_tab_select ~selected:100 () in
  equal ~msg:"clamped" int 4 (Tab_select.selected_index ts)

let create_empty_list_index_zero () =
  let _t, ts = make_tab_select ~options:[] ~selected:5 () in
  equal ~msg:"zero" int 0 (Tab_select.selected_index ts)

(* ── Selection ── *)

let set_selected_clamps () =
  let _t, ts = make_tab_select () in
  Tab_select.set_selected ts 100;
  equal ~msg:"clamped high" int 4 (Tab_select.selected_index ts);
  Tab_select.set_selected ts (-5);
  equal ~msg:"clamped low" int 0 (Tab_select.selected_index ts)

let set_selected_fires_on_change () =
  let _t, ts = make_tab_select () in
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  Tab_select.set_selected ts 2;
  equal ~msg:"fired" (list int) [ 2 ] !log

let set_selected_noop_same () =
  let _t, ts = make_tab_select () in
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  Tab_select.set_selected ts 0;
  equal ~msg:"no fire" (list int) [] !log

let selected_item_returns_item () =
  let _t, ts = make_tab_select ~selected:1 () in
  match Tab_select.selected_item ts with
  | Some it -> equal ~msg:"label" string "Files" it.label
  | None -> fail "expected item"

let selected_item_empty_list () =
  let _t, ts = make_tab_select ~options:[] () in
  match Tab_select.selected_item ts with
  | Some _ -> fail "expected None"
  | None -> ()

(* ── Navigation ── *)

let move_right_basic () =
  let _t, ts = make_tab_select () in
  emit_key ts (make_key Right);
  equal ~msg:"index" int 1 (Tab_select.selected_index ts)

let move_left_basic () =
  let _t, ts = make_tab_select ~selected:2 () in
  emit_key ts (make_key Left);
  equal ~msg:"index" int 1 (Tab_select.selected_index ts)

let move_right_bracket () =
  let _t, ts = make_tab_select () in
  emit_key ts (make_key (Char (Uchar.of_char ']')));
  equal ~msg:"index" int 1 (Tab_select.selected_index ts)

let move_left_bracket () =
  let _t, ts = make_tab_select ~selected:2 () in
  emit_key ts (make_key (Char (Uchar.of_char '[')));
  equal ~msg:"index" int 1 (Tab_select.selected_index ts)

let move_right_no_wrap () =
  let _t, ts = make_tab_select ~selected:4 () in
  emit_key ts (make_key Right);
  equal ~msg:"stays at end" int 4 (Tab_select.selected_index ts)

let move_left_no_wrap () =
  let _t, ts = make_tab_select ~selected:0 () in
  emit_key ts (make_key Left);
  equal ~msg:"stays at start" int 0 (Tab_select.selected_index ts)

let move_right_wrap () =
  let _t, ts = make_tab_select ~selected:4 ~wrap_selection:true () in
  emit_key ts (make_key Right);
  equal ~msg:"wraps to 0" int 0 (Tab_select.selected_index ts)

let move_left_wrap () =
  let _t, ts = make_tab_select ~selected:0 ~wrap_selection:true () in
  emit_key ts (make_key Left);
  equal ~msg:"wraps to end" int 4 (Tab_select.selected_index ts)

let enter_fires_on_activate () =
  let _t, ts = make_tab_select ~selected:2 () in
  let log = ref [] in
  Tab_select.set_on_activate ts (Some (fun i -> log := i :: !log));
  emit_key ts (make_key Enter);
  equal ~msg:"activated" (list int) [ 2 ] !log

let navigation_fires_on_change () =
  let _t, ts = make_tab_select () in
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  emit_key ts (make_key Right);
  emit_key ts (make_key Right);
  equal ~msg:"two changes" (list int) [ 2; 1 ] !log

let navigation_syncs_props_selected () =
  let _t, ts = make_tab_select () in
  emit_key ts (make_key Right);
  emit_key ts (make_key Right);
  equal ~msg:"index" int 2 (Tab_select.selected_index ts);
  (* Verify that apply_props doesn't reset due to stale props.selected *)
  let props = Tab_select.Props.make ~options:sample_items ~selected:2 () in
  Tab_select.apply_props ts props;
  equal ~msg:"still 2 after apply_props" int 2 (Tab_select.selected_index ts)

(* ── Options ── *)

let set_options_replaces () =
  let _t, ts = make_tab_select () in
  let new_opts =
    Tab_select.
      [
        { label = "One"; description = "" }; { label = "Two"; description = "" };
      ]
  in
  Tab_select.set_options ts new_opts;
  equal ~msg:"count" int 2 (List.length (Tab_select.options ts))

let set_options_clamps_index () =
  let _t, ts = make_tab_select ~selected:4 () in
  Tab_select.set_options ts [ Tab_select.{ label = "Only"; description = "" } ];
  equal ~msg:"clamped" int 0 (Tab_select.selected_index ts)

let set_options_empty () =
  let _t, ts = make_tab_select () in
  Tab_select.set_options ts [];
  equal ~msg:"zero" int 0 (Tab_select.selected_index ts);
  match Tab_select.selected_item ts with
  | Some _ -> fail "expected None"
  | None -> ()

(* ── Setter No-ops ── *)

let set_background_noop () =
  let t, ts = make_tab_select () in
  let before = !(t.schedule_count) in
  Tab_select.set_background ts (Ansi.Color.of_rgba 0 0 0 0);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_wrap_noop () =
  let t, ts = make_tab_select () in
  let before = !(t.schedule_count) in
  Tab_select.set_wrap_selection ts false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_show_underline_noop () =
  let t, ts = make_tab_select () in
  let before = !(t.schedule_count) in
  Tab_select.set_show_underline ts true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_show_description_noop () =
  let t, ts = make_tab_select () in
  let before = !(t.schedule_count) in
  Tab_select.set_show_description ts false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── apply_props ── *)

let apply_props_updates () =
  let t, ts = make_tab_select () in
  let props =
    Tab_select.Props.make ~options:sample_items ~selected:3 ~wrap_selection:true
      ()
  in
  let before = !(t.schedule_count) in
  Tab_select.apply_props ts props;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  equal ~msg:"index applied" int 3 (Tab_select.selected_index ts)

(* ── Runner ── *)

let () =
  run "mosaic.tab_select"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects options difference" props_detects_options_diff;
          test "detects selected difference" props_detects_selected_diff;
          test "detects wrap difference" props_detects_wrap_diff;
          test "detects color difference" props_detects_color_diff;
          test "detects selected_description_color difference"
            props_detects_selected_description_color_diff;
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
          test "set_selected clamps" set_selected_clamps;
          test "fires on_change" set_selected_fires_on_change;
          test "no-op on same index" set_selected_noop_same;
          test "selected_item returns item" selected_item_returns_item;
          test "selected_item empty list" selected_item_empty_list;
        ];
      group "Navigation"
        [
          test "move right" move_right_basic;
          test "move left" move_left_basic;
          test "] moves right" move_right_bracket;
          test "[ moves left" move_left_bracket;
          test "no wrap at end" move_right_no_wrap;
          test "no wrap at start" move_left_no_wrap;
          test "wrap at end" move_right_wrap;
          test "wrap at start" move_left_wrap;
          test "enter fires on_activate" enter_fires_on_activate;
          test "navigation fires on_change" navigation_fires_on_change;
          test "navigation syncs props.selected" navigation_syncs_props_selected;
        ];
      group "Options"
        [
          test "set_options replaces" set_options_replaces;
          test "set_options clamps index" set_options_clamps_index;
          test "set_options empty" set_options_empty;
        ];
      group "Setter no-ops"
        [
          test "set_background no-op" set_background_noop;
          test "set_wrap_selection no-op" set_wrap_noop;
          test "set_show_underline no-op" set_show_underline_noop;
          test "set_show_description no-op" set_show_description_noop;
        ];
      group "apply_props" [ test "updates all properties" apply_props_updates ];
    ]
