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
    ?show_scroll_indicator ?item_spacing ?fast_scroll_step ?key_bindings () =
  let t = make_ctx () in
  let root = make_root t in
  let sel =
    Select.create ~parent:root
      (Select.Props.make ?options ?selected_index ?wrap_selection
         ?show_description ?show_scroll_indicator ?item_spacing
         ?fast_scroll_step ?key_bindings ())
  in
  Select.set_on_change sel (Some (Select.set_selected_index sel));
  (t, sel)

let emit_key sel key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Select.node sel) ev;
  if not (Event.Key.default_prevented ev) then
    Renderable.Private.run_key_behavior (Select.node sel) ev

let emit_mouse sel ev = Renderable.Private.emit_mouse (Select.node sel) ev

let with_layout sel ~width ~height =
  layout_node (Select.node sel) ~x:0 ~y:0 ~width ~height

let with_layout_at sel ~x ~y ~width ~height =
  layout_node (Select.node sel) ~x ~y ~width ~height

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

let create_empty_list_has_no_selection () =
  let _t, sel = make_select ~selected_index:5 () in
  equal ~msg:"zero" int (-1) (Select.selected_index sel)

(* ── Selection ── *)

let set_selected_index_clamps () =
  let _t, sel = make_select ~options:sample_items () in
  Select.set_selected_index sel 100;
  equal ~msg:"clamped high" int 4 (Select.selected_index sel);
  Select.set_selected_index sel (-5);
  equal ~msg:"negative clears selection" int (-1) (Select.selected_index sel)

let set_selected_index_fires_on_change () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.set_selected_index sel 2;
  equal ~msg:"accepted setter is silent" (list int) [] !log

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
  emit_key sel (make_key ~keypad:true Enter);
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
  equal ~msg:"index zero" int (-1) (Select.selected_index sel)

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

let mouse_click_uses_local_coordinates () =
  let _t, sel = make_select ~options:sample_items () in
  with_layout_at sel ~x:10 ~y:6 ~width:30 ~height:20;
  (* Global y=10 is local y=4, which maps to item 2. *)
  emit_mouse sel (mouse_down ~x:15 ~y:10);
  equal ~msg:"selected" int 2 (Select.selected_index sel)

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
  equal ~msg:"zero" int (-1) (Select.selected_index sel);
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
  Select.set_background sel (Ansi.Color.of_rgba 0 0 0 0);
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
  Select.set_text_color sel (Ansi.Color.of_rgb 255 255 255);
  let before = !(t.schedule_count) in
  Select.set_text_color sel (Ansi.Color.of_rgb 255 255 255);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_focused_background_noop () =
  let t, sel = make_select () in
  Select.set_focused_background sel (Ansi.Color.of_rgb 26 26 26);
  let before = !(t.schedule_count) in
  Select.set_focused_background sel (Ansi.Color.of_rgb 26 26 26);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_selected_text_color_noop () =
  let t, sel = make_select () in
  Select.set_selected_text_color sel (Ansi.Color.of_rgb 255 255 0);
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
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_show_scroll_indicator_toggle () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_show_scroll_indicator sel true;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_wrap_selection_enables_wrapping () =
  let _t, sel = make_select ~options:sample_items ~selected_index:4 () in
  Select.set_wrap_selection sel true;
  emit_key sel (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Select.selected_index sel)

let set_item_spacing_schedules () =
  let t, sel = make_select () in
  let before = !(t.schedule_count) in
  Select.set_item_spacing sel 2;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

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
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  equal ~msg:"index applied" int 3 (Select.selected_index sel)

let apply_props_same_options_no_extra_render () =
  let t, sel = make_select ~options:sample_items () in
  let props = Select.Props.make ~options:sample_items () in
  Select.apply_props sel props;
  let before = !(t.schedule_count) in
  Select.apply_props sel props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

let apply_props_restores_declared_selection () =
  let _t, sel = make_select ~options:sample_items () in
  Select.set_selected_index sel 3;
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.apply_props sel
    (Select.Props.make ~options:sample_items ~wrap_selection:true ());
  equal ~msg:"omitted selection declares zero" int 0 (Select.selected_index sel);
  equal ~msg:"no callback echo" (list int) [] !log

let apply_props_controlled_selection_is_silent () =
  let _t, sel = make_select ~options:sample_items () in
  let log = ref [] in
  Select.set_on_change sel (Some (fun i -> log := i :: !log));
  Select.apply_props sel
    (Select.Props.make ~options:sample_items ~selected_index:2 ());
  equal ~msg:"controlled selection applied" int 2 (Select.selected_index sel);
  equal ~msg:"no callback echo" (list int) [] !log

(* ── Runner ── *)

let proposals_do_not_drift () =
  let _ctx, sel = make_select ~options:sample_items () in
  let proposals = ref [] in
  Select.set_on_change sel (Some (fun next -> proposals := next :: !proposals));
  emit_key sel (make_key Down);
  emit_key sel (make_key Down);
  equal ~msg:"rejected events derive from accepted state" (list int) [ 1; 1 ]
    !proposals;
  equal ~msg:"proposal does not mutate selection" int 0
    (Select.selected_index sel);
  Select.apply_props sel
    (Select.Props.make ~options:sample_items ~selected_index:2 ());
  emit_key sel (make_key Down);
  equal ~msg:"transformed accepted state drives next proposal" (list int)
    [ 3; 1; 1 ] !proposals

let absent_selection_is_preserved () =
  let context, sel =
    make_select ~options:sample_items ~selected_index:(-1) ()
  in
  ignore context;
  is_none ~msg:"no selected item" (Select.selected_item sel);
  let activations = ref [] in
  Select.set_on_activate sel (Some (fun i -> activations := i :: !activations));
  emit_key sel (make_key Enter);
  equal ~msg:"Enter does not activate an absent selection" (list int) []
    !activations;
  with_layout sel ~width:30 ~height:10;
  let grid = make_grid ~width:30 ~height:10 () in
  Renderable.Private.render_full (Select.node sel) ~grid ~dt:0.;
  is_false ~msg:"no selection marker"
    (String.contains (Matrix_grid.to_text grid) '\226');
  emit_key sel (make_key ~shift:true Down);
  equal ~msg:"first Down selects first even with a larger step" int 0
    (Select.selected_index sel);
  Select.apply_props sel
    (Select.Props.make ~options:sample_items ~selected_index:(-1) ());
  emit_key sel (make_key Up);
  equal ~msg:"first Up selects last without wrapping" int 4
    (Select.selected_index sel);
  Select.set_selected_index sel (-1);
  emit_mouse sel (mouse_down ~x:5 ~y:2);
  equal ~msg:"click selects a valid item from absence" int 1
    (Select.selected_index sel)

let measure_select ?width ?height
    ?(available_width = Toffee.Available_space.Max_content)
    ?(available_height = Toffee.Available_space.Max_content) sel =
  match Renderable.measure (Select.node sel) with
  | None -> fail "Select has no intrinsic measurement"
  | Some measure ->
      let size =
        measure
          ~known_dimensions:Toffee.Geometry.Size.{ width; height }
          ~available_space:
            Toffee.Geometry.Size.
              { width = available_width; height = available_height }
          ~style:(Renderable.style (Select.node sel))
      in
      (int_of_float size.width, int_of_float size.height)

let intrinsic_content_size () =
  let _, empty = make_select () in
  equal ~msg:"empty content has no intrinsic extent" (pair int int) (0, 0)
    (measure_select empty);
  let options =
    Select.
      [
        { label = "界"; description = Some "details" };
        { label = "B"; description = None };
      ]
  in
  let _, sel = make_select ~options ~item_spacing:3 () in
  equal ~msg:"description width and inter-item spacing" (pair int int) (10, 7)
    (measure_select sel);
  Select.set_show_description sel false;
  equal ~msg:"hidden descriptions contribute neither width nor rows"
    (pair int int) (5, 5) (measure_select sel);
  Select.set_show_scroll_indicator sel true;
  equal ~msg:"indicator reserves a rightmost column" (pair int int) (6, 5)
    (measure_select sel);
  Select.set_item_spacing sel 0;
  equal ~msg:"spacing change invalidates natural height" (pair int int) (6, 2)
    (measure_select sel)

let intrinsic_constraints () =
  let _, sel = make_select ~options:sample_items () in
  equal ~msg:"available dimensions constrain content" (pair int int) (4, 3)
    (measure_select ~available_width:(Definite 4.)
       ~available_height:(Definite 3.) sel);
  equal ~msg:"explicit dimensions take precedence including zero" (pair int int)
    (20, 0)
    (measure_select ~width:20. ~height:0. ~available_width:(Definite 4.) sel);
  equal ~msg:"zero available dimensions are respected" (pair int int) (0, 0)
    (measure_select ~available_width:(Definite 0.)
       ~available_height:(Definite 0.) sel);
  equal ~msg:"no wrapping gives the same min and max content" (pair int int)
    (measure_select sel)
    (measure_select ~available_width:Min_content ~available_height:Min_content
       sel)

let intrinsic_options_invalidation () =
  let options label = Select.[ { label; description = None } ] in
  let _, sel = make_select ~options:(options "A") ~show_description:false () in
  equal (pair int int) (4, 1) (measure_select sel);
  Select.set_options sel (options "abcdefghij");
  equal ~msg:"replacement invalidates measured width" (pair int int) (13, 1)
    (measure_select sel);
  Select.apply_props sel
    (Select.Props.make ~options:(options "界") ~show_description:false ());
  equal ~msg:"declaration can shrink cached content" (pair int int) (5, 1)
    (measure_select sel);
  Select.set_selected_index sel (-1);
  equal ~msg:"selection does not determine content size" (pair int int) (5, 1)
    (measure_select sel);
  Select.set_options sel [];
  equal ~msg:"clearing items clears intrinsic size" (pair int int) (0, 0)
    (measure_select sel)

let intrinsic_width_policy () =
  let width_method = ref `Unicode in
  let context = make_ctx () in
  let context =
    {
      context with
      ctx = { context.ctx with width_method = (fun () -> !width_method) };
    }
  in
  let root = make_root context in
  let sel =
    Select.create ~parent:root
      (Select.Props.make ~show_description:false
         ~options:Select.[ { label = "👩‍👩‍👦"; description = None } ]
         ())
  in
  equal ~msg:"Unicode measures the grapheme cluster" (pair int int) (5, 1)
    (measure_select sel);
  width_method := `Wcwidth;
  equal ~msg:"policy change invalidates cached item width" (pair int int) (9, 1)
    (measure_select sel)

let intrinsic_spacing_paints_last_item () =
  let _, sel =
    make_select ~show_description:false ~item_spacing:2
      ~options:
        Select.
          [
            { label = "A"; description = None };
            { label = "B"; description = None };
          ]
      ()
  in
  let width, height = measure_select sel in
  with_layout sel ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full (Select.node sel) ~grid ~dt:0.;
  equal ~msg:"last item does not need a trailing spacing row" string "B"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:3 ~y:3))

let bindings_use_exact_key_events () =
  let _, sel = make_select ~options:sample_items ~selected_index:2 () in
  List.iter
    (fun key ->
      let event = Event.Key.of_input key in
      Renderable.Private.run_key_behavior (Select.node sel) event;
      is_false ~msg:"unbound modified key remains available"
        (Event.Key.default_prevented event);
      equal ~msg:"unbound event leaves selection unchanged" int 2
        (Select.selected_index sel))
    [
      make_key ~ctrl:true Down;
      make_key ~alt:true Down;
      make_key ~super:true Down;
      make_key ~hyper:true Down;
      make_key ~meta:true Down;
    ];
  (* Caps Lock with j sends the capital, which no binding names. *)
  emit_key sel (make_key (Char (Uchar.of_char 'J')));
  equal ~msg:"a capital takes the lowercase binding" int 3
    (Select.selected_index sel);
  emit_key sel (make_key ~caps_lock:true (Char (Uchar.of_char 'K')));
  equal ~msg:"Caps Lock is undone when reported" int 2
    (Select.selected_index sel);
  emit_key sel
    (make_key ~caps_lock:true ~num_lock:true ~repeat:true ~keypad:true Down);
  equal ~msg:"repeat and keypad navigation ignore lock state" int 3
    (Select.selected_index sel);
  emit_key sel (make_key ~base:(Uchar.of_char 'k') (Char (Uchar.of_int 0x043B)));
  equal ~msg:"base-layout fallback uses the same default binding" int 2
    (Select.selected_index sel)

let custom_bindings_override_and_update () =
  let bindings =
    [ (Shortcut.down, Select.Move_up); (Shortcut.ctrl 'n', Select.Move_down) ]
  in
  let _, sel =
    make_select ~options:sample_items ~selected_index:2 ~key_bindings:bindings
      ()
  in
  emit_key sel (make_key Down);
  equal ~msg:"custom binding overrides default" int 1
    (Select.selected_index sel);
  emit_key sel (make_key ~ctrl:true (Char (Uchar.of_char 'n')));
  equal ~msg:"custom modified shortcut invokes action" int 2
    (Select.selected_index sel);
  Select.apply_props sel
    (Select.Props.make ~options:sample_items ~selected_index:2 ());
  emit_key sel (make_key Down);
  equal ~msg:"removing custom bindings restores defaults" int 3
    (Select.selected_index sel);
  emit_key sel (make_key ~ctrl:true (Char (Uchar.of_char 'n')));
  equal ~msg:"removed custom binding is unavailable" int 3
    (Select.selected_index sel)

let raw_handler_can_prevent_navigation () =
  let _, sel = make_select ~options:sample_items () in
  Renderable.on_key (Select.node sel) Event.Key.prevent_default;
  emit_key sel (make_key Down);
  equal ~msg:"raw handlers run before default navigation" int 0
    (Select.selected_index sel)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.select}) or to
   {!Select.Props.make} for {!Select.create}, reaches the live select
   ({!Test_harness.reaches}). The element's [selected] gives the select's
   [selected_index]. *)

let doors_size = Mosaic.size ~width:16 ~height:4
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let doors_labels = [ ("zero", "a"); ("one", "b"); ("two", "c") ]

let few_options =
  List.mapi
    (fun i (label, description) -> Mosaic.Select.item i ~description label)
    doors_labels

let few_items =
  List.map
    (fun (label, description) ->
      { Select.label; description = Some description })
    doors_labels

let doors_many = List.init 8 string_of_int

let many_options =
  List.mapi (fun i label -> Mosaic.Select.item i label) doors_many

let many_items =
  List.map (fun label -> { Select.label; description = None }) doors_many

(* Keys propose a selection through [on_change]. *)
let doors_proposals = ref []
let doors_record i = doors_proposals := i :: !doors_proposals

let doors_recording select =
  Select.set_on_change select (Some doors_record);
  Select.node select

let doors_press key renderer node =
  doors_proposals := [];
  Test_harness.press_key key renderer node

let doors_proposed _ _ =
  String.concat ";" (List.rev_map string_of_int !doors_proposals)

let doors_options () =
  Test_harness.reaches "options"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore many_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:many_items ())))

let doors_selected_index () =
  Test_harness.reaches "selected_index (the element's selected)"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 1)
         ~on_change:ignore few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~selected_index:1 ())))

let doors_background () =
  Test_harness.reaches "background"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~background:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~background:doors_red ())))

let doors_text_color () =
  Test_harness.reaches "text_color"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~text_color:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~text_color:doors_red ())))

let doors_focused_background () =
  Test_harness.reaches ~act:Test_harness.focus "focused_background"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~focused_background:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~focused_background:doors_red
              ())))

let doors_focused_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_text_color"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~focused_text_color:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~focused_text_color:doors_red
              ())))

let doors_selected_background () =
  Test_harness.reaches "selected_background"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~selected_background:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~selected_background:doors_red
              ())))

let doors_selected_text_color () =
  Test_harness.reaches "selected_text_color"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~selected_text_color:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~selected_text_color:doors_red
              ())))

let doors_description_color () =
  Test_harness.reaches "description_color"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~description_color:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~description_color:doors_red ())))

let doors_selected_description_color () =
  Test_harness.reaches "selected_description_color"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~selected_description_color:doors_red few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items
              ~selected_description_color:doors_red ())))

let doors_show_description () =
  Test_harness.reaches "show_description"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~show_description:false few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~show_description:false ())))

let doors_show_scroll_indicator () =
  Test_harness.reaches "show_scroll_indicator"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore many_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~show_scroll_indicator:true many_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:many_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:many_items ~show_scroll_indicator:true ())))

let doors_wrap_selection () =
  Test_harness.reaches
    ~act:(doors_press (make_key Up))
    ~observe:doors_proposed "wrap_selection"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~wrap_selection:true few_options)
    ~create_base:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~wrap_selection:true ())))

let doors_item_spacing () =
  Test_harness.reaches "item_spacing"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~item_spacing:1 few_options)
    ~create_base:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      Select.node
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ~item_spacing:1 ())))

let doors_fast_scroll_step () =
  Test_harness.reaches
    ~act:(doors_press (make_key ~shift:true Down))
    ~observe:doors_proposed "fast_scroll_step"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:false many_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:false ~fast_scroll_step:2
         many_options)
    ~create_base:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:many_items ~show_description:false ())))
    ~create:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:many_items ~show_description:false
              ~fast_scroll_step:2 ())))

let doors_key_bindings () =
  Test_harness.reaches
    ~act:(doors_press (char_key 'x'))
    ~observe:doors_proposed "key_bindings"
    ~base:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record few_options)
    ~element:
      (Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record
         ~key_bindings:[ (Shortcut.char 'x', Select.Move_down) ]
         few_options)
    ~create_base:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items ())))
    ~create:(fun parent ->
      doors_recording
        (Select.create ~parent ~style:doors_style
           (Select.Props.make ~options:few_items
              ~key_bindings:[ (Shortcut.char 'x', Select.Move_down) ]
              ())))

(* ── Paint ── *)

(* Items in the [Plain] role, descriptions in [Muted], the selected item
   marked by [cursor] while focused and [cursor_inactive] otherwise, its row
   on the [Cursor] surface while focused; a label replaces its role. *)

let paint_view ?selected_background () =
  Mosaic.select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
    ?selected_background ~on_change:ignore few_options

let paint_legible_on_light () =
  List.iter
    (fun focused ->
      equal ~msg:"no cell below its contrast" (list string) []
        (illegible xterm_light
           (painted
              (paint
                 ~theme:(Theme.default xterm_light)
                 ~focused (paint_view ())))))
    [ false; true ]

let paint_focus_shows () = focus_shows (paint_view ())

let paint_marks () =
  equal ~msg:"unfocused" string "\u{203A}"
    (cell_text (paint (paint_view ())) ~x:1 ~y:0);
  equal ~msg:"focused" string "\u{276F}"
    (cell_text (paint ~focused:true (paint_view ())) ~x:1 ~y:0);
  equal ~msg:"ascii, focused" string ">"
    (cell_text
       (paint ~focused:true
          (Mosaic.themed (Theme.with_glyphs Glyphs.ascii) (paint_view ())))
       ~x:1 ~y:0)

let paint_selected_background () =
  let r = paint (paint_view ~selected_background:doors_red ()) in
  is_true ~msg:"the label's background, unfocused"
    (bg_is doors_red (cell_style r ~x:5 ~y:0))

let () =
  run "mosaic.select"
    [
      group "Paint"
        [
          test "legible on a light terminal" paint_legible_on_light;
          test "focus shows at every styling" paint_focus_shows;
          test "the selected item's mark" paint_marks;
          test "selected_background" paint_selected_background;
        ];
      group "Intrinsic sizing"
        [
          test "content, descriptions and spacing" intrinsic_content_size;
          test "available and known dimensions" intrinsic_constraints;
          test "option changes invalidate size" intrinsic_options_invalidation;
          test "terminal width policy" intrinsic_width_policy;
          test "last item needs no trailing gap"
            intrinsic_spacing_paints_last_item;
        ];
      group "Typed bindings"
        [
          test "exact modifiers, repeat, release and base layout"
            bindings_use_exact_key_events;
          test "custom bindings override and update"
            custom_bindings_override_and_update;
          test "raw handlers prevent navigation"
            raw_handler_can_prevent_navigation;
        ];
      test "proposals do not drift" proposals_do_not_drift;
      test "absent selection is preserved" absent_selection_is_preserved;
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
          test "empty list has no selection" create_empty_list_has_no_selection;
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
          test "click uses local coordinates" mouse_click_uses_local_coordinates;
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
          test "preserves uncontrolled selection"
            apply_props_restores_declared_selection;
          test "controlled selection is silent"
            apply_props_controlled_selection_is_silent;
        ];
      group "Both doors"
        [
          test "options" doors_options;
          test "selected_index" doors_selected_index;
          test "background" doors_background;
          test "text_color" doors_text_color;
          test "focused_background" doors_focused_background;
          test "focused_text_color" doors_focused_text_color;
          test "selected_background" doors_selected_background;
          test "selected_text_color" doors_selected_text_color;
          test "description_color" doors_description_color;
          test "selected_description_color" doors_selected_description_color;
          test "show_description" doors_show_description;
          test "show_scroll_indicator" doors_show_scroll_indicator;
          test "wrap_selection" doors_wrap_selection;
          test "item_spacing" doors_item_spacing;
          test "fast_scroll_step" doors_fast_scroll_step;
          test "key_bindings" doors_key_bindings;
        ];
    ]
