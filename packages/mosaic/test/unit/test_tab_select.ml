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
  Tab_select.set_on_change ts (Some (Tab_select.set_selected ts));
  (t, ts)

let emit_key ts key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Tab_select.node ts) ev;
  if
    (not (Event.Key.default_prevented ev))
    && not (Event.Key.propagation_stopped ev)
  then Renderable.Private.run_key_behavior (Tab_select.node ts) ev

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

let create_empty_list_has_no_selection () =
  let _t, ts = make_tab_select ~options:[] ~selected:5 () in
  equal ~msg:"zero" int (-1) (Tab_select.selected_index ts)

(* ── Selection ── *)

let set_selected_clamps () =
  let _t, ts = make_tab_select () in
  Tab_select.set_selected ts 100;
  equal ~msg:"clamped high" int 4 (Tab_select.selected_index ts);
  Tab_select.set_selected ts (-5);
  equal ~msg:"negative clears selection" int (-1) (Tab_select.selected_index ts)

let set_selected_fires_on_change () =
  let _t, ts = make_tab_select () in
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  Tab_select.set_selected ts 2;
  equal ~msg:"accepted setter is silent" (list int) [] !log

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
  Tab_select.set_on_change ts
    (Some
       (fun i ->
         Tab_select.set_selected ts i;
         log := i :: !log));
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
  equal ~msg:"zero" int (-1) (Tab_select.selected_index ts);
  match Tab_select.selected_item ts with
  | Some _ -> fail "expected None"
  | None -> ()

(* ── Setter No-ops ── *)

let set_background_noop () =
  let t, ts = make_tab_select () in
  Tab_select.set_background ts (Ansi.Color.of_rgba 0 0 0 0);
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
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  equal ~msg:"index applied" int 3 (Tab_select.selected_index ts)

let apply_props_restores_declared_selection () =
  let _t, ts = make_tab_select () in
  Tab_select.set_selected ts 3;
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  Tab_select.apply_props ts
    (Tab_select.Props.make ~options:sample_items ~wrap_selection:true ());
  equal ~msg:"omitted selection declares zero" int 0
    (Tab_select.selected_index ts);
  equal ~msg:"no callback echo" (list int) [] !log

let apply_props_controlled_selection_is_silent () =
  let _t, ts = make_tab_select () in
  let log = ref [] in
  Tab_select.set_on_change ts (Some (fun i -> log := i :: !log));
  Tab_select.apply_props ts
    (Tab_select.Props.make ~options:sample_items ~selected:2 ());
  equal ~msg:"controlled selection applied" int 2 (Tab_select.selected_index ts);
  equal ~msg:"no callback echo" (list int) [] !log

(* ── Rendering ── *)

let render_tab_select ts ~width ~height =
  let node = Tab_select.node ts in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  grid

let truncation_keeps_cjk_graphemes_intact () =
  (* "你好世界" is four 2-column glyphs; with tab_width 5 the label budget is
     3 columns: one intact glyph plus a 1-column ellipsis. A byte-based cut
     would split the first character's UTF-8 sequence. *)
  let _t, ts =
    make_tab_select
      ~options:[ Tab_select.{ label = "你好世界"; description = "" } ]
      ~tab_width:5 ~show_underline:false ()
  in
  let grid = render_tab_select ts ~width:10 ~height:1 in
  equal ~msg:"first glyph intact" string "你"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:1 ~y:0));
  equal ~msg:"ellipsis after the glyph" string "\xe2\x80\xa6"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:3 ~y:0))

let truncation_never_splits_emoji () =
  (* Budget of 2 columns cannot fit a 2-column emoji plus the ellipsis, so
     only the ellipsis renders; byte-based truncation used to emit a partial
     UTF-8 sequence instead. *)
  let _t, ts =
    make_tab_select
      ~options:[ Tab_select.{ label = "👍👍👍"; description = "" } ]
      ~tab_width:4 ~show_underline:false ()
  in
  let grid = render_tab_select ts ~width:8 ~height:1 in
  equal ~msg:"only the ellipsis renders" string "\xe2\x80\xa6"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:1 ~y:0))

(* ── Runner ── *)

let proposals_do_not_drift () =
  let _ctx, ts = make_tab_select () in
  let proposals = ref [] in
  Tab_select.set_on_change ts
    (Some (fun next -> proposals := next :: !proposals));
  emit_key ts (make_key Right);
  emit_key ts (make_key Right);
  equal ~msg:"rejected events derive from accepted state" (list int) [ 1; 1 ]
    !proposals;
  equal ~msg:"proposal does not mutate selection" int 0
    (Tab_select.selected_index ts);
  Tab_select.apply_props ts
    (Tab_select.Props.make ~options:sample_items ~selected:2 ());
  emit_key ts (make_key Right);
  equal ~msg:"transformed accepted state drives next proposal" (list int)
    [ 3; 1; 1 ] !proposals

let absent_selection_is_preserved () =
  let context, ts = make_tab_select ~selected:(-1) ~show_description:true () in
  ignore context;
  is_none ~msg:"no selected tab" (Tab_select.selected_item ts);
  let activations = ref [] in
  Tab_select.set_on_activate ts
    (Some (fun i -> activations := i :: !activations));
  emit_key ts (make_key Enter);
  equal ~msg:"Enter does not activate an absent tab" (list int) [] !activations;
  let grid = render_tab_select ts ~width:80 ~height:3 in
  let lines = String.split_on_char '\n' (Matrix_grid.to_text grid) in
  equal ~msg:"no selected underline" string "" (String.trim (List.nth lines 1));
  equal ~msg:"no selected description" string ""
    (String.trim (List.nth lines 2));
  emit_key ts (make_key Right);
  equal ~msg:"first Right selects first" int 0 (Tab_select.selected_index ts);
  Tab_select.apply_props ts
    (Tab_select.Props.make ~options:sample_items ~selected:(-1) ());
  emit_key ts (make_key Left);
  equal ~msg:"first Left selects last without wrapping" int 4
    (Tab_select.selected_index ts)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.tab_select}) or to
   {!Tab_select.create}, reaches the live tabs ({!Test_harness.reaches}).
   Keys propose a selection through [on_change], which records it. *)

let doors_size = Mosaic.size ~width:30 ~height:3
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let doors_labels = [ ("zero", "a"); ("one", "b"); ("two", "c") ]

let doors_tabs =
  List.mapi
    (fun i (label, description) -> Mosaic.Tab_select.item i ~description label)
    doors_labels

let doors_tab_items =
  List.map
    (fun (label, description) -> Tab_select.item ~label ~description ())
    doors_labels

let doors_proposals = ref []
let doors_record i = doors_proposals := i :: !doors_proposals

let doors_press key renderer node =
  doors_proposals := [];
  Test_harness.press_key key renderer node

let doors_proposed _ _ =
  String.concat ";" (List.rev_map string_of_int !doors_proposals)

let doors_options () =
  Test_harness.reaches "options"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 2)
         ~on_change:ignore (List.rev doors_tabs))
      (* The element keeps the selection on its value: the value at the first
       place of the reversed list is 2. *)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style
           ~options:(List.rev doors_tab_items) ()))

let doors_selected () =
  Test_harness.reaches "selected"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 1)
         ~on_change:ignore doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~selected:1 ()))

let doors_tab_width () =
  Test_harness.reaches "tab_width"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~tab_width:8 doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~tab_width:8 ()))

let doors_background () =
  Test_harness.reaches "background"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~background:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~background:doors_red ()))

let doors_text_color () =
  Test_harness.reaches "text_color"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~text_color:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~text_color:doors_red ()))

let doors_focused_background () =
  Test_harness.reaches ~act:Test_harness.focus "focused_background"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~focused_background:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~focused_background:doors_red ()))

let doors_focused_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_text_color"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~focused_text_color:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~focused_text_color:doors_red ()))

let doors_selected_background () =
  Test_harness.reaches "selected_background"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~selected_background:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~selected_background:doors_red ()))

let doors_selected_text_color () =
  Test_harness.reaches "selected_text_color"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~selected_text_color:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~selected_text_color:doors_red ()))

let doors_selected_description_color () =
  Test_harness.reaches "selected_description_color" ~act:Test_harness.focus
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:true doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:true
         ~selected_description_color:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_description:true ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_description:true
           ~selected_description_color:doors_red ()))

let doors_description_color () =
  Test_harness.reaches "description_color"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:true doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:true
         ~description_color:doors_red doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_description:true ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_description:true
           ~description_color:doors_red ()))

let doors_show_underline () =
  Test_harness.reaches "show_underline"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_underline:false doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_underline:false ()))

let doors_show_description () =
  Test_harness.reaches "show_description"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_description:true doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_description:true ()))

let doors_show_scroll_arrows () =
  Test_harness.reaches "show_scroll_arrows"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~show_scroll_arrows:false doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~show_scroll_arrows:false ()))

let doors_wrap_selection () =
  Test_harness.reaches
    ~act:(doors_press (make_key Left))
    ~observe:doors_proposed "wrap_selection"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~wrap_selection:true doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ~wrap_selection:true ()))

let doors_key_bindings () =
  Test_harness.reaches
    ~act:(doors_press (char_key 'x'))
    ~observe:doors_proposed "key_bindings"
    ~base:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record doors_tabs)
    ~element:
      (Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record
         ~key_bindings:[ (Shortcut.char 'x', Tab_select.Move_right) ]
         doors_tabs)
    ~create_base:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record ()))
    ~create:(fun parent ->
      Tab_select.node
        (Tab_select.create ~parent ~style:doors_style ~options:doors_tab_items
           ~on_change:doors_record
           ~key_bindings:[ (Shortcut.char 'x', Tab_select.Move_right) ]
           ()))

(* ── Paint ── *)

(* Tab labels in the [Plain] role, the selected tab on the [Selected]
   surface with a mark in its first cell, [cursor] and the [Cursor] surface
   while focused; the description in [Muted], or in [description_color]
   unfocused and [selected_description_color] focused. *)

let paint_view ?description_color ?selected_description_color () =
  Mosaic.tab_select ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
    ~show_description:true ?description_color ?selected_description_color
    ~on_change:ignore doors_tabs

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

let paint_description_colors () =
  let blue = Ansi.Color.blue in
  let view =
    paint_view ~description_color:doors_red ~selected_description_color:blue ()
  in
  is_true ~msg:"unfocused, description_color"
    (fg_is doors_red (cell_style (paint view) ~x:0 ~y:2));
  is_true ~msg:"focused, selected_description_color"
    (fg_is blue (cell_style (paint ~focused:true view) ~x:0 ~y:2))

let () =
  run "mosaic.tab_select"
    [
      group "Paint"
        [
          test "legible on a light terminal" paint_legible_on_light;
          test "focus shows at every styling" paint_focus_shows;
          test "description colors" paint_description_colors;
        ];
      test "proposals do not drift" proposals_do_not_drift;
      test "absent selection is preserved" absent_selection_is_preserved;
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
          test "empty list has no selection" create_empty_list_has_no_selection;
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
      group "apply_props"
        [
          test "updates all properties" apply_props_updates;
          test "preserves uncontrolled selection"
            apply_props_restores_declared_selection;
          test "controlled selection is silent"
            apply_props_controlled_selection_is_silent;
        ];
      group "Rendering"
        [
          test "truncation keeps CJK graphemes intact"
            truncation_keeps_cjk_graphemes_intact;
          test "truncation never splits emoji" truncation_never_splits_emoji;
        ];
      group "Both doors"
        [
          test "options" doors_options;
          test "selected" doors_selected;
          test "tab_width" doors_tab_width;
          test "background" doors_background;
          test "text_color" doors_text_color;
          test "focused_background" doors_focused_background;
          test "focused_text_color" doors_focused_text_color;
          test "selected_background" doors_selected_background;
          test "selected_text_color" doors_selected_text_color;
          test "selected_description_color" doors_selected_description_color;
          test "description_color (drawn by no tab)" doors_description_color;
          test "show_underline" doors_show_underline;
          test "show_description" doors_show_description;
          test "show_scroll_arrows" doors_show_scroll_arrows;
          test "wrap_selection" doors_wrap_selection;
          test "key_bindings" doors_key_bindings;
        ];
    ]
