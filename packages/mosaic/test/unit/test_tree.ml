open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let sample_items =
  Tree.
    [
      item ~id:"src" "src"
        ~children:
          [ item ~id:"main.ml" "main.ml"; item ~id:"utils.ml" "utils.ml" ];
      item ~id:"test" "test"
        ~children:[ item ~id:"test_main.ml" "test_main.ml" ];
      item ~id:"README.md" "README.md";
    ]

let deep_items =
  Tree.
    [
      item ~id:"a" "a"
        ~children:
          [
            item ~id:"b" "b"
              ~children:[ item ~id:"c" "c" ~children:[ item ~id:"d" "d" ] ];
          ];
    ]

(* Tests explicitly accept proposals; rejection cases replace this handler. *)
let state_for_items items selected_index expand_depth =
  let rec walk depth items =
    List.concat_map
      (fun (item : Tree.item) ->
        if item.children <> [] && (expand_depth < 0 || depth < expand_depth)
        then item.id :: walk (depth + 1) item.children
        else [])
      items
  in
  let expanded = walk 0 items in
  let rec visible items =
    List.concat_map
      (fun (item : Tree.item) ->
        item.id
        :: (if List.mem item.id expanded then visible item.children else []))
      items
  in
  let ids = visible items in
  let selected =
    List.nth_opt ids (max 0 (min (List.length ids - 1) selected_index))
  in
  Tree.State.make ?selected ~expanded ()

let make_tree ?(items = []) ?(selected_index = 0) ?(expand_depth = 0)
    ?indent_size ?show_guides ?guide_style ?wrap_selection ?fast_scroll_step ()
    =
  let t = make_ctx () in
  let root = make_root t in
  let state = state_for_items items selected_index expand_depth in
  let props state items =
    Tree.Props.make ~items ~state ?indent_size ?show_guides ?guide_style
      ?wrap_selection ?fast_scroll_step ()
  in
  let tree =
    Tree.create ~parent:root ~items ~state ?indent_size ?show_guides
      ?guide_style ?wrap_selection ?fast_scroll_step ()
  in
  Tree.set_on_change tree
    (Some (fun state -> Tree.apply_props tree (props state (Tree.items tree))));
  (t, tree)

let emit_key tree key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Tree.node tree) ev;
  if
    (not (Event.Key.default_prevented ev))
    && not (Event.Key.propagation_stopped ev)
  then Renderable.Private.run_key_behavior (Tree.node tree) ev

let emit_mouse tree ev = Renderable.Private.emit_mouse (Tree.node tree) ev

let with_layout tree ~width ~height =
  layout_node (Tree.node tree) ~x:0 ~y:0 ~width ~height

(* ── Props ── *)

let props_defaults () =
  let p = Tree.Props.default in
  is_true ~msg:"equal to make()" (Tree.Props.equal p (Tree.Props.make ()))

let props_equal_identical () =
  let a = Tree.Props.make () in
  let b = Tree.Props.make () in
  is_true ~msg:"equal" (Tree.Props.equal a b)

let props_detects_items_diff () =
  let a = Tree.Props.make ~items:sample_items () in
  let b = Tree.Props.make () in
  is_false ~msg:"different" (Tree.Props.equal a b)

let props_detects_selected_index_diff () =
  let a = Tree.Props.make ~state:(Tree.State.make ~selected:"0" ()) () in
  let b = Tree.Props.make ~state:(Tree.State.make ~selected:"1" ()) () in
  is_false ~msg:"different" (Tree.Props.equal a b)

let props_detects_expand_depth_diff () =
  let a = Tree.Props.make ~state:(Tree.State.make ~expanded:[ "0" ] ()) () in
  let b = Tree.Props.make ~state:(Tree.State.make ~expanded:[ "1" ] ()) () in
  is_false ~msg:"different" (Tree.Props.equal a b)

let props_detects_show_guides_diff () =
  let a = Tree.Props.make ~show_guides:true () in
  let b = Tree.Props.make () in
  is_false ~msg:"different" (Tree.Props.equal a b)

let props_detects_color_diff () =
  let a = Tree.Props.make ~selected_background:Ansi.Color.red () in
  let b = Tree.Props.make () in
  is_false ~msg:"different" (Tree.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, tree = make_tree ~items:sample_items () in
  let node = Tree.node tree in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_is_focusable () =
  let _t, tree = make_tree () in
  is_true ~msg:"focusable" (Renderable.focusable (Tree.node tree))

let create_is_buffered () =
  let _t, tree = make_tree () in
  is_true ~msg:"buffered" (Renderable.buffered (Tree.node tree))

let create_clamps_initial_index () =
  let _t, tree =
    make_tree ~items:sample_items ~expand_depth:(-1) ~selected_index:100 ()
  in
  (* All expanded: src, main.ml, utils.ml, test, test_main.ml, README.md = 6 *)
  is_true ~msg:"clamped"
    (Tree.selected_index tree < 100 && Tree.selected_index tree >= 0)

let create_empty_items_index_zero () =
  let _t, tree = make_tree ~selected_index:5 () in
  equal ~msg:"no selection" int (-1) (Tree.selected_index tree)

(* ── Selection ── *)

let set_selected_index_clamps () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  Tree.set_selected_index tree 100;
  lt ~msg:"selected index clamped" ~than:100 (Tree.selected_index tree);
  Tree.set_selected_index tree (-5);
  equal ~msg:"clamped low" int 0 (Tree.selected_index tree)

let set_selected_index_is_silent () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  Tree.set_selected_index tree 2;
  equal ~msg:"accepted selection" int 2 (Tree.selected_index tree);
  equal ~msg:"no callback echo" (list (option string)) [] !log

let set_selected_index_noop_same () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  Tree.set_selected_index tree 0;
  equal ~msg:"no fire" (list (option string)) [] !log

let selected_item_returns_item () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  (* Index 0 = "src" *)
  match Tree.selected_item tree with
  | Some it -> equal ~msg:"label" string "src" it.label
  | None -> fail "expected item"

let selected_item_empty_tree () =
  let _t, tree = make_tree () in
  match Tree.selected_item tree with
  | Some _ -> fail "expected None"
  | None -> ()

(* ── Navigation ── *)

let move_down_basic () =
  let _t, tree = make_tree ~items:sample_items () in
  (* All collapsed: src, test, README.md *)
  emit_key tree (make_key Down);
  equal ~msg:"index" int 1 (Tree.selected_index tree)

let move_up_basic () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  emit_key tree (make_key Up);
  equal ~msg:"index" int 1 (Tree.selected_index tree)

let move_down_j () =
  let _t, tree = make_tree ~items:sample_items () in
  emit_key tree (make_key (Char (Uchar.of_char 'j')));
  equal ~msg:"index" int 1 (Tree.selected_index tree)

let move_up_k () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  emit_key tree (make_key (Char (Uchar.of_char 'k')));
  equal ~msg:"index" int 1 (Tree.selected_index tree)

let move_down_no_wrap () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  emit_key tree (make_key Down);
  equal ~msg:"stays at end" int 2 (Tree.selected_index tree)

let move_up_no_wrap () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:0 () in
  emit_key tree (make_key Up);
  equal ~msg:"stays at start" int 0 (Tree.selected_index tree)

let move_down_wrap () =
  let _t, tree =
    make_tree ~items:sample_items ~selected_index:2 ~wrap_selection:true ()
  in
  emit_key tree (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Tree.selected_index tree)

let move_up_wrap () =
  let _t, tree =
    make_tree ~items:sample_items ~selected_index:0 ~wrap_selection:true ()
  in
  emit_key tree (make_key Up);
  equal ~msg:"wraps to end" int 2 (Tree.selected_index tree)

let fast_scroll_down () =
  let _t, tree =
    make_tree ~items:sample_items ~expand_depth:(-1) ~fast_scroll_step:3 ()
  in
  emit_key tree (make_key ~shift:true Down);
  equal ~msg:"jumped" int 3 (Tree.selected_index tree)

let fast_scroll_up () =
  let _t, tree =
    make_tree ~items:sample_items ~expand_depth:(-1) ~selected_index:5
      ~fast_scroll_step:3 ()
  in
  emit_key tree (make_key ~shift:true Up);
  equal ~msg:"jumped" int 2 (Tree.selected_index tree)

let enter_fires_on_activate () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:1 () in
  let log = ref [] in
  Tree.set_on_activate tree (Some (fun item -> log := item.Tree.id :: !log));
  emit_key tree (make_key Enter);
  equal ~msg:"activated" (list string) [ "test" ] !log

let on_activate_empty_tree () =
  let _t, tree = make_tree () in
  let fired = ref false in
  Tree.set_on_activate tree (Some (fun _ -> fired := true));
  emit_key tree (make_key Enter);
  is_false ~msg:"not fired" !fired

let unhandled_key_ignored () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:1 () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  emit_key tree (make_key (Char (Uchar.of_char 'a')));
  equal ~msg:"no change" (list (option string)) [] !log;
  equal ~msg:"index unchanged" int 1 (Tree.selected_index tree)

let navigation_on_empty_tree () =
  let _t, tree = make_tree () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  emit_key tree (make_key Down);
  emit_key tree (make_key Up);
  equal ~msg:"no callbacks" (list (option string)) [] !log;
  equal ~msg:"empty tree has no selected entry" int (-1)
    (Tree.selected_index tree)

(* ── Expand/Collapse via Keys ── *)

let right_expands_collapsed () =
  let _t, tree = make_tree ~items:sample_items () in
  (* "src" is at index 0, collapsed *)
  is_false ~msg:"initially collapsed" (Tree.is_expanded tree 0);
  emit_key tree (make_key Right);
  is_true ~msg:"expanded" (Tree.is_expanded tree 0)

let right_moves_to_first_child () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  (* "src" is expanded, so Right should move to first child *)
  emit_key tree (make_key Right);
  equal ~msg:"at first child" int 1 (Tree.selected_index tree)

let right_on_leaf_noop () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  (* "README.md" is a leaf *)
  emit_key tree (make_key Right);
  equal ~msg:"still at leaf" int 2 (Tree.selected_index tree)

let left_collapses_expanded () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  (* "src" is at index 0, expanded *)
  is_true ~msg:"initially expanded" (Tree.is_expanded tree 0);
  emit_key tree (make_key Left);
  is_false ~msg:"collapsed" (Tree.is_expanded tree 0)

let left_moves_to_parent () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  (* Move to first child of "src" *)
  Tree.set_selected_index tree 1;
  emit_key tree (make_key Left);
  equal ~msg:"at parent" int 0 (Tree.selected_index tree)

let left_on_top_level_leaf_noop () =
  let _t, tree = make_tree ~items:sample_items () in
  (* Move to "README.md" (last top-level item, index 2 when collapsed) *)
  Tree.set_selected_index tree 2;
  emit_key tree (make_key Left);
  equal ~msg:"still at README" int 2 (Tree.selected_index tree)

let space_toggles_expand () =
  let _t, tree = make_tree ~items:sample_items () in
  is_false ~msg:"initially collapsed" (Tree.is_expanded tree 0);
  emit_key tree (make_key (Char (Uchar.of_char ' ')));
  is_true ~msg:"expanded" (Tree.is_expanded tree 0);
  emit_key tree (make_key (Char (Uchar.of_char ' ')));
  is_false ~msg:"collapsed again" (Tree.is_expanded tree 0)

(* ── Expansion API ── *)

let expand_makes_children_visible () =
  let _t, tree = make_tree ~items:sample_items () in
  (* All collapsed: 3 visible (src, test, README.md) *)
  equal ~msg:"3 visible" int 3 (Tree.visible_count tree);
  Tree.expand tree 0;
  (* src expanded: src, main.ml, utils.ml, test, README.md = 5 *)
  equal ~msg:"5 visible" int 5 (Tree.visible_count tree)

let collapse_hides_children () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  (* All first-level expanded: 6 visible *)
  equal ~msg:"6 visible" int 6 (Tree.visible_count tree);
  Tree.collapse tree 0;
  (* src collapsed: src, test, test_main.ml, README.md = 4 *)
  equal ~msg:"4 visible" int 4 (Tree.visible_count tree)

let expand_all_shows_everything () =
  let _t, tree = make_tree ~items:sample_items () in
  Tree.expand_all tree;
  equal ~msg:"6 visible" int 6 (Tree.visible_count tree)

let collapse_all_shows_top_level () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  Tree.collapse_all tree;
  equal ~msg:"3 visible" int 3 (Tree.visible_count tree)

let toggle_expand_works () =
  let _t, tree = make_tree ~items:sample_items () in
  Tree.toggle_expand tree 0;
  is_true ~msg:"expanded" (Tree.is_expanded tree 0);
  Tree.toggle_expand tree 0;
  is_false ~msg:"collapsed" (Tree.is_expanded tree 0)

let expand_is_silent () =
  let _t, tree = make_tree ~items:sample_items () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some
       (fun state -> log := ("src", Tree.State.is_expanded "src" state) :: !log));
  Tree.expand tree 0;
  is_true ~msg:"accepted expansion" (Tree.is_expanded tree 0);
  equal ~msg:"no callback echo" (list (pair string bool)) [] !log

let collapse_is_silent () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some
       (fun state -> log := ("src", Tree.State.is_expanded "src" state) :: !log));
  Tree.collapse tree 0;
  is_false ~msg:"accepted collapse" (Tree.is_expanded tree 0);
  equal ~msg:"no callback echo" (list (pair string bool)) [] !log

let expand_leaf_noop () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  let count_before = Tree.visible_count tree in
  Tree.expand tree 2;
  equal ~msg:"no change" int count_before (Tree.visible_count tree)

let collapse_adjusts_selected_index () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  (* Select a child of src (index 1 = "main.ml") *)
  Tree.set_selected_index tree 1;
  Tree.collapse tree 0;
  (* After collapse, selected should move to the parent (src at index 0) *)
  equal ~msg:"moved to parent" int 0 (Tree.selected_index tree)

let expand_depth_negative_one () =
  let _t, tree = make_tree ~items:deep_items ~expand_depth:(-1) () in
  (* a, b, c, d = 4 visible *)
  equal ~msg:"4 visible" int 4 (Tree.visible_count tree)

let expand_depth_one () =
  let _t, tree = make_tree ~items:deep_items ~expand_depth:1 () in
  (* a, b = 2 visible (b's children not expanded) *)
  equal ~msg:"2 visible" int 2 (Tree.visible_count tree)

(* ── Items ── *)

let set_items_replaces () =
  let _t, tree = make_tree ~items:sample_items () in
  let new_items = Tree.[ item ~id:"one" "one"; item ~id:"two" "two" ] in
  Tree.set_items tree new_items;
  equal ~msg:"count" int 2 (List.length (Tree.items tree))

let set_items_clamps_index () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  Tree.set_items tree [ Tree.item ~id:"only" "only" ];
  equal ~msg:"removed ID has no visible selection" int (-1)
    (Tree.selected_index tree)

let set_items_empty () =
  let _t, tree = make_tree ~items:sample_items () in
  Tree.set_items tree [];
  equal ~msg:"no selection" int (-1) (Tree.selected_index tree);
  equal ~msg:"no visible" int 0 (Tree.visible_count tree)

(* ── Queries ── *)

let depth_of_root () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  equal ~msg:"root depth" int 0 (Tree.depth_of tree 0)

let depth_of_child () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  (* Index 1 = main.ml, child of src *)
  equal ~msg:"child depth" int 1 (Tree.depth_of tree 1)

let depth_of_out_of_bounds () =
  let _t, tree = make_tree ~items:sample_items () in
  equal ~msg:"out of bounds" int 0 (Tree.depth_of tree 100)

(* ── Mouse ── *)

let mouse_click_selects () =
  let _t, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  (* Click on row 1 = "test" (all collapsed) *)
  emit_mouse tree (mouse_down ~x:10 ~y:1);
  equal ~msg:"selected" int 1 (Tree.selected_index tree)

let mouse_click_icon_toggles () =
  let _t, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  (* Click on icon area (x=0, y=0) of "src" (expandable, depth=0, icon at
     x=0..1) *)
  emit_mouse tree (mouse_down ~x:0 ~y:0);
  is_true ~msg:"expanded" (Tree.is_expanded tree 0)

let mouse_scroll_down_moves () =
  let _t, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  emit_mouse tree (mouse_scroll_down ~x:5 ~y:5);
  equal ~msg:"moved down" int 1 (Tree.selected_index tree)

let mouse_scroll_up_moves () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  with_layout tree ~width:40 ~height:10;
  emit_mouse tree (mouse_scroll_up ~x:5 ~y:5);
  equal ~msg:"moved up" int 1 (Tree.selected_index tree)

let mouse_click_selects_away_from_origin () =
  (* Mouse events carry absolute screen coordinates; the tree must translate
     them to widget-local space before mapping rows and icon columns. *)
  let _t, tree = make_tree ~items:sample_items () in
  layout_node (Tree.node tree) ~x:20 ~y:5 ~width:40 ~height:10;
  (* Click on the tree's second visible row: "test" (all collapsed). *)
  emit_mouse tree (mouse_down ~x:30 ~y:6);
  equal ~msg:"selected" int 1 (Tree.selected_index tree)

let mouse_click_icon_toggles_away_from_origin () =
  let _t, tree = make_tree ~items:sample_items () in
  layout_node (Tree.node tree) ~x:20 ~y:5 ~width:40 ~height:10;
  (* Click on the icon area of "src": local (0, 0). *)
  emit_mouse tree (mouse_down ~x:20 ~y:5);
  is_true ~msg:"expanded" (Tree.is_expanded tree 0)

let mouse_click_beyond_items_ignored () =
  let _t, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  emit_mouse tree (mouse_down ~x:5 ~y:8);
  equal ~msg:"no fire" (list (option string)) [] !log

(* ── Setter No-ops ── *)

(* The default background is transparent: a tree's blank cells show what is
   drawn beneath it. *)
let default_background_is_transparent () =
  let r = Renderer.create () in
  let under =
    Renderable.create ~parent:(Renderer.root r)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
        |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.))
      ()
  in
  Renderable.set_render under (fun _ grid ~dt:_ ->
      Matrix_grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:3
        ~color:Ansi.Color.red);
  let _tree =
    Tree.create ~parent:under
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
        |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.))
      ~items:[ Tree.item ~id:"a" "a" ]
      ()
  in
  Renderer.render_frame r ~width:10 ~height:3 ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen r) in
  is_true ~msg:"a blank cell shows the color beneath"
    (Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_bg grid ((2 * 10) + 5)))

let set_background_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_background tree (Ansi.Color.of_rgba 0 0 0 0);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_wrap_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_wrap_selection tree false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_indent_size_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_indent_size tree 2;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_show_guides_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_show_guides tree false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_text_color_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_text_color tree (Ansi.Color.of_rgb 255 255 255);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_fast_scroll_step_noop () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_fast_scroll_step tree 5;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── Setter Positive ── *)

let set_show_guides_toggle () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_show_guides tree true;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_indent_size_changes () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_indent_size tree 4;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_wrap_selection_enables_wrapping () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  Tree.set_wrap_selection tree true;
  emit_key tree (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Tree.selected_index tree)

let set_fast_scroll_step_changes_behavior () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  Tree.set_fast_scroll_step tree 2;
  emit_key tree (make_key ~shift:true Down);
  equal ~msg:"jumped by 2" int 2 (Tree.selected_index tree)

(* ── apply_props ── *)

let apply_props_updates () =
  let t, tree = make_tree ~items:sample_items () in
  let props =
    Tree.Props.make ~items:sample_items
      ~state:(state_for_items sample_items 1 0)
      ~wrap_selection:true ()
  in
  let before = !(t.schedule_count) in
  Tree.apply_props tree props;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  equal ~msg:"index applied" int 1 (Tree.selected_index tree)

let apply_props_same_no_extra_render () =
  let t, tree = make_tree ~items:sample_items () in
  let props = Tree.Props.make ~items:sample_items () in
  Tree.apply_props tree props;
  let before = !(t.schedule_count) in
  Tree.apply_props tree props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

let apply_props_restores_declared_selection () =
  let _t, tree = make_tree ~items:sample_items () in
  Tree.set_selected_index tree 2;
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  Tree.apply_props tree
    (Tree.Props.make ~items:sample_items ~wrap_selection:true ());
  equal ~msg:"missing selection explicitly declares none" int (-1)
    (Tree.selected_index tree);
  equal ~msg:"no callback echo" (list (option string)) [] !log

let apply_props_controlled_selection_is_silent () =
  let _t, tree = make_tree ~items:sample_items () in
  let log = ref [] in
  Tree.set_on_change tree
    (Some (fun state -> log := Tree.State.selected state :: !log));
  Tree.apply_props tree
    (Tree.Props.make ~items:sample_items
       ~state:(state_for_items sample_items 1 0)
       ());
  equal ~msg:"controlled selection applied" int 1 (Tree.selected_index tree);
  equal ~msg:"no callback echo" (list (option string)) [] !log

let rejected_state_and_reorder () =
  let _ctx, tree = make_tree ~items:sample_items () in
  let proposals = ref [] in
  Tree.set_on_change tree (Some (fun state -> proposals := state :: !proposals));
  emit_key tree (make_key Down);
  emit_key tree (make_key Down);
  equal ~msg:"rejected events derive from accepted selection"
    (list (option string))
    [ Some "test"; Some "test" ]
    (List.map Tree.State.selected !proposals);
  equal ~msg:"rejected proposal does not move selection" int 0
    (Tree.selected_index tree);
  let state = Tree.State.make ~selected:"src" ~expanded:[ "src" ] () in
  Tree.apply_props tree
    (Tree.Props.make ~items:(List.rev sample_items) ~state ());
  equal ~msg:"logical selection survives reorder" (option string) (Some "src")
    (Option.map (fun item -> item.Tree.id) (Tree.selected_item tree));
  equal ~msg:"expansion survives reorder" int 5 (Tree.visible_count tree)

let setters_work_without_callbacks () =
  let ctx, tree = make_tree ~items:sample_items () in
  Tree.set_on_change tree None;
  Tree.set_selected_index tree 1;
  equal ~msg:"selection needs no callback" int 1 (Tree.selected_index tree);
  Tree.expand tree 0;
  equal ~msg:"expansion needs no callback" int 5 (Tree.visible_count tree);
  equal ~msg:"selection follows its ID" int 3 (Tree.selected_index tree);
  Tree.toggle_expand tree 0;
  equal ~msg:"toggle needs no callback" int 3 (Tree.visible_count tree);
  Tree.expand_all tree;
  equal ~msg:"expand all needs no callback" int 6 (Tree.visible_count tree);
  let before = !(ctx.schedule_count) in
  Tree.expand_all tree;
  equal ~msg:"repeated expansion does not render" int before
    !(ctx.schedule_count);
  Tree.collapse_all tree;
  equal ~msg:"collapse all needs no callback" int 3 (Tree.visible_count tree);
  let before = !(ctx.schedule_count) in
  Tree.collapse_all tree;
  Tree.collapse tree 0;
  Tree.set_selected_index tree 1;
  equal ~msg:"accepted no-ops do not render" int before !(ctx.schedule_count)

let rejected_expansion_keeps_accepted_state () =
  let _ctx, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  let proposals = ref [] in
  Tree.set_on_change tree (Some (fun state -> proposals := state :: !proposals));
  emit_key tree (make_key Right);
  emit_key tree (make_key (Char (Uchar.of_char ' ')));
  emit_mouse tree (mouse_down ~x:0 ~y:0);
  equal ~msg:"each rejected gesture proposes expansion" (list bool)
    [ true; true; true ]
    (List.map (Tree.State.is_expanded "src") !proposals);
  equal ~msg:"rejected expansion keeps children hidden" int 3
    (Tree.visible_count tree);
  Tree.apply_props tree
    (Tree.Props.make ~items:sample_items
       ~state:(Tree.State.make ~selected:"src" ~expanded:[ "src" ] ())
       ());
  proposals := [];
  emit_key tree (make_key Left);
  emit_key tree (make_key (Char (Uchar.of_char ' ')));
  emit_mouse tree (mouse_down ~x:0 ~y:0);
  equal ~msg:"each rejected gesture proposes collapse" (list bool)
    [ false; false; false ]
    (List.map (Tree.State.is_expanded "src") !proposals);
  equal ~msg:"rejected collapse keeps children visible" int 5
    (Tree.visible_count tree)

(* ── Runner ── *)

(* ── State.step ── *)

(* a (a1, a2 (a2x)), b (b1), c *)
let step_items =
  Tree.
    [
      item ~id:"a"
        ~children:
          [
            item ~id:"a1" "Alpha one";
            item ~id:"a2" ~children:[ item ~id:"a2x" "Deep" ] "Alpha two";
          ]
        "Alpha";
      item ~id:"b" ~children:[ item ~id:"b1" "Beta one" ] "Beta";
      item ~id:"c" "Gamma";
    ]

let state ?selected ?(expanded = []) () = Tree.State.make ?selected ~expanded ()

let pp_step_outcome ppf = function
  | Tree.State.Changed s ->
      Format.fprintf ppf "Changed (%s, [%s])"
        (Option.value ~default:"none" (Tree.State.selected s))
        (String.concat ";" (Tree.State.expanded s))
  | Activated item -> Format.fprintf ppf "Activated %s" item.Tree.id
  | Nothing -> Format.fprintf ppf "Nothing"

let step_outcome =
  Testable.make ~pp:pp_step_outcome ~equal:(fun a b ->
      match (a, b) with
      | Tree.State.Changed a, Tree.State.Changed b -> Tree.State.equal a b
      | Activated a, Activated b -> String.equal a.id b.id
      | Nothing, Nothing -> true
      | _ -> false)

let changed ?selected ?expanded () =
  Tree.State.Changed (state ?selected ?expanded ())

let tree_step s a = Tree.State.step step_items s a

let step_moves () =
  let open Tree.State in
  let equal = Windtrap.equal in
  let s = state ~selected:"a" ~expanded:[ "a" ] () in
  equal step_outcome
    (changed ~selected:"a1" ~expanded:[ "a" ] ())
    (tree_step s Next);
  equal ~msg:"shown nodes only" step_outcome
    (changed ~selected:"b" ~expanded:[ "a" ] ())
    (tree_step (state ~selected:"a2" ~expanded:[ "a" ] ()) Next);
  equal ~msg:"no wrap" step_outcome Nothing
    (tree_step (state ~selected:"c" ()) Next);
  equal step_outcome (changed ~selected:"a" ())
    (tree_step (state ~selected:"c" ()) Next_wrap);
  equal ~msg:"from none, Down" step_outcome (changed ~selected:"a" ())
    (tree_step (state ()) Next);
  equal ~msg:"from none, Up" step_outcome (changed ~selected:"c" ())
    (tree_step (state ()) Prev);
  equal ~msg:"Home" step_outcome (changed ~selected:"a" ())
    (tree_step (state ~selected:"c" ()) First);
  equal ~msg:"End" step_outcome
    (changed ~selected:"c" ~expanded:[ "b" ] ())
    (tree_step (state ~selected:"a" ~expanded:[ "b" ] ()) Last);
  equal ~msg:"Up from the last root into an expanded node" step_outcome
    (changed ~selected:"b1" ~expanded:[ "b" ] ())
    (tree_step (state ~selected:"c" ~expanded:[ "b" ] ()) Prev);
  equal step_outcome (changed ~selected:"c" ())
    (tree_step (state ~selected:"a" ()) (By 5))

let step_right_left () =
  let open Tree.State in
  let equal = Windtrap.equal in
  equal ~msg:"Right opens a closed node" step_outcome
    (changed ~selected:"a" ~expanded:[ "a" ] ())
    (tree_step (state ~selected:"a" ()) Expand);
  equal ~msg:"Right on an open node moves to its first child" step_outcome
    (changed ~selected:"a1" ~expanded:[ "a" ] ())
    (tree_step (state ~selected:"a" ~expanded:[ "a" ] ()) Expand);
  equal ~msg:"Right on an end node" step_outcome Nothing
    (tree_step (state ~selected:"c" ()) Expand);
  equal ~msg:"Left closes an open node" step_outcome (changed ~selected:"a" ())
    (tree_step (state ~selected:"a" ~expanded:[ "a" ] ()) Collapse);
  equal ~msg:"Left on a child moves to its parent" step_outcome
    (changed ~selected:"a" ~expanded:[ "a" ] ())
    (tree_step (state ~selected:"a2" ~expanded:[ "a" ] ()) Collapse);
  equal ~msg:"Left on a closed root" step_outcome Nothing
    (tree_step (state ~selected:"b" ()) Collapse);
  equal ~msg:"Toggle" step_outcome
    (changed ~selected:"b" ~expanded:[ "b" ] ())
    (tree_step (state ~selected:"b" ()) Toggle);
  equal ~msg:"Toggle on an end node" step_outcome Nothing
    (tree_step (state ~selected:"c" ()) Toggle);
  equal ~msg:"Enter" step_outcome
    (Activated (Tree.item ~id:"c" "Gamma"))
    (tree_step (state ~selected:"c" ()) Activate)

let step_siblings () =
  let open Tree.State in
  let equal = Windtrap.equal in
  equal ~msg:"* opens every sibling" step_outcome
    (changed ~selected:"c" ~expanded:[ "a"; "b" ] ())
    (tree_step (state ~selected:"c" ()) Expand_siblings);
  equal ~msg:"siblings of a child" step_outcome
    (changed ~selected:"a1" ~expanded:[ "a"; "a2" ] ())
    (tree_step (state ~selected:"a1" ~expanded:[ "a" ] ()) Expand_siblings);
  equal ~msg:"all open already" step_outcome Nothing
    (tree_step (state ~selected:"c" ~expanded:[ "a"; "b" ] ()) Expand_siblings)

let typed s text =
  let rec go s i =
    if i = String.length text then s
    else
      match tree_step s (Typeahead (Uchar.of_char text.[i])) with
      | Tree.State.Changed s -> go s (i + 1)
      | o -> failf "%S at %d: %a" text i pp_step_outcome o
  in
  go s 0

let step_typeahead () =
  let selected s = Tree.State.selected s in
  equal ~msg:"shown nodes only" (option string) (Some "b")
    (selected (typed (state ~selected:"a" ()) "b"));
  equal ~msg:"a child once shown" (option string) (Some "b1")
    (selected (typed (state ~selected:"b" ~expanded:[ "b" ] ()) "beta "));
  equal ~msg:"restarts" (option string) (Some "c")
    (selected (typed (state ~selected:"a" ()) "ag"));
  equal ~msg:"no match" step_outcome Nothing
    (tree_step (state ~selected:"a" ()) (Typeahead (Uchar.of_char 'z')));
  is_false ~msg:"the prefix is part of the state"
    (Tree.State.equal (typed (state ()) "b") (state ~selected:"b" ()))

let step_hidden_cursor () =
  let open Tree.State in
  let equal = Windtrap.equal in
  let hidden = state ~selected:"a2x" () in
  equal ~msg:"Down from the shown ancestor" step_outcome
    (changed ~selected:"b" ()) (tree_step hidden Next);
  equal ~msg:"Enter activates the ancestor" step_outcome
    (Activated (List.hd step_items))
    (tree_step hidden Activate);
  equal ~msg:"Right opens the ancestor" step_outcome
    (changed ~selected:"a" ~expanded:[ "a" ] ())
    (tree_step hidden Expand);
  equal ~msg:"an inner collapsed node" step_outcome
    (changed ~selected:"a2" ~expanded:[ "a"; "a2" ] ())
    (tree_step (state ~selected:"a2x" ~expanded:[ "a" ] ()) Expand);
  equal ~msg:"an unknown id acts as none" step_outcome
    (changed ~selected:"a" ())
    (tree_step (state ~selected:"zz" ()) Next)

let () =
  run "mosaic.tree"
    [
      group "State.step"
        [
          test "moves over shown nodes" step_moves;
          test "Right and Left" step_right_left;
          test "* expands siblings" step_siblings;
          test "typeahead over shown nodes" step_typeahead;
          test "a cursor inside a collapsed node" step_hidden_cursor;
        ];
      test "accepted state and stable IDs" rejected_state_and_reorder;
      test "setters work without callbacks" setters_work_without_callbacks;
      test "rejected expansion preserves state"
        rejected_expansion_keeps_accepted_state;
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects items difference" props_detects_items_diff;
          test "detects selected_index difference"
            props_detects_selected_index_diff;
          test "detects expand_depth difference" props_detects_expand_depth_diff;
          test "detects show_guides difference" props_detects_show_guides_diff;
          test "detects color difference" props_detects_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "is focusable" create_is_focusable;
          test "is buffered" create_is_buffered;
          test "clamps initial index" create_clamps_initial_index;
          test "empty items index zero" create_empty_items_index_zero;
        ];
      group "Selection"
        [
          test "set_selected_index clamps" set_selected_index_clamps;
          test "set_selected_index is silent" set_selected_index_is_silent;
          test "no-op on same index" set_selected_index_noop_same;
          test "selected_item returns item" selected_item_returns_item;
          test "selected_item empty tree" selected_item_empty_tree;
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
          test "on_activate on empty tree" on_activate_empty_tree;
          test "unhandled key ignored" unhandled_key_ignored;
          test "navigation on empty tree" navigation_on_empty_tree;
        ];
      group "Expand/Collapse Keys"
        [
          test "Right expands collapsed" right_expands_collapsed;
          test "Right moves to first child" right_moves_to_first_child;
          test "Right on leaf no-op" right_on_leaf_noop;
          test "Left collapses expanded" left_collapses_expanded;
          test "Left moves to parent" left_moves_to_parent;
          test "Left on top-level leaf no-op" left_on_top_level_leaf_noop;
          test "Space toggles expand" space_toggles_expand;
        ];
      group "Expansion"
        [
          test "expand makes children visible" expand_makes_children_visible;
          test "collapse hides children" collapse_hides_children;
          test "expand_all shows everything" expand_all_shows_everything;
          test "collapse_all shows top level" collapse_all_shows_top_level;
          test "toggle_expand works" toggle_expand_works;
          test "expand is silent" expand_is_silent;
          test "collapse is silent" collapse_is_silent;
          test "expand leaf no-op" expand_leaf_noop;
          test "collapse adjusts selected index" collapse_adjusts_selected_index;
          test "expand_depth -1 expands all" expand_depth_negative_one;
          test "expand_depth 1 expands first level" expand_depth_one;
        ];
      group "Items"
        [
          test "set_items replaces" set_items_replaces;
          test "set_items clamps index" set_items_clamps_index;
          test "set_items empty" set_items_empty;
        ];
      group "Queries"
        [
          test "depth_of root" depth_of_root;
          test "depth_of child" depth_of_child;
          test "depth_of out of bounds" depth_of_out_of_bounds;
        ];
      group "Mouse"
        [
          test "click selects" mouse_click_selects;
          test "click icon toggles" mouse_click_icon_toggles;
          test "click selects away from origin"
            mouse_click_selects_away_from_origin;
          test "click icon toggles away from origin"
            mouse_click_icon_toggles_away_from_origin;
          test "scroll down moves" mouse_scroll_down_moves;
          test "scroll up moves" mouse_scroll_up_moves;
          test "click beyond items ignored" mouse_click_beyond_items_ignored;
        ];
      group "Setter no-ops"
        [
          test "default background is transparent"
            default_background_is_transparent;
          test "set_background no-op" set_background_noop;
          test "set_wrap_selection no-op" set_wrap_noop;
          test "set_indent_size no-op" set_indent_size_noop;
          test "set_show_guides no-op" set_show_guides_noop;
          test "set_text_color no-op" set_text_color_noop;
          test "set_fast_scroll_step no-op" set_fast_scroll_step_noop;
        ];
      group "Setter positive"
        [
          test "toggle show_guides" set_show_guides_toggle;
          test "indent_size changes" set_indent_size_changes;
          test "wrap_selection enables wrapping"
            set_wrap_selection_enables_wrapping;
          test "fast_scroll_step changes behavior"
            set_fast_scroll_step_changes_behavior;
        ];
      group "apply_props"
        [
          test "updates all properties" apply_props_updates;
          test "same props no extra render" apply_props_same_no_extra_render;
          test "preserves uncontrolled selection"
            apply_props_restores_declared_selection;
          test "controlled selection is silent"
            apply_props_controlled_selection_is_silent;
        ];
    ]
