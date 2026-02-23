open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let sample_items =
  Tree.
    [
      item "src" ~children:[ item "main.ml"; item "utils.ml" ];
      item "test" ~children:[ item "test_main.ml" ];
      item "README.md";
    ]

let deep_items =
  Tree.
    [
      item "a"
        ~children:[ item "b" ~children:[ item "c" ~children:[ item "d" ] ] ];
    ]

let make_tree ?items ?selected_index ?expand_depth ?indent_size ?show_guides
    ?guide_style ?wrap_selection ?fast_scroll_step () =
  let t = make_ctx () in
  let root = make_root t in
  let tree =
    Tree.create ~parent:root ?items ?selected_index ?expand_depth ?indent_size
      ?show_guides ?guide_style ?wrap_selection ?fast_scroll_step ()
  in
  (t, tree)

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

let emit_key tree key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Tree.node tree) ev

let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })

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
  let a = Tree.Props.make ~selected_index:0 () in
  let b = Tree.Props.make ~selected_index:1 () in
  is_false ~msg:"different" (Tree.Props.equal a b)

let props_detects_expand_depth_diff () =
  let a = Tree.Props.make ~expand_depth:0 () in
  let b = Tree.Props.make ~expand_depth:1 () in
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
  equal ~msg:"zero" int 0 (Tree.selected_index tree)

(* ── Selection ── *)

let set_selected_index_clamps () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  Tree.set_selected_index tree 100;
  is_true ~msg:"clamped high" (Tree.selected_index tree < 100);
  Tree.set_selected_index tree (-5);
  equal ~msg:"clamped low" int 0 (Tree.selected_index tree)

let set_selected_index_fires_on_change () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  let log = ref [] in
  Tree.set_on_change tree (Some (fun i -> log := i :: !log));
  Tree.set_selected_index tree 2;
  equal ~msg:"fired" (list int) [ 2 ] !log

let set_selected_index_noop_same () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:(-1) () in
  let log = ref [] in
  Tree.set_on_change tree (Some (fun i -> log := i :: !log));
  Tree.set_selected_index tree 0;
  equal ~msg:"no fire" (list int) [] !log

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
  Tree.set_on_activate tree (Some (fun i -> log := i :: !log));
  emit_key tree (make_key Enter);
  equal ~msg:"activated" (list int) [ 1 ] !log

let on_activate_empty_tree () =
  let _t, tree = make_tree () in
  let fired = ref false in
  Tree.set_on_activate tree (Some (fun _ -> fired := true));
  emit_key tree (make_key Enter);
  is_false ~msg:"not fired" !fired

let unhandled_key_ignored () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:1 () in
  let log = ref [] in
  Tree.set_on_change tree (Some (fun i -> log := i :: !log));
  emit_key tree (make_key (Char (Uchar.of_char 'a')));
  equal ~msg:"no change" (list int) [] !log;
  equal ~msg:"index unchanged" int 1 (Tree.selected_index tree)

let navigation_on_empty_tree () =
  let _t, tree = make_tree () in
  let log = ref [] in
  Tree.set_on_change tree (Some (fun i -> log := i :: !log));
  emit_key tree (make_key Down);
  emit_key tree (make_key Up);
  equal ~msg:"no callbacks" (list int) [] !log;
  equal ~msg:"index zero" int 0 (Tree.selected_index tree)

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

let expand_fires_on_expand () =
  let _t, tree = make_tree ~items:sample_items () in
  let log = ref [] in
  Tree.set_on_expand tree
    (Some (fun i expanded -> log := (i, expanded) :: !log));
  Tree.expand tree 0;
  equal ~msg:"fired" (list (pair int bool)) [ (0, true) ] !log

let collapse_fires_on_expand () =
  let _t, tree = make_tree ~items:sample_items ~expand_depth:1 () in
  let log = ref [] in
  Tree.set_on_expand tree
    (Some (fun i expanded -> log := (i, expanded) :: !log));
  Tree.collapse tree 0;
  equal ~msg:"fired" (list (pair int bool)) [ (0, false) ] !log

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
  let new_items = Tree.[ item "one"; item "two" ] in
  Tree.set_items tree new_items;
  equal ~msg:"count" int 2 (List.length (Tree.items tree))

let set_items_clamps_index () =
  let _t, tree = make_tree ~items:sample_items ~selected_index:2 () in
  Tree.set_items tree [ Tree.item "only" ];
  equal ~msg:"clamped" int 0 (Tree.selected_index tree)

let set_items_empty () =
  let _t, tree = make_tree ~items:sample_items () in
  Tree.set_items tree [];
  equal ~msg:"zero" int 0 (Tree.selected_index tree);
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

let mouse_click_beyond_items_ignored () =
  let _t, tree = make_tree ~items:sample_items () in
  with_layout tree ~width:40 ~height:10;
  let log = ref [] in
  Tree.set_on_change tree (Some (fun i -> log := i :: !log));
  emit_mouse tree (mouse_down ~x:5 ~y:8);
  equal ~msg:"no fire" (list int) [] !log

(* ── Setter No-ops ── *)

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
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_indent_size_changes () =
  let t, tree = make_tree () in
  let before = !(t.schedule_count) in
  Tree.set_indent_size tree 4;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

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
    Tree.Props.make ~items:sample_items ~selected_index:1 ~wrap_selection:true
      ()
  in
  let before = !(t.schedule_count) in
  Tree.apply_props tree props;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  equal ~msg:"index applied" int 1 (Tree.selected_index tree)

let apply_props_same_no_extra_render () =
  let t, tree = make_tree ~items:sample_items () in
  let props = Tree.Props.make ~items:sample_items () in
  Tree.apply_props tree props;
  let before = !(t.schedule_count) in
  Tree.apply_props tree props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

(* ── Runner ── *)

let () =
  run "mosaic.tree"
    [
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
          test "fires on_change" set_selected_index_fires_on_change;
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
          test "expand fires on_expand" expand_fires_on_expand;
          test "collapse fires on_expand" collapse_fires_on_expand;
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
          test "scroll down moves" mouse_scroll_down_moves;
          test "scroll up moves" mouse_scroll_up_moves;
          test "click beyond items ignored" mouse_click_beyond_items_ignored;
        ];
      group "Setter no-ops"
        [
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
        ];
    ]
