open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let sample_columns =
  [
    Table.column "Name";
    Table.column ~width:(`Fixed 5) ~alignment:`Right "Age";
    Table.column ~width:(`Flex 1.0) "City";
  ]

let sample_rows =
  [
    [| Table.cell "Alice"; Table.cell "30"; Table.cell "New York" |];
    [| Table.cell "Bob"; Table.cell "25"; Table.cell "London" |];
    [| Table.cell "Charlie"; Table.cell "35"; Table.cell "Paris" |];
    [| Table.cell "Diana"; Table.cell "28"; Table.cell "Tokyo" |];
    [| Table.cell "Eve"; Table.cell "42"; Table.cell "Berlin" |];
  ]

let make_table ?columns ?rows ?selected_row ?border ?border_style ?show_header
    ?show_column_separator ?show_row_separator ?wrap_selection ?fast_scroll_step
    () =
  let t = make_ctx () in
  let root = make_root t in
  let tbl =
    Table.create ~parent:root ?columns ?rows ?selected_row ?border ?border_style
      ?show_header ?show_column_separator ?show_row_separator ?wrap_selection
      ?fast_scroll_step ()
  in
  (t, tbl)

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

let emit_key tbl key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Table.node tbl) ev

let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })

let emit_mouse tbl ev = Renderable.Private.emit_mouse (Table.node tbl) ev

let with_layout tbl ~width ~height =
  layout_node (Table.node tbl) ~x:0 ~y:0 ~width ~height

(* ── Props ── *)

let props_defaults () =
  let p = Table.Props.default in
  is_true ~msg:"equal to make()" (Table.Props.equal p (Table.Props.make ()))

let props_equal_identical () =
  let a = Table.Props.make () in
  let b = Table.Props.make () in
  is_true ~msg:"equal" (Table.Props.equal a b)

let props_detects_columns_diff () =
  let a = Table.Props.make ~columns:sample_columns () in
  let b = Table.Props.make () in
  is_false ~msg:"different" (Table.Props.equal a b)

let props_detects_rows_diff () =
  let a = Table.Props.make ~rows:sample_rows () in
  let b = Table.Props.make () in
  is_false ~msg:"different" (Table.Props.equal a b)

let props_detects_selected_row_diff () =
  let a = Table.Props.make ~selected_row:0 () in
  let b = Table.Props.make ~selected_row:1 () in
  is_false ~msg:"different" (Table.Props.equal a b)

let props_detects_border_diff () =
  let a = Table.Props.make ~border:true () in
  let b = Table.Props.make ~border:false () in
  is_false ~msg:"different" (Table.Props.equal a b)

let props_detects_wrap_diff () =
  let a = Table.Props.make ~wrap_selection:true () in
  let b = Table.Props.make () in
  is_false ~msg:"different" (Table.Props.equal a b)

let props_detects_color_diff () =
  let a = Table.Props.make ~selected_background:Ansi.Color.red () in
  let b = Table.Props.make () in
  is_false ~msg:"different" (Table.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let node = Table.node tbl in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_is_focusable () =
  let _t, tbl = make_table () in
  is_true ~msg:"focusable" (Renderable.focusable (Table.node tbl))

let create_is_buffered () =
  let _t, tbl = make_table () in
  is_true ~msg:"buffered" (Renderable.buffered (Table.node tbl))

let create_clamps_initial_index () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:100 ()
  in
  equal ~msg:"clamped" int 4 (Table.selected_row tbl)

let create_empty_rows_index_zero () =
  let _t, tbl = make_table ~selected_row:5 () in
  equal ~msg:"zero" int 0 (Table.selected_row tbl)

(* ── Selection ── *)

let set_selected_row_clamps () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  Table.set_selected_row tbl 100;
  equal ~msg:"clamped high" int 4 (Table.selected_row tbl);
  Table.set_selected_row tbl (-5);
  equal ~msg:"clamped low" int 0 (Table.selected_row tbl)

let set_selected_row_fires_on_change () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  Table.set_selected_row tbl 2;
  equal ~msg:"fired" (list int) [ 2 ] !log

let set_selected_row_noop_same () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  Table.set_selected_row tbl 0;
  equal ~msg:"no fire" (list int) [] !log

let row_count_correct () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  equal ~msg:"count" int 5 (Table.row_count tbl)

let row_count_empty () =
  let _t, tbl = make_table () in
  equal ~msg:"zero" int 0 (Table.row_count tbl)

(* ── Navigation ── *)

let move_down_basic () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  emit_key tbl (make_key Down);
  equal ~msg:"index" int 1 (Table.selected_row tbl)

let move_up_basic () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:2 ()
  in
  emit_key tbl (make_key Up);
  equal ~msg:"index" int 1 (Table.selected_row tbl)

let move_down_j () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  emit_key tbl (make_key (Char (Uchar.of_char 'j')));
  equal ~msg:"index" int 1 (Table.selected_row tbl)

let move_up_k () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:2 ()
  in
  emit_key tbl (make_key (Char (Uchar.of_char 'k')));
  equal ~msg:"index" int 1 (Table.selected_row tbl)

let move_down_no_wrap () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4 ()
  in
  emit_key tbl (make_key Down);
  equal ~msg:"stays at end" int 4 (Table.selected_row tbl)

let move_up_no_wrap () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:0 ()
  in
  emit_key tbl (make_key Up);
  equal ~msg:"stays at start" int 0 (Table.selected_row tbl)

let move_down_wrap () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4
      ~wrap_selection:true ()
  in
  emit_key tbl (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Table.selected_row tbl)

let move_up_wrap () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:0
      ~wrap_selection:true ()
  in
  emit_key tbl (make_key Up);
  equal ~msg:"wraps to end" int 4 (Table.selected_row tbl)

let fast_scroll_down () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~fast_scroll_step:3 ()
  in
  emit_key tbl (make_key ~shift:true Down);
  equal ~msg:"jumped" int 3 (Table.selected_row tbl)

let fast_scroll_up () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4
      ~fast_scroll_step:3 ()
  in
  emit_key tbl (make_key ~shift:true Up);
  equal ~msg:"jumped" int 1 (Table.selected_row tbl)

let enter_fires_on_activate () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:2 ()
  in
  let log = ref [] in
  Table.set_on_activate tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key Enter);
  equal ~msg:"activated" (list int) [ 2 ] !log

let kp_enter_fires_on_activate () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:1 ()
  in
  let log = ref [] in
  Table.set_on_activate tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key KP_enter);
  equal ~msg:"activated" (list int) [ 1 ] !log

let on_change_fires_on_key_navigation () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key Down);
  equal ~msg:"fired" (list int) [ 1 ] !log

let on_activate_empty_table () =
  let _t, tbl = make_table () in
  let fired = ref false in
  Table.set_on_activate tbl (Some (fun _ -> fired := true));
  emit_key tbl (make_key Enter);
  is_false ~msg:"not fired" !fired

let unhandled_key_ignored () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:2 ()
  in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key (Char (Uchar.of_char 'a')));
  equal ~msg:"no change" (list int) [] !log;
  equal ~msg:"index unchanged" int 2 (Table.selected_row tbl)

let navigation_on_empty_table () =
  let _t, tbl = make_table () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key Down);
  emit_key tbl (make_key Up);
  equal ~msg:"no callbacks" (list int) [] !log;
  equal ~msg:"index zero" int 0 (Table.selected_row tbl)

let single_row_navigation () =
  let _t, tbl =
    make_table ~columns:sample_columns
      ~rows:[ [| Table.cell "Only"; Table.cell "1"; Table.cell "Here" |] ]
      ()
  in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  emit_key tbl (make_key Down);
  emit_key tbl (make_key Up);
  equal ~msg:"no callbacks" (list int) [] !log;
  equal ~msg:"stays at 0" int 0 (Table.selected_row tbl)

let fast_scroll_clamps_past_end () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~fast_scroll_step:10 ()
  in
  emit_key tbl (make_key ~shift:true Down);
  equal ~msg:"clamped to last" int 4 (Table.selected_row tbl)

let fast_scroll_clamps_past_start () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4
      ~fast_scroll_step:10 ()
  in
  emit_key tbl (make_key ~shift:true Up);
  equal ~msg:"clamped to 0" int 0 (Table.selected_row tbl)

(* ── Mouse ── *)

let mouse_click_selects_row () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true ()
  in
  with_layout tbl ~width:40 ~height:20;
  (* border_top=1, header=1, header_sep=1 -> data starts at y=3, row height=1 *)
  emit_mouse tbl (mouse_down ~x:5 ~y:4);
  equal ~msg:"selected" int 1 (Table.selected_row tbl)

let mouse_click_fires_on_change () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true ()
  in
  with_layout tbl ~width:40 ~height:20;
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  emit_mouse tbl (mouse_down ~x:5 ~y:5);
  equal ~msg:"fired" (list int) [ 2 ] !log

let mouse_scroll_down_moves () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  with_layout tbl ~width:40 ~height:20;
  emit_mouse tbl (mouse_scroll_down ~x:5 ~y:5);
  equal ~msg:"moved down" int 1 (Table.selected_row tbl)

let mouse_scroll_up_moves () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:3 ()
  in
  with_layout tbl ~width:40 ~height:20;
  emit_mouse tbl (mouse_scroll_up ~x:5 ~y:5);
  equal ~msg:"moved up" int 2 (Table.selected_row tbl)

(* ── Data ── *)

let set_rows_replaces () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let new_rows =
    [
      [| Table.cell "X"; Table.cell "1"; Table.cell "Y" |];
      [| Table.cell "Z"; Table.cell "2"; Table.cell "W" |];
    ]
  in
  Table.set_rows tbl new_rows;
  equal ~msg:"count" int 2 (Table.row_count tbl)

let set_rows_clamps_index () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4 ()
  in
  Table.set_rows tbl
    [ [| Table.cell "Only"; Table.cell "1"; Table.cell "Here" |] ];
  equal ~msg:"clamped" int 0 (Table.selected_row tbl)

let set_rows_empty () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  Table.set_rows tbl [];
  equal ~msg:"zero count" int 0 (Table.row_count tbl);
  equal ~msg:"zero index" int 0 (Table.selected_row tbl)

let set_rows_preserves_valid_index () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:1 ()
  in
  let extended =
    sample_rows
    @ [ [| Table.cell "Frank"; Table.cell "50"; Table.cell "Oslo" |] ]
  in
  Table.set_rows tbl extended;
  equal ~msg:"preserved" int 1 (Table.selected_row tbl)

let set_columns_replaces () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let new_cols = [ Table.column "A"; Table.column "B" ] in
  Table.set_columns tbl new_cols;
  equal ~msg:"count" int 2 (List.length (Table.columns tbl))

(* ── Cell equality ── *)

let cell_equal_plain () =
  is_true ~msg:"equal" (Table.cell_equal (Table.cell "a") (Table.cell "a"))

let cell_equal_plain_diff () =
  is_false ~msg:"different" (Table.cell_equal (Table.cell "a") (Table.cell "b"))

let cell_equal_rich_vs_plain () =
  is_false ~msg:"different kind"
    (Table.cell_equal
       (Table.rich [ Text.Text { text = "a"; style = None } ])
       (Table.cell "a"))

(* ── Setter no-ops ── *)

let set_border_noop () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_border tbl true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_wrap_noop () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_wrap_selection tbl false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_show_header_noop () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_show_header tbl true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_text_color_noop () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_text_color tbl (Ansi.Color.of_rgb 255 255 255);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_fast_scroll_step_noop () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_fast_scroll_step tbl 5;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── Setter positive ── *)

let set_border_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_border tbl false;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_show_header_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_show_header tbl false;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_show_column_separator_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_show_column_separator tbl true;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_wrap_selection_enables_wrapping () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:4 ()
  in
  Table.set_wrap_selection tbl true;
  emit_key tbl (make_key Down);
  equal ~msg:"wraps to 0" int 0 (Table.selected_row tbl)

let set_fast_scroll_step_changes_behavior () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  Table.set_fast_scroll_step tbl 2;
  emit_key tbl (make_key ~shift:true Down);
  equal ~msg:"jumped by 2" int 2 (Table.selected_row tbl)

(* ── apply_props ── *)

let apply_props_updates () =
  let t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let props =
    Table.Props.make ~columns:sample_columns ~rows:sample_rows ~selected_row:3
      ~wrap_selection:true ()
  in
  let before = !(t.schedule_count) in
  Table.apply_props tbl props;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  equal ~msg:"index applied" int 3 (Table.selected_row tbl)

let apply_props_same_no_extra_render () =
  let t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let props = Table.Props.make ~columns:sample_columns ~rows:sample_rows () in
  Table.apply_props tbl props;
  let before = !(t.schedule_count) in
  Table.apply_props tbl props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

(* ── Runner ── *)

let () =
  run "mosaic.table"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects columns difference" props_detects_columns_diff;
          test "detects rows difference" props_detects_rows_diff;
          test "detects selected_row difference" props_detects_selected_row_diff;
          test "detects border difference" props_detects_border_diff;
          test "detects wrap difference" props_detects_wrap_diff;
          test "detects color difference" props_detects_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "is focusable" create_is_focusable;
          test "is buffered" create_is_buffered;
          test "clamps initial index" create_clamps_initial_index;
          test "empty rows index zero" create_empty_rows_index_zero;
        ];
      group "Selection"
        [
          test "set_selected_row clamps" set_selected_row_clamps;
          test "fires on_change" set_selected_row_fires_on_change;
          test "no-op on same index" set_selected_row_noop_same;
          test "row_count correct" row_count_correct;
          test "row_count empty" row_count_empty;
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
          test "on_activate on empty table" on_activate_empty_table;
          test "unhandled key ignored" unhandled_key_ignored;
          test "navigation on empty table" navigation_on_empty_table;
          test "single row navigation" single_row_navigation;
          test "fast scroll clamps past end" fast_scroll_clamps_past_end;
          test "fast scroll clamps past start" fast_scroll_clamps_past_start;
        ];
      group "Mouse"
        [
          test "click selects row" mouse_click_selects_row;
          test "click fires on_change" mouse_click_fires_on_change;
          test "scroll down moves" mouse_scroll_down_moves;
          test "scroll up moves" mouse_scroll_up_moves;
        ];
      group "Data"
        [
          test "set_rows replaces" set_rows_replaces;
          test "set_rows clamps index" set_rows_clamps_index;
          test "set_rows empty" set_rows_empty;
          test "set_rows preserves valid index" set_rows_preserves_valid_index;
          test "set_columns replaces" set_columns_replaces;
        ];
      group "Cell equality"
        [
          test "plain equal" cell_equal_plain;
          test "plain different" cell_equal_plain_diff;
          test "rich vs plain" cell_equal_rich_vs_plain;
        ];
      group "Setter no-ops"
        [
          test "set_border no-op" set_border_noop;
          test "set_wrap_selection no-op" set_wrap_noop;
          test "set_show_header no-op" set_show_header_noop;
          test "set_text_color no-op" set_text_color_noop;
          test "set_fast_scroll_step no-op" set_fast_scroll_step_noop;
        ];
      group "Setter positive"
        [
          test "toggle border" set_border_toggle;
          test "toggle show_header" set_show_header_toggle;
          test "toggle show_column_separator" set_show_column_separator_toggle;
          test "wrap_selection enables wrapping"
            set_wrap_selection_enables_wrapping;
          test "fast_scroll_step changes behavior"
            set_fast_scroll_step_changes_behavior;
        ];
      group "apply_props"
        [
          test "updates all properties" apply_props_updates;
          test "same data no extra render" apply_props_same_no_extra_render;
        ];
    ]
