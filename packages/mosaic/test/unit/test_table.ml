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

let paging_rows =
  List.init 12 (fun index -> [| Table.cell ("row " ^ string_of_int index) |])

let make_table ?columns ?rows ?selected_row ?border ?border_style ?show_header
    ?show_column_separator ?show_row_separator ?selection_visible
    ?show_scroll_indicator ?activate_on_click ?wheel_navigation ?wrap_selection
    ?fast_scroll_step ?background ?selected_background () =
  let t = make_ctx () in
  let root = make_root t in
  let tbl =
    Table.create ~parent:root ?columns ?rows ?selected_row ?border ?border_style
      ?show_header ?show_column_separator ?show_row_separator ?wrap_selection
      ?selection_visible ?show_scroll_indicator ?activate_on_click
      ?wheel_navigation ?fast_scroll_step ?background ?selected_background ()
  in
  Table.set_on_change tbl (Some (Table.set_selected_row tbl));
  (t, tbl)

let emit_key tbl key =
  let ev = Event.Key.of_input key in
  Renderable.Private.emit_key (Table.node tbl) ev;
  if
    (not (Event.Key.default_prevented ev))
    && not (Event.Key.propagation_stopped ev)
  then Renderable.Private.run_key_behavior (Table.node tbl) ev

let emit_mouse tbl ev = Renderable.Private.emit_mouse (Table.node tbl) ev

let with_layout tbl ~width ~height =
  layout_node (Table.node tbl) ~x:0 ~y:0 ~width ~height

let render_with_layout tbl ~width ~height =
  let node = Table.node tbl in
  with_layout tbl ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~dt:0.

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

let props_detects_presentation_diff () =
  let defaults = Table.Props.make () in
  is_false ~msg:"selection visibility"
    (Table.Props.equal defaults (Table.Props.make ~selection_visible:false ()));
  is_false ~msg:"scroll indicator"
    (Table.Props.equal defaults
       (Table.Props.make ~show_scroll_indicator:true ()));
  is_false ~msg:"click activation"
    (Table.Props.equal defaults (Table.Props.make ~activate_on_click:true ()));
  is_false ~msg:"wheel navigation"
    (Table.Props.equal defaults (Table.Props.make ~wheel_navigation:false ()))

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

let create_empty_rows_has_no_selection () =
  let _t, tbl = make_table ~selected_row:5 () in
  equal ~msg:"zero" int (-1) (Table.selected_row tbl)

(* ── Selection ── *)

let set_selected_row_clamps () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  Table.set_selected_row tbl 100;
  equal ~msg:"clamped high" int 4 (Table.selected_row tbl);
  Table.set_selected_row tbl (-5);
  equal ~msg:"negative clears selection" int (-1) (Table.selected_row tbl)

let set_selected_row_fires_on_change () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  Table.set_selected_row tbl 2;
  equal ~msg:"accepted setter is silent" (list int) [] !log

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

let page_down_uses_bordered_body_height () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "Name" ]
      ~rows:paging_rows ~selected_row:2 ~border:true ~show_header:true ()
  in
  render_with_layout tbl ~width:20 ~height:8;
  let changes = ref [] in
  Table.set_on_change tbl
    (Some
       (fun index ->
         Table.set_selected_row tbl index;
         changes := index :: !changes));
  emit_key tbl (make_key Page_down);
  equal ~msg:"moves by four measured data rows" int 6 (Table.selected_row tbl);
  equal ~msg:"reports the new selection" (list int) [ 6 ] !changes

let page_up_accounts_for_row_separators () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "Name" ]
      ~rows:paging_rows ~selected_row:7 ~border:false ~show_header:false
      ~show_row_separator:true ()
  in
  render_with_layout tbl ~width:20 ~height:5;
  let changes = ref [] in
  Table.set_on_change tbl
    (Some
       (fun index ->
         Table.set_selected_row tbl index;
         changes := index :: !changes));
  emit_key tbl (make_key Page_up);
  equal ~msg:"moves by three measured data rows" int 4 (Table.selected_row tbl);
  equal ~msg:"reports the new selection" (list int) [ 4 ] !changes

let page_navigation_clamps_when_selection_wraps () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "Name" ]
      ~rows:paging_rows ~selected_row:1 ~border:false ~show_header:false
      ~wrap_selection:true ()
  in
  render_with_layout tbl ~width:20 ~height:4;
  let changes = ref [] in
  Table.set_on_change tbl
    (Some
       (fun index ->
         Table.set_selected_row tbl index;
         changes := index :: !changes));
  emit_key tbl (make_key Page_up);
  emit_key tbl (make_key Page_up);
  equal ~msg:"page up clamps at the first row" int 0 (Table.selected_row tbl);
  equal ~msg:"only the changed selection is reported" (list int) [ 0 ] !changes;
  Table.set_selected_row tbl 10;
  changes := [];
  emit_key tbl (make_key Page_down);
  emit_key tbl (make_key Page_down);
  equal ~msg:"page down clamps at the last row" int 11 (Table.selected_row tbl);
  equal ~msg:"only the changed selection is reported" (list int) [ 11 ] !changes

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
  emit_key tbl (make_key ~keypad:true Enter);
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
  equal ~msg:"index zero" int (-1) (Table.selected_row tbl)

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

let mouse_click_can_activate_exact_row () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true
      ~activate_on_click:true ()
  in
  with_layout tbl ~width:40 ~height:20;
  let log = ref [] in
  Table.set_on_activate tbl (Some (fun i -> log := i :: !log));
  emit_mouse tbl (mouse_down ~x:5 ~y:5);
  equal ~msg:"activated" (list int) [ 2 ] !log

let mouse_hover_reports_exact_rows () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true ()
  in
  with_layout tbl ~width:40 ~height:20;
  let log = ref [] in
  Table.set_on_hover tbl (Some (fun row -> log := row :: !log));
  emit_mouse tbl (mouse_move ~x:5 ~y:4);
  emit_mouse tbl (mouse_move ~x:8 ~y:4);
  emit_mouse tbl (mouse_move ~x:5 ~y:5);
  emit_mouse tbl (mouse_out ~x:5 ~y:5);
  equal ~msg:"transitions" (list (option int)) [ None; Some 2; Some 1 ] !log

let mouse_hover_excludes_row_separators () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true
      ~show_row_separator:true ()
  in
  with_layout tbl ~width:40 ~height:20;
  let log = ref [] in
  Table.set_on_hover tbl (Some (fun row -> log := row :: !log));
  emit_mouse tbl (mouse_move ~x:5 ~y:3);
  emit_mouse tbl (mouse_move ~x:5 ~y:4);
  equal ~msg:"separator clears hover" (list (option int)) [ None; Some 0 ] !log

let mouse_hover_and_click_exclude_table_chrome () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:true
      ~show_scroll_indicator:true ~activate_on_click:true ()
  in
  with_layout tbl ~width:12 ~height:5;
  let hover_log = ref [] in
  let activate_log = ref [] in
  Table.set_on_hover tbl (Some (fun row -> hover_log := row :: !hover_log));
  Table.set_on_activate tbl
    (Some (fun row -> activate_log := row :: !activate_log));
  emit_mouse tbl (mouse_move ~x:1 ~y:3);
  emit_mouse tbl (mouse_move ~x:0 ~y:3);
  emit_mouse tbl (mouse_move ~x:10 ~y:3);
  emit_mouse tbl (mouse_down ~x:10 ~y:3);
  equal ~msg:"border and indicator clear hover"
    (list (option int))
    [ None; Some 0 ] !hover_log;
  equal ~msg:"chrome does not activate" (list int) [] !activate_log

(* Hover is the row under the pointer: a selection that scrolls the rows
   under a still pointer reports the row now there. *)
let scrolling_reports_row_under_pointer () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:false
      ~show_header:false ()
  in
  with_layout tbl ~width:40 ~height:2;
  let log = ref [] in
  Table.set_on_hover tbl (Some (fun row -> log := row :: !log));
  emit_mouse tbl (mouse_move ~x:5 ~y:0);
  Table.set_selected_row tbl 4;
  equal ~msg:"the row scrolled under the pointer"
    (list (option int))
    [ Some 3; Some 0 ] !log

(* New rows under a still pointer are reported when another row comes under
   it, even at the index last reported. Equal rows report nothing. *)
let changed_rows_report_row_under_pointer () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~border:false
      ~show_header:false ()
  in
  with_layout tbl ~width:40 ~height:5;
  let log = ref [] in
  Table.set_on_hover tbl (Some (fun row -> log := row :: !log));
  emit_mouse tbl (mouse_move ~x:5 ~y:3);
  Table.set_rows tbl (List.tl sample_rows);
  Table.set_rows tbl (List.map Array.copy (List.tl sample_rows));
  Table.set_rows tbl (List.tl (List.tl sample_rows));
  emit_mouse tbl (mouse_move ~x:8 ~y:3);
  equal ~msg:"a report for each new data set"
    (list (option int))
    [ None; Some 3; Some 3 ] !log

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

let mouse_scroll_can_bubble () =
  let _t, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~wheel_navigation:false
      ()
  in
  with_layout tbl ~width:40 ~height:20;
  let event = mouse_scroll_down ~x:5 ~y:5 in
  emit_mouse tbl event;
  equal ~msg:"selection unchanged" int 0 (Table.selected_row tbl);
  is_false ~msg:"event not consumed" (Event.Mouse.propagation_stopped event)

let selection_can_be_visually_hidden () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "Name" ]
      ~rows:[ [| Table.cell "Alice" |] ]
      ~border:false ~show_header:false ~selection_visible:false
      ~background:Ansi.Color.blue ~selected_background:Ansi.Color.red ()
  in
  let node = Table.node tbl in
  with_layout tbl ~width:10 ~height:1;
  let grid = make_grid ~width:10 ~height:1 () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  let background = Matrix_grid.get_bg grid (Matrix_grid.idx grid ~x:0 ~y:0) in
  is_true ~msg:"base background retained"
    (Ansi.Color.equal Ansi.Color.blue background)

let scroll_indicator_handles_tiny_layouts () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "Name" ]
      ~rows:sample_rows ~border:false ~show_header:false
      ~show_scroll_indicator:true ()
  in
  let node = Table.node tbl in
  let grid = make_grid ~width:1 ~height:1 () in
  with_layout tbl ~width:0 ~height:0;
  Renderable.Private.render_full node ~grid ~dt:0.;
  with_layout tbl ~width:1 ~height:1;
  Renderable.Private.render_full node ~grid ~dt:0.;
  equal ~msg:"one-column indicator" string "↓"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:0 ~y:0))

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
  equal ~msg:"zero index" int (-1) (Table.selected_row tbl)

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

(* ── Rendering ── *)

let rich_cell_span_styles_apply () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "C" ]
      ~rows:
        [ [| Table.rich [ Text.Fragment.bold [ Text.Fragment.text "B" ] ] |] ]
      ~border:false ~show_header:false ()
  in
  let node = Table.node tbl in
  with_layout tbl ~width:5 ~height:1;
  let grid = make_grid ~width:5 ~height:1 () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  let idx = Matrix_grid.idx grid ~x:0 ~y:0 in
  equal ~msg:"cell text" string "B" (Matrix_grid.get_text grid idx);
  let style = Matrix_grid.get_style grid idx in
  is_true ~msg:"span bold applies" (Ansi.Attr.mem Bold style.Ansi.Style.attrs)

let auto_widths_follow_screen_width_method () =
  (* The ZWJ family emoji measures 2 columns under [`Unicode] but 6 under
     [`Wcwidth]; column sizing must agree with the grid the table renders
     into, so the second column lands after 6 cells plus the 1-cell gap. *)
  let t = make_ctx ~width_method:`Wcwidth () in
  let root = make_root t in
  let tbl =
    Table.create ~parent:root
      ~columns:[ Table.column "A"; Table.column "B" ]
      ~rows:
        [
          [|
            Table.cell
              "\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA6";
            Table.cell "X";
          |];
        ]
      ~border:false ~show_header:false ()
  in
  let node = Table.node tbl in
  layout_node node ~x:0 ~y:0 ~width:12 ~height:1;
  let grid = make_grid ~width:12 ~height:1 ~width_method:`Wcwidth () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  equal ~msg:"second column follows wcwidth measurement" string "X"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:7 ~y:0))

let auto_widths_recompute_after_set_rows () =
  (* Auto measurements are cached across renders; replacing the data must
     invalidate the cache so column sizing tracks the new cells. *)
  let t = make_ctx () in
  let root = make_root t in
  let tbl =
    Table.create ~parent:root
      ~columns:[ Table.column "A"; Table.column "B" ]
      ~rows:[ [| Table.cell "aa"; Table.cell "X" |] ]
      ~border:false ~show_header:false ()
  in
  let node = Table.node tbl in
  layout_node node ~x:0 ~y:0 ~width:12 ~height:1;
  let grid = make_grid ~width:12 ~height:1 () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  equal ~msg:"initial column width" string "X"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:3 ~y:0));
  Table.set_rows tbl [ [| Table.cell "aaaa"; Table.cell "X" |] ];
  let grid = make_grid ~width:12 ~height:1 () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  equal ~msg:"width follows the new data" string "X"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:5 ~y:0))

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

let set_cell_padding_pads_each_side () =
  let _t, tbl =
    make_table
      ~columns:[ Table.column "A"; Table.column "B" ]
      ~rows:[ [| Table.cell "1"; Table.cell "2" |] ]
      ~border:false ~show_header:false ()
  in
  Table.set_cell_padding tbl 2;
  with_layout tbl ~width:12 ~height:1;
  let grid = make_grid ~width:12 ~height:1 () in
  Renderable.Private.render_full (Table.node tbl) ~grid ~dt:0.;
  equal ~msg:"two columns of padding on each side of each cell" string
    "  1     2" (Matrix_grid.to_text grid)

let set_border_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_border tbl false;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_show_header_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_show_header tbl false;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_show_column_separator_toggle () =
  let t, tbl = make_table () in
  let before = !(t.schedule_count) in
  Table.set_show_column_separator tbl true;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

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
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  equal ~msg:"index applied" int 3 (Table.selected_row tbl)

let apply_props_same_no_extra_render () =
  let t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let props = Table.Props.make ~columns:sample_columns ~rows:sample_rows () in
  Table.apply_props tbl props;
  let before = !(t.schedule_count) in
  Table.apply_props tbl props;
  equal ~msg:"no extra schedule" int before !(t.schedule_count)

let apply_props_restores_declared_selection () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  Table.set_selected_row tbl 3;
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  Table.apply_props tbl
    (Table.Props.make ~columns:sample_columns ~rows:sample_rows
       ~wrap_selection:true ());
  equal ~msg:"omitted selection declares zero" int 0 (Table.selected_row tbl);
  equal ~msg:"no callback echo" (list int) [] !log

let apply_props_controlled_selection_is_silent () =
  let _t, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let log = ref [] in
  Table.set_on_change tbl (Some (fun i -> log := i :: !log));
  Table.apply_props tbl
    (Table.Props.make ~columns:sample_columns ~rows:sample_rows ~selected_row:2
       ());
  equal ~msg:"controlled selection applied" int 2 (Table.selected_row tbl);
  equal ~msg:"no callback echo" (list int) [] !log

(* ── Runner ── *)

let proposals_do_not_drift () =
  let _ctx, tbl = make_table ~columns:sample_columns ~rows:sample_rows () in
  let proposals = ref [] in
  Table.set_on_change tbl (Some (fun next -> proposals := next :: !proposals));
  emit_key tbl (make_key Down);
  emit_key tbl (make_key Down);
  equal ~msg:"rejected events derive from accepted state" (list int) [ 1; 1 ]
    !proposals;
  equal ~msg:"proposal does not mutate selection" int 0 (Table.selected_row tbl);
  Table.apply_props tbl
    (Table.Props.make ~columns:sample_columns ~rows:sample_rows ~selected_row:2
       ());
  emit_key tbl (make_key Down);
  equal ~msg:"transformed accepted state drives next proposal" (list int)
    [ 3; 1; 1 ] !proposals

let absent_selection_is_preserved () =
  let context, tbl =
    make_table ~columns:sample_columns ~rows:sample_rows ~selected_row:(-1)
      ~border:false ~show_header:false ~selected_background:Ansi.Color.red ()
  in
  ignore context;
  let activations = ref [] in
  Table.set_on_activate tbl (Some (fun i -> activations := i :: !activations));
  emit_key tbl (make_key Enter);
  equal ~msg:"Enter does not activate an absent row" (list int) [] !activations;
  with_layout tbl ~width:30 ~height:5;
  let grid = make_grid ~width:30 ~height:5 () in
  Renderable.Private.render_full (Table.node tbl) ~grid ~dt:0.;
  for i = 0 to 149 do
    match (Matrix_grid.get_style grid i).bg with
    | Some color ->
        is_false ~msg:"no selected background"
          (Ansi.Color.equal color Ansi.Color.red)
    | None -> ()
  done;
  emit_key tbl (make_key ~shift:true Down);
  equal ~msg:"first Down selects first even with a larger step" int 0
    (Table.selected_row tbl);
  Table.apply_props tbl
    (Table.Props.make ~columns:sample_columns ~rows:sample_rows
       ~selected_row:(-1) ~border:false ~show_header:false ());
  emit_key tbl (make_key Up);
  equal ~msg:"first Up selects last without wrapping" int 4
    (Table.selected_row tbl);
  Table.set_selected_row tbl (-1);
  emit_mouse tbl (mouse_down ~x:5 ~y:1);
  equal ~msg:"click selects a valid row from absence" int 1
    (Table.selected_row tbl)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.table}) or to {!Table.create},
   reaches the live table ({!Test_harness.reaches}). The element's rows are
   rows of values and its [selected] a value, which give the table's rows of
   cells and [selected_row]. Keys, clicks and the wheel propose a selection
   through [on_change] and an activation through [on_activate], which both
   doors record. *)

let doors_size = Mosaic.size ~width:24 ~height:6
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let doors_columns = [ Table.column "Name"; Table.column "N" ]

let cells i =
  [| Table.cell ("row" ^ string_of_int i); Table.cell (string_of_int i) |]

let few_rows = List.init 3 (fun i -> Mosaic.Table.row i (cells i))
let few_cells = List.init 3 cells
let many_rows = List.init 12 (fun i -> Mosaic.Table.row i (cells i))
let many_cells = List.init 12 cells
let doors_proposals = ref []
let doors_record i = doors_proposals := string_of_int i :: !doors_proposals

let doors_activated i =
  doors_proposals := ("activate " ^ string_of_int i) :: !doors_proposals

let doors_recording table =
  Table.set_on_change table (Some doors_record);
  Table.set_on_activate table (Some doors_activated);
  Table.node table

let doors_proposed _ _ = String.concat ";" (List.rev !doors_proposals)

let doors_press key renderer node =
  doors_proposals := [];
  Test_harness.press_key key renderer node

(* A click on the second data row, under the border and the header. *)
let doors_click renderer node =
  ignore (Renderer.render ~full:true renderer : string);
  doors_proposals := [];
  let x = Renderable.x node + 2 and y = Renderable.y node + 4 in
  ignore
    (Test_harness.dispatch_mouse renderer (mouse_report ~x ~y (Press Left)));
  ignore
    (Test_harness.dispatch_mouse renderer
       (mouse_report ~x ~y (Release (Some Left))))

let doors_wheel renderer node =
  ignore (Renderer.render ~full:true renderer : string);
  doors_proposals := [];
  let x = Renderable.x node + 2 and y = Renderable.y node + 3 in
  ignore
    (Test_harness.dispatch_mouse renderer (mouse_report ~x ~y (Scroll `Down)))

let doors_columns_label () =
  let other = [ Table.column "Name"; Table.column "Number" ] in
  Test_harness.reaches "columns"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~columns:other ~rows:few_rows ())
    ~create_base:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:other ~rows:few_cells
           ()))

let doors_rows () =
  Test_harness.reaches "rows"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~columns:doors_columns ~rows:many_rows ())
    ~create_base:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:many_cells ()))

let doors_selected_row () =
  Test_harness.reaches "selected_row (the element's selected)"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:ignore ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 1)
         ~on_change:ignore ~columns:doors_columns ~rows:few_rows ())
    ~create_base:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      Table.node
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~selected_row:1 ()))

let doors_border () =
  Test_harness.reaches "border"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~border:false ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~border:false ()))

let doors_border_style () =
  Test_harness.reaches "border_style"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows
         ~border_style:Matrix_grid.Border.double ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~border_style:Matrix_grid.Border.double ()))

let doors_show_header () =
  Test_harness.reaches "show_header"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~show_header:false ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~show_header:false ()))

let doors_show_column_separator () =
  Test_harness.reaches "show_column_separator"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~show_column_separator:true ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~show_column_separator:true ()))

let doors_show_row_separator () =
  Test_harness.reaches "show_row_separator"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~show_row_separator:true ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~show_row_separator:true ()))

let doors_cell_padding () =
  Test_harness.reaches "cell_padding"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~cell_padding:1 ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~cell_padding:1 ()))

let doors_header_color () =
  Test_harness.reaches "header_color"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~header_color:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~header_color:doors_red ()))

let doors_header_background () =
  Test_harness.reaches "header_background"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~header_background:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~header_background:doors_red ()))

let doors_text_color () =
  Test_harness.reaches "text_color"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~text_color:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~text_color:doors_red ()))

let doors_background () =
  Test_harness.reaches "background"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~background:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~background:doors_red ()))

let doors_selected_text_color () =
  Test_harness.reaches "selected_text_color"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~selected_text_color:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~selected_text_color:doors_red ()))

let doors_selected_background () =
  Test_harness.reaches "selected_background"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~selected_background:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~selected_background:doors_red ()))

let doors_focused_selected_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_selected_text_color"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows
         ~focused_selected_text_color:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~focused_selected_text_color:doors_red ()))

let doors_focused_selected_background () =
  Test_harness.reaches ~act:Test_harness.focus "focused_selected_background"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows
         ~focused_selected_background:doors_red ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~focused_selected_background:doors_red ()))

let doors_selection_visible () =
  Test_harness.reaches "selection_visible"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~selection_visible:false ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~selection_visible:false ()))

let doors_show_scroll_indicator () =
  Test_harness.reaches "show_scroll_indicator"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:many_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:many_rows ~show_scroll_indicator:true ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:many_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:many_cells ~show_scroll_indicator:true ()))

let doors_activate_on_click () =
  Test_harness.reaches ~act:doors_click ~observe:doors_proposed
    "activate_on_click"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~activate_on_click:true ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~activate_on_click:true ()))

let doors_wheel_navigation () =
  Test_harness.reaches ~act:doors_wheel ~observe:doors_proposed
    "wheel_navigation"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~wheel_navigation:false ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~wheel_navigation:false ()))

let doors_row_styles () =
  Test_harness.reaches "row_styles"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows
         ~row_styles:[ Ansi.Style.default; Ansi.Style.make ~bg:doors_red () ]
         ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells
           ~row_styles:[ Ansi.Style.default; Ansi.Style.make ~bg:doors_red () ]
           ()))

let doors_wrap_selection () =
  Test_harness.reaches
    ~act:(doors_press (make_key Up))
    ~observe:doors_proposed "wrap_selection"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ~wrap_selection:true ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ~wrap_selection:true ()))

let doors_key_bindings () =
  Test_harness.reaches
    ~act:(doors_press (char_key 'x'))
    ~observe:doors_proposed "key_bindings"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:few_rows
         ~key_bindings:[ (Shortcut.char 'x', Table.Move_down) ]
         ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:few_cells
           ~key_bindings:[ (Shortcut.char 'x', Table.Move_down) ]
           ()))

let doors_fast_scroll_step () =
  Test_harness.reaches
    ~act:(doors_press (make_key ~shift:true Down))
    ~observe:doors_proposed "fast_scroll_step"
    ~base:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:many_rows ())
    ~element:
      (Mosaic.table ~size:doors_size ~equal:Int.equal ~selected:(Some 0)
         ~on_change:doors_record ~on_activate:doors_activated
         ~columns:doors_columns ~rows:many_rows ~fast_scroll_step:2 ())
    ~create_base:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:many_cells ()))
    ~create:(fun parent ->
      doors_recording
        (Table.create ~parent ~style:doors_style ~columns:doors_columns
           ~rows:many_cells ~fast_scroll_step:2 ()))

let () =
  run "mosaic.table"
    [
      test "proposals do not drift" proposals_do_not_drift;
      test "absent selection is preserved" absent_selection_is_preserved;
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
          test "detects presentation differences"
            props_detects_presentation_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "is focusable" create_is_focusable;
          test "is buffered" create_is_buffered;
          test "clamps initial index" create_clamps_initial_index;
          test "empty rows have no selection" create_empty_rows_has_no_selection;
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
          test "page down uses bordered body height"
            page_down_uses_bordered_body_height;
          test "page up accounts for row separators"
            page_up_accounts_for_row_separators;
          test "page navigation clamps when selection wraps"
            page_navigation_clamps_when_selection_wraps;
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
          test "click can activate exact row" mouse_click_can_activate_exact_row;
          test "hover reports exact rows" mouse_hover_reports_exact_rows;
          test "hover excludes separators" mouse_hover_excludes_row_separators;
          test "hover and click exclude table chrome"
            mouse_hover_and_click_exclude_table_chrome;
          test "scrolling reports the row under the pointer"
            scrolling_reports_row_under_pointer;
          test "changed rows report the row under the pointer"
            changed_rows_report_row_under_pointer;
          test "scroll down moves" mouse_scroll_down_moves;
          test "scroll up moves" mouse_scroll_up_moves;
          test "scroll can bubble" mouse_scroll_can_bubble;
          test "selection can be visually hidden"
            selection_can_be_visually_hidden;
          test "scroll indicator handles tiny layouts"
            scroll_indicator_handles_tiny_layouts;
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
      group "Rendering"
        [
          test "rich span styles apply" rich_cell_span_styles_apply;
          test "auto widths follow the screen width method"
            auto_widths_follow_screen_width_method;
          test "auto widths recompute after set_rows"
            auto_widths_recompute_after_set_rows;
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
          test "set_cell_padding pads each side" set_cell_padding_pads_each_side;
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
          test "preserves uncontrolled selection"
            apply_props_restores_declared_selection;
          test "controlled selection is silent"
            apply_props_controlled_selection_is_silent;
        ];
      group "Both doors"
        [
          test "columns" doors_columns_label;
          test "rows" doors_rows;
          test "selected_row" doors_selected_row;
          test "border" doors_border;
          test "border_style" doors_border_style;
          test "show_header" doors_show_header;
          test "show_column_separator" doors_show_column_separator;
          test "show_row_separator" doors_show_row_separator;
          test "cell_padding" doors_cell_padding;
          test "header_color" doors_header_color;
          test "header_background" doors_header_background;
          test "text_color" doors_text_color;
          test "background" doors_background;
          test "selected_text_color" doors_selected_text_color;
          test "selected_background" doors_selected_background;
          test "focused_selected_text_color" doors_focused_selected_text_color;
          test "focused_selected_background" doors_focused_selected_background;
          test "selection_visible" doors_selection_visible;
          test "show_scroll_indicator" doors_show_scroll_indicator;
          test "activate_on_click" doors_activate_on_click;
          test "wheel_navigation" doors_wheel_navigation;
          test "row_styles" doors_row_styles;
          test "wrap_selection" doors_wrap_selection;
          test "key_bindings" doors_key_bindings;
          test "fast_scroll_step" doors_fast_scroll_step;
        ];
    ]
