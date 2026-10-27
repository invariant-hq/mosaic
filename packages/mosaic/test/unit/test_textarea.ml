open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let dispatch_mouse t ev =
  ignore (Test_harness.dispatch_mouse t ev : Event.mouse)

let editor ?(value = "") ?max_length ?cursor ?selection () =
  let state =
    if value = "" && max_length = None then Editor.empty
    else Editor.of_string ?max_length value
  in
  let state =
    match cursor with None -> state | Some pos -> Editor.set_cursor pos state
  in
  match selection with
  | None | Some None -> state
  | Some (Some (anchor, cursor)) -> Editor.select ~anchor ~cursor state

let make_props ?state ?value ?max_length ?cursor ?selection ?wrap ?annotations
    ?ghost_text ?ghost_text_color ?placeholder ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor () =
  let state =
    match state with
    | Some state -> state
    | None -> editor ?value ?max_length ?cursor ?selection ()
  in
  Textarea.Props.make ~state ?wrap ?annotations ?ghost_text ?ghost_text_color
    ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor ()

let property_builders = Hashtbl.create 32

let update_editor widget f =
  let props =
    Hashtbl.find property_builders (Renderable.id (Textarea.node widget))
  in
  Textarea.apply_props widget (props (f (Textarea.state widget)))

let create_editor ~parent ?style ?value ?max_length ?cursor ?selection ?wrap
    ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?(accept = true) ?on_change ?on_submit () =
  let state = editor ?value ?max_length ?cursor ?selection () in
  let props state =
    Textarea.Props.make ~state ?wrap ?placeholder ?text_color ?background_color
      ?focused_text_color ?focused_background_color ?placeholder_color
      ?selection_color ?selection_fg ?cursor_style ?cursor_color
      ?cursor_blinking ?selectable ?show_cursor ()
  in
  let install = ref (fun state -> ignore state) in
  let on_change state =
    if accept then !install state;
    Option.iter (fun f -> f state) on_change
  in
  let widget =
    Textarea.create ~parent ?style ~state ~on_change ?on_submit ?wrap
      ?placeholder ?text_color ?background_color ?focused_text_color
      ?focused_background_color ?placeholder_color ?selection_color
      ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ?selectable
      ?show_cursor ()
  in
  (install := fun state -> Textarea.apply_props widget (props state));
  Hashtbl.replace property_builders (Renderable.id (Textarea.node widget)) props;
  widget

let make_textarea ?value ?max_length ?cursor ?selection ?wrap ?placeholder
    ?text_color ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?accept ?on_change
    ?on_submit () =
  let t = make_ctx () in
  let root = make_root t in
  let widget =
    create_editor ~parent:root ?value ?max_length ?cursor ?selection ?wrap
      ?placeholder ?text_color ?background_color ?focused_text_color
      ?focused_background_color ?placeholder_color ?selection_color
      ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ?selectable
      ?show_cursor ?accept ?on_change ?on_submit ()
  in
  (t, widget)

let render_textarea ta ~width ~height =
  let node = Textarea.node ta in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  grid

let no_mod = Matrix_input.Keystroke.no_mods

let send_key ?keypad ta key =
  let ev = Event.Key.of_input (Matrix_input.Keystroke.make ?keypad key) in
  Renderable.Private.run_key_behavior (Textarea.node ta) ev

let send_key_with_mod ta ~modifier key =
  let ev =
    Event.Key.of_input (Matrix_input.Keystroke.make ~mods:modifier key)
  in
  Renderable.Private.run_key_behavior (Textarea.node ta) ev

let send_input_event ta data =
  let ev = Event.Key.of_input data in
  Renderable.Private.run_key_behavior (Textarea.node ta) ev

let send_char ta c =
  let text = String.make 1 c in
  let ev =
    Event.Key.of_input
      (Matrix_input.Keystroke.make ~text (Char (Uchar.of_char c)))
  in
  Renderable.Private.run_key_behavior (Textarea.node ta) ev

let send_char_with_mod ta ~modifier c =
  let ev =
    Event.Key.of_input
      (Matrix_input.Keystroke.make ~mods:modifier (Char (Uchar.of_char c)))
  in
  Renderable.Private.run_key_behavior (Textarea.node ta) ev

let focus_textarea t ta =
  let node = Textarea.node ta in
  Renderable.set_focusable node true;
  let _focused = t.ctx.focus node in
  ()

let blur_textarea ta = Renderable.Private.blur_direct (Textarea.node ta)

let sized_style ~width ~height =
  Toffee.Style.default
  |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int width))
  |> Toffee.Style.set_height
       (Toffee.Style.Dimension.length (Float.of_int height))

let render_frame r ~width ~height =
  Renderer.render_frame r ~width ~height ~dt:0.;
  ignore (Renderer.render ~full:true r : string)

let mouse kind : Matrix_input.Mouse_report.t =
  { kind; x = 0; y = 0; shift = false; alt = false; ctrl = false }

let mouse_down ~x ~y = { (mouse (Press Left)) with x; y }
let mouse_drag ~x ~y = { (mouse (Motion (Some Left))) with x; y }
let mouse_up ~x ~y = { (mouse (Release (Some Left))) with x; y }
let ctrl_mod = { no_mod with Matrix_input.Keystroke.ctrl = true }
let shift_mod = { no_mod with Matrix_input.Keystroke.shift = true }
let alt_mod = { no_mod with Matrix_input.Keystroke.alt = true }
let super_mod = { no_mod with Matrix_input.Keystroke.super = true }

(* ── Props ── *)

let props_default_equals_make () =
  let p = Textarea.Props.default in
  is_true ~msg:"default equals make()" (Textarea.Props.equal p (make_props ()))

let props_equal_identical () =
  let a = make_props () in
  let b = make_props () in
  is_true ~msg:"equal" (Textarea.Props.equal a b)

let props_detects_value_diff () =
  let a = make_props ~value:"hello" () in
  let b = make_props ~value:"world" () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_placeholder_diff () =
  let a = make_props ~placeholder:"Enter text" () in
  let b = make_props ~placeholder:"Type here" () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_wrap_diff () =
  let a = make_props ~wrap:`Word () in
  let b = make_props ~wrap:`Char () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_text_color_diff () =
  let a = make_props ~text_color:Ansi.Color.red () in
  let b = make_props ~text_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_cursor_style_diff () =
  let a = make_props ~cursor_style:`Block () in
  let b = make_props ~cursor_style:`Line () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_cursor_blinking_diff () =
  let a = make_props ~cursor_blinking:true () in
  let b = make_props ~cursor_blinking:false () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_annotations_diff () =
  let style = Ansi.Style.make ~fg:Ansi.Color.cyan () in
  let state = Editor.of_string "let" in
  let annotations =
    Text_buffer.Annotations.make ~content:(Editor.text state)
      [ { start_byte = 0; end_byte = 3; style } ]
  in
  let a = make_props ~state ~annotations () in
  let b = make_props ~state () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_selection_diff () =
  let a = make_props ~value:"abc" ~selection:(Some (0, 2)) () in
  let b = make_props ~selection:None () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

(* ── Construction ── *)

let create_attaches_to_parent () =
  let _t, ta = make_textarea () in
  let node = Textarea.node ta in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_initial_value () =
  let _t, ta = make_textarea ~value:"hello\nworld" () in
  equal ~msg:"value" string "hello\nworld" (Textarea.value ta)

let create_default_value_is_empty () =
  let _t, ta = make_textarea () in
  equal ~msg:"default empty" string "" (Textarea.value ta)

let create_buffer_accessible () =
  let _t, ta = make_textarea ~value:"test" () in
  equal ~msg:"buffer text" string "test" (Editor.text (Textarea.state ta))

let create_surface_accessible () =
  let _t, ta = make_textarea () in
  let _surface = Textarea.surface ta in
  is_true ~msg:"surface accessible" true

let create_registers_text_rows_provider () =
  let _t, ta = make_textarea ~value:"one\ntwo\nthree" () in
  match Renderable.text_rows (Textarea.node ta) with
  | None -> fail "expected text rows provider"
  | Some rows ->
      let layout = rows.Renderable.layout in
      equal ~msg:"line count" int 3
        (Matrix_text.Layout.Source.line_count
           (Matrix_text.Layout.source layout));
      ge ~msg:"display lines" ~than:3 (Matrix_text.Layout.row_count layout)

let focus_repaints_without_changing_geometry () =
  let ctx, ta =
    make_textarea ~value:"hello" ~text_color:Ansi.Color.red
      ~focused_text_color:Ansi.Color.blue ~show_cursor:false ()
  in
  let surface = Textarea.surface ta in
  let before = render_textarea ta ~width:10 ~height:2 in
  let geometry = Text_surface.layout surface in
  is_true ~msg:"unfocused text is red"
    (Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg before 0));
  Renderable.Private.clear_layout_dirty (Textarea.node ta);
  focus_textarea ctx ta;
  is_false ~msg:"focus does not dirty layout"
    (Renderable.Private.layout_dirty (Textarea.node ta));
  let focused = render_textarea ta ~width:10 ~height:2 in
  is_true ~msg:"focused text is blue"
    (Ansi.Color.equal Ansi.Color.blue (Matrix_grid.get_fg focused 0));
  is_true ~msg:"focus retains geometry" (geometry == Text_surface.layout surface);
  blur_textarea ta;
  let blurred = render_textarea ta ~width:10 ~height:2 in
  is_true ~msg:"blur restores red"
    (Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg blurred 0));
  is_true ~msg:"blur retains geometry" (geometry == Text_surface.layout surface)

let focus_preserves_explicit_styles_and_restyles_fallback () =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let state = Editor.of_string "hello" in
  let green = Ansi.Style.make ~fg:Ansi.Color.green () in
  let annotations =
    Text_buffer.Annotations.make ~content:(Editor.text state)
      [ { start_byte = 0; end_byte = 5; style = green } ]
  in
  let ta =
    Textarea.create ~parent:root ~state ~annotations ~on_change:ignore
      ~text_color:Ansi.Color.red ~focused_text_color:Ansi.Color.blue
      ~show_cursor:false ()
  in
  ignore (render_textarea ta ~width:10 ~height:2);
  let surface = Textarea.surface ta in
  let geometry = Text_surface.layout surface in
  focus_textarea ctx ta;
  let focused = render_textarea ta ~width:10 ~height:2 in
  is_true ~msg:"explicit green survives focus"
    (Ansi.Color.equal Ansi.Color.green (Matrix_grid.get_fg focused 0));
  is_true ~msg:"explicit style focus retains geometry"
    (geometry == Text_surface.layout surface);
  blur_textarea ta;
  let stale =
    Text_buffer.Annotations.make ~content:"stale"
      [ { start_byte = 0; end_byte = 5; style = green } ]
  in
  raises_match ~msg:"stale annotations rejected before declaration changes"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      Textarea.apply_props ta (make_props ~state ~annotations:stale ()));
  equal string "hello" (Textarea.value ta);
  Textarea.apply_props ta
    (make_props ~state ~text_color:Ansi.Color.red
       ~focused_text_color:Ansi.Color.blue ~show_cursor:false ());
  focus_textarea ctx ta;
  let fallback = render_textarea ta ~width:10 ~height:2 in
  is_true ~msg:"removing annotations restores focused plain style"
    (Ansi.Color.equal Ansi.Color.blue (Matrix_grid.get_fg fallback 0));
  is_true ~msg:"paint changes retain source geometry"
    (geometry == Text_surface.layout surface);
  Textarea.apply_props ta
    (make_props
       ~state:(Editor.of_string "replacement")
       ~focused_text_color:Ansi.Color.red ~show_cursor:false ());
  let changed = render_textarea ta ~width:15 ~height:2 in
  equal ~msg:"simultaneous source and colour update" string "replacement"
    (Text_buffer.plain_text (Text_surface.buffer surface));
  is_true ~msg:"new content gets updated focus colour"
    (Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg changed 0))

(* ── Value ── *)

let value_returns_current_text () =
  let _t, ta = make_textarea ~value:"line1\nline2" () in
  equal ~msg:"current text" string "line1\nline2" (Textarea.value ta)

let set_value_replaces_text () =
  let _t, ta = make_textarea ~value:"old" () in
  update_editor ta (fun _ -> Editor.of_string "new\ncontent");
  equal ~msg:"replaced" string "new\ncontent" (Textarea.value ta)

let set_value_requests_render () =
  let t, ta = make_textarea () in
  let before = !(t.schedule_count) in
  update_editor ta (fun _ -> Editor.of_string "something");
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let changes_propose_complete_state () =
  let proposals = ref [] in
  let _, ta =
    make_textarea ~value:"ab"
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  send_key ta Matrix_input.Keystroke.Left;
  send_char ta 'x';
  match !proposals with
  | [ edited; moved ] ->
      equal string "ab" (Editor.text moved);
      equal int 1 (Editor.cursor moved);
      equal string "axb" (Editor.text edited);
      equal int 2 (Editor.cursor edited)
  | _ -> fail "expected one complete proposal per state change"

let rejected_edits_leave_state_and_history_intact () =
  let proposals = ref [] in
  let _, ta =
    make_textarea ~value:"ab" ~accept:false
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  let accepted = Textarea.state ta in
  send_char ta 'x';
  send_char ta 'y';
  is_true (Textarea.state ta == accepted);
  equal string "ab" (Textarea.value ta);
  match !proposals with
  | [ latest; earlier ] ->
      equal string "aby" (Editor.text latest);
      equal string "abx" (Editor.text earlier);
      equal string "ab" (Editor.text (Editor.undo latest))
  | _ -> fail "expected independent proposals from accepted state"

let rejected_cursor_does_not_affect_next_edit () =
  let proposals = ref [] in
  let _, ta =
    make_textarea ~value:"ab" ~accept:false
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  send_key ta Matrix_input.Keystroke.Left;
  send_char ta 'x';
  equal int 2 (Textarea.cursor ta);
  equal string "abx" (Editor.text (List.hd !proposals))

let rejected_movement_preserves_preferred_column () =
  let proposal = ref Editor.empty in
  let _, ta =
    make_textarea ~value:"abcde\nx\nabcde" ~cursor:4 ~accept:false
      ~on_change:(fun next -> proposal := next)
      ()
  in
  ignore (render_textarea ta ~width:20 ~height:3 : Matrix_grid.t);
  send_key ta Matrix_input.Keystroke.Down;
  equal ~msg:"short row clamps the caret" int 7 (Editor.cursor !proposal);
  update_editor ta (fun _ -> !proposal);
  let accepted = Textarea.state ta in
  send_key ta Matrix_input.Keystroke.Down;
  equal ~msg:"vertical proposal restores desired column" int 12
    (Editor.cursor !proposal);
  is_true ~msg:"rejected vertical proposal leaves state intact"
    (Textarea.state ta == accepted);
  send_key ta Matrix_input.Keystroke.Left;
  equal ~msg:"horizontal proposal starts from accepted short row" int 6
    (Editor.cursor !proposal);
  send_key ta Matrix_input.Keystroke.Up;
  equal ~msg:"rejected movement preserves desired column" int 4
    (Editor.cursor !proposal);
  is_true ~msg:"proposal still derives from accepted state"
    (Editor.source !proposal == Editor.source accepted);
  equal ~msg:"original accepted value stays intact" int 7
    (Editor.cursor accepted)

(* A proposal is a complete value: an application that defers one and stores
   it later gets exactly that value, even over a newer one it stored since. *)
let deferred_proposal_is_a_complete_value () =
  let proposal = ref Editor.empty in
  let _, ta =
    make_textarea ~value:"ab" ~accept:false
      ~on_change:(fun next -> proposal := next)
      ()
  in
  send_char ta 'x';
  equal string "abx" (Editor.text !proposal);
  update_editor ta (Editor.insert "y");
  equal string "aby" (Textarea.value ta);
  update_editor ta (fun _ -> !proposal);
  equal ~msg:"deferred proposal stored" string "abx" (Textarea.value ta);
  equal ~msg:"with its caret" int 3 (Textarea.cursor ta)

let transformed_acceptance_is_next_event_base () =
  let widget = ref None in
  let on_change proposal =
    match !widget with
    | None -> fail "callback during construction"
    | Some widget ->
        update_editor widget (fun _ ->
            Editor.of_string (String.uppercase_ascii (Editor.text proposal)))
  in
  let _, ta = make_textarea ~accept:false ~on_change () in
  widget := Some ta;
  send_char ta 'a';
  send_char ta 'b';
  equal string "AB" (Textarea.value ta);
  equal int 2 (Textarea.cursor ta)

let blur_and_submit_do_not_propose_edits () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_change:(fun _ -> incr count) () in
  focus_textarea t ta;
  blur_textarea ta;
  equal int 0 !count

(* ── Callbacks — on_submit ── *)

let on_submit_fires_on_meta_enter () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:ctrl_mod Matrix_input.Keystroke.Enter;
  equal ~msg:"fired" int 1 !count

let on_submit_receives_current_value () =
  let fired = ref [] in
  let t, ta =
    make_textarea ~value:"hello" ~on_submit:(fun s -> fired := s :: !fired) ()
  in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:ctrl_mod Matrix_input.Keystroke.Enter;
  match !fired with
  | v :: _ -> equal ~msg:"received value" string "hello" v
  | [] -> fail "on_submit not fired"

let set_on_submit_none_disables () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  Textarea.set_on_submit ta None;
  send_key_with_mod ta ~modifier:ctrl_mod Matrix_input.Keystroke.Enter;
  equal ~msg:"disabled" int 0 !count

let enter_does_not_fire_submit () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  send_key ta Matrix_input.Keystroke.Enter;
  equal ~msg:"submit not fired" int 0 !count

(* ── Key Handling ── *)

let key_char_inserts_text () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  send_char ta 'b';
  send_char ta 'c';
  equal ~msg:"inserted" string "abc" (Textarea.value ta)

let key_enter_inserts_newline () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  send_key ta Matrix_input.Keystroke.Enter;
  send_char ta 'b';
  equal ~msg:"newline" string "a\nb" (Textarea.value ta)

let key_kp_enter_inserts_newline () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  send_key ~keypad:true ta Matrix_input.Keystroke.Enter;
  send_char ta 'b';
  equal ~msg:"newline" string "a\nb" (Textarea.value ta)

(* A line feed, 0x0A, is Ctrl+J: Enter under a terminal that translates
   carriage returns. *)
let ctrl_j_inserts_newline () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  send_char_with_mod ta ~modifier:ctrl_mod 'j';
  send_char ta 'b';
  equal ~msg:"newline" string "a\nb" (Textarea.value ta)

let key_left_right_moves_cursor () =
  let t, ta = make_textarea ~value:"ab" () in
  focus_textarea t ta;
  equal ~msg:"cursor at end" int 2 (Editor.cursor (Textarea.state ta));
  send_key ta Matrix_input.Keystroke.Left;
  equal ~msg:"cursor moved left" int 1 (Editor.cursor (Textarea.state ta));
  send_key ta Matrix_input.Keystroke.Right;
  equal ~msg:"cursor moved right" int 2 (Editor.cursor (Textarea.state ta))

let key_up_moves_to_previous_line () =
  let t, ta = make_textarea ~value:"one\ntwo" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:2 : Matrix_grid.t);
  send_key ta Matrix_input.Keystroke.Up;
  equal ~msg:"cursor" int 3 (Textarea.cursor ta)

let key_down_moves_to_next_line () =
  let t, ta = make_textarea ~value:"one\ntwo" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:2 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 0);
  send_key ta Matrix_input.Keystroke.Down;
  equal ~msg:"cursor" int 4 (Textarea.cursor ta)

let key_backspace_deletes_backward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  send_key ta Matrix_input.Keystroke.Backspace;
  equal ~msg:"deleted" string "ab" (Textarea.value ta)

let key_backspace_at_line_start_joins () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 4);
  send_key ta Matrix_input.Keystroke.Backspace;
  equal ~msg:"joined" string "abcdef" (Textarea.value ta)

let key_delete_deletes_forward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_key ta Matrix_input.Keystroke.Delete;
  equal ~msg:"deleted forward" string "bc" (Textarea.value ta)

(* ── Emacs Keybindings ── *)

let ctrl_a_moves_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 6);
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at line start" int 4 (Editor.cursor (Textarea.state ta))

let ctrl_shift_a_selects_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 6);
  (* Ctrl+Shift+a, its Shift consumed into the key: it selects. *)
  send_char_with_mod ta ~modifier:ctrl_mod 'A';
  equal ~msg:"cursor at line start" int 4 (Editor.cursor (Textarea.state ta));
  is_true ~msg:"selection"
    (Option.is_some (Editor.selection (Textarea.state ta)))

let ctrl_a_base_key_moves_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 6);
  let data =
    Matrix_input.Keystroke.make ~mods:ctrl_mod ~base:(Uchar.of_char 'a')
      (Matrix_input.Keystroke.Char (Uchar.of_int 0x314a))
  in
  send_input_event ta data;
  equal ~msg:"cursor at line start" int 4 (Editor.cursor (Textarea.state ta))

let ctrl_e_moves_to_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 4);
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at line end" int 7 (Editor.cursor (Textarea.state ta))

let logical_boundary_collapses_selection_without_crossing_line () =
  let _, ta = make_textarea ~value:"one\ntwo\nthree" () in
  update_editor ta (Editor.select ~anchor:1 ~cursor:4);
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"Home stays at selected line start" int 4 (Textarea.cursor ta);
  is_none (Editor.selection (Textarea.state ta));
  update_editor ta (Editor.select ~anchor:12 ~cursor:7);
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"End stays at selected line end" int 7 (Textarea.cursor ta);
  is_none (Editor.selection (Textarea.state ta))

let ctrl_b_moves_left () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  send_char_with_mod ta ~modifier:ctrl_mod 'b';
  equal ~msg:"cursor moved left" int 2 (Editor.cursor (Textarea.state ta))

let ctrl_f_moves_right () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_char_with_mod ta ~modifier:ctrl_mod 'f';
  equal ~msg:"cursor moved right" int 1 (Editor.cursor (Textarea.state ta))

let ctrl_d_deletes_forward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_char_with_mod ta ~modifier:ctrl_mod 'd';
  equal ~msg:"deleted forward" string "bc" (Textarea.value ta)

let ctrl_k_deletes_to_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 4);
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  equal ~msg:"deleted to line end" string "abc\n" (Textarea.value ta)

let ctrl_u_deletes_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 6);
  send_char_with_mod ta ~modifier:ctrl_mod 'u';
  equal ~msg:"deleted to line start" string "abc\nf" (Textarea.value ta)

let ctrl_w_deletes_word_backward_after_newline () =
  let t, ta = make_textarea ~value:"hello\nworld" () in
  focus_textarea t ta;
  send_char_with_mod ta ~modifier:ctrl_mod 'w';
  equal ~msg:"deleted word" string "hello\n" (Textarea.value ta);
  equal ~msg:"cursor" int 6 (Textarea.cursor ta)

let ctrl_shift_d_deletes_line () =
  let t, ta = make_textarea ~value:"abc\ndef\nghi" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 5);
  send_char_with_mod ta ~modifier:ctrl_mod 'D';
  equal ~msg:"line deleted" string "abc\nghi" (Textarea.value ta)

(* ── Alt Keybindings ── *)

let alt_b_moves_word_backward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  send_char_with_mod ta ~modifier:alt_mod 'b';
  equal ~msg:"moved to word boundary" int 6 (Editor.cursor (Textarea.state ta))

let meta_only_does_not_match_alt_binding () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  let meta_mod = { no_mod with Matrix_input.Keystroke.meta = true } in
  send_char_with_mod ta ~modifier:meta_mod 'b';
  equal ~msg:"Meta alone does not match Alt" int 11 (Textarea.cursor ta)

let alt_f_moves_word_forward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_char_with_mod ta ~modifier:alt_mod 'f';
  equal ~msg:"moved to word end" int 6 (Editor.cursor (Textarea.state ta))

let alt_d_deletes_word_forward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_char_with_mod ta ~modifier:alt_mod 'd';
  equal ~msg:"word deleted forward" string "world" (Textarea.value ta)

(* ── Super+Arrow Keybindings ── *)

let super_left_moves_to_visual_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:40 ~height:10 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 6);
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Left;
  equal ~msg:"cursor at visual line start" int 4
    (Editor.cursor (Textarea.state ta))

let super_right_moves_to_visual_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:40 ~height:10 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 4);
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Right;
  equal ~msg:"cursor at visual line end" int 7
    (Editor.cursor (Textarea.state ta))

let super_up_moves_to_buffer_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Up;
  equal ~msg:"cursor at start" int 0 (Editor.cursor (Textarea.state ta))

let super_down_moves_to_buffer_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Down;
  equal ~msg:"cursor at end" int 7 (Editor.cursor (Textarea.state ta))

(* ── Selection Keybindings ── *)

let shift_right_creates_selection () =
  let t, ta = make_textarea ~value:"hello" () in
  focus_textarea t ta;
  update_editor ta Editor.move_home;
  send_key_with_mod ta ~modifier:shift_mod Matrix_input.Keystroke.Right;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Textarea.state ta)))

let super_a_selects_all () =
  let t, ta = make_textarea ~value:"hello\nworld" () in
  focus_textarea t ta;
  send_char_with_mod ta ~modifier:super_mod 'a';
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Textarea.state ta)));
  equal ~msg:"all selected" string "hello\nworld"
    (Editor.selected_text (Textarea.state ta))

let type_with_selection_replaces () =
  let t, ta = make_textarea ~value:"hello" () in
  focus_textarea t ta;
  update_editor ta Editor.select_all;
  send_char ta 'x';
  equal ~msg:"replaced" string "x" (Textarea.value ta)

(* ── Undo / Redo ── *)

let ctrl_z_undoes () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  equal ~msg:"before undo" string "a" (Textarea.value ta);
  send_char_with_mod ta ~modifier:ctrl_mod 'z';
  equal ~msg:"after undo" string "" (Textarea.value ta)

let ctrl_shift_z_redoes () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_char ta 'a';
  send_char_with_mod ta ~modifier:ctrl_mod 'z';
  equal ~msg:"after undo" string "" (Textarea.value ta);
  send_char_with_mod ta ~modifier:ctrl_mod 'Z';
  equal ~msg:"after redo" string "a" (Textarea.value ta)

(* ── Visual Line Navigation ── *)

let alt_a_moves_to_visual_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:40 ~height:10 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 6);
  send_char_with_mod ta ~modifier:alt_mod 'a';
  equal ~msg:"cursor at visual line start" int 4
    (Editor.cursor (Textarea.state ta))

let alt_e_moves_to_visual_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:40 ~height:10 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 4);
  send_char_with_mod ta ~modifier:alt_mod 'e';
  equal ~msg:"cursor at visual line end" int 7
    (Editor.cursor (Textarea.state ta))

(* ── Ctrl+A/E Wrap-Around ── *)

let ctrl_a_wraps_to_previous_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 4);
  (* cursor at start of "def", Ctrl+A should wrap to end of "abc" *)
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at end of previous line" int 3
    (Editor.cursor (Textarea.state ta))

let ctrl_a_does_not_wrap_on_first_line () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 0);
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor stays at 0" int 0 (Editor.cursor (Textarea.state ta))

let ctrl_e_wraps_to_next_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 3);
  (* cursor at end of "abc", Ctrl+E should wrap to start of "def" *)
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at start of next line" int 4
    (Editor.cursor (Textarea.state ta))

let ctrl_e_does_not_wrap_on_last_line () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 7);
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor stays at end" int 7 (Editor.cursor (Textarea.state ta))

(* ── Alt+Enter Submit ── *)

let alt_enter_fires_submit () =
  let received = ref "" in
  let on_submit v = received := v in
  let t, ta = make_textarea ~value:"hello" ~on_submit () in
  focus_textarea t ta;
  let alt_mod = { no_mod with Matrix_input.Keystroke.alt = true } in
  send_key_with_mod ta ~modifier:alt_mod Matrix_input.Keystroke.Enter;
  equal ~msg:"submit fired" string "hello" !received

(* ── Ctrl+K at Line End ── *)

let ctrl_k_at_line_end_does_nothing () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 3);
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  equal ~msg:"text unchanged" string "abc\ndef" (Textarea.value ta);
  equal ~msg:"cursor unchanged" int 3 (Editor.cursor (Textarea.state ta))

(* ── Paste Handling ── *)

let handle_paste_inserts_text () =
  let _t, ta = make_textarea () in
  Textarea.handle_paste ta "pasted";
  equal ~msg:"pasted" string "pasted" (Textarea.value ta)

let handle_paste_preserves_newlines () =
  let _t, ta = make_textarea () in
  Textarea.handle_paste ta "line1\nline2\nline3";
  let v = Textarea.value ta in
  is_true ~msg:"has newlines" (String.contains v '\n');
  equal ~msg:"value" string "line1\nline2\nline3" v

let handle_paste_strips_ansi () =
  let _t, ta = make_textarea () in
  Textarea.handle_paste ta "\027[31mred\027[0m";
  equal ~msg:"stripped" string "red" (Textarea.value ta)

let handle_paste_with_selection_replaces () =
  let _t, ta = make_textarea ~value:"old" () in
  update_editor ta Editor.select_all;
  Textarea.handle_paste ta "new\ntext";
  equal ~msg:"replaced" string "new\ntext" (Textarea.value ta)

(* ── Mouse ── *)

let mouse_selection_syncs_to_buffer () =
  let r = Renderer.create () in
  let ta =
    create_editor ~parent:(Renderer.root r)
      ~style:(sized_style ~width:20 ~height:3)
      ~value:"hello world" ()
  in
  render_frame r ~width:20 ~height:3;
  dispatch_mouse r (mouse_down ~x:0 ~y:0);
  dispatch_mouse r (mouse_drag ~x:5 ~y:0);
  dispatch_mouse r (mouse_up ~x:5 ~y:0);
  equal ~msg:"selection"
    (option (pair int int))
    (Some (0, 5))
    (Textarea.selection ta);
  equal ~msg:"selected text" string "hello"
    (Editor.selected_text (Textarea.state ta))

let mouse_click_reports_cursor_without_selection () =
  let r = Renderer.create () in
  let fired = ref [] in
  let ta =
    create_editor ~parent:(Renderer.root r)
      ~style:(sized_style ~width:20 ~height:3)
      ~value:"hello world"
      ~on_change:(fun state ->
        fired := (Editor.cursor state, Editor.selection state) :: !fired)
      ()
  in
  render_frame r ~width:20 ~height:3;
  List.iter
    (fun x ->
      fired := [];
      dispatch_mouse r (mouse_down ~x ~y:0);
      dispatch_mouse r (mouse_up ~x ~y:0);
      equal int x (Textarea.cursor ta);
      is_true ~msg:"cursor notification without an active selection"
        (List.mem (x, None) !fired))
    [ 2; 4 ]

(* A press or a drag that starts in one editor leaves a sibling editor's
   caret and selection as they are, even when the drag crosses into it. *)
let press_leaves_sibling_editor_alone () =
  let r = Renderer.create () in
  let row =
    Renderable.create ~parent:(Renderer.root r)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row)
      ()
  in
  let _left =
    create_editor ~parent:row
      ~style:(sized_style ~width:10 ~height:3)
      ~value:"left text" ()
  in
  let proposals = ref 0 in
  let right =
    create_editor ~parent:row
      ~style:(sized_style ~width:10 ~height:3)
      ~value:"right text"
      ~on_change:(fun _ -> incr proposals)
      ()
  in
  render_frame r ~width:20 ~height:3;
  dispatch_mouse r (mouse_down ~x:0 ~y:0);
  dispatch_mouse r (mouse_drag ~x:15 ~y:0);
  dispatch_mouse r (mouse_up ~x:15 ~y:0);
  equal ~msg:"no proposal from the sibling" int 0 !proposals;
  equal ~msg:"the sibling's caret" int 10 (Textarea.cursor right)

let mouse_selection_disabled_when_not_selectable () =
  let r = Renderer.create () in
  let ta =
    create_editor ~parent:(Renderer.root r)
      ~style:(sized_style ~width:20 ~height:3)
      ~value:"hello world" ~selectable:false ()
  in
  render_frame r ~width:20 ~height:3;
  dispatch_mouse r (mouse_down ~x:0 ~y:0);
  dispatch_mouse r (mouse_drag ~x:5 ~y:0);
  dispatch_mouse r (mouse_up ~x:5 ~y:0);
  is_none ~msg:"selection disabled" (Textarea.selection ta)

let mouse_wheel_scrolls_surface () =
  let lines = "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten" in
  let _t, ta = make_textarea ~value:lines () in
  ignore (render_textarea ta ~width:20 ~height:3 : Matrix_grid.t);
  let ev =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier
      (Scroll { direction = Scroll_down; delta = 2 })
  in
  Renderable.Private.emit_mouse (Textarea.node ta) ev;
  is_true ~msg:"scroll default prevented" (Event.Mouse.default_prevented ev);
  equal ~msg:"scroll_y" int 2 (Text_surface.scroll_y (Textarea.surface ta))

(* A viewport that follows the caret resolves to the same offsets on every
   paint, so once the caret is shown the editor asks for no further frame. *)
let following_viewport_settles () =
  let r = Renderer.create () in
  let lines = "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten" in
  let ta =
    create_editor ~parent:(Renderer.root r)
      ~style:(sized_style ~width:20 ~height:3)
      ~value:lines ()
  in
  update_editor ta (Editor.with_viewport ~x:0 ~y:0 ~follow:true);
  render_frame r ~width:20 ~height:3;
  equal ~msg:"the caret's line is shown" int 7
    (Text_surface.scroll_y (Textarea.surface ta));
  render_frame r ~width:20 ~height:3;
  is_false ~msg:"no frame is owed" (Renderer.needs_render r)

let wheel_down ta delta =
  let ev =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier
      (Scroll { direction = Scroll_down; delta })
  in
  Renderable.Private.emit_mouse (Textarea.node ta) ev

(* The viewport belongs to the accepted value: a surface mounted again from it,
   as after a key change, shows the same lines. *)
let remounted_surface_keeps_wheel_viewport () =
  let lines = "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten" in
  let accepted = ref None in
  let _t, ta =
    make_textarea ~value:lines ~on_change:(fun s -> accepted := Some s) ()
  in
  ignore (render_textarea ta ~width:20 ~height:3 : Matrix_grid.t);
  wheel_down ta 2;
  let state =
    match !accepted with
    | Some state -> state
    | None -> fail "the wheel proposed no viewport"
  in
  let remounted =
    Textarea.create
      ~parent:(make_root (make_ctx ()))
      ~state ~on_change:ignore ()
  in
  ignore (render_textarea remounted ~width:20 ~height:3 : Matrix_grid.t);
  equal ~msg:"same first line" int 2
    (Text_surface.scroll_y (Textarea.surface remounted))

let rejected_wheel_proposal_does_not_scroll () =
  let lines = "one\ntwo\nthree\nfour\nfive\nsix" in
  let _t, ta = make_textarea ~value:lines ~accept:false () in
  ignore (render_textarea ta ~width:20 ~height:3 : Matrix_grid.t);
  wheel_down ta 2;
  ignore (render_textarea ta ~width:20 ~height:3 : Matrix_grid.t);
  equal ~msg:"viewport unchanged" int 0
    (Text_surface.scroll_y (Textarea.surface ta))

let resize_after_newline_clamps_scroll_y () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:1 : Matrix_grid.t);
  send_char ta 't';
  send_char ta 'e';
  send_char ta 's';
  send_char ta 't';
  send_key ta Matrix_input.Keystroke.Enter;
  equal ~msg:"scroll_y before growth" int 1
    (Text_surface.scroll_y (Textarea.surface ta));
  ignore (render_textarea ta ~width:20 ~height:2 : Matrix_grid.t);
  equal ~msg:"scroll_y after growth" int 0
    (Text_surface.scroll_y (Textarea.surface ta))

(* ── Cursor Provider ── *)

let cursor_returns_none_when_unfocused () =
  let _t, ta = make_textarea ~value:"hi" () in
  ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t);
  let node = Textarea.node ta in
  let cursor = Renderable.cursor node in
  is_none ~msg:"no cursor when unfocused" cursor

let cursor_returns_some_when_focused () =
  let t, ta = make_textarea ~value:"hi" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t);
  let node = Textarea.node ta in
  let cursor = Renderable.cursor node in
  is_some ~msg:"cursor when focused" cursor

let cursor_returns_none_when_hidden () =
  let t, ta = make_textarea ~value:"hi" ~show_cursor:false () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t);
  is_none ~msg:"cursor hidden" (Renderable.cursor (Textarea.node ta))

let cursor_style_is_block_by_default () =
  let t, ta = make_textarea ~value:"hi" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t);
  let node = Textarea.node ta in
  match Renderable.cursor node with
  | Some c -> is_true ~msg:"block style" (c.style = `Block)
  | None -> fail "expected cursor"

let cursor_has_correct_color () =
  let t, ta = make_textarea ~value:"hi" ~cursor_color:Ansi.Color.red () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t);
  let node = Textarea.node ta in
  match Renderable.cursor node with
  | Some c ->
      is_true ~msg:"cursor color is red"
        (Ansi.Color.equal c.color Ansi.Color.red)
  | None -> fail "expected cursor"

(* ── apply_props ── *)

let apply_props_value_change () =
  let _t, ta = make_textarea ~value:"old" () in
  let props = make_props ~value:"new" () in
  Textarea.apply_props ta props;
  equal ~msg:"value updated" string "new" (Textarea.value ta)

let apply_props_echoed_value_preserves_cursor () =
  let t, ta = make_textarea ~value:"aaa\nbbb\nccc" () in
  focus_textarea t ta;
  update_editor ta (Editor.set_cursor 5);
  send_char_with_mod ta ~modifier:ctrl_mod 'D';
  let cursor_before = Editor.cursor (Textarea.state ta) in
  lt ~msg:"cursor before the end of the buffer, pre apply_props"
    ~than:(Editor.length (Textarea.state ta))
    cursor_before;
  Textarea.apply_props ta (make_props ~state:(Textarea.state ta) ());
  equal ~msg:"cursor preserved after echoed value reconcile" int cursor_before
    (Editor.cursor (Textarea.state ta))

let apply_props_selection_change_updates_buffer () =
  let _t, ta = make_textarea ~value:"hello\nworld" () in
  Textarea.apply_props ta
    (make_props ~value:"hello\nworld" ~selection:(Some (1, 5)) ());
  equal ~msg:"selection updated"
    (option (pair int int))
    (Some (1, 5))
    (Textarea.selection ta)

let apply_props_selection_none_clears_buffer_selection () =
  let _t, ta =
    make_textarea ~value:"hello\nworld" ~selection:(Some (1, 5)) ()
  in
  Textarea.apply_props ta (make_props ~value:"hello\nworld" ~selection:None ());
  is_none ~msg:"selection cleared" (Textarea.selection ta)

let apply_props_selectable_false_clears_selection () =
  let _t, ta = make_textarea ~value:"hello world" ~selection:(Some (0, 5)) () in
  Textarea.apply_props ta (make_props ~value:"hello world" ~selectable:false ());
  is_none ~msg:"selection cleared" (Textarea.selection ta)

let apply_props_schedules_render () =
  let t, ta = make_textarea () in
  let before = !(t.schedule_count) in
  Textarea.apply_props ta Textarea.Props.default;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Rendering ── *)

let render_zero_size_no_crash () =
  let _t, ta = make_textarea ~value:"hello\nworld" () in
  let node = Textarea.node ta in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  let grid = make_grid ~width:10 ~height:10 () in
  Renderable.Private.render node grid ~dt:0.;
  is_true ~msg:"render completed" true

(* ── Edge Cases ── *)

let operations_on_empty_textarea_are_safe () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_key ta Matrix_input.Keystroke.Backspace;
  send_key ta Matrix_input.Keystroke.Delete;
  send_key ta Matrix_input.Keystroke.Left;
  send_key ta Matrix_input.Keystroke.Right;
  send_key ta Matrix_input.Keystroke.Up;
  send_key ta Matrix_input.Keystroke.Down;
  send_key ta Matrix_input.Keystroke.Home;
  send_key ta Matrix_input.Keystroke.End;
  send_char_with_mod ta ~modifier:ctrl_mod 'w';
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  send_char_with_mod ta ~modifier:ctrl_mod 'u';
  send_char_with_mod ta ~modifier:ctrl_mod 'z';
  equal ~msg:"still empty" string "" (Textarea.value ta)

(* ── Pretty-printing ── *)

let pp_produces_non_empty_output () =
  let _t, ta = make_textarea () in
  let s = Format.asprintf "%a" Textarea.pp ta in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

let pp_contains_textarea_prefix () =
  let _t, ta = make_textarea () in
  let s = Format.asprintf "%a" Textarea.pp ta in
  let has_prefix =
    let len = String.length s in
    let plen = String.length "Textarea" in
    if plen > len then false
    else
      let found = ref false in
      for i = 0 to len - plen do
        if (not !found) && String.sub s i plen = "Textarea" then found := true
      done;
      !found
  in
  is_true ~msg:"has Textarea prefix" has_prefix

(* ── Runner ── *)

let visual_end_keeps_previous_wrapped_row () =
  let t, ta = make_textarea ~value:"abcdef" ~wrap:`Char () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:3 ~height:2 : Matrix_grid.t);
  update_editor ta (Editor.set_cursor 1);
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Right;
  let caret = Editor.caret (Textarea.state ta) in
  equal int 3 caret.grapheme;
  is_true (caret.affinity = Matrix_text.Layout.Upstream);
  send_key_with_mod ta ~modifier:super_mod Matrix_input.Keystroke.Left;
  equal int 0 (Editor.cursor (Textarea.state ta))

(* ── Syntax ── *)

let keyword_style =
  Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", Ansi.Style.make ~fg:Ansi.Color.red ()) ]

let syntax_highlights_and_shows_pending_edits () =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let requests = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify:_ ->
        let outcome = ref None in
        requests := (request, outcome) :: !requests;
        Code.Highlighter.job
          ~poll:(fun () ->
            let result = !outcome in
            outcome := None;
            result)
          ~cancel:ignore)
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:keyword_style highlighter
  in
  let complete () =
    match !requests with
    | ((request : unit Code.Highlighter.request), outcome) :: _ ->
        let meta = { Syntax_highlight.default_meta with conceal = Some "" } in
        outcome :=
          Some
            (Ok
               ( Syntax_highlight.make ~content:request.content
                   [
                     Syntax_highlight.range ~meta ~start_byte:0
                       ~end_byte:(String.length request.content - 2)
                       ~scope:"keyword" ();
                   ],
                 () ))
    | [] -> fail "expected a request"
  in
  let state = Editor.of_string "letter x" in
  let ta =
    Textarea.create ~parent:root ~state ~syntax ~on_change:ignore
      ~text_color:Ansi.Color.white ~show_cursor:false ()
  in
  let red_at grid x =
    Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg grid x)
  in
  complete ();
  let grid = render_textarea ta ~width:12 ~height:1 in
  equal ~msg:"editable text never conceals" string "letter x"
    (Text_buffer.plain_text (Text_surface.buffer (Textarea.surface ta)));
  is_true ~msg:"result shown" (red_at grid 0 && red_at grid 5);
  let state = Editor.of_string "letXter x" in
  Textarea.apply_props ta
    (Textarea.Props.make ~state ~syntax ~show_cursor:false ());
  (match !requests with
  | (request, _) :: _ -> (
      match request.previous with
      | Some { edit = { start; removed; inserted }; _ } ->
          equal ~msg:"edit from the latest result" (triple int int int)
            (3, 0, 1) (start, removed, inserted)
      | None -> fail "expected the latest result")
  | [] -> fail "expected a request");
  is_some ~msg:"pending work reported"
    (Renderable.Private.pending_work (Textarea.node ta));
  let grid = render_textarea ta ~width:12 ~height:1 in
  is_true ~msg:"bytes before the edit keep their style" (red_at grid 2);
  is_false ~msg:"the inserted byte is plain" (red_at grid 3);
  is_true ~msg:"bytes after the edit keep their style" (red_at grid 6)

let syntax_mismatch_shows_plain () =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let syntax =
    Code.syntax ~style:keyword_style
      (Syntax_highlight.of_triples ~content:"other" [ (0, 3, "keyword") ])
  in
  let ta =
    Textarea.create ~parent:root ~state:(Editor.of_string "hello") ~syntax
      ~on_change:ignore ~text_color:Ansi.Color.white ~show_cursor:false ()
  in
  let grid = render_textarea ta ~width:10 ~height:1 in
  is_true ~msg:"captures of other bytes show plain"
    (Ansi.Color.equal Ansi.Color.white (Matrix_grid.get_fg grid 0))

(* ── Highlight ranges ──

   A document of 100 lines "let NN", seven bytes each with the line feed, and
   a highlighter that answers each request with the keywords of the whole
   text that meet its range, as the contract allows. *)

let lines = String.concat "\n" (List.init 100 (Printf.sprintf "let %02d"))

let ranged_highlighter () =
  let requests = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify:_ ->
        let outcome = ref None in
        requests := (request, outcome) :: !requests;
        Code.Highlighter.job
          ~poll:(fun () ->
            let result = !outcome in
            outcome := None;
            result)
          ~cancel:ignore)
  in
  (* [answer ~keywords] completes the latest request: with the keywords that
     meet its range, or with none, as after a change that turned them into
     plain words. *)
  let answer ?(keywords = true) () =
    match !requests with
    | ((request : unit Code.Highlighter.request), outcome) :: _ ->
        let content = request.content and first, last = request.range in
        let found = ref [] in
        for i = 0 to String.length content - 3 do
          if
            keywords
            && String.sub content i 3 = "let"
            && i < last
            && i + 3 > first
          then found := (i, i + 3, "keyword") :: !found
        done;
        outcome := Some (Ok (Syntax_highlight.of_triples ~content !found, ()))
    | [] -> fail "expected a request"
  in
  (requests, highlighter, answer)

let ranged_textarea highlighter state =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:keyword_style highlighter
  in
  let ta =
    Textarea.create ~parent:root ~state ~syntax ~on_change:ignore
      ~text_color:Ansi.Color.white ~show_cursor:false ()
  in
  let show state =
    Textarea.apply_props ta
      (Textarea.Props.make ~state ~syntax ~show_cursor:false ())
  in
  (ta, show)

let keyword_at ta line =
  let buffer = Text_surface.buffer (Textarea.surface ta) in
  let source = Text_buffer.source buffer in
  let g = Matrix_text.Layout.Source.line_start source line + 1 in
  Ansi.Style.equal
    (Ansi.Style.make ~fg:Ansi.Color.red ())
    (fst (Text_buffer.style_at buffer g))

let requests_ask_for_the_rows_shown () =
  let requests, highlighter, answer = ranged_highlighter () in
  let ta, show = ranged_textarea highlighter (Editor.of_string lines) in
  let render () =
    ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t)
  in
  render ();
  let range () = (fst (List.hd !requests)).Code.Highlighter.range in
  equal ~msg:"the first request asks for the whole text" (pair int int)
    (0, String.length lines)
    (range ());
  answer ();
  render ();
  equal ~msg:"its result covers every row" int 1 (List.length !requests);
  is_true ~msg:"a row far below shows its result" (keyword_at ta 50);
  show (Editor.of_string lines |> Editor.set_cursor 0 |> Editor.insert "x");
  equal ~msg:"an edit asks again" int 2 (List.length !requests);
  (* Lines 0 to 9, with the byte inserted on line 0. *)
  equal ~msg:"for the lines shown and a screen below them" (pair int int)
    (0, (9 * 7) + 6 + 1)
    (range ());
  answer ~keywords:false ();
  render ();
  is_false ~msg:"the result restyles the rows it was asked for"
    (keyword_at ta 0);
  is_true ~msg:"the rows outside keep their captures" (keyword_at ta 50)

let scrolling_asks_for_rows_outside () =
  let requests, highlighter, answer = ranged_highlighter () in
  let state = Editor.of_string lines |> Editor.set_cursor 0 in
  let ta, show = ranged_textarea highlighter state in
  let render () =
    ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t)
  in
  render ();
  answer ();
  render ();
  let state = Editor.insert "x" state in
  show state;
  answer ();
  render ();
  equal ~msg:"two requests" int 2 (List.length !requests);
  show (Editor.with_viewport ~x:0 ~y:50 ~follow:false state);
  render ();
  equal ~msg:"rows outside the asked bytes are asked for" int 3
    (List.length !requests);
  (* Lines 45 to 59, after the byte inserted on line 0. *)
  equal ~msg:"for the lines shown and a screen above and below them"
    (pair int int)
    ((45 * 7) + 1, (59 * 7) + 6 + 1)
    (fst (List.hd !requests)).Code.Highlighter.range;
  is_some ~msg:"pending work reported"
    (Renderable.Private.pending_work (Textarea.node ta));
  is_true ~msg:"meanwhile they keep their carried captures" (keyword_at ta 50);
  answer ~keywords:false ();
  render ();
  is_false ~msg:"then show the result" (keyword_at ta 50);
  is_true ~msg:"rows outside its range keep theirs" (keyword_at ta 0);
  render ();
  equal ~msg:"rows asked for are not asked again" int 3 (List.length !requests)

let failed_range_is_not_asked_again () =
  let requests, highlighter, answer = ranged_highlighter () in
  let state = Editor.of_string lines |> Editor.set_cursor 0 in
  let ta, show = ranged_textarea highlighter state in
  let render () =
    ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t)
  in
  render ();
  answer ();
  render ();
  let state = Editor.insert "x" state in
  show state;
  (match !requests with
  | (_, outcome) :: _ -> outcome := Some (Error (Failure "toy"))
  | [] -> fail "expected a request");
  render ();
  render ();
  equal ~msg:"no request after the failure" int 2 (List.length !requests);
  is_false ~msg:"the content shows plain" (keyword_at ta 0)

let stateless_highlighter_is_asked_for_everything () =
  let calls = ref [] in
  let highlighter =
    Code.Highlighter.sync (fun ~language:_ ~content ->
        calls := String.length content :: !calls;
        Syntax_highlight.of_triples ~content [])
  in
  let state = Editor.of_string lines |> Editor.set_cursor 0 in
  let ta, show = ranged_textarea highlighter state in
  let render () =
    ignore (render_textarea ta ~width:20 ~height:5 : Matrix_grid.t)
  in
  render ();
  let state = Editor.insert "x" state in
  show state;
  render ();
  show (Editor.with_viewport ~x:0 ~y:50 ~follow:false state);
  render ();
  equal ~msg:"asked at creation and at the edit, never for scrolling" (list int)
    [ String.length lines + 1; String.length lines ]
    !calls

let annotations_go_over_syntax () =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let state = Editor.of_string "hello" in
  let syntax =
    Code.syntax ~style:keyword_style
      (Syntax_highlight.of_triples ~content:"hello" [ (0, 5, "keyword") ])
  in
  let annotations =
    Text_buffer.Annotations.make ~content:"hello"
      [
        {
          start_byte = 1;
          end_byte = 3;
          style = Ansi.Style.make ~bg:Ansi.Color.blue ();
        };
      ]
  in
  let ta =
    Textarea.create ~parent:root ~state ~syntax ~annotations ~on_change:ignore
      ~show_cursor:false ()
  in
  let grid = render_textarea ta ~width:10 ~height:1 in
  let red x = Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg grid x) in
  let blue x = Ansi.Color.equal Ansi.Color.blue (Matrix_grid.get_bg grid x) in
  is_true ~msg:"syntax alone" (red 0 && not (blue 0));
  is_true ~msg:"annotation over syntax" (red 1 && blue 1 && red 2 && blue 2);
  is_true ~msg:"syntax after the annotation" (red 3 && not (blue 3))

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.textarea}) or to
   {!Textarea.create}, reaches the live textarea ({!Test_harness.reaches}).
   The caret is observed as the node reports it, and proposals as
   [on_change] receives them. *)

let doors_size = Mosaic.size ~width:10 ~height:3
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let abc = Editor.of_string "abc"
let abc_selected = Editor.select ~anchor:0 ~cursor:2 abc
let let_x = Editor.of_string "let x"

let let_x_bold =
  Text_buffer.Annotations.make ~content:"let x"
    [ { start_byte = 0; end_byte = 3; style = Ansi.Style.make ~bold:true () } ]

let let_x_syntax =
  Code.syntax
    ~style:
      (Syntax_style.make ~base:Ansi.Style.default
         [ ("keyword", Ansi.Style.make ~fg:doors_red ()) ])
    (Syntax_highlight.of_triples ~content:"let x" [ (0, 3, "keyword") ])

let doors_proposals = ref []

let propose e =
  doors_proposals :=
    Printf.sprintf "%S@%d" (Editor.text e) (Editor.cursor e) :: !doors_proposals

let proposed _ _ = String.concat ";" (List.rev !doors_proposals)

let caret _ node =
  Format.asprintf "%a"
    (Format.pp_print_option Renderable.pp_cursor)
    (Renderable.cursor node)

let press_x renderer node =
  doors_proposals := [];
  Test_harness.focus renderer node;
  ignore (Test_harness.dispatch_key renderer (char_key 'x') : Event.key)

let drag renderer node =
  doors_proposals := [];
  Test_harness.focus renderer node;
  Test_harness.select_text renderer node

let doors_state () =
  Test_harness.reaches "state"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:(Editor.of_string "xyz")
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style
           ~state:(Editor.of_string "xyz") ~on_change:propose ()))

let doors_annotations () =
  Test_harness.reaches "annotations"
    ~base:(Mosaic.textarea ~size:doors_size ~state:let_x ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:let_x ~annotations:let_x_bold
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:let_x
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:let_x
           ~annotations:let_x_bold ~on_change:propose ()))

let doors_syntax () =
  Test_harness.reaches "syntax"
    ~base:(Mosaic.textarea ~size:doors_size ~state:let_x ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:let_x ~syntax:let_x_syntax
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:let_x
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:let_x
           ~syntax:let_x_syntax ~on_change:propose ()))

let doors_ghost_text () =
  Test_harness.reaches ~act:Test_harness.focus "ghost_text"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~ghost_text:"zz"
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc ~ghost_text:"zz"
           ~on_change:propose ()))

let doors_ghost_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "ghost_text_color"
    ~base:
      (Mosaic.textarea ~size:doors_size ~state:abc ~ghost_text:"zz"
         ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~ghost_text:"zz"
         ~ghost_text_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc ~ghost_text:"zz"
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc ~ghost_text:"zz"
           ~ghost_text_color:doors_red ~on_change:propose ()))

let doors_placeholder () =
  Test_harness.reaches "placeholder"
    ~base:
      (Mosaic.textarea ~size:doors_size ~state:Editor.empty ~on_change:propose
         ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:Editor.empty
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~on_change:propose ()))

let doors_wrap () =
  Test_harness.reaches "wrap"
    ~base:
      (Mosaic.textarea ~size:doors_size
         ~state:(Editor.of_string "hello world foo")
         ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size
         ~state:(Editor.of_string "hello world foo")
         ~wrap:`None ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style
           ~state:(Editor.of_string "hello world foo")
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style
           ~state:(Editor.of_string "hello world foo")
           ~wrap:`None ~on_change:propose ()))

let doors_text_color () =
  Test_harness.reaches "text_color"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~text_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~text_color:doors_red ~on_change:propose ()))

let doors_background_color () =
  Test_harness.reaches "background_color"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~background_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~background_color:doors_red ~on_change:propose ()))

let doors_focused_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_text_color"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~focused_text_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~focused_text_color:doors_red ~on_change:propose ()))

let doors_focused_background_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_background_color"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc
         ~focused_background_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~focused_background_color:doors_red ~on_change:propose ()))

let doors_placeholder_color () =
  Test_harness.reaches "placeholder_color"
    ~base:
      (Mosaic.textarea ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~placeholder_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~placeholder_color:doors_red ~on_change:propose
           ()))

let doors_selection_color () =
  Test_harness.reaches "selection_color"
    ~base:
      (Mosaic.textarea ~size:doors_size ~state:abc_selected ~on_change:propose
         ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc_selected
         ~selection_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc_selected
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc_selected
           ~selection_color:doors_red ~on_change:propose ()))

let doors_selection_fg () =
  Test_harness.reaches "selection_fg"
    ~base:
      (Mosaic.textarea ~size:doors_size ~state:abc_selected ~on_change:propose
         ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc_selected
         ~selection_fg:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc_selected
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc_selected
           ~selection_fg:doors_red ~on_change:propose ()))

let doors_cursor_style () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_style"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~cursor_style:`Line
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~cursor_style:`Line ~on_change:propose ()))

let doors_cursor_color () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_color"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~cursor_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~cursor_color:doors_red ~on_change:propose ()))

let doors_cursor_blinking () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_blinking"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~cursor_blinking:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~cursor_blinking:false ~on_change:propose ()))

let doors_selectable () =
  Test_harness.reaches ~act:drag ~observe:proposed "selectable"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~selectable:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc ~selectable:false
           ~on_change:propose ()))

let doors_show_cursor () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "show_cursor"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc ~show_cursor:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~show_cursor:false ~on_change:propose ()))

let doors_key_bindings () =
  Test_harness.reaches ~act:press_x ~observe:proposed "key_bindings"
    ~base:(Mosaic.textarea ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.textarea ~size:doors_size ~state:abc
         ~key_bindings:[ (Shortcut.char 'x', Textarea.Move_left) ]
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Textarea.node
        (Textarea.create ~parent ~style:doors_style ~state:abc
           ~key_bindings:[ (Shortcut.char 'x', Textarea.Move_left) ]
           ~on_change:propose ()))

let () =
  run "mosaic.textarea"
    [
      group "Caret affinity"
        [
          test "visual end stays on the preceding wrapped row"
            visual_end_keeps_previous_wrapped_row;
        ];
      group "Props"
        [
          test "default equals make()" props_default_equals_make;
          test "equal on identical" props_equal_identical;
          test "detects value diff" props_detects_value_diff;
          test "detects placeholder diff" props_detects_placeholder_diff;
          test "detects wrap diff" props_detects_wrap_diff;
          test "detects text_color diff" props_detects_text_color_diff;
          test "detects cursor_style diff" props_detects_cursor_style_diff;
          test "detects cursor_blinking diff" props_detects_cursor_blinking_diff;
          test "detects annotations diff" props_detects_annotations_diff;
          test "detects selection diff" props_detects_selection_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches_to_parent;
          test "initial value" create_initial_value;
          test "default value is empty" create_default_value_is_empty;
          test "buffer accessible" create_buffer_accessible;
          test "surface accessible" create_surface_accessible;
          test "registers line info provider"
            create_registers_text_rows_provider;
        ];
      group "Syntax"
        [
          test "highlights and shows pending edits"
            syntax_highlights_and_shows_pending_edits;
          test "a mismatch shows plain" syntax_mismatch_shows_plain;
          test "annotations go over syntax" annotations_go_over_syntax;
          test "requests ask for the rows shown" requests_ask_for_the_rows_shown;
          test "scrolling asks for rows outside" scrolling_asks_for_rows_outside;
          test "a failed range is not asked again"
            failed_range_is_not_asked_again;
          test "a stateless highlighter is asked for everything"
            stateless_highlighter_is_asked_for_everything;
        ];
      group "Paint"
        [
          test "focus repaints without changing geometry"
            focus_repaints_without_changing_geometry;
          test "focus preserves explicit styles and restyles fallback"
            focus_preserves_explicit_styles_and_restyles_fallback;
        ];
      group "Value"
        [
          test "returns current text" value_returns_current_text;
          test "set_value replaces text" set_value_replaces_text;
          test "set_value requests render" set_value_requests_render;
        ];
      group "State proposals"
        [
          test "complete state" changes_propose_complete_state;
          test "rejected edits preserve accepted state"
            rejected_edits_leave_state_and_history_intact;
          test "rejected caret does not affect next edit"
            rejected_cursor_does_not_affect_next_edit;
          test "rejected movement preserves preferred column"
            rejected_movement_preserves_preferred_column;
          test "a deferred proposal is a complete value"
            deferred_proposal_is_a_complete_value;
          test "transformed acceptance is next event base"
            transformed_acceptance_is_next_event_base;
          test "blur does not propose" blur_and_submit_do_not_propose_edits;
        ];
      group "Callbacks -- on_submit"
        [
          test "fires on Ctrl+Enter" on_submit_fires_on_meta_enter;
          test "receives current value" on_submit_receives_current_value;
          test "set_on_submit None disables" set_on_submit_none_disables;
          test "Enter does not fire submit" enter_does_not_fire_submit;
        ];
      group "Key handling"
        [
          test "character input inserts text" key_char_inserts_text;
          test "Enter inserts newline" key_enter_inserts_newline;
          test "keypad Enter inserts newline" key_kp_enter_inserts_newline;
          test "Ctrl+J inserts newline" ctrl_j_inserts_newline;
          test "Left/Right moves cursor" key_left_right_moves_cursor;
          test "Up moves to previous line" key_up_moves_to_previous_line;
          test "Down moves to next line" key_down_moves_to_next_line;
          test "Backspace deletes backward" key_backspace_deletes_backward;
          test "Backspace at line start joins" key_backspace_at_line_start_joins;
          test "Delete key deletes forward" key_delete_deletes_forward;
        ];
      group "Emacs keybindings"
        [
          test "Ctrl+A moves to line start" ctrl_a_moves_to_line_start;
          test "Ctrl+Shift+A selects to line start"
            ctrl_shift_a_selects_to_line_start;
          test "Ctrl+A base key moves to line start"
            ctrl_a_base_key_moves_to_line_start;
          test "Ctrl+E moves to line end" ctrl_e_moves_to_line_end;
          test "logical boundary collapses selection without crossing line"
            logical_boundary_collapses_selection_without_crossing_line;
          test "Ctrl+B moves left" ctrl_b_moves_left;
          test "Ctrl+F moves right" ctrl_f_moves_right;
          test "Ctrl+D deletes forward" ctrl_d_deletes_forward;
          test "Ctrl+K deletes to line end" ctrl_k_deletes_to_line_end;
          test "Ctrl+U deletes to line start" ctrl_u_deletes_to_line_start;
          test "Ctrl+W deletes word backward after newline"
            ctrl_w_deletes_word_backward_after_newline;
          test "Ctrl+Shift+D deletes line" ctrl_shift_d_deletes_line;
        ];
      group "Alt keybindings"
        [
          test "Alt+B moves word backward" alt_b_moves_word_backward;
          test "Meta alone does not match Alt"
            meta_only_does_not_match_alt_binding;
          test "Alt+F moves word forward" alt_f_moves_word_forward;
          test "Alt+D deletes word forward" alt_d_deletes_word_forward;
        ];
      group "Super+Arrow keybindings"
        [
          test "Super+Left moves to visual line start"
            super_left_moves_to_visual_line_start;
          test "Super+Right moves to visual line end"
            super_right_moves_to_visual_line_end;
          test "Super+Up moves to buffer start" super_up_moves_to_buffer_start;
          test "Super+Down moves to buffer end" super_down_moves_to_buffer_end;
        ];
      group "Selection keybindings"
        [
          test "Shift+Right creates selection" shift_right_creates_selection;
          test "Super+A selects all" super_a_selects_all;
          test "type with selection replaces" type_with_selection_replaces;
        ];
      group "Undo / Redo"
        [
          test "Ctrl+Z undoes" ctrl_z_undoes;
          test "Ctrl+Shift+Z redoes" ctrl_shift_z_redoes;
        ];
      group "Visual line navigation"
        [
          test "Alt+A moves to visual line start"
            alt_a_moves_to_visual_line_start;
          test "Alt+E moves to visual line end" alt_e_moves_to_visual_line_end;
        ];
      group "Ctrl+A/E wrap-around"
        [
          test "Ctrl+A wraps to previous line end"
            ctrl_a_wraps_to_previous_line_end;
          test "Ctrl+A does not wrap on first line"
            ctrl_a_does_not_wrap_on_first_line;
          test "Ctrl+E wraps to next line start" ctrl_e_wraps_to_next_line_start;
          test "Ctrl+E does not wrap on last line"
            ctrl_e_does_not_wrap_on_last_line;
        ];
      group "Alt+Enter submit"
        [ test "Alt+Enter fires submit" alt_enter_fires_submit ];
      group "Ctrl+K at line end"
        [
          test "Ctrl+K at line end does nothing" ctrl_k_at_line_end_does_nothing;
        ];
      group "Paste handling"
        [
          test "handle_paste inserts text" handle_paste_inserts_text;
          test "handle_paste preserves newlines" handle_paste_preserves_newlines;
          test "handle_paste strips ansi" handle_paste_strips_ansi;
          test "handle_paste with selection replaces"
            handle_paste_with_selection_replaces;
        ];
      group "Mouse"
        [
          test "selection syncs to buffer" mouse_selection_syncs_to_buffer;
          test "click reports cursor without selection"
            mouse_click_reports_cursor_without_selection;
          test "press leaves a sibling editor alone"
            press_leaves_sibling_editor_alone;
          test "selection disabled when not selectable"
            mouse_selection_disabled_when_not_selectable;
          test "wheel scrolls surface" mouse_wheel_scrolls_surface;
          test "remounted surface keeps the wheel viewport"
            remounted_surface_keeps_wheel_viewport;
          test "following viewport settles" following_viewport_settles;
          test "rejected wheel proposal does not scroll"
            rejected_wheel_proposal_does_not_scroll;
          test "resize after newline clamps scroll_y"
            resize_after_newline_clamps_scroll_y;
        ];
      group "Cursor provider"
        [
          test "returns None when unfocused" cursor_returns_none_when_unfocused;
          test "returns Some when focused" cursor_returns_some_when_focused;
          test "returns None when hidden" cursor_returns_none_when_hidden;
          test "style is Block by default" cursor_style_is_block_by_default;
          test "has correct color" cursor_has_correct_color;
        ];
      group "apply_props"
        [
          test "value change updates buffer" apply_props_value_change;
          test "echoed value preserves cursor"
            apply_props_echoed_value_preserves_cursor;
          test "selection change updates buffer"
            apply_props_selection_change_updates_buffer;
          test "selection None clears buffer selection"
            apply_props_selection_none_clears_buffer_selection;
          test "selectable false clears selection"
            apply_props_selectable_false_clears_selection;
          test "schedules render" apply_props_schedules_render;
        ];
      group "Rendering"
        [ test "zero-size does not crash" render_zero_size_no_crash ];
      group "Edge cases"
        [
          test "operations on empty textarea are safe"
            operations_on_empty_textarea_are_safe;
        ];
      group "Pretty-printing"
        [
          test "non-empty output" pp_produces_non_empty_output;
          test "contains Textarea prefix" pp_contains_textarea_prefix;
        ];
      group "Both doors"
        [
          test "state" doors_state;
          test "annotations" doors_annotations;
          test "syntax" doors_syntax;
          test "ghost_text" doors_ghost_text;
          test "ghost_text_color" doors_ghost_text_color;
          test "placeholder" doors_placeholder;
          test "wrap" doors_wrap;
          test "text_color" doors_text_color;
          test "background_color" doors_background_color;
          test "focused_text_color" doors_focused_text_color;
          test "focused_background_color" doors_focused_background_color;
          test "placeholder_color" doors_placeholder_color;
          test "selection_color" doors_selection_color;
          test "selection_fg" doors_selection_fg;
          test "cursor_style" doors_cursor_style;
          test "cursor_color" doors_cursor_color;
          test "cursor_blinking" doors_cursor_blinking;
          test "selectable" doors_selectable;
          test "show_cursor" doors_show_cursor;
          test "key_bindings" doors_key_bindings;
        ];
    ]
