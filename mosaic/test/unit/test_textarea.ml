open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_textarea ?value ?placeholder ?wrap ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ?cursor ?selection ?on_input ?on_change ?on_submit
    ?on_cursor () =
  let t = make_ctx () in
  let root = make_root t in
  let ta =
    Textarea.create ~parent:root ?value ?placeholder ?wrap ?text_color
      ?background_color ?focused_text_color ?focused_background_color
      ?placeholder_color ?selection_color ?selection_fg ?cursor_style
      ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?cursor ?selection
      ?on_input ?on_change ?on_submit ?on_cursor ()
  in
  (t, ta)

let render_textarea ta ~width ~height =
  let node = Textarea.node ta in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~delta:0.;
  grid

let no_mod = Input.Modifier.none

let send_key ta key =
  let ev = Event.Key.of_input (Input.Key.make key) in
  Renderable.Private.emit_default_key (Textarea.node ta) ev

let send_key_with_mod ta ~modifier key =
  let ev = Event.Key.of_input (Input.Key.make ~modifier key) in
  Renderable.Private.emit_default_key (Textarea.node ta) ev

let send_char ta c =
  let text = String.make 1 c in
  let ev = Event.Key.of_input (Input.Key.of_char ~associated_text:text c) in
  Renderable.Private.emit_default_key (Textarea.node ta) ev

let send_char_with_mod ta ~modifier c =
  let ev = Event.Key.of_input (Input.Key.of_char ~modifier c) in
  Renderable.Private.emit_default_key (Textarea.node ta) ev

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
  Renderer.render_frame r ~width ~height ~delta:0.;
  ignore (Renderer.render ~full:true r : string)

let mouse_down ~x ~y =
  Input.Mouse.make ~x ~y ~modifiers:Input.Modifier.none (Down { button = Left })

let mouse_drag ~x ~y =
  Input.Mouse.make ~x ~y ~modifiers:Input.Modifier.none (Drag { button = Left })

let mouse_up ~x ~y =
  Input.Mouse.make ~x ~y ~modifiers:Input.Modifier.none
    (Up { button = Some Left })

let ctrl_mod = { no_mod with Input.Modifier.ctrl = true }
let shift_mod = { no_mod with Input.Modifier.shift = true }
let alt_mod = { no_mod with Input.Modifier.alt = true }
let super_mod = { no_mod with Input.Modifier.super = true }
let ctrl_shift_mod = { no_mod with Input.Modifier.ctrl = true; shift = true }

(* ── Props ── *)

let props_default_equals_make () =
  let p = Textarea.Props.default in
  is_true ~msg:"default equals make()"
    (Textarea.Props.equal p (Textarea.Props.make ()))

let props_equal_identical () =
  let a = Textarea.Props.make () in
  let b = Textarea.Props.make () in
  is_true ~msg:"equal" (Textarea.Props.equal a b)

let props_detects_value_diff () =
  let a = Textarea.Props.make ~value:"hello" () in
  let b = Textarea.Props.make ~value:"world" () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_placeholder_diff () =
  let a = Textarea.Props.make ~placeholder:"Enter text" () in
  let b = Textarea.Props.make ~placeholder:"Type here" () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_wrap_diff () =
  let a = Textarea.Props.make ~wrap:`Word () in
  let b = Textarea.Props.make ~wrap:`Char () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_text_color_diff () =
  let a = Textarea.Props.make ~text_color:Ansi.Color.red () in
  let b = Textarea.Props.make ~text_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_cursor_style_diff () =
  let a = Textarea.Props.make ~cursor_style:`Block () in
  let b = Textarea.Props.make ~cursor_style:`Line () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_cursor_blinking_diff () =
  let a = Textarea.Props.make ~cursor_blinking:true () in
  let b = Textarea.Props.make ~cursor_blinking:false () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_spans_diff () =
  let style = Ansi.Style.make ~fg:Ansi.Color.cyan () in
  let a =
    Textarea.Props.make ~spans:[ { Text_buffer.text = "let"; style } ] ()
  in
  let b = Textarea.Props.make ~spans:[] () in
  is_false ~msg:"different" (Textarea.Props.equal a b)

let props_detects_selection_diff () =
  let a = Textarea.Props.make ~selection:(Some (0, 2)) () in
  let b = Textarea.Props.make ~selection:None () in
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
  let buf = Textarea.buffer ta in
  equal ~msg:"buffer text" string "test" (Edit_buffer.text buf)

let create_surface_accessible () =
  let _t, ta = make_textarea () in
  let _surface = Textarea.surface ta in
  is_true ~msg:"surface accessible" true

let create_registers_line_info_provider () =
  let _t, ta = make_textarea ~value:"one\ntwo\nthree" () in
  match Renderable.line_info (Textarea.node ta) with
  | None -> fail "expected line info provider"
  | Some info ->
      equal ~msg:"line count" int 3 info.Renderable.line_count;
      is_true ~msg:"has display lines" (info.display_line_count >= 3)

(* ── Value ── *)

let value_returns_current_text () =
  let _t, ta = make_textarea ~value:"line1\nline2" () in
  equal ~msg:"current text" string "line1\nline2" (Textarea.value ta)

let set_value_replaces_text () =
  let _t, ta = make_textarea ~value:"old" () in
  Textarea.set_value ta "new\ncontent";
  equal ~msg:"replaced" string "new\ncontent" (Textarea.value ta)

let set_value_requests_render () =
  let t, ta = make_textarea () in
  let before = !(t.schedule_count) in
  Textarea.set_value ta "something";
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Callbacks — on_input ── *)

let on_input_fires_on_char_insert () =
  let fired = ref [] in
  let t, ta = make_textarea ~on_input:(fun s -> fired := s :: !fired) () in
  focus_textarea t ta;
  send_char ta 'a';
  equal ~msg:"callback fired once" int 1 (List.length !fired)

let on_input_fires_on_newline_insert () =
  let fired = ref [] in
  let t, ta = make_textarea ~on_input:(fun s -> fired := s :: !fired) () in
  focus_textarea t ta;
  send_char ta 'a';
  send_key ta Input.Key.Enter;
  equal ~msg:"fired twice" int 2 (List.length !fired)

let on_input_does_not_fire_on_cursor_movement () =
  let count = ref 0 in
  let t, ta = make_textarea ~value:"abc" ~on_input:(fun _ -> incr count) () in
  focus_textarea t ta;
  send_key ta Input.Key.Left;
  equal ~msg:"not fired" int 0 !count

let set_on_input_none_disables () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_input:(fun _ -> incr count) () in
  focus_textarea t ta;
  Textarea.set_on_input ta None;
  send_char ta 'a';
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_change ── *)

let on_change_fires_on_blur_when_changed () =
  let fired = ref [] in
  let t, ta = make_textarea ~on_change:(fun s -> fired := s :: !fired) () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  send_char ta 'h';
  send_char ta 'i';
  blur_textarea ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  equal ~msg:"on_change fired" int 1 (List.length !fired)

let on_change_does_not_fire_when_unchanged () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_change:(fun _ -> incr count) () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  blur_textarea ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  equal ~msg:"not fired" int 0 !count

let set_on_change_none_disables () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_change:(fun _ -> incr count) () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  Textarea.set_on_change ta None;
  send_char ta 'z';
  blur_textarea ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_cursor ── *)

let on_cursor_fires_on_cursor_movement () =
  let fired = ref [] in
  let t, ta =
    make_textarea ~value:"abc"
      ~on_cursor:(fun ~cursor ~selection ->
        fired := (cursor, selection) :: !fired)
      ()
  in
  focus_textarea t ta;
  send_key ta Input.Key.Left;
  is_true ~msg:"cursor callback fired" (List.length !fired >= 1)

let on_cursor_fires_on_selection_change () =
  let fired = ref [] in
  let t, ta =
    make_textarea ~value:"abc"
      ~on_cursor:(fun ~cursor ~selection ->
        fired := (cursor, selection) :: !fired)
      ()
  in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:shift_mod Input.Key.Left;
  let has_selection =
    List.exists (fun (_cursor, selection) -> Option.is_some selection) !fired
  in
  is_true ~msg:"selection reported" has_selection

let set_on_cursor_none_disables () =
  let count = ref 0 in
  let t, ta =
    make_textarea ~value:"abc"
      ~on_cursor:(fun ~cursor:_ ~selection:_ -> incr count)
      ()
  in
  focus_textarea t ta;
  Textarea.set_on_cursor ta None;
  send_key ta Input.Key.Left;
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_submit ── *)

let on_submit_fires_on_meta_enter () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:ctrl_mod Input.Key.Enter;
  equal ~msg:"fired" int 1 !count

let on_submit_receives_current_value () =
  let fired = ref [] in
  let t, ta =
    make_textarea ~value:"hello" ~on_submit:(fun s -> fired := s :: !fired) ()
  in
  focus_textarea t ta;
  send_key_with_mod ta ~modifier:ctrl_mod Input.Key.Enter;
  match !fired with
  | v :: _ -> equal ~msg:"received value" string "hello" v
  | [] -> fail "on_submit not fired"

let set_on_submit_none_disables () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  Textarea.set_on_submit ta None;
  send_key_with_mod ta ~modifier:ctrl_mod Input.Key.Enter;
  equal ~msg:"disabled" int 0 !count

let enter_does_not_fire_submit () =
  let count = ref 0 in
  let t, ta = make_textarea ~on_submit:(fun _ -> incr count) () in
  focus_textarea t ta;
  send_key ta Input.Key.Enter;
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
  send_key ta Input.Key.Enter;
  send_char ta 'b';
  equal ~msg:"newline" string "a\nb" (Textarea.value ta)

let key_left_right_moves_cursor () =
  let t, ta = make_textarea ~value:"ab" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  equal ~msg:"cursor at end" int 2 (Edit_buffer.cursor buf);
  send_key ta Input.Key.Left;
  equal ~msg:"cursor moved left" int 1 (Edit_buffer.cursor buf);
  send_key ta Input.Key.Right;
  equal ~msg:"cursor moved right" int 2 (Edit_buffer.cursor buf)

let key_backspace_deletes_backward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  send_key ta Input.Key.Backspace;
  equal ~msg:"deleted" string "ab" (Textarea.value ta)

let key_backspace_at_line_start_joins () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 4;
  send_key ta Input.Key.Backspace;
  equal ~msg:"joined" string "abcdef" (Textarea.value ta)

let key_delete_deletes_forward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_key ta Input.Key.Delete;
  equal ~msg:"deleted forward" string "bc" (Textarea.value ta)

(* ── Emacs Keybindings ── *)

let ctrl_a_moves_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 6;
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at line start" int 4 (Edit_buffer.cursor buf)

let ctrl_a_uppercase_from_parser_moves_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 6;
  (* Runtime parser emits Ctrl+A as 'A' with ctrl=true. *)
  send_char_with_mod ta ~modifier:ctrl_mod 'A';
  equal ~msg:"cursor at line start" int 4 (Edit_buffer.cursor buf)

let ctrl_e_moves_to_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 4;
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at line end" int 7 (Edit_buffer.cursor buf)

let ctrl_b_moves_left () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  send_char_with_mod ta ~modifier:ctrl_mod 'b';
  equal ~msg:"cursor moved left" int 2 (Edit_buffer.cursor buf)

let ctrl_f_moves_right () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod ta ~modifier:ctrl_mod 'f';
  equal ~msg:"cursor moved right" int 1 (Edit_buffer.cursor buf)

let ctrl_d_deletes_forward () =
  let t, ta = make_textarea ~value:"abc" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod ta ~modifier:ctrl_mod 'd';
  equal ~msg:"deleted forward" string "bc" (Textarea.value ta)

let ctrl_k_deletes_to_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 4;
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  equal ~msg:"deleted to line end" string "abc\n" (Textarea.value ta)

let ctrl_u_deletes_to_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 6;
  send_char_with_mod ta ~modifier:ctrl_mod 'u';
  equal ~msg:"deleted to line start" string "abc\nf" (Textarea.value ta)

let ctrl_shift_d_deletes_line () =
  let t, ta = make_textarea ~value:"abc\ndef\nghi" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 5;
  send_char_with_mod ta ~modifier:ctrl_shift_mod 'd';
  equal ~msg:"line deleted" string "abc\nghi" (Textarea.value ta)

(* ── Alt Keybindings ── *)

let alt_b_moves_word_backward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  send_char_with_mod ta ~modifier:alt_mod 'b';
  equal ~msg:"moved to word boundary" int 6 (Edit_buffer.cursor buf)

let alt_f_moves_word_forward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod ta ~modifier:alt_mod 'f';
  equal ~msg:"moved to word end" int 6 (Edit_buffer.cursor buf)

let alt_d_deletes_word_forward () =
  let t, ta = make_textarea ~value:"hello world" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod ta ~modifier:alt_mod 'd';
  equal ~msg:"word deleted forward" string "world" (Textarea.value ta)

(* ── Super+Arrow Keybindings ── *)

let super_left_moves_to_visual_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (render_textarea ta ~width:40 ~height:10 : Grid.t);
  Edit_buffer.set_cursor buf 6;
  send_key_with_mod ta ~modifier:super_mod Input.Key.Left;
  equal ~msg:"cursor at visual line start" int 4 (Edit_buffer.cursor buf)

let super_right_moves_to_visual_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (render_textarea ta ~width:40 ~height:10 : Grid.t);
  Edit_buffer.set_cursor buf 4;
  send_key_with_mod ta ~modifier:super_mod Input.Key.Right;
  equal ~msg:"cursor at visual line end" int 7 (Edit_buffer.cursor buf)

let super_up_moves_to_buffer_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  send_key_with_mod ta ~modifier:super_mod Input.Key.Up;
  equal ~msg:"cursor at start" int 0 (Edit_buffer.cursor buf)

let super_down_moves_to_buffer_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod ta ~modifier:super_mod Input.Key.Down;
  equal ~msg:"cursor at end" int 7 (Edit_buffer.cursor buf)

(* ── Selection Keybindings ── *)

let shift_right_creates_selection () =
  let t, ta = make_textarea ~value:"hello" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod ta ~modifier:shift_mod Input.Key.Right;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf)

let super_a_selects_all () =
  let t, ta = make_textarea ~value:"hello\nworld" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  send_char_with_mod ta ~modifier:super_mod 'a';
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"all selected" string "hello\nworld"
    (Edit_buffer.selected_text buf)

let type_with_selection_replaces () =
  let t, ta = make_textarea ~value:"hello" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.select_all buf;
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
  send_char_with_mod ta ~modifier:ctrl_shift_mod 'z';
  equal ~msg:"after redo" string "a" (Textarea.value ta)

(* ── Visual Line Navigation ── *)

let alt_a_moves_to_visual_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (render_textarea ta ~width:40 ~height:10 : Grid.t);
  Edit_buffer.set_cursor buf 6;
  send_char_with_mod ta ~modifier:alt_mod 'a';
  equal ~msg:"cursor at visual line start" int 4 (Edit_buffer.cursor buf)

let alt_e_moves_to_visual_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  ignore (render_textarea ta ~width:40 ~height:10 : Grid.t);
  Edit_buffer.set_cursor buf 4;
  send_char_with_mod ta ~modifier:alt_mod 'e';
  equal ~msg:"cursor at visual line end" int 7 (Edit_buffer.cursor buf)

(* ── Ctrl+A/E Wrap-Around ── *)

let ctrl_a_wraps_to_previous_line_end () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 4;
  (* cursor at start of "def", Ctrl+A should wrap to end of "abc" *)
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at end of previous line" int 3 (Edit_buffer.cursor buf)

let ctrl_a_does_not_wrap_on_first_line () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 0;
  send_char_with_mod ta ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor stays at 0" int 0 (Edit_buffer.cursor buf)

let ctrl_e_wraps_to_next_line_start () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 3;
  (* cursor at end of "abc", Ctrl+E should wrap to start of "def" *)
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at start of next line" int 4 (Edit_buffer.cursor buf)

let ctrl_e_does_not_wrap_on_last_line () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 7;
  send_char_with_mod ta ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor stays at end" int 7 (Edit_buffer.cursor buf)

(* ── Alt+Enter Submit ── *)

let alt_enter_fires_submit () =
  let received = ref "" in
  let on_submit v = received := v in
  let t, ta = make_textarea ~value:"hello" ~on_submit () in
  focus_textarea t ta;
  let alt_mod = { no_mod with Input.Modifier.alt = true } in
  send_key_with_mod ta ~modifier:alt_mod Input.Key.Enter;
  equal ~msg:"submit fired" string "hello" !received

(* ── Ctrl+K at Line End ── *)

let ctrl_k_at_line_end_does_nothing () =
  let t, ta = make_textarea ~value:"abc\ndef" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 3;
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  equal ~msg:"text unchanged" string "abc\ndef" (Textarea.value ta);
  equal ~msg:"cursor unchanged" int 3 (Edit_buffer.cursor buf)

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
  let buf = Textarea.buffer ta in
  Edit_buffer.select_all buf;
  Textarea.handle_paste ta "new\ntext";
  equal ~msg:"replaced" string "new\ntext" (Textarea.value ta)

(* ── Mouse ── *)

let mouse_selection_syncs_to_buffer () =
  let r = Renderer.create () in
  let ta =
    Textarea.create ~parent:(Renderer.root r)
      ~style:(sized_style ~width:20 ~height:3)
      ~value:"hello world" ()
  in
  render_frame r ~width:20 ~height:3;
  Renderer.dispatch_mouse r (mouse_down ~x:0 ~y:0);
  Renderer.dispatch_mouse r (mouse_drag ~x:5 ~y:0);
  Renderer.dispatch_mouse r (mouse_up ~x:5 ~y:0);
  some ~msg:"selection" (pair int int) (0, 5) (Textarea.selection ta);
  equal ~msg:"selected text" string "hello"
    (Edit_buffer.selected_text (Textarea.buffer ta))

let mouse_wheel_scrolls_surface () =
  let lines = "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten" in
  let _t, ta = make_textarea ~value:lines () in
  ignore (render_textarea ta ~width:20 ~height:3 : Grid.t);
  let ev =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier
      (Scroll { direction = Scroll_down; delta = 2 })
  in
  Renderable.Private.emit_mouse (Textarea.node ta) ev;
  is_true ~msg:"scroll default prevented" (Event.Mouse.default_prevented ev);
  equal ~msg:"scroll_y" int 2 (Text_surface.scroll_y (Textarea.surface ta))

(* ── Cursor Provider ── *)

let cursor_returns_none_when_unfocused () =
  let _t, ta = make_textarea ~value:"hi" () in
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  let node = Textarea.node ta in
  let cursor = Renderable.cursor node in
  is_none ~msg:"no cursor when unfocused" cursor

let cursor_returns_some_when_focused () =
  let t, ta = make_textarea ~value:"hi" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  let node = Textarea.node ta in
  let cursor = Renderable.cursor node in
  is_some ~msg:"cursor when focused" cursor

let cursor_style_is_block_by_default () =
  let t, ta = make_textarea ~value:"hi" () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  let node = Textarea.node ta in
  match Renderable.cursor node with
  | Some c -> is_true ~msg:"block style" (c.style = `Block)
  | None -> fail "expected cursor"

let cursor_has_correct_color () =
  let t, ta = make_textarea ~value:"hi" ~cursor_color:Ansi.Color.red () in
  focus_textarea t ta;
  ignore (render_textarea ta ~width:20 ~height:5 : Grid.t);
  let node = Textarea.node ta in
  match Renderable.cursor node with
  | Some c ->
      is_true ~msg:"cursor color is red"
        (Ansi.Color.equal c.color Ansi.Color.red)
  | None -> fail "expected cursor"

(* ── apply_props ── *)

let apply_props_value_change () =
  let _t, ta = make_textarea ~value:"old" () in
  let props = Textarea.Props.make ~value:"new" () in
  Textarea.apply_props ta props;
  equal ~msg:"value updated" string "new" (Textarea.value ta)

let apply_props_echoed_value_preserves_cursor () =
  let t, ta = make_textarea ~value:"aaa\nbbb\nccc" () in
  focus_textarea t ta;
  let buf = Textarea.buffer ta in
  Edit_buffer.set_cursor buf 5;
  send_char_with_mod ta ~modifier:ctrl_shift_mod 'd';
  let echoed = Textarea.value ta in
  let cursor_before = Edit_buffer.cursor buf in
  is_true ~msg:"cursor not at end before apply_props"
    (cursor_before < Edit_buffer.length buf);
  Textarea.apply_props ta (Textarea.Props.make ~value:echoed ());
  equal ~msg:"cursor preserved after echoed value reconcile" int cursor_before
    (Edit_buffer.cursor buf)

let apply_props_selection_change_updates_buffer () =
  let _t, ta = make_textarea ~value:"hello\nworld" () in
  Textarea.apply_props ta
    (Textarea.Props.make ~value:"hello\nworld" ~selection:(Some (1, 5)) ());
  some ~msg:"selection updated" (pair int int) (1, 5) (Textarea.selection ta)

let apply_props_selection_none_clears_buffer_selection () =
  let _t, ta =
    make_textarea ~value:"hello\nworld" ~selection:(Some (1, 5)) ()
  in
  Textarea.apply_props ta
    (Textarea.Props.make ~value:"hello\nworld" ~selection:None ());
  is_none ~msg:"selection cleared" (Textarea.selection ta)

let apply_props_schedules_render () =
  let t, ta = make_textarea () in
  let before = !(t.schedule_count) in
  Textarea.apply_props ta Textarea.Props.default;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Rendering ── *)

let render_zero_size_no_crash () =
  let _t, ta = make_textarea ~value:"hello\nworld" () in
  let node = Textarea.node ta in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  let grid = make_grid ~width:10 ~height:10 () in
  Renderable.Private.render node grid ~delta:0.;
  is_true ~msg:"render completed" true

(* ── Edge Cases ── *)

let operations_on_empty_textarea_are_safe () =
  let t, ta = make_textarea () in
  focus_textarea t ta;
  send_key ta Input.Key.Backspace;
  send_key ta Input.Key.Delete;
  send_key ta Input.Key.Left;
  send_key ta Input.Key.Right;
  send_key ta Input.Key.Up;
  send_key ta Input.Key.Down;
  send_key ta Input.Key.Home;
  send_key ta Input.Key.End;
  send_char_with_mod ta ~modifier:ctrl_mod 'w';
  send_char_with_mod ta ~modifier:ctrl_mod 'k';
  send_char_with_mod ta ~modifier:ctrl_mod 'u';
  send_char_with_mod ta ~modifier:ctrl_mod 'z';
  equal ~msg:"still empty" string "" (Textarea.value ta)

(* ── Pretty-printing ── *)

let pp_produces_non_empty_output () =
  let _t, ta = make_textarea () in
  let s = Format.asprintf "%a" Textarea.pp ta in
  is_true ~msg:"non-empty" (String.length s > 0)

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

let () =
  run "mosaic.textarea"
    [
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
          test "detects spans diff" props_detects_spans_diff;
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
            create_registers_line_info_provider;
        ];
      group "Value"
        [
          test "returns current text" value_returns_current_text;
          test "set_value replaces text" set_value_replaces_text;
          test "set_value requests render" set_value_requests_render;
        ];
      group "Callbacks -- on_input"
        [
          test "fires on character insert" on_input_fires_on_char_insert;
          test "fires on newline insert" on_input_fires_on_newline_insert;
          test "does not fire on cursor movement"
            on_input_does_not_fire_on_cursor_movement;
          test "set_on_input None disables" set_on_input_none_disables;
        ];
      group "Callbacks -- on_change"
        [
          test "fires on blur when changed" on_change_fires_on_blur_when_changed;
          test "does not fire when unchanged"
            on_change_does_not_fire_when_unchanged;
          test "set_on_change None disables" set_on_change_none_disables;
        ];
      group "Callbacks -- on_cursor"
        [
          test "fires on cursor movement" on_cursor_fires_on_cursor_movement;
          test "fires on selection change" on_cursor_fires_on_selection_change;
          test "set_on_cursor None disables" set_on_cursor_none_disables;
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
          test "Left/Right moves cursor" key_left_right_moves_cursor;
          test "Backspace deletes backward" key_backspace_deletes_backward;
          test "Backspace at line start joins" key_backspace_at_line_start_joins;
          test "Delete key deletes forward" key_delete_deletes_forward;
        ];
      group "Emacs keybindings"
        [
          test "Ctrl+A moves to line start" ctrl_a_moves_to_line_start;
          test "Ctrl+A uppercase from parser moves to line start"
            ctrl_a_uppercase_from_parser_moves_to_line_start;
          test "Ctrl+E moves to line end" ctrl_e_moves_to_line_end;
          test "Ctrl+B moves left" ctrl_b_moves_left;
          test "Ctrl+F moves right" ctrl_f_moves_right;
          test "Ctrl+D deletes forward" ctrl_d_deletes_forward;
          test "Ctrl+K deletes to line end" ctrl_k_deletes_to_line_end;
          test "Ctrl+U deletes to line start" ctrl_u_deletes_to_line_start;
          test "Ctrl+Shift+D deletes line" ctrl_shift_d_deletes_line;
        ];
      group "Alt keybindings"
        [
          test "Alt+B moves word backward" alt_b_moves_word_backward;
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
          test "wheel scrolls surface" mouse_wheel_scrolls_surface;
        ];
      group "Cursor provider"
        [
          test "returns None when unfocused" cursor_returns_none_when_unfocused;
          test "returns Some when focused" cursor_returns_some_when_focused;
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
    ]
