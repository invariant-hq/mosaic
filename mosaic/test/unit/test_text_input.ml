open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_input ?value ?placeholder ?max_length ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ?cursor ?selection ?on_input ?on_change ?on_submit
    ?on_cursor () =
  let t = make_ctx () in
  let root = make_root t in
  let input =
    Text_input.create ~parent:root ?value ?placeholder ?max_length ?text_color
      ?background_color ?focused_text_color ?focused_background_color
      ?placeholder_color ?selection_color ?selection_fg ?cursor_style
      ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?cursor ?selection
      ?on_input ?on_change ?on_submit ?on_cursor ()
  in
  (t, input)

let render_input input ~width ~height =
  let node = Text_input.node input in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~delta:0.;
  grid

let no_mod = Input.Modifier.none

let send_key input key =
  let ev = Event.Key.of_input (Input.Key.make key) in
  Renderable.Private.emit_default_key (Text_input.node input) ev

let send_key_with_mod input ~modifier key =
  let ev = Event.Key.of_input (Input.Key.make ~modifier key) in
  Renderable.Private.emit_default_key (Text_input.node input) ev

let send_char input c =
  let text = String.make 1 c in
  let ev = Event.Key.of_input (Input.Key.of_char ~associated_text:text c) in
  Renderable.Private.emit_default_key (Text_input.node input) ev

let send_char_with_mod input ~modifier c =
  let ev = Event.Key.of_input (Input.Key.of_char ~modifier c) in
  Renderable.Private.emit_default_key (Text_input.node input) ev

let send_release_key input key =
  let ev = Event.Key.of_input (Input.Key.make ~event_type:Release key) in
  Renderable.Private.emit_default_key (Text_input.node input) ev

let focus_input t input =
  let node = Text_input.node input in
  Renderable.set_focusable node true;
  let _focused = t.ctx.focus node in
  ()

let blur_input input = Renderable.Private.blur_direct (Text_input.node input)

let string_contains haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if (not !found) && String.sub haystack i nlen = needle then found := true
    done;
    !found

let ctrl_mod = { no_mod with Input.Modifier.ctrl = true }
let shift_mod = { no_mod with Input.Modifier.shift = true }
let alt_mod = { no_mod with Input.Modifier.alt = true }
let super_mod = { no_mod with Input.Modifier.super = true }
let ctrl_shift_mod = { no_mod with Input.Modifier.ctrl = true; shift = true }
let super_shift_mod = { no_mod with Input.Modifier.super = true; shift = true }

(* ── Props ── *)

let props_default_equals_make () =
  let p = Text_input.Props.default in
  is_true ~msg:"default equals make()"
    (Text_input.Props.equal p (Text_input.Props.make ()))

let props_equal_identical () =
  let a = Text_input.Props.make () in
  let b = Text_input.Props.make () in
  is_true ~msg:"equal" (Text_input.Props.equal a b)

let props_detects_value_diff () =
  let a = Text_input.Props.make ~value:"hello" () in
  let b = Text_input.Props.make ~value:"world" () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_placeholder_diff () =
  let a = Text_input.Props.make ~placeholder:"Enter name" () in
  let b = Text_input.Props.make ~placeholder:"Enter email" () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_max_length_diff () =
  let a = Text_input.Props.make ~max_length:10 () in
  let b = Text_input.Props.make ~max_length:20 () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_text_color_diff () =
  let a = Text_input.Props.make ~text_color:Ansi.Color.red () in
  let b = Text_input.Props.make ~text_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_background_color_diff () =
  let a = Text_input.Props.make ~background_color:Ansi.Color.red () in
  let b = Text_input.Props.make ~background_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_style_diff () =
  let a = Text_input.Props.make ~cursor_style:`Block () in
  let b = Text_input.Props.make ~cursor_style:`Line () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_selection_fg_diff () =
  let a = Text_input.Props.make ~selection_fg:Ansi.Color.red () in
  let b = Text_input.Props.make ~selection_fg:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_color_diff () =
  let a = Text_input.Props.make ~cursor_color:Ansi.Color.red () in
  let b = Text_input.Props.make ~cursor_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_blinking_diff () =
  let a = Text_input.Props.make ~cursor_blinking:true () in
  let b = Text_input.Props.make ~cursor_blinking:false () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

(* ── Construction ── *)

let create_attaches_to_parent () =
  let _t, input = make_input () in
  let node = Text_input.node input in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_node_returns_renderable () =
  let _t, input = make_input () in
  let node = Text_input.node input in
  is_true ~msg:"node is renderable" (String.length (Renderable.id node) > 0)

let create_buffer_returns_edit_buffer () =
  let _t, input = make_input () in
  let buf = Text_input.buffer input in
  is_true ~msg:"buffer exists" (Edit_buffer.max_length buf > 0)

let create_initial_value_matches () =
  let _t, input = make_input ~value:"hello" () in
  equal ~msg:"value matches" string "hello" (Text_input.value input)

let create_default_value_is_empty () =
  let _t, input = make_input () in
  equal ~msg:"default empty" string "" (Text_input.value input)

(* ── Value ── *)

let value_returns_current_text () =
  let _t, input = make_input ~value:"test" () in
  equal ~msg:"current text" string "test" (Text_input.value input)

let set_value_replaces_text () =
  let _t, input = make_input ~value:"old" () in
  Text_input.set_value input "new";
  equal ~msg:"replaced" string "new" (Text_input.value input)

let set_value_requests_render () =
  let t, input = make_input () in
  let before = !(t.schedule_count) in
  Text_input.set_value input "something";
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

let set_value_updates_buffer () =
  let _t, input = make_input ~value:"original" () in
  Text_input.set_value input "replaced";
  let buf = Text_input.buffer input in
  equal ~msg:"buffer updated" string "replaced" (Edit_buffer.text buf)

(* ── Callbacks — on_input ── *)

let on_input_fires_on_char_insert () =
  let fired = ref [] in
  let t, input = make_input ~on_input:(fun s -> fired := s :: !fired) () in
  focus_input t input;
  send_char input 'a';
  equal ~msg:"callback fired once" int 1 (List.length !fired)

let on_input_receives_current_value () =
  let fired = ref [] in
  let t, input = make_input ~on_input:(fun s -> fired := s :: !fired) () in
  focus_input t input;
  send_char input 'x';
  match !fired with
  | v :: _ -> equal ~msg:"received value" string "x" v
  | [] -> fail "on_input not fired"

let on_input_does_not_fire_on_cursor_movement () =
  let count = ref 0 in
  let t, input = make_input ~value:"abc" ~on_input:(fun _ -> incr count) () in
  focus_input t input;
  send_key input Input.Key.Left;
  equal ~msg:"not fired" int 0 !count

let set_on_input_none_disables () =
  let count = ref 0 in
  let t, input = make_input ~on_input:(fun _ -> incr count) () in
  focus_input t input;
  Text_input.set_on_input input None;
  send_char input 'a';
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_cursor ── *)

let on_cursor_fires_on_cursor_movement () =
  let fired = ref [] in
  let t, input =
    make_input ~value:"abc"
      ~on_cursor:(fun ~cursor ~selection ->
        fired := (cursor, selection) :: !fired)
      ()
  in
  focus_input t input;
  send_key input Input.Key.Left;
  is_true ~msg:"cursor callback fired" (List.length !fired >= 1)

let on_cursor_fires_on_selection_change () =
  let fired = ref [] in
  let t, input =
    make_input ~value:"abc"
      ~on_cursor:(fun ~cursor ~selection ->
        fired := (cursor, selection) :: !fired)
      ()
  in
  focus_input t input;
  send_key_with_mod input ~modifier:shift_mod Input.Key.Left;
  let has_selection =
    List.exists (fun (_cursor, selection) -> Option.is_some selection) !fired
  in
  is_true ~msg:"selection reported" has_selection

let set_on_cursor_none_disables () =
  let count = ref 0 in
  let t, input =
    make_input ~value:"abc"
      ~on_cursor:(fun ~cursor:_ ~selection:_ -> incr count)
      ()
  in
  focus_input t input;
  Text_input.set_on_cursor input None;
  send_key input Input.Key.Left;
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_change ── *)

let on_change_fires_on_blur_when_changed () =
  let fired = ref [] in
  let t, input = make_input ~on_change:(fun s -> fired := s :: !fired) () in
  focus_input t input;
  (* Render to set was_focused and snapshot last_committed_value *)
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  (* Edit text *)
  send_char input 'h';
  send_char input 'i';
  (* Blur *)
  blur_input input;
  (* Render again to detect focus loss *)
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  equal ~msg:"on_change fired" int 1 (List.length !fired)

let on_change_does_not_fire_on_blur_when_unchanged () =
  let count = ref 0 in
  let t, input = make_input ~on_change:(fun _ -> incr count) () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  (* No edits *)
  blur_input input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  equal ~msg:"not fired" int 0 !count

let on_change_fires_on_submit_when_changed () =
  let fired = ref [] in
  let t, input = make_input ~on_change:(fun s -> fired := s :: !fired) () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  send_char input 'a';
  send_key input Input.Key.Enter;
  is_true ~msg:"on_change fired on submit" (List.length !fired >= 1)

let set_on_change_none_disables () =
  let count = ref 0 in
  let t, input = make_input ~on_change:(fun _ -> incr count) () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  Text_input.set_on_change input None;
  send_char input 'z';
  blur_input input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  equal ~msg:"disabled" int 0 !count

(* ── Callbacks — on_submit ── *)

let on_submit_fires_on_enter () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_key input Input.Key.Enter;
  equal ~msg:"fired" int 1 !count

let on_submit_receives_current_value () =
  let fired = ref [] in
  let t, input =
    make_input ~value:"hello" ~on_submit:(fun s -> fired := s :: !fired) ()
  in
  focus_input t input;
  send_key input Input.Key.Enter;
  match !fired with
  | v :: _ -> equal ~msg:"received value" string "hello" v
  | [] -> fail "on_submit not fired"

let set_on_submit_none_disables () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  Text_input.set_on_submit input None;
  send_key input Input.Key.Enter;
  equal ~msg:"disabled" int 0 !count

(* ── Key Handling ── *)

let key_char_inserts_text () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'a';
  send_char input 'b';
  send_char input 'c';
  equal ~msg:"inserted" string "abc" (Text_input.value input)

let key_left_right_moves_cursor () =
  let t, input = make_input ~value:"ab" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  (* Cursor starts at end (position 2) *)
  equal ~msg:"cursor at end" int 2 (Edit_buffer.cursor buf);
  send_key input Input.Key.Left;
  equal ~msg:"cursor moved left" int 1 (Edit_buffer.cursor buf);
  send_key input Input.Key.Right;
  equal ~msg:"cursor moved right" int 2 (Edit_buffer.cursor buf)

let key_home_end_moves_cursor () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key input Input.Key.Home;
  equal ~msg:"cursor at home" int 0 (Edit_buffer.cursor buf);
  send_key input Input.Key.End;
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let key_backspace_deletes_backward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  send_key input Input.Key.Backspace;
  equal ~msg:"deleted" string "ab" (Text_input.value input)

let key_delete_deletes_forward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key input Input.Key.Delete;
  equal ~msg:"deleted forward" string "bc" (Text_input.value input)

let key_ctrl_w_deletes_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'w';
  equal ~msg:"word deleted" string "hello " (Text_input.value input)

let key_ctrl_k_deletes_to_end () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  for _ = 1 to 5 do
    ignore (Edit_buffer.move_right buf : bool)
  done;
  send_char_with_mod input ~modifier:ctrl_mod 'k';
  equal ~msg:"deleted to end" string "hello" (Text_input.value input)

let key_ctrl_u_deletes_to_start () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'u';
  equal ~msg:"deleted to start" string "" (Text_input.value input)

let key_enter_fires_submit () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_key input Input.Key.Enter;
  equal ~msg:"submitted" int 1 !count

let key_release_is_ignored () =
  let count = ref 0 in
  let t, input = make_input ~on_input:(fun _ -> incr count) () in
  focus_input t input;
  send_release_key input (Input.Key.Char (Uchar.of_char 'a'));
  equal ~msg:"release ignored" int 0 !count

let key_max_length_rejects_excess_typing () =
  let t, input = make_input ~max_length:3 () in
  focus_input t input;
  send_char input 'a';
  send_char input 'b';
  send_char input 'c';
  send_char input 'd';
  send_char input 'e';
  equal ~msg:"max 3 chars" string "abc" (Text_input.value input)

(* ── Emacs Keybindings ── *)

let ctrl_a_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf);
  send_char_with_mod input ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at start" int 0 (Edit_buffer.cursor buf)

let ctrl_a_uppercase_from_parser_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf);
  (* Runtime parser emits Ctrl+A as 'A' with ctrl=true. *)
  send_char_with_mod input ~modifier:ctrl_mod 'A';
  equal ~msg:"cursor at start" int 0 (Edit_buffer.cursor buf)

let ctrl_e_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod input ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let ctrl_b_moves_left () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_char_with_mod input ~modifier:ctrl_mod 'b';
  equal ~msg:"cursor moved left" int 2 (Edit_buffer.cursor buf)

let ctrl_f_moves_right () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod input ~modifier:ctrl_mod 'f';
  equal ~msg:"cursor moved right" int 1 (Edit_buffer.cursor buf)

let ctrl_d_deletes_forward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod input ~modifier:ctrl_mod 'd';
  equal ~msg:"deleted forward" string "bc" (Text_input.value input)

let ctrl_shift_d_deletes_line () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_shift_mod 'd';
  equal ~msg:"line deleted" string "" (Text_input.value input)

(* ── Alt Keybindings ── *)

let alt_b_moves_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_char_with_mod input ~modifier:alt_mod 'b';
  equal ~msg:"moved to word boundary" int 6 (Edit_buffer.cursor buf)

let alt_f_moves_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod input ~modifier:alt_mod 'f';
  equal ~msg:"moved to word end" int 6 (Edit_buffer.cursor buf)

let alt_d_deletes_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_char_with_mod input ~modifier:alt_mod 'd';
  equal ~msg:"word deleted forward" string "world" (Text_input.value input)

let alt_backspace_deletes_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_key_with_mod input ~modifier:alt_mod Input.Key.Backspace;
  equal ~msg:"word deleted backward" string "hello " (Text_input.value input)

(* ── Ctrl/Alt+Arrow Word Movement ── *)

let ctrl_left_moves_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:ctrl_mod Input.Key.Left;
  equal ~msg:"moved to word boundary" int 6 (Edit_buffer.cursor buf)

let ctrl_right_moves_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:ctrl_mod Input.Key.Right;
  equal ~msg:"moved to word end" int 6 (Edit_buffer.cursor buf)

(* ── Super+Arrow Keybindings ── *)

let super_left_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:super_mod Input.Key.Left;
  equal ~msg:"cursor at start" int 0 (Edit_buffer.cursor buf)

let super_right_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:super_mod Input.Key.Right;
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let super_up_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:super_mod Input.Key.Up;
  equal ~msg:"cursor at start" int 0 (Edit_buffer.cursor buf)

let super_down_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:super_mod Input.Key.Down;
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let super_shift_left_selects_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:super_shift_mod Input.Key.Left;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected all" string "hello" (Edit_buffer.selected_text buf)

let super_shift_right_selects_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:super_shift_mod Input.Key.Right;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected all" string "hello" (Edit_buffer.selected_text buf)

(* ── Ctrl+-/. Undo/Redo ── *)

let ctrl_minus_undoes () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'a';
  equal ~msg:"before undo" string "a" (Text_input.value input);
  send_char_with_mod input ~modifier:ctrl_mod '-';
  equal ~msg:"after undo" string "" (Text_input.value input)

let ctrl_dot_redoes () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'a';
  send_char_with_mod input ~modifier:ctrl_mod '-';
  equal ~msg:"after undo" string "" (Text_input.value input);
  send_char_with_mod input ~modifier:ctrl_mod '.';
  equal ~msg:"after redo" string "a" (Text_input.value input)

(* ── Selection Keybindings ── *)

let shift_right_creates_selection () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:shift_mod Input.Key.Right;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf)

let shift_left_creates_selection () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:shift_mod Input.Key.Left;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf)

let shift_home_selects_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_key_with_mod input ~modifier:shift_mod Input.Key.Home;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected all" string "hello" (Edit_buffer.selected_text buf)

let shift_end_selects_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key_with_mod input ~modifier:shift_mod Input.Key.End;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected all" string "hello" (Edit_buffer.selected_text buf)

let super_a_selects_all () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  send_char_with_mod input ~modifier:super_mod 'a';
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"all selected" string "hello world" (Edit_buffer.selected_text buf)

let type_with_selection_replaces () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  Edit_buffer.select_all buf;
  send_char input 'x';
  equal ~msg:"replaced" string "x" (Text_input.value input)

(* ── Undo / Redo via Keyboard ── *)

let ctrl_z_undoes () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'a';
  equal ~msg:"before undo" string "a" (Text_input.value input);
  send_char_with_mod input ~modifier:ctrl_mod 'z';
  equal ~msg:"after undo" string "" (Text_input.value input)

let ctrl_shift_z_redoes () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'a';
  send_char_with_mod input ~modifier:ctrl_mod 'z';
  equal ~msg:"after undo" string "" (Text_input.value input);
  send_char_with_mod input ~modifier:ctrl_shift_mod 'z';
  equal ~msg:"after redo" string "a" (Text_input.value input)

let super_z_undoes () =
  let t, input = make_input () in
  focus_input t input;
  send_char input 'b';
  equal ~msg:"before undo" string "b" (Text_input.value input);
  send_char_with_mod input ~modifier:super_mod 'z';
  equal ~msg:"after undo" string "" (Text_input.value input)

(* ── Paste Handling ── *)

let handle_paste_inserts_text () =
  let _t, input = make_input () in
  Text_input.handle_paste input "pasted";
  equal ~msg:"pasted" string "pasted" (Text_input.value input)

let handle_paste_strips_newlines () =
  let _t, input = make_input () in
  Text_input.handle_paste input "line1\nline2\nline3";
  let v = Text_input.value input in
  is_false ~msg:"no newlines" (String.contains v '\n')

let handle_paste_strips_ansi () =
  let _t, input = make_input () in
  Text_input.handle_paste input "\027[31mred\027[0m";
  equal ~msg:"stripped" string "red" (Text_input.value input)

let handle_paste_with_selection_replaces () =
  let _t, input = make_input ~value:"old" () in
  let buf = Text_input.buffer input in
  Edit_buffer.select_all buf;
  Text_input.handle_paste input "new";
  equal ~msg:"replaced" string "new" (Text_input.value input)

(* ── Rendering ── *)

let render_zero_size_no_crash () =
  let _t, input = make_input ~value:"hello" () in
  let node = Text_input.node input in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  let grid = make_grid ~width:10 ~height:10 () in
  Renderable.Private.render node grid ~delta:0.;
  is_true ~msg:"render completed" true

(* ── Cursor Provider ── *)

let cursor_returns_none_when_unfocused () =
  let _t, input = make_input ~value:"hi" () in
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  let node = Text_input.node input in
  let cursor = Renderable.cursor node in
  is_none ~msg:"no cursor when unfocused" cursor

let cursor_returns_some_when_focused () =
  let t, input = make_input ~value:"hi" () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  let node = Text_input.node input in
  let cursor = Renderable.cursor node in
  is_some ~msg:"cursor when focused" cursor

let cursor_style_is_block_by_default () =
  let t, input = make_input ~value:"hi" () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c -> is_true ~msg:"block style" (c.style = `Block)
  | None -> fail "expected cursor"

let cursor_has_correct_color () =
  let t, input = make_input ~value:"hi" ~cursor_color:Ansi.Color.red () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c ->
      is_true ~msg:"cursor color is red"
        (Ansi.Color.equal c.color Ansi.Color.red)
  | None -> fail "expected cursor"

let cursor_has_correct_blinking () =
  let t, input = make_input ~value:"hi" ~cursor_blinking:false () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c -> is_false ~msg:"not blinking" c.blinking
  | None -> fail "expected cursor"

(* ── apply_props ── *)

let apply_props_value_change_updates_buffer () =
  let _t, input = make_input ~value:"old" () in
  let props = Text_input.Props.make ~value:"new" () in
  Text_input.apply_props input props;
  equal ~msg:"value updated" string "new" (Text_input.value input)

let apply_props_max_length_change_updates_buffer () =
  let _t, input = make_input ~value:"hello world" () in
  let props = Text_input.Props.make ~value:"hello world" ~max_length:5 () in
  Text_input.apply_props input props;
  let buf = Text_input.buffer input in
  equal ~msg:"max_length updated" int 5 (Edit_buffer.max_length buf)

let apply_props_cursor_change_updates_buffer () =
  let _t, input = make_input ~value:"hello" () in
  let props = Text_input.Props.make ~value:"hello" ~cursor:2 () in
  Text_input.apply_props input props;
  equal ~msg:"cursor updated" int 2 (Text_input.cursor input)

let apply_props_selection_change_updates_buffer () =
  let _t, input = make_input ~value:"hello" () in
  let props =
    Text_input.Props.make ~value:"hello" ~selection:(Some (1, 4)) ()
  in
  Text_input.apply_props input props;
  some ~msg:"selection updated" (pair int int) (1, 4)
    (Text_input.selection input)

let apply_props_selection_none_clears_buffer_selection () =
  let _t, input = make_input ~value:"hello" ~selection:(Some (1, 4)) () in
  Text_input.apply_props input
    (Text_input.Props.make ~value:"hello" ~selection:None ());
  is_none ~msg:"selection cleared" (Text_input.selection input)

let apply_props_schedules_render () =
  let t, input = make_input () in
  let before = !(t.schedule_count) in
  Text_input.apply_props input Text_input.Props.default;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Edge Cases ── *)

let cursor_left_at_start_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  equal ~msg:"at start" int 0 (Edit_buffer.cursor buf);
  send_key input Input.Key.Left;
  equal ~msg:"still at start" int 0 (Edit_buffer.cursor buf)

let cursor_right_at_end_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  equal ~msg:"at end" int 5 (Edit_buffer.cursor buf);
  send_key input Input.Key.Right;
  equal ~msg:"still at end" int 5 (Edit_buffer.cursor buf)

let backspace_at_start_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let buf = Text_input.buffer input in
  ignore (Edit_buffer.move_home buf : bool);
  send_key input Input.Key.Backspace;
  equal ~msg:"unchanged" string "hello" (Text_input.value input)

let delete_at_end_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key input Input.Key.Delete;
  equal ~msg:"unchanged" string "hello" (Text_input.value input)

let operations_on_empty_input_are_safe () =
  let t, input = make_input () in
  focus_input t input;
  send_key input Input.Key.Backspace;
  send_key input Input.Key.Delete;
  send_key input Input.Key.Left;
  send_key input Input.Key.Right;
  send_key input Input.Key.Home;
  send_key input Input.Key.End;
  send_char_with_mod input ~modifier:ctrl_mod 'w';
  send_char_with_mod input ~modifier:ctrl_mod 'k';
  send_char_with_mod input ~modifier:ctrl_mod 'u';
  send_char_with_mod input ~modifier:ctrl_mod 'z';
  equal ~msg:"still empty" string "" (Text_input.value input)

(* ── prevent_default ── *)

let prevent_default_blocks_input () =
  let t, input = make_input () in
  focus_input t input;
  let text = String.make 1 'a' in
  let ev = Event.Key.of_input (Input.Key.of_char ~associated_text:text 'a') in
  Event.Key.prevent_default ev;
  Renderable.Private.emit_default_key (Text_input.node input) ev;
  equal ~msg:"value unchanged" string "" (Text_input.value input)

(* ── Pretty-printing ── *)

let pp_produces_non_empty_output () =
  let _t, input = make_input () in
  let s = Format.asprintf "%a" Text_input.pp input in
  is_true ~msg:"non-empty" (String.length s > 0)

let pp_contains_input_prefix () =
  let _t, input = make_input () in
  let s = Format.asprintf "%a" Text_input.pp input in
  is_true ~msg:"has Input prefix" (string_contains s "Input")

(* ── Runner ── *)

let () =
  run "mosaic.text-input"
    [
      group "Props"
        [
          test "default equals make()" props_default_equals_make;
          test "equal on identical" props_equal_identical;
          test "detects value diff" props_detects_value_diff;
          test "detects placeholder diff" props_detects_placeholder_diff;
          test "detects max_length diff" props_detects_max_length_diff;
          test "detects text_color diff" props_detects_text_color_diff;
          test "detects background_color diff"
            props_detects_background_color_diff;
          test "detects cursor_style diff" props_detects_cursor_style_diff;
          test "detects selection_fg diff" props_detects_selection_fg_diff;
          test "detects cursor_color diff" props_detects_cursor_color_diff;
          test "detects cursor_blinking diff" props_detects_cursor_blinking_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches_to_parent;
          test "node returns renderable" create_node_returns_renderable;
          test "buffer returns edit buffer" create_buffer_returns_edit_buffer;
          test "initial value matches" create_initial_value_matches;
          test "default value is empty" create_default_value_is_empty;
        ];
      group "Value"
        [
          test "returns current text" value_returns_current_text;
          test "set_value replaces text" set_value_replaces_text;
          test "set_value requests render" set_value_requests_render;
          test "set_value updates buffer" set_value_updates_buffer;
        ];
      group "Callbacks -- on_input"
        [
          test "fires on character insert" on_input_fires_on_char_insert;
          test "receives current value" on_input_receives_current_value;
          test "does not fire on cursor movement"
            on_input_does_not_fire_on_cursor_movement;
          test "set_on_input None disables" set_on_input_none_disables;
        ];
      group "Callbacks -- on_cursor"
        [
          test "fires on cursor movement" on_cursor_fires_on_cursor_movement;
          test "fires on selection change" on_cursor_fires_on_selection_change;
          test "set_on_cursor None disables" set_on_cursor_none_disables;
        ];
      group "Callbacks -- on_change"
        [
          test "fires on blur when changed" on_change_fires_on_blur_when_changed;
          test "does not fire on blur when unchanged"
            on_change_does_not_fire_on_blur_when_unchanged;
          test "fires on submit when changed"
            on_change_fires_on_submit_when_changed;
          test "set_on_change None disables" set_on_change_none_disables;
        ];
      group "Callbacks -- on_submit"
        [
          test "fires on Enter" on_submit_fires_on_enter;
          test "receives current value" on_submit_receives_current_value;
          test "set_on_submit None disables" set_on_submit_none_disables;
        ];
      group "Key handling"
        [
          test "character input inserts text" key_char_inserts_text;
          test "Left/Right moves cursor" key_left_right_moves_cursor;
          test "Home/End moves cursor" key_home_end_moves_cursor;
          test "Backspace deletes backward" key_backspace_deletes_backward;
          test "Delete key deletes forward" key_delete_deletes_forward;
          test "Ctrl+W deletes word backward" key_ctrl_w_deletes_word_backward;
          test "Ctrl+K deletes to end" key_ctrl_k_deletes_to_end;
          test "Ctrl+U deletes to start" key_ctrl_u_deletes_to_start;
          test "Enter fires submit" key_enter_fires_submit;
          test "key release is ignored" key_release_is_ignored;
          test "max_length rejects excess typing"
            key_max_length_rejects_excess_typing;
        ];
      group "Emacs keybindings"
        [
          test "Ctrl+A moves to start" ctrl_a_moves_to_start;
          test "Ctrl+A uppercase from parser moves to start"
            ctrl_a_uppercase_from_parser_moves_to_start;
          test "Ctrl+E moves to end" ctrl_e_moves_to_end;
          test "Ctrl+B moves left" ctrl_b_moves_left;
          test "Ctrl+F moves right" ctrl_f_moves_right;
          test "Ctrl+D deletes forward" ctrl_d_deletes_forward;
          test "Ctrl+Shift+D deletes line" ctrl_shift_d_deletes_line;
        ];
      group "Alt keybindings"
        [
          test "Alt+B moves word backward" alt_b_moves_word_backward;
          test "Alt+F moves word forward" alt_f_moves_word_forward;
          test "Alt+D deletes word forward" alt_d_deletes_word_forward;
          test "Alt+Backspace deletes word backward"
            alt_backspace_deletes_word_backward;
        ];
      group "Ctrl/Alt+Arrow word movement"
        [
          test "Ctrl+Left moves word backward" ctrl_left_moves_word_backward;
          test "Ctrl+Right moves word forward" ctrl_right_moves_word_forward;
        ];
      group "Super+Arrow keybindings"
        [
          test "Super+Left moves to start" super_left_moves_to_start;
          test "Super+Right moves to end" super_right_moves_to_end;
          test "Super+Up moves to start" super_up_moves_to_start;
          test "Super+Down moves to end" super_down_moves_to_end;
          test "Super+Shift+Left selects to start"
            super_shift_left_selects_to_start;
          test "Super+Shift+Right selects to end"
            super_shift_right_selects_to_end;
        ];
      group "Ctrl+-/. undo/redo"
        [
          test "Ctrl+- undoes" ctrl_minus_undoes;
          test "Ctrl+. redoes" ctrl_dot_redoes;
        ];
      group "Selection keybindings"
        [
          test "Shift+Right creates selection" shift_right_creates_selection;
          test "Shift+Left creates selection" shift_left_creates_selection;
          test "Shift+Home selects to start" shift_home_selects_to_start;
          test "Shift+End selects to end" shift_end_selects_to_end;
          test "Super+A selects all" super_a_selects_all;
          test "type with selection replaces" type_with_selection_replaces;
        ];
      group "Undo / Redo via keyboard"
        [
          test "Ctrl+Z undoes" ctrl_z_undoes;
          test "Ctrl+Shift+Z redoes" ctrl_shift_z_redoes;
          test "Super+Z undoes" super_z_undoes;
        ];
      group "Paste handling"
        [
          test "handle_paste inserts text" handle_paste_inserts_text;
          test "handle_paste strips newlines" handle_paste_strips_newlines;
          test "handle_paste strips ansi" handle_paste_strips_ansi;
          test "handle_paste with selection replaces"
            handle_paste_with_selection_replaces;
        ];
      group "Rendering"
        [ test "zero-size does not crash" render_zero_size_no_crash ];
      group "Cursor provider"
        [
          test "returns None when unfocused" cursor_returns_none_when_unfocused;
          test "returns Some when focused" cursor_returns_some_when_focused;
          test "style is Block by default" cursor_style_is_block_by_default;
          test "has correct color" cursor_has_correct_color;
          test "has correct blinking" cursor_has_correct_blinking;
        ];
      group "apply_props"
        [
          test "value change updates buffer"
            apply_props_value_change_updates_buffer;
          test "max_length change updates buffer"
            apply_props_max_length_change_updates_buffer;
          test "cursor change updates buffer"
            apply_props_cursor_change_updates_buffer;
          test "selection change updates buffer"
            apply_props_selection_change_updates_buffer;
          test "selection None clears buffer selection"
            apply_props_selection_none_clears_buffer_selection;
          test "schedules render" apply_props_schedules_render;
        ];
      group "Edge cases"
        [
          test "cursor Left at start is noop" cursor_left_at_start_is_noop;
          test "cursor Right at end is noop" cursor_right_at_end_is_noop;
          test "Backspace at start is noop" backspace_at_start_is_noop;
          test "Delete at end is noop" delete_at_end_is_noop;
          test "operations on empty input are safe"
            operations_on_empty_input_are_safe;
        ];
      group "prevent_default"
        [ test "prevent_default blocks input" prevent_default_blocks_input ];
      group "Pretty-printing"
        [
          test "non-empty output" pp_produces_non_empty_output;
          test "contains Input prefix" pp_contains_input_prefix;
        ];
    ]
