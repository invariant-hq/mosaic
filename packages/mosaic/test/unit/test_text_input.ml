open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

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

let make_props ?state ?value ?max_length ?cursor ?selection ?placeholder
    ?text_color ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor () =
  let state =
    match state with
    | Some state -> state
    | None -> editor ?value ?max_length ?cursor ?selection ()
  in
  Text_input.Props.make ~state ?placeholder ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ()

let property_builders = Hashtbl.create 32

let update_editor widget f =
  let props =
    Hashtbl.find property_builders (Renderable.id (Text_input.node widget))
  in
  Text_input.apply_props widget (props (f (Text_input.state widget)))

let create_editor ~parent ?style ?value ?max_length ?cursor ?selection
    ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?(accept = true) ?on_change ?on_submit () =
  let state = editor ?value ?max_length ?cursor ?selection () in
  let props state =
    Text_input.Props.make ~state ?placeholder ?text_color ?background_color
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
    Text_input.create ~parent ?style ~state ~on_change ?on_submit ?placeholder
      ?text_color ?background_color ?focused_text_color
      ?focused_background_color ?placeholder_color ?selection_color
      ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ?selectable
      ?show_cursor ()
  in
  (install := fun state -> Text_input.apply_props widget (props state));
  Hashtbl.replace property_builders
    (Renderable.id (Text_input.node widget))
    props;
  widget

let make_input ?value ?max_length ?cursor ?selection ?placeholder ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?accept ?on_change
    ?on_submit () =
  let t = make_ctx () in
  let root = make_root t in
  let widget =
    create_editor ~parent:root ?value ?max_length ?cursor ?selection
      ?placeholder ?text_color ?background_color ?focused_text_color
      ?focused_background_color ?placeholder_color ?selection_color
      ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ?selectable
      ?show_cursor ?accept ?on_change ?on_submit ()
  in
  (t, widget)

let render_input input ~width ~height =
  let node = Text_input.node input in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let grid = make_grid ~width ~height () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  grid

let no_mod = Matrix_input.Keystroke.no_mods

let send_key ?keypad input key =
  let ev = Event.Key.of_input (Matrix_input.Keystroke.make ?keypad key) in
  Renderable.Private.run_key_behavior (Text_input.node input) ev

let send_key_with_mod input ~modifier key =
  let ev =
    Event.Key.of_input (Matrix_input.Keystroke.make ~mods:modifier key)
  in
  Renderable.Private.run_key_behavior (Text_input.node input) ev

let send_input_event input data =
  let ev = Event.Key.of_input data in
  Renderable.Private.run_key_behavior (Text_input.node input) ev

let send_char input c =
  let text = String.make 1 c in
  let ev =
    Event.Key.of_input
      (Matrix_input.Keystroke.make ~text (Char (Uchar.of_char c)))
  in
  Renderable.Private.run_key_behavior (Text_input.node input) ev

let send_char_with_mod input ~modifier c =
  let ev =
    Event.Key.of_input
      (Matrix_input.Keystroke.make ~mods:modifier (Char (Uchar.of_char c)))
  in
  Renderable.Private.run_key_behavior (Text_input.node input) ev

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

let ctrl_mod = { no_mod with Matrix_input.Keystroke.ctrl = true }
let shift_mod = { no_mod with Matrix_input.Keystroke.shift = true }
let alt_mod = { no_mod with Matrix_input.Keystroke.alt = true }
let super_mod = { no_mod with Matrix_input.Keystroke.super = true }

let super_shift_mod =
  { no_mod with Matrix_input.Keystroke.super = true; shift = true }

(* ── Props ── *)

let props_default_equals_make () =
  let p = Text_input.Props.default in
  is_true ~msg:"default equals make()"
    (Text_input.Props.equal p (make_props ()))

let props_equal_identical () =
  let a = make_props () in
  let b = make_props () in
  is_true ~msg:"equal" (Text_input.Props.equal a b)

let props_detects_value_diff () =
  let a = make_props ~value:"hello" () in
  let b = make_props ~value:"world" () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_placeholder_diff () =
  let a = make_props ~placeholder:"Enter name" () in
  let b = make_props ~placeholder:"Enter email" () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_max_length_diff () =
  let a = make_props ~max_length:10 () in
  let b = make_props ~max_length:20 () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_text_color_diff () =
  let a = make_props ~text_color:Ansi.Color.red () in
  let b = make_props ~text_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_background_color_diff () =
  let a = make_props ~background_color:Ansi.Color.red () in
  let b = make_props ~background_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_style_diff () =
  let a = make_props ~cursor_style:`Block () in
  let b = make_props ~cursor_style:`Line () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_selection_fg_diff () =
  let a = make_props ~selection_fg:Ansi.Color.red () in
  let b = make_props ~selection_fg:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_color_diff () =
  let a = make_props ~cursor_color:Ansi.Color.red () in
  let b = make_props ~cursor_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Text_input.Props.equal a b)

let props_detects_cursor_blinking_diff () =
  let a = make_props ~cursor_blinking:true () in
  let b = make_props ~cursor_blinking:false () in
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
  satisfies ~claim:"node has a non-empty id" string
    (fun id -> String.length id > 0)
    (Renderable.id node)

let create_buffer_returns_edit_buffer () =
  let _t, input = make_input () in
  equal string "" (Editor.text (Text_input.state input))

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
  update_editor input (fun _ -> Editor.of_string "new");
  equal ~msg:"replaced" string "new" (Text_input.value input)

let set_value_requests_render () =
  let t, input = make_input () in
  let before = !(t.schedule_count) in
  update_editor input (fun _ -> Editor.of_string "something");
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_value_updates_buffer () =
  let _t, input = make_input ~value:"original" () in
  update_editor input (fun _ -> Editor.of_string "replaced");
  equal ~msg:"buffer updated" string "replaced"
    (Editor.text (Text_input.state input))

let changes_propose_complete_state () =
  let proposals = ref [] in
  let _, input =
    make_input ~value:"ab"
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  send_key input Matrix_input.Keystroke.Left;
  send_char input 'x';
  match !proposals with
  | [ edited; moved ] ->
      equal string "ab" (Editor.text moved);
      equal int 1 (Editor.cursor moved);
      equal string "axb" (Editor.text edited);
      equal int 2 (Editor.cursor edited)
  | _ -> fail "expected one complete proposal per state change"

let rejected_edits_leave_state_and_history_intact () =
  let proposals = ref [] in
  let _, input =
    make_input ~value:"ab" ~accept:false
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  let accepted = Text_input.state input in
  send_char input 'x';
  send_char input 'y';
  is_true (Text_input.state input == accepted);
  equal string "ab" (Text_input.value input);
  match !proposals with
  | [ latest; earlier ] ->
      equal string "aby" (Editor.text latest);
      equal string "abx" (Editor.text earlier);
      equal string "ab" (Editor.text (Editor.undo latest))
  | _ -> fail "expected independent proposals from accepted state"

let rejected_cursor_does_not_affect_next_edit () =
  let proposals = ref [] in
  let _, input =
    make_input ~value:"ab" ~accept:false
      ~on_change:(fun state -> proposals := state :: !proposals)
      ()
  in
  send_key input Matrix_input.Keystroke.Left;
  send_char input 'x';
  equal int 2 (Text_input.cursor input);
  equal string "abx" (Editor.text (List.hd !proposals))

let transformed_acceptance_is_next_event_base () =
  let widget = ref None in
  let on_change proposal =
    match !widget with
    | None -> fail "callback during construction"
    | Some widget ->
        update_editor widget (fun _ ->
            Editor.of_string (String.uppercase_ascii (Editor.text proposal)))
  in
  let _, input = make_input ~accept:false ~on_change () in
  widget := Some input;
  send_char input 'a';
  send_char input 'b';
  equal string "AB" (Text_input.value input);
  equal int 2 (Text_input.cursor input)

let blur_and_submit_do_not_propose_edits () =
  let count = ref 0 in
  let t, input = make_input ~on_change:(fun _ -> incr count) () in
  focus_input t input;
  blur_input input;
  equal int 0 !count

(* ── Callbacks — on_submit ── *)

let on_submit_fires_on_enter () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Enter;
  equal ~msg:"fired" int 1 !count

let on_submit_receives_current_value () =
  let fired = ref [] in
  let t, input =
    make_input ~value:"hello" ~on_submit:(fun s -> fired := s :: !fired) ()
  in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Enter;
  match !fired with
  | v :: _ -> equal ~msg:"received value" string "hello" v
  | [] -> fail "on_submit not fired"

let set_on_submit_none_disables () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  Text_input.set_on_submit input None;
  send_key input Matrix_input.Keystroke.Enter;
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
  (* Cursor starts at end (position 2) *)
  equal ~msg:"cursor at end" int 2 (Editor.cursor (Text_input.state input));
  send_key input Matrix_input.Keystroke.Left;
  equal ~msg:"cursor moved left" int 1 (Editor.cursor (Text_input.state input));
  send_key input Matrix_input.Keystroke.Right;
  equal ~msg:"cursor moved right" int 2 (Editor.cursor (Text_input.state input))

let key_home_end_moves_cursor () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Home;
  equal ~msg:"cursor at home" int 0 (Editor.cursor (Text_input.state input));
  send_key input Matrix_input.Keystroke.End;
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input))

let key_backspace_deletes_backward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Backspace;
  equal ~msg:"deleted" string "ab" (Text_input.value input)

let key_delete_deletes_forward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key input Matrix_input.Keystroke.Delete;
  equal ~msg:"deleted forward" string "bc" (Text_input.value input)

let key_ctrl_w_deletes_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'w';
  equal ~msg:"word deleted" string "hello " (Text_input.value input)

let key_ctrl_k_deletes_to_end () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  update_editor input Editor.move_home;
  for _ = 1 to 5 do
    update_editor input Editor.move_right
  done;
  send_char_with_mod input ~modifier:ctrl_mod 'k';
  equal ~msg:"deleted to end" string "hello" (Text_input.value input)

let key_ctrl_u_deletes_to_start () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'u';
  equal ~msg:"deleted to start" string "" (Text_input.value input)

let ctrl_j_fires_submit () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'j';
  equal ~msg:"a line feed submits" int 1 !count

let key_enter_fires_submit () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Enter;
  equal ~msg:"submitted" int 1 !count

let key_kp_enter_fires_submit () =
  let count = ref 0 in
  let t, input = make_input ~on_submit:(fun _ -> incr count) () in
  focus_input t input;
  send_key ~keypad:true input Matrix_input.Keystroke.Enter;
  equal ~msg:"submitted" int 1 !count

let key_kp_left_moves_cursor () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  send_key ~keypad:true input Matrix_input.Keystroke.Left;
  equal ~msg:"cursor moved left" int 2 (Text_input.cursor input)

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
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input));
  send_char_with_mod input ~modifier:ctrl_mod 'a';
  equal ~msg:"cursor at start" int 0 (Editor.cursor (Text_input.state input))

let ctrl_shift_a_selects_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input));
  (* Ctrl+Shift+a, its Shift consumed into the key: it selects. *)
  send_char_with_mod input ~modifier:ctrl_mod 'A';
  equal ~msg:"cursor at start" int 0 (Editor.cursor (Text_input.state input));
  is_true ~msg:"selection"
    (Option.is_some (Editor.selection (Text_input.state input)))

let ctrl_a_base_key_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  let data =
    Matrix_input.Keystroke.make ~mods:ctrl_mod ~base:(Uchar.of_char 'a')
      (Matrix_input.Keystroke.Char (Uchar.of_int 0x314a))
  in
  send_input_event input data;
  equal ~msg:"cursor at start" int 0 (Text_input.cursor input)

let ctrl_e_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_char_with_mod input ~modifier:ctrl_mod 'e';
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input))

let ctrl_b_moves_left () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'b';
  equal ~msg:"cursor moved left" int 2 (Editor.cursor (Text_input.state input))

let ctrl_f_moves_right () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_char_with_mod input ~modifier:ctrl_mod 'f';
  equal ~msg:"cursor moved right" int 1 (Editor.cursor (Text_input.state input))

let ctrl_d_deletes_forward () =
  let t, input = make_input ~value:"abc" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_char_with_mod input ~modifier:ctrl_mod 'd';
  equal ~msg:"deleted forward" string "bc" (Text_input.value input)

let ctrl_shift_d_deletes_line () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:ctrl_mod 'D';
  equal ~msg:"line deleted" string "" (Text_input.value input)

(* ── Alt Keybindings ── *)

let alt_b_moves_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:alt_mod 'b';
  equal ~msg:"moved to word boundary" int 6
    (Editor.cursor (Text_input.state input))

let meta_only_does_not_match_alt_binding () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  let meta_mod = { no_mod with Matrix_input.Keystroke.meta = true } in
  send_char_with_mod input ~modifier:meta_mod 'b';
  equal ~msg:"Meta alone does not match Alt" int 11 (Text_input.cursor input)

let alt_f_moves_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_char_with_mod input ~modifier:alt_mod 'f';
  equal ~msg:"moved to word end" int 6 (Editor.cursor (Text_input.state input))

let alt_d_deletes_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_char_with_mod input ~modifier:alt_mod 'd';
  equal ~msg:"word deleted forward" string "world" (Text_input.value input)

let alt_backspace_deletes_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_key_with_mod input ~modifier:alt_mod Matrix_input.Keystroke.Backspace;
  equal ~msg:"word deleted backward" string "hello " (Text_input.value input)

(* ── Ctrl/Alt+Arrow Word Movement ── *)

let ctrl_left_moves_word_backward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_key_with_mod input ~modifier:ctrl_mod Matrix_input.Keystroke.Left;
  equal ~msg:"moved to word boundary" int 6
    (Editor.cursor (Text_input.state input))

let ctrl_right_moves_word_forward () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:ctrl_mod Matrix_input.Keystroke.Right;
  equal ~msg:"moved to word end" int 6 (Editor.cursor (Text_input.state input))

(* ── Super+Arrow Keybindings ── *)

let super_left_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key_with_mod input ~modifier:super_mod Matrix_input.Keystroke.Left;
  equal ~msg:"cursor at start" int 0 (Editor.cursor (Text_input.state input))

let super_right_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:super_mod Matrix_input.Keystroke.Right;
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input))

let super_up_moves_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key_with_mod input ~modifier:super_mod Matrix_input.Keystroke.Up;
  equal ~msg:"cursor at start" int 0 (Editor.cursor (Text_input.state input))

let super_down_moves_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:super_mod Matrix_input.Keystroke.Down;
  equal ~msg:"cursor at end" int 5 (Editor.cursor (Text_input.state input))

let super_shift_left_selects_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key_with_mod input ~modifier:super_shift_mod Matrix_input.Keystroke.Left;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)));
  equal ~msg:"selected all" string "hello"
    (Editor.selected_text (Text_input.state input))

let super_shift_right_selects_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:super_shift_mod Matrix_input.Keystroke.Right;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)));
  equal ~msg:"selected all" string "hello"
    (Editor.selected_text (Text_input.state input))

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
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:shift_mod Matrix_input.Keystroke.Right;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)))

let shift_left_creates_selection () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key_with_mod input ~modifier:shift_mod Matrix_input.Keystroke.Left;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)))

let shift_home_selects_to_start () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key_with_mod input ~modifier:shift_mod Matrix_input.Keystroke.Home;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)));
  equal ~msg:"selected all" string "hello"
    (Editor.selected_text (Text_input.state input))

let shift_end_selects_to_end () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key_with_mod input ~modifier:shift_mod Matrix_input.Keystroke.End;
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)));
  equal ~msg:"selected all" string "hello"
    (Editor.selected_text (Text_input.state input))

let super_a_selects_all () =
  let t, input = make_input ~value:"hello world" () in
  focus_input t input;
  send_char_with_mod input ~modifier:super_mod 'a';
  is_true ~msg:"has selection"
    (Option.is_some (Editor.selection (Text_input.state input)));
  equal ~msg:"all selected" string "hello world"
    (Editor.selected_text (Text_input.state input))

let type_with_selection_replaces () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.select_all;
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
  send_char_with_mod input ~modifier:ctrl_mod 'Z';
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
  update_editor input Editor.select_all;
  Text_input.handle_paste input "new";
  equal ~msg:"replaced" string "new" (Text_input.value input)

(* ── Rendering ── *)

let render_zero_size_no_crash () =
  let _t, input = make_input ~value:"hello" () in
  let node = Text_input.node input in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  let grid = make_grid ~width:10 ~height:10 () in
  Renderable.Private.render node grid ~dt:0.;
  is_true ~msg:"render completed" true

(* ── Cursor Provider ── *)

let cursor_returns_none_when_unfocused () =
  let _t, input = make_input ~value:"hi" () in
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  let node = Text_input.node input in
  let cursor = Renderable.cursor node in
  is_none ~msg:"no cursor when unfocused" cursor

let cursor_returns_some_when_focused () =
  let t, input = make_input ~value:"hi" () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  let node = Text_input.node input in
  let cursor = Renderable.cursor node in
  is_some ~msg:"cursor when focused" cursor

let cursor_returns_none_when_hidden () =
  let t, input = make_input ~value:"hi" ~show_cursor:false () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  is_none ~msg:"cursor hidden" (Renderable.cursor (Text_input.node input))

let cursor_style_is_block_by_default () =
  let t, input = make_input ~value:"hi" () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c -> is_true ~msg:"block style" (c.style = `Block)
  | None -> fail "expected cursor"

let cursor_has_correct_color () =
  let t, input = make_input ~value:"hi" ~cursor_color:Ansi.Color.red () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c ->
      is_true ~msg:"cursor color is red"
        (Ansi.Color.equal c.color Ansi.Color.red)
  | None -> fail "expected cursor"

let cursor_has_correct_blinking () =
  let t, input = make_input ~value:"hi" ~cursor_blinking:false () in
  focus_input t input;
  ignore (render_input input ~width:20 ~height:1 : Matrix_grid.t);
  let node = Text_input.node input in
  match Renderable.cursor node with
  | Some c -> is_false ~msg:"not blinking" c.blinking
  | None -> fail "expected cursor"

(* ── apply_props ── *)

let apply_props_value_change_updates_buffer () =
  let _t, input = make_input ~value:"old" () in
  let props = make_props ~value:"new" () in
  Text_input.apply_props input props;
  equal ~msg:"value updated" string "new" (Text_input.value input)

let apply_props_max_length_change_updates_buffer () =
  let _t, input = make_input ~value:"hello world" () in
  let props = make_props ~value:"hello world" ~max_length:5 () in
  Text_input.apply_props input props;
  equal ~msg:"accepted editor is truncated" string "hello"
    (Text_input.value input)

let apply_props_truncated_value_preserves_cursor () =
  let _t, input = make_input ~value:"abc" ~max_length:3 () in
  update_editor input (Editor.set_cursor 1);
  Text_input.apply_props input (make_props ~value:"abcdef" ~max_length:3 ());
  equal ~msg:"value remains normalized" string "abc" (Text_input.value input);
  equal ~msg:"replacement owns its cursor" int 3 (Text_input.cursor input)

let apply_props_cursor_change_updates_buffer () =
  let _t, input = make_input ~value:"hello" () in
  let props = make_props ~value:"hello" ~cursor:2 () in
  Text_input.apply_props input props;
  equal ~msg:"cursor updated" int 2 (Text_input.cursor input)

let apply_props_selection_change_updates_buffer () =
  let _t, input = make_input ~value:"hello" () in
  let props = make_props ~value:"hello" ~selection:(Some (1, 4)) () in
  Text_input.apply_props input props;
  equal ~msg:"selection updated"
    (option (pair int int))
    (Some (1, 4))
    (Text_input.selection input)

let apply_props_selection_none_clears_buffer_selection () =
  let _t, input = make_input ~value:"hello" ~selection:(Some (1, 4)) () in
  Text_input.apply_props input (make_props ~value:"hello" ~selection:None ());
  is_none ~msg:"selection cleared" (Text_input.selection input)

let apply_props_schedules_render () =
  let t, input = make_input () in
  let before = !(t.schedule_count) in
  Text_input.apply_props input Text_input.Props.default;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Edge Cases ── *)

let cursor_left_at_start_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  equal ~msg:"at start" int 0 (Editor.cursor (Text_input.state input));
  send_key input Matrix_input.Keystroke.Left;
  equal ~msg:"still at start" int 0 (Editor.cursor (Text_input.state input))

let cursor_right_at_end_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  equal ~msg:"at end" int 5 (Editor.cursor (Text_input.state input));
  send_key input Matrix_input.Keystroke.Right;
  equal ~msg:"still at end" int 5 (Editor.cursor (Text_input.state input))

let backspace_at_start_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  update_editor input Editor.move_home;
  send_key input Matrix_input.Keystroke.Backspace;
  equal ~msg:"unchanged" string "hello" (Text_input.value input)

let delete_at_end_is_noop () =
  let t, input = make_input ~value:"hello" () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Delete;
  equal ~msg:"unchanged" string "hello" (Text_input.value input)

let operations_on_empty_input_are_safe () =
  let t, input = make_input () in
  focus_input t input;
  send_key input Matrix_input.Keystroke.Backspace;
  send_key input Matrix_input.Keystroke.Delete;
  send_key input Matrix_input.Keystroke.Left;
  send_key input Matrix_input.Keystroke.Right;
  send_key input Matrix_input.Keystroke.Home;
  send_key input Matrix_input.Keystroke.End;
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
  let ev =
    Event.Key.of_input
      (Matrix_input.Keystroke.make ~text (Char (Uchar.of_char 'a')))
  in
  Event.Key.prevent_default ev;
  Renderable.Private.run_key_behavior (Text_input.node input) ev;
  equal ~msg:"value unchanged" string "" (Text_input.value input)

(* ── Pretty-printing ── *)

let pp_produces_non_empty_output () =
  let _t, input = make_input () in
  let s = Format.asprintf "%a" Text_input.pp input in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

let pp_contains_input_prefix () =
  let _t, input = make_input () in
  let s = Format.asprintf "%a" Text_input.pp input in
  is_true ~msg:"has Input prefix" (string_contains s "Input")

(* ── Runner ── *)

let invalid_declarations_are_rejected () =
  let t = make_ctx () in
  let root = make_root t in
  List.iter
    (fun text ->
      let state = Editor.of_string text in
      let invalid = function
        | Invalid_argument message ->
            message = "Mosaic_ui.Text_input: state must not contain CR or LF"
        | _ -> false
      in
      raises_match ~msg:"property declaration rejects line breaks" invalid
        (fun () -> ignore (Text_input.Props.make ~state ()));
      raises_match ~msg:"construction rejects line breaks" invalid (fun () ->
          ignore (Text_input.create ~parent:root ~state ~on_change:ignore ())))
    [ "a\nb"; "a\rb"; "a\r\nb"; "a\n"; "\r" ];
  equal ~msg:"invalid construction attaches no child" int 0
    (List.length (Renderable.children root))

let invalid_replacement_preserves_accepted_state () =
  let t, input = make_input ~value:"valid" ~cursor:2 () in
  let accepted = Text_input.state input in
  let before = Matrix_grid.to_text (render_input input ~width:10 ~height:1) in
  raises_match ~msg:"invalid replacement is rejected"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      Text_input.apply_props input
        (Text_input.Props.make ~state:(Editor.of_string "bad\rvalue") ()));
  is_true ~msg:"accepted editor retained" (Text_input.state input == accepted);
  equal ~msg:"caret retained" int 2 (Text_input.cursor input);
  equal ~msg:"rendered text retained" string before
    (Matrix_grid.to_text (render_input input ~width:10 ~height:1));
  focus_input t input;
  send_char input 'x';
  equal ~msg:"next edit uses retained accepted state" string "vaxlid"
    (Text_input.value input)

let cursor_declarations_share_validated_source () =
  let text = String.make 4096 'x' in
  let t, input = make_input ~value:text () in
  let source = Editor.source (Text_input.state input) in
  let next = Editor.set_cursor 17 (Text_input.state input) in
  Text_input.apply_props input (Text_input.Props.make ~state:next ());
  is_true ~msg:"caret declaration shares indexed text"
    (Editor.source (Text_input.state input) == source);
  equal ~msg:"declared caret accepted" int 17 (Text_input.cursor input);
  focus_input t input;
  Text_input.handle_paste input "\027[31mA\r\nB\rC\n\027[0m";
  equal ~msg:"paste remains normalized and accepted" string
    (String.sub text 0 17 ^ "ABC" ^ String.sub text 17 (String.length text - 17))
    (Text_input.value input);
  is_false ~msg:"paste proposal contains no CR"
    (String.contains (Text_input.value input) '\r');
  is_false ~msg:"paste proposal contains no LF"
    (String.contains (Text_input.value input) '\n')

let multiline_history_is_ignored_by_single_line_input () =
  List.iter
    (fun (state, action) ->
      let proposals = ref 0 in
      let t, input =
        make_input
          ~on_change:(fun state ->
            ignore state;
            incr proposals)
          ()
      in
      Text_input.apply_props input (Text_input.Props.make ~state ());
      focus_input t input;
      send_char_with_mod input ~modifier:ctrl_mod action;
      equal ~msg:"multiline history emits no proposal" int 0 !proposals;
      is_true ~msg:"accepted editor and history are unchanged"
        (Text_input.state input == state);
      equal ~msg:"scratch text restored" string "" (Text_input.value input);
      equal ~msg:"scratch caret restored" int 0 (Text_input.cursor input);
      let restored = ref state in
      let textarea =
        Textarea.create ~parent:(make_root t) ~state
          ~on_change:(fun next -> restored := next)
          ()
      in
      let event =
        Event.Key.of_input
          (Matrix_input.Keystroke.make ~mods:ctrl_mod
             (Char (Uchar.of_char action)))
      in
      Renderable.Private.run_key_behavior (Textarea.node textarea) event;
      equal ~msg:"textarea can still restore the shared multiline history"
        string "a\nb" (Editor.text !restored);
      send_char input 'x';
      equal ~msg:"subsequent valid edit is accepted" string "x"
        (Text_input.value input);
      equal ~msg:"only the valid edit emits a proposal" int 1 !proposals)
    [
      (Editor.clear (Editor.of_string "a\nb"), '-');
      (Editor.undo (Editor.insert "a\nb" Editor.empty), '.');
    ]

let typed_bindings_override_defaults_and_recompile () =
  let state = editor ~value:"abc" ~cursor:1 () in
  let proposed = ref state in
  let parent = make_root (make_ctx ()) in
  let bindings =
    [
      (Shortcut.left, Text_input.Buffer_home);
      (Shortcut.key Matrix_input.Keystroke.Left, Text_input.Buffer_end);
      (Shortcut.ctrl 'a', Text_input.Buffer_end);
      (Shortcut.char ~meta:true 'b', Text_input.Buffer_home);
    ]
  in
  let input =
    Text_input.create ~parent ~state ~key_bindings:bindings
      ~on_change:(fun state -> proposed := state)
      ()
  in
  send_key input Matrix_input.Keystroke.Left;
  equal ~msg:"last equivalent custom shortcut overrides default" int 3
    (Editor.cursor !proposed);
  send_input_event input
    (Matrix_input.Keystroke.make ~mods:ctrl_mod ~base:(Uchar.of_char 'a')
       (Matrix_input.Keystroke.Char (Uchar.of_int 0x0444)));
  equal ~msg:"custom shortcuts use the base-layout fallback" int 3
    (Editor.cursor !proposed);
  send_char_with_mod input ~modifier:{ no_mod with meta = true } 'b';
  equal ~msg:"explicit Meta shortcut remains distinct from Alt" int 0
    (Editor.cursor !proposed);
  send_input_event input
    (Matrix_input.Keystroke.make ~repeat:true Matrix_input.Keystroke.Left);
  equal ~msg:"repeat runs the custom shortcut" int 3 (Editor.cursor !proposed);
  proposed := state;
  send_key_with_mod input
    ~modifier:{ no_mod with hyper = true }
    Matrix_input.Keystroke.Left;
  is_true ~msg:"extra non-lock modifier does not match" (!proposed == state);
  Text_input.apply_props input (Text_input.Props.make ~state ());
  send_key input Matrix_input.Keystroke.Left;
  equal ~msg:"removing custom bindings restores defaults" int 0
    (Editor.cursor !proposed)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.input}) or to
   {!Text_input.create}, reaches the live input ({!Test_harness.reaches}).
   The caret is observed as the node reports it, and proposals as
   [on_change] receives them. *)

let doors_size = Mosaic.size ~width:10 ~height:1
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let abc = Editor.of_string "abc"
let abc_selected = Editor.select ~anchor:0 ~cursor:2 abc
let proposals = ref []

let propose e =
  proposals :=
    Printf.sprintf "%S@%d" (Editor.text e) (Editor.cursor e) :: !proposals

let proposed _ _ = String.concat ";" (List.rev !proposals)

let caret _ node =
  Format.asprintf "%a"
    (Format.pp_print_option Renderable.pp_cursor)
    (Renderable.cursor node)

let press_x renderer node =
  proposals := [];
  Test_harness.focus renderer node;
  ignore (Test_harness.dispatch_key renderer (char_key 'x') : Event.key)

let drag renderer node =
  proposals := [];
  Test_harness.focus renderer node;
  Test_harness.select_text renderer node

let doors_state () =
  Test_harness.reaches "state"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:(Editor.of_string "xyz")
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style
           ~state:(Editor.of_string "xyz") ~on_change:propose ()))

let doors_placeholder () =
  Test_harness.reaches "placeholder"
    ~base:
      (Mosaic.input ~size:doors_size ~state:Editor.empty ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:Editor.empty
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~on_change:propose ()))

let doors_text_color () =
  Test_harness.reaches "text_color"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~text_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~text_color:doors_red ~on_change:propose ()))

let doors_background_color () =
  Test_harness.reaches "background_color"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~background_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~background_color:doors_red ~on_change:propose ()))

let doors_focused_text_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_text_color"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~focused_text_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~focused_text_color:doors_red ~on_change:propose ()))

let doors_focused_background_color () =
  Test_harness.reaches ~act:Test_harness.focus "focused_background_color"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc
         ~focused_background_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~focused_background_color:doors_red ~on_change:propose ()))

let doors_placeholder_color () =
  Test_harness.reaches "placeholder_color"
    ~base:
      (Mosaic.input ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:Editor.empty ~placeholder:"type"
         ~placeholder_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:Editor.empty
           ~placeholder:"type" ~placeholder_color:doors_red ~on_change:propose
           ()))

let doors_selection_color () =
  Test_harness.reaches "selection_color"
    ~base:
      (Mosaic.input ~size:doors_size ~state:abc_selected ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc_selected
         ~selection_color:doors_red ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc_selected
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc_selected
           ~selection_color:doors_red ~on_change:propose ()))

let doors_selection_fg () =
  Test_harness.reaches "selection_fg"
    ~base:
      (Mosaic.input ~size:doors_size ~state:abc_selected ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc_selected ~selection_fg:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc_selected
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc_selected
           ~selection_fg:doors_red ~on_change:propose ()))

let doors_cursor_style () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_style"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~cursor_style:`Line
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~cursor_style:`Line ~on_change:propose ()))

let doors_cursor_color () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_color"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~cursor_color:doors_red
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~cursor_color:doors_red ~on_change:propose ()))

let doors_cursor_blinking () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "cursor_blinking"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~cursor_blinking:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~cursor_blinking:false ~on_change:propose ()))

let doors_selectable () =
  Test_harness.reaches ~act:drag ~observe:proposed "selectable"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~selectable:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~selectable:false ~on_change:propose ()))

let doors_show_cursor () =
  Test_harness.reaches ~act:Test_harness.focus ~observe:caret "show_cursor"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc ~show_cursor:false
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~show_cursor:false ~on_change:propose ()))

let doors_key_bindings () =
  Test_harness.reaches ~act:press_x ~observe:proposed "key_bindings"
    ~base:(Mosaic.input ~size:doors_size ~state:abc ~on_change:propose ())
    ~element:
      (Mosaic.input ~size:doors_size ~state:abc
         ~key_bindings:[ (Shortcut.char 'x', Text_input.Move_left) ]
         ~on_change:propose ())
    ~create_base:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~on_change:propose ()))
    ~create:(fun parent ->
      Text_input.node
        (Text_input.create ~parent ~style:doors_style ~state:abc
           ~key_bindings:[ (Shortcut.char 'x', Text_input.Move_left) ]
           ~on_change:propose ()))

let () =
  run "mosaic.text-input"
    [
      test "typed bindings override defaults and recompile"
        typed_bindings_override_defaults_and_recompile;
      group "Single-line declarations"
        [
          test "multiline history is ignored by single-line input"
            multiline_history_is_ignored_by_single_line_input;
          test "reject invalid declarations" invalid_declarations_are_rejected;
          test "invalid replacement preserves accepted state"
            invalid_replacement_preserves_accepted_state;
          test "cursor declarations share source and paste normalizes"
            cursor_declarations_share_validated_source;
        ];
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
      group "State proposals"
        [
          test "complete state" changes_propose_complete_state;
          test "rejected edits preserve accepted state"
            rejected_edits_leave_state_and_history_intact;
          test "rejected caret does not affect next edit"
            rejected_cursor_does_not_affect_next_edit;
          test "transformed acceptance is next event base"
            transformed_acceptance_is_next_event_base;
          test "blur does not propose" blur_and_submit_do_not_propose_edits;
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
          test "Ctrl+J fires submit" ctrl_j_fires_submit;
          test "keypad Enter fires submit" key_kp_enter_fires_submit;
          test "keypad Left moves cursor" key_kp_left_moves_cursor;
          test "max_length rejects excess typing"
            key_max_length_rejects_excess_typing;
        ];
      group "Emacs keybindings"
        [
          test "Ctrl+A moves to start" ctrl_a_moves_to_start;
          test "Ctrl+Shift+A selects to start" ctrl_shift_a_selects_to_start;
          test "Ctrl+A base key moves to start" ctrl_a_base_key_moves_to_start;
          test "Ctrl+E moves to end" ctrl_e_moves_to_end;
          test "Ctrl+B moves left" ctrl_b_moves_left;
          test "Ctrl+F moves right" ctrl_f_moves_right;
          test "Ctrl+D deletes forward" ctrl_d_deletes_forward;
          test "Ctrl+Shift+D deletes line" ctrl_shift_d_deletes_line;
        ];
      group "Alt keybindings"
        [
          test "Alt+B moves word backward" alt_b_moves_word_backward;
          test "Meta alone does not match Alt"
            meta_only_does_not_match_alt_binding;
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
          test "returns None when hidden" cursor_returns_none_when_hidden;
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
          test "truncated value preserves cursor"
            apply_props_truncated_value_preserves_cursor;
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
      group "Both doors"
        [
          test "state" doors_state;
          test "placeholder" doors_placeholder;
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
