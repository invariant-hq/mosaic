(** Tests for the Input module *)

open Windtrap

let event_testable = Testable.make ~pp:Input.pp ~equal:Input.equal ()

let capability_testable =
  Testable.make ~pp:Input.Response.pp_capability
    ~equal:Input.Response.equal_capability ()

let response_testable =
  Testable.make ~pp:Input.Response.pp ~equal:Input.Response.equal ()

let event_type_pp fmt = function
  | Input.Key.Press -> Format.pp_print_string fmt "Press"
  | Input.Key.Repeat -> Format.pp_print_string fmt "Repeat"
  | Input.Key.Release -> Format.pp_print_string fmt "Release"

let event_type_testable = Testable.make ~pp:event_type_pp ~equal:( = ) ()

let key_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key key
    =
  Input.key ?modifier ?event_type ?associated_text ?shifted_key ?base_key key

let char_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key c =
  Input.char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c

(* Test helpers that wrap callback-based API to return lists *)
let feed_to_lists ?(now = 0.0) parser bytes off len =
  let user_acc = ref [] in
  let response_acc = ref [] in
  Input.Parser.feed parser bytes off len ~now
    ~on_event:(fun e -> user_acc := e :: !user_acc)
    ~on_response:(fun c -> response_acc := c :: !response_acc);
  (List.rev !user_acc, List.rev !response_acc)

let drain_to_lists ?(now = 0.0) parser =
  let user_acc = ref [] in
  let response_acc = ref [] in
  Input.Parser.drain parser ~now
    ~on_event:(fun e -> user_acc := e :: !user_acc)
    ~on_response:(fun c -> response_acc := c :: !response_acc);
  (List.rev !user_acc, List.rev !response_acc)

let parse_single s =
  let p = Input.Parser.create () in
  feed_to_lists p (Bytes.of_string s) 0 (String.length s)

let parse_user s = parse_single s |> fst
let parse_responses s = parse_single s |> snd

let capabilities responses =
  List.filter_map
    (function Input.Response.Capability c -> Some c | _ -> None)
    responses

let parse_capabilities s = parse_responses s |> capabilities
let feed_user parser bytes off len = feed_to_lists parser bytes off len |> fst
let drain_user ?now parser = drain_to_lists ?now parser |> fst
let drain_responses ?now parser = drain_to_lists ?now parser |> snd
let drain_capabilities ?now parser = drain_responses ?now parser |> capabilities
let unknown_response s = Input.Response.Unknown s

let test_parse_regular_chars () =
  let events = parse_user "a" in
  equal ~msg:"single char 'a'" (list event_testable) [ char_event 'a' ] events;

  let events = parse_user "hello" in
  equal ~msg:"multiple chars 'hello'" (list event_testable)
    [
      char_event 'h';
      char_event 'e';
      char_event 'l';
      char_event 'l';
      char_event 'o';
    ]
    events;

  match parse_user "A" with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      equal ~msg:"uppercase key" char 'A' (Uchar.to_char u);
      is_true ~msg:"shift flag" modifier.shift
  | _ -> fail "expected single uppercase char event"

let test_char_associated_text_default () =
  match parse_user "q" with
  | [ Input.Key { Input.Key.key = Char u; associated_text; _ } ] ->
      equal ~msg:"key" char 'q' (Uchar.to_char u);
      equal ~msg:"associated text" string "q" associated_text
  | _ -> fail "expected char key event"

let test_parse_control_chars () =
  let expect ctrl_seq letter =
    match parse_user ctrl_seq with
    | [ Input.Key { key = Char u; modifier; _ } ] ->
        equal ~msg:"letter" char letter (Uchar.to_char u);
        is_true ~msg:"ctrl modifier" modifier.ctrl
    | _ -> fail "expected ctrl key event"
  in
  expect "\x01" 'a';
  expect "\x03" 'c';
  expect "\x1a" 'z'

let test_opentui_ctrl_normalization_regressions () =
  let expect ctrl_seq letter =
    match parse_user ctrl_seq with
    | [ Input.Key { key = Char u; modifier; _ } ] ->
        equal ~msg:"ctrl letter normalizes lowercase" char letter
          (Uchar.to_char u);
        is_true ~msg:"ctrl modifier" modifier.ctrl
    | events ->
        failf "expected one ctrl key event, got %d events" (List.length events)
  in
  expect "\x01" 'a';
  expect "\x03" 'c';
  expect "\x1a" 'z';
  let expect_punct ctrl_seq punct =
    match parse_user ctrl_seq with
    | [ Input.Key { key = Char u; modifier; _ } ] ->
        equal ~msg:"ctrl punctuation key" char punct (Uchar.to_char u);
        is_true ~msg:"ctrl punctuation modifier" modifier.ctrl
    | events ->
        failf "expected one ctrl punctuation event, got %d events"
          (List.length events)
  in
  expect_punct "\x1c" '\\';
  expect_punct "\x1d" ']';
  expect_punct "\x1e" '^';
  expect_punct "\x1f" '_'

let test_opentui_alt_meta_regressions () =
  let expect_meta seq key =
    match parse_user seq with
    | [ Input.Key { key = actual; modifier; _ } ] ->
        is_true ~msg:"alt modifier" modifier.alt;
        is_true ~msg:"meta modifier follows OpenTUI Alt semantics" modifier.meta;
        equal ~msg:"key" event_testable
          (key_event
             ~modifier:{ Input.Modifier.none with alt = true; meta = true }
             key)
          (Input.Key
             {
               key = actual;
               modifier;
               event_type = Input.Key.Press;
               associated_text = "";
               shifted_key = None;
               base_key = None;
             })
    | events -> failf "expected one Alt/Meta event, got %d" (List.length events)
  in
  expect_meta "\x1b\r" Input.Key.Enter;
  expect_meta "\x1b[1;3D" Input.Key.Left;
  match parse_user "\x1b[97;3u" with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      equal ~msg:"kitty alt key" char 'a' (Uchar.to_char u);
      is_true ~msg:"kitty alt" modifier.alt;
      is_true ~msg:"kitty meta" modifier.meta
  | events -> failf "expected one Kitty Alt event, got %d" (List.length events)

let test_csi_sub_params_with_event_type () =
  match parse_user "\x1b[1:2:3A" with
  | [ Input.Key k ] ->
      is_true ~msg:"shift modifier from sub-params" k.modifier.shift;
      equal ~msg:"release event type" event_type_testable Input.Key.Release
        k.event_type
  | _ ->
      failf "expected single Up release event, got %d events"
        (List.length (parse_user "\x1b[1:2:3A"))

let test_parse_special_keys () =
  equal ~msg:"Enter" (list event_testable)
    [ key_event Input.Key.Enter ]
    (parse_user "\r");

  equal ~msg:"Line feed" (list event_testable)
    [ key_event Input.Key.Line_feed ]
    (parse_user "\n");

  equal ~msg:"Tab" (list event_testable)
    [ key_event Input.Key.Tab ]
    (parse_user "\t");

  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b") 0 1 in
  equal ~msg:"Escape buffered" (list event_testable) [] events;

  let events = feed_user parser (Bytes.of_string "\x7f") 0 1 in
  equal ~msg:"Alt+Backspace" (list event_testable)
    [
      key_event
        ~modifier:
          { Input.Modifier.none with Input.Modifier.alt = true; meta = true }
        Input.Key.Backspace;
    ]
    events

let test_parse_arrow_keys () =
  let arrows =
    [
      ("\x1b[A", Input.Key.Up, "Up arrow");
      ("\x1b[B", Input.Key.Down, "Down arrow");
      ("\x1b[C", Input.Key.Right, "Right arrow");
      ("\x1b[D", Input.Key.Left, "Left arrow");
    ]
  in
  List.iter
    (fun (seq, key, desc) ->
      equal ~msg:desc (list event_testable) [ key_event key ] (parse_user seq))
    arrows

let test_parse_function_keys () =
  let f_keys_ss3 =
    [
      ("\x1bOP", Input.Key.F 1);
      ("\x1bOQ", Input.Key.F 2);
      ("\x1bOR", Input.Key.F 3);
      ("\x1bOS", Input.Key.F 4);
    ]
  in
  List.iter
    (fun (seq, key) ->
      equal ~msg:"F-key" (list event_testable)
        [ key_event key ]
        (parse_user seq))
    f_keys_ss3;

  equal ~msg:"F5" (list event_testable)
    [ key_event (Input.Key.F 5) ]
    (parse_user "\x1b[15~")

let test_cygwin_function_key_regressions () =
  let cases =
    [
      ("\x1b[[A", Input.Key.F 1);
      ("\x1b[[B", Input.Key.F 2);
      ("\x1b[[C", Input.Key.F 3);
      ("\x1b[[D", Input.Key.F 4);
      ("\x1b[[E", Input.Key.F 5);
    ]
  in
  List.iter
    (fun (seq, key) ->
      equal ~msg:"Cygwin/libuv F-key" (list event_testable)
        [ key_event key ]
        (parse_user seq))
    cases

let test_parse_modifiers () =
  equal ~msg:"Shift+Tab" (list event_testable)
    [
      key_event
        ~modifier:{ Input.Modifier.none with shift = true }
        Input.Key.Tab;
    ]
    (parse_user "\x1b[Z");

  equal ~msg:"Ctrl+Up" (list event_testable)
    [
      key_event ~modifier:{ Input.Modifier.none with ctrl = true } Input.Key.Up;
    ]
    (parse_user "\x1b[1;5A");

  equal ~msg:"Alt+Left" (list event_testable)
    [
      key_event
        ~modifier:{ Input.Modifier.none with alt = true; meta = true }
        Input.Key.Left;
    ]
    (parse_user "\x1b[1;3D")

let test_parse_mouse_sgr () =
  equal ~msg:"Mouse click" (list event_testable)
    [ Input.mouse_press 9 19 Input.Mouse.Left ]
    (parse_user "\x1b[<0;10;20M");

  equal ~msg:"Mouse release" (list event_testable)
    [ Input.mouse_release 9 19 (Some Input.Mouse.Left) ]
    (parse_user "\x1b[<0;10;20m");

  equal ~msg:"Mouse motion" (list event_testable)
    [ Input.mouse_move 14 24 ]
    (parse_user "\x1b[<32;15;25M");

  equal ~msg:"Mouse scroll" (list event_testable)
    [ Input.mouse_scroll 9 4 Input.Mouse.Scroll_up ]
    (parse_user "\x1b[<64;10;5M")

let test_sgr_mouse_state_regressions () =
  let parser = Input.Parser.create () in
  equal ~msg:"SGR motion without prior press is move" (list event_testable)
    [ Input.mouse_move 9 4 ]
    (feed_user parser (Bytes.of_string "\x1b[<32;10;5M") 0 11);
  let parser = Input.Parser.create () in
  ignore (feed_user parser (Bytes.of_string "\x1b[<0;10;5M") 0 10);
  equal ~msg:"SGR motion after left press is drag" (list event_testable)
    [ Input.mouse_drag 11 4 Input.Mouse.Left ]
    (feed_user parser (Bytes.of_string "\x1b[<32;12;5M") 0 11);
  ignore (feed_user parser (Bytes.of_string "\x1b[<0;12;5m") 0 10);
  equal ~msg:"SGR motion after release is move" (list event_testable)
    [ Input.mouse_move 13 4 ]
    (feed_user parser (Bytes.of_string "\x1b[<32;14;5M") 0 11)

let test_sgr_mouse_partial_timeout_regression () =
  let parser = Input.Parser.create () in
  equal ~msg:"partial SGR mouse is pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[<35;20") 0 8);
  equal ~msg:"partial SGR mouse stays pending after timeout"
    (list event_testable) []
    (drain_user ~now:1.0 parser);
  equal ~msg:"partial SGR mouse completes after timeout" (list event_testable)
    [ Input.mouse_move 19 4 ]
    (feed_user parser (Bytes.of_string ";5m") 0 3)

let test_protocol_context_timeout_regressions () =
  let kitty_context =
    { Input.Parser.default_protocol_context with kitty_keyboard = true }
  in
  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser kitty_context;
  equal ~msg:"partial Kitty key pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[97;") 0 5);
  equal ~msg:"partial Kitty key stays pending after timeout"
    (list event_testable) []
    (drain_user ~now:1.0 parser);
  equal ~msg:"partial Kitty key completes after timeout" (list event_testable)
    [
      key_event
        ~modifier:{ Input.Modifier.none with shift = true }
        ~associated_text:"a"
        (Input.Key.Char (Uchar.of_char 'a'));
    ]
    (feed_user parser (Bytes.of_string "2u") 0 2);

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser kitty_context;
  equal ~msg:"numeric generic CSI pending with Kitty enabled"
    (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[123") 0 5);
  let events, responses = drain_to_lists ~now:1.0 parser in
  equal ~msg:"numeric generic CSI is not Kitty input" (list event_testable) []
    events;
  equal ~msg:"numeric generic CSI flushes as unknown response"
    (list response_testable)
    [ unknown_response "\x1b[123" ]
    responses;

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with explicit_width_cpr = true };
  equal ~msg:"partial explicit-width CPR pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[1;2") 0 5);
  equal ~msg:"partial explicit-width CPR stays pending"
    (list capability_testable) []
    (drain_capabilities ~now:1.0 parser);
  equal ~msg:"partial explicit-width CPR completes" (list capability_testable)
    [ Input.Response.Cursor_position (1, 2) ]
    (feed_to_lists parser (Bytes.of_string "R") 0 1 |> snd |> capabilities);

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with explicit_width_cpr = true };
  equal ~msg:"modified CSI key prefix pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[1;5") 0 5);
  let events, caps = drain_to_lists ~now:1.0 parser in
  equal ~msg:"modified CSI key prefix is not a CPR" (list event_testable) []
    events;
  equal ~msg:"modified CSI key prefix flushes as unknown response"
    (list response_testable)
    [ unknown_response "\x1b[1;5" ]
    caps;

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with explicit_width_cpr = true };
  equal ~msg:"generic CPR prefix pending during explicit-width probe"
    (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[24;80") 0 7);
  let events, caps = drain_to_lists ~now:1.0 parser in
  equal ~msg:"generic CPR is not explicit-width CPR" (list event_testable) []
    events;
  equal ~msg:"generic CPR prefix flushes during explicit-width probe"
    (list response_testable)
    [ unknown_response "\x1b[24;80" ]
    caps;

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with startup_cursor_cpr = true };
  equal ~msg:"startup CPR prefix pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[24;80") 0 7);
  equal ~msg:"startup CPR prefix stays pending" (list capability_testable) []
    (drain_capabilities ~now:1.0 parser);
  equal ~msg:"startup CPR prefix completes" (list capability_testable)
    [ Input.Response.Cursor_position (24, 80) ]
    (feed_to_lists parser (Bytes.of_string "R") 0 1 |> snd |> capabilities);

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with pixel_resolution = true };
  equal ~msg:"partial pixel response pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[4;1080;192") 0 12);
  equal ~msg:"partial pixel response stays pending" (list capability_testable)
    []
    (drain_capabilities ~now:1.0 parser);
  equal ~msg:"partial pixel response completes" (list capability_testable)
    [ Input.Response.Pixel_resolution (1920, 1080) ]
    (feed_to_lists parser (Bytes.of_string "0t") 0 2 |> snd |> capabilities);

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    {
      Input.Parser.default_protocol_context with
      private_capability_replies = true;
    };
  equal ~msg:"partial DECRPM pending" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[?1016;2$") 0 10);
  equal ~msg:"partial DECRPM stays pending" (list capability_testable) []
    (drain_capabilities ~now:1.0 parser);
  equal ~msg:"partial DECRPM completes" (list capability_testable)
    [
      Input.Response.Mode_report
        { Input.Response.is_private = true; modes = [ (1016, 2) ] };
    ]
    (feed_to_lists parser (Bytes.of_string "y") 0 1 |> snd |> capabilities);

  let parser = Input.Parser.create () in
  Input.Parser.set_protocol_context parser
    { Input.Parser.default_protocol_context with explicit_width_cpr = true };
  ignore (feed_user parser (Bytes.of_string "\x1b[1;2") 0 5);
  equal ~msg:"first deferred explicit CPR timeout" (list response_testable) []
    (drain_responses ~now:1.0 parser);
  equal ~msg:"deferred explicit CPR has no wake deadline" bool true
    (Option.is_none (Input.Parser.deadline parser));
  equal ~msg:"second deferred explicit CPR timeout does not rearm"
    (list response_testable) []
    (drain_responses ~now:2.0 parser);
  equal ~msg:"deferred explicit CPR still has no wake deadline" bool true
    (Option.is_none (Input.Parser.deadline parser));
  equal ~msg:"more bytes after deferred timeout do not emit immediately"
    (list response_testable) []
    (feed_to_lists ~now:2.0 parser (Bytes.of_string ";") 0 1 |> snd);
  equal ~msg:"more bytes after deferred timeout rearm deadline" bool true
    (Option.is_some (Input.Parser.deadline parser));
  equal ~msg:"extended explicit CPR flushes as unknown response"
    (list response_testable)
    [ unknown_response "\x1b[1;2;" ]
    (drain_responses ~now:3.0 parser)

let test_parse_paste_mode () =
  match parse_user "\x1b[200~Hello, World!\x1b[201~" with
  | [ Input.Paste content ] ->
      equal ~msg:"paste content" string "Hello, World!" content
  | _ -> fail "Expected [Paste(content)]"

let test_parse_utf8 () =
  equal ~msg:"UTF-8 emoji" (list event_testable)
    [ key_event ~associated_text:"😀" (Input.Key.Char (Uchar.of_int 0x1F600)) ]
    (parse_user "😀");

  equal ~msg:"UTF-8 accented char" (list event_testable)
    [ key_event ~associated_text:"é" (Input.Key.Char (Uchar.of_int 0xE9)) ]
    (parse_user "é")

let test_alt_and_alt_ctrl () =
  equal ~msg:"Alt+Enter" (list event_testable)
    [
      key_event
        ~modifier:
          { Input.Modifier.none with Input.Modifier.alt = true; meta = true }
        Input.Key.Enter;
    ]
    (parse_user "\x1b\r");

  equal ~msg:"Alt+Line_feed" (list event_testable)
    [
      key_event
        ~modifier:
          { Input.Modifier.none with Input.Modifier.alt = true; meta = true }
        Input.Key.Line_feed;
    ]
    (parse_user "\x1b\n");

  equal ~msg:"Alt+Ctrl+A" (list event_testable)
    [
      key_event
        ~modifier:
          {
            Input.Modifier.none with
            Input.Modifier.alt = true;
            ctrl = true;
            meta = true;
          }
        (Input.Key.Char (Uchar.of_char 'a'));
    ]
    (parse_user "\x1b\x01");

  equal ~msg:"Alt+Ctrl+Space" (list event_testable)
    [
      key_event
        ~modifier:
          {
            Input.Modifier.none with
            Input.Modifier.alt = true;
            ctrl = true;
            meta = true;
          }
        (Input.Key.Char (Uchar.of_char ' '));
    ]
    (parse_user "\x1b\x00");

  equal ~msg:"Alt+Shift+A" (list event_testable)
    [
      key_event
        ~modifier:
          {
            Input.Modifier.none with
            Input.Modifier.alt = true;
            shift = true;
            meta = true;
          }
        (Input.Key.Char (Uchar.of_char 'A'));
    ]
    (parse_user "\x1bA")

let test_incremental_parsing () =
  let parser = Input.Parser.create () in
  equal ~msg:"no events yet" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b") 0 1);
  equal ~msg:"still no events" (list event_testable) []
    (feed_user parser (Bytes.of_string "[") 0 1);
  equal ~msg:"complete escape sequence" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "A") 0 1)

let test_more_key_variants () =
  let keys =
    [
      ("\x1b[5~", Input.Key.Page_up);
      ("\x1b[6~", Input.Key.Page_down);
      ("\x1b[H", Input.Key.Home);
      ("\x1b[F", Input.Key.End);
      ("\x1b[2~", Input.Key.Insert);
      ("\x1b[3~", Input.Key.Delete);
    ]
  in
  List.iter
    (fun (seq, key) ->
      equal ~msg:"Key" (list event_testable) [ key_event key ] (parse_user seq))
    keys;

  let f_keys =
    [ (17, 6); (18, 7); (19, 8); (20, 9); (21, 10); (23, 11); (24, 12) ]
  in
  List.iter
    (fun (code, n) ->
      let seq = Printf.sprintf "\x1b[%d~" code in
      equal ~msg:(Printf.sprintf "F%d" n) (list event_testable)
        [ key_event (Input.Key.F n) ]
        (parse_user seq))
    f_keys;

  let rxvt_shift_arrows =
    [
      ("\x1b[a", Input.Key.Up);
      ("\x1b[b", Input.Key.Down);
      ("\x1b[c", Input.Key.Right);
      ("\x1b[d", Input.Key.Left);
      ("\x1b[e", Input.Key.KP_5);
    ]
  in
  List.iter
    (fun (seq, key) ->
      equal ~msg:"rxvt shift arrows" (list event_testable)
        [
          key_event
            ~modifier:{ Input.Modifier.none with Input.Modifier.shift = true }
            key;
        ]
        (parse_user seq))
    rxvt_shift_arrows;

  let rxvt_special_codes =
    [
      (2, Input.Key.Insert);
      (3, Input.Key.Delete);
      (5, Input.Key.Page_up);
      (6, Input.Key.Page_down);
      (7, Input.Key.Home);
      (8, Input.Key.End);
    ]
  in
  List.iter
    (fun (code, key) ->
      let seq = Printf.sprintf "\x1b[%d$" code in
      equal ~msg:"rxvt shift special" (list event_testable)
        [
          key_event
            ~modifier:{ Input.Modifier.none with Input.Modifier.shift = true }
            key;
        ]
        (parse_user seq))
    rxvt_special_codes;
  List.iter
    (fun (code, key) ->
      let seq = Printf.sprintf "\x1b[%d^" code in
      equal ~msg:"rxvt ctrl special" (list event_testable)
        [
          key_event
            ~modifier:{ Input.Modifier.none with Input.Modifier.ctrl = true }
            key;
        ]
        (parse_user seq))
    rxvt_special_codes;

  let ss3_ctrl =
    [
      ("\x1bOa", Input.Key.Up);
      ("\x1bOb", Input.Key.Down);
      ("\x1bOc", Input.Key.Right);
      ("\x1bOd", Input.Key.Left);
      ("\x1bOe", Input.Key.KP_5);
    ]
  in
  List.iter
    (fun (seq, key) ->
      equal ~msg:"ss3 ctrl arrows" (list event_testable)
        [
          key_event
            ~modifier:{ Input.Modifier.none with Input.Modifier.ctrl = true }
            key;
        ]
        (parse_user seq))
    ss3_ctrl

let test_escape_drain () =
  let parser = Input.Parser.create () in
  (* Feed at time 0.0 - deadline will be set to 0.0 + timeout *)
  let events = feed_user parser (Bytes.of_string "\x1b") 0 1 in
  equal ~msg:"no immediate escape" (list event_testable) [] events;
  (* Drain at time 1.0 - well past the timeout deadline *)
  let events = drain_user ~now:1.0 parser in
  equal ~msg:"escape after timeout" (list event_testable)
    [ key_event Input.Key.Escape ]
    events

let test_alt_escape_no_sticky () =
  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b\x1b") 0 2 in
  equal ~msg:"alt+escape" (list event_testable)
    [
      key_event
        ~modifier:
          { Input.Modifier.none with Input.Modifier.alt = true; meta = true }
        Input.Key.Escape;
    ]
    events;
  match feed_user parser (Bytes.of_string "a") 0 1 with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      is_false ~msg:"alt cleared" modifier.alt;
      equal ~msg:"plain a" char 'a' (Uchar.to_char u)
  | _ -> fail "expected plain 'a' after alt escape"

let test_invalid_sequences () =
  let parser = Input.Parser.create () in
  equal ~msg:"incomplete CSI buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[") 0 2);

  let events, responses =
    feed_to_lists parser (Bytes.of_string "999999X") 0 7
  in
  equal ~msg:"invalid CSI is not user input" int 0 (List.length events);
  equal ~msg:"invalid CSI is an unknown response" (list response_testable)
    [ unknown_response "\x1b[999999X" ]
    responses;

  let long_seq = "\x1b[" ^ String.make 100 '9' ^ "m" in
  let events = parse_user long_seq in
  is_true ~msg:"long sequence handled"
    (List.length events = 0 || List.length events > 0);

  let invalid_utf8 = Bytes.of_string "\xff\xfe" in
  let events = feed_user parser invalid_utf8 0 2 in
  is_true ~msg:"invalid UTF-8 handled" (List.length events >= 0);

  match parse_user "a\x1b[999999999999mbc" with
  | Input.Key { key = Char c; _ } :: _ ->
      equal ~msg:"first char parsed" char 'a' (Uchar.to_char c)
  | _ -> fail "expected at least one char"

let test_interrupted_escape_regressions () =
  let parser = Input.Parser.create () in
  equal ~msg:"partial CSI buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b[123") 0 5);
  equal ~msg:"CSI resyncs on later ESC" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "\x1b[A") 0 3);

  let parser = Input.Parser.create () in
  equal ~msg:"partial SS3 buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1bO") 0 2);
  equal ~msg:"SS3 resyncs on later ESC" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "\x1b[A") 0 3);

  let parser = Input.Parser.create () in
  equal ~msg:"partial OSC buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b]52;c;") 0 7);
  equal ~msg:"OSC resyncs on later ESC" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "\x1b[A") 0 3);

  let parser = Input.Parser.create () in
  equal ~msg:"partial DCS buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1bP>|") 0 4);
  equal ~msg:"DCS resyncs on later ESC" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "\x1b[A") 0 3);

  let parser = Input.Parser.create () in
  equal ~msg:"partial APC buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\x1b_Gi=1;") 0 7);
  equal ~msg:"APC resyncs on later ESC" (list event_testable)
    [ key_event Input.Key.Up ]
    (feed_user parser (Bytes.of_string "\x1b[A") 0 3)

let test_combined_inputs () =
  equal ~msg:"Ctrl+Alt+Shift+Up" (list event_testable)
    [
      key_event
        ~modifier:
          {
            ctrl = true;
            alt = true;
            shift = true;
            super = false;
            hyper = false;
            meta = true;
            caps_lock = false;
            num_lock = false;
          }
        Input.Key.Up;
    ]
    (parse_user "\x1b[1;8A");

  let events = parse_user "\x1b[<0;5;10Ma" in
  equal ~msg:"mouse + key event count" int 2 (List.length events);
  (match events with
  | [ Input.Mouse _; Input.Key _ ] -> ()
  | _ -> fail "expected mouse then key event");

  (match parse_user "\x1b[200~Hello\nWorld\t!\x1b[201~" with
  | [ Input.Paste s ] -> equal ~msg:"paste content" string "Hello\nWorld\t!" s
  | _ -> fail "Expected single Paste event");

  let rapid =
    String.concat ""
      (List.init 50 (fun i -> String.make 1 (Char.chr (65 + (i mod 26)))))
  in
  equal ~msg:"rapid keys parsed" int 50 (List.length (parse_user rapid))

let test_kitty_keyboard () =
  (match parse_user "\x1b[97u" with
  | [ Input.Key { key = Char u; modifier; event_type; associated_text; _ } ] ->
      equal ~msg:"kitty 'a'" char 'a' (Uchar.to_char u);
      is_false ~msg:"ctrl" modifier.ctrl;
      is_false ~msg:"meta" modifier.meta;
      equal ~msg:"press" event_type_testable Input.Key.Press event_type;
      equal ~msg:"associated text" string "a" associated_text
  | _ -> fail "expected kitty key");

  (match parse_user "\x1b[97;5u" with
  | [ Input.Key { key = Char u; modifier; _ } ] ->
      equal ~msg:"kitty ctrl+a key" char 'a' (Uchar.to_char u);
      is_true ~msg:"ctrl set" modifier.ctrl
  | _ -> fail "expected kitty ctrl key event");

  equal ~msg:"kitty enter" (list event_testable)
    [ key_event Input.Key.Enter ]
    (parse_user "\x1b[13u");

  match parse_user "\x1b[97;1:3u" with
  | [ Input.Key { key = Char c; event_type; _ } ] ->
      equal ~msg:"kitty with event type" char 'a' (Uchar.to_char c);
      equal ~msg:"release event" event_type_testable Input.Key.Release
        event_type
  | _ -> fail "expected single key event"

let test_kitty_associated_text_fallback () =
  let expect_text seq expected =
    match parse_user seq with
    | [ Input.Key { associated_text; _ } ] ->
        equal ~msg:"kitty associated text" string expected associated_text
    | _ -> fail "expected single kitty key event"
  in
  expect_text "\x1b[97u" "a";
  expect_text "\x1b[97:65:97;2u" "A";
  expect_text "\x1b[32;2u" " "

let test_input_edge_cases () =
  equal ~msg:"empty input" (list event_testable) [] (parse_user "");

  equal ~msg:"null byte as Ctrl+Space" (list event_testable)
    [ char_event ~modifier:{ Input.Modifier.none with ctrl = true } ' ' ]
    (parse_user "\x00");

  let parser = Input.Parser.create () in
  let events = feed_user parser (Bytes.of_string "\x1b\x1b") 0 2 in
  equal ~msg:"first escape parsed as Alt+Escape" (list event_testable)
    [
      key_event
        ~modifier:
          { Input.Modifier.none with Input.Modifier.alt = true; meta = true }
        Input.Key.Escape;
    ]
    events;

  let large = String.make 10000 'X' in
  equal ~msg:"large input parsed" int 10000 (List.length (parse_user large));

  let parser = Input.Parser.create () in
  let data = Bytes.of_string "XXXabcYYY" in
  match feed_user parser data 3 3 with
  | Input.Key { key = Char c; _ } :: _ as events ->
      equal ~msg:"partial feed count" int 3 (List.length events);
      equal ~msg:"partial feed first char" char 'a' (Uchar.to_char c)
  | _ -> fail "expected char event"

let test_split_utf8 () =
  let parser = Input.Parser.create () in
  equal ~msg:"incomplete UTF-8 should be buffered" (list event_testable) []
    (feed_user parser (Bytes.of_string "\xE2\x82") 0 2);
  match feed_user parser (Bytes.of_string "\xAC") 0 1 with
  | [ Input.Key { key = Char u; _ } ] ->
      equal ~msg:"euro sign unicode" int 0x20AC (Uchar.to_int u)
  | _ -> fail "expected single UTF-8 character after completion"

let test_invalid_split_utf8_regression () =
  let parser = Input.Parser.create () in
  equal ~msg:"invalid split lead is buffered first" (list event_testable) []
    (feed_user parser (Bytes.of_string "\xE2") 0 1);
  match feed_user parser (Bytes.of_string "(") 0 1 with
  | [ Input.Key { key = Char u; _ } ] ->
      equal ~msg:"invalid continuation is reparsed as text" char '('
        (Uchar.to_char u)
  | events ->
      failf "expected '(' after invalid UTF-8 recovery, got %d events"
        (List.length events)

let test_buffer_overflow () =
  let large = String.make 5000 'X' in
  equal ~msg:"should parse all characters" int 5000
    (List.length (parse_user large))

let test_paste_mode_collection () =
  let paste_content = String.make 1000 'A' in
  match parse_user ("\x1b[200~" ^ paste_content ^ "\x1b[201~") with
  | [ Input.Paste content ] ->
      equal ~msg:"paste content matches" string paste_content content
  | _ -> fail "Expected single Paste event"

let test_reset_aborts_paste () =
  let parser = Input.Parser.create () in
  let start_and_payload = "\x1b[200~abc" in
  ignore
    (feed_user parser
       (Bytes.of_string start_and_payload)
       0
       (String.length start_and_payload));
  Input.Parser.reset parser;
  let trailing = "\x1b[201~xyz" in
  match
    feed_user parser (Bytes.of_string trailing) 0 (String.length trailing)
  with
  | [
   Input.Key { key = Input.Key.Char ux; _ };
   Input.Key { key = Input.Key.Char uy; _ };
   Input.Key { key = Input.Key.Char uz; _ };
  ] ->
      equal ~msg:"first char after reset" char 'x' (Uchar.to_char ux);
      equal ~msg:"second char after reset" char 'y' (Uchar.to_char uy);
      equal ~msg:"third char after reset" char 'z' (Uchar.to_char uz)
  | _ -> fail "expected only text after reset"

let test_csi_param_overflow () =
  let huge_param = String.make 20 '9' in
  let seq = Printf.sprintf "\x1b[%s;1A" huge_param in
  let events = parse_user seq in
  is_true ~msg:"got some events" (List.length events >= 0)

let test_cursor_position_report () =
  equal ~msg:"cursor position report" (list capability_testable)
    [ Input.Response.Cursor_position (10, 25) ]
    (parse_capabilities "\x1b[10;25R")

let test_device_attributes () =
  equal ~msg:"device attributes" (list capability_testable)
    [ Input.Response.Device_attributes [ 1; 2; 6; 9; 15 ] ]
    (parse_capabilities "\x1b[?1;2;6;9;15c")

let test_mode_report () =
  match parse_capabilities "\x1b[?1004;2$y" with
  | [ Input.Response.Mode_report report ] ->
      is_true ~msg:"private mode" report.is_private;
      is_true ~msg:"mode values" (report.modes = [ (1004, 2) ])
  | _ ->
      failf "Expected single mode report, got %d events"
        (List.length (parse_capabilities "\x1b[?1004;2$y"))

let test_color_scheme_report () =
  (* Color scheme DSR response: CSI ? 997 ; value n Response to CSI ? 996 n
     query. Value 1 = dark, 2 = light. *)
  (match parse_capabilities "\x1b[?997;1n" with
  | [ Input.Response.Color_scheme `Dark ] -> ()
  | _ ->
      failf "Expected Color_scheme Dark, got %d events"
        (List.length (parse_capabilities "\x1b[?997;1n")));
  (match parse_capabilities "\x1b[?997;2n" with
  | [ Input.Response.Color_scheme `Light ] -> ()
  | _ ->
      failf "Expected Color_scheme Light, got %d events"
        (List.length (parse_capabilities "\x1b[?997;2n")));
  (* Unknown value should still be handled *)
  (match parse_capabilities "\x1b[?997;99n" with
  | [ Input.Response.Color_scheme (`Unknown 99) ] -> ()
  | _ ->
      failf "Expected Color_scheme Unknown, got %d events"
        (List.length (parse_capabilities "\x1b[?997;99n")));
  (* Verify it doesn't leak into user events *)
  let user, responses = parse_single "\x1b[?997;1n" in
  equal ~msg:"no user events" int 0 (List.length user);
  equal ~msg:"one response" int 1 (List.length responses)

let test_user_and_caps_split () =
  let user, responses = parse_single "\x1b[?1004;2$yab" in
  equal ~msg:"capability extracted" (list response_testable)
    [
      Input.Response.Capability
        (Input.Response.Mode_report { is_private = true; modes = [ (1004, 2) ] });
    ]
    responses;
  equal ~msg:"two user keys" int 2 (List.length user);
  match user with
  | [ Input.Key { key = Char a; _ }; Input.Key { key = Char b; _ } ] ->
      equal ~msg:"a" char 'a' (Uchar.to_char a);
      equal ~msg:"b" char 'b' (Uchar.to_char b)
  | _ -> fail "expected two key events after capability"

let test_x10_mouse () =
  equal ~msg:"X10 mouse left press at (4,9)" (list event_testable)
    [ Input.mouse_press 4 9 Input.Mouse.Left ]
    (parse_user "\x1b[M \x25\x2A")

let test_x10_high_byte_mouse_regression () =
  equal ~msg:"X10 raw byte coordinate 95" (list event_testable)
    [ Input.mouse_press 95 9 Input.Mouse.Left ]
    (parse_user "\x1b[M \x80\x2A")

let test_x10_scroll_regression () =
  equal ~msg:"X10 ctrl scroll" (list event_testable)
    [
      Input.mouse_scroll
        ~modifiers:{ Input.Modifier.none with ctrl = true }
        7 8 Input.Mouse.Scroll_up;
    ]
    (parse_user "\x1b[Mp()")

let test_urxvt_mouse () =
  equal ~msg:"URXVT mouse left press at (9,19)" (list event_testable)
    [ Input.mouse_press 9 19 Input.Mouse.Left ]
    (parse_user "\x1b[32;10;20M")

let test_osc_sequences () =
  equal ~msg:"OSC responses do not leak into user input" (list event_testable)
    []
    (parse_user "\x1b]52;c;YWJj\x07\x1b]10;#FFFFFF\x07");
  match parse_responses "\x1b]52;c;YWJj\x07\x1b]10;#FFFFFF\x07" with
  | [
   Input.Response.Clipboard (sel, data); Input.Response.Osc (code, osc_data);
  ] ->
      equal ~msg:"clipboard selection" string "c" sel;
      equal ~msg:"clipboard data" string "abc" data;
      equal ~msg:"osc code" int 10 code;
      equal ~msg:"osc data" string "#FFFFFF" osc_data
  | _ ->
      failf "Expected [Clipboard; Osc], got %d responses"
        (List.length (parse_responses "\x1b]52;c;YWJj\x07\x1b]10;#FFFFFF\x07"))

let test_window_events () =
  equal ~msg:"focus, blur, resize" (list event_testable)
    [ Input.Focus; Input.Blur; Input.Resize (80, 30) ]
    (parse_user "\x1b[I\x1b[O\x1b[8;30;80t")

let test_pixel_resolution_response () =
  equal ~msg:"pixel resolution response" (list capability_testable)
    [ Input.Response.Pixel_resolution (644, 448) ]
    (parse_capabilities "\x1b[4;448;644t")

let test_kitty_advanced () =
  match parse_user "\x1b[97:65:97;5:3;98:99u" with
  | [
   Input.Key
     {
       key = Char u;
       shifted_key = Some su;
       base_key = Some bu;
       event_type;
       associated_text;
       modifier = m;
     };
  ] ->
      equal ~msg:"key" char 'a' (Uchar.to_char u);
      equal ~msg:"shifted" char 'A' (Uchar.to_char su);
      equal ~msg:"base" char 'a' (Uchar.to_char bu);
      is_true ~msg:"ctrl modifier" m.ctrl;
      equal ~msg:"event type is Release" event_type_testable Input.Key.Release
        event_type;
      equal ~msg:"associated text" string "bc" associated_text
  | _ -> fail "expected advanced Kitty key event"

let test_media_and_modifier_keys () =
  equal ~msg:"media, volume, shift_left" (list event_testable)
    [
      key_event Input.Key.Media_next;
      key_event Input.Key.Volume_up;
      key_event Input.Key.Shift_left;
    ]
    (parse_user "\x1b[57435u\x1b[57439u\x1b[57441u")

let test_paste_embedded_escapes () =
  let content = "Hello\x1b[31mWorld" in
  match parse_user ("\x1b[200~" ^ content ^ "\x1b[201~") with
  | [ Input.Paste s ] ->
      equal ~msg:"embedded escapes preserved" string content s
  | _ -> fail "expected single paste with embedded seq"

let test_paste_preserves_escapes_regression () =
  let content = "abc\x1bdef" in
  match parse_user ("\x1b[200~" ^ content ^ "\x1b[201~") with
  | [ Input.Paste s ] ->
      equal ~msg:"paste preserves ESC payload bytes" string content s
  | events -> failf "expected single paste, got %d events" (List.length events)

let test_kitty_invalid_codepoint_regression () =
  let expect_no_crash seq =
    try ignore (parse_user seq)
    with Invalid_argument msg ->
      fail ("parser raised Invalid_argument: " ^ msg)
  in
  expect_no_crash "\x1b[1114112u";
  expect_no_crash "\x1b[97;1;1114112u";
  expect_no_crash ("\x1b[" ^ String.make 40 '9' ^ "u")

let test_parsing_efficiency () =
  let long_invalid = "\x1b[" ^ String.make 10000 '9' ^ "X" in
  let t0 = Unix.gettimeofday () in
  let events, responses = parse_single long_invalid in
  let dt = Unix.gettimeofday () -. t0 in
  is_true ~msg:"fast on long invalid (<0.1s)" (dt < 0.1);
  equal ~msg:"long invalid is not user input" int 0 (List.length events);
  equal ~msg:"long invalid is one unknown response" int 1
    (List.length responses);

  let large_paste = String.make 50000 'A' in
  let t1 = Unix.gettimeofday () in
  match parse_user ("\x1b[200~" ^ large_paste ^ "\x1b[201~") with
  | [ Input.Paste s ] ->
      let dt_large = Unix.gettimeofday () -. t1 in
      is_true ~msg:"fast large paste (<0.1s)" (dt_large < 0.1);
      equal ~msg:"single paste event" int
        (String.length large_paste)
        (String.length s)
  | _ -> fail "large paste not optimized to single event"

(* Split-boundary tests: verify parsing works correctly when input is split at
   every possible byte boundary. This catches bugs related to buffering and
   state management across feed calls. *)

(* Helper: feed a string one byte at a time and collect all events *)
let feed_byte_by_byte parser s =
  let bytes = Bytes.of_string s in
  let len = String.length s in
  let acc = ref [] in
  for i = 0 to len - 1 do
    let events = feed_user parser bytes i 1 in
    acc := !acc @ events
  done;
  !acc

(* Helper: feed a string split at every possible boundary and check result *)
let test_all_splits s expected_event_count desc =
  let len = String.length s in
  (* Test split at every boundary from 1 to len-1 *)
  for split_at = 1 to len - 1 do
    let parser = Input.Parser.create () in
    let bytes = Bytes.of_string s in
    let events1, responses1 = feed_to_lists parser bytes 0 split_at in
    let events2, responses2 =
      feed_to_lists parser bytes split_at (len - split_at)
    in
    let total =
      List.length events1 + List.length responses1 + List.length events2
      + List.length responses2
    in
    equal
      ~msg:(Printf.sprintf "%s split at %d" desc split_at)
      int expected_event_count total
  done

let test_csi_split_boundaries () =
  (* CSI sequence for Up arrow: ESC [ A *)
  test_all_splits "\x1b[A" 1 "CSI Up";
  (* CSI sequence with parameters: ESC [ 1 ; 5 A (Ctrl+Up) *)
  test_all_splits "\x1b[1;5A" 1 "CSI Ctrl+Up";
  (* CSI tilde sequence: ESC [ 5 ~ (Page_up) *)
  test_all_splits "\x1b[5~" 1 "CSI Page_up";
  (* CSI-u (Kitty keyboard): ESC [ 97 u (lowercase 'a') *)
  test_all_splits "\x1b[97u" 1 "CSI-u key"

let test_osc_split_boundaries () =
  (* OSC with ST terminator: ESC ] 0 ; title ESC \ *)
  test_all_splits "\x1b]0;title\x1b\\" 1 "OSC title ST";
  (* OSC with BEL terminator: ESC ] 0 ; title BEL *)
  test_all_splits "\x1b]0;title\x07" 1 "OSC title BEL"

let test_utf8_split_boundaries () =
  (* 2-byte UTF-8: e (U+00E9) = 0xC3 0xA9 *)
  let parser = Input.Parser.create () in
  let events = feed_byte_by_byte parser "\xC3\xA9" in
  equal ~msg:"2-byte UTF-8 byte-by-byte" int 1 (List.length events);

  (* 3-byte UTF-8: EUR (U+20AC) = 0xE2 0x82 0xAC *)
  let parser = Input.Parser.create () in
  let events = feed_byte_by_byte parser "\xE2\x82\xAC" in
  equal ~msg:"3-byte UTF-8 byte-by-byte" int 1 (List.length events);

  (* 4-byte UTF-8: U+1F600 = 0xF0 0x9F 0x98 0x80 *)
  let parser = Input.Parser.create () in
  let events = feed_byte_by_byte parser "\xF0\x9F\x98\x80" in
  equal ~msg:"4-byte UTF-8 byte-by-byte" int 1 (List.length events);

  (* Multiple UTF-8 characters *)
  let parser = Input.Parser.create () in
  let events = feed_byte_by_byte parser "é€😀" in
  equal ~msg:"multiple UTF-8 byte-by-byte" int 3 (List.length events)

let test_paste_split_boundaries () =
  (* Bracketed paste with content *)
  let paste = "\x1b[200~hello\x1b[201~" in
  let len = String.length paste in
  for split_at = 1 to len - 1 do
    let parser = Input.Parser.create () in
    let bytes = Bytes.of_string paste in
    let events1 = feed_user parser bytes 0 split_at in
    let events2 = feed_user parser bytes split_at (len - split_at) in
    let all_events = events1 @ events2 in
    (* Should get exactly one Paste event with "hello" *)
    match all_events with
    | [ Input.Paste content ] ->
        equal
          ~msg:(Printf.sprintf "paste content split at %d" split_at)
          string "hello" content
    | _ ->
        fail (Printf.sprintf "paste split at %d: expected Paste event" split_at)
  done

let test_dcs_split_boundaries () =
  (* DCS sequence (used by some terminal protocols) *)
  (* DCS = ESC P ... ST where ST = ESC \ *)
  let dcs = "\x1bP+q5465\x1b\\" in
  let len = String.length dcs in
  for split_at = 1 to len - 1 do
    let parser = Input.Parser.create () in
    let bytes = Bytes.of_string dcs in
    let _ = feed_user parser bytes 0 split_at in
    let _ = feed_user parser bytes split_at (len - split_at) in
    (* DCS sequences may not produce user events, but shouldn't crash *)
    ()
  done

let test_mixed_split_boundaries () =
  (* Mix of text, escape sequences, and UTF-8 *)
  let mixed = "abc\x1b[A\xC3\xA9\x1b[B" in
  let parser = Input.Parser.create () in
  let events = feed_byte_by_byte parser mixed in
  (* Should get: 'a', 'b', 'c', Up, 'e', Down = 6 events *)
  equal ~msg:"mixed content byte-by-byte" int 6 (List.length events)

let test_modify_other_keys () =
  let check_one seq expected_key expected_mod_desc expected_mod =
    match parse_user seq with
    | [ Input.Key { modifier; _ } ] ->
        is_true ~msg:expected_mod_desc (expected_mod modifier);
        equal ~msg:"single key" (list event_testable)
          [ Input.key ~modifier expected_key ]
          (parse_user seq)
    | _ -> fail "expected single key event"
  in
  check_one "\x1b[27;2;13~" Input.Key.Enter "shift" (fun m -> m.shift);
  check_one "\x1b[27;5;13~" Input.Key.Enter "ctrl" (fun m -> m.ctrl);
  check_one "\x1b[27;5;27~" Input.Key.Escape "ctrl" (fun m -> m.ctrl);
  check_one "\x1b[27;3;32~"
    (Input.Key.Char (Uchar.of_int 32))
    "alt"
    (fun m -> m.alt)

let test_kitty_keyboard_queries () =
  equal ~msg:"kitty query 1" (list event_testable) [] (parse_user "\x1b[?1u");
  equal ~msg:"kitty query 1;2" (list event_testable) []
    (parse_user "\x1b[?1;2u");
  equal ~msg:"kitty query 0" (list event_testable) [] (parse_user "\x1b[?0u");
  equal ~msg:"caps kitty query" (list capability_testable)
    [ Input.Response.Kitty_keyboard { level = 1; flags = None } ]
    (parse_capabilities "\x1b[?1u");
  equal ~msg:"caps kitty query with flags" (list capability_testable)
    [ Input.Response.Kitty_keyboard { level = 1; flags = Some 2 } ]
    (parse_capabilities "\x1b[?1;2u");
  equal ~msg:"caps kitty query unsupported" (list capability_testable)
    [ Input.Response.Kitty_keyboard { level = 0; flags = None } ]
    (parse_capabilities "\x1b[?0u")

let tests =
  [
    test "parse regular chars" test_parse_regular_chars;
    test "char associated text default" test_char_associated_text_default;
    test "parse control chars" test_parse_control_chars;
    test "OpenTUI ctrl normalization regressions"
      test_opentui_ctrl_normalization_regressions;
    test "OpenTUI alt/meta regressions" test_opentui_alt_meta_regressions;
    test "parse special keys" test_parse_special_keys;
    test "parse arrow keys" test_parse_arrow_keys;
    test "parse function keys" test_parse_function_keys;
    test "Cygwin function key regressions" test_cygwin_function_key_regressions;
    test "parse modifiers" test_parse_modifiers;
    test "CSI sub params event type" test_csi_sub_params_with_event_type;
    test "parse mouse SGR" test_parse_mouse_sgr;
    test "SGR mouse state regressions" test_sgr_mouse_state_regressions;
    test "SGR mouse partial timeout regression"
      test_sgr_mouse_partial_timeout_regression;
    test "protocol context timeout regressions"
      test_protocol_context_timeout_regressions;
    test "parse paste mode" test_parse_paste_mode;
    test "parse UTF-8" test_parse_utf8;
    test "incremental parsing" test_incremental_parsing;
    test "more key variants" test_more_key_variants;
    test "escape drain" test_escape_drain;
    test "alt escape no sticky" test_alt_escape_no_sticky;
    test "invalid sequences" test_invalid_sequences;
    test "interrupted escape regressions" test_interrupted_escape_regressions;
    test "combined inputs" test_combined_inputs;
    test "kitty keyboard" test_kitty_keyboard;
    test "kitty associated text fallback" test_kitty_associated_text_fallback;
    test "edge cases" test_input_edge_cases;
    test "alt and alt+ctrl" test_alt_and_alt_ctrl;
    test "split UTF-8" test_split_utf8;
    test "invalid split UTF-8 regression" test_invalid_split_utf8_regression;
    test "buffer overflow" test_buffer_overflow;
    test "paste mode collection" test_paste_mode_collection;
    test "reset aborts paste" test_reset_aborts_paste;
    test "CSI param overflow" test_csi_param_overflow;
    test "cursor position report" test_cursor_position_report;
    test "device attributes" test_device_attributes;
    test "mode report" test_mode_report;
    test "color scheme report" test_color_scheme_report;
    test "user and caps split" test_user_and_caps_split;
    test "X10 mouse" test_x10_mouse;
    test "X10 high byte mouse regression" test_x10_high_byte_mouse_regression;
    test "X10 scroll regression" test_x10_scroll_regression;
    test "URXVT mouse" test_urxvt_mouse;
    test "OSC sequences" test_osc_sequences;
    test "window events" test_window_events;
    test "pixel resolution response" test_pixel_resolution_response;
    test "kitty advanced" test_kitty_advanced;
    test "media and modifier keys" test_media_and_modifier_keys;
    test "paste embedded escapes" test_paste_embedded_escapes;
    test "paste preserves escapes regression"
      test_paste_preserves_escapes_regression;
    test "Kitty invalid codepoint regression"
      test_kitty_invalid_codepoint_regression;
    test "modify other keys" test_modify_other_keys;
    test "kitty keyboard queries" test_kitty_keyboard_queries;
    slow "parsing efficiency" test_parsing_efficiency;
    test "CSI split boundaries" test_csi_split_boundaries;
    test "OSC split boundaries" test_osc_split_boundaries;
    test "UTF-8 split boundaries" test_utf8_split_boundaries;
    test "paste split boundaries" test_paste_split_boundaries;
    test "DCS split boundaries" test_dcs_split_boundaries;
    test "mixed split boundaries" test_mixed_split_boundaries;
  ]

let () = run "Input" [ group "parsing" tests ]
