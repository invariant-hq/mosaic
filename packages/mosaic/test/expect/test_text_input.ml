open Mosaic_ui
open Expect_harness

let input ?app ?(value = "") ?max_length ?placeholder ?placeholder_color
    ?text_color ?focused_text_color () =
  let rec view state =
    Vnode.input ~state ?placeholder ?placeholder_color ?text_color
      ?focused_text_color
      ~on_change:(fun state ->
        Option.iter (fun app -> reconcile app (view state)) app)
      ()
  in
  view (Editor.of_string ?max_length value)

let%expect_test "empty unfocused shows placeholder" =
  render ~width:20 ~height:1 (input ~placeholder:"Type here" ());
  [%expect_exact {|
Type here|}]

let%expect_test "empty focused shows cursor area" =
  let app = make_app () in
  reconcile app (input ~app ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
|}]

let%expect_test "content renders left aligned" =
  render ~width:20 ~height:1 (input ~value:"hello" ());
  [%expect_exact {|
hello|}]

let%expect_test "content after typing" =
  let app = make_app () in
  reconcile app (input ~app ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  send_char app 'a';
  send_char app 'b';
  send_char app 'c';
  frame app ~width:20 ~height:1;
  [%expect_exact {|

abc|}]

let%expect_test "cursor at end of text" =
  let app = make_app () in
  reconcile app (input ~app ~value:"hello" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello|}]

let%expect_test "cursor in middle of text" =
  let app = make_app () in
  reconcile app (input ~app ~value:"hello" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  send_key app Matrix_input.Keystroke.Left;
  send_key app Matrix_input.Keystroke.Left;
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello
hello|}]

let%expect_test "selection highlight" =
  let app = make_app () in
  reconcile app (input ~app ~value:"hello" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  let ctrl_mod = { no_mod with Matrix_input.Keystroke.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod
    (Matrix_input.Keystroke.Char (Uchar.of_char 'a'));
  frame_ansi app ~width:20 ~height:1;
  [%expect_exact {|
hello
[0mhello               [0m|}]

let%expect_test "narrow width shows start of content" =
  render ~width:5 ~height:1 (input ~value:"hello world" ());
  [%expect_exact {|
hello|}]

let%expect_test "scrolled content after typing" =
  let app = make_app () in
  reconcile app (input ~app ());
  focus app (mounted app);
  frame app ~width:5 ~height:1;
  String.iter (fun c -> send_char app c) "abcdefghij";
  frame app ~width:5 ~height:1;
  [%expect_exact {|

ghij|}]

let%expect_test "after backspace" =
  let app = make_app () in
  reconcile app (input ~app ~value:"abc" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  send_key app Matrix_input.Keystroke.Backspace;
  frame app ~width:20 ~height:1;
  [%expect_exact {|
abc
ab|}]

let%expect_test "after ctrl u clear to start" =
  let app = make_app () in
  reconcile app (input ~app ~value:"hello world" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  let ctrl_mod = { no_mod with Matrix_input.Keystroke.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod
    (Matrix_input.Keystroke.Char (Uchar.of_char 'u'));
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello world
|}]

let%expect_test "placeholder color" =
  render_ansi ~width:20 ~height:1
    (input ~placeholder:"Type here" ~placeholder_color:Ansi.Color.red ());
  [%expect_exact {|
[0;38;5;1mType here[39m           [0m|}]

let%expect_test "focused vs unfocused styles" =
  let app = make_app () in
  reconcile app
    (input ~app ~value:"hi" ~text_color:Ansi.Color.white
       ~focused_text_color:Ansi.Color.green ());
  frame_ansi app ~width:20 ~height:1;
  focus app (mounted app);
  frame_ansi app ~width:20 ~height:1;
  [%expect_exact
    {|
[0;38;5;7mhi[39m                  [0m
[0;38;5;2mhi[39m                  [0m|}]

let%expect_test "placeholder shows when focused and empty" =
  let app = make_app () in
  reconcile app (input ~app ~placeholder:"Type here" ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
Type here|}]

let%expect_test "max length content" =
  let app = make_app () in
  reconcile app (input ~app ~max_length:5 ());
  focus app (mounted app);
  frame app ~width:20 ~height:1;
  send_char app 'a';
  send_char app 'b';
  send_char app 'c';
  send_char app 'd';
  send_char app 'e';
  send_char app 'f';
  frame app ~width:20 ~height:1;
  [%expect_exact {|

abcde|}]
