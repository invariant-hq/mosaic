open Mosaic_ui
open Expect_harness

let textarea ?app ?(value = "") ?max_length ?placeholder ?placeholder_color
    ?text_color ?focused_text_color () =
  let rec view state =
    Mosaic.textarea ~state ?placeholder ?placeholder_color ?text_color
      ?focused_text_color
      ~on_change:(fun state ->
        Option.iter (fun app -> reconcile app (view state)) app)
      ()
  in
  view (Editor.of_string ?max_length value)

let%expect_test "empty unfocused shows placeholder" =
  render ~width:20 ~height:3 (textarea ~placeholder:"Type here" ());
  [%expect_exact {|
Type here

|}]

let%expect_test "empty focused shows cursor area" =
  let app = make_app () in
  reconcile app (textarea ~app ());
  focus app (mounted app);
  frame app ~width:20 ~height:3;
  [%expect_exact {|


|}]

let%expect_test "single-line content renders" =
  render ~width:20 ~height:3 (textarea ~value:"hello" ());
  [%expect_exact {|
hello

|}]

let%expect_test "multi-line content renders" =
  render ~width:20 ~height:5
    (textarea ~value:"line one\nline two\nline three" ());
  [%expect_exact {|
line one
line two
line three

|}]

let%expect_test "content after typing with newlines" =
  let app = make_app () in
  reconcile app (textarea ~app ());
  focus app (mounted app);
  frame app ~width:20 ~height:5;
  send_char app 'a';
  send_char app 'b';
  send_key app Matrix_input.Keystroke.Enter;
  send_char app 'c';
  send_char app 'd';
  frame app ~width:20 ~height:5;
  [%expect_exact {|





ab
cd


|}]

let%expect_test "after backspace line join" =
  let app = make_app () in
  reconcile app (textarea ~app ~value:"abc\ndef" ());
  focus app (mounted app);
  frame app ~width:20 ~height:5;
  (* Move cursor to start of second line and backspace to join *)
  send_key app Matrix_input.Keystroke.Home;
  send_key app Matrix_input.Keystroke.Down;
  send_key app Matrix_input.Keystroke.Backspace;
  frame app ~width:20 ~height:5;
  [%expect_exact {|
abc
def



abcdef



|}]

let%expect_test "placeholder color" =
  render_ansi ~width:20 ~height:3
    (textarea ~placeholder:"Type here" ~placeholder_color:Ansi.Color.red ());
  [%expect_exact
    {|
[0;38;5;1mType here[39m           [0m
[0m                    [0m
[0m                    [0m|}]

let%expect_test "ctrl+u clears to line start" =
  let app = make_app () in
  reconcile app (textarea ~app ~value:"abc\ndef" ());
  focus app (mounted app);
  frame app ~width:20 ~height:5;
  (* Cursor is at buffer end (after 'f'); Ctrl+U deletes to line start *)
  let ctrl_mod = { no_mod with Matrix_input.Keystroke.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod
    (Matrix_input.Keystroke.Char (Uchar.of_char 'u'));
  frame app ~width:20 ~height:5;
  [%expect_exact {|
abc
def



abc



|}]
