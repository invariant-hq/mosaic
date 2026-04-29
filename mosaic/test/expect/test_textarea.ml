open Mosaic_ui
open Expect_harness

let%expect_test "empty unfocused shows placeholder" =
  render ~width:20 ~height:3 (Vnode.textarea ~placeholder:"Type here" ());
  [%expect_exact {|
Type here

|}]

let%expect_test "empty focused shows cursor area" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.textarea ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:3;
  [%expect_exact {|


|}]

let%expect_test "single-line content renders" =
  render ~width:20 ~height:3 (Vnode.textarea ~value:"hello" ());
  [%expect_exact {|
hello

|}]

let%expect_test "multi-line content renders" =
  render ~width:20 ~height:5
    (Vnode.textarea ~value:"line one\nline two\nline three" ());
  [%expect_exact {|
line one
line two
line three

|}]

let%expect_test "content after typing with newlines" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.textarea ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:5;
  send_char app 'a';
  send_char app 'b';
  send_key app Input.Key.Enter;
  send_char app 'c';
  send_char app 'd';
  frame app ~width:20 ~height:5;
  [%expect_exact {|





ab
cd


|}]

let%expect_test "after backspace line join" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.textarea ~ref:(fun n -> node := Some n) ~value:"abc\ndef" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:5;
  (* Move cursor to start of second line and backspace to join *)
  send_key app Input.Key.Home;
  send_key app Input.Key.Down;
  send_key app Input.Key.Backspace;
  frame app ~width:20 ~height:5;
  [%expect_exact {|
abc
def



abcdef



|}]

let%expect_test "placeholder color" =
  render_ansi ~width:20 ~height:3
    (Vnode.textarea ~placeholder:"Type here" ~placeholder_color:Ansi.Color.red
       ());
  [%expect_exact
    {|
[0;38;2;205;0;0mType here[0;38;2;255;255;255m           [0m
[0;38;2;255;255;255m                    [0m
[0;38;2;255;255;255m                    [0m|}]

let%expect_test "ctrl+u clears to line start" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.textarea ~ref:(fun n -> node := Some n) ~value:"abc\ndef" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:5;
  (* Cursor is at buffer end (after 'f'); Ctrl+U deletes to line start *)
  let ctrl_mod = { no_mod with Input.Modifier.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod (Input.Key.Char (Uchar.of_char 'u'));
  frame app ~width:20 ~height:5;
  [%expect_exact {|
abc
def



abc



|}]
