open Mosaic_ui
open Expect_harness

let%expect_test "empty unfocused shows placeholder" =
  render ~width:20 ~height:1 (Vnode.input ~placeholder:"Type here" ());
  [%expect_exact {|
Type here|}]

let%expect_test "empty focused shows cursor area" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
|}]

let%expect_test "content renders left aligned" =
  render ~width:20 ~height:1 (Vnode.input ~value:"hello" ());
  [%expect_exact {|
hello|}]

let%expect_test "content after typing" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  send_char app 'a';
  send_char app 'b';
  send_char app 'c';
  frame app ~width:20 ~height:1;
  [%expect_exact {|

abc|}]

let%expect_test "cursor at end of text" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ~value:"hello" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello|}]

let%expect_test "cursor in middle of text" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ~value:"hello" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  send_key app Input.Key.Left;
  send_key app Input.Key.Left;
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello
hello|}]

let%expect_test "selection highlight" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ~value:"hello" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  let ctrl_mod = { no_mod with Input.Key.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod (Input.Key.Char (Uchar.of_char 'a'));
  frame_ansi app ~width:20 ~height:1;
  [%expect_exact
    {|
hello
[0;38;2;229;229;229mhello[0;38;2;255;255;255m               [0m|}]

let%expect_test "narrow width shows start of content" =
  render ~width:5 ~height:1 (Vnode.input ~value:"hello world" ());
  [%expect_exact {|
hello|}]

let%expect_test "scrolled content after typing" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame app ~width:5 ~height:1;
  String.iter (fun c -> send_char app c) "abcdefghij";
  frame app ~width:5 ~height:1;
  [%expect_exact {|

ghij|}]

let%expect_test "after backspace" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ~value:"abc" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  send_key app Input.Key.Backspace;
  frame app ~width:20 ~height:1;
  [%expect_exact {|
abc
ab|}]

let%expect_test "after ctrl u clear to start" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.input ~ref:(fun n -> node := Some n) ~value:"hello world" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  let ctrl_mod = { no_mod with Input.Key.ctrl = true } in
  send_key_with_mod app ~modifier:ctrl_mod (Input.Key.Char (Uchar.of_char 'u'));
  frame app ~width:20 ~height:1;
  [%expect_exact {|
hello world
|}]

let%expect_test "placeholder color" =
  render_ansi ~width:20 ~height:1
    (Vnode.input ~placeholder:"Type here" ~placeholder_color:Ansi.Color.red ());
  [%expect_exact
    {|
[0;38;2;205;0;0mType here[0;38;2;255;255;255m           [0m|}]

let%expect_test "focused vs unfocused styles" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.input
       ~ref:(fun n -> node := Some n)
       ~value:"hi" ~text_color:Ansi.Color.white
       ~focused_text_color:Ansi.Color.green ());
  frame_ansi app ~width:20 ~height:1;
  focus app (Option.get !node);
  frame_ansi app ~width:20 ~height:1;
  [%expect_exact
    {|
[0;38;2;229;229;229mhi[0;38;2;255;255;255m                  [0m
[0;38;2;0;205;0mhi[0;38;2;255;255;255m                  [0m|}]

let%expect_test "placeholder shows when focused and empty" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.input ~ref:(fun n -> node := Some n) ~placeholder:"Type here" ());
  focus app (Option.get !node);
  frame app ~width:20 ~height:1;
  [%expect_exact {|
Type here|}]

let%expect_test "max length content" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.input ~ref:(fun n -> node := Some n) ~max_length:5 ());
  focus app (Option.get !node);
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
