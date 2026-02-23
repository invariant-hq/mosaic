open Mosaic_ui
open Expect_harness

let%expect_test "horizontal at 0%" =
  render ~width:20 ~height:1 (Vnode.progress_bar ~value:0. ());
  [%expect {|
    |}]

let%expect_test "horizontal at 25%" =
  render ~width:20 ~height:1 (Vnode.progress_bar ~value:0.25 ());
  [%expect {|
    |}]

let%expect_test "horizontal at 50%" =
  render ~width:20 ~height:1 (Vnode.progress_bar ~value:0.5 ());
  [%expect {|
    |}]

let%expect_test "horizontal at 75%" =
  render ~width:20 ~height:1 (Vnode.progress_bar ~value:0.75 ());
  [%expect {|
    |}]

let%expect_test "horizontal at 100%" =
  render ~width:20 ~height:1 (Vnode.progress_bar ~value:1.0 ());
  [%expect {|
    |}]

let%expect_test "vertical at 0%" =
  render ~width:1 ~height:10
    (Vnode.progress_bar ~value:0. ~orientation:`Vertical ());
  [%expect {|
    |}]

let%expect_test "vertical at 50%" =
  render ~width:1 ~height:10
    (Vnode.progress_bar ~value:0.5 ~orientation:`Vertical ());
  [%expect {|
    |}]

let%expect_test "vertical at 100%" =
  render ~width:1 ~height:10
    (Vnode.progress_bar ~value:1.0 ~orientation:`Vertical ());
  [%expect {|
    |}]

let%expect_test "range zero fills track" =
  render ~width:20 ~height:1
    (Vnode.progress_bar ~min:0.5 ~max:0.5 ~value:0.5 ());
  [%expect {|
    |}]

let%expect_test "small track" =
  render ~width:1 ~height:1 (Vnode.progress_bar ~value:0.5 ());
  [%expect {|▌|}]

let%expect_test "wide track" =
  render ~width:80 ~height:1 (Vnode.progress_bar ~value:0.5 ());
  [%expect {|
    |}]

let%expect_test "set_value moves fill" =
  let app = make_app () in
  reconcile app (Vnode.progress_bar ~value:0. ());
  frame app ~width:20 ~height:1;
  reconcile app (Vnode.progress_bar ~value:0.75 ());
  frame app ~width:20 ~height:1;
  [%expect {|
    |}]

let%expect_test "colored bar" =
  render_ansi ~width:20 ~height:1
    (Vnode.progress_bar ~value:0.5 ~filled_color:Ansi.Color.green
       ~empty_color:Ansi.Color.red ());
  [%expect
    {|[0;38;2;255;255;255;48;2;0;205;0m          [0;38;2;255;255;255;48;2;205;0;0m          [0m|}]
