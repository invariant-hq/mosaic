open Mosaic_ui
open Expect_harness

let%expect_test "horizontal at minimum" =
  render ~width:20 ~height:1 (Vnode.slider ~value:0. ~viewport_size:10. ());
  [%expect_exact {|
█▌|}]

let%expect_test "horizontal at maximum" =
  render ~width:20 ~height:1 (Vnode.slider ~value:100. ~viewport_size:10. ());
  [%expect_exact {|
                  ▐█|}]

let%expect_test "horizontal at midpoint" =
  render ~width:20 ~height:1 (Vnode.slider ~value:50. ~viewport_size:10. ());
  [%expect_exact {|
         ▐█|}]

let%expect_test "horizontal at 25%" =
  render ~width:20 ~height:1 (Vnode.slider ~value:25. ~viewport_size:10. ());
  [%expect_exact {|
    ▐█|}]

let%expect_test "horizontal at 75%" =
  render ~width:20 ~height:1 (Vnode.slider ~value:75. ~viewport_size:10. ());
  [%expect_exact {|
              █▌|}]

let%expect_test "vertical at minimum" =
  render ~width:1 ~height:10
    (Vnode.slider ~orientation:`Vertical ~value:0. ~viewport_size:10. ());
  [%expect_exact {|
▀








|}]

let%expect_test "vertical at maximum" =
  render ~width:1 ~height:10
    (Vnode.slider ~orientation:`Vertical ~value:100. ~viewport_size:10. ());
  [%expect_exact {|









▄|}]

let%expect_test "vertical at midpoint" =
  render ~width:1 ~height:10
    (Vnode.slider ~orientation:`Vertical ~value:50. ~viewport_size:10. ());
  [%expect_exact {|





▀



|}]

let%expect_test "vertical at 25%" =
  render ~width:1 ~height:10
    (Vnode.slider ~orientation:`Vertical ~value:25. ~viewport_size:10. ());
  [%expect_exact {|


▄






|}]

let%expect_test "vertical at 75%" =
  render ~width:1 ~height:10
    (Vnode.slider ~orientation:`Vertical ~value:75. ~viewport_size:10. ());
  [%expect_exact {|







▀

|}]

let%expect_test "small viewport small thumb" =
  render ~width:20 ~height:1 (Vnode.slider ~value:0. ~viewport_size:10. ());
  [%expect_exact {|
█▌|}]

let%expect_test "large viewport large thumb" =
  render ~width:20 ~height:1 (Vnode.slider ~value:0. ~viewport_size:50. ());
  [%expect_exact {|
██████▌|}]

let%expect_test "range zero fills track" =
  render ~width:20 ~height:1 (Vnode.slider ~min:50. ~max:50. ~value:50. ());
  [%expect_exact {|
████████████████████|}]

let%expect_test "wide track" =
  render ~width:80 ~height:1 (Vnode.slider ~value:50. ~viewport_size:10. ());
  [%expect_exact {|
                                    ▐██████▌|}]

let%expect_test "tall vertical track" =
  render ~width:1 ~height:20
    (Vnode.slider ~orientation:`Vertical ~value:50. ~viewport_size:10. ());
  [%expect_exact {|









▄
█








|}]

let%expect_test "single cell width horizontal" =
  render ~width:1 ~height:1 (Vnode.slider ~value:50. ~viewport_size:10. ());
  [%expect_exact {|
▐|}]

let%expect_test "set_value moves thumb" =
  let app = make_app () in
  reconcile app (Vnode.slider ~value:0. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  reconcile app (Vnode.slider ~value:75. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  [%expect_exact {|
█▌
              █▌|}]

let%expect_test "set_viewport_size changes thumb" =
  let app = make_app () in
  reconcile app (Vnode.slider ~value:0. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  reconcile app (Vnode.slider ~value:0. ~viewport_size:50. ());
  frame app ~width:20 ~height:1;
  [%expect_exact {|
█▌
██████▌|}]

let%expect_test "colored thumb and track" =
  render_ansi ~width:20 ~height:1
    (Vnode.slider ~value:50. ~viewport_size:10. ~track_color:Ansi.Color.blue
       ~thumb_color:Ansi.Color.red ());
  [%expect_exact
    {|
[0;38;2;255;255;255;48;2;0;0;238m         [0;38;2;205;0;0;48;2;0;0;238m▐█[0;38;2;255;255;255;48;2;0;0;238m         [0m|}]
