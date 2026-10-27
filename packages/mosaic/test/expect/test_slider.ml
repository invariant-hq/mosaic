open Expect_harness

let%expect_test "horizontal at minimum" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:10. ());
  [%expect_exact {|
█▌──────────────────|}]

let%expect_test "horizontal at maximum" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:100. ~viewport_size:10. ());
  [%expect_exact {|
──────────────────▐█|}]

let%expect_test "horizontal at midpoint" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:50. ~viewport_size:10. ());
  [%expect_exact {|
─────────▐█─────────|}]

let%expect_test "horizontal at 25%" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:25. ~viewport_size:10. ());
  [%expect_exact {|
────▐█──────────────|}]

let%expect_test "horizontal at 75%" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:75. ~viewport_size:10. ());
  [%expect_exact {|
──────────────█▌────|}]

let%expect_test "vertical at minimum" =
  render ~width:1 ~height:10
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:0.
       ~viewport_size:10. ());
  [%expect_exact {|
▀
│
│
│
│
│
│
│
│
│|}]

let%expect_test "vertical at maximum" =
  render ~width:1 ~height:10
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:100.
       ~viewport_size:10. ());
  [%expect_exact {|
│
│
│
│
│
│
│
│
│
▄|}]

let%expect_test "vertical at midpoint" =
  render ~width:1 ~height:10
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:50.
       ~viewport_size:10. ());
  [%expect_exact {|
│
│
│
│
│
▀
│
│
│
│|}]

let%expect_test "vertical at 25%" =
  render ~width:1 ~height:10
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:25.
       ~viewport_size:10. ());
  [%expect_exact {|
│
│
▄
│
│
│
│
│
│
│|}]

let%expect_test "vertical at 75%" =
  render ~width:1 ~height:10
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:75.
       ~viewport_size:10. ());
  [%expect_exact {|
│
│
│
│
│
│
│
▀
│
│|}]

let%expect_test "small viewport small thumb" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:10. ());
  [%expect_exact {|
█▌──────────────────|}]

let%expect_test "large viewport large thumb" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:50. ());
  [%expect_exact {|
██████▌─────────────|}]

let%expect_test "range zero fills track" =
  render ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~min:50. ~max:50. ~state:50. ());
  [%expect_exact {|
████████████████████|}]

let%expect_test "wide track" =
  render ~width:80 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:50. ~viewport_size:10. ());
  [%expect_exact
    {|
────────────────────────────────────▐██████▌────────────────────────────────────|}]

let%expect_test "tall vertical track" =
  render ~width:1 ~height:20
    (Mosaic.slider ~on_change:ignore ~orientation:`Vertical ~state:50.
       ~viewport_size:10. ());
  [%expect_exact {|
│
│
│
│
│
│
│
│
│
▄
█
│
│
│
│
│
│
│
│
│|}]

let%expect_test "single cell width horizontal" =
  render ~width:1 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:50. ~viewport_size:10. ());
  [%expect_exact {|
▐|}]

let%expect_test "set_value moves thumb" =
  let app = make_app () in
  reconcile app
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  reconcile app
    (Mosaic.slider ~on_change:ignore ~state:75. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  [%expect_exact {|
█▌──────────────────
──────────────█▌────|}]

let%expect_test "set_viewport_size changes thumb" =
  let app = make_app () in
  reconcile app
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:10. ());
  frame app ~width:20 ~height:1;
  reconcile app
    (Mosaic.slider ~on_change:ignore ~state:0. ~viewport_size:50. ());
  frame app ~width:20 ~height:1;
  [%expect_exact {|
█▌──────────────────
██████▌─────────────|}]

let%expect_test "colored thumb and track" =
  render_ansi ~width:20 ~height:1
    (Mosaic.slider ~on_change:ignore ~state:50. ~viewport_size:10.
       ~track_color:Ansi.Color.blue ~thumb_color:Ansi.Color.red ());
  [%expect_exact {|
[0;48;5;4m         [38;5;1m▐█[39m         [0m|}]
