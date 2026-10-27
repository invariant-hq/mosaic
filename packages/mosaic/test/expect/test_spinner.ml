open Mosaic_ui
open Expect_harness

let%expect_test "dots spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.dots ());
  [%expect {|⠋|}]

let%expect_test "line spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.line ());
  [%expect {|-|}]

let%expect_test "spinner with color" =
  render_ansi ~width:2 ~height:1
    (Mosaic.spinner ~frame_set:Spinner.dots ~color:Ansi.Color.red ());
  [%expect {| [0;38;5;1m⠋[39m [0m |}]

let%expect_test "dots2 spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.dots2 ());
  [%expect {|⣾|}]

let%expect_test "arc spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.arc ());
  [%expect {|◜|}]

let%expect_test "bounce spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.bounce ());
  [%expect {|⠁|}]

let%expect_test "circle spinner first frame" =
  render ~width:2 ~height:1 (Mosaic.spinner ~frame_set:Spinner.circle ());
  [%expect {|◡|}]
