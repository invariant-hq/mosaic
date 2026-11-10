open Expect_harness

let%expect_test "horizontal at 0%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~value:0. ());
  [%expect {| ──────────────────── |}]

let%expect_test "horizontal at 25%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~value:0.25 ());
  [%expect {| █████─────────────── |}]

let%expect_test "horizontal at 50%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~value:0.5 ());
  [%expect {| ██████████────────── |}]

let%expect_test "horizontal at 75%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~value:0.75 ());
  [%expect {| ███████████████───── |}]

let%expect_test "horizontal at 100%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~value:1.0 ());
  [%expect {| ████████████████████ |}]

let%expect_test "vertical at 0%" =
  render ~width:1 ~height:10
    (Mosaic.progress_bar ~value:0. ~orientation:`Vertical ());
  [%expect
    {|
    │
    │
    │
    │
    │
    │
    │
    │
    │
    │
    |}]

let%expect_test "vertical at 50%" =
  render ~width:1 ~height:10
    (Mosaic.progress_bar ~value:0.5 ~orientation:`Vertical ());
  [%expect
    {|
    │
    │
    │
    │
    │
    █
    █
    █
    █
    █
    |}]

let%expect_test "vertical at 100%" =
  render ~width:1 ~height:10
    (Mosaic.progress_bar ~value:1.0 ~orientation:`Vertical ());
  [%expect
    {|
    █
    █
    █
    █
    █
    █
    █
    █
    █
    █
    |}]

let%expect_test "range zero fills track" =
  render ~width:20 ~height:1
    (Mosaic.progress_bar ~min:0.5 ~max:0.5 ~value:0.5 ());
  [%expect {| ████████████████████ |}]

let%expect_test "small track" =
  render ~width:1 ~height:1 (Mosaic.progress_bar ~value:0.5 ());
  [%expect {|▌|}]

let%expect_test "wide track" =
  render ~width:80 ~height:1 (Mosaic.progress_bar ~value:0.5 ());
  [%expect
    {| ████████████████████████████████████████──────────────────────────────────────── |}]

let%expect_test "set_value moves fill" =
  let app = make_app () in
  reconcile app (Mosaic.progress_bar ~value:0. ());
  frame app ~width:20 ~height:1;
  reconcile app (Mosaic.progress_bar ~value:0.75 ());
  frame app ~width:20 ~height:1;
  [%expect {|
    ────────────────────
    ███████████████─────
    |}]

let%expect_test "colored bar" =
  render_ansi ~width:20 ~height:1
    (Mosaic.progress_bar ~value:0.5 ~filled_color:Ansi.Color.green
       ~empty_color:Ansi.Color.red ());
  [%expect {| [0;38;5;2m██████████[38;5;1m──────────[0m |}]

(* A thin gauge: heavy line filled, its left half where the value ends in
   a cell's first half, light line empty. *)
let thin = { Mosaic.Progress_bar.filled = "━"; half = "╸"; empty = "─" }

let%expect_test "glyphs at 0%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:0. ());
  [%expect {| ──────────────────── |}]

let%expect_test "glyphs at 25%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:0.25 ());
  [%expect {| ━━━━━─────────────── |}]

let%expect_test "glyphs end in half a cell" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:0.275 ());
  [%expect {| ━━━━━╸────────────── |}]

let%expect_test "glyphs at 100%" =
  render ~width:20 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:1. ());
  [%expect {| ━━━━━━━━━━━━━━━━━━━━ |}]

let%expect_test "glyphs clamp the value" =
  render ~width:4 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:7. ());
  render ~width:4 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:(-1.) ());
  [%expect {|
    ━━━━
    ────
    |}]

let%expect_test "glyphs in one cell" =
  render ~width:1 ~height:1 (Mosaic.progress_bar ~glyphs:thin ~value:0.5 ());
  [%expect {|╸|}]

let%expect_test "glyphs on a vertical bar fill from the bottom" =
  render ~width:1 ~height:5
    (Mosaic.progress_bar ~orientation:`Vertical
       ~glyphs:{ filled = "┃"; half = "╻"; empty = "│" }
       ~value:0.5 ());
  [%expect {|
    │
    │
    ╻
    ┃
    ┃
    |}]

(* Colors given with glyphs color the glyphs, and the half glyph takes the
   filled color. *)
let%expect_test "glyphs in colors" =
  render_ansi ~width:4 ~height:1
    (Mosaic.progress_bar ~glyphs:thin ~value:0.375
       ~filled_color:Ansi.Color.green ~empty_color:Ansi.Color.red ());
  [%expect {| [0;38;5;2m━╸[38;5;1m──[0m |}]

(* A glyph of several scalars, one cell wide, fills its cells too. *)
let%expect_test "glyphs of several scalars" =
  render ~width:4 ~height:1
    (Mosaic.progress_bar
       ~glyphs:{ filled = "e\u{0301}"; half = "\u{258C}"; empty = "-" }
       ~value:0.5 ());
  [%expect {| éé-- |}]
