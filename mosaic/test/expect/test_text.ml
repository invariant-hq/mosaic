open Mosaic_ui
open Expect_harness

let%expect_test "simple single line" =
  render ~width:20 ~height:1 (Vnode.text "Hello");
  [%expect_exact {|
Hello|}]

let%expect_test "empty content renders blank" =
  render ~width:10 ~height:1 (Vnode.text "");
  [%expect_exact {|
|}]

let%expect_test "multiline content" =
  render ~width:10 ~height:3 (Vnode.text "Line 1\nLine 2\nLine 3");
  [%expect_exact {|
Line 1
Line 2
Line 3|}]

let%expect_test "char wrap splits long words" =
  render ~width:12 ~height:2 (Vnode.text ~wrap:`Char "Supercalifragilistic");
  [%expect_exact {|
Supercalifra
gilistic|}]

let%expect_test "word wrap breaks on spaces" =
  render ~width:12 ~height:2 (Vnode.text ~wrap:`Word "hello world wrap");
  [%expect_exact {|
hello world
wrap|}]

let%expect_test "no wrap truncates" =
  render ~width:12 ~height:1 (Vnode.text "1234567890ABCDEF");
  [%expect_exact {|
1234567890AB|}]

let%expect_test "word wrap preserves words" =
  render ~width:10 ~height:3 (Vnode.text ~wrap:`Word "hello world foo");
  [%expect_exact {|
hello
world foo
|}]

let%expect_test "char wrap with multiline" =
  render ~width:8 ~height:4 (Vnode.text ~wrap:`Char "abcdefghij\nklmnop");
  [%expect_exact {|
abcdefgh
ij
klmnop
|}]

let%expect_test "word wrap with multiline" =
  render ~width:10 ~height:4 (Vnode.text ~wrap:`Word "hello world\nfoo bar baz");
  [%expect_exact {|
hello
world
foo bar
baz|}]

let%expect_test "tab characters expand" =
  render ~width:20 ~height:1 (Vnode.text ~tab_width:4 "a\tb");
  [%expect_exact {|
a    b|}]

let%expect_test "styled text single color" =
  let renderer = Renderer.create () in
  let txt = Text.create ~parent:(Renderer.root renderer) () in
  let red_style = Ansi.Style.fg Ansi.Color.red Ansi.Style.default in
  Text.set_styled_text txt
    [ { Text_buffer.text = "Red text"; style = red_style } ];
  Renderer.render_frame renderer ~width:20 ~height:1 ~delta:0.;
  let grid = Screen.grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_ansi grid);
  [%expect_exact
    {|
[0;38;2;205;0;0mRed text[0;38;2;255;255;255m            [0m|}]

let%expect_test "styled text multiple spans" =
  let renderer = Renderer.create () in
  let txt = Text.create ~parent:(Renderer.root renderer) () in
  let red_style = Ansi.Style.fg Ansi.Color.red Ansi.Style.default in
  let green_style = Ansi.Style.fg Ansi.Color.green Ansi.Style.default in
  Text.set_styled_text txt
    [
      { Text_buffer.text = "Red "; style = red_style };
      { Text_buffer.text = "Green"; style = green_style };
    ];
  Renderer.render_frame renderer ~width:20 ~height:1 ~delta:0.;
  let grid = Screen.grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_ansi grid);
  [%expect_exact
    {|
[0;38;2;205;0;0mRed [0;38;2;0;205;0mGreen[0;38;2;255;255;255m           [0m|}]

let%expect_test "content taller than viewport" =
  render ~width:10 ~height:2 (Vnode.text "Line 1\nLine 2\nLine 3\nLine 4");
  [%expect_exact {|
Line 1
Line 2|}]

let%expect_test "content wider than viewport" =
  render ~width:5 ~height:1 (Vnode.text "Hello World");
  [%expect_exact {|
Hello|}]

let%expect_test "unicode wide characters" =
  render ~width:10 ~height:1 (Vnode.text "\xe4\xbd\xa0\xe5\xa5\xbd");
  [%expect_exact {|
你好|}]

let%expect_test "emoji rendering" =
  render ~width:10 ~height:1 (Vnode.text "\xf0\x9f\x91\x8b");
  [%expect_exact {|
👋|}]

let%expect_test "word wrap long word falls back to char break" =
  render ~width:5 ~height:3 (Vnode.text ~wrap:`Word "abcdefghij");
  [%expect_exact {|
abcde
fghij
|}]

let%expect_test "word wrap preserves empty lines" =
  render ~width:10 ~height:3 (Vnode.text ~wrap:`Word "aaa\n\nbbb");
  [%expect_exact {|
aaa

bbb|}]

let%expect_test "word wrap single char words" =
  render ~width:5 ~height:3 (Vnode.text ~wrap:`Word "a b c d e");
  [%expect_exact {|
a b
c d e
|}]

let%expect_test "char wrap with CJK" =
  render ~width:6 ~height:2
    (Vnode.text ~wrap:`Char "\xe4\xbd\xa0\xe5\xa5\xbd\xe4\xb8\x96\xe7\x95\x8c");
  [%expect_exact {|
你好世
界|}]

let%expect_test "truncation with ellipsis" =
  render ~width:8 ~height:1
    (Vnode.text ~wrap:`None ~truncate:true "hello world");
  [%expect_exact {|
hello w…|}]
