open Mosaic_ui
open Expect_harness

let%expect_test "default border renders single-line chars" =
  render ~width:10 ~height:5 (Vnode.box ~border:true []);
  [%expect_exact {|
┌────────┐
│        │
│        │
│        │
└────────┘|}]

let%expect_test "double border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Grid.Border.double []);
  [%expect_exact {|
╔════════╗
║        ║
║        ║
║        ║
╚════════╝|}]

let%expect_test "rounded border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Grid.Border.rounded []);
  [%expect_exact {|
╭────────╮
│        │
│        │
│        │
╰────────╯|}]

let%expect_test "heavy border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Grid.Border.heavy []);
  [%expect_exact {|
┏━━━━━━━━┓
┃        ┃
┃        ┃
┃        ┃
┗━━━━━━━━┛|}]

let%expect_test "ascii border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Grid.Border.ascii []);
  [%expect_exact {|
+--------+
|        |
|        |
|        |
+--------+|}]

let%expect_test "custom border characters" =
  let c = Uchar.of_char in
  let custom =
    {
      Grid.Border.top_left = c '/';
      top_right = c '\\';
      bottom_left = c '\\';
      bottom_right = c '/';
      horizontal = c '~';
      vertical = c '!';
      top_t = c '^';
      bottom_t = c 'v';
      left_t = c '<';
      right_t = c '>';
      cross = c '+';
    }
  in
  render ~width:10 ~height:5 (Vnode.box ~border:true ~border_style:custom []);
  [%expect_exact {|
/~~~~~~~~\
!        !
!        !
!        !
\~~~~~~~~/|}]

let%expect_test "border sides top and bottom only" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_sides:[ `Top; `Bottom ] []);
  [%expect_exact {|
──────────



──────────|}]

let%expect_test "border sides left and right only" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_sides:[ `Left; `Right ] []);
  [%expect_exact {|
│        │
│        │
│        │
│        │
│        │|}]

let%expect_test "border disabled renders empty" =
  render ~width:10 ~height:5 (Vnode.box []);
  [%expect_exact {|




|}]

let%expect_test "background fill" =
  render_ansi ~width:10 ~height:3 (Vnode.box ~background:Ansi.Color.blue []);
  [%expect_exact
    {|
[0;38;2;255;255;255;48;2;0;0;238m          [0m
[0;38;2;255;255;255;48;2;0;0;238m          [0m
[0;38;2;255;255;255;48;2;0;0;238m          [0m|}]

let%expect_test "title left aligned" =
  render ~width:16 ~height:4 (Vnode.box ~border:true ~title:"Hello" []);
  [%expect_exact
    {|
┌─Hello────────┐
│              │
│              │
└──────────────┘|}]

let%expect_test "title center aligned" =
  render ~width:16 ~height:4
    (Vnode.box ~border:true ~title:"Hello" ~title_alignment:`Center []);
  [%expect_exact
    {|
┌────Hello─────┐
│              │
│              │
└──────────────┘|}]

let%expect_test "title right aligned" =
  render ~width:16 ~height:4
    (Vnode.box ~border:true ~title:"Hello" ~title_alignment:`Right []);
  [%expect_exact
    {|
┌────────Hello─┐
│              │
│              │
└──────────────┘|}]

let%expect_test "title omitted without top border" =
  render ~width:16 ~height:4
    (Vnode.box ~border:true ~title:"Hidden"
       ~border_sides:[ `Left; `Right; `Bottom ] []);
  [%expect_exact
    {|
│              │
│              │
│              │
└──────────────┘|}]

let%expect_test "title omitted when too long" =
  render ~width:8 ~height:4 (Vnode.box ~border:true ~title:"VeryLongTitle" []);
  [%expect_exact {|
┌──────┐
│      │
│      │
└──────┘|}]

let%expect_test "nested box renders correctly" =
  render ~width:12 ~height:6
    (Vnode.box ~border:true
       [
         Vnode.box ~border:true
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_size (Vnode.size ~width:8 ~height:3))
           [];
       ]);
  [%expect_exact
    {|
┌──────────┐
│┌──────┐  │
││      │  │
│└──────┘  │
│          │
└──────────┘|}]

let%expect_test "colored border" =
  render_ansi ~width:10 ~height:3
    (Vnode.box ~border:true ~border_color:Ansi.Color.red []);
  [%expect_exact
    {|
[0;38;2;205;0;0m┌────────┐[0m
[0;38;2;205;0;0m│[0;38;2;255;255;255m        [0;38;2;205;0;0m│[0m
[0;38;2;205;0;0m└────────┘[0m|}]

let%expect_test "box with no content large size" =
  render ~width:20 ~height:8 (Vnode.box ~border:true []);
  [%expect_exact
    {|
┌──────────────────┐
│                  │
│                  │
│                  │
│                  │
│                  │
│                  │
└──────────────────┘|}]

let%expect_test "minimum size box 3x3" =
  render ~width:3 ~height:3 (Vnode.box ~border:true []);
  [%expect_exact {|
┌─┐
│ │
└─┘|}]

(* ── Focused Border Color ── *)

let%expect_test "focused box uses focused border color" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.box ~border:true ~focusable:true ~ref:(fun n -> node := Some n) []);
  focus app (Option.get !node);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact
    {|
[0;38;2;0;255;255m┌────────┐[0m
[0;38;2;0;255;255m│[0;38;2;255;255;255m        [0;38;2;0;255;255m│[0m
[0;38;2;0;255;255m└────────┘[0m|}]

let%expect_test "unfocused box uses normal border color" =
  let app = make_app () in
  reconcile app (Vnode.box ~border:true ~focusable:true []);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact
    {|
[0;38;2;229;229;229m┌────────┐[0m
[0;38;2;229;229;229m│[0;38;2;255;255;255m        [0;38;2;229;229;229m│[0m
[0;38;2;229;229;229m└────────┘[0m|}]

let%expect_test "focused box with explicit border color" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.box ~border:true ~focusable:true ~border_color:Ansi.Color.red
       ~ref:(fun n -> node := Some n)
       []);
  frame_ansi app ~width:10 ~height:3;
  focus app (Option.get !node);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact
    {|
[0;38;2;205;0;0m┌────────┐[0m
[0;38;2;205;0;0m│[0;38;2;255;255;255m        [0;38;2;205;0;0m│[0m
[0;38;2;205;0;0m└────────┘[0m
[0;38;2;0;255;255m┌────────┐[0m
[0;38;2;0;255;255m│[0;38;2;255;255;255m        [0;38;2;0;255;255m│[0m
[0;38;2;0;255;255m└────────┘[0m|}]

(* ── Reconciliation ── *)

let%expect_test "style change preserves border insets" =
  let app = make_app () in
  reconcile app (Vnode.box ~border:true []);
  frame app ~width:10 ~height:5;
  reconcile app
    (Vnode.box ~border:true
       ~style:
         (Toffee.Style.default |> Toffee.Style.set_padding (Vnode.padding 1))
       []);
  frame app ~width:10 ~height:5;
  [%expect_exact
    {|
┌────────┐
│        │
│        │
│        │
└────────┘
┌────────┐
│        │
│        │
│        │
└────────┘|}]

let%expect_test "props change adds border" =
  let app = make_app () in
  reconcile app (Vnode.box []);
  frame app ~width:10 ~height:5;
  reconcile app (Vnode.box ~border:true []);
  frame app ~width:10 ~height:5;
  [%expect_exact
    {|





┌────────┐
│        │
│        │
│        │
└────────┘|}]

(* ── Overflow + Clipping ── *)

let%expect_test "overflow visible allows children beyond bounds" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true
       [
         Vnode.text
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:1))
           "ABCDEFGHIJKLMNOPQRST";
       ]);
  [%expect_exact {|
┌────────┐
│ABCDEFGHI
│        │
│        │
└────────┘|}]

let%expect_test "overflow hidden clips children to content area" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_overflow
              {
                x = Toffee.Style.Overflow.Hidden;
                y = Toffee.Style.Overflow.Hidden;
              })
       [
         Vnode.text
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:1))
           "ABCDEFGHIJKLMNOPQRST";
       ]);
  [%expect_exact {|
┌────────┐
│ABCDEFGH│
│        │
│        │
└────────┘|}]

(* ── Background + Border ── *)

let%expect_test "background with border" =
  render_ansi ~width:10 ~height:3
    (Vnode.box ~border:true ~background:Ansi.Color.blue []);
  [%expect_exact
    {|
[0;38;2;229;229;229;48;2;0;0;238m┌────────┐[0m
[0;38;2;229;229;229;48;2;0;0;238m│[0;38;2;255;255;255;48;2;0;0;238m        [0;38;2;229;229;229;48;2;0;0;238m│[0m
[0;38;2;229;229;229;48;2;0;0;238m└────────┘[0m|}]

(* ── Fill ── *)

let%expect_test "fill false does not fill interior" =
  render_ansi ~width:10 ~height:3
    (Vnode.box ~background:Ansi.Color.blue ~fill:false []);
  [%expect_exact
    {|
[0;38;2;255;255;255m          [0m
[0;38;2;255;255;255m          [0m
[0;38;2;255;255;255m          [0m|}]
