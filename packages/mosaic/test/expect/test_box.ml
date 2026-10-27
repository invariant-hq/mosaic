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
    (Vnode.box ~border:true ~border_style:Matrix_grid.Border.double []);
  [%expect_exact {|
╔════════╗
║        ║
║        ║
║        ║
╚════════╝|}]

let%expect_test "rounded border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Matrix_grid.Border.rounded []);
  [%expect_exact {|
╭────────╮
│        │
│        │
│        │
╰────────╯|}]

let%expect_test "heavy border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Matrix_grid.Border.heavy []);
  [%expect_exact {|
┏━━━━━━━━┓
┃        ┃
┃        ┃
┃        ┃
┗━━━━━━━━┛|}]

let%expect_test "ascii border style" =
  render ~width:10 ~height:5
    (Vnode.box ~border:true ~border_style:Matrix_grid.Border.ascii []);
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
      Matrix_grid.Border.top_left = c '/';
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
[0;48;5;4m          [0m
[0;48;5;4m          [0m
[0;48;5;4m          [0m|}]

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
[0;38;5;1m┌────────┐[0m
[0;38;5;1m│[39m        [38;5;1m│[0m
[0;38;5;1m└────────┘[0m|}]

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
  reconcile app (Vnode.box ~border:true ~focusable:true []);
  focus app (mounted app);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact
    {|
[0;1m┏━━━━━━━━┓[0m
[0;1m┃[22m        [1m┃[0m
[0;1m┗━━━━━━━━┛[0m|}]

let%expect_test "unfocused box uses normal border color" =
  let app = make_app () in
  reconcile app (Vnode.box ~border:true ~focusable:true []);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact {|
[0m┌────────┐[0m
[0m│        │[0m
[0m└────────┘[0m|}]

let%expect_test "focused box with explicit border color" =
  let app = make_app () in
  reconcile app
    (Vnode.box ~border:true ~focusable:true ~border_color:Ansi.Color.red []);
  frame_ansi app ~width:10 ~height:3;
  focus app (mounted app);
  frame_ansi app ~width:10 ~height:3;
  [%expect_exact
    {|
[0;38;5;1m┌────────┐[0m
[0;38;5;1m│[39m        [38;5;1m│[0m
[0;38;5;1m└────────┘[0m
[0;38;5;1;1m┏━━━━━━━━┓[0m
[0;38;5;1;1m┃[39;22m        [38;5;1;1m┃[0m
[0;38;5;1;1m┗━━━━━━━━┛[0m|}]

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
[0;48;5;4m┌────────┐[0m
[0;48;5;4m│        │[0m
[0;48;5;4m└────────┘[0m|}]

(* ── Fill ── *)

let%expect_test "fill false does not fill interior" =
  render_ansi ~width:10 ~height:3
    (Vnode.box ~background:Ansi.Color.blue ~fill:false []);
  [%expect_exact {|
[0m          [0m
[0m          [0m
[0m          [0m|}]

(* ── Absolute positioning ── *)

let abs_box ~styles children =
  Vnode.box ~border:true
    ~style:
      (List.fold_left
         (fun s f -> f s)
         (Toffee.Style.default
         |> Toffee.Style.set_position Toffee.Style.Position.Absolute
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
         styles)
    children

let rows n = List.init n (fun i -> Vnode.text (Printf.sprintf "row%d" i))

let auto_margins =
  {
    Toffee.Geometry.Rect.left = Toffee.Style.Length_percentage_auto.auto;
    right = Toffee.Style.Length_percentage_auto.auto;
    top = Toffee.Style.Length_percentage_auto.auto;
    bottom = Toffee.Style.Length_percentage_auto.auto;
  }

let%expect_test "absolute child without inset keeps its content size" =
  render ~width:20 ~height:10
    (Vnode.box [ Vnode.text "flow"; abs_box ~styles:[] (rows 5) ]);
  [%expect_exact {|
┌────┐
│row0│
│row1│
│row2│
│row3│
│row4│
└────┘


|}]

let%expect_test "absolute child with inset 0 and auto margins centers" =
  render ~width:20 ~height:11
    (Vnode.box
       [
         abs_box
           ~styles:
             [
               Toffee.Style.set_inset (Vnode.inset 0);
               Toffee.Style.set_margin auto_margins;
               Toffee.Style.set_size (Vnode.size ~width:10 ~height:5);
             ]
           (rows 3);
       ]);
  [%expect_exact
    {|



     ┌────────┐
     │row0    │
     │row1    │
     │row2    │
     └────────┘


|}]

let%expect_test "absolute child centers via parent justify and align" =
  render ~width:20 ~height:11
    (Vnode.box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_justify_content
              (Some Toffee.Style.Align_content.Center)
         |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Center)
         )
       [ abs_box ~styles:[] (rows 3) ]);
  [%expect_exact
    {|



       ┌────┐
       │row0│
       │row1│
       │row2│
       └────┘


|}]

let%expect_test "overflow hidden parent clips its absolute child" =
  render ~width:20 ~height:10
    (Vnode.box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
       [
         Vnode.text "above";
         Vnode.box
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:3)
             |> Toffee.Style.set_overflow
                  {
                    Toffee.Geometry.Point.x = Toffee.Style.Overflow.Hidden;
                    y = Toffee.Style.Overflow.Hidden;
                  })
           [ abs_box ~styles:[] (rows 5) ];
         Vnode.text "below";
       ]);
  [%expect_exact {|
above
┌────┐
│row0│
│row1│
below




|}]
