open Mosaic_ui
open Expect_harness

let scroll_box ?(scroll = Scroll.at 0) ?(on_scroll = ignore) ?style
    ?show_scrollbars ?key children =
  Mosaic.scroll_box ~scroll ~on_scroll ?layout:style ?show_scrollbars ?key
    children

(* ── Scroll box with vertical scrolling ── *)

let%expect_test "scroll box renders children in viewport" =
  render ~width:20 ~height:8
    (scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:8))
       [ Mosaic.text "Line 1"; Mosaic.text "Line 2"; Mosaic.text "Line 3" ]);
  [%expect {|
    Line 1
    Line 2
    Line 3 |}]

(* ── Scroll box with border ── *)

let%expect_test "scroll box inside bordered box" =
  render ~width:22 ~height:8
    (Mosaic.box ~border:true
       [
         scroll_box
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
             |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.))
           [ Mosaic.text "Hello"; Mosaic.text "World" ];
       ]);
  [%expect
    {|
    ┌────────────────────┐
    │Hello               │
    │World               │
    │                    │
    │                    │
    │                    │
    │                    │
    └────────────────────┘ |}]

(* ── Reconciliation: vnode updates ── *)

let%expect_test "scroll box reconciles children" =
  let app = make_app () in
  reconcile app
    (scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:5))
       [ Mosaic.text "First" ]);
  frame app ~width:20 ~height:5;
  reconcile app
    (scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:5))
       [ Mosaic.text "Second" ]);
  frame app ~width:20 ~height:5;
  [%expect {|
    First




    Second |}]

let%expect_test "scroll box stays constrained in column flex layout" =
  render ~width:20 ~height:8
    (Mosaic.box
       ~layout:
         (Toffee.Style.default
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
       [
         Mosaic.text "head";
         scroll_box
           ~style:(Toffee.Style.default |> Toffee.Style.set_flex_grow 1.)
           (List.init 10 (fun i ->
                Mosaic.text (Printf.sprintf "line %d" (i + 1))));
         Mosaic.text "foot";
       ]);
  [%expect
    {|
    head
    line 1             █
    line 2             █
    line 3             █
    line 4             │
    line 5             │
    line 6             │
    foot
    |}]

let numbered_lines count =
  List.init count (fun i -> Mosaic.text (Printf.sprintf "line %d" (i + 1)))

let%expect_test "scroll box reveal scrolls to content coordinate" =
  render ~width:20 ~height:5
    (scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:5))
       ~scroll:(Scroll.reveal ~line:6 (Scroll.at 0))
       (numbered_lines 12));
  [%expect
    {|
    line 3             │
    line 4             █
    line 5             █
    line 6             │
    line 7             │
    |}]

let%expect_test "scroll box reveal overrides followed edge" =
  let app = make_app () in
  reconcile app
    (scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:5))
       ~scroll:(Scroll.reveal ~line:2 (Scroll.follow `Bottom (Scroll.at 0)))
       (numbered_lines 12));
  frame app ~width:20 ~height:5;
  frame app ~width:20 ~height:5;
  [%expect
    {|
    line 3             │
    line 4             █
    line 5             █
    line 6             │
    line 7             │
    line 3             │
    line 4             █
    line 5             █
    line 6             │
    line 7             │
    |}]

let%expect_test "accepted scrolling leaves follow until the model follows again"
    =
  let app = make_app () in
  let scroll = ref (Scroll.follow `Bottom (Scroll.at 0)) in
  let view count =
    scroll_box ~key:"transcript" ~scroll:!scroll
      ~on_scroll:(fun next -> scroll := next)
      ~show_scrollbars:false (numbered_lines count)
  in
  let show label vnode =
    print_endline label;
    reconcile app vnode;
    Matrix_grid.clear (Matrix_screen.next_grid (Renderer.screen app.renderer));
    settled_frame app ~width:12 ~height:4;
    print_newline ()
  in
  show "initial tail" (view 12);
  focus app (mounted app);
  send_key app Matrix_input.Keystroke.Page_up;
  show "page up" (view 12);
  show "append stays parked" (view 16);
  scroll := Scroll.follow `Bottom !scroll;
  show "model follows again" (view 16);
  send_key app Matrix_input.Keystroke.Page_up;
  show "manual page after following" (view 16);
  show "accepted position stays parked" (view 20);
  scroll := Scroll.follow `Bottom !scroll;
  show "model follows after append" (view 20);
  [%expect
    {|initial tail

line 9
line 10
line 11
line 12
page up

line 7
line 8
line 9
line 10
append stays parked

line 7
line 8
line 9
line 10
model follows again

line 13
line 14
line 15
line 16
manual page after following

line 11
line 12
line 13
line 14
accepted position stays parked

line 11
line 12
line 13
line 14
model follows after append

line 17
line 18
line 19
line 20|}]

(* ── Scroll bar visibility ── *)

let ten_lines = List.init 10 (fun i -> Mosaic.text (Printf.sprintf "Line %d" i))

let scrollbar_app ?show_scrollbars () =
  let app = make_app () in
  reconcile app
    (scroll_box ?show_scrollbars
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:4))
       ten_lines);
  app

let%expect_test "overflowing scroll box shows the vertical bar" =
  let app = scrollbar_app () in
  settled_frame app ~width:20 ~height:4;
  [%expect_exact
    {|
Line 0             █
Line 1             ▀
Line 2             │
Line 3             │|}]

let%expect_test "show_scrollbars false hides the bars despite overflow" =
  let app = scrollbar_app ~show_scrollbars:false () in
  settled_frame app ~width:20 ~height:4;
  [%expect_exact {|
Line 0
Line 1
Line 2
Line 3|}]

(* ── A scroll box with no room left ── *)

(* Header, a flex-grown scroll box with a zero base height, footer. The frame
   height decides how many rows the box gets, down to none: at zero rows its
   viewport quantizes to an empty region and the children are skipped, rather
   than the box fabricating a one-cell scissor and painting a transcript row
   on top of the footer. *)
let squeezed ~height =
  let app = make_app () in
  reconcile app
    (Mosaic.box
       ~layout:
         (Toffee.Style.default
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
       [
         Mosaic.text "HEADER";
         scroll_box ~show_scrollbars:false
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_flex_grow 1.
             |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
             |> Toffee.Style.set_height (Toffee.Style.Dimension.length 0.))
           [ Mosaic.text "BODY" ];
         Mosaic.text "FOOTER";
       ]);
  settled_frame app ~width:10 ~height

let%expect_test "squeezed scroll box keeps its one row" =
  squeezed ~height:3;
  [%expect_exact {|
HEADER
BODY
FOOTER|}]

let%expect_test "scroll box squeezed to zero rows paints nothing" =
  squeezed ~height:2;
  [%expect_exact {|
HEADER
FOOTER|}]
