open Mosaic_ui
open Expect_harness

(* ── Scroll box with vertical scrolling ── *)

let%expect_test "scroll box renders children in viewport" =
  render ~width:20 ~height:8
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:8))
       [ Vnode.text "Line 1"; Vnode.text "Line 2"; Vnode.text "Line 3" ]);
  [%expect {|
    Line 1
    Line 2
    Line 3 |}]

(* ── Scroll box with border ── *)

let%expect_test "scroll box inside bordered box" =
  render ~width:22 ~height:8
    (Vnode.box ~border:true
       [
         Vnode.scroll_box
           ~style:
             (Toffee.Style.default
             |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
             |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.))
           [ Vnode.text "Hello"; Vnode.text "World" ];
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
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       [ Vnode.text "First" ]);
  frame app ~width:20 ~height:5;
  reconcile app
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       [ Vnode.text "Second" ]);
  frame app ~width:20 ~height:5;
  [%expect {|
    First




    Second |}]

let%expect_test "scroll box stays constrained in column flex layout" =
  render ~width:20 ~height:8
    (Vnode.box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
       [
         Vnode.text "head";
         Vnode.scroll_box
           ~style:(Toffee.Style.default |> Toffee.Style.set_flex_grow 1.)
           (List.init 10 (fun i ->
                Vnode.text (Printf.sprintf "line %d" (i + 1))));
         Vnode.text "foot";
       ]);
  [%expect
    {|
    head
    line 1
    line 2
    line 3
    line 4
    line 5
    line 6
    foot |}]

let reveal ?x ?y ?(align_x = `Nearest) ?(align_y = `Start) ?(margin = 0) key :
    Scroll_box.reveal =
  { key; x; y; align_x; align_y; margin }

let numbered_lines count =
  List.init count (fun i -> Vnode.text (Printf.sprintf "line %d" (i + 1)))

let%expect_test "scroll box reveal scrolls to content coordinate" =
  render ~width:20 ~height:5
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       ~reveal:(reveal ~y:6 "line-7") (numbered_lines 12));
  [%expect {|
    line 7
    line 8
    line 9
    line 10
    line 11 |}]

let%expect_test "scroll box reveal overrides sticky scroll" =
  let app = make_app () in
  reconcile app
    (Vnode.scroll_box
       ~style:
         (Toffee.Style.default
         |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:5))
       ~sticky_scroll:true ~sticky_start:`Bottom ~reveal:(reveal ~y:2 "line-3")
       (numbered_lines 12));
  frame app ~width:20 ~height:5;
  frame app ~width:20 ~height:5;
  [%expect
    {|line 3
line 4
line 5
line 6
line 7
line 3
line 4             █
line 5             █
line 6
line 7|}]
