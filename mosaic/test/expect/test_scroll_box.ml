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
