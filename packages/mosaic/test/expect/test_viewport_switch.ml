open Expect_harness

let branch label =
  Mosaic.box ~border:true
    ~layout:
      (Toffee.Style.default
      |> Toffee.Style.set_size
           {
             Toffee.Geometry.Size.width = Toffee.Style.Dimension.percent 1.;
             height = Toffee.Style.Dimension.length 3.;
           })
    [ Mosaic.text label ]

let view =
  Mosaic.viewport_switch ~at_least_width:30 ~wide:(branch "wide branch")
    ~narrow:(branch "narrow branch")

let%expect_test "selects narrow branch below viewport threshold" =
  render ~fill:false ~width:20 ~height:4 view;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}]

let%expect_test "selects wide branch at viewport threshold" =
  render ~fill:false ~width:30 ~height:4 view;
  [%expect_exact
    {|
┌────────────────────────────┐
│wide branch                 │
└────────────────────────────┘
|}]

let%expect_test "reselects on resize crossing with the same vnode" =
  let app = make_app () in
  reconcile ~fill:false ~viewport_width:20 app view;
  frame app ~width:20 ~height:4;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}];
  reconcile ~fill:false ~viewport_width:30 app view;
  frame app ~width:30 ~height:4;
  [%expect_exact
    {|
┌────────────────────────────┐
│wide branch                 │
└────────────────────────────┘
|}];
  reconcile ~fill:false ~viewport_width:20 app view;
  frame app ~width:20 ~height:4;
  [%expect_exact
    {|
┌──────────────────┐
│narrow branch     │
└──────────────────┘
|}]
