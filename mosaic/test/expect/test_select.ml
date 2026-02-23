open Mosaic_ui
open Expect_harness

let items =
  Select.
    [
      { label = "Alpha"; description = Some "First item" };
      { label = "Beta"; description = Some "Second item" };
      { label = "Gamma"; description = None };
      { label = "Delta"; description = Some "Fourth item" };
      { label = "Epsilon"; description = Some "Fifth item" };
    ]

let%expect_test "basic rendering first selected" =
  render ~width:30 ~height:10 (Vnode.select ~options:items ~selected_index:0 ());
  [%expect_exact
    {|
 ▶ Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "second item selected" =
  render ~width:30 ~height:10 (Vnode.select ~options:items ~selected_index:1 ());
  [%expect_exact
    {|
   Alpha
   First item
 ▶ Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "no descriptions" =
  render ~width:30 ~height:10
    (Vnode.select ~options:items ~show_description:false ());
  [%expect_exact {|
 ▶ Alpha
   Beta
   Gamma
   Delta
   Epsilon




|}]

let%expect_test "navigation changes selection" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.select ~ref:(fun n -> node := Some n) ~options:items ());
  focus app (Option.get !node);
  frame app ~width:30 ~height:10;
  send_key app Input.Key.Down;
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
 ▶ Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item
   Alpha
   First item
 ▶ Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "focused vs unfocused" =
  let app = make_app () in
  let node = ref None in
  reconcile app (Vnode.select ~ref:(fun n -> node := Some n) ~options:items ());
  frame app ~width:30 ~height:10;
  focus app (Option.get !node);
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
 ▶ Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item
 ▶ Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]
