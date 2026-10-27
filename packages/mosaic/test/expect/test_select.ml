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

(* A select over [options], each reporting its position. *)
let select ?(selected = 0) ?(on_change = ignore) ?show_description options =
  Mosaic.select ?show_description ~equal:Int.equal ~selected:(Some selected)
    ~on_change
    (List.mapi
       (fun i { Select.label; description } ->
         Mosaic.Select.item i ?description label)
       options)

let%expect_test "basic rendering first selected" =
  render ~width:30 ~height:10 (select ~selected:0 items);
  [%expect_exact
    {|
 › Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "second item selected" =
  render ~width:30 ~height:10 (select ~selected:1 items);
  [%expect_exact
    {|
   Alpha
   First item
 › Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "no descriptions" =
  render ~width:30 ~height:10 (select ~show_description:false items);
  [%expect_exact {|
 › Alpha
   Beta
   Gamma
   Delta
   Epsilon




|}]

let%expect_test "navigation changes selection" =
  let app = make_app () in
  let selected = ref 0 in
  let view () =
    select ~selected:!selected ~on_change:(fun index -> selected := index) items
  in
  reconcile app (view ());
  focus app (mounted app);
  frame app ~width:30 ~height:10;
  send_key app Matrix_input.Keystroke.Down;
  reconcile app (view ());
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
 ❯ Alpha
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
 ❯ Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "focused vs unfocused" =
  let app = make_app () in
  reconcile app (select items);
  frame app ~width:30 ~height:10;
  focus app (mounted app);
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
 › Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item
 ❯ Alpha
   First item
   Beta
   Second item
   Gamma

   Delta
   Fourth item
   Epsilon
   Fifth item|}]

let%expect_test "intrinsic select fits between siblings on its first frame" =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Start)
  in
  render ~width:12 ~height:4
    (Mosaic.box ~layout:style
       [
         Mosaic.text "before";
         select ~show_description:false
           Select.
             [
               { label = "A"; description = None };
               { label = "B"; description = None };
             ];
         Mosaic.text "after";
       ]);
  [%expect_exact {|
before
 › A
   B
after|}]
