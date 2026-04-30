open Mosaic_ui
open Expect_harness
module Patch = Diff.Patch

let parse s =
  match Patch.of_unified s with
  | Ok patch -> patch
  | Error message -> failwith ("parse failed: " ^ message)

let print_patch patch = Format.printf "%a" Patch.pp patch

let simple_diff =
  {|--- a/test.js
+++ b/test.js
@@ -1,3 +1,3 @@
 function hello() {
-  console.log("Hello");
+  console.log("Hello, World!");
 }
|}

let multi_line_diff =
  String.concat "\n"
    [
      "--- a/math.js";
      "+++ b/math.js";
      "@@ -1,7 +1,11 @@";
      " function add(a, b) {";
      "   return a + b;";
      " }";
      " ";
      "+function subtract(a, b) {";
      "+  return a - b;";
      "+}";
      "+";
      " function multiply(a, b) {";
      "-  return a * b;";
      "+  return a * b * 1;";
      " }";
      "";
    ]

let add_only_diff =
  {|--- a/new.js
+++ b/new.js
@@ -0,0 +1,3 @@
+function newFunction() {
+  return true;
+}
|}

let remove_only_diff =
  {|--- a/old.js
+++ b/old.js
@@ -1,3 +0,0 @@
-function oldFunction() {
-  return false;
-}
|}

let asymmetric_wrap_diff =
  {|--- a/wrap.txt
+++ b/wrap.txt
@@ -1,3 +1,3 @@
 start
-short
+abcdefghijklmnopqrstuvwxyz0123456789
 end
|}

let plain_highlight =
  let highlighter = Code.Highlighter.sync (fun _ -> []) in
  let syntax = Diff.syntax ~language:"text" highlighter in
  { Diff.old = syntax; new_ = syntax }

let selected_line_color =
  let color = Ansi.Color.of_rgb 80 90 120 in
  { Line_number.gutter = color; content = Some color }

let line_highlight side first last : Diff.line_highlight =
  { side; first; last; color = selected_line_color }

let print_source_line_row patch ~layout source =
  match Diff.source_line_row patch ~layout source with
  | None -> Format.printf "none\n"
  | Some row -> Format.printf "row=%d\n" row

let grid_of_vnode ~width ~height vnode =
  let app = make_app () in
  reconcile app vnode;
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~delta:0.;
  Screen.next_grid (Renderer.screen app.renderer)

let color_to_string color = Format.asprintf "%a" Ansi.Color.pp color

let background_at grid ~x ~y =
  Cell_grid.get_background grid (Cell_grid.idx grid ~x ~y)

let assert_background ~msg grid ~x ~y color =
  let actual = background_at grid ~x ~y in
  if not (Ansi.Color.equal actual color) then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" msg (color_to_string color)
         (color_to_string actual))

let assert_not_background ~msg grid ~x ~y color =
  let actual = background_at grid ~x ~y in
  if Ansi.Color.equal actual color then
    failwith
      (Printf.sprintf "%s: did not expect %s" msg (color_to_string color))

let git_diff_with_marker =
  {|diff --git a/a.txt b/a.txt
index 1111111..2222222 100644
--- a/a.txt
+++ b/a.txt
@@ -1,2 +1,2 @@
 one
-two
\ No newline at end of file
+too
\ No newline at end of file
diff --git a/b.txt b/b.txt
index 3333333..4444444 100644
--- a/b.txt
+++ b/b.txt
@@ -1,1 +1,1 @@
-ignored
+ignored too
|}

let%expect_test "parse unified add and remove-only zero ranges" =
  print_patch (parse add_only_diff);
  print_patch (parse remove_only_diff);
  [%expect_exact
    {|@@ -0,0 +1,3 @@
+function newFunction() {
+  return true;
+}
@@ -1,3 +0,0 @@
-function oldFunction() {
-  return false;
-}
|}]

let%expect_test "parse git diff headers and no-newline markers" =
  print_patch (parse git_diff_with_marker);
  [%expect_exact {|@@ -1,2 +1,2 @@
 one
-two
+too
|}]

let%expect_test "compute patch from strings with empty old side" =
  let patch = Patch.of_strings ~old:"" ~new_:"alpha\nbeta\n" ~context:3 () in
  print_patch patch;
  [%expect_exact {|@@ -0,0 +1,2 @@
+alpha
+beta
|}]

let%expect_test "unified view simple diff" =
  render ~width:60 ~height:5
    (Vnode.diff ~layout:Diff.Unified (parse simple_diff));
  [%expect_exact
    {|
 1   function hello() {
 2 -   console.log("Hello");
 2 +   console.log("Hello, World!");
 3   }
|}]

let%expect_test "split view simple diff" =
  render ~width:80 ~height:5 (Vnode.diff ~layout:Diff.Split (parse simple_diff));
  [%expect_exact
    {|
 1   function hello() {                  1   function hello() {
 2 -   console.log("Hello");             2 +   console.log("Hello, World!");
 3   }                                   3   }

|}]

let%expect_test "source line highlights preserve diff text" =
  let line_highlights = [ line_highlight Diff.New 2 2 ] in
  render ~width:60 ~height:5
    (Vnode.diff ~layout:Diff.Unified ~line_highlights (parse simple_diff));
  render ~width:80 ~height:5
    (Vnode.diff ~layout:Diff.Split ~line_highlights (parse simple_diff));
  let selected = selected_line_color.gutter in
  let unified =
    grid_of_vnode ~width:60 ~height:5
      (Vnode.diff ~layout:Diff.Unified ~line_highlights (parse simple_diff))
  in
  assert_not_background ~msg:"unified old line is not selected" unified ~x:8
    ~y:1 selected;
  assert_background ~msg:"unified selected gutter" unified ~x:1 ~y:2 selected;
  assert_background ~msg:"unified selected content" unified ~x:8 ~y:2 selected;
  let split =
    grid_of_vnode ~width:80 ~height:5
      (Vnode.diff ~layout:Diff.Split ~line_highlights (parse simple_diff))
  in
  assert_not_background ~msg:"split old side is not selected" split ~x:8 ~y:1
    selected;
  assert_background ~msg:"split selected gutter" split ~x:41 ~y:1 selected;
  assert_background ~msg:"split selected content" split ~x:48 ~y:1 selected;
  [%expect_exact
    {|
 1   function hello() {
 2 -   console.log("Hello");
 2 +   console.log("Hello, World!");
 3   }

 1   function hello() {                  1   function hello() {
 2 -   console.log("Hello");             2 +   console.log("Hello, World!");
 3   }                                   3   }

|}]

let%expect_test "set source line highlights rebuilds colors" =
  let selected = selected_line_color.gutter in
  let renderer = Renderer.create () in
  set_viewport renderer ~width:60 ~height:5;
  let diff =
    Diff.create ~parent:(Renderer.root renderer) ~layout:Diff.Unified
      (parse simple_diff)
  in
  fill_node (Diff.node diff);
  Renderer.render_frame renderer ~width:60 ~height:5 ~delta:0.;
  let initial = Screen.next_grid (Renderer.screen renderer) in
  assert_not_background ~msg:"initial line is not selected" initial ~x:8 ~y:2
    selected;
  Diff.set_line_highlights diff [ line_highlight Diff.New 2 2 ];
  Renderer.render_frame renderer ~width:60 ~height:5 ~delta:0.;
  let highlighted = Screen.next_grid (Renderer.screen renderer) in
  assert_background ~msg:"updated selected content" highlighted ~x:8 ~y:2
    selected;
  [%expect_exact {||}]

let%expect_test "source line rows" =
  let patch = parse multi_line_diff in
  print_source_line_row patch ~layout:Diff.Unified
    { side = Diff.New; line = 10 };
  print_source_line_row patch ~layout:Diff.Split { side = Diff.New; line = 10 };
  print_source_line_row patch ~layout:Diff.Unified { side = Diff.Old; line = 6 };
  print_source_line_row patch ~layout:Diff.Split { side = Diff.Old; line = 6 };
  print_source_line_row patch ~layout:Diff.Split { side = Diff.New; line = 99 };
  [%expect_exact {|row=10
row=9
row=9
row=9
none
|}]

let%expect_test "split view aligns asymmetric wrapped lines" =
  let app = make_app () in
  reconcile app
    (Vnode.diff ~layout:Diff.Split ~wrap:`Char (parse asymmetric_wrap_diff));
  set_viewport app.renderer ~width:44 ~height:7;
  Renderer.render_frame app.renderer ~width:44 ~height:7 ~delta:0.;
  ignore (Renderer.render app.renderer : string);
  frame app ~width:44 ~height:7;
  [%expect_exact
    {|
 1   start             1   start
 2 - short             2 + abcdefghijklmnopq
                           rstuvwxyz01234567
                           89
 3   end               3   end

|}]

let%expect_test "split view aligns highlighted asymmetric wrapped lines" =
  let app = make_app () in
  reconcile app
    (Vnode.diff ~layout:Diff.Split ~wrap:`Char ~highlight:plain_highlight
       (parse asymmetric_wrap_diff));
  set_viewport app.renderer ~width:44 ~height:7;
  Renderer.render_frame app.renderer ~width:44 ~height:7 ~delta:0.;
  ignore (Renderer.render app.renderer : string);
  frame app ~width:44 ~height:7;
  [%expect_exact
    {|
 1   start             1   start
 2 - short             2 + abcdefghijklmnopq
                           rstuvwxyz01234567
                           89
 3   end               3   end

|}]

let%expect_test "unified view multi-line diff" =
  render ~width:60 ~height:12
    (Vnode.diff ~layout:Diff.Unified (parse multi_line_diff));
  [%expect_exact
    {|
  1   function add(a, b) {
  2     return a + b;
  3   }
  4
  5 + function subtract(a, b) {
  6 +   return a - b;
  7 + }
  8 +
  9   function multiply(a, b) {
  6 -   return a * b;
 10 +   return a * b * 1;
 11   }|}]

let%expect_test "split view multi-line diff" =
  render ~width:80 ~height:12
    (Vnode.diff ~layout:Diff.Split (parse multi_line_diff));
  [%expect_exact
    {|
  1   function add(a, b) {                1   function add(a, b) {
  2     return a + b;                     2     return a + b;
  3   }                                   3   }
  4                                       4
                                          5 + function subtract(a, b) {
                                          6 +   return a - b;
                                          7 + }
                                          8 +
  5   function multiply(a, b) {           9   function multiply(a, b) {
  6 -   return a * b;                    10 +   return a * b * 1;
  7   }                                  11   }
|}]

let%expect_test "add-only unified and split views" =
  render ~width:60 ~height:4
    (Vnode.diff ~layout:Diff.Unified (parse add_only_diff));
  render ~width:80 ~height:4
    (Vnode.diff ~layout:Diff.Split (parse add_only_diff));
  [%expect_exact
    {|
 1 + function newFunction() {
 2 +   return true;
 3 + }

                                         1 + function newFunction() {
                                         2 +   return true;
                                         3 + }
|}]

let%expect_test "remove-only unified and split views" =
  render ~width:60 ~height:4
    (Vnode.diff ~layout:Diff.Unified (parse remove_only_diff));
  render ~width:80 ~height:4
    (Vnode.diff ~layout:Diff.Split (parse remove_only_diff));
  [%expect_exact
    {|
 1 - function oldFunction() {
 2 -   return false;
 3 - }

 1 - function oldFunction() {
 2 -   return false;
 3 - }
|}]

let%expect_test "unified view without line numbers" =
  render ~width:60 ~height:5
    (Vnode.diff ~layout:Diff.Unified ~show_line_numbers:false
       (parse simple_diff));
  [%expect_exact
    {|
function hello() {
  console.log("Hello");
  console.log("Hello, World!");
}
|}]

let%expect_test "stable reconciler update from unified to split" =
  let app = make_app () in
  reconcile app (Vnode.diff ~layout:Diff.Unified (parse simple_diff));
  frame app ~width:60 ~height:5;
  ignore (Renderer.render app.renderer : string);
  reconcile app (Vnode.diff ~layout:Diff.Split (parse simple_diff));
  frame app ~width:80 ~height:5;
  [%expect_exact
    {|
 1   function hello() {
 2 -   console.log("Hello");
 2 +   console.log("Hello, World!");
 3   }

 1   function hello() {                  1   function hello() {
 2 -   console.log("Hello");             2 +   console.log("Hello, World!");
 3   }                                   3   }

|}]
