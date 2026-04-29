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
