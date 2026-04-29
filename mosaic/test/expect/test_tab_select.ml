open Mosaic_ui
open Expect_harness

(* ── Helpers ── *)

let sample_options =
  Tab_select.
    [
      { label = "Home"; description = "Main dashboard" };
      { label = "Files"; description = "Browse files" };
      { label = "Settings"; description = "Configure options" };
      { label = "Help"; description = "Documentation" };
    ]

(* ── Static rendering ── *)

let%expect_test "default tab_select renders tabs" =
  render ~width:60 ~height:3 (Vnode.tab_select ~options:sample_options ());
  [%expect {| Home        Files       Settings    Help
▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "selected:0 highlights first tab" =
  render ~width:60 ~height:3
    (Vnode.tab_select ~options:sample_options ~selected:0 ());
  [%expect {| Home        Files       Settings    Help
▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "selected:2 highlights third tab" =
  render ~width:60 ~height:3
    (Vnode.tab_select ~options:sample_options ~selected:2 ());
  [%expect
    {|Home        Files       Settings    Help
                       ▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "show_description shows description line" =
  render ~width:60 ~height:4
    (Vnode.tab_select ~options:sample_options ~show_description:true ());
  [%expect
    {| Home        Files       Settings    Help
▬▬▬▬▬▬▬▬▬▬▬▬
Main dashboard|}]

let%expect_test "show_underline:false hides underline" =
  render ~width:60 ~height:3
    (Vnode.tab_select ~options:sample_options ~show_underline:false ());
  [%expect {|Home        Files       Settings    Help|}]

let%expect_test "custom tab_width changes column size" =
  render ~width:80 ~height:3
    (Vnode.tab_select ~options:sample_options ~tab_width:20 ());
  [%expect
    {| Home                Files               Settings            Help
▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬|}]

(* ── Interactive — keyboard navigation ── *)

let%expect_test "left arrow at start stays" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.tab_select ~options:sample_options ~selected:0
       ~ref:(fun n -> node := Some n)
       ());
  focus app (Option.get !node);
  send_key app Input.Key.Left;
  frame app ~width:60 ~height:3;
  [%expect {| Home        Files       Settings    Help
▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "wrap_selection wraps right to first" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.tab_select ~options:sample_options ~selected:3 ~wrap_selection:true
       ~ref:(fun n -> node := Some n)
       ());
  focus app (Option.get !node);
  send_key app Input.Key.Right;
  frame app ~width:60 ~height:3;
  [%expect {| Home        Files       Settings    Help
▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "wrap_selection wraps left to last" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.tab_select ~options:sample_options ~selected:0 ~wrap_selection:true
       ~ref:(fun n -> node := Some n)
       ());
  focus app (Option.get !node);
  send_key app Input.Key.Left;
  frame app ~width:60 ~height:3;
  [%expect
    {|Home        Files       Settings    Help
                                   ▬▬▬▬▬▬▬▬▬▬▬▬|}]

(* ── Scroll arrows ── *)

let%expect_test "scroll arrows appear when tabs overflow" =
  render ~width:30 ~height:3
    (Vnode.tab_select ~options:sample_options ~tab_width:12 ());
  [%expect {| Home        Files           ›
▬▬▬▬▬▬▬▬▬▬▬▬|}]

let%expect_test "narrow width shows scroll arrow" =
  render ~width:30 ~height:3
    (Vnode.tab_select ~options:sample_options ~tab_width:12 ~selected:3 ());
  [%expect {|‹Settings    Help
            ▬▬▬▬▬▬▬▬▬▬▬▬|}]

(* ── ANSI styling ── *)

let%expect_test "focused tab_select has styled output" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.tab_select ~options:sample_options ~ref:(fun n -> node := Some n) ());
  focus app (Option.get !node);
  frame_ansi app ~width:60 ~height:3;
  [%expect
    {|[0;38;2;255;255;255;48;2;59;130;246m [0;38;5;7;48;2;59;130;246mHome[0;38;2;255;255;255;48;2;59;130;246m       [0;38;2;255;255;255;48;2;26;26;26m [0;38;2;226;232;240;48;2;26;26;26mFiles[0;38;2;255;255;255;48;2;26;26;26m       [0;38;2;226;232;240;48;2;26;26;26mSettings[0;38;2;255;255;255;48;2;26;26;26m    [0;38;2;226;232;240;48;2;26;26;26mHelp[0;38;2;255;255;255;48;2;26;26;26m                   [0m
[0;38;5;7;48;2;59;130;246m▬▬▬▬▬▬▬▬▬▬▬▬[0;38;2;255;255;255;48;2;26;26;26m                                                [0m
[0;38;2;255;255;255;48;2;26;26;26m                                                            [0m|}]
