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

(* A tab select over [sample_options], each reporting its position. *)
let tab_select ?(selected = 0) ?(on_change = ignore) ?show_description
    ?show_underline ?tab_width ?wrap_selection () =
  Mosaic.tab_select ?show_description ?show_underline ?tab_width ?wrap_selection
    ~equal:Int.equal ~selected:(Some selected) ~on_change
    (List.mapi
       (fun i { Tab_select.label; description } ->
         Mosaic.Tab_select.item i ~description label)
       sample_options)

(* ── Static rendering ── *)

let%expect_test "default tab_select renders tabs" =
  render ~width:60 ~height:3 (tab_select ());
  [%expect
    {|
    ›Home        Files       Settings    Help
    ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "selected:0 highlights first tab" =
  render ~width:60 ~height:3 (tab_select ~selected:0 ());
  [%expect
    {|
    ›Home        Files       Settings    Help
    ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "selected:2 highlights third tab" =
  render ~width:60 ~height:3 (tab_select ~selected:2 ());
  [%expect
    {|
    Home        Files      ›Settings    Help
                           ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "show_description shows description line" =
  render ~width:60 ~height:4 (tab_select ~show_description:true ());
  [%expect
    {|
    ›Home        Files       Settings    Help
    ▬▬▬▬▬▬▬▬▬▬▬▬
    Main dashboard
    |}]

let%expect_test "show_underline:false hides underline" =
  render ~width:60 ~height:3 (tab_select ~show_underline:false ());
  [%expect {| ›Home        Files       Settings    Help |}]

let%expect_test "custom tab_width changes column size" =
  render ~width:80 ~height:3 (tab_select ~tab_width:20 ());
  [%expect
    {|
    ›Home                Files               Settings            Help
    ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

(* ── Interactive — keyboard navigation ── *)

let%expect_test "left arrow at start stays" =
  let app = make_app () in
  let selected = ref 0 in
  let view () =
    tab_select ~selected:!selected
      ~on_change:(fun index -> selected := index)
      ()
  in
  reconcile app (view ());
  focus app (mounted app);
  send_key app Matrix_input.Keystroke.Left;
  reconcile app (view ());
  frame app ~width:60 ~height:3;
  [%expect
    {|
    ❯Home        Files       Settings    Help
    ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "wrap_selection wraps right to first" =
  let app = make_app () in
  let selected = ref 3 in
  let view () =
    tab_select ~selected:!selected
      ~on_change:(fun index -> selected := index)
      ~wrap_selection:true ()
  in
  reconcile app (view ());
  focus app (mounted app);
  send_key app Matrix_input.Keystroke.Right;
  reconcile app (view ());
  frame app ~width:60 ~height:3;
  [%expect
    {|
    ❯Home        Files       Settings    Help
    ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "wrap_selection wraps left to last" =
  let app = make_app () in
  let selected = ref 0 in
  let view () =
    tab_select ~selected:!selected
      ~on_change:(fun index -> selected := index)
      ~wrap_selection:true ()
  in
  reconcile app (view ());
  focus app (mounted app);
  send_key app Matrix_input.Keystroke.Left;
  reconcile app (view ());
  frame app ~width:60 ~height:3;
  [%expect
    {|
    Home        Files       Settings   ❯Help
                                       ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

(* ── Scroll arrows ── *)

let%expect_test "scroll arrows appear when tabs overflow" =
  render ~width:30 ~height:3 (tab_select ~tab_width:12 ());
  [%expect {|
    ›Home        Files           ›
    ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

let%expect_test "narrow width shows scroll arrow" =
  render ~width:30 ~height:3 (tab_select ~tab_width:12 ~selected:3 ());
  [%expect {|
    ‹Settings   ›Help
                ▬▬▬▬▬▬▬▬▬▬▬▬
    |}]

(* ── ANSI styling ── *)

let%expect_test "focused tab_select has styled output" =
  let app = make_app () in
  reconcile app (tab_select ());
  focus app (mounted app);
  frame_ansi app ~width:60 ~height:3;
  [%expect
    {|
    [0;1;4;7m❯Home       [22;24;27m Files       Settings    Help                   [0m
    [0m▬▬▬▬▬▬▬▬▬▬▬▬                                                [0m
    [0m                                                            [0m
    |}]
