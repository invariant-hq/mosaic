open Mosaic_ui
open Expect_harness

let items =
  Tree.
    [
      item ~id:"src" "src"
        ~children:
          [
            item ~id:"main.ml" "main.ml";
            item ~id:"utils.ml" "utils.ml";
            item ~id:"lib" "lib"
              ~children:
                [
                  item ~id:"parser.ml" "parser.ml";
                  item ~id:"lexer.ml" "lexer.ml";
                ];
          ];
      item ~id:"test" "test"
        ~children:[ item ~id:"test_main.ml" "test_main.ml" ];
      item ~id:"README.md" "README.md";
    ]

let tree ?(items = items) ?(expanded = []) ?(selected = "src") ?show_guides () =
  Mosaic.tree ~items
    ~state:(Tree.State.make ~selected ~expanded ())
    ~on_change:ignore ?show_guides ()

let mount_tree app ?(expanded = []) () =
  let state = ref (Tree.State.make ~selected:"src" ~expanded ()) in
  let refresh () =
    reconcile app
      (Mosaic.tree ~items ~state:!state
         ~on_change:(fun next -> state := next)
         ())
  in
  refresh ();
  focus app (mounted app);
  refresh

(* ── Static rendering ── *)

let%expect_test "all collapsed (default)" =
  render ~width:30 ~height:8 (tree ~items ());
  [%expect_exact {|
▶ src
▶ test
  README.md




|}]

let%expect_test "explicit first-level expansion expands first level" =
  render ~width:30 ~height:10 (tree ~items ~expanded:[ "src"; "test" ] ());
  [%expect_exact
    {|
▼ src
    main.ml
    utils.ml
  ▶ lib
▼ test
    test_main.ml
  README.md


|}]

let%expect_test "explicit full expansion expands everything" =
  render ~width:30 ~height:12
    (tree ~items ~expanded:[ "src"; "test"; "lib" ] ());
  [%expect_exact
    {|
▼ src
    main.ml
    utils.ml
  ▼ lib
      parser.ml
      lexer.ml
▼ test
    test_main.ml
  README.md


|}]

let%expect_test "selected logical ID" =
  render ~width:30 ~height:10
    (tree ~items ~expanded:[ "src"; "test" ] ~selected:"utils.ml" ());
  [%expect_exact
    {|
▼ src
    main.ml
    utils.ml
  ▶ lib
▼ test
    test_main.ml
  README.md


|}]

let%expect_test "show_guides with explicit full expansion" =
  render ~width:30 ~height:12
    (tree ~items ~expanded:[ "src"; "test"; "lib" ] ~show_guides:true ());
  [%expect_exact
    {|
▼ src
├─  main.ml
├─  utils.ml
└─▼ lib
│ ├─  parser.ml
│ └─  lexer.ml
▼ test
└─  test_main.ml
  README.md


|}]

let%expect_test "show_guides with explicit first-level expansion" =
  render ~width:30 ~height:10
    (tree ~items ~expanded:[ "src"; "test" ] ~show_guides:true ());
  [%expect_exact
    {|
▼ src
├─  main.ml
├─  utils.ml
└─▶ lib
▼ test
└─  test_main.ml
  README.md


|}]

(* ── Interactive rendering ── *)

let%expect_test "navigation Down moves selection" =
  let app = make_app () in
  let refresh = mount_tree app ~expanded:[ "src"; "test" ] () in
  frame app ~width:30 ~height:10;
  send_key app Matrix_input.Keystroke.Down;
  refresh ();
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
▼ src
    main.ml
    utils.ml
  ▶ lib
▼ test
    test_main.ml
  README.md



▼ src
    main.ml
    utils.ml
  ▶ lib
▼ test
    test_main.ml
  README.md


|}]

let%expect_test "Right expands collapsed node" =
  let app = make_app () in
  let refresh = mount_tree app () in
  frame app ~width:30 ~height:10;
  send_key app Matrix_input.Keystroke.Right;
  refresh ();
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
▶ src
▶ test
  README.md







▼ src
    main.ml
    utils.ml
  ▶ lib
▶ test
  README.md



|}]

let%expect_test "Left collapses expanded node" =
  let app = make_app () in
  let refresh = mount_tree app ~expanded:[ "src"; "test" ] () in
  frame app ~width:30 ~height:10;
  send_key app Matrix_input.Keystroke.Left;
  refresh ();
  frame app ~width:30 ~height:10;
  [%expect_exact
    {|
▼ src
    main.ml
    utils.ml
  ▶ lib
▼ test
    test_main.ml
  README.md



▶ src
▼ test
    test_main.ml
  README.md





|}]

let%expect_test "Space toggles expand" =
  let app = make_app () in
  let refresh = mount_tree app () in
  frame app ~width:30 ~height:8;
  send_char app ' ';
  refresh ();
  frame app ~width:30 ~height:8;
  send_char app ' ';
  refresh ();
  frame app ~width:30 ~height:8;
  [%expect_exact
    {|
▶ src
▶ test
  README.md





▼ src
    main.ml
    utils.ml
  ▶ lib
▶ test
  README.md


▶ src
▶ test
  README.md




|}]

let%expect_test "deep nesting with guides" =
  let deep =
    Tree.
      [
        item ~id:"a" "a"
          ~children:
            [
              item ~id:"b" "b"
                ~children:
                  [
                    item ~id:"c" "c"
                      ~children:
                        [ item ~id:"d" "d" ~children:[ item ~id:"e" "e" ] ];
                  ];
            ];
      ]
  in
  render ~width:30 ~height:8
    (tree ~items:deep ~expanded:[ "a"; "b"; "c"; "d" ] ~show_guides:true ());
  [%expect_exact {|
▼ a
└─▼ b
  └─▼ c
    └─▼ d
      └─  e


|}]
