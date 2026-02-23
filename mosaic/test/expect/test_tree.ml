open Mosaic_ui
open Expect_harness

let items =
  Tree.
    [
      item "src"
        ~children:
          [
            item "main.ml";
            item "utils.ml";
            item "lib" ~children:[ item "parser.ml"; item "lexer.ml" ];
          ];
      item "test" ~children:[ item "test_main.ml" ];
      item "README.md";
    ]

(* ── Static rendering ── *)

let%expect_test "all collapsed (default)" =
  render ~width:30 ~height:8 (Vnode.tree ~items ());
  [%expect_exact {|
▶ src
▶ test
  README.md




|}]

let%expect_test "expand_depth:1 expands first level" =
  render ~width:30 ~height:10 (Vnode.tree ~items ~expand_depth:1 ());
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

let%expect_test "expand_depth:-1 expands everything" =
  render ~width:30 ~height:12 (Vnode.tree ~items ~expand_depth:(-1) ());
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

let%expect_test "selected_index:2" =
  render ~width:30 ~height:10
    (Vnode.tree ~items ~expand_depth:1 ~selected_index:2 ());
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

let%expect_test "show_guides with expand_depth:-1" =
  render ~width:30 ~height:12
    (Vnode.tree ~items ~expand_depth:(-1) ~show_guides:true ());
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

let%expect_test "show_guides with expand_depth:1" =
  render ~width:30 ~height:10
    (Vnode.tree ~items ~expand_depth:1 ~show_guides:true ());
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
  let node = ref None in
  reconcile app
    (Vnode.tree ~ref:(fun n -> node := Some n) ~items ~expand_depth:1 ());
  focus app (Option.get !node);
  frame app ~width:30 ~height:10;
  send_key app Input.Key.Down;
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
  let node = ref None in
  reconcile app (Vnode.tree ~ref:(fun n -> node := Some n) ~items ());
  focus app (Option.get !node);
  frame app ~width:30 ~height:10;
  send_key app Input.Key.Right;
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
  let node = ref None in
  reconcile app
    (Vnode.tree ~ref:(fun n -> node := Some n) ~items ~expand_depth:1 ());
  focus app (Option.get !node);
  frame app ~width:30 ~height:10;
  send_key app Input.Key.Left;
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
  let node = ref None in
  reconcile app (Vnode.tree ~ref:(fun n -> node := Some n) ~items ());
  focus app (Option.get !node);
  frame app ~width:30 ~height:8;
  send_char app ' ';
  frame app ~width:30 ~height:8;
  send_char app ' ';
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
        item "a"
          ~children:
            [
              item "b"
                ~children:
                  [ item "c" ~children:[ item "d" ~children:[ item "e" ] ] ];
            ];
      ]
  in
  render ~width:30 ~height:8
    (Vnode.tree ~items:deep ~expand_depth:(-1) ~show_guides:true ());
  [%expect_exact {|
▼ a
└─▼ b
  └─▼ c
    └─▼ d
      └─  e


|}]
