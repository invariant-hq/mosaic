module Grid = Matrix_grid
module C = Matrix_charts

let show ?(width = 30) ?(height = 10) ?theme ?view ?depth ?overlays fig =
  let g = Grid.create ~width ~height () in
  let l = C.layout ?theme ?view ?depth ~width ~height fig in
  C.draw ?overlays l g ~x:0 ~y:0;
  print_endline (Grid.to_text g)

let show_ansi ?(width = 30) ?(height = 10) ?theme ?depth fig =
  let g = Grid.create ~width ~height () in
  C.draw (C.layout ?theme ?depth ~width ~height fig) g ~x:0 ~y:0;
  print_endline (Grid.to_ansi g)

let bare fig = C.(layer [ fig; axis ~show:false "x"; axis ~show:false "y" ])
let points = [| (0.0, 0.0); (1.0, 2.0); (2.0, 1.0); (3.0, 3.0) |]

let wave n =
  Array.init n (fun i -> (Float.of_int i, sin (Float.of_int i /. 3.)))

(* {1 Lines} *)

let%expect_test "a line with its axes" =
  show C.(line ~x:fst ~y:snd points);
  [%expect
    {|
        │                       ⢠⠊
    2.5 ┤                      ⡔⠁
        │       ⢀⢄⡀          ⢠⠊
        │      ⡠⠃ ⠈⠒⢄⡀      ⡔⠁
        │    ⢀⠜      ⠈⠑⠤⡀ ⢠⠊
        │   ⡠⠃          ⠈⠑⠁
        │ ⢀⠜
    0.0 ┤⡠⠃
        └┬───────┬───────┬───────┬
         0       1       2       3
    |}]

let%expect_test "a line at each resolution" =
  List.iter
    (fun dots ->
      show ~width:16 ~height:4 (bare C.(line ~dots ~x:fst ~y:snd points));
      print_endline "--")
    [ `Cell; `Steps; `Half; `Quadrant; `Sextant; `Braille; `Octant ];
  [%expect
    {|
                  ╱╱
        ╱───    ╱╱
      ╱╱    ──╱╱
    ╱╱
    --
                   │
         ┌────┐    │
         │    └────┘
    ─────┘
    --
                  ▄▀
        ▄▀▄▄    ▄▀
      ▄▀    ▀▀▄▀
    ▄▀
    --
                  ▄▀
       ▗▞▀▄▄    ▄▀
     ▗▞▘    ▀▀▄▀
    ▞▘
    --
                  🬖🬅
       🬞🬖🬂🬋🬢🬏   🬖🬂
      🬖🬀    🬁🬂🬋🬂
    🬖🬂
    --
                  ⡠⠊
        ⡠⠒⠤⣀    ⡠⠊
      ⡠⠊    ⠉⠒⠤⠊
    ⡠⠊
    --
                  𜵑𜴁
        𜵑𜴆𜴧▂    𜵑𜴁
      𜵑𜴁    🮂𜴆𜴧𜴁
    𜵑𜴁
    --
    |}]

let%expect_test "a line without x draws its rows in order" =
  show ~width:12 ~height:3 (bare C.(line ~y:Fun.id [| 0.; 1.; 0.; 1. |]));
  [%expect {|
       ⡰⠱⡀    ⢀⠎
     ⢀⠎  ⠈⢆  ⢠⠃
    ⡰⠁     ⠱⡰⠁
    |}]

let%expect_test "missing values break a line" =
  let gaps =
    [|
      (0., Some 1.);
      (1., Some 2.);
      (2., None);
      (3., None);
      (4., Some 1.5);
      (5., Some 2.5);
    |]
  in
  show ~width:20 ~height:5 (bare C.(line_gaps ~dots:`Cell ~x:fst ~y:snd gaps));
  show ~width:20 ~height:5
    (bare
       C.(
         line ~dots:`Cell ~x:fst ~y:snd
           [| (0., 1.); (1., 2.); (2., Float.nan); (3., 1.5); (4., 2.5) |]));
  [%expect
    {|
                       ╱
        ╱             ╱
      ╱╱             ╱
     ╱              ╱
    ╱
                       ╱
         ╱           ╱╱
       ╱╱           ╱
     ╱╱            ╱
    ╱
    |}]

let%expect_test "dashed and dotted lines" =
  show ~width:20 ~height:2
    (bare C.(line ~dots:`Cell ~dash:`Dashed ~y:Fun.id [| 1.; 1. |]));
  show ~width:20 ~height:2 (bare C.(line ~dash:`Dotted ~y:Fun.id [| 1.; 1. |]));
  [%expect {|
    ┄┄┄┄   ┄┄┄┄   ┄┄┄┄

    ⡀⢀ ⡀⢀ ⡀⢀ ⡀⢀ ⡀⢀ ⡀⢀ ⡀⢀
    |}]

(* {1 Dots, areas, rects} *)

let%expect_test "dots as glyphs, braille dots and density" =
  let pts =
    Array.init 40 (fun i -> (Float.of_int (i mod 8), Float.of_int (i / 8)))
  in
  show ~width:16 ~height:5
    (bare C.(dot ~dots:`Cell ~symbol:"x" ~x:fst ~y:snd pts));
  show ~width:16 ~height:5 (bare C.(dot ~dots:`Braille ~x:fst ~y:snd pts));
  let cloud =
    Array.init 60 (fun i -> (Float.of_int (i mod 3), Float.of_int (i mod 2)))
  in
  show ~width:6 ~height:2 (bare C.(dot ~dots:`Density ~x:fst ~y:snd cloud));
  [%expect
    {|
    x x x x  x x x x
    x x x x  x x x x
    x x x x  x x x x
    x x x x  x x x x
    x x x x  x x x x
    ⠁ ⠁ ⠈ ⠈  ⠁ ⠁ ⠈ ⠈
    ⠁ ⠁ ⠈ ⠈  ⠁ ⠁ ⠈ ⠈
    ⠂ ⠂ ⠐ ⠐  ⠂ ⠂ ⠐ ⠐
    ⠄ ⠄ ⠠ ⠠  ⠄ ⠄ ⠠ ⠠
    ⡀ ⡀ ⢀ ⢀  ⡀ ⡀ ⢀ ⢀
    █  █ █
    █  █ █
    |}]

let%expect_test "an area fills to zero and between two series" =
  show_ansi ~width:12 ~height:4
    (bare
       C.(area ~color:(const Ansi.Color.blue) ~y:Fun.id [| 1.; 3.; 2.; 4. |]));
  show ~width:12 ~height:4
    (bare
       C.(area ~dots:`Braille ~y2:(fun _ -> 1.) ~y:Fun.id [| 2.; 3.; 2.; 4. |]));
  [%expect
    {|
    [0m    [48;5;4m [49m     [48;5;4m  [0m
    [0m  [48;5;4m          [0m
    [0;48;5;4m            [0m
    [0;48;5;4m            [0m
              ⢀⣾
      ⣠⣴⣦⣄   ⣰⣿⣿
    ⣴⣿⣿⣿⣿⣿⣷⣦⣾⣿⣿⣿
    ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
    |}]

let%expect_test "rects shade an x interval and one column" =
  show_ansi ~width:12 ~height:2
    (bare
       C.(
         layer
           [
             line ~y:Fun.id (Array.make 12 1.);
             rect ~color:(const Ansi.Color.red) ~x:fst ~x2:snd [| (2., 4.) |];
             rect ~color:(const Ansi.Color.green) ~x:Fun.id ~x2:Fun.id [| 8. |];
           ]));
  [%expect
    {|
    [0;38;5;6m⣀⣀[48;5;1m⣀⣀⣀[49m⣀⣀⣀[48;5;2m⣀[49m⣀⣀⣀[0m
    [0m  [48;5;1m   [49m   [48;5;2m [49m   [0m
    |}]

let hatch = [ "╱   "; "   ╱"; "  ╱ "; " ╱  " ]

let%expect_test "a patterned rect tiles glyphs that later marks cover" =
  let fig =
    C.(
      bare
        (layer
           [
             vline 6.5;
             rect ~pattern:hatch ~x:Fun.id ~x2:(fun _ -> 11.) [| 3. |];
             hline 2.5;
           ])
      |> scale "x" (Scale.linear ~domain:(0., 12.) ())
      |> scale "y" (Scale.linear ~domain:(0., 4.) ()))
  in
  show ~width:12 ~height:4 fig;
  [%expect
    {|
        ╱ │ ╱
    ────────────
          ╱   ╱
         ╱│  ╱
    |}];
  let l =
    C.layout ~width:12 ~height:4
      C.(rect ~pattern:[ "  "; " ▚" ] ~color:(series "gap") ~x:Fun.id [| 1. |])
  in
  List.iter (fun (c, _, sw) -> Printf.printf "%s %s\n" sw c) (C.Layout.legend l);
  [%expect {| ▚ gap |}];
  List.iter
    (fun pattern ->
      try ignore (C.rect ~pattern ~x:Fun.id [| 1. |])
      with Invalid_argument m -> print_endline m)
    [ []; [ "╱"; "" ] ];
  List.iter
    (fun pattern ->
      try
        ignore
          (C.layout ~width:10 ~height:4 (C.rect ~pattern ~x:Fun.id [| 1. |]))
      with Invalid_argument m -> print_endline m)
    [ [ "中 " ]; [ "\t" ] ];
  [%expect
    {|
    Matrix_charts.rect: pattern has no rows
    Matrix_charts.rect: pattern has an empty row
    Matrix_charts.layout: pattern glyph "\228\184\173" takes 2 cells
    Matrix_charts.layout: pattern glyph "\t" takes 0 cells
    |}]

(* {1 Bars} *)

let quarters = [| ("Q1", 30.0); ("Q2", 45.0); ("Q3", 25.0); ("Q4", 50.0) |]

let%expect_test "vertical and horizontal bars" =
  show ~width:24 ~height:8 C.(bar_y ~x:fst ~y:snd quarters);
  show ~width:24 ~height:6 C.(bar_x ~y:fst ~x:snd quarters);
  show ~width:24 ~height:6 C.(bar_y ~dots:`Cell ~x:fst ~y:snd quarters);
  [%expect
    {|
    50 ┤      ▃▃▃▃      ████
       │      ████      ████
       │ ▅▅▅▅ ████      ████
       │ ████ ████ ████ ████
       │ ████ ████ ████ ████
     0 ┤ ████ ████ ████ ████
       └──┬────┬────┬────┬──
          Q1   Q2   Q3   Q4
    Q1 ┤███████████▍
       │█████████████████▏
    Q3 ┤█████████▌
       │███████████████████
       └┬─────────────────┬
        0                 50
    50 ┤      ████      ████
       │      ████      ████
       │ ████ ████ ████ ████
     0 ┤ ████ ████ ████ ████
       └──┬────┬────┬────┬──
          Q1   Q2   Q3   Q4
    |}]

let%expect_test "stacked bars with labels inside and diverging values" =
  let rows =
    [|
      ("A", "in", 4.);
      ("A", "out", 3.);
      ("B", "in", 2.);
      ("B", "out", -2.);
      ("B", "rest", 5.);
    |]
  in
  let cat3 (c, _, _) = c and seg (_, s, _) = s and v (_, _, x) = x in
  show ~width:24 ~height:12
    C.(
      layer
        [
          bar_y ~color:(cat seg) ~label:seg ~x:cat3 ~y:v rows;
          legend ~side:`Right "color";
        ]);
  [%expect
    {|
      │ █████  █████  █ in
      │ █out█  █████  █ out
    5 ┤ █████  rest█  █ rest
      │ ▅▅▅▅▅  █████
      │ █████  █████
      │ █in██  ▄▄▄▄▄
      │ █████  █in██
    0 ┤ 🮅🮅🮅🮅🮅  🮅🮅🮅🮅🮅
      │        █████
      │        █out█
      └───┬──────┬───
          A      B
    |}]

let%expect_test "a band's step and padding set bar width and gap in cells" =
  show ~width:20 ~height:4
    (bare
       C.(
         bar_y ~x:fst ~y:snd quarters
         |> scale "x" (Scale.band ~step:3 ~padding:(1. /. 3.) ())));
  [%expect
    {|
       ▅▅    ██
    ▃▃ ██    ██
    ██ ██ ██ ██
    ██ ██ ██ ██
    |}]

(* {1 Rules, text, arrows, circles, candles, histograms} *)

let%expect_test "rules extend the domain" =
  show ~width:20 ~height:6
    C.(layer [ line ~y:Fun.id [| 1.; 2. |]; hline ~dash:`Dashed 5.; vline 3. ]);
  [%expect
    {|
    5 ┤┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄│
    3 ┤                │
      │     ⣀          │
    1 ┤⣀⠤⠔⠊⠉           │
      └┬─────────────┬──
      0.0           2.5
    |}]

let%expect_test "text anchors and arrow heads" =
  show ~width:24 ~height:6
    (bare
       C.(
         layer
           [
             text ~anchor:`Left ~x:fst ~y:snd
               ~text:(fun _ -> "L")
               [| (0., 4.) |];
             text ~anchor:`Center ~x:fst ~y:snd
               ~text:(fun _ -> "mid")
               [| (5., 4.) |];
             text ~anchor:`Right ~x:fst ~y:snd
               ~text:(fun _ -> "R")
               [| (10., 4.) |];
             arrow
               ~x:(fun _ -> 1.)
               ~y:(fun _ -> 0.)
               ~x2:(fun _ -> 8.)
               ~y2:(fun _ -> 0.)
               [| () |];
             arrow ~head:`Dot
               ~x:(fun _ -> 2.)
               ~y:(fun _ -> 1.)
               ~x2:(fun _ -> 2.)
               ~y2:(fun _ -> 3.)
               [| () |];
           ]));
  [%expect
    {|
    L          mid         R
        ●
        │
        │
        │
      ─────────────────→
    |}]

let%expect_test "an offset moves a text from its point, cut at the plot" =
  let label ?anchor offset s (x, y) =
    C.text ?anchor ~offset ~x:fst ~y:snd ~text:(fun _ -> s) [| (x, y) |]
  in
  show ~width:10 ~height:3
    C.(
      bare
        (layer
           [
             vline 5.;
             label (1, -1) "ab" (5., 1.5);
             label ~anchor:`Right (-1, 1) "cd" (5., 1.5);
             label (1, 0) "edge" (8., 1.5);
             label (-1, 0) "out" (11., 1.5);
           ])
      |> scale "x" (Scale.linear ~domain:(0., 10.) ())
      |> scale "y" (Scale.linear ~domain:(0., 3.) ()));
  [%expect {|
         │ab
         │   e
       cd│
    |}]

let%expect_test "a circle is an ellipse in cells" =
  show ~width:24 ~height:8
    (bare
       C.(
         circle ~x:fst ~y:fst ~r:snd [| (0., 1.) |]
         |> scale "x" (Scale.linear ~domain:(-1.5, 1.5) ())
         |> scale "y" (Scale.linear ~domain:(-1.5, 1.5) ())));
  [%expect
    {|
       ⣀⠤⠔⠒⠒⠒⠒⠒⠤⢄⡀
    ⢀⠔⠉          ⠈⠢⡀
    ⡇              ⠱⡀
    ⢇              ⢰⠁
    ⠈⠢⡀          ⣀⠔⠁
      ⠈⠑⠒⠤⠤⠤⠤⠤⠔⠒⠊
    |}]

let%expect_test "candles: wicks, bodies, hollow and width 2" =
  let data =
    C.
      [|
        { time = 0.; open_ = 100.; high = 110.; low = 95.; close = 105. };
        { time = 1.; open_ = 105.; high = 115.; low = 100.; close = 98. };
        { time = 2.; open_ = 98.; high = 105.; low = 92.; close = 102. };
      |]
  in
  let domain = C.scale "x" (C.Scale.linear ~domain:(-0.5, 2.5) ()) in
  show ~width:12 ~height:8 (bare (domain (C.candles data)));
  show ~width:12 ~height:8
    (bare (domain (C.candles ~width:2 ~hollow:true data)));
  [%expect
    {|
        │
    │   │
    │   │
    ┃   ┃   │
    ┃   ┃   ┃
    ┃   ┃   ┃
    │       │
            │
        ▕▏
    ▕▏  ▕▏
    ▕▏  ▕▏
    ▐▌  ██  ▕▏
    ▐▌  ██  ▐▌
    ▐▌  ██  ▐▌
    ▕▏      ▕▏
            ▕▏
    |}]

let%expect_test "histograms by count and by probability" =
  let values = [| 1.; 2.; 2.; 3.; 3.; 3.; 4.; 4.; 5. |] in
  show ~width:24 ~height:6 C.(histogram ~bins:(`Count 5) values);
  show ~width:24 ~height:6
    C.(histogram ~bins:(`Edges [| 0.; 3.; 6. |]) ~height:`Probability values);
  [%expect
    {|
    2.5 ┤       █████
        │   ▅▅▅▅████▅▅▅▅▅
        │▃▃▃████████████▃▃▃▃
    0.0 ┤███████████████████
        └┬────────┬────────┬
         1        3        5
        │         ▆▆▆▆▆▆▆▆▆▆
    0.5 ┤         ██████████
        │▇▇▇▇▇▇▇▇▇██████████
    0.0 ┤███████████████████
        └┬──────────────┬───
         0              5
    |}]

(* {1 Rasters} *)

let field =
  Array.init 16 (fun i ->
      (Float.of_int (i mod 4), Float.of_int (i / 4), Float.of_int i))

let%expect_test "a raster at each resolution" =
  let r dots =
    C.(
      raster ~dots
        ~x:(fun (x, _, _) -> x)
        ~y:(fun (_, y, _) -> y)
        ~value:(fun (_, _, v) -> v)
        field)
  in
  show ~width:8 ~height:4 (bare (r `Shade));
  show ~width:8 ~height:4 (bare (r (`Glyph "#")));
  show_ansi ~width:4 ~height:2 (bare (r `Cell));
  show_ansi ~width:4 ~height:2 (bare (r `Half));
  [%expect
    {|
    █ █  █ █
    ▓ ▓  ▓ ▓
    ▒ ▒  ▒ ▒
    ░ ░  ░ ░
    # #  # #
    # #  # #
    # #  # #
    # #  # #
    [0;48;2;126;209;82m [48;2;168;219;56m [48;2;211;226;41m [48;2;253;231;37m [0m
    [0;48;2;58;86;139m [48;2;49;104;142m [48;2;43;120;142m [48;2;37;136;141m [0m
    [0;38;2;126;209;82;48;2;33;152;138m▀[38;2;168;219;56;48;2;40;168;131m▀[38;2;211;226;41;48;2;53;183;121m▀[38;2;253;231;37;48;2;90;197;103m▀[0m
    [0;38;2;58;86;139;48;2;68;1;84m▀[38;2;49;104;142;48;2;71;27;105m▀[38;2;43;120;142;48;2;71;47;123m▀[38;2;37;136;141;48;2;65;67;134m▀[0m
    |}]

let%expect_test "a raster reduces the rows of a cell and interpolates" =
  let rows =
    [| (0., 0., 1.); (0., 0., 3.); (1., 0., 0.); (0., 1., 4.); (1., 1., 2.) |]
  in
  let r reduce interpolate =
    C.(
      raster ~dots:`Shade ~reduce ~interpolate
        ~x:(fun (x, _, _) -> x)
        ~y:(fun (_, y, _) -> y)
        ~value:(fun (_, _, v) -> v)
        rows
      |> scale "color" (Scale.linear ~domain:(0., 4.) ()))
  in
  show ~width:4 ~height:2 (bare (r `Last `None));
  show ~width:4 ~height:2 (bare (r `Mean `None));
  show ~width:4 ~height:2 (bare (r `Max `None));
  show ~width:4 ~height:2 (bare (r `Max `Bilinear));
  [%expect
    {|
    █  ▓
    █  ░
    █  ▓
    ▓  ░
    █  ▓
    █  ░
    █▓▓▒
    ▓▓▒░
    |}]

(* {1 Sparklines} *)

let%expect_test "a spark draws the newest values that fit, newest at the right"
    =
  let newest_first = [ 6.; 4.; 7.; 2.; 8.; 3.; 5.; 1.; -3. ] in
  show ~width:12 ~height:2 C.(spark newest_first);
  show ~width:5 ~height:2 C.(spark newest_first);
  show ~width:12 ~height:2 C.(spark ~dots:`Braille newest_first);
  show ~width:12 ~height:2
    C.(spark newest_first |> scale "y" (Scale.linear ~domain:(0., 16.) ()));
  [%expect
    {|
         ▂ █ ▆ ▄
        ▂█▆█▄███
    █ ▆ ▄
    █▄███
         ⣄⢠⢣⢠⠳⡠⠂
       ⡠⠜ ⠁ ⠃

        ▁▅▃█▂▇▄▆
    |}]

(* {1 Scales and ticks} *)

let%expect_test "ticks lie in the domain and labels share their decimals" =
  show ~width:40 ~height:3
    C.(
      line ~y:Fun.id [| 0.2; 3.2 |]
      |> scale "x" (Scale.linear ~domain:(0.2, 3.2) ()));
  show ~width:40 ~height:3 C.(line ~x:Fun.id ~y:Fun.id [| 0.; 0.75 |]);
  [%expect
    {|
    2 ┤⣀⣀⠤⠤⠤⠒⠒⠒⠉⠉
      └─────────┬────────────┬───────────┬──
                1            2           3
    0.5 ┤⣀⣀⣀⣀⣀⡠⠤⠤⠤⠤⠤⠤⠤⠤⠤⠤⠔⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠊⠉⠉⠉⠉⠉
        └┬─────────┬──────────┬──────────┬──
        0.00      0.25       0.50       0.75
    |}]

let%expect_test "labels beyond seven digits read against a note" =
  show ~width:50 ~height:6
    C.(line ~x:Fun.id ~y:(fun _ -> 0.) [| 1000000.1; 1000000.5 |]);
  show ~width:50 ~height:6
    C.(line ~x:Fun.id ~y:(fun _ -> 0.) [| 0.; 2000000. |]);
  [%expect
    {|
            │
          0 ┤⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒
            │
            └───────┬────────────────┬───────
                   0.2              0.4
                           +10⁶
      │
    0 ┤⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒
      │
      └┬──────────┬───────────┬──────────┬──────────┬
      0.0        0.5         1.0        1.5        2.0
                            ×10⁶
    |}]

let%expect_test "log scales tick at powers" =
  show ~width:30 ~height:8
    C.(
      line ~x:Fun.id ~y:Fun.id [| 1.; 10.; 100.; 1000. |]
      |> scale "y" (Scale.log ()));
  show ~width:30 ~height:4
    C.(
      line ~x:Fun.id ~y:Fun.id [| 0.; 1.; 8. |]
      |> scale "y" (Scale.log ~base:2. ()));
  [%expect
    {|
    1000 ┤            ⣀⣀⣀⠤⠤⠔⠒⠒⠊⠉
         │  ⣀⡠⠤⠤⠔⠒⠒⠉⠉⠉
         │ ⡰⠁
         │⡰⠁
         │⡇
       1 ┤⡇
         └┬────────────────────┬
          0                   1000
    2³ ┤              ⢀⣀⣀⡠⠤⠤⠤⠒⠒⠒⠉⠉
    2⁰ ┤   ⣀⣀⠤⠤⠤⠒⠒⠒⠊⠉⠉⠁
       └┬─────┬──────┬─────┬─────┬
        0     2      4     6     8
    |}]

let%expect_test "time scales tick at calendar boundaries, before 1970 too" =
  let hours =
    Array.init 30 (fun i ->
        (1.7e9 +. (Float.of_int i *. 3600.), Float.of_int i))
  in
  show ~width:50 ~height:5
    C.(line ~x:fst ~y:snd hours |> scale "x" (Scale.time ()));
  let before = [| (-86400. *. 3.5, 0.); (-3600., 1.) |] in
  show ~width:50 ~height:5
    C.(line ~x:fst ~y:snd before |> scale "x" (Scale.time ()));
  let seconds = [| (5.25, 0.); (5.75, 1.) |] in
  show ~width:50 ~height:5
    C.(line ~x:fst ~y:snd seconds |> scale "x" (Scale.time ()));
  [%expect
    {|
    15 ┤                      ⣀⣀⣀⣀⣀⡠⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠊⠉⠉⠉⠁
       │ ⢀⣀⣀⣀⣀⡠⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠊⠉⠉⠉⠉
       └────┬────────────────────────────────┬────────
            15                               16
         Nov 2023
    1 ┤                     ⣀⣀⣀⣀⣀⣀⠤⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠒⠉⠉⠉⠁
    0 ┤⣀⣀⣀⠤⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠒⠉⠉⠉⠉⠉⠉
      └──────┬───────────┬───────────┬───────────┬
             29          30          31          1
          Dec 1969                            Jan 1970
    1 ┤                       ⢀⣀⣀⣀⣀⣀⣀⠤⠤⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠒⠊⠉⠉⠉
    0 ┤⣀⣀⣀⡠⠤⠤⠤⠤⠤⠤⠒⠒⠒⠒⠒⠒⠒⠉⠉⠉⠉⠉⠉⠁
      └────┬─────────┬────────┬────────┬─────────┬────
           .3        .4       .5       .6        .7
        00:00:05
    |}]

let%expect_test
    "explicit ticks, reversed and unrounded scales, constant domains" =
  show ~width:30 ~height:3
    C.(
      line ~x:Fun.id ~y:Fun.id [| 0.; 9. |]
      |> scale "x" (Scale.linear ~ticks:[ 1.; 4.; 9. ] ()));
  show ~width:30 ~height:3
    C.(
      line ~x:Fun.id ~y:Fun.id [| 0.; 9. |]
      |> scale "x" (Scale.linear ~reverse:true ()));
  show ~width:30 ~height:3
    C.(
      line ~x:Fun.id ~y:Fun.id [| 0.3; 9.7 |]
      |> scale "x" (Scale.linear ~nice:false ()));
  show ~width:30 ~height:3 C.(line ~y:(fun _ -> 5.) [| (); (); () |]);
  show ~width:30 ~height:3 C.(line ~y:Fun.id [||]);
  [%expect
    {|
    5 ┤⣀⣀⣀⣀⡠⠤⠤⠤⠤⠤⠤⠤⠤⠔⠒⠒⠒⠒⠒⠒⠒⠒⠊⠉⠉⠉⠉
      └───┬────────┬─────────────┬
          1        4             9
    5 ┤⠉⠉⠉⠉⠑⠒⠒⠒⠒⠒⠒⠒⠒⠢⠤⠤⠤⠤⠤⠤⠤⠤⢄⣀⣀⣀⣀
      └───┬─────┬─────┬─────┬────┬
          8     6     4     2    0
    5 ┤⣀⣀⣀⣀⡠⠤⠤⠤⠤⠤⠤⠤⠤⠔⠒⠒⠒⠒⠒⠒⠒⠒⠊⠉⠉⠉⠉
      └──┬────┬─────┬─────┬────┬──
         1    3     5     7    9
    5 ┤⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒⠒
      └┬────────────┬────────────┬
       0            1            2
    0.5 ┤
        └┬──────────────────────┬
         0                      1
    |}]

let%expect_test "band categories in their given order, labels thinned to fit" =
  let days =
    Array.init 30 (fun i -> (Printf.sprintf "day-%02d" i, Float.of_int i))
  in
  show ~width:40 ~height:4 C.(bar_y ~x:fst ~y:snd days);
  show ~width:30 ~height:4
    C.(
      bar_y ~x:fst ~y:snd quarters
      |> scale "x" (Scale.band ~domain:[ "Q4"; "Q3"; "Q2"; "Q1" ] ()));
  [%expect
    {|
    15 ┤                   ▁ ▁▂▂▃▃ ▄▄▅▅▆ ▆▇▇
       │ ▁ ▁▂▂▃▃ ▄▄▅▅▆ ▆▇▇██ █████ █████ ███
       └┬───────────┬───────────┬───────────
      day-00      day-10      day-20
    50 ┤ █████        ▆▆▆▆▆ ▂▂▂▂▂
     0 ┤ █████ █████  █████ █████
       └───┬─────┬──────┬─────┬───
           Q4    Q3     Q2    Q1
    |}]

(* {1 Guides} *)

let%expect_test "axis titles, sides, formats and hidden axes" =
  show ~width:36 ~height:8
    C.(
      layer
        [
          line ~x:fst ~y:snd points;
          axis ~title:"time" "x";
          axis ~side:`Right ~title:"value"
            ~format:(fun i v -> Printf.sprintf "#%d:%g" i v)
            "y";
        ]
      |> title "Chart");
  show ~width:36 ~height:4
    C.(
      layer
        [ line ~x:fst ~y:snd points; axis ~side:`Top "x"; axis ~show:false "y" ]);
  [%expect
    {|
               Chart
                                   value
            ⢀⣀⡀            ⣀⡠⠤⠒⠉├ #1:2.5
        ⣀⡠⠔⠊⠁ ⠈⠉⠉⠒⠒⠤⠤⢄⣀⡠⠔⠒⠉     │
    ⣀⠤⠒⠉                        ├ #0:0
    ┬────────┬────────┬────────┬┘
    0        1        2        3
                time
    0           1           2          3
    ┴───────────┴───────────┴──────────┴
            ⢀⣀⣀⠤⠤⠤⣀⣀⣀⣀         ⢀⣀⣀⠤⠤⠔⠒⠊⠉
    ⣀⡠⠤⠤⠒⠒⠉⠉⠁         ⠉⠉⠉⠉⠒⠒⠒⠊⠉⠁
    |}]

let%expect_test "an axis carries the program's labels, ticks keep clear" =
  let ramp = C.line ~y:Fun.id (Array.init 101 Float.of_int) in
  show ~width:40 ~height:5
    C.(
      layer
        [
          ramp;
          axis
            ~labels:
              [
                (100., Ansi.span "step");
                (47., Ansi.span "47");
                (130., Ansi.span "out");
              ]
            "x";
          axis ~labels:[ (75., Ansi.span "cursor") ] "y";
        ]);
  [%expect
    {|
    cursor ┤                    ⢀⣀⣀⠤⠤⠤⠒⠒⠊⠉⠉
           │          ⢀⣀⣀⠤⠤⠔⠒⠒⠉⠉⠁
         0 ┤⣀⣀⡠⠤⠤⠒⠒⠊⠉⠉⠁
           └┬──────────────┬──────────────┬
            0             47           step
    |}];
  show_ansi ~width:12 ~height:3
    C.(
      layer
        [
          ramp;
          axis ~show:false "y";
          axis
            ~labels:
              [ (50., Ansi.span ~style:(Ansi.Style.make ~bold:true ()) "50") ]
            "x";
        ]);
  [%expect
    {|
    [0;38;5;6m⣀⣀⣠⠤⠤⠔⠒⠒⠋⠉⠉[39m [0m
    [0;38;5;245m┬─────────┬[39m [0m
    [0;38;5;246m0[39m    [38;5;246;1m50[39;22m  [38;5;246m100[0m
    |}];
  (try
     ignore
       (C.layout ~width:20 ~height:5
          C.(
            layer
              [
                bar_y ~x:fst ~y:snd quarters;
                axis ~labels:[ (1., Ansi.span "a") ] "x";
              ]))
   with Invalid_argument m -> print_endline m);
  [%expect {| Matrix_charts.layout: axis of "x", a band scale, with labels |}]

let%expect_test "an axis's labels take room whether drawn or not" =
  let plot ?(ticks = true) dir labels =
    let ticks = if ticks then None else Some [] in
    let r =
      C.Layout.plot
        (C.layout ~width:30 ~height:8
           C.(
             layer
               [
                 line ~x:Fun.id ~y:Fun.id [| 0.; 10. |];
                 axis
                   ~labels:(List.map (fun v -> (v, Ansi.span "a label")) labels)
                   dir;
               ]
             |> scale dir (Scale.linear ?ticks ~domain:(0., 10.) ())))
    in
    Printf.printf "%s, labels at [%s]%s: x %d, width %d, height %d\n" dir
      (String.concat "; " (List.map string_of_float labels))
      (if ticks = None then "" else ", no ticks")
      r.x r.width r.height
  in
  plot "y" [];
  plot "y" [ 5. ];
  plot "y" [ 50. ];
  plot "x" [];
  plot "x" [ 5. ];
  plot ~ticks:false "x" [];
  plot ~ticks:false "x" [ 5. ];
  [%expect
    {|
    y, labels at []: x 4, width 25, height 6
    y, labels at [5.]: x 9, width 20, height 6
    y, labels at [50.]: x 9, width 20, height 6
    x, labels at []: x 4, width 25, height 6
    x, labels at [5.]: x 4, width 25, height 6
    x, labels at [], no ticks: x 4, width 26, height 7
    x, labels at [5.], no ticks: x 4, width 26, height 6
    |}]

let%expect_test "an axis label sits at its value, a blank cell from tick labels"
    =
  Random.init 12;
  let bad = ref [] and checked = ref 0 in
  for _ = 1 to 300 do
    let width = 3 + Random.int 100 in
    let lo = Random.float 2000. -. 1000. in
    let hi = lo +. Float.pow 10. (Random.float 6. -. 2.) in
    let v = lo +. Random.float (hi -. lo) in
    let s = String.make (1 + Random.int 6) 'z' in
    let fig =
      C.(
        layer
          [
            line ~x:Fun.id ~y:Fun.id [| lo; hi |];
            axis ~show:false "y";
            axis ~labels:[ (v, Ansi.span s) ] "x";
          ])
    in
    let l = C.layout ~width ~height:3 fig in
    let g = Grid.create ~width ~height:3 () in
    C.draw l g ~x:0 ~y:0;
    let p = C.Layout.plot l in
    let w = String.length s in
    let c = C.Layout.position l "x" (C.Layout.Num v) in
    let start = max p.x (min (c - ((w - 1) / 2)) (p.x + p.width - w)) in
    let row = 2 in
    let at x =
      if x < 0 || x >= width then " "
      else
        let i = Grid.idx g ~x ~y:row in
        if Grid.is_empty g i then " " else Grid.get_text g i
    in
    let text = String.concat "" (List.init w (fun k -> at (start + k))) in
    let clear = at (start - 1) = " " && at (start + w) = " " in
    (* The region has a row of labels when the plot is left one row. *)
    if w <= p.width && p.height = 1 then (
      incr checked;
      if not (String.equal text s && clear) then
        bad := (width, lo, hi, v, s) :: !bad)
  done;
  Printf.printf "labels misplaced or crowded: %d of %d\n" (List.length !bad)
    !checked;
  List.iteri
    (fun i (width, lo, hi, v, s) ->
      if i < 3 then
        Printf.printf "%d cells, [%g, %g], %S at %g\n" width lo hi s v)
    (List.rev !bad);
  [%expect {| labels misplaced or crowded: 0 of 294 |}]

let%expect_test "gridlines at ticks, minor ones and every n cells" =
  show ~width:30 ~height:8
    C.(
      layer
        [
          line ~x:fst ~y:snd points;
          axis ~grid:`Ticks "x";
          axis ~grid:(`Minor 2) "y";
        ]);
  show ~width:30 ~height:6
    C.(layer [ line ~x:fst ~y:snd points; axis ~grid:(`Every 5) "x" ]);
  [%expect
    {|
    2.5 ┤┈┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┼┈┈┈┈┈┈⡠⠊
        │        ⣀       ┊   ⢀⡠⠊ ┊
        │      ⡠⠊┊⠉⠒⠤⣀   ┊ ⢀⠔⠁   ┊
        │┈┈┈┈⡠⠊┈┈┼┈┈┈┈⠉⠒⠤⣀⠔⠁┈┈┈┈┈┼
        │  ⡠⠊    ┊       ┊       ┊
    0.0 ┤⡠⠊      ┊       ┊       ┊
        └┬───────┬───────┬───────┬
         0       1       2       3
    2.5 ┤     ┊    ┊    ┊    ┊ ⡠⠔⠊
        │     ┊⣀⠤⠒⠤⠤⣀⡀  ┊  ⢀⡠⠔⠉
        │   ⣀⠔⠊    ┊ ⠈⠉⠒⠒⠤⠒⠁ ┊
    0.0 ┤⡠⠔⠉  ┊    ┊    ┊    ┊
        └┬───────┬───────┬───────┬
         0       1       2       3
    |}]

let%expect_test "a frame, margins and an inset" =
  show ~width:30 ~height:9
    C.(
      layer [ line ~x:fst ~y:snd points; frame ]
      |> margins ~left:6 ~top:1 ~inset:1);
  [%expect
    {|
        ┌────────────────────────┐
        │                        │
    2.5 ┤                    ⢀⠔⠊ │
        │      ⢀⠤⠒⠤⢄⣀     ⢀⠤⠊⠁   │
        │   ⢀⡠⠒⠁     ⠉⠑⠒⠤⠒⠁      │
    0.0 ┤ ⡠⠔⠁                    │
        │                        │
        └─┬─────────────────┬────┘
         0.0               2.5
    |}]

let%expect_test "the plot is the same whatever guide fills a least margin" =
  Random.init 12;
  let differ = ref [] and checked = ref 0 in
  for _ = 1 to 400 do
    let width = 10 + Random.int 70 and height = 3 + Random.int 6 in
    let left = 7 + Random.int 6 in
    let lo = Float.of_int (Random.int 20000 - 10000) in
    let hi = lo +. Float.of_int (1 + Random.int 5000) in
    let plot y_axis =
      C.Layout.plot
        (C.layout ~width ~height
           C.(
             layer [ line ~x:Fun.id ~y:Fun.id [| lo; hi |]; y_axis ]
             |> scale "x" (Scale.linear ~domain:(lo, hi) ())
             |> margins ~left))
    in
    (* the y axis's room within [left]: no labels, or labels of two cells *)
    let bare = plot C.(axis ~format:(fun _ _ -> "") "y")
    and labelled = plot C.(axis ~format:(fun _ _ -> "ab") "y") in
    if bare.x = left && labelled.x = left then begin
      incr checked;
      if bare <> labelled then
        differ := (width, height, left, lo, hi) :: !differ
    end
  done;
  Printf.printf "plots that differ: %d of %d\n" (List.length !differ) !checked;
  List.iteri
    (fun i (w, h, left, lo, hi) ->
      if i < 3 then
        Printf.printf "%dx%d, least left %d, x [%g, %g]\n" w h left lo hi)
    (List.rev !differ);
  [%expect {| plots that differ: 0 of 393 |}]

let%expect_test "a second y axis reads a second scale" =
  show ~width:40 ~height:8
    C.(
      layer
        [
          line ~x:fst ~y:snd points;
          line ~x:fst ~y:(fun (_, y) -> 1000. *. y) points
          |> rename "y" "volume";
          axis ~side:`Right "volume";
        ]);
  [%expect
    {|
    2.5 ┤                          ⢀⠤⠊├ 2500
        │         ⢀⡀             ⡠⠔⠁  │
        │       ⡠⠔⠁⠈⠑⠢⠤⣀      ⢀⠔⠊     │
        │    ⢀⡠⠊        ⠉⠑⠢⢄⡠⠒⠁       │
        │  ⢀⠔⠁                        │
    0.0 ┤⡠⠊⠁                          ├ 0
        └┬────────┬─────────┬────────┬┘
         0        1         2        3
    |}]

let%expect_test "a legend on the right lies beyond a second y axis" =
  show ~width:40 ~height:6
    C.(
      layer
        [
          line ~color:(series "price") ~x:fst ~y:snd points;
          line ~color:(series "volume") ~x:fst
            ~y:(fun (_, y) -> 1000. *. y)
            points
          |> rename "y" "volume";
          axis ~side:`Right "volume";
        ]);
  [%expect
    {|
    2.5 ┤                 ⢀⠤⠊├ 2500 ─ price
        │     ⣀⠔⠢⢄⣀     ⡠⠒⠁  │      ─ volume
        │  ⢀⠤⠊     ⠉⠑⠢⠔⠉     │
    0.0 ┤⡠⠒⠁                 ├ 0
        └┬───────────────┬───┘
        0.0             2.5
    |}]

let%expect_test "legends: implied, on each side, in rows or columns" =
  let fig side direction =
    C.(
      layer
        [
          line ~color:(series "up") ~y:Fun.id [| 0.; 1. |];
          line ~color:(series "down") ~y:Fun.id [| 1.; 0. |];
          legend ~side ~direction ~title:"key" "color";
          axis ~show:false "x";
          axis ~show:false "y";
        ])
  in
  show ~width:24 ~height:5
    C.(
      layer
        [
          line ~color:(series "up") ~y:Fun.id [| 0.; 1. |];
          line ~color:(series "down") ~y:Fun.id [| 1.; 0. |];
        ]);
  show ~width:24 ~height:5 (fig `Right `Rows);
  show ~width:30 ~height:5 (fig `Bottom `Columns);
  show ~width:24 ~height:5 (fig (`Inside `Bottom_left) `Rows);
  [%expect
    {|
    1 ┤⠉⠒⠤⢄⡀    ⢀⡠⠤⠒⠉ ─ up
      │    ⠈⠉⠒⠤⣀⡀     ─ down
    0 ┤⣀⠤⠒⠊⠁    ⠈⠑⠒⠤⣀
      └┬────────────┬
       0            1
    ⠑⠢⢄⡀         ⢀⡠⠔⠊ key
       ⠈⠒⠤⣀   ⣀⠤⠒⠁    ─ up
           ⠑⠢⢄        ─ down
       ⢀⠤⠒⠉   ⠉⠒⠤⡀
    ⡠⠔⠊⠁         ⠈⠑⠢⢄
    ⠉⠑⠒⠒⠢⠤⠤⣀⣀⡀          ⢀⣀⣀⠤⠤⠔⠒⠒⠊⠉
             ⠈⠉⠉⠑⠒⠒⠤⠤⢄⣀⣀⡀
    ⣀⡠⠤⠤⠔⠒⠒⠉⠉⠁          ⠈⠉⠉⠒⠒⠢⠤⠤⢄⣀
    key
    ─ up  ─ down
    ⠉⠒⠤⢄⡀              ⢀⡠⠤⠒⠉
        ⠈⠉⠒⠤⢄⡀    ⢀⡠⠤⠒⠉⠁
    key      ⠈⠉⠒⠤⣀⡀
    ─ up  ⠤⠒⠊⠁    ⠈⠑⠒⠤⣀⡀
    ─ down             ⠈⠑⠒⠤⣀
    |}]

(* {1 Themes and glyphs} *)

let%expect_test "the ASCII glyphs draw every chart in ASCII" =
  let theme = C.Theme.{ dark with glyphs = ascii_glyphs } in
  let fig =
    C.(
      layer
        [
          bar_y ~x:fst ~y:snd quarters;
          line ~dots:`Braille ~y:snd quarters |> rename "x" "x2";
          axis ~grid:`Ticks "y";
          axis ~show:false "x2";
        ]
      |> title "ascii")
  in
  show ~theme ~width:30 ~height:8 fig;
  let g = Grid.create ~width:30 ~height:8 () in
  C.draw (C.layout ~theme ~width:30 ~height:8 fig) g ~x:0 ~y:0;
  let text = Grid.to_text g in
  print_endline
    (if String.for_all (fun c -> Char.code c < 128) text then "ascii only"
     else "not ascii");
  [%expect
    {|
                  ascii
    50 +....-------#........###///
       |----   ####----    ////##
       | ##### #####  #--// #####
       | ##### #####  ##### #####
     0 + ##### #####  ##### #####
       +---+-----+------+-----+---
           Q1    Q2     Q3    Q4
    ascii only
    |}]

let%expect_test "a theme's background fills the region" =
  let theme = C.Theme.{ dark with background = Some Ansi.Color.blue } in
  show_ansi ~theme ~width:6 ~height:2 (bare C.(line ~y:Fun.id [| 0.; 1. |]));
  [%expect
    {|
    [0;48;5;4m   [38;5;6m⣀⠔⠊[0m
    [0;38;5;6;48;5;4m⡠⠔⠉[39m   [0m
    |}]

let%expect_test "256 colors keep a color map's lightness in order" =
  let rows = Array.init 64 (fun i -> (Float.of_int i, 0., Float.of_int i)) in
  let fig =
    C.(
      raster
        ~x:(fun (x, _, _) -> x)
        ~y:(fun (_, y, _) -> y)
        ~value:(fun (_, _, v) -> v)
        rows
      |> scale "x" (Scale.linear ~nice:false ()))
  in
  let g = Grid.create ~width:64 ~height:1 () in
  C.draw (C.layout ~depth:`Ansi256 ~width:64 ~height:1 (bare fig)) g ~x:0 ~y:0;
  let lightness c =
    let r, g, b = Ansi.Color.to_rgb c in
    (0.2126 *. Float.of_int r)
    +. (0.7152 *. Float.of_int g)
    +. (0.0722 *. Float.of_int b)
  in
  let reversals = ref 0 and prev = ref neg_infinity in
  for x = 0 to 63 do
    let c = Grid.get_bg g (Grid.idx g ~x ~y:0) in
    (match Ansi.Color.intent c with
    | Indexed _ -> ()
    | _ -> print_endline "not indexed");
    let l = lightness c in
    if l < !prev -. 1e-9 then incr reversals;
    prev := l
  done;
  Printf.printf "reversals: %d\n" !reversals;
  show ~depth:`Ansi16 ~width:16 ~height:1 (bare fig);
  [%expect {|
    reversals: 0
    ░░░░▒▒▒▒▓▓▓▓████
    |}]

(* {1 Laying out} *)

let%expect_test "every size from 1 by 1 draws inside its rectangle" =
  let fig =
    C.(layer [ line ~x:fst ~y:snd points; axis ~title:"t" "x" ] |> title "T")
  in
  for w = 1 to 12 do
    for h = 1 to 6 do
      let g = Grid.create ~width:16 ~height:10 () in
      Grid.draw_text g ~x:0 ~y:0 ~text:"................";
      C.draw (C.layout ~width:w ~height:h fig) g ~x:2 ~y:2;
      for y = 0 to 9 do
        for x = 0 to 15 do
          let inside = x >= 2 && x < 2 + w && y >= 2 && y < 2 + h in
          let i = Grid.idx g ~x ~y in
          let t = Grid.get_text g i in
          if (not inside) && t <> if y = 0 then "." else " " then
            Printf.printf "%dx%d wrote outside at (%d, %d)\n" w h x y
        done
      done
    done
  done;
  show ~width:4 ~height:2 fig;
  [%expect {|
    │⡠⠢⠜
    └───
    |}]

let%expect_test "the plot, values and positions of a layout" =
  let l = C.layout ~width:30 ~height:10 C.(bar_y ~x:fst ~y:snd quarters) in
  let p = C.Layout.plot l in
  Printf.printf "plot %d %d %d %d\n" p.x p.y p.width p.height;
  let pr = function
    | Some (C.Layout.Num v) -> Printf.printf "%g\n" v
    | Some (Cat c) -> print_endline c
    | None -> print_endline "none"
  in
  pr (C.Layout.value l "x" (p.x, p.y));
  pr (C.Layout.value l "x" (p.x + p.width - 1, p.y));
  pr (C.Layout.value l "y" (p.x, p.y + p.height - 1));
  pr (C.Layout.value l "y" (0, 0));
  Printf.printf "Q3 at column %d\n" (C.Layout.position l "x" (Cat "Q3"));
  Printf.printf "25 at row %d, 1000 at row %d\n"
    (C.Layout.position l "y" (Num 25.))
    (C.Layout.position l "y" (Num 1000.));
  List.iter pr (List.map Option.some (C.Layout.domain l "y"));
  [%expect
    {|
    plot 4 0 26 8
    Q1
    Q4
    3.125
    none
    Q3 at column 20
    25 at row 3, 1000 at row 0
    0
    50
    |}]

let%expect_test "conflicting specifications raise" =
  let attempt f =
    try
      ignore (f ());
      print_endline "ok"
    with Invalid_argument m -> print_endline m
  in
  let lay fig () = C.layout ~width:10 ~height:5 fig in
  attempt
    (lay
       C.(
         line ~y:Fun.id [| 1. |]
         |> scale "y" (Scale.log ())
         |> scale "y" (Scale.linear ())));
  attempt (lay C.(layer [ line ~y:Fun.id [| 1. |]; axis "nope" ]));
  attempt (lay C.(bar_y ~x:fst ~y:snd quarters |> scale "x" (Scale.linear ())));
  attempt (lay C.(layer [ line ~y:Fun.id [| 1. |] |> title "a" ] |> title "b"));
  attempt (lay C.(line ~y:Fun.id [| 1. |] |> scale "z" (Scale.linear ())));
  [%expect
    {|
    Matrix_charts.layout: two specifications of scale "y"
    Matrix_charts.layout: axis of "nope", which no role reads
    Matrix_charts.layout: scale "x" of quantities is read by categories
    Matrix_charts.layout: two titles, "b" and "a"
    Matrix_charts.layout: scale "z" has a specification but no role reads it
    |}]

(* {1 Views} *)

let%expect_test "zooming keeps the value under the pointer, pans compose" =
  (* Margins wider than any label keep the plot in place. *)
  let fig =
    C.(
      line ~x:Fun.id ~y:Fun.id (Array.init 101 Float.of_int)
      |> margins ~left:6 ~right:4)
  in
  let l = C.layout ~width:50 ~height:12 fig in
  let at = (30, 5) in
  let before = C.Layout.value l "x" at in
  let v = C.Layout.zoom ~axes:`X l ~at 2. in
  let l' = C.layout ~view:v ~width:50 ~height:12 fig in
  let pr = function
    | Some (C.Layout.Num x) -> Printf.printf "%.3f\n" x
    | _ -> print_endline "none"
  in
  pr before;
  pr (C.Layout.value l' "x" at);
  let window n v =
    match C.View.window n v with
    | Some (a, b) -> Printf.printf "%s %.3f..%.3f\n" n a b
    | None -> Printf.printf "%s whole\n" n
  in
  window "x" v;
  let p1 = C.Layout.pan l' ~by:(3, 0) in
  let p2 =
    C.Layout.pan (C.layout ~view:p1 ~width:50 ~height:12 fig) ~by:(2, 0)
  in
  let p3 = C.Layout.pan l' ~by:(5, 0) in
  window "x" p2;
  window "x" p3;
  [%expect
    {|
    61.250
    61.250
    x 30.625..80.625
    x 24.375..74.375
    x 24.375..74.375
    |}]

let%expect_test "views zoom, pan and clamp without a layout" =
  let fig = C.(line ~x:Fun.id ~y:Fun.id [| 0.; 10. |]) in
  let window v =
    match C.View.window "x" v with
    | Some (a, b) -> Printf.printf "x %g..%g\n" a b
    | None -> print_endline "x whole"
  in
  let v = C.View.zoom ~axes:`X fig 2. C.View.empty in
  window v;
  let v = C.View.pan fig ~by:(0.5, 0.) v in
  window v;
  window (C.View.clamp fig v);
  window (C.View.clamp fig (C.View.set "x" (Some (-5., 20.)) C.View.empty));
  (try ignore (C.View.set "x" (Some (1., 1.)) C.View.empty)
   with Invalid_argument m -> print_endline m);
  [%expect
    {|
    x 2.5..7.5
    x 5..10
    x 5..10
    x whole
    Matrix_charts.View.set: window (1, 1) of "x" is not finite and increasing
    |}]

(* {1 Hits and overlays} *)

let%expect_test "hits name the topmost row under the pointer" =
  let a = [| (0., 0.); (5., 5.); (10., 10.) |]
  and b = [| (0., 10.); (10., 0.) |] in
  let l =
    C.layout ~width:30 ~height:12
      C.(
        layer
          [
            dot ~dots:`Cell ~x:fst ~y:snd a |> name "a";
            line ~x:fst ~y:snd b |> name "b";
          ])
  in
  let pr = function
    | None -> print_endline "none"
    | Some (h : C.hit) ->
        Printf.printf "%s row %d at (%d, %d) d=%.2f\n" (String.concat "/" h.id)
          h.row (fst h.cell) (snd h.cell) h.distance
  in
  let at v =
    (C.Layout.position l "x" (Num v), C.Layout.position l "y" (Num v))
  in
  pr (C.Layout.hit l (at 0.));
  pr (C.Layout.hit l (at 5.));
  pr (C.Layout.hit ~metric:`X l (fst (at 10.), snd (at 0.)));
  pr (C.Layout.hit ~radius:0. l (fst (at 2.), snd (at 8.)));
  pr (C.Layout.hit l (0, 0));
  [%expect
    {|
    a row 0 at (4, 9) d=0.00
    b row 0 at (16, 4) d=0.00
    b row 1 at (28, 9) d=0.00
    none
    none
    |}]

let%expect_test "bars, areas and rasters are hit over their cells" =
  let l = C.layout ~width:24 ~height:8 C.(bar_y ~x:fst ~y:snd quarters) in
  let at =
    (C.Layout.position l "x" (Cat "Q2"), C.Layout.position l "y" (Num 10.))
  in
  (match C.Layout.hit ~radius:0. l at with
  | Some h -> Printf.printf "row %d\n" h.row
  | None -> print_endline "none");
  [%expect {| row 1 |}]

let%expect_test "overlays: crosshair, marker and a tip that avoids its point" =
  let l = C.layout ~width:30 ~height:10 C.(line ~x:fst ~y:snd points) in
  let c =
    (C.Layout.position l "x" (Num 3.), C.Layout.position l "y" (Num 3.))
  in
  let g = Grid.create ~width:30 ~height:10 () in
  C.draw
    ~overlays:
      [
        Crosshair c;
        Marker c;
        Tip { at = c; anchor = `Auto; lines = [ "x 3"; "y 3" ] };
      ]
    l g ~x:0 ~y:0;
  print_string (Grid.to_text g);
  [%expect
    {|
        │────────────────────────●
    2.5 ┤                      ⡔⠁│
        │       ⢀⢄⡀        ┌─────┐
        │      ⡠⠃ ⠈⠒⢄⡀     │ x 3 │
        │    ⢀⠜      ⠈⠑⠤⡀ ⢠│ y 3 │
        │   ⡠⠃          ⠈⠑⠁└─────┘
        │ ⢀⠜                     │
    0.0 ┤⡠⠃                      │
        └┬───────┬───────┬───────┬
         0       1       2       3
    |}]

let%expect_test "the legend entries a program draws itself" =
  let l =
    C.layout ~width:20 ~height:5
      C.(
        layer
          [
            line ~color:(series "a") ~y:Fun.id [| 1. |];
            dot ~color:(cat fst) ~x:snd ~y:snd [| ("b", 1.); ("c", 2.) |];
          ])
  in
  List.iter
    (fun (label, _, swatch) -> Printf.printf "%s %s\n" swatch label)
    (C.Layout.legend l);
  [%expect {|
    ─ a
    ∙ b
    ∙ c
    |}]

let%expect_test "draw at an offset" =
  let g = Grid.create ~width:14 ~height:5 () in
  C.draw
    (C.layout ~width:10 ~height:3
       (bare C.(line ~dots:`Cell ~y:Fun.id [| 0.; 1. |])))
    g ~x:3 ~y:1;
  print_string (Grid.to_text g);
  [%expect {|
           ───
       ────
    ───
    |}]

(* {1 Smoothing} *)

let%expect_test "smoothing" =
  let data = [| (0., 0.); (1., 10.); (2., 0.); (3., 10.) |] in
  let pr a =
    Array.iter (fun (x, y) -> Printf.printf "(%g, %.3g) " x y) a;
    print_newline ()
  in
  pr (C.ema 0.5 data);
  pr (C.sma 2 data);
  pr (C.gaussian 1. data);
  [%expect
    {|
    (0, 0) (1, 5) (2, 2.5) (3, 6.25)
    (0, 0) (1, 5) (2, 5) (3, 5)
    (0, 3.52) (1, 4.83) (2, 5.17) (3, 6.48)
    |}]

(* {1 Properties} *)

let text_rows g = String.split_on_char '\n' (Grid.to_text g)

let%expect_test "x labels keep a blank cell between them" =
  Random.init 14;
  let bad = ref 0 in
  for _ = 1 to 300 do
    let a = Random.float 2000. -. 1000. in
    let b = a +. Float.pow 10. (Random.float 8. -. 4.) in
    let width = 5 + Random.int 120 in
    let g = Grid.create ~width ~height:4 () in
    C.draw
      (C.layout ~width ~height:4 C.(line ~x:Fun.id ~y:Fun.id [| a; b |]))
      g ~x:0 ~y:0;
    (* the labels are on the row after the axis's, whose ticks are ┬, the
       only glyph of the axis that ends in the byte 0xAC *)
    let rec after = function
      | axis :: labels :: _
        when String.length axis > 0 && String.contains axis '\xac' ->
          Some (axis, labels)
      | _ :: rest -> after rest
      | [] -> None
    in
    match after (text_rows g) with
    | Some (axis, labels) ->
        let ticks = List.length (String.split_on_char '\xac' axis) - 1 in
        let words = List.filter (( <> ) "") (String.split_on_char ' ' labels) in
        if ticks <> List.length words then incr bad
    | None -> ()
  done;
  Printf.printf "charts whose labels run together: %d\n" !bad;
  [%expect {| charts whose labels run together: 0 |}]

let%expect_test "a value lies in the same cell at every resolution" =
  Random.init 3;
  let col dots v =
    let g = Grid.create ~width:37 ~height:1 () in
    let fig =
      C.(
        dot ~dots ~x:Fun.id ~y:(fun _ -> 0.) [| v |]
        |> scale "x" (Scale.linear ~domain:(0., 1.) ()))
    in
    C.draw (C.layout ~width:37 ~height:1 (bare fig)) g ~x:0 ~y:0;
    let rec find x =
      if
        x >= 37
        || (not (Grid.is_empty g (Grid.idx g ~x ~y:0)))
           && Grid.get_text g (Grid.idx g ~x ~y:0) <> " "
      then x
      else find (x + 1)
    in
    find 0
  in
  let differ = ref 0 in
  for _ = 1 to 1000 do
    let v = Random.float 1. in
    let c = col `Cell v in
    List.iter
      (fun d -> if col d v <> c then incr differ)
      [ `Half; `Quadrant; `Sextant; `Braille; `Octant ]
  done;
  Printf.printf "values in another cell: %d\n" !differ;
  [%expect {| values in another cell: 0 |}]

(* The cells of a grid that hold a glyph. *)
let inked g =
  let l = ref [] in
  for y = Grid.height g - 1 downto 0 do
    for x = Grid.width g - 1 downto 0 do
      let i = Grid.idx g ~x ~y in
      if (not (Grid.is_empty g i)) && Grid.get_text g i <> " " then
        l := (x, y) :: !l
    done
  done;
  !l

let drawn ~width ~height fig =
  let g = Grid.create ~width ~height () in
  C.draw (C.layout ~width ~height fig) g ~x:0 ~y:0;
  g

(* A value at a cell's boundary: [5 / 6] of [60] cells is [50]. Drawn values
   are such boundaries, as [k / m] of the domain on an axis of a multiple of
   [m] cells, half the time. *)
let boundary () =
  let m = 1 + Random.int 12 in
  let len = m * (1 + Random.int 8) in
  let lo = Float.of_int (Random.int 2000 - 1000) /. 8. in
  let span = Float.of_int (1 + Random.int 400) /. 16. in
  let v =
    if Random.bool () then
      lo +. (span *. Float.of_int (Random.int (m + 1)) /. Float.of_int m)
    else lo +. Random.float span
  in
  (lo, lo +. span, v, len)

let%expect_test "marks, rules and ticks at one value share a cell" =
  Random.init 21;
  let x_marks = [ `Cell; `Half; `Quadrant; `Sextant; `Braille; `Octant ] in
  let col fig ~width ~height =
    match inked (drawn ~width ~height fig) with (x, _) :: _ -> x | [] -> -1
  in
  let row fig ~width ~height =
    match inked (drawn ~width ~height fig) with (_, y) :: _ -> y | [] -> -1
  in
  let differ = ref [] in
  let check ?start dir (lo, hi, v, len) =
    let start =
      match start with Some s -> s | None -> lo +. Random.float (hi -. lo)
    in
    let sx = C.Scale.linear ~domain:(lo, hi) () in
    let unit = C.Scale.linear ~domain:(0., 1.) () in
    let other = if dir = "x" then "y" else "x" in
    let hidden fig =
      C.(layer [ fig; axis ~show:false "x"; axis ~show:false "y" ])
      |> C.scale dir sx |> C.scale other unit
    in
    let width, height = if dir = "x" then (len, 1) else (1, len) in
    let at fig =
      if dir = "x" then col fig ~width ~height else row fig ~width ~height
    in
    let half _ = 0.5 and value _ = v in
    let point (dots : C.dots) =
      if dir = "x" then
        C.dot ~dots:(dots :> [ C.dots | `Density ]) ~x:value ~y:half [| () |]
      else C.dot ~dots:(dots :> [ C.dots | `Density ]) ~x:half ~y:value [| () |]
    in
    let rule =
      C.(
        layer [ (if dir = "x" then vline v else hline v); axis ~show:false dir ]
        |> scale dir sx)
    in
    let l = C.layout ~width ~height (hidden (point `Cell)) in
    let tick = C.Layout.position l dir (C.Layout.Num v) in
    (* A line from [start] ends at [v]: its last cell toward [v]. *)
    let line (dots : C.dots) =
      let dots = (dots :> [ C.dots | `Steps ]) in
      if dir = "x" then C.line ~dots ~x:fst ~y:snd [| (start, 0.5); (v, 0.5) |]
      else C.line ~dots ~x:fst ~y:snd [| (0.5, start); (0.5, v) |]
    in
    let last fig =
      let g = drawn ~width ~height fig in
      let cells =
        List.map (fun (x, y) -> if dir = "x" then x else y) (inked g)
      in
      let toward =
        if C.Layout.position l dir (Num start) <= tick then max else min
      in
      match cells with [] -> -1 | c :: cs -> List.fold_left toward c cs
    in
    let cells =
      ("tick", tick)
      :: ("rule", at rule)
      :: List.concat_map
           (fun d ->
             let name =
               match d with
               | `Cell -> "cell"
               | `Half -> "half"
               | `Quadrant -> "quadrant"
               | `Sextant -> "sextant"
               | `Braille -> "braille"
               | `Octant -> "octant"
             in
             [
               (name, at (hidden (point d)));
               ("line " ^ name, last (hidden (line d)));
             ])
           x_marks
    in
    if List.exists (fun (_, c) -> c <> tick) cells then
      differ := (dir, lo, hi, v, len, cells) :: !differ
  in
  check "x" (0., 6., 5., 60);
  check "x" ~start:(-87.579420775857358) (-91.25, -71.5, -74.79166666666667, 12);
  for _ = 1 to 400 do
    check "x" (boundary ());
    check "y" (boundary ())
  done;
  Printf.printf "values in two cells: %d\n" (List.length !differ);
  List.iteri
    (fun i (axis, lo, hi, v, len, cells) ->
      if i < 3 then (
        Printf.printf "%s on [%g, %g] at %d cells, %g:" axis lo hi len v;
        List.iter (fun (n, c) -> Printf.printf " %s %d" n c) cells;
        print_newline ()))
    (List.rev !differ);
  [%expect {| values in two cells: 0 |}]

(* The dots of a braille glyph, as (column, row) in its cell. *)
let braille_dots s =
  let u = Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0)) in
  let bits = u - 0x2800 in
  let at =
    [| (0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (1, 2); (0, 3); (1, 3) |]
  in
  if bits < 0 || bits > 0xff then []
  else
    List.filter_map
      (fun b -> if bits land (1 lsl b) <> 0 then Some at.(b) else None)
      (List.init 8 Fun.id)

let%expect_test "a value's dot is where a mark draws it" =
  Random.init 5;
  let wrong = ref [] in
  for _ = 1 to 600 do
    let lo, hi, v, len = boundary () in
    let v =
      match Random.int 8 with
      | 0 -> lo -. Random.float 10.
      | 1 -> hi +. Random.float 10.
      | _ -> v
    in
    let reverse = Random.int 4 = 0 in
    let dir = if Random.bool () then "x" else "y" in
    let other = if dir = "x" then "y" else "x" in
    let width, height = if dir = "x" then (len, 1) else (1, len) in
    let res, k =
      match Random.int 3 with
      | 0 -> (`Cell, 1)
      | 1 -> (`Braille, if dir = "x" then 2 else 4)
      | _ -> (`Quadrant, 2)
    in
    let half _ = 0.5 and value _ = v in
    let fig =
      C.(
        layer
          [
            (if dir = "x" then dot ~dots:res ~x:value ~y:half [| () |]
             else dot ~dots:res ~x:half ~y:value [| () |]);
            axis ~show:false "x";
            axis ~show:false "y";
          ]
        |> scale dir (Scale.linear ~domain:(lo, hi) ~reverse ())
        |> scale other (Scale.linear ~domain:(0., 1.) ()))
    in
    let l = C.layout ~width ~height fig in
    let d = C.Layout.dot l dir (res :> C.dots) (C.Layout.Num v) in
    let g = Grid.create ~width ~height () in
    C.draw l g ~x:0 ~y:0;
    let seen =
      match inked g with
      | [] -> None
      | (x, y) :: _ -> (
          let cell = if dir = "x" then x else y in
          match res with
          | `Cell -> Some cell
          | `Quadrant ->
              let s = Grid.get_text g (Grid.idx g ~x ~y) in
              let c, r =
                match s with
                | "▘" -> (0, 0)
                | "▝" -> (1, 0)
                | "▖" -> (0, 1)
                | _ -> (1, 1)
              in
              Some ((cell * k) + if dir = "x" then c else r)
          | `Braille -> (
              match braille_dots (Grid.get_text g (Grid.idx g ~x ~y)) with
              | [ (c, r) ] -> Some ((cell * k) + if dir = "x" then c else r)
              | _ -> Some (-2)))
    in
    let expected =
      match seen with Some s -> s | None -> if d < 0 then d else len * k
    in
    if d <> expected || (seen = None && d >= 0 && d < len * k) then
      wrong := (dir, lo, hi, v, len, reverse, d, seen) :: !wrong
  done;
  Printf.printf "values whose dot is elsewhere: %d\n" (List.length !wrong);
  List.iteri
    (fun i (dir, lo, hi, v, len, reverse, d, seen) ->
      if i < 3 then
        Printf.printf "%s on [%g, %g]%s at %d cells, %g: dot %d, drawn %s\n" dir
          lo hi
          (if reverse then " reversed" else "")
          len v d
          (match seen with Some s -> string_of_int s | None -> "nowhere"))
    (List.rev !wrong);
  [%expect {| values whose dot is elsewhere: 0 |}]

let%expect_test "a category's dot lies in the cell of its tick" =
  let wrong = ref [] and cases = ref 0 in
  for n = 1 to 40 do
    for len = 1 to 120 do
      let cats = List.init n string_of_int in
      let rows = Array.of_list (List.map (fun c -> (c, 1.)) cats) in
      List.iter
        (fun dir ->
          let fig =
            if dir = "x" then C.(bare (bar_y ~x:fst ~y:snd rows))
            else C.(bare (bar_x ~y:fst ~x:snd rows))
          in
          let width, height = if dir = "x" then (len, 3) else (3, len) in
          let l = C.layout ~width ~height fig in
          let p = C.Layout.plot l in
          let start = if dir = "x" then p.x else p.y in
          List.iter
            (fun c ->
              let v = C.Layout.Cat c in
              let cell = C.Layout.position l dir v - start in
              List.iter
                (fun (res, k) ->
                  incr cases;
                  let d = C.Layout.dot l dir res v in
                  if d / k <> cell then
                    wrong := (dir, n, len, c, cell, d) :: !wrong)
                [ (`Cell, 1); (`Braille, if dir = "x" then 2 else 4) ])
            cats)
        [ "x"; "y" ]
    done
  done;
  Printf.printf "category dots outside their tick's cell: %d of %d\n"
    (List.length !wrong) !cases;
  List.iteri
    (fun i (dir, n, len, c, cell, d) ->
      if i < 3 then
        Printf.printf "%s, %d categories on %d cells, %S: cell %d, dot %d\n" dir
          n len c cell d)
    (List.rev !wrong);
  [%expect {| category dots outside their tick's cell: 0 of 393600 |}]

let%expect_test "a line's last dot is its value's dot" =
  Random.init 8;
  let wrong = ref 0 in
  for _ = 1 to 600 do
    let lo, hi, v, len = boundary () in
    let start = lo +. Random.float (hi -. lo) in
    let dir = if Random.bool () then "x" else "y" in
    let other = if dir = "x" then "y" else "x" in
    let width, height = if dir = "x" then (len, 1) else (1, len) in
    let k = if dir = "x" then 2 else 4 in
    let pts =
      if dir = "x" then [| (start, 0.5); (v, 0.5) |]
      else [| (0.5, start); (0.5, v) |]
    in
    let fig =
      C.(
        layer
          [
            line ~dots:`Braille ~x:fst ~y:snd pts;
            axis ~show:false "x";
            axis ~show:false "y";
          ]
        |> scale dir (Scale.linear ~domain:(lo, hi) ())
        |> scale other (Scale.linear ~domain:(0., 1.) ()))
    in
    let l = C.layout ~width ~height fig in
    let g = Grid.create ~width ~height () in
    C.draw l g ~x:0 ~y:0;
    let dots =
      List.concat_map
        (fun (x, y) ->
          let cell = if dir = "x" then x else y in
          List.map
            (fun (c, r) -> (cell * k) + if dir = "x" then c else r)
            (braille_dots (Grid.get_text g (Grid.idx g ~x ~y))))
        (inked g)
    in
    let d = C.Layout.dot l dir `Braille (Num v) in
    let ds = C.Layout.dot l dir `Braille (Num start) in
    let toward = if ds <= d then max else min in
    match dots with
    | [] -> incr wrong
    | e :: es -> if List.fold_left toward e es <> d then incr wrong
  done;
  Printf.printf "lines whose last dot is elsewhere: %d\n" !wrong;
  [%expect {| lines whose last dot is elsewhere: 0 |}]

let%expect_test "a value's dot lies outside the plot when the value does" =
  let l =
    C.layout ~width:10 ~height:4
      C.(
        bare (line ~x:Fun.id ~y:Fun.id [||])
        |> scale "x" (Scale.linear ~domain:(0., 10.) ())
        |> scale "y" (Scale.linear ~domain:(0., 10.) ()))
  in
  let show n v =
    Printf.printf "%s %g: %d\n" n v (C.Layout.dot l n `Braille (C.Layout.Num v))
  in
  List.iter (show "x") [ -1.; -0.; 0.; 5.; 10.; 10.5 ];
  List.iter (show "y") [ -1.; 0.; 10.; 10.5 ];
  List.iter
    (fun v ->
      try ignore (C.Layout.dot l "x" `Braille (C.Layout.Num v))
      with Invalid_argument m -> print_endline m)
    [ Float.nan; Float.infinity; Float.neg_infinity ];
  [%expect
    {|
    x -1: -1
    x -0: 0
    x 0: 0
    x 5: 10
    x 10: 19
    x 10.5: 20
    y -1: 16
    y 0: 15
    y 10: 0
    y 10.5: -1
    Matrix_charts.Layout.dot: nan is a value that marks drop
    Matrix_charts.Layout.dot: inf is a value that marks drop
    Matrix_charts.Layout.dot: -inf is a value that marks drop
    |}]

let%expect_test "marks leave the plot alone when the domains are given" =
  Random.init 8;
  let num () =
    match Random.int 10 with
    | 0 -> Float.nan
    | 1 -> Float.infinity
    | 2 -> -1e300
    | 3 -> 1e300
    | 4 -> -0.
    | _ -> Random.float 2e6 -. 1e6
  in
  let moved = ref 0 and cases = ref 0 in
  for _ = 1 to 300 do
    let width = 1 + Random.int 80 and height = 1 + Random.int 30 in
    let lo = Random.float 1e4 -. 5e3 in
    let hi = lo +. Float.pow 10. (Random.float 12. -. 6.) in
    let ylo = Float.pow 10. (Random.float 6. -. 3.) in
    let yhi = ylo *. Float.pow 10. (Random.float 6.) in
    let view =
      if Random.bool () then C.View.empty
      else C.View.set "x" (Some (lo +. ((hi -. lo) /. 4.), hi)) C.View.empty
    in
    let n = Random.int 30 in
    let rows = Array.init n (fun _ -> (num (), num (), num ())) in
    let fst3 (a, _, _) = a and snd3 (_, b, _) = b and thd3 (_, _, c) = c in
    let fig rows =
      C.(
        layer
          [
            line ~x:fst3 ~y:snd3 rows;
            dot ~x:fst3 ~y:thd3 rows;
            area ~x:fst3 ~y:snd3 ~y2:thd3 rows;
            rect ~x:fst3 ~x2:snd3 rows;
            text ~x:fst3 ~y:snd3 ~text:(fun _ -> "label") rows;
            bar_x ~y:(fun _ -> "b") ~x:thd3 ~color:(const Ansi.Color.red) rows
            |> rename "y" "band";
            histogram (Array.map fst3 rows);
            spark (Array.to_list (Array.map snd3 rows));
            axis ~title:"x" "x";
            axis ~title:"y" ~grid:`Ticks "y";
            axis "band";
          ]
        |> scale "x" (Scale.linear ~domain:(lo, hi) ())
        |> scale "y" (Scale.log ~domain:(ylo, yhi) ())
        |> scale "band" (Scale.band ~domain:[ "a"; "b" ] ())
        |> title "a chart")
    in
    let plot rows = C.Layout.plot (C.layout ~view ~width ~height (fig rows)) in
    incr cases;
    if plot rows <> plot [||] then incr moved
  done;
  Printf.printf "plots moved by rows: %d of %d\n" !moved !cases;
  [%expect {| plots moved by rows: 0 of 300 |}]

let%expect_test "every finite domain places every value in it" =
  let max = Float.max_float and tiny = 4.9e-324 in
  let domains =
    [
      ("linear", (-.max, max));
      ("linear", (-1e308, 1e308));
      ("linear", (-.max, -.max /. 2.));
      ("linear", (0., tiny));
      ("linear", (tiny, 3. *. tiny));
      ("linear", (1., 1. +. epsilon_float));
      ("linear", (-1e-300, 1e300));
      ("time", (-.max, max));
      ("log", (tiny, max));
      ("log", (1., 1. +. epsilon_float));
    ]
  in
  let width = 40 in
  List.iter
    (fun (kind, (lo, hi)) ->
      let spec =
        match kind with
        | "log" -> C.Scale.log ~domain:(lo, hi) ()
        | "time" -> C.Scale.time ~domain:(lo, hi) ()
        | _ -> C.Scale.linear ~domain:(lo, hi) ()
      in
      let vs =
        List.init 33 (fun i ->
            let u = Float.of_int i /. 32. in
            if kind = "log" then
              Float.exp ((Float.log lo *. (1. -. u)) +. (Float.log hi *. u))
            else (lo *. (1. -. u)) +. (hi *. u))
      in
      let fig =
        C.(
          layer
            [
              dot ~x:Fun.id ~y:(fun _ -> 0.) (Array.of_list vs);
              axis ~grid:(`Minor 2) "x";
            ]
          |> scale "x" spec)
      in
      let l = C.layout ~width ~height:6 fig in
      let dots = List.map (fun v -> C.Layout.dot l "x" `Braille (Num v)) vs in
      let k = 2 * (C.Layout.plot l).width in
      let inside = List.for_all (fun d -> d >= 0 && d < k) dots in
      let ordered = List.sort compare dots = dots in
      let p = C.Layout.plot l in
      let values =
        List.init p.width (fun c ->
            match C.Layout.value l "x" (p.x + c, p.y) with
            | Some (Num v) -> Float.is_finite v && v >= lo && v <= hi
            | _ -> false)
      in
      let g = Grid.create ~width ~height:6 () in
      C.draw l g ~x:0 ~y:0;
      Printf.printf "%s [%g, %g]: dots %s, %s, ends %d %d; values %s\n" kind lo
        hi
        (if inside then "inside" else "OUTSIDE")
        (if ordered then "in order" else "OUT OF ORDER")
        (List.hd dots) (List.nth dots 32)
        (if List.for_all Fun.id values then "inside" else "OUTSIDE");
      List.iter print_endline (text_rows g))
    domains;
  [%expect
    {|
    linear [-1.79769e+308, 1.79769e+308]: dots inside, in order, ends 0 73; values inside
      │   ┊    ┊    ┊    ┊    ┊    ┊    ┊
    0 ┤⠂⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐
      │   ┊    ┊    ┊    ┊    ┊    ┊    ┊
      └───┬─────────┬─────────┬─────────┬───
         -150      -50        50       150
                      ×10³⁰⁶
    linear [-1e+308, 1e+308]: dots inside, in order, ends 0 71; values inside
      │         ┊        ┊        ┊       ┊
    0 ┤⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠐⠐⠐⠐⠐
      │         ┊        ┊        ┊       ┊
      └┬─────────────────┬────────────────┬
      -100               0               100
                      ×10³⁰⁶
    linear [-1.79769e+308, -8.98847e+307]: dots inside, in order, ends 0 73; values inside
      │ ┊     ┊    ┊    ┊    ┊    ┊    ┊
    0 ┤⠂⠂⠂⠂⠐⠐⠐┊⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐┊⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐
      │ ┊     ┊    ┊    ┊    ┊    ┊    ┊
      └─┬──────────┬─────────┬─────────┬────
       -175       -150      -125      -100
                      ×10³⁰⁶
    linear [0, 4.94066e-324]: dots inside, in order, ends 0 73; values inside
      │                                    ┊
    0 ┤⠂                                   ⠐
      │                                    ┊
      └┬───────────────────────────────────┬
       0                                   5
                      ×10⁻³²⁴
    linear [4.94066e-324, 1.4822e-323]: dots inside, in order, ends 0 71; values inside
      │                  ┊                ┊
    0 ┤⠂                 ⠂                ⠐
      │                  ┊                ┊
      └┬─────────────────┬────────────────┬
       5                 10               15
                     ×10⁻³²⁴
    linear [1, 1]: dots inside, in order, ends 0 71; values inside
      │                                   ┊
    0 ┤⠂                                  ⠐
      │                                   ┊
      └┬──────────────────────────────────┬
       0                                 200
                    ×10⁻¹⁸ +1
    linear [-1e-300, 1e+300]: dots inside, in order, ends 0 71; values inside
      │         ┊        ┊        ┊       ┊
    0 ┤⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠂⠂⠐⠐⠐⠐
      │         ┊        ┊        ┊       ┊
      └┬─────────────────┬────────────────┬
      0.0               0.5              1.0
                      ×10³⁰⁰
    time [-1.79769e+308, 1.79769e+308]: dots inside, in order, ends 0 73; values inside
      │
      │
    0 ┤⠂⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐ ⠂⠂⠂⠐⠐⠐ ⠂⠂⠂⠐⠐⠐⠐
      │
      │
      └─────────────────────────────────────
    log [4.94066e-324, 1.79769e+308]: dots inside, in order, ends 0 69; values inside
      │                                  ┊
    0 ┤⡀⡀⡀⡀⡀⡀⢀⢀⢀⢀⢀ ⡀⡀⡀⡀⡀⢀⢀⢀⢀⢀⢀ ⡀⡀⡀⡀⡀⢀⢀⢀⢀⢀⢀
      │                                  ┊
      │                                  ┊
      └──────────────────────────────────┬
                                       10³⁰⁸
    log [1, 1]: dots inside, in order, ends 0 71; values inside
      │                                   ┊
    0 ┤⠂                                  ⠐
      │                                   ┊
      └┬──────────────────────────────────┬
       0                                 200
                    ×10⁻¹⁸ +1
    |}]

let%expect_test "a fitted domain stays finite" =
  let l =
    C.layout ~width:30 ~height:4 C.(line ~y:Fun.id [| -1.7e308; 1.7e308 |])
  in
  (match C.Layout.domain l "y" with
  | [ Num lo; Num hi ] -> Printf.printf "%g %g\n" lo hi
  | _ -> ());
  [%expect {| -1.7e+308 1.7e+308 |}]

(* Domains whose length, or its inverse, passes the largest float. *)
let extreme_domains =
  let max = Float.max_float in
  [ (-.max, max); (-.max, -.max /. 2.); (0., 1e-310) ]

let extreme_figure (lo, hi) =
  C.(
    layer
      [
        line ~x:Fun.id ~y:(fun _ -> 0.) [| lo; hi |];
        axis ~show:false "x";
        axis ~show:false "y";
      ]
    |> scale "x" (Scale.linear ~domain:(lo, hi) ())
    |> scale "y" (Scale.linear ~domain:(-1., 1.) ()))

let num = function C.Layout.Num v -> v | Cat _ -> Float.nan

let%expect_test "zoom keeps a value at its cell on extreme domains" =
  List.iter
    (fun (lo, hi) ->
      let fig = extreme_figure (lo, hi) in
      let l = C.layout ~width:40 ~height:4 fig in
      let p = C.Layout.plot l in
      List.iter
        (fun k ->
          let kept = ref 0 and cells = ref 0 and changed = ref false in
          for cx = p.x to p.x + p.width - 1 do
            let at = (cx, p.y) in
            let v = num (Option.get (C.Layout.value l "x" at)) in
            let view = C.Layout.zoom ~axes:`X l ~at k in
            if C.View.window "x" view <> None then changed := true;
            let l' = C.layout ~view ~width:40 ~height:4 fig in
            incr cells;
            if C.Layout.position l' "x" (Num v) = cx then incr kept
          done;
          Printf.printf "[%g, %g] zoomed %g: %s, value kept at %d of %d cells\n"
            lo hi k
            (if !changed then "window set" else "unchanged")
            !kept !cells)
        [ 2.; 0.5 ])
    extreme_domains;
  [%expect
    {|
    [-1.79769e+308, 1.79769e+308] zoomed 2: window set, value kept at 40 of 40 cells
    [-1.79769e+308, 1.79769e+308] zoomed 0.5: unchanged, value kept at 40 of 40 cells
    [-1.79769e+308, -8.98847e+307] zoomed 2: window set, value kept at 40 of 40 cells
    [-1.79769e+308, -8.98847e+307] zoomed 0.5: unchanged, value kept at 40 of 40 cells
    [0, 1e-310] zoomed 2: window set, value kept at 40 of 40 cells
    [0, 1e-310] zoomed 0.5: window set, value kept at 40 of 40 cells
    |}]

let%expect_test "pan moves the data by its cells on extreme domains" =
  List.iter
    (fun (lo, hi) ->
      let fig = extreme_figure (lo, hi) in
      let l = C.layout ~width:40 ~height:4 fig in
      let p = C.Layout.plot l in
      List.iter
        (fun dx ->
          let view = C.Layout.pan l ~by:(dx, 0) in
          match C.View.window "x" view with
          | None -> Printf.printf "[%g, %g] panned %d: unchanged\n" lo hi dx
          | Some _ ->
              let l' = C.layout ~view ~width:40 ~height:4 fig in
              let moved = ref 0 and cells = ref 0 in
              for cx = p.x to p.x + p.width - 1 do
                if cx + dx >= p.x && cx + dx < p.x + p.width then begin
                  incr cells;
                  let v = num (Option.get (C.Layout.value l "x" (cx, p.y))) in
                  if C.Layout.position l' "x" (Num v) = cx + dx then incr moved
                end
              done;
              Printf.printf "[%g, %g] panned %d: moved %d of %d cells\n" lo hi
                dx !moved !cells)
        [ 3; -5 ])
    extreme_domains;
  [%expect
    {|
    [-1.79769e+308, 1.79769e+308] panned 3: unchanged
    [-1.79769e+308, 1.79769e+308] panned -5: unchanged
    [-1.79769e+308, -8.98847e+307] panned 3: unchanged
    [-1.79769e+308, -8.98847e+307] panned -5: moved 35 of 35 cells
    [0, 1e-310] panned 3: moved 37 of 37 cells
    [0, 1e-310] panned -5: moved 35 of 35 cells
    |}]

let%expect_test "clamp brings a window into an extreme domain, once" =
  let max = Float.max_float in
  List.iter
    (fun ((lo, hi), windows) ->
      let fig = extreme_figure (lo, hi) in
      List.iter
        (fun (a, b) ->
          let once =
            C.View.clamp fig (C.View.set "x" (Some (a, b)) C.View.empty)
          in
          let twice = C.View.clamp fig once in
          let w = C.View.window "x" once in
          Printf.printf "[%g, %g] window [%g, %g]: %s, %s\n" lo hi a b
            (match w with
            | None -> "removed"
            | Some (a', b') ->
                let w = b -. a and w' = b' -. a' in
                if lo <= a' && b' <= hi && Float.abs (w' -. w) <= 1e-12 *. w
                then Printf.sprintf "inside at [%g, %g]" a' b'
                else Printf.sprintf "OUTSIDE at [%g, %g]" a' b')
            (if C.View.window "x" twice = w then "again the same"
             else "MOVED AGAIN"))
        windows)
    [
      ( (-.max, -.max /. 2.),
        [ (0.75 *. max, max); (-0.6 *. max, -0.4 *. max); (-.max, 0.) ] );
      ((max /. 2., max), [ (-.max, -0.75 *. max) ]);
      ((-.max, max), [ (-.max, max); (0., max /. 2.) ]);
      ((0., 1e-310), [ (8e-311, 1.2e-310); (-3e-310, -2.5e-310) ]);
    ];
  [%expect
    {|
    [-1.79769e+308, -8.98847e+307] window [1.34827e+308, 1.79769e+308]: inside at [-1.34827e+308, -8.98847e+307], again the same
    [-1.79769e+308, -8.98847e+307] window [-1.07862e+308, -7.19077e+307]: inside at [-1.25839e+308, -8.98847e+307], again the same
    [-1.79769e+308, -8.98847e+307] window [-1.79769e+308, 0]: removed, again the same
    [8.98847e+307, 1.79769e+308] window [-1.79769e+308, -1.34827e+308]: inside at [8.98847e+307, 1.34827e+308], again the same
    [-1.79769e+308, 1.79769e+308] window [-1.79769e+308, 1.79769e+308]: removed, again the same
    [-1.79769e+308, 1.79769e+308] window [0, 8.98847e+307]: inside at [0, 8.98847e+307], again the same
    [0, 1e-310] window [8e-311, 1.2e-310]: inside at [6e-311, 1e-310], again the same
    [0, 1e-310] window [-3e-310, -2.5e-310]: inside at [0, 5e-311], again the same
    |}]

let%expect_test "a log axis over many decades labels few candidates" =
  List.iter
    (fun (lo, hi, width) ->
      let calls = ref 0 in
      let format _ v =
        incr calls;
        Printf.sprintf "%g" v
      in
      let fig =
        C.(
          layer
            [
              dot ~x:Fun.id ~y:(fun _ -> 0.) [| lo; hi |];
              axis ~format "x";
              axis ~show:false "y";
            ]
          |> scale "x" (Scale.log ~domain:(lo, hi) ()))
      in
      let g = drawn ~width ~height:4 fig in
      Printf.printf "[%g, %g] at %d cells: %d labelled values; %s\n" lo hi width
        !calls
        (String.trim (List.nth (text_rows g) 3)))
    [
      (4.9e-324, Float.max_float, 40);
      (1e-300, 1e5, 80);
      (1e-30, 1e5, 80);
      (1e-10, 1e5, 80);
      (1., 1e3, 40);
    ];
  [%expect
    {|
    [4.94066e-324, 1.79769e+308] at 40 cells: 1266 labelled values; 1e+308
    [1e-300, 100000] at 80 cells: 8579 labelled values; 1e-279  1e-248  1e-217  1e-186   1e-155  1e-124  1e-93   1e-62   1e-31     1
    [1e-30, 100000] at 80 cells: 255 labelled values; 1e-28    1e-24    1e-20    1e-16     1e-12    1e-08    0.0001     1      10000
    [1e-10, 100000] at 80 cells: 310 labelled values; 1e-10     1e-08     1e-06      0.0001     0.01        1        100      10000
    [1, 1000] at 40 cells: 106 labelled values; 1           10          100         1000
    |}]
