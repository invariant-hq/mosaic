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
    1 ┤⠉⠒⠒⠤⠤⣀⣀       ⣀─ up
      │       ⠉⠑⠒⠢⠤⢄⣀ ─ down
    0 ┤⣀⠤⠤⠒⠒⠉⠉       ⠉⠉⠒⠒⠤⠤⣀
      └┬───────────────────┬
       0                   1
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
