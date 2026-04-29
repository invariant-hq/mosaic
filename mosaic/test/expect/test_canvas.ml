open Mosaic_ui
open Expect_harness

let%expect_test "empty canvas" =
  print_canvas ~width:10 ~height:3 (fun _ -> ()) ();
  [%expect_exact {|


|}]

let%expect_test "draw text at origin" =
  print_canvas ~width:10 ~height:3
    (fun c -> Grid.draw_text (Canvas.grid c) ~x:0 ~y:0 ~text:"Hello")
    ();
  [%expect_exact {|
Hello

|}]

let%expect_test "draw text at offset" =
  print_canvas ~width:15 ~height:5
    (fun c -> Grid.draw_text (Canvas.grid c) ~x:5 ~y:3 ~text:"Hello")
    ();
  [%expect_exact {|



     Hello
|}]

let%expect_test "after clear" =
  print_canvas ~width:10 ~height:3
    (fun c ->
      Grid.draw_text (Canvas.grid c) ~x:0 ~y:0 ~text:"Hello";
      Canvas.clear c)
    ();
  [%expect_exact {|


|}]

let%expect_test "fill rect" =
  print_canvas_ansi ~width:10 ~height:5
    (fun c ->
      Grid.fill_rect (Canvas.grid c) ~x:2 ~y:1 ~width:6 ~height:3
        ~color:Ansi.Color.white)
    ();
  [%expect_exact
    {|
[0;38;2;255;255;255m          [0m
[0;38;2;255;255;255m  [0;38;2;255;255;255;48;5;7m      [0;38;2;255;255;255m  [0m
[0;38;2;255;255;255m  [0;38;2;255;255;255;48;5;7m      [0;38;2;255;255;255m  [0m
[0;38;2;255;255;255m  [0;38;2;255;255;255;48;5;7m      [0;38;2;255;255;255m  [0m
[0;38;2;255;255;255m          [0m|}]

let%expect_test "multiple draws" =
  print_canvas ~width:15 ~height:3
    (fun c ->
      let g = Canvas.grid c in
      Grid.draw_text g ~x:0 ~y:0 ~text:"First";
      Grid.draw_text g ~x:0 ~y:1 ~text:"Second";
      Grid.draw_text g ~x:0 ~y:2 ~text:"Third")
    ();
  [%expect_exact {|
First
Second
Third|}]

let%expect_test "auto resize reflected" =
  let renderer = Renderer.create () in
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  set_viewport renderer ~width:10 ~height:3;
  Renderer.render_frame renderer ~width:10 ~height:3 ~delta:0.;
  Grid.draw_text (Canvas.grid canvas) ~x:0 ~y:0 ~text:"Small";
  Renderer.render_frame renderer ~width:10 ~height:3 ~delta:0.;
  print_newline ();
  print_string (grid_to_text (Canvas.grid canvas));
  set_viewport renderer ~width:15 ~height:5;
  Renderer.render_frame renderer ~width:15 ~height:5 ~delta:0.;
  Grid.draw_text (Canvas.grid canvas) ~x:0 ~y:3 ~text:"Bigger";
  Renderer.render_frame renderer ~width:15 ~height:5 ~delta:0.;
  print_newline ();
  print_string (grid_to_text (Canvas.grid canvas));
  [%expect_exact {|
Small


Small


Bigger
|}]

let%expect_test "draw box" =
  print_canvas ~width:10 ~height:5
    (fun c -> Canvas.draw_box c ~x:1 ~y:0 ~width:7 ~height:4 ())
    ();
  [%expect_exact {|
 ┌─────┐
 │     │
 │     │
 └─────┘
|}]

let%expect_test "draw line" =
  print_canvas ~width:10 ~height:3
    (fun c -> Canvas.draw_line c ~x1:0 ~y1:1 ~x2:8 ~y2:1 ())
    ();
  [%expect_exact {|

─────────
|}]

let%expect_test "wide characters" =
  print_canvas ~width:15 ~height:2
    (fun c -> Canvas.draw_text c ~x:0 ~y:0 ~text:"Hi 🌍!")
    ();
  [%expect_exact {|
Hi 🌍!
|}]

let%expect_test "blit to correct parent position" =
  let renderer = Renderer.create () in
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  set_viewport renderer ~width:20 ~height:10;
  Renderer.render_frame renderer ~width:20 ~height:10 ~delta:0.;
  Grid.draw_text (Canvas.grid canvas) ~x:0 ~y:0 ~text:"X";
  Renderer.render_frame renderer ~width:20 ~height:10 ~delta:0.;
  let grid = Screen.next_grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_text grid);
  [%expect_exact {|
X








|}]
