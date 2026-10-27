open Mosaic_ui
open Expect_harness

(* A 40 by 40 picture: 4 by 2 cells of 10 by 20 pixels. *)
let cat =
  Matrix_grid.picture ~width:40 ~height:40 (String.make (4 * 40 * 40) '\255')

let show ?cell_size ~width vnode =
  let renderer = Renderer.create () in
  Matrix_screen.apply_capabilities (Renderer.screen renderer)
    ~explicit_width:true ~explicit_cursor_positioning:false ~hyperlinks:true
    ~colors:`Truecolor ~styling:`Truecolor ~graphics:`Blocks ~cell_size;
  set_viewport renderer ~width ~height:4;
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  Mosaic.Reconciler.render reconciler ~viewport_width:width vnode;
  Renderer.render_frame renderer ~width ~height:4 ~dt:0.;
  print_newline ();
  print_string
    (grid_to_text (Matrix_screen.next_grid (Renderer.screen renderer)))

let column children =
  Mosaic.box
    ~layout:
      (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column ())
    children

let%expect_test "a picture takes its size in cells" =
  show ~cell_size:(10, 20) ~width:10 (column [ Mosaic.picture cat ]);
  [%expect_exact {|
￼￼￼￼
￼￼￼￼

|}]

let%expect_test "at 10 by 20 pixels a cell while the size is unknown" =
  show ~width:10 (column [ Mosaic.picture cat ]);
  [%expect_exact {|
￼￼￼￼
￼￼￼￼

|}]

let%expect_test "smaller cells, more of them" =
  show ~cell_size:(5, 10) ~width:10 (column [ Mosaic.picture cat ]);
  [%expect_exact {|
￼￼￼￼￼￼￼￼
￼￼￼￼￼￼￼￼
￼￼￼￼￼￼￼￼
￼￼￼￼￼￼￼￼|}]

let%expect_test "narrower than the picture, it shrinks with its ratio" =
  show ~cell_size:(10, 20) ~width:2 (column [ Mosaic.picture cat ]);
  [%expect_exact {|
￼￼


|}]

let%expect_test "a width the style gives sets the height" =
  show ~cell_size:(10, 20) ~width:10
    (column
       [
         Mosaic.picture
           ~layout:
             (Toffee.Style.make
                ~size:
                  (Toffee.Geometry.Size.make
                     (Toffee.Style.Dimension.length 6.)
                     Toffee.Style.Dimension.auto)
                ())
           cat;
       ]);
  [%expect_exact {|
￼￼￼￼￼￼
￼￼￼￼￼￼
￼￼￼￼￼￼
|}]
