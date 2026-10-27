(* How a picture is laid out: a replaced element, which keeps its natural size
   where nothing stretches it, and whose measurement is its final layout. *)

open Mosaic_ui
open Windtrap
module Style = Toffee.Style
module Size = Toffee.Geometry.Size

let length = Style.Dimension.length

(* A picture of [width] by [height] pixels. *)
let picture width height =
  Matrix_grid.picture ~width ~height (String.make (4 * width * height) '\255')

(* 200 by 200 pixels: 20 by 10 cells of 10 by 20 pixels, a ratio of 2. *)
let square = picture 200 200

(* [view] laid out on a screen of [width] by [height] cells of [cell]
   pixels; the root's children, and theirs. *)
let lay_out ?(cell = (10, 20)) ?(width = 40) ?(height = 30) view =
  let renderer = Renderer.create () in
  Matrix_screen.apply_capabilities (Renderer.screen renderer)
    ~explicit_width:true ~explicit_cursor_positioning:false ~hyperlinks:true
    ~colors:`Truecolor ~styling:`Truecolor ~graphics:`Blocks
    ~cell_size:(Some cell);
  let root = Renderer.root renderer in
  Renderable.set_style root
    (Renderable.style root
    |> Style.set_width (length (Float.of_int width))
    |> Style.set_height (length (Float.of_int height)));
  let reconciler = Mosaic.Reconciler.create ~container:root ~deliver:ignore in
  Mosaic.Reconciler.render reconciler ~viewport_width:width view;
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  root

let only node =
  match Renderable.children node with
  | [ child ] -> child
  | children -> failf "%d children, not one" (List.length children)

let pair node =
  match Renderable.children node with
  | [ a; b ] -> (a, b)
  | children -> failf "%d children, not two" (List.length children)

let box node =
  ( Renderable.x node,
    Renderable.y node,
    Renderable.width node,
    Renderable.height node )

let box_t = Testable.(quad int int int int)

(* A container with [style] holding the picture and the line "next". *)
let holding ?(picture_style = Style.default) style =
  Mosaic.box ~layout:style
    [ Mosaic.picture ~layout:picture_style square; Mosaic.text "next" ]

(* The picture and the line after it, in a container with [style]. *)
let picture_and_next ?width ?height ?picture_style style =
  pair (only (lay_out ?width ?height (holding ?picture_style style)))

let column ?width () =
  let style = Style.default |> Style.set_flex_direction Column in
  match width with Some w -> Style.set_width (length w) style | None -> style

let block width =
  Style.default |> Style.set_display Block |> Style.set_width (length width)

let grid width =
  Style.default |> Style.set_display Grid |> Style.set_width (length width)

(* ───── Measurement ───── *)

(* The picture's measurement and its final layout under [inputs]. *)
let measured_and_laid_out node ~known_dimensions ~available_space ~parent_size
    ~sizing_mode =
  let measure =
    match Renderable.measure node with
    | Some measure -> measure
    | None -> fail "a picture has a measure function"
  in
  let style = Renderable.style node in
  let size run_mode =
    Toffee_compute.compute_leaf_layout
      ~inputs:
        (Toffee.Layout_input.make ~run_mode ~sizing_mode
           ~axis:Toffee_tree.Requested_axis.Both ~known_dimensions ~parent_size
           ~available_space
           ~vertical_margins_are_collapsible:Toffee.Geometry.Line.both_false)
      ~style
      ~resolve_calc_value:(fun _ _ -> 0.)
      ~measure_function:(fun known_dimensions available_space ->
        Toffee.Geometry.Sizef.of_size
          (measure ~known_dimensions ~available_space ~style))
    |> Toffee.Layout_output.size
  in
  (size Toffee.Run_mode.Compute_size, size Toffee.Run_mode.Perform_layout)

let size_t =
  Testable.make ~pp:Toffee.Geometry.Sizef.pp ~equal:Toffee.Geometry.Sizef.equal

(* A block 31 cells wide gives a child with an auto width its width. A picture
   20 cells wide measured with that width took the height its ratio gives it,
   15.5 rounded up to 16, and laid out with that width as available space took
   its own 20 cells and a height of 15.5: the block measured one box and laid
   out another. *)
let block_parent_measures_as_it_lays_out () =
  let picture =
    only
      (only
         (lay_out (Mosaic.box ~layout:(block 31.) [ Mosaic.picture square ])))
  in
  let measured, laid_out =
    measured_and_laid_out picture
      ~known_dimensions:Size.{ width = Some 31.; height = None }
      ~available_space:
        Size.
          {
            width = Toffee.Available_space.of_float 31.;
            height = Toffee.Available_space.max_content;
          }
      ~parent_size:Size.{ width = Some 31.; height = None }
      ~sizing_mode:Toffee_tree.Sizing_mode.Inherent_size
  in
  equal ~msg:"measured as laid out" size_t measured laid_out

(* A picture's measurement is its final layout, a property. For random
   pictures, cell sizes and styles (sizes, minimums and maximums in lengths and
   percentages, padding, an aspect ratio of its own, both box sizings), and
   random inputs a parent can give (known sizes, definite, min-content or
   max-content space, both sizing modes), a measurement and a final layout of
   the picture give it the same size. *)
let measurement_is_final_layout seed =
  let rng = Random.State.make [| seed |] in
  let int n = Random.State.int rng n in
  let chance p = Random.State.float rng 1. < p in
  let pick a = a.(int (Array.length a)) in
  let cells () = Float.of_int (int 40) in
  let dimension () =
    match int 3 with
    | 0 -> length (cells ())
    | 1 -> Style.Dimension.percent (Float.of_int (10 + int 90) /. 100.)
    | _ -> Style.Dimension.auto
  in
  let sometimes f = if chance 0.3 then f () else Style.Dimension.auto in
  let style =
    Style.make
      ~box_sizing:(pick [| Style.Box_sizing.Border_box; Content_box |])
      ~size:{ width = sometimes dimension; height = sometimes dimension }
      ~min_size:{ width = sometimes dimension; height = sometimes dimension }
      ~max_size:{ width = sometimes dimension; height = sometimes dimension }
      ?aspect_ratio:(if chance 0.1 then Some (pick [| 0.5; 1.; 3. |]) else None)
      ~padding:
        (Toffee.Geometry.Rect.all
           (Style.Length_percentage.length (Float.of_int (int 3))))
      ()
  in
  let cell = (1 + int 20, 1 + int 40) in
  let pixels = picture (1 + int 400) (1 + int 400) in
  let node =
    only
      (only
         (lay_out ~cell (Mosaic.box [ Mosaic.picture ~layout:style pixels ])))
  in
  let known () = if chance 0.5 then Some (cells ()) else None in
  let available () =
    match int 3 with
    | 0 -> Toffee.Available_space.of_float (cells ())
    | 1 -> Toffee.Available_space.min_content
    | _ -> Toffee.Available_space.max_content
  in
  let measured, laid_out =
    measured_and_laid_out node
      ~known_dimensions:Size.{ width = known (); height = known () }
      ~available_space:Size.{ width = available (); height = available () }
      ~parent_size:Size.{ width = known (); height = known () }
      ~sizing_mode:
        (pick [| Toffee_tree.Sizing_mode.Inherent_size; Content_size |])
  in
  if not (Toffee.Geometry.Sizef.equal measured laid_out) then
    failf "seed %d: measured %s, laid out %s" seed
      (Toffee.Geometry.Sizef.to_string measured)
      (Toffee.Geometry.Sizef.to_string laid_out)

(* ───── Parents ───── *)

(* A column 30 wide stretches the picture to its width, its height following
   through the ratio, and the next line starts under it. *)
let stretched_column () =
  let picture, next = picture_and_next (column ~width:30. ()) in
  equal ~msg:"picture" box_t (0, 0, 30, 15) (box picture);
  equal ~msg:"next" int 15 (Renderable.y next)

(* A column narrower than the picture shrinks it with it. *)
let narrow_column () =
  let picture, next = picture_and_next (column ~width:12. ()) in
  equal ~msg:"picture" box_t (0, 0, 12, 6) (box picture);
  equal ~msg:"next" int 6 (Renderable.y next)

(* A column of the content's width, on a screen narrower than the picture,
   shrinks to the screen and the picture with it. *)
let column_on_a_narrow_screen () =
  let picture, next = picture_and_next ~width:12 (column ()) in
  equal ~msg:"picture" box_t (0, 0, 12, 6) (box picture);
  equal ~msg:"next" int 6 (Renderable.y next)

(* A block 30 wide does not stretch the picture: it keeps its 20 by 10
   cells, and the next line starts under it. *)
let block_keeps_natural_size () =
  let picture, next = picture_and_next (block 30.) in
  equal ~msg:"picture" box_t (0, 0, 20, 10) (box picture);
  equal ~msg:"next" int 10 (Renderable.y next)

(* A block narrower than the picture fits it in its width. *)
let narrow_block () =
  let picture, next = picture_and_next (block 12.) in
  equal ~msg:"picture" box_t (0, 0, 12, 6) (box picture);
  equal ~msg:"next" int 6 (Renderable.y next)

(* In a row of its content's height the picture keeps its natural size and the
   next item follows it. *)
let row () =
  let row =
    Mosaic.box ~layout:Style.default
      [ Mosaic.picture square; Mosaic.text "next" ]
  in
  let picture, next =
    pair (only (only (lay_out (Mosaic.box ~layout:(column ()) [ row ]))))
  in
  equal ~msg:"picture" box_t (0, 0, 20, 10) (box picture);
  equal ~msg:"next" int 20 (Renderable.x next)

(* A grid's normal alignment keeps the picture's natural size in an area 30
   wide. *)
let grid_keeps_natural_size () =
  let picture, _ = picture_and_next (grid 30.) in
  equal ~msg:"picture" box_t (0, 0, 20, 10) (box picture)

(* A width the style gives sets the height through the ratio; both sizes give
   the box. *)
let explicit_size () =
  let size width height = Style.make ~size:Size.{ width; height } () in
  let picture, next =
    picture_and_next
      ~picture_style:(size (length 6.) Style.Dimension.auto)
      (column ~width:30. ())
  in
  equal ~msg:"width 6" box_t (0, 0, 6, 3) (box picture);
  equal ~msg:"next" int 3 (Renderable.y next);
  let picture, _ =
    picture_and_next
      ~picture_style:(size (length 6.) (length 6.))
      (column ~width:30. ())
  in
  equal ~msg:"6 by 6" box_t (0, 0, 6, 6) (box picture);
  let picture, _ =
    picture_and_next
      ~picture_style:(size (length 6.) Style.Dimension.auto)
      (block 30.)
  in
  equal ~msg:"width 6 in a block" box_t (0, 0, 6, 3) (box picture)

let () =
  run "mosaic.picture"
    [
      group "Measurement"
        [
          test "A block parent measures it as it lays it out"
            block_parent_measures_as_it_lays_out;
          prop ~count:500 "Its measurement is its final layout" Gen.int
            measurement_is_final_layout;
        ];
      group "Parents"
        [
          test "A stretched column" stretched_column;
          test "A narrow column" narrow_column;
          test "A column on a narrow screen" column_on_a_narrow_screen;
          test "A block keeps its natural size" block_keeps_natural_size;
          test "A narrow block" narrow_block;
          test "A row" row;
          test "A grid keeps its natural size" grid_keeps_natural_size;
          test "An explicit size" explicit_size;
        ];
    ]
