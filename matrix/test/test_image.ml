open Windtrap
open Matrix

let idx grid x y = (y * Grid.width grid) + x
let to_byte f = Float.round (f *. 255.) |> int_of_float

let read_bg grid x y =
  let i = idx grid x y in
  ( to_byte (Grid.get_bg_r grid i),
    to_byte (Grid.get_bg_g grid i),
    to_byte (Grid.get_bg_b grid i),
    to_byte (Grid.get_bg_a grid i) )

let text_width_method_is_applied_during_render () =
  let grid = Grid.create ~width:6 ~height:1 ~width_method:`Unicode () in
  let img = Image.text ~width_method:`No_zwj "👩\u{200D}🚀" in

  Image.render grid img;

  let start0 = Grid.get_cell grid (Grid.idx grid ~x:0 ~y:0) in
  let start2 = Grid.get_cell grid (Grid.idx grid ~x:2 ~y:0) in
  is_true ~msg:"first grapheme start written" (start0 <> Grid.Cell.space);
  is_true ~msg:"second grapheme start written at x=2" (start2 <> Grid.Cell.space);
  is_true ~msg:"render restores grid width method"
    (Grid.width_method grid = `Unicode)

let box_without_fill_preserves_background () =
  let grid = Grid.create ~width:4 ~height:3 () in
  let bg = Ansi.Color.of_rgb 5 15 25 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:3 ~color:bg;
  let border_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let img = Image.box ~border_style ~width:4 ~height:3 () in
  Image.render grid img;
  let er, eg, eb, ea = Ansi.Color.to_rgba bg in
  equal ~msg:"border cell keeps existing bg"
    (pair int (pair int (pair int int)))
    (er, (eg, (eb, ea)))
    (let r, g, b, a = read_bg grid 0 0 in
     (r, (g, (b, a))))

let () =
  Windtrap.run "matrix.image"
    [
      group "render"
        [
          test "text width_method is applied during render"
            text_width_method_is_applied_during_render;
          test "box without fill preserves background"
            box_without_fill_preserves_background;
        ];
    ]
