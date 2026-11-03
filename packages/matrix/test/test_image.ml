module Image = Matrix_image
open Windtrap
open Matrix

let idx grid x y = (y * Grid.width grid) + x

let read_bg grid x y =
  let i = idx grid x y in
  Ansi.Color.to_rgba (Grid.get_bg grid i)

let text_width_method_is_applied_during_render () =
  let grid = Grid.create ~width:6 ~height:1 ~width_method:`Unicode () in
  let img = Image.text ~width_method:`Wcwidth "👩\u{200D}🚀" in

  Image.draw img grid;

  let start0 = Grid.get_text grid (Grid.idx grid ~x:0 ~y:0) in
  let start2 = Grid.get_text grid (Grid.idx grid ~x:2 ~y:0) in
  is_true ~msg:"first grapheme start written" (start0 <> " ");
  is_true ~msg:"second grapheme start written at x=2" (start2 <> " ");
  is_true ~msg:"render restores grid width method"
    (Grid.width_method grid = `Unicode)

let box_without_fill_preserves_background () =
  let grid = Grid.create ~width:4 ~height:3 () in
  let bg = Ansi.Color.of_rgb 5 15 25 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:3 ~color:bg;
  let border_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let img = Image.box ~border_style ~width:4 ~height:3 () in
  Image.draw img grid;
  let er, eg, eb, ea = Ansi.Color.to_rgba bg in
  equal ~msg:"border cell keeps existing bg"
    (pair int (pair int (pair int int)))
    (er, (eg, (eb, ea)))
    (let r, g, b, a = read_bg grid 0 0 in
     (r, (g, (b, a))))

let cell_written grid x y = Grid.get_text grid (Grid.idx grid ~x ~y) <> " "

let cropped_padding_stays_empty () =
  let image =
    Image.text "ABC" |> Image.with_hit ~id:1 |> Image.hcrop 0 2
    |> Image.hpad 0 2 |> Image.hcrop 2 0
  in
  let grid = Grid.create ~width:3 ~height:1 () in
  let hits = Screen.Hit_grid.create ~width:3 ~height:1 in
  Image.draw ~hits ~x:1 image grid;
  equal ~msg:"cropping into padding does not reveal hidden text" string " "
    (Grid.get_text grid (Grid.idx grid ~x:1 ~y:0));
  equal ~msg:"hidden text has no hit region" int Screen.Hit_grid.empty_id
    (Screen.Hit_grid.get hits ~x:1 ~y:0)

let composed_clips_stay_empty () =
  let child = Image.text "ABC" |> Image.with_hit ~id:1 |> Image.hcrop 0 2 in
  let padding =
    Image.fill ~color:Ansi.Color.transparent ~width:2 ~height:1 ()
  in
  let image = Image.hcat [ child; padding ] |> Image.hcrop 2 0 in
  let grid = Grid.create ~width:3 ~height:1 () in
  let hits = Screen.Hit_grid.create ~width:3 ~height:1 in
  Image.draw ~hits ~x:1 image grid;
  equal ~msg:"disjoint parent and child clips do not reveal hidden text" string
    " "
    (Grid.get_text grid (Grid.idx grid ~x:1 ~y:0));
  equal ~msg:"disjoint clips do not register hidden hits" int
    Screen.Hit_grid.empty_id
    (Screen.Hit_grid.get hits ~x:1 ~y:0)

let custom_hits_follow_crop () =
  let image =
    Image.custom ~width:5 ~height:1 (fun grid hits ~x ~y ->
        Grid.draw_text grid ~x ~y ~text:"ABCDE";
        Option.iter
          (fun hits -> Screen.Hit_grid.add hits ~x ~y ~width:5 ~height:1 ~id:7)
          hits)
    |> Image.hcrop 1 1
  in
  let grid = Grid.create ~width:7 ~height:1 () in
  let hits = Screen.Hit_grid.create ~width:7 ~height:1 in
  Image.draw ~hits ~x:2 image grid;
  for x = 0 to 6 do
    let visible = x >= 2 && x < 5 in
    equal
      ~msg:(Printf.sprintf "cropped custom hit at %d" x)
      int
      (if visible then 7 else Screen.Hit_grid.empty_id)
      (Screen.Hit_grid.get hits ~x ~y:0);
    equal
      ~msg:(Printf.sprintf "cropped custom visual at %d" x)
      bool visible (cell_written grid x 0)
  done;
  Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:7 ~height:1 ~id:8;
  equal ~msg:"custom clip is popped" int 8 (Screen.Hit_grid.get hits ~x:0 ~y:0)

let hit_rect_is_clipped_to_the_image () =
  let image =
    Image.void 3 2 |> Image.with_hit_rect ~id:4 ~x:(-1) ~y:1 ~width:6 ~height:3
  in
  let hits = Screen.Hit_grid.create ~width:7 ~height:5 in
  Image.draw ~hits ~x:2 ~y:1 image (Grid.create ~width:7 ~height:5 ());
  for y = 0 to 4 do
    for x = 0 to 6 do
      let inside = x >= 2 && x < 5 && y = 2 in
      equal
        ~msg:(Printf.sprintf "hit at (%d, %d)" x y)
        int
        (if inside then 4 else Screen.Hit_grid.empty_id)
        (Screen.Hit_grid.get hits ~x ~y)
    done
  done;
  let outside =
    Image.void 3 2 |> Image.with_hit_rect ~id:4 ~x:3 ~y:0 ~width:2 ~height:2
  in
  let hits = Screen.Hit_grid.create ~width:7 ~height:5 in
  Image.draw ~hits outside (Grid.create ~width:7 ~height:5 ());
  equal ~msg:"a rectangle beside the image adds nothing" int
    Screen.Hit_grid.empty_id
    (Screen.Hit_grid.get hits ~x:3 ~y:0)

(* Every value that would return an empty image returns [empty]. *)
let hit_rect_on_empty_is_empty () =
  is_true ~msg:"with_hit_rect on empty"
    (Image.with_hit_rect ~id:4 ~x:0 ~y:0 ~width:2 ~height:2 Image.empty
    == Image.empty);
  is_true ~msg:"with_hit on empty"
    (Image.with_hit ~id:4 Image.empty == Image.empty)

(* Every crop clips what remains, a crop of nothing included. *)
let crop_of_nothing_clips () =
  let wide =
    Image.custom ~width:3 ~height:1 (fun grid _ ~x ~y ->
        Grid.draw_text grid ~x ~y ~text:"ABCDE")
  in
  let grid = Grid.create ~width:6 ~height:1 () in
  Image.draw (Image.crop wide) grid;
  for x = 0 to 5 do
    equal ~msg:(Printf.sprintf "cell %d" x) bool (x < 3) (cell_written grid x 0)
  done

let hsnap_defaults_to_center () =
  let grid = Grid.create ~width:3 ~height:1 () in
  let img = Image.hsnap 3 (Image.text "x") in
  Image.draw img grid;
  is_false ~msg:"left cell is padding" (cell_written grid 0 0);
  is_true ~msg:"content centered at x=1" (cell_written grid 1 0);
  is_false ~msg:"right cell is padding" (cell_written grid 2 0)

let vsnap_defaults_to_middle () =
  let grid = Grid.create ~width:1 ~height:3 () in
  let img = Image.vsnap 3 (Image.text "x") in
  Image.draw img grid;
  is_false ~msg:"top cell is padding" (cell_written grid 0 0);
  is_true ~msg:"content centered at y=1" (cell_written grid 0 1);
  is_false ~msg:"bottom cell is padding" (cell_written grid 0 2)

let fill_defaults_to_black () =
  let grid = Grid.create ~width:2 ~height:1 () in
  let img = Image.fill ~width:1 ~height:1 () in
  Image.draw img grid;
  let _, _, _, alpha = read_bg grid 0 0 in
  satisfies ~claim:"filled cell has an opaque background (alpha > 0)" int
    (fun a -> a > 0)
    alpha;
  let _, _, _, untouched_alpha = read_bg grid 1 0 in
  equal ~msg:"unfilled cell keeps the terminal default" int 0 untouched_alpha

let () =
  exit
    (Windtrap.run "matrix.image"
       [
         group "render"
           [
             test "text width_method is applied during render"
               text_width_method_is_applied_during_render;
             test "box without fill preserves background"
               box_without_fill_preserves_background;
             test "cropped padding stays empty" cropped_padding_stays_empty;
             test "composed clips stay empty" composed_clips_stay_empty;
             test "custom hits follow crop" custom_hits_follow_crop;
             test "a crop of nothing clips" crop_of_nothing_clips;
             test "a hit rectangle is clipped to the image"
               hit_rect_is_clipped_to_the_image;
             test "a hit region on empty is empty" hit_rect_on_empty_is_empty;
           ];
         group "defaults"
           [
             test "hsnap defaults to `Center" hsnap_defaults_to_center;
             test "vsnap defaults to `Middle" vsnap_defaults_to_middle;
             test "fill defaults to black" fill_defaults_to_black;
           ];
       ])
