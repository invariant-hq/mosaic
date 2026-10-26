open Windtrap
open Mosaic_ui
open Test_harness

(* ───── Glyphs ───── *)

(* Each glyph of a set with its name and whether it is drawn in one cell. *)
let fields (g : Glyphs.t) =
  [
    ("cursor", g.cursor, false);
    ("cursor_inactive", g.cursor_inactive, false);
    ("checked", g.checked, false);
    ("unchecked", g.unchecked, false);
    ("expand", g.expand, false);
    ("collapse", g.collapse, false);
    ("leaf", g.leaf, false);
    ("ellipsis", g.ellipsis, false);
    ("thumb", g.thumb, true);
    ("more_before", g.more_before, true);
    ("more_after", g.more_after, true);
    ("underline", g.underline, true);
    ("arrow_up", g.arrow_up, true);
    ("arrow_down", g.arrow_down, true);
    ("arrow_left", g.arrow_left, true);
    ("arrow_right", g.arrow_right, true);
  ]

let width s = Matrix.Text.measure ~width_method:`Unicode ~tab_width:2 s

let ascii_set () =
  List.iter2
    (fun (name, unicode, one_cell) (_, ascii, _) ->
      is_true
        ~msg:(name ^ ": an ASCII glyph")
        (ascii <> "" && String.for_all (fun c -> Char.code c < 128) ascii);
      is_true ~msg:(name ^ ": a Unicode glyph") (unicode <> "");
      if one_cell then (
        equal ~msg:(name ^ ": one cell in unicode") int 1 (width unicode);
        equal ~msg:(name ^ ": one cell in ascii") int 1 (width ascii)))
    (fields Glyphs.unicode) (fields Glyphs.ascii);
  is_true ~msg:"the cursor glyphs differ"
    (Glyphs.unicode.cursor <> Glyphs.unicode.cursor_inactive
    && Glyphs.ascii.cursor <> Glyphs.ascii.cursor_inactive);
  is_true ~msg:"the checks differ"
    (Glyphs.ascii.checked <> Glyphs.ascii.unchecked);
  is_false (Glyphs.equal Glyphs.unicode Glyphs.ascii);
  is_true (Glyphs.equal Glyphs.ascii { Glyphs.ascii with cursor = ">" })

(* ───── Column widths ───── *)

type row = { name : string; age : string; city : string }

let rows =
  [
    { name = "Alice"; age = "30"; city = "New York" };
    { name = "Bob"; age = "25"; city = "London" };
    { name = "Charlie"; age = "35"; city = "Paris" };
  ]

let widths ?padding ~available columns =
  Array.to_list
    (Column.widths ~width_method:`Unicode ?padding ~available columns
       (Items.of_list rows))

let name = Column.make ~header:"Name" (fun r -> Column.text r.name)

let rules () =
  equal ~msg:"auto: the widest cell" (list int) [ 7 ]
    (widths ~available:40 [ name ]);
  equal ~msg:"auto: the header when wider" (list int) [ 11 ]
    (widths ~available:40
       [ Column.make ~header:"Name of one" (fun r -> Column.text r.name) ]);
  equal ~msg:"auto with padding" (list int) [ 9 ]
    (widths ~padding:1 ~available:40 [ name ]);
  equal ~msg:"auto bounded" (list int) [ 5; 9 ]
    (widths ~available:40
       [
         Column.make ~max_width:5 (fun r -> Column.text r.name);
         Column.make ~min_width:9 (fun r -> Column.text r.age);
       ]);
  equal ~msg:"fixed" (list int) [ 4; 6 ]
    (widths ~padding:1 ~available:40
       [
         Column.make ~width:(`Fixed 2) (fun r -> Column.text r.age);
         Column.make ~width:(`Fixed 4) ~max_width:1 (fun r -> Column.text r.age);
       ]);
  equal ~msg:"fractions share the rest, rounded down, gaps taken" (list int)
    [ 7; 10; 20 ]
    (widths ~available:40
       [
         name;
         Column.make ~width:(`Fraction 1.) (fun r -> Column.text r.city);
         Column.make ~width:(`Fraction 2.) (fun r -> Column.text r.city);
       ]);
  equal ~msg:"a fraction is one cell wider than its padding" (list int) [ 7; 3 ]
    (widths ~padding:1 ~available:8
       [
         Column.make ~width:(`Fixed 5) (fun r -> Column.text r.age);
         Column.make ~width:(`Fraction 1.) (fun r -> Column.text r.city);
       ]);
  equal ~msg:"rich cells measure their text" (list int) [ 9 ]
    (widths ~available:40
       [
         Column.make (fun r ->
             Column.rich
               [
                 Text.Span
                   {
                     style = None;
                     children =
                       [ Text.Text { text = r.name ^ "!!"; style = None } ];
                   };
               ]);
       ]);
  equal ~msg:"no columns" (list int) [] (widths ~available:40 [])

(* A table draws its columns at the widths it resolves, with a separator in
   each gap: the separators' places give the widths. *)
let table_widths ~padding ~available columns =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let table_column (c : row Column.t) =
    Table.column ~alignment:c.alignment ?min_width:c.min_width
      ?max_width:c.max_width
      ~width:
        (match c.width with
        | `Auto -> `Auto
        | `Fixed n -> `Fixed n
        | `Fraction f -> `Flex f)
      c.header
  in
  let cells r =
    Array.of_list
      (List.map
         (fun (c : row Column.t) ->
           match c.header with
           | "Name" -> Table.cell r.name
           | "Age" -> Table.cell r.age
           | _ -> Table.cell r.city)
         columns)
  in
  let width = available + 2 in
  let table =
    Table.create ~parent:root
      ~columns:(List.map table_column columns)
      ~rows:(List.map cells rows) ~cell_padding:padding
      ~show_column_separator:true ()
  in
  let node = Table.node table in
  layout_node node ~x:0 ~y:0 ~width ~height:8;
  let grid = make_grid ~width ~height:8 () in
  Renderable.Private.render_full node ~grid ~dt:0.;
  let separators =
    List.filter
      (fun x ->
        Matrix_grid.get_text grid (Matrix_grid.idx grid ~x ~y:1) = "\u{2502}")
      (List.init width Fun.id)
  in
  let rec gaps = function
    | a :: (b :: _ as rest) -> (b - a - 1) :: gaps rest
    | _ -> []
  in
  gaps separators

let as_table () =
  let columns =
    [
      name;
      Column.make ~header:"Age" ~width:(`Fixed 5) (fun r -> Column.text r.age);
      Column.make ~header:"City" ~width:(`Fraction 1.) ~min_width:3 (fun r ->
          Column.text r.city);
    ]
  in
  List.iter
    (fun (padding, available) ->
      equal
        ~msg:(Printf.sprintf "padding %d, %d cells" padding available)
        (list int)
        (table_widths ~padding ~available columns)
        (widths ~padding ~available columns))
    [ (0, 40); (1, 40); (2, 30); (0, 18) ]

let () =
  run "mosaic.column"
    [
      group "Glyphs" [ test "an ASCII glyph for every Unicode one" ascii_set ];
      group "Column widths"
        [ test "rules" rules; test "as a table resolves them" as_table ];
    ]
