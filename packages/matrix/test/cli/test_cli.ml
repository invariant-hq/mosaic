open Windtrap
module Table = Matrix_cli.Table
module Tree = Matrix_cli.Tree
module Grid = Matrix_grid
module Layout = Matrix_text.Layout

let measure = Matrix_text.measure ~width_method:`Unicode ~tab_width:2
let col ?(floor = 1) ?(grow = 0) min max = { Table.floor; min; max; grow }
let ints = list int
let widths cols room = Array.to_list (Table.widths (Array.of_list cols) ~room)
let light = { Table.edge = Some `Light; line = Some `Light; pad = 1 }

(* The text of a table of frame [f] and columns [ws] whose rows are [rows],
   each [Some cells] or [None] for a rule. *)
let frame_text ?glyphs f ws rows =
  let g =
    Grid.create
      ~width:(max 1 (Table.width f ws))
      ~height:(max 1 (Table.height f rows))
      ()
  in
  Table.draw ?glyphs g ~x:0 ~y:0 f ws rows;
  Grid.to_text g

(* ───── Widths ───── *)

let width_tests =
  [
    test "content that fits takes its max-content widths" (fun () ->
        equal ~msg:"no growth" ints [ 10; 4 ] (widths [ col 3 10; col 4 4 ] 40);
        equal ~msg:"growth by weight, ties to the earlier" ints [ 24; 4; 12 ]
          (widths [ col ~grow:3 3 10; col 4 4; col ~grow:1 1 8 ] 40);
        equal ~msg:"exact" ints [ 10; 4 ] (widths [ col 3 10; col 4 4 ] 14));
    test "between min and max, slack is shared by max - min" (fun () ->
        equal ~msg:"proportional" ints [ 40; 20 ]
          (widths [ col 10 100; col 10 40 ] 60);
        equal ~msg:"largest remainders, ties to the earlier" ints [ 6; 5 ]
          (widths [ col 1 10; col 1 10 ] 11));
    test "below min-content, columns go down to their floors" (fun () ->
        equal ~msg:"words break" ints [ 3; 4 ]
          (widths [ col ~floor:1 5 10; col ~floor:2 8 8 ] 7);
        equal ~msg:"floors" ints [ 2; 2 ]
          (widths [ col ~floor:2 5 10; col ~floor:2 8 8 ] 3);
        equal ~msg:"no columns" ints [] (widths [] 10));
    test "widths rejects a column out of order" (fun () ->
        raises_match (Exn.invalid_arg ~substring:"Table.widths") (fun () ->
            ignore (widths [ col 5 4 ] 10));
        raises_match (Exn.invalid_arg ~substring:"Table.widths") (fun () ->
            ignore (widths [ col ~floor:3 2 4 ] 10));
        raises_match (Exn.invalid_arg ~substring:"Table.widths") (fun () ->
            ignore (widths [ col ~grow:(-1) 2 4 ] 10)));
  ]

(* ───── Placing and drawing ───── *)

let regions placed =
  List.map
    (fun (p : Table.placed) ->
      ( (p.row.x, (p.row.y, (p.row.width, p.row.height))),
        Array.to_list
          (Array.map (fun (r : Grid.region) -> (r.x, r.width)) p.cells) ))
    placed

let region_list =
  list (pair (pair int (pair int (pair int int))) (list (pair int int)))

let place_tests =
  [
    test "cells sit after the edge, the padding and the lines" (fun () ->
        let rows = Table.[ Row 2; Rule `Light; Spanned (1, [ 2 ]) ] in
        equal ~msg:"regions" region_list
          [
            ((1, (5, (17, 2))), [ (3, 4); (10, 6) ]);
            ((1, (7, (17, 1))), []);
            ((1, (8, (17, 1))), [ (3, 13) ]);
          ]
          (regions (Table.place ~x:1 ~y:4 light [| 4; 6 |] rows));
        equal ~msg:"width" int 17 (Table.width light [| 4; 6 |]);
        equal ~msg:"chrome" int 7 (Table.chrome light 2);
        equal ~msg:"height" int 6 (Table.height light rows));
    test "a table without an edge has no padding at its sides" (fun () ->
        let f = { Table.edge = None; line = None; pad = 1 } in
        equal ~msg:"regions" region_list
          [ ((0, (0, (6, 1))), [ (0, 2); (4, 2) ]) ]
          (regions (Table.place ~x:0 ~y:0 f [| 2; 2 |] [ Table.Row 1 ]));
        equal ~msg:"chrome" int 2 (Table.chrome f 2));
    test "place rejects spans that do not fit" (fun () ->
        let bad spans () =
          ignore
            (Table.place ~x:0 ~y:0 light [| 1; 1 |]
               [ Table.Spanned (1, spans) ])
        in
        raises_match (Exn.invalid_arg ~substring:"Table.place") (bad [ 1 ]);
        raises_match (Exn.invalid_arg ~substring:"Table.place") (bad [ 3 ]);
        raises_match (Exn.invalid_arg ~substring:"Table.place") (bad [ 0; 2 ]));
    test "a double edge over spanned rows keeps single inner tees" (fun () ->
        equal ~msg:"frame" string
          "╔══════╤════════╤═════╗\n\
           ║      │        │     ║\n\
           ╟──────┼────────┴─────╢\n\
           ║      │              ║\n\
           ╟──────┴──────────────╢\n\
           ║                     ║\n\
           ╟──────┬────────┬─────╢\n\
           ║      │        │     ║\n\
           ╚══════╧════════╧═════╝"
          (frame_text
             { Table.edge = Some `Double; line = Some `Light; pad = 0 }
             [| 6; 8; 5 |]
             Table.
               [
                 Row 1;
                 Rule `Light;
                 Spanned (1, [ 1; 2 ]);
                 Rule `Light;
                 Spanned (1, [ 3 ]);
                 Rule `Light;
                 Row 1;
               ]));
    test "a heavy rule under the header, rows without rules" (fun () ->
        equal ~msg:"frame" string
          "┌───┬──┐\n│   │  │\n┝━━━┿━━┥\n│   │  │\n│   │  │\n└───┴──┘"
          (frame_text light [| 1; 0 |]
             Table.[ Row 1; Rule `Heavy; Row 1; Row 1 ]));
    test "drawing far from the origin allocates only for the table" (fun () ->
        let g = Grid.create ~width:20 ~height:5 () in
        let rows = Table.[ Row 1; Rule `Light; Row 1 ] in
        let draw x y =
          let before = Gc.allocated_bytes () in
          Table.draw g ~x ~y light [| 3; 4 |] rows;
          Gc.allocated_bytes () -. before
        in
        ignore (draw 0 0);
        let near = draw 0 0 and far = draw 200_000 200_000 in
        is_true
          ~msg:(Printf.sprintf "%.0f bytes near, %.0f far" near far)
          (far <= near +. 256.));
    test "the preamble's example prints a framed table" (fun () ->
        let text =
          let open Matrix_cli in
          let rows =
            [ Some [| "name"; "size" |]; None; Some [| "a.ml"; "12" |] ]
          in
          let column i =
            let need = function Some r -> measure r.(i) | None -> 0 in
            let w = List.fold_left (fun a r -> max a (need r)) 0 rows in
            { Table.floor = 1; min = w; max = w; grow = 0 }
          in
          let ws = Table.widths [| column 0; column 1 |] ~room:40 in
          let frame =
            { Table.edge = Some `Light; line = Some `Light; pad = 1 }
          in
          let shape =
            List.map
              (function Some _ -> Table.Row 1 | None -> Rule `Light)
              rows
          in
          let g =
            Matrix_grid.create ~width:(Table.width frame ws)
              ~height:(Table.height frame shape) ()
          in
          Table.draw g ~x:0 ~y:0 frame ws shape;
          List.iter2
            (fun (p : Table.placed) -> function
              | None -> ()
              | Some r ->
                  Array.iteri
                    (fun i (c : Matrix_grid.region) ->
                      Matrix_grid.draw_text g ~x:c.x ~y:c.y ~text:r.(i))
                    p.cells)
            (Table.place ~x:0 ~y:0 frame ws shape)
            rows;
          Matrix_grid.to_text g
        in
        equal ~msg:"table" string
          "┌──────┬──────┐\n\
           │ name │ size │\n\
           ├──────┼──────┤\n\
           │ a.ml │ 12   │\n\
           └──────┴──────┘"
          text);
  ]

(* ───── Trees ───── *)

module N = struct
  type t = N of string * t list
end

let tree =
  N.
    [
      N
        ( "root",
          [
            N ("bin", []);
            N ("etc", [ N ("hosts", []); N ("ssl", [ N ("certs", []) ]) ]);
            N ("usr", []);
          ] );
    ]

(* The example of [Tree]'s documentation. *)
module Tree_example = struct
  type node = N of string * node list

  let print roots =
    let open Matrix_cli in
    Tree.fold
      ~children:(fun (N (_, c)) -> c)
      (fun lasts (N (label, _)) () ->
        print_endline (Tree.prefix Tree.single lasts ^ label))
      roots ()
end

let tree_tests =
  [
    test "Tree's example prints" (fun () -> Tree_example.(print [ N ("", []) ]));
    test "prefix and rails" (fun () ->
        let lines g =
          Tree.fold
            ~children:(fun (N.N (_, c)) -> c)
            (fun lasts (N.N (label, _)) acc ->
              (Tree.rails g lasts ^ "~") :: (Tree.prefix g lasts ^ label) :: acc)
            tree []
          |> List.rev
        in
        equal ~msg:"single" (list string)
          [
            "root";
            "~";
            "├── bin";
            "│   ~";
            "├── etc";
            "│   ~";
            "│   ├── hosts";
            "│   │   ~";
            "│   └── ssl";
            "│       ~";
            "│       └── certs";
            "│           ~";
            "└── usr";
            "    ~";
          ]
          (lines Tree.single);
        equal ~msg:"ascii" string "`-- usr" (List.nth (lines Tree.ascii) 12));
    test "a collapsed node has no children" (fun () ->
        let labels =
          Tree.fold
            ~children:(fun (N.N (l, c)) -> if l = "etc" then [] else c)
            (fun lasts (N.N (l, _)) acc ->
              (Tree.prefix Tree.single lasts ^ l) :: acc)
            tree []
          |> List.rev
        in
        equal ~msg:"collapsed" (list string)
          [ "root"; "├── bin"; "├── etc"; "└── usr" ]
          labels);
  ]

(* ───── ocaml-console's capabilities ─────

   ocaml-console prints tables and trees whose capabilities its interfaces
   promise: columns with an alignment, bounds, wrapping or truncation and a
   column that narrows last; rules between rows; ragged rows whose last cell
   spans the rest; a table that narrows to fit, and whose lines are cut with an
   ellipsis when even the narrowest columns do not fit, no column left out;
   borders of any glyphs, ASCII among them; frames that move while the
   content stands still; trees whose labels wrap under their guides; and any
   widget inside another. Each is composed here over [Table] and [Tree]. *)

type column = {
  head : string;
  align : [ `Left | `Right ];
  wrap : bool;  (** Wrap at words, or truncate with an ellipsis. *)
  min_width : int;
  max_width : int option;
  narrows_last : bool;
}

let column ?(align = `Left) ?(wrap = true) ?(min_width = 1) ?max_width
    ?(narrows_last = false) head =
  { head; align; wrap; min_width; max_width; narrows_last }

let widest_word s =
  String.split_on_char ' ' s |> List.fold_left (fun a w -> max a (measure w)) 0

(* What a column needs, from its header and cells. A truncated column breaks
   no line, so its min-content width is its max-content width, as CSS's
   [white-space: nowrap] makes it, and it narrows only below the others'
   min-content widths. A column that narrows last does the same, then down to
   its widest word. *)
let need c cells =
  let fold f = List.fold_left (fun a s -> max a (f s)) 0 (c.head :: cells) in
  let bound w =
    let w = max w c.min_width in
    match c.max_width with Some m -> min w m | None -> w
  in
  let max_ = bound (fold measure) in
  let word = if c.wrap then min max_ (bound (fold widest_word)) else max_ in
  let floor = min word c.min_width in
  if c.narrows_last then
    { Table.floor = word; min = max_; max = max_; grow = 0 }
  else { Table.floor; min = word; max = max_; grow = 0 }

(* The rows of [s] laid out in [width] cells, each with its width: wrapped
   at words, or cut with an ellipsis. *)
let lines_of c width s =
  if c.wrap then
    let src = Layout.Source.of_string s in
    let l = Layout.create src ~width ~wrap:`Word in
    List.init (Layout.row_count l) (fun r ->
        let text =
          String.trim
            (Layout.Source.slice src ~first:(Layout.row_start l r)
               ~last:(Layout.row_end l r))
        in
        (text, measure text))
  else if measure s <= width then [ (s, measure s) ]
  else
    let p =
      Matrix_text.find_wrap_pos ~width_method:`Unicode ~tab_width:2 s
        ~max_columns:(max 0 (width - 1))
    in
    [ (String.sub s 0 p.byte_offset ^ "…", p.columns_used + 1) ]

(* A table of [cols] printed in [room] columns: a header, a rule under it,
   [rules] between rows, and ragged rows whose last cell spans the columns
   left. When even the floors do not fit, each line is cut with an
   ellipsis. *)
let print ?(frame = light) ?glyphs ?(rules = false) ~room cols rows =
  let n = Array.length cols in
  let full = List.filter (fun r -> Array.length r = n) rows in
  let needs =
    Array.mapi (fun i c -> need c (List.map (fun r -> r.(i)) full)) cols
  in
  let ws = Table.widths needs ~room:(room - Table.chrome frame n) in
  let all = Array.map (fun c -> c.head) cols :: rows in
  let shape r =
    let k = Array.length r in
    if k = n then `Row
    else `Spanned (List.init (k - 1) (fun _ -> 1) @ [ n - k + 1 ])
  in
  let place_row r =
    match shape r with `Row -> Table.Row 1 | `Spanned s -> Spanned (1, s)
  in
  (* Heights: lay each cell out in its region's width. *)
  let probe = Table.place ~x:0 ~y:0 frame ws (List.map place_row all) in
  let height r (p : Table.placed) =
    Array.to_list p.cells
    |> List.mapi (fun i (reg : Grid.region) ->
        List.length
          (lines_of
             cols.(if i = Array.length r - 1 then n - 1 else i)
             reg.width r.(i)))
    |> List.fold_left max 1
  in
  let table_rows =
    List.concat
      (List.mapi
         (fun k (r, p) ->
           let row =
             match shape r with
             | `Row -> Table.Row (height r p)
             | `Spanned s -> Spanned (height r p, s)
           in
           if k = 0 then [ row; Rule `Light ]
           else if rules && k > 1 then [ Rule `Light; row ]
           else [ row ])
         (List.combine all probe))
  in
  let w = Table.width frame ws and h = Table.height frame table_rows in
  let g = Grid.create ~width:w ~height:h () in
  Table.draw ?glyphs g ~x:0 ~y:0 frame ws table_rows;
  let placed =
    List.filter
      (fun (p : Table.placed) -> Array.length p.cells > 0)
      (Table.place ~x:0 ~y:0 frame ws table_rows)
  in
  List.iter2
    (fun r (p : Table.placed) ->
      Array.iteri
        (fun i (reg : Grid.region) ->
          let c = cols.(if i = Array.length r - 1 then n - 1 else i) in
          List.iteri
            (fun dy (text, tw) ->
              let dx =
                match c.align with `Left -> 0 | `Right -> reg.width - tw
              in
              Grid.draw_text g ~x:(reg.x + dx) ~y:(reg.y + dy) ~text)
            (lines_of c reg.width r.(i)))
        p.cells)
    all placed;
  let text = Grid.to_text g in
  if w <= room then text
  else
    String.split_on_char '\n' text
    |> List.map (fun line ->
        let p =
          Matrix_text.find_wrap_pos ~width_method:`Unicode ~tab_width:2 line
            ~max_columns:(room - 1)
        in
        String.sub line 0 p.byte_offset ^ "…")
    |> String.concat "\n"

let packages =
  [|
    column "pkg" ~wrap:false ~max_width:8;
    column "version" ~align:`Right ~narrows_last:true;
    column "synopsis" ~min_width:4;
  |]

let package_rows =
  [
    [|
      "cmdliner"; "2.1.1"; "Declarative definition of command line interfaces";
    |];
    [| "uutf-and-a-long-name"; "1.0.4"; "Non-blocking streaming codec" |];
    [| "fmt"; "this row's last cell spans the rest" |];
  ]

let console_tests =
  [
    test "columns wrap, truncate, align right and stay in their bounds"
      (fun () ->
        equal ~msg:"44 columns" string
          "┌──────────┬─────────┬─────────────────────┐\n\
           │ pkg      │ version │ synopsis            │\n\
           ├──────────┼─────────┼─────────────────────┤\n\
           │ cmdliner │   2.1.1 │ Declarative         │\n\
           │          │         │ definition of       │\n\
           │          │         │ command line        │\n\
           │          │         │ interfaces          │\n\
           ├──────────┼─────────┼─────────────────────┤\n\
           │ uutf-an… │   1.0.4 │ Non-blocking        │\n\
           │          │         │ streaming codec     │\n\
           ├──────────┼─────────┴─────────────────────┤\n\
           │ fmt      │ this row's last cell spans    │\n\
           │          │ the rest                      │\n\
           └──────────┴───────────────────────────────┘"
          (print ~rules:true ~room:44 packages package_rows));
    test "a table narrows to fit, the column that narrows last last" (fun () ->
        let text = print ~room:30 packages package_rows in
        List.iter
          (fun line -> is_true ~msg:line (measure line <= 30))
          (String.split_on_char '\n' text);
        let contains sub s =
          let n = String.length sub in
          let rec go i =
            i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
          in
          go 0
        in
        is_true ~msg:"version keeps its width"
          (contains "│ version │" (List.nth (String.split_on_char '\n' text) 1)));
    test "too narrow for the floors, lines are cut and no column is dropped"
      (fun () ->
        let text = print ~room:12 packages [ List.hd package_rows ] in
        let lines = String.split_on_char '\n' text in
        List.iter
          (fun line ->
            equal ~msg:line int 12 (measure line);
            is_true ~msg:"ends with an ellipsis"
              (String.ends_with ~suffix:"…" line))
          lines;
        equal ~msg:"the header keeps every column" string "│ … │ versi…"
          (List.nth lines 1));
    test "borders: edgeless, ASCII and custom glyphs" (fun () ->
        let rows = [ [| "a"; "1" |]; [| "bb"; "22" |] ] in
        let cols = [| column "x"; column "y" ~align:`Right |] in
        equal ~msg:"edgeless with a rule" string
          "x  │  y\n───┼───\na  │  1\nbb │ 22"
          (print
             ~frame:{ Table.edge = None; line = Some `Light; pad = 1 }
             ~room:20 cols rows);
        equal ~msg:"ascii" string
          "+----+----+\n\
           | x  |  y |\n\
           +----+----+\n\
           | a  |  1 |\n\
           | bb | 22 |\n\
           +----+----+"
          (print ~glyphs:Grid.Lines.ascii ~room:20 cols rows);
        let stars =
          Grid.Lines.Set
            {
              horizontal = "═";
              vertical = "‖";
              top_left = "*";
              top_right = "*";
              bottom_left = "*";
              bottom_right = "*";
              top_t = "*";
              bottom_t = "*";
              left_t = "*";
              right_t = "*";
              cross = "*";
            }
        in
        equal ~msg:"custom" string
          "*════*════*\n\
           ‖ x  ‖  y ‖\n\
           *════*════*\n\
           ‖ a  ‖  1 ‖\n\
           ‖ bb ‖ 22 ‖\n\
           *════*════*"
          (print ~glyphs:stars ~room:20 cols rows));
    test "a frame moves while its content stands still" (fun () ->
        (* Border motion: each frame recolors the cells outside every cell
           region by the time, so widths and text never change. *)
        let f = light and ws = [| 3 |] and rows = [ Table.Row 1 ] in
        let draw t =
          let g =
            Grid.create ~width:(Table.width f ws) ~height:(Table.height f rows)
              ()
          in
          Table.draw g ~x:0 ~y:0 f ws rows;
          let cells = (List.hd (Table.place ~x:0 ~y:0 f ws rows)).cells in
          Grid.draw_text g ~x:cells.(0).x ~y:cells.(0).y ~text:"abc";
          let inside x y =
            Array.exists
              (fun (r : Grid.region) ->
                x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height)
              cells
          in
          for y = 0 to Grid.height g - 1 do
            for x = 0 to Grid.width g - 1 do
              let i = Grid.idx g ~x ~y in
              if (not (inside x y)) && Grid.get_codepoint g i > 32 then
                Grid.set_uchar g ~x ~y
                  ~uchar:(Uchar.of_int (Grid.get_codepoint g i))
                  ~fg:(Ansi.Color.of_rgb ((x + t) * 40 mod 256) 100 100)
                  ~bg:Ansi.Color.transparent ~attrs:Ansi.Attr.empty ()
            done
          done;
          g
        in
        let a = draw 0 and b = draw 1 in
        equal ~msg:"text" string (Grid.to_text a) (Grid.to_text b);
        is_true ~msg:"frame color moved"
          (not (Ansi.Color.equal (Grid.get_fg a 0) (Grid.get_fg b 0)));
        is_true ~msg:"content color stayed"
          (Ansi.Color.equal
             (Grid.get_fg a (Grid.idx a ~x:2 ~y:1))
             (Grid.get_fg b (Grid.idx b ~x:2 ~y:1))));
    test "a table inside a cell of another" (fun () ->
        (* The inner table's width is its column's min- and max-content width,
           and it is drawn into the cell's region. *)
        let inner_ws = [| 1; 1 |] and inner_rows = Table.[ Row 1 ] in
        let iw = Table.width light inner_ws
        and ih = Table.height light inner_rows in
        let inner = Grid.create ~width:iw ~height:ih () in
        Table.draw inner ~x:0 ~y:0 light inner_ws inner_rows;
        let outer_ws = Table.widths [| col 4 4; col iw iw |] ~room:40 in
        let outer_rows = [ Table.Row ih ] in
        let g =
          Grid.create
            ~width:(Table.width light outer_ws)
            ~height:(Table.height light outer_rows)
            ()
        in
        Table.draw g ~x:0 ~y:0 light outer_ws outer_rows;
        let cell =
          (List.hd (Table.place ~x:0 ~y:0 light outer_ws outer_rows)).cells.(1)
        in
        Grid.blit_region ~src:inner ~dst:g ~src_x:0 ~src_y:0 ~width:iw
          ~height:ih ~dst_x:cell.x ~dst_y:cell.y;
        equal ~msg:"nested" string
          "┌──────┬───────────┐\n\
           │      │ ┌───┬───┐ │\n\
           │      │ │   │   │ │\n\
           │      │ └───┴───┘ │\n\
           └──────┴───────────┘"
          (Grid.to_text g));
    test "a tree whose labels wrap under their guides, indented" (fun () ->
        let room = 24 and indent = "  " in
        let tree =
          N.
            [
              N
                ( "project",
                  [
                    N
                      ( "lib: the library, whose modules are many",
                        [ N ("a.ml", []) ] );
                    N ("test", []);
                  ] );
            ]
        in
        let lines =
          Tree.fold
            ~children:(fun (N.N (_, c)) -> c)
            (fun lasts (N.N (label, _)) acc ->
              let first = indent ^ Tree.prefix Tree.single lasts
              and rest = indent ^ Tree.rails Tree.single lasts in
              let src = Layout.Source.of_string label in
              let l =
                Layout.create src ~width:(room - measure first) ~wrap:`Word
              in
              List.rev_append
                (List.init (Layout.row_count l) (fun r ->
                     (if r = 0 then first else rest)
                     ^ String.trim
                         (Layout.Source.slice src ~first:(Layout.row_start l r)
                            ~last:(Layout.row_end l r))))
                acc)
            tree []
          |> List.rev
        in
        equal ~msg:"tree" (list string)
          [
            "  project";
            "  ├── lib: the library,";
            "  │   whose modules are";
            "  │   many";
            "  │   └── a.ml";
            "  └── test";
          ]
          lines;
        List.iter (fun l -> is_true ~msg:l (measure l <= room)) lines);
  ]

let () =
  run "matrix.cli"
    [
      group "widths" width_tests;
      group "placing" place_tests;
      group "trees" tree_tests;
      group "ocaml-console" console_tests;
    ]
