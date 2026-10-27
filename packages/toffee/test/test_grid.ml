(* Additional grid-focused unit tests ported from taffy's non-generated suite *)

module Tree = Toffee_tree
open Toffee
open Geometry
open Tree
open Windtrap

let line = Style.Grid.Placement.line
let span = Style.Grid.Placement.span
let auto = Style.Grid.Placement.auto

let make_grid_child ~col_start ~col_end ~row_start ~row_end =
  Style.make
    ~grid_column:{ start = col_start; end_ = col_end }
    ~grid_row:{ start = row_start; end_ = row_end }
    ()

let empty_line_names track_count = List.init (track_count + 1) (fun _ -> [])

let repeat count tracks =
  Style.Grid.Template_component.repeat
    (Style.Grid_repetition.make ~count ~tracks
       ~line_names:(empty_line_names (List.length tracks)))

let resolve_calc _ basis = basis

let test_compute_grid_size_estimate_explicit_children () =
  let child_styles =
    [
      make_grid_child ~col_start:(line 1) ~col_end:(span 2) ~row_start:(line 2)
        ~row_end:auto;
      make_grid_child ~col_start:(line (-4)) ~col_end:auto
        ~row_start:(line (-2)) ~row_end:auto;
    ]
  in
  let cols, rows =
    Toffee_compute_grid.Implicit_grid.compute_grid_size_estimate
      ~explicit_col_count:6 ~explicit_row_count:8
      ~child_styles_iter:(List.to_seq child_styles)
  in
  equal ~msg:"Inline negative implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.negative_implicit cols);
  equal ~msg:"Inline explicit" int 6
    (Toffee_compute_grid.Grid_track_counts.explicit cols);
  equal ~msg:"Inline positive implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.positive_implicit cols);
  equal ~msg:"Block negative implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.negative_implicit rows);
  equal ~msg:"Block explicit" int 8
    (Toffee_compute_grid.Grid_track_counts.explicit rows);
  equal ~msg:"Block positive implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.positive_implicit rows)

let test_compute_grid_size_estimate_negative_implicit () =
  let child_styles =
    [
      make_grid_child ~col_start:(line (-6)) ~col_end:(span 2)
        ~row_start:(line (-8)) ~row_end:auto;
      make_grid_child ~col_start:(line 4) ~col_end:auto ~row_start:(line 3)
        ~row_end:auto;
    ]
  in
  let cols, rows =
    Toffee_compute_grid.Implicit_grid.compute_grid_size_estimate
      ~explicit_col_count:4 ~explicit_row_count:4
      ~child_styles_iter:(List.to_seq child_styles)
  in
  equal ~msg:"Inline negative implicit" int 1
    (Toffee_compute_grid.Grid_track_counts.negative_implicit cols);
  equal ~msg:"Inline explicit" int 4
    (Toffee_compute_grid.Grid_track_counts.explicit cols);
  equal ~msg:"Inline positive implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.positive_implicit cols);
  equal ~msg:"Block negative implicit" int 3
    (Toffee_compute_grid.Grid_track_counts.negative_implicit rows);
  equal ~msg:"Block explicit" int 4
    (Toffee_compute_grid.Grid_track_counts.explicit rows);
  equal ~msg:"Block positive implicit" int 0
    (Toffee_compute_grid.Grid_track_counts.positive_implicit rows)

let test_explicit_grid_sizing_auto_fill_exact_fit () =
  let grid_style =
    Style.make ~display:Style.Display.Grid
      ~size:
        {
          width = Style.Dimension.length 120.0;
          height = Style.Dimension.length 80.0;
        }
      ~grid_template_columns:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 40.0 ];
        ]
      ~grid_template_rows:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 20.0 ];
        ]
      ()
  in
  let preferred_size =
    Style.size grid_style |> Size.map Style.Dimension.to_option
  in
  let auto_cols, col_count =
    Toffee_compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.width
      ~auto_fit_strategy:
        Toffee_compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Horizontal
  in
  let auto_rows, row_count =
    Toffee_compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.height
      ~auto_fit_strategy:
        Toffee_compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Vertical
  in
  equal ~msg:"Column count" int 3 col_count;
  equal ~msg:"Row count" int 4 row_count;
  equal ~msg:"Auto column reps" int 3 auto_cols;
  equal ~msg:"Auto row reps" int 4 auto_rows

let test_explicit_grid_sizing_auto_fill_with_gaps () =
  let grid_style =
    Style.make ~display:Style.Display.Grid
      ~size:
        {
          width = Style.Dimension.length 140.0;
          height = Style.Dimension.length 100.0;
        }
      ~grid_template_columns:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 40.0 ];
        ]
      ~grid_template_rows:
        [
          repeat Style.Repetition_count.auto_fill
            [ Style.Grid.Track_sizing_function.length 20.0 ];
        ]
      ~gap:
        {
          width = Style.Length_percentage.length 20.0;
          height = Style.Length_percentage.length 20.0;
        }
      ()
  in
  let preferred_size =
    Style.size grid_style |> Size.map Style.Dimension.to_option
  in
  let auto_cols, col_count =
    Toffee_compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.width
      ~auto_fit_strategy:
        Toffee_compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Horizontal
  in
  let auto_rows, row_count =
    Toffee_compute_grid.Explicit_grid.compute_explicit_grid_size_in_axis
      ~style:grid_style ~auto_fit_container_size:preferred_size.height
      ~auto_fit_strategy:
        Toffee_compute_grid.Explicit_grid.Max_repetitions_that_do_not_overflow
      ~resolve_calc_value:resolve_calc ~axis:Absolute_axis.Vertical
  in
  equal ~msg:"Column count with gap" int 2 col_count;
  equal ~msg:"Row count with gap" int 3 row_count;
  equal ~msg:"Auto column reps with gap" int 2 auto_cols;
  equal ~msg:"Auto row reps with gap" int 3 auto_rows

(* Placement algorithm tests *)

let make_counts ~neg ~explicit ~pos =
  Toffee_compute_grid.Grid_track_counts.make ~negative_implicit:neg ~explicit
    ~positive_implicit:pos

let make_grid_style
    ((col_start, col_end, row_start, row_end) :
      Style.Grid.Placement.t
      * Style.Grid.Placement.t
      * Style.Grid.Placement.t
      * Style.Grid.Placement.t) =
  Style.make ~display:Style.Display.Grid
    ~grid_column:Geometry.Line.{ start = col_start; end_ = col_end }
    ~grid_row:Geometry.Line.{ start = row_start; end_ = row_end }
    ()

let expected_to_tuple (line : int Geometry.Line.t) = (line.start, line.end_)

let placement_test_runner ~flow ~explicit_cols ~explicit_rows children
    expected_col_counts expected_row_counts =
  let child_styles_iter =
    List.to_seq children |> Seq.map (fun (_, style, _) -> style)
  in
  let estimated_cols, estimated_rows =
    Toffee_compute_grid.Implicit_grid.compute_grid_size_estimate
      ~explicit_col_count:explicit_cols ~explicit_row_count:explicit_rows
      ~child_styles_iter
  in
  let cell_occupancy =
    Toffee_compute_grid.Cell_occupancy.with_track_counts estimated_cols
      estimated_rows
  in
  let items = ref [] in
  let name_resolver = Toffee_compute_grid.Named.create Style.default 0 0 in
  Toffee_compute_grid.Named.set_explicit_column_count name_resolver
    explicit_cols;
  Toffee_compute_grid.Named.set_explicit_row_count name_resolver explicit_rows;

  let child_ids_styles =
    List.map (fun (id, style, _) -> (Tree.Node_id.make id, style)) children
  in
  let parent_node = Tree.Node_id.make (-1) in
  let children_array = Array.of_list (List.map fst child_ids_styles) in
  let style_table = Hashtbl.create 16 in
  List.iter
    (fun (id, style) -> Hashtbl.add style_table id style)
    child_ids_styles;

  let tree =
    (module struct
      type t = unit

      let child_ids _ _ = Array.to_seq children_array
      let child_count _ _ = Array.length children_array
      let get_child_id _ _ idx = children_array.(idx)
      let get_core_container_style _ node = Hashtbl.find style_table node
      let set_unrounded_layout _ _ _ = ()
      let compute_child_layout _ _ _ = Layout_output.hidden
      let resolve_calc_value _ _ v = v
    end : Tree.LAYOUT_PARTIAL_TREE
      with type t = unit)
  in

  Toffee_compute_grid.Placement.place_grid_items tree
    ~cell_occupancy_matrix:cell_occupancy ~items ~tree:() ~parent_node
    ~grid_auto_flow:flow ~align_items:(Some Style.Align_items.Start)
    ~justify_items:(Some Style.Align_items.Start)
    ~named_line_resolver:name_resolver;

  (* Compare placements *)
  let sorted_items =
    List.sort
      (fun a b ->
        Int.compare
          (Tree.Node_id.index a.Toffee_compute_grid.Grid_item.node)
          (Tree.Node_id.index b.Toffee_compute_grid.Grid_item.node))
      !items
  in
  let sorted_children =
    List.sort (fun (id_a, _, _) (id_b, _, _) -> Int.compare id_a id_b) children
  in
  List.iter2
    (fun (_, _, (exp_col_start, exp_col_end, exp_row_start, exp_row_end)) item
       ->
      equal ~msg:"Column placement" (pair int int)
        (exp_col_start, exp_col_end)
        (expected_to_tuple item.Toffee_compute_grid.Grid_item.column);
      equal ~msg:"Row placement" (pair int int)
        (exp_row_start, exp_row_end)
        (expected_to_tuple item.Toffee_compute_grid.Grid_item.row))
    sorted_children sorted_items;

  let actual_rows =
    Toffee_compute_grid.Cell_occupancy.track_counts cell_occupancy
      Geometry.Absolute_axis.Vertical
  in
  let actual_cols =
    Toffee_compute_grid.Cell_occupancy.track_counts cell_occupancy
      Geometry.Absolute_axis.Horizontal
  in
  equal ~msg:"Row negative implicit" int
    (Toffee_compute_grid.Grid_track_counts.negative_implicit expected_row_counts)
    (Toffee_compute_grid.Grid_track_counts.negative_implicit actual_rows);
  equal ~msg:"Row explicit" int
    (Toffee_compute_grid.Grid_track_counts.explicit expected_row_counts)
    (Toffee_compute_grid.Grid_track_counts.explicit actual_rows);
  equal ~msg:"Row positive implicit" int
    (Toffee_compute_grid.Grid_track_counts.positive_implicit expected_row_counts)
    (Toffee_compute_grid.Grid_track_counts.positive_implicit actual_rows);
  equal ~msg:"Col negative implicit" int
    (Toffee_compute_grid.Grid_track_counts.negative_implicit expected_col_counts)
    (Toffee_compute_grid.Grid_track_counts.negative_implicit actual_cols);
  equal ~msg:"Col explicit" int
    (Toffee_compute_grid.Grid_track_counts.explicit expected_col_counts)
    (Toffee_compute_grid.Grid_track_counts.explicit actual_cols);
  equal ~msg:"Col positive implicit" int
    (Toffee_compute_grid.Grid_track_counts.positive_implicit expected_col_counts)
    (Toffee_compute_grid.Grid_track_counts.positive_implicit actual_cols)

(* The words placing the items of a grid of one row and [columns] columns
   allocates, and the column of the last item. *)
let placement_words ~columns children =
  let cell_occupancy =
    Toffee_compute_grid.Cell_occupancy.with_track_counts
      (make_counts ~neg:0 ~explicit:columns ~pos:0)
      (make_counts ~neg:0 ~explicit:1 ~pos:0)
  in
  let items = ref [] in
  let name_resolver = Toffee_compute_grid.Named.create Style.default 0 0 in
  Toffee_compute_grid.Named.set_explicit_column_count name_resolver columns;
  Toffee_compute_grid.Named.set_explicit_row_count name_resolver 1;
  let children = Array.of_list children in
  let tree =
    (module struct
      type t = unit

      let child_ids _ _ =
        Array.to_seq (Array.mapi (fun i _ -> Tree.Node_id.make i) children)

      let child_count _ _ = Array.length children
      let get_child_id _ _ idx = Tree.Node_id.make idx
      let get_core_container_style _ node = children.(Tree.Node_id.index node)
      let set_unrounded_layout _ _ _ = ()
      let compute_child_layout _ _ _ = Layout_output.hidden
      let resolve_calc_value _ _ v = v
    end : Tree.LAYOUT_PARTIAL_TREE
      with type t = unit)
  in
  let before = Gc.minor_words () in
  Toffee_compute_grid.Placement.place_grid_items tree
    ~cell_occupancy_matrix:cell_occupancy ~items ~tree:()
    ~parent_node:(Tree.Node_id.make (-1))
    ~grid_auto_flow:Style.Grid_auto_flow.Row
    ~align_items:(Some Style.Align_items.Start)
    ~justify_items:(Some Style.Align_items.Start)
    ~named_line_resolver:name_resolver;
  let words = Gc.minor_words () -. before in
  let last =
    List.find
      (fun item ->
        Tree.Node_id.index item.Toffee_compute_grid.Grid_item.node
        = Array.length children - 1)
      !items
  in
  (words, last.Toffee_compute_grid.Grid_item.column.start)

(* An item placed automatically after an item that spans [occupied] columns
   tries each of those columns before the next: the search allocates the
   same whatever the count of columns it tries. *)
let test_placement_search_allocates_nothing_per_cell () =
  let words occupied =
    let children =
      [
        make_grid_style (line 1, span occupied, line 1, auto);
        make_grid_style (auto, auto, line 1, auto);
      ]
    in
    let words, column = placement_words ~columns:30 children in
    equal ~msg:"column" int occupied column;
    words
  in
  equal ~msg:"words after 20 columns as after 2" float_exact (words 2)
    (words 20)

let placement_tests =
  let flow_row = Style.Grid_auto_flow.Row in
  let flow_col = Style.Grid_auto_flow.Column in
  let flow_dense = Style.Grid_auto_flow.Row_dense in
  [
    ( "Only fixed placement",
      fun () ->
        let children =
          [
            (1, make_grid_style (line 1, auto, line 1, auto), (0, 1, 0, 1));
            ( 2,
              make_grid_style (line (-4), auto, line (-3), auto),
              (-1, 0, 0, 1) );
            ( 3,
              make_grid_style (line (-3), auto, line (-4), auto),
              (0, 1, -1, 0) );
            (4, make_grid_style (line 3, span 2, line 5, auto), (2, 4, 4, 5));
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:1 ~explicit:2 ~pos:3 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Placement spanning origin",
      fun () ->
        let children =
          [
            ( 1,
              make_grid_style (line (-1), line (-1), line (-1), line (-1)),
              (2, 3, 2, 3) );
            ( 2,
              make_grid_style (line (-1), span 2, line (-1), span 2),
              (2, 4, 2, 4) );
            ( 3,
              make_grid_style (line (-4), line (-4), line (-4), line (-4)),
              (-1, 0, -1, 0) );
            ( 4,
              make_grid_style (line (-4), span 2, line (-4), span 2),
              (-1, 1, -1, 1) );
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:1 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Only auto placement row flow",
      fun () ->
        let auto_child = make_grid_style (auto, auto, auto, auto) in
        let mk id placement =
          let col_start, col_end, row_start, row_end = placement in
          (id, auto_child, (col_start, col_end, row_start, row_end))
        in
        let children =
          [
            mk 1 (0, 1, 0, 1);
            mk 2 (1, 2, 0, 1);
            mk 3 (0, 1, 1, 2);
            mk 4 (1, 2, 1, 2);
            mk 5 (0, 1, 2, 3);
            mk 6 (1, 2, 2, 3);
            mk 7 (0, 1, 3, 4);
            mk 8 (1, 2, 3, 4);
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Only auto placement column flow",
      fun () ->
        let auto_child = make_grid_style (auto, auto, auto, auto) in
        let mk id placement =
          let col_start, col_end, row_start, row_end = placement in
          (id, auto_child, (col_start, col_end, row_start, row_end))
        in
        let children =
          [
            mk 1 (0, 1, 0, 1);
            mk 2 (0, 1, 1, 2);
            mk 3 (1, 2, 0, 1);
            mk 4 (1, 2, 1, 2);
            mk 5 (2, 3, 0, 1);
            mk 6 (2, 3, 1, 2);
            mk 7 (3, 4, 0, 1);
            mk 8 (3, 4, 1, 2);
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_col ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Oversized item",
      fun () ->
        let children =
          [ (1, make_grid_style (span 5, auto, auto, auto), (0, 5, 0, 1)) ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:3 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Fixed in secondary axis",
      fun () ->
        let children =
          [
            (1, make_grid_style (span 2, auto, line 1, auto), (0, 2, 0, 1));
            (2, make_grid_style (auto, auto, line 2, auto), (0, 1, 1, 2));
            (3, make_grid_style (auto, auto, line 1, auto), (2, 3, 0, 1));
            (4, make_grid_style (auto, auto, line 4, auto), (0, 1, 3, 4));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:2 ~pos:1 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:2 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Definite in secondary axis with negative",
      fun () ->
        let children =
          [
            (2, make_grid_style (auto, auto, line 2, auto), (0, 1, 1, 2));
            (1, make_grid_style (line (-4), auto, line 2, auto), (-1, 0, 1, 2));
            (3, make_grid_style (auto, auto, line 1, auto), (-1, 0, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:1 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
    ( "Dense packing algorithm",
      fun () ->
        let children =
          [
            (1, make_grid_style (line 2, auto, line 1, auto), (1, 2, 0, 1));
            (2, make_grid_style (span 2, auto, auto, auto), (2, 4, 0, 1));
            (3, make_grid_style (auto, auto, auto, auto), (0, 1, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        placement_test_runner ~flow:flow_dense ~explicit_cols:4 ~explicit_rows:4
          children expected_cols expected_rows );
    ( "Sparse packing algorithm",
      fun () ->
        let children =
          [
            (1, make_grid_style (auto, span 3, auto, auto), (0, 3, 0, 1));
            (2, make_grid_style (auto, span 3, auto, auto), (0, 3, 1, 2));
            (3, make_grid_style (auto, span 1, auto, auto), (3, 4, 1, 2));
          ]
        in
        let expected_cols = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:4 ~pos:0 in
        placement_test_runner ~flow:flow_row ~explicit_cols:4 ~explicit_rows:4
          children expected_cols expected_rows );
    ( "Auto placement in negative tracks",
      fun () ->
        let children =
          [
            (1, make_grid_style (line (-5), auto, line 1, auto), (-2, -1, 0, 1));
            (2, make_grid_style (auto, auto, line 2, auto), (-2, -1, 1, 2));
            (3, make_grid_style (auto, auto, auto, auto), (-1, 0, 0, 1));
          ]
        in
        let expected_cols = make_counts ~neg:2 ~explicit:2 ~pos:0 in
        let expected_rows = make_counts ~neg:0 ~explicit:2 ~pos:0 in
        placement_test_runner ~flow:flow_dense ~explicit_cols:2 ~explicit_rows:2
          children expected_cols expected_rows );
  ]

let () =
  run "Toffee Grid Tests"
    [
      group "implicit_grid"
        [
          test "Size estimate explicit children"
            test_compute_grid_size_estimate_explicit_children;
          test "Size estimate negative implicit"
            test_compute_grid_size_estimate_negative_implicit;
        ];
      group "explicit_grid"
        [
          test "Auto-fill exact fit"
            test_explicit_grid_sizing_auto_fill_exact_fit;
          test "Auto-fill with gaps"
            test_explicit_grid_sizing_auto_fill_with_gaps;
        ];
      group "placement"
        (List.map (fun (name, fn) -> test name fn) placement_tests
        @ [
            test "The search allocates nothing per cell"
              test_placement_search_allocates_nothing_per_cell;
          ]);
    ]
