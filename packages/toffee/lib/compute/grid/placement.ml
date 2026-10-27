(* Implements placing items in the grid and resolving the implicit grid.
   https://www.w3.org/TR/css-grid-1/#placement *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

(* Helper module for grid placement operations *)
module OriginZeroPlacement = struct
  let is_definite = function
    | Grid.Origin_zero_placement.Line _ -> true
    | _ -> false
end

(* Helper module for Line<OriginZeroPlacement> operations *)
module PlacementLine = struct
  let is_definite line =
    OriginZeroPlacement.is_definite line.Line.start
    || OriginZeroPlacement.is_definite line.Line.end_

  let indefinite_span line =
    match (line.Line.start, line.Line.end_) with
    | Grid.Origin_zero_placement.Line _, Grid.Origin_zero_placement.Auto -> 1
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Line _ -> 1
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Auto -> 1
    | Grid.Origin_zero_placement.Line _, Grid.Origin_zero_placement.Span span ->
        span
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Line _ ->
        span
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Auto ->
        span
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Span span ->
        span
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Span _ ->
        span
    | Grid.Origin_zero_placement.Line _, Grid.Origin_zero_placement.Line _ ->
        failwith
          "indefinite_span should only be called on indefinite grid tracks"

  let resolve_definite_grid_lines line =
    match (line.Line.start, line.Line.end_) with
    | ( Grid.Origin_zero_placement.Line start,
        Grid.Origin_zero_placement.Line end_ )
      when start = end_ ->
        Line.{ start; end_ = start + 1 }
    | ( Grid.Origin_zero_placement.Line start,
        Grid.Origin_zero_placement.Line end_ ) ->
        let start_line = min start end_ in
        let end_line = max start end_ in
        Line.{ start = start_line; end_ = end_line }
    | ( Grid.Origin_zero_placement.Line start,
        Grid.Origin_zero_placement.Span span ) ->
        Line.{ start; end_ = start + span }
    | Grid.Origin_zero_placement.Line start, Grid.Origin_zero_placement.Auto ->
        Line.{ start; end_ = start + 1 }
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Line end_
      ->
        Line.{ start = end_ - span; end_ }
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Line end_ ->
        Line.{ start = end_ - 1; end_ }
    | _ -> failwith "resolve_definite_grid_lines called on indefinite placement"

  let resolve_indefinite_grid_tracks line position =
    match (line.Line.start, line.Line.end_) with
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Auto ->
        Line.{ start = position; end_ = position + 1 }
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Auto ->
        Line.{ start = position; end_ = position + span }
    | Grid.Origin_zero_placement.Auto, Grid.Origin_zero_placement.Span span ->
        Line.{ start = position; end_ = position + span }
    | Grid.Origin_zero_placement.Span span, Grid.Origin_zero_placement.Span _ ->
        Line.{ start = position; end_ = position + span }
    | _ ->
        failwith
          "resolve_indefinite_grid_tracks should only be called on indefinite \
           grid tracks"
end

(* Module for InBothAbsAxis<Line<OriginZeroPlacement>> operations *)
module PlacementInBothAxes = struct
  let get_absolute placement axis =
    match axis with
    | Absolute_axis.Horizontal -> placement.In_both_abs_axis.horizontal
    | Absolute_axis.Vertical -> placement.In_both_abs_axis.vertical

  let get placement axis =
    get_absolute placement (Abstract_axis.to_absolute_naive axis)
end

(* 8.5. Grid Item Placement Algorithm Place a single definitely placed item into
   the grid *)
let place_definite_grid_item placement primary_axis =
  (* Resolve spans to tracks *)
  let primary_span =
    PlacementInBothAxes.get placement primary_axis
    |> PlacementLine.resolve_definite_grid_lines
  in
  let secondary_span =
    PlacementInBothAxes.get placement (Abstract_axis.other primary_axis)
    |> PlacementLine.resolve_definite_grid_lines
  in
  (primary_span, secondary_span)

(* 8.5. Grid Item Placement Algorithm Step 2. Place remaining children with
   definite secondary axis positions *)
let place_definite_secondary_axis_item cell_occupancy_matrix placement auto_flow
    =
  let primary_axis = Grid.Auto_flow.primary_axis auto_flow in
  let secondary_axis = Abstract_axis.other primary_axis in

  let secondary_axis_placement =
    PlacementInBothAxes.get placement secondary_axis
    |> PlacementLine.resolve_definite_grid_lines
  in
  let primary_axis_grid_start_line =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive primary_axis)
    |> Grid_track_counts.implicit_start_line
  in
  let starting_position =
    if Grid.Auto_flow.is_dense auto_flow then primary_axis_grid_start_line
    else
      match
        Cell_occupancy.last_of_type cell_occupancy_matrix
          (Abstract_axis.to_absolute_naive primary_axis)
          secondary_axis_placement.Line.start Cell_occupancy.AutoPlaced
      with
      | Some pos -> pos
      | None -> primary_axis_grid_start_line
  in

  (* The item spans the same count of tracks wherever it starts *)
  let primary_line = PlacementInBothAxes.get placement primary_axis in
  let primary_span =
    let first =
      PlacementLine.resolve_indefinite_grid_tracks primary_line
        starting_position
    in
    first.end_ - first.start
  in
  let position = ref starting_position in
  while
    not
      (Cell_occupancy.lines_are_unoccupied cell_occupancy_matrix
         (Abstract_axis.to_absolute_naive primary_axis)
         ~primary_start:!position ~primary_end:(!position + primary_span)
         ~secondary_start:secondary_axis_placement.Line.start
         ~secondary_end:secondary_axis_placement.end_)
  do
    incr position
  done;
  ( PlacementLine.resolve_indefinite_grid_tracks primary_line !position,
    secondary_axis_placement )

(* 8.5. Grid Item Placement Algorithm Step 4. Position the remaining grid
   items *)
let place_indefinitely_positioned_item cell_occupancy_matrix placement auto_flow
    grid_position =
  let primary_axis = Grid.Auto_flow.primary_axis auto_flow in
  let secondary_axis = Abstract_axis.other primary_axis in

  let primary_placement_style =
    PlacementInBothAxes.get placement primary_axis
  in
  let secondary_placement_style =
    PlacementInBothAxes.get placement secondary_axis
  in

  let secondary_span =
    PlacementLine.indefinite_span secondary_placement_style
  in
  let has_definite_primary_axis_position =
    PlacementLine.is_definite primary_placement_style
  in
  let primary_axis_grid_start_line =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive primary_axis)
    |> Grid_track_counts.implicit_start_line
  in
  let primary_axis_grid_end_line =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive primary_axis)
    |> Grid_track_counts.implicit_end_line
  in
  let secondary_axis_grid_start_line =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive secondary_axis)
    |> Grid_track_counts.implicit_start_line
  in

  let primary_absolute_axis = Abstract_axis.to_absolute_naive primary_axis in

  let primary_idx, secondary_idx = grid_position in
  let primary_idx = ref primary_idx in
  let secondary_idx = ref secondary_idx in

  if has_definite_primary_axis_position then (
    let primary_span =
      PlacementLine.resolve_definite_grid_lines primary_placement_style
    in

    (* Compute secondary axis starting position for search *)
    secondary_idx :=
      if Grid.Auto_flow.is_dense auto_flow then
        (* If auto-flow is dense then we always search from the first track *)
        secondary_axis_grid_start_line
      else if primary_span.Line.start < !primary_idx then !secondary_idx + 1
      else !secondary_idx;

    (* Item has fixed primary axis position: so we simply increment the
       secondary axis position until we find a space that the item fits in *)
    while
      not
        (Cell_occupancy.lines_are_unoccupied cell_occupancy_matrix
           primary_absolute_axis ~primary_start:primary_span.Line.start
           ~primary_end:primary_span.end_ ~secondary_start:!secondary_idx
           ~secondary_end:(!secondary_idx + secondary_span))
    do
      (* If area is occupied, increment the index and try again *)
      incr secondary_idx
    done;
    (* Once we find a free space, return that position *)
    ( primary_span,
      Line.{ start = !secondary_idx; end_ = !secondary_idx + secondary_span } ))
  else
    let primary_span = PlacementLine.indefinite_span primary_placement_style in

    (* Item does not have any fixed axis, so we search along the primary axis
       until we hit the end of the already existent tracks, and then we reset
       the primary axis back to zero and increment the secondary axis index. We
       continue in this vein until we find a space that the item fits in. *)
    let found = ref false in
    while not !found do
      (* If the primary index is out of bounds, then increment the secondary
         index and reset the primary index back to the start of the grid *)
      if !primary_idx + primary_span > primary_axis_grid_end_line then (
        incr secondary_idx;
        primary_idx := primary_axis_grid_start_line)
      else if
        Cell_occupancy.lines_are_unoccupied cell_occupancy_matrix
          primary_absolute_axis ~primary_start:!primary_idx
          ~primary_end:(!primary_idx + primary_span)
          ~secondary_start:!secondary_idx
          ~secondary_end:(!secondary_idx + secondary_span)
      then found := true
      else
        (* If area is occupied, increment the primary index and try again *)
        incr primary_idx
    done;
    (* Once we find a free space that's in bounds, return that position *)
    ( Line.{ start = !primary_idx; end_ = !primary_idx + primary_span },
      Line.{ start = !secondary_idx; end_ = !secondary_idx + secondary_span } )

(* Record the grid item in both CellOccupancyMatrix and the GridItems list once
   a definite placement has been determined *)
let record_grid_placement cell_occupancy_matrix items node index style
    parent_align_items parent_justify_items primary_axis primary_span
    secondary_span placement_type =
  (* Mark area of grid as occupied *)
  Cell_occupancy.mark_area_as cell_occupancy_matrix
    (Abstract_axis.to_absolute_naive primary_axis)
    primary_span secondary_span placement_type;

  (* Create grid item *)
  let col_span, row_span =
    match Abstract_axis.to_absolute_naive primary_axis with
    | Absolute_axis.Horizontal -> (primary_span, secondary_span)
    | Absolute_axis.Vertical -> (secondary_span, primary_span)
  in
  let item =
    Grid_item.new_with_placement_style_and_order ~node ~col_span ~row_span
      ~style ~parent_align_items ~parent_justify_items ~source_order:index
  in
  items := item :: !items

(* 8.5. Grid Item Placement Algorithm Place items into the grid, generating new
   rows/column into the implicit grid as required

   [Specification](https://www.w3.org/TR/css-grid-2/#auto-placement-algo) *)
let place_grid_items (type t)
    (module Tree : LAYOUT_PARTIAL_TREE with type t = t) ~cell_occupancy_matrix
    ~items ~tree ~parent_node ~grid_auto_flow ~align_items ~justify_items
    ~named_line_resolver =
  let primary_axis = Grid.Auto_flow.primary_axis grid_auto_flow in
  let secondary_axis = Abstract_axis.other primary_axis in

  let map_child_style_to_origin_zero_placement =
    let explicit_col_count =
      Cell_occupancy.track_counts cell_occupancy_matrix Absolute_axis.Horizontal
      |> Grid_track_counts.explicit
    in
    let explicit_row_count =
      Cell_occupancy.track_counts cell_occupancy_matrix Absolute_axis.Vertical
      |> Grid_track_counts.explicit
    in
    fun (index, node, style) ->
      let horizontal_placement =
        (* Get grid_column from style and resolve named lines *)
        let grid_placement = Style.grid_column style in
        let resolved_placement =
          Named.resolve_column_names named_line_resolver grid_placement
        in
        Line.map
          (fun placement ->
            Grid.Placement.into_origin_zero_placement placement
              explicit_col_count)
          resolved_placement
      in
      let vertical_placement =
        (* Get grid_row from style and resolve named lines *)
        let grid_placement = Style.grid_row style in
        let resolved_placement =
          Named.resolve_row_names named_line_resolver grid_placement
        in
        Line.map
          (fun placement ->
            Grid.Placement.into_origin_zero_placement placement
              explicit_row_count)
          resolved_placement
      in
      let origin_zero_placement =
        In_both_abs_axis.
          { horizontal = horizontal_placement; vertical = vertical_placement }
      in
      (index, node, origin_zero_placement, style)
  in

  (* Helper to iterate children, filtering out display:none and
     position:absolute items *)
  let children_array =
    let count = Tree.child_count tree parent_node in
    let acc = ref [] in
    for index = count - 1 downto 0 do
      let child_id = Tree.get_child_id tree parent_node index in
      let child_style = Tree.get_core_container_style tree child_id in
      if
        Style.box_generation_mode child_style <> Box_generation_mode.None
        && Style.position child_style <> Position.Absolute
      then acc := (index, child_id, child_style) :: !acc
    done;
    Array.of_list !acc
  in

  (* 1. Place children with definite positions *)
  Array.iter
    (fun (index, child_node, child_style) ->
      let _, _, child_placement, style =
        map_child_style_to_origin_zero_placement (index, child_node, child_style)
      in
      if
        PlacementLine.is_definite child_placement.In_both_abs_axis.horizontal
        && PlacementLine.is_definite child_placement.In_both_abs_axis.vertical
      then
        let primary_span, secondary_span =
          place_definite_grid_item child_placement primary_axis
        in
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis primary_span secondary_span
          Cell_occupancy.DefinitelyPlaced)
    children_array;

  (* 2. Place remaining children with definite secondary axis positions *)
  Array.iter
    (fun (index, child_node, child_style) ->
      let _, _, child_placement, style =
        map_child_style_to_origin_zero_placement (index, child_node, child_style)
      in
      let secondary_definite =
        PlacementLine.is_definite
          (PlacementInBothAxes.get child_placement secondary_axis)
      in
      let primary_indefinite =
        not
          (PlacementLine.is_definite
             (PlacementInBothAxes.get child_placement primary_axis))
      in
      if secondary_definite && primary_indefinite then
        let primary_span, secondary_span =
          place_definite_secondary_axis_item cell_occupancy_matrix
            child_placement grid_auto_flow
        in
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis primary_span secondary_span
          Cell_occupancy.AutoPlaced)
    children_array;

  (* 3. Determine the number of columns in the implicit grid By the time we get
     to this point in the execution, this is actually already accounted for:

     3.1 Start with the columns from the explicit grid => Handled by grid size
     estimate which is used to pre-size the GridOccupancyMatrix

     3.2 Among all the items with a definite column position (explicitly
     positioned items, items positioned in the previous step, and items not yet
     positioned but with a definite column) add columns to the beginning and end
     of the implicit grid as necessary to accommodate those items. => Handled by
     expand_to_fit_range which expands the GridOccupancyMatrix as necessary ->
     Called by mark_area_as -> Called by record_grid_placement

     3.3 If the largest column span among all the items without a definite
     column position is larger than the width of the implicit grid, add columns
     to the end of the implicit grid to accommodate that column span. => Handled
     by grid size estimate which is used to pre-size the GridOccupancyMatrix *)

  (* 4. Position the remaining grid items (which either have definite position
     only in the secondary axis or indefinite positions in both axis) *)
  let primary_neg_tracks =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive primary_axis)
    |> Grid_track_counts.negative_implicit
  in
  let secondary_neg_tracks =
    Cell_occupancy.track_counts cell_occupancy_matrix
      (Abstract_axis.to_absolute_naive secondary_axis)
    |> Grid_track_counts.negative_implicit
  in
  let grid_start_position = (-primary_neg_tracks, -secondary_neg_tracks) in
  let grid_position = ref grid_start_position in

  Array.iter
    (fun (index, child_node, child_style) ->
      let _, _, child_placement, style =
        map_child_style_to_origin_zero_placement (index, child_node, child_style)
      in
      if
        not
          (PlacementLine.is_definite
             (PlacementInBothAxes.get child_placement secondary_axis))
      then (
        (* Compute placement *)
        let primary_span, secondary_span =
          place_indefinitely_positioned_item cell_occupancy_matrix
            child_placement grid_auto_flow !grid_position
        in

        (* Record item *)
        record_grid_placement cell_occupancy_matrix items child_node index style
          align_items justify_items primary_axis primary_span secondary_span
          Cell_occupancy.AutoPlaced;

        (* If using the "dense" placement algorithm then reset the grid position
           back to grid_start_position ready for the next item Otherwise set it
           to the position of the current item so that the next item it placed
           after it. *)
        grid_position :=
          if Grid.Auto_flow.is_dense grid_auto_flow then grid_start_position
          else (primary_span.Line.end_, secondary_span.Line.start)))
    children_array;

  (* Reverse the items list to maintain original order (we accumulated in
     reverse for performance) *)
  items := List.rev !items
