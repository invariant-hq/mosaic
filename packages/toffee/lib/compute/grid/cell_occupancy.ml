(* Contains CellOccupancyMatrix used to track occupied cells during grid
   placement *)

open Toffee_geometry

(* The occupancy state of a single grid cell *)
type cell_occupancy_state =
  | Unoccupied (* Indicates that a grid cell is unoccupied *)
  | DefinitelyPlaced
    (* Indicates that a grid cell is occupied by a definitely placed item *)
  | AutoPlaced
(* Indicates that a grid cell is occupied by an item that was placed by the auto
   placement algorithm *)

(* A dynamically sized matrix (2d grid) which tracks the occupancy of each grid
   cell during auto-placement It also keeps tabs on how many tracks there are
   and which tracks are implicit and which are explicit *)
type t = {
  mutable inner : cell_occupancy_state array array;
      (* The grid of occupancy states *)
  mutable columns : Grid_track_counts.t;
      (* The counts of implicit and explicit columns *)
  mutable rows : Grid_track_counts.t;
      (* The counts of implicit and explicit rows *)
}

(* Create a CellOccupancyMatrix given a set of provisional track counts *)
let with_track_counts columns rows =
  let col_count = Grid_track_counts.len columns in
  let row_count = Grid_track_counts.len rows in
  let inner = Array.make_matrix row_count col_count Unoccupied in
  { inner; columns; rows }

(* Get the number of rows *)
let row_count t = Array.length t.inner

(* Get the number of columns *)
let col_count t = if row_count t > 0 then Array.length t.inner.(0) else 0

(* Determines whether the specified area fits within the tracks currently
   represented by the matrix *)
let is_area_in_range t primary_axis primary_range secondary_range =
  let open Absolute_axis in
  let primary_start, primary_end = primary_range in
  let secondary_start, secondary_end = secondary_range in

  let primary_count =
    match primary_axis with
    | Horizontal -> col_count t
    | Vertical -> row_count t
  in
  let secondary_count =
    match primary_axis with
    | Horizontal -> row_count t
    | Vertical -> col_count t
  in

  primary_start >= 0
  && primary_end <= primary_count
  && secondary_start >= 0
  && secondary_end <= secondary_count

(* Expands the grid (potentially in all 4 directions) in order to ensure that
   the specified range fits within the allocated space *)
let expand_to_fit_range t row_range col_range =
  let row_start, row_end = row_range in
  let col_start, col_end = col_range in

  (* Calculate number of rows and columns missing to accommodate ranges (if
     any) *)
  let req_negative_rows = max (-row_start) 0 in
  let req_positive_rows = max (row_end - row_count t) 0 in
  let req_negative_cols = max (-col_start) 0 in
  let req_positive_cols = max (col_end - col_count t) 0 in

  let old_row_count = row_count t in
  let old_col_count = col_count t in
  let new_row_count = old_row_count + req_negative_rows + req_positive_rows in
  let new_col_count = old_col_count + req_negative_cols + req_positive_cols in

  (* Create new matrix *)
  let new_inner = Array.make_matrix new_row_count new_col_count Unoccupied in

  (* Copy existing data to the new matrix *)
  for row = 0 to old_row_count - 1 do
    for col = 0 to old_col_count - 1 do
      new_inner.(row + req_negative_rows).(col + req_negative_cols) <-
        t.inner.(row).(col)
    done
  done;

  (* Update the matrix *)
  t.inner <- new_inner;
  t.rows <-
    {
      t.rows with
      negative_implicit = t.rows.negative_implicit + req_negative_rows;
      positive_implicit = t.rows.positive_implicit + req_positive_rows;
    };
  t.columns <-
    {
      t.columns with
      negative_implicit = t.columns.negative_implicit + req_negative_cols;
      positive_implicit = t.columns.positive_implicit + req_positive_cols;
    }

(* Mark an area of the matrix as occupied, expanding the allocated space as
   necessary to accommodate the passed area *)
let mark_area_as t primary_axis primary_span secondary_span value =
  let open Absolute_axis in
  let row_span, column_span =
    match primary_axis with
    | Horizontal -> (secondary_span, primary_span)
    | Vertical -> (primary_span, secondary_span)
  in

  let col_range =
    Grid_track_counts.oz_line_range_to_track_range t.columns column_span
  in
  let row_range =
    Grid_track_counts.oz_line_range_to_track_range t.rows row_span
  in

  (* Check that if the resolved ranges fit within the allocated grid. And if
     they don't then expand the grid to fit and then re-resolve the ranges once
     the grid has been expanded as the resolved indexes may have changed *)
  let is_in_range = is_area_in_range t Horizontal col_range row_range in
  let col_range, row_range =
    if not is_in_range then (
      expand_to_fit_range t row_range col_range;
      let col_range =
        Grid_track_counts.oz_line_range_to_track_range t.columns column_span
      in
      let row_range =
        Grid_track_counts.oz_line_range_to_track_range t.rows row_span
      in
      (col_range, row_range))
    else (col_range, row_range)
  in

  let row_start, row_end = row_range in
  let col_start, col_end = col_range in
  for row = row_start to row_end - 1 do
    for col = col_start to col_end - 1 do
      t.inner.(row).(col) <- value
    done
  done

(* Determines whether a grid area specified by a range of indexes into this
   CellOccupancyMatrix is entirely unnocupied. Returns true if all grid cells
   within the grid area are unoccupied, else false *)
(* Whether the cells of the rows [row_start] to [row_end] (exclusive) and the
   columns [col_start] to [col_end] are unoccupied. Out of bounds cells are
   considered unoccupied *)
let cells_are_unoccupied t ~row_start ~row_end ~col_start ~col_end =
  try
    for row = row_start to row_end - 1 do
      for col = col_start to col_end - 1 do
        if row >= 0 && row < row_count t && col >= 0 && col < col_count t then
          match t.inner.(row).(col) with Unoccupied -> () | _ -> raise Exit
      done
    done;
    true
  with Exit -> false

let track_area_is_unoccupied t primary_axis (primary_start, primary_end)
    (secondary_start, secondary_end) =
  match primary_axis with
  | Absolute_axis.Horizontal ->
      cells_are_unoccupied t ~row_start:secondary_start ~row_end:secondary_end
        ~col_start:primary_start ~col_end:primary_end
  | Vertical ->
      cells_are_unoccupied t ~row_start:primary_start ~row_end:primary_end
        ~col_start:secondary_start ~col_end:secondary_end

(* Returns the track counts of this CellOccupancyMatrix in the relevant axis *)
let track_counts t track_type =
  let open Absolute_axis in
  match track_type with Horizontal -> t.columns | Vertical -> t.rows

(* Determines whether the specified row contains any items *)
let row_is_occupied t row_index =
  if row_index >= row_count t then false
  else
    try
      for col = 0 to col_count t - 1 do
        match t.inner.(row_index).(col) with
        | Unoccupied -> ()
        | _ -> raise Exit
      done;
      false
    with Exit -> true

(* Determines whether a grid area specified by the bounding grid lines in
   OriginZero coordinates is entirely unnocupied. Returns true if all grid cells
   within the grid area are unoccupied, else false *)
let lines_are_unoccupied t primary_axis ~primary_start ~primary_end
    ~secondary_start ~secondary_end =
  (* A line in OriginZero coordinates is followed by the track at its index
     plus the negative implicit track count
     ([Grid_track_counts.oz_line_range_to_track_range]) *)
  let primary = (track_counts t primary_axis).negative_implicit in
  let secondary =
    (track_counts t (Absolute_axis.other primary_axis)).negative_implicit
  in
  let primary_start = primary_start + primary
  and primary_end = primary_end + primary
  and secondary_start = secondary_start + secondary
  and secondary_end = secondary_end + secondary in
  match primary_axis with
  | Absolute_axis.Horizontal ->
      cells_are_unoccupied t ~row_start:secondary_start ~row_end:secondary_end
        ~col_start:primary_start ~col_end:primary_end
  | Vertical ->
      cells_are_unoccupied t ~row_start:primary_start ~row_end:primary_end
        ~col_start:secondary_start ~col_end:secondary_end

let line_area_is_unoccupied t primary_axis (primary_span : int Line.t)
    (secondary_span : int Line.t) =
  lines_are_unoccupied t primary_axis ~primary_start:primary_span.start
    ~primary_end:primary_span.end_ ~secondary_start:secondary_span.start
    ~secondary_end:secondary_span.end_

(* Determines whether the specified column contains any items *)
let column_is_occupied t column_index =
  if column_index >= col_count t then false
  else
    try
      for row = 0 to row_count t - 1 do
        match t.inner.(row).(column_index) with
        | Unoccupied -> ()
        | _ -> raise Exit
      done;
      false
    with Exit -> true

(* Given an axis and a track index Search backwards from the end of the track
   and find the last grid cell matching the specified state (if any) Return the
   index of that cell or None *)
let last_of_type t track_type start_at kind =
  let open Absolute_axis in
  let track_counts = track_counts t (other track_type) in
  let track_computed_index =
    Grid_track_counts.oz_line_to_next_track track_counts start_at
  in

  (* Index out of bounds: no track to search *)
  if
    track_computed_index < 0
    || track_computed_index
       >=
       match track_type with
       | Horizontal -> row_count t
       | Vertical -> col_count t
  then None
  else
    let rec search_backwards idx =
      if idx < 0 then None
      else
        let cell =
          match track_type with
          | Horizontal -> t.inner.(track_computed_index).(idx)
          | Vertical -> t.inner.(idx).(track_computed_index)
        in
        if cell = kind then
          Some (Grid_track_counts.track_to_prev_oz_line track_counts idx)
        else search_backwards (idx - 1)
    in
    let max_idx =
      match track_type with
      | Horizontal -> col_count t - 1
      | Vertical -> row_count t - 1
    in
    search_backwards max_idx

(* Debug representation that represents the matrix in a compact 2d text
   format *)
let to_string t =
  let buf = Buffer.create 256 in
  Printf.bprintf buf "Rows: neg_implicit=%d explicit=%d pos_implicit=%d\n"
    t.rows.negative_implicit t.rows.explicit t.rows.positive_implicit;
  Printf.bprintf buf "Cols: neg_implicit=%d explicit=%d pos_implicit=%d\n"
    t.columns.negative_implicit t.columns.explicit t.columns.positive_implicit;
  Buffer.add_string buf "State:\n";

  for row = 0 to row_count t - 1 do
    for col = 0 to col_count t - 1 do
      let letter =
        match t.inner.(row).(col) with
        | Unoccupied -> '_'
        | DefinitelyPlaced -> 'D'
        | AutoPlaced -> 'A'
      in
      Buffer.add_char buf letter
    done;
    Buffer.add_char buf '\n'
  done;

  Buffer.contents buf
