(* Taffy uses two coordinate systems to refer to grid lines (the gaps/gutters
   between rows/columns) *)

open Geometry
open Style

type grid_line = int
(* Represents a grid line position in "CSS Grid Line" coordinates *)

(* Create a grid line from an integer value *)
let grid_line i = if i = 0 then failwith "Grid line of zero is invalid" else i

(* Returns the underlying integer *)
let grid_line_to_int gl = gl

type origin_zero_line = int
(* Represents a grid line position in "OriginZero" coordinates *)

(* Convert into OriginZero coordinates using the specified explicit track
   count *)
let grid_line_to_origin_zero_line gl explicit_track_count =
  let explicit_line_count = explicit_track_count + 1 in
  if gl > 0 then gl - 1
  else if gl < 0 then gl + explicit_line_count
  else failwith "Grid line of zero is invalid"

(* Converts a grid line in OriginZero coordinates into the index of that same
   grid line in the GridTrackVec. This fallible version is used for the
   placement of absolutely positioned grid items *)
let try_origin_zero_line_to_track_vec_index ozl track_counts =
  (* OriginZero grid line cannot be less than the number of negative grid
     lines *)
  if ozl < -track_counts.Grid.negative_implicit then
    None
    (* OriginZero grid line cannot be more than the number of positive grid
       lines *)
  else if ozl > track_counts.Grid.explicit + track_counts.Grid.positive_implicit
  then None
  else Some (2 * (ozl + track_counts.Grid.negative_implicit))

(* Converts a grid line in OriginZero coordinates into the index of that same
   grid line in the GridTrackVec *)
let origin_zero_line_to_track_vec_index ozl track_counts =
  match try_origin_zero_line_to_track_vec_index ozl track_counts with
  | Some idx -> idx
  | None ->
      if ozl > 0 then
        failwith
          "OriginZero grid line cannot be more than the number of positive \
           grid lines"
      else
        failwith
          "OriginZero grid line cannot be less than the number of negative \
           grid lines"

(* The minimum number of negative implicit tracks there must be if a grid item
   starts at this line *)
let origin_zero_line_implied_negative_implicit_tracks ozl =
  if ozl < 0 then abs ozl else 0

(* The minimum number of positive implicit tracks there must be if a grid item
   ends at this line *)
let origin_zero_line_implied_positive_implicit_tracks ozl explicit_track_count =
  if ozl > explicit_track_count then ozl - explicit_track_count else 0

(* The number of tracks between the start and end lines *)
let line_span line = max (line.Line.end_ - line.start) 0

(* Comparison function for origin zero lines *)
let compare_origin_zero_line = Int.compare

(* Equality function for origin zero lines *)
let equal_origin_zero_line = Int.equal
