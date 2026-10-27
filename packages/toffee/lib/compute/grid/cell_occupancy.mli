(** Cell occupancy tracking for CSS Grid auto-placement.

    Tracks which grid cells are occupied during auto-placement using a
    dynamically expanding 2D matrix. Maintains counts of implicit and explicit
    tracks in both axes.

    Coordinates use the OriginZero system where negative implicit tracks have
    negative indices, explicit tracks start at 0, and positive implicit tracks
    follow the explicit ones.

    {1 Grid Expansion}

    The matrix expands automatically in all four directions when items are
    placed outside current bounds. Track counts update accordingly to maintain
    correct coordinate mappings.

    {1 Key Invariants}

    - Out-of-bounds cells are treated as unoccupied
    - Track counts must accurately reflect the matrix dimensions after expansion
    - Coordinate conversions depend on track counts being synchronized with
      matrix size *)

type cell_occupancy_state = Unoccupied | DefinitelyPlaced | AutoPlaced
type t

val with_track_counts : Grid_track_counts.t -> Grid_track_counts.t -> t
(** [with_track_counts columns rows] creates an occupancy matrix with
    provisional track counts. *)

val row_count : t -> int
(** [row_count t] returns the number of rows in the matrix. *)

val col_count : t -> int
(** [col_count t] returns the number of columns in the matrix. *)

val is_area_in_range :
  t -> Toffee_geometry.Absolute_axis.t -> int * int -> int * int -> bool
(** [is_area_in_range t primary_axis primary_range secondary_range] checks if
    the specified area fits within current matrix bounds.

    Ranges are (start, end) pairs in track indices. *)

val expand_to_fit_range : t -> int * int -> int * int -> unit
(** [expand_to_fit_range t row_range col_range] expands the matrix to
    accommodate the specified ranges, adding implicit tracks as needed.

    Algorithm:

    + Calculate required expansion in all four directions
    + Allocate new matrix with expanded dimensions
    + Copy existing cells with offset for negative expansion
    + Update track counts to reflect new implicit tracks

    Mutates [t.inner], [t.rows], and [t.columns]. *)

val mark_area_as :
  t ->
  Toffee_geometry.Absolute_axis.t ->
  int Toffee_geometry.Line.t ->
  int Toffee_geometry.Line.t ->
  cell_occupancy_state ->
  unit
(** [mark_area_as t primary_axis primary_span secondary_span value] marks the
    specified area as occupied, expanding the matrix if necessary.

    Spans are in OriginZero line coordinates. Automatically resolves to track
    ranges and expands the grid if the area extends beyond current bounds.
    Re-resolves coordinates after expansion since track indices may shift. *)

val track_area_is_unoccupied :
  t -> Toffee_geometry.Absolute_axis.t -> int * int -> int * int -> bool
(** [track_area_is_unoccupied t primary_axis primary_range secondary_range]
    checks if all cells in the area are unoccupied.

    Ranges are (start, end) pairs in track indices. Out-of-bounds cells are
    treated as unoccupied. *)

val line_area_is_unoccupied :
  t ->
  Toffee_geometry.Absolute_axis.t ->
  int Toffee_geometry.Line.t ->
  int Toffee_geometry.Line.t ->
  bool
(** [line_area_is_unoccupied t primary_axis primary_span secondary_span] checks
    if all cells in the area are unoccupied.

    Spans are in OriginZero line coordinates. Converts to track ranges before
    checking occupancy. *)

val lines_are_unoccupied :
  t ->
  Toffee_geometry.Absolute_axis.t ->
  primary_start:int ->
  primary_end:int ->
  secondary_start:int ->
  secondary_end:int ->
  bool
(** [lines_are_unoccupied t primary_axis ~primary_start ~primary_end
     ~secondary_start ~secondary_end] is {!line_area_is_unoccupied} of the spans
    from [primary_start] to [primary_end] and from [secondary_start] to
    [secondary_end], given as their lines. *)

val row_is_occupied : t -> int -> bool
(** [row_is_occupied t row_index] checks if the specified row contains any
    occupied cells.

    Returns [false] if [row_index] is out of bounds. *)

val column_is_occupied : t -> int -> bool
(** [column_is_occupied t column_index] checks if the specified column contains
    any occupied cells.

    Returns [false] if [column_index] is out of bounds. *)

val track_counts : t -> Toffee_geometry.Absolute_axis.t -> Grid_track_counts.t
(** [track_counts t track_type] returns the track counts for the specified axis.
*)

val last_of_type :
  t ->
  Toffee_geometry.Absolute_axis.t ->
  int ->
  cell_occupancy_state ->
  int option
(** [last_of_type t track_type start_at kind] searches backwards from the end of
    the specified track to find the last cell matching [kind].

    Returns the OriginZero line index of the matching cell, or [None] if no
    match is found or the track is out of bounds. *)

val to_string : t -> string
(** [to_string t] returns a debug representation showing track counts and cell
    states in compact 2D format.

    States are represented as: [_] = Unoccupied, [D] = DefinitelyPlaced, [A] =
    AutoPlaced. *)
