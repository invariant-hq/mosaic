(** Hardware cursor state tracking.

    [Cursor_state] tracks the desired position, style, visibility, and colour of
    the terminal's hardware cursor. Style, visibility, and colour use
    {e delta encoding}: only changes since the last {!emit} produce escape
    sequences. Position is emitted whenever requested (no delta tracking).

    {1:types Types} *)

type t
(** The type for mutable cursor state. Tracks both the desired cursor state and
    the last emitted state for delta encoding. *)

type snapshot = {
  row : int;  (** One-based row. *)
  col : int;  (** One-based column. *)
  has_position : bool;
      (** [true] iff a position has been set via {!set_position}. *)
  style : [ `Block | `Line | `Underline ];  (** Cursor shape. *)
  blinking : bool;  (** [true] iff the cursor blinks. *)
  color : (int * int * int) option;
      (** [Some (r, g, b)] or [None] for the terminal default. *)
  visible : bool;  (** [true] iff the cursor is logically visible. *)
}
(** The type for cursor state snapshots. *)

(** {1:constructors Constructors} *)

val create : unit -> t
(** [create ()] is a fresh cursor state with all emitted state marked as
    unknown, forcing full re-emission on the first {!emit}. *)

(** {1:state State management} *)

val set_position : t -> row:int -> col:int -> unit
(** [set_position t ~row ~col] sets the desired cursor position. Coordinates are
    clamped to [>= 1]. *)

val clear_position : t -> unit
(** [clear_position t] clears the desired position so the cursor remains
    wherever the last rendering operation left it. *)

val set_style :
  t -> style:[ `Block | `Line | `Underline ] -> blinking:bool -> unit
(** [set_style t ~style ~blinking] sets the desired cursor shape and blinking
    behaviour. *)

val set_color : t -> (int * int * int) option -> unit
(** [set_color t c] sets the desired cursor colour. [None] means the terminal
    default; [Some (r, g, b)] are values in \[[0]; [255]\]. *)

val set_visible : t -> bool -> unit
(** [set_visible t b] sets the logical cursor visibility. *)

val is_visible : t -> bool
(** [is_visible t] is [true] iff the cursor is logically visible. *)

val clamp_to_bounds : t -> max_row:int -> max_col:int -> unit
(** [clamp_to_bounds t ~max_row ~max_col] restricts [t]'s stored position to the
    given limits, if a position is set. *)

val snapshot : t -> snapshot
(** [snapshot t] is the current desired cursor state. *)

val reset : t -> unit
(** [reset t] marks all emitted state as unknown, forcing full re-emission on
    the next {!emit}. *)

(** {1:emission Emission} *)

val hide_temporarily : t -> Ansi.writer -> unit
(** [hide_temporarily t w] emits a hide sequence through [w] if the cursor is
    visible or in an unknown state. Updates the internal tracking state to
    reflect the hide.

    Used by render loops to hide the cursor during drawing without altering the
    logical visibility. *)

val emit : t -> row_offset:int -> Ansi.writer -> unit
(** [emit t ~row_offset w] synchronises the terminal cursor with the desired
    state by emitting escape sequences through [w].

    Style, colour, and visibility are delta-encoded: only values that differ
    from the last emission produce output. Position is emitted whenever
    {!set_position} has been called, regardless of whether the coordinates
    changed.

    [row_offset] is added to the row coordinate during positioning. *)
