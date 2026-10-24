(** Primary-screen layout.

    In [`Primary] mode Matrix shares the terminal's normal screen with the
    shell. The screen's rows are a {e transcript}, the shell's output and then
    the static rows the application writes, above a {e live region} that the
    application redraws every frame and that ends at the bottom row. A row
    leaves the transcript only by scrolling off the top of the screen into the
    terminal's scrollback, so every transcript row stays, once and in order,
    either in the scrollback or on the screen.

    The live region grows by scrolling: a line feed on the bottom row moves the
    whole screen up and its top row into the scrollback, so the region claims
    rows without covering a transcript row. Every terminal with a scrollback
    does this, and it needs no scrolling margins.

    [Primary] is a pure state machine: it never writes to a terminal. A
    transition returns the terminal operations that realise it, which the
    runtime emits, in order, before it next draws the live region. After
    operations, or a move of the live region, the runtime redraws the live
    region whole. *)

type t
(** The type for layouts.

    Invariant: [0 <= render_offset t < h], where [h >= 1] is the terminal
    height, and the live region is the [h - render_offset t] rows from
    [render_offset t]. After a transition other than {!enqueue_static} and
    {!replace_static} it has at least [min h m] rows, where [m] is the minimum
    live height. *)

(** The type for terminal operations. *)
type terminal_op =
  | Move_cursor of { row : int; col : int }
      (** Move the cursor to one-based coordinates. *)
  | Reset_sgr  (** Reset SGR attributes. *)
  | Write of string  (** Write the bytes. *)
  | Draw of (unit -> string)
      (** Write the bytes the function gives when the operation is done. *)
  | Clear_and_home  (** Erase the screen and move the cursor home. *)
  | Clear_scrollback  (** Erase the terminal's scrollback. *)

type region = { row_offset : int; height : int }
(** The type for live regions: [height] rows from the zero-based row
    [row_offset]. *)

type static_write = {
  draw : top:int -> string;
  rows : int;
  preserve_live_region : bool;
}
(** The type for static writes. [draw ~top] is [rows] rows, each ending in CRLF
    and each replacing the whole terminal row it lands on, for rows that end on
    the terminal's rows from [top], from [0], negative when some scroll off the
    screen. When [preserve_live_region] is [true], the live region keeps its
    height: the rows above it, the new rows included, scroll up to make room.
    Otherwise the live region gives up rows to the new ones, down to its minimum
    height. *)

val create : terminal_height:int -> min_live_height:int -> t
(** [create ~terminal_height ~min_live_height] is the layout of a terminal of
    [terminal_height] rows with a live region of at least [min_live_height]
    rows, both at least [1], and no transcript: the live region is the screen.
*)

val anchor : t -> row:int -> col:int -> t * terminal_op list
(** [anchor t ~row ~col] starts the live region at the cursor, reported at the
    one-based [(row, col)], clamped to the screen: on the cursor's row when the
    cursor is in the first column, where the row holds nothing yet, and on the
    next row otherwise, below the shell's prompt. The rows above it are
    transcript. *)

val resize : t -> terminal_height:int -> t * terminal_op list
(** [resize t ~terminal_height] is the layout after the terminal changed its
    height, if its rows kept their places counted from the top. A terminal may
    instead move them, dropping the rows below its cursor or pulling rows back
    from its scrollback: a runtime that can ask where the cursor went uses
    {!anchor} instead. *)

val enqueue_static : t -> static_write -> t
(** [enqueue_static t w] queues [w] for the next {!flush_static}. A write of no
    rows is ignored.

    Raises [Invalid_argument] if [w.rows < 0]. *)

val replace_static : t -> static_write -> t
(** [replace_static t w] queues, for the next {!flush_static}, the replacement
    of the whole transcript, the terminal's scrollback included, by [w], which
    may have no rows.

    Raises [Invalid_argument] if [w.rows < 0]. *)

val flush_static : t -> t * terminal_op list
(** [flush_static t] writes the queued static rows, in order, at the top of the
    live region, which then starts below them. *)

val clear_static : t -> t * terminal_op list
(** [clear_static t] erases the screen, drops the queued writes, and makes the
    live region the whole screen. *)

val require_rows : t -> rows:int -> t * terminal_op list
(** [require_rows t ~rows] grows the live region to [rows] rows, at most the
    terminal's height, if it is smaller. *)

val render_offset : t -> int
(** [render_offset t] is the number of rows above the live region. *)

val live_region : t -> region
(** [live_region t] is the live region. *)

val effective_region : t -> region
(** [effective_region t] is the live region after the next {!flush_static}. *)

val terminal_cursor_row : t -> live_row:int -> int
(** [terminal_cursor_row t ~live_row] is the one-based terminal row of the
    zero-based live row [live_row], clamped to the live region. *)

val map_mouse_y : t -> int -> int
(** [map_mouse_y t y] maps the zero-based terminal row [y] of a mouse report to
    the zero-based row of the live region it is on: the live region's first row
    is [0]. Rows above the live region map to [-1]. *)
