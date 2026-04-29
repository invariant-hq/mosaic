(** Primary-screen viewport state.

    [Primary] models Matrix's primary-screen layout as an inline live viewport
    with static transcript rows above it. It is a pure state machine: it never
    writes to a terminal, mutates a {!Screen.t}, or owns I/O resources.

    The runtime interprets returned plans inside a frame transaction. *)

type t
(** Primary-screen state.

    Invariant:
    - terminal height is at least [1];
    - minimum live height is in \[[1];[terminal_height t]\];
    - [render_offset t >= 0];
    - [live_region t].row_offset = [render_offset t];
    - [live_region t].height >= [min_live_height t];
    - [render_offset t + (live_region t).height = terminal_height t]. *)

(** Terminal operations produced by primary-screen planning.

    Operations are terminal-shaped, but still inert. The Matrix runtime is
    responsible for encoding them as ANSI inside the current frame buffer. *)
type terminal_op =
  | Move_cursor of { row : int; col : int }
      (** Move the terminal cursor to one-based coordinates. *)
  | Reset_sgr  (** Reset SGR attributes before writing static text. *)
  | Write of string  (** Write literal bytes. *)
  | Erase_line  (** Erase the current terminal row. *)
  | Erase_below  (** Erase from cursor to the end of the screen. *)
  | Clear_and_home  (** Clear the screen and move the cursor home. *)
  | Scroll_up of int  (** Scroll the current terminal viewport up by rows. *)
  | Set_scroll_region of { top : int; bottom : int }
      (** Set DECSTBM to one-based inclusive rows. *)
  | Reset_scroll_region  (** Reset DECSTBM to the full screen. *)

type plan = {
  terminal_ops : terminal_op list;
      (** Terminal operations in the order the runtime must emit them. *)
  region_changed : bool;
      (** [true] iff the live viewport row offset or height may have changed. *)
  invalidate_presented : bool;
      (** [true] iff the {!Screen} presented baseline must be invalidated. *)
  force_full_redraw : bool;
      (** [true] iff the next live viewport render must be full. *)
}
(** A planned primary-screen transition. *)

type region = { row_offset : int; height : int }
(** A live viewport region.

    [row_offset] is zero-based. [height] is the number of live viewport rows. *)

type static_write = { text : string; rows : int }
(** A pending static write.

    [rows] is the exact number of terminal rows consumed by [text]. The caller
    is responsible for computing it. *)

type cursor_anchor = {
  render_offset : int;
  static_needs_newline : bool;
  scroll_bottom : bool;
}
(** Primary state derived from a cursor position report.

    [scroll_bottom] is [true] iff the cursor was on the bottom terminal row and
    the runtime must first write a newline to move that row into scrollback. *)

val empty_plan : plan
(** [empty_plan] performs no terminal operations and has no runtime effects. *)

val create :
  terminal_height:int ->
  min_live_height:int ->
  render_offset:int ->
  static_needs_newline:bool ->
  t
(** [create ~terminal_height ~min_live_height ~render_offset
     ~static_needs_newline] is primary state for a terminal of [terminal_height]
    rows.

    [min_live_height] is clamped to \[[1];[terminal_height]\]. [render_offset]
    is clamped so the live viewport keeps at least the clamped minimum height.
*)

val anchor_of_cursor :
  terminal_height:int -> row:int -> col:int -> cursor_anchor
(** [anchor_of_cursor ~terminal_height ~row ~col] is the primary state implied
    by a cursor position report.

    [row] and [col] are one-based terminal coordinates. The runtime owns the
    terminal write described by [scroll_bottom]. *)

val terminal_height : t -> int
(** [terminal_height t] is the current terminal height in rows. *)

val min_live_height : t -> int
(** [min_live_height t] is the minimum live viewport height in rows. *)

val render_offset : t -> int
(** [render_offset t] is the number of terminal rows above the live viewport. *)

val static_needs_newline : t -> bool
(** [static_needs_newline t] is [true] iff the next static write must first move
    to a fresh row to avoid continuing a prior mid-line write. *)

val live_region : t -> region
(** [live_region t] is the current live viewport region. *)

val effective_region : t -> region
(** [effective_region t] is the live viewport region that will be in effect
    after pending static writes are flushed.

    This must stay in lockstep with {!flush_static}. *)

val size : t -> width:int -> int * int
(** [size t ~width] is the current live viewport size as [(width, rows)]. *)

val effective_size : t -> width:int -> int * int
(** [effective_size t ~width] is the live viewport size that will be in effect
    after pending static writes are flushed.

    This must stay in lockstep with {!flush_static}. *)

val resize : t -> terminal_height:int -> t * plan
(** [resize t ~terminal_height] updates terminal height and clamps the live
    viewport if needed. *)

val reanchor : t -> render_offset:int -> static_needs_newline:bool -> t
(** [reanchor t ~render_offset ~static_needs_newline] updates the live viewport
    anchor without changing pending static writes. *)

val enqueue_static : t -> static_write -> t
(** [enqueue_static t write] queues [write] for the next static flush. Empty
    text is ignored. Raises [Invalid_argument] if [write.rows < 0]. *)

val has_pending_static : t -> bool
(** [has_pending_static t] is [true] iff [t] has queued static writes. *)

val flush_static : t -> t * plan
(** [flush_static t] plans all pending static writes and returns the updated
    state.

    The returned plan may physically change terminal rows outside the live
    viewport. The runtime must interpret it before rendering the next live
    frame.

    If the live viewport already occupies the full terminal height, static rows
    are scrolled directly into terminal history and the live viewport is left at
    the same size. *)

val clear_static : t -> t * plan
(** [clear_static t] resets primary static state and returns the terminal plan
    for clearing the primary screen. *)

val apply_required_rows :
  t -> active_rows:int -> required_rows:int option -> t * plan * int option
(** [apply_required_rows t ~active_rows ~required_rows] grows the live viewport
    if the current frame needs more rows.

    The returned [int option] is the maximum live viewport height to render.
    [None] means the full live viewport can be rendered. *)

val terminal_cursor_row : t -> live_row:int -> live_height:int -> int
(** [terminal_cursor_row t ~live_row ~live_height] maps a zero-based live
    viewport cursor row to a one-based terminal row, clamped to [live_height].
*)

val default_terminal_cursor_row : t -> live_height:int -> int
(** [default_terminal_cursor_row t ~live_height] is the terminal row used when
    the live cursor has no explicit position in primary mode. *)

val map_mouse_y : t -> int -> int
(** [map_mouse_y t y] maps a one-based terminal mouse row to the live viewport
    coordinate expected by Matrix input handlers. Rows above the live viewport
    map to [-1]. *)
