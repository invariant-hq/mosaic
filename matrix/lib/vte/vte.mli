(** Virtual terminal emulator.

    A virtual terminal emulator (VTE) processes raw byte streams containing
    ANSI/VT100 escape sequences and maintains the resulting terminal state: a
    visible {!Grid.t}, cursor position, style, scrollback history, and mode
    flags.

    The emulator maintains two screen buffers. The {e primary screen} has
    optional scrollback history; the {e alternate screen} is a temporary buffer
    used by full-screen applications like [vi] or [less] and never accumulates
    scrollback. The VTE automatically switches between buffers in response to
    DECSET/DECRST sequences.

    {b Dirty tracking.} Grid, cursor, and style mutations flip a dirty flag (see
    {!is_dirty} and {!dirty_rows}). Metadata changes such as {!title} updates or
    mode toggles do {e not} mark the VTE dirty; track those separately if you
    display them.

    {b Thread safety.} Values of type {!t} are mutable and {e not} thread-safe.

    {1:invariants Invariants}

    - Cursor row satisfies [0 <= row < rows t]. Column satisfies
      [0 <= col <= cols t]; [col = cols t] encodes the pending-wrap column
      required by DECAWM.
    - Scroll region is always within \[[0]; [{!rows} - 1]\].
    - The active grid is either primary or alternate, never both.
    - Scrollback only accumulates on the primary screen. *)

type t
(** The type for virtual terminal emulators. *)

(** {1:create Constructors} *)

val create :
  ?scrollback:int ->
  ?width_method:Text.width_method ->
  ?respect_alpha:bool ->
  ?default_fg:Ansi.Color.t ->
  ?default_bg:Ansi.Color.t ->
  rows:int ->
  cols:int ->
  unit ->
  t
(** [create ~rows ~cols ()] is a VTE with the given dimensions and:
    - [scrollback] is the maximum number of scrollback lines stored in a ring
      buffer. Defaults to [10000]. Use [0] to disable. Only applies to the
      primary screen.
    - [width_method] is the grapheme width computation method. Defaults to
      [`Unicode].
    - [respect_alpha] enables alpha blending for semi-transparent colours.
      Defaults to [false].
    - [default_fg] is the opaque foreground colour used when the current style
      has no explicit foreground. Defaults to {!Ansi.Color.white}.
    - [default_bg] is the opaque background colour used when the current style
      has no explicit background, and for clearing grids on resize or reset.
      Defaults to {!Ansi.Color.black}.

    [rows] and [cols] are clamped to a minimum of [1]. The returned VTE has both
    screens cleared with [default_bg], cursor at [(0, 0)] and visible, auto-wrap
    on, all other modes off, and the scroll region spanning the full screen. *)

(** {1:dims Terminal dimensions} *)

val rows : t -> int
(** [rows t] is [t]'s height in lines. *)

val cols : t -> int
(** [cols t] is [t]'s width in columns. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] resizes both grids to the given dimensions. Both
    grids are cleared after the resize. Cursor and scroll region are clamped to
    the new bounds. Scrollback content is preserved; compressed lines adapt to
    the new width during decompression.

    [rows] and [cols] are clamped to a minimum of [1]. Marks all rows dirty. *)

(** {1:input Input processing} *)

val feed : t -> bytes -> int -> int -> unit
(** [feed t buf ofs len] processes [len] bytes starting at offset [ofs] in [buf]
    as terminal input.

    Accepts partial and interleaved text/escape sequences. Invalid or
    unrecognised sequences are silently ignored. Updates the active grid,
    cursor, title, style, and mode flags.

    [ofs] and [len] must satisfy [0 <= ofs] and [ofs + len <= Bytes.length buf].
    Out-of-bounds access is undefined behaviour.

    O([len]), amortised constant time per byte. *)

val feed_string : t -> string -> unit
(** [feed_string t s] is
    [feed t (Bytes.unsafe_of_string s) 0 (String.length s)]. *)

(** {1:state Terminal state} *)

val grid : t -> Grid.t
(** [grid t] is the active visible grid (primary or alternate).

    {b Warning.} The grid is mutable and shared with [t]. External modifications
    break dirty tracking and may cause rendering inconsistencies. *)

val title : t -> string
(** [title t] is the terminal title set via OSC 0 or OSC 2 sequences. *)

val is_alternate_screen : t -> bool
(** [is_alternate_screen t] is [true] iff the alternate screen is active.
    Switched via DECSET/DECRST 47, 1047, or 1049.

    Defaults to [false]. *)

(** {1:dirty Dirty tracking} *)

val is_dirty : t -> bool
(** [is_dirty t] is [true] iff grid, cursor, or style state changed since the
    last {!clear_dirty}.

    {b Note.} OSC title updates and mode toggles do {e not} flip the dirty flag.
*)

val dirty_rows : t -> int list
(** [dirty_rows t] is the list of zero-based row indices modified since the last
    {!clear_dirty}, sorted ascending. O(k) where k is the number of dirty rows.
*)

val is_cursor_dirty : t -> bool
(** [is_cursor_dirty t] is [true] iff cursor position or visibility changed
    since the last {!clear_dirty}. *)

val clear_dirty : t -> unit
(** [clear_dirty t] resets the dirty flag, dirty row set, and cursor dirty flag.
    Does not alter content. *)

(** {1:cursor Cursor} *)

val cursor_pos : t -> int * int
(** [cursor_pos t] is the cursor position [(row, col)]. Satisfies
    [0 <= row < rows t] and [0 <= col <= cols t]. [col] can equal [cols t] in
    pending-wrap state when auto-wrap is enabled. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is [true] iff the cursor is visible. Controlled via
    DECTCEM (CSI ?25h/l). Defaults to [true]. *)

val set_cursor_pos : t -> row:int -> col:int -> unit
(** [set_cursor_pos t ~row ~col] moves the cursor. [row] is clamped to \[0,
    rows-1\], [col] to \[0, cols\]. Marks cursor dirty if the position changes.
*)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t v] shows or hides the cursor. Marks cursor dirty if
    visibility changes. *)

(** {1:scrollback Scrollback} *)

val scrollback_capacity : t -> int
(** [scrollback_capacity t] is the maximum number of scrollback lines. [0] if
    scrollback is disabled. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the current number of lines in the scrollback buffer.
    [0] if scrollback is disabled, empty, or the alternate screen is active.
    Satisfies [0 <= scrollback_size t <= scrollback_capacity t]. *)

val scrollback_lines : t -> string list
(** [scrollback_lines t] is the scrollback content as plain-text strings ordered
    oldest to newest. Style information is discarded. Trailing spaces are
    trimmed during compression. Empty if scrollback is disabled, empty, or the
    alternate screen is active. O({!scrollback_size}). *)

val render_with_scrollback : t -> offset:int -> Grid.t -> unit
(** [render_with_scrollback t ~offset dst] renders a snapshot combining
    scrollback history and the live screen into [dst].

    The top rows of [dst] are filled with scrollback lines (oldest at the top)
    and the remaining rows with the current screen. [offset] is the number of
    lines above the live screen the snapshot extends: [0] shows only the live
    terminal. [offset] is clamped to \[0, {!scrollback_size}\].

    [dst] should have the same width as [t] for correct glyph placement; smaller
    widths clip decompressed graphemes. On the alternate screen (no scrollback)
    this is equivalent to [Grid.blit ~src:(grid t) ~dst].

    O(Grid.height dst × {!cols}). *)

(** {1:modes Terminal modes} *)

val auto_wrap_mode : t -> bool
(** [auto_wrap_mode t] is [true] iff auto-wrap (DECAWM) is active. When enabled,
    writing past the right margin wraps to the next line. Controlled via CSI
    ?7h/l. Defaults to [true]. *)

val insert_mode : t -> bool
(** [insert_mode t] is [true] iff insert mode (IRM) is active. Characters push
    existing content right instead of replacing it. Controlled via CSI 4h/l.
    Defaults to [false]. *)

val cursor_key_mode : t -> bool
(** [cursor_key_mode t] is [true] iff cursor keys use application mode (DECCKM).
    Application mode sends [ESC O A] instead of [ESC \[ A] for arrow keys.
    Controlled via CSI ?1h/l. Defaults to [false]. *)

val bracketed_paste_mode : t -> bool
(** [bracketed_paste_mode t] is [true] iff bracketed paste is active. Pasted
    text is wrapped with [ESC \[ 200 ~] / [ESC \[ 201 ~] markers. Controlled via
    CSI ?2004h/l. Defaults to [false]. *)

val origin_mode : t -> bool
(** [origin_mode t] is [true] iff origin mode (DECOM) is active. In origin mode,
    CUP/HVP coordinates are relative to the scroll region.

    {b Note.} DECOM handling is not wired yet; this accessor always returns
    [false] in the current release. *)

(** {1:scrolling Scrolling} *)

val scroll_up : t -> int -> unit
(** [scroll_up t n] scrolls the current scroll region up by [n] lines. Lines
    leaving the top enter scrollback only when the primary screen is active,
    scrollback is enabled, and the region starts at row 0. Newly exposed rows
    are cleared with the default transparent background. No effect if [n <= 0].

    Marks all rows in the scroll region dirty. *)

val scroll_down : t -> int -> unit
(** [scroll_down t n] scrolls the current scroll region down by [n] lines. Blank
    rows are inserted at the top; lines leaving the bottom are discarded (never
    saved to scrollback). No effect if [n <= 0].

    Marks all rows in the scroll region dirty.

    See also {!scroll_up}. *)

(** {1:control Terminal control} *)

val reset : t -> unit
(** [reset t] resets [t] to its initial state. Clears both grids and scrollback,
    moves cursor to [(0, 0)] and makes it visible, resets style to default, sets
    auto-wrap on and all other modes off, resets scroll region to full screen,
    and switches to the primary screen. Dimensions are unchanged. Equivalent to
    RIS ([ESC c]).

    Marks all rows dirty. *)

(** {1:debug Debugging} *)

val to_string : t -> string
(** [to_string t] is the visible screen content as a multi-line string. One line
    per row, separated by newlines. Trailing spaces are preserved. Wide
    graphemes appear once. Style information is not included. The result does
    not end with a trailing newline. *)
