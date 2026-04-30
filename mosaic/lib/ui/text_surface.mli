(** Text rendering surface with wrapping, viewport, and selection.

    A surface wraps a {!Text_buffer.t} and a {!Renderable.t}, providing line
    wrapping, viewport-based scrolling, intrinsic measurement, and selection
    overlay rendering. The surface registers render and measure callbacks on the
    renderable automatically.

    Used by {!Text} for static rich text display, by {!Textarea} for multi-line
    editing, and potentially by other renderables that need text buffer
    rendering.

    See {!Text_buffer} for the underlying styled text storage. *)

type t
(** A text rendering surface. *)

type wrap = [ `None | `Char | `Word ]
(** Wrapping mode.
    - [`None]: no wrapping; lines extend beyond the viewport.
    - [`Char]: break at grapheme cluster boundaries.
    - [`Word]: break at word boundaries (spaces, hyphens, etc.); falls back to
      [`Char] when a word exceeds the wrap width. *)

(** {1:construction Construction} *)

val create : Renderable.t -> Text_buffer.t -> t
(** [create node buffer] is a surface that renders [buffer] through [node].
    Registers render and measure callbacks on [node]. *)

(** {1:accessors Accessors} *)

val buffer : t -> Text_buffer.t
(** [buffer t] is the underlying text buffer. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

(** {1:wrapping Wrapping} *)

val wrap : t -> wrap
(** [wrap t] is the current wrapping mode. *)

val set_wrap : t -> wrap -> unit
(** [set_wrap t mode] changes the wrapping mode. Invalidates display lines and
    marks layout dirty. *)

val wrap_width : t -> int option
(** [wrap_width t] is the explicit wrap width, or [None] when the width is
    derived from the renderable's layout width. *)

val set_wrap_width : t -> int option -> unit
(** [set_wrap_width t w] sets an explicit wrap width. [None] derives the width
    from the renderable's layout. Invalidates display lines and marks layout
    dirty. *)

val truncate : t -> bool
(** [truncate t] is [true] when lines are truncated with an ellipsis. Only
    applies when [wrap t = `None]. *)

val set_truncate : t -> bool -> unit
(** [set_truncate t v] enables or disables line truncation. When enabled and
    [wrap = `None], lines exceeding the viewport width are cut with an ellipsis
    character. Invalidates display lines and marks layout dirty. *)

(** {1:viewport Viewport} *)

val scroll_x : t -> int
(** [scroll_x t] is the horizontal scroll offset in columns. *)

val scroll_y : t -> int
(** [scroll_y t] is the vertical scroll offset in display lines. *)

val set_scroll_x : t -> int -> unit
(** [set_scroll_x t x] sets the horizontal scroll offset, clamped to
    \[[0];{!max_scroll_x}\]. Requests a render if changed. *)

val set_scroll_x_for_cursor : t -> int -> unit
(** [set_scroll_x_for_cursor t x] sets the horizontal scroll offset for an
    editable insertion cursor. Unlike {!set_scroll_x}, this may scroll one extra
    cell past the last content column so a cursor at end-of-line remains
    visible. *)

val set_scroll_y : t -> int -> unit
(** [set_scroll_y t y] sets the vertical scroll offset, clamped to
    \[[0];{!max_scroll_y}\]. Requests a render if changed. *)

val scroll_height : t -> int
(** [scroll_height t] is the total number of display lines. *)

val scroll_width : t -> int
(** [scroll_width t] is the maximum display line width in columns. *)

val max_scroll_x : t -> int
(** [max_scroll_x t] is [max 0 ({!scroll_width} t - viewport_width)]. *)

val max_scroll_y : t -> int
(** [max_scroll_y t] is [max 0 ({!scroll_height} t - viewport_height)]. *)

(** {1:display Display lines} *)

type display_line = Text_buffer.span list
(** A display line is a list of styled spans that fit within the wrap width. *)

type display_info = {
  lines : display_line array;  (** The wrapped display lines. *)
  line_sources : int array;
      (** [line_sources.(i)] is the logical line index that display line [i]
          originates from. *)
  line_grapheme_offsets : int array;
      (** [line_grapheme_offsets.(i)] is the grapheme offset of display line [i]
          in the full text. Used for mapping highlights and cursor positions. *)
  line_wrap_indices : int array;
      (** [line_wrap_indices.(i)] is the sub-line index within its logical line:
          [0] for the first display line, [1] for the first continuation, etc.
          Useful for line numbering and gutter rendering. *)
  max_line_width : int;  (** The widest display line in columns. *)
}
(** Wrapped display line metrics. *)

val display_info : t -> display_info
(** [display_info t] is the current wrapped line metrics. Cached; recomputed
    when content or wrap settings change. *)

val display_line_count : t -> int
(** [display_line_count t] is [Array.length (display_info t).lines]. *)

(** {1:selection Selection} *)

val set_selection_bg : t -> Ansi.Color.t option -> unit
(** [set_selection_bg t color] sets the selection background color. *)

val set_selection_fg : t -> Ansi.Color.t option -> unit
(** [set_selection_fg t color] sets the selection foreground color. *)

val set_selection : t -> start:int -> end_:int -> bool
(** [set_selection t ~start ~end_] sets the selection range by grapheme offsets
    into the buffer content. Offsets are clamped to content bounds. The
    selection is active when [start <> end_]. Returns [true] if the selection
    state changed. *)

val selection : t -> (int * int) option
(** [selection t] is [Some (start, end_)] when a selection is active, with
    [start < end_] as normalized grapheme offsets. [None] when no selection is
    active. *)

val set_local_selection :
  t -> anchor_x:int -> anchor_y:int -> focus_x:int -> focus_y:int -> bool
(** [set_local_selection t ~anchor_x ~anchor_y ~focus_x ~focus_y] sets the
    selection from viewport-local coordinates. Coordinates are converted to
    grapheme offsets accounting for scroll position. Activates the selection
    unconditionally. Returns [true] if the selection changed.

    See {!update_local_selection} for updating an existing drag. *)

val update_local_selection :
  t -> anchor_x:int -> anchor_y:int -> focus_x:int -> focus_y:int -> bool
(** [update_local_selection t ~anchor_x ~anchor_y ~focus_x ~focus_y] is like
    {!set_local_selection} but only considers offset changes (does not re-check
    active state). Returns [true] if the selection changed. *)

val reset_selection : t -> unit
(** [reset_selection t] clears any active selection and requests a render. *)

val has_selection : t -> bool
(** [has_selection t] is [true] iff a selection is active. *)

val selected_text : t -> string
(** [selected_text t] is the text within the current selection. Returns [""] if
    no selection is active. *)

(** {1:invalidation Invalidation} *)

val invalidate : t -> unit
(** [invalidate t] clears cached display lines, marks layout dirty, and requests
    a render. Call after modifying the underlying buffer externally. *)
