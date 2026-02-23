(** Grapheme-aware text editing buffer.

    A mutable buffer that tracks cursor position, selection, and undo history in
    terms of Unicode grapheme clusters. Content may contain newlines for
    multi-line editing. *)

type t
(** The type for mutable editing buffers. *)

(** {1:helpers Helpers} *)

val strip_newlines : string -> string
(** [strip_newlines s] is [s] with all CR, LF, and CRLF sequences removed.

    {b Note.} When [s] contains no newline characters the original string is
    returned without allocation. *)

(** {1:constructors Constructors} *)

val create : ?max_length:int -> string -> t
(** [create ?max_length initial] is a buffer whose initial content is [initial],
    with cursor at the end.

    [max_length] is the maximum number of grapheme clusters the buffer may hold.
    [initial] is truncated to [max_length] grapheme clusters if it exceeds that
    limit. [max_length] defaults to [1000] and is clamped to [>= 0].

    See also {!set_max_length}. *)

(** {1:content Content} *)

val text : t -> string
(** [text t] is the current content of [t]. *)

val set_text : t -> string -> unit
(** [set_text t s] replaces the content of [t] with [s].

    [s] is truncated to {!max_length} [t] grapheme clusters if it exceeds that
    limit. The cursor moves to the end and any active selection is cleared. No
    undo point is saved. *)

val length : t -> int
(** [length t] is the number of grapheme clusters in [t]. *)

val display_width : t -> int
(** [display_width t] is the total display width of [t] in terminal columns. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff {!length} [t] is [0]. *)

(** {1:lines Line information} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines in [t]. Always [>= 1]. *)

val cursor_line : t -> int
(** [cursor_line t] is the 0-based logical line index of the cursor. *)

val cursor_col : t -> int
(** [cursor_col t] is the grapheme column of the cursor within its logical line.
*)

(** {1:cursor Cursor} *)

val cursor : t -> int
(** [cursor t] is the cursor position as a 0-based grapheme cluster offset. The
    value is in \[[0];[length t]\]. *)

val set_cursor : t -> int -> unit
(** [set_cursor t pos] moves the cursor to [pos], clamped to \[[0];[length t]\].
    Clears any active selection. *)

val set_cursor_offset : ?select:bool -> t -> int -> unit
(** [set_cursor_offset ?select t pos] moves the cursor to [pos], clamped to
    \[[0];[length t]\].

    When [select] is [true], extends or creates a selection anchored at the
    previous cursor position. When [select] is [false] (default), any active
    selection is cleared.

    See also {!set_cursor}. *)

val cursor_display_offset : t -> int
(** [cursor_display_offset t] is the display column of the cursor in terminal
    columns. *)

(** {1:selection Selection} *)

val selection : t -> (int * int) option
(** [selection t] is [Some (lo, hi)] when a selection is active, where [lo < hi]
    are grapheme cluster offsets. [None] when no selection is active. *)

val has_selection : t -> bool
(** [has_selection t] is [true] iff {!selection} [t] is [Some _]. *)

val selected_text : t -> string
(** [selected_text t] is the text of the current selection, or [""] if no
    selection is active. *)

val clear_selection : t -> unit
(** [clear_selection t] clears the active selection without moving the cursor.
*)

val select_all : t -> unit
(** [select_all t] selects all text in [t] and moves the cursor to the end. *)

(** {1:editing Editing} *)

val insert : t -> string -> bool
(** [insert t s] is [true] iff inserting [s] changed the text of [t].

    If a selection is active it is deleted first. Then [s] is inserted at the
    cursor position. The result is truncated to {!max_length} [t] grapheme
    clusters. An undo point is saved before any modification. *)

val delete_backward : t -> bool
(** [delete_backward t] is [true] iff a deletion changed the text of [t].

    If a selection is active the selected text is deleted. Otherwise the
    grapheme cluster immediately before the cursor (Backspace behaviour) is
    deleted. An undo point is saved before any modification. *)

val delete_forward : t -> bool
(** [delete_forward t] is [true] iff a deletion changed the text of [t].

    If a selection is active the selected text is deleted. Otherwise the
    grapheme cluster immediately after the cursor (Delete behaviour) is deleted.
    An undo point is saved before any modification. *)

val delete_word_backward : t -> bool
(** [delete_word_backward t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the previous word boundary. An undo point is
    saved before any modification. *)

val delete_word_forward : t -> bool
(** [delete_word_forward t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the next word boundary. An undo point is saved
    before any modification. *)

val delete_to_line_start : t -> bool
(** [delete_to_line_start t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the start of the current logical line. An undo
    point is saved before any modification. *)

val delete_to_line_end : t -> bool
(** [delete_to_line_end t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the end of the current logical line (before any
    newline). Returns [false] when the cursor is already at the line end. An
    undo point is saved before any modification. *)

val delete_to_start : t -> bool
(** [delete_to_start t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the start of the buffer. An undo point is saved
    before any modification. *)

val delete_to_end : t -> bool
(** [delete_to_end t] is [true] iff a deletion changed the text of [t].

    Deletes from the cursor to the end of the buffer. An undo point is saved
    before any modification. *)

val delete_line : t -> bool
(** [delete_line t] is [true] iff a deletion changed the text of [t].

    Deletes the current logical line including its trailing line separator. An
    undo point is saved before any modification. *)

(** {1:movement Cursor movement}

    Movement operations return [true] iff the cursor position changed.

    When [select] is [true] the operation extends or creates a selection. When
    [select] is [false] (default) any active selection is cleared. If a
    selection was active and [select] is [false], the cursor moves to the
    selection edge in the direction of movement. *)

val move_left : ?select:bool -> t -> bool
(** [move_left ?select t] is [true] iff the cursor moved.

    Moves the cursor one grapheme cluster to the left. [select] defaults to
    [false]. *)

val move_right : ?select:bool -> t -> bool
(** [move_right ?select t] is [true] iff the cursor moved.

    Moves the cursor one grapheme cluster to the right. [select] defaults to
    [false]. *)

val move_word_forward : ?select:bool -> t -> bool
(** [move_word_forward ?select t] is [true] iff the cursor moved.

    Moves the cursor to the next word boundary. [select] defaults to [false]. *)

val move_word_backward : ?select:bool -> t -> bool
(** [move_word_backward ?select t] is [true] iff the cursor moved.

    Moves the cursor to the previous word boundary. [select] defaults to
    [false]. *)

val move_home : ?select:bool -> t -> bool
(** [move_home ?select t] is [true] iff the cursor moved.

    Moves the cursor to the start of the buffer. [select] defaults to [false].
*)

val move_end : ?select:bool -> t -> bool
(** [move_end ?select t] is [true] iff the cursor moved.

    Moves the cursor to the end of the buffer. [select] defaults to [false]. *)

val move_line_start : ?select:bool -> t -> bool
(** [move_line_start ?select t] is [true] iff the cursor moved.

    Moves the cursor to the start of the current logical line. [select] defaults
    to [false]. *)

val move_line_end : ?select:bool -> t -> bool
(** [move_line_end ?select t] is [true] iff the cursor moved.

    Moves the cursor to the end of the current logical line, before any trailing
    newline. [select] defaults to [false]. *)

(** {1:undo Undo and redo} *)

val undo : t -> bool
(** [undo t] is [true] iff undoing restored a previous state.

    Restores the most recent undo point. Returns [false] when no undo history is
    available. *)

val redo : t -> bool
(** [redo t] is [true] iff redoing re-applied a change.

    Re-applies the most recently undone change. Returns [false] when no redo
    history is available. *)

(** {1:limits Limits} *)

val max_length : t -> int
(** [max_length t] is the maximum number of grapheme clusters [t] may hold. *)

val set_max_length : t -> int -> unit
(** [set_max_length t n] sets the maximum grapheme cluster count of [t] to [n],
    clamped to [>= 0]. If the current content exceeds [n] grapheme clusters it
    is truncated to fit.

    See also {!create}. *)
