(** Persistent text editing state. *)

type t
(** Text, caret, selection, undo history, and the viewport at which an edit
    surface shows them. Operations leave their argument intact. Cursor-only
    changes share the indexed source and history. A text edit copies the text
    and indexes again only the lines it touches
    ({!Matrix.Text.Layout.Source.edit}).

    The viewport is interaction state like the caret: wheel scrolling over an
    edit surface proposes a value with a new viewport, and a surface shows the
    viewport of the value it is given, so remounting a widget does not move it.
    A value from {!of_string} shows the start of its text; a value from an
    editing or movement operation keeps its caret visible.

    A value carries no identity: whichever value its owner stores is the state,
    whatever it was derived from, and two values built by the same operations
    behave alike. *)

val empty : t
(** [empty] has empty text and no history. *)

val of_string : ?max_length:int -> string -> t
(** [of_string ?max_length text] places the caret at the end of [text].
    [max_length] defaults to [max_int] and limits grapheme clusters, clamped to
    zero. Initial text exceeding it is truncated. *)

val text : t -> string
(** [text t] is the editor's text, without copying. *)

val length : t -> int
(** [length t] is its number of grapheme clusters. *)

val cursor : t -> int
(** [cursor t] is the caret's grapheme boundary. *)

val caret : t -> Matrix.Text.Layout.caret
(** [caret t] includes the boundary's soft-wrap affinity. *)

val selection : t -> (int * int) option
(** [selection t] is the selected half-open grapheme range, if nonempty. *)

val selected_text : t -> string
(** [selected_text t] is the selected text, or the empty string. *)

val source : t -> Matrix.Text.Layout.Source.t
(** [source t] is the immutable indexed text shared with layout. *)

val clear : t -> t
(** [clear t] deletes all text, as an undo step of its own. *)

val insert : string -> t -> t
(** [insert text t] replaces the selection, or inserts at the caret. The result
    is resegmented and the caret rebased after the insertion. The length limit
    admits a fitting prefix while preserving unselected text. If the resulting
    text is unchanged, the caret, selection and history are preserved. *)

val set_cursor : ?select:bool -> int -> t -> t
(** [set_cursor ?select pos t] moves to a clamped grapheme boundary. [select],
    default [false], extends selection from its existing anchor or old caret. *)

val set_caret : ?select:bool -> Matrix.Text.Layout.caret -> t -> t
(** [set_caret ?select caret t] also sets soft-wrap affinity. *)

val select : anchor:int -> cursor:int -> t -> t
(** [select ~anchor ~cursor t] selects between clamped grapheme boundaries. *)

val clear_selection : t -> t
(** [clear_selection t] clears selection without moving the caret. *)

val select_all : t -> t
(** [select_all t] selects all text with the caret at its end. *)

(** {1 Editing}
    Deletions replace an active selection first, except line deletion which
    deletes the caret's logical line. Each text change is part of an undo step,
    as {!section-undo} describes. *)

val delete_backward : t -> t
(** [delete_backward t] deletes the preceding grapheme. *)

val delete_forward : t -> t
(** [delete_forward t] deletes the following grapheme. *)

val delete_word_backward : t -> t
(** [delete_word_backward t] deletes to the previous word boundary. *)

val delete_word_forward : t -> t
(** [delete_word_forward t] deletes to the next word boundary. *)

val delete_to_line_start : t -> t
(** [delete_to_line_start t] deletes to the logical line start. *)

val delete_to_line_end : t -> t
(** [delete_to_line_end t] deletes to the logical line end. *)

val delete_line : t -> t
(** [delete_line t] deletes the current logical line including its newline. *)

(** {1:undo Undo}

    Undo works in steps, and a step is what a person means by one change: typing
    a word, not each of its letters. An edit joins the latest step when all of
    these hold:

    - it is of the step's kind: typing after typing, where typing inserts one
      grapheme cluster; {!delete_backward} after {!delete_backward}; or
      {!delete_forward} after {!delete_forward};
    - nothing else happened since the step's last edit: no caret movement, no
      selection change, no undo or redo, no other edit. An operation that
      changes nothing returns its argument and does not count;
    - it continues the step's text at the caret: typing right after what the
      step typed, a deletion right at the edge of what it deleted. A letter
      typed before a combining mark joins the mark's cluster and the caret lands
      after the mark, so the next letter starts a new step.

    A line break, typed or deleted, starts a new step, and so does a word
    boundary while typing: a space typed after other text, or other text typed
    after two or more spaces. A word therefore undoes with the single space
    before it, and a run of spaces, such as indentation, on its own. Any other
    edit is a step of its own: an insertion of several grapheme clusters at
    once, as a paste is, a word or line deletion, or {!clear}. The first edit of
    a step may replace a selection; undoing the step restores the selection.

    The rule observes no clock: whether an edit joins depends only on what came
    before it, so the same edits make the same steps however fast they come. VS
    Code groups typing the same way; ProseMirror and CodeMirror instead join
    adjacent changes made within 500 milliseconds of each other.

    A step costs the size of its text change, not of the document, and the
    history keeps every step. *)

val undo : t -> t
(** [undo t] restores the text, caret and selection from before the latest step,
    or is [t] when there is none. *)

val redo : t -> t
(** [redo t] reapplies the most recently undone step, with the caret and
    selection that followed it, or is [t] when there is none. A text change
    after an undo discards the steps that could be redone. *)

(** {1 Movement}
    Movement clears selection by default. [select:true] extends it. *)

val move_left : ?select:bool -> t -> t
(** [move_left ?select t] moves to the preceding grapheme. *)

val move_right : ?select:bool -> t -> t
(** [move_right ?select t] moves to the following grapheme. *)

val move_word_backward : ?select:bool -> t -> t
(** [move_word_backward ?select t] moves to the previous word boundary. *)

val move_word_forward : ?select:bool -> t -> t
(** [move_word_forward ?select t] moves to the next word boundary. *)

val move_home : ?select:bool -> t -> t
(** [move_home ?select t] moves to the start of the text. *)

val move_end : ?select:bool -> t -> t
(** [move_end ?select t] moves to the end of the text. *)

val move_line_start : ?select:bool -> t -> t
(** [move_line_start ?select t] moves to the logical line start. *)

val move_line_end : ?select:bool -> t -> t
(** [move_line_end ?select t] moves to the logical line end. *)

(**/**)

(* Widget support: the edit surface derives proposals from the accepted value.

   The viewport is the display position, in columns and display rows, at which
   an edit surface shows the value. A value from [of_string] shows its start. A
   value produced by an editing or movement operation follows its caret: the
   surface moves the viewport as little as needed to show the caret. A wheel
   proposal sets a position that does not follow the caret. *)

val viewport : t -> int * int * bool
val with_viewport : x:int -> y:int -> follow:bool -> t -> t

val select_carets :
  anchor:Matrix.Text.Layout.caret -> cursor:Matrix.Text.Layout.caret -> t -> t

val preferred_col : t -> int option
val propose : base:t -> preferred_col:int option -> t -> t
