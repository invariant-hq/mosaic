(** Units of text selection.

    A selection grows by a unit: a grapheme cluster, a word or a logical line.
    The view's selection and the editors share them.

    A {e word} is, in this precedence, a run of whitespace, a single boundary
    character, or a run of anything else, never across a line terminator. The
    boundary characters are Ghostty's: the apostrophe, the quotation mark, the
    grave accent, [│], [|], [:], [;], [,], [$], and the parentheses, square
    brackets, braces and angle brackets. A cluster is of the class of its first
    code point. *)

type t =
  | Grapheme  (** A grapheme cluster. *)
  | Word  (** A word. *)
  | Line  (** A logical line, without its terminator. *)

val of_clicks : int -> t
(** [of_clicks n] is the unit the [n]th press of a series selects: [Grapheme]
    for the first, [Word] for the second, and [Line] for any later one. *)

val span :
  t -> Matrix.Text.Layout.Source.t -> first:int -> last:int -> int * int
(** [span u source ~first ~last] is the span of whole units of [u] around the
    boundaries [first] to [last] of [source], with [first <= last]:
    - for [Grapheme], [(first, last)] itself;
    - for [Word], from the start of the word of cluster [first] to the end of
      the word of cluster [last - 1]. An empty span is inside the run its two
      sides belong to, if any, and is itself otherwise;
    - for [Line], from the start of the logical line of [first] to the end,
      before its terminator, of the logical line of cluster [last - 1], or of
      [first] if the span is empty. *)
