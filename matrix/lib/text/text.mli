(** Terminal text measurement and segmentation.

    This module contains the Unicode text operations needed by terminal
    renderers: grapheme cluster iteration, display-width measurement, and
    lightweight break discovery. It does not own cells, styles, or interned
    storage. *)

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]
(** The type for width calculation methods.

    - [`Unicode] uses full grapheme segmentation with emoji ZWJ composition.
    - [`Wcwidth] sums per-codepoint wcwidth-style widths inside each string.
    - [`No_zwj] uses grapheme-aware width logic but disables emoji ZWJ
      composition. *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminators. *)

val measure : width_method:width_method -> tab_width:int -> string -> int
(** [measure ~width_method ~tab_width s] is the display width of [s]. Control
    characters contribute [0].

    Invalid UTF-8 byte sequences are treated as U+FFFD, each contributing width
    [1]. *)

val measure_sub :
  width_method:width_method ->
  tab_width:int ->
  string ->
  pos:int ->
  len:int ->
  int
(** [measure_sub ~width_method ~tab_width s ~pos ~len] is like {!measure} but
    operates on [s.[pos] .. s.[pos + len - 1]] without allocating. The result is
    [0] when [len <= 0].

    Raises [Invalid_argument] if [len > 0] and [pos] and [len] do not designate
    a valid substring of [s]. *)

val grapheme_count : string -> int
(** [grapheme_count s] is the number of grapheme clusters in [s]. *)

val iter_graphemes :
  ?ignore_zwj:bool -> (offset:int -> len:int -> unit) -> string -> unit
(** [iter_graphemes f s] calls [f ~offset ~len] for each grapheme cluster in
    [s].

    [ignore_zwj] defaults to [false]. When [true], ZWJ does not join emoji
    sequences. Invalid UTF-8 byte sequences are treated as individual
    replacement characters. *)

val iter_grapheme_info :
  width_method:width_method ->
  tab_width:int ->
  (offset:int -> len:int -> width:int -> unit) ->
  string ->
  unit
(** [iter_grapheme_info ~width_method ~tab_width f s] calls
    [f ~offset ~len ~width] for each non-zero-width grapheme cluster in [s]. *)

val iter_wrap_breaks :
  ?width_method:width_method ->
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_wrap_breaks f s] calls
    [f ~break_byte_offset ~next_byte_offset ~grapheme_offset] for each word-wrap
    break opportunity in [s], in order.

    [break_byte_offset] is the byte position of the grapheme containing the
    break character. [next_byte_offset] is the byte position where scanning
    resumes. [grapheme_offset] is the zero-based grapheme index of the break
    grapheme. *)

val iter_line_breaks :
  (pos:int -> kind:line_break_kind -> unit) -> string -> unit
(** [iter_line_breaks f s] calls [f ~pos ~kind] for each line terminator in [s],
    in order. For [`CRLF], [pos] is the position of the LF byte. *)
