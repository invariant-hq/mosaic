(** Terminal text measurement and segmentation.

    This module contains the Unicode text operations needed by terminal
    renderers: grapheme cluster iteration, display-width measurement, and
    lightweight break discovery. It does not own cells, styles, or interned
    storage. *)

type width_method = [ `Unicode | `Wcwidth ]
(** The type for width calculation methods.

    - [`Unicode] uses full grapheme segmentation with emoji ZWJ composition.
    - [`Wcwidth] sums per-codepoint wcwidth-style widths inside each string. *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminators. *)

type position = { byte_offset : int; grapheme_count : int; columns_used : int }
(** The type for a byte position found by display width.

    [grapheme_count] is a cluster count for [`Unicode] and a codepoint count
    for [`Wcwidth]. *)

type grapheme = { byte_offset : int; byte_length : int; width : int }
(** The type for a grapheme or codepoint span.

    Values returned in [`Wcwidth] mode describe one codepoint. *)

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

val iter_graphemes : (offset:int -> len:int -> unit) -> string -> unit
(** [iter_graphemes f s] calls [f ~offset ~len] for each grapheme cluster in
    [s]. Invalid UTF-8 byte sequences are treated as individual replacement
    characters. *)

val iter_grapheme_info :
  width_method:width_method ->
  tab_width:int ->
  (offset:int -> len:int -> width:int -> unit) ->
  string ->
  unit
(** [iter_grapheme_info ~width_method ~tab_width f s] calls
    [f ~offset ~len ~width] for each non-zero-width grapheme cluster in [s]. *)

val iter_wrap_breaks :
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

val find_wrap_pos :
  width_method:width_method ->
  tab_width:int ->
  string ->
  max_columns:int ->
  position
(** [find_wrap_pos ~width_method ~tab_width s ~max_columns] is the longest
    prefix of [s] whose display width is at most [max_columns].

    If the next grapheme would exceed [max_columns], the result points to the
    start of that grapheme. In [`Wcwidth] mode, movement is by codepoint rather
    than by grapheme cluster. *)

val find_pos :
  width_method:width_method ->
  tab_width:int ->
  ?include_start_before:bool ->
  string ->
  columns:int ->
  position
(** [find_pos ~width_method ~tab_width s ~columns] is the byte position in [s]
    corresponding to display column [columns].

    By default, a wide grapheme is included only if its end column is at or
    before [columns]. If [include_start_before] is [true], a wide grapheme is
    included if its start column is before [columns]. *)

val width_at :
  width_method:width_method -> tab_width:int -> string -> byte_offset:int -> int
(** [width_at ~width_method ~tab_width s ~byte_offset] is the display width of
    the grapheme starting at [byte_offset]. Returns [0] if [byte_offset] is out
    of bounds.

    In [`Wcwidth] mode, the result is the width of the codepoint starting at
    [byte_offset]. *)

val prev_grapheme :
  width_method:width_method ->
  tab_width:int ->
  string ->
  byte_offset:int ->
  grapheme option
(** [prev_grapheme ~width_method ~tab_width s ~byte_offset] is the grapheme
    immediately before [byte_offset], if any.

    If [byte_offset] falls inside a grapheme, that containing grapheme is
    returned. In [`Wcwidth] mode, the result is the previous non-zero-width
    codepoint. *)
