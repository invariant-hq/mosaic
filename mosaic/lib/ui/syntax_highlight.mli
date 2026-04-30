(** Source highlight ranges.

    [Syntax_highlight] is the stable bridge between syntax highlighters and code
    renderables. It describes styled byte ranges over a UTF-8 source string
    without depending on a concrete highlighter such as Tree-sitter.

    Ranges can carry metadata used by source renderables for behaviours such as
    concealment and injection-container suppression. *)

type meta = {
  is_injection : bool;
      (** [true] when the range belongs to injected source code. *)
  contains_injection : bool;
      (** [true] when the range contains injected source code. *)
  conceal : string option;
      (** Replacement text when concealment is enabled. [Some ""] hides the
          range. *)
  conceal_lines : bool;
      (** [true] when the line break following the range should be hidden when
          concealment is enabled. *)
}
(** Metadata attached to a highlight range. *)

val default_meta : meta
(** [default_meta] has all flags disabled and no conceal replacement. *)

type range
(** A highlighted source byte range. *)

type t = range list
(** A list of highlighted source byte ranges. *)

val range :
  ?meta:meta -> start_byte:int -> end_byte:int -> scope:string -> unit -> range
(** [range ~start_byte ~end_byte ~scope ()] highlights bytes from [start_byte]
    inclusive to [end_byte] exclusive with capture scope [scope].

    Raises [Invalid_argument] if [start_byte < 0] or [end_byte < start_byte]. *)

val of_triples : (int * int * string) list -> t
(** [of_triples ranges] converts legacy [(start_byte, end_byte, scope)] ranges
    to highlight ranges with {!default_meta}. *)

val start_byte : range -> int
(** [start_byte r] is the inclusive byte start of [r]. *)

val end_byte : range -> int
(** [end_byte r] is the exclusive byte end of [r]. *)

val scope : range -> string
(** [scope r] is the capture scope of [r]. *)

val meta : range -> meta
(** [meta r] is the metadata attached to [r]. *)

val to_spans :
  ?conceal:bool ->
  style:Syntax_style.t ->
  content:string ->
  t ->
  Text_buffer.span list
(** [to_spans ~style ~content ranges] converts [ranges] into styled text spans.

    Overlapping ranges are cascade-merged by scope specificity, least-specific
    first, then by range order. Text outside ranges receives the syntax style's
    base style. If [conceal] is [true] (the default), conceal metadata is
    applied.

    Raises [Invalid_argument] if any byte offset is out of bounds for [content].
*)
