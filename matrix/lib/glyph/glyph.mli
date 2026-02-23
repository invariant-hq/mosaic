(** Unicode glyphs for terminal rendering.

    A {e glyph} is a packed, unboxed integer representing a visual character in
    a terminal cell. Glyphs come in two kinds:

    - {e Simple} glyphs store a single Unicode scalar (U+0000 – U+10FFFF)
      directly. Zero allocation, zero lookup.
    - {e Complex} glyphs reference a multi-codepoint grapheme cluster interned
      in a {!Pool}. They carry a pool index, a generation counter, and extent
      information.

    Multi-column characters (wide CJK, emoji) are represented as one {e start}
    glyph followed by one or more {e continuation} glyphs that reference the
    same pool entry. Control characters and zero-width sequences map to
    {!empty}.

    {1:quick_start Quick start}

    Create a pool, encode a string, and process glyphs via callback:

    {[
      let pool = Pool.create () in
      Pool.encode pool ~width_method:`Unicode ~tab_width:2
        (fun glyph -> Printf.printf "%s " (Pool.to_string pool glyph))
        "Hello 👋 World"
    ]}

    {1:safety Memory safety}

    The {!Pool} uses manual reference counting with automatic slot recycling.
    Pool-backed glyph IDs include a {e generation counter} so that accessing a
    glyph whose slot has been recycled returns safe defaults ({!empty}, zero
    width) rather than stale data. This guarantee holds across normal
    {!Pool.incref}/{!Pool.decref} cycles. {!Pool.clear} resets the pool and
    invalidates all previously issued IDs.

    {1:width Width calculation}

    Display width follows {{:https://www.unicode.org/reports/tr11/}UAX #11} and
    {{:https://www.unicode.org/reports/tr29/}UAX #29}, correctly handling ZWJ
    emoji sequences, regional indicator (flag) pairs, variation selectors, and
    skin-tone modifiers. See {!type-width_method} for the available strategies.
*)

(** {1:types Types} *)

type t = private int
(** The type for glyphs. A packed 63-bit integer, always unboxed.

    The type is [private] to prevent construction of invalid values. Use
    {!of_uchar}, {!Pool.intern}, {!Pool.encode}, {!empty}, or {!space} to create
    glyphs. The integer representation is readable (e.g. for storage in
    [Bigarray]); use {!unsafe_of_int} when loading from external storage.

    {b Note.} The bit layout is not a stable serialization format across major
    versions. *)

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]
(** The type for width calculation methods. Determines how grapheme cluster
    display widths are computed:
    - [`Unicode] — full UAX #29 segmentation with ZWJ emoji composition. Use for
      correct emoji and flag rendering.
    - [`Wcwidth] — grapheme boundary segmentation for rendering, but each
      grapheme's width is the sum of per-codepoint wcwidth-style widths. Use for
      legacy compatibility.
    - [`No_zwj] — UAX #29 segmentation that forces a break after ZWJ (no emoji
      ZWJ sequences), but keeps the full grapheme-aware width logic (RI pairs,
      VS16, Indic virama). *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminator kinds.
    - [`LF] — line feed (U+000A).
    - [`CR] — carriage return (U+000D).
    - [`CRLF] — the two-byte CR LF sequence. *)

(** {1:constants Constants} *)

val empty : t
(** [empty] is the empty glyph ([0]). It represents control characters,
    zero-width sequences, and U+0000. This is the only glyph for which
    {!is_empty} is [true]. *)

val space : t
(** [space] is the space glyph (U+0020, width 1). It is the default blank-cell
    content in terminal grids. *)

(** {1:creating Creating} *)

val of_uchar : Uchar.t -> t
(** [of_uchar u] is a glyph for the single Unicode scalar [u].

    The result is {!empty} for control or zero-width codepoints. Simple glyphs
    are stored directly in the packed integer with no pool allocation.

    See also {!Pool.intern} and {!Pool.encode}. *)

(** {1:predicates Predicates} *)

val is_empty : t -> bool
(** [is_empty g] is [true] iff [g] is {!empty}. *)

val is_inline : t -> bool
(** [is_inline g] is [true] iff [g] requires no pool lookup. Useful for skipping
    reference counting on simple glyphs. *)

val is_start : t -> bool
(** [is_start g] is [true] iff [g] is the start of a character (simple or
    complex start). *)

val is_continuation : t -> bool
(** [is_continuation g] is [true] iff [g] is a wide-character continuation
    placeholder. See {!make_continuation}. *)

val is_complex : t -> bool
(** [is_complex g] is [true] iff [g] is pool-backed (complex start or complex
    continuation). *)

(** {1:properties Properties} *)

val grapheme_width : ?tab_width:int -> t -> int
(** [grapheme_width g] is the full display width of the grapheme represented by
    [g]. For complex glyphs (start or continuation) the result is the total
    cluster width (1–4). For tab glyphs the result is [tab_width].

    [tab_width] defaults to [2].

    See also {!cell_width}. *)

val cell_width : t -> int
(** [cell_width g] is the display width that [g] occupies in a single cell. The
    result is [0] for {!empty} and continuation cells. For start cells, the
    result is the character's display width (1 for most characters, 2 for wide
    CJK/emoji). Tab glyphs return [1].

    Unlike {!grapheme_width}, continuation cells return [0] because they occupy
    no additional columns beyond the start cell. *)

val left_extent : t -> int
(** [left_extent g] is the distance from a continuation cell to its start cell.
    The result is [0] for simple and complex-start glyphs. *)

val right_extent : t -> int
(** [right_extent g] is the distance from a glyph to the rightmost continuation
    cell. For a complex start glyph this is [width - 1]. *)

val codepoint : t -> int
(** [codepoint g] is the Unicode codepoint of a simple glyph [g] (U+0000 –
    U+10FFFF).

    {b Warning.} The result is undefined for complex glyphs. *)

val pool_key : t -> int option
(** [pool_key g] is [Some key] if [g] is a pool-backed glyph (complex start or
    continuation), and [None] otherwise. The key is a stable, process-local
    identity for deduplicating interned grapheme references.

    The key is only meaningful for glyphs originating from the same pool. *)

(** {1:construction Construction} *)

val make_continuation : code:t -> left:int -> right:int -> t
(** [make_continuation ~code ~left ~right] is a continuation cell referencing
    the same pool entry as [code] with the given left and right extents. [left]
    and [right] are clamped to \[0;3\]. If [code] is a simple glyph the
    continuation carries no pool reference.

    {b Note.} Intended for renderer and grid internals that materialize
    wide-cell spans. *)

(** {1:converting Converting} *)

val to_int : t -> int
(** [to_int g] is the raw integer representation of [g].

    {b Note.} The integer layout is not a stable serialization format across
    major versions. Use for in-process storage only (e.g. [Bigarray]).

    See also {!unsafe_of_int}. *)

val unsafe_of_int : int -> t
(** [unsafe_of_int n] is [n] interpreted as a glyph without validation.

    {b Warning.} The caller must ensure [n] was produced by {!to_int} or read
    from trusted storage. An invalid integer causes undefined behaviour in pool
    operations.

    See also {!to_int}. *)

(** {1:pool Pool}

    A {!Pool.t} manages the storage and lifecycle of {e complex} glyphs
    (multi-codepoint grapheme clusters) through manual reference counting with
    generation-based use-after-free protection.

    {b Warning.} Pools are not thread-safe. Use one pool per thread or provide
    external synchronization. *)

module Pool : sig
  type glyph := t

  type t
  (** The type for glyph pools. *)

  (** {2:lifecycle Lifecycle} *)

  val create : unit -> t
  (** [create ()] is a new empty pool with initial capacity for 4096 glyphs. *)

  val clear : t -> unit
  (** [clear pool] resets [pool], invalidating {e all} existing glyph
      references. Does not free memory, only resets internal cursors for reuse.

      {b Warning.} Glyphs must not be used after [clear]. Because [clear] can
      recycle IDs with the same generation, behaviour is undefined for
      previously issued IDs. *)

  (** {2:refcounting Reference counting} *)

  val incref : t -> glyph -> unit
  (** [incref pool g] increments the reference count of [g] in [pool]. No-op for
      simple glyphs or stale complex glyphs whose generation does not match. *)

  val decref : t -> glyph -> unit
  (** [decref pool g] decrements the reference count of [g] in [pool]. When the
      count reaches zero the slot is recycled and its generation is incremented.
      No-op for simple glyphs or stale complex glyphs.

      See also {!incref}. *)

  (** {2:interning Interning} *)

  val intern :
    t -> ?width_method:width_method -> ?tab_width:int -> string -> glyph
  (** [intern pool str] is a glyph for the contents of [str].

      The result is {!empty} for control characters or zero-width sequences.
      [width_method] defaults to [`Unicode]. [tab_width] defaults to [2].

      {b Note.} The entire string is stored as a single glyph with cumulative
      width. For example, [intern pool "ab"] produces one glyph with width 2.
      Use {!encode} when per-character segmentation is needed.

      {b Note.} Invalid UTF-8 byte sequences are replaced with U+FFFD
      (replacement character).

      Raises [Failure] if [pool] exceeds 262K interned graphemes.

      See also {!intern_sub} and {!encode}. *)

  val intern_sub :
    t ->
    width_method:width_method ->
    tab_width:int ->
    string ->
    pos:int ->
    len:int ->
    width:int ->
    glyph
  (** [intern_sub pool ~width_method ~tab_width str ~pos ~len ~width] is like
      {!intern} but operates on the substring [str.[pos] .. str.[pos + len - 1]]
      with precomputed display [width], avoiding redundant width calculation and
      [String.sub] allocation.

      Raises [Failure] if [pool] exceeds 262K interned graphemes. *)

  val encode :
    t ->
    width_method:width_method ->
    tab_width:int ->
    (glyph -> unit) ->
    string ->
    unit
  (** [encode pool ~width_method ~tab_width f str] segments [str] into glyphs
      and calls [f] for each one, in string order.

      Multi-column characters emit one start glyph followed by [width - 1]
      continuation glyphs. Control characters and zero-width sequences are
      skipped. Single codepoints become simple glyphs; multi-codepoint grapheme
      clusters are interned.

      {b Note.} Invalid UTF-8 byte sequences are replaced with U+FFFD
      (replacement character). Each invalid byte consumes exactly one byte and
      produces one replacement glyph.

      Raises [Failure] if [pool] exceeds 262K interned graphemes.

      See also {!intern}. *)

  (** {2:accessing Accessing} *)

  val length : t -> glyph -> int
  (** [length pool g] is the UTF-8 byte length of [g]. The result is [1] for
      simple glyphs (including {!empty}, encoded as U+0000), the actual byte
      length for complex glyphs, and [0] for stale complex IDs. *)

  val blit : t -> glyph -> bytes -> pos:int -> int
  (** [blit pool g buf ~pos] copies the UTF-8 bytes of [g] into [buf] starting
      at [pos] and returns the number of bytes written. {!empty} is encoded as
      U+0000 (1 byte). The result is [0] for stale complex IDs or insufficient
      buffer space. *)

  val copy : src:t -> glyph -> dst:t -> glyph
  (** [copy ~src g ~dst] transfers [g] from [src] to [dst].

      Simple glyphs are returned unchanged. Complex glyphs are interned in [dst]
      and a new glyph is returned. Stale IDs return {!empty}.

      {b Warning.} A glyph obtained from one pool must not be used with a
      different pool. Use [copy] to transfer between pools. *)

  val to_string : t -> glyph -> string
  (** [to_string pool g] is a freshly allocated string containing the UTF-8
      sequence of [g]. Simple glyphs produce a single-character string (["\000"]
      for {!empty}). Stale complex IDs produce [""]. *)
end

(** {1:string String utilities}

    Pool-free measurement and iteration on raw [string] values. These functions
    do not require a {!Pool.t}. *)

module String : sig
  (** {2:measuring Measuring} *)

  val measure : width_method:width_method -> tab_width:int -> string -> int
  (** [measure ~width_method ~tab_width s] is the total display width of [s].
      Control characters contribute [0].

      {b Note.} Invalid UTF-8 byte sequences are replaced with U+FFFD, each
      contributing width 1.

      See also {!measure_sub}. *)

  val measure_sub :
    width_method:width_method ->
    tab_width:int ->
    string ->
    pos:int ->
    len:int ->
    int
  (** [measure_sub ~width_method ~tab_width s ~pos ~len] is like {!measure} but
      operates on the substring [s.[pos] .. s.[pos + len - 1]] without
      allocating. The result is [0] when [len <= 0]. *)

  (** {2:counting Counting} *)

  val grapheme_count : string -> int
  (** [grapheme_count s] is the number of user-perceived characters (grapheme
      clusters) in [s]. Uses full UAX #29 segmentation. *)

  (** {2:iterating Iterating} *)

  val iter_graphemes :
    ?ignore_zwj:bool -> (offset:int -> len:int -> unit) -> string -> unit
  (** [iter_graphemes f s] calls [f ~offset ~len] for each grapheme cluster in
      [s].

      [ignore_zwj] defaults to [false]. When [true], ZWJ does not join emoji
      sequences (same boundary behaviour as [`No_zwj]).

      {b Note.} Invalid UTF-8 byte sequences are treated as individual
      replacement characters (U+FFFD).

      See also {!iter_grapheme_info}. *)

  val iter_grapheme_info :
    width_method:width_method ->
    tab_width:int ->
    (offset:int -> len:int -> width:int -> unit) ->
    string ->
    unit
  (** [iter_grapheme_info ~width_method ~tab_width f s] calls
      [f ~offset ~len ~width] for each grapheme cluster in [s]. Uses the same
      width calculation and ZWJ handling as {!Pool.encode}. Graphemes whose
      width resolves to [0] (control and zero-width sequences) are skipped.

      {b Note.} Invalid UTF-8 byte sequences are treated as individual
      replacement characters (U+FFFD).

      See also {!iter_graphemes}. *)

  val iter_wrap_breaks :
    ?width_method:width_method ->
    (break_byte_offset:int ->
    next_byte_offset:int ->
    grapheme_offset:int ->
    unit) ->
    string ->
    unit
  (** [iter_wrap_breaks f s] calls
      [f ~break_byte_offset ~next_byte_offset ~grapheme_offset] for each
      word-wrap break opportunity in [s], in order from start to end, with:
      - [break_byte_offset] — zero-based byte position of the grapheme
        containing the wrap-break character.
      - [next_byte_offset] — zero-based byte position of the next grapheme after
        the break (the resume position).
      - [grapheme_offset] — zero-based grapheme index of the grapheme containing
        the wrap-break character.

      Breaks occur after graphemes containing ASCII space, tab, hyphen, path
      separators, punctuation, brackets, and Unicode NBSP, ZWSP, soft hyphen,
      and typographic spaces.

      [width_method] controls grapheme boundary detection: [`Unicode] (the
      default) treats ZWJ sequences as single graphemes, [`No_zwj] breaks them
      apart.

      See also {!iter_line_breaks}. *)

  val iter_line_breaks :
    (pos:int -> kind:line_break_kind -> unit) -> string -> unit
  (** [iter_line_breaks f s] calls [f ~pos ~kind] for each line terminator in
      [s], in order from start to end, with:
      - [pos] — zero-based byte position. For [`CRLF] this is the position of
        the LF byte; for [`LF] and [`CR], the respective byte.
      - [kind] — the {!line_break_kind}.

      CRLF sequences are reported once as [`CRLF], not as separate [`CR] and
      [`LF] breaks.

      See also {!iter_wrap_breaks}. *)
end
