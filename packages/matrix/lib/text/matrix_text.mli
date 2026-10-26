(** Grapheme clusters, display widths, and line and word breaks of Unicode text,
    as a terminal shows it.

    A text is a string of UTF-8 bytes, and every offset into it is a byte
    offset. Each ill-formed subsequence reads as one U+FFFD (Unicode §3.9,
    maximal subparts), and the bytes stay as they are.

    A {e grapheme cluster} is what a reader takes as one character, such as a
    letter with its accents or an emoji sequence, as the extended grapheme
    cluster boundaries of {{:https://www.unicode.org/reports/tr29/}UAX #29}
    delimit it. A terminal places text in cells. A {e display unit} is one or
    more scalars that it places as one, in a number of cells, the unit's
    {e width}. A {e width method} ({!type-width_method}) says which scalars make
    a unit and how wide it is, as one kind of terminal does. A control character
    is a unit of its own, of width [0], except TAB, whose width is the
    {e tab width}.

    The module therefore has two kinds of boundaries. {!iter_graphemes} and
    {!grapheme_count} follow grapheme clusters whatever the width method, for
    editing text. {!measure}, {!iter_units}, {!find_pos} and the others of the
    section {!section-measuring} follow display units, for placing it. A
    {!Segmenter} finds display units in a stream that arrives in pieces, and
    {!Layout} cuts a text into rows of a given width.

    Every function that takes a [tab_width] raises [Invalid_argument] if it is
    below [1].

    This measures a text with two wide characters and a TAB:
    {[
    assert (Matrix_text.measure ~width_method:`Unicode ~tab_width:4 "日本\t" = 8)
    ]} *)

(** {1:units Display units}

    A terminal receives its output in pieces of any size and must cut it into
    the same display units as if it had arrived whole. The grapheme cluster
    boundary rules of UAX #29 look only backwards, so a boundary is known when
    the scalar after it arrives.

    The units are extended grapheme clusters for [`Unicode] and [`Unicode_wide].
    For [`Wcwidth] they are what a terminal that measures each character with
    [wcwidth] places, a scalar of positive width and the scalars of width [0]
    that follow it. In every method a control character (General_Category Cc,
    which is C0, DEL and C1, TAB included) is a unit of its own, since a
    terminal performs it. CR LF is therefore two units, where UAX #29 makes one
    cluster of it (GB3).

    The width of a unit never decreases as scalars are added to it. A terminal
    places a unit before it knows that the unit is complete, and widens it in
    place as it grows. It could not take back a character that already wrapped
    the line, so a later scalar never narrows a unit. VS15 (U+FE0E), which asks
    for the text presentation of an emoji
    ({{:https://www.unicode.org/reports/tr51/}UTS #51} §4), leaves an emoji that
    is already two cells wide at two cells. *)

type width_method = [ `Unicode | `Unicode_wide | `Wcwidth ]
(** The type for width methods.

    - [`Unicode] makes each extended grapheme cluster one unit, as wide as its
      first scalar of positive width, as a terminal does in grapheme cluster
      mode (mode 2027 of "Unicode in Terminals", and kitty's text-sizing
      protocol). What continues a cluster, whether a mark, a joined emoji or a
      conjoined consonant, takes no cells of its own, so the conjunct क्ष (U+0915
      U+094D U+0937) takes the one cell of क. A spacing mark (General_Category
      Mc) has width [0], as kitty's protocol gives every mark, so a cluster that
      one starts, as U+093F does alone, takes no cells, unless the mark is wide
      (U+302E). A cluster is two cells wide when VS16 follows an emoji or an
      emoji modifier follows a modifier base, as in ☝🏽 (U+261D U+1F3FD), both of
      which have the emoji presentation (UTS #51 §2.7.1, §4), when it is a pair
      of regional indicators, and when it holds Thai SARA AM (U+0E33) or Lao AM
      (U+0EB3), which kitty's protocol widens.
    - [`Unicode_wide] makes each extended grapheme cluster one unit, as
      [`Unicode] does, and a cluster that holds more than one scalar of positive
      width at least two cells wide, as Ghostty does.
    - [`Wcwidth] gives each scalar its [wcwidth] width, and makes one unit of
      each scalar of positive width and the scalars of width [0] after it. *)

module Segmenter : sig
  (** The display units of a stream that arrives in pieces.

      A segmenter carries what cutting a stream into units takes from one scalar
      to the next, the state of the boundary rules and the width of the unit in
      progress, so that the units do not depend on where the stream was cut. *)

  type t
  (** The type for the segmentation of one stream. A segmenter is mutable and
      must not be used by two domains in parallel. *)

  val create : width_method -> t
  (** [create m] is a segmenter for the units of method [m], before the first
      scalar. *)

  val reset : t -> unit
  (** [reset s] ends the unit in progress, so the next scalar starts a unit, as
      at the start of a text (UAX #29 GB1). *)

  val add : t -> Uchar.t -> bool
  (** [add s u] adds the scalar [u] to the stream and is [true] iff [u] starts a
      unit. Afterwards [u] belongs to the unit in progress, so {!width} is that
      of the unit [u] started or continued, and a caller that needs the width of
      the unit [u] closed reads {!width} before [add]. It allocates nothing. *)

  val may_continue : width_method -> Uchar.t -> Uchar.t -> bool
  (** [may_continue m u v] is [false] when [v] starts a unit of method [m] after
      any text that ends with [u], and [true] when [v] continues the unit that
      ends with [u] in some text. It reads the two scalars only. A unit may
      continue by a rule that looks further back than [u], as two regional
      indicators do after an even number of them and not after an odd number,
      and then only {!add}, which has seen that text, tells. It allocates
      nothing. *)

  val ascii_run : t -> string -> pos:int -> len:int -> int
  (** [ascii_run s str ~pos ~len] is the number [k] of bytes from [pos] that are
      complete units of one printable ASCII character (U+0020 to U+007E) each.
      Each of them starts a unit and is followed within the slice by another
      printable ASCII byte, which therefore starts the next unit. [k] is [0]
      when no such run starts at [pos], in particular when the unit in progress
      absorbs the byte at [pos], as a Prepend character does (UAX #29 GB9b).
      Afterwards [s] is as if the [k] scalars had been added with {!add}, and
      the byte at [pos + k] is not added. A writer therefore places [k] cells at
      once and goes on with {!add}, and the units do not depend on how the
      stream was cut. It allocates nothing.

      Raises [Invalid_argument] if [pos] and [len] do not designate a substring
      of [str]. *)

  val width : t -> int
  (** [width s] is the width of the unit in progress, and [0] for a control,
      before the first scalar, and while the unit has nothing to show. It never
      decreases until the next unit starts. *)
end

(** {1:properties Characters} *)

val uchar_width : tab_width:int -> Uchar.t -> int
(** [uchar_width ~tab_width u] is the width of [u] alone under [`Wcwidth]:
    [tab_width] for TAB, [0] for the other controls and for combining
    characters, and [1] or [2] otherwise. The other methods give a scalar alone
    the same width, except that [`Unicode] gives a spacing mark [0] unless it is
    wide. Measuring a sequence of scalars takes {!measure}. *)

type grapheme_cluster_break =
  [ `CN
  | `CR
  | `EX
  | `L
  | `LF
  | `LV
  | `LVT
  | `PP
  | `RI
  | `SM
  | `T
  | `V
  | `XX
  | `ZWJ ]
(** The type for the values of the Grapheme_Cluster_Break property, which the
    boundary rules of UAX #29 read (Table 2). The tags are the property's short
    value aliases, as in uucp's [Uucp.Break.grapheme_cluster], into which a
    value coerces:
    - [`CR] and [`LF], CARRIAGE RETURN and LINE FEED, which make one cluster of
      CR LF (GB3) and none with anything else.
    - [`CN], Control, the other controls, the line and paragraph separators, and
      most format characters. A cluster boundary falls on both sides (GB4, GB5).
    - [`EX], Extend, the characters that continue the cluster before them (GB9),
      which are nonspacing and enclosing marks, emoji modifiers, ZWNJ and
      variation selectors.
    - [`ZWJ], ZERO WIDTH JOINER, which also continues the cluster before it and
      joins an emoji to the pictograph after it (GB11).
    - [`SM], SpacingMark, spacing combining marks that continue the cluster
      before them (GB9a), as U+0903 DEVANAGARI SIGN VISARGA does.
    - [`PP], Prepend, characters that the character after them joins (GB9b), as
      the digits it precedes join U+0600 ARABIC NUMBER SIGN.
    - [`RI], Regional_Indicator, the letters U+1F1E6..U+1F1FF, which pair into
      flags (GB12, GB13).
    - [`L], [`V], [`T], [`LV] and [`LVT], Hangul leading consonants, vowels,
      trailing consonants and precomposed syllables, which conjoin into one
      syllable (GB6 to GB8).
    - [`XX], Other, everything else, letters and pictographs among them.

    UAX #29 keeps four more values, E_Base, E_Modifier, Glue_After_Zwj and
    E_Base_GAZ, as "obsolete and unused". No character has had them since
    Unicode 11.0, so the type leaves them out. *)

val grapheme_cluster_break : Uchar.t -> grapheme_cluster_break
(** [grapheme_cluster_break u] is the Grapheme_Cluster_Break property of [u], by
    the Unicode version of the tables {!Segmenter} follows. It describes [u]
    alone. Whether a boundary falls between two characters also depends on rules
    that look further back, which only a {!Segmenter} applies. It allocates
    nothing. *)

(** {1:measuring Measuring}

    These functions read a text's display units, a TAB [tab_width] cells wide.
*)

type position = { byte_offset : int; units : int; columns_used : int }
(** The type for positions found by width. [byte_offset] is the position,
    [units] the number of units of positive width before it, and [columns_used]
    their width. *)

val measure : width_method:width_method -> tab_width:int -> string -> int
(** [measure ~width_method ~tab_width s] is the sum of the widths of the units
    of [s]. *)

val measure_sub :
  width_method:width_method ->
  tab_width:int ->
  string ->
  pos:int ->
  len:int ->
  int
(** [measure_sub ~width_method ~tab_width s ~pos ~len] is {!measure} of the
    [len] bytes of [s] from [pos], without copying them, and [0] if [len <= 0].
    A scalar cut by either end of the slice is ill-formed, as in the substring.

    Raises [Invalid_argument] if [len > 0] and [pos] and [len] do not designate
    a substring of [s]. *)

val width_at :
  width_method:width_method -> tab_width:int -> string -> byte_offset:int -> int
(** [width_at ~width_method ~tab_width s ~byte_offset] is the width of the first
    unit of the text of [s] from [byte_offset], and [0] if [byte_offset] is
    outside [s]. *)

val iter_units :
  width_method:width_method ->
  tab_width:int ->
  (offset:int -> len:int -> width:int -> unit) ->
  string ->
  unit
(** [iter_units ~width_method ~tab_width f s] calls [f ~offset ~len ~width] on
    each unit of [s] of positive width, in order, where the unit is the [len]
    bytes from [offset]. *)

val find_pos :
  width_method:width_method ->
  tab_width:int ->
  ?include_start_before:bool ->
  string ->
  columns:int ->
  position
(** [find_pos ~width_method ~tab_width s ~columns] is the end of the longest
    prefix of [s] whose units of positive width each end at or before column
    [columns]. With [~include_start_before:true], a unit is in the prefix if it
    starts before column [columns], so a wide unit that straddles [columns] is
    in it. The units of width [0] before the first unit left out are in the
    prefix. [include_start_before] defaults to [false]. *)

val find_wrap_pos :
  width_method:width_method ->
  tab_width:int ->
  string ->
  max_columns:int ->
  position
(** [find_wrap_pos ~width_method ~tab_width s ~max_columns] is
    {!find_pos}[ ~width_method ~tab_width s ~columns:max_columns], the longest
    prefix of [s] at most [max_columns] wide. It ends at the start of the first
    unit that does not fit. *)

(** {1:graphemes Grapheme clusters} *)

val grapheme_count : string -> int
(** [grapheme_count s] is the number of grapheme clusters in [s]. *)

val iter_graphemes : (offset:int -> len:int -> unit) -> string -> unit
(** [iter_graphemes f s] calls [f ~offset ~len] on each grapheme cluster of [s],
    in order, where the cluster is the [len] bytes from [offset]. *)

(** {1:breaks Breaks}

    A {e break opportunity} is a place where a line may wrap. One follows each
    grapheme cluster that holds a space, HT, [-], [/], a backslash, [,], [;],
    [:], [!], [?], a bracket, a brace or a parenthesis, or one of U+1680,
    U+2000..U+2006, U+2008..U+200A, U+200B, U+205F, U+3000, U+00AD, U+2010,
    U+3001, U+3002, U+FF01 and U+FF1F. The no-break spaces U+00A0, U+2007 and
    U+202F, of class GL (glue) in UAX #14, make none. One also falls between a
    cluster that starts with an ASCII letter, digit or [_] and one that starts
    with a CJK character (Han, kana or Hangul), in either order.

    An opportunity is reported as [~break_byte_offset], the offset of the
    cluster before it, [~next_byte_offset], the offset of the cluster after it,
    where scanning resumes, and [~grapheme_offset], the index of the cluster
    before it. *)

val iter_wrap_breaks :
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_wrap_breaks f s] calls [f] on each break opportunity of [s], in order,
    and also between two adjacent CJK clusters. It makes none of the latter
    before a kana voicing mark (U+3099, U+309A), U+30A0 or U+30FB, or after a
    cluster of width [0]. *)

val iter_word_breaks :
  (break_byte_offset:int -> next_byte_offset:int -> grapheme_offset:int -> unit) ->
  string ->
  unit
(** [iter_word_breaks f s] calls [f] on each break opportunity of [s], in order.
    A run of CJK clusters stays one word, which suits moving the cursor by words
    and deleting them. *)

type line_break_kind = [ `LF | `CR | `CRLF ]
(** The type for line terminators. *)

val iter_line_breaks :
  (pos:int -> kind:line_break_kind -> unit) -> string -> unit
(** [iter_line_breaks f s] calls [f ~pos ~kind] on each line terminator of [s],
    in order, where [pos] is the offset of its last byte, the LF of a CR LF. *)

(** {1:layout Layout} *)

module Layout : sig
  (** Texts cut into rows of a given width, as a text view or an editor shows
      them.

      A {e source} ({!Source}) is a text indexed by grapheme cluster. Its
      clusters are numbered from [0], and {e boundary} [g] is the boundary
      before cluster [g], from [0] to the number of clusters. Every position in
      a layout is a boundary. A {e logical line} is a run of clusters ended by a
      line terminator (LF, CR or CR LF) or by the end of the text.

      A {e layout} cuts each logical line of a source into {e rows} of a given
      width, by one of the {!type-wrap} modes. A row after the first of its line
      is a {e continuation}. A cluster wider than the width has a row of its
      own. The {e blanks} (spaces and TABs) at the end of a row that wraps
      {e hang} there. They belong to the row and are shown in no cell, and they
      take none of its width when the row is filled, so a blank never starts a
      continuation and never makes a row wrap early. At the end of a line the
      blanks hang only where they do not fit. CSS Text 3 hangs the white space
      at the end of a [pre-wrap] line in the same way, "unless the sequence is
      followed by a forced line break, in which case it must conditionally hang
      the sequence instead" (§4.1.2). Without wrapping, a layout may
      {e truncate} a line wider than the width, which then shows the prefix that
      fits in one cell less, and an {e ellipsis}, a cell that stands for the
      clusters left out, its {e omitted suffix}. The program draws the ellipsis.
      At a width of [0] or less the ellipsis does not fit either, and a
      truncated line shows only the clusters of width [0] that begin it.

      A {e caret} is a boundary with an {e affinity}. A boundary where a line
      wraps is both the end of a row and the start of the next, and one inside
      an omitted suffix is on both sides of its ellipsis. [Upstream] places the
      caret on the earlier side and [Downstream] on the later.

      Unless a function's doc says otherwise, a row, line or boundary out of
      range is clamped to the nearest one, the width method defaults to
      [`Unicode], and the tab width to [2]. The functions that map a cell to
      boundaries ({!hit} and {!coverage}) take a row before the first as the
      start of the source and a row after the last as its end, so that a pointer
      dragged above or below the text reaches its first or last boundary. A TAB
      is as wide as the tab width wherever it is. Each cluster is as wide as
      {!measure} makes it, whatever the width method, so a layout follows
      grapheme clusters even under [`Wcwidth].

      The life cycle of a layout:
      + {!Source.of_string} indexes a text once, and {!Source.edit} indexes
        again only the lines an edit touches.
      + {!create} lays it out at a width, and again when the width changes.
      + {!iter_rows} and {!iter_cells} draw the rows that show, and {!locate},
        {!hit} and {!coverage} map carets and cells to each other. *)

  (** The type for affinities. *)
  type affinity =
    | Upstream  (** The earlier side of a boundary. *)
    | Downstream  (** The later side of a boundary. *)

  type caret = { grapheme : int; affinity : affinity }
  (** The type for carets. [grapheme] is a boundary. *)

  type wrap = [ `None | `Char | `Word ]
  (** The type for wrap modes. [`None] keeps one row per logical line. [`Char]
      wraps between clusters. [`Word] wraps at the last break opportunity
      ({!iter_wrap_breaks}) that fits, and between clusters in a row that has
      none. *)

  module Source : sig
    (** Texts indexed by grapheme cluster and logical line.

        A source keeps its text's bytes as they are, and indexes every cluster,
        those of width [0] included. CR LF is one cluster of two bytes. A source
        indexes each logical line, and a source made by {!edit} shares the index
        of every line the edit leaves whole with the source it was made from. A
        line that holds more than printable ASCII has its clusters indexed when
        something first asks about it, with their widths by the width method and
        tab width it was last laid out with. A source also keeps its layouts
        ({!Layout.create}). Sources made from one another share what they cache,
        so they must not be used by two domains in parallel. *)

    type t
    (** The type for sources. *)

    val of_string : string -> t
    (** [of_string s] is the source of [s]. *)

    val text : t -> string
    (** [text t] is the text of [t]. *)

    val length : t -> int
    (** [length t] is the number of clusters of [t]. *)

    val byte_offset : t -> int -> int
    (** [byte_offset t g] is the byte offset of boundary [g]. *)

    val grapheme_of_byte : t -> affinity:affinity -> int -> int
    (** [grapheme_of_byte t ~affinity b] is the boundary at byte offset [b],
        clamped to the text. An offset inside a cluster gives the boundary
        before it if [affinity] is [Upstream], and after it if [Downstream]. *)

    val line_count : t -> int
    (** [line_count t] is the number of logical lines of [t], empty ones and an
        empty last line included. It is at least [1]. *)

    val line_start : t -> int -> int
    (** [line_start t line] is the first boundary of [line]. *)

    val line_end : t -> int -> int
    (** [line_end t line] is the boundary before the line terminator of [line],
        or the end of [t] for the last line. *)

    val line_of_offset : t -> int -> int
    (** [line_of_offset t g] is the logical line of boundary [g]. *)

    val columns :
      ?width_method:width_method ->
      ?tab_width:int ->
      t ->
      first:int ->
      last:int ->
      int
    (** [columns t ~first ~last] is the width of the clusters from boundary
        [first] to boundary [last], negative if [last] is before [first]. *)

    val slice : t -> first:int -> last:int -> string
    (** [slice t ~first ~last] is a copy of the bytes from boundary [first] to
        boundary [last], and [""] if [last] is not after [first]. *)

    val edit : t -> first:int -> last:int -> string -> t
    (** [edit t ~first ~last s] is the source of [t]'s text with the bytes from
        [first] to [last] replaced by [s]. It copies the text, indexes again the
        lines the replacement touches, and shares the index of the others with
        [t], which is unchanged.

        Raises [Invalid_argument] unless
        [0 <= first <= last <= String.length (text t)]. *)
  end

  type t
  (** The type for layouts. A layout is immutable and holds its source. Its rows
      count from [0], and a row's columns from [0], before any scrolling or
      clipping. *)

  val create :
    ?width_method:width_method ->
    ?tab_width:int ->
    ?truncate:bool ->
    Source.t ->
    width:int ->
    wrap:wrap ->
    t
  (** [create source ~width ~wrap] is the layout of [source] in rows of [width]
      cells, wrapped by [wrap]. A [width] of [0] or less keeps one row per
      logical line. With [~truncate:true] and [`None] a line wider than [width]
      is truncated. [truncate] defaults to [false].

      [source] keeps the layouts of its last four calls that lay it out
      differently, and [create] returns the one it keeps when its arguments lay
      it out the same way. A layout makes the rows of a line when they are first
      asked, and the layouts a source keeps share 32 MiB of rows: the one
      [create] returns is kept whatever its rows, and the others while their
      rows fit what it leaves. When [source] was made by {!Source.edit} from a
      source that keeps a layout for the same arguments, [create] lays out only
      the lines the edit touched. *)

  val measure :
    ?width_method:width_method ->
    ?tab_width:int ->
    ?truncate:bool ->
    Source.t ->
    width:int ->
    wrap:wrap ->
    int * int
  (** [measure source ~width ~wrap] is [(row_count l, max_width l)] for the
      layout [l] that {!create} returns, which makes no rows. *)

  val source : t -> Source.t
  (** [source t] is the source of [t]. *)

  val row_count : t -> int
  (** [row_count t] is the number of rows of [t], at least [1]. *)

  val max_width : t -> int
  (** [max_width t] is the width of the widest row of [t]. *)

  val row_start : t -> int -> int
  (** [row_start t row] is the first boundary of [row]. *)

  val row_end : t -> int -> int
  (** [row_end t row] is the boundary that ends [row], before its line
      terminator and after its omitted suffix or the blanks it hangs. *)

  val row_width : t -> int -> int
  (** [row_width t row] is the width of [row], its ellipsis included. *)

  val logical_line : t -> int -> int
  (** [logical_line t row] is the logical line of [row]. *)

  val continuation : t -> int -> int
  (** [continuation t row] is [0] for the first row of a logical line and [n]
      for its [n]th continuation. *)

  val iter_rows :
    t ->
    first:int ->
    count:int ->
    (row:int -> logical_line:int -> continuation:int -> unit) ->
    unit
  (** [iter_rows t ~first ~count f] calls [f ~row ~logical_line ~continuation]
      on the rows from [first] to [first + count - 1] that [t] has, in order,
      and on none if [count] is negative. *)

  val locate : t -> caret -> int * int
  (** [locate t c] is the row and column of the caret [c]. At a wrap, [Upstream]
      is the end of the earlier row and [Downstream] the start of the later. A
      boundary inside an omitted suffix is before the ellipsis if [Upstream] and
      after it if [Downstream], and the end of the suffix is after it. *)

  val hit : ?affinity:affinity -> t -> row:int -> column:int -> caret
  (** [hit t ~row ~column] is the caret nearest to [column] in [row], the later
      of two equally near. Where boundaries share a column, as around a cluster
      of width [0], it is the first if [affinity] is [Upstream] and the last if
      [Downstream], which is the default. A caret at the end of a row that wraps
      is [Upstream] and one at the start of a row [Downstream], so that it
      locates on [row]. In a truncated row, the ellipsis gives the boundary
      before it, and a column after the row its end. A row out of range gives
      the start or the end of the source ({!Layout}). Several cells hit one
      caret, so {!locate} of a hit may be another cell. *)

  val coverage : t -> row:int -> column:int -> int * int
  (** [coverage t ~row ~column] is [(first, last)], the boundaries around the
      clusters the cell at [row] and [column] shows. These are the cluster for
      each cell of a wide cluster, and the omitted suffix for the ellipsis. The
      range is empty for a cell that shows nothing, and for a row out of range,
      at the start or the end of the source ({!Layout}). The program decides
      whether a gesture on the cell selects it. *)

  val iter_cells :
    t ->
    row:int ->
    first_column:int ->
    last_column:int ->
    (first:int -> last:int -> column:int -> width:int -> ellipsis:bool -> unit) ->
    unit
  (** [iter_cells t ~row ~first_column ~last_column f] calls
      [f ~first ~last ~column ~width ~ellipsis] on each cluster of positive
      width of [row] with a cell in columns [first_column] to [last_column - 1],
      in order. [first] and [last] are the boundaries around the cluster, and
      [column] and [width] its first column and width. The ellipsis comes last,
      as its omitted suffix with [~width:1] and [~ellipsis:true]. A row out of
      range visits nothing. *)
end
