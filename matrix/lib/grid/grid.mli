(** Mutable grid of terminal cells.

    A grid is a two-dimensional framebuffer where each cell stores character
    content, foreground and background colors (RGBA), text attributes, and a
    hyperlink. Backed by bigarrays for cache-friendly access.

    Single codepoints are stored directly as packed integers. Multi-codepoint
    grapheme clusters (ZWJ emoji, combining characters) are stored in grid-owned
    grapheme storage. Wide characters span multiple cells: a start cell followed
    by continuation markers.

    When {!respect_alpha} is enabled, colors with alpha < 1.0 are blended with
    existing cell colors using a perceptual curve. A stack of {e scissor}
    clipping regions constrains drawing. Wide characters are span-atomic: if the
    start cell is accepted by a clipping region, continuation cells are written
    with it to keep the row well-formed. *)

(** {1:types Types} *)

module Cell : sig
  (** Packed terminal cell content.

      [Cell] values describe the character stored in one grid cell. Simple cells
      store one Unicode scalar directly. Complex cells are opaque references to
      grid-owned storage and are meaningful only inside the grid that created
      them. *)

  type t
  (** The type for packed cell content. *)

  val empty : t
  (** [empty] is the zero-width empty cell content. *)

  val space : t
  (** [space] is U+0020. *)

  val of_uchar : Uchar.t -> t
  (** [of_uchar u] is the single-codepoint cell content for [u], or {!empty}
      when [u] has zero terminal width. *)

  val is_empty : t -> bool
  (** [is_empty c] is [true] iff [c] is {!empty}. *)

  val is_inline : t -> bool
  (** [is_inline c] is [true] iff [c] stores its Unicode scalar directly and
      needs no grid-owned storage lookup. *)

  val is_start : t -> bool
  (** [is_start c] is [true] iff [c] is a simple cell or the first cell of a
      complex grapheme span. *)

  val is_continuation : t -> bool
  (** [is_continuation c] is [true] iff [c] is a continuation cell in a wide
      grapheme span. *)

  val is_complex : t -> bool
  (** [is_complex c] is [true] iff [c] references grid-owned storage. *)

  val grapheme_width : ?tab_width:int -> t -> int
  (** [grapheme_width c] is the full terminal width represented by [c]. *)

  val cell_width : t -> int
  (** [cell_width c] is the width contribution of this one grid cell. *)

  val left_extent : t -> int
  (** [left_extent c] is the number of cells between continuation cell [c] and
      its span start. It is [0] for non-continuation cells. *)

  val right_extent : t -> int
  (** [right_extent c] is the number of continuation cells to the right of [c].
  *)

  val codepoint : t -> int
  (** [codepoint c] is the Unicode scalar value of an inline cell.

      {b Warning.} The result is unspecified for complex cells. *)
end

type t
(** The type for mutable cell grids. *)

type region = { x : int; y : int; width : int; height : int }
(** The type for rectangular areas in cell coordinates. Covers cells satisfying
    [x <= col < x + width] and [y <= row < y + height]. Non-positive [width] or
    [height] yields an empty region. *)

(** {1:constructors Constructors} *)

val create :
  width:int ->
  height:int ->
  ?width_method:Text.width_method ->
  ?respect_alpha:bool ->
  unit ->
  t
(** [create ~width ~height ()] is a grid with all cells set to spaces (white
    foreground, transparent background) with:
    - [width_method] grapheme width method. Defaults to [`Unicode].
    - [respect_alpha] alpha blending on {!set_cell}. Defaults to [false].

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

(** {1:properties Properties} *)

val width : t -> int
(** [width g] is the grid width in cells. *)

val height : t -> int
(** [height g] is the grid height in cells. *)

val width_method : t -> Text.width_method
(** [width_method g] is the current width computation method. *)

val set_width_method : t -> Text.width_method -> unit
(** [set_width_method g m] changes the width method for subsequent {!draw_text}
    calls. Existing cell widths are not updated. *)

val respect_alpha : t -> bool
(** [respect_alpha g] is [true] iff alpha blending is enabled for {!set_cell}
    and when [g] is used as the source of {!blit_region}. *)

val set_respect_alpha : t -> bool -> unit
(** [set_respect_alpha g b] sets the alpha blending mode. *)

val active_height : t -> int
(** [active_height g] is the number of rows from the top containing non-blank
    content (character code other than 0 or space). Returns [0] for an empty or
    cleared grid. *)

(** {1:cell_access Cell access}

    Linear index [idx] is row-major: [idx = y * width + x]. *)

val idx : t -> x:int -> y:int -> int
(** [idx g ~x ~y] is the flat index for cell [(x, y)]. No bounds checking. *)

val get_cell : t -> int -> Cell.t
(** [get_cell g idx] is the character content at [idx]. *)

val get_attrs : t -> int -> int
(** [get_attrs g idx] is the packed attribute integer at [idx]. *)

val get_link : t -> int -> int32
(** [get_link g idx] is the internal hyperlink ID at [idx]. *)

val get_fg_r : t -> int -> float
val get_fg_g : t -> int -> float
val get_fg_b : t -> int -> float

val get_fg_a : t -> int -> float
(** Foreground RGBA components at [idx], in \[0.0, 1.0\]. *)

val get_bg_r : t -> int -> float
val get_bg_g : t -> int -> float
val get_bg_b : t -> int -> float

val get_bg_a : t -> int -> float
(** Background RGBA components at [idx], in \[0.0, 1.0\]. *)

val get_text : t -> int -> string
(** [get_text g idx] is the grapheme at [idx] as a string. Returns [""] for
    empty or continuation cells. *)

val cell_text_length : t -> int -> int
(** [cell_text_length g idx] is the number of UTF-8 bytes needed to encode the
    cell content at [idx]. *)

val blit_cell_text : t -> int -> bytes -> pos:int -> int
(** [blit_cell_text g idx dst ~pos] writes the UTF-8 bytes for the cell content
    at [idx] into [dst] starting at [pos] and returns the number of bytes
    written. Returns [0] if [dst] has insufficient space. *)

val get_style : t -> int -> Ansi.Style.t
(** [get_style g idx] reconstructs the style at [idx]. *)

val get_background : t -> int -> Ansi.Color.t
(** [get_background g idx] is the background color at [idx]. *)

(** {1:predicates Predicates} *)

val is_empty : t -> int -> bool
(** [is_empty g idx] is [true] iff the cell is the null/empty sentinel.
    Different from a blank space cell ({!Cell.space}), which is the default
    content after {!create} and {!clear}. *)

val is_continuation : t -> int -> bool
(** [is_continuation g idx] is [true] iff the cell is the trailing part of a
    wide character. *)

val is_inline : t -> int -> bool
(** [is_inline g idx] is [true] iff the cell needs no grapheme store lookup. *)

val cell_width : t -> int -> int
(** [cell_width g idx] is the display width of the cell (0 for
    empty/continuation, 1–2 for start). *)

val cells_equal : t -> int -> t -> int -> bool
(** [cells_equal g1 idx1 g2 idx2] is [true] iff both cells have identical
    content and styling. Uses epsilon comparison for RGBA floats. *)

val hyperlink_url : t -> int32 -> string option
(** [hyperlink_url g id] resolves a link ID (from {!get_link}) to a URL. Returns
    [None] for the no-link sentinel or unknown IDs. *)

val hyperlink_url_direct : t -> int32 -> string
(** [hyperlink_url_direct g id] is like {!hyperlink_url} but returns [""]
    instead of [None]. *)

(** {1:manipulation Manipulation} *)

val resize : t -> width:int -> height:int -> unit
(** [resize g ~width ~height] resizes the grid, preserving existing contents
    where possible. Cells outside the new bounds are released. No-op if
    dimensions are unchanged.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear ~color g] resets all cells to spaces with white foreground and
    [color] background. [color] defaults to transparent. Releases all stored
    grapheme references. The scissor stack is preserved. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies all cell data from [src] to [dst], resizing [dst] to
    match. Graphemes are copied into [dst]'s storage as needed. No alpha
    blending. *)

val copy : t -> t
(** [copy g] is a deep copy of [g]. The scissor stack starts empty on the copy.
*)

val blit_region :
  src:t ->
  dst:t ->
  src_x:int ->
  src_y:int ->
  width:int ->
  height:int ->
  dst_x:int ->
  dst_y:int ->
  unit
(** [blit_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y] copies a
    rectangular region from [src] to [dst].

    The region is clamped to valid bounds in both grids. Negative coordinates
    shift the region inward. Respects the scissor on [dst]. Cross-grid alpha
    blending is source-driven: when [src]'s {!respect_alpha} is [true],
    semi-transparent source cells are composited over [dst] and fully
    transparent source cells are skipped; otherwise stored alpha values are
    copied as-is. Same-grid overlapping regions are handled as copies.
    Wide-character fragments whose start or continuation is outside the copied
    region are written as spaces. Never resizes [dst]. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect g ~x ~y ~width ~height ~color] fills a rectangle:
    - Transparent (alpha ≈ 0): leaves cells unchanged.
    - Semi-transparent: blends over existing background.
    - Opaque: overwrites entirely (space cell, white foreground, [color]
      background).

    Clipped to grid bounds and scissor. *)

val clear_rect :
  ?color:Ansi.Color.t -> t -> x:int -> y:int -> width:int -> height:int -> unit
(** [clear_rect g ~x ~y ~width ~height] clears a rectangle to spaces, default
    foreground, and [color] background. [color] defaults to transparent.

    Clipped to grid bounds and scissor. *)

(** {1:drawing Drawing} *)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text ~style ~tab_width g ~x ~y ~text] draws single-line text. Text is
    segmented into grapheme clusters using the current {!width_method}. Wide
    characters occupy multiple cells with continuation markers. Newlines are
    skipped; tabs expand to [tab_width] spaces (default [2]).

    When the resolved background has alpha < 1.0, colors are blended with
    existing cells. A space on a translucent background preserves the existing
    cell content and tints its colors.

    Respects the active scissor for start cells. If a wide character's start
    cell is visible, its continuation cells are written as well to preserve the
    grid's wide-span invariant. *)

module Border = Border
(** Box-drawing border character sets. *)

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  ?border:Border.t ->
  ?sides:Border.side list ->
  ?style:Ansi.Style.t ->
  ?fill:Ansi.Color.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?title_style:Ansi.Style.t ->
  unit ->
  unit
(** [draw_box g ~x ~y ~width ~height ()] draws a box with Unicode borders and:
    - [border] character set. Defaults to {!Border.single}.
    - [sides] to draw. Defaults to {!Border.all}.
    - [style] for border characters. Defaults to {!Ansi.Style.default}.
    - [fill] interior and border cell background color. When absent, no fill is
      applied.
    - [title] on the top border when [\`Top] is included and
      [width >= title_width + 4].
    - [title_alignment] defaults to [\`Left] with 2-cell padding.
    - [title_style] for the title text.

    Respects the scissor. *)

type line_symbols = {
  h : string;  (** Horizontal segment. *)
  v : string;  (** Vertical segment. *)
  diag_up : string;  (** Diagonal up-right. *)
  diag_down : string;  (** Diagonal down-right. *)
}
(** The type for line drawing symbol sets. *)

val default_line_symbols : line_symbols
(** Unicode box-drawing: ["─"], ["│"], ["╱"], ["╲"]. *)

val ascii_line_symbols : line_symbols
(** ASCII: ["-"], ["|"], ["/"], ["\\"]. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?symbols:line_symbols ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line g ~x1 ~y1 ~x2 ~y2 ()] draws a line using Bresenham's algorithm
    with:
    - [\`Line] (default) uses box-drawing characters with per-step symbol
      selection based on direction.
    - [\`Braille] uses 2×4 dot patterns that merge with existing braille cells,
      allowing multiple lines to share cells.

    Respects the scissor. *)

(** {1:set_cell Direct cell write} *)

val set_cell :
  t ->
  x:int ->
  y:int ->
  cell:Cell.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_cell g ~x ~y ~cell ~fg ~bg ~attrs ()] writes a single cell. [blend]
    defaults to the grid's {!respect_alpha} setting; pass [~blend:true] to force
    blending.

    Cells outside the grid or scissor are skipped. Existing wide graphemes
    spanning this cell are cleaned up. The caller is responsible for writing
    continuation cells for multi-column graphemes. *)

(** {1:clipping Clipping} *)

val push_clip : t -> region -> unit
(** [push_clip g r] pushes a clipping region. The effective clip is the
    intersection of [r] with the current clip. *)

val pop_clip : t -> unit
(** [pop_clip g] pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** [clear_clip g] removes all clipping regions. *)

val clip : t -> region -> (unit -> 'a) -> 'a
(** [clip g r f] runs [f ()] with [r] as the active clip, popping it on return
    (even on exception). *)

(** {1:opacity Opacity stack}

    Hierarchical opacity for UI trees. Drawing operations multiply color alpha
    by the product of all stacked opacities. Push/pop pairs must be balanced. *)

val push_opacity : t -> float -> unit
(** [push_opacity g o] pushes [o] (clamped to \[0.0, 1.0\]). *)

val pop_opacity : t -> unit
(** [pop_opacity g] pops the most recent opacity. No-op if the stack is empty.
*)

val current_opacity : t -> float
(** [current_opacity g] is the product of all stacked opacities, or [1.0] if the
    stack is empty. *)

(** {1:scrolling Scrolling} *)

val scroll : t -> top:int -> bottom:int -> int -> unit
(** [scroll g ~top ~bottom n] scrolls rows \[[top]..[bottom]\] by [n] lines.
    Positive [n] scrolls content up (new blank lines at bottom). Negative [n]
    scrolls down. Zero is a no-op. *)

(** {1:comparison Comparison} *)

val diff_cells : t -> t -> (int * int) array
(** [diff_cells prev curr] is the [(x, y)] coordinates of cells that differ.
    Iterates over the union of both grids' dimensions. Uses epsilon comparison
    for RGBA. Sorted by row then column. *)

(** {1:converting Converting} *)

val to_ansi : ?reset:bool -> t -> string
(** [to_ansi ~reset g] renders [g] to a string with full ANSI escape sequences.
    Appends a reset sequence when [reset] is [true] (default). *)
