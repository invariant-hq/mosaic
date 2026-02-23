(** Mutable grid of terminal cells.

    A grid is a two-dimensional framebuffer where each cell stores a character
    ({!Glyph.t}), foreground and background colors (RGBA), text attributes, and
    a hyperlink. Backed by bigarrays for cache-friendly access.

    Single codepoints are stored directly as packed integers. Multi-codepoint
    grapheme clusters (ZWJ emoji, combining characters) are interned in a
    reference-counted {!Glyph.Pool.t}. Wide characters span multiple cells: a
    start cell followed by continuation markers.

    When {!respect_alpha} is enabled, colors with alpha < 1.0 are blended with
    existing cell colors using a perceptual curve. A stack of {e scissor}
    clipping regions constrains drawing; cells outside the active clip are
    silently skipped. *)

(** {1:types Types} *)

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
  ?glyph_pool:Glyph.Pool.t ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  unit ->
  t
(** [create ~width ~height ()] is a grid with all cells set to spaces (white
    foreground, transparent background) with:
    - [glyph_pool] shared grapheme storage. A fresh pool is allocated if
      omitted.
    - [width_method] grapheme width method. Defaults to [`Unicode].
    - [respect_alpha] alpha blending on {!set_cell}. Defaults to [false].

    Raises [Invalid_argument] if [width <= 0] or [height <= 0]. *)

(** {1:properties Properties} *)

val width : t -> int
(** [width g] is the grid width in cells. *)

val height : t -> int
(** [height g] is the grid height in cells. *)

val glyph_pool : t -> Glyph.Pool.t
(** [glyph_pool g] is the glyph pool used by [g]. *)

val width_method : t -> Glyph.width_method
(** [width_method g] is the current width computation method. *)

val set_width_method : t -> Glyph.width_method -> unit
(** [set_width_method g m] changes the width method for subsequent {!draw_text}
    calls. Existing cell widths are not updated. *)

val respect_alpha : t -> bool
(** [respect_alpha g] is [true] iff alpha blending is enabled for {!set_cell}
    and {!blit_region}. *)

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

val get_code : t -> int -> int
(** [get_code g idx] is the cell code at [idx]. *)

val get_glyph : t -> int -> Glyph.t
(** [get_glyph g idx] is the glyph at [idx]. *)

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

val get_style : t -> int -> Ansi.Style.t
(** [get_style g idx] reconstructs the style at [idx]. *)

val get_background : t -> int -> Ansi.Color.t
(** [get_background g idx] is the background color at [idx]. *)

(** {1:predicates Predicates} *)

val is_empty : t -> int -> bool
(** [is_empty g idx] is [true] iff the cell is the null/empty glyph sentinel.
    Different from a blank space cell ({!Glyph.space}), which is the default
    content after {!create} and {!clear}. *)

val is_continuation : t -> int -> bool
(** [is_continuation g idx] is [true] iff the cell is the trailing part of a
    wide character. *)

val is_inline : t -> int -> bool
(** [is_inline g idx] is [true] iff the cell needs no glyph pool lookup (ASCII
    or single codepoint). *)

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
    [color] background. [color] defaults to transparent. Releases all glyph pool
    references. The scissor stack is preserved. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies all cell data from [src] to [dst], resizing [dst] to
    match. When pools are shared, codes are copied verbatim. When pools differ,
    each distinct grapheme is re-interned once. No alpha blending. *)

val copy : t -> t
(** [copy g] is a deep copy of [g] sharing the same glyph pool. The scissor
    stack starts empty on the copy. *)

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
    shift the region inward. Respects the scissor on [dst]. Alpha blending
    occurs when [dst]'s {!respect_alpha} is [true] or source alpha < 1.0.
    Same-grid overlapping regions are handled correctly. Never resizes [dst]. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect g ~x ~y ~width ~height ~color] fills a rectangle:
    - Transparent (alpha ≈ 0): clears content, preserves existing background.
    - Semi-transparent: blends over existing background.
    - Opaque: overwrites entirely (space glyph, white foreground, [color]
      background).

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
    glyph and tints its colors.

    Respects the active scissor. *)

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

type line_glyphs = {
  h : string;  (** Horizontal segment. *)
  v : string;  (** Vertical segment. *)
  diag_up : string;  (** Diagonal up-right. *)
  diag_down : string;  (** Diagonal down-right. *)
}
(** The type for line drawing glyph sets. *)

val default_line_glyphs : line_glyphs
(** Unicode box-drawing: ["─"], ["│"], ["╱"], ["╲"]. *)

val ascii_line_glyphs : line_glyphs
(** ASCII: ["-"], ["|"], ["/"], ["\\"]. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?glyphs:line_glyphs ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line g ~x1 ~y1 ~x2 ~y2 ()] draws a line using Bresenham's algorithm
    with:
    - [\`Line] (default) uses box-drawing characters with per-step glyph
      selection based on direction.
    - [\`Braille] uses 2×4 dot patterns that merge with existing braille cells,
      allowing multiple lines to share cells.

    Respects the scissor. *)

(** {1:set_cell Direct cell write} *)

val set_cell :
  t ->
  x:int ->
  y:int ->
  glyph:Glyph.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_cell g ~x ~y ~glyph ~fg ~bg ~attrs ()] writes a single cell. [blend]
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
