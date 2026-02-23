(** Mutable grid of terminal cells.

    A grid is a two-dimensional framebuffer where each cell stores a character
    (glyph), foreground/background colors (RGBA), text attributes, and a
    hyperlink. Backed by Bigarrays for cache-friendly access and zero-allocation
    cell reads.

    {1 Quick Start}

    {[
      let grid = Grid.create ~width:80 ~height:24 () in
      Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello, world!";
      Grid.draw_text
        ~style:(Ansi.Style.make ~fg:Ansi.Color.red ~bold:true ())
        grid ~x:0 ~y:1 ~text:"Error: Operation failed"
    ]}

    {1 Character Encoding}

    Single codepoints are stored directly as packed integers. Multi-codepoint
    grapheme clusters (emoji with ZWJ, combining characters) are interned in a
    reference-counted glyph pool. Wide characters (CJK, emoji) span multiple
    cells: a start cell followed by continuation markers.

    {1 Alpha Blending}

    When {!respect_alpha} is enabled, colors with alpha < 1.0 are blended with
    existing cell colors using a perceptual curve that preserves visual contrast
    better than linear compositing. Use [~blend:true] on {!set_cell} to blend
    regardless of the {!respect_alpha} setting.

    {1 Scissor Clipping}

    A stack of clipping regions constrains drawing operations. Cells outside the
    active clip are silently skipped. Use {!clip} for scoped clipping or
    {!push_clip}/{!pop_clip} for manual control.

    {1 Performance}

    - Cell updates: O(1), may trigger glyph pool reference counting
    - Blitting: O(width × height)
    - Text drawing: O(grapheme_count)
    - Scissor checks: O(1) per cell write
    - Zero-allocation cell accessors ({!get_fg_r}, {!get_bg_r}, etc.) for
      efficient rendering loops *)

type t
(** Mutable grid of terminal cells. *)

type region = { x : int; y : int; width : int; height : int }
(** Rectangular area in cell coordinates. Covers cells satisfying
    [x <= col < x + width] and [y <= row < y + height]. Non-positive [width] or
    [height] yields an empty region. *)

(** {1 Creation} *)

val create :
  width:int ->
  height:int ->
  ?glyph_pool:Glyph.Pool.t ->
  ?width_method:Glyph.width_method ->
  ?respect_alpha:bool ->
  unit ->
  t
(** [create ~width ~height ()] creates a grid with all cells initialized to
    spaces (white foreground, transparent background).

    @param glyph_pool
      Shared pool for grapheme storage. A fresh pool is allocated if omitted.
    @param width_method Grapheme width method. Default [`Unicode].
    @param respect_alpha Enable alpha blending on {!set_cell}. Default [false].
    @raise Invalid_argument if [width <= 0] or [height <= 0]. *)

(** {1 Properties} *)

val glyph_pool : t -> Glyph.Pool.t
(** Returns the glyph pool used by this grid. *)

val width_method : t -> Glyph.width_method
(** Returns the current width computation method. *)

val set_width_method : t -> Glyph.width_method -> unit
(** Changes the width computation method for subsequent {!draw_text} calls.
    Existing cell widths are not retroactively updated. *)

val respect_alpha : t -> bool
(** Returns whether alpha blending is enabled for {!set_cell} and
    {!blit_region}. *)

val set_respect_alpha : t -> bool -> unit
(** Sets the alpha blending mode. See {!respect_alpha}. *)

val width : t -> int
(** Grid width in cells. *)

val height : t -> int
(** Grid height in cells. *)

val active_height : t -> int
(** Number of rows from the top containing non-blank content (character code
    other than 0 or space). Returns 0 for an empty or cleared grid. *)

(** {1 Cell Access}

    Linear index [idx] is row-major: [idx = y * width + x]. All accessors are
    zero-allocation. *)

val idx : t -> x:int -> y:int -> int
(** [idx t ~x ~y] returns the flat index for cell [(x, y)]. Equivalent to
    [y * width t + x]. No bounds checking. *)

val get_code : t -> int -> int
(** Cell code at [idx]. Aligned with {!Glyph.t} encoding. *)

val get_glyph : t -> int -> Glyph.t
(** Glyph value at [idx], for use with {!Glyph.Pool} operations. *)

val get_attrs : t -> int -> int
(** Packed attribute integer at [idx]. *)

val get_link : t -> int -> int32
(** Internal hyperlink ID at [idx]. *)

val get_fg_r : t -> int -> float
val get_fg_g : t -> int -> float
val get_fg_b : t -> int -> float

val get_fg_a : t -> int -> float
(** Foreground RGBA components at [idx], in [[0.0, 1.0]]. *)

val get_bg_r : t -> int -> float
val get_bg_g : t -> int -> float
val get_bg_b : t -> int -> float

val get_bg_a : t -> int -> float
(** Background RGBA components at [idx], in [[0.0, 1.0]]. *)

val get_text : t -> int -> string
(** Decodes the grapheme at [idx] into a string. Returns [""] for empty or
    continuation cells. Allocates. *)

val get_style : t -> int -> Ansi.Style.t
(** Reconstructs the style at [idx]. Allocates. *)

val get_background : t -> int -> Ansi.Color.t
(** Background color at [idx]. *)

val is_empty : t -> int -> bool
(** [true] if the cell contains the null code (0). *)

val is_continuation : t -> int -> bool
(** [true] if the cell is the trailing part of a wide character. *)

val is_inline : t -> int -> bool
(** [true] if the cell needs no glyph pool lookup (ASCII or single codepoint).
*)

val cell_width : t -> int -> int
(** Display width of the cell (0 for empty/continuation, 1-2 for start). *)

val cells_equal : t -> int -> t -> int -> bool
(** [cells_equal t1 idx1 t2 idx2] returns [true] if both cells have identical
    content and styling. Uses epsilon comparison for RGBA floats. *)

val hyperlink_url : t -> int32 -> string option
(** Resolves a link ID (from {!get_link}) to a URL. Returns [None] for "no link"
    or unknown IDs. *)

val hyperlink_url_direct : t -> int32 -> string
(** Like {!hyperlink_url} but returns [""] instead of [None]. Zero-allocation
    for the no-link case. *)

(** {1 Manipulation} *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes the grid, preserving existing contents
    where possible. Cells outside the new bounds are released. No-op if
    dimensions are unchanged.

    @raise Invalid_argument if [width <= 0] or [height <= 0]. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear ?color t] resets all cells to spaces with white foreground and
    [color] background (default: transparent). Releases all glyph pool
    references. The scissor stack is preserved. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies all cell data from [src] to [dst], resizing [dst] to
    match. When pools are shared, codes are copied verbatim. When pools differ,
    each distinct grapheme is re-interned once. No alpha blending. *)

val copy : t -> t
(** [copy t] creates a deep copy sharing the same glyph pool. The scissor stack
    starts empty on the copy. *)

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
    occurs when [dst.respect_alpha] is true or source alpha < 1.0.

    Same-grid overlapping regions are handled correctly. Never resizes [dst]. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect t ~x ~y ~width ~height ~color] fills a rectangle with [color].

    - Transparent (alpha ~ 0): clears content but preserves existing background.
    - Semi-transparent: blends over existing background.
    - Opaque: overwrites entirely (space glyph, white foreground, [color]
      background).

    Clipped to grid bounds and scissor. *)

(** {1 Drawing} *)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text ?style ?tab_width t ~x ~y ~text] draws single-line text.

    Text is segmented into grapheme clusters using the current {!width_method}.
    Wide characters occupy multiple cells with continuation markers. Newlines
    are skipped; tabs expand to [tab_width] spaces (default 2).

    When the resolved background has alpha < 1.0, colors are blended with
    existing cells. A space on a translucent background preserves the existing
    glyph and tints its colors. Graphemes that don't fit clear the remaining
    columns with styled spaces.

    Respects the active scissor. *)

module Border = Border

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
(** [draw_box t ~x ~y ~width ~height ()] draws a box with Unicode borders.

    @param border Character set for borders. Default {!Border.single}.
    @param sides Sides to draw. Default {!Border.all} (all four).
    @param style Style for border characters. Default {!Ansi.Style.default}.
    @param fill
      When provided, fills the interior and sets the border cell background to
      this color. When absent, no fill is applied and border cells use the
      [style]'s background (or terminal default).

    Titles appear on the top border when [`Top] is included and the box is wide
    enough (≥ title width + 4). Title alignment defaults to [`Left] with 2-cell
    padding. Respects the scissor. *)

type line_glyphs = {
  h : string;  (** Horizontal segment (e.g., ["─"]). *)
  v : string;  (** Vertical segment (e.g., ["│"]). *)
  diag_up : string;  (** Diagonal up-right (e.g., ["╱"]). *)
  diag_down : string;  (** Diagonal down-right (e.g., ["╲"]). *)
}
(** Glyph set for {!draw_line}. *)

val default_line_glyphs : line_glyphs
(** Unicode box-drawing glyphs: ["─"], ["│"], ["╱"], ["╲"]. *)

val ascii_line_glyphs : line_glyphs
(** ASCII glyphs: ["-"], ["|"], ["/"], ["\\"]. *)

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
(** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line using Bresenham's algorithm.

    [`Line] mode (default) uses box-drawing characters with per-step glyph
    selection based on direction. [`Braille] mode uses 2×4 dot patterns that
    merge with existing Braille cells, allowing multiple lines to share cells.

    Respects the scissor. *)

(** {1 Direct Cell Write} *)

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
(** [set_cell t ~x ~y ~glyph ~fg ~bg ~attrs ()] writes a single cell.

    @param blend
      Override alpha blending. Defaults to the grid's {!respect_alpha} setting.
      Pass [~blend:true] to force blending regardless.

    Cells outside the grid or scissor are skipped. Existing wide graphemes
    spanning this cell are cleaned up. The caller is responsible for writing
    continuation cells for multi-column graphemes. *)

(** {1 Clipping} *)

val push_clip : t -> region -> unit
(** Pushes a clipping region. The effective clip is the intersection of the new
    region with the current clip (hierarchical narrowing). *)

val pop_clip : t -> unit
(** Pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** Removes all clipping regions. *)

val clip : t -> region -> (unit -> 'a) -> 'a
(** [clip t region f] runs [f ()] with [region] as the active clip, popping it
    on return (even on exception). *)

(** {1 Opacity Stack}

    Hierarchical opacity for UI trees. Drawing operations multiply color alpha
    by the product of all stacked opacities. Push/pop pairs must be balanced.
    Supports up to 32 levels. *)

val push_opacity : t -> float -> unit
(** Pushes [opacity] (clamped to [[0.0, 1.0]]). *)

val pop_opacity : t -> unit
(** Pops the most recent opacity. No-op if the stack is empty. *)

val current_opacity : t -> float
(** Product of all stacked opacities, or [1.0] if empty. *)

(** {1 Scrolling} *)

val scroll : t -> top:int -> bottom:int -> int -> unit
(** [scroll t ~top ~bottom n] scrolls the region [[top..bottom]] by [n] lines.
    Positive [n] scrolls content up (new blank lines at bottom). Negative [n]
    scrolls content down (new blank lines at top). Zero is a no-op. *)

(** {1 Comparison} *)

val diff_cells : t -> t -> (int * int) array
(** [diff_cells prev curr] returns coordinates [(x, y)] of cells that differ.
    Iterates over the union of both grids' dimensions. Uses epsilon comparison
    for RGBA. Sorted by row, then column. *)

(** {1 Serialization} *)

val to_ansi : ?reset:bool -> t -> string
(** [to_ansi ?reset grid] renders the grid to a string with full ANSI escape
    sequences. Appends a reset sequence when [reset] is [true] (default). Useful
    for debugging, tests, and static ANSI output. *)
