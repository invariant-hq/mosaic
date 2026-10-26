(** Grids of terminal cells, the frames a renderer draws, compares and writes.

    A {e grid} is a mutable rectangle of {e cells}, [width] columns by [height]
    rows. A cell holds text, a foreground and a background color
    ({!Ansi.Color}), attributes ({!Ansi.Attr}) and a hyperlink. It is named by
    its column [x] and row [y], both from [0], or by its {e index}
    [y * width + x] ({!idx}).

    A {e grapheme} is the text a cell shows, one or more scalars that a terminal
    places as one, as {!Matrix_text} measures them. A grapheme [w] cells wide
    fills [w] adjacent cells of one row, its {e span}. The first is its
    {e start cell}, which holds it, and the other [w - 1] are its
    {e continuation cells}. A grapheme may be as wide as a row, and every cell
    belongs to exactly one span ({!span}). A cell may also be {e empty}, as
    after a control character is written. A {e blank} is a space with the
    default foreground, no attributes and no link, on a background the operation
    that makes it chooses. Text is stored with each ill-formed UTF-8 subsequence
    replaced by U+FFFD.

    A grapheme is never stored in part. An operation that writes, erases or
    moves some of a grapheme's cells turns the rest of them into blanks, on the
    background the operation writes. That is the written cell's background for a
    write, and the [color] of {!clear_rect}, {!fill_rect} and {!shift}, so that
    one of these operations never leaves two kinds of blank. A terminal needs
    this, since ECMA-48 puts the positions an erasure or a deletion vacates
    "into the erased state" (§8.3.26), and the half of a wide character they cut
    is such a position. A grapheme also has one rendition, its start cell's
    colors, attributes and hyperlink, since a terminal shows a character in one.
    Blending a translucent color over a wide character blends it over the start
    cell's background, and the other cells take the start cell's rendition.

    {e Drawing} composites colors over the cells it writes by source-over
    blending ({!Ansi.Color.blend}), after multiplying their alpha by the
    {e opacity}, the product of the opacity stack (see {!section-opacity}).
    {e Copying} stores colors as they are. Drawing writes only the start cells
    that the {e scissor}, the top of the stack of clipping regions
    ({!section-clipping}), contains. {!draw_text} and {!val-set_uchar} write the
    continuation cells of a grapheme whose start cell they write even outside
    the scissor, so that the row keeps whole spans. Each function says whether
    it draws or copies.

    Grids made with {!create_like} share their grapheme storage, which makes
    comparing and copying cells between them cheaper. A grapheme of more than
    one scalar, or of one scalar more than three cells wide, takes a slot in the
    storage while a cell holds it. Equal graphemes share a slot, and the storage
    holds [2{^ 36} - 1] slots, more than memory can hold the cells for, so only
    memory limits how many distinct graphemes the grids hold.

    Unless a function's doc says otherwise, the parts of a write outside the
    grid are left out, and a function that takes an index raises
    [Invalid_argument] if it is not in \[[0];[width g * height g - 1]\]. A grid,
    and grids that share storage, must not be used by two domains in parallel.

    The life cycle of a frame:
    + {!create} makes a grid, and {!create_like} a second one for the frame
      after it.
    + {!clear} makes its cells blanks, and {!draw_text}, {!draw_box} and the
      other drawing functions draw the frame, within {!push_clip} and
      {!push_opacity}.
    + {!cells_equal} compares it with the frame before, and {!to_ansi} or the
      cell accessors write it out.
    + {!resize} follows the terminal's size.

    This draws a word and reads the grid back as text:
    {[
    let g = Matrix_grid.create ~width:20 ~height:2 () in
    Matrix_grid.draw_text g ~x:0 ~y:0 ~text:"Hello";
    assert (Matrix_grid.to_text g = "Hello\n")
    ]} *)

(** {1:types Types} *)

type t
(** The type for grids. *)

type region = { x : int; y : int; width : int; height : int }
(** The type for rectangles of cells, the cells of columns [x] to
    [x + width - 1] and rows [y] to [y + height - 1]. A rectangle whose [width]
    or [height] is [0] or less is empty. *)

(** {1:constructors Constructors}

    A constructor raises [Invalid_argument] if [width <= 0], [height <= 0] or
    [width > max_width]. *)

val create :
  width:int -> height:int -> ?width_method:Matrix_text.width_method -> unit -> t
(** [create ~width ~height ()] is a grid of blanks on a transparent background,
    with its own grapheme storage and the width method [width_method], which
    defaults to [`Unicode]. *)

val create_like : t -> width:int -> height:int -> t
(** [create_like g ~width ~height] is a grid of blanks on a transparent
    background that shares [g]'s grapheme storage and has its width method. Two
    frames that a renderer compares are made this way. *)

(** {1:properties Properties} *)

val max_width : int
(** [max_width] is the greatest width of a grid, 262,144 cells. *)

val width : t -> int
(** [width g] is the number of columns of [g]. *)

val height : t -> int
(** [height g] is the number of rows of [g]. *)

val width_method : t -> Matrix_text.width_method
(** [width_method g] is the width method {!draw_text} uses by default. *)

val set_width_method : t -> Matrix_text.width_method -> unit
(** [set_width_method g m] makes [m] the width method of [g] for the draws after
    it. The cells already written keep their widths. *)

val active_height : t -> int
(** [active_height g] is the number of rows from the top to the last row with a
    cell that shows something a terminal's erase does not. Such a cell holds a
    character other than a space, or has an attribute, a hyperlink or a
    background other than the terminal's default, a transparent one counting as
    the default. It is [0] when no cell does. *)

(** {1:cell_access Cell access} *)

val idx : t -> x:int -> y:int -> int
(** [idx g ~x ~y] is [y * width g + x], the index of the cell at column [x] and
    row [y]. It does not check that the cell is in [g]. *)

val get_codepoint : t -> int -> int
(** [get_codepoint g idx] is the scalar of cell [idx], and [0] for an empty
    cell. It is [-1] for a continuation cell, a grapheme of more than one
    scalar, and a scalar stored more than three cells wide. *)

val get_attrs : t -> int -> int
(** [get_attrs g idx] is the attributes of cell [idx], as {!Ansi.Attr.pack}
    packs them. *)

val get_link : t -> int -> string
(** [get_link g idx] is the hyperlink URL of cell [idx], and [""] for none. *)

val get_fg : t -> int -> Ansi.Color.t
(** [get_fg g idx] is the foreground of cell [idx], its intent included. *)

val get_bg : t -> int -> Ansi.Color.t
(** [get_bg g idx] is the background of cell [idx], its intent included. *)

val get_text : t -> int -> string
(** [get_text g idx] is the grapheme of cell [idx], and [""] for an empty or a
    continuation cell. *)

val cell_text_length : t -> int -> int
(** [cell_text_length g idx] is the length in bytes of {!get_text}[ g idx], [0]
    for an empty or a continuation cell, computed without allocating. *)

val blit_cell_text : t -> int -> bytes -> pos:int -> int
(** [blit_cell_text g idx dst ~pos] writes {!get_text}[ g idx] into [dst] from
    [pos] and is the number of bytes written, [cell_text_length g idx]. It
    writes nothing and is [0] for an empty or a continuation cell, or if the
    bytes do not fit in [dst].

    Raises [Invalid_argument] if [pos < 0] or [pos > Bytes.length dst]. *)

val get_style : t -> int -> Ansi.Style.t
(** [get_style g idx] is the style of cell [idx], with its colors, attributes
    and hyperlink. *)

(** {1:predicates Predicates} *)

val is_empty : t -> int -> bool
(** [is_empty g idx] is [true] iff cell [idx] is empty. A blank holds a space
    and is not empty. *)

val is_continuation : t -> int -> bool
(** [is_continuation g idx] is [true] iff cell [idx] is a continuation cell. *)

val cell_width : t -> int -> int
(** [cell_width g idx] is the width of the grapheme that starts at cell [idx],
    and [0] for an empty or a continuation cell. *)

val span : t -> int -> int * int
(** [span g idx] is [(start, width)], the span of the grapheme that covers cell
    [idx], which starts at index [start] and covers [width] cells of the same
    row, [idx] among them. A cell that holds a grapheme one cell wide, or none,
    is a span of its own, [(idx, 1)]. Every cell of a wide grapheme, its start
    cell and its continuations, gives the same span, whose [width] is the start
    cell's {!cell_width}. A program acts on the whole span of a wide grapheme,
    such as the cells to select, to redraw, or to report under the pointer. It
    reads one cell. *)

val cells_equal : t -> int -> t -> int -> bool
(** [cells_equal g1 idx1 g2 idx2] is [true] iff the two cells have the same
    text, place in their span, colors, attributes and hyperlink. Protection is
    not compared, since it does not show. The grids may have their own storage,
    and a comparison between grids that share storage reads no grapheme. *)

(** {1:manipulation Resizing, clearing and copying} *)

val resize : t -> width:int -> height:int -> unit
(** [resize g ~width ~height] makes [g] [width] by [height] cells. A cell in
    both sizes keeps its contents, and the new cells are blanks on a transparent
    background, as are the cells of a grapheme the new width cuts. Nothing
    changes if [g] already has that size. The scissor and opacity stacks stay as
    they are.

    Raises [Invalid_argument] if [width <= 0], [height <= 0] or
    [width > max_width]. *)

val resize_clear : ?color:Ansi.Color.t -> t -> width:int -> height:int -> unit
(** [resize_clear g ~width ~height] is {!resize} followed by {!clear}[ ~color],
    if the size changes, and does nothing otherwise.

    Raises [Invalid_argument] if [width <= 0], [height <= 0] or
    [width > max_width]. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear ~color g] makes every cell of [g] a blank on [color], which defaults
    to transparent. The scissor and opacity stacks stay as they are. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies [src] to [dst], every cell and the width method, and
    resizes [dst] to the size of [src]. The graphemes are copied into the
    storage of [dst] if it is not that of [src]. Nothing changes if [src] and
    [dst] are the same grid. *)

val copy : t -> t
(** [copy g] is a grid with the cells and width method of [g], its own grapheme
    storage, and empty scissor and opacity stacks. *)

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
(** [blit_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y] copies
    the [width] by [height] cells of [src] at column [src_x] and row [src_y] to
    [dst] at column [dst_x] and row [dst_y], with their colors as they are. The
    rectangle is cut to the cells that are in both grids, and to the scissor of
    [dst]. It may overlap itself when [src] and [dst] are the same grid. It
    never resizes [dst].

    A cell whose grapheme the rectangle cuts is copied as a blank. Within one
    grid such blanks, and the cells of a grapheme of [dst] that the copy cuts,
    are on a transparent background. Between grids a blank keeps the background
    of its cell in [src], and a cut grapheme of [dst] becomes blanks on the
    background of the cell written over it. *)

val draw_region :
  src:t ->
  dst:t ->
  src_x:int ->
  src_y:int ->
  width:int ->
  height:int ->
  dst_x:int ->
  dst_y:int ->
  unit
(** [draw_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y] draws the
    cells of [src] that {!blit_region} would copy over those of [dst]. The
    colors of [src] are multiplied by the opacity of [dst], and a cell whose
    colors are then both transparent leaves its cell of [dst] as it is. A blank
    or empty cell of [src] on a background that is not opaque keeps the text of
    the cell it covers, so that a layer shows what lies under its blanks. It
    tints that cell's colors and gives it its hyperlink, and tints a wide
    grapheme whole when it covers the start cell, leaving it as it is otherwise.
    When [src] and [dst] are the same grid, it reads a copy of [src] taken
    first. It draws nothing while the opacity of [dst] is [0]. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect g ~x ~y ~width ~height ~color] draws [color] over the rectangle,
    cut to the scissor, after multiplying its alpha by the opacity. A
    transparent [color] changes nothing. A translucent one tints the colors of
    each cell, keeps its text and drops its hyperlink. An opaque one makes each
    cell a blank on [color], as {!clear_rect} does. *)

val clear_rect :
  ?color:Ansi.Color.t -> t -> x:int -> y:int -> width:int -> height:int -> unit
(** [clear_rect g ~x ~y ~width ~height] makes the cells of the rectangle, cut to
    the scissor, blanks on [color], which defaults to transparent. [color] is
    stored as it is. A grapheme with a cell in the rectangle becomes blanks
    whole, also where it extends outside the rectangle or the scissor. *)

(** {1:drawing Drawing} *)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  ?width_method:Matrix_text.width_method ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text ~style ~tab_width ~width_method g ~x ~y ~text] draws the display
    units of [text] in [style] from column [x] of row [y], each in as many cells
    as it is wide. [width_method] defaults to {!width_method}[ g], and [style]
    to {!Ansi.Style.default}, whose absent colors are the default foreground and
    a transparent background. A TAB is [tab_width] spaces, and [tab_width]
    defaults to [2]. Units of width [0], such as U+200B alone and the control
    characters, newlines among them, take no cell. A unit that would cross the
    right edge leaves spaces in [style] up to it, and nothing is drawn if both
    colors of [style] are transparent.

    Each unit replaces the text of the cells it covers, a space included, so
    drawing spaces in any style erases the text under them. To tint cells and
    keep their text, a program draws a translucent {!fill_rect}.

    Raises [Invalid_argument] if [tab_width < 1]. *)

module Border = Border
(** Box-drawing characters. *)

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
(** [draw_box g ~x ~y ~width ~height ()] draws the box of the rectangle, with
    the characters [border], {!Border.single} by default, on its sides [sides],
    {!Border.all} by default, in [style], {!Ansi.Style.default} by default.

    With [fill], the inside of the box is filled with [fill] ({!fill_rect}), and
    the border cells have [fill] as their background. Without it, their
    background is that of [style], or transparent, and the inside is left as it
    is.

    With [title], the title is drawn on the top side, if it is drawn and the box
    is at least four cells wider than the title. [title_alignment] places it two
    cells from the left, which is the default, two cells from the right, or
    centered. It is drawn in [title_style], or in the foreground of [style], on
    the border's background.

    Raises [Invalid_argument] if a character of [border] is wider than one cell.
*)

module Lines = Lines
(** Lines that join where they meet. *)

val draw_lines : ?glyphs:Lines.glyphs -> t -> Lines.t -> unit
(** [draw_lines ~glyphs g l] draws each cell of [l] that a line passes through
    at the same column and row of [g], with its glyph in [glyphs]
    ({!Lines.Unicode} by default) and its line's style, as {!draw_text} draws a
    text in a style. The cells of [l] outside [g] are left out. *)

type line_symbols = {
  h : string;  (** Horizontal segment. *)
  v : string;  (** Vertical segment. *)
  diag_up : string;  (** Diagonal from bottom left to top right. *)
  diag_down : string;  (** Diagonal from top left to bottom right. *)
}
(** The type for the characters of lines. *)

val default_line_symbols : line_symbols
(** [default_line_symbols] is ["─"], ["│"], ["╱"] and ["╲"]. *)

val ascii_line_symbols : line_symbols
(** [ascii_line_symbols] is ["-"], ["|"], ["/"] and ["\\"]. *)

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
(** [draw_line g ~x1 ~y1 ~x2 ~y2 ()] draws the line from [(x1, y1)] to
    [(x2, y2)] by Bresenham's algorithm, in [style]. With [`Line], the default,
    each cell takes the character of [symbols] for the direction of the step
    that leaves it, and the last cell that of the line's direction. With
    [`Braille], the coordinates are in dots, two across and four down per cell,
    and a cell's dots are added to those of a braille character already in it,
    so that lines can share cells. *)

(** {1:pictures Pictures}

    A {e picture} is a rectangle of pixels the program decoded. Drawing a
    picture in a rectangle of cells, its {e box}, makes each cell of the box a
    {e picture cell}, which holds one {e tile} of the picture, the part that
    falls in that cell once the picture is fit in the box, its aspect ratio kept
    and centered. A picture cell has a background, which shows through
    transparent pixels and around the picture, and has the default foreground,
    no attributes and no hyperlink. Every operation treats it as a cell of text
    one cell wide whose text is U+FFFC, as {!get_text} and {!to_text} read it.
    Copying moves it, a write over it replaces it, an erase makes it a blank,
    and a translucent color drawn over it tints its background and keeps its
    tile. Pixels are never blended, so the opacity does not apply to them.
    [Matrix_screen] decides how a terminal is sent the pixels. *)

type picture
(** The type for pictures. A screen sends a terminal the pixels of a picture it
    shows once, while the picture stays shown, so a program makes a picture once
    and keeps it. *)

val picture : width:int -> height:int -> string -> picture
(** [picture ~width ~height rgba] is the picture of [width] by [height] pixels
    whose rows, top to bottom, are in [rgba], four bytes a pixel, red, green,
    blue and alpha, in sRGB with alpha not premultiplied. It does not copy
    [rgba].

    Raises [Invalid_argument] if [width] or [height] is [0] or less, or if
    [String.length rgba <> 4 * width * height]. *)

val picture_size : picture -> int * int
(** [picture_size p] is the width and height of [p] in pixels. *)

val draw_picture :
  t -> x:int -> y:int -> width:int -> height:int -> picture -> unit
(** [draw_picture g ~x ~y ~width ~height p] makes the cells of the [width] by
    [height] box from column [x] of row [y] that the scissor contains picture
    cells of [p], each keeping its background. A grapheme it cuts becomes blanks
    on that grapheme's background. *)

(** {1:set_uchar Writing cells} *)

val set_uchar :
  t ->
  x:int ->
  y:int ->
  uchar:Uchar.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_uchar g ~x ~y ~uchar ~fg ~bg ~attrs ()] writes the scalar [uchar] at
    column [x] of row [y], as wide as {!Matrix_text.uchar_width} makes it. It
    copies colors, and draws them if [blend] is [true] ([false] by default). It
    writes nothing if the start cell is outside the grid or the scissor, and
    writes continuation cells even outside the scissor. A scalar that would
    cross the right edge is written as spaces up to it, and a control or a
    scalar of width [0] as an empty cell. Writing a grapheme of more than one
    scalar takes {!draw_text} or {!set_grapheme}. *)

val set_grapheme :
  t ->
  x:int ->
  y:int ->
  width:int ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?protected:bool ->
  string ->
  pos:int ->
  len:int ->
  unit
(** [set_grapheme g ~x ~y ~width ~fg ~bg ~attrs s ~pos ~len] stores the [len]
    bytes of [s] from [pos] as one grapheme in the [width] cells from column [x]
    of row [y]. It copies, so neither the scissor nor the opacity applies. The
    caller chooses [width], so that a terminal places text by its own display
    units and fits it to its line. [link] defaults to no link and [protected]
    ({!section-protection}) to [false]. The graphemes the write cuts become
    blanks on [bg].

    No cell holds a control character, since one has no glyph and {!to_ansi}
    would send it to the terminal that receives the cells, where it would act.

    Raises [Invalid_argument] if [len <= 0] or [pos] and [len] do not designate
    a substring of [s], if the cells are not all in the grid ([width < 1],
    [x < 0], [x + width > width g], or [y] outside the rows), or if the bytes
    hold a control character, C0, DEL or C1 (U+0080 to U+009F). *)

(** {1:protection Protection}

    A DEC terminal can mark the characters it writes as not erasable by a
    selective erase (DECSCA, VT220 Programmer Reference Manual §4.9), so that a
    form's fields can be cleared while its labels stay. The mark belongs to the
    character, so it lives in the cell and moves with it. It does not show, so
    {!get_attrs}, {!get_style}, {!cells_equal} and {!to_ansi} ignore it. Every
    operation other than {!erase_unprotected} treats it as part of the cell, and
    a blank is not protected. *)

val is_protected : t -> int -> bool
(** [is_protected g idx] is [true] iff cell [idx] was written by {!set_grapheme}
    with [~protected:true] and not written since. *)

val erase_unprotected : t -> region -> unit
(** [erase_unprotected g r] replaces with spaces the text of every unprotected
    grapheme with a cell in [r], and drops its hyperlink, which named that text.
    Each cell keeps its colors and attributes, since a selective erase leaves
    the rendition as it is (VT220 Programmer Reference Manual §4.12, DECSEL
    "does not affect video line attributes or video character attributes").
    Protected graphemes stay as they are. [r] is cut to the grid, and the
    scissor does not apply. *)

(** {1:clipping Clipping}

    The scissor is the intersection of the regions on the stack, or the whole
    grid when the stack is empty. *)

val push_clip : t -> region -> unit
(** [push_clip g r] pushes [r], so that the scissor becomes its intersection
    with [r]. *)

val pop_clip : t -> unit
(** [pop_clip g] pops the region pushed last, and does nothing if the stack is
    empty. *)

val clear_clip : t -> unit
(** [clear_clip g] empties the stack. *)

val clip_region : t -> region
(** [clip_region g] is the scissor cut to the grid, whose [width] or [height] is
    [0] when the two do not intersect. *)

val intersects_clip : t -> region -> bool
(** [intersects_clip g r] is [true] iff [r] is not empty and shares a cell with
    both the grid and the scissor. *)

val clip : t -> region -> (unit -> 'a) -> 'a
(** [clip g r f] is [f ()] with [r] pushed, and pops [r] when [f] returns or
    raises. *)

(** {1:opacity Opacity}

    The opacity stack serves a tree of drawn elements, where each element's
    opacity multiplies those of the elements around it. The opacity is the
    product of the stack's entries, and [1.0] when it is empty. Each
    {!push_opacity} must be followed by its {!pop_opacity}. *)

val push_opacity : t -> float -> unit
(** [push_opacity g o] pushes [o], clamped to \[[0.0];[1.0]\]. *)

val pop_opacity : t -> unit
(** [pop_opacity g] pops the opacity pushed last, and does nothing if the stack
    is empty. *)

val current_opacity : t -> float
(** [current_opacity g] is the opacity of [g]. *)

(** {1:shifting Shifting} *)

val shift : t -> region -> dx:int -> dy:int -> color:Ansi.Color.t -> unit
(** [shift g r ~dx ~dy ~color] moves the contents of [r] by [dx] columns and
    [dy] rows, within [r]. What leaves [r] is lost, and the cells the move
    uncovers become blanks on [color]. Text, colors, attributes and links move
    together. A grapheme cut by an edge of [r], before or after the move,
    becomes blanks on [color] whole, also outside [r]. A shift by [0] and [0]
    changes nothing. A terminal's scrolls, insertions and deletions of lines and
    characters, and column shifts are each a shift of a region by its margins.
    [r] is cut to the grid, and neither the scissor nor the opacity applies. *)

(** {1:diagnostics Diagnostics} *)

val grapheme_stats : t -> int * int
(** [grapheme_stats g] is [(live, slots)] for the grapheme storage of [g], which
    grids made with {!create_like} share. [live] is the number of graphemes
    cells hold, and [slots] the number of slots ever allocated, those freed for
    reuse included. A test that writes many graphemes checks that [slots] stays
    bounded, which shows that freed slots are reused. *)

(** {1:converting Converting} *)

val to_text : ?trim:bool -> t -> string
(** [to_text ~trim g] is the text of [g], one line per row, separated by LF. A
    start cell gives its grapheme and an empty cell a space, and a continuation
    cell gives nothing. With [~trim:true], the default, the spaces at the end of
    each line are dropped. Styles are left out, which {!to_ansi} writes. *)

val to_ansi :
  ?styling:Ansi.Styling.t ->
  ?trim:bool ->
  ?complete_rows:bool ->
  ?rows:int ->
  t ->
  string
(** [to_ansi ~styling ~trim ~complete_rows ~rows g] is the first [rows] rows of
    [g] with their styles and hyperlinks, [height g] rows by default. A row
    starts with its whole style and ends by closing its hyperlink and with
    [CSI 0 m]. Rows are separated by LF, without one after the last. With
    [~complete_rows:true] ([false] by default), each row starts by erasing the
    terminal's line (EL 2) and ends with CR LF, the last one included. An empty
    cell is written as a space. It is [""] if [rows] is [0].

    [styling] says how the styles are written ({!Ansi.Styling.t}), at
    [`Truecolor] by default. At a color depth, each color is reduced to it
    ({!Ansi.Color.to_depth}), so [`Mono] keeps the attributes, such as bold, and
    drops the colors. [`Plain] writes no style and no hyperlink, so each row is
    its text.

    With [~trim:true] ([false] by default), each row ends at its last cell that
    shows something, and the cells after it that show nothing, empty or a space
    on the default background with no attribute and no hyperlink, are not
    written. A row printed into the scrollback without them takes no more room
    than its text when the terminal rewraps it.

    Raises [Invalid_argument] if [rows] is negative or greater than [height g].
*)

(**/**)

(* For matrix's own libraries, such as the screen's diff, which reads every cell
   of a frame: the cell accessors above without their index check. Each reads
   memory outside the grid when [idx] is not in [0] to [width g * height g - 1],
   so a caller must use them only where its loop bounds prove the index in
   range. *)

module Unsafe : sig
  (* The tile of a picture cell, [picture] fit in a box of [cols] by [rows]
     cells, at column [col] and row [row] of the box. *)
  type tile = {
    picture : picture;
    cols : int;
    rows : int;
    col : int;
    row : int;
  }

  val has_pictures : t -> bool
  (* [true] iff a grid that shares [t]'s storage holds a picture cell. *)

  val is_picture : t -> int -> bool
  val tile : t -> int -> tile
  (* The tile of a picture cell; meaningless for any other cell. *)

  val picture_id : picture -> int
  (* A number of its own for each picture, from 1. *)

  val picture_rgba : picture -> string
  val get_codepoint : t -> int -> int
  val get_attrs : t -> int -> int
  val get_link : t -> int -> string
  val get_fg : t -> int -> Ansi.Color.t
  val get_bg : t -> int -> Ansi.Color.t
  val cell_text_length : t -> int -> int
  val blit_cell_text : t -> int -> bytes -> pos:int -> int
  val cell_width : t -> int -> int
  val is_empty : t -> int -> bool
  val cells_equal : t -> int -> t -> int -> bool
end

(**/**)
