(** Charts drawn in terminal cells.

    A {e figure} ({!t}) is an immutable description of a chart. Its {e marks},
    such as {!line} and {!bar_y}, draw {e rows}. A mark's rows are the elements
    of the array the program gives it, and the mark reads each row through
    accessors bound to its {e roles}, such as [x], [y] and [color]. A role reads
    {e quantities}, floats, or {e categories}, strings.

    A role reads a {e scale} by name. [x] roles read the scale ["x"], [y] roles
    ["y"] and color roles ["color"], unless {!rename} names another scale. A
    scale read by [x] or [y] roles is a {e position scale}, and one read by
    color roles a {e color scale}. A scale of quantities is {e continuous}
    (linear, log or time), and a scale of categories is a {e band} scale. A
    scale maps each value to a {e position} in \[[0];[1]\], [0] at the left of
    an x axis and at the bottom of a y axis.

    The {e domain} of a continuous scale is fitted to the union of the values of
    the roles that read it. Marks that stand on zero, such as bars, widen it to
    [0], and on a position scale it is then rounded outward. A domain given with
    {!scale} replaces the fitted one. The program keeps a {e view} ({!View.t}),
    which may give a position scale a {e window}, and the window replaces the
    domain on screen. The window, or else the domain, is the scale's
    {e shown domain}. The {e guides}, axes for position scales and legends for
    color scales of categories, follow from the scales.

    A figure is drawn in two steps. {!layout} fits the scales, chooses the
    ticks, measures the labels and places the {e plot}, the rectangle where
    marks draw, inside a region of cells. The result, a {!Layout.t}, tells which
    value or row lies under a cell. {!draw} writes a layout into a
    {!Matrix_grid.t} at an origin. The life cycle of a chart in an interactive
    program usually goes like this:
    + Keep the data and a view in the program's state, {!View.empty} at first.
    + Build the figure from them each frame, lay it out at the region's size and
      draw it.
    + On a pointer event, lay the figure out at the same size, ask the layout
      for the row under the pointer ({!Layout.hit}) or the next view
      ({!Layout.zoom}, {!Layout.pan}), and store the view.

    Unless a value's doc says otherwise:
    - Lengths are in cells. A value at position [p] lies in the cell
      [min (W - 1) ⌊p W⌋] of a [W]-cell axis, and in the matching dot at every
      sub-cell resolution, so every mark and tick at one value lands in the same
      cell.
    - Accessors and format functions must be pure, since layout calls them and
      may call them again.
    - A row is {e dropped} when a value a position role reads is NaN, infinite
      or, on a log scale, not above [0]. Layout skips a dropped row when it fits
      domains and when it draws, and a line breaks at it.
    - A mark {e inks} the cells it writes, and is cut at the plot's edges. A
      cell has two {e planes}, a glyph with its foreground and a background. A
      mark's write replaces the plane it paints, except that dots of the color
      already in the cell merge into its glyph.
    - Layout accepts a region of any size from one cell and never raises because
      of it. A region too small for every guide gives up the title, the legends,
      the axis titles, the tick labels with their notes, the axis lines and the
      margins of {!margins}, in this order.

    This prints a loss curve with a reference line. The line takes each loss's
    index as its [x], and the reference line widens the y domain to include
    [1.0]:
    {[
    let losses = [| 2.0; 1.6; 1.3; 1.2; 1.1 |] in
    let fig = Matrix_charts.(layer [ line ~y:Fun.id losses; hline 1.0 ]) in
    let g = Matrix_grid.create ~width:60 ~height:15 () in
    Matrix_charts.(draw (layout ~width:60 ~height:15 fig) g ~x:0 ~y:0);
    print_string (Matrix_grid.to_ansi g)
    ]} *)

(** {1:figures Figures} *)

type t
(** The type for figures. *)

type figure = t
(** [figure] is {!t}, for the submodules that define their own [t]. *)

type id = string list
(** The type for the ids of marks. A mark's id is its path in the figure, with
    one segment for each {!layer} that contains it. A segment is the index of
    the mark's branch among the layer's children, guides included, in decimal,
    or the name {!name} gives it there. *)

(** {1:color Colors}

    A mark without a color takes the theme's next palette color, as does each
    category the first time it appears, in the order of the figure. Text,
    arrows, rules, rasters and candles take the colors their docs give instead.
    Only categories have legend entries. *)

type 'r paint
(** The type for the colors of rows of type ['r]. *)

val const : Ansi.Color.t -> 'r paint
(** [const c] is the color [c] for every row. *)

val series : ?color:Ansi.Color.t -> string -> 'r paint
(** [series s] is the color of the category [s] of the color scale for every
    row. [color] pins the category's color. *)

val cat : ('r -> string) -> 'r paint
(** [cat f] is the color of the category [f r] of the color scale for each row
    [r]. *)

(** {1:marks Marks}

    A mark's [?dots] is its resolution, in the sense of the dots it draws in a
    cell. [`Cell] draws whole-cell glyphs, [`Half] two dots per cell, one above
    the other, [`Quadrant] 2 by 2, [`Sextant] 2 by 3, and [`Braille] and
    [`Octant] 2 by 4. Lines and dots default to {!Theme.t.dots}. The rows of a
    mark built from plain values, {!spark} or {!histogram}, are made by the
    mark, and its doc says what they are. *)

type dots = [ `Cell | `Half | `Quadrant | `Sextant | `Braille | `Octant ]
(** The type for resolutions. *)

type dash = [ `Solid | `Dashed | `Dotted ]
(** The type for line patterns. *)

val line :
  ?dots:[ dots | `Steps ] ->
  ?dash:dash ->
  ?color:'r paint ->
  ?x:('r -> float) ->
  y:('r -> float) ->
  'r array ->
  t
(** [line ~x ~y rows] is the line through the points of [rows] in order. Without
    [x], a row's [x] is its index. [`Steps] goes horizontally then vertically
    from point to point, in the theme's box glyphs. [dash] defaults to [`Solid].
*)

val line_gaps :
  ?dots:[ dots | `Steps ] ->
  ?dash:dash ->
  ?color:'r paint ->
  ?x:('r -> float) ->
  y:('r -> float option) ->
  'r array ->
  t
(** [line_gaps ~y rows] is {!line} where a row whose [y] is [None] is dropped.
*)

val area :
  ?dots:dots ->
  ?color:'r paint ->
  ?x:('r -> float) ->
  ?y2:('r -> float) ->
  y:('r -> float) ->
  'r array ->
  t
(** [area ~y rows] is the region between the line of [y] and the line of [y2],
    or [0] without [y2]. At [`Cell], the default, it paints backgrounds. *)

val dot :
  ?dots:[ dots | `Density ] ->
  ?symbol:string ->
  ?color:'r paint ->
  x:('r -> float) ->
  y:('r -> float) ->
  'r array ->
  t
(** [dot ~x ~y rows] is a mark at each point. At [`Cell] a point is [symbol],
    the theme's point glyph by default. [`Density] shades each cell by the
    number of points in it against the fullest cell. *)

val rect :
  ?color:'r paint ->
  ?x:('r -> float) ->
  ?x2:('r -> float) ->
  ?y:('r -> float) ->
  ?y2:('r -> float) ->
  'r array ->
  t
(** [rect rows] is the background of the cells from [x] to [x2] and from [y] to
    [y2] of each row. An extent with neither end covers the plot, and an extent
    with one end covers the cells of that value. [rect ~x ~x2:x] fills the
    column of [x]. *)

val bar_y :
  ?dots:[ `Eighths | `Cell ] ->
  ?color:'r paint ->
  ?label:('r -> string) ->
  x:('r -> string) ->
  y:('r -> float) ->
  'r array ->
  t
(** [bar_y ~x ~y rows] is a bar from [0] to [y] in the band of category [x] for
    each row. Rows of one category stack in row order, those above [0] upward
    and those below downward, and the ends of the stacks are fitted. A bar ends
    in an eighth of a cell at [`Eighths], the default. A row's [label] is drawn
    inside its bar where it fits. *)

val bar_x :
  ?dots:[ `Eighths | `Cell ] ->
  ?color:'r paint ->
  ?label:('r -> string) ->
  y:('r -> string) ->
  x:('r -> float) ->
  'r array ->
  t
(** [bar_x ~y ~x rows] is {!bar_y} with horizontal bars, the first category at
    the top. *)

val raster :
  ?dots:[ `Cell | `Half | `Shade | `Glyph of string ] ->
  ?reduce:[ `Last | `Mean | `Max ] ->
  ?interpolate:[ `None | `Bilinear ] ->
  x:('r -> float) ->
  y:('r -> float) ->
  value:('r -> float) ->
  'r array ->
  t
(** [raster ~x ~y ~value rows] colors each cell by [value] on the color scale.
    The rows in a cell reduce to the [`Last], the default, the [`Mean] or the
    [`Max]. [`Bilinear] instead interpolates each cell's middle between the four
    rows around it, and then the rows must form a grid. [`Cell], the default,
    paints the background, [`Half] two values per cell, [`Shade] a shade glyph
    by value and [`Glyph g] the glyph [g]. *)

val text :
  ?color:'r paint ->
  ?anchor:[ `Left | `Center | `Right ] ->
  x:('r -> float) ->
  y:('r -> float) ->
  text:('r -> string) ->
  'r array ->
  t
(** [text ~x ~y ~text rows] is [text] written at each point, in the theme's
    label color by default. [anchor] puts the point at the text's start
    ([`Left], the default), middle or end. *)

val arrow :
  ?color:'r paint ->
  ?head:[ `Arrow | `Dot | `None ] ->
  x:('r -> float) ->
  y:('r -> float) ->
  x2:('r -> float) ->
  y2:('r -> float) ->
  'r array ->
  t
(** [arrow ~x ~y ~x2 ~y2 rows] is a line from [(x, y)] to [(x2, y2)], in the
    theme's label color by default, that ends in [head], [`Arrow] by default. *)

val spark :
  ?dots:[ `Eighths | `Braille ] -> ?color:Ansi.Color.t -> float list -> t
(** [spark vs] is the values of [vs] one per column, the newest at the right and
    the columns at the right of the plot. [vs] lists the newest first, as
    [v :: vs] builds it, and the oldest values that do not fit are left out. A
    value below [0] is drawn as [0]. Its y domain runs from [0] to the largest
    of all of [vs], unrounded, it reads no x scale, and its y axis shows only if
    {!axis} adds it. Its rows are the elements of [vs]. *)

val hline : ?dash:dash -> ?color:Ansi.Color.t -> float -> t
(** [hline v] is a horizontal line across the plot at [y = v], in the theme's
    axis color by default. Its value is fitted. *)

val vline : ?dash:dash -> ?color:Ansi.Color.t -> float -> t
(** [vline v] is {!hline} for a vertical line at [x = v]. *)

type ohlc = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}
(** The type for the prices of one period. *)

val candles :
  ?width:int ->
  ?hollow:bool ->
  ?rising:Ansi.Color.t ->
  ?falling:Ansi.Color.t ->
  ohlc array ->
  t
(** [candles rows] is a candle at each [time], a body from [open_] to [close]
    over a wick from [low] to [high]. A candle is [width] cells wide, [1] by
    default. A rising candle ([close >= open_]) is [rising], green by default,
    and drawn hollow if [hollow] holds, and a falling one is [falling], red by
    default.

    Raises [Invalid_argument] if [width] is not [1] or [2]. *)

val histogram :
  ?bins:[ `Count of int | `Width of float | `Edges of float array ] ->
  ?height:[ `Count | `Probability | `Density ] ->
  ?color:(float * float * float) paint ->
  float array ->
  t
(** [histogram vs] is a bar per bin of the finite values of [vs]. [bins] splits
    their range into [`Count n] equal bins, [`Count 10] by default, or into bins
    [`Width w] wide, or takes the bins between the sorted [`Edges]. A bin holds
    its lower edge, and the last one its upper edge too. [height] makes a bar
    the bin's count ([`Count], the default), its share of the values
    ([`Probability]) or that share over the bin's width ([`Density]). Its rows
    are the bins [(lower, upper, height)] in order.

    Raises [Invalid_argument] if [`Count n] has [n < 1] or [`Width w] is not
    positive. *)

val circle :
  ?color:'r paint ->
  x:('r -> float) ->
  y:('r -> float) ->
  r:('r -> float) ->
  'r array ->
  t
(** [circle ~x ~y ~r rows] is, for each row, the circle of center [(x, y)] and
    radius [r] in data units, whose points are fitted. It is drawn as an ellipse
    when the two axes have different scales. *)

(** {1:scales Scales}

    A scale specification sets the properties it is given. The others take what
    the marks reading the scale {e imply}, then the defaults of the constructors
    below. Bars, areas without [y2], histograms and sparks imply [zero] on the
    scale of their quantities, except on a log scale, and a band scale read by
    [y] implies [reverse]. A domain given to a specification is neither widened
    nor rounded. A domain of no values is \[[0];[1]\], or \[[1];[10]\] on a log
    scale. A domain of one value maps every value to the position [0.5], except
    that a domain widened to [0] from [0] alone is \[[0];[1]\]. *)

module Scale : sig
  type 'd t
  (** The type for specifications of scales of values ['d]. *)

  val linear :
    ?domain:float * float ->
    ?nice:bool ->
    ?zero:bool ->
    ?reverse:bool ->
    ?ticks:float list ->
    ?colors:Ansi.Color.t list ->
    unit ->
    float t
  (** [linear ()] is a scale that maps values linearly. [domain] replaces the
      fitted domain. [nice], [true] by default for position scales and [false]
      for color scales, rounds a fitted domain outward to multiples of the step
      [1], [2] or [5] times a power of ten nearest a tenth of its length. [zero]
      widens it to [0]. [reverse] maps the domain from [1] to [0]. [ticks] puts
      the axis's ticks at those of its values in the shown domain. [colors] are
      a color scale's colors, low to high.

      Raises [Invalid_argument] if [domain] is not finite and increasing or
      [colors] is empty. *)

  val log :
    ?base:float ->
    ?domain:float * float ->
    ?nice:bool ->
    ?reverse:bool ->
    ?ticks:float list ->
    ?colors:Ansi.Color.t list ->
    unit ->
    float t
  (** [log ()] is a scale that maps the logarithms of values in [base], [10.] by
      default, as {!linear} maps values, and [nice] rounds to powers of [base].

      Raises [Invalid_argument] if [base] is not above [1], or as {!linear}
      raises, or if [domain] starts at or below [0]. *)

  val time :
    ?domain:float * float -> ?nice:bool -> ?offset_s:int -> unit -> float t
  (** [time ()] is a scale that maps Unix seconds linearly, and ticks and labels
      them in the local time [offset_s] seconds from UTC, [0] by default
      ({!section-guides}). [nice] rounds to boundaries of the calendar interval
      nearest a tenth of the domain's length.

      Raises [Invalid_argument] as {!linear} raises. *)

  val band :
    ?domain:string list ->
    ?padding:float ->
    ?step:int ->
    ?reverse:bool ->
    ?colors:Ansi.Color.t list ->
    unit ->
    string t
  (** [band ()] is a scale that gives each category an equal band, in the order
      of [domain] or else in the order the figure first gives them. With
      [domain], the rows of other categories are dropped. A bar leaves [padding]
      of its band empty, [0.2] by default. With [step], a band is [step] cells
      and the bands start at the start of the axis. [colors] gives a color
      scale's categories their colors, in domain order, cycled.

      Raises [Invalid_argument] if [padding] is not in \[[0];[1]\) or [step] is
      below [1], or [colors] is empty. *)
end

(** {1:composing Composing}

    Settings and guides are figures too. A wrapper ({!title}, {!name}, {!scale},
    {!rename}, {!attrs}, {!margins}) applies to the figure it wraps, and a guide
    ({!axis}, {!legend}) or the {!frame} is a child of a layer, as in
    [layer [ line ~y data; axis ~title:"loss" "y" ]]. A figure is
    {e well-formed} when it has at most one title, one specification of each
    scale, one axis and one legend of each scale; when each scale it specifies
    or gives a guide is read by a role; when each axis is of a position scale
    and on a side along its direction, and each legend of a color scale of
    categories; when no scale is read by both position and color roles or by [x]
    and [y] roles, or by both quantities and categories, and none against the
    kind its specification gives; and when a third y scale has an axis with a
    side. *)

val layer : t list -> t
(** [layer fs] is the figures [fs] drawn in order, later over earlier, on shared
    scales. *)

val title : string -> t -> t
(** [title s f] is [f] with the title [s] centered above the plot. *)

val name : string -> t -> t
(** [name s f] is [f] with the id segment [s] in its layer ({!type-id}). *)

val scale : string -> 'd Scale.t -> t -> t
(** [scale n s f] is [f] with [s] the specification of scale [n]. *)

val rename : string -> string -> t -> t
(** [rename a b f] is [f] where every role, specification, axis and legend that
    names scale [a] names [b]. A figure gets a second y scale where one of its
    layers renames ["y"], as in [rename "y" "volume" f]. *)

val attrs : Ansi.Attr.t -> t -> t
(** [attrs a f] is [f] with the attributes [a] added to the glyphs of its marks.
*)

val margins :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> ?inset:int -> t -> t
(** [margins f] is [f] with least margins in cells, where the guides need less
    room, so that charts placed side by side align. [inset] puts that many cells
    between the plot and its axis lines. Each is [0] by default.

    Raises [Invalid_argument] if one is negative. *)

(** {1:guides Guides}

    Each position scale has an axis. An x scale's axis lies below the plot, the
    first y scale's to its left and the second's to its right. Each color scale
    of categories has a legend right of the plot, beyond a second y axis, in the
    room it needs. A legend [`Inside] a corner of the plot is drawn where it
    fits, over the cells of the marks under it. {!axis} and {!legend} replace
    these.

    The ticks of a continuous axis lie in its shown domain, at steps of [1],
    [5], [2] or [2.5] times a power of ten, at powers on a log scale and at
    calendar boundaries on a time scale. Their number follows the axis's length,
    and two labels keep at least one blank cell between them. Labels share their
    decimals, as [0.00], [0.25], [0.50]. When the values need more than seven
    significant digits, the labels show each value minus an offset, and when
    they reach [10⁶] or lie below [10⁻⁴] in magnitude, each value over a factor.
    The axis title is followed by that {e note}, as [+10⁶] or [×10⁶]. Time
    labels name the coarsest unit that starts at every tick, as [2026], [Mar],
    [3], [14:05], [:09] or [.25], over a context line, as [Mar 2026], shown
    where it changes. A band axis labels every [k]th category for the least [k]
    of [1], [2], [5], [10], … whose labels fit. *)

type side = [ `Left | `Right | `Top | `Bottom ]
(** The type for the sides of the plot. *)

val axis :
  ?side:side ->
  ?show:bool ->
  ?title:string ->
  ?grid:[ `None | `Ticks | `Minor of int | `Every of int ] ->
  ?format:(int -> float -> string) ->
  ?spacing:int ->
  ?style:Ansi.Style.t ->
  ?tick_style:Ansi.Style.t ->
  ?label_style:Ansi.Style.t ->
  ?title_style:Ansi.Style.t ->
  string ->
  t
(** [axis n] is the axis of scale [n] on [side], by default the side its scale's
    axis takes ({!section-guides}). [~show:false] hides it and gives its room to
    the plot. A horizontal axis's [title] is centered below its labels, and a
    vertical one's is on the row of cells above the plot. [grid] draws gridlines
    across the plot at its ticks ([`Ticks]), also dividing each interval into
    [n] parts ([`Minor n]), or every [n] cells ([`Every n]), and none by
    default; a band axis draws them between bands. [format i v] labels the [i]th
    tick, of value [v], in place of the labels and their note. The ticks aim to
    be at least [spacing] cells apart, [0] by default. [style] is the line's,
    [tick_style] the ticks', and [label_style] and [title_style] the labels' and
    the title's.

    Raises [Invalid_argument] if [`Minor n] has [n < 2], [`Every n] has [n < 1]
    or [spacing] is negative. *)

val legend :
  ?side:
    [ side
    | `Inside of [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ] ] ->
  ?show:bool ->
  ?title:string ->
  ?direction:[ `Rows | `Columns ] ->
  string ->
  t
(** [legend n] is the legend of scale [n], under [title]. Each category has an
    entry, the glyph of the first mark that colors it, in its color, then its
    name. [`Rows], the default, puts an entry on each row of cells and
    [`Columns] puts them on one. [side] defaults to [`Right], and [~show:false]
    hides it. *)

val frame : t
(** [frame] is the axis lines closed into a box around the plot. *)

(** {1:theme Themes}

    A layout reads its theme, and an element without a style of its own takes
    the theme's. Colors go to the grid as the theme and the figure give them,
    except color scales, which take the terminal's depth into account
    ({!layout}). *)

module Theme : sig
  type glyphs = {
    box : Matrix_grid.Border.t;
        (** Axis lines, ticks, the frame, solid gridlines, crosshairs, tip
            borders and steps. *)
    dashed : string * string;  (** Dashed lines, horizontal and vertical. *)
    dotted : string * string;  (** Dotted lines, horizontal and vertical. *)
    diagonals : string * string;  (** Lines at [`Cell] rising and falling. *)
    point : string;  (** A point at [`Cell]. *)
    marker : string;  (** Markers and the [`Dot] arrow head. *)
    shades : string array;
        (** Shades from lightest to fullest. The last fills bars. *)
    blocks : bool;
        (** Whether the terminal draws block elements, braille patterns,
            sextants and octants. Without them every mark draws whole cells. *)
  }
  (** The type for the glyphs a chart draws. *)

  type t = {
    palette : Ansi.Color.t list;
        (** Colors of marks without a color and of categories, cycled. *)
    colors : Ansi.Color.t list;
        (** Color scales' colors, low to high, interpolated in Oklab. *)
    background : Ansi.Color.t option;
        (** Fill of the region. [None] leaves the cells under the chart. *)
    axis : Ansi.Style.t;  (** Axis lines, ticks and rules. *)
    label : Ansi.Style.t;  (** Labels, axis titles, legends and text. *)
    title : Ansi.Style.t;  (** The figure's title. *)
    grid : Ansi.Style.t;  (** Gridlines. *)
    minor_grid : Ansi.Style.t;  (** Minor gridlines. *)
    grid_dash : dash;  (** The pattern of gridlines. *)
    tip : Ansi.Style.t;  (** Tips' text and fill. *)
    tip_border : Ansi.Style.t option;  (** Tips' border, [None] for none. *)
    tip_padding : int;  (** Columns between a tip's border and its text. *)
    crosshair : Ansi.Style.t;  (** Crosshairs. *)
    crosshair_dash : dash;  (** The pattern of crosshairs. *)
    marker : Ansi.Style.t;  (** Markers. *)
    glyphs : glyphs;  (** Glyphs. *)
    dots : dots;  (** The default resolution of strokes and dots. *)
    tick_length : int;
        (** Cells a tick reaches beyond its axis line. At [0] it is a tee in the
            line. *)
    label_padding : int;
        (** Blank columns between a vertical axis's ticks and its labels. A
            horizontal axis's labels are on the row of cells after its ticks. *)
    aspect : float;  (** The height of a cell over its width. *)
  }
  (** The type for themes. *)

  val dark : t
  (** [dark] is the theme for a dark background. Its palette is cyan, magenta,
      yellow, green, blue, red and three blues, and its color scales run through
      viridis. Its guides are gray, its gridlines dotted, its dots braille, its
      ticks tees with labels one column away, its background [None] and its
      aspect [2.]. *)

  val light : t
  (** [light] is {!dark} with styles for a light background. *)

  val light_glyphs : glyphs
  (** [light_glyphs] are light box-drawing glyphs, those of {!dark}. *)

  val heavy_glyphs : glyphs
  (** [heavy_glyphs] are heavy box-drawing glyphs. *)

  val rounded_glyphs : glyphs
  (** [rounded_glyphs] are light box-drawing glyphs with round corners. *)

  val ascii_glyphs : glyphs
  (** [ascii_glyphs] are ASCII characters, without blocks, so that every chart
      draws in ASCII. *)
end

(** {1:views Views}

    A view maps names of continuous position scales to windows, ranges of data
    values. A program lays its figure out under its view and replaces the view
    with the one {!View.zoom}, {!View.pan}, {!View.clamp}, {!Layout.zoom} or
    {!Layout.pan} returns. Windows survive resizing and rebuilding, and layout
    ignores a window of a band scale or of a scale the figure lacks. Layout
    shows a window as it is, outside the domain too, and {!View.clamp} brings it
    inside. *)

module View : sig
  type t
  (** The type for views. *)

  val empty : t
  (** [empty] is the view without windows. *)

  val window : string -> t -> (float * float) option
  (** [window n v] is the window of scale [n] in [v]. *)

  val set : string -> (float * float) option -> t -> t
  (** [set n w v] is [v] with window [w] for scale [n], or none.

      Raises [Invalid_argument] if [w] is not finite and increasing. *)

  val zoom : ?axes:[ `X | `Y | `Both ] -> figure -> float -> t -> t
  (** [zoom f k v] is [v] with the shown domain of each continuous position
      scale of [f] on [axes], [`Both] by default, [k] times narrower about its
      middle, in the space of the logarithms on a log scale. A window too narrow
      for floating-point numbers to narrow it further is left unchanged.

      Raises [Invalid_argument] if [k] is not finite and positive. *)

  val pan : figure -> by:float * float -> t -> t
  (** [pan f ~by:(dx, dy) v] is [v] with the shown domain of each continuous x
      scale of [f] moved [dx] times its length toward greater values, and of
      each y scale [dy] times. *)

  val clamp : figure -> t -> t
  (** [clamp f v] is [v] with each window moved inside its scale's domain in
      [f], and removed where it is as wide as the domain or wider. *)
end

(** {1:laying_out Laying out and drawing}

    Cells of a layout are counted from the top-left cell of its region, so a
    program that draws at [(x, y)] subtracts [(x, y)] from a grid cell before
    asking. A {e hit} ({!type-hit}) is a row found under a cell, and an
    {e overlay} ({!type-overlay}) a mark in cells drawn over a chart, such as
    pointer feedback. *)

type hit = {
  id : id;  (** The mark's id. *)
  row : int;  (** The row's index in the mark's rows. *)
  cell : int * int;  (** The cell where the row was found. *)
  distance : float;  (** Its distance from the cell asked about. *)
}
(** The type for hits. *)

(** The type for overlays. *)
type overlay =
  | Crosshair of (int * int)
      (** Lines across the plot through a cell, in the theme's crosshair style.
      *)
  | Marker of (int * int)  (** The theme's marker glyph at a cell. *)
  | Tip of {
      at : int * int;
      anchor : [ `Auto | `Left | `Right | `Top | `Bottom ];
      lines : string list;
    }
      (** A box of [lines] two cells beside, above or below [at], moved into the
          plot. [`Auto] takes the first of [`Right], [`Left], [`Top] and
          [`Bottom] that the plot clips least among those that leave [at]
          uncovered. *)

module Layout : sig
  type t
  (** The type for figures laid out in a region. *)

  type value = Num of float | Cat of string  (** The type for scale values. *)

  val plot : t -> Matrix_grid.region
  (** [plot l] is [l]'s plot. *)

  val value : t -> string -> int * int -> value option
  (** [value l n c] is the value of scale [n] at the middle of cell [c], or the
      category whose band holds it, and [None] if [c] is outside the plot.

      Raises [Invalid_argument] if [n] is not a position scale of [l]. *)

  val position : t -> string -> value -> int
  (** [position l n v] is the column, or for a y scale the row of cells, of [v]
      on scale [n], clamped to the plot, and for a category its band's middle.

      Raises [Invalid_argument] if [n] is not a position scale of [l], or [v] is
      not one of its values. *)

  val domain : t -> string -> value list
  (** [domain l n] is the shown domain [[Num lo; Num hi]] of scale [n], or its
      categories.

      Raises [Invalid_argument] if [n] is not a position scale of [l]. *)

  val zoom : ?axes:[ `X | `Y | `Both ] -> t -> at:int * int -> float -> View.t
  (** [zoom l ~at k] is [l]'s view with the shown domain of each continuous
      position scale on [axes], [`Both] by default, [k] times narrower about its
      value at cell [at], or at the plot's nearest cell if [at] is outside it. A
      layout of the new view whose plot has the same place shows that value at
      [at].

      Raises [Invalid_argument] if [k] is not finite and positive. *)

  val pan : t -> by:int * int -> View.t
  (** [pan l ~by:(dx, dy)] is [l]'s view in which the data drawn at cell
      [(x, y)] is drawn at [(x + dx, y + dy)]. For a drag, [by] is the pointer's
      move in cells since the previous event. *)

  val hit :
    ?radius:float ->
    ?metric:[ `Euclidean | `X | `Y ] ->
    t ->
    int * int ->
    hit option
  (** [hit l c] is the row whose mark inked cell [c] last. When no mark inked
      [c], it is the row that inked last the nearest cell within [radius], [3.]
      by default, the later mark's of two at one distance, and [None] if there
      is none or [c] is outside the plot. Distances are in columns.
      [`Euclidean], the default, counts a row of cells as {!Theme.t.aspect}
      columns. [`X] keeps the cells within [radius] columns and adds a hundredth
      of the vertical distance, which breaks ties, and [`Y] the converse. It
      inks the marks again, so it costs about a draw. *)

  val legend : t -> (string * Ansi.Color.t * string) list
  (** [legend l] is the category, color and glyph of every legend entry of [l],
      for a program that draws a legend itself. *)
end

val layout :
  ?view:View.t ->
  ?theme:Theme.t ->
  ?depth:Ansi.Color.depth ->
  ?width_method:Matrix_text.width_method ->
  width:int ->
  height:int ->
  t ->
  Layout.t
(** [layout f ~width ~height] is [f] laid out in a region of [width] by [height]
    cells, under [view], {!View.empty} by default, with [theme], {!Theme.dark}
    by default, measuring text by [width_method], [`Unicode] by default. At
    [depth] [`Ansi256] color scales take the terminal's palette entries 16 to
    255 in order of lightness, and at [`Ansi16], [`Ansi8] and [`Mono] they draw
    shade glyphs. [depth] defaults to [`Truecolor].

    Raises [Invalid_argument] if [f] is not well-formed ({!section-composing}).
*)

val draw :
  ?overlays:overlay list -> Layout.t -> Matrix_grid.t -> x:int -> y:int -> unit
(** [draw l g ~x ~y] draws [l] into [g] with its region's top-left cell at
    [(x, y)], then draws [overlays] in order. It writes only inside the region.
*)

(** {1:smoothing Smoothing}

    Each function keeps the [x] of every point of its array, in order, and
    smooths its [y]. *)

val ema : float -> (float * float) array -> (float * float) array
(** [ema alpha ps] keeps the first point and gives each later one
    [alpha *. y +. (1. -. alpha) *. prev], where [prev] is the previous result.
    [alpha] must be in \[[0];[1]\]. *)

val sma : int -> (float * float) array -> (float * float) array
(** [sma n ps] gives each point the mean of the [n] points up to it, or of those
    there are, and is empty if [n <= 0]. *)

val gaussian : float -> (float * float) array -> (float * float) array
(** [gaussian sigma ps] is [ps] smoothed with a Gaussian kernel of standard
    deviation [sigma] points and radius [ceil (3 *. sigma)], its weights
    renormalized near the ends. It is [ps] if [sigma <= 0.]. *)
