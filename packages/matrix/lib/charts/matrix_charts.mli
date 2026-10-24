(** Charts drawn in terminal cells.

    A {e figure} ({!t}) is an immutable description of a chart. Its {e marks},
    such as lines, bars and rasters, draw {e rows}, the elements of an array the
    program gives each mark, and read each row through accessor functions bound
    to the mark's {e roles}, such as [x], [y] and [color]. A role reads a
    {e scale} found by name: position roles read ["x"] and ["y"] and color roles
    read ["color"], unless {!rename} names another. A scale maps the values of
    every role that reads it to a {e position} in \[[0];[1]\]. Its {e domain} is
    fitted to the union of their values, widened to zero by the marks that stand
    on it, rounded outward, replaced by an explicit domain ({!scale}) and
    replaced again by a window of a {!View.t}. Axes follow from the position
    scales and legends from the color scales read through categories; {!axis}
    and {!legend} change them.

    A figure is drawn in two steps. {!layout} fits the scales, chooses the
    ticks, measures the labels and places the {e plot}, the rectangle where
    marks draw, inside a region of cells. The result ({!Layout.t}) answers
    pointer questions, which value or row lies under a cell, without drawing.
    {!draw} writes a layout into a {!Matrix_grid.t} at an origin. The life cycle
    of a chart in an interactive program usually goes like this:
    + Keep the data and a {!View.t} in the program's state, {!View.empty} at
      first.
    + Build the figure from them each frame, lay it out at the region's size and
      draw it.
    + On a pointer event, lay the figure out at the same size, ask the layout
      for the row under the pointer ({!Layout.hit}) or the next view
      ({!Layout.zoom}, {!Layout.pan}), and store the view.

    Unless a value's doc says otherwise:
    - Lengths are in cells. A value at position [p] lies in the cell
      [min (W - 1) ⌊p W⌋] of a [W]-cell axis, and in the matching dot at every
      sub-cell resolution, so a dot, a bar end and a tick at one value share a
      cell.
    - Accessors and format functions must be pure, since layout calls them and
      may call them again.
    - A row is {e dropped} when a position it gives is NaN, infinite or, on a
      log scale, not above 0. It is drawn nowhere and fitted to nothing, and a
      line breaks at it.
    - Within a cell, a mark's write replaces the plane it paints, the glyph with
      its foreground or the background, except that dots of the color already in
      the cell merge into its glyph.
    - Nothing raises for a size. A region too small for every guide gives up the
      title, the legends, the axis titles, the tick labels with their notes, the
      axis lines and the margins of {!margins}, in this order, and every size
      from one cell draws.

    This prints a loss curve with a reference line:
    {[
    let losses = [| 2.0; 1.4; 1.1; 0.9; 0.8 |] in
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
(** The type for the ids of marks. A mark's id is its path in the figure, one
    segment per {!layer} it is in: its index in that layer, in decimal, or the
    name {!name} gives it there. *)

(** {1:color Colors}

    A mark without a color takes the theme's next palette color, as does each
    category the first time it appears, in the order of the figure. *)

type 'r paint
(** The type for the colors of the rows of type ['r]. *)

val const : Ansi.Color.t -> 'r paint
(** [const c] colors every row [c], with no legend entry. *)

val series : ?color:Ansi.Color.t -> string -> 'r paint
(** [series s] colors every row as the category [s] of the color scale, which
    gives [s] a legend entry. [color] pins the category's color. *)

val cat : ('r -> string) -> 'r paint
(** [cat f] colors each row [r] as the category [f r] of the color scale. *)

(** {1:marks Marks}

    A mark's [?dots] is its resolution: [`Cell] draws whole-cell glyphs, [`Half]
    two dots per cell, one above the other, [`Quadrant] 2 by 2, [`Sextant] 2 by
    3, and [`Braille] and [`Octant] 2 by 4. Strokes and dots default to the
    theme's [dots]. With dots, a cell shows one foreground, so marks that share
    a cell show the later mark's color there. *)

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
(** [line ~x ~y rows] joins the points of [rows] in order. Without [x], a row's
    [x] is its index. [`Steps] draws a horizontal then a vertical segment
    between points, with the theme's box glyphs. [dash] defaults to [`Solid]. *)

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
(** [area ~y rows] fills between the line of [y] and the line of [y2], or [0]
    without [y2]. At [`Cell], the default, it paints backgrounds. *)

val dot :
  ?dots:[ dots | `Density ] ->
  ?symbol:string ->
  ?color:'r paint ->
  x:('r -> float) ->
  y:('r -> float) ->
  'r array ->
  t
(** [dot ~x ~y rows] marks each point. At [`Cell] a point is [symbol], the
    theme's point glyph by default. [`Density] shades each cell by the number of
    points in it against the fullest cell. *)

val rect :
  ?color:'r paint ->
  ?x:('r -> float) ->
  ?x2:('r -> float) ->
  ?y:('r -> float) ->
  ?y2:('r -> float) ->
  'r array ->
  t
(** [rect rows] paints the background of the cells from [x] to [x2] and from [y]
    to [y2] of each row. An extent without either end covers the plot, and with
    one end the cells of that value, so [rect ~x ~x2:x] fills one column. *)

val bar_y :
  ?dots:[ `Eighths | `Cell ] ->
  ?color:'r paint ->
  ?label:('r -> string) ->
  x:('r -> string) ->
  y:('r -> float) ->
  'r array ->
  t
(** [bar_y ~x ~y rows] draws a bar from [0] to [y] in the band of category [x].
    Rows of one category stack in row order, those above [0] upwards and those
    below downwards. [`Eighths], the default, ends a bar in an eighth of a cell.
    A row's [label] is drawn inside its bar where it fits. *)

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
(** [raster ~x ~y ~value rows] colors each cell by [value] on the color scale
    ({!Theme.t.colors} unless the scale gives [colors]). The rows in a cell
    reduce to the [`Last], the default, the [`Mean] or the [`Max]; [`Bilinear]
    instead interpolates each cell's middle between the four rows around it, the
    rows forming a grid. [`Cell], the default, paints the background, [`Half]
    two values per cell, [`Shade] a shade glyph by value and [`Glyph g] the
    glyph [g]. *)

val text :
  ?color:'r paint ->
  ?anchor:[ `Left | `Center | `Right ] ->
  x:('r -> float) ->
  y:('r -> float) ->
  text:('r -> string) ->
  'r array ->
  t
(** [text ~x ~y ~text rows] writes [text] at each point, starting at it,
    centered on it or ending at it by [anchor], [`Left] by default, in the
    theme's label color by default. *)

val arrow :
  ?color:'r paint ->
  ?head:[ `Arrow | `Dot | `None ] ->
  x:('r -> float) ->
  y:('r -> float) ->
  x2:('r -> float) ->
  y2:('r -> float) ->
  'r array ->
  t
(** [arrow ~x ~y ~x2 ~y2 rows] draws a line from [(x, y)] to [(x2, y2)] with
    [head], [`Arrow] by default, at its end, in the theme's label color by
    default. *)

val spark :
  ?dots:[ `Eighths | `Braille ] -> ?color:Ansi.Color.t -> float list -> t
(** [spark vs] draws the values of [vs], newest first as [v :: vs] holds them,
    one per column with the newest at the right, as many as fit. A value below
    [0] is drawn as [0]. Its y domain runs from [0] to the largest of [vs]
    unrounded, and it implies no axes. A row of a hit is an index into [vs]. *)

val hline : ?dash:dash -> ?color:Ansi.Color.t -> float -> t
(** [hline v] draws a horizontal line across the plot at [y = v], in the theme's
    axis color by default. Its value widens the y domain. *)

val vline : ?dash:dash -> ?color:Ansi.Color.t -> float -> t
(** [vline v] is {!hline} vertical, at [x = v]. *)

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
(** [candles rows] draws a candle at each [time], a body from [open_] to [close]
    on a wick from [low] to [high], [width] cells wide, [1] or [2], [1] by
    default. A rising candle ([close >= open_]) is [rising], green by default,
    and drawn hollow if [hollow] holds; a falling one is [falling], red by
    default.

    Raises [Invalid_argument] if [width] is not [1] or [2]. *)

val histogram :
  ?bins:[ `Count of int | `Width of float | `Edges of float array ] ->
  ?height:[ `Count | `Probability | `Density ] ->
  ?color:(float * float * float) paint ->
  float array ->
  t
(** [histogram vs] draws a bar per bin of the finite values of [vs]. [bins]
    splits their range into [`Count n] equal bins, [`Count 10] by default, or
    bins [`Width w] wide, or takes the bins between the sorted [`Edges]; a bin
    holds its lower edge and the last one its upper edge too. A bar is the bin's
    count, its share of the values or that share over its width. The rows of the
    mark, which [color] reads and a hit names, are the bins
    [(lower, upper, height)] in order.

    Raises [Invalid_argument] if [`Count n] has [n < 1] or [`Width w] is not
    positive. *)

val circle :
  ?color:'r paint ->
  x:('r -> float) ->
  y:('r -> float) ->
  r:('r -> float) ->
  'r array ->
  t
(** [circle ~x ~y ~r rows] draws the circle of center [(x, y)] and radius [r] in
    data units of each row, an ellipse in cells when the axes scale differently.
*)

(** {1:scales Scales}

    A scale specification sets the properties it is given, and the others take
    what the marks reading the scale imply, then the defaults below. A scale
    read by quantities is linear, and one read by categories is a band scale,
    unless a specification says otherwise. Bars, areas without [y2], histograms
    and sparks imply [zero] on their value scale; bars imply a band padding of
    [0.2] and a band read by [y] implies [reverse]. *)

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
  (** [linear ()] maps values linearly. [domain] replaces the fitted domain.
      [nice], [true] by default for position scales, rounds a fitted domain
      outward to multiples of the step [1], [2] or [5] times a power of ten
      nearest a tenth of its length. [zero] widens it to [0]. [reverse] runs
      positions from [1] to [0]. [ticks] places the axis's ticks at those of its
      values in the domain. [colors] are a color scale's colors, low to high.

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
  (** [log ()] maps the logarithms of values in [base], [10.] by default, as
      {!linear} maps values; [nice] rounds to powers of [base]. A value not
      above [0] is not placeable.

      Raises [Invalid_argument] if [base] is not above [1], or as {!linear}
      raises, or if [domain] starts at or below [0]. *)

  val time :
    ?domain:float * float -> ?nice:bool -> ?offset_s:int -> unit -> float t
  (** [time ()] maps Unix seconds linearly and ticks and labels them in local
      time at [offset_s] seconds from UTC, [0] by default ({!section-guides}).
      [nice] rounds to boundaries of the calendar interval nearest a tenth of
      the domain's length.

      Raises [Invalid_argument] as {!linear} raises. *)

  val band :
    ?domain:string list ->
    ?padding:float ->
    ?step:int ->
    ?reverse:bool ->
    ?colors:Ansi.Color.t list ->
    unit ->
    string t
  (** [band ()] gives each category an equal band, in the order of [domain],
      which drops the rows of other categories, or else in the order the figure
      first gives them. A bar leaves [padding] of its band empty, [0.1] by
      default. With [step], a band is [step] cells and the bands start at the
      axis's start. [colors] gives a color scale's categories their colors, in
      domain order and cycled.

      Raises [Invalid_argument] if [padding] is not in \[[0];[1]\) or [step] is
      below [1], or [colors] is empty. *)
end

(** {1:composing Composing} *)

val layer : t list -> t
(** [layer fs] draws [fs] in order, later over earlier, on shared scales. *)

val title : string -> t -> t
(** [title s f] is [f] with the title [s], centered above the plot. *)

val name : string -> t -> t
(** [name s f] is [f] with the id segment [s] in its layer ({!type-id}). *)

val scale : string -> 'd Scale.t -> t -> t
(** [scale n s f] is [f] with [s] the specification of scale [n]. *)

val rename : string -> string -> t -> t
(** [rename a b f] is [f] where every role, specification, axis and legend that
    names scale [a] names [b]. A second y scale is a renamed one. *)

val attrs : Ansi.Attr.t -> t -> t
(** [attrs a f] draws the glyphs of [f]'s marks with the attributes [a] added.
*)

val margins :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> ?inset:int -> t -> t
(** [margins f] is [f] with least margins in cells, where the guides need less
    room, so that charts placed side by side align. [inset] puts that many cells
    between the plot and its axis lines. Each is [0] by default.

    Raises [Invalid_argument] if one is negative. *)

(** {1:guides Guides}

    Each position scale has an axis: an x scale below the plot, the first y
    scale left of it and the second right of it. Each color scale of categories
    has a legend inside the plot's top-right corner, drawn where it fits.
    {!axis} and {!legend} replace these.

    The ticks of a continuous axis lie in its shown domain. They are chosen by
    the scored search of Talbot, Lin and Hanrahan for steps of [1], [5], [2] or
    [2.5] times a power of ten, powers on a log scale and calendar boundaries on
    a time scale. The search balances round steps, the domain's coverage and a
    density of labels that fill half the axis, and keeps at least one blank cell
    between two labels. Labels share their decimals ([0.00], [0.25], [0.50]);
    values with more than seven significant digits read against an offset note
    ([+10⁶]), and values of [10^6] or more or below [10^-4] against a factor
    note ([×10⁶]), shown after the axis's title. Time labels name the coarsest
    unit that starts at every tick ([2026], [Mar], [3], [14:05], [:09], [.25])
    over a context line, [Mar 2026], shown where it changes. A band axis labels
    every [k]th category for the least [k] of [1], [2], [5], [10], … whose
    labels fit. *)

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
(** [axis n] is the axis of scale [n] on [side]. With [show], [true] by default,
    false, it is drawn nowhere and takes no room. [grid] draws gridlines across
    the plot at its ticks, also between them dividing each interval into [n]
    parts ([`Minor n]), or every [n] cells; a band axis draws them between
    bands. [format i v] labels the [i]th tick, of value [v]. The search aims for
    ticks at least [spacing] cells apart. The styles default to the theme's axis
    and label styles.

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
(** [legend n] is the legend of scale [n]: an entry per category, its mark's
    glyph in its color then its name, one per row or on one row, under [title].
    [side] defaults to [`Inside `Top_right]. *)

val frame : t
(** [frame] closes the axis lines into a box around the plot. *)

(** {1:theme Themes} *)

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
        (** Shades from lightest to fullest; the last fills bars. *)
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
        (** Cells a tick reaches beyond its axis line; at [0] it is a tee in the
            line. *)
    label_padding : int;
        (** Blank columns between a vertical axis's ticks and its labels. A
            horizontal axis's labels are on the row after its ticks. *)
    aspect : float;  (** The height of a cell over its width. *)
  }
  (** The type for themes. A layout reads its theme, so changing the theme
      restyles every element the figure leaves unstyled. *)

  val dark : t
  (** [dark] suits a dark background: the palette cyan, magenta, yellow, green,
      blue, red then three blues; viridis for color scales; gray guides; dotted
      gridlines; braille dots; ticks as tees with labels a column away; aspect
      [2.]. *)

  val light : t
  (** [light] is {!dark} with styles for a light background. *)

  val light_glyphs : glyphs
  (** [light_glyphs], {!dark}'s, are light box-drawing glyphs. *)

  val heavy_glyphs : glyphs
  (** [heavy_glyphs] are heavy box-drawing glyphs. *)

  val rounded_glyphs : glyphs
  (** [rounded_glyphs] are light box-drawing glyphs with round corners. *)

  val ascii_glyphs : glyphs
  (** [ascii_glyphs] are ASCII characters, without blocks, so that every chart
      draws in ASCII. *)
end

(** {1:views Views}

    A {e view} gives some continuous position scales a {e window}, the part of
    the domain the plot shows. A program keeps its view, lays its figure out
    under it, and replaces it with the view an edit returns. Windows are in data
    values, so they survive resizing and rebuilding. *)

module View : sig
  type t
  (** The type for views. *)

  val empty : t
  (** [empty] shows every scale's domain. *)

  val window : string -> t -> (float * float) option
  (** [window n v] is the window of scale [n] in [v]. *)

  val set : string -> (float * float) option -> t -> t
  (** [set n w v] is [v] with window [w] for scale [n], or none.

      Raises [Invalid_argument] if [w] is not finite and increasing. *)

  val zoom : ?axes:[ `X | `Y | `Both ] -> figure -> float -> t -> t
  (** [zoom f k v] is [v] with the shown window of each continuous position
      scale of [f] on [axes], [`Both] by default, [k] times narrower about its
      middle, on a log scale in powers. A window too narrow for floats stays.

      Raises [Invalid_argument] if [k] is not finite and positive. *)

  val pan : figure -> by:float * float -> t -> t
  (** [pan f ~by:(dx, dy) v] is [v] with the shown window of each continuous x
      scale of [f] moved [dx] times its length towards greater values, and of
      each y scale [dy] times. *)

  val clamp : figure -> t -> t
  (** [clamp f v] is [v] with each window moved inside its scale's domain in
      [f], and dropped where it is wider than the domain. *)
end

(** {1:laying_out Laying out and drawing}

    Cells of a layout are counted from the top-left cell of its region. *)

type hit = {
  id : id;  (** The mark's id. *)
  row : int;  (** The row's index in the array the mark was given. *)
  cell : int * int;  (** The cell the row inks. *)
  distance : float;  (** The distance from the pointer's cell. *)
}
(** The type for the rows found under a cell. *)

(** The type for marks drawn over a chart in cells, such as pointer feedback. *)
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
  (** [position l n v] is the column, or for a y scale the row, of [v] on scale
      [n], clamped to the plot, and for a category its band's middle.

      Raises [Invalid_argument] if [n] is not a position scale of [l], or [v] is
      not one of its values. *)

  val domain : t -> string -> value list
  (** [domain l n] is the shown window [[Num lo; Num hi]] of scale [n], or its
      categories.

      Raises [Invalid_argument] if [n] is not a position scale of [l]. *)

  val zoom : ?axes:[ `X | `Y | `Both ] -> t -> at:int * int -> float -> View.t
  (** [zoom l ~at k] is [l]'s view with each continuous position scale on
      [axes], [`Both] by default, [k] times narrower about its value at cell
      [at], moved into the plot, so that value stays under [at] while the plot
      keeps its place.

      Raises [Invalid_argument] if [k] is not finite and positive. *)

  val pan : t -> by:int * int -> View.t
  (** [pan l ~by:(dx, dy)] is [l]'s view in which the data drawn at cell
      [(x, y)] is drawn at [(x + dx, y + dy)]. A drag is the pan by the
      pointer's move. *)

  val hit :
    ?radius:float ->
    ?metric:[ `Euclidean | `X | `Y ] ->
    t ->
    int * int ->
    hit option
  (** [hit l c] is the row whose mark inks cell [c] last, or else that inks the
      nearest cell within [radius], [3.] by default, the later mark of two at
      one distance. [`Euclidean], the default, measures rows [aspect] times
      columns. [`X] keeps cells within [radius] columns and adds the vertical
      distance divided by [100], and [`Y] the converse. It is [None] if [c] is
      outside the plot. It inks the marks again, so it costs about a {!draw}. *)

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
(** [layout f ~width ~height] lays [f] out in a region of [width] by [height]
    cells under [view], {!View.empty} by default, with [theme], {!Theme.dark} by
    default, measuring text by [width_method], [`Unicode] by default. At [depth]
    [`Ansi256] color scales take palette entries 16 to 255 in order of
    lightness; at [`Ansi16], [`Ansi8] and [`Mono] they draw shade glyphs;
    [`Truecolor] is the default.

    Raises [Invalid_argument] for two specifications, axes, legends or titles of
    one thing, a specification, axis or legend of a scale no role reads, an axis
    of a color scale or across its direction, a legend of a scale without
    categories, a scale read as more than one of x, y and color or by both
    quantities and categories against its kind, or a third y scale without a
    side. *)

val draw :
  ?overlays:overlay list -> Layout.t -> Matrix_grid.t -> x:int -> y:int -> unit
(** [draw l g ~x ~y] draws [l] into [g] with its region's top-left cell at
    [(x, y)], writing only inside the region, then [overlays] in order. *)

(** {1:smoothing Smoothing}

    Each keeps the [x] of every point and smooths its [y]. *)

val ema : float -> (float * float) array -> (float * float) array
(** [ema alpha ps] keeps the first point and gives each later one
    [alpha *. y +. (1. -. alpha) *. prev], where [prev] is the previous result.
*)

val sma : int -> (float * float) array -> (float * float) array
(** [sma n ps] gives each point the mean of the [n] points up to it, or of those
    there are, and is empty if [n <= 0]. *)

val gaussian : float -> (float * float) array -> (float * float) array
(** [gaussian sigma ps] smooths [ps] with a Gaussian kernel of standard
    deviation [sigma] points and radius [ceil (3 *. sigma)], its weights
    renormalized near the ends. It is [ps] if [sigma <= 0.]. *)
