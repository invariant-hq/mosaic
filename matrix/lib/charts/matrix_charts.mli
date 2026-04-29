(** Terminal charts rendered to a {!Grid.t}.

    [Matrix_charts] renders line, scatter, bar, heatmap, candlestick, and other
    chart types into a terminal grid. Charts are immutable specifications built
    from composable {!Mark.t} values, then compiled into a {!Layout.t} for
    drawing and interaction.

    The typical workflow is:

    + Build an immutable {!t} by layering marks (lines, bars, scatter, etc.).
    + Keep viewport state in a separate {!View.t} (zoom and pan belong in your
      model, not the chart).
    + Call {!draw} which compiles a {!Layout.t} and renders to a {!Grid.t}.
    + Use {!Layout} + {!Hit} + {!Overlay} for hover tooltips, crosshairs,
      snapping, and zoom-to-cursor.

    {2:coords Coordinate systems}

    {e Cell coordinates} ([px], [py]) are integer terminal-cell positions.
    [(0, 0)] is the top-left corner of the grid.

    {e Data coordinates} ([x], [y]) are floating-point values in the data
    domain. Convert between systems with {!Layout.data_of_px} and
    {!Layout.px_of_data}.

    Some marks support sub-cell resolution (e.g. [`Braille2x4] renders 2×4 dots
    per cell). Sub-cell rendering is purely visual; all coordinates in the API
    remain in cell units. *)

(** {1:charset Charset} *)

module Charset : sig
  (** Character sets for chart rendering.

      Each preset provides a complete set of glyphs for frames, axes, ticks,
      gridlines, data points, and tooltips. *)

  type line_pattern = [ `Solid | `Dashed | `Dotted ]
  (** The type for line rendering patterns. *)

  type frame = {
    tl : string;  (** Top-left corner. *)
    tr : string;  (** Top-right corner. *)
    bl : string;  (** Bottom-left corner. *)
    br : string;  (** Bottom-right corner. *)
    h : string;  (** Horizontal segment. *)
    v : string;  (** Vertical segment. *)
    tee_up : string;  (** Upward tee junction. *)
    tee_down : string;  (** Downward tee junction. *)
    tee_left : string;  (** Leftward tee junction. *)
    tee_right : string;  (** Rightward tee junction. *)
    cross : string;  (** Cross junction. *)
  }
  (** The type for box-drawing character sets. *)

  type t = {
    frame : frame;  (** Primary frame characters. *)
    axis_h : string;  (** Horizontal axis line. *)
    axis_v : string;  (** Vertical axis line. *)
    tick_h : string;  (** Horizontal tick mark. *)
    tick_v : string;  (** Vertical tick mark. *)
    grid_h_solid : string;  (** Horizontal solid gridline. *)
    grid_v_solid : string;  (** Vertical solid gridline. *)
    grid_h_dashed : string;  (** Horizontal dashed gridline. *)
    grid_v_dashed : string;  (** Vertical dashed gridline. *)
    grid_h_dotted : string;  (** Horizontal dotted gridline. *)
    grid_v_dotted : string;  (** Vertical dotted gridline. *)
    point_default : string;  (** Default scatter point glyph. *)
    point_heavy : string;  (** Heavy scatter point glyph. *)
    bar_fill : string;  (** Bar fill character. *)
    shade_levels : string array;
        (** Shade density levels, lightest to heaviest. *)
    tooltip_frame : frame;  (** Tooltip border characters. *)
    diag_up : string;  (** Upward diagonal. *)
    diag_down : string;  (** Downward diagonal. *)
  }
  (** The type for complete chart character sets. *)

  val ascii : t
  (** ASCII-only characters. *)

  val unicode_light : t
  (** Light Unicode box-drawing characters. *)

  val unicode_heavy : t
  (** Heavy Unicode box-drawing characters. *)

  val unicode_rounded : t
  (** Rounded Unicode box-drawing characters. *)

  val default : t
  (** [default] is {!unicode_light}. *)
end

(** {1:theme Theme} *)

module Theme : sig
  (** Colour and style themes for charts.

      A theme controls the colour palette for auto-styling marks, background,
      and styles for axes, gridlines, labels, tooltips, crosshairs, and markers.
      It also embeds a {!Charset.t}. *)

  type t = {
    palette : Ansi.Color.t array;
        (** Colour cycle for marks without explicit styles. Marks are assigned
            colours sequentially, wrapping around. *)
    background : Ansi.Color.t option;
        (** Plot background. [None] leaves the grid unchanged. *)
    axes : Ansi.Style.t;  (** Axis lines and tick marks. *)
    border : Ansi.Style.t;  (** Chart frame/border. *)
    grid : Ansi.Style.t;  (** Major gridlines. *)
    grid_minor : Ansi.Style.t;  (** Minor gridlines. *)
    labels : Ansi.Style.t;  (** Axis labels and titles. *)
    tooltip : Ansi.Style.t;  (** Tooltip text and background. *)
    tooltip_border : Ansi.Style.t option;
        (** Tooltip border. [None] draws no border. *)
    crosshair : Ansi.Style.t;  (** Crosshair lines. *)
    marker : Ansi.Style.t;  (** Highlight markers. *)
    charset : Charset.t;  (** Symbol character set. *)
  }

  val dark : t
  (** Dark theme for dark terminal backgrounds. *)

  val light : t
  (** Light theme for light terminal backgrounds. *)

  val default : t
  (** [default] is {!dark}. *)

  val with_charset : Charset.t -> t -> t
  (** [with_charset cs t] is [t] with character set [cs]. *)
end

(** {1:label_format Label formatting} *)

module Label_format : sig
  (** Axis label formatters.

      Each formatter has the signature [int -> float -> string] matching
      {!Axis.formatter}. The [int] is the tick index (zero-based); the [float]
      is the tick value. *)

  val float : ?precision:int -> unit -> int -> float -> string
  (** [float ()] formats values with [%.*g]. [precision] defaults to [3]. *)

  val mmdd_utc : int -> float -> string
  (** [mmdd_utc] formats Unix timestamps as [MM/DD] in UTC. *)

  val hhmmss_utc : int -> float -> string
  (** [hhmmss_utc] formats Unix timestamps as [HH:MM:SS] in UTC. *)
end

(** {1:transform Transform} *)

module Transform : sig
  (** Data smoothing and transformation.

      All transforms operate on [(x, y)] arrays, preserving x-values and
      smoothing y-values. The input array is not modified. Empty arrays produce
      empty output. *)

  val ema : float -> (float * float) array -> (float * float) array
  (** [ema alpha data] is the exponential moving average of [data]. [alpha] is
      the smoothing factor in \[0, 1\]; higher values weight recent observations
      more heavily. *)

  val sma : int -> (float * float) array -> (float * float) array
  (** [sma window data] is the simple moving average of [data] over [window]
      points. For indices before the window is full a partial average is used.
      Empty if [window <= 0]. *)

  val gaussian : float -> (float * float) array -> (float * float) array
  (** [gaussian sigma data] applies Gaussian kernel smoothing. [sigma] controls
      the smoothing width; the kernel radius is [ceil(3 × sigma)]. Returns
      [data] unchanged if [sigma <= 0]. *)
end

(** {1:scale Scale} *)

module Scale : sig
  (** Axis scaling strategies.

      Scales control how data values map to pixel positions. *)

  type numeric_domain = [ `Auto | `Domain of float * float ]
  (** The type for domain specifications. [`Auto] infers bounds from the data.
  *)

  type t =
    | Auto
        (** Infer scale type from marks. Selects {!Band} when bar marks are
            present, {!Numeric} otherwise. *)
    | Numeric of { domain : numeric_domain; clamp : bool }
        (** Linear numeric scale. When [clamp] is [true], values outside the
            domain are clamped. *)
    | Log of { base : float; domain : numeric_domain; clamp : bool }
        (** Logarithmic scale. Positive values only; values [<= 0] are clamped
            to [1e-10]. *)
    | Band of { categories : string list option; padding : float }
        (** Categorical band scale. Each category gets an equal-width band.
            [padding] (\[0, 0.95\]) controls inter-band spacing. *)

  val numeric : ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [numeric ()] is a linear numeric scale. [domain] defaults to [`Auto].
      [clamp] defaults to [true]. *)

  val log : ?base:float -> ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [log ()] is a logarithmic scale. [base] defaults to [10.0] and must be
      [> 1] (values [<= 1] fall back to [10]). [domain] defaults to [`Auto].
      [clamp] defaults to [true]. *)

  val band : ?categories:string list -> ?padding:float -> unit -> t
  (** [band ()] is a categorical band scale. [categories] is an explicit
      category order; [None] infers from marks. [padding] defaults to [0.1],
      clamped to \[0, 0.95\]. *)
end

(** {1:axis Axis} *)

module Axis : sig
  (** Axis rendering configuration.

      Controls visibility, tick marks, labels, and styling for a single axis. *)

  type formatter = int -> float -> string
  (** The type for tick label formatters. Receives the tick index (zero-based)
      and the tick value. See {!Label_format}. *)

  type line = [ `None | `Axis_only | `Frame ]
  (** The type for axis line rendering modes.
      - [`None]: no axis line.
      - [`Axis_only]: axis line only.
      - [`Frame]: full frame around the plot area. *)

  type title = { text : string; style : Ansi.Style.t option }
  (** The type for axis titles. *)

  type t = {
    show : bool;  (** Whether to render this axis. *)
    line : line;  (** Axis line mode. *)
    ticks : int;
        (** Target number of ticks. The actual count may differ for "nice" tick
            spacing. *)
    format : formatter;  (** Tick label formatter. *)
    style : Ansi.Style.t option;
        (** Axis line style. [None] inherits from theme. *)
    tick_style : Ansi.Style.t option;
        (** Tick mark style. [None] inherits from theme. *)
    label_style : Ansi.Style.t option;
        (** Tick label style. [None] inherits from theme. *)
    tick_length : int;  (** Tick mark length in cells. *)
    label_padding : int;  (** Space between tick mark and label in cells. *)
    title : title option;  (** Axis title. *)
  }

  val hidden : t
  (** Hidden axis: [show = false], no ticks, no line. *)

  val default : t
  (** Default axis: visible, 6 target ticks, [`Axis_only] line, 1-cell ticks
      with 1-cell label padding. *)

  val with_ticks : int -> t -> t
  (** [with_ticks n a] is [a] with target tick count [n]. Clamped to [>= 0]. *)

  val with_format : formatter -> t -> t
  (** [with_format fmt a] is [a] with tick label formatter [fmt]. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s a] is [a] with axis line style [s]. *)

  val with_tick_style : Ansi.Style.t -> t -> t
  (** [with_tick_style s a] is [a] with tick mark style [s]. *)

  val with_label_style : Ansi.Style.t -> t -> t
  (** [with_label_style s a] is [a] with tick label style [s]. *)

  val with_tick_length : int -> t -> t
  (** [with_tick_length n a] is [a] with tick mark length [n]. Clamped to
      [>= 0]. *)

  val with_label_padding : int -> t -> t
  (** [with_label_padding n a] is [a] with label padding [n]. Clamped to [>= 0].
  *)

  val with_line : line -> t -> t
  (** [with_line mode a] is [a] with axis line mode [mode]. *)

  val with_title : ?style:Ansi.Style.t -> string -> t -> t
  (** [with_title text a] is [a] with title [text]. *)
end

(** {1:gridlines Gridlines} *)

module Gridlines : sig
  (** Background gridline configuration. *)

  type t = {
    show : bool;  (** Master visibility toggle. *)
    x : bool;  (** Vertical gridlines (at x-axis ticks). *)
    y : bool;  (** Horizontal gridlines (at y-axis ticks). *)
    style : Ansi.Style.t;  (** Major gridline style. *)
    pattern : Charset.line_pattern;  (** Gridline pattern. *)
    x_step : int option;
        (** Show every [n]th vertical gridline. [None] shows all. *)
    y_step : int option;
        (** Show every [n]th horizontal gridline. [None] shows all. *)
    minor : int option;
        (** Minor subdivisions between major gridlines. [None] disables minor
            gridlines. *)
    minor_style : Ansi.Style.t option;
        (** Minor gridline style. [None] inherits from theme. *)
  }

  val hidden : t
  (** Hidden gridlines: [show = false]. *)

  val default : t
  (** Default: visible, dotted, both axes, dimmed style. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s g] is [g] with major gridline style [s]. *)

  val with_pattern : Charset.line_pattern -> t -> t
  (** [with_pattern p g] is [g] with gridline pattern [p]. *)

  val with_x : bool -> t -> t
  (** [with_x b g] enables or disables vertical gridlines. *)

  val with_y : bool -> t -> t
  (** [with_y b g] enables or disables horizontal gridlines. *)

  val with_x_step : int option -> t -> t
  (** [with_x_step n g] is [g] showing every [n]th vertical gridline. [None]
      uses automatic spacing. *)

  val with_y_step : int option -> t -> t
  (** [with_y_step n g] is [g] showing every [n]th horizontal gridline. [None]
      uses automatic spacing. *)

  val with_minor : int option -> t -> t
  (** [with_minor n g] is [g] with [n] minor gridlines between major lines.
      [None] disables. *)

  val with_minor_style : Ansi.Style.t option -> t -> t
  (** [with_minor_style s g] is [g] with minor gridline style [s]. [None]
      inherits from major style. *)
end

(** {1:view View} *)

module View : sig
  (** Viewport windowing for zoom and pan.

      A view constrains which portion of the data domain is visible. Store a
      {!t} in your application model and pass it to {!Matrix_charts.draw}. Use
      {!empty} to show the full data extent.

      All window operations produce valid windows: [min] is always [< max], with
      a minimum span enforced to prevent degenerate ranges. *)

  type window = { min : float; max : float }
  (** The type for 1D ranges. Invariant: [min < max]. *)

  type t = { x : window option; y : window option; y2 : window option }
  (** The type for viewport state. [None] on any axis means "show full domain".
  *)

  val empty : t
  (** No constraints on any axis. *)

  val set_x : window option -> t -> t
  (** [set_x w v] is [v] with x-axis window [w]. *)

  val set_y : window option -> t -> t
  (** [set_y w v] is [v] with y-axis window [w]. *)

  val set_y2 : window option -> t -> t
  (** [set_y2 w v] is [v] with secondary y-axis window [w]. *)

  val window : min:float -> max:float -> window
  (** [window ~min ~max] is a window over \[[min], [max]\]. If [min > max] the
      values are swapped. A minimum span is enforced. *)

  val zoom : window -> factor:float -> window
  (** [zoom w ~factor] zooms around the centre of [w]. [factor > 1] zooms in
      (smaller range), [factor < 1] zooms out. Non-positive factors are treated
      as [1.0]. *)

  val zoom_around : window -> center:float -> factor:float -> window
  (** [zoom_around w ~center ~factor] zooms around [center], preserving its
      relative position within the window. *)

  val pan : window -> delta:float -> window
  (** [pan w ~delta] shifts [w] by [delta] data units. *)

  val clamp : domain:window -> window -> window
  (** [clamp ~domain w] restricts [w] to fit within [domain]. If [w] is wider
      than [domain], returns [domain]. Otherwise slides [w] to the nearest
      position that fits. *)
end

(** {1:raster Raster} *)

module Raster : sig
  (** Sub-cell resolution modes. *)

  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
  (** The type for sub-cell resolution modes.
      - [`Cell]: one character per data point (default).
      - [`Wave]: curved line segments using wave characters.
      - [`Block2x2]: 2×2 sub-cell resolution using block elements.
      - [`Braille2x4]: 2×4 sub-cell resolution using Braille patterns. Requires
        Braille-capable fonts. *)
end

(** {1:mark Mark} *)

module Mark : sig
  (** Chart marks (visual encodings of data).

      Marks are the graphical primitives rendered on a chart: lines, scatter
      points, bars, heatmaps, etc. Each constructor extracts data eagerly via
      accessor functions ([~x], [~y], etc.), so the source data can be garbage
      collected after mark creation.

      Marks without an explicit [~style] are auto-coloured from the theme
      palette in the order they are added to the chart. *)

  (** {2:types Types} *)

  type id = string
  (** The type for mark identifiers. When set, {!Hit.t.mark_id} carries this
      value. *)

  type t
  (** The type for marks. Construct with the functions below, then pass to
      {!Matrix_charts.add} or {!Matrix_charts.make}. *)

  type direction = [ `Vertical | `Horizontal ]
  (** The type for bar chart and rule directions.
      - [`Vertical]: bars grow upward; rules are horizontal.
      - [`Horizontal]: bars grow rightward; rules are vertical. *)

  type scatter_mode = [ `Cell | `Braille | `Density ]
  (** The type for scatter point rendering modes.
      - [`Cell]: one glyph per point.
      - [`Braille]: sub-cell resolution using Braille patterns.
      - [`Density]: heatmap-style density shading for overlapping points. *)

  type heatmap_agg = [ `Last | `Avg | `Max ]
  (** The type for heatmap cell aggregation. *)

  (** The type for heatmap rendering modes. *)
  type heatmap_mode =
    | Cells_fg  (** Foreground colour per cell. *)
    | Cells_bg  (** Background colour per cell. *)
    | Halfblock_fg_bg  (** Half-block characters for 2× vertical resolution. *)
    | Shaded  (** Shade characters from {!Charset.t.shade_levels}. *)
    | Dense_bilinear  (** Bilinear interpolation for smooth gradients. *)

  type bar_mode = [ `Cell | `Half_block ]
  (** The type for bar rendering granularity.
      - [`Cell]: one cell per unit.
      - [`Half_block]: half-block characters for sub-cell bar heights. *)

  type candle_body = [ `Filled | `Hollow ]
  (** The type for candlestick body styles. *)

  type candle_width = [ `One | `Two ]
  (** The type for candlestick widths in cells. *)

  type area_baseline = [ `Zero | `Value of float ]
  (** The type for area chart baselines. [`Zero] fills down to y=0; [`Value v]
      fills down to y=[v]. *)

  (** The type for histogram binning methods. *)
  type bin_method =
    | Bins of int  (** [n] equal-width bins. *)
    | Width of float  (** Bins of width [w]. *)
    | Edges of float array  (** Explicit bin edges. At least 2 elements. *)

  type histogram_normalize = [ `Count | `Density | `Probability ]
  (** The type for histogram normalisation.
      - [`Count]: raw frequency counts.
      - [`Density]: normalised so the integral equals 1.
      - [`Probability]: fraction of total observations per bin. *)

  type bar_segment = {
    value : float;
    style : Ansi.Style.t;
    label : string option;
  }
  (** The type for stacked bar segments. *)

  type stacked_bar = { category : string; segments : bar_segment list }
  (** The type for stacked bar entries. *)

  type ohlc = {
    time : float;  (** X-axis position (typically a timestamp). *)
    open_ : float;
    high : float;
    low : float;
    close : float;
  }
  (** The type for open-high-low-close data. *)

  type y_axis_selector = [ `Y1 | `Y2 ]
  (** The type for y-axis selectors. [`Y2] requires a secondary y-axis
      configured via {!Matrix_charts.with_y2_scale}. *)

  (** {2:constructors Constructors} *)

  val line :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?pattern:Charset.line_pattern ->
    ?glyph:string ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [line ~x ~y data] is a line mark connecting consecutive data points in
      array order. NaN y-values are skipped.
      - [resolution] defaults to [`Cell].
      - [pattern] defaults to [`Solid].
      - [glyph], when set, draws this glyph at each point instead of connecting
        lines.
      - [y_axis] defaults to [`Y1].

      See also {!line_gaps}. *)

  val line_gaps :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?pattern:Charset.line_pattern ->
    ?glyph:string ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float option) ->
    'a array ->
    t
  (** [line_gaps ~x ~y data] is like {!line} except the line breaks where [y]
      returns [None] and resumes at the next [Some] value. *)

  val scatter :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    ?mode:scatter_mode ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [scatter ~x ~y data] plots individual data points. [glyph] defaults to the
      charset's default point. [mode] defaults to [`Cell]. *)

  val bar :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?direction:direction ->
    ?mode:bar_mode ->
    category:('a -> string) ->
    value:('a -> float) ->
    'a array ->
    t
  (** [bar ~category ~value data] is a bar chart. Categories are placed on the
      band-scale axis; values determine bar length. Implicitly uses a
      {!Scale.Band} on the category axis and includes zero in the value domain.
      [direction] defaults to [`Vertical]. [mode] defaults to [`Half_block]. *)

  val stacked_bar :
    ?id:id ->
    ?direction:direction ->
    ?gap:int ->
    ?size:int ->
    ?mode:bar_mode ->
    stacked_bar array ->
    t
  (** [stacked_bar data] is stacked bars from pre-segmented data. Segments stack
      from the baseline upward (or leftward for horizontal).
      - [direction] defaults to [`Vertical].
      - [gap] is inter-bar spacing in cells. Defaults to [1]. Clamped to [>= 0].
      - [size] is explicit bar width in cells. [None] auto-sizes.
      - [mode] defaults to [`Half_block]. *)

  val rule :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?direction:direction ->
    ?pattern:Charset.line_pattern ->
    ?y_axis:y_axis_selector ->
    float ->
    t
  (** [rule value] is a reference line spanning the full plot area. [direction]
      defaults to [`Horizontal] (line at y=[value]). [pattern] defaults to
      [`Solid]. *)

  val heatmap :
    ?id:id ->
    ?color_scale:Ansi.Color.t array ->
    ?value_range:float * float ->
    ?auto_value_range:bool ->
    ?agg:heatmap_agg ->
    ?mode:heatmap_mode ->
    x:('a -> float) ->
    y:('a -> float) ->
    value:('a -> float) ->
    'a array ->
    t
  (** [heatmap ~x ~y ~value data] is a heatmap mapping [value] to colours.
      Multiple points in the same cell are combined using [agg].
      - [color_scale] is the colour gradient. Empty uses a default gradient.
      - [value_range] fixes the [(min, max)] for colour mapping. [None] infers
        from data.
      - [auto_value_range] defaults to [true]; computes the range from data when
        [value_range] is not set.
      - [agg] defaults to [`Last].
      - [mode] defaults to [Cells_fg]. *)

  val candles :
    ?id:id ->
    ?bullish:Ansi.Style.t ->
    ?bearish:Ansi.Style.t ->
    ?width:candle_width ->
    ?body:candle_body ->
    ?y_axis:y_axis_selector ->
    ohlc array ->
    t
  (** [candles data] is a candlestick chart from OHLC data, sorted by
      {!ohlc.time} internally. [bullish] (close >= open) defaults to green;
      [bearish] (close < open) defaults to red. [width] defaults to [`One].
      [body] defaults to [`Filled]. *)

  val circle :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    cx:('a -> float) ->
    cy:('a -> float) ->
    r:('a -> float) ->
    'a array ->
    t
  (** [circle ~cx ~cy ~r data] draws circles. Radius [r] is in data units. *)

  val shade :
    ?id:id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t
  (** [shade ~min ~max ()] is a vertical shaded region between x=[min] and
      x=[max]. If [min > max] the values are swapped. *)

  val column_bg : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [column_bg x] is a full-height background highlight at data x-coordinate
      [x]. *)

  val area :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?baseline:area_baseline ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y:('a -> float) ->
    'a array ->
    t
  (** [area ~x ~y data] is a filled area chart between the data line and
      [baseline]. [baseline] defaults to [`Zero]. *)

  val fill_between :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?resolution:Raster.resolution ->
    ?y_axis:y_axis_selector ->
    x:('a -> float) ->
    y_low:('a -> float) ->
    y_high:('a -> float) ->
    'a array ->
    t
  (** [fill_between ~x ~y_low ~y_high data] fills the region between two y-value
      curves. *)

  val histogram :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?bins:bin_method ->
    ?normalize:histogram_normalize ->
    x:('a -> float) ->
    'a array ->
    t
  (** [histogram ~x data] is a histogram from continuous data. Binning is
      computed eagerly at construction time. [bins] defaults to [Bins 10].
      [normalize] defaults to [`Count]. *)
end

(** {1:hit Hit testing} *)

module Hit : sig
  (** Hit-testing results for interactive charts.

      Use {!Layout.hit_test} to find the nearest data point or bar to a given
      cell coordinate, then inspect the returned {!t} for data-level
      information. *)

  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]
  (** The type for distance metrics.
      - [`Nearest_px]: Euclidean distance in cell space.
      - [`Nearest_x]: horizontal distance only.
      - [`Nearest_y]: vertical distance only. *)

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]
  (** The type of mark that was hit. *)

  (** Mark-specific data payload. *)
  type payload =
    | XY of { x : float; y : float }
        (** Point data from line, scatter, or circle marks. *)
    | Bar of { category : string; value : float }  (** Bar chart data. *)
    | Stacked_bar of {
        category : string;
        segment_index : int;
        value : float;
        total : float;
      }  (** Stacked bar data with segment detail. *)
    | Heat of { x : float; y : float; value : float }  (** Heatmap cell data. *)
    | OHLC of {
        time : float;
        open_ : float;
        high : float;
        low : float;
        close : float;
      }  (** Candlestick data. *)

  type t = {
    mark_id : string option;  (** The {!Mark.id} of the hit mark, if set. *)
    kind : kind;  (** Type of mark that was hit. *)
    index : int;  (** Index of the data point within the mark's data array. *)
    px : int;  (** Cell x-coordinate of the snapped hit. *)
    py : int;  (** Cell y-coordinate of the snapped hit. *)
    distance_px : float;
        (** Distance from query to hit in cell units. [0.0] for hits inside
            bars. *)
    payload : payload;  (** Data values at the hit point. *)
  }
  (** The type for hit-test results. *)
end

(** {1:layout Layout} *)

module Layout : sig
  (** Compiled chart layout for coordinate mapping, hit-testing, and
      interaction.

      A layout is produced by {!Matrix_charts.draw} or {!Matrix_charts.layout}
      and captures the computed coordinate mapping, plot region, and resolved
      scales. It bridges cell coordinates (from mouse/cursor input) and data
      coordinates (for tooltips, crosshairs, and snapping). *)

  type t
  (** The type for compiled layouts. *)

  type rect = { x : int; y : int; width : int; height : int }
  (** The type for axis-aligned rectangles in cell coordinates. *)

  (** {2:geometry Geometry} *)

  val size : t -> int * int
  (** [size l] is [(width, height)] of the full chart area. *)

  val plot_rect : t -> rect
  (** [plot_rect l] is the data plotting region, excluding axes, labels, and
      margins. *)

  val is_inside_plot : t -> px:int -> py:int -> bool
  (** [is_inside_plot l ~px ~py] is [true] iff [(px, py)] falls within the plot
      region. *)

  (** {2:domain Domain and view} *)

  val x_domain : t -> View.window
  (** [x_domain l] is the full x-axis data domain. *)

  val y_domain : t -> View.window
  (** [y_domain l] is the full y-axis data domain. *)

  val y2_domain : t -> View.window option
  (** [y2_domain l] is the secondary y-axis domain, or [None] if no secondary
      axis is configured. *)

  val x_view : t -> View.window
  (** [x_view l] is the currently visible x-axis range (may be a subset of the
      domain when zoomed). *)

  val y_view : t -> View.window
  (** [y_view l] is the currently visible y-axis range. *)

  val y2_view : t -> View.window option
  (** [y2_view l] is the currently visible secondary y-axis range. *)

  val y_axis_title_width : t -> int
  (** [y_axis_title_width l] is the width reserved for the y-axis title, in
      cells. *)

  val y2_axis_width : t -> int
  (** [y2_axis_width l] is the width reserved for the secondary y-axis, in
      cells. *)

  val has_y2 : t -> bool
  (** [has_y2 l] is [true] iff a secondary y-axis is active. *)

  (** {2:converting Coordinate conversion} *)

  val data_of_px : t -> px:int -> py:int -> (float * float) option
  (** [data_of_px l ~px ~py] converts cell coordinates to data coordinates.
      [None] if [(px, py)] is outside the plot region. *)

  val px_of_data : t -> x:float -> y:float -> int * int
  (** [px_of_data l ~x ~y] converts data coordinates to cell coordinates. Values
      outside the visible range are clamped to the plot boundary. *)

  (** {2:categories Category lookup} *)

  val x_category_of_px : t -> px:int -> string option
  (** [x_category_of_px l ~px] is the category at cell column [px] when the
      x-axis uses a band scale. [None] for non-band scales or out-of-range
      positions. *)

  val y_category_of_px : t -> py:int -> string option
  (** [y_category_of_px l ~py] is the category at cell row [py] when the y-axis
      uses a band scale. *)

  val px_of_x_category : t -> string -> int option
  (** [px_of_x_category l cat] is the centre cell column for category [cat].
      [None] if [cat] is not in the scale. *)

  val py_of_y_category : t -> string -> int option
  (** [py_of_y_category l cat] is the centre cell row for category [cat]. *)

  (** {2:view_manip View manipulation} *)

  type axis = [ `X | `Y | `Both ]
  (** The type for axis selectors in zoom/pan operations. *)

  val clamp_view : t -> View.t -> View.t
  (** [clamp_view l view] constrains [view] to the layout's data domain.
      Respects the [clamp] setting on each scale; band scales are unchanged. *)

  val zoom_view_around_px :
    t -> view:View.t -> axis:axis -> px:int -> py:int -> factor:float -> View.t
  (** [zoom_view_around_px l ~view ~axis ~px ~py ~factor] zooms [view] around
      the data point at cell position [(px, py)]. If the position is outside the
      plot, zooms around the centre. *)

  val pan_view_by_px : t -> view:View.t -> dx:int -> dy:int -> View.t
  (** [pan_view_by_px l ~view ~dx ~dy] pans [view] by [(dx, dy)] cells. The
      delta is converted to data units proportional to the current view range
      and plot size. *)

  val plot_center_px : t -> int * int
  (** [plot_center_px l] is the cell coordinates of the plot centre. *)

  val zoom_view_around_center :
    t -> view:View.t -> axis:axis -> factor:float -> View.t
  (** [zoom_view_around_center l ~view ~axis ~factor] zooms [view] around the
      plot centre.

      See also {!zoom_view_around_px}. *)

  (** {2:hit_testing Hit testing} *)

  val hit_test :
    ?radius:int -> ?policy:Hit.policy -> t -> px:int -> py:int -> Hit.t option
  (** [hit_test l ~px ~py] is the nearest data point to cell position
      [(px, py)], or [None] if [(px, py)] is outside the plot region or no point
      is within [radius]. [radius] defaults to [3]. [policy] defaults to
      [`Nearest_px]. *)
end

(** {1:overlay Overlay} *)

module Overlay : sig
  (** Interactive overlays drawn on top of the chart.

      Overlays mutate the {!Grid.t} directly. They require a {!Layout.t} to
      convert between data and cell coordinates. All position parameters ([~x],
      [~y]) are in data coordinates. *)

  val crosshair :
    ?style:Ansi.Style.t ->
    ?pattern:Charset.line_pattern ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    unit
  (** [crosshair layout grid ~x ~y] draws vertical and horizontal lines through
      [(x, y)], spanning the full plot area. [style] inherits from
      {!Theme.t.crosshair}. [pattern] defaults to [`Solid]. *)

  val marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    unit
  (** [marker layout grid ~x ~y] draws a single glyph at [(x, y)]. [style]
      inherits from {!Theme.t.marker}. [glyph] defaults to ["●"]. *)

  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]
  (** The type for tooltip placement.

      [`Auto] tries [`Right], [`Left], [`Top], [`Bottom] in order, choosing the
      position with the least clipping and no overlap with the anchor point. *)

  type tooltip_border = [ `Theme | `None | `Style of Ansi.Style.t ]
  (** The type for tooltip border styles.
      - [`Theme]: use the theme's tooltip border.
      - [`None]: no border.
      - [`Style s]: custom border style. *)

  val tooltip :
    ?style:Ansi.Style.t ->
    ?border:tooltip_border ->
    ?padding:int ->
    ?anchor:tooltip_anchor ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    string list ->
    unit
  (** [tooltip layout grid ~x ~y lines] draws a tooltip box anchored at [(x, y)]
      containing [lines]. The tooltip is clamped to the plot region. Does
      nothing if [(x, y)] maps outside the plot.
      - [style] inherits from {!Theme.t.tooltip}.
      - [border] defaults to [`Theme].
      - [padding] is interior padding in cells. Defaults to [1]. Clamped to
        [>= 0].
      - [anchor] defaults to [`Auto]. *)

  type h_anchor = [ `Left | `Center | `Right ]
  (** The type for horizontal text alignment. *)

  type v_anchor = [ `Top | `Middle | `Bottom ]
  (** The type for vertical text alignment. *)

  val text :
    ?style:Ansi.Style.t ->
    ?anchor:h_anchor ->
    ?v_anchor:v_anchor ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    string ->
    unit
  (** [text layout grid ~x ~y label] draws a text label at [(x, y)]. [style]
      inherits from {!Theme.t.labels}. [anchor] defaults to [`Left]. *)

  type arrow_head = [ `None | `Arrow | `Dot ]
  (** The type for arrow head styles.
      - [`None]: no head.
      - [`Arrow]: directional arrow.
      - [`Dot]: circular dot. *)

  val arrow :
    ?style:Ansi.Style.t ->
    ?head:arrow_head ->
    Layout.t ->
    Grid.t ->
    x1:float ->
    y1:float ->
    x2:float ->
    y2:float ->
    unit
  (** [arrow layout grid ~x1 ~y1 ~x2 ~y2] draws a line from [(x1, y1)] to
      [(x2, y2)] with an optional head at the endpoint. [style] inherits from
      {!Theme.t.labels}. [head] defaults to [`Arrow]. *)
end

(** {1:legend Legend} *)

module Legend : sig
  (** Chart legend rendering. *)

  type item = { label : string; style : Ansi.Style.t; marker : string }
  (** The type for legend entries: coloured [marker] glyph followed by [label]
      text. *)

  val draw :
    ?direction:[ `Horizontal | `Vertical ] ->
    ?gap:int ->
    item list ->
    Grid.t ->
    width:int ->
    height:int ->
    unit
  (** [draw items grid ~width ~height] renders legend items. [direction]
      defaults to [`Vertical]. [gap] defaults to [0] for vertical, [2] for
      horizontal. *)

  val items_of_layout : Layout.t -> item list
  (** [items_of_layout layout] extracts legend items from all marks that have a
      [label]. Mark types without legend support (rules, shades) are excluded.
  *)
end

(** {1:sparkline Sparkline} *)

module Sparkline = Sparkline
(** Compact sparkline charts. See {!Sparkline}. *)

(** {1:chart Chart specification} *)

type t
(** The type for immutable chart specifications. Build with {!empty} or {!make},
    configure with [with_*] functions, add marks with {!add}, then render with
    {!draw}. *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
(** The type for manual frame configurations. [margins] is
    [(top, right, bottom, left)] in cells. *)

(** The type for frame modes. *)
type frame =
  | Auto  (** Compute margins from axis/label sizes. *)
  | Manual of frame_config  (** Explicit margins and padding. *)

type title = { text : string; style : Ansi.Style.t option }
(** The type for chart titles. *)

val default_frame : frame
(** [default_frame] is [Auto]. *)

val manual_frame :
  ?margins:int * int * int * int -> ?inner_padding:int -> unit -> frame
(** [manual_frame ()] is a {!Manual} frame. [margins] defaults to
    [(0, 0, 0, 0)]. [inner_padding] defaults to [0], clamped to [>= 0]. *)

(** {2:constructors Constructors} *)

val empty : ?theme:Theme.t -> unit -> t
(** [empty ()] is an empty chart with no marks. Gridlines are hidden by default;
    use {!with_grid} to enable them. [theme] defaults to {!Theme.default}. *)

val make : ?theme:Theme.t -> ?title:string -> Mark.t list -> t
(** [make marks] is a chart from [marks]. Marks render in list order (first mark
    is drawn first, last is on top). [theme] defaults to {!Theme.default}. *)

(** {2:config Configuration} *)

val with_theme : Theme.t -> t -> t
(** [with_theme theme t] is [t] with the given theme. Axis and gridline styles
    are updated to inherit from the new theme. *)

val with_frame : frame -> t -> t
(** [with_frame frame t] is [t] with the given frame mode. *)

val with_title : ?style:Ansi.Style.t -> string -> t -> t
(** [with_title text t] is [t] with chart title [text]. *)

val with_x_scale : Scale.t -> t -> t
(** [with_x_scale scale t] is [t] with x-axis scale [scale]. *)

val with_y_scale : Scale.t -> t -> t
(** [with_y_scale scale t] is [t] with primary y-axis scale [scale]. *)

val with_y2_scale : Scale.t -> t -> t
(** [with_y2_scale scale t] enables and sets the secondary y-axis scale. *)

val with_axes : ?x:Axis.t -> ?y:Axis.t -> t -> t
(** [with_axes t] configures the primary axes. Only provided axes are replaced;
    omitted axes keep their current configuration. *)

val with_y2_axis : Axis.t -> t -> t
(** [with_y2_axis axis t] enables and configures the secondary y-axis. *)

val with_grid : Gridlines.t -> t -> t
(** [with_grid g t] is [t] with background gridlines [g]. *)

val add : Mark.t -> t -> t
(** [add mark t] appends [mark] to the chart. Marks render in add order (later
    marks draw on top). *)

(** {2:mark_wrappers Mark convenience wrappers}

    Shorthand for [add (Mark.xxx ...) t], enabling pipeline-style chart
    construction. *)

val line :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?pattern:Charset.line_pattern ->
  ?glyph:string ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [line ~x ~y data t] is [add (Mark.line ~x ~y data) t]. See {!Mark.line}. *)

val line_gaps :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?pattern:Charset.line_pattern ->
  ?glyph:string ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float option) ->
  'a array ->
  t ->
  t
(** [line_gaps ~x ~y data t] is [add (Mark.line_gaps ~x ~y data) t]. See
    {!Mark.line_gaps}. *)

val scatter :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?glyph:string ->
  ?mode:Mark.scatter_mode ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [scatter ~x ~y data t] is [add (Mark.scatter ~x ~y data) t]. See
    {!Mark.scatter}. *)

val bar :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?direction:Mark.direction ->
  ?mode:Mark.bar_mode ->
  category:('a -> string) ->
  value:('a -> float) ->
  'a array ->
  t ->
  t
(** [bar ~category ~value data t] is [add (Mark.bar ~category ~value data) t].
    See {!Mark.bar}. *)

val stacked_bar :
  ?id:Mark.id ->
  ?direction:Mark.direction ->
  ?gap:int ->
  ?size:int ->
  ?mode:Mark.bar_mode ->
  Mark.stacked_bar array ->
  t ->
  t
(** [stacked_bar data t] is [add (Mark.stacked_bar data) t]. See
    {!Mark.stacked_bar}. *)

val rule :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?direction:Mark.direction ->
  ?pattern:Charset.line_pattern ->
  ?y_axis:Mark.y_axis_selector ->
  float ->
  t ->
  t
(** [rule value t] is [add (Mark.rule value) t]. See {!Mark.rule}. *)

val heatmap :
  ?id:Mark.id ->
  ?color_scale:Ansi.Color.t array ->
  ?value_range:float * float ->
  ?auto_value_range:bool ->
  ?agg:Mark.heatmap_agg ->
  ?mode:Mark.heatmap_mode ->
  x:('a -> float) ->
  y:('a -> float) ->
  value:('a -> float) ->
  'a array ->
  t ->
  t
(** [heatmap ~x ~y ~value data t] is [add (Mark.heatmap ~x ~y ~value data) t].
    See {!Mark.heatmap}. *)

val candles :
  ?id:Mark.id ->
  ?bullish:Ansi.Style.t ->
  ?bearish:Ansi.Style.t ->
  ?width:Mark.candle_width ->
  ?body:Mark.candle_body ->
  ?y_axis:Mark.y_axis_selector ->
  Mark.ohlc array ->
  t ->
  t
(** [candles data t] is [add (Mark.candles data) t]. See {!Mark.candles}. *)

val circle :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
  cx:('a -> float) ->
  cy:('a -> float) ->
  r:('a -> float) ->
  'a array ->
  t ->
  t
(** [circle ~cx ~cy ~r data t] is [add (Mark.circle ~cx ~cy ~r data) t]. See
    {!Mark.circle}. *)

val shade :
  ?id:Mark.id -> ?style:Ansi.Style.t -> min:float -> max:float -> t -> t
(** [shade ~min ~max t] is [add (Mark.shade ~min ~max ()) t]. See {!Mark.shade}.
*)

val column_bg : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [column_bg x t] is [add (Mark.column_bg x) t]. See {!Mark.column_bg}. *)

val area :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?baseline:Mark.area_baseline ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y:('a -> float) ->
  'a array ->
  t ->
  t
(** [area ~x ~y data t] is [add (Mark.area ~x ~y data) t]. See {!Mark.area}. *)

val fill_between :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?resolution:Raster.resolution ->
  ?y_axis:Mark.y_axis_selector ->
  x:('a -> float) ->
  y_low:('a -> float) ->
  y_high:('a -> float) ->
  'a array ->
  t ->
  t
(** [fill_between ~x ~y_low ~y_high data t] is
    [add (Mark.fill_between ~x ~y_low ~y_high data) t]. See
    {!Mark.fill_between}. *)

val histogram :
  ?id:Mark.id ->
  ?label:string ->
  ?style:Ansi.Style.t ->
  ?bins:Mark.bin_method ->
  ?normalize:Mark.histogram_normalize ->
  x:('a -> float) ->
  'a array ->
  t ->
  t
(** [histogram ~x data t] is [add (Mark.histogram ~x data) t]. See
    {!Mark.histogram}. *)

(** {1:rendering Rendering} *)

val layout :
  ?view:View.t -> ?x:int -> ?y:int -> t -> width:int -> height:int -> Layout.t
(** [layout t ~width ~height] compiles [t] into a {!Layout.t} without rendering.
    Use this when you need layout information (coordinate mapping, hit-testing)
    without drawing.
    - [view] defaults to {!View.empty}.
    - [x] and [y] are horizontal and vertical offsets. Default [0].
    - [width] and [height] are clamped to [>= 1]. *)

val draw :
  ?view:View.t ->
  ?x:int ->
  ?y:int ->
  t ->
  Grid.t ->
  width:int ->
  height:int ->
  Layout.t
(** [draw t grid ~width ~height] renders [t] to [grid] and returns the compiled
    layout.

    Fills the chart region with the theme background, then draws gridlines,
    marks (in add order), axes, and title. The returned {!Layout.t} can be used
    for hit-testing and overlay drawing.
    - [view] defaults to {!View.empty}.
    - [x] and [y] are horizontal and vertical offsets. Default [0].
    - [width] and [height] are clamped to [>= 1]. *)
