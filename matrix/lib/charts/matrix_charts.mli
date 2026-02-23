(** Terminal charts rendered to a {!Grid.t}.

    Matrix_charts renders line, scatter, bar, heatmap, candlestick, and other
    chart types into a terminal grid. Charts are immutable specifications built
    from composable {!Mark.t} values, compiled into a {!Layout.t} for drawing
    and interaction.

    {1 Quick Start}

    Create a chart from a list of marks and draw it:
    {[
      let data = [| (0., 10.); (1., 20.); (2., 15.) |] in
      let chart = make [ Mark.line ~x:fst ~y:snd data ] in
      let _layout = draw chart grid ~width:80 ~height:24
    ]}

    Pipeline style with {!add}:
    {[
      let chart =
        empty ()
        |> add (Mark.line ~x:fst ~y:snd data)
        |> add (Mark.scatter ~x:fst ~y:snd data)
        |> with_axes ~x:Axis.default ~y:Axis.default
      in
      draw chart grid ~width:80 ~height:24
    ]}

    Zoom and pan with {!View.t}:
    {[
      let view =
        View.empty |> View.set_x (Some (View.window ~min:0. ~max:10.))
      in
      draw ~view chart grid ~width:80 ~height:24
    ]}

    Add tooltips on hover using the returned {!Layout.t}:
    {[
      match Layout.hit_test layout ~px ~py with
      | Some { Hit.payload = Hit.XY { x; y }; _ } ->
          Overlay.tooltip layout grid ~x ~y [ "Value: " ^ string_of_float y ]
      | Some { payload = Hit.Bar { category; value }; _ } ->
          Overlay.tooltip layout grid ~x:0. ~y:value
            [ category ^ ": " ^ string_of_float value ]
      | _ -> ()
    ]}

    {1 Architecture}

    The typical workflow is:

    + Build an immutable {!t} by layering marks (lines, bars, scatter, etc.).
    + Keep viewport state in a separate {!View.t} (zoom and pan belong in your
      model, not the chart).
    + Call {!draw} which compiles a {!Layout.t} and renders to a {!Grid.t}.
    + Use {!Layout} + {!Hit} + {!Overlay} for hover tooltips, crosshairs,
      snapping, and zoom-to-cursor.

    {1 Coordinate Systems}

    {2 Cell coordinates ([px], [py])}

    Integer terminal-cell positions. [(0, 0)] is the top-left corner of the
    grid.

    {2 Data coordinates ([x], [y])}

    Floating-point values in the data domain. Convert between systems using
    {!Layout.data_of_px} and {!Layout.px_of_data}.

    {2 Sub-cell rendering}

    Some marks support sub-cell resolution (e.g., [`Braille2x4] renders 2x4 dots
    per cell). Sub-cell rendering is purely visual -- all coordinates in the API
    remain in cell units. *)

module Charset : sig
  (** Character sets for chart rendering.

      Each preset provides a complete set of glyphs for frames, axes, ticks,
      gridlines, data points, and tooltips. Use {!default} (Unicode light) for
      most terminals, or {!ascii} for maximum compatibility. *)

  type line_pattern = [ `Solid | `Dashed | `Dotted ]
  (** Line rendering pattern for gridlines, rules, and crosshairs. *)

  type frame = {
    tl : string;  (** Top-left corner. *)
    tr : string;  (** Top-right corner. *)
    bl : string;  (** Bottom-left corner. *)
    br : string;  (** Bottom-right corner. *)
    h : string;  (** Horizontal line segment. *)
    v : string;  (** Vertical line segment. *)
    tee_up : string;  (** Upward tee junction. *)
    tee_down : string;  (** Downward tee junction. *)
    tee_left : string;  (** Leftward tee junction. *)
    tee_right : string;  (** Rightward tee junction. *)
    cross : string;  (** Cross junction. *)
  }
  (** Box-drawing characters for borders, frames, and tooltip outlines. *)

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
    tooltip_frame : frame;  (** Frame characters for tooltip borders. *)
    diag_up : string;  (** Upward diagonal (e.g., [/] or [╱]). *)
    diag_down : string;  (** Downward diagonal (e.g., [╲]). *)
  }
  (** A complete character set specification for all chart elements. *)

  val ascii : t
  (** ASCII-only characters ([+], [-], [|], [*], [#]). Use for terminals without
      Unicode support. *)

  val unicode_light : t
  (** Light Unicode box-drawing characters (e.g., [─], [│], [┌]). *)

  val unicode_heavy : t
  (** Heavy Unicode box-drawing characters (e.g., [━], [┃], [┏]). *)

  val unicode_rounded : t
  (** Rounded Unicode box-drawing characters (e.g., [╭], [╮], [╰]). *)

  val default : t
  (** Alias for {!unicode_light}. *)
end

module Theme : sig
  (** Color and style themes for charts.

      A theme controls the color palette used for auto-styling marks, background
      color, and styles for axes, gridlines, labels, tooltips, crosshairs, and
      markers. It also embeds a {!Charset.t} for glyph selection. *)

  type t = {
    palette : Ansi.Color.t array;
        (** Color cycle for marks without explicit styles. Marks are assigned
            colors sequentially from this array, wrapping around. *)
    background : Ansi.Color.t option;
        (** Plot background. [None] leaves the grid background unchanged. *)
    axes : Ansi.Style.t;  (** Style for axis lines and tick marks. *)
    border : Ansi.Style.t;  (** Style for chart frame/border. *)
    grid : Ansi.Style.t;  (** Style for major gridlines. *)
    grid_minor : Ansi.Style.t;  (** Style for minor gridlines. *)
    labels : Ansi.Style.t;  (** Style for axis labels and titles. *)
    tooltip : Ansi.Style.t;  (** Style for tooltip text and background. *)
    tooltip_border : Ansi.Style.t option;
        (** Tooltip border style. [None] draws no border. *)
    crosshair : Ansi.Style.t;  (** Style for crosshair lines. *)
    marker : Ansi.Style.t;  (** Style for highlight markers. *)
    charset : Charset.t;  (** Character set for rendering glyphs. *)
  }

  val dark : t
  (** Dark theme optimized for dark terminal backgrounds. *)

  val light : t
  (** Light theme optimized for light terminal backgrounds. *)

  val default : t
  (** Alias for {!dark}. *)

  val with_charset : Charset.t -> t -> t
  (** [with_charset cs theme] returns [theme] with character set [cs]. *)
end

module Label_format : sig
  (** Axis label formatters.

      Each formatter has the signature [int -> float -> string] matching
      {!Axis.formatter}. The [int] parameter is the tick index (0-based), and
      the [float] is the tick value. *)

  val float : ?precision:int -> unit -> int -> float -> string
  (** [float ?precision ()] formats values using [%.*g].

      @param precision Significant digits. Default is [3]. *)

  val mmdd_utc : int -> float -> string
  (** [mmdd_utc] formats Unix timestamps as [MM/DD] in UTC. *)

  val hhmmss_utc : int -> float -> string
  (** [hhmmss_utc] formats Unix timestamps as [HH:MM:SS] in UTC. *)
end

module Transform : sig
  (** Data smoothing and transformation utilities.

      All transforms operate on [(x, y)] arrays, preserving x-values and
      smoothing y-values. The input array is not modified; a new array is
      returned. Empty input arrays produce empty output arrays. *)

  val ema : float -> (float * float) array -> (float * float) array
  (** [ema alpha data] computes an exponential moving average.

      [alpha] is the smoothing factor in [[0, 1]]. Higher values weight recent
      observations more heavily. *)

  val sma : int -> (float * float) array -> (float * float) array
  (** [sma window data] computes a simple moving average.

      [window] is the number of points to average. For indices before the window
      is full, a partial average is used. Returns an empty array if
      [window <= 0]. *)

  val gaussian : float -> (float * float) array -> (float * float) array
  (** [gaussian sigma data] applies Gaussian kernel smoothing.

      [sigma] controls the smoothing width. The kernel radius is
      [ceil(3 * sigma)]. Returns [data] unchanged if [sigma <= 0]. *)
end

module Scale : sig
  (** Axis scaling strategies.

      Scales control how data values map to pixel positions. Use {!Scale.Auto}
      (the default) to let the library infer the appropriate scale from the
      marks. For categorical data (bars), use {!band}. For data spanning
      multiple orders of magnitude, use {!log}. *)

  type numeric_domain = [ `Auto | `Domain of float * float ]
  (** Domain specification. [`Auto] infers bounds from the data. [`Domain] fixes
      the bounds explicitly. *)

  type t =
    | Auto
        (** Infer scale type from marks. Selects {!Band} when bar marks are
            present, {!Numeric} otherwise. *)
    | Numeric of { domain : numeric_domain; clamp : bool }
        (** Linear numeric scale. When [clamp] is [true], values outside the
            domain are clamped to the domain boundary. *)
    | Log of { base : float; domain : numeric_domain; clamp : bool }
        (** Logarithmic scale. Positive values only; values [<= 0] are clamped
            to [1e-10]. *)
    | Band of { categories : string list option; padding : float }
        (** Categorical band scale. Each category gets an equal-width band.
            [padding] (\[0, 0.95\]) controls inter-band spacing as a fraction of
            the total extent. *)

  val numeric : ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [numeric ()] creates a linear numeric scale.

      @param domain Default is [`Auto].
      @param clamp Default is [true]. *)

  val log : ?base:float -> ?domain:numeric_domain -> ?clamp:bool -> unit -> t
  (** [log ()] creates a logarithmic scale.

      @param base
        Logarithm base. Must be [> 1]; values [<= 1] default to [10]. Default is
        [10.0].
      @param domain Default is [`Auto].
      @param clamp Default is [true]. *)

  val band : ?categories:string list -> ?padding:float -> unit -> t
  (** [band ()] creates a categorical band scale.

      @param categories Explicit category order. [None] infers from marks.
      @param padding
        Inter-band padding as a fraction of total extent. Clamped to
        [[0, 0.95]]. Default is [0.1]. *)
end

module Axis : sig
  (** Axis rendering configuration.

      Controls visibility, tick marks, labels, and styling for a single axis.
      Use {!default} as a starting point, then customize with the [with_*]
      functions. Use {!hidden} to suppress an axis entirely. *)

  type formatter = int -> float -> string
  (** Tick label formatter. Receives the tick index (0-based) and the tick
      value, and returns the label string. See {!Label_format} for built-in
      formatters. *)

  type line = [ `None | `Axis_only | `Frame ]
  (** Axis line rendering mode.

      - [`None]: No axis line.
      - [`Axis_only]: Draw the axis line only.
      - [`Frame]: Draw a full frame around the plot area. *)

  type title = { text : string; style : Ansi.Style.t option }
  (** Axis title with optional style override. *)

  type t = {
    show : bool;  (** Whether to render this axis at all. *)
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
  (** [with_ticks n axis] sets the target tick count. Clamped to [>= 0]. *)

  val with_format : formatter -> t -> t
  (** [with_format fmt axis] sets the tick label formatter. *)

  val with_style : Ansi.Style.t -> t -> t
  (** [with_style s axis] sets the axis line style. *)

  val with_tick_style : Ansi.Style.t -> t -> t
  (** [with_tick_style s axis] sets the tick mark style. *)

  val with_label_style : Ansi.Style.t -> t -> t
  (** [with_label_style s axis] sets the tick label style. *)

  val with_tick_length : int -> t -> t
  (** [with_tick_length n axis] sets the tick mark length. Clamped to [>= 0]. *)

  val with_label_padding : int -> t -> t
  (** [with_label_padding n axis] sets label padding. Clamped to [>= 0]. *)

  val with_line : line -> t -> t
  (** [with_line mode axis] sets the axis line rendering mode. *)

  val with_title : ?style:Ansi.Style.t -> string -> t -> t
  (** [with_title text axis] sets the axis title. *)
end

module Gridlines : sig
  (** Background gridline configuration.

      Controls visibility, pattern, and spacing of horizontal and vertical
      gridlines behind the chart data. Minor gridlines can be enabled for
      additional subdivision. *)

  type t = {
    show : bool;  (** Master visibility toggle. *)
    x : bool;  (** Show vertical gridlines (at x-axis ticks). *)
    y : bool;  (** Show horizontal gridlines (at y-axis ticks). *)
    style : Ansi.Style.t;  (** Major gridline style. *)
    pattern : Charset.line_pattern;  (** Gridline pattern. *)
    x_step : int option;
        (** Show every [n]th vertical gridline. [None] shows all. *)
    y_step : int option;
        (** Show every [n]th horizontal gridline. [None] shows all. *)
    minor : int option;
        (** Number of minor subdivisions between major gridlines. [None]
            disables minor gridlines. *)
    minor_style : Ansi.Style.t option;
        (** Minor gridline style. [None] inherits from theme. *)
  }

  val hidden : t
  (** Hidden gridlines: [show = false]. *)

  val default : t
  (** Default: visible, dotted pattern, both axes, dimmed style. *)

  val with_style : Ansi.Style.t -> t -> t
  (** Set the major gridline style. *)

  val with_pattern : Charset.line_pattern -> t -> t
  (** Set the gridline pattern (e.g. [Dotted], [Dashed]). *)

  val with_x : bool -> t -> t
  (** Enable or disable vertical gridlines. *)

  val with_y : bool -> t -> t
  (** Enable or disable horizontal gridlines. *)

  val with_x_step : int option -> t -> t
  (** Override x-axis major gridline step. [None] uses automatic spacing. *)

  val with_y_step : int option -> t -> t
  (** Override y-axis major gridline step. [None] uses automatic spacing. *)

  val with_minor : int option -> t -> t
  (** Set the number of minor gridlines between major lines. [None] disables. *)

  val with_minor_style : Ansi.Style.t option -> t -> t
  (** Set the minor gridline style. [None] inherits from major style. *)
end

module View : sig
  (** Viewport windowing for zoom and pan.

      A view constrains which portion of the data domain is visible. Store a
      {!View.t} in your application model and pass it to {!draw} or {!layout}.
      Use {!View.empty} to show the full data extent.

      All window operations produce valid windows: [min] is always [< max], with
      a minimum span enforced to prevent degenerate ranges. *)

  type window = { min : float; max : float }
  (** A 1D range. Invariant: [min < max]. *)

  type t = { x : window option; y : window option; y2 : window option }
  (** Viewport state. [None] on any axis means "show full domain". *)

  val empty : t
  (** No constraints on any axis. Equivalent to showing the full domain. *)

  val set_x : window option -> t -> t
  (** Set the x-axis viewport window. *)

  val set_y : window option -> t -> t
  (** Set the y-axis viewport window. *)

  val set_y2 : window option -> t -> t
  (** Set the secondary y-axis viewport window. *)

  val window : min:float -> max:float -> window
  (** [window ~min ~max] creates a window. If [min > max], the values are
      swapped. A minimum span is enforced to prevent degenerate ranges. *)

  val zoom : window -> factor:float -> window
  (** [zoom w ~factor] zooms around the center of [w]. [factor > 1] zooms in
      (smaller range), [factor < 1] zooms out. Non-positive factors are treated
      as [1.0]. *)

  val zoom_around : window -> center:float -> factor:float -> window
  (** [zoom_around w ~center ~factor] zooms around [center], preserving its
      relative position within the window. *)

  val pan : window -> delta:float -> window
  (** [pan w ~delta] shifts the window by [delta] in data units. *)

  val clamp : domain:window -> window -> window
  (** [clamp ~domain w] restricts [w] to fit within [domain]. If [w] is wider
      than [domain], returns [domain]. Otherwise, slides [w] to the nearest
      position that fits. *)
end

module Raster : sig
  (** Sub-cell resolution modes for line, area, circle, and fill marks. *)

  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
  (** - [`Cell]: One character per data point. Default.
      - [`Wave]: Curved line segments using wave characters.
      - [`Block2x2]: 2x2 sub-cell resolution using block elements.
      - [`Braille2x4]: 2x4 sub-cell resolution using Braille patterns. Highest
        fidelity but requires Braille-capable fonts. *)
end

module Mark : sig
  (** Chart marks (visual encodings of data).

      Marks are the graphical primitives rendered on a chart -- lines, scatter
      points, bars, heatmaps, etc. Each constructor extracts data eagerly at
      construction time via accessor functions ([~x], [~y], etc.), so the source
      data can be garbage collected after mark creation.

      Marks without an explicit [~style] are auto-colored from the theme palette
      in the order they are added to the chart. *)

  type id = string
  (** Optional mark identifier for hit-testing. When set, {!Hit.t.mark_id}
      carries this value. *)

  type t
  (** An opaque mark value. Construct with the functions below, then pass to
      {!Matrix_charts.add} or {!Matrix_charts.make}. *)

  type direction = [ `Vertical | `Horizontal ]
  (** Direction for bar charts and rules.

      - [`Vertical]: Bars grow upward from the baseline; rules are horizontal.
      - [`Horizontal]: Bars grow rightward; rules are vertical. *)

  type scatter_mode = [ `Cell | `Braille | `Density ]
  (** Scatter point rendering mode.

      - [`Cell]: One glyph per data point.
      - [`Braille]: Sub-cell resolution using Braille patterns.
      - [`Density]: Heatmap-style density shading for overlapping points. *)

  type heatmap_agg = [ `Last | `Avg | `Max ]
  (** Aggregation for multiple values in the same heatmap cell. *)

  (** Heatmap rendering mode. *)
  type heatmap_mode =
    | Cells_fg  (** Foreground color per cell. *)
    | Cells_bg  (** Background color per cell. *)
    | Halfblock_fg_bg  (** Half-block characters for 2x vertical resolution. *)
    | Shaded  (** Shade characters from {!Charset.t.shade_levels}. *)
    | Dense_bilinear  (** Bilinear interpolation for smooth gradients. *)

  type bar_mode = [ `Cell | `Half_block ]
  (** Bar rendering granularity.

      - [`Cell]: One cell per unit.
      - [`Half_block]: Half-block characters for sub-cell bar heights. *)

  type candle_body = [ `Filled | `Hollow ]
  (** Candlestick body style. [`Filled] uses solid blocks; [`Hollow] uses
      outline characters. *)

  type candle_width = [ `One | `Two ]
  (** Candlestick width in cells. *)

  type area_baseline = [ `Zero | `Value of float ]
  (** Baseline for area charts. [`Zero] fills down to y=0; [`Value v] fills down
      to y=[v]. *)

  type bin_method =
    | Bins of int
    | Width of float
    | Edges of float array
        (** Histogram binning method.

            - [Bins n]: Use [n] equal-width bins.
            - [Width w]: Use bins of width [w].
            - [Edges arr]: Use explicit bin edges. Must have at least 2
              elements. *)

  type histogram_normalize = [ `Count | `Density | `Probability ]
  (** Histogram normalization.

      - [`Count]: Raw frequency counts.
      - [`Density]: Normalized so the integral over bins equals 1.
      - [`Probability]: Each bin shows the fraction of total observations. *)

  type bar_segment = {
    value : float;
    style : Ansi.Style.t;
    label : string option;
  }
  (** A single segment in a stacked bar. *)

  type stacked_bar = { category : string; segments : bar_segment list }
  (** A stacked bar entry with a category label and ordered segments. *)

  type ohlc = {
    time : float;  (** X-axis position (typically a timestamp). *)
    open_ : float;
    high : float;
    low : float;
    close : float;
  }
  (** Open-high-low-close data for candlestick charts. *)

  type y_axis_selector = [ `Y1 | `Y2 ]
  (** Selects which y-axis a mark is associated with. [`Y2] requires a secondary
      y-axis configured via {!Matrix_charts.with_y2_scale} and
      {!Matrix_charts.with_y2_axis}. *)

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
  (** [line ~x ~y data] creates a line mark connecting consecutive data points.

      Points are connected in array order. NaN y-values are skipped (use
      {!line_gaps} for explicit gap control).

      @param resolution Sub-cell resolution mode. Default is [`Cell].
      @param pattern Line pattern. Default is [`Solid].
      @param glyph
        When set, draws this glyph at each point instead of connecting lines.
      @param y_axis Default is [`Y1]. *)

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
  (** [line_gaps ~x ~y data] creates a line with explicit gaps where [y] returns
      [None]. The line breaks at gaps and resumes at the next [Some] value. *)

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
  (** [scatter ~x ~y data] plots individual data points.

      @param glyph Custom point glyph. Default depends on the charset.
      @param mode Rendering mode. Default is [`Cell]. *)

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
  (** [bar ~category ~value data] creates a bar chart.

      Categories are placed on the band-scale axis; values determine bar length.
      Implicitly uses a {!Scale.Band} on the category axis and includes zero in
      the value domain.

      @param direction
        Default is [`Vertical] (categories on x-axis, bars grow upward).
      @param mode Default is [`Half_block]. *)

  val stacked_bar :
    ?id:id ->
    ?direction:direction ->
    ?gap:int ->
    ?size:int ->
    ?mode:bar_mode ->
    stacked_bar array ->
    t
  (** [stacked_bar data] creates stacked bars from pre-segmented data.

      Each {!stacked_bar} entry specifies a category and its segments. Segments
      stack from the baseline upward (or leftward for horizontal).

      @param direction Default is [`Vertical].
      @param gap
        Spacing between bars in cells. Default is [1]. Clamped to [>= 0].
      @param size
        Explicit bar width/height in cells. [None] auto-sizes based on available
        space.
      @param mode Default is [`Half_block]. *)

  val rule :
    ?id:id ->
    ?style:Ansi.Style.t ->
    ?direction:direction ->
    ?pattern:Charset.line_pattern ->
    ?y_axis:y_axis_selector ->
    float ->
    t
  (** [rule value] draws a reference line spanning the full plot area.

      @param direction
        Default is [`Horizontal] (horizontal line at y=[value]). [`Vertical]
        draws a vertical line at x=[value].
      @param pattern Line pattern. Default is [`Solid]. *)

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
  (** [heatmap ~x ~y ~value data] creates a heatmap.

      Maps [value] to colors. Multiple points in the same cell are combined
      using [agg].

      @param color_scale Color gradient array. Empty uses a default gradient.
      @param value_range
        Fixed [(min, max)] for color mapping. [None] infers from data.
      @param auto_value_range
        When [true] (the default), computes the range from data when
        [value_range] is not set.
      @param agg Aggregation for colliding cells. Default is [`Last].
      @param mode Rendering mode. Default is [Cells_fg]. *)

  val candles :
    ?id:id ->
    ?bullish:Ansi.Style.t ->
    ?bearish:Ansi.Style.t ->
    ?width:candle_width ->
    ?body:candle_body ->
    ?y_axis:y_axis_selector ->
    ohlc array ->
    t
  (** [candles data] creates a candlestick chart from OHLC data.

      Data is sorted by {!ohlc.time} internally. Bullish candles (close >= open)
      and bearish candles (close < open) are styled separately.

      @param bullish Style for bullish candles. Default is green.
      @param bearish Style for bearish candles. Default is red.
      @param width Default is [`One].
      @param body Default is [`Filled]. *)

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
  (** [circle ~cx ~cy ~r data] draws circles.

      Radius [r] is in data units. *)

  val shade :
    ?id:id -> ?style:Ansi.Style.t -> min:float -> max:float -> unit -> t
  (** [shade ~min ~max ()] creates a vertical shaded region between x=[min] and
      x=[max]. If [min > max], the values are swapped. Useful for highlighting
      date ranges or intervals. *)

  val column_bg : ?id:id -> ?style:Ansi.Style.t -> float -> t
  (** [column_bg x] highlights the full-height column at data x-coordinate [x].
  *)

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
  (** [area ~x ~y data] creates a filled area chart.

      Fills the region between the data line and [baseline].

      @param baseline Default is [`Zero]. *)

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
      curves. Useful for confidence intervals or range bands. *)

  val histogram :
    ?id:id ->
    ?label:string ->
    ?style:Ansi.Style.t ->
    ?bins:bin_method ->
    ?normalize:histogram_normalize ->
    x:('a -> float) ->
    'a array ->
    t
  (** [histogram ~x data] creates a histogram from continuous data.

      Binning is computed eagerly at construction time.

      @param bins Binning method. Default is [Bins 10].
      @param normalize Normalization mode. Default is [`Count]. *)
end

module Hit : sig
  (** Hit-testing results for interactive charts.

      Hit-testing finds the nearest data point or bar to a given cell
      coordinate. Use {!Layout.hit_test} to perform a test, then inspect the
      returned {!Hit.t} for data-level information. *)

  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]
  (** Distance metric for finding the nearest hit.

      - [`Nearest_px]: Euclidean distance in cell space.
      - [`Nearest_x]: Horizontal distance only.
      - [`Nearest_y]: Vertical distance only. *)

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]
  (** The type of mark that was hit. *)

  (** Mark-specific data payload for the hit point. *)
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
    px : int;  (** Cell x-coordinate of the snapped hit point. *)
    py : int;  (** Cell y-coordinate of the snapped hit point. *)
    distance_px : float;
        (** Distance from query point to hit point in cell units. [0.0] for hits
            inside bars. *)
    payload : payload;  (** Data values at the hit point. *)
  }
  (** A hit-test result. *)
end

module Layout : sig
  (** Compiled chart layout for coordinate mapping, hit-testing, and
      interaction.

      A layout is produced by {!Matrix_charts.draw} or {!Matrix_charts.layout}
      and captures the computed coordinate mapping, plot region, and resolved
      scales. It is the bridge between cell coordinates (from mouse/cursor
      input) and data coordinates (for tooltips, crosshairs, and snapping). *)

  type t
  (** An opaque compiled layout. *)

  type rect = { x : int; y : int; width : int; height : int }
  (** An axis-aligned rectangle in cell coordinates. *)

  (** {2 Geometry} *)

  val size : t -> int * int
  (** [size layout] returns [(width, height)] of the full chart area. *)

  val plot_rect : t -> rect
  (** [plot_rect layout] returns the data plotting region, excluding axes,
      labels, and margins. *)

  val is_inside_plot : t -> px:int -> py:int -> bool
  (** [is_inside_plot layout ~px ~py] tests whether [(px, py)] falls within the
      plot region. *)

  (** {2 Domain and View} *)

  val x_domain : t -> View.window
  (** [x_domain layout] returns the full x-axis data domain. *)

  val y_domain : t -> View.window
  (** [y_domain layout] returns the full y-axis data domain. *)

  val y2_domain : t -> View.window option
  (** [y2_domain layout] returns the secondary y-axis domain, or [None] if no
      secondary axis is configured. *)

  val x_view : t -> View.window
  (** [x_view layout] returns the currently visible x-axis range (may be a
      subset of the domain if zoomed). *)

  val y_view : t -> View.window
  (** [y_view layout] returns the currently visible y-axis range. *)

  val y2_view : t -> View.window option
  (** [y2_view layout] returns the currently visible secondary y-axis range. *)

  val y_axis_title_width : t -> int
  (** [y_axis_title_width layout] returns the width reserved for the y-axis
      title, in cells. *)

  val y2_axis_width : t -> int
  (** [y2_axis_width layout] returns the width reserved for the secondary y-axis
      (labels + ticks), in cells. *)

  val has_y2 : t -> bool
  (** [has_y2 layout] returns [true] if a secondary y-axis is active. *)

  (** {2 Coordinate Conversion} *)

  val data_of_px : t -> px:int -> py:int -> (float * float) option
  (** [data_of_px layout ~px ~py] converts cell coordinates to data coordinates.
      Returns [None] if [(px, py)] is outside the plot region. *)

  val px_of_data : t -> x:float -> y:float -> int * int
  (** [px_of_data layout ~x ~y] converts data coordinates to cell coordinates.
      Values outside the visible range are clamped to the plot boundary. *)

  (** {2 Category Lookup} *)

  val x_category_of_px : t -> px:int -> string option
  (** [x_category_of_px layout ~px] returns the category at cell column [px]
      when the x-axis uses a band scale. Returns [None] for non-band scales or
      out-of-range positions. *)

  val y_category_of_px : t -> py:int -> string option
  (** [y_category_of_px layout ~py] returns the category at cell row [py] when
      the y-axis uses a band scale. *)

  val px_of_x_category : t -> string -> int option
  (** [px_of_x_category layout cat] returns the center cell column for category
      [cat]. Returns [None] if [cat] is not in the scale. *)

  val py_of_y_category : t -> string -> int option
  (** [py_of_y_category layout cat] returns the center cell row for category
      [cat]. *)

  (** {2 View Manipulation} *)

  type axis = [ `X | `Y | `Both ]
  (** Axis selector for zoom and pan operations. *)

  val clamp_view : t -> View.t -> View.t
  (** [clamp_view layout view] constrains [view] to the layout's data domain.
      Respects the [clamp] setting on each scale; band scales are left
      unchanged. *)

  val zoom_view_around_px :
    t -> view:View.t -> axis:axis -> px:int -> py:int -> factor:float -> View.t
  (** [zoom_view_around_px layout ~view ~axis ~px ~py ~factor] zooms [view]
      around the data point at cell position [(px, py)]. If the position is
      outside the plot, zooms around the plot center. *)

  val pan_view_by_px : t -> view:View.t -> dx:int -> dy:int -> View.t
  (** [pan_view_by_px layout ~view ~dx ~dy] pans [view] by [(dx, dy)] cells. The
      delta is converted to data units proportional to the current view range
      and plot size. *)

  val plot_center_px : t -> int * int
  (** [plot_center_px layout] returns the cell coordinates of the plot center.
  *)

  val zoom_view_around_center :
    t -> view:View.t -> axis:axis -> factor:float -> View.t
  (** [zoom_view_around_center layout ~view ~axis ~factor] zooms [view] around
      the plot center. Convenience wrapper over {!zoom_view_around_px}. *)

  (** {2 Hit Testing} *)

  val hit_test :
    ?radius:int -> ?policy:Hit.policy -> t -> px:int -> py:int -> Hit.t option
  (** [hit_test layout ~px ~py] finds the nearest data point to cell position
      [(px, py)].

      Returns [None] if [(px, py)] is outside the plot region or no data point
      is within [radius].

      @param radius Maximum hit distance in cells. Default is [3].
      @param policy Distance metric. Default is [`Nearest_px]. *)
end

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
      the data point [(x, y)], spanning the full plot area.

      @param style Default inherits from {!Theme.t.crosshair}.
      @param pattern Line pattern. Default is [`Solid]. *)

  val marker :
    ?style:Ansi.Style.t ->
    ?glyph:string ->
    Layout.t ->
    Grid.t ->
    x:float ->
    y:float ->
    unit
  (** [marker layout grid ~x ~y] draws a single glyph at data point [(x, y)].

      @param style Default inherits from {!Theme.t.marker}.
      @param glyph Default is ["●"]. *)

  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]
  (** Tooltip placement relative to the anchor point.

      [`Auto] tries [`Right], [`Left], [`Top], [`Bottom] in order, choosing the
      position with the least clipping and no overlap with the anchor point. *)

  type tooltip_border = [ `Theme | `None | `Style of Ansi.Style.t ]
  (** Tooltip border style.

      - [`Theme]: Use the theme's tooltip border (may be [None]).
      - [`None]: No border.
      - [`Style s]: Custom border style. *)

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
  (** [tooltip layout grid ~x ~y lines] draws a tooltip box anchored at data
      point [(x, y)] containing the given text [lines].

      The tooltip is clamped to the plot region. Does nothing if [(x, y)] maps
      outside the plot.

      @param style Text style. Default inherits from {!Theme.t.tooltip}.
      @param border Default is [`Theme].
      @param padding
        Interior padding in cells. Default is [1]. Clamped to [>= 0].
      @param anchor Placement strategy. Default is [`Auto]. *)

  type h_anchor = [ `Left | `Center | `Right ]
  (** Horizontal text alignment. *)

  type v_anchor = [ `Top | `Middle | `Bottom ]
  (** Vertical text alignment. *)

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
  (** [text layout grid ~x ~y label] draws a text label at data point [(x, y)].

      @param style Default inherits from {!Theme.t.labels}.
      @param anchor Horizontal alignment. Default is [`Left].
      @param v_anchor Currently unused (all variants behave identically). *)

  type arrow_head = [ `None | `Arrow | `Dot ]
  (** Arrow head style at the endpoint.

      - [`None]: No head.
      - [`Arrow]: Directional arrow ([→], [←], [↑], [↓]).
      - [`Dot]: Circular dot (["●"]). *)

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
      [(x2, y2)] with an optional head at the endpoint.

      @param style Default inherits from {!Theme.t.labels}.
      @param head Arrow head style. Default is [`Arrow]. *)
end

module Legend : sig
  (** Chart legend rendering.

      Renders legend items (colored marker + label) to a {!Grid.t}. Use
      {!items_of_layout} to automatically extract items from marks that have
      labels. *)

  type item = { label : string; style : Ansi.Style.t; marker : string }
  (** A legend entry: colored [marker] glyph followed by [label] text. *)

  val draw :
    ?direction:[ `Horizontal | `Vertical ] ->
    ?gap:int ->
    item list ->
    Grid.t ->
    width:int ->
    height:int ->
    unit
  (** [draw items grid ~width ~height] renders legend items to [grid].

      @param direction Layout direction. Default is [`Vertical].
      @param gap
        Spacing between items in cells. Default is [0] for vertical, [2] for
        horizontal. *)

  val items_of_layout : Layout.t -> item list
  (** [items_of_layout layout] extracts legend items from all marks in [layout]
      that have a [label] set. Mark types without legend support (e.g., rules,
      shades) are excluded. *)
end

module Sparkline = Sparkline
(** Compact sparkline charts. See {!Sparkline} for the full API. *)

(** {1 Chart Specification} *)

type t
(** An immutable chart specification. Build with {!empty} or {!make}, configure
    with [with_*] functions, add marks with {!add}, then render with {!draw}. *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
(** Manual frame configuration. [margins] is [(top, right, bottom, left)] in
    cells. [inner_padding] is the space between frame and plot area. *)

type frame =
  | Auto
  | Manual of frame_config
      (** Frame mode. [Auto] computes margins from axis/label sizes. [Manual]
          uses explicit margins and padding. *)

type title = { text : string; style : Ansi.Style.t option }
(** Chart title with optional style override. *)

val default_frame : frame
(** The default frame mode ([Auto]). *)

val manual_frame :
  ?margins:int * int * int * int -> ?inner_padding:int -> unit -> frame
(** [manual_frame ()] creates a {!Manual} frame.

    @param margins
      [(top, right, bottom, left)] in cells. Default is [(0, 0, 0, 0)].
    @param inner_padding
      Space between frame and plot area. Clamped to [>= 0]. Default is [0]. *)

val empty : ?theme:Theme.t -> unit -> t
(** [empty ()] creates an empty chart with no marks.

    Gridlines are hidden by default; use {!with_grid} to enable them.

    @param theme Chart theme. Default is {!Theme.default}. *)

val make : ?theme:Theme.t -> ?title:string -> Mark.t list -> t
(** [make marks] creates a chart from a list of marks.

    Marks render in list order (first mark is drawn first, last mark is on top).

    @param theme Chart theme. Default is {!Theme.default}.
    @param title Chart title displayed above the plot area. *)

val with_theme : Theme.t -> t -> t
(** [with_theme theme t] replaces the chart theme. Axis and gridline styles are
    updated to inherit from the new theme. *)

val with_frame : frame -> t -> t
(** [with_frame frame t] sets the frame mode. *)

val with_title : ?style:Ansi.Style.t -> string -> t -> t
(** [with_title text t] sets the chart title. *)

val with_x_scale : Scale.t -> t -> t
(** [with_x_scale scale t] sets the x-axis scale. *)

val with_y_scale : Scale.t -> t -> t
(** [with_y_scale scale t] sets the primary y-axis scale. *)

val with_y2_scale : Scale.t -> t -> t
(** [with_y2_scale scale t] enables and sets the secondary y-axis scale. *)

val with_axes : ?x:Axis.t -> ?y:Axis.t -> t -> t
(** [with_axes t] configures the primary axes. Only provided axes are replaced;
    omitted axes keep their current configuration. *)

val with_y2_axis : Axis.t -> t -> t
(** [with_y2_axis axis t] enables and configures the secondary y-axis. *)

val with_grid : Gridlines.t -> t -> t
(** [with_grid gridlines t] sets the background gridline configuration. *)

val add : Mark.t -> t -> t
(** [add mark t] appends [mark] to the chart. Marks render in add order (later
    marks draw on top). *)

(** {2 Mark convenience wrappers}

    Shorthand for [add (Mark.xxx ...) t], enabling pipeline-style chart
    construction:

    {[
      empty () |> line ~x:fst ~y:snd data |> rule 0.0
    ]} *)

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
(** [line ~x ~y data t] adds a line mark connecting consecutive data points.

    Points are connected in array order. NaN y-values are skipped (use
    {!line_gaps} for explicit gap control).

    @param resolution Sub-cell resolution mode. Default is [`Cell].
    @param pattern Line pattern. Default is [`Solid].
    @param glyph
      When set, draws this glyph at each point instead of connecting lines.
    @param y_axis Default is [`Y1]. *)

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
(** [line_gaps ~x ~y data t] adds a line with explicit gaps where [y] returns
    [None]. The line breaks at gaps and resumes at the next [Some] value.

    @param resolution Sub-cell resolution mode. Default is [`Cell].
    @param pattern Line pattern. Default is [`Solid].
    @param glyph
      When set, draws this glyph at each point instead of connecting lines.
    @param y_axis Default is [`Y1]. *)

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
(** [scatter ~x ~y data t] adds individual data points.

    @param glyph Custom point glyph. Default depends on the charset.
    @param mode Rendering mode. Default is [`Cell]. *)

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
(** [bar ~category ~value data t] adds a bar chart.

    Categories are placed on the band-scale axis; values determine bar length.
    Implicitly uses a {!Scale.Band} on the category axis and includes zero in
    the value domain.

    @param direction
      Default is [`Vertical] (categories on x-axis, bars grow upward).
    @param mode Default is [`Half_block]. *)

val stacked_bar :
  ?id:Mark.id ->
  ?direction:Mark.direction ->
  ?gap:int ->
  ?size:int ->
  ?mode:Mark.bar_mode ->
  Mark.stacked_bar array ->
  t ->
  t
(** [stacked_bar data t] adds stacked bars from pre-segmented data.

    Each {!Mark.stacked_bar} entry specifies a category and its segments.
    Segments stack from the baseline upward (or leftward for horizontal).

    @param direction Default is [`Vertical].
    @param gap Spacing between bars in cells. Default is [1]. Clamped to [>= 0].
    @param size
      Explicit bar width/height in cells. [None] auto-sizes based on available
      space.
    @param mode Default is [`Half_block]. *)

val rule :
  ?id:Mark.id ->
  ?style:Ansi.Style.t ->
  ?direction:Mark.direction ->
  ?pattern:Charset.line_pattern ->
  ?y_axis:Mark.y_axis_selector ->
  float ->
  t ->
  t
(** [rule value t] adds a reference line spanning the full plot area.

    @param direction
      Default is [`Horizontal] (horizontal line at y=[value]). [`Vertical] draws
      a vertical line at x=[value].
    @param pattern Line pattern. Default is [`Solid]. *)

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
(** [heatmap ~x ~y ~value data t] adds a heatmap.

    Maps [value] to colors. Multiple points in the same cell are combined using
    [agg].

    @param color_scale Color gradient array. Empty uses a default gradient.
    @param value_range
      Fixed [(min, max)] for color mapping. [None] infers from data.
    @param auto_value_range
      When [true] (the default), computes the range from data when [value_range]
      is not set.
    @param agg Aggregation for colliding cells. Default is [`Last].
    @param mode Rendering mode. Default is [Cells_fg]. *)

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
(** [candles data t] adds a candlestick chart from OHLC data.

    Data is sorted by {!Mark.ohlc.time} internally. Bullish candles (close >=
    open) and bearish candles (close < open) are styled separately.

    @param bullish Style for bullish candles. Default is green.
    @param bearish Style for bearish candles. Default is red.
    @param width Default is [`One].
    @param body Default is [`Filled]. *)

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
(** [circle ~cx ~cy ~r data t] adds circles.

    Radius [r] is in data units. *)

val shade :
  ?id:Mark.id -> ?style:Ansi.Style.t -> min:float -> max:float -> t -> t
(** [shade ~min ~max t] adds a vertical shaded region between x=[min] and
    x=[max]. If [min > max], the values are swapped. Useful for highlighting
    date ranges or intervals. *)

val column_bg : ?id:Mark.id -> ?style:Ansi.Style.t -> float -> t -> t
(** [column_bg x t] adds a full-height background highlight at data x-coordinate
    [x]. *)

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
(** [area ~x ~y data t] adds a filled area chart.

    Fills the region between the data line and [baseline].

    @param baseline Default is [`Zero]. *)

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
(** [fill_between ~x ~y_low ~y_high data t] adds a filled region between two
    y-value curves. Useful for confidence intervals or range bands. *)

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
(** [histogram ~x data t] adds a histogram from continuous data.

    Binning is computed eagerly at construction time.

    @param bins Binning method. Default is [Bins 10].
    @param normalize Normalization mode. Default is [`Count]. *)

val layout :
  ?view:View.t -> ?x:int -> ?y:int -> t -> width:int -> height:int -> Layout.t
(** [layout t ~width ~height] compiles [t] into a {!Layout.t} without rendering.
    Use this when you need layout information (coordinate mapping, hit-testing)
    without drawing.

    @param view Viewport constraints. Default is {!View.empty}.
    @param x Horizontal offset in the grid. Default is [0].
    @param y Vertical offset in the grid. Default is [0].
    @param width Chart width in cells. Clamped to [>= 1].
    @param height Chart height in cells. Clamped to [>= 1]. *)

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
    for subsequent hit-testing and overlay drawing.

    @param view Viewport constraints. Default is {!View.empty}.
    @param x Horizontal offset in the grid. Default is [0].
    @param y Vertical offset in the grid. Default is [0].
    @param width Chart width in cells. Clamped to [>= 1].
    @param height Chart height in cells. Clamped to [>= 1]. *)
