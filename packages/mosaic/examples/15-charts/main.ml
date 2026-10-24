(** Interactive charts demo: zoom, pan, hover tooltips for multiple chart types.

    Line, scatter, bar, stacked bar, heatmap and candlestick charts, each
    keeping its zoom and pan.

    Tab and Shift+Tab or < and > switch charts; the wheel zooms at the pointer,
    [+] and [-] at the center; dragging or the arrows pan; r resets the chart
    and 0 all of them; g toggles the grid and G cycles its pattern; c cycles the
    charset, t the theme, m the chart's mode; p toggles the line chart's points,
    h the help; q or Esc quits. *)

open Mosaic
module Charts = Matrix_charts

(* --- Chart types --- *)

type chart_type =
  | Line
  | Scatter
  | Bar
  | Stacked_bar
  | Heatmap
  | Candlestick
  | Area
  | Histogram
  | Dual_axis

let all_charts =
  [
    Line;
    Scatter;
    Bar;
    Stacked_bar;
    Heatmap;
    Candlestick;
    Area;
    Histogram;
    Dual_axis;
  ]

let chart_name = function
  | Line -> "Line"
  | Scatter -> "Scatter"
  | Bar -> "Bar"
  | Stacked_bar -> "Stacked Bar"
  | Heatmap -> "Heatmap"
  | Candlestick -> "Candlestick"
  | Area -> "Area"
  | Histogram -> "Histogram"
  | Dual_axis -> "Dual Axis"

let chart_index (c : chart_type) : int =
  let rec find i = function
    | [] -> 0
    | x :: tl -> if x = c then i else find (i + 1) tl
  in
  find 0 all_charts

let chart_of_index (i : int) : chart_type =
  let n = List.length all_charts in
  let i = if n <= 0 then 0 else ((i mod n) + n) mod n in
  match List.nth_opt all_charts i with Some c -> c | None -> Line

let next_chart c = chart_of_index (chart_index c + 1)
let prev_chart c = chart_of_index (chart_index c - 1)

(* --- Line style options --- *)

type line_resolution = [ `Cell | `Steps | `Quadrant | `Braille ]

let line_resolution_name : line_resolution -> string = function
  | `Cell -> "Cell"
  | `Steps -> "Steps"
  | `Quadrant -> "Quadrant"
  | `Braille -> "Braille"

let cycle_line_resolution : line_resolution -> line_resolution = function
  | `Cell -> `Steps
  | `Steps -> `Quadrant
  | `Quadrant -> `Braille
  | `Braille -> `Cell

type line_pattern = [ `Solid | `Dashed | `Dotted ]

let line_pattern_name : line_pattern -> string = function
  | `Solid -> "Solid"
  | `Dashed -> "Dashed"
  | `Dotted -> "Dotted"

let cycle_line_pattern : line_pattern -> line_pattern = function
  | `Solid -> `Dashed
  | `Dashed -> `Dotted
  | `Dotted -> `Solid

(* --- Scatter mode options --- *)

type scatter_mode = [ `Cell | `Braille | `Density ]

let scatter_mode_name : scatter_mode -> string = function
  | `Cell -> "Cell"
  | `Braille -> "Braille"
  | `Density -> "Density"

let cycle_scatter_mode : scatter_mode -> scatter_mode = function
  | `Cell -> `Braille
  | `Braille -> `Density
  | `Density -> `Cell

(* --- Grid pattern options --- *)

let grid_pattern_name : Charts.dash -> string = function
  | `Solid -> "Solid"
  | `Dashed -> "Dashed"
  | `Dotted -> "Dotted"

let cycle_grid_pattern : Charts.dash -> Charts.dash = function
  | `Solid -> `Dashed
  | `Dashed -> `Dotted
  | `Dotted -> `Solid

(* --- Charset options --- *)

type charset_mode =
  [ `Unicode_light | `Unicode_heavy | `Unicode_rounded | `Ascii ]

let charset_name : charset_mode -> string = function
  | `Unicode_light -> "Light"
  | `Unicode_heavy -> "Heavy"
  | `Unicode_rounded -> "Rounded"
  | `Ascii -> "ASCII"

let cycle_charset : charset_mode -> charset_mode = function
  | `Unicode_light -> `Unicode_heavy
  | `Unicode_heavy -> `Unicode_rounded
  | `Unicode_rounded -> `Ascii
  | `Ascii -> `Unicode_light

let glyphs_of_mode : charset_mode -> Charts.Theme.glyphs = function
  | `Unicode_light -> Charts.Theme.light_glyphs
  | `Unicode_heavy -> Charts.Theme.heavy_glyphs
  | `Unicode_rounded -> Charts.Theme.rounded_glyphs
  | `Ascii -> Charts.Theme.ascii_glyphs

(* --- Heatmap render options --- *)

type heatmap_mode = [ `Glyph | `Cell | `Half | `Shade | `Dense ]

let heatmap_mode_name : heatmap_mode -> string = function
  | `Glyph -> "Glyph"
  | `Cell -> "Cell"
  | `Half -> "Halfblock"
  | `Shade -> "Shaded"
  | `Dense -> "Dense"

let cycle_heatmap_mode : heatmap_mode -> heatmap_mode = function
  | `Glyph -> `Cell
  | `Cell -> `Half
  | `Half -> `Shade
  | `Shade -> `Dense
  | `Dense -> `Glyph

(* --- Sample data --- *)

let line_data =
  Array.init 80 (fun i ->
      let x = float_of_int i in
      let y = (sin (x *. 0.18) *. 3.0) +. (cos (x *. 0.07) *. 2.2) +. 6.0 in
      (x, y))

let scatter_data =
  Array.init 160 (fun i ->
      let x = float_of_int (i mod 20) +. (Random.float 0.6 -. 0.3) in
      let y = float_of_int (i / 20) +. (Random.float 0.6 -. 0.3) in
      (x, y))

let bar_data =
  [|
    ("Mon", 12.0);
    ("Tue", 19.0);
    ("Wed", 8.0);
    ("Thu", 15.0);
    ("Fri", 22.0);
    ("Sat", 10.0);
    ("Sun", 14.0);
  |]

(* Stacked bar data: rows of one quarter stack, one segment per product *)
let products = [ "Product A"; "Product B"; "Product C" ]

let product_colors =
  [
    Ansi.Color.of_rgb 100 180 255;
    Ansi.Color.of_rgb 255 150 100;
    Ansi.Color.of_rgb 150 220 150;
  ]

let stacked_bar_data =
  Array.concat
    (List.map
       (fun (quarter, values) ->
         Array.of_list (List.map2 (fun p v -> (quarter, p, v)) products values))
       [
         ("Q1", [ 15.0; 22.0; 18.0 ]);
         ("Q2", [ 20.0; 18.0; 25.0 ]);
         ("Q3", [ 12.0; 28.0; 20.0 ]);
         ("Q4", [ 25.0; 15.0; 22.0 ]);
       ])

let heatmap_data =
  Array.concat
    (List.map Array.of_list
       (List.init 14 (fun y ->
            List.init 18 (fun x ->
                let xf = float_of_int x and yf = float_of_int y in
                let value =
                  (sin (xf *. 0.35) *. cos (yf *. 0.32))
                  +. (0.35 *. sin ((xf +. yf) *. 0.2))
                in
                let v01 = (value +. 2.0) /. 4.0 |> max 0.0 |> min 1.0 in
                (xf, yf, v01)))))

let candlestick_data =
  let base = 100.0 in
  Array.init 40 (fun i ->
      let open_ =
        base +. (float_of_int i *. 0.35) +. (Random.float 5.0 -. 2.5)
      in
      let close = open_ +. (Random.float 7.0 -. 3.5) in
      let high = max open_ close +. Random.float 2.4 in
      let low = min open_ close -. Random.float 2.4 in
      Charts.{ time = float_of_int i; open_; high; low; close })

(* Area chart data - CPU usage over time with variation *)
let area_data =
  Array.init 60 (fun i ->
      let x = float_of_int i in
      let base = 40.0 +. (sin (x *. 0.1) *. 20.0) in
      let noise = Random.float 10.0 -. 5.0 in
      (x, max 5.0 (min 95.0 (base +. noise))))

(* Histogram data - normally distributed values *)
let histogram_data =
  Array.init 500 (fun _ ->
      (* Box-Muller transform for normal distribution *)
      let u1 = Random.float 1.0 in
      let u2 = Random.float 1.0 in
      let z =
        sqrt (-2.0 *. log (max 0.0001 u1)) *. cos (2.0 *. Float.pi *. u2)
      in
      (* Mean 50, stddev 15 *)
      50.0 +. (z *. 15.0))

(* Dual axis data - price and volume *)
let dual_axis_price_data =
  Array.init 50 (fun i ->
      let x = float_of_int i in
      let trend = 100.0 +. (x *. 0.5) in
      let cycle = sin (x *. 0.15) *. 10.0 in
      let noise = Random.float 4.0 -. 2.0 in
      (x, trend +. cycle +. noise))

let dual_axis_volume_data =
  Array.init 50 (fun i ->
      let x = float_of_int i in
      let base = 1000.0 +. (sin (x *. 0.2) *. 400.0) in
      let spike = if i mod 7 = 0 then 800.0 else 0.0 in
      let noise = Random.float 200.0 -. 100.0 in
      (x, max 100.0 (base +. spike +. noise)))

(* --- UI styles (for header/footer only) --- *)

let header_bg = Ansi.Color.of_rgb 40 60 80
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let emph = Ansi.Style.make ~bold:true ()

(* --- Interaction model --- *)

type theme_mode = [ `Dark | `Light ]

type model = {
  chart : chart_type;
  (* per-chart view persistence *)
  views : Charts.View.t array;
  hover : (int * int) option; (* the pointer's cell in the canvas *)
  dragging : (int * int) option; (* the cell the drag last reached *)
  show_grid : bool;
  theme : theme_mode;
  show_help : bool;
  heatmap_mode : heatmap_mode;
  (* style options for demo *)
  line_resolution : line_resolution;
  line_pattern : line_pattern;
  show_line_points : bool;
  show_smoothing : bool;
  show_confidence : bool;
  show_annotations : bool;
  scatter_mode : scatter_mode;
  grid_pattern : Charts.dash;
  charset : charset_mode;
  use_log_scale : bool;
}

type msg =
  | Next_chart
  | Prev_chart
  | Pointer of Event.mouse
  | Zoom_center of [ `In | `Out ]
  | Pan_by_keys of float * float
  | Reset_view
  | Reset_all_views
  | Toggle_grid
  | Toggle_theme
  | Toggle_help
  | Toggle_line_points
  | Toggle_smoothing
  | Toggle_confidence
  | Toggle_annotations
  | Toggle_log_scale
  | Cycle_style
  | Cycle_grid_pattern
  | Cycle_charset
  | Quit

let init () =
  let n = List.length all_charts |> max 1 in
  ( {
      chart = Line;
      views = Array.init n (fun _ -> Charts.View.empty);
      hover = None;
      dragging = None;
      show_grid = true;
      theme = `Dark;
      show_help = false;
      heatmap_mode = `Cell;
      line_resolution = `Braille;
      line_pattern = `Solid;
      show_line_points = false;
      show_smoothing = false;
      show_confidence = false;
      show_annotations = false;
      scatter_mode = `Braille;
      grid_pattern = `Solid;
      charset = `Unicode_light;
      use_log_scale = false;
    },
    Cmd.none )

let get_view (m : model) : Charts.View.t =
  let i = chart_index m.chart in
  if i >= 0 && i < Array.length m.views then m.views.(i) else Charts.View.empty

let set_view (m : model) (v : Charts.View.t) : model =
  let i = chart_index m.chart in
  if i < 0 || i >= Array.length m.views then m
  else
    let views = Array.copy m.views in
    views.(i) <- v;
    { m with views }

let reset_all_views (m : model) : model =
  let views = Array.map (fun _ -> Charts.View.empty) m.views in
  { m with views }

let theme_of_model (m : model) : Charts.Theme.t =
  let base =
    match m.theme with
    | `Dark -> Charts.Theme.dark
    | `Light -> Charts.Theme.light
  in
  { base with glyphs = glyphs_of_mode m.charset; grid_dash = m.grid_pattern }

let rgb = Ansi.Color.of_rgb
let gray level = Ansi.Color.grayscale ~level

(* --- Chart specifications (pure) --- *)

let spec_for (m : model) (ct : chart_type) : Charts.t =
  let grid = if m.show_grid then `Ticks else `None in
  let axes x y = Charts.[ axis ~title:x ~grid "x"; axis ~title:y ~grid "y" ] in
  let first (x, _, _) = x and second (_, y, _) = y and third (_, _, z) = z in
  Charts.(
    match ct with
    | Line ->
        let data = if m.show_smoothing then ema 0.15 line_data else line_data in
        let confidence_data =
          Array.map
            (fun (x, y) ->
              let band = 0.8 +. (sin (x *. 0.1) *. 0.3) in
              (x, y -. band, y +. band))
            line_data
        in
        let given b l = if b then l else [] in
        layer
          (given m.show_confidence
             [
               area ~dots:`Cell
                 ~color:(series ~color:(rgb 100 150 200) "±1σ")
                 ~x:first ~y:third ~y2:second confidence_data
               |> name "confidence";
             ]
          @ [
              line
                ~dots:(m.line_resolution :> [ dots | `Steps ])
                ~dash:m.line_pattern ~color:(series "Signal") ~x:fst ~y:snd data
              |> name "line";
            ]
          @ given m.show_smoothing
              [
                dot ~dots:`Cell ~symbol:"·"
                  ~color:(series ~color:(gray 12) "Raw")
                  ~x:fst ~y:snd line_data
                |> name "raw";
              ]
          @ given m.show_line_points
              [
                dot ~dots:`Cell ~symbol:"∙" ~color:(series "Samples") ~x:fst
                  ~y:snd line_data
                |> name "points";
              ]
          @ axes "Time (samples)" "Amplitude")
        |> title "Waveform Analysis"
    | Scatter ->
        layer
          ((dot
              ~dots:(m.scatter_mode :> [ dots | `Density ])
              ~symbol:"·" ~color:(series "Points") ~x:fst ~y:snd scatter_data
           |> name "scatter")
          :: axes "X Position" "Y Position")
        |> title "Cluster Distribution"
    | Bar ->
        layer
          ((bar_y ~color:(series "Activity") ~x:fst ~y:snd bar_data
           |> name "bars")
          :: axes "Day" "Count")
        |> title "Weekly Activity"
    | Stacked_bar ->
        layer
          ((bar_y ~color:(cat second) ~label:second ~x:first ~y:third
              stacked_bar_data
           |> name "stacked")
          :: axes "Quarter" "Revenue")
        |> scale "color" (Scale.band ~colors:product_colors ())
        |> title "Quarterly Revenue"
    | Heatmap ->
        let dots, interpolate =
          match m.heatmap_mode with
          | `Glyph -> (`Glyph "█", `None)
          | `Cell -> (`Cell, `None)
          | `Half -> (`Half, `None)
          | `Shade -> (`Shade, `Bilinear)
          | `Dense -> (`Cell, `Bilinear)
        in
        layer
          ((raster ~dots ~interpolate ~x:first ~y:second ~value:third
              heatmap_data
           |> name "heat")
          :: axes "Column" "Row")
        |> title "Intensity Map"
    | Candlestick ->
        let fig =
          layer
            ((candles candlestick_data |> name "ohlc") :: axes "Time" "Price")
        in
        if m.use_log_scale then
          fig |> scale "y" (Scale.log ()) |> title "Price Action (Log Scale)"
        else fig |> title "Price Action"
    | Area ->
        layer
          ([
             area ~dots:`Cell
               ~color:(series ~color:(rgb 100 180 255) "CPU")
               ~x:fst ~y:snd area_data
             |> name "cpu";
             line ~color:(const (rgb 50 130 220)) ~x:fst ~y:snd area_data
             |> name "cpu-line";
           ]
          @ axes "Time (s)" "Usage (%)")
        |> title "CPU Usage Over Time"
    | Histogram ->
        layer
          ((histogram ~bins:(`Count 20)
              ~color:(series ~color:(rgb 150 200 100) "Distribution")
              histogram_data
           |> name "dist")
          :: axes "Value" "Count")
        |> title "Value Distribution"
    | Dual_axis ->
        layer
          [
            area ~dots:`Cell
              ~color:(series ~color:(rgb 255 180 100) "Volume")
              ~x:fst ~y:snd dual_axis_volume_data
            |> rename "y" "volume" |> name "volume";
            line
              ~color:(series ~color:(rgb 100 200 255) "Price")
              ~x:fst ~y:snd dual_axis_price_data
            |> name "price";
            axis ~title:"Time" ~grid "x";
            axis ~title:"Price ($)" ~grid "y";
            axis ~side:`Right ~title:"Volume" "volume";
          ]
        |> title "Price & Volume (Dual Axis)")

(* --- Hit/tooltip formatting --- *)

let hit_params (ct : chart_type) : float * [ `Euclidean | `X | `Y ] =
  match ct with
  | Line | Area | Dual_axis -> (4., `X)
  | Scatter -> (4., `Euclidean)
  | Bar | Stacked_bar | Histogram -> (2., `Euclidean)
  | Heatmap -> (3., `Euclidean)
  | Candlestick -> (2., `X)

(* What the pointer is over, read from the program's own rows. *)
let hit_lines (m : model) (hit : Charts.hit) =
  let f = Printf.sprintf in
  let xy (x, y) = [ f "x: %.3g" x; f "y: %.3g" y ] in
  let header = String.concat "/" hit.id in
  let row = hit.row in
  header :: f "index: %d" row
  ::
  (match hit.id with
  | [ "line" ] ->
      xy
        (if m.show_smoothing then Charts.ema 0.15 line_data else line_data).(row)
  | [ ("raw" | "points") ] -> xy line_data.(row)
  | [ "confidence" ] ->
      let x, lo, hi =
        (* the band of the row's sample *)
        let x, y = line_data.(row) in
        let band = 0.8 +. (sin (x *. 0.1) *. 0.3) in
        (x, y -. band, y +. band)
      in
      [ f "x: %.3g" x; f "band: %.3g..%.3g" lo hi ]
  | [ "scatter" ] -> xy scatter_data.(row)
  | [ "bars" ] ->
      let day, v = bar_data.(row) in
      [ "category: " ^ day; f "value: %.3g" v ]
  | [ "stacked" ] ->
      let quarter, product, v = stacked_bar_data.(row) in
      let total =
        Array.fold_left
          (fun t (q, _, v) -> if q = quarter then t +. v else t)
          0. stacked_bar_data
      in
      [ "category: " ^ quarter; product; f "value: %.3g (of %.3g)" v total ]
  | [ "heat" ] ->
      let x, y, v = heatmap_data.(row) in
      [ f "x: %.3g" x; f "y: %.3g" y; f "value: %.3g" v ]
  | [ "ohlc" ] ->
      let c = candlestick_data.(row) in
      [
        f "t: %.3g" c.time;
        f "O: %.3g" c.open_;
        f "H: %.3g" c.high;
        f "L: %.3g" c.low;
        f "C: %.3g" c.close;
      ]
  | [ ("cpu" | "cpu-line") ] -> xy area_data.(row)
  | [ "volume" ] -> xy dual_axis_volume_data.(row)
  | [ "price" ] -> xy dual_axis_price_data.(row)
  | _ -> [])

let readout_lines (l : Charts.Layout.t) at =
  let num name =
    match Charts.Layout.value l name at with
    | Some (Num v) -> [ Printf.sprintf "%s: %.3g" name v ]
    | Some (Cat c) -> [ "category: " ^ c ]
    | None -> []
  in
  num "x" @ num "y"

(* --- Update (interaction state machine) --- *)

let update (msg : msg) (m : model) =
  match msg with
  | Next_chart ->
      ( { m with chart = next_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Prev_chart ->
      ( { m with chart = prev_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Pointer ev -> (
      (* The event gives the pointer in the canvas's cells and the canvas's
         size, at which the chart lays out as it is drawn. *)
      let at = Event.Mouse.local ev in
      let width, height = Event.Mouse.size ev in
      let fig = spec_for m m.chart in
      let l =
        Charts.layout ~view:(get_view m) ~theme:(theme_of_model m) ~width
          ~height fig
      in
      let set v = set_view m (Charts.View.clamp fig v) in
      match Event.Mouse.kind ev with
      | Move -> ({ m with hover = Some at }, Cmd.none)
      | Out -> ({ m with hover = None; dragging = None }, Cmd.none)
      | Down _ -> ({ m with hover = Some at; dragging = Some at }, Cmd.none)
      | Up _ | Drag_end _ -> ({ m with dragging = None }, Cmd.none)
      | Drag _ -> (
          match m.dragging with
          | None -> ({ m with hover = Some at; dragging = Some at }, Cmd.none)
          | Some (x0, y0) ->
              let by = (fst at - x0, snd at - y0) in
              ( {
                  (set (Charts.Layout.pan l ~by)) with
                  hover = Some at;
                  dragging = Some at;
                },
                Cmd.none ))
      | Scroll { direction = Scroll_up; _ } ->
          (set (Charts.Layout.zoom l ~at 1.25), Cmd.none)
      | Scroll { direction = Scroll_down; _ } ->
          (set (Charts.Layout.zoom l ~at 0.8), Cmd.none)
      | _ -> (m, Cmd.none))
  | Zoom_center dir ->
      let factor = match dir with `In -> 1.25 | `Out -> 0.8 in
      let fig = spec_for m m.chart in
      ( set_view m
          (Charts.View.clamp fig (Charts.View.zoom fig factor (get_view m))),
        Cmd.none )
  | Pan_by_keys (fx, fy) ->
      let fig = spec_for m m.chart in
      ( set_view m
          (Charts.View.clamp fig
             (Charts.View.pan fig ~by:(fx, fy) (get_view m))),
        Cmd.none )
  | Reset_view -> (set_view m Charts.View.empty, Cmd.none)
  | Reset_all_views -> (reset_all_views m, Cmd.none)
  | Toggle_grid -> ({ m with show_grid = not m.show_grid }, Cmd.none)
  | Toggle_theme ->
      let theme = match m.theme with `Dark -> `Light | `Light -> `Dark in
      ({ m with theme }, Cmd.none)
  | Toggle_help -> ({ m with show_help = not m.show_help }, Cmd.none)
  | Toggle_line_points ->
      ({ m with show_line_points = not m.show_line_points }, Cmd.none)
  | Cycle_style -> (
      match m.chart with
      | Line ->
          (* Cycle: resolution -> pattern -> resolution... *)
          if m.line_pattern <> `Solid then
            ( { m with line_pattern = cycle_line_pattern m.line_pattern },
              Cmd.none )
          else if m.line_resolution = `Braille then
            ( { m with line_resolution = `Cell; line_pattern = `Dashed },
              Cmd.none )
          else
            ( {
                m with
                line_resolution = cycle_line_resolution m.line_resolution;
              },
              Cmd.none )
      | Scatter ->
          ({ m with scatter_mode = cycle_scatter_mode m.scatter_mode }, Cmd.none)
      | Heatmap ->
          ({ m with heatmap_mode = cycle_heatmap_mode m.heatmap_mode }, Cmd.none)
      | Bar | Stacked_bar | Candlestick | Area | Histogram | Dual_axis ->
          (m, Cmd.none))
  | Toggle_smoothing ->
      ({ m with show_smoothing = not m.show_smoothing }, Cmd.none)
  | Toggle_confidence ->
      ({ m with show_confidence = not m.show_confidence }, Cmd.none)
  | Toggle_annotations ->
      ({ m with show_annotations = not m.show_annotations }, Cmd.none)
  | Toggle_log_scale ->
      ({ m with use_log_scale = not m.use_log_scale }, Cmd.none)
  | Cycle_grid_pattern ->
      ({ m with grid_pattern = cycle_grid_pattern m.grid_pattern }, Cmd.none)
  | Cycle_charset -> ({ m with charset = cycle_charset m.charset }, Cmd.none)
  | Quit -> (m, Cmd.quit)

(* --- Drawing overlays (hover + help) --- *)

let hover_overlays (m : model) (l : Charts.Layout.t) : Charts.overlay list =
  match m.hover with
  | None -> []
  | Some at -> (
      let radius, metric = hit_params m.chart in
      match Charts.Layout.hit ~radius ~metric l at with
      | Some hit ->
          [
            Crosshair hit.cell;
            Marker hit.cell;
            Tip { at = hit.cell; anchor = `Auto; lines = hit_lines m hit };
          ]
      | None -> (
          (* Free cursor readout when not near a mark *)
          match readout_lines l at with
          | [] -> []
          | lines -> [ Crosshair at; Tip { at; anchor = `Right; lines } ]))

let help_overlays (m : model) (l : Charts.Layout.t) : Charts.overlay list =
  if not m.show_help then []
  else
    let r = Charts.Layout.plot l in
    let center =
      (r.x + (max 0 (r.width - 1) / 2), r.y + (max 0 (r.height - 1) / 2))
    in
    let mode_line =
      match m.chart with
      | Line ->
          [
            "m: cycle line mode (Cell/Steps/Quadrant/Braille + pattern)";
            "p: toggle data points";
            "s: toggle smoothing (EMA)";
            "f: toggle confidence bands";
          ]
      | Scatter -> [ "m: cycle scatter mode (Cell/Braille/Density)" ]
      | Heatmap -> [ "m: cycle heatmap mode (Cells/Halfblock/Shaded/Dense)" ]
      | Candlestick -> [ "l: toggle log scale" ]
      | Bar | Stacked_bar | Area | Histogram | Dual_axis -> []
    in
    let lines =
      [
        "Controls";
        "────────";
        "Mouse wheel: zoom at cursor";
        "Drag: pan";
        "+ / -: zoom at center";
        "Arrows: pan";
        "r: reset current view";
        "0: reset all views";
        "Tab / Shift+Tab or < / >: switch chart";
        "g: toggle grid";
        "G: cycle grid pattern (Solid/Dashed/Dotted)";
        "c: cycle charset (Light/Heavy/Rounded/ASCII)";
        "t: toggle theme";
        "h: toggle this help";
        "q / Esc: quit";
      ]
      @ mode_line
    in
    [ Tip { at = center; anchor = `Top; lines } ]

let draw_chart (m : model) (c : Canvas.t) =
  let l =
    Charts.layout ~view:(get_view m) ~theme:(theme_of_model m)
      ~width:(Canvas.width c) ~height:(Canvas.height c) (spec_for m m.chart)
  in
  let overlays = hover_overlays m l @ help_overlays m l in
  Charts.draw ~overlays l (Canvas.grid c) ~x:0 ~y:0

(* --- View --- *)

let view (m : model) =
  let idx = chart_index m.chart in
  let n = List.length all_charts in
  let view_to_s = function
    | None -> "auto"
    | Some (lo, hi) -> Printf.sprintf "%.3g..%.3g" lo hi
  in
  let v = get_view m in
  let style_info =
    match m.chart with
    | Line ->
        Printf.sprintf "  line:%s/%s"
          (line_resolution_name m.line_resolution)
          (line_pattern_name m.line_pattern)
    | Scatter ->
        Printf.sprintf "  scatter:%s" (scatter_mode_name m.scatter_mode)
    | Heatmap ->
        Printf.sprintf "  heatmap:%s" (heatmap_mode_name m.heatmap_mode)
    | Bar | Stacked_bar | Candlestick | Area | Histogram | Dual_axis -> ""
  in
  let status =
    Printf.sprintf "theme:%s  charset:%s  grid:%s(%s)%s  view[x=%s y=%s]%s"
      (match m.theme with `Dark -> "dark" | `Light -> "light")
      (charset_name m.charset)
      (if m.show_grid then "on" else "off")
      (grid_pattern_name m.grid_pattern)
      style_info
      (view_to_s (Charts.View.window "x" v))
      (view_to_s (Charts.View.window "y" v))
      (match m.dragging with None -> "" | Some _ -> "  (dragging)")
  in

  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:emph "Charts (interactive)";
              text ~style:muted
                (Printf.sprintf "[%d/%d] %s" (idx + 1) n (chart_name m.chart));
            ];
        ];
      (* Chart area *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          canvas ~live:true
            ~on_mouse:(fun ev -> Some (Pointer ev))
            ~size:{ width = pct 100; height = pct 100 }
            (fun c ~dt:_ ->
              Canvas.clear c;
              draw_chart m c);
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg ~flex_direction:Column
        [
          text ~style:hint
            "Tab/</>: switch  |  wheel: zoom  |  drag: pan  |  +/-: zoom  |  r \
             reset  |  g grid  |  c charset  |  m mode  |  t theme  |  h help  \
             |  q quit";
          text ~style:muted status;
        ];
    ]

(* --- Subscriptions --- *)

let subscriptions (_m : model) =
  Sub.on_key (fun ev ->
      let data = Event.Key.data ev in
      let step = if data.mods.shift then 0.25 else 0.1 in
      match data.key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      (* Chart switching *)
      | Tab when not data.mods.shift -> Some Next_chart
      | Tab when data.mods.shift -> Some Prev_chart
      | Char c when Uchar.equal c (Uchar.of_char '>') -> Some Next_chart
      | Char c when Uchar.equal c (Uchar.of_char '<') -> Some Prev_chart
      (* Zoom *)
      | Char c when Uchar.equal c (Uchar.of_char '+') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '=') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '-') -> Some (Zoom_center `Out)
      (* Pan: move the view a tenth of its span, a quarter with Shift *)
      | Left -> Some (Pan_by_keys (-.step, 0.))
      | Right -> Some (Pan_by_keys (step, 0.))
      | Up -> Some (Pan_by_keys (0., step))
      | Down -> Some (Pan_by_keys (0., -.step))
      (* Reset *)
      | Char c when Uchar.equal c (Uchar.of_char 'r') -> Some Reset_view
      | Char c when Uchar.equal c (Uchar.of_char '0') -> Some Reset_all_views
      (* Toggles *)
      | Char c when Uchar.equal c (Uchar.of_char 'g') -> Some Toggle_grid
      | Char c when Uchar.equal c (Uchar.of_char 't') -> Some Toggle_theme
      | Char c when Uchar.equal c (Uchar.of_char 'h') -> Some Toggle_help
      (* Style cycling *)
      | Char c when Uchar.equal c (Uchar.of_char 'm') -> Some Cycle_style
      | Char c when Uchar.equal c (Uchar.of_char 'p') -> Some Toggle_line_points
      | Char c when Uchar.equal c (Uchar.of_char 'G') -> Some Cycle_grid_pattern
      | Char c when Uchar.equal c (Uchar.of_char 'c') -> Some Cycle_charset
      (* Feature toggles for Line/Candlestick charts *)
      | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Toggle_smoothing
      | Char c when Uchar.equal c (Uchar.of_char 'f') -> Some Toggle_confidence
      | Char c when Uchar.equal c (Uchar.of_char 'a') -> Some Toggle_annotations
      | Char c when Uchar.equal c (Uchar.of_char 'l') -> Some Toggle_log_scale
      | _ -> None)

let () =
  Random.init 42;
  run { init; update; view; subscriptions }
