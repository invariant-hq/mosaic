(** Interactive charts demo: zoom, pan, hover tooltips for multiple chart types.
*)

module Cell_grid = Grid
open Mosaic
open Matrix_charts

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

type line_resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]

let line_resolution_name : line_resolution -> string = function
  | `Cell -> "Cell"
  | `Wave -> "Wave"
  | `Block2x2 -> "Block2x2"
  | `Braille2x4 -> "Braille"

let cycle_line_resolution : line_resolution -> line_resolution = function
  | `Cell -> `Wave
  | `Wave -> `Block2x2
  | `Block2x2 -> `Braille2x4
  | `Braille2x4 -> `Cell

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

let grid_pattern_name : Charset.line_pattern -> string = function
  | `Solid -> "Solid"
  | `Dashed -> "Dashed"
  | `Dotted -> "Dotted"

let cycle_grid_pattern : Charset.line_pattern -> Charset.line_pattern = function
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

let charset_of_mode : charset_mode -> Charset.t = function
  | `Unicode_light -> Charset.unicode_light
  | `Unicode_heavy -> Charset.unicode_heavy
  | `Unicode_rounded -> Charset.unicode_rounded
  | `Ascii -> Charset.ascii

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

(* Stacked bar data using Mark.stacked_bar type *)
let stacked_bar_data =
  let colors =
    [|
      Ansi.Color.of_rgb 100 180 255;
      Ansi.Color.of_rgb 255 150 100;
      Ansi.Color.of_rgb 150 220 150;
    |]
  in
  let labels = [| "Product A"; "Product B"; "Product C" |] in
  let make_segments values =
    Array.to_list
      (Array.mapi
         (fun i v ->
           Mark.
             {
               value = v;
               style = Ansi.Style.make ~fg:colors.(i) ();
               label = Some labels.(i);
             })
         values)
  in
  [|
    Mark.{ category = "Q1"; segments = make_segments [| 15.0; 22.0; 18.0 |] };
    Mark.{ category = "Q2"; segments = make_segments [| 20.0; 18.0; 25.0 |] };
    Mark.{ category = "Q3"; segments = make_segments [| 12.0; 28.0; 20.0 |] };
    Mark.{ category = "Q4"; segments = make_segments [| 25.0; 15.0; 22.0 |] };
  |]

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
      Mark.{ time = float_of_int i; open_; high; low; close })

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
type drag = { start_px : int; start_py : int; start_view : View.t }

type model = {
  chart : chart_type;
  (* per-chart view persistence *)
  views : View.t array;
  hover : (int * int) option;
  dragging : drag option;
  show_grid : bool;
  theme : theme_mode;
  show_help : bool;
  heatmap_mode : Mark.heatmap_mode;
  (* style options for demo *)
  line_resolution : line_resolution;
  line_pattern : line_pattern;
  show_line_points : bool;
  show_smoothing : bool;
  show_confidence : bool;
  show_annotations : bool;
  scatter_mode : scatter_mode;
  grid_pattern : Charset.line_pattern;
  charset : charset_mode;
  use_log_scale : bool;
}

type msg =
  | Next_chart
  | Prev_chart
  | Pointer_move of int * int
  | Pointer_leave
  | Pointer_down of int * int
  | Pointer_up
  | Scroll_zoom of int * int * [ `In | `Out ]
  | Zoom_center of [ `In | `Out ]
  | Pan_by_keys of int * int
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
      views = Array.init n (fun _ -> View.empty);
      hover = None;
      dragging = None;
      show_grid = true;
      theme = `Dark;
      show_help = false;
      heatmap_mode = Mark.Cells_bg;
      line_resolution = `Braille2x4;
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

let get_view (m : model) : View.t =
  let i = chart_index m.chart in
  if i >= 0 && i < Array.length m.views then m.views.(i) else View.empty

let set_view (m : model) (v : View.t) : model =
  let i = chart_index m.chart in
  if i < 0 || i >= Array.length m.views then m
  else
    let views = Array.copy m.views in
    views.(i) <- v;
    { m with views }

let reset_all_views (m : model) : model =
  let views = Array.map (fun _ -> View.empty) m.views in
  { m with views }

let theme_of_model (m : model) : Theme.t =
  let base = match m.theme with `Dark -> Theme.dark | `Light -> Theme.light in
  Theme.with_charset (charset_of_mode m.charset) base

let grid_of_model (m : model) : Gridlines.t =
  if not m.show_grid then Gridlines.hidden
  else
    (* Set style to Style.default so the theme can supply grid style. *)
    Gridlines.default
    |> Gridlines.with_style Ansi.Style.default
    |> Gridlines.with_pattern m.grid_pattern

(* --- Chart specifications (pure) --- *)

let spec_for (m : model) (ct : chart_type) : Matrix_charts.t =
  let theme = theme_of_model m in
  match ct with
  | Line ->
      let smoothed_data =
        if m.show_smoothing then Transform.ema 0.15 line_data else line_data
      in
      let confidence_data =
        Array.map
          (fun (x, y) ->
            let band = 0.8 +. (sin (x *. 0.1) *. 0.3) in
            (x, y -. band, y +. band))
          line_data
      in
      let chart =
        empty ~theme ()
        |> with_title "Waveform Analysis"
        |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
        |> with_axes
             ~x:
               (Axis.default |> Axis.with_ticks 8
               |> Axis.with_title "Time (samples)")
             ~y:
               (Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Amplitude")
        |> with_grid (grid_of_model m)
      in
      (* Add confidence band first (behind line) *)
      let chart =
        if m.show_confidence then
          chart
          |> fill_between ~id:"confidence" ~label:"±1σ"
               ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 100 150 200) ())
               ~resolution:`Cell
               ~x:(fun (x, _, _) -> x)
               ~y_low:(fun (_, lo, _) -> lo)
               ~y_high:(fun (_, _, hi) -> hi)
               confidence_data
        else chart
      in
      (* Add main line *)
      let chart =
        chart
        |> line ~id:"line" ~label:"Signal" ~resolution:m.line_resolution
             ~pattern:m.line_pattern ~x:fst ~y:snd
             (if m.show_smoothing then smoothed_data else line_data)
      in
      (* Add original data points if smoothing is on *)
      let chart =
        if m.show_smoothing then
          chart
          |> scatter ~id:"raw" ~label:"Raw"
               ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ())
               ~glyph:"·" ~mode:`Cell ~x:fst ~y:snd line_data
        else chart
      in
      (* Add sample points if toggled *)
      let chart =
        if m.show_line_points then
          chart
          |> scatter ~id:"points" ~label:"Samples" ~glyph:"∙" ~mode:`Cell ~x:fst
               ~y:snd line_data
        else chart
      in
      chart
  | Scatter ->
      empty ~theme ()
      |> with_title "Cluster Distribution"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 7) ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "X Position")
           ~y:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Y Position")
      |> with_grid (grid_of_model m)
      |> scatter ~id:"scatter" ~label:"Points" ~mode:m.scatter_mode ~glyph:"·"
           ~x:fst ~y:snd scatter_data
  | Bar ->
      let categories = Array.to_list (Array.map fst bar_data) in
      empty ~theme ()
      |> with_title "Weekly Activity"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
      |> with_x_scale (Scale.band ~categories ~padding:0.18 ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 0 |> Axis.with_title "Day")
           ~y:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Count")
      |> with_grid (grid_of_model m)
      |> bar ~id:"bars" ~label:"Activity" ~category:fst ~value:snd bar_data
      |> rule 0.0
  | Stacked_bar ->
      let categories =
        Array.to_list (Array.map (fun b -> b.Mark.category) stacked_bar_data)
      in
      empty ~theme ()
      |> with_title "Quarterly Revenue"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
      |> with_x_scale (Scale.band ~categories ~padding:0.18 ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 0 |> Axis.with_title "Quarter")
           ~y:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Revenue")
      |> with_grid (grid_of_model m)
      |> stacked_bar ~id:"stacked" stacked_bar_data
      |> rule 0.0
  | Heatmap ->
      empty ~theme () |> with_title "Intensity Map"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 7) ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Column")
           ~y:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Row")
      |> with_grid (if m.show_grid then grid_of_model m else Gridlines.hidden)
      |> heatmap ~id:"heat" ~auto_value_range:true ~mode:m.heatmap_mode
           ~x:(fun (x, _, _) -> x)
           ~y:(fun (_, y, _) -> y)
           ~value:(fun (_, _, v) -> v)
           heatmap_data
  | Candlestick ->
      let title =
        if m.use_log_scale then "Price Action (Log Scale)" else "Price Action"
      in
      let base =
        empty ~theme () |> with_title title
        |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
        |> with_axes
             ~x:(Axis.default |> Axis.with_ticks 7 |> Axis.with_title "Time")
             ~y:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Price")
        |> with_grid (grid_of_model m)
      in
      let chart =
        if m.use_log_scale then base |> with_y_scale (Scale.log ()) else base
      in
      chart |> candles ~id:"ohlc" candlestick_data
  | Area ->
      let area_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 100 180 255) () in
      let line_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 50 130 220) () in
      empty ~theme ()
      |> with_title "CPU Usage Over Time"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Time (s)")
           ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_title "Usage (%)")
      |> with_grid (grid_of_model m)
      |> area ~id:"cpu" ~label:"CPU" ~style:area_style ~baseline:`Zero
           ~resolution:`Cell ~x:fst ~y:snd area_data
      |> line ~id:"cpu-line" ~style:line_style ~resolution:`Braille2x4 ~x:fst
           ~y:snd area_data
  | Histogram ->
      let hist_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 150 200 100) () in
      empty ~theme ()
      |> with_title "Value Distribution"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Value")
           ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_title "Count")
      |> with_grid (grid_of_model m)
      |> histogram ~id:"dist" ~label:"Distribution" ~style:hist_style
           ~bins:(Mark.Bins 20) ~normalize:`Count ~x:Fun.id histogram_data
  | Dual_axis ->
      let price_style =
        Ansi.Style.make ~fg:(Ansi.Color.of_rgb 100 200 255) ()
      in
      let volume_style =
        Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 180 100) ()
      in
      empty ~theme ()
      |> with_title "Price & Volume (Dual Axis)"
      |> with_frame (manual_frame ~margins:(2, 2, 3, 8) ())
      |> with_axes
           ~x:(Axis.default |> Axis.with_ticks 6 |> Axis.with_title "Time")
           ~y:(Axis.default |> Axis.with_ticks 5 |> Axis.with_title "Price ($)")
      |> with_y2_scale (Scale.numeric ())
      |> with_y2_axis
           (Axis.default |> Axis.with_ticks 4 |> Axis.with_title "Volume")
      |> with_grid (grid_of_model m)
      (* Volume as area on Y2 axis (drawn first, behind price line) *)
      |> area ~id:"volume" ~label:"Volume" ~style:volume_style ~y_axis:`Y2
           ~baseline:`Zero ~resolution:`Cell ~x:fst ~y:snd dual_axis_volume_data
      (* Price line on Y1 axis *)
      |> line ~id:"price" ~label:"Price" ~style:price_style
           ~resolution:`Braille2x4 ~x:fst ~y:snd dual_axis_price_data

(* Mutable ref to capture canvas size from draw callback for pointer handlers *)
let canvas_size_ref : (int * int) ref = ref (0, 0)

(* --- Layout helpers used in update (interaction) --- *)

let with_layout_for (m : model) ~(view : View.t) (f : Layout.t -> model * 'a) :
    (model * 'a) option =
  let w, h = !canvas_size_ref in
  if w = 0 || h = 0 then None
  else
    let chart = spec_for m m.chart in
    let layout = Matrix_charts.layout ~view chart ~width:w ~height:h in
    Some (f layout)

let plot_center_px (layout : Layout.t) : int * int =
  let r = Layout.plot_rect layout in
  (r.x + (max 0 (r.width - 1) / 2), r.y + (max 0 (r.height - 1) / 2))

let clamp_view (layout : Layout.t) (v : View.t) : View.t =
  Layout.clamp_view layout v

(* --- Hit/tooltip formatting --- *)

let string_of_hit_kind : Hit.kind -> string = function
  | `Line -> "line"
  | `Scatter -> "scatter"
  | `Bars -> "bars"
  | `Stacked_bars -> "stacked bars"
  | `Heatmap -> "heatmap"
  | `Candles -> "candles"
  | `Circle -> "circle"

let hit_params (ct : chart_type) : int * Hit.policy =
  match ct with
  | Line | Area | Dual_axis -> (4, `Nearest_x)
  | Scatter -> (4, `Nearest_px)
  | Bar | Stacked_bar | Histogram -> (2, `Nearest_px)
  | Heatmap -> (3, `Nearest_px)
  | Candlestick -> (2, `Nearest_x)

let x_center_for_category (layout : Layout.t) (category : string) : float =
  let fallback () =
    let w = Layout.x_view layout in
    (w.min +. w.max) /. 2.0
  in
  match Layout.px_of_x_category layout category with
  | None -> fallback ()
  | Some px -> (
      let r = Layout.plot_rect layout in
      let py = r.y + (max 0 (r.height - 1) / 2) in
      match Layout.data_of_px layout ~px ~py with
      | None -> fallback ()
      | Some (x, _) -> x)

let cursor_readout_lines (layout : Layout.t) ~(px : int) ~(py : int) :
    string list =
  match Layout.data_of_px layout ~px ~py with
  | None -> []
  | Some (x, y) ->
      let base = [ Printf.sprintf "x: %.3g" x; Printf.sprintf "y: %.3g" y ] in
      let extra =
        match Layout.x_category_of_px layout ~px with
        | None -> []
        | Some cat -> [ "category: " ^ cat ]
      in
      extra @ base

let hit_tooltip (layout : Layout.t) (hit : Hit.t) : float * float * string list
    =
  let header =
    let mark =
      match hit.mark_id with None -> "" | Some id -> Printf.sprintf " (%s)" id
    in
    Printf.sprintf "%s%s" (string_of_hit_kind hit.kind) mark
  in
  match hit.payload with
  | Hit.XY { x; y } ->
      ( x,
        y,
        [
          header;
          Printf.sprintf "x: %.3g" x;
          Printf.sprintf "y: %.3g" y;
          Printf.sprintf "index: %d" hit.index;
        ] )
  | Hit.Bar { category; value } ->
      ( x_center_for_category layout category,
        value,
        [ header; "category: " ^ category; Printf.sprintf "value: %.3g" value ]
      )
  | Hit.Stacked_bar { category; segment_index; value; total } ->
      ( x_center_for_category layout category,
        total,
        [
          header;
          "category: " ^ category;
          Printf.sprintf "segment: %d" segment_index;
          Printf.sprintf "value: %.3g (of %.3g)" value total;
        ] )
  | Hit.Heat { x; y; value } ->
      ( x,
        y,
        [
          header;
          Printf.sprintf "x: %.3g" x;
          Printf.sprintf "y: %.3g" y;
          Printf.sprintf "value: %.3g" value;
        ] )
  | Hit.OHLC { time; open_; high; low; close } ->
      let x = time and y = close in
      ( x,
        y,
        [
          header;
          Printf.sprintf "t: %.3g" time;
          Printf.sprintf "O: %.3g" open_;
          Printf.sprintf "H: %.3g" high;
          Printf.sprintf "L: %.3g" low;
          Printf.sprintf "C: %.3g" close;
        ] )

(* --- Update (interaction state machine) --- *)

let cycle_heatmap_render = function
  | Mark.Cells_fg -> Mark.Cells_bg
  | Mark.Cells_bg -> Mark.Halfblock_fg_bg
  | Mark.Halfblock_fg_bg -> Mark.Shaded
  | Mark.Shaded -> Mark.Dense_bilinear
  | Mark.Dense_bilinear -> Mark.Cells_fg

let update (msg : msg) (m : model) =
  match msg with
  | Next_chart ->
      ( { m with chart = next_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Prev_chart ->
      ( { m with chart = prev_chart m.chart; hover = None; dragging = None },
        Cmd.none )
  | Pointer_leave -> ({ m with hover = None; dragging = None }, Cmd.none)
  | Pointer_up -> ({ m with dragging = None }, Cmd.none)
  | Pointer_down (px, py) -> (
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            if Layout.is_inside_plot layout ~px ~py then
              ( {
                  m with
                  hover = Some (px, py);
                  dragging =
                    Some { start_px = px; start_py = py; start_view = view };
                },
                () )
            else ({ m with hover = Some (px, py); dragging = None }, ()))
      with
      | None -> ({ m with hover = Some (px, py) }, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Pointer_move (px, py) -> (
      match m.dragging with
      | None -> ({ m with hover = Some (px, py) }, Cmd.none)
      | Some d -> (
          (* Pan relative to drag start. Use layout compiled at drag.start_view
             to keep the mapping stable even while the view changes. *)
          match
            with_layout_for m ~view:d.start_view (fun layout ->
                let dx = d.start_px - px in
                let dy = py - d.start_py in
                let view' =
                  Layout.pan_view_by_px layout ~view:d.start_view ~dx ~dy
                  |> clamp_view layout
                in
                let m' = set_view m view' in
                ({ m' with hover = Some (px, py); dragging = Some d }, ()))
          with
          | None -> ({ m with hover = Some (px, py) }, Cmd.none)
          | Some (m', ()) -> (m', Cmd.none)))
  | Scroll_zoom (px, py, dir) -> (
      let factor = match dir with `In -> 1.25 | `Out -> 0.8 in
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            (* If cursor isn't inside plot, zoom around plot center. *)
            let px, py =
              if Layout.is_inside_plot layout ~px ~py then (px, py)
              else plot_center_px layout
            in
            let view' =
              Layout.zoom_view_around_px layout ~view ~axis:`Both ~px ~py
                ~factor
              |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Zoom_center dir -> (
      let factor = match dir with `In -> 1.25 | `Out -> 0.8 in
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            let px, py = plot_center_px layout in
            let view' =
              Layout.zoom_view_around_px layout ~view ~axis:`Both ~px ~py
                ~factor
              |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Pan_by_keys (dx, dy) -> (
      let view = get_view m in
      match
        with_layout_for m ~view (fun layout ->
            let view' =
              Layout.pan_view_by_px layout ~view ~dx ~dy |> clamp_view layout
            in
            (set_view m view', ()))
      with
      | None -> (m, Cmd.none)
      | Some (m', ()) -> (m', Cmd.none))
  | Reset_view -> (set_view m View.empty, Cmd.none)
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
          else if m.line_resolution = `Braille2x4 then
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
          ( { m with heatmap_mode = cycle_heatmap_render m.heatmap_mode },
            Cmd.none )
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

let draw_hover_overlay (m : model) (layout : Layout.t) (grid : Cell_grid.t) =
  match m.hover with
  | None -> ()
  | Some (px, py) -> (
      if not (Layout.is_inside_plot layout ~px ~py) then ()
      else
        let radius, policy = hit_params m.chart in
        match Layout.hit_test layout ~px ~py ~radius ~policy with
        | Some hit ->
            let x, y, lines = hit_tooltip layout hit in
            Overlay.crosshair layout grid ~x ~y;
            Overlay.marker layout grid ~x ~y;
            Overlay.tooltip layout grid ~x ~y lines
        | None -> (
            (* Free cursor readout when not near a mark *)
            match Layout.data_of_px layout ~px ~py with
            | None -> ()
            | Some (x, y) ->
                let lines = cursor_readout_lines layout ~px ~py in
                Overlay.crosshair layout grid ~x ~y;
                Overlay.tooltip ~anchor:`Right layout grid ~x ~y lines))

let draw_help_overlay (m : model) (layout : Layout.t) (grid : Cell_grid.t) =
  if not m.show_help then ()
  else
    let r = Layout.plot_rect layout in
    let px = r.x + (max 0 (r.width - 1) / 2) in
    let py = r.y + (max 0 (r.height - 1) / 2) in
    match Layout.data_of_px layout ~px ~py with
    | None -> ()
    | Some (x, y) ->
        let mode_line =
          match m.chart with
          | Line ->
              [
                "m: cycle line mode (Cell/Wave/Block2x2/Braille + pattern)";
                "p: toggle data points";
                "s: toggle smoothing (EMA)";
                "f: toggle confidence bands";
              ]
          | Scatter -> [ "m: cycle scatter mode (Cell/Braille/Density)" ]
          | Heatmap ->
              [ "m: cycle heatmap mode (Cells/Halfblock/Shaded/Dense)" ]
          | Bar | Stacked_bar -> []
          | Candlestick -> [ "l: toggle log scale" ]
          | Area -> []
          | Histogram -> []
          | Dual_axis -> []
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
        Overlay.tooltip ~anchor:`Top layout grid ~x ~y lines

let draw_legend (layout : Layout.t) (grid : Cell_grid.t) =
  let items = Legend.items_of_layout layout in
  if items = [] then ()
  else
    let r = Layout.plot_rect layout in
    (* Position legend in top-right corner of plot, with some padding *)
    let legend_x = r.x + r.width - 1 in
    let legend_y = r.y + 1 in
    List.iteri
      (fun i { Legend.label; style; marker } ->
        let y = legend_y + i in
        if y < r.y + r.height - 1 then (
          let text = marker ^ " " ^ label in
          let w = String.length text in
          let x = max r.x (legend_x - w) in
          Cell_grid.draw_text grid ~x ~y ~style ~text:marker;
          Cell_grid.draw_text grid
            ~x:(x + String.length marker + 1)
            ~y ~text:label))
      items

let draw_chart (m : model) (grid : Cell_grid.t) ~(width : int) ~(height : int) :
    Layout.t =
  let chart = spec_for m m.chart in
  let view = get_view m in
  let layout = Matrix_charts.draw ~view chart grid ~width ~height in
  draw_legend layout grid;
  draw_hover_overlay m layout grid;
  draw_help_overlay m layout grid;
  layout

(* --- View --- *)

let heatmap_mode_name = function
  | Mark.Cells_fg -> "Cells_fg"
  | Mark.Cells_bg -> "Cells_bg"
  | Mark.Halfblock_fg_bg -> "Halfblock"
  | Mark.Shaded -> "Shaded"
  | Mark.Dense_bilinear -> "Dense"

let view (m : model) =
  let idx = chart_index m.chart in
  let n = List.length all_charts in
  let view_to_s = function
    | None -> "auto"
    | Some (w : View.window) -> Printf.sprintf "%.3g..%.3g" w.min w.max
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
      style_info (view_to_s v.x) (view_to_s v.y)
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
            ~on_mouse:(fun ev ->
              let px, py = (Event.Mouse.x ev, Event.Mouse.y ev) in
              match Event.Mouse.kind ev with
              | Move | Drag _ -> Some (Pointer_move (px, py))
              | Out -> Some Pointer_leave
              | Down _ -> Some (Pointer_down (px, py))
              | Up _ | Drag_end _ -> Some Pointer_up
              | Scroll { direction = Scroll_up; _ } ->
                  Some (Scroll_zoom (px, py, `In))
              | Scroll { direction = Scroll_down; _ } ->
                  Some (Scroll_zoom (px, py, `Out))
              | _ -> None)
            ~size:{ width = pct 100; height = pct 100 }
            (fun c ~delta:_ ->
              let w = Canvas.width c in
              let h = Canvas.height c in
              canvas_size_ref := (w, h);
              Canvas.clear c;
              ignore (draw_chart m (Canvas.grid c) ~width:w ~height:h));
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
      let step = if data.modifier.shift then 18 else 8 in
      match data.key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      (* Chart switching *)
      | Tab when not data.modifier.shift -> Some Next_chart
      | Tab when data.modifier.shift -> Some Prev_chart
      | Char c when Uchar.equal c (Uchar.of_char '>') -> Some Next_chart
      | Char c when Uchar.equal c (Uchar.of_char '<') -> Some Prev_chart
      (* Zoom *)
      | Char c when Uchar.equal c (Uchar.of_char '+') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '=') -> Some (Zoom_center `In)
      | Char c when Uchar.equal c (Uchar.of_char '-') -> Some (Zoom_center `Out)
      (* Pan (grab semantics) *)
      | Left -> Some (Pan_by_keys (step, 0)) (* move content left *)
      | Right -> Some (Pan_by_keys (-step, 0)) (* move content right *)
      | Up -> Some (Pan_by_keys (0, -step)) (* move content up *)
      | Down -> Some (Pan_by_keys (0, step)) (* move content down *)
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
