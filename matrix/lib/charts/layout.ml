module Style = Ansi.Style
module Color = Ansi.Color

(* {1 Charset} *)

module Charset = struct
  type line_pattern = [ `Solid | `Dashed | `Dotted ]

  type frame = {
    tl : string;
    tr : string;
    bl : string;
    br : string;
    h : string;
    v : string;
    tee_up : string;
    tee_down : string;
    tee_left : string;
    tee_right : string;
    cross : string;
  }

  type t = {
    frame : frame;
    axis_h : string;
    axis_v : string;
    tick_h : string;
    tick_v : string;
    grid_h_solid : string;
    grid_v_solid : string;
    grid_h_dashed : string;
    grid_v_dashed : string;
    grid_h_dotted : string;
    grid_v_dotted : string;
    point_default : string;
    point_heavy : string;
    bar_fill : string;
    shade_levels : string array;
    tooltip_frame : frame;
    diag_up : string;
    diag_down : string;
  }

  let ascii_frame =
    {
      tl = "+";
      tr = "+";
      bl = "+";
      br = "+";
      h = "-";
      v = "|";
      tee_up = "+";
      tee_down = "+";
      tee_left = "+";
      tee_right = "+";
      cross = "+";
    }

  let light_frame =
    {
      tl = "\xE2\x94\x8C";
      tr = "\xE2\x94\x90";
      bl = "\xE2\x94\x94";
      br = "\xE2\x94\x98";
      h = "\xE2\x94\x80";
      v = "\xE2\x94\x82";
      tee_up = "\xE2\x94\xB4";
      tee_down = "\xE2\x94\xAC";
      tee_left = "\xE2\x94\xA4";
      tee_right = "\xE2\x94\x9C";
      cross = "\xE2\x94\xBC";
    }

  let heavy_frame =
    {
      tl = "\xE2\x94\x8F";
      tr = "\xE2\x94\x93";
      bl = "\xE2\x94\x97";
      br = "\xE2\x94\x9B";
      h = "\xE2\x94\x81";
      v = "\xE2\x94\x83";
      tee_up = "\xE2\x94\xBB";
      tee_down = "\xE2\x94\xB3";
      tee_left = "\xE2\x94\xAB";
      tee_right = "\xE2\x94\xA3";
      cross = "\xE2\x95\x8B";
    }

  let rounded_frame =
    {
      tl = "\xE2\x95\xAD";
      tr = "\xE2\x95\xAE";
      bl = "\xE2\x95\xB0";
      br = "\xE2\x95\xAF";
      h = "\xE2\x94\x80";
      v = "\xE2\x94\x82";
      tee_up = "\xE2\x94\xB4";
      tee_down = "\xE2\x94\xAC";
      tee_left = "\xE2\x94\xA4";
      tee_right = "\xE2\x94\x9C";
      cross = "\xE2\x94\xBC";
    }

  let ascii =
    {
      frame = ascii_frame;
      axis_h = "-";
      axis_v = "|";
      tick_h = "-";
      tick_v = "|";
      grid_h_solid = "-";
      grid_v_solid = "|";
      grid_h_dashed = "-";
      grid_v_dashed = "|";
      grid_h_dotted = ".";
      grid_v_dotted = ":";
      point_default = "*";
      point_heavy = "O";
      bar_fill = "#";
      shade_levels = [| " "; "."; "o"; "O"; "#" |];
      tooltip_frame = ascii_frame;
      diag_up = "/";
      diag_down = "\\";
    }

  let unicode_light =
    {
      frame = light_frame;
      axis_h = "─";
      axis_v = "│";
      tick_h = "─";
      tick_v = "│";
      grid_h_solid = "─";
      grid_v_solid = "│";
      grid_h_dashed = "┄";
      grid_v_dashed = "┆";
      grid_h_dotted = "┈";
      grid_v_dotted = "┊";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = rounded_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let unicode_heavy =
    {
      frame = heavy_frame;
      axis_h = "━";
      axis_v = "┃";
      tick_h = "━";
      tick_v = "┃";
      grid_h_solid = "━";
      grid_v_solid = "┃";
      grid_h_dashed = "┅";
      grid_v_dashed = "┇";
      grid_h_dotted = "┉";
      grid_v_dotted = "┋";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = heavy_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let unicode_rounded =
    {
      frame = rounded_frame;
      axis_h = "─";
      axis_v = "│";
      tick_h = "─";
      tick_v = "│";
      grid_h_solid = "─";
      grid_v_solid = "│";
      grid_h_dashed = "┄";
      grid_v_dashed = "┆";
      grid_h_dotted = "┈";
      grid_v_dotted = "┊";
      point_default = "∙";
      point_heavy = "●";
      bar_fill = "█";
      shade_levels = [| " "; "░"; "▒"; "▓"; "█" |];
      tooltip_frame = rounded_frame;
      diag_up = "╱";
      diag_down = "╲";
    }

  let default = unicode_light
end

(* {1 Raster} *)

module Raster = struct
  type resolution = [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]
end

(* {1 Utilities} *)

let clamp_int lo hi v = if v < lo then lo else if v > hi then hi else v
let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

let safe_domain_range (a : float) (b : float) =
  let eps = 1e-12 in
  if Float.abs (b -. a) < eps then (a -. 1.0, b +. 1.0) else (a, b)

let safe_view_range (a : float) (b : float) =
  let epsilon = 1e-12 in
  let span = Float.abs (b -. a) in
  if span >= epsilon then (a, b)
  else
    let scale = Float.max (Float.abs a) (Float.abs b) in
    let expansion = Float.max (epsilon /. 2.) (1e-6 *. scale) in
    (a -. expansion, b +. expansion)

let safe_range = safe_domain_range

let text_width (s : string) : int =
  Text.measure ~width_method:`Unicode ~tab_width:2 s

let lerp a b t = a +. ((b -. a) *. t)

let nice_number x round =
  let exp = Float.floor (Float.log10 x) in
  let f = x /. Float.pow 10. exp in
  let nf =
    if round then
      if f < 1.5 then 1. else if f < 3. then 2. else if f < 7. then 5. else 10.
    else if f <= 1. then 1.
    else if f <= 2. then 2.
    else if f <= 5. then 5.
    else 10.
  in
  nf *. Float.pow 10. exp

let nice_ticks ~min_val ~max_val ~target_ticks =
  if Float.abs (max_val -. min_val) < 1e-12 then [ min_val ]
  else
    let range = max_val -. min_val in
    let target_ticks = Float.max 2. (float target_ticks) in
    let step = nice_number (range /. (target_ticks -. 1.)) true in
    let start = Float.floor (min_val /. step) *. step in
    let stop = Float.ceil (max_val /. step) *. step in
    let rec gen acc v =
      if v > stop +. (step *. 0.5) then List.rev acc
      else if v >= min_val -. (step *. 0.5) && v <= max_val +. (step *. 0.5)
      then gen (v :: acc) (v +. step)
      else gen acc (v +. step)
    in
    let ticks = gen [] start in
    if ticks = [] then [ min_val; max_val ] else ticks

let log_transform ~base v =
  if v <= 0. then Float.neg_infinity else Float.log v /. Float.log base

let log_ticks ~base ~min_val ~max_val ~target_ticks =
  if min_val <= 0. || max_val <= 0. then [ min_val; max_val ]
  else
    let log_min = Float.floor (log_transform ~base min_val) in
    let log_max = Float.ceil (log_transform ~base max_val) in
    let num_decades = log_max -. log_min in
    let rec gen_major acc exp =
      if exp > log_max +. 0.5 then List.rev acc
      else
        let v = Float.pow base exp in
        if v >= min_val *. 0.999 && v <= max_val *. 1.001 then
          gen_major (v :: acc) (exp +. 1.)
        else gen_major acc (exp +. 1.)
    in
    let major = gen_major [] log_min in
    if List.length major < target_ticks && num_decades <= 3. then
      let minors =
        if base = 10. then
          let rec gen_minor acc exp =
            if exp >= log_max then acc
            else
              let base_val = Float.pow base exp in
              let m2 = base_val *. 2. in
              let m5 = base_val *. 5. in
              let acc =
                if m2 >= min_val && m2 <= max_val then m2 :: acc else acc
              in
              let acc =
                if m5 >= min_val && m5 <= max_val then m5 :: acc else acc
              in
              gen_minor acc (exp +. 1.)
          in
          gen_minor [] log_min
        else []
      in
      List.sort Float.compare (major @ minors)
    else if major = [] then [ min_val; max_val ]
    else major

(* Scale-to-pixel helpers used by rendering and axis drawing *)

let scale_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  if Float.abs (maxv -. minv) < 1e-12 then origin
  else if extent <= 1 then origin
  else
    let t = (v -. minv) /. (maxv -. minv) in
    let t = if clamp then clamp01 t else t in
    origin + int_of_float (Float.round (t *. float (extent - 1)))

let y_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  scale_to_px ~minv:maxv ~maxv:minv ~extent ~origin ~clamp v

let x_to_px ~minv ~maxv ~extent ~origin ~clamp v =
  scale_to_px ~minv ~maxv ~extent ~origin ~clamp v

let log_scale_to_px ~base ~minv ~maxv ~extent ~origin ~clamp v =
  let safe_minv = Float.max 1e-10 minv in
  let safe_maxv = Float.max 1e-10 maxv in
  if Float.abs (safe_maxv -. safe_minv) < 1e-12 then origin
  else if extent <= 1 then origin
  else
    let log_min = log_transform ~base safe_minv in
    let log_max = log_transform ~base safe_maxv in
    let safe_v = Float.max 1e-10 v in
    let log_v = log_transform ~base safe_v in
    let t = (log_v -. log_min) /. (log_max -. log_min) in
    let t = if clamp then clamp01 t else t in
    origin + int_of_float (Float.round (t *. float (extent - 1)))

let y_to_px_log ~base ~minv ~maxv ~extent ~origin ~clamp v =
  log_scale_to_px ~base ~minv:maxv ~maxv:minv ~extent ~origin ~clamp v

let x_to_px_log ~base ~minv ~maxv ~extent ~origin ~clamp v =
  log_scale_to_px ~base ~minv ~maxv ~extent ~origin ~clamp v

(* {1 Theme} *)

module Theme = struct
  type t = {
    palette : Color.t array;
    background : Color.t option;
    axes : Style.t;
    border : Style.t;
    grid : Style.t;
    grid_minor : Style.t;
    labels : Style.t;
    tooltip : Style.t;
    tooltip_border : Style.t option;
    crosshair : Style.t;
    marker : Style.t;
    charset : Charset.t;
  }

  let default_palette =
    [|
      Color.cyan;
      Color.magenta;
      Color.yellow;
      Color.green;
      Color.blue;
      Color.red;
      Color.indexed 33;
      Color.indexed 39;
      Color.indexed 45;
    |]

  let dark =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:13) ();
      border = Style.make ~fg:(Color.grayscale ~level:10) ();
      grid = Style.make ~fg:(Color.grayscale ~level:5) ();
      grid_minor = Style.make ~fg:(Color.grayscale ~level:3) ();
      labels = Style.make ~fg:(Color.grayscale ~level:14) ();
      tooltip = Style.make ~fg:Color.white ~bg:(Color.grayscale ~level:4) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:10) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:10) ();
      marker = Style.make ~fg:Color.yellow ~bold:true ();
      charset = Charset.unicode_light;
    }

  let light =
    {
      palette = default_palette;
      background = None;
      axes = Style.make ~fg:(Color.grayscale ~level:2) ();
      border = Style.make ~fg:(Color.grayscale ~level:5) ();
      grid = Style.make ~fg:(Color.grayscale ~level:18) ();
      grid_minor = Style.make ~fg:(Color.grayscale ~level:20) ();
      labels = Style.make ~fg:(Color.grayscale ~level:1) ();
      tooltip = Style.make ~fg:Color.black ~bg:(Color.grayscale ~level:20) ();
      tooltip_border = Some (Style.make ~fg:(Color.grayscale ~level:8) ());
      crosshair = Style.make ~fg:(Color.grayscale ~level:8) ();
      marker = Style.make ~fg:Color.red ~bold:true ();
      charset = Charset.unicode_light;
    }

  let default = dark
  let with_charset charset t = { t with charset }
end

(* {1 Scale} *)

module Scale = struct
  type numeric_domain = [ `Auto | `Domain of float * float ]

  type t =
    | Auto
    | Numeric of { domain : numeric_domain; clamp : bool }
    | Log of { base : float; domain : numeric_domain; clamp : bool }
    | Band of { categories : string list option; padding : float }

  let numeric ?(domain = `Auto) ?(clamp = true) () = Numeric { domain; clamp }

  let log ?(base = 10.) ?(domain = `Auto) ?(clamp = true) () =
    let base = if base <= 1. then 10. else base in
    Log { base; domain; clamp }

  let band ?categories ?(padding = 0.1) () =
    let padding = Float.min 0.95 (clamp01 padding) in
    Band { categories; padding }
end

(* {1 Axis} *)

module Axis = struct
  type formatter = int -> float -> string
  type line = [ `None | `Axis_only | `Frame ]
  type title = { text : string; style : Style.t option }

  type t = {
    show : bool;
    line : line;
    ticks : int;
    format : formatter;
    style : Style.t option;
    tick_style : Style.t option;
    label_style : Style.t option;
    tick_length : int;
    label_padding : int;
    title : title option;
  }

  let default_format : formatter = fun _ v -> Printf.sprintf "%.*g" 3 v

  let hidden =
    {
      show = false;
      line = `None;
      ticks = 0;
      format = default_format;
      style = None;
      tick_style = None;
      label_style = None;
      tick_length = 0;
      label_padding = 0;
      title = None;
    }

  let default =
    {
      show = true;
      line = `Axis_only;
      ticks = 6;
      format = default_format;
      style = None;
      tick_style = None;
      label_style = None;
      tick_length = 1;
      label_padding = 1;
      title = None;
    }

  let with_ticks ticks a = { a with ticks = max 0 ticks }
  let with_format format a = { a with format }
  let with_style style a = { a with style = Some style }
  let with_tick_style tick_style a = { a with tick_style = Some tick_style }
  let with_label_style label_style a = { a with label_style = Some label_style }

  let with_tick_length tick_length a =
    { a with tick_length = max 0 tick_length }

  let with_label_padding label_padding a =
    { a with label_padding = max 0 label_padding }

  let with_line line a = { a with line }
  let with_title ?style text a = { a with title = Some { text; style } }
end

(* {1 Gridlines} *)

module Gridlines = struct
  type t = {
    show : bool;
    x : bool;
    y : bool;
    style : Style.t;
    pattern : Charset.line_pattern;
    x_step : int option;
    y_step : int option;
    minor : int option;
    minor_style : Style.t option;
  }

  let hidden =
    {
      show = false;
      x = true;
      y = true;
      style = Style.default;
      pattern = `Dotted;
      x_step = None;
      y_step = None;
      minor = None;
      minor_style = None;
    }

  let default =
    {
      show = true;
      x = true;
      y = true;
      style = Style.make ~dim:true ();
      pattern = `Dotted;
      x_step = None;
      y_step = None;
      minor = None;
      minor_style = None;
    }

  let with_style style g = { g with style }
  let with_pattern pattern g = { g with pattern }
  let with_x x g = { g with x }
  let with_y y g = { g with y }
  let with_x_step x_step g = { g with x_step }
  let with_y_step y_step g = { g with y_step }
  let with_minor minor g = { g with minor }
  let with_minor_style minor_style g = { g with minor_style }
end

(* {1 View} *)

module View = struct
  type window = { min : float; max : float }
  type t = { x : window option; y : window option; y2 : window option }

  let empty = { x = None; y = None; y2 = None }
  let set_x x t = { t with x }
  let set_y y t = { t with y }
  let set_y2 y2 t = { t with y2 }

  let window ~min ~max =
    let min, max = if min <= max then (min, max) else (max, min) in
    let min, max = safe_view_range min max in
    { min; max }

  let zoom (w : window) ~factor =
    let factor = if factor <= 0. then 1.0 else factor in
    let center = (w.min +. w.max) /. 2.0 in
    let half = (w.max -. w.min) /. 2.0 /. factor in
    window ~min:(center -. half) ~max:(center +. half)

  let zoom_around (w : window) ~center ~factor =
    let factor = if factor <= 0. then 1.0 else factor in
    let range = w.max -. w.min in
    if Float.abs range < 1e-12 then w
    else
      let new_range = range /. factor in
      let ratio = (center -. w.min) /. range in
      let new_min = center -. (ratio *. new_range) in
      window ~min:new_min ~max:(new_min +. new_range)

  let pan (w : window) ~delta =
    window ~min:(w.min +. delta) ~max:(w.max +. delta)

  let clamp ~domain (w : window) =
    let size = w.max -. w.min in
    let dom_size = domain.max -. domain.min in
    if size <= 0. || dom_size <= 0. then domain
    else if size >= dom_size then domain
    else
      let min' = Float.max domain.min (Float.min w.min (domain.max -. size)) in
      { min = min'; max = min' +. size }
end

(* {1 Hit} *)

module Hit = struct
  type policy = [ `Nearest_px | `Nearest_x | `Nearest_y ]

  type kind =
    [ `Line | `Scatter | `Bars | `Stacked_bars | `Heatmap | `Candles | `Circle ]

  type payload =
    | XY of { x : float; y : float }
    | Bar of { category : string; value : float }
    | Stacked_bar of {
        category : string;
        segment_index : int;
        value : float;
        total : float;
      }
    | Heat of { x : float; y : float; value : float }
    | OHLC of {
        time : float;
        open_ : float;
        high : float;
        low : float;
        close : float;
      }

  type t = {
    mark_id : string option;
    kind : kind;
    index : int;
    px : int;
    py : int;
    distance_px : float;
    payload : payload;
  }
end

(* {1 Internal Scale Representation} *)

type axis_kind =
  | Numeric of { domain : View.window; view : View.window; clamp : bool }
  | Log of {
      base : float;
      domain : View.window;
      view : View.window;
      clamp : bool;
    }
  | Band of {
      categories : string list;
      categories_arr : string array;
      index_of : (string, int) Hashtbl.t;
      padding : float;
      domain : View.window;
      view : View.window;
    }

let make_band ~categories ~padding ~domain ~view =
  let categories_arr = Array.of_list categories in
  let index_of = Hashtbl.create (List.length categories) in
  List.iteri (fun i cat -> Hashtbl.add index_of cat i) categories;
  Band { categories; categories_arr; index_of; padding; domain; view }

(* {1 Layout Type} *)

type frame_config = { margins : int * int * int * int; inner_padding : int }
type frame = Auto | Manual of frame_config
type rect = { x : int; y : int; width : int; height : int }
type title_info = { text : string; style : Style.t }

type t = {
  width : int;
  height : int;
  plot : rect;
  theme : Theme.t;
  x_scale : axis_kind;
  y_scale : axis_kind;
  y2_scale : axis_kind option;
  x_domain : View.window;
  y_domain : View.window;
  y2_domain : View.window option;
  x_view : View.window;
  y_view : View.window;
  y2_view : View.window option;
  x_ticks : float list;
  y_ticks : float list;
  y2_ticks : float list option;
  x_axis : Axis.t;
  y_axis : Axis.t;
  y2_axis : Axis.t option;
  grid : Gridlines.t;
  frame_inner_padding : int;
  margin_left : int;
  y_axis_width : int;
  y_axis_title_width : int;
  y2_axis_width : int;
  marks : Mark.t list;
  title : title_info option;
}

(* {1 Layout Queries} *)

let rect_contains r ~x ~y =
  x >= r.x && x < r.x + r.width && y >= r.y && y < r.y + r.height

let size t = (t.width, t.height)
let plot_rect t = t.plot
let is_inside_plot t ~px ~py = rect_contains t.plot ~x:px ~y:py
let x_domain t = t.x_domain
let y_domain t = t.y_domain
let y2_domain t = t.y2_domain
let x_view t = t.x_view
let y_view t = t.y_view
let y2_view t = t.y2_view
let y_axis_title_width t = t.y_axis_title_width
let y2_axis_width t = t.y2_axis_width
let has_y2 t = Option.is_some t.y2_scale && Option.is_some t.y2_axis

(* {1 Coordinate Transforms} *)

let inverse_x_transform t tx =
  match t.x_scale with
  | Numeric _ | Band _ -> lerp t.x_view.min t.x_view.max tx
  | Log { base; view; _ } ->
      let safe_min = Float.max 1e-10 view.min in
      let safe_max = Float.max 1e-10 view.max in
      let log_min = log_transform ~base safe_min in
      let log_max = log_transform ~base safe_max in
      let log_v = lerp log_min log_max tx in
      Float.pow base log_v

let inverse_y_transform t ty =
  match t.y_scale with
  | Numeric _ | Band _ -> lerp t.y_view.max t.y_view.min ty
  | Log { base; view; _ } ->
      let safe_min = Float.max 1e-10 view.min in
      let safe_max = Float.max 1e-10 view.max in
      let log_min = log_transform ~base safe_min in
      let log_max = log_transform ~base safe_max in
      let log_v = lerp log_max log_min ty in
      Float.pow base log_v

let forward_x_transform t x =
  match t.x_scale with
  | Numeric _ | Band _ ->
      if Float.abs (t.x_view.max -. t.x_view.min) < 1e-12 then 0.
      else (x -. t.x_view.min) /. (t.x_view.max -. t.x_view.min)
  | Log { base; view; _ } ->
      let safe_min = Float.max 1e-10 view.min in
      let safe_max = Float.max 1e-10 view.max in
      let safe_x = Float.max 1e-10 x in
      let log_min = log_transform ~base safe_min in
      let log_max = log_transform ~base safe_max in
      if Float.abs (log_max -. log_min) < 1e-12 then 0.
      else (log_transform ~base safe_x -. log_min) /. (log_max -. log_min)

let forward_y_transform t y =
  match t.y_scale with
  | Numeric _ | Band _ ->
      if Float.abs (t.y_view.max -. t.y_view.min) < 1e-12 then 0.
      else (t.y_view.max -. y) /. (t.y_view.max -. t.y_view.min)
  | Log { base; view; _ } ->
      let safe_min = Float.max 1e-10 view.min in
      let safe_max = Float.max 1e-10 view.max in
      let safe_y = Float.max 1e-10 y in
      let log_min = log_transform ~base safe_min in
      let log_max = log_transform ~base safe_max in
      if Float.abs (log_max -. log_min) < 1e-12 then 0.
      else (log_max -. log_transform ~base safe_y) /. (log_max -. log_min)

let forward_y2_transform t y =
  match (t.y2_scale, t.y2_view) with
  | Some (Numeric _ | Band _), Some view ->
      if Float.abs (view.max -. view.min) < 1e-12 then 0.
      else (view.max -. y) /. (view.max -. view.min)
  | Some (Log { base; view; _ }), _ ->
      let safe_min = Float.max 1e-10 view.min in
      let safe_max = Float.max 1e-10 view.max in
      let safe_y = Float.max 1e-10 y in
      let log_min = log_transform ~base safe_min in
      let log_max = log_transform ~base safe_max in
      if Float.abs (log_max -. log_min) < 1e-12 then 0.
      else (log_max -. log_transform ~base safe_y) /. (log_max -. log_min)
  | _ -> forward_y_transform t y

let data_of_px t ~px ~py =
  if not (rect_contains t.plot ~x:px ~y:py) then None
  else
    let rel_x = px - t.plot.x in
    let rel_y = py - t.plot.y in
    let w = max 1 (t.plot.width - 1) in
    let h = max 1 (t.plot.height - 1) in
    let tx = float rel_x /. float w in
    let ty = float rel_y /. float h in
    let x = inverse_x_transform t tx in
    let y = inverse_y_transform t ty in
    Some (x, y)

let px_of_data_unclamped ?(y_axis : Mark.y_axis_selector = `Y1) t ~x ~y =
  let forward_y =
    match y_axis with
    | `Y1 -> forward_y_transform t y
    | `Y2 -> forward_y2_transform t y
  in
  if t.plot.width <= 1 && t.plot.height <= 1 then
    let tx = forward_x_transform t x in
    let ty = forward_y in
    if tx < 0. || tx > 1. || ty < 0. || ty > 1. then None
    else Some (t.plot.x, t.plot.y)
  else
    let w = max 0 (t.plot.width - 1) in
    let h = max 0 (t.plot.height - 1) in
    let tx = forward_x_transform t x in
    let ty = forward_y in
    if tx < 0. || tx > 1. || ty < 0. || ty > 1. then None
    else
      let px = t.plot.x + int_of_float (Float.round (tx *. float w)) in
      let py = t.plot.y + int_of_float (Float.round (ty *. float h)) in
      Some (px, py)

let px_of_data t ~x ~y =
  if t.plot.width <= 1 && t.plot.height <= 1 then (t.plot.x, t.plot.y)
  else
    let w = max 0 (t.plot.width - 1) in
    let h = max 0 (t.plot.height - 1) in
    let tx = clamp01 (forward_x_transform t x) in
    let ty = clamp01 (forward_y_transform t y) in
    let px = t.plot.x + int_of_float (Float.round (tx *. float w)) in
    let py = t.plot.y + int_of_float (Float.round (ty *. float h)) in
    (px, py)

(* {1 Band Helpers} *)

let band_params ~cats ~padding ~extent =
  let n = max 1 (List.length cats) in
  let padding = Float.min 0.95 (clamp01 padding) in
  let pad = padding *. float extent in
  let band_extent = float extent -. pad in
  let bw = Float.max 1e-6 (band_extent /. float n) in
  (pad /. 2., bw)

let band_start_px ~origin ~offset ~bw ~index =
  origin + int_of_float (Float.round (offset +. (float index *. bw)))

let band_size_px ~bw = max 1 (int_of_float (Float.max 1. (bw -. 1.)))
let band_index_fast index_of s = Hashtbl.find_opt index_of s

let band_category_fast categories_arr i =
  if i >= 0 && i < Array.length categories_arr then Some categories_arr.(i)
  else None

(* {1 Category Lookup} *)

let x_category_of_px t ~px =
  match t.x_scale with
  | Band { categories_arr; padding; categories; _ } ->
      if t.plot.width <= 0 then None
      else if px < t.plot.x || px >= t.plot.x + t.plot.width then None
      else
        let offset, bw =
          band_params ~cats:categories ~padding ~extent:t.plot.width
        in
        let rel = float (px - t.plot.x) in
        let i = int_of_float ((rel -. offset) /. bw) in
        let i = clamp_int 0 (Array.length categories_arr - 1) i in
        band_category_fast categories_arr i
  | _ -> None

let y_category_of_px t ~py =
  match t.y_scale with
  | Band { categories_arr; padding; categories; _ } ->
      if t.plot.height <= 0 then None
      else if py < t.plot.y || py >= t.plot.y + t.plot.height then None
      else
        let offset, bw =
          band_params ~cats:categories ~padding ~extent:t.plot.height
        in
        let rel = float (py - t.plot.y) in
        let i = int_of_float ((rel -. offset) /. bw) in
        let i = clamp_int 0 (Array.length categories_arr - 1) i in
        band_category_fast categories_arr i
  | _ -> None

let px_of_x_category t cat =
  match t.x_scale with
  | Band { index_of; padding; categories; _ } -> (
      match band_index_fast index_of cat with
      | None -> None
      | Some i ->
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.width
          in
          let band_w = band_size_px ~bw in
          let x0 = band_start_px ~origin:t.plot.x ~offset ~bw ~index:i in
          Some (x0 + (band_w / 2)))
  | _ -> None

let py_of_y_category t cat =
  match t.y_scale with
  | Band { index_of; padding; categories; _ } -> (
      match band_index_fast index_of cat with
      | None -> None
      | Some i ->
          let offset, bw =
            band_params ~cats:categories ~padding ~extent:t.plot.height
          in
          let band_h = band_size_px ~bw in
          let y0 = band_start_px ~origin:t.plot.y ~offset ~bw ~index:i in
          Some (y0 + (band_h / 2)))
  | _ -> None

(* {1 View Operations} *)

let clamp_view t (v : View.t) =
  let clamp_axis scale_opt domain_opt win_opt =
    match (scale_opt, domain_opt, win_opt) with
    | Some (Numeric { clamp = true; _ }), Some domain, Some w
    | Some (Log { clamp = true; _ }), Some domain, Some w ->
        Some (View.clamp ~domain w)
    | Some (Numeric { clamp = true; _ }), None, Some w
    | Some (Log { clamp = true; _ }), None, Some w ->
        Some w
    | Some (Numeric { clamp = false; _ }), _, Some w
    | Some (Log { clamp = false; _ }), _, Some w ->
        Some w
    | Some (Numeric _), _, None | Some (Log _), _, None -> None
    | Some (Band _), _, _ | None, _, _ -> None
  in
  {
    View.x = clamp_axis (Some t.x_scale) (Some t.x_domain) v.View.x;
    y = clamp_axis (Some t.y_scale) (Some t.y_domain) v.View.y;
    y2 = clamp_axis t.y2_scale t.y2_domain v.View.y2;
  }

type axis = [ `X | `Y | `Both ]

let zoom_view_around_px t ~view ~axis ~px ~py ~factor =
  let base = clamp_view t view in
  let center_x, center_y =
    match data_of_px t ~px ~py with
    | Some (x, y) -> (x, y)
    | None ->
        ( (t.x_view.min +. t.x_view.max) /. 2.0,
          (t.y_view.min +. t.y_view.max) /. 2.0 )
  in
  let zoom_x w = View.zoom_around w ~center:center_x ~factor in
  let zoom_y w = View.zoom_around w ~center:center_y ~factor in
  let apply_x, clamp_x =
    match t.x_scale with
    | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
    | Band _ -> (false, false)
  in
  let apply_y, clamp_y =
    match t.y_scale with
    | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
    | Band _ -> (false, false)
  in
  let apply_y2, clamp_y2, y2_domain =
    match t.y2_scale with
    | Some (Numeric { clamp; _ }) | Some (Log { clamp; _ }) ->
        (true, clamp, t.y2_domain)
    | Some (Band _) | None -> (false, false, None)
  in
  let x =
    if apply_x && (axis = `X || axis = `Both) then
      let w = Option.value base.View.x ~default:t.x_view in
      let zoomed = zoom_x w in
      Some (if clamp_x then View.clamp ~domain:t.x_domain zoomed else zoomed)
    else base.View.x
  in
  let y =
    if apply_y && (axis = `Y || axis = `Both) then
      let w = Option.value base.View.y ~default:t.y_view in
      let zoomed = zoom_y w in
      Some (if clamp_y then View.clamp ~domain:t.y_domain zoomed else zoomed)
    else base.View.y
  in
  let y2 =
    match (apply_y2, axis, base.View.y2, t.y2_view) with
    | true, (`Y | `Both), Some w, _ ->
        let center_y2 =
          match t.y2_view with
          | Some v -> (v.min +. v.max) /. 2.0
          | None -> center_y
        in
        let zoomed = View.zoom_around w ~center:center_y2 ~factor in
        let clamped =
          match (clamp_y2, y2_domain) with
          | true, Some domain -> View.clamp ~domain zoomed
          | _ -> zoomed
        in
        Some clamped
    | true, (`Y | `Both), None, Some default_view ->
        let center_y2 = (default_view.min +. default_view.max) /. 2.0 in
        let zoomed = View.zoom_around default_view ~center:center_y2 ~factor in
        let clamped =
          match (clamp_y2, y2_domain) with
          | true, Some domain -> View.clamp ~domain zoomed
          | _ -> zoomed
        in
        Some clamped
    | _ -> base.View.y2
  in
  { View.x; y; y2 }

let pan_view_by_px t ~view ~dx ~dy =
  let base = clamp_view t view in
  let apply_x, clamp_x =
    match t.x_scale with
    | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
    | Band _ -> (false, false)
  in
  let apply_y, clamp_y =
    match t.y_scale with
    | Numeric { clamp; _ } | Log { clamp; _ } -> (true, clamp)
    | Band _ -> (false, false)
  in
  let apply_y2, clamp_y2, y2_domain =
    match t.y2_scale with
    | Some (Numeric { clamp; _ }) | Some (Log { clamp; _ }) ->
        (true, clamp, t.y2_domain)
    | Some (Band _) | None -> (false, false, None)
  in
  let pan_x =
    if not apply_x then None
    else
      let w = Option.value base.View.x ~default:t.x_view in
      let denom = float (max 1 (t.plot.width - 1)) in
      let delta = float dx *. (w.max -. w.min) /. denom in
      let panned = View.pan w ~delta in
      Some (if clamp_x then View.clamp ~domain:t.x_domain panned else panned)
  in
  let pan_y =
    if not apply_y then None
    else
      let w = Option.value base.View.y ~default:t.y_view in
      let denom = float (max 1 (t.plot.height - 1)) in
      let delta = float dy *. (w.max -. w.min) /. denom in
      let panned = View.pan w ~delta in
      Some (if clamp_y then View.clamp ~domain:t.y_domain panned else panned)
  in
  let pan_y2 =
    if not apply_y2 then base.View.y2
    else
      match (base.View.y2, t.y2_view) with
      | Some w, _ ->
          let denom = float (max 1 (t.plot.height - 1)) in
          let delta = float dy *. (w.max -. w.min) /. denom in
          let panned = View.pan w ~delta in
          let clamped =
            match (clamp_y2, y2_domain) with
            | true, Some domain -> View.clamp ~domain panned
            | _ -> panned
          in
          Some clamped
      | None, Some default_view ->
          let denom = float (max 1 (t.plot.height - 1)) in
          let delta =
            float dy *. (default_view.max -. default_view.min) /. denom
          in
          let panned = View.pan default_view ~delta in
          let clamped =
            match (clamp_y2, y2_domain) with
            | true, Some domain -> View.clamp ~domain panned
            | _ -> panned
          in
          Some clamped
      | None, None -> None
  in
  { View.x = pan_x; y = pan_y; y2 = pan_y2 }

let plot_center_px t =
  let px = t.plot.x + (t.plot.width / 2) in
  let py = t.plot.y + (t.plot.height / 2) in
  (px, py)

let zoom_view_around_center t ~view ~axis ~factor =
  let px, py = plot_center_px t in
  zoom_view_around_px t ~view ~axis ~px ~py ~factor

(* {1 Hit Testing} *)

let hit_distance ~policy ~px ~py ~x ~y =
  let dx = float (px - x) in
  let dy = float (py - y) in
  match policy with
  | `Nearest_px -> sqrt ((dx *. dx) +. (dy *. dy))
  | `Nearest_x -> Float.abs dx
  | `Nearest_y -> Float.abs dy

let within_radius ~radius d = d <= float radius

let hit_test ?(radius = 3) ?(policy = (`Nearest_px : Hit.policy)) t ~px ~py =
  if not (rect_contains t.plot ~x:px ~y:py) then None
  else
    let best : Hit.t option ref = ref None in
    let r = t.plot in
    let consider (cand : Hit.t) =
      match !best with
      | None -> best := Some cand
      | Some b when cand.distance_px <= b.distance_px -> best := Some cand
      | _ -> ()
    in
    let map_xy_unclamped ~y_axis x y = px_of_data_unclamped ~y_axis t ~x ~y in

    let check_points ~y_axis ~mark_id ~kind ~payload_of ~data_iter =
      data_iter (fun idx (x, y) ->
          match map_xy_unclamped ~y_axis x y with
          | None -> ()
          | Some (xpx, ypx) ->
              let d = hit_distance ~policy ~px ~py ~x:xpx ~y:ypx in
              if within_radius ~radius d then
                consider
                  {
                    Hit.mark_id;
                    kind;
                    index = idx;
                    px = xpx;
                    py = ypx;
                    distance_px = d;
                    payload = payload_of x y;
                  })
    in

    let check_rects ~mark_id ~kind ~payload_of ~data_iter =
      data_iter (fun idx (x0, y0, x1, y1, payload) ->
          let x_min = min x0 x1 and x_max = max x0 x1 in
          let y_min = min y0 y1 and y_max = max y0 y1 in
          let inside =
            px >= x_min && px <= x_max && py >= y_min && py <= y_max
          in
          if inside then
            consider
              {
                Hit.mark_id;
                kind;
                index = idx;
                px;
                py;
                distance_px = 0.;
                payload = payload_of payload;
              }
          else
            let cx = clamp_int x_min x_max px in
            let cy = clamp_int y_min y_max py in
            let d = hit_distance ~policy ~px ~py ~x:cx ~y:cy in
            if within_radius ~radius d then
              consider
                {
                  Hit.mark_id;
                  kind;
                  index = idx;
                  px = cx;
                  py = cy;
                  distance_px = d;
                  payload = payload_of payload;
                })
    in

    List.iter
      (fun (m : Mark.t) ->
        match m.kind with
        | Line { x = xa; y = ya; _ } ->
            check_points ~y_axis:m.y_axis ~mark_id:m.id ~kind:`Line
              ~payload_of:(fun x y -> Hit.XY { x; y })
              ~data_iter:(fun f ->
                for i = 0 to Array.length xa - 1 do
                  let yi = ya.(i) in
                  if Float.is_finite yi then f i (xa.(i), yi)
                done)
        | Scatter { x = xa; y = ya; _ } ->
            check_points ~y_axis:m.y_axis ~mark_id:m.id ~kind:`Scatter
              ~payload_of:(fun x y -> Hit.XY { x; y })
              ~data_iter:(fun f ->
                for i = 0 to Array.length xa - 1 do
                  f i (xa.(i), ya.(i))
                done)
        | Bar { categories = bar_cats; values; direction = `Vertical; _ } -> (
            match t.x_scale with
            | Band { categories = band_cats; index_of; padding; _ } ->
                let offset, bw =
                  band_params ~cats:band_cats ~padding ~extent:r.width
                in
                let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                check_rects ~mark_id:m.id ~kind:`Bars
                  ~payload_of:(fun (cat, value) ->
                    Hit.Bar { category = cat; value })
                  ~data_iter:(fun f ->
                    Array.iteri
                      (fun i cat ->
                        match band_index_fast index_of cat with
                        | None -> ()
                        | Some bi ->
                            let x0 =
                              r.x + int_of_float (offset +. (float bi *. bw))
                            in
                            let x1 = x0 + band_w - 1 in
                            let v = values.(i) in
                            let y0px =
                              snd (px_of_data t ~x:t.x_view.min ~y:v)
                            in
                            let ybase =
                              snd (px_of_data t ~x:t.x_view.min ~y:0.)
                            in
                            let y0 = min y0px ybase in
                            let y1 = max y0px ybase in
                            f i (x0, y0, x1, y1, (cat, v)))
                      bar_cats)
            | _ -> ())
        | Bar { categories = bar_cats; values; direction = `Horizontal; _ } -> (
            match t.y_scale with
            | Band { categories = band_cats; index_of; padding; _ } ->
                let offset, bw =
                  band_params ~cats:band_cats ~padding ~extent:r.height
                in
                let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                check_rects ~mark_id:m.id ~kind:`Bars
                  ~payload_of:(fun (cat, value) ->
                    Hit.Bar { category = cat; value })
                  ~data_iter:(fun f ->
                    Array.iteri
                      (fun i cat ->
                        match band_index_fast index_of cat with
                        | None -> ()
                        | Some bi ->
                            let y0 =
                              r.y + int_of_float (offset +. (float bi *. bw))
                            in
                            let y1 = y0 + band_h - 1 in
                            let v = values.(i) in
                            let x0px, _ = px_of_data t ~x:0. ~y:t.y_view.min in
                            let x1px, _ = px_of_data t ~x:v ~y:t.y_view.min in
                            let x0 = min x0px x1px in
                            let x1 = max x0px x1px in
                            f i (x0, y0, x1, y1, (cat, v)))
                      bar_cats)
            | _ -> ())
        | Stacked_bar { data = sdata; direction = `Vertical; _ } -> (
            match t.x_scale with
            | Band { categories = band_cats; index_of; padding; _ } ->
                let offset, bw =
                  band_params ~cats:band_cats ~padding ~extent:r.width
                in
                let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                check_rects ~mark_id:m.id ~kind:`Stacked_bars
                  ~payload_of:(fun (cat, seg_i, v, total) ->
                    Hit.Stacked_bar
                      {
                        category = cat;
                        segment_index = seg_i;
                        value = v;
                        total;
                      })
                  ~data_iter:(fun f ->
                    Array.iteri
                      (fun i bar ->
                        match band_index_fast index_of bar.Mark.category with
                        | None -> ()
                        | Some bi ->
                            let x0 =
                              r.x + int_of_float (offset +. (float bi *. bw))
                            in
                            let x1 = x0 + band_w - 1 in
                            let total =
                              List.fold_left
                                (fun acc s -> acc +. max 0. s.Mark.value)
                                0. bar.segments
                            in
                            let cum = ref 0. in
                            List.iteri
                              (fun si seg ->
                                let v = max 0. seg.Mark.value in
                                let y0v = !cum in
                                cum := !cum +. v;
                                let y1v = !cum in
                                let y0px =
                                  snd (px_of_data t ~x:t.x_view.min ~y:y0v)
                                in
                                let y1px =
                                  snd (px_of_data t ~x:t.x_view.min ~y:y1v)
                                in
                                let y0 = min y0px y1px in
                                let y1 = max y0px y1px in
                                f i
                                  (x0, y0, x1, y1, (bar.category, si, v, total)))
                              bar.segments)
                      sdata)
            | _ -> ())
        | Stacked_bar { data = sdata; direction = `Horizontal; _ } -> (
            match t.y_scale with
            | Band { categories = band_cats; index_of; padding; _ } ->
                let offset, bw =
                  band_params ~cats:band_cats ~padding ~extent:r.height
                in
                let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
                check_rects ~mark_id:m.id ~kind:`Stacked_bars
                  ~payload_of:(fun (cat, seg_i, v, total) ->
                    Hit.Stacked_bar
                      {
                        category = cat;
                        segment_index = seg_i;
                        value = v;
                        total;
                      })
                  ~data_iter:(fun f ->
                    Array.iteri
                      (fun i bar ->
                        match band_index_fast index_of bar.Mark.category with
                        | None -> ()
                        | Some bi ->
                            let y0 =
                              r.y + int_of_float (offset +. (float bi *. bw))
                            in
                            let y1 = y0 + band_h - 1 in
                            let total =
                              List.fold_left
                                (fun acc s -> acc +. max 0. s.Mark.value)
                                0. bar.segments
                            in
                            let cum = ref 0. in
                            List.iteri
                              (fun si seg ->
                                let v = max 0. seg.Mark.value in
                                let x0v = !cum in
                                cum := !cum +. v;
                                let x1v = !cum in
                                let x0px, _ =
                                  px_of_data t ~x:x0v ~y:t.y_view.min
                                in
                                let x1px, _ =
                                  px_of_data t ~x:x1v ~y:t.y_view.min
                                in
                                let x0 = min x0px x1px in
                                let x1 = max x0px x1px in
                                f i
                                  (x0, y0, x1, y1, (bar.category, si, v, total)))
                              bar.segments)
                      sdata)
            | _ -> ())
        | Heatmap { x = xa; y = ya; values = va; _ } ->
            for i = 0 to Array.length xa - 1 do
              let xv = xa.(i) and yv = ya.(i) and v = va.(i) in
              match map_xy_unclamped ~y_axis:`Y1 xv yv with
              | None -> ()
              | Some (xpx, ypx) ->
                  let d = hit_distance ~policy ~px ~py ~x:xpx ~y:ypx in
                  if within_radius ~radius d then
                    consider
                      {
                        Hit.mark_id = m.id;
                        kind = `Heatmap;
                        index = i;
                        px = xpx;
                        py = ypx;
                        distance_px = d;
                        payload = Hit.Heat { x = xv; y = yv; value = v };
                      }
            done
        | Candle { data = cdata; _ } ->
            for i = 0 to Array.length cdata - 1 do
              let o = cdata.(i) in
              match
                map_xy_unclamped ~y_axis:m.y_axis o.Mark.time t.y_view.min
              with
              | None -> ()
              | Some (cx, _) ->
                  let d = hit_distance ~policy ~px ~py ~x:cx ~y:py in
                  if within_radius ~radius d then
                    consider
                      {
                        Hit.mark_id = m.id;
                        kind = `Candles;
                        index = i;
                        px = cx;
                        py;
                        distance_px = d;
                        payload =
                          Hit.OHLC
                            {
                              time = o.time;
                              open_ = o.open_;
                              high = o.high;
                              low = o.low;
                              close = o.close;
                            };
                      }
            done
        | Circle { cx = cxa; cy = cya; _ } ->
            check_points ~y_axis:m.y_axis ~mark_id:m.id ~kind:`Circle
              ~payload_of:(fun x y -> Hit.XY { x; y })
              ~data_iter:(fun f ->
                for i = 0 to Array.length cxa - 1 do
                  f i (cxa.(i), cya.(i))
                done)
        | _ -> ())
      t.marks;
    !best

(* {1 Axis Kind Inference} *)

type axis_kind_poly =
  [ `Band of string list * float | `Numeric of bool | `Log of float * bool ]

let infer_axis_kind_x (scale : Scale.t) (marks : Mark.t list) =
  match scale with
  | Scale.Band { categories; padding } ->
      let cats =
        match categories with
        | Some c -> c
        | None -> Mark.collect_x_categories marks
      in
      `Band (cats, padding)
  | Scale.Numeric { clamp; domain = _ } -> `Numeric clamp
  | Scale.Log { base; clamp; domain = _ } -> `Log (base, clamp)
  | Scale.Auto ->
      let has_band =
        List.exists
          (fun (m : Mark.t) ->
            match m.kind with
            | Bar { direction = `Vertical; _ }
            | Stacked_bar { direction = `Vertical; _ } ->
                true
            | _ -> false)
          marks
      in
      if has_band then `Band (Mark.collect_x_categories marks, 0.1)
      else `Numeric true

let infer_axis_kind_y (scale : Scale.t) (marks : Mark.t list) =
  match scale with
  | Scale.Band { categories; padding } ->
      let cats =
        match categories with
        | Some c -> c
        | None -> Mark.collect_y_categories marks
      in
      `Band (cats, padding)
  | Scale.Numeric { clamp; domain = _ } -> `Numeric clamp
  | Scale.Log { base; clamp; domain = _ } -> `Log (base, clamp)
  | Scale.Auto ->
      let has_band =
        List.exists
          (fun (m : Mark.t) ->
            match m.kind with
            | Bar { direction = `Horizontal; _ }
            | Stacked_bar { direction = `Horizontal; _ } ->
                true
            | _ -> false)
          marks
      in
      if has_band then `Band (Mark.collect_y_categories marks, 0.1)
      else `Numeric true

(* {1 Domain Helpers} *)

let apply_domain_override (scale : Scale.t) (dom : View.window) : View.window =
  match scale with
  | Scale.Numeric { domain = `Domain (a, b); _ }
  | Scale.Log { domain = `Domain (a, b); _ } ->
      View.window ~min:a ~max:b
  | _ -> dom

let resolve_view ~clamp (dom : View.window) (view_opt : View.window option) :
    View.window =
  match view_opt with
  | None -> dom
  | Some v -> if clamp then View.clamp ~domain:dom v else v

let validate_log_domain (w : View.window) : View.window =
  let safe_min = 1e-10 in
  let min' = if w.min <= 0. then safe_min else w.min in
  let max' = if w.max <= 0. then safe_min *. 10. else w.max in
  let min', max' = if min' >= max' then (min', min' *. 10.) else (min', max') in
  { View.min = min'; max = max' }

(* {1 Axis Label Width} *)

let axis_label_width_from_ticks (axis : Axis.t) (tick_values : float list) : int
    =
  if not axis.show then 0
  else
    List.fold_left
      (fun (acc, i) v ->
        let s = axis.format i v in
        (max acc (text_width s), i + 1))
      (0, 0) tick_values
    |> fst

let axis_label_width_numeric (axis : Axis.t) (w : View.window) : int =
  if not axis.show then 0
  else
    let tick_values =
      nice_ticks ~min_val:w.min ~max_val:w.max ~target_ticks:axis.ticks
    in
    axis_label_width_from_ticks axis tick_values

let axis_label_width_log (axis : Axis.t) ~base (w : View.window) : int =
  if not axis.show then 0
  else
    let tick_values =
      log_ticks ~base ~min_val:w.min ~max_val:w.max ~target_ticks:axis.ticks
    in
    axis_label_width_from_ticks axis tick_values

let axis_label_width_band (axis : Axis.t) (cats : string list) : int =
  if not axis.show then 0
  else List.fold_left (fun acc s -> max acc (text_width s)) 0 cats

(* {1 Config} *)

type config = {
  theme : Theme.t;
  frame : frame;
  x_scale : Scale.t;
  y_scale : Scale.t;
  y2_scale : Scale.t option;
  x_axis : Axis.t;
  y_axis : Axis.t;
  y2_axis : Axis.t option;
  grid : Gridlines.t;
  marks : Mark.t list;
  title : title_info option;
}

(* {1 Layout Computation} *)

let compute ?(view = View.empty) ?(x = 0) ?(y = 0) (cfg : config) ~width ~height
    : t =
  let width = max 1 width and height = max 1 height in
  let mt, mr, mb, ml, ip =
    match cfg.frame with
    | Manual { margins = mt, mr, mb, ml; inner_padding } ->
        (max 0 mt, max 0 mr, max 0 mb, max 0 ml, max 0 inner_padding)
    | Auto -> (0, 0, 0, 0, 0)
  in

  let marks = cfg.marks in
  let x_kind = infer_axis_kind_x cfg.x_scale marks in
  let y_kind = infer_axis_kind_y cfg.y_scale marks in

  (* Resolve domains *)
  let x_domain0 =
    match x_kind with
    | `Band (cats, _) ->
        View.window ~min:0. ~max:(float (max 1 (List.length cats)))
    | `Numeric _ | `Log _ ->
        let dom = Mark.infer_x_domain marks in
        let merged =
          match Mark.infer_x_domain_additive marks with
          | None -> dom
          | Some add_dom -> Mark.merge_domain dom add_dom
        in
        View.window ~min:merged.Mark.min ~max:merged.max
  in
  let y1_dom, y2_dom = Mark.infer_y_domains marks in
  let y1_domain0 =
    match y_kind with
    | `Band (cats, _) ->
        View.window ~min:0. ~max:(float (max 1 (List.length cats)))
    | `Numeric _ | `Log _ -> View.window ~min:y1_dom.Mark.min ~max:y1_dom.max
  in
  let y2_domain0 = View.window ~min:y2_dom.Mark.min ~max:y2_dom.max in

  let x_domain = apply_domain_override cfg.x_scale x_domain0 in
  let y_domain = apply_domain_override cfg.y_scale y1_domain0 in

  let x_scale_res =
    match x_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        make_band ~categories:cats ~padding ~domain:dom ~view:dom
    | `Numeric clamp ->
        let v = resolve_view ~clamp x_domain view.View.x in
        Numeric { domain = x_domain; view = v; clamp }
    | `Log (base, clamp) ->
        let dom = validate_log_domain x_domain in
        let v = resolve_view ~clamp dom view.View.x in
        let v = validate_log_domain v in
        Log { base; domain = dom; view = v; clamp }
  in

  let y_scale_res =
    match y_kind with
    | `Band (cats, padding) ->
        let dom = View.window ~min:0. ~max:(float (max 1 (List.length cats))) in
        make_band ~categories:cats ~padding ~domain:dom ~view:dom
    | `Numeric clamp ->
        let v = resolve_view ~clamp y_domain view.View.y in
        Numeric { domain = y_domain; view = v; clamp }
    | `Log (base, clamp) ->
        let dom = validate_log_domain y_domain in
        let v = resolve_view ~clamp dom view.View.y in
        let v = validate_log_domain v in
        Log { base; domain = dom; view = v; clamp }
  in

  let y2_scale_res, y2_domain_res, y2_view_res =
    match cfg.y2_scale with
    | None -> (None, None, None)
    | Some scale ->
        let y2_kind = infer_axis_kind_y scale marks in
        let y2_dom =
          match y2_kind with
          | `Band _ -> y2_domain0
          | `Numeric _ | `Log _ -> apply_domain_override scale y2_domain0
        in
        let res =
          match y2_kind with
          | `Band (cats, padding) ->
              let dom =
                View.window ~min:0. ~max:(float (max 1 (List.length cats)))
              in
              make_band ~categories:cats ~padding ~domain:dom ~view:dom
          | `Numeric clamp ->
              let v = resolve_view ~clamp y2_dom view.View.y2 in
              Numeric { domain = y2_dom; view = v; clamp }
          | `Log (base, clamp) ->
              let y2_dom = validate_log_domain y2_dom in
              let v = resolve_view ~clamp y2_dom view.View.y2 in
              let v = validate_log_domain v in
              Log { base; domain = y2_dom; view = v; clamp }
        in
        let view_win =
          match res with
          | Numeric { view; _ } | Log { view; _ } | Band { view; _ } -> view
        in
        (Some res, Some y2_dom, Some view_win)
  in

  let x_view =
    match x_scale_res with
    | Numeric { view; _ } | Log { view; _ } | Band { view; _ } -> view
  in
  let y_view =
    match y_scale_res with
    | Numeric { view; _ } | Log { view; _ } | Band { view; _ } -> view
  in

  (* Axis reserved sizes *)
  let y_axis_width =
    if not cfg.y_axis.show then 0
    else
      let label_w =
        match y_scale_res with
        | Numeric { domain; _ } -> axis_label_width_numeric cfg.y_axis domain
        | Log { base; domain; _ } ->
            axis_label_width_log cfg.y_axis ~base domain
        | Band { categories; _ } -> axis_label_width_band cfg.y_axis categories
      in
      label_w + cfg.y_axis.label_padding + cfg.y_axis.tick_length + 1
  in
  let x_axis_height =
    if not cfg.x_axis.show then 0
    else
      let base = 1 + cfg.x_axis.tick_length + cfg.x_axis.label_padding + 1 in
      let title_space = match cfg.x_axis.title with None -> 0 | Some _ -> 1 in
      base + title_space
  in
  let y_axis_title_width =
    match cfg.y_axis.title with None -> 0 | Some _ -> 3
  in
  let y2_axis_width =
    match (cfg.y2_axis, y2_scale_res) with
    | Some y2_ax, Some y2_sc when y2_ax.Axis.show ->
        let label_w =
          match y2_sc with
          | Numeric { domain; _ } -> axis_label_width_numeric y2_ax domain
          | Log { base; domain; _ } -> axis_label_width_log y2_ax ~base domain
          | Band { categories; _ } -> axis_label_width_band y2_ax categories
        in
        label_w + y2_ax.label_padding + y2_ax.tick_length + 1
    | _ -> 0
  in

  let plot_x_rel = ml + y_axis_title_width + y_axis_width + ip in
  let plot_y_rel = mt + ip in
  let plot_w = max 1 (width - plot_x_rel - mr - ip - y2_axis_width) in
  let plot_h = max 1 (height - plot_y_rel - (mb + x_axis_height + ip)) in
  let plot_x = x + plot_x_rel in
  let plot_y = y + plot_y_rel in
  let plot : rect =
    { x = plot_x; y = plot_y; width = plot_w; height = plot_h }
  in

  (* Compile marks (resolve styles from palette) *)
  let compiled_marks = Mark.compile ~palette:cfg.theme.palette marks in

  let title = cfg.title in

  (* Precompute tick values *)
  let x_ticks =
    match x_scale_res with
    | Numeric _ ->
        nice_ticks ~min_val:x_view.min ~max_val:x_view.max
          ~target_ticks:cfg.x_axis.ticks
    | Log { base; view; _ } ->
        log_ticks ~base ~min_val:view.min ~max_val:view.max
          ~target_ticks:cfg.x_axis.ticks
    | Band _ -> []
  in
  let y_ticks =
    match y_scale_res with
    | Numeric _ ->
        nice_ticks ~min_val:y_view.min ~max_val:y_view.max
          ~target_ticks:cfg.y_axis.ticks
    | Log { base; view; _ } ->
        log_ticks ~base ~min_val:view.min ~max_val:view.max
          ~target_ticks:cfg.y_axis.ticks
    | Band _ -> []
  in
  let y2_ticks =
    match (y2_scale_res, cfg.y2_axis) with
    | Some (Numeric _), Some y2_ax ->
        let y2v = Option.get y2_view_res in
        Some
          (nice_ticks ~min_val:y2v.min ~max_val:y2v.max
             ~target_ticks:y2_ax.ticks)
    | Some (Log { base; view; _ }), Some y2_ax ->
        Some
          (log_ticks ~base ~min_val:view.min ~max_val:view.max
             ~target_ticks:y2_ax.ticks)
    | _ -> None
  in

  {
    width;
    height;
    plot;
    theme = cfg.theme;
    x_scale = x_scale_res;
    y_scale = y_scale_res;
    y2_scale = y2_scale_res;
    x_domain;
    y_domain;
    y2_domain = y2_domain_res;
    x_view;
    y_view;
    y2_view = y2_view_res;
    x_ticks;
    y_ticks;
    y2_ticks;
    x_axis = cfg.x_axis;
    y_axis = cfg.y_axis;
    y2_axis = cfg.y2_axis;
    grid = cfg.grid;
    frame_inner_padding = ip;
    margin_left = ml;
    y_axis_width;
    y_axis_title_width;
    y2_axis_width;
    marks = compiled_marks;
    title;
  }
