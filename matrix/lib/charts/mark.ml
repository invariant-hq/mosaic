module Style = Ansi.Style
module Color = Ansi.Color

(* {1 Supporting Types} *)

type direction = [ `Vertical | `Horizontal ]
type scatter_mode = [ `Cell | `Braille | `Density ]
type heatmap_agg = [ `Last | `Avg | `Max ]

type heatmap_mode =
  | Cells_fg
  | Cells_bg
  | Halfblock_fg_bg
  | Shaded
  | Dense_bilinear

type bar_mode = [ `Cell | `Half_block ]
type candle_body = [ `Filled | `Hollow ]
type candle_width = [ `One | `Two ]
type area_baseline = [ `Zero | `Value of float ]

(* *)

type bin_method = Bins of int | Width of float | Edges of float array

(* *)

type histogram_normalize = [ `Count | `Density | `Probability ]
type bar_segment = { value : float; style : Style.t; label : string option }
type stacked_bar = { category : string; segments : bar_segment list }

type ohlc = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

type y_axis_selector = [ `Y1 | `Y2 ]

(* {1 Mark Kind} *)

type kind =
  | Line of {
      x : float array;
      y : float array;
      resolution : [ `Cell | `Wave | `Block2x2 | `Braille2x4 ];
      pattern : [ `Solid | `Dashed | `Dotted ];
      glyph : string option;
    }
  | Scatter of {
      x : float array;
      y : float array;
      mode : scatter_mode;
      glyph : string option;
    }
  | Bar of {
      categories : string array;
      values : float array;
      direction : direction;
      mode : bar_mode;
    }
  | Stacked_bar of {
      data : stacked_bar array;
      direction : direction;
      mode : bar_mode;
      gap : int;
      size : int option;
    }
  | Area of {
      x : float array;
      y : float array;
      baseline : area_baseline;
      resolution : [ `Cell | `Wave | `Block2x2 | `Braille2x4 ];
    }
  | Fill_between of {
      x : float array;
      y_low : float array;
      y_high : float array;
      resolution : [ `Cell | `Wave | `Block2x2 | `Braille2x4 ];
    }
  | Heatmap of {
      x : float array;
      y : float array;
      values : float array;
      color_scale : Color.t array;
      value_range : (float * float) option;
      auto_value_range : bool;
      agg : heatmap_agg;
      mode : heatmap_mode;
    }
  | Candle of {
      data : ohlc array;
      bullish : Style.t;
      bearish : Style.t;
      width : candle_width;
      body : candle_body;
    }
  | Circle of {
      cx : float array;
      cy : float array;
      r : float array;
      resolution : [ `Cell | `Wave | `Block2x2 | `Braille2x4 ];
    }
  | Rule of {
      value : float;
      direction : direction;
      pattern : [ `Solid | `Dashed | `Dotted ];
    }
  | Shade of { x0 : float; x1 : float }
  | Column_bg of { x : float }
  | Histogram of { bin_edges : float array; bin_values : float array }

(* {1 Mark} *)

type id = string

type t = {
  id : id option;
  label : string option;
  style : Style.t option;
  y_axis : y_axis_selector;
  kind : kind;
}

(* {1 Histogram Binning} *)

let compute_histogram_bins ~(bins : bin_method)
    ~(normalize : histogram_normalize) (xs : float array) :
    float array * float array =
  let n = Array.length xs in
  if n = 0 then ([||], [||])
  else
    let values = Array.copy xs in
    Array.sort Float.compare values;
    let vmin = values.(0) in
    let vmax = values.(n - 1) in
    let range = if vmax -. vmin <= 0. then 1. else vmax -. vmin in
    let edges =
      match bins with
      | Bins num_bins ->
          let num_bins = max 1 num_bins in
          let width = range /. float num_bins in
          Array.init (num_bins + 1) (fun i -> vmin +. (float i *. width))
      | Width w ->
          let w = if w <= 0. then range /. 10. else w in
          let num_bins = max 1 (int_of_float (Float.ceil (range /. w))) in
          Array.init (num_bins + 1) (fun i -> vmin +. (float i *. w))
      | Edges e ->
          if Array.length e >= 2 then e
          else
            let num_bins = 10 in
            let width = range /. float num_bins in
            Array.init (num_bins + 1) (fun i -> vmin +. (float i *. width))
    in
    let num_bins = Array.length edges - 1 in
    let counts = Array.make num_bins 0 in
    Array.iter
      (fun v ->
        let rec find lo hi =
          if lo >= hi then lo
          else
            let mid = (lo + hi) / 2 in
            if mid + 1 < Array.length edges && v >= edges.(mid + 1) then
              find (mid + 1) hi
            else find lo mid
        in
        let bin = min (num_bins - 1) (find 0 (num_bins - 1)) in
        counts.(bin) <- counts.(bin) + 1)
      values;
    let bin_values =
      match normalize with
      | `Count -> Array.map float counts
      | `Probability ->
          let total = float n in
          Array.map (fun c -> float c /. total) counts
      | `Density ->
          let total = float n in
          Array.mapi
            (fun i c ->
              let bin_width =
                if i + 1 < Array.length edges then edges.(i + 1) -. edges.(i)
                else 1.
              in
              float c /. (total *. bin_width))
            counts
    in
    (edges, bin_values)

(* {1 Constructors} *)

let extract f data = Array.map f data

let line ?id ?label ?style ?(resolution = `Cell) ?(pattern = `Solid) ?glyph
    ?(y_axis = `Y1) ~x ~y data =
  let kind =
    Line { x = extract x data; y = extract y data; resolution; pattern; glyph }
  in
  { id; label; style; y_axis; kind }

let line_gaps ?id ?label ?style ?(resolution = `Cell) ?(pattern = `Solid) ?glyph
    ?(y_axis = `Y1) ~x ~y data =
  let ya =
    extract (fun d -> match y d with None -> Float.nan | Some v -> v) data
  in
  let kind = Line { x = extract x data; y = ya; resolution; pattern; glyph } in
  { id; label; style; y_axis; kind }

let scatter ?id ?label ?style ?glyph ?(mode = (`Cell : scatter_mode))
    ?(y_axis = `Y1) ~x ~y data =
  {
    id;
    label;
    style;
    y_axis;
    kind = Scatter { x = extract x data; y = extract y data; mode; glyph };
  }

let bar ?id ?label ?style ?(direction = `Vertical)
    ?(mode = (`Half_block : bar_mode)) ~category ~value data =
  let kind =
    Bar
      {
        categories = extract category data;
        values = extract value data;
        direction;
        mode;
      }
  in
  { id; label; style; y_axis = `Y1; kind }

let stacked_bar ?id ?(direction = `Vertical) ?(gap = 1) ?size
    ?(mode = (`Half_block : bar_mode)) data =
  let gap = max 0 gap in
  {
    id;
    label = None;
    style = None;
    y_axis = `Y1;
    kind = Stacked_bar { data; direction; mode; gap; size };
  }

let rule ?id ?style ?(direction = `Horizontal) ?(pattern = `Solid)
    ?(y_axis = `Y1) value =
  { id; label = None; style; y_axis; kind = Rule { value; direction; pattern } }

let heatmap ?id ?(color_scale = [||]) ?value_range ?(auto_value_range = true)
    ?(agg = (`Last : heatmap_agg)) ?(mode = Cells_fg) ~x ~y ~value data =
  let kind =
    Heatmap
      {
        x = extract x data;
        y = extract y data;
        values = extract value data;
        color_scale;
        value_range;
        auto_value_range;
        agg;
        mode;
      }
  in
  { id; label = None; style = None; y_axis = `Y1; kind }

let candles ?id ?bullish ?bearish ?(width = (`One : candle_width))
    ?(body = (`Filled : candle_body)) ?(y_axis = `Y1) data =
  let bullish =
    Option.value bullish ~default:(Style.fg Color.green Style.default)
  in
  let bearish =
    Option.value bearish ~default:(Style.fg Color.red Style.default)
  in
  let sorted =
    let arr = Array.copy data in
    Array.sort (fun a b -> compare a.time b.time) arr;
    arr
  in
  {
    id;
    label = None;
    style = None;
    y_axis;
    kind = Candle { data = sorted; bullish; bearish; width; body };
  }

let circle ?id ?style
    ?(resolution = (`Cell : [ `Cell | `Wave | `Block2x2 | `Braille2x4 ]))
    ?(y_axis = `Y1) ~cx ~cy ~r data =
  let kind =
    Circle
      {
        cx = extract cx data;
        cy = extract cy data;
        r = extract r data;
        resolution;
      }
  in
  { id; label = None; style; y_axis; kind }

let shade ?id ?style ~min ~max () =
  let x0, x1 = if min <= max then (min, max) else (max, min) in
  { id; label = None; style; y_axis = `Y1; kind = Shade { x0; x1 } }

let column_bg ?id ?style x =
  { id; label = None; style; y_axis = `Y1; kind = Column_bg { x } }

let area ?id ?label ?style ?(baseline = `Zero) ?(resolution = `Cell)
    ?(y_axis = `Y1) ~x ~y data =
  let kind =
    Area { x = extract x data; y = extract y data; baseline; resolution }
  in
  { id; label; style; y_axis; kind }

let fill_between ?id ?label ?style ?(resolution = `Cell) ?(y_axis = `Y1) ~x
    ~y_low ~y_high data =
  let kind =
    Fill_between
      {
        x = extract x data;
        y_low = extract y_low data;
        y_high = extract y_high data;
        resolution;
      }
  in
  { id; label; style; y_axis; kind }

let histogram ?id ?label ?style ?(bins = Bins 10) ?(normalize = `Count) ~x data
    =
  let bin_edges, bin_values =
    compute_histogram_bins ~bins ~normalize (extract x data)
  in
  { id; label; style; y_axis = `Y1; kind = Histogram { bin_edges; bin_values } }

(* {1 Domain Inference} *)

type domain = { min : float; max : float }

type dom_acc = {
  mutable lo : float;
  mutable hi : float;
  mutable has_data : bool;
}

let dom_acc () = { lo = infinity; hi = neg_infinity; has_data = false }

let dom_add acc v =
  if Float.is_finite v then begin
    if v < acc.lo then acc.lo <- v;
    if v > acc.hi then acc.hi <- v;
    acc.has_data <- true
  end

let dom_add_array acc arr = Array.iter (fun v -> dom_add acc v) arr

let dom_finish acc =
  if not acc.has_data then { min = 0.; max = 1. }
  else
    let lo = acc.lo and hi = acc.hi in
    if Float.abs (hi -. lo) < 1e-12 then
      if Float.abs lo < 1e-12 then { min = -1.; max = 1. }
      else
        { min = lo -. (Float.abs lo *. 0.1); max = hi +. (Float.abs hi *. 0.1) }
    else { min = lo; max = hi }

let merge_domain d1 d2 =
  { min = Float.min d1.min d2.min; max = Float.max d1.max d2.max }

let infer_x_domain (marks : t list) =
  let acc = dom_acc () in
  List.iter
    (fun m ->
      match m.kind with
      | Line { x; _ } | Scatter { x; _ } | Area { x; _ } -> dom_add_array acc x
      | Fill_between { x; _ } -> dom_add_array acc x
      | Bar _ -> ()
      | Stacked_bar _ -> ()
      | Heatmap { x; _ } -> dom_add_array acc x
      | Candle { data; _ } -> Array.iter (fun o -> dom_add acc o.time) data
      | Circle { cx; r; _ } ->
          Array.iteri
            (fun i c ->
              dom_add acc (c -. r.(i));
              dom_add acc (c +. r.(i)))
            cx
      | Rule { direction = `Vertical; value; _ } -> dom_add acc value
      | Rule { direction = `Horizontal; _ } -> ()
      | Shade { x0; x1; _ } ->
          dom_add acc x0;
          dom_add acc x1
      | Column_bg { x; _ } -> dom_add acc x
      | Histogram { bin_edges; _ } ->
          if Array.length bin_edges > 0 then begin
            dom_add acc bin_edges.(0);
            dom_add acc bin_edges.(Array.length bin_edges - 1)
          end)
    marks;
  dom_finish acc

let infer_y_domains (marks : t list) =
  let y1acc = dom_acc () in
  let y2acc = dom_acc () in
  let add_y y_axis v =
    match y_axis with `Y1 -> dom_add y1acc v | `Y2 -> dom_add y2acc v
  in
  List.iter
    (fun m ->
      let y_axis = m.y_axis in
      match m.kind with
      | Line { y; _ } ->
          Array.iter (fun v -> if Float.is_finite v then add_y y_axis v) y
      | Scatter { y; _ } -> Array.iter (add_y y_axis) y
      | Bar { values; direction; _ } -> (
          match direction with
          | `Vertical ->
              dom_add y1acc 0.;
              Array.iter (dom_add y1acc) values
          | `Horizontal -> ())
      | Stacked_bar { data; direction; _ } -> (
          match direction with
          | `Vertical ->
              dom_add y1acc 0.;
              Array.iter
                (fun b ->
                  let total =
                    List.fold_left
                      (fun acc s -> acc +. Float.max 0. s.value)
                      0. b.segments
                  in
                  dom_add y1acc total)
                data
          | `Horizontal -> ())
      | Area { y; baseline; _ } ->
          (match baseline with
          | `Zero -> add_y y_axis 0.
          | `Value v -> add_y y_axis v);
          Array.iter (add_y y_axis) y
      | Fill_between { y_low; y_high; _ } ->
          Array.iter (add_y y_axis) y_low;
          Array.iter (add_y y_axis) y_high
      | Heatmap { y; _ } -> Array.iter (dom_add y1acc) y
      | Candle { data; _ } ->
          Array.iter
            (fun o ->
              add_y y_axis o.open_;
              add_y y_axis o.close;
              add_y y_axis o.high;
              add_y y_axis o.low)
            data
      | Circle { cy; r; _ } ->
          Array.iteri
            (fun i c ->
              add_y y_axis (c -. Float.max 0. r.(i));
              add_y y_axis (c +. Float.max 0. r.(i)))
            cy
      | Rule { direction = `Horizontal; value; _ } -> add_y y_axis value
      | Rule { direction = `Vertical; _ } -> ()
      | Shade _ | Column_bg _ -> ()
      | Histogram { bin_values; _ } ->
          dom_add y1acc 0.;
          if Array.length bin_values > 0 then
            dom_add y1acc (Array.fold_left Float.max bin_values.(0) bin_values))
    marks;
  (dom_finish y1acc, dom_finish y2acc)

let infer_x_domain_additive (marks : t list) =
  let acc = dom_acc () in
  List.iter
    (fun m ->
      match m.kind with
      | Bar { values; direction = `Horizontal; _ } ->
          dom_add acc 0.;
          dom_add_array acc values
      | Stacked_bar { data; direction = `Horizontal; _ } ->
          dom_add acc 0.;
          Array.iter
            (fun b ->
              let total =
                List.fold_left
                  (fun a s -> a +. Float.max 0. s.value)
                  0. b.segments
              in
              dom_add acc total)
            data
      | _ -> ())
    marks;
  if acc.has_data then Some (dom_finish acc) else None

(* {1 Category Extraction} *)

let collect_categories ~(dir : direction) (marks : t list) =
  let cats = ref [] in
  let seen = Hashtbl.create 16 in
  let add c =
    if not (Hashtbl.mem seen c) then begin
      Hashtbl.add seen c ();
      cats := c :: !cats
    end
  in
  List.iter
    (fun m ->
      match m.kind with
      | Bar { categories; direction; _ } when direction = dir ->
          Array.iter add categories
      | Stacked_bar { data; direction; _ } when direction = dir ->
          Array.iter (fun b -> add b.category) data
      | _ -> ())
    marks;
  List.rev !cats

let collect_x_categories marks = collect_categories ~dir:`Vertical marks
let collect_y_categories marks = collect_categories ~dir:`Horizontal marks

(* {1 Style Resolution} *)

let resolve_style (palette : Color.t array) (idx : int ref)
    (style_opt : Style.t option) =
  match style_opt with
  | Some s -> s
  | None ->
      let i = !idx in
      incr idx;
      let len = Array.length palette in
      let color = if len = 0 then Color.default else palette.(i mod len) in
      Style.fg color Style.default

let compile ~palette (marks : t list) : t list =
  let idx = ref 0 in
  let default_shade_style () = Style.make ~bg:(Color.grayscale ~level:2) () in
  List.map
    (fun m ->
      let style =
        match m.kind with
        | Shade _ | Column_bg _ ->
            Some (Option.value m.style ~default:(default_shade_style ()))
        | Heatmap _ | Stacked_bar _ | Candle _ -> m.style
        | _ -> Some (resolve_style palette idx m.style)
      in
      { m with style })
    marks
