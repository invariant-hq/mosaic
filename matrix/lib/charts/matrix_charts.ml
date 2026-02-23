module G = Grid
module Style = Ansi.Style
module Color = Ansi.Color

(* {1 Re-exports} *)

module Charset = Layout.Charset
module Raster = Layout.Raster
module Theme = Layout.Theme
module Scale = Layout.Scale
module Axis = Layout.Axis
module Gridlines = Layout.Gridlines
module View = Layout.View
module Hit = Layout.Hit
module Mark = Mark
module Layout = Layout
module Sparkline = Sparkline

(* {1 Label Formatting} *)

module Label_format = struct
  let float ?(precision = 3) () : int -> float -> string =
   fun _ v -> Printf.sprintf "%.*g" precision v

  let days_in_month ~leap m =
    match m with
    | 1 -> 31
    | 2 -> if leap then 29 else 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31
    | _ -> 30

  let is_leap_year y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

  let timestamp_to_date v =
    let secs = int_of_float v in
    let days = secs / 86400 in
    let rem = secs mod 86400 in
    let hour = rem / 3600 in
    let min = rem mod 3600 / 60 in
    let sec = rem mod 60 in
    let rec find_year days year =
      let days_in_year = if is_leap_year year then 366 else 365 in
      if days < days_in_year then (year, days)
      else find_year (days - days_in_year) (year + 1)
    in
    let year, day_of_year = find_year days 1970 in
    let leap = is_leap_year year in
    let rec find_month day month =
      let dim = days_in_month ~leap month in
      if day < dim then (month, day + 1) else find_month (day - dim) (month + 1)
    in
    let month, day = find_month day_of_year 1 in
    (year, month, day, hour, min, sec)

  let mmdd_utc _ v =
    let _, month, day, _, _, _ = timestamp_to_date v in
    Printf.sprintf "%02d/%02d" month day

  let hhmmss_utc _ v =
    let _, _, _, hour, min, sec = timestamp_to_date v in
    Printf.sprintf "%02d:%02d:%02d" hour min sec
end

(* {1 Data Transforms} *)

module Transform = struct
  let ema alpha data =
    let n = Array.length data in
    if n = 0 then [||]
    else
      let result = Array.make n (0., 0.) in
      let x0, y0 = data.(0) in
      result.(0) <- (x0, y0);
      let prev_y = ref y0 in
      for i = 1 to n - 1 do
        let x, y = data.(i) in
        let smoothed = (alpha *. y) +. ((1. -. alpha) *. !prev_y) in
        result.(i) <- (x, smoothed);
        prev_y := smoothed
      done;
      result

  let sma window data =
    let n = Array.length data in
    if n = 0 || window <= 0 then [||]
    else
      let window = min window n in
      let result = Array.make n (0., 0.) in
      let sum = ref 0. in
      for i = 0 to window - 1 do
        let _, y = data.(i) in
        sum := !sum +. y
      done;
      let x0, _ = data.(window - 1) in
      result.(window - 1) <- (x0, !sum /. float window);
      for i = window to n - 1 do
        let _, y_old = data.(i - window) in
        let x, y_new = data.(i) in
        sum := !sum -. y_old +. y_new;
        result.(i) <- (x, !sum /. float window)
      done;
      let partial_sum = ref 0. in
      for i = 0 to window - 2 do
        let x, y = data.(i) in
        partial_sum := !partial_sum +. y;
        result.(i) <- (x, !partial_sum /. float (i + 1))
      done;
      result

  let gaussian sigma data =
    let n = Array.length data in
    if n = 0 || sigma <= 0. then data
    else
      let radius = int_of_float (Float.ceil (3. *. sigma)) in
      let kernel_size = (2 * radius) + 1 in
      let kernel = Array.make kernel_size 0. in
      let kernel_sum = ref 0. in
      for i = 0 to kernel_size - 1 do
        let d = float (i - radius) in
        let w = Float.exp (-.(d *. d) /. (2. *. sigma *. sigma)) in
        kernel.(i) <- w;
        kernel_sum := !kernel_sum +. w
      done;
      for i = 0 to kernel_size - 1 do
        kernel.(i) <- kernel.(i) /. !kernel_sum
      done;
      let result = Array.make n (0., 0.) in
      for i = 0 to n - 1 do
        let x, _ = data.(i) in
        let sum = ref 0. in
        let weight_sum = ref 0. in
        for k = 0 to kernel_size - 1 do
          let j = i + k - radius in
          if j >= 0 && j < n then (
            let _, y = data.(j) in
            sum := !sum +. (kernel.(k) *. y);
            weight_sum := !weight_sum +. kernel.(k))
        done;
        let smoothed = if !weight_sum > 0. then !sum /. !weight_sum else 0. in
        result.(i) <- (x, smoothed)
      done;
      result
end

(* {1 Overlay} *)

module Overlay = struct
  type tooltip_anchor = [ `Auto | `Left | `Right | `Top | `Bottom ]
  type tooltip_border = [ `Theme | `None | `Style of Style.t ]
  type h_anchor = [ `Left | `Center | `Right ]
  type v_anchor = [ `Top | `Middle | `Bottom ]
  type arrow_head = [ `None | `Arrow | `Dot ]

  let crosshair ?style ?(pattern = `Solid) (layout : Layout.t) (grid : G.t) ~x
      ~y =
    let style = Option.value style ~default:layout.theme.crosshair in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    let ch = layout.theme.charset in
    let glyph_v, glyph_h =
      match pattern with
      | `Solid -> (ch.grid_v_solid, ch.grid_h_solid)
      | `Dashed -> (ch.grid_v_dashed, ch.grid_h_dashed)
      | `Dotted -> (ch.grid_v_dotted, ch.grid_h_dotted)
    in
    if px >= r.x && px < r.x + r.width then
      for yy = r.y to r.y + r.height - 1 do
        Render.draw_text grid ~x:px ~y:yy ~style glyph_v
      done;
    if py >= r.y && py < r.y + r.height then
      for xx = r.x to r.x + r.width - 1 do
        Render.draw_text grid ~x:xx ~y:py ~style glyph_h
      done

  let marker ?style ?(glyph = "●") (layout : Layout.t) (grid : G.t) ~x ~y =
    let style = Option.value style ~default:layout.theme.marker in
    let px, py = Layout.px_of_data layout ~x ~y in
    let r = Layout.plot_rect layout in
    if Layout.rect_contains r ~x:px ~y:py then
      Render.draw_text grid ~x:px ~y:py ~style glyph

  let pad_lines lines =
    let w =
      List.fold_left (fun acc s -> max acc (Layout.text_width s)) 0 lines
    in
    List.map
      (fun s ->
        let sw = Layout.text_width s in
        if sw >= w then s else s ^ String.make (w - sw) ' ')
      lines

  let border_size = function Some _ -> 2 | None -> 0

  let best_anchor ~rect ~px ~py ~box_w ~box_h =
    let propose = function
      | `Right -> (px + 2, py - (box_h / 2))
      | `Left -> (px - box_w - 2, py - (box_h / 2))
      | `Top -> (px - (box_w / 2), py - box_h - 2)
      | `Bottom -> (px - (box_w / 2), py + 2)
    in
    let score (x0, y0) =
      let clip =
        max 0 (rect.Layout.x - x0)
        + max 0 (x0 + box_w - (rect.x + rect.width))
        + max 0 (rect.y - y0)
        + max 0 (y0 + box_h - (rect.y + rect.height))
      in
      let overlap =
        if px >= x0 && px < x0 + box_w && py >= y0 && py < y0 + box_h then 1000
        else 0
      in
      clip + overlap
    in
    let best = ref (propose `Right) in
    let best_score = ref max_int in
    List.iter
      (fun a ->
        let pos = propose a in
        let s = score pos in
        if s < !best_score then (
          best_score := s;
          best := pos))
      [ `Right; `Left; `Top; `Bottom ];
    !best

  let draw_tooltip_border grid ~style:bst ~charset ~x0 ~y0 ~box_w ~box_h =
    if box_w >= 2 && box_h >= 2 then begin
      let tf = charset.Layout.Charset.tooltip_frame in
      let top = y0 and bot = y0 + box_h - 1 in
      let left = x0 and right = x0 + box_w - 1 in
      Render.draw_text grid ~x:left ~y:top ~style:bst tf.tl;
      Render.draw_text grid ~x:right ~y:top ~style:bst tf.tr;
      Render.draw_text grid ~x:left ~y:bot ~style:bst tf.bl;
      Render.draw_text grid ~x:right ~y:bot ~style:bst tf.br;
      for xx = left + 1 to right - 1 do
        Render.draw_text grid ~x:xx ~y:top ~style:bst tf.h;
        Render.draw_text grid ~x:xx ~y:bot ~style:bst tf.h
      done;
      for yy = top + 1 to bot - 1 do
        Render.draw_text grid ~x:left ~y:yy ~style:bst tf.v;
        Render.draw_text grid ~x:right ~y:yy ~style:bst tf.v
      done
    end

  let tooltip ?style ?(border = `Theme) ?(padding = 1) ?(anchor = `Auto)
      (layout : Layout.t) (grid : G.t) ~x ~y (lines : string list) =
    let style = Option.value style ~default:layout.theme.tooltip in
    let border =
      match border with
      | `Theme -> layout.theme.tooltip_border
      | `None -> None
      | `Style s -> Some s
    in
    let padding = max 0 padding in
    let px, py = Layout.px_of_data layout ~x ~y in
    let rect = Layout.plot_rect layout in
    if not (Layout.rect_contains rect ~x:px ~y:py) then ()
    else
      let lines = pad_lines lines in
      let content_w =
        List.fold_left (fun acc s -> max acc (Layout.text_width s)) 0 lines
      in
      let content_h = List.length lines in
      let bsz = border_size border in
      let box_w = content_w + (2 * padding) + bsz in
      let box_h = content_h + (2 * padding) + bsz in
      if box_w <= 0 || box_h <= 0 then ()
      else
        let x0, y0 =
          match anchor with
          | (`Left | `Right | `Top | `Bottom) as a -> (
              match a with
              | `Right -> (px + 2, py - (box_h / 2))
              | `Left -> (px - box_w - 2, py - (box_h / 2))
              | `Top -> (px - (box_w / 2), py - box_h - 2)
              | `Bottom -> (px - (box_w / 2), py + 2))
          | `Auto -> best_anchor ~rect ~px ~py ~box_w ~box_h
        in
        let x0 = Layout.clamp_int rect.x (rect.x + rect.width - box_w) x0 in
        let y0 = Layout.clamp_int rect.y (rect.y + rect.height - box_h) y0 in
        (* Clear background *)
        for yy = 0 to box_h - 1 do
          for xx = 0 to box_w - 1 do
            let x = x0 + xx and y = y0 + yy in
            if Layout.rect_contains rect ~x ~y then
              Render.draw_text grid ~x ~y ~style " "
          done
        done;
        (* Draw border *)
        Option.iter
          (fun bst ->
            draw_tooltip_border grid ~style:bst ~charset:layout.theme.charset
              ~x0 ~y0 ~box_w ~box_h)
          border;
        (* Draw content *)
        let inner_offset = (bsz / 2) + padding in
        List.iteri
          (fun i line ->
            let y = y0 + inner_offset + i in
            if y >= rect.y && y < rect.y + rect.height then
              Render.draw_text grid ~x:(x0 + inner_offset) ~y ~style line)
          lines

  let text ?style ?(anchor = `Left) ?(v_anchor = `Middle) (layout : Layout.t)
      (grid : G.t) ~x ~y label =
    let style = Option.value style ~default:layout.theme.labels in
    let px, py = Layout.px_of_data layout ~x ~y in
    let rect = Layout.plot_rect layout in
    let w = Layout.text_width label in
    let px =
      match anchor with
      | `Left -> px
      | `Center -> px - (w / 2)
      | `Right -> px - w
    in
    let py = match v_anchor with `Top -> py | `Middle -> py | `Bottom -> py in
    if py >= rect.y && py < rect.y + rect.height then
      Render.draw_text grid ~x:px ~y:py ~style label

  let arrow ?style ?(head = `Arrow) (layout : Layout.t) (grid : G.t) ~x1 ~y1 ~x2
      ~y2 =
    let style = Option.value style ~default:layout.theme.labels in
    let ch = layout.theme.charset in
    let line_glyphs : G.line_glyphs =
      {
        G.h = ch.frame.h;
        v = ch.frame.v;
        diag_up = ch.diag_up;
        diag_down = ch.diag_down;
      }
    in
    let px1, py1 = Layout.px_of_data layout ~x:x1 ~y:y1 in
    let px2, py2 = Layout.px_of_data layout ~x:x2 ~y:y2 in
    let rect = Layout.plot_rect layout in
    G.draw_line grid ~x1:px1 ~y1:py1 ~x2:px2 ~y2:py2 ~style ~glyphs:line_glyphs
      ~kind:`Line ();
    if Layout.rect_contains rect ~x:px2 ~y:py2 then
      match head with
      | `None -> ()
      | `Dot -> Render.draw_text grid ~x:px2 ~y:py2 ~style "●"
      | `Arrow ->
          let dx = px2 - px1 in
          let dy = py2 - py1 in
          let glyph =
            if abs dx > abs dy then if dx > 0 then "→" else "←"
            else if dy > 0 then "↓"
            else "↑"
          in
          Render.draw_text grid ~x:px2 ~y:py2 ~style glyph
end

(* {1 Legend} *)

module Legend = struct
  type item = { label : string; style : Style.t; marker : string }

  let draw ?(direction = `Vertical) ?(gap = 0) items (grid : G.t) ~width
      ~height:_ =
    match direction with
    | `Horizontal ->
        let x = ref 0 in
        let y = 0 in
        let gap = max 0 (if gap = 0 then 2 else gap) in
        List.iter
          (fun { label; style; marker } ->
            let block = marker ^ " " in
            let w_block = Layout.text_width block in
            let w_label = Layout.text_width label in
            if !x < width then (
              Render.draw_text grid ~x:!x ~y ~style block;
              let x' = !x + w_block in
              if x' < width then Render.draw_text grid ~x:x' ~y label;
              x := x' + w_label + gap))
          items
    | `Vertical ->
        let y = ref 0 in
        let gap = max 0 gap in
        List.iter
          (fun { label; style; marker } ->
            Render.draw_text grid ~x:0 ~y:!y ~style marker;
            Render.draw_text grid ~x:(Layout.text_width marker + 1) ~y:!y label;
            y := !y + 1 + gap)
          items

  let items_of_layout (layout : Layout.t) : item list =
    let charset = layout.theme.charset in
    let extract_item ~label ~style ~marker =
      match label with
      | Some l -> Some { label = l; style; marker }
      | None -> None
    in
    List.filter_map
      (fun (m : Mark.t) ->
        let style = Option.value m.style ~default:Style.default in
        match m.kind with
        | Mark.Line { glyph; _ } ->
            let marker = Option.value glyph ~default:charset.frame.h in
            extract_item ~label:m.label ~style ~marker
        | Mark.Scatter { glyph; _ } ->
            let marker = Option.value glyph ~default:charset.point_default in
            extract_item ~label:m.label ~style ~marker
        | Mark.Bar _ ->
            extract_item ~label:m.label ~style ~marker:charset.bar_fill
        | Mark.Area _ -> extract_item ~label:m.label ~style ~marker:"▒"
        | Mark.Fill_between _ -> extract_item ~label:m.label ~style ~marker:"▒"
        | Mark.Histogram _ ->
            extract_item ~label:m.label ~style ~marker:charset.bar_fill
        | _ -> None)
      layout.marks
end

(* {1 Chart} *)

type frame_config = Layout.frame_config = {
  margins : int * int * int * int;
  inner_padding : int;
}

type frame = Layout.frame = Auto | Manual of frame_config
type title = { text : string; style : Style.t option }

type t = {
  theme : Theme.t;
  frame : frame;
  x_scale : Scale.t;
  y_scale : Scale.t;
  y2_scale : Scale.t option;
  x_axis : Axis.t;
  y_axis : Axis.t;
  y2_axis : Axis.t option;
  grid : Gridlines.t;
  marks_rev : Mark.t list;
  title : title option;
}

let marks t = List.rev t.marks_rev
let default_frame = Auto

let manual_frame ?(margins = (0, 0, 0, 0)) ?(inner_padding = 0) () =
  Manual { margins; inner_padding }

let apply_theme_defaults (theme : Theme.t) (a : Axis.t) =
  {
    a with
    style = Some (Option.value a.style ~default:theme.axes);
    tick_style = Some (Option.value a.tick_style ~default:theme.axes);
    label_style = Some (Option.value a.label_style ~default:theme.labels);
  }

let apply_grid_theme (theme : Theme.t) (g : Gridlines.t) =
  { g with style = (if g.style = Style.default then theme.grid else g.style) }

let create_chart theme ~marks_rev ~title =
  {
    theme;
    frame = default_frame;
    x_scale = Scale.Auto;
    y_scale = Scale.Auto;
    y2_scale = None;
    x_axis = apply_theme_defaults theme Axis.default;
    y_axis = apply_theme_defaults theme Axis.default;
    y2_axis = None;
    grid = apply_grid_theme theme Gridlines.hidden;
    marks_rev;
    title;
  }

let empty ?(theme = Theme.default) () =
  create_chart theme ~marks_rev:[] ~title:None

let make ?(theme = Theme.default) ?title marks =
  let title = Option.map (fun text -> { text; style = None }) title in
  create_chart theme ~marks_rev:(List.rev marks) ~title

let with_theme theme t =
  {
    t with
    theme;
    x_axis = apply_theme_defaults theme t.x_axis;
    y_axis = apply_theme_defaults theme t.y_axis;
    grid = apply_grid_theme theme t.grid;
  }

let with_frame frame t =
  let frame =
    match frame with
    | Auto -> Auto
    | Manual cfg -> Manual { cfg with inner_padding = max 0 cfg.inner_padding }
  in
  { t with frame }

let with_title ?style text t = { t with title = Some { text; style } }
let with_x_scale x_scale t = { t with x_scale }
let with_y_scale y_scale t = { t with y_scale }
let with_y2_scale y2_scale t = { t with y2_scale = Some y2_scale }

let with_axes ?x ?y t =
  let x_axis =
    match x with None -> t.x_axis | Some a -> apply_theme_defaults t.theme a
  in
  let y_axis =
    match y with None -> t.y_axis | Some a -> apply_theme_defaults t.theme a
  in
  { t with x_axis; y_axis }

let with_y2_axis y2_axis t =
  { t with y2_axis = Some (apply_theme_defaults t.theme y2_axis) }

let with_grid g t = { t with grid = apply_grid_theme t.theme g }
let add m t = { t with marks_rev = m :: t.marks_rev }

(* {1 Convenience wrappers} *)

let line ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data t =
  add
    (Mark.line ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data)
    t

let line_gaps ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y data t
    =
  add
    (Mark.line_gaps ?id ?label ?style ?resolution ?pattern ?glyph ?y_axis ~x ~y
       data)
    t

let scatter ?id ?label ?style ?glyph ?mode ?y_axis ~x ~y data t =
  add (Mark.scatter ?id ?label ?style ?glyph ?mode ?y_axis ~x ~y data) t

let bar ?id ?label ?style ?direction ?mode ~category ~value data t =
  add (Mark.bar ?id ?label ?style ?direction ?mode ~category ~value data) t

let stacked_bar ?id ?direction ?gap ?size ?mode data t =
  add (Mark.stacked_bar ?id ?direction ?gap ?size ?mode data) t

let rule ?id ?style ?direction ?pattern ?y_axis value t =
  add (Mark.rule ?id ?style ?direction ?pattern ?y_axis value) t

let heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?mode ~x ~y
    ~value data t =
  add
    (Mark.heatmap ?id ?color_scale ?value_range ?auto_value_range ?agg ?mode ~x
       ~y ~value data)
    t

let candles ?id ?bullish ?bearish ?width ?body ?y_axis data t =
  add (Mark.candles ?id ?bullish ?bearish ?width ?body ?y_axis data) t

let circle ?id ?style ?resolution ?y_axis ~cx ~cy ~r data t =
  add (Mark.circle ?id ?style ?resolution ?y_axis ~cx ~cy ~r data) t

let shade ?id ?style ~min ~max t = add (Mark.shade ?id ?style ~min ~max ()) t
let column_bg ?id ?style x t = add (Mark.column_bg ?id ?style x) t

let area ?id ?label ?style ?baseline ?resolution ?y_axis ~x ~y data t =
  add (Mark.area ?id ?label ?style ?baseline ?resolution ?y_axis ~x ~y data) t

let fill_between ?id ?label ?style ?resolution ?y_axis ~x ~y_low ~y_high data t
    =
  add
    (Mark.fill_between ?id ?label ?style ?resolution ?y_axis ~x ~y_low ~y_high
       data)
    t

let histogram ?id ?label ?style ?bins ?normalize ~x data t =
  add (Mark.histogram ?id ?label ?style ?bins ?normalize ~x data) t

let to_config t : Layout.config =
  let title =
    match t.title with
    | None -> None
    | Some { text; style } ->
        let style = Option.value style ~default:t.theme.labels in
        Some ({ Layout.text; style } : Layout.title_info)
  in
  {
    Layout.theme = t.theme;
    frame = t.frame;
    x_scale = t.x_scale;
    y_scale = t.y_scale;
    y2_scale = t.y2_scale;
    x_axis = t.x_axis;
    y_axis = t.y_axis;
    y2_axis = t.y2_axis;
    grid = t.grid;
    marks = marks t;
    title;
  }

let layout ?view ?x ?y t ~width ~height =
  Layout.compute ?view ?x ?y (to_config t) ~width ~height

let draw ?view ?(x = 0) ?(y = 0) t (grid : G.t) ~width ~height : Layout.t =
  let lay = Layout.compute ?view ~x ~y (to_config t) ~width ~height in
  let bg = Option.value t.theme.background ~default:Color.default in
  G.fill_rect grid ~x ~y ~width ~height ~color:bg;
  Render.draw_grid lay grid;
  Render.draw_marks lay grid;
  Render.draw_axes lay grid;
  (match lay.title with
  | None -> ()
  | Some { text; style } ->
      let text_w = Layout.text_width text in
      let center_x = lay.plot.x + (lay.plot.width / 2) - (text_w / 2) in
      let title_y = max y (lay.plot.y - 1) in
      G.draw_text grid ~x:center_x ~y:title_y ~text ~style);
  lay
