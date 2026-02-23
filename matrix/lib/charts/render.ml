module G = Grid
module Style = Ansi.Style
module Color = Ansi.Color

(* {1 Glyph Helpers} *)

let utf8_of_uchar (u : Uchar.t) : string =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let braille_glyph_of_bits bits =
  let code = 0x2800 + bits in
  let u =
    match Uchar.of_int code with
    | exception Invalid_argument _ -> Uchar.of_int 0x2800
    | x -> x
  in
  utf8_of_uchar u

let quadrant_glyphs =
  [|
    " ";
    "▘";
    "▝";
    "▀";
    "▖";
    "▌";
    "▞";
    "▛";
    "▗";
    "▚";
    "▐";
    "▜";
    "▄";
    "▙";
    "▟";
    "█";
  |]

let quadrant_glyph_of_bits bits = quadrant_glyphs.(bits land 0xF)

(* {1 Block Glyphs} *)

let lower_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▁"
    | 2 -> Some "▂"
    | 3 -> Some "▃"
    | 4 -> Some "▄"
    | 5 -> Some "▅"
    | 6 -> Some "▆"
    | 7 -> Some "▇"
    | _ -> Some "█"

let upper_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▔"
    | 2 -> Some "▔"
    | 3 -> Some "▀"
    | 4 -> Some "▀"
    | 5 -> Some "▀"
    | 6 -> Some "▀"
    | 7 -> Some "█"
    | _ -> Some "█"

let left_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▏"
    | 2 -> Some "▎"
    | 3 -> Some "▍"
    | 4 -> Some "▌"
    | 5 -> Some "▋"
    | 6 -> Some "▊"
    | 7 -> Some "▉"
    | _ -> Some "█"

let right_block_glyph frac =
  if frac <= 0. then None
  else if frac >= 1. then Some "█"
  else
    let eighth = 0.125 in
    let e = int_of_float (Float.floor (frac /. eighth)) in
    let rem = frac -. (float e *. eighth) in
    let e = if rem >= 0.0625 then e + 1 else e in
    match e with
    | 0 -> None
    | 1 -> Some "▕"
    | 2 -> Some "▕"
    | 3 -> Some "▐"
    | 4 -> Some "▐"
    | 5 -> Some "▐"
    | 6 -> Some "▐"
    | 7 -> Some "█"
    | _ -> Some "█"

(* {1 Cohen-Sutherland Line Clipping} *)

module Clip = struct
  let inside = 0
  let left = 1
  let right = 2
  let bottom = 4
  let top = 8

  let compute_code ~xmin ~xmax ~ymin ~ymax x y =
    let code = ref inside in
    if x < xmin then code := !code lor left
    else if x > xmax then code := !code lor right;
    if y < ymin then code := !code lor bottom
    else if y > ymax then code := !code lor top;
    !code

  let line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1 ~y1 ~x2 ~y2 =
    let x1 = ref (float x1) and y1 = ref (float y1) in
    let x2 = ref (float x2) and y2 = ref (float y2) in
    let xmin = float xmin and xmax = float xmax in
    let ymin = float ymin and ymax = float ymax in
    let code1 = ref (compute_code ~xmin ~xmax ~ymin ~ymax !x1 !y1) in
    let code2 = ref (compute_code ~xmin ~xmax ~ymin ~ymax !x2 !y2) in
    let accept = ref false in
    let done_ = ref false in
    while not !done_ do
      if !code1 lor !code2 = 0 then (
        accept := true;
        done_ := true)
      else if !code1 land !code2 <> 0 then done_ := true
      else
        let code_out = if !code1 <> 0 then !code1 else !code2 in
        let x = ref 0. and y = ref 0. in
        let dx = !x2 -. !x1 and dy = !y2 -. !y1 in
        if code_out land top <> 0 then (
          x := if dy = 0. then !x1 else !x1 +. (dx *. (ymax -. !y1) /. dy);
          y := ymax)
        else if code_out land bottom <> 0 then (
          x := if dy = 0. then !x1 else !x1 +. (dx *. (ymin -. !y1) /. dy);
          y := ymin)
        else if code_out land right <> 0 then (
          y := if dx = 0. then !y1 else !y1 +. (dy *. (xmax -. !x1) /. dx);
          x := xmax)
        else if code_out land left <> 0 then (
          y := if dx = 0. then !y1 else !y1 +. (dy *. (xmin -. !x1) /. dx);
          x := xmin);
        if code_out = !code1 then (
          x1 := !x;
          y1 := !y;
          code1 := compute_code ~xmin ~xmax ~ymin ~ymax !x1 !y1)
        else (
          x2 := !x;
          y2 := !y;
          code2 := compute_code ~xmin ~xmax ~ymin ~ymax !x2 !y2)
    done;
    if !accept then
      Some
        ( int_of_float (Float.round !x1),
          int_of_float (Float.round !y1),
          int_of_float (Float.round !x2),
          int_of_float (Float.round !y2) )
    else None
end

(* {1 Heatmap Color Helpers} *)

let heatmap_color_fun ~color_scale ~vmin ~vmax =
  let len = Array.length color_scale in
  if len = 0 then fun _ -> Color.default
  else fun v ->
    let t =
      if Float.equal vmin vmax then 0.
      else Layout.clamp01 ((v -. vmin) /. Float.max 1e-12 (vmax -. vmin))
    in
    let raw = int_of_float (t *. float len) in
    let idx = Layout.clamp_int 0 (len - 1) raw in
    color_scale.(idx)

let heatmap_color_idx ~len ~vmin ~vmax v =
  if len <= 0 then 0
  else
    let t =
      if Float.equal vmin vmax then 0.
      else Layout.clamp01 ((v -. vmin) /. Float.max 1e-12 (vmax -. vmin))
    in
    let raw = int_of_float (t *. float len) in
    Layout.clamp_int 0 (len - 1) raw

(* {1 Draw Text Helper} *)

let draw_text ?style ?tab_width grid ~x ~y text =
  G.draw_text ?style ?tab_width grid ~x ~y ~text

(* {1 Grid Lines} *)

let draw_grid (layout : Layout.t) (grid : G.t) =
  let g = layout.grid in
  if not g.show then ()
  else
    let r = layout.plot in
    let style = g.style in
    let minor_style =
      match g.minor_style with Some s -> s | None -> layout.theme.grid_minor
    in
    let ch = layout.theme.charset in
    let v_glyph =
      match g.pattern with
      | `Solid -> ch.grid_v_solid
      | `Dashed -> ch.grid_v_dashed
      | `Dotted -> ch.grid_v_dotted
    in
    let h_glyph =
      match g.pattern with
      | `Solid -> ch.grid_h_solid
      | `Dashed -> ch.grid_h_dashed
      | `Dotted -> ch.grid_h_dotted
    in
    let draw_v_line ?(minor = false) x =
      let st = if minor then minor_style else style in
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x ~y ~style:st v_glyph
      done
    in
    let draw_h_line ?(minor = false) y =
      let st = if minor then minor_style else style in
      for x = r.x to r.x + r.width - 1 do
        draw_text grid ~x ~y ~style:st h_glyph
      done
    in
    let draw_every_x step =
      if step > 0 then
        for x = r.x to r.x + r.width - 1 do
          if (x - r.x) mod step = 0 then draw_v_line x
        done
    in
    let draw_every_y step =
      if step > 0 then
        for y = r.y to r.y + r.height - 1 do
          if (y - r.y) mod step = 0 then draw_h_line y
        done
    in
    let draw_minor_v ~n px0 px1 =
      if n > 1 then
        let span = px1 - px0 in
        for i = 1 to n - 1 do
          let px = px0 + (span * i / n) in
          if px > px0 && px < px1 && px >= r.x && px < r.x + r.width then
            draw_v_line ~minor:true px
        done
    in
    let draw_minor_h ~n py0 py1 =
      if n > 1 then
        let span = py1 - py0 in
        for i = 1 to n - 1 do
          let py = py0 + (span * i / n) in
          if py > py0 && py < py1 && py >= r.y && py < r.y + r.height then
            draw_h_line ~minor:true py
        done
    in
    (match g.x_step with
    | Some s -> if g.x then draw_every_x s
    | None -> (
        if g.x then
          match layout.x_scale with
          | Numeric _ ->
              let prev_px = ref None in
              List.iter
                (fun v ->
                  let px =
                    Layout.x_to_px ~minv:layout.x_view.min
                      ~maxv:layout.x_view.max ~extent:r.width ~origin:r.x
                      ~clamp:true v
                  in
                  (match (g.minor, !prev_px) with
                  | Some n, Some px0 -> draw_minor_v ~n px0 px
                  | _ -> ());
                  prev_px := Some px;
                  draw_v_line px)
                layout.x_ticks
          | Log { base; view; _ } ->
              let prev_px = ref None in
              List.iter
                (fun v ->
                  let px =
                    Layout.x_to_px_log ~base ~minv:view.min ~maxv:view.max
                      ~extent:r.width ~origin:r.x ~clamp:true v
                  in
                  (match (g.minor, !prev_px) with
                  | Some n, Some px0 -> draw_minor_v ~n px0 px
                  | _ -> ());
                  prev_px := Some px;
                  draw_v_line px)
                layout.x_ticks
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.width
              in
              let n = List.length categories in
              for i = 0 to n do
                let x0 =
                  r.x + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                if x0 >= r.x && x0 < r.x + r.width then draw_v_line x0
              done));
    match g.y_step with
    | Some s -> if g.y then draw_every_y s
    | None -> (
        if g.y then
          match layout.y_scale with
          | Numeric _ ->
              let prev_py = ref None in
              List.iter
                (fun v ->
                  let py =
                    Layout.y_to_px ~minv:layout.y_view.min
                      ~maxv:layout.y_view.max ~extent:r.height ~origin:r.y
                      ~clamp:true v
                  in
                  (match (g.minor, !prev_py) with
                  | Some n, Some py0 -> draw_minor_h ~n py0 py
                  | _ -> ());
                  prev_py := Some py;
                  draw_h_line py)
                layout.y_ticks
          | Log { base; view; _ } ->
              let prev_py = ref None in
              List.iter
                (fun v ->
                  let py =
                    Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
                      ~extent:r.height ~origin:r.y ~clamp:true v
                  in
                  (match (g.minor, !prev_py) with
                  | Some n, Some py0 -> draw_minor_h ~n py0 py
                  | _ -> ());
                  prev_py := Some py;
                  draw_h_line py)
                layout.y_ticks
          | Band { categories; padding; _ } ->
              let offset, bw =
                Layout.band_params ~cats:categories ~padding ~extent:r.height
              in
              let n = List.length categories in
              for i = 0 to n do
                let y0 =
                  r.y + int_of_float (Float.round (offset +. (float i *. bw)))
                in
                if y0 >= r.y && y0 < r.y + r.height then draw_h_line y0
              done)

(* {1 Axes} *)

let draw_axes (layout : Layout.t) (grid : G.t) =
  let r = layout.plot in
  let ip = layout.frame_inner_padding in
  let ch = layout.theme.charset in
  let line_glyphs : G.line_glyphs =
    {
      G.h = ch.frame.h;
      v = ch.frame.v;
      diag_up = ch.diag_up;
      diag_down = ch.diag_down;
    }
  in

  let draw_y_axis_line ~ax ~st =
    match layout.y_axis.line with
    | `None -> ()
    | `Axis_only ->
        G.draw_line grid ~x1:ax ~y1:r.y ~x2:ax
          ~y2:(r.y + r.height - 1)
          ~style:st ~glyphs:line_glyphs ~kind:`Line ()
    | `Frame ->
        G.draw_line grid ~x1:ax ~y1:r.y ~x2:ax
          ~y2:(r.y + r.height - 1)
          ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        G.draw_line grid ~x1:ax ~y1:r.y
          ~x2:(r.x + r.width - 1)
          ~y2:r.y ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        if not layout.x_axis.show then
          G.draw_line grid ~x1:ax
            ~y1:(r.y + r.height - 1)
            ~x2:(r.x + r.width - 1)
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ()
  in

  let draw_x_axis_line ~ay ~st =
    match layout.x_axis.line with
    | `None -> ()
    | `Axis_only ->
        G.draw_line grid ~x1:r.x ~y1:ay
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ()
    | `Frame ->
        G.draw_line grid ~x1:r.x ~y1:ay
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        G.draw_line grid
          ~x1:(r.x + r.width - 1)
          ~y1:r.y
          ~x2:(r.x + r.width - 1)
          ~y2:ay ~style:st ~glyphs:line_glyphs ~kind:`Line ();
        if not layout.y_axis.show then
          G.draw_line grid ~x1:r.x ~y1:r.y ~x2:r.x ~y2:ay ~style:st
            ~glyphs:line_glyphs ~kind:`Line ()
  in

  (* Y axis *)
  if layout.y_axis.show then (
    let ax =
      layout.margin_left + layout.y_axis_title_width + layout.y_axis_width - 1
    in
    let st = Option.get layout.y_axis.style in
    let tick_st = Option.get layout.y_axis.tick_style in
    let label_st = Option.get layout.y_axis.label_style in
    draw_y_axis_line ~ax ~st;
    (match layout.y_scale with
    | Numeric _ ->
        List.iteri
          (fun i v ->
            let py =
              Layout.y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
                ~extent:r.height ~origin:r.y ~clamp:true v
            in
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            let label = layout.y_axis.format i v in
            let lw = Layout.text_width label in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st label)
          layout.y_ticks
    | Log { base; view; _ } ->
        List.iteri
          (fun i v ->
            let py =
              Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
                ~extent:r.height ~origin:r.y ~clamp:true v
            in
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            let label = layout.y_axis.format i v in
            let lw = Layout.text_width label in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st label)
          layout.y_ticks
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.height
        in
        List.iteri
          (fun i cat ->
            let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
            let py = y0 + (band_h / 2) in
            for k = 1 to layout.y_axis.tick_length do
              let x = ax - k in
              if x >= 0 then draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
            done;
            let lw = Layout.text_width cat in
            let label_x =
              ax - layout.y_axis.tick_length - layout.y_axis.label_padding - lw
            in
            if label_x >= 0 then
              draw_text grid ~x:label_x ~y:py ~style:label_st cat)
          categories);
    if layout.x_axis.show then
      let y_corner = r.y + r.height + ip in
      if y_corner >= 0 && y_corner < layout.height then
        draw_text grid ~x:ax ~y:y_corner ~style:st ch.frame.bl);

  (* X axis *)
  if layout.x_axis.show then (
    let ay = r.y + r.height + ip in
    let st = Option.get layout.x_axis.style in
    let tick_st = Option.get layout.x_axis.tick_style in
    let label_st = Option.get layout.x_axis.label_style in
    draw_x_axis_line ~ay ~st;
    match layout.x_scale with
    | Numeric _ ->
        let last_label_right = ref min_int in
        List.iteri
          (fun i v ->
            let px =
              Layout.x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
                ~extent:r.width ~origin:r.x ~clamp:true v
            in
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
            done;
            let label = layout.x_axis.format i v in
            let lw = Layout.text_width label in
            let label_y =
              ay + layout.x_axis.tick_length + layout.x_axis.label_padding
            in
            let label_x = px - (lw / 2) in
            if
              label_y >= 0 && label_y < layout.height
              && label_x > !last_label_right
            then (
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st label;
              last_label_right := label_x + lw))
          layout.x_ticks
    | Log { base; view; _ } ->
        let last_label_right = ref min_int in
        List.iteri
          (fun i v ->
            let px =
              Layout.x_to_px_log ~base ~minv:view.min ~maxv:view.max
                ~extent:r.width ~origin:r.x ~clamp:true v
            in
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
            done;
            let label = layout.x_axis.format i v in
            let lw = Layout.text_width label in
            let label_y =
              ay + layout.x_axis.tick_length + layout.x_axis.label_padding
            in
            let label_x = px - (lw / 2) in
            if
              label_y >= 0 && label_y < layout.height
              && label_x > !last_label_right
            then (
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st label;
              last_label_right := label_x + lw))
          layout.x_ticks
    | Band { categories; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.width
        in
        let last_label_right = ref min_int in
        List.iteri
          (fun i cat ->
            let band_w = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
            let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
            let px = x0 + (band_w / 2) in
            for k = 1 to layout.x_axis.tick_length do
              let y = ay + k in
              if y >= 0 && y < layout.height then
                draw_text grid ~x:px ~y ~style:tick_st ch.tick_v
            done;
            let lw = Layout.text_width cat in
            let label_y =
              ay + layout.x_axis.tick_length + layout.x_axis.label_padding
            in
            let label_x = px - (lw / 2) in
            if
              label_y >= 0 && label_y < layout.height
              && label_x > !last_label_right
            then (
              draw_text grid ~x:(max 0 label_x) ~y:label_y ~style:label_st cat;
              last_label_right := label_x + lw))
          categories);

  (* Y2 axis *)
  (match (layout.y2_axis, layout.y2_scale, layout.y2_view) with
  | Some y2_ax, Some y2_sc, Some y2_view when y2_ax.Layout.Axis.show ->
      let ax2 = r.x + r.width + ip in
      let st = Option.get y2_ax.style in
      let tick_st = Option.get y2_ax.tick_style in
      let label_st = Option.get y2_ax.label_style in
      (match y2_ax.line with
      | `None -> ()
      | `Axis_only ->
          G.draw_line grid ~x1:ax2 ~y1:r.y ~x2:ax2
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ()
      | `Frame ->
          G.draw_line grid ~x1:ax2 ~y1:r.y ~x2:ax2
            ~y2:(r.y + r.height - 1)
            ~style:st ~glyphs:line_glyphs ~kind:`Line ();
          if not layout.y_axis.show then
            G.draw_line grid ~x1:r.x ~y1:r.y ~x2:ax2 ~y2:r.y ~style:st
              ~glyphs:line_glyphs ~kind:`Line ();
          if not layout.x_axis.show then
            G.draw_line grid ~x1:r.x
              ~y1:(r.y + r.height - 1)
              ~x2:ax2
              ~y2:(r.y + r.height - 1)
              ~style:st ~glyphs:line_glyphs ~kind:`Line ());
      let y2_tick_values = Option.value layout.y2_ticks ~default:[] in
      (match y2_sc with
      | Numeric _ ->
          List.iteri
            (fun i v ->
              let py =
                Layout.y_to_px ~minv:y2_view.min ~maxv:y2_view.max
                  ~extent:r.height ~origin:r.y ~clamp:true v
              in
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              let label = y2_ax.format i v in
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st label)
            y2_tick_values
      | Log { base; view; _ } ->
          List.iteri
            (fun i v ->
              let py =
                Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
                  ~extent:r.height ~origin:r.y ~clamp:true v
              in
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              let label = y2_ax.format i v in
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st label)
            y2_tick_values
      | Band { categories; padding; _ } ->
          let offset, bw =
            Layout.band_params ~cats:categories ~padding ~extent:r.height
          in
          List.iteri
            (fun i cat ->
              let band_h = max 1 (int_of_float (Float.max 1. (bw -. 1.))) in
              let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
              let py = y0 + (band_h / 2) in
              for k = 1 to y2_ax.tick_length do
                let x = ax2 + k in
                if x < layout.width then
                  draw_text grid ~x ~y:py ~style:tick_st ch.tick_h
              done;
              let label_x = ax2 + y2_ax.tick_length + y2_ax.label_padding in
              if label_x < layout.width then
                draw_text grid ~x:label_x ~y:py ~style:label_st cat)
            categories);
      if layout.x_axis.show then
        let y_corner = r.y + r.height + ip in
        if y_corner >= 0 && y_corner < layout.height then
          draw_text grid ~x:ax2 ~y:y_corner ~style:st ch.frame.br
  | _ -> ());

  (* Y-axis title *)
  (match layout.y_axis.title with
  | None -> ()
  | Some { text; style } ->
      let default_st = Option.get layout.y_axis.label_style in
      let title_style = Option.value ~default:default_st style in
      let graphemes = ref [] in
      Glyph.String.iter_graphemes
        (fun ~offset ~len ->
          graphemes := String.sub text offset len :: !graphemes)
        text;
      let chars = List.rev !graphemes in
      let title_len = List.length chars in
      let title_y_start = r.y + ((r.height - title_len) / 2) in
      let title_x = layout.margin_left in
      List.iteri
        (fun i char_str ->
          let y = title_y_start + i in
          if y >= r.y && y < r.y + r.height then
            draw_text grid ~x:title_x ~y ~style:title_style char_str)
        chars);

  (* X-axis title *)
  match layout.x_axis.title with
  | None -> ()
  | Some { text; style } ->
      let default_st = Option.get layout.x_axis.label_style in
      let title_style = Option.value ~default:default_st style in
      let title_w = Layout.text_width text in
      let title_x = r.x + ((r.width - title_w) / 2) in
      let title_y =
        r.y + r.height + ip + layout.x_axis.tick_length
        + layout.x_axis.label_padding + 2
      in
      if title_y < layout.height then
        draw_text grid ~x:(max 0 title_x) ~y:title_y ~style:title_style text

(* {1 Marks} *)

let draw_marks (layout : Layout.t) (grid : G.t) =
  let r = layout.plot in

  (* Coordinate mapping closures computed once per draw call *)
  let x_to_px_cell =
    match layout.x_scale with
    | Numeric { view; _ } ->
        fun x ->
          Layout.x_to_px ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:true x
    | Log { base; view; _ } ->
        fun x ->
          Layout.x_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:true x
    | Band _ ->
        fun x ->
          Layout.x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
            ~extent:r.width ~origin:r.x ~clamp:true x
  in
  let x_to_px_unclamped =
    match layout.x_scale with
    | Numeric { view; _ } ->
        fun x ->
          Layout.x_to_px ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:false x
    | Log { base; view; _ } ->
        fun x ->
          Layout.x_to_px_log ~base ~minv:view.min ~maxv:view.max ~extent:r.width
            ~origin:r.x ~clamp:false x
    | Band _ ->
        fun x ->
          Layout.x_to_px ~minv:layout.x_view.min ~maxv:layout.x_view.max
            ~extent:r.width ~origin:r.x ~clamp:false x
  in
  let y1_to_px_cell =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          Layout.y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | Log { base; view; _ } ->
        fun y ->
          Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
            ~extent:r.height ~origin:r.y ~clamp:true y
    | Band _ ->
        fun y ->
          Layout.y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:true y
  in
  let y1_to_px_unclamped =
    match layout.y_scale with
    | Numeric { view; _ } ->
        fun y ->
          Layout.y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | Log { base; view; _ } ->
        fun y ->
          Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
            ~extent:r.height ~origin:r.y ~clamp:false y
    | Band _ ->
        fun y ->
          Layout.y_to_px ~minv:layout.y_view.min ~maxv:layout.y_view.max
            ~extent:r.height ~origin:r.y ~clamp:false y
  in
  let y2_to_px_cell =
    match (layout.y2_scale, layout.y2_view) with
    | Some (Numeric { view; _ }), _ ->
        fun y ->
          Layout.y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | Some (Log { base; view; _ }), _ ->
        fun y ->
          Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
            ~extent:r.height ~origin:r.y ~clamp:true y
    | Some (Band _), Some y2_view ->
        fun y ->
          Layout.y_to_px ~minv:y2_view.min ~maxv:y2_view.max ~extent:r.height
            ~origin:r.y ~clamp:true y
    | _ -> y1_to_px_cell
  in
  let y2_to_px_unclamped =
    match (layout.y2_scale, layout.y2_view) with
    | Some (Numeric { view; _ }), _ ->
        fun y ->
          Layout.y_to_px ~minv:view.min ~maxv:view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | Some (Log { base; view; _ }), _ ->
        fun y ->
          Layout.y_to_px_log ~base ~minv:view.min ~maxv:view.max
            ~extent:r.height ~origin:r.y ~clamp:false y
    | Some (Band _), Some y2_view ->
        fun y ->
          Layout.y_to_px ~minv:y2_view.min ~maxv:y2_view.max ~extent:r.height
            ~origin:r.y ~clamp:false y
    | _ -> y1_to_px_unclamped
  in
  let y_to_px_cell = y1_to_px_cell in
  let select_y_to_px_cell = function
    | `Y1 -> y1_to_px_cell
    | `Y2 -> y2_to_px_cell
  in
  let select_y_to_px_unclamped = function
    | `Y1 -> y1_to_px_unclamped
    | `Y2 -> y2_to_px_unclamped
  in
  let select_y_view = function
    | `Y1 -> layout.y_view
    | `Y2 -> Option.value layout.y2_view ~default:layout.y_view
  in

  (* Band axis bar placement helpers *)
  let x_band =
    match layout.x_scale with
    | Band { categories; index_of; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.width
        in
        let band_w = max 1 (int_of_float bw - 1) in
        Some (categories, index_of, offset, bw, band_w)
    | _ -> None
  in
  let y_band =
    match layout.y_scale with
    | Band { categories; index_of; padding; _ } ->
        let offset, bw =
          Layout.band_params ~cats:categories ~padding ~extent:r.height
        in
        let band_h = max 1 (int_of_float bw - 1) in
        Some (categories, index_of, offset, bw, band_h)
    | _ -> None
  in

  (* Grid.line_glyphs for theme-aware line rendering *)
  let line_glyphs : G.line_glyphs =
    let charset = layout.theme.charset in
    {
      G.h = charset.frame.h;
      v = charset.frame.v;
      diag_up = charset.diag_up;
      diag_down = charset.diag_down;
    }
  in

  (* Block2x2 dot buffer *)
  let block2x2_dots = Array.make (r.width * r.height) 0 in

  let block2x2_set_dot px py =
    let cell_x = px / 2 in
    let cell_y = py / 2 in
    if
      cell_x >= r.x
      && cell_x < r.x + r.width
      && cell_y >= r.y
      && cell_y < r.y + r.height
    then
      let sub_x = px mod 2 in
      let sub_y = py mod 2 in
      let bit = (sub_y * 2) + sub_x in
      let idx = cell_x - r.x + ((cell_y - r.y) * r.width) in
      Array.unsafe_set block2x2_dots idx
        (Array.unsafe_get block2x2_dots idx lor (1 lsl bit))
  in

  let block2x2_clear () = Array.fill block2x2_dots 0 (r.width * r.height) 0 in

  let render_block2x2 style =
    for cy = r.y to r.y + r.height - 1 do
      for cx = r.x to r.x + r.width - 1 do
        let idx = cx - r.x + ((cy - r.y) * r.width) in
        let bits = Array.unsafe_get block2x2_dots idx in
        if bits <> 0 then
          let glyph = quadrant_glyph_of_bits bits in
          draw_text grid ~x:cx ~y:cy ~style glyph
      done
    done
  in

  (* Braille dot buffer *)
  let braille_dots = Array.make (r.width * r.height) 0 in

  let braille_set_dot x_sub y_sub =
    let cell_x = x_sub / 2 in
    let cell_y = y_sub / 4 in
    if
      cell_x >= r.x
      && cell_x < r.x + r.width
      && cell_y >= r.y
      && cell_y < r.y + r.height
    then
      let bit_x = x_sub land 1 in
      let bit_y = y_sub mod 4 in
      let bit_pos =
        match (bit_x, bit_y) with
        | 0, 0 -> 0
        | 0, 1 -> 1
        | 0, 2 -> 2
        | 0, 3 -> 6
        | 1, 0 -> 3
        | 1, 1 -> 4
        | 1, 2 -> 5
        | 1, 3 -> 7
        | _ -> 0
      in
      let idx = cell_x - r.x + ((cell_y - r.y) * r.width) in
      Array.unsafe_set braille_dots idx
        (Array.unsafe_get braille_dots idx lor (1 lsl bit_pos))
  in

  let braille_clear () = Array.fill braille_dots 0 (r.width * r.height) 0 in

  let render_braille style =
    for cy = r.y to r.y + r.height - 1 do
      for cx = r.x to r.x + r.width - 1 do
        let idx = cx - r.x + ((cy - r.y) * r.width) in
        let bits = Array.unsafe_get braille_dots idx in
        if bits <> 0 then
          let glyph = braille_glyph_of_bits bits in
          draw_text grid ~x:cx ~y:cy ~style glyph
      done
    done
  in

  let draw_block2x2_line (x1, y1) (x2, y2) =
    let dx = abs (x2 - x1) and dy = abs (y2 - y1) in
    let sx = if x1 < x2 then 1 else -1 in
    let sy = if y1 < y2 then 1 else -1 in
    let err = ref (dx - dy) in
    let x = ref x1 and y = ref y1 in
    while not (!x = x2 && !y = y2) do
      block2x2_set_dot !x !y;
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        x := !x + sx);
      if e2 < dx then (
        err := !err + dx;
        y := !y + sy)
    done;
    block2x2_set_dot x2 y2
  in

  let stipple_should_draw ~pattern ~step =
    match pattern with
    | `Solid -> true
    | `Dashed -> step mod 5 < 3
    | `Dotted -> step mod 2 = 0
  in

  let draw_stippled_line ~style ~pattern ~step_counter (x1, y1) (x2, y2) =
    let dx = abs (x2 - x1) and dy = abs (y2 - y1) in
    let sx = if x1 < x2 then 1 else -1 in
    let sy = if y1 < y2 then 1 else -1 in
    let err = ref (dx - dy) in
    let px = ref x1 and py = ref y1 in
    let glyph = if dx > dy then "─" else if dy > dx then "│" else "·" in
    while not (!px = x2 && !py = y2) do
      if stipple_should_draw ~pattern ~step:!step_counter then
        if Layout.rect_contains r ~x:!px ~y:!py then
          draw_text grid ~x:!px ~y:!py ~style glyph;
      incr step_counter;
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        px := !px + sx);
      if e2 < dx then (
        err := !err + dx;
        py := !py + sy)
    done;
    if stipple_should_draw ~pattern ~step:!step_counter then
      if Layout.rect_contains r ~x:x2 ~y:y2 then
        draw_text grid ~x:x2 ~y:y2 ~style glyph;
    incr step_counter
  in

  (* Line series rendering: handles gaps via NaN in ya *)
  let draw_line_series ~y_axis ~style ~pattern ~kind ~xa ~ya =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_to_px_unclamped' = select_y_to_px_unclamped y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length xa in
    match kind with
    | `Points glyph ->
        for i = 0 to n - 1 do
          let yi = ya.(i) in
          if Float.is_finite yi then
            let px = x_to_px_cell xa.(i) in
            let py = y_to_px_cell' yi in
            if Layout.rect_contains r ~x:px ~y:py then
              draw_text grid ~x:px ~y:py ~style glyph
        done
    | `Wave ->
        let seq_y = Array.make r.width (-1) in
        let set_col x yval =
          let ix = x - r.x in
          if ix >= 0 && ix < r.width then seq_y.(ix) <- yval
        in
        let draw_seg (x1, y1) (x2, y2) =
          if x1 = x2 then set_col x1 y2
          else
            let x_start = min x1 x2 and x_end = max x1 x2 in
            for xc = x_start to x_end do
              let t = float (xc - x1) /. float (x2 - x1) in
              let yc =
                int_of_float (Float.round (float y1 +. (t *. float (y2 - y1))))
              in
              set_col xc yc
            done
        in
        let prev = ref None in
        for i = 0 to n - 1 do
          let yi = ya.(i) in
          if not (Float.is_finite yi) then prev := None
          else begin
            let px = x_to_px_cell xa.(i) in
            let py = y_to_px_cell' yi in
            (match !prev with
            | None -> ()
            | Some (px0, py0) -> draw_seg (px0, py0) (px, py));
            prev := Some (px, py)
          end
        done;
        let prev_y = ref None in
        for i = 0 to r.width - 1 do
          let yval = seq_y.(i) in
          if yval < 0 then prev_y := None
          else if yval >= r.y && yval < r.y + r.height then
            match !prev_y with
            | None ->
                draw_text grid ~x:(r.x + i) ~y:yval ~style "─";
                prev_y := Some yval
            | Some py ->
                let xc = r.x + i in
                if py = yval then draw_text grid ~x:xc ~y:yval ~style "─"
                else if py > yval then (
                  draw_text grid ~x:xc ~y:yval ~style "╭";
                  if py >= r.y && py < r.y + r.height then
                    draw_text grid ~x:xc ~y:py ~style "╯";
                  for yy = yval + 1 to py - 1 do
                    if yy >= r.y && yy < r.y + r.height then
                      draw_text grid ~x:xc ~y:yy ~style "│"
                  done)
                else (
                  draw_text grid ~x:xc ~y:yval ~style "╰";
                  if py >= r.y && py < r.y + r.height then
                    draw_text grid ~x:xc ~y:py ~style "╮";
                  for yy = py + 1 to yval - 1 do
                    if yy >= r.y && yy < r.y + r.height then
                      draw_text grid ~x:xc ~y:yy ~style "│"
                  done);
                prev_y := Some yval
        done
    | `Line ->
        let prev = ref None in
        let xmin = r.x and xmax = r.x + r.width - 1 in
        let ymin = r.y and ymax = r.y + r.height - 1 in
        let step_counter = ref 0 in
        for i = 0 to n - 1 do
          let yi = ya.(i) in
          if not (Float.is_finite yi) then (
            step_counter := 0;
            prev := None)
          else begin
            let px = x_to_px_unclamped xa.(i) in
            let py = y_to_px_unclamped' yi in
            (match !prev with
            | None -> ()
            | Some (px0, py0) -> (
                match
                  Clip.line_to_rect ~xmin ~xmax ~ymin ~ymax ~x1:px0 ~y1:py0
                    ~x2:px ~y2:py
                with
                | Some (x1, y1, x2, y2) -> (
                    match pattern with
                    | `Solid ->
                        G.draw_line grid ~x1 ~y1 ~x2 ~y2 ~style
                          ~glyphs:line_glyphs ~kind:`Line ()
                    | `Dashed | `Dotted ->
                        draw_stippled_line ~style ~pattern ~step_counter
                          (x1, y1) (x2, y2))
                | None -> ()));
            prev := Some (px, py)
          end
        done
    | `Block2x2 ->
        let gx = r.width * 2 in
        let gy = r.height * 2 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = (select_y_view y_axis).min
        and ymax = (select_y_view y_axis).max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let clip_xmin = r.x * 2 in
        let clip_xmax = ((r.x + r.width) * 2) - 1 in
        let clip_ymin = r.y * 2 in
        let clip_ymax = ((r.y + r.height) * 2) - 1 in
        block2x2_clear ();
        let prev = ref None in
        for i = 0 to n - 1 do
          let yi = ya.(i) in
          if not (Float.is_finite yi) then (
            render_block2x2 style;
            block2x2_clear ();
            prev := None)
          else begin
            let xv = xa.(i) in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (yi -. ymin) *. float (gy - 1) /. dy
            in
            let px = (r.x * 2) + int_of_float (Float.round sx) in
            let py = (r.y * 2) + (gy - 1 - int_of_float (Float.round sy)) in
            (match !prev with
            | None -> ()
            | Some (px0, py0) -> (
                match
                  Clip.line_to_rect ~xmin:clip_xmin ~xmax:clip_xmax
                    ~ymin:clip_ymin ~ymax:clip_ymax ~x1:px0 ~y1:py0 ~x2:px
                    ~y2:py
                with
                | Some (x1, y1, x2, y2) -> draw_block2x2_line (x1, y1) (x2, y2)
                | None -> ()));
            prev := Some (px, py)
          end
        done;
        render_block2x2 style
    | `Braille ->
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        let clip_xmin = r.x * 2 in
        let clip_xmax = ((r.x + r.width) * 2) - 1 in
        let clip_ymin = r.y * 4 in
        let clip_ymax = ((r.y + r.height) * 4) - 1 in
        let prev = ref None in
        for i = 0 to n - 1 do
          let yi = ya.(i) in
          if not (Float.is_finite yi) then prev := None
          else begin
            let xv = xa.(i) in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (yi -. ymin) *. float (gy - 1) /. dy
            in
            let px = (r.x * 2) + int_of_float (Float.round sx) in
            let py = (r.y * 4) + (gy - 1 - int_of_float (Float.round sy)) in
            (match !prev with
            | None -> ()
            | Some (px0, py0) -> (
                match
                  Clip.line_to_rect ~xmin:clip_xmin ~xmax:clip_xmax
                    ~ymin:clip_ymin ~ymax:clip_ymax ~x1:px0 ~y1:py0 ~x2:px
                    ~y2:py
                with
                | Some (x1, y1, x2, y2) ->
                    G.draw_line grid ~x1 ~y1 ~x2 ~y2 ~style ~kind:`Braille ()
                | None -> ()));
            prev := Some (px, py)
          end
        done
  in

  let draw_scatter ~y_axis ~style ~glyph ~kind ~xa ~ya =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length xa in
    match kind with
    | `Cell ->
        for i = 0 to n - 1 do
          let px = x_to_px_cell xa.(i) in
          let py = y_to_px_cell' ya.(i) in
          if Layout.rect_contains r ~x:px ~y:py then
            draw_text grid ~x:px ~y:py ~style glyph
        done
    | `Density ->
        let w = r.width and h = r.height in
        if w > 0 && h > 0 then (
          let counts = Array.make (w * h) 0 in
          let max_count = ref 0 in
          for i = 0 to n - 1 do
            let px = x_to_px_cell xa.(i) in
            let py = y_to_px_cell' ya.(i) in
            if Layout.rect_contains r ~x:px ~y:py then (
              let idx = ((py - r.y) * w) + (px - r.x) in
              let new_count = counts.(idx) + 1 in
              counts.(idx) <- new_count;
              max_count := max !max_count new_count)
          done;
          let shade_chars = layout.theme.charset.shade_levels in
          let num_levels = Array.length shade_chars in
          let max_c = max 1 !max_count in
          for py = r.y to r.y + h - 1 do
            for px = r.x to r.x + w - 1 do
              let idx = ((py - r.y) * w) + (px - r.x) in
              let count = counts.(idx) in
              if count > 0 then
                let t = float count /. float max_c in
                let level_idx =
                  max 0
                    (min (num_levels - 1)
                       (int_of_float (t *. float (num_levels - 1))))
                in
                draw_text grid ~x:px ~y:py ~style shade_chars.(level_idx)
            done
          done)
    | `Braille ->
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        braille_clear ();
        for i = 0 to n - 1 do
          let xv = xa.(i) and yv = ya.(i) in
          let sx =
            if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
          in
          let sy =
            if dy <= 0. then 0. else (yv -. ymin) *. float (gy - 1) /. dy
          in
          let xg = int_of_float (Float.round sx) in
          let yg = int_of_float (Float.round sy) in
          let x_sub = (r.x * 2) + xg in
          let y_sub = (r.y * 4) + (gy - 1 - yg) in
          braille_set_dot x_sub y_sub
        done;
        render_braille style
  in

  let draw_area ~y_axis ~style ~baseline ~resolution ~xa ~ya =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length xa in
    if n = 0 then ()
    else
      let baseline_y = match baseline with `Zero -> 0. | `Value v -> v in
      match resolution with
      | `Cell | `Wave | `Block2x2 ->
          for i = 0 to n - 1 do
            let xv = xa.(i) in
            let yv = ya.(i) in
            let px = x_to_px_cell xv in
            if px >= r.x && px < r.x + r.width then
              let py_data = y_to_px_cell' yv in
              let py_base = y_to_px_cell' baseline_y in
              let py_top = min py_data py_base in
              let py_bot = max py_data py_base in
              for py = py_top to py_bot do
                if py >= r.y && py < r.y + r.height then
                  draw_text grid ~x:px ~y:py ~style "█"
              done
          done
      | `Braille2x4 ->
          let gx = r.width * 2 in
          let gy = r.height * 4 in
          let xmin = layout.x_view.min and xmax = layout.x_view.max in
          let ymin = y_view.min and ymax = y_view.max in
          let dx = xmax -. xmin and dy = ymax -. ymin in
          braille_clear ();
          let base_sub =
            let sy =
              if dy <= 0. then 0.
              else (baseline_y -. ymin) *. float (gy - 1) /. dy
            in
            (r.y * 4) + (gy - 1 - int_of_float (Float.round sy))
          in
          for i = 0 to n - 1 do
            let xv = xa.(i) in
            let yv = ya.(i) in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy =
              if dy <= 0. then 0. else (yv -. ymin) *. float (gy - 1) /. dy
            in
            let x_sub = (r.x * 2) + int_of_float (Float.round sx) in
            let y_sub = (r.y * 4) + (gy - 1 - int_of_float (Float.round sy)) in
            let y_top = min y_sub base_sub in
            let y_bot = max y_sub base_sub in
            for ys = y_top to y_bot do
              braille_set_dot x_sub ys
            done
          done;
          render_braille style
  in

  let draw_fill_between ~y_axis ~style ~resolution ~xa ~yla ~yha =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length xa in
    if n = 0 then ()
    else
      match resolution with
      | `Cell | `Wave | `Block2x2 ->
          for i = 0 to n - 1 do
            let xv = xa.(i) in
            let yv_low = yla.(i) in
            let yv_high = yha.(i) in
            let px = x_to_px_cell xv in
            if px >= r.x && px < r.x + r.width then
              let py_low = y_to_px_cell' yv_low in
              let py_high = y_to_px_cell' yv_high in
              let py_top = min py_low py_high in
              let py_bot = max py_low py_high in
              for py = py_top to py_bot do
                if py >= r.y && py < r.y + r.height then
                  draw_text grid ~x:px ~y:py ~style "▒"
              done
          done
      | `Braille2x4 ->
          let gx = r.width * 2 in
          let gy = r.height * 4 in
          let xmin = layout.x_view.min and xmax = layout.x_view.max in
          let ymin = y_view.min and ymax = y_view.max in
          let dx = xmax -. xmin and dy = ymax -. ymin in
          braille_clear ();
          for i = 0 to n - 1 do
            let xv = xa.(i) in
            let yv_low = yla.(i) in
            let yv_high = yha.(i) in
            let sx =
              if dx <= 0. then 0. else (xv -. xmin) *. float (gx - 1) /. dx
            in
            let sy_low =
              if dy <= 0. then 0. else (yv_low -. ymin) *. float (gy - 1) /. dy
            in
            let sy_high =
              if dy <= 0. then 0. else (yv_high -. ymin) *. float (gy - 1) /. dy
            in
            let x_sub = (r.x * 2) + int_of_float (Float.round sx) in
            let y_sub_low =
              (r.y * 4) + (gy - 1 - int_of_float (Float.round sy_low))
            in
            let y_sub_high =
              (r.y * 4) + (gy - 1 - int_of_float (Float.round sy_high))
            in
            let y_top = min y_sub_low y_sub_high in
            let y_bot = max y_sub_low y_sub_high in
            for ys = y_top to y_bot do
              braille_set_dot x_sub ys
            done
          done;
          render_braille style
  in

  let draw_bars_y ~style ~mode ~categories ~values =
    match x_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_w) ->
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        let baseline_clamped = Float.max y_view.min (Float.min y_view.max 0.) in
        let n = Array.length categories in
        for k = 0 to n - 1 do
          match Layout.band_index_fast index_of categories.(k) with
          | None -> ()
          | Some i -> (
              let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
              let v = values.(k) in
              let y0v = Float.min baseline_clamped v in
              let y1v = Float.max baseline_clamped v in
              let y0_f = (y0v -. y_view.min) *. scale in
              let y1_f = (y1v -. y_view.min) *. scale in
              match mode with
              | `Cell ->
                  let lo_cell = int_of_float (Float.round y0_f) in
                  let hi_cell = int_of_float (Float.round y1_f) in
                  for kk = lo_cell to hi_cell - 1 do
                    let yy = r.y + r.height - 1 - kk in
                    if yy >= r.y && yy < r.y + r.height then
                      for xx = 0 to band_w - 1 do
                        let px = x0 + xx in
                        if px >= r.x && px < r.x + r.width then
                          draw_text grid ~x:px ~y:yy ~style "█"
                      done
                  done
              | `Half_block -> (
                  let lo_cell = int_of_float (Float.floor y0_f) in
                  let hi_cell = int_of_float (Float.floor y1_f) in
                  let lo_frac = y0_f -. float lo_cell in
                  let hi_frac = y1_f -. float hi_cell in
                  let bot_glyph =
                    if lo_frac > 1e-6 then upper_block_glyph (1. -. lo_frac)
                    else None
                  in
                  let top_glyph = lower_block_glyph hi_frac in
                  (match bot_glyph with
                  | None -> ()
                  | Some glyph ->
                      let yy = r.y + r.height - 1 - lo_cell in
                      if yy >= r.y && yy < r.y + r.height then
                        for xx = 0 to band_w - 1 do
                          let px = x0 + xx in
                          if px >= r.x && px < r.x + r.width then
                            draw_text grid ~x:px ~y:yy ~style glyph
                        done);
                  let start_cell =
                    if lo_frac > 1e-6 then lo_cell + 1 else lo_cell
                  in
                  for kk = start_cell to hi_cell - 1 do
                    let yy = r.y + r.height - 1 - kk in
                    if yy >= r.y && yy < r.y + r.height then
                      for xx = 0 to band_w - 1 do
                        let px = x0 + xx in
                        if px >= r.x && px < r.x + r.width then
                          draw_text grid ~x:px ~y:yy ~style "█"
                      done
                  done;
                  match top_glyph with
                  | None -> ()
                  | Some glyph ->
                      let yy = r.y + r.height - 1 - hi_cell in
                      if yy >= r.y && yy < r.y + r.height then
                        for xx = 0 to band_w - 1 do
                          let px = x0 + xx in
                          if px >= r.x && px < r.x + r.width then
                            draw_text grid ~x:px ~y:yy ~style glyph
                        done))
        done
  in

  let draw_bars_x ~style ~mode ~categories ~values =
    match y_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_h) ->
        let x_view = layout.x_view in
        let x_span = x_view.max -. x_view.min in
        let x_span = if x_span <= 0. then 1. else x_span in
        let scale = float r.width /. x_span in
        let baseline_clamped = Float.max x_view.min (Float.min x_view.max 0.) in
        let n = Array.length categories in
        for k = 0 to n - 1 do
          match Layout.band_index_fast index_of categories.(k) with
          | None -> ()
          | Some i -> (
              let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
              let v = values.(k) in
              let x0v = Float.min baseline_clamped v in
              let x1v = Float.max baseline_clamped v in
              let x0_f = (x0v -. x_view.min) *. scale in
              let x1_f = (x1v -. x_view.min) *. scale in
              match mode with
              | `Cell ->
                  let lo_cell = int_of_float (Float.round x0_f) in
                  let hi_cell = int_of_float (Float.round x1_f) in
                  for kk = lo_cell to hi_cell - 1 do
                    let xx = r.x + kk in
                    if xx >= r.x && xx < r.x + r.width then
                      for yy = 0 to band_h - 1 do
                        let py = y0 + yy in
                        if py >= r.y && py < r.y + r.height then
                          draw_text grid ~x:xx ~y:py ~style "█"
                      done
                  done
              | `Half_block -> (
                  let lo_cell = int_of_float (Float.floor x0_f) in
                  let hi_cell = int_of_float (Float.floor x1_f) in
                  let lo_frac = x0_f -. float lo_cell in
                  let hi_frac = x1_f -. float hi_cell in
                  let left_glyph =
                    if lo_frac > 1e-6 then right_block_glyph (1. -. lo_frac)
                    else None
                  in
                  let right_glyph = left_block_glyph hi_frac in
                  (match left_glyph with
                  | None -> ()
                  | Some glyph ->
                      let xx = r.x + lo_cell in
                      if xx >= r.x && xx < r.x + r.width then
                        for yy = 0 to band_h - 1 do
                          let py = y0 + yy in
                          if py >= r.y && py < r.y + r.height then
                            draw_text grid ~x:xx ~y:py ~style glyph
                        done);
                  let start_cell =
                    if lo_frac > 1e-6 then lo_cell + 1 else lo_cell
                  in
                  for kk = start_cell to hi_cell - 1 do
                    let xx = r.x + kk in
                    if xx >= r.x && xx < r.x + r.width then
                      for yy = 0 to band_h - 1 do
                        let py = y0 + yy in
                        if py >= r.y && py < r.y + r.height then
                          draw_text grid ~x:xx ~y:py ~style "█"
                      done
                  done;
                  match right_glyph with
                  | None -> ()
                  | Some glyph ->
                      let xx = r.x + hi_cell in
                      if xx >= r.x && xx < r.x + r.width then
                        for yy = 0 to band_h - 1 do
                          let py = y0 + yy in
                          if py >= r.y && py < r.y + r.height then
                            draw_text grid ~x:xx ~y:py ~style glyph
                        done))
        done
  in

  let draw_stacked_y ~gap ~bar_width ~mode data =
    match x_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_w_auto) ->
        let bw_cells =
          match bar_width with
          | Some w -> w
          | None ->
              let gap = max 0 gap in
              max 1 (int_of_float bw - gap - 1)
        in
        let bw_cells = min bw_cells band_w_auto in
        let y_view = layout.y_view in
        let y_span = y_view.max -. y_view.min in
        let y_span = if y_span <= 0. then 1. else y_span in
        let scale = float r.height /. y_span in
        Array.iter
          (fun (b : Mark.stacked_bar) ->
            match Layout.band_index_fast index_of b.category with
            | None -> ()
            | Some i ->
                let x0 = r.x + int_of_float (offset +. (float i *. bw)) in
                let cum = ref 0. in
                List.iter
                  (fun (seg : Mark.bar_segment) ->
                    let v = max 0. seg.value in
                    let y0v = !cum in
                    cum := !cum +. v;
                    let y1v = !cum in
                    let y0_f = (y0v -. y_view.min) *. scale in
                    let y1_f = (y1v -. y_view.min) *. scale in
                    match mode with
                    | `Cell ->
                        let y0_cells = int_of_float (Float.round y0_f) in
                        let y1_cells = int_of_float (Float.round y1_f) in
                        for kk = y0_cells to y1_cells - 1 do
                          let yy = r.y + r.height - 1 - kk in
                          if yy >= r.y && yy < r.y + r.height then
                            for xx = 0 to bw_cells - 1 do
                              let px = x0 + xx in
                              if px >= r.x && px < r.x + r.width then
                                draw_text grid ~x:px ~y:yy ~style:seg.style "█"
                            done
                        done
                    | `Half_block -> (
                        let y0_cells = int_of_float (Float.floor y0_f) in
                        let y1_cells = int_of_float (Float.floor y1_f) in
                        let y1_frac = y1_f -. float y1_cells in
                        let top_glyph = lower_block_glyph y1_frac in
                        for kk = y0_cells to y1_cells - 1 do
                          let yy = r.y + r.height - 1 - kk in
                          if yy >= r.y && yy < r.y + r.height then
                            for xx = 0 to bw_cells - 1 do
                              let px = x0 + xx in
                              if px >= r.x && px < r.x + r.width then
                                draw_text grid ~x:px ~y:yy ~style:seg.style "█"
                            done
                        done;
                        match top_glyph with
                        | None -> ()
                        | Some glyph ->
                            let yy = r.y + r.height - 1 - y1_cells in
                            if yy >= r.y && yy < r.y + r.height then
                              for xx = 0 to bw_cells - 1 do
                                let px = x0 + xx in
                                if px >= r.x && px < r.x + r.width then
                                  draw_text grid ~x:px ~y:yy ~style:seg.style
                                    glyph
                              done))
                  b.segments)
          data
  in

  let draw_stacked_x ~gap ~bar_height ~mode data =
    match y_band with
    | None -> ()
    | Some (_cats, index_of, offset, bw, band_h_auto) ->
        let bh_cells =
          match bar_height with
          | Some h -> h
          | None ->
              let gap = max 0 gap in
              max 1 (int_of_float bw - gap - 1)
        in
        let bh_cells = min bh_cells band_h_auto in
        let x_view = layout.x_view in
        let x_span = x_view.max -. x_view.min in
        let x_span = if x_span <= 0. then 1. else x_span in
        let scale = float r.width /. x_span in
        Array.iter
          (fun (b : Mark.stacked_bar) ->
            match Layout.band_index_fast index_of b.category with
            | None -> ()
            | Some i ->
                let y0 = r.y + int_of_float (offset +. (float i *. bw)) in
                let cum = ref 0. in
                List.iter
                  (fun (seg : Mark.bar_segment) ->
                    let v = max 0. seg.value in
                    let x0v = !cum in
                    cum := !cum +. v;
                    let x1v = !cum in
                    let x0_f = (x0v -. x_view.min) *. scale in
                    let x1_f = (x1v -. x_view.min) *. scale in
                    match mode with
                    | `Cell ->
                        let x0_cells = int_of_float (Float.round x0_f) in
                        let x1_cells = int_of_float (Float.round x1_f) in
                        for kk = x0_cells to x1_cells - 1 do
                          let xx = r.x + kk in
                          if xx >= r.x && xx < r.x + r.width then
                            for yy = 0 to bh_cells - 1 do
                              let py = y0 + yy in
                              if py >= r.y && py < r.y + r.height then
                                draw_text grid ~x:xx ~y:py ~style:seg.style "█"
                            done
                        done
                    | `Half_block -> (
                        let x0_cells = int_of_float (Float.floor x0_f) in
                        let x1_cells = int_of_float (Float.floor x1_f) in
                        let x1_frac = x1_f -. float x1_cells in
                        let right_glyph = left_block_glyph x1_frac in
                        for kk = x0_cells to x1_cells - 1 do
                          let xx = r.x + kk in
                          if xx >= r.x && xx < r.x + r.width then
                            for yy = 0 to bh_cells - 1 do
                              let py = y0 + yy in
                              if py >= r.y && py < r.y + r.height then
                                draw_text grid ~x:xx ~y:py ~style:seg.style "█"
                            done
                        done;
                        match right_glyph with
                        | None -> ()
                        | Some glyph ->
                            let xx = r.x + x1_cells in
                            if xx >= r.x && xx < r.x + r.width then
                              for yy = 0 to bh_cells - 1 do
                                let py = y0 + yy in
                                if py >= r.y && py < r.y + r.height then
                                  draw_text grid ~x:xx ~y:py ~style:seg.style
                                    glyph
                              done))
                  b.segments)
          data
  in

  let charset = layout.theme.charset in

  let draw_rule_v ~style ~pattern x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      let glyph =
        match pattern with
        | `Solid -> charset.grid_v_solid
        | `Dashed -> charset.grid_v_dashed
        | `Dotted -> charset.grid_v_dotted
      in
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x:px ~y ~style glyph
      done
  in

  let draw_rule_h ~y_axis ~style ~pattern y =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let py = y_to_px_cell' y in
    if py >= r.y && py < r.y + r.height then
      let glyph =
        match pattern with
        | `Solid -> charset.grid_h_solid
        | `Dashed -> charset.grid_h_dashed
        | `Dotted -> charset.grid_h_dotted
      in
      for x = r.x to r.x + r.width - 1 do
        draw_text grid ~x ~y:py ~style glyph
      done
  in

  let draw_candles ~y_axis ~bullish ~bearish ~width ~body data =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let body_char, body_filled_char =
      match width with `One -> ("┃", "┃") | `Two -> ("▐▌", "██")
    in
    let wick_char = match width with `One -> "│" | `Two -> " │" in
    Array.iter
      (fun (o : Mark.ohlc) ->
        let cx = x_to_px_cell o.time in
        let st = if o.close >= o.open_ then bullish else bearish in
        let is_bullish = o.close >= o.open_ in
        let y_body_top = y_to_px_cell' (max o.open_ o.close) in
        let y_body_bot = y_to_px_cell' (min o.open_ o.close) in
        let y_wick_top = y_to_px_cell' o.high in
        let y_wick_bot = y_to_px_cell' o.low in
        let body_str =
          match body with
          | `Filled -> body_filled_char
          | `Hollow -> if is_bullish then body_char else body_filled_char
        in
        if cx >= r.x && cx < r.x + r.width then (
          for yy = min y_wick_top y_body_top to max y_wick_top y_body_top do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st wick_char
          done;
          for yy = min y_body_top y_body_bot to max y_body_top y_body_bot do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st body_str
          done;
          for yy = min y_body_bot y_wick_bot to max y_body_bot y_wick_bot do
            if yy >= r.y && yy < r.y + r.height then
              draw_text grid ~x:cx ~y:yy ~style:st wick_char
          done))
      data
  in

  let get_circle_points ~cx ~cy ~radius =
    if radius <= 0 then []
    else
      let points = ref [] in
      let t1 = ref (radius / 16) in
      let t2 = ref 0 in
      let x = ref radius in
      let y = ref 0 in
      let add dx dy = points := (cx + dx, cy + dy) :: !points in
      while !x >= !y do
        add !x !y;
        add !x (- !y);
        add (- !x) !y;
        add (- !x) (- !y);
        add !y !x;
        add !y (- !x);
        add (- !y) !x;
        add (- !y) (- !x);
        incr y;
        t1 := !t1 + !y;
        t2 := !t1 - !x;
        if !t2 >= 0 then (
          t1 := !t2;
          decr x)
      done;
      !points
  in

  let draw_circle ~y_axis ~style ~kind ~cxa ~cya ~ra =
    let y_to_px_cell' = select_y_to_px_cell y_axis in
    let y_view = select_y_view y_axis in
    let n = Array.length cxa in
    match kind with
    | `Line ->
        for i = 0 to n - 1 do
          let cxv = cxa.(i) and cyv = cya.(i) and rv = ra.(i) in
          let cx_i = int_of_float (Float.round cxv) in
          let cy_i = int_of_float (Float.round cyv) in
          let radius = int_of_float (Float.round (max 0. rv)) in
          let pts = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
          List.iter
            (fun (xd, yd) ->
              let px = x_to_px_cell (float xd) in
              let py = y_to_px_cell' (float yd) in
              if Layout.rect_contains r ~x:px ~y:py then
                draw_text grid ~x:px ~y:py ~style "█")
            pts
        done
    | `Braille ->
        let gx = r.width * 2 in
        let gy = r.height * 4 in
        let xmin = layout.x_view.min and xmax = layout.x_view.max in
        let ymin = y_view.min and ymax = y_view.max in
        let dx = xmax -. xmin and dy = ymax -. ymin in
        braille_clear ();
        for i = 0 to n - 1 do
          let cxv = cxa.(i) and cyv = cya.(i) and rv = ra.(i) in
          let cx_i = int_of_float (Float.round cxv) in
          let cy_i = int_of_float (Float.round cyv) in
          let radius = int_of_float (Float.round (max 0. rv)) in
          let pts = get_circle_points ~cx:cx_i ~cy:cy_i ~radius in
          List.iter
            (fun (xd, yd) ->
              let x' = float xd and y' = float yd in
              let sx =
                if dx <= 0. then 0. else (x' -. xmin) *. float (gx - 1) /. dx
              in
              let sy =
                if dy <= 0. then 0. else (y' -. ymin) *. float (gy - 1) /. dy
              in
              let xg = int_of_float (Float.round sx) in
              let yg = int_of_float (Float.round sy) in
              let x_sub = (r.x * 2) + xg in
              let y_sub = (r.y * 4) + (gy - 1 - yg) in
              braille_set_dot x_sub y_sub)
            pts
        done;
        render_braille style
  in

  let draw_shade_x style x0 x1 =
    let px0 = x_to_px_cell x0 and px1 = x_to_px_cell x1 in
    let a = min px0 px1 and b = max px0 px1 in
    for x = a to b do
      if x >= r.x && x < r.x + r.width then
        for y = r.y to r.y + r.height - 1 do
          draw_text grid ~x ~y ~style " "
        done
    done
  in

  let draw_column_bg style x =
    let px = x_to_px_cell x in
    if px >= r.x && px < r.x + r.width then
      for y = r.y to r.y + r.height - 1 do
        draw_text grid ~x:px ~y ~style " "
      done
  in

  let draw_histogram ~style ~bin_edges ~bin_values =
    let num_bins = Array.length bin_values in
    if num_bins > 0 && Array.length bin_edges > num_bins then
      for i = 0 to num_bins - 1 do
        let x0 = bin_edges.(i) in
        let x1 = bin_edges.(i + 1) in
        let y_val = bin_values.(i) in
        let px0 = x_to_px_cell x0 in
        let px1 = x_to_px_cell x1 in
        let py_base = y_to_px_cell 0. in
        let py_top = y_to_px_cell y_val in
        let py_min = min py_base py_top in
        let py_max = max py_base py_top in
        for px = max r.x px0 to min (r.x + r.width - 1) (px1 - 1) do
          for py = py_min to py_max do
            if Layout.rect_contains r ~x:px ~y:py then
              draw_text grid ~x:px ~y:py ~style "█"
          done
        done
      done
  in

  let safe_range = Layout.safe_domain_range in

  let rec draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg ~render
      ~xa ~ya ~va =
    let color_scale =
      if Array.length color_scale > 0 then color_scale
      else if Array.length layout.theme.palette > 0 then layout.theme.palette
      else [| Color.default |]
    in
    let n = Array.length xa in
    let vmin, vmax =
      match value_range with
      | Some (a, b) -> (a, b)
      | None ->
          if not auto_value_range then (0., 1.)
          else if n = 0 then (0., 1.)
          else
            let mn = ref va.(0) in
            let mx = ref !mn in
            for i = 1 to n - 1 do
              let v = va.(i) in
              if v < !mn then mn := v;
              if v > !mx then mx := v
            done;
            (!mn, !mx)
    in
    let vmin, vmax = safe_range vmin vmax in
    let color_of = heatmap_color_fun ~color_scale ~vmin ~vmax in
    let ncolors = Array.length color_scale in
    let bg_styles = Array.map (fun c -> Style.make ~bg:c ()) color_scale in
    let fg_styles = Array.map (fun c -> Style.make ~fg:c ()) color_scale in
    let color_idx v = heatmap_color_idx ~len:ncolors ~vmin ~vmax v in

    let draw_cell px py v =
      if Layout.rect_contains r ~x:px ~y:py then
        draw_text grid ~x:px ~y:py ~style:bg_styles.(color_idx v) " "
    in

    match render with
    | Mark.Cells_bg ->
        let size = r.width * r.height in
        let idx px py = px - r.x + ((py - r.y) * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        for i = 0 to n - 1 do
          let px = x_to_px_cell xa.(i) in
          let py = y_to_px_cell ya.(i) in
          let v = va.(i) in
          if Layout.rect_contains r ~x:px ~y:py then
            let ii = idx px py in
            match agg with
            | `Last ->
                values_arr.(ii) <- v;
                counts_arr.(ii) <- 1
            | `Max ->
                if counts_arr.(ii) = 0 then (
                  values_arr.(ii) <- v;
                  counts_arr.(ii) <- 1)
                else values_arr.(ii) <- Float.max values_arr.(ii) v
            | `Avg ->
                values_arr.(ii) <- values_arr.(ii) +. v;
                counts_arr.(ii) <- counts_arr.(ii) + 1
        done;
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let ii = idx px py in
            let cnt = counts_arr.(ii) in
            if cnt > 0 then
              let v =
                match agg with
                | `Avg -> values_arr.(ii) /. float cnt
                | _ -> values_arr.(ii)
              in
              draw_cell px py v
          done
        done
    | Mark.Cells_fg ->
        let draw_cell_fg px py v =
          if Layout.rect_contains r ~x:px ~y:py then
            draw_text grid ~x:px ~y:py
              ~style:fg_styles.(color_idx v)
              charset.bar_fill
        in
        let size = r.width * r.height in
        let idx px py = px - r.x + ((py - r.y) * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        for i = 0 to n - 1 do
          let px = x_to_px_cell xa.(i) in
          let py = y_to_px_cell ya.(i) in
          let v = va.(i) in
          if Layout.rect_contains r ~x:px ~y:py then
            let ii = idx px py in
            match agg with
            | `Last ->
                values_arr.(ii) <- v;
                counts_arr.(ii) <- 1
            | `Max ->
                if counts_arr.(ii) = 0 then (
                  values_arr.(ii) <- v;
                  counts_arr.(ii) <- 1)
                else values_arr.(ii) <- Float.max values_arr.(ii) v
            | `Avg ->
                values_arr.(ii) <- values_arr.(ii) +. v;
                counts_arr.(ii) <- counts_arr.(ii) + 1
        done;
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let ii = idx px py in
            let cnt = counts_arr.(ii) in
            if cnt > 0 then
              let v =
                match agg with
                | `Avg -> values_arr.(ii) /. float cnt
                | _ -> values_arr.(ii)
              in
              draw_cell_fg px py v
          done
        done
    | Mark.Halfblock_fg_bg ->
        let half_height = r.height * 2 in
        let size = r.width * half_height in
        let idx px py_half = px - r.x + (py_half * r.width) in
        let values_arr = Array.make size 0. in
        let counts_arr = Array.make size 0 in
        for i = 0 to n - 1 do
          let px = x_to_px_cell xa.(i) in
          let y_data = ya.(i) in
          let py_float =
            let minv = layout.y_view.min and maxv = layout.y_view.max in
            let extent = half_height in
            if Float.abs (maxv -. minv) < 1e-12 then float (extent / 2)
            else
              let t = (y_data -. minv) /. (maxv -. minv) in
              float (extent - 1) *. (1. -. t)
          in
          let py_half = int_of_float (Float.round py_float) in
          let py_half = max 0 (min (half_height - 1) py_half) in
          let v = va.(i) in
          if px >= r.x && px < r.x + r.width then
            let ii = idx px py_half in
            match agg with
            | `Last ->
                values_arr.(ii) <- v;
                counts_arr.(ii) <- 1
            | `Max ->
                if counts_arr.(ii) = 0 then (
                  values_arr.(ii) <- v;
                  counts_arr.(ii) <- 1)
                else values_arr.(ii) <- Float.max values_arr.(ii) v
            | `Avg ->
                values_arr.(ii) <- values_arr.(ii) +. v;
                counts_arr.(ii) <- counts_arr.(ii) + 1
        done;
        let get_value px py_half =
          let ii = idx px py_half in
          let cnt = counts_arr.(ii) in
          if cnt = 0 then None
          else
            Some
              (match agg with
              | `Avg -> values_arr.(ii) /. float cnt
              | _ -> values_arr.(ii))
        in
        for py = r.y to r.y + r.height - 1 do
          for px = r.x to r.x + r.width - 1 do
            let py_rel = py - r.y in
            let py_half_top = py_rel * 2 in
            let py_half_bot = (py_rel * 2) + 1 in
            let v_top = get_value px py_half_top in
            let v_bot = get_value px py_half_bot in
            match (v_top, v_bot) with
            | None, None -> ()
            | Some vt, None ->
                draw_text grid ~x:px ~y:py ~style:fg_styles.(color_idx vt) "▀"
            | None, Some vb ->
                draw_text grid ~x:px ~y:py ~style:fg_styles.(color_idx vb) "▄"
            | Some vt, Some vb ->
                let st = Style.make ~fg:(color_of vt) ~bg:(color_of vb) () in
                draw_text grid ~x:px ~y:py ~style:st "▀"
          done
        done
    | Mark.Dense_bilinear ->
        let xs_tbl = Hashtbl.create 64 and ys_tbl = Hashtbl.create 64 in
        for i = 0 to n - 1 do
          Hashtbl.replace xs_tbl xa.(i) ();
          Hashtbl.replace ys_tbl ya.(i) ()
        done;
        let xs =
          Hashtbl.to_seq_keys xs_tbl |> List.of_seq |> List.sort Float.compare
        in
        let ys =
          Hashtbl.to_seq_keys ys_tbl |> List.of_seq |> List.sort Float.compare
        in
        let nx = List.length xs and ny = List.length ys in
        if nx <= 1 || ny <= 1 then
          draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg
            ~render:Mark.Cells_bg ~xa ~ya ~va
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          let grid_size = nx * ny in
          let idx ix iy = ix + (iy * nx) in
          let values_arr = Array.make grid_size 0. in
          let counts_arr = Array.make grid_size 0 in
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
            else if len = 2 then (0, 1, arr.(0), arr.(1))
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              let i = bsearch 0 (len - 2) in
              (i, i + 1, arr.(i), arr.(i + 1))
          in
          let find_index_floor arr v =
            let len = Array.length arr in
            if len = 1 then 0
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              bsearch 0 (len - 1)
          in
          for k = 0 to n - 1 do
            let xi = find_index_floor xs xa.(k) in
            let yi = find_index_floor ys ya.(k) in
            let ii = idx xi yi in
            let v = va.(k) in
            match agg with
            | `Last ->
                values_arr.(ii) <- v;
                counts_arr.(ii) <- 1
            | `Max ->
                if counts_arr.(ii) = 0 then (
                  values_arr.(ii) <- v;
                  counts_arr.(ii) <- 1)
                else values_arr.(ii) <- Float.max values_arr.(ii) v
            | `Avg ->
                values_arr.(ii) <- values_arr.(ii) +. v;
                counts_arr.(ii) <- counts_arr.(ii) + 1
          done;
          let lookup ix iy =
            let ii = idx ix iy in
            match agg with
            | `Avg ->
                let cnt = counts_arr.(ii) in
                if cnt <= 0 then 0. else values_arr.(ii) /. float cnt
            | `Last | `Max -> values_arr.(ii)
          in
          let sample xq yq =
            let ix0, ix1, x0, x1 = find_interval xs xq in
            let iy0, iy1, y0, y1 = find_interval ys yq in
            let tx =
              if Float.equal x0 x1 then 0.
              else (xq -. x0) /. Float.max 1e-12 (x1 -. x0)
            in
            let ty =
              if Float.equal y0 y1 then 0.
              else (yq -. y0) /. Float.max 1e-12 (y1 -. y0)
            in
            let v00 = lookup ix0 iy0 in
            let v10 = lookup ix1 iy0 in
            let v01 = lookup ix0 iy1 in
            let v11 = lookup ix1 iy1 in
            let v0 = v00 +. ((v10 -. v00) *. tx) in
            let v1 = v01 +. ((v11 -. v01) *. tx) in
            v0 +. ((v1 -. v0) *. ty)
          in
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              match Layout.data_of_px layout ~px ~py with
              | None -> ()
              | Some (dx, dy) ->
                  let v = sample dx dy in
                  draw_cell px py v
            done
          done
    | Mark.Shaded ->
        let shade_chars = charset.shade_levels in
        let num_levels = Array.length shade_chars in
        let shade_idx v =
          let t = (v -. vmin) /. Float.max 1e-12 (vmax -. vmin) in
          let t = Layout.clamp01 t in
          max 0
            (min (num_levels - 1) (int_of_float (t *. float (num_levels - 1))))
        in
        let draw_shaded_cell px py v =
          if Layout.rect_contains r ~x:px ~y:py then
            draw_text grid ~x:px ~y:py
              ~style:fg_styles.(color_idx v)
              shade_chars.(shade_idx v)
        in
        let xs_tbl = Hashtbl.create 64 and ys_tbl = Hashtbl.create 64 in
        for i = 0 to n - 1 do
          Hashtbl.replace xs_tbl xa.(i) ();
          Hashtbl.replace ys_tbl ya.(i) ()
        done;
        let xs =
          Hashtbl.to_seq_keys xs_tbl |> List.of_seq |> List.sort Float.compare
        in
        let ys =
          Hashtbl.to_seq_keys ys_tbl |> List.of_seq |> List.sort Float.compare
        in
        let nx = List.length xs and ny = List.length ys in
        if nx <= 1 || ny <= 1 then (
          let size = r.width * r.height in
          let idx px py = px - r.x + ((py - r.y) * r.width) in
          let values_arr = Array.make size 0. in
          let has_value = Array.make size false in
          for i = 0 to n - 1 do
            let px = x_to_px_cell xa.(i) in
            let py = y_to_px_cell ya.(i) in
            let v = va.(i) in
            if Layout.rect_contains r ~x:px ~y:py then (
              let ii = idx px py in
              values_arr.(ii) <- v;
              has_value.(ii) <- true)
          done;
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              let ii = idx px py in
              if has_value.(ii) then draw_shaded_cell px py values_arr.(ii)
            done
          done)
        else
          let xs = Array.of_list xs and ys = Array.of_list ys in
          let grid_size = nx * ny in
          let idx ix iy = ix + (iy * nx) in
          let values_arr = Array.make grid_size 0. in
          let counts_arr = Array.make grid_size 0 in
          let find_interval arr v =
            let len = Array.length arr in
            if len = 1 then (0, 0, arr.(0), arr.(0))
            else if len = 2 then (0, 1, arr.(0), arr.(1))
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              let i = bsearch 0 (len - 2) in
              (i, i + 1, arr.(i), arr.(i + 1))
          in
          let find_index_floor arr v =
            let len = Array.length arr in
            if len = 1 then 0
            else
              let rec bsearch lo hi =
                if lo >= hi then lo
                else
                  let mid = lo + ((hi - lo + 1) / 2) in
                  if Float.compare arr.(mid) v <= 0 then bsearch mid hi
                  else bsearch lo (mid - 1)
              in
              bsearch 0 (len - 1)
          in
          for k = 0 to n - 1 do
            let xi = find_index_floor xs xa.(k) in
            let yi = find_index_floor ys ya.(k) in
            let ii = idx xi yi in
            let v = va.(k) in
            match agg with
            | `Last ->
                values_arr.(ii) <- v;
                counts_arr.(ii) <- 1
            | `Max ->
                if counts_arr.(ii) = 0 then (
                  values_arr.(ii) <- v;
                  counts_arr.(ii) <- 1)
                else values_arr.(ii) <- Float.max values_arr.(ii) v
            | `Avg ->
                values_arr.(ii) <- values_arr.(ii) +. v;
                counts_arr.(ii) <- counts_arr.(ii) + 1
          done;
          let lookup ix iy =
            let ii = idx ix iy in
            match agg with
            | `Avg ->
                let cnt = counts_arr.(ii) in
                if cnt <= 0 then 0. else values_arr.(ii) /. float cnt
            | `Last | `Max -> values_arr.(ii)
          in
          let sample xq yq =
            let ix0, ix1, x0, x1 = find_interval xs xq in
            let iy0, iy1, y0, y1 = find_interval ys yq in
            let tx =
              if Float.equal x0 x1 then 0.
              else (xq -. x0) /. Float.max 1e-12 (x1 -. x0)
            in
            let ty =
              if Float.equal y0 y1 then 0.
              else (yq -. y0) /. Float.max 1e-12 (y1 -. y0)
            in
            let v00 = lookup ix0 iy0 in
            let v10 = lookup ix1 iy0 in
            let v01 = lookup ix0 iy1 in
            let v11 = lookup ix1 iy1 in
            let v0 = v00 +. ((v10 -. v00) *. tx) in
            let v1 = v01 +. ((v11 -. v01) *. tx) in
            v0 +. ((v1 -. v0) *. ty)
          in
          for py = r.y to r.y + r.height - 1 do
            for px = r.x to r.x + r.width - 1 do
              match Layout.data_of_px layout ~px ~py with
              | None -> ()
              | Some (dx, dy) ->
                  let v = sample dx dy in
                  draw_shaded_cell px py v
            done
          done
  in

  (* Dispatch: iterate marks in order (layering) *)
  List.iter
    (fun (m : Mark.t) ->
      let style = Option.value m.style ~default:Style.default in
      match m.kind with
      | Mark.Shade { x0; x1 } -> draw_shade_x style x0 x1
      | Mark.Column_bg { x } -> draw_column_bg style x
      | Mark.Line { x = xa; y = ya; resolution; pattern; glyph } ->
          let kind =
            match resolution with
            | `Cell -> ( match glyph with Some g -> `Points g | None -> `Line)
            | `Wave -> `Wave
            | `Block2x2 -> `Block2x2
            | `Braille2x4 -> `Braille
          in
          draw_line_series ~y_axis:m.y_axis ~style ~pattern ~kind ~xa ~ya
      | Mark.Scatter { x = xa; y = ya; mode; glyph } ->
          let glyph_str = Option.value glyph ~default:charset.point_default in
          draw_scatter ~y_axis:m.y_axis ~style ~glyph:glyph_str ~kind:mode ~xa
            ~ya
      | Mark.Bar { categories; values; direction = `Vertical; mode } ->
          draw_bars_y ~style ~mode ~categories ~values
      | Mark.Bar { categories; values; direction = `Horizontal; mode } ->
          draw_bars_x ~style ~mode ~categories ~values
      | Mark.Stacked_bar { data; direction = `Vertical; mode; gap; size } ->
          draw_stacked_y ~gap ~bar_width:size ~mode data
      | Mark.Stacked_bar { data; direction = `Horizontal; mode; gap; size } ->
          draw_stacked_x ~gap ~bar_height:size ~mode data
      | Mark.Rule { value; direction = `Vertical; pattern } ->
          draw_rule_v ~style ~pattern value
      | Mark.Rule { value; direction = `Horizontal; pattern } ->
          draw_rule_h ~y_axis:m.y_axis ~style ~pattern value
      | Mark.Heatmap
          {
            x = xa;
            y = ya;
            values = va;
            color_scale;
            value_range;
            auto_value_range;
            agg;
            mode;
          } ->
          draw_heatmap ~color_scale ~value_range ~auto_value_range ~agg
            ~render:mode ~xa ~ya ~va
      | Mark.Candle { data; bullish; bearish; width; body } ->
          draw_candles ~y_axis:m.y_axis ~bullish ~bearish ~width ~body data
      | Mark.Circle { cx = cxa; cy = cya; r = ra; resolution } ->
          let kind =
            match resolution with
            | `Cell | `Wave | `Block2x2 -> `Line
            | `Braille2x4 -> `Braille
          in
          draw_circle ~y_axis:m.y_axis ~style ~kind ~cxa ~cya ~ra
      | Mark.Area { x = xa; y = ya; baseline; resolution } ->
          draw_area ~y_axis:m.y_axis ~style ~baseline ~resolution ~xa ~ya
      | Mark.Fill_between { x = xa; y_low = yla; y_high = yha; resolution } ->
          draw_fill_between ~y_axis:m.y_axis ~style ~resolution ~xa ~yla ~yha
      | Mark.Histogram { bin_edges; bin_values } ->
          draw_histogram ~style ~bin_edges ~bin_values)
    layout.marks
