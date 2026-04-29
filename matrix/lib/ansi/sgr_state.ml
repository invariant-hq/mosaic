type color_depth = [ `Ansi16 | `Ansi256 | `Truecolor ]

type t = {
  mutable fg_color : int;
  mutable bg_color : int;
  mutable attrs : int;
  mutable link : string; (* "" = no link *)
  mutable link_open : bool;
  mutable color_depth : color_depth;
}

let create () =
  {
    fg_color = -1;
    bg_color = -1;
    attrs = -1;
    link = "";
    link_open = false;
    color_depth = `Truecolor;
  }

let set_color_depth t depth =
  if t.color_depth <> depth then (
    t.color_depth <- depth;
    t.fg_color <- -1;
    t.bg_color <- -1;
    t.attrs <- -1)

let reset t =
  t.fg_color <- -1;
  t.bg_color <- -1;
  t.attrs <- -1;
  t.link <- "";
  t.link_open <- false

let update_link t w link =
  if not (String.equal link t.link) then (
    if t.link_open then Escape.hyperlink_close w;
    if link <> "" then (
      Escape.hyperlink_open w link;
      t.link <- link;
      t.link_open <- true)
    else (
      t.link <- "";
      t.link_open <- false))

let ansi16_rgb =
  [|
    0x000000;
    0x800000;
    0x008000;
    0x808000;
    0x000080;
    0x800080;
    0x008080;
    0xc0c0c0;
    0x808080;
    0xff0000;
    0x00ff00;
    0xffff00;
    0x0000ff;
    0xff00ff;
    0x00ffff;
    0xffffff;
  |]

let cube_level = [| 0; 95; 135; 175; 215; 255 |]

let[@inline] rgb24 r g b = (r lsl 16) lor (g lsl 8) lor b

let[@inline] palette_rgb24 idx =
  if idx < 16 then Array.unsafe_get ansi16_rgb idx
  else if idx < 232 then
    let n = idx - 16 in
    rgb24
      (Array.unsafe_get cube_level (n / 36))
      (Array.unsafe_get cube_level (n / 6 mod 6))
      (Array.unsafe_get cube_level (n mod 6))
  else
    let gray = 8 + ((idx - 232) * 10) in
    rgb24 gray gray gray

let[@inline] color_distance r g b candidate =
  let dr = r - ((candidate lsr 16) land 0xFF) in
  let dg = g - ((candidate lsr 8) land 0xFF) in
  let db = b - (candidate land 0xFF) in
  (dr * dr) + (dg * dg) + (db * db)

let nearest_palette_index limit color =
  let r = Color.Packed.red color in
  let g = Color.Packed.green color in
  let b = Color.Packed.blue color in
  let rec loop i best best_dist =
    if i >= limit then best
    else
      let dist = color_distance r g b (palette_rgb24 i) in
      if dist < best_dist then loop (i + 1) i dist
      else loop (i + 1) best best_dist
  in
  loop 0 0 max_int

let emit_indexed_sgr w ~bg idx =
  Escape.sgr_sep w;
  Escape.sgr_code w (if bg then 48 else 38);
  Escape.sgr_sep w;
  Escape.sgr_code w 5;
  Escape.sgr_sep w;
  Escape.sgr_code w idx

let emit_ansi16_sgr w ~bg idx =
  Escape.sgr_sep w;
  if idx < 8 then Escape.sgr_code w ((if bg then 40 else 30) + idx)
  else Escape.sgr_code w ((if bg then 100 else 90) + idx - 8)

let emit_rgb_sgr w ~bg color =
  Escape.sgr_sep w;
  Escape.sgr_code w (if bg then 48 else 38);
  Escape.sgr_sep w;
  Escape.sgr_code w 2;
  Escape.sgr_sep w;
  Escape.sgr_code w (Color.Packed.red color);
  Escape.sgr_sep w;
  Escape.sgr_code w (Color.Packed.green color);
  Escape.sgr_sep w;
  Escape.sgr_code w (Color.Packed.blue color)

let emit_color_sgr t w ~bg color =
  match Color.Packed.intent color with
  | Color.Default -> ()
  | Color.Indexed idx -> (
      match t.color_depth with
      | `Truecolor | `Ansi256 -> emit_indexed_sgr w ~bg idx
      | `Ansi16 -> emit_ansi16_sgr w ~bg (nearest_palette_index 16 color))
  | Color.Rgb -> (
      if Color.Packed.alpha color = 0 then ()
      else
        match t.color_depth with
        | `Truecolor -> emit_rgb_sgr w ~bg color
        | `Ansi256 -> emit_indexed_sgr w ~bg (nearest_palette_index 256 color)
        | `Ansi16 -> emit_ansi16_sgr w ~bg (nearest_palette_index 16 color))

let emit_attrs w attrs =
  if attrs <> 0 then (
    if attrs land 0x001 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 1);
    if attrs land 0x002 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 2);
    if attrs land 0x004 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 3);
    if attrs land 0x008 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 4);
    if attrs land 0x010 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 5);
    if attrs land 0x020 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 7);
    if attrs land 0x040 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 8);
    if attrs land 0x080 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 9);
    if attrs land 0x100 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 21);
    if attrs land 0x200 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 53);
    if attrs land 0x400 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 51);
    if attrs land 0x800 <> 0 then (
      Escape.sgr_sep w;
      Escape.sgr_code w 52))

let update t w ~fg ~bg ~attrs ~link =
  update_link t w link;
  let fg = Color.Packed.encode fg in
  let bg = Color.Packed.encode bg in
  if fg <> t.fg_color || bg <> t.bg_color || attrs <> t.attrs then (
    Escape.sgr_open w;
    Escape.sgr_code w 0;
    emit_color_sgr t w ~bg:false fg;
    emit_color_sgr t w ~bg:true bg;
    emit_attrs w attrs;
    Escape.sgr_close w;
    t.fg_color <- fg;
    t.bg_color <- bg;
    t.attrs <- attrs)

let close_link t w =
  if t.link_open then (
    Escape.hyperlink_close w;
    t.link <- "";
    t.link_open <- false)
