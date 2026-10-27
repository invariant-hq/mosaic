type intent = Default | Indexed of int | Rgb
type t = int

let clamp_byte v = max 0 (min 255 v)
let clamp_channel_f v = max 0. (min 1. v)
let float_of_byte v = float_of_int v /. 255.
let byte_of_float v = int_of_float (Float.round (clamp_channel_f v *. 255.))

module Packed = struct
  type color = t

  let () = assert (Sys.int_size >= 62)
  let alpha_shift = 0
  let blue_shift = 8
  let green_shift = 16
  let red_shift = 24
  let slot_shift = 32
  let intent_shift = 40
  let intent_indexed = 1
  let intent_default = 2
  let intent_mask = 0x3
  let full_mask = (1 lsl 42) - 1

  let make ~intent ~slot ~r ~g ~b ~a =
    (clamp_byte a lsl alpha_shift)
    lor (clamp_byte b lsl blue_shift)
    lor (clamp_byte g lsl green_shift)
    lor (clamp_byte r lsl red_shift)
    lor (clamp_byte slot lsl slot_shift)
    lor ((intent land intent_mask) lsl intent_shift)

  let[@inline] red c = (c lsr red_shift) land 0xFF
  let[@inline] green c = (c lsr green_shift) land 0xFF
  let[@inline] blue c = (c lsr blue_shift) land 0xFF
  let[@inline] alpha c = (c lsr alpha_shift) land 0xFF
  let[@inline] slot c = (c lsr slot_shift) land 0xFF
  let[@inline] intent_tag c = (c lsr intent_shift) land intent_mask
  let[@inline] rgba c = c land 0xFFFFFFFF
  let default_bg = make ~intent:intent_default ~slot:0 ~r:0 ~g:0 ~b:0 ~a:255

  let default_fg =
    make ~intent:intent_default ~slot:0 ~r:255 ~g:255 ~b:255 ~a:255

  let decode bits =
    let bits = bits land full_mask in
    match intent_tag bits with 0 | 1 | 2 -> bits | _ -> default_bg

  let[@inline] encode color = color
  let[@inline] red_f color = float_of_byte (red color)
  let[@inline] green_f color = float_of_byte (green color)
  let[@inline] blue_f color = float_of_byte (blue color)
  let[@inline] alpha_f color = float_of_byte (alpha color)

  let[@inline] of_rgba_f r g b a =
    let r = byte_of_float r in
    let g = byte_of_float g in
    let b = byte_of_float b in
    let a = byte_of_float a in
    a lor (b lsl blue_shift) lor (g lsl green_shift) lor (r lsl red_shift)

  let[@inline] with_alpha color a = color land lnot 255 lor clamp_byte a

  let[@inline] blend_channel src_weight dst_weight coverage sc dc =
    ((sc * src_weight) + (dc * dst_weight) + (coverage / 2)) / coverage

  let blend ~src ~dst =
    let sa = alpha src in
    if sa = 0 then dst
    else if sa = 255 then src
    else
      let da = alpha dst in
      let inv = 255 - sa in
      let src_weight = sa * 255 in
      let dst_weight = da * inv in
      let coverage = src_weight + dst_weight in
      let a = (coverage + 127) / 255 in
      let r =
        blend_channel src_weight dst_weight coverage (red src) (red dst)
      in
      let g =
        blend_channel src_weight dst_weight coverage (green src) (green dst)
      in
      let b =
        blend_channel src_weight dst_weight coverage (blue src) (blue dst)
      in
      a lor (b lsl blue_shift) lor (g lsl green_shift) lor (r lsl red_shift)

  let intent color =
    match intent_tag color with
    | 1 -> Indexed (slot color)
    | 2 -> Default
    | _ -> Rgb

  let indexed_slot color = slot color
end

let default_fg = Packed.default_fg
let default_bg = Packed.default_bg
let transparent = 0

let[@inline] make_rgb_unchecked r g b =
  255 lor (b lsl Packed.blue_shift) lor (g lsl Packed.green_shift)
  lor (r lsl Packed.red_shift)

let[@inline] make_rgba_unchecked r g b a =
  a lor (b lsl Packed.blue_shift) lor (g lsl Packed.green_shift)
  lor (r lsl Packed.red_shift)

let[@inline] of_rgb r g b =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  make_rgb_unchecked r g b

let[@inline] of_rgba r g b a =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  let a = clamp_byte a in
  make_rgba_unchecked r g b a

let[@inline] make_indexed_unchecked slot r g b =
  255 lor (b lsl Packed.blue_shift) lor (g lsl Packed.green_shift)
  lor (r lsl Packed.red_shift)
  lor (slot lsl Packed.slot_shift)
  lor (Packed.intent_indexed lsl Packed.intent_shift)

let[@inline] cube_level = function
  | 0 -> 0
  | 1 -> 95
  | 2 -> 135
  | 3 -> 175
  | 4 -> 215
  | _ -> 255

(* xterm's palette (256colres.pl). Its gray ramp is "intentionally leaving
   out black and white", and xterm takes entries 0 to 15 from resources,
   "usually more-or-less a reproduction of the standard ANSI colors, but
   possibly more pleasing shades"; these are HTML 4.01's 16 colors. *)
let make_indexed idx =
  if idx < 16 then
    match idx with
    | 0 -> make_indexed_unchecked 0 0x00 0x00 0x00
    | 1 -> make_indexed_unchecked 1 0x80 0x00 0x00
    | 2 -> make_indexed_unchecked 2 0x00 0x80 0x00
    | 3 -> make_indexed_unchecked 3 0x80 0x80 0x00
    | 4 -> make_indexed_unchecked 4 0x00 0x00 0x80
    | 5 -> make_indexed_unchecked 5 0x80 0x00 0x80
    | 6 -> make_indexed_unchecked 6 0x00 0x80 0x80
    | 7 -> make_indexed_unchecked 7 0xc0 0xc0 0xc0
    | 8 -> make_indexed_unchecked 8 0x80 0x80 0x80
    | 9 -> make_indexed_unchecked 9 0xff 0x00 0x00
    | 10 -> make_indexed_unchecked 10 0x00 0xff 0x00
    | 11 -> make_indexed_unchecked 11 0xff 0xff 0x00
    | 12 -> make_indexed_unchecked 12 0x00 0x00 0xff
    | 13 -> make_indexed_unchecked 13 0xff 0x00 0xff
    | 14 -> make_indexed_unchecked 14 0x00 0xff 0xff
    | _ -> make_indexed_unchecked 15 0xff 0xff 0xff
  else if idx < 232 then
    let n = idx - 16 in
    make_indexed_unchecked idx
      (cube_level (n / 36))
      (cube_level (n / 6 mod 6))
      (cube_level (n mod 6))
  else
    let gray = 8 + ((idx - 232) * 10) in
    make_indexed_unchecked idx gray gray gray

let indexed idx =
  let idx = clamp_byte idx in
  make_indexed idx

(* Depths *)

type depth = [ `Truecolor | `Ansi256 | `Ansi16 | `Ansi8 | `Mono ]

let[@inline] distance r g b c =
  let dr = r - Packed.red c and dg = g - Packed.green c in
  let db = b - Packed.blue c in
  (dr * dr) + (dg * dg) + (db * db)

(* The palette entry of [first..last] nearest to [(r, g, b)], a tie going to
   the higher index, as xterm's search from the top keeps the first best
   (xtermClosestColor in xterm's misc.c). A perceptual distance, such as
   CIELAB's, would match some colors differently; this one gives what xterm
   shows. *)
let nearest_entry ~first ~last r g b =
  let best = ref last and best_d = ref (distance r g b (make_indexed last)) in
  for i = last - 1 downto first do
    let d = distance r g b (make_indexed i) in
    if d < !best_d then (
      best := i;
      best_d := d)
  done;
  !best

(* The cube step whose level (0, 95, 135, 175, 215, 255) is nearest to [v]. The
   midpoints are 47.5, 115, 155, 195 and 235; at one, the higher step. *)
let[@inline] cube_step v =
  if v < 48 then 0 else if v < 115 then 1 else (v - 35) / 40

(* The entry of 16..255 nearest to [(r, g, b)]. The distance is a sum over the
   channels, so the nearest cube color is the nearest level in each; the
   nearest gray, at 8 + 10k, is the one nearest to the channels' mean, of the
   two around it. On a tie the higher index wins, so the gray. *)
let nearest_256 r g b =
  let cube = 16 + (36 * cube_step r) + (6 * cube_step g) + cube_step b in
  let k = max 0 (min 23 ((r + g + b - 24) / 30)) in
  let k =
    if
      k < 23
      && distance r g b (make_indexed (233 + k))
         <= distance r g b (make_indexed (232 + k))
    then k + 1
    else k
  in
  let gray = 232 + k in
  if distance r g b (make_indexed gray) <= distance r g b (make_indexed cube)
  then gray
  else cube

let to_depth depth c =
  let tag = Packed.intent_tag c in
  if tag = Packed.intent_default then c
  else
    match depth with
    | `Truecolor -> c
    | `Mono ->
        c
        land lnot (0xFF lsl Packed.slot_shift)
        land lnot (Packed.intent_mask lsl Packed.intent_shift)
        lor (Packed.intent_default lsl Packed.intent_shift)
    | (`Ansi256 | `Ansi16 | `Ansi8) as depth ->
        let entries =
          match depth with `Ansi256 -> 256 | `Ansi16 -> 16 | `Ansi8 -> 8
        in
        let shown = tag = Packed.intent_indexed && Packed.slot c < entries in
        if shown || Packed.alpha c = 0 then c
        else
          let r = Packed.red c and g = Packed.green c and b = Packed.blue c in
          let entry =
            if entries = 256 then nearest_256 r g b
            else nearest_entry ~first:0 ~last:(entries - 1) r g b
          in
          Packed.with_alpha (make_indexed entry) (Packed.alpha c)

let of_palette_index = indexed
let black = indexed 0
let red = indexed 1
let green = indexed 2
let yellow = indexed 3
let blue = indexed 4
let magenta = indexed 5
let cyan = indexed 6
let white = indexed 7
let bright_black = indexed 8
let bright_red = indexed 9
let bright_green = indexed 10
let bright_yellow = indexed 11
let bright_blue = indexed 12
let bright_magenta = indexed 13
let bright_cyan = indexed 14
let bright_white = indexed 15

let grayscale ~level =
  let level = max 0 (min 23 level) in
  indexed (232 + level)

let of_rgba_f r g b a = Packed.decode (Packed.of_rgba_f r g b a)
let intent = Packed.intent
let to_rgb color = (Packed.red color, Packed.green color, Packed.blue color)

let to_rgba color =
  (Packed.red color, Packed.green color, Packed.blue color, Packed.alpha color)

let to_rgba_f color =
  let r, g, b, a = to_rgba color in
  (float_of_byte r, float_of_byte g, float_of_byte b, float_of_byte a)

let alpha color = float_of_byte (Packed.alpha color)
let equal = Int.equal
let compare = Int.compare
let hash = Hashtbl.hash
let equal_rgba a b = Packed.rgba a = Packed.rgba b

let[@inline] with_rgba_f color f =
  f
    (float_of_byte (Packed.red color))
    (float_of_byte (Packed.green color))
    (float_of_byte (Packed.blue color))
    (float_of_byte (Packed.alpha color))

let of_hsl ~h ~s ~l ?a () =
  let h = Float.rem h 360. in
  let h = if h < 0. then h +. 360. else h in
  let s = clamp_channel_f s in
  let l = clamp_channel_f l in
  let a = Option.value a ~default:1.0 |> clamp_channel_f in
  let c = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
  let h' = h /. 60. in
  let x = c *. (1. -. abs_float (Float.rem h' 2. -. 1.)) in
  let m = l -. (c /. 2.) in
  let r', g', b' =
    if h < 60. then (c, x, 0.)
    else if h < 120. then (x, c, 0.)
    else if h < 180. then (0., c, x)
    else if h < 240. then (0., x, c)
    else if h < 300. then (x, 0., c)
    else (c, 0., x)
  in
  let r = byte_of_float (r' +. m) in
  let g = byte_of_float (g' +. m) in
  let b = byte_of_float (b' +. m) in
  let alpha = byte_of_float a in
  if alpha = 255 then of_rgb r g b else of_rgba r g b alpha

let to_hsl color =
  let rf, gf, bf, af = to_rgba_f color in
  let max_val = max rf (max gf bf) in
  let min_val = min rf (min gf bf) in
  let l = (max_val +. min_val) /. 2. in
  if max_val = min_val then (0., 0., l, af)
  else
    let d = max_val -. min_val in
    let s =
      if l > 0.5 then d /. (2. -. max_val -. min_val)
      else d /. (max_val +. min_val)
    in
    let h =
      if max_val = rf then ((gf -. bf) /. d) +. if gf < bf then 6. else 0.
      else if max_val = gf then ((bf -. rf) /. d) +. 2.
      else ((rf -. gf) /. d) +. 4.
    in
    let h = h *. 60. in
    (h, s, l, af)

let blend ~src ~dst = Packed.blend ~src ~dst

let emit_indexed_sgr ~bg push idx =
  push (if bg then 48 else 38);
  push 5;
  push idx

(* At 16 and 8 colors an entry is written with the codes of ECMA-48 and
   aixterm, which every such terminal reads: 30..37 and 90..97 for the
   foreground, 40..47 and 100..107 for the background. *)
let emit_sgr_codes ?(depth = `Truecolor) ~bg push color =
  let color = to_depth depth color in
  match intent color with
  | Default -> push (if bg then 49 else 39)
  | Indexed idx -> (
      match depth with
      | `Ansi16 | `Ansi8 ->
          if idx < 8 then push ((if bg then 40 else 30) + idx)
          else push ((if bg then 100 else 90) + idx - 8)
      | `Truecolor | `Ansi256 | `Mono -> emit_indexed_sgr ~bg push idx)
  | Rgb ->
      if Packed.alpha color = 0 then push (if bg then 49 else 39)
      else (
        push (if bg then 48 else 38);
        push 2;
        push (Packed.red color);
        push (Packed.green color);
        push (Packed.blue color))

let to_sgr_codes ?depth ~bg color =
  let acc = ref [] in
  emit_sgr_codes ?depth ~bg (fun code -> acc := code :: !acc) color;
  List.rev !acc

let invert color =
  let r, g, b = to_rgb color in
  of_rgb (255 - r) (255 - g) (255 - b)

(* WCAG 2's relative luminance of the snapshot: each channel linearised by
   the sRGB transfer function, then weighted. WCAG 2.0 put the linear
   segment's end at 0.03928 and WCAG 2.2 at 0.04045, as sRGB does; no 8-bit
   channel lies between them, so both give the same ratio. *)
let luminance c =
  let linear v =
    let v = float_of_byte v in
    if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4
  in
  (0.2126 *. linear (Packed.red c))
  +. (0.7152 *. linear (Packed.green c))
  +. (0.0722 *. linear (Packed.blue c))

let contrast a b =
  let la = luminance a and lb = luminance b in
  (Float.max la lb +. 0.05) /. (Float.min la lb +. 0.05)

let string_of_color color =
  match intent color with
  | Default -> "Default"
  | Indexed idx ->
      if idx = 0 then "Black"
      else if idx = 1 then "Red"
      else if idx = 2 then "Green"
      else if idx = 3 then "Yellow"
      else if idx = 4 then "Blue"
      else if idx = 5 then "Magenta"
      else if idx = 6 then "Cyan"
      else if idx = 7 then "White"
      else if idx = 8 then "Bright_black"
      else if idx = 9 then "Bright_red"
      else if idx = 10 then "Bright_green"
      else if idx = 11 then "Bright_yellow"
      else if idx = 12 then "Bright_blue"
      else if idx = 13 then "Bright_magenta"
      else if idx = 14 then "Bright_cyan"
      else if idx = 15 then "Bright_white"
      else Printf.sprintf "Indexed(%d)" idx
  | Rgb ->
      let r, g, b, a = to_rgba color in
      if a = 255 then Printf.sprintf "Rgb(%d,%d,%d)" r g b
      else Printf.sprintf "Rgba(%d,%d,%d,%d)" r g b a

let pp fmt color = Format.pp_print_string fmt (string_of_color color)

let hex_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let parse_hex_component s start len =
  let rec aux acc idx remaining =
    if remaining = 0 then Some acc
    else
      match hex_value s.[idx] with
      | None -> None
      | Some v -> aux ((acc lsl 4) lor v) (idx + 1) (remaining - 1)
  in
  aux 0 start len

let expand_short_hex s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    Bytes.unsafe_set buf (i * 2) c;
    Bytes.unsafe_set buf ((i * 2) + 1) c
  done;
  Bytes.unsafe_to_string buf

let sanitize_hex s =
  let s =
    if String.length s > 0 && s.[0] = '#' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  match String.length s with
  | 3 | 4 -> expand_short_hex s
  | 6 | 8 -> s
  | _ -> ""

let of_hex hex =
  let s = sanitize_hex hex in
  let len = String.length s in
  if len = 0 then None
  else if len = 6 then
    match
      ( parse_hex_component s 0 2,
        parse_hex_component s 2 2,
        parse_hex_component s 4 2 )
    with
    | Some r, Some g, Some b -> Some (of_rgb r g b)
    | _ -> None
  else if len = 8 then
    match
      ( parse_hex_component s 0 2,
        parse_hex_component s 2 2,
        parse_hex_component s 4 2,
        parse_hex_component s 6 2 )
    with
    | Some r, Some g, Some b, Some a -> Some (of_rgba r g b a)
    | _ -> None
  else None

let of_hex_exn hex =
  match of_hex hex with
  | Some color -> color
  | None -> invalid_arg "Color.of_hex_exn: invalid hex string"

let to_hex color =
  let r, g, b = to_rgb color in
  Printf.sprintf "#%02x%02x%02x" r g b

type reported = {
  foreground : int * int * int;
  background : int * int * int;
  palette : (int * int * int) option array;
}

let entry c i =
  if i >= 0 && i < 16 && i < Array.length c.palette then c.palette.(i) else None

let rgb (r, g, b) = of_rgb r g b

(* The RGB [color] shows on a terminal of colors [c]: SGR writes the
   default color, and a transparent RGB color, as [39] or [49], and the
   terminal draws its own [default]. *)
let shown c ~default color =
  match intent color with
  | Default -> Some default
  | Rgb when Packed.alpha color = 0 -> Some default
  | Indexed i when i < 16 -> entry c i
  | Indexed _ | Rgb -> Some (to_rgb color)

let contrast_on c ~fg ~bg =
  match
    (shown c ~default:c.foreground fg, shown c ~default:c.background bg)
  with
  | Some fg, Some bg -> Some (contrast (rgb fg) (rgb bg))
  | _ -> None

(* The legibility gate. Every table of styles by purpose that takes palette
   entries for text, mosaic's theme and its syntax style among them, calls
   this function, so that they all choose the same entries. *)
let legible ?target c i =
  let target =
    match target with
    | Some target -> target
    | None -> Float.min 4.5 (contrast (rgb c.foreground) (rgb c.background))
  in
  let reaches i =
    match entry c i with
    | Some e -> contrast (rgb e) (rgb c.background) >= target
    | None -> false
  in
  if reaches i then Some (indexed i)
  else if 0 <= i && i < 8 && reaches (i + 8) then Some (indexed (i + 8))
  else None
