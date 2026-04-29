type intent = Default | Indexed of int | Rgb
type t = int

let clamp_byte v = max 0 (min 255 v)
let clamp_channel_f v = max 0. (min 1. v)
let float_of_byte v = float_of_int v /. 255.
let byte_of_float v = int_of_float (Float.round (clamp_channel_f v *. 255.))

(* Standard ANSI16 fallback color palette. *)
let ansi_16_rgb =
  [|
    (0x00, 0x00, 0x00);
    (0x80, 0x00, 0x00);
    (0x00, 0x80, 0x00);
    (0x80, 0x80, 0x00);
    (0x00, 0x00, 0x80);
    (0x80, 0x00, 0x80);
    (0x00, 0x80, 0x80);
    (0xc0, 0xc0, 0xc0);
    (0x80, 0x80, 0x80);
    (0xff, 0x00, 0x00);
    (0x00, 0xff, 0x00);
    (0xff, 0xff, 0x00);
    (0x00, 0x00, 0xff);
    (0xff, 0x00, 0xff);
    (0x00, 0xff, 0xff);
    (0xff, 0xff, 0xff);
  |]

let cube_level = [| 0; 95; 135; 175; 215; 255 |]

(* Pre-computed flat palette: 256 colors * 3 channels = 768 values. Avoids
   tuple allocation when looking up palette colors. *)
let palette_flat =
  let arr = Array.make 768 0 in
  for i = 0 to 255 do
    let base = i * 3 in
    if i < 16 then begin
      let r, g, b = ansi_16_rgb.(i) in
      arr.(base) <- r;
      arr.(base + 1) <- g;
      arr.(base + 2) <- b
    end
    else if i < 232 then begin
      let n = i - 16 in
      arr.(base) <- cube_level.(n / 36);
      arr.(base + 1) <- cube_level.(n / 6 mod 6);
      arr.(base + 2) <- cube_level.(n mod 6)
    end
    else begin
      let gray = 8 + ((i - 232) * 10) in
      arr.(base) <- gray;
      arr.(base + 1) <- gray;
      arr.(base + 2) <- gray
    end
  done;
  arr

let palette_rgb_int idx =
  let idx = clamp_byte idx in
  if idx < 16 then ansi_16_rgb.(idx)
  else if idx < 232 then
    let n = idx - 16 in
    let r = cube_level.(n / 36) in
    let g = cube_level.(n / 6 mod 6) in
    let b = cube_level.(n mod 6) in
    (r, g, b)
  else
    let gray = 8 + ((idx - 232) * 10) in
    (gray, gray, gray)

module Packed = struct
  let () = assert (Sys.int_size >= 62)
  let alpha_shift = 0
  let blue_shift = 8
  let green_shift = 16
  let red_shift = 24
  let slot_shift = 32
  let intent_shift = 40
  let intent_rgb = 0
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
  let[@inline] intent c = (c lsr intent_shift) land intent_mask
  let[@inline] rgba c = c land 0xFFFFFFFF
  let default = make ~intent:intent_default ~slot:0 ~r:0 ~g:0 ~b:0 ~a:0

  let decode bits =
    let bits = bits land full_mask in
    match intent bits with 0 | 1 | 2 -> bits | _ -> default
end

let default = Packed.default
let of_rgb r g b = Packed.make ~intent:Packed.intent_rgb ~slot:0 ~r ~g ~b ~a:255
let of_rgba r g b a = Packed.make ~intent:Packed.intent_rgb ~slot:0 ~r ~g ~b ~a

let indexed idx =
  let idx = clamp_byte idx in
  let r, g, b = palette_rgb_int idx in
  Packed.make ~intent:Packed.intent_indexed ~slot:idx ~r ~g ~b ~a:255

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

let of_rgba_f r g b a =
  of_rgba (byte_of_float r) (byte_of_float g) (byte_of_float b)
    (byte_of_float a)

let intent color =
  match Packed.intent color with
  | 1 -> Indexed (Packed.slot color)
  | 2 -> Default
  | _ -> Rgb

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

let blend ?(mode = `Perceptual) ~src ~dst () =
  with_rgba_f src (fun sr sg sb sa_f ->
      with_rgba_f dst (fun dr dg db da_f ->
          let sa = clamp_channel_f sa_f in
          if sa >= 0.999 then
            of_rgb (byte_of_float sr) (byte_of_float sg) (byte_of_float sb)
          else if sa <= Float.epsilon then dst
          else
            let sa_blend =
              match mode with
              | `Linear -> sa
              | `Perceptual ->
                  if sa >= 0.8 then
                    let norm = (sa -. 0.8) *. 5. in
                    0.8 +. (Float.pow norm 0.2 *. 0.2)
                  else Float.pow sa 0.9
            in
            let blend sc dc = (sa_blend *. sc) +. ((1. -. sa_blend) *. dc) in
            let r = byte_of_float (blend sr dr) in
            let g = byte_of_float (blend sg dg) in
            let b = byte_of_float (blend sb db) in
            let a = byte_of_float (sa +. da_f -. (sa *. da_f)) in
            if a = 255 then of_rgb r g b else of_rgba r g b a))

(* Check if string contains a substring. Zero-allocation. *)
let contains_substring s sub =
  let len = String.length s in
  let sublen = String.length sub in
  if sublen = 0 then true
  else if sublen > len then false
  else
    let rec match_at i j =
      if j >= sublen then true
      else if String.unsafe_get s (i + j) = String.unsafe_get sub j then
        match_at i (j + 1)
      else false
    in
    let rec check i =
      if i > len - sublen then false
      else if match_at i 0 then true
      else check (i + 1)
    in
    check 0

let detected_level =
  lazy
    (match Sys.getenv_opt "COLORTERM" with
    | Some "truecolor" | Some "24bit" -> `Truecolor
    | _ -> (
        match Sys.getenv_opt "TERM" with
        | Some term when contains_substring term "256" -> `Ansi256
        | Some term when contains_substring term "truecolor" -> `Truecolor
        | _ -> `Ansi16))

let detect_level () = Lazy.force detected_level

let downgrade ?level color =
  if Packed.intent color = Packed.intent_default || Packed.alpha color = 0 then
    color
  else
    match Option.value level ~default:(detect_level ()) with
    | `Truecolor -> color
    | (`Ansi256 | `Ansi16) as effective_level ->
        let target_size = if effective_level = `Ansi256 then 256 else 16 in
        let r, g, b = to_rgb color in
        let min_dist = ref max_int in
        let nearest = ref 0 in
        for i = 0 to target_size - 1 do
          let base = i * 3 in
          let pr = Array.unsafe_get palette_flat base in
          let pg = Array.unsafe_get palette_flat (base + 1) in
          let pb = Array.unsafe_get palette_flat (base + 2) in
          let dr = r - pr in
          let dg = g - pg in
          let db = b - pb in
          let dist = (dr * dr) + (dg * dg) + (db * db) in
          if dist < !min_dist then (
            min_dist := dist;
            nearest := i)
        done;
        indexed !nearest

let emit_indexed_sgr ~bg push idx =
  if idx < 8 then push ((if bg then 40 else 30) + idx)
  else if idx < 16 then push ((if bg then 100 else 90) + idx - 8)
  else (
    push (if bg then 48 else 38);
    push 5;
    push idx)

let emit_sgr_codes ~bg push color =
  match intent color with
  | Default -> push (if bg then 49 else 39)
  | Indexed idx -> emit_indexed_sgr ~bg push idx
  | Rgb ->
      if Packed.alpha color = 0 then push (if bg then 49 else 39)
      else (
        push (if bg then 48 else 38);
        push 2;
        push (Packed.red color);
        push (Packed.green color);
        push (Packed.blue color))

let to_sgr_codes ~bg color =
  let acc = ref [] in
  emit_sgr_codes ~bg (fun code -> acc := code :: !acc) color;
  List.rev !acc

let invert color =
  let r, g, b = to_rgb color in
  of_rgb (255 - r) (255 - g) (255 - b)

let pack color = color
let unpack = Packed.decode

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
