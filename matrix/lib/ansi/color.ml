type t =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Extended of int
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : int }

let clamp_byte v = max 0 (min 255 v)
let clamp_channel_f v = max 0. (min 1. v)
let float_of_byte v = float_of_int v /. 255.
let byte_of_float v = int_of_float (Float.round (clamp_channel_f v *. 255.))

(* Standard xterm-256 ANSI 16 color palette (RGB in [0,255]) *)
let ansi_16_rgb =
  [|
    (0, 0, 0);
    (* 0: Black *)
    (205, 0, 0);
    (* 1: Red *)
    (0, 205, 0);
    (* 2: Green *)
    (205, 205, 0);
    (* 3: Yellow *)
    (0, 0, 238);
    (* 4: Blue *)
    (205, 0, 205);
    (* 5: Magenta *)
    (0, 205, 205);
    (* 6: Cyan *)
    (229, 229, 229);
    (* 7: White *)
    (127, 127, 127);
    (* 8: Bright_black *)
    (255, 0, 0);
    (* 9: Bright_red *)
    (0, 255, 0);
    (* 10: Bright_green *)
    (255, 255, 0);
    (* 11: Bright_yellow *)
    (92, 92, 255);
    (* 12: Bright_blue *)
    (255, 0, 255);
    (* 13: Bright_magenta *)
    (0, 255, 255);
    (* 14: Bright_cyan *)
    (255, 255, 255);
    (* 15: Bright_white *)
  |]

let cube_level = [| 0; 95; 135; 175; 215; 255 |]

(* Pre-computed flat palette: 256 colors * 3 channels = 768 values. Avoids tuple
   allocation when looking up palette colors. *)
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

let palette_index = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
  | Bright_black -> 8
  | Bright_red -> 9
  | Bright_green -> 10
  | Bright_yellow -> 11
  | Bright_blue -> 12
  | Bright_magenta -> 13
  | Bright_cyan -> 14
  | Bright_white -> 15
  | Extended idx -> idx (* palette_rgb_int clamps if needed *)
  | Rgb _ | Rgba _ -> -1

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

let to_rgb color =
  match color with
  | Rgb { r; g; b } -> (r, g, b)
  | Rgba { r; g; b; _ } -> (r, g, b)
  | _ ->
      let idx = palette_index color in
      if idx >= 0 then palette_rgb_int idx else (0, 0, 0)

let to_rgba color =
  let r, g, b = to_rgb color in
  let a = match color with Rgba { a; _ } -> a | _ -> 255 in
  (r, g, b, a)

let to_rgba_f color =
  let r, g, b, a = to_rgba color in
  (float_of_byte r, float_of_byte g, float_of_byte b, float_of_byte a)

let default = Rgba { r = 0; g = 0; b = 0; a = 0 }
let black = Black
let red = Red
let green = Green
let yellow = Yellow
let blue = Blue
let magenta = Magenta
let cyan = Cyan
let white = White
let bright_black = Bright_black
let bright_red = Bright_red
let bright_green = Bright_green
let bright_yellow = Bright_yellow
let bright_blue = Bright_blue
let bright_magenta = Bright_magenta
let bright_cyan = Bright_cyan
let bright_white = Bright_white

let grayscale ~level =
  let level = max 0 (min 23 level) in
  Extended (232 + level)

let of_rgb r g b = Rgb { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b }

let of_rgba r g b a =
  Rgba
    { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; a = clamp_byte a }

let of_rgba_f r g b a =
  of_rgba (byte_of_float r) (byte_of_float g) (byte_of_float b)
    (byte_of_float a)

(* Pre-allocated Extended colors (16-255) - avoids allocation in hot paths *)
let extended_colors = Array.init 240 (fun i -> Extended (i + 16))

let of_palette_index idx =
  let idx = clamp_byte idx in
  if idx < 16 then
    match idx with
    | 0 -> Black
    | 1 -> Red
    | 2 -> Green
    | 3 -> Yellow
    | 4 -> Blue
    | 5 -> Magenta
    | 6 -> Cyan
    | 7 -> White
    | 8 -> Bright_black
    | 9 -> Bright_red
    | 10 -> Bright_green
    | 11 -> Bright_yellow
    | 12 -> Bright_blue
    | 13 -> Bright_magenta
    | 14 -> Bright_cyan
    | _ -> Bright_white
  else Array.unsafe_get extended_colors (idx - 16)

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

(* Internal helper: extract RGBA components without tuple allocation. Returns
   components via out parameters encoded as a single int: (r << 24) | (g << 16)
   | (b << 8) | a *)
let[@inline] rgba_packed color =
  match color with
  | Rgb { r; g; b } -> (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor 255
  | Rgba { r; g; b; a } -> (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor a
  | _ ->
      let idx = clamp_byte (palette_index color) in
      let base = idx * 3 in
      let r = Array.unsafe_get palette_flat base in
      let g = Array.unsafe_get palette_flat (base + 1) in
      let b = Array.unsafe_get palette_flat (base + 2) in
      (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor 255

let equal a b = rgba_packed a = rgba_packed b
let compare a b = Int.compare (rgba_packed a) (rgba_packed b)

let hash color =
  let h = rgba_packed color in
  h lxor (h lsr 16) land max_int

let alpha color = match color with Rgba { a; _ } -> float_of_byte a | _ -> 1.

let[@inline] with_rgba_f color f =
  let packed = rgba_packed color in
  let r = float_of_byte ((packed lsr 24) land 0xFF) in
  let g = float_of_byte ((packed lsr 16) land 0xFF) in
  let b = float_of_byte ((packed lsr 8) land 0xFF) in
  let a = float_of_byte (packed land 0xFF) in
  f r g b a

let blend ?(mode = `Perceptual) ~src ~dst () =
  with_rgba_f src (fun sr sg sb sa_f ->
      with_rgba_f dst (fun dr dg db da_f ->
          let sa = clamp_channel_f sa_f in
          if sa >= 0.999 then
            Rgb
              {
                r = byte_of_float sr;
                g = byte_of_float sg;
                b = byte_of_float sb;
              }
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
            if a = 255 then Rgb { r; g; b } else Rgba { r; g; b; a }))

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
  (* Transparent colors (alpha=0) represent "use terminal default" — preserve
     them through downgrading since they carry no meaningful RGB to quantize. *)
  match color with
  | Rgba { a = 0; _ } -> color
  | _ -> (
      let effective_level = Option.value level ~default:(detect_level ()) in
      match effective_level with
      | `Truecolor -> color
      | `Ansi256 | `Ansi16 ->
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
          of_palette_index !nearest)

(* Emit SGR codes via push callback. Zero-allocation. *)
let emit_sgr_codes ~bg push color =
  match color with
  | Rgba { a = 0; _ } -> push (if bg then 49 else 39)
  | Black -> push (if bg then 40 else 30)
  | Red -> push (if bg then 41 else 31)
  | Green -> push (if bg then 42 else 32)
  | Yellow -> push (if bg then 43 else 33)
  | Blue -> push (if bg then 44 else 34)
  | Magenta -> push (if bg then 45 else 35)
  | Cyan -> push (if bg then 46 else 36)
  | White -> push (if bg then 47 else 37)
  | Bright_black -> push (if bg then 100 else 90)
  | Bright_red -> push (if bg then 101 else 91)
  | Bright_green -> push (if bg then 102 else 92)
  | Bright_yellow -> push (if bg then 103 else 93)
  | Bright_blue -> push (if bg then 104 else 94)
  | Bright_magenta -> push (if bg then 105 else 95)
  | Bright_cyan -> push (if bg then 106 else 96)
  | Bright_white -> push (if bg then 107 else 97)
  | Extended idx ->
      push (if bg then 48 else 38);
      push 5;
      push (clamp_byte idx)
  | Rgb { r; g; b } ->
      push (if bg then 48 else 38);
      push 2;
      push r;
      push g;
      push b
  | Rgba { r; g; b; _ } ->
      push (if bg then 48 else 38);
      push 2;
      push r;
      push g;
      push b

let to_sgr_codes ~bg color =
  let acc = ref [] in
  emit_sgr_codes ~bg (fun code -> acc := code :: !acc) color;
  List.rev !acc

let invert color =
  let r, g, b = to_rgb color in
  of_rgb (255 - r) (255 - g) (255 - b)

module Packed = struct
  let () = assert (Sys.int_size >= 62)
  let tag_shift = 58
  let tag_basic = 1 lsl tag_shift
  let tag_extended = 2 lsl tag_shift
  let tag_rgb = 3 lsl tag_shift
  let tag_rgba = 4 lsl tag_shift
  let tag_mask = 7 lsl tag_shift
  let data_mask = (1 lsl tag_shift) - 1

  let encode color =
    match color with
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white ->
        let idx = palette_index color in
        tag_basic lor idx
    | Extended idx -> tag_extended lor clamp_byte idx
    | Rgb { r; g; b } ->
        let data = (r lsl 16) lor (g lsl 8) lor b in
        tag_rgb lor data
    | Rgba { r; g; b; a } ->
        let data = (r lsl 24) lor (g lsl 16) lor (b lsl 8) lor a in
        tag_rgba lor data

  let decode packed =
    let tag = packed land tag_mask in
    let data = packed land data_mask in
    match tag lsr tag_shift with
    | 0 -> Rgba { r = 0; g = 0; b = 0; a = 0 }
    | 1 -> of_palette_index data
    | 2 -> Extended data
    | 3 ->
        let r = (data lsr 16) land 0xFF in
        let g = (data lsr 8) land 0xFF in
        let b = data land 0xFF in
        Rgb { r; g; b }
    | 4 ->
        let r = (data lsr 24) land 0xFF in
        let g = (data lsr 16) land 0xFF in
        let b = (data lsr 8) land 0xFF in
        let a = data land 0xFF in
        Rgba { r; g; b; a }
    | _ -> Rgba { r = 0; g = 0; b = 0; a = 0 }
end

let pack = Packed.encode
let unpack = Packed.decode

let string_of_color = function
  | Black -> "Black"
  | Red -> "Red"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Magenta -> "Magenta"
  | Cyan -> "Cyan"
  | White -> "White"
  | Bright_black -> "Bright_black"
  | Bright_red -> "Bright_red"
  | Bright_green -> "Bright_green"
  | Bright_yellow -> "Bright_yellow"
  | Bright_blue -> "Bright_blue"
  | Bright_magenta -> "Bright_magenta"
  | Bright_cyan -> "Bright_cyan"
  | Bright_white -> "Bright_white"
  | Extended idx -> Printf.sprintf "Extended(%d)" idx
  | Rgb { r; g; b } -> Printf.sprintf "Rgb(%d,%d,%d)" r g b
  | Rgba { r; g; b; a } -> Printf.sprintf "Rgba(%d,%d,%d,%d)" r g b a

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
  (* unsafe_to_string is safe here: buf is local and not used after
     conversion *)
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
