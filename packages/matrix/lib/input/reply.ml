type setting =
  | Not_recognized
  | Set
  | Reset
  | Permanently_set
  | Permanently_reset

type t =
  | Primary_attributes of int list
  | Secondary_attributes of int list
  | Unit_id of string
  | Mode of { dec : bool; mode : int; setting : setting }
  | Cursor of { row : int; col : int; page : int option }
  | Operating_status of [ `Ok | `Malfunction ]
  | Text_area_pixels of { height : int; width : int }
  | Cell_pixels of { height : int; width : int }
  | Text_area of { rows : int; cols : int }
  | Resize of { rows : int; cols : int; height : int; width : int }
  | Keyboard_flags of int
  | Color_scheme of [ `Dark | `Light ]
  | Version of string
  | Termcap of { valid : bool; entries : (string * string option) list }
  | Color of {
      target : [ `Foreground | `Background | `Cursor | `Entry of int ];
      rgb : int * int * int;
      terminator : Ansi.terminator;
    }
  | Clipboard of {
      selection : string;
      data : string;
      terminator : Ansi.terminator;
    }
  | Graphics of { id : int option; message : string }
  | Osc of { content : string; terminator : Ansi.terminator }
  | Dcs of { header : Ansi.Parser.sequence; data : string }
  | Csi of Ansi.Parser.sequence
  | Ss3 of Ansi.Parser.sequence

let setting_name = function
  | Not_recognized -> "Not_recognized"
  | Set -> "Set"
  | Reset -> "Reset"
  | Permanently_set -> "Permanently_set"
  | Permanently_reset -> "Permanently_reset"

let pp_ints ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
    Format.pp_print_int ppf l

(* An OSC reply ends with ST unless it says otherwise. *)
let pp_bel ppf = function
  | `St -> ()
  | `Bel -> Format.pp_print_string ppf " BEL"

let pp ppf = function
  | Primary_attributes l -> Format.fprintf ppf "Primary_attributes %a" pp_ints l
  | Secondary_attributes l ->
      Format.fprintf ppf "Secondary_attributes %a" pp_ints l
  | Unit_id s -> Format.fprintf ppf "Unit_id %S" s
  | Mode { dec; mode; setting } ->
      Format.fprintf ppf "Mode %s%d %s"
        (if dec then "?" else "")
        mode (setting_name setting)
  | Cursor { row; col; page } -> (
      Format.fprintf ppf "Cursor %d,%d" row col;
      match page with None -> () | Some p -> Format.fprintf ppf " page %d" p)
  | Operating_status `Ok -> Format.pp_print_string ppf "Operating_status Ok"
  | Operating_status `Malfunction ->
      Format.pp_print_string ppf "Operating_status Malfunction"
  | Text_area_pixels { height; width } ->
      Format.fprintf ppf "Text_area_pixels %dx%d" width height
  | Cell_pixels { height; width } ->
      Format.fprintf ppf "Cell_pixels %dx%d" width height
  | Text_area { rows; cols } -> Format.fprintf ppf "Text_area %dx%d" cols rows
  | Resize { rows; cols; height; width } ->
      Format.fprintf ppf "Resize %dx%d %dx%d" cols rows width height
  | Keyboard_flags f -> Format.fprintf ppf "Keyboard_flags %d" f
  | Color_scheme `Dark -> Format.pp_print_string ppf "Color_scheme Dark"
  | Color_scheme `Light -> Format.pp_print_string ppf "Color_scheme Light"
  | Version s -> Format.fprintf ppf "Version %S" s
  | Termcap { valid; entries } ->
      Format.fprintf ppf "Termcap%s" (if valid then "" else " invalid");
      List.iter
        (fun (name, value) ->
          match value with
          | None -> Format.fprintf ppf " %S" name
          | Some v -> Format.fprintf ppf " %S=%S" name v)
        entries
  | Color { target; rgb = r, g, b; terminator } ->
      let target =
        match target with
        | `Foreground -> "foreground"
        | `Background -> "background"
        | `Cursor -> "cursor"
        | `Entry i -> "entry " ^ string_of_int i
      in
      Format.fprintf ppf "Color %s #%02x%02x%02x%a" target r g b pp_bel
        terminator
  | Clipboard { selection; data; terminator } ->
      Format.fprintf ppf "Clipboard %S %S%a" selection data pp_bel terminator
  | Graphics { id; message } ->
      Format.fprintf ppf "Graphics%s %S"
        (match id with None -> "" | Some i -> " i=" ^ string_of_int i)
        message
  | Osc { content; terminator } ->
      Format.fprintf ppf "Osc %S%a" content pp_bel terminator
  | Dcs { header; data } ->
      Format.fprintf ppf "Dcs %a %S" Ansi.Parser.pp_token
        (Ansi.Parser.Open (Dcs header)) data
  | Csi s ->
      Format.fprintf ppf "Csi %a" Ansi.Parser.pp_token (Ansi.Parser.Csi s)
  | Ss3 s ->
      Format.fprintf ppf "Ss3 %a" Ansi.Parser.pp_token (Ansi.Parser.Csi s)

(* Reading control strings *)

module Params = Ansi.Parser.Params

let hex_digit c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | 'A' .. 'F' -> Char.code c - 55
  | _ -> -1

(* [s] written as two hex digits per byte, as XTGETTCAP writes names and
   values. *)
let of_hex s =
  let n = String.length s in
  if n mod 2 <> 0 then None
  else
    let b = Bytes.create (n / 2) in
    let rec go i =
      if i >= n then Some (Bytes.unsafe_to_string b)
      else
        let h = hex_digit s.[i] and l = hex_digit s.[i + 1] in
        if h < 0 || l < 0 then None
        else (
          Bytes.unsafe_set b (i / 2) (Char.unsafe_chr ((h lsl 4) lor l));
          go (i + 2))
    in
    go 0

(* The digits of a channel: [None] unless 1 to 4 hex digits. *)
let hex_value s =
  let n = String.length s in
  if n < 1 || n > 4 then None
  else
    let rec go i v =
      if i >= n then Some v
      else
        let d = hex_digit s.[i] in
        if d < 0 then None else go (i + 1) ((v lsl 4) lor d)
    in
    go 0 0

(* [rgb:r/g/b]: each channel scaled to 0..255. *)
let scaled s =
  Option.map
    (fun v ->
      let max = (1 lsl (4 * String.length s)) - 1 in
      ((v * 255) + (max / 2)) / max)
    (hex_value s)

(* [#rgb] and longer: the digits are the high bits of a 16-bit channel. *)
let high_bits s =
  Option.map (fun v -> (v lsl (16 - (4 * String.length s))) lsr 8) (hex_value s)

(* A color name as Xlib reads it: case-insensitive, and without its spaces,
   as Xcms looks names up (libX11, src/xcms/cmsColNm.c). A spec of more
   characters than the longest name, spaces aside, names no color, and is
   rejected without a copy: a reply's spec can be kilobytes long. *)
let is_space = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let longest_name =
  Array.fold_left (fun n name -> max n (String.length name)) 0 X11_colors.names

let named spec =
  let length = ref 0 in
  for i = 0 to String.length spec - 1 do
    if not (is_space spec.[i]) then incr length
  done;
  if !length = 0 || !length > longest_name then None
  else
    let key = Bytes.create !length in
    let j = ref 0 in
    for i = 0 to String.length spec - 1 do
      let c = spec.[i] in
      if not (is_space c) then (
        Bytes.set key !j (Char.lowercase_ascii c);
        incr j)
    done;
    let key = Bytes.unsafe_to_string key in
    let rec search lo hi =
      if lo > hi then None
      else
        let mid = (lo + hi) / 2 in
        let c = String.compare X11_colors.names.(mid) key in
        if c = 0 then
          let v = X11_colors.rgb.(mid) in
          Some ((v lsr 16) land 0xFF, (v lsr 8) land 0xFF, v land 0xFF)
        else if c < 0 then search (mid + 1) hi
        else search lo (mid - 1)
    in
    search 0 (Array.length X11_colors.names - 1)

let rgb_of_spec spec =
  let channels f r g b =
    match (f r, f g, f b) with
    | Some r, Some g, Some b -> Some (r, g, b)
    | _ -> None
  in
  let n = String.length spec in
  if n > 4 && String.sub spec 0 4 = "rgb:" then
    match String.split_on_char '/' (String.sub spec 4 (n - 4)) with
    | [ r; g; b ] -> channels scaled r g b
    | _ -> None
  else if n > 1 && spec.[0] = '#' && (n - 1) mod 3 = 0 && n - 1 <= 12 then
    let d = (n - 1) / 3 in
    channels high_bits (String.sub spec 1 d)
      (String.sub spec (1 + d) d)
      (String.sub spec (1 + (2 * d)) d)
  else if n > 0 && spec.[0] = '#' then None
  else named spec

let base64_value c =
  match c with
  | 'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 71
  | '0' .. '9' -> Char.code c + 4
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

(* Base64 (RFC 4648 §4), its padding optional. *)
let of_base64 s =
  let n = String.length s in
  let n = if n > 0 && s.[n - 1] = '=' then n - 1 else n in
  let n = if n > 0 && s.[n - 1] = '=' then n - 1 else n in
  if n mod 4 = 1 || (String.length s > n && String.length s mod 4 <> 0) then
    None
  else
    let out = Buffer.create (n * 3 / 4) in
    let rec go i acc bits =
      if i >= n then Some (Buffer.contents out)
      else
        let v = base64_value s.[i] in
        if v < 0 then None
        else
          let acc = (acc lsl 6) lor v and bits = bits + 6 in
          if bits >= 8 then (
            Buffer.add_char out
              (Char.unsafe_chr ((acc lsr (bits - 8)) land 0xFF));
            go (i + 1) (acc land ((1 lsl (bits - 8)) - 1)) (bits - 8))
          else go (i + 1) acc bits
    in
    go 0 0 0

let split_at c s =
  match String.index_opt s c with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let is_number s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let of_osc terminator content =
  let raw = Osc { content; terminator } in
  let number, rest = split_at ';' content in
  let color target spec =
    match rgb_of_spec spec with
    | Some rgb -> Color { target; rgb; terminator }
    | None -> raw
  in
  match (number, rest) with
  | "10", Some spec -> color `Foreground spec
  | "11", Some spec -> color `Background spec
  | "12", Some spec -> color `Cursor spec
  | "4", Some rest -> (
      match split_at ';' rest with
      | index, Some spec when is_number index && String.length index <= 9 ->
          color (`Entry (int_of_string index)) spec
      | _ -> raw)
  | "52", Some rest -> (
      match split_at ';' rest with
      | selection, Some data -> (
          match of_base64 data with
          | Some data -> Clipboard { selection; data; terminator }
          | None -> raw)
      | _ -> raw)
  | _ -> raw

let termcap valid data =
  let entry s =
    match split_at '=' s with
    | name, None -> Option.map (fun n -> (n, None)) (of_hex name)
    | name, Some value -> (
        match (of_hex name, of_hex value) with
        | Some n, Some v -> Some (n, Some v)
        | _ -> None)
  in
  let entries = if data = "" then [] else String.split_on_char ';' data in
  let rec go acc = function
    | [] -> Some (Termcap { valid; entries = List.rev acc })
    | e :: rest -> (
        match entry e with Some e -> go (e :: acc) rest | None -> None)
  in
  go [] entries

let of_dcs (h : Ansi.Parser.sequence) data =
  let n = Params.count h.params in
  let raw = Dcs { header = h; data } in
  match (h.marker, h.intermediates, h.final) with
  | Some '>', "", '|' when n = 0 -> Version data
  | None, "!", '|' when n = 0 -> Unit_id data
  | None, "+", 'r' when n = 1 -> (
      match Params.value h.params 0 with
      | Some ((0 | 1) as v) -> Option.value (termcap (v = 1) data) ~default:raw
      | _ -> raw)
  | _ -> raw

let graphics content =
  let keys, message = split_at ';' content in
  let id =
    List.find_map
      (fun kv ->
        match split_at '=' kv with
        | "i", Some v when is_number v && String.length v <= 9 ->
            Some (int_of_string v)
        | _ -> None)
      (String.split_on_char ',' keys)
  in
  Graphics { id; message = Option.value message ~default:"" }

(* Encoding *)

let invalid what = invalid_arg ("Reply.encode: " ^ what)

let add_nat b n =
  if n < 0 then invalid "a negative number";
  Buffer.add_string b (string_of_int n)

let add_nats b l =
  List.iteri
    (fun i n ->
      if i > 0 then Buffer.add_char b ';';
      add_nat b n)
    l

(* Rows and columns count from 1 in the bytes, so -1 would write 0, which
   reads back as row or column 0. *)
let add_position b n =
  if n < 0 then invalid "a negative number";
  if n = max_int then invalid "a position of max_int";
  add_nat b (n + 1)

let add_channel b v =
  if v < 0 || v > 255 then invalid "a channel outside 0..255";
  Printf.bprintf b "%04x" (v * 257)

(* What the parser passes as a string's content (Ansi.Parser.Data): UTF-8
   with no control but BS..CR, and no U+0080..U+009F (C2 80..C2 9F). *)
let add_content b s =
  let n = String.length s in
  let rec ok i =
    i >= n
    ||
    let c = Char.code (String.unsafe_get s i) in
    ((c >= 0x20 && c <> 0x7F) || (c >= 0x08 && c <= 0x0D))
    && (not (c = 0xC2 && i + 1 < n && Char.code s.[i + 1] < 0xA0))
    && ok (i + 1)
  in
  if not (String.is_valid_utf_8 s && ok 0) then
    invalid "string content outside the command-string set";
  Buffer.add_string b s

let st b = Buffer.add_string b "\027\\"
let add_terminator b = function `St -> st b | `Bel -> Buffer.add_char b '\007'
let add_hex b s = String.iter (fun c -> Printf.bprintf b "%02X" (Char.code c)) s

let base64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let add_base64 b s =
  let n = String.length s in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let rec go i =
    if i < n then (
      let v = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let sextet k = base64_alphabet.[(v lsr (18 - (6 * k))) land 63] in
      Buffer.add_char b (sextet 0);
      Buffer.add_char b (sextet 1);
      Buffer.add_char b (if i + 1 < n then sextet 2 else '=');
      Buffer.add_char b (if i + 2 < n then sextet 3 else '=');
      go (i + 3))
  in
  go 0

(* A control sequence's bytes after its introducer. *)
let add_sequence b (s : Ansi.Parser.sequence) =
  Option.iter (Buffer.add_char b) s.marker;
  let p = s.params in
  for i = 0 to Params.count p - 1 do
    if i > 0 then Buffer.add_char b ';';
    for j = 0 to Params.fields p i - 1 do
      if j > 0 then Buffer.add_char b ':';
      Option.iter (add_nat b) (Params.field p i j)
    done
  done;
  Buffer.add_string b s.intermediates;
  Buffer.add_char b s.final

let setting_value = function
  | Not_recognized -> 0
  | Set -> 1
  | Reset -> 2
  | Permanently_set -> 3
  | Permanently_reset -> 4

(* The bound [of_osc] and [graphics] read numbers to. *)
let add_id b n =
  if n > 999_999_999 then invalid "an index or id above 999,999,999";
  add_nat b n

let add b = function
  | Primary_attributes l ->
      Buffer.add_string b "\027[?";
      add_nats b l;
      Buffer.add_char b 'c'
  | Secondary_attributes l ->
      Buffer.add_string b "\027[>";
      add_nats b l;
      Buffer.add_char b 'c'
  | Unit_id s ->
      Buffer.add_string b "\027P!|";
      add_content b s;
      st b
  | Mode { dec; mode; setting } ->
      Buffer.add_string b (if dec then "\027[?" else "\027[");
      add_nats b [ mode; setting_value setting ];
      Buffer.add_string b "$y"
  | Cursor { row; col; page } -> (
      Buffer.add_string b (if page = None then "\027[" else "\027[?");
      add_position b row;
      Buffer.add_char b ';';
      add_position b col;
      match page with
      | None -> Buffer.add_char b 'R'
      | Some p ->
          if p < 1 then invalid "a page below 1";
          Buffer.add_char b ';';
          add_nat b p;
          Buffer.add_char b 'R')
  | Operating_status s ->
      Buffer.add_string b (if s = `Ok then "\027[0n" else "\027[3n")
  | Text_area_pixels { height; width } ->
      Buffer.add_string b "\027[";
      add_nats b [ 4; height; width ];
      Buffer.add_char b 't'
  | Cell_pixels { height; width } ->
      Buffer.add_string b "\027[";
      add_nats b [ 6; height; width ];
      Buffer.add_char b 't'
  | Text_area { rows; cols } ->
      Buffer.add_string b "\027[";
      add_nats b [ 8; rows; cols ];
      Buffer.add_char b 't'
  | Resize { rows; cols; height; width } ->
      Buffer.add_string b "\027[";
      add_nats b [ 48; rows; cols; height; width ];
      Buffer.add_char b 't'
  | Keyboard_flags f ->
      Buffer.add_string b "\027[?";
      add_nat b f;
      Buffer.add_char b 'u'
  | Color_scheme s ->
      Buffer.add_string b (if s = `Dark then "\027[?997;1n" else "\027[?997;2n")
  | Version s ->
      Buffer.add_string b "\027P>|";
      add_content b s;
      st b
  | Termcap { valid; entries } ->
      Buffer.add_string b (if valid then "\027P1+r" else "\027P0+r");
      List.iteri
        (fun i (name, value) ->
          if name = "" then invalid "an empty capability name";
          if i > 0 then Buffer.add_char b ';';
          add_hex b name;
          Option.iter
            (fun v ->
              Buffer.add_char b '=';
              add_hex b v)
            value)
        entries;
      st b
  | Color { target; rgb = r, g, b'; terminator } ->
      Buffer.add_string b "\027]";
      (match target with
      | `Foreground -> Buffer.add_string b "10"
      | `Background -> Buffer.add_string b "11"
      | `Cursor -> Buffer.add_string b "12"
      | `Entry i ->
          Buffer.add_string b "4;";
          add_id b i);
      Buffer.add_string b ";rgb:";
      add_channel b r;
      Buffer.add_char b '/';
      add_channel b g;
      Buffer.add_char b '/';
      add_channel b b';
      add_terminator b terminator
  | Clipboard { selection; data; terminator } ->
      if String.contains selection ';' then invalid "a selection holding ';'";
      Buffer.add_string b "\027]52;";
      add_content b selection;
      Buffer.add_char b ';';
      add_base64 b data;
      add_terminator b terminator
  | Graphics { id; message } ->
      Buffer.add_string b "\027_G";
      Option.iter
        (fun i ->
          Buffer.add_string b "i=";
          add_id b i)
        id;
      Buffer.add_char b ';';
      add_content b message;
      st b
  | Osc { content; terminator } ->
      Buffer.add_string b "\027]";
      add_content b content;
      add_terminator b terminator
  | Dcs { header; data } ->
      Buffer.add_string b "\027P";
      add_sequence b header;
      add_content b data;
      st b
  | Csi s ->
      Buffer.add_string b "\027[";
      add_sequence b s
  | Ss3 s ->
      Buffer.add_string b "\027O";
      add_sequence b s

let encode = function
  | [] -> ""
  | rs ->
      let b = Buffer.create 32 in
      List.iter (add b) rs;
      Buffer.contents b
