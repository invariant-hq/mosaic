(* Escape: Low-level ANSI escape sequence builders *)

type writer = Writer.t
type t = writer -> unit
type terminator = [ `Bel | `St ]

(* ASCII character code constants *)
let char_0 = Char.code '0' (* 48 *)
let char_a_hex = Char.code 'a' - 10 (* 87: for hex digits a-f *)

(* Writer primitives — delegated to Writer module *)

let make = Writer.make
let len = Writer.len
let reset_pos = Writer.reset_pos
let slice = Writer.slice

(* Low-level writing — delegated to Writer module *)

let write_char = Writer.write_char
let write_string = Writer.write_string
let write_subbytes = Writer.write_subbytes

(* Integer writing - optimized for zero stack allocation *)

let[@inline] digit_char n = Char.unsafe_chr (char_0 + n)

(* Powers of 10 lookup table sized for the host int range. *)
let pow10 =
  let rec max_digits p d =
    if p > max_int / 10 then d else max_digits (p * 10) (d + 1)
  in
  let len = max_digits 1 1 in
  let tbl = Array.make len 1 in
  for i = 1 to len - 1 do
    tbl.(i) <- tbl.(i - 1) * 10
  done;
  tbl

(* Write unsigned integer - fully iterative, no stack allocation *)
let add_uint w n =
  if n = 0 then write_char w '0'
  else
    let digits = ref 1 in
    let max_digits = Array.length pow10 in
    while !digits < max_digits && n >= pow10.(!digits) do
      incr digits
    done;
    for i = !digits - 1 downto 0 do
      write_char w (digit_char (n / pow10.(i) mod 10))
    done

let add_int w n =
  if n >= 0 then add_uint w n
  else (
    (* Work in negative space to handle Int.min_int without overflow. For
       negative n: q = n / 10 is negative or zero, r = n - q*10 is in [-9, 0] *)
    write_char w '-';
    let rec emit_negative n =
      let q = n / 10 in
      let r = n - (q * 10) in
      if q <> 0 then emit_negative q;
      write_char w (digit_char (-r))
    in
    emit_negative n)

(* Write a two-digit lowercase hex value for a byte [0,255] *)
let[@inline] hex_digit n =
  Char.unsafe_chr (if n < 10 then char_0 + n else char_a_hex + n)

let add_hex2 w v =
  let v = max 0 (min 255 v) in
  write_char w (hex_digit (v lsr 4));
  write_char w (hex_digit (v land 0xF))

let write_terminator w = function
  | `Bel -> write_char w '\007'
  | `St ->
      write_char w '\027';
      write_char w '\\'

(* Combinators *)

let empty _ = ()
let literal s w = write_string w s
let char c w = write_char w c

let concat a b w =
  a w;
  b w

let seq ts w =
  let rec loop = function
    | [] -> ()
    | t :: rest ->
        t w;
        loop rest
  in
  loop ts

let emit t w = t w
let bytes b ~off ~len w = write_subbytes w b off len

let utf8 cp w =
  let cp =
    (* Clamp invalid unicode scalars or surrogates to replacement char U+FFFD *)
    if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then 0xFFFD
    else cp
  in
  if cp < 0x80 then write_char w (Char.chr cp)
  else if cp < 0x800 then (
    write_char w (Char.chr (0xC0 lor (cp lsr 6)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000 then (
    write_char w (Char.chr (0xE0 lor (cp lsr 12)));
    write_char w (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    write_char w (Char.chr (0xF0 lor (cp lsr 18)));
    write_char w (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    write_char w (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))

(* IO Utilities *)

let to_string (t : t) =
  (* Pass 1: counting *)
  let counter = Writer.make_counting () in
  t counter;
  let n = Writer.len counter in
  (* Fast path: return empty string literal without allocation *)
  if n = 0 then ""
  else
    (* Pass 2: actual write *)
    let bytes = Bytes.create n in
    let w = Writer.make bytes in
    t w;
    let actual = Writer.len w in
    if actual <> n then
      invalid_arg
        (Printf.sprintf
           "Escape.to_string: stateful sequence detected (expected %d bytes, \
            wrote %d)"
           n actual);
    Bytes.unsafe_to_string bytes

(* to_buffer: convenience wrapper via to_string (allocates intermediate
   string) *)
let to_buffer t buf = Buffer.add_string buf (to_string t)

(* CSI / SGR helpers *)

let esc body w =
  write_string w "\027[";
  write_string w body

let csi ~params ~command w =
  write_string w "\027[";
  write_string w params;
  write_char w command

let sgr codes w =
  match codes with
  | [] -> ()
  | first :: rest ->
      write_string w "\027[";
      add_int w first;
      let rec loop = function
        | [] -> ()
        | code :: rest ->
            write_char w ';';
            add_int w code;
            loop rest
      in
      loop rest;
      write_char w 'm'

(* sgr_direct uses writer position to track separator state without ref
   allocation *)
let sgr_direct write_codes w =
  write_string w "\027[";
  let start_pos = Writer.pos w in
  write_codes (fun code ->
      if Writer.pos w > start_pos then write_char w ';';
      add_int w code);
  write_char w 'm'

(* Low-level SGR building primitives for zero-allocation emission *)
let[@inline] sgr_open w = write_string w "\027["
let[@inline] sgr_code w n = add_int w n
let[@inline] sgr_sep w = write_char w ';'
let[@inline] sgr_close w = write_char w 'm'
let reset : t = sgr [ 0 ]
let clamp_nonneg v = max 0 v
let clamp_pos v = max 1 v
let clamp_byte v = max 0 (min 255 v)

(* CSI sequence helpers to reduce code duplication *)

(* CSI with single param, returns empty if n=0 *)
let[@inline] csi_n_opt cmd n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w cmd

(* CSI with single param, always emits *)
let[@inline] csi_n cmd n w =
  write_string w "\027[";
  add_int w n;
  write_char w cmd

(* CSI with two params *)
let[@inline] csi_nn cmd a b w =
  write_string w "\027[";
  add_int w a;
  write_char w ';';
  add_int w b;
  write_char w cmd

(* Cursor Control *)

let cursor_up ~n = csi_n_opt 'A' n
let cursor_down ~n = csi_n_opt 'B' n
let cursor_forward ~n = csi_n_opt 'C' n
let cursor_back ~n = csi_n_opt 'D' n
let cursor_next_line ~n = csi_n_opt 'E' n
let cursor_previous_line ~n = csi_n_opt 'F' n

let cursor_horizontal_absolute col w =
  let col = clamp_pos col in
  csi_n 'G' col w

let cursor_vertical_absolute row w =
  let row = clamp_pos row in
  csi_n 'd' row w

let cursor_position ~row ~col w =
  let row = clamp_pos row in
  let col = clamp_pos col in
  csi_nn 'H' row col w

let cursor_save : t = literal "\027[s"
let cursor_restore : t = literal "\027[u"

(* Cursor Appearance *)

type cursor_shape =
  [ `Default
  | `Blinking_block
  | `Block
  | `Blinking_underline
  | `Underline
  | `Blinking_bar
  | `Bar ]

let cursor_shape_to_int = function
  | `Default -> 0
  | `Blinking_block -> 1
  | `Block -> 2
  | `Blinking_underline -> 3
  | `Underline -> 4
  | `Blinking_bar -> 5
  | `Bar -> 6

let cursor_style ~shape w =
  write_string w "\027[";
  add_int w (cursor_shape_to_int shape);
  write_string w " q"

let cursor_color ~r ~g ~b w =
  write_string w "\027]12;#";
  add_hex2 w (clamp_byte r);
  add_hex2 w (clamp_byte g);
  add_hex2 w (clamp_byte b);
  write_terminator w `Bel

let reset_cursor_color : t = literal "\027]112\007"
let reset_cursor_color_fallback : t = literal "\027]12;default\007"

(* Screen Control *)

let clear : t = literal "\027[2J"
let home : t = literal "\027[H"
let clear_and_home : t = literal "\027[H\027[2J"

type erase_display_mode = [ `Below | `Above | `All | `Scrollback ]

let erase_display_mode_to_int = function
  | `Below -> 0
  | `Above -> 1
  | `All -> 2
  | `Scrollback -> 3

let erase_display ~mode w = csi_n 'J' (erase_display_mode_to_int mode) w
let erase_below_cursor : t = literal "\027[J"

type erase_line_mode = [ `Right | `Left | `All ]

let erase_line_mode_to_int = function `Right -> 0 | `Left -> 1 | `All -> 2
let erase_line ~mode w = csi_n 'K' (erase_line_mode_to_int mode) w
let insert_lines ~n = csi_n_opt 'L' n
let delete_lines ~n = csi_n_opt 'M' n
let scroll_up ~n = csi_n_opt 'S' n
let scroll_down ~n = csi_n_opt 'T' n

let set_scrolling_region ~top ~bottom w =
  if top < 1 || bottom <= top then
    invalid_arg "Escape.set_scrolling_region: invalid bounds";
  csi_nn 'r' top bottom w

let reset_scrolling_region : t = literal "\027[r"

(* Colors and Attributes *)

(* RGB color helper - combines consecutive writes *)
let[@inline] csi_rgb prefix r g b w =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  write_string w prefix;
  add_int w r;
  write_char w ';';
  add_int w g;
  write_char w ';';
  add_int w b;
  write_char w 'm'

let set_foreground ~r ~g ~b w = csi_rgb "\027[38;2;" r g b w
let set_background ~r ~g ~b w = csi_rgb "\027[48;2;" r g b w
let reset_background : t = literal "\027[49m"
let reset_foreground : t = literal "\027[39m"

(* Terminal Properties *)

let set_title ~title w =
  write_string w "\027]0;";
  write_string w title;
  write_terminator w `St

let[@inline] write_explicit_width_prefix w width =
  write_string w "\027]66;w=";
  add_int w width;
  write_char w ';'

let explicit_width ~width ~text w =
  write_explicit_width_prefix w width;
  write_string w text;
  write_terminator w `St

let explicit_width_bytes ~width ~bytes ~off ~len w =
  write_explicit_width_prefix w width;
  write_subbytes w bytes off len;
  write_terminator w `St

(* OSC helpers *)

let osc ?(terminator = `St) ~payload w =
  write_string w "\027]";
  write_string w payload;
  write_terminator w terminator

(* Hyperlinks (OSC 8) *)

let hyperlink_start ?(params = "") ~url w =
  write_string w "\027]8;";
  if params <> "" then write_string w params;
  write_char w ';';
  write_string w url;
  write_terminator w `St

let hyperlink_end : t = osc ~terminator:`St ~payload:"8;;"

let hyperlink ?(params = "") ~url ~text w =
  hyperlink_start ~params ~url w;
  write_string w text;
  hyperlink_end w

let[@inline] hyperlink_open w url =
  write_string w "\027]8;;";
  write_string w url;
  write_terminator w `St

let[@inline] hyperlink_close w =
  write_string w "\027]8;;";
  write_terminator w `St

(* Terminal Modes *)

type mode =
  | Cursor_visible
  | Mouse_tracking
  | Mouse_button_tracking
  | Mouse_motion
  | Mouse_sgr
  | Mouse_sgr_pixel
  | Mouse_x10
  | Urxvt_mouse
  | Alternate_screen
  | Focus_tracking
  | Bracketed_paste
  | Sync_output
  | Unicode
  | Color_scheme

let mode_code = function
  | Cursor_visible -> 25
  | Mouse_tracking -> 1000
  | Mouse_button_tracking -> 1002
  | Mouse_motion -> 1003
  | Mouse_sgr -> 1006
  | Mouse_sgr_pixel -> 1016
  | Mouse_x10 -> 9
  | Urxvt_mouse -> 1015
  | Alternate_screen -> 1049
  | Focus_tracking -> 1004
  | Bracketed_paste -> 2004
  | Sync_output -> 2026
  | Unicode -> 2027
  | Color_scheme -> 2031

let enable mode w =
  write_string w "\027[?";
  add_int w (mode_code mode);
  write_char w 'h'

let disable mode w =
  write_string w "\027[?";
  add_int w (mode_code mode);
  write_char w 'l'

(* Key Encoding *)

let csi_u_on : t = literal "\027[>1u"
let csi_u_off : t = literal "\027[<1u"

let csi_u_push ~flags w =
  write_string w "\027[>";
  add_int w flags;
  write_char w 'u'

let csi_u_pop : t = literal "\027[<u"
let modify_other_keys_on : t = literal "\027[>4;1m"
let modify_other_keys_off : t = literal "\027[>4;0m"

(* Device and Capability Queries *)

type query =
  | Cursor_position
  | Pixel_size
  | Device_attributes
  | Tertiary_attributes
  | Terminal_identity
  | Device_status
  | Csi_u_support
  | Kitty_graphics
  | Sixel_geometry
  | Explicit_width_support
  | Scaled_text_support
  | Color_scheme_query
  | Focus_mode
  | Sgr_pixels_mode
  | Bracketed_paste_mode
  | Sync_mode
  | Unicode_mode
  | Color_scheme_mode

let query = function
  | Cursor_position -> literal "\027[6n"
  | Pixel_size -> literal "\027[14t"
  | Device_attributes -> literal "\027[c"
  | Tertiary_attributes -> literal "\027[=c"
  | Terminal_identity -> literal "\027[>0q"
  | Device_status -> literal "\027[5n"
  | Csi_u_support -> literal "\027[?u"
  | Kitty_graphics ->
      literal "\027_Gi=31337,s=1,v=1,a=q,t=d,f=24;AAAA\027\\\027[c"
  | Sixel_geometry -> literal "\027[?2;1;0S"
  | Explicit_width_support -> literal "\027]66;w=1; \027\\"
  | Scaled_text_support -> literal "\027]66;s=2; \027\\"
  | Color_scheme_query -> literal "\027[?996n"
  | Focus_mode -> literal "\027[?1004$p"
  | Sgr_pixels_mode -> literal "\027[?1016$p"
  | Bracketed_paste_mode -> literal "\027[?2004$p"
  | Sync_mode -> literal "\027[?2026$p"
  | Unicode_mode -> literal "\027[?2027$p"
  | Color_scheme_mode -> literal "\027[?2031$p"
