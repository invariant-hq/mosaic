module P = Ansi.Parser
module Params = P.Params
module K = Keystroke

type event =
  | Key of Keystroke.t
  | Key_release of Keystroke.release
  | Text of string
  | Mouse of Mouse_report.t
  | Focus_in
  | Focus_out
  | Paste_start
  | Paste_data of string
  | Paste_end of [ `Complete | `Cut ]
  | Invalid of string
  | Reply of Reply.t
  | Cut of [ `Sequence | `String | `Mouse ]
  | Too_long of [ `Dcs | `Osc | `Apc ]

let pp_event ppf = function
  | Key k -> Format.fprintf ppf "Key %a" K.pp k
  | Key_release k -> Format.fprintf ppf "Key_release %a" K.pp_release k
  | Text s -> Format.fprintf ppf "Text %S" s
  | Mouse m -> Format.fprintf ppf "Mouse %a" Mouse_report.pp m
  | Focus_in -> Format.pp_print_string ppf "Focus_in"
  | Focus_out -> Format.pp_print_string ppf "Focus_out"
  | Paste_start -> Format.pp_print_string ppf "Paste_start"
  | Paste_data s -> Format.fprintf ppf "Paste_data %S" s
  | Paste_end `Complete -> Format.pp_print_string ppf "Paste_end"
  | Paste_end `Cut -> Format.pp_print_string ppf "Paste_end cut"
  | Invalid s -> Format.fprintf ppf "Invalid %S" s
  | Reply r -> Format.fprintf ppf "Reply %a" Reply.pp r
  | Cut `Sequence -> Format.pp_print_string ppf "Cut sequence"
  | Cut `String -> Format.pp_print_string ppf "Cut string"
  | Cut `Mouse -> Format.pp_print_string ppf "Cut mouse"
  | Too_long `Dcs -> Format.pp_print_string ppf "Too_long DCS"
  | Too_long `Osc -> Format.pp_print_string ppf "Too_long OSC"
  | Too_long `Apc -> Format.pp_print_string ppf "Too_long APC"

type pending = Nothing | Ambiguous | Sequence
type owed = { cursor_reports : int; string_replies : int }

(* The memory budget of a control string's content. *)
let budget = 16 * 1024 * 1024
let paste_end = "\x1b[201~"

(* Keys *)

(* Modifiers from their bits, shared so that a key allocates none. *)
let mods_of_bits =
  Array.init 256 (fun b ->
      let bit n = b land n <> 0 in
      {
        K.shift = bit 1;
        alt = bit 2;
        ctrl = bit 4;
        super = bit 8;
        hyper = bit 16;
        meta = bit 32;
        caps_lock = bit 64;
        num_lock = bit 128;
      })

let shift_bit = 1
let alt_bit = 2
let ctrl_bit = 4

(* A scalar value that may be text: not a C0 or C1 control, not DEL. *)
let is_text c = Uchar.is_valid c && c >= 0x20 && (c < 0x7F || c > 0x9F)

let utf_8 u =
  let b = Bytes.create (Uchar.utf_8_byte_length u) in
  ignore (Bytes.set_utf_8_uchar b 0 u : int);
  Bytes.unsafe_to_string b

(* A chord types nothing. *)
let types_nothing bits = bits land 0b111110 <> 0

(* A key with the event type [ev] of the kitty protocol: 1 press, 2 repeat,
   3 release. A release types nothing. *)
let stroke ~text ~base ~keypad key bits ev =
  let mods = mods_of_bits.(bits) in
  if ev = 3 then Key_release { key; mods; base; keypad }
  else Key { key; mods; text; base; keypad; repeat = ev = 2 }

let press key bits = stroke ~text:"" ~base:None ~keypad:false key bits 1

let char_key ~keypad c bits =
  let u = Uchar.of_int c in
  let text = if types_nothing bits || not (is_text c) then "" else utf_8 u in
  stroke ~text ~base:None ~keypad (Char u) bits 1

(* The key of each ASCII byte typed, and the same after an ESC (Alt). A C0
   control is Ctrl and a key (kitty's legacy ctrl mapping). *)
let ascii_key alt b =
  let bits = if alt then alt_bit else 0 in
  match b with
  | 0x08 | 0x7F -> press Backspace bits
  | 0x09 -> press Tab bits
  | 0x0D -> press Enter bits
  | 0x1B -> press Escape bits
  | 0x00 -> char_key ~keypad:false 0x20 (bits lor ctrl_bit)
  | b when b <= 0x1A -> char_key ~keypad:false (b + 0x60) (bits lor ctrl_bit)
  | b when b <= 0x1F -> char_key ~keypad:false (b + 0x40) (bits lor ctrl_bit)
  | b -> char_key ~keypad:false b bits

let ascii = Array.init 128 (ascii_key false)
let alt_ascii = Array.init 128 (ascii_key true)
let escape = ascii.(0x1B)

(* A key with Alt added, as an ESC before it says. *)
let with_alt = function
  | Key k -> Key { k with mods = { k.mods with alt = true }; text = "" }
  | Key_release k -> Key_release { k with mods = { k.mods with alt = true } }
  | e -> e

(* xterm's numbered keys, [CSI n ~]. *)
let numbered = function
  | 1 | 7 -> Some K.Home
  | 2 -> Some K.Insert
  | 3 -> Some K.Delete
  | 4 | 8 -> Some K.End
  | 5 -> Some K.Page_up
  | 6 -> Some K.Page_down
  | (11 | 12 | 13 | 14 | 15) as n -> Some (K.F (n - 10))
  | (17 | 18 | 19 | 20 | 21) as n -> Some (K.F (n - 11))
  | (23 | 24 | 25 | 26) as n -> Some (K.F (n - 12))
  | (28 | 29) as n -> Some (K.F (n - 13))
  | (31 | 32 | 33 | 34) as n -> Some (K.F (n - 14))
  | _ -> None

(* The keypad keys' main-keyboard equivalents. *)
let keypad_char c = (K.Char (Uchar.of_char c), true)

(* The kitty protocol's functional keys, 57344..63743, with whether each is on
   the keypad. *)
let functional c : K.key * bool =
  match c with
  | 57344 -> (Escape, false)
  | 57345 -> (Enter, false)
  | 57346 -> (Tab, false)
  | 57347 -> (Backspace, false)
  | 57348 -> (Insert, false)
  | 57349 -> (Delete, false)
  | 57350 -> (Left, false)
  | 57351 -> (Right, false)
  | 57352 -> (Up, false)
  | 57353 -> (Down, false)
  | 57354 -> (Page_up, false)
  | 57355 -> (Page_down, false)
  | 57356 -> (Home, false)
  | 57357 -> (End, false)
  | 57358 -> (Caps_lock, false)
  | 57359 -> (Scroll_lock, false)
  | 57360 -> (Num_lock, false)
  | 57361 -> (Print_screen, false)
  | 57362 -> (Pause, false)
  | 57363 -> (Menu, false)
  | c when c >= 57364 && c <= 57398 -> (F (c - 57363), false)
  | c when c >= 57399 && c <= 57408 -> keypad_char (Char.chr (c - 57399 + 0x30))
  | 57409 -> keypad_char '.'
  | 57410 -> keypad_char '/'
  | 57411 -> keypad_char '*'
  | 57412 -> keypad_char '-'
  | 57413 -> keypad_char '+'
  | 57414 -> (Enter, true)
  | 57415 -> keypad_char '='
  | 57416 -> keypad_char ','
  | 57417 -> (Left, true)
  | 57418 -> (Right, true)
  | 57419 -> (Up, true)
  | 57420 -> (Down, true)
  | 57421 -> (Page_up, true)
  | 57422 -> (Page_down, true)
  | 57423 -> (Home, true)
  | 57424 -> (End, true)
  | 57425 -> (Insert, true)
  | 57426 -> (Delete, true)
  | 57427 -> (Begin, true)
  | 57428 -> (Media Play, false)
  | 57429 -> (Media Pause, false)
  | 57430 -> (Media Play_pause, false)
  | 57431 -> (Media Reverse, false)
  | 57432 -> (Media Stop, false)
  | 57433 -> (Media Fast_forward, false)
  | 57434 -> (Media Rewind, false)
  | 57435 -> (Media Track_next, false)
  | 57436 -> (Media Track_previous, false)
  | 57437 -> (Media Record, false)
  | 57438 -> (Media Volume_down, false)
  | 57439 -> (Media Volume_up, false)
  | 57440 -> (Media Mute, false)
  | 57441 -> (Modifier Left_shift, false)
  | 57442 -> (Modifier Left_ctrl, false)
  | 57443 -> (Modifier Left_alt, false)
  | 57444 -> (Modifier Left_super, false)
  | 57445 -> (Modifier Left_hyper, false)
  | 57446 -> (Modifier Left_meta, false)
  | 57447 -> (Modifier Right_shift, false)
  | 57448 -> (Modifier Right_ctrl, false)
  | 57449 -> (Modifier Right_alt, false)
  | 57450 -> (Modifier Right_super, false)
  | 57451 -> (Modifier Right_hyper, false)
  | 57452 -> (Modifier Right_meta, false)
  | 57453 -> (Modifier Level3_shift, false)
  | 57454 -> (Modifier Level5_shift, false)
  | c -> (Unidentified c, false)

let is_functional c = c >= 57344 && c <= 63743

(* The modifiers and event type of parameter [i], [m:e], packed as
   [bits lor (e lsl 8)]; -1 when out of the kitty protocol's range (m is 1 +
   bits, 1..256; e is 1..3). Absent means 1. Fields after [e] are ignored. *)
let mods_event p i =
  let m = Params.ps p i ~default:1 in
  let e =
    if Params.fields p i < 2 then 1
    else match Params.field p i 1 with None -> 1 | Some e -> e
  in
  if m < 1 || m > 256 || e < 1 || e > 3 then -1 else (m - 1) lor (e lsl 8)

(* Mouse *)

(* A report's button code [cb], without X10's offset, at 0-based [x], [y]. *)
let mouse cb x y ~released : Mouse_report.t option =
  let low = cb land 3 in
  let wheel = cb land 64 <> 0 and extra = cb land 128 <> 0 in
  if cb < 0 || cb > 255 || (wheel && extra) then None
  else
    let button : Mouse_report.button option =
      if extra then Some (Extra (8 + low))
      else
        match low with
        | 0 -> Some Left
        | 1 -> Some Middle
        | 2 -> Some Right
        | _ -> None
    in
    let kind : Mouse_report.kind =
      if wheel then
        Scroll
          (match low with 0 -> `Up | 1 -> `Down | 2 -> `Left | _ -> `Right)
      else if cb land 32 <> 0 then Motion button
      else if released then Release button
      else match button with Some b -> Press b | None -> Release None
    in
    Some
      {
        kind;
        x = max 0 x;
        y = max 0 y;
        shift = cb land 4 <> 0;
        alt = cb land 8 <> 0;
        ctrl = cb land 16 <> 0;
      }

(* Decoders *)

type state =
  | Ground
  | Esc of bool (* ESC held; [true] when an earlier ESC prefixes it *)
  | Intro of bool * char (* ESC and an introducer held; prefixed *)
  | Header of bool * bool (* in a CSI ([false]) or SS3 header; prefixed *)
  | X10 of P.sequence * bool (* the raw bytes after CSI M; prefixed *)
  | Linux of P.sequence * bool (* after CSI [, before the console's letter *)
  | String of char (* in a control string, by its introducer *)
  | String_esc of char (* ESC in a control string *)
  | Paste of int (* in a paste, with the end marker's bytes matched *)

type t = {
  p : P.t;
  mutable st : state;
  one : Bytes.t;
  mutable token : P.token option; (* the last token the parser gave *)
  (* A UTF-8 character in progress, its length, and whether an ESC came
     before it. *)
  u8 : Bytes.t;
  mutable u8_len : int;
  mutable u8_need : int;
  mutable u8_alt : bool;
  (* rxvt's [CSI n $]: the number so far, -1 once the header holds anything
     but digits, and whether it has any digit. *)
  mutable rx : int;
  mutable rx_digits : bool;
  (* The X10 report's bytes. *)
  x10 : Bytes.t;
  mutable x10_len : int;
  (* The open string's content, whether it went over the budget, and a DCS
     header. *)
  data : Buffer.t;
  mutable too_long : bool;
  mutable dcs : P.sequence option;
  mutable cursor_reports : int;
  mutable string_replies : int;
  mutable on_token : P.token -> unit;
  (* Whether [feed], [idle], [finish] or [cut_paste] is running, and so may be
     calling back. *)
  mutable decoding : bool;
}

let create () =
  let d =
    {
      p = P.create ();
      st = Ground;
      one = Bytes.create 1;
      token = None;
      u8 = Bytes.create 4;
      u8_len = 0;
      u8_need = 0;
      u8_alt = false;
      rx = -1;
      rx_digits = false;
      x10 = Bytes.create 3;
      x10_len = 0;
      data = Buffer.create 256;
      too_long = false;
      dcs = None;
      cursor_reports = 0;
      string_replies = 0;
      on_token = ignore;
      decoding = false;
    }
  in
  d.on_token <-
    (function
    | P.Data s ->
        if not d.too_long then
          if Buffer.length d.data + String.length s <= budget then
            Buffer.add_string d.data s
          else (
            d.too_long <- true;
            Buffer.reset d.data)
    | Open (Dcs h) -> d.dcs <- Some h
    | t -> d.token <- Some t);
  d

let forward d c =
  Bytes.unsafe_set d.one 0 c;
  d.token <- None;
  ignore (P.feed d.p d.one ~off:0 ~len:1 d.on_token : int)

(* The token the last byte forwarded gave, which is then forgotten. *)
let take d =
  let t = d.token in
  d.token <- None;
  t

(* A callback runs in the middle of a step, so what changes the state must
   wait until the decoding function that called it has returned. *)
let check_free d name =
  if d.decoding then
    invalid_arg
      ("Matrix_input.Decoder." ^ name ^ ": called from the decoder's callback")

let drop d =
  P.reset d.p;
  d.st <- Ground;
  d.token <- None;
  d.u8_len <- 0;
  d.u8_alt <- false;
  d.x10_len <- 0;
  Buffer.reset d.data;
  d.too_long <- false;
  d.dcs <- None

(* What a prefixed sequence decodes to: Alt and its key, or, when it is no
   key, the Escape key typed before it. *)
let prefixed pre ev emit =
  if not pre then emit ev
  else
    match ev with
    | Key _ | Key_release _ -> emit (with_alt ev)
    | ev ->
        emit escape;
        emit ev

let reply pre r emit = prefixed pre (Reply r) emit

(* The text of a kitty key, its third parameter, when it is one scalar. *)
let text_scalar p =
  if Params.count p < 3 || Params.fields p 2 <> 1 then None
  else match Params.value p 2 with Some c when is_text c -> Some c | _ -> None

(* The kitty protocol's [CSI code:shifted:base ; m:e ; text u], and
   xterm's [CSI code ; m u] form of modifyOtherKeys. *)
let kitty (s : P.sequence) pre emit =
  let p = s.params in
  let me = mods_event p 1 in
  let shifted = if Params.fields p 0 > 1 then Params.field p 0 1 else None in
  let base = if Params.fields p 0 > 2 then Params.field p 0 2 else None in
  (* The associated text: code points, each text. *)
  let text =
    if Params.count p < 3 then Some ""
    else
      let n = Params.fields p 2 in
      if n = 1 && Params.value p 2 = None then Some ""
      else
        let b = Buffer.create 8 in
        let rec go j =
          if j >= n then Some (Buffer.contents b)
          else
            match Params.field p 2 j with
            | Some c when is_text c ->
                Buffer.add_utf_8_uchar b (Uchar.of_int c);
                go (j + 1)
            | _ -> None
        in
        go 0
  in
  let valid = function None -> true | Some c -> Uchar.is_valid c in
  match (Params.value p 0, text) with
  | None, _ | _, None -> reply pre (Csi s) emit
  | Some _, _ when me < 0 || not (valid shifted && valid base) ->
      reply pre (Csi s) emit
  | Some 0, Some text ->
      (* Text without a key. A release of it says nothing. *)
      if text = "" then reply pre (Csi s) emit
      else if me lsr 8 <> 3 then prefixed pre (Text text) emit
  | Some code, Some text -> (
      let bits = me land 0xFF and ev = me lsr 8 in
      let base = Option.map Uchar.of_int base in
      let key =
        match code with
        | 9 -> Some (K.Tab, false)
        | 13 -> Some (K.Enter, false)
        | 27 -> Some (K.Escape, false)
        | 8 | 127 -> Some (K.Backspace, false)
        | c when is_functional c -> Some (functional c)
        | c when is_text c -> Some (K.Char (Uchar.of_int c), false)
        | _ -> None
      in
      match key with
      | None -> reply pre (Csi s) emit
      | Some (key, keypad) ->
          (* The character typed, when the text or the shifted key says it.
             The text is the character with the locks applied, and Shift is
             consumed when it changed the character. A chord types nothing,
             so its key is the shifted one. *)
          let typed = if types_nothing bits then None else text_scalar p in
          let key, bits =
            match (key, typed, shifted) with
            | Char _, Some t, _ ->
                let changed =
                  t <> code
                  || match shifted with Some sh -> sh <> code | None -> false
                in
                ( K.Char (Uchar.of_int t),
                  if changed then bits land lnot shift_bit else bits )
            | Char _, None, Some sh when bits land shift_bit <> 0 && is_text sh
              ->
                (K.Char (Uchar.of_int sh), bits land lnot shift_bit)
            | _ -> (key, bits)
          in
          let text =
            match key with
            | _ when types_nothing bits -> ""
            | _ when text <> "" -> text
            | Char u when bits land lnot 128 = 0 && is_text (Uchar.to_int u) ->
                (* Num Lock changes no character: the keypad's keys are
                   distinct. *)
                utf_8 u
            | _ -> ""
          in
          prefixed pre (stroke ~text ~base ~keypad key bits ev) emit)

(* xterm's modifyOtherKeys, [CSI 27 ; m ; code ~]: the code is the character
   typed, so Shift is consumed, except on Space, which Shift does not
   change. *)
let modify_other_keys (s : P.sequence) pre emit =
  let p = s.params in
  let me = mods_event p 1 in
  let code = Params.ps p 2 ~default:(-1) in
  let bits = me land 0xFF in
  let key =
    if me < 0 || me lsr 8 <> 1 then None
    else
      match code with
      | 9 -> Some (press Tab bits)
      | 13 -> Some (press Enter bits)
      | 27 -> Some (press Escape bits)
      | 8 | 127 -> Some (press Backspace bits)
      | 32 -> Some (char_key ~keypad:false 32 bits)
      | c when is_text c ->
          Some (char_key ~keypad:false c (bits land lnot shift_bit))
      | _ -> None
  in
  match key with
  | Some k -> prefixed pre k emit
  | None -> reply pre (Csi s) emit

let set_mode dec mode setting : Reply.t option =
  let setting : Reply.setting option =
    match setting with
    | Some 0 -> Some Not_recognized
    | Some 1 -> Some Set
    | Some 2 -> Some Reset
    | Some 3 -> Some Permanently_set
    | Some 4 -> Some Permanently_reset
    | _ -> None
  in
  match (mode, setting) with
  | Some mode, Some setting -> Some (Mode { dec; mode; setting })
  | _ -> None

let ints p = List.init (Params.count p) (fun i -> Params.ps p i ~default:0)

(* A key with a modifier parameter: [CSI 1 ; m:e F] for a [letter] key,
   [CSI n ; m:e ~] for a numbered one, with [add] to the modifiers. *)
let modified (s : P.sequence) ~letter ~add ~keypad key pre emit =
  let p = s.params in
  let me = mods_event p 1 in
  if Params.count p > 2 || (letter && Params.pn p 0 ~default:1 <> 1) || me < 0
  then reply pre (Csi s) emit
  else
    let bits = me land 0xFF lor add in
    prefixed pre (stroke ~text:"" ~base:None ~keypad key bits (me lsr 8)) emit

let letter s key pre emit =
  modified s ~letter:true ~add:0 ~keypad:false key pre emit

let mouse_report (s : P.sequence) cb x y ~released pre emit =
  match mouse cb x y ~released with
  | Some m -> prefixed pre (Mouse m) emit
  | None -> reply pre (Csi s) emit

(* A complete control sequence. *)
let csi d (s : P.sequence) pre emit =
  let p = s.params in
  let n = Params.count p in
  match (s.marker, s.intermediates, s.final) with
  | None, "", 'A' -> letter s Up pre emit
  | None, "", 'B' -> letter s Down pre emit
  | None, "", 'C' -> letter s Right pre emit
  | None, "", 'D' -> letter s Left pre emit
  | None, "", 'E' -> modified s ~letter:true ~add:0 ~keypad:true Begin pre emit
  | None, "", 'F' -> letter s End pre emit
  | None, "", 'H' -> letter s Home pre emit
  | None, "", 'P' -> letter s (F 1) pre emit
  | None, "", 'Q' -> letter s (F 2) pre emit
  | None, "", 'S' -> letter s (F 4) pre emit
  | None, "", 'Z' ->
      modified s ~letter:true ~add:shift_bit ~keypad:false Tab pre emit
  | None, "", 'R' ->
      let f3 = n = 0 || (n <= 2 && Params.pn p 0 ~default:1 = 1) in
      if d.cursor_reports = 0 && f3 then letter s (F 3) pre emit
      else if n <= 2 then
        reply pre
          (Cursor
             {
               row = Params.pn p 0 ~default:1 - 1;
               col = Params.pn p 1 ~default:1 - 1;
               page = None;
             })
          emit
      else reply pre (Csi s) emit
  | None, "", 'n' when n = 1 -> (
      match Params.value p 0 with
      | Some 0 -> reply pre (Operating_status `Ok) emit
      | Some 3 -> reply pre (Operating_status `Malfunction) emit
      | _ -> reply pre (Csi s) emit)
  | None, "", '~' -> (
      match Params.value p 0 with
      | Some 27 when n = 3 -> modify_other_keys s pre emit
      | Some 200 when n = 1 ->
          if pre then emit escape;
          emit Paste_start;
          d.st <- Paste 0
      | Some k when n <= 2 -> (
          match numbered k with
          | Some key ->
              modified s ~letter:false ~add:0 ~keypad:false key pre emit
          | None when is_functional k ->
              let key, keypad = functional k in
              modified s ~letter:false ~add:0 ~keypad key pre emit
          | None -> reply pre (Csi s) emit)
      | _ -> reply pre (Csi s) emit)
  | None, "", (('^' | '@') as f) when n = 1 -> (
      (* rxvt: Ctrl, and Ctrl+Shift, with a numbered key. *)
      match Option.bind (Params.value p 0) numbered with
      | Some key ->
          let bits = if f = '^' then ctrl_bit else ctrl_bit lor shift_bit in
          prefixed pre (press key bits) emit
      | None -> reply pre (Csi s) emit)
  | None, "", (('a' | 'b' | 'c' | 'd' | 'e') as f) when n = 0 ->
      (* rxvt: Shift with an arrow or Begin. *)
      let key : K.key =
        match f with
        | 'a' -> Up
        | 'b' -> Down
        | 'c' -> Right
        | 'd' -> Left
        | _ -> Begin
      in
      let keypad = f = 'e' in
      prefixed pre (stroke ~text:"" ~base:None ~keypad key shift_bit 1) emit
  | None, "", 'u' -> kitty s pre emit
  | None, "", 'I' when n = 0 -> prefixed pre Focus_in emit
  | None, "", 'O' when n = 0 -> prefixed pre Focus_out emit
  | None, "", 'M' when n = 0 ->
      d.x10_len <- 0;
      d.st <- X10 (s, pre)
  | None, "", 'M' when n = 3 ->
      (* URXVT: X10's code in decimal. *)
      mouse_report s
        (Params.ps p 0 ~default:0 - 32)
        (Params.pn p 1 ~default:1 - 1)
        (Params.pn p 2 ~default:1 - 1)
        ~released:false pre emit
  | None, "", '[' when n = 0 -> d.st <- Linux (s, pre)
  | None, "", 't' -> (
      let v i = Params.ps p i ~default:0 in
      match (Params.value p 0, n) with
      | Some 4, 3 ->
          reply pre (Text_area_pixels { height = v 1; width = v 2 }) emit
      | Some 6, 3 -> reply pre (Cell_pixels { height = v 1; width = v 2 }) emit
      | Some 8, 3 -> reply pre (Text_area { rows = v 1; cols = v 2 }) emit
      | Some 48, 5 ->
          reply pre
            (Resize { rows = v 1; cols = v 2; height = v 3; width = v 4 })
            emit
      | _ -> reply pre (Csi s) emit)
  | Some '<', "", (('M' | 'm') as f) ->
      let one_field i = Params.fields p i = 1 in
      if n <> 3 || not (one_field 0 && one_field 1 && one_field 2) then
        reply pre (Csi s) emit
      else
        mouse_report s (Params.ps p 0 ~default:0)
          (Params.pn p 1 ~default:1 - 1)
          (Params.pn p 2 ~default:1 - 1)
          ~released:(f = 'm') pre emit
  | Some '?', "", 'c' -> reply pre (Primary_attributes (ints p)) emit
  | Some '>', "", 'c' -> reply pre (Secondary_attributes (ints p)) emit
  | (Some '?' | None), "$", 'y' when n >= 2 && n mod 2 = 0 -> (
      let dec = s.marker <> None in
      let rec modes i acc =
        if i >= n then Some (List.rev acc)
        else
          match set_mode dec (Params.value p i) (Params.value p (i + 1)) with
          | Some m -> modes (i + 2) (m :: acc)
          | None -> None
      in
      match modes 0 [] with
      | Some (m :: ms) ->
          reply pre m emit;
          List.iter (fun m -> emit (Reply m)) ms
      | _ -> reply pre (Csi s) emit)
  | Some '?', "", 'R' when n = 2 || n = 3 ->
      reply pre
        (Cursor
           {
             row = Params.pn p 0 ~default:1 - 1;
             col = Params.pn p 1 ~default:1 - 1;
             page = Some (Params.pn p 2 ~default:1);
           })
        emit
  | Some '?', "", 'u' when n <= 1 ->
      reply pre (Keyboard_flags (Params.ps p 0 ~default:0)) emit
  | Some '?', "", 'n' when n = 2 && Params.value p 0 = Some 997 -> (
      match Params.value p 1 with
      | Some 1 -> reply pre (Color_scheme `Dark) emit
      | Some 2 -> reply pre (Color_scheme `Light) emit
      | _ -> reply pre (Csi s) emit)
  | _ -> reply pre (Csi s) emit

(* An SS3 sequence, read as the control sequence its bytes make after CSI:
   xterm's cursor, function and application keypad keys, with a modifier
   parameter in older forms ([SS3 2 P], [SS3 1 ; 2 P]), and rxvt's Ctrl with
   an arrow. *)
let ss3 (s : P.sequence) pre emit =
  let p = s.params in
  let n = Params.count p in
  let me =
    if s.marker <> None || s.intermediates <> "" then -1
    else if n = 0 then 1 lsl 8
    else if n = 1 then mods_event p 0
    else if n = 2 && Params.pn p 0 ~default:1 = 1 then mods_event p 1
    else -1
  in
  let bits = me land 0xFF and ev = me lsr 8 in
  let key ?(add = 0) ?(keypad = false) k =
    prefixed pre (stroke ~text:"" ~base:None ~keypad k (bits lor add) ev) emit
  in
  if me < 0 then reply pre (Ss3 s) emit
  else
    match s.final with
    | 'A' -> key Up
    | 'B' -> key Down
    | 'C' -> key Right
    | 'D' -> key Left
    | 'E' -> key ~keypad:true Begin
    | 'F' -> key End
    | 'H' -> key Home
    | 'P' -> key (F 1)
    | 'Q' -> key (F 2)
    | 'R' -> key (F 3)
    | 'S' -> key (F 4)
    | 'I' -> key ~keypad:true Tab
    | 'M' -> key ~keypad:true Enter
    | ('X' | 'j' .. 'y') as f ->
        (* The application keypad's characters. *)
        let c =
          if f = 'X' then '=' else "*+,-./0123456789".[Char.code f - 0x6A]
        in
        let text = if types_nothing bits then "" else String.make 1 c in
        prefixed pre
          (stroke ~text ~base:None ~keypad:true
             (Char (Uchar.of_char c))
             bits ev)
          emit
    | 'a' -> key ~add:ctrl_bit Up
    | 'b' -> key ~add:ctrl_bit Down
    | 'c' -> key ~add:ctrl_bit Right
    | 'd' -> key ~add:ctrl_bit Left
    | 'e' -> key ~add:ctrl_bit ~keypad:true Begin
    | _ -> reply pre (Ss3 s) emit

(* Strings *)

let string_end d intro ending emit =
  let content = Buffer.contents d.data in
  let too_long = d.too_long in
  Buffer.reset d.data;
  d.too_long <- false;
  let dcs = d.dcs in
  d.dcs <- None;
  match ending with
  | `Aborted -> emit (Cut `String)
  | (`St | `Bel) as ending -> (
      match intro with
      | ']' ->
          emit
            (if too_long then Too_long `Osc
             else Reply (Reply.of_osc ending content))
      | '_' ->
          emit
            (if too_long then Too_long `Apc
             else
               (* The decoder begins no APC string but G's. *)
               Reply
                 (Reply.graphics
                    (String.sub content 1 (String.length content - 1))))
      | _ -> (
          (* A malformed DCS has no effect. *)
          match dcs with
          | Some h ->
              emit
                (if too_long then Too_long `Dcs
                 else Reply (Reply.of_dcs h content))
          | None -> ()))

(* After a byte forwarded inside a string: the string ended if the parser
   closed it, or reported the CAN or SUB that aborted it. *)
let string_byte_done d intro emit =
  match take d with
  | Some (Close e) ->
      d.st <- Ground;
      string_end d intro e emit
  | Some (Control _) ->
      d.st <- Ground;
      string_end d intro `Aborted emit
  | _ -> ()

(* The framer *)

(* The bytes an SS3 sequence may continue with: parameters, and the final
   bytes of SS3 keys. *)
let ss3_commits b =
  match Char.unsafe_chr b with
  | '0' .. '9'
  | ';' | ' '
  | 'A' .. 'F'
  | 'H' | 'I' | 'M'
  | 'P' .. 'S'
  | 'X'
  | 'a' .. 'e'
  | 'j' .. 'y' ->
      true
  | _ -> false

let commits intro b =
  match intro with
  | '[' -> b >= 0x20 && b <= 0x7E
  | 'O' -> ss3_commits b
  | 'P' -> b >= 0x20 && b <= 0x3F
  | ']' -> (b >= 0x30 && b <= 0x39) || b = 0x4C || b = 0x6C
  | _ -> b = 0x47

(* rxvt's [CSI n $] after a header byte [c]: the number so far, or -1 once
   the header holds anything but digits. *)
let rxvt_byte d c =
  if d.rx >= 0 then
    if c >= 0x30 && c <= 0x39 then (
      d.rx_digits <- true;
      d.rx <- (if d.rx > 100_000 then d.rx else (d.rx * 10) + c - 0x30))
    else d.rx <- -1

let csi_bytes = Bytes.of_string "\x1b["

(* The parser, at the start of a control sequence's header. *)
let open_header d =
  P.reset d.p;
  ignore (P.feed d.p csi_bytes ~off:0 ~len:2 ignore : int)

let flush_invalid d emit =
  let s = Bytes.sub_string d.u8 0 d.u8_len in
  d.u8_len <- 0;
  if d.u8_alt then emit escape;
  d.u8_alt <- false;
  emit (Invalid s)

(* An introducer that begins nothing is a key: Alt and the introducer, or,
   after ESC ESC, Alt+Escape and the introducer, as ESC ESC before any other
   byte is. *)
let intro_keys pre intro emit =
  if pre then (
    emit alt_ascii.(0x1B);
    emit ascii.(Char.code intro))
  else emit alt_ascii.(Char.code intro)

let rec step d b emit =
  match d.st with
  | Ground -> ground d b emit
  | Esc pre ->
      if b = 0x1B then
        if pre then (
          emit alt_ascii.(0x1B);
          d.st <- Esc false)
        else d.st <- Esc true
      else if b = 0x5B || b = 0x4F then d.st <- Intro (pre, Char.unsafe_chr b)
      else if (b = 0x50 || b = 0x5D || b = 0x5F) && d.string_replies > 0 then
        d.st <- Intro (pre, Char.unsafe_chr b)
      else if pre then (
        emit alt_ascii.(0x1B);
        d.st <- Ground;
        ground d b emit)
      else (
        d.st <- Ground;
        if b < 0x80 then emit alt_ascii.(b)
        else (
          d.u8_alt <- true;
          ground d b emit))
  | Intro (pre, intro) ->
      if intro = 'O' && b = 0x20 then (
        (* SS3 SP: the keypad's space. *)
        d.st <- Ground;
        prefixed pre (char_key ~keypad:true 0x20 0) emit)
      else if commits intro b then
        if intro = '[' || intro = 'O' then (
          open_header d;
          d.rx <- (if intro = '[' then 0 else -1);
          d.rx_digits <- false;
          d.st <- Header (intro = 'O', pre);
          header d (intro = 'O') pre b emit)
        else (
          if pre then emit escape;
          P.reset d.p;
          forward d '\x1b';
          forward d intro;
          d.st <- String intro;
          forward d (Char.unsafe_chr b);
          string_byte_done d intro emit)
      else (
        d.st <- Ground;
        intro_keys pre intro emit;
        step d b emit)
  | Header (ss3, pre) -> header d ss3 pre b emit
  | X10 (header, pre) ->
      if b < 0x20 then (
        (* A control cannot be a report's byte: the report was cut. *)
        d.st <- Ground;
        if pre then emit escape;
        emit (Cut `Mouse);
        step d b emit)
      else (
        Bytes.unsafe_set d.x10 d.x10_len (Char.unsafe_chr b);
        d.x10_len <- d.x10_len + 1;
        if d.x10_len = 3 then (
          d.st <- Ground;
          let byte i = Char.code (Bytes.unsafe_get d.x10 i) in
          match
            mouse (byte 0 - 32) (byte 1 - 33) (byte 2 - 33) ~released:false
          with
          | Some m -> prefixed pre (Mouse m) emit
          | None -> reply pre (Csi header) emit))
  | Linux (header, pre) ->
      d.st <- Ground;
      if b >= 0x41 && b <= 0x45 then prefixed pre (press (F (b - 0x40)) 0) emit
      else (
        reply pre (Csi header) emit;
        step d b emit)
  | String intro ->
      if b = 0x1B then d.st <- String_esc intro
      else (
        forward d (Char.unsafe_chr b);
        string_byte_done d intro emit)
  | String_esc intro ->
      if b = 0x5C then (
        forward d '\x1b';
        forward d '\\';
        d.st <- Ground;
        match take d with
        | Some (Close e) -> string_end d intro e emit
        | _ ->
            (* A malformed DCS, or ST before its header ended: no effect. *)
            Buffer.reset d.data;
            d.too_long <- false;
            d.dcs <- None)
      else (
        (* The ESC ends the string and begins what follows. *)
        P.reset d.p;
        d.st <- Ground;
        string_end d intro `Aborted emit;
        step d 0x1B emit;
        step d b emit)
  | Paste _ ->
      Bytes.unsafe_set d.one 0 (Char.unsafe_chr b);
      ignore (paste_run d d.one 0 1 emit : int)

and ground d b emit =
  if d.u8_len > 0 then utf_8_continue d b emit
  else if b = 0x1B then d.st <- Esc false
  else if b < 0x80 then emit ascii.(b)
  else
    let need =
      if b >= 0xC2 && b <= 0xDF then 2
      else if b >= 0xE0 && b <= 0xEF then 3
      else if b >= 0xF0 && b <= 0xF4 then 4
      else 0
    in
    Bytes.unsafe_set d.u8 0 (Char.unsafe_chr b);
    d.u8_len <- 1;
    d.u8_need <- need;
    if need = 0 then flush_invalid d emit

(* A byte after the first of a UTF-8 character. Unicode Table 3-7 gives the
   range of the second byte by the first; the others are 0x80..0xBF. *)
and utf_8_continue d b emit =
  let lead = Char.code (Bytes.unsafe_get d.u8 0) in
  let lo, hi =
    if d.u8_len > 1 then (0x80, 0xBF)
    else if lead = 0xE0 then (0xA0, 0xBF)
    else if lead = 0xED then (0x80, 0x9F)
    else if lead = 0xF0 then (0x90, 0xBF)
    else if lead = 0xF4 then (0x80, 0x8F)
    else (0x80, 0xBF)
  in
  if b < lo || b > hi then (
    (* The character ends before [b], ill formed. *)
    flush_invalid d emit;
    ground d b emit)
  else (
    Bytes.unsafe_set d.u8 d.u8_len (Char.unsafe_chr b);
    d.u8_len <- d.u8_len + 1;
    if d.u8_len = d.u8_need then (
      let u = Uchar.utf_decode_uchar (Bytes.get_utf_8_uchar d.u8 0) in
      let bits = if d.u8_alt then alt_bit else 0 in
      d.u8_len <- 0;
      d.u8_alt <- false;
      emit (char_key ~keypad:false (Uchar.to_int u) bits)))

and header d is_ss3 pre b emit =
  if b < 0x20 || b > 0x7E then (
    (* A byte that cannot continue the sequence cuts it. *)
    P.reset d.p;
    d.st <- Ground;
    if pre then emit escape;
    emit (Cut `Sequence);
    step d b emit)
  else
    match if b = 0x24 && d.rx_digits then numbered d.rx else None with
    | Some key ->
        (* rxvt's Shift with a numbered key, [CSI n $], has no final byte. *)
        P.reset d.p;
        d.st <- Ground;
        prefixed pre (press key shift_bit) emit
    | None ->
        rxvt_byte d b;
        forward d (Char.unsafe_chr b);
        if b >= 0x40 then (
          d.st <- Ground;
          match take d with
          | Some (Csi s) -> if is_ss3 then ss3 s pre emit else csi d s pre emit
          | _ ->
              (* A malformed sequence has no effect. *)
              if pre then emit escape)

(* A run of paste content from [i]: what is content is passed on, and at most
   the bytes of an end marker begun are held. *)
and paste_run d b i stop emit =
  let m0 = match d.st with Paste m -> m | _ -> 0 in
  let m = ref m0 and j = ref i in
  while !m < 6 && !j < stop do
    let c = Bytes.unsafe_get b !j in
    m := if c = paste_end.[!m] then !m + 1 else if c = '\x1b' then 1 else 0;
    incr j
  done;
  let held = !m in
  let data = m0 + (!j - i) - held in
  (if data > 0 then
     let from_held = min m0 data in
     emit
       (Paste_data
          (if from_held = 0 then Bytes.sub_string b i data
           else
             String.sub paste_end 0 from_held
             ^ Bytes.sub_string b i (data - from_held))));
  if held = 6 then (
    d.st <- Ground;
    emit (Paste_end `Complete))
  else d.st <- Paste held;
  !j

(* A run of string content from [i], forwarded whole; the bytes that may end
   the string go one at a time. *)
let string_run d b i stop emit =
  let j = ref i in
  while
    !j < stop
    &&
    match Bytes.unsafe_get b !j with
    | '\x1b' | '\x07' | '\x18' | '\x1a' -> false
    | _ -> true
  do
    incr j
  done;
  if !j > i then (
    d.token <- None;
    ignore (P.feed d.p b ~off:i ~len:(!j - i) d.on_token : int);
    !j)
  else (
    step d (Char.code (Bytes.unsafe_get b i)) emit;
    i + 1)

(* A run of parameter bytes of a header from [i], forwarded whole. *)
let parameter_run d b i stop emit =
  let j = ref i in
  while
    !j < stop
    &&
    let c = Char.code (Bytes.unsafe_get b !j) in
    c >= 0x30 && c <= 0x3F
  do
    rxvt_byte d (Char.code (Bytes.unsafe_get b !j));
    incr j
  done;
  if !j > i then (
    ignore (P.feed d.p b ~off:i ~len:(!j - i) d.on_token : int);
    !j)
  else (
    step d (Char.code (Bytes.unsafe_get b i)) emit;
    i + 1)

let reset d =
  check_free d "reset";
  drop d

(* [decode d name step emit] is [step d emit], with [d] marked as decoding
   while it runs. A callback that raises leaves the step half done, so what
   [d] holds is dropped, the one state that is then defined. *)
let decode d name step emit =
  check_free d name;
  d.decoding <- true;
  match step d emit with
  | () -> d.decoding <- false
  | exception e ->
      d.decoding <- false;
      drop d;
      raise e

let feed_bytes d b off stop emit =
  let i = ref off in
  while !i < stop do
    match d.st with
    | Paste _ -> i := paste_run d b !i stop emit
    | String _ -> i := string_run d b !i stop emit
    | Header _ -> i := parameter_run d b !i stop emit
    | _ ->
        step d (Char.code (Bytes.unsafe_get b !i)) emit;
        incr i
  done

let feed d b ~off ~len emit =
  if off < 0 || len < 0 || off > Bytes.length b - len then
    invalid_arg "Matrix_input.Decoder.feed";
  check_free d "feed";
  d.decoding <- true;
  match feed_bytes d b off (off + len) emit with
  | () -> d.decoding <- false
  | exception e ->
      d.decoding <- false;
      drop d;
      raise e

let pending d =
  match d.st with
  | Ground -> if d.u8_len > 0 then Sequence else Nothing
  | Esc _ | Intro _ -> Ambiguous
  | Header _ | X10 _ | Linux _ -> Sequence
  | String _ | String_esc _ | Paste _ -> Nothing

let silence d emit =
  match d.st with
  | Ground -> if d.u8_len > 0 then flush_invalid d emit
  | Esc pre ->
      d.st <- Ground;
      emit (if pre then alt_ascii.(0x1B) else escape)
  | Intro (pre, intro) ->
      d.st <- Ground;
      intro_keys pre intro emit
  | Header (_, pre) | Linux (_, pre) ->
      P.reset d.p;
      d.st <- Ground;
      if pre then emit escape;
      emit (Cut `Sequence)
  | X10 (_, pre) ->
      d.st <- Ground;
      if pre then emit escape;
      emit (Cut `Mouse)
  | String _ | String_esc _ | Paste _ -> ()

let end_paste d emit =
  match d.st with
  | Paste m ->
      if m > 0 then emit (Paste_data (String.sub paste_end 0 m));
      d.st <- Ground;
      emit (Paste_end `Cut)
  | _ -> ()

let end_input d emit =
  silence d emit;
  (match d.st with
  | String _ | String_esc _ -> emit (Cut `String)
  | Paste _ -> end_paste d emit
  | _ -> ());
  drop d

let idle d emit = decode d "idle" silence emit
let cut_paste d emit = decode d "cut_paste" end_paste emit
let finish d emit = decode d "finish" end_input emit

(* Owed replies *)

let expect d (o : owed) =
  if o.cursor_reports < 0 || o.string_replies < 0 then
    invalid_arg "Matrix_input.Decoder.expect: negative count";
  d.cursor_reports <- d.cursor_reports + o.cursor_reports;
  d.string_replies <- d.string_replies + o.string_replies

let settle d (o : owed) =
  if o.cursor_reports < 0 || o.string_replies < 0 then
    invalid_arg "Matrix_input.Decoder.settle: negative count";
  if o.cursor_reports > d.cursor_reports || o.string_replies > d.string_replies
  then invalid_arg "Matrix_input.Decoder.settle: more than is owed";
  d.cursor_reports <- d.cursor_reports - o.cursor_reports;
  d.string_replies <- d.string_replies - o.string_replies

(* Encoding *)

let encode_paste ~bracketed s =
  if bracketed then "\x1b[200~" ^ s ^ paste_end else s

let encode_focus focused = if focused then "\x1b[I" else "\x1b[O"
