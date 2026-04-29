(* parser.ml *)

open Event

type key = Key.t
type event_type = Key.event_type
type modifier = Modifier.t
type mouse_button = Mouse.button
type event = Event.t
type response = Response.t
type capability = Response.capability
type parsed = [ `User of event | `Response of response ]

type protocol_context = {
  kitty_keyboard : bool;
  explicit_width_cpr : bool;
  startup_cursor_cpr : bool;
  pixel_resolution : bool;
  private_capability_replies : bool;
}

let default_protocol_context =
  {
    kitty_keyboard = false;
    explicit_width_cpr = false;
    startup_cursor_cpr = false;
    pixel_resolution = false;
    private_capability_replies = false;
  }

(* Maximum CSI parameters we track. Covers all keyboard/mouse sequences. *)
let max_csi_params = 8

(* Maximum sub-parameters per CSI param (for Kitty keyboard:
   code:shifted:base) *)
let max_sub_params = 4

type t = {
  raw : Tokenizer.parser;
  scratch_buffer : Buffer.t; (* Reusable buffer for various operations *)
  (* UTF-8 incomplete sequence buffer for streaming decode *)
  utf8_buf : bytes;
  mutable utf8_len : int;
  (* Pre-allocated CSI parameter storage to avoid list allocation *)
  csi_param_starts : int array;
  csi_param_stops : int array;
  csi_param_values : int array; (* -1 means empty/missing *)
  mutable csi_param_count : int;
  (* Pre-allocated sub-parameter storage *)
  sub_param_values : int array; (* -1 means empty/missing *)
  mutable sub_param_count : int;
  mutable mouse_left_pressed : bool;
  mutable mouse_middle_pressed : bool;
  mutable mouse_right_pressed : bool;
  mutable protocol_context : protocol_context;
}

(* Modifiers *)

let no_modifier = Modifier.none

(* Precomputed modifier table to avoid allocations *)
let modifier_table : modifier array =
  Array.init 256 (fun mask ->
      {
        Modifier.ctrl = mask land 4 <> 0;
        alt = mask land 2 <> 0;
        shift = mask land 1 <> 0;
        super = mask land 8 <> 0;
        hyper = mask land 16 <> 0;
        meta = mask land 2 <> 0 || mask land 32 <> 0;
        caps_lock = mask land 64 <> 0;
        num_lock = mask land 128 <> 0;
      })

(* Small “modifier DSL” *)
let mk_modifier ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) ?(caps_lock = false) ?(num_lock = false) ()
    : modifier =
  {
    Modifier.ctrl;
    alt;
    shift;
    super;
    hyper;
    meta = meta || alt;
    caps_lock;
    num_lock;
  }

let modifier_of_bits bits =
  if bits <= 0 then no_modifier else modifier_table.((bits - 1) land 0xff)

let event_type_of_int : int -> event_type = function
  | 2 -> Repeat
  | 3 -> Release
  | _ -> Press

(* Mouse helpers *)

let mouse_button_of_code : int -> mouse_button = function
  | 0 -> Left
  | 1 -> Middle
  | 2 -> Right
  | 3 -> Left
  | 128 -> Button 8
  | 129 -> Button 9
  | 130 -> Button 10
  | 131 -> Button 11
  | n -> Button n

let is_scroll_code code = code land 64 <> 0

let scroll_direction_of_code code =
  match code land 3 with
  | 0 -> Mouse.Scroll_up
  | 1 -> Scroll_down
  | 2 -> Scroll_left
  | _ -> Scroll_right

let mouse_pressed_button parser =
  if parser.mouse_left_pressed then Some Mouse.Left
  else if parser.mouse_middle_pressed then Some Middle
  else if parser.mouse_right_pressed then Some Right
  else None

let clear_mouse_pressed parser =
  parser.mouse_left_pressed <- false;
  parser.mouse_middle_pressed <- false;
  parser.mouse_right_pressed <- false

let set_mouse_pressed parser button pressed =
  match button with
  | Mouse.Left -> parser.mouse_left_pressed <- pressed
  | Middle -> parser.mouse_middle_pressed <- pressed
  | Right -> parser.mouse_right_pressed <- pressed
  | _ -> ()

let mouse_event ~x ~y ~modifier kind =
  Mouse (Mouse.make ~x ~y ~modifiers:modifier kind)

let modifier_of_mouse_value value : modifier =
  let mask = (value land 0x1c) lsr 2 in
  modifier_table.(mask land 0xff)

(* Parser state *)

let create () =
  {
    raw = Tokenizer.create ();
    scratch_buffer = Buffer.create 32;
    utf8_buf = Bytes.create 4;
    utf8_len = 0;
    csi_param_starts = Array.make max_csi_params 0;
    csi_param_stops = Array.make max_csi_params 0;
    csi_param_values = Array.make max_csi_params (-1);
    csi_param_count = 0;
    sub_param_values = Array.make max_sub_params (-1);
    sub_param_count = 0;
    mouse_left_pressed = false;
    mouse_middle_pressed = false;
    mouse_right_pressed = false;
    protocol_context = default_protocol_context;
  }

let protocol_context parser = parser.protocol_context

(* CSI helpers *)

let is_ascii_digit c = c >= '0' && c <= '9'
let is_csi_param c = c >= '\x30' && c <= '\x3f'
let is_csi_intermediate c = c >= '\x20' && c <= '\x2f'

let is_csi_final c =
  let code = Char.code c in
  (code >= 0x40 && code <= 0x7e) || code = 0x24 || code = 0x5e

(* Manual integer parsing without exceptions *)
let parse_int_range_opt s start end_ =
  if start >= end_ then None
  else
    let rec loop acc i =
      if i >= end_ then Some acc
      else
        let c = s.[i] in
        if is_ascii_digit c then loop ((acc * 10) + (Char.code c - 48)) (i + 1)
        else None
    in
    loop 0 start

let parse_int_range_limit_opt s start end_ limit =
  if start >= end_ then None
  else
    let rec loop acc i =
      if i >= end_ then Some acc
      else
        let c = s.[i] in
        if is_ascii_digit c then
          let digit = Char.code c - 48 in
          if acc > (limit - digit) / 10 then None
          else loop ((acc * 10) + digit) (i + 1)
        else None
    in
    loop 0 start

let uchar_of_int_opt cp =
  if cp < 0 || cp > 0x10ffff || (cp >= 0xd800 && cp <= 0xdfff) then None
  else Some (Uchar.of_int cp)

(* Parse CSI parameters into pre-allocated arrays - zero allocation *)
let parse_csi_params_into parser s start end_ =
  parser.csi_param_count <- 0;
  let rec loop i =
    if i >= end_ || parser.csi_param_count >= max_csi_params then ()
    else
      let rec find_param_end j =
        if j >= end_ || s.[j] = ';' || not (is_csi_param s.[j]) then j
        else find_param_end (j + 1)
      in
      let param_end = find_param_end i in
      let idx = parser.csi_param_count in
      parser.csi_param_starts.(idx) <- i;
      parser.csi_param_stops.(idx) <- param_end;
      parser.csi_param_values.(idx) <-
        (match parse_int_range_opt s i param_end with
        | Some v -> v
        | None -> -1);
      parser.csi_param_count <- idx + 1;
      let next =
        if param_end < end_ && s.[param_end] = ';' then param_end + 1
        else param_end
      in
      loop next
  in
  loop start

(* Parse sub-parameters (colon-separated) into pre-allocated array *)
let parse_sub_params_into parser s start stop =
  parser.sub_param_count <- 0;
  let rec loop i =
    if i >= stop || parser.sub_param_count >= max_sub_params then ()
    else
      let rec find_field_end j =
        if j >= stop || s.[j] = ':' then j else find_field_end (j + 1)
      in
      let field_end = find_field_end i in
      let idx = parser.sub_param_count in
      parser.sub_param_values.(idx) <-
        (match parse_int_range_opt s i field_end with
        | Some v -> v
        | None -> -1);
      parser.sub_param_count <- idx + 1;
      let next =
        if field_end < stop && s.[field_end] = ':' then field_end + 1
        else field_end
      in
      loop next
  in
  loop start

(* Get sub-param value by index, -1 if missing *)
let[@inline] get_sub_param parser idx =
  if idx < parser.sub_param_count then parser.sub_param_values.(idx) else -1

let has_prefix s prefix =
  let len = String.length prefix in
  String.length s >= len
  &&
  let rec loop i =
    if i = len then true else if s.[i] <> prefix.[i] then false else loop (i + 1)
  in
  loop 0

let has_suffix s suffix =
  let len = String.length suffix in
  let slen = String.length s in
  slen >= len
  &&
  let rec loop i =
    if i = len then true
    else if s.[slen - len + i] <> suffix.[i] then false
    else loop (i + 1)
  in
  loop 0

let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let limit = len_s - len_sub in
    let rec outer i =
      if i > limit then false
      else
        let rec inner j =
          if j = len_sub then true
          else if s.[i + j] <> sub.[j] then false
          else inner (j + 1)
        in
        if inner 0 then true else outer (i + 1)
    in
    outer 0

let is_all_digits s start stop =
  start < stop
  &&
  let rec loop i =
    if i >= stop then true
    else if is_ascii_digit s.[i] then loop (i + 1)
    else false
  in
  loop start

let is_digits_and s start stop allowed =
  start < stop
  &&
  let rec loop i =
    if i >= stop then true
    else if is_ascii_digit s.[i] || String.contains allowed s.[i] then
      loop (i + 1)
    else false
  in
  loop start

let is_deferred_kitty_csi context body =
  context.kitty_keyboard
  &&
  let len = String.length body in
  len > 0
  && (String.contains body ';' || String.contains body ':')
  && is_digits_and body 0 len ":;"

let is_deferred_explicit_width_cpr context body =
  context.explicit_width_cpr
  &&
  match String.index_opt body ';' with
  | None -> false
  | Some semi ->
      if
        String.equal (String.sub body 0 semi) "1"
        && is_all_digits body (semi + 1) (String.length body)
      then
        match
          parse_int_range_limit_opt body (semi + 1) (String.length body) 3
        with
        | Some col -> col >= 2 && col <= 3
        | None -> false
      else false

let is_deferred_startup_cpr context body =
  context.startup_cursor_cpr
  &&
  match String.index_opt body ';' with
  | None -> false
  | Some semi ->
      is_all_digits body 0 semi
      && is_all_digits body (semi + 1) (String.length body)

let is_deferred_pixel_resolution context body =
  context.pixel_resolution
  &&
  let prefix = "4;" in
  has_prefix body prefix
  && is_digits_and body (String.length prefix) (String.length body) ";"

let is_deferred_private_capability context body =
  context.private_capability_replies
  && String.length body >= 1
  && body.[0] = '?'
  && is_digits_and body 1 (String.length body) ";$"

let should_defer_pending parser pending =
  let len = String.length pending in
  if len < 3 || pending.[0] <> '\x1b' || pending.[1] <> '[' then false
  else
    let body = String.sub pending 2 (len - 2) in
    let context = parser.protocol_context in
    is_deferred_kitty_csi context body
    || is_deferred_explicit_width_cpr context body
    || is_deferred_startup_cpr context body
    || is_deferred_pixel_resolution context body
    || is_deferred_private_capability context body

let set_protocol_context parser context =
  parser.protocol_context <- context;
  let pending = Bytes.to_string (Tokenizer.pending parser.raw) in
  if pending <> "" && not (should_defer_pending parser pending) then
    Tokenizer.wake_deferred parser.raw

type capability_match = Event of capability | Drop | No_match

let capability_event_of_sequence seq =
  let len = String.length seq in
  if len >= 6 && has_prefix seq "\x1bP>|" && has_suffix seq "\x1b\\" then
    let payload_len = len - 6 in
    let payload =
      if payload_len > 0 then String.sub seq 4 payload_len else ""
    in
    Event (Response.Xtversion payload)
  else if
    len >= 4 && has_prefix seq "\x1bP" && has_suffix seq "\x1b\\"
    && contains_substring (String.lowercase_ascii seq) "kitty"
  then
    (* Some terminals respond with a plain DCS payload containing "kitty"
       without the XTVersion prefix; treat as Kitty identification. *)
    Event (Response.Xtversion "kitty")
  else if len >= 5 && has_prefix seq "\x1b_G" && has_suffix seq "\x1b\\" then
    let payload_len = len - 5 in
    let payload =
      if payload_len > 0 then String.sub seq 3 payload_len else ""
    in
    Event (Response.Kitty_graphics_reply payload)
  else if len >= 4 && has_prefix seq "\x1b[?" && has_suffix seq "u" then
    let body_len = len - 4 in
    if body_len <= 0 then Drop
    else
      let body = String.sub seq 3 body_len in
      let parts = String.split_on_char ';' body in
      let to_int_opt s = try Some (int_of_string s) with _ -> None in
      match parts with
      | level_str :: rest -> (
          match to_int_opt level_str with
          | None -> Drop
          | Some level ->
              let flags =
                match rest with [] -> None | h :: _ -> to_int_opt h
              in
              Event (Response.Kitty_keyboard { level; flags }))
      | _ -> Drop
  else No_match

(* OSC helpers *)

type osc_terminator = Bel of int | St of int

let find_osc_terminator s start stop =
  let rec loop i =
    if i >= stop then None
    else
      match s.[i] with
      | '\x07' -> Some (Bel i)
      | '\x1b' when i + 1 < stop && s.[i + 1] = '\\' -> Some (St i)
      | _ -> loop (i + 1)
  in
  loop start

let find_char s c =
  let len = String.length s in
  let rec loop i =
    if i >= len then None else if s.[i] = c then Some i else loop (i + 1)
  in
  loop 0

let split_first s c =
  match find_char s c with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let is_base64_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' | '=' -> true
  | _ -> false

let base64_value = function
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
  | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 26
  | '0' .. '9' as c -> Char.code c - Char.code '0' + 52
  | '+' -> 62
  | '/' -> 63
  | _ -> 0

let base64_padding_is_valid s =
  let len = String.length s in
  if len < 4 then true
  else
    match (s.[len - 2], s.[len - 1]) with
    | '=', '=' | _, '=' -> true
    | '=', _ -> false
    | _ -> true

let base64_chars_are_valid s =
  let len = String.length s in
  let rec loop i =
    if i >= len then true else is_base64_char s.[i] && loop (i + 1)
  in
  loop 0

let decode_base64 parser s =
  let len = String.length s in
  if len = 0 || len mod 4 <> 0 then None
  else if not (base64_chars_are_valid s && base64_padding_is_valid s) then None
  else
    let out = parser.scratch_buffer in
    Buffer.clear out;
    let rec loop i =
      if i + 4 <= len then begin
        let a = base64_value s.[i] in
        let b = base64_value s.[i + 1] in
        let c = base64_value s.[i + 2] in
        let d = base64_value s.[i + 3] in
        Buffer.add_char out (Char.chr ((a lsl 2) lor (b lsr 4)));
        if s.[i + 2] <> '=' then begin
          Buffer.add_char out (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
          if s.[i + 3] <> '=' then
            Buffer.add_char out (Char.chr (((c land 3) lsl 6) lor d))
        end;
        loop (i + 4)
      end
    in
    loop 0;
    Some (Buffer.contents out)

let parse_osc_52 parser data =
  let selection, base64_data = split_first data ';' in
  let clipboard_data =
    match decode_base64 parser base64_data with
    | Some decoded -> decoded
    | None -> base64_data
  in
  Response.Clipboard (selection, clipboard_data)

let response_of_osc parser payload =
  let code_str, data = split_first payload ';' in
  let code =
    match parse_int_range_opt code_str 0 (String.length code_str) with
    | Some code -> code
    | None -> 0
  in
  if code = 52 then parse_osc_52 parser data else Response.Osc (code, data)

let parse_osc_sequence parser s start length =
  match find_osc_terminator s (start + 2) (start + length) with
  | None -> (None, 0)
  | Some terminator ->
      let payload_stop, consumed =
        match terminator with
        | Bel i -> (i, i - start + 1)
        | St i -> (i, i - start + 2)
      in
      let payload = String.sub s (start + 2) (payload_stop - start - 2) in
      (Some (`Response (response_of_osc parser payload)), consumed)

type csi_mod = { code : int option; mods : int option; event : int option }

(* Convert -1 to None, otherwise Some v *)
let[@inline] int_to_opt v = if v < 0 then None else Some v

(* Split a CSI param at index into sub-params and return first 3 values *)
let split_param_at parser s idx =
  if idx >= parser.csi_param_count then (None, None, None)
  else
    let start = parser.csi_param_starts.(idx) in
    let stop = parser.csi_param_stops.(idx) in
    parse_sub_params_into parser s start stop;
    ( int_to_opt (get_sub_param parser 0),
      int_to_opt (get_sub_param parser 1),
      int_to_opt (get_sub_param parser 2) )

(* Extract code/mods/event from parsed CSI parameters - zero allocation *)
let csi_mod_of_parsed parser s =
  match parser.csi_param_count with
  | 0 -> { code = None; mods = None; event = None }
  | 1 ->
      let code, mods, event = split_param_at parser s 0 in
      { code; mods; event }
  | 2 ->
      let code, _, _ = split_param_at parser s 0 in
      let mods, event, _ = split_param_at parser s 1 in
      { code; mods; event }
  | _ ->
      let code, _, _ = split_param_at parser s 0 in
      let mods, _, _ = split_param_at parser s 1 in
      let event, _, _ = split_param_at parser s 2 in
      { code; mods; event }

let modifier_of_csi_mod m =
  match m.mods with Some bits -> modifier_of_bits bits | None -> no_modifier

let event_type_of_csi_mod m =
  match m.event with Some e -> event_type_of_int e | None -> Press

let ensure_shift m = if m.Modifier.shift then m else { m with shift = true }

let ensure_ctrl m =
  if m.Modifier.ctrl then m else { m with Modifier.ctrl = true }

let ctrl_key_of_code code =
  match code with
  | 0x00 -> Some (Key.Char (Uchar.of_int 32))
  | c when c >= 0x01 && c <= 0x1a ->
      Some (Char (Uchar.of_int (Char.code 'a' + c - 1)))
  | 0x1c -> Some (Char (Uchar.of_char '\\'))
  | 0x1d -> Some (Char (Uchar.of_char ']'))
  | 0x1e -> Some (Char (Uchar.of_char '^'))
  | 0x1f -> Some (Char (Uchar.of_char '_'))
  | _ -> None

let parse_x10_normal_mouse_string s start =
  let len = String.length s in
  if start + 3 <= len then
    let btn = Char.code s.[start] in
    let ux = Char.code s.[start + 1] in
    let uy = Char.code s.[start + 2] in
    let cb = btn - 32 in
    let x = ux - 33 in
    let y = uy - 33 in
    if x < 0 || y < 0 then None
    else
      let base_button = cb land 3 in
      let is_scroll = cb land 64 <> 0 in
      let is_motion = cb land 32 <> 0 in
      let modifier = modifier_of_mouse_value cb in
      let ev : event =
        if is_scroll then
          mouse_event ~x ~y ~modifier
            (Scroll { direction = scroll_direction_of_code cb; delta = 1 })
        else if is_motion then
          let kind =
            if base_button = 3 then Mouse.Move
            else Drag { button = mouse_button_of_code base_button }
          in
          mouse_event ~x ~y ~modifier kind
        else if base_button = 3 then
          mouse_event ~x ~y ~modifier (Up { button = None })
        else
          let button =
            match base_button with
            | 0 -> Mouse.Left
            | 1 -> Middle
            | 2 -> Right
            | n -> Button n
          in
          mouse_event ~x ~y ~modifier (Down { button })
      in
      Some (ev, 3)
  else None

(* Generic helper for mouse events used by SGR/URXVT-style encodings *)
let mouse_event_of_codes ?release_button ~button_code ~x ~y ~is_release
    ~is_motion ~modifier () : event =
  if is_scroll_code button_code then
    mouse_event ~x ~y ~modifier
      (Scroll { direction = scroll_direction_of_code button_code; delta = 1 })
  else if is_motion then
    mouse_event ~x ~y ~modifier
      (Drag { button = mouse_button_of_code button_code })
  else if is_release then
    let button =
      match release_button with
      | Some button -> Some button
      | None ->
          if button_code = 3 then None
          else Some (mouse_button_of_code button_code)
    in
    mouse_event ~x ~y ~modifier (Up { button })
  else
    mouse_event ~x ~y ~modifier
      (Down { button = mouse_button_of_code button_code })

let parse_sgr_mouse parser s start end_ =
  (* Format: "<btn;x;y" followed by final M/m at [end_ - 1]. *)
  let final = s.[end_ - 1] in
  if final <> 'M' && final <> 'm' then None
  else if s.[start] <> '<' || end_ - start < 5 then None
  else
    let limit = end_ - 1 in
    let parse_field i =
      if i >= limit then (None, i)
      else
        let c0 = s.[i] in
        if c0 < '0' || c0 > '9' then (None, i)
        else
          let rec loop acc j =
            if j >= limit then (Some acc, j)
            else
              let c = s.[j] in
              if c >= '0' && c <= '9' then
                loop ((acc * 10) + (Char.code c - 48)) (j + 1)
              else (Some acc, j)
          in
          loop 0 i
    in
    let btn_opt, idx1 = parse_field (start + 1) in
    match btn_opt with
    | None -> None
    | Some btn -> (
        if idx1 >= limit || s.[idx1] <> ';' then None
        else
          let x_opt, idx2 = parse_field (idx1 + 1) in
          match x_opt with
          | None -> None
          | Some x -> (
              if idx2 >= limit || s.[idx2] <> ';' then None
              else
                let y_opt, idx3 = parse_field (idx2 + 1) in
                match y_opt with
                | None -> None
                | Some y ->
                    if idx3 <> limit then None
                    else
                      let button_code = btn land 3 lor ((btn lsr 6) lsl 6) in
                      let modifier = modifier_of_mouse_value btn in
                      let button = mouse_button_of_code button_code in
                      let is_scroll = is_scroll_code button_code in
                      let is_motion = btn land 32 <> 0 && not is_scroll in
                      let is_release =
                        final = 'm' && (not is_scroll) && not is_motion
                      in
                      let event =
                        if is_scroll then
                          mouse_event_of_codes ~button_code ~x:(x - 1)
                            ~y:(y - 1) ~is_release ~is_motion ~modifier ()
                        else if is_motion then
                          let kind =
                            match mouse_pressed_button parser with
                            | Some button -> Mouse.Drag { button }
                            | None -> Move
                          in
                          mouse_event ~x:(x - 1) ~y:(y - 1) ~modifier kind
                        else if is_release then (
                          clear_mouse_pressed parser;
                          mouse_event_of_codes ~button_code ~x:(x - 1)
                            ~y:(y - 1) ~is_release ~is_motion ~modifier ())
                        else (
                          set_mouse_pressed parser button true;
                          mouse_event_of_codes ~button_code ~x:(x - 1)
                            ~y:(y - 1) ~is_release ~is_motion ~modifier ())
                      in
                      Some event))

let parse_urxvt_mouse s start end_ =
  (* Format: "btn;x;y" followed by final M/m at [end_ - 1]. *)
  let final = s.[end_ - 1] in
  if final <> 'M' && final <> 'm' then None
  else if end_ - start < 5 then None
  else
    let limit = end_ - 1 in
    let parse_field i =
      if i >= limit then (None, i)
      else
        let c0 = s.[i] in
        if c0 < '0' || c0 > '9' then (None, i)
        else
          let rec loop acc j =
            if j >= limit then (Some acc, j)
            else
              let c = s.[j] in
              if c >= '0' && c <= '9' then
                loop ((acc * 10) + (Char.code c - 48)) (j + 1)
              else (Some acc, j)
          in
          loop 0 i
    in
    let btn_opt, idx1 = parse_field start in
    match btn_opt with
    | None -> None
    | Some btn -> (
        if idx1 >= limit || s.[idx1] <> ';' then None
        else
          let x_opt, idx2 = parse_field (idx1 + 1) in
          match x_opt with
          | None -> None
          | Some x -> (
              if idx2 >= limit || s.[idx2] <> ';' then None
              else
                let y_opt, idx3 = parse_field (idx2 + 1) in
                match y_opt with
                | None -> None
                | Some y ->
                    if idx3 <> limit then None
                    else
                      let base_btn = btn - 32 in
                      let button_code =
                        base_btn land 3 lor ((base_btn lsr 6) lsl 6)
                      in
                      let is_scroll = is_scroll_code button_code in
                      let modifier = modifier_of_mouse_value base_btn in
                      let is_motion = base_btn land 32 <> 0 && not is_scroll in
                      let is_release =
                        button_code = 3 && (not is_scroll) && not is_motion
                      in
                      let event =
                        if is_scroll then
                          mouse_event ~x:(x - 1) ~y:(y - 1) ~modifier
                            (Scroll
                               {
                                 direction =
                                   scroll_direction_of_code button_code;
                                 delta = 1;
                               })
                        else
                          mouse_event_of_codes ~button_code ~x:(x - 1)
                            ~y:(y - 1) ~is_release ~is_motion ~modifier ()
                      in
                      Some event))

(* Kitty keyboard PUA mapping *)

let pua_to_key c : key =
  match c with
  (* Kitty PUA codes follow the upstream mapping documented in the protocol spec. *)
  (* Standard keys in PUA block *)
  | 57344 -> Escape
  | 57345 -> Enter
  | 57346 -> Tab
  | 57347 -> Backspace
  | 57348 -> Insert
  | 57349 -> Delete
  | 57350 -> Left
  | 57351 -> Right
  | 57352 -> Up
  | 57353 -> Down
  | 57354 -> Page_up
  | 57355 -> Page_down
  | 57356 -> Home
  | 57357 -> End
  (* Locks and system keys *)
  | 57358 -> Caps_lock
  | 57359 -> Scroll_lock
  | 57360 -> Num_lock
  | 57361 -> Print_screen
  | 57362 -> Pause
  | 57363 -> Menu
  (* Function keys F1-F35: 57364..57398 *)
  | c when c >= 57364 && c <= 57398 -> F (c - 57363)
  (* Keypad keys: codes per Kitty keyboard protocol spec.
     https://sw.kovidgoyal.net/kitty/keyboard-protocol/#functional-key-definitions *)
  | 57399 -> KP_0
  | 57400 -> KP_1
  | 57401 -> KP_2
  | 57402 -> KP_3
  | 57403 -> KP_4
  | 57404 -> KP_5
  | 57405 -> KP_6
  | 57406 -> KP_7
  | 57407 -> KP_8
  | 57408 -> KP_9
  | 57409 -> KP_decimal
  | 57410 -> KP_divide
  | 57411 -> KP_multiply
  | 57412 -> KP_subtract
  | 57413 -> KP_add
  | 57414 -> KP_enter
  | 57415 -> KP_equal
  | 57416 -> KP_separator
  | 57417 -> KP_left
  | 57418 -> KP_right
  | 57419 -> KP_up
  | 57420 -> KP_down
  | 57421 -> KP_page_up
  | 57422 -> KP_page_down
  | 57423 -> KP_home
  | 57424 -> KP_end
  | 57425 -> KP_insert
  | 57426 -> KP_delete
  | 57427 -> KP_begin
  (* Media keys *)
  | 57428 -> Media_play
  | 57429 -> Media_pause
  | 57430 -> Media_play_pause
  | 57431 -> Media_reverse
  | 57432 -> Media_stop
  | 57433 -> Media_fast_forward
  | 57434 -> Media_rewind
  | 57435 -> Media_next
  | 57436 -> Media_prev
  | 57437 -> Media_record
  | 57438 -> Volume_down
  | 57439 -> Volume_up
  | 57440 -> Volume_mute
  (* Modifier keys *)
  | 57441 -> Shift_left
  | 57442 -> Ctrl_left
  | 57443 -> Alt_left
  | 57444 -> Super_left
  | 57445 -> Hyper_left
  | 57446 -> Meta_left
  | 57447 -> Shift_right
  | 57448 -> Ctrl_right
  | 57449 -> Alt_right
  | 57450 -> Super_right
  | 57451 -> Hyper_right
  | 57452 -> Meta_right
  | 57453 -> Iso_level3_shift
  | 57454 -> Iso_level5_shift
  | _ -> Unknown c

let key_of_tilde_code = function
  | 1 | 7 -> Some Key.Home
  | 2 -> Some Insert
  | 3 -> Some Delete
  | 4 | 8 -> Some End
  | 5 -> Some Page_up
  | 6 -> Some Page_down
  | 11 -> Some (F 1)
  | 12 -> Some (F 2)
  | 13 -> Some (F 3)
  | 14 -> Some (F 4)
  | 15 -> Some (F 5)
  | 17 -> Some (F 6)
  | 18 -> Some (F 7)
  | 19 -> Some (F 8)
  | 20 -> Some (F 9)
  | 21 -> Some (F 10)
  | 23 -> Some (F 11)
  | 24 -> Some (F 12)
  | 25 -> Some (F 13)
  | 26 -> Some (F 14)
  | 28 -> Some (F 15)
  | 29 -> Some (F 16)
  | 31 -> Some (F 17)
  | 32 -> Some (F 18)
  | 33 -> Some (F 19)
  | 34 -> Some (F 20)
  | _ -> None

(* Key helpers *)

let make_key_event ?(modifier = no_modifier) ?(event_type = Key.Press)
    ?(associated_text = "") ?shifted_key ?base_key (k : key) : event =
  Event.key ~modifier ~event_type ~associated_text ?shifted_key ?base_key k

let make_char_event ?modifier ?event_type ?associated_text ?shifted_key
    ?base_key c : event =
  Event.char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c

let alt_escape_event : event =
  make_key_event ~modifier:(mk_modifier ~alt:true ()) Escape

(* Kitty keyboard CSI u *)

let parse_kitty_keyboard parser s start end_ =
  (* Parse CSI ... u format directly from string to avoid allocations *)
  let i = ref start in
  let len = end_ - 1 in
  (* exclude the 'u' final char *)

  (* Helper to find next separator *)
  let find_sep sep =
    let start_pos = !i in
    while !i < len && s.[!i] <> sep do
      incr i
    done;
    let end_pos = !i in
    if !i < len then incr i;
    (* skip separator *)
    (start_pos, end_pos)
  in

  (* Parse first field: code[:shifted[:base]] *)
  let code_start, code_end = find_sep ';' in
  if code_start >= code_end then None
  else
    (* Parse code with optional :shifted:base *)
    let code_i = ref code_start in
    while !code_i < code_end && s.[!code_i] <> ':' do
      incr code_i
    done;
    let code =
      parse_int_range_limit_opt s code_start !code_i 0x10ffff
      |> Option.value ~default:0
    in
    let shifted =
      if !code_i < code_end then (
        incr code_i;
        (* skip ':' *)
        let shifted_start = !code_i in
        while !code_i < code_end && s.[!code_i] <> ':' do
          incr code_i
        done;
        let shifted_val =
          parse_int_range_limit_opt s shifted_start !code_i 0x10ffff
        in
        Option.bind shifted_val (fun cp ->
            if cp > 0 then uchar_of_int_opt cp else None))
      else None
    in
    let base =
      if !code_i < code_end then (
        incr code_i;
        (* skip ':' *)
        let base_val = parse_int_range_limit_opt s !code_i code_end 0x10ffff in
        Option.bind base_val (fun cp ->
            if cp > 0 then uchar_of_int_opt cp else None))
      else None
    in

    (* Parse second field: mods[:event] *)
    let mods_start, mods_end = find_sep ';' in
    let mods_i = ref mods_start in
    while !mods_i < mods_end && s.[!mods_i] <> ':' do
      incr mods_i
    done;
    let mods =
      parse_int_range_limit_opt s mods_start !mods_i 255
      |> Option.value ~default:1
    in
    let event_val =
      if !mods_i < mods_end then (
        incr mods_i;
        (* skip ':' *)
        parse_int_range_limit_opt s !mods_i mods_end 3
        |> Option.value ~default:1)
      else 1
    in

    (* Parse optional third field: text as colon-separated codepoints *)
    let associated_text =
      if !i >= len then ""
      else
        let buf = parser.scratch_buffer in
        Buffer.clear buf;
        let pos = ref !i in
        while !pos < len do
          let field_start = !pos in
          while !pos < len && s.[!pos] <> ':' do
            incr pos
          done;
          let cp = parse_int_range_limit_opt s field_start !pos 0x10ffff in
          (match Option.bind cp uchar_of_int_opt with
          | Some u when Uchar.to_int u > 0 -> Buffer.add_utf_8_uchar buf u
          | _ -> ());
          if !pos < len then incr pos (* skip ':' *)
        done;
        Buffer.contents buf
    in

    let modifier = modifier_of_bits mods in
    let event_type = event_type_of_int event_val in
    let key_opt : key option =
      match code with
      | 0 -> None
      | 9 -> Some Tab
      | 13 -> Some Enter
      | 27 -> Some Escape
      | 127 -> Some Backspace
      | c when c >= 57344 && c <= 63743 -> Some (pua_to_key c)
      | c when c > 0 -> Option.map (fun u -> Key.Char u) (uchar_of_int_opt c)
      | _ -> None
    in
    match key_opt with
    | None -> None
    | Some k ->
        let associated_text =
          if associated_text <> "" then associated_text
          else
            match k with
            | Char u ->
                let buf = parser.scratch_buffer in
                Buffer.clear buf;
                let chosen =
                  match (modifier.Modifier.shift, shifted) with
                  | true, Some su -> su
                  | _ -> u
                in
                Buffer.add_utf_8_uchar buf chosen;
                Buffer.contents buf
            | _ -> ""
        in
        Some
          (make_key_event ~modifier ~event_type ~associated_text
             ?shifted_key:shifted ?base_key:base k)

(* CSI parsing *)

(* Helper to get CSI param value by index, -1 if missing *)
let[@inline] get_csi_param parser idx =
  if idx < parser.csi_param_count then parser.csi_param_values.(idx) else -1

(* CSI dispatch helpers — grouped by function for readability. *)

let parse_csi_param_values s start end_ =
  let rec loop acc i =
    if i >= end_ then List.rev acc
    else
      let rec find_param_end j =
        if j >= end_ || s.[j] = ';' || not (is_csi_param s.[j]) then j
        else find_param_end (j + 1)
      in
      let param_end = find_param_end i in
      let value =
        match parse_int_range_opt s i param_end with Some v -> v | None -> -1
      in
      let next =
        if param_end < end_ && s.[param_end] = ';' then param_end + 1
        else param_end
      in
      loop (value :: acc) next
  in
  loop [] start

let parse_csi_param_pairs values =
  let rec loop acc = function
    | mode :: value :: rest when mode >= 0 && value >= 0 ->
        loop ((mode, value) :: acc) rest
    | _ -> List.rev acc
  in
  loop [] values

let parse_csi_positive_values values =
  let rec loop acc = function
    | [] -> List.rev acc
    | v :: rest -> if v >= 0 then loop (v :: acc) rest else loop acc rest
  in
  loop [] values

let parse_csi_cursor final_char modifier event_type : parsed option =
  match final_char with
  | 'A' -> Some (`User (make_key_event ~modifier ~event_type Up))
  | 'B' -> Some (`User (make_key_event ~modifier ~event_type Down))
  | 'C' -> Some (`User (make_key_event ~modifier ~event_type Right))
  | 'D' -> Some (`User (make_key_event ~modifier ~event_type Left))
  | 'E' -> Some (`User (make_key_event ~modifier ~event_type KP_5))
  | 'a' ->
      let modifier = ensure_shift modifier in
      Some (`User (make_key_event ~modifier ~event_type Up))
  | 'b' ->
      let modifier = ensure_shift modifier in
      Some (`User (make_key_event ~modifier ~event_type Down))
  | 'd' ->
      let modifier = ensure_shift modifier in
      Some (`User (make_key_event ~modifier ~event_type Left))
  | 'e' ->
      let modifier = ensure_shift modifier in
      Some (`User (make_key_event ~modifier ~event_type KP_5))
  | 'H' -> Some (`User (make_key_event ~modifier ~event_type Home))
  | 'F' -> Some (`User (make_key_event ~modifier ~event_type End))
  | 'Z' ->
      let modifier = ensure_shift modifier in
      Some (`User (make_key_event ~modifier ~event_type Tab))
  | _ -> None

let parse_csi_tilde parser final_char modifier event_type csi_mod :
    parsed option =
  match final_char with
  | ('$' | '^') as suffix ->
      let n = get_csi_param parser 0 in
      if parser.csi_param_count = 1 && n >= 0 then
        match key_of_tilde_code n with
        | Some key ->
            let modifier =
              if suffix = '$' then ensure_shift modifier
              else ensure_ctrl modifier
            in
            Some (`User (make_key_event ~modifier ~event_type key))
        | None -> None
      else None
  | '~' -> (
      let p0 = get_csi_param parser 0 in
      let p1 = get_csi_param parser 1 in
      let p2 = get_csi_param parser 2 in
      if parser.csi_param_count >= 3 && p0 = 27 && p1 >= 0 && p2 >= 0 then
        let modifier = modifier_of_bits p1 in
        let key_from_charcode = function
          | 13 -> Key.Enter
          | 27 -> Escape
          | 9 -> Tab
          | 32 -> Char (Uchar.of_int 32)
          | 127 | 8 -> Backspace
          | c when c > 0 -> Char (Uchar.of_int c)
          | _ -> Char (Uchar.of_int 0)
        in
        Some (`User (make_key_event ~modifier (key_from_charcode p2)))
      else
        match csi_mod.code with
        | Some 200 | Some 201 -> None
        | Some n -> (
            match key_of_tilde_code n with
            | Some key ->
                Some (`User (make_key_event ~modifier ~event_type key))
            | None -> None)
        | None -> None)
  | _ -> None

let parse_csi_capability s start params_end final_char : parsed option =
  match final_char with
  | 'y' ->
      let is_private =
        start < params_end && s.[start] = '?' && start + 1 <= params_end
      in
      let params_start = if is_private then start + 1 else start in
      let modes =
        parse_csi_param_values s params_start params_end
        |> parse_csi_param_pairs
      in
      Some
        (`Response
           (Response.Capability
              (Response.Mode_report { Response.is_private; modes })))
  (* Color scheme DSR response: CSI ? 997 ; value n Response to CSI ? 996 n
     query. Value 1 = dark, 2 = light. *)
  | 'n' ->
      if start < params_end && s.[start] = '?' then
        match parse_csi_param_values s (start + 1) params_end with
        | 997 :: value :: _ ->
            let scheme =
              match value with 1 -> `Dark | 2 -> `Light | v -> `Unknown v
            in
            Some
              (`Response (Response.Capability (Response.Color_scheme scheme)))
        | _ -> None
      else None
  | 't' -> (
      match parse_csi_param_values s start params_end with
      | [ 8; p1; p2 ] when p1 >= 0 && p2 >= 0 -> Some (`User (Resize (p2, p1)))
      | [ 4; p1; p2 ] when p1 >= 0 && p2 >= 0 ->
          Some
            (`Response
               (Response.Capability (Response.Pixel_resolution (p2, p1))))
      | _ -> None)
  | 'R' -> (
      match parse_csi_param_values s start params_end with
      | [ row; col ] when row >= 0 && col >= 0 ->
          Some
            (`Response
               (Response.Capability (Response.Cursor_position (row, col))))
      | _ -> None)
  | 'c' ->
      if start < params_end && s.[start] = '?' then
        let attrs =
          parse_csi_param_values s (start + 1) params_end
          |> parse_csi_positive_values
        in
        Some
          (`Response (Response.Capability (Response.Device_attributes attrs)))
      else if start = params_end then
        Some
          (`User (make_key_event ~modifier:(mk_modifier ~shift:true ()) Right))
      else None
  | _ -> None

let parse_csi_mouse_event parser s start intermediate_end final_char params_end
    : parsed option =
  match final_char with
  | 'M' | 'm' ->
      if start < String.length s && s.[start] = '<' then
        Option.map
          (fun e -> `User e)
          (parse_sgr_mouse parser s start (intermediate_end + 1))
      else if params_end > start then
        Option.map
          (fun e -> `User e)
          (parse_urxvt_mouse s start (intermediate_end + 1))
      else None
  | _ -> None

let parse_csi parser s start end_ : parsed option =
  let params_end = ref start in
  while !params_end < end_ && is_csi_param s.[!params_end] do
    incr params_end
  done;

  let intermediate_end = ref !params_end in
  let rec consume_intermediate () =
    if !intermediate_end < end_ then
      let ch = s.[!intermediate_end] in
      let is_dollar = ch = '$' || ch = '^' in
      if is_csi_intermediate ch then
        if is_dollar && !intermediate_end + 1 >= end_ then ()
        else (
          incr intermediate_end;
          consume_intermediate ())
      else ()
    else ()
  in
  consume_intermediate ();

  if !intermediate_end < end_ && is_csi_final s.[!intermediate_end] then
    let final_char = s.[!intermediate_end] in

    let capability =
      match final_char with
      | 'y' | 'n' | 't' | 'R' | 'c' ->
          parse_csi_capability s start !params_end final_char
      | _ -> None
    in
    match capability with
    | Some _ as event -> event
    | None -> (
        parse_csi_params_into parser s start !params_end;

        let csi_mod = csi_mod_of_parsed parser s in
        let modifier = modifier_of_csi_mod csi_mod in
        let event_type = event_type_of_csi_mod csi_mod in

        match final_char with
        (* Cursor movement, Home/End, Tab *)
        | 'A' | 'B' | 'C' | 'D' | 'E' | 'a' | 'b' | 'd' | 'e' | 'H' | 'F' | 'Z'
          ->
            parse_csi_cursor final_char modifier event_type
        (* Focus events *)
        | 'I' -> Some (`User Focus)
        | 'O' -> Some (`User Blur)
        (* Tilde sequences and rxvt shift/ctrl variants *)
        | '$' | '^' | '~' ->
            parse_csi_tilde parser final_char modifier event_type csi_mod
        (* CSI-u format (modern keyboard protocol) *)
        | 'u' -> (
            match
              parse_kitty_keyboard parser s start (!intermediate_end + 1)
            with
            | None -> None
            | Some e -> Some (`User e))
        (* Mouse events *)
        | 'M' | 'm' ->
            parse_csi_mouse_event parser s start !intermediate_end final_char
              !params_end
        | _ -> None)
  else None

(* ESC parsing *)

let parse_escape_sequence parser s start length : parsed option * int =
  if length <= 0 then (None, 0)
  else if length = 1 then (Some (`User (make_key_event Escape)), 1)
  else
    let esc2 = s.[start + 1] in
    if esc2 = '[' then
      if length = 4 && s.[start + 2] = '[' then
        let key =
          match s.[start + 3] with
          | 'A' -> Some (Key.F 1)
          | 'B' -> Some (Key.F 2)
          | 'C' -> Some (Key.F 3)
          | 'D' -> Some (Key.F 4)
          | 'E' -> Some (Key.F 5)
          | _ -> None
        in
        match key with
        | Some key -> (Some (`User (make_key_event key)), length)
        | None -> (None, 0)
      else
        match parse_csi parser s (start + 2) (start + length) with
        | Some event -> (Some event, length)
        | None -> (None, 0)
    else if esc2 = 'O' && length >= 3 then
      (* SS3 sequences - used by terminals for arrows, home/end, function
         keys *)
      let event_opt =
        match s.[start + 2] with
        | 'A' -> Some (make_key_event Up)
        | 'B' -> Some (make_key_event Down)
        | 'C' -> Some (make_key_event Right)
        | 'D' -> Some (make_key_event Left)
        | 'F' -> Some (make_key_event End)
        | 'H' -> Some (make_key_event Home)
        | 'P' -> Some (make_key_event (F 1))
        | 'Q' -> Some (make_key_event (F 2))
        | 'R' -> Some (make_key_event (F 3))
        | 'S' -> Some (make_key_event (F 4))
        | 'a' -> Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Up)
        | 'b' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Down)
        | 'c' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Right)
        | 'd' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) Left)
        | 'e' ->
            Some (make_key_event ~modifier:(mk_modifier ~ctrl:true ()) KP_5)
        | 'X' -> Some (make_char_event '=') (* keypad = *)
        | _ -> None
      in
      (Option.map (fun e -> `User e) event_opt, 3)
    else if esc2 = ']' then parse_osc_sequence parser s start length
    else if length >= 2 then
      (* Alt (ESC-prefix) combinations and ESC+control handling *)
      let c = s.[start + 1] in
      (* ESC + control-char -> Alt+Ctrl letter (or space for NUL) *)
      if c >= '\x00' && c <= '\x1f' then
        let key, modifier =
          if c = '\t' then (Key.Tab, mk_modifier ~alt:true ())
          else if c = '\n' then (Key.Line_feed, mk_modifier ~alt:true ())
          else if c = '\r' then (Key.Enter, mk_modifier ~alt:true ())
          else
            match ctrl_key_of_code (Char.code c) with
            | Some key -> (key, mk_modifier ~alt:true ~ctrl:true ())
            | None ->
                (Char (Uchar.of_int (Char.code c)), mk_modifier ~alt:true ())
        in
        (Some (`User (make_key_event ~modifier key)), 2)
      else if c = '\x7f' || c = '\b' then
        ( Some
            (`User
               (make_key_event ~modifier:(mk_modifier ~alt:true ()) Backspace)),
          2 )
      else if c = ' ' then
        ( Some
            (`User
               (make_key_event ~modifier:(mk_modifier ~alt:true ())
                  (Char (Uchar.of_int 32)))),
          2 )
      else if c >= '!' && c <= '~' then
        let is_upper = c >= 'A' && c <= 'Z' in
        let modifier =
          if is_upper then mk_modifier ~alt:true ~shift:true ()
          else mk_modifier ~alt:true ()
        in
        ( Some
            (`User
               (make_key_event ~modifier (Char (Uchar.of_int (Char.code c))))),
          2 )
      else (None, 0)
    else (None, 0)

(* Char decoding *)

let special_key_event_of_code code : event option =
  match code with
  | 0x00 -> Some (make_char_event ~modifier:(mk_modifier ~ctrl:true ()) ' ')
  | 0x08 | 0x7f -> Some (make_key_event Backspace)
  | 0x09 -> Some (make_key_event Tab)
  | 0x0a -> Some (make_key_event Line_feed)
  | 0x0d -> Some (make_key_event Enter)
  | c when (c >= 0x01 && c <= 0x1a) || (c >= 0x1c && c <= 0x1f) ->
      Option.map
        (make_key_event ~modifier:(mk_modifier ~ctrl:true ()))
        (ctrl_key_of_code c)
  | _ -> None

(* Precomputed ASCII key events to avoid per-character allocation in hot
   paths. *)
let ascii_events : event option array =
  Array.init 128 (fun code ->
      match special_key_event_of_code code with
      | Some ev -> Some ev
      | None ->
          let modifier =
            if code >= Char.code 'A' && code <= Char.code 'Z' then
              mk_modifier ~shift:true ()
            else no_modifier
          in
          Some (make_char_event ~modifier (Char.chr code)))

(* Unified logic for “what to do with a code point” *)
let key_event_of_code parser code : event option =
  if code >= 0 && code < Array.length ascii_events then ascii_events.(code)
  else
    let u = Uchar.of_int code in
    let modifier =
      if code >= Char.code 'A' && code <= Char.code 'Z' then
        mk_modifier ~shift:true ()
      else no_modifier
    in
    let buf = parser.scratch_buffer in
    Buffer.clear buf;
    Buffer.add_utf_8_uchar buf u;
    let associated_text = Buffer.contents buf in
    Some (make_key_event ~modifier ~associated_text (Char u))

(* Expected UTF-8 sequence length from lead byte, 0 if invalid *)
let[@inline] utf8_seq_len byte =
  if byte < 0x80 then 1
  else if byte < 0xC2 then 0 (* continuation or overlong *)
  else if byte < 0xE0 then 2
  else if byte < 0xF0 then 3
  else if byte < 0xF5 then 4
  else 0

let[@inline] is_utf8_continuation byte = byte land 0xc0 = 0x80

(* Callback-based text event processing - zero list allocation *)
let text_events_iter parser bytes off len emit =
  let end_pos = off + len in
  let i = ref off in
  (* Handle pending incomplete UTF-8 sequence from previous chunk *)
  if parser.utf8_len > 0 then begin
    let lead = Bytes.get_uint8 parser.utf8_buf 0 in
    let need = utf8_seq_len lead in
    let j = ref off in
    let valid = ref true in
    while
      !valid && parser.utf8_len < need && !j < end_pos
      && is_utf8_continuation (Bytes.get_uint8 bytes !j)
    do
      Bytes.unsafe_set parser.utf8_buf parser.utf8_len
        (Bytes.unsafe_get bytes !j);
      parser.utf8_len <- parser.utf8_len + 1;
      incr j
    done;
    if parser.utf8_len < need && !j < end_pos then valid := false;
    if !valid && parser.utf8_len = need then begin
      let d = Bytes.get_utf_8_uchar parser.utf8_buf 0 in
      if Uchar.utf_decode_is_valid d then begin
        let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
        match key_event_of_code parser code with Some e -> emit e | None -> ()
      end;
      parser.utf8_len <- 0;
      i := !j
    end
    else if !valid then i := end_pos
    else begin
      parser.utf8_len <- 0;
      i := !j
    end
  end;
  (* Process remaining bytes *)
  while !i < end_pos do
    let b = Bytes.get_uint8 bytes !i in
    if b < 0x80 then begin
      (* ASCII fast path *)
      (match key_event_of_code parser b with Some e -> emit e | None -> ());
      incr i
    end
    else begin
      let seq_len = utf8_seq_len b in
      let remaining = end_pos - !i in
      if seq_len = 0 then
        (* Invalid lead byte - skip *)
        incr i
      else if remaining >= seq_len then begin
        (* Complete sequence available *)
        let d = Bytes.get_utf_8_uchar bytes !i in
        if Uchar.utf_decode_is_valid d then begin
          let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
          match key_event_of_code parser code with
          | Some e -> emit e
          | None -> ()
        end;
        i :=
          !i
          + if Uchar.utf_decode_is_valid d then Uchar.utf_decode_length d else 1
      end
      else begin
        (* Incomplete at chunk boundary - buffer it *)
        Bytes.blit bytes !i parser.utf8_buf 0 remaining;
        parser.utf8_len <- remaining;
        i := end_pos
      end
    end
  done

(* Token processing *)

let process_sequence_token parser seq ~on_event ~on_response =
  if seq = "\x1b[200~" || seq = "\x1b[201~" then ()
  else
    (* Prefer capability responses over generic escape parsing so DCS/OSC
       replies (e.g., Kitty detection) are handled correctly. *)
    match capability_event_of_sequence seq with
    | Event cap -> on_response (Response.Capability cap)
    | Drop -> ()
    | No_match ->
        let ev_opt =
          let seq_len = String.length seq in
          if
            seq_len >= 3
            && seq.[0] = '\x1b'
            && seq.[1] = '['
            && seq.[2] = 'M'
          then
            match parse_x10_normal_mouse_string seq 3 with
            | Some (e, _) -> Some (`User e)
            | None -> None
          else if seq = "\x1b\x1b" then Some (`User alt_escape_event)
          else
            let ev, _ = parse_escape_sequence parser seq 0 seq_len in
            ev
        in
        match ev_opt with
        | Some (`User e) -> on_event e
        | Some (`Response r) -> on_response r
        | None -> on_response (Response.Unknown seq)

let process_token parser ~on_event ~on_response = function
  | Tokenizer.Paste payload ->
      if payload <> "" then on_event (Paste payload)
  | Tokenizer.Sequence seq -> process_sequence_token parser seq ~on_event ~on_response
  | Tokenizer.Text s ->
      let bytes = Bytes.unsafe_of_string s in
      text_events_iter parser bytes 0 (String.length s) on_event

(* Public API *)

let feed parser bytes offset length ~now ~on_event ~on_response =
  Tokenizer.feed_iter parser.raw bytes offset length ~now
    ~emit:(process_token parser ~on_event ~on_response)

let drain parser ~now ~on_event ~on_response =
  Tokenizer.flush_expired_iter
    ~defer:(should_defer_pending parser)
    parser.raw now ~emit:(process_token parser ~on_event ~on_response)

let deadline parser = Tokenizer.deadline parser.raw
let pending parser = Tokenizer.pending parser.raw

let reset parser =
  Tokenizer.reset parser.raw;
  Buffer.clear parser.scratch_buffer;
  parser.utf8_len <- 0;
  clear_mouse_pressed parser
