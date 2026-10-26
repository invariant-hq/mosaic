let max_fields = 256
let max_intermediates = 4
let replacement = "\xEF\xBF\xBD"

module Params = struct
  (* Every field in order, -1 when absent. Parameter [i] starts at field
     [starts.(i)]; [starts] is empty when every parameter has one field. *)
  type t = { fields : int array; starts : int array }

  let empty = { fields = [||]; starts = [||] }

  let count p =
    if Array.length p.starts = 0 then Array.length p.fields
    else Array.length p.starts

  let start p i = if Array.length p.starts = 0 then i else p.starts.(i)

  let fields p i =
    let n = count p in
    if i < 0 || i >= n then 0
    else if i + 1 = n then Array.length p.fields - start p i
    else start p (i + 1) - start p i

  (* Field [j] of parameter [i], -1 when absent or out of range. *)
  let raw p i j =
    if j < 0 || j >= fields p i then -1 else p.fields.(start p i + j)

  (* Field 0 of parameter [i], the one read most. *)
  let first p i = if i < 0 || i >= count p then -1 else p.fields.(start p i)

  let pn p i ~default =
    let v = first p i in
    if v <= 0 then default else v

  let ps p i ~default =
    let v = first p i in
    if v < 0 then default else v

  let value p i =
    let v = first p i in
    if v < 0 then None else Some v

  let field p i j =
    let v = raw p i j in
    if v < 0 then None else Some v

  let pp ppf p =
    for i = 0 to count p - 1 do
      if i > 0 then Format.pp_print_char ppf ';';
      for j = 0 to fields p i - 1 do
        if j > 0 then Format.pp_print_char ppf ':';
        let v = raw p i j in
        if v >= 0 then Format.pp_print_int ppf v
      done
    done
end

type sequence = {
  marker : char option;
  params : Params.t;
  intermediates : string;
  final : char;
}

type opening = Dcs of sequence | Osc | Apc | Pm | Sos

type token =
  | Text of string
  | Control of char
  | Esc of { intermediates : string; final : char }
  | Csi of sequence
  | Open of opening
  | Data of string
  | Close of [ `St | `Bel | `Aborted ]

(* Formatting, in ECMA-48's notation *)

let c0_names =
  Array.of_list
    (String.split_on_char ' '
       "NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR SO SI DLE DC1 DC2 \
        DC3 DC4 NAK SYN ETB CAN EM SUB ESC IS4 IS3 IS2 IS1")

(* ECMA-48's C1 set; 08/04 is DEC's IND, which ECMA-48 withdrew. *)
let c1_names =
  Array.of_list
    (String.split_on_char ','
       ",,BPH,NBH,IND,NEL,SSA,ESA,HTS,HTJ,VTS,PLD,PLU,RI,SS2,SS3,DCS,PU1,PU2,STS,CCH,MW,SPA,EPA,SOS,,SCI,CSI,ST,OSC,PM,APC")

let pp_byte ppf c =
  if c = ' ' then Format.pp_print_string ppf "SP"
  else Format.pp_print_char ppf c

let pp_control ppf c =
  let b = Char.code c in
  if b < 0x20 then Format.pp_print_string ppf c0_names.(b)
  else if b = 0x7F then Format.pp_print_string ppf "DEL"
  else if b >= 0x80 && b <= 0x9F && c1_names.(b - 0x80) <> "" then
    Format.pp_print_string ppf c1_names.(b - 0x80)
  else if b >= 0x80 && b <= 0x9F then
    Format.fprintf ppf "ESC %c" (Char.chr (b - 0x40))
  else Format.fprintf ppf "%C" c

let pp_tail ppf intermediates final =
  String.iter (Format.fprintf ppf " %a" pp_byte) intermediates;
  Format.fprintf ppf " %a" pp_byte final

let pp_sequence ppf intro s =
  Format.pp_print_string ppf intro;
  if s.marker <> None || Params.count s.params > 0 then (
    Format.pp_print_char ppf ' ';
    Option.iter (Format.pp_print_char ppf) s.marker;
    Params.pp ppf s.params);
  pp_tail ppf s.intermediates s.final

(* A string in quotes, its UTF-8 kept and its controls escaped. *)
let pp_quoted ppf s =
  let add = Format.pp_print_string ppf in
  add "\"";
  String.iter
    (function
      | '"' -> add "\\\""
      | '\\' -> add "\\\\"
      | '\b' -> add "\\b"
      | '\t' -> add "\\t"
      | '\n' -> add "\\n"
      | '\r' -> add "\\r"
      | c when c < ' ' || c = '\x7f' ->
          Format.fprintf ppf "\\x%02X" (Char.code c)
      | c -> Format.pp_print_char ppf c)
    s;
  add "\""

let pp_token ppf = function
  | Text s | Data s -> pp_quoted ppf s
  | Control c -> pp_control ppf c
  | Esc { intermediates; final } ->
      Format.pp_print_string ppf "ESC";
      pp_tail ppf intermediates final
  | Csi s -> pp_sequence ppf "CSI" s
  | Open (Dcs s) -> pp_sequence ppf "DCS" s
  | Open Osc -> Format.pp_print_string ppf "OSC"
  | Open Apc -> Format.pp_print_string ppf "APC"
  | Open Pm -> Format.pp_print_string ppf "PM"
  | Open Sos -> Format.pp_print_string ppf "SOS"
  | Close `St -> Format.pp_print_string ppf "ST"
  | Close `Bel -> Format.pp_print_string ppf "BEL"
  | Close `Aborted -> Format.pp_print_string ppf "Aborted"

(* Parsers *)

type state =
  | Ground
  | Escape (* after ESC: intermediates, then the final byte *)
  | Csi_header (* after CSI: the header of a control sequence *)
  | Dcs_header (* after DCS: the header of a device control string *)
  | String (* the content of a control string *)
  | String_esc (* ESC in a control string: ST, or the end of the string *)

type t = {
  mutable state : state;
  (* The first bytes of a UTF-8 character that an earlier call cut. *)
  held : Bytes.t;
  mutable held_len : int;
  (* The header in progress. [marker] is '\000' for none; [current] is the
     field being read, -1 while absent; [sub] tells that the next field
     continues the current parameter. *)
  mutable marker : char;
  mutable malformed : bool;
  mutable in_params : bool;
  mutable in_intermediates : bool;
  mutable current : int;
  mutable sub : bool;
  fields : int array;
  starts : int array;
  mutable nfields : int;
  mutable nparams : int;
  intermediates : Bytes.t;
  mutable nintermediates : int;
  (* The open string, in [String] and [String_esc]; [None] for a malformed
     DCS, consumed without tokens. *)
  mutable string : opening option;
  (* [stop] was called in the feed in progress. *)
  mutable stopped : bool;
}

let create () =
  {
    state = Ground;
    held = Bytes.create 4;
    held_len = 0;
    marker = '\000';
    malformed = false;
    in_params = false;
    in_intermediates = false;
    current = -1;
    sub = false;
    fields = Array.make max_fields 0;
    starts = Array.make max_fields 0;
    nfields = 0;
    nparams = 0;
    intermediates = Bytes.create max_intermediates;
    nintermediates = 0;
    string = None;
    stopped = false;
  }

let reset p =
  p.state <- Ground;
  p.held_len <- 0;
  p.string <- None

let clear_header p =
  p.marker <- '\000';
  p.malformed <- false;
  p.in_params <- false;
  p.in_intermediates <- false;
  p.current <- -1;
  p.sub <- false;
  p.nfields <- 0;
  p.nparams <- 0;
  p.nintermediates <- 0

let enter p state =
  clear_header p;
  p.state <- state

let end_field p =
  if p.nfields >= max_fields then p.malformed <- true
  else (
    if not p.sub then (
      p.starts.(p.nparams) <- p.nfields;
      p.nparams <- p.nparams + 1);
    p.fields.(p.nfields) <- p.current;
    p.nfields <- p.nfields + 1);
  p.current <- -1

let collect p c =
  if p.nintermediates >= max_intermediates then p.malformed <- true
  else (
    Bytes.unsafe_set p.intermediates p.nintermediates c;
    p.nintermediates <- p.nintermediates + 1)

let intermediates p =
  if p.nintermediates = 0 then ""
  else Bytes.sub_string p.intermediates 0 p.nintermediates

(* The header read, its last field ended. *)
let sequence p final =
  let params =
    if p.nparams = 0 then Params.empty
    else
      {
        Params.fields = Array.sub p.fields 0 p.nfields;
        starts =
          (if p.nparams = p.nfields then [||]
           else Array.sub p.starts 0 p.nparams);
      }
  in
  {
    marker = (if p.marker = '\000' then None else Some p.marker);
    params;
    intermediates = intermediates p;
    final;
  }

let open_string p opening emit =
  p.string <- Some opening;
  p.state <- String;
  emit (Open opening)

let close p ending emit =
  (match p.string with Some _ -> emit (Close ending) | None -> ());
  p.string <- None;
  p.state <- Ground

(* ESC Fe: a C1 control, or the introducer of a sequence or string. *)
let c1 p final emit =
  match final with
  | '[' -> enter p Csi_header
  | 'P' -> enter p Dcs_header
  | ']' -> open_string p Osc emit
  | '_' -> open_string p Apc emit
  | '^' -> open_string p Pm emit
  | 'X' -> open_string p Sos emit
  | '\\' -> p.state <- Ground (* ST outside a string *)
  | _ ->
      p.state <- Ground;
      emit (Control (Char.unsafe_chr (Char.code final + 0x40)))

(* A C0 control or DEL inside an escape sequence or a header. *)
let control_in_sequence p b emit =
  if b = 0x1B then enter p Escape
  else if b = 0x18 || b = 0x1A then (
    p.state <- Ground;
    emit (Control (Char.unsafe_chr b)))
  else if b <> 0x7F && p.state <> Dcs_header then
    emit (Control (Char.unsafe_chr b))

let header_byte p b emit =
  let c = Char.unsafe_chr b in
  if b >= 0x30 && b <= 0x3F then
    if p.in_intermediates then p.malformed <- true
    else if b <= 0x39 then (
      p.in_params <- true;
      let d = b - 0x30 in
      p.current <-
        (if p.current < 0 then d
         else if p.current > (max_int - d) / 10 then max_int
         else (p.current * 10) + d))
    else if b <= 0x3B then (
      p.in_params <- true;
      end_field p;
      p.sub <- b = 0x3A)
    else if p.in_params || p.marker <> '\000' then p.malformed <- true
    else p.marker <- c
  else if b >= 0x20 && b <= 0x2F then (
    p.in_intermediates <- true;
    collect p c)
  else if b >= 0x40 && b <= 0x7E then (
    if p.in_params then end_field p;
    if p.state = Csi_header then (
      p.state <- Ground;
      if not p.malformed then emit (Csi (sequence p c)))
    else if p.malformed then (
      p.string <- None;
      p.state <- String)
    else open_string p (Dcs (sequence p c)) emit)
  else control_in_sequence p b emit

(* An ASCII byte in any state but [Ground] and [String]'s content, which
   [feed] reads in runs. *)
let rec step p b emit =
  match p.state with
  | Ground ->
      if b = 0x1B then enter p Escape else emit (Control (Char.unsafe_chr b))
  | Escape ->
      if b >= 0x20 && b <= 0x2F then collect p (Char.unsafe_chr b)
      else if b >= 0x30 && b <= 0x7E then
        if p.malformed then p.state <- Ground
        else if p.nintermediates = 0 && b >= 0x40 && b <= 0x5F then
          c1 p (Char.unsafe_chr b) emit
        else (
          p.state <- Ground;
          emit
            (Esc { intermediates = intermediates p; final = Char.unsafe_chr b }))
      else control_in_sequence p b emit
  | Csi_header | Dcs_header -> header_byte p b emit
  | String -> (
      if b = 0x1B then p.state <- String_esc
      else if b = 0x18 || b = 0x1A then (
        close p `Aborted emit;
        emit (Control (Char.unsafe_chr b)))
      else if b = 0x07 then
        match p.string with Some Osc -> close p `Bel emit | _ -> ())
  | String_esc ->
      if b = 0x5C then close p `St emit
      else (
        close p `Aborted emit;
        enter p Escape;
        step p b emit)

(* A non-ASCII character that is not text or content: one in a sequence,
   U+0080..U+009F, or U+FFFD in place of an ill-formed subsequence. *)
let non_ascii p ~replace emit =
  match p.state with
  | Ground -> if replace then emit (Text replacement)
  | String -> (
      match p.string with
      | Some _ when replace -> emit (Data replacement)
      | _ -> ())
  | String_esc ->
      (* The ESC began an escape sequence, which the character ends. *)
      close p `Aborted emit
  | Escape | Csi_header -> p.state <- Ground
  | Dcs_header ->
      p.string <- None;
      p.state <- String

let emit_run p s emit =
  match p.state with
  | Ground -> emit (Text s)
  | _ -> ( match p.string with Some _ -> emit (Data s) | None -> ())

let[@inline] byte b k = Char.code (Bytes.unsafe_get b k)
let[@inline] is_cont b k = byte b k land 0xC0 = 0x80

(* The UTF-8 character at [i], whose first byte is not ASCII: [n > 0] for a
   well-formed character of [n] bytes, [-k] for an ill-formed subsequence
   whose maximal subpart, one U+FFFD, is [k] bytes (Unicode §3.9), and [0]
   when [stop] cuts a character that may still be well formed. Unicode Table
   3-7: the first byte gives the length and the range of the second; the
   others are 0x80..0xBF. *)
let utf_8 b i stop =
  let c = byte b i in
  if c < 0xC2 || c > 0xF4 then -1
  else if i + 1 >= stop then 0
  else
    let c1 = byte b (i + 1) in
    let lo = if c = 0xE0 then 0xA0 else if c = 0xF0 then 0x90 else 0x80 in
    let hi = if c = 0xED then 0x9F else if c = 0xF4 then 0x8F else 0xBF in
    if c1 < lo || c1 > hi then -1
    else if c <= 0xDF then 2
    else if i + 2 >= stop then 0
    else if not (is_cont b (i + 2)) then -2
    else if c <= 0xEF then 3
    else if i + 3 >= stop then 0
    else if not (is_cont b (i + 3)) then -3
    else 4

(* The well-formed character at [i] is U+0080..U+009F. *)
let[@inline] is_c1 b i = byte b i = 0xC2 && byte b (i + 1) < 0xA0

(* The end of the run of text, or of string content, from [i]. *)
let rec scan b i stop ~content =
  if i >= stop then i
  else
    let c = byte b i in
    if (c >= 0x20 && c < 0x7F) || (content && c >= 0x08 && c <= 0x0D) then
      scan b (i + 1) stop ~content
    else if c >= 0x80 then
      let n = utf_8 b i stop in
      if n > 0 && not (is_c1 b i) then scan b (i + n) stop ~content else i
    else i

(* The character begun by an earlier call, completed from the first bytes of
   [b]: the number of bytes of [b] it takes. *)
let complete_held p b off len emit =
  let held = p.held_len in
  let k = min (Bytes.length p.held - held) len in
  Bytes.blit b off p.held held k;
  let n = utf_8 p.held 0 (held + k) in
  if n = 0 then (
    p.held_len <- held + k;
    k)
  else (
    p.held_len <- 0;
    (match p.state with
    | (Ground | String) when n > 0 && not (is_c1 p.held 0) ->
        emit_run p (Bytes.sub_string p.held 0 n) emit
    | _ -> non_ascii p ~replace:(n < 0) emit);
    abs n - held)

let feed p b ~off ~len emit =
  if off < 0 || len < 0 || off > Bytes.length b - len then
    invalid_arg "Ansi.Parser.feed";
  let stop = off + len in
  (* [run] starts the Text or Data run not yet emitted, -1 if none. *)
  let run = ref (-1) in
  let flush i =
    if !run >= 0 then (
      if i > !run then emit_run p (Bytes.sub_string b !run (i - !run)) emit;
      run := -1)
  in
  let i = ref off in
  p.stopped <- false;
  if p.held_len > 0 then i := off + complete_held p b off len emit;
  (* A stop takes effect after the token reported, so a run of text flushed
     before a byte stops before that byte. *)
  while !i < stop && not p.stopped do
    let c = Char.code (Bytes.unsafe_get b !i) in
    if c < 0x80 then (
      match p.state with
      | Ground when c >= 0x20 && c < 0x7F ->
          if !run < 0 then run := !i;
          i := scan b !i stop ~content:false
      | String when (c >= 0x20 && c < 0x7F) || (c >= 0x08 && c <= 0x0D) ->
          if !run < 0 then run := !i;
          i := scan b !i stop ~content:true
      | Ground | String ->
          flush !i;
          if not p.stopped then (
            step p c emit;
            incr i)
      | Csi_header | Dcs_header ->
          header_byte p c emit;
          incr i
      | Escape | String_esc ->
          step p c emit;
          incr i)
    else
      let n = utf_8 b !i stop in
      if n > 0 && (p.state = Ground || p.state = String) && not (is_c1 b !i)
      then (
        if !run < 0 then run := !i;
        i := scan b (!i + n) stop ~content:(p.state = String))
      else (
        flush !i;
        if p.stopped then ()
        else if n = 0 then (
          Bytes.blit b !i p.held 0 (stop - !i);
          p.held_len <- stop - !i;
          i := stop)
        else (
          non_ascii p ~replace:(n < 0) emit;
          i := !i + abs n))
  done;
  flush !i;
  !i - off

let stop p = p.stopped <- true

let finish p emit =
  if p.held_len > 0 then non_ascii p ~replace:true emit;
  (match p.state with String | String_esc -> close p `Aborted emit | _ -> ());
  reset p

let at_boundary p = p.state = Ground && p.held_len = 0

let parse s =
  let p = create () in
  let tokens = ref [] in
  let emit t = tokens := t :: !tokens in
  ignore (feed p (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s) emit);
  finish p emit;
  List.rev !tokens
