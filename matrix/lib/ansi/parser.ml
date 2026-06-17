let max_escape_length = 256
let max_osc_length = 8192

type control =
  | CUU of int
  | CUD of int
  | CUF of int
  | CUB of int
  | CNL of int
  | CPL of int
  | CHA of int
  | VPA of int
  | CUP of int * int
  | ED of int
  | EL of int
  | IL of int
  | DL of int
  | DCH of int
  | ICH of int
  | OSC of int * string
  | DCS of string
  | APC of string
  | PM of string
  | SOS of string
  | Hyperlink of ((string * string) list * string) option
  | Reset
  | DECSC
  | DECRC
  | Unknown of string

type sgr_attr =
  [ `Reset
  | `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Inverse
  | `Hidden
  | `Strikethrough
  | `Overline
  | `Framed
  | `Encircled
  | `No_bold
  | `No_dim
  | `No_italic
  | `No_underline
  | `No_blink
  | `No_inverse
  | `No_hidden
  | `No_strikethrough
  | `No_overline
  | `No_framed
  | `No_encircled
  | `Fg of Color.t
  | `Bg of Color.t ]

type token = Text of string | SGR of sgr_attr list | Control of control

let is_final_byte c =
  let code = Char.code c in
  code >= 0x40 && code <= 0x7e

(* Parse SGR parameters - tail-recursive, no refs *)
let parse_sgr_params params len =
  let rec loop i acc =
    if i >= len then List.rev acc
    else
      let param = params.(i) in
      match param with
      | 0 -> loop (i + 1) (`Reset :: acc)
      | 1 -> loop (i + 1) (`Bold :: acc)
      | 2 -> loop (i + 1) (`Dim :: acc)
      | 3 -> loop (i + 1) (`Italic :: acc)
      | 4 -> loop (i + 1) (`Underline :: acc)
      | 21 -> loop (i + 1) (`Double_underline :: acc)
      | 5 -> loop (i + 1) (`Blink :: acc)
      | 7 -> loop (i + 1) (`Inverse :: acc)
      | 8 -> loop (i + 1) (`Hidden :: acc)
      | 9 -> loop (i + 1) (`Strikethrough :: acc)
      | 53 -> loop (i + 1) (`Overline :: acc)
      | 51 -> loop (i + 1) (`Framed :: acc)
      | 52 -> loop (i + 1) (`Encircled :: acc)
      | 22 -> loop (i + 1) (`No_dim :: `No_bold :: acc)
      | 23 -> loop (i + 1) (`No_italic :: acc)
      | 24 -> loop (i + 1) (`No_underline :: acc)
      | 25 -> loop (i + 1) (`No_blink :: acc)
      | 27 -> loop (i + 1) (`No_inverse :: acc)
      | 28 -> loop (i + 1) (`No_hidden :: acc)
      | 29 -> loop (i + 1) (`No_strikethrough :: acc)
      | 54 -> loop (i + 1) (`No_encircled :: `No_framed :: acc)
      | 55 -> loop (i + 1) (`No_overline :: acc)
      | 39 -> loop (i + 1) (`Fg Color.default :: acc)
      | 49 -> loop (i + 1) (`Bg Color.default :: acc)
      | n when 30 <= n && n <= 37 ->
          loop (i + 1) (`Fg (Color.of_palette_index (n - 30)) :: acc)
      | n when 90 <= n && n <= 97 ->
          loop (i + 1) (`Fg (Color.of_palette_index (n - 90 + 8)) :: acc)
      | n when 40 <= n && n <= 47 ->
          loop (i + 1) (`Bg (Color.of_palette_index (n - 40)) :: acc)
      | n when 100 <= n && n <= 107 ->
          loop (i + 1) (`Bg (Color.of_palette_index (n - 100 + 8)) :: acc)
      | 38 when i + 2 < len && params.(i + 1) = 5 ->
          let idx = params.(i + 2) in
          loop (i + 3) (`Fg (Color.of_palette_index idx) :: acc)
      | 38 when i + 4 < len && params.(i + 1) = 2 ->
          let r = params.(i + 2) in
          let g = params.(i + 3) in
          let b = params.(i + 4) in
          loop (i + 5) (`Fg (Color.of_rgb r g b) :: acc)
      | 48 when i + 2 < len && params.(i + 1) = 5 ->
          let idx = params.(i + 2) in
          loop (i + 3) (`Bg (Color.of_palette_index idx) :: acc)
      | 48 when i + 4 < len && params.(i + 1) = 2 ->
          let r = params.(i + 2) in
          let g = params.(i + 3) in
          let b = params.(i + 4) in
          loop (i + 5) (`Bg (Color.of_rgb r g b) :: acc)
      | _ -> loop (i + 1) acc
  in
  loop 0 []

(* Maximum CSI parameters supported. 32 covers even complex SGR sequences. *)
let max_csi_params = 32
let csi_param_missing = -1

(* Parse CSI body into parameters - returns param count *)
let parse_csi_params body body_len params max_params =
  let rec parse i current saw_digit count =
    if i >= body_len then (
      if count < max_params then
        params.(count) <- (if saw_digit then current else csi_param_missing);
      min (count + 1) max_params)
    else
      match body.[i] with
      | ';' ->
          if count < max_params then
            params.(count) <- (if saw_digit then current else csi_param_missing);
          parse (i + 1) 0 false (count + 1)
      | '0' .. '9' as c ->
          parse (i + 1) ((current * 10) + Char.code c - 48) true count
      | _ -> parse (i + 1) current saw_digit count
  in
  if body_len > 0 then parse 0 0 false 0 else 0

let parse_csi ~params body final : token option =
  let body_len = String.length body in
  (* Zero out params for reuse - only need to clear used portion *)
  let param_count = parse_csi_params body body_len params max_csi_params in
  let get n default =
    if n < param_count then
      let value = params.(n) in
      if value = csi_param_missing then default else value
    else default
  in
  let sgr_param n =
    if n < param_count then
      let value = params.(n) in
      if value = csi_param_missing then 0 else value
    else 0
  in
  match final with
  | 'A' -> Some (Control (CUU (get 0 1)))
  | 'B' -> Some (Control (CUD (get 0 1)))
  | 'C' -> Some (Control (CUF (get 0 1)))
  | 'D' -> Some (Control (CUB (get 0 1)))
  | 'E' -> Some (Control (CNL (get 0 1)))
  | 'F' -> Some (Control (CPL (get 0 1)))
  | 'G' -> Some (Control (CHA (get 0 1)))
  | 'd' -> Some (Control (VPA (get 0 1)))
  | 'H' | 'f' -> Some (Control (CUP (get 0 1, get 1 1)))
  | 'J' -> Some (Control (ED (get 0 0)))
  | 'K' -> Some (Control (EL (get 0 0)))
  | 'L' -> Some (Control (IL (get 0 1)))
  | 'M' -> Some (Control (DL (get 0 1)))
  | 'P' -> Some (Control (DCH (get 0 1)))
  | '@' -> Some (Control (ICH (get 0 1)))
  | 'm' ->
      let attrs =
        if param_count = 0 then [ `Reset ]
        else begin
          for i = 0 to param_count - 1 do
            params.(i) <- sgr_param i
          done;
          parse_sgr_params params param_count
        end
      in
      Some (SGR attrs)
  | 's' -> Some (Control DECSC) (* CSI s - save cursor position *)
  | 'u' -> Some (Control DECRC) (* CSI u - restore cursor position *)
  | _ -> Some (Control (Unknown (Printf.sprintf "CSI[%s%c" body final)))

(* Parse OSC 8 hyperlink parameters and URL *)
let parse_hyperlink data =
  let len = String.length data in
  (* Find first semicolon *)
  let rec find_semi i =
    if i >= len then -1 else if data.[i] = ';' then i else find_semi (i + 1)
  in
  match find_semi 0 with
  | -1 -> None
  | semi1 ->
      let params_str = String.sub data 0 semi1 in
      let link = String.sub data (semi1 + 1) (len - semi1 - 1) in
      if params_str = "" && link = "" then Some (Hyperlink None)
      else if params_str = "" then Some (Hyperlink (Some ([], link)))
      else
        (* Parse key=value pairs from params *)
        let rec parse_kv i acc =
          if i >= String.length params_str then List.rev acc
          else
            (* Find next colon or end *)
            let rec find_delim j =
              if j >= String.length params_str then j
              else if params_str.[j] = ':' then j
              else find_delim (j + 1)
            in
            let next_colon = find_delim i in
            let segment = String.sub params_str i (next_colon - i) in
            (* Find = in segment *)
            let rec find_eq k =
              if k >= String.length segment then -1
              else if segment.[k] = '=' then k
              else find_eq (k + 1)
            in
            let acc' =
              match find_eq 0 with
              | -1 -> acc
              | eq_pos ->
                  let key = String.sub segment 0 eq_pos in
                  let value =
                    String.sub segment (eq_pos + 1)
                      (String.length segment - eq_pos - 1)
                  in
                  (key, value) :: acc
            in
            parse_kv (next_colon + 1) acc'
        in
        let kv_pairs = parse_kv 0 [] in
        Some (Hyperlink (Some (kv_pairs, link)))

let parse_osc body : token =
  match String.index_opt body ';' with
  | None -> Control (Unknown ("OSC " ^ body))
  | Some semi -> (
      let code_str = String.sub body 0 semi in
      let data = String.sub body (semi + 1) (String.length body - semi - 1) in
      match int_of_string_opt code_str with
      | Some 8 -> (
          match parse_hyperlink data with
          | Some c -> Control c
          | None -> Control (Unknown ("OSC8 " ^ data)))
      | Some code -> Control (OSC (code, data))
      | None -> Control (Unknown ("OSC " ^ body)))

type string_control = Dcs | Apc | Pm | Sos

type state =
  | Normal
  | Escape
  | Csi
  | Osc
  | Osc_escape
  | String_control of string_control
  | String_control_escape of string_control

let control_of_string_control kind body =
  match kind with
  | Dcs -> DCS body
  | Apc -> APC body
  | Pm -> PM body
  | Sos -> SOS body

type t = {
  (* Pending bytes buffer - for incomplete escape sequences at chunk
     boundaries *)
  mutable pending_buf : bytes;
  mutable pending_len : int;
  (* State machine *)
  mutable state : state;
  (* Escape sequence accumulators *)
  csi_buf : Buffer.t;
  osc_buf : Buffer.t;
  string_control_buf : Buffer.t;
  (* Decoded text accumulator *)
  text_buf : Buffer.t;
  (* UTF-8 incomplete sequence buffer (max 3 bytes needed) *)
  utf8_buf : bytes;
  mutable utf8_len : int;
  (* Pre-allocated CSI params array to avoid allocation per CSI sequence *)
  csi_params : int array;
}

let create () =
  {
    pending_buf = Bytes.create 256;
    pending_len = 0;
    state = Normal;
    csi_buf = Buffer.create 32;
    osc_buf = Buffer.create 64;
    string_control_buf = Buffer.create 64;
    text_buf = Buffer.create 128;
    utf8_buf = Bytes.create 4;
    utf8_len = 0;
    csi_params = Array.make max_csi_params 0;
  }

let reset p =
  p.pending_len <- 0;
  p.state <- Normal;
  Buffer.clear p.csi_buf;
  Buffer.clear p.osc_buf;
  Buffer.clear p.string_control_buf;
  Buffer.clear p.text_buf;
  p.utf8_len <- 0

let has_pending p = p.pending_len > 0 || p.state <> Normal || p.utf8_len > 0
let pending p = Bytes.sub p.pending_buf 0 p.pending_len

let[@inline] utf8_sequence_len b =
  if b < 0x80 then 1
  else if b >= 0xC2 && b <= 0xDF then 2
  else if b >= 0xE0 && b <= 0xEF then 3
  else if b >= 0xF0 && b <= 0xF4 then 4
  else 0

let[@inline] emit_utf8_decode buf bytes off =
  let d = Bytes.get_utf_8_uchar bytes off in
  if Uchar.utf_decode_is_valid d then (
    Buffer.add_utf_8_uchar buf (Uchar.utf_decode_uchar d);
    true)
  else (
    Buffer.add_utf_8_uchar buf Uchar.rep;
    false)

(* Decode UTF-8 bytes to text_buf. Handles chunk boundaries for streaming
   input. *)
let decode_utf8 p src off len =
  if len <= 0 then ()
  else begin
    let buf = p.text_buf in
    let end_pos = off + len in
    let i = ref off in
    (* Complete any pending incomplete sequence from previous chunk *)
    if p.utf8_len > 0 then begin
      let lead = Bytes.get_uint8 p.utf8_buf 0 in
      let expected = utf8_sequence_len lead in
      if expected = 0 then begin
        Buffer.add_utf_8_uchar buf Uchar.rep;
        p.utf8_len <- 0
      end;
      while p.utf8_len > 0 && p.utf8_len < expected && !i < end_pos do
        Bytes.set p.utf8_buf p.utf8_len (Bytes.get src !i);
        p.utf8_len <- p.utf8_len + 1;
        incr i
      done;
      if p.utf8_len = expected then begin
        ignore (emit_utf8_decode buf p.utf8_buf 0 : bool);
        p.utf8_len <- 0
      end
    end;
    (* Process remaining bytes. Only decode after proving the complete
       sequence is inside the caller's logical slice. *)
    while !i < end_pos do
      let b = Bytes.get_uint8 src !i in
      let expected = utf8_sequence_len b in
      if expected = 0 then begin
        Buffer.add_utf_8_uchar buf Uchar.rep;
        incr i
      end
      else if !i + expected > end_pos then begin
        let available = end_pos - !i in
        Bytes.blit src !i p.utf8_buf 0 available;
        p.utf8_len <- available;
        i := end_pos
      end
      else
        begin if emit_utf8_decode buf src !i then i := !i + expected else incr i
        end
    done
  end

(* Flush incomplete UTF-8 at EOF - emit replacement for incomplete sequence *)
let flush_utf8 p =
  if p.utf8_len > 0 then begin
    Buffer.add_utf_8_uchar p.text_buf Uchar.rep;
    p.utf8_len <- 0
  end

(* Emit text token if buffer non-empty - callback version *)
let[@inline] emit_text_cb p emit =
  if Buffer.length p.text_buf > 0 then (
    emit (Text (Buffer.contents p.text_buf));
    Buffer.clear p.text_buf)

(* Main parsing loop - callback-based, zero list allocation *)
let feed p src ~off ~len emit =
  let eof = len = 0 in
  (* Determine effective input: pending bytes + new input *)
  let input, input_off, input_len =
    if p.pending_len = 0 then (src, off, len)
    else
      (* Append new data to pending buffer *)
      let total = p.pending_len + len in
      if total > Bytes.length p.pending_buf then (
        let new_size = max total (Bytes.length p.pending_buf * 2) in
        let new_buf = Bytes.create new_size in
        Bytes.blit p.pending_buf 0 new_buf 0 p.pending_len;
        p.pending_buf <- new_buf);
      Bytes.blit src off p.pending_buf p.pending_len len;
      p.pending_len <- p.pending_len + len;
      (p.pending_buf, 0, p.pending_len)
  in
  let rec loop pos =
    if pos >= input_len then pos
    else
      match p.state with
      | Normal ->
          (* Scan for ESC, accumulating text *)
          let rec scan_text start i =
            if i >= input_len then (
              (* Decode text from start to i *)
              if i > start then
                decode_utf8 p input (input_off + start) (i - start);
              i)
            else
              let c = Bytes.get input (input_off + i) in
              if c = '\x1b' then (
                (* Decode text up to ESC *)
                if i > start then
                  decode_utf8 p input (input_off + start) (i - start);
                emit_text_cb p emit;
                p.state <- Escape;
                loop (i + 1))
              else scan_text start (i + 1)
          in
          scan_text pos pos
      | Escape -> (
          let c = Bytes.get input (input_off + pos) in
          match c with
          | '[' ->
              Buffer.clear p.csi_buf;
              p.state <- Csi;
              loop (pos + 1)
          | ']' ->
              Buffer.clear p.osc_buf;
              p.state <- Osc;
              loop (pos + 1)
          | 'P' ->
              Buffer.clear p.string_control_buf;
              p.state <- String_control Dcs;
              loop (pos + 1)
          | '_' ->
              Buffer.clear p.string_control_buf;
              p.state <- String_control Apc;
              loop (pos + 1)
          | '^' ->
              Buffer.clear p.string_control_buf;
              p.state <- String_control Pm;
              loop (pos + 1)
          | 'X' ->
              Buffer.clear p.string_control_buf;
              p.state <- String_control Sos;
              loop (pos + 1)
          | 'c' ->
              p.state <- Normal;
              emit (Control Reset);
              loop (pos + 1)
          | '7' ->
              p.state <- Normal;
              emit (Control DECSC);
              loop (pos + 1)
          | '8' ->
              p.state <- Normal;
              emit (Control DECRC);
              loop (pos + 1)
          | '%' | '(' | ')' | '*' | '+' ->
              (* Character set selection - skip next byte *)
              if pos + 1 < input_len then (
                p.state <- Normal;
                loop (pos + 2))
              else (
                (* Need more data, back up to ESC and reset state *)
                p.state <- Normal;
                pos - 1)
          | _ ->
              (* Unknown escape, emit ESC as text and reprocess current char *)
              Buffer.add_char p.text_buf '\x1b';
              p.state <- Normal;
              loop pos)
      | Csi ->
          let c = Bytes.get input (input_off + pos) in
          if Buffer.length p.csi_buf >= max_escape_length then (
            (* Overflow - emit unknown and reset *)
            p.state <- Normal;
            Buffer.clear p.csi_buf;
            emit (Control (Unknown "CSI_TOO_LONG"));
            loop (pos + 1))
          else if is_final_byte c then (
            let body = Buffer.contents p.csi_buf in
            let tok =
              match parse_csi ~params:p.csi_params body c with
              | Some t -> t
              | None -> Control (Unknown "")
            in
            p.state <- Normal;
            emit tok;
            loop (pos + 1))
          else (
            Buffer.add_char p.csi_buf c;
            loop (pos + 1))
      | Osc ->
          let c = Bytes.get input (input_off + pos) in
          if c = '\x07' then (
            (* BEL terminates OSC *)
            let body = Buffer.contents p.osc_buf in
            let tok = parse_osc body in
            p.state <- Normal;
            emit tok;
            loop (pos + 1))
          else if c = '\x1b' then (
            (* Potential ST terminator *)
            p.state <- Osc_escape;
            loop (pos + 1))
          else if Buffer.length p.osc_buf >= max_osc_length then (
            (* Overflow - emit unknown and reset *)
            p.state <- Normal;
            Buffer.clear p.osc_buf;
            emit (Control (Unknown "OSC_TOO_LONG"));
            loop (pos + 1))
          else (
            Buffer.add_char p.osc_buf c;
            loop (pos + 1))
      | Osc_escape ->
          let c = Bytes.get input (input_off + pos) in
          if c = '\\' then (
            (* ST terminator complete *)
            let body = Buffer.contents p.osc_buf in
            let tok = parse_osc body in
            p.state <- Normal;
            emit tok;
            loop (pos + 1))
          else (
            (* Not ST - add ESC to buffer and reprocess *)
            Buffer.add_char p.osc_buf '\x1b';
            p.state <- Osc;
            loop pos)
      | String_control kind ->
          let c = Bytes.get input (input_off + pos) in
          if c = '\x1b' then (
            p.state <- String_control_escape kind;
            loop (pos + 1))
          else if Buffer.length p.string_control_buf >= max_osc_length then (
            p.state <- Normal;
            Buffer.clear p.string_control_buf;
            emit (Control (Unknown "STRING_CONTROL_TOO_LONG"));
            loop (pos + 1))
          else (
            Buffer.add_char p.string_control_buf c;
            loop (pos + 1))
      | String_control_escape kind ->
          let c = Bytes.get input (input_off + pos) in
          if c = '\\' then (
            let body = Buffer.contents p.string_control_buf in
            p.state <- Normal;
            emit (Control (control_of_string_control kind body));
            loop (pos + 1))
          else (
            Buffer.add_char p.string_control_buf '\x1b';
            p.state <- String_control kind;
            loop pos)
  in
  let consumed = loop 0 in
  (* Handle remaining bytes *)
  let remaining = input_len - consumed in
  if remaining > 0 then (
    (* Copy remaining to pending buffer *)
    if input == p.pending_buf then (
      if
        (* Already using pending_buf - shift remaining bytes down if consumed >
           0 *)
        consumed > 0
      then Bytes.blit p.pending_buf consumed p.pending_buf 0 remaining)
    else (
      (* New input - copy to pending buffer *)
      if remaining > Bytes.length p.pending_buf then
        p.pending_buf <- Bytes.create (max remaining 256);
      Bytes.blit input (input_off + consumed) p.pending_buf 0 remaining);
    p.pending_len <- remaining)
  else p.pending_len <- 0;
  (* Flush incomplete UTF-8 on EOF *)
  if eof then flush_utf8 p;
  emit_text_cb p emit

let parse s =
  let p = create () in
  let acc = ref [] in
  let collect tok = acc := tok :: !acc in
  feed p (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s) collect;
  (* Flush with EOF *)
  feed p Bytes.empty ~off:0 ~len:0 collect;
  List.rev !acc
