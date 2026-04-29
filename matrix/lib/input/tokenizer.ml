let esc = Char.chr 0x1b
let br_paste_start = "\x1b[200~"
let br_paste_end = "\x1b[201~"
let br_paste_start_len = String.length br_paste_start
let br_paste_end_len = String.length br_paste_end

(* Hard caps to prevent unbounded buffer growth from malformed input. *)
let max_paste_len = 1_048_576 (* 1 MB *)
let max_sequence_len = 4_096 (* 4 KB *)

let br_paste_end_failure =
  let len = br_paste_end_len in
  let fail = Array.make len 0 in
  let j = ref 0 in
  for i = 1 to len - 1 do
    while !j > 0 && br_paste_end.[!j] <> br_paste_end.[i] do
      j := fail.(!j - 1)
    done;
    if br_paste_end.[!j] = br_paste_end.[i] then incr j;
    fail.(i) <- !j
  done;
  fail

type token = Sequence of string | Text of string | Paste of string

type parser = {
  buffer : Buffer.t;
  mutable paste_buffer : bytes;
  mutable paste_len : int;
  mutable paste_match : int;
  mutable flush_deadline : float option;
  mutable deferred_timeout : bool;
  mutable mode : [ `Normal | `Paste ];
}

(* Timeout for ambiguous lone ESC (could be ESC key or start of Alt+key) *)
let ambiguity_timeout = 0.050

(* Timeout for clearly incomplete escape sequences (CSI, OSC, DCS, etc.) *)
let incomplete_seq_timeout = 0.100

let schedule_flush t now =
  t.deferred_timeout <- false;
  if t.mode = `Paste || Buffer.length t.buffer = 0 then t.flush_deadline <- None
  else
    let len = Buffer.length t.buffer in
    let delay =
      if len = 1 && Buffer.nth t.buffer 0 = esc then
        (* Lone ESC: ambiguous between ESC key and start of escape sequence *)
        ambiguity_timeout
      else if len >= 2 && Buffer.nth t.buffer 0 = esc then
        (* Incomplete escape sequence - use longer timeout *)
        incomplete_seq_timeout
      else
        (* Plain text with no ESC - shouldn't normally happen but use short *)
        ambiguity_timeout
    in
    t.flush_deadline <- Some (now +. delay)

let create () =
  {
    buffer = Buffer.create 128;
    paste_buffer = Bytes.create 128;
    paste_len = 0;
    paste_match = 0;
    flush_deadline = None;
    deferred_timeout = false;
    mode = `Normal;
  }

let pending t = Bytes.of_string (Buffer.contents t.buffer)

let reset t =
  Buffer.clear t.buffer;
  t.paste_len <- 0;
  t.paste_match <- 0;
  t.mode <- `Normal;
  t.flush_deadline <- None;
  t.deferred_timeout <- false

(* helpers *)

let emit_paste_tokens emit payload =
  if payload <> "" then emit (Paste payload);
  emit (Sequence br_paste_end)

let has_substring_at s ~sub ~pos =
  let sub_len = String.length sub in
  let limit = String.length s - sub_len in
  if pos < 0 || pos > limit then false
  else
    let rec loop i =
      if i = sub_len then true
      else if s.[pos + i] <> sub.[i] then false
      else loop (i + 1)
    in
    loop 0

let find_substring_from s sub start =
  let sub_len = String.length sub in
  let len = String.length s in
  let limit = len - sub_len in
  let rec scan i =
    if i > limit then -1
    else if has_substring_at s ~sub ~pos:i then i
    else scan (i + 1)
  in
  if sub_len = 0 || start > limit then -1 else scan start

let ensure_paste_capacity t needed =
  let required = t.paste_len + needed in
  if required > Bytes.length t.paste_buffer then (
    let new_cap = max required (Bytes.length t.paste_buffer * 2) in
    let buf = Bytes.create new_cap in
    Bytes.blit t.paste_buffer 0 buf 0 t.paste_len;
    t.paste_buffer <- buf)

let reset_paste_state t =
  t.paste_len <- 0;
  t.paste_match <- 0

let complete_paste t =
  let payload_len = t.paste_len - br_paste_end_len in
  let payload =
    if payload_len <= 0 then ""
    else Bytes.sub_string t.paste_buffer 0 payload_len
  in
  reset_paste_state t;
  t.mode <- `Normal;
  payload

let rec advance_paste_match current c =
  if c = br_paste_end.[current] then current + 1
  else if current = 0 then 0
  else advance_paste_match br_paste_end_failure.(current - 1) c

let add_paste_char t c =
  if t.paste_len < max_paste_len then (
    ensure_paste_capacity t 1;
    Bytes.unsafe_set t.paste_buffer t.paste_len c;
    t.paste_len <- t.paste_len + 1);
  (* Always advance the KMP match state so we detect the end marker even when
     the payload has been truncated. *)
  t.paste_match <- advance_paste_match t.paste_match c;
  t.paste_match = br_paste_end_len

(* escape-sequence parsing *)

let is_csi_final c =
  let code = Char.code c in
  (code >= 0x40 && code <= 0x7e) || code = 0x24 || code = 0x5e

type sequence_end = End of int | Restart of int | Incomplete

let find_x10_end s start len =
  let expected = start + 6 in
  let rec find_esc i =
    if i >= min expected len then Incomplete
    else if s.[i] = esc then Restart i
    else find_esc (i + 1)
  in
  if expected <= len then
    match find_esc (start + 3) with
    | End _ | Incomplete -> End expected
    | r -> r
  else find_esc (start + 3)

let find_sequence_end s start len =
  if start + 1 >= len then Incomplete
  else
    match s.[start + 1] with
    | '[' ->
        (* Mouse reporting: ESC [ M ... (3 bytes after M) *)
        if start + 2 < len && s.[start + 2] = 'M' then find_x10_end s start len
        else if
          start + 3 < len
          && s.[start + 2] = '['
          && s.[start + 3] >= 'A'
          && s.[start + 3] <= 'E'
        then End (start + 4)
        else
          let rec loop i =
            if i >= len then Incomplete
            else if s.[i] = esc then Restart i
            else if is_csi_final s.[i] then
              if s.[i] = '$' && s.[start + 2] = '?' && i + 1 >= len then
                Incomplete
              else if (s.[i] = '$' || s.[i] = '^') && i + 1 < len then
                loop (i + 1)
              else End (i + 1)
            else loop (i + 1)
          in
          loop (start + 2)
    | ']' ->
        (* OSC terminates with BEL or ST (ESC \) *)
        let rec loop i =
          if i >= len then Incomplete
          else
            let c = s.[i] in
            if c = '\x07' then End (i + 1)
            else if c = esc && i + 1 < len && s.[i + 1] = '\\' then End (i + 2)
            else if c = esc then Restart i
            else loop (i + 1)
        in
        loop (start + 2)
    | 'P' | '_' ->
        (* DCS / APC, terminated by ST *)
        let rec loop i =
          if i >= len then Incomplete
          else if s.[i] = esc && i + 1 < len && s.[i + 1] = '\\' then End (i + 2)
          else if s.[i] = esc then Restart i
          else loop (i + 1)
        in
        loop (start + 2)
    | 'O' ->
        (* SS3: ESC O <char> *)
        if start + 2 >= len then Incomplete
        else if s.[start + 2] = esc then Restart (start + 2)
        else End (start + 3)
    | _ ->
        (* Generic short escape: ESC X *)
        End (start + 2)

let is_partial_sgr_mouse s =
  let len = String.length s in
  len >= 3
  && s.[0] = esc
  && s.[1] = '['
  && s.[2] = '<'
  &&
  let rec loop i =
    if i >= len then true
    else match s.[i] with '0' .. '9' | ';' -> loop (i + 1) | _ -> false
  in
  loop 3

let extract_sequences_iter_from s emit =
  let len = String.length s in
  let rec loop pos =
    if pos >= len then ""
    else
      let c = s.[pos] in
      if c = esc then
        match find_sequence_end s pos len with
        | Incomplete ->
            (* incomplete sequence: keep the rest for later *)
            String.sub s pos (len - pos)
        | Restart restart ->
            (* A fresh ESC inside an incomplete sequence starts a new unit. The
               interrupted prefix is protocol noise, not user input. *)
            loop restart
        | End end_pos ->
            let seq = String.sub s pos (end_pos - pos) in
            emit (Sequence seq);
            loop end_pos
      else
        (* run of plain text until next ESC or end *)
        let rec find_esc i =
          if i >= len then len else if s.[i] = esc then i else find_esc (i + 1)
        in
        let stop = find_esc (pos + 1) in
        let txt = String.sub s pos (stop - pos) in
        emit (Text txt);
        loop stop
  in
  loop 0

(* state machine *)

let consume_paste_from_string_iter t s start stop emit =
  if start >= stop then None
  else
    let rec loop i =
      if i >= stop then None
      else
        let matched = add_paste_char t s.[i] in
        if matched then
          let payload = complete_paste t in
          emit_paste_tokens emit payload;
          Some (i + 1)
        else loop (i + 1)
    in
    loop start

let consume_paste_from_bytes_iter t bytes start stop emit =
  if start >= stop then None
  else
    let rec loop i =
      if i >= stop then None
      else
        let matched = add_paste_char t (Bytes.unsafe_get bytes i) in
        if matched then
          let payload = complete_paste t in
          emit_paste_tokens emit payload;
          Some (i + 1)
        else loop (i + 1)
    in
    loop start

let rec process_iter t now emit =
  if t.mode = `Paste then ()
  else if Buffer.length t.buffer = 0 then ()
  else
    let buf_str = Buffer.contents t.buffer in
    Buffer.clear t.buffer;
    let len = String.length buf_str in
    let start_idx = find_substring_from buf_str br_paste_start 0 in
    if start_idx < 0 then
      let rem = extract_sequences_iter_from buf_str emit in
      if rem <> "" then
        if String.length rem > max_sequence_len then (
          (* Incomplete sequence exceeded the safety cap — treat as plain text
             rather than buffering without bound. *)
          t.flush_deadline <- None;
          emit (Text rem))
        else (
          Buffer.add_string t.buffer rem;
          schedule_flush t now)
      else (
        t.flush_deadline <- None)
    else
      let before = String.sub buf_str 0 start_idx in
      let after_start = start_idx + br_paste_start_len in
      let after_len = len - after_start in
      let after =
        if after_len > 0 then String.sub buf_str after_start after_len else ""
      in
      let rem = extract_sequences_iter_from before emit in
      reset_paste_state t;
      t.mode <- `Paste;
      t.flush_deadline <- None;
      emit (Sequence br_paste_start);
      let rem_stop =
        if rem = "" then None
        else consume_paste_from_string_iter t rem 0 (String.length rem) emit
      in
      if t.mode = `Normal then (
        (match rem_stop with
        | Some idx when idx < String.length rem ->
            Buffer.add_substring t.buffer rem idx (String.length rem - idx)
        | _ -> ());
        if after <> "" then Buffer.add_string t.buffer after;
        t.flush_deadline <- None;
        process_iter t now emit)
      else
        let after_stop =
          if after = "" then None
          else
            consume_paste_from_string_iter t after 0 (String.length after) emit
        in
        if t.mode = `Normal then (
          (match after_stop with
          | Some idx when idx < String.length after ->
              Buffer.add_substring t.buffer after idx (String.length after - idx)
          | _ -> ());
          t.flush_deadline <- None;
          process_iter t now emit)

let feed_iter t bytes off len ~now ~emit =
  if off < 0 || len < 0 || off + len > Bytes.length bytes then
    invalid_arg "Input_tokenizer.feed: out of bounds";
  if t.mode = `Paste then (
    let stop_opt = consume_paste_from_bytes_iter t bytes off (off + len) emit in
    match stop_opt with
    | None -> ()
    | Some stop ->
        let remaining = off + len - stop in
        if remaining > 0 then Buffer.add_subbytes t.buffer bytes stop remaining;
        t.flush_deadline <- None;
        process_iter t now emit)
  else (
    t.deferred_timeout <- false;
    Buffer.add_subbytes t.buffer bytes off len;
    t.flush_deadline <- None;
    process_iter t now emit)

let feed t bytes off len ~now =
  let acc = ref [] in
  feed_iter t bytes off len ~now ~emit:(fun token -> acc := token :: !acc);
  List.rev !acc

let deadline t = t.flush_deadline
let wake_deferred t = if t.deferred_timeout then t.flush_deadline <- Some 0.

let flush_expired_iter ?(defer = fun _ -> false) t now ~emit =
  match
    ( t.mode = `Normal,
      t.deferred_timeout,
      match t.flush_deadline with Some expiry -> now >= expiry | None -> false
    )
  with
  | true, true, _ | true, false, true ->
      if Buffer.length t.buffer = 0 then (
        t.deferred_timeout <- false;
        ())
      else
        let leftover = Buffer.contents t.buffer in
        if is_partial_sgr_mouse leftover || defer leftover then (
          t.flush_deadline <- None;
          t.deferred_timeout <- true;
          ())
        else (
          t.flush_deadline <- None;
          t.deferred_timeout <- false;
          Buffer.clear t.buffer;
          if leftover <> "" then emit (Sequence leftover))
  | _ -> ()

let flush_expired ?defer t now =
  let acc = ref [] in
  flush_expired_iter ?defer t now ~emit:(fun token -> acc := token :: !acc);
  List.rev !acc
