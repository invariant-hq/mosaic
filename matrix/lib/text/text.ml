open StdLabels

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]
type line_break_kind = [ `LF | `CR | `CRLF ]

let default_tab_width = 2

(* ASCII Helpers *)

let[@inline] normalize_tab_width w = if w <= 0 then default_tab_width else w

let check_sub name str ~pos ~len =
  let str_len = String.length str in
  if pos < 0 then invalid_arg (name ^ ": negative position");
  if len < 0 then invalid_arg (name ^ ": negative length");
  if pos > str_len || len > str_len - pos then
    invalid_arg (name ^ ": substring out of bounds")

(* Width of an ASCII byte (0-127). Tab returns tab_width, printable (0x20-0x7E)
   returns 1, control characters return 0. Two comparisons instead of a table
   lookup (which costs 3 dependent memory loads). *)
let[@inline] ascii_width ~tab_width b =
  if b = 0x09 then tab_width else if b >= 0x20 && b <= 0x7E then 1 else 0

(* Check if 4 consecutive bytes are all ASCII (< 128). Uses native int
   operations only — zero allocation on 64-bit OCaml. *)
let[@inline] is_ascii_4 str i =
  let c0 = Char.code (String.unsafe_get str i) in
  let c1 = Char.code (String.unsafe_get str (i + 1)) in
  let c2 = Char.code (String.unsafe_get str (i + 2)) in
  let c3 = Char.code (String.unsafe_get str (i + 3)) in
  c0 lor c1 lor c2 lor c3 < 128

let rec is_ascii_only_tail str len j =
  j >= len
  || Char.code (String.unsafe_get str j) < 128
     && is_ascii_only_tail str len (j + 1)

let rec is_ascii_only str len i =
  if i + 4 <= len then is_ascii_4 str i && is_ascii_only str len (i + 4)
  else is_ascii_only_tail str len i

(* Width Predicates *)

let[@inline] is_regional_indicator cp = cp >= 0x1F1E6 && cp <= 0x1F1FF

(* Detects Indic virama characters (U+094D, U+09CD, U+0A4D, U+0ACD, U+0B4D,
   U+0BCD, U+0C4D, U+0CCD, U+0D4D). The virama joins two consonants into a
   conjunct (e.g. क + ् + ष = क्ष) which may be wider than a single cell. We check
   specific virama codepoints rather than the broader GeneralCategory=Mn class
   because the conjunct-width logic should only fire for actual virama
   sequences, not for arbitrary combining marks like diacriticals. *)
let[@inline] is_virama cp = cp land 0x7F = 0x4D && cp >= 0x094D && cp <= 0x0D4D

let[@inline] is_devanagari_base cp =
  (cp >= 0x0915 && cp <= 0x0939) || (cp >= 0x0958 && cp <= 0x095F)

(* Codepoint Width *)

let[@inline] codepoint_width_wcwidth ~tab_width cp =
  if cp < 0x80 then
    if cp = 0x09 then tab_width else if cp < 32 || cp = 127 then 0 else 1
  else
    let w = Unicode.tty_width_hint (Uchar.unsafe_of_int cp) in
    if w = -1 then 0 else w

let[@inline] codepoint_width_unicode ~tab_width cp =
  if cp < 0x80 then
    if cp = 0x09 then tab_width else if cp < 32 || cp = 127 then -1 else 1
  else Unicode.tty_width_hint (Uchar.unsafe_of_int cp)

let width_flag_has_width = 1
let width_flag_ri_pair = 2
let width_flag_virama = 4

let rec grapheme_width_unicode_loop str limit tab_width i width flags =
  if i >= limit then width
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    let cp_width = codepoint_width_unicode ~tab_width cp in
    let has_width = flags land width_flag_has_width <> 0 in
    let is_ri_pair = flags land width_flag_ri_pair <> 0 in
    let has_virama = flags land width_flag_virama <> 0 in
    if cp = 0xFE0F then
      let new_width = if has_width && width = 1 then 2 else width in
      grapheme_width_unicode_loop str limit tab_width next new_width flags
    else if is_virama cp then
      grapheme_width_unicode_loop str limit tab_width next width
        (flags lor width_flag_virama)
    else if is_regional_indicator cp then
      if is_ri_pair then
        grapheme_width_unicode_loop str limit tab_width next (width + cp_width)
          (flags lor width_flag_has_width land lnot width_flag_ri_pair
         land lnot width_flag_virama)
      else
        let new_w = if not has_width then cp_width else width in
        grapheme_width_unicode_loop str limit tab_width next new_w
          (flags lor width_flag_has_width lor width_flag_ri_pair
         land lnot width_flag_virama)
    else if has_width && has_virama && is_devanagari_base cp then
      let add = if cp <> 0x0930 && cp_width > 0 then cp_width else 0 in
      grapheme_width_unicode_loop str limit tab_width next (width + add)
        (flags lor width_flag_has_width land lnot width_flag_virama)
    else if (not has_width) && cp_width > 0 then
      grapheme_width_unicode_loop str limit tab_width next cp_width
        (flags lor width_flag_has_width land lnot width_flag_virama)
    else
      grapheme_width_unicode_loop str limit tab_width next width
        (flags land lnot width_flag_virama)

let rec grapheme_width_wcwidth_loop str limit tab_width i acc =
  if i >= limit then acc
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    grapheme_width_wcwidth_loop str limit tab_width next
      (acc + codepoint_width_wcwidth ~tab_width cp)

let cluster_width ~method_ ~tab_width str off len =
  let limit = off + len in
  match method_ with
  | `Wcwidth -> grapheme_width_wcwidth_loop str limit tab_width off 0
  | `Unicode | `No_zwj ->
      grapheme_width_unicode_loop str limit tab_width off 0 0

(* Grapheme Segmentation *)

let rec find_boundary_loop seg str limit pos =
  if pos >= limit then limit
  else
    let d = String.get_utf_8_uchar str pos in
    let u = Uchar.utf_decode_uchar d in
    if Uuseg_grapheme_cluster.check_boundary seg u then pos
    else find_boundary_loop seg str limit (pos + Uchar.utf_decode_length d)

(* Find the next grapheme cluster boundary starting at [start]. Returns the byte
   offset after the grapheme cluster. When [ignore_zwj] is true, GB11 is
   disabled (no emoji ZWJ sequences). *)
let next_boundary seg ~ignore_zwj str start limit =
  if start >= limit then limit
  else (
    Uuseg_grapheme_cluster.reset seg;
    Uuseg_grapheme_cluster.set_ignore_zwj seg ignore_zwj;
    let d = String.get_utf_8_uchar str start in
    let u = Uchar.utf_decode_uchar d in
    let _ = Uuseg_grapheme_cluster.check_boundary seg u in
    find_boundary_loop seg str limit (start + Uchar.utf_decode_length d))

(* Grapheme Iteration *)

let rec iter_graphemes_ascii str len f i =
  if i >= len then ()
  else if
    Char.code (Stdlib.String.unsafe_get str i) = 0x0D
    && i + 1 < len
    && Stdlib.String.unsafe_get str (i + 1) = '\n'
  then (
    f ~offset:i ~len:2;
    iter_graphemes_ascii str len f (i + 2))
  else (
    f ~offset:i ~len:1;
    iter_graphemes_ascii str len f (i + 1))

let rec iter_graphemes_unicode seg str len f i start =
  if i >= len then (if start < len then f ~offset:start ~len:(len - start))
  else
    let d = Stdlib.String.get_utf_8_uchar str i in
    let u = Uchar.utf_decode_uchar d in
    let next = i + Uchar.utf_decode_length d in
    if Uuseg_grapheme_cluster.check_boundary seg u then (
      f ~offset:start ~len:(i - start);
      iter_graphemes_unicode seg str len f next i)
    else iter_graphemes_unicode seg str len f next start

let iter_graphemes ?(ignore_zwj = false) f str =
  let len = Stdlib.String.length str in
  if len = 0 then ()
  else if is_ascii_only str len 0 then iter_graphemes_ascii str len f 0
  else
    let seg = Uuseg_grapheme_cluster.create ~ignore_zwj () in
    let d = Stdlib.String.get_utf_8_uchar str 0 in
    let _ =
      Uuseg_grapheme_cluster.check_boundary seg (Uchar.utf_decode_uchar d)
    in
    iter_graphemes_unicode seg str len f (Uchar.utf_decode_length d) 0

let iter_grapheme_info ~width_method ~tab_width f str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then ()
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let ignore_zwj = width_method = `No_zwj in

    let emit_ascii i =
      let b = Char.code (Stdlib.String.unsafe_get str i) in
      let w = ascii_width ~tab_width b in
      if w > 0 then f ~offset:i ~len:1 ~width:w
    in

    let rec loop i =
      if i >= len then ()
      else if i + 4 <= len && is_ascii_4 str i then (
        emit_ascii i;
        emit_ascii (i + 1);
        emit_ascii (i + 2);
        emit_ascii (i + 3);
        loop (i + 4))
      else
        let c = Stdlib.String.unsafe_get str i in
        if Char.code c < 128 then (
          emit_ascii i;
          loop (i + 1))
        else
          let end_pos = next_boundary seg ~ignore_zwj str i len in
          let clus_len = end_pos - i in
          let w =
            cluster_width ~method_:width_method ~tab_width str i clus_len
          in
          if w > 0 then (
            f ~offset:i ~len:clus_len ~width:w;
            loop end_pos)
          else loop end_pos
    in
    loop 0

(* String Measurement *)

let rec measure_ascii_tail str len tab_width i total =
  if i >= len then total
  else
    let w =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    measure_ascii_tail str len tab_width (i + 1) (total + w)

let rec measure_ascii str len tab_width i total =
  if i + 4 <= len && is_ascii_4 str i then
    let w0 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    let w1 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 1)))
    in
    let w2 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 2)))
    in
    let w3 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 3)))
    in
    measure_ascii str len tab_width (i + 4) (total + w0 + w1 + w2 + w3)
  else measure_ascii_tail str len tab_width i total

let rec measure_wcwidth str len tab_width i total =
  if i >= len then total
  else
    let d = Stdlib.String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let w = codepoint_width_wcwidth ~tab_width cp in
    measure_wcwidth str len tab_width (i + Uchar.utf_decode_length d) (total + w)

(* Fused segmentation + width loop for Unicode/No_zwj methods.

   State flags packed in [flags]: - bit 0: has_width (grapheme has a base
   width) - bit 1: ri_pair (last RI was first of a pair) - bit 2: virama (last
   codepoint was a virama) *)
let ms_has_width = 1
let ms_ri_pair = 2
let ms_virama = 4

let rec measure_segmented seg str len tab_width i total g_w flags =
  if i >= len then if flags land ms_has_width <> 0 then total + g_w else total
  else
    let d = Stdlib.String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    (* Single property lookup yields both boundary decision and width. *)
    let bw =
      Uuseg_grapheme_cluster.check_boundary_with_width seg
        (Uchar.unsafe_of_int cp)
    in
    let cp_w = if cp = 0x09 then tab_width else (bw land 3) - 1 in
    if bw land 4 <> 0 then
      let new_total =
        if flags land ms_has_width <> 0 then total + g_w else total
      in
      if cp = 0xFE0F then
        measure_segmented seg str len tab_width next new_total 0 0
      else if is_virama cp then
        measure_segmented seg str len tab_width next new_total 0 ms_virama
      else if is_regional_indicator cp then
        measure_segmented seg str len tab_width next new_total cp_w
          (ms_has_width lor ms_ri_pair)
      else if cp_w > 0 then
        measure_segmented seg str len tab_width next new_total cp_w ms_has_width
      else measure_segmented seg str len tab_width next new_total 0 0
    else if cp = 0xFE0F then
      let new_w = if flags land ms_has_width <> 0 && g_w = 1 then 2 else g_w in
      measure_segmented seg str len tab_width next total new_w flags
    else if is_virama cp then
      measure_segmented seg str len tab_width next total g_w
        (flags lor ms_virama)
    else if is_regional_indicator cp then
      if flags land ms_ri_pair <> 0 then
        measure_segmented seg str len tab_width next total (g_w + cp_w)
          (ms_has_width land lnot ms_virama)
      else
        let new_w = if flags land ms_has_width = 0 then cp_w else g_w in
        measure_segmented seg str len tab_width next total new_w
          (flags lor ms_has_width lor ms_ri_pair land lnot ms_virama)
    else if
      flags land ms_has_width <> 0
      && flags land ms_virama <> 0
      && is_devanagari_base cp
    then
      let add = if cp <> 0x0930 && cp_w > 0 then cp_w else 0 in
      measure_segmented seg str len tab_width next total (g_w + add)
        (flags lor ms_has_width land lnot ms_virama)
    else if flags land ms_has_width = 0 && cp_w > 0 then
      measure_segmented seg str len tab_width next total cp_w
        (flags lor ms_has_width land lnot ms_virama)
    else
      measure_segmented seg str len tab_width next total g_w
        (flags land lnot ms_virama)

let measure ~width_method ~tab_width str =
  let tab_width = normalize_tab_width tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then 0
  else if is_ascii_only str len 0 then measure_ascii str len tab_width 0 0
  else
    match width_method with
    | `Wcwidth -> measure_wcwidth str len tab_width 0 0
    | `Unicode | `No_zwj ->
        let seg = Uuseg_grapheme_cluster.create () in
        Uuseg_grapheme_cluster.set_ignore_zwj seg (width_method = `No_zwj);
        let d = Stdlib.String.get_utf_8_uchar str 0 in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        let bw =
          Uuseg_grapheme_cluster.check_boundary_with_width seg
            (Uchar.unsafe_of_int cp)
        in
        let w = if cp = 0x09 then tab_width else (bw land 3) - 1 in
        let init_w = if w > 0 then w else 0 in
        let init_flags =
          (if w > 0 then ms_has_width else 0)
          lor (if is_regional_indicator cp then ms_ri_pair else 0)
          lor if is_virama cp then ms_virama else 0
        in
        measure_segmented seg str len tab_width
          (Uchar.utf_decode_length d)
          0 init_w init_flags

let measure_sub ~width_method ~tab_width str ~pos ~len:sub_len =
  let tab_width = normalize_tab_width tab_width in
  if sub_len <= 0 then 0
  else (
    check_sub "Text.measure_sub" str ~pos ~len:sub_len;
    let end_pos = pos + sub_len in
    if is_ascii_only str end_pos pos then
      measure_ascii str end_pos tab_width pos 0
    else
      match width_method with
      | `Wcwidth -> measure_wcwidth str end_pos tab_width pos 0
      | `Unicode | `No_zwj ->
          let seg = Uuseg_grapheme_cluster.create () in
          Uuseg_grapheme_cluster.set_ignore_zwj seg (width_method = `No_zwj);
          let d = Stdlib.String.get_utf_8_uchar str pos in
          let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
          let bw =
            Uuseg_grapheme_cluster.check_boundary_with_width seg
              (Uchar.unsafe_of_int cp)
          in
          let w = if cp = 0x09 then tab_width else (bw land 3) - 1 in
          let init_w = if w > 0 then w else 0 in
          let init_flags =
            (if w > 0 then ms_has_width else 0)
            lor (if is_regional_indicator cp then ms_ri_pair else 0)
            lor if is_virama cp then ms_virama else 0
          in
          measure_segmented seg str end_pos tab_width
            (pos + Uchar.utf_decode_length d)
            0 init_w init_flags)

let grapheme_count str =
  let n = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr n) str;
  !n

(* Text Segmentation (wrap breaks, line breaks) *)

let[@inline] is_ascii_wrap_break b =
  match b with
  | 0x20 | 0x09 | 0x2D | 0x2F | 0x5C | 0x2E | 0x2C | 0x3B | 0x3A | 0x21 | 0x3F
  | 0x28 | 0x29 | 0x5B | 0x5D | 0x7B | 0x7D ->
      true
  | _ -> false

let[@inline] is_unicode_wrap_break cp =
  match cp with
  | 0x00A0 | 0x1680 | 0x202F | 0x205F | 0x3000 | 0x200B | 0x00AD | 0x2010
  | 0x3001 | 0x3002 | 0xFF01 | 0xFF1F ->
      true
  | cp when cp >= 0x2000 && cp <= 0x200A -> true
  | _ -> false

type word_class = Ascii_word | Cjk_word | Other

let[@inline] is_ascii_word_byte b =
  (b >= 0x61 && b <= 0x7A)
  || (b >= 0x41 && b <= 0x5A)
  || (b >= 0x30 && b <= 0x39)
  || b = 0x5F

let[@inline] is_cjk_word_codepoint cp =
  (cp >= 0x3400 && cp <= 0x4DBF)
  || (cp >= 0x4E00 && cp <= 0x9FFF)
  || (cp >= 0xF900 && cp <= 0xFAFF)
  || (cp >= 0x20000 && cp <= 0x2A6DF)
  || (cp >= 0x2A700 && cp <= 0x2B73F)
  || (cp >= 0x2B740 && cp <= 0x2B81F)
  || (cp >= 0x2B820 && cp <= 0x2CEAF)
  || (cp >= 0x2CEB0 && cp <= 0x2EBEF)
  || (cp >= 0x2EBF0 && cp <= 0x2EE5D)
  || (cp >= 0x2F800 && cp <= 0x2FA1F)
  || (cp >= 0x3040 && cp <= 0x309F)
  || (cp >= 0x30A0 && cp <= 0x30FF)
  || (cp >= 0x31F0 && cp <= 0x31FF)
  || (cp >= 0xFF66 && cp <= 0xFF9D)
  || (cp >= 0x1100 && cp <= 0x11FF)
  || (cp >= 0x3130 && cp <= 0x318F)
  || (cp >= 0xA960 && cp <= 0xA97F)
  || (cp >= 0xAC00 && cp <= 0xD7AF)
  || (cp >= 0xD7B0 && cp <= 0xD7FF)

let[@inline] classify_word cp =
  if cp <= 0x7F then if is_ascii_word_byte cp then Ascii_word else Other
  else if is_cjk_word_codepoint cp then Cjk_word
  else Other

let[@inline] is_cjk_ascii_transition prev curr =
  match (prev, curr) with
  | Cjk_word, Ascii_word | Ascii_word, Cjk_word -> true
  | _ -> false

let[@inline] first_codepoint s off =
  let d = Stdlib.String.get_utf_8_uchar s off in
  Uchar.to_int (Uchar.utf_decode_uchar d)

let iter_wrap_breaks_core ?(width_method = `Unicode) f s =
  let len = Stdlib.String.length s in
  let ignore_zwj = width_method = `No_zwj in
  let seg = Uuseg_grapheme_cluster.create () in
  let rec has_break i limit =
    if i >= limit then false
    else
      let b0 = Char.code (Stdlib.String.unsafe_get s i) in
      if b0 < 0x80 then is_ascii_wrap_break b0 || has_break (i + 1) limit
      else
        let d = Stdlib.String.get_utf_8_uchar s i in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        is_unicode_wrap_break cp
        || has_break (i + Uchar.utf_decode_length d) limit
  in
  let rec loop byte_off g_off prev_byte_off prev_g_off prev_class =
    if byte_off >= len then ()
    else
      let next = next_boundary seg ~ignore_zwj s byte_off len in
      let curr_class = classify_word (first_codepoint s byte_off) in
      (match (prev_byte_off, prev_g_off, prev_class) with
      | Some prev_byte_off, Some prev_g_off, Some prev_class
        when is_cjk_ascii_transition prev_class curr_class ->
          f ~byte_off:prev_byte_off ~next_off:byte_off ~grapheme_off:prev_g_off
      | _ -> ());
      if has_break byte_off next then
        f ~byte_off ~next_off:next ~grapheme_off:g_off;
      loop next (g_off + 1) (Some byte_off) (Some g_off) (Some curr_class)
  in
  loop 0 0 None None None

let iter_wrap_breaks ?(width_method = `Unicode) f s =
  iter_wrap_breaks_core ~width_method
    (fun ~byte_off ~next_off ~grapheme_off ->
      f ~break_byte_offset:byte_off ~next_byte_offset:next_off
        ~grapheme_offset:grapheme_off)
    s

let iter_line_breaks f s =
  let len = Stdlib.String.length s in
  let rec loop i =
    if i < len then
      let b = Char.code (Stdlib.String.unsafe_get s i) in
      if b = 0x0D then
        if i + 1 < len && Char.code (Stdlib.String.unsafe_get s (i + 1)) = 0x0A
        then (
          f ~pos:(i + 1) ~kind:`CRLF;
          loop (i + 2))
        else (
          f ~pos:i ~kind:`CR;
          loop (i + 1))
      else if b = 0x0A then (
        f ~pos:i ~kind:`LF;
        loop (i + 1))
      else loop (i + 1)
  in
  loop 0
