open StdLabels

type t = int
type width_method = Text.width_method

let[@inline] to_int (x : t) = x
let[@inline] unsafe_of_int (x : int) = x
let empty = 0
let flag_grapheme = if Sys.word_size = 64 then 1 lsl 62 else 0
let flag_continuation = if Sys.word_size = 64 then 1 lsl 61 else 0
let shift_right_extent = 59
let shift_left_extent = 57
let shift_generation = 18
let mask_generation = 0x7F
let mask_index = 0x3FFFF
let shift_width = 21
let mask_codepoint = 0x1FFFFF
let default_tab_width = 2

let () =
  if Sys.word_size <> 64 then
    failwith "Grid.Cell: 64-bit OCaml required for packed cells"

let[@inline] normalize_tab_width w = if w <= 0 then default_tab_width else w

let check_sub name str ~pos ~len =
  let str_len = String.length str in
  if pos < 0 then invalid_arg (name ^ ": negative position");
  if len < 0 then invalid_arg (name ^ ": negative length");
  if pos > str_len || len > str_len - pos then
    invalid_arg (name ^ ": substring out of bounds")

let[@inline] ascii_width ~tab_width b =
  if b = 0x09 then tab_width else if b >= 0x20 && b <= 0x7E then 1 else 0

let[@inline] is_ascii_4 str i =
  let c0 = Char.code (String.unsafe_get str i) in
  let c1 = Char.code (String.unsafe_get str (i + 1)) in
  let c2 = Char.code (String.unsafe_get str (i + 2)) in
  let c3 = Char.code (String.unsafe_get str (i + 3)) in
  c0 lor c1 lor c2 lor c3 < 128

let rec is_ascii_only_tail str len i =
  i >= len
  || Char.code (String.unsafe_get str i) < 128
     && is_ascii_only_tail str len (i + 1)

let rec is_ascii_only str len i =
  if i + 4 <= len then is_ascii_4 str i && is_ascii_only str len (i + 4)
  else is_ascii_only_tail str len i

let string_of_uchar uchar =
  let len = Uchar.utf_8_byte_length uchar in
  let buf = Bytes.create len in
  ignore (Bytes.set_utf_8_uchar buf 0 uchar);
  Bytes.unsafe_to_string buf

let normalize_malformed_utf8_slice str off len =
  let limit = off + len in
  let rec valid_loop i =
    if i >= limit then true
    else
      let d = String.get_utf_8_uchar str i in
      Uchar.utf_decode_is_valid d && valid_loop (i + Uchar.utf_decode_length d)
  in
  if valid_loop off then None
  else
    let b = Buffer.create len in
    let rec normalize_loop i =
      if i < limit then (
        let d = String.get_utf_8_uchar str i in
        Buffer.add_utf_8_uchar b
          (if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
           else Uchar.rep);
        normalize_loop (i + Uchar.utf_decode_length d))
    in
    normalize_loop off;
    Some (Buffer.contents b)

let[@inline] clamp_extent v = if v < 0 then 0 else if v > 3 then 3 else v

let[@inline] pack_start idx gen width =
  let width = if width < 1 then 1 else width in
  let right = if width > 4 then 3 else width - 1 in
  flag_grapheme
  lor (right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_continuation ~idx ~gen ~left ~right =
  flag_grapheme lor flag_continuation
  lor (clamp_extent left lsl shift_left_extent)
  lor (clamp_extent right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_simple cp width = (width lsl shift_width) lor cp
let[@inline] is_inline c = c land flag_grapheme = 0
let[@inline] is_complex c = c land flag_grapheme <> 0
let[@inline] is_start c = is_inline c || c land flag_continuation = 0

let[@inline] is_continuation c =
  (not (is_inline c)) && c land flag_continuation <> 0

let[@inline] is_empty c = c = 0
let[@inline] left_extent c = (c lsr shift_left_extent) land 3
let[@inline] right_extent c = (c lsr shift_right_extent) land 3
let[@inline] codepoint c = c land mask_codepoint
let[@inline] store_index c = c land mask_index
let[@inline] store_payload c = c land 0x01FFFFFF
let[@inline] unpack_idx c = c land mask_index
let[@inline] unpack_gen c = (c lsr shift_generation) land mask_generation
let space = pack_simple 0x20 1

let[@inline] grapheme_width ?(tab_width = default_tab_width) c =
  let tab_width = normalize_tab_width tab_width in
  if is_empty c then 0
  else if is_inline c then
    let cp = c land mask_codepoint in
    if cp = 0x09 then tab_width else (c lsr shift_width) land 3
  else
    let left = left_extent c in
    let right = right_extent c in
    if is_continuation c then left + 1 + right
    else if left <> 0 then 0
    else right + 1

let[@inline] cell_width c =
  if c = 0 then 0
  else if is_inline c then
    let width = (c lsr shift_width) land 3 in
    if width = 0 then 1 else width
  else if is_continuation c then 0
  else right_extent c + 1

let[@inline] store_key c =
  if is_inline c then None
  else
    let idx = store_index c in
    if idx = 0 then None else Some (store_payload c)

let make_continuation ~code ~left ~right =
  let payload = if is_inline code then 0 else store_payload code in
  flag_grapheme lor flag_continuation lor payload
  lor (clamp_extent left lsl shift_left_extent)
  lor (clamp_extent right lsl shift_right_extent)

let of_uchar uchar =
  let cp = Uchar.to_int uchar in
  let tab_width = default_tab_width in
  if cp < 128 then
    let width = ascii_width ~tab_width cp in
    if width <= 0 then empty
    else if cp = 0x09 then pack_simple cp 0
    else pack_simple cp width
  else
    let width =
      Text.measure ~width_method:`Unicode ~tab_width (string_of_uchar uchar)
    in
    if width <= 0 then empty else pack_simple cp width

let incref store cell =
  if not (is_inline cell) then
    Grapheme_store.incref store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let decref store cell =
  if not (is_inline cell) then
    Grapheme_store.decref store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let pack_interned store idx width =
  pack_start idx (Grapheme_store.generation store idx) width

let intern_core store width_method tab_width precomputed_width str pos len =
  if len = 0 then empty
  else
    let width =
      match precomputed_width with
      | Some width -> width
      | None -> Text.measure_sub ~width_method ~tab_width str ~pos ~len
    in
    if width <= 0 then empty
    else
      let d = String.get_utf_8_uchar str pos in
      if Uchar.utf_decode_length d = len then
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        if cp = 0x09 then pack_simple cp 0 else pack_simple cp width
      else
        let str, pos, len =
          match normalize_malformed_utf8_slice str pos len with
          | None -> (str, pos, len)
          | Some str -> (str, 0, String.length str)
        in
        let idx = Grapheme_store.intern store str ~off:pos ~len in
        pack_interned store idx width

let intern store ?(width_method = `Unicode) ?(tab_width = default_tab_width) str
    =
  intern_core store width_method
    (normalize_tab_width tab_width)
    None str 0 (String.length str)

let intern_sub store ~width_method ~tab_width str ~pos ~len ~width =
  check_sub "Grid.Packed_cell.intern_sub" str ~pos ~len;
  if width < 0 then invalid_arg "Grid.Packed_cell.intern_sub: negative width";
  intern_core store width_method
    (normalize_tab_width tab_width)
    (Some width) str pos len

let emit_span f cell width =
  f cell;
  if width > 1 then
    let span = min 4 width - 1 in
    for left = 1 to span do
      f (make_continuation ~code:cell ~left ~right:(span - left))
    done

let encode store ~width_method ~tab_width f str =
  let tab_width = normalize_tab_width tab_width in
  let len = String.length str in
  if is_ascii_only str len 0 then
    for i = 0 to len - 1 do
      let b = Char.code (String.unsafe_get str i) in
      if b = 0x09 then f (pack_simple b 0)
      else if b >= 0x20 && b <= 0x7E then f (pack_simple b 1)
    done
  else
    Text.iter_grapheme_info ~width_method ~tab_width
      (fun ~offset ~len ~width ->
        let cell =
          intern_sub store ~width_method ~tab_width str ~pos:offset ~len ~width
        in
        emit_span f cell width)
      str

let length store cell =
  if is_inline cell then
    Uchar.utf_8_byte_length (Uchar.unsafe_of_int (cell land mask_codepoint))
  else Grapheme_store.length store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let blit store cell buf ~pos =
  if pos < 0 || pos > Bytes.length buf then
    invalid_arg "Grid.Packed_cell.blit: position out of bounds";
  if is_inline cell then
    let uchar = Uchar.unsafe_of_int (cell land mask_codepoint) in
    let len = Uchar.utf_8_byte_length uchar in
    if len > Bytes.length buf - pos then 0
    else Bytes.set_utf_8_uchar buf pos uchar
  else
    Grapheme_store.blit store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell) buf
      ~pos

let copy ~src cell ~dst =
  if is_inline cell then cell
  else
    match
      Grapheme_store.copy ~src ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)
        ~dst
    with
    | None -> empty
    | Some idx ->
        let gen = Grapheme_store.generation dst idx in
        if is_continuation cell then
          pack_continuation ~idx ~gen ~left:(left_extent cell)
            ~right:(right_extent cell)
        else pack_start idx gen (grapheme_width cell)

let to_string store cell =
  if is_inline cell then
    string_of_uchar (Uchar.unsafe_of_int (cell land mask_codepoint))
  else
    Grapheme_store.to_string store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)
