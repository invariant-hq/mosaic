module Text = Matrix_text
open StdLabels

type t = int

let[@inline] to_int (x : t) = x
let[@inline] unsafe_of_int (x : int) = x
(* An inline cell holds a scalar (bits 0-20) and its width (bits 21-22). A
   complex start cell holds a grapheme store slot (bits 0-35) and its
   generation (bits 36-42), the distance to its span's last cell (bits 43-60)
   and its flag (bit 62). A continuation cell holds the distances to its
   span's first and last cells (bits 25-42 and 43-60) and its flags (bits
   61-62); its text is its start cell's, so it names no slot. A span is
   therefore at most [max_extent + 1] cells wide, which bounds the grid's
   width. *)

let empty = 0
let flag_grapheme = if Sys.word_size = 64 then 1 lsl 62 else 0
let flag_continuation = if Sys.word_size = 64 then 1 lsl 61 else 0
let shift_right_extent = 43
let shift_left_extent = 25
let max_extent = (1 lsl 18) - 1
let max_width = max_extent + 1
let shift_generation = Grapheme_store.index_bits
let mask_generation = (1 lsl Grapheme_store.generation_bits) - 1
let mask_index = (1 lsl Grapheme_store.index_bits) - 1

let mask_payload =
  (1 lsl (shift_generation + Grapheme_store.generation_bits)) - 1

let () =
  assert (
    shift_generation + Grapheme_store.generation_bits <= shift_right_extent)

let shift_width = 21
let mask_codepoint = 0x1FFFFF
let default_tab_width = 2

let () =
  if Sys.word_size <> 64 then
    failwith "Matrix_grid: 64-bit OCaml required for packed cells"

let[@inline] normalize_tab_width w = if w <= 0 then default_tab_width else w

let check_sub name str ~pos ~len =
  let str_len = String.length str in
  if pos < 0 then invalid_arg (name ^ ": negative position");
  if len < 0 then invalid_arg (name ^ ": negative length");
  if pos > str_len || len > str_len - pos then
    invalid_arg (name ^ ": substring out of bounds")

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

let[@inline] clamp_extent v =
  if v < 0 then 0 else if v > max_extent then max_extent else v

let[@inline] pack_start idx gen width =
  let right = clamp_extent (width - 1) in
  flag_grapheme
  lor (right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_simple cp width = (width lsl shift_width) lor cp
let[@inline] is_inline c = c land flag_grapheme = 0
let[@inline] is_complex c = c land flag_grapheme <> 0
let[@inline] is_start c = is_inline c || c land flag_continuation = 0

let[@inline] is_continuation c =
  (not (is_inline c)) && c land flag_continuation <> 0

let[@inline] is_empty c = c = 0

let[@inline] left_extent c =
  if c land flag_continuation = 0 then 0
  else (c lsr shift_left_extent) land max_extent

let[@inline] right_extent c = (c lsr shift_right_extent) land max_extent
let[@inline] codepoint c = c land mask_codepoint

(* The slot and generation of a complex start cell, and [0] for any other
   cell, whose low bits hold a scalar or extents. *)
let[@inline] names_slot c =
  c land (flag_grapheme lor flag_continuation) = flag_grapheme

let[@inline] store_index c = if names_slot c then c land mask_index else 0
let[@inline] store_payload c = if names_slot c then c land mask_payload else 0
let[@inline] unpack_idx c = store_index c

let[@inline] unpack_gen c =
  if names_slot c then (c lsr shift_generation) land mask_generation else 0

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

let make_continuation ~left ~right =
  flag_grapheme lor flag_continuation
  lor (clamp_extent left lsl shift_left_extent)
  lor (clamp_extent right lsl shift_right_extent)

let of_uchar uchar =
  let cp = Uchar.to_int uchar in
  if cp < 128 then if cp < 0x20 || cp = 0x7F then empty else pack_simple cp 1
  else
    let width = Text.uchar_width ~tab_width:default_tab_width uchar in
    if width <= 0 then empty else pack_simple cp width

let incref store cell =
  if is_start cell && not (is_inline cell) then
    Grapheme_store.incref store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let decref store cell =
  if is_start cell && not (is_inline cell) then
    Grapheme_store.decref store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let pack_interned store idx width =
  pack_start idx (Grapheme_store.generation store idx) width

(* [width] is the slice's, measured by the caller. *)
let intern_core store width str pos len =
  if len = 0 then empty
  else if width <= 0 then empty
  else
    let d = String.get_utf_8_uchar str pos in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    if Uchar.utf_decode_length d = len && cp = 0x09 then pack_simple cp 0
    else if Uchar.utf_decode_length d = len && width <= 3 then
      pack_simple cp width
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
  let len = String.length str in
  let width =
    Text.measure_sub ~width_method
      ~tab_width:(normalize_tab_width tab_width)
      str ~pos:0 ~len
  in
  intern_core store width str 0 len

let intern_sub store str ~pos ~len ~width =
  check_sub "Matrix_grid.Packed_cell.intern_sub" str ~pos ~len;
  if width < 0 then
    invalid_arg "Matrix_grid.Packed_cell.intern_sub: negative width";
  intern_core store width str pos len

let length store cell =
  if is_inline cell then
    Uchar.utf_8_byte_length (Uchar.unsafe_of_int (cell land mask_codepoint))
  else Grapheme_store.length store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let blit store cell buf ~pos =
  if pos < 0 || pos > Bytes.length buf then
    invalid_arg "Matrix_grid.Packed_cell.blit: position out of bounds";
  if is_inline cell then
    let uchar = Uchar.unsafe_of_int (cell land mask_codepoint) in
    let len = Uchar.utf_8_byte_length uchar in
    if len > Bytes.length buf - pos then 0
    else Bytes.set_utf_8_uchar buf pos uchar
  else
    Grapheme_store.blit store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell) buf
      ~pos

let copy ~src cell ~dst =
  if is_inline cell || is_continuation cell then cell
  else
    match
      Grapheme_store.copy ~src ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)
        ~dst
    with
    | None -> empty
    | Some idx ->
        pack_start idx (Grapheme_store.generation dst idx) (grapheme_width cell)

let to_string store cell =
  if is_inline cell then
    string_of_uchar (Uchar.unsafe_of_int (cell land mask_codepoint))
  else
    Grapheme_store.to_string store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)

let equal store1 c1 store2 c2 =
  if is_inline c1 || is_inline c2 then c1 = c2
  else
    c1 land lnot mask_payload = c2 land lnot mask_payload
    &&
    if unpack_idx c1 = 0 || unpack_idx c2 = 0 then unpack_idx c1 = unpack_idx c2
    else
      Grapheme_store.equal store1 ~idx1:(unpack_idx c1) ~gen1:(unpack_gen c1)
        store2 ~idx2:(unpack_idx c2) ~gen2:(unpack_gen c2)

(* A tile is a start cell one cell wide whose slot holds it. *)
let intern_tile store picture ~cols ~rows ~col ~row =
  let idx = Grapheme_store.intern_tile store picture ~cols ~rows ~col ~row in
  pack_interned store idx 1

let tile store cell =
  if names_slot cell then
    Grapheme_store.tile store ~idx:(unpack_idx cell) ~gen:(unpack_gen cell)
  else Grapheme_store.tile store ~idx:0 ~gen:0
