open StdLabels

(* Types *)

type t = int

let[@inline] to_int (x : t) : int = x
let[@inline] unsafe_of_int (x : int) : t = x
let empty = 0

type width_method = [ `Unicode | `Wcwidth | `No_zwj ]

type pool = {
  mutable storage : bytes;
  mutable offsets : int array;
  mutable lengths : int array;
  mutable capacities : int array;
  mutable refcounts : int array;
  mutable generations : int array;
  mutable free_stack : int array;
  mutable free_count : int;
  mutable next_id : int;
  mutable storage_cursor : int;
  mutable live_buckets : int array;
  mutable live_bucket_stamps : int array;
  mutable live_epoch : int;
  mutable live_count : int;
  mutable live_next : int array;
  mutable live_hashes : int array;
  segmenter : Uuseg_grapheme_cluster.t;
}

(* Constants & Bit Layout *)

(* 63-bit glyph layout (requires 64-bit OCaml):

   Simple (single Unicode scalar, no pool allocation): bits 62-61: 00, bits
   21-22: width (0 = tab sentinel, 1 = narrow, 2 = wide), bits 0-20: codepoint
   (21 bits, U+0000 - U+10FFFF)

   Complex Start (pool-backed grapheme cluster): bit 62: 1, bit 61: 0, bits
   59-60: right_extent (width - 1, clamped to 3), bits 18-24: generation (7
   bits), bits 0-17: pool index (18 bits, max 262K)

   Complex Continuation (wide-character placeholder): bit 62: 1, bit 61: 1, bits
   59-60: right_extent (distance to end), bits 57-58: left_extent (distance to
   start), bits 18-24: generation (7 bits), bits 0-17: pool index (18 bits) *)

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
let initial_pool_ids = 4096
let initial_pool_bytes = 4096 * 8
let initial_live_buckets = 1024

let () =
  if Sys.word_size <> 64 then
    failwith "Glyph: 64-bit OCaml required (63-bit integer packing)"

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

let[@inline] codepoint_width ~method_ ~tab_width cp =
  match method_ with
  | `Wcwidth -> codepoint_width_wcwidth ~tab_width cp
  | `Unicode | `No_zwj -> codepoint_width_unicode ~tab_width cp

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
        let u =
          if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
          else Uchar.rep
        in
        Buffer.add_utf_8_uchar b u;
        normalize_loop (i + Uchar.utf_decode_length d))
    in
    normalize_loop off;
    Some (Buffer.contents b)

let rec width_sum_loop ~method_ ~tab_width str limit i acc =
  if i >= limit then acc
  else
    let d = String.get_utf_8_uchar str i in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let w = codepoint_width ~method_ ~tab_width cp in
    let acc = if w > 0 then acc + w else acc in
    width_sum_loop ~method_ ~tab_width str limit
      (i + Uchar.utf_decode_length d)
      acc

let width_sum ~method_ ~tab_width str off len =
  width_sum_loop ~method_ ~tab_width str (off + len) off 0

(* Grapheme Cluster Width (for a slice of string) *)

(* Flag bits for width state *)
let width_flag_has_width = 1
let width_flag_ri_pair = 2
let width_flag_virama = 4
let width_utf8_valid = 0
let width_utf8_invalid = 1
let width_utf8_unknown = 2
let[@inline] pack_width_status width status = (width lsl 2) lor status
let[@inline] width_status_width v = v lsr 2
let[@inline] width_status v = v land 3

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

let rec grapheme_width_unicode_status_loop str limit tab_width i width flags
    valid =
  if i >= limit then
    pack_width_status width
      (if valid then width_utf8_valid else width_utf8_invalid)
  else
    let d = String.get_utf_8_uchar str i in
    let valid = valid && Uchar.utf_decode_is_valid d in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    let cp_width = codepoint_width_unicode ~tab_width cp in
    let has_width = flags land width_flag_has_width <> 0 in
    let is_ri_pair = flags land width_flag_ri_pair <> 0 in
    let has_virama = flags land width_flag_virama <> 0 in
    if cp = 0xFE0F then
      let new_width = if has_width && width = 1 then 2 else width in
      grapheme_width_unicode_status_loop str limit tab_width next new_width
        flags valid
    else if is_virama cp then
      grapheme_width_unicode_status_loop str limit tab_width next width
        (flags lor width_flag_virama)
        valid
    else if is_regional_indicator cp then
      if is_ri_pair then
        grapheme_width_unicode_status_loop str limit tab_width next
          (width + cp_width)
          (flags lor width_flag_has_width land lnot width_flag_ri_pair
         land lnot width_flag_virama)
          valid
      else
        let new_w = if not has_width then cp_width else width in
        grapheme_width_unicode_status_loop str limit tab_width next new_w
          (flags lor width_flag_has_width lor width_flag_ri_pair
         land lnot width_flag_virama)
          valid
    else if has_width && has_virama && is_devanagari_base cp then
      let add = if cp <> 0x0930 && cp_width > 0 then cp_width else 0 in
      grapheme_width_unicode_status_loop str limit tab_width next (width + add)
        (flags lor width_flag_has_width land lnot width_flag_virama)
        valid
    else if (not has_width) && cp_width > 0 then
      grapheme_width_unicode_status_loop str limit tab_width next cp_width
        (flags lor width_flag_has_width land lnot width_flag_virama)
        valid
    else
      grapheme_width_unicode_status_loop str limit tab_width next width
        (flags land lnot width_flag_virama)
        valid

let rec grapheme_width_wcwidth_status_loop str limit tab_width i acc valid =
  if i >= limit then
    pack_width_status acc
      (if valid then width_utf8_valid else width_utf8_invalid)
  else
    let d = String.get_utf_8_uchar str i in
    let valid = valid && Uchar.utf_decode_is_valid d in
    let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let next = i + Uchar.utf_decode_length d in
    grapheme_width_wcwidth_status_loop str limit tab_width next
      (acc + codepoint_width_wcwidth ~tab_width cp)
      valid

let cluster_width_status ~method_ ~tab_width str off len =
  let limit = off + len in
  match method_ with
  | `Wcwidth ->
      grapheme_width_wcwidth_status_loop str limit tab_width off 0 true
  | `Unicode | `No_zwj ->
      grapheme_width_unicode_status_loop str limit tab_width off 0 0 true

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

(* Glyph Packing & Accessors *)

let[@inline] clamp_extent v = if v < 0 then 0 else if v > 3 then 3 else v

let[@inline] pack_start idx gen width =
  let w = if width < 1 then 1 else width in
  let right = if w > 4 then 3 else w - 1 in
  flag_grapheme
  lor (right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_continuation ~idx ~gen ~left ~right =
  flag_grapheme lor flag_continuation
  lor (clamp_extent left lsl shift_left_extent)
  lor (clamp_extent right lsl shift_right_extent)
  lor (gen lsl shift_generation) lor (idx land mask_index)

let[@inline] pack_simple cp w = (w lsl shift_width) lor cp
let[@inline] is_inline c = c land flag_grapheme = 0
let[@inline] is_complex c = c land flag_grapheme <> 0
let[@inline] is_start c = is_inline c || c land flag_continuation = 0

let[@inline] is_continuation c =
  (not (is_inline c)) && c land flag_continuation <> 0

let[@inline] is_empty c = c = 0
let[@inline] right_extent c = (c lsr shift_right_extent) land 3
let[@inline] left_extent c = (c lsr shift_left_extent) land 3
let[@inline] codepoint c = c land mask_codepoint
let[@inline] pool_payload c = c land 0x01FFFFFF
let[@inline] pool_index c = c land mask_index
let[@inline] unpack_idx c = c land mask_index
let[@inline] unpack_gen c = (c lsr shift_generation) land mask_generation

let[@inline] validate_complex pool c =
  let idx = unpack_idx c in
  let gen = unpack_gen c in
  if
    idx > 0 && idx < pool.next_id
    && Array.unsafe_get pool.generations idx = gen
    && Array.unsafe_get pool.refcounts idx >= 0
  then idx
  else -1

let space = pack_simple 0x20 1

let[@inline] grapheme_width ?(tab_width = default_tab_width) c =
  let tab_width = normalize_tab_width tab_width in
  if is_empty c then 0
  else if is_inline c then
    let cp = c land mask_codepoint in
    if cp = 0x09 then tab_width else (c lsr shift_width) land 3
  else
    let l = left_extent c in
    let r = right_extent c in
    if is_continuation c then l + 1 + r else if l <> 0 then 0 else r + 1

let[@inline] pool_key c =
  if is_inline c then None
  else
    let idx = pool_index c in
    if idx = 0 then None else Some (pool_payload c)

let[@inline] cell_width c =
  if c = 0 then 0
  else if is_inline c then
    let w = (c lsr shift_width) land 3 in
    if w = 0 then 1 else w
  else if is_continuation c then 0
  else right_extent c + 1

let make_continuation ~code ~left ~right =
  let payload = if is_inline code then 0 else pool_payload code in
  let l_enc = clamp_extent left in
  let r_enc = clamp_extent right in
  flag_grapheme lor flag_continuation lor payload
  lor (l_enc lsl shift_left_extent)
  lor (r_enc lsl shift_right_extent)

let of_uchar uchar =
  let u = Uchar.to_int uchar in
  let tab_width = default_tab_width in
  if u < 128 then
    let w = ascii_width ~tab_width u in
    if w <= 0 then 0 else if u = 0x09 then pack_simple u 0 else pack_simple u w
  else
    let w = codepoint_width ~method_:`Unicode ~tab_width u in
    if w <= 0 then 0 else pack_simple u w

(* Pool *)

module Pool = struct
  type t = pool

  (* Pool Management *)

  let create () =
    {
      storage = Bytes.create initial_pool_bytes;
      offsets = Array.make initial_pool_ids 0;
      lengths = Array.make initial_pool_ids 0;
      capacities = Array.make initial_pool_ids 0;
      refcounts = Array.make initial_pool_ids 0;
      generations = Array.make initial_pool_ids 0;
      free_stack = Array.make initial_pool_ids 0;
      free_count = 0;
      next_id = 1;
      storage_cursor = 0;
      live_buckets = Array.make initial_live_buckets 0;
      live_bucket_stamps = Array.make initial_live_buckets 0;
      live_epoch = 1;
      live_count = 0;
      live_next = Array.make initial_pool_ids (-1);
      live_hashes = Array.make initial_pool_ids 0;
      segmenter = Uuseg_grapheme_cluster.create ();
    }

  let clear_live_table pool =
    pool.live_count <- 0;
    if pool.live_epoch = max_int then (
      Array.fill pool.live_bucket_stamps ~pos:0
        ~len:(Array.length pool.live_bucket_stamps)
        0;
      pool.live_epoch <- 1)
    else pool.live_epoch <- pool.live_epoch + 1

  let clear pool =
    let used = pool.next_id in
    pool.next_id <- 1;
    pool.storage_cursor <- 0;
    pool.free_count <- 0;
    (* Only reset slots [0..used-1]. Offsets and refcounts are overwritten by
       alloc_string so they don't need clearing. Lengths and capacities must be
       zeroed to prevent the storage-reuse path from reading stale offsets.
       Generations are bumped so old glyphs fail generation validation even if
       the same slot is reused immediately after clear. *)
    Array.fill pool.lengths ~pos:0 ~len:used 0;
    Array.fill pool.capacities ~pos:0 ~len:used 0;
    for i = 1 to used - 1 do
      Array.unsafe_set pool.generations i
        ((Array.unsafe_get pool.generations i + 1) land mask_generation)
    done;
    clear_live_table pool

  let ensure_id_capacity pool =
    let cap = Array.length pool.offsets in
    if pool.next_id >= cap then (
      let new_cap = cap * 2 in
      if new_cap > mask_index + 1 then failwith "Glyph pool ID exhaustion";
      let resize arr def =
        let new_arr = Array.make new_cap def in
        Array.blit ~src:arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:cap;
        new_arr
      in
      pool.offsets <- resize pool.offsets 0;
      pool.lengths <- resize pool.lengths 0;
      pool.capacities <- resize pool.capacities 0;
      pool.refcounts <- resize pool.refcounts 0;
      pool.generations <- resize pool.generations 0;
      pool.free_stack <- resize pool.free_stack 0;
      pool.live_next <- resize pool.live_next (-1);
      pool.live_hashes <- resize pool.live_hashes 0)

  let ensure_storage_capacity pool needed =
    let cap = Bytes.length pool.storage in
    if pool.storage_cursor + needed > cap then (
      let new_cap = max (cap * 2) (pool.storage_cursor + needed) in
      let new_bytes = Bytes.create new_cap in
      Bytes.blit ~src:pool.storage ~src_pos:0 ~dst:new_bytes ~dst_pos:0
        ~len:pool.storage_cursor;
      pool.storage <- new_bytes)

  let[@inline] next_free_id pool =
    if pool.free_count > 0 then (
      let i = pool.free_count - 1 in
      pool.free_count <- i;
      let id = Array.unsafe_get pool.free_stack i in
      let g = (Array.unsafe_get pool.generations id + 1) land mask_generation in
      Array.unsafe_set pool.generations id g;
      id)
    else
      let id = pool.next_id in
      pool.next_id <- id + 1;
      id

  let[@inline] push_free pool idx =
    Array.unsafe_set pool.free_stack pool.free_count idx;
    pool.free_count <- pool.free_count + 1

  let rec hash_slice_loop str limit i hash =
    if i >= limit then if hash = 0 then 1 else hash
    else
      let hash =
        ((hash lsl 5) - hash + Char.code (String.unsafe_get str i)) land max_int
      in
      hash_slice_loop str limit (i + 1) hash

  let[@inline] hash_slice str off len =
    hash_slice_loop str (off + len) off 0x345678

  let rec hash_storage_loop storage limit i hash =
    if i >= limit then if hash = 0 then 1 else hash
    else
      let hash =
        ((hash lsl 5) - hash + Char.code (Bytes.unsafe_get storage i))
        land max_int
      in
      hash_storage_loop storage limit (i + 1) hash

  let[@inline] hash_storage storage off len =
    hash_storage_loop storage (off + len) off 0x345678

  let rec storage_equals_slice_loop storage storage_off str off len i =
    i >= len
    || Char.equal
         (Bytes.unsafe_get storage (storage_off + i))
         (String.unsafe_get str (off + i))
       && storage_equals_slice_loop storage storage_off str off len (i + 1)

  let[@inline] storage_equals_slice storage storage_off str off len =
    storage_equals_slice_loop storage storage_off str off len 0

  let[@inline] live_bucket pool hash =
    hash land (Array.length pool.live_buckets - 1)

  let[@inline] live_bucket_head pool bucket =
    if Array.unsafe_get pool.live_bucket_stamps bucket = pool.live_epoch then
      Array.unsafe_get pool.live_buckets bucket
    else 0

  let[@inline] set_live_bucket_head pool bucket head =
    Array.unsafe_set pool.live_bucket_stamps bucket pool.live_epoch;
    Array.unsafe_set pool.live_buckets bucket head

  let rec live_lookup_loop pool str off len hash idx =
    if idx = 0 then 0
    else if
      Array.unsafe_get pool.live_hashes idx = hash
      && Array.unsafe_get pool.refcounts idx > 0
      && Array.unsafe_get pool.lengths idx = len
      && storage_equals_slice pool.storage
           (Array.unsafe_get pool.offsets idx)
           str off len
    then idx
    else
      live_lookup_loop pool str off len hash
        (Array.unsafe_get pool.live_next idx)

  let[@inline] live_lookup pool str off len hash =
    live_lookup_loop pool str off len hash
      (live_bucket_head pool (live_bucket pool hash))

  let rehash_live_table pool new_bucket_count =
    pool.live_buckets <- Array.make new_bucket_count 0;
    pool.live_bucket_stamps <- Array.make new_bucket_count 0;
    pool.live_epoch <- 1;
    pool.live_count <- 0;
    for idx = 1 to pool.next_id - 1 do
      if
        Array.unsafe_get pool.refcounts idx > 0
        && Array.unsafe_get pool.live_next idx >= 0
      then (
        let hash = Array.unsafe_get pool.live_hashes idx in
        let bucket = live_bucket pool hash in
        let head = live_bucket_head pool bucket in
        Array.unsafe_set pool.live_next idx head;
        set_live_bucket_head pool bucket idx;
        pool.live_count <- pool.live_count + 1)
    done

  let ensure_live_capacity pool =
    let bucket_count = Array.length pool.live_buckets in
    if pool.live_count * 2 >= bucket_count then
      rehash_live_table pool (bucket_count * 2)

  let live_add pool idx hash =
    ensure_live_capacity pool;
    let bucket = live_bucket pool hash in
    let head = live_bucket_head pool bucket in
    Array.unsafe_set pool.live_hashes idx hash;
    Array.unsafe_set pool.live_next idx head;
    set_live_bucket_head pool bucket idx;
    pool.live_count <- pool.live_count + 1

  let rec live_remove_loop pool bucket idx prev cur =
    if cur = 0 then ()
    else
      let next = Array.unsafe_get pool.live_next cur in
      if cur = idx then (
        if prev = 0 then set_live_bucket_head pool bucket next
        else Array.unsafe_set pool.live_next prev next;
        Array.unsafe_set pool.live_next idx (-1);
        Array.unsafe_set pool.live_hashes idx 0;
        pool.live_count <- pool.live_count - 1)
      else live_remove_loop pool bucket idx cur next

  let live_remove pool idx =
    let hash = Array.unsafe_get pool.live_hashes idx in
    if hash <> 0 then
      let bucket = live_bucket pool hash in
      if Array.unsafe_get pool.live_bucket_stamps bucket <> pool.live_epoch then (
        Array.unsafe_set pool.live_next idx (-1);
        Array.unsafe_set pool.live_hashes idx 0)
      else
        live_remove_loop pool bucket idx 0
          (Array.unsafe_get pool.live_buckets bucket)

  let live_hash pool idx =
    let hash = Array.unsafe_get pool.live_hashes idx in
    if hash <> 0 then hash
    else
      let hash =
        hash_storage pool.storage
          (Array.unsafe_get pool.offsets idx)
          (Array.unsafe_get pool.lengths idx)
      in
      Array.unsafe_set pool.live_hashes idx hash;
      hash

  let intern_live_payload pool idx = live_add pool idx (live_hash pool idx)
  let remove_live_payload pool idx = live_remove pool idx

  let alloc_string pool str off len =
    ensure_id_capacity pool;
    let id = next_free_id pool in
    let cap = Array.unsafe_get pool.capacities id in
    let cursor =
      if cap >= len then Array.unsafe_get pool.offsets id
      else (
        ensure_storage_capacity pool len;
        let cur = pool.storage_cursor in
        pool.storage_cursor <- cur + len;
        Array.unsafe_set pool.capacities id len;
        cur)
    in
    Bytes.blit_string ~src:str ~src_pos:off ~dst:pool.storage ~dst_pos:cursor
      ~len;
    Array.unsafe_set pool.offsets id cursor;
    Array.unsafe_set pool.lengths id len;
    Array.unsafe_set pool.refcounts id 0;
    Array.unsafe_set pool.live_next id (-1);
    Array.unsafe_set pool.live_hashes id 0;
    id

  (* Reference Counting *)

  let incref pool c =
    if is_inline c then ()
    else
      let idx = validate_complex pool c in
      if idx >= 0 then (
        let rc = Array.unsafe_get pool.refcounts idx in
        Array.unsafe_set pool.refcounts idx (rc + 1);
        if rc = 0 then intern_live_payload pool idx)

  let decref pool c =
    if is_inline c then ()
    else
      let idx = validate_complex pool c in
      if idx < 0 then ()
      else
        let rc = Array.unsafe_get pool.refcounts idx in
        if rc < 0 then ()
        else
          let rc' = rc - 1 in
          if rc' > 0 then Array.unsafe_set pool.refcounts idx rc'
          else (
            remove_live_payload pool idx;
            Array.unsafe_set pool.refcounts idx (-1);
            push_free pool idx)

  (* Interning *)

  (* Check ASCII and compute width in one pass. Returns -1 if non-ASCII
     found. *)
  let rec ascii_width_loop_tail str limit tab_width i acc =
    if i >= limit then acc
    else
      let b = Char.code (String.unsafe_get str i) in
      if b >= 128 then -1
      else
        ascii_width_loop_tail str limit tab_width (i + 1)
          (acc + ascii_width ~tab_width b)

  let rec ascii_width_loop str limit tab_width i acc =
    if i + 4 <= limit then
      if not (is_ascii_4 str i) then -1
      else
        let w0 = ascii_width ~tab_width (Char.code (String.unsafe_get str i)) in
        let w1 =
          ascii_width ~tab_width (Char.code (String.unsafe_get str (i + 1)))
        in
        let w2 =
          ascii_width ~tab_width (Char.code (String.unsafe_get str (i + 2)))
        in
        let w3 =
          ascii_width ~tab_width (Char.code (String.unsafe_get str (i + 3)))
        in
        ascii_width_loop str limit tab_width (i + 4) (acc + w0 + w1 + w2 + w3)
    else ascii_width_loop_tail str limit tab_width i acc

  let intern_core pool method_ tab_width precomputed_width off len str =
    if len = 0 then 0
    else if len = 1 then
      let b = Char.code (String.unsafe_get str off) in
      if b < 0x80 then
        let w =
          match precomputed_width with
          | Some w -> w
          | None -> ascii_width ~tab_width b
        in
        if w <= 0 then 0
        else if b = 0x09 then pack_simple b 0
        else pack_simple b w
      else
        (* Single invalid UTF-8 byte is interpreted as U+FFFD. *)
        let d = String.get_utf_8_uchar str off in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        let w =
          match precomputed_width with
          | Some w -> w
          | None -> codepoint_width ~method_ ~tab_width cp
        in
        if w <= 0 then 0 else pack_simple cp w
    else
      (* Multi-byte: check if single codepoint *)
      let d = String.get_utf_8_uchar str off in
      let cp_len = Uchar.utf_decode_length d in
      if cp_len = len then
        (* Single Unicode scalar: store directly *)
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        let w =
          match precomputed_width with
          | Some w -> w
          | None -> codepoint_width ~method_ ~tab_width cp
        in
        if w <= 0 then 0 else pack_simple cp w
      else
        (* Multi-codepoint cluster: pool allocation *)
        let w_status =
          match precomputed_width with
          | Some w -> pack_width_status w width_utf8_unknown
          | None ->
              let first_b = Char.code (String.unsafe_get str off) in
              if first_b >= 128 then
                cluster_width_status ~method_ ~tab_width str off len
              else
                let ascii_w =
                  ascii_width_loop str (off + len) tab_width off 0
                in
                if ascii_w >= 0 then pack_width_status ascii_w width_utf8_valid
                else cluster_width_status ~method_ ~tab_width str off len
        in
        let w = width_status_width w_status in
        if w <= 0 then 0
        else
          let str, off, len, w =
            if width_status w_status = width_utf8_valid then (str, off, len, w)
            else
              match normalize_malformed_utf8_slice str off len with
              | None -> (str, off, len, w)
              | Some s ->
                  let len = String.length s in
                  let w =
                    match precomputed_width with
                    | Some w -> w
                    | None -> width_sum ~method_ ~tab_width s 0 len
                  in
                  (s, 0, len, w)
          in
          let hash = hash_slice str off len in
          let idx = live_lookup pool str off len hash in
          if idx <> 0 then
            pack_start idx (Array.unsafe_get pool.generations idx) w
          else
            let idx = alloc_string pool str off len in
            Array.unsafe_set pool.live_hashes idx hash;
            pack_start idx (Array.unsafe_get pool.generations idx) w

  let intern pool ?(width_method = `Unicode) ?(tab_width = default_tab_width)
      str =
    let tab_width = normalize_tab_width tab_width in
    intern_core pool width_method tab_width None 0 (String.length str) str

  let intern_sub pool ~width_method ~tab_width str ~pos ~len ~width =
    check_sub "Glyph.Pool.intern_sub" str ~pos ~len;
    if width < 0 then invalid_arg "Glyph.Pool.intern_sub: negative width";
    let tab_width = normalize_tab_width tab_width in
    intern_core pool width_method tab_width (Some width) pos len str

  (* Encoding (string -> glyph stream) *)

  let encode pool ~width_method ~tab_width f str =
    let tab_width = normalize_tab_width tab_width in
    let len = String.length str in
    if is_ascii_only str len 0 then
      for i = 0 to len - 1 do
        let b = Char.code (String.unsafe_get str i) in
        if b = 0x09 then f (pack_simple 0x09 0)
        else if b >= 0x20 && b <= 0x7E then f (pack_simple b 1)
      done
    else
      let ignore_zwj = width_method = `No_zwj in
      let seg = pool.segmenter in
      Uuseg_grapheme_cluster.reset seg;
      Uuseg_grapheme_cluster.set_ignore_zwj seg ignore_zwj;

      let emit_complex ~off ~clus_len ~width =
        let idx = alloc_string pool str off clus_len in
        let gen = Array.unsafe_get pool.generations idx in
        f (pack_start idx gen width);
        if width > 1 then
          let max_span = min 4 width - 1 in
          for k = 1 to max_span do
            f (pack_continuation ~idx ~gen ~left:k ~right:(max_span - k))
          done
      in

      let emit_ascii b =
        if b = 0x09 then f (pack_simple 0x09 0)
        else if b >= 0x20 && b <= 0x7E then f (pack_simple b 1)
      in

      let rec loop i =
        if i >= len then ()
        else if i + 4 <= len && is_ascii_4 str i then (
          emit_ascii (Char.code (String.unsafe_get str i));
          emit_ascii (Char.code (String.unsafe_get str (i + 1)));
          emit_ascii (Char.code (String.unsafe_get str (i + 2)));
          emit_ascii (Char.code (String.unsafe_get str (i + 3)));
          loop (i + 4))
        else
          let c = String.unsafe_get str i in
          if Char.code c < 128 then (
            emit_ascii (Char.code c);
            loop (i + 1))
          else
            let end_pos = next_boundary seg ~ignore_zwj str i len in
            let clus_len = end_pos - i in
            let w =
              cluster_width ~method_:width_method ~tab_width str i clus_len
            in
            (if w > 0 then
               let d = String.get_utf_8_uchar str i in
               if Uchar.utf_decode_length d = clus_len then (
                 (* Single codepoint: store as simple glyph *)
                 let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
                 f (pack_simple cp w);
                 if w > 1 then
                   let max_span = min 4 w - 1 in
                   for k = 1 to max_span do
                     f
                       (pack_continuation ~idx:0 ~gen:0 ~left:k
                          ~right:(max_span - k))
                   done)
               else emit_complex ~off:i ~clus_len ~width:w);
            loop end_pos
      in
      loop 0

  (* Data Retrieval *)

  let length pool c =
    if is_inline c then
      Uchar.utf_8_byte_length (Uchar.unsafe_of_int (c land mask_codepoint))
    else
      let idx = validate_complex pool c in
      if idx < 0 then 0 else Array.unsafe_get pool.lengths idx

  let blit pool c buf ~pos =
    if pos < 0 || pos > Bytes.length buf then
      invalid_arg "Glyph.Pool.blit: position out of bounds";
    if is_inline c then
      let u = Uchar.unsafe_of_int (c land mask_codepoint) in
      let len = Uchar.utf_8_byte_length u in
      if len > Bytes.length buf - pos then 0
      else Bytes.set_utf_8_uchar buf pos u
    else
      let idx = validate_complex pool c in
      if idx < 0 then 0
      else
        let len = Array.unsafe_get pool.lengths idx in
        if len > Bytes.length buf - pos then 0
        else
          let src_off = Array.unsafe_get pool.offsets idx in
          Bytes.blit ~src:pool.storage ~src_pos:src_off ~dst:buf ~dst_pos:pos
            ~len;
          len

  let copy ~src c ~dst =
    if is_inline c then c
    else
      let idx = validate_complex src c in
      if idx < 0 then 0
      else
        let len = Array.unsafe_get src.lengths idx in
        let src_off = Array.unsafe_get src.offsets idx in
        if src_off + len > Bytes.length src.storage then 0
        else (
          ensure_id_capacity dst;
          let dst_id = next_free_id dst in
          let cap = Array.unsafe_get dst.capacities dst_id in
          let cursor =
            if cap >= len then Array.unsafe_get dst.offsets dst_id
            else (
              ensure_storage_capacity dst len;
              let cur = dst.storage_cursor in
              dst.storage_cursor <- cur + len;
              Array.unsafe_set dst.capacities dst_id len;
              cur)
          in
          Bytes.blit ~src:src.storage ~src_pos:src_off ~dst:dst.storage
            ~dst_pos:cursor ~len;
          Array.unsafe_set dst.offsets dst_id cursor;
          Array.unsafe_set dst.lengths dst_id len;
          Array.unsafe_set dst.refcounts dst_id 0;
          let dst_gen = Array.unsafe_get dst.generations dst_id in
          if is_continuation c then
            pack_continuation ~idx:dst_id ~gen:dst_gen ~left:(left_extent c)
              ~right:(right_extent c)
          else pack_start dst_id dst_gen (grapheme_width c))

  let to_string pool c =
    if is_inline c then (
      let u = Uchar.unsafe_of_int (c land mask_codepoint) in
      let len = Uchar.utf_8_byte_length u in
      let buf = Bytes.create len in
      ignore (Bytes.set_utf_8_uchar buf 0 u);
      Bytes.unsafe_to_string buf)
    else
      let idx = validate_complex pool c in
      if idx < 0 then ""
      else
        let len = Array.unsafe_get pool.lengths idx in
        let off = Array.unsafe_get pool.offsets idx in
        Bytes.sub_string pool.storage ~pos:off ~len
end

(* Text utilities *)

type line_break_kind = [ `LF | `CR | `CRLF ]

module String = struct
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
        ascii_width ~tab_width
          (Char.code (Stdlib.String.unsafe_get str (i + 1)))
      in
      let w2 =
        ascii_width ~tab_width
          (Char.code (Stdlib.String.unsafe_get str (i + 2)))
      in
      let w3 =
        ascii_width ~tab_width
          (Char.code (Stdlib.String.unsafe_get str (i + 3)))
      in
      measure_ascii str len tab_width (i + 4) (total + w0 + w1 + w2 + w3)
    else measure_ascii_tail str len tab_width i total

  let rec measure_wcwidth str len tab_width i total =
    if i >= len then total
    else
      let d = Stdlib.String.get_utf_8_uchar str i in
      let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let w = codepoint_width_wcwidth ~tab_width cp in
      measure_wcwidth str len tab_width
        (i + Uchar.utf_decode_length d)
        (total + w)

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
          measure_segmented seg str len tab_width next new_total cp_w
            ms_has_width
        else measure_segmented seg str len tab_width next new_total 0 0
      else if cp = 0xFE0F then
        let new_w =
          if flags land ms_has_width <> 0 && g_w = 1 then 2 else g_w
        in
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
      check_sub "Glyph.String.measure_sub" str ~pos ~len:sub_len;
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
            f ~byte_off:prev_byte_off ~next_off:byte_off
              ~grapheme_off:prev_g_off
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
          if
            i + 1 < len && Char.code (Stdlib.String.unsafe_get s (i + 1)) = 0x0A
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
end
