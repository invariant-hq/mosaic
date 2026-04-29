open StdLabels

type t = {
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
}

let mask_generation = 0x7F
let mask_index = 0x3FFFF
let initial_ids = 4096
let initial_bytes = 4096 * 8
let initial_live_buckets = 1024

let create () =
  {
    storage = Bytes.create initial_bytes;
    offsets = Array.make initial_ids 0;
    lengths = Array.make initial_ids 0;
    capacities = Array.make initial_ids 0;
    refcounts = Array.make initial_ids 0;
    generations = Array.make initial_ids 0;
    free_stack = Array.make initial_ids 0;
    free_count = 0;
    next_id = 1;
    storage_cursor = 0;
    live_buckets = Array.make initial_live_buckets 0;
    live_bucket_stamps = Array.make initial_live_buckets 0;
    live_epoch = 1;
    live_count = 0;
    live_next = Array.make initial_ids (-1);
    live_hashes = Array.make initial_ids 0;
  }

let clear_live_table t =
  t.live_count <- 0;
  if t.live_epoch = max_int then (
    Array.fill t.live_bucket_stamps ~pos:0
      ~len:(Array.length t.live_bucket_stamps)
      0;
    t.live_epoch <- 1)
  else t.live_epoch <- t.live_epoch + 1

let clear t =
  let used = t.next_id in
  t.next_id <- 1;
  t.storage_cursor <- 0;
  t.free_count <- 0;
  Array.fill t.lengths ~pos:0 ~len:used 0;
  Array.fill t.capacities ~pos:0 ~len:used 0;
  for i = 1 to used - 1 do
    Array.unsafe_set t.generations i
      ((Array.unsafe_get t.generations i + 1) land mask_generation)
  done;
  clear_live_table t

let ensure_id_capacity t =
  let cap = Array.length t.offsets in
  if t.next_id >= cap then (
    let new_cap = cap * 2 in
    if new_cap > mask_index + 1 then
      failwith "Grid.Grapheme_store: ID exhaustion";
    let resize arr def =
      let next = Array.make new_cap def in
      Array.blit ~src:arr ~src_pos:0 ~dst:next ~dst_pos:0 ~len:cap;
      next
    in
    t.offsets <- resize t.offsets 0;
    t.lengths <- resize t.lengths 0;
    t.capacities <- resize t.capacities 0;
    t.refcounts <- resize t.refcounts 0;
    t.generations <- resize t.generations 0;
    t.free_stack <- resize t.free_stack 0;
    t.live_next <- resize t.live_next (-1);
    t.live_hashes <- resize t.live_hashes 0)

let ensure_storage_capacity t needed =
  let cap = Bytes.length t.storage in
  if t.storage_cursor + needed > cap then (
    let new_cap = max (cap * 2) (t.storage_cursor + needed) in
    let next = Bytes.create new_cap in
    Bytes.blit ~src:t.storage ~src_pos:0 ~dst:next ~dst_pos:0
      ~len:t.storage_cursor;
    t.storage <- next)

let[@inline] next_free_id t =
  if t.free_count > 0 then (
    let i = t.free_count - 1 in
    t.free_count <- i;
    let id = Array.unsafe_get t.free_stack i in
    let gen = (Array.unsafe_get t.generations id + 1) land mask_generation in
    Array.unsafe_set t.generations id gen;
    id)
  else
    let id = t.next_id in
    t.next_id <- id + 1;
    id

let[@inline] push_free t idx =
  Array.unsafe_set t.free_stack t.free_count idx;
  t.free_count <- t.free_count + 1

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

let[@inline] live_bucket t hash = hash land (Array.length t.live_buckets - 1)

let[@inline] live_bucket_head t bucket =
  if Array.unsafe_get t.live_bucket_stamps bucket = t.live_epoch then
    Array.unsafe_get t.live_buckets bucket
  else 0

let[@inline] set_live_bucket_head t bucket head =
  Array.unsafe_set t.live_bucket_stamps bucket t.live_epoch;
  Array.unsafe_set t.live_buckets bucket head

let rec live_lookup_loop t str off len hash idx =
  if idx = 0 then 0
  else if
    Array.unsafe_get t.live_hashes idx = hash
    && Array.unsafe_get t.refcounts idx > 0
    && Array.unsafe_get t.lengths idx = len
    && storage_equals_slice t.storage
         (Array.unsafe_get t.offsets idx)
         str off len
  then idx
  else live_lookup_loop t str off len hash (Array.unsafe_get t.live_next idx)

let live_lookup t str off len hash =
  live_lookup_loop t str off len hash (live_bucket_head t (live_bucket t hash))

let rehash_live_table t new_bucket_count =
  t.live_buckets <- Array.make new_bucket_count 0;
  t.live_bucket_stamps <- Array.make new_bucket_count 0;
  t.live_epoch <- 1;
  t.live_count <- 0;
  for idx = 1 to t.next_id - 1 do
    if
      Array.unsafe_get t.refcounts idx > 0
      && Array.unsafe_get t.live_next idx >= 0
    then (
      let hash = Array.unsafe_get t.live_hashes idx in
      let bucket = live_bucket t hash in
      let head = live_bucket_head t bucket in
      Array.unsafe_set t.live_next idx head;
      set_live_bucket_head t bucket idx;
      t.live_count <- t.live_count + 1)
  done

let ensure_live_capacity t =
  let bucket_count = Array.length t.live_buckets in
  if t.live_count * 2 >= bucket_count then rehash_live_table t (bucket_count * 2)

let live_add t idx hash =
  ensure_live_capacity t;
  let bucket = live_bucket t hash in
  let head = live_bucket_head t bucket in
  Array.unsafe_set t.live_hashes idx hash;
  Array.unsafe_set t.live_next idx head;
  set_live_bucket_head t bucket idx;
  t.live_count <- t.live_count + 1

let rec live_remove_loop t bucket idx prev cur =
  if cur = 0 then ()
  else
    let next = Array.unsafe_get t.live_next cur in
    if cur = idx then (
      if prev = 0 then set_live_bucket_head t bucket next
      else Array.unsafe_set t.live_next prev next;
      Array.unsafe_set t.live_next idx (-1);
      Array.unsafe_set t.live_hashes idx 0;
      t.live_count <- t.live_count - 1)
    else live_remove_loop t bucket idx cur next

let live_remove t idx =
  let hash = Array.unsafe_get t.live_hashes idx in
  if hash <> 0 then
    let bucket = live_bucket t hash in
    if Array.unsafe_get t.live_bucket_stamps bucket <> t.live_epoch then (
      Array.unsafe_set t.live_next idx (-1);
      Array.unsafe_set t.live_hashes idx 0)
    else
      live_remove_loop t bucket idx 0 (Array.unsafe_get t.live_buckets bucket)

let live_hash t idx =
  let hash = Array.unsafe_get t.live_hashes idx in
  if hash <> 0 then hash
  else
    let hash =
      hash_storage t.storage
        (Array.unsafe_get t.offsets idx)
        (Array.unsafe_get t.lengths idx)
    in
    Array.unsafe_set t.live_hashes idx hash;
    hash

let alloc t str off len =
  ensure_id_capacity t;
  let id = next_free_id t in
  let cap = Array.unsafe_get t.capacities id in
  let cursor =
    if cap >= len then Array.unsafe_get t.offsets id
    else (
      ensure_storage_capacity t len;
      let cursor = t.storage_cursor in
      t.storage_cursor <- cursor + len;
      Array.unsafe_set t.capacities id len;
      cursor)
  in
  Bytes.blit_string ~src:str ~src_pos:off ~dst:t.storage ~dst_pos:cursor ~len;
  Array.unsafe_set t.offsets id cursor;
  Array.unsafe_set t.lengths id len;
  Array.unsafe_set t.refcounts id 0;
  Array.unsafe_set t.live_next id (-1);
  Array.unsafe_set t.live_hashes id 0;
  id

let intern t str ~off ~len =
  let hash = hash_slice str off len in
  let idx = live_lookup t str off len hash in
  if idx <> 0 then idx
  else
    let idx = alloc t str off len in
    Array.unsafe_set t.live_hashes idx hash;
    idx

let valid t ~idx ~gen =
  idx > 0 && idx < t.next_id
  && Array.unsafe_get t.generations idx = gen
  && Array.unsafe_get t.refcounts idx >= 0

let generation t idx = Array.unsafe_get t.generations idx

let incref t ~idx ~gen =
  if valid t ~idx ~gen then (
    let rc = Array.unsafe_get t.refcounts idx in
    Array.unsafe_set t.refcounts idx (rc + 1);
    if rc = 0 then live_add t idx (live_hash t idx))

let decref t ~idx ~gen =
  if valid t ~idx ~gen then
    let rc = Array.unsafe_get t.refcounts idx in
    if rc >= 0 then
      let next = rc - 1 in
      if next > 0 then Array.unsafe_set t.refcounts idx next
      else (
        live_remove t idx;
        Array.unsafe_set t.refcounts idx (-1);
        push_free t idx)

let length t ~idx ~gen =
  if valid t ~idx ~gen then Array.unsafe_get t.lengths idx else 0

let blit t ~idx ~gen buf ~pos =
  if pos < 0 || pos > Bytes.length buf then
    invalid_arg "Grid.Packed_cell.blit: position out of bounds";
  if not (valid t ~idx ~gen) then 0
  else
    let len = Array.unsafe_get t.lengths idx in
    if len > Bytes.length buf - pos then 0
    else (
      Bytes.blit ~src:t.storage
        ~src_pos:(Array.unsafe_get t.offsets idx)
        ~dst:buf ~dst_pos:pos ~len;
      len)

let to_string t ~idx ~gen =
  if not (valid t ~idx ~gen) then ""
  else
    Bytes.sub_string t.storage
      ~pos:(Array.unsafe_get t.offsets idx)
      ~len:(Array.unsafe_get t.lengths idx)

let copy ~src ~idx ~gen ~dst =
  if not (valid src ~idx ~gen) then None
  else
    let len = Array.unsafe_get src.lengths idx in
    let off = Array.unsafe_get src.offsets idx in
    if off + len > Bytes.length src.storage then None
    else
      let dst_idx = alloc dst (Bytes.unsafe_to_string src.storage) off len in
      Some dst_idx
