(* Unicode property lookups for grapheme segmentation and width calculation.

   All properties are pre-computed at build time into a two-level page table for
   O(1) lookup with zero initialization cost. The codepoint space is split into
   256-entry blocks; identical blocks share data via deduplication.

   Layout per 16-bit entry: - bits 0-4: gcb (grapheme_cluster_break, values
   0-17) - bits 5-6: incb (indic_conjunct_break, values 0-3) - bit 7: extpic
   (extended_pictographic, boolean) - bits 8-9: width (encoded: 0=-1, 1=0, 2=1,
   3=2) *)

(* Surrogate packing: skip 0xD800-0xDFFF range *)
let[@inline] pack_u u = if u > 0xd7ff then u - 0x800 else u

(* Two-level page table lookup: index[block] → block_id, then data[block_id *
   512 + offset * 2] *)
let[@inline] get u =
  let packed = pack_u (Uchar.to_int u) in
  let block_id =
    Char.code (String.unsafe_get Unicode_data.prop_index (packed lsr 8))
  in
  let off = (block_id lsl 9) lor ((packed land 0xFF) lsl 1) in
  Char.code (String.unsafe_get Unicode_data.prop_data off)
  lor (Char.code (String.unsafe_get Unicode_data.prop_data (off + 1)) lsl 8)

(* Public API - O(1) lookup *)

let[@inline] grapheme_cluster_break u = get u land 0x1F
let[@inline] indic_conjunct_break u = (get u lsr 5) land 0x03
let[@inline] is_extended_pictographic u = get u land 0x80 <> 0
let[@inline] tty_width_hint u = ((get u lsr 8) land 0x03) - 1

(* Combined lookup - returns packed (gcb, incb, extpic) in one access. Returns:
   bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic *)
let[@inline] grapheme_props u = get u land 0xFF

(* Full packed lookup - returns all properties including width in one access.
   Returns: bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic, bits 8-9 =
   width_enc (0=-1, 1=0, 2=1, 3=2) *)
let[@inline] all_props u = get u
