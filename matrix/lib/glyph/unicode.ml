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

let[@inline] terminal_wide_symbol_override cp =
  (cp >= 0x1F000 && cp <= 0x1F02B)
  || (cp >= 0x1F030 && cp <= 0x1F093)
  || (cp >= 0x1F0A0 && cp <= 0x1F0AE)
  || (cp >= 0x1F0B1 && cp <= 0x1F0BF)
  || (cp >= 0x1F0C1 && cp <= 0x1F0CF)
  || (cp >= 0x1F0D1 && cp <= 0x1F0F5)
  || cp = 0x231A || cp = 0x231B || cp = 0x2329 || cp = 0x232A
  || (cp >= 0x23E9 && cp <= 0x23EC)
  || cp = 0x23F0 || cp = 0x23F3
  || (cp >= 0x25FD && cp <= 0x25FE)
  || (cp >= 0x2614 && cp <= 0x2615)
  || cp = 0x2622 || cp = 0x2623
  || (cp >= 0x2630 && cp <= 0x2637)
  || (cp >= 0x2648 && cp <= 0x2653)
  || cp = 0x267F || cp = 0x2693 || cp = 0x269B || cp = 0x26A0 || cp = 0x26A1
  || (cp >= 0x26AA && cp <= 0x26AB)
  || (cp >= 0x26BD && cp <= 0x26BE)
  || (cp >= 0x26C4 && cp <= 0x26C5)
  || cp = 0x26CE || cp = 0x26D1 || cp = 0x26D4 || cp = 0x26EA || cp = 0x26F2
  || cp = 0x26F3 || cp = 0x26F5 || cp = 0x26FA || cp = 0x26FD || cp = 0x203C
  || cp = 0x2049 || cp = 0x2705
  || (cp >= 0x270A && cp <= 0x270B)
  || cp = 0x2728 || cp = 0x274C || cp = 0x274E
  || (cp >= 0x2753 && cp <= 0x2755)
  || cp = 0x2757
  || (cp >= 0x2760 && cp <= 0x2767)
  || (cp >= 0x2795 && cp <= 0x2797)
  || cp = 0x27B0 || cp = 0x27BF
  || (cp >= 0x2B1B && cp <= 0x2B1C)
  || cp = 0x2B50 || cp = 0x2B55
  || (cp >= 0x1F300 && cp <= 0x1F320)
  || (cp >= 0x1F32D && cp <= 0x1F335)
  || (cp >= 0x1F337 && cp <= 0x1F37C)
  || (cp >= 0x1F37E && cp <= 0x1F393)
  || (cp >= 0x1F3A0 && cp <= 0x1F3CA)
  || (cp >= 0x1F3CF && cp <= 0x1F3D3)
  || (cp >= 0x1F3E0 && cp <= 0x1F3F0)
  || cp = 0x1F3F4
  || (cp >= 0x1F3F8 && cp <= 0x1F3FF)
  || (cp >= 0x1F400 && cp <= 0x1F43E)
  || cp = 0x1F440
  || (cp >= 0x1F442 && cp <= 0x1F4FC)
  || (cp >= 0x1F4FF && cp <= 0x1F6C5)
  || cp = 0x1F6CC
  || (cp >= 0x1F6D0 && cp <= 0x1F6D2)
  || (cp >= 0x1F6D5 && cp <= 0x1F6D7)
  || (cp >= 0x1F6DC && cp <= 0x1F6DF)
  || (cp >= 0x1F6EB && cp <= 0x1F6EC)
  || (cp >= 0x1F6F4 && cp <= 0x1F6FC)
  || (cp >= 0x1F700 && cp <= 0x1F773)
  || (cp >= 0x1F780 && cp <= 0x1F7D8)
  || (cp >= 0x1F7E0 && cp <= 0x1F7EB)
  || (cp >= 0x1F800 && cp <= 0x1F80B)
  || (cp >= 0x1F810 && cp <= 0x1F847)
  || (cp >= 0x1F850 && cp <= 0x1F859)
  || (cp >= 0x1F860 && cp <= 0x1F887)
  || (cp >= 0x1F890 && cp <= 0x1F8AD)
  || (cp >= 0x1F8B0 && cp <= 0x1F8B1)
  || (cp >= 0x1F90C && cp <= 0x1F93A)
  || (cp >= 0x1F93C && cp <= 0x1F945)
  || (cp >= 0x1F947 && cp <= 0x1FA53)
  || (cp >= 0x1FA60 && cp <= 0x1FA6D)
  || (cp >= 0x1FA70 && cp <= 0x1FA74)
  || (cp >= 0x1FA78 && cp <= 0x1FA7C)
  || (cp >= 0x1FA80 && cp <= 0x1FA86)
  || (cp >= 0x1FA90 && cp <= 0x1FAAC)
  || (cp >= 0x1FAB0 && cp <= 0x1FABA)
  || (cp >= 0x1FAC0 && cp <= 0x1FAC5)
  || (cp >= 0x1FAD0 && cp <= 0x1FAD9)
  || (cp >= 0x1FAE0 && cp <= 0x1FAE7)
  || (cp >= 0x1FAF0 && cp <= 0x1FAF8)

let[@inline] tty_width_hint u =
  let cp = Uchar.to_int u in
  if terminal_wide_symbol_override cp then 2 else ((get u lsr 8) land 0x03) - 1

(* Combined lookup - returns packed (gcb, incb, extpic) in one access. Returns:
   bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic *)
let[@inline] grapheme_props u = get u land 0xFF

(* Full packed lookup - returns all properties including width in one access.
   Returns: bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic, bits 8-9 =
   width_enc (0=-1, 1=0, 2=1, 3=2) *)
let[@inline] all_props u =
  let packed = get u in
  if terminal_wide_symbol_override (Uchar.to_int u) then
    packed land lnot 0x0300 lor 0x0300
  else packed
