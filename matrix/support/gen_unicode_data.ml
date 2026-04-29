(* Generate pre-computed Unicode property table for matrix.glyph.

   Packs all Unicode properties needed for grapheme segmentation and width
   calculation into a two-level page table with block deduplication. Each
   codepoint maps to a 16-bit entry, giving O(1) lookups at runtime with zero
   initialization cost.

   Structure: - prop_index: maps block numbers to deduplicated block IDs (1 byte
   each) - prop_data: concatenated deduplicated blocks (512 bytes each)

   The codepoint space is divided into 256-entry blocks. Blocks with identical
   content share the same data, dramatically reducing size for the large
   unassigned/CJK/etc. ranges that share properties.

   Layout per 16-bit entry: - bits 0-4: grapheme_cluster_break (0-17) - bits
   5-6: indic_conjunct_break (0-3) - bit 7: extended_pictographic (boolean) -
   bits 8-9: width (encoded: 0=-1, 1=0, 2=1, 3=2)

   Surrogates (U+D800-U+DFFF) are excluded from the table.

   Adapted from notty's generator (Copyright (c) 2020 David Kaloper Meršinjak).

   Usage: dune exec matrix/support/gen_unicode_data.exe *)

let unicode_packed_size = 0x110000 - 0x800
let block_size = 256
let block_bytes = block_size * 2

let terminal_wide_symbol_override cp =
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

let compute_prop_table () =
  let buf = Bytes.create (unicode_packed_size * 2) in
  for packed = 0 to unicode_packed_size - 1 do
    let u = Uchar.of_int (if packed < 0xD800 then packed else packed + 0x800) in
    let gcb = Uucp.Break.Low.grapheme_cluster u in
    let incb = Uucp.Break.Low.indic_conjunct_break u in
    let extpic = Uucp.Emoji.is_extended_pictographic u in
    let cp = Uchar.to_int u in
    let width_raw = Uucp.Break.tty_width_hint u in
    let width_raw =
      if width_raw = 1 && terminal_wide_symbol_override cp then 2 else width_raw
    in
    let width_enc = width_raw + 1 in
    let v =
      gcb land 0x1F
      lor ((incb land 0x03) lsl 5)
      lor (if extpic then 0x80 else 0)
      lor ((width_enc land 0x03) lsl 8)
    in
    Bytes.set_uint16_le buf (packed * 2) v
  done;
  Bytes.unsafe_to_string buf

let compress_prop_table flat =
  let num_blocks = unicode_packed_size / block_size in
  (* Extract and deduplicate blocks *)
  let block_map = Hashtbl.create 128 in
  let unique_blocks = Buffer.create (64 * block_bytes) in
  let next_id = ref 0 in
  let index = Bytes.create num_blocks in
  for i = 0 to num_blocks - 1 do
    let block = String.sub flat (i * block_bytes) block_bytes in
    let id =
      match Hashtbl.find_opt block_map block with
      | Some id -> id
      | None ->
          let id = !next_id in
          Hashtbl.add block_map block id;
          Buffer.add_string unique_blocks block;
          incr next_id;
          id
    in
    Bytes.set index i (Char.chr id)
  done;
  let num_unique = !next_id in
  assert (num_unique <= 256);
  (Bytes.unsafe_to_string index, Buffer.contents unique_blocks, num_unique)

let header =
  Printf.sprintf
    "(* WARNING: Do not edit. This file was automatically generated.\n\n\
    \   Unicode version %s.\n\
    \   Generated using matrix/support/gen_unicode_data.ml\n\
     *)\n\n\
     [@@@ocamlformat \"disable\"]\n\n"
    Uucp.unicode_version

let write_string_literal oc name data =
  Printf.fprintf oc "let %s = \"\\\n  " name;
  let bytes_per_line = 40 in
  for i = 0 to String.length data - 1 do
    Printf.fprintf oc "\\x%02x" (Char.code (String.get data i));
    if (i + 1) mod bytes_per_line = 0 && i + 1 < String.length data then
      output_string oc "\\\n  "
  done;
  output_string oc "\"\n\n"

let write_mli oc =
  output_string oc header;
  output_string oc
    "(** Block index: maps block numbers (codepoint lsr 8) to deduplicated\n\
    \    block IDs. Each entry is 1 byte. *)\n\
     val prop_index : string\n\n\
     (** Deduplicated block data: concatenated 512-byte blocks, each containing\n\
    \    256 packed 16-bit entries. Lookup: prop_data.[block_id * 512 + (cp \
     land 0xFF) * 2]. *)\n\
     val prop_data : string\n"

let write_ml oc =
  let flat = compute_prop_table () in
  let index, data, num_unique = compress_prop_table flat in
  output_string oc header;
  Printf.fprintf oc
    "(* %d unique blocks out of %d total (%.1f%% dedup, %d bytes data) *)\n\n"
    num_unique
    (unicode_packed_size / block_size)
    (100.
    *. (1.
       -. float_of_int num_unique
          /. float_of_int (unicode_packed_size / block_size)))
    (String.length data);
  write_string_literal oc "prop_index" index;
  write_string_literal oc "prop_data" data

let file = "matrix/lib/glyph/unicode_data"

let () =
  Format.printf "Dumping Unicode v%s data to %s.@." Uucp.unicode_version file;
  let write name f =
    let oc = open_out_gen [ Open_trunc; Open_creat; Open_wronly ] 0o664 name in
    f oc;
    close_out oc
  in
  write (file ^ ".mli") write_mli;
  write (file ^ ".ml") write_ml
