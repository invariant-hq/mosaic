(* WARNING: Do not edit. This file was automatically generated.

   Unicode version 17.0.0.
   Generated using matrix/support/gen_unicode_data.ml
*)

[@@@ocamlformat "disable"]

(** Block index: maps block numbers (codepoint lsr 8) to deduplicated
    block IDs. Each entry is 1 byte. *)
val prop_index : string

(** Deduplicated block data: concatenated 512-byte blocks, each containing
    256 packed 16-bit entries. Lookup: prop_data.[block_id * 512 + (cp land 0xFF) * 2]. *)
val prop_data : string
