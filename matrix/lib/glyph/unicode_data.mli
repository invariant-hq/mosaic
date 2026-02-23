(* WARNING: Do not edit. This file was automatically generated.

   Unicode version 17.0.0. Generated using matrix/support/gen_unicode_data.ml *)

[@@@ocamlformat "disable"]

(** Generated Unicode property tables.

    Pre-computed, deduplicated two-level page tables for O(1)
    property lookups. Do not edit; regenerate with
    [matrix/support/gen_unicode_data.ml]. *)

val prop_index : string
(** Block index mapping block numbers ([codepoint lsr 8]) to
    deduplicated block IDs. Each entry is 1 byte. *)

val prop_data : string
(** Deduplicated block data. Concatenated 512-byte blocks, each
    containing 256 packed 16-bit entries. Lookup:
    [prop_data.\[block_id * 512 + (cp land 0xFF) * 2\]]. *)
