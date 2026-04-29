(** Unicode property lookups.

    O(1) property lookups for grapheme segmentation and display-width
    calculation using compact two-level page tables generated from
    {{:https://github.com/ocaml/uucp}uucp}. See
    [matrix/support/gen_unicode_data.ml] for the generator. *)

(** {1:grapheme Grapheme properties} *)

val grapheme_cluster_break : Uchar.t -> int
(** [grapheme_cluster_break u] is the
    {{:https://www.unicode.org/reports/tr29/}UAX #29} Grapheme Cluster Break
    property of [u] as an index in \[0;17\]. *)

val indic_conjunct_break : Uchar.t -> int
(** [indic_conjunct_break u] is the Indic Conjunct Break property of [u] as an
    index in \[0;3\]. Used for rule GB9c in UAX #29. *)

val is_extended_pictographic : Uchar.t -> bool
(** [is_extended_pictographic u] is [true] iff [u] has the Extended_Pictographic
    property. Used for rule GB11 in UAX #29. *)

(** {1:width Width} *)

val tty_width_hint : Uchar.t -> int
(** [tty_width_hint u] is the suggested terminal display width of [u]:
    - [-1] for control characters (C0, DEL, C1).
    - [0] for non-spacing marks and format characters.
    - [1] for most characters.
    - [2] for wide and fullwidth East Asian characters. *)

(** {1:combined Combined lookups} *)

val packed_props : Uchar.t -> int
(** [packed_props u] is all properties in a single table lookup. Bit layout:
    bits 0–4 = gcb, bits 5–6 = incb, bit 7 = extpic, bits 8–9 = width encoding
    (0 = -1, 1 = 0, 2 = 1, 3 = 2). *)

val grapheme_props : Uchar.t -> int
(** [grapheme_props u] is all grapheme properties in a single lookup. Bit
    layout: bits 0–4 = gcb, bits 5–6 = incb, bit 7 = extpic. *)
