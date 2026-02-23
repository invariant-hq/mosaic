(** Text attribute flags.

    A compact bit-flag representation for text attributes (bold, italic,
    underline, etc.). Attributes are stored as a 12-bit integer bitmask,
    enabling fast set operations and minimal memory usage.

    Terminal support varies. Bold, underline, and inverse are widely supported.
    Blink, framed, and encircled have limited support; unsupported attributes
    simply have no visible effect. *)

(** {1:flags Flags} *)

(** The type for individual text attribute flags. *)
type flag =
  | Bold  (** Increased weight/brightness (SGR 1). *)
  | Dim  (** Decreased brightness (SGR 2). *)
  | Italic  (** Slanted text (SGR 3). *)
  | Underline  (** Single underline (SGR 4). *)
  | Double_underline  (** Double underline (SGR 21). *)
  | Blink  (** Blinking text (SGR 5). *)
  | Inverse  (** Swap foreground/background (SGR 7). *)
  | Hidden  (** Invisible text (SGR 8). *)
  | Strikethrough  (** Line through text (SGR 9). *)
  | Overline  (** Line above text (SGR 53). *)
  | Framed  (** Framed text (SGR 51). *)
  | Encircled  (** Encircled text (SGR 52). *)

val flag_to_sgr_code : flag -> int
(** [flag_to_sgr_code f] is the SGR code to enable [f]. *)

val flag_to_sgr_disable_code : flag -> int
(** [flag_to_sgr_disable_code f] is the SGR code to disable [f].

    {b Note.} Some flags share disable codes: Bold and Dim both use 22,
    Underline and Double_underline both use 24, Framed and Encircled both use
    54. *)

val flag_to_string : flag -> string
(** [flag_to_string f] is the name of [f] as a string. *)

(** {1:sets Attribute sets} *)

type t
(** The type for attribute sets. Internally a 12-bit integer bitmask. *)

(** {2:predefined Predefined sets} *)

val empty : t
(** [empty] is the set with no attributes. *)

val bold : t
val dim : t
val italic : t
val underline : t
val double_underline : t
val blink : t
val inverse : t
val hidden : t
val strikethrough : t
val overline : t
val framed : t
val encircled : t

(** {2:predicates Predicates} *)

val is_empty : t -> bool
(** [is_empty s] is [true] iff [s] contains no flags. *)

val mem : flag -> t -> bool
(** [mem f s] is [true] iff [f] is in [s]. *)

(** {2:ops Set operations} *)

val add : flag -> t -> t
(** [add f s] is [s] with [f] added. Idempotent. *)

val remove : flag -> t -> t
(** [remove f s] is [s] with [f] removed. Idempotent. *)

val toggle : flag -> t -> t
(** [toggle f s] adds [f] if absent, removes it if present. *)

val union : t -> t -> t
(** [union a b] is the set containing flags in either [a] or [b]. *)

val intersect : t -> t -> t
(** [intersect a b] is the set containing flags in both [a] and [b]. *)

val diff : t -> t -> t
(** [diff a b] is the set of flags in [a] not in [b]. *)

val cardinal : t -> int
(** [cardinal s] is the number of flags in [s]. *)

val with_flag : flag -> bool -> t -> t
(** [with_flag f enabled s] adds [f] if [enabled] is [true], removes it
    otherwise. *)

(** {1:converting Converting} *)

val of_list : flag list -> t
(** [of_list fs] is a set from the list [fs]. Duplicates are ignored. *)

val to_list : t -> flag list
(** [to_list s] is the flags in [s] as a list. Order is deterministic but
    unspecified. *)

val combine :
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?double_underline:bool ->
  ?blink:bool ->
  ?inverse:bool ->
  ?hidden:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?framed:bool ->
  ?encircled:bool ->
  unit ->
  t
(** [combine ?bold ?dim ... ()] is a set from labelled arguments. Each parameter
    defaults to [false]. *)

(** {1:sgr ANSI SGR codes} *)

val to_sgr_codes : t -> int list
(** [to_sgr_codes s] is the SGR enable codes for [s]. Use {!iter_sgr_codes} or
    {!fold_sgr_codes} to avoid allocation. *)

val iter_sgr_codes : (int -> unit) -> t -> unit
(** [iter_sgr_codes f s] calls [f code] for each SGR enable code in [s]. *)

val iter_sgr_disable_codes : (int -> unit) -> t -> unit
(** [iter_sgr_disable_codes f s] calls [f code] for each SGR disable code in
    [s]. Shared codes are deduplicated (e.g. if both Bold and Dim are in [s],
    code 22 is emitted once). *)

val fold_sgr_codes : (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_sgr_codes f s init] folds [f] over SGR enable codes of [s]. *)

(** {1:iteration Iteration} *)

val fold : (flag -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f s init] folds [f] over the flags in [s]. *)

val iter : (flag -> unit) -> t -> unit
(** [iter f s] calls [f] for each flag in [s]. *)

(** {1:predicates_cmp Comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] contain the same flags. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b]. The order is compatible with {!equal}. *)

(** {1:encoding Binary encoding} *)

val pack : t -> int
(** [pack s] is the integer representation of [s]. Stable across releases. *)

val unpack : int -> t
(** [unpack n] is the attribute set from the integer [n]. Inverse of {!pack}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an attribute set for inspection (e.g. ["[Bold, Italic]"]). *)
