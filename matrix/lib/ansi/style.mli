(** Terminal text styles.

    A style aggregates foreground color, background color, text attributes, and
    an optional hyperlink URL into an immutable value. Styles compose with
    overlay semantics: colors and hyperlinks from the overlay replace those in
    the base, while attributes are unioned. See {!merge}. *)

(** {1:styles Styles} *)

type t = private {
  fg : Color.t option;
  bg : Color.t option;
  attrs : Attr.t;
  link : string option;
}
(** The type for styles. Fields are exposed for pattern matching but cannot be
    modified directly. Use {!make} or the modifier functions to create new
    instances. *)

val default : t
(** [default] is the empty style: no colors, no attributes, no hyperlink. *)

(** {1:constructors Constructors} *)

val make :
  ?fg:Color.t ->
  ?bg:Color.t ->
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?blink:bool ->
  ?inverse:bool ->
  ?hidden:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?double_underline:bool ->
  ?framed:bool ->
  ?encircled:bool ->
  ?link:string ->
  unit ->
  t
(** [make ?fg ?bg ... ()] is a style with the given properties. [fg] and [bg]
    default to [None] (inherit terminal default). Boolean attributes default to
    [false]. [link] defaults to [None]. *)

(** {1:modifiers Modifiers} *)

(** {2:mod_colors Colors} *)

val fg : Color.t -> t -> t
(** [fg c s] is [s] with foreground set to [c]. *)

val bg : Color.t -> t -> t
(** [bg c s] is [s] with background set to [c]. *)

val with_no_fg : t -> t
(** [with_no_fg s] is [s] with no foreground (inherits terminal default). *)

val with_no_bg : t -> t
(** [with_no_bg s] is [s] with no background (inherits terminal default). *)

(** {2:mod_attrs Attributes} *)

val with_attrs : Attr.t -> t -> t
(** [with_attrs a s] is [s] with attributes replaced by [a]. *)

val overlay_attrs : t -> Attr.t -> t
(** [overlay_attrs s a] is [s] with [a] unioned into its existing attributes. *)

val add_attr : Attr.flag -> t -> t
(** [add_attr f s] is [s] with [f] enabled. *)

val remove_attr : Attr.flag -> t -> t
(** [remove_attr f s] is [s] with [f] disabled. *)

val with_bold : bool -> t -> t
(** [with_bold b s] sets or clears bold on [s]. *)

val with_dim : bool -> t -> t
(** [with_dim b s] sets or clears dim on [s]. *)

val with_italic : bool -> t -> t
(** [with_italic b s] sets or clears italic on [s]. *)

val with_underline : bool -> t -> t
(** [with_underline b s] sets or clears underline on [s]. *)

val with_double_underline : bool -> t -> t
(** [with_double_underline b s] sets or clears double underline on [s]. *)

val with_blink : bool -> t -> t
(** [with_blink b s] sets or clears blink on [s]. *)

val with_inverse : bool -> t -> t
(** [with_inverse b s] sets or clears inverse on [s]. *)

val with_hidden : bool -> t -> t
(** [with_hidden b s] sets or clears hidden on [s]. *)

val with_strikethrough : bool -> t -> t
(** [with_strikethrough b s] sets or clears strikethrough on [s]. *)

val with_overline : bool -> t -> t
(** [with_overline b s] sets or clears overline on [s]. *)

val with_framed : bool -> t -> t
(** [with_framed b s] sets or clears framed on [s]. *)

val with_encircled : bool -> t -> t
(** [with_encircled b s] sets or clears encircled on [s]. *)

(** {2:mod_links Hyperlinks} *)

val hyperlink : string -> t -> t
(** [hyperlink url s] is [s] with OSC 8 hyperlink set to [url]. *)

val link : t -> string option
(** [link s] is the hyperlink URL of [s], if any. *)

val unlink : t -> t
(** [unlink s] is [s] with the hyperlink removed. *)

(** {1:composition Composition} *)

val merge : base:t -> overlay:t -> t
(** [merge ~base ~overlay] is the style combining [base] and [overlay]:
    - Colors: [overlay] takes precedence. If [overlay.fg] is [None], [base.fg]
      is kept.
    - Attributes: union of [base] and [overlay] attributes.
    - Link: [overlay] takes precedence. *)

val ( ++ ) : t -> t -> t
(** [base ++ overlay] is [merge ~base ~overlay]. *)

val resolve : t list -> t
(** [resolve ss] merges [ss] left to right starting from {!default}. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff all colors, attributes, and links are identical.
*)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b]. The order is compatible with {!equal}. *)

val hash : t -> int
(** [hash s] is a hash of [s]. Compatible with {!equal}. *)

(** {1:emission Emission} *)

val to_sgr_codes : ?prev:t -> t -> int list
(** [to_sgr_codes ~prev s] is the minimal SGR codes needed to transition from
    [prev] to [s]. [prev] defaults to {!default}. Returns [[]] if [prev] and [s]
    are equal. Returns [[0]] if [s] is {!default}. *)

val sgr_sequence : ?prev:t -> t -> string
(** [sgr_sequence ~prev s] is the ANSI escape string for
    {!to_sgr_codes}[ ~prev s]. Returns [""] if no transition is needed. *)

val emit : ?prev:t -> t -> Writer.t -> unit
(** [emit ~prev s w] writes the minimal SGR codes to [w] to transition from
    [prev] (defaults to {!default}) to [s].

    Handles shared disable codes correctly (Bold/Dim share 22,
    Underline/Double_underline share 24, Framed/Encircled share 54).

    {b Note.} Emits SGR sequences only. The [link] field is not emitted here;
    use {!Ansi.render} for hyperlink support. *)

val styled : ?reset:bool -> t -> string -> string
(** [styled ~reset s str] is [str] wrapped in the escape codes for [s]. If
    [reset] is [true] (default [false]), appends a reset sequence. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a style for inspection (e.g.
    ["Style\{fg=#FF0000, attrs=[Bold, Underline]\}"]). *)
