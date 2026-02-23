(** CSS text-align property for block layout.

    Implements legacy HTML alignment behavior for block-level elements,
    corresponding to vendor-prefixed text-align values in browsers.

    Text alignment affects the horizontal positioning of in-flow block children
    within their container. It does not affect absolutely positioned elements or
    non-block layouts. *)

type t =
  | Auto
      (** No special text alignment behavior. Block children remain
          left-aligned. *)
  | Legacy_left
      (** Corresponds to [-webkit-left] or [-moz-left] in browsers. Equivalent
          to left alignment. *)
  | Legacy_right
      (** Corresponds to [-webkit-right] or [-moz-right] in browsers.
          Right-aligns block children. *)
  | Legacy_center
      (** Corresponds to [-webkit-center] or [-moz-center] in browsers. Centers
          block children horizontally. *)

val default : t
(** [default] is [Auto]. *)

val to_string : t -> string
(** [to_string align] is the CSS string representation of [align]: ["auto"],
    ["legacy-left"], ["legacy-right"], or ["legacy-center"]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same alignment. *)

val compare : t -> t -> int
(** [compare a b] is a total order on alignment values, compatible with
    {!equal}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf align] formats [align] on [ppf] using {!to_string}. *)
