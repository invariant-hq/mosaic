(** CSS Grid track sizing functions.

    A track sizing function specifies the minimum and maximum sizes of a grid
    track (row or column). Grid tracks automatically size between their minimum
    and maximum based on content size, available space, and the sizing
    constraints applied to the grid.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns}MDN:
     grid-template-columns} *)

type t = {
  min : Compact_length.t;  (** Minimum track sizing function. *)
  max : Compact_length.t;  (** Maximum track sizing function. *)
}
(** A track sizing function combining minimum and maximum sizing constraints. *)

(** {1 Construction} *)

val make : min:Compact_length.t -> max:Compact_length.t -> t
(** [make ~min ~max] creates a track sizing function with explicit min and max
    constraints. *)

val auto : t
(** [auto] creates a track sized automatically according to grid algorithm
    rules. Equivalent to [minmax(auto, auto)]. *)

val min_content : t
(** [min_content] creates a track sized to the smallest size that fits content
    with all soft line-wrapping opportunities taken. Equivalent to
    [minmax(min-content, min-content)]. *)

val max_content : t
(** [max_content] creates a track sized to the smallest size that fits content
    with no soft line-wrapping opportunities taken. Equivalent to
    [minmax(max-content, max-content)]. *)

val zero : t
(** [zero] creates a zero-sized track. *)

val fr : float -> t
(** [fr value] creates a flexible track that takes a fraction of available
    space. The track receives [value] parts out of the sum of all fr values in
    the grid dimension. Equivalent to [minmax(auto, <value>fr)].

    See
    {{:https://www.w3.org/TR/css3-grid-layout/#fr-unit}CSS Grid fr unit
     specification}. *)

val length : float -> t
(** [length value] creates a fixed-size track with the specified length.
    Equivalent to [minmax(<value>px, <value>px)]. *)

val percent : float -> t
(** [percent value] creates a track sized as a percentage of the grid container.

    The [value] is a fraction in the range [0.0, 1.0], where [0.5] represents
    50%. See {!Compact_length} for percentage conventions. *)

val from_length_percentage : Compact_length.t -> t
(** [from_length_percentage lp] creates a track from a length or percentage
    value, applying it to both min and max sizing functions. *)

val fit_content : Compact_length.t -> t
(** [fit_content lp] creates a track sized by the fit-content formula:
    [max(min-content, min(max-content, lp))].

    The track takes the size of [lp] clamped between min-content and max-content
    sizes. Equivalent to [minmax(auto, fit-content(<lp>))]. *)

val minmax : min:Compact_length.t -> max:Compact_length.t -> t
(** [minmax ~min ~max] creates a track sizing function with separate min and max
    constraints. Alias for {!make}. *)

(** {1 Accessors} *)

val min : t -> Compact_length.t
(** [min t] returns the minimum sizing function. *)

val max : t -> Compact_length.t
(** [max t] returns the maximum sizing function. *)

val min_sizing_function : t -> Compact_length.t
(** [min_sizing_function t] returns the minimum sizing function. Alias for
    {!val-min}. *)

val max_sizing_function : t -> Compact_length.t
(** [max_sizing_function t] returns the maximum sizing function. Alias for
    {!val-max}. *)

(** {1 Predicates} *)

val has_fixed_component : t -> bool
(** [has_fixed_component t] returns [true] if at least one component (min or
    max) is a fixed length or percentage value. *)

(** {1 Comparison and Display} *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] represent the same track sizing
    function. *)

val compare : t -> t -> int
(** [compare a b] compares [a] and [b] lexicographically, first by min then by
    max. *)

val to_string : t -> string
(** [to_string t] converts [t] to a string representation. Returns a compact
    form when min equals max, otherwise returns [minmax(min, max)] form. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats [t] for pretty-printing. *)

(** {1 Minimum Sizing Function Helpers} *)

(** Operations on the minimum sizing function component. *)
module Min : sig
  val is_intrinsic : t -> bool
  (** [is_intrinsic t] returns [true] if the min component is [min-content],
      [max-content], or [auto]. *)

  val is_min_or_max_content : t -> bool
  (** [is_min_or_max_content t] returns [true] if the min component is
      [min-content] or [max-content]. *)

  val is_fr : t -> bool
  (** [is_fr t] returns [true] if the min component is an [fr] value. *)

  val is_auto : t -> bool
  (** [is_auto t] returns [true] if the min component is [auto]. *)

  val is_min_content : t -> bool
  (** [is_min_content t] returns [true] if the min component is [min-content].
  *)

  val is_max_content : t -> bool
  (** [is_max_content t] returns [true] if the min component is [max-content].
  *)

  val definite_value : t -> float option -> float option
  (** [definite_value t parent_size] resolves the min component to a concrete
      value.

      Returns [Some value] for fixed lengths or percentages (when [parent_size]
      is [Some]). Returns [None] for intrinsic or flexible values. *)

  val definite_value_with_calc :
    t -> float option -> (int -> float -> float) -> float option
  (** [definite_value_with_calc t parent_size calc_resolver] resolves the min
      component, including [calc()] expressions.

      The [calc_resolver] is called with a calc index and parent size to resolve
      calc values. *)

  val resolved_percentage_size : t -> float -> float option
  (** [resolved_percentage_size t parent_size] resolves percentage values.

      Returns [Some (value * parent_size)] for percentage values. Returns [None]
      otherwise. *)

  val uses_percentage : t -> bool
  (** [uses_percentage t] returns [true] if the min component depends on parent
      size. *)

  val behaves_as_auto : t -> float option -> bool
  (** [behaves_as_auto t parent_size] returns [true] if the min component is
      [auto], or depends on [parent_size] and [parent_size] is [None]: a
      percentage min track sizing function against an indefinite container
      behaves as [auto]. *)
end

(** {1 Maximum Sizing Function Helpers} *)

(** Operations on the maximum sizing function component. *)
module Max : sig
  val is_intrinsic : t -> bool
  (** [is_intrinsic t] returns [true] if the max component is [min-content],
      [max-content], [fit-content], or [auto]. *)

  val is_max_content_alike : t -> bool
  (** [is_max_content_alike t] returns [true] if the max component is
      [max-content], [fit-content], or [auto].

      Per the CSS Grid specification, [auto] and [fit-content] are treated as
      [max-content] in most contexts. *)

  val is_fr : t -> bool
  (** [is_fr t] returns [true] if the max component is an [fr] value. *)

  val is_auto : t -> bool
  (** [is_auto t] returns [true] if the max component is [auto]. *)

  val is_min_content : t -> bool
  (** [is_min_content t] returns [true] if the max component is [min-content].
  *)

  val is_max_content : t -> bool
  (** [is_max_content t] returns [true] if the max component is [max-content].
  *)

  val is_fit_content : t -> bool
  (** [is_fit_content t] returns [true] if the max component is [fit-content].
  *)

  val is_max_or_fit_content : t -> bool
  (** [is_max_or_fit_content t] returns [true] if the max component is
      [max-content] or [fit-content]. *)

  val fr_value : t -> float
  (** [fr_value t] returns the [fr] coefficient if the max component is an [fr]
      value. Returns [0.0] otherwise. *)

  val has_definite_value : t -> float option -> bool
  (** [has_definite_value t parent_size] returns [true] if the max component can
      be resolved to a concrete value given [parent_size]. *)

  val definite_value : t -> float option -> float option
  (** [definite_value t parent_size] resolves the max component to a concrete
      value.

      Returns [Some value] for fixed lengths or percentages (when [parent_size]
      is [Some]). Returns [None] for intrinsic or flexible values. *)

  val definite_value_with_calc :
    t -> float option -> (int -> float -> float) -> float option
  (** [definite_value_with_calc t parent_size calc_resolver] resolves the max
      component, including [calc()] expressions.

      The [calc_resolver] is called with a calc index and parent size to resolve
      calc values. *)

  val definite_limit : t -> float option -> float option
  (** [definite_limit t parent_size] resolves the maximum limit of the track.

      For [fit-content] values, returns the fit-content limit. Otherwise behaves
      like {!definite_value}. *)

  val definite_limit_with_calc :
    t -> float option -> (int -> float -> float) -> float option
  (** [definite_limit_with_calc t parent_size calc_resolver] resolves the
      maximum limit, including [calc()] expressions in [fit-content] arguments.
  *)

  val resolved_percentage_size : t -> float -> float option
  (** [resolved_percentage_size t parent_size] resolves percentage values.

      Returns [Some (value * parent_size)] for percentage values. Returns [None]
      otherwise. *)

  val uses_percentage : t -> bool
  (** [uses_percentage t] returns [true] if the max component depends on parent
      size. *)
end
