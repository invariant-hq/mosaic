(** CSS length-percentage-auto values.

    This module provides a type for representing CSS dimensions that can be
    absolute lengths, percentages relative to a containing block, or the [auto]
    keyword. It wraps {!Compact_length.t} to provide a constrained interface.

    See {!Compact_length} for conventions on percentage range, abstract units,
    and f32 precision. *)

(** {1 Type} *)

type t = Compact_length.t
(** A length-percentage-auto value. Can be an absolute length, a percentage,
    [auto], or a [calc()] expression. *)

(** {1 Constructors} *)

val length : float -> t
(** [length value] creates an absolute length value. *)

val percent : float -> t
(** [percent value] creates a percentage length relative to the containing
    block.

    The [value] is a fraction in the range [0.0, 1.0], where [0.5] represents
    50%. For 0–100 percentage inputs, prefer {!pct}. *)

val px : float -> t
(** [px value] creates an absolute length value, equivalent to {!length}. *)

val pct : float -> t
(** [pct percent] creates a percentage length from a 0–100 value.

    [pct 50.0] is equivalent to [percent 0.5]. *)

val auto : t
(** [auto] represents automatic dimension computation according to
    algorithm-specific rules. *)

val calc : int -> t
(** [calc index] creates a [calc()] expression value.

    The [index] is an opaque handle to the actual calc representation, typically
    used as an index into a table of calc functions. *)

(** {1 Constants} *)

val zero : t
(** [zero] is an absolute length of 0.0. Equivalent to [length 0.0]. *)

(** {1 Inspection} *)

val is_length : t -> bool
(** [is_length t] returns [true] if [t] is an absolute length. *)

val is_percent : t -> bool
(** [is_percent t] returns [true] if [t] is a percentage. *)

val is_auto : t -> bool
(** [is_auto t] returns [true] if [t] is [auto]. *)

val is_calc : t -> bool
(** [is_calc t] returns [true] if [t] is a [calc()] expression. *)

(** {1 Value Extraction} *)

val value : t -> float
(** [value t] extracts the numeric value from [t].

    Raises [Failure] if [t] is [auto] or a [calc()] expression, as those have no
    numeric value. *)

(** {1 Resolution} *)

val resolve_to_option : t -> float -> float option
(** [resolve_to_option t context] resolves [t] to an absolute length.

    Returns:
    - [Some length] for absolute length values
    - [Some (context * percent)] for percentage values
    - [None] for [auto] values

    Percentage results are rounded to f32 precision to match Taffy behavior.

    Raises [Failure] if [t] is a [calc()] expression or any unsupported tag. Use
    {!resolve_to_option_with_calc} for calc support. *)

val resolve_to_option_with_calc :
  t -> float -> (int -> float -> float) -> float option
(** [resolve_to_option_with_calc t context calc_resolver] resolves [t] to an
    absolute length with calc support.

    Returns:
    - [Some length] for absolute length values
    - [Some (context * percent)] for percentage values
    - [None] for [auto] values
    - [Some (calc_resolver index context)] for [calc()] expressions

    Percentage results are rounded to f32 precision to match Taffy behavior.

    Raises [Failure] if [t] is not length, percentage, auto, or calc. *)

val maybe_resolve : t -> float option -> (int -> float -> float) -> float option
(** [maybe_resolve t context calc_resolver] conditionally resolves [t] to an
    absolute length.

    Returns:
    - [Some length] for absolute length values
    - [None] if [context] is [None] for percentage or calc values
    - [Some (ctx * percent)] if [context] is [Some ctx] for percentage values
    - [None] for [auto] values
    - [Some (calc_resolver index ctx)] if [context] is [Some ctx] for calc
      values

    Percentage results are rounded to f32 precision to match Taffy behavior.

    This matches taffy's [MaybeResolve] trait for [LengthPercentageAuto].

    Raises [Failure] if [t] is not length, percentage, auto, or calc. *)

val resolve_or_zero : t -> float option -> (int -> float -> float) -> float
(** [resolve_or_zero t context calc_resolver] resolves [t] to an absolute length
    with a fallback to [0.0].

    Returns:
    - [length] for absolute length values
    - [0.0] if [context] is [None] for percentage or calc values
    - [ctx * percent] if [context] is [Some ctx] for percentage values
    - [0.0] for [auto] values
    - [calc_resolver index ctx] if [context] is [Some ctx] for calc values

    This matches taffy's [ResolveOrZero] trait for [LengthPercentageAuto].

    Raises [Failure] if [t] has an invalid tag. *)

(** {1 Additional Helpers} *)

val uses_percentage : t -> bool
(** [uses_percentage t] returns [true] if [t] is a percentage or a [calc()]
    expression that might contain percentages. *)

val resolved_percentage_size : t -> float -> float option
(** [resolved_percentage_size t parent_size] resolves percentage values to
    absolute lengths.

    Returns [Some (parent_size * percent)] if [t] is a percentage, [None]
    otherwise. *)

val resolved_percentage_size_with_calc :
  t -> float -> (int -> float -> float) -> float option
(** [resolved_percentage_size_with_calc t parent_size calc_resolver] resolves
    percentage and calc values to absolute lengths.

    Returns:
    - [Some (parent_size * percent)] if [t] is a percentage
    - [Some (calc_resolver index parent_size)] if [t] is a calc expression
    - [None] otherwise *)

(** {1 Formatting and Comparison} *)

val to_string : t -> string
(** [to_string t] converts [t] to a string representation for debugging. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality of [a] and [b].

    For numeric values, uses epsilon comparison for floating-point tolerance. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering for [a] and [b].

    Calc expressions are ordered before other variants by index, then by tag,
    then by numeric value. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints [t] to the formatter [fmt]. *)

(**/**)

val resolve_rect_or_zero :
  float option ->
  (int -> float -> float) ->
  t Toffee_geometry.Rect.t ->
  float Toffee_geometry.Rect.t
(** [resolve_rect_or_zero context calc_resolver rect] is [rect] with
    {!resolve_or_zero} applied to each side. A rect of zero lengths resolves to
    {!Toffee_geometry.Rect.zero} without allocating. *)
