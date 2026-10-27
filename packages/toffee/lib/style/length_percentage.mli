(** CSS length-percentage values.

    Length-percentage values represent linear measurements in CSS, supporting
    both absolute lengths and relative percentages. Values are stored in a
    compact tagged representation via {!Compact_length}.

    See {!Compact_length} for conventions on percentage range, abstract units,
    and f32 precision. *)

type t = Compact_length.t
(** A CSS length-percentage value. *)

(** {1 Constructors} *)

val length : float -> t
(** [length value] creates an absolute length. *)

val percent : float -> t
(** [percent value] creates a percentage length.

    The [value] is a fraction in the range [0.0, 1.0], where [0.5] represents
    50%. For 0–100 percentage inputs, prefer {!pct}. *)

val px : float -> t
(** [px value] creates an absolute length, equivalent to {!length}. *)

val pct : float -> t
(** [pct percent] creates a percentage length from a 0–100 value.

    [pct 50.0] is equivalent to [percent 0.5]. *)

val calc : int -> t
(** [calc index] creates a calc expression.

    The [index] is an opaque handle to the actual calc representation. The low 3
    bits are reserved as a tag and returned as 0. *)

(** {1 Constants} *)

val zero : t
(** Zero-length constant, equivalent to [length 0.0]. *)

(** {1 Inspection} *)

val is_length : t -> bool
(** [is_length t] returns [true] if [t] is an absolute length. *)

val is_percent : t -> bool
(** [is_percent t] returns [true] if [t] is a percentage. *)

val is_calc : t -> bool
(** [is_calc t] returns [true] if [t] is a calc expression. *)

(** {1 Value Extraction} *)

val value : t -> float
(** [value t] extracts the numeric value.

    For length values, returns the length. For percentage values, returns the
    percentage in the range [0.0, 1.0].

    Raises [Failure] if [t] is a calc or any unsupported tag. *)

(** {1 Resolution} *)

val resolve : t -> float -> float
(** [resolve t context] resolves [t] to an absolute length.

    For length values, returns the length unchanged. For percentage values,
    computes [context * value t] with f32 precision.

    Raises [Failure] if [t] is a calc expression or any unsupported tag
    (auto/min-/max-content or fit-content). Use {!resolve_with_calc} for calc
    support. *)

val resolve_with_calc : t -> float -> (int -> float -> float) -> float
(** [resolve_with_calc t context calc_resolver] resolves [t] to an absolute
    length.

    For length and percentage values, behaves like {!resolve}. For calc values,
    invokes [calc_resolver index context] where [index] is the calc expression
    handle.

    Raises [Failure] if [t] is not a length, percentage, or calc value. *)

val maybe_resolve : t -> float option -> (int -> float -> float) -> float option
(** [maybe_resolve t context calc_resolver] resolves [t] if [context] is
    available.

    Returns [Some length] for absolute lengths. Returns [Some resolved] for
    percentages and calc expressions when [context] is [Some dim]. Returns
    [None] when [context] is [None] and [t] is a percentage or calc requiring
    context.

    Raises [Failure] if [t] is not a length, percentage, or calc value. *)

val resolve_or_zero : t -> float option -> (int -> float -> float) -> float
(** [resolve_or_zero t context calc_resolver] resolves [t], defaulting to 0.0.

    Equivalent to
    [maybe_resolve t context calc_resolver |> Option.value ~default:0.0]. *)

val resolve_rect_or_zero :
  float option ->
  (int -> float -> float) ->
  t Toffee_geometry.Rect.t ->
  Toffee_geometry.Rectf.t
(** [resolve_rect_or_zero context calc_resolver rect] is [rect] with
    {!resolve_or_zero} applied to each side, as padding and border resolve
    against the containing block's width. A rect of four zero lengths is
    {!Toffee_geometry.Rectf.zero} itself, allocating nothing. *)

(** {1 Utilities} *)

val uses_percentage : t -> bool
(** [uses_percentage t] returns [true] if [t] contains a percentage component.

    Returns [true] for percentage values and calc expressions. Returns [false]
    for absolute lengths. *)

val resolved_percentage_size : t -> float -> float option
(** [resolved_percentage_size t parent_size] resolves percentage to absolute
    size.

    Returns [Some (parent_size * value t)] if [t] is a percentage, [None]
    otherwise. Uses f32 precision. *)

val resolved_percentage_size_with_calc :
  t -> float -> (int -> float -> float) -> float option
(** [resolved_percentage_size_with_calc t parent_size calc_resolver] resolves
    percentage or calc to absolute size.

    Returns [Some size] for percentages and calc expressions, [None] for
    absolute lengths. *)

(** {1 Comparison and Formatting} *)

val equal : t -> t -> bool
(** [equal a b] tests equality of two values.

    Calc values are equal if their indices match. Non-calc values are equal if
    their tags and numeric values match within floating-point epsilon. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering.

    Calc values sort before non-calc values. Within each category, values are
    ordered by tag, then by numeric value. *)

val to_string : t -> string
(** [to_string t] formats [t] for debugging.

    Formats as "Npx" for lengths, "N%" for percentages, and "calc(#N)" for calc
    expressions. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats [t] using {!to_string}. *)
