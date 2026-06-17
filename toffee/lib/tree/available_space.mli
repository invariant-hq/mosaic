(** Available space constraints for layout computation.

    Available space represents the amount of space available to a node in a
    given axis during CSS layout computation. It distinguishes between definite
    sizes (concrete pixel values) and intrinsic sizing constraints (min-content
    and max-content). See
    {{:https://www.w3.org/TR/css-sizing-3/#available}CSS Sizing Level 3}.

    Layout algorithms use these constraints to determine node dimensions.
    [Definite] specifies a fixed pixel count, [Min_content] instructs the node
    to shrink-wrap to its minimum intrinsic size, and [Max_content] instructs
    the node to expand to its maximum intrinsic size.

    {1 Constraint Propagation}

    Operations on available space follow consistent semantics for constraint
    propagation:

    - Arithmetic operations ([add], [sub]) apply only to [Definite] values;
      constraints propagate unchanged.
    - [min] operations convert constraints to [Definite] when combined with
      concrete values, as constraints represent unbounded space.
    - [max] operations preserve constraints, as they remain unbounded after
      comparison.
    - Optional variants ([min_or_self], [add_or_zero]) treat [None] as a no-op.
*)

type t =
  | Definite of float  (** Definite space of a specified number of pixels. *)
  | Min_content
      (** Indefinite space under min-content constraint. The node shrinks to its
          minimum intrinsic size. *)
  | Max_content
      (** Indefinite space under max-content constraint. The node expands to its
          maximum intrinsic size. *)

(** {1 Constants} *)

val zero : t
(** [zero] is [Definite 0.0]. *)

val max_content : t
(** [max_content] is [Max_content]. *)

val min_content : t
(** [min_content] is [Min_content]. *)

(** {1 Construction} *)

val of_length : float -> t
(** [of_length value] creates [Definite value]. *)

val of_float : float -> t
(** [of_float value] creates [Definite value]. Alias for {!of_length}. *)

val of_option : float option -> t
(** [of_option opt] converts an optional float to available space. [Some value]
    becomes [Definite value]; [None] becomes [Max_content]. *)

(** {1 Predicates} *)

val is_definite : t -> bool
(** [is_definite t] returns [true] if [t] is [Definite], else [false]. *)

(** {1 Conversion} *)

val to_option : t -> float option
(** [to_option t] converts to an option. [Definite value] becomes [Some value];
    constraints become [None]. *)

val unwrap_or : t -> float -> float
(** [unwrap_or t default] returns the definite value or [default] if indefinite.
*)

val unwrap : t -> float
(** [unwrap t] returns the definite value.

    Raises [Invalid_argument] if [t] is not [Definite]. *)

val unwrap_or_else : t -> (unit -> float) -> float
(** [unwrap_or_else t default_cb] returns the definite value or the result of
    [default_cb ()] if indefinite. *)

val to_string : t -> string
(** [to_string t] converts to a string representation. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats [t] for pretty-printing. *)

(** {1 Combination} *)

val or_ : t -> t -> t
(** [or_ t default] returns [t] if [Definite], else [default]. *)

val or_else : t -> (unit -> t) -> t
(** [or_else t default_cb] returns [t] if [Definite], else [default_cb ()]. *)

val set_or_self : t -> float option -> t
(** [set_or_self t value] returns [Definite v] if [value] is [Some v], else [t].
*)

(** {1 Transformation} *)

val map_definite_value : t -> (float -> float) -> t
(** [map_definite_value t f] applies [f] to the value if [Definite], else
    returns [t] unchanged. *)

(** {1 Layout Computation} *)

val compute_free_space : t -> float -> float
(** [compute_free_space t used_space] computes remaining free space after
    allocating [used_space].

    Returns [Float.infinity] for [Max_content] (unbounded expansion), [0.0] for
    [Min_content] (no free space for expansion), and [available -. used_space]
    for [Definite available]. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality. [Definite] values are equal if their absolute
    difference is less than [Float.epsilon]. Constraints match only their own
    variant. *)

val compare : t -> t -> int
(** [compare a b] orders values. [Definite] values sort before constraints;
    [Min_content] sorts before [Max_content]. *)

val is_roughly_equal : t -> t -> bool
(** [is_roughly_equal t other] tests approximate equality. Equivalent to
    {!equal}. *)

(** {1 Arithmetic Operations with Concrete Values}

    These operations combine available space with concrete float values.
    Operations on [Definite] values apply standard arithmetic. Constraint
    handling varies by operation as documented. *)

val min : t -> float -> t
(** [min t rhs] returns the minimum of [t] and [rhs]. [Definite] values use
    [Float.min]. Constraints become [Definite rhs], as unbounded space is
    greater than any finite value. *)

val max : t -> float -> t
(** [max t rhs] returns the maximum of [t] and [rhs]. [Definite] values use
    [Float.max]. Constraints remain unchanged, as they represent unbounded space
    greater than any finite value. *)

val clamp : t -> float -> float -> t
(** [clamp t min_val max_val] clamps [t] to the range [[min_val, max_val]].
    [Definite] values are clamped. Constraints remain unchanged. *)

val add : t -> float -> t
(** [add t rhs] adds [rhs] to [t]. [Definite] values are incremented.
    Constraints remain unchanged. *)

val sub : t -> float -> t
(** [sub t rhs] subtracts [rhs] from [t]. [Definite] values are decremented.
    Constraints remain unchanged. *)

(** {1 Arithmetic Operations with Optional Values}

    These operations combine available space with optional float values. When
    the optional value is [None], the operation returns [t] unchanged. *)

val min_or_self : t -> float option -> t
(** [min_or_self t rhs] returns the minimum of [t] and [rhs]. Returns [t]
    unchanged if [rhs] is [None]. Constraints become [Definite] when combined
    with [Some value]. *)

val max_or_self : t -> float option -> t
(** [max_or_self t rhs] returns the maximum of [t] and [rhs]. Returns [t]
    unchanged if [rhs] is [None]. Constraints remain unchanged when combined
    with [Some value]. *)

val clamp_or_self : t -> float option -> float option -> t
(** [clamp_or_self t min_val max_val] clamps [t] to the optional bounds. [None]
    bounds are ignored. [Definite] values are clamped; constraints remain
    unchanged. *)

val add_or_zero : t -> float option -> t
(** [add_or_zero t rhs] adds [rhs] to [t] if [Some], else returns [t] unchanged.
    [Definite] values are incremented; constraints remain unchanged. *)

val sub_or_zero : t -> float option -> t
(** [sub_or_zero t rhs] subtracts [rhs] from [t] if [Some], else returns [t]
    unchanged. [Definite] values are decremented; constraints remain unchanged.
*)

(** {1 Size Operations}

    Operations on pairs of available space values for width and height. *)

type size = { width : t; height : t }
(** A size with width and height constraints. *)

val size_to_options : size -> float option Geometry.size
(** [size_to_options t] converts to a size with optional components. [Definite]
    values become [Some]; constraints become [None]. *)

val size_maybe_set : size -> float option Geometry.size -> size
(** [size_maybe_set t value] updates dimensions where [value] has [Some]. [None]
    values leave the corresponding dimension unchanged. *)
