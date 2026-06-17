(** 2-dimensional size representation.

    A generic container representing width and height extents. The type is
    polymorphic over the contained value type, enabling concrete sizes
    ([float t]), optional sizes ([float option t]), and dimension-typed sizes.

    All operations preserve type safety and handle axis-specific transformations
    through abstract and absolute axis types. *)

type 'a t = { width : 'a; height : 'a }
(** ['a t] represents a 2-dimensional size with width and height of type ['a].
*)

(** {1 Constants} *)

val zero : float t
(** [zero] is a size with both width and height set to [0.0]. *)

val none : 'a option t
(** [none] is a size with both width and height set to [None]. *)

(** {1 Creation} *)

val make : 'a -> 'a -> 'a t
(** [make width height] creates a size. *)

val square : 'a -> 'a t
(** [square side] creates a size with equal width and height. *)

(** {1 Arithmetic Operations} *)

val add : float t -> float t -> float t
(** [add a b] adds corresponding dimensions of [a] and [b]. *)

val sub : float t -> float t -> float t
(** [sub a b] subtracts corresponding dimensions of [b] from [a]. *)

val ( + ) : float t -> float t -> float t
(** [a + b] is [add a b]. *)

val ( - ) : float t -> float t -> float t
(** [a - b] is [sub a b]. *)

val mul_scalar : float t -> float -> float t
(** [mul_scalar size scalar] multiplies both width and height by [scalar]. *)

val div_scalar : float t -> float -> float t
(** [div_scalar size scalar] divides both width and height by [scalar]. *)

val ( * ) : float t -> float -> float t
(** [size * scalar] is [mul_scalar size scalar]. *)

val ( / ) : float t -> float -> float t
(** [size / scalar] is [div_scalar size scalar]. *)

(** {1 Mapping Functions} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f size] applies [f] to both width and height, transforming ['a t] into
    ['b t]. *)

val map_width : ('a -> 'a) -> 'a t -> 'a t
(** [map_width f size] applies [f] to the width while preserving the height. *)

val map_height : ('a -> 'a) -> 'a t -> 'a t
(** [map_height f size] applies [f] to the height while preserving the width. *)

val zip_map : 'b t -> ('a -> 'b -> 'c) -> 'a t -> 'c t
(** [zip_map other f size] applies [f] pairwise to corresponding dimensions of
    [size] and [other]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f s1 s2] is [zip_map s2 f s1]. *)

(** {1 Axis Accessors} *)

val get : Abstract_axis.t -> 'a t -> 'a
(** [get axis size] retrieves the dimension corresponding to [axis].

    Returns width for [Abstract_axis.Inline] and height for
    [Abstract_axis.Block]. *)

val set : Abstract_axis.t -> 'a -> 'a t -> 'a t
(** [set axis value size] returns a new size with the dimension corresponding to
    [axis] set to [value].

    Sets width for [Abstract_axis.Inline] and height for [Abstract_axis.Block].
*)

val get_absolute : Absolute_axis.t -> 'a t -> 'a
(** [get_absolute axis size] retrieves the dimension corresponding to [axis].

    Returns width for [Absolute_axis.Horizontal] and height for
    [Absolute_axis.Vertical]. *)

(** {1 Float-Specific Operations} *)

val max : float t -> float t -> float t
(** [max a b] computes the component-wise maximum of [a] and [b]. *)

val min : float t -> float t -> float t
(** [min a b] computes the component-wise minimum of [a] and [b]. *)

val has_non_zero_area : float t -> bool
(** [has_non_zero_area size] returns [true] if both width and height are
    strictly greater than [0.0]. *)

val equal : float t -> float t -> bool
(** [equal a b] tests structural equality of [a] and [b] using float equality.
*)

(** {1 Option-Specific Operations} *)

val unwrap_or : 'a t -> 'a option t -> 'a t
(** [unwrap_or alt size] unwraps each dimension of [size], using the
    corresponding dimension from [alt] when [None]. *)

val choose_first : 'a option t -> 'a option t -> 'a option t
(** [choose_first size alt] selects dimensions from [size] when [Some], falling
    back to [alt] when [None]. *)

val both_axis_defined : 'a option t -> bool
(** [both_axis_defined size] returns [true] if both width and height are [Some].
*)

val apply_aspect_ratio : float option -> float option t -> float option t
(** [apply_aspect_ratio aspect_ratio size] applies [aspect_ratio] to compute the
    missing dimension.

    If [aspect_ratio] is [Some ratio]:
    - When width is [Some] but height is [None], computes height as
      [width / ratio]
    - When height is [Some] but width is [None], computes width as
      [height * ratio]
    - Otherwise returns [size] unchanged

    If [aspect_ratio] is [None], returns [size] unchanged. *)

(** {1 Comparison and Formatting} *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp a b] performs lexicographic comparison using [cmp], comparing
    width first, then height. *)

val equal_with : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal_with eq a b] tests equality using [eq] for both width and height. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string f size] converts [size] to a string representation using [f] for
    each dimension. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt size] formats [size] using [fmt] and [f] for each dimension. *)

(** {1 Option Operations}

    These operations implement Taffy's MaybeMath patterns for working with
    optional values. They follow specific semantics where [None] represents the
    absence of a constraint. *)

(** {2 Option → Option Operations} *)

val min_option : float option t -> float option t -> float option t
(** [min_option a b] computes the component-wise minimum of optional sizes.

    For each dimension:
    - [Some l, Some r] produces [Some (min l r)]
    - [Some _, None] produces the left value
    - [None, _] produces [None] *)

val max_option : float option t -> float option t -> float option t
(** [max_option a b] computes the component-wise maximum of optional sizes.

    For each dimension:
    - [Some l, Some r] produces [Some (max l r)]
    - [Some _, None] produces the left value
    - [None, _] produces [None] *)

val add_option : float option t -> float option t -> float option t
(** [add_option a b] adds optional sizes component-wise.

    For each dimension:
    - [Some l, Some r] produces [Some (l + r)]
    - [Some _, None] produces the left value
    - [None, Some _] produces [None] *)

val sub_option : float option t -> float option t -> float option t
(** [sub_option a b] subtracts optional sizes component-wise.

    For each dimension:
    - [Some l, Some r] produces [Some (l - r)]
    - [Some _, None] produces the left value
    - [None, _] produces [None] *)

val clamp_option :
  float option t -> float option t -> float option t -> float option t
(** [clamp_option min max size] clamps each dimension of [size] between optional
    bounds.

    For each dimension, returns:
    - [Some (clamp base min_val max_val)] when all three are [Some]
    - [Some (clamp base _ max_val)] when only max is [Some]
    - [Some (clamp base min_val _)] when only min is [Some]
    - The original value when both bounds are [None]
    - [None] when [size] is [None] *)

(** {2 Concrete + Option → Option Operations} *)

val min_with_option : float t -> float option t -> float option t
(** [min_with_option concrete optional] computes the minimum of a concrete size
    with an optional size.

    For each dimension, returns [Some (min concrete opt)] when optional is
    [Some opt], otherwise [Some concrete]. *)

val max_with_option : float t -> float option t -> float option t
(** [max_with_option concrete optional] computes the maximum of a concrete size
    with an optional size.

    For each dimension, returns [Some (max concrete opt)] when optional is
    [Some opt], otherwise [Some concrete]. *)

val add_with_option : float t -> float option t -> float option t
(** [add_with_option concrete optional] adds a concrete size to an optional
    size.

    For each dimension, returns [Some (concrete + opt)] when optional is
    [Some opt], otherwise [Some concrete]. *)

val sub_with_option : float t -> float option t -> float option t
(** [sub_with_option concrete optional] subtracts an optional size from a
    concrete size.

    For each dimension, returns [Some (concrete - opt)] when optional is
    [Some opt], otherwise [Some concrete]. *)

(** {2 Concrete Operations with Optional Parameters} *)

val clamp : float option t -> float option t -> float t -> float t
(** [clamp min max size] clamps [size] between optional bounds, returning a
    concrete size.

    For each dimension:
    - When both bounds are [Some], returns [min max_val (max min_val size)]
    - When only max is [Some], returns [min max_val size]
    - When only min is [Some], returns [max min_val size]
    - When both are [None], returns [size] unchanged *)

val min_or_self : float option t -> float t -> float t
(** [min_or_self optional size] computes the minimum with an optional size,
    falling back to [size].

    For each dimension, returns [min size val_] when optional is [Some val_],
    otherwise [size]. *)

val max_or_self : float option t -> float t -> float t
(** [max_or_self optional size] computes the maximum with an optional size,
    falling back to [size].

    For each dimension, returns [max size val_] when optional is [Some val_],
    otherwise [size]. *)

val add_or_zero : float option t -> float t -> float t
(** [add_or_zero optional size] adds an optional size to a concrete size,
    treating [None] as zero.

    For each dimension, returns [size + val_] when optional is [Some val_],
    otherwise [size]. *)

val sub_or_zero : float option t -> float t -> float t
(** [sub_or_zero optional size] subtracts an optional size from a concrete size,
    treating [None] as zero.

    For each dimension, returns [size - val_] when optional is [Some val_],
    otherwise [size]. *)

val max_concrete : float t -> float t -> float t
(** [max_concrete a b] computes the component-wise maximum using [Float.max]. *)

(** {2 Option + Concrete → Option Operations} *)

val maybe_add : float t -> float option t -> float option t
(** [maybe_add concrete opt_size] adds a concrete size to an optional size.

    For each dimension, maps [Some v] to [Some (v + concrete)], and [None] to
    [None]. *)

val maybe_sub : float t -> float option t -> float option t
(** [maybe_sub concrete opt_size] subtracts a concrete size from an optional
    size.

    For each dimension, maps [Some v] to [Some (v - concrete)], and [None] to
    [None]. *)

val maybe_min : float t -> float option t -> float option t
(** [maybe_min concrete opt_size] computes the minimum of an optional size with
    a concrete size.

    For each dimension, maps [Some v] to [Some (min v concrete)], and [None] to
    [None]. *)

val maybe_max : float t -> float option t -> float option t
(** [maybe_max concrete opt_size] computes the maximum of an optional size with
    a concrete size.

    For each dimension, maps [Some v] to [Some (max v concrete)], and [None] to
    [None]. *)
