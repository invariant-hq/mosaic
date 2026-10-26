(** Sizes of floats.

    A [t] is a width and a height in floats. Unlike a [float Size.t], whose
    floats are boxed, a [t] stores both flat, so building one allocates a single
    small block and reading one allocates nothing. Layout computes its concrete
    sizes as [t]s; {!Size} remains for sizes of other values, such as optional
    floats or dimensions. *)

type t = { width : float; height : float }
(** A width and a height. *)

(** {1 Constants and creation} *)

val zero : t
(** [zero] has a width and height of [0.0]. *)

val make : float -> float -> t
(** [make width height] is [{ width; height }]. *)

val square : float -> t
(** [square side] has a width and height of [side]. *)

(** {1 Arithmetic} *)

val add : t -> t -> t
(** [add a b] adds [a] and [b] axis by axis. *)

val sub : t -> t -> t
(** [sub a b] subtracts [b] from [a] axis by axis. *)

val ( + ) : t -> t -> t
(** [a + b] is [add a b]. *)

val ( - ) : t -> t -> t
(** [a - b] is [sub a b]. *)

val mul_scalar : t -> float -> t
(** [mul_scalar size scalar] multiplies both axes by [scalar]. *)

val div_scalar : t -> float -> t
(** [div_scalar size scalar] divides both axes by [scalar]. *)

val ( * ) : t -> float -> t
(** [size * scalar] is [mul_scalar size scalar]. *)

val ( / ) : t -> float -> t
(** [size / scalar] is [div_scalar size scalar]. *)

val max : t -> t -> t
(** [max a b] is the larger of [a] and [b] on each axis, as [Stdlib.max]. *)

val min : t -> t -> t
(** [min a b] is the smaller of [a] and [b] on each axis, as [Stdlib.min]. *)

val has_non_zero_area : t -> bool
(** [has_non_zero_area size] is [true] iff both axes are greater than [0.0]. *)

(** {1 Axis accessors} *)

val get : Abstract_axis.t -> t -> float
(** [get axis size] is the width for [Inline] and the height for [Block]. *)

val set : Abstract_axis.t -> float -> t -> t
(** [set axis value size] is [size] with the width ([Inline]) or height
    ([Block]) set to [value]. *)

val get_absolute : Absolute_axis.t -> t -> float
(** [get_absolute axis size] is the width for [Horizontal] and the height for
    [Vertical]. *)

(** {1 Conversions} *)

val of_size : float Size.t -> t
(** [of_size size] has the width and height of [size]. *)

val to_size : t -> float Size.t
(** [to_size size] has the width and height of [size]. *)

val some : t -> float option Size.t
(** [some size] is [size] with [Some] on each axis. *)

(** {1 With optional sizes}

    These implement Taffy's MaybeMath between a concrete size and an optional
    one, where [None] stands for an absent constraint. *)

val unwrap_or : t -> float option Size.t -> t
(** [unwrap_or alt size] is [size] on the axes where it is [Some], and [alt]
    elsewhere. *)

val clamp : float option Size.t -> float option Size.t -> t -> t
(** [clamp min max size] clamps each axis of [size] between the bounds that are
    [Some]; the minimum wins over the maximum. *)

val min_or_self : float option Size.t -> t -> t
(** [min_or_self optional size] is the smaller of [size] and [optional] on the
    axes where [optional] is [Some], and [size] elsewhere. *)

val max_or_self : float option Size.t -> t -> t
(** [max_or_self optional size] is the larger of [size] and [optional] on the
    axes where [optional] is [Some], and [size] elsewhere. *)

val add_or_zero : float option Size.t -> t -> t
(** [add_or_zero optional size] adds [optional] to [size], [None] as zero. *)

val sub_or_zero : float option Size.t -> t -> t
(** [sub_or_zero optional size] subtracts [optional] from [size], [None] as
    zero. *)

val min_with_option : t -> float option Size.t -> float option Size.t
(** [min_with_option size optional] is [Some] of the smaller of [size] and
    [optional] on each axis, of [size] where [optional] is [None]. *)

val max_with_option : t -> float option Size.t -> float option Size.t
(** [max_with_option size optional] is [Some] of the larger of [size] and
    [optional] on each axis, of [size] where [optional] is [None]. *)

val add_with_option : t -> float option Size.t -> float option Size.t
(** [add_with_option size optional] is [Some] of [size] plus [optional] on each
    axis, of [size] where [optional] is [None]. *)

val sub_with_option : t -> float option Size.t -> float option Size.t
(** [sub_with_option size optional] is [Some] of [size] minus [optional] on each
    axis, of [size] where [optional] is [None]. *)

val maybe_add : t -> float option Size.t -> float option Size.t
(** [maybe_add size opt] adds [size] to the axes of [opt] that are [Some]. *)

val maybe_sub : t -> float option Size.t -> float option Size.t
(** [maybe_sub size opt] subtracts [size] from the axes of [opt] that are
    [Some]. *)

val maybe_min : t -> float option Size.t -> float option Size.t
(** [maybe_min size opt] is the smaller of [size] and the axes of [opt] that are
    [Some]. *)

val maybe_max : t -> float option Size.t -> float option Size.t
(** [maybe_max size opt] is the larger of [size] and the axes of [opt] that are
    [Some]. *)

(** {1 Comparison and formatting} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff both axes are equal floats. *)

val compare : t -> t -> int
(** [compare a b] orders by width, then height, as [Float.compare]. *)

val to_string : t -> string
(** [to_string size] is a readable representation of [size]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a size as {!to_string}. *)
