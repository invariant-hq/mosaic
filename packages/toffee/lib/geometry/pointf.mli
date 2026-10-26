(** Points of floats.

    A [t] holds two coordinates in floats, flat, as {!Sizef.t} holds a size:
    layout computes its locations as [t]s. {!Point} remains for points of other
    values. *)

type t = { x : float; y : float }
(** Two coordinates. *)

(** {1 Constants and creation} *)

val zero : t
(** [zero] is the origin. *)

val make : float -> float -> t
(** [make x y] is [{ x; y }]. *)

(** {1 Arithmetic} *)

val add : t -> t -> t
(** [add a b] adds [a] and [b] coordinate by coordinate. *)

val sub : t -> t -> t
(** [sub a b] subtracts [b] from [a] coordinate by coordinate. *)

val ( + ) : t -> t -> t
(** [a + b] is [add a b]. *)

val ( - ) : t -> t -> t
(** [a - b] is [sub a b]. *)

val mul_scalar : t -> float -> t
(** [mul_scalar point scalar] multiplies both coordinates by [scalar]. *)

val div_scalar : t -> float -> t
(** [div_scalar point scalar] divides both coordinates by [scalar]. *)

val ( * ) : t -> float -> t
(** [point * scalar] is [mul_scalar point scalar]. *)

val ( / ) : t -> float -> t
(** [point / scalar] is [div_scalar point scalar]. *)

(** {1 Axis accessors} *)

val get : Abstract_axis.t -> t -> float
(** [get axis point] is [x] for [Inline] and [y] for [Block]. *)

(** {1 Conversions} *)

val of_point : float Point.t -> t
(** [of_point point] has the coordinates of [point]. *)

val to_point : t -> float Point.t
(** [to_point point] has the coordinates of [point]. *)

(** {1 Comparison and formatting} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff both coordinates are equal floats. *)

val compare : t -> t -> int
(** [compare a b] orders by [x], then [y], as [Float.compare]. *)

val to_string : t -> string
(** [to_string point] is a readable representation of [point]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a point as {!to_string}. *)
