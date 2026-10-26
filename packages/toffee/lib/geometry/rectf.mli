(** Rectangles of floats.

    A [t] holds four edges in floats, flat, as {!Sizef.t} holds a size: layout
    computes its padding, borders and margins as [t]s. {!Rect} remains for
    rectangles of other values, such as lengths or optional floats. *)

type t = { left : float; right : float; top : float; bottom : float }
(** Four edges. *)

(** {1 Constants and creation} *)

val zero : t
(** [zero] has every edge [0.0]. *)

val make : left:float -> right:float -> top:float -> bottom:float -> t
(** [make ~left ~right ~top ~bottom] is [{ left; right; top; bottom }]. *)

val all : float -> t
(** [all value] has every edge [value]. *)

(** {1 Arithmetic} *)

val add : t -> t -> t
(** [add a b] adds [a] and [b] edge by edge. *)

val ( + ) : t -> t -> t
(** [a + b] is [add a b]. *)

val horizontal_axis_sum : t -> float
(** [horizontal_axis_sum rect] is [rect.left +. rect.right]. *)

val vertical_axis_sum : t -> float
(** [vertical_axis_sum rect] is [rect.top +. rect.bottom]. *)

val sum_axes : t -> Sizef.t
(** [sum_axes rect] is the horizontal sum as the width and the vertical sum as
    the height. *)

val grid_axis_sum : Absolute_axis.t -> t -> float
(** [grid_axis_sum axis rect] is the horizontal sum for [Horizontal] and the
    vertical sum for [Vertical]. *)

(** {1 Conversions} *)

val of_rect : float Rect.t -> t
(** [of_rect rect] has the edges of [rect]. *)

val to_rect : t -> float Rect.t
(** [to_rect rect] has the edges of [rect]. *)

(** {1 Comparison and formatting} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff every edge is an equal float. *)

val compare : t -> t -> int
(** [compare a b] orders by left, right, top, then bottom, as [Float.compare].
*)

val to_string : t -> string
(** [to_string rect] is a readable representation of [rect]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a rectangle as {!to_string}. *)
