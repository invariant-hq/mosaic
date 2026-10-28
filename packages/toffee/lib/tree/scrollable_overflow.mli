(** A box's scrollable overflow area.

    The rectangle a box's content reaches, relative to the box's padding-box
    origin (its inner border edge), with the direction of its scroll origin in
    each axis. In a non-reversed axis the scroll origin is at the start (left or
    top) and only the end edge is reachable by scrolling; in a reversed axis (a
    [row-reverse] flex container's horizontal axis, for one) the origin is at
    the end and the content reaches before it.

    The rectangle always contains the origin. A scroll container's area is
    clipped to what scrolling can reach; another box's area is all of its
    content's overflow, which it propagates to its parent. *)

type t
(** A scrollable overflow area. *)

val zero : t
(** [zero] is the empty area at the origin, with no reversed axis. *)

val make : Toffee_geometry.Rectf.t -> reversed_x:bool -> reversed_y:bool -> t
(** [make rect ~reversed_x ~reversed_y] is the area [rect], which must contain
    the origin, with the given scroll origin directions. *)

val rect : t -> Toffee_geometry.Rectf.t
(** [rect t] is the rectangle of [t]. *)

val packed : t -> Toffee_geometry.Rectf.t
(** [packed t] is the rectangle of [t] with its right and bottom edges negated
    in its reversed axes, the form [t] is stored in: equal bit for bit exactly
    when the areas are. *)

val left : t -> float
(** [left t] is the left edge of [t]: zero or less. *)

val right : t -> float
(** [right t] is the right edge of [t]: zero or more. *)

val top : t -> float
(** [top t] is the top edge of [t]: zero or less. *)

val bottom : t -> float
(** [bottom t] is the bottom edge of [t]: zero or more. *)

val reversed_x : t -> bool
(** [reversed_x t] is [true] if the horizontal scroll origin of [t] is at the
    right. *)

val reversed_y : t -> bool
(** [reversed_y t] is [true] if the vertical scroll origin of [t] is at the
    bottom. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same area with the same scroll
    origins. *)

val compare : t -> t -> int
(** [compare a b] orders areas by their rectangles, then their scroll origins.
*)

val to_string : t -> string
(** [to_string t] is a human-readable form of [t]. *)
