(** Text selections with anchor and focus points.

    A selection spans from an {!anchor} to a {!focus} point in 0-based terminal
    coordinates. The {e anchor} is the fixed starting point of the selection
    gesture; the {e focus} is the current endpoint. Selections support dynamic
    anchor positioning via a callback, useful for scrollable content where the
    anchor moves as the viewport scrolls.

    See also {!type-t} for the mutable selection type and {!create} for
    construction. *)

(** {1:points Points and bounds} *)

type point = { x : int; y : int }
(** The type for 2D coordinates in 0-based column and row indices. *)

val pp_point : Format.formatter -> point -> unit
(** [pp_point ppf p] formats [p] on [ppf] as [(x, y)]. *)

val equal_point : point -> point -> bool
(** [equal_point a b] is [true] iff [a] and [b] have the same coordinates. *)

type bounds = { x : int; y : int; width : int; height : int }
(** The type for bounding rectangles in cell coordinates.

    The rectangle covers all cells satisfying [x <= col < x + width] and
    [y <= row < y + height]. Both the anchor and focus cells are included in the
    selection, so a single-cell selection has [width = 1] and [height = 1]. *)

val pp_bounds : Format.formatter -> bounds -> unit
(** [pp_bounds ppf b] formats [b] on [ppf] as [{x=...; y=...; w=...; h=...}]. *)

val equal_bounds : bounds -> bounds -> bool
(** [equal_bounds a b] is [true] iff [a] and [b] describe the same rectangle. *)

type local_bounds = { anchor : point; focus : point }
(** The type for selection endpoints in local coordinates, after transformation
    via {!to_local}. *)

val pp_local_bounds : Format.formatter -> local_bounds -> unit
(** [pp_local_bounds ppf lb] formats [lb] on [ppf] as [{anchor=...; focus=...}].
*)

val equal_local_bounds : local_bounds -> local_bounds -> bool
(** [equal_local_bounds a b] is [true] iff both endpoints match. *)

(** {1:selections Selections} *)

type t
(** The type for mutable text selections. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats a human-readable representation of [t] on [ppf]. *)

val create :
  ?anchor_position:(unit -> point) -> anchor:point -> focus:point -> unit -> t
(** [create ?anchor_position ~anchor ~focus ()] is a new active selection from
    [anchor] to [focus].

    The selection is created with {!is_active}, {!is_dragging}, and {!is_start}
    all [true].

    [anchor_position] defaults to a static function returning [anchor]. When
    provided, the anchor is recomputed by calling it on each access to
    {!anchor}, which is useful for selections in scrollable content.

    See also {!set_anchor} and {!set_focus}. *)

(** {2:position Position} *)

val anchor : t -> point
(** [anchor t] is the anchor point of [t], i.e. where the selection gesture
    started. If an [anchor_position] callback was provided at {!create}, it is
    called on each access to obtain the current position. *)

val focus : t -> point
(** [focus t] is the focus point of [t], i.e. the current endpoint of the
    selection. *)

val set_anchor : t -> point -> unit
(** [set_anchor t p] sets the anchor of [t] to [p]. Any [anchor_position]
    callback provided at {!create} is replaced by a static value. *)

val set_focus : t -> point -> unit
(** [set_focus t p] sets the focus of [t] to [p]. *)

val bounds : t -> bounds
(** [bounds t] is the bounding rectangle enclosing [t].

    Both the anchor and focus cells are included. A selection from [(x0, y0)] to
    [(x1, y1)] produces [width = |x1 - x0| + 1] and [height = |y1 - y0| + 1].

    See also {!type-bounds}. *)

(** {2:state State} *)

val is_active : t -> bool
(** [is_active t] is [true] iff [t] should be rendered. *)

val set_is_active : t -> bool -> unit
(** [set_is_active t v] shows or hides [t] depending on [v]. *)

val is_dragging : t -> bool
(** [is_dragging t] is [true] iff the user is actively dragging to extend [t].
*)

val set_is_dragging : t -> bool -> unit
(** [set_is_dragging t v] sets the dragging state of [t] to [v]. *)

val is_start : t -> bool
(** [is_start t] is [true] iff [t] is on the first frame of a new selection.
    Consumers use this to distinguish starting a new selection from extending an
    existing one. *)

val set_is_start : t -> bool -> unit
(** [set_is_start t v] sets the start flag of [t] to [v]. *)

(** {1:converting Converting} *)

val to_local : t -> origin:point -> local_bounds
(** [to_local t ~origin] is [t]'s anchor and focus in coordinates relative to
    [origin].

    See also {!type-local_bounds}. *)
