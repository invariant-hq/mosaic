(** Final computed layout for a single node.

    Layout represents the output of a layout algorithm, containing all spatial
    and box model information needed to render a node. This includes the node's
    position, dimensions, spacing properties (border, padding, margin), and
    rendering order.

    {1 Box Model}

    CSS layout uses a nested box model where each node has multiple boxes:

    - Margin box: outermost, transparent spacing around the border
    - Border box: contains the border, padding, and content
    - Padding box: spacing between border and content
    - Content box: innermost, holds the actual content

    The [size] field represents the border box dimensions. Use
    [content_box_size] to compute the content box dimensions. The [content_size]
    field may exceed [size] when content overflows, which is important for
    computing scroll dimensions.

    {1 Coordinate System}

    The [location] field specifies the top-left corner of the node's margin box
    relative to its parent's content box origin. This is the primary positioning
    coordinate. To compute other box positions, use [content_box_x] and
    [content_box_y] which add the appropriate spacing offsets. *)

(** {1 Type} *)

type t = {
  order : int;
      (** Rendering order for layering. Nodes with higher order values render on
          top of nodes with lower values. This implements a topological sort of
          the tree. *)
  location : Toffee_geometry.Pointf.t;
      (** Top-left corner of the margin box relative to the parent's content
          box. *)
  size : Toffee_geometry.Sizef.t;
      (** Width and height of the border box (includes padding and border, but
          not margin). *)
  content_size : Toffee_geometry.Sizef.t;
      (** Width and height of the content inside the node. May exceed [size]
          when content overflows, useful for computing scroll dimensions. *)
  scrollbar_size : Toffee_geometry.Sizef.t;
      (** Size of scrollbars in each dimension. Zero when no scrollbar is
          present. *)
  border : Toffee_geometry.Rectf.t;  (** Border widths on each edge. *)
  padding : Toffee_geometry.Rectf.t;  (** Padding on each edge. *)
  margin : Toffee_geometry.Rectf.t;  (** Margin on each edge. *)
}
(** [t] represents the complete layout state for a node after layout
    computation. *)

(** {1 Construction} *)

val make :
  order:int ->
  location:Toffee_geometry.Pointf.t ->
  size:Toffee_geometry.Sizef.t ->
  content_size:Toffee_geometry.Sizef.t ->
  scrollbar_size:Toffee_geometry.Sizef.t ->
  border:Toffee_geometry.Rectf.t ->
  padding:Toffee_geometry.Rectf.t ->
  margin:Toffee_geometry.Rectf.t ->
  t
(** [make ~order ~location ~size ~content_size ~scrollbar_size ~border ~padding
     ~margin] creates a layout with all fields specified. *)

val default : t
(** [default] is a zero layout with all dimensions and spacing set to zero and
    order set to 0. *)

val with_order : int -> t
(** [with_order order] creates a zero layout with the specified [order] value.
    All other fields are set to zero. *)

(** {1 Field Accessors} *)

val order : t -> int
(** [order layout] returns the rendering order. *)

val location : t -> Toffee_geometry.Pointf.t
(** [location layout] returns the top-left corner position. *)

val size : t -> Toffee_geometry.Sizef.t
(** [size layout] returns the border box dimensions. *)

val content_size : t -> Toffee_geometry.Sizef.t
(** [content_size layout] returns the content dimensions. *)

val scrollbar_size : t -> Toffee_geometry.Sizef.t
(** [scrollbar_size layout] returns the scrollbar dimensions. *)

val border : t -> Toffee_geometry.Rectf.t
(** [border layout] returns the border widths. *)

val padding : t -> Toffee_geometry.Rectf.t
(** [padding layout] returns the padding. *)

val margin : t -> Toffee_geometry.Rectf.t
(** [margin layout] returns the margin. *)

(** {1 Content Box Dimensions} *)

val content_box_width : t -> float
(** [content_box_width layout] computes the width of the content box.

    This is the border box width minus horizontal padding and border:
    [size.width - padding.left - padding.right - border.left - border.right]. *)

val content_box_height : t -> float
(** [content_box_height layout] computes the height of the content box.

    This is the border box height minus vertical padding and border:
    [size.height - padding.top - padding.bottom - border.top - border.bottom].
*)

val content_box_size : t -> Toffee_geometry.Sizef.t
(** [content_box_size layout] computes the content box dimensions.

    Equivalent to
    [{width = content_box_width layout; height = content_box_height layout}]. *)

val content_box_x : t -> float
(** [content_box_x layout] computes the x offset of the content box relative to
    the parent's content box.

    This is [location.x + margin.left + border.left + padding.left]. *)

val content_box_y : t -> float
(** [content_box_y layout] computes the y offset of the content box relative to
    the parent's content box.

    This is [location.y + margin.top + border.top + padding.top]. *)

(** {1 Scroll Dimensions} *)

val scroll_width : t -> float
(** [scroll_width layout] computes the horizontal scroll extent.

    Returns the amount by which content overflows horizontally, accounting for
    scrollbar width. Result is always non-negative. Returns [0.0] when content
    fits within the border box.

    Formula:
    [max 0.0 (content_size.width + min scrollbar_size.width size.width -
     size.width + border.right)]. *)

val scroll_height : t -> float
(** [scroll_height layout] computes the vertical scroll extent.

    Returns the amount by which content overflows vertically, accounting for
    scrollbar height. Result is always non-negative. Returns [0.0] when content
    fits within the border box.

    Formula:
    [max 0.0 (content_size.height + min scrollbar_size.height size.height -
     size.height + border.bottom)]. *)

(** {1 Comparison and Formatting} *)

val compare : t -> t -> int
(** [compare a b] compares layouts lexicographically.

    Fields are compared in order: order, location, size, content_size,
    scrollbar_size, border, padding, margin. *)

val equal : t -> t -> bool
(** [equal a b] tests whether two layouts are structurally equal. *)

val to_string : t -> string
(** [to_string layout] converts [layout] to a string representation showing
    order, location, size, and content_size. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt layout] formats [layout] for printing. *)
