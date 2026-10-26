(** Axis-aligned rectangles representing margins, padding, borders, or insets.

    A rectangle is defined by four edges: left, right, top, and bottom. Each
    edge value can represent coordinates, spacing, or offsets depending on
    context.

    In CSS layout, rectangles primarily represent spacing properties like
    padding and margin, where each edge specifies the amount of space on that
    side. *)

(** {1 Type} *)

type 'a t = { left : 'a; right : 'a; top : 'a; bottom : 'a }
(** ['a t] represents a rectangle with four edges.

    The type parameter ['a] allows rectangles of different value types:
    - [float option t] for optional measurements
    - [Dimension.t t] for CSS dimension values
    - [Length_percentage.t t] for length or percentage values

    Concrete rectangles of floats are {!Rectf.t}, which store their floats flat.
*)

(** {1 Constants} *)

val all : 'a -> 'a t
(** [all value] creates a rectangle with all edges set to [value]. *)

(** {1 Creation} *)

val make : left:'a -> right:'a -> top:'a -> bottom:'a -> 'a t
(** [make ~left ~right ~top ~bottom] creates a rectangle. *)

(** {1 Mapping Functions} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f rect] applies [f] to each edge of [rect]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f r1 r2] applies [f] to corresponding edges of [r1] and [r2]. *)

val zip_size : 'b Size.t -> ('a -> 'b -> 'c) -> 'a t -> 'c t
(** [zip_size size f rect] applies [f] to each edge with its corresponding
    dimension.

    For left and right edges, [f] receives [size.width] as the second argument.
    For top and bottom edges, [f] receives [size.height] as the second argument.
*)

(** {1 Component Extraction} *)

val horizontal_components : 'a t -> 'a Line.t
(** [horizontal_components rect] extracts the left and right edges as a line. *)

val vertical_components : 'a t -> 'a Line.t
(** [vertical_components rect] extracts the top and bottom edges as a line. *)

(** {1 Comparison and String Functions} *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp a b] lexicographically compares [a] and [b] using [cmp].

    Edges are compared in order: left, right, top, bottom. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq a b] tests if [a] and [b] are equal using [eq] for each edge. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string f rect] converts [rect] to a string representation using [f] for
    each edge. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt rect] prints [rect] to [fmt] using [f] for each edge. *)
