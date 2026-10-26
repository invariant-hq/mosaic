(** 2-dimensional point representation.

    Concrete points of floats are {!Pointf.t}, which store their floats flat. *)

type 'a t = { x : 'a; y : 'a }
(** ['a t] represents a 2-dimensional point with [x] and [y] coordinates. *)

(** {1 Constants} *)

val none : 'a option t
(** [none] is a point with both coordinates set to [None]. *)

(** {1 Creation} *)

val make : 'a -> 'a -> 'a t
(** [make x y] creates a point. *)

(** {1 Mapping} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f point] applies [f] to both coordinates. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f p1 p2] applies [f] to corresponding coordinates of [p1] and [p2]. *)

(** {1 Axis accessors} *)

val get : Abstract_axis.t -> 'a t -> 'a
(** [get axis point] retrieves the component corresponding to [axis]. *)

val set : Abstract_axis.t -> 'a -> 'a t -> 'a t
(** [set axis value point] returns a new point with the component corresponding
    to [axis] set to [value]. *)

(** {1 Transformations} *)

val transpose : 'a t -> 'a t
(** [transpose point] swaps the x and y components. *)

(** {1 Conversions} *)

val to_size : 'a t -> 'a Size.t
(** [to_size point] converts a point to a size by mapping x -> width and y ->
    height. *)

val of_size : 'a Size.t -> 'a t
(** [of_size size] converts a size to a point by mapping width -> x and height
    -> y. *)

(** {1 Comparison and string functions} *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** [compare cmp a b] compares [a] and [b] lexicographically using [cmp]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq a b] tests equality using [eq] for each coordinate. *)

val to_string : ('a -> string) -> 'a t -> string
(** [to_string f point] converts [point] to a string using [f]. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt point] prints [point] using [fmt] and [f]. *)
