(** The lists a widget displays, held as an application's list and a function
    from its elements.

    A widget's options or rows can come from a list of the application's own
    values, such as the label of each item. A {e source} holds that list and the
    function without applying it. Two sources are {e the same} when they hold
    the same list and the same function, physically, and then have the same
    elements. A widget compares a declaration's source with the previous
    declaration's in constant time when the two are the same, and converts it
    only when they are not. *)

type 'b t
(** The type for sources of ['b] elements. *)

val of_list : 'b list -> 'b t
(** [of_list l] is the source of the elements of [l]. *)

val map : ('a -> 'b) -> 'a list -> 'b t
(** [map f l] is the source of the elements of [List.map f l]. [f] is applied
    when the elements are read, and must return equal elements for one element
    of [l] every time. *)

val same : 'b t -> 'b t -> bool
(** [same s s'] is [true] iff [s] and [s'] are the same. *)

val equal : ('b -> 'b -> bool) -> 'b t -> 'b t -> bool
(** [equal eq s s'] is [true] iff [s] and [s'] are the same or have as many
    elements, pairwise equal under [eq]. *)

val length : 'b t -> int
(** [length s] is the number of elements of [s]. *)

val to_array : 'b t -> 'b array
(** [to_array s] is the elements of [s]. *)

val to_list : 'b t -> 'b list
(** [to_list s] is the elements of [s]. *)
