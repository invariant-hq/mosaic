(** Sequences of the items a collection widget shows.

    A value of type ['a t] holds a collection's {e items} in order, the item at
    {e position} [i] for each [i] from [0] to [length - 1]. It holds the
    application's own list, array or function from positions, and copies none of
    them, so making one costs a constant whatever the number of items.

    Reading costs follow the data. On items made by {!of_array} or {!make},
    {!get} and {!length} cost a constant. On items made by {!of_list}, {!get}
    costs the position and {!length} the number of items. A widget reads the
    items it draws and the items a key moves over. A widget that reads a list by
    position converts it to an array once and keeps the array while the items
    stay the same.

    Two sequences are {e the same} when they hold the same list, array or
    function, physically ({!same}). Sequences that are the same have the same
    items, provided nobody mutated the array in between. A widget compares a
    declaration's items with the previous declaration's in constant time when
    they are the same, and only then does it skip reading them. So items an
    application keeps in its model and declares again cost nothing, and a list
    that [view] builds again, even an equal one, is new. *)

type 'a t
(** The type for sequences of ['a] items. *)

val of_list : 'a list -> 'a t
(** [of_list l] is the items of [l]. *)

val of_array : 'a array -> 'a t
(** [of_array a] is the items of [a]. [a] must not be mutated while a widget
    shows it: the widget would not see the change, since the items stay the
    same. *)

val make : length:int -> (int -> 'a) -> 'a t
(** [make ~length f] is the [length] items [f 0], ..., [f (length - 1)]. [f] is
    called when an item is read, and must return equal items for one position
    every time.

    Raises [Invalid_argument] if [length] is negative. *)

val length : 'a t -> int
(** [length s] is the number of items of [s]. *)

val get : 'a t -> int -> 'a
(** [get s i] is the item of [s] at position [i].

    Raises [Invalid_argument] if [i] is not a position of [s]. *)

val same : 'a t -> 'a t -> bool
(** [same s s'] is [true] iff [s] and [s'] are the same. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq s s'] is [true] iff [s] and [s'] are the same, or have as many
    items and the items at each position are equal under [eq]. *)

val to_array : 'a t -> 'a array
(** [to_array s] is a new array of the items of [s]. *)

val to_list : 'a t -> 'a list
(** [to_list s] is the items of [s]. *)

(**/**)

(* For the widgets that hold lists of their own items: select, tab_select and
   table. *)

val map_list : ('a -> 'b) -> 'a list -> 'b t
(** [map_list f l] is the items of [List.map f l], with [f] applied when an item
    is read. [f] must return equal items for one element of [l] every time.
    [map_list f l] and [map_list f' l'] are the same when [f] and [f'], and [l]
    and [l'], are physically equal. *)

(**/**)
