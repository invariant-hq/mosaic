(** Persistent ordered maps that a view shows with [Mosaic.keyed].

    A value of type [('k, 'v) t] binds keys of type ['k] to values of type ['v],
    in the {e order} of the [compare] function the map was made with. A map made
    from another by {!add} or {!remove} shares all but a logarithmic number of
    its nodes with it. [Mosaic.keyed] compares the map a view declares with the
    one it declared last, skipping what the two share, so a view that shows a
    collection with [keyed] costs a render what changed in the collection.

    An application keeps such a map in its model and changes it with {!add} and
    {!remove}. A key names one item for the item's life: a log keys its lines by
    a counter it keeps in its model, which no removal reuses, and a map keyed by
    position would give an item's key to another item when one is removed.
    Unless a function's doc says otherwise, a function takes time logarithmic in
    the number of bindings.

    This keeps a log of the last 1,000 lines, oldest first:
    {[
    type model = { lines : (int, string) Keyed.t; next : int }

    let init = { lines = Keyed.empty ~compare:Int.compare; next = 0 }

    let append m line =
      let lines = Keyed.add m.next line m.lines in
      let lines =
        if Keyed.cardinal lines > 1_000 then Keyed.remove (m.next - 1_000) lines
        else lines
      in
      { lines; next = m.next + 1 }
    ]} *)

type ('k, 'v) t
(** The type for maps from ['k] to ['v]. *)

val empty : compare:('k -> 'k -> int) -> ('k, 'v) t
(** [empty ~compare] is the map with no binding, ordered by [compare]. Every map
    made from it has its order. [compare] must be a total order. *)

val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
(** [add k v m] is [m] with [k] bound to [v], replacing [k]'s binding if any. It
    is [m] itself when [k] is bound to [v] already, physically. *)

val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
(** [remove k m] is [m] without [k]'s binding. It is [m] itself when [k] is not
    bound. *)

val find_opt : 'k -> ('k, 'v) t -> 'v option
(** [find_opt k m] is [k]'s value in [m], if any. *)

val cardinal : ('k, 'v) t -> int
(** [cardinal m] is the number of bindings of [m], in constant time. *)

val min_binding_opt : ('k, 'v) t -> ('k * 'v) option
(** [min_binding_opt m] is the first binding of [m] in its order, if any. *)

val max_binding_opt : ('k, 'v) t -> ('k * 'v) option
(** [max_binding_opt m] is the last binding of [m] in its order, if any. *)

val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
(** [fold f m acc] applies [f] to the bindings of [m] in order, threading [acc],
    in time linear in their number. *)

val of_list : compare:('k -> 'k -> int) -> ('k * 'v) list -> ('k, 'v) t
(** [of_list ~compare l] is the map of the bindings of [l], ordered by
    [compare]. Of two bindings of one key, the later one stays, as with {!add}.
*)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
(** [iter f m] applies [f] to the bindings of [m] in order, in time linear in
    their number. *)

val find_map : ('v -> 'a option) -> ('k, 'v) t -> 'a option
(** [find_map f m] is the first [Some] that [f] gives on the values of [m], in
    order, if any. *)

(** {1:positions Positions}

    A binding's {e position} is the number of bindings before it in the map's
    order. Each map holds the size of its subtrees, so reading by position takes
    the logarithmic time of a lookup, as a list view does to show a page of a
    long log. *)

val nth : ('k, 'v) t -> int -> 'k * 'v
(** [nth m i] is the binding at position [i] of [m].

    Raises [Invalid_argument] if [i] is not in [0] to [cardinal m - 1]. *)

val rank : ('k, 'v) t -> 'k -> int
(** [rank m k] is the number of keys of [m] before [k] in its order: [k]'s
    position when it is bound, else the position it would have. *)

val fold_range :
  ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> int -> int -> 'acc -> 'acc
(** [fold_range f m lo hi acc] folds [f] over the bindings at positions [lo] to
    [hi - 1], in order, in time logarithmic in the size of [m] and linear in
    [hi - lo]. Positions outside [m] are none. *)

(** {1:changes Changes} *)

val order : ('k, 'v) t -> 'k -> 'k -> int
(** [order m] is the [compare] function [m] was made with, which every map made
    from [m] keeps. [empty ~compare:(order m)] is an empty map in [m]'s order.
    Two maps have one order when their orders are the same function, physically.
*)

(** The type for the change of a key from one map to another. *)
type ('a, 'b) change =
  | Removed of 'a  (** Bound in the first map only, to this value. *)
  | Added of 'b  (** Bound in the second map only, to this value. *)
  | Changed of 'a * 'b
      (** Bound in both, to these values, which differ physically. *)

val symmetric_diff :
  ('k, 'a) t ->
  ('k, 'b) t ->
  ('k -> ('a, 'b) change -> 'acc -> 'acc) ->
  'acc ->
  'acc
(** [symmetric_diff a b f acc] folds [f] over the keys whose bindings differ
    from [a] to [b], in order: those bound in one map only, and those bound in
    both to values that differ physically. It skips the subtrees the two maps
    share, so when [b] was made from [a] by a few {!add}s and {!remove}s its
    time follows them, not the size of the maps. This is how [Mosaic.keyed]
    finds what changed in a map a view declares again.

    Raises [Invalid_argument] if [a] and [b] do not have one {!order}. *)
