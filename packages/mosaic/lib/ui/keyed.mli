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

(**/**)

module Private : sig
  val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
  (** [iter f m] applies [f] to the bindings of [m] in order. *)

  val nth : ('k, 'v) t -> int -> 'k * 'v
  (** [nth m i] is the binding at position [i] of [m], in its order.

      Raises [Invalid_argument] if [i] is not a position of [m]. *)

  type ('a, 'b) change = Removed of 'a | Added of 'b | Changed of 'a * 'b

  val empty_like : ('k, 'v) t -> ('k, 'w) t
  (** [empty_like m] is the map with no binding in [m]'s order. *)

  val same_order : ('k, 'a) t -> ('k, 'b) t -> bool
  (** [same_order a b] is [true] iff [a] and [b] were made with the same
      [compare] function, physically. *)

  val diff :
    ('k, 'a) t ->
    ('k, 'b) t ->
    ('k -> ('a, 'b) change -> 'acc -> 'acc) ->
    'acc ->
    'acc
  (** [diff a b f acc] folds [f] over the keys bound in one map and not the
      other, and those whose values differ physically, in order. It skips the
      subtrees the two maps share, so its time follows what changed from [a] to
      [b].

      Raises [Invalid_argument] if [a] and [b] were made with different
      [compare] functions. *)

  val find_map : ('v -> 'a option) -> ('k, 'v) t -> 'a option
  (** [find_map f m] is the first [Some] [f] gives on [m]'s values in order. *)
end
