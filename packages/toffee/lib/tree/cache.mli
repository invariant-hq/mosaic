(** Layout computation result caching.

    During layout, parents often lay a child out several times under different
    constraints. A node's cache keeps the result of its final layout
    ([Perform_layout]) and of up to {!cache_size} measurements ([Compute_size]),
    so that asking again with the same inputs costs nothing. This is Taffy's
    cache, with more measurements.

    {1 Keys}

    An entry is keyed on its {!Layout_input.t}: on each axis, the known
    dimension when there is one and else the available space; the parent size;
    and the requested axis. The final layout matches only the same key. A
    measurement matches the same known dimensions or available space and the
    same parent width, and it answers a query for either axis only if it was
    computed for both axes, since a measurement for one axis may hold garbage in
    the other.

    The sizing mode and the collapsibility of the vertical margins are not in
    the key: they change for a node only when its parent's algorithm or its own
    style does. A cache records the one mode its entries were computed with,
    misses for any other, and drops its entries when a result computed with
    another mode is stored.

    {1 Eviction}

    Measurements are found by their full key, not by a fixed slot. When all
    {!cache_size} are full, a new one replaces the first that was not used since
    the eviction cursor last passed it (second chance, or CLOCK). Measurements
    that carry margin-collapse metadata are not cached, as a hit gives back only
    the size. Hidden layouts ([Perform_hidden_layout]) are never cached. *)

val cache_size : int
(** [cache_size] is the number of measurements a cache keeps (24). *)

type t
(** A node's layout cache. *)

val make : unit -> t
(** [make ()] is an empty cache. *)

(** Result of a clear operation. *)
type clear_state =
  | Cleared  (** Some entries were removed. *)
  | Already_empty  (** Cache was already empty. *)

val get : t -> Layout_input.t -> Layout_output.t option
(** [get t inputs] is the cached result for [inputs], if any. *)

val store : t -> Layout_input.t -> Layout_output.t -> unit
(** [store t inputs output] caches [output] as the result for [inputs]. *)

val clear : t -> clear_state
(** [clear t] removes all entries.

    @return [Cleared] if entries were present, [Already_empty] otherwise. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] holds no entry. *)
