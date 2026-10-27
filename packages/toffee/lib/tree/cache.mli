(** Layout computation result caching.

    During layout, parents often lay a child out several times under different
    constraints. A node's cache keeps the result of its final layout
    ([Perform_layout]) and of up to {!cache_size} measurements ([Compute_size]),
    so that asking again with the same inputs costs nothing. This is Taffy's
    cache, with more measurements.

    {1 Keys}

    An entry is keyed on its {!Layout_input.t}: on each axis, the known
    dimension when there is one and else the available space, and which of the
    two it is; the requested axis; the parent's width; and, when the node's
    style resolves a percentage or a [calc] height, minimum height or maximum
    height against it, the parent's height. A layout reads its parent's height
    through those properties alone, so a node without them keeps its results
    when only its parent's height changes. The final layout matches only the
    same key. A measurement matches the same key except for the requested axis.
    One computed for a single axis answers only queries for that axis, since it
    may hold garbage in the other, and one computed for both axes answers any.

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

val get : t -> Toffee_style.t -> Layout_input.t -> Layout_output.t option
(** [get t style inputs] is the result cached in [t] for [inputs], if any, where
    [style] is the node's style. *)

val store : t -> Toffee_style.t -> Layout_input.t -> Layout_output.t -> unit
(** [store t style inputs output] caches [output] as the result for [inputs] of
    a node whose style is [style]. *)

val clear : t -> clear_state
(** [clear t] removes all entries.

    @return [Cleared] if entries were present, [Already_empty] otherwise. *)

val forget_final_layout : t -> unit
(** [forget_final_layout t] makes [t]'s final layout answer no inputs, so that
    the next final layout of the node is computed. [t] keeps its measurements
    and is non-empty afterwards ({!is_empty}), even if it was empty. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] holds no entry. *)
