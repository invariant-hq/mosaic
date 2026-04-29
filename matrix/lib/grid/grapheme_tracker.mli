(** Buffer-local reference counting for grapheme clusters.

    This module implements a "write-combining" strategy for reference counting.
    Instead of touching the grapheme store for every cell assignment in a grid,
    this tracker maintains a local count of usage within a specific grid buffer.

    Store refcounts are only updated when a grapheme's local count transitions
    between 0 and 1. This keeps store operations proportional to the number of
    distinct graphemes rather than the number of cells. *)

type t
(** The abstract state of a tracker. *)

val create : Grapheme_store.t -> t
(** [create store] initializes a new tracker associated with [store]. *)

val add : t -> Packed_cell.t -> unit
(** [add t id] increments the local reference count for the given encoded
    character [id].

    If [id] represents a simple scalar (not an interned pointer), this is a
    no-op. If [id] is a complex grapheme seeing its first local reference, the
    store reference count is incremented.

    Start and continuation cells of the same grapheme share a single entry,
    keyed by a stable store key, so wide graphemes only bump the store refcount
    once per grid. *)

val remove : t -> Packed_cell.t -> unit
(** [remove t id] decrements the local reference count for the given encoded
    character [id].

    If [id] represents a simple scalar, this is a no-op. If [id] is a complex
    grapheme and its local count reaches zero, the store reference count is
    decremented. *)

val replace : t -> old_id:Packed_cell.t -> new_id:Packed_cell.t -> unit
(** [replace t ~old_id ~new_id] effectively performs [remove t old_id] followed
    by [add t new_id].

    This is optimized to avoid unnecessary hash table operations when [old_id]
    and [new_id] are identical or both are simple scalars. *)

val clear : t -> unit
(** [clear t] releases all references tracked by this instance.

    Decrements the global refcount for every unique interned grapheme currently
    tracked, then resets the local state. This should be called when clearing a
    Grid or before destroying the tracker. *)

val unique_count : t -> int
(** Returns the number of unique interned graphemes currently tracked.

    This is O(1) because the tracker maintains the count alongside the hash
    table, so it can be queried cheaply in hot paths. Useful for statistics and
    debugging. *)
