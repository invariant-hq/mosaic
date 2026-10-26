(** The cells of one grid that hold each stored grapheme, counted so that the
    grid holds one reference of the {!Grapheme_store}.

    A tracker counts, for each grapheme of the store, the cells of its grid that
    hold it, the start and continuation cells of a grapheme counting together.
    It adds a reference to the store when a grapheme's count rises from 0 and
    takes it back when the count falls to 0, so the store is touched once per
    grapheme and grid, whatever the number of cells. *)

type t
(** The type for trackers. *)

val create : Grapheme_store.t -> t
(** [create store] is a tracker of no cell for [store]. When the tracker is
    collected, the release of its references is queued on [store]
    ({!Grapheme_store.defer_release}). *)

val add : t -> Packed_cell.t -> unit
(** [add t id] counts one more cell that holds [id], and does nothing if [id]
    holds its scalar inline. A grapheme counted for the first time gains a
    reference in the store. *)

val remove : t -> Packed_cell.t -> unit
(** [remove t id] counts one cell fewer that holds [id], and does nothing if
    [id] holds its scalar inline or is not counted. A grapheme whose count falls
    to 0 loses its reference in the store. *)

val replace : t -> old_id:Packed_cell.t -> new_id:Packed_cell.t -> unit
(** [replace t ~old_id ~new_id] is [add t new_id] followed by [remove t old_id],
    so a grapheme that both name keeps its reference. It does nothing if
    [old_id] and [new_id] are equal. *)

val clear : t -> unit
(** [clear t] takes back the store reference of every grapheme [t] counts, and
    counts none afterwards. A grid calls it when it clears all its cells. *)

type counts
(** The counts a tracker held before a recount. *)

val start_recount : t -> counts
(** [start_recount t] has [t] count no cell, keeping the store references of the
    counts it held, which it returns. The cells are then counted again with
    {!add}, and the old references given back with {!finish_recount}, so that a
    grapheme both counts keep its reference. A grid recounts its cells when it
    resizes, with no new tracker to drop. *)

val finish_recount : t -> counts -> unit
(** [finish_recount t old] gives back the store references of [old], the counts
    {!start_recount} returned. *)

val unique_count : t -> int
(** [unique_count t] is the number of graphemes [t] counts. It reads one field.
*)
