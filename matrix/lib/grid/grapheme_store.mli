(** Grid-local storage for non-inline grapheme clusters.

    A store owns the UTF-8 payloads that cannot be represented directly in a
    packed grid cell. Stored payloads are addressed by a compact index plus a
    generation number. The pair [(idx, gen)] is a handle: [idx] selects a slot
    and [gen] prevents stale cells from observing a slot after it has been
    released and reused.

    This module is an internal storage primitive for {!Grid}. It does not model
    ownership in the returned index alone. Callers that install a stored
    grapheme in cells must balance {!incref} and {!decref} for the corresponding
    handle. *)

type t
(** The type for grapheme stores. *)

val create : unit -> t
(** [create ()] is a new empty grapheme store. *)

val clear : t -> unit
(** [clear t] removes all payloads from [t] and invalidates all existing
    handles. *)

val intern : t -> string -> off:int -> len:int -> int
(** [intern t s ~off ~len] is the store index for the byte slice
    [String.sub s off len].

    If an equal live payload already exists in [t], its index is returned.
    Otherwise a new slot is allocated with reference count [0]. The returned
    index is not a complete handle until paired with {!generation}.

    The caller must ensure [off] and [len] designate a valid slice of [s]. *)

val valid : t -> idx:int -> gen:int -> bool
(** [valid t ~idx ~gen] is [true] iff [(idx, gen)] currently names an allocated
    slot in [t]. *)

val generation : t -> int -> int
(** [generation t idx] is the current generation for [idx].

    The caller must ensure [idx] is a valid store index. *)

val incref : t -> idx:int -> gen:int -> unit
(** [incref t ~idx ~gen] records one additional owner of [(idx, gen)].

    If the handle is stale or invalid, this is a no-op. A transition from zero
    to one reference makes the payload visible to subsequent {!intern} calls. *)

val decref : t -> idx:int -> gen:int -> unit
(** [decref t ~idx ~gen] releases one owner of [(idx, gen)].

    If the handle is stale or invalid, this is a no-op. Releasing the last
    owner removes the payload from the live intern table and makes the slot
    reusable. Calls must be balanced with prior {!incref} calls. *)

val length : t -> idx:int -> gen:int -> int
(** [length t ~idx ~gen] is the UTF-8 byte length of [(idx, gen)], or [0] if the
    handle is stale or invalid. *)

val blit : t -> idx:int -> gen:int -> bytes -> pos:int -> int
(** [blit t ~idx ~gen dst ~pos] writes the UTF-8 bytes for [(idx, gen)] into
    [dst] starting at [pos] and returns the number of bytes written.

    Returns [0] if the handle is stale or invalid, or if [dst] does not have
    enough space from [pos].

    Raises [Invalid_argument] if [pos < 0] or [pos > Bytes.length dst]. *)

val to_string : t -> idx:int -> gen:int -> string
(** [to_string t ~idx ~gen] is the UTF-8 payload for [(idx, gen)], or [""] if
    the handle is stale or invalid. *)

val copy : src:t -> idx:int -> gen:int -> dst:t -> int option
(** [copy ~src ~idx ~gen ~dst] copies [(idx, gen)] from [src] into [dst].

    The result is [Some dst_idx] if the source handle is valid. [dst_idx] is a
    fresh index in [dst] with reference count [0]; callers must pair it with
    [generation dst dst_idx] and acquire ownership with {!incref}. The result is
    [None] if the source handle is stale or invalid. *)
