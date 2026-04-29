(** Spatial indexing for mouse hit testing.

    A hit grid maps screen coordinates to integer element IDs. Lookup is [O(1)];
    registration is [O(region area)].

    {1:constants Constants} *)

val empty_id : int
(** [empty_id] is [0]. Represents the absence of any element. *)

type id = int
(** The type for hit-test element identifiers. [0] is reserved for {!empty_id};
    real element identifiers should be positive. *)

(** {1:types Types} *)

type rect = { x : int; y : int; width : int; height : int }
(** The type for rectangular areas in cell coordinates. *)

type t
(** The type for hit grids. Lookups are [O(1)]; adding a region is
    [O(width * height)] in the clipped region. *)

(** {1:lifecycle Lifecycle} *)

val create : width:int -> height:int -> t
(** [create ~width ~height] is a hit grid of the given dimensions with all cells
    set to {!empty_id}. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] updates [t]'s dimensions to [width] and [height].
    All cells are reset to {!empty_id}. Internal storage is grown only when
    necessary. *)

val clear : t -> unit
(** [clear t] resets all cells to {!empty_id}. The clip stack is preserved. *)

(** {1:ops Operations} *)

val add : t -> x:int -> y:int -> width:int -> height:int -> id:id -> unit
(** [add t ~x ~y ~width ~height ~id] fills the rectangular region with [id]
    (painter's algorithm: overwrites any existing IDs). The rectangle is clipped
    to the grid bounds and the active clip region. Zero or negative dimensions
    are a no-op. *)

val get : t -> x:int -> y:int -> id
(** [get t ~x ~y] is the element ID at [(x, y)], or {!empty_id} if the
    coordinates are out of bounds. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies the content of [src] into [dst]. [dst] is resized to
    match [src]. *)

(** {1:clipping Clipping}

    Hierarchical clipping for hit regions. When a clip is active, {!add}
    operations are constrained to the intersection of the clip rectangle and the
    grid bounds. This prevents elements inside overflow-hidden containers from
    receiving mouse events outside their visible area.

    Push/pop pairs must be balanced. *)

val push_clip : t -> rect -> unit
(** [push_clip t r] pushes a clipping rectangle. The effective clip is the
    intersection of [r] with the current clip (hierarchical narrowing). *)

val pop_clip : t -> unit
(** [pop_clip t] pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** [clear_clip t] removes all clipping regions. *)

val with_clip : t -> rect -> (unit -> 'a) -> 'a
(** [with_clip t r f] runs [f ()] with [r] as the active clip and pops it on
    return (even on exception). *)
