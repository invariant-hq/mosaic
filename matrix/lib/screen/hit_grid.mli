(** Spatial indexing for UI mouse interactions.

    A Hit Grid maps screen coordinates to integer IDs. It allows O(1) lookup to
    determine which UI element sits at a specific (x, y) coordinate.

    {1 Constants} *)

val empty_id : int
(** [empty_id] (0) represents the absence of any element. *)

(** {1 Types} *)

type rect = { x : int; y : int; width : int; height : int }
(** Rectangular area in cell coordinates. *)

type t
(** The hit grid.

    Internally backed by a specialized [int32] buffer for cache locality. *)

(** {1 Lifecycle} *)

val create : width:int -> height:int -> t
(** [create ~width ~height] creates a new grid initialized to {!empty_id}. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] updates the grid dimensions.

    **Behavior**: This operation invalidates the grid content. All cells are
    reset to {!empty_id}. Internal storage is grown only if necessary,
    minimizing allocation churn. *)

val clear : t -> unit
(** [clear t] resets all cells in the current bounds to {!empty_id}. The clip
    stack is preserved. *)

(** {1 Operations} *)

val add : t -> x:int -> y:int -> width:int -> height:int -> id:int -> unit
(** [add t ~x ~y ~width ~height ~id] fills a rectangular region with [id].

    - Overwrites any existing IDs in that region (painter's algorithm).
    - Automatically clips the rectangle to the grid boundaries and the active
      clip region.
    - Zero or negative dimensions result in a no-op. *)

val get : t -> x:int -> y:int -> int
(** [get t ~x ~y] returns the ID at the specified coordinates.

    Returns {!empty_id} if the coordinates are out of bounds. *)

val blit : src:t -> dst:t -> unit
(** [blit ~src ~dst] copies the content of [src] to [dst].

    [dst] is automatically resized to match [src]. *)

(** {1 Clipping}

    Hierarchical clipping for hit regions. When a clip is active, {!add}
    operations are constrained to the intersection of the clip and the grid
    bounds. This prevents elements inside overflow-hidden containers from
    receiving mouse events outside their visible area.

    Push/pop pairs must be balanced. *)

val push_clip : t -> rect -> unit
(** Pushes a clipping region. The effective clip is the intersection of the new
    region with the current clip (hierarchical narrowing). *)

val pop_clip : t -> unit
(** Pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** Removes all clipping regions. *)

val with_clip : t -> rect -> (unit -> 'a) -> 'a
(** [with_clip t rect f] runs [f ()] with [rect] as the active clip, popping it
    on return (even on exception). *)
