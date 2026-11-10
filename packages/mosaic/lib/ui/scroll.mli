(** Persistent scroll positions, resolved against the current viewport. *)

type t
(** The type for scroll state. Following and reveal targets remain symbolic
    across layout changes; resolving them never changes the value. *)

val at : ?x:int -> int -> t
(** [at ?x y] positions the viewport at column [x] (default [0]) and row [y].
    Negative coordinates become zero. *)

val follow : [ `Top | `Bottom | `Left | `Right ] -> t -> t
(** [follow edge t] follows [edge] as content changes, preserving the other
    axis. It replaces the affected axis's reveal target. *)

val reveal : ?column:int -> line:int -> t -> t
(** [reveal ?column ~line t] keeps the given zero-based content coordinates
    visible with the smallest movement from [t]'s declared position. The target
    remains active until replaced by a position or follow value. A reveal
    measures from the declared position, not from the rows an earlier reveal or
    a follow shows: to reveal from the rows shown, first position [t] at
    {!resolve}'s offset with {!with_position}. *)

val with_position : ?x:int -> ?y:int -> t -> t
(** [with_position ?x ?y t] replaces the specified axes with absolute positions,
    ending following or reveal on those axes. *)

val equal : t -> t -> bool
(** [equal a b] is whether [a] and [b] declare the same scroll state. *)

val resolve :
  t ->
  width:int ->
  height:int ->
  content_width:int ->
  content_height:int ->
  int * int
(** [resolve t ~width ~height ~content_width ~content_height] is the scroll
    offset clamped to the current content and viewport dimensions. *)
