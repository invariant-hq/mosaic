(** The reconciler that [Mosaic.Reconciler] exports, where its contract is
    documented. It reconciles the nodes of {!Vnode}: on each {!render} it
    flattens each parent's children, matches elements to the previous children's
    fibers, reuses or creates their widgets, destroys the unmatched fibers and
    places the children. *)

open Mosaic_ui

type t

val create : container:Renderable.t -> t
val container : t -> Renderable.t

val render : t -> viewport_width:int -> unit Vnode.t -> unit
(** Raises [Invalid_argument] if [viewport_width] is not positive. *)

val unmount : t -> unit

val focused : t -> Renderable.t option
(** Raises [Invalid_argument] if several shown elements that are not inert
    declare focus. *)

val modal : t -> Renderable.t option
val request_close : t -> bool
