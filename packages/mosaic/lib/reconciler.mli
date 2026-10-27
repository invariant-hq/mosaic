(** The reconciler that [Mosaic.Reconciler] exports, where its contract is
    documented. It reconciles the nodes of {!Vnode}: on each {!render} it walks
    each parent's children, matches elements to the previous children's fibers,
    reuses or creates their widgets, destroys the unmatched fibers and places
    the children. *)

open Mosaic_ui

type 'msg t

val create : container:Renderable.t -> deliver:('msg -> unit) -> 'msg t
val container : 'msg t -> Renderable.t

val render : 'msg t -> viewport_width:int -> 'msg Vnode.t -> unit
(** Raises [Invalid_argument] if [viewport_width] is not positive, or if a
    memo's key holds a function, an object or an abstract value. *)

val unmount : 'msg t -> unit

val focused : 'msg t -> Renderable.t option
(** Raises [Invalid_argument] if several shown elements that are not inert
    declare focus. *)

val modal : 'msg t -> Renderable.t option
val request_close : 'msg t -> bool
