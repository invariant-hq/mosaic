(** Reconciler for diffing and patching {!Mosaic_ui.Vnode.t} trees.

    The reconciler bridges the declarative {!Mosaic_ui.Vnode.t} tree produced by
    a [view] function and the imperative {!Mosaic_ui.Renderable.t} tree. On each
    {!val-render} call it flattens each parent's children, resolving
    {!Mosaic_ui.Vnode.Viewport_switch} nodes from the explicit current viewport
    width, then matches elements to the previous children's fibers by key
    (preferred) then by position, reuses or creates renderable nodes, destroys
    unmatched fibers, and commits child placement. {!Mosaic_ui.Vnode.Embed}
    nodes are attached but bypass the reconciler lifecycle.

    A memo ({!Mosaic_ui.Vnode.val-memo}) owns the children its view produced.
    When the memo at the same position among its parent's memos is equal and the
    viewport width is unchanged, its children are kept without being visited, so
    an unchanged memo costs one comparison.

    Fiber tracking and instance management are internal; this module exposes the
    container, reconciliation, and declared focus. *)

open Mosaic_ui

(** {1:types Types} *)

type t
(** The type for a mutable reconciler bound to a container node. Tracks the
    current fiber tree between {!val-render} calls. *)

(** {1:constructors Constructors} *)

val create : container:Renderable.t -> t
(** [create ~container] is a reconciler that manages children of [container].
    The reconciler starts with an empty fiber tree; existing children of
    [container] are not adopted. *)

(** {1:accessors Accessors} *)

val container : t -> Renderable.t
(** [container r] is the container node of [r]. *)

(** {1:reconciliation Reconciliation} *)

val render : t -> viewport_width:int -> unit Vnode.t -> unit
(** [render r ~viewport_width vnode] reconciles [vnode] against the previous
    fiber tree, applying minimal mutations to the renderable tree rooted at
    [container r]. [viewport_width] is the current terminal width in cells and
    resolves {!Mosaic_ui.Vnode.Viewport_switch} nodes before their selected
    branch is reconciled or laid out.

    Fibers unmatched in the new tree are destroyed and their renderable nodes
    removed. Requests a render on [container r] after reconciliation completes.

    Raises [Invalid_argument] if [viewport_width] is not positive. *)

val unmount : t -> unit
(** [unmount r] destroys all fibers and detaches all embedded nodes, leaving
    [container r] empty. Requests a render on [container r] after cleanup. [r]
    may be reused after unmounting. *)

val focused : t -> Renderable.t option
(** [focused t] is the element declared focused that is visible and not inert,
    or [None] if none is. An element is inert in an inert subtree and, while a
    modal ({!Mosaic_ui.Vnode.val-modal}) is shown, outside it. Reconciliation
    does not apply focus: callers synchronize this result with
    {!Mosaic_ui.Renderer.val-focus} or {!Mosaic_ui.Renderer.val-blur} after
    {!render}.

    Raises [Invalid_argument] if several such elements declare focus. *)

val modal : t -> Renderable.t option
(** [modal t] is the node of the shown modal, the last in tree order, that no
    inert subtree holds, if any. Callers give it to
    {!Mosaic_ui.Renderer.set_modal} after {!render}. *)

val request_close : t -> bool
(** [request_close t] invokes the [on_close] of the modal {!modal} is, and is
    [true] iff it has one. *)
