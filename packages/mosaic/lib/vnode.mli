(** The tree of nodes a view is, behind [Mosaic.t].

    [Mosaic] builds these nodes, and the reconciler ([Reconciler]) mounts and
    updates widgets from them. The rules a view follows are [Mosaic]'s (its
    section "UI elements"); this module states what they leave unsaid about the
    representation.

    An element that declares no attribute or handler shares one record of each
    with every such element, and the reconciler compares an element's
    {!type-attrs} and props physically before it compares them by value. *)

open Mosaic_ui

(** {1:handlers Handlers} *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg option) option;
  on_key : (Event.key -> 'msg option) option;
  on_paste : (Event.paste -> 'msg option) option;
  on_click : (Event.mouse -> 'msg option) option;
  on_focus_request : (unit -> 'msg) option;
  on_size : (w:int -> h:int -> 'msg option) option;
}
(** The type for generic terminal event handlers, parameterised by the message
    type. Raw handlers return [None] to ignore an event. [on_focus_request] is
    invoked lazily on a pointer focus request. A widget's own events, such as a
    slider's value changes, go to the element's handler ([on]). *)

(** {1:attrs Attributes} *)

type attrs = {
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  focused : bool;
  buffered : bool;
  live : bool;
}
(** The type for non-handler element attributes. Grouped for sharing and
    physical-equality fast paths in the reconciler.

    [focused] declares application-owned focus and implies [focusable]. At most
    one visible element that is not inert may declare focus. The application
    runtime synchronizes this declaration with the renderer after
    reconciliation. *)

(** {1:vnodes Virtual nodes} *)

type 'msg memo
(** The type for a deferred subtree: a view function and the key it is applied
    to. See {!val-memo}. *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Viewport_switch of 'msg viewport_switch
  | Memo of 'msg memo
  | Embed of Renderable.t
  | Themed of (Theme.t -> Theme.t) * 'msg t
  | Map : ('a -> 'msg) * 'a t -> 'msg t
  | Keyed : ('k, 'v) Keyed.t * ('k -> 'v -> 'msg t) -> 'msg t
  | Empty
      (** The type for a virtual node in the UI tree.

          - [Element] is a widget of a kind, with its props, attributes,
            handlers and children.
          - [Fragment] groups multiple vnodes without a wrapper element.
          - [Viewport_switch] selects one branch from the current terminal
            viewport width before reconciliation and layout.
          - [Embed] wraps an existing {!Renderable.t} node. The node is attached
            but not managed by the reconciler. Use this as an escape hatch for
            imperative code.
          - [Themed] draws a vnode with a theme ({!val-themed}).
          - [Map] gives the messages of its subtree to its function
            ({!val-map}).
          - [Keyed] is the item views of a map's bindings, in its order
            ({!val-keyed}).
          - [Empty] renders nothing. *)

and 'msg element =
  | E : {
      kind : ('props, 'event, 'w) Kind.t;
      props : 'props;
      on : 'event -> 'msg option;
          (** The element's handler: what each event of its widget means. *)
      key : string option;
      attrs : attrs;
      handlers : 'msg handlers;
      children : 'msg t list;
    }
      -> 'msg element
      (** The type for an element vnode: a widget of a kind ([Mosaic.Element])
          with its props and handler, an optional reconciliation key,
          {!type-attrs}, generic {!type-handlers}, and children. *)

and 'msg viewport_switch = {
  at_least_width : int;
      (** Inclusive viewport-width threshold, in terminal cells. *)
  wide : 'msg t;  (** Branch selected at or above [at_least_width]. *)
  narrow : 'msg t;  (** Branch selected below [at_least_width]. *)
}
(** The type for a viewport-conditioned branch. Exactly one branch is reconciled
    at a time. *)

(** {1:constructors Constructors} *)

val empty : 'msg t
val fragment : 'msg t list -> 'msg t
val embed : Renderable.t -> 'msg t
val themed : (Theme.t -> Theme.t) -> 'msg t -> 'msg t

val memo : key:'key -> ('key -> 'msg t) -> 'msg t
(** [memo ~key view] is [Mosaic.memo]. It does not look into [key]. *)

val check_memo : 'msg memo -> unit
(** [check_memo m] walks all of [m]'s key. The reconciler calls it when [m]
    mounts.

    Raises [Invalid_argument] if the key contains a function, an object or an
    abstract value. *)

val memo_equal : 'a memo -> 'a memo -> bool
(** [memo_equal old m] is [true] iff [old] and [m] are equal in the sense of
    [Mosaic.memo], where [old]'s key has been checked. It walks every part of
    [m]'s key that is not physically the part of [old]'s at the same place,
    whatever the result.

    Raises [Invalid_argument] if such a part contains a function, an object or
    an abstract value. *)

val memo_view : 'msg memo -> 'msg t
(** [memo_view m] calls [m]'s view function on its key. *)

val viewport_switch :
  at_least_width:int -> wide:'msg t -> narrow:'msg t -> 'msg t
(** [viewport_switch ~at_least_width ~wide ~narrow] is [Mosaic.viewport_switch].

    Raises [Invalid_argument] if [at_least_width] is not positive. *)

val element :
  ?key:string ->
  style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  kind:('props, 'event, 'w) Kind.t ->
  ?on:('event -> 'msg option) ->
  'props ->
  'msg t list ->
  'msg t
(** [element ~style ~kind props children] is an element of [kind] with [props],
    laid out by [style]. It is focusable when its kind's elements are or when it
    has [on_focus_request]. [on] defaults to [Kind.no_handler]. The other
    attributes take [Mosaic]'s defaults. *)

(** {1:transform Transformation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f vnode] is [Map (f, vnode)], [Mosaic.map].

    The reconciler keeps a route cell at a map's place and stores the map's
    function in it on each render. A handler's message goes up the routes of its
    fiber, through each cell's current function, when the handler fires. It
    erases message types to do so, and the erasure is sound under the types of
    [Mosaic]. Kept fibers meet a map function of another inner type in three
    places only:
    - under an equal memo, whose view function is physically the one that built
      them. If the inner type differs, the view is polymorphic in its message
      type, so by parametricity its messages come from its key. Keys are equal
      by representation and must be immutable data ([Mosaic.memo]), and
      evaluation is deterministic and ignores types, so the kept messages are
      indistinguishable from those the new render would give;
    - under a physically equal child list, which at two message types must be
      generalizable, and a list holding a handler cannot be;
    - under a map whose place is kept while its subtree changes, where the
      subtree is reconciled again and every fiber takes the new handlers. A
      fiber never moves to another list, so its route is fixed for its life. *)

val keyed : ('k, 'v) Keyed.t -> ('k -> 'v -> 'msg t) -> 'msg t
(** [keyed m item] is [Keyed (m, item)], [Mosaic.keyed]. The reconciler keeps
    one span per binding and the map it declared last, and compares that map
    with the next one ([Keyed.Private.diff]). The erasure of the keys' and
    values' types is sound as [map]'s is: a kept binding's view is reused only
    while its value and [item] are physically those it was built from. *)
