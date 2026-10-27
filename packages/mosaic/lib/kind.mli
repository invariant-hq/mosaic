(** The kinds of elements, behind [Mosaic.Element], where their contract is
    documented. A kind's [id] proves two elements of one kind have one props,
    event and widget type. *)

open Mosaic_ui

type ('props, 'event, 'w) t = private {
  id : ('props * 'event * 'w) Type.Id.t;
  name : string;
  create : parent:Renderable.t -> layout:Toffee.Style.t -> 'props -> 'w;
  node : 'w -> Renderable.t;
  layout : 'w -> Toffee.Style.t -> unit;
  update : 'w -> 'props -> unit;
  equal : 'props -> 'props -> bool;
  focusable : bool;
  events : ('w -> ('event -> unit) -> unit) option;
  handler : ('w -> ('event -> unit) option -> unit) option;
}

val make :
  name:string ->
  create:(parent:Renderable.t -> layout:Toffee.Style.t -> 'props -> 'w) ->
  node:('w -> Renderable.t) ->
  ?layout:('w -> Toffee.Style.t -> unit) ->
  update:('w -> 'props -> unit) ->
  equal:('props -> 'props -> bool) ->
  ?focusable:bool ->
  ?events:('w -> ('event -> unit) -> unit) ->
  ?handler:('w -> ('event -> unit) option -> unit) ->
  unit ->
  ('props, 'event, 'w) t
(** [make ...] is [Mosaic.Element.kind]. Without [layout], a widget's layout is
    set with [Renderable.set_style] on its node. *)

val name : ('props, 'event, 'w) t -> string

val same : ('p, 'e, 'w) t -> ('q, 'f, 'v) t -> bool
(** [same a b] is [true] iff [a] and [b] are one kind. *)

val no_handler : 'event -> 'msg option
(** [no_handler] is the handler of an element that declares none, one value
    shared by all of them, so that a kind's [handler] hook can tell it. *)
