(** Scrollable container with viewport clipping and scroll bars.

    A scroll box manages a content area larger than its visible viewport.
    Internally it builds a multi-node hierarchy:

    - {e root} -- flex-row container holding wrapper and the vertical bar.
    - {e wrapper} -- flex-column holding viewport and the horizontal bar.
    - {e viewport} -- overflow-hidden node that clips content.
    - {e content} -- translated node holding user children.

    Children are routed to the content node via {!Renderable.set_child_target}.
    Scrolling translates content without triggering relayout. *)

(** {1:scroll_accel Scroll acceleration} *)

module Scroll_accel : sig
  type t
  (** The type for scroll acceleration strategies.

      {b Note.} Values are stateful: they track timing internally. *)

  val linear : unit -> t
  (** [linear ()] is an acceleration strategy that always returns a [1.0]
      multiplier. No acceleration is applied. *)

  val macos : ?a:float -> ?tau:float -> ?max_multiplier:float -> unit -> t
  (** [macos ()] is a macOS-style exponential acceleration strategy with:
      - [a] exponential coefficient. Defaults to [0.8].
      - [tau] time constant in seconds. Defaults to [3.0].
      - [max_multiplier] upper bound on the returned multiplier. Defaults to
        [6.0]. *)

  val tick : t -> now:float -> float
  (** [tick t ~now] is the current multiplier given the monotonic timestamp
      [now] in milliseconds. Call once per scroll event. Resets automatically on
      timeout. *)

  val reset : t -> unit
  (** [reset t] clears the velocity history of [t]. *)
end

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for scroll box property bundles used for reconciler diffing. *)

  val make :
    ?scroll_x:bool ->
    ?scroll_y:bool ->
    ?sticky_scroll:bool ->
    ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
    ?background:Ansi.Color.t ->
    ?scrollbar_props:Scroll_bar.Props.t ->
    ?vertical_bar_props:Scroll_bar.Props.t ->
    ?horizontal_bar_props:Scroll_bar.Props.t ->
    unit ->
    t
  (** [make ()] is a scroll box property bundle with:
      - [scroll_x] enables horizontal scrolling. Defaults to [false].
      - [scroll_y] enables vertical scrolling. Defaults to [true].
      - [sticky_scroll] sticks to an edge as content grows. Defaults to [false].
      - [sticky_start] selects the sticky edge. Defaults to [`Bottom].
      - [background] fills the container background with a color.
      - [scrollbar_props] is applied to both scroll bars.
      - [vertical_bar_props] overrides [scrollbar_props] for the vertical bar.
      - [horizontal_bar_props] overrides [scrollbar_props] for the horizontal
        bar.

      See also {!val-default}. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

(** {1:types Types} *)

type t
(** The type for scroll box widgets backed by a {!Renderable.t}. *)

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?sticky_scroll:bool ->
  ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
  ?background:Ansi.Color.t ->
  ?scroll_accel:Scroll_accel.t ->
  ?scrollbar_props:Scroll_bar.Props.t ->
  ?vertical_bar_props:Scroll_bar.Props.t ->
  ?horizontal_bar_props:Scroll_bar.Props.t ->
  ?on_scroll:(x:int -> y:int -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a new scroll box attached to [parent] with:
    - [index] insertion index among [parent]'s children.
    - [id] node identifier for debugging.
    - [style] layout style. Defaults to the {!Toffee.Style.t} default.
    - [visible] initial visibility. Defaults to [true].
    - [z_index] stacking order. Defaults to [0].
    - [opacity] node opacity. Defaults to [1.0].
    - [scroll_x] enables horizontal scrolling. Defaults to [false].
    - [scroll_y] enables vertical scrolling. Defaults to [true].
    - [sticky_scroll] enables sticky edge tracking. Defaults to [false].
    - [sticky_start] selects the sticky edge. Defaults to [`Bottom].
    - [background] fills the container background.
    - [scroll_accel] acceleration strategy. Defaults to {!Scroll_accel.linear}.
    - [scrollbar_props] visual properties applied to both scroll bars.
    - [vertical_bar_props] overrides [scrollbar_props] for the vertical bar.
    - [horizontal_bar_props] overrides [scrollbar_props] for the horizontal bar.
    - [on_scroll] callback invoked with the new scroll position whenever it
      changes. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the root {!Renderable.t} of [t]. *)

val content : t -> Renderable.t
(** [content t] is the internal content node of [t] that holds user children. *)

val viewport : t -> Renderable.t
(** [viewport t] is the internal viewport node of [t] that clips content. *)

val vertical_bar : t -> Scroll_bar.t
(** [vertical_bar t] is the vertical {!Scroll_bar.t} of [t]. *)

val horizontal_bar : t -> Scroll_bar.t
(** [horizontal_bar t] is the horizontal {!Scroll_bar.t} of [t]. *)

(** {1:scroll_state Scroll state} *)

val scroll_top : t -> int
(** [scroll_top t] is the vertical scroll offset of [t] in cells.

    See also {!val-set_scroll_top}. *)

val scroll_left : t -> int
(** [scroll_left t] is the horizontal scroll offset of [t] in cells.

    See also {!val-set_scroll_left}. *)

val set_scroll_top : t -> int -> unit
(** [set_scroll_top t v] sets the vertical scroll offset of [t] to [v], clamped
    to the valid range. Fires the [on_scroll] callback.

    See also {!val-scroll_top}. *)

val set_scroll_left : t -> int -> unit
(** [set_scroll_left t v] sets the horizontal scroll offset of [t] to [v],
    clamped to the valid range. Fires the [on_scroll] callback.

    See also {!val-scroll_left}. *)

val scroll_to : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_to t ~x ~y ()] sets the absolute scroll position of [t]. Each axis
    is clamped to its valid range.

    See also {!val-scroll_by}. *)

val scroll_by : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_by t ~x ~y ()] adjusts the scroll position of [t] by a relative
    offset.

    See also {!val-scroll_to}. *)

val scroll_by_unit :
  t -> ?x:float -> ?y:float -> unit:Scroll_bar.scroll_unit -> unit -> unit
(** [scroll_by_unit t ~x ~y ~unit ()] adjusts the scroll position of [t] by [x]
    and [y] expressed in [unit]. Delegates to {!Scroll_bar.scroll_by} which
    handles unit conversion and clamping.

    See also {!val-scroll_by} and {!Scroll_bar.type-scroll_unit}. *)

val scroll_width : t -> int
(** [scroll_width t] is the total content width of [t] in cells. *)

val scroll_height : t -> int
(** [scroll_height t] is the total content height of [t] in cells. *)

val viewport_width : t -> int
(** [viewport_width t] is the visible area width of [t] in cells. *)

val viewport_height : t -> int
(** [viewport_height t] is the visible area height of [t] in cells. *)

(** {1:sticky_scroll Sticky scroll} *)

val set_sticky_scroll : t -> bool -> unit
(** [set_sticky_scroll t v] enables ([true]) or disables ([false]) sticky
    scrolling on [t].

    See also {!val-reset_sticky}. *)

val set_sticky_start : t -> [ `Top | `Bottom | `Left | `Right ] option -> unit
(** [set_sticky_start t edge] sets the sticky edge of [t] to [edge]. [None]
    removes the sticky edge. *)

val reset_sticky : t -> unit
(** [reset_sticky t] clears the manual scroll flag of [t] and reapplies sticky
    positioning.

    See also {!val-set_sticky_scroll}. *)

(** {1:appearance Appearance} *)

val set_background : t -> Ansi.Color.t option -> unit
(** [set_background t color] sets the container background color of [t] to
    [color]. [None] removes the background. *)

(** {1:callbacks Callbacks} *)

val set_on_scroll : t -> (x:int -> y:int -> unit) option -> unit
(** [set_on_scroll t f] replaces the scroll callback of [t] with [f]. [None]
    removes the callback. *)

val set_scroll_accel : t -> Scroll_accel.t -> unit
(** [set_scroll_accel t accel] replaces the scroll acceleration strategy of [t]
    with [accel].

    See also {!module-Scroll_accel}. *)

(** {1:keyboard Keyboard} *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] is [true] iff [event] was consumed by [t]. Delegates to
    the internal scroll bars for directional arrows, Page Up/Down, and Home/End.
*)

(** {1:reconciler Reconciler} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces the visual properties of [t] with [props].
    Creation-time fields ({!val-create}'s [scroll_x], [scroll_y]) remain
    unchanged.

    See also {!Props.make}. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a scroll box value for debugging. *)
