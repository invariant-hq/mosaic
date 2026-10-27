(** A clipped viewport rendering an application's accepted {!Scroll.t}. Input
    proposes a new value through [on_scroll]; only [apply_props] or [set_scroll]
    accepts it. Wheel events at a boundary bubble to the enclosing viewport.

    {1:keys Keys}

    When focused, the scroll box's behavior (see {!Renderable.set_key_behavior})
    acts on the keys of the scroll bar of each axis it scrolls
    ({!Scroll_bar.handle_key}) and consumes them: they reach neither its
    ancestors nor any listener after the elements, such as an application's
    shortcuts. Every other key passes on.
    - Up, k, Down, j — if it scrolls vertically, scroll up or down by a fifth of
      the viewport.
    - Left, h, Right, l — if it scrolls horizontally, scroll left or right by a
      fifth of the viewport.
    - PageUp, PageDown, Home, End — scroll by half the viewport, or to the start
      or the end, vertically if it scrolls vertically, otherwise horizontally if
      it scrolls horizontally.

    A key it uses stays consumed at the edge, where it changes nothing, as a
    full text input still takes letters. A box that scrolls along neither axis
    acts on no key. *)

module Scroll_accel : sig
  type t
  (** The type for scroll gesture acceleration. *)

  val linear : unit -> t
  (** [linear ()] applies no acceleration. *)

  val macos : ?a:float -> ?tau:float -> ?max_multiplier:float -> unit -> t
  (** [macos ()] uses exponential acceleration with defaults [0.8], [3.0]
      seconds, and [6.0]. *)

  val tick : t -> seconds:float -> float
  (** [tick t ~seconds] advances the gesture clock to the time [seconds]. *)

  val reset : t -> unit
  (** [reset t] clears its velocity history. *)
end

module Props : sig
  type t

  val make :
    ?scroll_x:bool ->
    ?scroll_y:bool ->
    ?scroll:Scroll.t ->
    ?background:Ansi.Color.t ->
    ?scroll_accel:Scroll_accel.t ->
    ?show_scrollbars:bool ->
    ?scrollbar_style:Scroll_bar.style ->
    ?vertical_bar_style:Scroll_bar.style ->
    ?horizontal_bar_style:Scroll_bar.style ->
    unit ->
    t
  (** [make ()] declares a viewport with:
      - [scroll_x] is [true] iff the content scrolls horizontally. Defaults to
        [false].
      - [scroll_y] is [true] iff the content scrolls vertically. Defaults to
        [true].
      - [scroll] is the declared position. Defaults to [Scroll.at 0].
      - [background] is the scroll box's background color. Defaults to none.
      - [scroll_accel] is the acceleration of wheel scrolling. Defaults to
        {!Scroll_accel.linear}. It keeps the history of the gesture in progress,
        and props compare it physically, so a view must give the same value on
        every declaration, made once outside the view. A new value starts a new
        history.
      - [show_scrollbars] is [true] iff the bars are shown when the content
        overflows. Defaults to [true].
      - [scrollbar_style], [vertical_bar_style] and [horizontal_bar_style] are
        the bars' appearance, as follows. Default to none.

      {!val-create} and [Mosaic.scroll_box] take these labels with these
      defaults.

      [scrollbar_style] applies to both bars. A [vertical_bar_style] or
      [horizontal_bar_style] replaces the shared style for that axis in full.
      Without either override, the bar uses {!Scroll_bar.style}'s defaults.
      Removing an axis-specific style restores the shared style; removing all
      styles restores the defaults. The viewport owns each bar's orientation,
      position, range and automatic visibility. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] compares declared properties. *)
end

type t

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
  ?scroll:Scroll.t ->
  ?background:Ansi.Color.t ->
  ?scroll_accel:Scroll_accel.t ->
  ?show_scrollbars:bool ->
  ?scrollbar_style:Scroll_bar.style ->
  ?vertical_bar_style:Scroll_bar.style ->
  ?horizontal_bar_style:Scroll_bar.style ->
  ?on_scroll:(Scroll.t -> unit) ->
  unit ->
  t
(** [create ~parent ()] attaches a viewport. User children are routed to its
    content node. The labels after [opacity], but [on_scroll], are
    {!Props.make}'s and take its defaults. Missing [on_scroll] rejects
    interactive scroll proposals. *)

val node : t -> Renderable.t
val content : t -> Renderable.t
val viewport : t -> Renderable.t
val vertical_bar : t -> Scroll_bar.t
val horizontal_bar : t -> Scroll_bar.t
val scroll_top : t -> int
val scroll_left : t -> int
val scroll_width : t -> int
val scroll_height : t -> int
val viewport_width : t -> int
val viewport_height : t -> int

val set_scroll : t -> Scroll.t -> unit
(** [set_scroll t state] accepts a declaration without emitting [on_scroll]. *)

val set_scroll_top : t -> int -> unit
val set_scroll_left : t -> int -> unit

val scroll_to : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_to t ?x ?y ()] proposes the requested absolute position. *)

val scroll_by : t -> ?x:int -> ?y:int -> unit -> unit
(** [scroll_by t ?x ?y ()] proposes movement from the accepted offset. *)

val scroll_by_unit :
  t -> ?x:float -> ?y:float -> unit:Scroll_bar.scroll_unit -> unit -> unit
(** [scroll_by_unit t ?x ?y ~unit ()] proposes movement in the given units. *)

val set_on_scroll : t -> (Scroll.t -> unit) option -> unit
val set_scroll_accel : t -> Scroll_accel.t -> unit
val set_background : t -> Ansi.Color.t option -> unit
val set_show_scrollbars : t -> bool -> unit
val handle_key : t -> Event.key -> bool
val set_style : t -> Toffee.Style.t -> unit

val apply_props : t -> Props.t -> unit
(** [apply_props t props] accepts the declaration and resolves it against the
    viewport; following and reveal remain active after content changes. *)

val pp : Format.formatter -> t -> unit
