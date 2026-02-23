(** Slider with sub-cell precision thumb.

    A slider maps a [float] value in \[[min];[max]\] to a visual thumb position
    along a horizontal or vertical track. The thumb size is proportional to
    [viewport_size] relative to the total range, giving scrollbar-style display
    where the thumb represents the visible portion of content.

    The slider handles left-button mouse events: clicking on the track jumps the
    thumb, clicking on the thumb initiates a drag, and releasing ends it. *)

type t
(** The type for sliders. A slider owns a {!Renderable.t} and manages its own
    rendering and mouse interaction. *)

type orientation = [ `Horizontal | `Vertical ]
(** The type for track directions. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?orientation:orientation ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?on_change:(float -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a slider attached to [parent] with:
    - [orientation] defaults to [`Horizontal].
    - [min] defaults to [0.0].
    - [max] defaults to [100.0].
    - [value] defaults to [min]. Clamped to \[[min];[max]\].
    - [viewport_size] controls thumb size relative to the range. Defaults to
      [max (max -. min) *. 0.1], minimum [1.0].
    - [track_color] defaults to dark gray (RGB 37 37 39).
    - [thumb_color] defaults to medium gray (RGB 154 158 163).
    - [on_change] is called when the clamped value changes, whether from mouse
      interaction or {!set_value}. *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. Callbacks are not part
      of props; set them via {!set_on_change}. *)

  val make :
    ?orientation:orientation ->
    ?value:float ->
    ?min:float ->
    ?max:float ->
    ?viewport_size:float ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    unit ->
    t
  (** [make ()] is a property set. Defaults match {!create}. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties at once and clamps the value
    to the new range. Always triggers a re-render. Does {e not} fire
    [on_change]. *)

(** {1:value Value} *)

val value : t -> float
(** [value t] is the current value, always in \[[min];[max]\]. *)

val set_value : t -> float -> unit
(** [set_value t v] sets the value to [v] clamped to \[[min];[max]\]. Fires
    [on_change] if the clamped result differs from the current value. *)

val min : t -> float
(** [min t] is the lower bound of the range. *)

val set_min : t -> float -> unit
(** [set_min t v] sets the lower bound. If the current value is below [v], it is
    clamped up, which may fire [on_change]. No effect if [v] equals the current
    minimum. *)

val max : t -> float
(** [max t] is the upper bound of the range. *)

val set_max : t -> float -> unit
(** [set_max t v] sets the upper bound. If the current value is above [v], it is
    clamped down, which may fire [on_change]. No effect if [v] equals the
    current maximum. *)

(** {1:appearance Appearance} *)

val set_orientation : t -> orientation -> unit
(** [set_orientation t o] sets the track direction. No effect if [o] equals the
    current orientation. *)

val set_viewport_size : t -> float -> unit
(** [set_viewport_size t v] sets the visible portion size, clamped to
    \[[0.01];[max -. min]\]. Controls thumb size relative to the range. No
    effect if the clamped result equals the current viewport size. *)

val set_track_color : t -> Ansi.Color.t -> unit
(** [set_track_color t c] sets the track background color. No effect if [c]
    equals the current track color. *)

val set_thumb_color : t -> Ansi.Color.t -> unit
(** [set_thumb_color t c] sets the thumb foreground color. No effect if [c]
    equals the current thumb color. *)

(** {1:callback Callback} *)

val set_on_change : t -> (float -> unit) option -> unit
(** [set_on_change t f] replaces the change callback. [None] removes it. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a slider for debugging. *)
