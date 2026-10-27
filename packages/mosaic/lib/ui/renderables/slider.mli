(** Slider with sub-cell precision thumb.

    A slider maps a [float] value in \[[min];[max]\] to a visual thumb position
    along a horizontal or vertical track. The thumb size is proportional to
    [viewport_size] relative to the total range, giving scrollbar-style display
    where the thumb represents the visible portion of content.

    The slider handles left-button mouse events: clicking on the track jumps the
    thumb, clicking on the thumb initiates a drag, and releasing ends it.

    The slider draws by the roles of the theme in scope ({!Renderable.theme}):
    the track as a line in the [Border] role and the thumb as blocks in the
    [Plain] role, so it shows whatever the output's styling. A [track_color]
    fills the track as a background instead, and a [thumb_color] colors the
    thumb. *)

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
(** [create ~parent ()] is a slider attached to [parent]. The labels from
    [orientation] to [thumb_color] are {!Props.make}'s and take its defaults.
    [on_change] receives clamped proposals from mouse interaction. Input
    proposes a value; it does not accept it. *)

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
  (** [make ()] declares an accepted value, clamped to the declared range, with:
      - [orientation] defaults to [`Horizontal].
      - [min] defaults to [0.0].
      - [max] defaults to [100.0].
      - [value] defaults to [min]. Clamped to \[[min];[max]\]. There is no
        uncontrolled value.
      - [viewport_size] controls thumb size relative to the range. Defaults to
        [max (max -. min) *. 0.1], minimum [1.0].
      - [track_color] defaults to none, a line in the [Border] role.
      - [thumb_color] defaults to none, the [Plain] role.

      {!val-create} and {!Vnode.slider} take these labels with these defaults.
  *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] accepts all properties, clamps the value to the
    declared range, and requests rendering without firing [on_change]. *)

val value : t -> float
(** [value t] is the current value, always in \[[min];[max]\]. *)

val set_value : t -> float -> unit
(** [set_value t value] accepts and clamps [value] without firing [on_change].
*)

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
