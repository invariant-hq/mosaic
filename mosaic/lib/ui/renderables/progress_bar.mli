(** Progress bar with sub-cell precision fill rendering.

    A progress bar maps a numeric value within a [min, max] range to a visual
    fill level along a horizontal or vertical track. The filled portion
    represents the current value relative to the range.

    Rendering uses a double-resolution virtual coordinate system with Unicode
    half-block characters for sub-cell fill precision. Values are clamped to
    \[[min];[max]\] before rendering. When [min = max], the bar renders as fully
    filled. *)

type t
(** The type for progress bar widgets backed by a {!Renderable.t}. *)

type orientation = [ `Horizontal | `Vertical ]
(** The type for track direction. *)

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?orientation:orientation ->
  ?filled_color:Ansi.Color.t ->
  ?empty_color:Ansi.Color.t ->
  unit ->
  t
(** [create ~parent ()] is a progress bar attached to [parent] with:
    - [value] is the initial fill value. Defaults to [0.0].
    - [min] is the lower bound of the range. Defaults to [0.0].
    - [max] is the upper bound of the range. Defaults to [1.0].
    - [orientation] is the track direction. Defaults to [`Horizontal].
    - [filled_color] is the color of the filled portion. Defaults to medium
      gray.
    - [empty_color] is the color of the unfilled portion. Defaults to dark gray.
*)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles used for reconciler diffing. *)

  val make :
    ?value:float ->
    ?min:float ->
    ?max:float ->
    ?orientation:orientation ->
    ?filled_color:Ansi.Color.t ->
    ?empty_color:Ansi.Color.t ->
    unit ->
    t
  (** [make ()] is a property set with:
      - [value] is the fill value. Defaults to [0.0].
      - [min] is the lower bound of the range. Defaults to [0.0].
      - [max] is the upper bound of the range. Defaults to [1.0].
      - [orientation] is the track direction. Defaults to [`Horizontal].
      - [filled_color] is the color of the filled portion. Defaults to medium
        gray.
      - [empty_color] is the color of the unfilled portion. Defaults to dark
        gray. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties of [t] with [props]. Always
    triggers a re-render. *)

(** {1:value Value} *)

val value : t -> float
(** [value t] is the current fill value of [t]. *)

val set_value : t -> float -> unit
(** [set_value t v] sets the fill value of [t] to [v]. The value is stored
    as-is; clamping to \[[min t];[max t]\] occurs at render time. Triggers a
    re-render if the value changed. *)

val min : t -> float
(** [min t] is the lower bound of the range of [t]. *)

val set_min : t -> float -> unit
(** [set_min t v] sets the lower bound of the range of [t] to [v]. Triggers a
    re-render if the bound changed. *)

val max : t -> float
(** [max t] is the upper bound of the range of [t]. *)

val set_max : t -> float -> unit
(** [set_max t v] sets the upper bound of the range of [t] to [v]. Triggers a
    re-render if the bound changed. *)

(** {1:appearance Appearance} *)

val set_orientation : t -> orientation -> unit
(** [set_orientation t o] sets the track direction of [t] to [o]. Triggers a
    re-render if the orientation changed. *)

val set_filled_color : t -> Ansi.Color.t -> unit
(** [set_filled_color t c] sets the filled portion color of [t] to [c]. Triggers
    a re-render if the color changed. *)

val set_empty_color : t -> Ansi.Color.t -> unit
(** [set_empty_color t c] sets the unfilled portion color of [t] to [c].
    Triggers a re-render if the color changed. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a progress bar for debugging. *)
