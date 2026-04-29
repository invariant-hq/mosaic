(** Animated spinner cycling through configurable frame sets.

    A spinner cycles through an array of text frames at a fixed interval,
    rendering the current frame with a configurable foreground color. Animation
    advances on each tick: elapsed time accumulates and the frame index advances
    by one or more steps when the interval threshold is reached.

    Several built-in frame sets are provided ({!dots}, {!line}, {!arc}, etc.).
    Custom frame sets can be constructed directly as {!type-frame_set} records.
*)

(** {1:types Types} *)

type t
(** The type for animated spinner widgets. *)

type frame_set = {
  frames : string array;  (** Frames to cycle through. *)
  interval : float;  (** Time between frames, in milliseconds. *)
}
(** The type for animation frame sets. The spinner advances one frame each time
    {!field-interval} milliseconds have elapsed. *)

(** {1:frame_sets Built-in frame sets} *)

val dots : frame_set
(** Braille dot pattern (10 frames, 80 ms). *)

val dots2 : frame_set
(** Braille block pattern (8 frames, 80 ms). *)

val line : frame_set
(** ASCII line rotation (4 frames, 130 ms). *)

val arc : frame_set
(** Quarter-circle arc (6 frames, 100 ms). *)

val bounce : frame_set
(** Braille bounce (4 frames, 120 ms). *)

val circle : frame_set
(** Circle animation (3 frames, 120 ms). *)

val default_frame_set : frame_set
(** [default_frame_set] is {!dots}. *)

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?frame_set:frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  t
(** [create ~parent ()] is a spinner attached to [parent], with:
    - [frame_set] is the animation sequence. Defaults to {!default_frame_set}.
    - [color] is the foreground color for the spinner glyphs. Defaults to
      {!Ansi.Color.white}. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} of [t]. *)

(** {1:props Properties} *)

module Props : sig
  type t
  (** The type for declarative property bundles used by the reconciler. *)

  val make : ?frame_set:frame_set -> ?color:Ansi.Color.t -> unit -> t
  (** [make ()] is a property set, with:
      - [frame_set] is the animation sequence. Defaults to {!default_frame_set}.
      - [color] is the foreground color. Defaults to {!Ansi.Color.white}. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties of [t] with [props]. Wraps the
    frame index when the frame set changes. Always triggers a re-render. *)

(** {1:accessors Accessors} *)

val frame_index : t -> int
(** [frame_index t] is the current zero-based frame index of [t]. *)

val elapsed : t -> float
(** [elapsed t] is the accumulated time in milliseconds since the last frame
    advance of [t]. *)

(** {1:setters Setters} *)

val set_frame_set : t -> frame_set -> unit
(** [set_frame_set t fs] changes the frame set of [t] to [fs]. Resets elapsed
    time to [0.] and wraps the frame index to the new frame count. *)

val set_color : t -> Ansi.Color.t -> unit
(** [set_color t c] changes the foreground color of [t] to [c]. Re-renders only
    if [c] differs from the current color. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
