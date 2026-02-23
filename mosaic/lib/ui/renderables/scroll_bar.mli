(** Scroll bar widget with optional arrow buttons and a proportional slider
    thumb.

    A scroll bar manages a scroll position within a range determined by
    {!scroll_size} and {!viewport_size}. The slider thumb is sized
    proportionally to the visible portion of content. Arrow buttons at each end
    provide incremental scrolling with hold-to-repeat.

    {2:coordinates Coordinate system}

    The scroll coordinate space is expressed in cells:
    - {e scroll_position} — current scroll offset.
    - {e viewport_size} — size of the visible area.
    - {e scroll_size} — total content size.
    - Valid range: \[[0];[max(0, scroll_size - viewport_size)]\].

    {2:scroll_units Scroll units}

    {!scroll_by} supports four unit types:
    - [`Absolute] — delta multiplied by 1.
    - [`Viewport] — delta multiplied by viewport size.
    - [`Content] — delta multiplied by content size.
    - [`Step] — delta multiplied by the custom step size (default [1]). *)

type orientation = [ `Vertical | `Horizontal ]
(** The type for scroll bar orientation. *)

type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]
(** The type for scroll delta units used by {!scroll_by}. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for the declarative property bundle used for reconciler diffing.
  *)

  val make :
    ?orientation:orientation ->
    ?show_arrows:bool ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    ?arrow_fg:Ansi.Color.t ->
    ?arrow_bg:Ansi.Color.t ->
    unit ->
    t
  (** [make ()] is a scroll bar property bundle. The optional parameters are:
      - [orientation] — layout direction. Defaults to [`Vertical].
      - [show_arrows] — display arrow buttons at each end. Defaults to [false].
      - [track_color] — track background color.
      - [thumb_color] — thumb foreground color.
      - [arrow_fg] — arrow foreground color. Defaults to white.
      - [arrow_bg] — arrow background color. Defaults to transparent. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

(** {1:types Types} *)

type t
(** The type for scroll bar widgets backed by a {!Renderable.t}. *)

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?orientation:orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  ?on_change:(int -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a scroll bar attached to [parent]. The optional
    parameters are:
    - [orientation] — layout direction. Defaults to [`Vertical].
    - [show_arrows] — display arrow buttons at each end. Defaults to [false].
    - [on_change] — callback invoked with the new scroll position whenever it
      changes. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

(** {1:scroll_state Scroll state} *)

val scroll_position : t -> int
(** [scroll_position t] is the current scroll offset of [t], clamped to
    \[[0];[max(0, scroll_size - viewport_size)]\].

    See also {!set_scroll_position}. *)

val set_scroll_position : t -> int -> unit
(** [set_scroll_position t v] sets the scroll position of [t] to [v], clamped to
    the valid range. Fires the [on_change] callback if the clamped value differs
    from the current position.

    See also {!scroll_position} and {!scroll_by}. *)

val scroll_size : t -> int
(** [scroll_size t] is the total content size of [t] in cells.

    See also {!set_scroll_size}. *)

val set_scroll_size : t -> int -> unit
(** [set_scroll_size t v] sets the content size of [t] to [v] cells. Clamps the
    scroll position to the updated valid range and recalculates thumb
    visibility.

    See also {!scroll_size}. *)

val viewport_size : t -> int
(** [viewport_size t] is the visible area size of [t] in cells.

    See also {!set_viewport_size}. *)

val set_viewport_size : t -> int -> unit
(** [set_viewport_size t v] sets the visible area size of [t] to [v] cells.
    Affects thumb sizing and the scroll range.

    See also {!viewport_size}. *)

val scroll_by : t -> float -> unit:scroll_unit -> unit
(** [scroll_by t delta ~unit] adjusts the scroll position of [t] by [delta]
    expressed in [unit]. The resulting position is clamped to the valid range.
    See {!type-scroll_unit} for the meaning of each unit. *)

val set_scroll_step : t -> int option -> unit
(** [set_scroll_step t step] sets the custom step size used when [unit] is
    [`Step] in {!scroll_by}. [None] resets the step to the default of [1]. *)

(** {1:appearance Appearance} *)

val set_show_arrows : t -> bool -> unit
(** [set_show_arrows t v] shows ([true]) or hides ([false]) the arrow buttons of
    [t]. *)

val set_track_color : t -> Ansi.Color.t -> unit
(** [set_track_color t c] sets the track background color of [t] to [c]. *)

val set_thumb_color : t -> Ansi.Color.t -> unit
(** [set_thumb_color t c] sets the thumb foreground color of [t] to [c]. *)

(** {1:visibility Visibility} *)

val set_visible_override : t -> bool -> unit
(** [set_visible_override t v] sets the visibility of [t] to [v] and disables
    automatic visibility control.

    See also {!reset_visibility_control}. *)

val reset_visibility_control : t -> unit
(** [reset_visibility_control t] re-enables automatic visibility for [t] based
    on content overflow, undoing any prior call to {!set_visible_override}. *)

(** {1:callback Callback} *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t f] replaces the change callback of [t] with [f]. [None]
    removes the callback. *)

(** {1:applying Applying props} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces the visual properties of [t] with [props].
    Creation-time fields such as orientation remain unchanged. Does not fire the
    [on_change] callback.

    See also {!Props.make}. *)

(** {1:keyboard Keyboard interaction} *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] is [true] iff [event] was consumed by [t]. Handles
    directional arrows, Page Up/Down, and Home/End to adjust the scroll
    position. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a scroll bar value for debugging. *)
