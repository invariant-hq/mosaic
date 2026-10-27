(** Scrollbar input emits position proposals. Accept positions with
    [set_scroll_position]; proposals never move the accepted thumb themselves.
*)

(** Scroll bar widget with optional arrow buttons and a proportional slider
    thumb.

    A scroll bar manages a scroll position within a range determined by
    {!scroll_size} and {!viewport_size}. The slider thumb is sized
    proportionally to the visible portion of content. Arrow buttons at each end
    provide incremental scrolling with hold-to-repeat. The arrows are the
    theme's glyphs ({!Glyphs}), and the track and the thumb are drawn as a
    {!Slider}'s.

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

(** {1:style Appearance} *)

type style
(** Complete scrollbar appearance, independent of orientation, position, range
    and automatic visibility. *)

val style :
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  unit ->
  style
(** [style ()] is the default appearance: arrows hidden, and the track, the
    thumb and the arrows drawn by the roles of the theme in scope, as
    {!Props.make} states. Each optional argument replaces its role. *)

val style_equal : style -> style -> bool
(** [style_equal a b] is [true] iff [a] and [b] specify the same appearance. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for the declarative property bundle used for reconciler diffing.
  *)

  val make :
    ?position:int ->
    ?scroll_size:int ->
    ?viewport_size:int ->
    ?orientation:orientation ->
    ?show_arrows:bool ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    ?arrow_fg:Ansi.Color.t ->
    ?arrow_bg:Ansi.Color.t ->
    unit ->
    t
  (** [make ()] is a scroll bar property bundle. The optional parameters are:
      - [position] — the scroll position, at least [0]; the bar keeps it in its
        valid range. Defaults to [0].
      - [scroll_size] — total content size, at least [0]. Defaults to [0].
      - [viewport_size] — size of the visible area, at least [0]. Defaults to
        [0].
      - [orientation] — layout direction. Defaults to [`Vertical].
      - [show_arrows] — display arrow buttons at each end. Defaults to [false].
      - [track_color] — track background color. Defaults to none, a line in the
        [Border] role ({!Slider}).
      - [thumb_color] — thumb foreground color. Defaults to none, the [Plain]
        role.
      - [arrow_fg] — arrow foreground color. Defaults to none, the [Border]
        role.
      - [arrow_bg] — arrow background color. Defaults to none.

      {!val-create} takes these labels with these defaults, and so does
      [Mosaic.scroll_bar] for the labels it has. *)

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
  ?position:int ->
  ?scroll_size:int ->
  ?viewport_size:int ->
  ?orientation:orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  ?on_change:(int -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a scroll bar attached to [parent]. The labels from
    [position] to [arrow_bg] are {!Props.make}'s and take its defaults.
    [on_change] is invoked with the new scroll position whenever it changes. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

(** {1:scroll_state Scroll state} *)

val scroll_position : t -> int
(** [scroll_position t] is the current scroll offset of [t], clamped to
    \[[0];[max(0, scroll_size - viewport_size)]\].

    See also {!set_scroll_position}. *)

val set_scroll_position : t -> int -> unit
(** [set_scroll_position t value] accepts and clamps [value] without firing
    [on_change]. *)

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

val scroll_target : t -> float -> unit:scroll_unit -> int
(** [scroll_target t delta ~unit] computes a target from the accepted position
    without emitting a proposal. *)

val scroll_by : t -> float -> unit:scroll_unit -> unit
(** [scroll_by t delta ~unit] adjusts the scroll position of [t] by [delta]
    expressed in [unit]. The resulting position is clamped to the valid range.
    See {!type-scroll_unit} for the meaning of each unit. *)

val set_scroll_step : t -> int option -> unit
(** [set_scroll_step t step] sets the custom step size used when [unit] is
    [`Step] in {!scroll_by}. [None] resets the step to the default of [1]. *)

(** {1:appearance Appearance} *)

val apply_style : t -> style -> unit
(** [apply_style t style] replaces [t]'s appearance without changing its
    orientation, accepted position, range or visibility policy. Does not emit
    [on_change]. Arrow visibility can change the space available to the thumb.
*)

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
(** [apply_props t props] replaces the visual properties of [t] with [props],
    including orientation and arrow colors. An orientation change reorients the
    slider and arrows and swaps the dimensions the widget owns (those left
    [auto] at creation); user-set dimensions are preserved. Does not fire the
    [on_change] callback.

    See also {!Props.make}. *)

(** {1:keyboard Keyboard interaction} *)

val handle_key : t -> Event.key -> bool
(** [handle_key t event] proposes a new position for the keys a scroll bar acts
    on, and is [true] iff [event] is one of them; that is the bar's behavior
    when focused (see {!Renderable.set_key_behavior}), which consumes the keys
    it acts on. A vertical bar acts on:
    - Up, k, Down, j — move by a fifth of the viewport;
    - PageUp, PageDown — move by half the viewport;
    - Home, End — move to the start or the end.

    A horizontal bar acts on Left, h, Right and l in place of Up, k, Down and j.
    It acts on them at an edge too. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a scroll bar value for debugging. *)
