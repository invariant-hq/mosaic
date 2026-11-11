(** Progress bar with sub-cell precision fill rendering.

    A progress bar maps a numeric value within a [min, max] range to a visual
    fill level along a horizontal or vertical track. The filled portion
    represents the current value relative to the range.

    Rendering uses a double-resolution virtual coordinate system with Unicode
    half-block characters for sub-cell fill precision. Values are clamped to
    \[[min];[max]\] before rendering. When [min = max], the bar renders as fully
    filled.

    The bar draws a glyph in each cell ({!type-glyphs}), by the roles of the
    theme in scope ({!Renderable.theme}): the filled portion's in the [Plain]
    role, the unfilled portion's in the [Border] role, and the half glyph, in
    the cell where the value ends half way, in the filled portion's. So it shows
    its value whatever the output's styling. A [filled_color] or [empty_color]
    is the foreground of its portion's glyphs. By default the glyphs are its
    orientation's in the glyph set of the theme in scope ({!Theme.glyphs}): in
    {!Glyphs.unicode} full blocks, a half block and a line. A heavy line filled
    and a light one empty make a gauge one row high, [━━━━╸─────]. *)

type t
(** The type for progress bar widgets backed by a {!Renderable.t}. *)

type orientation = [ `Horizontal | `Vertical ]
(** The type for track direction. *)

type glyphs = Glyphs.progress = {
  filled : string;
  half : string;
  empty : string;
}
(** The type for the glyphs of a bar's cells ({!Glyphs.type-progress}). Each
    glyph takes one cell, by every {!Matrix.Text.width_method}. *)

(** {1:constructors Constructors} *)

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
    ?glyphs:glyphs ->
    unit ->
    t
  (** [make ()] is a property set with:
      - [value] is the fill value. Defaults to [0.0].
      - [min] is the lower bound of the range. Defaults to [0.0].
      - [max] is the upper bound of the range. Defaults to [1.0].
      - [orientation] is the track direction. Defaults to [`Horizontal].
      - [filled_color] is the color of the filled portion's glyphs. Defaults to
        none, the [Plain] role.
      - [empty_color] is the color of the unfilled portion's glyphs. Defaults to
        none, the [Border] role.
      - [glyphs] are the glyphs of the bar's cells. Defaults to none: the
        orientation's in the glyph set of the theme in scope,
        {!Glyphs.t.progress_horizontal} or {!Glyphs.t.progress_vertical}.

      [Mosaic.progress_bar] takes these labels with these defaults.

      Raises [Invalid_argument] if a glyph does not take one cell. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  Props.t ->
  t
(** [create ~parent props] is a progress bar attached to [parent], showing
    [props]. The node declares itself unselectable
    ({!Renderable.set_selectable}). *)

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

val set_glyphs : t -> glyphs option -> unit
(** [set_glyphs t g] sets the glyphs of [t]'s cells to [g]. [None] draws its
    orientation's in the glyph set of the theme in scope. Triggers a re-render
    if the glyphs changed.

    Raises [Invalid_argument] if a glyph of [g] does not take one cell. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a progress bar for debugging. *)
