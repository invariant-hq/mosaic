(** Bordered container with background fill and optional title.

    A box wraps a {!Renderable.t} with border drawing, background filling, and
    child clipping. While the box has a border, each of its border sides takes
    its {e border width} in layout, one cell unless [border_width] says
    otherwise. A side of width [0] draws no line. A wider side draws its line in
    its outermost cell and leaves the cells inside it blank. A side without a
    border takes no cells, whatever its border width, as a CSS border whose
    style is [none] has width [0] (CSS Backgrounds and Borders Level 3, §3.2).
    Children are clipped to the area inside the border widths. The box sets the
    border of its layout style from these widths, replacing the one the style
    gives.

    The border is drawn in the [Border] role of the theme in scope
    ({!Renderable.theme}), or in [border_color] where it is given. While the box
    is focused, its border is drawn in [focused_border_color] where it is given,
    and otherwise in {!Matrix_grid.Border.heavy} lines in bold, so focus shows
    by a glyph whatever the output's styling, and by an attribute where it has
    styling. The title is drawn in the border's style.

    Setting a border-related property ({!set_border_style}, {!set_border_color},
    {!set_focused_border_color}) auto-enables borders. *)

type t
(** The type for boxes. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?border:bool ->
  ?border_style:Matrix_grid.Border.t ->
  ?border_sides:Matrix_grid.Border.side list ->
  ?border_width:Toffee.Style.Length_percentage.t Toffee.Geometry.Rect.t ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?inert:bool ->
  unit ->
  t
(** [create ~parent ()] is a box node attached to [parent]. The labels after
    [opacity] are {!Props.make}'s and take its defaults.

    Raises [Invalid_argument] as {!Props.make} does. *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles. Used for reconciler diffing. *)

  val make :
    ?border:bool ->
    ?border_style:Matrix_grid.Border.t ->
    ?border_sides:Matrix_grid.Border.side list ->
    ?border_width:Toffee.Style.Length_percentage.t Toffee.Geometry.Rect.t ->
    ?border_color:Ansi.Color.t ->
    ?focused_border_color:Ansi.Color.t ->
    ?background:Ansi.Color.t ->
    ?fill:bool ->
    ?title:string ->
    ?title_alignment:[ `Left | `Center | `Right ] ->
    ?inert:bool ->
    unit ->
    t
  (** [make ()] is the properties of a box, with:
      - [border] defaults to [false]. Providing any of [border_style],
        [border_color], or [focused_border_color] implicitly enables it.
      - [border_style] defaults to {!Matrix_grid.Border.single}.
      - [border_sides] defaults to {!Matrix_grid.Border.all} (all four sides).
      - [border_width] is the border width of each side, in whole cells.
        Defaults to one cell on each side.
      - [border_color] defaults to none, the [Border] role.
      - [focused_border_color] defaults to none, heavy lines in bold.
      - [background] defaults to [None] (transparent).
      - [fill] defaults to [true].
      - [title] defaults to [None].
      - [title_alignment] defaults to [`Left].
      - [inert] makes the box and its descendants inert
        ({!Renderable.set_inert}). Defaults to [false].

      {!val-create} and [Mosaic.box] take these labels with these defaults.

      Raises [Invalid_argument] if a border width is not a length of a whole
      number of cells, [0] or more. *)

  val default : t
  (** [default] is [make ()]. *)

  val inert : t -> bool
  (** [inert p] is [p]'s [inert]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties of [t] with [props], updates
    the border layout, and schedules a re-render. *)

(** {1:border Border} *)

val set_border : t -> bool -> unit
(** [set_border t v] enables or disables border rendering. Adjusts layout to
    reserve or release space for active border sides. *)

val set_border_style : t -> Matrix_grid.Border.t -> unit
(** [set_border_style t chars] sets the border character set to [chars].
    Auto-enables borders. *)

val set_border_sides : t -> Matrix_grid.Border.side list -> unit
(** [set_border_sides t sides] selects which sides to draw. Adjusts layout for
    changed insets. *)

val set_border_color : t -> Ansi.Color.t -> unit
(** [set_border_color t color] sets the unfocused border color. Auto-enables
    borders. *)

val set_focused_border_color : t -> Ansi.Color.t option -> unit
(** [set_focused_border_color t color] sets the border color used when [t] has
    focus. [None] draws a focused border in heavy lines in bold. [Some _]
    auto-enables borders. Only triggers a re-render when [t] is focused. *)

(** {1:background Background} *)

val set_background : t -> Ansi.Color.t option -> unit
(** [set_background t color] sets the background fill color. [None] is
    transparent. *)

val set_fill : t -> bool -> unit
(** [set_fill t v] enables or disables background filling of the interior. When
    [false], the background color is ignored. *)

(** {1:title Title} *)

val set_title : t -> string option -> unit
(** [set_title t text] sets or clears the title drawn on the top border. *)

val set_title_alignment : t -> [ `Left | `Center | `Right ] -> unit
(** [set_title_alignment t align] positions the title within the top border. *)

(** {1:layout Layout} *)

val set_style : t -> Toffee.Style.t -> unit
(** [set_style t style] updates the layout style. The box replaces [style]'s
    border with its border widths. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a box for debugging. *)
