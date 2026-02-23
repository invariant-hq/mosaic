(** Bordered container with background fill and optional title.

    A box wraps a {!Renderable.t} with border drawing, background filling, and
    child clipping. Each active border side consumes one terminal cell in
    layout; children are clipped to the content area inside borders.

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
  ?border_style:Grid.Border.t ->
  ?border_sides:Grid.Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  unit ->
  t
(** [create ~parent ()] is a box node attached to [parent] with:
    - [border] defaults to [false]. Providing any of [border_style],
      [border_color], or [focused_border_color] implicitly enables it.
    - [border_style] defaults to {!Grid.Border.single}.
    - [border_sides] defaults to {!Grid.Border.all} (all four sides).
    - [border_color] defaults to {!Ansi.Color.White}.
    - [focused_border_color] defaults to {!Ansi.Color.Bright_cyan}.
    - [background] defaults to [None] (transparent).
    - [fill] defaults to [true].
    - [title] defaults to [None].
    - [title_alignment] defaults to [`Left]. *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles. Used for reconciler diffing. *)

  val make :
    ?border:bool ->
    ?border_style:Grid.Border.t ->
    ?border_sides:Grid.Border.side list ->
    ?border_color:Ansi.Color.t ->
    ?focused_border_color:Ansi.Color.t ->
    ?background:Ansi.Color.t ->
    ?fill:bool ->
    ?title:string ->
    ?title_alignment:[ `Left | `Center | `Right ] ->
    unit ->
    t
  (** [make ()] is a property set with the same defaults as {!val-create}.
      Border-related options auto-enable borders when present. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties of [t] with [props], updates
    the border layout, and schedules a re-render. *)

(** {1:border Border} *)

val set_border : t -> bool -> unit
(** [set_border t v] enables or disables border rendering. Adjusts layout to
    reserve or release space for active border sides. *)

val set_border_style : t -> Grid.Border.t -> unit
(** [set_border_style t chars] sets the border character set to [chars].
    Auto-enables borders. *)

val set_border_sides : t -> Grid.Border.side list -> unit
(** [set_border_sides t sides] selects which sides to draw. Adjusts layout for
    changed insets. *)

val set_border_color : t -> Ansi.Color.t -> unit
(** [set_border_color t color] sets the unfocused border color. Auto-enables
    borders. *)

val set_focused_border_color : t -> Ansi.Color.t option -> unit
(** [set_focused_border_color t color] sets the border color used when [t] has
    focus. [None] falls back to the unfocused color. [Some _] auto-enables
    borders. Only triggers a re-render when [t] is focused. *)

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
(** [set_style t style] updates the layout style. Border insets are applied
    automatically on top of [style]. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a box for debugging. *)
