(** Line number gutter for code display.

    A composite container that pairs a line number gutter with a content area.
    Children are routed to the content area via {!Renderable.child_target}. On
    render, the gutter discovers the first child with a {!Renderable.line_info}
    provider and draws line numbers accordingly.

    The gutter renders line numbers, optional signs, and per-line background
    colors. Content area colors are applied via a [render_before] callback on
    the content node. Setting [show_line_numbers] to [false] hides the gutter
    entirely; it then takes no layout space. *)

(** {1:types Types} *)

type t
(** The type for line number gutter containers. *)

type line_color = { gutter : Ansi.Color.t; content : Ansi.Color.t option }
(** The type for per-line background colors. {!gutter} colors the gutter area.
    {!content} colors the content area; when [None], defaults to 80 % brightness
    of {!gutter}. *)

type line_sign = {
  before : string option;
  after : string option;
  before_color : Ansi.Color.t option;
  after_color : Ansi.Color.t option;
}
(** The type for gutter signs rendered before and after the line number. Before
    signs are right-aligned within their column for visual consistency. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles. Used for reconciler diffing. *)

  val make :
    ?fg:Ansi.Color.t ->
    ?bg:Ansi.Color.t ->
    ?min_width:int ->
    ?padding_right:int ->
    ?show_line_numbers:bool ->
    ?line_number_offset:int ->
    ?line_colors:(int * line_color) list ->
    ?line_signs:(int * line_sign) list ->
    ?line_numbers:(int * int) list ->
    ?hidden_line_numbers:int list ->
    unit ->
    t
  (** [make ()] is a line number props value with:
      - [fg] is the line number foreground color. Defaults to medium gray.
      - [bg] is the gutter background color. Defaults to [None] (transparent).
      - [min_width] is the minimum gutter width in columns. Defaults to [3].
      - [padding_right] is the padding between the number and the content area.
        Defaults to [1].
      - [show_line_numbers] controls whether the gutter is displayed. When
        [false], the gutter is hidden and takes no layout space. Defaults to
        [true].
      - [line_number_offset] is added to each logical line index. Defaults to
        [0]. Use to display line numbers starting from a value other than 1.
      - [line_colors] are per-line background colors, keyed by logical line
        index. Defaults to [[]].
      - [line_signs] are per-line gutter signs, keyed by logical line index.
        Defaults to [[]].
      - [line_numbers] are custom line number overrides, keyed by logical line
        index. When set for a line, the custom number is displayed instead of
        the computed [logical_line + 1 + line_number_offset]. Also affects
        gutter width calculation. Defaults to [[]].
      - [hidden_line_numbers] is the list of logical line indices whose numbers
        are hidden, leaving a blank gutter. Defaults to [[]]. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?fg:Ansi.Color.t ->
  ?bg:Ansi.Color.t ->
  ?min_width:int ->
  ?padding_right:int ->
  ?show_line_numbers:bool ->
  ?line_number_offset:int ->
  ?line_colors:(int * line_color) list ->
  ?line_signs:(int * line_sign) list ->
  ?line_numbers:(int * int) list ->
  ?hidden_line_numbers:int list ->
  unit ->
  t
(** [create ~parent ()] is a line number gutter container attached to [parent].
    Optional parameters have the same defaults as {!Props.make}. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying renderable. *)

(** {1:updating Updating} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces all properties of [t] with [props] and
    schedules a re-render. *)
