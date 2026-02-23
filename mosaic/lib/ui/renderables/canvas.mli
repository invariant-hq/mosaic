(** Mutable cell-level drawing surface.

    A canvas is a {!Renderable.t} backed by a {!Grid.t} that the caller draws
    into directly. The grid is resized to match the node's layout dimensions at
    the start of each render pass. On render the canvas blits its grid to the
    parent at the node's position.

    Use {!draw_text}, {!fill_rect}, {!draw_box}, {!draw_line}, or {!set_cell} to
    draw into the canvas. Drawing changes persist between frames. To draw at
    render time, register a callback with {!set_on_draw}; the callback fires
    after auto-resize each pass. Combine with [~live:true] on the renderable for
    per-frame animation. For one-shot or event-driven drawing, call
    {!request_render} after drawing to schedule a re-render.

    In the declarative Vnode system, use {!Vnode.canvas}. *)

type t
(** The type for canvases. *)

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?respect_alpha:bool ->
  unit ->
  t
(** [create ~parent ()] is a canvas node attached to [parent].

    The internal grid starts at 1x1 and is resized to match the node's layout
    dimensions on the first render pass. The grid shares the parent's glyph pool
    (if available) and uses [`Unicode] width computation.

    Optional parameters:
    - [index] is the child index within [parent]. Defaults to appending at the
      end.
    - [id] is the node identifier. Defaults to [None].
    - [style] is the layout style. Defaults to the empty style.
    - [visible] controls node visibility. Defaults to [true].
    - [z_index] is the stacking order. Defaults to [0].
    - [opacity] is the compositing opacity. Defaults to [1.0].
    - [respect_alpha] controls alpha blending {e within} the canvas grid.
      Defaults to [false]. The compositing step to the parent always respects
      source alpha regardless of this flag.

    See also {!set_on_resize} to redraw content when dimensions change. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable for [t]. *)

(** {1:dimensions Dimensions} *)

val width : t -> int
(** [width t] is the grid width of [t] in cells.

    Starts at [1] and is updated to match layout dimensions at the start of each
    render pass. Inside {!set_on_draw} and {!set_on_resize} callbacks the value
    reflects the current layout dimensions. *)

val height : t -> int
(** [height t] is the grid height of [t] in cells.

    Starts at [1] and is updated to match layout dimensions at the start of each
    render pass. Inside {!set_on_draw} and {!set_on_resize} callbacks the value
    reflects the current layout dimensions. *)

(** {1:drawing Drawing}

    Drawing operations write into the canvas grid. Changes persist between
    frames. Drawing does {b not} auto-schedule a re-render; call
    {!request_render} when done, or use {!set_on_draw} for render-time drawing.
*)

val draw_text :
  ?style:Ansi.Style.t ->
  ?tab_width:int ->
  t ->
  x:int ->
  y:int ->
  text:string ->
  unit
(** [draw_text t ~x ~y ~text] draws [text] as a single line into the canvas grid
    at column [x], row [y].

    See {!Grid.draw_text} for full semantics.

    Optional parameters:
    - [style] is the text style. Defaults to the empty style.
    - [tab_width] is the number of cells per tab stop. Defaults to [8]. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect t ~x ~y ~width ~height ~color] fills the rectangle at column [x],
    row [y] with the given [width], [height], and background [color].

    See {!Grid.fill_rect} for full semantics. *)

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  ?border:Grid.Border.t ->
  ?sides:Grid.Border.side list ->
  ?style:Ansi.Style.t ->
  ?fill:Ansi.Color.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?title_style:Ansi.Style.t ->
  unit ->
  unit
(** [draw_box t ~x ~y ~width ~height ()] draws a Unicode box at column [x], row
    [y] with the given [width] and [height].

    See {!Grid.draw_box} for full semantics.

    Optional parameters:
    - [border] is the border character set. Defaults to the rounded Unicode box
      style.
    - [sides] is the list of sides to draw. Defaults to all four sides.
    - [style] is the border style. Defaults to the empty style.
    - [fill] is the background fill color. Defaults to no fill.
    - [title] is a title string rendered on the top border. Defaults to no
      title.
    - [title_alignment] is the horizontal alignment of [title]. Defaults to
      [`Left].
    - [title_style] is the style applied to [title]. Defaults to the empty
      style. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?glyphs:Grid.line_glyphs ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to [(x2, y2)].

    See {!Grid.draw_line} for full semantics.

    Optional parameters:
    - [style] is the line style. Defaults to the empty style.
    - [glyphs] is the glyph set used for line segments. Defaults to the standard
      box-drawing characters.
    - [kind] is the line rendering mode. [`Line] uses box-drawing characters;
      [`Braille] uses Braille dot patterns. Defaults to [`Line]. *)

val set_cell :
  t ->
  x:int ->
  y:int ->
  glyph:Glyph.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_cell t ~x ~y ~glyph ~fg ~bg ~attrs ()] writes a single cell at column
    [x], row [y].

    See {!Grid.set_cell} for full semantics.

    Optional parameters:
    - [link] is a hyperlink URI attached to the cell. Defaults to no link.
    - [blend] controls whether alpha blending is applied. Defaults to the canvas
      [respect_alpha] setting. *)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear t] clears the canvas grid and schedules a re-render.

    [color] is the background color used to fill the cleared grid. Defaults to
    the default background color.

    See {!Grid.clear} for full semantics. *)

(** {1:grid Low-level grid access} *)

val grid : t -> Grid.t
(** [grid t] is the underlying {!Grid.t} for [t].

    Use this for operations not covered by the canvas drawing functions: scissor
    clipping, cell queries, scrolling, {!Grid.blit_region}, and similar. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles used by the reconciler for
      diffing. *)

  val make : ?respect_alpha:bool -> unit -> t
  (** [make ()] is a property set with the same defaults as {!val-create}.

      Optional parameters:
      - [respect_alpha] controls alpha blending within the canvas grid. Defaults
        to [false]. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t].

    Updates [respect_alpha] on the underlying grid. Layout dimensions are
    managed by the layout system and are not affected by this call. *)

(** {1:callbacks Callbacks} *)

val set_on_draw : t -> (t -> delta:float -> unit) option -> unit
(** [set_on_draw t cb] registers [cb] as the render-time drawing callback for
    [t], replacing any previously registered callback. [None] clears the
    callback.

    [cb] fires each render pass, after the grid has been auto-resized. [~delta]
    is the elapsed time in milliseconds since the last frame. {!width} and
    {!height} reflect the current layout dimensions inside [cb]. Content from
    previous frames persists in the grid; call {!clear} first for full redraws.

    {b Note.} Combine with [~live:true] on the renderable for per-frame
    animation. *)

val set_on_resize : t -> (t -> unit) option -> unit
(** [set_on_resize t cb] registers [cb] as the resize callback for [t],
    replacing any previously registered callback. [None] clears the callback.

    [cb] fires when the canvas grid is resized due to layout changes. The grid
    has already been resized when [cb] is called; use {!width} and {!height} to
    read the new dimensions. When both a resize callback and a draw callback are
    registered, the resize callback is called first in the same render pass. *)

(** {1:render_control Render control} *)

val request_render : t -> unit
(** [request_render t] schedules a re-render of [t].

    Call this after drawing into the canvas outside of a draw callback to make
    changes visible. Not needed after {!clear} (which schedules automatically)
    or inside a {!set_on_draw} callback (which already runs inside a render
    pass). *)

(** {1:properties Properties} *)

val set_respect_alpha : t -> bool -> unit
(** [set_respect_alpha t v] enables or disables alpha blending within the canvas
    grid of [t].

    See {!val-create} for semantics. *)

val respect_alpha : t -> bool
(** [respect_alpha t] is [true] iff alpha blending is enabled within the canvas
    grid of [t]. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a canvas for debugging. *)
