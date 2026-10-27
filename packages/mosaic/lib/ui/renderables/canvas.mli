(** Mutable cell-level drawing surface.

    A canvas is a {!Renderable.t} backed by a {!Matrix_grid.t} that the caller
    draws into directly. The grid is resized to match the node's layout
    dimensions at the start of each render pass. On render the canvas blits its
    grid to the parent at the node's position.

    Use {!draw_text}, {!fill_rect}, {!draw_box}, {!draw_line}, or {!set_uchar}
    to draw into the canvas. Drawing changes persist between frames. To draw at
    render time, register a callback with {!set_on_draw}; the callback fires
    after auto-resize each pass. Combine with [~live:true] on the renderable for
    per-frame animation. For one-shot or event-driven drawing, call
    {!request_render} after drawing to schedule a re-render.

    In a Mosaic view, use [Mosaic.canvas]. *)

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
  unit ->
  t
(** [create ~parent ()] is a canvas node attached to [parent].

    The internal grid starts at 1x1 and is resized to match the node's layout
    dimensions on the first render pass. It uses the width computation method of
    the screen the canvas renders into.

    Optional parameters:
    - [index] is the child index within [parent]. Defaults to appending at the
      end.
    - [id] is the node identifier. Defaults to [None].
    - [style] is the layout style. Defaults to the empty style.
    - [visible] controls node visibility. Defaults to [true].
    - [z_index] is the stacking order. Defaults to [0].
    - [opacity] is the compositing opacity. Defaults to [1.0].

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

(** {1:theming Theme} *)

val theme : t -> Theme.t
(** [theme t] is the theme in scope at [t] ({!Renderable.theme}), with which a
    {!set_on_draw} callback draws by role, as widgets do: a drawing that styles
    its cells with it follows a [themed] around the canvas and the terminal's
    colors, and is drawn again with a theme that changes. *)

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

    See {!Matrix_grid.draw_text} for full semantics and parameter defaults;
    [style] and [tab_width] are passed through unchanged. *)

val fill_rect :
  t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
(** [fill_rect t ~x ~y ~width ~height ~color] fills the rectangle at column [x],
    row [y] with the given [width], [height], and background [color].

    See {!Matrix_grid.fill_rect} for full semantics. *)

val draw_box :
  t ->
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  ?border:Matrix_grid.Border.t ->
  ?sides:Matrix_grid.Border.side list ->
  ?style:Ansi.Style.t ->
  ?fill:Ansi.Color.t ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?title_style:Ansi.Style.t ->
  unit ->
  unit
(** [draw_box t ~x ~y ~width ~height ()] draws a Unicode box at column [x], row
    [y] with the given [width] and [height].

    See {!Matrix_grid.draw_box} for full semantics and parameter defaults; all
    optional parameters are passed through unchanged. *)

val draw_line :
  t ->
  x1:int ->
  y1:int ->
  x2:int ->
  y2:int ->
  ?style:Ansi.Style.t ->
  ?symbols:Matrix_grid.line_symbols ->
  ?kind:[ `Line | `Braille ] ->
  unit ->
  unit
(** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to [(x2, y2)].

    See {!Matrix_grid.draw_line} for full semantics.

    Optional parameters:
    - [style] is the line style. Defaults to the empty style.
    - [symbols] is the symbol set used for line segments. Defaults to the
      standard box-drawing characters.
    - [kind] is the line rendering mode. [`Line] uses box-drawing characters;
      [`Braille] uses Braille dot patterns. Defaults to [`Line]. *)

val set_uchar :
  t ->
  x:int ->
  y:int ->
  uchar:Uchar.t ->
  fg:Ansi.Color.t ->
  bg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  ?link:string ->
  ?blend:bool ->
  unit ->
  unit
(** [set_uchar t ~x ~y ~uchar:cell ~fg ~bg ~attrs ()] writes a single cell at
    column [x], row [y].

    See {!Matrix_grid.set_uchar} for full semantics.

    Optional parameters:
    - [link] is a hyperlink URI attached to the cell. Defaults to no link.
    - [blend] controls whether alpha blending is applied. Defaults to [false].
*)

val clear : ?color:Ansi.Color.t -> t -> unit
(** [clear t] clears the canvas grid and, outside [t]'s draw callback
    ({!set_on_draw}), schedules a re-render. A clear inside the callback shows
    in the frame being drawn, so it schedules nothing.

    [color] is the background color used to fill the cleared grid. Defaults to
    the default background color.

    See {!Matrix_grid.clear} for full semantics. *)

(** {1:grid Low-level grid access} *)

val grid : t -> Matrix_grid.t
(** [grid t] is the underlying {!Matrix_grid.t} for [t].

    Use this for operations not covered by the canvas drawing functions: scissor
    clipping, cell queries, scrolling, {!Matrix_grid.blit_region}, and similar.
*)

(** {1:callbacks Callbacks} *)

val set_on_draw : t -> (t -> dt:float -> unit) option -> unit
(** [set_on_draw t cb] registers [cb] as the render-time drawing callback for
    [t], replacing any previously registered callback. [None] clears the
    callback.

    [cb] fires each render pass, after the grid has been auto-resized. [~dt] is
    the elapsed time in seconds since the last frame. {!width} and {!height}
    reflect the current layout dimensions inside [cb]. Content from previous
    frames persists in the grid; call {!clear} first for full redraws.

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

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a canvas for debugging. *)
