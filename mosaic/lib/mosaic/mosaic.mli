(** TEA (The Elm Architecture) runtime for terminal UIs.

    Mosaic implements the Model-View-Update loop: the application maintains an
    immutable {e model}, produces a {!type-t} describing the UI from that model,
    and reacts to events by running an {e update} function that returns a new
    model together with optional {!Cmd} side-effects. Subscriptions ({!Sub}) let
    the application express ongoing event interests such as timers, key presses,
    and terminal resize events.

    - {{!geometry}Geometry types} and {{!dim_helpers}dimension helpers}
    - {{!layout_enums}Layout enums} and the {{!grid}Grid module}
    - {{!widget_types}Widget types}
    - {{!elements}UI elements}
    - {{!views}Views}, {{!commands}Commands}, and
      {{!subscriptions}Subscriptions}
    - {{!application}Application} and {{!running}Running}

    The entry point is {!val-run}. *)

open Mosaic_ui

(** {1:re_exports Re-exports} *)

module Ansi = Matrix.Ansi
(** Terminal ANSI escape sequences: colors ({!Ansi.Color}), styles
    ({!Ansi.Style}), and attributes ({!Ansi.Attr}). *)

module Border = Matrix.Grid.Border
(** Border character sets for box-drawing. Provides {!Border.single},
    {!Border.rounded}, {!Border.heavy}, {!Border.double}, and {!Border.ascii}.
*)

module Event = Mosaic_ui.Event
(** Input event types for keyboard, mouse, and paste events. *)

module Canvas : sig
  (** Mutable cell-level drawing surface.

      A canvas is passed to the [~on_draw] callback of the {!val-canvas}
      element. Use the drawing functions to render content into the canvas grid.
  *)

  type t = Mosaic_ui.Canvas.t
  (** The type for canvases. *)

  (** {1:canvas_dimensions Dimensions} *)

  val width : t -> int
  (** [width t] is the grid width of [t] in cells. *)

  val height : t -> int
  (** [height t] is the grid height of [t] in cells. *)

  (** {1:drawing Drawing} *)

  val draw_text :
    ?style:Ansi.Style.t ->
    ?tab_width:int ->
    t ->
    x:int ->
    y:int ->
    text:string ->
    unit
  (** [draw_text t ~x ~y ~text] draws [text] as a single line at column [x], row
      [y].

      [style] defaults to {!Ansi.Style.default}. [tab_width] defaults to [8]. *)

  val fill_rect :
    t -> x:int -> y:int -> width:int -> height:int -> color:Ansi.Color.t -> unit
  (** [fill_rect t ~x ~y ~width ~height ~color] fills the rectangle with the
      given background [color]. *)

  val draw_box :
    t ->
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    ?border:Border.t ->
    ?sides:Border.side list ->
    ?style:Ansi.Style.t ->
    ?fill:Ansi.Color.t ->
    ?title:string ->
    ?title_alignment:[ `Left | `Center | `Right ] ->
    ?title_style:Ansi.Style.t ->
    unit ->
    unit
  (** [draw_box t ~x ~y ~width ~height ()] draws a Unicode box at column [x],
      row [y].
      - [border] defaults to {!Border.rounded}.
      - [sides] defaults to all four sides.
      - [style] defaults to {!Ansi.Style.default}.
      - [fill] defaults to no fill.
      - [title] defaults to no title.
      - [title_alignment] defaults to [`Left].
      - [title_style] defaults to {!Ansi.Style.default}. *)

  val draw_line :
    t ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    ?style:Ansi.Style.t ->
    ?glyphs:Matrix.Grid.line_glyphs ->
    ?kind:[ `Line | `Braille ] ->
    unit ->
    unit
  (** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to
      [(x2, y2)].
      - [style] defaults to {!Ansi.Style.default}.
      - [glyphs] defaults to standard box-drawing characters.
      - [kind] defaults to [`Line]. Use [`Braille] for Braille dot patterns. *)

  val set_cell :
    t ->
    x:int ->
    y:int ->
    glyph:Matrix.Glyph.t ->
    fg:Ansi.Color.t ->
    bg:Ansi.Color.t ->
    attrs:Ansi.Attr.t ->
    ?link:string ->
    ?blend:bool ->
    unit ->
    unit
  (** [set_cell t ~x ~y ~glyph ~fg ~bg ~attrs ()] writes a single cell.

      [link] defaults to no hyperlink. [blend] defaults to the canvas
      {!respect_alpha} setting. *)

  val clear : ?color:Ansi.Color.t -> t -> unit
  (** [clear t] clears the canvas grid and schedules a re-render.

      [color] defaults to the terminal default background. *)

  (** {1:grid Low-level grid access} *)

  val grid : t -> Matrix.Grid.t
  (** [grid t] is the underlying grid for [t]. Use this for operations not
      covered by the canvas drawing functions. *)

  (** {1:callbacks Callbacks} *)

  val set_on_draw : t -> (t -> delta:float -> unit) option -> unit
  (** [set_on_draw t cb] registers [cb] as the render-time drawing callback for
      [t]. [cb] fires each render pass after the grid has been auto-resized;
      [~delta] is the elapsed time in seconds since the last frame. Pass [None]
      to clear the callback. *)

  val set_on_resize : t -> (t -> unit) option -> unit
  (** [set_on_resize t cb] registers [cb] as the resize callback for [t]. [cb]
      fires when the canvas grid dimensions change. Pass [None] to clear the
      callback. *)

  (** {1:render_control Render control} *)

  val request_render : t -> unit
  (** [request_render t] schedules a re-render of [t]. Call this after drawing
      outside of a draw callback. *)

  (** {1:properties Properties} *)

  val set_respect_alpha : t -> bool -> unit
  (** [set_respect_alpha t v] enables or disables alpha blending within the
      canvas grid. *)

  val respect_alpha : t -> bool
  (** [respect_alpha t] is [true] iff alpha blending is enabled. *)
end

(** {1:geometry Geometry types} *)

type 'a size = 'a Toffee.Geometry.Size.t = { width : 'a; height : 'a }
(** The type for 2-dimensional sizes. See {!val-size} and {!val-size_wh}. *)

type 'a rect = 'a Toffee.Geometry.Rect.t = {
  left : 'a;
  right : 'a;
  top : 'a;
  bottom : 'a;
}
(** The type for axis-aligned rectangles with four edges. See {!val-padding},
    {!val-margin}, and {!val-inset}. *)

type 'a point = 'a Toffee.Geometry.Point.t = { x : 'a; y : 'a }
(** The type for 2-dimensional points. *)

type 'a line = 'a Toffee.Geometry.Line.t = { start : 'a; end_ : 'a }
(** The type for line segments with start and end values. Used by
    {!Grid.line_range} and {!Grid.span_range} for grid placement. *)

type dimension = Toffee.Style.Dimension.t
(** The type for CSS dimensions: a fixed length, a percentage, or auto. Create
    values with {!val-px}, {!val-pct}, or {!val-auto}. *)

type length_percentage = Toffee.Style.Length_percentage.t
(** The type for CSS length-or-percentage values. Produced by helpers such as
    {!val-padding}, {!val-gap}, and {!val-gap_xy}. *)

type length_percentage_auto = Toffee.Style.Length_percentage_auto.t
(** The type for CSS length-or-percentage-or-auto values. Produced by helpers
    such as {!val-margin}, {!val-margin_xy}, and {!val-inset}. *)

type span = Mosaic_ui.Text_buffer.span = { text : string; style : Ansi.Style.t }
(** A contiguous run of text with a single visual style. Used by {!val-code} for
    syntax highlighting via its [~spans] argument. *)

(** {1:layout_enums Layout enum modules} *)

(** CSS display property controlling box generation and child layout algorithm.
*)
module Display : sig
  type t = Toffee.Style.Display.t =
    | Block  (** Children follow the block layout algorithm. *)
    | Flex  (** Children follow the flexbox layout algorithm. *)
    | Grid  (** Children follow the CSS grid layout algorithm. *)
    | None  (** Node is hidden and generates no boxes. *)
end

(** CSS position property controlling normal flow participation. *)
module Position : sig
  type t = Toffee.Style.Position.t =
    | Relative
        (** The offset is computed relative to the final position given by the
            layout algorithm. *)
    | Absolute
        (** The node is taken out of the normal flow and positioned relative to
            its parent. *)
end

(** CSS box-sizing property. *)
module Box_sizing : sig
  type t = Toffee.Style.Box_sizing.t =
    | Border_box  (** Size styles specify the box's border box. *)
    | Content_box  (** Size styles specify the box's content box. *)
end

(** CSS overflow property. *)
module Overflow : sig
  type t = Toffee.Style.Overflow.t =
    | Visible  (** Overflowing content is displayed beyond the element. *)
    | Clip  (** Overflowing content is clipped at the element boundary. *)
    | Hidden  (** Overflowing content is hidden; no scrolling. *)
    | Scroll  (** Overflowing content is scrollable. *)
end

(** CSS text-align property for block layout. *)
module Text_align : sig
  type t = Toffee.Style.Text_align.t =
    | Auto  (** No special text alignment behavior. *)
    | Legacy_left  (** Left alignment. *)
    | Legacy_right  (** Right alignment. *)
    | Legacy_center  (** Center alignment. *)
end

(** CSS flex-direction property. *)
module Flex_direction : sig
  type t = Toffee.Style.Flex_direction.t =
    | Row  (** Main axis is horizontal, left to right. *)
    | Column  (** Main axis is vertical, top to bottom. *)
    | Row_reverse  (** Main axis is horizontal, right to left. *)
    | Column_reverse  (** Main axis is vertical, bottom to top. *)
end

(** CSS flex-wrap property. *)
module Flex_wrap : sig
  type t = Toffee.Style.Flex_wrap.t =
    | No_wrap  (** Items stay on a single line. *)
    | Wrap  (** Items wrap to multiple lines as needed. *)
    | Wrap_reverse  (** Items wrap to multiple lines in reverse direction. *)
end

(** Alignment along the cross/block axis (align-items, align-self,
    justify-items, justify-self). *)
module Align : sig
  type t = Toffee.Style.Align_items.t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start  (** Pack items toward the flex-relative start. *)
    | Flex_end  (** Pack items toward the flex-relative end. *)
    | Center  (** Pack items along the center of the axis. *)
    | Baseline  (** Align items such that their baselines align. *)
    | Stretch  (** Stretch items to fill the container. *)
end

(** Distribution of space between and around content items (align-content,
    justify-content). *)
module Justify : sig
  type t = Toffee.Style.Align_content.t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start  (** Pack items toward the flex-relative start. *)
    | Flex_end  (** Pack items toward the flex-relative end. *)
    | Center  (** Center items around the middle of the axis. *)
    | Stretch  (** Stretch items to fill the container. *)
    | Space_between  (** Distribute items evenly, flush with edges. *)
    | Space_evenly  (** Distribute items evenly with equal edge spacing. *)
    | Space_around  (** Distribute items evenly with half-size edge gaps. *)
end

(** CSS grid-auto-flow property. *)
module Grid_auto_flow : sig
  type t = Toffee.Style.Grid_auto_flow.t =
    | Row  (** Place items by filling each row in turn. *)
    | Column  (** Place items by filling each column in turn. *)
    | Row_dense  (** Fill rows first using the dense packing algorithm. *)
    | Column_dense  (** Fill columns first using the dense packing algorithm. *)
end

(** {1:grid Grid module} *)

(** Grid layout constructors for defining tracks and placing items.

    {[
      box ~display:Display.Grid
        ~grid_template_columns:[ Grid.length 20.; Grid.fr 1.; Grid.fr 1. ]
        ~grid_template_rows:[ Grid.length 3.; Grid.fr 1.; Grid.length 3. ]
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 1 4)
        [...]
    ]} *)
module Grid : sig
  (** {2 Grid template components}

      Values for [~grid_template_columns] and [~grid_template_rows]. *)

  type template = Toffee.Style.grid_template_component
  (** A grid template component (single track or repeat clause). *)

  val fr : float -> template
  (** [fr n] is a flexible track taking [n] fractional units of remaining space.
  *)

  val length : float -> template
  (** [length n] is a fixed-width track of [n] cells. *)

  val percent : float -> template
  (** [percent n] is a percentage-width track (0.0 to 1.0 range). *)

  val auto : template
  (** [auto] is an auto-sized track. *)

  val min_content : template
  (** [min_content] is a track sized to the minimum content width. *)

  val max_content : template
  (** [max_content] is a track sized to the maximum content width. *)

  val fit_content : Toffee.Style.Compact_length.t -> template
  (** [fit_content limit] is a track clamped between min-content and [limit]. *)

  val minmax :
    min:Toffee.Style.Compact_length.t ->
    max:Toffee.Style.Compact_length.t ->
    template
  (** [minmax ~min ~max] is a track that sizes between [min] and [max]. *)

  (** {2 Grid placement}

      Values for [~grid_row] and [~grid_column]. *)

  type placement = Toffee.Style.grid_placement
  (** A grid placement specification for a single axis endpoint. *)

  val line : int -> placement
  (** [line n] places at grid line [n] (1-indexed, negative from end). *)

  val span : int -> placement
  (** [span n] spans [n] tracks from the opposite endpoint. *)

  val auto_placement : placement
  (** [auto_placement] uses the auto-placement algorithm. *)

  val line_range : int -> int -> placement line
  (** [line_range s e] is a placement from line [s] to line [e]. Shorthand for
      [{ start = line s; end_ = line e }]. *)

  val span_range : int -> int -> placement line
  (** [span_range s n] places starting at line [s] and spanning [n] tracks.
      Shorthand for [{ start = line s; end_ = span n }]. *)

  (** {2 Track sizing functions}

      Values for [~grid_auto_rows] and [~grid_auto_columns]. *)

  type track = Toffee.Style.track_sizing_function
  (** A track sizing function for auto rows/columns. *)

  val track_fr : float -> track
  (** [track_fr n] is a flexible track of [n] fractional units. *)

  val track_length : float -> track
  (** [track_length n] is a fixed-length track of [n] cells. *)

  val track_percent : float -> track
  (** [track_percent n] is a percentage-width track (0.0 to 1.0 range). *)

  val track_auto : track
  (** [track_auto] is an auto-sized track. *)

  val track_min_content : track
  (** [track_min_content] is a track sized to the minimum content width. *)

  val track_max_content : track
  (** [track_max_content] is a track sized to the maximum content width. *)

  (** {2 Grid template areas} *)

  type area = Toffee.Style.grid_template_area
  (** A named grid template area. *)
end

(** {1:widget_types Widget types} *)

(** Companion types for the {!val-select} widget. *)
module Select : sig
  type item = Mosaic_ui.Select.item = {
    label : string;  (** Display text for the item. *)
    description : string option;
        (** Optional secondary text shown below the label. *)
  }
  (** The type for select list entries. *)
end

(** Companion types for the {!val-tab_select} widget. *)
module Tab_select : sig
  type item = Mosaic_ui.Tab_select.item = {
    label : string;  (** Display text for the tab. *)
    description : string;  (** Secondary text shown below the tab. *)
  }
  (** The type for tab entries. *)
end

(** Companion types for the {!val-table} widget. *)
module Table : sig
  type alignment = Mosaic_ui.Table.alignment
  (** Horizontal text alignment: [`Left], [`Right], or [`Center]. Defaults to
      [`Left]. *)

  type width = Mosaic_ui.Table.width
  (** Column width strategy: [`Auto], [`Fixed of int], or [`Fraction of float].
      Defaults to [`Auto]. *)

  type overflow = Mosaic_ui.Table.overflow
  (** Cell content overflow: [`Ellipsis] or [`Wrap]. Defaults to [`Ellipsis]. *)

  type column = Mosaic_ui.Table.column = {
    header : string;  (** Column header text. *)
    width : width;  (** Sizing strategy. *)
    alignment : alignment;  (** Text alignment. *)
    overflow : overflow;  (** Content overflow strategy. *)
    min_width : int option;  (** Minimum column width in cells. *)
    max_width : int option;  (** Maximum column width in cells. *)
  }
  (** The type for column definitions. Use {!val-column} to create values with
      sensible defaults. *)

  val column :
    ?width:width ->
    ?alignment:alignment ->
    ?overflow:overflow ->
    ?min_width:int ->
    ?max_width:int ->
    string ->
    column
  (** [column header] is a column definition with the given [header].

      [width] defaults to [`Auto]. [alignment] defaults to [`Left]. [overflow]
      defaults to [`Ellipsis]. *)

  type cell = Mosaic_ui.Table.cell
  (** The type for table cell content. *)

  val cell : ?style:Ansi.Style.t -> string -> cell
  (** [cell s] is a plain-text cell containing [s].

      [style] defaults to {!Ansi.Style.default}. *)

  val rich : Mosaic_ui.Text.fragment list -> cell
  (** [rich fragments] is a styled cell built from a list of styled text
      fragments. *)
end

(** Companion types for the {!val-tree} widget. *)
module Tree : sig
  type item = Mosaic_ui.Tree.item = {
    label : string;  (** Display text for the node. *)
    children : item list;  (** Child nodes (empty for leaves). *)
  }
  (** The type for tree nodes. *)

  val item : ?children:item list -> string -> item
  (** [item label] is a tree node with [label]. [children] defaults to [[]]. *)
end

(** Companion types for the {!val-spinner} widget. *)
module Spinner : sig
  type frame_set = Mosaic_ui.Spinner.frame_set = {
    frames : string array;  (** Characters cycled as animation frames. *)
    interval : float;  (** Time in seconds between frame advances. *)
  }
  (** The type for animation frame sets. *)

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
end

(** Companion types for the {!val-slider} widget. *)
module Slider : sig
  type orientation = Mosaic_ui.Slider.orientation
  (** The type for track direction: [`Horizontal] or [`Vertical]. *)
end

(** Companion types for the {!val-scroll_bar} widget. *)
module Scroll_bar : sig
  type orientation = Mosaic_ui.Scroll_bar.orientation
  (** The type for scroll bar orientation: [`Vertical] or [`Horizontal]. *)
end

(** Companion types for the {!val-text} and {!val-code} widgets. *)
module Text_surface : sig
  type wrap = Mosaic_ui.Text_surface.wrap
  (** The type for wrapping mode: [`None] (no wrapping), [`Char] (break at any
      character), or [`Word] (break at word boundaries). *)
end

(** Companion types for the {!val-line_number} widget. *)
module Line_number : sig
  type line_color = Mosaic_ui.Line_number.line_color = {
    gutter : Ansi.Color.t;  (** Background color of the gutter. *)
    content : Ansi.Color.t option;
        (** Background color of the content area, if any. *)
  }
  (** The type for per-line background color overrides. *)

  type line_sign = Mosaic_ui.Line_number.line_sign = {
    before : string option;  (** Icon rendered before the number. *)
    after : string option;  (** Icon rendered after the number. *)
    before_color : Ansi.Color.t option;  (** Color of [before]. *)
    after_color : Ansi.Color.t option;  (** Color of [after]. *)
  }
  (** The type for gutter sign decorations. *)
end

(** Companion types for the {!val-markdown} widget. *)
module Markdown : sig
  type style_key = Mosaic_ui.Markdown.style_key =
    | Default
    | Heading of int
    | Emphasis
    | Strong
    | Code_span
    | Code_block
    | Link
    | Image
    | Blockquote
    | Thematic_break
    | List_marker
    | Strikethrough
    | Task_marker
    | Table_border
    | Conceal_punctuation
        (** Style keys identifying markdown element classes. *)

  type style = Mosaic_ui.Markdown.style
  (** Style resolver: maps a {!style_key} to an {!Ansi.Style.t}. *)

  val default_style : style
  (** The built-in terminal style resolver. *)
end

(** Syntax themes: maps capture-group names to terminal styles. *)
module Syntax_theme : sig
  type t = Mosaic_ui.Syntax_theme.t
  (** The type for syntax themes. *)

  val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  (** [make ~base mappings] is a theme with [base] as the default style. *)

  val default : t
  (** The built-in dark theme. *)

  val resolve_overlay : t -> string -> Ansi.Style.t
  (** [resolve_overlay theme group] is the raw overlay style for [group]. *)

  val resolve : t -> string -> Ansi.Style.t
  (** [resolve theme group] is the complete style for [group]: overlay merged on
      top of the base style. *)

  val apply : t -> content:string -> (int * int * string) list -> span list
  (** [apply theme ~content ranges] is the list of styled spans for [content]
      under [theme]. *)
end

(** {1:views Views} *)

type 'msg t = 'msg option Vnode.t
(** The type for view nodes parameterized by message type ['msg]. Event handlers
    embedded in a node return ['msg option]: [Some msg] dispatches the message
    into the update loop; [None] ignores the event.

    Build values of this type with the element constructors ({!val-box},
    {!val-text}, {!val-input}, etc.) and compose them with {!val-fragment}. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f view] is [view] with every message transformed by [f].

    Use [map] to embed a child component whose message type differs from the
    parent's. *)

(** {1:commands Commands} *)

(** Side-effects issued by the application.

    Commands are values returned alongside a new model from {!app.init} and
    {!app.update}. The runtime executes them after each update cycle. Compose
    multiple commands with {!Cmd.val-batch}. *)
module Cmd : sig
  (** The type for commands. *)
  type 'msg t =
    | None  (** No command. Equivalent to [batch []]. *)
    | Batch of 'msg t list
        (** Execute all commands in the list. Order is not guaranteed. *)
    | Perform of (('msg -> unit) -> unit)
        (** Execute an arbitrary side-effecting function. The function receives
            a {e dispatch} callback and may call it zero or more times, from any
            thread. See {!val-perform}. *)
    | Quit  (** Request orderly termination of the application. *)
    | Set_title of string
        (** Set the terminal window title to the given string. *)
    | Focus of string
        (** Move keyboard focus to the element identified by the given [id]. Has
            no effect if no element carries that [id]. *)
    | Static_commit of 'msg option Vnode.t
        (** Render a vnode snapshot and write it to the static area with ANSI
            styling preserved. The row count is computed automatically from the
            rendered grid. *)
    | Static_clear  (** Clear all previously written static content. *)

  val none : 'msg t
  (** [none] is the empty command. Produces no side-effects. *)

  val batch : 'msg t list -> 'msg t
  (** [batch cmds] is a command that executes every command in [cmds]. Execution
      order among the commands is not specified. *)

  val perform : (('msg -> unit) -> unit) -> 'msg t
  (** [perform f] is a command that calls [f dispatch] asynchronously.

      [f] may call [dispatch msg] any number of times; each call enqueues [msg]
      for the next update cycle. By default the runtime runs [f] on a fresh
      native thread so that long-running operations never block the UI loop.
      This behaviour can be overridden with the [process_perform] argument of
      {!val-run}. *)

  val quit : 'msg t
  (** [quit] requests orderly application termination after the current update
      cycle completes. *)

  val set_title : string -> 'msg t
  (** [set_title s] sets the terminal window title to [s]. *)

  val focus : string -> 'msg t
  (** [focus id] moves keyboard focus to the element whose [id] attribute equals
      [id]. Has no effect when no matching element exists. *)

  val static_commit : 'msg option Vnode.t -> 'msg t
  (** [static_commit view] renders [view] offscreen at the current terminal
      width and appends the styled result to static output. *)

  val static_clear : 'msg t
  (** [static_clear] removes all previously emitted static content. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f cmd] is [cmd] with every dispatched message transformed by [f]. Use
      this to embed child-component commands in a parent command. *)
end

(** {1:subscriptions Subscriptions} *)

(** Ongoing event interests declared by the application.

    The {!app.subscriptions} function is called after every update and returns a
    subscription value describing which events the application wants to receive.
    Subscriptions are re-evaluated on each cycle, so they may depend on the
    current model. Compose multiple subscriptions with {!Sub.val-batch}. *)
module Sub : sig
  (** The type for subscriptions. *)
  type 'msg t =
    | None  (** No subscription. Equivalent to [batch []]. *)
    | Batch of 'msg t list  (** Activate all subscriptions in the list. *)
    | Every of float * (unit -> 'msg)
        (** [Every (interval, f)] fires [f ()] repeatedly at approximately
            [interval]-second intervals. *)
    | On_tick of (dt:float -> 'msg)
        (** [On_tick f] fires [f ~dt] on every render frame, where [dt] is the
            elapsed time in seconds since the previous frame. *)
    | On_key of (Event.key -> 'msg option)
        (** [On_key f] delivers key events only to the currently focused
            element. [f] returns [None] to ignore an event. *)
    | On_key_all of (Event.key -> 'msg option)
        (** [On_key_all f] delivers all key events regardless of focus. [f]
            returns [None] to ignore an event. *)
    | On_mouse of (Event.mouse -> 'msg option)
        (** [On_mouse f] delivers mouse events only to the currently focused
            element. *)
    | On_mouse_all of (Event.mouse -> 'msg option)
        (** [On_mouse_all f] delivers all mouse events regardless of focus. *)
    | On_paste of (Event.paste -> 'msg option)
        (** [On_paste f] delivers paste events only to the currently focused
            element. *)
    | On_paste_all of (Event.paste -> 'msg option)
        (** [On_paste_all f] delivers all paste events regardless of focus. *)
    | On_resize of (width:int -> height:int -> 'msg)
        (** [On_resize f] fires [f ~width ~height] whenever the terminal is
            resized. *)
    | On_focus of 'msg
        (** [On_focus msg] dispatches [msg] when the terminal window gains
            focus. *)
    | On_blur of 'msg
        (** [On_blur msg] dispatches [msg] when the terminal window loses focus.
        *)

  val none : 'msg t
  (** [none] is the empty subscription. Produces no events. *)

  val batch : 'msg t list -> 'msg t
  (** [batch subs] is a subscription that activates every subscription in
      [subs]. *)

  val every : float -> (unit -> 'msg) -> 'msg t
  (** [every interval f] fires [f ()] at approximately [interval]-second
      intervals for as long as the subscription is active. *)

  val on_tick : (dt:float -> 'msg) -> 'msg t
  (** [on_tick f] fires [f ~dt] on every rendered frame. [dt] is the elapsed
      time in seconds since the previous frame. Use this for animations that
      must advance at the display frame rate. *)

  val on_key : (Event.key -> 'msg option) -> 'msg t
  (** [on_key f] delivers key events to [f] for the currently focused element.
      [f] returns [None] to ignore an event without dispatching. See also
      {!val-on_key_all}. *)

  val on_key_all : (Event.key -> 'msg option) -> 'msg t
  (** [on_key_all f] is like {!val-on_key} but delivers all key events
      regardless of which element has focus. *)

  val on_mouse : (Event.mouse -> 'msg option) -> 'msg t
  (** [on_mouse f] delivers mouse events to [f] for the currently focused
      element. See also {!val-on_mouse_all}. *)

  val on_mouse_all : (Event.mouse -> 'msg option) -> 'msg t
  (** [on_mouse_all f] is like {!val-on_mouse} but delivers all mouse events
      regardless of focus. *)

  val on_paste : (Event.paste -> 'msg option) -> 'msg t
  (** [on_paste f] delivers paste events to [f] for the currently focused
      element. See also {!val-on_paste_all}. *)

  val on_paste_all : (Event.paste -> 'msg option) -> 'msg t
  (** [on_paste_all f] is like {!val-on_paste} but delivers all paste events
      regardless of focus. *)

  val on_resize : (width:int -> height:int -> 'msg) -> 'msg t
  (** [on_resize f] fires [f ~width ~height] whenever the terminal is resized,
      reporting the new dimensions in columns and rows. *)

  val on_focus : 'msg -> 'msg t
  (** [on_focus msg] dispatches [msg] when the terminal window gains focus. *)

  val on_blur : 'msg -> 'msg t
  (** [on_blur msg] dispatches [msg] when the terminal window loses focus. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f sub] is [sub] with every produced message transformed by [f]. Use
      this to embed child-component subscriptions in a parent subscription. *)
end

(** {1:application Application} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
      (** [init ()] is the initial model and any startup commands. Called once
          before the first render. *)
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
      (** [update msg model] is the new model and any side-effects produced in
          response to [msg]. Called once per dispatched message. *)
  view : 'model -> 'msg t;
      (** [view model] is the UI description for [model]. Called on every render
          cycle when the model has changed. *)
  subscriptions : 'model -> 'msg Sub.t;
      (** [subscriptions model] is the set of events the application wants to
          receive given [model]. Re-evaluated after every update. *)
}
(** The type for a Mosaic application. Provide a value of this type to
    {!val-run}. *)

(** {1:running Running} *)

val run :
  ?matrix:Matrix.app ->
  ?process_perform:((unit -> unit) -> unit) ->
  ('model, 'msg) app ->
  unit
(** [run ?matrix ?process_perform app] starts [app] and blocks until the
    application exits (via {!Cmd.val-quit} or an OS signal).

    - [matrix] -- the low-level terminal backend. Defaults to a backend with
      [target_fps = Some 60.] and a hidden cursor.
    - [process_perform] -- controls how {!Cmd.Perform} callbacks are executed.
      Receives a thunk that wraps the user callback with the dispatch function
      already wired in. The default spawns a native [Thread] per callback so
      that long-running operations (e.g. HTTP requests) never block the UI loop.

    {b Eio integration.} Pass a [matrix] and [process_perform] built with the
    [matrix_eio] library:

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let matrix =
        Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:env#stdin
          ~stdout:env#stdout ()
      in
      let process_perform thunk =
        Eio.Fiber.fork_daemon ~sw (fun () ->
            thunk ();
            `Stop_daemon)
      in
      Mosaic.run ~matrix ~process_perform { init; update; view; subscriptions }
    ]}

    Daemon fibers are cancelled when the switch completes, so long-running
    perform operations do not block application shutdown. *)

(** {1:dim_helpers Dimension helpers} *)

val px : int -> dimension
(** [px n] is a dimension of exactly [n] terminal columns or rows. *)

val pct : int -> dimension
(** [pct n] is a dimension of [n] percent of the parent's available space. *)

val auto : dimension
(** [auto] is the automatic dimension, letting the layout engine determine the
    size. *)

val size : width:int -> height:int -> dimension size
(** [size ~width ~height] is a fixed size with the given [width] and [height]
    measured in terminal cells. *)

val size_wh : dimension -> dimension -> dimension size
(** [size_wh w h] is a size with the given [w] width and [h] height dimensions.
*)

val gap : int -> length_percentage size
(** [gap n] is a uniform gap of [n] cells applied to both axes of a flex or grid
    container. *)

val gap_xy : int -> int -> length_percentage size
(** [gap_xy x y] is a gap of [x] cells on the horizontal axis and [y] cells on
    the vertical axis. *)

val padding : int -> length_percentage rect
(** [padding n] is a uniform padding of [n] cells on all four sides. *)

val padding_xy : int -> int -> length_percentage rect
(** [padding_xy x y] is padding of [x] cells on the left and right sides and [y]
    cells on the top and bottom sides. *)

val padding_lrtb : int -> int -> int -> int -> length_percentage rect
(** [padding_lrtb l r t b] is padding of [l] cells on the left, [r] on the
    right, [t] on the top, and [b] on the bottom. *)

val margin : int -> length_percentage_auto rect
(** [margin n] is a uniform margin of [n] cells on all four sides. *)

val margin_xy : int -> int -> length_percentage_auto rect
(** [margin_xy x y] is margin of [x] cells on the left and right sides and [y]
    cells on the top and bottom sides. *)

val margin_lrtb : int -> int -> int -> int -> length_percentage_auto rect
(** [margin_lrtb l r t b] is margin of [l] cells on the left, [r] on the right,
    [t] on the top, and [b] on the bottom. *)

val inset : int -> length_percentage_auto rect
(** [inset n] is a uniform inset of [n] cells on all four sides, used with
    [position = `Absolute] or [position = `Fixed]. *)

val inset_lrtb : int -> int -> int -> int -> length_percentage_auto rect
(** [inset_lrtb l r t b] is inset of [l] cells on the left, [r] on the right,
    [t] on the top, and [b] on the bottom. *)

(** {1:elements UI elements}

    Every element constructor accepts a large set of optional layout and styling
    arguments that mirror CSS flexbox and grid properties. Arguments shared
    across all elements are listed once here; only element-specific arguments
    are documented on each constructor.

    {2:layout_args Common layout arguments}

    - [key] -- reconciler identity hint for list items.
    - [id] -- unique identifier used by {!Cmd.val-focus}.
    - [display] -- layout mode ({!Display.Flex}, {!Display.Grid},
      {!Display.Block}, ...).
    - [box_sizing] -- whether [size] includes padding and border.
    - [position] -- positioning scheme ({!Position.Relative},
      {!Position.Absolute}).
    - [overflow] -- how overflowing content is handled per axis.
    - [scrollbar_width] -- width reserved for overflow scrollbars.
    - [text_align] -- text alignment within the element.
    - [inset] -- position offsets for absolutely-positioned elements.
    - [flex_direction], [flex_wrap], [justify_content], [align_items],
      [align_content], [align_self], [flex_grow], [flex_shrink], [flex_basis] --
      flexbox properties.
    - [justify_items], [justify_self] -- grid alignment properties.
    - [size], [min_size], [max_size] -- element dimensions.
    - [aspect_ratio] -- width-to-height ratio constraint.
    - [gap] -- spacing between flex or grid children.
    - [padding] -- inner spacing between border and content.
    - [margin] -- outer spacing outside the border.
    - [border_width] -- widths of each border side in cells.
    - [grid_template_rows], [grid_template_columns], [grid_auto_rows],
      [grid_auto_columns], [grid_auto_flow], [grid_template_areas],
      [grid_template_column_names], [grid_template_row_names], [grid_row],
      [grid_column] -- CSS grid properties.
    - [visible] -- when [false] the element takes no space and is not rendered.
      Defaults to [true].
    - [z_index] -- stacking order for overlapping elements.
    - [opacity] -- alpha in \[[0.];[1.]\]; [1.] is fully opaque.
    - [focusable] -- when [true] the element can receive keyboard focus.
      Defaults to [false].
    - [autofocus] -- when [true] the element receives focus on mount. Defaults
      to [false].
    - [buffered] -- when [true] renders to an off-screen buffer. Defaults to
      [false].
    - [live] -- when [true] the element re-renders every frame even when the
      model has not changed. Defaults to [false].
    - [ref] -- callback invoked with the rendered {!Mosaic_ui.Renderable.t}
      after each frame.
    - [on_mouse] -- mouse event handler for this element.
    - [on_key] -- key event handler for this element.
    - [on_paste] -- paste event handler for this element. *)

val empty : 'msg t
(** [empty] is the empty view that renders nothing and occupies no space. Useful
    as a placeholder. *)

val fragment : 'msg t list -> 'msg t
(** [fragment views] groups [views] into a single view node without introducing
    a wrapping container element. Layout is applied to each child independently
    within the parent. *)

val embed : Mosaic_ui.Renderable.t -> 'msg t
(** [embed r] wraps a pre-rendered {!Mosaic_ui.Renderable.t} as a view node. Use
    this to integrate non-TEA rendering into the view tree. *)

val box :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  'msg t list ->
  'msg t
(** [box children] is a generic container element that lays out [children]
    according to the common layout arguments (see {!elements}).

    Box-specific optional arguments:
    - [border] -- when [true] draws a border using [border_style]. Defaults to
      [false].
    - [border_style] -- the border character set. Defaults to {!Border.single}.
    - [border_sides] -- which sides to draw. Defaults to all four.
    - [border_color] -- color of the border when unfocused. Defaults to white.
    - [focused_border_color] -- color of the border when the element has focus.
      Defaults to bright cyan.
    - [background] -- background fill color. Defaults to transparent.
    - [fill] -- when [true] fills the background over the entire allocated area.
      Defaults to [true].
    - [title] -- text drawn in the top border. Has no effect when [border] is
      [false].
    - [title_alignment] -- alignment of [title] within the top border. Defaults
      to [`Left]. *)

val text :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?style:Ansi.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  string ->
  'msg t
(** [text s] is a text element that renders the string [s].

    Text-specific optional arguments:
    - [style] -- ANSI style applied to the whole element. Defaults to
      {!Ansi.Style.default}.
    - [text_style] -- ANSI style applied to the text content only, composing
      with [style]. Defaults to {!Ansi.Style.default}.
    - [wrap] -- line-wrapping mode. Defaults to [`None].
    - [selectable] -- when [true] the user can select text with the mouse.
      Defaults to [true].
    - [selection_bg] -- background color of selected text. Defaults to the
      terminal default.
    - [selection_fg] -- foreground color of selected text. Defaults to the
      terminal default.
    - [tab_width] -- number of cells a tab character expands to. Defaults to
      [2].
    - [truncate] -- when [true] clips content that overflows the allocated area
      instead of wrapping. Defaults to [false]. *)

val slider :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?orientation:Slider.orientation ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?on_value_change:(float -> 'msg option) ->
  unit ->
  'msg t
(** [slider ()] is an interactive range slider.

    Slider-specific optional arguments:
    - [orientation] -- [`Horizontal] or [`Vertical]. Defaults to [`Horizontal].
    - [value] -- current thumb position. Defaults to [0.].
    - [min] -- minimum value of the range. Defaults to [0.].
    - [max] -- maximum value of the range. Defaults to [1.].
    - [viewport_size] -- size of the visible viewport relative to the total
      range; used to size the thumb proportionally.
    - [track_color] -- color of the slider track.
    - [thumb_color] -- color of the slider thumb.
    - [on_value_change] -- callback fired when the value changes; receives the
      new value and returns an optional message. *)

val input :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?value:string ->
  ?cursor:int ->
  ?selection:(int * int) option ->
  ?placeholder:string ->
  ?max_length:int ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?on_input:(string -> 'msg option) ->
  ?on_change:(string -> 'msg option) ->
  ?on_submit:(string -> 'msg option) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> 'msg option) ->
  unit ->
  'msg t
(** [input ()] is a single-line text input field.

    See {!val-textarea} for the multi-line variant.

    Input-specific optional arguments:
    - [value] -- current content of the field. Defaults to [""].
    - [cursor] -- optional controlled cursor grapheme offset.
    - [selection] -- optional controlled selection range.
    - [placeholder] -- hint text shown when the field is empty. Defaults to
      [""].
    - [max_length] -- maximum number of characters accepted. Defaults to [1000].
    - [text_color] -- foreground color of the text. Defaults to white.
    - [background_color] -- background color of the field. Defaults to the
      terminal default.
    - [focused_text_color] -- foreground color when focused. Defaults to white.
    - [focused_background_color] -- background color when focused. Defaults to
      the terminal default.
    - [placeholder_color] -- color of the placeholder text. Defaults to bright
      black (dark gray).
    - [selection_color] -- background color of selected text. Defaults to blue.
    - [selection_fg] -- foreground color of selected text. Defaults to the
      terminal default.
    - [cursor_style] -- cursor shape: [`Block], [`Line], or [`Underline].
      Defaults to [`Block].
    - [cursor_color] -- color of the cursor. Defaults to white.
    - [cursor_blinking] -- when [true] the cursor blinks. Defaults to [true].
    - [on_input] -- fired on every keystroke; receives the full current value
      after the change.
    - [on_change] -- fired when the value changes after editing; semantics may
      differ from [on_input] depending on the component.
    - [on_submit] -- fired when the user presses Enter; receives the current
      value.
    - [on_cursor] -- fired when cursor position or selection changes. *)

val select :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?selected_index:int ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?description_color:Ansi.Color.t ->
  ?selected_description_color:Ansi.Color.t ->
  ?show_description:bool ->
  ?show_scroll_indicator:bool ->
  ?wrap_selection:bool ->
  ?item_spacing:int ->
  ?fast_scroll_step:int ->
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  Select.item list ->
  'msg t
(** [select items] is a vertical list from which the user can choose one item.

    See {!val-tab_select} for a horizontal tab-bar variant.

    Select-specific optional arguments:
    - [selected_index] -- zero-based index of the highlighted item. Defaults to
      [0].
    - [background] -- background color of unselected items.
    - [text_color] -- foreground color of unselected items.
    - [focused_background] -- background when the widget has focus.
    - [focused_text_color] -- foreground when the widget has focus.
    - [selected_background] -- background of the selected item.
    - [selected_text_color] -- foreground of the selected item.
    - [description_color] -- color of item description text.
    - [selected_description_color] -- description color for the selected item.
    - [show_description] -- when [true] shows item descriptions. Defaults to
      [true].
    - [show_scroll_indicator] -- when [true] shows a scroll indicator when the
      list overflows. Defaults to [false].
    - [wrap_selection] -- when [true] wraps the selection from the last item
      back to the first and vice versa. Defaults to [false].
    - [item_spacing] -- number of blank lines between items. Defaults to [0].
    - [fast_scroll_step] -- number of items skipped per fast-scroll action (e.g.
      Page Down). Defaults to [5].
    - [on_change] -- fired when the highlighted index changes; receives the new
      index.
    - [on_activate] -- fired when the user confirms the selection (e.g. by
      pressing Enter); receives the confirmed index. *)

val tab_select :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?selected:int ->
  ?tab_width:int ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?focused_background:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?description_color:Ansi.Color.t ->
  ?selected_description_color:Ansi.Color.t ->
  ?show_underline:bool ->
  ?show_description:bool ->
  ?show_scroll_arrows:bool ->
  ?wrap_selection:bool ->
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  Tab_select.item list ->
  'msg t
(** [tab_select items] is a horizontal tab-bar selector.

    See {!val-select} for the vertical list variant.

    Tab-select-specific optional arguments:
    - [selected] -- zero-based index of the active tab. Defaults to [0].
    - [tab_width] -- fixed width for each tab in cells. Defaults to [12].
    - [background] -- background color of inactive tabs.
    - [text_color] -- foreground color of inactive tabs.
    - [focused_background] -- background when the widget has focus.
    - [focused_text_color] -- foreground when the widget has focus.
    - [selected_background] -- background of the active tab.
    - [selected_text_color] -- foreground of the active tab.
    - [description_color] -- color of tab description text.
    - [selected_description_color] -- description color for the active tab.
    - [show_underline] -- when [true] draws an underline below tabs. Defaults to
      [true].
    - [show_description] -- when [true] shows tab descriptions. Defaults to
      [false].
    - [show_scroll_arrows] -- when [true] shows arrows when tabs overflow the
      available width. Defaults to [true].
    - [wrap_selection] -- when [true] wraps from the last tab to the first and
      vice versa. Defaults to [false].
    - [on_change] -- fired when the active tab index changes.
    - [on_activate] -- fired when the user confirms the tab selection. *)

val canvas :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?respect_alpha:bool ->
  (Canvas.t -> delta:float -> unit) ->
  'msg t
(** [canvas draw] is a free-form drawing surface. [draw c ~delta] is called on
    every frame with a fresh {!Canvas.t} [c] and the elapsed time [delta] in
    seconds since the previous frame. The canvas fills its allocated layout
    area.

    Canvas-specific optional arguments:
    - [respect_alpha] -- when [true] the canvas composites with alpha blending.
      Defaults to [true]. *)

val spinner :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?frame_set:Spinner.frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  'msg t
(** [spinner ()] is an animated activity indicator.

    Spinner-specific optional arguments:
    - [frame_set] -- the animation frame sequence. Defaults to {!Spinner.dots}.
    - [color] -- foreground color of the spinner character. Defaults to white.
*)

val progress_bar :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?filled_color:Ansi.Color.t ->
  ?empty_color:Ansi.Color.t ->
  unit ->
  'msg t
(** [progress_bar ()] is a read-only progress indicator.

    Progress-bar-specific optional arguments:
    - [value] -- current progress value. Defaults to [0.].
    - [min] -- value representing 0% progress. Defaults to [0.].
    - [max] -- value representing 100% progress. Defaults to [1.].
    - [orientation] -- [`Horizontal] or [`Vertical]. Defaults to [`Horizontal].
    - [filled_color] -- color of the filled portion of the bar. Defaults to
      medium gray.
    - [empty_color] -- color of the unfilled portion of the bar. Defaults to
      dark gray. *)

val scroll_bar :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?orientation:Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  ?on_change:(int -> 'msg option) ->
  unit ->
  'msg t
(** [scroll_bar ()] is a standalone scroll-bar widget. Use this when you manage
    scroll state externally; for automatic scrolling consider {!val-scroll_box}
    instead.

    Scroll-bar-specific optional arguments:
    - [orientation] -- [`Horizontal] or [`Vertical]. Defaults to [`Vertical].
    - [show_arrows] -- when [true] renders arrow buttons at each end. Defaults
      to [false].
    - [track_color] -- color of the scroll track. Defaults to dark gray.
    - [thumb_color] -- color of the scroll thumb. Defaults to medium gray.
    - [arrow_fg] -- foreground color of the arrow buttons. Defaults to white.
    - [arrow_bg] -- background color of the arrow buttons. Defaults to the
      terminal default.
    - [on_change] -- fired when the scroll position changes; receives the new
      position in scroll units. *)

val scroll_box :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?sticky_scroll:bool ->
  ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
  ?background:Ansi.Color.t ->
  ?on_scroll:(x:int -> y:int -> 'msg option) ->
  'msg t list ->
  'msg t
(** [scroll_box children] is a scrollable container. It clips its [children] to
    the allocated area and manages scroll state internally.

    Scroll-box-specific optional arguments:
    - [scroll_x] -- when [true] enables horizontal scrolling. Defaults to
      [false].
    - [scroll_y] -- when [true] enables vertical scrolling. Defaults to [true].
    - [sticky_scroll] -- when [true] the viewport follows new content appended
      at the sticky edge. Defaults to [false].
    - [sticky_start] -- the edge to which sticky scrolling anchors. Defaults to
      [`Bottom].
    - [background] -- background fill color of the scroll area.
    - [on_scroll] -- fired after a scroll event; receives the new scroll
      position [~x] and [~y] in cells. *)

val textarea :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?value:string ->
  ?cursor:int ->
  ?selection:(int * int) option ->
  ?spans:span list ->
  ?ghost_text:string ->
  ?ghost_text_color:Ansi.Color.t ->
  ?placeholder:string ->
  ?wrap:Text_surface.wrap ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?on_input:(string -> 'msg option) ->
  ?on_change:(string -> 'msg option) ->
  ?on_submit:(string -> 'msg option) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> 'msg option) ->
  unit ->
  'msg t
(** [textarea ()] is a multi-line text editing area. It shares its optional
    argument set with {!val-input}; see that entry for argument descriptions.

    Textarea-specific optional arguments not present on {!val-input}:
    - [cursor] -- optional controlled cursor grapheme offset.
    - [selection] -- optional controlled selection range.
    - [spans] -- optional styled spans used for syntax highlighting. When
      provided, the span text must match [value].
    - [ghost_text] -- optional inline ghost completion rendered at the cursor.
    - [ghost_text_color] -- color used for [ghost_text].
    - [wrap] -- line-wrapping mode within the editing area. Defaults to the
      surface default.
    - [on_cursor] -- fired when cursor position or selection changes. *)

val code :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?spans:span list ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  string ->
  'msg t
(** [code s] is a read-only code display element that renders [s] with optional
    syntax highlighting spans.

    Wrap with {!val-line_number} to add a gutter with line numbers.

    Code-specific optional arguments:
    - [spans] -- list of {!type-span} values that apply syntax-highlighting
      styles to ranges of the text.
    - [text_style] -- base ANSI style applied to unstyled text.
    - [wrap] -- line-wrapping mode. Defaults to no wrap.
    - [tab_width] -- number of cells a tab character expands to. Defaults to
      [4].
    - [selectable] -- when [true] the user can select text with the mouse.
      Defaults to [true].
    - [selection_bg] -- background color of selected text.
    - [selection_fg] -- foreground color of selected text.
    - [on_selection] -- fired when the current selection changes. *)

val line_number :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?fg:Ansi.Color.t ->
  ?bg:Ansi.Color.t ->
  ?min_width:int ->
  ?padding_right:int ->
  ?show_line_numbers:bool ->
  ?line_number_offset:int ->
  ?line_colors:(int * Line_number.line_color) list ->
  ?line_signs:(int * Line_number.line_sign) list ->
  ?hidden_line_numbers:int list ->
  'msg t ->
  'msg t
(** [line_number child] wraps [child] with a gutter that displays line numbers
    alongside each line of [child]'s content. Commonly used with {!val-code} or
    {!val-textarea} as the child.

    Line-number-specific optional arguments:
    - [fg] -- foreground color of the line-number gutter.
    - [bg] -- background color of the line-number gutter.
    - [min_width] -- minimum width of the gutter in cells. Widens automatically
      as the line count grows. Defaults to [3].
    - [padding_right] -- cells of padding between the gutter and the content.
      Defaults to [1].
    - [show_line_numbers] -- when [false] hides the numeric labels but keeps the
      gutter structure. Defaults to [true].
    - [line_number_offset] -- added to each displayed line number. Useful when
      the content is a slice of a larger document. Defaults to [0].
    - [line_colors] -- per-line foreground color overrides, given as
      [(line_index, color)] pairs.
    - [line_signs] -- per-line sign icons shown in the gutter, given as
      [(line_index, sign)] pairs.
    - [hidden_line_numbers] -- line indices whose number labels are suppressed.
*)

val markdown :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?md_style:Markdown.style ->
  ?conceal:bool ->
  ?streaming:bool ->
  string ->
  'msg t
(** [markdown s] is a rendered Markdown view of the string [s].

    Markdown-specific optional arguments:
    - [md_style] -- theme controlling heading, code, and emphasis colors.
      Defaults to the built-in style.
    - [conceal] -- when [true] hides Markdown syntax characters (asterisks,
      backticks, etc.) from the rendered output. Defaults to [false].
    - [streaming] -- when [true] the renderer tolerates incomplete Markdown
      (e.g. an unclosed code fence) as it arrives from a streaming source.
      Defaults to [false]. *)

val table :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?columns:Table.column list ->
  ?rows:Table.cell array list ->
  ?selected_row:int ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?show_header:bool ->
  ?show_column_separator:bool ->
  ?show_row_separator:bool ->
  ?cell_padding:int ->
  ?header_color:Ansi.Color.t ->
  ?header_background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?focused_selected_text_color:Ansi.Color.t ->
  ?focused_selected_background:Ansi.Color.t ->
  ?row_styles:Ansi.Style.t list ->
  ?wrap_selection:bool ->
  ?fast_scroll_step:int ->
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  unit ->
  'msg t
(** [table ()] is a scrollable data table.

    Table-specific optional arguments:
    - [columns] -- column definitions ({!Table.column}), including headers and
      sizing. Defaults to [[]].
    - [rows] -- table data as a list of cell arrays. Each array must have the
      same length as [columns]. Defaults to [[]].
    - [selected_row] -- zero-based index of the highlighted row. Defaults to
      [0].
    - [border] -- when [true] draws an outer border. Defaults to [true].
    - [border_style] -- character set for the outer border. Defaults to
      {!Border.single}.
    - [show_header] -- when [true] renders a header row. Defaults to [true].
    - [show_column_separator] -- when [true] draws vertical lines between
      columns. Defaults to [false].
    - [show_row_separator] -- when [true] draws horizontal lines between rows.
      Defaults to [false].
    - [cell_padding] -- horizontal padding in cells within each cell. Defaults
      to [0].
    - [header_color] -- foreground color of the header row.
    - [header_background] -- background color of the header row.
    - [text_color] -- foreground color of body cells.
    - [background] -- background color of body cells.
    - [selected_text_color] -- foreground of the selected row.
    - [selected_background] -- background of the selected row.
    - [focused_selected_text_color] -- foreground of the selected row when the
      table has focus.
    - [focused_selected_background] -- background of the selected row when the
      table has focus.
    - [row_styles] -- repeating list of styles applied to body rows, useful for
      alternating row colors.
    - [wrap_selection] -- when [true] wraps selection past the last or first
      row. Defaults to [false].
    - [fast_scroll_step] -- rows skipped per fast-scroll action. Defaults to
      [5].
    - [on_change] -- fired when the selected row index changes.
    - [on_activate] -- fired when the user confirms the selected row (e.g. by
      pressing Enter). *)

val tree :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?items:Tree.item list ->
  ?selected_index:int ->
  ?expand_depth:int ->
  ?indent_size:int ->
  ?show_guides:bool ->
  ?guide_style:Border.t ->
  ?expand_icon:string ->
  ?collapse_icon:string ->
  ?leaf_icon:string ->
  ?background:Ansi.Color.t ->
  ?text_color:Ansi.Color.t ->
  ?selected_background:Ansi.Color.t ->
  ?selected_text_color:Ansi.Color.t ->
  ?focused_selected_background:Ansi.Color.t ->
  ?focused_selected_text_color:Ansi.Color.t ->
  ?guide_color:Ansi.Color.t ->
  ?icon_color:Ansi.Color.t ->
  ?wrap_selection:bool ->
  ?fast_scroll_step:int ->
  ?on_change:(int -> 'msg option) ->
  ?on_activate:(int -> 'msg option) ->
  ?on_expand:(int -> bool -> 'msg option) ->
  unit ->
  'msg t
(** [tree ()] is an interactive collapsible tree view.

    Tree-specific optional arguments:
    - [items] -- the root-level {!Tree.item} nodes. Defaults to [[]].
    - [selected_index] -- zero-based flat index of the highlighted item.
      Defaults to [0].
    - [expand_depth] -- number of levels expanded on first render. [0] collapses
      all; use [max_int] to expand everything. Defaults to [0].
    - [indent_size] -- cells of indentation per nesting level. Defaults to [2].
    - [show_guides] -- when [true] draws vertical guide lines. Defaults to
      [false].
    - [guide_style] -- border character set used for guide lines.
    - [expand_icon] -- string shown next to collapsible nodes when collapsed.
      Defaults to ["▶"].
    - [collapse_icon] -- string shown next to collapsible nodes when expanded.
      Defaults to ["▼"].
    - [leaf_icon] -- string shown next to leaf nodes. Defaults to [" "].
    - [background] -- background color of unselected items.
    - [text_color] -- foreground color of unselected items.
    - [selected_background] -- background of the selected item.
    - [selected_text_color] -- foreground of the selected item.
    - [focused_selected_background] -- background of the selected item when the
      widget has focus.
    - [focused_selected_text_color] -- foreground of the selected item when the
      widget has focus.
    - [guide_color] -- color of the vertical guide lines.
    - [icon_color] -- color of the expand/collapse/leaf icons.
    - [wrap_selection] -- when [true] wraps selection past the last or first
      visible item. Defaults to [false].
    - [fast_scroll_step] -- items skipped per fast-scroll action. Defaults to
      [5].
    - [on_change] -- fired when the selected index changes.
    - [on_activate] -- fired when the user confirms the selection.
    - [on_expand] -- fired when a node is expanded or collapsed; receives the
      flat index and [true] if expanded, [false] if collapsed. *)

(** {1:internal Internal modules} *)

module Reconciler = Reconciler
(** The virtual-DOM reconciler used by {!val-run}.

    {b Note.} This module is part of the internal implementation and its
    interface may change between versions. *)
