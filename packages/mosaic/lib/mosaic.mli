(** Terminal applications in the Elm architecture.

    A Mosaic {e program} runs an application ({!type-app}) in a terminal. The
    application's state is its {e model}, a value it never changes in place.
    [init] gives the first model, [view] describes the screen for a model
    ({!type-t}), and [update] takes a {e message} and the model, and gives the
    next model with a {e command} ({!Cmd}) for the effects it asks for. Messages
    come from the elements of the view, such as a button's [on_click], from the
    {e subscriptions} the model declares ({!Sub}), such as keys and timers, and
    from work done off the loop ({!Cmd.val-perform}, {!Sub.val-worker}).

    A program holds the terminal with a run of {!Matrix} ({!Matrix.run}), and
    its {e loop} takes the run's steps on one thread. The loop applies each
    message with [update], and calls [view] when the model or the usable size
    ({!Matrix.size}) changed. After each event and each change it draws a
    {e frame}, the view laid out and painted on the terminal, which writes only
    the cells that changed. [update], [view], [subscriptions] and the commands
    start on the loop, one message at a time, and [init] runs on the same thread
    before it. Work done elsewhere gives the loop its messages, which it applies
    in a later step.

    The life cycle of a program:
    + {!val-run} calls [init], then takes the terminal.
    + It starts [init]'s commands, calls [subscriptions] with the first model,
      and draws the first frame from [view].
    + Each message goes through [update]. The commands it returns start, and
      [subscriptions] is called again. [view] is called before the next event or
      frame when the model changed.
    + The program ends at {!Cmd.val-quit}, at a Ctrl+C that no handler
      prevented, at the end of the input, at a stop request, or at a signal that
      ends the process, and {!val-run} gives the terminal back.

    This program counts the presses of [+], and quits at [q]:
    {[
    open Mosaic

    type msg = Increment | Quit

    let () =
      run
        {
          init = (fun () -> (0, Cmd.none));
          update =
            (fun msg n ->
              match msg with
              | Increment -> (n + 1, Cmd.none)
              | Quit -> (n, Cmd.quit));
          view = (fun n -> text (Printf.sprintf "%d presses" n));
          subscriptions =
            (fun _ ->
              Sub.on_keys
                [ (Shortcut.char '+', Increment); (Shortcut.char 'q', Quit) ]);
        }
    ]}

    The elements of the view are in {{!elements}UI elements}. *)

open Mosaic_ui

(** {1:re_exports Re-exports} *)

module Ansi = Matrix.Ansi
(** Terminal ANSI escape sequences: colors ({!Ansi.Color}), styles
    ({!Ansi.Style}), and attributes ({!Ansi.Attr}). *)

module Border = Matrix.Grid.Border
(** Border character sets for box-drawing. Provides {!Border.single},
    {!Border.rounded}, {!Border.heavy}, {!Border.double}, and {!Border.ascii}.
*)

module Editor = Mosaic_ui.Editor
(** Persistent text, caret, selection, undo and viewport state. *)

module Scroll = Mosaic_ui.Scroll
(** Persistent viewport position and following intent. *)

module Event = Mosaic_ui.Event
(** Input event types for keyboard, mouse, and paste events. *)

module Focus : sig
  (** Pure traversal of application-owned focus identifiers. *)

  val next : equal:('a -> 'a -> bool) -> 'a list -> 'a option -> 'a option
  (** [next ~equal order current] is the identifier after [current] in [order],
      wrapping to the first identifier. An absent or unknown [current] selects
      the first identifier; an empty [order] returns [None]. Identifiers should
      be distinct under [equal]. No widget focus is changed. *)

  val prev : equal:('a -> 'a -> bool) -> 'a list -> 'a option -> 'a option
  (** [prev ~equal order current] traverses in reverse, wrapping to the last
      identifier. An absent or unknown [current] selects the last identifier; an
      empty [order] returns [None]. *)
end

module Shortcut = Mosaic_ui.Shortcut
(** Typed keyboard shortcuts shared by widgets and subscriptions. *)

module Theme = Mosaic_ui.Theme
(** Themes, the styles an interface's roles are drawn in ({!val-themed}). *)

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

  val theme : t -> Theme.t
  (** [theme t] is the theme in scope at [t] ({!Theme}), for an [on_draw] that
      draws by role: its cells then follow a {!val-themed} around the canvas and
      the terminal's colors, and are drawn again with a theme that changes. *)

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
      [y]. Defaults are those of {!Matrix.Grid.draw_text}. *)

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
      row [y]. Defaults are those of {!Matrix.Grid.draw_box}. *)

  val draw_line :
    t ->
    x1:int ->
    y1:int ->
    x2:int ->
    y2:int ->
    ?style:Ansi.Style.t ->
    ?symbols:Matrix.Grid.line_symbols ->
    ?kind:[ `Line | `Braille ] ->
    unit ->
    unit
  (** [draw_line t ~x1 ~y1 ~x2 ~y2 ()] draws a line from [(x1, y1)] to
      [(x2, y2)].
      - [style] defaults to {!Ansi.Style.default}.
      - [symbols] defaults to standard box-drawing characters.
      - [kind] defaults to [`Line]. Use [`Braille] for Braille dot patterns. *)

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
  (** [set_uchar t ~x ~y ~uchar:cell ~fg ~bg ~attrs ()] writes a single cell.

      [link] defaults to no hyperlink. [blend] defaults to [false]. *)

  val clear : ?color:Ansi.Color.t -> t -> unit
  (** [clear t] clears the canvas grid and schedules a re-render.

      [color] defaults to the terminal default background. *)

  (** {1:grid Low-level grid access} *)

  val grid : t -> Matrix.Grid.t
  (** [grid t] is the underlying grid for [t]. Use this for operations not
      covered by the canvas drawing functions. *)

  (** {1:callbacks Callbacks} *)

  val set_on_draw : t -> (t -> dt:float -> unit) option -> unit
  (** [set_on_draw t cb] registers [cb] as the render-time drawing callback for
      [t]. [cb] fires each render pass after the grid has been auto-resized;
      [~dt] is the elapsed time in seconds since the last frame. Pass [None] to
      clear the callback. *)

  val set_on_resize : t -> (t -> unit) option -> unit
  (** [set_on_resize t cb] registers [cb] as the resize callback for [t]. [cb]
      fires when the canvas grid dimensions change. Pass [None] to clear the
      callback. *)

  (** {1:render_control Render control} *)

  val request_render : t -> unit
  (** [request_render t] schedules a re-render of [t]. Call this after drawing
      outside of a draw callback. *)
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
(** A contiguous run of text with a single visual style. Used by editable text
    widgets that accept pre-styled content. *)

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
  type action = Mosaic_ui.Select.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Activate  (** Keyboard navigation and activation actions. *)

  type 'a item
  (** A display label and optional description paired with an application value.
      Values must be distinct under the selection widget's [equal] function. *)

  val item : 'a -> ?description:string -> string -> 'a item
  (** [item value ?description label] displays [label] and reports [value] when
      selected or activated. [description] is absent by default. *)
end

(** Companion types for the {!val-tab_select} widget. *)
module Tab_select : sig
  type action = Mosaic_ui.Tab_select.action =
    | Move_left
    | Move_right
    | Activate  (** Keyboard navigation and activation actions. *)

  type 'a item
  (** A tab label and description paired with an application value. Values must
      be distinct under the selection widget's [equal] function. *)

  val item : 'a -> ?description:string -> string -> 'a item
  (** [item value ?description label] displays [label] and reports [value] when
      selected or activated. [description] defaults to the empty string. *)
end

(** Companion types for the {!val-table} widget. *)
module Table : sig
  type action = Mosaic_ui.Table.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Page_up
    | Page_down
    | Activate  (** Keyboard navigation and activation actions. *)

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

  type 'a row
  (** Table cells paired with an application value. Values must be distinct
      under the table's [equal] function. *)

  val cell : ?style:Ansi.Style.t -> string -> cell
  (** [cell s] is a plain-text cell containing [s].

      [style] defaults to {!Ansi.Style.default}. *)

  val rich : Mosaic_ui.Text.fragment list -> cell
  (** [rich fragments] is a styled cell built from a list of styled text
      fragments. *)

  val row : 'a -> cell array -> 'a row
  (** [row value cells] displays [cells] and reports [value] when selected or
      activated. [cells] must not be mutated while the row is mounted. *)
end

(** Companion types for the {!val-tree} widget. *)
module Tree : sig
  type action = Mosaic_ui.Tree.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Expand
    | Collapse
    | Toggle
    | Activate  (** Keyboard navigation, expansion and activation actions. *)

  type item = Mosaic_ui.Tree.item = {
    id : string;
    label : string;
    children : item list;
  }
  (** A node with a stable ID unique in the tree. *)

  val item : id:string -> ?children:item list -> string -> item
  (** [item ~id label] is a node whose children default to [[]]. *)

  module State = Mosaic_ui.Tree.State
end

(** Companion types for the {!val-spinner} widget. *)
module Spinner : sig
  type frame_set = Mosaic_ui.Spinner.frame_set = {
    frames : string array;  (** Characters cycled as animation frames. *)
    period : float;  (** Time in seconds between frame advances. *)
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

  type style = Mosaic_ui.Scroll_bar.style
  (** Scroll bar appearance, independent of scroll position and dimensions. *)

  val style :
    ?show_arrows:bool ->
    ?track_color:Ansi.Color.t ->
    ?thumb_color:Ansi.Color.t ->
    ?arrow_fg:Ansi.Color.t ->
    ?arrow_bg:Ansi.Color.t ->
    unit ->
    style
  (** [style ()] is the default scroll bar appearance. Optional fields have the
      same defaults as {!Mosaic_ui.Scroll_bar.val-style}. *)
end

(** Structured content for {!val-rich_text}. *)
module Text : sig
  type fragment = Mosaic_ui.Text.fragment
  (** An immutable text fragment with optional nested styles. *)

  module Fragment = Mosaic_ui.Text.Fragment
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

module Diff = Mosaic_ui.Diff
(** Companion types for the {!val-diff} widget. *)

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

  module Style = Mosaic_ui.Markdown.Style
  (** Element styles: the terminal style of every {!style_key}, compared by
      value. *)
end

module Code : sig
  type syntax = Mosaic_ui.Code.syntax
  (** The type for source-code syntax settings. *)

  module Highlighter : sig
    type edit = Mosaic_ui.Code.Highlighter.edit = {
      start : int;
      removed : int;
      inserted : int;
    }

    type 'state previous = 'state Mosaic_ui.Code.Highlighter.previous = {
      content : string;
      state : 'state;
      edit : edit;
    }

    type 'state request = 'state Mosaic_ui.Code.Highlighter.request = {
      content : string;
      language : string;
      range : int * int;
      previous : 'state previous option;
    }

    type 'state result =
      (Mosaic_ui.Syntax_highlight.t * 'state, exn) Stdlib.result

    type 'state job = 'state Mosaic_ui.Code.Highlighter.job
    type t = Mosaic_ui.Code.Highlighter.t

    val job :
      poll:(unit -> 'state result option) -> cancel:(unit -> unit) -> 'state job

    val sync :
      (language:string -> content:string -> Mosaic_ui.Syntax_highlight.t) -> t
    (** [sync f] runs [f ~language ~content] on the rendering thread and keeps
        nothing between results, so it is asked for the whole content; errors
        fall back to plain text. *)

    val threaded :
      (language:string -> content:string -> Mosaic_ui.Syntax_highlight.t) -> t
    (** [threaded f] runs [f ~language ~content] on one on-demand worker per
        highlighter, on the program's [process_perform] executor, as
        {!threaded_incremental} runs its function. Reuse the highlighter across
        renders. *)

    val sync_incremental :
      ('state request -> Mosaic_ui.Syntax_highlight.t * 'state) -> t
    (** [sync_incremental f] runs [f] on the rendering thread with the view's
        latest result; errors fall back to plain text. See
        {!Mosaic_ui.Code.Highlighter} for requests and results. *)

    val threaded_incremental :
      ('state request -> Mosaic_ui.Syntax_highlight.t * 'state) -> t
    (** [threaded_incremental f] serializes requests on one on-demand worker per
        highlighter, which runs on the program's [process_perform] executor
        ({!val-run}). Cancellation removes queued requests and discards running
        results. Reuse the highlighter across renders. See
        {!Mosaic_ui.Code.Highlighter.threaded_incremental} for the contract. *)

    val async : ('state request -> notify:(unit -> unit) -> 'state job) -> t
  end

  val syntax :
    ?language:string ->
    ?style:Mosaic_ui.Syntax_style.t ->
    Mosaic_ui.Syntax_highlight.t ->
    syntax
  (** [syntax highlights] is a source-code syntax configuration. *)

  val with_highlighter :
    language:string ->
    ?style:Mosaic_ui.Syntax_style.t ->
    Highlighter.t ->
    syntax
  (** [with_highlighter ~language highlighter] is a source-code syntax
      configuration produced by [highlighter]. See
      {!Mosaic_ui.Code.with_highlighter} for what is shown while a result is
      pending. *)
end

(** Syntax styles. The normative scope vocabulary and fallback rules are
    documented in {!Mosaic_ui.Syntax_style}. *)
module Syntax_style : sig
  type t = Mosaic_ui.Syntax_style.t
  (** The type for syntax styles. *)

  val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
  (** [make ~base mappings] is a syntax style with [base] as the default style.
  *)

  val default : t
  (** The built-in dark syntax style. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have equal base styles and equal
      mappings. Styles are values; see {!Mosaic_ui.Syntax_style.equal}. *)

  val base : t -> Ansi.Style.t
  (** [base style] is the default style for text outside highlighted ranges. *)

  val resolve_overlay : t -> string -> Ansi.Style.t
  (** [resolve_overlay style group] is the raw overlay style for [group]. *)

  val resolve : t -> string -> Ansi.Style.t
  (** [resolve style group] is the complete style for [group]: overlay merged on
      top of the base style. *)
end

module Syntax_highlight = Mosaic_ui.Syntax_highlight
(** Source-bound highlight ranges and their resolved byte annotations. *)

(** {1:views Views} *)

type 'msg t = 'msg Vnode.t
(** The type for view nodes parameterized by message type ['msg]. Semantic
    callbacks produce messages directly. Raw event handlers return
    ['msg option]: [Some msg] dispatches the message; [None] ignores the event.

    Build values of this type with the element constructors ({!val-box},
    {!val-text}, {!val-input}, etc.) and compose them with {!val-fragment}. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f view] is [view] with every message transformed by [f].

    Use [map] to embed a child component whose message type differs from the
    parent's. *)

(** {1:off_loop Work off the loop} *)

type 'msg mailbox = {
  dispatch : 'msg -> unit;
      (** [dispatch msg] queues [msg] for the loop and returns. Off the loop's
          thread it blocks while 65,536 posts to the run are pending
          ({!Matrix.post}), so work that dispatches faster than the loop applies
          its messages keeps pace with it. *)
  send : 'msg -> unit;
      (** [send msg] queues [msg] as [dispatch] does, and returns once the loop
          has taken it and [update] has applied it, or the run has ended, so
          work that sends keeps pace with the loop. Work that produces values
          faster than they are shown, such as progress, keeps the latest and
          sends it when the previous [send] returns.

          [send] waits for the loop, so on the loop's thread it would wait
          forever, and raises [Invalid_argument] there. It must not be called
          while holding a lock that [update], [view] or a command takes, or from
          a fiber or a coroutine that shares the loop's thread, which would wait
          forever too. *)
}
(** The type for the way work done off the loop gives the program messages, from
    any thread, any number of times. A worker ({!Sub.val-worker}) has a mailbox,
    and the work of {!Cmd.val-perform} has its [dispatch] alone. The messages of
    one piece of work come in the order it gave them, and the loop applies each
    with [update] in a later step, as a message posted to its run
    ({!Matrix.post}). Once a worker's stop is requested, its [dispatch] and
    [send] return immediately and give nothing, and the loop drops what the
    worker gave before and has not taken, so no message of a worker reaches
    [update] after the update that removed it. *)

(** {1:commands Commands} *)

(** Commands, the effects the application asks for.

    A {e command} is returned beside a model by [init] and [update]. It starts
    on the loop after the update that returned it, before the next message,
    event or frame, or after [init] once the terminal is taken. A command that
    acts on the terminal (a title, the clipboard, static output, a program given
    the terminal) takes effect as it starts. The work of {!Cmd.val-perform}
    starts then, off the loop, and goes on after. *)
module Cmd : sig
  (** The type for commands. Each constructor is the command of the function of
      the same name below. *)
  type 'msg t =
    | None  (** {!val-none}. *)
    | Batch of 'msg t list  (** {!val-batch}. *)
    | Perform of (('msg -> unit) -> unit)  (** {!val-perform}. *)
    | Exec of (unit -> 'msg)  (** {!val-exec}. *)
    | Suspend  (** {!val-suspend}. *)
    | Quit  (** {!val-quit}. *)
    | Set_title of string  (** {!val-set_title}. *)
    | Copy_to_clipboard of string  (** {!val-copy_to_clipboard}. *)
    | Copy_selection  (** {!val-copy_selection}. *)
    | Query_color_scheme  (** {!val-query_color_scheme}. *)
    | Query_terminal_colors  (** {!val-query_terminal_colors}. *)
    | Bell  (** {!val-bell}. *)
    | Notify of {
        title : string;  (** [notify]'s [~title]. *)
        body : string;  (** [notify]'s [~body]. *)
      }  (** {!val-notify}. *)
    | Clear_selection  (** {!val-clear_selection}. *)
    | Static_commit of 'msg Vnode.t  (** {!val-static_commit}. *)
    | Static_clear  (** {!val-static_clear}. *)
    | Abort_paste  (** {!val-abort_paste}. *)
    | Keep_frame of bool  (** {!val-keep_frame}. *)
    | Focus_next  (** {!val-focus_next}. *)
    | Focus_prev  (** {!val-focus_prev}. *)

  val none : 'msg t
  (** [none] is the command that does nothing. *)

  val batch : 'msg t list -> 'msg t
  (** [batch cmds] starts the commands of [cmds] in list order, depth first. The
      commands that act on the terminal take effect in that order, since they
      write to one stream. The work of {!val-perform} starts in that order too,
      and its messages arrive in the order it finishes, which nothing promises.
  *)

  val perform : (('msg -> unit) -> unit) -> 'msg t
  (** [perform f] runs [f dispatch] off the loop, by {!val-run}'s
      [process_perform] (by default a thread of a helper domain), so that long
      work never holds the loop. [f] gives the program messages with [dispatch],
      which is a mailbox's [dispatch] ({!type-mailbox}). [dispatch] stays valid
      after [f] returns, until the run ends, so [f] may hand it to a thread or a
      callback the program did not start:
      {[
      Cmd.perform (fun dispatch ->
          Watcher.on_change (fun path -> dispatch (Changed path)))
      ]}

      An exception [f] raises ends the run: {!val-run} gives the terminal back
      and raises it, with the backtrace of where [f] raised it (when backtraces
      are recorded, [Printexc.record_backtrace]). After the run, [dispatch] does
      nothing, and an exception from [f] has no run left to end, and is dropped.
      [f] cannot be stopped, and cannot wait for the loop to apply its messages:
      work the program may stop wanting, or that keeps pace with the loop with
      [send], is a {!Sub.val-worker}. *)

  val exec : (unit -> 'msg) -> 'msg t
  (** [exec f] gives the terminal to [f] and dispatches [f ()]'s result once the
      terminal is taken again ({!Matrix.release}): [f] can run another program
      in the terminal, an editor or a pager, as a shell runs a command. The
      program waits for [f] as POSIX [system()]'s caller does, so nothing is
      drawn or read, and timers, workers' messages and other commands wait. [f]
      finds the terminal as a shell leaves it to a command: cooked, on the
      primary screen, with the cursor shown and the keys typed for it, and the
      terminal's suspend character stopping its program. The next frame is drawn
      whole.

      An exception [f] raises ends the run, once the terminal is taken again. *)

  val suspend : 'msg t
  (** [suspend] stops the program's job as the terminal's suspend character does
      in a shell ({!Matrix.stop_job}). The terminal is given back, the job
      stops, and when the shell continues it the terminal is taken again and the
      next frame drawn whole. Raw mode turns the suspend character, Ctrl+Z, into
      a key, so a program that wants it to suspend binds it to this command.
      Nothing happens when SIGTSTP was ignored as the run began, or the terminal
      is not the process's controlling terminal, since the job cannot be stopped
      then. *)

  val quit : 'msg t
  (** [quit] ends the run once the commands of the update that returned it have
      started, so [batch [static_commit v; quit]] commits [v]. Nothing after
      that update is applied. The messages that come after it, from the same
      event too, and the events read and not yet handled, typed for a program
      that has ended, are dropped, and no frame is drawn after the step that
      quit, but, in [`Primary] mode, the last one, which a program keeps unless
      it says otherwise ({!val-keep_frame}). *)

  val set_title : string -> 'msg t
  (** [set_title s] sets the terminal window's title to [s]
      ({!Matrix.Terminal.set_title}). [s] must hold no control character. *)

  val copy_to_clipboard : string -> 'msg t
  (** [copy_to_clipboard s] asks the terminal to set the clipboard to [s] (OSC
      52, {!Matrix.Terminal.copy_to_clipboard}). A terminal without OSC 52
      leaves the clipboard as it is. *)

  val copy_selection : 'msg t
  (** [copy_selection] copies the text selected in the view to the clipboard, as
      {!copy_to_clipboard} does. It does nothing when no text is selected. *)

  val query_color_scheme : 'msg t
  (** [query_color_scheme] asks the terminal for its color scheme
      ({!Matrix.Terminal.query_color_scheme}). A terminal that knows the query
      answers, and the answer comes to {!Sub.val-on_color_scheme}. *)

  val query_terminal_colors : 'msg t
  (** [query_terminal_colors] asks the terminal for its default foreground and
      background and the first 16 entries of its palette (OSC 10, 11 and 4). A
      terminal that knows the queries answers, and the answer comes to
      {!Sub.val-on_terminal_colors}. *)

  val bell : 'msg t
  (** [bell] rings the terminal's bell (BEL). *)

  val notify : title:string -> body:string -> 'msg t
  (** [notify ~title ~body] posts a desktop notification through the terminal
      ({!Matrix.Terminal.notify}). [title] and [body] must hold no control
      character, and [title] no [;]. *)

  val clear_selection : 'msg t
  (** [clear_selection] clears the text selected in the view, if any. *)

  val static_commit : 'msg Vnode.t -> 'msg t
  (** [static_commit view] renders [view] at the width of the next frame
      ({!Matrix.effective_size}), as tall as it needs, and writes its rows up to
      the last that holds something to the transcript, with their styles
      ({!Matrix.static_write}). In [`Alt] mode it does nothing. *)

  val static_clear : 'msg t
  (** [static_clear] erases the transcript and the screen
      ({!Matrix.static_clear}). The scrollback stays. In [`Alt] mode it does
      nothing. *)

  val keep_frame : bool -> 'msg t
  (** [keep_frame keep] says what a program in [`Primary] mode leaves on the
      screen when it ends ({!Matrix.keep_frame}). With [true], as by default, it
      draws the view of the model it ends with as its last frame, after the
      update that quit, and the frame stays, with the cursor below it. With
      [false], the live region is erased. The latest [keep_frame] holds,
      whichever way the program ends. In [`Alt] mode it does nothing. *)

  val abort_paste : 'msg t
  (** [abort_paste] ends the bracketed paste in progress, if any, and drops what
      it received ({!Matrix.abort_paste}). The bytes after it are input again. A
      paste ends at its end marker, so one whose terminal stops before the
      marker takes every later byte as content, keys typed included, until the
      program gives up on it. The program gives up after 5 s of silence, unless
      the application decides when ({!Sub.val-on_paste_stalled}). *)

  val focus_next : 'msg t
  (** [focus_next] dispatches the [on_focus_request] of the next stop after the
      focused element, as Tab does ({{!elements}UI elements}), or of the first
      stop when none is focused. It acts on the next view, so an update that
      adds an element and asks for [focus_next] can reach it. Nothing is
      dispatched when the view has no other stop. *)

  val focus_prev : 'msg t
  (** [focus_prev] is {!focus_next} backwards, as Shift+Tab: the stop before the
      focused element, or the last stop when none is focused. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f cmd] is [cmd] with every dispatched message transformed by [f]. Use
      this to embed child-component commands in a parent command. *)
end

(** {1:subscriptions Subscriptions} *)

(** Subscriptions, the events and the work the application declares from its
    model.

    The program calls the application's [subscriptions] with the model after
    [init]'s commands have started and after every update, and listens to and
    runs what it returns. A subscription that has an {e id} ({!Sub.val-every},
    {!Sub.val-after}, {!Sub.val-on_tick}, {!Sub.val-worker}) lives from the
    update whose subscriptions first declare its id to the update whose
    subscriptions no longer do, and the others act for as long as they are
    declared. An id must be nonempty, and the run raises [Invalid_argument] when
    two subscriptions declare the same id. Compose subscriptions with
    {!Sub.val-batch}. *)
module Sub : sig
  (** {1:phases Phases}

      A key, a paste or a mouse event goes from the application down to its
      target element and back up, as in the DOM: the application's [Capture]
      listeners, then the target's handlers, then its behavior unless prevented
      (the character a key types into a focused input, the item a key selects in
      a focused list), then each ancestor's handlers, then the application's
      [Bubble] listeners. A key or a paste targets the focused element; a mouse
      event the element under the pointer. {!Event} says what stopping
      propagation and preventing the default do along the way. A behavior that
      acts on an event consumes it, which prevents and stops it. The keys a
      widget's behavior acts on are part of the widget's interface, listed in
      its module (for example {!Mosaic_ui.Select}).

      The application is one node, so its listeners of a phase all see what
      reaches the phase, in declaration order, whatever the others did with it.
      A [Capture] listener sees every event before any element, and can keep it
      from them. A [Bubble] listener sees every event no element stopped,
      prevented or not, and reads {!Event.Key.default_prevented} to know whether
      something used it. A behavior that acts also stops the event, so a
      [Bubble] listener never sees a key typed into a focused input, and a
      shortcut on [q] declared there does not fire while [q] is typed into one.
      A key the application takes whatever is focused, such as F1 opening help,
      belongs in [Capture], with {!Event.Key.prevent_default} so the focused
      element does not act on it too. One [Capture] listener cannot hide an
      event from another; while a dialog owns the keyboard, stop declaring the
      listeners it overrides.

      A listener's message goes through [update] immediately. The path is taken
      when the event is dispatched, so a focus change that message makes moves
      the next event, not this one, with one exception. The behavior runs on the
      element focused when it runs, so a [Capture] listener that focuses an
      input on a printable key gets that character inserted into the input. *)

  (** The type for the phase a listener runs in. *)
  type phase =
    | Capture  (** Before the elements, for every event. *)
    | Bubble  (** After the elements, for the events no element stopped. *)

  (** {1:workers Workers}

      A worker is work that runs off the loop for as long as the program
      declares it: following a file, reading a socket, watching a process. Its
      lifetime is its subscription's, as a timer's is. It starts in the update
      whose subscriptions first declare its id, and its stop is requested in the
      update whose subscriptions no longer do, or when the run ends. OCaml
      cannot interrupt a thread, so a worker stops itself. It reads
      {!Stop.requested} between steps, and a step that blocks (a read, a wait)
      is unblocked from {!Stop.on_request}, for example by closing what it waits
      on. *)

  (** Stop requests. *)
  module Stop : sig
    type t
    (** The type for a worker's stop request, as the DOM's [AbortSignal]. *)

    val requested : t -> bool
    (** [requested t] is [true] once the worker's stop was requested. Any thread
        may call it. *)

    val on_request : t -> (unit -> unit) -> unit
    (** [on_request t f] calls [f ()] once, when the stop is requested, on the
        loop's thread, or immediately, on the calling thread, if it was
        requested already. [f] runs on the loop, so it should only unblock the
        worker. An exception it raises ends the run, or is dropped when the run
        is ending. *)
  end

  (** The type for subscriptions. Each constructor is the subscription of the
      function of the same name below. *)
  type 'msg t =
    | None  (** {!val-none}. *)
    | Batch of 'msg t list  (** {!val-batch}. *)
    | Every of string * float * (int -> 'msg)  (** {!val-every}. *)
    | After of string * float * 'msg  (** {!val-after}. *)
    | On_tick of string * (dt:float -> 'msg)  (** {!val-on_tick}. *)
    | On_key of phase * (Event.key -> 'msg option)  (** {!val-on_key}. *)
    | On_key_release of (Matrix.Input.Keystroke.release -> 'msg option)
        (** {!val-on_key_release}. *)
    | On_mouse of phase * (Event.mouse -> 'msg option)  (** {!val-on_mouse}. *)
    | On_paste of phase * (Event.paste -> 'msg option)  (** {!val-on_paste}. *)
    | On_paste_stalled of float * 'msg  (** {!val-on_paste_stalled}. *)
    | On_resize of (width:int -> height:int -> 'msg)  (** {!val-on_resize}. *)
    | On_focus of 'msg  (** {!val-on_focus}. *)
    | On_blur of 'msg  (** {!val-on_blur}. *)
    | On_color_scheme of ([ `Dark | `Light ] -> 'msg option)
        (** {!val-on_color_scheme}. *)
    | Worker of string * (Stop.t -> 'msg mailbox -> unit)  (** {!val-worker}. *)
    | On_focus_lost of 'msg  (** {!val-on_focus_lost}. *)
    | On_terminal_colors of (Ansi.Theme.colors -> 'msg option)
        (** {!val-on_terminal_colors}. *)

  val none : 'msg t
  (** [none] is the subscription to nothing. *)

  val batch : 'msg t list -> 'msg t
  (** [batch subs] is the subscriptions of [subs]. *)

  val every : id:string -> float -> (int -> 'msg) -> 'msg t
  (** [every ~id period f] fires on the grid [origin + n * period], [origin] the
      time [id] first appeared in the subscriptions, and dispatches [f k].
      [k >= 1] is the number of periods that ended since its previous call: [1]
      on time, more when the loop could not run for longer than a period (a long
      [update], a stopped process). The grid does not drift, since a late call
      does not move the next one, and a timer never fires twice in a turn.
      Timers that come due in the same turn fire in the order of their grid
      points.

      The subscription is the timer. Keeping [id] keeps its grid, declaring it
      with another [period] starts a new grid at that time, and dropping [id]
      stops it. A pending timer costs nothing until it fires. Not for animation:
      use {!val-on_tick}.

      Raises [Invalid_argument] if [id] is empty or [period] is not positive and
      finite. *)

  val after : id:string -> float -> 'msg -> 'msg t
  (** [after ~id delay msg] dispatches [msg] once, [delay] seconds after [id]
      first appears in the subscriptions, in the first turn at or past that
      time, as {!val-every} fires. Dropping [id] before then cancels it, which
      makes a debounce. Declare [after] while the model waits, and each update
      that should restart the wait declares it under a new id. Once fired, it
      stays inert while [id] stays declared, and fires again only if [id] leaves
      and returns. Declaring [id] with another [delay] starts it again from that
      time.

      Raises [Invalid_argument] if [id] is empty or [delay] is negative or not
      finite. *)

  val on_tick : id:string -> (dt:float -> 'msg) -> 'msg t
  (** [on_tick ~id f] dispatches [f ~dt] once per frame while declared, before
      the frame is laid out, and asks for the next frame, so frames then come at
      the frame rate ({!Matrix.val-config}'s [fps]), and stop when no tick
      subscription or animated element, such as a spinner, asks. An early frame
      ({!Matrix.early}), drawn for input before the grid's next one, ticks
      nothing and moves no element's animation, so ticks keep the frame rate
      while the user types. [dt] is the time in seconds since this
      subscription's previous tick, [0.] on its first, so an animation started
      after a pause does not jump by the pause. Keeping [id] keeps the count of
      time; dropping it and declaring it again starts at [0.]. Use this for
      animations; for work at a fixed rate, {!val-every}.

      Raises [Invalid_argument] if [id] is empty. *)

  val on_key : ?phase:phase -> (Event.key -> 'msg option) -> 'msg t
  (** [on_key f] calls [f] on each key press that reaches [phase] (defaults to
      [Bubble]; see {!section-phases}) and dispatches the message it returns. *)

  val on_keys : ?phase:phase -> (Shortcut.t * 'msg) list -> 'msg t
  (** [on_keys bindings] dispatches the message bound to a key that reaches
      [phase] (defaults to [Bubble]; see {!section-phases}) with its default not
      prevented, and prevents it, since a binding is a default action of its
      key, so a key something prevented fires none. Later equivalent shortcuts
      override earlier ones. Resolution tries the key, then the base-layout key,
      as widget bindings do. See {!Shortcut} for how keys and modifiers match.
  *)

  val on_key_release : (Matrix.Input.Keystroke.release -> 'msg option) -> 'msg t
  (** [on_key_release f] delivers key releases to [f], which no widget sees. A
      terminal reports releases only under the kitty keyboard protocol, when the
      runtime asks for them ({!Matrix.val-config}'s [key_releases], given
      through {!run}'s [config]). *)

  val on_mouse : ?phase:phase -> (Event.mouse -> 'msg option) -> 'msg t
  (** [on_mouse f] calls [f] on each mouse event that reaches [phase] (defaults
      to [Bubble]; see {!section-phases}) and dispatches the message it returns.
  *)

  val on_paste : ?phase:phase -> (Event.paste -> 'msg option) -> 'msg t
  (** [on_paste f] calls [f] on each paste that reaches [phase] (defaults to
      [Bubble]; see {!section-phases}) and dispatches the message it returns.
      The focused editor's behavior inserts a paste and stops it, so a [Bubble]
      listener sees a paste no editor took; a [Capture] listener can prevent the
      insertion with {!Event.Paste.prevent_default}. A bracketed paste is
      delivered whole once its end marker arrives, and dropped when cut short
      ({!Cmd.abort_paste}, or the end of the input); see {!val-on_paste_stalled}
      for a paste whose end marker never comes. Text a terminal reports without
      a key, such as an input method's, is delivered as a paste too. *)

  val on_paste_stalled : float -> 'msg -> 'msg t
  (** [on_paste_stalled seconds msg] dispatches [msg] once each time a bracketed
      paste in progress has received nothing for [seconds].

      A paste ends at its end marker, however long the terminal takes to send
      it. A paste whose marker never comes, from a terminal or link that failed
      mid-paste, takes every later byte as its content, Ctrl+C included, and
      nothing the user types can end it. So while the subscriptions declare no
      [on_paste_stalled], the program ends a paste that has received nothing for
      5 s, as {!Cmd.abort_paste} does, and the keys typed after it reach the
      program again. An application that declares [on_paste_stalled] decides
      instead when to give up, with {!Cmd.abort_paste}, from how long the paste
      has been silent. Any byte counts, keys typed into the paste too, since
      they are its content. Raises [Invalid_argument] if [seconds] is not
      positive and finite. *)

  val on_resize : (width:int -> height:int -> 'msg) -> 'msg t
  (** [on_resize f] dispatches [f ~width ~height] with the usable size in
      columns and rows ({!Matrix.size}) as the program begins, when declared
      then, and whenever the size changes ({!Matrix.Resize}). *)

  val on_focus : 'msg -> 'msg t
  (** [on_focus msg] dispatches [msg] when the terminal window gains focus. *)

  val on_blur : 'msg -> 'msg t
  (** [on_blur msg] dispatches [msg] when the terminal window loses focus. *)

  val on_focus_lost : 'msg -> 'msg t
  (** [on_focus_lost msg] dispatches [msg] when the view stops declaring a
      focused element, after a view that declared one: the focused element was
      removed or hidden, or the model no longer declares it
      ({{!elements}UI elements}). [msg] is applied before the frame is drawn, so
      a view that answers it with a new focus, declared or asked with
      {!Cmd.focus_next}, is the one the frame shows. *)

  val on_color_scheme : ([ `Dark | `Light ] -> 'msg option) -> 'msg t
  (** [on_color_scheme f] calls [f] with the terminal's color scheme when the
      terminal answers {!Cmd.val-query_color_scheme} or reports a change (mode
      2031), and dispatches the message it returns. *)

  val on_terminal_colors : (Ansi.Theme.colors -> 'msg option) -> 'msg t
  (** [on_terminal_colors f] calls [f] with the colors the terminal reports, its
      default foreground and background and the first 16 entries of its palette,
      and dispatches the message it returns. The program asks for them as it
      begins, after each report of the color scheme ({!val-on_color_scheme}),
      with which the user may have changed them, and when the application asks
      ({!Cmd.val-query_terminal_colors}); [f] is called once the terminal has
      answered, with the entries it reported. A subscription declared after the
      program began receives the colors from the next answer, so an application
      that subscribes later asks once. A terminal that reports no foreground or
      no background, or an output that is not a terminal, gives no call. *)

  val worker : id:string -> (Stop.t -> 'msg mailbox -> unit) -> 'msg t
  (** [worker ~id f] runs [f stop mailbox] off the loop, on {!val-run}'s
      [process_perform] executor, from the update whose subscriptions first
      declare [id] (see {{!workers}Workers}), and gives the program messages
      with [mailbox] ({!type-mailbox}). Declaring [id] again, with any function,
      keeps the worker that runs. [f] may return before its stop is requested,
      as work under an event loop does once it has started its own tasks. Its
      mailbox still gives the program messages until the stop is requested, and
      [f] runs again only if [id] leaves the subscriptions and returns.

      An exception [f] raises ends the run, as one from {!Cmd.val-perform} does,
      unless its stop was requested before, since the program no longer waits
      for anything from it.

      Raises [Invalid_argument] if [id] is empty. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f sub] is [sub] with every produced message transformed by [f]. Use
      this to embed child-component subscriptions in a parent subscription. *)
end

(** {1:application Application} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
      (** [init ()] is the first model and the commands to start with. The
          program calls it once, before it takes the terminal. *)
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
      (** [update msg model] is the model after [msg], and the commands it asks
          for. The program calls it once for each message. *)
  view : 'model -> 'msg t;
      (** [view model] describes the screen for [model]. The program calls it
          before it next handles an event or draws a frame, when [update]
          returned a model other than the one it was given, by physical
          equality, or the usable size changed. So several updates may come
          before one [view], and several views before one frame. *)
  subscriptions : 'model -> 'msg Sub.t;
      (** [subscriptions model] is what the program listens to and runs for
          [model] ({!Sub}). *)
}
(** The type for applications, which {!val-run} runs. *)

(** {1:running Running}

    {!val-run} runs a program until it ends, waiting for the terminal with
    [select(2)], and runs the work of {!Cmd.val-perform} and {!Sub.val-worker}
    on threads. A program whose own threads give it messages needs nothing more.
    Such a thread ends the run with a stop request ([stop]), and gives the
    program messages through a mailbox or through the [dispatch] that a
    {!Cmd.val-perform} handed it, which stays valid after the perform's function
    returns. *)

val run :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Matrix.Terminal.Caps.env ->
  ?stop:Matrix.Stop.t ->
  ?config:Matrix.config ->
  ?process_perform:((unit -> unit) -> unit) ->
  ('model, 'msg) app ->
  unit
(** [run app] calls [app]'s [init], takes the terminal ({!Matrix.run}), and runs
    [app] until it ends: at {!Cmd.val-quit}, at a Ctrl+C that no handler
    prevented ({!Event.Key.prevent_default}), at the end of the input, or at a
    stop request. A signal that ends the process ends the program too, and then
    [run] does not return ({!Matrix}). In raw mode Ctrl+C is a key, and quitting
    is its default, which an element or a listener that uses the key prevents.
    The terminal is given back on every way out, an exception included. When the
    run ends, [run] requests the stop of the workers and returns without waiting
    for work still running off the loop.

    - [input], [output], [env] -- the terminal to run in and its session's
      environment, as {!Matrix.run} takes them. Default to the process's.
    - [stop] -- ends the program from elsewhere, before it has begun or while it
      runs ({!Matrix.Stop}), as the end of its input does.
    - [config] -- what the run sets up. Defaults to {!Matrix.val-config}[ ()].
    - [process_perform] -- the executor: it runs the work of {!Cmd.val-perform}
      and {!Sub.val-worker}, and of threaded highlighters
      ({!Code.Highlighter.threaded}), off the loop, each given as a function
      that has the dispatch wired in and raises nothing. By default each runs on
      a thread of its own, in one helper domain, so that the loop's domain runs
      the loop alone. Work on a thread of the loop's domain shares its lock, and
      work that computes would hold up each key by up to a tick of OCaml's
      thread scheduler (50 ms) at each of its blocking calls. The helper domain
      starts with the first work and ends when it has none. While it runs,
      OCaml's systhread tick wakes the process 20 times a second, and
      [Unix.fork] raises, as OCaml forbids forking a process that runs several
      domains, while [Unix.create_process] works. An executor given here runs
      the work where it likes, such as on fibers of the application's own
      scheduler, and the messages go through the loop ({!Matrix.post}) whatever
      thread or fiber dispatches them.

    A test runs a program on an emulated terminal with [Mosaic_test.run]
    (library [mosaic.test]).

    Raises what [init] raises, before the terminal is taken, and, once the
    terminal is given back, what [update], [view], [subscriptions] or a command
    raised, and what work done off the loop raised ({!Cmd.val-perform},
    {!Sub.val-worker}), with its backtrace. Raises [Invalid_argument] when two
    subscriptions declare the same id, and what {!Matrix.run} raises. *)

(** {2:event_loops Event loops}

    A program run from an event loop runs with that loop's library.
    [Mosaic_eio.run] (library [mosaic.eio], built when eio is installed) runs it
    from a fiber of an Eio program, and [Mosaic_lwt.run] (library [mosaic.lwt],
    built when lwt is installed) runs it as an Lwt promise. Under another event
    loop, the program runs with {!val-start} and the loop does the waiting. It
    takes the program's steps with {!step}, and between them waits until one of
    {!fds} is readable or the time {!deadline} comes, as {!Matrix.next} does for
    a run. [process_perform] hands the program's work to the loop. The source of
    [Mosaic_eio] is a worked example of this, and the source of [Mosaic_lwt] one
    of a program run beside a loop on a domain of its own. *)

type 'msg running
(** The type for programs running in a terminal ({!val-start}), whose work done
    elsewhere gives them messages of type ['msg]. A [running] exists only inside
    {!val-start}, and belongs to the thread that called it. Every function that
    takes one must be called on that thread, except {!val-dispatch}. *)

val start :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Matrix.Terminal.Caps.env ->
  ?stop:Matrix.Stop.t ->
  ?config:Matrix.config ->
  ?process_perform:((unit -> unit) -> unit) ->
  ('model, 'msg) app ->
  ('msg running -> 'a) ->
  'a
(** [start app f] is {!val-run}[ app] with the waiting left to [f]. It calls
    [app]'s [init], takes the terminal, calls [f] with the running program, and
    gives the terminal back when [f] returns or raises. [f] takes the program's
    steps with {!step} until it says [`Stopped], and the program ends when [f]
    returns, whether it said so or not. The optional arguments are {!val-run}'s.

    Raises what [f] raises, and what {!val-run} raises before [f] is called. *)

val step : 'msg running -> now:int -> [ `Stepped | `Idle | `Stopped ]
(** [step r ~now] has [r] take the next step of its run at the time [now]
    ({!Matrix.val-step}) and act on it, as {!val-run} does with each. It is
    [`Stepped] when it did, [`Idle] when the run has nothing to do until one of
    {!fds} is readable or the time {!deadline} comes, and [`Stopped] once the
    program is over: it quit, Ctrl+C ended it, its input ended, or its stop was
    requested. It never waits for input, a message or a time. [now] must be on
    {!Matrix.clock}, and never earlier than the time of the step before
    ({!Matrix.val-step}).

    Raises what {!val-run} raises, at the step that had it. *)

val fds : 'msg running -> Unix.file_descr list
(** [fds r] is the descriptors [r]'s run waits for ({!Matrix.fds}). *)

val deadline : 'msg running -> int option
(** [deadline r] is the time on {!Matrix.clock} by which {!step} must be called
    again after it said [`Idle], or [None] when only {!fds} can give the program
    something to do ({!Matrix.deadline}). *)

val dispatch : 'msg running -> 'msg -> unit
(** [dispatch r msg] gives [msg] to [r]'s [update] in a later step, as the
    [dispatch] of work done off the loop does ({!type-mailbox}). Any thread,
    domain or fiber may call it. Off the thread that runs [r] it blocks while
    65,536 posts to the run are pending ({!Matrix.post}). After the run ends it
    does nothing. *)

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

    Elements describe the current model. Layout and paint use labelled
    arguments. Layout follows {!Toffee.Style.default}; [box] uses its default
    row direction, and minimum dimensions remain automatic unless explicitly
    supplied.

    Common layout arguments include [display], [position], [size], [min_size],
    [max_size], [padding], [margin], [gap], the [flex_*] properties, alignment,
    and the [grid_*] properties. They map directly to Toffee layout
    declarations. [visible] defaults to [true], [opacity] to [1.], [z_index] to
    [0], and [buffered] and [live] to [false] unless the widget supplies a
    different default.

    [key] is an optional reconciliation hint. [focused] declares keyboard focus.
    With no focused element, keys reach subscriptions only. Declaring more than
    one visible focused element that is not inert raises [Invalid_argument].
    Hidden subtrees do not participate in focus.

    An {e inert} element takes neither focus nor the pointer, as with HTML's
    [inert] attribute: a [focused] declaration on it does not count, Tab passes
    it by, and a pointer over it finds what is drawn below it. Events still go
    along a path through an inert ancestor. Every element in a [box ~inert:true]
    is inert, and while a {!modal} is shown every element outside it is.

    Focus is the model's, so nothing moves it but a declaration. Removing or
    hiding the focused element leaves nothing focused: keys reach subscriptions
    only until the model declares another element focused. The update that
    removes the focused element is where to choose the next one, such as the row
    now at its position. An application that does not know which update did it
    learns it from {!Sub.on_focus_lost}, before the frame without focus is
    drawn, and answers with a declaration or with {!Cmd.focus_next}. Some
    toolkits (AWT) move focus forward by themselves when the focused component
    goes; here that would give the model's focus a second owner.

    An element with [on_focus_request] asks for focus with that message, and the
    application grants it by declaring the element [focused]. Such an element is
    a {e stop}: it asks on an ordinary left-button press while unfocused, and
    when Tab reaches it. A press's request runs before caret and selection
    handling. Rejecting the request, hiding or removing the element cancels that
    press's default interaction. Shift-click does not request focus.

    Tab's default action, after the [Bubble] listeners and unless something
    prevented it ({!Sub.section-phases}), is {!Cmd.focus_next}: the next shown
    stop in view order, an element before its children, asks for focus, the
    first stop following the last, and with nothing focused the first stop asks.
    Shift+Tab's is {!Cmd.focus_prev}. Ctrl+I is Tab in the legacy encoding,
    which sends the same byte for both. An element's [on_key] or a listener that
    gives Tab a meaning of its own prevents the default with
    {!Event.Key.prevent_default}; a binding of {!Sub.on_keys} does so itself.
    {!Focus} steps through the application's own focus identifiers, for an order
    the view does not state.

    Generic event callbacks return a message or [None]. [on_click] runs after a
    primary-button press and release on the element, without dragging.
    Preventing the mouse event default cancels activation. Neither callback
    changes application state by itself. Interactive [state] values belong to
    the model. Semantic callbacks such as [on_change], [on_scroll], [on_submit]
    and [on_activate] return messages directly; acceptance or rejection belongs
    in [update]. Storing an [on_change] proposal accepts it. Subsequent events
    derive from the accepted model.

    [on_size] reports an element's allocated dimensions in cells when they
    differ from the last report. Reports are delivered during layout, and their
    messages are applied before the frame is painted, so a view that depends on
    a size settles within the frame. Within one frame, once a layout pass has
    delivered reports, later passes deliver only the reports of elements nested
    deeper than the shallowest element already reported: a response that resizes
    descendants settles in the same frame. A response that resizes the reported
    element itself, a sibling or an ancestor waits: the frame is painted from
    the model as it stands, and the paint delivers the waiting report, whose
    message is applied before the next frame. This is the depth rule of CSS
    Resize Observer
    ({{:https://www.w3.org/TR/resize-observer/#gather-active-observations-h}
      §3.4.1}). A response that keeps resizing its own element therefore
    alternates on every frame, presenting each one, instead of blocking the
    loop. *)

val empty : 'msg t
(** [empty] occupies no space. *)

val fragment : 'msg t list -> 'msg t
(** [fragment children] groups children without a layout wrapper. *)

val embed : Mosaic_ui.Renderable.t -> 'msg t
(** [embed node] integrates an existing renderable into the view tree. Mosaic
    attaches the node but does not manage its properties or interaction state;
    the integration is responsible for their ownership. *)

val memo : key:'key -> ('key -> 'msg t) -> 'msg t
(** [memo ~key view] defers [view key]. When the previous render mounted an
    equal memo at the same position, Mosaic reuses that subtree without calling
    [view].

    Two memos are equal when they have the same [view] function, compared
    physically, equal keys, and the same message mappers ({!val-map}). Keys are
    equal when they have the same representation: the same constructors and
    integers, the same characters, and the same floating-point bits. Since a
    [view] receives only its key, equal memos build equal subtrees whatever the
    key's type.

    Pass a [view] defined once, for example at top level, and put every value
    the subtree depends on in [key]:
    {[
      let message_view (id, revision, theme) = ...
      let transcript model =
        List.map
          (fun m -> memo ~key:(m.id, m.revision, model.theme) message_view)
          model.messages
    ]}
    A closure allocated by each view call is a new function: it never matches,
    so its subtree is rebuilt on every render. Keys must be immutable and
    acyclic.

    Raises [Invalid_argument] if [key] contains a function, an object or an
    abstract value, which have no representation to compare. *)

val viewport_switch :
  at_least_width:int -> wide:'msg t -> narrow:'msg t -> 'msg t
(** [viewport_switch ~at_least_width ~wide ~narrow] selects [wide] when the
    current terminal viewport is at least [at_least_width] cells wide and
    [narrow] otherwise.

    Mosaic resolves the condition before reconciliation and layout. Only the
    selected branch is mounted, so the other branch cannot take space, receive
    focus, install handlers, or keep live rendering active. Crossing the
    threshold reconciles the new branch normally against the old one; compatible
    keyed elements reuse their renderables and caches; unmatched elements are
    unmounted.

    This is a viewport query, not a query on the containing element's allocated
    width. Raises [Invalid_argument] if [at_least_width] is not positive. *)

val box :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?inert:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
(** [box children] is a container for [children]. [border_width], [inert] and
    the labels from [border] on are {!Mosaic_ui.Box.Props.make}'s and take its
    defaults; the box lays out its border widths in place of a layout border.
    {{!elements}UI elements} describes inert elements and the other labels.

    Raises [Invalid_argument] if a border width is not a length of a whole
    number of cells, [0] or more. *)

val modal : ?key:string -> ?on_close:'msg -> 'msg t -> 'msg t
(** [modal child] shows [child] as a modal dialog, as HTML's [showModal] does:
    every element outside the modal is inert ({{!elements}UI elements}), so
    focus, the pointer and Tab stay within it. Wherever the view declares it,
    the modal is in the {e top layer}: it is laid out against the root of the
    view, which it fills, centring [child], and drawn above every other element,
    clipped by none of its ancestors, so the pointer finds it first. Of several
    modals shown, the last in view order is the one, and the others are inert.
    Once the view drops the modal, the rest is laid out and drawn as before.

    Escape's default action, after the [Bubble] listeners and unless something
    prevented it ({!Sub.section-phases}), is to dispatch [on_close], whatever is
    focused. [Capture] listeners still see every key first. Closing the modal is
    the application's: [on_close] asks, and the view without the modal closes
    it. A focus declared outside the modal is lost while it is shown
    ({!Sub.on_focus_lost}). The application answers with a focus inside, such as
    {!Cmd.focus_next}, and declares the element that opened the modal focused
    again when it closes it, as APG's dialog pattern asks. *)

val themed : (Theme.t -> Theme.t) -> 'msg t -> 'msg t
(** [themed f child] draws [child] with [f] applied to the theme in scope around
    it, as {!Theme} describes scopes, so [themed (Theme.with_style Error s)]
    restyles one role and keeps the others. The view's root is in the scope of
    {!Theme.default} of the colors the terminal reports
    ({!Sub.on_terminal_colors}), and of {!Theme.plain} until it reports them or
    when it reports none. [themed] is no element of its own: it changes neither
    layout nor focus. *)

val button :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?text_style:Ansi.Style.t ->
  ?disabled:bool ->
  on_activate:'msg ->
  string ->
  'msg t
(** [button ~on_activate label] displays [label] and emits [on_activate] on a
    primary-button click or an unmodified Enter, keypad Enter or Space press. A
    click requires a press and release inside the button without dragging. Key
    repeat and release events do not activate it; lock modifiers are ignored.
    When focused, the button acts on those keys and consumes them: they reach
    neither its ancestors nor a [Bubble] listener ({!Sub.section-phases}); every
    other key passes on.

    [focused] defaults to [false] and is owned by the application. Use
    [on_focus_request] to accept pointer focus before activation. [disabled],
    default [false], suppresses activation and focus requests and makes the
    button unfocused. Layout and box paint defaults follow {!box}; [text_style]
    styles the label. Focus adds inverse video; disabled adds dim instead.
    Caller-supplied colors and other attributes are preserved.

    A [Capture] listener ({!Sub.section-phases}) can prevent activation with
    {!Event.Key.prevent_default}. The button composes a box and text; it has no
    separate widget state. *)

val text :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  string ->
  'msg t
(** [text content] is a text element. The labels from [text_style] on are
    {!Mosaic_ui.Text.Props.make}'s and take its defaults; [style] is the text
    style when [text_style] is absent. Layout and paint use the other labelled
    arguments. *)

val rich_text :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  Text.fragment list ->
  'msg t
(** [rich_text fragments] renders structured text as one selectable text widget.
    Fragment styles inherit from [text_style]; nested colours override inherited
    colours. Wrapping and selection cross fragment boundaries. All labelled
    options have the same meaning as {!val-text}. Switching between {!val-text}
    and [rich_text] retains the text widget. *)

val slider :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?orientation:Slider.orientation ->
  ?min:float ->
  ?max:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  state:float ->
  on_change:(float -> 'msg) ->
  unit ->
  'msg t
(** [slider ()] is a slider. Widget-specific visual options follow
    {!Mosaic_ui.Slider}; layout and paint use the labelled arguments above. *)

val input :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?placeholder:string ->
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
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Text_input.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [input ~state ~on_change ()] is a single-line editor. [on_change] proposes
    editing state, including caret and selection changes. Accept the state on
    every edit, and compare {!Editor.text} before running text-dependent work.
    [on_submit] receives the accepted text on Enter.

    Raises [Invalid_argument] if [state] contains CR or LF. Typed and pasted
    newlines are stripped. Undo and redo that would restore multiline text are
    ignored. Widget-specific visual options follow {!Mosaic_ui.Text_input},
    which lists the keys the input acts on when focused; layout and paint use
    the labelled arguments above. *)

val select :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
  ?key_bindings:(Shortcut.t * Select.action) list ->
  ?item_spacing:int ->
  ?fast_scroll_step:int ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  'a Select.item list ->
  'msg t
(** [select ~equal ~selected ~on_change options] displays [options], keeping
    selection attached to the application value under [equal] when options move.
    [None] or a value absent from [options] displays no selection; it does not
    select another item or call [on_change]. Navigation then proposes an item
    value; activation without a selection does nothing. Values must be distinct
    under [equal]. Callbacks receive the value from the current options.

    When focused, the list acts on the keys {!Mosaic_ui.Select} lists. A
    declaration looks for [selected] among the values from the first option on,
    and a callback finds its value by its position, each in time linear in that
    position. The element converts [options] for display only when they are not
    the list it was last declared with, physically, so declaring again the list
    the model keeps costs no conversion. *)

val tab_select :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
  ?key_bindings:(Shortcut.t * Tab_select.action) list ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  'a Tab_select.item list ->
  'msg t
(** [tab_select ~equal ~selected ~on_change options] displays horizontal tabs
    with the value-selection and callback semantics of {!select}. [None] or a
    value absent from [options] displays no selection. Values must be distinct
    under [equal]. When focused, the tabs act on the keys
    {!Mosaic_ui.Tab_select} lists. Declarations and callbacks cost what they
    cost for {!select}. *)

val canvas :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  (Canvas.t -> dt:float -> unit) ->
  'msg t
(** [canvas draw] is a custom drawing surface. Widget-specific visual options
    follow {!Mosaic_ui.Canvas}; layout and paint use the labelled arguments
    above. *)

val picture :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  Matrix.Grid.picture ->
  'msg t
(** [picture p] shows the picture [p] ({!Matrix.Grid.section-pictures}) fit in
    its box, which is [p]'s size in cells where the size is left to the content
    ({!Mosaic_ui.Vnode.picture}); layout and paint use the labelled arguments
    above. *)

val spinner :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?frame_set:Spinner.frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  'msg t
(** [spinner ()] is an animated activity indicator. Widget-specific visual
    options follow {!Mosaic_ui.Spinner}; layout and paint use the labelled
    arguments above. *)

val progress_bar :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?filled_color:Ansi.Color.t ->
  ?empty_color:Ansi.Color.t ->
  unit ->
  'msg t
(** [progress_bar ()] is a progress indicator. Widget-specific visual options
    follow {!Mosaic_ui.Progress_bar}; layout and paint use the labelled
    arguments above. *)

val scroll_bar :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?orientation:Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  on_change:(int -> 'msg) ->
  state:int ->
  scroll_size:int ->
  viewport_size:int ->
  unit ->
  'msg t
(** [scroll_bar ()] is a scroll indicator. Widget-specific visual options follow
    {!Mosaic_ui.Scroll_bar}, whose {!Mosaic_ui.Scroll_bar.handle_key} lists the
    keys the bar acts on when focused; layout and paint use the labelled
    arguments above. *)

val scroll_box :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?background:Ansi.Color.t ->
  ?show_scrollbars:bool ->
  ?scrollbar_style:Scroll_bar.style ->
  ?vertical_bar_style:Scroll_bar.style ->
  ?horizontal_bar_style:Scroll_bar.style ->
  on_scroll:(Scroll.t -> 'msg) ->
  scroll:Scroll.t ->
  'msg t list ->
  'msg t
(** [scroll_box children] is a scrollable container. Widget-specific visual
    options follow {!Mosaic_ui.Scroll_box}; layout and paint use the labelled
    arguments above. [scrollbar_style] sets shared bar appearance;
    [vertical_bar_style] and [horizontal_bar_style] replace it for the
    corresponding axis. When focused, the box acts on the keys
    {!Mosaic_ui.Scroll_box} lists. *)

val textarea :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?annotations:Mosaic_ui.Text_buffer.Annotations.t ->
  ?syntax:Code.syntax ->
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
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Textarea.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [textarea ()] is a multiline editor. Widget-specific visual options follow
    {!Mosaic_ui.Textarea}, which lists the keys the editor acts on when focused;
    layout and paint use the labelled arguments above. *)

val code :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?syntax:Code.syntax ->
  ?conceal:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  string ->
  'msg t
(** [code content] is syntax-highlighted source. Widget-specific visual options
    follow {!Mosaic_ui.Code}; layout and paint use the labelled arguments above.
*)

val line_number :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
(** [line_number child] is a line-number gutter for [child]. Widget-specific
    visual options follow {!Mosaic_ui.Line_number}; layout and paint use the
    labelled arguments above. *)

val diff :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?layout:Diff.layout ->
  ?theme:Diff.theme ->
  ?highlight:Diff.highlight ->
  ?line_highlights:Diff.line_highlight list ->
  ?line_spans:Diff.line_span list ->
  ?line_signs:Diff.line_sign list ->
  ?show_line_numbers:bool ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?text_style:Ansi.Style.t ->
  ?on_line_click:(Diff.line_hit -> 'msg option) ->
  Diff.Patch.t ->
  'msg t
(** [diff patch] is a rendered patch. Widget-specific visual options follow
    {!Mosaic_ui.Diff}; layout and paint use the labelled arguments above. *)

val markdown :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?md_style:Markdown.Style.t ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?highlighter:Code.Highlighter.t ->
  ?syntax_style:Syntax_style.t ->
  ?on_selection:(string option -> 'msg option) ->
  string ->
  'msg t
(** [markdown content] is rendered Markdown. Widget-specific visual options
    follow {!Mosaic_ui.Markdown}; layout and paint use the labelled arguments
    above. *)

val table :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?columns:Table.column list ->
  ?rows:'a Table.row list ->
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
  ?selection_visible:bool ->
  ?show_scroll_indicator:bool ->
  ?activate_on_click:bool ->
  ?wheel_navigation:bool ->
  ?row_styles:Ansi.Style.t list ->
  ?wrap_selection:bool ->
  ?key_bindings:(Shortcut.t * Table.action) list ->
  ?fast_scroll_step:int ->
  on_change:('a -> 'msg) ->
  ?on_activate:('a -> 'msg) ->
  ?on_hover:('a option -> 'msg option) ->
  equal:('a -> 'a -> bool) ->
  selected:'a option ->
  unit ->
  'msg t
(** [table ~equal ~selected ~on_change ?rows ()] displays rows whose selection
    follows their application values under [equal]. [rows] defaults to [[]].
    [None] or a value absent from [rows] displays no selection; activation
    without selection does nothing. Values must be distinct under [equal].
    Change and activation callbacks receive the row value. A left press captures
    that value before its hover callback: filtering or reordering rows during
    the press preserves the clicked value for selection and activation. Removing
    the table cancels the remaining callbacks. [on_hover] receives [None] when
    the pointer leaves all rows.

    When focused, the table acts on the keys {!Mosaic_ui.Table} lists.
    Declarations and callbacks cost what they cost for {!select}, with [rows]
    for its options. *)

val tree :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?items:Tree.item list ->
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
  ?key_bindings:(Shortcut.t * Tree.action) list ->
  ?fast_scroll_step:int ->
  on_change:(Tree.State.t -> 'msg) ->
  ?on_activate:(Tree.item -> 'msg) ->
  state:Tree.State.t ->
  unit ->
  'msg t
(** [tree ()] is a tree view. Widget-specific visual options follow
    {!Mosaic_ui.Tree}, which lists the keys the tree acts on when focused;
    layout and paint use the labelled arguments above. *)

module Reconciler = Reconciler
