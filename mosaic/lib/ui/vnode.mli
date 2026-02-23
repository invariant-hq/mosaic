(** Virtual node tree for declarative UI descriptions.

    A vnode describes what a portion of the UI should look like at a given point
    in time. The [view] function returns a ['msg t]; the runtime maps message
    handlers to [unit] via {!val-map} before passing the tree to the reconciler.

    This module lives in {!Mosaic_ui} so view functions can be tested as pure
    functions, independently of the reconciler. *)

(** {1:handlers Handlers} *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg) option;
  on_key : (Event.key -> 'msg) option;
  on_paste : (Event.paste -> 'msg) option;
}
(** The type for generic terminal event handlers, parameterised by the message
    type. These are low-level input events that any widget can receive.
    Widget-specific semantic callbacks (e.g. slider value changes) live in
    {!type-widget_callbacks}. *)

val no_handlers : 'msg handlers
(** [no_handlers] is a {!type-handlers} value with all fields set to [None]. *)

(** {1:kind Element kind} *)

type kind =
  | Box of Box.Props.t
  | Text of Text.Props.t
  | Slider of Slider.Props.t
  | Text_input of Text_input.Props.t
  | Select of Select.Props.t
  | Tab_select of Tab_select.Props.t
  | Canvas of Canvas.Props.t
  | Spinner of Spinner.Props.t
  | Progress_bar of Progress_bar.Props.t
  | Scroll_bar of Scroll_bar.Props.t
  | Scroll_box of Scroll_box.Props.t
  | Textarea of Textarea.Props.t
  | Table of Table.Props.t
  | Code of Code.Props.t
  | Line_number of Line_number.Props.t
  | Markdown of Markdown.Props.t
  | Tree of Tree.Props.t
      (** The type for widget kind and visual configuration. Values are
          monomorphic — they are shared by reference across {!val-map} and used
          for physical-equality fast paths in the reconciler. *)

(** {1:callbacks Widget callbacks} *)

type 'msg widget_callbacks =
  | No_callbacks
  | Slider_callbacks of { on_value_change : (float -> 'msg) option }
  | Input_callbacks of {
      on_input : (string -> 'msg) option;
      on_change : (string -> 'msg) option;
      on_submit : (string -> 'msg) option;
    }
  | Select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Tab_select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Canvas_callbacks of { on_draw : (Canvas.t -> delta:float -> unit) option }
  | Scroll_bar_callbacks of { on_change : (int -> 'msg) option }
  | Scroll_box_callbacks of { on_scroll : (x:int -> y:int -> 'msg) option }
  | Textarea_callbacks of {
      on_input : (string -> 'msg) option;
      on_change : (string -> 'msg) option;
      on_submit : (string -> 'msg) option;
    }
  | Table_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Tree_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
      on_expand : (int -> bool -> 'msg) option;
    }
      (** The type for per-widget semantic callbacks, parameterised by the
          message type. Separated from {!type-handlers} so that adding new
          widget callbacks does not pollute the generic handler record.
          Separated from {!type-kind} so that visual props remain monomorphic.
      *)

(** {1:attrs Attributes} *)

type attrs = {
  id : string option;
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  autofocus : bool;
  buffered : bool;
  live : bool;
  ref : (Renderable.t -> unit) option;
}
(** The type for non-handler element attributes. Grouped for zero-copy sharing
    across {!val-map} and physical-equality fast paths in the reconciler. *)

(** {1:vnodes Virtual nodes} *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Embed of Renderable.t
  | Empty
      (** The type for a virtual node in the UI tree.

          - [Element] is a widget with kind, attributes, handlers, and children.
          - [Fragment] groups multiple vnodes without a wrapper element.
          - [Embed] wraps an existing {!Renderable.t} node. The node is attached
            but not managed by the reconciler. Use this as an escape hatch for
            imperative code.
          - [Empty] renders nothing. *)

and 'msg element = {
  kind : kind;
  key : string option;
  attrs : attrs;
  handlers : 'msg handlers;
  callbacks : 'msg widget_callbacks;
  children : 'msg t list;
}
(** The type for an element vnode, carrying its {!type-kind}, an optional
    reconciliation key, {!type-attrs}, generic {!type-handlers}, widget-specific
    {!type-widget_callbacks}, and children. *)

(** {1:constructors Constructors} *)

val empty : 'msg t
(** [empty] is the {!Empty} vnode. Useful for conditional rendering. *)

val fragment : 'msg t list -> 'msg t
(** [fragment children] is a {!Fragment} grouping [children] without a wrapper
    element. *)

val embed : Renderable.t -> 'msg t
(** [embed node] is an {!Embed} vnode wrapping [node]. The renderable is
    attached by the reconciler but not otherwise managed. *)

val box :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?border:bool ->
  ?border_style:Grid.Border.t ->
  ?border_sides:Grid.Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  'msg t list ->
  'msg t
(** [box children] is a box container element holding [children].

    Common attributes:
    - [key] is the reconciliation key for stable identity across re-renders.
      Defaults to none.
    - [style] is the layout style. Defaults to {!Toffee.Style.default}.
    - [visible] controls visibility. Defaults to [true].
    - [z_index] is the rendering order among siblings. Defaults to [0].
    - [opacity] is the opacity in \[[0.0];[1.0]\]. Defaults to [1.0].
    - [focusable] controls whether the node can receive focus. Defaults to
      [false].
    - [buffered] enables offscreen buffering. When [true], the node renders into
      a private grid that is blitted to the parent. Defaults to [false].
    - [live] enables continuous rendering. When [true], the renderer runs every
      frame rather than only on request. Defaults to [false].

    Box-specific attributes:
    - [border] enables border rendering. Defaults to [false]; also enabled
      automatically when any border option is set.
    - [border_style] is the border character set. Defaults to
      {!Grid.Border.single}.
    - [border_sides] is the set of sides on which the border is drawn. Defaults
      to all four sides.
    - [border_color] is the border color when unfocused.
    - [focused_border_color] is the border color when focused.
    - [background] is the background fill color.
    - [fill] controls whether the background fills the interior. Defaults to
      [true].
    - [title] is the text rendered in the top border.
    - [title_alignment] is the alignment of the title text. Defaults to [`Left].
*)

val text :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  string ->
  'msg t
(** [text content] is a text leaf element displaying [content]. Text does not
    accept children.

    Text-specific attributes:
    - [text_style] is the ANSI style applied to the text. Defaults to
      {!Ansi.Style.default}.
    - [wrap] is the line-wrapping mode. Defaults to [`None].
    - [selectable] controls whether text can be mouse-selected. Defaults to
      [true].
    - [selection_bg] is the selection background color.
    - [selection_fg] is the selection foreground color.
    - [tab_width] is the tab-stop width in columns. Defaults to [2].
    - [truncate] controls whether overlong lines are truncated with an ellipsis.
      Defaults to [false]. *)

val slider :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?orientation:Slider.orientation ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?viewport_size:float ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?on_value_change:(float -> 'msg) ->
  unit ->
  'msg t
(** [slider ()] is a slider leaf element. Slider does not accept children.

    Slider-specific attributes:
    - [orientation] is the track direction. Defaults to [`Horizontal].
    - [value] is the current value. Defaults to [min].
    - [min] is the lower bound. Defaults to [0.0].
    - [max] is the upper bound. Defaults to [100.0].
    - [viewport_size] is the visible-portion size, which controls the thumb
      length. Defaults to 10% of the range.
    - [track_color] is the track background color. Defaults to dark gray.
    - [thumb_color] is the thumb foreground color. Defaults to medium gray.
    - [on_value_change] is called when the slider value changes. *)

val input :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?value:string ->
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
  ?on_input:(string -> 'msg) ->
  ?on_change:(string -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  unit ->
  'msg t
(** [input ()] is a single-line text input leaf element. Input does not accept
    children.

    Input-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [value] is the initial text content. Defaults to [""].
    - [placeholder] is the text shown when the input is empty, regardless of
      focus. Defaults to [""].
    - [max_length] is the maximum grapheme cluster count. Defaults to [1000].
    - [text_color] is the text color when unfocused. Defaults to
      {!Ansi.Color.White}.
    - [background_color] is the background color when unfocused.
    - [focused_text_color] is the text color when focused. Defaults to
      {!Ansi.Color.White}.
    - [focused_background_color] is the background color when focused.
    - [placeholder_color] is the placeholder text color. Defaults to
      {!Ansi.Color.Bright_black}.
    - [selection_color] is the selection background color. Defaults to
      {!Ansi.Color.Blue}.
    - [selection_fg] is the selection foreground color. When unset, the normal
      text color is used.
    - [cursor_style] is the cursor shape when focused. Defaults to [`Block].
    - [cursor_color] is the cursor color when focused. Defaults to
      {!Ansi.Color.White}.
    - [cursor_blinking] controls whether the cursor blinks. Defaults to [true].
    - [on_input] is called after every text change at keystroke-level.
    - [on_change] is called when the committed value changes (on blur or
      submit).
    - [on_submit] is called when Enter is pressed. *)

val select :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?options:Select.item list ->
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
  ?on_change:(int -> 'msg) ->
  ?on_activate:(int -> 'msg) ->
  unit ->
  'msg t
(** [select ()] is a vertical selection list leaf element. Select does not
    accept children.

    Select-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [options] is the list of items to display. Defaults to [[]].
    - [selected_index] is the initial selection index. Defaults to [0].
    - [background] is the item background color.
    - [text_color] is the item text color.
    - [focused_background] is the item background color when the widget is
      focused.
    - [focused_text_color] is the item text color when the widget is focused.
    - [selected_background] is the selected item background color.
    - [selected_text_color] is the selected item text color.
    - [description_color] is the description text color.
    - [selected_description_color] is the selected item description color.
    - [show_description] controls whether description lines are displayed.
      Defaults to [true].
    - [show_scroll_indicator] controls whether a scroll indicator is displayed.
      Defaults to [false].
    - [wrap_selection] controls whether selection wraps at list boundaries.
      Defaults to [false].
    - [item_spacing] is the vertical spacing between items in rows. Defaults to
      [0].
    - [fast_scroll_step] is the number of items skipped with Shift+Up/Down.
      Defaults to [5].
    - [on_change] is called when the selection index changes.
    - [on_activate] is called when the current item is activated via Enter. *)

val tab_select :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  options:Tab_select.item list ->
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
  ?on_change:(int -> 'msg) ->
  ?on_activate:(int -> 'msg) ->
  unit ->
  'msg t
(** [tab_select ~options ()] is a horizontal tab selection leaf element.
    Tab_select does not accept children.

    Tab_select-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [options] is the list of tab items to display. Required.
    - [selected] is the initial selection index. Defaults to [0].
    - [tab_width] is the fixed width of each tab cell in columns. Defaults to
      [12].
    - [background] is the tab background color.
    - [text_color] is the tab text color.
    - [focused_background] is the tab background color when the widget is
      focused.
    - [focused_text_color] is the tab text color when the widget is focused.
    - [selected_background] is the selected tab background color.
    - [selected_text_color] is the selected tab text color.
    - [description_color] is the description text color.
    - [selected_description_color] is the selected tab description color.
    - [show_underline] controls whether a selection underline is displayed.
      Defaults to [true].
    - [show_description] controls whether the description line below the tabs is
      displayed. Defaults to [false].
    - [show_scroll_arrows] controls whether scroll indicator arrows are
      displayed. Defaults to [true].
    - [wrap_selection] controls whether selection wraps at tab boundaries.
      Defaults to [false].
    - [on_change] is called when the selection index changes.
    - [on_activate] is called when the current tab is activated via Enter. *)

val canvas :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?respect_alpha:bool ->
  ?on_draw:(Canvas.t -> delta:float -> unit) ->
  unit ->
  'msg t
(** [canvas ()] is a canvas leaf element for custom cell-level drawing. Canvas
    does not accept children.

    The [on_draw] callback fires each render pass, after the canvas has been
    resized to match the current layout. Inside the callback, {!Canvas.width}
    and {!Canvas.height} reflect the current dimensions. Content from previous
    frames persists; call {!Canvas.clear} first when a full redraw is required.

    Canvas-specific attributes:
    - [respect_alpha] controls whether alpha blending is honoured when drawing.
      Defaults to [false].
    - [on_draw] is the render-time drawing callback. The [~delta] argument is
      elapsed time in milliseconds since the last frame. Combine with
      [~live:true] for per-frame animation. *)

val spinner :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?frame_set:Spinner.frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  'msg t
(** [spinner ()] is an animated spinner leaf element. Spinner does not accept
    children.

    The spinner cycles through Unicode frame strings at a fixed interval driven
    by the frame-based animation system.

    Spinner-specific attributes:
    - [live] enables continuous rendering. Defaults to [true]; spinners animate
      by default.
    - [frame_set] is the set of animation frames and their interval. Defaults to
      {!Spinner.dots}.
    - [color] is the spinner foreground color. Defaults to {!Ansi.Color.White}.
*)

val progress_bar :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?value:float ->
  ?min:float ->
  ?max:float ->
  ?orientation:[ `Horizontal | `Vertical ] ->
  ?filled_color:Ansi.Color.t ->
  ?empty_color:Ansi.Color.t ->
  unit ->
  'msg t
(** [progress_bar ()] is a progress bar leaf element. Progress_bar does not
    accept children.

    The bar is rendered with sub-cell precision using Unicode half-block
    characters to show the filled and empty portions.

    Progress_bar-specific attributes:
    - [value] is the current progress value. Defaults to [0.0].
    - [min] is the lower bound. Defaults to [0.0].
    - [max] is the upper bound. Defaults to [1.0].
    - [orientation] is the bar fill direction. Defaults to [`Horizontal].
    - [filled_color] is the filled portion color. Defaults to medium gray.
    - [empty_color] is the empty portion color. Defaults to dark gray. *)

val scroll_bar :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?orientation:Scroll_bar.orientation ->
  ?show_arrows:bool ->
  ?track_color:Ansi.Color.t ->
  ?thumb_color:Ansi.Color.t ->
  ?arrow_fg:Ansi.Color.t ->
  ?arrow_bg:Ansi.Color.t ->
  ?on_change:(int -> 'msg) ->
  unit ->
  'msg t
(** [scroll_bar ()] is a scroll bar leaf element. Scroll_bar does not accept
    children.

    Scroll_bar-specific attributes:
    - [orientation] is the scroll bar direction. Defaults to [`Vertical].
    - [show_arrows] controls whether arrow buttons are displayed at each end.
      Defaults to [false].
    - [track_color] is the track background color.
    - [thumb_color] is the thumb foreground color.
    - [arrow_fg] is the arrow button foreground color.
    - [arrow_bg] is the arrow button background color.
    - [on_change] is called when the scroll position changes. *)

val scroll_box :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?sticky_scroll:bool ->
  ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
  ?background:Ansi.Color.t ->
  ?on_scroll:(x:int -> y:int -> 'msg) ->
  'msg t list ->
  'msg t
(** [scroll_box children] is a scrollable container element. Children are
    attached to an internal content node.

    Scroll_box-specific attributes:
    - [scroll_x] enables horizontal scrolling. Defaults to [false].
    - [scroll_y] enables vertical scrolling. Defaults to [true].
    - [sticky_scroll] makes the viewport stick to [sticky_start] as content
      grows. Defaults to [false].
    - [sticky_start] is the edge to stick to when [sticky_scroll] is [true].
      Defaults to [`Bottom].
    - [background] is the optional background fill color.
    - [on_scroll] is called when the scroll position changes, receiving the new
      offsets as [~x] and [~y]. *)

val textarea :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?value:string ->
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
  ?on_input:(string -> 'msg) ->
  ?on_change:(string -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  unit ->
  'msg t
(** [textarea ()] is a multi-line text editing leaf element. Textarea does not
    accept children.

    Textarea-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [value] is the initial text content. Defaults to [""].
    - [placeholder] is the text shown when the textarea is empty, regardless of
      focus. Defaults to [""].
    - [wrap] is the line-wrapping mode. Defaults to [`Word].
    - [text_color] is the text color when unfocused. Defaults to
      {!Ansi.Color.White}.
    - [background_color] is the background color when unfocused.
    - [focused_text_color] is the text color when focused. Defaults to
      {!Ansi.Color.White}.
    - [focused_background_color] is the background color when focused.
    - [placeholder_color] is the placeholder text color. Defaults to
      {!Ansi.Color.Bright_black}.
    - [selection_color] is the selection background color. Defaults to
      {!Ansi.Color.Blue}.
    - [selection_fg] is the selection foreground color. When unset, the normal
      text color is used.
    - [cursor_style] is the cursor shape when focused. Defaults to [`Block].
    - [cursor_color] is the cursor color when focused. Defaults to
      {!Ansi.Color.White}.
    - [cursor_blinking] controls whether the cursor blinks. Defaults to [true].
    - [on_input] is called after every text change at keystroke-level.
    - [on_change] is called when the committed value changes (on blur or
      submit).
    - [on_submit] is called when Cmd+Enter or Ctrl+Enter is pressed. *)

val table :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?columns:Table.column list ->
  ?rows:Table.cell array list ->
  ?selected_row:int ->
  ?border:bool ->
  ?border_style:Grid.Border.t ->
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
  ?on_change:(int -> 'msg) ->
  ?on_activate:(int -> 'msg) ->
  unit ->
  'msg t
(** [table ()] is a data table leaf element. Table does not accept children.

    Table-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [columns] is the list of column specifications. Defaults to [[]].
    - [rows] is the list of data rows, each a [cell array]. Defaults to [[]].
    - [selected_row] is the initial selection index. Defaults to [0].
    - [border] enables the outer border and header separator. Defaults to
      [true].
    - [border_style] is the border character set. Defaults to
      {!Grid.Border.single}.
    - [show_header] controls whether the header row is shown. Defaults to
      [true].
    - [show_column_separator] controls whether vertical lines between columns
      are shown. Defaults to [false].
    - [show_row_separator] controls whether horizontal lines between rows are
      shown. Defaults to [false].
    - [cell_padding] is the horizontal padding per side of each cell in columns.
      Defaults to [0].
    - [header_color] is the header text color.
    - [header_background] is the header background color.
    - [text_color] is the row text color.
    - [background] is the row background color.
    - [selected_text_color] is the selected row text color when unfocused.
    - [selected_background] is the selected row background color when unfocused.
    - [focused_selected_text_color] is the selected row text color when focused.
    - [focused_selected_background] is the selected row background color when
      focused.
    - [row_styles] is a list of styles cycled by modulo across rows for
      alternating row styling. Defaults to [[]] (no alternation).
    - [wrap_selection] controls whether selection wraps at row boundaries.
      Defaults to [false].
    - [fast_scroll_step] is the number of rows skipped with Shift+Up/Down.
      Defaults to [5].
    - [on_change] is called when the selected row changes.
    - [on_activate] is called when the current row is activated via Enter. *)

val code :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?highlights:Text_buffer.span list ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  string ->
  'msg t
(** [code content] is a code display leaf element showing [content]. Code does
    not accept children.

    Code-specific attributes:
    - [highlights] is the list of pre-computed syntax highlighting spans.
      Defaults to [[]].
    - [text_style] is the base ANSI style applied to the text. Defaults to
      {!Ansi.Style.default}.
    - [wrap] is the line-wrapping mode. Defaults to [`None].
    - [tab_width] is the tab-stop width in columns. Defaults to [4].
    - [truncate] controls whether overlong lines are truncated with an ellipsis.
      Defaults to [false].
    - [selectable] controls whether text can be mouse-selected. Defaults to
      [true].
    - [selection_bg] is the selection background color.
    - [selection_fg] is the selection foreground color. *)

val line_number :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?fg:Ansi.Color.t ->
  ?bg:Ansi.Color.t ->
  ?min_width:int ->
  ?padding_right:int ->
  ?show_line_numbers:bool ->
  ?line_number_offset:int ->
  ?line_colors:(int * Line_number.line_color) list ->
  ?line_signs:(int * Line_number.line_sign) list ->
  ?line_numbers:(int * int) list ->
  ?hidden_line_numbers:int list ->
  'msg t ->
  'msg t
(** [line_number child] is a line-number gutter container wrapping [child]. The
    gutter discovers the child via a {!Renderable.line_info} provider and draws
    line numbers accordingly.

    Line_number-specific attributes:
    - [fg] is the line number foreground color. Defaults to medium gray.
    - [bg] is the gutter background color.
    - [min_width] is the minimum gutter width in columns. Defaults to [3].
    - [padding_right] is the padding between the number and the content in
      columns. Defaults to [1].
    - [show_line_numbers] controls whether the gutter is visible. When [false],
      the gutter is hidden entirely and takes no layout space. Defaults to
      [true].
    - [line_number_offset] is added to each logical line index when displaying
      numbers. Defaults to [0].
    - [line_colors] is a list of per-line background colors, keyed by logical
      line index. Defaults to [[]].
    - [line_signs] is a list of per-line gutter signs, keyed by logical line
      index. Defaults to [[]].
    - [line_numbers] is a list of custom line number overrides, keyed by logical
      line index. Defaults to [[]].
    - [hidden_line_numbers] is the list of logical line indices whose numbers
      are hidden. Defaults to [[]]. *)

val markdown :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?md_style:Markdown.style ->
  ?conceal:bool ->
  ?streaming:bool ->
  string ->
  'msg t
(** [markdown content] is a markdown display leaf element. Parses [content] as
    CommonMark and renders headings, paragraphs, code blocks, lists,
    blockquotes, tables, and inline formatting. Markdown does not accept
    children.

    Markdown-specific attributes:
    - [md_style] is the style resolver for markdown elements. Defaults to
      {!Markdown.default_style}.
    - [conceal] controls whether markdown syntax characters are hidden. Defaults
      to [true].
    - [streaming] enables graceful handling of incomplete trailing content,
      useful when content arrives incrementally. Defaults to [false]. *)

val tree :
  ?key:string ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg) ->
  ?on_key:(Event.key -> 'msg) ->
  ?on_paste:(Event.paste -> 'msg) ->
  ?items:Tree.item list ->
  ?selected_index:int ->
  ?expand_depth:int ->
  ?indent_size:int ->
  ?show_guides:bool ->
  ?guide_style:Grid.Border.t ->
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
  ?on_change:(int -> 'msg) ->
  ?on_activate:(int -> 'msg) ->
  ?on_expand:(int -> bool -> 'msg) ->
  unit ->
  'msg t
(** [tree ()] is a hierarchical tree leaf element. Tree does not accept
    children.

    Tree-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [items] is the list of tree items. Defaults to [[]].
    - [selected_index] is the initial selection index. Defaults to [0].
    - [expand_depth] is the initial expansion depth. [0] means all nodes are
      collapsed. Defaults to [0].
    - [indent_size] is the number of columns per depth level. Defaults to [2].
    - [show_guides] controls whether box-drawing guide lines are drawn. Defaults
      to [false].
    - [guide_style] is the guide character set. Defaults to
      {!Grid.Border.single}.
    - [expand_icon] is the icon string for expandable nodes.
    - [collapse_icon] is the icon string for collapsible nodes.
    - [leaf_icon] is the icon string for leaf nodes.
    - [background] is the item background color.
    - [text_color] is the item text color.
    - [selected_background] is the selected item background color when
      unfocused.
    - [selected_text_color] is the selected item text color when unfocused.
    - [focused_selected_background] is the selected item background color when
      focused.
    - [focused_selected_text_color] is the selected item text color when
      focused.
    - [guide_color] is the guide line color.
    - [icon_color] is the icon color.
    - [wrap_selection] controls whether selection wraps at boundaries. Defaults
      to [false].
    - [fast_scroll_step] is the number of items skipped with Shift+Up/Down.
      Defaults to [5].
    - [on_change] is called when the selected index changes.
    - [on_activate] is called when the current node is activated via Enter.
    - [on_expand] is called when a node is expanded or collapsed, receiving the
      node index and the new expansion state. *)

(** {1:transform Transformation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f vnode] is [vnode] with all handler and callback return values
    transformed by [f]. The {!type-attrs} and {!type-kind} records are shared by
    reference — no allocation occurs for non-handler fields. *)

(** {1:layout Layout helpers} *)

val px : int -> Toffee.Style.dimension
(** [px n] is a pixel dimension of [n] columns. *)

val pct : int -> Toffee.Style.dimension
(** [pct n] is a percentage dimension. [n] is in \[[0];[100]\]. *)

val auto : Toffee.Style.dimension
(** [auto] is the auto dimension. *)

val size :
  width:int -> height:int -> Toffee.Style.dimension Toffee.Geometry.size
(** [size ~width ~height] is a pixel size. *)

val gap : int -> Toffee.Style.length_percentage Toffee.Geometry.size
(** [gap n] is a uniform pixel gap of [n] columns on both axes. *)

val padding : int -> Toffee.Style.length_percentage Toffee.Geometry.rect
(** [padding n] is a uniform pixel padding of [n] columns on all sides. *)

val margin : int -> Toffee.Style.length_percentage_auto Toffee.Geometry.rect
(** [margin n] is a uniform pixel margin of [n] columns on all sides. *)

val inset : int -> Toffee.Style.length_percentage_auto Toffee.Geometry.rect
(** [inset n] is a uniform pixel inset of [n] columns on all sides. *)
