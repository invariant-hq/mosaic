(** Virtual node tree for declarative UI descriptions.

    A vnode describes what a portion of the UI should look like at a given point
    in time. The [view] function returns a ['msg t]; the runtime maps message
    handlers to [unit] via {!val-map} before passing the tree to the reconciler.

    This module lives in {!Mosaic_ui} so view functions can be tested as pure
    functions, independently of the reconciler. *)

(** {1:handlers Handlers} *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg option) option;
  on_key : (Event.key -> 'msg option) option;
  on_paste : (Event.paste -> 'msg option) option;
  on_click : (Event.mouse -> 'msg option) option;
  on_focus_request : (unit -> 'msg) option;
  on_size : (w:int -> h:int -> 'msg option) option;
}
(** The type for generic terminal event handlers, parameterised by the message
    type. Raw handlers return [None] to ignore an event. [on_focus_request] is
    invoked lazily on a pointer focus request, never while mapping a view.
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
  | Canvas
  | Picture of Matrix_grid.picture
  | Spinner of Spinner.Props.t
  | Progress_bar of Progress_bar.Props.t
  | Scroll_bar of Scroll_bar.Props.t
  | Scroll_box of Scroll_box.Props.t
  | Textarea of Textarea.Props.t
  | Table of Table.Props.t
  | Code of Code.Props.t
  | Line_number of Line_number.Props.t
  | Markdown of Markdown.Props.t
  | Diff of Diff.Props.t
  | Tree of Tree.Props.t
      (** The type for widget kind and visual configuration. Values are
          monomorphic — they are shared by reference across {!val-map} and used
          for physical-equality fast paths in the reconciler. *)

(** {1:callbacks Widget callbacks} *)

(** The type for per-widget semantic callbacks, parameterised by the message
    type. Separated from {!type-handlers} so that adding new widget callbacks
    does not pollute the generic handler record. Separated from {!type-kind} so
    that visual props remain monomorphic. *)
type 'msg widget_callbacks =
  | No_callbacks
  | Slider_callbacks of { on_value_change : (float -> 'msg) option }
  | Input_callbacks of {
      on_change : (Editor.t -> 'msg) option;
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
  | Canvas_callbacks of { on_draw : (Canvas.t -> dt:float -> unit) option }
  | Scroll_bar_callbacks of { on_change : (int -> 'msg) option }
  | Scroll_box_callbacks of { on_scroll : (Scroll.t -> 'msg) option }
  | Textarea_callbacks of {
      on_change : (Editor.t -> 'msg) option;
      on_submit : (string -> 'msg) option;
    }
  | Code_callbacks of {
      on_selection : ((int * int) option -> 'msg option) option;
    }
  | Markdown_callbacks of {
      on_selection : (string option -> 'msg option) option;
    }
  | Diff_callbacks of { on_line_click : (Diff.line_hit -> 'msg option) option }
  | Table_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
      on_hover : (int option -> 'msg option) option;
    }
  | Tree_callbacks of {
      on_change : (Tree.State.t -> 'msg) option;
      on_activate : (Tree.item -> 'msg) option;
    }
  | Modal_callbacks of { on_close : (unit -> 'msg) option }
      (** A box with these callbacks is a modal ({!val-modal}). *)

(** {1:attrs Attributes} *)

type attrs = {
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  focused : bool;
  buffered : bool;
  live : bool;
}
(** The type for non-handler element attributes. Grouped for zero-copy sharing
    across {!val-map} and physical-equality fast paths in the reconciler.

    [focused] declares application-owned focus and implies [focusable]. At most
    one visible element that is not inert may declare focus. The application
    runtime synchronizes this declaration with the renderer after
    reconciliation. *)

(** {1:vnodes Virtual nodes} *)

type 'msg memo
(** The type for a deferred subtree: a view function, the key it is applied to,
    and the message mappers applied since. See {!val-memo}. *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Viewport_switch of 'msg viewport_switch
  | Memo of 'msg memo
  | Embed of Renderable.t
  | Themed of (Theme.t -> Theme.t) * 'msg t
  | Empty
      (** The type for a virtual node in the UI tree.

          - [Element] is a widget with kind, attributes, handlers, and children.
          - [Fragment] groups multiple vnodes without a wrapper element.
          - [Viewport_switch] selects one branch from the current terminal
            viewport width before reconciliation and layout.
          - [Embed] wraps an existing {!Renderable.t} node. The node is attached
            but not managed by the reconciler. Use this as an escape hatch for
            imperative code.
          - [Themed] draws a vnode with a theme ({!val-themed}).
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

and 'msg viewport_switch = {
  at_least_width : int;
      (** Inclusive viewport-width threshold, in terminal cells. *)
  wide : 'msg t;  (** Branch selected at or above [at_least_width]. *)
  narrow : 'msg t;  (** Branch selected below [at_least_width]. *)
}
(** The type for a viewport-conditioned branch. Exactly one branch is reconciled
    at a time. *)

(** {1:constructors Constructors} *)

val empty : 'msg t
(** [empty] is the {!Empty} vnode. Useful for conditional rendering. *)

val fragment : 'msg t list -> 'msg t
(** [fragment children] is a {!Fragment} grouping [children] without a wrapper
    element. *)

val embed : Renderable.t -> 'msg t
(** [embed node] is an {!Embed} vnode wrapping [node]. The renderable is
    attached by the reconciler but not otherwise managed. *)

val themed : (Theme.t -> Theme.t) -> 'msg t -> 'msg t
(** [themed f child] is [child] drawn with [f] applied to the theme in scope
    around it ({!Theme}). It is no element of its own: it changes neither the
    layout nor the focus, and the reconciler gives [f] to each node [child]
    places ({!Renderable.Private.set_place_theme}). In
    [themed f (themed g child)], [child] is drawn with [g] applied to [f]'s
    theme. An embedded node ({!val-embed}) is in the scope its place declares,
    and its own function ({!Renderable.set_theme}), if any, applies after that
    scope. *)

val memo : key:'key -> ('key -> 'msg t) -> 'msg t
(** [memo ~key view] defers [view key]. A reconciler that mounted an equal memo
    at the same position reuses that subtree instead of calling [view].

    Two memos are equal when they have the same [view] function, compared
    physically, equal keys, and the same message mappers ({!val-map}). Keys are
    equal when they have the same representation: the same constructors and
    integers, the same characters, and the same floating-point bits, so [nan]
    equals itself and [0.] differs from [-0.]. Since a [view] receives only its
    key, equal memos build equal subtrees whatever the key's type.

    So that reuse is sound, pass a [view] that is defined once (for example at
    top level) and captures nothing that changes, and put every value the
    subtree depends on in [key]. A closure allocated by each view call is a new
    function: it never matches, and its subtree is rebuilt on every render. Keys
    must be immutable and acyclic.

    Raises [Invalid_argument] if [key] contains a function, an object or an
    abstract value, which have no representation to compare. *)

val memo_equal : 'a memo -> 'a memo -> bool
(** [memo_equal a b] is [true] iff [a] and [b] are equal in the sense of
    {!val-memo}: then they build equal subtrees. *)

val memo_view : 'msg memo -> 'msg t
(** [memo_view m] calls [m]'s view function on its key, through its message
    mappers. *)

val viewport_switch :
  at_least_width:int -> wide:'msg t -> narrow:'msg t -> 'msg t
(** [viewport_switch ~at_least_width ~wide ~narrow] is [wide] when the current
    terminal viewport is at least [at_least_width] cells wide and [narrow]
    otherwise.

    Selection happens before child reconciliation and layout. The unselected
    branch is not mounted and cannot receive focus or events. When selection
    changes, the newly selected tree is reconciled normally against the old
    tree: compatible keyed elements retain their widget state, while unmatched
    elements are unmounted.

    This condition always refers to the terminal viewport, not an element's
    allocated width. Raises [Invalid_argument] if [at_least_width] is not
    positive. *)

val box :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?inert:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
    - [inert] makes the box and its descendants inert ({!Renderable.set_inert}).
      Defaults to [false].
    - [border] enables border rendering. Defaults to [false]; also enabled
      automatically when any border option is set.
    - [border_style] is the border character set. Defaults to
      {!Matrix_grid.Border.single}.
    - [border_sides] is the set of sides on which the border is drawn. Defaults
      to all four sides.
    - [border_width] is the border width of each side, in whole cells, as
      {!Box.val-create} lays it out. Defaults to one cell on each side. The box
      replaces the border of [style] with its border widths.
    - [border_color] is the border color when unfocused.
    - [focused_border_color] is the border color when focused.
    - [background] is the background fill color.
    - [fill] controls whether the background fills the interior. Defaults to
      [true].
    - [title] is the text rendered in the top border.
    - [title_alignment] is the alignment of the title text. Defaults to [`Left].
*)

val modal : ?key:string -> ?on_close:'msg -> 'msg t -> 'msg t
(** [modal child] is a box that centres [child] and that, while it is shown and
    set as the renderer's modal node ({!Renderer.set_modal}), is in the top
    layer: laid out against the root, painted above everything and unclipped,
    with every other element inert. Of several modals shown, the last in tree
    order is the one. [on_close] is the message for a request to close it, the
    Escape key. The application runtime sets the modal node and delivers
    [on_close]. *)

val text :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
      Defaults to [false].

    Raises [Invalid_argument] if [tab_width < 1]. *)

val rich_text :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
    options have the same meaning as {!text}. Switching between {!text} and
    [rich_text] retains the text widget.

    Raises [Invalid_argument] if [tab_width < 1]. *)

val slider :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
  ?key_bindings:Text_input.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [input ()] is a single-line text input leaf element. Input does not accept
    children.

    Input-specific attributes:
    - [focusable] controls whether the node can receive focus. Defaults to
      [true].
    - [value] is the controlled text content. Every reconciliation restores it
      if the live edit buffer has diverged. Defaults to [""].
    - [cursor] is an optional controlled cursor grapheme offset.
    - [selection] is an optional controlled selection range.
    - [placeholder] is the text shown when the input is empty, regardless of
      focus. Defaults to [""].
    - [max_length] is the maximum grapheme cluster count. Defaults to [1000].
    - [text_color] is the text color when unfocused. Defaults to
      {!Ansi.Color.white}.
    - [background_color] is the background color when unfocused.
    - [focused_text_color] is the text color when focused. Defaults to
      {!Ansi.Color.white}.
    - [focused_background_color] is the background color when focused.
    - [placeholder_color] is the placeholder text color. Defaults to
      {!Ansi.Color.bright_black}.
    - [selection_color] is the selection background color. Defaults to
      {!Ansi.Color.blue}.
    - [selection_fg] is the selection foreground color. When unset, the normal
      text color is used.
    - [cursor_style] is the cursor shape when focused. Defaults to [`Block].
    - [cursor_color] is the cursor color when focused. Defaults to
      {!Ansi.Color.white}.
    - [cursor_blinking] controls whether the cursor blinks. Defaults to [true].
    - [selectable] controls whether mouse selection is enabled. Defaults to
      [true].
    - [show_cursor] controls whether the focused cursor is shown. Defaults to
      [true].
    - [on_input] is called after every text change at keystroke-level.
    - [on_change] is called when the committed value changes (on blur or
      submit).
    - [on_submit] is called when Enter is pressed.
    - [on_cursor] is called when cursor position or selection changes. *)

val select :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
  ?key_bindings:(Shortcut.t * Select.action) list ->
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
  ?key_bindings:(Shortcut.t * Tab_select.action) list ->
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?on_draw:(Canvas.t -> dt:float -> unit) ->
  unit ->
  'msg t
(** [canvas ()] is a canvas leaf element for custom cell-level drawing. Canvas
    does not accept children.

    The [on_draw] callback fires each render pass, after the canvas has been
    resized to match the current layout. Inside the callback, {!Canvas.width}
    and {!Canvas.height} reflect the current dimensions. Content from previous
    frames persists; call {!Canvas.clear} first when a full redraw is required.

    Canvas-specific attributes:
    - [on_draw] is the render-time drawing callback. The [~dt] argument is
      elapsed time in seconds since the last frame. Combine with [~live:true]
      for per-frame animation. *)

val picture :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  Matrix_grid.picture ->
  'msg t
(** [picture p] is a leaf element whose box shows [p], fit in it with its aspect
    ratio kept and centered ({!Matrix_grid.draw_picture}). It keeps [p]'s ratio
    of columns to rows at the terminal's cell size
    ({!Renderable.Private.cell_size}, 10 by 20 pixels while unknown) through
    layout, as a replaced element keeps its aspect ratio, unless its style sets
    one: a size given on one axis, by the style or the layout, sets the other.
    Where nothing gives a size, it is [p]'s size in cells,
    [ceil (width / cell width)] by [ceil (height / cell height)], shrunk to the
    space available, and it shrinks down to one cell wide. A box of another
    shape, with both sizes given, shows the picture letterboxed. *)

val spinner :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?frame_set:Spinner.frame_set ->
  ?color:Ansi.Color.t ->
  unit ->
  'msg t
(** [spinner ()] is an animated spinner leaf element. Spinner does not accept
    children.

    The spinner cycles through Unicode frame strings at a fixed period driven by
    the frame-based animation system.

    Spinner-specific attributes:
    - [live] lets the spinner animate. While it is [false] the spinner shows its
      current frame and requests no frames. Defaults to [true].
    - [frame_set] is the set of animation frames and their period. Defaults to
      {!Spinner.dots}.
    - [color] is the spinner foreground color. Defaults to {!Ansi.Color.white}.
*)

val progress_bar :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?position:int ->
  ?scroll_size:int ->
  ?viewport_size:int ->
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
(** [scroll_box ~scroll ~on_scroll children] is a scrollable container with
    application-owned scroll state. A wheel event is consumed only when it
    changes the position on an enabled axis; otherwise it bubbles to an
    enclosing scroll box. [scroll_x] defaults to [false], [scroll_y] to [true].

    [show_scrollbars] defaults to [true] and displays bars on overflowing axes.
    [scrollbar_style] sets their shared appearance. [vertical_bar_style] and
    [horizontal_bar_style] each replace it for that axis; absent styles use
    {!Scroll_bar.style}. Appearance does not change the scroll state. *)

val textarea :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?annotations:Text_buffer.Annotations.t ->
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
  ?key_bindings:Textarea.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  unit ->
  'msg t
(** [textarea ~state ~on_change ()] displays the accepted editor state. Editing,
    cursor movement, and selection propose complete {!Editor.t} values through
    [on_change]. The application decides which proposals to install. [on_submit]
    reports the current text on Ctrl+Enter or Cmd+Enter.

    [annotations] styles source byte ranges without copying text. Its source
    bytes must match [state]; stale annotations raise [Invalid_argument] before
    changing the widget. [syntax] highlights the text as {!Code} does, and
    [annotations] go over it. Other visual options follow {!Textarea}. *)

val table :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?columns:Table.column list ->
  ?rows:Table.cell array list ->
  ?selected_row:int ->
  ?border:bool ->
  ?border_style:Matrix_grid.Border.t ->
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
  ?on_change:(int -> 'msg) ->
  ?on_activate:(int -> 'msg) ->
  ?on_hover:(int option -> 'msg option) ->
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
      {!Matrix_grid.Border.single}.
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
    - [selection_visible] controls whether selected-row colors are drawn while
      retaining the current row for navigation and scrolling. Defaults to
      [true].
    - [show_scroll_indicator] shows a trailing directional overflow indicator
      when rows do not fit. Defaults to [false].
    - [activate_on_click] activates a data row on left click after selecting it.
      Defaults to [false].
    - [wheel_navigation] controls whether wheel events change selection and are
      consumed. Defaults to [true].
    - [row_styles] is a list of styles cycled by modulo across rows for
      alternating row styling. Defaults to [[]] (no alternation).
    - [wrap_selection] controls whether selection wraps at row boundaries.
      Defaults to [false].
    - [fast_scroll_step] is the number of rows skipped with Shift+Up/Down.
      Defaults to [5].
    - [on_change] is called when the selected row changes.
    - [on_activate] is called when the current row is activated via Enter.
    - [on_hover] is called with the complete-data index under the pointer, or
      [None] when no data row is under it.

    PageUp/PageDown move by the measured number of visible data rows and clamp
    at the table boundaries even when [wrap_selection] is [true]. *)

val code :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?syntax:Code.syntax ->
  ?conceal:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  string ->
  'msg t
(** [code content] is a code display leaf element showing [content]. Code does
    not accept children.

    Code-specific attributes:
    - [syntax] configures source-code highlighting. Defaults to plain text.
    - [conceal] applies the captures' conceal metadata. Defaults to [true].
    - [text_style] is the base ANSI style applied to the text. Defaults to
      {!Ansi.Style.default}.
    - [wrap] is the line-wrapping mode. Defaults to [`None].
    - [tab_width] is the tab-stop width in columns. Defaults to [4].
    - [truncate] controls whether overlong lines are truncated with an ellipsis.
      Defaults to [false].
    - [selectable] controls whether text can be mouse-selected. Defaults to
      [true].
    - [selection_bg] is the selection background color.
    - [selection_fg] is the selection foreground color.
    - [on_selection] is called when the current selection changes.

    Raises [Invalid_argument] if [tab_width < 1]. *)

val line_number :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
    gutter discovers the child via a {!Renderable.text_rows} provider and draws
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
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
(** [markdown content] is a markdown display leaf element. Parses [content] as
    CommonMark and renders headings, paragraphs, code blocks, lists,
    blockquotes, tables, and inline formatting. Markdown does not accept
    children.

    Markdown-specific attributes:
    - [md_style] is the element style. Styles compare by value, so an equal
      style rebuilt on every view re-renders nothing. Defaults to
      {!Markdown.Style.default}.
    - [conceal] controls whether markdown syntax characters are hidden. Defaults
      to [true].
    - [streaming] enables graceful handling of incomplete trailing content,
      useful when content arrives incrementally. Defaults to [false].
    - [selectable] controls text selection on rendered text children. Defaults
      to [true].
    - [selection_bg] and [selection_fg] are the selection colours.
    - [highlighter] highlights fenced code blocks that name a language; each
      fenced block is rendered as a borderless {!Code} view. It is a resource
      compared by identity: create it once. See {!Markdown.create}.
    - [syntax_style] maps highlight scopes to styles. Defaults to
      {!Syntax_style.default}.
    - [on_selection] is called with selected rendered text, or [None] when no
      text is selected. *)

val diff :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
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
(** [diff patch] is a diff display leaf element rendering [patch].

    Diff-specific attributes:
    - [layout] is the view layout. Defaults to [Diff.Unified].
    - [theme] is the colour theme. Defaults to {!Diff.default_theme}.
    - [highlight] is optional highlighter-backed split-side syntax.
    - [line_highlights] are source-line background highlights, using inclusive
      1-based old/new source line numbers. Earlier entries win when ranges
      overlap. Defaults to [[]].
    - [line_spans] are source-line sub-range highlights: a byte range within one
      1-based old/new source line, drawn as a background over its cells.
      Defaults to [[]]. See {!Diff.line_span}.
    - [line_signs] are source-line gutter signs, using inclusive 1-based old/new
      source line numbers. Earlier entries win when ranges overlap. Defaults to
      [[]].
    - [show_line_numbers] controls whether gutters are shown. Defaults to
      [true].
    - [wrap] is the wrap mode for embedded {!Code} children. Defaults to
      [`None].
    - [selectable] controls text selection on embedded {!Code} children.
      Defaults to [true].
    - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.
    - [on_line_click] is called with a semantic diff hit when the user clicks a
      rendered diff line without dragging. *)

val tree :
  ?key:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?items:Tree.item list ->
  ?indent_size:int ->
  ?show_guides:bool ->
  ?guide_style:Matrix_grid.Border.t ->
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
      {!Matrix_grid.Border.single}.
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
