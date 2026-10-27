(** Input/navigation proposes selection through [on_change]. A proposal does not
    change the accepted selection. [apply_props] and explicit setters accept
    values without emitting another proposal. *)

(** Focusable data table with column headers and row selection.

    A table widget that displays rows of {!cell} values in typed {!column}s. The
    widget is keyboard- and mouse-navigable and automatically scrolls to keep
    the selected row visible.

    {2:keys Keys}

    When focused, the widget's behavior (see {!Renderable.set_key_behavior})
    acts on the keys below and consumes them: they reach neither its ancestors
    nor any listener after the elements, such as an application's shortcuts.
    Every other key passes on.
    - Up, Down, k, j — move the selection by one row.
    - Shift+Up, Shift+Down — move it by {!Props.t}'s [fast_scroll_step] rows.
    - PageUp, PageDown — move it by the number of data rows in the measured
      viewport, clamped at the first or last row, independently of selection
      wrapping.
    - Enter, Ctrl+J — activate the current row, firing the {!set_on_activate}
      callback if a row is selected.

    It acts on them even when the selection cannot move. [key_bindings] adds
    keys to the list or binds one in it to another action.

    {2:mouse Mouse}

    - Left click selects a row.
    - Pointer movement reports row transitions through {!set_on_hover}.
    - Scroll wheel navigates rows.

    A left press captures its row index and selection/activation callbacks
    before invoking the hover callback. Changes to rows or callbacks during the
    press do not change that captured index or those callbacks. Destroying the
    table cancels the remaining callbacks.

    Selection presentation can be hidden independently of the current row. This
    supports an unfocused glance that acquires a visible selection only after
    interaction. An opt-in vertical overflow indicator is derived from the
    table's actual viewport; it reserves one terminal column and shows [↑], [↓],
    or [↕] wherever undisplayed rows remain.

    {2:column_sizing Column sizing}

    Each column carries a {!type-width} strategy:
    - [`Auto] sizes the column to the widest cell content, including the header.
    - [`Fixed n] sets an exact width of [n] terminal columns.
    - [`Flex f] claims a proportional share of remaining space by factor [f].

    Remaining space, after [`Fixed] and [`Auto] columns are resolved, is
    distributed among [`Flex] columns in proportion to their factors.

    {2:overflow Text overflow}

    Each column carries an {!type-overflow} strategy:
    - [`Ellipsis] truncates with a ["..."] suffix (default).
    - [`Crop] hard-truncates at the column width. *)

type t
(** The type for table widgets. *)

(** {1:columns Column specification} *)

type alignment = [ `Left | `Center | `Right ]
(** The type for horizontal text alignment within a column. *)

type width = [ `Fixed of int | `Auto | `Flex of float ]
(** The type for column width strategies. See {{!column_sizing}column sizing}.
*)

type overflow = [ `Ellipsis | `Crop ]
(** The type for cell content overflow strategies. See
    {{!overflow}text overflow}. *)

type column = {
  header : string;
  width : width;
  alignment : alignment;
  overflow : overflow;
  min_width : int option;
  max_width : int option;
}
(** The type for column definitions.

    [header] is the column header text displayed in the header row. [width],
    [alignment], and [overflow] govern sizing and rendering. [min_width] and
    [max_width] constrain {!width} values of [`Auto] and [`Flex]; they are
    ignored for [`Fixed] columns. *)

val column :
  ?width:width ->
  ?alignment:alignment ->
  ?overflow:overflow ->
  ?min_width:int ->
  ?max_width:int ->
  string ->
  column
(** [column header] is a column specification with [header] as the header text
    and with:
    - [width] the column width strategy. Defaults to [`Auto].
    - [alignment] the horizontal text alignment. Defaults to [`Left].
    - [overflow] the overflow handling strategy. Defaults to [`Ellipsis].
    - [min_width] the minimum column width in terminal columns, applied to
      [`Auto] and [`Flex] columns. Defaults to [None].
    - [max_width] the maximum column width in terminal columns, applied to
      [`Auto] and [`Flex] columns. Defaults to [None]. *)

(** {1:cells Cell content} *)

type cell
(** The type for table cells. A cell holds either plain text or rich styled
    fragments (see {!val-cell} and {!rich}). *)

val cell : ?style:Ansi.Style.t -> string -> cell
(** [cell s] is a plain-text cell containing [s] with:
    - [style] an optional style that overrides the row's default style when
      provided. Defaults to [None]. *)

val rich : Text.fragment list -> cell
(** [rich fragments] is a styled cell built from [fragments]. *)

val cell_equal : cell -> cell -> bool
(** [cell_equal a b] is [true] iff [a] and [b] have identical content and style.
*)

(** {1:constructors Constructors} *)

type action =
  | Move_up
  | Move_down
  | Move_up_fast
  | Move_down_fast
  | Page_up
  | Page_down
  | Activate  (** Keyboard navigation and activation actions. *)

type key_binding = Shortcut.t * action
(** A shortcut and its action. Custom bindings override defaults; the last
    binding for a shortcut wins. Releases are ignored. *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?columns:column list ->
  ?rows:cell array list ->
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
  ?key_bindings:key_binding list ->
  ?fast_scroll_step:int ->
  unit ->
  t
(** [create ~parent ()] is a table node attached to [parent]. The labels after
    [opacity] are {!Props.make}'s and take its defaults.

    The node is focusable and uses buffered rendering. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for the declarative property bundle used for reconciler diffing.
  *)

  val make :
    ?columns:column list ->
    ?rows:cell array list ->
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
    ?key_bindings:key_binding list ->
    ?fast_scroll_step:int ->
    unit ->
    t
  (** [make ()] is a property set with:
      - [columns] the column specification list. Defaults to [[]].
      - [rows] the data rows, each a [cell array]. Defaults to [[]].
      - [selected_row] the accepted index, defaulting to [0] for a nonempty
        table. Negative values mean no selection ([-1]); positive overflow
        clamps to the last row. Empty tables have no selection. {!apply_props}
        accepts it without firing the change callback.
      - [border] enables the outer border and header separator. Defaults to
        [true].
      - [border_style] the border character set. Defaults to
        {!Matrix_grid.Border.single}.
      - [show_header] shows the header row. Defaults to [true].
      - [show_column_separator] shows vertical lines between columns. Defaults
        to [false].
      - [show_row_separator] shows horizontal lines between rows. Defaults to
        [false].
      - [cell_padding] the horizontal padding (in terminal columns) per side of
        each cell. Defaults to [0].
      - [header_color] the header text color. Defaults to white.
      - [header_background] the header background color. Defaults to dark gray.
      - [text_color] the default cell text color. Defaults to white.
      - [background] the table background color. Defaults to transparent.
      - [selected_text_color] the selected row text color. Defaults to yellow.
      - [selected_background] the selected row background. Defaults to dark
        blue.
      - [focused_selected_text_color] the focused-and-selected row text color.
        When [None], [selected_text_color] is used instead.
      - [focused_selected_background] the focused-and-selected row background.
        When [None], [selected_background] is used instead.
      - [selection_visible] controls whether the selected row receives its
        selected foreground and background. It defaults to [true]; navigation,
        activation, and automatic scrolling still use [selected_row] when it is
        [false].
      - [show_scroll_indicator] reserves one trailing column when the data rows
        exceed the current viewport and draws directional overflow marks from
        the actual scroll offset. It defaults to [false].
      - [activate_on_click] invokes the activation callback after a left click
        selects a data row. It defaults to [false].
      - [wheel_navigation] lets the mouse wheel move table selection and consume
        the event. It defaults to [true]; when [false], wheel events bubble to
        an enclosing scroll surface.
      - [row_styles] the list of alternating row styles, applied by modulo
        index. Defaults to [[]] (no alternation).
      - [wrap_selection] wraps selection at row boundaries. Defaults to [false].
      - [key_bindings] adds keys to those {!section-keys} lists or binds one of
        them to another action. Defaults to [[]].
      - [fast_scroll_step] the number of rows to skip on Shift+Up/Down. Defaults
        to [5].

      {!val-create} and {!Vnode.table} take these labels with these defaults. *)

  val default : t
  (** [default] is [make ()]. *)

  val with_rows_of : ('a -> cell array) -> 'a list -> t -> t
  (** [with_rows_of f l props] is [props] with the rows [List.map f l], where
      [f] must return equal cells for one element every time. A table given the
      same [l] and [f], physically, as by its previous properties neither
      converts nor compares its rows. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t], triggering the minimum
    necessary layout and render updates. Never fires the change callback;
    callbacks are reserved for user gestures. *)

(** {1:data Data} *)

val columns : t -> column list
(** [columns t] is the current column list of [t]. *)

val set_columns : t -> column list -> unit
(** [set_columns t cols] replaces the column specifications of [t] with [cols]
    and triggers a re-render. *)

val rows : t -> cell array list
(** [rows t] is the current data rows of [t]. *)

val set_rows : t -> cell array list -> unit
(** [set_rows t data] replaces the data rows of [t] with [data], clamping the
    selection index to the new valid range. Does not fire the {!set_on_change}
    callback. Triggers a re-render. *)

(** {1:selection Selection} *)

val selected_row : t -> int
(** [selected_row t] is the 0-based selected index, or [-1] when no row is
    selected. *)

val set_selected_row : t -> int -> unit
(** [set_selected_row t value] accepts [value] without firing [on_change].
    Negative values clear selection; positive overflow clamps to the last row.
    The result is [-1] for an empty table. From no selection, Down proposes the
    first row and Up the last. *)

val row_count : t -> int
(** [row_count t] is the number of data rows in [t]. *)

(** {1:display Display} *)

val set_border : t -> bool -> unit
(** [set_border t v] enables ([true]) or disables ([false]) the outer border and
    header separator of [t]. *)

val set_border_style : t -> Matrix_grid.Border.t -> unit
(** [set_border_style t style] sets the border character set of [t] to [style].
*)

val set_show_header : t -> bool -> unit
(** [set_show_header t v] shows ([true]) or hides ([false]) the header row of
    [t]. *)

val set_show_column_separator : t -> bool -> unit
(** [set_show_column_separator t v] shows ([true]) or hides ([false]) vertical
    separator lines between columns of [t]. *)

val set_show_row_separator : t -> bool -> unit
(** [set_show_row_separator t v] shows ([true]) or hides ([false]) horizontal
    separator lines between rows of [t]. *)

val set_cell_padding : t -> int -> unit
(** [set_cell_padding t n] sets the horizontal padding per side of each cell in
    [t] to [n] terminal columns. *)

val set_row_styles : t -> Ansi.Style.t list -> unit
(** [set_row_styles t styles] sets the alternating row styles of [t] to
    [styles], applied by modulo index. Pass [[]] to disable alternation. *)

(** {1:colors Colors} *)

val set_header_color : t -> Ansi.Color.t -> unit
(** [set_header_color t c] sets the header text color of [t] to [c]. *)

val set_header_background : t -> Ansi.Color.t -> unit
(** [set_header_background t c] sets the header background color of [t] to [c].
*)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t c] sets the default cell text color of [t] to [c]. *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t c] sets the table background color of [t] to [c]. *)

val set_selected_text_color : t -> Ansi.Color.t -> unit
(** [set_selected_text_color t c] sets the selected row text color of [t] to
    [c]. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t c] sets the selected row background color of [t]
    to [c]. *)

val set_selection_visible : t -> bool -> unit
(** [set_selection_visible t visible] controls whether the current row is drawn
    with the selected-row colors. It does not change selection, scrolling, or
    interaction behavior. *)

val set_show_scroll_indicator : t -> bool -> unit
(** [set_show_scroll_indicator t show] enables or disables the trailing viewport
    indicator. The indicator appears only while some rows do not fit. *)

val set_activate_on_click : t -> bool -> unit
(** [set_activate_on_click t activate] controls whether a left click activates
    the exact clicked row after selecting it. *)

val set_wheel_navigation : t -> bool -> unit
(** [set_wheel_navigation t enabled] controls whether wheel events navigate
    table selection. Disabled wheel events are left unconsumed for ancestors. *)

(** {1:behavior Behavior} *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t v] enables ([true]) or disables ([false]) selection
    wrapping at row boundaries for [t]. When enabled, moving past the last row
    wraps to the first, and vice versa. *)

val set_fast_scroll_step : t -> int -> unit
(** [set_fast_scroll_step t n] sets the number of rows to skip on Shift+Up/Down
    in [t] to [n]. Values below [1] are clamped to [1]. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t cb] registers [cb] as the callback to invoke when the
    selected row of [t] changes. [cb] receives the new 0-based row index. Pass
    [None] to remove the callback. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** [set_on_activate t cb] registers [cb] as the callback to invoke when the
    current row is activated via Enter or KP_enter. [cb] receives the activated
    0-based row index. Pass [None] to remove the callback. *)

val set_on_hover : t -> (int option -> unit) option -> unit
(** [set_on_hover t cb] registers [cb] for pointer row transitions. [Some i]
    identifies the row under the pointer in the complete data set, after
    applying the current scroll offset. [None] reports table chrome, blank table
    space, or the pointer leaving the table. Repeated movement within the same
    row does not invoke [cb]. A still pointer is reported again when the row
    under it changes, by index or by cells, as when a selection scrolls the
    rows, the table moves, or new rows put another row at the same index. Rows
    replaced by equal rows report nothing. Pass [None] to remove the callback.
*)

(** {1:layout Layout} *)

val set_style : t -> Toffee.Style.t -> unit
(** [set_style t style] sets the layout style of [t] to [style]. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
