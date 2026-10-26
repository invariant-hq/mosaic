(** Input/navigation proposes selection through [on_change]. A proposal does not
    change the accepted selection. [apply_props] and explicit setters accept
    values without emitting another proposal. *)

(** Vertical list selector with optional descriptions.

    A focusable, keyboard-navigable list where at most one item is highlighted
    at a time. Each {!type-item} has a label and an optional description. The
    widget automatically scrolls to keep the selected item centered in the
    viewport; rendering is buffered.

    Without explicit dimensions, the node fits its content: width includes the
    three-cell label/description indent, and height includes every item's one
    label row, an additional row when descriptions are enabled, and spacing
    between items. Empty lists measure zero in both dimensions. Enabling the
    scroll indicator reserves one additional column. Definite available space
    constrains this size; explicit dimensions take precedence. Text is clipped
    horizontally rather than wrapped.

    {2:keys Keys}

    When focused, the widget's behavior (see {!Renderable.set_key_behavior})
    acts on the keys below and consumes them: they reach neither its ancestors
    nor any listener after the elements, such as an application's shortcuts.
    Every other key passes on.
    - Up, Down, k, j — move the selection by one item.
    - Shift+Up, Shift+Down — move it by {!val-set_fast_scroll_step} items.
    - Enter — activate the selection, firing the {!val-set_on_activate} callback
      if an item is selected.

    It acts on them even when the selection cannot move. [key_bindings] adds
    keys to the list or binds one in it to another action. Bindings use
    {!Shortcut} matching: modifiers are exact apart from lock state, keypad
    equivalents and base-layout keys are recognized, and release events are
    ignored. A key handler on the widget runs first, and can keep the behavior
    from acting with {!Event.Key.prevent_default}.

    {2:mouse Mouse}

    - Left click — select the item under the cursor.
    - Scroll wheel — navigate up or down by one item.

    {2:scroll Scroll behaviour}

    The viewport displays as many items as fit within the available height. When
    an item is selected the view scrolls to center it. The
    {!val-set_show_scroll_indicator} option displays a proportional position
    indicator in the rightmost column. *)

(** {1:types Types} *)

type item = {
  label : string;  (** Display text. *)
  description : string option;  (** Optional secondary text. *)
}
(** The type for list items. *)

type action =
  | Move_up
  | Move_down
  | Move_up_fast
  | Move_down_fast
  | Activate
      (** Navigation and activation actions. Fast movement uses
          [fast_scroll_step]. *)

type t
(** A select widget backed by a {!Renderable.t}. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?options:item list ->
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
  ?key_bindings:(Shortcut.t * action) list ->
  unit ->
  t
(** [create ~parent ()] is a select node attached to [parent].

    The node is focusable and uses buffered rendering. See {!Props.make} for
    parameter defaults. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. *)

  val make :
    ?options:item list ->
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
    ?key_bindings:(Shortcut.t * action) list ->
    unit ->
    t
  (** [make ()] is a property set with the same defaults as {!val-create}, with:
      - [options] items to display. Defaults to [[]].
      - [selected_index] the accepted index. Negative values mean no selection
        ([-1]); other values clamp to the last item. The default [0] selects the
        first item of a nonempty list. Empty lists have no selection.
      - [background] unfocused background color. Defaults to transparent.
      - [text_color] unfocused text color. Defaults to white.
      - [focused_background] focused background color. Defaults to [background]
        when provided, otherwise a subtle dark gray.
      - [focused_text_color] focused text color. Defaults to [text_color].
      - [selected_background] selected item background. Defaults to dark blue.
      - [selected_text_color] selected item text color. Defaults to yellow.
      - [description_color] description text color. Defaults to gray.
      - [selected_description_color] selected item description color. Defaults
        to light gray.
      - [show_description] whether to display description lines. Defaults to
        [true].
      - [show_scroll_indicator] whether to display scroll position indicator.
        Defaults to [false].
      - [wrap_selection] whether to wrap to the opposite end when navigating
        past list boundaries. Defaults to [false].
      - [item_spacing] vertical spacing between items in cells, clamped to
        minimum [0]. Defaults to [0].
      - [fast_scroll_step] items to skip with Shift+Up/Down, clamped to minimum
        [1]. Defaults to [5].
      - [key_bindings] custom shortcut/action pairs, default [[]]. Later
        bindings override equivalent defaults; other defaults remain available.
  *)

  val default : t
  (** [default] is [make ()]. *)

  val with_options_of : ('a -> item) -> 'a list -> t -> t
  (** [with_options_of f l props] is [props] with the options [List.map f l],
      where [f] must return equal items for one element every time. A select
      given the same [l] and [f], physically, as by its previous properties
      neither converts nor compares its options. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have identical visual properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t]. Only properties that differ
    from the current state trigger layout recomputation and re-render. Never
    fires the change callback; callbacks are reserved for user gestures. *)

(** {1:options Options} *)

val options : t -> item list
(** [options t] is the current item list. *)

val set_options : t -> item list -> unit
(** [set_options t items] replaces the item list. The selection index is clamped
    to the new valid range. Does not fire the {!val-set_on_change} callback.
    Triggers re-render. *)

(** {1:selection Selection} *)

val selected_index : t -> int
(** [selected_index t] is the 0-based selected index, or [-1] when no item is
    selected. *)

val selected_item : t -> item option
(** [selected_item t] is the currently selected item, or [None] when no item is
    selected. *)

val set_selected_index : t -> int -> unit
(** [set_selected_index t value] accepts [value] without firing [on_change].
    Negative values clear selection; positive overflow clamps to the last item.
    The result is [-1] for an empty list. From no selection, Down proposes the
    first item and Up the last. *)

(** {1:display Display} *)

val set_show_description : t -> bool -> unit
(** [set_show_description t flag] shows or hides item description lines.
    Recalculates layout and triggers re-render when the value changes. *)

val set_show_scroll_indicator : t -> bool -> unit
(** [set_show_scroll_indicator t flag] shows or hides the scroll position
    indicator. Triggers re-render when the value changes. *)

val set_item_spacing : t -> int -> unit
(** [set_item_spacing t n] sets vertical spacing between items in cells. [n] is
    clamped to minimum [0]. Recalculates layout and triggers re-render when the
    value changes. *)

(** {1:behavior Behavior} *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t flag] enables or disables wrapping to the opposite end
    when navigating past list boundaries. Triggers re-render when the value
    changes. *)

val set_fast_scroll_step : t -> int -> unit
(** [set_fast_scroll_step t n] sets the number of items to skip with
    Shift+Up/Down. [n] is clamped to minimum [1]. Triggers re-render when the
    value changes. *)

(** {1:colors Colors} *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] sets the unfocused background color. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t color] sets the unfocused text color. *)

val set_focused_background : t -> Ansi.Color.t -> unit
(** [set_focused_background t color] sets the focused background color. *)

val set_focused_text_color : t -> Ansi.Color.t -> unit
(** [set_focused_text_color t color] sets the focused text color. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t color] sets the selected item background color.
*)

val set_selected_text_color : t -> Ansi.Color.t -> unit
(** [set_selected_text_color t color] sets the selected item text color. *)

val set_description_color : t -> Ansi.Color.t -> unit
(** [set_description_color t color] sets the description text color. *)

val set_selected_description_color : t -> Ansi.Color.t -> unit
(** [set_selected_description_color t color] sets the selected item description
    text color. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t callback] registers a callback fired when the selected
    index changes. The callback receives the new index. Pass [None] to clear.
    See {!val-set_selected_index}. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** [set_on_activate t callback] registers a callback fired when the current
    item is activated (Enter or KP_enter). The callback receives the activated
    item index. Pass [None] to clear. *)

(** {1:layout Layout} *)

val set_style : t -> Toffee.Style.t -> unit
(** [set_style t style] sets the layout style on the underlying renderable. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
