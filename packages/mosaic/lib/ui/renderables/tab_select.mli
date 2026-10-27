(** Input/navigation proposes selection through [on_change]. A proposal does not
    change the accepted selection. [apply_props] and explicit setters accept
    values without emitting another proposal. *)

(** Horizontal tab navigation bar with scrolling and descriptions.

    A tab selector renders fixed-width tabs in a horizontal strip with keyboard
    navigation and optional scroll arrows. Each tab has a label and an optional
    description line displayed below the strip.

    Selection state and scroll offset are managed internally.

    The tab selector draws by the roles of the theme in scope
    ({!Renderable.theme}): labels in the [Plain] role, the selected tab on the
    [Selected] surface and also on the [Cursor] surface while focused, the
    scroll arrows, the underline and the marks as glyphs of the theme
    ({!Glyphs}), the arrows and the description in the [Muted] role. The
    selected tab's first cell holds the [cursor] glyph while the selector is
    focused and [cursor_inactive] otherwise, so the selection and the focus show
    whatever the output's styling. A color label replaces the role or surface it
    names.

    {2:keys Keys}

    When focused and holding at least one tab, the widget's behavior (see
    {!Renderable.set_key_behavior}) acts on the keys below and consumes them:
    they reach neither its ancestors nor any listener after the elements, such
    as an application's shortcuts. Every other key passes on.
    - Left, [\[] — move the selection left.
    - Right, [\]] — move the selection right.
    - Enter, Ctrl+J — activate the selected tab.

    It acts on them even when the selection cannot move. [key_bindings] adds
    keys to the list or binds one in it to another action.

    Callbacks ({!set_on_change}, {!set_on_activate}) are not part of {!Props}
    and must be set separately. *)

(** {1:items Items} *)

type item = { label : string; description : string }
(** A tab entry. [label] is displayed in the tab strip; [description] appears
    below the strip when {!set_show_description} is enabled. *)

val item : label:string -> ?description:string -> unit -> item
(** [item ~label ?description ()] is a tab item. [description] defaults to [""].
*)

(** {1:types Types} *)

type t
(** A tab selector backed by a {!Renderable.t}. *)

type action =
  | Move_left
  | Move_right
  | Activate  (** Keyboard navigation and activation actions. *)

type key_binding = Shortcut.t * action
(** A shortcut and its action. Custom bindings override defaults; the last
    binding for a shortcut wins. Releases are ignored. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  options:item list ->
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
  ?key_bindings:key_binding list ->
  ?on_change:(int -> unit) ->
  ?on_activate:(int -> unit) ->
  unit ->
  t
(** [create ~parent ~options ()] is a tab selector attached to [parent]. The
    node is focusable and uses buffered rendering. The labels from [options] to
    [key_bindings] are {!Props.make}'s and take its defaults.
    - [on_change] fires when the selected index changes via navigation or
      {!set_selected}.
    - [on_activate] fires when a tab is activated (Enter key or
      {!select_current}). *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. Contains visual state
      only; callbacks are excluded. *)

  val make :
    options:item list ->
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
    ?key_bindings:key_binding list ->
    unit ->
    t
  (** [make ~options ()] is the properties of a tab selector showing [options],
      with:
      - [selected] is the accepted index. Defaults to [0] for a nonempty list.
        Negative values mean no selection ([-1]); positive overflow clamps to
        the last tab. Empty lists have no selection. {!apply_props} accepts it
        without firing the change callback.
      - [tab_width] defaults to [12] (minimum [1]).
      - [background] defaults to none.
      - [text_color] defaults to none, the [Plain] role.
      - [focused_background] falls back to [background].
      - [focused_text_color] falls back to [text_color].
      - [selected_background] defaults to none, the [Selected] surface, with the
        [Cursor] surface over it while focused.
      - [selected_text_color] defaults to none.
      - [description_color] is the description's color while the selector is
        unfocused. Defaults to none, the [Muted] role.
      - [selected_description_color] is the description's color while the
        selector is focused. Falls back to [description_color].
      - [show_underline] defaults to [true].
      - [show_description] defaults to [false].
      - [show_scroll_arrows] defaults to [true].
      - [wrap_selection] defaults to [false].
      - [key_bindings] adds keys to those {!section-keys} lists or binds one of
        them to another action. Defaults to [[]].

      {!val-create} and {!Vnode.tab_select} take these labels with these
      defaults. *)

  val default : t
  (** [default] is [make ~options:[] ()]. *)

  val with_options_of : ('a -> item) -> 'a list -> t -> t
  (** [with_options_of f l props] is [props] with the options [List.map f l],
      where [f] must return equal items for one element every time. A tab select
      given the same [l] and [f], physically, as by its previous properties
      neither converts nor compares its options. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t]. Individual property changes
    trigger the minimum necessary layout and render updates. Never fires the
    change callback; callbacks are reserved for user gestures. *)

(** {1:options Options} *)

val options : t -> item list
(** [options t] is the current tab list. *)

val set_options : t -> item list -> unit
(** [set_options t items] replaces the tab list. Clamps the selected index to
    the new range, marks layout dirty, and re-renders. No-op if [items] is
    structurally equal to the current list. *)

(** {1:selection Selection} *)

val selected_index : t -> int
(** [selected_index t] is the zero-based selected index, or [-1] when no tab is
    selected. *)

val selected_item : t -> item option
(** [selected_item t] is the selected tab, or [None] when no tab is selected. *)

val set_selected : t -> int -> unit
(** [set_selected t value] accepts [value] without firing [on_change]. Negative
    values clear selection; positive overflow clamps to the last tab. The result
    is [-1] for an empty list. From no selection, Right proposes the first tab
    and Left the last. *)

(** {1:tab_width Tab width} *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t w] sets the fixed width of each tab cell. Values below [1]
    are ignored. *)

(** {1:colors Colors} *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] sets the unfocused background color. *)

val set_text_color : t -> Ansi.Color.t -> unit
(** [set_text_color t color] sets the unfocused text color. *)

val set_focused_background : t -> Ansi.Color.t -> unit
(** [set_focused_background t color] sets the focused background color. Only
    re-renders when the node is focused. *)

val set_focused_text_color : t -> Ansi.Color.t -> unit
(** [set_focused_text_color t color] sets the focused text color. Only
    re-renders when the node is focused. *)

val set_selected_background : t -> Ansi.Color.t -> unit
(** [set_selected_background t color] sets the selected-tab background. *)

val set_selected_text_color : t -> Ansi.Color.t -> unit
(** [set_selected_text_color t color] sets the selected-tab text color. *)

val set_description_color : t -> Ansi.Color.t -> unit
(** [set_description_color t color] sets the description line text color while
    [t] is unfocused. Only re-renders when descriptions are visible. *)

val set_selected_description_color : t -> Ansi.Color.t -> unit
(** [set_selected_description_color t color] sets the description line text
    color while [t] is focused. Only re-renders when descriptions are visible.
*)

(** {1:display Display flags} *)

val set_show_underline : t -> bool -> unit
(** [set_show_underline t v] shows or hides the selection underline. Changes the
    intrinsic height and marks layout dirty. *)

val set_show_description : t -> bool -> unit
(** [set_show_description t v] shows or hides the description line below tabs.
    Changes the intrinsic height and marks layout dirty. *)

val set_show_scroll_arrows : t -> bool -> unit
(** [set_show_scroll_arrows t v] shows or hides scroll indicators when tabs
    overflow. *)

val set_wrap_selection : t -> bool -> unit
(** [set_wrap_selection t v] enables or disables wraparound at edges. When
    enabled, navigating past the last tab wraps to the first and vice versa. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (int -> unit) option -> unit
(** [set_on_change t f] registers a handler called with the new index when the
    selected index changes. [None] clears the handler. *)

val set_on_activate : t -> (int -> unit) option -> unit
(** [set_on_activate t f] registers a handler called with the index when a tab
    is activated (Enter key or {!select_current}). [None] clears the handler. *)

(** {1:navigation Navigation} *)

val move_left : t -> unit
(** [move_left t] moves the selection one tab to the left. Wraps to the last tab
    if [wrap_selection] is enabled and the selection is at the first tab. No-op
    if options are empty. Fires [on_change] if the index changed. *)

val move_right : t -> unit
(** [move_right t] moves the selection one tab to the right. Wraps to the first
    tab if [wrap_selection] is enabled and the selection is at the last tab.
    No-op if options are empty. Fires [on_change] if the index changed. *)

val select_current : t -> unit
(** [select_current t] activates the currently selected tab, firing
    [on_activate]. No-op if options are empty. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a tab selector for debugging. *)
