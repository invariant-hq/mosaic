(** Horizontal tab navigation bar with scrolling and descriptions.

    A tab selector renders fixed-width tabs in a horizontal strip with keyboard
    navigation and optional scroll arrows. Each tab has a label and an optional
    description line displayed below the strip.

    Selection state and scroll offset are managed internally. The built-in key
    handler responds to:
    - Left arrow and [\[] — move selection left.
    - Right arrow and [\]] — move selection right.
    - Enter — activate the selected tab.

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
  ?on_change:(int -> unit) ->
  ?on_activate:(int -> unit) ->
  unit ->
  t
(** [create ~parent ~options ()] is a tab selector attached to [parent]. The
    node is focusable and uses buffered rendering.

    - [selected] defaults to [0], clamped to \[[0];[List.length options - 1]\].
    - [tab_width] defaults to [12] (minimum [1]).
    - [background] defaults to transparent (RGBA 0 0 0 0).
    - [text_color] defaults to light gray (RGB 226 232 240).
    - [focused_background] falls back to [background] if provided, otherwise
      dark gray (RGB 26 26 26).
    - [focused_text_color] falls back to [text_color].
    - [selected_background] defaults to blue (RGB 59 130 246).
    - [selected_text_color] defaults to white.
    - [description_color] defaults to slate (RGB 203 213 225).
    - [selected_description_color] defaults to light gray (RGB 204 204 204).
    - [show_underline] defaults to [true].
    - [show_description] defaults to [false].
    - [show_scroll_arrows] defaults to [true].
    - [wrap_selection] defaults to [false].
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
    unit ->
    t
  (** [make ~options ()] is a property set with the same defaults as
      {!val-create}. *)

  val default : t
  (** [default] is [make ~options:[] ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t]. Individual property changes
    trigger the minimum necessary layout and render updates. Fires
    {!set_on_change} if the selected index changes. *)

(** {1:options Options} *)

val options : t -> item list
(** [options t] is the current tab list. *)

val set_options : t -> item list -> unit
(** [set_options t items] replaces the tab list. Clamps the selected index to
    the new range, marks layout dirty, and re-renders. No-op if [items] is
    structurally equal to the current list. *)

(** {1:selection Selection} *)

val selected_index : t -> int
(** [selected_index t] is the zero-based index of the selected tab. [0] when the
    tab list is empty. *)

val selected_item : t -> item option
(** [selected_item t] is the selected tab, or [None] if the tab list is empty.
*)

val set_selected : t -> int -> unit
(** [set_selected t i] selects the tab at index [i], clamped to
    \[[0];[length - 1]\]. Fires [on_change] if the index changed. *)

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
(** [set_description_color t color] sets the description line text color. Only
    re-renders when descriptions are visible. *)

val set_selected_description_color : t -> Ansi.Color.t -> unit
(** [set_selected_description_color t color] sets the description text color for
    the selected tab. Only re-renders when descriptions are visible. *)

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
