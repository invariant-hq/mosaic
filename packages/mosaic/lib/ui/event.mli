(** Keyboard, paste, and mouse UI events.

    Each event wraps terminal input from {!Matrix_input} with two flags that
    control its delivery. An event goes along a path, its target and the
    target's ancestors (see {!Renderer} for how each kind finds its target), and
    at each node the node's handlers see it. Then come the actions the event
    causes: a widget's behavior, such as the character a key types into an
    editor, and the renderer's defaults, such as the text selection a press
    starts. The flags mean what they mean in the DOM (DOM Standard, §2.9
    "Interface Event"):

    - {e Stopping propagation} keeps the event from every node after the current
      one. The current node's other handlers still see it: stopping never hides
      an event from the node that stopped it.
    - {e Preventing the default} cancels the actions the event causes. The
      handlers after the current one still see the event, and read the flag to
      know it was used.

    The flags are independent: a stopped event still types its character unless
    it is also prevented. A behavior that acts on an event both prevents and
    stops it: a key typed into an input reaches neither the input's ancestors
    nor the application's shortcuts. *)

(** {1:keyboard Keyboard events} *)

module Key : sig
  type t
  (** The type for keyboard events.

      Wraps a key press or repeat ({!Matrix_input.Keystroke.t}) with the two
      dispatch flags. Releases are not key events: widgets never see them. *)

  val of_input : Matrix_input.Keystroke.t -> t
  (** [of_input ev] is a keyboard event wrapping [ev], neither stopped nor
      prevented. *)

  val data : t -> Matrix_input.Keystroke.t
  (** [data t] is the underlying keystroke. *)

  val prevent_default : t -> unit
  (** [prevent_default t] cancels the actions [t] causes, such as the character
      it types. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val stop_propagation : t -> unit
  (** [stop_propagation t] keeps [t] from the nodes after the current one. *)

  val propagation_stopped : t -> bool
  (** [propagation_stopped t] is [true] iff {!stop_propagation} has been called
      on [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] wrap the same key event. The flags
      are ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a keyboard event for debugging. *)
end

(** {1:paste Paste events} *)

module Paste : sig
  type t
  (** The type for text paste events: pasted text with the two dispatch flags.
  *)

  val of_text : string -> t
  (** [of_text s] is a paste event containing [s], neither stopped nor
      prevented. *)

  val text : t -> string
  (** [text t] is the pasted text carried by [t]. *)

  val prevent_default : t -> unit
  (** [prevent_default t] cancels the actions [t] causes, such as inserting its
      text. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val stop_propagation : t -> unit
  (** [stop_propagation t] keeps [t] from the nodes after the current one. *)

  val propagation_stopped : t -> bool
  (** [propagation_stopped t] is [true] iff {!stop_propagation} has been called
      on [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] contain the same text. The flags are
      ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a paste event for debugging. *)
end

(** {1:mouse Mouse events} *)

module Mouse : sig
  type t
  (** The type for mouse events.

      Carries cursor coordinates, modifier keys, propagation control, and
      hit-testing metadata. Variant-specific data (button, source, scroll
      direction) is accessed by pattern-matching on {!type-kind}. The cursor is
      given on the screen ({!x}, {!y}) and in the cells of the node whose
      handler sees the event ({!local}, {!size}).

      Only {!stop_propagation} and {!prevent_default} mutate the event; all
      other fields are immutable after construction. *)

  (** {2:types Supporting types} *)

  (** The type for mouse buttons. *)
  type button =
    | Left  (** Primary (left) button. *)
    | Middle  (** Middle button. *)
    | Right  (** Secondary (right) button. *)
    | Button of int  (** Extended button with one-based index [n]. *)

  val equal_button : button -> button -> bool
  (** [equal_button a b] is [true] iff [a] and [b] are the same button. *)

  val pp_button : Format.formatter -> button -> unit
  (** [pp_button] formats a {!button} value. *)

  type modifier = Matrix_input.Keystroke.mods = {
    shift : bool;  (** Shift key held. *)
    alt : bool;  (** Alt / Option key held. *)
    ctrl : bool;  (** Control key held. *)
    super : bool;  (** Super / Windows / Command key held. *)
    hyper : bool;  (** Hyper key held. *)
    meta : bool;  (** Meta key held. *)
    caps_lock : bool;  (** Caps Lock active. *)
    num_lock : bool;  (** Num Lock active. *)
  }
  (** The type for modifier key state, re-exported from
      {!Matrix_input.Keystroke.type-mods}. A mouse report carries Shift, Alt and
      Ctrl only. *)

  val no_modifier : modifier
  (** [no_modifier] is the modifier state with every field set to [false]. *)

  val equal_modifier : modifier -> modifier -> bool
  (** [equal_modifier a b] is [true] iff all modifier fields of [a] and [b] are
      equal. *)

  val pp_modifier : Format.formatter -> modifier -> unit
  (** [pp_modifier] formats a {!modifier} value. *)

  (** The type for scroll-wheel directions. *)
  type scroll_direction =
    | Scroll_up  (** Scroll towards the top of the content. *)
    | Scroll_down  (** Scroll towards the bottom of the content. *)
    | Scroll_left  (** Scroll towards the left of the content. *)
    | Scroll_right  (** Scroll towards the right of the content. *)

  val equal_scroll_direction : scroll_direction -> scroll_direction -> bool
  (** [equal_scroll_direction a b] is [true] iff [a] and [b] are the same
      direction. *)

  val pp_scroll_direction : Format.formatter -> scroll_direction -> unit
  (** [pp_scroll_direction] formats a {!scroll_direction} value. *)

  (** {2:kinds Event kinds}

      Each variant carries exactly the data relevant to that kind of event.
      Common fields (coordinates, modifiers, target) are accessed via {!x},
      {!y}, {!modifiers}, and {!target}. *)

  (** The type for mouse event kinds. *)
  type kind =
    | Down of { button : button }  (** Button pressed. *)
    | Up of { button : button; is_dragging : bool }
        (** Button released. [is_dragging] is [true] iff a drag was in progress
            when the button was released. *)
    | Move  (** Cursor moved with no button pressed. *)
    | Drag of { button : button; is_dragging : bool }
        (** Cursor moved with [button] held. [is_dragging] is [true] iff the
            drag threshold has been exceeded. *)
    | Drag_end of { button : button }
        (** Drag gesture ended; [button] was the dragging button. *)
    | Drop of { button : button; source : int option }
        (** Drop target reached. [source] is the node identifier of the drag
            source, if known. *)
    | Over of { source : int option }
        (** Cursor moved over a potential drop target. [source] is the node
            identifier of the drag source, if known. *)
    | Out  (** Cursor left a node during a drag. *)
    | Scroll of { direction : scroll_direction; delta : int }
        (** Scroll-wheel event. [delta] is the number of steps in [direction].
        *)

  val equal_kind : kind -> kind -> bool
  (** [equal_kind a b] is [true] iff [a] and [b] are the same constructor and
      carry equal payloads. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind] formats a {!type-kind} value. *)

  (** {2:constructors Construction} *)

  val make : x:int -> y:int -> modifiers:modifier -> ?target:int -> kind -> t
  (** [make ~x ~y ~modifiers ?target kind] is a mouse event at [(x, y)] with
      modifier state [modifiers], optional hit-test node [target], and event
      kind [kind]. Propagation and default-prevention flags start as [false].

      [target] defaults to [None]. *)

  val at_node : t -> x:int -> y:int -> width:int -> height:int -> t
  (** [at_node t ~x ~y ~width ~height] is [t] as the node whose top-left cell is
      [(x, y)] and whose size is [width] by [height] sees it: its {!local} is
      counted from [(x, y)] and its {!size} is [(width, height)]. It shares
      [t]'s propagation and default flags, so stopping or preventing one stops
      or prevents the other. [Renderer.dispatch_mouse] gives each node it
      reaches the event so made; a handler's test makes one to give the handler.
  *)

  (** {2:accessors Accessors} *)

  val kind : t -> kind
  (** [kind t] is [t]'s event kind with its variant-specific payload. See
      {!type-kind}. *)

  val x : t -> int
  (** [x t] is [t]'s horizontal cursor position (0-based column). *)

  val y : t -> int
  (** [y t] is [t]'s vertical cursor position (0-based row). *)

  val local : t -> int * int
  (** [local t] is the cell under the cursor counted from the top-left cell of
      the node whose handler sees [t]. It is [(0, 0)] on that node's own
      top-left cell, so a canvas reads it in the coordinates of its grid. Each
      node on the event's path sees its own value. Before any node sees [t], as
      for an event made by {!make}, it is [(x t, y t)]. *)

  val size : t -> int * int
  (** [size t] is the width and height in cells of the node whose handler sees
      [t], and [(0, 0)] before any node sees it. A handler lays out what its
      node draws at this size to find what lies under {!local}, so the program
      keeps no size of its own. *)

  val modifiers : t -> modifier
  (** [modifiers t] is [t]'s modifier key state at the time of the event. *)

  val target : t -> int option
  (** [target t] is the hit-test target node identifier of [t], if any. *)

  (** {2:dispatch Dispatch control} *)

  val stop_propagation : t -> unit
  (** [stop_propagation t] keeps [t] from the nodes after the current one. *)

  val propagation_stopped : t -> bool
  (** [propagation_stopped t] is [true] iff {!stop_propagation} has been called
      on [t]. *)

  val prevent_default : t -> unit
  (** [prevent_default t] cancels the renderer's defaults for [t], such as the
      text selection a press starts or the click a release completes. *)

  val default_prevented : t -> bool
  (** [default_prevented t] is [true] iff {!prevent_default} has been called on
      [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same kind, coordinates,
      modifiers, and target. The flags, {!local} and {!size} are ignored. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a mouse event for debugging. *)
end

(** {1:aliases Type aliases} *)

type key = Key.t
(** The type for keyboard events. Alias for {!Key.t}. *)

type paste = Paste.t
(** The type for paste events. Alias for {!Paste.t}. *)

type mouse = Mouse.t
(** The type for mouse events. Alias for {!Mouse.t}. *)
