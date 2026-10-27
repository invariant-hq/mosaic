(** The tree of nodes a view is, behind [Mosaic.t].

    [Mosaic] builds these nodes, and the reconciler ([Reconciler]) mounts and
    updates widgets from them. The rules a view follows are [Mosaic]'s (its
    section "UI elements"); this module states what they leave unsaid about the
    representation.

    The {!type-attrs} and {!type-kind} of an element are shared by reference
    across {!val-map}, and the reconciler compares them physically before it
    compares them by value. *)

open Mosaic_ui

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
    Widget-specific semantic callbacks, such as a slider's value changes, live
    in {!type-widget_callbacks}. *)

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
          monomorphic. They are shared by reference across {!val-map} and used
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
  | Text_callbacks of {
      on_selection : ((int * int) option -> 'msg option) option;
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
      (** A box with these callbacks is a modal ([Mosaic.modal]). *)

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
val fragment : 'msg t list -> 'msg t
val embed : Renderable.t -> 'msg t
val themed : (Theme.t -> Theme.t) -> 'msg t -> 'msg t

val memo : key:'key -> ('key -> 'msg t) -> 'msg t
(** [memo ~key view] is [Mosaic.memo].

    Raises [Invalid_argument] if [key] contains a function, an object or an
    abstract value. *)

val memo_equal : 'a memo -> 'a memo -> bool
(** [memo_equal a b] is [true] iff [a] and [b] are equal in the sense of
    [Mosaic.memo]: then they build equal subtrees. *)

val memo_view : 'msg memo -> 'msg t
(** [memo_view m] calls [m]'s view function on its key, through its message
    mappers. *)

val viewport_switch :
  at_least_width:int -> wide:'msg t -> narrow:'msg t -> 'msg t
(** [viewport_switch ~at_least_width ~wide ~narrow] is [Mosaic.viewport_switch].

    Raises [Invalid_argument] if [at_least_width] is not positive. *)

val element :
  ?key:string ->
  style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  focusable:bool ->
  ?focused:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  callbacks:'msg widget_callbacks ->
  kind ->
  'msg t list ->
  'msg t
(** [element ~style ~focusable ~callbacks kind children] is an element of [kind]
    laid out by [style]. [focusable] is the kind's default; the element is
    focusable when it is or when it has [on_focus_request]. The other attributes
    take [Mosaic]'s defaults. *)

(** {1:transform Transformation} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f vnode] is [Mosaic.map]. It allocates new handlers and callbacks and
    shares the {!type-attrs} and {!type-kind} of each element. *)
