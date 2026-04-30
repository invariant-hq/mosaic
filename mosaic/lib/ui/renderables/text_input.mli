(** Single-line text input widget.

    A text input provides keyboard-driven single-line editing with horizontal
    scrolling, selection highlighting, cursor display, and placeholder text.
    Supports undo/redo, word-level navigation, and configurable visual styling.
    Newlines are always stripped from content.

    The widget fires four callbacks:
    - [on_input]: after every text change (keystroke-level).
    - [on_change]: when the committed value differs on blur or submit. See
      {!section:callbacks}.
    - [on_submit]: when Enter is pressed.
    - [on_cursor]: when cursor position or selection changes.

    See {!Textarea} for multi-line editing. *)

type t
(** The type for single-line text inputs. *)

type action = Textarea.action =
  | Move_left
  | Move_right
  | Move_up
  | Move_down
  | Select_left
  | Select_right
  | Select_up
  | Select_down
  | Line_home
  | Line_end
  | Select_line_home
  | Select_line_end
  | Visual_line_home
  | Visual_line_end
  | Select_visual_line_home
  | Select_visual_line_end
  | Buffer_home
  | Buffer_end
  | Select_buffer_home
  | Select_buffer_end
  | Delete_line
  | Delete_to_line_end
  | Delete_to_line_start
  | Backspace
  | Delete
  | Newline
  | Undo
  | Redo
  | Word_forward
  | Word_backward
  | Select_word_forward
  | Select_word_backward
  | Delete_word_forward
  | Delete_word_backward
  | Select_all
  | Submit
(** Editing actions resolved from key bindings. *)

type key_binding = Textarea.key_binding
(** The type for text input key bindings. *)

val key_binding :
  ?ctrl:bool ->
  ?shift:bool ->
  ?alt:bool ->
  ?super:bool ->
  string ->
  action ->
  key_binding
(** [key_binding name action] binds [name] with optional modifiers to [action].
*)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
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
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:key_binding list ->
  ?key_aliases:(string * string) list ->
  ?on_input:(string -> unit) ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a text input attached to [parent] with:
    - [value]: initial text content. Newlines are stripped. Defaults to [""].
    - [cursor]: optional controlled cursor grapheme offset.
    - [selection]: optional controlled selection range. When provided as
      [Some (lo, hi)], selection is normalized/clamped.
    - [placeholder]: text shown when empty. Defaults to [""].
    - [max_length]: maximum grapheme cluster count. Defaults to [1000].
    - [text_color]: unfocused text color. Defaults to {!Ansi.Color.white}.
    - [background_color]: unfocused background. Defaults to
      {!Ansi.Color.default}.
    - [focused_text_color]: focused text color. Defaults to {!Ansi.Color.white}.
    - [focused_background_color]: focused background. Defaults to
      {!Ansi.Color.default}.
    - [placeholder_color]: placeholder text color. Defaults to
      {!Ansi.Color.bright_black}.
    - [selection_color]: selection background. Defaults to {!Ansi.Color.blue}.
    - [selection_fg]: selection foreground. When [None], uses the normal text
      color. Defaults to [None].
    - [cursor_style]: cursor shape when focused. Defaults to [`Block].
    - [cursor_color]: cursor color when focused. Defaults to
      {!Ansi.Color.white}.
    - [cursor_blinking]: whether the cursor blinks. Defaults to [true].
    - [selectable]: whether mouse selection is enabled. Defaults to [true].
    - [show_cursor]: whether the focused cursor is shown. Defaults to [true].
    - [key_bindings]: custom bindings that override default bindings.
    - [key_aliases]: custom key aliases merged with default keypad aliases.
    - [on_input]: called after every text change.
    - [on_change]: called when committed value changes (blur or submit).
    - [on_submit]: called when Enter is pressed.
    - [on_cursor]: called when cursor position or selection changes. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val buffer : t -> Edit_buffer.t
(** [buffer t] is the underlying edit buffer. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles, used by the reconciler for
      diffing. *)

  val make :
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
    ?selectable:bool ->
    ?show_cursor:bool ->
    ?key_bindings:key_binding list ->
    ?key_aliases:(string * string) list ->
    unit ->
    t
  (** [make ()] is a property set with the same defaults as {!val-create}. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] updates [t] to match [props]. When [value] differs,
    the buffer content is replaced (newlines stripped). Requests a render. *)

(** {1:value Value} *)

val value : t -> string
(** [value t] is the current text content. *)

val cursor : t -> int
(** [cursor t] is the current cursor grapheme offset. *)

val selection : t -> (int * int) option
(** [selection t] is the active selection as normalized grapheme offsets, if
    any. *)

val set_value : t -> string -> unit
(** [set_value t s] replaces the text content with [s] (newlines stripped).
    Ensures the cursor remains visible. *)

(** {1:callbacks Callbacks} *)

val set_on_input : t -> (string -> unit) option -> unit
(** [set_on_input t f] sets the keystroke-level input callback. [None] clears
    it. *)

val set_on_change : t -> (string -> unit) option -> unit
(** [set_on_change t f] sets the committed-value change callback. [None] clears
    it.

    The callback fires on blur or submit, only when the value has changed since
    focus was gained (or since the last [on_change] firing). *)

val set_on_submit : t -> (string -> unit) option -> unit
(** [set_on_submit t f] sets the Enter-key submit callback. [None] clears it.
    Fires [on_change] before the submit callback. *)

val set_on_cursor :
  t -> (cursor:int -> selection:(int * int) option -> unit) option -> unit
(** [set_on_cursor t f] sets the cursor/selection change callback. [None] clears
    it. *)

(** {1:paste Paste} *)

val handle_paste : t -> string -> unit
(** [handle_paste t text] inserts [text] as if pasted, stripping ANSI escape
    sequences and newlines. Fires [on_input] if the buffer changed. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
