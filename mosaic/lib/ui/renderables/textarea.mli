(** Multi-line text editing widget with wrapping, selection, and scrolling.

    A textarea wraps a {!Renderable.t} with an {!Edit_buffer.t},
    {!Text_buffer.t}, and {!Text_surface.t}, providing keyboard-driven
    multi-line text editing with word/character wrapping, vertical scrolling,
    selection highlighting, cursor display, and placeholder text. Supports
    undo/redo, word-level and line-level navigation, and configurable visual
    styling.

    The widget fires three callbacks:
    - [on_input]: after every text change (keystroke-level).
    - [on_change]: when the committed value differs on blur or submit.
    - [on_submit]: when Cmd+Enter or Ctrl+Enter is pressed.

    See {!Text_input} for single-line editing and {!Text} for read-only display.
*)

type t
(** A multi-line text editing widget. *)

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
  ?highlights:Text_buffer.span list ->
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
  ?on_input:(string -> unit) ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a textarea attached to [parent] with:
    - [value]: initial text content. Defaults to [""].
    - [cursor]: optional initial cursor grapheme offset. Defaults to end.
    - [highlights]: optional styled spans used for syntax highlighting. The span
      text must match [value]. Defaults to [[]].
    - [ghost_text]: optional inline ghost completion rendered at the cursor.
      Defaults to [None].
    - [ghost_text_color]: ghost text foreground color. Defaults to a dim gray.
    - [placeholder]: text shown when empty. Defaults to [""].
    - [wrap]: line wrapping mode. Defaults to [`Word].
    - [text_color]: unfocused text color. Defaults to {!Ansi.Color.White}.
    - [background_color]: unfocused background. Defaults to
      {!Ansi.Color.default}.
    - [focused_text_color]: focused text color. Defaults to {!Ansi.Color.White}.
    - [focused_background_color]: focused background. Defaults to
      {!Ansi.Color.default}.
    - [placeholder_color]: placeholder text color. Defaults to
      {!Ansi.Color.Bright_black}.
    - [selection_color]: selection background. Defaults to {!Ansi.Color.Blue}.
    - [selection_fg]: selection foreground. When [None], uses the normal text
      color. Defaults to [None].
    - [cursor_style]: cursor shape when focused. Defaults to [`Block].
    - [cursor_color]: cursor color when focused. Defaults to
      {!Ansi.Color.White}.
    - [cursor_blinking]: whether the cursor blinks. Defaults to [true].
    - [on_input]: called after every text change.
    - [on_change]: called when committed value changes (blur or submit).
    - [on_submit]: called when Cmd+Enter or Ctrl+Enter is pressed.
    - [on_cursor]: called when cursor position or selection changes. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val buffer : t -> Edit_buffer.t
(** [buffer t] is the underlying edit buffer. *)

val surface : t -> Text_surface.t
(** [surface t] is the underlying text surface. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. *)

  val make :
    ?value:string ->
    ?cursor:int ->
    ?highlights:Text_buffer.span list ->
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
(** [apply_props t props] applies [props] to [t], triggering the minimum
    necessary layout and render updates. *)

(** {1:value Value} *)

val value : t -> string
(** [value t] is the current text content. *)

val cursor : t -> int
(** [cursor t] is the current cursor grapheme offset. *)

val selection : t -> (int * int) option
(** [selection t] is the active selection as normalized grapheme offsets, if
    any. *)

val set_value : t -> string -> unit
(** [set_value t s] replaces the text content with [s]. Resets scroll and cursor
    visibility. *)

(** {1:callbacks Callbacks} *)

val set_on_input : t -> (string -> unit) option -> unit
(** [set_on_input t handler] sets the keystroke-level input callback. *)

val set_on_change : t -> (string -> unit) option -> unit
(** [set_on_change t handler] sets the committed-value change callback. *)

val set_on_submit : t -> (string -> unit) option -> unit
(** [set_on_submit t handler] sets the Cmd/Ctrl+Enter submit callback. *)

val set_on_cursor :
  t -> (cursor:int -> selection:(int * int) option -> unit) option -> unit
(** [set_on_cursor t handler] sets the cursor/selection change callback. It
    fires when cursor position or selection changes. *)

(** {1:paste Paste} *)

val handle_paste : t -> string -> unit
(** [handle_paste t text] inserts [text] as if pasted, preserving newlines.
    Fires [on_input] if the buffer changed. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
