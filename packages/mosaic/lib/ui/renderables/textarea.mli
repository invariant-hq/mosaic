(** Multiline editing of application-owned {!Editor.t} values.

    Default events propose complete states through [on_change]. The displayed
    text, caret, selection and undo history change only when the owner applies
    accepted state. Paste strips ANSI escapes.

    The editor is drawn on the [Field] surface of the theme in scope
    ({!Renderable.theme}), its text as {!Text_surface.section-paint} states, and
    its placeholder in the [Muted] role; a color label replaces the role or
    surface it names. Focus shows as the caret, which is drawn at the focus
    point whatever the output's styling.

    {1:keys Keys}

    When focused, the textarea's behavior (see {!Renderable.set_key_behavior})
    acts on every key that types text, inserting it, and on the keys below, and
    consumes them: they reach neither its ancestors nor any listener after the
    elements, such as an application's shortcuts. A chord (Ctrl, Alt, Super or
    Hyper held) types nothing (see {!Matrix_input.Keystroke}), and neither do
    Tab, Escape and the function keys: those not listed below pass on.
    - Left, Right, Ctrl+B, Ctrl+F — move by a character; Shift+Left and
      Shift+Right extend the selection.
    - Up, Down — move by a display row; with Shift, extend.
    - Ctrl+A, Ctrl+E — move to the start or end of the line; with Shift, extend.
    - Alt+A, Alt+E, Super+Left, Super+Right — move to the start or end of the
      display row; with Shift, extend.
    - Home, End, Super+Up, Super+Down — move to the start or end of the text;
      with Shift, extend.
    - Alt+B, Alt+F, Alt+Left, Alt+Right, Ctrl+Left, Ctrl+Right — move by a word;
      Alt+Shift+B, Alt+Shift+F, Alt+Shift+Left and Alt+Shift+Right extend.
    - Backspace, Shift+Backspace — delete backward; Delete, Shift+Delete, Ctrl+D
      — delete forward.
    - Ctrl+W, Ctrl+Backspace, Alt+Backspace — delete a word backward; Alt+D,
      Alt+Delete, Ctrl+Delete — delete a word forward.
    - Ctrl+Shift+D — delete the line; Ctrl+K — delete to its end; Ctrl+U —
      delete to its start.
    - Enter, Ctrl+J — insert a line break.
    - Alt+Enter, Ctrl+Enter, Super+Enter, Ctrl+Alt+J, Ctrl+Super+J — submit.
    - Ctrl+Z, Ctrl+-, Super+Z — undo; Ctrl+Shift+Z, Ctrl+., Super+Shift+Z —
      redo.
    - Super+A — select everything.

    It acts on them even when they change nothing, at the start of the text for
    instance. [key_bindings] adds keys to the list or binds one in it to another
    action. *)

type t
(** A multi-line text editing widget. *)

(** Editing actions resolved from key bindings. *)
type action = Edit_surface.action =
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

type key_binding = Shortcut.t * action
(** The type for textarea key bindings. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  on_change:(Editor.t -> unit) ->
  ?on_submit:(string -> unit) ->
  state:Editor.t ->
  ?annotations:Text_buffer.Annotations.t ->
  ?syntax:Syntax_session.syntax ->
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
  ?key_bindings:key_binding list ->
  unit ->
  t
(** [create ~parent ~state ~on_change ()] renders the accepted [state]. Default
    editing events propose a complete next state through [on_change]; they do
    not mutate the accepted editor. Apply new properties to accept a proposal.
    [on_submit] reports accepted text. Focus and blur do not commit edits. The
    labels from [state] to [key_bindings] are {!Props.make}'s and take its
    defaults.

    Raises [Invalid_argument] as {!Props.make} does. *)

(** {1:accessors Accessors} *)

val state : t -> Editor.t
(** [state t] is the accepted editor. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val surface : t -> Text_surface.t
(** [surface t] is the underlying text surface. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. *)

  val make :
    state:Editor.t ->
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
    ?key_bindings:key_binding list ->
    unit ->
    t
  (** [make ~state ()] describes [state] with:
      - [annotations] and [syntax] style the text, as described below. Default
        to none.
      - [ghost_text] is text drawn in italics at the caret of the focused
        textarea when nothing is selected, such as a suggested completion. It is
        not part of the text. Defaults to none.
      - [ghost_text_color] is the ghost text's color. Defaults to none, the
        [Muted] role over the [Field] surface.
      - [placeholder] is the text shown when the textarea is empty. Defaults to
        [""].
      - [wrap] is the wrapping mode. Defaults to [`Word].
      - [text_color] is the text color. Defaults to none, the [Field] surface's
        foreground under the [Plain] role.
      - [background_color] is the background color. Defaults to none, the
        [Field] surface's background.
      - [focused_text_color] is the text color when focused. Defaults to
        [text_color].
      - [focused_background_color] is the background color when focused.
        Defaults to [background_color].
      - [placeholder_color] is the placeholder's color. Defaults to none, the
        [Muted] role over the [Field] surface.
      - [selection_color] is the selection's background color. Defaults to none.
        Without it and [selection_fg], the selection is drawn over the
        [Highlight] surface ({!Text_surface.section-paint}).
      - [selection_fg] is the selection's foreground color. Defaults to the text
        color.
      - [cursor_style] is the caret's shape when focused. Defaults to [`Block].
      - [cursor_color] is the caret's color when focused. Defaults to
        {!Ansi.Color.default_fg}, which leaves the caret in the terminal's own
        color.
      - [cursor_blinking] is [true] iff the caret blinks. Defaults to [true].
      - [selectable] is [true] iff the pointer selects text. Defaults to [true].
      - [show_cursor] is [true] iff the focused textarea shows its caret.
        Defaults to [true].
      - [key_bindings] adds keys to those {!section-keys} lists or binds one of
        them to another action. Defaults to [[]].

      {!val-create} and [Mosaic.textarea] take these labels with these defaults.

      Two inputs style the text, and [annotations] go over [syntax] (see
      {!Text_buffer.Annotations.merge}).
      - [syntax] highlights the text as {!Code} does. A highlighter receives
        each accepted text with the edit from the latest result, and while a
        result is pending the text shows as {!Code.with_highlighter} states.
        Editable text shows every byte, so captures never conceal.
      - [annotations] are styles the application resolved itself, such as search
        matches or diagnostics. They must describe the same bytes as
        [Editor.text state]. Omit them while awaiting a result for a changed
        source. They depend only on the source bytes, so the same annotations
        stay valid while the caret and selection move.

      Raises [Invalid_argument] before applying any properties if [annotations]
      describe other bytes than [Editor.text state]. *)

  val default : t
  (** [default] is [make ~state:Editor.empty ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] refer to the same editor value and
      the same annotations, and have equal syntax settings (see
      {!Code.type-syntax}) and visual properties. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props t props] installs the complete accepted editor and visual
    properties. It does not emit proposals. Source indexes and geometry are
    reused when only the caret or selection changes. *)

(** {1:value Value} *)

val value : t -> string
(** [value t] is the current text content. *)

val cursor : t -> int
(** [cursor t] is the current cursor grapheme offset. *)

val selection : t -> (int * int) option
(** [selection t] is the active selection as normalized grapheme offsets, if
    any. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (Editor.t -> unit) -> unit
(** [set_on_change t f] sets the complete-state proposal callback. *)

val set_on_submit : t -> (string -> unit) option -> unit
(** [set_on_submit t f] sets the submit notification. [None] clears it. *)

(** {1:paste Paste} *)

val handle_paste : t -> string -> unit
(** [handle_paste t text] proposes insertion of pasted text after stripping ANSI
    escapes. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)

(**/**)

module Private : sig
  val of_props :
    parent:Renderable.t ->
    ?index:int ->
    ?id:string ->
    ?style:Toffee.Style.t ->
    ?visible:bool ->
    ?z_index:int ->
    ?opacity:float ->
    on_change:(Editor.t -> unit) ->
    ?on_submit:(string -> unit) ->
    Props.t ->
    t
  (** [of_props ~parent props] is a widget showing [props], made as [create]
      makes one. *)
end
