(** Single-line input from application-owned {!Editor.t} values.

    Default events propose complete states through [on_change]. The displayed
    text, caret, selection and undo history change only when the owner applies
    accepted state. Declared text must not contain CR or LF; invalid
    declarations raise [Invalid_argument]. Paste strips ANSI escapes and
    newlines. Undo and redo that would restore multiline text are ignored.

    The editor is drawn on the [Field] surface of the theme in scope
    ({!Renderable.theme}), its text as {!Text_surface.section-paint} states, and
    its placeholder in the [Muted] role; a color label replaces the role or
    surface it names. Focus shows as the caret, which is drawn at the focus
    point whatever the output's styling.

    {1:keys Keys}

    When focused, the input acts on the keys {!Textarea} acts on, and consumes
    them, with two differences: Enter and Ctrl+J submit instead of inserting a
    line break, and Up, Down, Shift+Up and Shift+Down, which have no line to
    move to, pass on. *)

type t
(** The type for single-line text inputs. *)

(** Editing actions resolved from key bindings. *)
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

type key_binding = Shortcut.t * action
(** The type for text input key bindings. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  state:Editor.t ->
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
  ?key_bindings:key_binding list ->
  on_change:(Editor.t -> unit) ->
  ?on_submit:(string -> unit) ->
  unit ->
  t
(** [create ~parent ~state ~on_change ()] renders the accepted [state]. Default
    editing events propose a complete next state through [on_change]; they do
    not mutate the accepted editor. Apply new properties to accept a proposal.
    [on_submit] reports accepted text. Focus and blur do not commit edits. The
    labels from [state] to [key_bindings] are {!Props.make}'s and take its
    defaults.

    Raises [Invalid_argument] as {!Props.make} does, before attaching a
    renderable to [parent]. *)

(** {1:accessors Accessors} *)

val state : t -> Editor.t
(** [state t] is the accepted editor. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for declarative property bundles, used by the reconciler for
      diffing. *)

  val make :
    state:Editor.t ->
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
    ?key_bindings:key_binding list ->
    unit ->
    t
  (** [make ~state ()] describes [state] with:
      - [placeholder] is the text shown when the input is empty, whether or not
        it is focused. Defaults to [""].
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
      - [show_cursor] is [true] iff the focused input shows its caret. Defaults
        to [true].
      - [key_bindings] adds keys to those {!section-keys} lists or binds one of
        them to another action. Defaults to [[]].

      {!val-create} and [Mosaic.input] take these labels with these defaults.

      Raises [Invalid_argument] if [Editor.text state] contains CR or LF.
      Validation uses the editor's cached logical-line index; caret-only changes
      do not rescan the text. *)

  val default : t
  (** [default] is [make ~state:Editor.empty ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] refer to the same editor value and
      have equal visual properties. *)
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
    escapes and newlines. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
