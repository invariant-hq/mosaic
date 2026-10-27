(** Editable text rendering surfaces.

    [Edit_surface] displays an accepted {!Editor.t} through a {!Text_surface.t},
    with cursor display, focus styling and cursor-visible scrolling. Keys,
    pastes and pointer gestures become proposals: the whole next {!Editor.t},
    handed to the change callback. The surface never commits a proposal itself;
    it displays the state it is next given.

    This module is an implementation module for editable text widgets such as
    {!Textarea}. *)

type t
(** The type for editable text surfaces. *)

type mode = [ `Multiline | `Single_line ]
(** The editing mode. [`Single_line] strips inserted newlines and submits on
    Enter. Undo and redo that would restore multiline text are ignored, leaving
    the accepted editor and its history intact. *)

(** Editing actions resolved from key bindings. *)
type action =
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
(** The type for editing key bindings. *)

(** {1:properties Properties} *)

module Props : sig
  type t
  (** Declarative properties for an editable text surface. *)

  val make :
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
  (** [make ~state ()] describes [state] with textarea visual defaults. *)

  (**/**)

  val make_k :
    (t -> 'a) ->
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
    'a

  (**/**)

  val default : t
  (** [default] is [make ~state:Editor.empty ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)
end

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?mode:mode ->
  on_change:(Editor.t -> unit) ->
  ?on_submit:(string -> unit) ->
  Props.t ->
  t
(** [create ~parent ~on_change props] renders the accepted state of [props].
    Default editing events propose a complete next state through [on_change];
    they do not mutate the accepted editor. Apply new properties to accept a
    proposal. [on_submit] reports accepted text. Focus and blur do not commit
    edits. *)

(** {1:accessors Accessors} *)

val state : t -> Editor.t
(** [state t] is the accepted editor. *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val surface : t -> Text_surface.t
(** [surface t] is the underlying text surface. *)

val value : t -> string
(** [value t] is the current text content. *)

val cursor : t -> int
(** [cursor t] is the current cursor grapheme offset. *)

val selection : t -> (int * int) option
(** [selection t] is the active selection, if any. *)

(** {1:mutation Mutation} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] installs the complete accepted editor and visual
    properties. It does not emit proposals. Source indexes and geometry are
    reused when only the caret or selection changes. *)

(** {1:callbacks Callbacks} *)

val set_on_change : t -> (Editor.t -> unit) -> unit
(** [set_on_change t f] sets the complete-state proposal callback. *)

val set_on_submit : t -> (string -> unit) option -> unit
(** [set_on_submit t f] sets the submit notification. [None] clears it. *)

(** {1:paste Paste} *)

val handle_paste : t -> string -> unit
(** [handle_paste t text] proposes insertion of pasted text after stripping ANSI
    escapes. *)

(** {1:formatting Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats [t] for debugging. *)
