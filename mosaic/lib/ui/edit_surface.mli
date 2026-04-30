(** Editable text rendering surfaces.

    [Edit_surface] connects an {!Edit_buffer.t} to a {!Text_surface.t}. It
    synchronizes edited content into styled text storage and adds cursor
    display, focus styling, keyboard actions, paste handling, selection, and
    cursor-visible scrolling.

    This module is an implementation module for editable text widgets such as
    {!Textarea}. *)

type t
(** The type for editable text surfaces. *)

type mode = [ `Multiline | `Single_line ]
(** The editing mode. [`Single_line] strips newlines and submits on Enter. *)

(** {1:properties Properties} *)

module Props : sig
  type t
  (** Declarative properties for an editable text surface. *)

  val make :
    ?value:string ->
    ?cursor:int ->
    ?selection:(int * int) option ->
    ?spans:Text_buffer.span list ->
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
  (** [make ()] is a property set with textarea defaults. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical properties. *)

  val spans_equal : Text_buffer.span list -> Text_buffer.span list -> bool
  (** [spans_equal a b] is [true] iff [a] and [b] contain the same styled text.
  *)
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
  ?value:string ->
  ?cursor:int ->
  ?selection:(int * int) option ->
  ?spans:Text_buffer.span list ->
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
  ?mode:mode ->
  ?max_length:int ->
  ?on_input:(string -> unit) ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> unit) ->
  unit ->
  t
(** [create ~parent ()] is an editable text surface attached to [parent]. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val buffer : t -> Edit_buffer.t
(** [buffer t] is the underlying edit buffer. *)

val surface : t -> Text_surface.t
(** [surface t] is the underlying text surface. *)

val value : t -> string
(** [value t] is the current text content. *)

val cursor : t -> int
(** [cursor t] is the current cursor grapheme offset. *)

val selection : t -> (int * int) option
(** [selection t] is the active selection, if any. *)

(** {1:mutation Mutation} *)

val set_value : t -> string -> unit
(** [set_value t s] replaces the text content with [s]. *)

val set_max_length : t -> int -> unit
(** [set_max_length t n] updates the backing buffer's maximum grapheme count. *)

val edit : t -> (Edit_buffer.t -> bool) -> unit
(** [edit t f] runs [f] on the underlying edit buffer and synchronizes the
    surface if [f] returns [true]. *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] updates [t] to match [props]. *)

(** {1:callbacks Callbacks} *)

val set_on_input : t -> (string -> unit) option -> unit
(** [set_on_input t handler] sets the input callback. *)

val set_on_change : t -> (string -> unit) option -> unit
(** [set_on_change t handler] sets the committed-change callback. *)

val set_on_submit : t -> (string -> unit) option -> unit
(** [set_on_submit t handler] sets the submit callback. *)

val set_on_cursor :
  t -> (cursor:int -> selection:(int * int) option -> unit) option -> unit
(** [set_on_cursor t handler] sets the cursor/selection callback. *)

(** {1:paste Paste} *)

val handle_paste : t -> string -> unit
(** [handle_paste t text] inserts [text] as pasted text after removing ANSI
    escape sequences. *)

(** {1:formatting Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats [t] for debugging. *)
