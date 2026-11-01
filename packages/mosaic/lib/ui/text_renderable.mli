(** Text-backed renderable ownership.

    [Text_renderable] owns the common mutable state for leaf renderables backed
    by a {!Text_buffer.t} and rendered through a {!Text_surface.t}. It creates
    the underlying {!Renderable.t}, registers its text rows, which the view
    selection reads, and keeps surface invalidation consistent after buffer
    changes.

    This module is an implementation module for text-like widgets. *)

type t
(** The type for text-backed renderables. *)

(** {1:construction Construction} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?selectable:bool ->
  unit ->
  t
(** [create ~parent ()] is a text-backed renderable attached to [parent].

    [text_style] is the default style for plain text, [wrap] is the wrapping
    mode, [tab_width] is the tab stop width in columns, and [truncate] enables
    ellipsis truncation for unwrapped long lines. [selectable], when given,
    declares whether text selection reaches the renderable
    ({!Renderable.set_selectable}).

    Raises [Invalid_argument] if [tab_width < 1]. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val buffer : t -> Text_buffer.t
(** [buffer t] is the backing text buffer. *)

val surface : t -> Text_surface.t
(** [surface t] is the text surface rendering {!buffer}. *)

(** {1:content Content} *)

val set_text : t -> string -> unit
(** [set_text t s] replaces the buffer content by [s] styled with the current
    default text style and invalidates the surface. *)

val set_styled_text : t -> Text_buffer.span list -> unit
(** [set_styled_text t spans] replaces the buffer content by [spans] and
    invalidates the surface. *)

val set_annotations : t -> Text_buffer.Annotations.t -> unit
(** [set_annotations t annotations] installs the annotated source and paint,
    retaining the source index and geometry when the text is unchanged. *)

val set_text_style : ?restyle:string -> t -> Ansi.Style.t -> unit
(** [set_text_style ?restyle t style] sets the buffer default style to [style].
    If [restyle] is [Some s], [s] is re-applied as plain text so existing plain
    content observes the new default style. *)

(** {1:configuration Configuration} *)

val set_wrap : t -> Text_surface.wrap -> unit
(** [set_wrap t wrap] sets the wrapping mode. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t width] sets the tab width and invalidates the surface if
    the width changes.

    Raises [Invalid_argument] if [width < 1]. *)

val set_truncate : t -> bool -> unit
(** [set_truncate t truncate] enables or disables truncation. *)

val set_selectable : t -> bool option -> unit
(** [set_selectable t d] is {!Renderable.set_selectable} on {!node}. *)

val selected_text : t -> string
(** [selected_text t] is the part of the view selection in [t]'s text, [""] if
    none. *)

(** {1:highlights Highlights} *)

val add_highlight : t -> Text_buffer.Highlight.t -> unit
(** [add_highlight t h] adds highlight [h] and requests a render. *)

val remove_highlights_by_ref : t -> int -> unit
(** [remove_highlights_by_ref t ref_id] removes highlights whose
    {!Text_buffer.Highlight.ref_id} is [ref_id] and requests a render. *)

val clear_highlights : t -> unit
(** [clear_highlights t] removes all highlights and requests a render. *)

(** {1:query Query} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines. *)

val display_line_count : t -> int
(** [display_line_count t] is the number of display lines after wrapping. *)
