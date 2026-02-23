(** Markdown content display.

    Parses CommonMark source via {e cmarkit} and renders it as a tree of child
    {!Renderable.t} values. Supports headings, paragraphs, code blocks, lists,
    blockquotes, tables, thematic breaks, and inline formatting (emphasis,
    strong, code spans, links, strikethrough, and task lists). *)

type t
(** The type for markdown display widgets. *)

(** {1:style Style} *)

(** The type for style keys identifying markdown element classes. *)
type style_key =
  | Default
  | Heading of int  (** Level (1-3 have distinct colors; 4+ share one). *)
  | Emphasis
  | Strong
  | Code_span
  | Code_block
  | Link
  | Image
  | Blockquote
  | Thematic_break
  | List_marker
  | Strikethrough
  | Task_marker
  | Table_border  (** Border color for table borders. *)
  | Conceal_punctuation
      (** Style for markdown syntax markers ([\*], [\`], [\[], etc.). *)

type style = style_key -> Ansi.Style.t
(** The type for style resolvers. Maps a {!style_key} to the {!Ansi.Style.t} to
    apply when rendering that element. *)

val default_style : style
(** [default_style] is the built-in terminal style resolver. The mapping is:
    - [Default]: no decoration.
    - [Heading n]: bold; colored by level (magenta for 1, green for 2, cyan for
      3, blue for 4+).
    - [Emphasis]: italic.
    - [Strong]: bold.
    - [Code_span] and [Code_block]: dim.
    - [Link] and [Image]: underline and cyan.
    - [Blockquote]: italic and yellow.
    - [Thematic_break] and [List_marker]: dim.
    - [Strikethrough]: strikethrough.
    - [Task_marker]: bold.
    - [Table_border]: dim.
    - [Conceal_punctuation]: dim. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for bundled markdown widget properties. *)

  val make :
    ?content:string ->
    ?conceal:bool ->
    ?streaming:bool ->
    ?style:style ->
    unit ->
    t
  (** [make ()] is a props value. With:
      - [content]: the markdown source text. Defaults to [""].
      - [conceal]: whether to hide markdown syntax characters. Defaults to
        [true].
      - [streaming]: whether to enable streaming mode. Defaults to [false].
      - [style]: the style resolver. Defaults to {!default_style}. *)

  val default : t
  (** [default] is the default props value, equivalent to [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are equal. *)
end

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?layout_style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?content:string ->
  ?style:style ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?render_node:
    (Cmarkit.Block.t ->
    parent:Renderable.t ->
    is_last:bool ->
    Renderable.t option) ->
  ?render_code:
    (parent:Renderable.t ->
    language:string option ->
    content:string ->
    Renderable.t) ->
  unit ->
  t
(** [create ~parent ()] is a markdown display widget attached to [parent]. With:
    - [content]: the markdown source text. Defaults to [""].
    - [style]: the style resolver. Defaults to {!default_style}.
    - [conceal]: whether to hide markdown syntax characters. When [true], a link
      [[[text](url)]] is displayed as [text (url)]. Defaults to [true].
    - [streaming]: when [true], incomplete trailing content is handled
      gracefully; the last table row is skipped to avoid flickering during
      incremental appends. Defaults to [false].
    - [render_node]: a custom block renderer called for every {!Cmarkit.Block.t}
      before default rendering. Return [Some r] to replace default rendering
      with renderable [r], or [None] to fall through to the default. The
      returned renderable must be attached to [parent].
    - [render_code]: a custom code block renderer called for code blocks before
      [render_node]. Receives the optional language tag and the code content.
      The returned renderable must be attached to [parent]. Use this to
      integrate syntax highlighting. *)

val node : t -> Renderable.t
(** [node t] is the underlying {!Renderable.t} for [t]. *)

val set_layout_style : t -> Toffee.Style.t -> unit
(** [set_layout_style t s] updates the layout style of [t] to [s], preserving
    the required column direction and full-width sizing. *)

(** {1:content Content} *)

val set_content : t -> string -> unit
(** [set_content t s] replaces the markdown source of [t] with [s] and
    re-renders. Unchanged blocks are updated in place without destroying their
    renderables, making this efficient for streaming appends. No-op when [s]
    equals the current content.

    See also {!content}. *)

val content : t -> string
(** [content t] is the current markdown source of [t].

    See also {!set_content}. *)

(** {1:config Configuration} *)

val set_style : t -> style -> unit
(** [set_style t f] changes the style resolver of [t] to [f] and re-renders.
    Re-parsing is skipped when blocks already exist; leaf blocks are updated in
    place without destroying their renderables. *)

val set_conceal : t -> bool -> unit
(** [set_conceal t v] sets whether [t] hides markdown syntax characters.
    Re-parsing is skipped when blocks already exist; leaf blocks are updated in
    place. No-op when [v] equals the current value. *)

val set_streaming : t -> bool -> unit
(** [set_streaming t v] enables ([true]) or disables ([false]) streaming mode on
    [t]. When enabled, incomplete trailing table rows are skipped. No-op when
    [v] equals the current value. *)

(** {1:apply Props application} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t p] applies the properties [p] to [t], updating content,
    style, conceal, and streaming mode as needed. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a markdown widget for debugging. *)
