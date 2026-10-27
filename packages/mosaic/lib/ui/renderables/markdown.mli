(** Markdown content display.

    Parses CommonMark source via {e cmarkit} and renders it as a tree of child
    {!Renderable.t} values. Supports headings, paragraphs, code blocks, lists,
    blockquotes, tables, thematic breaks, and inline formatting (emphasis,
    strong, code spans, links, strikethrough, and task lists). Fenced code is
    highlighted by a {!Code.Highlighter.t} (see the [highlighter] argument of
    {!create}). *)

type t
(** The type for markdown display widgets. *)

(** {1:style Style} *)

(** The type for style keys identifying markdown element classes. *)
type style_key =
  | Default
  | Heading of int  (** Level, from 1 to 6. *)
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

(** Element styles. *)
module Style : sig
  type t
  (** The type for element styles: the terminal style of every {!style_key}.

      Styles are values. Two styles that give every key equal terminal styles
      are equal, so a view may rebuild its style on every render, or take it
      from the model, and only a change of some key's style re-renders the
      content. *)

  val make : ?base:t -> (style_key * Ansi.Style.t) list -> t
  (** [make ?base bindings] gives each key of [bindings] its terminal style and
      every other key the style [base] gives it. [base] defaults to {!default}.
      When a key appears more than once, its last binding wins.

      Raises [Invalid_argument] if a key is [Heading n] with [n] outside
      \[[1];[6]\], CommonMark's heading levels. *)

  val default : t
  (** [default] is the built-in terminal style:
      - [Default]: no decoration.
      - [Heading n]: bold; colored by level, with levels 4 to 6 sharing one
        color.
      - [Emphasis]: italic.
      - [Strong]: bold.
      - [Code_span] and [Code_block]: a light blue-gray.
      - [Link] and [Image]: underlined, light blue.
      - [Blockquote]: italic gray.
      - [Thematic_break], [Table_border] and [Conceal_punctuation]: dark gray.
      - [List_marker]: blue.
      - [Strikethrough]: struck-through gray.
      - [Task_marker]: bold green. *)

  val get : t -> style_key -> Ansi.Style.t
  (** [get t key] is [key]'s terminal style in [t].

      Raises [Invalid_argument] if [key] is [Heading n] with [n] outside
      \[[1];[6]\]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] give every key equal terminal
      styles. *)
end

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for bundled markdown widget properties. *)

  val make :
    ?content:string ->
    ?conceal:bool ->
    ?streaming:bool ->
    ?style:Style.t ->
    ?selectable:bool ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?highlighter:Code.Highlighter.t ->
    ?syntax_style:Syntax_style.t ->
    unit ->
    t
  (** [make ()] is a props value. With:
      - [content]: the markdown source text. Defaults to [""].
      - [conceal]: whether to hide markdown syntax characters. When [true], a
        link [[[text](url)]] is displayed as [text (url)]. Defaults to [true].
      - [streaming]: when [true], incomplete trailing content is handled
        gracefully; the last table row is skipped to avoid flickering during
        incremental appends. Defaults to [false].
      - [style]: the element style. Styles compare by value, so an equal style
        rebuilt on every view re-renders nothing. Defaults to {!Style.default}.
      - [selectable]: whether rendered markdown text can be selected. Defaults
        to [true].
      - [selection_bg] and [selection_fg]: selection colors applied to internal
        text children. Default to none.
      - [highlighter]: highlights fenced code blocks when {!create}'s
        [render_code] is not set. Each fenced block is rendered as a borderless
        {!Code} view. A block whose info string names a language is highlighted
        by [highlighter], which receives that language and the block's content
        in each request and interprets the language name itself; a block without
        a language is shown plain. [highlighter] is a resource compared by
        identity (see {!Code.Highlighter.t}): create it once and reuse it.
        Defaults to [None], which renders fenced blocks as bordered plain
        blocks.
      - [syntax_style]: maps the highlighter's scopes to terminal styles.
        Defaults to {!Syntax_style.default}. Styles compare by value, so
        rebuilding an equal style changes nothing, and a changed style restyles
        the blocks without highlighting them again.

      {!create} and {!Vnode.markdown} take these labels with these defaults,
      [style] being [md_style] in {!Vnode.markdown}. *)

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
  ?style:Style.t ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:(string option -> unit) ->
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
  ?highlighter:Code.Highlighter.t ->
  ?syntax_style:Syntax_style.t ->
  unit ->
  t
(** [create ~parent ()] is a markdown display widget attached to [parent]. The
    labels after [opacity], but [layout_style], [on_selection], [render_node]
    and [render_code], are {!Props.make}'s and take its defaults. With:
    - [layout_style]: the node's layout style, in a column of the full width
      whatever it gives.
    - [on_selection]: called with the selected rendered text when selection
      changes. [None] means no active selected text.
    - [render_node]: a custom block renderer called for every {!Cmarkit.Block.t}
      before default rendering. Return [Some r] to replace default rendering
      with renderable [r], or [None] to fall through to the default. The
      returned renderable must be attached to [parent].
    - [render_code]: a custom code block renderer called for code blocks before
      [render_node]. Receives the optional language tag and the code content.
      The returned renderable must be attached to [parent]. Use this for full
      control over code block rendering. When it is set, [highlighter] is not
      used. *)

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

val set_style : t -> Style.t -> unit
(** [set_style t style] changes the element style of [t] and re-renders.
    Re-parsing is skipped when blocks already exist; leaf blocks are updated in
    place without destroying their renderables. No-op when [style] equals the
    current style. *)

val set_highlighter : t -> Code.Highlighter.t option -> unit
(** [set_highlighter t h] changes the fenced-code highlighter of [t] to [h] (see
    the [highlighter] argument of {!create}) and re-renders. [None] falls back
    to the default bordered code block. Re-parsing is skipped when blocks
    already exist. No-op when [h] is physically equal to the current
    highlighter. *)

val set_syntax_style : t -> Syntax_style.t -> unit
(** [set_syntax_style t style] changes the style of highlighted fenced code
    blocks and restyles them without highlighting them again. No-op when [style]
    equals the current style. *)

val set_conceal : t -> bool -> unit
(** [set_conceal t v] sets whether [t] hides markdown syntax characters.
    Re-parsing is skipped when blocks already exist; leaf blocks are updated in
    place. No-op when [v] equals the current value. *)

val set_streaming : t -> bool -> unit
(** [set_streaming t v] enables ([true]) or disables ([false]) streaming mode on
    [t]. When enabled, incomplete trailing table rows are skipped. No-op when
    [v] equals the current value. *)

val set_selectable : t -> bool -> unit
(** [set_selectable t v] enables or disables text selection for the rendered
    markdown text children. *)

val set_selection_bg : t -> Ansi.Color.t option -> unit
(** [set_selection_bg t c] sets the selection background color on rendered text
    children. *)

val set_selection_fg : t -> Ansi.Color.t option -> unit
(** [set_selection_fg t c] sets the selection foreground color on rendered text
    children. *)

val set_on_selection : t -> (string option -> unit) option -> unit
(** [set_on_selection t f] sets the aggregate markdown selection callback.
    [None] clears it. *)

val selected_text : t -> string
(** [selected_text t] is the selected rendered text across all markdown text
    children, in render order. Returns [""] if no text is selected. *)

(** {1:apply Props application} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t p] applies the properties [p] to [t], updating content,
    style, conceal, and streaming mode as needed. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a markdown widget for debugging. *)
