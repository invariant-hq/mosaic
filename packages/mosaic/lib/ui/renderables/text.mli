(** Rich text rendering with styled fragments.

    A text node renders styled content into the grid, handling line breaks and
    optional word/character wrapping. Content can be plain text (via
    {!set_content}) or structured rich text with per-run styles (via
    {!set_fragments} or {!set_spans}).

    Internally backed by a {!Text_buffer.t} and {!Text_surface.t}.

    {2:representations Representations}

    Text accepts three forms of content:
    - {e Content}: a plain string set via {!set_content} or the [content]
      parameter of {!Props.make}. Styled with {!Text_buffer.val-default_style}.
    - {e Fragments/Spans}: structured styled content set via {!set_fragments} or
      {!set_spans}. Fragments support nested style hierarchies; spans are flat
      styled text chunks.
    - {e Styled text}: concrete {!Text_buffer.span}s set via {!set_styled_text}.
      These styles remain unchanged when the base style changes.

    {2:style_merging Style merging}

    Styles merge hierarchically: parent styles provide the base and child
    overrides apply on top, preserving unspecified parent attributes. The
    [text_style] parameter provides the base style for plain text and fragments.
    Concrete styled text keeps its supplied styles. Each resulting style is
    merged over the [Plain] role of the theme in scope as the text is drawn
    ({!Text_surface.section-paint}), so a color the content does not give is the
    role's.

    See {!Text_input} for single-line editing and {!Textarea} for multi-line
    editing. *)

type t
(** A text widget. *)

(** {1:types Types} *)

type fragment =
  | Text of { text : string; style : Ansi.Style.t option }
  | Span of { style : Ansi.Style.t option; children : fragment list }
      (** Structured text fragments for rich text composition.
          - [Text]: leaf node with text and optional style override.
          - [Span]: container applying an optional style to all [children]. *)

type span = { text : string; style : Ansi.Style.t option }
(** A flat styled text chunk. [style] overrides the default text style when
    [Some]; [None] inherits the default. *)

(** {1:fragment_builder Fragment builder}

    Convenience constructors for {!type-fragment} values. *)

module Fragment : sig
  type t = fragment

  val text : ?style:Ansi.Style.t -> string -> t
  (** [text ?style s] is a text fragment containing [s]. When [style] is
      provided, it overrides the inherited style for this fragment. *)

  val span : ?style:Ansi.Style.t -> t list -> t
  (** [span ?style children] is a container grouping [children] under an
      optional shared [style]. *)

  (** {2:attr_builders Attribute builders}

      Each wraps [children] in a {!val-span} with a single attribute set. *)

  val bold : t list -> t
  val italic : t list -> t
  val underline : t list -> t
  val dim : t list -> t
  val blink : t list -> t
  val inverse : t list -> t
  val hidden : t list -> t
  val strikethrough : t list -> t

  (** {2:combo_builders Combination builders} *)

  val bold_italic : t list -> t
  val bold_underline : t list -> t
  val italic_underline : t list -> t
  val bold_italic_underline : t list -> t

  (** {2:color_builders Color builders} *)

  val fg : Ansi.Color.t -> t list -> t
  (** [fg color children] sets the foreground color. *)

  val bg : Ansi.Color.t -> t list -> t
  (** [bg color children] sets the background color. *)

  val color : Ansi.Color.t -> t list -> t
  (** [color] is {!fg}. *)

  val bg_color : Ansi.Color.t -> t list -> t
  (** [bg_color] is {!bg}. *)

  val styled : Ansi.Style.t -> t list -> t
  (** [styled style children] wraps [children] with the full [style]. *)
end

(** {1:construction Construction} *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is the underlying renderable. *)

val buffer : t -> Text_buffer.t
(** [buffer t] is the underlying text buffer. *)

val surface : t -> Text_surface.t
(** [surface t] is the underlying text surface. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** Declarative property bundle for reconciler diffing. *)

  val make :
    ?content:string ->
    ?text_style:Ansi.Style.t ->
    ?wrap:Text_surface.wrap ->
    ?selectable:bool ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?tab_width:int ->
    ?truncate:bool ->
    unit ->
    t
  (** [make ()] is the properties of a text, with:
      - [content]: plain text content. Defaults to [""].
      - [text_style]: base visual style for all content. Defaults to
        {!Ansi.Style.default}.
      - [wrap]: wrapping mode. Defaults to [`None].
      - [selectable]: whether text can be selected. Defaults to [true].
      - [selection_bg]: background color for selected text. Defaults to [None].
      - [selection_fg]: foreground color for selected text. Defaults to [None].
        Without either, selected text is drawn over the [Highlight] surface
        ({!Text_surface.section-paint}).
      - [tab_width]: tab stop width. Defaults to [2].
      - [truncate]: whether to truncate with ellipsis when [wrap = `None].
        Defaults to [false].

      [Mosaic.text] and [Mosaic.rich_text] take these labels with these
      defaults.

      Raises [Invalid_argument] if [tab_width < 1]. *)

  val make_rich :
    ?text_style:Ansi.Style.t ->
    ?wrap:Text_surface.wrap ->
    ?selectable:bool ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    ?tab_width:int ->
    ?truncate:bool ->
    fragment list ->
    t
  (** [make_rich fragments] declares structured text with the same presentation
      defaults as {!make}. Fragment styles inherit from [text_style].

      Raises [Invalid_argument] if [tab_width < 1]. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe identical visual
      properties. *)
end

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?on_selection:((int * int) option -> unit) ->
  Props.t ->
  t
(** [create ~parent props] is a text node attached to [parent], showing [props].
    [on_selection] is called with the selected grapheme range, or [None], when
    the selection of a [selectable] text changes; [Mosaic.text] and
    [Mosaic.rich_text] take it as a callback that returns a message. *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] replaces the accepted properties and content of [t],
    including content installed by imperative setters. It triggers the minimum
    necessary layout and render updates. *)

(** {1:content Content} *)

val set_content : t -> string -> unit
(** [set_content t s] sets plain text content styled with {!set_text_style}.
    Unchanged text retains indexed source and geometry. *)

val set_fragments : t -> fragment list -> unit
(** [set_fragments t fragments] replaces content with [fragments]. Normalization
    merges adjacent text with identical styles, removes empty fragments, and
    removes empty spans. No-op if the normalized result equals the accepted
    fragments. *)

val fragments : t -> fragment list
(** [fragments t] represents the accepted content as fragments. Plain content
    becomes an unstyled text fragment; concrete styled text becomes flat
    explicitly styled fragments. Structured fragments retain their hierarchy. *)

val fragments_equal : fragment list -> fragment list -> bool
(** [fragments_equal a b] is [true] iff [a] and [b] are structurally equal. *)

val plain_text : t -> string
(** [plain_text t] is the accepted text without styling. Its concatenation is
    cached until the text changes. *)

val spans : t -> span list
(** [spans t] is the accepted content as flat spans. Fragment styles are
    resolved against the base style; [None] denotes that base style. Concrete
    styled text retains its supplied styles as [Some] values. The list is cached
    until content or base style changes. *)

val set_spans : t -> span list -> unit
(** [set_spans t spans] replaces content with flat [spans]. Converts to
    fragments internally. *)

val append_span : t -> span -> unit
(** [append_span t s] appends [s] to the accepted content. Structured content
    retains its style hierarchy. For concrete styled text, [s]'s inherited
    components are resolved against the current base style when appended. *)

val clear_spans : t -> unit
(** [clear_spans t] removes all content. *)

val set_styled_text : t -> Text_buffer.span list -> unit
(** [set_styled_text t spans] replaces the accepted content with concrete styled
    [spans], without applying the base style. The supplied list is installed
    directly in the backing buffer. *)

val set_text_style : t -> Ansi.Style.t -> unit
(** [set_text_style t s] changes the base text style. Plain text and fragments
    repaint with the new inherited style; explicit fragment colours remain.
    Concrete styled text retains its supplied styles. Geometry is unchanged. *)

(** {1:wrapping Wrapping} *)

val set_wrap : t -> Text_surface.wrap -> unit
(** [set_wrap t mode] changes the wrapping mode. Delegates to
    {!Text_surface.set_wrap}. *)

val set_tab_width : t -> int -> unit
(** [set_tab_width t w] changes the tab stop width. Delegates to
    {!Text_buffer.set_tab_width}.

    Raises [Invalid_argument] if [w < 1]. *)

(** {1:selection Selection} *)

val set_selectable : t -> bool -> unit
(** [set_selectable t v] enables or disables text selection. *)

val set_selection_bg : t -> Ansi.Color.t option -> unit
(** [set_selection_bg t color] sets the selection background color. *)

val set_selection_fg : t -> Ansi.Color.t option -> unit
(** [set_selection_fg t color] sets the selection foreground color. *)

val set_on_selection : t -> ((int * int) option -> unit) option -> unit
(** [set_on_selection t f] sets the selection-change callback used when
    [selectable = true]. [None] clears it. *)

val selected_text : t -> string
(** [selected_text t] is the currently selected text. Returns [""] if no
    selection is active. *)

(** {1:highlights Highlights} *)

val add_highlight : t -> Text_buffer.Highlight.t -> unit
(** [add_highlight t h] adds a highlight overlay and requests a render. *)

val remove_highlights_by_ref : t -> int -> unit
(** [remove_highlights_by_ref t ref_id] removes highlights by
    {!Text_buffer.Highlight.ref_id} and requests a render. *)

val clear_highlights : t -> unit
(** [clear_highlights t] removes all highlights and requests a render. *)

(** {1:query Query} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines. *)

val display_line_count : t -> int
(** [display_line_count t] is the number of wrapped display lines. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)
