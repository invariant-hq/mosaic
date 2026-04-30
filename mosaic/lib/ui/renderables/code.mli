(** Syntax-highlighted code display.

    A leaf renderable for displaying source code with optional pre-computed
    source-range highlighting. Uses {!Text_buffer.t} for text storage and
    {!Text_surface.t} for rendering.

    [Code] differs from {!Text} in its defaults: wrapping defaults to [`None],
    tab width defaults to [4], and it registers as a
    {!type:Renderable.line_info} provider. *)

(** {1:types Types} *)

type t
(** The type for code display renderables. *)

type syntax
(** The type for source-code syntax settings. *)

val syntax :
  ?language:string ->
  ?style:Syntax_style.t ->
  ?conceal:bool ->
  ?draw_unstyled:bool ->
  ?streaming:bool ->
  Syntax_highlight.t ->
  syntax
(** [syntax highlights] is a syntax configuration.

    - [language] names the source language when known.
    - [style] maps highlight scopes to terminal styles. Defaults to
      {!Syntax_style.default}.
    - [highlights] are source byte ranges for [content]. Omit [syntax] to render
      plain text.
    - [conceal] enables conceal metadata in [highlights]. Defaults to [true].
    - [draw_unstyled] records whether plain source should be visible while
      highlighting is pending. Synchronous highlighting always renders a final
      buffer immediately.
    - [streaming] records whether later asynchronous highlighters should retain
      the previous rendered buffer while fresh highlighting is pending. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for code display props. *)

  val make :
    ?content:string ->
    ?syntax:syntax ->
    ?text_style:Ansi.Style.t ->
    ?wrap:Text_surface.wrap ->
    ?tab_width:int ->
    ?truncate:bool ->
    ?selectable:bool ->
    ?selection_bg:Ansi.Color.t ->
    ?selection_fg:Ansi.Color.t ->
    unit ->
    t
  (** [make ()] is a code props value with:
      - [content] is the plain source code text. Defaults to [""].
      - [syntax] is the source-code syntax configuration. Defaults to [None].
      - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.
      - [wrap] is the wrapping mode. Defaults to [`None].
      - [tab_width] is the tab stop width in columns. Defaults to [4].
      - [truncate] controls whether long lines are truncated with an ellipsis.
        Defaults to [false].
      - [selectable] controls whether text can be selected. Defaults to [true].
      - [selection_bg] is the selection background color.
      - [selection_fg] is the selection foreground color. *)

  val default : t
  (** [default] is the default props value. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same prop values. *)
end

(** {1:constructors Constructors} *)

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?content:string ->
  ?syntax:syntax ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> unit) ->
  unit ->
  t
(** [create ~parent ()] is a new code display renderable attached to [parent]
    with:
    - [index] is the child insertion index in [parent].
    - [id] is an optional identifier for the node.
    - [style] is the layout style.
    - [visible] controls initial visibility.
    - [z_index] is the stacking order.
    - [opacity] is the opacity. Defaults to [1.0].
    - [content] is the plain source code text. Defaults to [""].
    - [syntax] configures source-code highlighting.
    - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.
    - [wrap] is the wrapping mode. Defaults to [`None].
    - [tab_width] is the tab stop width in columns. Defaults to [4].
    - [truncate] controls whether long lines are truncated with an ellipsis.
      Defaults to [false].
    - [selectable] controls whether text can be selected. Defaults to [true].
    - [selection_bg] is the selection background color.
    - [selection_fg] is the selection foreground color.

    See also {!Props.make} for the code-specific props. *)

(** {1:accessors Accessors} *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying {!Renderable.t} node. *)

val buffer : t -> Text_buffer.t
(** [buffer t] is [t]'s backing {!Text_buffer.t}. *)

val surface : t -> Text_surface.t
(** [surface t] is [t]'s {!Text_surface.t}. *)

(** {1:content Content} *)

val set_content : t -> string -> unit
(** [set_content t s] sets the code content to [s] and re-applies the current
    syntax configuration. *)

val set_syntax : t -> syntax option -> unit
(** [set_syntax t syntax] sets the syntax configuration and re-renders [t]'s
    current content. [None] renders plain text. *)

val set_highlights : t -> Syntax_highlight.t -> unit
(** [set_highlights t highlights] replaces the current syntax highlights,
    creating a default syntax configuration when needed. *)

val set_on_selection : t -> ((int * int) option -> unit) option -> unit
(** [set_on_selection t f] sets the selection-change callback used when
    [selectable = true]. [None] clears it. *)

(** {1:props_application Props application} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t], updating content, syntax,
    style, and wrapping as specified. *)

(** {1:query Query} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines in [t]. *)

val is_highlighting : t -> bool
(** [is_highlighting t] is [true] while asynchronous highlighting work is
    pending. The current implementation is synchronous and always returns
    [false]. *)

val display_line_count : t -> int
(** [display_line_count t] is the number of display lines in [t] after wrapping.
*)
