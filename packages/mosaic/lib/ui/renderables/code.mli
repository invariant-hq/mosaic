(** Syntax-highlighted code display.

    A leaf renderable for displaying source code with optional pre-computed
    source-range highlighting. Uses {!Text_buffer.t} for text storage and
    {!Text_surface.t} for rendering.

    [Code] differs from {!Text} in its defaults: wrapping defaults to [`None],
    tab width defaults to [4], and it registers as a
    {!type:Renderable.text_rows} provider.

    {1:highlighting Highlighting}

    Code colors byte ranges that something else computed. Either the ranges are
    already known ({!val-syntax}), or a {!Highlighter.t} computes them for
    whatever content the view shows ({!with_highlighter}). Mosaic holds no
    grammar and no table of languages: a language is a name the application
    passes to {!with_highlighter} and its highlighter interprets, so aliases,
    filename extensions and the choice of parser stay with the application. *)

(** {1:types Types} *)

type t
(** The type for code display renderables. *)

type syntax = Syntax_session.syntax
(** The type for source-code syntax settings.

    Syntax settings are values: their language, style and ranges compare by
    value, so a view may rebuild them on every render. Only the {!Highlighter.t}
    inside compares by identity (see {!Highlighter.t}).

    [style] only presents captures. A change of style alone restyles the result
    already received for the content without highlighting it again, and a
    pending result is styled with the settings current when it arrives. The same
    holds for the view's [conceal] property (see {!Props.make}). *)

module Highlighter : sig
  (** {1:requests Requests} *)

  type edit = { start : int; removed : int; inserted : int }
  (** The type for the change from one content to the next, in bytes. The
      [removed] bytes at [start] are replaced by [inserted] bytes, and the bytes
      before [start] and after the replaced ones are kept. In tree-sitter's
      terms, [start_byte = start], [old_end_byte = start + removed] and
      [new_end_byte = start + inserted]. *)

  type 'state previous = {
    content : string;  (** The content of the view's latest result. *)
    state : 'state;
        (** What the highlighter returned with that result (see {!type-result}).
        *)
    edit : edit;
        (** The change from [content] to the requested content. It keeps the
            longest common prefix, then the longest common suffix that does not
            overlap it. Changes made while a request runs reach the next request
            as the one edit that encloses them. *)
  }
  (** The type for what a view kept from its latest result. *)

  type 'state request = {
    content : string;
    language : string;
    range : int * int;
    previous : 'state previous option;
  }
  (** The type for highlight requests. [content] is the whole buffer the view
      shows, and [language] is the name given to {!with_highlighter}, passed
      through uninterpreted. [range] is the bytes of [content] whose captures
      the view needs, from [fst range] to [snd range]: all of [content] on the
      view's first request, as [previous] is [None], and for a highlighter made
      by {!sync} or {!threaded}; otherwise the rows the view shows and a screen
      above and below them.

      [previous] is [None] while the view holds no result of this highlighter
      for this language, as on its first request and after a change of
      highlighter or language. Otherwise it describes the view's latest result,
      so that a highlighter may reuse the work it did for it. A tree-sitter
      highlighter, for instance, edits the tree it returned as its state and
      parses [content] again from it, at a cost that grows with the size of the
      change. A highlighter that keeps nothing between results is made with
      {!sync} or {!threaded} from a function of the language and the content,
      and has [()] as its state. *)

  type 'state result = (Syntax_highlight.t * 'state, exn) Stdlib.result
  (** The type for highlighter results. [Ok (captures, state)] holds captures
      bound to the request's [content] (see {!Syntax_highlight.make}) and the
      state that the view's next request receives in its [previous].

      The captures must hold every capture of the whole highlight of [content]
      by [state] that meets the request's [range], whole, and only captures of
      that highlight; those outside [range] may be left out. [range] bounds
      which captures are returned and never what a capture depends on: a
      reference that a definition before [range] colours, as tree-sitter's
      locals do, is coloured as in the whole highlight. For tree-sitter, they
      are the matches of the highlights query on the whole returned tree that
      meet [range], with locals resolved over the whole tree. A highlighter must
      not piece them together from the previous result's captures, because a
      capture may depend on bytes far from the edit, such as through a pattern
      that relates siblings or a field of a parent, and no span around the edit
      bounds those bytes. A [previous] therefore changes the cost of a result,
      and changes its captures only through the state. A tree parsed again from
      an edited tree can differ from a fresh parse of the same bytes only where
      the bytes have a syntax error, where both trees are guesses of error
      recovery.

      An [Error], or captures bound to other bytes, is a highlighting failure.
      The view shows that content as plain text and keeps its latest result and
      state, so it may pass the same state again. A highlighter must therefore
      treat a state as a value and must not change one it returned. A
      tree-sitter highlighter copies a tree before it edits it, and the copy is
      cheap. *)

  type 'state job
  (** The type for an in-flight asynchronous highlight job. *)

  type t
  (** The type for source-code highlighters.

      A highlighter is a resource, not a value: it may own a worker thread and
      its queue of requests. Two highlighters are the same highlighter only when
      they are physically equal. Create a highlighter once, at module level or
      in the model, and reuse it across renders and code blocks; a highlighter
      created during a render is a new producer, and the code it configures is
      highlighted again. {!Diff} and {!Markdown} compare highlighters the same
      way, and one highlighter may serve any number of views, each with its own
      state.

      Each view has at most one request queued or running at a time, even across
      highlighter changes. Content and syntax changes made meanwhile coalesce
      into a single follow-up request, sent when that one completes. A state
      therefore reaches the highlighter only after the view has taken the result
      that carried it. Destroying the view cancels its request. *)

  val job :
    poll:(unit -> 'state result option) -> cancel:(unit -> unit) -> 'state job
  (** [job ~poll ~cancel] is an asynchronous job. [poll ()] returns [None] while
      the job is pending and [Some outcome] once it has completed. [cancel ()]
      asks the producer to stop work or discard its result when the widget is
      destroyed. While a job runs, later requests wait (see {!t}); an obsolete
      result is never shown as such (see {!with_highlighter}). A live job must
      therefore eventually complete even if its input has become obsolete. *)

  val sync : (language:string -> content:string -> Syntax_highlight.t) -> t
  (** [sync f] is a synchronous highlighter that keeps nothing between results:
      [f ~language ~content] runs on the rendering thread when content or syntax
      changes, and returns the captures of [content]. It is {!sync_incremental}
      with a function that ignores [previous] and [range], except that it is
      asked for all of [content], so scrolling the view asks it nothing. *)

  val threaded : (language:string -> content:string -> Syntax_highlight.t) -> t
  (** [threaded f] is {!threaded_incremental} with [f ~language ~content] for
      each request: [f] runs off the rendering thread and keeps nothing between
      results. Like {!sync}, it is asked for all of the content. *)

  val sync_incremental : ('state request -> Syntax_highlight.t * 'state) -> t
  (** [sync_incremental f] is a synchronous highlighter that may build on the
      view's latest result: [f] runs on the rendering thread when content or
      syntax changes. Exceptions raised by [f] are highlighting failures (see
      {!type-result}). *)

  val threaded_incremental :
    ('state request -> Syntax_highlight.t * 'state) -> t
  (** [threaded_incremental f] runs requests off the rendering thread, on the
      executor of the view's renderer ({!Renderable.off_loop}), which a Mosaic
      program's is its [process_perform]. Reuse the returned highlighter: it
      runs at most one call to [f] at a time, queues live requests in order,
      starts its worker on the executor when a request arrives and ends it when
      the queue empties. Views sharing the highlighter share its worker, so a
      slow call delays the requests queued behind it. Cancelled queued requests
      are removed immediately. A running call cannot be interrupted;
      cancellation discards its result. Polling and cancellation are thread-safe
      and do not wait for [f] to finish. Exceptions from [f] are highlighting
      failures of their request (see {!type-result}); the worker goes on with
      the next one.

      The worker publishes the immutable result and schedules a render; the
      result is applied on the rendering thread.

      [f] must finish, must not mutate renderables, and must synchronize any
      state shared outside this highlighter. A view's state reaches [f] on one
      thread at a time. On an executor of systhreads in the rendering domain,
      native calls that retain the OCaml runtime lock can still block rendering.
      Use {!async} for a different execution policy. *)

  val async : ('state request -> notify:(unit -> unit) -> 'state job) -> t
  (** [async f] is an asynchronous highlighter. [f] starts a job for a request.
      The job producer should call [notify] when [poll] may return a completed
      result. [notify] is safe to call from a worker thread. Its only effect is
      to schedule rendering; polling and applying results happen on the render
      thread. Exceptions raised by the starter or job polling are highlighting
      failures (see {!type-result}). *)
end

val syntax :
  ?language:string -> ?style:Syntax_style.t -> Syntax_highlight.t -> syntax
(** [syntax highlights] is a syntax configuration.

    - [language] names the source language when known.
    - [style] maps highlight scopes to terminal styles. Defaults to
      {!Syntax_style.default}.
    - [highlights] are bound to the exact content they describe. Mismatched
      content falls back to plain text, even when byte lengths agree. Omit
      [syntax] to render plain text. *)

val with_highlighter :
  language:string -> ?style:Syntax_style.t -> Highlighter.t -> syntax
(** [with_highlighter ~language highlighter] is a syntax configuration that
    derives highlight ranges from [highlighter], which receives [language] and
    the whole content in each request, with the bytes of it that the view needs
    ({!Highlighter.type-request}). Empty content is shown without a request.

    A synchronous highlighter's result is shown immediately. While an
    asynchronous result is pending, the code still shows its current content.
    Bytes it shares with the source of the latest result, as a common prefix or
    a common suffix, keep that result's styling, also under a capture the change
    cuts through, and the other bytes are plain. A concealing capture the change
    cuts through shows its bytes instead, since its replacement stood for all of
    them (see {!Syntax_highlight.conceals}). A streaming append therefore keeps
    its highlighted prefix and shows its new tail plain until the result
    arrives, and content seen for the first time shows plain. A result for
    content that has since changed is never shown as such, but it becomes the
    latest result, with its state, so styling keeps up with content that changes
    faster than it can be highlighted. A different highlighter or language
    starts again from plain text.

    A result restyles the bytes it was asked for, and the other bytes keep the
    captures of earlier results, moved across the changes made since as while a
    result is pending. When the view shows rows outside the bytes asked for
    since its content changed, it asks for them, and until that result arrives
    they show those earlier captures. *)

(** {1:props Props} *)

module Props : sig
  type t
  (** The type for code display props. *)

  val make :
    ?content:string ->
    ?syntax:syntax ->
    ?conceal:bool ->
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
      - [conceal] applies the captures' conceal metadata (see
        {!Syntax_highlight.to_annotations}), which may hide or replace the bytes
        a capture conceals. Defaults to [true].
      - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.
      - [wrap] is the wrapping mode. Defaults to [`None].
      - [tab_width] is the tab stop width in columns. Defaults to [4].
      - [truncate] controls whether long lines are truncated with an ellipsis.
        Defaults to [false].
      - [selectable] controls whether text can be selected. Defaults to [true].
      - [selection_bg] is the selection background color.
      - [selection_fg] is the selection foreground color.

      Raises [Invalid_argument] if [tab_width < 1]. *)

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
  ?conceal:bool ->
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
    - [conceal] applies the captures' conceal metadata. Defaults to [true].
    - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.
    - [wrap] is the wrapping mode. Defaults to [`None].
    - [tab_width] is the tab stop width in columns. Defaults to [4].
    - [truncate] controls whether long lines are truncated with an ellipsis.
      Defaults to [false].
    - [selectable] controls whether text can be selected. Defaults to [true].
    - [selection_bg] is the selection background color.
    - [selection_fg] is the selection foreground color.

    See also {!Props.make} for the code-specific props.

    Raises [Invalid_argument] if [tab_width < 1]. *)

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

val set_on_selection : t -> ((int * int) option -> unit) option -> unit
(** [set_on_selection t f] sets the selection-change callback used when
    [selectable = true]. [None] clears it. *)

val set_on_rows_change : t -> (unit -> unit) option -> unit
(** [set_on_rows_change t f] sets the callback called when content,
    highlighting, or layout configuration may have changed [t]'s
    {!Renderable.text_rows}. [None] clears it. *)

(** {1:props_application Props application} *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t], updating content, syntax,
    style, and wrapping as specified. *)

(** {1:query Query} *)

val line_count : t -> int
(** [line_count t] is the number of logical lines in [t]. *)

val is_highlighting : t -> bool
(** [is_highlighting t] is [true] while a highlighter result for [t]'s current
    content and syntax settings is pending. Meanwhile the shown buffer and its
    line metrics are provisional (see {!with_highlighter}); once [false], they
    are final until the content or syntax changes, or the view shows rows
    outside the bytes asked for since. *)

val display_line_count : t -> int
(** [display_line_count t] is the number of display lines in [t] after wrapping.
*)
