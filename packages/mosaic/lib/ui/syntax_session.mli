(** The highlighting of a view's content.

    A session keeps a view's captures up to date with the content it shows. It
    requests captures from a highlighter, at most one request at a time,
    coalesces the changes made meanwhile, keeps the latest result and the
    highlighter's state for the next request, and says which captures describe
    the content shown. A request asks for the bytes the view needs ({!view}); a
    result replaces the captures over the bytes it was asked for, and the
    captures elsewhere stay, carried across the changes made since. {!Code} and
    the editable surface of {!Textarea} each hold one. {!Code} documents
    highlighters and syntax settings. *)

(** {1:highlighters Highlighters} *)

module Highlighter : sig
  type edit = { start : int; removed : int; inserted : int }
  type 'state previous = { content : string; state : 'state; edit : edit }

  type 'state request = {
    content : string;
    language : string;
    range : int * int;
    previous : 'state previous option;
  }

  type 'state result = (Syntax_highlight.t * 'state, exn) Stdlib.result
  type 'state job
  type t

  val job :
    poll:(unit -> 'state result option) -> cancel:(unit -> unit) -> 'state job

  val sync : (language:string -> content:string -> Syntax_highlight.t) -> t
  val threaded : (language:string -> content:string -> Syntax_highlight.t) -> t
  val sync_incremental : ('state request -> Syntax_highlight.t * 'state) -> t

  val threaded_incremental :
    ('state request -> Syntax_highlight.t * 'state) -> t

  val async : ('state request -> notify:(unit -> unit) -> 'state job) -> t
end

(** {1:syntax Syntax settings} *)

type syntax
(** Where a view's captures come from, and the style that presents them. *)

val syntax :
  ?language:string -> ?style:Syntax_style.t -> Syntax_highlight.t -> syntax

val with_highlighter :
  language:string -> ?style:Syntax_style.t -> Highlighter.t -> syntax

val language : syntax -> string option
val style : syntax -> Syntax_style.t

val equal : syntax -> syntax -> bool
(** [equal a b] compares by value, except highlighters, by identity. *)

(** {1:sessions Sessions} *)

type t
(** The type for a view's highlighting. *)

val create : notify:(unit -> unit) -> off_loop:((unit -> unit) -> unit) -> t
(** [create ~notify ~off_loop] shows nothing yet. [notify] schedules a render of
    the view; a highlighter may call it from any thread. [off_loop] runs a
    threaded highlighter's work off the rendering thread
    ({!Renderable.off_loop}). *)

val set : t -> content:string -> range:int * int -> syntax option -> unit
(** [set t ~content ~range syntax] makes [t] describe [content] under [syntax],
    of which the view needs the bytes in [range] (see {!view}). A change of
    content, highlighter or language requests captures; a change of style alone
    requests nothing. *)

val needs_range : t -> bool
(** [needs_range t] is [true] when {!view} may request captures: [t]'s
    highlighter keeps state between results, and [t] holds a result of it. A
    highlighter that keeps nothing, and every highlighter on a view's first
    request, is asked for the whole content. *)

val view : t -> range:int * int -> bool
(** [view t ~range] says that the view needs the captures of the bytes from
    [fst range] to [snd range] of its content: the rows it shows and a screen
    above and below them. If they are outside the results for the content shown,
    and no request is pending and none failed since the content changed, [view]
    requests them, and is [true] iff {!shown} may have changed, as {!poll}. *)

val poll : t -> bool
(** [poll t] takes the results of completed requests and starts the request that
    waited for them. It is [true] iff {!shown} may have changed. *)

val shown : t -> conceal:bool -> Syntax_highlight.t option
(** [shown t ~conceal] is the captures that describe the content shown, bound to
    it, or [None] when it shows plain. Each result's captures replace those of
    the bytes it was asked for, and the others stay. Captures computed for an
    earlier content are moved onto the content: a capture keeps the bytes the
    content shares with their source as a common prefix or suffix, and the other
    bytes are plain. When [conceal] holds, a concealing capture that the change
    cuts through is dropped, since its replacement stood for all its bytes.
    After a failure for the content shown, it shows plain. *)

val is_highlighting : t -> bool
(** [is_highlighting t] is [true] while a result for the content shown is
    pending. *)

val cancel : t -> unit
(** [cancel t] cancels the request in flight and drops the one waiting. *)
