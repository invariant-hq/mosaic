(** Terminal SGR state tracker.

    Tracks the terminal's active rendering state (colors, attributes, hyperlink)
    and emits escape codes only when the requested style differs. Designed for
    hot render loops processing thousands of cells per frame.

    {!update} is zero-allocation: it writes directly to a {!Writer.t} using
    low-level primitives. No closures, lists, or intermediate strings are
    created. *)

(** {1:state State} *)

type t
(** The type for mutable SGR state trackers. Not thread-safe. *)

(** {1:lifecycle Lifecycle} *)

val create : unit -> t
(** [create ()] is a new state tracker. The initial state is "unknown",
    guaranteeing that the first {!update} emits a full reset and style
    application. *)

val reset : t -> unit
(** [reset s] invalidates [s], forcing the next {!update} to emit a full reset
    sequence. Use after external modifications to the output stream (e.g.
    subprocesses) or non-contiguous cursor jumps. *)

(** {1:operations Operations} *)

val update :
  t ->
  Writer.t ->
  fg_r:float ->
  fg_g:float ->
  fg_b:float ->
  fg_a:float ->
  bg_r:float ->
  bg_g:float ->
  bg_b:float ->
  bg_a:float ->
  attrs:int ->
  link:string ->
  unit
(** [update s w ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b ~bg_a ~attrs ~link]
    synchronizes the terminal with the requested style. Emits escape codes to
    [w] and updates [s] only if the style changed. Hyperlink changes are emitted
    first (OSC 8), then SGR codes if any component changed.

    Color components are normalized floats in \[0.0, 1.0\]. [attrs] is a bitmask
    compatible with {!Attr.pack}. [link] is the hyperlink URL or [""] for none.
*)

val close_link : t -> Writer.t -> unit
(** [close_link s w] closes any open hyperlink. Safe to call when no link is
    open. *)
