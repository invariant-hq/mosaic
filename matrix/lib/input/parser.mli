(** Incremental input parser.

    Consumes raw terminal bytes and emits user-facing events ({!Event.t}) and
    capability responses ({!Event.Caps.event}) through callbacks. A single
    parser instance drives both streams so callers do not need to run two
    parsers over the same bytes.

    {b Warning.} Not thread-safe; use one instance per input source. *)

(** {1:parsers Parsers} *)

type t
(** The type for parser state. *)

val create : unit -> t
(** [create ()] is a fresh parser with empty buffers. *)

(** {1:feeding Feeding} *)

val feed :
  t ->
  bytes ->
  int ->
  int ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_caps:(Event.Caps.event -> unit) ->
  unit
(** [feed p buf off len ~now ~on_event ~on_caps] consumes [len] bytes from [buf]
    starting at offset [off].

    Each parsed user event is passed to [on_event]; each capability response is
    passed to [on_caps]. Incomplete escape sequences are buffered and combined
    with subsequent calls.

    [now] is the current time in seconds since the epoch, used to schedule flush
    deadlines for ambiguous sequences. *)

(** {1:draining Draining} *)

val drain :
  t ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_caps:(Event.Caps.event -> unit) ->
  unit
(** [drain p ~now ~on_event ~on_caps] emits any pending escape sequences whose
    deadline has passed. A lone {!Event.Key.Escape} only appears after this
    drain when the terminal splits a modifier sequence across reads. Call after
    {!deadline} has elapsed. *)

val deadline : t -> float option
(** [deadline p] is the absolute timestamp (seconds since the epoch) at which
    the next {!drain} should fire, or [None] when no drain is scheduled. *)

(** {1:state State} *)

val pending : t -> bytes
(** [pending p] is a copy of the incomplete data buffered so far. Useful for
    diagnostics and tests. *)

val reset : t -> unit
(** [reset p] clears all parser state, dropping any buffered partial sequences
    and capability tracking. *)
