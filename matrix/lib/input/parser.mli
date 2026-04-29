(** Incremental input parser.

    Consumes raw terminal bytes and emits user-facing events ({!Event.t}) and
    terminal responses ({!Event.Response.t}) through callbacks. A single parser
    instance drives both streams so callers do not need to run two parsers over
    the same bytes.

    {b Warning.} Not thread-safe; use one instance per input source. *)

(** {1:parsers Parsers} *)

type t
(** The type for parser state. *)

type protocol_context = {
  kitty_keyboard : bool;
      (** [true] iff Kitty keyboard protocol replies or key events are expected.
      *)
  explicit_width_cpr : bool;
      (** [true] iff an explicit-width cursor-position response is expected. *)
  startup_cursor_cpr : bool;
      (** [true] iff a generic startup cursor-position response is expected. *)
  pixel_resolution : bool;
      (** [true] iff a pixel-resolution response is expected. *)
  private_capability_replies : bool;
      (** [true] iff private capability replies such as DA, DECRPM, and
          colour-scheme reports are expected. *)
}
(** The type for protocol context used to defer incomplete terminal replies
    across the ordinary escape-sequence timeout. *)

val default_protocol_context : protocol_context
(** [default_protocol_context] has all protocol context fields set to [false].
*)

val create : unit -> t
(** [create ()] is a fresh parser with empty buffers. *)

val set_protocol_context : t -> protocol_context -> unit
(** [set_protocol_context p ctx] sets the protocol context for [p].

    The context affects timeout handling for incomplete protocol replies. It
    does not emit or discard already-buffered data. *)

val protocol_context : t -> protocol_context
(** [protocol_context p] is [p]'s current protocol context. *)

(** {1:feeding Feeding} *)

val feed :
  t ->
  bytes ->
  int ->
  int ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_response:(Event.Response.t -> unit) ->
  unit
(** [feed p buf off len ~now ~on_event ~on_response] consumes [len] bytes from
    [buf] starting at offset [off].

    Each parsed user event is passed to [on_event]; each terminal response is
    passed to [on_response]. Incomplete escape sequences are buffered and
    combined with subsequent calls.

    [now] is the current time in seconds since the epoch, used to schedule flush
    deadlines for ambiguous sequences. *)

(** {1:draining Draining} *)

val drain :
  t ->
  now:float ->
  on_event:(Event.t -> unit) ->
  on_response:(Event.Response.t -> unit) ->
  unit
(** [drain p ~now ~on_event ~on_response] emits any pending escape sequences
    whose deadline has passed. A lone {!Event.Key.Escape} only appears after
    this drain when the terminal splits a modifier sequence across reads. Call
    after {!deadline} has elapsed. *)

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
