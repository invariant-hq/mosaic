(** Incremental tokenizer for terminal input.

    Splits a raw byte stream into complete escape sequences, contiguous runs of
    non-escape bytes, and bracketed paste payloads. Tokens preserve input order.

    {!Sequence} tokens begin with ESC and contain exactly one complete CSI, OSC,
    DCS, SS3, etc. sequence. {!Text} tokens never contain ESC and may hold
    arbitrary UTF-8 (validation is deferred to higher layers). {!Paste} tokens
    hold the complete bracketed paste payload with markers stripped.

    {b Warning.} Not thread-safe; use one instance per input source.

    {1:limits Safety limits}

    The paste buffer is capped at 1 MB; payloads exceeding this limit are
    silently truncated. The escape-sequence buffer is capped at 4 KB; if an
    incomplete sequence grows beyond this limit the bytes are flushed as plain
    text. *)

(** {1:tokens Tokens} *)

(** The type for tokens. *)
type token =
  | Sequence of string
      (** [Sequence seq] is a complete escape or control sequence.
          [seq.[0] = '\\x1b']. Bracketed paste start and end markers are emitted
          verbatim. *)
  | Text of string
      (** [Text run] is a maximal run of bytes containing no ESC. May contain
          newlines or multi-byte UTF-8. *)
  | Paste of string
      (** [Paste payload] is the complete bracketed paste payload (markers
          stripped). Emitted between the start and end marker {!Sequence}
          tokens. *)

(** {1:parsers Parsers} *)

type parser
(** The type for incremental tokenizer state. Accumulates partial sequences
    between {!feed} calls and tracks whether a bracketed paste is currently
    open. *)

val create : unit -> parser
(** [create ()] is a tokenizer with empty buffers and paste tracking disabled.
*)

(** {1:feeding Feeding} *)

val feed : parser -> bytes -> int -> int -> now:float -> token list
(** [feed p buf off len ~now] ingests [len] bytes from [buf] starting at offset
    [off] and returns all tokens that can be emitted.

    Token order matches input order. Escape sequences are only emitted once the
    full sequence (including terminators) has been received. When a bracketed
    paste start marker is seen, subsequent bytes are buffered until the matching
    end marker, at which point the payload is returned as a single {!Paste}
    token between the start and end {!Sequence} tokens.

    Returns an empty list if [buf] ends in the middle of a sequence or inside a
    paste payload. Does not mutate [buf]; the caller may reuse it.

    [now] is the current time in seconds since the epoch, used to schedule flush
    deadlines for partial sequences.

    Raises [Invalid_argument] if [off] and [len] describe a range outside [buf].
*)

val feed_iter :
  parser -> bytes -> int -> int -> now:float -> emit:(token -> unit) -> unit
(** [feed_iter p buf off len ~now ~emit] is like {!feed}, but calls [emit] once
    for each token instead of allocating and returning a token list. Tokens are
    emitted in input order. *)

(** {1:flushing Flushing} *)

val deadline : parser -> float option
(** [deadline p] is the absolute time (seconds since the epoch) at which the
    tokenizer will flush its pending escape sequence, or [None] when no flush is
    scheduled. *)

val wake_deferred : parser -> unit
(** [wake_deferred p] schedules an immediate flush for a pending sequence that
    was deferred by {!flush_expired}. Does nothing when no sequence is deferred.
*)

val flush_expired : ?defer:(string -> bool) -> parser -> float -> token list
(** [flush_expired ~defer p now] emits any pending partial sequence whose
    deadline is at or before [now]. Returns an empty list if nothing was
    flushed.

    If [defer pending] is [true], [pending] remains buffered and no new deadline
    is scheduled until more bytes arrive or {!wake_deferred} is called. [defer]
    defaults to [fun _ -> false]. *)

val flush_expired_iter :
  ?defer:(string -> bool) -> parser -> float -> emit:(token -> unit) -> unit
(** [flush_expired_iter ~defer p now ~emit] is like {!flush_expired}, but calls
    [emit] once for each flushed token instead of allocating and returning a
    token list. *)

(** {1:state State} *)

val pending : parser -> bytes
(** [pending p] is a copy of incomplete data buffered so far.

    Excludes bytes inside an open bracketed paste (those stay hidden until the
    end marker). The returned buffer should be treated as immutable. Useful for
    diagnostics. *)

val reset : parser -> unit
(** [reset p] drops all buffered data, exits paste mode if active, and returns
    the tokenizer to its initial state. *)
