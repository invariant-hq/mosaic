(** Byte buffer writers for escape sequence emission.

    A writer wraps a {!Stdlib.Bytes.t} buffer and manages a write position.
    Writers are mutable and not thread-safe: use one writer per thread. *)

(** {1:writers Writers} *)

type t
(** The type for byte buffer writers. *)

val make : bytes -> t
(** [make buf] is a writer targeting [buf]. The buffer must be large enough to
    contain all generated output.

    Write operations raise [Invalid_argument] if they would exceed [buf]. *)

val make_counting : unit -> t
(** [make_counting ()] is a writer that tracks output length without writing any
    bytes. Useful for measuring escape sequence sizes before allocating a
    buffer. *)

(** {1:inspection Inspection} *)

val len : t -> int
(** [len w] is the number of bytes written so far. *)

val pos : t -> int
(** [pos w] is the current write position. Same as {!len}. *)

val reset_pos : t -> unit
(** [reset_pos w] resets the write position to zero. The underlying buffer is
    not cleared. *)

val slice : t -> bytes
(** [slice w] is a fresh copy of the bytes written so far. *)

(** {1:writing Writing} *)

val write_char : t -> char -> unit
(** [write_char w c] appends character [c]. In counting mode, increments the
    position without writing. *)

val write_string : t -> string -> unit
(** [write_string w s] appends string [s]. *)

val write_subbytes : t -> bytes -> int -> int -> unit
(** [write_subbytes w buf off len] appends [len] bytes from [buf] starting at
    offset [off].

    Raises [Invalid_argument] if the source slice is out of bounds or the
    destination writer has insufficient remaining capacity. *)
