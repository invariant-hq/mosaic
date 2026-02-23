(** Streaming ANSI escape sequence parser.

    Converts byte streams into high-level {!token}s representing ANSI escape
    sequences, SGR attributes, and plain text. The parser handles partial input:
    incomplete escape sequences and UTF-8 multi-byte characters at chunk
    boundaries are buffered until the next {!feed} call.

    Parsing never raises on malformed input. Unrecognized sequences become
    {!Unknown} controls and invalid UTF-8 is replaced with U+FFFD. Maximum
    sequence lengths ([max_escape_length] for CSI, [max_osc_length] for OSC)
    prevent unbounded buffering. *)

(** {1:tokens Tokens} *)

(** The type for control sequences other than SGR. *)
type control =
  | CUU of int  (** Cursor Up by [n] lines. *)
  | CUD of int  (** Cursor Down by [n] lines. *)
  | CUF of int  (** Cursor Forward by [n] columns. *)
  | CUB of int  (** Cursor Backward by [n] columns. *)
  | CNL of int  (** Cursor Next Line (down [n], column 1). *)
  | CPL of int  (** Cursor Previous Line (up [n], column 1). *)
  | CHA of int  (** Cursor Horizontal Absolute to column [n]. *)
  | VPA of int  (** Vertical Position Absolute to row [n]. *)
  | CUP of int * int  (** Cursor Position to [(row, col)]. *)
  | ED of int  (** Erase in Display. *)
  | EL of int  (** Erase in Line. *)
  | IL of int  (** Insert [n] blank Lines. *)
  | DL of int  (** Delete [n] Lines. *)
  | DCH of int  (** Delete [n] Characters. *)
  | ICH of int  (** Insert [n] blank Characters. *)
  | OSC of int * string  (** Generic OSC with code and payload. *)
  | Hyperlink of ((string * string) list * string) option
      (** OSC 8 hyperlink. [None] closes the current hyperlink.
          [Some (params, url)] opens one, where [params] are key=value pairs
          from the parameter string. *)
  | Reset  (** RIS — reset to initial state ([ESC c]). *)
  | DECSC  (** Save cursor position ([ESC 7]). *)
  | DECRC  (** Restore cursor position ([ESC 8]). *)
  | Unknown of string  (** Unrecognized sequence, preserved as raw bytes. *)

type sgr_attr =
  [ `Reset
  | `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Inverse
  | `Hidden
  | `Strikethrough
  | `Overline
  | `Framed
  | `Encircled
  | `No_bold
  | `No_dim
  | `No_italic
  | `No_underline
  | `No_blink
  | `No_inverse
  | `No_hidden
  | `No_strikethrough
  | `No_overline
  | `No_framed
  | `No_encircled
  | `Fg of Color.t
  | `Bg of Color.t ]
(** The type for SGR (Select Graphic Rendition) attributes. Represents
    individual style changes from a single SGR sequence. A sequence like
    [ESC \[ 1 ; 31 m] produces [[`Bold; `Fg Red]]. *)

(** The type for parsed tokens. *)
type token =
  | Text of string
      (** Plain UTF-8 text between escape sequences. Always contains complete
          characters. *)
  | SGR of sgr_attr list
      (** SGR command. An empty list is equivalent to [[`Reset]]. *)
  | Control of control  (** Non-SGR control sequence. *)

(** {1:parsers Parsers} *)

type t
(** The type for parsers. Mutable and not thread-safe. *)

val create : unit -> t
(** [create ()] is a fresh parser in the default state. *)

val reset : t -> unit
(** [reset p] clears internal buffers and returns [p] to the default state.
    Discards buffered partial sequences. *)

(** {1:feeding Feeding} *)

val feed : t -> bytes -> off:int -> len:int -> (token -> unit) -> unit
(** [feed p buf ~off ~len f] processes [len] bytes from [buf] starting at [off],
    calling [f] for each complete token. Incomplete sequences are buffered until
    the next call. *)

val parse : string -> token list
(** [parse s] parses a complete string into tokens. Creates a temporary parser.
    Not suitable for streaming; use {!feed}. *)

(** {1:inspection Inspection} *)

val has_pending : t -> bool
(** [has_pending p] is [true] iff [p] has buffered data (incomplete escape
    sequences or pending UTF-8 bytes). *)

val pending : t -> bytes
(** [pending p] is a copy of raw input bytes not yet consumed. Escape sequence
    bodies being accumulated (CSI parameters, OSC payloads) are stored in
    separate internal buffers and are not included. Use {!has_pending} to avoid
    allocation when only checking for pending data. *)
