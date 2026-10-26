(** Control functions in a UTF-8 stream (ECMA-48, ECMA-35), read into tokens.

    A terminal receives characters. {e Graphic characters} are shown, and the
    coded forms of {e control functions} are performed. ECMA-48 §5 codes a
    control function in five ways:
    - a {e C0 control}, one character U+0000..U+001F, such as LF;
    - a {e C1 control} written ESC Fe, which is ESC then a byte 0x40..0x5F, as
      ESC D is IND;
    - an {e escape sequence} ESC I.. F, made of intermediate bytes 0x20..0x2F
      and a final byte 0x30..0x7E, as in ESC ( 0. The intermediates and the
      final byte name the function (ECMA-35 §13.1);
    - a {e control sequence} CSI P.. I.. F, where CSI is [ESC \[], followed by
      parameter bytes 0x30..0x3F, intermediates and a final byte 0x40..0x7E, as
      in [CSI 1 ; 31 m];
    - a {e control string}, made of an opening (DCS, OSC, APC, PM or SOS,
      written [ESC P], [ESC \]], [ESC _], [ESC ^] and [ESC X]), its content, and
      ST, written [ESC \\ ] (ECMA-48 §5.6). xterm also ends an OSC with BEL.

    A parser ({!type-t}) reads the bytes of a stream and reports what they code
    as {e tokens} ({!type-token}). The bytes are UTF-8, decoded first. Each
    ill-formed subsequence, such as a lone byte 0x80..0xFF, is one U+FFFD
    (Unicode §3.9, maximal subparts). The code points U+0080..U+009F are never
    controls, since C1 controls are single bytes only in the 8-bit code, and
    xterm, whose protocol programs target, states that "it is not possible to
    use a C1 control obtained from decoding the UTF-8 text". They have no glyph
    either, so they are dropped wherever they occur. Performing them would let
    mis-encoded text swallow the output that follows it, as Windows-1252 "Ÿ"
    read as Latin-1 is U+009F, APC.

    Bytes may be fed in chunks split anywhere. Whatever the split, the tokens
    are the same once adjacent {!Text} tokens, and adjacent {!Data} tokens, are
    joined, and neither is ever cut inside a character. The parser holds the
    sequence in progress and keeps neither text nor string content.

    The life cycle of a parser:
    + {!create} makes a parser at the start of a stream.
    + {!feed} reads each chunk as it arrives and reports its tokens.
    + While the stream is silent, {!at_boundary} tells whether the bytes fed end
      inside something that later bytes complete. A key decoder that has
      received a lone ESC cannot tell the Escape key from the start of a
      sequence, so it waits for more bytes while [at_boundary] is [false].
    + {!finish} ends the stream, once the reader decides that it has stopped,
      and leaves the parser at the start of a new one.

    A parser is mutable and must not be used by two domains in parallel.

    This reads a bold word:
    {[
    Ansi.Parser.parse "\027[1mhi"
    (* [ Csi { marker = None; params; intermediates = ""; final = 'm' };
           Text "hi" ], where [params] holds the one parameter 1 *)
    ]} *)

(** {1:recovery What every character does in every state}

    + ESC, CAN and SUB are never part of anything. Each ends the sequence or
      string in progress (VT100 User Guide ch. 3, VT510 §4.3.5), so one ESC
      always brings the parser back. CAN and SUB are then reported as
      {!Control}, and ESC starts a new escape sequence.
    + A C0 control inside an escape or control sequence is performed where it
      occurs, and the sequence continues, so CSI 2 LF C is LF, then CSI 2 C
      (VT100 User Guide ch. 3, "Embedded control characters are executed as soon
      as they are encountered"). Inside a string a C0 control is never
      performed. BS..CR are content (ECMA-48 §5.6), BEL ends an OSC, and the
      others are dropped. Inside a DCS header every C0 control is dropped and
      the header goes on, so [ESC P 1 LF 2 q] is [DCS 12 q].
    + DEL is dropped inside sequences and strings. Elsewhere it is reported,
      since only the device knows whether a 96-character set makes it a graphic
      character.
    + A sequence is {e malformed} when it holds a byte that may appear in a
      sequence but not where it occurs (a private marker after a parameter, a
      parameter after an intermediate), a fifth intermediate, or a 257th
      parameter field. It is consumed to its final byte, or to the end of the
      string for a DCS, and none of it takes effect.
    + A non-ASCII character ends an escape or control sequence in progress,
      which has no effect, and is dropped, as xterm does. In a DCS header it
      makes the DCS malformed, because the content of a string never becomes
      text. *)

(** {1:meaning Syntax and meaning}

    Tokens report how a function was written. The device that receives it
    decides what it does and what its parameters default to. ECMA-48 §6 makes
    HPR the same as CUF only in a device "without a data component", and xterm
    reads CSI s as SCOSC or DECSLRM depending on a mode. The device matches
    [(marker, intermediates, final)]. *)

(** {1:params Parameters}

    The parameters of a control sequence or DCS header (ECMA-48 §5.4.2) are
    separated by [;]. Each has one or more {e fields} separated by [:], the
    sub-parameters SGR 38, 48 and 58 use. A field is absent, as both are in
    [CSI ; H], or a non-negative integer. An integer too large for [int]
    saturates at [max_int], so a program must clamp a value to the distance it
    can act on before adding it to anything.

    ECMA-48 §5.4.3 sets no maximum count. A parser that holds bounded memory
    must still bound the fields it keeps, because a sequence's meaning depends
    on its final byte, which comes last. The bound is 256 fields, above the
    number of distinct values any variadic function assigns, and a sequence with
    more is malformed. *)

module Params : sig
  type t
  (** The type for the parameters of a control sequence or DCS header. *)

  val count : t -> int
  (** [count p] is the number of parameters, such as 0 for [CSI m] and 2 for
      [CSI ; m]. *)

  val pn : t -> int -> default:int -> int
  (** [pn p i ~default] reads parameter [i] as a count or a position (ECMA-48
      §8.1, Pn). Absent, 0 or [i >= count p] means [default]. DEC terminals give
      0 the default's meaning for these parameters (VT510 §4.3.3, "A 0 value or
      omitted parameter indicates a default value"), as does ECMA-48's ZERO
      DEFAULT MODE for the functions it lists (F.4.2). Reads the first field. *)

  val ps : t -> int -> default:int -> int
  (** [ps p i ~default] reads parameter [i] as a selector (ECMA-48 §8.1, Ps).
      Absent or [i >= count p] means [default], and 0 is the value 0, as in ED 0
      or SGR 0. Reads the first field. *)

  val value : t -> int -> int option
  (** [value p i] is the first field of parameter [i], [None] if it is absent or
      [i >= count p]. For functions that skip absent parameters, such as SM and
      RM. *)

  val fields : t -> int -> int
  (** [fields p i] is the number of fields of parameter [i], such as 1 for [5]
      and 3 for [5:1:2], and 0 if [i >= count p]. *)

  val field : t -> int -> int -> int option
  (** [field p i j] is field [j] of parameter [i], [None] if it is absent or out
      of range. Field 0 is the one {!value} reads. *)
end

(** {1:tokens Tokens} *)

type sequence = {
  marker : char option;
      (** The private marker [<], [=], [>] or [?], written as the first
          parameter byte (ECMA-48 §5.4.1 b), such as [Some '?'] in [CSI ? 25 h].
      *)
  params : Params.t;
  intermediates : string;  (** Bytes 0x20..0x2F, at most 4. *)
  final : char;
      (** A byte 0x40..0x7E. With [marker] and [intermediates] it names the
          function. *)
}
(** The type for the header of a control sequence or of a DCS. *)

(** The type for the kinds of control strings. *)
type opening =
  | Dcs of sequence  (** DEVICE CONTROL STRING, with its header. *)
  | Osc  (** OPERATING SYSTEM COMMAND. *)
  | Apc  (** APPLICATION PROGRAM COMMAND. *)
  | Pm  (** PRIVACY MESSAGE. *)
  | Sos  (** START OF STRING. *)

(** The type for tokens. *)
type token =
  | Text of string
      (** Graphic characters, valid UTF-8 without C0, DEL or U+0080..U+009F. *)
  | Control of char
      (** A C0 control other than ESC, DEL outside sequences and strings, or a
          C1 control 0x80..0x9F written ESC Fe, as ESC D is [Control '\x84'],
          IND. CSI, DCS, OSC, APC, PM, SOS and ST are never reported this way,
          and a lone ST is dropped. *)
  | Esc of { intermediates : string; final : char }
      (** An escape sequence other than ESC Fe, such as [ESC 7], [ESC ( B],
          [ESC ( % 5], [ESC # 8] or [ESC SP F]. ECMA-35 allows any number of
          intermediates (§13.2.2). A sequence with more than 4 is malformed, and
          no device acts on one. A DRCS designation, such as [ESC $ ( SP ! @],
          has 4 (§14.4.2). *)
  | Csi of sequence  (** A control sequence. *)
  | Open of opening
      (** The start of a control string. Its content follows as {!Data}, then
          exactly one {!Close}, with no other token in between. *)
  | Data of string
      (** Content of the open string, valid UTF-8 made of ECMA-48 §5.6's
          command-string characters (BS..CR and 0x20..0x7E) and non-ASCII
          characters other than U+0080..U+009F. *)
  | Close of [ `St | `Bel | `Aborted ]
      (** The end of the open string. [`St] ends any string, and [`Bel] only an
          OSC. xterm replies to an OSC with the terminator of its query.
          [`Aborted] is a string ended by CAN, SUB, an ESC that does not begin
          ST, or the end of the input. Its content is in error (ECMA-48 §8.3.6,
          CAN: "this data shall be ignored"), so a program must not act on what
          it has not already acted on. *)

val pp_token : Format.formatter -> token -> unit
(** [pp_token ppf t] formats [t] in ECMA-48's notation, such as [CSI ?25 h],
    [CSI 1;5 H], [CSI 38:2::1:2:3 m], [CSI 1 SP @], [ESC ( B], [LF], [IND],
    [DCS $ q], [OSC], [ST], [BEL] or [Aborted]. Text and data are quoted
    strings. *)

(** {1:parsers Parsers} *)

type t
(** The type for parsers. *)

val create : unit -> t
(** [create ()] is a parser at the start of a stream. *)

val reset : t -> unit
(** [reset p] returns [p] to the start of a stream. It drops the character,
    sequence or string in progress without reporting it. *)

val feed : t -> bytes -> off:int -> len:int -> (token -> unit) -> int
(** [feed p b ~off ~len f] reads the [len] bytes of [b] from [off], calls [f] on
    each token, in order, and is the number of bytes it read. It reads them all
    unless [f] calls {!stop}, and then reads no byte after the one that ended
    the token [f] was given. A byte that ends one token and begins another, such
    as CAN in a string, gives both. A character, sequence or string unfinished
    at the end of the bytes read continues with the next call.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

val stop : t -> unit
(** [stop p], called by the function given to {!feed}, makes that feed return
    after the token it is reporting. Called elsewhere, it does nothing. *)

val finish : t -> (token -> unit) -> unit
(** [finish p f] ends the stream. It calls [f] on the tokens the end of the
    input makes of the unfinished parts, in order. An unfinished character is
    U+FFFD, an open string is closed [`Aborted], and an unfinished sequence has
    no effect. [p] is then at the start of a stream, as after {!reset}. *)

val at_boundary : t -> bool
(** [at_boundary p] is [true] iff no character, sequence or string is in
    progress, so that every byte fed so far is reported in a token or dropped.
    The next byte then starts afresh, whatever it is, and {!finish} would report
    nothing. Otherwise the bytes fed end inside something that only later bytes
    complete, such as a UTF-8 character cut short, an ESC or a control sequence
    without its final byte, or a control string without its terminator.

    A program that hands the stream to another reader, or cuts a recording into
    pieces, does so at a boundary, where no token straddles the cut.
    [at_boundary] changes nothing in [p]. *)

val parse : string -> token list
(** [parse s] is the tokens {!feed} and {!finish} give for the complete stream
    [s]. *)
