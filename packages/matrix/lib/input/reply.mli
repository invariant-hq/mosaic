(** Terminal replies, the answers a terminal writes into its input to the
    program's queries.

    A terminal writes into its input both what the user does and its answers to
    the program's queries ("in response to commands from the host", VT510
    §4.4.2). A {e reply} is one such answer. Every reply is a control sequence
    or a control string, and {!Decoder} tells replies from keys by their form
    and by the replies the program is owed ({!Decoder.section-owed}).

    A {e typed} reply is one whose form a published reference fixes, decoded to
    its values. A sequence or string whose values fall outside that form is kept
    as it was sent ({!Csi}, {!Dcs}, {!Osc}). Sizes and positions saturate at
    [max_int], as {!Ansi.Parser.Params} reads every parameter.

    An OSC reply ends with the terminator of the query it answers. xterm "uses
    the same terminator used in a query" (xterm's Control Sequences, "Operating
    System Commands"), BEL or ST, and a program that sent BEL expects BEL back,
    so each OSC reply keeps its terminator. *)

(** The type for the settings a DEC mode report gives (DECRPM, xterm's Control
    Sequences, "Request DEC private mode"). *)
type setting =
  | Not_recognized  (** 0, the terminal does not recognize the mode. *)
  | Set  (** 1. *)
  | Reset  (** 2. *)
  | Permanently_set  (** 3, set and unchangeable. *)
  | Permanently_reset  (** 4, reset and unchangeable. *)

(** The type for replies. *)
type t =
  | Primary_attributes of int list
      (** DA1, [CSI ? Ps ; ... c], the terminal's class and features. It ends a
          batch of queries ({!Decoder.section-owed}). *)
  | Secondary_attributes of int list
      (** DA2, [CSI > Ps ; ... c], the terminal's type, version and options. *)
  | Unit_id of string
      (** DA3, [DCS ! | id ST], the unit's identity as the hex digits sent. *)
  | Mode of { dec : bool; mode : int; setting : setting }
      (** DECRPM, [CSI ? mode ; setting $ y] for a DEC private mode
          ([dec = true]) or [CSI mode ; setting $ y] for an ANSI mode. *)
  | Cursor of { row : int; col : int; page : int option }
      (** CPR, [CSI row ; col R], with no page, or DECXCPR,
          [CSI ? row ; col ; page R], with rows and columns from 0 and pages
          from 1 ("the cursor is currently at line Pl, column Pc, on page Pp",
          VT510, DECXCPR). xterm omits the page, "the default page, i.e., "1""
          (xterm's Control Sequences, "Device Status Report"). xterm sends a
          modified F3 as [CSI 1 ; m R], which a CPR can also be
          ({!Decoder.section-sequences}). *)
  | Operating_status of [ `Ok | `Malfunction ]
      (** DSR, [CSI 0 n] or [CSI 3 n], says the terminal "is in good operating
          condition" or "has a malfunction". It is the reply to [CSI 5 n]
          (VT510, DSR, Operating Status). *)
  | Text_area_pixels of { height : int; width : int }
      (** XTWINOPS, [CSI 4 ; height ; width t], the reply to [CSI 14 t]. *)
  | Cell_pixels of { height : int; width : int }
      (** XTWINOPS, [CSI 6 ; height ; width t], the reply to [CSI 16 t]. *)
  | Text_area of { rows : int; cols : int }
      (** XTWINOPS, [CSI 8 ; rows ; cols t], the reply to [CSI 18 t]. *)
  | Resize of { rows : int; cols : int; height : int; width : int }
      (** The in-band resize report, [CSI 48 ; rows ; cols ; height ; width t],
          sent on every resize while mode 2048 is set, with the text area in
          cells and in pixels. Sub-parameters are ignored, as its definition
          requires of a client ("If a client does not understand these, it MUST
          ignore subparameters"). *)
  | Keyboard_flags of int
      (** [CSI ? flags u], the kitty keyboard protocol's enhancement flags. *)
  | Color_scheme of [ `Dark | `Light ]
      (** [CSI ? 997 ; 1 n] (dark) or [CSI ? 997 ; 2 n] (light), the reply to
          [CSI ? 996 n] and the report a terminal sends on a change while mode
          2031 is set (contour, "Dark and Light Mode detection"). *)
  | Version of string
      (** XTVERSION, [DCS > | text ST], the terminal's name and version. *)
  | Termcap of { valid : bool; entries : (string * string option) list }
      (** XTGETTCAP, [DCS 1 + r name=value ; ... ST] ([valid]) or
          [DCS 0 + r name ST], with each capability's name and value decoded
          from hex. *)
  | Color of {
      target : [ `Foreground | `Background | `Cursor | `Entry of int ];
      rgb : int * int * int;
      terminator : Ansi.terminator;
    }
      (** [OSC 10], [OSC 11], [OSC 12] or [OSC 4 ; index], the OSC that would
          set that color of the palette ({!Ansi.Palette}), with a color in a
          form {!rgb_of_spec} reads. The OSCs that set a color take the same
          forms. A reply with a color in any other form is {!Osc}. *)
  | Clipboard of {
      selection : string;
      data : string;
      terminator : Ansi.terminator;
    }
      (** [OSC 52 ; selection ; data], a clipboard read, with its data decoded
          from base64. A reply whose data is not base64 is {!Osc}. *)
  | Graphics of { id : int option; message : string }
      (** [APC G keys ; message ST], the kitty graphics protocol's reply, with
          the image id of its [i] key and its message, such as ["OK"]. *)
  | Osc of { content : string; terminator : Ansi.terminator }
      (** Any other OSC, its content as sent. *)
  | Dcs of { header : Ansi.Parser.sequence; data : string }
      (** Any other DCS, such as DECRQSS's reply [DCS 1 $ r ... ST]. *)
  | Csi of Ansi.Parser.sequence
      (** Any other control sequence, such as one that no reference defines as
          input, or a key or report whose values fall outside the range its
          reference fixes. *)
  | Ss3 of Ansi.Parser.sequence
      (** An SS3 sequence that names no key, written as the control sequence its
          bytes would be after [CSI]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf r] formats [r] for debugging. *)

(** {1:strings Reading control strings}

    A control string's content is read to the reply its form says, by the rules
    of the constructors above. Content of any other form is kept as it is. *)

val of_osc : Ansi.terminator -> string -> t
(** [of_osc e s] is the reply that an OSC with content [s], ended by [e], is. It
    is {!Color} or {!Clipboard} when [s] has their form, and {!Osc} otherwise.
*)

val of_dcs : Ansi.Parser.sequence -> string -> t
(** [of_dcs h s] is the reply that a DCS with header [h] and data [s] is. It is
    {!Version}, {!Unit_id} or {!Termcap} when [h] and [s] have their form, and
    {!Dcs} otherwise. *)

val graphics : string -> t
(** [graphics s] is the {!Graphics} reply whose keys and message are [s], the
    content of an APC after its [G]. *)

val rgb_of_spec : string -> (int * int * int) option
(** [rgb_of_spec s] is the color the X11 color string [s] names (Xlib, ch. 6,
    "Color Strings"), each channel from 0 to 255:
    - [rgb:r/g/b], each channel of 1 to 4 hex digits, "scaled" to its range, so
      [rgb:f/8/0] is [(255, 136, 0)];
    - [#rgb], [#rrggbb], [#rrrgggbbb] or [#rrrrggggbbbb], the older form, whose
      digits "represent the most significant bits of the value", so [#f80] is
      [(240, 128, 0)];
    - a color name of X.Org's rgb.txt (rgb 1.1.0), such as [red] or
      [ghost white]. "Color strings are case-insensitive", and spaces do not
      count, as Xcms removes them before it looks a name up (libX11), so
      [GhostWhite] and [ghost white] are one color.

    It is [None] for any other string, such as a device-independent form like
    [CIEXYZ:]. *)

(** {1:encoding Encoding}

    {!encode} writes each reply in the form its constructor gives, as a terminal
    does, with 7-bit controls. CSI is [ESC \[] and ST is [ESC \\ ]. A color is
    written [rgb:rrrr/gggg/bbbb], each channel scaled to 16 bits as xterm writes
    it. Capability names and values are written in hex, and a clipboard's data
    in base64 with padding (RFC 4648 §4).

    For every reply [r] that {!Decoder} can produce, the decoder reads
    [encode [r]] back as [r]. A control string and a CPR of row 0, whose form a
    modified F3 shares, read back so only while they are owed
    ({!Decoder.expect}). *)

val encode : t list -> string
(** [encode rs] is the bytes a terminal sends for the replies [rs], in order,
    and [""] for none.

    Raises [Invalid_argument] if a reply holds a value its form cannot carry,
    which would read back as another reply. Such values are a negative number, a
    position of [max_int], a page below 1, a channel outside 0..255, a palette
    index or graphics id above 999,999,999, an empty capability name, a
    selection holding [;], or string content that is not UTF-8 or holds a
    control other than BS to CR. *)
