(** Terminal input events.

    This module defines the data model for terminal input: keyboard keys and
    modifiers, mouse buttons and motion, terminal responses, and the unified
    high-level event type {!t} for user input.

    The types live in a separate module so components that only need the data
    model (e.g. the terminal runtime) can depend on them without pulling in the
    parser. *)

(** {1:modifiers Modifiers} *)

(** Modifier key state shared by keyboard and mouse events. *)
module Modifier : sig
  type t = {
    ctrl : bool;  (** Control key held. *)
    alt : bool;  (** Alt/Option key held. *)
    shift : bool;  (** Shift key held. *)
    super : bool;  (** Super/Windows/Command key held. *)
    hyper : bool;  (** Hyper modifier key held. *)
    meta : bool;  (** Meta modifier key held. *)
    caps_lock : bool;  (** Caps Lock toggle is active. *)
    num_lock : bool;  (** Num Lock toggle is active. *)
  }
  (** The type for modifier state.

      Lock fields ([caps_lock], [num_lock]) indicate toggle state, not whether
      the physical key is currently pressed. *)

  val none : t
  (** [none] is a modifier state with all fields set to [false]. Useful as a
      base value: [{none with ctrl = true}]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff all fields of [a] and [b] are equal. *)

  val ctrl : t -> bool
  (** [ctrl m] is [m.ctrl]. *)

  val alt : t -> bool
  (** [alt m] is [m.alt]. *)

  val shift : t -> bool
  (** [shift m] is [m.shift]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats modifier sets for debugging. *)
end

(** {1:keys Keys} *)

(** Keyboard keys.

    Most keys map to dedicated constructors. {!Char} handles all Unicode
    characters including control codes. Keypad keys ([KP_*]) are reported when
    the terminal sends distinct codes for keypad versus main keyboard.
    {!Unknown} captures unrecognized Kitty protocol key codes for forward
    compatibility. *)
module Key : sig
  (** {1:keys Keys} *)

  (** The type for keyboard keys. *)
  type t =
    | Char of Uchar.t
        (** Unicode character, including control characters (e.g.
            [Uchar.of_int 0x03] for Ctrl+C). *)
    | Enter
    | Line_feed
    | Tab
    | Backspace
    | Delete
    | Escape
    | Up
    | Down
    | Left
    | Right
    | Home
    | End
    | Page_up
    | Page_down
    | Insert
    | F of int
        (** Function key. Values outside \[1;35\] may appear from the Kitty
            protocol. *)
    | Print_screen
    | Pause
    | Menu
    | Scroll_lock
    | Media_play
    | Media_pause
    | Media_play_pause
    | Media_stop
    | Media_reverse
    | Media_fast_forward
    | Media_rewind
    | Media_next
    | Media_prev
    | Media_record
    | Volume_up
    | Volume_down
    | Volume_mute
    | Shift_left
    | Shift_right
    | Ctrl_left
    | Ctrl_right
    | Alt_left
    | Alt_right
    | Super_left
    | Super_right
    | Hyper_left
    | Hyper_right
    | Meta_left
    | Meta_right
    | Iso_level3_shift
    | Iso_level5_shift
    | Caps_lock
    | Num_lock
    | KP_0
        (** Keypad keys. Reported when the terminal sends distinct codes for
            keypad versus main keyboard. *)
    | KP_1
    | KP_2
    | KP_3
    | KP_4
    | KP_5
    | KP_6
    | KP_7
    | KP_8
    | KP_9
    | KP_decimal
    | KP_divide
    | KP_multiply
    | KP_subtract
    | KP_add
    | KP_enter
    | KP_equal
    | KP_separator
    | KP_begin
    | KP_left
    | KP_right
    | KP_up
    | KP_down
    | KP_page_up
    | KP_page_down
    | KP_home
    | KP_end
    | KP_insert
    | KP_delete
    | Unknown of int
        (** Unknown key code from Private Use Area or unmapped sequences. The
            [int] is the Kitty protocol key code. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] denote the same key.

      For {!Char} variants, compares {!Uchar.t} values by code point. Unicode
      normalization is {e not} performed: U+00E9 and U+0065 U+0301 are distinct.
  *)

  val is_char : t -> bool
  (** [is_char k] is [true] iff [k] is [Char _]. *)

  val is_enter : t -> bool
  (** [is_enter k] is [true] iff [k] is [Enter]. *)

  val is_arrow : t -> bool
  (** [is_arrow k] is [true] iff [k] is [Up], [Down], [Left] or [Right]. *)

  val is_function : t -> bool
  (** [is_function k] is [true] iff [k] is [F _]. *)

  val is_ctrl_char : t -> bool
  (** [is_ctrl_char k] is [true] iff [k] is a [Char] in the ASCII control range
      U+0000..U+001F or U+007F (DEL). *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats keys for debugging. *)

  (** {1:event_types Event types} *)

  type event_type =
    | Press
    | Repeat
    | Release
        (** The type for key event types.

            Only available when the terminal supports the Kitty keyboard
            protocol. Legacy terminals always report {!Press}. *)

  (** {1:events Key events} *)

  type event = {
    key : t;  (** The key. *)
    modifier : Modifier.t;  (** Active modifiers. *)
    event_type : event_type;  (** Press, repeat or release. *)
    associated_text : string;
        (** UTF-8 text to insert. Populated by the Kitty protocol for
            text-producing keys and by the parser for legacy text bytes. Empty
            for non-text keys or when not provided. *)
    shifted_key : Uchar.t option;
        (** Key with Shift applied (Kitty protocol only). [None] on legacy
            terminals. *)
    base_key : Uchar.t option;
        (** Key without modifiers (Kitty protocol only). [None] on legacy
            terminals. *)
  }
  (** The type for key events. On legacy terminals [event_type] is always
      {!Press}, and [shifted_key] and [base_key] are [None]. *)

  val make :
    ?modifier:Modifier.t ->
    ?event_type:event_type ->
    ?associated_text:string ->
    ?shifted_key:Uchar.t ->
    ?base_key:Uchar.t ->
    t ->
    event
  (** [make key] is a key event for [key] with:
      - [modifier] defaults to {!Modifier.none}.
      - [event_type] defaults to {!Press}.
      - [associated_text] defaults to [""].
      - [shifted_key] defaults to [None].
      - [base_key] defaults to [None]. *)

  val of_char :
    ?modifier:Modifier.t ->
    ?event_type:event_type ->
    ?associated_text:string ->
    ?shifted_key:Uchar.t ->
    ?base_key:Uchar.t ->
    char ->
    event
  (** [of_char c] is like {!make} with [Char (Uchar.of_char c)].

      When [associated_text] is not provided it defaults to a single-character
      string for [c]. Uses shared strings for ASCII characters to reduce
      allocations. *)

  (** {1:event_preds Event predicates and comparisons} *)

  val equal_event : event -> event -> bool
  (** [equal_event a b] is [true] iff all fields of [a] and [b] are structurally
      equal. *)

  val pp_event : Format.formatter -> event -> unit
  (** [pp_event] formats key events for debugging. *)
end

(** {1:mouse Mouse} *)

(** Mouse events. *)
module Mouse : sig
  (** {1:buttons Buttons} *)

  (** The type for mouse buttons. *)
  type button =
    | Left
    | Middle
    | Right
    | Button of int
        (** Extended button. Numbering follows the terminal protocol after the
            primary buttons. *)

  val equal_button : button -> button -> bool
  (** [equal_button a b] is [true] iff [a] and [b] are the same button. *)

  val pp_button : Format.formatter -> button -> unit
  (** [pp_button] formats mouse buttons for debugging. *)

  (** {1:scroll Scroll direction} *)

  type scroll_direction =
    | Scroll_up
    | Scroll_down
    | Scroll_left
    | Scroll_right  (** The type for normalized scroll direction. *)

  val equal_scroll_direction : scroll_direction -> scroll_direction -> bool
  (** [equal_scroll_direction a b] is [true] iff [a] and [b] are the same
      direction. *)

  val pp_scroll_direction : Format.formatter -> scroll_direction -> unit
  (** [pp_scroll_direction] formats scroll directions for debugging. *)

  (** {1:events Events} *)

  (** The type for mouse event kinds. Coordinates and modifiers live on the
      enclosing {!event} record. *)
  type kind =
    | Down of { button : button }  (** Button pressed. *)
    | Up of { button : button option }
        (** Button released. [button] is [None] for legacy protocols that do not
            identify the released button. *)
    | Move  (** Pointer moved with no tracked button pressed. *)
    | Drag of { button : button }  (** Pointer moved with [button] pressed. *)
    | Scroll of { direction : scroll_direction; delta : int }
        (** Scroll-wheel event. [delta] is the number of steps in [direction].
        *)

  val equal_kind : kind -> kind -> bool
  (** [equal_kind a b] is [true] iff [a] and [b] are structurally equal. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind] formats mouse event kinds for debugging. *)

  type event = {
    x : int;  (** Horizontal coordinate, 0-based. *)
    y : int;  (** Vertical coordinate, 0-based. *)
    modifiers : Modifier.t;  (** Active modifiers. *)
    kind : kind;  (** Button, motion, or scroll payload. *)
  }
  (** The type for mouse events. Coordinates are 0-based with top-left origin.
  *)

  val make : x:int -> y:int -> modifiers:Modifier.t -> kind -> event
  (** [make ~x ~y ~modifiers kind] is a mouse event. *)

  val equal_event : event -> event -> bool
  (** [equal_event a b] is [true] iff [a] and [b] are structurally equal. *)

  val pp_event : Format.formatter -> event -> unit
  (** [pp_event] formats mouse events for debugging. *)
end

(** {1:responses Terminal responses} *)

(** Terminal protocol responses.

    Responses are terminal replies to control sequence queries or operating
    system commands. They arrive asynchronously and are routed to a separate
    callback by {!Parser}; they never appear in the user-facing event stream. *)
module Response : sig
  (** {1:mode_reports Mode reports} *)

  type mode_report = { is_private : bool; modes : (int * int) list }
  (** The type for DEC private mode status responses (DECRPM). [modes] holds
      [(mode, value)] pairs following the DEC convention: 0 or 3 for disabled, 1
      or 2 for enabled. *)

  val equal_mode_report : mode_report -> mode_report -> bool
  (** [equal_mode_report a b] is [true] iff [a] and [b] are structurally equal.
  *)

  val pp_mode_report : Format.formatter -> mode_report -> unit
  (** [pp_mode_report] formats mode reports for debugging. *)

  (** {1:capabilities Capability responses} *)

  (** The type for interpreted terminal capability responses. *)
  type capability =
    | Device_attributes of int list
        (** Device Attributes (DA/DA2/DA3) response payload. *)
    | Mode_report of mode_report  (** DEC mode status report (DECRPM). *)
    | Pixel_resolution of int * int
        (** Terminal pixel dimensions [(width_px, height_px)] from
            [CSI 4 ; height ; width t]. *)
    | Cursor_position of int * int
        (** Cursor position [(row, col)], 1-based. Row 1 is the top line, column
            1 is the leftmost column. *)
    | Xtversion of string  (** XTerm [XTVERSION] response (DCS > | ... ST). *)
    | Kitty_graphics_reply of string
        (** Kitty graphics response (APC G ... ST). *)
    | Kitty_keyboard of { level : int; flags : int option }
        (** Kitty keyboard protocol query response [CSI ? level [; flags] u].
            [level] is non-zero when the protocol is active. [flags] is the
            optional terminal-reported bitfield. *)
    | Color_scheme of [ `Dark | `Light | `Unknown of int ]
        (** Color scheme DSR response [CSI ? 997 ; value n], the reply to the
            [CSI ? 996 n] query. Value 1 is dark, 2 is light. *)

  val equal_capability : capability -> capability -> bool
  (** [equal_capability a b] is [true] iff [a] and [b] are structurally equal.
  *)

  val pp_capability : Format.formatter -> capability -> unit
  (** [pp_capability] formats capability responses for debugging. *)

  (** {1:responses Responses} *)

  (** The type for terminal responses. *)
  type t =
    | Capability of capability  (** Interpreted terminal capability response. *)
    | Clipboard of string * string
        (** [Clipboard (selection, data)] is an OSC 52 clipboard response.
            [data] is base64-decoded when possible, verbatim otherwise. *)
    | Osc of int * string
        (** [Osc (number, payload)] is an unhandled OSC sequence. [payload] is
            the raw text between introducer and terminator with no sanitization.
        *)
    | Unknown of string
        (** [Unknown bytes] is an unrecognized terminal protocol sequence.
            [bytes] is preserved verbatim. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are structurally equal. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats responses for debugging. *)
end

(** {1:events Events} *)

(** The type for terminal input events. *)
type t =
  | Key of Key.event  (** Keyboard input. *)
  | Mouse of Mouse.event
      (** Mouse button, motion, or scroll-wheel input. Coordinates are 0-based.
      *)
  | Resize of int * int  (** [Resize (width, height)] is a terminal resize. *)
  | Focus  (** Terminal gained focus. *)
  | Blur  (** Terminal lost focus. *)
  | Paste of string
      (** [Paste text] is bracketed paste content preserved exactly. Empty
          payloads are dropped by the parser. *)

(** {1:preds Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff all fields of [a] and [b] are structurally equal.
    Key events compare [event_type], [associated_text], [shifted_key], and
    [base_key] as well as the key and modifiers. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats events for debugging. *)

(** {1:constructors Constructors} *)

val key :
  ?modifier:Modifier.t ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  t
(** [key k] is [Key (Key.make k)] with the given optional arguments. See
    {!Key.make} for defaults. *)

val char :
  ?modifier:Modifier.t ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  char ->
  t
(** [char c] is [Key (Key.of_char c)] with the given optional arguments. See
    {!Key.of_char} for defaults. *)

val key_event :
  ?modifier:Modifier.t ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [key_event k] is {!Key.make}[ k]. Alias useful when you need the raw
    {!Key.event} without wrapping in {!t}. *)

val char_event :
  ?modifier:Modifier.t ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  char ->
  Key.event
(** [char_event c] is {!Key.of_char}[ c]. *)

val press :
  ?modifier:Modifier.t ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [press k] is {!Key.make}[ ~event_type:Press k]. *)

val repeat :
  ?modifier:Modifier.t ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [repeat k] is {!Key.make}[ ~event_type:Repeat k]. *)

val release :
  ?modifier:Modifier.t ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [release k] is {!Key.make}[ ~event_type:Release k]. *)

val mouse_press : ?modifiers:Modifier.t -> int -> int -> Mouse.button -> t
(** [mouse_press x y button] is a mouse button press. [modifiers] defaults to
    {!Modifier.none}. *)

val mouse_release :
  ?modifiers:Modifier.t -> int -> int -> Mouse.button option -> t
(** [mouse_release x y button] is a mouse button release. [button] is [None]
    when the terminal protocol does not identify the released button.
    [modifiers] defaults to {!Modifier.none}. *)

val mouse_move : ?modifiers:Modifier.t -> int -> int -> t
(** [mouse_move x y] is pointer motion with no tracked button pressed.
    [modifiers] defaults to {!Modifier.none}. *)

val mouse_drag : ?modifiers:Modifier.t -> int -> int -> Mouse.button -> t
(** [mouse_drag x y button] is pointer motion with [button] pressed. [modifiers]
    defaults to {!Modifier.none}. *)

val mouse_scroll :
  ?modifiers:Modifier.t ->
  ?delta:int ->
  int ->
  int ->
  Mouse.scroll_direction ->
  t
(** [mouse_scroll x y direction] is a scroll-wheel event. [delta] defaults to
    [1] and [modifiers] defaults to {!Modifier.none}. *)

(** {1:helpers Helpers} *)

val match_ctrl_char : t -> char option
(** [match_ctrl_char e] is [Some c] when [e] is a {!Key} event with
    [modifier.ctrl = true] and the key is an ASCII [Char] (code point < 0x80).
    [None] otherwise. *)

val is_scroll : t -> bool
(** [is_scroll e] is [true] iff [e] is a mouse scroll event. *)

val is_drag : t -> bool
(** [is_drag e] is [true] iff [e] is a mouse drag event. *)
