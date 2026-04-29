(** Terminal input events.

    This module defines the data model for terminal input: keyboard keys and
    modifiers, mouse buttons and motion, terminal capability responses, and the
    unified high-level event type {!t} that combines them all.

    The types live in a separate module so components that only need the data
    model (e.g. the terminal runtime) can depend on them without pulling in the
    parser. *)

(** {1:keys Keys and modifiers} *)

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

  (** {1:modifiers Modifiers} *)

  type modifier = {
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

  val no_modifier : modifier
  (** [no_modifier] is a modifier with all fields set to [false]. Useful as a
      base value: [{no_modifier with ctrl = true}]. *)

  val equal_modifier : modifier -> modifier -> bool
  (** [equal_modifier a b] is [true] iff all fields of [a] and [b] are equal. *)

  val ctrl : modifier -> bool
  (** [ctrl m] is [m.ctrl]. *)

  val alt : modifier -> bool
  (** [alt m] is [m.alt]. *)

  val shift : modifier -> bool
  (** [shift m] is [m.shift]. *)

  val pp_modifier : Format.formatter -> modifier -> unit
  (** [pp_modifier] formats modifier sets for debugging. *)

  (** {1:events Key events} *)

  type event = {
    key : t;  (** The key. *)
    modifier : modifier;  (** Active modifiers. *)
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
    ?modifier:modifier ->
    ?event_type:event_type ->
    ?associated_text:string ->
    ?shifted_key:Uchar.t ->
    ?base_key:Uchar.t ->
    t ->
    event
  (** [make key] is a key event for [key] with:
      - [modifier] defaults to {!no_modifier}.
      - [event_type] defaults to {!Press}.
      - [associated_text] defaults to [""].
      - [shifted_key] defaults to [None].
      - [base_key] defaults to [None]. *)

  val of_char :
    ?modifier:modifier ->
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

(** Mouse buttons and motion events. *)
module Mouse : sig
  (** {1:buttons Buttons} *)

  (** The type for mouse buttons. *)
  type button =
    | Left
    | Middle
    | Right
    | Wheel_up
    | Wheel_down
    | Wheel_left
    | Wheel_right
    | Button of int
        (** Extended button (4+). In legacy tracking modes (X10/Normal, URXVT)
            release events do not encode which button was released; these are
            reported as [Button 0]. *)

  val equal_button : button -> button -> bool
  (** [equal_button a b] is [true] iff [a] and [b] are the same button. *)

  val pp_button : Format.formatter -> button -> unit
  (** [pp_button] formats mouse buttons for debugging. *)

  (** {1:button_state Button state} *)

  type button_state = { left : bool; middle : bool; right : bool }
  (** The type for primary button state during motion events. *)

  val equal_button_state : button_state -> button_state -> bool
  (** [equal_button_state a b] is [true] iff all fields are equal. *)

  val pp_button_state : Format.formatter -> button_state -> unit
  (** [pp_button_state] formats button state for debugging. *)

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

  (** {1:events Mouse events} *)

  (** The type for mouse events. Coordinates are 0-based with top-left origin.
  *)
  type event =
    | Button_press of int * int * button * Key.modifier
        (** [Button_press (x, y, button, mods)] is a button press at [(x, y)].
            Coordinates are 0-based, top-left origin. *)
    | Button_release of int * int * button * Key.modifier
        (** [Button_release (x, y, button, mods)] is a button release at
            [(x, y)]. In legacy tracking modes (X10/Normal, URXVT) [button] is
            [Button 0] because the protocol does not encode which button was
            released. *)
    | Motion of int * int * button_state * Key.modifier
        (** [Motion (x, y, state, mods)] is mouse motion to [(x, y)] with
            primary button [state] and [mods]. *)

  val equal_event : event -> event -> bool
  (** [equal_event a b] is [true] iff [a] and [b] are structurally equal. *)

  val pp_event : Format.formatter -> event -> unit
  (** [pp_event] formats mouse events for debugging. *)
end

(** {1:caps Terminal capabilities} *)

(** Terminal capability responses.

    Capability events are terminal responses to control sequence queries. They
    arrive asynchronously and are routed to a separate callback by {!Parser};
    they never appear in the user-facing event stream. *)
module Caps : sig
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

  (** {1:events Capability events} *)

  (** The type for terminal capability events. *)
  type event =
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

  val equal_event : event -> event -> bool
  (** [equal_event a b] is [true] iff [a] and [b] are structurally equal. *)

  val pp_event : Format.formatter -> event -> unit
  (** [pp_event] formats capability events for debugging. *)
end

(** {1:events Events} *)

(** The type for terminal input events. *)
type t =
  | Key of Key.event  (** Keyboard input. *)
  | Mouse of Mouse.event  (** Mouse button or motion. *)
  | Scroll of int * int * Mouse.scroll_direction * int * Key.modifier
      (** [Scroll (x, y, dir, delta, mods)] is a scroll wheel event at [(x, y)]
          with direction [dir], step [delta] (usually 1), and modifiers [mods].
          Coordinates are 0-based. Normalizes wheel actions across terminal
          protocols (SGR, URXVT, X10) into a single event. *)
  | Resize of int * int  (** [Resize (width, height)] is a terminal resize. *)
  | Focus  (** Terminal gained focus. *)
  | Blur  (** Terminal lost focus. *)
  | Paste of string
      (** [Paste text] is bracketed paste content preserved exactly. Empty
          payloads are dropped by the parser. *)
  | Clipboard of string * string
      (** [Clipboard (selection, data)] is an OSC 52 clipboard response. [data]
          is base64-decoded when possible, verbatim otherwise. *)
  | Osc of int * string
      (** [Osc (number, payload)] is an unhandled OSC sequence. [payload] is the
          raw text between introducer and terminator with no sanitization. *)

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
  ?modifier:Key.modifier ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  t
(** [key k] is [Key (Key.make k)] with the given optional arguments. See
    {!Key.make} for defaults. *)

val char :
  ?modifier:Key.modifier ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  char ->
  t
(** [char c] is [Key (Key.of_char c)] with the given optional arguments. See
    {!Key.of_char} for defaults. *)

val key_event :
  ?modifier:Key.modifier ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [key_event k] is {!Key.make}[ k]. Alias useful when you need the raw
    {!Key.event} without wrapping in {!t}. *)

val char_event :
  ?modifier:Key.modifier ->
  ?event_type:Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  char ->
  Key.event
(** [char_event c] is {!Key.of_char}[ c]. *)

val press :
  ?modifier:Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [press k] is {!Key.make}[ ~event_type:Press k]. *)

val repeat :
  ?modifier:Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [repeat k] is {!Key.make}[ ~event_type:Repeat k]. *)

val release :
  ?modifier:Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Key.t ->
  Key.event
(** [release k] is {!Key.make}[ ~event_type:Release k]. *)

val mouse_press : ?modifier:Key.modifier -> int -> int -> Mouse.button -> t
(** [mouse_press x y button] is [Mouse (Button_press (x, y, button, modifier))].
    [modifier] defaults to {!Key.no_modifier}. *)

val mouse_release : ?modifier:Key.modifier -> int -> int -> Mouse.button -> t
(** [mouse_release x y button] is
    [Mouse (Button_release (x, y, button, modifier))]. [modifier] defaults to
    {!Key.no_modifier}. *)

val mouse_motion :
  ?modifier:Key.modifier -> int -> int -> Mouse.button_state -> t
(** [mouse_motion x y state] is [Mouse (Motion (x, y, state, modifier))].
    [modifier] defaults to {!Key.no_modifier}. *)

(** {1:helpers Helpers} *)

val match_ctrl_char : t -> char option
(** [match_ctrl_char e] is [Some c] when [e] is a {!Key} event with
    [modifier.ctrl = true] and the key is an ASCII [Char] (code point < 0x80).
    [None] otherwise. *)

val is_scroll : t -> bool
(** [is_scroll e] is [true] iff [e] is a {!Scroll} event or a {!Mouse} event
    with a wheel button ([Wheel_up], [Wheel_down], [Wheel_left], [Wheel_right]).
*)

val is_drag : t -> bool
(** [is_drag e] is [true] iff [e] is a {!Mouse} [Motion] event with at least one
    primary button pressed. *)
