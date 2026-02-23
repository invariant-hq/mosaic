(** Terminal protocol state machine with capability detection.

    {!Terminal} tracks which terminal protocols are active and writes escape
    sequences through a caller-provided [output] callback. It has no scheduler
    dependencies: no {!Unix.select}, no threads, no wakeup pipe, no signal
    handling.

    The caller (runtime) owns all I/O resources. Terminal writes escape
    sequences through [output] and tracks protocol state. The runtime drives
    event reading, capability probing, and frame writing.

    {1 Capability detection}

    Capabilities are seeded from environment heuristics at creation time. The
    runtime refines them by calling {!probe} with I/O callbacks, or
    incrementally via {!apply_capability_event} as the input parser produces
    capability responses.

    {1 Protocol symmetry}

    Anything enabled through the handle is unwound by {!close} or
    {!reset_state}. Protocol operations are idempotent: requesting a mode that
    is already active is a no-op. For non-TTY handles ([tty = false]), escape
    sequences are suppressed but internal state tracking remains active. *)

(** {1 Types} *)

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]
(** Mouse tracking protocols.

    - [`Off]: disable mouse tracking.
    - [`X10]: basic press-only tracking (legacy; coordinates limited to 223x223
      cells).
    - [`Normal]: press and release events with standard xterm encoding (1000).
    - [`Button]: press, release, and drag events (1002).
    - [`Any]: all motion events, including motion with no buttons pressed
      (1003).
    - [`Sgr_normal]: SGR-extended press/release only (1006 + 1000).
    - [`Sgr_button]: SGR-extended with drag events (1006 + 1000 + 1002).
    - [`Sgr_any]: SGR-extended with all motion (1006 + 1000 + 1002 + 1003). *)

type unicode_width = [ `Wcwidth | `Unicode ]
(** Unicode width calculation method.

    - [`Wcwidth]: use platform [wcwidth()]-like behavior. Compatible with older
      setups but inaccurate for some emoji and combining characters.
    - [`Unicode]: use modern Unicode width tables. *)

type cursor_style = [ `Block | `Line | `Underline ]
(** Cursor visual style. Corresponds to standard DECSCUSR shapes. *)

type cursor_position = { x : int; y : int; visible : bool }
(** Tracked cursor position and visibility.

    - [x], [y] are 1-based coordinates; [(1, 1)] is the top-left corner.
    - [visible] reflects the last requested visibility. *)

(** {1 Capabilities} *)

type capabilities = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  explicit_cursor_positioning : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}
(** Terminal capabilities.

    Detection happens in two stages:

    - {b Environment stage}: {!make} inspects [$TERM], [$COLORTERM],
      [$KITTY_WINDOW_ID], platform quirks, and overrides such as
      [MATRIX_FORCE_UNICODE]. No escape sequences are emitted.
    - {b Probe stage}: The runtime calls {!probe} with I/O callbacks and the
      results are folded into the capability record.

    Boolean fields from probing are monotonic: once confirmed, subsequent probes
    will not disable them. Environment policy overrides may still disable
    features in specific contexts (e.g. tmux forces wcwidth mode). *)

type terminal_info = { name : string; version : string; from_xtversion : bool }
(** Best-known terminal program name and version.

    Populated from XTVersion when available; otherwise derived from
    [$TERM_PROGRAM]/[$TERM_PROGRAM_VERSION]. *)

(** {1 Session handle} *)

type t
(** Terminal protocol state.

    A value of type [t] encapsulates:

    - an output callback for writing escape sequences
    - capability and terminal metadata
    - an input parser for decoding {!Input.t} events
    - protocol state: mouse mode, cursor, alternate screen, etc.

    Close with {!close} to unwind all active protocols. *)

val make :
  output:(string -> unit) ->
  ?tty:bool ->
  ?initial_caps:capabilities ->
  ?parser:Input.Parser.t ->
  unit ->
  t
(** [make ~output ?tty ?initial_caps ?parser ()] creates a terminal handle.

    - [output] receives escape sequence strings. For Unix:
      [fun s -> write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)].
    - [tty] controls whether escape sequences are actually emitted (default
      [true]). Set to [false] for non-TTY outputs or testing.
    - [initial_caps] seeds the capability record. When [None], capabilities are
      built from [$TERM] and environment heuristics. When [Some caps], the
      record is used as-is (environment overrides are not applied).
    - [parser] shares an existing {!Input.Parser.t} with the runtime. When
      [None], a fresh parser is created.

    The handle starts with no protocols enabled. *)

val close : t -> unit
(** [close t] unwinds all active protocols via [output].

    Disables mouse tracking, bracketed paste, focus reporting, Kitty keyboard,
    modifyOtherKeys, Unicode mode, and alternate screen. Resets cursor
    visibility, SGR attributes, cursor colour/style, and window title.

    Does {b not} restore termios or close file descriptors -- that is the
    runtime's responsibility. *)

(** {1 Capability access} *)

val capabilities : t -> capabilities
(** Returns the current capability record. *)

val set_capabilities : t -> capabilities -> unit
(** Replaces the capability record (e.g. after probing). *)

val terminal_info : t -> terminal_info
(** Returns the current terminal identity. *)

val set_terminal_info : t -> terminal_info -> unit
(** Replaces the terminal identity (e.g. after XTVersion response). *)

val pixel_resolution : t -> (int * int) option
(** Returns the last known pixel resolution as [(width, height)]. *)

val set_pixel_resolution : t -> (int * int) option -> unit
(** Updates the cached pixel resolution. *)

val apply_capability_event : t -> Input.Caps.event -> unit
(** [apply_capability_event t event] folds a single capability response into
    state. Called by the runtime as {!Input.Parser} produces capability events.

    Updates {!capabilities}, {!terminal_info}, and {!pixel_resolution} as
    appropriate. *)

(** {1 Probing} *)

val probe :
  ?timeout:float ->
  on_event:(Input.t -> unit) ->
  read_into:(bytes -> int -> int -> int) ->
  wait_readable:(timeout:float -> bool) ->
  t ->
  unit
(** [probe ?timeout ~on_event ~read_into ~wait_readable t] actively probes
    terminal capabilities.

    Sends a compound query payload through [output] and processes responses
    using the provided I/O callbacks. Updates {!capabilities} and
    {!terminal_info} with the results.

    User input events received during probing are forwarded to [on_event].

    @param timeout maximum probe duration in seconds (default 0.2) *)

(** {1 Protocol control}

    All protocol operations are idempotent. For TTY handles, escape sequences
    are emitted through [output]. For non-TTY handles, only internal state is
    updated. *)

val restore_modes : t -> unit
(** [restore_modes t] unconditionally re-sends enable sequences for every
    currently-active protocol mode (mouse tracking, focus reporting, bracketed
    paste, Kitty keyboard, modifyOtherKeys).

    Intended for focus-in recovery: some terminal emulators (notably Windows
    Terminal / ConPTY) strip DEC private modes when the window loses focus.
    Re-sending the active modes restores correct behavior.

    For the Kitty keyboard protocol, the existing stack entry is popped before
    re-pushing to avoid unbounded stack growth. *)

val set_mouse_mode : t -> mouse_mode -> unit
(** Configures mouse event tracking. Disables conflicting modes first. *)

val mouse_mode : t -> mouse_mode
(** Returns the current mouse tracking mode. Initially [`Off]. *)

val enable_bracketed_paste : t -> bool -> unit
(** Toggles bracketed paste mode (DECSET/DECRST 2004). *)

val bracketed_paste_enabled : t -> bool
(** Reports whether bracketed paste is currently enabled. *)

val enable_focus_reporting : t -> bool -> unit
(** Toggles focus event reporting (DECSET/DECRST 1004). *)

val focus_reporting_enabled : t -> bool
(** Reports whether focus reporting is currently enabled. *)

val enable_kitty_keyboard : ?flags:int -> t -> bool -> unit
(** Toggles the Kitty keyboard protocol.

    @param flags
      bitmask controlling reported features (default [0b00101]):
      - bit 0: disambiguate escape codes
      - bit 1: report event types (press/repeat/release)
      - bit 2: report alternate keys
      - bit 3: report all keys as escape sequences
      - bit 4: report associated text

    Enabling pushes flags; disabling pops. Changing flags while enabled pushes
    new flags. *)

val kitty_keyboard_enabled : t -> bool
(** Reports whether the Kitty keyboard protocol is currently enabled. *)

val enable_modify_other_keys : t -> bool -> unit
(** Toggles xterm modifyOtherKeys mode. *)

val modify_other_keys_enabled : t -> bool
(** Reports whether modifyOtherKeys is currently enabled. *)

val set_unicode_width : t -> unicode_width -> unit
(** Sets the Unicode width mode and updates capabilities. *)

val enter_alternate_screen : t -> unit
(** Switches to the alternate screen buffer (DECSET 1049). Hides the cursor.
    Idempotent: calling when already on the alternate screen has no effect. *)

val leave_alternate_screen : t -> unit
(** Returns to the primary screen buffer (DECRST 1049). Idempotent. *)

val alt_screen : t -> bool
(** Reports whether the alternate screen is currently active. *)

val set_scroll_region : t -> top:int -> bottom:int -> unit
(** Sets the scrolling region (DECSTBM). 1-based, inclusive. Idempotent. *)

val clear_scroll_region : t -> unit
(** Resets the scrolling region to the full screen. Idempotent. *)

val scroll_region : t -> (int * int) option
(** Returns the current scroll region as [Some (top, bottom)] or [None]. *)

(** {1 Cursor and appearance} *)

val move_cursor : ?visible:bool -> t -> row:int -> col:int -> unit
(** Moves the cursor to 1-based coordinates. Coordinates are clamped to >= 1.
    [visible] defaults to [true]. *)

val set_cursor_visible : t -> bool -> unit
(** Shows or hides the cursor (DECTCEM). Idempotent. *)

val cursor_visible : t -> bool
(** Returns the last requested cursor visibility. *)

val cursor_position : t -> cursor_position
(** Returns the tracked cursor position and visibility. *)

val set_cursor_style : t -> cursor_style -> blinking:bool -> unit
(** Sets the cursor shape and blinking behaviour (DECSCUSR). *)

val cursor_style_state : t -> cursor_style * bool
(** Returns the current cursor style and blinking flag. *)

val set_cursor_color : t -> r:float -> g:float -> b:float -> a:float -> unit
(** Sets the cursor colour. Components are clamped to [[0.0, 1.0]]. NaN
    components are treated as [0.0]. The alpha channel is tracked but not sent
    to the terminal. *)

val cursor_color : t -> float * float * float * float
(** Returns the last set cursor colour as [(r, g, b, a)]. *)

val reset_cursor_color : t -> unit
(** Restores the terminal's default cursor colour. *)

val set_title : t -> string -> unit
(** Sets the terminal window title (OSC 0). *)

val query_pixel_resolution : t -> unit
(** Sends a pixel resolution query (CSI 14 t) through [output]. The response
    arrives asynchronously and should be processed via
    {!apply_capability_event}. *)

(** {1 Output and state} *)

val send : t -> string -> unit
(** Writes through the output callback. Respects the [tty] flag: for non-TTY
    handles, this is a no-op. *)

val tty : t -> bool
(** Returns whether escape sequences are emitted. *)

val parser : t -> Input.Parser.t
(** Returns the input parser (for the runtime to feed bytes into). *)

val reset_state : t -> unit
(** Resets all protocol state to defaults.

    Disables mouse tracking, bracketed paste, focus events, Kitty keyboard,
    modifyOtherKeys, Unicode mode, scroll region, and alternate screen. Resets
    SGR attributes, cursor colour/style, and window title. *)

(** {1 TTY helpers}

    Stateless functions for terminal I/O. These do not require a session handle.
*)

val set_raw : Unix.file_descr -> Unix.terminal_io
(** [set_raw fd] puts [fd] into raw mode (no echo, no canonical processing, no
    signals, non-blocking) and returns the original termios for later
    restoration with {!restore}. *)

val restore : Unix.file_descr -> Unix.terminal_io -> unit
(** [restore fd termios] restores [fd] to the given termios settings and clears
    non-blocking mode. *)

val size : Unix.file_descr -> int * int
(** [size fd] returns terminal dimensions as [(cols, rows)]. Falls back to
    [(80, 24)] on failure. *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] tests whether [fd] is a terminal. *)

val flush_input : Unix.file_descr -> unit
(** [flush_input fd] discards unread input on [fd] via [tcflush]. *)

val enable_vt : Unix.file_descr -> unit
(** [enable_vt fd] enables VT100 processing on Windows consoles. No-op on Unix.
    Failures are silently ignored. *)
