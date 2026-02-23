(** Terminal protocol state machine.

    [Terminal] manages terminal protocol state and writes escape sequences
    through a caller-provided {e output} callback. The caller ({e runtime}) owns
    all I/O resources: no {!Unix.select}, no threads, no wakeup pipe, no signal
    handling.

    {2:caps_detect Capability detection}

    Capabilities are seeded from environment heuristics at creation time
    ({!make}). The runtime refines them by calling {!probe} with I/O callbacks,
    or incrementally via {!apply_capability_event} as the input parser produces
    capability responses.

    {2:protocol_sym Protocol symmetry}

    Every protocol enabled through the handle is unwound by {!close} or
    {!reset_state}. Protocol operations are idempotent: requesting a mode that
    is already active is a no-op. For non-TTY handles ([tty = false]) escape
    sequences are suppressed but internal state tracking remains active.

    {1:types Types} *)

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]
(** The type for mouse tracking protocols.
    - [`Off] disables mouse tracking.
    - [`X10] is basic press-only tracking (legacy; coordinates limited to
      223x223 cells).
    - [`Normal] reports press and release events with standard xterm encoding
      (mode 1000).
    - [`Button] reports press, release, and drag events (mode 1002).
    - [`Any] reports all motion events, including motion with no buttons pressed
      (mode 1003).
    - [`Sgr_normal] is SGR-extended press and release only (modes 1006 + 1000).
    - [`Sgr_button] is SGR-extended with drag events (modes 1006 + 1000 + 1002).
    - [`Sgr_any] is SGR-extended with all motion (modes 1006 + 1000 + 1002 +
      1003). *)

type unicode_width = [ `Wcwidth | `Unicode ]
(** The type for Unicode width calculation methods.
    - [`Wcwidth] uses platform [wcwidth()]-like behaviour. Compatible with older
      setups but inaccurate for some emoji and combining characters.
    - [`Unicode] uses modern Unicode width tables. *)

type cursor_style = [ `Block | `Line | `Underline ]
(** The type for cursor visual styles. Corresponds to standard DECSCUSR shapes.
*)

type cursor_position = { x : int; y : int; visible : bool }
(** The type for tracked cursor positions. [x] and [y] are one-based
    coordinates; [(1, 1)] is the top-left corner. [visible] reflects the last
    requested visibility. *)

(** {1:capabilities Capabilities} *)

type capabilities = {
  term : string;  (** Raw [$TERM] value used for heuristics. *)
  rgb : bool;  (** [true] if 24-bit colour is considered safe. *)
  kitty_keyboard : bool;
      (** [true] if the Kitty keyboard protocol is supported. *)
  kitty_graphics : bool;
      (** [true] if the Kitty graphics protocol is supported. *)
  bracketed_paste : bool;
      (** [true] if bracketed paste (mode 2004) is supported. *)
  focus_tracking : bool;
      (** [true] if focus tracking (mode 1004) is supported. *)
  unicode_width : unicode_width;  (** Current Unicode width mode. *)
  sgr_pixels : bool;
      (** [true] if SGR pixel-position mouse reports (mode 1016) are supported.
      *)
  color_scheme_updates : bool;
      (** [true] if colour-scheme update notifications (mode 2031) are
          supported. *)
  explicit_width : bool;
      (** [true] if the terminal reported explicit cell-width information via
          proprietary cursor-position queries. *)
  explicit_cursor_positioning : bool;
      (** [true] if the terminal may miscalculate grapheme widths but supports
          reliable cursor positioning. When set, the renderer repositions the
          cursor after each wide grapheme to prevent column drift. Only used
          when {!field-explicit_width} is [false]. *)
  scaled_text : bool;  (** [true] if scaled text sizing is supported. *)
  sixel : bool;  (** [true] if SIXEL graphics are supported. *)
  sync : bool;  (** [true] if synchronised output (mode 2026) is supported. *)
  hyperlinks : bool;  (** [true] if OSC 8 hyperlinks are supported. *)
}
(** The type for terminal capabilities.

    Detection happens in two stages:
    - {b Environment stage.} {!make} inspects [$TERM], [$COLORTERM],
      [$KITTY_WINDOW_ID], platform quirks, and overrides such as
      [MATRIX_FORCE_UNICODE]. No escape sequences are emitted.
    - {b Probe stage.} The runtime calls {!probe} with I/O callbacks and the
      results are folded into the capability record.

    Boolean fields from probing are monotonic: once confirmed, subsequent probes
    do not disable them. Environment policy overrides may still disable features
    in specific contexts (e.g. tmux forces wcwidth mode). *)

type terminal_info = { name : string; version : string; from_xtversion : bool }
(** The type for terminal emulator identity. [name] and [version] are populated
    from XTVersion when available; otherwise derived from [$TERM_PROGRAM] and
    [$TERM_PROGRAM_VERSION]. [from_xtversion] is [true] iff [name] and [version]
    came from an XTVersion response. *)

(** {1:handle Session handle} *)

type t
(** The type for terminal protocol state. A value of type [t] encapsulates an
    output callback, capability and terminal metadata, and protocol state (mouse
    mode, cursor, alternate screen, etc.).

    Close with {!close} to unwind all active protocols. *)

val make :
  output:(string -> unit) ->
  ?tty:bool ->
  ?initial_caps:capabilities ->
  unit ->
  t
(** [make ~output ~tty ~initial_caps ()] is a terminal handle with:
    - [output] receives escape sequence strings.
    - [tty] controls whether escape sequences are emitted. Defaults to [true].
      Set to [false] for non-TTY outputs or testing.
    - [initial_caps] seeds the capability record. When omitted, capabilities are
      built from [$TERM] and environment heuristics. When provided, the record
      is used as-is and environment overrides are not applied.

    The handle starts with no protocols enabled. *)

val close : t -> unit
(** [close t] unwinds all active protocols via [output].

    Disables mouse tracking, bracketed paste, focus reporting, Kitty keyboard,
    modifyOtherKeys, Unicode mode, and alternate screen. Resets cursor
    visibility, SGR attributes, cursor colour and style, and window title.

    {b Note.} Does not restore termios or close file descriptors; that is the
    runtime's responsibility. *)

(** {1:cap_access Capability access} *)

val capabilities : t -> capabilities
(** [capabilities t] is [t]'s current capability record. *)

val set_capabilities : t -> capabilities -> unit
(** [set_capabilities t caps] replaces [t]'s capability record with [caps]. *)

val terminal_info : t -> terminal_info
(** [terminal_info t] is [t]'s current terminal identity. *)

val set_terminal_info : t -> terminal_info -> unit
(** [set_terminal_info t info] replaces [t]'s terminal identity with [info]. *)

val pixel_resolution : t -> (int * int) option
(** [pixel_resolution t] is the last known pixel resolution as
    [Some (width, height)], or [None] if unknown. *)

val set_pixel_resolution : t -> (int * int) option -> unit
(** [set_pixel_resolution t res] updates [t]'s cached pixel resolution. *)

val apply_capability_event : t -> Input.Caps.event -> unit
(** [apply_capability_event t event] folds a single capability response into
    [t]'s state. Updates {!capabilities}, {!terminal_info}, and
    {!pixel_resolution} as appropriate.

    Called by the runtime as {!Input.Parser} produces capability events. *)

(** {1:probing Probing} *)

val probe :
  ?timeout:float ->
  on_event:(Input.t -> unit) ->
  read_into:(bytes -> int -> int -> int) ->
  wait_readable:(timeout:float -> bool) ->
  parser:Input.Parser.t ->
  t ->
  unit
(** [probe ~timeout ~on_event ~read_into ~wait_readable ~parser t] actively
    probes terminal capabilities.

    Sends a compound query payload through [output] and processes responses
    using the provided I/O callbacks and [parser]. Updates {!capabilities} and
    {!terminal_info} with the results.

    Using the caller's [parser] ensures that partial escape sequences spanning
    the probe boundary are preserved. User input events received during probing
    are forwarded to [on_event].

    [timeout] is the maximum probe duration in seconds. Defaults to [0.2]. *)

(** {1:protocol Protocol control}

    All protocol operations are idempotent. For TTY handles, escape sequences
    are emitted through [output]. For non-TTY handles, only internal state is
    updated. *)

val restore_modes : ?skip_focus:bool -> t -> unit
(** [restore_modes ?skip_focus t] unconditionally re-sends enable sequences for
    every currently-active protocol mode (mouse tracking, focus reporting,
    bracketed paste, Kitty keyboard, modifyOtherKeys).

    When [skip_focus] is [true], the focus-tracking enable sequence is not
    re-sent. This avoids a feedback loop when [restore_modes] is called from a
    Focus event handler: some terminals re-report focus state upon receiving the
    enable sequence, creating an infinite cycle.

    Intended for focus-in recovery: some terminal emulators (notably Windows
    Terminal / ConPTY) strip DEC private modes when the window loses focus.

    {b Note.} For the Kitty keyboard protocol, the existing stack entry is
    popped before re-pushing to avoid unbounded stack growth. *)

(** {2:mouse Mouse tracking} *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] configures mouse event tracking to [mode].
    Conflicting modes are disabled first. *)

val mouse_mode : t -> mouse_mode
(** [mouse_mode t] is [t]'s current mouse tracking mode. Initially [`Off]. *)

(** {2:paste Bracketed paste} *)

val enable_bracketed_paste : t -> bool -> unit
(** [enable_bracketed_paste t b] enables or disables bracketed paste mode
    (DECSET/DECRST 2004). Idempotent. *)

val bracketed_paste_enabled : t -> bool
(** [bracketed_paste_enabled t] is [true] iff bracketed paste mode is active. *)

(** {2:focus Focus reporting} *)

val enable_focus_reporting : t -> bool -> unit
(** [enable_focus_reporting t b] enables or disables focus event reporting
    (DECSET/DECRST 1004). Idempotent. *)

val focus_reporting_enabled : t -> bool
(** [focus_reporting_enabled t] is [true] iff focus event reporting is active.
*)

(** {2:kitty_kb Kitty keyboard} *)

val enable_kitty_keyboard : ?flags:int -> t -> bool -> unit
(** [enable_kitty_keyboard ~flags t b] enables or disables the Kitty keyboard
    protocol with:
    {ul
     {- [flags] is a bitmask controlling reported features. Defaults to
        [0b00101] (disambiguate escape codes + report alternate keys). The bits
        are:
        - bit 0: disambiguate escape codes
        - bit 1: report event types (press/repeat/release)
        - bit 2: report alternate keys
        - bit 3: report all keys as escape sequences
        - bit 4: report associated text
     }
    }

    Enabling pushes [flags] onto the terminal's keyboard stack; disabling pops.
    Changing [flags] while enabled pushes the new value. *)

val kitty_keyboard_enabled : t -> bool
(** [kitty_keyboard_enabled t] is [true] iff the Kitty keyboard protocol is
    active. *)

(** {2:mok modifyOtherKeys} *)

val enable_modify_other_keys : t -> bool -> unit
(** [enable_modify_other_keys t b] enables or disables xterm modifyOtherKeys
    mode. Idempotent. *)

val modify_other_keys_enabled : t -> bool
(** [modify_other_keys_enabled t] is [true] iff modifyOtherKeys mode is active.
*)

(** {2:unicode_w Unicode width} *)

val set_unicode_width : t -> unicode_width -> unit
(** [set_unicode_width t w] sets the Unicode width mode to [w] and updates
    {!capabilities} accordingly. Idempotent. *)

(** {2:alt_screen Alternate screen} *)

val enter_alternate_screen : t -> unit
(** [enter_alternate_screen t] switches to the alternate screen buffer (DECSET
    1049). Idempotent. *)

val leave_alternate_screen : t -> unit
(** [leave_alternate_screen t] returns to the primary screen buffer (DECRST
    1049). Idempotent. *)

val alt_screen : t -> bool
(** [alt_screen t] is [true] iff the alternate screen buffer is active. *)

(** {2:scroll Scroll region} *)

val set_scroll_region : t -> top:int -> bottom:int -> unit
(** [set_scroll_region t ~top ~bottom] sets the scrolling region (DECSTBM).
    [top] and [bottom] are one-based, inclusive. Idempotent. *)

val clear_scroll_region : t -> unit
(** [clear_scroll_region t] resets the scrolling region to the full screen.
    Idempotent. *)

val scroll_region : t -> (int * int) option
(** [scroll_region t] is [Some (top, bottom)] if a scrolling region is set, or
    [None] if the full screen is used. *)

(** {1:cursor Cursor and appearance} *)

val move_cursor : ?visible:bool -> t -> row:int -> col:int -> unit
(** [move_cursor ~visible t ~row ~col] moves the cursor to one-based coordinates
    [row], [col]. Coordinates are clamped to [>= 1]. [visible] defaults to
    [true]. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t b] shows or hides the cursor (DECTCEM). Idempotent. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is the last requested cursor visibility. *)

val cursor_position : t -> cursor_position
(** [cursor_position t] is [t]'s tracked cursor position and visibility. *)

val set_cursor_style : t -> cursor_style -> blinking:bool -> unit
(** [set_cursor_style t style ~blinking] sets the cursor shape and blinking
    behaviour (DECSCUSR). *)

val cursor_style_state : t -> cursor_style * bool
(** [cursor_style_state t] is the current cursor style and blinking flag as
    [(style, blinking)]. *)

val set_cursor_color : t -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color t ~r ~g ~b ~a] sets the cursor colour. Components are
    clamped to \[[0.0]; [1.0]\]. [NaN] components are treated as [0.0]. The
    alpha channel is tracked but not sent to the terminal. *)

val cursor_color : t -> float * float * float * float
(** [cursor_color t] is the last set cursor colour as [(r, g, b, a)]. *)

val reset_cursor_color : t -> unit
(** [reset_cursor_color t] restores the terminal's default cursor colour. *)

val set_title : t -> string -> unit
(** [set_title t s] sets the terminal window title to [s] (OSC 0). *)

val query_pixel_resolution : t -> unit
(** [query_pixel_resolution t] sends a pixel resolution query (CSI 14 t) through
    [output]. The response arrives asynchronously and should be processed via
    {!apply_capability_event}. *)

(** {1:output Output and state} *)

val send : t -> string -> unit
(** [send t s] writes [s] through the output callback. For non-TTY handles this
    is a no-op. *)

val tty : t -> bool
(** [tty t] is [true] iff escape sequences are emitted. *)

val reset_state : t -> unit
(** [reset_state t] resets all protocol state to defaults.

    Disables mouse tracking, bracketed paste, focus events, Kitty keyboard,
    modifyOtherKeys, Unicode mode, scroll region, and alternate screen. Resets
    SGR attributes, cursor colour and style, and window title. *)

(** {1:tty_helpers TTY helpers}

    Stateless functions for terminal I/O. These do not require a {!type-t}
    session handle. *)

val set_raw : Unix.file_descr -> Unix.terminal_io
(** [set_raw fd] puts [fd] into raw mode (no echo, no canonical processing, no
    signals, non-blocking) and returns the original termios for later
    restoration with {!restore}. *)

val restore : Unix.file_descr -> Unix.terminal_io -> unit
(** [restore fd termios] restores [fd] to the given [termios] settings and
    clears non-blocking mode. *)

val size : Unix.file_descr -> int * int
(** [size fd] is the terminal dimensions as [(cols, rows)]. Falls back to
    [(80, 24)] on failure. *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] is [true] iff [fd] is a terminal. *)

val flush_input : Unix.file_descr -> unit
(** [flush_input fd] discards unread input on [fd] via [tcflush]. *)

val enable_vt : Unix.file_descr -> unit
(** [enable_vt fd] enables VT100 processing on Windows consoles. No-op on Unix.
    Failures are silently ignored. *)
