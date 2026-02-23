(** Immediate-mode runtime for terminal user interfaces.

    Matrix owns the terminal while an application runs: it negotiates
    mouse/keyboard protocols, builds frames against a double-buffered grid, and
    diffs the grid to emit minimal ANSI output. The module re-exports the
    lower-level subsystems (ANSI generation, grids, terminal protocol, input
    parsing, images) and exposes an immediate-mode application via {!create}.

    {1 Overview}

    {!create} returns an inert application handle with configuration only. A
    platform-specific runtime (e.g., {!Matrix_unix}) attaches I/O via {!attach},
    then calls {!run} to drive the event loop. *)

(** {1 Sub-Libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Glyph = Glyph
(** Glyph management for text rendering. *)

module Grid = Grid
(** Cell-based drawing grid. *)

module Input = Input
(** Terminal input event parsing. *)

module Screen = Screen
(** Zero-allocation frame rendering. *)

module Terminal = Terminal
(** Terminal protocol state machine. *)

(** {1 Declarative Image API} *)

module Image = Image
(** Declarative image rendering and manipulation. *)

(** {1 Types} *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
(** Kitty keyboard protocol configuration used by {!create}.

    - [`Auto] (default) enables the protocol when the terminal advertises
      support. Modify-other-keys stays enabled otherwise so Ctrl+Alt chords keep
      working even on legacy terminals.
    - [`Disabled] never negotiates the protocol, which avoids the extra escape
      sequences on terminals with broken Kitty implementations.
    - [`Enabled flags] forces activation with the provided bitmask. Flags mirror
      the Kitty protocol specification (see
      {{:https://sw.kovidgoyal.net/kitty/keyboard-protocol/}Kitty keyboard
       protocol} for exact bits). *)

type mode = [ `Alt | `Primary ]
(** Presentation mode for the renderer:

    - [`Alt] (default) uses the terminal's alternate screen buffer where the app
      fully owns the screen and content is restored on exit.
    - [`Primary] renders inline on the primary screen anchored below the current
      cursor row. The UI occupies the remaining terminal height (while keeping a
      static row when possible), static output flows above it naturally, and all
      content enters native scrollback. *)

type debug_overlay_corner =
  [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** Corner to anchor the debug overlay when enabled. *)

type config
(** Inert configuration. Created by {!create}. No I/O, no terminal state. *)

type app
(** Attached application with I/O wired. All frame, query, and control functions
    require this type. Created by {!attach} from a {!config}. *)

(** {1 Lifecycle} *)

val create :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?target_fps:float option ->
  ?respect_alpha:bool ->
  ?mouse_enabled:bool ->
  ?mouse:Terminal.mouse_mode option ->
  ?bracketed_paste:bool ->
  ?focus_reporting:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?exit_on_ctrl_c:bool ->
  ?debug_overlay:bool ->
  ?debug_overlay_corner:debug_overlay_corner ->
  ?debug_overlay_capacity:int ->
  ?frame_dump_every:int ->
  ?frame_dump_dir:string ->
  ?frame_dump_pattern:string ->
  ?frame_dump_hits:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  ?input_timeout:float option ->
  ?resize_debounce:float option ->
  unit ->
  config
(** [create ()] builds a configuration record.

    The result is inert until passed to {!attach} by a runtime. No I/O is
    performed and no terminal state is modified.

    {4 Display Mode}

    @param mode
      Presentation mode for the renderer. Defaults to [`Alt] (alternate screen).
      See {!mode} for full description of [`Alt] and [`Primary] modes.
    @param raw_mode
      Whether the runtime should switch the TTY into raw mode (disables line
      buffering and echo). Defaults to [true].

    {4 Rendering Configuration}

    @param respect_alpha
      Whether to honor alpha blending when rendering cells. Defaults to [false].
    @param cursor_visible
      Initial cursor visibility. Defaults to [true] in [`Alt] mode and [false]
      in [`Primary] mode.
    @param explicit_width
      Whether to use explicit wcwidth values instead of querying the terminal.
      Defaults to [false].

    {4 Frame Timing}

    @param target_fps
      Optional FPS cap in Hz. Defaults to [Some 30.]. Set to [None] for uncapped
      rendering.
    @param resize_debounce
      Debounce window in seconds for resize events. Defaults to [Some 0.1].

    {4 Input Configuration}

    @param mouse_enabled Whether to enable mouse tracking. Defaults to [true].
    @param mouse
      Explicit mouse tracking mode. Defaults to [None], which selects [`Sgr_any]
      when [mouse_enabled] is [true].
    @param bracketed_paste
      Whether to enable bracketed paste mode. Defaults to [true].
    @param focus_reporting
      Whether to enable focus-in/focus-out events. Defaults to [true].
    @param kitty_keyboard
      Kitty keyboard protocol configuration. Defaults to [`Auto].
    @param exit_on_ctrl_c
      Whether to treat Ctrl+C as an exit signal. Defaults to [true].
    @param input_timeout
      Timeout in seconds for input polling when no cadence is active. Defaults
      to [None] (block indefinitely).

    {4 Debug and Diagnostics}

    @param debug_overlay
      Whether to show the built-in debug overlay. Defaults to [false].
    @param debug_overlay_corner
      Corner to anchor the debug overlay. Defaults to [`Bottom_right].
    @param debug_overlay_capacity
      Maximum number of metrics samples retained by the debug overlay.
    @param frame_dump_every
      Dump every Nth frame to disk. Defaults to [0] (disabled).
    @param frame_dump_dir Directory for frame dumps.
    @param frame_dump_pattern Filename pattern for frame dumps.
    @param frame_dump_hits
      Whether to include hit grid in frame dumps. Defaults to [false]. *)

val run :
  ?on_frame:(app -> dt:float -> unit) ->
  ?on_input:(app -> Input.t -> unit) ->
  ?on_resize:(app -> cols:int -> rows:int -> unit) ->
  on_render:(app -> unit) ->
  app ->
  unit
(** [run ?on_frame ?on_input ?on_resize ~on_render app] drives an immediate-mode
    loop.

    Matrix manages the lifecycle automatically: 1. Polls for events and invokes
    [on_input] / [on_resize]. 2. Calls {!prepare} to clear buffers. 3. Invokes
    [on_render] (users should draw to {!grid} here). 4. Calls {!submit} to flush
    output.

    [on_frame] runs before [prepare] with the elapsed seconds since the last
    render. The loop exits when {!running} becomes [false] and closes the
    runtime; exceptions close the runtime before propagating. *)

val prepare : app -> unit
(** [prepare app] starts a new frame lifecycle.

    It clears the internal {!grid} and {!hits} buffers and updates layout
    calculations based on the current terminal size. This must be called before
    drawing into the grid.

    {b Note:} If you use {!run}, this is called automatically. *)

val grid : app -> Grid.t
(** [grid app] returns the mutable grid for the current frame. *)

val hits : app -> Screen.Hit_grid.t
(** [hits app] returns the hit grid for the current frame. *)

val submit : app -> unit
(** [submit app] diffs the current frame against the previous one and flushes
    ANSI output to the terminal. Call this after you have finished drawing into
    {!grid}. *)

val close : app -> unit
(** [close app] tears down protocols and calls [io.cleanup]. Safe to call
    multiple times. *)

val stop : app -> unit
(** [stop app] marks the runtime as stopped. The run loop will exit on the next
    tick. *)

val start : app -> unit
(** [start app] explicitly resumes the render cadence (if it was paused) and
    marks the control state as explicitly started. *)

val pause : app -> unit
(** [pause app] stops the render cadence but leaves the terminal configured. A
    subsequent {!start} will resume. *)

val auto : app -> unit
(** [auto app] switches to automatic cadence control, idling when there are no
    live requests and resuming when {!request_live} is called. *)

val suspend : app -> unit
(** [suspend app] pauses rendering and restores the terminal to cooked mode,
    disabling negotiated input modes. {!resume} reapplies configuration. *)

val resume : app -> unit
(** [resume app] reapplies terminal configuration after {!suspend} and restores
    the previous control state (auto or explicit). *)

val request_live : app -> unit
(** [request_live app] signals that live work is pending. When transitioning
    from idle it restarts the render cadence automatically. *)

val drop_live : app -> unit
(** [drop_live app] decrements the live counter; when it reaches zero in auto
    mode the cadence idles. *)

val running : app -> bool
(** [running app] reports if the event loop is active. *)

val request_redraw : app -> unit
(** [request_redraw app] marks the frame dirty for the next loop iteration. *)

(** {1 Diagnostics} *)

val set_debug_overlay :
  ?corner:debug_overlay_corner -> app -> enabled:bool -> unit
(** [set_debug_overlay ?corner app ~enabled] toggles the built-in debug overlay.
*)

val toggle_debug_overlay : ?corner:debug_overlay_corner -> app -> unit
(** [toggle_debug_overlay ?corner app] flips overlay visibility. *)

val configure_frame_dump :
  ?every:int -> ?dir:string -> ?pattern:string -> ?hits:bool -> app -> unit
(** [configure_frame_dump ?every ?dir ?pattern ?hits app] updates the periodic
    frame-dump schedule. *)

val dump_frame : ?hits:bool -> ?dir:string -> ?pattern:string -> app -> unit
(** [dump_frame ?hits ?dir ?pattern app] writes the current frame to disk
    immediately. *)

(** {1 Terminal Information} *)

val mode_of_config : config -> mode
(** [mode_of_config config] returns the presentation mode from a configuration.
*)

val mode : app -> mode
(** [mode app] returns the presentation mode configured at creation time. *)

val size : app -> int * int
(** [size app] returns the current dynamic-region dimensions as [(cols, rows)].
*)

val pixel_resolution : app -> (int * int) option
(** [pixel_resolution app] returns the last known pixel resolution reported by
    the terminal as [(width, height)], or [None] if unknown. *)

val terminal : app -> Terminal.t
(** [terminal app] returns the underlying terminal handle. *)

val capabilities : app -> Terminal.capabilities
(** [capabilities app] returns the current terminal capabilities. *)

(** {1 Static Output} *)

val static_write : app -> string -> unit
(** [static_write app text] writes [text] to the primary screen above the
    renderer. Ignored in [`Alt] mode. *)

val static_print : app -> string -> unit
(** [static_print app text] writes [text] as a complete line to the static area.
    Any trailing newline in [text] is stripped; line separation between
    consecutive calls is handled automatically. Ignored in [`Alt] mode. *)

val static_clear : app -> unit
(** [static_clear app] clears previously written static content and resets the
    primary scroll region. *)

(** {1 Cursor Control} *)

val set_cursor : ?visible:bool -> ?style:Terminal.cursor_style -> app -> unit
(** [set_cursor ?visible ?style app] updates cursor visibility and style. *)

val set_cursor_style :
  app -> style:Terminal.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets the cursor style and blink flag
    explicitly. *)

val set_cursor_position : app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] moves the terminal cursor to 1-based
    coordinates within the dynamic render region. *)

val set_cursor_color : app -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color app ~r ~g ~b ~a] sets the terminal cursor color. *)

(** {1 Runtime Integration}

    Used by platform-specific runtimes to initialize and drive the application.
    Not intended for direct use by application code. *)

type io = {
  write_output : bytes -> int -> int -> unit;
      (** Write rendered frame bytes to terminal output. *)
  now : unit -> float;  (** Current wall-clock time in seconds. *)
  wake : unit -> unit;  (** Wake the event loop (cross-thread/signal safe). *)
  terminal_size : unit -> int * int;
      (** Query terminal dimensions as [(cols, rows)]. *)
  set_raw_mode : bool -> unit;  (** Enable or disable raw mode. *)
  flush_input : unit -> unit;  (** Discard unread input bytes. *)
  read_events : timeout:float option -> on_event:(Input.t -> unit) -> unit;
      (** Block up to [timeout] seconds, calling [on_event] for each input
          event. Handles signal-generated events (e.g. SIGWINCH) internally. *)
  query_cursor_position : timeout:float -> (int * int) option;
      (** Send CPR query and block for response. *)
  cleanup : unit -> unit;
      (** Release runtime resources (close fds, deregister signals). *)
}
(** I/O callback record that platform-specific runtimes provide. *)

val attach :
  config ->
  io:io ->
  terminal:Terminal.t ->
  width:int ->
  height:int ->
  ?render_offset:int ->
  ?static_needs_newline:bool ->
  unit ->
  app
(** [attach config ~io ~terminal ~width ~height ()] wires I/O callbacks and a
    terminal into a live application. Applies protocol configuration
    automatically. *)

val render_offset_of_cursor :
  terminal:Terminal.t -> height:int -> int -> int -> int * bool
(** [render_offset_of_cursor ~terminal ~height row col] interprets a CPR
    response as [(render_offset, static_needs_newline)]. Emits a CRLF to scroll
    if the cursor is at the bottom row. Used by runtimes during primary mode
    initialization and resume. *)
