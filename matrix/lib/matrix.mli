(** Immediate-mode terminal application runtime.

    [Matrix] owns the terminal while an application runs: it negotiates input
    protocols, builds frames against a double-buffered {!Grid.t}, and diffs the
    grid to emit minimal ANSI output.

    The module re-exports the lower-level subsystems as submodules and provides
    an immediate-mode event loop via {!create} and {!run}.

    {1:sub_libraries Sub-libraries}

    {!modules:Ansi Glyph Grid Input Screen Terminal Image} *)

(** {1:sub Sub-libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Glyph = Glyph
(** Unicode glyph measurement. *)

module Grid = Grid
(** Cell-based drawing grid. *)

module Input = Input
(** Terminal input event parsing. *)

module Screen = Screen
(** Double-buffered frame rendering. *)

module Terminal = Terminal
(** Terminal protocol state machine. *)

module Image = Image
(** Declarative image composition and rendering. *)

(** {1:types Types} *)

type kitty_keyboard = [ `Auto | `Disabled | `Enabled of int ]
(** The type for Kitty keyboard protocol configurations.
    - [`Auto] enables the protocol when the terminal advertises support; falls
      back to modify-other-keys for Ctrl+Alt chords on legacy terminals.
    - [`Disabled] never negotiates the protocol.
    - [`Enabled flags] forces activation with the given bitmask. See the
      {{:https://sw.kovidgoyal.net/kitty/keyboard-protocol/}Kitty keyboard
       protocol specification} for flag values. *)

type mode = [ `Alt | `Primary ]
(** The type for presentation modes.
    - [`Alt] uses the alternate screen buffer. The application fully owns the
      screen; content is restored on exit.
    - [`Primary] renders inline on the primary screen below the current cursor
      row. Static output flows above the UI and enters native scrollback. *)

type debug_overlay_corner =
  [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** The type for debug overlay anchor corners. *)

type app
(** The type for live applications. All frame, query, and control functions
    require this handle. Created by {!create} or {!attach}. *)

(** {1:lifecycle Lifecycle} *)

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
  ?output:[ `Stdout | `Fd of Unix.file_descr ] ->
  ?signal_handlers:bool ->
  ?initial_caps:Terminal.capabilities ->
  ?min_tui_height:int ->
  ?start_idle:bool ->
  unit ->
  app
(** [create ()] is a live application with Unix I/O wired in.

    Sets up the terminal, enables raw mode, installs signal handlers, and
    returns a ready-to-run handle.

    {b Display:}
    - [mode] presentation mode. Defaults to [`Alt].
    - [raw_mode] whether to switch the TTY into raw mode. Defaults to [true].

    {b Rendering:}
    - [respect_alpha] whether to honour alpha blending. Defaults to [false].
    - [cursor_visible] initial cursor visibility. Defaults to [true] in [`Alt]
      mode, [false] in [`Primary].
    - [explicit_width] whether to use built-in wcwidth instead of querying the
      terminal. Defaults to [false].

    {b Frame timing:}
    - [target_fps] optional FPS cap in Hz. Defaults to [Some 30.]. [None] for
      uncapped.
    - [resize_debounce] debounce window in seconds for resize events. Defaults
      to [Some 0.1].

    {b Primary mode:}
    - [min_tui_height] minimum height in rows reserved for the dynamic TUI
      region. Static content will not grow past this floor. Defaults to [1].
    - [start_idle] when [true], the render loop starts in idle state even when
      [target_fps] is set. The loop begins running only when {!request_live}
      is called. One-shot redraws via {!request_redraw} still work while idle.
      Defaults to [false].

    {b Input:}
    - [mouse_enabled] whether to enable mouse tracking. Defaults to [true].
    - [mouse] explicit mouse tracking mode. Defaults to [None] (selects
      [`Sgr_any] when [mouse_enabled] is [true]).
    - [bracketed_paste] whether to enable bracketed paste. Defaults to [true].
    - [focus_reporting] whether to enable focus-in/focus-out events. Defaults to
      [true].
    - [kitty_keyboard] Kitty keyboard protocol configuration. Defaults to
      [`Auto].
    - [exit_on_ctrl_c] whether Ctrl+C exits. Defaults to [true].
    - [input_timeout] timeout in seconds for input polling when no cadence is
      active. Defaults to [None] (block indefinitely).

    {b Unix:}
    - [output] output target. Defaults to [`Stdout].
    - [signal_handlers] whether to install SIGTERM/SIGINT/SIGQUIT handlers.
      Defaults to [true].
    - [initial_caps] seed capabilities passed to {!Terminal.make}.

    {b Diagnostics:}
    - [debug_overlay] show the debug overlay. Defaults to [false].
    - [debug_overlay_corner] anchor corner. Defaults to [`Bottom_right].
    - [debug_overlay_capacity] maximum metric samples retained.
    - [frame_dump_every] dump every Nth frame. Defaults to [0] (disabled).
    - [frame_dump_dir] directory for frame dumps.
    - [frame_dump_pattern] filename pattern for frame dumps.
    - [frame_dump_hits] include hit grid in dumps. Defaults to [false]. *)

val run :
  ?on_frame:(app -> dt:float -> unit) ->
  ?on_input:(app -> Input.t -> unit) ->
  ?on_resize:(app -> cols:int -> rows:int -> unit) ->
  ?primary_required_rows:(app -> int option) ->
  on_render:(app -> unit) ->
  app ->
  unit
(** [run ~on_render app] drives the immediate-mode event loop.

    Each iteration:
    - Polls for events and invokes [on_input] / [on_resize].
    - Calls [on_frame] with the elapsed seconds since the last render.
    - Calls {!prepare} to clear buffers.
    - Invokes [on_render] (draw to {!grid} here).
    - Optionally samples [primary_required_rows] for [`Primary] mode sizing.
    - Calls {!submit} to diff and flush output.

    The loop exits when {!running} becomes [false]. Exceptions close the runtime
    before propagating. *)

(** {1:frame Frame building} *)

val prepare : app -> unit
(** [prepare app] starts a new frame. Clears the {!grid} and {!hits} buffers and
    updates layout calculations.

    {b Note.} Called automatically by {!run}. *)

val grid : app -> Grid.t
(** [grid app] is the mutable grid for the current frame. *)

val hits : app -> Screen.Hit_grid.t
(** [hits app] is the hit grid for the current frame. *)

val submit : ?primary_required_rows:int -> app -> unit
(** [submit ?primary_required_rows app] diffs the current frame against the
    previous one and flushes ANSI output. Call after drawing into {!grid}.

    In [`Primary] mode, [primary_required_rows] can be provided to request a
    larger dynamic render region for this frame.

    {b Note.} Called automatically by {!run}. *)

(** {1:control Control} *)

val close : app -> unit
(** [close app] tears down protocols and releases resources. Safe to call
    multiple times. *)

val stop : app -> unit
(** [stop app] marks the runtime as stopped. The {!run} loop exits on the next
    tick. *)

val start : app -> unit
(** [start app] resumes the render cadence and marks the control state as
    explicitly started. *)

val pause : app -> unit
(** [pause app] stops the render cadence but leaves the terminal configured.
    {!start} resumes. *)

val suspend : app -> unit
(** [suspend app] pauses rendering and restores the terminal to cooked mode.
    {!resume} reapplies configuration. *)

val resume : app -> unit
(** [resume app] reapplies terminal configuration after {!suspend}. *)

val request_live : app -> unit
(** [request_live app] signals pending live work. Restarts the render cadence
    when transitioning from idle. *)

val drop_live : app -> unit
(** [drop_live app] decrements the live counter. When it reaches zero in auto
    mode the cadence idles. *)

val running : app -> bool
(** [running app] is [true] iff the event loop is active. *)

val request_redraw : app -> unit
(** [request_redraw app] marks the frame dirty for the next iteration. *)

(** {1:queries Terminal queries} *)

val mode : app -> mode
(** [mode app] is the presentation mode set at creation time. *)

val size : app -> int * int
(** [size app] is the current dynamic-region dimensions as [(cols, rows)]. *)

val full_size : app -> int * int
(** [full_size app] is the full terminal dimensions [(cols, rows)],
    ignoring the primary-mode render offset. In [`Alt] mode this is
    identical to {!size}. *)

val effective_size : app -> int * int
(** [effective_size app] is the dynamic-region dimensions [(cols, rows)]
    that will be in effect once pending static writes are flushed. In
    [`Alt] mode or when the static queue is empty this equals {!size}.
    Use this in [on_render] to lay out the dynamic UI against the
    post-commit geometry. *)

val pixel_resolution : app -> (int * int) option
(** [pixel_resolution app] is the last known pixel resolution as
    [(width, height)], or [None] if the terminal has not reported one. *)

val terminal : app -> Terminal.t
(** [terminal app] is the underlying terminal handle. *)

val capabilities : app -> Terminal.capabilities
(** [capabilities app] is the current terminal capabilities. *)

(** {1:static Static output}

    These functions write to the primary screen above the renderer. They are
    ignored in [`Alt] mode. *)

val static_write : app -> rows:int -> string -> unit
(** [static_write app ~rows s] writes [s] to the static area, using [rows] as
    the exact number of terminal rows consumed. The caller is responsible for
    computing [rows] accurately (e.g. from {!Grid.active_height} after
    rendering to a grid). *)

val static_clear : app -> unit
(** [static_clear app] clears static content and resets the primary scroll
    region. *)

(** {1:scroll Scroll optimisation} *)

val set_scroll_hint : app -> Screen.scroll_hint -> unit
(** [set_scroll_hint app hint] sets a scroll hint for the current frame.
    Consumed by the next {!submit} call. Only effective in [`Alt] mode; the
    hint is silently discarded in [`Primary] mode.

    Use this when a scrollable container's viewport shifts: the renderer
    applies DECSTBM hardware scroll so only the newly-revealed edge rows
    need cell-level diffing. Without the hint, scrolling rewrites every row
    in the viewport. *)

(** {1:cursor Cursor control} *)

val set_cursor : ?visible:bool -> ?style:Terminal.cursor_style -> app -> unit
(** [set_cursor ?visible ?style app] updates cursor visibility and/or style. *)

val set_cursor_style :
  app -> style:Terminal.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets cursor shape and blink. *)

val set_cursor_position : app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] moves the cursor to 1-based coordinates
    within the dynamic render region. *)

val set_cursor_color : app -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color app ~r ~g ~b ~a] sets the cursor color. Components are in
    \[[0.];[1.]\]. *)

(** {1:diagnostics Diagnostics} *)

val set_debug_overlay :
  ?corner:debug_overlay_corner -> app -> enabled:bool -> unit
(** [set_debug_overlay app ~enabled] shows or hides the debug overlay. [corner]
    defaults to the value given to {!create}. *)

val toggle_debug_overlay : ?corner:debug_overlay_corner -> app -> unit
(** [toggle_debug_overlay app] flips overlay visibility. *)

val configure_frame_dump :
  ?every:int -> ?dir:string -> ?pattern:string -> ?hits:bool -> app -> unit
(** [configure_frame_dump app] updates the periodic frame-dump schedule. See the
    diagnostics parameters of {!create}. *)

val dump_frame : ?hits:bool -> ?dir:string -> ?pattern:string -> app -> unit
(** [dump_frame app] writes the current frame to disk immediately. *)

(** {1:unix Unix utilities} *)

val install_signal_handlers : unit -> unit
(** [install_signal_handlers ()] installs shutdown handlers for SIGTERM, SIGINT,
    SIGQUIT, and SIGABRT. Idempotent. *)

(** {1:custom Custom backends}

    {!attach} creates an application wired to caller-provided I/O callbacks
    instead of Unix. Use this in tests or with alternative runtimes (e.g. Eio).
*)

val attach :
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
  ?min_tui_height:int ->
  ?start_idle:bool ->
  write_output:(bytes -> int -> int -> unit) ->
  now:(unit -> float) ->
  wake:(unit -> unit) ->
  terminal_size:(unit -> int * int) ->
  set_raw_mode:(bool -> unit) ->
  flush_input:(unit -> unit) ->
  read_events:(timeout:float option -> on_event:(Input.t -> unit) -> unit) ->
  query_cursor_position:(timeout:float -> (int * int) option) ->
  cleanup:(unit -> unit) ->
  parser:Input.Parser.t ->
  terminal:Terminal.t ->
  width:int ->
  height:int ->
  ?render_offset:int ->
  ?static_needs_newline:bool ->
  unit ->
  app
(** [attach ... ()] is like {!create} but wired to caller-provided I/O
    callbacks.

    The optional parameters mirror {!create}. The required callbacks are:
    - [write_output] writes [len] bytes from [buf] at [off].
    - [now] returns the current monotonic time in seconds.
    - [wake] signals the event loop to re-check state.
    - [terminal_size] returns [(cols, rows)].
    - [set_raw_mode] toggles raw mode.
    - [flush_input] discards pending input.
    - [read_events] blocks for events up to [timeout], invoking [on_event] for
      each.
    - [query_cursor_position] queries cursor position with the given timeout.
    - [cleanup] releases resources on {!close}.

    [parser] is the input parser, [terminal] the protocol handle, and
    [width]/[height] the initial dimensions.

    [render_offset] defaults to [0]. [static_needs_newline] defaults to [false].
*)
