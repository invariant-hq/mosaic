(** The runtime of a terminal application.

    [Matrix] holds a terminal while an application runs. It sets the terminal
    up, decodes its input, keeps time, and presents the frames the application
    draws on a {!Grid.t}, writing only the cells that changed since the frame
    before.

    A {e run} ({!run}) holds one terminal for the scope of a function and gives
    it back however that function ends. Inside it, the application asks for the
    next thing to do and does it. Each answer is a {e step} ({!type-step}): an
    input event, a timer that came due, a message another thread posted, or a
    frame to draw. The runtime decides the order and the timing of the steps,
    and the application decides what each one means. {!next} waits for the next
    step. A program that waits with an event loop of its own takes the steps
    with {!val-step}, which never waits, and waits for what {!fds} and
    {!deadline} say ({{!loop}The loop}).

    A run draws on the alternate screen, which it owns whole, or in [`Primary]
    mode on the normal screen, beside the shell ({!type-mode}). There the
    screen's rows are a {e transcript}, the shell's output and then the
    {e static} rows the application writes ({{!static}Static output}), above a
    {e live region} that the application redraws in each frame and that ends at
    the terminal's last row. A transcript row leaves the screen only by
    scrolling into the terminal's scrollback.

    The runtime asks the terminal what it can do and where its cursor is, and
    reads the answers among the input. The time the terminal took to answer the
    first batch of queries, the {e startup probe}, is its {e round trip}. The
    runtime waits for an answer at most max(500 ms, four round trips), the
    {e answer wait}, and then goes on without it.

    The life cycle of a run:
    + {!run} takes the terminal and calls the application's function with the
      run's app.
    + The application calls {!next} and acts on each step. It handles an event,
      a timer or a message, and asks for a frame ({!request_redraw}) when what
      it shows has changed.
    + At a {!Frame} step it draws the frame ({!draw}).
    + It returns when it is done, or at the {!Stopped} step, and {!run} gives
      the terminal back.

    Unless a function's doc says otherwise, a function that takes an app must be
    called on the thread that called {!run}, and raises [Invalid_argument] on
    another ({!type-app}). Times are integer nanoseconds ({{!time}Time}), and a
    grid's rows and columns count from 0.

    This program counts the keys pressed and shows the count. It asks for a
    frame after each key, and ends at Escape or Ctrl+C, which raw mode makes a
    key ({!val-config}'s [raw_mode]):
    {[
    let () =
      Matrix.run @@ fun app ->
      let rec loop count =
        match Matrix.next app with
        | Stopped | Event (Key { key = Escape; _ }) -> ()
        | Event (Key k) when Matrix.Input.Keystroke.is_interrupt k -> ()
        | Event (Key _) ->
            Matrix.request_redraw app;
            loop (count + 1)
        | Frame ->
            Matrix.draw app (fun grid ->
                Matrix.Grid.draw_text grid ~x:0 ~y:0
                  ~text:(Printf.sprintf "%d keys" count));
            loop count
        | Event _ | Timer _ | Posted _ -> loop count
      in
      loop 0
    ]} *)

(** {1:sub Sub-libraries} *)

module Ansi = Ansi
(** ANSI escape sequence generation. *)

module Grid = Matrix_grid
(** Cell-based drawing grid. *)

module Input = Matrix_input
(** Terminal input decoding. *)

module Screen = Matrix_screen
(** Double-buffered frame rendering. *)

module Terminal = Matrix_terminal
(** Terminal protocol state machine. *)

module Terminfo = Matrix_terminfo
(** Terminal descriptions from the terminfo database. *)

module Text = Matrix_text
(** Terminal text measurement and segmentation. *)

module Tty = Tty
(** The terminal device and the signals it sends. *)

(** {1:types Types} *)

type kitty_keyboard = [ `Auto | `Off ]
(** The type for kitty keyboard protocol configurations.
    - [`Auto] uses the protocol once the terminal answers the protocol's query,
      or from the start when {!val-config}'s [caps] says the terminal has it.
      The run pushes the flags that disambiguate keys and report alternate keys,
      or every flag with [key_releases], then asks for the flags again so that
      {!capabilities} says which hold. Without the protocol the run sets xterm's
      modifyOtherKeys, so that chords such as Ctrl+Alt with a letter stay
      distinct.
    - [`Off] never uses the protocol, and sets modifyOtherKeys. *)

type mode = [ `Alt | `Primary ]
(** The type for presentation modes.
    - [`Alt] draws on the alternate screen, which the run owns whole. The
      shell's screen comes back when the run gives the terminal back.
    - [`Primary] draws the live region on the normal screen, below the
      transcript. When the input is a terminal, the live region starts on the
      cursor's row as the run begins, or on the row below it when the cursor is
      past the first column. After a resize or a {!release}, the runtime finds
      the live region again from a cursor position report, since a terminal
      moves its rows as it resizes. *)

(** {1:events Events} *)

(** The type for what the terminal says to the application, decoded by
    {!Input.Decoder}. A reply to one of the runtime's own queries is the
    runtime's. It reaches the application only as the fact it carries
    ({!Color_scheme}), or when it comes late, among the replies to the
    application's own query ({!query}). *)
type event =
  | Key of Input.Keystroke.t  (** A key pressed or auto-repeated. *)
  | Key_release of Input.Keystroke.release
      (** A key released, reported only when {!val-config}'s [key_releases]
          asks. *)
  | Text of string
      (** Text the terminal reports without a key, such as text an input method
          composed. *)
  | Mouse of Input.Mouse_report.t
      (** A mouse report. In [`Primary] mode its row counts from the live
          region's first row, and is [-1] above the live region. *)
  | Paste_start  (** A bracketed paste begins. *)
  | Paste_data of string
      (** Content of the paste, exactly as sent, in pieces as it arrives. The
          runtime keeps none of it and sets no limit on its length. *)
  | Paste_end of [ `Complete | `Cut ]
      (** The paste ends at its end marker ([`Complete]), or is cut by the end
          of the input, {!abort_paste} or {!release}. *)
  | Focus_in  (** The terminal window gained focus. *)
  | Focus_out  (** The terminal window lost focus. *)
  | Resize of int * int
      (** The usable size ({!size}) changed to [(cols, rows)], the terminal's in
          [`Alt] mode and the live region's in [`Primary] mode. It comes just
          before the {!Frame} drawn at that size, so a storm of resizes is one
          [Resize]. The terminal reports its size by the window-size signal of
          the process's controlling terminal or by its own report (mode 2048),
          and the runtime applies the latest report when it next draws. In
          [`Primary] mode the live region also changes as a frame writes the
          static rows queued or requires rows ({!draw}). That change comes as a
          [Resize] before the frame after the one that made it. *)
  | Color_scheme of [ `Dark | `Light ]
      (** The terminal's color scheme, in reply to a query or on a change. *)
  | Invalid of string
      (** Bytes that are not UTF-8, one ill-formed subsequence. *)
  | Reply of Input.Reply.t
      (** A reply to the application's own queries, sent with {!query}. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event] formats an event for debugging. *)

(** {1:stopping Stop requests} *)

(** Ending a run from another thread, domain or fiber.

    A {e stop request} is made before the run it stops and given to {!run}, so
    that a stop requested before the run has begun, when nothing could {!post}
    to it yet, is not lost. Once the stop is requested, every step of the run is
    {!Stopped}, and the events decoded and not yet returned are never returned.
    The request wakes a run that waits for its descriptors, and a run given a
    request already made returns {!Stopped} from its first step. The application
    ends the run by returning, as at the end of its input. *)
module Stop : sig
  type t
  (** The type for stop requests. *)

  val create : unit -> t
  (** [create ()] is a stop request not yet requested. *)

  val request : t -> unit
  (** [request s] requests the stop of every run [s] is given to. Each returns
      {!Stopped} from its next step on. A run that is still taking its terminal
      sees the request at its first step, once the terminal is taken, which
      takes at most an answer wait for each batch of queries the run waits for
      ({!run}). Any thread, domain or fiber may call it. Requesting it again
      does nothing. *)
end

(** {1:running Running}

    A run takes the terminal whose input and output descriptors are given to
    {!run}, by default the process's. A process may hold its controlling
    terminal ({!Tty.is_controlling}) in one run at a time, and runs on other
    terminals, each in its domain, run side by side. A {e served session} is a
    terminal the process runs for a client elsewhere, such as a remote shell's
    or a web page's. It runs on the slave side of a pseudo-terminal the program
    opened, with the client's environment as [env], while the program copies
    bytes between the master side and the client. The runtime queries only a
    terminal, so a socket in its place would leave it guessing. A terminal that
    is not the process's controlling one sends the process no window signal, so
    the program writes the client's size to the master in band,
    [CSI 48 ; rows ; cols ; height ; width t] as mode 2048 reports it, before
    the run and on every change. An output that is not a terminal, such as a
    pipe, has no window either. The run then keeps the size last reported in
    band on its input, 80 by 24 before any. *)

type config
(** The type for what a run sets up. *)

val config :
  ?mode:mode ->
  ?raw_mode:bool ->
  ?mouse:Terminal.mouse_mode ->
  ?bracketed_paste:bool ->
  ?kitty_keyboard:kitty_keyboard ->
  ?key_releases:bool ->
  ?fps:float ->
  ?explicit_width:bool ->
  ?min_tui_height:int ->
  ?caps:Terminal.capabilities ->
  unit ->
  config
(** [config ()] is the configuration of a run.

    {b Display:}
    - [mode] presentation mode. Defaults to [`Alt].
    - [raw_mode] whether to switch the input into raw mode. Defaults to [true].
      In raw mode the terminal sends its interrupt and suspend characters as
      keys, so Ctrl+C is a {!Key}, and the application decides what it means.
      Without raw mode the terminal's settings stay as they are, so in the usual
      cooked mode input comes a line at a time and Ctrl+C sends SIGINT
      ({{!signals}Signals}). The runtime then sends no query, and uses the kitty
      keyboard protocol only when [caps] says the terminal has it. With the
      protocol the terminal sends Ctrl+C as a key sequence even then, which
      cooked mode holds until a line ends, as it holds all input.
    - [explicit_width] whether to measure text with the built-in wcwidth tables
      instead of asking the terminal how it measures text. Defaults to [false].
    - [min_tui_height] the fewest rows the live region keeps in [`Primary] mode,
      as static rows are written and the terminal shrinks. Defaults to [1]. It
      holds between frames: {!size}, {!effective_size} and each {!Resize} count
      it. A frame's [primary_required_rows] ({!draw}) also gives the live region
      rows, but only from that frame on.

    {b Frames:}
    - [fps] the most frames per second. Defaults to [60.]. The
      {e frame interval} is [1 / fps] seconds. 60 is the refresh rate of most
      displays.

    {b Input:}
    - [mouse] mouse tracking. Defaults to [`Sgr_any]. [`Off] leaves the mouse to
      the terminal.
    - [bracketed_paste] whether to enable bracketed paste. Defaults to [true].
    - [kitty_keyboard] kitty keyboard protocol configuration. Defaults to
      [`Auto].
    - [key_releases] whether to ask the kitty keyboard protocol for key releases
      ({!Key_release}), which also has every key reported as a sequence.
      Defaults to [false].
    - [caps] the capabilities to assume where the terminal does not answer, in
      place of what the environment says ({!Terminal.Caps.assume}). By default
      {!run}'s [env] is read when the run begins.

    Focus reports (mode 1004), color scheme reports (2031) and resize reports
    (2048) are always set, without asking whether the terminal has them, since
    the terminals checked ignore a mode they do not know. The runtime resets
    them when it gives the terminal back.

    The application draws everything its frames show, except the panel
    [MATRIX_DEBUG] has the run draw ({{!diagnostics}Diagnostics}), and sets the
    cursor's visibility ({!set_cursor}).

    Raises [Invalid_argument] if [fps] is not positive and finite. *)

type 'msg app
(** The type for running applications whose other threads send them messages of
    type ['msg] ({!post}). An [app] exists only inside {!run}, and belongs to
    the thread that called {!run}. Every function of this module that takes one
    raises [Invalid_argument] on another thread, except {!post}. *)

val run :
  ?input:Unix.file_descr ->
  ?output:Unix.file_descr ->
  ?env:Terminal.Caps.env ->
  ?stop:Stop.t ->
  ?config:config ->
  ('msg app -> 'a) ->
  'a
(** [run f] takes the terminal whose input is [input] and whose output is
    [output], calls [f app], and gives the terminal back when [f] returns or
    raises. [input] defaults to [Unix.stdin] and [output] to [Unix.stdout].
    [env] is the environment of the session the terminal runs, from which the
    runtime guesses what the terminal can do until it answers. It defaults to
    the process's, with the description the terminfo database has for its [TERM]
    ({!Tty.terminfo}). [stop] ends the run from elsewhere ({!Stop}).

    The run takes the terminal, and gives it back, as
    {{!lifecycle}Taking and giving back the terminal} says. It draws and dumps
    frames itself as the environment variable [MATRIX_DEBUG] says
    ({{!diagnostics}Diagnostics}).

    Raises what [f] raises, and otherwise the first exception raised while
    giving the terminal back. Raises [Invalid_argument] if [MATRIX_DEBUG] names
    something other than a tool, or if the terminal is the process's controlling
    terminal and another run holds it, unless that run gave it away for
    {!release}. Raises [Failure] on Windows, where no run starts
    ({!Tty.Signal.hold}), and [Unix.Unix_error] if the run's wake pipe cannot be
    made. *)

(** {1:loop The loop}

    The application takes the run's work one step at a time. {!val-step} gives
    the next step at the time the caller gives it, and never waits for one. When
    the run has nothing to do it returns [None], and the caller waits until one
    of the descriptors {!fds} is readable or the time {!deadline} comes, then
    asks again. {!next} is that loop, waiting with [select(2)]. A program that
    waits with an event loop of its own, such as Eio's, calls {!val-step} and
    waits with that loop instead. With nothing to wait for, {!deadline} is
    [None], so an idle application costs no wakeups.

    Steps come from {e turns}. A turn begins at the first step after [None], and
    at the step after the work of the previous turn is done. A turn:

    + takes the time of the step that began it as its own ({!now});
    + acts on the signals received;
    + reads the input once, without waiting, and decodes it, unless an event
      decoded earlier is still waiting, which leaves the rest in the operating
      system's buffer;
    + takes the messages posted since the last turn;
    + returns work, one step at a time. First comes one step of each kind that
      has some, in this order: an input {!Event}, a {!Timer} that came due, a
      {!Posted} message. Then come the events, then the timers, then the posted
      messages, until none is left or one frame interval has passed between the
      turn's time and the latest step's. So a flood of one kind delays the
      others by at most a frame interval, and within a kind everything comes in
      order;
    + returns {!Stopped} once the input has ended and every event decoded before
      its end has been returned;
    + returns a {!Frame} if one was asked for ({!request_redraw}), one frame
      interval has passed since the last frame by the latest step's time (the
      first frame waits for no interval), and the runtime waits for no answer
      from the terminal (the place of the live region after a resize or a
      release, for at most an answer wait). Work that took the turn's budget may
      have taken the interval with it. Before the frame, if the usable size
      changed, comes a {!Resize}.

    Each step is done before the next is asked for, so the application handles
    one event at a time, and an event sees what the previous one did. A turn
    orders its steps by kind first. A key read in a turn comes before a timer
    that came due in the same turn, even if the timer's deadline was earlier.
    The input is never held for an answer from the terminal, and only frames
    are. *)

type timer = private int
(** The type for timers, set with {!at}. Two timers set in one run differ. *)

(** The type for the steps of a run. *)
type 'msg step =
  | Event of event  (** An input event, in the order the terminal sent them. *)
  | Timer of timer  (** A timer whose deadline the turn's time reached. *)
  | Posted of 'msg  (** A message another thread sent with {!post}. *)
  | Frame
      (** Draw a frame, with {!draw}. A frame not drawn by the next step is
          dropped. *)
  | Stopped
      (** The input ended, and every event decoded before its end has been
          returned; or the run's stop was requested ({!Stop}). Every later step
          is [Stopped]. *)

val step : 'msg app -> now:int -> 'msg step option
(** [step app ~now] is the next step of the run at the time [now], or [None]
    when the run has nothing to do until one of {!fds} is readable or the time
    {!deadline} comes. It looks at the input without waiting, and reads and
    decodes what is there. It never waits for input, a post or a time. It blocks
    while the terminal takes no output, and while the process is stopped by
    SIGTSTP ({{!signals}Signals}). [now] must be on the run's clock, and never
    earlier than the time of the step before ({{!time}Time}).

    Raises [Invalid_argument] if [now] is earlier than the time of the step
    before. *)

val fds : 'msg app -> Unix.file_descr list
(** [fds app] is the descriptors the run waits for. Once one of them is
    readable, {!val-step} has something to do. They are the input while it has
    not ended, and the run's wake, which {!post} and the signal handlers write,
    and they may differ from one wait to the next. A caller must wait for them
    to be readable and must never read them, since what it reads the run never
    sees. *)

val deadline : 'msg app -> int option
(** [deadline app] is the time by which {!val-step} must be called again after
    it returned [None], if no descriptor of {!fds} has become readable first, or
    [None] when only those descriptors can give the run something to do. Right
    after [step app ~now] returned [None], it is [None] or later than [now]. It
    is computed when called, so a timer set ({!at}) or a message posted
    ({!post}) since moves it, and a message posted makes it the turn's time
    ({!now}). *)

val clock : unit -> int
(** [clock ()] is the run's own clock, {!Tty.now}, in nanoseconds. *)

val next : 'msg app -> 'msg step
(** [next app] is the next step of the run, waiting for one: {!val-step} at
    {!clock}'s time, waiting with [select(2)] whenever it returns [None]. It
    behaves as this loop does, but allocates nothing while the run is idle:
    {[
    let rec next app =
      match Matrix.step app ~now:(Matrix.clock ()) with
      | Some step -> step
      | None ->
          let timeout =
            match Matrix.deadline app with
            | None -> -1.
            | Some d -> Float.max 0. (Float.of_int (d - Matrix.clock ()) *. 1e-9)
          in
          (try ignore (Unix.select (Matrix.fds app) [] [] timeout)
           with Unix.Unix_error (EINTR, _, _) -> ());
          next app
    ]} *)

(** {1:time Time}

    Times are integer nanoseconds from an arbitrary origin, on a clock that
    never goes back, so a deadline computed as [origin + n * interval] is exact
    and two computations of it are equal. A run's time is the time of its steps.
    Timers compare with the time of the turn ({!now}), and a turn's budget and
    the frame interval are measured to the time of the latest step. A step is a
    function of the run's state, its input and that time alone.

    The times given to {!val-step} must never decrease, and must be on the run's
    clock, {!clock}, which {!next} reads, and which the run reads itself as it
    takes the terminal, releases it and gives it back. A time from another
    clock, such as [Unix.gettimeofday], makes the run's deadlines meaningless. A
    test that scripts time runs on the emulated terminal of [Matrix_test]
    (library [matrix.test]), whose clock the run takes as its own. *)

val now : 'msg app -> int
(** [now app] is the time of the current turn, in nanoseconds. *)

val at : 'msg app -> int -> timer
(** [at app time] is a timer that the run returns as {!Timer} in the first turn
    whose time is at least [time], the current turn included. A deadline is a
    lower bound, so the timer may come late and never comes early. Timers due in
    one turn come in deadline order, then in the order they were set. *)

val cancel : 'msg app -> timer -> unit
(** [cancel app timer] keeps [timer] from being returned. Cancelling a timer
    that was returned or cancelled does nothing. *)

(** {1:frames Frames} *)

val request_redraw : 'msg app -> unit
(** [request_redraw app] asks for a frame. Requests until that frame is drawn
    are one request. Frames are at least a frame interval apart ({!val-config}'s
    [fps]). A frame asked for once that interval has passed since the last one
    comes in the turn that asked, and one asked for sooner comes once the
    interval has passed. A request made from a {!Frame} step until the next step
    asks for the next frame, so an animation asks again from each frame, and
    stops by not asking.

    The runtime asks for frames itself: as the run begins, after a resize, a
    {!release} or a SIGCONT, when the terminal's capabilities change, and when
    static rows are queued ({!static_write}). *)

val frame_dt : 'msg app -> float
(** [frame_dt app] is the time in seconds from the frame before the latest
    {!Frame} to that frame, measured between the times of the steps that
    returned them. It is [0.] in the run's first frame. An animation moves by it
    in each frame. One that stopped asking for frames sees the whole pause in
    the first frame it asks for again. *)

val draw : 'msg app -> ?primary_required_rows:int -> (Grid.t -> unit) -> unit
(** [draw app f] draws the frame the last step returned. It lays the frame out
    at {!effective_size}, the usable size once the static rows queued are
    written, calls [f] with the frame's grid, cleared, and presents the frame.
    Presenting writes the terminal operations of the layout and the cells that
    differ from the frame presented before, in one write, and writes nothing
    when nothing changed.

    In [`Primary] mode the static rows queued go to the transcript first, in the
    same write, and the live region grows to [primary_required_rows] rows when
    it has fewer, at most the terminal's height, by scrolling the rows above it
    into the terminal's scrollback. The frame is as tall as the live region, so
    it shows the rows it asked for. [primary_required_rows] is ignored in [`Alt]
    mode.

    A frame drawn across a {!release} is not presented, and the next one is
    drawn whole.

    Raises [Invalid_argument] if no {!Frame} is being drawn, and what
    {!dump_frame} raises when [MATRIX_DEBUG] names [frames]
    ({{!diagnostics}Diagnostics}). *)

val current_grid : 'msg app -> Grid.t
(** [current_grid app] is the grid of the frame presented last, blank before the
    first. The application must not change it, since the next frame is diffed
    against it. *)

val hits : 'msg app -> Screen.Hit_grid.t
(** [hits app] is the hit grid of the frame being drawn ({!Screen.Hit_grid}). *)

val screen : 'msg app -> Screen.t
(** [screen app] is the runtime's screen. The runtime sizes and presents it:
    {!draw} clears and resizes it before calling its function, and diffs and
    swaps it after. An embedding renderer builds its frames into it
    ({!Screen.build}) from the function given to {!draw}, so that the runtime
    presents its output with one diff. The application must not call
    {!Screen.render} on it. *)

(** {1:threads Threads} *)

val post : 'msg app -> 'msg -> unit
(** [post app msg] has the run return [Posted msg] in a later turn. It is the
    one function of this module that any thread, domain or fiber may call, and
    it never blocks and never fails. Posts come in the order they were made,
    those of one thread in program order. The queue has no bound, so a producer
    that posts faster than the loop handles what it posts grows memory, and
    collecting that memory slows the loop. After the run ends, [post] does
    nothing. *)

(** {1:lifecycle Taking and giving back the terminal}

    Taking the terminal is a sequence of steps, each of which pushes its undo:
    the signals ({{!signals}Signals}), raw mode, the startup probe, the screen
    (the alternate screen, or in [`Primary] mode the live region, found from a
    cursor report), the measure of how the terminal sizes text
    ({!Terminal.Caps.Widths}), and the modes. The probe and the cursor report
    each take at most an answer wait, and input that comes meanwhile is kept and
    returned first by {!next}. Only a terminal that answered the probe is
    measured, on cells the run owns and then erases: the alternate screen's
    second row, or the live region's first, which then has at least two rows,
    the height of text drawn at scale 2 by kitty's text-sizing protocol. If a
    step raises, the undos made so far run in reverse and the exception
    propagates with its backtrace.

    Giving the terminal back runs every undo, each even if another raises. When
    a reply is owed, it first waits at most an answer wait for the terminal to
    answer, so that no reply reaches the shell as typing. Then the modes are
    reset (mouse tracking, bracketed paste, the keyboard protocols, and focus,
    color scheme and resize reports), the cursor shown, the screen left (in
    [`Primary] mode the live region is erased below the transcript), cooked mode
    restored, and the input not read discarded.

    A run gives its terminal back on every way out: when the function given to
    {!run} returns or raises, for a program run in its place ({!release}), on a
    signal that stops or ends the process ({{!signals}Signals}), and on the
    process's exit path. If the process exits while a run holds its terminal, as
    when that function calls [exit], or when the program's main domain returns
    while a run on another domain is still taking its terminal or giving it
    back, the exit path restores the terminal as an abort does. It writes the
    bytes that reset the modes the run set and puts back the terminal's saved
    settings. A terminal given back already is left alone. *)

val release : 'msg app -> (unit -> 'a) -> 'a
(** [release app f] gives the terminal back for [f ()], as at the end of the
    run, and takes it again when [f] returns or raises, as at its start. [f] can
    run another program in the terminal, as POSIX's [system()] does for its
    caller. The loop does not run during [f], so nothing reads the terminal, and
    timers and posts wait for [f] to return.

    Before [f], the static rows queued are written first. Input not yet read
    stays for [f], and in [`Primary] mode the cursor waits at the start of the
    erased live region, below the transcript, where [f]'s output goes on.

    The events decoded and not yet returned, and those decoded while waiting for
    the replies owed, are dropped, but for {!Reply}, and a paste in progress is
    cut. A turn decodes all it reads before it returns a step, so this drops the
    keys read in the turn of the step that called [release], whatever that step
    was. Those keys were typed after the event that called [release], or at a
    time that cannot be ordered against the timer or the post that did, so they
    count as typed for [f].

    During [f], SIGTSTP, SIGTERM and SIGHUP have their default actions, so the
    terminal's suspend character stops [f]'s program and the runtime with it.
    SIGINT and SIGQUIT, which the terminal sends to [f]'s program as well, are
    caught, and discarded when [f] returns.

    After [f], the terminal is taken again. Input left unread is discarded, the
    terminal's settings are kept as the ones to restore, the size is read, the
    screen and modes are set again, and the next frame is drawn whole, in
    [`Primary] mode below what [f] wrote, once a cursor report has said where
    that ends. A frame being drawn when [release] is called is not presented.

    Raises what [f] raises, and [Invalid_argument] if called from the [f] of
    another [release] of the same run. *)

val stop_job : 'msg app -> unit
(** [stop_job app] stops the job as the terminal's suspend character does in
    cooked mode. It {!release}s the terminal around sending SIGTSTP to the
    process's group, and returns once the job is continued. Raw mode turns the
    suspend character into a key, so an application that wants Ctrl+Z to suspend
    calls [stop_job] on it. Nothing happens if SIGTSTP was ignored when the run
    began, as nothing could continue the job, or if the run's terminal is not
    the process's controlling terminal ({!Tty.is_controlling}), since the job is
    not that terminal's and stopping it would stop every other session. *)

(** {1:signals Signals}

    A run holds its terminal's signals as {!Tty.Signal} says every holder of a
    terminal in the process does. A run on any terminal takes the signals that
    end the process, SIGINT, SIGTERM, SIGHUP, SIGQUIT and SIGABRT, and a run on
    the process's controlling terminal ({!Tty.is_controlling}) also takes
    SIGTSTP, SIGWINCH and SIGCONT. All but SIGWINCH and SIGCONT are taken only
    if their action is the default when the run begins, as X/Open Curses allows,
    so an application that installed its own action keeps it. The handlers
    record the signal for each run it concerns and wake it (its wake is among
    {!fds}), and the loop acts on it in its next turn. Runs side by side take a
    signal together, and the action it had comes back when the last of them
    ends.

    - SIGINT, SIGTERM, SIGHUP and SIGQUIT end the process, and every run with
      it. The run whose loop acts on the signal first gives its terminal back,
      as when it ends. Every other run's terminal is restored as an abort
      restores it (below), its reset bytes written and its saved settings put
      back. Then the signal is raised again with its default action, so the
      process ends with that signal's status and {!run} does not return. An
      application that wants {!run} to return installs its own handler first. A
      write that the terminal is not taking stops when one of them comes, with
      the rest unwritten.
    - SIGTSTP releases the terminal and stops the process, as {!stop_job} does
      for the whole job. On continuing, the terminal is taken again.
    - A SIGCONT the run did not cause, as when the shell continued the job after
      a stop from elsewhere, sets the terminal again, since the shell has put
      back its own settings. The size is read and the next frame drawn whole.
    - SIGWINCH has the size read when the next frame is drawn.
    - SIGABRT, from [abort()], returns to no point where OCaml can run, so its
      handler restores every run's terminal itself. It writes the bytes that
      reset the modes the run set to the run's terminal, puts back its saved
      settings, and the process aborts. *)

(** {1:pastes Pastes} *)

val abort_paste : 'msg app -> unit
(** [abort_paste app] ends the bracketed paste in progress, if any, with
    [Paste_end `Cut]. The bytes after it decode as input again. A paste goes on
    until its end marker, so after a terminal that stopped sending mid-paste,
    every later byte would be the paste's content. An application that will wait
    no longer for the marker calls [abort_paste]. *)

(** {1:queries Queries}

    A terminal answers a query with a reply among its input. The runtime sends
    its queries, and the application's, in {e batches}. A batch is {e owed} from
    when it is sent until the terminal has answered it or, from a terminal that
    never does, for max(5 s, four round trips). While a batch is owed the
    decoder reads its replies as replies, and a reply that comes after that
    decodes as input. A batch stays owed after the runtime has stopped waiting
    for its answer.

    An application's query sent other than by {!query}, such as with
    {!Terminal.send}, is not owed. Its reply is never a {!Reply}, and one that
    is a control string, or a cursor report from the first row, reaches the
    application as input. The replies {!query} delivers are those the terminal
    sends after it has answered the queries sent before, so they may include the
    late answer to a query the runtime itself sent. A report the terminal sends
    on its own that is a fact for the application comes as its event
    ({!Color_scheme}). *)

val query : 'msg app -> owed:Input.Decoder.owed -> string -> unit
(** [query app ~owed q] sends the queries [q] to the terminal, followed by a
    primary device attributes query (DA1), and delivers their replies as
    {!Reply} events, ending with the DA1 reply. [owed] says which of the replies
    [q] asks for are cursor position reports or control strings (DCS, OSC and
    APC replies), as {!Input.Decoder.expect} counts them. The decoder must know
    that they are coming, or a string reply such as XTVERSION's
    [DCS > | text ST] decodes as Alt+P and typed text, and a cursor report
    [CSI 1 ; m R] as F3. A terminal answers in order and answers DA1, so its DA1
    reply says that [q] has been answered, or never will be, as for the
    runtime's own queries. Every DA1 reply is alike, so one batch that ends with
    DA1 is in flight at a time, and [q] is sent once the terminal has answered
    the batches sent before it, or they have been given up.

    Nothing is sent, and no {!Reply} comes, when the input is not a terminal in
    raw mode, or it has ended, since no reply could come. *)

(** {1:terminal The terminal} *)

val mode : 'msg app -> mode
(** [mode app] is the presentation mode of the run. *)

val size : 'msg app -> int * int
(** [size app] is the usable size [(cols, rows)], the terminal's in [`Alt] mode
    and the live region's in [`Primary] mode. *)

val full_size : 'msg app -> int * int
(** [full_size app] is the terminal's size [(cols, rows)], which is {!size} in
    [`Alt] mode. *)

val effective_size : 'msg app -> int * int
(** [effective_size app] is the usable size [(cols, rows)] that {!draw} will lay
    the next frame out at, once it has written the static rows queued and before
    it grows the live region to the rows the frame requires. It is {!size} in
    [`Alt] mode, when no static row is queued, and so in the function given to
    {!draw}. *)

val pixel_resolution : 'msg app -> (int * int) option
(** [pixel_resolution app] is the size in pixels [(width, height)] of the
    terminal's text area, as the terminal last reported it, or [None] before any
    report. *)

val cell_size : 'msg app -> (int * int) option
(** [cell_size app] is the size in pixels [(width, height)] of one of the
    terminal's cells, as it answered [CSI 16 t], or else its text area's size
    ({!pixel_resolution}) or its window's size as the system reports it, divided
    by its columns and rows. It is [None] when none is known. *)

val terminal : 'msg app -> Terminal.t
(** [terminal app] is the terminal handle the run writes through. *)

val capabilities : 'msg app -> Terminal.capabilities
(** [capabilities app] is what the run knows the terminal can do, which changes
    as the terminal answers. *)

(** {1:static Static output}

    In [`Primary] mode the application adds rows to the transcript, above the
    live region, with these functions. The rows go out with the next frame
    {!draw} lays out, which they ask for. They take their rows from the live
    region, which moves down until it has its fewest rows ({!val-config}'s
    [min_tui_height]). After that the rows above the live region scroll into the
    terminal's scrollback, and the live region keeps its height. Only
    {!static_replace} and {!static_clear} erase transcript rows. In [`Alt] mode
    these functions do nothing and raise nothing. *)

val static_write :
  ?preserve_live_region:bool -> ?rows:int -> 'msg app -> Grid.t -> unit
(** [static_write ~preserve_live_region ~rows app grid] queues the first [rows]
    rows of [grid] for the transcript, every row by default, blank rows
    included. It copies them immediately, so later changes to [grid] do not
    change them. Each row replaces the whole terminal row and ends with CRLF. A
    row never wraps. In a terminal that narrowed before the row is written, its
    cells past the right margin overwrite the last column, as a terminal's right
    margin makes them. Picture cells are written in the terminal's form of
    pictures ({!Screen.section-pictures}), under sixel with a sixel over the
    rows still on the screen. A row queued from the function given to {!draw}
    goes out with the frame after that one. [rows = 0] queues nothing.

    With [preserve_live_region] [true], the live region keeps its height, and
    the transcript, these rows included, scrolls up into the scrollback to make
    room. Otherwise, by default, the rows take theirs from the live region, down
    to [min_tui_height].

    Raises [Invalid_argument], in [`Primary] mode, if [grid] is wider than the
    terminal, or [rows] is negative or greater than the height of [grid]. *)

val static_replace :
  ?preserve_live_region:bool -> ?rows:int -> 'msg app -> Grid.t -> unit
(** [static_replace ~preserve_live_region ~rows app grid] queues the replacement
    of the whole transcript with the first [rows] rows of [grid], taken as
    {!static_write} takes them. [rows = 0] empties the transcript. The
    replacement and the next frame go out in one write.

    {b Warning.} The replacement erases the terminal's scrollback ([CSI 3 J]),
    the shell's output from before the run included. A terminal without
    [CSI 3 J] keeps its scrollback.

    Raises as {!static_write} does. *)

val static_clear : 'msg app -> unit
(** [static_clear app] erases the screen, drops the static rows queued, and
    gives the live region the whole screen. It writes the erase immediately, and
    asks for the next frame. The scrollback stays. *)

(** {1:scroll Scroll hints} *)

val set_scroll_hint : 'msg app -> Screen.scroll_hint -> unit
(** [set_scroll_hint app hint] tells the frame being drawn, or the next, that a
    region of it scrolled ({!Screen.type-scroll_hint}). Presenting the frame
    then scrolls the terminal's rows (DECSTBM) and diffs only the rows the
    scroll revealed. Without a hint, a scrolled region rewrites every row it
    shows. Hints are used in [`Alt] mode and dropped in [`Primary] mode. *)

(** {1:cursor Cursor}

    The cursor's setters take effect with the next frame {!draw} presents, and
    hold until set again. A run begins with the cursor hidden. *)

val set_cursor : ?visible:bool -> ?style:Ansi.cursor_style -> 'msg app -> unit
(** [set_cursor ~visible ~style app] sets the cursor's visibility and style,
    each when given. An application that wants the cursor shown from the first
    frame sets it before that frame:
    {[
    Matrix.run @@ fun app ->
    Matrix.set_cursor app ~visible:true;
    loop app
    ]} *)

val set_cursor_style :
  'msg app -> style:Ansi.cursor_style -> blinking:bool -> unit
(** [set_cursor_style app ~style ~blinking] sets the cursor's shape and whether
    it blinks. *)

val set_cursor_position : 'msg app -> row:int -> col:int -> unit
(** [set_cursor_position app ~row ~col] places the cursor at row [row] and
    column [col] of the usable area ({!size}), counted from 1, unlike a grid's
    rows and columns. [row] is clamped to the usable rows and [col] to at least
    1. *)

val set_cursor_color : 'msg app -> r:float -> g:float -> b:float -> unit
(** [set_cursor_color app ~r ~g ~b] sets the cursor's color. Each component is
    clamped to \[[0.];[1.]\]. *)

(** {1:diagnostics Diagnostics}

    {!Debug_overlay} draws the frames' timing and the collector's statistics in
    a corner of a frame, and {!dump_frame} writes a frame to files. An
    application uses them in the frames it draws. A run on descriptors also uses
    them itself when the environment variable [MATRIX_DEBUG], read as the run
    begins, names them in a list separated by commas:
    - [overlay] draws a {!Debug_overlay} over each frame, after the function
      given to {!draw}. It times the frame from the start of {!draw} to the end
      of its write. Its [app] row is the time since the frame before that the
      application spent handling steps, outside {!val-step}, {!next} and
      {!draw}. The waits for a step are left out of it.
    - [frames] or [frames=DIR] dumps each frame drawn into the directory [DIR],
      ["frames"] by default, with its hit grid ({!dump_frame}). The dump is
      taken before the overlay is drawn, so it holds the application's frame
      alone.

    An empty [MATRIX_DEBUG] names no tool. The overlay's cells are part of the
    frames the run presents and of their metrics. A run on a test's terminal,
    such as a [Matrix_test] session's, reads no [MATRIX_DEBUG], so that its
    frames depend only on the test. Any program shows the panel and keeps every
    frame it draws, its code unchanged:
    {v MATRIX_DEBUG=overlay,frames=/tmp/frames ./app.exe v}

    An application that dumps every thirtieth frame counts them:
    {[
    let frames = ref 0 in
    let on_frame app view =
      incr frames;
      Matrix.draw app (fun grid ->
          view grid;
          if !frames mod 30 = 0 then Matrix.dump_frame ~dir:"dumps" app)
    in
    on_frame
    ]}

    While a run holds the terminal, the program must write to it only through
    the run. Bytes written to the terminal by other means, such as by
    [Printf.printf] or by a library's message on standard error, land in the
    frame where the run does not know they are, and stay on the screen until the
    cells under them change. A program that prints diagnostics while it runs
    must write them to standard error, and be run with standard error redirected
    to a file, which another terminal follows:
    {v
    ./app.exe 2>app.log
    tail -f app.log
    v}
    In [`Primary] mode it can also write rows above the live region with
    {!static_write}. *)

module Debug_overlay = Debug_overlay
(** A panel of a run's frame timing and memory, drawn in a corner of a frame. *)

val dump_frame :
  ?hits:bool -> ?dir:string -> ?pattern:string -> 'msg app -> unit
(** [dump_frame app] writes the frame being drawn, as the function given to
    {!draw} has drawn it so far, to a new file of [dir] (default ["frames"],
    made if missing), named [pattern] (default ["frame_"]) followed by a number
    that grows with each dump of the process and [.ansi]. The file holds the
    frame's cells as ANSI text, a file named with [.txt] in place of [.ansi]
    holds them as plain text ({!Grid.to_text}) and, if [hits] (default [false]),
    a file named with [.hits.txt] holds its hit grid.

    Raises [Invalid_argument] if no {!Frame} is being drawn, [Failure] if [dir]
    cannot be made, and [Sys_error] if a file cannot be written. *)

(**/**)

(* The device a run drives, for the runtime's own tests, which script its
   clock, its input and what the terminal answers. [run] is [run_on] the
   device of its descriptors. A device's [wait] returns once the input is
   readable (when [reading]), once [wake] was called since the last wait, or
   once [now ()] reaches [until], and says whether the input is readable; a
   wait until a time already past only looks. [read_input] returns [0] at the
   end of the input and [-1] when nothing is there. [terminal_size] is [None]
   for an output with no window, and [window_pixels], the window's size in
   pixels, [None] when the system reports none. [fds] are the descriptors
   whose readability ends a wait. *)

module Backend : sig
  type signal = Winch | Cont | Tstp | Terminate of int

  type t = {
    now : unit -> int;
    wait : until:int option -> reading:bool -> bool;
    wake : unit -> unit;
    fds : reading:bool -> Unix.file_descr list;
    take_signals : unit -> signal list;
    read_input : bytes -> int -> int -> int;
    write_output : bytes -> int -> int -> unit;
    terminal_size : unit -> (int * int) option;
    window_pixels : unit -> (int * int) option;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    input_is_tty : bool;
    output_is_tty : bool;
    controlling : bool;
    on_fatal : string -> unit;
    cleanup : unit -> unit;
    env : Terminal.Caps.env;
  }

  val unix :
    ?input:Unix.file_descr ->
    ?output:Unix.file_descr ->
    ?env:Terminal.Caps.env ->
    unit ->
    t
end

val run_on :
  Backend.t -> ?stop:Stop.t -> ?config:config -> ('msg app -> 'a) -> 'a

(* A terminal emulated in the program's own process, for [Matrix_test], which
   a run takes through its configuration ([on_device]), and then from [run]
   with no descriptor and no environment of its own. Its writes are applied
   before [write] returns and its input is queued whole, so a run on it
   differs from one on a terminal in three ways:
   - a frame asked for outside a frame (by input, a timer, a post or the
     runtime itself, as after a resize) is drawn in the turn that asked, and
     one asked for from a Frame step until the next step, an animation's,
     waits its interval, whose grid a frame drawn early does not move;
   - input is decoded whole once nothing follows it, as the terminal has
     finished writing;
   - the run takes no signal of the process: [resized] is its window signal.
   [wait ~until] says whether input is queued. With none queued and [until]
   later than [read_clock ()], the terminal is silent until then, and the device
   moves its clock there, as a terminal that never answers leaves the run's
   wait for an answer. [read] returns [-1] when nothing is queued, [window]
   [None] for an output with no window, and [frame] is called as each frame
   begins. [next] does not wait on a device: when the run has nothing to do
   it calls [idle ~until ~stepped], [until] the run's deadline and [stepped]
   whether [next] returned a step since its last call, which returns once
   input is queued or the clock has moved, and says whether input is
   queued. As the run begins, [presented] is given a function that is the
   frame the run last presented ({!current_grid}), the terminal row its
   first row is shown on, and the form its picture cells were written in. *)

type device = {
  read_clock : unit -> int;
  wait : until:int option -> bool;
  read : bytes -> int -> int -> int;
  write : bytes -> int -> int -> unit;
  window : unit -> (int * int) option;
  resized : unit -> bool;
  set_raw : bool -> unit;
  flush : unit -> unit;
  tty : bool;
  env : Terminal.Caps.env;
  frame : unit -> unit;
  idle : until:int option -> stepped:bool -> bool;
  presented : (unit -> Grid.t * int * [ `Kitty | `Sixel | `Blocks ]) -> unit;
}

val on_device : device -> config -> config

(**/**)
