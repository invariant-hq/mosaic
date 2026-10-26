(** The terminal device, as the operating system knows it.

    A {e terminal} is a file descriptor that the system drives as one
    (isatty(3)), such as a console, a serial line or either side of a
    pseudo-terminal. The system keeps three things for a terminal, which this
    module reads and sets. They are the size of its window ({!window}), the
    settings of its {e line discipline}, the part of the system that edits what
    is typed before a program reads it ({!section-settings}), and the signals it
    sends the process ({!Signal}). The module also finds the description of a
    terminal type in the terminfo database ({!terminfo}) and has the monotonic
    clock ({!now}) by which the libraries above it time a terminal's answers.
    What to write to a terminal is decided above this library, by
    [Matrix_terminal] and [Ansi].

    A program that takes a terminal for itself, as a full-screen program does,
    goes through these steps and undoes them in reverse order:
    + {!is_tty} and {!is_controlling} tell what its descriptors are.
    + {!Signal.hold} registers the program as a {e holder} of the terminal, and
      {!Signal.take} takes each of {!Signal.signals}.
    + {!Signal.keep_settings} keeps the terminal's settings for an abort, and
      {!raw} turns the line discipline off and returns the same settings.
    + While it runs, it reads {!window} again after each SIGWINCH that
      {!Signal.received} reports.
    + {!restore} puts the settings back, {!Signal.give_back} gives back each
      signal it took, and {!Signal.release} ends the holder.

    This reads one key as it is typed, with no echo and no Enter, and puts the
    settings back however the read ends:
    {[
    let () =
      let saved = Tty.raw Unix.stdin in
      Fun.protect ~finally:(fun () -> Tty.restore Unix.stdin saved) @@ fun () ->
      let key = Bytes.create 1 in
      ignore (Unix.read Unix.stdin key 0 1 : int)
    ]} *)

(** {1:device The device} *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] is [true] iff [fd] is a terminal (isatty(3)). It is [false] for
    a closed descriptor. *)

val is_controlling : Unix.file_descr -> bool
(** [is_controlling fd] is [true] iff [fd] is the process's
    {e controlling terminal}, the terminal associated with the process's session
    (tcgetsid(3)), whose job the process runs in. The shell stops and continues
    the job through it, and its window is the one SIGWINCH reports. The master
    side of a pseudo-terminal is [false], even when its slave is the controlling
    terminal. So is a pseudo-terminal the process opened, and every descriptor
    on Windows. *)

val window : Unix.file_descr -> (int * int) option
(** [window fd] is the size of [fd]'s window, [Some (cols, rows)], as the system
    reports it ([TIOCGWINSZ], or the window of the console screen buffer on
    Windows). It is [None] when [fd] is not a terminal or the system reports a
    zero size, as it does for a pseudo-terminal whose size was never set. *)

val window_pixels : Unix.file_descr -> (int * int) option
(** [window_pixels fd] is the size of [fd]'s window in pixels,
    [Some (width, height)], as the system reports it ([TIOCGWINSZ]). It is
    [None] when [fd] is not a terminal, when the system reports a zero size, as
    many terminals and every pseudo-terminal whose pixel size was never set do,
    and on Windows. *)

val readable : Unix.file_descr -> bool
(** [readable fd] is [true] iff a read from [fd] would return without waiting:
    [fd] has input, has reached its end, or has an error to report. It never
    waits ([poll(2)] with no timeout), takes nothing from [fd] and allocates
    nothing. On Windows a console's handle is asked whether it is signaled,
    which takes none of the records it holds, as waiting with [Unix.select]
    does. *)

val now : unit -> int
(** [now ()] is the time in nanoseconds since an arbitrary origin, on a clock
    that never goes back and that setting the system's time does not move
    ([CLOCK_MONOTONIC], or the performance counter on Windows). On Linux the
    clock stops while the system sleeps, so a deadline comes that much later on
    the wall clock. On macOS it goes on while the system sleeps. *)

(** {1:settings Settings}

    A terminal's line discipline echoes what is typed, collects a line until
    Enter, and turns the interrupt, quit and suspend characters into signals. A
    program that reads every key as it is typed turns all of that off with
    {!raw}. One that reads keys and leaves the signals to the terminal, as a
    prompt does, turns off the echo and the lines with {!keys}, and one that
    reads a password turns off the echo alone with {!no_echo}. Each returns the
    settings it replaced, and {!restore} puts them back. *)

type settings
(** The type for a terminal's settings, what tcgetattr(3) gives, with the
    [IEXTEN] local flag that [Unix.terminal_io] leaves out. *)

val raw : Unix.file_descr -> settings
(** [raw fd] puts [fd] in raw mode immediately and returns the settings it had.
    In raw mode the terminal does not echo, edits no line, and turns no
    character into a signal, so Ctrl+C reaches the program as a byte. It also
    turns off output flow control ([IXON]) and the implementation-defined input
    characters ([IEXTEN]), with which the system would keep ^O and ^V, and stops
    turning CR into LF ([ICRNL]). A read returns as soon as one byte is there.
    Output processing ([OPOST]) and the file status flags, such as [O_NONBLOCK],
    stay as they were.

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val keys : Unix.file_descr -> settings
(** [keys fd] puts [fd] in the {e key interval} immediately and returns the
    settings it had. The terminal echoes nothing and edits no line, so a read
    returns each key as soon as one byte of it is there, and a carriage return
    stays one ([ICRNL] is off), as it is in raw mode. The interrupt, quit and
    suspend characters still send their signals to the job ([ISIG]), the
    implementation-defined characters stay ([IEXTEN]), and output is processed
    as before. So Ctrl+C ends the job and Ctrl+Z stops it as at a shell's
    prompt, while Ctrl+D, the erase and the kill characters reach the program as
    bytes. Input not yet read stays.

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val no_echo : Unix.file_descr -> settings
(** [no_echo fd] turns [fd]'s echo off and returns the settings it had, as
    getpass(3) does to read a password. Lines are still edited, the signal
    characters still send their signals, and nothing typed is shown, line end
    included ([ECHO], [ECHOE], [ECHOK] and [ECHONL] are off). It first discards
    the input not yet read ([TCSAFLUSH]), so what was typed before the question
    is not taken as its answer.

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val restore : Unix.file_descr -> settings -> unit
(** [restore fd s] sets [fd]'s settings to [s] and keeps the input not yet read
    for whoever reads next. It blocks until the output written to [fd] has been
    sent ([TCSADRAIN]).

    Raises [Unix.Unix_error] if [fd] is not a terminal. *)

val flush_input : Unix.file_descr -> unit
(** [flush_input fd] discards the input [fd] has received and nobody has read
    ([tcflush], [TCIFLUSH]). It does nothing if [fd] is not a terminal. *)

type console
(** The type for what {!enable_vt} changes in a Windows console, the mode of the
    console and the output code page of the process's console. *)

val enable_vt : Unix.file_descr -> console
(** [enable_vt fd] has the Windows console [fd] interpret the control functions
    written to it as a VT100 does, sets the output code page of the process's
    console to UTF-8, and returns the mode and code page it replaced. The
    console also defers the wrap ([DISABLE_NEWLINE_AUTO_RETURN]). A character
    written in the last column leaves the cursor there, and the cursor moves to
    the next line only when the next printable character comes, so a line that
    fills the width and ends with a line feed takes one row. The console is
    shared by every process attached to it, the shell among them, so a program
    must give the result to {!restore_console} before it ends. It does nothing
    on other systems, or if [fd] is not a console. *)

val restore_console : Unix.file_descr -> console -> unit
(** [restore_console fd c] sets the mode of the console [fd] and the output code
    page of the process's console back to those [c] holds. It does nothing when
    the {!enable_vt} that returned [c] did nothing. *)

(** {1:descriptions Terminal descriptions}

    The description of a terminal type [name] is looked for in these places, in
    order, as ncurses looks for it (terminfo(5), "Fetching Compiled
    Descriptions"):
    + [TERMINFO], which is a database, or a description itself encoded in
      hexadecimal after [hex:] or in base64 after [b64:] (as [tic -Q] writes
      them), used only if one of its names is [name];
    + [$HOME/.terminfo];
    + each database of [TERMINFO_DIRS], separated by colons, where an empty one
      is [/usr/share/terminfo];
    + the system's databases, [/etc/terminfo], [/lib/terminfo],
      [/usr/share/terminfo], [/usr/lib/terminfo], [/usr/share/lib/terminfo] and
      [/usr/local/share/terminfo].

    In a database, the description is the file [name] in the directory named by
    [name]'s first character, or by that character's code in two hexadecimal
    digits, as a database on a file system that ignores case has it. A database
    kept as a hashed file is not read. An empty variable counts as unset. A
    process whose user or group differs from its caller's (setuid) reads none of
    [TERMINFO], [HOME] and [TERMINFO_DIRS], so that its caller cannot give it a
    description. *)

val terminfo :
  string -> (Matrix_terminfo.t, [ `Not_found | `Invalid of string ]) result
(** [terminfo name] is the first description of the terminal type [name], such
    as [TERM]'s value, that reads ({!Matrix_terminfo.of_string}) in the places
    the {{!section-descriptions}section} lists. It is [Error (`Invalid why)]
    when descriptions were found and none reads, where [why] says why the first
    one does not. It is [Error `Not_found] when there is none, and when [name]
    cannot be a file name ([""], ["."], [".."], or a name with a [/]). *)

(** {1:signals Signals} *)

(** The signals a terminal sends the process, shared by every holder of a
    terminal in it.

    A program that changes a terminal's settings or modes must put them back
    when a signal ends or stops the process. Otherwise the shell gets a terminal
    that echoes nothing, reports the mouse or shows no cursor. A process has one
    action per signal, which the program, the libraries it links and every
    holder share. This module takes the actions once, by one policy, and tells
    every holder of the signals that concern it.

    {2:policy The policy}

    - SIGINT, SIGTERM, SIGHUP and SIGQUIT end the process, so they concern every
      holder, on any terminal. A system call they interrupt is not resumed, so a
      write the terminal is not taking returns and the holder can give its
      terminal back.
    - SIGABRT, which [abort()] raises, returns to no point where OCaml can run.
      Its handler restores every holder's terminal itself, as
      {!Signal.restore_all} does, and the process aborts.
    - SIGTSTP, SIGWINCH and SIGCONT concern the holders of the controlling
      terminal ({!is_controlling}) alone. Its job is the one the shell stops and
      continues, and its window the one that SIGWINCH reports. A terminal the
      process serves, such as a pseudo-terminal it opened, reports its size
      itself.

    SIGINT, SIGTERM, SIGHUP, SIGQUIT, SIGABRT and SIGTSTP are taken only when
    their action is the default, as X/Open Curses allows, so a program that
    installed its own action keeps it and writes its own handler. SIGWINCH and
    SIGCONT are taken whatever their action, and a handler installed before them
    is still called. The system calls that SIGTSTP, SIGWINCH, SIGCONT and
    SIGABRT interrupt are resumed ([SA_RESTART]), so a resize never cuts a
    write. {!Signal.write} goes on with a write that a signal cuts all the same.

    Holders take a signal together. The first {!Signal.take} installs the
    handler, and the action it replaced comes back when the last holder gives
    the signal back ({!Signal.give_back}). The handlers are C. A handler records
    the signal for each holder it concerns and writes one byte to the holder's
    {e wake pipe} ({!Signal.wake_fd}), which wakes a holder waiting in
    [select(2)] whenever the signal lands.

    Signals are numbered as OCaml numbers them ([Sys.sigint]). On Windows, which
    has none of these signals, {!Signal.take} and {!Signal.ignored} are [false],
    {!Signal.give_back}, {!Signal.raise_default}, {!Signal.stop_group} and
    {!Signal.restore_all} do nothing, and {!Signal.hold} raises [Failure]. *)
module Signal : sig
  (** {1:actions Actions} *)

  val signals : controlling:bool -> int list
  (** [signals ~controlling] is the signals a holder of a terminal takes. They
      are SIGABRT, SIGINT, SIGTERM, SIGHUP and SIGQUIT, and before them, when
      the terminal is the controlling one, SIGWINCH, SIGCONT and SIGTSTP. *)

  val take : int -> bool
  (** [take s] takes [s] as the {{!policy}policy} says and is [true] iff it did.
      It is [false] when the program has an action of its own for [s] and the
      policy leaves [s] to it.

      Raises [Invalid_argument] if [s] is not one of {!val-signals}'s. *)

  val give_back : int -> unit
  (** [give_back s] gives back a [take s] that was [true]. The action [s] had
      before comes back once every holder that took it has given it back. *)

  val ignored : int -> bool
  (** [ignored s] is [true] iff the action of [s] is to ignore it. Nothing would
      continue a job whose SIGTSTP is ignored once it stopped, so a holder does
      not stop such a job ({!stop_group}). *)

  val raise_default : int -> unit
  (** [raise_default s] sets the action of [s] to the default, forgets every
      take of it, and raises it in the calling thread, so that the process ends
      or stops as it would with no handler. If the process stops,
      [raise_default] returns once it is continued, and [s]'s action stays the
      default until it is taken again. A {!give_back} of an earlier take of [s]
      then does nothing. *)

  val stop_group : unit -> unit
  (** [stop_group ()] sends SIGTSTP to the process's group, as the terminal's
      suspend character does. Where SIGTSTP's action is the default, the job
      stops, the process with it, and [stop_group] returns once the job is
      continued. *)

  (** {1:holders Holders}

      A holder is valid from {!hold} to {!release}. Every function below that is
      given a holder after its release raises [Invalid_argument], except
      {!wake}, which then does nothing. *)

  type holder
  (** The type for holders of a terminal. *)

  val hold : controlling:bool -> output:Unix.file_descr option -> holder
  (** [hold ~controlling ~output] registers a holder of a terminal, the
      controlling one if [controlling], with a wake pipe of its own. If the
      process ends where no OCaml code runs, the holder's reset bytes
      ({!set_reset}) are written to [output], the descriptor that the terminal
      is written through. It takes no signal ({!take} does).

      Raises [Unix.Unix_error] if the pipe cannot be made, and [Failure] on
      Windows. *)

  val release : holder -> unit
  (** [release h] ends [h]. The handlers forget it, and an abort no longer
      restores its terminal. *)

  val wake_fd : holder -> Unix.file_descr
  (** [wake_fd h] is the read end of [h]'s wake pipe. It is non-blocking, and
      readable once a signal that concerns [h] has been received, or {!wake} has
      been called, since the holder last read it empty. *)

  val wake : holder -> unit
  (** [wake h] makes {!wake_fd} readable. It may be called from any thread or
      domain, and does nothing once [h] is released, so a thread may wake a
      holder that another thread releases. *)

  val received : holder -> int -> bool
  (** [received h s] is [true] iff [s] has been received for [h] since [h] was
      made or since the last [received h s].

      Raises [Invalid_argument] if [s] is not a signal number of the system. *)

  val terminating : holder -> bool
  (** [terminating h] is [true] iff SIGINT, SIGTERM, SIGHUP or SIGQUIT has been
      received for [h] and not yet taken with {!received}. *)

  val keep_settings : holder -> input:Unix.file_descr -> unit
  (** [keep_settings h ~input] saves the settings [input] has now, which are put
      back if the process ends where no OCaml code runs. It saves nothing if
      [input] is not a terminal. *)

  val forget_settings : holder -> unit
  (** [forget_settings h] drops the settings {!keep_settings} saved, once the
      holder has put them back itself. *)

  val set_reset : holder -> string -> unit
  (** [set_reset h bytes] makes [bytes] [h]'s {e reset bytes}, the bytes that
      reset the modes [h] has set, of any length. They are written to [h]'s
      output ({!hold}) if the process ends where no OCaml code runs. A holder
      that has given its terminal back sets [""]. A handler that runs while
      [set_reset] replaces the bytes writes either the old bytes or the new ones
      whole.

      A holder must set the bytes that depend on a write only after the write
      has returned. A signal that came in between would otherwise reset for a
      screen the terminal does not show. *)

  val restore_all : unit -> unit
  (** [restore_all ()] restores the terminal of every holder not yet released,
      as an abort does. It writes each holder's reset bytes to its output and
      puts back the settings the holder kept. A holder that ends the process on
      a termination signal gives its own terminal back, then calls [restore_all]
      for the others, for which no OCaml code will run. *)

  val write : holder -> Unix.file_descr -> bytes -> int -> int -> unit
  (** [write h fd buf off len] writes the [len] bytes of [buf] from [off] to
      [fd]. It blocks until every byte is written. It goes on from where a write
      stopped when a signal cut it, and waits for [fd] to take more when [fd] is
      non-blocking and full. When a signal that ends the process
      ({!terminating}) interrupts it, it returns with the rest unwritten.

      Raises [Unix.Unix_error] on an error other than [EINTR] and [EAGAIN]. *)
end
