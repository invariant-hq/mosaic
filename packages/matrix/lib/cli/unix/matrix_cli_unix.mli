(** Command-line output on Unix: the standard outputs, a region of rows redrawn
    below them, and questions asked at the terminal.

    An {e output} ({!t}) is a descriptor together with what is decided for it,
    its styling ({!Ansi.Styling}) and the mode its progress shows in
    ({!Matrix_cli.Progress}). The decisions are made once, when the output is
    made, from the program's choices, the process's environment, the terminal's
    capabilities (from [TERM] and its terminfo description) and whether the
    descriptor is a terminal. What a program prints through an output is adapted
    to its styling ({!Matrix_cli.section-adapting}) and written at once, one
    write for each call to {!print} and each flush of its {!formatter}, so a
    pipe gets each record whole and as soon as it is printed. On an output whose
    mode is [`Json], what it prints is events. A grid, such as a table that
    {!Matrix_cli.Table.grid} lays out, is printed with {!print_grid}. Outputs
    whose descriptors reach one terminal, or one file, share a lock, so their
    writes never interleave. What the program printed through [Stdlib.stdout],
    [Stdlib.stderr], [Format.std_formatter] and [Format.err_formatter], such as
    with [Fmt.pr], is flushed before each write, as [Format.pp_print_flush]
    flushes a formatter, so the standard outputs keep their order under [2>&1].

    A {e live} region ({!val-live}) is a few rows that a program redraws below
    its output while it works, such as a progress bar. It is a
    {!Matrix_screen.Region}, whose model says what a redraw may and may not
    erase. Its rows show below a blank row, its origin, where the cursor rests
    between two draws, so that what another process prints there stays whole.
    The cursor is hidden while the region shows, and shown again when the region
    goes off the screen, for {!suspend}, a prompt or a stop, and when it ends.
    While it lives, the lines printed through any output on its terminal go
    above it, each in the write that redraws the rows below, and a line printed
    without its end waits for its end, or for the region to be suspended or
    closed. A frame is drawn at most ten times a second, as cargo and Rich
    redraw their progress bars, and at once when lines go above it. A scope may
    also have the region drawn on a clock ({!val-live}'s [tick]). The region is
    redrawn when the terminal's width changes, and drawn afresh when the job is
    continued. A job stopped with Ctrl+Z first erases it. A job in the
    background ({!Tty.background}) draws no frame, so its rows never land among
    what the shell writes, and its lines go out as they are. A [live] scope that
    begins while another lives on the same terminal joins its region, and its
    rows show below the rows of the scopes that began before it. The region
    shows the last rows of a frame taller than the terminal. When a scope ends,
    its last rows go above the region, which stays for the others, and when the
    last one ends, its last rows stay in the scrollback with the cursor below
    them. A scope's last rows stay whole, however tall. A region is drawn on an
    output whose mode ({!progress}) is [`Tty]. Where it is [`Plain], such as on
    a pipe or in a CI job's log, the lines go out as they are printed and the
    last frame is printed once, at the end, and where it is [`Quiet] or [`Json]
    no frame is drawn or printed.

    The program's own writes to a terminal that bypass its outputs, such as
    [print_string] or a child process's output, land where the cursor rests, at
    the start of the region's origin (in its second column on WezTerm,
    {!Matrix_screen.Region}), and the next redraw erases what they leave on the
    origin without a line end. So a program prints through its outputs while a
    region lives, and runs a child that writes to the terminal inside
    {!suspend}. On WezTerm a line a program prints while the cursor rests in the
    first column can be lost when the window's width changes, whatever library
    prints it.

    A region and a prompt take the signals of their terminal, as {!Tty.Signal}
    shares them, and give them back when they end. SIGINT, SIGTERM, SIGHUP and
    SIGQUIT, at their default action, end the process once the terminal is put
    back. The region's write that a signal cut is ended, the cursor is shown and
    moved below the region's rows, and a prompt's settings are restored. A
    process that ends with [exit] inside [live] scopes ends them as their ends
    would. A region leaves the last rows of every scope in the scrollback, with
    the cursor below them, and an output where no region may be drawn prints its
    last frame. This happens before the [at_exit] functions registered before
    the first scope run, so what they print goes below the rows. A terminal that
    stops taking output, as under Ctrl+S, holds this back for at most a second.

    This prints the files a program compiles above a line that counts them, on
    standard error:
    {[
    let compile files =
      let err = Matrix_cli_unix.stderr () in
      let n = List.length files in
      let progress i ~width ~height:_ =
        let g = Matrix_grid.create ~width ~height:1 () in
        Matrix_grid.draw_text g ~x:0 ~y:0
          ~text:(Printf.sprintf "%d/%d compiled" i n);
        [ g ]
      in
      Matrix_cli_unix.live err @@ fun live ->
      List.iteri
        (fun i file ->
          Matrix_cli_unix.show live (progress i);
          compile_one file;
          Matrix_cli_unix.print err (file ^ "\n"))
        files;
      Matrix_cli_unix.show live (progress n)
    ]}
    This prints a table as wide as standard output allows, with styles as the
    output decides:
    {[
    let print_table cols rows =
      let out = Matrix_cli_unix.stdout () in
      let room = Option.value (Matrix_cli_unix.width out) ~default:80 in
      let frame =
        { Matrix_cli.Table.edge = Some `Light; line = Some `Light; pad = 1 }
      in
      Matrix_cli_unix.print_grid out
        (Matrix_cli.Table.grid frame ~room cols rows)
    ]} *)

(** {1:outputs Outputs} *)

type t
(** The type for outputs. *)

val make :
  ?color:Ansi.Styling.choice ->
  ?progress:Matrix_cli.Progress.choice ->
  Unix.file_descr ->
  t
(** [make ~color ~progress fd] is the output on [fd], with the program's choices
    [color] and [progress] ([`Auto] by default). On Windows, a console's VT
    processing is turned on ({!Tty.enable_vt}) and turned back off when the
    program exits. *)

val stdout :
  ?color:Ansi.Styling.choice ->
  ?progress:Matrix_cli.Progress.choice ->
  unit ->
  t
(** [stdout ~color ~progress ()] is [make ~color ~progress Unix.stdout]. *)

val stderr :
  ?color:Ansi.Styling.choice ->
  ?progress:Matrix_cli.Progress.choice ->
  unit ->
  t
(** [stderr ~color ~progress ()] is [make ~color ~progress Unix.stderr]. *)

val styling : t -> Ansi.Styling.t
(** [styling o] is [o]'s styling, at which a program writes a grid for it
    ({!Matrix_grid.to_ansi}). *)

val width : t -> int option
(** [width o] is the width a program lays out its output in: [COLUMNS] when it
    is set to a positive number (POSIX's "user's preferred width in column
    positions"), else the width of [o]'s terminal when [o] is one, read at each
    call, and [None] for a pipe or a file. *)

val progress : t -> Matrix_cli.Progress.t
(** [progress o] is [o]'s mode ({!Matrix_cli.Progress}). A live region redraws
    its rows in place where it is [`Tty]. Where it is [`Plain], a program prints
    what the rows would otherwise show, such as the start of each step, and
    where it is [`Json], it prints its lines for people on another output. *)

val print : t -> string -> unit
(** [print o s] writes [s] to [o], adapted to its styling, in one write. Where
    [o]'s mode is [`Json], it writes each line of [s], and the text after its
    last line end, as {!Ansi.sanitize} leaves them, as an event of its own,
    [{"time":t,"event":"output","text":line}], where [t] is the seconds since
    [o] was made. So a program prints its own lines for people on another
    output. *)

val print_grid : t -> Matrix_grid.t -> unit
(** [print_grid o g] writes the rows of [g] to [o] in one write, at [o]'s
    styling ({!Matrix_grid.to_ansi}), each row ending at its last cell that
    shows something and with a line end. *)

val formatter : t -> Format.formatter
(** [formatter o] is a formatter that writes to [o] as {!print} does, one write
    for each flush, with its margin at [o]'s width. Fmt writes styles on it only
    when its style renderer is set ([Fmt.set_style_renderer]), as on any
    formatter, and [o] then decides how they show. *)

val adapt_std_formatters : ?color:Ansi.Styling.choice -> unit -> unit
(** [adapt_std_formatters ~color ()] makes [Format.std_formatter] and
    [Format.err_formatter] write as {!formatter}s of [stdout ~color ()] and
    [stderr ~color ()] do, with their margins at the outputs' widths. So the
    printers of a program that prints with [Fmt.pr] and [Fmt.epr], or reports
    with [Logs_fmt]'s reporter, obey the decision, and their lines go above a
    live region. Fmt writes styles there when its setup gives the two formatters
    [`Ansi_tty], as [Fmt_tty.setup_std_outputs] does on a terminal. *)

val write_raw : t -> string -> unit
(** [write_raw o bytes] writes [bytes] to [o] as they are, unadapted, for a
    control function the program means, such as a window title. Where a region
    lives on [o]'s terminal, the region is first erased, and drawn again
    afterwards from a row of its own. *)

val notify : t -> title:string -> body:string -> unit
(** [notify o ~title ~body] posts a desktop notification through [o]'s terminal,
    in the form it shows ({!Matrix_terminal.Caps.t}'s [notification]), wrapped
    for tmux inside it ({!Matrix_terminal.notify}). It does nothing when [o] is
    not a terminal or is a dumb one. *)

val bell : t -> unit
(** [bell o] rings [o]'s terminal's bell, and does nothing when [o] is not a
    terminal or is a dumb one. *)

(** {1:live Live regions} *)

type live
(** The type for live regions. *)

val live : ?tick:float -> t -> (live -> 'a) -> 'a
(** [live ~tick o f] runs [f] with a live region on [o], the region that lives
    on [o]'s terminal if one does, where [o]'s mode ({!progress}) is [`Tty], or
    with one that draws nothing elsewhere. With [tick], the region also draws
    every [tick] seconds while [f] runs, calling the frames again, for a frame
    that changes with the time, such as a spinner or an elapsed time, between
    two {!show}s. A region whose scopes give ticks draws at the shortest, and at
    most ten times a second as any region. When [f] returns or raises, or the
    process exits in it, its last rows stay, as the module's preamble says, or
    are printed once, at [o]'s {!width} or 80 columns when it has none.

    Raises [Invalid_argument] if [tick] is not positive, and what a frame given
    to {!show} raised, if [f] returned. *)

val show : live -> (width:int -> height:int -> Matrix_grid.t list) -> unit
(** [show l frame] makes [frame] [l]'s rows and draws them, now or at most a
    tenth of a second later. [frame ~width ~height] is the grids the rows show,
    one under another, [[]] for none. The region calls it whenever it draws,
    from any thread, so [frame] must only read values that do not change while
    it runs, and must not print. A grid costs its making, so a frame gives few.
    [width] is the width its rows take and [height] the number of rows the
    region shows below the rows of the scopes that began before, both at least
    1, so a frame can give way on a small terminal. Where no region is drawn,
    [width] is that of the last rows printed and [height] is [LINES] when it is
    set to a positive number, else the terminal's height, else 24. Rows wider
    than [width] are cut, and the region shows the last [height] rows of a
    taller frame, so its first grids give way first.

    Raises what an earlier frame of [l] raised, which was dropped, and
    [Invalid_argument] if [l]'s scope has ended. *)

val suspend : live -> (unit -> 'a) -> 'a
(** [suspend l f] erases [l]'s rows and runs [f], for something else that writes
    to the terminal, such as a child process, then draws [l] again from a row of
    its own, below a line [f] left without its end. *)

(** {1:tasks Tasks}

    A {e task} is a step of the work a program reports: a label, a count of
    items or of bytes done, out of a total when the program says one, the lines
    of output the program gives it, and how it ended. A task succeeds, fails or
    is cancelled, once. Tasks form a tree under a {e root}, the work as a whole,
    which {!val-tasks} makes for the scope of a function and shows on an output
    in the output's mode ({!progress}):
    - [`Tty] draws a live region ({!Task.rows}): a row for each running task,
      with the latest lines of its output under it, in its {e window}, and the
      tasks under it behind guides, a task that ended staying there, marked,
      while its top task runs. The last row is the {e summary}, the line that
      says how far the work is. When a task of the root ends, its rows go above
      the region as it ended, with every task under it, then the whole output of
      each of them that failed. So the scrollback keeps the order of what
      happened by top task, as [`Plain] prints it.
    - [`Plain] prints a line when a task starts, one for each line of its output
      and one when it ends, which says [failed] or [cancelled] in words when it
      did not succeed. Each line names its task by its {e path}, the labels from
      the root's task down to it, so the lines of tasks that run at once can
      interleave. The summary is the last line.
    - [`Quiet] prints the ending line of each task that failed, then all its
      output.
    - [`Json] prints each change as one event a line ({!section-events}).

    The lines are for people and their format may change. A program reads
    [`Json].

    A {e transient} task prints its lines only when it fails, and leaves nothing
    above the region when it succeeds. A task's label and lines show as text
    ({!Ansi.sanitize}), a label on one line. A task is told from any thread,
    domain or fiber, and the lines of the changes are written in the order of
    the changes. A task that ended ignores what it is told. A task keeps its
    output until the work ends. Times are seconds since the root started, by the
    clock {!val-tasks} is given.

    An Eio fiber cancelled inside {!Task.run} fails its task, and inside
    {!val-tasks} fails the work. A program shows them cancelled with
    {[
    let run parent label f =
      Task.run parent label (fun t ->
          try f t
          with Eio.Cancel.Cancelled _ as e ->
            Task.cancel t;
            raise e)

    let tasks out f =
      Matrix_cli_unix.tasks out (fun root ->
          try f root
          with Eio.Cancel.Cancelled _ as e ->
            Task.cancel root;
            raise e)
    ]} *)

(** Tasks. *)
module Task : sig
  type t
  (** The type for tasks, the root among them. *)

  type status = [ `Running | `Succeeded | `Failed | `Cancelled ]
  (** The type for where a task is: running or how it ended. *)

  val start : ?transient:bool -> ?lines:int -> t -> string -> t
  (** [start ~transient ~lines parent label] is a task named [label], started
      under [parent], transient iff [transient] ([false] by default), whose
      window shows its latest [lines] lines of output ([8] by default). A task
      started under one that ended is cancelled at once and prints nothing.

      Raises [Invalid_argument] if [lines] is negative. *)

  val run : ?transient:bool -> ?lines:int -> t -> string -> (t -> 'a) -> 'a
  (** [run ~transient ~lines parent label f] is [f t] for the task [t] that
      [start] starts. [t] succeeds when [f] returns and fails when [f] raises,
      with the exception as its message, which is raised again. An ending [f]
      gave [t] counts. *)

  val items : t -> ?total:int -> int -> unit
  (** [items t ~total n] says that [t] has done [n] items, out of [total] when
      given. Its rate is the count a second, weighing the past by
      [exp (-. dt /. 2.)] for [dt] seconds ago, and a count below the last
      starts it again.

      Raises [Invalid_argument] if [n] or [total] is negative. *)

  val bytes : t -> ?total:int -> int -> unit
  (** [bytes] is {!items} for a task that counts bytes. *)

  val rename : t -> string -> unit
  (** [rename t label] makes [label] [t]'s label. *)

  val log : t -> string -> unit
  (** [log t text] gives [t] each line of [text] as its output. *)

  val succeed : ?message:string -> t -> unit
  (** [succeed ~message t] ends [t] as a success, [message] after its label. The
      tasks under [t] that still run end first, and succeed with it. *)

  val fail : ?message:string -> t -> unit
  (** [fail ~message t] ends [t] as a failure, [message] after its label. The
      tasks under [t] that still run are cancelled first. *)

  val cancel : ?message:string -> t -> unit
  (** [cancel ~message t] ends [t] as cancelled, [message] after its label. The
      tasks under [t] that still run are cancelled first. *)

  type state = {
    label : string;
    status : status;
    message : string option;  (** The message it ended with. *)
    count : int;  (** [0] until it is counted. *)
    total : int option;
    unit : [ `Items | `Bytes ];  (** What it counts. *)
    elapsed : float;  (** Seconds since it started, up to its end. *)
    rate : float option;  (** Its count a second, once it has one. *)
    output : string list;  (** Its lines of output, oldest first. *)
    children : t list;  (** The tasks started under it, oldest first. *)
  }
  (** The type for what a task is, at a time. *)

  val state : t -> state
  (** [state t] is [t] as it is now. *)

  val rows : t -> width:int -> height:int -> Matrix_grid.t
  (** [rows t ~width ~height] is the rows of the running tasks under [t], and
      the summary under them if [t] is the root, [width] cells wide, as [`Tty]
      draws them, for a [frame] of {!val-tasks} to show with other grids. A
      task's row is its mark, its label, a bar when it has a total, its message,
      its count, its rate when it counts bytes, and its time, each in a column
      as wide as its widest cell. Rows taller than [height] give way: first the
      tasks that ended under each running task fold into one line, such as
      [10 done, 2 failed], then the windows shrink. *)
end

val tasks :
  ?label:string ->
  ?now:(unit -> float) ->
  ?frame:(Task.t -> width:int -> height:int -> Matrix_grid.t list) ->
  t ->
  (Task.t -> 'a) ->
  'a
(** [tasks ~label ~now ~frame o f] is [f root] for the root of the work, named
    [label] on its summary line ([""] by default), shown on [o] in [o]'s mode
    until [f] ends. The root succeeds when [f] returns and fails when [f]
    raises, which is raised again, unless [f] ended it, and its ending ends the
    tasks still running as {!Task.succeed} and {!Task.fail} do. [now] is the
    clock in seconds, monotonic by default. With [frame], the region of [`Tty]
    shows [frame root] in place of {!Task.rows}[ root], and [`Plain] prints its
    last frame once, at the end, the summary by default. [frame] must not tell a
    task anything and must not print. A [tasks] inside another on one output
    numbers its tasks apart ({!section-events}). *)

(** {2:events JSON events}

    Under [`Json], each change is an event, one JSON object on a line of its
    own, in the order of the changes, as ocaml-console writes them. Every event
    has two members: [time], the seconds since the root started, rounded to the
    millisecond, and [event], its name. An event of a task also has [id], a
    number that no other task shown on the output has, and [task], its path, an
    array of strings. The events are:
    - ["start"], with [parent], the [id] of the task it started under unless
      that is the root;
    - ["progress"], with [count], [total] when the program gave one, and [unit],
      ["items"] or ["bytes"];
    - ["label"], with [text], its new label;
    - ["output"], with [text], a line of output. An output event of no task is a
      line the program printed ({!print});
    - ["end"], with [status], ["succeeded"], ["failed"] or ["cancelled"],
      [elapsed], its seconds from start to end, and [message] when it has one.
      An end event of no task is the end of the work as a whole.

    Strings are valid UTF-8, with their C0 controls and DEL escaped. Objects may
    gain members and new events may be added, but the members above keep their
    meaning, so a program skips a member or an event it does not know. This is
    the scope of a task that compiles one file:
    {v
    {"time":0,"event":"start","id":0,"task":["compile"]}
    {"time":0,"event":"output","id":0,"task":["compile"],"text":"parser.ml"}
    {"time":0.5,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0.5}
    {"time":0.5,"event":"end","status":"succeeded","elapsed":0.5}
    v} *)

(** {1:prompts Prompts} *)

(** Questions asked at the terminal.

    A prompt asks only a user at a terminal. A question about the program's
    input reads standard input in the terminal's canonical mode, where the user
    edits a line before Enter sends it. A question only the user can answer,
    such as a password, is asked on the {e controlling terminal} ([/dev/tty]),
    as getpass(3) asks it, however the program's input is redirected. While a
    line or a secret is asked, a live region on its terminal is off the screen,
    and what the program prints on that terminal waits, to go out once the
    prompt has its answer, so that no line lands inside the question. While keys
    are read the region stays, so a program can draw a menu in it.

    A prompt writes its question, then its {e hint} in brackets when it shows
    one, such as [[y/N]] or a default, then a space. The question is adapted to
    the styling of the terminal it is written to ({!section-outputs}), for the
    program's [color] choice. A [default] answers an empty line, and a run where
    nobody can answer, such as with standard input a pipe, where a prompt
    without one is [Error `Not_interactive] and writes nothing.

    A prompt leaves the interrupt, quit and suspend characters to the terminal,
    so Ctrl+C ends the job, the quit character quits it and Ctrl+Z stops it as
    at the shell's prompt. A prompt that changed the terminal's settings puts
    them back before the job stops and turns them on again when it continues,
    asking its question again on a new line.

    A prompt blocks the thread that asks until it has its answer, waiting in
    system calls that release the runtime lock, so the program's other threads
    run while the user types. The fibers that share the thread wait with it. So
    an Eio program, whose fibers share their domain's thread, asks through
    [Eio_unix.run_in_systhread], as in
    [Eio_unix.run_in_systhread (fun () -> Prompt.line "Name?")], and its other
    fibers keep running. *)
module Prompt : sig
  type error =
    [ `Not_interactive  (** No terminal to ask on. *)
    | `End_of_input  (** The input ended before a line did. *) ]
  (** The type for the reasons a prompt has no answer. *)

  val line :
    ?color:Ansi.Styling.choice ->
    ?default:string ->
    string ->
    (string, error) result
  (** [line ~color ~default question] writes [question] to standard error, with
      [default] as its hint, and is the line read from standard input, without
      its end. It is [Error `Not_interactive] when standard input is not a
      terminal. *)

  val confirm :
    ?color:Ansi.Styling.choice ->
    ?default:bool ->
    string ->
    (bool, error) result
  (** [confirm ~color ~default question] asks [question] as {!line} does, with
      the hint [[y/n]], or [[Y/n]] and [[y/N]] for a [default] of [true] and
      [false]. It is [true] for a line whose first character after its blanks is
      [y] or [Y], and [false] for [n] or [N], as the POSIX locale's [yesexpr]
      and [noexpr] read an answer. Another line, or an empty one without
      [default], asks again. *)

  val secret :
    ?color:Ansi.Styling.choice ->
    ?default:string ->
    ?mask:bool ->
    string ->
    (string, error) result
  (** [secret ~color ~default ~mask question] writes [question] to the
      controlling terminal and is the line read there, without its end, with the
      echo off ({!Tty.no_echo}). Its hint never shows [default]. What was typed
      before the question is discarded, the terminal's settings are put back on
      every way out, and the line end the echo did not show is written after the
      answer. It is [Error `Not_interactive] when the process has no controlling
      terminal.

      With [~mask:true] ([false] by default), each character typed shows as [*],
      so the user sees the keys reach the program, and anyone who sees the
      screen sees the secret's length. The keys are decoded, so a key that types
      no text, such as an arrow, is ignored, and the text of a key, a paste or
      an input method is typed, a line end in a paste ending the line. The
      terminal's erase character and Backspace erase the last character, its
      kill character every one, Ctrl+W the last word, and its end-of-file
      character on an empty line ends the input. The stars are erased once the
      line ends. On a dumb terminal, which cannot erase them, nothing shows. *)

  val keys : (Matrix_input.Keystroke.t Seq.t -> 'a) -> ('a, error) result
  (** [keys f] is [Ok (f ks)] where [ks] is the keys typed at the controlling
      terminal, read as they are typed ({!Tty.keys}) while [f] runs, after which
      the settings are put back on every way out. [ks] blocks for each key it
      gives, ends when the terminal's input ends, and must be read once, inside
      [f]. An Escape is told from the start of a key's sequence by 50
      milliseconds of silence. It is [Error `Not_interactive] when the process
      has no controlling terminal.

      A menu that the keys move through is drawn on the terminal they are typed
      at, in a {!val-live} region on an output made on [/dev/tty] with
      [~progress:`Tty], which redraws it whatever [CI] or the program's
      [--progress] choice say. *)
end

(**/**)

(* A region's clock, for the tests that run it on every system. [clock m ~due
   fire] starts a thread that calls [fire ()] whenever the time [due ()] gives
   ({!Tty.now}) has come, both with [m] held, and that waits for [wake c]
   while [due ()] is [None]. [wake] and [stop] must be called with [m] held,
   and [stop c] ends the thread. *)

type clock

val clock : Mutex.t -> due:(unit -> int option) -> (unit -> unit) -> clock
val wake : clock -> unit
val stop : clock -> unit

(**/**)
