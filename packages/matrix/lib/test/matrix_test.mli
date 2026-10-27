(** Terminals emulated in a test's process, and the programs that run on them.

    A {e terminal} ({!Terminal.t}) is an emulator ({!Vte}) playing a {e profile}
    ({!Profile.t}). It applies what a program writes, answers the program's
    queries as that terminal does, keeps what the program asks of its host, and
    gives the bytes it sends for what its user does, in the encoding the program
    has set (its keyboard protocol, mouse format and paste mode). It performs no
    input or output itself.

    A {e session} ({!t}) runs a program in the test's process on a terminal,
    with a {e clock} that moves only inside the session's functions. The program
    runs on {!Matrix}, or on a library built on it, with the session's
    configuration ({!config}), which gives the run the terminal and the clock. A
    program that waits for its input, with [Matrix.next] as [Mosaic.run] does,
    runs unchanged ({!run}). When it has nothing to do, it waits for the test
    instead of the terminal. A program that takes its own steps has the test
    take them ({!attach}). Either way the program goes on only inside the
    session's functions, and between two calls nothing runs.

    The terminal shows each write as it is made and sends each input as one
    write. So the run draws a frame asked for by input, a timer, a message or
    the runtime itself at the instant it is asked, and decodes a lone ESC as
    Escape at once. An animation's next frame, asked for from its frame, comes
    one frame interval later ({!Matrix.val-config}'s [fps]), as on any terminal,
    and a frame that input brings in between does not move the times of the
    animation's later frames. Time moves only by {!advance} and {!set_time}, and
    by the wait for an answer of a terminal that gives none ({!Profile.t}'s
    [answers]).

    A session is {e settled} when the program has nothing to do at the session's
    time and the work it runs off its loop, such as Mosaic's commands and
    workers, has finished or waits for something only the test can change. That
    work runs on an {e executor}, the session's queue ({!perform}) or the
    program's own, for which {!run} and {!attach} take a [quiesce].

    A session differs from a real terminal in two ways. A real terminal writes
    over time, so a lone ESC can wait for what may follow it, and a child on a
    pseudo-terminal ({!Child}) shows that race. And a session draws at once
    every frame asked for outside a frame, where a run on a real terminal draws
    at once only the frames discrete input asks for, and draws the others on the
    grid ({!Matrix.request_redraw}). The grid needs a real clock: on a session's
    clock, which moves only when the test moves it, a frame a timer or a post
    asks for would wait for a grid point that never comes. The grid is the same
    on both: a frame a session draws before the grid's next one is early
    ({!Matrix.early}), as on a terminal, and an animation moves only in the
    frames that are not, as Mosaic's ticks do, so its goldens do not depend on
    the input, the timers or the messages.

    The life cycle of a session:
    + {!Terminal.create} makes the terminal.
    + {!run} makes the session, starts the program, and settles; or {!create}
      makes it, the program starts with {!config}, and {!attach} gives the
      session its steps.
    + {!input}, {!resize}, {!settle} and {!advance} drive it, {!set_time} moves
      its clock, and the readers of {!Terminal} observe it.
    + The run ends when the program returns, on its own or at the end of the
      input, which {!run} gives once its body returns. The terminal then shows
      what the run left on it.

    The functions that drive a session, {!input}, {!resize}, {!settle} and
    {!advance}, settle it before they return, and raise [Invalid_argument] if no
    program is attached or its run has ended. The other functions that take a
    session do not settle it, and raise only as their docs say.

    This runs a program that counts keys, types two and prints the screen:
    {[
    let term = Matrix_test.Terminal.create ~cols:20 ~rows:1 () in
    Matrix_test.run term (fun s ->
        Matrix.run ~config:(Matrix_test.config s) @@ fun app ->
        let rec loop keys =
          match Matrix.next app with
          | Stopped -> ()
          | Event (Key _) ->
              Matrix.request_redraw app;
              loop (keys + 1)
          | Frame ->
              Matrix.draw app (fun grid ->
                  Matrix.Grid.draw_text grid ~x:0 ~y:0
                    ~text:(Printf.sprintf "%d keys" keys));
              loop keys
          | Event _ | Timer _ | Posted _ -> loop keys
        in
        loop 0)
    @@ fun s ->
    Matrix_test.input s (Matrix_test.Terminal.text term "ab");
    print_string (Matrix_test.Screen.text (Matrix_test.Terminal.screen term))
    ]}
    A Mosaic program runs with [Mosaic_test.run term app] (library
    [mosaic.test]), which is
    [Mosaic.run ~config:(Matrix_test.config s)
     ~process_perform:(Matrix_test.perform s) app] under {!run}, so that its
    work off the loop runs on the session's queue. *)

(** {1:profiles Profiles} *)

(** Terminal profiles, the facts of a terminal that a session plays. *)
module Profile : sig
  type t = {
    tty : bool;
        (** [false] for an output that is not a terminal, such as a pipe. The
            run then sets no mode and asks nothing, and the size is reported in
            band, as a mode 2048 report in the run's input ({!Matrix.run}). *)
    answers : bool;
        (** [false] for a terminal that answers no query, such as one behind a
            link that drops its replies. A run waits for its answers until its
            wait ends, and the session's clock moves there. *)
    env : (string * string) list;
        (** The environment of the program's session, such as [TERM] and
            [COLORTERM], from which the program guesses what the terminal does
            until it answers. *)
    identity : Vte.identity;  (** Its answers to DA1, DA2 and XTVERSION. *)
    extensions : Vte.extension list;
        (** The protocols it implements beyond the VT core. *)
    width_method : Matrix.Text.width_method;  (** How it measures text. *)
    palette : Vte.palette;  (** The colors its user configured. *)
    color_scheme : [ `Dark | `Light ];  (** The scheme its user chose. *)
    cell_size : (int * int) option;
        (** The size of its cells in pixels, [(width, height)], which it reports
            to [CSI 16 t] and [CSI 14 t] ({!Vte.create}). *)
  }
  (** The type for profiles. A named terminal enters as a value whose facts cite
      that terminal's documentation. *)

  val vte : t
  (** [vte] is matrix's emulator as it is: a terminal that answers, with its own
      identity ({!Vte.vt100}), every extension, Unicode widths, the default
      palette, a dark scheme, no cell size, [TERM=xterm-256color] and
      [COLORTERM=truecolor]. *)

  val pipe : t
  (** [pipe] is {!vte} as an output that is not a terminal, which answers no
      query and measures text with wcwidth, as a program that cannot ask the
      terminal does. *)
end

(** {1:screens Screens} *)

(** What a terminal shows at one moment. *)
module Screen : sig
  type t
  (** The type for screens, the cells and the cursor of a terminal at one
      moment. A screen does not change. Rows and columns count from 0. *)

  type cursor = {
    row : int;  (** The cursor's row. *)
    col : int;  (** The cursor's column. *)
    shape : Matrix.Ansi.cursor_style;  (** The cursor's shape. *)
    blinking : bool;  (** Whether the cursor blinks. *)
  }
  (** The type for a visible cursor. *)

  val rows : t -> int
  (** [rows s] is the number of rows of [s]. *)

  val cols : t -> int
  (** [cols s] is the number of columns of [s]. *)

  val row : t -> int -> string
  (** [row s y] is the text of row [y], a space for an empty cell, without the
      spaces that end it.

      Raises [Invalid_argument] if [y] is not a row of [s]. *)

  val cell : t -> row:int -> col:int -> string * Matrix.Ansi.Style.t
  (** [cell s ~row ~col] is the grapheme and the style of that cell, [""] for an
      empty cell or the second column of a wide character.

      Raises [Invalid_argument] if the cell is not one of [s]. *)

  val cursor : t -> cursor option
  (** [cursor s] is the cursor, [None] when it is hidden. *)

  val find : t -> string -> (int * int) list
  (** [find s text] is the row and column of each occurrence of [text] within a
      row of {!val-row}'s text, top to bottom, then left to right.

      Raises [Invalid_argument] if [text] is empty. *)

  (** {2:goldens Goldens}

      A {e golden} is a screen as text, which a test compares with an expected
      one, such as in an expect test. Its rows are numbered from 01 as
      ["NN |text"], with at least two digits, more when the last row's number
      needs them, and its last line is the cursor's, ["cursor ROW,COL SHAPE"]
      counted from 1, with [blinking] after the shape when it blinks, or
      ["cursor hidden"]. *)

  val text : t -> string
  (** [text s] is the golden of [s]: each row's {!val-row} text, then the cursor
      line, each ending with a newline. *)

  val styled : t -> string
  (** [styled s] is {!text} where each row that holds a cell not in the default
      style is followed by a line of one {e key} per column, under the row's
      prefix, naming that cell's style: a space for the default style (the
      terminal's default colors, no attribute, no hyperlink), and the same key
      for both columns of a wide character. After the cursor line comes one line
      for each key, the key and its style. Keys are letters, digits, then other
      characters, in the order in which styles first appear, left to right and
      top to bottom. *)

  val diff : t -> t -> string option
  (** [diff a b] is [None] when [a] and [b] show the same cells and cursor.
      Otherwise it is each row of [b] that differs from [a]'s, as in a golden,
      followed by a line with a letter under each column that differs, by the
      first of these that holds, as Vim's screen dumps mark them: [X] another
      character, [w] another width, [f] another foreground, [b] another
      background, [a] other attributes or another hyperlink. Then come both
      cursor lines, if the cursors differ. Screens of different sizes give both
      sizes. *)
end

(** {1:terminals Terminals} *)

(** Terminals, emulators playing a profile. *)
module Terminal : sig
  type t
  (** The type for terminals. *)

  val create : ?profile:Profile.t -> cols:int -> rows:int -> unit -> t
  (** [create ~cols ~rows ()] is a terminal at power-up playing [profile]
      (default {!Profile.vte}), [cols] wide and [rows] high. *)

  val profile : t -> Profile.t
  (** [profile t] is the profile [t] plays. *)

  (** {2:user The user's side}

      Each of these is the bytes the terminal sends for what its user does, in
      the encoding the program has set, and [""] for a report the program has
      not asked for. {!color_scheme} also changes the terminal's scheme. A
      session ({!input}) or a child ({!Child.send}) delivers them. *)

  val key :
    t ->
    ?mods:Matrix.Input.Keystroke.mods ->
    Matrix.Input.Keystroke.key ->
    string
  (** [key t k] is the bytes of [k] pressed with [mods] (default none). A [Char]
      types its character unless the modifiers make a chord.

      Raises [Invalid_argument] as {!Matrix.Input.Keystroke.encode} does. *)

  val keystroke : t -> Matrix.Input.Keystroke.t -> string
  (** [keystroke t k] is the bytes of the keystroke [k], with its text, keypad,
      base layout key and repeat.

      Raises [Invalid_argument] as {!Matrix.Input.Keystroke.encode} does. *)

  val release : t -> Matrix.Input.Keystroke.release -> string
  (** [release t r] is the bytes of the key release [r], [""] unless the program
      asked the kitty keyboard protocol for event types.

      Raises [Invalid_argument] as {!Matrix.Input.Keystroke.encode} does. *)

  val text : t -> string -> string
  (** [text t s] is the bytes of typing each character of [s] as a key.

      Raises [Invalid_argument] if [s] is not UTF-8 or holds a control
      character. *)

  val paste : t -> string -> string
  (** [paste t s] is the bytes of pasting [s], between the paste markers when
      the program has set bracketed paste. *)

  val mouse : t -> Matrix.Input.Mouse_report.t -> string
  (** [mouse t m] is the report of [m], when the program's mouse tracking
      reports it.

      Raises [Invalid_argument] as {!Matrix.Input.Mouse_report.encode} does. *)

  val focus : t -> bool -> string
  (** [focus t focused] is the report of the window gaining focus ([true]) or
      losing it, when the program has set focus reports. *)

  val color_scheme : t -> [ `Dark | `Light ] -> string
  (** [color_scheme t c] changes [t]'s color scheme to [c], and is its report
      when the scheme changed and the program has set color scheme reports. *)

  val set_colors : t -> Vte.palette -> string
  (** [set_colors t p] changes the colors [t]'s user configured to [p], as when
      they pick another theme ({!Vte.set_colors}), and is the report of the
      change, the color scheme's, when the program has set color scheme reports.
  *)

  (** {2:reading Reading} *)

  val screen : t -> Screen.t
  (** [screen t] is what [t] shows now. *)

  val scrollback : t -> string list
  (** [scrollback t] is the text of the lines scrolled off the primary screen's
      top, oldest first. *)

  val title : t -> string
  (** [title t] is the window title the program last set. *)

  val modes : t -> Vte.modes
  (** [modes t] is [t]'s modes. *)

  val raw : t -> bool
  (** [raw t] is [true] while a session's run has [t]'s input in raw mode. *)

  val requests : t -> Vte.request list
  (** [requests t] is what the program has asked of [t]'s host, oldest first
      ({!Vte.val-requests}). *)

  val output : t -> string
  (** [output t] is every byte the program has written to [t]. *)

  val vte : t -> Vte.t
  (** [vte t] is [t]'s emulator, which a test must only read. *)
end

(** {1:sessions Sessions} *)

type t
(** The type for sessions. *)

val run :
  ?config:Matrix.config ->
  ?now:int ->
  ?quiesce:(unit -> unit) ->
  Terminal.t ->
  (t -> unit) ->
  (t -> 'a) ->
  'a
(** [run term program body] runs [program s] on a session [s] of [term], whose
    run takes {!config}[ s], until the program waits for input ([Matrix.next],
    as in [Mosaic.run]) with nothing to do, then is [body s]. The program's
    first frame is on the terminal when [body] begins. The program stays where
    it waits, and each function of [body] that settles has it go on until it
    waits again. When [body] returns or raises, the input ends, and the program
    goes on until it returns, as it does at the end of its input. [config] is
    the configuration the run is given on the session's terminal (default
    [Matrix.config ()]), [now] as for {!create}, and [quiesce] as for {!attach}.

    The program runs on the test's thread, under an effect handler that [run]
    installs: where the program would wait, the session performs an effect that
    suspends it, and the session's functions resume it. So the program must wait
    on the thread and in the domain that called [run], inside [program], and not
    under a handler of its own that catches every effect. A program that waits
    with an event loop of its own takes its steps through {!attach} instead.

    Raises what [program] raises, at the call that had it go on, and
    [Invalid_argument] if the program waits from another thread or domain. *)

val create : ?config:Matrix.config -> ?now:int -> Terminal.t -> t
(** [create term] is a session on [term] whose clock reads [now] (default [0])
    nanoseconds, for a program driven by its steps ({!attach}), whose run takes
    [config] (default [Matrix.config ()]) on the session's terminal ({!config}).
    A terminal must have at most one session. *)

val terminal : t -> Terminal.t
(** [terminal s] is [s]'s terminal. *)

val config : t -> Matrix.config
(** [config s] is the configuration [s] was made with, for a run on [s]'s
    terminal and clock. The run takes no descriptor and no environment of its
    own, since the terminal's profile has them. *)

val perform : t -> (unit -> unit) -> unit
(** [perform s job] queues [job] on the session's queue, an executor such as
    [Mosaic.run]'s [process_perform]. Settling runs the queue in order, on the
    calling thread. A job must return and must not wait for the loop, so work
    that runs as long as the program does, such as a worker reading a stream,
    must run on the program's own executor, with a [quiesce] ({!attach}). *)

val attach :
  ?quiesce:(unit -> unit) ->
  t ->
  step:(now:int -> [ `Stepped | `Idle | `Stopped ]) ->
  deadline:(unit -> int option) ->
  unit
(** [attach s ~step ~deadline] has [s] drive a program that takes its own steps,
    as one does that waits with an event loop of its own, and settles.
    [step ~now] takes the program's next step at [now], as [Matrix.step] does,
    and says whether it took one, the program has nothing to do, or its run has
    stopped. [deadline ()] is [Matrix.deadline] of the program's run. They are
    [Mosaic.step] and [Mosaic.deadline] for a Mosaic program. [quiesce ()] must
    return once every piece of work the program's executor was given has
    finished or waits for something only the test can change. It defaults to
    running {!perform}'s queue.

    Raises [Invalid_argument] if a program is attached. *)

val input : t -> string -> unit
(** [input s bytes] delivers [bytes] as one write, which the run reads until it
    has read all of it. *)

val resize : t -> cols:int -> rows:int -> unit
(** [resize s ~cols ~rows] resizes the terminal and tells the run as the
    terminal does: by a window signal and the mode 2048 report when the program
    has set it, or, for an output that is not a terminal, by the report alone.
*)

val settle : t -> unit
(** [settle s] settles [s], as after the test released work it held. *)

val advance : t -> int -> unit
(** [advance s ns] moves the clock [ns] nanoseconds forward, and settles at each
    deadline on the way ({!deadline}) and at the end. So every timer and every
    animation frame on the way has an instant of its own.

    Raises [Invalid_argument] if [ns] is negative. *)

val set_time : t -> int -> unit
(** [set_time s time] moves the clock to [time] without taking a step, so that
    input delivered next is read in the same turn as the timers due at [time],
    and before them. It does not settle.

    Raises [Invalid_argument] if [time] is earlier than the clock. *)

val check_presented : t -> unit
(** [check_presented s] checks that the terminal shows, on the rows of the frame
    ([Matrix]'s live region in [`Primary] mode), the text of the frame the run
    believes it presented ([Matrix.current_grid]), which the run's own diff of
    frames wrote. A test calls it after its steps to catch output that puts on
    the terminal another frame than the run drew. Once the run has ended, and
    given the terminal back, it checks nothing.

    Raises [Failure], with each row that differs, if they differ, and
    [Invalid_argument] if no run has begun on [s]. *)

val now : t -> int
(** [now s] is the time of [s]'s clock, in nanoseconds. *)

val deadline : t -> int option
(** [deadline s] is the next time the program has something to do, if any.

    Raises [Invalid_argument] if no program is attached. *)

val frames : t -> int
(** [frames s] is the number of frames the run has begun. *)

val steps : t -> int
(** [steps s] is the number of steps the program has taken. *)

val ended : t -> bool
(** [ended s] is [true] once the program's run has stopped. *)

(** {1:children Programs in another process} *)

(** Programs run as a child process on a pseudo-terminal.

    A {e child} runs on the slave side of a pseudo-terminal whose master a
    terminal holds, on the real clock. The slave is the child's controlling
    terminal, so {!resize} signals it as a window does, and the terminal answers
    its queries as its profile says, as soon as its output is read. Nothing
    outside the child knows when it is done, so a test waits for what it expects
    to see, and checks it only where the screen is one a terminal may show:
    where no update (mode 2026) is open, and where one opens
    ({!Vte.section-updates}). *)
module Child : sig
  type t
  (** The type for children. *)

  val with_spawn :
    Terminal.t ->
    ?env:(string * string) list ->
    ?cwd:string ->
    prog:string ->
    args:string list ->
    (t -> 'a) ->
    'a
  (** [with_spawn term ~prog ~args f] runs [prog] with [args] on a
      pseudo-terminal of [term]'s size, calls [f] with the child, and ends and
      reaps the child when [f] returns or raises ({!Pty.with_spawn}). The
      child's environment is the profile's, then [env], and nothing of the
      test's, so a program that needs [PATH] or [HOME] is given them. [cwd] is
      its working directory, by default the test's.

      Raises [Unix.Unix_error] as {!Pty.spawn} does. *)

  val send : t -> string -> unit
  (** [send c bytes] writes [bytes] to the child's input. *)

  val resize : t -> cols:int -> rows:int -> unit
  (** [resize c ~cols ~rows] resizes the terminal and the pseudo-terminal, which
      signals the child, and sends the mode 2048 report when the child has set
      it. *)

  val wait : t -> within:float -> (Terminal.t -> bool) -> unit
  (** [wait c ~within ready] applies the child's output to the terminal and
      answers it until [ready] holds of the terminal, and returns with the
      terminal at that point of the output. The output read after it is applied
      by the next call. [ready] is checked at each point of the output where no
      update is open or where one opens, from the point the terminal is at. It
      blocks for at most [within] seconds.

      Raises [Failure], with the screen, if [within] seconds pass or the child's
      output ends first. *)

  val exit : t -> within:float -> Unix.process_status
  (** [exit c ~within] applies the child's output until it ends, and is the
      child's exit status. It blocks for at most [within] seconds of output.

      Raises [Failure], with the screen, if the output has not ended within
      [within] seconds. *)
end
