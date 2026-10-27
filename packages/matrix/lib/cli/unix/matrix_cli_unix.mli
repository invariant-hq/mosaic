(** Command-line output on Unix: the standard outputs, a region of rows redrawn
    below them, and questions asked at the terminal.

    An {e output} ({!t}) is a descriptor together with what {!Matrix_cli.Policy}
    decides for it, its styling and whether a region of rows may be redrawn on
    it. The decision is made once, when the output is made, from the program's
    choice, the process's environment, the terminal's capabilities (from [TERM]
    and its terminfo description) and whether the descriptor is a terminal. What
    a program prints through an output is adapted to its styling
    ({!Matrix_cli.section-adapting}) and written at once, one write for each
    call to {!print} and each flush of its {!formatter}, so a pipe gets each
    record whole and as soon as it is printed. A grid, such as a table that
    {!Matrix_cli.Table.grid} lays out, is printed with {!print_grid}. Outputs
    whose descriptors reach one terminal, or one file, share a lock, so their
    writes never interleave. What the program printed through [Stdlib.stdout],
    [Stdlib.stderr], [Format.std_formatter] and [Format.err_formatter], such as
    with [Fmt.pr], is flushed before each write, as [Format.pp_print_flush]
    flushes a formatter, so the standard outputs keep their order under [2>&1].

    A {e live} region ({!live}) is a few rows that a program redraws below its
    output while it works, such as a progress bar. It is a
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
    also have the region drawn on a clock ({!live}'s [tick]). The region is
    redrawn when the terminal's width changes, and drawn afresh when the job is
    continued. A job stopped with Ctrl+Z first erases it. A job in the
    background ({!Tty.background}) draws no frame, so its rows never land among
    what the shell writes, and its lines go out as they are. A [live] scope that
    begins while another lives on the same terminal joins its region, and its
    rows show below the rows of the scopes that began before it. The region
    shows the last rows of a frame taller than the terminal. When a scope ends,
    its last rows go above the region, which stays for the others, and when the
    last one ends, its last rows stay in the scrollback with the cursor below
    them. A scope's last rows stay whole, however tall. On an output where no
    region may be drawn ({!redraws}), such as a pipe or a CI job's log, the
    lines go out as they are printed and the last frame is printed once, at the
    end.

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
      let progress i ~width =
        let g = Matrix_grid.create ~width ~height:1 () in
        Matrix_grid.draw_text g ~x:0 ~y:0
          ~text:(Printf.sprintf "%d/%d compiled" i n);
        Some g
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

val make : ?color:Ansi.Styling.choice -> Unix.file_descr -> t
(** [make ~color fd] is the output on [fd], with the program's choice [color]
    ([`Auto] by default). On Windows, a console's VT processing is turned on
    ({!Tty.enable_vt}) and turned back off when the program exits. *)

val stdout : ?color:Ansi.Styling.choice -> unit -> t
(** [stdout ~color ()] is [make ~color Unix.stdout]. *)

val stderr : ?color:Ansi.Styling.choice -> unit -> t
(** [stderr ~color ()] is [make ~color Unix.stderr]. *)

val styling : t -> Ansi.Styling.t
(** [styling o] is [o]'s styling, at which a program writes a grid for it
    ({!Matrix_grid.to_ansi}). *)

val width : t -> int option
(** [width o] is the width a program lays out its output in: [COLUMNS] when it
    is set to a positive number (POSIX's "user's preferred width in column
    positions"), else the width of [o]'s terminal when [o] is one, read at each
    call, and [None] for a pipe or a file. *)

val redraws : t -> bool
(** [redraws o] is [true] iff a live region on [o] redraws its rows in place
    ({!Matrix_cli.Policy.live}). Where it is [false], as on a pipe, a dumb
    terminal or in a CI job's log, a region shows its last frame once, at its
    end, so a program prints there what it would otherwise only show in the
    rows, such as the start of each step. *)

val print : t -> string -> unit
(** [print o s] writes [s] to [o], adapted to its styling, in one write. *)

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
    on [o]'s terminal if one does, or with one that draws nothing where none may
    be drawn ({!Matrix_cli.Policy.live}). With [tick], the region also draws
    every [tick] seconds while [f] runs, calling the frames again, for a frame
    that changes with the time, such as a spinner or an elapsed time, between
    two {!show}s. A region whose scopes give ticks draws at the shortest, and at
    most ten times a second as any region. When [f] returns or raises, or the
    process exits in it, its last rows stay, as the module's preamble says, or
    are printed once, at [o]'s {!width} or 80 columns when it has none.

    Raises [Invalid_argument] if [tick] is not positive, and what a frame given
    to {!show} raised, if [f] returned. *)

val show : live -> (width:int -> Matrix_grid.t option) -> unit
(** [show l frame] makes [frame] [l]'s rows and draws them, now or at most a
    tenth of a second later. The region calls [frame ~width] for the width its
    rows take, at least 1, whenever it draws, from any thread, so [frame] must
    only read values that do not change while it runs, and must not print. Rows
    wider than [width] are cut, and [None] shows no row.

    Raises what an earlier frame of [l] raised, which was dropped, and
    [Invalid_argument] if [l]'s scope has ended. *)

val suspend : live -> (unit -> 'a) -> 'a
(** [suspend l f] erases [l]'s rows and runs [f], for something else that writes
    to the terminal, such as a child process, then draws [l] again from a row of
    its own, below a line [f] left without its end. *)

(** {1:prompts Prompts} *)

(** Questions asked at the terminal.

    A prompt asks only a user at a terminal. A question about the program's
    input reads standard input in the terminal's canonical mode, where the user
    edits a line before Enter sends it. A question only the user can answer,
    such as a password, is asked on the {e controlling terminal} ([/dev/tty]),
    as getpass(3) asks it, however the program's input is redirected. While a
    line or a secret is asked, a live region on its terminal is off the screen.
    While keys are read it stays, so a program can draw a menu in it.

    A prompt leaves the interrupt, quit and suspend characters to the terminal,
    so Ctrl+C ends the job, the quit character quits it and Ctrl+Z stops it as
    at the shell's prompt. A prompt that changed the terminal's settings puts
    them back before the job stops and turns them on again when it continues,
    asking its question again on a new line. *)
module Prompt : sig
  type error =
    [ `Not_interactive  (** No terminal to ask on. *)
    | `End_of_input  (** The input ended before a line did. *) ]
  (** The type for the reasons a prompt has no answer. *)

  val line : string -> (string, error) result
  (** [line question] writes [question] to standard error and is the line read
      from standard input, without its end. It is [Error `Not_interactive],
      writing nothing, when standard input is not a terminal. *)

  val secret : string -> (string, error) result
  (** [secret question] writes [question] to the controlling terminal and is the
      line read there with the echo off ({!Tty.no_echo}), without its end. What
      was typed before the question is discarded, the terminal's settings are
      put back on every way out, and the line end the echo did not show is
      written after the answer. It is [Error `Not_interactive] when the process
      has no controlling terminal. *)

  val keys : (Matrix_input.Keystroke.t Seq.t -> 'a) -> ('a, error) result
  (** [keys f] is [Ok (f ks)] where [ks] is the keys typed at the controlling
      terminal, read as they are typed ({!Tty.keys}) while [f] runs, after which
      the settings are put back on every way out. [ks] blocks for each key it
      gives, ends when the terminal's input ends, and must be read once, inside
      [f]. An Escape is told from the start of a key's sequence by 50
      milliseconds of silence. It is [Error `Not_interactive] when the process
      has no controlling terminal. *)
end
