(** Command-line output on Unix: the standard outputs, and a region of rows
    redrawn below them.

    An {e output} ({!t}) is a descriptor together with what {!Matrix_cli.Policy}
    decides for it, its styling and whether a region of rows may be redrawn on
    it. The decision is made once, when the output is made, from the program's
    choice, the process's environment, the terminal's capabilities (from [TERM]
    and its terminfo description) and whether the descriptor is a terminal. What
    a program prints through an output is adapted to its styling
    ({!Matrix_cli.section-adapting}) and written at once, one write for each
    call to {!print} and each flush of its {!formatter}, so a pipe gets each
    record whole and as soon as it is printed. Outputs whose descriptors reach
    one terminal, or one file, share a lock, so their writes never interleave,
    and what the program printed through [Stdlib.stdout] and [Stdlib.stderr] is
    flushed before each write, so the two keep their order under [2>&1].

    A {e live} region ({!live}) is a few rows that a program redraws below its
    output while it works, such as a progress bar. It is a
    {!Matrix_screen.Region}, whose model says what a redraw may and may not
    erase. While it lives, the lines printed through any output on its terminal
    go above it, each in the write that redraws the rows below, and a line
    printed without its end waits for its end, or for the region to be suspended
    or closed. A frame is drawn at most ten times a second, as cargo and Rich
    redraw their progress bars, and at once when lines go above it. The region
    is redrawn when the terminal's width changes, and drawn afresh when the job
    is continued. A job stopped with Ctrl+Z first erases it. A job in the
    background ({!Tty.background}) draws no frame, so its rows never land among
    what the shell writes, and its lines go out as they are. When the scope
    ends, its last frame stays in the scrollback with the cursor below it. On an
    output where no region may be drawn, such as a pipe or a CI job's log, the
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

    A region takes the signals of its terminal, as {!Tty.Signal} shares them,
    and gives them back when it ends. SIGINT, SIGTERM, SIGHUP and SIGQUIT, at
    their default action, end the process once the terminal is put back. The
    region's write that a signal cut is ended, and the cursor is shown and moved
    below the region's rows. A process that ends with [exit] while a region
    lives does the same. A terminal that stops taking output, as under Ctrl+S,
    holds this back for at most a second.

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
    ]} *)

(** {1:outputs Outputs} *)

type t
(** The type for outputs. *)

val make : ?color:Matrix_cli.Policy.choice -> Unix.file_descr -> t
(** [make ~color fd] is the output on [fd], with the program's choice [color]
    ([`Auto] by default). On Windows, a console's VT processing is turned on
    ({!Tty.enable_vt}) and turned back off when the program exits. *)

val stdout : ?color:Matrix_cli.Policy.choice -> unit -> t
(** [stdout ~color ()] is [make ~color Unix.stdout]. *)

val stderr : ?color:Matrix_cli.Policy.choice -> unit -> t
(** [stderr ~color ()] is [make ~color Unix.stderr]. *)

val styling : t -> Matrix_cli.styling
(** [styling o] is [o]'s styling, at which a program writes a grid for it
    ({!Matrix_grid.to_ansi}). *)

val width : t -> int option
(** [width o] is the width a program lays out its output in: [COLUMNS] when it
    is set to a positive number (POSIX's "user's preferred width in column
    positions"), else the width of [o]'s terminal when [o] is one, read at each
    call, and [None] for a pipe or a file. *)

val print : t -> string -> unit
(** [print o s] writes [s] to [o], adapted to its styling, in one write. *)

val formatter : t -> Format.formatter
(** [formatter o] is a formatter that writes to [o] as {!print} does, one write
    for each flush, with its margin at [o]'s width. Fmt writes styles on it only
    when its style renderer is set ([Fmt.set_style_renderer]), as on any
    formatter, and [o] then decides how they show. *)

val adapt_std_formatters : ?color:Matrix_cli.Policy.choice -> unit -> unit
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

val live : t -> (live -> 'a) -> 'a
(** [live o f] runs [f] with a live region on [o], or with one that draws
    nothing where none may be drawn ({!Matrix_cli.Policy.live}). When [f]
    returns or raises, the region's last frame stays with the cursor below it,
    or is printed once, at [o]'s {!width} or 80 columns when it has none.

    Raises [Invalid_argument] if a region already lives on [o]'s terminal, and
    what a frame given to {!show} raised, if [f] returned. *)

val show : live -> (width:int -> Matrix_grid.t option) -> unit
(** [show l frame] makes [frame] [l]'s frame and draws it, now or at most a
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
