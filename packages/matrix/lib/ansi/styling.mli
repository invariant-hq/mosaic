(** The styling of a destination, how much of a style the bytes written there
    show.

    A program's styles reach a destination, such as a terminal, a pipe or a
    file, as the program asks and as its user lets them. The {e styling} of a
    destination ({!type-t}) is how they reach it. A color depth writes styles
    with each color reduced to it ({!Color.to_depth}), so [`Mono] writes the
    attributes, such as bold and underline, and no color. [`Plain] writes no
    style and no hyperlink.

    {!decide} gives a destination its styling from the program's {e choice}
    ({!type-choice}), the user's environment and the terminal. The choice, from
    a [--color] option or a configuration file, comes first. [`Always] writes
    the colors the terminal shows, and [`Never] writes no style at all, as
    Fmt_cli's [--color=never] "forces no styling". [`Auto] leaves the decision
    to the environment, read in this order, where an empty variable counts as
    unset:
    + [NO_COLOR] set removes the colors and keeps the attributes, since it "only
      signals the user's intention regarding adding ANSI color" (no-color.org,
      FAQ).
    + [FORCE_COLOR] or [CLICOLOR_FORCE] set, unless to [0] or [false], writes
      colors, also to a destination that is not a terminal, such as a CI job's
      log (force-color.org; bixense.com/clicolors).
    + [CLICOLOR] set to [0] removes the colors as [NO_COLOR] does.
    + A destination that is not a terminal, or a {e dumb} terminal, one that
      performs no control function and shows only text, gets no style.
    + Any other gets the colors the terminal shows.

    Removing the colors keeps the attributes only on a destination that shows
    styles at all, a terminal or one where colors are forced, and gives [`Plain]
    elsewhere. A wrong force writes escapes into a file, so a variable that may
    mean either way, such as [FORCE_COLOR=0], forces nothing. Each destination
    is decided on its own, so a program whose standard output is a pipe still
    writes colors to its standard error on a terminal.

    This module reads no environment and no terminal. The caller gives the
    environment as a function, such as [Sys.getenv_opt], and what the terminal
    shows, such as [Matrix_terminal.Caps]' [colors] and [dumb]. *)

type t = [ `Plain | Color.depth ]
(** The type for stylings. *)

type choice = [ `Auto | `Always | `Never ]
(** The type for a program's choices. *)

val choices : (string * choice) list
(** [choices] is the choices as a [--color] option names them, [auto], [always]
    and [never], as [Cmdliner.Arg.enum] takes them. *)

val variables : string list
(** [variables] is the environment variables {!decide} reads, [NO_COLOR],
    [FORCE_COLOR], [CLICOLOR_FORCE] and [CLICOLOR], for a program that keeps
    them as it read them, such as with the rest of a terminal's environment. *)

val decide :
  ?color:choice ->
  (string -> string option) ->
  tty:bool ->
  dumb:bool ->
  Color.depth ->
  t
(** [decide ~color getenv ~tty ~dumb colors] is the styling of a destination,
    which is a terminal iff [tty], dumb iff [dumb], and shows the colors of
    depth [colors], for the program's choice [color] ([`Auto] by default) and
    the environment read through [getenv], by the module's rule. *)
