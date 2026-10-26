(** Terminal capabilities, derived from evidence.

    The {e capabilities} of a terminal ({!t}) are the facts a program needs
    about it, such as whether it shows 24-bit color or reports focus changes.
    They are learned from three sources. An {e answer} is what the
    {e terminal that does the work}, the one that draws the screen and reads the
    keyboard, replies to a query. Inside a multiplexer that is the multiplexer.
    An {e identity} is the name and version of the terminal, which it reports or
    the environment names. A {e guess} comes from the environment alone. The
    sources disagree, and an answer can arrive late, so the capabilities are a
    function, {!derive}, of the {e evidence} gathered so far ({!type-evidence}),
    and a later answer can take back what an earlier guess gave.

    A program learns the capabilities in these steps:
    + {!val-env} reads the environment once, and {!val-evidence} makes the
      evidence before any answer.
    + {!ask} gives the bytes of the first {e batch} of queries, {!Probe}, which
      the program writes to the terminal, and of {!Widths}, which it writes on
      rows it owns.
    + The program adds each reply to the evidence with {!add}, {!val-cursor} and
      {!answered}, and {!derive} gives the capabilities so far.
    + {!next} gives the batch to send after an answered one, until it is [None].

    This is what the environment alone gives, before the terminal answers:
    {[
    let caps =
      Matrix_terminal.Caps.(
        derive (evidence (env Sys.getenv_opt ~win32:Sys.win32)))
    ]}

    {1:precedence Precedence}

    Each fact is taken from the first source that has it:
    + {b User policy}, which the person running the program decides.
      [MATRIX_FORCE_UNICODE] and [MATRIX_FORCE_WCWIDTH] set the width method,
      [MATRIX_GRAPHICS] how pictures are sent ({!field-graphics}), and
      [VHS_RECORD] (a recording) asks for the plain widths and no kitty
      protocols.
    + {b Corrections} keyed on the terminal's identity, for a terminal that
      answers but is known to do otherwise, each with its reason. Ghostty from
      1.3 widens graphemes past what mode 2027 describes, and Konsole draws
      sixel in a way pictures cannot use ({!field-graphics}).
    + {b The answer} of the terminal that does the work.
    + {b Identity}, what a terminal of that name and version is known to do.
    + {b Guesses} from the environment, [TERM], [COLORTERM], the platform and
      the description the terminfo database has for [TERM] ({!val-env}). A
      description only ever adds a fact, direct color ([RGB], [Tc], or [setrgbf]
      with [setrgbb]), 256 colors ([colors]), bracketed paste ([BE]), focus
      reporting ([fe]), synchronized output ([Sync] as mode 2026) or an eager
      wrap ([am] without [xenl]). Descriptions leave out direct color and
      synchronized output for most terminals that have them, and a terminal
      other than the one [TERM] names (over ssh, or in a multiplexer) may have
      no description here, so a description's silence takes nothing away.
    + The default, [false], [None], [`Ansi16] or [`Wcwidth], or the facts the
      caller assumes in place of the environment ({!assume}).

    Inside tmux, questions about drawing and keys go to tmux itself, which
    answers or stays silent. tmux answers no mode query before 3.6, and none for
    mode 2026 before 3.7, so its facts then come from its identity, by version.
    It brackets the pastes it passes to a pane from 1.7 and handles focus
    reporting from 1.8, and it synchronizes a pane's output from 3.7. It has no
    mode 2027. A question about the terminal outside, such as kitty graphics,
    which tmux passes through untouched, is wrapped for tmux's passthrough, and
    only once passthrough is known to be on. tmux drops wrapped queries unless
    its [allow-passthrough] option is set, which it is not by default since 3.3.

    The environment describes the terminal the process started in, which is not
    always the one it talks to. [TERM_PROGRAM] survives into ssh sessions, GNU
    screen, zellij and tmux. So the environment gives an identity only when the
    terminal is {e directly attached}, that is, when none of [SSH_CONNECTION],
    [SSH_CLIENT], [SSH_TTY], [TMUX], [STY] and [ZELLIJ] is set and [TERM] starts
    with none of [tmux], [screen] and [zellij]. An identity the terminal reports
    itself (XTVERSION) is an answer and holds everywhere.

    {1:batches Batches}

    Queries go in batches, each ending with a primary device attributes query
    (DA1), which every terminal answers, but for {!Widths}, which its last
    cursor position report answers. A terminal answers in order, so the DA1
    reply says that the batch has been answered. The kitty keyboard protocol
    states it as "If an answer for the device attributes is received without
    getting back an answer for the progressive enhancement the terminal does not
    support this protocol" ("Detection of support"). A query still silent when
    its batch's DA1 reply comes is answered "no" where its protocol says so
    (kitty graphics), and stays unknown otherwise, since a terminal without
    DECRQM says nothing of its modes. The program writes the batches, waits for
    their DA1 replies and passes the replies here. *)

(** {1:caps Capabilities} *)

type t = {
  colors : [ `Ansi16 | `Ansi256 | `Truecolor ];
      (** The colors the terminal shows, the 16 of its palette's first entries,
          the 256 of its whole palette, or any 24-bit color. No query tells it.
          24-bit color comes from kitty's identity, a [COLORTERM] of [truecolor]
          or [24bit], a [TERM] that ends in [-direct], a Windows console, or the
          description's direct color. 256 colors come from a [TERM] that
          contains [256color], or a description with 256 colors or more
          ([colors]). Any other terminal gets 16. *)
  dumb : bool;
      (** The terminal performs no control function and shows only text, as a
          printer does. [TERM] says so when it is unset, [dumb] or [unknown],
          outside a Windows console, which has no [TERM]. *)
  eager_wrap : bool;
      (** The terminal moves the cursor to the next line as soon as it prints a
          character in the last column, where a VT100 waits for the next
          character, so that a line as wide as the screen and its line end take
          one row. The description says so with [am] and without [xenl]
          (terminfo(5), "Glitches and Brain Damage"). A Windows console waits,
          since [Tty.enable_vt] has it wait. A program that must keep the cursor
          on its row writes nothing in the last column of such a terminal. *)
  lifts_resting_cursor : bool;
      (** When its width changes, the terminal moves a cursor that rests in the
          first column of a row other than its first up to the row above, so the
          next line printed lands on the line before it. WezTerm does (its issue
          7994, "Width resize corrupts cursor position when it is at column 0 on
          a non-first line"), so its identity, or a [TERM] that names it, gives
          this. A program that keeps the cursor on a row of its own rests it in
          the second column of such a terminal. *)
  kitty_keyboard : int option;
      (** [Some flags] when the terminal has answered the kitty keyboard
          protocol's query, with the enhancement flags it last reported. It is
          [None] under [VHS_RECORD]. *)
  kitty_graphics : bool;
      (** The kitty graphics protocol, from the answer to its query or kitty's
          identity. Silence before the DA1 reply of the batch that asked says
          the terminal does not have it. *)
  bracketed_paste : bool;
      (** Bracketed paste (mode 2004), from the mode's report, kitty's identity,
          tmux from 1.7, a Windows console, or the description's [BE]. *)
  focus_tracking : bool;
      (** The terminal handles focus reporting (mode 1004), from the mode's
          report, tmux from 1.8, or the description's [fe]. When reports come is
          up to the terminal. tmux sends a pane the focus changes only while its
          [focus-events] option is on. *)
  unicode_width : Matrix_text.width_method;
      (** How the terminal measures text, which its answer for mode 2027
          (grapheme cluster mode, "Unicode in Terminals") tells, as a DECRPM
          setting (VT510). Set, permanently set and reset give [`Unicode], since
          a terminal that answers reset has the mode and a program can set it
          ({!grapheme_mode}). Permanently reset gives [`Wcwidth], since with the
          mode off a terminal behaves "as if this specification was not
          implemented at all". A terminal that answers "not recognized", or does
          not answer, does not have the mode and measures each character by
          itself, as wcwidth does, so it gets [`Wcwidth] unless its identity or
          [TERM] says otherwise. kitty answers "not recognized" and places
          grapheme clusters, so its identity gives [`Unicode]. *)
  grapheme_mode : bool option;
      (** Mode 2027 as the terminal answered. [Some false] says the mode is
          reset and a program can set it. A program must set it only then, since
          [Matrix_terminal.reset_state] resets it. [Some true] says the mode is
          set or permanently set. [None] when the terminal does not have the
          mode, has it permanently reset, or has not answered. *)
  sgr_pixels : bool;
      (** SGR pixel mouse reports (mode 1016), from the mode's report. *)
  color_scheme_updates : bool;
      (** Color scheme change reports (mode 2031), from the mode's report. *)
  explicit_width : bool;
      (** The terminal sizes text as OSC 66 says, as {!Widths} measures ([w=1]
          moves the cursor one cell). *)
  explicit_cursor_positioning : bool;
      (** The terminal may measure a grapheme otherwise than the program, so a
          renderer places the cursor after each wide grapheme. It comes from the
          identity of tmux or alacritty, or from any multiplexer. *)
  scaled_text : bool;  (** OSC 66 scaled text ([s=2]), as {!Widths} measures. *)
  sixel : bool;
      (** SIXEL graphics, attribute 4 in the DA1 reply to {!Probe}. *)
  graphics : [ `Kitty | `Sixel | `Blocks ];
      (** How the terminal is sent pictures ([Matrix_screen]). [MATRIX_GRAPHICS]
          set to [kitty], [sixel] or [blocks] decides it. Otherwise it is
          [`Kitty] when the terminal that does the work, or the terminal outside
          tmux through its passthrough, has {!kitty_graphics} and is kitty from
          0.28 or Ghostty, the terminals known to draw the protocol's Unicode
          placeholders. It is [`Sixel] when the terminal has {!sixel}, answers
          at least 252 color registers to {!Probe} or does not answer their
          query, and is not Konsole, which erases the text under an image and
          deletes the image when text lands on its top row. It is [`Blocks]
          otherwise. *)
  sync : bool;
      (** Synchronized output (mode 2026), from the mode's report, tmux from
          3.7, the identity of Ghostty, kitty, WezTerm, Alacritty or iTerm, a
          [TERM] that names one of them, or the description's [Sync]. *)
  hyperlinks : bool;
      (** OSC 8 hyperlinks. No query tells it. It comes from the identity of
          tmux, Ghostty, kitty, WezTerm, Alacritty or iTerm, a [TERM] that names
          one of the five terminals, a [COLORTERM] of [truecolor] or [24bit], or
          a Windows console. *)
  notification : [ `Osc9 | `Osc777 ];
      (** The form of desktop notification the terminal shows. No query tells
          it. iTerm2's OSC 9 is the one most terminals read: iTerm2, kitty
          ("kitty also supports the legacy OSC 9 protocol developed by iTerm2",
          its "Desktop notifications"), WezTerm and foot (their lists of escape
          sequences). rxvt-unicode reads only OSC 777, an extension of its own,
          so a [TERM] that starts with [rxvt] gives [`Osc777]. Any other
          terminal gets [`Osc9]. *)
  meta_sends_escape : bool option;
      (** xterm's metaSendsEscape (mode 1036) as the terminal answered.
          [Some false] says the mode is reset and a program can set it. A
          program must set it only then, since [Matrix_terminal.reset_state]
          resets it. While the mode is reset, Alt sends the key's character with
          its high bit set. [Some true] says the mode is set or permanently set.
          [None] when the terminal does not have the mode, has it permanently
          reset, or has not answered. *)
  multiplexer : [ `Tmux | `Screen ] option;
      (** The multiplexer between the program and the terminal. When the
          terminal has answered XTVERSION, it is tmux if the name reported
          contains [tmux], and none otherwise. Before that it is tmux when
          [TMUX] is set, GNU screen when [STY] is set, and else as [TERM]
          starts, with [tmux] or [screen]. *)
  identity : (string * string) option;
      (** The terminal's name and version, from XTVERSION, or, when the terminal
          is directly attached, from [TERM_PROGRAM] and [TERM_PROGRAM_VERSION],
          ["kitty"] when [KITTY_WINDOW_ID] is set, or ["termux"] with
          [TERMUX_VERSION]. *)
}
(** The type for capabilities. *)

(** {1:env Environment} *)

type env
(** The type for the environment variables that bear on capabilities. *)

val env :
  ?terminfo:Matrix_terminfo.t -> (string -> string option) -> win32:bool -> env
(** [env ~terminfo getenv ~win32] is the environment, read once through
    [getenv], such as [Sys.getenv_opt], where an empty value counts as unset.
    [terminfo] is the description of [TERM], if there is one ([Tty.terminfo]
    finds it). [win32] says the terminal is a Windows console. *)

val variable : env -> string -> string option
(** [variable env name] is the value [env] read for [name], one of the variables
    that decide a styling ({!Ansi.Styling.variables}), and [None] for any other
    name. So a run decides its styling ({!Ansi.Styling.decide}) from the
    environment its capabilities come from. *)

(** {1:evidence Evidence} *)

type evidence
(** The type for what is known of a terminal, its environment and the answers
    received so far. *)

val evidence : env -> evidence
(** [evidence env] is the evidence before any answer. *)

val assume : t -> evidence
(** [assume caps] is evidence with no environment, in which every fact that no
    answer gives is as [caps] says. *)

val add : evidence -> Matrix_input.Reply.t -> evidence
(** [add ev r] is [ev] with the reply [r], a report of a DEC private mode
    (DECRPM), the kitty keyboard flags, XTVERSION or a reply to the kitty
    graphics query. Other replies leave [ev] as it is, among them the cursor
    reports and DA1 replies, whose meaning depends on their batch
    ({!val-cursor}, {!answered}). *)

val derive : evidence -> t
(** [derive ev] is the capabilities [ev] gives, by the {!section-precedence}. *)

(** {1:questions Questions} *)

(** The type for batches of queries. *)
type batch =
  | Probe
      (** The startup batch, to the terminal that does the work. It starts with
          S7C1T, so that no reply comes with 8-bit controls. It asks for the
          reports of modes 1016, 2027, 2031, 1004, 2004, 2026 and 1036,
          XTVERSION, the kitty keyboard flags, and, unless a multiplexer is in
          the way, kitty graphics, then the cell's size in pixels ([CSI 16 t])
          and the number of sixel color registers (XTSMGRAPHICS), and ends with
          DA1. It writes nothing on the screen. *)
  | Widths
      (** The measure of text sizing (OSC 66), to the terminal that does the
          work. It writes two spaces with OSC 66 from the first column of the
          cursor's row, one of width 1 and one of scale 2, each after a carriage
          return and followed by a cursor position report. It then returns to
          the first column and erases from there to the end of the screen. A
          program must write it with the cursor on a row that it owns with every
          row below it, at least two, since a space of scale 2 is two rows high.
      *)
  | Passthrough
      (** DA1 alone, wrapped for tmux, which is answered only when tmux passes
          it on. *)
  | Outer
      (** Questions about the terminal outside tmux, wrapped for tmux, the kitty
          graphics query and XTVERSION, then DA1. *)

val ask : evidence -> batch -> string * Matrix_input.Decoder.owed
(** [ask ev b] is the bytes of [b] and the replies it owes the decoder
    ({!Matrix_input.Decoder.expect}). *)

val cursor : evidence -> batch -> index:int -> row:int -> col:int -> evidence
(** [cursor ev b ~index ~row ~col] is [ev] with the [index]th cursor position
    report that [b] asked for, from [0], at [row] and [col], from [0]. *)

val answered : evidence -> batch -> attributes:int list -> evidence
(** [answered ev b ~attributes] is [ev] once the DA1 that ends [b] is answered
    with [attributes]. What [b] asked and was not answered is then known not to
    be there, where its protocol says that silence means so. For {!Widths},
    which its last cursor position report answers,
    [answered ev Widths ~attributes] is [ev]. *)

val next : evidence -> batch -> batch option
(** [next ev b] is the batch to send once [b] is answered, if any. Inside tmux,
    it is {!Passthrough} after {!Probe}, then {!Outer} once passthrough is known
    to be on. *)
