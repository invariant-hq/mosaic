(** A terminal emulator, which applies a program's output to a model of a
    terminal's screen.

    A program running in a terminal writes bytes. Some are graphic characters,
    which the terminal shows, and the others code control functions, which move
    the cursor, edit the screen or ask the terminal something ({!Ansi.Parser}
    reads them). A {e terminal} [t] applies these bytes to a model of a DEC
    VT-series terminal as xterm extends it (xterm's Control Sequences, ctlseqs
    below), and keeps what a renderer shows.

    The {e screen} is a grid of cells, [rows t] high and [cols t] wide. The
    terminal has two screens, primary and alternate, and shows one at a time
    ({!section-screens}). The {e cursor} is the position at which the terminal
    writes ({!section-cursor}). The {e pen} is the rendition SGR selects, the
    hyperlink OSC 8 opens, and whether DECSCA protects the characters written
    next. DECSTBM sets the {e scroll region}, the rows from its {e top margin}
    to its {e bottom margin}, which scrolling moves; at power-up it is the whole
    screen. A {e blank} is a cell an erase leaves ({!section-blanks}). Lines
    scrolled off the top of the primary screen enter the {e scrollback}
    ({!section-rows}). The {e palette} is the colors the terminal's user
    configured, which programs may change ({!section-palette}).

    A program also asks the terminal questions, such as where the cursor is. The
    answers are the terminal's {e replies}, and {!feed} returns them
    ({!section-replies}). It asks the terminal's host for what is not on the
    screen, such as the bell or a desktop notification, and the terminal keeps
    these {e requests} ({!section-requests}). It sets the modes that decide the
    bytes the terminal sends for its user's keys and mouse ({!section-user}).
    The terminal performs no input or output, so the caller writes the replies
    to the program's input. The screen depends only on the bytes fed, and
    splitting them differently across {!feed} calls leaves the same screen.

    The life cycle of a terminal usually goes like this:
    + {!val-create} makes one in its power-up state, with a size, a palette, an
      identity and the extensions it implements ({!section-extensions}).
    + {!feed} applies the program's output as it arrives and returns the
      replies, which the caller writes to the program's input.
    + A renderer reads the screen ({!grid}, {!cursor_pos}, {!val-modes}) and the
      scrollback ({!blit_view}), redraws what changed ({!section-dirty}), and
      calls {!clear_dirty}.
    + The caller acts on the {!val-requests}, and writes the bytes of its user's
      keys ({!keyboard}) and mouse ({!val-mouse}).
    + {!resize} follows the window's size, and {!reset} returns the terminal to
      its power-up state.

    Unless a value's doc says otherwise:
    - rows and columns count from 0, from the top left cell;
    - a value that returns part of [t]'s state, such as {!grid}, returns [t]'s
      own, which the caller must not change;
    - a terminal is mutable and must be used by one domain at a time.

    This feeds [hello] and a cursor position request, [CSI 6 n]. The terminal
    shows the text, and its reply puts the cursor at row 0, column 5, which
    {!Matrix_input.Reply.encode} writes as [CSI 1 ; 6 R]:
    {[
    let t = Vte.create ~rows:24 ~cols:80 () in
    let replies = Vte.feed_string t "hello\x1b[6n" in
    assert (Vte.text t 0 = "hello");
    print_string (Matrix_input.Reply.encode replies)
    ]} *)

type t
(** The type for terminals. *)

type palette = {
  colors : (int * int * int) array;
      (** The 256 indexed colors, from index 0. They are the eight of SGR 30 to
          37, the eight bright ones of SGR 90 to 97, then those only SGR 38 ; 5
          selects. *)
  foreground : int * int * int;  (** The default foreground (OSC 10). *)
  background : int * int * int;  (** The default background (OSC 11). *)
  cursor : int * int * int;  (** The cursor's color (OSC 12). *)
}
(** The type for palettes ({!section-palette}). Each color is red, green and
    blue, from 0 to 255. *)

(** {1:create Creating} *)

val default_palette : palette
(** [default_palette] is the colors of {!Ansi.Color.indexed}, with indexed color
    7 as the default foreground and the cursor's color, and 0 as the default
    background. *)

type identity = {
  attributes : int list;  (** The parameters of its DA1 reply. *)
  secondary : int list;  (** The parameters of its DA2 reply. *)
  version : string option;  (** Its XTVERSION reply, if it answers one. *)
}
(** The type for what a terminal says it is ({!section-extensions}). *)

val vt100 : identity
(** [vt100] is this module's own identity, a VT100 with Advanced Video
    ([CSI ? 1 ; 2 c]), DA2's type 0, version 0 and no options
    ([CSI > 0 ; 0 ; 0 c]), and no XTVERSION reply. *)

(** The type for the protocols beyond the VT core that a terminal may implement
    ({!section-extensions}). *)
type extension =
  | Kitty_keyboard  (** The kitty keyboard protocol's flags. *)
  | Modify_other_keys  (** xterm's modifyOtherKeys. *)
  | Synchronized_output  (** Mode 2026. *)
  | Grapheme_clusters  (** Mode 2027, reported as the width method sets it. *)
  | Color_scheme_reports  (** Mode 2031 and its query, [CSI ? 996 n]. *)
  | In_band_resize  (** Mode 2048. *)
  | Focus_reports  (** Mode 1004. *)
  | Kitty_graphics
      (** The kitty graphics protocol's images, as Unicode placeholders show
          them ({!section-graphics}). *)

val extensions : extension list
(** [extensions] is every extension, in the order of {!type-extension}. *)

val create :
  ?identity:identity ->
  ?extensions:extension list ->
  ?scrollback:int ->
  ?width_method:Matrix_text.width_method ->
  ?palette:palette ->
  ?cell_size:int * int ->
  rows:int ->
  cols:int ->
  unit ->
  t
(** [create ~rows ~cols ()] is a terminal in its power-up state
    ({!section-resets}), with:
    - [identity], what it answers to DA1, DA2 and XTVERSION. Defaults to
      {!vt100}.
    - [extensions], the protocols it implements beyond the VT core. Defaults to
      {!val-extensions}.
    - [scrollback], the number of lines the scrollback keeps, the oldest dropped
      first. Defaults to [10000]; [0] keeps none.
    - [width_method], how wide characters are ({!section-printing}). Defaults to
      [`Unicode].
    - [palette], the palette. Defaults to {!default_palette}. [t] keeps a copy.
    - [cell_size], the size of a cell in pixels, [(width, height)], which it
      reports to [CSI 16 t], and times its size to [CSI 14 t]. Without it, it
      answers neither.

    [rows] and [cols] below [1] are taken as [1].

    Raises [Invalid_argument] if [cols > Matrix_grid.max_width], or if [palette]
    does not have 256 colors or has a channel outside 0..255. *)

(** {1:feeding Feeding output}

    {!feed} reads the bytes as {!Ansi.Parser} tokens. It performs each control
    function this terminal implements, as the sections below say, and ignores
    the others.

    For counts and positions, 0 means the default, as an absent parameter does
    (VT510 §4.3.3). A count acts only as far as it can, so [CSI 99999 C] stops
    at the last column. A parameter may be a string of sub-parameters separated
    by colons, whose meaning the function defines (ECMA-48 §5.4.2). Only SGR
    defines any. In any other control sequence a sub-parameter makes the
    sequence malformed, and it has no effect, so [CSI 2:3 H] does not move the
    cursor.

    CAN ends the sequence or string in progress and shows nothing (VT220
    Programmer Reference Manual ch. 4, "No error character is displayed"). SUB
    ends it too and, wherever it occurs, shows the error character, a reversed
    question mark U+2E2E (VT510 Programmer Information ch. 4).

    Of the control strings, the terminal performs OSC 0 and 2, which set the
    {!title}, OSC 8, which opens a hyperlink ([OSC 8 ; params ; URI]) or closes
    it when the URI is empty, and the OSCs of the palette ({!section-palette}).
    It performs an OSC when the OSC ends with ST or BEL, and only then. It
    ignores an OSC whose text after its number is over 65,536 bytes, and every
    other control string. *)

val feed : t -> bytes -> int -> int -> Matrix_input.Reply.t list
(** [feed t b off len] applies the [len] bytes of [b] from [off] to [t] and is
    the replies they asked for, in order ({!section-replies}). The caller writes
    [Matrix_input.Reply.encode replies] to the program's input, after rewriting
    or dropping any it answers otherwise. It is [[]], with no allocation, when
    there are none. A sequence split across calls is answered by the call that
    completes it.

    Raises [Invalid_argument] if [off] and [len] do not designate a range of
    [b]. *)

val feed_string : t -> string -> Matrix_input.Reply.t list
(** [feed_string t s] is
    [feed t (Bytes.unsafe_of_string s) 0 (String.length s)]. *)

val reset : t -> unit
(** [reset t] does RIS ({!section-resets}). It also drops a sequence or string
    left unfinished by {!feed}. *)

(** {2:printing Printing}

    Characters are placed by {e display unit}, as {!Matrix_text.Segmenter} cuts
    them under the width method {!val-create} was given. A display unit is a
    grapheme cluster, or a wcwidth unit. A control function ends the unit in
    progress. The terminal places a unit as soon as it has a width, before it
    knows that the unit is complete, and places it again from the same position
    when later characters extend it. A unit never narrows. So that the screen
    does not depend on where the output was split, placement follows four rules:
    + with autowrap on, a unit that does not fit before the right margin first
      erases the cells from its position to the margin, then moves to the next
      line. A unit placed narrow and then widened past the margin vacates the
      cells it wrote this way, and a unit that arrives whole erases the same
      cells;
    + with autowrap off, a unit that does not fit ends at the margin, replacing
      what is there;
    + in insert mode (IRM) a unit moves the rest of the line right by its width,
      and by its growth when it widens;
    + a unit wider than the line is narrowed to it.

    A unit of no width that continues nothing, such as U+200B or a combining
    mark after a control function, joins the character in the cell before the
    cursor, or in the cursor's cell when the Last Column Flag is set
    ({!section-cursor}). It changes neither that character's width nor its
    rendition. At the first column, with no cell before the cursor, it is
    dropped. The cell is found from the cursor alone, so a mark after an SGR
    change joins the character before the cursor whatever came before. No cell
    holds a control character, and the code points U+0080 to U+009F never reach
    a cell ({!Ansi.Parser}).

    REP repeats the unit printed just before it, if that unit has a width and
    nothing came between them (ECMA-48 §8.3.103). It repeats it as many times as
    asked, each repetition a unit of its own. The work of a REP is bounded by
    the size of the screen and the scrollback, whatever its count.

    Character sets translate characters before units are formed. The terminal
    has tables for DEC Special Graphics, the UK set and the national replacement
    sets (VT220 Programmer Reference Manual tables 2-5 to 2-15, with the final
    characters ctlseqs lists). SCS designates a set as G0 to G3. SI, SO, LS2 and
    LS3 invoke G0, G1, G2 or G3 into GL, and SS2 and SS3 invoke G2 or G3 for the
    next character. The national sets "are available when you select national
    mode" (§2.4.3), DECNRCM. Out of national mode, a slot designated with one
    leaves characters unchanged, and it translates once national mode is
    selected. A set this terminal has no table for, such as DEC Supplemental or
    a 96-character set, leaves characters unchanged. *)

(** {2:line_feeds Line feeds}

    LF, VT and FF move the cursor down a line, or scroll the region when the
    cursor is at its bottom margin, and keep its column, since "These three
    codes are always processed identically" (VT220 Programmer Reference Manual
    §4.6.6). With line feed/new line mode (LNM, [CSI 20 h]) set, they also move
    it to the first column, as NEL does. IND and NEL do the same in either mode.
    LNM also says that the Return key sends CR LF instead of CR, which
    {!type-modes} reports for whoever encodes the keys.

    A program's output usually reaches its terminal through a tty, whose output
    processing (the ONLCR flag of termios) writes each LF as CR LF. The terminal
    then sees no bare LF, and lines start at the first column. This terminal
    reads the bytes it is fed, as a tty delivers them. To read a program's
    output as a cooked tty shows it, feed [CSI 20 h] first, which sets LNM. The
    two differ only on VT and FF, which LNM also returns to the first column and
    ONLCR leaves alone. *)

(** {2:replies Replies}

    A program asks the terminal questions, and the terminal answers on the
    program's input. The answers are {!Matrix_input.Reply.t} values, the ones a
    program's input decoder reads, which {!Matrix_input.Reply.encode} writes as
    the terminal's bytes. The terminal answers these queries:
    - DA ([CSI c]), and DECID ([ESC Z]), its obsolete form (ctlseqs), with its
      identity's [Primary_attributes], DA2 ([CSI > c]) with its
      [Secondary_attributes], and XTVERSION ([CSI > q]) with its [Version], if
      it has one ({!section-extensions});
    - DSR 5 with [Operating_status `Ok], [CSI 0 n], and CPR (DSR 6) with a
      [Cursor] with no page, [CSI r ; c R], the row counted from the top margin
      in origin mode;
    - DECXCPR ([CSI ? 6 n]) with a [Cursor] on page 1, [CSI ? r ; c ; 1 R]. The
      VT510 manual shows the reply without [?]. xterm sends it, as DEC's other
      answers to [CSI ? ... n] carry it, and the [?] tells the answer apart from
      a modified F3 key ([CSI 1 ; 2 R]);
    - DECRQM, which asks for the state of one mode, [CSI Ps $ p] for an ANSI
      mode and [CSI ? Ps $ p] for a DEC private one, with DECRPM, a [Mode],
      [CSI Ps ; Pm $ y] and [CSI ? Ps ; Pm $ y]. [Pm] is 1 for set, 2 for reset,
      and 0 for a mode this terminal does not recognize (VT320 Programmer
      Information, "Mode Settings"), and 3 and 4 for one permanently set or
      reset. The modes recognized are those of {!type-modes} and of the mouse
      ({!section-user}), but those of an extension the terminal lacks. Modes 47,
      1047 and 1049 report the alternate screen. Mode 1036 is permanently set,
      since Alt sends an ESC before the key. Mode 2027, with
      [Grapheme_clusters], is permanently set when the width method is
      [`Unicode], permanently reset when it is [`Wcwidth], and not recognized
      when it is [`Unicode_wide], which no setting of the mode describes;
    - the kitty keyboard protocol's [CSI ? u] with a [Keyboard_flags], and
      [CSI ? 996 n] with the terminal's [Color_scheme] ({!set_color_scheme}),
      when it has the extension;
    - a color spec [?] in OSC 4, 10, 11 or 12, which asks for a color of the
      palette ({!section-palette}), with a [Color] from the palette as it is,
      the OSC that would set it, such as [OSC 11 ; rgb:0000/0000/0000]. The
      reply ends as the query did, with BEL or ST, since xterm "uses the same
      terminator used in a query" (ctlseqs, "Operating System Commands"). *)

(** {2:resets Resets}

    RIS ([ESC c], or {!reset}) returns the terminal to its power-up state. It
    empties the scrollback, clears the title and restores the whole palette
    {!val-create} was given, the colors of OSC 10, 11 and 12 included, since RIS
    "replaces all set-up features with their saved settings" (VT510, RIS). xterm
    (patch 398) restores the colors of OSC 4 and keeps those of OSC 10, 11 and
    12. At power-up the screens are blank, the cursor is a visible blinking
    block at the top left, autowrap is on and the other modes are off, the tab
    stops are every eight columns, and the pen and the character sets are their
    defaults.

    DECSTR ([CSI ! p]) returns each terminal setting to its power-up value
    (VT510, DECSTR). These are the insert, origin, autowrap, reverse-wrap and
    national modes, cursor keys, cursor visibility and the margins. The pen and
    the character sets return to their defaults, and both DECSC slots to the
    power-up cursor state. The VT510's table gives "No autowrap", its own
    power-up value. This terminal powers up with autowrap on, so DECSTR sets it
    on. The screen, the cursor position, the active screen, LNM, the cursor
    style, bracketed paste, the tab stops, the title and the palette stay as
    they are, since the table lists none of LNM, DECSCUSR or the colors. *)

(** {1:cursor The cursor}

    The {e cursor state} decides where and how the next character is written. It
    is the cursor's position, the {e Last Column Flag}, the pen, the character
    sets designated as G0 to G3 with the one invoked into GL and a pending
    single shift, and origin mode (DECOM). DECSC saves exactly this and DECRC
    restores it. Autowrap (DECAWM) is a setting outside the cursor state, so
    DECRC restores the flag and leaves autowrap as it is. With autowrap off the
    flag is never set, so DECRC then restores the position without it.

    A character printed into the last column with autowrap on leaves the cursor
    on it with the Last Column Flag set. The next character first moves to the
    start of the next line. Every function that sets the cursor position or
    changes cells clears the flag, and so does resetting DECAWM. HT and CHT keep
    it, since they never leave the last column. SGR, modes that leave the cursor
    where it is, DECSC and reports keep it too. With autowrap off the flag is
    never set, and a character printed at the last column replaces the one
    there. Cursor reports leave the flag out and give the last column as the
    cursor's column.

    How the cursor looks is outside the cursor state. DECTCEM shows or hides it,
    and DECSCUSR, [CSI Ps SP q], sets its style. 0, 1 or no parameter is a
    blinking block, 2 a steady block, 3 and 4 a blinking and a steady underline
    (VT510 Programmer Information, DECSCUSR), 5 and 6 a blinking and a steady
    bar, and 7 the style at power-up (ctlseqs). Other values have no effect.
    Both screens share the style, and DECSC does not save it. *)

val cursor_pos : t -> int * int
(** [cursor_pos t] is the cursor's [(row, col)], with [row < rows t] and
    [col < cols t]. *)

val last_column : t -> bool
(** [last_column t] is the Last Column Flag. While it is set, the cursor is on
    the character just printed in the last column, and the next one wraps first.
    It implies autowrap and [snd (cursor_pos t) = cols t - 1]. *)

val cursor_style : t -> Ansi.cursor_style * bool
(** [cursor_style t] is the cursor's shape and whether it blinks, as DECSCUSR
    last set them, [(`Block, true)] at power-up. *)

(** {1:screens Screens}

    The terminal has two screens, primary and alternate. Each has its own cells,
    its own wrap marks and its own DECSC slot, which holds the power-up cursor
    state until DECSC fills it. Everything else is shared. xterm's modes 47,
    1047, 1048 and 1049 compose four steps, which switch screens, erase the
    alternate screen, do DECSC and do DECRC:
    - 47 switches;
    - 1047 switches, and erases the alternate screen when leaving it;
    - 1048 is DECSC when set and DECRC when reset;
    - 1049 is DECSC, a switch and an erase when set, and the reset of 1047 then
      DECRC when reset.

    A row's {e wrap mark} says that its text goes on at the start of the next
    row because autowrap moved there. Only printing sets it. An edit that
    reaches the row's last column or moves its cells sideways clears it, and a
    scroll moves it with the row. On the last row of the screen, below the
    scroll region, the text has no next row, so autowrap sets no mark there and
    starts the line again on the same row. Rows scrolled off the top of the
    primary screen, while the scroll region starts at the first row, enter the
    scrollback with their mark. *)

val rows : t -> int
(** [rows t] is the number of rows. *)

val cols : t -> int
(** [cols t] is the number of columns. *)

val grid : t -> Matrix_grid.t
(** [grid t] are the cells of the active screen. The caller must not change
    them. *)

val resize : t -> rows:int -> cols:int -> Matrix_input.Reply.t list
(** [resize t ~rows ~cols] changes the screen size and keeps the text without
    reflowing it, and is the report the terminal sends: the [Resize] of mode
    2048, with no pixel size (0), when the size changed while the mode is set,
    and [[]] otherwise. Each screen keeps the rows around its cursor, which for
    the inactive screen is the cursor in its DECSC slot. Shrinking drops rows
    below that cursor first, then moves rows off the top, into the scrollback
    for the primary screen. Growing takes the newest scrollback lines back onto
    the primary screen, then adds rows at the bottom. The rows and columns a
    resize adds are blank on {!Ansi.Color.default_bg}. Rows keep their wrap
    marks, and a row back from the scrollback has no protection. Cursors and
    DECSC slots move with their rows and lose the Last Column Flag, the margins
    become the whole screen, and a unit in progress is ended. Tab stops stay,
    and new columns have one every eight columns. [rows] and [cols] below [1]
    are taken as [1]. A resize to the current size changes nothing, so a size
    report that repeats the size keeps the margins, the Last Column Flag and the
    changes ({!section-dirty}).

    Raises [Invalid_argument], and changes nothing, if
    [cols > Matrix_grid.max_width]. *)

(** {2:blanks Blanks}

    Every cell an erase, insertion, deletion or scroll empties becomes a blank,
    a space with no attributes, unprotected, on the pen's background color.
    ECMA-48 puts such cells "into the erased state" (§8.3.26, DCH), and the
    color is xterm's, which terminfo advertises as [bce]. A wide character such
    an operation cuts becomes blanks of the same color. Selective erase (DECSED,
    DECSEL) instead turns the characters of unprotected cells into spaces, drops
    their hyperlinks and keeps their rendition, since it "does not affect video
    line attributes or video character attributes (SGR)" (VT220 Programmer
    Reference Manual §4.12). *)

(** {1:modes Modes and title} *)

type modes = {
  insert : bool;  (** IRM (SM 4). *)
  new_line : bool;
      (** LNM (SM 20). LF, VT and FF also return to the first column
          ({!section-line_feeds}), and the Return key sends CR LF. *)
  origin : bool;  (** DECOM (DECSET 6). *)
  auto_wrap : bool;  (** DECAWM (DECSET 7). *)
  reverse_wrap : bool;
      (** xterm's reverse-wraparound (DECSET 45). With autowrap, BS and CUB go
          back across the wraps that printing made. *)
  cursor_keys : bool;  (** DECCKM (DECSET 1). Cursor keys send [ESC O]. *)
  cursor_visible : bool;  (** DECTCEM (DECSET 25). *)
  bracketed_paste : bool;  (** xterm's bracketed paste (DECSET 2004). *)
  alternate_screen : bool;  (** The alternate screen is active. *)
  national : bool;
      (** DECNRCM (DECSET 42). The national replacement character sets are
          available. *)
  keypad : bool;
      (** DECNKM (DECSET 66), which DECKPAM ([ESC =]) also sets and DECKPNM
          ([ESC >]) resets. The keypad sends its application forms. *)
  focus_reports : bool;  (** Focus reports (DECSET 1004). *)
  synchronized_output : bool;
      (** Synchronized output (DECSET 2026). The program is between the start
          and the end of an update. This terminal shows every change as it is
          fed, so a reader that must see only whole updates reads while the mode
          is off. *)
  color_scheme_reports : bool;  (** Color scheme reports (DECSET 2031). *)
  resize_reports : bool;  (** In-band resize reports (DECSET 2048). *)
}
(** The type for the terminal's modes. At power-up autowrap is on, the cursor
    visible and the others off. The modes of an extension the terminal lacks
    stay off ({!section-extensions}). *)

val modes : t -> modes
(** [modes t] are [t]'s modes. *)

val title : t -> string
(** [title t] is the title OSC 0 or OSC 2 last set, [""] at power-up. *)

(** {1:user The user's side}

    The bytes a terminal sends for its user's keys and mouse depend on modes the
    program sets. Beside DECCKM, DECNKM and LNM ({!type-modes}), the terminal
    keeps:
    - the kitty keyboard protocol's flags, on a stack for each screen, with
      [Kitty_keyboard]. [CSI > flags u] pushes flags, [CSI < n u] pops [n]
      entries (default 1), [CSI = flags ; mode u] sets the current flags (mode
      1, the default), adds them (2) or takes them away (3), and [CSI ? u] asks
      for them (kitty keyboard protocol, "Progressive enhancement"). The flags
      are [0] while a stack is empty;
    - xterm's modifyOtherKeys level, with [Modify_other_keys], which
      [CSI > 4 ; v m] sets and [CSI > 4 m] and [CSI > 4 n] reset to 0;
    - one mouse tracking mode and one format, as xterm does (ctlseqs, "Mouse
      Tracking"). Setting mode 9, 1000, 1002 or 1003 makes it the tracking mode,
      and resetting any of them turns tracking off. Mode 1006 makes SGR the
      format and 1015 URXVT, and resetting the format in use returns to X10's
      bytes. This terminal has neither UTF-8 coordinates (1005) nor SGR pixels
      (1016).

    RIS resets them, and DECSTR resets DECNKM. *)

val keyboard : t -> Matrix_input.Keystroke.encoding
(** [keyboard t] is the keyboard state the program has set, which
    {!Matrix_input.Keystroke.encode} turns a key into bytes with. *)

val mouse : t -> Matrix_input.Mouse_report.encoding option
(** [mouse t] is the mouse reporting the program has set, [None] while tracking
    is off. {!Matrix_input.Mouse_report.encode} turns a report into bytes with
    it. *)

(** {1:extensions Identity and extensions}

    A terminal says what it is in its answers to DA1, DA2 and XTVERSION, its
    {e identity} ({!type-identity}), and implements the VT core this module
    describes and the {e extensions} ({!type-extension}) it was created with. A
    terminal without an extension behaves as one that never had it: the
    sequences that set it do nothing, DECRQM reports its mode as not recognized,
    and its queries get no answer. *)

val identity : t -> identity
(** [identity t] is the identity [t] answers with. *)

(** {1:graphics Kitty graphics}

    With [Kitty_graphics], the terminal reads the kitty graphics protocol's
    commands (APC [G]) as far as Unicode placeholders need them. It answers the
    query ([a=q]) with OK. It holds the images transmitted in raw pixels ([a=t]
    or [a=T], [f=24] or [f=32], in chunks with [m=1]) whose payload has the
    length their width and height make, without keeping their pixels, each with
    a virtual placement when [a=T] has [U=1] or [a=p] with [U=1] follows. It
    deletes an image with [a=d] and [d=i] or [d=I], and every image with [d=a],
    [d=A] or no [d]. A placeholder, U+10EEEE and its marks, is stored as U+FFFC
    when it is printed while the terminal holds, with a virtual placement, an
    image whose id's three low bytes are the RGB or palette entry of its
    foreground, and as U+FFFD otherwise. A placeholder of an image the terminal
    deletes then reads U+FFFD. *)

(** {1:requests Requests to the host}

    A program asks the terminal's host for what is not on the screen. A
    {e request} ({!type-request}) is one such ask, which the terminal keeps, in
    order, for its caller to act on:
    - BEL, outside a control string, rings the bell;
    - OSC 9, iTerm2's notification, posts a desktop notification of its text,
      and OSC 777 [; notify ; title ; body], rxvt's, one with a title, as foot,
      WezTerm and Ghostty read both;
    - OSC 52 [; selection ; data], with [data] in base64, sets a clipboard ([c]
      the clipboard, [p] the primary selection). A read, whose data is [?], asks
      for a clipboard this terminal does not have, and gets no answer. *)

type request =
  | Bell
  | Notification of { title : string; body : string }
      (** A desktop notification, its title [""] for OSC 9. *)
  | Clipboard of { selection : string; data : string }
      (** A clipboard write, its data decoded. *)

val requests : t -> request list
(** [requests t] is the requests the program has made since {!val-create} or
    {!clear_requests}, oldest first. *)

val clear_requests : t -> unit
(** [clear_requests t] forgets the requests. *)

(** {1:scheme Color scheme}

    The terminal's {e color scheme} is dark or light, as its user chose it, and
    dark at power-up. A program asks for it with [CSI ? 996 n] and, with mode
    2031 set, is told of each change (contour, "Dark and Light Mode detection").
    RIS leaves it as it is. *)

val color_scheme : t -> [ `Dark | `Light ]
(** [color_scheme t] is [t]'s color scheme. *)

val set_color_scheme : t -> [ `Dark | `Light ] -> Matrix_input.Reply.t list
(** [set_color_scheme t s] is the change of [t]'s color scheme to [s] by its
    user, and is the report the terminal sends: a [Color_scheme] when the scheme
    changed while mode 2031 is set, and [[]] otherwise. The caller writes it to
    the program's input. *)

(** {1:palette The palette}

    The palette is the colors the emulated terminal's user configured. These are
    the 256 indexed colors SGR selects, the default foreground and background,
    and the cursor's color. {!val-create} is given it, as it is given the size,
    and programs may change it, as xterm lets them (ctlseqs, "Operating System
    Commands"):
    - OSC 4 [; c ; spec], with any number of pairs, sets indexed color [c], from
      0 to 255;
    - OSC 10, 11 and 12 [; spec] set the default foreground, the default
      background and the cursor's color. "Each successive parameter changes the
      next color in the list", so [OSC 10 ; a ; b] sets the foreground to [a]
      and the background to [b];
    - a spec [?] asks for the color instead ({!section-replies});
    - OSC 104 [; c] resets the indexed colors listed, all of them when none is,
      and OSC 110, 111 and 112 reset the foreground, the background and the
      cursor's color, each to the palette {!val-create} was given.

    {!Matrix_input.Reply.rgb_of_spec} reads a spec, such as [rgb:r/g/b],
    [#rrggbb] or an X11 color name such as [ghost white]. OSC 4 and OSC 104 stop
    at their first entry in error, as xterm does. In OSC 4 that is an index
    other than a number from 0 to 255, or a spec that names no color. In OSC 104
    it is an entry other than a number, and a number above 255 is skipped. In
    OSC 10, 11 and 12, a spec that names no color is skipped. A change marks
    every row and the cursor changed ({!section-dirty}).

    Cells keep colors as the pen gave them. The defaults (SGR 39 and 49, or no
    color) are {!Ansi.Color.default_fg} and {!Ansi.Color.default_bg}, indexed
    colors keep their index, and RGB colors are kept as they are. The palette
    alone says what the defaults and the indices show, so a change of the
    palette changes no cell. A renderer resolves the default and indexed intents
    of {!Ansi.Color} through {!val-palette} when it draws. *)

val palette : t -> palette
(** [palette t] is the palette as programs left it. The caller must not change
    its [colors]. *)

(** {1:rows Rows and the scrollback}

    Rows are numbered from the top of the screen, [0] to [rows t - 1], and on
    upward into the scrollback. Row [-1] is the line that last scrolled off the
    top, and row [-(scrollback_size t)] the oldest one kept. A scrollback line
    is the row as it left the screen. It keeps each grapheme with its width,
    column, rendition and hyperlink, and the row's wrap mark, and drops
    protection. Past its last cell that shows something, it is blank. Any other
    row is blank, with no wrap mark.

    The screen's rows are also the cells of {!grid}. *)

val scrollback_capacity : t -> int
(** [scrollback_capacity t] is the number of lines the scrollback keeps. *)

val scrollback_size : t -> int
(** [scrollback_size t] is the number of lines in the scrollback, [0] while the
    alternate screen is active. *)

val wrapped : t -> int -> bool
(** [wrapped t y] is the wrap mark of row [y] ({!section-screens}), which says
    that its text goes on at the start of row [y + 1]. *)

val text : t -> int -> string
(** [text t y] is the text of row [y], the grapheme of each cell with a space
    for an empty one, without the spaces that end it. The scrollback's lines,
    oldest first, are [List.init n (fun i -> text t (i - n))] with
    [n = scrollback_size t]. *)

val blit_row : t -> int -> Matrix_grid.t -> y:int -> unit
(** [blit_row t r dst ~y] writes row [r] of [t] on row [y] of [dst]. It writes
    the row's cells, with their rendition and hyperlinks, as far as [dst] is
    wide, then blanks on {!Ansi.Color.default_bg}.

    Raises [Invalid_argument] if [y] is not a row of [dst]. *)

val blit_view : t -> offset:int -> Matrix_grid.t -> unit
(** [blit_view t ~offset dst] writes on [dst] the view [offset] lines up into
    the scrollback: each row [y] of [dst] gets row [y - offset] of [t], as
    {!blit_row} writes it. [offset] is clamped to \[[0];{!scrollback_size}\], so
    [0] shows the screen, and so does any [offset] while the alternate screen is
    active. *)

(** {1:dirty Changes}

    A renderer redraws what changed since it last looked. Writing cells, moving
    the cursor, showing or hiding it, changing its style, changing the
    rendition, switching screens, resizing and changing the palette mark [t]
    changed. Erasing a scrollback that holds lines (ED 3) marks [t] changed and
    no row, since the rows are those of the screen. Setting the title or another
    mode leaves the marks as they are. *)

val is_dirty : t -> bool
(** [is_dirty t] is [true] iff [t] changed since {!clear_dirty}. *)

val dirty_rows : t -> int list
(** [dirty_rows t] are the rows to redraw since {!clear_dirty}, in increasing
    order. They are the rows whose cells changed, and every row after a switch
    of screens, a resize, a reset or a change of the palette. *)

val is_cursor_dirty : t -> bool
(** [is_cursor_dirty t] is [true] iff, since {!clear_dirty}, the cursor moved,
    was shown or hidden, or changed style, or [t] was resized or its palette
    changed. *)

val clear_dirty : t -> unit
(** [clear_dirty t] forgets the changes. *)

(** {1:debug Debugging} *)

val to_string : t -> string
(** [to_string t] is the text of the screen, a line a row, separated by
    newlines, trailing spaces kept. *)
