(** Escape sequences that control a terminal, and the modules that style text
    and read sequences back.

    A terminal shows the characters it receives and performs the
    {e control functions} coded among them, such as moving the cursor, setting a
    color or switching a mode. A control function is coded as an
    {e escape sequence}, a run of bytes that begins with ESC. This module builds
    the escape sequences a program sends. A sequence is a value of type
    {!type-t}, a function that writes its bytes to a {e writer} ({!Writer}), the
    buffer a program fills with a frame and then sends to the terminal.
    Sequences combine as functions do, without building strings, and
    {!to_string} runs one to get its bytes.

    The submodules cover the rest of a terminal's output. {!Color}, {!Attr} and
    {!Style} describe how text looks, and {!Style.emit} writes the codes that
    turn one style into another. {!Sgr_state} tracks the style a terminal is in,
    for a renderer that writes cell after cell. {!Styling} decides how much of a
    style a destination shows, from the program's choice, the user's environment
    and the terminal. {!Parser} reads a stream that holds escape sequences, such
    as a program's output, back into tokens.

    Unless a value's doc says otherwise:
    - A count, such as the [n] of {!cursor_up}, below [0] is [0], and a count of
      [0] writes nothing, since a terminal reads an absent or zero count as [1].
    - Rows and columns are 1-based, and one below [1] is [1].
    - A color component is clamped to \[[0];[255]\].
    - A string argument, such as a title or a URL, is written as it is. A string
      that holds a control function, such as the ST or BEL that ends a control
      string, breaks the sequence it is written in, so text from someone else
      goes through {!sanitize} first.

    This fills a writer with a frame and empties it for the next one. The writer
    keeps its storage, so the frames after the first reuse it.
    {[
    let w = Ansi.Writer.create 4096 in
    Ansi.emit
      (Ansi.seq [ Ansi.cursor_position ~row:10 ~col:5; Ansi.literal "Hi" ])
      w;
    (* send the first [Ansi.Writer.len w] bytes of [Ansi.Writer.buffer w] *)
    Ansi.Writer.reset_pos w
    ]} *)

(** {1:types Sequences} *)

type writer = Writer.t
(** The type for writers, {!Writer.t}. *)

type t = writer -> unit
(** The type for escape sequences and other output. [s w] writes [s]'s bytes to
    [w]. *)

(** {1:combinators Building output} *)

val empty : t
(** [empty] writes nothing. *)

val literal : string -> t
(** [literal s] writes [s]. *)

val char : char -> t
(** [char c] writes [c]. *)

val concat : t -> t -> t
(** [concat a b] writes [a], then [b]. *)

val seq : t list -> t
(** [seq ss] writes the elements of [ss] in order. *)

val bytes : bytes -> off:int -> len:int -> t
(** [bytes b ~off ~len] writes the [len] bytes of [b] from [off].

    Raises [Invalid_argument] when it runs, if [off] and [len] do not designate
    a range of [b]. *)

val utf8 : int -> t
(** [utf8 u] writes the UTF-8 encoding of the Unicode scalar value [u], and of
    U+FFFD if [u] is negative, a surrogate (U+D800..U+DFFF) or above U+10FFFF.
*)

(** {1:output Running output} *)

val emit : t -> writer -> unit
(** [emit s w] writes [s] to [w]. *)

val to_string : t -> string
(** [to_string s] is the bytes [s] writes. *)

(** {1:csi Control sequences and SGR}

    CSI, the control sequence introducer, is [ESC \[]. A {e control sequence} is
    CSI, parameters separated by [;], and a final byte. SGR, SELECT GRAPHIC
    RENDITION, is the control sequence with final byte [m]. It sets the colors
    and attributes of the characters written after it, and its parameters are
    called {e codes}. *)

val esc : string -> t
(** [esc body] writes [ESC \[] followed by [body]. *)

val csi : params:string -> command:char -> t
(** [csi ~params ~command] writes [ESC \[], [params], then [command]. *)

val sgr : int list -> t
(** [sgr codes] writes the SGR sequence of [codes], and nothing if [codes] is
    [[]]. *)

val sgr_direct : ((int -> unit) -> unit) -> writer -> unit
(** [sgr_direct f w] writes an SGR sequence whose codes are those [f] passes to
    the function it receives, in order, and builds no list. If [f] passes none,
    it writes [ESC \[ m], which a terminal reads as [0]. *)

val reset : t
(** [reset] is [CSI 0 m], which clears every color and attribute. *)

(** {1:cursor Cursor} *)

(** {2:cursor_rel Relative movement} *)

val cursor_up : n:int -> t
(** [cursor_up ~n] is CUU, which moves the cursor up [n] lines. *)

val cursor_down : n:int -> t
(** [cursor_down ~n] is CUD, which moves the cursor down [n] lines. *)

val cursor_forward : n:int -> t
(** [cursor_forward ~n] is CUF, which moves the cursor right [n] columns. *)

val cursor_back : n:int -> t
(** [cursor_back ~n] is CUB, which moves the cursor left [n] columns. *)

val cursor_next_line : n:int -> t
(** [cursor_next_line ~n] is CNL, which moves the cursor to column 1, [n] lines
    down. *)

val cursor_previous_line : n:int -> t
(** [cursor_previous_line ~n] is CPL, which moves the cursor to column 1, [n]
    lines up. *)

(** {2:cursor_abs Absolute positioning} *)

val cursor_horizontal_absolute : int -> t
(** [cursor_horizontal_absolute col] is CHA, which moves the cursor to column
    [col]. *)

val cursor_vertical_absolute : int -> t
(** [cursor_vertical_absolute row] is VPA, which moves the cursor to row [row].
*)

val cursor_position : row:int -> col:int -> t
(** [cursor_position ~row ~col] is CUP, which moves the cursor to row [row] and
    column [col]. *)

(** {2:cursor_state Saving the position} *)

val cursor_save : t
(** [cursor_save] is [CSI s], which saves the cursor position for
    {!cursor_restore}. *)

val cursor_restore : t
(** [cursor_restore] is [CSI u], which moves the cursor to the position
    {!cursor_save} saved. Without an earlier {!cursor_save}, the terminal
    decides where it goes. *)

(** {2:cursor_appearance Appearance} *)

type cursor_shape =
  [ `Default
  | `Blinking_block
  | `Block
  | `Blinking_underline
  | `Underline
  | `Blinking_bar
  | `Bar ]
(** The type for cursor shapes, the values DECSCUSR selects. [`Default] is the
    terminal's own shape. *)

val cursor_style : shape:cursor_shape -> t
(** [cursor_style ~shape] is DECSCUSR, which sets the cursor shape to [shape].
*)

type cursor_style = [ `Block | `Line | `Underline ]
(** The type for cursor shapes without their blinking, the shapes [Terminal] and
    [Screen] take. {!cursor_shape_of_style} adds the blinking. *)

val cursor_shape_of_style : style:cursor_style -> blinking:bool -> cursor_shape
(** [cursor_shape_of_style ~style ~blinking] is the {!cursor_shape} of [style],
    blinking if [blinking] is [true]. [`Line] is a bar. *)

val cursor_color : r:int -> g:int -> b:int -> t
(** [cursor_color ~r ~g ~b] is OSC 12, which sets the cursor color to
    [(r, g, b)]. A program that sets it resets it before it exits, with
    {!reset_cursor_color} or {!reset_cursor_color_fallback}. *)

val reset_cursor_color : t
(** [reset_cursor_color] is OSC 112, which gives the cursor its default color.
*)

val reset_cursor_color_fallback : t
(** [reset_cursor_color_fallback] is OSC 12 with the color ["default"], which
    gives the cursor its default color in terminals without OSC 112. *)

(** {1:screen Screen} *)

val clear : t
(** [clear] is ED 2, which erases the whole screen and leaves the cursor where
    it is. *)

val home : t
(** [home] is CUP, which moves the cursor to row 1, column 1. *)

val clear_and_home : t
(** [clear_and_home] is {!home} followed by {!clear}. *)

type erase_display_mode = [ `Below | `Above | `All | `Scrollback ]
(** The type for the parts of the screen ED erases. [`Below] is from the cursor
    to the end of the screen, [`Above] from the start of the screen to the
    cursor, [`All] the whole screen, and [`Scrollback] the lines scrolled off
    the screen. *)

val erase_display : mode:erase_display_mode -> t
(** [erase_display ~mode] is ED, which erases the part [mode] of the screen. *)

val erase_below_cursor : t
(** [erase_below_cursor] is [CSI J], which erases from the cursor to the end of
    the screen. *)

type erase_line_mode = [ `Right | `Left | `All ]
(** The type for the parts of the line EL erases. [`Right] is from the cursor to
    the end of the line and [`Left] from the start of the line to the cursor. *)

val erase_line : mode:erase_line_mode -> t
(** [erase_line ~mode] is EL, which erases the part [mode] of the cursor's line.
*)

val insert_lines : n:int -> t
(** [insert_lines ~n] is IL, which inserts [n] blank lines at the cursor's line.
*)

val delete_lines : n:int -> t
(** [delete_lines ~n] is DL, which deletes [n] lines from the cursor's line. *)

val scroll_up : n:int -> t
(** [scroll_up ~n] is SU, which scrolls the screen's contents up [n] lines. *)

val scroll_down : n:int -> t
(** [scroll_down ~n] is SD, which scrolls the screen's contents down [n] lines.
*)

val set_scrolling_region : top:int -> bottom:int -> t
(** [set_scrolling_region ~top ~bottom] is DECSTBM, which limits scrolling to
    rows [top] to [bottom].

    Raises [Invalid_argument] when it runs, if [top < 1] or [bottom <= top]. *)

val reset_scrolling_region : t
(** [reset_scrolling_region] is DECSTBM without parameters, which lets the whole
    screen scroll. *)

(** {1:colors Colors} *)

val set_foreground : r:int -> g:int -> b:int -> t
(** [set_foreground ~r ~g ~b] is SGR [38;2;r;g;b], which sets the foreground to
    the RGB color [(r, g, b)]. *)

val set_background : r:int -> g:int -> b:int -> t
(** [set_background ~r ~g ~b] is SGR [48;2;r;g;b], which sets the background to
    the RGB color [(r, g, b)]. *)

val reset_foreground : t
(** [reset_foreground] is SGR 39, which sets the default foreground. *)

val reset_background : t
(** [reset_background] is SGR 49, which sets the default background. *)

(** {1:terminal Window and text width} *)

val set_title : title:string -> t
(** [set_title ~title] is OSC 0, which sets the window title and icon name to
    [title], without the characters an OSC leaves out ({!section-osc}). *)

val explicit_width : width:int -> text:string -> t
(** [explicit_width ~width ~text] is OSC 66 with [w=width], which asks the
    terminal to show [text] in [width] cells, without the characters an OSC
    leaves out ({!section-osc}). *)

val explicit_width_bytes : width:int -> bytes:bytes -> off:int -> len:int -> t
(** [explicit_width_bytes ~width ~bytes ~off ~len] is {!explicit_width} of the
    [len] bytes of [bytes] from [off].

    Raises [Invalid_argument] when it runs, if [off] and [len] do not designate
    a range of [bytes]. *)

(** {1:osc Operating system commands}

    An OSC, OPERATING SYSTEM COMMAND, is [ESC \]], a payload, and a terminator.
    ECMA-48's terminator is ST, [ESC \\ ], and xterm also accepts BEL. Its text
    is "composed of printable characters" (xterm's Control Sequences,
    "Definitions"), and a control in it would end the sequence early or start
    another. So every OSC this module writes, {!val-osc} included, leaves out of
    the strings it is given the controls (C0, DEL and the C1 controls
    U+0080..U+009F, ST among them) and the bytes that are not UTF-8. A string
    that fills a field followed by another also leaves out [;], the field
    separator. *)

type terminator = [ `Bel | `St ]
(** The type for OSC terminators. *)

val osc : ?terminator:terminator -> payload:string -> t
(** [osc ~terminator ~payload] is the OSC [payload], ended by [terminator],
    which defaults to [`St]. *)

(** {2:hyperlinks Hyperlinks}

    OSC 8 links the text written after it to a URL, until the OSC 8 that closes
    the link. *)

val hyperlink_start : ?params:string -> url:string -> t
(** [hyperlink_start ~params ~url] is the OSC 8 that links the text after it to
    [url]. [params] holds [key=value] pairs separated by [:], and defaults to
    [""]; a [;] in it is left out. The link must be closed with
    {!hyperlink_end}, or all the text after it is linked. *)

val hyperlink_end : t
(** [hyperlink_end] is the OSC 8 that closes the open link. *)

val hyperlink : ?params:string -> url:string -> text:string -> t
(** [hyperlink ~params ~url ~text] is {!hyperlink_start}, [text], then
    {!hyperlink_end}. *)

(** {2:clipboard Clipboard} *)

val set_clipboard : text:string -> t
(** [set_clipboard ~text] is OSC 52 for the [c] selection, which sets the system
    clipboard to [text]. The sequence carries [text] in base64. *)

(** {2:notifications Notifications} *)

val notify : [ `Osc9 | `Osc777 ] -> title:string -> body:string -> t
(** [notify form ~title ~body] posts a desktop notification in one of its two
    forms. [`Osc9] is iTerm2's OSC 9 with the message [title: body]. [`Osc777]
    is OSC 777 [notify;title;body], the form of an rxvt extension
    ([ESC \] 777;notify;title;msg ESC \\ ] in foot-ctlseqs(7)), whose title
    leaves out [;]. Inside tmux, it must be wrapped with {!tmux_passthrough} to
    reach the terminal. *)

(** {2:passthrough Multiplexer passthrough} *)

val tmux_passthrough : t -> t
(** [tmux_passthrough s] is [s] inside a DCS that tmux passes to the terminal it
    runs in, with each ESC of [s] doubled, as tmux requires. *)

(** {2:bell Bell} *)

val bell : t
(** [bell] is BEL, which rings the terminal's bell. *)

(** {1:modes Modes}

    DECSET, [CSI ? n h], enables the DEC private mode [n], and DECRST,
    [CSI ? n l], disables it. *)

(** The type for DEC private modes. *)
type mode =
  | Cursor_visible  (** DECTCEM, 25. Shows the cursor. *)
  | Mouse_tracking  (** 1000. Reports button presses and releases. *)
  | Mouse_button_tracking
      (** 1002. Also reports motion while a button is down. *)
  | Mouse_motion  (** 1003. Reports all motion. *)
  | Mouse_sgr  (** 1006. Writes mouse reports in the SGR encoding. *)
  | Mouse_sgr_pixel
      (** 1016. Writes mouse reports in the SGR encoding, in pixels. *)
  | Mouse_x10  (** 9. Reports button presses, as X10 did. *)
  | Urxvt_mouse  (** 1015. Writes mouse reports in urxvt's encoding. *)
  | Alternate_screen
      (** 1049. Saves the cursor and switches to the alternate screen. *)
  | Focus_tracking  (** 1004. Reports focus in and out. *)
  | Bracketed_paste  (** 2004. Brackets pasted text. *)
  | Sync_output  (** 2026. Synchronized output. *)
  | Unicode  (** 2027. Grapheme cluster mode. *)
  | Color_scheme  (** 2031. Reports changes of the color scheme. *)

val enable : mode -> t
(** [enable m] is the DECSET of [m]. *)

val disable : mode -> t
(** [disable m] is the DECRST of [m]. *)

(** {2:keyboard Keyboard encoding} *)

val csi_u_push : flags:int -> t
(** [csi_u_push ~flags] is [CSI > flags u], which pushes [flags] on the kitty
    keyboard protocol's stack. *)

val csi_u_pop : t
(** [csi_u_pop] is [CSI < u], which pops the top of the kitty keyboard
    protocol's stack. *)

val modify_other_keys_on : t
(** [modify_other_keys_on] is [CSI > 4 ; 1 m], which sets xterm's
    modifyOtherKeys to level 1. *)

val modify_other_keys_off : t
(** [modify_other_keys_off] is [CSI > 4 ; 0 m], which turns xterm's
    modifyOtherKeys off. *)

(** {1:queries Queries} *)

(** The type for terminal queries. The terminal answers in its input stream,
    after what it was already sending. The reader of the input must expect the
    answer. An answer that is a control string, as those of DA3, XTVERSION and
    the color queries are, is otherwise read as keys. *)
type query =
  | Cursor_position  (** DSR 6. *)
  | Pixel_size  (** [CSI 14 t]. *)
  | Device_attributes  (** DA1. *)
  | Tertiary_attributes  (** DA3. *)
  | Terminal_identity  (** XTVERSION. *)
  | Device_status  (** DSR 5. *)
  | Csi_u_support  (** [CSI ? u]. *)
  | Kitty_graphics
      (** The kitty graphics protocol's query, for a 1x1 image with id 31337. A
          terminal without the protocol stays silent, so the query is followed
          by a query every terminal answers, such as DA1. *)
  | Sixel_geometry  (** Sixel geometry limits. *)
  | Explicit_width_support  (** OSC 66 width probe. *)
  | Scaled_text_support  (** OSC 66 scaled text probe. *)
  | Focus_mode  (** DECRQM 1004. *)
  | Sgr_pixels_mode  (** DECRQM 1016. *)
  | Bracketed_paste_mode  (** DECRQM 2004. *)
  | Sync_mode  (** DECRQM 2026. *)
  | Unicode_mode  (** DECRQM 2027. *)
  | Color_scheme_mode  (** DECRQM 2031. *)
  | Color_scheme_report
      (** DSR [CSI ? 996 n], which asks for the light or dark color scheme. The
          terminal replies [CSI ? 997 ; value n]. *)
  | Foreground_color
      (** OSC 10 with [?], which asks for the default foreground color. The
          terminal replies with the OSC 10 that would set it. *)
  | Background_color
      (** OSC 11 with [?], which asks for the default background color. The
          terminal replies with the OSC 11 that would set it. *)
  | Cursor_color
      (** OSC 12 with [?], which asks for the text cursor's color. The terminal
          replies with the OSC 12 that would set it. *)
  | Palette_entry of int
      (** OSC 4 with [i] and [?], which asks for palette entry [i], clamped to
          \[[0];[255]\]. The terminal replies with the OSC 4 that would set it.
      *)

val query : query -> t
(** [query q] is the sequence that asks [q]. *)

(** {1:styled Styled text}

    A {e span} is a text in a style. A line of styled text is a list of spans,
    read as one text, so a character cut between two spans, such as a letter and
    the accent after it, stays whole, in the style of the span its first byte is
    in. Its text shows as {!sanitize} leaves it ({!sanitize_spans}), so only a
    span's style colors it and only its style's link links it. *)

type span = { text : string; style : Style.t }
(** The type for spans. *)

val span : ?style:Style.t -> string -> span
(** [span ~style text] is [text] in [style], {!Style.default} by default. *)

val render :
  ?depth:Color.depth -> ?hyperlinks_enabled:bool -> span list -> string
(** [render ~depth ~hyperlinks_enabled spans] is the bytes that show the line
    [spans], as {!sanitize_spans} leaves it, from {!Style.default}. Before each
    span it writes an OSC 8 that closes or opens a hyperlink where the link
    changes, and the transition of {!Style.emit} from the style before, at
    [depth], [`Truecolor] by default. It ends by closing an open hyperlink and
    writing {!reset}. With [~hyperlinks_enabled:false] it writes no hyperlink.
    [hyperlinks_enabled] defaults to [true]. *)

(** {1:untrusted Removing control functions}

    Text from someone else, such as a file name, a subprocess's output or a
    message read off the network, can hold control functions. Printed as it is,
    it can move the cursor and write over what is on the screen, clear the
    screen, set the title or the clipboard, or ask the terminal a question whose
    answer arrives as if it were typed. Text can also show other than what it
    holds. A CR or a BS makes the end of a line write over its start, and
    Unicode's bidirectional controls reorder the characters after them, so that
    the reader sees one order and a program reads another (CVE-2021-42574,
    "Trojan Source"). *)

val strip : string -> string
(** [strip s] is the {!Parser.Text} tokens and the C0 {!Parser.Control} tokens
    of {!Parser.parse}[ s], in order. It removes escape and control sequences,
    control strings, DEL and C1 controls, an unfinished one at the end included,
    and keeps line feeds and tabs. As {!Parser} reads UTF-8, each ill-formed
    subsequence becomes U+FFFD and U+0080..U+009F are dropped.

    The C0 controls it keeps, such as CR, BS and BEL, still act on a terminal,
    so text from someone else goes through {!sanitize} instead. *)

val sanitize : string -> string
(** [sanitize s] is the text of [s] that shows, in the order of [s], which is
    safe to print to a terminal:
    - Every control function goes, as {!Parser} reads them. These are escape and
      control sequences, control strings with their content (a title, a link's
      target), C1 controls written ESC Fe, DEL, and the C0 controls other than
      HT and the line ends. {!Parser} drops U+0080..U+009F and makes each
      ill-formed UTF-8 subsequence U+FFFD.
    - A line ends at LF, at VT and FF, which a terminal performs as LF (VT220
      Programmer Reference Manual §4.6.6), at NEL, and at CR, which returns to
      the start of the line to write over it. A run of CRs and the LF, VT, FF or
      NEL after it end one line. Each line end becomes one LF.
    - The characters a terminal shows as nothing go. These are Unicode's
      Default_Ignorable_Code_Point (DerivedCoreProperties.txt), which include
      every bidirectional control (Bidi_Control, the embeddings, overrides and
      isolates of UAX #9 and the marks LRM, RLM and ALM), the zero-width space,
      the word joiner, the tag characters, the Hangul fillers and the soft
      hyphen.
    - Three kinds of them stay where they change how the character before them
      shows. These are a variation selector directly after a character other
      than a space, HT or LF, a joiner (ZWJ or ZWNJ) after such a character or
      its selector, and the tags of an RGI emoji tag sequence, the flags of
      England, Scotland and Wales (emoji-sequences.txt). Emoji and the scripts
      that need joiners show as they should, and a run of invisible characters
      cannot hide text.

    Styles go too, as control functions. SGR can hide text (SGR 8 conceals it,
    and a foreground equal to the background hides it), so no style is safe. To
    show the colors of a source trusted with them, a program reads its SGR with
    {!Parser} and {!Style.apply_sgr} and draws what they select.

    Two effects remain that no character filter removes. A terminal that
    implements bidirectional display still reorders text in a right-to-left
    script, in the order its own letters give, and characters that look alike,
    such as Latin "a" and Cyrillic "а", stay distinct.

    The result is valid UTF-8 with no control character other than LF and HT,
    and [sanitize (sanitize s) = sanitize s]. *)

val sanitize_spans : span list -> span list
(** [sanitize_spans spans] is the line [spans] with its text sanitized as one
    text. The texts of the result, joined, are {!sanitize} of the texts of
    [spans], joined, and each grapheme of it is in the style of the span of
    [spans] that holds its first byte. *)

(** {1:submodules Submodules} *)

module Writer = Writer
(** Byte writers. *)

module Color = Color
(** Colors. *)

module Palette = Palette
(** Palettes, the colors a terminal's user configured. *)

module Attr = Attr
(** Text attributes. *)

module Style = Style
(** Styles. *)

module Parser = Parser
(** Control functions in a UTF-8 stream. *)

module Sgr_state = Sgr_state
(** The style a terminal is in. *)

module Styling = Styling
(** The styling of a destination. *)
