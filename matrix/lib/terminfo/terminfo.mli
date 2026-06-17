(** Typed access to terminfo capabilities.

    [Terminfo] loads entries from the system terminfo database and exposes each
    capability as a typed token. The {!type-cap} GADT encodes the return type of
    every capability so that {!get} is type-safe at compile time.

    {b Terminology.}
    - A {e capability} is a named terminal property: a boolean flag, an integer
      quantity, a fixed escape sequence, or a parameterized escape sequence.
    - An {e entry} is the set of capabilities declared for a terminal type (e.g.
      [xterm-256color]).
    - A {e handle} ({!type-t}) is an immutable, in-memory representation of one
      entry.

    {1:quick_start Quick start}

    {[
    match Terminfo.load () with
    | Error (`Parse_error msg) -> prerr_endline ("terminfo parse error: " ^ msg)
    | Error `Not_found -> prerr_endline "no terminfo entry"
    | Ok ti -> (
        Option.iter print_string (Terminfo.get ti Terminfo.Clear_screen);
        (match Terminfo.get ti Terminfo.Cursor_position with
        | Some goto -> print_string (goto (5, 10))
        | None -> ());
        match Terminfo.get ti Terminfo.Has_colors with
        | Some true -> print_endline "colors"
        | _ -> print_endline "monochrome")
    ]} *)

(** {1:caps Capabilities} *)

(** {2:cap_type Capability tokens} *)

(** The type for capability tokens.

    The phantom type parameter encodes the OCaml type returned by {!get}:
    - [bool cap] — boolean flags.
    - [int cap] — numeric quantities.
    - [string cap] — fixed escape sequences.
    - [(… -> string) cap] — parameterized escape sequences. The returned
      function evaluates the terminfo [%]-expression and allocates a fresh
      [string] on each call.

    Each constructor maps to a standard terminfo capability whose short name
    appears in the constructor's documentation. *)
type _ cap =
  | Auto_left_margin : bool cap  (** Automatic left margins ([bw]). *)
  | Auto_right_margin : bool cap  (** Automatic right margins ([am]). *)
  | Back_color_erase : bool cap
      (** Screen erased with background color ([bce]). *)
  | Can_change : bool cap  (** Terminal can redefine existing colors ([ccc]). *)
  | Eat_newline_glitch : bool cap
      (** Newline ignored after rightmost column ([xenl]). *)
  | Has_colors : bool cap
      (** Terminal supports colors. Synthesized: [true] iff the numeric [colors]
          capability is present and greater than [0]. Always yields [Some true]
          or [Some false].

          See also {!Max_colors}. *)
  | Has_meta_key : bool cap  (** Terminal has a meta key ([km]). *)
  | Insert_null_glitch : bool cap
      (** Insert mode distinguishes nulls ([in]). *)
  | Move_insert_mode : bool cap
      (** Safe to move while in insert mode ([mir]). *)
  | Move_standout_mode : bool cap
      (** Safe to move while in standout mode ([msgr]). *)
  | Over_strike : bool cap  (** Terminal can overstrike ([os]). *)
  | Transparent_underline : bool cap
      (** Underline character overstrikes ([ul]). *)
  | Xon_xoff : bool cap  (** Terminal uses xon/xoff handshaking ([xon]). *)
  (* Numeric capabilities *)
  | Columns : int cap  (** Number of columns in a line ([cols]). *)
  | Lines : int cap  (** Number of lines on screen ([lines]). *)
  | Max_colors : int cap
      (** Maximum number of colors ([colors]).

          See also {!Has_colors}. *)
  | Max_pairs : int cap  (** Maximum number of color pairs ([pairs]). *)
  | Max_attributes : int cap  (** Maximum combined attributes ([ma]). *)
  | Init_tabs : int cap  (** Initial tab stop spacing ([it]). *)
  | Virtual_terminal : int cap  (** Virtual terminal number ([vt]). *)
  (* String capabilities — non-parameterized *)
  | Bell : string cap  (** Audible bell ([bel]). *)
  | Carriage_return : string cap  (** Carriage return ([cr]). *)
  | Clear_screen : string cap  (** Clear screen and home cursor ([clear]). *)
  | Clear_to_eol : string cap  (** Clear to end of line ([el]). *)
  | Clear_to_eos : string cap  (** Clear to end of screen ([ed]). *)
  | Cursor_down : string cap  (** Move cursor down one line ([cud1]). *)
  | Cursor_home : string cap  (** Home cursor to upper-left corner ([home]). *)
  | Cursor_invisible : string cap  (** Make cursor invisible ([civis]). *)
  | Cursor_left : string cap  (** Move cursor left one character ([cub1]). *)
  | Cursor_normal : string cap
      (** Restore cursor to normal visibility ([cnorm]). *)
  | Cursor_right : string cap  (** Move cursor right one character ([cuf1]). *)
  | Cursor_up : string cap  (** Move cursor up one line ([cuu1]). *)
  | Cursor_visible : string cap  (** Make cursor very visible ([cvvis]). *)
  | Delete_character : string cap  (** Delete one character ([dch1]). *)
  | Delete_line : string cap  (** Delete one line ([dl1]). *)
  | Enter_alt_charset : string cap
      (** Start alternate character set ([smacs]). *)
  | Enter_blink_mode : string cap  (** Turn on blinking ([blink]). *)
  | Enter_bold_mode : string cap  (** Turn on bold ([bold]). *)
  | Enter_dim_mode : string cap  (** Turn on dim ([dim]). *)
  | Enter_insert_mode : string cap  (** Enter insert mode ([smir]). *)
  | Enter_reverse_mode : string cap  (** Turn on reverse video ([rev]). *)
  | Enter_standout_mode : string cap  (** Begin standout mode ([smso]). *)
  | Enter_underline_mode : string cap  (** Turn on underline ([smul]). *)
  | Exit_alt_charset : string cap  (** End alternate character set ([rmacs]). *)
  | Exit_attribute_mode : string cap  (** Turn off all attributes ([sgr0]). *)
  | Exit_insert_mode : string cap  (** End insert mode ([rmir]). *)
  | Exit_standout_mode : string cap  (** End standout mode ([rmso]). *)
  | Exit_underline_mode : string cap  (** End underline mode ([rmul]). *)
  | Flash_screen : string cap  (** Visible bell ([flash]). *)
  | Insert_character : string cap  (** Insert one character ([ich1]). *)
  | Insert_line : string cap  (** Insert one line ([il1]). *)
  | Keypad_local : string cap  (** Leave keypad transmit mode ([rmkx]). *)
  | Keypad_xmit : string cap  (** Enter keypad transmit mode ([smkx]). *)
  | Newline : string cap
      (** Newline, behaves like carriage return followed by line feed ([nel]).
      *)
  | Reset_1string : string cap  (** Reset string 1 ([rs1]). *)
  | Reset_2string : string cap  (** Reset string 2 ([rs2]). *)
  | Restore_cursor : string cap  (** Restore saved cursor position ([rc]). *)
  | Save_cursor : string cap  (** Save cursor position ([sc]). *)
  | Scroll_forward : string cap  (** Scroll forward one line ([ind]). *)
  | Scroll_reverse : string cap  (** Scroll reverse one line ([ri]). *)
  | Tab : string cap  (** Tab character ([ht]). *)
  (* Parameterized capabilities *)
  | Column_address : (int -> string) cap
      (** [Column_address]: move cursor to column [n] ([hpa]). *)
  | Cursor_position : (int * int -> string) cap
      (** [Cursor_position]: move cursor to [(row, col)] ([cup]). The terminfo
          [%]-expression applies coordinate transformations (e.g. [%i] for
          1-based indexing). *)
  | Delete_chars : (int -> string) cap
      (** [Delete_chars]: delete [n] characters ([dch]). *)
  | Delete_lines : (int -> string) cap
      (** [Delete_lines]: delete [n] lines ([dl]). *)
  | Insert_chars : (int -> string) cap
      (** [Insert_chars]: insert [n] characters ([ich]). *)
  | Insert_lines : (int -> string) cap
      (** [Insert_lines]: insert [n] lines ([il]). *)
  | Parm_down_cursor : (int -> string) cap
      (** [Parm_down_cursor]: move cursor down [n] lines ([cud]). *)
  | Parm_left_cursor : (int -> string) cap
      (** [Parm_left_cursor]: move cursor left [n] characters ([cub]). *)
  | Parm_right_cursor : (int -> string) cap
      (** [Parm_right_cursor]: move cursor right [n] characters ([cuf]). *)
  | Parm_up_cursor : (int -> string) cap
      (** [Parm_up_cursor]: move cursor up [n] lines ([cuu]). *)
  | Repeat_char : (char * int -> string) cap
      (** [Repeat_char]: repeat character [(c, n)] times ([rep]). *)
  | Row_address : (int -> string) cap
      (** [Row_address]: move cursor to row [n] ([vpa]). *)
  | Set_background : (int -> string) cap
      (** [Set_background]: set background to color index [n] ([setab]). *)
  | Set_foreground : (int -> string) cap
      (** [Set_foreground]: set foreground to color index [n] ([setaf]). *)

(** {1:entries Entries} *)

type t
(** The type for terminfo entries. A value of this type is an immutable,
    in-memory handle to a parsed terminfo entry. It can be shared freely across
    threads. *)

val load :
  ?term:string -> unit -> (t, [ `Not_found | `Parse_error of string ]) result
(** [load ?term ()] is the terminfo entry for terminal type [term].

    [term] defaults to the value of the [TERM] environment variable.

    The search order is [/usr/share/terminfo], [/lib/terminfo], [/etc/terminfo],
    and [$HOME/.terminfo] (when present). The entry is parsed eagerly; each call
    creates a fresh, independent handle.

    Errors with [`Not_found] if no entry is found and [`Parse_error msg] if the
    file cannot be decoded.

    Raises [Sys_error] if the entry exists but cannot be read. *)

(** {1:lookup Lookup} *)

val get : t -> 'a cap -> 'a option
(** [get ti cap] is the value of [cap] in [ti], if declared.

    For parameterized capabilities the returned function evaluates the terminfo
    [%]-expression and allocates a fresh [string] on each call. The function
    never mutates [ti].

    {b Note.} {!Has_colors} is synthesized from {!Max_colors} and always yields
    [Some true] or [Some false]. *)
