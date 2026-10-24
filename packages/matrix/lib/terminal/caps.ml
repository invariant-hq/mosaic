module Reply = Matrix_input.Reply
module Decoder = Matrix_input.Decoder

type t = {
  rgb : bool;
  kitty_keyboard : int option;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : Matrix_text.width_method;
  grapheme_mode : bool option;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  explicit_cursor_positioning : bool;
  scaled_text : bool;
  sixel : bool;
  graphics : [ `Kitty | `Sixel | `Blocks ];
  sync : bool;
  hyperlinks : bool;
  notification : [ `Osc9 | `Osc777 ];
  meta_sends_escape : bool option;
  multiplexer : [ `Tmux | `Screen ] option;
  identity : (string * string) option;
}

let defaults =
  {
    rgb = false;
    kitty_keyboard = None;
    kitty_graphics = false;
    bracketed_paste = false;
    focus_tracking = false;
    unicode_width = `Wcwidth;
    grapheme_mode = None;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = false;
    scaled_text = false;
    sixel = false;
    graphics = `Blocks;
    sync = false;
    hyperlinks = false;
    notification = `Osc9;
    meta_sends_escape = None;
    multiplexer = None;
    identity = None;
  }

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i j = j = m || (s.[i + j] = sub.[j] && at i (j + 1)) in
  let rec from i = i + m <= n && (at i 0 || from (i + 1)) in
  from 0

(* Environment *)

type env = {
  term : string;  (** Lowercase, [""] when unset. *)
  colorterm : string;
  term_program : (string * string) option;
  kitty_window : bool;
  termux : string option;
  attached : bool;
      (** No ssh session, multiplexer or zellij between the process and the
          terminal its environment names. *)
  tmux : bool;
  screen : bool;
  vhs : bool;
  force_wcwidth : bool;
  force_unicode : bool;
  graphics : [ `Kitty | `Sixel | `Blocks ] option;  (** [MATRIX_GRAPHICS]. *)
  win32 : bool;
  described : described;
}

(* What the description of [TERM] says, where it says yes: a description
   leaves out what many terminals do, so its silence says nothing. *)
and described = {
  direct_color : bool;  (** [RGB], [Tc], or [setrgbf] and [setrgbb]. *)
  paste : bool;  (** [BE]. *)
  focus : bool;  (** [fe]. *)
  synchronized : bool;  (** [Sync], as mode 2026. *)
}

let describe = function
  | None ->
      {
        direct_color = false;
        paste = false;
        focus = false;
        synchronized = false;
      }
  | Some t ->
      let module T = Matrix_terminfo in
      let has name = T.string t name <> None in
      {
        direct_color =
          T.flag t "RGB"
          || T.number t "RGB" <> None
          || has "RGB" || T.flag t "Tc"
          || (has "setrgbf" && has "setrgbb");
        paste = has "BE";
        focus = has "fe";
        synchronized =
          (match T.string t "Sync" with
          | Some s -> String.starts_with ~prefix:"\027[?2026" s
          | None -> false);
      }

let env ?terminfo getenv ~win32 =
  let get name =
    match getenv name with Some "" | None -> None | Some v -> Some v
  in
  let set name = Option.is_some (get name) in
  let lower name =
    String.lowercase_ascii (Option.value (get name) ~default:"")
  in
  let term = lower "TERM" in
  let prefixed prefix = String.starts_with ~prefix term in
  let tmux = set "TMUX" and screen = set "STY" in
  {
    term;
    colorterm = lower "COLORTERM";
    term_program =
      Option.map
        (fun name ->
          (name, Option.value (get "TERM_PROGRAM_VERSION") ~default:""))
        (get "TERM_PROGRAM");
    kitty_window = set "KITTY_WINDOW_ID";
    termux = get "TERMUX_VERSION";
    attached =
      not
        (List.exists set [ "SSH_CONNECTION"; "SSH_CLIENT"; "SSH_TTY"; "ZELLIJ" ]
        || tmux || screen || prefixed "tmux" || prefixed "screen"
        || prefixed "zellij");
    tmux;
    screen;
    vhs = set "VHS_RECORD";
    force_wcwidth = set "MATRIX_FORCE_WCWIDTH";
    force_unicode = set "MATRIX_FORCE_UNICODE";
    graphics =
      (match lower "MATRIX_GRAPHICS" with
      | "kitty" -> Some `Kitty
      | "sixel" -> Some `Sixel
      | "blocks" -> Some `Blocks
      | _ -> None);
    win32;
    described = describe terminfo;
  }

let no_env = env (fun _ -> None) ~win32:false

(* Evidence *)

type evidence = {
  env : env;
  assumed : t;
  modes : (int * Reply.setting) list;  (** DECRPM of DEC modes, latest first. *)
  keyboard : int option;
  version : string option;
  outer_version : string option;
      (** The XTVERSION of the terminal outside tmux, through passthrough. *)
  registers : int option;  (** The sixel color registers (XTSMGRAPHICS). *)
  attributes : int list option;  (** The probe's DA1. *)
  explicit : bool option;
  scaled : bool option;
  graphics : bool option;
  passthrough : bool;
}

let make env assumed =
  {
    env;
    assumed;
    modes = [];
    keyboard = None;
    version = None;
    outer_version = None;
    registers = None;
    attributes = None;
    explicit = None;
    scaled = None;
    graphics = None;
    passthrough = false;
  }

let evidence env = make env defaults
let assume caps = make no_env caps

let add ev (r : Reply.t) =
  match r with
  | Mode { dec = true; mode; setting } ->
      { ev with modes = (mode, setting) :: List.remove_assoc mode ev.modes }
  | Keyboard_flags flags -> { ev with keyboard = Some flags }
  (* Once passthrough is on, the one batch that asks XTVERSION is the outer
     terminal's. *)
  | Version text when ev.passthrough -> { ev with outer_version = Some text }
  | Version text -> { ev with version = Some text }
  | Csi { marker = Some '?'; final = 'S'; intermediates = ""; params }
    when Ansi.Parser.Params.(value params 0 = Some 1 && value params 1 = Some 0)
    ->
      { ev with registers = Ansi.Parser.Params.value params 2 }
  | Graphics { id = Some 31337; _ } -> { ev with graphics = Some true }
  | _ -> ev

(* XTVERSION is "name(version)" (xterm, kitty) or "name version" (tmux,
   WezTerm, Ghostty). *)
let parse_version text =
  let text = String.trim text in
  let cut i j = String.trim (String.sub text i (j - i)) in
  let n = String.length text in
  match String.index_opt text '(' with
  | Some i ->
      let j = Option.value (String.index_from_opt text i ')') ~default:n in
      (cut 0 i, if j > i then cut (i + 1) j else "")
  | None -> (
      match String.index_opt text ' ' with
      | Some i -> (cut 0 i, cut (i + 1) n)
      | None -> (text, ""))

(* The leading [major.minor] of a version such as "3.5a" or "next-3.7". *)
let major_minor version =
  let n = String.length version in
  let rec number i acc =
    if i < n && version.[i] >= '0' && version.[i] <= '9' then
      number (i + 1) ((acc * 10) + Char.code version.[i] - 48)
    else (i, acc)
  in
  let rec start i =
    if i >= n then None
    else if version.[i] >= '0' && version.[i] <= '9' then
      let i, major = number i 0 in
      if i < n && version.[i] = '.' then
        let j, minor = number (i + 1) 0 in
        if j > i + 1 then Some (major, minor) else None
      else None
    else start (i + 1)
  in
  start 0

let at_least version v =
  match major_minor version with Some mm -> mm >= v | None -> false

let decimal s =
  if s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s then
    int_of_string_opt s
  else None

(* Ghostty widens graphemes from 1.3.0, and in its dev builds from
   2026-02-24 ("0.0.0-YYYYMMDD.…"), with a release's own dev builds
   ("1.3.0-N-gHASH") counting as the release. *)
let ghostty_wide version =
  let release =
    match String.split_on_char '-' version with
    | core :: suffix -> (
        match String.split_on_char '.' core with
        | [ major; minor; patch ] -> (
            match (decimal major, decimal minor, decimal patch) with
            | Some major, Some minor, Some patch ->
                major > 1
                || (major = 1 && minor > 3)
                || major = 1 && minor = 3
                   && (patch > 0
                      ||
                      match suffix with
                      | [] -> true
                      | [ commits; revision ] ->
                          Option.is_some (decimal commits)
                          && String.length revision > 1
                          && revision.[0] = 'g'
                          && String.for_all
                               (function
                                 | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
                                 | _ -> false)
                               (String.sub revision 1
                                  (String.length revision - 1))
                      | _ -> false)
            | _ -> false)
        | _ -> false)
    | [] -> false
  in
  release
  || String.starts_with ~prefix:"0.0.0-" version
     && String.length version > 14
     && version.[14] = '.'
     &&
     match decimal (String.sub version 6 8) with
     | Some date -> date >= 20260224
     | None -> false

(* The identity an answer gives, else the environment's when directly
   attached. *)
let reported_identity ev =
  match ev.version with
  | Some text -> Some (parse_version text)
  | None when ev.env.attached -> (
      match (ev.env.term_program, ev.env.termux) with
      | Some id, _ -> Some id
      | None, _ when ev.env.kitty_window -> Some ("kitty", "")
      | None, Some version -> Some ("termux", version)
      | None, None -> None)
  | None -> None

(* The multiplexer the environment says is in the way. *)
let env_multiplexer env =
  if env.tmux then Some `Tmux
  else if env.screen then Some `Screen
  else if String.starts_with ~prefix:"tmux" env.term then Some `Tmux
  else if String.starts_with ~prefix:"screen" env.term then Some `Screen
  else None

(* An XTVERSION reply is the terminal that answers: tmux, or no multiplexer
   (GNU screen does not answer). *)
let reported_multiplexer ev =
  match ev.version with
  | Some text ->
      let name, _ = parse_version text in
      if contains (String.lowercase_ascii name) "tmux" then Some `Tmux else None
  | None -> env_multiplexer ev.env

let first options default =
  match List.find_map Fun.id options with Some v -> v | None -> default

let when_ b v = if b then Some v else None

let supported : Reply.setting -> bool = function
  | Set | Reset | Permanently_set -> true
  | Not_recognized | Permanently_reset -> false

let modern_names = [ "ghostty"; "kitty"; "wezterm"; "alacritty"; "iterm" ]

(* The terminals known to draw kitty's Unicode placeholders, which no query
   tells: kitty from 0.28.0, where they came ("versionadded 0.28.0" in its
   graphics protocol), and Ghostty. WezTerm answers the graphics query and
   draws a virtual placement as an image at the cursor (measured on
   20240203). *)
let placeholders name version =
  (contains name "kitty" && at_least version (0, 28)) || contains name "ghostty"

(* Identity rows and guesses apply to what was reported, never to what the
   caller assumed: an assumed fact is already the answer for that fact. *)

(* tmux's rows follow its CHANGES: "Support for bracketed-paste mode" (1.6
   to 1.7), "Panes can now emit focus notifications" (1.7 to 1.8), synchronized
   output and its DECRQM answer (3.6b to 3.7); DECRQM answers for 2004 and
   1004 came in 3.6, years after tmux had both. Measured on 3.5a with its
   default options: a pane that set 2004 and 1004 gets the outer terminal's
   bracketed paste bracketed, paste-buffer -p bracketed, a focus report when
   it sets the mode and the outer terminal's focus reports, which tmux asks
   the outer terminal for only with focus-events on. *)
let derive ev =
  let env = ev.env and d = ev.assumed in
  let described = env.described in
  let id = reported_identity ev in
  let name, version =
    match id with
    | Some (name, version) -> (String.lowercase_ascii name, version)
    | None -> ("", "")
  in
  let is family = contains name family in
  let tmux = is "tmux" in
  let modern = List.exists is modern_names in
  let term_modern = List.exists (contains env.term) modern_names in
  let truecolor =
    env.colorterm = "truecolor" || env.colorterm = "24bit" || env.win32
  in
  let mux = reported_multiplexer ev in
  let mode m = Option.map supported (List.assoc_opt m ev.modes) in
  let sixel = first [ Option.map (List.mem 4) ev.attributes ] d.sixel in
  let kitty_graphics =
    first
      [ when_ env.vhs false; ev.graphics; when_ (is "kitty") true ]
      d.kitty_graphics
  in
  {
    rgb =
      first
        [
          when_ (is "kitty") true;
          when_ truecolor true;
          when_ described.direct_color true;
        ]
        d.rgb;
    kitty_keyboard =
      (if env.vhs then None
       else match ev.keyboard with Some _ as k -> k | None -> d.kitty_keyboard);
    kitty_graphics;
    bracketed_paste =
      first
        [
          mode 2004;
          when_ (is "kitty") true;
          when_ tmux (at_least version (1, 7));
          when_ env.win32 true;
          when_ described.paste true;
        ]
        d.bracketed_paste;
    focus_tracking =
      first
        [
          mode 1004;
          when_ tmux (at_least version (1, 8));
          when_ described.focus true;
        ]
        d.focus_tracking;
    unicode_width =
      first
        [
          when_ env.force_unicode `Unicode;
          when_ (env.force_wcwidth || env.vhs) `Wcwidth;
          when_ (is "ghostty" && ghostty_wide version) `Unicode_wide;
          (match List.assoc_opt 2027 ev.modes with
          | Some (Set | Reset | Permanently_set) -> Some `Unicode
          | Some Permanently_reset -> Some `Wcwidth
          | Some Not_recognized | None -> None);
          (* xterm and VTE answer mode 2027 as not recognized and st does not
             answer, and the three measure by character. kitty answers not
             recognized too, and places grapheme clusters (measured: 0.49.2 as
             [`Unicode] does; 0.41.1, older than its cell splitting algorithm
             of 0.42, nearer [`Unicode] than [`Wcwidth]). *)
          when_ (is "kitty") `Unicode;
          when_ (tmux || is "apple_terminal" || is "termux") `Wcwidth;
          when_ (Option.is_some mux) `Wcwidth;
          when_ (contains env.term "kitty") `Unicode;
        ]
        d.unicode_width;
    grapheme_mode =
      (match List.assoc_opt 2027 ev.modes with
      | Some (Set | Permanently_set) -> Some true
      | Some Reset -> Some false
      | Some (Not_recognized | Permanently_reset) -> None
      | None -> d.grapheme_mode);
    sgr_pixels = first [ mode 1016 ] d.sgr_pixels;
    color_scheme_updates = first [ mode 2031 ] d.color_scheme_updates;
    explicit_width = first [ ev.explicit ] d.explicit_width;
    explicit_cursor_positioning =
      first
        [ when_ (tmux || is "alacritty") true; when_ (Option.is_some mux) true ]
        d.explicit_cursor_positioning;
    scaled_text = first [ ev.scaled ] d.scaled_text;
    sixel;
    graphics =
      first
        [
          env.graphics;
          when_
            (kitty_graphics
            && (placeholders name version
               || tmux
                  &&
                  match ev.outer_version with
                  | Some text ->
                      let name, version = parse_version text in
                      placeholders (String.lowercase_ascii name) version
                  | None -> false))
            `Kitty;
          (* A cube of 252 colors needs 252 registers; xterm as a VT340 has
             16 unless told more (xterm(1), numColorRegisters). Konsole
             erases the text under a sixel and deletes the whole image when
             text lands on its top row (Screen.cpp), so the half blocks
             under a sixel cannot stand in for a lost one there. *)
          when_
            (sixel
            && (match ev.registers with Some n -> n >= 252 | None -> true)
            && not (is "konsole"))
            `Sixel;
        ]
        d.graphics;
    sync =
      first
        [
          mode 2026;
          when_ tmux (at_least version (3, 7));
          when_ modern true;
          when_ term_modern true;
          when_ described.synchronized true;
        ]
        d.sync;
    hyperlinks =
      first
        [ when_ (tmux || modern) true; when_ (term_modern || truecolor) true ]
        d.hyperlinks;
    notification =
      (if String.starts_with ~prefix:"rxvt" env.term then `Osc777
       else d.notification);
    meta_sends_escape =
      (match List.assoc_opt 1036 ev.modes with
      | Some (Set | Permanently_set) -> Some true
      | Some Reset -> Some false
      | Some (Not_recognized | Permanently_reset) -> None
      | None -> d.meta_sends_escape);
    multiplexer =
      (match (ev.version, mux) with
      | Some _, mux | None, (Some _ as mux) -> mux
      | None, None -> d.multiplexer);
    identity = (match id with Some _ -> id | None -> d.identity);
  }

(* Questions *)

type batch = Probe | Widths | Passthrough | Outer

let s7c1t = "\027 F"
let da1 = Ansi.(to_string (query Device_attributes))
let cursor_report = Ansi.(to_string (query Cursor_position))
let first_column = "\r"
let erase_below = "\027[J"
let graphics_query = Ansi.(to_string (query Kitty_graphics))
let wrap s = Ansi.(to_string (tmux_passthrough (literal s)))

let probe ~graphics =
  String.concat ""
    [
      s7c1t;
      Ansi.(to_string (query Sgr_pixels_mode));
      Ansi.(to_string (query Unicode_mode));
      Ansi.(to_string (query Color_scheme_mode));
      Ansi.(to_string (query Focus_mode));
      Ansi.(to_string (query Bracketed_paste_mode));
      Ansi.(to_string (query Sync_mode));
      "\027[?1036$p";
      Ansi.(to_string (query Terminal_identity));
      Ansi.(to_string (query Csi_u_support));
      (if graphics then graphics_query else "");
      "\027[16t";
      "\027[?1;1;0S";
      da1;
    ]

let widths =
  String.concat ""
    [
      first_column;
      Ansi.(to_string (query Explicit_width_support));
      cursor_report;
      first_column;
      Ansi.(to_string (query Scaled_text_support));
      cursor_report;
      first_column;
      erase_below;
    ]

(* The probe asks for kitty graphics directly only with no multiplexer in the
   way, as the environment or the caller tells before any answer: tmux takes an
   APC string for a window title. *)
let probe_graphics ev =
  Option.is_none
    (match env_multiplexer ev.env with
    | Some _ as m -> m
    | None -> ev.assumed.multiplexer)

let ask ev = function
  | Probe ->
      let graphics = probe_graphics ev in
      ( probe ~graphics,
        {
          Decoder.cursor_reports = 0;
          string_replies = (if graphics then 2 else 1);
        } )
  | Widths -> (widths, { cursor_reports = 2; string_replies = 0 })
  | Passthrough -> (wrap da1, { cursor_reports = 0; string_replies = 0 })
  | Outer ->
      ( wrap graphics_query
        ^ wrap Ansi.(to_string (query Terminal_identity))
        ^ wrap da1,
        { cursor_reports = 0; string_replies = 2 } )

(* [Widths] writes a space of width 1, then one of scale 2, from the first
   column of the row the cursor is on, and asks where the cursor went: a
   terminal without OSC 66 skips the string and leaves the cursor in place.
   The column says it, whatever the row. This is the detection kitty's text
   sizing protocol gives ("Detecting if the terminal supports this
   protocol"), with a carriage return before each space. A scaled space is
   two rows high, and kitty scrolls the screen when the row below is
   missing, so the program needs that row too. *)
let cursor ev batch ~index ~row:_ ~col =
  match (batch, index) with
  | Widths, 0 -> { ev with explicit = Some (col = 1) }
  | Widths, 1 ->
      let scaled = col = 2 in
      {
        ev with
        scaled = Some scaled;
        explicit = (if scaled then Some true else ev.explicit);
      }
  | _ -> ev

let answered ev batch ~attributes =
  let silent = function Some true -> Some true | _ -> Some false in
  match batch with
  | Probe ->
      {
        ev with
        attributes = Some attributes;
        graphics =
          (if probe_graphics ev then silent ev.graphics else ev.graphics);
      }
  | Widths -> ev
  | Passthrough -> { ev with passthrough = true }
  | Outer -> { ev with graphics = silent ev.graphics }

let next ev = function
  | Probe ->
      if (derive ev).multiplexer = Some `Tmux then Some Passthrough else None
  | Passthrough -> if ev.passthrough then Some Outer else None
  | Widths | Outer -> None
