(* C stubs *)

external get_size : Unix.file_descr -> int * int = "terminal_get_size"
external enable_vt_raw : Unix.file_descr -> unit = "terminal_enable_vt"

(* Types *)

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]

type unicode_width = [ `Wcwidth | `Unicode ]
type cursor_style = [ `Block | `Line | `Underline ]
type cursor_position = { x : int; y : int; visible : bool }

type capabilities = Caps.t = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  explicit_cursor_positioning : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}

type terminal_info = Caps.terminal_info = {
  name : string;
  version : string;
  from_xtversion : bool;
}

type cursor_state = {
  mutable x : int;
  mutable y : int;
  mutable visible : bool;
  mutable style : cursor_style;
  mutable blinking : bool;
  mutable color : float * float * float * float;
}

type t = {
  output : string -> unit;
  tty : bool;
  mutable caps : Caps.t;
  mutable terminal_info : Caps.terminal_info;
  parser : Input.Parser.t;
  mutable mouse_mode : mouse_mode;
  mutable bracketed_paste_enabled : bool;
  mutable focus_enabled : bool;
  mutable kitty_keyboard_enabled : bool;
  mutable kitty_keyboard_flags : int;
  mutable modify_other_keys_enabled : bool;
  mutable unicode_mode_enabled : bool;
  cursor : cursor_state;
  mutable alt_screen : bool;
  mutable scroll_region : (int * int) option;
  mutable pixel_resolution : (int * int) option;
  env_overrides : bool;
}

(* Pre-computed escape sequences *)

let alternate_on = Ansi.(to_string (enable Alternate_screen))
let alternate_off = Ansi.(to_string (disable Alternate_screen))
let focus_on = Ansi.(to_string (enable Focus_tracking))
let focus_off = Ansi.(to_string (disable Focus_tracking))
let paste_on = Ansi.(to_string (enable Bracketed_paste))
let paste_off = Ansi.(to_string (disable Bracketed_paste))
let kitty_kb_push flags = Ansi.(to_string (csi_u_push ~flags))
let kitty_kb_pop = Ansi.(to_string csi_u_pop)
let modify_other_keys_on_seq = Ansi.(to_string modify_other_keys_on)
let modify_other_keys_off_seq = Ansi.(to_string modify_other_keys_off)
let cursor_show = Ansi.(to_string (enable Cursor_visible))
let cursor_hide = Ansi.(to_string (disable Cursor_visible))
let sgr_enable = Ansi.(to_string (enable Mouse_sgr))
let unicode_on = Ansi.(to_string (enable Unicode))
let unicode_off = Ansi.(to_string (disable Unicode))
let reset_sgr = Ansi.(to_string reset)
let erase_below = Ansi.(to_string erase_below_cursor)
let cursor_default = Ansi.(to_string (cursor_style ~shape:`Default))

let reset_cursor_color_fallback_seq =
  Ansi.(to_string reset_cursor_color_fallback)

let reset_cursor_color_seq = Ansi.(to_string reset_cursor_color)
let cursor_block = Ansi.(to_string (cursor_style ~shape:`Block))
let cursor_block_blink = Ansi.(to_string (cursor_style ~shape:`Blinking_block))
let cursor_line = Ansi.(to_string (cursor_style ~shape:`Bar))
let cursor_line_blink = Ansi.(to_string (cursor_style ~shape:`Blinking_bar))
let cursor_underline = Ansi.(to_string (cursor_style ~shape:`Underline))

let cursor_underline_blink =
  Ansi.(to_string (cursor_style ~shape:`Blinking_underline))

let mouse_x10 = Ansi.(to_string (enable Mouse_x10))
let mouse_tracking = Ansi.(to_string (enable Mouse_tracking))
let mouse_button = Ansi.(to_string (enable Mouse_button_tracking))
let mouse_motion = Ansi.(to_string (enable Mouse_motion))

let disable_all_mouse_seq =
  String.concat ""
    [
      Ansi.(to_string (disable Mouse_tracking));
      Ansi.(to_string (disable Mouse_button_tracking));
      Ansi.(to_string (disable Mouse_motion));
      Ansi.(to_string (disable Urxvt_mouse));
      Ansi.(to_string (disable Mouse_sgr));
    ]

let sgr_normal_seq = String.concat "" [ sgr_enable; mouse_tracking ]

let sgr_button_seq =
  String.concat "" [ sgr_enable; mouse_tracking; mouse_button ]

let sgr_any_seq =
  String.concat "" [ sgr_enable; mouse_tracking; mouse_button; mouse_motion ]

(* Helpers *)

let make_osc payload = Ansi.(to_string (osc ~terminator:`Bel ~payload))

let clamp_color_component x =
  let x = if Float.is_nan x then 0. else x in
  int_of_float (Float.round (Float.max 0. (Float.min 1. x) *. 255.))

(* Core output *)

let send t seq = if t.tty then t.output seq
let tty t = t.tty
let parser t = t.parser

(* Idempotent toggle: checks current state, emits sequence, updates state *)
let toggle t ~current ~set ~enable ~on_seq ~off_seq =
  if current () = enable then ()
  else (
    send t (if enable then on_seq else off_seq);
    set enable)

(* Constructor *)

let make ~output ?(tty = true) ?initial_caps ?parser () =
  let term = Sys.getenv_opt "TERM" |> Option.value ~default:"unknown" in
  let caps, terminal_info = Caps.initial ?provided:initial_caps ~term () in
  let env_overrides = Option.is_none initial_caps in
  let parser =
    match parser with Some p -> p | None -> Input.Parser.create ()
  in
  {
    output;
    tty;
    caps;
    terminal_info;
    parser;
    mouse_mode = `Off;
    bracketed_paste_enabled = false;
    focus_enabled = false;
    kitty_keyboard_enabled = false;
    kitty_keyboard_flags = 0b00101;
    modify_other_keys_enabled = false;
    unicode_mode_enabled = false;
    cursor =
      {
        x = 1;
        y = 1;
        visible = true;
        style = `Block;
        blinking = false;
        color = (1., 1., 1., 1.);
      };
    alt_screen = false;
    scroll_region = None;
    pixel_resolution = None;
    env_overrides;
  }

(* Capability access *)

let capabilities t = t.caps
let set_capabilities t caps = t.caps <- caps
let terminal_info t = t.terminal_info
let set_terminal_info t info = t.terminal_info <- info
let pixel_resolution t = t.pixel_resolution
let set_pixel_resolution t res = t.pixel_resolution <- res

let apply_capability_event t (event : Input.Caps.event) =
  let caps, info =
    Caps.apply_event ~apply_env_overrides:t.env_overrides ~caps:t.caps
      ~info:t.terminal_info event
  in
  t.caps <- caps;
  t.terminal_info <- info;
  match event with
  | Input.Caps.Pixel_resolution (w, h) -> t.pixel_resolution <- Some (w, h)
  | _ -> ()

(* Probing *)

let probe ?(timeout = 0.2) ~on_event ~read_into ~wait_readable t =
  let caps, info =
    Caps.probe ~timeout ~apply_env_overrides:t.env_overrides ~on_event
      ~read_into ~wait_readable ~send:(send t) ~caps:t.caps
      ~info:t.terminal_info ()
  in
  t.caps <- caps;
  t.terminal_info <- info

(* Mouse mode *)

let disable_all_mouse t = send t disable_all_mouse_seq

let set_mouse_mode t mode =
  if t.mouse_mode = mode then ()
  else (
    disable_all_mouse t;
    (match mode with
    | `Off -> ()
    | `X10 -> send t mouse_x10
    | `Normal -> send t mouse_tracking
    | `Button -> send t mouse_button
    | `Any -> send t mouse_motion
    | `Sgr_normal -> send t sgr_normal_seq
    | `Sgr_button -> send t sgr_button_seq
    | `Sgr_any -> send t sgr_any_seq);
    t.mouse_mode <- mode)

let mouse_mode t = t.mouse_mode

(* Bracketed paste *)

let enable_bracketed_paste t enable =
  toggle t
    ~current:(fun () -> t.bracketed_paste_enabled)
    ~set:(fun v -> t.bracketed_paste_enabled <- v)
    ~enable ~on_seq:paste_on ~off_seq:paste_off

let bracketed_paste_enabled t = t.bracketed_paste_enabled

(* Focus reporting *)

let enable_focus_reporting t enable =
  toggle t
    ~current:(fun () -> t.focus_enabled)
    ~set:(fun v -> t.focus_enabled <- v)
    ~enable ~on_seq:focus_on ~off_seq:focus_off

let focus_reporting_enabled t = t.focus_enabled

(* Kitty keyboard *)

let enable_kitty_keyboard ?(flags = 0b00101) t enable =
  if enable then (
    if (not t.kitty_keyboard_enabled) || t.kitty_keyboard_flags <> flags then (
      send t (kitty_kb_push flags);
      t.kitty_keyboard_enabled <- true;
      t.kitty_keyboard_flags <- flags))
  else if t.kitty_keyboard_enabled then (
    send t kitty_kb_pop;
    t.kitty_keyboard_enabled <- false)

let kitty_keyboard_enabled t = t.kitty_keyboard_enabled

(* Modify other keys *)

let enable_modify_other_keys t enable =
  toggle t
    ~current:(fun () -> t.modify_other_keys_enabled)
    ~set:(fun v -> t.modify_other_keys_enabled <- v)
    ~enable ~on_seq:modify_other_keys_on_seq ~off_seq:modify_other_keys_off_seq

let modify_other_keys_enabled t = t.modify_other_keys_enabled

(* Unicode width *)

let set_unicode_width t width =
  let enable = match width with `Unicode -> true | `Wcwidth -> false in
  toggle t
    ~current:(fun () -> t.unicode_mode_enabled)
    ~set:(fun v -> t.unicode_mode_enabled <- v)
    ~enable ~on_seq:unicode_on ~off_seq:unicode_off;
  t.caps <- { t.caps with unicode_width = width }

(* Alternate screen *)

let enter_alternate_screen t =
  if not t.alt_screen then (
    send t alternate_on;
    send t cursor_hide;
    t.cursor.visible <- false;
    t.alt_screen <- true)

let leave_alternate_screen t =
  if t.alt_screen then (
    send t alternate_off;
    t.alt_screen <- false)

let alt_screen t = t.alt_screen

(* Scroll region *)

let set_scroll_region t ~top ~bottom =
  if t.scroll_region <> Some (top, bottom) then (
    send t Ansi.(to_string (set_scrolling_region ~top ~bottom));
    t.scroll_region <- Some (top, bottom))

let clear_scroll_region t =
  if t.scroll_region <> None then (
    send t Ansi.(to_string reset_scrolling_region);
    t.scroll_region <- None)

let scroll_region t = t.scroll_region

(* Cursor *)

let set_cursor_visible t visible =
  toggle t
    ~current:(fun () -> t.cursor.visible)
    ~set:(fun v -> t.cursor.visible <- v)
    ~enable:visible ~on_seq:cursor_show ~off_seq:cursor_hide

let cursor_visible t = t.cursor.visible

let cursor_position t =
  { x = t.cursor.x; y = t.cursor.y; visible = t.cursor.visible }

let move_cursor ?(visible = true) t ~row ~col =
  let row = max 1 row in
  let col = max 1 col in
  if visible <> t.cursor.visible then set_cursor_visible t visible;
  t.cursor.x <- col;
  t.cursor.y <- row;
  send t Ansi.(to_string (cursor_position ~row ~col))

let cursor_style_state t = (t.cursor.style, t.cursor.blinking)
let cursor_color t = t.cursor.color

let cursor_color_osc r g b =
  let r = clamp_color_component r in
  let g = clamp_color_component g in
  let b = clamp_color_component b in
  make_osc (Printf.sprintf "12;#%02X%02X%02X" r g b)

let cursor_style_seq style blinking =
  match (style, blinking) with
  | `Block, true -> cursor_block_blink
  | `Block, false -> cursor_block
  | `Line, true -> cursor_line_blink
  | `Line, false -> cursor_line
  | `Underline, true -> cursor_underline_blink
  | `Underline, false -> cursor_underline

let set_cursor_visuals t =
  if not t.tty then ()
  else if t.cursor.visible then (
    let r, g, b, _ = t.cursor.color in
    send t (cursor_color_osc r g b);
    send t (cursor_style_seq t.cursor.style t.cursor.blinking))
  else send t cursor_hide

let set_cursor_style t style ~blinking =
  t.cursor.style <- style;
  t.cursor.blinking <- blinking;
  set_cursor_visuals t

let set_cursor_color t ~r ~g ~b ~a =
  t.cursor.color <- (r, g, b, a);
  set_cursor_visuals t

let reset_cursor_color t =
  t.cursor.color <- (1., 1., 1., 1.);
  send t reset_cursor_color_fallback_seq;
  send t reset_cursor_color_seq

let set_title t title = send t Ansi.(to_string (set_title ~title))
let query_pixel_resolution t = send t Ansi.(to_string (query Pixel_size))

(* Mode restoration *)

let restore_modes t =
  (match t.mouse_mode with
  | `Off -> ()
  | `X10 -> send t mouse_x10
  | `Normal -> send t mouse_tracking
  | `Button -> send t mouse_button
  | `Any -> send t mouse_motion
  | `Sgr_normal -> send t sgr_normal_seq
  | `Sgr_button -> send t sgr_button_seq
  | `Sgr_any -> send t sgr_any_seq);
  if t.focus_enabled then send t focus_on;
  if t.bracketed_paste_enabled then send t paste_on;
  if t.kitty_keyboard_enabled then (
    send t kitty_kb_pop;
    send t (kitty_kb_push t.kitty_keyboard_flags));
  if t.modify_other_keys_enabled then send t modify_other_keys_on_seq

(* Reset and close *)

let reset_state t =
  send t cursor_show;
  send t reset_sgr;
  send t reset_cursor_color_fallback_seq;
  send t reset_cursor_color_seq;
  send t cursor_default;
  if t.kitty_keyboard_enabled then enable_kitty_keyboard t false;
  if t.modify_other_keys_enabled then enable_modify_other_keys t false;
  if t.mouse_mode <> `Off then set_mouse_mode t `Off;
  if t.bracketed_paste_enabled then enable_bracketed_paste t false;
  if t.focus_enabled then enable_focus_reporting t false;
  if t.unicode_mode_enabled then set_unicode_width t `Wcwidth;
  if t.scroll_region <> None then clear_scroll_region t;
  if t.alt_screen then leave_alternate_screen t
  else if Sys.win32 then (
    send t "\r";
    for _ = 1 to max 0 (t.cursor.y - 1) do
      send t Ansi.(to_string (cursor_up ~n:1))
    done;
    send t erase_below);
  set_title t "";
  send t cursor_show;
  t.cursor.visible <- true

let close t = reset_state t

(* TTY helpers *)

let is_tty fd = try Unix.isatty fd with Unix.Unix_error _ -> false

let set_raw fd =
  let original = Unix.tcgetattr fd in
  let raw =
    {
      original with
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_vmin = 1;
      c_vtime = 0;
      c_ixon = false;
      c_icrnl = false;
    }
  in
  Unix.tcsetattr fd Unix.TCSANOW raw;
  (try Unix.set_nonblock fd with Unix.Unix_error _ -> ());
  original

let restore fd termios =
  Unix.tcsetattr fd Unix.TCSANOW termios;
  try Unix.clear_nonblock fd with Unix.Unix_error _ -> ()

let size fd = try get_size fd with _ -> (80, 24)
let flush_input fd = try Unix.tcflush fd Unix.TCIFLUSH with _ -> ()
let enable_vt fd = try enable_vt_raw fd with _ -> ()
