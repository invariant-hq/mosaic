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
  mutable mouse_mode : mouse_mode;
  mutable bracketed_paste_enabled : bool;
  mutable focus_enabled : bool;
  mutable kitty_keyboard_enabled : bool;
  mutable kitty_keyboard_flags : int;
  mutable modify_other_keys_enabled : bool;
  mutable unicode_mode_enabled : bool;
  mutable mouse_armed : bool;
  mutable bracketed_paste_armed : bool;
  mutable focus_armed : bool;
  mutable kitty_keyboard_armed : bool;
  mutable modify_other_keys_armed : bool;
  cursor : cursor_state;
  mutable alt_screen : bool;
  mutable scroll_region : (int * int) option;
  mutable pixel_resolution : (int * int) option;
  env_overrides : bool;
}

(* Pre-computed escape sequences *)

let alternate_on = "\027[?1049h"
let alternate_off = "\027[?1049l"
let focus_on = "\027[?1004h"
let focus_off = "\027[?1004l"
let paste_on = "\027[?2004h"
let paste_off = "\027[?2004l"
let kitty_kb_push flags = Ansi.(to_string (csi_u_push ~flags))
let kitty_kb_pop = "\027[<u"
let modify_other_keys_on_seq = "\027[>4;1m"
let modify_other_keys_off_seq = "\027[>4;0m"
let cursor_show = "\027[?25h"
let cursor_hide = "\027[?25l"
let unicode_on = "\027[?2027h"
let unicode_off = "\027[?2027l"
let reset_sgr = "\027[0m"
let erase_below = "\027[J"
let cursor_default = "\027[0 q"
let reset_cursor_color_fallback_seq = "\027]12;default\007"
let reset_cursor_color_seq = "\027]112\007"
let cursor_block = "\027[2 q"
let cursor_block_blink = "\027[1 q"
let cursor_line = "\027[6 q"
let cursor_line_blink = "\027[5 q"
let cursor_underline = "\027[4 q"
let cursor_underline_blink = "\027[3 q"
let mouse_x10 = "\027[?9h"
let mouse_tracking = "\027[?1000h"
let mouse_button = "\027[?1002h"
let mouse_motion = "\027[?1003h"

let disable_all_mouse_seq =
  "\027[?9l\027[?1000l\027[?1002l\027[?1003l\027[?1015l\027[?1006l"

let sgr_normal_seq = "\027[?1006h\027[?1000h"
let sgr_button_seq = "\027[?1006h\027[?1000h\027[?1002h"
let sgr_any_seq = "\027[?1006h\027[?1000h\027[?1002h\027[?1003h"

(* Helpers *)

let make_osc payload = Ansi.(to_string (osc ~terminator:`Bel ~payload))

let clamp_color_component x =
  let x = if Float.is_nan x then 0. else x in
  int_of_float (Float.round (Float.max 0. (Float.min 1. x) *. 255.))

(* Core output *)

let send t seq = if t.tty then t.output seq
let tty t = t.tty

(* Idempotent toggle: checks current state, emits sequence, updates state *)
let toggle t ~current ~set ~enable ~on_seq ~off_seq =
  if current () = enable then ()
  else (
    send t (if enable then on_seq else off_seq);
    set enable)

(* Constructor *)

let make ~output ?(tty = true) ?initial_caps () =
  let term = Sys.getenv_opt "TERM" |> Option.value ~default:"unknown" in
  let caps, terminal_info = Caps.initial ?provided:initial_caps ~term () in
  let env_overrides = Option.is_none initial_caps in
  {
    output;
    tty;
    caps;
    terminal_info;
    mouse_mode = `Off;
    bracketed_paste_enabled = false;
    focus_enabled = false;
    kitty_keyboard_enabled = false;
    kitty_keyboard_flags = 0b00101;
    modify_other_keys_enabled = false;
    unicode_mode_enabled = false;
    mouse_armed = false;
    bracketed_paste_armed = false;
    focus_armed = false;
    kitty_keyboard_armed = false;
    modify_other_keys_armed = false;
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

let apply_capability_event t (event : Input.Response.capability) =
  let caps, info =
    Caps.apply_event ~apply_env_overrides:t.env_overrides ~caps:t.caps
      ~info:t.terminal_info event
  in
  t.caps <- caps;
  t.terminal_info <- info;
  match event with
  | Input.Response.Pixel_resolution (w, h) -> t.pixel_resolution <- Some (w, h)
  | _ -> ()

(* Probing *)

let probe ?(timeout = 0.2) ~on_event ~read_into ~wait_readable ~parser t =
  let caps, info =
    Caps.probe ~timeout ~apply_env_overrides:t.env_overrides ~on_event
      ~read_into ~wait_readable ~send:(send t) ~parser ~caps:t.caps
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
    if mode <> `Off then t.mouse_armed <- true;
    (match mode with
    | `Off -> ()
    | `X10 -> send t mouse_x10
    | `Normal -> send t mouse_tracking
    | `Button -> send t mouse_button
    | `Any -> send t mouse_motion
    | `Sgr_normal -> send t sgr_normal_seq
    | `Sgr_button -> send t sgr_button_seq
    | `Sgr_any -> send t sgr_any_seq);
    t.mouse_mode <- mode;
    if mode = `Off then t.mouse_armed <- false)

let mouse_mode t = t.mouse_mode

(* Bracketed paste *)

let enable_bracketed_paste t enable =
  if t.bracketed_paste_enabled = enable then ()
  else if enable then (
    t.bracketed_paste_armed <- true;
    send t paste_on;
    t.bracketed_paste_enabled <- true)
  else (
    send t paste_off;
    t.bracketed_paste_enabled <- false;
    t.bracketed_paste_armed <- false)

let bracketed_paste_enabled t = t.bracketed_paste_enabled

(* Focus reporting *)

let enable_focus_reporting t enable =
  if t.focus_enabled = enable then ()
  else if enable then (
    t.focus_armed <- true;
    send t focus_on;
    t.focus_enabled <- true)
  else (
    send t focus_off;
    t.focus_enabled <- false;
    t.focus_armed <- false)

let focus_reporting_enabled t = t.focus_enabled

(* Kitty keyboard *)

let enable_kitty_keyboard ?(flags = 0b00101) t enable =
  if enable then (
    if (not t.kitty_keyboard_enabled) || t.kitty_keyboard_flags <> flags then (
      t.kitty_keyboard_armed <- true;
      send t (kitty_kb_push flags);
      t.kitty_keyboard_enabled <- true;
      t.kitty_keyboard_flags <- flags))
  else if t.kitty_keyboard_enabled then (
    send t kitty_kb_pop;
    t.kitty_keyboard_enabled <- false;
    t.kitty_keyboard_armed <- false)

let kitty_keyboard_enabled t = t.kitty_keyboard_enabled

(* Modify other keys *)

let enable_modify_other_keys t enable =
  if t.modify_other_keys_enabled = enable then ()
  else if enable then (
    t.modify_other_keys_armed <- true;
    send t modify_other_keys_on_seq;
    t.modify_other_keys_enabled <- true)
  else (
    send t modify_other_keys_off_seq;
    t.modify_other_keys_enabled <- false;
    t.modify_other_keys_armed <- false)

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

let restore_modes ?(skip_focus = false) t =
  (match t.mouse_mode with
  | `Off -> ()
  | `X10 -> send t mouse_x10
  | `Normal -> send t mouse_tracking
  | `Button -> send t mouse_button
  | `Any -> send t mouse_motion
  | `Sgr_normal -> send t sgr_normal_seq
  | `Sgr_button -> send t sgr_button_seq
  | `Sgr_any -> send t sgr_any_seq);
  if t.focus_enabled && not skip_focus then send t focus_on;
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
  if t.kitty_keyboard_armed then send t kitty_kb_pop;
  t.kitty_keyboard_enabled <- false;
  t.kitty_keyboard_armed <- false;
  t.kitty_keyboard_flags <- 0b00101;
  if t.modify_other_keys_armed then send t modify_other_keys_off_seq;
  t.modify_other_keys_enabled <- false;
  t.modify_other_keys_armed <- false;
  if t.mouse_armed || t.mouse_mode <> `Off then disable_all_mouse t;
  t.mouse_mode <- `Off;
  t.mouse_armed <- false;
  if t.bracketed_paste_armed then send t paste_off;
  t.bracketed_paste_enabled <- false;
  t.bracketed_paste_armed <- false;
  if t.focus_armed then send t focus_off;
  t.focus_enabled <- false;
  t.focus_armed <- false;
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
  t.cursor.visible <- true;
  t.cursor.style <- `Block;
  t.cursor.blinking <- false;
  t.cursor.color <- (1., 1., 1., 1.)

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
