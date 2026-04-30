open Windtrap
module T = Terminal

let contains s needle =
  let n = String.length needle in
  let rec loop i =
    if i + n > String.length s then false
    else if String.sub s i n = needle then true
    else loop (i + 1)
  in
  n = 0 || loop 0

(* Helper to create a non-TTY terminal with a buffer-backed output *)
let with_terminal ?initial_caps f =
  let buf = Buffer.create 4096 in
  let output s = Buffer.add_string buf s in
  let term = T.make ~output ~tty:false ?initial_caps () in
  f term buf

let with_tty_terminal ?initial_caps f =
  let buf = Buffer.create 4096 in
  let output s = Buffer.add_string buf s in
  let term = T.make ~output ~tty:true ?initial_caps () in
  f term buf

let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true
  else if len_sub > len_s then false
  else
    let rec loop idx =
      if idx + len_sub > len_s then false
      else if String.sub s idx len_sub = sub then true
      else loop (idx + 1)
    in
    loop 0

let with_env bindings f =
  let saved = List.map (fun (key, _) -> (key, Sys.getenv_opt key)) bindings in
  Fun.protect
    (fun () ->
      List.iter (fun (key, value) -> Unix.putenv key value) bindings;
      f ())
    ~finally:(fun () ->
      List.iter
        (fun (key, value) -> Unix.putenv key (Option.value ~default:"" value))
        saved)

(* Test: Non-TTY terminal doesn't send escape sequences *)
let test_non_tty_no_escape_sequences () =
  with_terminal @@ fun term buf ->
  T.set_cursor_visible term false;
  T.set_mouse_mode term `Sgr_any;
  T.enable_bracketed_paste term true;
  T.enable_focus_reporting term true;
  T.enter_alternate_screen term;
  T.set_title term "Test";

  is_true ~msg:"no escape sequences on non-TTY" (Buffer.length buf = 0);

  T.close term

(* Test: Non-TTY terminal still tracks state *)
let test_non_tty_state_tracking () =
  with_terminal @@ fun term _buf ->
  T.set_cursor_visible term false;
  is_false ~msg:"cursor visible state" (T.cursor_visible term);

  T.move_cursor term ~row:20 ~col:10;
  let pos = T.cursor_position term in
  equal ~msg:"cursor x" int 10 pos.x;
  equal ~msg:"cursor y" int 20 pos.y;
  is_true ~msg:"cursor visible" pos.visible;

  T.set_cursor_style term `Line ~blinking:true;
  let style, blinking = T.cursor_style_state term in
  let style_str =
    match style with
    | `Block -> "Block"
    | `Line -> "Line"
    | `Underline -> "Underline"
  in
  equal ~msg:"cursor style" string "Line" style_str;
  is_true ~msg:"cursor blinking" blinking;

  T.set_cursor_color term ~r:0.5 ~g:0.25 ~b:0.75 ~a:1.0;
  let r, g, b, a = T.cursor_color term in
  equal ~msg:"color r" (float 0.01) 0.5 r;
  equal ~msg:"color g" (float 0.01) 0.25 g;
  equal ~msg:"color b" (float 0.01) 0.75 b;
  equal ~msg:"color a" (float 0.01) 1.0 a;

  T.set_mouse_mode term `Sgr_button;
  let mouse_mode_str =
    match T.mouse_mode term with
    | `Off -> "Off"
    | `X10 -> "X10"
    | `Normal -> "Normal"
    | `Button -> "Button"
    | `Any -> "Any"
    | `Sgr_normal -> "Sgr_normal"
    | `Sgr_button -> "Sgr_button"
    | `Sgr_any -> "Sgr_any"
  in
  equal ~msg:"mouse mode" string "Sgr_button" mouse_mode_str;

  T.close term

(* Test: Helper function - contains_substring *)
let test_contains_substring () =
  let module Impl = struct
    let contains_substring s sub =
      let len_s = String.length s and len_sub = String.length sub in
      if len_sub = 0 then true
      else
        let rec loop idx =
          if idx + len_sub > len_s then false
          else if String.sub s idx len_sub = sub then true
          else loop (idx + 1)
        in
        loop 0
  end in
  is_true ~msg:"contains empty" (Impl.contains_substring "hello" "");
  is_true ~msg:"contains at start" (Impl.contains_substring "hello" "hel");
  is_true ~msg:"contains in middle" (Impl.contains_substring "hello" "ell");
  is_true ~msg:"contains at end" (Impl.contains_substring "hello" "llo");
  is_true ~msg:"contains full" (Impl.contains_substring "hello" "hello");
  is_false ~msg:"not contains" (Impl.contains_substring "hello" "world");
  is_false ~msg:"not contains substring too long"
    (Impl.contains_substring "hi" "hello")

(* Test: Capability normalization *)
let test_capability_normalization () =
  let caps =
    {
      T.term = "test";
      rgb = true;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = true;
      focus_tracking = true;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_terminal ~initial_caps:caps @@ fun term _buf ->
  let normalized = T.capabilities term in
  is_true ~msg:"rgb preserved" normalized.rgb;
  is_true ~msg:"focus_tracking preserved" normalized.focus_tracking;
  T.close term

(* Test: TERM_PROGRAM fallback populates terminal info when XTVersion missing *)
let test_terminal_info_from_env () =
  let save key = Sys.getenv_opt key in
  let term_prog = save "TERM_PROGRAM" in
  let term_prog_ver = save "TERM_PROGRAM_VERSION" in
  Fun.protect
    (fun () ->
      Unix.putenv "TERM_PROGRAM" "Alacritty";
      Unix.putenv "TERM_PROGRAM_VERSION" "1.99";
      with_terminal @@ fun term _buf ->
      let info = T.terminal_info term in
      equal ~msg:"env terminal name" string "Alacritty" info.name;
      equal ~msg:"env terminal version" string "1.99" info.version;
      is_false ~msg:"env info not from xtversion" info.from_xtversion;
      T.close term)
    ~finally:(fun () ->
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> Unix.putenv "TERM_PROGRAM" "");
      match term_prog_ver with
      | Some v -> Unix.putenv "TERM_PROGRAM_VERSION" v
      | None -> Unix.putenv "TERM_PROGRAM_VERSION" "")

(* Test: Cursor position clamping *)
let test_cursor_position_clamping () =
  with_terminal @@ fun term _buf ->
  (* Negative values should be clamped to 1 *)
  T.move_cursor term ~row:(-10) ~col:(-5);
  let pos = T.cursor_position term in
  equal ~msg:"x clamped to 1" int 1 pos.x;
  equal ~msg:"y clamped to 1" int 1 pos.y;

  (* Zero should be clamped to 1 *)
  T.set_cursor_visible term false;
  T.move_cursor term ~row:0 ~col:0;
  let pos = T.cursor_position term in
  equal ~msg:"x=0 clamped to 1" int 1 pos.x;
  equal ~msg:"y=0 clamped to 1" int 1 pos.y;

  T.close term

(* Test: Color component clamping *)
let test_color_clamping () =
  with_terminal @@ fun term _buf ->
  (* Out of range values *)
  T.set_cursor_color term ~r:2.5 ~g:(-1.0) ~b:0.5 ~a:1.5;
  let r, g, b, a = T.cursor_color term in
  equal ~msg:"r stored" (float 0.01) 2.5 r;
  equal ~msg:"g stored" (float 0.01) (-1.0) g;
  equal ~msg:"b stored" (float 0.01) 0.5 b;
  equal ~msg:"a stored" (float 0.01) 1.5 a;
  T.close term

(* Test: Environment variable capability overrides *)
let test_env_overrides () =
  let save_env key = Sys.getenv_opt key in
  let kitty_id = save_env "KITTY_WINDOW_ID" in
  let term_prog = save_env "TERM_PROGRAM" in
  let colorterm = save_env "COLORTERM" in
  Fun.protect
    (fun () ->
      Unix.putenv "KITTY_WINDOW_ID" "1";
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> ());
      (match colorterm with Some v -> Unix.putenv "COLORTERM" v | None -> ());
      let caps =
        {
          T.term = "xterm";
          rgb = false;
          kitty_keyboard = false;
          kitty_graphics = false;
          bracketed_paste = false;
          focus_tracking = false;
          unicode_width = `Wcwidth;
          sgr_pixels = false;
          color_scheme_updates = false;
          explicit_width = false;
          explicit_cursor_positioning = false;
          scaled_text = false;
          sixel = false;
          sync = false;
          hyperlinks = false;
        }
      in
      with_terminal ~initial_caps:caps @@ fun term _buf ->
      (* KITTY_WINDOW_ID should not override initial_caps when provided *)
      let current_caps = T.capabilities term in
      is_false ~msg:"explicit caps not overridden" current_caps.kitty_keyboard;
      T.close term)
    ~finally:(fun () ->
      (match kitty_id with
      | Some v -> Unix.putenv "KITTY_WINDOW_ID" v
      | None -> Unix.putenv "KITTY_WINDOW_ID" "");
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> Unix.putenv "TERM_PROGRAM" "");
      match colorterm with
      | Some v -> Unix.putenv "COLORTERM" v
      | None -> Unix.putenv "COLORTERM" "")

(* Test: TTY terminal emits escape sequences via send *)
let test_tty_send () =
  with_tty_terminal @@ fun term buf ->
  T.send term "Hello";
  T.send term " World";
  let output_data = Buffer.contents buf in
  equal ~msg:"send output" string "Hello World" output_data;
  T.close term

(* Test: make with initial_caps *)
let test_make_with_caps () =
  let caps =
    {
      T.term = "test";
      rgb = true;
      kitty_keyboard = true;
      kitty_graphics = false;
      bracketed_paste = true;
      focus_tracking = true;
      unicode_width = `Unicode;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = true;
      hyperlinks = false;
    }
  in
  with_terminal ~initial_caps:caps @@ fun term _buf ->
  let c = T.capabilities term in
  is_true ~msg:"rgb" c.rgb;
  is_true ~msg:"kitty_keyboard" c.kitty_keyboard;
  is_true ~msg:"sync" c.sync;
  T.close term

(* Regression: OpenTUI treats CSI ?0u as Kitty keyboard support. *)
let test_kitty_keyboard_level_zero_capability () =
  let caps =
    {
      T.term = "xterm";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_terminal ~initial_caps:caps @@ fun term _buf ->
  is_false ~msg:"kitty initially off" (T.capabilities term).kitty_keyboard;
  T.apply_capability_event term
    (Input.Response.Kitty_keyboard { level = 0; flags = None });
  is_true ~msg:"kitty enabled by level zero response"
    (T.capabilities term).kitty_keyboard;
  T.close term

(* Regression: startup probe uses DECRQM 2031 and avoids CSI ?996n. *)
let test_probe_payload_color_scheme_mode () =
  with_tty_terminal @@ fun term buf ->
  let parser = Input.Parser.create () in
  T.probe ~timeout:0.0
    ~on_event:(fun _ -> ())
    ~read_into:(fun _ _ _ -> 0)
    ~wait_readable:(fun ~timeout:_ -> false)
    ~parser term;
  let output_data = Buffer.contents buf in
  is_true ~msg:"probe queries color scheme update mode"
    (contains_substring output_data "\027[?2031$p");
  is_false ~msg:"probe does not send color scheme DSR"
    (contains_substring output_data "\027[?996n");
  T.close term

let test_probe_payload_screen_is_not_tmux_wrapped () =
  let caps =
    {
      T.term = "screen-256color";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = true;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_env [ ("TMUX", "") ] @@ fun () ->
  with_tty_terminal ~initial_caps:caps @@ fun term buf ->
  let parser = Input.Parser.create () in
  T.probe ~timeout:0.0
    ~on_event:(fun _ -> ())
    ~read_into:(fun _ _ _ -> 0)
    ~wait_readable:(fun ~timeout:_ -> false)
    ~parser term;
  let output_data = Buffer.contents buf in
  is_false ~msg:"screen probe is not tmux wrapped"
    (contains_substring output_data "\027Ptmux;");
  is_false ~msg:"screen skips graphics query"
    (contains_substring output_data "\027_Gi=31337");
  T.close term

let test_probe_payload_tmux_is_wrapped () =
  let caps =
    {
      T.term = "tmux-256color";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = true;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_tty_terminal ~initial_caps:caps @@ fun term buf ->
  let parser = Input.Parser.create () in
  T.probe ~timeout:0.0
    ~on_event:(fun _ -> ())
    ~read_into:(fun _ _ _ -> 0)
    ~wait_readable:(fun ~timeout:_ -> false)
    ~parser term;
  let output_data = Buffer.contents buf in
  is_true ~msg:"tmux probe wraps DECRQM block"
    (contains_substring output_data "\027Ptmux;\027\027[?1016$p");
  T.close term

let test_probe_xtversion_tmux_resends_pending_queries_wrapped () =
  let caps =
    {
      T.term = "xterm";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_tty_terminal ~initial_caps:caps @@ fun term buf ->
  let parser = Input.Parser.create () in
  let input = Bytes.of_string "\027P>|tmux 3.5a\027\\" in
  let consumed = ref false in
  T.probe ~timeout:0.1
    ~on_event:(fun _ -> ())
    ~read_into:(fun buf off len ->
      if !consumed then 0
      else (
        consumed := true;
        let n = min len (Bytes.length input) in
        Bytes.blit input 0 buf off n;
        n))
    ~wait_readable:(fun ~timeout:_ -> not !consumed)
    ~parser term;
  let output_data = Buffer.contents buf in
  is_true ~msg:"initial probe sends unwrapped queries"
    (contains_substring output_data "\027[?1016$p");
  is_true ~msg:"XTVersion tmux resends pending queries wrapped"
    (contains_substring output_data "\027Ptmux;\027\027[?1016$p");
  T.close term

let test_probe_xtversion_non_tmux_does_not_resend_wrapped () =
  let caps =
    {
      T.term = "xterm";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_tty_terminal ~initial_caps:caps @@ fun term buf ->
  let parser = Input.Parser.create () in
  let input = Bytes.of_string "\027P>|ghostty 1.2.3\027\\" in
  let consumed = ref false in
  T.probe ~timeout:0.1
    ~on_event:(fun _ -> ())
    ~read_into:(fun buf off len ->
      if !consumed then 0
      else (
        consumed := true;
        let n = min len (Bytes.length input) in
        Bytes.blit input 0 buf off n;
        n))
    ~wait_readable:(fun ~timeout:_ -> not !consumed)
    ~parser term;
  let output_data = Buffer.contents buf in
  is_false ~msg:"non-tmux XTVersion does not wrap pending queries"
    (contains_substring output_data "\027Ptmux;");
  T.close term

let test_probe_preserves_user_input_and_capabilities () =
  let caps =
    {
      T.term = "xterm";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = true;
      explicit_cursor_positioning = false;
      scaled_text = true;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_tty_terminal ~initial_caps:caps @@ fun term _buf ->
  let parser = Input.Parser.create () in
  let input = Bytes.of_string "a\027[?0u\027[?62;c" in
  let consumed = ref false in
  let events = ref [] in
  T.probe ~timeout:0.1
    ~on_event:(fun event -> events := event :: !events)
    ~read_into:(fun buf off len ->
      if !consumed then 0
      else (
        consumed := true;
        let n = min len (Bytes.length input) in
        Bytes.blit input 0 buf off n;
        n))
    ~wait_readable:(fun ~timeout:_ -> not !consumed)
    ~parser term;
  (match List.rev !events with
  | [ event ] ->
      is_true ~msg:"probe forwards ordinary input"
        (Input.equal (Input.char 'a') event)
  | events ->
      failf "expected one forwarded probe input event, got %d"
        (List.length events));
  is_true ~msg:"probe still folds capability replies"
    (T.capabilities term).kitty_keyboard;
  T.close term

(* Regression: X10 mouse tracking must be disabled on teardown. *)
let test_x10_mouse_disable () =
  with_tty_terminal @@ fun term buf ->
  T.set_mouse_mode term `X10;
  T.set_mouse_mode term `Off;
  let output_data = Buffer.contents buf in
  is_true ~msg:"X10 enabled" (contains_substring output_data "\027[?9h");
  is_true ~msg:"X10 disabled" (contains_substring output_data "\027[?9l");
  T.close term

let test_explicit_cursor_positioning_env_overrides () =
  with_env
    [
      ("TERM", "tmux-256color");
      ("TMUX", "/tmp/tmux-1000/default,12345,0");
      ("TERM_PROGRAM", "");
    ]
    (fun () ->
      with_terminal @@ fun term _buf ->
      let caps = T.capabilities term in
      equal ~msg:"tmux width" string "Wcwidth"
        (match caps.unicode_width with
        | `Wcwidth -> "Wcwidth"
        | `Unicode -> "Unicode");
      is_true ~msg:"tmux explicit cursor positioning"
        caps.explicit_cursor_positioning;
      T.close term);
  with_env
    [ ("TERM", "screen-256color"); ("TMUX", ""); ("TERM_PROGRAM", "") ]
    (fun () ->
      with_terminal @@ fun term _buf ->
      let caps = T.capabilities term in
      equal ~msg:"screen width" string "Wcwidth"
        (match caps.unicode_width with
        | `Wcwidth -> "Wcwidth"
        | `Unicode -> "Unicode");
      is_true ~msg:"screen explicit cursor positioning"
        caps.explicit_cursor_positioning;
      T.close term);
  with_env
    [ ("TERM", "xterm-256color"); ("TMUX", ""); ("TERM_PROGRAM", "Alacritty") ]
    (fun () ->
      with_terminal @@ fun term _buf ->
      is_true ~msg:"alacritty explicit cursor positioning"
        (T.capabilities term).explicit_cursor_positioning;
      T.close term)

(* Test: Mouse mode validation on non-TTY *)
let test_mouse_validation () =
  let caps =
    {
      T.term = "test";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      explicit_cursor_positioning = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in
  with_terminal ~initial_caps:caps @@ fun term _buf ->
  T.set_mouse_mode term `Sgr_any;
  equal ~msg:"mouse mode set on non-TTY" string "Sgr_any"
    (match T.mouse_mode term with
    | `Off -> "Off"
    | `X10 -> "X10"
    | `Normal -> "Normal"
    | `Button -> "Button"
    | `Any -> "Any"
    | `Sgr_normal -> "Sgr_normal"
    | `Sgr_button -> "Sgr_button"
    | `Sgr_any -> "Sgr_any");
  T.close term

(* Test: set_unicode_width *)
let test_set_unicode_width () =
  with_terminal @@ fun term _buf ->
  T.set_unicode_width term `Unicode;
  let caps = T.capabilities term in
  let new_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in
  equal ~msg:"unicode width changed" string "Unicode" new_width;

  T.set_unicode_width term `Wcwidth;
  let caps = T.capabilities term in
  let final_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in
  equal ~msg:"unicode width changed again" string "Wcwidth" final_width;
  T.close term

(* Test: modifyOtherKeys toggling on non-TTY updates state *)
let test_modify_other_keys_toggle () =
  with_terminal @@ fun term _buf ->
  is_false ~msg:"initial MOK disabled" (T.modify_other_keys_enabled term);
  T.enable_modify_other_keys term true;
  is_true ~msg:"MOK enabled" (T.modify_other_keys_enabled term);
  T.enable_modify_other_keys term false;
  is_false ~msg:"MOK disabled" (T.modify_other_keys_enabled term);
  T.close term

(* Test: Protocol state is idempotent *)
let test_idempotent_protocols () =
  with_terminal @@ fun term _buf ->
  T.enable_bracketed_paste term true;
  is_true ~msg:"paste on" (T.bracketed_paste_enabled term);
  T.enable_bracketed_paste term true;
  is_true ~msg:"paste still on" (T.bracketed_paste_enabled term);
  T.enable_bracketed_paste term false;
  is_false ~msg:"paste off" (T.bracketed_paste_enabled term);

  T.enable_focus_reporting term true;
  is_true ~msg:"focus on" (T.focus_reporting_enabled term);
  T.enable_focus_reporting term false;
  is_false ~msg:"focus off" (T.focus_reporting_enabled term);

  T.enable_kitty_keyboard term true;
  is_true ~msg:"kitty on" (T.kitty_keyboard_enabled term);
  T.enable_kitty_keyboard term false;
  is_false ~msg:"kitty off" (T.kitty_keyboard_enabled term);

  T.enter_alternate_screen term;
  is_true ~msg:"alt on" (T.alt_screen term);
  T.enter_alternate_screen term;
  is_true ~msg:"alt still on" (T.alt_screen term);
  T.leave_alternate_screen term;
  is_false ~msg:"alt off" (T.alt_screen term);

  T.close term

(* Test: reset_state unwinds all protocols *)
let test_reset_state () =
  with_terminal @@ fun term _buf ->
  T.enable_bracketed_paste term true;
  T.enable_focus_reporting term true;
  T.enable_kitty_keyboard term true;
  T.enable_modify_other_keys term true;
  T.set_mouse_mode term `Sgr_any;
  T.enter_alternate_screen term;
  T.set_unicode_width term `Unicode;

  T.reset_state term;

  is_true ~msg:"cursor visible after reset" (T.cursor_visible term);
  is_false ~msg:"alt screen off after reset" (T.alt_screen term);
  equal ~msg:"mouse off after reset" string "Off"
    (match T.mouse_mode term with `Off -> "Off" | _ -> "Other");
  T.close term

let test_reset_state_disables_partially_enabled_paste () =
  let output = Buffer.create 256 in
  let fail_on_paste_on = ref true in
  let term =
    T.make ~tty:true
      ~output:(fun s ->
        Buffer.add_string output s;
        if !fail_on_paste_on && contains s "\027[?2004h" then (
          fail_on_paste_on := false;
          failwith "paste enable failed after write"))
      ()
  in
  let raised =
    try
      T.enable_bracketed_paste term true;
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  is_true ~msg:"enable raised" raised;
  is_false ~msg:"steady state still off" (T.bracketed_paste_enabled term);
  T.reset_state term;
  is_true ~msg:"reset disables bracketed paste after partial enable"
    (contains_substring (Buffer.contents output) "\027[?2004l")

let test_reset_state_resets_cursor_metadata () =
  with_terminal @@ fun term _buf ->
  T.set_cursor_visible term false;
  T.set_cursor_style term `Line ~blinking:true;
  T.set_cursor_color term ~r:0.1 ~g:0.2 ~b:0.3 ~a:0.4;
  T.reset_state term;
  let style, blinking = T.cursor_style_state term in
  equal ~msg:"cursor style reset" string "Block"
    (match style with
    | `Block -> "Block"
    | `Line -> "Line"
    | `Underline -> "Underline");
  is_false ~msg:"cursor blinking reset" blinking;
  equal ~msg:"cursor color reset" (float 0.01) 1.0
    (let r, _, _, _ = T.cursor_color term in
     r);
  T.close term

let () =
  run "Terminal"
    [
      group "non-tty"
        [
          test "no escape sequences on non-TTY" test_non_tty_no_escape_sequences;
          test "state tracking on non-TTY" test_non_tty_state_tracking;
          test "tty send" test_tty_send;
        ];
      group "helpers" [ test "contains_substring" test_contains_substring ];
      group "capabilities"
        [
          test "normalization" test_capability_normalization;
          test "environment overrides" test_env_overrides;
          test "terminal info env" test_terminal_info_from_env;
          test "make with caps" test_make_with_caps;
          test "kitty keyboard level zero"
            test_kitty_keyboard_level_zero_capability;
          test "probe color scheme mode" test_probe_payload_color_scheme_mode;
          test "screen probe not tmux wrapped"
            test_probe_payload_screen_is_not_tmux_wrapped;
          test "tmux probe wrapped" test_probe_payload_tmux_is_wrapped;
          test "XTVersion tmux resends pending queries wrapped"
            test_probe_xtversion_tmux_resends_pending_queries_wrapped;
          test "XTVersion non-tmux does not resend wrapped"
            test_probe_xtversion_non_tmux_does_not_resend_wrapped;
          test "probe preserves user input and capabilities"
            test_probe_preserves_user_input_and_capabilities;
          test "explicit cursor positioning env"
            test_explicit_cursor_positioning_env_overrides;
        ];
      group "cursor"
        [
          test "position clamping" test_cursor_position_clamping;
          test "color clamping" test_color_clamping;
        ];
      group "mouse"
        [
          test "mouse mode on non-TTY" test_mouse_validation;
          test "X10 disable" test_x10_mouse_disable;
        ];
      group "unicode" [ test "set_unicode_width" test_set_unicode_width ];
      group "keyboard"
        [ test "modifyOtherKeys toggle" test_modify_other_keys_toggle ];
      group "protocols"
        [
          test "idempotent protocols" test_idempotent_protocols;
          test "reset state" test_reset_state;
          test "reset state disables partial paste"
            test_reset_state_disables_partially_enabled_paste;
          test "reset state resets cursor metadata"
            test_reset_state_resets_cursor_metadata;
        ];
    ]
