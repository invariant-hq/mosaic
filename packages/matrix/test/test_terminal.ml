module Input = Matrix_input
open Windtrap
module T = Matrix_terminal

(* A predicate, not an assertion: this one feeds a branch inside a fake
   terminal's output callback, where windtrap's [contains] cannot go. *)
let has_substring s needle =
  let n = String.length needle in
  let rec loop i =
    if i + n > String.length s then false
    else if String.sub s i n = needle then true
    else loop (i + 1)
  in
  n = 0 || loop 0

(* Helper to create a non-TTY terminal with a buffer-backed output *)
module Caps = T.Caps
module Reply = Input.Reply

(* The environment, as a value: tests never read the process's. *)
let env ?(win32 = false) vars =
  Caps.env (fun name -> List.assoc_opt name vars) ~win32

let plain = Caps.derive (Caps.evidence (env []))

let with_terminal ?(caps = plain) f =
  let buf = Buffer.create 4096 in
  let output s = Buffer.add_string buf s in
  let term = T.make ~output ~tty:false caps in
  f term buf

let with_tty_terminal ?(caps = plain) f =
  let buf = Buffer.create 4096 in
  let output s = Buffer.add_string buf s in
  let term = T.make ~output ~tty:true caps in
  f term buf

let width_method =
  Testable.make ~equal:( = ) ~pp:(fun fmt -> function
    | `Unicode -> Format.pp_print_string fmt "Unicode"
    | `Unicode_wide -> Format.pp_print_string fmt "Unicode_wide"
    | `Wcwidth -> Format.pp_print_string fmt "Wcwidth")

(* Test: Non-TTY terminal doesn't send escape sequences *)
let test_non_tty_no_escape_sequences () =
  with_terminal @@ fun term buf ->
  T.set_cursor_visible term false;
  T.set_mouse_mode term `Sgr_any;
  T.enable_bracketed_paste term true;
  T.enable_focus_reporting term true;
  T.enter_alternate_screen term;
  T.set_title term "Test";
  T.set_cursor_style term `Line ~blinking:true;
  T.set_cursor_color term ~r:1. ~g:0. ~b:0. ~a:1.;
  T.reset_cursor_color term;

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

(* Test: Capability normalization *)
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

(* Test: TTY terminal emits escape sequences via send *)
let test_tty_send () =
  with_tty_terminal @@ fun term buf ->
  T.send term "Hello";
  T.send term " World";
  let output_data = Buffer.contents buf in
  equal ~msg:"send output" string "Hello World" output_data;
  T.close term

let test_x10_mouse_disable () =
  with_tty_terminal @@ fun term buf ->
  T.set_mouse_mode term `X10;
  T.set_mouse_mode term `Off;
  let output_data = Buffer.contents buf in
  contains ~msg:"X10 enabled" ~sub:"\027[?9h" output_data;
  contains ~msg:"X10 disabled" ~sub:"\027[?9l" output_data;
  T.close term

(* Test: Mouse mode validation on non-TTY *)
let test_mouse_validation () =
  with_terminal @@ fun term _buf ->
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

(* The grapheme mode is set and reset as asked; the capabilities are the
   runtime's to set. *)
let test_enable_grapheme_mode () =
  with_tty_terminal ~caps:{ plain with unicode_width = `Unicode }
  @@ fun term buf ->
  T.enable_grapheme_mode term true;
  T.enable_grapheme_mode term true;
  equal ~msg:"set once" string "\027[?2027h" (Buffer.contents buf);
  T.enable_grapheme_mode term false;
  equal ~msg:"reset" string "\027[?2027h\027[?2027l" (Buffer.contents buf);
  equal ~msg:"capabilities untouched" width_method `Unicode
    (T.capabilities term).unicode_width;
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

(* Regression: changing kitty keyboard flags while enabled must pop the
   previous stack entry before pushing the new one, so close leaves the
   terminal's keyboard stack balanced. *)
let test_kitty_flag_change_keeps_stack_balanced () =
  let count needle haystack =
    let n = String.length needle in
    let rec loop i acc =
      if i + n > String.length haystack then acc
      else if String.sub haystack i n = needle then loop (i + n) (acc + 1)
      else loop (i + 1) acc
    in
    if n = 0 then 0 else loop 0 0
  in
  with_tty_terminal @@ fun term buf ->
  T.enable_kitty_keyboard ~flags:1 term true;
  T.enable_kitty_keyboard ~flags:5 term true;
  T.enable_kitty_keyboard term false;
  T.close term;
  let out = Buffer.contents buf in
  equal ~msg:"first flags pushed once" int 1 (count "\027[>1u" out);
  equal ~msg:"second flags pushed once" int 1 (count "\027[>5u" out);
  equal ~msg:"pops balance pushes" int
    (count "\027[>1u" out + count "\027[>5u" out)
    (count "\027[<u" out);
  contains ~msg:"flag change pops before re-pushing"
    ~sub:"\027[>1u\027[<u\027[>5u" out

(* Test: reset_state unwinds all protocols *)
let test_reset_state () =
  with_terminal @@ fun term _buf ->
  T.enable_bracketed_paste term true;
  T.enable_focus_reporting term true;
  T.enable_kitty_keyboard term true;
  T.enable_modify_other_keys term true;
  T.set_mouse_mode term `Sgr_any;
  T.enter_alternate_screen term;
  T.enable_grapheme_mode term true;

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
        if !fail_on_paste_on && has_substring s "\027[?2004h" then (
          fail_on_paste_on := false;
          failwith "paste enable failed after write"))
      plain
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
  contains ~msg:"reset disables bracketed paste after partial enable"
    ~sub:"\027[?2004l" (Buffer.contents output)

(* A mode whose control function reached [output] is put back, though
   [output] raised: the terminal may have taken it. *)
let test_reset_state_undoes_partial_modes () =
  List.iter
    (fun (name, set, on, off) ->
      let output = Buffer.create 64 in
      let failing = ref true in
      let term =
        T.make ~tty:true
          ~output:(fun s ->
            Buffer.add_string output s;
            if !failing && has_substring s on then (
              failing := false;
              failwith "cut"))
          plain
      in
      (match set term with
      | () -> failf "%s: output did not raise" name
      | exception Failure _ -> ());
      Buffer.clear output;
      T.reset_state term;
      contains ~msg:name ~sub:off (Buffer.contents output))
    [
      ( "the alternate screen",
        T.enter_alternate_screen,
        "\027[?1049h",
        "\027[?1049l" );
      ( "grapheme cluster mode",
        (fun t -> T.enable_grapheme_mode t true),
        "\027[?2027h",
        "\027[?2027l" );
      ( "the scrolling region",
        (fun t -> T.set_scroll_region t ~top:2 ~bottom:5),
        "\027[2;5r",
        "\027[r" );
    ]

(* Regression: reset_state must not clobber terminal state the app never
   touched — blanking the title or resetting cursor colour/style clobbers
   user- or shell-configured state on exit. *)
let test_reset_state_skips_untouched_appearance () =
  with_tty_terminal @@ fun term buf ->
  T.reset_state term;
  let out = Buffer.contents buf in
  not_contains ~msg:"untouched title is not cleared" ~sub:"\027]0;" out;
  not_contains ~msg:"untouched cursor color is not reset" ~sub:"\027]112" out;
  not_contains ~msg:"untouched cursor color fallback is not sent"
    ~sub:"\027]12;" out;
  not_contains ~msg:"untouched cursor style is not reset" ~sub:"\027[0 q" out

let test_reset_state_restores_touched_appearance () =
  with_tty_terminal @@ fun term buf ->
  T.set_title term "matrix";
  T.set_cursor_style term `Line ~blinking:false;
  Buffer.clear buf;
  T.reset_state term;
  let out = Buffer.contents buf in
  contains ~msg:"touched title is cleared" ~sub:"\027]0;" out;
  contains ~msg:"touched cursor style is reset" ~sub:"\027[0 q" out;
  not_contains ~msg:"untouched cursor color is not reset" ~sub:"\027]112" out

(* Each of the cursor's style and color is written alone: setting one
   leaves the other as the terminal has it. *)
let test_cursor_style_and_color_alone () =
  with_tty_terminal @@ fun term buf ->
  T.set_cursor_style term `Line ~blinking:false;
  equal ~msg:"the style alone" string "\027[6 q" (Buffer.contents buf);
  Buffer.clear buf;
  T.set_cursor_color term ~r:1. ~g:0. ~b:0. ~a:1.;
  let out = Buffer.contents buf in
  contains ~msg:"the color" ~sub:"\027]12;" out;
  not_contains ~msg:"the color alone" ~sub:" q" out

(* A change made while the cursor is hidden reaches the terminal, which
   shows it with the cursor, and is put back. *)
let test_hidden_cursor_change_reaches_the_terminal () =
  with_tty_terminal @@ fun term buf ->
  T.set_cursor_visible term false;
  Buffer.clear buf;
  T.set_cursor_style term `Line ~blinking:true;
  T.set_cursor_color term ~r:1. ~g:0. ~b:0. ~a:1.;
  T.set_cursor_visible term true;
  let out = Buffer.contents buf in
  contains ~msg:"the style" ~sub:"\027[5 q" out;
  contains ~msg:"the color" ~sub:"\027]12;" out;
  Buffer.clear buf;
  T.reset_state term;
  let out = Buffer.contents buf in
  contains ~msg:"the style is put back" ~sub:"\027[0 q" out;
  contains ~msg:"the color is put back" ~sub:"\027]112" out

(* The title the handle replaced is pushed on the terminal's title stack
   with the first title set, once, and popped by reset_state. *)
let test_title_comes_back () =
  with_tty_terminal @@ fun term buf ->
  T.set_title term "one";
  T.set_title term "two";
  equal ~msg:"pushed once, before the first title" string
    "\027[22;0t\027]0;one\027\\\027]0;two\027\\" (Buffer.contents buf);
  Buffer.clear buf;
  T.reset_state term;
  let out = Buffer.contents buf in
  contains ~msg:"popped" ~sub:"\027]0;\027\\\027[23;0t" out;
  Buffer.clear buf;
  T.set_title term "three";
  contains ~msg:"pushed again after a reset" ~sub:"\027[22;0t"
    (Buffer.contents buf)

(* On the primary screen the cells are the shell's as much as the
   program's: reset_state erases none of them, wherever the cursor is. *)
let test_reset_state_erases_nothing () =
  with_tty_terminal @@ fun term buf ->
  T.move_cursor term ~row:5 ~col:1;
  Buffer.clear buf;
  T.reset_state term;
  let out = Buffer.contents buf in
  not_contains ~msg:"no erase" ~sub:"\027[J" out;
  not_contains ~msg:"no move up" ~sub:"\027[A" out

let test_cursor_batch_matches_immediate_output () =
  with_tty_terminal @@ fun term output ->
  T.set_cursor_style term `Line ~blinking:true;
  T.set_cursor_color term ~r:2.5 ~g:Float.nan ~b:0.5 ~a:0.25;
  let writer = Ansi.Writer.create 128 in
  T.write_cursor_style term ~shape:`Blinking_bar writer;
  T.write_cursor_color term (Some (255, 0, 128)) writer;
  equal ~msg:"immediate and batched cursor output agree" string
    (Buffer.contents output)
    (Ansi.Writer.contents writer);
  Ansi.Writer.reset_pos writer;
  T.write_cursor_color term None writer;
  Buffer.clear output;
  T.reset_cursor_color term;
  equal ~msg:"both reset paths include the fallback" string
    "\027]12;default\007\027]112\007"
    (Ansi.Writer.contents writer);
  equal ~msg:"immediate and batched resets agree" string
    (Buffer.contents output)
    (Ansi.Writer.contents writer);
  Buffer.clear output;
  T.reset_state term;
  not_contains ~msg:"successful immediate reset clears restoration"
    ~sub:"\027]112" (Buffer.contents output)

let test_cursor_reset_survives_partial_output () =
  List.iter
    (fun batched ->
      List.iter
        (fun cut ->
          let output = Buffer.create 128 in
          let fail_output = ref false in
          let term =
            T.make ~tty:true
              ~output:(fun bytes ->
                if !fail_output then (
                  Buffer.add_substring output bytes 0
                    (min cut (String.length bytes));
                  failwith "partial cursor reset")
                else Buffer.add_string output bytes)
              plain
          in
          T.set_cursor_color term ~r:1. ~g:0. ~b:0. ~a:1.;
          fail_output := true;
          raises_match
            (function
              | Failure message -> String.equal message "partial cursor reset"
              | _ -> false)
            (fun () ->
              if batched then (
                let writer = Ansi.Writer.create 128 in
                T.write_cursor_color term None writer;
                T.send term (Ansi.Writer.contents writer))
              else T.reset_cursor_color term);
          fail_output := false;
          Buffer.clear output;
          T.reset_state term;
          contains ~msg:"cleanup retries a reset whose output failed"
            ~sub:"\027]12;default\007\027]112\007" (Buffer.contents output))
        [ 0; 7; 14 ])
    [ false; true ]

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

(* The route follows the capabilities, derived from the environment and
   tmux's own XTVERSION, never from the process's environment. *)
let clipboard_routes =
  [
    ("direct", [ ("TERM", "xterm") ], [], "\027]52;c;dGVzdA==\007");
    ( "tmux TERM",
      [ ("TERM", "tmux-256color") ],
      [],
      "\027Ptmux;\027\027]52;c;dGVzdA==\007\027\\" );
    ( "tmux environment",
      [ ("TERM", "screen-256color"); ("TMUX", "session"); ("STY", "screen") ],
      [],
      "\027Ptmux;\027\027]52;c;dGVzdA==\007\027\\" );
    ( "tmux identity",
      [ ("TERM", "xterm"); ("STY", "screen") ],
      [ Reply.Version "tmux 3.5a" ],
      "\027Ptmux;\027\027]52;c;dGVzdA==\007\027\\" );
    ( "Screen TERM",
      [ ("TERM", "screen-256color") ],
      [],
      "\027P\027]52;c;dGVzdA==\007\027\\" );
    ( "Screen environment",
      [ ("TERM", "xterm"); ("STY", "screen") ],
      [],
      "\027P\027]52;c;dGVzdA==\007\027\\" );
    ( "tmux variable, another terminal answering",
      [ ("TERM", "xterm"); ("TMUX", "session") ],
      [ Reply.Version "XTerm(398)" ],
      "\027]52;c;dGVzdA==\007" );
  ]

let test_clipboard_route (_, vars, replies, expected) =
  let caps =
    Caps.derive (List.fold_left Caps.add (Caps.evidence (env vars)) replies)
  in
  with_tty_terminal ~caps @@ fun term buf ->
  T.copy_to_clipboard term "test";
  equal string expected (Buffer.contents buf);
  T.close term

let test_screen_clipboard_chunks () =
  let caps =
    Caps.derive (Caps.evidence (env [ ("TERM", "screen-256color") ]))
  in
  with_tty_terminal ~caps @@ fun term buf ->
  List.iter
    (fun size ->
      Buffer.clear buf;
      let payload = String.make size 'A' in
      T.copy_to_clipboard term payload;
      let output = Buffer.contents buf in
      let decoded = Buffer.create (String.length output) in
      let offset = ref 0 in
      let chunks = ref 0 in
      while !offset < String.length output do
        equal ~msg:"DCS prefix" string "\027P" (String.sub output !offset 2);
        let start = !offset + 2 in
        let finish = String.index_from output start '\027' in
        (* Only the first payload has an ESC: the OSC introducer. *)
        let finish =
          if finish = start then String.index_from output (start + 1) '\027'
          else finish
        in
        equal ~msg:"DCS terminator" string "\027\\" (String.sub output finish 2);
        let count = finish - start in
        is_true ~msg:"bounded nonempty payload" (count > 0 && count <= 252);
        if finish + 2 < String.length output then
          equal ~msg:"full intermediate chunk" int 252 count;
        Buffer.add_substring decoded output start count;
        incr chunks;
        offset := finish + 2
      done;
      let expected = Ansi.(to_string (set_clipboard ~text:payload)) in
      equal ~msg:"one intact OSC 52 across envelopes" string expected
        (Buffer.contents decoded);
      equal ~msg:"chunk count" int
        ((String.length expected + 251) / 252)
        !chunks)
    [ 0; 1; 182; 183; 184; 185; 372; 4096 ];
  T.close term

let test_non_tty_clipboard () =
  let caps = Caps.derive (Caps.evidence (env [ ("TERM", "screen") ])) in
  with_terminal ~caps @@ fun term buf ->
  T.copy_to_clipboard term "test";
  equal string "" (Buffer.contents buf);
  T.close term

(* Capability derivation, a pure function of the environment and the answers.
   Each case gives environment variables, replies in order, and the facts
   the derivation must give. *)

type fact =
  | Width of [ `Unicode | `Unicode_wide | `Wcwidth ]
  | Sync of bool
  | Paste of bool
  | Focus of bool
  | Graphics of bool
  | Keyboard of int option
  | Multiplexer of [ `Tmux | `Screen ] option
  | Identity of (string * string) option
  | Colors of [ `Ansi16 | `Ansi256 | `Truecolor ]
  | Dumb of bool
  | Hyperlinks of bool
  | Meta of bool option
  | Grapheme of bool option
  | Repositions of bool

let check_fact (c : Caps.t) fact =
  let opt pp =
    Format.pp_print_option ~none:(fun ppf () -> Format.fprintf ppf "none") pp
  in
  match fact with
  | Width w -> equal ~msg:"width method" width_method w c.unicode_width
  | Sync b -> equal ~msg:"sync" bool b c.sync
  | Paste b -> equal ~msg:"bracketed paste" bool b c.bracketed_paste
  | Focus b -> equal ~msg:"focus tracking" bool b c.focus_tracking
  | Graphics b -> equal ~msg:"kitty graphics" bool b c.kitty_graphics
  | Keyboard k ->
      equal ~msg:"kitty keyboard"
        (Testable.make ~equal:( = ) ~pp:(opt Format.pp_print_int))
        k c.kitty_keyboard
  | Multiplexer m ->
      equal ~msg:"multiplexer"
        (Testable.make ~equal:( = )
           ~pp:
             (opt (fun ppf -> function
               | `Tmux -> Format.fprintf ppf "tmux"
               | `Screen -> Format.fprintf ppf "screen")))
        m c.multiplexer
  | Identity i ->
      equal ~msg:"identity"
        (Testable.make ~equal:( = )
           ~pp:(opt (fun ppf (n, v) -> Format.fprintf ppf "%s %s" n v)))
        i c.identity
  | Dumb b -> equal ~msg:"dumb" bool b c.dumb
  | Colors n ->
      equal ~msg:"colors"
        (Testable.make ~equal:( = ) ~pp:(fun ppf -> function
          | `Ansi16 -> Format.fprintf ppf "16"
          | `Ansi256 -> Format.fprintf ppf "256"
          | `Truecolor -> Format.fprintf ppf "24-bit"))
        n c.colors
  | Hyperlinks b -> equal ~msg:"hyperlinks" bool b c.hyperlinks
  | Meta m ->
      equal ~msg:"meta sends escape"
        (Testable.make ~equal:( = ) ~pp:(opt Format.pp_print_bool))
        m c.meta_sends_escape
  | Grapheme m ->
      equal ~msg:"grapheme mode"
        (Testable.make ~equal:( = ) ~pp:(opt Format.pp_print_bool))
        m c.grapheme_mode
  | Repositions b ->
      equal ~msg:"explicit cursor positioning" bool b
        c.explicit_cursor_positioning

let derive vars replies =
  Caps.derive (List.fold_left Caps.add (Caps.evidence (env vars)) replies)

let test_derivation (_, vars, replies, facts) =
  List.iter (check_fact (derive vars replies)) facts

let mode mode setting = Reply.Mode { dec = true; mode; setting }

(* The description of TERM, from the ncurses database (data/terminfo). *)
let description name =
  let bytes =
    In_channel.with_open_bin ("data/terminfo/" ^ name) In_channel.input_all
  in
  Result.get_ok (Matrix_terminfo.of_string bytes)

let described name vars replies =
  let env =
    Caps.env ~terminfo:(description name)
      (fun v -> List.assoc_opt v vars)
      ~win32:false
  in
  Caps.derive (List.fold_left Caps.add (Caps.evidence env) replies)

(* A description says yes: matrix-test declares Tc, BE, fe and Sync. *)
let test_description_adds () =
  let c = described "m/matrix-test" [ ("TERM", "matrix-test") ] [] in
  is_true ~msg:"direct colour from Tc" (c.colors = `Truecolor);
  is_true ~msg:"bracketed paste from BE" c.bracketed_paste;
  is_true ~msg:"focus reports from fe" c.focus_tracking;
  is_true ~msg:"synchronized output from Sync" c.sync;
  let c = described "x/xterm-direct" [ ("TERM", "xterm-direct") ] [] in
  is_true ~msg:"direct colour from RGB" (c.colors = `Truecolor);
  let c = described "x/xterm-256color" [ ("TERM", "xterm") ] [] in
  is_true ~msg:"256 colours from colors#256" (c.colors = `Ansi256);
  let c = described "x/xterm" [ ("TERM", "xterm") ] [] in
  is_true ~msg:"16 colours from colors#8" (c.colors = `Ansi16);
  let c = derive [ ("TERM", "matrix-test") ] [] in
  is_false ~msg:"no description, no direct colour" (c.colors = `Truecolor);
  is_false ~msg:"no description, no sync" c.sync

(* A terminal that wraps as soon as it prints in the last column says so with
   am and no xenl, as the Wyse 350's description and dumb's do. *)
let test_description_eager_wrap () =
  let eager name =
    (described name [ ("TERM", Filename.basename name) ] []).eager_wrap
  in
  is_true ~msg:"wy350" (eager "w/wy350");
  is_true ~msg:"dumb" (eager "d/dumb");
  is_false ~msg:"xterm" (eager "x/xterm");
  is_false ~msg:"linux" (eager "l/linux");
  is_false ~msg:"no description" (derive [ ("TERM", "wy350") ] []).eager_wrap;
  let console =
    Caps.derive
      (Caps.evidence
         (Caps.env ~terminfo:(description "w/wy350")
            (fun v -> List.assoc_opt v [ ("TERM", "wy350") ])
            ~win32:true))
  in
  is_false ~msg:"a Windows console defers its wrap" console.eager_wrap;
  is_false ~msg:"a Windows console has no TERM and is not dumb"
    (Caps.derive (Caps.evidence (env ~win32:true []))).dumb

(* Its silence takes nothing away: xterm-256color has no RGB. *)
let test_description_silence () =
  let c =
    described "x/xterm-256color"
      [ ("TERM", "xterm-256color"); ("COLORTERM", "truecolor") ]
      []
  in
  is_true ~msg:"COLORTERM's direct colour" (c.colors = `Truecolor)

(* A guess: the terminal's answer and its identity come first. *)
let test_description_below () =
  let c =
    described "m/matrix-test"
      [ ("TERM", "matrix-test") ]
      [ mode 2004 Not_recognized; mode 2026 Permanently_reset ]
  in
  is_false ~msg:"no bracketed paste, answered" c.bracketed_paste;
  is_false ~msg:"no sync, answered" c.sync;
  is_true ~msg:"focus, unanswered" c.focus_tracking;
  let c =
    described "m/matrix-test"
      [ ("TERM", "matrix-test") ]
      [ Reply.Version "tmux 1.6" ]
  in
  is_false ~msg:"tmux 1.6's identity: no bracketed paste" c.bracketed_paste;
  is_false ~msg:"tmux 1.6's identity: no sync" c.sync

let precedence_cases =
  [
    ("nothing known", [], [], [ Width `Wcwidth; Sync false; Keyboard None ]);
    ( "user policy beats an answer",
      [ ("MATRIX_FORCE_WCWIDTH", "1") ],
      [ mode 2027 Set ],
      [ Width `Wcwidth ] );
    ( "forced Unicode beats forced wcwidth",
      [ ("MATRIX_FORCE_WCWIDTH", "1"); ("MATRIX_FORCE_UNICODE", "1") ],
      [],
      [ Width `Unicode ] );
    ( "a recording beats answers",
      [ ("VHS_RECORD", "1") ],
      [
        mode 2027 Set;
        Reply.Keyboard_flags 0;
        Reply.Graphics { id = Some 31337; message = "OK" };
      ],
      [ Width `Wcwidth; Keyboard None; Graphics false ] );
    ( "a correction beats an answer",
      [ ("TERM_PROGRAM", "ghostty"); ("TERM_PROGRAM_VERSION", "1.3.1") ],
      [ mode 2027 Reset ],
      [ Width `Unicode_wide ] );
    ( "an answer beats identity",
      [],
      [
        Reply.Version "kitty(0.35.2)";
        mode 2027 Permanently_reset;
        mode 2026 Not_recognized;
      ],
      [ Width `Wcwidth; Sync false; Identity (Some ("kitty", "0.35.2")) ] );
    ( "identity beats a guess",
      [ ("TERM", "screen-256color") ],
      [ Reply.Version "kitty(0.35.2)" ],
      [ Width `Unicode; Multiplexer None; Repositions false ] );
    ( "a guess fills what nothing answers",
      [ ("TERM", "xterm-kitty"); ("COLORTERM", "truecolor") ],
      [],
      [ Sync true; Hyperlinks true; Colors `Truecolor; Width `Unicode ] );
    ( "identity from the environment when directly attached",
      [ ("TERM_PROGRAM", "Alacritty"); ("TERM_PROGRAM_VERSION", "0.13") ],
      [],
      [ Identity (Some ("Alacritty", "0.13")); Repositions true; Sync true ] );
    ( "no identity from the environment through ssh",
      [ ("TERM_PROGRAM", "Alacritty"); ("SSH_TTY", "/dev/pts/3") ],
      [],
      [ Identity None; Repositions false; Sync false ] );
    ( "XTVERSION holds through ssh",
      [ ("SSH_CONNECTION", "remote") ],
      [ Reply.Version "ghostty 1.3.1" ],
      [ Width `Unicode_wide; Identity (Some ("ghostty", "1.3.1")) ] );
    ( "the kitty keyboard flags reported",
      [],
      [ Reply.Keyboard_flags 0; Reply.Keyboard_flags 5 ],
      [ Keyboard (Some 5) ] );
    ("not a Windows console", [], [], [ Colors `Ansi16; Paste false ]);
    ("TERM unset is dumb", [], [], [ Dumb true ]);
    ("TERM dumb", [ ("TERM", "dumb") ], [], [ Dumb true ]);
    ("TERM unknown", [ ("TERM", "unknown") ], [], [ Dumb true ]);
    ("TERM empty is unset", [ ("TERM", "") ], [], [ Dumb true ]);
    ("a terminal type is not dumb", [ ("TERM", "vt100") ], [], [ Dumb false ]);
    ( "256 colors from TERM",
      [ ("TERM", "xterm-256color") ],
      [],
      [ Colors `Ansi256 ] );
    ( "24-bit color from a direct TERM",
      [ ("TERM", "foot-direct") ],
      [],
      [ Colors `Truecolor ] );
    ( "COLORTERM above TERM",
      [ ("TERM", "screen-256color"); ("COLORTERM", "24bit") ],
      [],
      [ Colors `Truecolor ] );
    ("metaSendsEscape reset", [], [ mode 1036 Reset ], [ Meta (Some false) ]);
    ( "metaSendsEscape set",
      [],
      [ mode 1036 Permanently_set ],
      [ Meta (Some true) ] );
    ("no metaSendsEscape", [], [ mode 1036 Not_recognized ], [ Meta None ]);
    ( "grapheme mode reset",
      [],
      [ mode 2027 Reset ],
      [ Grapheme (Some false); Width `Unicode ] );
    ( "grapheme mode set",
      [],
      [ mode 2027 Set ],
      [ Grapheme (Some true); Width `Unicode ] );
    ( "grapheme mode permanently set",
      [],
      [ mode 2027 Permanently_set ],
      [ Grapheme (Some true) ] );
    ("no grapheme mode", [], [ mode 2027 Not_recognized ], [ Grapheme None ]);
    ( "grapheme mode that cannot be set",
      [],
      [ mode 2027 Permanently_reset ],
      [ Grapheme None; Width `Wcwidth ] );
    ( "metaSendsEscape that cannot be set",
      [],
      [ mode 1036 Permanently_reset ],
      [ Meta None ] );
    ( "a mode permanently set is supported",
      [],
      [ mode 2004 Permanently_set; mode 1004 Permanently_reset ],
      [ Paste true; Focus false ] );
  ]

(* The width method from the answer for mode 2027, as DECRPM states it
   (VT510): 0 not recognized, 1 set, 2 reset, 3 permanently set, 4
   permanently reset. A terminal without the mode, or with it permanently
   reset, measures by character, unless its identity says it places clusters;
   one with the mode reset can set it, and the runtime does. The rows are
   terminals measured (CPR after each cluster), with the replies they gave
   to the probe; iTerm2's is read from its source (VT100Terminal.m). *)
let width_answer_cases =
  [
    ( "xterm 398: not recognized",
      [ ("TERM", "xterm-256color") ],
      [ mode 2027 Not_recognized; Reply.Version "XTerm(398)" ],
      `Wcwidth );
    ( "VTE 0.80.1: not recognized",
      [ ("TERM", "xterm-256color"); ("VTE_VERSION", "8001") ],
      [ mode 2027 Not_recognized; Reply.Version "VTE(8001)" ],
      `Wcwidth );
    ("st 0.9.2: no answer", [ ("TERM", "st-256color") ], [], `Wcwidth);
    ("not recognized, no identity", [], [ mode 2027 Not_recognized ], `Wcwidth);
    ( "kitty 0.49.2: not recognized, clusters",
      [ ("TERM", "xterm-kitty") ],
      [ mode 2027 Not_recognized; Reply.Version "kitty(0.49.2)" ],
      `Unicode );
    ( "kitty 0.41.1: not recognized, clusters",
      [],
      [ mode 2027 Not_recognized; Reply.Version "kitty(0.41.1)" ],
      `Unicode );
    ( "kitty from the environment, no answer",
      [ ("KITTY_WINDOW_ID", "1") ],
      [],
      `Unicode );
    ( "WezTerm 20240203: permanently set",
      [ ("TERM_PROGRAM", "WezTerm") ],
      [
        mode 2027 Permanently_set;
        Reply.Version "WezTerm 20240203-110809-5046fc22";
      ],
      `Unicode );
    ( "foot 1.21.0: set",
      [ ("TERM", "foot") ],
      [ mode 2027 Set; Reply.Version "foot(1.21.0)" ],
      `Unicode );
    ( "iTerm2: permanently reset",
      [ ("TERM_PROGRAM", "iTerm.app") ],
      [ mode 2027 Permanently_reset ],
      `Wcwidth );
    (* The terminal has the mode, off: the runtime sets it. *)
    ("reset", [], [ mode 2027 Reset ], `Unicode);
  ]

(* The capabilities once the probe's DA1 is answered after [replies]: what
   was not answered then never will be. *)
let test_width_answer (_, vars, replies, expected) =
  let ev = List.fold_left Caps.add (Caps.evidence (env vars)) replies in
  let caps = Caps.derive (Caps.answered ev Probe ~attributes:[ 62 ]) in
  equal ~msg:"width method" width_method expected caps.unicode_width

(* Ghostty widens graphemes from 1.3, whatever it answers. Before 1.3, or
   when the environment does not name the terminal, its answer for mode 2027
   decides, and with none the width method is the default. *)
let ghostty_width_cases =
  let ghostty version extra =
    extra
    @ [
        ("TERM", "xterm-ghostty");
        ("TERM_PROGRAM", "ghostty");
        ("TERM_PROGRAM_VERSION", version);
      ]
  in
  List.map
    (fun (name, version, extra, expected) ->
      (name, ghostty version extra, [], [ Width expected ]))
    [
      ("release", "1.3.1", [], `Unicode_wide);
      ("release boundary", "1.3.0", [], `Unicode_wide);
      ("old release", "1.2.3", [], `Wcwidth);
      ("prerelease", "1.3.0-dev", [], `Wcwidth);
      ("nightly boundary", "0.0.0-20260224.r14762.gc51f0d7", [], `Unicode_wide);
      ("old nightly", "0.0.0-20260223.r14707.gc61f184", [], `Wcwidth);
      ("malformed", "1.3", [], `Wcwidth);
      ("tmux", "1.3.1", [ ("TMUX", "/tmp/tmux") ], `Wcwidth);
      ("screen", "1.3.1", [ ("TERM", "screen-256color") ], `Wcwidth);
      ("screen environment", "1.3.1", [ ("STY", "session") ], `Wcwidth);
      ("zellij", "1.3.1", [ ("ZELLIJ", "0") ], `Wcwidth);
      ("ssh", "1.3.1", [ ("SSH_CONNECTION", "remote") ], `Wcwidth);
      ("forced wcwidth", "1.3.1", [ ("MATRIX_FORCE_WCWIDTH", "1") ], `Wcwidth);
      ("forced unicode", "1.3.1", [ ("MATRIX_FORCE_UNICODE", "1") ], `Unicode);
    ]

(* tmux answers no mode query before 3.6, so its facts come from its
   version: bracketed paste from 1.7, focus reports from 1.8. *)
let tmux_cases =
  let tmux version = [ Reply.Version ("tmux " ^ version) ] in
  [
    ( "3.5a",
      [],
      tmux "3.5a",
      [
        Width `Wcwidth;
        Sync false;
        Paste true;
        Focus true;
        Repositions true;
        Hyperlinks true;
        Multiplexer (Some `Tmux);
      ] );
    ("1.6", [], tmux "1.6", [ Paste false; Focus false ]);
    ("1.7", [], tmux "1.7", [ Paste true; Focus false ]);
    ("1.8", [], tmux "1.8", [ Paste true; Focus true ]);
    ("3.6", [], tmux "3.6", [ Sync false; Paste true; Focus true ]);
    ("3.7", [], tmux "3.7", [ Width `Wcwidth; Sync true ]);
    ("next-3.7", [], tmux "next-3.7", [ Sync true ]);
    ( "an answer beats the version",
      [],
      tmux "3.7" @ [ mode 2026 Not_recognized; mode 2027 Set ],
      [ Sync false; Width `Unicode ] );
    ( "the environment alone",
      [ ("TMUX", "/tmp/tmux-1000/default,1,0"); ("TERM", "tmux-256color") ],
      [],
      [ Width `Wcwidth; Sync false; Repositions true; Identity None ] );
  ]

(* Which multiplexer is in the way. An XTVERSION reply is the terminal that
   answers, which beats a [TMUX] variable the environment carried in. *)
let multiplexer_cases =
  [
    ("none", [ ("TERM", "xterm") ], [], [ Multiplexer None ]);
    ("TMUX", [ ("TMUX", "s") ], [], [ Multiplexer (Some `Tmux) ]);
    ("STY", [ ("STY", "s") ], [], [ Multiplexer (Some `Screen) ]);
    ( "tmux TERM",
      [ ("TERM", "tmux-256color") ],
      [],
      [ Multiplexer (Some `Tmux) ] );
    ( "screen TERM",
      [ ("TERM", "screen.xterm") ],
      [],
      [ Multiplexer (Some `Screen) ] );
    ( "tmux XTVERSION",
      [ ("TERM", "xterm-256color") ],
      [ Reply.Version "tmux 3.5a" ],
      [ Multiplexer (Some `Tmux) ] );
    ( "TMUX set, another terminal answering",
      [ ("TMUX", "s") ],
      [ Reply.Version "ghostty 1.2.3" ],
      [ Multiplexer None; Identity (Some ("ghostty", "1.2.3")) ] );
  ]

let caps_testable =
  Testable.make ~equal:( = ) ~pp:(fun ppf _ -> Format.fprintf ppf "<caps>")

let test_assume () =
  let kitty =
    {
      plain with
      kitty_keyboard = Some 5;
      identity = Some ("kitty", "0.35");
      colors = `Ansi16;
    }
  in
  let tmux =
    { plain with multiplexer = Some `Tmux; unicode_width = `Unicode }
  in
  List.iter
    (fun caps -> equal caps_testable caps (Caps.derive (Caps.assume caps)))
    [ plain; kitty; tmux ];
  let caps =
    Caps.derive (Caps.add (Caps.assume tmux) (mode 2027 Permanently_reset))
  in
  equal ~msg:"an answer still applies" width_method `Wcwidth caps.unicode_width

(* A guess gives sync, an answer takes it back, and a later answer gives it
   again. *)
let test_not_monotone () =
  let ev = Caps.evidence (env [ ("TERM", "xterm-kitty") ]) in
  is_true ~msg:"guess" (Caps.derive ev).sync;
  let ev = Caps.add ev (mode 2026 Not_recognized) in
  is_false ~msg:"answer" (Caps.derive ev).sync;
  let ev = Caps.add ev (mode 2026 Reset) in
  is_true ~msg:"later answer" (Caps.derive ev).sync

let owed =
  Testable.make ~equal:( = ) ~pp:(fun ppf (o : Input.Decoder.owed) ->
      Format.fprintf ppf "{cursor %d; strings %d}" o.cursor_reports
        o.string_replies)

let test_probe_payload () =
  let bytes, o = Caps.ask (Caps.evidence (env [ ("TERM", "xterm") ])) Probe in
  is_true ~msg:"starts with S7C1T" (String.starts_with ~prefix:"\027 F" bytes);
  is_true ~msg:"ends with DA1" (String.ends_with ~suffix:"\027[c" bytes);
  List.iter
    (fun sub -> contains ~sub bytes)
    [
      "\027[?2027$p";
      "\027[?2026$p";
      "\027[?2031$p";
      "\027[?1036$p";
      "\027[>0q";
      "\027[?u";
      "\027_Gi=31337";
      "\027[16t";
      "\027[?1;1;0S";
    ];
  not_contains ~msg:"no cursor report" ~sub:"\027[6n" bytes;
  not_contains ~msg:"nothing written on the screen" ~sub:"\027]66" bytes;
  not_contains ~msg:"one DA1" ~sub:"\027[c"
    (String.sub bytes 0 (String.length bytes - 3));
  not_contains ~msg:"nothing wrapped" ~sub:"\027Ptmux;" bytes;
  equal ~msg:"owed" owed { cursor_reports = 0; string_replies = 2 } o

(* The measure of text sizing starts, measures and ends in the first column
   of the cursor's row, and erases from there what it wrote. *)
let test_widths_payload () =
  let bytes, o = Caps.ask (Caps.evidence (env [])) Widths in
  equal ~msg:"bytes" string
    "\r\027]66;w=1; \027\\\027[6n\r\027]66;s=2; \027\\\027[6n\r\027[J" bytes;
  equal ~msg:"owed" owed { cursor_reports = 2; string_replies = 0 } o

(* Questions about drawing and keys go to the multiplexer itself, unwrapped,
   whatever the environment says; the kitty graphics query waits for
   passthrough. *)
let test_probe_payload_multiplexed () =
  List.iter
    (fun vars ->
      let bytes, o = Caps.ask (Caps.evidence (env vars)) Probe in
      not_contains ~msg:"nothing wrapped" ~sub:"\027Ptmux;" bytes;
      not_contains ~msg:"no graphics query" ~sub:"\027_G" bytes;
      contains ~msg:"rendering asked" ~sub:"\027[?2026$p" bytes;
      equal ~msg:"owed" owed { cursor_reports = 0; string_replies = 1 } o)
    [
      [ ("TMUX", "s") ];
      [ ("TERM", "tmux-256color") ];
      [ ("STY", "s") ];
      [ ("TERM", "screen") ];
    ]

let batch =
  Testable.make ~equal:( = ) ~pp:(fun ppf -> function
    | Caps.Probe -> Format.fprintf ppf "Probe"
    | Widths -> Format.fprintf ppf "Widths"
    | Passthrough -> Format.fprintf ppf "Passthrough"
    | Outer -> Format.fprintf ppf "Outer")

let next_cases =
  [
    ("no multiplexer", [ ("TERM", "xterm") ], [], None);
    ("TMUX", [ ("TMUX", "s") ], [], Some Caps.Passthrough);
    ( "tmux by XTVERSION",
      [ ("TERM", "xterm") ],
      [ Reply.Version "tmux 3.5a" ],
      Some Passthrough );
    ( "TMUX set, another terminal answering",
      [ ("TMUX", "s") ],
      [ Reply.Version "ghostty 1.2.3" ],
      None );
    ("screen", [ ("STY", "s") ], [], None);
  ]

let test_next (_, vars, replies, expected) =
  let ev = List.fold_left Caps.add (Caps.evidence (env vars)) replies in
  let ev = Caps.answered ev Probe ~attributes:[ 1; 2; 4 ] in
  equal (Testable.option batch) expected (Caps.next ev Probe)

(* Wrapped questions go only once passthrough has answered, each group closed
   by its own wrapped DA1. *)
let test_outer_groups () =
  let ev = Caps.evidence (env [ ("TMUX", "s") ]) in
  let ev = Caps.answered ev Probe ~attributes:[ 1; 2 ] in
  let bytes, o = Caps.ask ev Passthrough in
  equal ~msg:"passthrough is a wrapped DA1" string "\027Ptmux;\027\027[c\027\\"
    bytes;
  equal ~msg:"owes nothing" owed { cursor_reports = 0; string_replies = 0 } o;
  equal ~msg:"nothing before passthrough answers" (Testable.option batch) None
    (Caps.next ev Passthrough);
  let ev = Caps.answered ev Passthrough ~attributes:[ 64 ] in
  equal ~msg:"outer once it answers" (Testable.option batch) (Some Outer)
    (Caps.next ev Passthrough);
  let bytes, o = Caps.ask ev Outer in
  equal ~msg:"graphics, XTVERSION then DA1, each wrapped" string
    "\027Ptmux;\027\027_Gi=31337,s=1,v=1,a=q,t=d,f=24;AAAA\027\027\\\027\\\027Ptmux;\027\027[>0q\027\\\027Ptmux;\027\027[c\027\\"
    bytes;
  equal ~msg:"owes the graphics and XTVERSION replies" owed
    { cursor_reports = 0; string_replies = 2 }
    o;
  is_false ~msg:"the outer DA1 says nothing of sixel"
    (Caps.derive (Caps.answered ev Outer ~attributes:[ 4 ])).sixel;
  is_true ~msg:"tmux's own DA1 does"
    (Caps.derive
       (Caps.answered (Caps.evidence (env [])) Probe ~attributes:[ 4 ]))
      .sixel;
  equal ~msg:"no batch after outer" (Testable.option batch) None
    (Caps.next ev Outer)

(* The kitty graphics query is answered "no" by silence before its batch's
   DA1, which beats identity; other questions stay unknown. *)
let test_silence_before_da1 () =
  let kitty = Caps.evidence (env [ ("KITTY_WINDOW_ID", "1") ]) in
  is_true ~msg:"identity before the answer" (Caps.derive kitty).kitty_graphics;
  let silent = Caps.answered kitty Probe ~attributes:[] in
  is_false ~msg:"silence" (Caps.derive silent).kitty_graphics;
  let answered =
    Caps.answered
      (Caps.add kitty (Reply.Graphics { id = Some 31337; message = "OK" }))
      Probe ~attributes:[]
  in
  is_true ~msg:"an answer" (Caps.derive answered).kitty_graphics;
  equal ~msg:"no keyboard answer is no keyboard" (Testable.option int) None
    (Caps.derive silent).kitty_keyboard;
  let tmux =
    Caps.answered (Caps.evidence (env [ ("TMUX", "s") ])) Probe ~attributes:[]
  in
  let tmux =
    Caps.answered
      (Caps.answered tmux Passthrough ~attributes:[])
      Outer ~attributes:[]
  in
  is_false ~msg:"silence after the outer DA1" (Caps.derive tmux).kitty_graphics;
  let tmux =
    Caps.add tmux (Reply.Graphics { id = Some 31337; message = "OK" })
  in
  is_true ~msg:"a late answer" (Caps.derive tmux).kitty_graphics

(* The form of pictures: placeholders for kitty and Ghostty that answer the
   graphics query, sixel with enough registers and not on Konsole, half
   blocks otherwise, and the user's choice first. *)
let graphics_form =
  Testable.make ~equal:( = ) ~pp:(fun ppf g ->
      Format.pp_print_string ppf
        (match g with
        | `Kitty -> "Kitty"
        | `Sixel -> "Sixel"
        | `Blocks -> "Blocks"))

let ok = Reply.Graphics { id = Some 31337; message = "OK" }

let registers n =
  match Ansi.Parser.parse (Printf.sprintf "\027[?1;0;%dS" n) with
  | [ Ansi.Parser.Csi s ] -> Reply.Csi s
  | _ -> assert false

let graphics_cases =
  [
    ("kitty", [], [ Reply.Version "kitty(0.49.2)"; ok ], [ 62 ], `Kitty);
    ("kitty before 0.28", [], [ Reply.Version "kitty(0.27.1)"; ok ], [], `Blocks);
    ("ghostty", [], [ Reply.Version "ghostty 1.2.3"; ok ], [], `Kitty);
    ("wezterm", [], [ Reply.Version "WezTerm 2024"; ok ], [ 65; 4 ], `Sixel);
    ("xterm with 16 registers", [], [ registers 16 ], [ 63; 4 ], `Blocks);
    ("xterm with 256 registers", [], [ registers 256 ], [ 63; 4 ], `Sixel);
    ("konsole", [], [ Reply.Version "Konsole 24.08" ], [ 62; 1; 4 ], `Blocks);
    ("nothing", [], [], [ 61 ], `Blocks);
    ( "the user's choice",
      [ ("MATRIX_GRAPHICS", "sixel") ],
      [ Reply.Version "kitty(0.49.2)"; ok ],
      [],
      `Sixel );
  ]

let test_graphics (_, vars, replies, attributes, expected) =
  let ev = List.fold_left Caps.add (Caps.evidence (env vars)) replies in
  let ev = Caps.answered ev Probe ~attributes in
  equal graphics_form expected (Caps.derive ev).graphics

(* Inside tmux, the terminal outside answers through passthrough. *)
let test_graphics_tmux () =
  let ev = Caps.evidence (env [ ("TMUX", "s") ]) in
  let ev = Caps.add ev (Reply.Version "tmux 3.5a") in
  let ev = Caps.answered ev Probe ~attributes:[ 1; 2 ] in
  let ev = Caps.answered ev Passthrough ~attributes:[ 62 ] in
  let ev = List.fold_left Caps.add ev [ ok; Reply.Version "kitty(0.49.2)" ] in
  let caps = Caps.derive (Caps.answered ev Outer ~attributes:[ 62 ]) in
  equal ~msg:"placeholders" graphics_form `Kitty caps.graphics;
  equal ~msg:"tmux is still the multiplexer"
    (Testable.option (Testable.make ~equal:( = ) ~pp:(fun _ _ -> ())))
    (Some `Tmux) caps.multiplexer

(* The probe writes a space of width 1 and one of scale 2 at the start of the
   second row; the column the cursor went to says whether the terminal sized
   them, and a terminal of one row reports the first. Positions are from 0. *)
let probe_cursor_cases =
  [
    ("both", [ (1, 1); (1, 2) ], (true, true));
    ("explicit width only", [ (1, 1); (1, 0) ], (true, false));
    ("scaled implies explicit width", [ (1, 0); (1, 2) ], (true, true));
    ("wrong slots", [ (1, 2); (1, 1) ], (false, false));
    ("any row", [ (0, 1); (0, 2) ], (true, true));
    ("far columns", [ (1, 19); (1, 29) ], (false, false));
    ("none", [], (false, false));
  ]

let test_probe_cursor (_, reports, (explicit, scaled)) =
  let ev =
    List.fold_left
      (fun (i, ev) (row, col) ->
        (i + 1, Caps.cursor ev Widths ~index:i ~row ~col))
      (0, Caps.evidence (env []))
      reports
    |> snd
  in
  let caps = Caps.derive ev in
  equal ~msg:"explicit width" bool explicit caps.explicit_width;
  equal ~msg:"scaled text" bool scaled caps.scaled_text

(* A notification is one sequence, in the form the terminal shows: OSC 777
   for rxvt-unicode, iTerm2's OSC 9 for every other. *)
let test_notify_once () =
  let notified vars =
    let caps = Caps.derive (Caps.evidence (env vars)) in
    with_tty_terminal ~caps @@ fun term buf ->
    T.notify term ~title:"t" ~body:"b";
    Buffer.contents buf
  in
  equal ~msg:"unknown" string "\x1b]9;t: b\x07" (notified []);
  equal ~msg:"kitty" string "\x1b]9;t: b\x07"
    (notified [ ("TERM", "xterm-kitty") ]);
  equal ~msg:"rxvt-unicode" string "\x1b]777;notify;t;b\x07"
    (notified [ ("TERM", "rxvt-unicode-256color") ])

let () =
  run "Terminal"
    [
      group "non-tty"
        [
          test "no escape sequences on non-TTY" test_non_tty_no_escape_sequences;
          test "state tracking on non-TTY" test_non_tty_state_tracking;
          test "tty send" test_tty_send;
        ];
      group "clipboard"
        [
          cases "route" clipboard_routes
            ~name:(fun (name, _, _, _) -> name)
            test_clipboard_route;
          test "Screen chunk boundaries" test_screen_clipboard_chunks;
          test "non-TTY clipboard" test_non_tty_clipboard;
          test "a notification is one sequence" test_notify_once;
        ];
      group "capabilities"
        [
          test "a description adds" test_description_adds;
          test "a description's eager wrap" test_description_eager_wrap;
          test "a description's silence" test_description_silence;
          test "a description is a guess" test_description_below;
          cases "precedence" precedence_cases
            ~name:(fun (name, _, _, _) -> name)
            test_derivation;
          cases "width answers" width_answer_cases
            ~name:(fun (name, _, _, _) -> name)
            test_width_answer;
          cases "Ghostty width" ghostty_width_cases
            ~name:(fun (name, _, _, _) -> name)
            test_derivation;
          cases "tmux by version" tmux_cases
            ~name:(fun (name, _, _, _) -> name)
            test_derivation;
          cases "multiplexer" multiplexer_cases
            ~name:(fun (name, _, _, _) -> name)
            test_derivation;
          test "assumed capabilities derive to themselves" test_assume;
          test "derivation is not monotone" test_not_monotone;
          cases "graphics" graphics_cases
            ~name:(fun (name, _, _, _, _) -> name)
            test_graphics;
        ];
      group "batches"
        [
          test "probe" test_probe_payload;
          test "widths" test_widths_payload;
          test "probe inside a multiplexer" test_probe_payload_multiplexed;
          cases "follow-up" next_cases
            ~name:(fun (name, _, _, _) -> name)
            test_next;
          test "passthrough and outer groups" test_outer_groups;
          test "graphics through tmux" test_graphics_tmux;
          test "silence before DA1" test_silence_before_da1;
          cases "probe cursor reports" probe_cursor_cases
            ~name:(fun (name, _, _) -> name)
            test_probe_cursor;
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
      group "unicode" [ test "enable_grapheme_mode" test_enable_grapheme_mode ];
      group "keyboard"
        [ test "modifyOtherKeys toggle" test_modify_other_keys_toggle ];
      group "protocols"
        [
          test "idempotent protocols" test_idempotent_protocols;
          test "kitty flag change keeps stack balanced"
            test_kitty_flag_change_keeps_stack_balanced;
          test "reset state" test_reset_state;
          test "reset state disables partial paste"
            test_reset_state_disables_partially_enabled_paste;
          test "reset state undoes partial modes"
            test_reset_state_undoes_partial_modes;
          test "reset state skips untouched appearance"
            test_reset_state_skips_untouched_appearance;
          test "reset state restores touched appearance"
            test_reset_state_restores_touched_appearance;
          test "reset state erases nothing" test_reset_state_erases_nothing;
          test "the title comes back" test_title_comes_back;
          test "cursor batch matches immediate output"
            test_cursor_batch_matches_immediate_output;
          test "cursor reset survives partial output"
            test_cursor_reset_survives_partial_output;
          test "cursor style and color alone" test_cursor_style_and_color_alone;
          test "a change while the cursor is hidden"
            test_hidden_cursor_change_reaches_the_terminal;
          test "reset state resets cursor metadata"
            test_reset_state_resets_cursor_metadata;
        ];
    ]
