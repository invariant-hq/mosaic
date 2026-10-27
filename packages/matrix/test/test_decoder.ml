(** Tests for the input decoder.

    Each case feeds bytes, with silences ([idle]) where a read says so, and
    compares the events, printed with [Decoder.pp_event]. The groups follow
    [decoder.mli]: keys in every encoding, mouse reports, paste, replies,
    strings, silence and what is owed. Then come the regressions of the defects
    found in the previous parser, the attack cases its redesign was checked
    against, and the bytes a real xterm sends for a set of chords, which the
    encoder must send too. *)

module I = Matrix_input
module D = I.Decoder
module K = I.Keystroke
open Windtrap

(* Harness ----------------------------------------------------------------- *)

type step =
  | F of string  (** feed *)
  | Idle
  | Finish
  | Cut_paste
  | Expect of D.owed
  | Settle of D.owed

let owed ?(cursor = 0) ?(strings = 0) () =
  { D.cursor_reports = cursor; string_replies = strings }

let show e = Format.asprintf "%a" D.pp_event e

(* The events of [steps], printed, adjacent [Paste_data] joined. *)
let decode ?(owed = owed ()) steps =
  let d = D.create () in
  D.expect d owed;
  let out = ref [] in
  let emit = function
    | D.Paste_data s -> (
        match !out with
        | D.Paste_data t :: rest -> out := D.Paste_data (t ^ s) :: rest
        | _ -> out := D.Paste_data s :: !out)
    | e -> out := e :: !out
  in
  List.iter
    (function
      | F s -> D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) emit
      | Idle -> D.idle d emit
      | Finish -> D.finish d emit
      | Cut_paste -> D.cut_paste d emit
      | Expect o -> D.expect d o
      | Settle o -> D.settle d o)
    steps;
  List.rev_map show !out

(* The events of [s] fed whole, then a silence. *)
let dec ?owed s = decode ?owed [ F s; Idle ]
let events = list string

let check ?owed s expected =
  equal ~msg:(Printf.sprintf "%S" s) events expected (dec ?owed s)

let check_steps ?owed steps expected =
  equal events expected (decode ?owed steps)

(* A table of cases: bytes and the events they decode to. *)
let table ?owed name rows =
  test name (fun () -> List.iter (fun (s, e) -> check ?owed s e) rows)

let strings = owed ~strings:1 ()
let cursor = owed ~cursor:1 ()

(* Keys ------------------------------------------------------------------- *)

let legacy_keys =
  [
    table "text"
      [
        ("a", [ "Key 'a' text \"a\"" ]);
        ("A", [ "Key 'A' text \"A\"" ]);
        ("$", [ "Key '$' text \"$\"" ]);
        (" ", [ "Key U+0020 text \" \"" ]);
        ("\xc3\xa9", [ "Key '\xc3\xa9' text \"\\195\\169\"" ]);
        ("\xe6\x97\xa5", [ "Key '\xe6\x97\xa5' text \"\\230\\151\\165\"" ]);
        ( "\xf0\x9f\x98\x80",
          [ "Key '\xf0\x9f\x98\x80' text \"\\240\\159\\152\\128\"" ] );
      ];
    table "C0 controls are Ctrl and a key"
      [
        ("\x01", [ "Key Ctrl+'a'" ]);
        ("\x13", [ "Key Ctrl+'s'" ]);
        ("\x1a", [ "Key Ctrl+'z'" ]);
        ("\x00", [ "Key Ctrl+U+0020" ]);
        ("\x1c", [ "Key Ctrl+'\\'" ]);
        ("\x1d", [ "Key Ctrl+']'" ]);
        ("\x1e", [ "Key Ctrl+'^'" ]);
        ("\x1f", [ "Key Ctrl+'_'" ]);
      ];
    table "named controls"
      [
        ("\x09", [ "Key Tab" ]);
        ("\x0d", [ "Key Enter" ]);
        ("\x7f", [ "Key Backspace" ]);
        ("\x08", [ "Key Backspace" ]);
        ("\x0a", [ "Key Ctrl+'j'" ]);
        ("\x1b", [ "Key Escape" ]);
      ];
    table "ESC before a key is Alt"
      [
        ("\x1ba", [ "Key Alt+'a'" ]);
        ("\x1bA", [ "Key Alt+'A'" ]);
        ("\x1b!", [ "Key Alt+'!'" ]);
        ("\x1b ", [ "Key Alt+U+0020" ]);
        ("\x1b\x01", [ "Key Ctrl+Alt+'a'" ]);
        ("\x1b\x0d", [ "Key Alt+Enter" ]);
        ("\x1b\x09", [ "Key Alt+Tab" ]);
        ("\x1b\x7f", [ "Key Alt+Backspace" ]);
        ("\x1b\x08", [ "Key Alt+Backspace" ]);
        ("\x1b\x0a", [ "Key Ctrl+Alt+'j'" ]);
        ("\x1b\x1b", [ "Key Alt+Escape" ]);
        ("\x1b\xc3\xa9", [ "Key Alt+'\xc3\xa9'" ]);
        ("\x1b\xe6\x97\xa5", [ "Key Alt+'\xe6\x97\xa5'" ]);
        ("\x1bX", [ "Key Alt+'X'" ]);
        ("\x1b^", [ "Key Alt+'^'" ]);
      ];
    table "string introducers are Alt keys when no string is owed"
      [
        ("\x1bP", [ "Key Alt+'P'" ]);
        ("\x1b]", [ "Key Alt+']'" ]);
        ("\x1b_", [ "Key Alt+'_'" ]);
        ("\x1b]1", [ "Key Alt+']'"; "Key '1' text \"1\"" ]);
      ];
    table "a C1 code point types nothing"
      [ ("\xc2\x9b", [ "Key U+009B" ]); ("\xc2\x81", [ "Key U+0081" ]) ];
  ]

let xterm_keys =
  [
    table "cursor and editing keys"
      [
        ("\x1b[A", [ "Key Up" ]);
        ("\x1b[B", [ "Key Down" ]);
        ("\x1b[C", [ "Key Right" ]);
        ("\x1b[D", [ "Key Left" ]);
        ("\x1b[H", [ "Key Home" ]);
        ("\x1b[F", [ "Key End" ]);
        ("\x1b[E", [ "Key Begin keypad" ]);
        ("\x1b[Z", [ "Key Shift+Tab" ]);
        ("\x1b[1;5Z", [ "Key Ctrl+Shift+Tab" ]);
        ("\x1b[2~", [ "Key Insert" ]);
        ("\x1b[3~", [ "Key Delete" ]);
        ("\x1b[5~", [ "Key Page_up" ]);
        ("\x1b[6~", [ "Key Page_down" ]);
        ("\x1b[1~", [ "Key Home" ]);
        ("\x1b[4~", [ "Key End" ]);
        ("\x1b[7~", [ "Key Home" ]);
        ("\x1b[8~", [ "Key End" ]);
      ];
    table "modifier parameter"
      [
        ("\x1b[1;2A", [ "Key Shift+Up" ]);
        ("\x1b[1;3A", [ "Key Alt+Up" ]);
        ("\x1b[1;5A", [ "Key Ctrl+Up" ]);
        ("\x1b[1;9A", [ "Key Super+Up" ]);
        ("\x1b[1;17A", [ "Key Hyper+Up" ]);
        ("\x1b[1;33A", [ "Key Meta+Up" ]);
        ("\x1b[1;65A", [ "Key Up caps_lock" ]);
        ("\x1b[1;129A", [ "Key Up num_lock" ]);
        ( "\x1b[1;256A",
          [ "Key Ctrl+Alt+Shift+Super+Hyper+Meta+Up caps_lock num_lock" ] );
        ("\x1b[3;5~", [ "Key Ctrl+Delete" ]);
        ("\x1b[;5A", [ "Key Ctrl+Up" ]);
      ];
    table "function keys"
      [
        ("\x1bOP", [ "Key F 1" ]);
        ("\x1bOQ", [ "Key F 2" ]);
        ("\x1bOR", [ "Key F 3" ]);
        ("\x1bOS", [ "Key F 4" ]);
        ("\x1b[P", [ "Key F 1" ]);
        ("\x1b[1;2P", [ "Key Shift+F 1" ]);
        ("\x1b[1;5Q", [ "Key Ctrl+F 2" ]);
        ("\x1b[1;2R", [ "Key Shift+F 3" ]);
        ("\x1b[1;5S", [ "Key Ctrl+F 4" ]);
        ("\x1b[11~", [ "Key F 1" ]);
        ("\x1b[13~", [ "Key F 3" ]);
        ("\x1b[15~", [ "Key F 5" ]);
        ("\x1b[17~", [ "Key F 6" ]);
        ("\x1b[21~", [ "Key F 10" ]);
        ("\x1b[23~", [ "Key F 11" ]);
        ("\x1b[24;2~", [ "Key Shift+F 12" ]);
        ("\x1b[25~", [ "Key F 13" ]);
        ("\x1b[29~", [ "Key F 16" ]);
        ("\x1b[34~", [ "Key F 20" ]);
      ];
    table "SS3 forms"
      [
        ("\x1bOA", [ "Key Up" ]);
        ("\x1bOH", [ "Key Home" ]);
        ("\x1bOF", [ "Key End" ]);
        ("\x1bOE", [ "Key Begin keypad" ]);
        ("\x1bO2P", [ "Key Shift+F 1" ]);
        ("\x1bO1;2P", [ "Key Shift+F 1" ]);
        ("\x1bO5A", [ "Key Ctrl+Up" ]);
      ];
    table "application keypad"
      [
        ("\x1bOM", [ "Key Enter keypad" ]);
        ("\x1bOI", [ "Key Tab keypad" ]);
        ("\x1bO ", [ "Key U+0020 keypad text \" \"" ]);
        ("\x1bOX", [ "Key '=' keypad text \"=\"" ]);
        ("\x1bOj", [ "Key '*' keypad text \"*\"" ]);
        ("\x1bOk", [ "Key '+' keypad text \"+\"" ]);
        ("\x1bOl", [ "Key ',' keypad text \",\"" ]);
        ("\x1bOm", [ "Key '-' keypad text \"-\"" ]);
        ("\x1bOn", [ "Key '.' keypad text \".\"" ]);
        ("\x1bOo", [ "Key '/' keypad text \"/\"" ]);
        ("\x1bOp", [ "Key '0' keypad text \"0\"" ]);
        ("\x1bOy", [ "Key '9' keypad text \"9\"" ]);
      ];
    table "rxvt"
      [
        ("\x1b[a", [ "Key Shift+Up" ]);
        ("\x1b[b", [ "Key Shift+Down" ]);
        ("\x1b[c", [ "Key Shift+Right" ]);
        ("\x1b[d", [ "Key Shift+Left" ]);
        ("\x1b[e", [ "Key Shift+Begin keypad" ]);
        ("\x1bOa", [ "Key Ctrl+Up" ]);
        ("\x1bOd", [ "Key Ctrl+Left" ]);
        ("\x1bOe", [ "Key Ctrl+Begin keypad" ]);
        ("\x1b[2$", [ "Key Shift+Insert" ]);
        ("\x1b[3$", [ "Key Shift+Delete" ]);
        ("\x1b[23$", [ "Key Shift+F 11" ]);
        ("\x1b[2^", [ "Key Ctrl+Insert" ]);
        ("\x1b[11^", [ "Key Ctrl+F 1" ]);
        ("\x1b[11@", [ "Key Ctrl+Shift+F 1" ]);
        ("\x1b\x1b[A", [ "Key Alt+Up" ]);
        ("\x1b\x1bOA", [ "Key Alt+Up" ]);
      ];
    table "Linux console"
      [
        ("\x1b[[A", [ "Key F 1" ]);
        ("\x1b[[E", [ "Key F 5" ]);
        ("\x1b\x1b[[B", [ "Key Alt+F 2" ]);
        ("\x1b[[x", [ "Reply Csi CSI ["; "Key 'x' text \"x\"" ]);
      ];
    table "modifyOtherKeys"
      [
        ("\x1b[27;5;13~", [ "Key Ctrl+Enter" ]);
        ("\x1b[27;2;13~", [ "Key Shift+Enter" ]);
        ("\x1b[27;5;9~", [ "Key Ctrl+Tab" ]);
        ("\x1b[27;3;27~", [ "Key Alt+Escape" ]);
        ("\x1b[27;5;127~", [ "Key Ctrl+Backspace" ]);
        ("\x1b[27;6;33~", [ "Key Ctrl+'!'" ]);
        ("\x1b[27;5;45~", [ "Key Ctrl+'-'" ]);
        ("\x1b[27;2;65~", [ "Key 'A' text \"A\"" ]);
        ("\x1b[27;6;32~", [ "Key Ctrl+Shift+U+0020" ]);
        ("\x1b[27;3;233~", [ "Key Alt+'\xc3\xa9'" ]);
        ("\x1b[27;5;55296~", [ "Reply Csi CSI 27;5;55296 ~" ]);
        ("\x1b[27;5;1~", [ "Reply Csi CSI 27;5;1 ~" ]);
      ];
  ]

let kitty_keys =
  [
    table "key codes"
      [
        ("\x1b[97u", [ "Key 'a' text \"a\"" ]);
        ("\x1b[97;5u", [ "Key Ctrl+'a'" ]);
        ("\x1b[97;3u", [ "Key Alt+'a'" ]);
        ("\x1b[97;7u", [ "Key Ctrl+Alt+'a'" ]);
        ("\x1b[27u", [ "Key Escape" ]);
        ("\x1b[13;2u", [ "Key Shift+Enter" ]);
        ("\x1b[9;5u", [ "Key Ctrl+Tab" ]);
        ("\x1b[127;3u", [ "Key Alt+Backspace" ]);
        ("\x1b[32;6u", [ "Key Ctrl+Shift+U+0020" ]);
      ];
    table "the shifted key is the character typed"
      [
        ("\x1b[97:65;6u", [ "Key Ctrl+'A'" ]);
        ("\x1b[49:33;4u", [ "Key Alt+'!'" ]);
        ("\x1b[61:43;6u", [ "Key Ctrl+'+'" ]);
        (* A shifted key without Shift is ignored. *)
        ("\x1b[97:65;5u", [ "Key Ctrl+'a'" ]);
      ];
    table "without the alternate-key enhancement, Shift stays"
      [
        ("\x1b[49;4u", [ "Key Alt+Shift+'1'" ]);
        ("\x1b[97;6u", [ "Key Ctrl+Shift+'a'" ]);
        ("\x1b[97;2u", [ "Key Shift+'a'" ]);
      ];
    table "base layout key"
      [
        ("\x1b[1089::99;5u", [ "Key Ctrl+'\xd1\x81' base 'c'" ]);
        ("\x1b[1089:1057:99;6u", [ "Key Ctrl+'\xd0\xa1' base 'c'" ]);
      ];
    table "locks are state, never the key"
      [
        ("\x1b[115;69u", [ "Key Ctrl+'s' caps_lock" ]);
        ("\x1b[97;67u", [ "Key Alt+'a' caps_lock" ]);
        ("\x1b[97:65;68u", [ "Key Alt+'A' caps_lock" ]);
      ];
    table "event types"
      [
        ("\x1b[97;1:1u", [ "Key 'a' text \"a\"" ]);
        ("\x1b[97;1:2u", [ "Key 'a' repeat text \"a\"" ]);
        ("\x1b[97;1:3u", [ "Key_release 'a'" ]);
        ("\x1b[97;5:3u", [ "Key_release Ctrl+'a'" ]);
        ("\x1b[1;1:3A", [ "Key_release Up" ]);
        ("\x1b[3;1:3~", [ "Key_release Delete" ]);
        ("\x1b[1;1:3P", [ "Key_release F 1" ]);
        ("\x1b[57441;2:3u", [ "Key_release Shift+Left_shift" ]);
      ];
    table "associated text"
      [
        ("\x1b[97;;97u", [ "Key 'a' text \"a\"" ]);
        ("\x1b[97;2;65u", [ "Key 'A' text \"A\"" ]);
        ("\x1b[97:65;2;65u", [ "Key 'A' text \"A\"" ]);
        (* With Caps Lock, the text is what was typed. *)
        ("\x1b[114;65;82u", [ "Key 'R' caps_lock text \"R\"" ]);
        ("\x1b[114:82;66;114u", [ "Key 'r' caps_lock text \"r\"" ]);
        (* Shift that does not change the character stays. *)
        ("\x1b[32;2;32u", [ "Key Shift+U+0020 text \" \"" ]);
        ("\x1b[97;3;97u", [ "Key Alt+'a'" ]);
        ("\x1b[97;5;97u", [ "Key Ctrl+'a'" ]);
        ("\x1b[97;;1u", [ "Reply Csi CSI 97;;1 u" ]);
      ];
    table "text without a key"
      [
        ("\x1b[0;;229u", [ "Text \"\\195\\165\"" ]);
        ("\x1b[0;;104:105u", [ "Text \"hi\"" ]);
        ("\x1b[0;1:3;229u", []);
        ("\x1b[0u", [ "Reply Csi CSI 0 u" ]);
      ];
    table "functional keys"
      [
        ("\x1b[57358u", [ "Key Caps_lock" ]);
        ("\x1b[57361u", [ "Key Print_screen" ]);
        ("\x1b[57362u", [ "Key Pause" ]);
        ("\x1b[57363u", [ "Key Menu" ]);
        ("\x1b[57376u", [ "Key F 13" ]);
        ("\x1b[57398u", [ "Key F 35" ]);
        ("\x1b[57428u", [ "Key Media Play" ]);
        ("\x1b[57440u", [ "Key Media Mute" ]);
        ("\x1b[57442;5u", [ "Key Ctrl+Left_ctrl" ]);
        ("\x1b[57453u", [ "Key Level3_shift" ]);
        ("\x1b[57455u", [ "Key Unidentified 57455" ]);
        ("\x1b[63743u", [ "Key Unidentified 63743" ]);
      ];
    table "keypad keys are their main keys, on the keypad"
      [
        ("\x1b[57399u", [ "Key '0' keypad text \"0\"" ]);
        ("\x1b[57399;129u", [ "Key '0' num_lock keypad text \"0\"" ]);
        ("\x1b[57411u", [ "Key '*' keypad text \"*\"" ]);
        ("\x1b[57414u", [ "Key Enter keypad" ]);
        ("\x1b[57419;5u", [ "Key Ctrl+Up keypad" ]);
        ("\x1b[57427~", [ "Key Begin keypad" ]);
        ("\x1b[1;5E", [ "Key Ctrl+Begin keypad" ]);
      ];
    table "out of range"
      [
        ("\x1b[55296u", [ "Reply Csi CSI 55296 u" ]);
        ("\x1b[1114112u", [ "Reply Csi CSI 1114112 u" ]);
        ("\x1b[97;257u", [ "Reply Csi CSI 97;257 u" ]);
        ("\x1b[97;0u", [ "Reply Csi CSI 97;0 u" ]);
        ("\x1b[97;1:4u", [ "Reply Csi CSI 97;1:4 u" ]);
        ("\x1b[1u", [ "Reply Csi CSI 1 u" ]);
        ("\x1b[128u", [ "Reply Csi CSI 128 u" ]);
        ("\x1b[97:55296;2u", [ "Reply Csi CSI 97:55296;2 u" ]);
      ];
    table "extra fields are ignored"
      [ ("\x1b[97:65:97:5;6:1:9;65:66;7u", [ "Key Ctrl+'A' base 'a'" ]) ];
  ]

(* Mouse ------------------------------------------------------------------- *)

let mouse =
  [
    table "SGR"
      [
        ("\x1b[<0;1;1M", [ "Mouse Press Left at 0,0" ]);
        ("\x1b[<1;10;20M", [ "Mouse Press Middle at 9,19" ]);
        ("\x1b[<2;10;20m", [ "Mouse Release Right at 9,19" ]);
        ("\x1b[<0;10;20m", [ "Mouse Release Left at 9,19" ]);
        ("\x1b[<32;5;5M", [ "Mouse Motion Left at 4,4" ]);
        ("\x1b[<34;5;5M", [ "Mouse Motion Right at 4,4" ]);
        ("\x1b[<35;5;5M", [ "Mouse Motion None at 4,4" ]);
        ("\x1b[<64;3;3M", [ "Mouse Scroll Up at 2,2" ]);
        ("\x1b[<65;3;3M", [ "Mouse Scroll Down at 2,2" ]);
        ("\x1b[<66;3;3M", [ "Mouse Scroll Left at 2,2" ]);
        ("\x1b[<67;3;3M", [ "Mouse Scroll Right at 2,2" ]);
        ("\x1b[<128;1;1M", [ "Mouse Press Extra 8 at 0,0" ]);
        ("\x1b[<131;1;1m", [ "Mouse Release Extra 11 at 0,0" ]);
        ("\x1b[<4;1;1M", [ "Mouse Press Left at 0,0 shift" ]);
        ("\x1b[<8;1;1M", [ "Mouse Press Left at 0,0 alt" ]);
        ("\x1b[<16;1;1M", [ "Mouse Press Left at 0,0 ctrl" ]);
        ("\x1b[<92;1;1M", [ "Mouse Scroll Up at 0,0 shift alt ctrl" ]);
      ];
    table "coordinates below 1 are the first cell"
      [
        ("\x1b[<0;0;0M", [ "Mouse Press Left at 0,0" ]);
        ("\x1b[<0;0;0m", [ "Mouse Release Left at 0,0" ]);
        ("\x1b[<0;;5M", [ "Mouse Press Left at 0,4" ]);
      ];
    table "SGR out of range"
      [
        ("\x1b[<192;1;1M", [ "Reply Csi CSI <192;1;1 M" ]);
        ("\x1b[<256;1;1M", [ "Reply Csi CSI <256;1;1 M" ]);
        ("\x1b[<0;1M", [ "Reply Csi CSI <0;1 M" ]);
        ("\x1b[<0:1;1;1M", [ "Reply Csi CSI <0:1;1;1 M" ]);
      ];
    table "URXVT"
      [
        ("\x1b[32;5;5M", [ "Mouse Press Left at 4,4" ]);
        ("\x1b[35;5;5M", [ "Mouse Release None at 4,4" ]);
        ("\x1b[96;5;5M", [ "Mouse Scroll Up at 4,4" ]);
        ("\x1b[64;5;5M", [ "Mouse Motion Left at 4,4" ]);
        ("\x1b[31;5;5M", [ "Reply Csi CSI 31;5;5 M" ]);
      ];
    table "X10"
      [
        ("\x1b[M !!", [ "Mouse Press Left at 0,0" ]);
        ("\x1b[M#!!", [ "Mouse Release None at 0,0" ]);
        ("\x1b[M`!!", [ "Mouse Scroll Up at 0,0" ]);
        ("\x1b[M@++", [ "Mouse Motion Left at 10,10" ]);
        ("\x1b[M \xff\xff", [ "Mouse Press Left at 222,222" ]);
        ("\x1b[M   ", [ "Mouse Press Left at 0,0" ]);
        ("\x1b[M \x1b[A", [ "Cut mouse"; "Key Up" ]);
        ("\x1b\x1b[M !!", [ "Key Escape"; "Mouse Press Left at 0,0" ]);
      ];
    test "X10 cut by silence" (fun () ->
        check_steps
          [ F "\x1b[M !"; Idle; F "!" ]
          [ "Cut mouse"; "Key '!' text \"!\"" ]);
  ]

(* Focus and paste ------------------------------------------------------- *)

let paste =
  [
    table "focus"
      [
        ("\x1b[I", [ "Focus_in" ]);
        ("\x1b[O", [ "Focus_out" ]);
        ("\x1b[1I", [ "Reply Csi CSI 1 I" ]);
      ];
    table "paste is passed on exactly"
      [
        ( "\x1b[200~hello\x1b[201~",
          [ "Paste_start"; "Paste_data \"hello\""; "Paste_end" ] );
        ("\x1b[200~\x1b[201~", [ "Paste_start"; "Paste_end" ]);
        ( "\x1b[200~x\x1b[201x\x1b[200~\xff\x1b\x1b[201~",
          [
            "Paste_start";
            "Paste_data \"x\\027[201x\\027[200~\\255\\027\"";
            "Paste_end";
          ] );
        ( "\x1b[200~a\x1b[201;~b\x1b[201~c",
          [
            "Paste_start";
            "Paste_data \"a\\027[201;~b\"";
            "Paste_end";
            "Key 'c' text \"c\"";
          ] );
        ( "\x1b[200~\x03\x1b[A\r\n\x1b[201~",
          [ "Paste_start"; "Paste_data \"\\003\\027[A\\r\\n\""; "Paste_end" ] );
      ];
    test "markers split across reads" (fun () ->
        check_steps
          [ F "\x1b[20"; F "0~ab\x1b["; F "20"; F "1"; F "~" ]
          [ "Paste_start"; "Paste_data \"ab\""; "Paste_end" ]);
    test "silence never ends a paste" (fun () ->
        check_steps
          [ F "\x1b[200~ab"; Idle; F "c\x1b[2"; Idle; F "01~d" ]
          [
            "Paste_start";
            "Paste_data \"abc\"";
            "Paste_end";
            "Key 'd' text \"d\"";
          ]);
    test "the end of input ends a paste" (fun () ->
        check_steps
          [ F "\x1b[200~ab\x1b[20"; Finish ]
          [ "Paste_start"; "Paste_data \"ab\\027[20\""; "Paste_end cut" ]);
    test "cut_paste ends a paste, and what follows is input" (fun () ->
        check_steps
          [ F "\x1b[200~ab\x1b"; Cut_paste; F "[Aq" ]
          [
            "Paste_start";
            "Paste_data \"ab\\027\"";
            "Paste_end cut";
            "Key '[' text \"[\"";
            "Key 'A' text \"A\"";
            "Key 'q' text \"q\"";
          ]);
    test "cut_paste outside a paste does nothing" (fun () ->
        check_steps [ F "a"; Cut_paste ] [ "Key 'a' text \"a\"" ]);
    table "a stray end marker is no key"
      [ ("\x1b[201~", [ "Reply Csi CSI 201 ~" ]) ];
    table "an ESC before a paste is Escape"
      [
        ( "\x1b\x1b[200~x\x1b[201~",
          [ "Key Escape"; "Paste_start"; "Paste_data \"x\""; "Paste_end" ] );
      ];
  ]

(* Replies ---------------------------------------------------------------- *)

let replies =
  [
    table "device attributes"
      [
        ("\x1b[?64;1;2;4c", [ "Reply Primary_attributes 64;1;2;4" ]);
        ("\x1b[?c", [ "Reply Primary_attributes " ]);
        ("\x1b[>41;398;0c", [ "Reply Secondary_attributes 41;398;0" ]);
        ("\x1b[>1;10;0c", [ "Reply Secondary_attributes 1;10;0" ]);
      ];
    table "mode reports"
      [
        ("\x1b[?2026;0$y", [ "Reply Mode ?2026 Not_recognized" ]);
        ("\x1b[?2026;1$y", [ "Reply Mode ?2026 Set" ]);
        ("\x1b[?2026;2$y", [ "Reply Mode ?2026 Reset" ]);
        ("\x1b[?2026;3$y", [ "Reply Mode ?2026 Permanently_set" ]);
        ("\x1b[?2004;4$y", [ "Reply Mode ?2004 Permanently_reset" ]);
        ("\x1b[4;2$y", [ "Reply Mode 4 Reset" ]);
        ("\x1b[?1;1;2;2$y", [ "Reply Mode ?1 Set"; "Reply Mode ?2 Reset" ]);
        ("\x1b[?2026;5$y", [ "Reply Csi CSI ?2026;5 $ y" ]);
        ("\x1b[?2026$y", [ "Reply Csi CSI ?2026 $ y" ]);
        ("\x1b[?2026;$y", [ "Reply Csi CSI ?2026; $ y" ]);
      ];
    table "cursor reports"
      [
        ("\x1b[12;40R", [ "Reply Cursor 11,39" ]);
        (* xterm's DECXCPR omits the page: the default page, 1. *)
        ("\x1b[?12;40R", [ "Reply Cursor 11,39 page 1" ]);
        ("\x1b[?12;40;2R", [ "Reply Cursor 11,39 page 2" ]);
        ("\x1b[4611686018427387904;1R", [ "Reply Cursor 4611686018427387902,0" ]);
      ];
    table ~owed:cursor "F3 or a cursor report"
      [
        ("\x1b[1;2R", [ "Reply Cursor 0,1" ]);
        ("\x1b[1;1R", [ "Reply Cursor 0,0" ]);
        ("\x1b[13;2~", [ "Key Shift+F 3" ]);
      ];
    table "XTWINOPS and the in-band resize"
      [
        ("\x1b[4;480;800t", [ "Reply Text_area_pixels 800x480" ]);
        ("\x1b[6;20;10t", [ "Reply Cell_pixels 10x20" ]);
        ("\x1b[8;24;80t", [ "Reply Text_area 80x24" ]);
        ("\x1b[48;24;80;480;800t", [ "Reply Resize 80x24 800x480" ]);
        ("\x1b[48;24:1;80;480;800:9t", [ "Reply Resize 80x24 800x480" ]);
        ("\x1b[48;24;80t", [ "Reply Csi CSI 48;24;80 t" ]);
      ];
    table "kitty flags and color scheme"
      [
        ("\x1b[?5u", [ "Reply Keyboard_flags 5" ]);
        ("\x1b[?u", [ "Reply Keyboard_flags 0" ]);
        ("\x1b[?997;1n", [ "Reply Color_scheme Dark" ]);
        ("\x1b[?997;2n", [ "Reply Color_scheme Light" ]);
        ("\x1b[?997;3n", [ "Reply Csi CSI ?997;3 n" ]);
      ];
    table "operating status"
      [
        ("\x1b[0n", [ "Reply Operating_status Ok" ]);
        ("\x1b[3n", [ "Reply Operating_status Malfunction" ]);
        ("\x1b[1n", [ "Reply Csi CSI 1 n" ]);
      ];
    table "unknown sequences are never keys"
      [
        ("\x1b[?1a", [ "Reply Csi CSI ?1 a" ]);
        ("\x1b[1 q", [ "Reply Csi CSI 1 SP q" ]);
        ("\x1b[5n", [ "Reply Csi CSI 5 n" ]);
        ( "\x1bOz1",
          [ "Key Alt+'O'"; "Key 'z' text \"z\""; "Key '1' text \"1\"" ] );
        ("\x1bO1z", [ "Reply Ss3 CSI 1 z" ]);
      ];
  ]

let string_replies =
  [
    table ~owed:strings "DCS"
      [
        ("\x1bP>|xterm(398)\x1b\\", [ "Reply Version \"xterm(398)\"" ]);
        ("\x1bP>|tmux 3.5a\x1b\\", [ "Reply Version \"tmux 3.5a\"" ]);
        ("\x1bP1+r524742=31\x1b\\", [ "Reply Termcap \"RGB\"=\"1\"" ]);
        ( "\x1bP1+r544e=787465726d;436f=323536\x1b\\",
          [ "Reply Termcap \"TN\"=\"xterm\" \"Co\"=\"256\"" ] );
        ("\x1bP0+r544e\x1b\\", [ "Reply Termcap invalid \"TN\"" ]);
        ("\x1bP0+r\x1b\\", [ "Reply Termcap invalid" ]);
        ("\x1bP1+rzz\x1b\\", [ "Reply Dcs DCS 1 + r \"zz\"" ]);
        ("\x1bP!|00000000\x1b\\", [ "Reply Unit_id \"00000000\"" ]);
        ("\x1bP1$r0m\x1b\\", [ "Reply Dcs DCS 1 $ r \"0m\"" ]);
        (* A malformed header has no effect. *)
        ("\x1bP1?1q\x1b\\", []);
      ];
    table ~owed:strings "OSC"
      [
        (* An OSC reply keeps the terminator its query chose. *)
        ( "\x1b]11;rgb:ffff/8080/0000\x07",
          [ "Reply Color background #ff8000 BEL" ] );
        ("\x1b]10;rgb:f/8/0\x1b\\", [ "Reply Color foreground #ff8800" ]);
        ("\x1b]12;rgb:fff/000/800\x07", [ "Reply Color cursor #ff0080 BEL" ]);
        ("\x1b]4;1;rgb:cd/00/00\x1b\\", [ "Reply Color entry 1 #cd0000" ]);
        (* The older form: its digits are the high bits, not scaled. *)
        ("\x1b]11;#ffffff\x1b\\", [ "Reply Color background #ffffff" ]);
        ("\x1b]11;#f80\x1b\\", [ "Reply Color background #f08000" ]);
        ("\x1b]11;#fff800000\x1b\\", [ "Reply Color background #ff8000" ]);
        ("\x1b]11;#ffff\x1b\\", [ "Reply Osc \"11;#ffff\"" ]);
        (* X11 names: case-insensitive, spaces not counted (rgb 1.1.0). *)
        ("\x1b]11;white\x1b\\", [ "Reply Color background #ffffff" ]);
        ("\x1b]11;GhostWhite\x1b\\", [ "Reply Color background #f8f8ff" ]);
        ("\x1b]11;ghost white\x1b\\", [ "Reply Color background #f8f8ff" ]);
        ("\x1b]11;GHOST  WHITE\x1b\\", [ "Reply Color background #f8f8ff" ]);
        ("\x1b]11;Web Gray\x1b\\", [ "Reply Color background #808080" ]);
        ("\x1b]11;gray\x1b\\", [ "Reply Color background #bebebe" ]);
        ("\x1b]11;whitish\x1b\\", [ "Reply Osc \"11;whitish\"" ]);
        ("\x1b]11;\x1b\\", [ "Reply Osc \"11;\"" ]);
        ("\x1b]52;c;aGVsbG8=\x07", [ "Reply Clipboard \"c\" \"hello\" BEL" ]);
        ("\x1b]52;c;aGVsbG8\x1b\\", [ "Reply Clipboard \"c\" \"hello\"" ]);
        ("\x1b]52;c;\x1b\\", [ "Reply Clipboard \"c\" \"\"" ]);
        ("\x1b]52;c;a*b\x07", [ "Reply Osc \"52;c;a*b\" BEL" ]);
        ( "\x1b]1337;ReportCellSize=17.0;8.0\x1b\\",
          [ "Reply Osc \"1337;ReportCellSize=17.0;8.0\"" ] );
        ("\x1b]Lterm\x1b\\", [ "Reply Osc \"Lterm\"" ]);
      ];
    table ~owed:strings "APC"
      [
        ("\x1b_Gi=31337;OK\x1b\\", [ "Reply Graphics i=31337 \"OK\"" ]);
        ("\x1b_Gi=1,p=2;ENOENT:x\x1b\\", [ "Reply Graphics i=1 \"ENOENT:x\"" ]);
        ("\x1b_Ga=q\x1b\\", [ "Reply Graphics \"\"" ]);
        ("\x1b_X\x1b\\", [ "Key Alt+'_'"; "Key 'X' text \"X\""; "Key Alt+'\\'" ]);
      ];
    table ~owed:strings "what ends a string"
      [
        ("\x1b]11;x\x18a", [ "Cut string"; "Key 'a' text \"a\"" ]);
        ("\x1b]11;x\x1aa", [ "Cut string"; "Key 'a' text \"a\"" ]);
        ("\x1b]11;x\x1b[A", [ "Cut string"; "Key Up" ]);
        ("\x1bP>|x\x07y\x1b\\", [ "Reply Version \"xy\"" ]);
        ("\x1b_Gi=1;O\x07K\x1b\\", [ "Reply Graphics i=1 \"OK\"" ]);
      ];
    table ~owed:strings "the introducer commits by the next byte"
      [
        ("\x1b]a", [ "Key Alt+']'"; "Key 'a' text \"a\"" ]);
        ("\x1bPa", [ "Key Alt+'P'"; "Key 'a' text \"a\"" ]);
        ("\x1b_a", [ "Key Alt+'_'"; "Key 'a' text \"a\"" ]);
        ( "\x1b\x1b]a",
          [ "Key Alt+Escape"; "Key ']' text \"]\""; "Key 'a' text \"a\"" ] );
        ("\x1b\x1b]11;?\x07", [ "Key Escape"; "Reply Osc \"11;?\" BEL" ]);
      ];
    test "silence never cuts a string" (fun () ->
        check_steps ~owed:strings
          [ F "\x1b]11;rgb:1"; Idle; F "/2/3"; Idle; F "\x07" ]
          [ "Reply Color background #112233 BEL" ]);
    test "the end of input cuts a string" (fun () ->
        check_steps ~owed:strings [ F "\x1b]11;rgb:1"; Finish ] [ "Cut string" ]);
    test "a string over the budget is dropped whole" (fun () ->
        let big = String.make ((16 * 1024 * 1024) + 1) 'A' in
        check_steps ~owed:strings
          [ F "\x1b]52;c;"; F big; F "\x07a" ]
          [ "Too_long OSC"; "Key 'a' text \"a\"" ]);
    test "a string at the budget is kept" (fun () ->
        (* The content, "52;c;" and the data, is exactly the budget. *)
        let n = (16 * 1024 * 1024) - 5 in
        let d = D.create () in
        D.expect d strings;
        let got = ref [] in
        let s = "\x1b]52;c;" ^ String.make n 'A' ^ "\x07" in
        D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) (fun e ->
            got := e :: !got);
        match !got with
        | [ D.Reply (Clipboard { data; _ }) ] ->
            equal int ~msg:"decoded bytes" (n * 3 / 4) (String.length data)
        | l -> failf "got %s" (String.concat "; " (List.map show l)));
  ]

(* Silence and what is owed ------------------------------------------------- *)

let pending_name = function
  | D.Nothing -> "Nothing"
  | Ambiguous -> "Ambiguous"
  | Sequence -> "Sequence"

let pending_after ?(owed = owed ()) s =
  let d = D.create () in
  D.expect d owed;
  D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) ignore;
  pending_name (D.pending d)

let silence =
  [
    test "pending says what silence would decide" (fun () ->
        List.iter
          (fun (owed, s, p) ->
            equal string ~msg:(Printf.sprintf "%S" s) p (pending_after ~owed s))
          [
            (owed (), "", "Nothing");
            (owed (), "a", "Nothing");
            (owed (), "\x1b", "Ambiguous");
            (owed (), "\x1b\x1b", "Ambiguous");
            (owed (), "\x1b[", "Ambiguous");
            (owed (), "\x1bO", "Ambiguous");
            (strings, "\x1b]", "Ambiguous");
            (owed (), "\x1b]", "Nothing");
            (owed (), "\x1b[1", "Sequence");
            (owed (), "\x1b[M", "Sequence");
            (owed (), "\x1b[M!", "Sequence");
            (owed (), "\x1b[[", "Sequence");
            (owed (), "\xc3", "Sequence");
            (owed (), "\x1b\xe6\x97", "Sequence");
            (strings, "\x1b]1", "Nothing");
            (strings, "\x1b]1;x\x1b", "Nothing");
            (owed (), "\x1b[200~ab\x1b[2", "Nothing");
          ]);
    test "silence decides held bytes" (fun () ->
        List.iter
          (fun (owed, s, e) -> check ~owed s e)
          [
            (owed (), "\x1b", [ "Key Escape" ]);
            (owed (), "\x1b\x1b", [ "Key Alt+Escape" ]);
            (owed (), "\x1b\x1b\x1b", [ "Key Alt+Escape"; "Key Escape" ]);
            (owed (), "\x1b[", [ "Key Alt+'['" ]);
            (owed (), "\x1bO", [ "Key Alt+'O'" ]);
            (owed (), "\x1b\x1b[", [ "Key Alt+Escape"; "Key '[' text \"[\"" ]);
            (strings, "\x1b\x1bP", [ "Key Alt+Escape"; "Key 'P' text \"P\"" ]);
            (owed (), "\x1b\x1bP", [ "Key Alt+Escape"; "Key 'P' text \"P\"" ]);
            (owed (), "\x1b\x1ba", [ "Key Alt+Escape"; "Key 'a' text \"a\"" ]);
            (strings, "\x1b]", [ "Key Alt+']'" ]);
            (strings, "\x1bP", [ "Key Alt+'P'" ]);
            (owed (), "\x1b[1;5", [ "Cut sequence" ]);
            (owed (), "\x1b\x1b[1;5", [ "Key Escape"; "Cut sequence" ]);
            (owed (), "\x1b[[", [ "Cut sequence" ]);
            (owed (), "\xc3", [ "Invalid \"\\195\"" ]);
            (owed (), "\xf0\x9f\x98", [ "Invalid \"\\240\\159\\152\"" ]);
            (owed (), "\x1b\xc3", [ "Key Escape"; "Invalid \"\\195\"" ]);
          ]);
    test "silence after silence does nothing" (fun () ->
        check_steps
          [ F "\x1b"; Idle; Idle; F "a"; Idle ]
          [ "Key Escape"; "Key 'a' text \"a\"" ]);
    test "finish ends the input" (fun () ->
        check_steps [ F "\x1b"; Finish ] [ "Key Escape" ];
        check_steps [ F "\xc3"; Finish ] [ "Invalid \"\\195\"" ];
        check_steps
          [ F "\x1b[1"; Finish; F "a" ]
          [ "Cut sequence"; "Key 'a' text \"a\"" ]);
    test "reset drops what is held, and keeps what is owed" (fun () ->
        let d = D.create () in
        D.expect d strings;
        let got = ref [] in
        let emit e = got := show e :: !got in
        let feed s =
          D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) emit
        in
        feed "\x1b[1;5";
        D.reset d;
        feed "a\x1b]1;x\x07";
        equal events
          [ "Key 'a' text \"a\""; "Reply Osc \"1;x\" BEL" ]
          (List.rev !got));
  ]

let owed_tests =
  [
    test "a string commits only while one is owed" (fun () ->
        check_steps
          [
            F "\x1b]1;x\x07";
            Expect strings;
            F "\x1b]1;x\x07";
            Settle strings;
            F "\x1b]1";
          ]
          [
            "Key Alt+']'";
            "Key '1' text \"1\"";
            "Key ';' text \";\"";
            "Key 'x' text \"x\"";
            "Key Ctrl+'g'";
            "Reply Osc \"1;x\" BEL";
            "Key Alt+']'";
            "Key '1' text \"1\"";
          ]);
    test "a string begun goes on after the count is settled" (fun () ->
        check_steps
          [ Expect strings; F "\x1b]1;x"; Settle strings; F "y\x07" ]
          [ "Reply Osc \"1;xy\" BEL" ]);
    test "a cursor report is owed until settled, whatever arrives" (fun () ->
        check_steps
          [
            F "\x1b[1;2R";
            Expect cursor;
            F "\x1b[1;2R\x1b[1;2R";
            Settle cursor;
            F "\x1b[1;2R";
          ]
          [
            "Key Shift+F 3";
            "Reply Cursor 0,1";
            "Reply Cursor 0,1";
            "Key Shift+F 3";
          ]);
    test "counts add up" (fun () ->
        check_steps
          [
            Expect cursor;
            Expect cursor;
            Settle cursor;
            F "\x1b[1;2R";
            Settle cursor;
            F "\x1b[1;2R";
          ]
          [ "Reply Cursor 0,1"; "Key Shift+F 3" ]);
    test "settling more than is owed raises" (fun () ->
        let d = D.create () in
        D.expect d cursor;
        raises_match (Exn.invalid_arg ~substring:"more than is owed") (fun () ->
            D.settle d (owed ~cursor:2 ()));
        raises_match (Exn.invalid_arg ~substring:"more than is owed") (fun () ->
            D.settle d strings));
    test "negative counts raise" (fun () ->
        let d = D.create () in
        raises_match (Exn.invalid_arg ~substring:"negative") (fun () ->
            D.expect d (owed ~cursor:(-1) ()));
        raises_match (Exn.invalid_arg ~substring:"negative") (fun () ->
            D.settle d (owed ~strings:(-1) ())));
    test "feed checks its range" (fun () ->
        let d = D.create () in
        raises_match (Exn.invalid_arg ~substring:"feed") (fun () ->
            D.feed d (Bytes.create 2) ~off:1 ~len:2 ignore));
    test "the callback may settle, and the next bytes see it" (fun () ->
        let d = D.create () in
        D.expect d (owed ~cursor:1 ());
        let out = ref [] in
        let emit e =
          out := show e :: !out;
          match e with
          | D.Reply (I.Reply.Primary_attributes _) ->
              D.settle d (owed ~cursor:1 ())
          | _ -> ()
        in
        let s = "\x1b[?62c\x1b[1;2R" in
        D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) emit;
        equal events
          [ "Reply Primary_attributes 62"; "Key Shift+F 3" ]
          (List.rev !out));
    test "the callback must not decode with its decoder" (fun () ->
        let feed d s f =
          D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) f
        in
        List.iter
          (fun (name, call) ->
            let d = D.create () in
            raises_match ~msg:name (Exn.invalid_arg ~substring:name) (fun () ->
                feed d "\x1b[200~ab" (fun _ -> call d));
            (* The decoder is free again once the exception has left it. *)
            let out = ref [] in
            feed d "x" (fun e -> out := show e :: !out);
            equal ~msg:(name ^ ", then") events [ "Key 'x' text \"x\"" ]
              (List.rev !out))
          [
            ("feed", fun d -> feed d "x" ignore);
            ("idle", fun d -> D.idle d ignore);
            ("finish", fun d -> D.finish d ignore);
            ("cut_paste", fun d -> D.cut_paste d ignore);
            ("reset", D.reset);
          ]);
    test "a callback that raises drops what the decoder holds" (fun () ->
        let feed d s f =
          D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) f
        in
        let fail_on_first _ = raise Exit in
        let after d s =
          let out = ref [] in
          feed d s (fun e -> out := show e :: !out);
          List.rev !out
        in
        (* A paste begun by an earlier call, and the rest of this call. *)
        let d = D.create () in
        D.expect d (owed ~cursor:1 ());
        feed d "\x1b[200~" ignore;
        raises ~msg:"feed" Exit (fun () -> feed d "ab\x1b[201~c" fail_on_first);
        equal ~msg:"nothing held" bool true (D.pending d = D.Nothing);
        equal ~msg:"input again, still owed" events
          [ "Key 'z' text \"z\""; "Reply Cursor 0,1" ]
          (after d "z\x1b[1;2R");
        (* An ESC held by an earlier call, which idle decides. *)
        let d = D.create () in
        feed d "\x1b" ignore;
        raises ~msg:"idle" Exit (fun () -> D.idle d fail_on_first);
        equal ~msg:"after idle" events [ "Key 'x' text \"x\"" ] (after d "x"));
  ]

(* Regressions --------------------------------------------------------------- *)

(* The probes that found the previous parser's defects, each with the result
   the decoder's rules give. *)

let long_osc = "\x1b]52;c;" ^ String.make 6000 'A' ^ "\x1b\\"
let long_version = "\x1bP>|" ^ String.make 5000 'v' ^ "\x1b\\"

let split_at k s =
  [ F (String.sub s 0 k); F (String.sub s k (String.length s - k)) ]

let regressions =
  [
    test "a long reply is one reply, however it is split" (fun () ->
        List.iter
          (fun k ->
            let got = decode ~owed:strings (split_at k long_osc) in
            equal int ~msg:"one event" 1 (List.length got);
            is_true ~msg:"a clipboard reply"
              (String.starts_with ~prefix:"Reply Clipboard" (List.hd got)))
          [ 1; 2; 3; 100; 4095; 4096; 4097; 6000 ];
        List.iter
          (fun k ->
            equal events
              [ "Reply Version \"" ^ String.make 5000 'v' ^ "\"" ]
              (decode ~owed:strings (split_at k long_version)))
          [ 4096; 4097 ]);
    test "an endless sequence types nothing" (fun () ->
        let s = "\x1b[" ^ String.make 5000 '1' in
        check_steps (split_at 4097 s @ [ Idle ]) [ "Cut sequence" ];
        check_steps (split_at 4097 s @ [ Finish ]) [ "Cut sequence" ]);
    table "no input raises"
      [
        ("\x1b[27;5;55296~", [ "Reply Csi CSI 27;5;55296 ~" ]);
        ("\x1b[27;5;99999999~", [ "Reply Csi CSI 27;5;99999999 ~" ]);
        ("\x1b[55296u", [ "Reply Csi CSI 55296 u" ]);
      ];
    table "modified F1..F4"
      [
        ("\x1b[1;2P", [ "Key Shift+F 1" ]);
        ("\x1b[1;5Q", [ "Key Ctrl+F 2" ]);
        ("\x1b[1;5S", [ "Key Ctrl+F 4" ]);
        ("\x1b[P", [ "Key F 1" ]);
        ("\x1b[1;1:3P", [ "Key_release F 1" ]);
        ("\x1b[1;2R", [ "Key Shift+F 3" ]);
      ];
    table "SS3 with a modifier, and the application keypad"
      [
        ("\x1bO2P", [ "Key Shift+F 1" ]);
        ("\x1bO1;2P", [ "Key Shift+F 1" ]);
        ("\x1bOM", [ "Key Enter keypad" ]);
        ("\x1bOj", [ "Key '*' keypad text \"*\"" ]);
        ("\x1bOp", [ "Key '0' keypad text \"0\"" ]);
        ("\x1bOE", [ "Key Begin keypad" ]);
      ];
    table "Alt with an introducer loses no key"
      [
        ("\x1b[", [ "Key Alt+'['" ]);
        ("\x1b]", [ "Key Alt+']'" ]);
        ( "\x1b]abc",
          [
            "Key Alt+']'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
          ] );
        ( "\x1bPabc",
          [
            "Key Alt+'P'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
          ] );
        ( "\x1b_abc",
          [
            "Key Alt+'_'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
          ] );
        ("\x1b\xc3\xa9", [ "Key Alt+'\xc3\xa9'" ]);
        ("\x1b\xe6\x97\xa5", [ "Key Alt+'\xe6\x97\xa5'" ]);
        ("\x1b\x1b[A", [ "Key Alt+Up" ]);
      ];
    table ~owed:strings
      "Alt with an introducer loses no key, with a string owed"
      [
        ( "\x1b]abc",
          [
            "Key Alt+']'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
          ] );
        ( "\x1bPabc",
          [
            "Key Alt+'P'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
          ] );
      ];
    test "a reply prefix never waits for the next key" (fun () ->
        check_steps
          [ F "\x1b[?1"; Idle; F "a" ]
          [ "Cut sequence"; "Key 'a' text \"a\"" ];
        check_steps
          [ F "\x1b[1;"; Idle; F "a" ]
          [ "Cut sequence"; "Key 'a' text \"a\"" ]);
    table "a private marker makes no key"
      [ ("\x1b[?1a", [ "Reply Csi CSI ?1 a" ]) ];
    test "SGR motion is read from the report" (fun () ->
        check "\x1b[<32;5;5M" [ "Mouse Motion Left at 4,4" ];
        check "\x1b[<0;1;1M\x1b[<35;5;5M"
          [ "Mouse Press Left at 0,0"; "Mouse Motion None at 4,4" ];
        check "\x1b[<0;1;1M\x1b[<34;5;5M"
          [ "Mouse Press Left at 0,0"; "Mouse Motion Right at 4,4" ]);
    table "integers saturate, and a range is checked"
      [
        ( "\x1b[<0;99999999999999999999;1M",
          [ "Mouse Press Left at 4611686018427387902,0" ] );
        ("\x1b[<0;0;0M", [ "Mouse Press Left at 0,0" ]);
        ( "\x1b[1;99999999999999999999A",
          [ "Reply Csi CSI 1;4611686018427387903 A" ] );
        ("\x1b[4611686018427387904;1R", [ "Reply Cursor 4611686018427387902,0" ]);
        ( "\x1b[97;256u",
          [ "Key Ctrl+Alt+Shift+Super+Hyper+Meta+'a' caps_lock num_lock" ] );
      ];
    table "parameters out of place are no key"
      [
        ("\x1b[1;5;3A", [ "Reply Csi CSI 1;5;3 A" ]);
        ("\x1b[3;5;2~", [ "Reply Csi CSI 3;5;2 ~" ]);
        ("\x1b[1;2;3;4;5;6;7;8;9A", [ "Reply Csi CSI 1;2;3;4;5;6;7;8;9 A" ]);
        ("\x1b[97;1:4u", [ "Reply Csi CSI 97;1:4 u" ]);
      ];
    table "replies with their meaning"
      [
        ("\x1b[?2026;3$y", [ "Reply Mode ?2026 Permanently_set" ]);
        ("\x1b[?2027;3$y", [ "Reply Mode ?2027 Permanently_set" ]);
        ("\x1b[?2004;4$y", [ "Reply Mode ?2004 Permanently_reset" ]);
        ("\x1b[>1;10;0c", [ "Reply Secondary_attributes 1;10;0" ]);
        ("\x1b[?997;1n", [ "Reply Color_scheme Dark" ]);
        ("\x1b[?2026$y", [ "Reply Csi CSI ?2026 $ y" ]);
        ( "\x1b_Xabc\x1b\\",
          [
            "Key Alt+'_'";
            "Key 'X' text \"X\"";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
            "Key Alt+'\\'";
          ] );
        ( "\x1bXabc\x1b\\",
          [
            "Key Alt+'X'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
            "Key Alt+'\\'";
          ] );
        ( "\x1b^abc\x1b\\",
          [
            "Key Alt+'^'";
            "Key 'a' text \"a\"";
            "Key 'b' text \"b\"";
            "Key 'c' text \"c\"";
            "Key Alt+'\\'";
          ] );
      ];
    table "the in-band resize, and X10's limit"
      [
        ("\x1b[48;24;80;480;800t", [ "Reply Resize 80x24 800x480" ]);
        ("\x1b[M \xff\xff", [ "Mouse Press Left at 222,222" ]);
        (* Mode 1005's UTF-8 coordinates are not decoded. *)
        ( "\x1b[M \xc5\x8d!",
          [ "Mouse Press Left at 164,108"; "Key '!' text \"!\"" ] );
      ];
    table "one keystroke, one value"
      [
        ("\x1b[E", [ "Key Begin keypad" ]);
        ("\x1b[57427~", [ "Key Begin keypad" ]);
        ("\x1b[27;3;233~", [ "Key Alt+'\xc3\xa9'" ]);
        ("\x9bA", [ "Invalid \"\\155\""; "Key 'A' text \"A\"" ]);
        ("\x8fP", [ "Invalid \"\\143\""; "Key 'P' text \"P\"" ]);
        ("A", [ "Key 'A' text \"A\"" ]);
        ("\x1b[97:65;2u", [ "Key 'A' text \"A\"" ]);
        ("\x1b[97;1:3u", [ "Key_release 'a'" ]);
      ];
    test "the end of input decides what it holds" (fun () ->
        check_steps [ F "\x1b"; Finish ] [ "Key Escape" ];
        check_steps [ F "\xc3"; Finish ] [ "Invalid \"\\195\"" ];
        check_steps
          [ F "\x1b[200~abc"; Idle; F "def\x1b[201~" ]
          [ "Paste_start"; "Paste_data \"abcdef\""; "Paste_end" ]);
    test "silence between reads" (fun () ->
        check_steps [ F "\x1b"; Idle; F "a" ]
          [ "Key Escape"; "Key 'a' text \"a\"" ];
        check_steps [ F "\x1b["; Idle; F "A" ]
          [ "Key Alt+'['"; "Key 'A' text \"A\"" ];
        check_steps ~owed:strings
          [ F "\x1b]11;rgb:1"; Idle; F "/2/3\x07" ]
          [ "Reply Color background #112233 BEL" ];
        check_steps
          [ F "\x1b[?2026;"; Idle; F "2$y" ]
          [
            "Cut sequence";
            "Key '2' text \"2\"";
            "Key '$' text \"$\"";
            "Key 'y' text \"y\"";
          ];
        check_steps
          [ F "\xc3"; Idle; F "\xa9" ]
          [ "Invalid \"\\195\""; "Invalid \"\\169\"" ]);
    table "CAN, SUB and non-ASCII bytes cut a sequence"
      [
        ("\x1b[1\x18A", [ "Cut sequence"; "Key Ctrl+'x'"; "Key 'A' text \"A\"" ]);
        ("\x1b[1\x1aA", [ "Cut sequence"; "Key Ctrl+'z'"; "Key 'A' text \"A\"" ]);
        ( "\x1b[1\xc3\xa9",
          [ "Cut sequence"; "Key '\xc3\xa9' text \"\\195\\169\"" ] );
        ( "\x1b[1\x7f;5A",
          [
            "Cut sequence";
            "Key Backspace";
            "Key ';' text \";\"";
            "Key '5' text \"5\"";
            "Key 'A' text \"A\"";
          ] );
        ("\x1b[1\x1b[A", [ "Cut sequence"; "Key Up" ]);
      ];
  ]

(* Attacks -------------------------------------------------------------------- *)

let typed s =
  List.init (String.length s) (fun i ->
      match s.[i] with
      | '\r' -> "Key Enter"
      | ' ' -> "Key U+0020 text \" \""
      | c -> Printf.sprintf "Key '%c' text \"%c\"" c c)

let attacks =
  [
    test "Alt+] and a digit, then typing, with no string owed" (fun () ->
        check_steps
          [ F "\x1b]2"; F "hello\rworld\r" ]
          ("Key Alt+']'" :: typed "2hello\rworld\r"));
    test "Alt+] and a digit while a string is owed: swallowed until an ESC"
      (fun () ->
        check_steps ~owed:strings
          [ F "\x1b]2"; Idle; F "hello\r"; Idle; F "\x1b[A" ]
          [ "Cut string"; "Key Up" ]);
    test "Alt+Shift+P, then a parameter byte, then typing" (fun () ->
        check "\x1bP;abc\r" ("Key Alt+'P'" :: typed ";abc\r"));
    test "Alt+Shift+O then x is keypad 8: the bytes are the same" (fun () ->
        check "\x1bOx" [ "Key '8' keypad text \"8\"" ]);
    test "Alt+[ then 1 then x is a sequence: the bytes are the same" (fun () ->
        check "\x1b[1x" [ "Reply Csi CSI 1 x" ]);
    test "a reply stalled past the wait is cut, and its tail typed" (fun () ->
        check_steps
          [ F "\x1b[?64;1"; Idle; F ";2;6c" ]
          ("Cut sequence" :: typed ";2;6c"));
    test "a clipboard read that stalls is never typed" (fun () ->
        let n = 6 * 1024 * 1024 in
        let body = String.make n 'A' in
        let first = String.sub body 0 (1024 * 1024) in
        let rest = String.sub body (1024 * 1024) (n - (1024 * 1024)) in
        let got =
          decode ~owed:strings
            [ F "\x1b]52;c;"; F first; Idle; Idle; F rest; F "\x07" ]
        in
        equal int ~msg:"one event" 1 (List.length got);
        is_true ~msg:"a clipboard reply"
          (String.starts_with ~prefix:"Reply Clipboard \"c\"" (List.hd got)));
    test "settling what is owed makes ESC ] Alt+] again" (fun () ->
        check_steps
          [ Expect strings; Settle strings; F "\x1b]1" ]
          [ "Key Alt+']'"; "Key '1' text \"1\"" ]);
    test "a paste with no end has two ends" (fun () ->
        check_steps
          [ F "\x1b[200~abc"; Idle; F "\x03"; Cut_paste; F "\x03" ]
          [
            "Paste_start";
            "Paste_data \"abc\\003\"";
            "Paste_end cut";
            "Key Ctrl+'c'";
          ];
        check_steps
          [ F "\x1b[200~abc"; Finish ]
          [ "Paste_start"; "Paste_data \"abc\""; "Paste_end cut" ]);
    (* Inputs constructed against the rules, with silences ("|"). *)
    table "constructed inputs"
      [
        ("\x1bOq", [ "Key '1' keypad text \"1\"" ]);
        ("\x1b[A", [ "Key Up" ]);
        ("\x1b:", [ "Key Alt+':'" ]);
        ( "\x1b[?64;\x181c",
          [
            "Cut sequence";
            "Key Ctrl+'x'";
            "Key '1' text \"1\"";
            "Key 'c' text \"c\"";
          ] );
        ("\x9b?64;1c", "Invalid \"\\155\"" :: typed "?64;1c");
        ("\x1b[a", [ "Key Shift+Up" ]);
        ("\x1bOa", [ "Key Ctrl+Up" ]);
        ("\x1b[<0;0;5M", [ "Mouse Press Left at 0,4" ]);
        ("\x1b[27;1:3u", [ "Key_release Escape" ]);
      ];
    test "constructed inputs with silences" (fun () ->
        check_steps
          [ F "\x1b[M "; F "\xc3"; F "!" ]
          [ "Mouse Press Left at 162,0" ];
        check_steps
          [ F "\x1b\x1b"; Idle; F "[A" ]
          [ "Key Alt+Escape"; "Key '[' text \"[\""; "Key 'A' text \"A\"" ];
        check_steps
          [ F "\x1b[12;"; Idle; F "40R" ]
          ("Cut sequence" :: typed "40R");
        check_steps
          [ F "\x1b[200~abc"; Idle; F "def"; Idle; F "\x1b[A" ]
          [ "Paste_start"; "Paste_data \"abcdef\\027[A\"" ]);
  ]

(* The bytes a real xterm sends -------------------------------------------- *)

(* xterm 398 driven through XTest, in five configurations: legacy, with
   metaSendsEscape ("mse"), modifyOtherKeys 1 with and without it, and
   modifyOtherKeys 2 with it; Caps Lock off, then on. Each row is the bytes
   recorded and what they decode to. Without metaSendsEscape, Alt arrives as
   a Latin-1 character in UTF-8, which no rule can tell from typing; with
   modifyOtherKeys 2 xterm applies Caps Lock to chords. *)
let measured =
  [
    (* chord, configuration, bytes, events *)
    ("a", "all", "a", [ "Key 'a' text \"a\"" ]);
    ("Shift+a", "legacy", "A", [ "Key 'A' text \"A\"" ]);
    ("Shift+a", "moOK2 mse", "\x1b[27;2;65~", [ "Key 'A' text \"A\"" ]);
    ("Shift+1", "all", "!", [ "Key '!' text \"!\"" ]);
    ("Alt+a", "legacy", "\xc3\xa1", [ "Key '\xc3\xa1' text \"\\195\\161\"" ]);
    ("Alt+a", "mse", "\x1ba", [ "Key Alt+'a'" ]);
    ("Alt+a", "moOK2 mse", "\x1b[27;3;97~", [ "Key Alt+'a'" ]);
    ( "Alt+Shift+a",
      "legacy",
      "\xc3\x81",
      [ "Key '\xc3\x81' text \"\\195\\129\"" ] );
    ("Alt+Shift+a", "mse", "\x1bA", [ "Key Alt+'A'" ]);
    ("Alt+Shift+a", "moOK2 mse", "\x1b[27;4;65~", [ "Key Alt+'A'" ]);
    ( "Alt+Shift+1",
      "legacy",
      "\xc2\xa1",
      [ "Key '\xc2\xa1' text \"\\194\\161\"" ] );
    ("Alt+Shift+1", "mse", "\x1b!", [ "Key Alt+'!'" ]);
    ("Alt+Shift+1", "moOK2 mse", "\x1b[27;4;33~", [ "Key Alt+'!'" ]);
    ("Alt+[", "legacy", "\xc3\x9b", [ "Key '\xc3\x9b' text \"\\195\\155\"" ]);
    ("Alt+[", "mse", "\x1b[", [ "Key Alt+'['" ]);
    ("Alt+[", "moOK2 mse", "\x1b[27;3;91~", [ "Key Alt+'['" ]);
    ("Ctrl+a", "legacy", "\x01", [ "Key Ctrl+'a'" ]);
    ("Ctrl+a", "moOK2 mse", "\x1b[27;5;97~", [ "Key Ctrl+'a'" ]);
    ("Ctrl+Shift+a", "legacy", "\x01", [ "Key Ctrl+'a'" ]);
    ("Ctrl+Shift+a", "moOK2 mse", "\x1b[27;6;65~", [ "Key Ctrl+'A'" ]);
    ("Ctrl+s", "legacy", "\x13", [ "Key Ctrl+'s'" ]);
    ("Ctrl+s", "moOK2 mse", "\x1b[27;5;115~", [ "Key Ctrl+'s'" ]);
    ("Ctrl+Shift+1", "legacy", "!", [ "Key '!' text \"!\"" ]);
    ("Ctrl+Shift+1", "moOK1", "\x1b[27;6;33~", [ "Key Ctrl+'!'" ]);
    ("Ctrl+Return", "legacy", "\r", [ "Key Enter" ]);
    ("Ctrl+Return", "moOK1", "\x1b[27;5;13~", [ "Key Ctrl+Enter" ]);
    ("Shift+Return", "legacy", "\r", [ "Key Enter" ]);
    ("Shift+Return", "moOK1", "\x1b[27;2;13~", [ "Key Shift+Enter" ]);
    ("Ctrl+i", "legacy", "\t", [ "Key Tab" ]);
    ("Ctrl+i", "moOK2 mse", "\x1b[27;5;105~", [ "Key Ctrl+'i'" ]);
    ("Ctrl+j", "legacy", "\n", [ "Key Ctrl+'j'" ]);
    ("Ctrl+j", "moOK2 mse", "\x1b[27;5;106~", [ "Key Ctrl+'j'" ]);
    ("Ctrl+2", "legacy", "\x00", [ "Key Ctrl+U+0020" ]);
    ("Ctrl+2", "moOK2 mse", "\x1b[27;5;50~", [ "Key Ctrl+'2'" ]);
    ("Ctrl+minus", "legacy", "-", [ "Key '-' text \"-\"" ]);
    ("Ctrl+minus", "moOK1", "\x1b[27;5;45~", [ "Key Ctrl+'-'" ]);
    ("Ctrl+Alt+a", "legacy", "\xc2\x81", [ "Key U+0081" ]);
    ("Ctrl+Alt+a", "mse", "\x1b\x01", [ "Key Ctrl+Alt+'a'" ]);
    ("Ctrl+Alt+a", "moOK2 mse", "\x1b[27;7;97~", [ "Key Ctrl+Alt+'a'" ]);
    ("Shift+F3", "all", "\x1b[1;2R", [ "Key Shift+F 3" ]);
    ("Ctrl+F1", "all", "\x1b[1;5P", [ "Key Ctrl+F 1" ]);
    ("Shift+Up", "all", "\x1b[1;2A", [ "Key Shift+Up" ]);
    ("KP_5", "all", "\x1b[E", [ "Key Begin keypad" ]);
    ("KP_Enter", "all", "\r", [ "Key Enter" ]);
    ("Escape", "all", "\x1b", [ "Key Escape" ]);
    ("Alt+Escape", "legacy", "\xc2\x9b", [ "Key U+009B" ]);
    ("Alt+Escape", "mse", "\x1b\x1b", [ "Key Alt+Escape" ]);
    ("Alt+Escape", "moOK2 mse", "\x1b[27;3;27~", [ "Key Alt+Escape" ]);
    (* Caps Lock on *)
    ("Caps a", "all", "A", [ "Key 'A' text \"A\"" ]);
    ( "Caps Alt+a",
      "legacy",
      "\xc3\x81",
      [ "Key '\xc3\x81' text \"\\195\\129\"" ] );
    ("Caps Alt+a", "mse", "\x1bA", [ "Key Alt+'A'" ]);
    ("Caps Alt+a", "moOK2 mse", "\x1b[27;3;65~", [ "Key Alt+'A'" ]);
    ("Caps Ctrl+s", "legacy", "\x13", [ "Key Ctrl+'s'" ]);
    ("Caps Ctrl+s", "moOK2 mse", "\x1b[27;5;83~", [ "Key Ctrl+'S'" ]);
    ( "Caps Alt+Shift+a",
      "legacy",
      "\xc3\xa1",
      [ "Key '\xc3\xa1' text \"\\195\\161\"" ] );
    ("Caps Alt+Shift+a", "mse", "\x1ba", [ "Key Alt+'a'" ]);
    ("Caps Alt+Shift+a", "moOK2 mse", "\x1b[27;4;97~", [ "Key Alt+'a'" ]);
  ]

let measured_tests =
  [
    cases "xterm 398"
      ~name:(fun (chord, config, _, _) -> chord ^ ", " ^ config)
      measured
      (fun (_, _, bytes, e) -> check bytes e);
  ]

(* Key equivalence ----------------------------------------------------------- *)

(* Every keystroke of a US layout's 47 text keys under seven sets of modifiers
   and both Caps Lock states, encoded as legacy bytes (xterm with
   metaSendsEscape, whose rules the rows above measure) and as the kitty
   protocol sends it with flags 1|4, with flag 1 alone, and with every
   enhancement (its specification and source). Each decodes to the keystroke's value, the character typed
   with Shift consumed, or to the stated loss of its encoding. *)

let unshifted = "abcdefghijklmnopqrstuvwxyz`1234567890-=[]\\;',./"
let shifted = "ABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_+{}|:\"<>?"
let shift_of k = shifted.[String.index unshifted k]
let is_letter k = k >= 'a' && k <= 'z'

type chord = { shift : bool; alt : bool; ctrl : bool }

let chords =
  [
    { shift = false; alt = false; ctrl = false };
    { shift = true; alt = false; ctrl = false };
    { shift = false; alt = true; ctrl = false };
    { shift = true; alt = true; ctrl = false };
    { shift = false; alt = false; ctrl = true };
    { shift = true; alt = false; ctrl = true };
    { shift = false; alt = true; ctrl = true };
  ]

(* The character the key types: Shift applied, then Caps Lock to a letter,
   which Shift cancels (XKB). *)
let typed_char k m ~caps =
  let c = if m.shift then shift_of k else k in
  if caps && is_letter k then if m.shift then k else Char.uppercase_ascii k
  else c

(* The C0 control legacy bytes send for Ctrl with a key, if any. *)
let c0 k =
  match k with
  | 'a' .. 'z' -> Some (Char.chr (Char.code k - 0x60))
  | '[' -> Some '\x1b'
  | '\\' -> Some '\x1c'
  | ']' -> Some '\x1d'
  | '2' -> Some '\x00'
  | _ -> None

let legacy k m ~caps =
  let alt = if m.alt then "\x1b" else "" in
  if m.ctrl then Option.map (fun c -> alt ^ String.make 1 c) (c0 k)
  else Some (alt ^ String.make 1 (typed_char k m ~caps))

let kitty ~flag4 k m ~caps =
  if not (m.alt || m.ctrl) then
    (* Text is sent as text, with the locks applied. *)
    Some (String.make 1 (typed_char k m ~caps))
  else
    let bits =
      (if m.shift then 1 else 0)
      lor (if m.alt then 2 else 0)
      lor (if m.ctrl then 4 else 0)
      lor if caps then 64 else 0
    in
    let code = Char.code k in
    let alternate =
      if m.shift && flag4 then Printf.sprintf ":%d" (Char.code (shift_of k))
      else ""
    in
    Some (Printf.sprintf "\x1b[%d%s;%du" code alternate (bits + 1))

(* With every enhancement (flags 1|2|4|8|16), a key that types text is a
   sequence too, with the text it types, the locks applied: Caps Lock with r
   is [CSI 114 ; 65 ; 82 u]. A chord is as with flags 1|4. *)
let kitty_all k m ~caps =
  if m.alt || m.ctrl then kitty ~flag4:true k m ~caps
  else
    let bits = (if m.shift then 1 else 0) lor if caps then 64 else 0 in
    let alternate =
      if m.shift then Printf.sprintf ":%d" (Char.code (shift_of k)) else ""
    in
    Some
      (Printf.sprintf "\x1b[%d%s;%s;%du" (Char.code k) alternate
         (if bits = 0 then "" else string_of_int (bits + 1))
         (Char.code (typed_char k m ~caps)))

(* The keystroke's value: for a chord, the character typed without the lock;
   for text, the character typed. *)
let value k m ~caps =
  let chord = m.alt || m.ctrl in
  let c = if chord then typed_char k m ~caps:false else typed_char k m ~caps in
  let shift = chord && m.shift && c = k in
  (Uchar.of_char c, { K.no_mods with shift; alt = m.alt; ctrl = m.ctrl })

let decode_key s =
  let d = D.create () in
  let got = ref [] in
  D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) (fun e ->
      got := e :: !got);
  D.idle d (fun e -> got := e :: !got);
  match !got with
  | [ D.Key k ] -> k
  | l -> failf "%S decodes to %s" s (String.concat "; " (List.map show l))

let without_locks (m : K.mods) = { m with caps_lock = false; num_lock = false }

let key_equivalence () =
  let failures = ref [] in
  let checked = ref 0 and losses = ref 0 in
  String.iter
    (fun k ->
      List.iter
        (fun m ->
          List.iter
            (fun caps ->
              let c, mods = value k m ~caps in
              let expected = (K.Char c, mods) in
              List.iter
                (fun (name, encoding) ->
                  match encoding k m ~caps with
                  | None -> ()
                  | Some bytes ->
                      incr checked;
                      let got = decode_key bytes in
                      let got_v = (got.K.key, without_locks got.mods) in
                      (* The stated losses, each with the value it gives. *)
                      let loss =
                        if name = "kitty 1" && m.shift && (m.alt || m.ctrl) then
                          Some
                            ( K.Char (Uchar.of_char k),
                              { mods with shift = true } )
                        else if name = "legacy" && m.ctrl then
                          (* A C0 control: Shift lost, and some aliases. *)
                          let byte = Option.get (c0 k) in
                          let ctrl key =
                            (key, { K.no_mods with ctrl = true; alt = m.alt })
                          in
                          let alt key = (key, { K.no_mods with alt = m.alt }) in
                          Some
                            (match byte with
                            | '\x1b' -> alt K.Escape
                            | '\x09' -> alt K.Tab
                            | '\x0d' -> alt K.Enter
                            | '\x08' -> alt K.Backspace
                            | '\x00' -> ctrl (K.Char (Uchar.of_char ' '))
                            | _ -> ctrl (K.Char (Uchar.of_char k)))
                        else if name = "legacy" && caps && m.alt && is_letter k
                        then
                          (* The lock applied before the ESC. *)
                          Some
                            ( K.Char (Uchar.of_char (typed_char k m ~caps)),
                              { mods with shift = false } )
                        else None
                      in
                      let want = Option.value loss ~default:expected in
                      if loss <> None && want <> expected then incr losses;
                      if got_v <> want then
                        failures :=
                          Format.asprintf "%s %c %s%s%s%s: %S gives %a" name k
                            (if m.ctrl then "ctrl " else "")
                            (if m.alt then "alt " else "")
                            (if m.shift then "shift " else "")
                            (if caps then "caps" else "")
                            bytes K.pp got
                          :: !failures;
                      (* Locks are state: the kitty protocol reports them on
                         chords, and on every key when all keys are
                         sequences; legacy bytes never do. *)
                      let caps_reported = got.K.mods.caps_lock in
                      let caps_expected =
                        caps && name <> "legacy"
                        && (m.alt || m.ctrl || name = "kitty all")
                      in
                      if caps_reported <> caps_expected then
                        failures :=
                          Printf.sprintf "%s %c: caps_lock %b" name k
                            caps_reported
                          :: !failures)
                [
                  ("legacy", legacy);
                  ("kitty 1|4", kitty ~flag4:true);
                  ("kitty 1", kitty ~flag4:false);
                  ("kitty all", kitty_all);
                ])
            [ false; true ])
        chords)
    unshifted;
  if !failures <> [] then
    failf "%d of %d keystrokes:\n%s" (List.length !failures) !checked
      (String.concat "\n" (List.rev !failures));
  (* The count of keystrokes whose encoding loses a fact, pinned so that a
     change to the rules shows here. *)
  equal int ~msg:"keystrokes checked" 2530 !checked;
  equal int ~msg:"stated losses" 320 !losses

let equivalence_tests = [ test "one value per keystroke" key_equivalence ]

(* Encoding ------------------------------------------------------------------ *)

let keyboard ?(cursor_keys = false) ?(keypad = false) ?(new_line = false)
    ?(modify_other_keys = 0) ?(kitty = 0) () =
  { K.cursor_keys; keypad; new_line; modify_other_keys; kitty }

(* The rows of [measured] whose keystroke the decoder reads whole: xterm's
   Alt arrives as an ESC (metaSendsEscape, which the encoder always is), and
   modifyOtherKeys 2 applies Caps Lock to a chord, which the value read
   back has lost. Each keystroke read encodes to the bytes xterm sent. *)
let xterm_bytes () =
  let has_prefix p s =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  let contains_alt s =
    let rec go i =
      i + 3 <= String.length s && (String.sub s i 3 = "Alt" || go (i + 1))
    in
    go 0
  in
  List.iter
    (fun (chord, config, bytes, _) ->
      let e =
        match config with
        | "all" | "mse" -> Some (keyboard ())
        | "legacy" when not (contains_alt chord) -> Some (keyboard ())
        | "moOK1" -> Some (keyboard ~modify_other_keys:1 ())
        | "moOK2 mse" when not (has_prefix "Caps" chord) ->
            Some (keyboard ~modify_other_keys:2 ())
        | _ -> None
      in
      Option.iter
        (fun e ->
          equal string
            ~msg:(Printf.sprintf "%s, %s" chord config)
            bytes
            (K.encode e (decode_key bytes)))
        e)
    measured

(* The decoder reads the encoder's bytes for every keystroke of the
   equivalence table as it reads the bytes the terminals send. *)
let encoded_equivalence () =
  String.iter
    (fun k ->
      List.iter
        (fun m ->
          List.iter
            (fun caps ->
              List.iter
                (fun (encoding, e) ->
                  match encoding k m ~caps with
                  | None -> ()
                  | Some bytes ->
                      let read = decode_key bytes in
                      let again = K.encode e read in
                      let read' = decode_key again in
                      if read' <> read then
                        failf "%S reads %a, which encodes to %S, read as %a"
                          bytes K.pp read again K.pp read')
                [
                  (legacy, keyboard ());
                  (kitty ~flag4:true, keyboard ~kitty:5 ());
                  (kitty ~flag4:false, keyboard ~kitty:1 ());
                  (kitty_all, keyboard ~kitty:31 ());
                ])
            [ false; true ])
        chords)
    unshifted

let press ?(mods = K.no_mods) ?(text = "") ?base ?(keypad = false)
    ?(repeat = false) key =
  { K.key; mods; text; base; keypad; repeat }

let char c = K.Char (Uchar.of_char c)
let ctrl = { K.no_mods with ctrl = true }

(* The losses [keystroke.mli] states, each in the bytes that carry it. *)
let stated_losses () =
  let read e k = Format.asprintf "%a" K.pp (decode_key (K.encode e k)) in
  let loses msg e k expected = equal string ~msg expected (read e k) in
  loses "legacy bytes lose Shift under Ctrl" (keyboard ())
    (press ~mods:ctrl (char 'A'))
    "Ctrl+'a'";
  loses "modifyOtherKeys carries it"
    (keyboard ~modify_other_keys:1 ())
    (press ~mods:ctrl (char 'A'))
    "Ctrl+'A'";
  loses "kitty without alternate keys sends the unshifted key"
    (keyboard ~kitty:1 ())
    (press ~mods:ctrl (char '!'))
    "Ctrl+Shift+'1'";
  loses "with them, the character" (keyboard ~kitty:5 ())
    (press ~mods:ctrl (char '!'))
    "Ctrl+'!'";
  loses "Ctrl+h is Backspace in legacy bytes" (keyboard ())
    (press ~mods:ctrl (char 'h'))
    "Backspace";
  loses "Ctrl+[ is Escape in legacy bytes" (keyboard ())
    (press ~mods:ctrl (char '['))
    "Escape";
  loses "the kitty protocol's disambiguation keeps it" (keyboard ~kitty:1 ())
    (press ~mods:ctrl (char 'h'))
    "Ctrl+'h'";
  loses "legacy bytes send a keypad key as its main-keyboard twin" (keyboard ())
    (press ~keypad:true ~text:"5" (char '5'))
    "'5' text \"5\"";
  loses "the application keypad keeps it" (keyboard ~keypad:true ())
    (press ~keypad:true ~text:"5" (char '5'))
    "'5' keypad text \"5\"";
  loses "legacy bytes never report the locks" (keyboard ())
    (press ~mods:{ ctrl with caps_lock = true } (char 's'))
    "Ctrl+'s'";
  loses "the kitty protocol reports them on chords" (keyboard ~kitty:1 ())
    (press ~mods:{ ctrl with caps_lock = true } (char 's'))
    "Ctrl+'s' caps_lock";
  loses "a repeat is a press without event types" (keyboard ~kitty:1 ())
    (press ~repeat:true Escape)
    "Escape";
  loses "with them, a repeat" (keyboard ~kitty:3 ())
    (press ~repeat:true Escape)
    "Escape repeat";
  equal string ~msg:"LNM: Enter is CR LF" "\r\n"
    (K.encode (keyboard ~new_line:true ()) (press Enter));
  equal string ~msg:"DECCKM: SS3 forms" "\027OA"
    (K.encode (keyboard ~cursor_keys:true ()) (press Up));
  equal string ~msg:"DECCKM: modified keys keep CSI" "\027[1;5A"
    (K.encode (keyboard ~cursor_keys:true ()) (press ~mods:ctrl Up));
  let release =
    { K.key = Escape; mods = K.no_mods; base = None; keypad = false }
  in
  equal string ~msg:"no release without event types" ""
    (K.encode_release (keyboard ~kitty:1 ()) release);
  equal string ~msg:"a release" "\027[27;1:3u"
    (K.encode_release (keyboard ~kitty:3 ()) release);
  equal string ~msg:"no release of a key sent as text" ""
    (K.encode_release (keyboard ~kitty:3 ()) { release with key = char 'a' })

let unencodable () =
  let raises msg e k =
    raises_match ~msg (Exn.invalid_arg ~substring:"Keystroke.encode") (fun () ->
        K.encode e k)
  in
  raises "a media key in legacy bytes" (keyboard ()) (press (Media Play));
  raises "F21 in legacy bytes" (keyboard ()) (press (F 21));
  raises "F36" (keyboard ~kitty:1 ()) (press (F 36));
  raises "a modifier key without every key a sequence" (keyboard ~kitty:1 ())
    (press (Modifier Left_shift));
  raises "a control character" (keyboard ()) (press (char '\001'));
  raises "an unidentified key outside the functional range"
    (keyboard ~kitty:1 ()) (press (Unidentified 65));
  equal string ~msg:"a media key in the kitty protocol" "\027[57428u"
    (K.encode (keyboard ~kitty:1 ()) (press (Media Play)))

let paste_and_focus () =
  check
    (D.encode_paste ~bracketed:true "a\027[A")
    [ "Paste_start"; "Paste_data \"a\\027[A\""; "Paste_end" ];
  check
    (D.encode_paste ~bracketed:false "ab")
    [ "Key 'a' text \"a\""; "Key 'b' text \"b\"" ];
  check (D.encode_focus true) [ "Focus_in" ];
  check (D.encode_focus false) [ "Focus_out" ]

let encoding_tests =
  [
    test "a keystroke read encodes to xterm's bytes" xterm_bytes;
    test "every keystroke of the table, encoded" encoded_equivalence;
    test "the stated losses" stated_losses;
    test "what no terminal sends raises" unencodable;
    test "paste and focus reports" paste_and_focus;
  ]

(* Ctrl+C, however the terminal encodes it, and nothing else. *)
let interrupt () =
  List.iter
    (fun (bytes, expected) ->
      equal bool ~msg:(String.escaped bytes) expected
        (K.is_interrupt (decode_key bytes)))
    [
      ("\003", true);
      ("\027[99;5u", true);
      ("\027[99:67;6u", true);
      ("\027[1089::99;5u", true);
      ("c", false);
      ("\004", false);
      ("\027\003", false);
      ("\027[99;7u", false);
    ]

let interrupt_tests = [ test "Ctrl+C is the interrupt" interrupt ]

(* A keystroke made with [Keystroke.make] and its defaults is the one the
   decoder reports for the same key: the text a key types, none for a chord,
   a control or a key that is not a character. *)
let made_as_decoded () =
  let ctrl = { K.no_mods with ctrl = true } in
  let alt = { K.no_mods with alt = true } in
  let char c = K.Char (Uchar.of_char c) in
  List.iter
    (fun (bytes, made) -> check bytes [ show (D.Key made) ])
    [
      ("a", K.make (char 'a'));
      ("\xc3\xa9", K.make (K.Char (Uchar.of_int 0xE9)));
      ("\x01", K.make ~mods:ctrl (char 'a'));
      ("\x1ba", K.make ~mods:alt (char 'a'));
      ("\x1b[A", K.make K.Up);
      ("\x1b[97;5u", K.make ~mods:ctrl (char 'a'));
    ]

let make_tests = [ test "make agrees with the decoder" made_as_decoded ]

let () =
  run "matrix.input.decoder"
    [
      group "legacy keys" legacy_keys;
      group "xterm keys" xterm_keys;
      group "kitty keys" kitty_keys;
      group "mouse" mouse;
      group "focus and paste" paste;
      group "replies" replies;
      group "string replies" string_replies;
      group "silence" silence;
      group "owed" owed_tests;
      group "regressions" regressions;
      group "attacks" attacks;
      group "measured" measured_tests;
      group "encoding" encoding_tests;
      group "key equivalence" equivalence_tests;
      group "interrupt" interrupt_tests;
      group "make" make_tests;
    ]
