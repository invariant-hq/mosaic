open Windtrap
open Ansi

(* --- Helpers --- *)

(* Test helper that wraps callback-based feed to return a list *)
let feed_to_list p bytes off len =
  let acc = ref [] in
  Parser.feed p bytes ~off ~len (fun tok -> acc := tok :: !acc);
  List.rev !acc

let check_color = Testable.make ~pp:Color.pp ~equal:Color.equal ()

(* Strict checking for escape sequences *)
let check_seq msg expected actual = equal ~msg string expected actual

(* --- 1. Color Logic & Math --- *)

let color_parsing_hex () =
  let c = Color.of_hex_exn in
  (* Standard 6 digit *)
  equal ~msg:"hex 6 digit" check_color (Color.of_rgb 255 0 128) (c "#ff0080");
  (* Short 3 digit *)
  equal ~msg:"hex 3 digit" check_color (Color.of_rgb 255 0 255) (c "#f0f");
  (* Alpha 8 digit *)
  equal ~msg:"hex 8 digit" check_color
    (Color.of_rgba 0 255 0 128)
    (c "#00ff0080");
  (* Case insensitivity *)
  equal ~msg:"hex case" check_color (Color.of_rgb 170 187 204) (c "#AABBCC");
  (* Failures *)
  is_true ~msg:"invalid hex len" (Color.of_hex "12" = None);
  is_true ~msg:"invalid hex char" (Color.of_hex "GG0000" = None)

let color_hsl_roundtrip () =
  (* Pure Red (RGB): 0 deg, 100%, 50% *)
  (* Use explicit RGB rather than Color.red since ANSI red varies by terminal *)
  let pure_red = Color.of_rgb 255 0 0 in
  let h, s, l, _ = Color.to_hsl pure_red in
  equal ~msg:"red hue" (float 0.01) 0.0 h;
  equal ~msg:"red saturation" (float 0.01) 1.0 s;
  equal ~msg:"red lightness" (float 0.01) 0.5 l;

  (* Roundtrip reconstruction *)
  let reconstructed = Color.of_hsl ~h ~s ~l () in
  equal ~msg:"red roundtrip" check_color pure_red reconstructed

let color_blending () =
  let bg = Color.of_rgb 0 0 0 in
  (* Black *)
  let fg = Color.of_rgba 255 255 255 128 in
  (* 50% White *)

  (* Linear blend: 50% white over black should be approx gray 127 *)
  let res = Color.blend ~mode:`Linear ~src:fg ~dst:bg () in
  let r, g, b, a = Color.to_rgba res in
  equal ~msg:"blend result opaque" int 255 a;
  is_true ~msg:"blend result roughly gray" (r > 120 && r < 135 && r = g && g = b)

let color_intent_is_part_of_identity () =
  (* Exact equality must preserve terminal emission intent, while visual
     equality can still compare the RGBA snapshot. *)
  let indexed_red = Color.indexed 1 in
  let rgb_red = Color.of_rgb 128 0 0 in
  (match Color.intent indexed_red with
  | Color.Indexed 1 -> ()
  | _ -> fail "Expected indexed intent");
  (match Color.intent rgb_red with
  | Color.Rgb -> ()
  | _ -> fail "Expected RGB intent");
  is_false ~msg:"intent differs" (Color.equal indexed_red rgb_red);
  is_true ~msg:"rgba snapshot matches" (Color.equal_rgba indexed_red rgb_red);
  is_false ~msg:"default intent differs from transparent RGB"
    (Color.equal Color.default (Color.of_rgba 0 0 0 0))

let color_ansi16_palette_matches_standard_fallback () =
  (* The fallback ANSI16 table uses 0x80/0xc0 base colors. *)
  let expected =
    [|
      (0x00, 0x00, 0x00);
      (0x80, 0x00, 0x00);
      (0x00, 0x80, 0x00);
      (0x80, 0x80, 0x00);
      (0x00, 0x00, 0x80);
      (0x80, 0x00, 0x80);
      (0x00, 0x80, 0x80);
      (0xc0, 0xc0, 0xc0);
      (0x80, 0x80, 0x80);
      (0xff, 0x00, 0x00);
      (0x00, 0xff, 0x00);
      (0xff, 0xff, 0x00);
      (0x00, 0x00, 0xff);
      (0xff, 0x00, 0xff);
      (0x00, 0xff, 0xff);
      (0xff, 0xff, 0xff);
    |]
  in
  Array.iteri
    (fun i expected_rgb ->
      let er, eg, eb = expected_rgb in
      let ar, ag, ab = Color.to_rgb (Color.indexed i) in
      equal ~msg:(Printf.sprintf "ansi16 slot %d r" i) int er ar;
      equal ~msg:(Printf.sprintf "ansi16 slot %d g" i) int eg ag;
      equal ~msg:(Printf.sprintf "ansi16 slot %d b" i) int eb ab)
    expected

let color_downgrade_semantics () =
  (* 1. Truecolor -> Truecolor (Identity) *)
  let c = Color.of_rgb 100 150 200 in
  equal ~msg:"identity downgrade" check_color c
    (Color.downgrade ~level:`Truecolor c);

  (* 2. Grayscale mapping in Ansi256 *)
  (* 256 palette has a grayscale ramp from 232 (dark) to 255 (light) *)
  (* Gray 10/10/10 should map low in the ramp *)
  let dark_gray = Color.of_rgb 8 8 8 in
  let down = Color.downgrade ~level:`Ansi256 dark_gray in
  match Color.intent down with
  | Color.Indexed n -> is_true ~msg:"mapped to grayscale ramp" (n = 232)
  | _ -> fail "Expected indexed color"

let color_extended_out_of_range_is_safe () =
  (* Regression from the matrix.ansi review: out-of-range palette inputs must
     not reach unsafe palette indexing. *)
  is_true ~msg:"negative extended compares safely"
    (Color.equal (Color.indexed (-1)) Color.black);
  is_true ~msg:"large extended compares safely"
    (Color.equal (Color.indexed 999) (Color.indexed 255));
  ignore (Color.hash (Color.indexed 999) : int);
  ignore (Color.compare (Color.indexed (-1)) (Color.indexed 999) : int)

(* --- 2. Attributes & Style --- *)

let attr_bitmask_integrity () =
  (* Verify bitmasks don't overlap using a set operation *)
  let all_set =
    Attr.combine ~bold:true ~dim:true ~italic:true ~underline:true ~blink:true
      ~inverse:true ~hidden:true ~strikethrough:true ~double_underline:true
      ~overline:true ~framed:true ~encircled:true ()
  in
  let count = Attr.cardinal all_set in
  equal ~msg:"all flags distinct" int 12 count;

  (* Check that removing one specific flag works *)
  let minus_bold = Attr.remove Attr.Bold all_set in
  is_false ~msg:"removed bold" (Attr.mem Attr.Bold minus_bold);
  is_true ~msg:"kept italic" (Attr.mem Attr.Italic minus_bold)

let style_composition_and_hash () =
  let s1 = Style.make ~fg:Color.red () in
  let s2 = Style.make ~bg:Color.blue () in
  let s3 = Style.merge ~base:s1 ~overlay:s2 in

  equal ~msg:"composed fg" check_color Color.red (Option.get s3.fg);
  equal ~msg:"composed bg" check_color Color.blue (Option.get s3.bg);

  (* Hash consistency *)
  let s3_clone = Style.make ~fg:Color.red ~bg:Color.blue () in
  equal ~msg:"hash stable" int (Style.hash s3) (Style.hash s3_clone)

let attr_extended_ordering () =
  let style =
    Style.make ~double_underline:true ~overline:true ~framed:true
      ~encircled:true ()
  in
  (* Minimal SGR: no reset needed when going from default, just enable attrs *)
  check_seq "extended attr ordering" "\x1b[21;53;51;52m"
    (Style.sgr_sequence style);
  equal ~msg:"no-op when unchanged" string ""
    (Style.sgr_sequence ~prev:style style)

let style_shared_disable_codes () =
  (* Bold and Dim share disable code 22. When transitioning from bold+dim to
     dim-only, we must re-enable dim after disabling bold. *)
  let bold_dim = Style.make ~bold:true ~dim:true () in
  let dim_only = Style.make ~dim:true () in
  let codes = Style.to_sgr_codes ~prev:bold_dim dim_only in
  (* Should contain 22 (disable bold/dim) and 2 (re-enable dim) *)
  is_true ~msg:"contains disable code 22" (List.mem 22 codes);
  is_true ~msg:"contains re-enable dim code 2" (List.mem 2 codes);

  (* Underline and Double_underline share disable code 24 *)
  let underline_double = Style.make ~underline:true ~double_underline:true () in
  let underline_only = Style.make ~underline:true () in
  let codes2 = Style.to_sgr_codes ~prev:underline_double underline_only in
  is_true ~msg:"contains disable code 24" (List.mem 24 codes2);
  is_true ~msg:"contains re-enable underline code 4" (List.mem 4 codes2)

let style_emit_function () =
  (* Test that emit produces the same output as sgr_sequence *)
  let style = Style.make ~fg:Color.red ~bold:true () in
  let buf = Bytes.create 64 in
  let w = Writer.make buf in
  Style.emit style w;
  let emitted = Bytes.sub_string buf 0 (Writer.len w) in
  let seq = Style.sgr_sequence style in
  check_seq "emit matches sgr_sequence" seq emitted;

  (* Test emit with prev style *)
  let buf2 = Bytes.create 64 in
  let w2 = Writer.make buf2 in
  let style2 = Style.make ~fg:Color.red ~italic:true () in
  Style.emit ~prev:style style2 w2;
  let emitted2 = Bytes.sub_string buf2 0 (Writer.len w2) in
  (* Should only disable bold (22) and enable italic (3) - not emit fg again *)
  let codes2 = Style.to_sgr_codes ~prev:style style2 in
  is_true ~msg:"emit delta non-empty" (String.length emitted2 > 0);
  is_true ~msg:"delta contains disable bold (22)" (List.mem 22 codes2);
  is_true ~msg:"delta contains enable italic (3)" (List.mem 3 codes2);
  is_false ~msg:"delta does not re-emit fg (31)" (List.mem 31 codes2)

let style_resolve_multiple () =
  let s1 = Style.make ~fg:Color.red () in
  let s2 = Style.make ~bg:Color.blue () in
  let s3 = Style.make ~bold:true () in
  let resolved = Style.resolve [ s1; s2; s3 ] in
  equal ~msg:"resolve fg" check_color Color.red (Option.get resolved.fg);
  equal ~msg:"resolve bg" check_color Color.blue (Option.get resolved.bg);
  is_true ~msg:"resolve has bold" (Attr.mem Attr.Bold resolved.attrs);

  (* Later styles override colors *)
  let s4 = Style.make ~fg:Color.green () in
  let resolved2 = Style.resolve [ s1; s4 ] in
  equal ~msg:"resolve override fg" check_color Color.green
    (Option.get resolved2.fg)

let style_compare_and_hash () =
  let s1 = Style.make ~fg:Color.red ~bold:true () in
  let s2 = Style.make ~fg:Color.red ~bold:true () in
  let s3 = Style.make ~fg:Color.blue ~bold:true () in

  (* Equal styles should compare equal *)
  equal ~msg:"equal styles compare 0" int 0 (Style.compare s1 s2);
  (* Equal styles should have same hash *)
  equal ~msg:"equal styles same hash" int (Style.hash s1) (Style.hash s2);

  (* Different styles should not compare equal *)
  is_true ~msg:"different styles compare non-zero" (Style.compare s1 s3 <> 0);

  (* Ordering should be consistent *)
  let cmp12 = Style.compare s1 s3 in
  let cmp21 = Style.compare s3 s1 in
  is_true ~msg:"compare antisymmetric"
    (cmp12 = -cmp21 || (cmp12 = 0 && cmp21 = 0))

(* --- 3. SGR State Machine (Diffing) --- *)

let sgr_state_transitions () =
  let buf = Bytes.create 128 in
  let w = Writer.make buf in
  let state = Sgr_state.create () in

  (* 1. Initial: emits reset(0) + attributes *)
  Sgr_state.update state w ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default
    ~attrs:(Attr.pack Attr.bold) ~link:"";

  let out1 = Writer.slice w in
  (* Expect: Reset(0); FG(Red); Bold(1) *)
  (* Sequence: \x1b[0;38;2;255;0;0;1m *)
  check_seq "initial transition" "\x1b[0;38;2;255;0;0;1m" (Bytes.to_string out1);

  (* 2. No-op: update with exact same values *)
  let w2 = Writer.make buf in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default
    ~attrs:(Attr.pack Attr.bold) ~link:"";
  equal ~msg:"noop update empty" int 0 (Writer.len w2);

  (* 3. Delta: Remove Bold, keep color *)
  let w3 = Writer.make buf in
  Sgr_state.update state w3 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default
    ~attrs:0 ~link:"";

  (* No bold *)

  (* Expect: Reset(0); FG(Red) -- Bold is implicitly removed by Reset, need to
     restore Color *)
  let out3 = Bytes.to_string (Writer.slice w3) in
  check_seq "delta transition" "\x1b[0;38;2;255;0;0m" out3

let sgr_transparent_bg_resets () =
  let buf = Bytes.create 128 in
  let state = Sgr_state.create () in
  let w1 = Writer.make buf in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0)
    ~bg:(Color.of_rgb 0 0 255) ~attrs:0 ~link:"";
  check_seq "bg applied" "\x1b[0;38;2;255;0;0;48;2;0;0;255m"
    (Bytes.to_string (Writer.slice w1));
  let w2 = Writer.make buf in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default
    ~attrs:0 ~link:"";
  check_seq "bg cleared to default" "\x1b[0;38;2;255;0;0m"
    (Bytes.to_string (Writer.slice w2))

let sgr_transparent_fg_resets_to_default () =
  let buf = Bytes.create 128 in
  let state = Sgr_state.create () in
  let w1 = Writer.make buf in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default
    ~attrs:0 ~link:"";
  check_seq "fg applied" "\x1b[0;38;2;255;0;0m"
    (Bytes.to_string (Writer.slice w1));
  let w2 = Writer.make buf in
  Sgr_state.update state w2 ~fg:Color.default ~bg:Color.default ~attrs:0
    ~link:"";
  let out = Bytes.to_string (Writer.slice w2) in
  check_seq "fg cleared to default" "\x1b[0m" out;
  is_false ~msg:"default fg is not emitted as truecolor black"
    (String.contains out '3' && String.contains out '8')

(* --- 4. Parser Resilience --- *)

let parser_chunked_utf8 () =
  let p = Parser.create () in
  (* Euro sign: \xE2\x82\xAC *)

  (* Feed byte 1 *)
  let t1 = feed_to_list p (Bytes.of_string "\xE2") 0 1 in
  equal ~msg:"chunk 1 buffered" int 0 (List.length t1);

  (* Feed byte 2 *)
  let t2 = feed_to_list p (Bytes.of_string "\x82") 0 1 in
  equal ~msg:"chunk 2 buffered" int 0 (List.length t2);

  (* Feed byte 3 + extra text *)
  let t3 = feed_to_list p (Bytes.of_string "\xACok") 0 3 in
  let combined =
    match t3 with
    | [ Parser.Text s ] -> s
    | [ Parser.Text utf8; Parser.Text plain ] -> utf8 ^ plain
    | _ -> ""
  in
  equal ~msg:"utf8 reconstructed" string "€ok" combined

let parser_chunked_utf8_same_buffer_slice () =
  (* Parser feeds must respect the logical off/len slice, not bytes that happen
     to follow in the same underlying buffer. *)
  let p = Parser.create () in
  let input = Bytes.of_string "\xE2\x82\xACok" in
  let t1 = feed_to_list p input 0 1 in
  equal ~msg:"same buffer byte 1 buffered" int 0 (List.length t1);
  let t2 = feed_to_list p input 1 1 in
  equal ~msg:"same buffer byte 2 buffered" int 0 (List.length t2);
  let t3 = feed_to_list p input 2 3 in
  match t3 with
  | [ Parser.Text s ] ->
      equal ~msg:"same buffer utf8 reconstructed" string "€ok" s
  | _ -> fail "Expected reconstructed UTF-8 text token"

let parser_chunked_escape () =
  let p = Parser.create () in
  (* Sequence: \x1b[31m (Red) *)

  (* Feed partial CSI *)
  let t1 = feed_to_list p (Bytes.of_string "\x1b[3") 0 3 in
  equal ~msg:"partial csi buffered" int 0 (List.length t1);

  (* Finish it *)
  let t2 = feed_to_list p (Bytes.of_string "1m") 0 2 in
  match t2 with
  | [ Parser.SGR [ `Fg c ] ] ->
      equal ~msg:"parsed split csi" check_color Color.red c
  | _ -> fail "Failed to reconstruct split CSI"

let parser_malformed_input () =
  (* 1. Lone ESC at end of string *)
  let t1 = Parser.parse "foo\x1b" in
  (* Should probably ignore the lone ESC or treat as text depending on
     implementation details. The current implementation buffers it waiting for
     more. Since parse calls feed with 0 at end... *)
  match t1 with
  | [ Parser.Text "foo" ] -> () (* It was dropped/buffered as incomplete *)
  | _ -> fail "Lone ESC handling unexpected"

(* Regression test: 0xFE and 0xFF are never valid UTF-8 lead bytes. Previously,
   the parser incorrectly treated any byte >= 0xC0 as a potential multi-byte
   sequence start and would buffer it, causing subsequent bytes to be lost.
   Valid UTF-8 lead bytes are only 0xC2-0xF4. *)
let parser_invalid_utf8_lead_bytes () =
  let p = Parser.create () in
  (* Input: 'A', 0xFF (invalid), 0xFE (invalid), 'B', 'C' *)
  let input = Bytes.create 5 in
  Bytes.set input 0 'A';
  Bytes.set input 1 '\xff';
  Bytes.set input 2 '\xfe';
  Bytes.set input 3 'B';
  Bytes.set input 4 'C';

  let tokens = feed_to_list p input 0 5 in
  (* Flush any pending state *)
  let tokens = tokens @ feed_to_list p Bytes.empty 0 0 in

  match tokens with
  | [ Parser.Text s ] ->
      (* Should be: A + replacement + replacement + B + C *)
      (* U+FFFD in UTF-8 is \xEF\xBF\xBD *)
      let expected = "A\xEF\xBF\xBD\xEF\xBF\xBDBC" in
      equal ~msg:"invalid lead bytes produce replacements" string expected s
  | _ -> fail "Expected single text token with replacements"

let parser_max_length_overflow () =
  (* Construct a massive garbage sequence *)
  let huge_seq = "\x1b[" ^ String.make 300 '1' ^ "m" in
  let tokens = Parser.parse huge_seq in
  match tokens with
  | Parser.Control (Parser.Unknown msg) :: _ ->
      equal ~msg:"overflow detected" string "CSI_TOO_LONG" msg
  | _ -> fail "Parser should reject oversized sequences"

let parser_osc8_st_terminated () =
  let seq = "\x1b]8;;http://foo\x1b\\txt\x1b]8;;\x1b\\" in
  match Parser.parse seq with
  | [
   Parser.Control (Parser.Hyperlink (Some ([], link)));
   Parser.Text "txt";
   Parser.Control (Parser.Hyperlink None);
  ] ->
      equal ~msg:"parsed link url" string "http://foo" link
  | _ -> fail "OSC 8 ST tokens mismatch"

let parser_osc8_bel_terminated () =
  let seq = "\x1b]8;;http://foo\x07txt\x1b]8;;\x07" in
  match Parser.parse seq with
  | [
   Parser.Control (Parser.Hyperlink (Some ([], link)));
   Parser.Text "txt";
   Parser.Control (Parser.Hyperlink None);
  ] ->
      equal ~msg:"parsed link url bel" string "http://foo" link
  | _ -> fail "OSC 8 BEL tokens mismatch"

let parser_protocol_string_controls () =
  (match Parser.parse "\x1bP>|kitty(0.40.1)\x1b\\" with
  | [ Parser.Control (Parser.DCS ">|kitty(0.40.1)") ] -> ()
  | _ -> fail "DCS response not tokenized");
  (match Parser.parse "\x1b_Gi=1;OK\x1b\\" with
  | [ Parser.Control (Parser.APC "Gi=1;OK") ] -> ()
  | _ -> fail "APC response not tokenized");
  (match Parser.parse "\x1b^pm-data\x1b\\" with
  | [ Parser.Control (Parser.PM "pm-data") ] -> ()
  | _ -> fail "PM response not tokenized");
  match Parser.parse "\x1bXsos-data\x1b\\" with
  | [ Parser.Control (Parser.SOS "sos-data") ] -> ()
  | _ -> fail "SOS response not tokenized"

let parser_protocol_string_controls_chunked () =
  let p = Parser.create () in
  let t1 = feed_to_list p (Bytes.of_string "\x1bP>|kitty") 0 9 in
  equal ~msg:"partial DCS buffered" int 0 (List.length t1);
  let t2 = feed_to_list p (Bytes.of_string "(0.40.1)\x1b") 0 9 in
  equal ~msg:"DCS split ESC buffered" int 0 (List.length t2);
  match feed_to_list p (Bytes.of_string "\\") 0 1 with
  | [ Parser.Control (Parser.DCS ">|kitty(0.40.1)") ] -> ()
  | _ -> fail "split DCS terminator not reconstructed"

let parser_csi_empty_parameter_defaults () =
  (match Parser.parse "\x1b[;5H" with
  | [ Parser.Control (Parser.CUP (1, 5)) ] -> ()
  | _ -> fail "CUP omitted row should default to 1");
  (match Parser.parse "\x1b[5;H" with
  | [ Parser.Control (Parser.CUP (5, 1)) ] -> ()
  | _ -> fail "CUP omitted col should default to 1");
  (match Parser.parse "\x1b[H" with
  | [ Parser.Control (Parser.CUP (1, 1)) ] -> ()
  | _ -> fail "CUP empty params should default to 1;1");
  (match Parser.parse "\x1b[A" with
  | [ Parser.Control (Parser.CUU 1) ] -> ()
  | _ -> fail "CUU omitted count should default to 1");
  match Parser.parse "\x1b[;31m" with
  | [ Parser.SGR [ `Reset; `Fg c ] ] ->
      equal ~msg:"SGR empty param fg" check_color Color.red c
  | _ -> fail "SGR empty param should remain reset then fg"

(* --- 5. Writer & Low-Level Escape --- *)

let writer_utf8_encoding () =
  let buf = Bytes.create 16 in
  let w = Writer.make buf in

  (* 1 byte: A *)
  utf8 (Char.code 'A') w;
  (* 3 byte: € (0x20AC) *)
  utf8 0x20AC w;
  (* 4 byte: 🚀 (Rocket 0x1F680) *)
  utf8 0x1F680 w;

  let out = Bytes.to_string (Writer.slice w) in
  equal ~msg:"utf8 encoding" string "A€🚀" out

let writer_hyperlink_params () =
  let buf = Bytes.create 64 in
  let w = Writer.make buf in
  hyperlink_start ~params:"id=123" ~url:"http://foo" w;
  let out = Bytes.to_string (Writer.slice w) in
  (* OSC 8 ; params ; url ST (\x1b\\) *)
  check_seq "hyperlink with params" "\x1b]8;id=123;http://foo\x1b\\" out

let writer_invalid_scalar_replacement () =
  let buf = Bytes.create 8 in
  let w = Writer.make buf in
  (* Surrogate code point should be clamped to U+FFFD *)
  utf8 0xD800 w;
  check_seq "invalid scalar -> replacement" "\xef\xbf\xbd"
    (Bytes.to_string (Writer.slice w))

let writer_checked_bounds () =
  let w = Writer.make (Bytes.create 1) in
  Writer.write_char w 'a';
  (try
     Writer.write_char w 'b';
     fail "expected write_char overflow"
   with Invalid_argument _ -> ());
  let w = Writer.make (Bytes.create 4) in
  try
    Writer.write_subbytes w (Bytes.of_string "abc") 2 2;
    fail "expected invalid source slice"
  with Invalid_argument _ -> ()

(* --- 6. High Level API --- *)

let strip_edge_cases () =
  (* Standard *)
  equal ~msg:"strip standard" string "foo" (Ansi.strip "\x1b[31mfoo\x1b[0m");
  (* Incomplete at end *)
  equal ~msg:"strip incomplete" string "foo" (Ansi.strip "foo\x1b[");
  (* Lone ESC *)
  equal ~msg:"strip lone esc" string "foo" (Ansi.strip "foo\x1b");
  (* OSC title *)
  equal ~msg:"strip osc" string "foo" (Ansi.strip "\x1b]0;Title\007foo")

let strip_protocol_string_controls () =
  equal ~msg:"strip DCS" string "ok" (Ansi.strip "\x1bP>|kitty(0.40.1)\x1b\\ok");
  equal ~msg:"strip APC" string "ok" (Ansi.strip "\x1b_Gi=1;OK\x1b\\ok")

let parameter_clamping () =
  (* Cursor movement: negative -> 0 -> empty string *)
  check_seq "negative lines" "" Ansi.(to_string (cursor_up ~n:(-5)));
  check_seq "zero lines" "" Ansi.(to_string (cursor_up ~n:0));
  (* Absolute position: negative/zero -> 1 *)
  check_seq "clamp row/col" "\x1b[1;1H"
    Ansi.(to_string (cursor_position ~row:0 ~col:(-5)))

let explicit_error_handling () =
  try
    let buf = Bytes.create 16 in
    let w = Writer.make buf in
    (set_scrolling_region ~top:10 ~bottom:5) w;
    fail "scrolling region validation failed"
  with Invalid_argument _ -> ()

let cursor_and_explicit_width_sequences () =
  check_seq "cursor color osc12" "\x1b]12;#010203\x07"
    Ansi.(to_string (cursor_color ~r:1 ~g:2 ~b:3));
  check_seq "explicit width osc66" "\x1b]66;w=5;hi\x1b\\"
    Ansi.(to_string (explicit_width ~width:5 ~text:"hi"))

let kitty_keyboard_sequences () =
  check_seq "kitty keyboard push" "\x1b[>5u"
    Ansi.(to_string (csi_u_push ~flags:5));
  check_seq "kitty keyboard pop" "\x1b[<u" Ansi.(to_string csi_u_pop)

let hyperlink_lifecycle () =
  let link_style = Style.hyperlink "http://foo" Style.default in
  let rendered = Ansi.render [ (link_style, "hi") ] in
  (* Minimal SGR: no SGR emitted when only link changes, just OSC 8 + text + end
     + reset *)
  check_seq "osc8 opened/closed with reset"
    "\x1b]8;;http://foo\x1b\\hi\x1b]8;;\x1b\\\x1b[0m" rendered;
  (* Test that hyperlinks are suppressed when disabled *)
  let rendered_no_links =
    Ansi.render ~hyperlinks_enabled:false [ (link_style, "hi") ]
  in
  (* No OSC8 sequences, just text + reset *)
  check_seq "osc8 suppressed when disabled" "hi\x1b[0m" rendered_no_links

(* --- 7. Self Round-Trip Tests --- *)

(* Verify parser recognizes sequences the library emits *)
let roundtrip_cursor_sequences () =
  (* Test cursor movement sequences *)
  let test_cursor name seq expected_control =
    let tokens = Parser.parse (to_string seq) in
    match tokens with
    | [ Parser.Control c ] -> is_true ~msg:name (c = expected_control)
    | _ -> fail (Printf.sprintf "%s: unexpected tokens" name)
  in
  test_cursor "cursor up" (cursor_up ~n:5) (Parser.CUU 5);
  test_cursor "cursor down" (cursor_down ~n:3) (Parser.CUD 3);
  test_cursor "cursor forward" (cursor_forward ~n:10) (Parser.CUF 10);
  test_cursor "cursor back" (cursor_back ~n:2) (Parser.CUB 2);
  test_cursor "cursor position"
    (cursor_position ~row:10 ~col:20)
    (Parser.CUP (10, 20));
  test_cursor "cursor next line" (cursor_next_line ~n:3) (Parser.CNL 3);
  test_cursor "cursor prev line" (cursor_previous_line ~n:2) (Parser.CPL 2);
  test_cursor "cursor horiz abs" (cursor_horizontal_absolute 15) (Parser.CHA 15);
  test_cursor "cursor vert abs" (cursor_vertical_absolute 8) (Parser.VPA 8);
  (* Test cursor save/restore (CSI s/u) *)
  test_cursor "cursor save" cursor_save Parser.DECSC;
  test_cursor "cursor restore" cursor_restore Parser.DECRC

let roundtrip_screen_sequences () =
  let test_screen name seq expected_control =
    let tokens = Parser.parse (to_string seq) in
    match tokens with
    | [ Parser.Control c ] -> is_true ~msg:name (c = expected_control)
    | _ -> fail (Printf.sprintf "%s: unexpected tokens" name)
  in
  test_screen "erase display 0" (erase_display ~mode:`Below) (Parser.ED 0);
  test_screen "erase display 2" (erase_display ~mode:`All) (Parser.ED 2);
  test_screen "erase line 0" (erase_line ~mode:`Right) (Parser.EL 0);
  test_screen "erase line 2" (erase_line ~mode:`All) (Parser.EL 2);
  test_screen "insert lines" (insert_lines ~n:5) (Parser.IL 5);
  test_screen "delete lines" (delete_lines ~n:3) (Parser.DL 3)

let roundtrip_sgr_sequences () =
  (* Test that SGR sequences round-trip through parser *)
  let test_sgr name seq check_fn =
    let tokens = Parser.parse (to_string seq) in
    match tokens with
    | [ Parser.SGR attrs ] -> check_fn attrs
    | _ -> fail (Printf.sprintf "%s: expected SGR token" name)
  in
  (* Reset *)
  test_sgr "sgr reset" reset (fun attrs ->
      is_true ~msg:"has reset" (List.mem `Reset attrs));
  (* Bold via sgr [1] *)
  test_sgr "sgr bold" (sgr [ 1 ]) (fun attrs ->
      is_true ~msg:"has bold" (List.mem `Bold attrs));
  (* Truecolor foreground via sgr [38;2;r;g;b] *)
  test_sgr "sgr truecolor fg"
    (sgr [ 38; 2; 100; 150; 200 ])
    (fun attrs ->
      match attrs with
      | [ `Fg c ] ->
          let r, g, b = Color.to_rgb c in
          equal ~msg:"r" int 100 r;
          equal ~msg:"g" int 150 g;
          equal ~msg:"b" int 200 b
      | _ -> fail "expected fg color")

let roundtrip_hyperlink_sequences () =
  (* Test hyperlink sequences *)
  let test_link name seq check_fn =
    let tokens = Parser.parse (to_string seq) in
    match tokens with
    | [ Parser.Control (Parser.Hyperlink h) ] -> check_fn h
    | _ -> fail (Printf.sprintf "%s: expected hyperlink control" name)
  in
  (* Open hyperlink *)
  test_link "hyperlink open" (hyperlink_start ~url:"http://test.com") (fun h ->
      match h with
      | Some ([], url) -> equal ~msg:"url" string "http://test.com" url
      | _ -> fail "expected open hyperlink");
  (* Close hyperlink *)
  test_link "hyperlink close" hyperlink_end (fun h ->
      is_true ~msg:"is close" (h = None))

let roundtrip_strip_removes_all () =
  (* Verify strip removes all sequences we emit *)
  let test_strip name input expected =
    let stripped = Ansi.strip input in
    equal ~msg:name string expected stripped
  in
  let s = Ansi.to_string in
  (* Cursor sequences *)
  test_strip "strip cursor up" (s (cursor_up ~n:5) ^ "text") "text";
  test_strip "strip cursor position"
    (s (cursor_position ~row:10 ~col:20) ^ "text")
    "text";
  (* SGR sequences *)
  test_strip "strip sgr"
    (Style.styled (Style.make ~fg:Color.red ~bold:true ()) "text")
    "text";
  test_strip "strip reset" (s reset ^ "text") "text";
  (* Screen control *)
  test_strip "strip clear" (s clear ^ "text") "text";
  test_strip "strip erase display"
    (s (erase_display ~mode:`All) ^ "text")
    "text";
  (* Hyperlinks *)
  test_strip "strip hyperlink"
    (s (hyperlink ~url:"http://test" ~text:"link"))
    "link";
  (* Title *)
  test_strip "strip title" (s (set_title ~title:"test") ^ "text") "text";
  (* Alternate screen *)
  test_strip "strip alt screen"
    (s (enable Alternate_screen) ^ "text" ^ s (disable Alternate_screen))
    "text";
  (* Mouse modes *)
  test_strip "strip mouse on"
    (s (enable Mouse_button_tracking)
    ^ "text"
    ^ s (disable Mouse_button_tracking))
    "text"

let tests =
  [
    group "Color Logic"
      [
        test "hex parsing" color_parsing_hex;
        test "hsl roundtrip" color_hsl_roundtrip;
        test "blending" color_blending;
        test "intent identity" color_intent_is_part_of_identity;
        test "ansi16 fallback palette"
          color_ansi16_palette_matches_standard_fallback;
        test "downgrade" color_downgrade_semantics;
        test "extended out of range safe" color_extended_out_of_range_is_safe;
      ];
    group "Attributes & Style"
      [
        test "bitmask integrity" attr_bitmask_integrity;
        test "style composition" style_composition_and_hash;
        test "extended attr ordering" attr_extended_ordering;
        test "shared disable codes" style_shared_disable_codes;
        test "emit function" style_emit_function;
        test "resolve multiple" style_resolve_multiple;
        test "compare and hash" style_compare_and_hash;
      ];
    group "SGR Engine"
      [
        test "state diffing" sgr_state_transitions;
        test "transparent bg reset" sgr_transparent_bg_resets;
        test "transparent fg reset" sgr_transparent_fg_resets_to_default;
      ];
    group "Parser"
      [
        test "chunked utf8" parser_chunked_utf8;
        test "chunked utf8 same buffer slice"
          parser_chunked_utf8_same_buffer_slice;
        test "chunked escape" parser_chunked_escape;
        test "malformed inputs" parser_malformed_input;
        test "invalid utf8 lead bytes" parser_invalid_utf8_lead_bytes;
        test "overflow protection" parser_max_length_overflow;
        test "osc8 st terminated" parser_osc8_st_terminated;
        test "osc8 bel terminated" parser_osc8_bel_terminated;
        test "protocol string controls" parser_protocol_string_controls;
        test "chunked protocol string controls"
          parser_protocol_string_controls_chunked;
        test "csi empty parameter defaults" parser_csi_empty_parameter_defaults;
      ];
    group "Writer & Seq"
      [
        test "utf8 encoding" writer_utf8_encoding;
        test "hyperlink params" writer_hyperlink_params;
        test "invalid scalar replacement" writer_invalid_scalar_replacement;
        test "checked bounds" writer_checked_bounds;
      ];
    group "High Level API"
      [
        test "strip semantics" strip_edge_cases;
        test "strip protocol string controls" strip_protocol_string_controls;
        test "param clamping" parameter_clamping;
        test "input validation" explicit_error_handling;
        test "cursor & explicit width" cursor_and_explicit_width_sequences;
        test "kitty keyboard sequences" kitty_keyboard_sequences;
        test "hyperlink lifecycle" hyperlink_lifecycle;
      ];
    group "Round-Trip"
      [
        test "cursor sequences" roundtrip_cursor_sequences;
        test "screen sequences" roundtrip_screen_sequences;
        test "sgr sequences" roundtrip_sgr_sequences;
        test "hyperlink sequences" roundtrip_hyperlink_sequences;
        test "strip removes all" roundtrip_strip_removes_all;
      ];
  ]

let () = run "matrix.ansi" tests
