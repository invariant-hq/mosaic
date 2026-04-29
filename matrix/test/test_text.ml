open Windtrap
open Text

let check_width msg expected actual = equal ~msg int expected actual

let expect_invalid_arg msg f =
  try
    f ();
    fail (msg ^ ": expected Invalid_argument")
  with Invalid_argument _ -> ()

let uchar_to_utf8 cp =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
  Buffer.contents buf

let grapheme_break_test_path () =
  let here = Filename.dirname Sys.argv.(0) in
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_root parent
  in
  let root = Option.value (find_root (Sys.getcwd ())) ~default:here in
  let candidates =
    [
      Filename.concat here "data/GraphemeBreakTest.txt";
      Filename.concat root "matrix/test/data/GraphemeBreakTest.txt";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> fail "Could not find data/GraphemeBreakTest.txt"

let run_grapheme_conformance () =
  let path = grapheme_break_test_path () in
  let ic = open_in path in
  try
    let rec loop lineno =
      match input_line ic with
      | exception End_of_file -> ()
      | line ->
          let rule =
            match Stdlib.String.index_opt line '#' with
            | Some i -> Stdlib.String.sub line 0 i |> Stdlib.String.trim
            | None -> Stdlib.String.trim line
          in
          if rule <> "" then (
            let parts =
              Stdlib.String.split_on_char ' ' rule |> List.filter (( <> ) "")
            in
            let full_text = Buffer.create 32 in
            let expected_segments = ref [] in
            let current_segment = Buffer.create 16 in
            let rec parse = function
              | "÷" :: rest ->
                  if Buffer.length current_segment > 0 then (
                    expected_segments :=
                      Buffer.contents current_segment :: !expected_segments;
                    Buffer.clear current_segment);
                  parse rest
              | "×" :: rest -> parse rest
              | hex :: rest ->
                  let s = uchar_to_utf8 (int_of_string ("0x" ^ hex)) in
                  Buffer.add_string full_text s;
                  Buffer.add_string current_segment s;
                  parse rest
              | [] ->
                  if Buffer.length current_segment > 0 then
                    expected_segments :=
                      Buffer.contents current_segment :: !expected_segments
            in
            parse parts;
            let text = Buffer.contents full_text in
            let expected = List.rev !expected_segments in
            let actual = ref [] in
            iter_graphemes
              (fun ~offset:off ~len ->
                actual := Stdlib.String.sub text off len :: !actual)
              text;
            let actual = List.rev !actual in
            if expected <> actual then
              fail
                (Printf.sprintf "Grapheme break mismatch on line %d: %s" lineno
                   rule));
          loop (lineno + 1)
    in
    loop 1;
    close_in ic
  with exn ->
    close_in_noerr ic;
    raise exn

let measurement_semantics () =
  let cases =
    [
      ("basic ASCII", "abc", `Unicode, 8, 3);
      ("ASCII controls", "a\n\r\x00b", `Unicode, 8, 2);
      ("tab default", "\t", `Unicode, 2, 2);
      ("tab custom", "\t", `Unicode, 4, 4);
      ("emoji simple", "👋", `Unicode, 8, 2);
      ("emoji ZWJ sequence", "👩\u{200D}🚀", `Unicode, 8, 2);
      ("emoji family", "👨\u{200D}👩\u{200D}👧\u{200D}👦", `Unicode, 8, 2);
      ("regional indicator pair", "🇺🇸", `Unicode, 8, 2);
      ("skin tone sequence", "👍🏽", `Unicode, 8, 2);
      ("no_zwj strategy", "👩\u{200D}🚀", `No_zwj, 8, 4);
      ("wcwidth strategy", "👩\u{200D}🚀", `Wcwidth, 8, 4);
      ("combining accent", "a\u{0301}", `Unicode, 8, 1);
      ("zero width space", "\u{200B}", `Unicode, 8, 0);
      ("C1 control", uchar_to_utf8 0x008A, `Unicode, 8, 0);
    ]
  in
  List.iter
    (fun (name, input, width_method, tab_width, expected) ->
      check_width name expected (measure ~width_method ~tab_width input))
    cases

let ascii_fast_path_consistency () =
  let s = "hello\tworld" in
  let unicode_w = measure ~width_method:`Unicode ~tab_width:4 s in
  let no_zwj_w = measure ~width_method:`No_zwj ~tab_width:4 s in
  let wcwidth_w = measure ~width_method:`Wcwidth ~tab_width:4 s in
  check_width "unicode vs no_zwj" unicode_w no_zwj_w;
  check_width "unicode vs wcwidth" unicode_w wcwidth_w

let measure_multi_grapheme_regression () =
  check_width "ASCII + wide chars" 10
    (measure ~width_method:`Unicode ~tab_width:2 "Hello 世界");
  check_width "three wide chars" 6
    (measure ~width_method:`Unicode ~tab_width:2 "日本語");
  check_width "emoji + ASCII + wide" 6
    (measure ~width_method:`Unicode ~tab_width:2 "🚀Hi世");
  check_width "unicode matches wcwidth for simple case" 10
    (measure ~width_method:`Wcwidth ~tab_width:2 "Hello 世界")

let malformed_utf8_measurement () =
  let invalid = "\xC3" in
  check_width "truncated sequence" 1
    (measure ~width_method:`Unicode ~tab_width:2 invalid);
  check_width "invalid middle byte" 3
    (measure ~width_method:`Unicode ~tab_width:2 ("a" ^ "\x80" ^ "b"));
  check_width "measure_sub invalid slice" 2
    (measure_sub ~width_method:`Unicode ~tab_width:2
       ("x" ^ invalid ^ "(")
       ~pos:1 ~len:2)

let measure_sub_validation () =
  check_width "sub ASCII" 3
    (measure_sub ~width_method:`Unicode ~tab_width:2 "xxabcxx" ~pos:2 ~len:3);
  check_width "sub non-positive length" 0
    (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:(-1) ~len:0);
  expect_invalid_arg "negative pos" (fun () ->
      ignore
        (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:(-1) ~len:1));
  expect_invalid_arg "oversized len" (fun () ->
      ignore
        (measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:1 ~len:3))

let grapheme_iteration () =
  let zwj_seq = "👩\u{200D}🚀" in
  let count_default = ref 0 in
  let count_no_zwj = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr count_default) zwj_seq;
  iter_graphemes ~ignore_zwj:true
    (fun ~offset:_ ~len:_ -> incr count_no_zwj)
    zwj_seq;
  equal ~msg:"default: ZWJ sequence is one grapheme" int 1 !count_default;
  equal ~msg:"ignore_zwj: ZWJ sequence splits" int 2 !count_no_zwj;
  equal ~msg:"grapheme_count" int 3 (grapheme_count ("a" ^ zwj_seq ^ "b"))

let grapheme_info_skips_zero_width () =
  let seen = ref [] in
  iter_grapheme_info ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    ("a" ^ "\u{200B}" ^ "界");
  equal ~msg:"zero-width grapheme is skipped"
    (list (triple int int int))
    [ (0, 1, 1); (4, 3, 2) ]
    (List.rev !seen)

type wrap_break = { byte_offset : int; grapheme_offset : int }
type line_break = { pos : int; kind : line_break_kind }

let wrap_breaks ?(width_method = `Unicode) s =
  let acc = ref [] in
  iter_wrap_breaks ~width_method
    (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset ->
      acc := { byte_offset = next_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let wrap_break_points ?(width_method = `Unicode) s =
  let acc = ref [] in
  iter_wrap_breaks ~width_method
    (fun ~break_byte_offset ~next_byte_offset:_ ~grapheme_offset ->
      acc := { byte_offset = break_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let line_breaks_to_list s =
  let acc = ref [] in
  iter_line_breaks (fun ~pos ~kind -> acc := { pos; kind } :: !acc) s;
  List.rev !acc

let wrap_break_testable =
  Testable.make
    ~pp:(fun ppf { byte_offset; grapheme_offset } ->
      Format.fprintf ppf "{byte=%d; grapheme=%d}" byte_offset grapheme_offset)
    ~equal:( = ) ()

let line_break_testable =
  Testable.make
    ~pp:(fun ppf { pos; kind } ->
      let kind_s =
        match kind with `LF -> "LF" | `CR -> "CR" | `CRLF -> "CRLF"
      in
      Format.fprintf ppf "{pos=%d; kind=%s}" pos kind_s)
    ~equal:( = ) ()

let wrap_breaks_ascii_spaces () =
  let breaks = wrap_breaks "hello world test" in
  equal ~msg:"two breaks for two spaces" int 2 (Array.length breaks);
  equal ~msg:"first break after 'hello '" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 5 }
    breaks.(0);
  equal ~msg:"second break after 'world '" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 11 }
    breaks.(1)

let wrap_breaks_ascii_punctuation () =
  let breaks = wrap_breaks "a-b/c.d,e;f:g!h?i" in
  equal ~msg:"8 breaks for 8 punctuation marks" int 8 (Array.length breaks);
  equal ~msg:"break after hyphen" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_ascii_brackets () =
  let breaks = wrap_breaks "(a)[b]{c}" in
  equal ~msg:"6 breaks for 6 brackets" int 6 (Array.length breaks)

let wrap_breaks_tabs () =
  let breaks = wrap_breaks "a\tb\tc" in
  equal ~msg:"2 breaks for 2 tabs" int 2 (Array.length breaks);
  equal ~msg:"break after first tab" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_unicode_spaces () =
  let nbsp = "\xC2\xA0" in
  let zwsp = "\xE2\x80\x8B" in
  let ideographic = "\xE3\x80\x80" in
  equal ~msg:"break after NBSP" int 1
    (Array.length (wrap_breaks ("a" ^ nbsp ^ "b")));
  equal ~msg:"break after ZWSP" int 1
    (Array.length (wrap_breaks ("a" ^ zwsp ^ "b")));
  equal ~msg:"break after ideographic space" int 1
    (Array.length (wrap_breaks ("a" ^ ideographic ^ "b")))

let wrap_breaks_soft_hyphen () =
  let soft_hyphen = "\xC2\xAD" in
  equal ~msg:"break after soft hyphen" int 1
    (Array.length (wrap_breaks ("word" ^ soft_hyphen ^ "break")))

let wrap_breaks_en_space_range () =
  let en_quad = "\xE2\x80\x80" in
  let hair_space = "\xE2\x80\x8A" in
  equal ~msg:"break after EN QUAD" int 1
    (Array.length (wrap_breaks ("a" ^ en_quad ^ "b")));
  equal ~msg:"break after HAIR SPACE" int 1
    (Array.length (wrap_breaks ("a" ^ hair_space ^ "b")))

let wrap_breaks_no_break_in_plain_text () =
  equal ~msg:"no breaks in continuous text" int 0
    (Array.length (wrap_breaks "helloworld"))

let wrap_breaks_grapheme_aware () =
  let emoji = "👍🏽" in
  let breaks = wrap_breaks ("a " ^ emoji ^ " b") in
  equal ~msg:"breaks respect grapheme boundaries" int 2 (Array.length breaks);
  equal ~msg:"first break" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_empty_string () =
  equal ~msg:"no breaks in empty string" int 0 (Array.length (wrap_breaks ""))

let wrap_breaks_width_method_no_zwj () =
  let zwj_seq = "👩\u{200D}🚀" in
  equal ~msg:"unicode: no breaks in ZWJ sequence" int 0
    (Array.length (wrap_breaks ~width_method:`Unicode zwj_seq));
  equal ~msg:"no_zwj: still no breaks in ZWJ sequence" int 0
    (Array.length (wrap_breaks ~width_method:`No_zwj zwj_seq))

let wrap_break_points_ascii_space () =
  let points = wrap_break_points "a b" in
  let resumes = wrap_breaks "a b" in
  equal ~msg:"one break point for one ASCII space" int 1 (Array.length points);
  equal ~msg:"point offset at space grapheme" wrap_break_testable
    { byte_offset = 1; grapheme_offset = 1 }
    points.(0);
  equal ~msg:"resume offset starts next grapheme" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    resumes.(0)

let wrap_break_points_unicode_space () =
  let nbsp = "\xC2\xA0" in
  let s = "a" ^ nbsp ^ "b" in
  let points = wrap_break_points s in
  let resumes = wrap_breaks s in
  equal ~msg:"one break point for one NBSP" int 1 (Array.length points);
  equal ~msg:"point offset at NBSP grapheme start" wrap_break_testable
    { byte_offset = 1; grapheme_offset = 1 }
    points.(0);
  equal ~msg:"resume offset after NBSP grapheme" wrap_break_testable
    { byte_offset = 3; grapheme_offset = 1 }
    resumes.(0)

let wrap_breaks_mixed_cjk_ascii_transitions () =
  let cjk_ascii = "日本語abc" in
  let cjk_points = wrap_break_points cjk_ascii in
  let cjk_resumes = wrap_breaks cjk_ascii in
  equal ~msg:"CJK->ASCII one break" int 1 (Array.length cjk_points);
  equal ~msg:"CJK->ASCII point" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 2 }
    cjk_points.(0);
  equal ~msg:"CJK->ASCII resume" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 2 }
    cjk_resumes.(0);
  let ascii_cjk = "abc日本語" in
  let ascii_points = wrap_break_points ascii_cjk in
  let ascii_resumes = wrap_breaks ascii_cjk in
  equal ~msg:"ASCII->CJK one break" int 1 (Array.length ascii_points);
  equal ~msg:"ASCII->CJK point" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 2 }
    ascii_points.(0);
  equal ~msg:"ASCII->CJK resume" wrap_break_testable
    { byte_offset = 3; grapheme_offset = 2 }
    ascii_resumes.(0)

let wrap_breaks_fullwidth_cjk_punctuation () =
  let ideographic_full_stop = "日本語。abc" in
  let points = wrap_break_points ideographic_full_stop in
  let resumes = wrap_breaks ideographic_full_stop in
  equal ~msg:"CJK punctuation one break" int 1 (Array.length points);
  equal ~msg:"CJK punctuation point" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 3 }
    points.(0);
  equal ~msg:"CJK punctuation resume" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 3 }
    resumes.(0);
  let check_one label cp =
    let s = "a" ^ uchar_to_utf8 cp ^ "b" in
    equal ~msg:label int 1 (Array.length (wrap_breaks s))
  in
  check_one "ideographic comma" 0x3001;
  check_one "fullwidth exclamation" 0xFF01;
  check_one "fullwidth question" 0xFF1F

let line_breaks_lf () =
  let breaks = line_breaks_to_list "a\nb\nc" in
  equal ~msg:"two LF breaks" int 2 (List.length breaks);
  equal ~msg:"first LF" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"second LF" line_break_testable { pos = 3; kind = `LF }
    (List.nth breaks 1)

let line_breaks_cr () =
  let breaks = line_breaks_to_list "a\rb\rc" in
  equal ~msg:"two CR breaks" int 2 (List.length breaks);
  equal ~msg:"first CR" line_break_testable { pos = 1; kind = `CR }
    (List.nth breaks 0);
  equal ~msg:"second CR" line_break_testable { pos = 3; kind = `CR }
    (List.nth breaks 1)

let line_breaks_crlf () =
  let breaks = line_breaks_to_list "a\r\nb\r\nc" in
  equal ~msg:"two CRLF breaks" int 2 (List.length breaks);
  equal ~msg:"first CRLF" line_break_testable { pos = 2; kind = `CRLF }
    (List.nth breaks 0);
  equal ~msg:"second CRLF" line_break_testable { pos = 5; kind = `CRLF }
    (List.nth breaks 1)

let line_breaks_mixed () =
  let breaks = line_breaks_to_list "a\nb\r\nc\rd" in
  equal ~msg:"three mixed breaks" int 3 (List.length breaks);
  equal ~msg:"LF" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"CRLF" line_break_testable { pos = 4; kind = `CRLF }
    (List.nth breaks 1);
  equal ~msg:"CR" line_break_testable { pos = 6; kind = `CR }
    (List.nth breaks 2)

let line_breaks_empty () =
  equal ~msg:"no breaks in empty" int 0 (List.length (line_breaks_to_list ""))

let line_breaks_no_newlines () =
  equal ~msg:"no breaks without newlines" int 0
    (List.length (line_breaks_to_list "hello world"))

let line_breaks_consecutive_lf () =
  let breaks = line_breaks_to_list "a\n\n\nb" in
  equal ~msg:"three consecutive LF" int 3 (List.length breaks);
  equal ~msg:"first" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"second" line_break_testable { pos = 2; kind = `LF }
    (List.nth breaks 1);
  equal ~msg:"third" line_break_testable { pos = 3; kind = `LF }
    (List.nth breaks 2)

let emoji_presentation_widths () =
  let check label cp expected_w =
    check_width label expected_w
      (measure ~width_method:`Unicode ~tab_width:2 (uchar_to_utf8 cp))
  in
  check "CJK U+4E2D" 0x4E2D 2;
  check "CJK U+3000 ideographic space" 0x3000 2;
  check "watch U+231A" 0x231A 2;
  check "hourglass U+231B" 0x231B 2;
  check "check mark U+2705" 0x2705 2;
  check "raised fist U+270A" 0x270A 2;
  check "sparkles U+2728" 0x2728 2;
  check "cross mark U+274C" 0x274C 2;
  check "question marks U+2753" 0x2753 2;
  check "exclamation U+2757" 0x2757 2;
  check "plus U+2795" 0x2795 2;
  check "rainbow U+1F308" 0x1F308 2;
  check "rocket U+1F680" 0x1F680 2;
  check "wave U+1F44B" 0x1F44B 2;
  check "thumbs up U+1F44D" 0x1F44D 2;
  check "face with tears U+1F602" 0x1F602 2;
  check "red heart U+2764" 0x2764 2;
  check "red heart + VS16 base" 0x2764 2;
  let heart_vs16 = uchar_to_utf8 0x2764 ^ uchar_to_utf8 0xFE0F in
  check_width "heart with VS16 = width 2" 2
    (measure ~width_method:`Unicode ~tab_width:2 heart_vs16);
  let flag = uchar_to_utf8 0x1F1FA ^ uchar_to_utf8 0x1F1F8 in
  check_width "flag pair = width 2" 2
    (measure ~width_method:`Unicode ~tab_width:2 flag);
  check "umbrella U+2614" 0x2614 2;
  check "hot beverage U+2615" 0x2615 2;
  check "white medium square U+25FD" 0x25FD 2;
  check "mahjong U+1F004" 0x1F004 2;
  check "playing card U+1F0CF" 0x1F0CF 2;
  check "skin tone U+1F3FD" 0x1F3FD 2;
  check "pizza U+1F355" 0x1F355 2;
  check "dog face U+1F436" 0x1F436 2;
  check "melting face U+1FAE0" 0x1FAE0 2

let c1_control_widths () =
  let check_zero label cp =
    check_width label 0
      (measure ~width_method:`Unicode ~tab_width:2 (uchar_to_utf8 cp))
  in
  check_zero "C1 U+0080" 0x0080;
  check_zero "C1 U+008A" 0x008A;
  check_zero "C1 U+009F" 0x009F

let () =
  run "matrix.text"
    [
      group "Conformance"
        [ test "UAX #29 grapheme boundaries" run_grapheme_conformance ];
      group "Measurement"
        [
          test "semantics" measurement_semantics;
          test "ASCII fast path" ascii_fast_path_consistency;
          test "multi-grapheme regression" measure_multi_grapheme_regression;
          test "malformed UTF-8" malformed_utf8_measurement;
          test "measure_sub validation" measure_sub_validation;
        ];
      group "Graphemes"
        [
          test "iteration" grapheme_iteration;
          test "info skips zero-width" grapheme_info_skips_zero_width;
        ];
      group "Wrap Breaks"
        [
          test "ASCII spaces" wrap_breaks_ascii_spaces;
          test "ASCII punctuation" wrap_breaks_ascii_punctuation;
          test "ASCII brackets" wrap_breaks_ascii_brackets;
          test "tabs" wrap_breaks_tabs;
          test "Unicode spaces" wrap_breaks_unicode_spaces;
          test "soft hyphen" wrap_breaks_soft_hyphen;
          test "EN space range" wrap_breaks_en_space_range;
          test "no break in plain text" wrap_breaks_no_break_in_plain_text;
          test "grapheme aware" wrap_breaks_grapheme_aware;
          test "empty string" wrap_breaks_empty_string;
          test "width_method No_zwj" wrap_breaks_width_method_no_zwj;
          test "ASCII space break point" wrap_break_points_ascii_space;
          test "Unicode NBSP break point" wrap_break_points_unicode_space;
          test "mixed CJK/ASCII transitions"
            wrap_breaks_mixed_cjk_ascii_transitions;
          test "fullwidth CJK punctuation" wrap_breaks_fullwidth_cjk_punctuation;
        ];
      group "Line Breaks"
        [
          test "LF" line_breaks_lf;
          test "CR" line_breaks_cr;
          test "CRLF" line_breaks_crlf;
          test "mixed" line_breaks_mixed;
          test "empty" line_breaks_empty;
          test "no newlines" line_breaks_no_newlines;
          test "consecutive LF" line_breaks_consecutive_lf;
        ];
      group "Width Tables"
        [
          test "emoji presentation widths" emoji_presentation_widths;
          test "C1 control widths" c1_control_widths;
        ];
    ]
