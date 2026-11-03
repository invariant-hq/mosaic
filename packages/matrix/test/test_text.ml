open Windtrap
open Matrix_text

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

let data_path name =
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
      Filename.concat here ("data/" ^ name);
      Filename.concat root ("packages/matrix/test/data/" ^ name);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> fail ("Could not find data/" ^ name)

let run_grapheme_conformance () =
  let path = data_path "GraphemeBreakTest.txt" in
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

(* One test per row: a failure names the row it came from, and a single
   character class can be selected with -f. *)
let measurement_rows =
  [
    ("wide Malayalam vowel", "രി", `Unicode_wide, 2, 2);
    ("wide Malayalam conjunct", "ക്ക", `Unicode_wide, 2, 2);
    ( "wide Malayalam report",
      "OpenCode search configuration പരിശോധിക്കൽ",
      `Unicode_wide,
      2,
      40 );
    ("wide combining accent", "a\u{0301}", `Unicode_wide, 2, 1);
    ("wide Hangul jamo", "\u{1100}\u{1161}\u{11A8}", `Unicode_wide, 2, 2);
    ("wide emoji modifier", "👍🏽", `Unicode_wide, 2, 2);
    ("wide ASCII with spacing mark", "a\u{093E}", `Unicode_wide, 2, 2);
    ("basic ASCII", "abc", `Unicode, 8, 3);
    ("ASCII controls", "a\n\r\x00b", `Unicode, 8, 2);
    ("tab default", "\t", `Unicode, 2, 2);
    ("tab custom", "\t", `Unicode, 4, 4);
    (* A non-positive tab_width falls back to the default rather than
       measuring the tab as nothing. Zero is the value that tells the clamp
       from its off-by-one: `w < 0` would let 0 through as a width. *)
    ("emoji simple", "👋", `Unicode, 8, 2);
    ("emoji ZWJ sequence", "👩\u{200D}🚀", `Unicode, 8, 2);
    ("emoji family", "👨\u{200D}👩\u{200D}👧\u{200D}👦", `Unicode, 8, 2);
    ("regional indicator pair", "🇺🇸", `Unicode, 8, 2);
    ("skin tone sequence", "👍🏽", `Unicode, 8, 2);
    (* An emoji modifier sequence has the emoji presentation whatever its
       base's (UTS #51 §2.7.1): kitty, WezTerm and foot draw ☝🏽 in two cells.
       Ghostty's code leaves it at its base's width. *)
    ("skin tone on a text-default base", "\u{261D}\u{1F3FD}", `Unicode, 8, 2);
    ( "skin tone on a text-default base wcwidth",
      "\u{261D}\u{1F3FD}",
      `Wcwidth,
      8,
      3 );
    ( "skin tone on a text-default base wide",
      "\u{261D}\u{1F3FD}",
      `Unicode_wide,
      8,
      1 );
    ("skin tone after writing hand", "\u{270D}\u{1F3FB}", `Unicode, 8, 2);
    ("skin tone after VS16", "\u{261D}\u{FE0F}\u{1F3FD}", `Unicode, 8, 2);
    (* A modifier after an emoji that is no modifier base makes no sequence. *)
    ("skin tone after a heart", "\u{2764}\u{1F3FD}", `Unicode, 8, 1);
    ("wcwidth strategy", "👩\u{200D}🚀", `Wcwidth, 8, 4);
    ("combining accent", "a\u{0301}", `Unicode, 8, 1);
    ("Thai spacing mark", "น้ำ", `Unicode, 8, 2);
    ("Thai spacing mark wcwidth", "น้ำ", `Wcwidth, 8, 2);
    ("Thai word with spacing mark", "คำว่า", `Unicode, 8, 4);
    ("Thai tone mark", "ว่", `Unicode, 8, 1);
    ("Thai standalone spacing mark", "ำ", `Unicode, 8, 1);
    (* A spacing mark has no width of its own: alone, or after a lone
       combining mark, it leaves its cluster no cells, as kitty, WezTerm and
       foot draw it. A wide one keeps its two cells. *)
    ("Indic spacing mark alone", "\u{093F}", `Unicode, 8, 0);
    ("spacing mark after a combining mark", "\u{0301}\u{0940}", `Unicode, 8, 0);
    ("spacing mark alone wcwidth", "\u{093F}", `Wcwidth, 8, 1);
    ("wide spacing mark alone", "\u{302E}", `Unicode, 8, 2);
    ("spacing marks after a base", "\u{0915}\u{093E}\u{0903}", `Unicode, 8, 1);
    ("Thai repeated spacing marks", "น้ำำ", `Unicode, 8, 2);
    ("Thai repeated spacing marks wcwidth", "น้ำำ", `Wcwidth, 8, 3);
    ("Thai spacing mark after wide base", "界ำ", `Unicode, 8, 2);
    ("Lao spacing mark", "\u{0E99}\u{0EB3}", `Unicode, 8, 2);
    ("Lao spacing mark wcwidth", "\u{0E99}\u{0EB3}", `Wcwidth, 8, 2);
    ("Thai spacing mark wide", "น้ำ", `Unicode_wide, 8, 2);
    ("Indic spacing mark retains base width", "क\u{093E}", `Unicode, 8, 1);
    ("Indic spacing mark wcwidth", "क\u{093E}", `Wcwidth, 8, 2);
    (* A conjunct takes its first consonant's cell, in every script. *)
    ("Devanagari conjunct", "\u{0915}\u{094D}\u{0937}", `Unicode, 8, 1);
    ( "conjunct of three consonants",
      "\u{0915}\u{094D}\u{0915}\u{094D}\u{0915}",
      `Unicode,
      8,
      1 );
    ("Malayalam conjunct", "\u{0D15}\u{0D4D}\u{0D15}", `Unicode, 8, 1);
    ("zero width space", "\u{200B}", `Unicode, 8, 0);
    ("C1 control", uchar_to_utf8 0x008A, `Unicode, 8, 0);
  ]

let ascii_fast_path_consistency () =
  let s = "hello\tworld" in
  let unicode_w = measure ~width_method:`Unicode ~tab_width:4 s in
  let wcwidth_w = measure ~width_method:`Wcwidth ~tab_width:4 s in
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

let measure_sub_matches_slice () =
  let samples = [ "a中é👨‍💻z"; "\xF0\x9F\x80x\xC3\x80\xFF" ] in
  List.iter
    (fun width_method ->
      List.iter
        (fun text ->
          for pos = 0 to String.length text do
            for len = 0 to String.length text - pos do
              equal
                ~msg:(Printf.sprintf "slice %d+%d" pos len)
                int
                (measure ~width_method ~tab_width:2 (String.sub text pos len))
                (measure_sub ~width_method ~tab_width:2 text ~pos ~len)
            done
          done)
        samples)
    [ `Unicode; `Wcwidth ]

let grapheme_iteration () =
  let zwj_seq = "👩\u{200D}🚀" in
  let count_default = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr count_default) zwj_seq;
  equal ~msg:"default: ZWJ sequence is one grapheme" int 1 !count_default;
  equal ~msg:"grapheme_count" int 3 (grapheme_count ("a" ^ zwj_seq ^ "b"))

let grapheme_info_skips_zero_width () =
  let seen = ref [] in
  iter_units ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    ("a" ^ "\u{200B}" ^ "界");
  equal ~msg:"zero-width grapheme is skipped"
    (list (triple int int int))
    [ (0, 1, 1); (4, 3, 2) ]
    (List.rev !seen)

let grapheme_info_ascii_base_with_combining_mark () =
  let seen = ref [] in
  iter_units ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    "e\u{0301}x";
  equal ~msg:"e + U+0301 is one 3-byte cluster"
    (list (triple int int int))
    [ (0, 3, 1); (3, 1, 1) ]
    (List.rev !seen);
  let seen = ref [] in
  iter_units ~width_method:`Unicode ~tab_width:2
    (fun ~offset ~len ~width -> seen := (offset, len, width) :: !seen)
    "abce\u{0301}";
  equal ~msg:"mark extends the last byte of an ASCII run"
    (list (triple int int int))
    [ (0, 1, 1); (1, 1, 1); (2, 1, 1); (3, 3, 1) ]
    (List.rev !seen)

type wrap_break = { byte_offset : int; grapheme_offset : int }
type line_break = { pos : int; kind : line_break_kind }

let wrap_breaks s =
  let acc = ref [] in
  iter_wrap_breaks
    (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset ->
      acc := { byte_offset = next_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let wrap_break_points s =
  let acc = ref [] in
  iter_wrap_breaks
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
    ~equal:( = )

let line_break_testable =
  Testable.make
    ~pp:(fun ppf { pos; kind } ->
      let kind_s =
        match kind with `LF -> "LF" | `CR -> "CR" | `CRLF -> "CRLF"
      in
      Format.fprintf ppf "{pos=%d; kind=%s}" pos kind_s)
    ~equal:( = )

let position_testable =
  Testable.make
    ~pp:(fun ppf { byte_offset; units; columns_used } ->
      Format.fprintf ppf "{byte_offset=%d; units=%d; columns_used=%d}"
        byte_offset units columns_used)
    ~equal:( = )

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
  equal ~msg:"period excluded from seven ASCII breaks" int 7
    (Array.length breaks);
  equal ~msg:"break after hyphen" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_keep_periods_inside_tokens () =
  equal ~msg:"executable suffix is not a boundary"
    (array wrap_break_testable)
    [|
      { byte_offset = 1; grapheme_offset = 1 };
      { byte_offset = 5; grapheme_offset = 5 };
    |]
    (wrap_break_points "./bin/main.exe");
  equal ~msg:"dotted filename has no boundary"
    (array wrap_break_testable)
    [||]
    (wrap_break_points "release.tar.gz");
  equal ~msg:"decimal has no boundary"
    (array wrap_break_testable)
    [||]
    (wrap_break_points "3.1415")

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
  (* UAX #14 class GL: glue, which forbids a break on either side. *)
  List.iter
    (fun (name, glue) ->
      equal ~msg:("no break after " ^ name) int 0
        (Array.length (wrap_breaks ("a" ^ glue ^ "b"))))
    [
      ("NBSP", nbsp);
      ("FIGURE SPACE", "\xE2\x80\x87");
      ("NNBSP", "\xE2\x80\xAF");
      ("NON-BREAKING HYPHEN", "\xE2\x80\x91");
    ];
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

let wrap_breaks_zwj_sequence () =
  let zwj_seq = "👩\u{200D}🚀" in
  equal ~msg:"unicode: no breaks in ZWJ sequence" int 0
    (Array.length (wrap_breaks zwj_seq))

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
  let en_space = "\xE2\x80\x82" in
  let s = "a" ^ en_space ^ "b" in
  let points = wrap_break_points s in
  let resumes = wrap_breaks s in
  equal ~msg:"one break point for one EN SPACE" int 1 (Array.length points);
  equal ~msg:"point offset at EN SPACE grapheme start" wrap_break_testable
    { byte_offset = 1; grapheme_offset = 1 }
    points.(0);
  equal ~msg:"resume offset after EN SPACE grapheme" wrap_break_testable
    { byte_offset = 4; grapheme_offset = 1 }
    resumes.(0)

let wrap_breaks_mixed_cjk_ascii_transitions () =
  let cjk_ascii = "日本語abc" in
  let cjk_points = wrap_break_points cjk_ascii in
  let cjk_resumes = wrap_breaks cjk_ascii in
  equal ~msg:"CJK->ASCII breaks" int 3 (Array.length cjk_points);
  equal ~msg:"CJK->ASCII point" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 2 }
    cjk_points.(2);
  equal ~msg:"CJK->ASCII resume" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 2 }
    cjk_resumes.(2);
  let ascii_cjk = "abc日本語" in
  let ascii_points = wrap_break_points ascii_cjk in
  let ascii_resumes = wrap_breaks ascii_cjk in
  equal ~msg:"ASCII->CJK breaks" int 3 (Array.length ascii_points);
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
  equal ~msg:"CJK punctuation breaks" int 3 (Array.length points);
  equal ~msg:"CJK punctuation point" wrap_break_testable
    { byte_offset = 9; grapheme_offset = 3 }
    points.(2);
  equal ~msg:"CJK punctuation resume" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 3 }
    resumes.(2);
  let check_one label cp =
    let s = "a" ^ uchar_to_utf8 cp ^ "b" in
    equal ~msg:label int 1 (Array.length (wrap_breaks s))
  in
  check_one "ideographic comma" 0x3001;
  check_one "fullwidth exclamation" 0xFF01;
  check_one "fullwidth question" 0xFF1F

let cjk_break_cases =
  [
    ("Han", "中文", [ 3 ], []);
    ("script transition", "中文A", [ 3; 6 ], [ 6 ]);
    ("ideographic punctuation", "字。", [ 6 ], [ 6 ]);
    ("Kana middle dot", "カ・ナ", [ 6 ], []);
    ("Kana double hyphen", "カ゠ナ", [ 6 ], []);
    ("Kana combining mark", "か\u{3099}く", [ 6 ], []);
    ("isolated combining mark", "\u{3099}く", [], []);
    ("Hangul", "테스트", [ 3; 6 ], []);
    ("halfwidth Kana", "ｶﾀｶﾅ", [ 3; 6; 9 ], []);
    ("supplementary Han", "𠀀𠀁", [ 4 ], []);
  ]

let cjk_breaks (_, text, line_breaks, word_breaks) =
  let collect iter =
    let breaks = ref [] in
    iter
      (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset:_ ->
        breaks := next_byte_offset :: !breaks)
      text;
    List.rev !breaks
  in
  equal ~msg:"line opportunities" (list int) line_breaks
    (collect iter_wrap_breaks);
  equal ~msg:"editor words" (list int) word_breaks (collect iter_word_breaks)

let cjk_layout_cases =
  [
    ("mixed", "AB 测试文字", 6, [ "AB 测"; "试文字" ]);
    ("punctuation", "测试。文字", 4, [ "测"; "试。"; "文字" ]);
    ("Kana punctuation", "カ・ナ", 4, [ "カ・"; "ナ" ]);
    ("Kana combining mark", "か\u{3099}くけ", 4, [ "か\u{3099}く"; "け" ]);
  ]

let cjk_layout (_, text, width, expected) =
  List.iter
    (fun width_method ->
      let source = Layout.Source.of_string text in
      let layout = Layout.create ~width_method source ~width ~wrap:`Word in
      let rows =
        List.init (Layout.row_count layout) (fun row ->
            Layout.Source.slice source
              ~first:(Layout.row_start layout row)
              ~last:(Layout.row_end layout row))
      in
      equal ~msg:"wrapped rows" (list string) expected rows;
      equal ~msg:"source preserved" string text (String.concat "" rows);
      equal ~msg:"measurement agrees" (pair int int)
        (Layout.row_count layout, Layout.max_width layout)
        (Layout.measure ~width_method source ~width ~wrap:`Word))
    [ `Unicode; `Unicode_wide; `Wcwidth ]

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

let width_at_positions () =
  equal ~msg:"empty" int 0
    (width_at ~width_method:`Unicode ~tab_width:8 "" ~byte_offset:0);
  equal ~msg:"out of bounds" int 0
    (width_at ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:10);
  equal ~msg:"ASCII" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "hello" ~byte_offset:1);
  equal ~msg:"tab fixed width" int 4
    (width_at ~width_method:`Unicode ~tab_width:4 "a\tb" ~byte_offset:1);
  equal ~msg:"CJK" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "世界" ~byte_offset:3);
  equal ~msg:"combining cluster" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "cafe\u{0301}" ~byte_offset:3);
  equal ~msg:"ZWJ cluster" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "👩\u{200D}🚀" ~byte_offset:0);
  equal ~msg:"flag cluster" int 2
    (width_at ~width_method:`Unicode ~tab_width:8 "🇺🇸" ~byte_offset:0);
  equal ~msg:"malformed byte" int 1
    (width_at ~width_method:`Unicode ~tab_width:8 "\xC3(" ~byte_offset:0)

let wcwidth_position_semantics () =
  let emoji = "👋🏿" in
  equal ~msg:"wcwidth first emoji codepoint" int 2
    (width_at ~width_method:`Wcwidth ~tab_width:4 emoji ~byte_offset:0);
  equal ~msg:"wcwidth skin-tone codepoint" int 2
    (width_at ~width_method:`Wcwidth ~tab_width:4 emoji ~byte_offset:4);
  let zwj = "👩\u{200D}🚀" in
  equal ~msg:"wcwidth ZWJ codepoint" int 0
    (width_at ~width_method:`Wcwidth ~tab_width:4 zwj ~byte_offset:4);
  equal ~msg:"wcwidth find_pos snap back" position_testable
    { byte_offset = 2; units = 2; columns_used = 2 }
    (find_pos ~width_method:`Wcwidth ~tab_width:4 "AB👋🏿CD" ~columns:3);
  equal ~msg:"wcwidth find_pos include start" position_testable
    { byte_offset = 6; units = 3; columns_used = 4 }
    (find_pos ~width_method:`Wcwidth ~tab_width:4 ~include_start_before:true
       "AB👋🏿CD" ~columns:3);
  equal ~msg:"wcwidth wrap through emoji" position_testable
    { byte_offset = 6; units = 3; columns_used = 4 }
    (find_wrap_pos ~width_method:`Wcwidth ~tab_width:4 "Hi👋🏿Bye" ~max_columns:4)

let malformed_wcwidth_positions () =
  let text = "\xF0\x9F" in
  equal ~msg:"position uses the measurement replacement policy"
    position_testable
    { byte_offset = 2; units = 1; columns_used = 1 }
    (find_pos ~width_method:`Wcwidth ~tab_width:2 text ~columns:2)

let wcwidth_decoder_matches_stdlib () =
  let check text =
    let offset = ref 0 in
    let count = ref 0 in
    while !offset < String.length text do
      let decoded = String.get_utf_8_uchar text !offset in
      offset := !offset + Uchar.utf_decode_length decoded;
      (* Each scalar of positive width starts a unit. *)
      if uchar_width ~tab_width:2 (Uchar.utf_decode_uchar decoded) > 0 then
        incr count
    done;
    let actual =
      find_pos ~width_method:`Wcwidth ~tab_width:2 text ~columns:max_int
    in
    equal ~msg:"maximal-subpart count" int !count actual.units;
    equal ~msg:"measurement agrees" int
      (measure ~width_method:`Wcwidth ~tab_width:2 text)
      actual.columns_used
  in
  let bytes = Bytes.create 2 in
  for first = 0 to 255 do
    Bytes.set bytes 0 (Char.chr first);
    check (Bytes.sub_string bytes 0 1);
    for second = 0 to 255 do
      Bytes.set bytes 1 (Char.chr second);
      check (Bytes.unsafe_to_string bytes)
    done
  done;
  let boundaries = [ 0x7F; 0x80; 0x8F; 0x90; 0x9F; 0xA0; 0xBF; 0xC0 ] in
  List.iter
    (fun first ->
      List.iter
        (fun second ->
          List.iter
            (fun third ->
              List.iter
                (fun fourth ->
                  let text =
                    String.init 4 (function
                      | 0 -> Char.chr first
                      | 1 -> Char.chr second
                      | 2 -> Char.chr third
                      | _ -> Char.chr fourth)
                  in
                  check (String.sub text 0 3);
                  check text)
                boundaries)
            boundaries)
        boundaries)
    [ 0xE0; 0xE1; 0xED; 0xEF; 0xF0; 0xF1; 0xF4; 0xF5 ]

let unicode_position_semantics () =
  let input = "A🌍B世C" in
  equal ~msg:"find_pos after ASCII" position_testable
    { byte_offset = 1; units = 1; columns_used = 1 }
    (find_pos ~width_method:`Unicode ~tab_width:8 input ~columns:2);
  equal ~msg:"find_pos include start before wide" position_testable
    { byte_offset = 5; units = 2; columns_used = 3 }
    (find_pos ~width_method:`Unicode ~tab_width:8 ~include_start_before:true
       input ~columns:2);
  equal ~msg:"find_wrap_pos includes fitting wide grapheme" position_testable
    { byte_offset = 5; units = 2; columns_used = 3 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 input ~max_columns:3);
  equal ~msg:"find_wrap_pos stops before too-wide grapheme" position_testable
    { byte_offset = 1; units = 1; columns_used = 1 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 input ~max_columns:2);
  equal ~msg:"combining mark included" position_testable
    { byte_offset = 6; units = 4; columns_used = 4 }
    (find_pos ~width_method:`Unicode ~tab_width:8 "cafe\u{0301}test" ~columns:4);
  equal ~msg:"malformed offsets" position_testable
    { byte_offset = 1; units = 1; columns_used = 1 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 "\xC3(" ~max_columns:1);
  (* No columns still hold the units of no width before the first that has
     one. *)
  let none = { byte_offset = 3; units = 0; columns_used = 0 } in
  List.iter
    (fun m ->
      equal ~msg:"zero-width unit in no columns" position_testable none
        (find_wrap_pos ~width_method:m ~tab_width:8 "\u{200B}\u{4E2D}"
           ~max_columns:0);
      equal ~msg:"and in fewer" position_testable none
        (find_pos ~width_method:m ~tab_width:8 "\u{200B}\u{4E2D}" ~columns:(-1)))
    [ `Unicode; `Unicode_wide; `Wcwidth ];
  equal ~msg:"an ASCII control in no columns" position_testable
    { byte_offset = 1; units = 0; columns_used = 0 }
    (find_wrap_pos ~width_method:`Unicode ~tab_width:8 "\x01a" ~max_columns:0)

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
  check "red heart U+2764" 0x2764 1;
  check "red heart + VS16 base" 0x2764 1;
  let heart_vs16 = uchar_to_utf8 0x2764 ^ uchar_to_utf8 0xFE0F in
  check_width "heart with VS16 = width 2" 2
    (measure ~width_method:`Unicode ~tab_width:2 heart_vs16);
  let vs16 base =
    measure ~width_method:`Unicode ~tab_width:2 (base ^ "\u{FE0F}")
  in
  check_width "VS16 widens a narrow emoji" 2 (vs16 "\u{263A}");
  check_width "VS16 widens a keycap base" 2 (vs16 "1");
  check_width "VS16 leaves a letter" 1 (vs16 "a");
  check_width "VS16 leaves a space" 1 (vs16 " ");
  check_width "VS16 leaves a mark" 1 (vs16 "1\u{301}");
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

let scalar_width_agrees_with_measurement () =
  let bytes = Bytes.create 4 in
  List.iter
    (fun cp ->
      let uchar = Uchar.of_int cp in
      let len = Bytes.set_utf_8_uchar bytes 0 uchar in
      let text = Bytes.sub_string bytes 0 len in
      List.iter
        (fun width_method ->
          equal int
            (measure ~width_method ~tab_width:4 text)
            (uchar_width ~tab_width:4 uchar))
        [ `Unicode; `Wcwidth ])
    [
      0;
      9;
      32;
      127;
      0x85;
      0x301;
      0x915;
      0x94D;
      0x200D;
      0x754C;
      0xFE0F;
      0x1F1EB;
      0x1F680;
      0x10FFFF;
    ]

let layout_source_boundaries () =
  let s = Layout.Source.of_string "a\226\128\139b\r\n" in
  equal int 4 (Layout.Source.length s);
  equal int 2 (Layout.Source.line_count s);
  equal int 3 (Layout.Source.line_end s 0);
  equal int 4 (Layout.Source.line_start s 1);
  equal string "a\226\128\139b\r\n" (Layout.Source.slice s ~first:0 ~last:4)

let layout_truncated_source () =
  let source = Layout.Source.of_string "abcdef\nXY" in
  let t = Layout.create source ~width:3 ~wrap:`None ~truncate:true in
  equal int 7 (Layout.row_start t 1);
  equal (pair int int) (2, 6) (Layout.coverage t ~row:0 ~column:2);
  equal (pair int int) (7, 8) (Layout.coverage t ~row:1 ~column:0);
  equal (pair int int) (0, 2)
    (Layout.locate t Layout.{ grapheme = 4; affinity = Upstream });
  equal (pair int int) (0, 3)
    (Layout.locate t Layout.{ grapheme = 4; affinity = Downstream })

let layout_affinity () =
  let t = Layout.create (Layout.Source.of_string "abcd") ~width:2 ~wrap:`Char in
  equal (pair int int) (0, 2)
    (Layout.locate t Layout.{ grapheme = 2; affinity = Upstream });
  equal (pair int int) (1, 0)
    (Layout.locate t Layout.{ grapheme = 2; affinity = Downstream });
  let hit = Layout.hit t ~row:0 ~column:2 in
  equal int 2 hit.grapheme;
  is_true (hit.affinity = Layout.Upstream)

let layout_whole_clusters () =
  List.iter
    (fun width_method ->
      let source = Layout.Source.of_string "e\204\129👩‍💻界" in
      equal int 3 (Layout.Source.length source);
      let t = Layout.create ~width_method source ~width:1 ~wrap:`Char in
      equal int 3 (Layout.row_count t);
      for row = 0 to 2 do
        equal int row (Layout.row_start t row);
        equal int (row + 1) (Layout.row_end t row)
      done)
    [ `Unicode; `Wcwidth ]

let thai_spacing_mark_layout () =
  let text = "น้ำX" in
  let source = Layout.Source.of_string text in
  equal ~msg:"spacing mark stays in the first source cluster" int 2
    (Layout.Source.length source);
  List.iter
    (fun width_method ->
      let layout = Layout.create ~width_method source ~width:2 ~wrap:`Char in
      equal ~msg:"following character wraps" int 2 (Layout.row_count layout);
      equal ~msg:"first row spans two cells" int 2 (Layout.row_width layout 0);
      equal ~msg:"first row retains the whole cluster" string "น้ำ"
        (Layout.Source.slice source
           ~first:(Layout.row_start layout 0)
           ~last:(Layout.row_end layout 0));
      equal ~msg:"second cell selects the same cluster" (pair int int) (0, 1)
        (Layout.coverage layout ~row:0 ~column:1);
      equal ~msg:"caret after the cluster starts the next row" (pair int int)
        (1, 0)
        (Layout.locate layout Layout.{ grapheme = 1; affinity = Downstream }))
    [ `Unicode; `Wcwidth ];
  equal ~msg:"a one-column prefix cannot split the Thai cluster" int 0
    (find_wrap_pos ~width_method:`Unicode ~tab_width:2 text ~max_columns:1)
      .byte_offset;
  equal ~msg:"a two-column prefix fits the whole Thai cluster" int
    (String.length "น้ำ")
    (find_wrap_pos ~width_method:`Unicode ~tab_width:2 text ~max_columns:2)
      .byte_offset

let wide_grapheme_positions () =
  let text = "രിX" in
  equal int 2
    (width_at ~width_method:`Unicode_wide ~tab_width:2 text ~byte_offset:0);
  equal int 2
    (width_at ~width_method:`Unicode_wide ~tab_width:2 "a\u{093E}"
       ~byte_offset:0);
  equal int 0
    (find_wrap_pos ~width_method:`Unicode_wide ~tab_width:2 text ~max_columns:1)
      .byte_offset;
  equal int 0
    (find_pos ~width_method:`Unicode_wide ~tab_width:2 text ~columns:1)
      .byte_offset;
  let source = Layout.Source.of_string text in
  let normal = Layout.create source ~width:2 ~wrap:`Char in
  let wide =
    Layout.create ~width_method:`Unicode_wide source ~width:2 ~wrap:`Char
  in
  equal int 1 (Layout.row_count normal);
  equal int 2 (Layout.row_count wide);
  equal (pair int int) (0, 1) (Layout.coverage wide ~row:0 ~column:1);
  equal (pair int int) (1, 0)
    (Layout.locate wide Layout.{ grapheme = 1; affinity = Downstream });
  equal int 1 (Layout.row_count (Layout.create source ~width:2 ~wrap:`Char))

let layout_zero_width_snapping () =
  let source = Layout.Source.of_string "a\226\128\139b" in
  let t = Layout.create source ~width:20 ~wrap:`None in
  equal int 1 (Layout.hit ~affinity:Upstream t ~row:0 ~column:1).grapheme;
  equal int 2 (Layout.hit ~affinity:Downstream t ~row:0 ~column:1).grapheme;
  let cells = ref [] in
  Layout.iter_cells t ~row:0 ~first_column:0 ~last_column:2
    (fun ~first ~last ~column:_ ~width:_ ~ellipsis:_ ->
      cells := (first, last) :: !cells);
  equal (list (pair int int)) [ (0, 1); (2, 3) ] (List.rev !cells);
  let overwide =
    Layout.create (Layout.Source.of_string "界\226\128\139") ~width:1 ~wrap:`Char
  in
  equal int 1 (Layout.row_count overwide);
  equal int 2 (Layout.row_end overwide 0)

let layout_bounded_iteration () =
  let source = Layout.Source.of_string (String.make 100_000 'x') in
  let t = Layout.create source ~width:100_000 ~wrap:`None in
  let visited = ref 0 in
  Layout.iter_cells t ~row:0 ~first_column:99_990 ~last_column:99_995
    (fun ~first ~last:_ ~column ~width:_ ~ellipsis:_ ->
      equal int first column;
      incr visited);
  equal int 5 !visited;
  let wrapped = Layout.create source ~width:1 ~wrap:`Char in
  let rows = ref [] in
  Layout.iter_rows wrapped ~first:99990 ~count:3
    (fun ~row ~logical_line:_ ~continuation ->
      rows := (row, continuation) :: !rows);
  equal
    (list (pair int int))
    [ (99990, 99990); (99991, 99991); (99992, 99992) ]
    (List.rev !rows)

let layout_edge_cases () =
  List.iter
    (fun text ->
      let source = Layout.Source.of_string text in
      equal string text
        (Layout.Source.slice source ~first:0
           ~last:(Layout.Source.length source));
      List.iter
        (fun width ->
          List.iter
            (fun wrap ->
              let t = Layout.create source ~width ~wrap in
              equal (pair int int)
                (Layout.row_count t, Layout.max_width t)
                (Layout.measure source ~width ~wrap);
              equal int 0 (Layout.row_start t 0);
              equal int
                (Layout.Source.length source)
                (Layout.row_end t (Layout.row_count t - 1)))
            [ `None; `Char; `Word ])
        [ 0; 1; 2; 7 ])
    [ ""; "\n"; "\r\n"; "hello world"; "界x"; "\255a\226\128\139" ]

(* Segmenter *)

let methods = [ `Unicode; `Unicode_wide; `Wcwidth ]

let method_name = function
  | `Unicode -> "Unicode"
  | `Unicode_wide -> "Unicode_wide"
  | `Wcwidth -> "Wcwidth"

(* An emoji without Emoji_Presentation and a pictograph that is not an emoji
   are East_Asian_Width N, so one cell, in every method (UAX #11; UTS #51 ED-7),
   as in glibc, xterm, Ghostty, WezTerm and foot. VS16 widens an emoji to two
   cells in the grapheme methods only: [`Wcwidth] adds the width of each
   scalar, and U+FE0F has none. *)
let text_default_pictograph_widths () =
  let narrow =
    [
      ("red heart U+2764", "\u{2764}");
      ("warning sign U+26A0", "\u{26A0}");
      ("double exclamation U+203C", "\u{203C}");
      ("helmet U+26D1", "\u{26D1}");
      ("dove U+1F54A", "\u{1F54A}");
      ("mahjong east wind U+1F000", "\u{1F000}");
      ("domino U+1F030", "\u{1F030}");
      ("alchemical U+1F700", "\u{1F700}");
      ("chess king U+1FA00", "\u{1FA00}");
    ]
  in
  List.iter
    (fun m ->
      let measure s = measure ~width_method:m ~tab_width:2 s in
      let name = method_name m in
      List.iter
        (fun (label, s) -> check_width (name ^ " " ^ label) 1 (measure s))
        narrow;
      check_width (name ^ " grinning face U+1F600") 2 (measure "\u{1F600}");
      check_width (name ^ " heart in text") 4 (measure "a\u{2764}bc");
      check_width
        (name ^ " VS16 after a non-emoji pictograph")
        1
        (measure "\u{1F000}\u{FE0F}");
      let vs16 = if m = `Wcwidth then 1 else 2 in
      check_width (name ^ " red heart + VS16") vs16 (measure "\u{2764}\u{FE0F}");
      check_width
        (name ^ " warning sign + VS16")
        vs16
        (measure "\u{26A0}\u{FE0F}"))
    methods;
  check_width "uchar_width red heart" 1
    (uchar_width ~tab_width:2 (Uchar.of_int 0x2764));
  check_width "uchar_width grinning face" 2
    (uchar_width ~tab_width:2 (Uchar.of_int 0x1F600))

(* A leading consonant composes the conjoining vowels and trailing consonants
   after it into one syllable (Unicode §3.12): they take no cells of their own,
   as in wcwidth (Markus Kuhn's, glibc's, xterm's), under every method. *)
let hangul_conjoining_widths () =
  List.iter
    (fun (label, cp) ->
      check_width ("uchar_width " ^ label) 0
        (uchar_width ~tab_width:2 (Uchar.of_int cp)))
    [
      ("filler U+1160", 0x1160);
      ("vowel U+1161", 0x1161);
      ("trailing consonant U+11A8", 0x11A8);
      ("trailing consonant U+11FF", 0x11FF);
      ("extended-B vowel U+D7B0", 0xD7B0);
      ("extended-B consonant U+D7FB", 0xD7FB);
    ];
  check_width "uchar_width leading consonant U+1100" 2
    (uchar_width ~tab_width:2 (Uchar.of_int 0x1100));
  List.iter
    (fun m ->
      let measure s = measure ~width_method:m ~tab_width:2 s in
      let name = method_name m in
      check_width (name ^ " syllable L V T") 2
        (measure "\u{1100}\u{1161}\u{11A8}");
      check_width (name ^ " syllable L V") 2 (measure "\u{1100}\u{1161}"))
    [ `Unicode; `Wcwidth ]

(* Unicode 18.0 assigns wide characters and nonspacing marks of its own, and
   changes GB9c to bind a consonant to any linker before it, with no
   consonant before the linker (UAX #29 rev. 49). *)
let unicode_18 () =
  List.iter
    (fun (label, cp, w) ->
      check_width ("uchar_width " ^ label) w
        (uchar_width ~tab_width:2 (Uchar.of_int cp)))
    [
      ("Hebrew mark U+05C8", 0x05C8, 0);
      ("Arabic mark U+10ECF", 0x10ECF, 0);
      ("musical mark U+1D25C", 0x1D25C, 0);
      ("Tangut component U+18CD6", 0x18CD6, 2);
      ("Tangut component U+18FE8", 0x18FE8, 2);
      ("thumb sign U+1FAFA", 0x1FAFA, 2);
    ];
  equal ~msg:"a linker binds a consonant with no consonant before it" int 1
    (grapheme_count "a\u{094D}\u{0937}");
  equal ~msg:"Extend after the linker keeps the binding" int 1
    (grapheme_count "\u{094D}\u{0951}\u{0937}")

(* Widths measured in seven terminals: data/measured_widths.txt, whose header
   says how. Each method is checked against the terminals whose way of placing
   text it models: [`Wcwidth] against xterm, VTE and st, which place each
   character by itself, on every row; [`Unicode] against kitty 0.49.2,
   WezTerm and foot, which place grapheme clusters, on the rows where the
   three agree. *)
type measured = {
  id : string;
  text : string;
  what : string;
  widths : (string * int option) list;
}

let measured_rows =
  let utf8 cps =
    let b = Buffer.create 16 in
    List.iter
      (fun cp ->
        Buffer.add_utf_8_uchar b (Uchar.of_int (int_of_string ("0x" ^ cp))))
      (Stdlib.String.split_on_char ' ' cps |> List.filter (( <> ) ""));
    Buffer.contents b
  in
  let fields line =
    List.map Stdlib.String.trim (Stdlib.String.split_on_char '|' line)
  in
  let lines =
    In_channel.with_open_bin
      (data_path "measured_widths.txt")
      In_channel.input_lines
  in
  (* "# id | code points | terminal ... | what" *)
  let terminals =
    let header = List.find (Stdlib.String.starts_with ~prefix:"# id |") lines in
    let columns = fields header in
    List.filteri (fun i _ -> i >= 2 && i < List.length columns - 1) columns
  in
  List.filter_map
    (fun line ->
      if line = "" || line.[0] = '#' then None
      else
        match fields line with
        | id :: cps :: rest ->
            let widths =
              List.mapi
                (fun i t ->
                  ( t,
                    match List.nth rest i with
                    | "-" -> None
                    | w -> Some (int_of_string w) ))
                terminals
            in
            Some
              {
                id;
                text = utf8 cps;
                what = List.nth rest (List.length terminals);
                widths;
              }
        | _ -> failwith ("measured_widths.txt: " ^ line))
    lines

let per_character = [ "xterm 398"; "VTE 0.80.1"; "st 0.9.2" ]
let clustering = [ "kitty 0.49.2"; "WezTerm 20240203"; "foot 1.21.0" ]

let check_measured r =
  let measure m = measure ~width_method:m ~tab_width:8 r.text in
  List.iter
    (fun t ->
      match List.assoc t r.widths with
      | Some w -> check_width ("Wcwidth as " ^ t) w (measure `Wcwidth)
      | None -> ())
    per_character;
  match List.map (fun t -> List.assoc t r.widths) clustering with
  | Some w :: rest when List.for_all (( = ) (Some w)) rest ->
      check_width "Unicode as the clustering terminals" w (measure `Unicode)
  | _ -> ()

(* The units of [chunks], fed one scalar at a time with [Segmenter.add], or
   with [Segmenter.ascii_run] wherever it applies when [runs]. Each unit is
   its bytes and its width when it closed. *)
let units ?(runs = false) m chunks =
  let s = Segmenter.create m in
  let out = ref [] and cur = Buffer.create 8 in
  let close w =
    if Buffer.length cur > 0 then out := (Buffer.contents cur, w) :: !out;
    Buffer.clear cur
  in
  List.iter
    (fun chunk ->
      let len = String.length chunk in
      let i = ref 0 in
      while !i < len do
        let w = Segmenter.width s in
        let k =
          if runs then Segmenter.ascii_run s chunk ~pos:!i ~len:(len - !i)
          else 0
        in
        if k > 0 then begin
          close w;
          for j = !i to !i + k - 2 do
            out := (String.make 1 chunk.[j], 1) :: !out
          done;
          Buffer.add_char cur chunk.[!i + k - 1];
          i := !i + k
        end
        else begin
          let d = String.get_utf_8_uchar chunk !i in
          let n = Uchar.utf_decode_length d in
          if Segmenter.add s (Uchar.utf_decode_uchar d) then close w;
          Buffer.add_string cur (String.sub chunk !i n);
          i := !i + n
        end
      done)
    chunks;
  close (Segmenter.width s);
  List.rev !out

let unit_list = list (pair string int)

let segmenter_units () =
  let check m text expected =
    equal
      ~msg:(method_name m ^ " " ^ String.escaped text)
      unit_list expected (units m [ text ])
  in
  List.iter
    (fun m ->
      check m "e\u{0301}x" [ ("e\u{0301}", 1); ("x", 1) ];
      check m "a\r\nb" [ ("a", 1); ("\r", 0); ("\n", 0); ("b", 1) ];
      check m "a\tb" [ ("a", 1); ("\t", 0); ("b", 1) ];
      check m "a\u{9B}b" [ ("a", 1); ("\u{9B}", 0); ("b", 1) ];
      check m "\t\u{0301}" [ ("\t", 0); ("\u{0301}", 0) ];
      check m "中" [ ("中", 2) ])
    methods;
  check `Unicode "\u{0600}1b" [ ("\u{0600}1", 1); ("b", 1) ];
  check `Unicode "👨\u{200D}💻" [ ("👨\u{200D}💻", 2) ];
  check `Wcwidth "👨\u{200D}💻" [ ("👨\u{200D}", 2); ("💻", 2) ];
  check `Wcwidth "\u{0301}e\u{0301}" [ ("\u{0301}", 0); ("e\u{0301}", 1) ];
  check `Unicode "1\u{FE0F}\u{20E3}" [ ("1\u{FE0F}\u{20E3}", 2) ];
  check `Unicode "a1\u{FE0F}" [ ("a", 1); ("1\u{FE0F}", 2) ];
  check `Unicode "ab\u{FE0F}" [ ("a", 1); ("b\u{FE0F}", 1) ]

(* Feeding with runs of ASCII, and cut anywhere, gives the units of the whole
   text fed a scalar at a time. *)
let segmenter_split_invariance () =
  let texts =
    [
      "\u{0D4E}ab";
      "x\u{111C2}ab";
      "\u{0600}1bcd";
      "11\u{FE0F}2";
      "ab#\u{FE0F}\u{20E3}cd";
      "abc\u{0301}de";
      "ab\u{1F1EB}\u{1F1F7}cd";
      "a中e\u{0301}👨\u{200D}💻xyz";
      "\u{0915}\u{094D}\u{0937}ab";
      "ab\r\ncd\tef";
      "a\u{200B}bc";
    ]
  in
  let rec utf8_cuts text i acc =
    if i >= String.length text then List.rev acc
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar text i) in
      utf8_cuts text (i + n) ((i + n) :: acc)
  in
  List.iter
    (fun m ->
      List.iter
        (fun text ->
          let whole = units m [ text ] in
          let msg cut =
            Printf.sprintf "%s %S cut at %d" (method_name m) text cut
          in
          equal ~msg:(msg 0) unit_list whole (units ~runs:true m [ text ]);
          List.iter
            (fun cut ->
              let len = String.length text in
              let chunks =
                [ String.sub text 0 cut; String.sub text cut (len - cut) ]
              in
              equal ~msg:(msg cut) unit_list whole (units m chunks);
              equal ~msg:(msg cut) unit_list whole (units ~runs:true m chunks))
            (utf8_cuts text 0 []))
        texts)
    methods

let segmenter_ascii_run () =
  let s = Segmenter.create `Unicode in
  equal ~msg:"leaves the last byte" int 3
    (Segmenter.ascii_run s "abcd" ~pos:0 ~len:4);
  equal ~msg:"a lone byte is no run" int 0
    (Segmenter.ascii_run s "d" ~pos:0 ~len:1);
  is_true ~msg:"the last byte starts the next unit"
    (Segmenter.add s (Uchar.of_char 'd'));
  ignore (Segmenter.add s (Uchar.of_int 0x0600) : bool);
  equal ~msg:"a Prepend character absorbs the next byte" int 0
    (Segmenter.ascii_run s "ab" ~pos:0 ~len:2);
  is_false ~msg:"which then joins it" (Segmenter.add s (Uchar.of_char 'a'));
  equal ~msg:"after it, a run" int 1 (Segmenter.ascii_run s "bc" ~pos:0 ~len:2);
  equal ~msg:"stops at a non-ASCII byte" int 1
    (Segmenter.ascii_run s "xy\u{0301}" ~pos:0 ~len:4);
  expect_invalid_arg "out of bounds" (fun () ->
      ignore (Segmenter.ascii_run s "ab" ~pos:1 ~len:2 : int))

let segmenter_reset () =
  let s = Segmenter.create `Unicode in
  ignore (Segmenter.add s (Uchar.of_char 'e') : bool);
  is_false ~msg:"a mark continues" (Segmenter.add s (Uchar.of_int 0x0301));
  Segmenter.reset s;
  is_true ~msg:"after reset, a mark starts a unit"
    (Segmenter.add s (Uchar.of_int 0x0301));
  equal ~msg:"of no width" int 0 (Segmenter.width s)

(* Width never decreases within a unit: from units in progress of every kind,
   add each scalar of the planes in use. *)
let segmenter_width_monotone () =
  let prefixes =
    [
      "";
      "a";
      "1";
      "❤";
      "中";
      "\u{0915}\u{094D}";
      "\u{1F1EB}";
      "👨\u{200D}";
      "e\u{0301}";
      "\u{0E19}";
      "\u{0600}";
      "\u{1100}";
    ]
  in
  let scalars =
    Seq.append (Seq.init 0x30000 Fun.id)
      (Seq.init 0x1000 (fun i -> 0xE0000 + i))
    |> Seq.filter (fun cp -> cp < 0xD800 || cp > 0xDFFF)
    |> Array.of_seq
  in
  List.iter
    (fun m ->
      List.iter
        (fun prefix ->
          let s = Segmenter.create m in
          Array.iter
            (fun cp ->
              Segmenter.reset s;
              let i = ref 0 in
              while !i < String.length prefix do
                let d = String.get_utf_8_uchar prefix !i in
                ignore (Segmenter.add s (Uchar.utf_decode_uchar d) : bool);
                i := !i + Uchar.utf_decode_length d
              done;
              let before = Segmenter.width s in
              if
                (not (Segmenter.add s (Uchar.of_int cp)))
                && Segmenter.width s < before
              then
                failf "%s: U+%04X narrows %S from %d to %d" (method_name m) cp
                  prefix before (Segmenter.width s))
            scalars)
        prefixes)
    methods

(* When [may_continue] rules a pair out, the scalar starts a unit after any
   text that ends with the other: after texts ending in each grapheme break
   class, and in each state the rules that look further back keep, add each
   scalar of the planes in use. *)
let segmenter_may_continue () =
  let prefixes =
    [
      "a";
      "中";
      "\u{0600}";
      "\u{1100}";
      "\u{1161}";
      "\u{AC00}";
      "\u{AC01}";
      "\u{11A8}";
      "\u{1F1EB}";
      "\u{1F1EB}\u{1F1F7}";
      "a\u{1F1EB}\u{1F1F7}\u{1F1EB}";
      "👨\u{200D}";
      "x\u{200D}";
      "👨\u{0301}\u{200D}";
      "\u{0915}\u{094D}";
      "\u{0915}\u{094D}\u{0301}";
      "\u{094D}";
      "e\u{0301}";
      "\u{0915}\u{0903}";
      "\u{200B}";
      "1\u{FE0F}";
    ]
  in
  let scalars =
    Seq.append (Seq.init 0x30000 Fun.id)
      (Seq.init 0x1000 (fun i -> 0xE0000 + i))
    |> Seq.filter (fun cp -> cp < 0xD800 || cp > 0xDFFF)
    |> Array.of_seq
  in
  List.iter
    (fun m ->
      List.iter
        (fun prefix ->
          let s = Segmenter.create m in
          let last = ref Uchar.min in
          Array.iter
            (fun cp ->
              let v = Uchar.of_int cp in
              Segmenter.reset s;
              let i = ref 0 in
              while !i < String.length prefix do
                let d = String.get_utf_8_uchar prefix !i in
                last := Uchar.utf_decode_uchar d;
                ignore (Segmenter.add s !last : bool);
                i := !i + Uchar.utf_decode_length d
              done;
              if
                (not (Segmenter.may_continue m !last v))
                && not (Segmenter.add s v)
              then
                failf "%s: U+%04X continues %S, which may_continue denies"
                  (method_name m) cp prefix)
            scalars)
        prefixes)
    methods

(* Every function that measures text agrees with the segmenter, also the
   loop that measure keeps apart for `Wcwidth. *)
let folds_agree_with_segmenter () =
  let texts =
    [
      "Hello 世界 👩\u{200D}🚀 cafe\u{0301} 🇫🇷 ";
      "\u{0D4E}ab\t\u{0600}1b";
      "น้ำXว่Y";
      "a\r\n\u{9B}b\u{200B}c";
      "\xF0\x9F(\xC3";
      "a1\u{FE0F}\u{20E3}#\u{FE0F}";
      "abc\u{0D4E}defg\u{0301}hi\u{0600}jk";
      "plain ascii text only";
      "中abcdefgh中";
      "中abcdefgh";
      "\u{0600}abc";
    ]
  in
  List.iter
    (fun m ->
      List.iter
        (fun text ->
          let msg = method_name m ^ " " ^ String.escaped text in
          let expected =
            (* Units of positive width, a TAB two cells wide. *)
            let s = Segmenter.create m in
            let out = ref [] and start = ref 0 and w = ref 0 in
            let i = ref 0 in
            let close stop =
              let w = if text.[!start] = '\t' then 2 else !w in
              if stop > !start && w > 0 then
                out := (!start, stop - !start, w) :: !out
            in
            while !i < String.length text do
              let d = String.get_utf_8_uchar text !i in
              let before = Segmenter.width s in
              if Segmenter.add s (Uchar.utf_decode_uchar d) then (
                w := before;
                close !i;
                start := !i);
              i := !i + Uchar.utf_decode_length d
            done;
            w := Segmenter.width s;
            close (String.length text);
            List.rev !out
          in
          let visited = ref [] in
          iter_units ~width_method:m ~tab_width:2
            (fun ~offset ~len ~width ->
              visited := (offset, len, width) :: !visited)
            text;
          equal ~msg:(msg ^ " iter_units")
            (list (triple int int int))
            expected (List.rev !visited);
          let total =
            List.fold_left (fun acc (_, _, w) -> acc + w) 0 expected
          in
          equal ~msg:(msg ^ " measure") int total
            (measure ~width_method:m ~tab_width:2 text);
          (* Every budget: the longest prefix of whole units that fits. *)
          for columns = 1 to total + 1 do
            let rec fit count used = function
              | (off, _, w) :: rest ->
                  if used + w > columns then (off, count, used)
                  else fit (count + 1) (used + w) rest
              | [] -> (String.length text, count, used)
            in
            let p =
              find_wrap_pos ~width_method:m ~tab_width:2 text
                ~max_columns:columns
            in
            equal
              ~msg:(Printf.sprintf "%s find_wrap_pos %d" msg columns)
              (triple int int int) (fit 0 0 expected)
              (p.byte_offset, p.units, p.columns_used)
          done;
          let p =
            find_wrap_pos ~width_method:m ~tab_width:2 text ~max_columns:max_int
          in
          equal ~msg:(msg ^ " find_wrap_pos") (pair int int)
            (List.length expected, total)
            (p.units, p.columns_used))
        texts)
    methods

let gcb_name : grapheme_cluster_break -> string = function
  | `CN -> "CN"
  | `CR -> "CR"
  | `EX -> "EX"
  | `L -> "L"
  | `LF -> "LF"
  | `LV -> "LV"
  | `LVT -> "LVT"
  | `PP -> "PP"
  | `RI -> "RI"
  | `SM -> "SM"
  | `T -> "T"
  | `V -> "V"
  | `XX -> "XX"
  | `ZWJ -> "ZWJ"

let grapheme_cluster_break_values () =
  let check cp expected =
    equal
      ~msg:(Printf.sprintf "U+%04X" cp)
      string expected
      (gcb_name (grapheme_cluster_break (Uchar.of_int cp)))
  in
  List.iter
    (fun (cp, v) -> check cp v)
    [
      (0x000D, "CR");
      (0x000A, "LF");
      (0x0000, "CN");
      (0x2028, "CN") (* LINE SEPARATOR *);
      (0x200B, "CN") (* ZERO WIDTH SPACE, a format character *);
      (0x0300, "EX");
      (0x200C, "EX") (* ZWNJ *);
      (0xFE0F, "EX");
      (0x1F3FB, "EX") (* an emoji modifier, E_Modifier before Unicode 11 *);
      (0x200D, "ZWJ");
      (0x0903, "SM");
      (0x0600, "PP");
      (0x1F1E6, "RI");
      (0x1100, "L");
      (0x1160, "V");
      (0x11A8, "T");
      (0xAC00, "LV");
      (0xAC01, "LVT");
      (0x0061, "XX");
      (0x1F466, "XX") (* BOY, E_Base before Unicode 11 *);
      (0x2764, "XX") (* HEAVY BLACK HEART, Glue_After_Zwj before *);
    ]

(* Every scalar's value agrees with how the segmenter joins it, by the rule
   each value is read by: GB4 and GB5 for controls, GB9 and GB9a for what
   continues the cluster before it, GB9b for Prepend. *)
let grapheme_cluster_break_agrees_with_segmentation () =
  let s cp = uchar_to_utf8 cp in
  for cp = 0 to 0x10FFFF do
    if Uchar.is_valid cp then (
      let v = grapheme_cluster_break (Uchar.of_int cp) in
      let msg rule =
        Printf.sprintf "U+%04X is %s, by %s" cp (gcb_name v) rule
      in
      let control = match v with `CN | `CR | `LF -> true | _ -> false in
      if control <> (grapheme_count (s cp ^ "\u{0300}") = 2) then
        fail (msg "GB4");
      let continues = match v with `EX | `ZWJ | `SM -> true | _ -> false in
      if continues <> (grapheme_count ("a" ^ s cp) = 1) then fail (msg "GB9");
      if v = `PP <> (grapheme_count (s cp ^ "a") = 1) then fail (msg "GB9b"))
  done

(* One rule for a tab width below 1, in measuring and in layouts. *)
let tab_width_below_one_raises () =
  let raises name f =
    raises_match ~msg:name (Exn.invalid_arg ~sub:"tab width") (fun () ->
        ignore (f () : int))
  in
  raises "measure" (fun () -> measure ~width_method:`Unicode ~tab_width:0 "\t");
  raises "measure_sub" (fun () ->
      measure_sub ~width_method:`Unicode ~tab_width:(-1) "\t" ~pos:0 ~len:1);
  raises "uchar_width" (fun () -> uchar_width ~tab_width:0 (Uchar.of_int 9));
  raises "Layout.create" (fun () ->
      Layout.row_count
        (Layout.create ~tab_width:0
           (Layout.Source.of_string "\t")
           ~width:10 ~wrap:`None));
  equal ~msg:"a tab width of 1" int 1
    (measure ~width_method:`Unicode ~tab_width:1 "\t")

let () =
  exit
    (run "matrix.text"
       [
         group "Layout"
           [
             test "wide grapheme positions and metric switches"
               wide_grapheme_positions;
             test "source boundaries retain invisible clusters and CRLF"
               layout_source_boundaries;
             test "truncation retains source coverage" layout_truncated_source;
             test "soft-wrap caret affinity" layout_affinity;
             test "whole clusters under both width methods"
               layout_whole_clusters;
             test "Thai spacing mark wrapping and source coverage"
               thai_spacing_mark_layout;
             test "zero-width snapping and visible iteration"
               layout_zero_width_snapping;
             test "bounded row and horizontal iteration"
               layout_bounded_iteration;
             test "measurement and rows agree at edges" layout_edge_cases;
           ];
         cases "Measured widths" measured_rows
           ~id:(fun r -> r.id ^ " " ^ r.what)
           check_measured;
         group "Conformance"
           [ test "UAX #29 grapheme boundaries" run_grapheme_conformance ];
         group "Measurement"
           [
             cases "semantics" measurement_rows
               ~id:(fun (name, _, _, _, _) -> name)
               (fun (name, input, width_method, tab_width, expected) ->
                 check_width name expected
                   (measure ~width_method ~tab_width input));
             test "ASCII fast path" ascii_fast_path_consistency;
             test "scalar width agrees with measurement"
               scalar_width_agrees_with_measurement;
             test "multi-grapheme regression" measure_multi_grapheme_regression;
             test "malformed UTF-8" malformed_utf8_measurement;
             test "measure_sub validation" measure_sub_validation;
             test "measure_sub matches independent slices"
               measure_sub_matches_slice;
             test "malformed wcwidth positions" malformed_wcwidth_positions;
             test "wcwidth decoder matches Stdlib"
               wcwidth_decoder_matches_stdlib;
             test "Unicode 18" unicode_18;
             test "grapheme cluster break values" grapheme_cluster_break_values;
             test "grapheme cluster break agrees with segmentation"
               grapheme_cluster_break_agrees_with_segmentation;
           ];
         group "Segmenter"
           [
             test "units" segmenter_units;
             test "split invariance" segmenter_split_invariance;
             test "ASCII runs" segmenter_ascii_run;
             test "reset" segmenter_reset;
             test "width never decreases" segmenter_width_monotone;
             test "pairs that never continue" segmenter_may_continue;
             test "folds agree" folds_agree_with_segmenter;
           ];
         group "Graphemes"
           [
             test "iteration" grapheme_iteration;
             test "info skips zero-width" grapheme_info_skips_zero_width;
             test "info keeps combining marks with ASCII base"
               grapheme_info_ascii_base_with_combining_mark;
           ];
         group "Tab width" [ test "below 1 raises" tab_width_below_one_raises ];
         group "Wrap Breaks"
           [
             cases "CJK opportunities" cjk_break_cases
               ~id:(fun (name, _, _, _) -> name)
               cjk_breaks;
             cases "CJK rows" cjk_layout_cases
               ~id:(fun (name, _, _, _) -> name)
               cjk_layout;
             test "ASCII spaces" wrap_breaks_ascii_spaces;
             test "ASCII punctuation" wrap_breaks_ascii_punctuation;
             test "periods stay inside tokens"
               wrap_breaks_keep_periods_inside_tokens;
             test "ASCII brackets" wrap_breaks_ascii_brackets;
             test "tabs" wrap_breaks_tabs;
             test "Unicode spaces" wrap_breaks_unicode_spaces;
             test "soft hyphen" wrap_breaks_soft_hyphen;
             test "EN space range" wrap_breaks_en_space_range;
             test "no break in plain text" wrap_breaks_no_break_in_plain_text;
             test "grapheme aware" wrap_breaks_grapheme_aware;
             test "empty string" wrap_breaks_empty_string;
             test "ZWJ sequence" wrap_breaks_zwj_sequence;
             test "ASCII space break point" wrap_break_points_ascii_space;
             test "Unicode space break point" wrap_break_points_unicode_space;
             test "mixed CJK/ASCII transitions"
               wrap_breaks_mixed_cjk_ascii_transitions;
             test "fullwidth CJK punctuation"
               wrap_breaks_fullwidth_cjk_punctuation;
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
         group "Positions"
           [
             test "width_at" width_at_positions;
             test "wcwidth semantics" wcwidth_position_semantics;
             test "unicode semantics" unicode_position_semantics;
           ];
         group "Width Tables"
           [
             test "emoji presentation widths" emoji_presentation_widths;
             test "text-default pictographs are narrow"
               text_default_pictograph_widths;
             test "C1 control widths" c1_control_widths;
             test "Hangul conjoining jamo are zero width"
               hangul_conjoining_widths;
           ];
       ])
