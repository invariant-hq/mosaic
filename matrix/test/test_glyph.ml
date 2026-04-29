open Windtrap
open Glyph

(* Helpers *)

let encode_to_list pool ~width_method ~tab_width str =
  let lst = ref [] in
  Pool.encode pool ~width_method ~tab_width
    (fun cell -> lst := cell :: !lst)
    str;
  List.rev !lst

let check_width msg expected actual = equal ~msg int expected actual

let expect_invalid_arg msg f =
  try
    f ();
    fail (msg ^ ": expected Invalid_argument")
  with Invalid_argument _ -> ()

(* 1. Conformance & Segmentation *)

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

let uchar_to_utf8 cp =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
  Buffer.contents buf

let run_grapheme_conformance () =
  let path = grapheme_break_test_path () in
  let ic = open_in path in
  try
    let rec loop lineno =
      try
        let line = input_line ic in
        let rule =
          match Stdlib.String.index_opt line '#' with
          | Some i -> Stdlib.String.sub line 0 i |> Stdlib.String.trim
          | None -> Stdlib.String.trim line
        in
        if rule <> "" then (
          let parts =
            Stdlib.String.split_on_char ' ' rule |> List.filter (( <> ) "")
          in
          (* Reconstruct full text and expected segments *)
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
                let cp = int_of_string ("0x" ^ hex) in
                let s = uchar_to_utf8 cp in
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
          String.iter_graphemes
            (fun ~offset:off ~len ->
              actual := Stdlib.String.sub text off len :: !actual)
            text;
          let actual = List.rev !actual in

          if expected <> actual then (
            Printf.printf
              "Line %d failure:\nInput: %s\nExpected: %s\nActual:   %s\n" lineno
              rule
              (Stdlib.String.concat "|" expected)
              (Stdlib.String.concat "|" actual);
            fail "Grapheme break mismatch"));
        loop (lineno + 1)
      with End_of_file -> ()
    in
    loop 1
  with e ->
    close_in ic;
    raise e

(* 2. Measurement Semantics *)

type measure_case = {
  name : string;
  input : string;
  expected : int;
  method_ : width_method;
  tab_width : int;
}

let measurement_semantics () =
  let cases =
    [
      (* ASCII *)
      {
        name = "basic_ascii";
        input = "abc";
        expected = 3;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "ascii_controls";
        input = "a\n\r\x00b";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      (* Tabs *)
      {
        name = "tab_default";
        input = "\t";
        expected = 2;
        method_ = `Unicode;
        tab_width = 2;
      };
      {
        name = "tab_custom";
        input = "\t";
        expected = 4;
        method_ = `Unicode;
        tab_width = 4;
      };
      (* Unicode Standard *)
      {
        name = "emoji_simple";
        input = "👋";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "emoji_zwj_seq";
        input = "👩\u{200D}🚀";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "emoji_family";
        input = "👨\u{200D}👩\u{200D}👧\u{200D}👦";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "regional_indicator";
        input = "🇺🇸";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "skin_tone";
        input = "👍🏽";
        expected = 2;
        method_ = `Unicode;
        tab_width = 8;
      };
      (* Strategies *)
      {
        name = "no_zwj_strategy";
        input = "👩\u{200D}🚀";
        expected = 4;
        method_ = `No_zwj;
        tab_width = 8;
      };
      {
        name = "wcwidth_strategy";
        input = "👩\u{200D}🚀";
        expected = 4;
        method_ = `Wcwidth;
        tab_width = 8;
      };
      (* Edge Cases *)
      {
        name = "combining_accents";
        input = "a\u{0301}";
        expected = 1;
        method_ = `Unicode;
        tab_width = 8;
      };
      {
        name = "zero_width_control";
        input = "\u{200B}";
        expected = 0;
        method_ = `Unicode;
        tab_width = 8;
      };
    ]
  in

  List.iter
    (fun c ->
      let w =
        String.measure ~width_method:c.method_ ~tab_width:c.tab_width c.input
      in
      check_width c.name c.expected w)
    cases

let ascii_fast_path_consistency () =
  let s = "hello\tworld" in
  let unicode_w = String.measure ~width_method:`Unicode ~tab_width:4 s in
  let no_zwj_w = String.measure ~width_method:`No_zwj ~tab_width:4 s in
  let wcwidth_w = String.measure ~width_method:`Wcwidth ~tab_width:4 s in
  check_width "unicode vs no_zwj" unicode_w no_zwj_w;
  check_width "unicode vs wcwidth" unicode_w wcwidth_w

(* Regression test: measure must iterate grapheme-by-grapheme and sum widths.
   Previously, measure for Unicode mode called calculate_width on the entire
   string, but calculate_width is designed for single grapheme clusters and only
   returned the first grapheme's width. *)
let measure_multi_grapheme_regression () =
  (* ASCII + wide characters: "Hello 世界" should be 6 + 2 + 2 = 10 *)
  let s = "Hello 世界" in
  let w = String.measure ~width_method:`Unicode ~tab_width:2 s in
  check_width "ascii + wide chars" 10 w;

  (* Multiple wide characters *)
  let s2 = "日本語" in
  let w2 = String.measure ~width_method:`Unicode ~tab_width:2 s2 in
  check_width "three wide chars" 6 w2;

  (* Mixed: emoji + ascii + wide *)
  let s3 = "🚀Hi世" in
  let w3 = String.measure ~width_method:`Unicode ~tab_width:2 s3 in
  (* 🚀=2, H=1, i=1, 世=2 = 6 *)
  check_width "emoji + ascii + wide" 6 w3;

  (* Verify consistency with Wcwidth mode *)
  let w_wcwidth = String.measure ~width_method:`Wcwidth ~tab_width:2 s in
  check_width "unicode matches wcwidth for simple case" w w_wcwidth

let no_zwj_segmentation () =
  let pool = Pool.create () in
  let text = "👩\u{200D}🚀" in
  let cells = encode_to_list pool ~width_method:`No_zwj ~tab_width:2 text in
  let starts = List.filter is_start cells in
  equal ~msg:"two grapheme starts" int 2 (List.length starts);
  equal ~msg:"four cells" int 4 (List.length cells);
  let total =
    List.fold_left
      (fun acc g ->
        if is_start g then acc + grapheme_width ~tab_width:8 g else acc)
      0 cells
  in
  check_width "total width preserved (starts only)" 4 total

(* 3. Encoding & Storage *)

let simple_vs_complex_optimization () =
  let pool = Pool.create () in
  let check_type str expect_simple =
    let lst = encode_to_list pool ~width_method:`Unicode ~tab_width:2 str in
    match lst with
    | g :: _ ->
        if is_inline g <> expect_simple then
          fail
            (Printf.sprintf "Expected %s to be %s" str
               (if expect_simple then "Simple" else "Complex"))
    | [] -> fail (Printf.sprintf "Expected glyph for %s" str)
  in
  check_type "a" true;
  check_type "\t" true;
  (* Tabs remain simple encoded; control bytes like DEL are dropped *)
  let del_cells =
    encode_to_list pool ~width_method:`Unicode ~tab_width:2 "\x7F"
  in
  equal ~msg:"DEL is zero-width and skipped" int 0 (List.length del_cells);
  (* Single codepoints are stored as simple glyphs (no pool allocation) *)
  check_type "é" true;
  check_type "€" true;
  check_type "中" true

let zero_length_intern () =
  let pool = Pool.create () in
  let g = Pool.intern pool "" in
  is_true ~msg:"empty glyph" (is_empty g);
  equal ~msg:"empty width" int 0 (grapheme_width g);
  (* Empty maps to a NUL sentinel: zero width, single-byte payload *)
  equal ~msg:"empty len" int 1 (Pool.length pool g);
  equal ~msg:"empty string" string "\000" (Pool.to_string pool g)

let complex_clustering_behavior () =
  let pool = Pool.create () in
  (* Devanagari: "Namaste" *)
  (* 'Na'(1) + 'Ma'(1) + 'S'(1)+Virama(0)+'Ta'(1)+Matra(0) -> fused cluster 'Ste'(2) *)
  let text = "नमस्ते" in
  let cells = encode_to_list pool ~width_method:`Unicode ~tab_width:2 text in

  match cells with
  | [ na; _ma; ste_start; ste_cont ] ->
      check_width "Na width" 1 (grapheme_width na);
      is_true ~msg:"Na simple (single codepoint)" (is_inline na);

      check_width "Ste width" 2 (grapheme_width ste_start);
      is_true ~msg:"Ste start" (is_start ste_start);

      is_true ~msg:"Ste cont flag" (is_continuation ste_cont);
      check_width "Ste cont width" 2 (grapheme_width ste_cont);

      (* Verify visual re-assembly *)
      let buf = Bytes.create 32 in
      let len = Pool.blit pool ste_start buf ~pos:0 in
      let s_out = Bytes.sub_string buf 0 len in
      equal ~msg:"Ste content" string "स्ते" s_out
  | _ -> fail "Unexpected clustering result for Devanagari"

let malformed_utf8_resilience () =
  let pool = Pool.create () in
  (* Truncated sequence: C3 (start of 2-byte seq) without second byte *)
  let invalid = "\xC3" in
  try
    let cells =
      encode_to_list pool ~width_method:`Unicode ~tab_width:2 invalid
    in
    let w = List.fold_left (fun acc g -> acc + grapheme_width g) 0 cells in
    is_true ~msg:"produced output without crash" (w >= 0)
  with _ -> fail "Crashed on invalid UTF-8"

let malformed_utf8_intern_replaces_with_ufffd () =
  let pool = Pool.create () in
  let invalid = "\xC3" in
  let ufffd = uchar_to_utf8 0xFFFD in

  let g = Pool.intern pool invalid in
  equal ~msg:"intern malformed byte -> U+FFFD" string ufffd
    (Pool.to_string pool g);
  equal ~msg:"intern malformed width" int 1 (grapheme_width g);

  let g_sub =
    Pool.intern_sub pool ~width_method:`Unicode ~tab_width:2 invalid ~pos:0
      ~len:1 ~width:1
  in
  equal ~msg:"intern_sub malformed byte -> U+FFFD" string ufffd
    (Pool.to_string pool g_sub);
  equal ~msg:"intern_sub malformed width" int 1 (grapheme_width g_sub)

let malformed_utf8_intern_normalizes_mixed_strings () =
  let pool = Pool.create () in
  let ufffd = uchar_to_utf8 0xFFFD in

  let g = Pool.intern pool ("\xC3" ^ "(") in
  equal ~msg:"intern malformed prefix normalizes" string (ufffd ^ "(")
    (Pool.to_string pool g);
  equal ~msg:"intern malformed prefix width" int 2 (grapheme_width g);

  let g_mid = Pool.intern pool ("a" ^ "\x80" ^ "b") in
  equal ~msg:"intern malformed middle normalizes" string
    ("a" ^ ufffd ^ "b")
    (Pool.to_string pool g_mid);
  equal ~msg:"intern malformed middle width" int 3 (grapheme_width g_mid);

  let input = "x" ^ "\xC3" ^ "(" ^ "y" in
  let g_sub =
    Pool.intern_sub pool ~width_method:`Unicode ~tab_width:2 input ~pos:1 ~len:2
      ~width:2
  in
  equal ~msg:"intern_sub malformed slice normalizes" string (ufffd ^ "(")
    (Pool.to_string pool g_sub)

let public_slice_range_validation () =
  let pool = Pool.create () in
  expect_invalid_arg "intern_sub negative pos" (fun () ->
      ignore
        (Pool.intern_sub pool ~width_method:`Unicode ~tab_width:2 "abc"
           ~pos:(-1) ~len:1 ~width:1));
  expect_invalid_arg "intern_sub oversized len" (fun () ->
      ignore
        (Pool.intern_sub pool ~width_method:`Unicode ~tab_width:2 "abc" ~pos:1
           ~len:3 ~width:3));
  expect_invalid_arg "measure_sub negative pos" (fun () ->
      ignore
        (String.measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:(-1)
           ~len:1));
  expect_invalid_arg "measure_sub oversized len" (fun () ->
      ignore
        (String.measure_sub ~width_method:`Unicode ~tab_width:2 "abc" ~pos:1
           ~len:3));
  expect_invalid_arg "blit negative pos" (fun () ->
      let buf = Bytes.create 8 in
      ignore (Pool.blit pool space buf ~pos:(-1)));
  expect_invalid_arg "blit pos past end" (fun () ->
      let buf = Bytes.create 8 in
      ignore (Pool.blit pool space buf ~pos:9))

(* 4. Lifecycle & Pool *)

let lifecycle_generation_safety () =
  let pool = Pool.create () in
  (* Use multi-codepoint clusters to exercise pool allocation *)
  let g1 = Pool.intern pool "e\u{0301}" in
  is_false ~msg:"is complex" (is_inline g1);

  equal ~msg:"content exists" string "e\u{0301}" (Pool.to_string pool g1);

  Pool.decref pool g1;

  let g2 = Pool.intern pool "a\u{0302}" in
  equal ~msg:"new content correct" string "a\u{0302}" (Pool.to_string pool g2);

  (* Stale reference should return safe defaults *)
  equal ~msg:"stale string empty" string "" (Pool.to_string pool g1);
  let buf = Bytes.create 8 in
  equal ~msg:"stale blit is empty" int 0 (Pool.blit pool g1 buf ~pos:0)

let pool_clear_invalidates_old_ids () =
  let pool = Pool.create () in
  let old_g = Pool.intern pool "e\u{0301}" in
  equal ~msg:"old content exists" string "e\u{0301}" (Pool.to_string pool old_g);
  Pool.clear pool;
  let new_g = Pool.intern pool "a\u{0302}" in
  equal ~msg:"new content exists" string "a\u{0302}" (Pool.to_string pool new_g);
  equal ~msg:"old glyph invalid after clear and reuse" string ""
    (Pool.to_string pool old_g)

let pool_resize_stress () =
  let pool = Pool.create () in
  let n = 5000 in
  (* Exceeds default 4096 *)
  let handles = Array.make n empty in

  (* Fill pool *)
  for i = 0 to n - 1 do
    (* Use unique strings to force separate allocations *)
    let s = "x" ^ string_of_int i in
    handles.(i) <- Pool.intern pool s
  done;

  (* Verify random access after resize *)
  let g_first = handles.(0) in
  equal ~msg:"first element intact" string "x0" (Pool.to_string pool g_first);

  let g_last = handles.(n - 1) in
  equal ~msg:"last element intact" string
    ("x" ^ string_of_int (n - 1))
    (Pool.to_string pool g_last)

let copy_safety () =
  let p1 = Pool.create () in
  let p2 = Pool.create () in

  let g = Pool.intern p1 "abc" in
  (* Simple *)
  let g_copy = Pool.copy ~src:p1 g ~dst:p2 in
  equal ~msg:"simple copy identity" int (to_int g) (to_int g_copy);

  (* Use multi-codepoint cluster to exercise pool copy *)
  let complex = Pool.intern p1 "e\u{0301}" in
  Pool.decref p1 complex;

  let _reused = Pool.intern p1 "reused" in
  let stale_copy = Pool.copy ~src:p1 complex ~dst:p2 in
  is_true ~msg:"copied stale is empty" (is_empty stale_copy)

let decref_deduplicates_free_list () =
  (* Test that multiple decrements don't corrupt the pool *)
  let pool = Pool.create () in
  let g = Pool.intern pool "wide" in
  Pool.incref pool g;
  Pool.incref pool g;
  Pool.decref pool g;
  Pool.decref pool g;
  Pool.decref pool g;
  (* Should be able to allocate new glyphs without issues *)
  let g2 = Pool.intern pool "next" in
  let g3 = Pool.intern pool "another" in
  equal ~msg:"g2 content" string "next" (Pool.to_string pool g2);
  equal ~msg:"g3 content" string "another" (Pool.to_string pool g3);
  (* Stale reference should return empty *)
  equal ~msg:"stale g empty" string "" (Pool.to_string pool g)

let live_complex_graphemes_are_interned () =
  let pool = Pool.create () in
  let g1 = Pool.intern pool "e\u{0301}" in
  Pool.incref pool g1;
  let g2 = Pool.intern pool "e\u{0301}" in
  equal ~msg:"live duplicate pool key" (option int) (pool_key g1) (pool_key g2);
  Pool.incref pool g2;
  Pool.decref pool g1;
  Pool.decref pool g2;
  let g3 = Pool.intern pool "e\u{0301}" in
  Pool.incref pool g3;
  equal ~msg:"content remains correct after reintern" string "e\u{0301}"
    (Pool.to_string pool g3);
  Pool.decref pool g3

let live_complex_interning_is_not_just_last_entry () =
  let pool = Pool.create () in
  let g1 = Pool.intern pool "e\u{0301}" in
  Pool.incref pool g1;
  let other = Pool.intern pool "a\u{0302}" in
  Pool.incref pool other;
  let g2 = Pool.intern pool "e\u{0301}" in
  equal ~msg:"non-adjacent live duplicate pool key" (option int) (pool_key g1)
    (pool_key g2);
  Pool.decref pool g1;
  Pool.decref pool other

let live_complex_interning_survives_table_growth () =
  let pool = Pool.create () in
  let first = "e\u{0301}:0" in
  let first_g = Pool.intern pool first in
  Pool.incref pool first_g;
  for i = 1 to 600 do
    let g = Pool.intern pool ("e\u{0301}:" ^ string_of_int i) in
    Pool.incref pool g
  done;
  let again = Pool.intern pool first in
  equal ~msg:"live duplicate after table growth" (option int) (pool_key first_g)
    (pool_key again)

let pool_capacity_reuse () =
  (* Regression: alloc_string used to overwrite capacity with actual length,
     causing storage fragmentation when a freed slot was reused for shorter data
     then freed again for medium-length data. Verify that repeated
     alloc-free-reuse cycles produce correct content. *)
  let pool = Pool.create () in
  (* Intern a long complex string (forces pool allocation) *)
  let long = Pool.intern pool "ABCDEFGH\u{0301}" in
  is_false ~msg:"long is complex" (is_inline long);
  equal ~msg:"long content" string "ABCDEFGH\u{0301}" (Pool.to_string pool long);
  Pool.decref pool long;
  (* Slot is freed. Intern a short complex string to reuse the slot. *)
  let short = Pool.intern pool "xy\u{0302}" in
  is_false ~msg:"short is complex" (is_inline short);
  equal ~msg:"short content" string "xy\u{0302}" (Pool.to_string pool short);
  Pool.decref pool short;
  (* Freed again. Now intern a medium-length string that fits the original
     capacity but is longer than the short string. If capacities are tracked
     correctly, the medium string should reuse the original allocation. *)
  let medium = Pool.intern pool "ABCDE\u{0303}" in
  is_false ~msg:"medium is complex" (is_inline medium);
  equal ~msg:"medium content" string "ABCDE\u{0303}"
    (Pool.to_string pool medium);
  (* Repeat the cycle many times to stress capacity tracking *)
  Pool.decref pool medium;
  for i = 0 to 9 do
    let s = Printf.sprintf "%d\u{0301}" i in
    let g = Pool.intern pool s in
    equal
      ~msg:(Printf.sprintf "cycle %d content" i)
      string s (Pool.to_string pool g);
    Pool.decref pool g
  done

let iter_graphemes_ignore_zwj () =
  (* Woman astronaut ZWJ sequence: woman + ZWJ + rocket *)
  let zwj_seq = "\u{1F469}\u{200D}\u{1F680}" in
  let count_default = ref 0 in
  String.iter_graphemes (fun ~offset:_ ~len:_ -> incr count_default) zwj_seq;
  let count_no_zwj = ref 0 in
  String.iter_graphemes ~ignore_zwj:true
    (fun ~offset:_ ~len:_ -> incr count_no_zwj)
    zwj_seq;
  equal ~msg:"default: 1 grapheme (ZWJ joins)" int 1 !count_default;
  equal ~msg:"ignore_zwj: 2 graphemes (ZWJ breaks)" int 2 !count_no_zwj

(* 5. API Surface *)

let tab_expansion_mechanics () =
  let pool = Pool.create () in
  (* Test mixing tabs with content *)
  let cells_2 =
    encode_to_list pool ~width_method:`Unicode ~tab_width:2 "a\tb"
  in
  let w_2 =
    List.fold_left (fun acc g -> acc + grapheme_width ~tab_width:2 g) 0 cells_2
  in
  equal ~msg:"width with tab=2" int (1 + 2 + 1) w_2;

  let cells_4 =
    encode_to_list pool ~width_method:`Unicode ~tab_width:4 "a\tb"
  in
  let w_4 =
    List.fold_left (fun acc g -> acc + grapheme_width ~tab_width:4 g) 0 cells_4
  in
  equal ~msg:"width with tab=4" int (1 + 4 + 1) w_4;

  (* Ensure the tab glyph itself reports the correct width dynamically *)
  let tab_glyph = List.nth cells_4 1 in
  equal ~msg:"tab glyph dynamic width" int 4
    (grapheme_width ~tab_width:4 tab_glyph);
  equal ~msg:"tab glyph dynamic width re-check" int 8
    (grapheme_width ~tab_width:8 tab_glyph)

let tab_cache_offsets () =
  let pool = Pool.create () in
  let cells =
    encode_to_list pool ~width_method:`Unicode ~tab_width:4 "a\tb\tc"
  in
  match cells with
  | [ _a; tab1; _b; tab2; _c ] ->
      is_true ~msg:"tab1 simple" (is_inline tab1);
      is_true ~msg:"tab2 simple" (is_inline tab2);
      equal ~msg:"tab1 width" int 4 (grapheme_width ~tab_width:4 tab1);
      equal ~msg:"tab2 width" int 4 (grapheme_width ~tab_width:4 tab2)
  | _ -> fail "Expected five glyphs with tabs at positions 1 and 3"

let max_width_clamping () =
  let pool = Pool.create () in
  (* Force a width > 4 on a multi-byte grapheme; bit layout clamps to max 4. *)
  let g =
    Pool.intern_sub pool ~width_method:`Unicode ~tab_width:2 "ab" ~pos:0 ~len:2
      ~width:10
  in
  equal ~msg:"clamps large width" int 4 (grapheme_width g)

let continuation_width_encoding () =
  let pool = Pool.create () in
  let cells = encode_to_list pool ~width_method:`Unicode ~tab_width:2 "界" in
  (* CJK wide character -> width 2 and a continuation cell *)
  match cells with
  | [ start; cont ] ->
      check_width "start width" 2 (grapheme_width start);
      is_true ~msg:"start is start" (is_start start);
      is_true ~msg:"cont flag" (is_continuation cont);
      check_width "cont width matches" 2 (grapheme_width cont)
  | _ -> fail "Expected start+continuation for wide glyph"

(* 6. Text Segmentation (wrap_breaks / line_breaks) *)

(* Local test-only types for easy comparison *)
type wrap_break = { byte_offset : int; grapheme_offset : int }
type line_break = { pos : int; kind : line_break_kind }

(* Local helper: convert iter_wrap_breaks to array for tests *)
let wrap_breaks ?(width_method = `Unicode) s =
  let acc = ref [] in
  String.iter_wrap_breaks ~width_method
    (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset ->
      acc := { byte_offset = next_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

let wrap_break_points ?(width_method = `Unicode) s =
  let acc = ref [] in
  String.iter_wrap_breaks ~width_method
    (fun ~break_byte_offset ~next_byte_offset:_ ~grapheme_offset ->
      acc := { byte_offset = break_byte_offset; grapheme_offset } :: !acc)
    s;
  Array.of_list (List.rev !acc)

(* Local helper: convert iter_line_breaks to list for tests *)
let line_breaks_to_list s =
  let acc = ref [] in
  String.iter_line_breaks (fun ~pos ~kind -> acc := { pos; kind } :: !acc) s;
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
  (* Break after each space *)
  let breaks = wrap_breaks "hello world test" in
  equal ~msg:"two breaks for two spaces" int 2 (Array.length breaks);
  equal ~msg:"first break after 'hello '" wrap_break_testable
    { byte_offset = 6; grapheme_offset = 5 }
    breaks.(0);
  equal ~msg:"second break after 'world '" wrap_break_testable
    { byte_offset = 12; grapheme_offset = 11 }
    breaks.(1)

let wrap_breaks_ascii_punctuation () =
  (* Breaks after punctuation: - / . , ; : ! ? *)
  let breaks = wrap_breaks "a-b/c.d,e;f:g!h?i" in
  (* Each punctuation mark should create a break *)
  equal ~msg:"8 breaks for 8 punctuation marks" int 8 (Array.length breaks);
  (* First break after 'a-' at byte 2, grapheme 1 *)
  equal ~msg:"break after hyphen" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_ascii_brackets () =
  (* Breaks after brackets: ( ) [ ] { } *)
  let breaks = wrap_breaks "(a)[b]{c}" in
  equal ~msg:"6 breaks for 6 brackets" int 6 (Array.length breaks)

let wrap_breaks_tabs () =
  let breaks = wrap_breaks "a\tb\tc" in
  equal ~msg:"2 breaks for 2 tabs" int 2 (Array.length breaks);
  equal ~msg:"break after first tab" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_unicode_spaces () =
  (* NBSP (U+00A0), ZWSP (U+200B), Ideographic space (U+3000) *)
  let nbsp = "\xC2\xA0" in
  (* U+00A0 *)
  let zwsp = "\xE2\x80\x8B" in
  (* U+200B *)
  let ideographic = "\xE3\x80\x80" in
  (* U+3000 *)

  let breaks_nbsp = wrap_breaks ("a" ^ nbsp ^ "b") in
  equal ~msg:"break after NBSP" int 1 (Array.length breaks_nbsp);

  let breaks_zwsp = wrap_breaks ("a" ^ zwsp ^ "b") in
  equal ~msg:"break after ZWSP" int 1 (Array.length breaks_zwsp);

  let breaks_ideo = wrap_breaks ("a" ^ ideographic ^ "b") in
  equal ~msg:"break after ideographic space" int 1 (Array.length breaks_ideo)

let wrap_breaks_soft_hyphen () =
  (* U+00AD soft hyphen *)
  let soft_hyphen = "\xC2\xAD" in
  let breaks = wrap_breaks ("word" ^ soft_hyphen ^ "break") in
  equal ~msg:"break after soft hyphen" int 1 (Array.length breaks)

let wrap_breaks_en_space_range () =
  (* U+2000 to U+200A are all break opportunities *)
  let en_quad = "\xE2\x80\x80" in
  (* U+2000 *)
  let hair_space = "\xE2\x80\x8A" in
  (* U+200A *)

  let breaks1 = wrap_breaks ("a" ^ en_quad ^ "b") in
  equal ~msg:"break after EN QUAD" int 1 (Array.length breaks1);

  let breaks2 = wrap_breaks ("a" ^ hair_space ^ "b") in
  equal ~msg:"break after HAIR SPACE" int 1 (Array.length breaks2)

let wrap_breaks_no_break_in_plain_text () =
  let breaks = wrap_breaks "helloworld" in
  equal ~msg:"no breaks in continuous text" int 0 (Array.length breaks)

let wrap_breaks_grapheme_aware () =
  (* Emoji with skin tone modifier is one grapheme *)
  let emoji = "👍🏽" in
  (* thumbs up + skin tone *)
  let breaks = wrap_breaks ("a " ^ emoji ^ " b") in
  (* Should have breaks after "a " and after emoji+" " *)
  equal ~msg:"breaks respect grapheme boundaries" int 2 (Array.length breaks);
  (* grapheme_offset should count the emoji as 1 grapheme *)
  equal ~msg:"first break" wrap_break_testable
    { byte_offset = 2; grapheme_offset = 1 }
    breaks.(0)

let wrap_breaks_empty_string () =
  let breaks = wrap_breaks "" in
  equal ~msg:"no breaks in empty string" int 0 (Array.length breaks)

let wrap_breaks_width_method_no_zwj () =
  (* With No_zwj, ZWJ sequences are split into separate graphemes *)
  let zwj_seq = "👩\u{200D}🚀" in
  (* woman + ZWJ + rocket *)

  let breaks_unicode = wrap_breaks ~width_method:`Unicode zwj_seq in
  let breaks_no_zwj = wrap_breaks ~width_method:`No_zwj zwj_seq in

  (* In Unicode mode: 1 grapheme, no breaks (no break chars) *)
  equal ~msg:"unicode: no breaks in ZWJ sequence" int 0
    (Array.length breaks_unicode);
  (* In No_zwj mode: ZWJ (U+200D) could be treated as separate, but it's not a wrap break char *)
  (* The grapheme count will differ though *)
  equal ~msg:"no_zwj: still no breaks (ZWJ not a wrap break char)" int 0
    (Array.length breaks_no_zwj)

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
  (* Ported from OpenTUI utf8_test.zig:
     "wrap breaks: CJK to ASCII script transition" and
     "wrap breaks: ASCII to CJK script transition". *)
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
  (* Ported from OpenTUI utf8_test.zig:
     "wrap breaks: CJK punctuation before ASCII". *)
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
    let breaks = wrap_breaks s in
    equal ~msg:label int 1 (Array.length breaks)
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
  (* CRLF reports at LF position *)
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
  let breaks = line_breaks_to_list "" in
  equal ~msg:"no breaks in empty" int 0 (List.length breaks)

let line_breaks_no_newlines () =
  let breaks = line_breaks_to_list "hello world" in
  equal ~msg:"no breaks without newlines" int 0 (List.length breaks)

let line_breaks_consecutive_lf () =
  let breaks = line_breaks_to_list "a\n\n\nb" in
  equal ~msg:"three consecutive LF" int 3 (List.length breaks);
  equal ~msg:"first" line_break_testable { pos = 1; kind = `LF }
    (List.nth breaks 0);
  equal ~msg:"second" line_break_testable { pos = 2; kind = `LF }
    (List.nth breaks 1);
  equal ~msg:"third" line_break_testable { pos = 3; kind = `LF }
    (List.nth breaks 2)

(* 7. Emoji Presentation Width Audit *)

(* Verify that common emoji and symbols are measured as width 2. Terminal
   emulators generally render these in two columns. This catches any gaps in the
   tty_width_hint table. *)
let emoji_presentation_widths () =
  let check label cp expected_w =
    let s = uchar_to_utf8 cp in
    let w = String.measure ~width_method:`Unicode ~tab_width:2 s in
    check_width label expected_w w
  in
  (* CJK Unified Ideographs *)
  check "CJK U+4E2D" 0x4E2D 2;
  check "CJK U+3000 ideographic space" 0x3000 2;
  (* Misc symbols commonly rendered wide *)
  check "watch U+231A" 0x231A 2;
  check "hourglass U+231B" 0x231B 2;
  (* Dingbats *)
  check "check mark U+2705" 0x2705 2;
  check "raised fist U+270A" 0x270A 2;
  check "sparkles U+2728" 0x2728 2;
  check "cross mark U+274C" 0x274C 2;
  check "question marks U+2753" 0x2753 2;
  check "exclamation U+2757" 0x2757 2;
  check "plus U+2795" 0x2795 2;
  (* Emoticons *)
  check "rainbow U+1F308" 0x1F308 2;
  check "rocket U+1F680" 0x1F680 2;
  check "wave U+1F44B" 0x1F44B 2;
  check "thumbs up U+1F44D" 0x1F44D 2;
  check "face with tears U+1F602" 0x1F602 2;
  check "red heart U+2764" 0x2764 2;
  check "red heart + VS16 base" 0x2764 2;
  (* VS16 promotion to width 2 happens at grapheme level *)
  let heart_vs16 = uchar_to_utf8 0x2764 ^ uchar_to_utf8 0xFE0F in
  let w_heart = String.measure ~width_method:`Unicode ~tab_width:2 heart_vs16 in
  check_width "heart with VS16 = width 2" 2 w_heart;
  (* Regional indicator pairs *)
  let flag = uchar_to_utf8 0x1F1FA ^ uchar_to_utf8 0x1F1F8 in
  let w_flag = String.measure ~width_method:`Unicode ~tab_width:2 flag in
  check_width "flag pair = width 2" 2 w_flag;
  (* Transport/map symbols *)
  check "umbrella U+2614" 0x2614 2;
  check "hot beverage U+2615" 0x2615 2;
  (* Geometric shapes *)
  check "white medium square U+25FD" 0x25FD 2;
  (* Mahjong tile *)
  check "mahjong U+1F004" 0x1F004 2;
  (* Playing card *)
  check "playing card U+1F0CF" 0x1F0CF 2;
  (* Skin tone modifier - zero width when standalone *)
  check "skin tone U+1F3FD" 0x1F3FD 2;
  (* Food & drink *)
  check "pizza U+1F355" 0x1F355 2;
  (* Animals *)
  check "dog face U+1F436" 0x1F436 2;
  (* Recent emoji *)
  check "melting face U+1FAE0" 0x1FAE0 2

(* C1 control characters (U+0080-U+009F) must have width 0 (they are control
   codes, not printable). *)
let c1_control_widths () =
  let check_zero label cp =
    let s = uchar_to_utf8 cp in
    let w = String.measure ~width_method:`Unicode ~tab_width:2 s in
    check_width label 0 w
  in
  check_zero "C1 U+0080" 0x0080;
  check_zero "C1 U+008A" 0x008A;
  check_zero "C1 U+009F" 0x009F

let () =
  run "matrix.glyph"
    [
      group "Conformance"
        [ test "UAX #29 Grapheme Boundaries" run_grapheme_conformance ];
      group "Text Segmentation"
        [
          test "wrap breaks: ASCII spaces" wrap_breaks_ascii_spaces;
          test "wrap breaks: ASCII punctuation" wrap_breaks_ascii_punctuation;
          test "wrap breaks: ASCII brackets" wrap_breaks_ascii_brackets;
          test "wrap breaks: tabs" wrap_breaks_tabs;
          test "wrap breaks: Unicode spaces" wrap_breaks_unicode_spaces;
          test "wrap breaks: soft hyphen" wrap_breaks_soft_hyphen;
          test "wrap breaks: EN space range" wrap_breaks_en_space_range;
          test "wrap breaks: no break in plain text"
            wrap_breaks_no_break_in_plain_text;
          test "wrap breaks: grapheme aware" wrap_breaks_grapheme_aware;
          test "wrap breaks: empty string" wrap_breaks_empty_string;
          test "wrap breaks: width_method No_zwj"
            wrap_breaks_width_method_no_zwj;
          test "wrap break points: ASCII space" wrap_break_points_ascii_space;
          test "wrap break points: Unicode NBSP" wrap_break_points_unicode_space;
          test "wrap breaks: mixed CJK/ASCII transitions"
            wrap_breaks_mixed_cjk_ascii_transitions;
          test "wrap breaks: fullwidth CJK punctuation"
            wrap_breaks_fullwidth_cjk_punctuation;
          test "line breaks: LF" line_breaks_lf;
          test "line breaks: CR" line_breaks_cr;
          test "line breaks: CRLF" line_breaks_crlf;
          test "line breaks: mixed" line_breaks_mixed;
          test "line breaks: empty" line_breaks_empty;
          test "line breaks: no newlines" line_breaks_no_newlines;
          test "line breaks: consecutive LF" line_breaks_consecutive_lf;
        ];
      group "Measurement Semantics"
        [
          test "width tables" measurement_semantics;
          test "ascii fast path" ascii_fast_path_consistency;
          test "multi-grapheme measure" measure_multi_grapheme_regression;
          test "no zwj segmentation" no_zwj_segmentation;
        ];
      group "Encoding & Storage"
        [
          test "simple vs complex" simple_vs_complex_optimization;
          test "zero length intern" zero_length_intern;
          test "complex clustering" complex_clustering_behavior;
          test "malformed utf8" malformed_utf8_resilience;
          test "malformed utf8 intern replacement"
            malformed_utf8_intern_replaces_with_ufffd;
          test "malformed utf8 intern mixed strings"
            malformed_utf8_intern_normalizes_mixed_strings;
          test "public slice range validation" public_slice_range_validation;
        ];
      group "Lifecycle & Pool"
        [
          test "generation safety" lifecycle_generation_safety;
          test "clear invalidates old IDs" pool_clear_invalidates_old_ids;
          test "pool resize" pool_resize_stress;
          test "copy safety" copy_safety;
          test "decref deduplication" decref_deduplicates_free_list;
          test "live complex graphemes are interned"
            live_complex_graphemes_are_interned;
          test "live complex interning finds non-adjacent entries"
            live_complex_interning_is_not_just_last_entry;
          test "live complex interning survives table growth"
            live_complex_interning_survives_table_growth;
          test "capacity reuse" pool_capacity_reuse;
          test "iter_graphemes ignore_zwj" iter_graphemes_ignore_zwj;
        ];
      group "API Surface"
        [
          test "tab expansion" tab_expansion_mechanics;
          test "tab cache offsets" tab_cache_offsets;
          test "max width clamping" max_width_clamping;
          test "continuation width encoding" continuation_width_encoding;
        ];
      group "Width Tables"
        [
          test "emoji presentation widths" emoji_presentation_widths;
          test "C1 control widths" c1_control_widths;
        ];
    ]
