open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let span_text (s : Text_buffer.span) = s.text

let make_surface ?(width = 40) ?(height = 10) ?content ?wrap () =
  let t = make_ctx () in
  let root = make_root t in
  let node = Renderable.create ~parent:root () in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let buffer = Text_buffer.create () in
  (match content with Some s -> Text_buffer.set_text buffer s | None -> ());
  let surface = Text_surface.create node buffer in
  (match wrap with
  | Some mode -> Text_surface.set_wrap surface mode
  | None -> ());
  surface

(* ── Construction ── *)

let defaults () =
  let s = make_surface () in
  equal ~msg:"wrap" int 0
    (match Text_surface.wrap s with `None -> 0 | `Char -> 1 | `Word -> 2);
  equal ~msg:"scroll_x" int 0 (Text_surface.scroll_x s);
  equal ~msg:"scroll_y" int 0 (Text_surface.scroll_y s);
  is_false ~msg:"no selection" (Text_surface.has_selection s);
  is_false ~msg:"truncate" (Text_surface.truncate s)

(* ── Wrapping — None ── *)

let wrap_none_single_line () =
  let s = make_surface ~content:"hello world" () in
  equal ~msg:"display_line_count" int 1 (Text_surface.display_line_count s)

let wrap_none_multiline () =
  let s = make_surface ~content:"line1\nline2\nline3" () in
  equal ~msg:"display_line_count" int 3 (Text_surface.display_line_count s)

(* ── Wrapping — Char ── *)

let wrap_char_splits () =
  (* 10 chars wide, content is 20 chars -> should wrap to 2 display lines *)
  let s =
    make_surface ~width:10 ~content:"abcdefghijklmnopqrst" ~wrap:`Char ()
  in
  equal ~msg:"display_line_count" int 2 (Text_surface.display_line_count s)

let wrap_char_short_line_no_wrap () =
  let s = make_surface ~width:40 ~content:"short" ~wrap:`Char () in
  equal ~msg:"display_line_count" int 1 (Text_surface.display_line_count s)

let wrap_char_line_sources () =
  let s = make_surface ~width:5 ~content:"abcdefghij\nxy" ~wrap:`Char () in
  let info = Text_surface.display_info s in
  (* "abcdefghij" wraps into 2 display lines, "xy" is 1 *)
  equal ~msg:"display_line_count" int 3 (Array.length info.lines);
  equal ~msg:"source 0" int 0 info.line_sources.(0);
  equal ~msg:"source 1" int 0 info.line_sources.(1);
  equal ~msg:"source 2" int 1 info.line_sources.(2)

let wrap_char_wrap_indices () =
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Char () in
  let info = Text_surface.display_info s in
  equal ~msg:"wrap_index 0" int 0 info.line_wrap_indices.(0);
  equal ~msg:"wrap_index 1" int 1 info.line_wrap_indices.(1)

(* ── Wrapping — Word ── *)

let wrap_word_at_boundary () =
  let s = make_surface ~width:10 ~content:"hello world foo" ~wrap:`Word () in
  let info = Text_surface.display_info s in
  (* "hello " fits in 10, "world foo" doesn't all fit, should wrap *)
  is_true ~msg:"wraps" (Array.length info.lines >= 2)

let wrap_word_preserves_sources () =
  let s =
    make_surface ~width:10 ~content:"hello world\nfoo bar baz" ~wrap:`Word ()
  in
  let info = Text_surface.display_info s in
  (* First logical line wraps, second might wrap too *)
  let last_source = info.line_sources.(Array.length info.line_sources - 1) in
  equal ~msg:"last source is line 1" int 1 last_source

(* ── set_wrap ── *)

let set_wrap_changes_mode () =
  let s = make_surface ~content:"abcdefghij" ~width:5 () in
  Text_surface.set_wrap s `Char;
  equal ~msg:"display_line_count" int 2 (Text_surface.display_line_count s);
  Text_surface.set_wrap s `None;
  equal ~msg:"back to 1" int 1 (Text_surface.display_line_count s)

(* ── wrap_width ── *)

let explicit_wrap_width () =
  let s = make_surface ~width:40 ~content:"abcdefghij" ~wrap:`Char () in
  Text_surface.set_wrap_width s (Some 5);
  equal ~msg:"wraps at 5" int 2 (Text_surface.display_line_count s)

let wrap_width_none_derives_from_node () =
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Char () in
  Text_surface.set_wrap_width s None;
  equal ~msg:"wraps at node width" int 2 (Text_surface.display_line_count s)

(* ── Truncation ── *)

let truncate_long_line () =
  let s = make_surface ~width:5 ~content:"hello world" () in
  Text_surface.set_truncate s true;
  let info = Text_surface.display_info s in
  (* Should still be 1 display line, but truncated *)
  equal ~msg:"1 display line" int 1 (Array.length info.lines);
  (* The text should end with ellipsis (U+2026 = \xe2\x80\xa6) *)
  let line = info.lines.(0) in
  let last_span = List.nth line (List.length line - 1) in
  equal ~msg:"ends with ellipsis" string "\xe2\x80\xa6" (span_text last_span)

let truncate_short_line_unchanged () =
  let s = make_surface ~width:40 ~content:"hi" () in
  Text_surface.set_truncate s true;
  let info = Text_surface.display_info s in
  let line = info.lines.(0) in
  equal ~msg:"no truncation" string "hi" (span_text (List.hd line))

(* ── Display Info ── *)

let display_info_grapheme_offsets () =
  let s = make_surface ~content:"abc\ndef" () in
  let info = Text_surface.display_info s in
  equal ~msg:"offset line 0" int 0 info.line_grapheme_offsets.(0);
  (* "abc" = 3 graphemes + 1 for newline = offset 4 for "def" *)
  equal ~msg:"offset line 1" int 4 info.line_grapheme_offsets.(1)

let display_info_max_line_width () =
  let s = make_surface ~content:"hi\nhello world\nbye" () in
  let info = Text_surface.display_info s in
  equal ~msg:"max_line_width" int 11 info.max_line_width

(* ── Viewport / Scroll ── *)

let scroll_x_clamps () =
  let s = make_surface ~width:5 ~content:"hello" () in
  Text_surface.set_scroll_x s (-10);
  equal ~msg:"clamped to 0" int 0 (Text_surface.scroll_x s);
  (* Content is 5 wide, viewport is 5 wide, max scroll_x = 0 *)
  Text_surface.set_scroll_x s 100;
  equal ~msg:"clamped to max" int 0 (Text_surface.scroll_x s)

let scroll_y_clamps () =
  let s = make_surface ~height:3 ~content:"a\nb\nc" () in
  Text_surface.set_scroll_y s (-5);
  equal ~msg:"clamped to 0" int 0 (Text_surface.scroll_y s);
  Text_surface.set_scroll_y s 100;
  equal ~msg:"clamped to max" int 0 (Text_surface.scroll_y s)

let scroll_height_equals_display_lines () =
  let s = make_surface ~content:"a\nb\nc\nd" () in
  equal ~msg:"scroll_height" int 4 (Text_surface.scroll_height s)

let scroll_width_equals_max_width () =
  let s = make_surface ~content:"hi\nhello world" () in
  equal ~msg:"scroll_width" int 11 (Text_surface.scroll_width s)

(* ── Selection — set_selection ── *)

let selection_set_and_get () =
  let s = make_surface ~content:"hello world" () in
  let changed = Text_surface.set_selection s ~start:2 ~end_:7 in
  is_true ~msg:"changed" changed;
  is_true ~msg:"has_selection" (Text_surface.has_selection s);
  some ~msg:"selection" (pair int int) (2, 7) (Text_surface.selection s)

let selection_clamps_to_bounds () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.set_selection s ~start:(-5) ~end_:100 in
  some ~msg:"clamped" (pair int int) (0, 5) (Text_surface.selection s)

let selection_normalized () =
  let s = make_surface ~content:"hello world" () in
  (* Reversed: end < start *)
  let _ = Text_surface.set_selection s ~start:7 ~end_:2 in
  some ~msg:"normalized" (pair int int) (2, 7) (Text_surface.selection s)

let selection_returns_true_when_changed () =
  let s = make_surface ~content:"hello" () in
  let c1 = Text_surface.set_selection s ~start:0 ~end_:3 in
  is_true ~msg:"first set" c1;
  let c2 = Text_surface.set_selection s ~start:0 ~end_:3 in
  is_false ~msg:"same again" c2

let selected_text_basic () =
  let s = make_surface ~content:"hello world" () in
  let _ = Text_surface.set_selection s ~start:6 ~end_:11 in
  equal ~msg:"text" string "world" (Text_surface.selected_text s)

let selected_text_empty_when_no_selection () =
  let s = make_surface ~content:"hello" () in
  equal ~msg:"empty" string "" (Text_surface.selected_text s)

(* ── Selection — reset ── *)

let reset_selection_clears () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.set_selection s ~start:0 ~end_:3 in
  Text_surface.reset_selection s;
  is_false ~msg:"not active" (Text_surface.has_selection s);
  equal ~msg:"empty text" string "" (Text_surface.selected_text s)

(* ── Selection — local ── *)

let set_local_selection_basic () =
  let s = make_surface ~width:40 ~content:"hello world" () in
  let changed =
    Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:5
      ~focus_y:0
  in
  is_true ~msg:"changed" changed;
  is_true ~msg:"has_selection" (Text_surface.has_selection s);
  equal ~msg:"selected text" string "hello" (Text_surface.selected_text s)

let update_local_selection_extends () =
  let s = make_surface ~width:40 ~content:"hello world" () in
  let _ =
    Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:5
      ~focus_y:0
  in
  let changed =
    Text_surface.update_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:11
      ~focus_y:0
  in
  is_true ~msg:"changed" changed;
  equal ~msg:"full text" string "hello world" (Text_surface.selected_text s)

(* ── Cache Invalidation ── *)

let invalidate_clears_cache () =
  let s = make_surface ~content:"hello" () in
  (* Access display info to populate cache *)
  let _ = Text_surface.display_info s in
  (* Change buffer content directly *)
  Text_buffer.set_text (Text_surface.buffer s) "hello\nworld";
  Text_surface.invalidate s;
  equal ~msg:"updated" int 2 (Text_surface.display_line_count s)

let buffer_version_triggers_recompute () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.display_info s in
  Text_buffer.set_text (Text_surface.buffer s) "ab\ncd\nef";
  let info = Text_surface.display_info s in
  equal ~msg:"3 lines" int 3 (Array.length info.lines)

let buffer_tab_width_triggers_recompute () =
  let s = make_surface ~content:"a\tb" () in
  let info = Text_surface.display_info s in
  equal ~msg:"width with default tab" int 4 info.max_line_width;
  Text_buffer.set_tab_width (Text_surface.buffer s) 8;
  let info = Text_surface.display_info s in
  equal ~msg:"width with updated tab" int 10 info.max_line_width

(* ── Measurement ── *)

let call_measure surface ~available_width ~available_height =
  let node = Text_surface.node surface in
  match Renderable.Private.measure node with
  | None -> failwith "no measure function registered"
  | Some m ->
      m
        ~known_dimensions:{ Toffee.Geometry.Size.width = None; height = None }
        ~available_space:
          {
            Toffee.Geometry.Size.width = available_width;
            height = available_height;
          }
        ~style:Toffee.Style.default

let call_measure_with_known surface ~known_w ~known_h ~available_width
    ~available_height =
  let node = Text_surface.node surface in
  match Renderable.Private.measure node with
  | None -> failwith "no measure function registered"
  | Some m ->
      m
        ~known_dimensions:
          { Toffee.Geometry.Size.width = known_w; height = known_h }
        ~available_space:
          {
            Toffee.Geometry.Size.width = available_width;
            height = available_height;
          }
        ~style:Toffee.Style.default

let measure_max_content_returns_natural_width () =
  let s = make_surface ~content:"hello world" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* "hello world" = 11 columns *)
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_max_content_multiline () =
  let s = make_surface ~content:"hi\nhello world\nbye" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 3 (int_of_float result.height)

let measure_min_content_char_wrap () =
  let s = make_surface ~content:"hello world" ~wrap:`Char () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Min_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Min_content with char wrap: wrap_width=1, each grapheme gets its own
     line *)
  equal ~msg:"width" int 1 (int_of_float result.width)

let measure_min_content_word_wrap () =
  let s = make_surface ~content:"hi there" ~wrap:`Word () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Min_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Min_content with word wrap: wraps at width=1, words break to char level *)
  is_true ~msg:"width >= 1" (result.width >= 1.)

let measure_definite_caps_with_wrap () =
  let s = make_surface ~content:"hello world" ~wrap:`Word () in
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 8.)
      ~available_height:Toffee.Available_space.Max_content
  in
  (* With wrapping: width should not exceed available width of 8 *)
  is_true ~msg:"width <= 8" (result.width <= 8.);
  (* Content wraps: "hello" + "world" -> at least 2 lines *)
  is_true ~msg:"height >= 2" (result.height >= 2.)

let measure_definite_no_wrap_uncapped () =
  let s = make_surface ~content:"hello world" () in
  (* wrap = `None by default for surface *)
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 5.)
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Without wrapping: width should be natural width, not capped *)
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_known_width_overrides () =
  let s = make_surface ~content:"hello world" () in
  let result =
    call_measure_with_known s ~known_w:(Some 42.) ~known_h:None
      ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 42 (int_of_float result.width)

let measure_known_height_overrides () =
  let s = make_surface ~content:"a\nb\nc" () in
  let result =
    call_measure_with_known s ~known_w:None ~known_h:(Some 10.)
      ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"height" int 10 (int_of_float result.height)

let measure_empty_buffer () =
  let s = make_surface ~content:"" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Empty buffer should return minimum 1x1 *)
  is_true ~msg:"width >= 1" (result.width >= 1.);
  is_true ~msg:"height >= 1" (result.height >= 1.)

let measure_height_grows_for_wrapped () =
  (* 20 chars in 10-wide viewport should produce 2 lines *)
  let s = make_surface ~content:"abcdefghijklmnopqrst" ~wrap:`Char () in
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 10.)
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"height" int 2 (int_of_float result.height)

(* ── Convenience Getters ── *)

let max_scroll_x_wider_content () =
  (* Content "hello world" = 11 cols, viewport width = 5 *)
  let s = make_surface ~width:5 ~content:"hello world" () in
  equal ~msg:"max_scroll_x" int 6 (Text_surface.max_scroll_x s)

let max_scroll_x_fits () =
  let s = make_surface ~width:40 ~content:"hi" () in
  equal ~msg:"max_scroll_x" int 0 (Text_surface.max_scroll_x s)

let max_scroll_y_taller_content () =
  let s = make_surface ~height:2 ~content:"a\nb\nc\nd" () in
  equal ~msg:"max_scroll_y" int 2 (Text_surface.max_scroll_y s)

let max_scroll_y_fits () =
  let s = make_surface ~height:10 ~content:"a\nb" () in
  equal ~msg:"max_scroll_y" int 0 (Text_surface.max_scroll_y s)

(* ── Selection — multi-line ── *)

let selection_multiline_text () =
  let s = make_surface ~content:"hello\nworld" () in
  let _ = Text_surface.set_selection s ~start:3 ~end_:9 in
  equal ~msg:"selected" string "lo\nwor" (Text_surface.selected_text s)

let selection_full_content () =
  let s = make_surface ~content:"abc" () in
  let _ = Text_surface.set_selection s ~start:0 ~end_:3 in
  equal ~msg:"full" string "abc" (Text_surface.selected_text s)

let selection_with_styled_text () =
  let s = make_surface () in
  let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default in
  Text_buffer.set_styled_text (Text_surface.buffer s)
    [
      { Text_buffer.text = "hello "; style = red };
      { Text_buffer.text = "world"; style = Ansi.Style.default };
    ];
  Text_surface.invalidate s;
  let _ = Text_surface.set_selection s ~start:4 ~end_:8 in
  equal ~msg:"across spans" string "o wo" (Text_surface.selected_text s)

(* ── Wrapping — edge cases ── *)

let wrap_word_long_word_fallback () =
  (* Word exceeding wrap width should fall back to char break *)
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Word () in
  let info = Text_surface.display_info s in
  (* "abcdefghij" (10 chars) can't break at word boundaries, must char-break *)
  is_true ~msg:"wraps" (Array.length info.lines >= 2)

let wrap_word_empty_lines_preserved () =
  let s = make_surface ~width:40 ~content:"a\n\nb" ~wrap:`Word () in
  equal ~msg:"3 lines" int 3 (Text_surface.display_line_count s)

let wrap_word_single_char_words () =
  let s = make_surface ~width:5 ~content:"a b c d e f" ~wrap:`Word () in
  let info = Text_surface.display_info s in
  is_true ~msg:"wraps" (Array.length info.lines >= 2)

(* ── Property Tests ── *)

(* Small printable strings *)
let small_string =
  Testable.make
    ~pp:(fun fmt s -> Format.fprintf fmt "%S" s)
    ~equal:String.equal
    ~gen:(Windtrap_prop.Gen.string_of (Windtrap_prop.Gen.char_range 'a' 'z'))
    ()

(* ── Runner ── *)

let () =
  run "mosaic.text_surface"
    [
      group "Construction" [ test "defaults" defaults ];
      group "Wrapping — None"
        [
          test "single line" wrap_none_single_line;
          test "multiline" wrap_none_multiline;
        ];
      group "Wrapping — Char"
        [
          test "splits at width" wrap_char_splits;
          test "short line no wrap" wrap_char_short_line_no_wrap;
          test "line_sources map" wrap_char_line_sources;
          test "wrap_indices" wrap_char_wrap_indices;
        ];
      group "Wrapping — Word"
        [
          test "breaks at word boundary" wrap_word_at_boundary;
          test "preserves line sources" wrap_word_preserves_sources;
        ];
      group "set_wrap"
        [ test "changes mode and recomputes" set_wrap_changes_mode ];
      group "wrap_width"
        [
          test "explicit override" explicit_wrap_width;
          test "None derives from node" wrap_width_none_derives_from_node;
        ];
      group "Truncation"
        [
          test "long line truncated with ellipsis" truncate_long_line;
          test "short line unchanged" truncate_short_line_unchanged;
        ];
      group "Display info"
        [
          test "grapheme offsets" display_info_grapheme_offsets;
          test "max_line_width" display_info_max_line_width;
        ];
      group "Viewport / scroll"
        [
          test "scroll_x clamps" scroll_x_clamps;
          test "scroll_y clamps" scroll_y_clamps;
          test "scroll_height = display lines"
            scroll_height_equals_display_lines;
          test "scroll_width = max width" scroll_width_equals_max_width;
        ];
      group "Selection — set_selection"
        [
          test "set and get" selection_set_and_get;
          test "clamps to bounds" selection_clamps_to_bounds;
          test "normalized" selection_normalized;
          test "returns true when changed" selection_returns_true_when_changed;
          test "selected_text" selected_text_basic;
          test "empty when no selection" selected_text_empty_when_no_selection;
        ];
      group "Selection — reset"
        [ test "clears active state" reset_selection_clears ];
      group "Selection — local"
        [
          test "set_local_selection" set_local_selection_basic;
          test "update_local_selection extends" update_local_selection_extends;
        ];
      group "Cache invalidation"
        [
          test "invalidate clears cache" invalidate_clears_cache;
          test "buffer version triggers recompute"
            buffer_version_triggers_recompute;
          test "buffer tab width triggers recompute"
            buffer_tab_width_triggers_recompute;
        ];
      group "Measurement"
        [
          test "max_content returns natural width"
            measure_max_content_returns_natural_width;
          test "max_content multiline" measure_max_content_multiline;
          test "min_content char wrap" measure_min_content_char_wrap;
          test "min_content word wrap" measure_min_content_word_wrap;
          test "definite caps with wrap" measure_definite_caps_with_wrap;
          test "definite no wrap uncapped" measure_definite_no_wrap_uncapped;
          test "known width overrides" measure_known_width_overrides;
          test "known height overrides" measure_known_height_overrides;
          test "empty buffer" measure_empty_buffer;
          test "height grows for wrapped" measure_height_grows_for_wrapped;
        ];
      group "Convenience getters"
        [
          test "max_scroll_x wider content" max_scroll_x_wider_content;
          test "max_scroll_x fits" max_scroll_x_fits;
          test "max_scroll_y taller content" max_scroll_y_taller_content;
          test "max_scroll_y fits" max_scroll_y_fits;
        ];
      group "Selection — multi-line"
        [
          test "multi-line selection" selection_multiline_text;
          test "full content selection" selection_full_content;
          test "selection with styled text" selection_with_styled_text;
        ];
      group "Wrapping — edge cases"
        [
          test "long word falls back to char" wrap_word_long_word_fallback;
          test "empty lines preserved" wrap_word_empty_lines_preserved;
          test "single char words" wrap_word_single_char_words;
        ];
      group "Properties"
        [
          prop "display_line_count >= line_count" small_string (fun s ->
              let surface = make_surface ~width:5 ~content:s ~wrap:`Char () in
              let buf = Text_surface.buffer surface in
              Text_surface.display_line_count surface
              >= Text_buffer.line_count buf);
          prop "scroll_x always >= 0" small_string (fun s ->
              let surface = make_surface ~width:5 ~content:s () in
              Text_surface.set_scroll_x surface (-100);
              Text_surface.scroll_x surface >= 0);
          prop "scroll_y always >= 0" small_string (fun s ->
              let surface = make_surface ~height:3 ~content:s () in
              Text_surface.set_scroll_y surface (-100);
              Text_surface.scroll_y surface >= 0);
        ];
    ]
