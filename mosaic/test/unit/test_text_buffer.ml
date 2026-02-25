open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let span_text (s : Text_buffer.span) = s.text
let span_style (s : Text_buffer.span) = s.style
let red_style = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let green_style = Ansi.Style.fg Ansi.Color.green Ansi.Style.default

(* Bounded positive int for property tests *)
let small_nat =
  Testable.make
    ~pp:(fun fmt i -> Format.fprintf fmt "%d" i)
    ~equal:Int.equal ~gen:Windtrap_prop.Gen.nat ()

(* ── Construction ── *)

let create_defaults () =
  let buf = Text_buffer.create () in
  equal ~msg:"plain_text" string "" (Text_buffer.plain_text buf);
  equal ~msg:"version" int 0 (Text_buffer.version buf);
  equal ~msg:"line_count" int 1 (Text_buffer.line_count buf);
  equal ~msg:"grapheme_count" int 0 (Text_buffer.grapheme_count buf);
  equal ~msg:"tab_width" int 2 (Text_buffer.tab_width buf)

let create_custom_tab_width () =
  let buf = Text_buffer.create ~tab_width:4 () in
  equal ~msg:"tab_width" int 4 (Text_buffer.tab_width buf)

let create_tab_width_clamps () =
  let buf = Text_buffer.create ~tab_width:0 () in
  equal ~msg:"clamped to 1" int 1 (Text_buffer.tab_width buf)

(* ── Content — set_text ── *)

let set_text_single_line () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"plain_text" string "hello" (Text_buffer.plain_text buf)

let set_text_multiline () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "line1\nline2\nline3";
  equal ~msg:"plain_text" string "line1\nline2\nline3"
    (Text_buffer.plain_text buf);
  equal ~msg:"line_count" int 3 (Text_buffer.line_count buf)

let set_text_empty () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.set_text buf "";
  equal ~msg:"plain_text" string "" (Text_buffer.plain_text buf);
  equal ~msg:"line_count" int 1 (Text_buffer.line_count buf)

let set_text_replaces_previous () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "old";
  Text_buffer.set_text buf "new";
  equal ~msg:"plain_text" string "new" (Text_buffer.plain_text buf)

let set_text_increments_version () =
  let buf = Text_buffer.create () in
  let v0 = Text_buffer.version buf in
  Text_buffer.set_text buf "hello";
  is_true ~msg:"version increased" (Text_buffer.version buf > v0)

(* ── Content — set_styled_text ── *)

let set_styled_text_single_span () =
  let buf = Text_buffer.create () in
  Text_buffer.set_styled_text buf
    [ { Text_buffer.text = "red"; style = red_style } ];
  equal ~msg:"plain_text" string "red" (Text_buffer.plain_text buf)

let set_styled_text_multiple_spans () =
  let buf = Text_buffer.create () in
  Text_buffer.set_styled_text buf
    [
      { Text_buffer.text = "hello "; style = red_style };
      { Text_buffer.text = "world"; style = green_style };
    ];
  equal ~msg:"plain_text" string "hello world" (Text_buffer.plain_text buf)

(* ── Content — append / append_styled ── *)

let append_to_existing () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.append buf " world";
  equal ~msg:"plain_text" string "hello world" (Text_buffer.plain_text buf)

let append_multiple () =
  let buf = Text_buffer.create () in
  Text_buffer.append buf "a";
  Text_buffer.append buf "b";
  Text_buffer.append buf "c";
  equal ~msg:"plain_text" string "abc" (Text_buffer.plain_text buf)

let append_after_read () =
  let buf = Text_buffer.create () in
  Text_buffer.append buf "a";
  (* Force ensure_span_order via plain_text *)
  equal ~msg:"after first" string "a" (Text_buffer.plain_text buf);
  Text_buffer.append buf "b";
  equal ~msg:"after second" string "ab" (Text_buffer.plain_text buf);
  Text_buffer.append buf "c";
  equal ~msg:"after third" string "abc" (Text_buffer.plain_text buf)

let append_styled_after_read () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "start";
  (* Read to force forward order *)
  ignore (Text_buffer.plain_text buf : string);
  Text_buffer.append_styled buf
    [ { Text_buffer.text = " mid"; style = red_style } ];
  (* Read again to force forward order *)
  equal ~msg:"after first append" string "start mid"
    (Text_buffer.plain_text buf);
  Text_buffer.append_styled buf
    [ { Text_buffer.text = " end"; style = green_style } ];
  equal ~msg:"after second append" string "start mid end"
    (Text_buffer.plain_text buf)

let append_styled_to_existing () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "start";
  Text_buffer.append_styled buf
    [ { Text_buffer.text = " end"; style = red_style } ];
  equal ~msg:"plain_text" string "start end" (Text_buffer.plain_text buf)

(* ── Content — clear ── *)

let clear_resets_content () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.clear buf;
  equal ~msg:"plain_text" string "" (Text_buffer.plain_text buf);
  equal ~msg:"line_count" int 1 (Text_buffer.line_count buf);
  equal ~msg:"grapheme_count" int 0 (Text_buffer.grapheme_count buf)

let clear_removes_highlights () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.add_highlight buf
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:3 ~style:red_style
       ~ref_id:1 ());
  Text_buffer.clear buf;
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:10 in
  equal ~msg:"no highlights" int 0 (List.length hl)

let clear_increments_version () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let v1 = Text_buffer.version buf in
  Text_buffer.clear buf;
  is_true ~msg:"version increased" (Text_buffer.version buf > v1)

(* ── Grapheme Count ── *)

let grapheme_count_ascii () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"count" int 5 (Text_buffer.grapheme_count buf)

let grapheme_count_empty () =
  let buf = Text_buffer.create () in
  equal ~msg:"count" int 0 (Text_buffer.grapheme_count buf)

let grapheme_count_multibyte () =
  let buf = Text_buffer.create () in
  (* "café" has 4 grapheme clusters *)
  Text_buffer.set_text buf "caf\xc3\xa9";
  equal ~msg:"count" int 4 (Text_buffer.grapheme_count buf)

(* ── Line Information ── *)

let line_count_single () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"line_count" int 1 (Text_buffer.line_count buf)

let line_count_multi () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "a\nb\nc";
  equal ~msg:"line_count" int 3 (Text_buffer.line_count buf)

let line_count_trailing_newline () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "a\nb\n";
  equal ~msg:"line_count" int 3 (Text_buffer.line_count buf)

let line_width_per_line () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hi\nhello";
  equal ~msg:"line 0" int 2 (Text_buffer.line_width buf 0);
  equal ~msg:"line 1" int 5 (Text_buffer.line_width buf 1)

let line_width_out_of_range () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"negative" int 0 (Text_buffer.line_width buf (-1));
  equal ~msg:"too large" int 0 (Text_buffer.line_width buf 99)

let max_line_width_basic () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hi\nhello world\nbye";
  equal ~msg:"max" int 11 (Text_buffer.max_line_width buf)

(* ── Line Spans ── *)

let line_spans_single_line () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let spans = Text_buffer.line_spans buf 0 in
  equal ~msg:"count" int 1 (List.length spans);
  equal ~msg:"text" string "hello" (span_text (List.hd spans))

let line_spans_multiline () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "aaa\nbbb";
  let s0 = Text_buffer.line_spans buf 0 in
  let s1 = Text_buffer.line_spans buf 1 in
  equal ~msg:"line 0" string "aaa" (span_text (List.hd s0));
  equal ~msg:"line 1" string "bbb" (span_text (List.hd s1))

let line_spans_out_of_range () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let spans = Text_buffer.line_spans buf 5 in
  equal ~msg:"empty" int 0 (List.length spans)

let line_spans_styled_across_lines () =
  let buf = Text_buffer.create () in
  Text_buffer.set_styled_text buf
    [
      { Text_buffer.text = "aa\nbb"; style = red_style };
      { Text_buffer.text = "cc"; style = green_style };
    ];
  let s0 = Text_buffer.line_spans buf 0 in
  let s1 = Text_buffer.line_spans buf 1 in
  equal ~msg:"line 0 text" string "aa" (span_text (List.hd s0));
  (* line 1 should have "bb" with red_style and "cc" with green_style *)
  equal ~msg:"line 1 count" int 2 (List.length s1);
  equal ~msg:"line 1 first" string "bb" (span_text (List.hd s1));
  equal ~msg:"line 1 second" string "cc" (span_text (List.nth s1 1))

(* ── text_in_range ── *)

let text_in_range_middle () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello world";
  equal ~msg:"range" string "llo w"
    (Text_buffer.text_in_range buf ~start:2 ~len:5)

let text_in_range_from_start () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"range" string "hel"
    (Text_buffer.text_in_range buf ~start:0 ~len:3)

let text_in_range_clamped_to_end () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"range" string "llo"
    (Text_buffer.text_in_range buf ~start:2 ~len:100)

let text_in_range_empty () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"zero len" string ""
    (Text_buffer.text_in_range buf ~start:0 ~len:0);
  equal ~msg:"negative len" string ""
    (Text_buffer.text_in_range buf ~start:0 ~len:(-1))

let text_in_range_out_of_bounds () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"out of bounds" string ""
    (Text_buffer.text_in_range buf ~start:99 ~len:3)

(* ── Highlights ── *)

let highlight_add_and_query () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello world";
  let h =
    Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:red_style
      ~ref_id:1 ()
  in
  Text_buffer.add_highlight buf h;
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:5 in
  equal ~msg:"count" int 1 (List.length hl)

let highlight_range_filters () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello world";
  let h1 =
    Text_buffer.Highlight.make ~start_offset:0 ~end_offset:3 ~style:red_style
      ~ref_id:1 ()
  in
  let h2 =
    Text_buffer.Highlight.make ~start_offset:6 ~end_offset:11 ~style:green_style
      ~ref_id:2 ()
  in
  Text_buffer.add_highlight buf h1;
  Text_buffer.add_highlight buf h2;
  (* Query only the first 3 graphemes — should find only h1 *)
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:3 in
  equal ~msg:"only first" int 1 (List.length hl);
  equal ~msg:"ref_id" int 1 (Text_buffer.Highlight.ref_id (List.hd hl))

let highlight_sorts_by_priority () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let h_low =
    Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:red_style
      ~priority:1 ~ref_id:1 ()
  in
  let h_high =
    Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:green_style
      ~priority:10 ~ref_id:2 ()
  in
  Text_buffer.add_highlight buf h_high;
  Text_buffer.add_highlight buf h_low;
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:5 in
  equal ~msg:"count" int 2 (List.length hl);
  (* Sorted ascending by priority *)
  is_true ~msg:"low first"
    (Text_buffer.Highlight.priority (List.hd hl)
    <= Text_buffer.Highlight.priority (List.nth hl 1))

let highlight_remove_by_ref () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.add_highlight buf
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:red_style
       ~ref_id:42 ());
  Text_buffer.add_highlight buf
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:green_style
       ~ref_id:99 ());
  Text_buffer.remove_highlights_by_ref buf 42;
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:5 in
  equal ~msg:"one left" int 1 (List.length hl);
  equal ~msg:"remaining ref_id" int 99
    (Text_buffer.Highlight.ref_id (List.hd hl))

let highlight_clear_all () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.add_highlight buf
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:red_style
       ~ref_id:1 ());
  Text_buffer.add_highlight buf
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:green_style
       ~ref_id:2 ());
  Text_buffer.clear_highlights buf;
  let hl = Text_buffer.highlights_in_range buf ~start:0 ~len:5 in
  equal ~msg:"empty" int 0 (List.length hl)

(* ── Tab Width ── *)

let tab_width_default () =
  let buf = Text_buffer.create () in
  equal ~msg:"default" int 2 (Text_buffer.tab_width buf)

let set_tab_width_clamps () =
  let buf = Text_buffer.create () in
  Text_buffer.set_tab_width buf 0;
  equal ~msg:"clamped" int 1 (Text_buffer.tab_width buf)

let set_tab_width_noop_same () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let v_before = Text_buffer.version buf in
  Text_buffer.set_tab_width buf 2;
  equal ~msg:"version unchanged" int v_before (Text_buffer.version buf)

let set_tab_width_increments_version () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let v_before = Text_buffer.version buf in
  Text_buffer.set_tab_width buf 8;
  is_true ~msg:"version increased" (Text_buffer.version buf > v_before)

(* ── Width Method ── *)

let set_width_method_noop_same () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  let v_before = Text_buffer.version buf in
  Text_buffer.set_width_method buf `Unicode;
  equal ~msg:"version unchanged" int v_before (Text_buffer.version buf)

(* ── Versioning ── *)

let version_increments_on_mutations () =
  let buf = Text_buffer.create () in
  let v0 = Text_buffer.version buf in
  Text_buffer.set_text buf "a";
  let v1 = Text_buffer.version buf in
  is_true ~msg:"set_text" (v1 > v0);
  Text_buffer.append buf "b";
  let v2 = Text_buffer.version buf in
  is_true ~msg:"append" (v2 > v1);
  Text_buffer.clear buf;
  let v3 = Text_buffer.version buf in
  is_true ~msg:"clear" (v3 > v2)

(* ── Default Style ── *)

let default_style_applied_to_set_text () =
  let buf = Text_buffer.create ~default_style:red_style () in
  Text_buffer.set_text buf "hello";
  let spans = Text_buffer.line_spans buf 0 in
  is_true ~msg:"uses default style"
    (Ansi.Style.equal red_style (span_style (List.hd spans)))

let set_default_style_does_not_restyle () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.set_default_style buf red_style;
  let spans = Text_buffer.line_spans buf 0 in
  (* Existing content should still have the old default style *)
  is_false ~msg:"not restyled"
    (Ansi.Style.equal red_style (span_style (List.hd spans)))

(* ── Property Tests ── *)

(* Printable ASCII string for property tests *)
let printable_string =
  Testable.make
    ~pp:(fun fmt s -> Format.fprintf fmt "%S" s)
    ~equal:String.equal
    ~gen:(Windtrap_prop.Gen.string_of (Windtrap_prop.Gen.char_range 'a' 'z'))
    ()

(* ── Runner ── *)

let () =
  run "mosaic.text_buffer"
    [
      group "Construction"
        [
          test "defaults" create_defaults;
          test "custom tab_width" create_custom_tab_width;
          test "tab_width clamps to >= 1" create_tab_width_clamps;
        ];
      group "Content — set_text"
        [
          test "single line" set_text_single_line;
          test "multiline" set_text_multiline;
          test "empty string" set_text_empty;
          test "replaces previous" set_text_replaces_previous;
          test "increments version" set_text_increments_version;
        ];
      group "Content — set_styled_text"
        [
          test "single span" set_styled_text_single_span;
          test "multiple spans" set_styled_text_multiple_spans;
        ];
      group "Content — append"
        [
          test "append to existing" append_to_existing;
          test "multiple appends" append_multiple;
          test "append after read" append_after_read;
          test "append_styled" append_styled_to_existing;
          test "append_styled after read" append_styled_after_read;
        ];
      group "Content — clear"
        [
          test "resets content" clear_resets_content;
          test "removes highlights" clear_removes_highlights;
          test "increments version" clear_increments_version;
        ];
      group "Grapheme count"
        [
          test "ASCII" grapheme_count_ascii;
          test "empty" grapheme_count_empty;
          test "multibyte UTF-8" grapheme_count_multibyte;
        ];
      group "Line information"
        [
          test "single line" line_count_single;
          test "multiple lines" line_count_multi;
          test "trailing newline" line_count_trailing_newline;
          test "line_width per line" line_width_per_line;
          test "line_width out of range" line_width_out_of_range;
          test "max_line_width" max_line_width_basic;
        ];
      group "Line spans"
        [
          test "single line" line_spans_single_line;
          test "multiline" line_spans_multiline;
          test "out of range" line_spans_out_of_range;
          test "styled across line boundaries" line_spans_styled_across_lines;
        ];
      group "text_in_range"
        [
          test "middle of content" text_in_range_middle;
          test "from start" text_in_range_from_start;
          test "clamped to end" text_in_range_clamped_to_end;
          test "empty range" text_in_range_empty;
          test "out of bounds" text_in_range_out_of_bounds;
        ];
      group "Highlights"
        [
          test "add and query" highlight_add_and_query;
          test "range filters" highlight_range_filters;
          test "sorts by priority" highlight_sorts_by_priority;
          test "remove by ref_id" highlight_remove_by_ref;
          test "clear all" highlight_clear_all;
        ];
      group "Tab width"
        [
          test "default is 2" tab_width_default;
          test "clamps to >= 1" set_tab_width_clamps;
          test "no-op on same value" set_tab_width_noop_same;
          test "increments version on change" set_tab_width_increments_version;
        ];
      group "Width method"
        [ test "no-op on same value" set_width_method_noop_same ];
      group "Versioning"
        [ test "increments on mutations" version_increments_on_mutations ];
      group "Default style"
        [
          test "applied to set_text" default_style_applied_to_set_text;
          test "set_default_style does not restyle"
            set_default_style_does_not_restyle;
        ];
      group "Properties"
        [
          prop "line_count >= 1" printable_string (fun s ->
              let buf = Text_buffer.create () in
              Text_buffer.set_text buf s;
              Text_buffer.line_count buf >= 1);
          prop "grapheme_count >= 0" printable_string (fun s ->
              let buf = Text_buffer.create () in
              Text_buffer.set_text buf s;
              Text_buffer.grapheme_count buf >= 0);
          prop "plain_text round-trip" printable_string (fun s ->
              let buf = Text_buffer.create () in
              Text_buffer.set_text buf s;
              String.equal s (Text_buffer.plain_text buf));
          prop2 "version monotonically increases" printable_string
            printable_string (fun s1 s2 ->
              let buf = Text_buffer.create () in
              Text_buffer.set_text buf s1;
              let v1 = Text_buffer.version buf in
              Text_buffer.set_text buf s2;
              let v2 = Text_buffer.version buf in
              v2 > v1);
          prop "highlights_in_range subset" small_nat (fun n ->
              let buf = Text_buffer.create () in
              Text_buffer.set_text buf "hello world test";
              let end_offset = min (n + 1) 16 in
              let start_offset = min n end_offset in
              let h =
                Text_buffer.Highlight.make ~start_offset ~end_offset
                  ~style:red_style ~ref_id:1 ()
              in
              Text_buffer.add_highlight buf h;
              let hl =
                Text_buffer.highlights_in_range buf ~start:start_offset
                  ~len:(end_offset - start_offset)
              in
              List.length hl <= 1);
        ];
    ]
