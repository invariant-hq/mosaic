open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let span_text (s : Text_buffer.span) = s.text
let span_style (s : Text_buffer.span) = s.style
let red_style = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let green_style = Ansi.Style.fg Ansi.Color.green Ansi.Style.default

(* Bounded positive int for property tests *)
let small_nat = Gen.nat

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

let create_tab_width_below_1_raises () =
  raises_match (Exn.invalid_arg ~sub:"tab width") (fun () ->
      ignore (Text_buffer.create ~tab_width:0 () : Text_buffer.t))

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
  satisfies ~claim:"version increased past the initial" int
    (fun v -> v > v0)
    (Text_buffer.version buf)

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
  satisfies ~claim:"version increased past the pre-clear value" int
    (fun v -> v > v1)
    (Text_buffer.version buf)

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
  equal ~msg:"line 0" int 2
    (Text_buffer.line_width buf ~width_method:`Unicode 0);
  equal ~msg:"line 1" int 5
    (Text_buffer.line_width buf ~width_method:`Unicode 1)

let line_width_out_of_range () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  equal ~msg:"negative" int 0
    (Text_buffer.line_width buf ~width_method:`Unicode (-1));
  equal ~msg:"too large" int 0
    (Text_buffer.line_width buf ~width_method:`Unicode 99)

let max_line_width_basic () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hi\nhello world\nbye";
  equal ~msg:"max" int 11
    (Text_buffer.max_line_width buf ~width_method:`Unicode)

(* ── Styled text ── *)

let styled_text_preserves_lines_and_styles () =
  let buf = Text_buffer.create () in
  Text_buffer.set_styled_text buf
    [
      { Text_buffer.text = "aa\r\nbb"; style = red_style };
      { Text_buffer.text = "cc"; style = green_style };
    ];
  let spans = Text_buffer.styled_text buf in
  equal (list string) [ "aa\r\nbb"; "cc" ] (List.map span_text spans);
  is_true (Ansi.Style.equal red_style (span_style (List.hd spans)));
  is_true (Ansi.Style.equal green_style (span_style (List.nth spans 1)))

let styled_text_keeps_append_order_and_snapshot () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "one";
  Text_buffer.append buf "two";
  Text_buffer.append_styled buf
    [ { Text_buffer.text = "three"; style = red_style } ];
  let spans = Text_buffer.styled_text buf in
  Text_buffer.append buf "four";
  equal (list string) [ "one"; "two"; "three" ] (List.map span_text spans);
  equal (list string)
    [ "one"; "two"; "three"; "four" ]
    (List.map span_text (Text_buffer.styled_text buf));
  Text_buffer.clear buf;
  equal (list string) [] (List.map span_text (Text_buffer.styled_text buf))

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
  let p1 = Text_buffer.Highlight.priority (List.nth hl 1) in
  satisfies
    ~claim:(Printf.sprintf "low priority first (at most %d)" p1)
    int
    (fun p -> p <= p1)
    (Text_buffer.Highlight.priority (List.hd hl))

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

let set_tab_width_below_1_raises () =
  let buf = Text_buffer.create () in
  raises_match (Exn.invalid_arg ~sub:"tab width") (fun () ->
      Text_buffer.set_tab_width buf (-1));
  equal ~msg:"unchanged" int 2 (Text_buffer.tab_width buf)

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
  satisfies ~claim:"version increased past the prior value" int
    (fun v -> v > v_before)
    (Text_buffer.version buf)

(* ── Width Method ── *)

let max_line_width_follows_method () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "ab\n\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbbx";
  let width width_method = Text_buffer.max_line_width buf ~width_method in
  equal ~msg:"one joined cluster" int 3 (width `Unicode);
  equal ~msg:"each scalar's width" int 5 (width `Wcwidth);
  equal ~msg:"not the cached width of another method" int 3 (width `Unicode)

(* ── Versioning ── *)

let version_increments_on_mutations () =
  let buf = Text_buffer.create () in
  let v0 = Text_buffer.version buf in
  Text_buffer.set_text buf "a";
  let v1 = Text_buffer.version buf in
  satisfies
    ~claim:(Printf.sprintf "set_text bumped the version above %d" v0)
    int
    (fun v -> v > v0)
    v1;
  Text_buffer.append buf "b";
  let v2 = Text_buffer.version buf in
  satisfies
    ~claim:(Printf.sprintf "append bumped the version above %d" v1)
    int
    (fun v -> v > v1)
    v2;
  Text_buffer.clear buf;
  let v3 = Text_buffer.version buf in
  satisfies
    ~claim:(Printf.sprintf "clear bumped the version above %d" v2)
    int
    (fun v -> v > v2)
    v3

(* ── Default Style ── *)

let default_style_applied_to_set_text () =
  let buf = Text_buffer.create ~default_style:red_style () in
  Text_buffer.set_text buf "hello";
  let spans = Text_buffer.styled_text buf in
  is_true ~msg:"uses default style"
    (Ansi.Style.equal red_style (span_style (List.hd spans)))

let set_default_style_does_not_restyle () =
  let buf = Text_buffer.create () in
  Text_buffer.set_text buf "hello";
  Text_buffer.set_default_style buf red_style;
  let spans = Text_buffer.styled_text buf in
  (* Existing content should still have the old default style *)
  is_false ~msg:"not restyled"
    (Ansi.Style.equal red_style (span_style (List.hd spans)))

let default_style_preserves_prepared_paint () =
  List.iter
    (fun texts ->
      let buf = Text_buffer.create () in
      Text_buffer.set_styled_text buf
        (List.map (fun text -> Text_buffer.{ text; style = green_style }) texts);
      Text_buffer.add_highlight buf
        (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:1
           ~style:(Ansi.Style.make ~bold:true ())
           ~ref_id:0 ());
      let before = fst (Text_buffer.style_at buf 0) in
      Text_buffer.set_default_style buf red_style;
      is_true ~msg:"default changes preserve prepared explicit paint"
        (before == fst (Text_buffer.style_at buf 0)))
    [ [ "hello" ]; [ "é" ]; [ ""; "\195"; "\169"; "" ] ];
  let empty = Text_buffer.create () in
  ignore (Text_buffer.style_at empty 0);
  Text_buffer.set_default_style empty red_style;
  is_true ~msg:"empty buffer uses current default"
    (Ansi.Style.equal red_style (fst (Text_buffer.style_at empty 0)))

(* ── Property Tests ── *)

(* Printable ASCII string for property tests *)
let printable_string = Gen.string_of (Gen.char_range 'a' 'z')

(* ── Runner ── *)

let () =
  exit
    (run "mosaic.text_buffer"
       [
         group "Construction"
           [
             test "defaults" create_defaults;
             test "custom tab_width" create_custom_tab_width;
             test "a tab width below 1 raises" create_tab_width_below_1_raises;
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
         group "Styled text"
           [
             test "preserves lines and styles"
               styled_text_preserves_lines_and_styles;
             test "append order and retained snapshot"
               styled_text_keeps_append_order_and_snapshot;
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
             test "a tab width below 1 raises" set_tab_width_below_1_raises;
             test "no-op on same value" set_tab_width_noop_same;
             test "increments version on change"
               set_tab_width_increments_version;
           ];
         group "Width method"
           [
             test "max_line_width follows the method"
               max_line_width_follows_method;
           ];
         group "Versioning"
           [ test "increments on mutations" version_increments_on_mutations ];
         group "Default style"
           [
             test "applied to set_text" default_style_applied_to_set_text;
             test "set_default_style does not restyle"
               set_default_style_does_not_restyle;
             test "default style preserves prepared paint"
               default_style_preserves_prepared_paint;
           ];
         group "Properties"
           [
             prop "line_count >= 1" printable_string (fun s ->
                 let buf = Text_buffer.create () in
                 Text_buffer.set_text buf s;
                 satisfies ~claim:"at least 1 line" int
                   (fun n -> n >= 1)
                   (Text_buffer.line_count buf));
             prop "grapheme_count >= 0" printable_string (fun s ->
                 let buf = Text_buffer.create () in
                 Text_buffer.set_text buf s;
                 satisfies ~claim:"non-negative" int
                   (fun n -> n >= 0)
                   (Text_buffer.grapheme_count buf));
             prop "plain_text round-trip" printable_string (fun s ->
                 let buf = Text_buffer.create () in
                 Text_buffer.set_text buf s;
                 equal text s (Text_buffer.plain_text buf));
             prop "version changes with text"
               (Gen.pair printable_string printable_string) (fun (s1, s2) ->
                 let buf = Text_buffer.create () in
                 Text_buffer.set_text buf s1;
                 let v1 = Text_buffer.version buf in
                 Text_buffer.set_text buf s2;
                 let v2 = Text_buffer.version buf in
                 satisfies
                   ~claim:
                     "unchanged for equal text, increasing for different text"
                   int
                   (fun v -> if String.equal s1 s2 then v = v1 else v > v1)
                   v2);
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
                 satisfies ~claim:"at most 1 highlight" int
                   (fun n -> n <= 1)
                   (List.length hl));
           ];
       ])
