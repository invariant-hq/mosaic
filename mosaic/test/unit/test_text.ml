open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let red_style = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let green_style = Ansi.Style.fg Ansi.Color.green Ansi.Style.default

let make_text ?content ?text_style ?wrap ?selectable ?selection_bg ?selection_fg
    ?tab_width () =
  let t = make_ctx () in
  let root = make_root t in
  let txt =
    Text.create ~parent:root ?content ?text_style ?wrap ?selectable
      ?selection_bg ?selection_fg ?tab_width ()
  in
  (t, txt)

(* ── Props ── *)

let props_default_equals_make () =
  is_true ~msg:"equal"
    (Text.Props.equal Text.Props.default (Text.Props.make ()))

let props_detects_content_diff () =
  let a = Text.Props.make ~content:"hello" () in
  let b = Text.Props.make ~content:"world" () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_text_style_diff () =
  let a = Text.Props.make ~text_style:red_style () in
  let b = Text.Props.make () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_wrap_diff () =
  let a = Text.Props.make ~wrap:`Char () in
  let b = Text.Props.make ~wrap:`Word () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_selectable_diff () =
  let a = Text.Props.make ~selectable:true () in
  let b = Text.Props.make ~selectable:false () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_selection_bg_diff () =
  let a = Text.Props.make ~selection_bg:Ansi.Color.blue () in
  let b = Text.Props.make () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_selection_fg_diff () =
  let a = Text.Props.make ~selection_fg:Ansi.Color.red () in
  let b = Text.Props.make () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_detects_tab_width_diff () =
  let a = Text.Props.make ~tab_width:4 () in
  let b = Text.Props.make ~tab_width:8 () in
  is_false ~msg:"different" (Text.Props.equal a b)

let props_equal_identical () =
  let a = Text.Props.make ~content:"hello" ~wrap:`Char ~tab_width:4 () in
  let b = Text.Props.make ~content:"hello" ~wrap:`Char ~tab_width:4 () in
  is_true ~msg:"equal" (Text.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, txt = make_text () in
  let node = Text.node txt in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_accessors () =
  let _t, txt = make_text () in
  let _node : Renderable.t = Text.node txt in
  let _buf : Text_buffer.t = Text.buffer txt in
  let _surface : Text_surface.t = Text.surface txt in
  ()

let create_with_content () =
  let _t, txt = make_text ~content:"hello" () in
  equal ~msg:"plain_text" string "hello"
    (Text_buffer.plain_text (Text.buffer txt))

let create_with_wrap () =
  let _t, txt = make_text ~wrap:`Char () in
  equal ~msg:"wrap" int 1
    (match Text_surface.wrap (Text.surface txt) with
    | `None -> 0
    | `Char -> 1
    | `Word -> 2)

let create_with_tab_width () =
  let _t, txt = make_text ~tab_width:8 () in
  equal ~msg:"tab_width" int 8 (Text_buffer.tab_width (Text.buffer txt))

(* ── Content — set_content ── *)

let set_content_updates () =
  let _t, txt = make_text ~content:"old" () in
  Text.set_content txt "new";
  equal ~msg:"plain_text" string "new"
    (Text_buffer.plain_text (Text.buffer txt))

let set_content_updates_line_count () =
  let _t, txt = make_text () in
  Text.set_content txt "a\nb\nc";
  equal ~msg:"line_count" int 3 (Text.line_count txt)

(* ── Content — set_styled_text ── *)

let set_styled_text_sets_content () =
  let _t, txt = make_text () in
  Text.set_styled_text txt
    [
      { Text_buffer.text = "red "; style = red_style };
      { Text_buffer.text = "green"; style = green_style };
    ];
  equal ~msg:"plain_text" string "red green"
    (Text_buffer.plain_text (Text.buffer txt))

let set_styled_text_prevents_props_overwrite () =
  let _t, txt = make_text ~content:"initial" () in
  Text.set_styled_text txt
    [ { Text_buffer.text = "styled"; style = red_style } ];
  (* apply_props with a different content should not overwrite *)
  Text.apply_props txt (Text.Props.make ~content:"overwritten" ());
  equal ~msg:"not overwritten" string "styled"
    (Text_buffer.plain_text (Text.buffer txt))

(* ── Content — set_text_style ── *)

let set_text_style_changes_default () =
  let _t, txt = make_text () in
  Text.set_text_style txt red_style;
  is_true ~msg:"default style changed"
    (Ansi.Style.equal red_style (Text_buffer.default_style (Text.buffer txt)))

(* ── Wrapping ── *)

let set_wrap_delegates () =
  let _t, txt = make_text () in
  Text.set_wrap txt `Word;
  equal ~msg:"wrap mode" int 2
    (match Text_surface.wrap (Text.surface txt) with
    | `None -> 0
    | `Char -> 1
    | `Word -> 2)

let set_tab_width_delegates () =
  let _t, txt = make_text () in
  Text.set_tab_width txt 4;
  equal ~msg:"tab_width" int 4 (Text_buffer.tab_width (Text.buffer txt))

(* ── Selection ── *)

let selected_text_empty_no_selection () =
  let _t, txt = make_text ~content:"hello" () in
  equal ~msg:"empty" string "" (Text.selected_text txt)

(* ── Highlights ── *)

let add_and_remove_highlight () =
  let t, txt = make_text ~content:"hello world" () in
  let h =
    Text_buffer.Highlight.make ~start_offset:0 ~end_offset:5 ~style:red_style
      ~ref_id:42 ()
  in
  let before = !(t.schedule_count) in
  Text.add_highlight txt h;
  is_true ~msg:"render requested" (!(t.schedule_count) > before);
  let hl = Text_buffer.highlights_in_range (Text.buffer txt) ~start:0 ~len:5 in
  equal ~msg:"1 highlight" int 1 (List.length hl);
  let before2 = !(t.schedule_count) in
  Text.remove_highlights_by_ref txt 42;
  is_true ~msg:"render requested again" (!(t.schedule_count) > before2);
  let hl2 = Text_buffer.highlights_in_range (Text.buffer txt) ~start:0 ~len:5 in
  equal ~msg:"0 highlights" int 0 (List.length hl2)

let clear_highlights_removes_all () =
  let _t, txt = make_text ~content:"hello" () in
  Text.add_highlight txt
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:3 ~style:red_style
       ~ref_id:1 ());
  Text.add_highlight txt
    (Text_buffer.Highlight.make ~start_offset:0 ~end_offset:3 ~style:green_style
       ~ref_id:2 ());
  Text.clear_highlights txt;
  let hl = Text_buffer.highlights_in_range (Text.buffer txt) ~start:0 ~len:5 in
  equal ~msg:"empty" int 0 (List.length hl)

(* ── Query ── *)

let line_count_reflects_buffer () =
  let _t, txt = make_text ~content:"a\nb\nc" () in
  equal ~msg:"line_count" int 3 (Text.line_count txt)

let display_line_count_delegates () =
  let _t, txt = make_text ~content:"a\nb\nc" () in
  equal ~msg:"display_line_count" int 3 (Text.display_line_count txt)

(* ── Fragments — constructors ── *)

let fragment_text_plain () =
  let f = Text.Fragment.text "hello" in
  match f with
  | Text.Text { text; style } ->
      equal ~msg:"text" string "hello" text;
      is_true ~msg:"no style" (Option.is_none style)
  | _ -> fail "expected Text"

let fragment_text_styled () =
  let f = Text.Fragment.text ~style:red_style "red" in
  match f with
  | Text.Text { text; style } ->
      equal ~msg:"text" string "red" text;
      is_true ~msg:"has style" (Option.is_some style)
  | _ -> fail "expected Text"

let fragment_span_groups () =
  let f =
    Text.Fragment.span [ Text.Fragment.text "a"; Text.Fragment.text "b" ]
  in
  match f with
  | Text.Span { style; children } ->
      is_true ~msg:"no style" (Option.is_none style);
      equal ~msg:"children" int 2 (List.length children)
  | _ -> fail "expected Span"

let fragment_bold () =
  let f = Text.Fragment.bold [ Text.Fragment.text "hi" ] in
  match f with
  | Text.Span { style = Some s; _ } ->
      is_true ~msg:"bold" (Ansi.Attr.mem Ansi.Attr.Bold s.attrs)
  | _ -> fail "expected Span with bold style"

let fragment_fg () =
  let f = Text.Fragment.fg Ansi.Color.red [ Text.Fragment.text "r" ] in
  match f with
  | Text.Span { style = Some _; children } ->
      equal ~msg:"1 child" int 1 (List.length children)
  | _ -> fail "expected Span with style"

(* ── Fragments — set_fragments / roundtrip ── *)

let set_fragments_and_read_back () =
  let _t, txt = make_text () in
  let frags = [ Text.Fragment.text "hello"; Text.Fragment.text " world" ] in
  Text.set_fragments txt frags;
  let got = Text.fragments txt in
  (* Normalization merges adjacent unstyled text fragments *)
  let expected = [ Text.Fragment.text "hello world" ] in
  is_true ~msg:"roundtrip" (Text.fragments_equal got expected)

let set_fragments_updates_buffer () =
  let _t, txt = make_text () in
  Text.set_fragments txt [ Text.Fragment.text "abc" ];
  equal ~msg:"plain_text" string "abc"
    (Text_buffer.plain_text (Text.buffer txt))

let set_fragments_styled () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [
      Text.Fragment.bold [ Text.Fragment.text "bold" ];
      Text.Fragment.text " plain";
    ];
  equal ~msg:"plain_text" string "bold plain" (Text.plain_text txt)

(* ── Fragments — fragments_equal ── *)

let fragments_equal_same () =
  let a = [ Text.Fragment.text "hi" ] in
  let b = [ Text.Fragment.text "hi" ] in
  is_true ~msg:"equal" (Text.fragments_equal a b)

let fragments_equal_different () =
  let a = [ Text.Fragment.text "hi" ] in
  let b = [ Text.Fragment.text "bye" ] in
  is_false ~msg:"different" (Text.fragments_equal a b)

let fragments_equal_empty () =
  is_true ~msg:"both empty" (Text.fragments_equal [] [])

let fragments_equal_different_style () =
  let a = [ Text.Fragment.text ~style:red_style "hi" ] in
  let b = [ Text.Fragment.text "hi" ] in
  is_false ~msg:"style differs" (Text.fragments_equal a b)

let fragments_equal_nested () =
  let a =
    [
      Text.Span
        {
          style = Some red_style;
          children = [ Text.Text { text = "hi"; style = None } ];
        };
    ]
  in
  let b =
    [
      Text.Span
        {
          style = Some red_style;
          children = [ Text.Text { text = "hi"; style = None } ];
        };
    ]
  in
  is_true ~msg:"nested equal" (Text.fragments_equal a b)

(* ── Fragments — normalization ── *)

let set_fragments_normalizes_empty () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [ Text.Fragment.text ""; Text.Fragment.text "hi"; Text.Fragment.text "" ];
  let frags = Text.fragments txt in
  (* Empty fragments should be removed *)
  equal ~msg:"count" int 1 (List.length frags);
  match List.hd frags with
  | Text.Text { text; _ } -> equal ~msg:"text" string "hi" text
  | _ -> fail "expected Text"

let set_fragments_normalizes_merge () =
  let _t, txt = make_text () in
  (* Adjacent Text nodes with same style should merge *)
  Text.set_fragments txt [ Text.Fragment.text "hel"; Text.Fragment.text "lo" ];
  let frags = Text.fragments txt in
  equal ~msg:"count" int 1 (List.length frags);
  match List.hd frags with
  | Text.Text { text; _ } -> equal ~msg:"merged" string "hello" text
  | _ -> fail "expected Text"

let set_fragments_no_merge_different_styles () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [ Text.Fragment.text ~style:red_style "a"; Text.Fragment.text "b" ];
  let frags = Text.fragments txt in
  equal ~msg:"count" int 2 (List.length frags)

(* ── Fragments — spans ── *)

let spans_from_fragments () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [ Text.Fragment.text "plain "; Text.Fragment.text ~style:red_style "red" ];
  let spans = Text.spans txt in
  equal ~msg:"count" int 2 (List.length spans);
  let s0 = List.nth spans 0 in
  let s1 = List.nth spans 1 in
  equal ~msg:"s0 text" string "plain " s0.text;
  equal ~msg:"s1 text" string "red" s1.text

let set_spans_roundtrip () =
  let _t, txt = make_text () in
  let input =
    [
      { Text.text = "hello "; style = None };
      { Text.text = "world"; style = Some red_style };
    ]
  in
  Text.set_spans txt input;
  let got = Text.spans txt in
  equal ~msg:"count" int 2 (List.length got);
  equal ~msg:"text0" string "hello " (List.nth got 0).text;
  equal ~msg:"text1" string "world" (List.nth got 1).text

let append_span_adds () =
  let _t, txt = make_text () in
  Text.set_spans txt [ { Text.text = "hello"; style = None } ];
  Text.append_span txt { Text.text = " world"; style = None };
  equal ~msg:"plain_text" string "hello world" (Text.plain_text txt)

let clear_spans_removes_all () =
  let _t, txt = make_text () in
  Text.set_fragments txt [ Text.Fragment.text "hello" ];
  Text.clear_spans txt;
  equal ~msg:"empty" string "" (Text_buffer.plain_text (Text.buffer txt))

(* ── Fragments — plain_text ── *)

let plain_text_from_fragments () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [
      Text.Fragment.bold [ Text.Fragment.text "bold " ];
      Text.Fragment.text "plain";
    ];
  equal ~msg:"plain_text" string "bold plain" (Text.plain_text txt)

let plain_text_nested () =
  let _t, txt = make_text () in
  Text.set_fragments txt
    [
      Text.Fragment.span
        [
          Text.Fragment.text "a";
          Text.Fragment.span [ Text.Fragment.text "b" ];
          Text.Fragment.text "c";
        ];
    ];
  equal ~msg:"plain_text" string "abc" (Text.plain_text txt)

(* ── Fragments — style inheritance via spans ── *)

let style_inheritance_span_to_text () =
  let _t, txt = make_text () in
  (* Span with red style containing unstyled text *)
  Text.set_fragments txt
    [
      Text.Span
        {
          style = Some red_style;
          children = [ Text.Text { text = "hello"; style = None } ];
        };
    ];
  let spans = Text.spans txt in
  equal ~msg:"count" int 1 (List.length spans);
  let s = List.hd spans in
  equal ~msg:"text" string "hello" s.text;
  (* The span should have inherited the red style from the parent *)
  is_true ~msg:"inherited style" (Option.is_some s.style)

(* ── apply_props ── *)

let apply_props_updates_content () =
  let _t, txt = make_text ~content:"old" () in
  Text.apply_props txt (Text.Props.make ~content:"new" ());
  equal ~msg:"content" string "new" (Text_buffer.plain_text (Text.buffer txt))

let apply_props_updates_wrap () =
  let _t, txt = make_text () in
  Text.apply_props txt (Text.Props.make ~wrap:`Word ());
  equal ~msg:"wrap" int 2
    (match Text_surface.wrap (Text.surface txt) with
    | `None -> 0
    | `Char -> 1
    | `Word -> 2)

let apply_props_updates_tab_width () =
  let _t, txt = make_text () in
  Text.apply_props txt (Text.Props.make ~tab_width:8 ());
  equal ~msg:"tab_width" int 8 (Text_buffer.tab_width (Text.buffer txt))

let apply_props_updates_text_style () =
  let _t, txt = make_text () in
  Text.apply_props txt (Text.Props.make ~text_style:red_style ());
  is_true ~msg:"style"
    (Ansi.Style.equal red_style (Text_buffer.default_style (Text.buffer txt)))

(* Regression: apply_props with both content and text_style must stamp the
   content spans with the new text_style, not the old default. *)
let apply_props_stamps_content_with_text_style () =
  let _t, txt = make_text () in
  let bold_style = Ansi.Style.make ~bold:true () in
  Text.apply_props txt
    (Text.Props.make ~content:"Bold text" ~text_style:bold_style ());
  let buf = Text.buffer txt in
  let spans = Text_buffer.line_spans buf 0 in
  equal ~msg:"span count" int 1 (List.length spans);
  let span = List.hd spans in
  is_true ~msg:"span carries bold style"
    (Ansi.Style.equal bold_style span.style)

(* Regression: apply_props with text_style change but same content must re-stamp
   the existing content with the new default style. *)
let apply_props_restamps_on_style_change () =
  let _t, txt = make_text ~content:"hello" () in
  Text.apply_props txt
    (Text.Props.make ~content:"hello" ~text_style:red_style ());
  let buf = Text.buffer txt in
  let spans = Text_buffer.line_spans buf 0 in
  equal ~msg:"span count" int 1 (List.length spans);
  let span = List.hd spans in
  is_true ~msg:"span carries red style" (Ansi.Style.equal red_style span.style)

(* ── Runner ── *)

let () =
  run "mosaic.text"
    [
      group "Props"
        [
          test "default equals make()" props_default_equals_make;
          test "detects content diff" props_detects_content_diff;
          test "detects text_style diff" props_detects_text_style_diff;
          test "detects wrap diff" props_detects_wrap_diff;
          test "detects selectable diff" props_detects_selectable_diff;
          test "detects selection_bg diff" props_detects_selection_bg_diff;
          test "detects selection_fg diff" props_detects_selection_fg_diff;
          test "detects tab_width diff" props_detects_tab_width_diff;
          test "equal on identical" props_equal_identical;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "accessors work" create_accessors;
          test "initial content" create_with_content;
          test "initial wrap" create_with_wrap;
          test "initial tab_width" create_with_tab_width;
        ];
      group "Content — set_content"
        [
          test "updates buffer" set_content_updates;
          test "updates line count" set_content_updates_line_count;
        ];
      group "Content — set_styled_text"
        [
          test "sets content" set_styled_text_sets_content;
          test "prevents props overwrite"
            set_styled_text_prevents_props_overwrite;
        ];
      group "Content — set_text_style"
        [ test "changes default style" set_text_style_changes_default ];
      group "Wrapping"
        [
          test "set_wrap delegates" set_wrap_delegates;
          test "set_tab_width delegates" set_tab_width_delegates;
        ];
      group "Selection"
        [ test "empty when no selection" selected_text_empty_no_selection ];
      group "Highlights"
        [
          test "add and remove" add_and_remove_highlight;
          test "clear all" clear_highlights_removes_all;
        ];
      group "Query"
        [
          test "line_count reflects buffer" line_count_reflects_buffer;
          test "display_line_count delegates" display_line_count_delegates;
        ];
      group "Fragments — constructors"
        [
          test "text plain" fragment_text_plain;
          test "text styled" fragment_text_styled;
          test "span groups children" fragment_span_groups;
          test "bold convenience" fragment_bold;
          test "fg convenience" fragment_fg;
        ];
      group "Fragments — set_fragments"
        [
          test "set and read back" set_fragments_and_read_back;
          test "updates buffer" set_fragments_updates_buffer;
          test "styled fragments" set_fragments_styled;
        ];
      group "Fragments — fragments_equal"
        [
          test "same" fragments_equal_same;
          test "different" fragments_equal_different;
          test "both empty" fragments_equal_empty;
          test "different style" fragments_equal_different_style;
          test "nested" fragments_equal_nested;
        ];
      group "Fragments — normalization"
        [
          test "removes empty" set_fragments_normalizes_empty;
          test "merges adjacent same-style" set_fragments_normalizes_merge;
          test "no merge different styles"
            set_fragments_no_merge_different_styles;
        ];
      group "Fragments — spans"
        [
          test "spans from fragments" spans_from_fragments;
          test "set_spans roundtrip" set_spans_roundtrip;
          test "append_span" append_span_adds;
          test "clear_spans" clear_spans_removes_all;
        ];
      group "Fragments — plain_text"
        [
          test "from fragments" plain_text_from_fragments;
          test "nested" plain_text_nested;
        ];
      group "Fragments — style inheritance"
        [ test "span to text" style_inheritance_span_to_text ];
      group "apply_props"
        [
          test "updates content" apply_props_updates_content;
          test "updates wrap" apply_props_updates_wrap;
          test "updates tab_width" apply_props_updates_tab_width;
          test "updates text_style" apply_props_updates_text_style;
          test "stamps content with text_style"
            apply_props_stamps_content_with_text_style;
          test "restamps on style change" apply_props_restamps_on_style_change;
        ];
    ]
