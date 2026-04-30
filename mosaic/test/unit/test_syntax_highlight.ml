open Windtrap
open Mosaic_ui

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let green = Ansi.Style.fg Ansi.Color.green Ansi.Style.default
let bold = Ansi.Style.with_bold true Ansi.Style.default

let style =
  Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", red); ("keyword.control", green); ("markup.raw.block", red) ]

let span_text (span : Text_buffer.span) = span.text
let span_style (span : Text_buffer.span) = span.style

let text_of_spans spans =
  let buf = Buffer.create 32 in
  List.iter (fun span -> Buffer.add_string buf (span_text span)) spans;
  Buffer.contents buf

let style_at spans index = span_style (List.nth spans index)

let of_triples_applies_style () =
  let spans =
    Syntax_highlight.of_triples [ (0, 3, "keyword") ]
    |> Syntax_highlight.to_spans ~style ~content:"abcdef"
  in
  equal ~msg:"span count" int 2 (List.length spans);
  equal ~msg:"highlighted text" string "abc" (span_text (List.nth spans 0));
  is_true ~msg:"highlighted style" (Ansi.Style.equal red (style_at spans 0));
  equal ~msg:"plain text" string "def" (span_text (List.nth spans 1));
  is_true ~msg:"plain style"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 1))

let overlapping_same_scope_keeps_later_range_active () =
  let ranges =
    Syntax_highlight.of_triples [ (0, 4, "keyword"); (2, 6, "keyword") ]
  in
  let spans = Syntax_highlight.to_spans ~style ~content:"abcdef" ranges in
  equal ~msg:"text" string "abcdef" (text_of_spans spans);
  equal ~msg:"merged span count" int 1 (List.length spans);
  is_true ~msg:"style remains active through second range"
    (Ansi.Style.equal red (style_at spans 0))

let specificity_cascades_from_parent_to_child () =
  let style =
    Syntax_style.make ~base:Ansi.Style.default
      [ ("keyword", bold); ("keyword.control", green) ]
  in
  let spans =
    Syntax_highlight.of_triples [ (0, 6, "keyword"); (0, 6, "keyword.control") ]
    |> Syntax_highlight.to_spans ~style ~content:"return"
  in
  let expected = Ansi.Style.merge ~base:bold ~overlay:green in
  equal ~msg:"span count" int 1 (List.length spans);
  is_true ~msg:"merged style" (Ansi.Style.equal expected (style_at spans 0))

let conceal_replaces_range () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let ranges =
    [
      Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"conceal" ();
    ]
  in
  let spans = Syntax_highlight.to_spans ~style ~content:"abc def" ranges in
  equal ~msg:"concealed text" string "* def" (text_of_spans spans)

let injection_container_suppresses_container_style () =
  let container_meta =
    { Syntax_highlight.default_meta with contains_injection = true }
  in
  let injected_meta =
    { Syntax_highlight.default_meta with is_injection = true }
  in
  let ranges =
    [
      Syntax_highlight.range ~meta:container_meta ~start_byte:0 ~end_byte:4
        ~scope:"markup.raw.block" ();
      Syntax_highlight.range ~meta:injected_meta ~start_byte:1 ~end_byte:3
        ~scope:"keyword.control" ();
    ]
  in
  let spans = Syntax_highlight.to_spans ~style ~content:"xxxx" ranges in
  equal ~msg:"text" string "xxxx" (text_of_spans spans);
  equal ~msg:"span count" int 3 (List.length spans);
  is_true ~msg:"container prefix is unstyled"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 0));
  is_true ~msg:"injection is styled" (Ansi.Style.equal green (style_at spans 1));
  is_true ~msg:"container suffix is unstyled"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 2))

let invalid_range_raises () =
  raises_match ~msg:"out of bounds"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Syntax_highlight.of_triples [ (0, 10, "keyword") ]
        |> Syntax_highlight.to_spans ~style ~content:"abc"))

let () =
  run "mosaic.syntax_highlight"
    [
      group "Ranges"
        [
          test "of_triples applies style" of_triples_applies_style;
          test "overlapping same scope keeps later range active"
            overlapping_same_scope_keeps_later_range_active;
          test "specificity cascades from parent to child"
            specificity_cascades_from_parent_to_child;
          test "invalid range raises" invalid_range_raises;
        ];
      group "Metadata"
        [
          test "conceal replaces range" conceal_replaces_range;
          test "injection container suppresses container style"
            injection_container_suppresses_container_style;
        ];
    ]
