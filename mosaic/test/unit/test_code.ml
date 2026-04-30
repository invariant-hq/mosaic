open Windtrap
open Mosaic_ui
open Test_harness

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let blue = Ansi.Style.fg Ansi.Color.blue Ansi.Style.default

let syntax_style color =
  Syntax_style.make ~base:Ansi.Style.default [ ("keyword", color) ]

let highlights = Syntax_highlight.of_triples [ (0, 3, "keyword") ]

let make_code ?content ?syntax ?text_style () =
  let t = make_ctx () in
  let root = make_root t in
  let code = Code.create ~parent:root ?content ?syntax ?text_style () in
  (t, code)

let line_spans code = Text_buffer.line_spans (Code.buffer code) 0
let span_text span = (span : Text_buffer.span).text
let span_style span = (span : Text_buffer.span).style

let plain_without_syntax () =
  let _ctx, code = make_code ~content:"let x = 1" () in
  equal ~msg:"plain text" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  equal ~msg:"span count" int 1 (List.length (line_spans code))

let precomputed_highlights_apply_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let invalid_highlights_fall_back_to_plain_text () =
  let highlights = Syntax_highlight.of_triples [ (0, 20, "keyword") ] in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let" ~syntax () in
  equal ~msg:"plain text" string "let"
    (Text_buffer.plain_text (Code.buffer code));
  match line_spans code with
  | [ span ] ->
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let conceal_metadata_applies () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let highlights =
    [
      Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"keyword" ();
    ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  equal ~msg:"concealed text" string "* x = 1"
    (Text_buffer.plain_text (Code.buffer code))

let set_content_reapplies_syntax () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_content code "var x";
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "var" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let set_syntax_none_renders_plain () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_syntax code None;
  match line_spans code with
  | [ span ] ->
      equal ~msg:"plain text" string "let x" (span_text span);
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let apply_props_updates_syntax_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let syntax = Code.syntax ~style:(syntax_style blue) highlights in
  Code.apply_props code (Code.Props.make ~content:"let x" ~syntax ());
  match line_spans code with
  | first :: _ ->
      is_true ~msg:"updated style" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let text_style_restamps_plain_content () =
  let _ctx, code = make_code ~content:"plain" () in
  Code.apply_props code (Code.Props.make ~content:"plain" ~text_style:red ());
  match line_spans code with
  | [ span ] ->
      is_true ~msg:"restamped style" (Ansi.Style.equal red (span_style span))
  | _ -> fail "expected one plain span"

let is_highlighting_false_for_sync_highlights () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code)

let () =
  run "mosaic.code"
    [
      group "Syntax"
        [
          test "plain without syntax" plain_without_syntax;
          test "precomputed highlights apply style"
            precomputed_highlights_apply_style;
          test "invalid highlights fall back to plain text"
            invalid_highlights_fall_back_to_plain_text;
          test "conceal metadata applies" conceal_metadata_applies;
        ];
      group "Updates"
        [
          test "set_content reapplies syntax" set_content_reapplies_syntax;
          test "set_syntax None renders plain" set_syntax_none_renders_plain;
          test "apply_props updates syntax style"
            apply_props_updates_syntax_style;
          test "text_style restamps plain content"
            text_style_restamps_plain_content;
          test "is_highlighting false for sync highlights"
            is_highlighting_false_for_sync_highlights;
        ];
    ]
