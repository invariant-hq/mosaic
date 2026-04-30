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

let render_code code =
  let grid = make_grid ~width:40 ~height:5 () in
  Renderable.Private.render_full (Code.node code) ~grid ~delta:0.

let line_spans code = Text_buffer.line_spans (Code.buffer code) 0
let span_text span = (span : Text_buffer.span).text
let span_style span = (span : Text_buffer.span).style

type async_job = {
  request : Code.Highlighter.request;
  notify : unit -> unit;
  mutable outcome : Code.Highlighter.result option;
  mutable cancelled : bool;
}

let async_highlighter () =
  let jobs = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify ->
        let job = { request; notify; outcome = None; cancelled = false } in
        jobs := !jobs @ [ job ];
        Code.Highlighter.job
          ~poll:(fun () ->
            match job.outcome with
            | None -> None
            | Some outcome ->
                job.outcome <- None;
                Some outcome)
          ~cancel:(fun () -> job.cancelled <- true))
  in
  (jobs, highlighter)

let complete job outcome =
  job.outcome <- Some outcome;
  job.notify ()

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

let sync_highlighter_applies_style () =
  let highlighter =
    Code.Highlighter.sync (fun request ->
        equal ~msg:"request content" string "let x" request.content;
        equal ~msg:"request language" string "ocaml" request.language;
        highlights)
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let async_highlighter_applies_completed_result () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"plain while pending" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  let job = List.hd !jobs in
  equal ~msg:"request content" string "let x" job.request.content;
  equal ~msg:"request language" string "ocaml" job.request.language;
  complete job (Ok highlights);
  render_code code;
  is_false ~msg:"done highlighting" (Code.is_highlighting code);
  match line_spans code with
  | first :: _ ->
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let async_draw_unstyled_false_hides_pending_render () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      ~draw_unstyled:false highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"content remains measurable" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  is_false ~msg:"render disabled while pending"
    (Text_surface.render_enabled (Code.surface code));
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  is_true ~msg:"render enabled after highlight"
    (Text_surface.render_enabled (Code.surface code));
  equal ~msg:"content after highlight" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

let async_streaming_retains_previous_buffer () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      ~streaming:true highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"initial highlighted content" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  Code.set_content code "var x";
  equal ~msg:"previous content retained" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job (Ok highlights);
  render_code code;
  equal ~msg:"fresh content after highlight" string "var x"
    (Text_buffer.plain_text (Code.buffer code))

let async_content_change_cancels_previous_job () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  Code.set_content code "var x";
  is_true ~msg:"first job cancelled" first_job.cancelled;
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"current plain text retained" string "var x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job (Ok highlights);
  render_code code;
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted current content" string "var" (span_text first)
  | [] -> fail "expected highlighted span"

let async_line_info_stabilizes_on_completion () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let changes = ref 0 in
  Code.set_on_line_info_change code (Some (fun () -> incr changes));
  is_false ~msg:"pending line info" (Code.line_info_stable code);
  complete (List.hd !jobs) (Ok highlights);
  equal ~msg:"callback waits for polling" int 0 !changes;
  render_code code;
  is_true ~msg:"stable line info" (Code.line_info_stable code);
  equal ~msg:"line info callback" int 1 !changes

let async_starter_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        raise (Failure "starter failed"))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"render enabled after fallback"
    (Text_surface.render_enabled (Code.surface code))

let async_poll_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        Code.Highlighter.job
          ~poll:(fun () -> raise (Failure "poll failed"))
          ~cancel:(fun () -> ()))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

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
          test "sync highlighter applies style" sync_highlighter_applies_style;
          test "async highlighter applies completed result"
            async_highlighter_applies_completed_result;
          test "async draw_unstyled false hides pending render"
            async_draw_unstyled_false_hides_pending_render;
          test "async streaming retains previous buffer"
            async_streaming_retains_previous_buffer;
          test "async content change cancels previous job"
            async_content_change_cancels_previous_job;
          test "async line info stabilizes on completion"
            async_line_info_stabilizes_on_completion;
          test "async starter exception falls back to plain text"
            async_starter_exception_falls_back_to_plain_text;
          test "async poll exception falls back to plain text"
            async_poll_exception_falls_back_to_plain_text;
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
