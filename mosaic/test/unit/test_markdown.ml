open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_md ?content ?conceal ?streaming ?render_node ?render_code () =
  let t = make_ctx () in
  let root = make_root t in
  let md =
    Markdown.create ~parent:root ?content ?conceal ?streaming ?render_node
      ?render_code ()
  in
  (t, md)

(* ── Construction ── *)

let create_default_content () =
  let _t, md = make_md () in
  equal ~msg:"empty" string "" (Markdown.content md)

let create_with_content () =
  let _t, md = make_md ~content:"# Hello" () in
  equal ~msg:"content" string "# Hello" (Markdown.content md)

let node_is_valid () =
  let _t, md = make_md () in
  is_false ~msg:"not destroyed" (Renderable.destroyed (Markdown.node md))

let node_has_parent () =
  let _t, md = make_md () in
  is_some ~msg:"has parent" (Renderable.parent (Markdown.node md))

(* ── Content ── *)

let set_content_updates_accessor () =
  let _t, md = make_md () in
  Markdown.set_content md "# World";
  equal ~msg:"updated" string "# World" (Markdown.content md)

let set_content_same_is_noop () =
  let t, md = make_md ~content:"# Hello" () in
  let before = !(t.schedule_count) in
  Markdown.set_content md "# Hello";
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_content_empty_clears () =
  let _t, md = make_md ~content:"# Hello" () in
  Markdown.set_content md "";
  equal ~msg:"empty" string "" (Markdown.content md)

let set_content_appending () =
  let _t, md = make_md ~content:"# Hello" () in
  Markdown.set_content md "# Hello\n\nWorld";
  equal ~msg:"appended" string "# Hello\n\nWorld" (Markdown.content md)

(* ── Configuration ── *)

let set_conceal_toggles () =
  let _t, md = make_md ~conceal:true () in
  Markdown.set_conceal md false;
  Markdown.set_conceal md true

let set_conceal_same_is_noop () =
  let t, md = make_md ~content:"**bold**" ~conceal:true () in
  let before = !(t.schedule_count) in
  Markdown.set_conceal md true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_streaming_toggles () =
  let _t, md = make_md ~streaming:false () in
  Markdown.set_streaming md true;
  Markdown.set_streaming md false

let set_streaming_same_is_noop () =
  let t, md = make_md ~content:"text" ~streaming:false () in
  let before = !(t.schedule_count) in
  Markdown.set_streaming md false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_style_custom () =
  let _t, md = make_md ~content:"# Heading" () in
  let custom_style = function
    | Markdown.Default -> Ansi.Style.default
    | _ -> Ansi.Style.default
  in
  Markdown.set_style md custom_style

let selection_callback_reports_rendered_text () =
  let renderer = Renderer.create () in
  let selected = ref None in
  let _md =
    Markdown.create ~parent:(Renderer.root renderer) ~content:"Hello **world**"
      ~on_selection:(fun text -> selected := text)
      ()
  in
  Renderer.render_frame renderer ~width:40 ~height:5 ~delta:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let mouse kind =
    Input.Mouse.make ~x:0 ~y:0 ~modifiers:Input.Modifier.none kind
  in
  Renderer.dispatch_mouse renderer (mouse (Down { button = Left }));
  Renderer.dispatch_mouse renderer
    (Input.Mouse.make ~x:5 ~y:0 ~modifiers:Input.Modifier.none
       (Drag { button = Left }));
  some ~msg:"selected markdown text" string "Hello" !selected

let selection_callback_clears () =
  let renderer = Renderer.create () in
  let selected = ref (Some "stale") in
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ~content:"Hello"
      ~on_selection:(fun text -> selected := text)
      ()
  in
  Renderer.render_frame renderer ~width:40 ~height:5 ~delta:0.;
  ignore (Renderer.render ~full:true renderer : string);
  Markdown.set_on_selection md None;
  Markdown.set_on_selection md (Some (fun text -> selected := text));
  Markdown.set_selectable md false;
  let mouse kind =
    Input.Mouse.make ~x:0 ~y:0 ~modifiers:Input.Modifier.none kind
  in
  Renderer.dispatch_mouse renderer (mouse (Down { button = Left }));
  Renderer.dispatch_mouse renderer
    (Input.Mouse.make ~x:5 ~y:0 ~modifiers:Input.Modifier.none
       (Drag { button = Left }));
  some ~msg:"selection did not change while disabled" string "stale" !selected

(* ── Lifecycle ── *)

let create_streaming () =
  let _t, md = make_md ~streaming:true ~content:"# Hello" () in
  is_false ~msg:"not destroyed" (Renderable.destroyed (Markdown.node md))

let create_no_conceal () =
  let _t, md = make_md ~conceal:false ~content:"**bold**" () in
  is_false ~msg:"not destroyed" (Renderable.destroyed (Markdown.node md))

let create_custom_render_node_none () =
  let _t, md =
    make_md ~content:"# Hello"
      ~render_node:(fun _block ~parent:_ ~is_last:_ -> None)
      ()
  in
  is_false ~msg:"not destroyed" (Renderable.destroyed (Markdown.node md))

let create_custom_render_code () =
  let used = ref false in
  let _t, _md =
    make_md ~content:"```\ncode\n```"
      ~render_code:(fun ~parent ~language:_ ~content:_ ->
        used := true;
        Renderable.create ~parent ())
      ()
  in
  is_true ~msg:"render_code called" !used

let pp_does_not_crash () =
  let _t, md = make_md ~content:"# Hello\n\nWorld" () in
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Markdown.pp ppf md;
  Format.pp_print_flush ppf ();
  is_true ~msg:"produced output" (Buffer.length buf > 0)

(* ── Runner ── *)

let () =
  run "mosaic.markdown"
    [
      group "Construction"
        [
          test "default content" create_default_content;
          test "with content" create_with_content;
          test "node is valid" node_is_valid;
          test "node has parent" node_has_parent;
        ];
      group "Content"
        [
          test "set_content updates" set_content_updates_accessor;
          test "same content no-op" set_content_same_is_noop;
          test "set_content empty" set_content_empty_clears;
          test "set_content appending" set_content_appending;
        ];
      group "Configuration"
        [
          test "set_conceal toggles" set_conceal_toggles;
          test "set_conceal same no-op" set_conceal_same_is_noop;
          test "set_streaming toggles" set_streaming_toggles;
          test "set_streaming same no-op" set_streaming_same_is_noop;
          test "set_style custom" set_style_custom;
          test "selection callback reports rendered text"
            selection_callback_reports_rendered_text;
          test "selection disabled suppresses callback"
            selection_callback_clears;
        ];
      group "Lifecycle"
        [
          test "streaming mode" create_streaming;
          test "no conceal mode" create_no_conceal;
          test "custom render_node returning None"
            create_custom_render_node_none;
          test "custom render_code" create_custom_render_code;
          test "pp does not crash" pp_does_not_crash;
        ];
    ]
