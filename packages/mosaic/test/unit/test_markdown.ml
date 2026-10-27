open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let dispatch_mouse r ev =
  ignore (Test_harness.dispatch_mouse r ev : Event.mouse)

let make_md ?content ?conceal ?streaming ?render_node ?render_code ?highlighter
    ?syntax_style () =
  let t = make_ctx () in
  let root = make_root t in
  let md =
    Markdown.create ~parent:root
      (Markdown.Props.make ?content ?conceal ?streaming ?render_node
         ?render_code ?highlighter ?syntax_style ())
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
  let before = Test_harness.clean t in
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
  let before = Test_harness.clean t in
  Markdown.set_conceal md true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_streaming_toggles () =
  let _t, md = make_md ~streaming:false () in
  Markdown.set_streaming md true;
  Markdown.set_streaming md false

let set_streaming_same_is_noop () =
  let t, md = make_md ~content:"text" ~streaming:false () in
  let before = Test_harness.clean t in
  Markdown.set_streaming md false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_style_custom () =
  let _t, md = make_md ~content:"# Heading" () in
  Markdown.set_style md
    (Markdown.Style.make [ (Markdown.Heading 1, Ansi.Style.default) ])

(* A synchronous highlighter that records its requests and styles the first
   three bytes as a keyword. *)
let recording_highlighter () =
  let requests = ref [] in
  let highlighter =
    Code.Highlighter.sync (fun ~language ~content ->
        requests := (language, content) :: !requests;
        Syntax_highlight.of_triples ~content
          [ (0, min 3 (String.length content), "keyword") ])
  in
  (requests, highlighter)

let set_highlighter_rerenders_existing_block () =
  (* The code block is first rendered with the default renderer, then
     [set_highlighter] must re-render it through the highlighter. *)
  let _t, md = make_md ~content:"```ocaml\ncode\n```" () in
  let requests, highlighter = recording_highlighter () in
  Markdown.set_highlighter md (Some highlighter);
  equal ~msg:"highlighter called on re-render" int 1 (List.length !requests)

let selection_callback_reports_rendered_text () =
  let renderer = Renderer.create () in
  let selected = ref None in
  let _md =
    Markdown.create ~parent:(Renderer.root renderer)
      ~on_selection:(fun text -> selected := text)
      (Markdown.Props.make ~content:"Hello **world**" ())
  in
  Renderer.render_frame renderer ~width:40 ~height:5 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let mouse kind = Test_harness.mouse_report ~x:0 ~y:0 kind in
  dispatch_mouse renderer (mouse (Press Left));
  dispatch_mouse renderer
    (Test_harness.mouse_report ~x:5 ~y:0 (Motion (Some Left)));
  equal ~msg:"selected markdown text" (option string) (Some "Hello") !selected

let selection_callback_clears () =
  let renderer = Renderer.create () in
  let selected = ref (Some "stale") in
  let md =
    Markdown.create ~parent:(Renderer.root renderer)
      ~on_selection:(fun text -> selected := text)
      (Markdown.Props.make ~content:"Hello" ())
  in
  Renderer.render_frame renderer ~width:40 ~height:5 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  Markdown.set_on_selection md None;
  Markdown.set_on_selection md (Some (fun text -> selected := text));
  Markdown.set_selectable md false;
  let mouse kind = Test_harness.mouse_report ~x:0 ~y:0 kind in
  dispatch_mouse renderer (mouse (Press Left));
  dispatch_mouse renderer
    (Test_harness.mouse_report ~x:5 ~y:0 (Motion (Some Left)));
  equal ~msg:"selection did not change while disabled" (option string)
    (Some "stale") !selected

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

let highlighter_receives_fence_language () =
  let requests, highlighter = recording_highlighter () in
  let _t, _md =
    make_md ~content:"```ocaml\ncode\n```\n\n```\nplain\n```" ~highlighter ()
  in
  equal ~msg:"only the fence naming a language is highlighted"
    (list (pair string string))
    [ ("ocaml", "code") ]
    !requests

let render_code_takes_precedence_over_highlighter () =
  (* When both are set, [render_code] wins for code blocks. *)
  let render_code_used = ref false in
  let requests, highlighter = recording_highlighter () in
  let _t, _md =
    make_md ~content:"```ocaml\ncode\n```"
      ~render_code:(fun ~parent ~language:_ ~content:_ ->
        render_code_used := true;
        Renderable.create ~parent ())
      ~highlighter ()
  in
  is_true ~msg:"render_code used" !render_code_used;
  equal ~msg:"highlighter not used" int 0 (List.length !requests)

(* Fences highlighted by one highlighter, rebuilt by a view on every refresh,
   with a style the view takes from its model. *)
let fences n =
  String.concat "\n"
    (List.init n (fun i ->
         Printf.sprintf "Block %d\n\n```ocaml\nlet x%d = %d\n```\n" i i i))

let view_refreshes_do_not_highlight_again () =
  let requests, highlighter = recording_highlighter () in
  let theme () =
    Syntax_style.make ~base:Ansi.Style.default
      [ ("keyword", Ansi.Style.fg Ansi.Color.red Ansi.Style.default) ]
  in
  let props () =
    Markdown.Props.make ~content:(fences 20) ~highlighter
      ~syntax_style:(theme ()) ()
  in
  let _t, md = make_md () in
  for _ = 1 to 5 do
    Markdown.apply_props md (props ())
  done;
  equal ~msg:"each fence highlighted once over five refreshes" int 20
    (List.length !requests)

let style_change_restyles_without_highlighting () =
  let requests, highlighter = recording_highlighter () in
  let style color =
    Syntax_style.make ~base:Ansi.Style.default
      [ ("keyword", Ansi.Style.fg color Ansi.Style.default) ]
  in
  let _t, md =
    make_md ~content:"```ocaml\nlet x\n```" ~highlighter
      ~syntax_style:(style Ansi.Color.red) ()
  in
  Markdown.set_syntax_style md (style Ansi.Color.red);
  Markdown.set_syntax_style md (style Ansi.Color.blue);
  equal ~msg:"a style change does not highlight again" int 1
    (List.length !requests)

(* An element style a view rebuilds from its model on every refresh. *)
let element_style_rebuilt_renders_nothing () =
  let requests, highlighter = recording_highlighter () in
  let model_color = Ansi.Color.red in
  let props () =
    Markdown.Props.make ~content:(fences 20) ~highlighter
      ~style:
        (Markdown.Style.make
           [ (Markdown.Strong, Ansi.Style.fg model_color Ansi.Style.default) ])
      ()
  in
  let t, md = make_md () in
  Markdown.apply_props md (props ());
  let scheduled = Test_harness.clean t in
  for _ = 1 to 4 do
    Markdown.apply_props md (props ())
  done;
  equal ~msg:"each fence highlighted once over five refreshes" int 20
    (List.length !requests);
  equal ~msg:"no render scheduled" int scheduled !(t.schedule_count)

let element_style_change_rerenders () =
  let renderer = Renderer.create () in
  let heading color =
    Markdown.Style.make
      [ (Markdown.Heading 1, Ansi.Style.fg color Ansi.Style.default) ]
  in
  let md =
    Markdown.create ~parent:(Renderer.root renderer)
      (Markdown.Props.make ~content:"# Title" ~style:(heading Ansi.Color.red) ())
  in
  let fg () =
    Renderer.render_frame renderer ~width:20 ~height:2 ~dt:0.;
    ignore (Renderer.render ~full:true renderer : string);
    let grid = Matrix_screen.current_grid (Renderer.screen renderer) in
    Matrix_grid.get_fg grid (Matrix_grid.idx grid ~x:0 ~y:0)
  in
  is_true ~msg:"first style" (Ansi.Color.equal Ansi.Color.red (fg ()));
  Markdown.set_style md (heading Ansi.Color.red);
  Markdown.set_style md (heading Ansi.Color.blue);
  is_true ~msg:"changed style" (Ansi.Color.equal Ansi.Color.blue (fg ()))

let element_style_values () =
  let link = Ansi.Style.make ~underline:true () in
  let a = Markdown.Style.make [ (Markdown.Link, link) ] in
  is_true ~msg:"rebuilt"
    (Markdown.Style.equal a (Markdown.Style.make [ (Markdown.Link, link) ]));
  is_false ~msg:"changed" (Markdown.Style.equal a Markdown.Style.default);
  is_true ~msg:"other keys from the base"
    (Ansi.Style.equal
       (Markdown.Style.get Markdown.Style.default Markdown.Strong)
       (Markdown.Style.get a Markdown.Strong));
  is_true ~msg:"last binding wins"
    (Ansi.Style.equal link
       (Markdown.Style.get
          (Markdown.Style.make
             [ (Markdown.Link, Ansi.Style.default); (Markdown.Link, link) ])
          Markdown.Link));
  raises ~msg:"heading level outside 1..6"
    (Invalid_argument "Markdown.Style: heading level 7 is not in 1..6")
    (fun () -> Markdown.Style.make [ (Markdown.Heading 7, link) ])

(* ── Incremental updates ── *)

let apply_props_same_content_no_rerender () =
  let t, md = make_md ~content:"# Hello\n\nWorld" () in
  let props = Markdown.Props.make ~content:"# Hello\n\nWorld" () in
  let before = Test_harness.clean t in
  Markdown.apply_props md props;
  equal ~msg:"no schedule for an unchanged update" int before
    !(t.schedule_count)

let streaming_append_reuses_prefix_blocks () =
  (* Quote, list, and highlighted code blocks in the unchanged prefix must
     keep their widgets across a streaming append; only the tail may change. *)
  let _requests, highlighter = recording_highlighter () in
  let base =
    "> quoted\n\n- item one\n- item two\n\n```ocaml\nlet x = 1\n```\n\ntail"
  in
  let _t, md = make_md ~content:base ~streaming:true ~highlighter () in
  let before = Renderable.children (Markdown.node md) in
  Markdown.apply_props md
    (Markdown.Props.make
       ~content:(base ^ " more\n\nanother paragraph")
       ~streaming:true ~highlighter ());
  let after = Renderable.children (Markdown.node md) in
  equal ~msg:"one block appended" int
    (List.length before + 1)
    (List.length after);
  match (before, after) with
  | b0 :: b1 :: b2 :: _, a0 :: a1 :: a2 :: _ ->
      is_true ~msg:"quote widget reused" (b0 == a0);
      is_true ~msg:"list widget reused" (b1 == a1);
      is_true ~msg:"code widget reused" (b2 == a2)
  | _ -> fail "expected at least three blocks"

let pp_does_not_crash () =
  let _t, md = make_md ~content:"# Hello\n\nWorld" () in
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Markdown.pp ppf md;
  Format.pp_print_flush ppf ();
  gt ~msg:"produced output (bytes)" ~than:0 (Buffer.length buf)

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.markdown}) or to
   {!Markdown.Props.make} for {!Markdown.create}, reaches the live markdown
   ({!Test_harness.reaches}). The element's [md_style] is the widget's
   [style]. *)

let doors_size = Mosaic.size ~width:24 ~height:6
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let doors_md = "**bold** and `code`"
let doors_table = "| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |"
let doors_fenced = "```ocaml\nlet x\n```"

let doors_md_style =
  Markdown.Style.make [ (Strong, Ansi.Style.make ~fg:doors_red ()) ]

let doors_hl =
  Code.Highlighter.sync (fun ~language:_ ~content ->
      Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])

let doors_syntax_style =
  Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", Ansi.Style.make ~fg:doors_red ()) ]

let doors_selected renderer _ =
  Option.value ~default:"(none)" (Renderer.selection_text renderer)

let doors_content () =
  Test_harness.reaches "content"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:(Mosaic.markdown ~size:doors_size "# Title")
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:"# Title" ())))

let doors_style_label () =
  Test_harness.reaches "style (the element's md_style)"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:
      (Mosaic.markdown ~size:doors_size ~md_style:doors_md_style doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~style:doors_md_style ())))

let doors_conceal () =
  Test_harness.reaches "conceal"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:(Mosaic.markdown ~size:doors_size ~conceal:false doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~conceal:false ())))

let doors_streaming () =
  Test_harness.reaches "streaming"
    ~base:(Mosaic.markdown ~size:doors_size doors_table)
    ~element:(Mosaic.markdown ~size:doors_size ~streaming:true doors_table)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_table ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_table ~streaming:true ())))

let doors_selectable () =
  Test_harness.reaches ~act:Test_harness.select_text ~observe:doors_selected
    "selectable"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:(Mosaic.markdown ~size:doors_size ~selectable:false doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~selectable:false ())))

let doors_selection_bg () =
  Test_harness.reaches ~act:Test_harness.select_text "selection_bg"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:(Mosaic.markdown ~size:doors_size ~selection_bg:doors_red doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~selection_bg:doors_red ())))

let doors_selection_fg () =
  Test_harness.reaches ~act:Test_harness.select_text "selection_fg"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:(Mosaic.markdown ~size:doors_size ~selection_fg:doors_red doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~selection_fg:doors_red ())))

let doors_highlighter () =
  Test_harness.reaches "highlighter"
    ~base:(Mosaic.markdown ~size:doors_size doors_fenced)
    ~element:
      (Mosaic.markdown ~size:doors_size ~highlighter:doors_hl doors_fenced)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced ~highlighter:doors_hl ())))

let doors_syntax_style () =
  Test_harness.reaches "syntax_style"
    ~base:(Mosaic.markdown ~size:doors_size ~highlighter:doors_hl doors_fenced)
    ~element:
      (Mosaic.markdown ~size:doors_size ~highlighter:doors_hl
         ~syntax_style:doors_syntax_style doors_fenced)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced ~highlighter:doors_hl ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced ~highlighter:doors_hl
              ~syntax_style:doors_syntax_style ())))

(* A renderer of the doors' tests draws a paragraph, or a code block, as a
   text of its own. The same function is given on every declaration, as
   props compare it physically. *)
let doors_custom parent text =
  Text.node (Text.create ~parent (Text.Props.make ~content:text ()))

let doors_render_node block ~parent ~is_last:_ =
  match block with
  | Cmarkit.Block.Paragraph _ -> Some (doors_custom parent "custom paragraph")
  | _ -> None

let doors_render_code ~parent ~language:_ ~content:_ =
  doors_custom parent "custom code"

let doors_render_node_label () =
  Test_harness.reaches "render_node"
    ~base:(Mosaic.markdown ~size:doors_size doors_md)
    ~element:
      (Mosaic.markdown ~size:doors_size ~render_node:doors_render_node doors_md)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_md ~render_node:doors_render_node
              ())))

let doors_render_code_label () =
  Test_harness.reaches "render_code"
    ~base:(Mosaic.markdown ~size:doors_size doors_fenced)
    ~element:
      (Mosaic.markdown ~size:doors_size ~render_code:doors_render_code
         doors_fenced)
    ~create_base:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced ())))
    ~create:(fun parent ->
      Markdown.node
        (Markdown.create ~parent ~layout_style:doors_style
           (Markdown.Props.make ~content:doors_fenced
              ~render_code:doors_render_code ())))

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
          test "set_highlighter re-renders existing block"
            set_highlighter_rerenders_existing_block;
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
          test "highlighter receives fence language"
            highlighter_receives_fence_language;
          test "render_code takes precedence over highlighter"
            render_code_takes_precedence_over_highlighter;
          test "view refreshes do not highlight again"
            view_refreshes_do_not_highlight_again;
          test "style change restyles without highlighting"
            style_change_restyles_without_highlighting;
          test "element style rebuilt renders nothing"
            element_style_rebuilt_renders_nothing;
          test "element style change re-renders" element_style_change_rerenders;
          test "element styles are values" element_style_values;
          test "pp does not crash" pp_does_not_crash;
        ];
      group "Incremental updates"
        [
          test "unchanged apply_props does not re-render"
            apply_props_same_content_no_rerender;
          test "streaming append reuses prefix blocks"
            streaming_append_reuses_prefix_blocks;
        ];
      group "Both doors"
        [
          test "content" doors_content;
          test "style (the element's md_style)" doors_style_label;
          test "conceal" doors_conceal;
          test "streaming" doors_streaming;
          test "selectable" doors_selectable;
          test "selection_bg" doors_selection_bg;
          test "selection_fg" doors_selection_fg;
          test "render_node" doors_render_node_label;
          test "render_code" doors_render_code_label;
          test "highlighter" doors_highlighter;
          test "syntax_style" doors_syntax_style;
        ];
    ]
