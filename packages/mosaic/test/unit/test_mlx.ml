open Windtrap
open Mosaic_ui

let make () =
  let renderer = Renderer.create () in
  let container = Renderer.root renderer in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Mosaic.size ~width:20 ~height:8)
  in
  Renderable.set_style container style;
  (renderer, Mosaic.Reconciler.create ~container ~deliver:ignore)

let render renderer reconciler view dispatch =
  Mosaic.Reconciler.render reconciler ~viewport_width:20
    (Mosaic.map dispatch view);
  Renderer.render_frame renderer ~width:20 ~height:8 ~dt:0.

let children_and_editor_state () =
  let renderer, reconciler = make () in
  let state = ref (Mosaic.Editor.of_string "a") in
  let view () =
    Mosaic_mlx.box ~flex_direction:Mosaic.Flex_direction.Column
      ~children:
        [
          Mosaic_mlx.text ~children:[ "head"; "er" ] ();
          Mosaic_mlx.input ~focused:true ~state:!state
            ~on_change:(fun next -> next)
            ~children:[] ();
        ]
      ()
  in
  let accept next = state := next in
  render renderer reconciler (view ()) accept;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  equal ~msg:"text children concatenate" string "h"
    (Matrix_grid.get_text grid 0);
  equal ~msg:"last text child follows first" string "r"
    (Matrix_grid.get_text grid 5);
  let node = Option.get (Mosaic.Reconciler.focused reconciler) in
  ignore (Renderer.focus renderer node : bool);
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'b') : Event.key);
  equal ~msg:"editor callback proposes model state" string "ab"
    (Mosaic.Editor.text !state);
  render renderer reconciler (view ()) accept;
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'c') : Event.key);
  equal ~msg:"next edit uses accepted declaration" string "abc"
    (Mosaic.Editor.text !state)

let scroll_children_and_state () =
  let renderer, reconciler = make () in
  let scroll = ref (Mosaic.Scroll.at 0) in
  let view () =
    Mosaic_mlx.scroll_box
      ~size:(Mosaic.size ~width:10 ~height:4)
      ~scroll:!scroll
      ~on_scroll:(fun next -> next)
      ~show_scrollbars:false
      ~children:
        [
          Mosaic_mlx.box ~flex_direction:Mosaic.Flex_direction.Column
            ~flex_shrink:0.
            ~children:
              (List.init 20 (fun i ->
                   Mosaic_mlx.text ~children:[ string_of_int i ] ()))
            ();
        ]
      ()
  in
  let frame () =
    render renderer reconciler (view ()) (fun next -> scroll := next);
    ignore (Renderer.render ~full:true renderer : string)
  in
  let wheel () =
    let mouse = Test_harness.mouse_report ~x:1 ~y:1 (Scroll `Down) in
    ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse)
  in
  let position () =
    snd
      (Mosaic.Scroll.resolve !scroll ~width:10 ~height:4 ~content_width:10
         ~content_height:20)
  in
  frame ();
  wheel ();
  let first = position () in
  is_true ~msg:"scroll callback proposes a position" (first > 0);
  frame ();
  wheel ();
  is_true ~msg:"next wheel starts from accepted model position"
    (position () > first)

let line_number_requires_one_child () =
  raises_match ~msg:"line numbers require exactly one child"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Mosaic_mlx.line_number ~children:[] ()))

let rich_children_and_high_level_text_share_the_widget () =
  let renderer, reconciler = make () in
  let fragments = Mosaic.Text.Fragment.[ text "a"; bold [ text "b" ] ] in
  let direct =
    Mosaic.rich_text ~key:"text"
      ~text_style:(Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.red ())
      fragments
  in
  render renderer reconciler direct ignore;
  let node = List.hd (Renderable.children (Renderer.root renderer)) in
  let view =
    Mosaic_mlx.rich_text ~key:"text"
      ~text_style:(Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.blue ())
      ~children:fragments ()
  in
  render renderer reconciler view ignore;
  is_true ~msg:"MLX preserves the same text node"
    (node == List.hd (Renderable.children (Renderer.root renderer)));
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  equal string "b" (Matrix_grid.get_text grid 1);
  is_true ~msg:"MLX keeps nested bold"
    (Mosaic.Ansi.Attr.mem Mosaic.Ansi.Attr.Bold
       (Matrix_grid.get_style grid 1).attrs);
  is_true ~msg:"MLX inherits base colour"
    (Mosaic.Ansi.Color.equal Mosaic.Ansi.Color.blue (Matrix_grid.get_fg grid 1));
  render renderer reconciler (Mosaic_mlx.rich_text ~key:"text" ()) ignore;
  is_true ~msg:"empty rich content retains text node"
    (node == List.hd (Renderable.children (Renderer.root renderer)))

let annotated_textarea_flows_through_mosaic_and_mlx () =
  let renderer, reconciler = make () in
  let content = "let x" in
  let state = Mosaic.Editor.of_string content in
  let highlights =
    Mosaic.Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ]
  in
  let style =
    Mosaic.Syntax_style.make ~base:Mosaic.Ansi.Style.default
      [ ("keyword", Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.red ()) ]
  in
  let annotations = Mosaic.Syntax_highlight.to_annotations ~style highlights in
  render renderer reconciler
    (Mosaic.textarea ~state ~annotations ~show_cursor:false ~on_change:ignore ())
    ignore;
  let node = List.hd (Renderable.children (Renderer.root renderer)) in
  render renderer reconciler
    (Mosaic_mlx.textarea ~state ~annotations ~show_cursor:false
       ~on_change:ignore ())
    ignore;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  equal string "l" (Matrix_grid.get_text grid 0);
  is_true ~msg:"annotations reach the textarea through MLX"
    (Mosaic.Ansi.Color.equal Mosaic.Ansi.Color.red (Matrix_grid.get_fg grid 0));
  is_true ~msg:"annotation wrappers reuse the accepted widget"
    (node == List.hd (Renderable.children (Renderer.root renderer)));
  raises_match ~msg:"stale annotations rejected before rendering"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Mosaic_mlx.textarea ~annotations
           ~state:(Mosaic.Editor.of_string "new content")
           ~on_change:ignore ()));
  equal ~msg:"rejected declaration leaves rendered content intact" string "l"
    (Matrix_grid.get_text
       (Matrix_screen.next_grid (Renderer.screen renderer))
       0)

(* The MLX box takes the labels {!Mosaic.box} takes, [inert] among them. *)
let box_takes_inert () =
  Test_harness.reaches "inert"
    ~observe:(fun _ node -> string_of_bool (Renderable.inert node))
    ~base:(Mosaic_mlx.box ())
    ~element:(Mosaic_mlx.box ~inert:true ())
    ~create_base:(fun parent ->
      Box.node (Box.create ~parent (Box.Props.make ())))
    ~create:(fun parent ->
      Box.node (Box.create ~parent (Box.Props.make ~inert:true ())))

(* The MLX code takes the labels {!Mosaic.code} takes, [truncate] among
   them. *)
let code_takes_truncate () =
  let size = Mosaic.size ~width:8 ~height:1 in
  let style = Toffee.Style.make ~size () in
  Test_harness.reaches "truncate"
    ~base:(Mosaic_mlx.code ~size ~children:[ "hello world" ] ())
    ~element:
      (Mosaic_mlx.code ~size ~truncate:true ~children:[ "hello world" ] ())
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style (Code.Props.make ~content:"hello world" ())))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style
           (Code.Props.make ~content:"hello world" ~truncate:true ())))

(* The MLX line number takes the labels {!Mosaic.line_number} takes,
   [line_numbers] among them. *)
let line_number_takes_line_numbers () =
  let size = Mosaic.size ~width:8 ~height:2 in
  let style = Toffee.Style.make ~size () in
  let gutter g =
    ignore
      (Text.create ~parent:(Line_number.node g)
         (Text.Props.make ~content:"a\nb" ())
        : Text.t);
    Line_number.node g
  in
  Test_harness.reaches "line_numbers"
    ~base:(Mosaic_mlx.line_number ~size ~children:[ Mosaic.text "a\nb" ] ())
    ~element:
      (Mosaic_mlx.line_number ~size
         ~line_numbers:[ (0, 42) ]
         ~children:[ Mosaic.text "a\nb" ]
         ())
    ~create_base:(fun parent ->
      gutter (Line_number.create ~parent ~style (Line_number.Props.make ())))
    ~create:(fun parent ->
      gutter
        (Line_number.create ~parent ~style
           (Line_number.Props.make ~line_numbers:[ (0, 42) ] ())))

let () =
  run "mosaic.mlx"
    [
      test "annotated textarea through Mosaic and MLX"
        annotated_textarea_flows_through_mosaic_and_mlx;
      test "rich children and high-level text share the widget"
        rich_children_and_high_level_text_share_the_widget;
      test "children and model-owned editor" children_and_editor_state;
      test "children and model-owned scroll" scroll_children_and_state;
      test "line-number children contract" line_number_requires_one_child;
      test "box takes inert" box_takes_inert;
      test "code takes truncate" code_takes_truncate;
      test "line_number takes line_numbers" line_number_takes_line_numbers;
    ]
