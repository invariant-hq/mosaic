open Windtrap
open Mosaic_ui
open Test_harness

(* ── Construction ── *)

let create_attaches () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  match Renderable.parent node with
  | Some p -> equal ~msg:"parent" string (Renderable.id root) (Renderable.id p)
  | None -> fail "expected parent"

let create_grid_starts_at_1x1 () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let g = Canvas.grid canvas in
  equal ~msg:"width" int 1 (Matrix_grid.width g);
  equal ~msg:"height" int 1 (Matrix_grid.height g)

let create_grid_follows_screen_width_method () =
  let t = make_ctx ~width_method:`Wcwidth () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  is_true ~msg:"grid uses the screen's width method"
    (Matrix_grid.width_method (Canvas.grid canvas) = `Wcwidth)

(* ── Auto-resize ── *)

let auto_resize_on_render () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  (* First layout *)
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"w1" int 10 (Matrix_grid.width g);
  equal ~msg:"h1" int 5 (Matrix_grid.height g);
  (* Resize layout *)
  layout_node node ~x:0 ~y:0 ~width:20 ~height:10;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"w2" int 20 (Matrix_grid.width g);
  equal ~msg:"h2" int 10 (Matrix_grid.height g)

let auto_resize_fires_callback () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let resize_count = ref 0 in
  Canvas.set_on_resize canvas (Some (fun _ -> incr resize_count));
  (* First render triggers resize from 1x1 → 10x5 *)
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"first resize" int 1 !resize_count;
  (* Same size — no callback *)
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"no resize" int 1 !resize_count;
  (* New size *)
  layout_node node ~x:0 ~y:0 ~width:20 ~height:10;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"second resize" int 2 !resize_count

let no_render_for_zero_dimensions () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  Renderable.Private.render node parent_grid ~dt:0.;
  (* Grid should remain 1x1 — no resize for zero dims *)
  equal ~msg:"w" int 1 (Matrix_grid.width (Canvas.grid canvas));
  equal ~msg:"h" int 1 (Matrix_grid.height (Canvas.grid canvas))

(* ── Clear ── *)

let clear_schedules_render () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.clear canvas;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* A canvas that clears itself in its draw, as a full redraw does, owes no
   frame: the clear shows in the frame being drawn. *)
let clear_in_draw_schedules_nothing () =
  let r = Renderer.create () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 4.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 2.)
  in
  let canvas = Canvas.create ~parent:(Renderer.root r) ~style () in
  let draws = ref 0 in
  Canvas.set_on_draw canvas
    (Some
       (fun c ~dt:_ ->
         incr draws;
         Canvas.clear c));
  Renderer.render_frame r ~width:10 ~height:4 ~dt:0.;
  equal ~msg:"drawn" int 1 !draws;
  is_false ~msg:"no frame owed" (Renderer.needs_render r);
  Canvas.clear canvas;
  is_true ~msg:"a clear outside the draw owes one" (Renderer.needs_render r)

(* ── Properties ── *)

let request_render_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.request_render canvas;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Drawing ── *)

let draw_text_writes_to_grid () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  Canvas.draw_text canvas ~x:0 ~y:0 ~text:"Hi";
  let g = Canvas.grid canvas in
  let text = Matrix_grid.get_text g (Matrix_grid.idx g ~x:0 ~y:0) in
  equal ~msg:"first char" string "H" text

let fill_rect_fills_cells () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  Canvas.fill_rect canvas ~x:0 ~y:0 ~width:2 ~height:2 ~color:Ansi.Color.red;
  let g = Canvas.grid canvas in
  let bg = Matrix_grid.get_bg g (Matrix_grid.idx g ~x:0 ~y:0) in
  is_true ~msg:"has fill color" (Ansi.Color.equal Ansi.Color.red bg)

let width_height_accessors () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  equal ~msg:"initial w" int 1 (Canvas.width canvas);
  equal ~msg:"initial h" int 1 (Canvas.height canvas);
  layout_node node ~x:0 ~y:0 ~width:15 ~height:8;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"after render w" int 15 (Canvas.width canvas);
  equal ~msg:"after render h" int 8 (Canvas.height canvas)

(* ── on_draw ── *)

let on_draw_fires_during_render () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let draw_count = ref 0 in
  Canvas.set_on_draw canvas (Some (fun _ ~dt:_ -> incr draw_count));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"drawn once" int 1 !draw_count;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"drawn twice" int 2 !draw_count

let on_draw_has_correct_dimensions () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let seen_w = ref 0 in
  let seen_h = ref 0 in
  Canvas.set_on_draw canvas
    (Some
       (fun c ~dt:_ ->
         seen_w := Canvas.width c;
         seen_h := Canvas.height c));
  layout_node node ~x:0 ~y:0 ~width:30 ~height:12;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"width" int 30 !seen_w;
  equal ~msg:"height" int 12 !seen_h

let on_resize_fires_before_on_draw () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let order = ref [] in
  Canvas.set_on_resize canvas (Some (fun _ -> order := "resize" :: !order));
  Canvas.set_on_draw canvas (Some (fun _ ~dt:_ -> order := "draw" :: !order));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"order" (list string) [ "draw"; "resize" ] !order

let on_draw_can_draw () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  Canvas.set_on_draw canvas
    (Some (fun c ~dt:_ -> Canvas.draw_text c ~x:0 ~y:0 ~text:"X"));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"drawn" string "X"
    (Matrix_grid.get_text g (Matrix_grid.idx g ~x:0 ~y:0))

let on_draw_forwards_delta () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let seen_delta = ref 0. in
  Canvas.set_on_draw canvas (Some (fun _ ~dt -> seen_delta := dt));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:42.0;
  equal ~msg:"delta" float_exact 42.0 !seen_delta

let on_draw_none_clears_callback () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let draw_count = ref 0 in
  Canvas.set_on_draw canvas (Some (fun _ ~dt:_ -> incr draw_count));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"fired once" int 1 !draw_count;
  Canvas.set_on_draw canvas None;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"still one" int 1 !draw_count

(* ── set_uchar ── *)

let set_uchar_writes_glyph () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  let cell = Uchar.of_char 'Z' in
  Canvas.set_uchar canvas ~x:1 ~y:2 ~uchar:cell ~fg:Ansi.Color.red
    ~bg:Ansi.Color.blue ~attrs:Ansi.Attr.empty ();
  let g = Canvas.grid canvas in
  let idx = Matrix_grid.idx g ~x:1 ~y:2 in
  equal ~msg:"text" string "Z" (Matrix_grid.get_text g idx);
  is_true ~msg:"bg"
    (Ansi.Color.equal Ansi.Color.blue (Matrix_grid.get_bg g idx))

(* ── on_resize dimensions ── *)

let on_resize_sees_new_dimensions () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let seen_w = ref 0 in
  let seen_h = ref 0 in
  Canvas.set_on_resize canvas
    (Some
       (fun c ->
         seen_w := Canvas.width c;
         seen_h := Canvas.height c));
  layout_node node ~x:0 ~y:0 ~width:25 ~height:15;
  Renderable.Private.render node parent_grid ~dt:0.;
  equal ~msg:"width" int 25 !seen_w;
  equal ~msg:"height" int 15 !seen_h

(* ── Persistence ── *)

let content_persists_across_frames () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~dt:0.;
  Canvas.draw_text canvas ~x:0 ~y:0 ~text:"AB";
  (* Re-render without clearing — content should persist *)
  Renderable.Private.render node parent_grid ~dt:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"first char" string "A"
    (Matrix_grid.get_text g (Matrix_grid.idx g ~x:0 ~y:0));
  equal ~msg:"second char" string "B"
    (Matrix_grid.get_text g (Matrix_grid.idx g ~x:1 ~y:0));
  (* Also verify it was blitted to the parent grid *)
  equal ~msg:"parent first" string "A"
    (Matrix_grid.get_text parent_grid (Matrix_grid.idx parent_grid ~x:0 ~y:0))

(* ── pp ── *)

let pp_output () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root ~id:"test-canvas" () in
  let s = Format.asprintf "%a" Canvas.pp canvas in
  contains ~msg:"contains id" ~sub:"test-canvas" s;
  starts_with ~affix:"Canvas" s

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.canvas}) or to
   {!Canvas.Props.make} for {!Canvas.create}, reaches the live canvas
   ({!Test_harness.reaches}). A canvas has no labels of its own: its [draw]
   is the callback {!Canvas.set_on_draw} sets. *)

let doors_size = Mosaic.size ~width:6 ~height:2
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_nothing _ ~dt:_ = ()
let doors_draw c ~dt:_ = Canvas.draw_text c ~x:0 ~y:0 ~text:"drawn"

let doors_drawing canvas draw =
  Canvas.set_on_draw canvas (Some draw);
  Canvas.node canvas

let doors_on_draw () =
  Test_harness.reaches "on_draw (the element's draw)"
    ~base:(Mosaic.canvas ~size:doors_size doors_nothing)
    ~element:(Mosaic.canvas ~size:doors_size doors_draw)
    ~create_base:(fun parent ->
      doors_drawing (Canvas.create ~parent ~style:doors_style ()) doors_nothing)
    ~create:(fun parent ->
      doors_drawing (Canvas.create ~parent ~style:doors_style ()) doors_draw)

let () =
  run "mosaic.canvas"
    [
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "grid starts at 1x1" create_grid_starts_at_1x1;
          test "grid follows the screen width method"
            create_grid_follows_screen_width_method;
        ];
      group "Auto-resize"
        [
          test "resizes grid on render" auto_resize_on_render;
          test "fires on_resize callback" auto_resize_fires_callback;
          test "on_resize sees new dimensions" on_resize_sees_new_dimensions;
          test "skips zero dimensions" no_render_for_zero_dimensions;
        ];
      group "Clear"
        [
          test "schedules render" clear_schedules_render;
          test "in the canvas's draw schedules nothing"
            clear_in_draw_schedules_nothing;
        ];
      group "request_render"
        [ test "schedules render" request_render_schedules ];
      group "Drawing"
        [
          test "draw_text writes to grid" draw_text_writes_to_grid;
          test "fill_rect fills cells" fill_rect_fills_cells;
          test "set_uchar writes cell" set_uchar_writes_glyph;
          test "width/height accessors" width_height_accessors;
        ];
      group "on_draw"
        [
          test "fires during render" on_draw_fires_during_render;
          test "has correct dimensions" on_draw_has_correct_dimensions;
          test "forwards delta" on_draw_forwards_delta;
          test "None clears callback" on_draw_none_clears_callback;
          test "on_resize fires before on_draw" on_resize_fires_before_on_draw;
          test "can draw inside callback" on_draw_can_draw;
        ];
      group "Persistence"
        [ test "content persists across frames" content_persists_across_frames ];
      group "pp" [ test "produces output" pp_output ];
      group "Both doors" [ test "on_draw (the element's draw)" doors_on_draw ];
    ]
