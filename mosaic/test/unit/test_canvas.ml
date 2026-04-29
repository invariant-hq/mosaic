open Windtrap
open Mosaic_ui
open Test_harness

(* ── Props ── *)

let props_defaults () =
  let p = Canvas.Props.default in
  is_true ~msg:"equal to make()" (Canvas.Props.equal p (Canvas.Props.make ()))

let props_equal_identical () =
  let a = Canvas.Props.make () in
  let b = Canvas.Props.make () in
  is_true ~msg:"equal" (Canvas.Props.equal a b)

let props_detects_respect_alpha_diff () =
  let a = Canvas.Props.make ~respect_alpha:true () in
  let b = Canvas.Props.make () in
  is_false ~msg:"different" (Canvas.Props.equal a b)

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
  equal ~msg:"width" int 1 (Grid.width g);
  equal ~msg:"height" int 1 (Grid.height g)

let create_respect_alpha_default_false () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  is_false ~msg:"default false" (Canvas.respect_alpha canvas)

let create_respect_alpha_true () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root ~respect_alpha:true () in
  is_true ~msg:"set true" (Canvas.respect_alpha canvas);
  is_true ~msg:"grid respect_alpha" (Grid.respect_alpha (Canvas.grid canvas))

(* ── Auto-resize ── *)

let auto_resize_on_render () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  (* First layout *)
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"w1" int 10 (Grid.width g);
  equal ~msg:"h1" int 5 (Grid.height g);
  (* Resize layout *)
  layout_node node ~x:0 ~y:0 ~width:20 ~height:10;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"w2" int 20 (Grid.width g);
  equal ~msg:"h2" int 10 (Grid.height g)

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
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"first resize" int 1 !resize_count;
  (* Same size — no callback *)
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"no resize" int 1 !resize_count;
  (* New size *)
  layout_node node ~x:0 ~y:0 ~width:20 ~height:10;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"second resize" int 2 !resize_count

let no_render_for_zero_dimensions () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:0 ~height:0;
  Renderable.Private.render node parent_grid ~delta:0.;
  (* Grid should remain 1x1 — no resize for zero dims *)
  equal ~msg:"w" int 1 (Grid.width (Canvas.grid canvas));
  equal ~msg:"h" int 1 (Grid.height (Canvas.grid canvas))

(* ── Clear ── *)

let clear_schedules_render () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.clear canvas;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Properties ── *)

let set_respect_alpha_updates_grid () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  is_false ~msg:"initially false" (Grid.respect_alpha (Canvas.grid canvas));
  Canvas.set_respect_alpha canvas true;
  is_true ~msg:"set true" (Grid.respect_alpha (Canvas.grid canvas));
  is_true ~msg:"prop true" (Canvas.respect_alpha canvas)

let set_respect_alpha_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.set_respect_alpha canvas false;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_respect_alpha_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.set_respect_alpha canvas true;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── apply_props ── *)

let apply_props_updates_respect_alpha () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let props = Canvas.Props.make ~respect_alpha:true () in
  Canvas.apply_props canvas props;
  is_true ~msg:"updated" (Canvas.respect_alpha canvas);
  is_true ~msg:"grid updated" (Grid.respect_alpha (Canvas.grid canvas))

let apply_props_noop_same () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.apply_props canvas Canvas.Props.default;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── request_render ── *)

let request_render_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let before = !(t.schedule_count) in
  Canvas.request_render canvas;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Drawing ── *)

let draw_text_writes_to_grid () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:20 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  Canvas.draw_text canvas ~x:0 ~y:0 ~text:"Hi";
  let g = Canvas.grid canvas in
  let text = Grid.get_text g (Grid.idx g ~x:0 ~y:0) in
  equal ~msg:"first char" string "H" text

let fill_rect_fills_cells () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  Canvas.fill_rect canvas ~x:0 ~y:0 ~width:2 ~height:2 ~color:Ansi.Color.Red;
  let g = Canvas.grid canvas in
  let bg = Grid.get_background g (Grid.idx g ~x:0 ~y:0) in
  is_true ~msg:"has fill color" (Ansi.Color.equal Ansi.Color.Red bg)

let width_height_accessors () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  equal ~msg:"initial w" int 1 (Canvas.width canvas);
  equal ~msg:"initial h" int 1 (Canvas.height canvas);
  layout_node node ~x:0 ~y:0 ~width:15 ~height:8;
  Renderable.Private.render node parent_grid ~delta:0.;
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
  Canvas.set_on_draw canvas (Some (fun _ ~delta:_ -> incr draw_count));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"drawn once" int 1 !draw_count;
  Renderable.Private.render node parent_grid ~delta:0.;
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
       (fun c ~delta:_ ->
         seen_w := Canvas.width c;
         seen_h := Canvas.height c));
  layout_node node ~x:0 ~y:0 ~width:30 ~height:12;
  Renderable.Private.render node parent_grid ~delta:0.;
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
  Canvas.set_on_draw canvas (Some (fun _ ~delta:_ -> order := "draw" :: !order));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"order" (list string) [ "draw"; "resize" ] !order

let on_draw_can_draw () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  Canvas.set_on_draw canvas
    (Some (fun c ~delta:_ -> Canvas.draw_text c ~x:0 ~y:0 ~text:"X"));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"drawn" string "X" (Grid.get_text g (Grid.idx g ~x:0 ~y:0))

let on_draw_forwards_delta () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let seen_delta = ref 0. in
  Canvas.set_on_draw canvas (Some (fun _ ~delta -> seen_delta := delta));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:42.0;
  equal ~msg:"delta" (float 0.) 42.0 !seen_delta

let on_draw_none_clears_callback () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  let draw_count = ref 0 in
  Canvas.set_on_draw canvas (Some (fun _ ~delta:_ -> incr draw_count));
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"fired once" int 1 !draw_count;
  Canvas.set_on_draw canvas None;
  Renderable.Private.render node parent_grid ~delta:0.;
  equal ~msg:"still one" int 1 !draw_count

(* ── set_cell ── *)

let set_cell_writes_glyph () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root () in
  let node = Canvas.node canvas in
  let parent_grid = make_grid ~width:80 ~height:24 () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.render node parent_grid ~delta:0.;
  let cell = Grid.Cell.of_uchar (Uchar.of_char 'Z') in
  Canvas.set_cell canvas ~x:1 ~y:2 ~cell ~fg:Ansi.Color.Red ~bg:Ansi.Color.Blue
    ~attrs:Ansi.Attr.empty ();
  let g = Canvas.grid canvas in
  let idx = Grid.idx g ~x:1 ~y:2 in
  equal ~msg:"text" string "Z" (Grid.get_text g idx);
  is_true ~msg:"bg"
    (Ansi.Color.equal Ansi.Color.Blue (Grid.get_background g idx))

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
  Renderable.Private.render node parent_grid ~delta:0.;
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
  Renderable.Private.render node parent_grid ~delta:0.;
  Canvas.draw_text canvas ~x:0 ~y:0 ~text:"AB";
  (* Re-render without clearing — content should persist *)
  Renderable.Private.render node parent_grid ~delta:0.;
  let g = Canvas.grid canvas in
  equal ~msg:"first char" string "A" (Grid.get_text g (Grid.idx g ~x:0 ~y:0));
  equal ~msg:"second char" string "B" (Grid.get_text g (Grid.idx g ~x:1 ~y:0));
  (* Also verify it was blitted to the parent grid *)
  equal ~msg:"parent first" string "A"
    (Grid.get_text parent_grid (Grid.idx parent_grid ~x:0 ~y:0))

(* ── pp ── *)

let pp_output () =
  let t = make_ctx () in
  let root = make_root t in
  let canvas = Canvas.create ~parent:root ~id:"test-canvas" () in
  let s = Format.asprintf "%a" Canvas.pp canvas in
  is_true ~msg:"contains id" (String.length s > 0);
  is_true ~msg:"has Canvas prefix"
    (String.length s >= 6 && String.sub s 0 6 = "Canvas")

(* ── Runner ── *)

let () =
  run "mosaic.canvas"
    [
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects respect_alpha difference"
            props_detects_respect_alpha_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "grid starts at 1x1" create_grid_starts_at_1x1;
          test "respect_alpha defaults to false"
            create_respect_alpha_default_false;
          test "respect_alpha true propagates" create_respect_alpha_true;
        ];
      group "Auto-resize"
        [
          test "resizes grid on render" auto_resize_on_render;
          test "fires on_resize callback" auto_resize_fires_callback;
          test "on_resize sees new dimensions" on_resize_sees_new_dimensions;
          test "skips zero dimensions" no_render_for_zero_dimensions;
        ];
      group "Clear" [ test "schedules render" clear_schedules_render ];
      group "Properties"
        [
          test "set_respect_alpha updates grid" set_respect_alpha_updates_grid;
          test "set_respect_alpha no-op on same value"
            set_respect_alpha_noop_same_value;
          test "set_respect_alpha schedules render" set_respect_alpha_schedules;
        ];
      group "apply_props"
        [
          test "updates respect_alpha" apply_props_updates_respect_alpha;
          test "no-op on same props" apply_props_noop_same;
        ];
      group "request_render"
        [ test "schedules render" request_render_schedules ];
      group "Drawing"
        [
          test "draw_text writes to grid" draw_text_writes_to_grid;
          test "fill_rect fills cells" fill_rect_fills_cells;
          test "set_cell writes cell" set_cell_writes_glyph;
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
    ]
