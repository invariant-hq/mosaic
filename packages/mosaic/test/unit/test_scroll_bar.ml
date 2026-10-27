open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_scroll_bar ?orientation ?show_arrows ?track_color ?thumb_color
    ?arrow_fg ?arrow_bg ?on_change () =
  let t = make_ctx () in
  let root = make_root t in
  let bar =
    Scroll_bar.create ~parent:root ?on_change
      (Scroll_bar.Props.make ?orientation ?show_arrows ?track_color ?thumb_color
         ?arrow_fg ?arrow_bg ())
  in
  Scroll_bar.set_on_change bar
    (Some
       (fun value ->
         Scroll_bar.set_scroll_position bar value;
         Option.iter (fun f -> f value) on_change));
  (t, bar)

(* ── Props ── *)

let props_defaults () =
  let p = Scroll_bar.Props.default in
  is_true ~msg:"equal to make()"
    (Scroll_bar.Props.equal p (Scroll_bar.Props.make ()))

let props_equal_identical () =
  let a = Scroll_bar.Props.make () in
  let b = Scroll_bar.Props.make () in
  is_true ~msg:"equal" (Scroll_bar.Props.equal a b)

let props_detects_orientation_diff () =
  let a = Scroll_bar.Props.make ~orientation:`Horizontal () in
  let b = Scroll_bar.Props.make ~orientation:`Vertical () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_show_arrows_diff () =
  let a = Scroll_bar.Props.make ~show_arrows:true () in
  let b = Scroll_bar.Props.make ~show_arrows:false () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_track_color_diff () =
  let a = Scroll_bar.Props.make ~track_color:Ansi.Color.red () in
  let b = Scroll_bar.Props.make ~track_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

let props_detects_thumb_color_diff () =
  let a = Scroll_bar.Props.make ~thumb_color:Ansi.Color.red () in
  let b = Scroll_bar.Props.make ~thumb_color:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_bar.Props.equal a b)

(* ── Construction ── *)

let create_attaches () =
  let _t, bar = make_scroll_bar () in
  let node = Scroll_bar.node bar in
  match Renderable.parent node with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_position_zero () =
  let _t, bar = make_scroll_bar () in
  equal ~msg:"pos = 0" int 0 (Scroll_bar.scroll_position bar)

let create_default_sizes_zero () =
  let _t, bar = make_scroll_bar () in
  equal ~msg:"scroll_size = 0" int 0 (Scroll_bar.scroll_size bar);
  equal ~msg:"viewport_size = 0" int 0 (Scroll_bar.viewport_size bar)

(* ── Position Clamping ── *)

let set_position_clamps_to_range () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 50;
  equal ~msg:"pos = 50" int 50 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_position bar 200;
  equal ~msg:"clamped to 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_position bar (-10);
  equal ~msg:"clamped to 0" int 0 (Scroll_bar.scroll_position bar)

let set_position_fires_on_change () =
  let changed = ref None in
  let _t, bar = make_scroll_bar ~on_change:(fun v -> changed := Some v) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_scroll_position bar 42;
  equal ~msg:"accepted setter is silent" (option int) None !changed

let set_position_noop_same_value () =
  let count = ref 0 in
  let _t, bar = make_scroll_bar ~on_change:(fun _ -> incr count) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 10;
  equal ~msg:"setter is silent" int 0 !count;
  Scroll_bar.set_scroll_position bar 10;
  equal ~msg:"setter remains silent" int 0 !count

(* ── Scroll Size / Viewport Size ── *)

let set_scroll_size_reclamps_position () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 80;
  equal ~msg:"pos = 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.set_scroll_size bar 50;
  equal ~msg:"clamped to 30" int 30 (Scroll_bar.scroll_position bar)

let set_viewport_size_reclamps_position () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 80;
  Scroll_bar.set_viewport_size bar 50;
  equal ~msg:"clamped to 50" int 50 (Scroll_bar.scroll_position bar)

(* ── scroll_by ── *)

let scroll_by_absolute () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 10. ~unit:`Absolute;
  equal ~msg:"pos = 10" int 10 (Scroll_bar.scroll_position bar)

let scroll_by_viewport () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 0.5 ~unit:`Viewport;
  equal ~msg:"pos = 10" int 10 (Scroll_bar.scroll_position bar)

let scroll_by_content () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 0.5 ~unit:`Content;
  equal ~msg:"pos = 50" int 50 (Scroll_bar.scroll_position bar)

let scroll_by_step_default () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 5. ~unit:`Step;
  equal ~msg:"pos = 5" int 5 (Scroll_bar.scroll_position bar)

let scroll_by_step_custom () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_step bar (Some 3);
  Scroll_bar.scroll_by bar 2. ~unit:`Step;
  equal ~msg:"pos = 6" int 6 (Scroll_bar.scroll_position bar)

let scroll_by_clamps () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 200. ~unit:`Absolute;
  equal ~msg:"clamped to 80" int 80 (Scroll_bar.scroll_position bar);
  Scroll_bar.scroll_by bar (-200.) ~unit:`Absolute;
  equal ~msg:"clamped to 0" int 0 (Scroll_bar.scroll_position bar)

(* ── Auto-visibility ── *)

let auto_visibility_hidden_when_no_overflow () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  is_false ~msg:"hidden" (Renderable.visible (Scroll_bar.node bar))

let auto_visibility_visible_when_overflow () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  is_true ~msg:"visible" (Renderable.visible (Scroll_bar.node bar))

let manual_visibility_overrides () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_visible_override bar true;
  is_true ~msg:"forced visible" (Renderable.visible (Scroll_bar.node bar))

let reset_visibility_restores_auto () =
  let _t, bar = make_scroll_bar () in
  Scroll_bar.set_scroll_size bar 10;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_visible_override bar true;
  is_true ~msg:"forced visible" (Renderable.visible (Scroll_bar.node bar));
  Scroll_bar.reset_visibility_control bar;
  is_false ~msg:"auto hidden" (Renderable.visible (Scroll_bar.node bar))

(* ── Show Arrows ── *)

let show_arrows_default_false () =
  let _t, bar = make_scroll_bar () in
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  (* With show_arrows=false, only the slider should be visible *)
  equal ~msg:"1 visible child (slider)" int 1 visible_count

let show_arrows_true () =
  let _t, bar = make_scroll_bar ~show_arrows:true () in
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  (* With show_arrows=true, slider + 2 arrows = 3 *)
  equal ~msg:"3 visible children" int 3 visible_count

let set_show_arrows_toggles () =
  let _t, bar = make_scroll_bar () in
  let count_visible () =
    let children = Renderable.Private.children_z (Scroll_bar.node bar) in
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  equal ~msg:"1 initially" int 1 (count_visible ());
  Scroll_bar.set_show_arrows bar true;
  equal ~msg:"3 after show" int 3 (count_visible ());
  Scroll_bar.set_show_arrows bar false;
  equal ~msg:"1 after hide" int 1 (count_visible ())

(* ── apply_props ── *)

let apply_props_updates_appearance () =
  let _t, bar = make_scroll_bar () in
  let props =
    Scroll_bar.Props.make ~show_arrows:true ~track_color:Ansi.Color.red
      ~thumb_color:Ansi.Color.blue ()
  in
  Scroll_bar.apply_props bar props;
  let children = Renderable.Private.children_z (Scroll_bar.node bar) in
  let visible_count =
    Array.fold_left
      (fun n c -> if Renderable.visible c then n + 1 else n)
      0 children
  in
  equal ~msg:"arrows visible" int 3 visible_count

let flex_direction_of bar =
  Toffee.Style.flex_direction (Renderable.style (Scroll_bar.node bar))

let apply_props_updates_orientation () =
  let _t, bar = make_scroll_bar () in
  is_true ~msg:"vertical by default"
    (flex_direction_of bar = Toffee.Style.Flex_direction.Column);
  Scroll_bar.apply_props bar (Scroll_bar.Props.make ~orientation:`Horizontal ());
  is_true ~msg:"row after apply"
    (flex_direction_of bar = Toffee.Style.Flex_direction.Row);
  let size = Toffee.Style.size (Renderable.style (Scroll_bar.node bar)) in
  is_true ~msg:"height becomes the thickness"
    (Toffee.Style.Dimension.equal size.height
       (Toffee.Style.Dimension.length 1.));
  is_true ~msg:"width becomes the length"
    (Toffee.Style.Dimension.equal size.width
       (Toffee.Style.Dimension.percent 1.0))

(* ── set_on_change ── *)

let set_on_change_replaces_callback () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, bar = make_scroll_bar ~on_change:(fun _ -> incr first_count) () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.scroll_by bar 10. ~unit:`Absolute;
  equal ~msg:"first fired" int 1 !first_count;
  Scroll_bar.set_on_change bar (Some (fun _ -> incr second_count));
  Scroll_bar.scroll_by bar 10. ~unit:`Absolute;
  equal ~msg:"first not again" int 1 !first_count;
  equal ~msg:"second fired" int 1 !second_count

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, bar = make_scroll_bar () in
  let s = Format.asprintf "%a" Scroll_bar.pp bar in
  satisfies ~claim:"non-empty pp output" string
    (fun out -> String.length out > 0)
    s

let pp_contains_scroll_bar () =
  let _t, bar = make_scroll_bar () in
  let s = Format.asprintf "%a" Scroll_bar.pp bar in
  is_true ~msg:"has ScrollBar prefix"
    (String.length s >= 9 && String.sub s 0 9 = "ScrollBar")

(* ── Arrow repeat ── *)

(* Holding an arrow scrolls once on the press, again after half a second,
   then every fifth of a second; frame deltas are seconds. *)
let arrow_hold_repeats_in_seconds () =
  let renderer = Renderer.create () in
  let bar = ref None in
  let style =
    Toffee.Style.make
      ~size:
        (Toffee.Geometry.Size.make
           (Toffee.Style.Dimension.length 1.)
           (Toffee.Style.Dimension.length 10.))
      ()
  in
  let b =
    Scroll_bar.create ~parent:(Renderer.root renderer) ~style
      ~on_change:(fun v ->
        Option.iter (fun b -> Scroll_bar.set_scroll_position b v) !bar)
      (Scroll_bar.Props.make ~show_arrows:true ~scroll_size:100
         ~viewport_size:10 ())
  in
  bar := Some b;
  let frame dt = Renderer.render_frame renderer ~width:1 ~height:10 ~dt in
  frame 0.;
  ignore (Renderer.render ~full:true renderer : string);
  ignore
    (dispatch_mouse renderer (mouse_report ~x:0 ~y:9 (Press Left))
      : Event.mouse);
  equal ~msg:"the press scrolls half a viewport" int 5
    (Scroll_bar.scroll_position b);
  frame 0.25;
  equal ~msg:"no repeat within half a second" int 5
    (Scroll_bar.scroll_position b);
  frame 0.25;
  equal ~msg:"the first repeat after half a second" int 10
    (Scroll_bar.scroll_position b);
  frame 0.125;
  equal ~msg:"no repeat within a fifth of a second" int 10
    (Scroll_bar.scroll_position b);
  frame 0.125;
  equal ~msg:"the next repeat a fifth of a second later" int 12
    (Scroll_bar.scroll_position b)

(* ── Runner ── *)

let declared_state_and_rejected_proposals () =
  let t = make_ctx () in
  let proposals = ref [] in
  let bar =
    Scroll_bar.create ~parent:(make_root t)
      ~on_change:(fun next -> proposals := next :: !proposals)
      (Scroll_bar.Props.make ~position:10 ~scroll_size:100 ~viewport_size:20 ())
  in
  equal ~msg:"constructor accepts position" int 10
    (Scroll_bar.scroll_position bar);
  Scroll_bar.scroll_by bar 1. ~unit:`Viewport;
  Scroll_bar.scroll_by bar 1. ~unit:`Viewport;
  equal ~msg:"rejected proposals use accepted position" (list int) [ 30; 30 ]
    !proposals;
  equal ~msg:"rejection preserves position" int 10
    (Scroll_bar.scroll_position bar);
  Scroll_bar.apply_props bar
    (Scroll_bar.Props.make ~position:30 ~scroll_size:50 ~viewport_size:20 ());
  equal ~msg:"declaration accepts position and range" int 30
    (Scroll_bar.scroll_position bar);
  Scroll_bar.apply_props bar
    (Scroll_bar.Props.make ~position:30 ~scroll_size:25 ~viewport_size:20 ());
  equal ~msg:"new range clamps declared position" int 5
    (Scroll_bar.scroll_position bar);
  equal ~msg:"accepting declaration emits no proposal" (list int) [ 30; 30 ]
    !proposals

let apply_style_preserves_scroll_state () =
  let ctx, bar = make_scroll_bar ~orientation:`Horizontal () in
  Scroll_bar.set_scroll_size bar 100;
  Scroll_bar.set_viewport_size bar 20;
  Scroll_bar.set_scroll_position bar 35;
  Scroll_bar.set_visible_override bar false;
  let proposals = ref 0 in
  Scroll_bar.set_on_change bar (Some (fun _ -> incr proposals));
  let style =
    Scroll_bar.style ~show_arrows:true ~thumb_color:Ansi.Color.cyan ()
  in
  Scroll_bar.apply_style bar style;
  equal ~msg:"range retained" int 100 (Scroll_bar.scroll_size bar);
  equal ~msg:"viewport retained" int 20 (Scroll_bar.viewport_size bar);
  equal ~msg:"position retained" int 35 (Scroll_bar.scroll_position bar);
  is_true ~msg:"orientation retained"
    (flex_direction_of bar = Toffee.Style.Flex_direction.Row);
  is_false ~msg:"visibility policy retained"
    (Renderable.visible (Scroll_bar.node bar));
  equal ~msg:"appearance never proposes scrolling" int 0 !proposals;
  let before = !(ctx.schedule_count) in
  Scroll_bar.apply_style bar style;
  equal ~msg:"same appearance schedules nothing" int before
    !(ctx.schedule_count)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.scroll_bar}) or to
   {!Scroll_bar.Props.make} for {!Scroll_bar.create}, reaches the live scroll
   bar ({!Test_harness.reaches}). The element's [state] is the bar's
   [position]. *)

let doors_size = Mosaic.size ~width:1 ~height:8
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red

let doors_position () =
  Test_harness.reaches "position (the element's state)"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:60 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~position:60 ~scroll_size:100
              ~viewport_size:10 ())))

let doors_scroll_size () =
  Test_harness.reaches "scroll_size"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:20
         ~viewport_size:10 ~on_change:ignore ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:20 ~viewport_size:10 ())))

let doors_viewport_size () =
  Test_harness.reaches "viewport_size"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:50 ~on_change:ignore ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:50 ())))

let doors_orientation () =
  Test_harness.reaches "orientation"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~orientation:`Horizontal ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~orientation:`Horizontal ())))

let doors_show_arrows () =
  Test_harness.reaches "show_arrows"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~show_arrows:true ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~show_arrows:true ())))

let doors_track_color () =
  Test_harness.reaches "track_color"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~track_color:doors_red ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~track_color:doors_red ())))

let doors_thumb_color () =
  Test_harness.reaches "thumb_color"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~thumb_color:doors_red ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10 ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~thumb_color:doors_red ())))

let doors_arrow_fg () =
  Test_harness.reaches "arrow_fg"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~show_arrows:true ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~show_arrows:true
         ~arrow_fg:doors_red ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~show_arrows:true ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~show_arrows:true ~arrow_fg:doors_red ())))

let doors_arrow_bg () =
  Test_harness.reaches "arrow_bg"
    ~base:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~show_arrows:true ())
    ~element:
      (Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
         ~viewport_size:10 ~on_change:ignore ~show_arrows:true
         ~arrow_bg:doors_red ())
    ~create_base:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~show_arrows:true ())))
    ~create:(fun parent ->
      Scroll_bar.node
        (Scroll_bar.create ~parent ~style:doors_style
           (Scroll_bar.Props.make ~scroll_size:100 ~viewport_size:10
              ~show_arrows:true ~arrow_bg:doors_red ())))

(* ── Paint ── *)

(* With no color label, the track is a line in the [Border] role and the
   arrows the theme's glyphs. *)
let paint_glyphs () =
  let view =
    Mosaic.scroll_bar ~size:doors_size ~state:0 ~scroll_size:100
      ~viewport_size:10 ~show_arrows:true ~on_change:ignore ()
  in
  let r = paint (Mosaic.themed (Theme.with_glyphs Glyphs.ascii) view) in
  equal ~msg:"the up arrow" string "^" (cell_text r ~x:0 ~y:0);
  equal ~msg:"the down arrow" string "v" (cell_text r ~x:0 ~y:7);
  is_true ~msg:"the track's line" (includes (at `Plain r) "\u{2502}")

let () =
  run "mosaic.scroll_bar"
    [
      group "Paint" [ test "track and arrows" paint_glyphs ];
      test "declared state and rejected proposals"
        declared_state_and_rejected_proposals;
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects orientation diff" props_detects_orientation_diff;
          test "detects show_arrows diff" props_detects_show_arrows_diff;
          test "detects track_color diff" props_detects_track_color_diff;
          test "detects thumb_color diff" props_detects_thumb_color_diff;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "default position is 0" create_default_position_zero;
          test "default sizes are 0" create_default_sizes_zero;
        ];
      group "Position clamping"
        [
          test "clamps to range" set_position_clamps_to_range;
          test "fires on_change" set_position_fires_on_change;
          test "no-op same value" set_position_noop_same_value;
        ];
      group "Scroll size / viewport size"
        [
          test "set_scroll_size reclamps" set_scroll_size_reclamps_position;
          test "set_viewport_size reclamps" set_viewport_size_reclamps_position;
        ];
      group "scroll_by"
        [
          test "absolute" scroll_by_absolute;
          test "viewport" scroll_by_viewport;
          test "content" scroll_by_content;
          test "step default" scroll_by_step_default;
          test "step custom" scroll_by_step_custom;
          test "clamps" scroll_by_clamps;
        ];
      group "Auto-visibility"
        [
          test "hidden when no overflow" auto_visibility_hidden_when_no_overflow;
          test "visible when overflow" auto_visibility_visible_when_overflow;
          test "manual override" manual_visibility_overrides;
          test "reset restores auto" reset_visibility_restores_auto;
        ];
      group "Show arrows"
        [
          test "default false" show_arrows_default_false;
          test "true shows 3 children" show_arrows_true;
          test "toggle" set_show_arrows_toggles;
        ];
      group "apply_props"
        [
          test "updates appearance" apply_props_updates_appearance;
          test "appearance preserves scroll state"
            apply_style_preserves_scroll_state;
          test "updates orientation" apply_props_updates_orientation;
        ];
      group "Arrow repeat"
        [ test "holding repeats in seconds" arrow_hold_repeats_in_seconds ];
      group "set_on_change"
        [ test "replaces callback" set_on_change_replaces_callback ];
      group "Pretty-printing"
        [
          test "produces output" pp_produces_output;
          test "contains ScrollBar" pp_contains_scroll_bar;
        ];
      group "Both doors"
        [
          test "position (the element's state)" doors_position;
          test "scroll_size" doors_scroll_size;
          test "viewport_size" doors_viewport_size;
          test "orientation" doors_orientation;
          test "show_arrows" doors_show_arrows;
          test "track_color" doors_track_color;
          test "thumb_color" doors_thumb_color;
          test "arrow_fg" doors_arrow_fg;
          test "arrow_bg" doors_arrow_bg;
        ];
    ]
