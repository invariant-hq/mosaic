open Windtrap
open Mosaic_ui
open Test_harness

(* ── Props ── *)

let props_defaults () =
  let p = Box.Props.default in
  is_true ~msg:"equal to make()" (Box.Props.equal p (Box.Props.make ()))

let props_equal_identical () =
  let a = Box.Props.make () in
  let b = Box.Props.make () in
  is_true ~msg:"equal" (Box.Props.equal a b)

let props_detects_border_diff () =
  let a = Box.Props.make ~border:true () in
  let b = Box.Props.make () in
  is_false ~msg:"different" (Box.Props.equal a b)

let props_detects_title_diff () =
  let a = Box.Props.make ~title:"A" () in
  let b = Box.Props.make ~title:"B" () in
  is_false ~msg:"different title" (Box.Props.equal a b)

let props_border_style_auto_enables () =
  let p = Box.Props.make ~border_style:Matrix_grid.Border.double () in
  let p_default =
    Box.Props.make ~border:true ~border_style:Matrix_grid.Border.double ()
  in
  is_true ~msg:"auto-enabled" (Box.Props.equal p p_default)

let props_border_color_auto_enables () =
  let p = Box.Props.make ~border_color:Ansi.Color.red () in
  let p_expected =
    Box.Props.make ~border:true ~border_color:Ansi.Color.red ()
  in
  is_true ~msg:"auto-enabled" (Box.Props.equal p p_expected)

let props_focused_border_color_auto_enables () =
  let p = Box.Props.make ~focused_border_color:Ansi.Color.blue () in
  let p_expected =
    Box.Props.make ~border:true ~focused_border_color:Ansi.Color.blue ()
  in
  is_true ~msg:"auto-enabled" (Box.Props.equal p p_expected)

(* A color equal to the old default enables the border as any other: the
   label is compared as given, not by value. *)
let props_white_auto_enables () =
  let p = Box.Props.make ~border_color:Ansi.Color.white () in
  let q = Box.Props.make ~border:true ~border_color:Ansi.Color.white () in
  is_true ~msg:"auto-enabled" (Box.Props.equal p q)

(* ── Construction ── *)

let create_attaches () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let node = Box.node box in
  match Renderable.parent node with
  | Some p -> equal ~msg:"parent" string (Renderable.id root) (Renderable.id p)
  | None -> fail "expected parent"

let create_with_border_has_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp one b.top);
  is_true ~msg:"right" (lp one b.right);
  is_true ~msg:"bottom" (lp one b.bottom);
  is_true ~msg:"left" (lp one b.left)

let create_without_border_no_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp zero_lp b.top);
  is_true ~msg:"right" (lp zero_lp b.right);
  is_true ~msg:"bottom" (lp zero_lp b.bottom);
  is_true ~msg:"left" (lp zero_lp b.left)

let create_border_sides_subset () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root
      (Box.Props.make ~border:true ~border_sides:[ `Top; `Bottom ] ())
  in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp one b.top);
  is_true ~msg:"bottom" (lp one b.bottom);
  is_true ~msg:"left zero" (lp zero_lp b.left);
  is_true ~msg:"right zero" (lp zero_lp b.right)

(* Each side with a border takes its border width in layout, and a side of
   width 0 draws no line. A side without a border takes no cells, as a CSS
   border whose style is none. *)
let border_width_sets_insets_and_lines () =
  let t = make_ctx () in
  let root = make_root t in
  let cells n = Toffee.Style.Length_percentage.length (Float.of_int n) in
  let width ~left =
    { (Toffee.Geometry.Rect.all (cells 2)) with left = cells left }
  in
  let box =
    Box.create ~parent:root
      (Box.Props.make ~border:true ~border_width:(width ~left:0) ())
  in
  let node = Box.node box in
  let b = border_of node in
  is_true ~msg:"a side takes its width" (lp (cells 2) b.top);
  is_true ~msg:"a side of width 0 takes nothing" (lp zero_lp b.left);
  layout_node node ~x:0 ~y:0 ~width:6 ~height:5;
  let grid = make_grid ~width:6 ~height:5 () in
  Test_harness.render node grid ~dt:0.;
  let cell x y = Matrix_grid.get_text grid ((y * 6) + x) in
  equal ~msg:"a side of width 0 draws no line" string " " (cell 0 2);
  equal ~msg:"a wide side draws its line outermost" string "\u{2502}" (cell 5 2);
  equal ~msg:"and leaves the cells inside it blank" string " " (cell 4 2);
  Box.set_border box false;
  is_true ~msg:"no border takes no cells" (lp zero_lp (border_of node).top);
  raises_match ~msg:"a part of a cell"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Box.Props.make
           ~border_width:
             (Toffee.Geometry.Rect.all
                (Toffee.Style.Length_percentage.length 0.5))
           ()
          : Box.Props.t))

let child_clip_inside_border_width () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root
      (Box.Props.make ~border:true
         ~border_width:
           (Toffee.Geometry.Rect.all (Toffee.Style.Length_percentage.length 2.))
         ())
  in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:8;
  match Renderable.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 2 clip.x;
      equal ~msg:"y" int 2 clip.y;
      equal ~msg:"width" int 6 clip.width;
      equal ~msg:"height" int 4 clip.height
  | None -> fail "expected clip"

(* ── Border Setters ── *)

let set_border_enables_disables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_border box true;
  let b1 = border_of (Box.node box) in
  is_true ~msg:"enabled top" (lp one b1.top);
  Box.set_border box false;
  let b2 = border_of (Box.node box) in
  is_true ~msg:"disabled top" (lp zero_lp b2.top)

let set_border_style_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_border_style box Matrix_grid.Border.double;
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_border_color_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_border_color box Ansi.Color.red;
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_focused_border_color_some_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_focused_border_color box (Some Ansi.Color.green);
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_focused_border_color_none_no_enable () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_focused_border_color box None;
  let b = border_of (Box.node box) in
  is_true ~msg:"still zero" (lp zero_lp b.top)

let set_border_sides_updates () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  Box.set_border_sides box [ `Left; `Right ];
  let b = border_of (Box.node box) in
  is_true ~msg:"top zero" (lp zero_lp b.top);
  is_true ~msg:"bottom zero" (lp zero_lp b.bottom);
  is_true ~msg:"left one" (lp one b.left);
  is_true ~msg:"right one" (lp one b.right)

(* ── Background and Fill ── *)

let set_background () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_background box (Some Ansi.Color.blue);
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  let before2 = Test_harness.clean t in
  Box.set_background box None;
  gt ~msg:"scheduled again" ~than:before2 !(t.schedule_count)

let set_fill () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_fill box false;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Title ── *)

let set_title () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_title box (Some "Hello");
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  let before2 = Test_harness.clean t in
  Box.set_title box None;
  gt ~msg:"scheduled again" ~than:before2 !(t.schedule_count)

let set_title_alignment () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_title_alignment box `Center;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── apply_props ── *)

let apply_props_updates () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let props = Box.Props.make ~border:true ~title:"Test" () in
  let before = Test_harness.clean t in
  Box.apply_props box props;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count);
  let b = border_of (Box.node box) in
  is_true ~msg:"border applied" (lp one b.top)

(* ── set_style ── *)

let set_style_with_border_adds_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  Box.set_style box Toffee.Style.default;
  let b = border_of (Box.node box) in
  is_true ~msg:"top inset" (lp one b.top);
  is_true ~msg:"left inset" (lp one b.left)

let set_style_without_border_passthrough () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  Box.set_style box Toffee.Style.default;
  let b = border_of (Box.node box) in
  is_true ~msg:"top zero" (lp zero_lp b.top);
  is_true ~msg:"left zero" (lp zero_lp b.left)

(* ── Child Clipping ── *)

let child_clip_all_borders () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:8;
  match Renderable.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 1 clip.x;
      equal ~msg:"y" int 1 clip.y;
      equal ~msg:"width" int 8 clip.width;
      equal ~msg:"height" int 6 clip.height
  | None -> fail "expected clip"

let child_clip_partial_borders () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root
      (Box.Props.make ~border:true ~border_sides:[ `Top; `Left ] ())
  in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:8;
  match Renderable.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 1 clip.x;
      equal ~msg:"y" int 1 clip.y;
      equal ~msg:"width" int 9 clip.width;
      equal ~msg:"height" int 7 clip.height
  | None -> fail "expected clip"

let child_clip_no_borders () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let node = Box.node box in
  layout_node node ~x:5 ~y:3 ~width:10 ~height:8;
  match Renderable.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 5 clip.x;
      equal ~msg:"y" int 3 clip.y;
      equal ~msg:"width" int 10 clip.width;
      equal ~msg:"height" int 8 clip.height
  | None -> fail "expected clip"

let child_clip_clamps_zero () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:1 ~height:1;
  match Renderable.child_clip node with
  | Some clip ->
      equal ~msg:"width" int 0 clip.width;
      equal ~msg:"height" int 0 clip.height
  | None -> fail "expected clip"

(* ── Focused Border Color ── *)

let focused_border_color_default () =
  let a = Box.Props.make ~border:true () in
  let b =
    Box.Props.make ~border:true ~focused_border_color:Ansi.Color.bright_cyan ()
  in
  is_false ~msg:"default is none" (Box.Props.equal a b)

let focused_border_color_explicit () =
  let a =
    Box.Props.make ~border:true ~focused_border_color:Ansi.Color.green ()
  in
  let b = Box.Props.make ~border:true () in
  is_false ~msg:"green differs from default" (Box.Props.equal a b)

(* ── Setter No-ops ── *)

let set_border_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let before = Test_harness.clean t in
  Box.set_border box true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_style_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let before = Test_harness.clean t in
  Box.set_border_style box Matrix_grid.Border.single;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_color_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root
      (Box.Props.make ~border:true ~border_color:Ansi.Color.white ())
  in
  let before = Test_harness.clean t in
  Box.set_border_color box Ansi.Color.white;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_sides_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~border:true ()) in
  let before = Test_harness.clean t in
  Box.set_border_sides box Matrix_grid.Border.all;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_background_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root (Box.Props.make ~background:Ansi.Color.blue ())
  in
  let before = Test_harness.clean t in
  Box.set_background box (Some Ansi.Color.blue);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_fill_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_fill box true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_title_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ~title:"Hello" ()) in
  let before = Test_harness.clean t in
  Box.set_title box (Some "Hello");
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_title_alignment_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root (Box.Props.make ()) in
  let before = Test_harness.clean t in
  Box.set_title_alignment box `Left;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.box}) or to {!Box.Props.make}
   for {!Box.create}, reaches the live box ({!Test_harness.reaches}). The box
   holds a text, so its border widths show where the text goes. *)

let size = Mosaic.size ~width:12 ~height:4
let style = Toffee.Style.make ~size ()
let red = Ansi.Color.red

let boxed b =
  ignore
    (Text.create ~parent:(Box.node b) (Text.Props.make ~content:"x" ())
      : Text.t);
  Box.node b

let reaches ?act ?observe label ~base ~element ~create_base ~create =
  Test_harness.reaches ?act ?observe label
    ~base:(base [ Mosaic.text "x" ])
    ~element:(element [ Mosaic.text "x" ])
    ~create_base:(fun parent -> boxed (create_base ~parent))
    ~create:(fun parent -> boxed (create ~parent))

let doors_border () =
  reaches "border" ~base:(Mosaic.box ~size)
    ~element:(Mosaic.box ~size ~border:true)
    ~create_base:(fun ~parent -> Box.create ~parent ~style (Box.Props.make ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))

let doors_border_style () =
  reaches "border_style"
    ~base:(Mosaic.box ~size ~border:true)
    ~element:
      (Mosaic.box ~size ~border:true ~border_style:Matrix_grid.Border.double)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~border_style:Matrix_grid.Border.double ()))

let doors_border_sides () =
  reaches "border_sides"
    ~base:(Mosaic.box ~size ~border:true)
    ~element:(Mosaic.box ~size ~border:true ~border_sides:[ `Top ])
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~border_sides:[ `Top ] ()))

let doors_border_width () =
  let two = Toffee.Style.Length_percentage.length 2. in
  let one = Toffee.Style.Length_percentage.length 1. in
  let widths =
    Toffee.Geometry.Rect.make ~left:two ~right:one ~top:one ~bottom:one
  in
  reaches "border_width"
    ~base:(Mosaic.box ~size ~border:true)
    ~element:(Mosaic.box ~size ~border:true ~border_width:widths)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~border_width:widths ()))

let doors_border_color () =
  reaches "border_color"
    ~base:(Mosaic.box ~size ~border:true)
    ~element:(Mosaic.box ~size ~border:true ~border_color:red)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~border_color:red ()))

let doors_focused_border_color () =
  reaches "focused_border_color" ~act:Test_harness.focus
    ~base:(Mosaic.box ~size ~border:true)
    ~element:(Mosaic.box ~size ~border:true ~focused_border_color:red)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~focused_border_color:red ()))

let doors_background () =
  reaches "background" ~base:(Mosaic.box ~size)
    ~element:(Mosaic.box ~size ~background:red)
    ~create_base:(fun ~parent -> Box.create ~parent ~style (Box.Props.make ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~background:red ()))

let doors_fill () =
  reaches "fill"
    ~base:(Mosaic.box ~size ~background:red)
    ~element:(Mosaic.box ~size ~background:red ~fill:false)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~background:red ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~background:red ~fill:false ()))

let doors_title () =
  reaches "title"
    ~base:(Mosaic.box ~size ~border:true)
    ~element:(Mosaic.box ~size ~border:true ~title:"T")
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ~title:"T" ()))

let doors_title_alignment () =
  reaches "title_alignment"
    ~base:(Mosaic.box ~size ~border:true ~title:"T")
    ~element:(Mosaic.box ~size ~border:true ~title:"T" ~title_alignment:`Right)
    ~create_base:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~border:true ~title:"T" ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style
        (Box.Props.make ~border:true ~title:"T" ~title_alignment:`Right ()))

let doors_inert () =
  reaches "inert"
    ~observe:(fun _ node -> string_of_bool (Renderable.inert node))
    ~base:(Mosaic.box ~size)
    ~element:(Mosaic.box ~size ~inert:true)
    ~create_base:(fun ~parent -> Box.create ~parent ~style (Box.Props.make ()))
    ~create:(fun ~parent ->
      Box.create ~parent ~style (Box.Props.make ~inert:true ()))

(* ── Paint ── *)

(* A box draws its border by the [Border] role, a label overriding it, and
   shows its focus by heavy lines in bold. *)

let paint_size = Mosaic.size ~width:10 ~height:3
let red = Ansi.Color.indexed 1
let blue = Ansi.Color.indexed 4

let paint_legible_on_light () =
  let view =
    Mosaic.box ~border:true ~title:"Title" ~size:paint_size
      [ Mosaic.text "hello" ]
  in
  equal ~msg:"no cell below its contrast" (list string) []
    (illegible xterm_light
       (painted (paint ~theme:(Theme.default xterm_light) view)))

let paint_border_role () =
  let themed = Theme.with_style Border (Ansi.Style.make ~fg:red ()) in
  let r =
    paint (Mosaic.themed themed (Mosaic.box ~border:true ~size:paint_size []))
  in
  is_true ~msg:"the role's color" (fg_is red (cell_style r ~x:0 ~y:0));
  let r =
    paint
      (Mosaic.themed themed
         (Mosaic.box ~border:true ~border_color:blue ~size:paint_size []))
  in
  is_true ~msg:"the label's color" (fg_is blue (cell_style r ~x:0 ~y:0))

let paint_focus_shows () =
  focus_shows (Mosaic.box ~border:true ~size:paint_size [])

let paint_focused_border_color () =
  let r =
    paint ~focused:true
      (Mosaic.box ~border:true ~focused_border_color:red ~size:paint_size [])
  in
  is_true ~msg:"the label's color" (fg_is red (cell_style r ~x:0 ~y:0));
  equal ~msg:"its own lines" string "\u{250C}" (cell_text r ~x:0 ~y:0)

let () =
  run "mosaic.box"
    [
      group "Paint"
        [
          test "legible on a light terminal" paint_legible_on_light;
          test "border by role, label first" paint_border_role;
          test "focus shows at every styling" paint_focus_shows;
          test "focused_border_color" paint_focused_border_color;
        ];
      group "Props"
        [
          test "default values" props_defaults;
          test "equal on identical" props_equal_identical;
          test "detects border difference" props_detects_border_diff;
          test "detects title difference" props_detects_title_diff;
          test "border_style auto-enables" props_border_style_auto_enables;
          test "border_color non-default auto-enables"
            props_border_color_auto_enables;
          test "focused_border_color auto-enables"
            props_focused_border_color_auto_enables;
          test "White border_color auto-enables" props_white_auto_enables;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "with border has insets" create_with_border_has_insets;
          test "without border has no insets" create_without_border_no_insets;
          test "border_sides subset" create_border_sides_subset;
          test "border width sets insets and lines"
            border_width_sets_insets_and_lines;
          test "child clip inside border width" child_clip_inside_border_width;
        ];
      group "Border setters"
        [
          test "set_border enables/disables" set_border_enables_disables;
          test "set_border_style auto-enables" set_border_style_auto_enables;
          test "set_border_color auto-enables" set_border_color_auto_enables;
          test "set_focused_border_color Some auto-enables"
            set_focused_border_color_some_auto_enables;
          test "set_focused_border_color None no enable"
            set_focused_border_color_none_no_enable;
          test "set_border_sides updates" set_border_sides_updates;
        ];
      group "Background and fill"
        [
          test "set_background Some/None" set_background;
          test "set_fill toggle" set_fill;
        ];
      group "Title"
        [
          test "set_title sets and clears" set_title;
          test "set_title_alignment" set_title_alignment;
        ];
      group "apply_props" [ test "updates all properties" apply_props_updates ];
      group "set_style"
        [
          test "with border adds insets" set_style_with_border_adds_insets;
          test "without border passes through"
            set_style_without_border_passthrough;
        ];
      group "Child clipping"
        [
          test "content area inside all-side borders" child_clip_all_borders;
          test "partial borders" child_clip_partial_borders;
          test "no borders" child_clip_no_borders;
          test "clamps to zero" child_clip_clamps_zero;
        ];
      group "Focused border color"
        [
          test "default is none" focused_border_color_default;
          test "explicit color differs from default"
            focused_border_color_explicit;
        ];
      group "Both doors"
        [
          test "border" doors_border;
          test "border_style" doors_border_style;
          test "border_sides" doors_border_sides;
          test "border_width" doors_border_width;
          test "border_color" doors_border_color;
          test "focused_border_color" doors_focused_border_color;
          test "background" doors_background;
          test "fill" doors_fill;
          test "title" doors_title;
          test "title_alignment" doors_title_alignment;
          test "inert" doors_inert;
        ];
      group "Setter no-ops"
        [
          test "set_border no-op on same value" set_border_noop_same_value;
          test "set_border_style no-op on same value"
            set_border_style_noop_same_value;
          test "set_border_color no-op on same value"
            set_border_color_noop_same_value;
          test "set_border_sides no-op on same value"
            set_border_sides_noop_same_value;
          test "set_background no-op on same value"
            set_background_noop_same_value;
          test "set_fill no-op on same value" set_fill_noop_same_value;
          test "set_title no-op on same value" set_title_noop_same_value;
          test "set_title_alignment no-op on same value"
            set_title_alignment_noop_same_value;
        ];
    ]
