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
  let p = Box.Props.make ~border_style:Grid.Border.double () in
  let p_default =
    Box.Props.make ~border:true ~border_style:Grid.Border.double ()
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

let props_white_does_not_auto_enable () =
  let p = Box.Props.make ~border_color:Ansi.Color.White () in
  let p_default = Box.Props.make () in
  is_true ~msg:"not auto-enabled" (Box.Props.equal p p_default)

(* ── Construction ── *)

let create_attaches () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let node = Box.node box in
  match Renderable.parent node with
  | Some p -> equal ~msg:"parent" string (Renderable.id root) (Renderable.id p)
  | None -> fail "expected parent"

let create_with_border_has_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp one b.top);
  is_true ~msg:"right" (lp one b.right);
  is_true ~msg:"bottom" (lp one b.bottom);
  is_true ~msg:"left" (lp one b.left)

let create_without_border_no_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp zero_lp b.top);
  is_true ~msg:"right" (lp zero_lp b.right);
  is_true ~msg:"bottom" (lp zero_lp b.bottom);
  is_true ~msg:"left" (lp zero_lp b.left)

let create_border_sides_subset () =
  let t = make_ctx () in
  let root = make_root t in
  let box =
    Box.create ~parent:root ~border:true ~border_sides:[ `Top; `Bottom ] ()
  in
  let b = border_of (Box.node box) in
  is_true ~msg:"top" (lp one b.top);
  is_true ~msg:"bottom" (lp one b.bottom);
  is_true ~msg:"left zero" (lp zero_lp b.left);
  is_true ~msg:"right zero" (lp zero_lp b.right)

(* ── Border Setters ── *)

let set_border_enables_disables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_border box true;
  let b1 = border_of (Box.node box) in
  is_true ~msg:"enabled top" (lp one b1.top);
  Box.set_border box false;
  let b2 = border_of (Box.node box) in
  is_true ~msg:"disabled top" (lp zero_lp b2.top)

let set_border_style_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_border_style box Grid.Border.double;
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_border_color_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_border_color box Ansi.Color.red;
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_focused_border_color_some_auto_enables () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_focused_border_color box (Some Ansi.Color.green);
  let b = border_of (Box.node box) in
  is_true ~msg:"auto-enabled" (lp one b.top)

let set_focused_border_color_none_no_enable () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_focused_border_color box None;
  let b = border_of (Box.node box) in
  is_true ~msg:"still zero" (lp zero_lp b.top)

let set_border_sides_updates () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
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
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_background box (Some Ansi.Color.blue);
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  let before2 = !(t.schedule_count) in
  Box.set_background box None;
  is_true ~msg:"scheduled again" (!(t.schedule_count) > before2)

let set_fill () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_fill box false;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── Title ── *)

let set_title () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_title box (Some "Hello");
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  let before2 = !(t.schedule_count) in
  Box.set_title box None;
  is_true ~msg:"scheduled again" (!(t.schedule_count) > before2)

let set_title_alignment () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_title_alignment box `Center;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before)

(* ── apply_props ── *)

let apply_props_updates () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let props = Box.Props.make ~border:true ~title:"Test" () in
  let before = !(t.schedule_count) in
  Box.apply_props box props;
  is_true ~msg:"scheduled" (!(t.schedule_count) > before);
  let b = border_of (Box.node box) in
  is_true ~msg:"border applied" (lp one b.top)

(* ── set_style ── *)

let set_style_with_border_adds_insets () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  Box.set_style box Toffee.Style.default;
  let b = border_of (Box.node box) in
  is_true ~msg:"top inset" (lp one b.top);
  is_true ~msg:"left inset" (lp one b.left)

let set_style_without_border_passthrough () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  Box.set_style box Toffee.Style.default;
  let b = border_of (Box.node box) in
  is_true ~msg:"top zero" (lp zero_lp b.top);
  is_true ~msg:"left zero" (lp zero_lp b.left)

(* ── Child Clipping ── *)

let child_clip_all_borders () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:8;
  match Renderable.Private.child_clip node with
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
    Box.create ~parent:root ~border:true ~border_sides:[ `Top; `Left ] ()
  in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:8;
  match Renderable.Private.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 1 clip.x;
      equal ~msg:"y" int 1 clip.y;
      equal ~msg:"width" int 9 clip.width;
      equal ~msg:"height" int 7 clip.height
  | None -> fail "expected clip"

let child_clip_no_borders () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let node = Box.node box in
  layout_node node ~x:5 ~y:3 ~width:10 ~height:8;
  match Renderable.Private.child_clip node with
  | Some clip ->
      equal ~msg:"x" int 5 clip.x;
      equal ~msg:"y" int 3 clip.y;
      equal ~msg:"width" int 10 clip.width;
      equal ~msg:"height" int 8 clip.height
  | None -> fail "expected clip"

let child_clip_clamps_zero () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let node = Box.node box in
  layout_node node ~x:0 ~y:0 ~width:1 ~height:1;
  match Renderable.Private.child_clip node with
  | Some clip ->
      equal ~msg:"width" int 0 clip.width;
      equal ~msg:"height" int 0 clip.height
  | None -> fail "expected clip"

(* ── Focused Border Color ── *)

let focused_border_color_default () =
  let a = Box.Props.make ~border:true () in
  let b =
    Box.Props.make ~border:true ~focused_border_color:Ansi.Color.Bright_cyan ()
  in
  is_true ~msg:"default is Bright_cyan" (Box.Props.equal a b)

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
  let box = Box.create ~parent:root ~border:true () in
  let before = !(t.schedule_count) in
  Box.set_border box true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_style_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let before = !(t.schedule_count) in
  Box.set_border_style box Grid.Border.single;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_color_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let before = !(t.schedule_count) in
  Box.set_border_color box Ansi.Color.White;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_border_sides_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~border:true () in
  let before = !(t.schedule_count) in
  Box.set_border_sides box Grid.Border.all;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_background_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~background:Ansi.Color.blue () in
  let before = !(t.schedule_count) in
  Box.set_background box (Some Ansi.Color.blue);
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_fill_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_fill box true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_title_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root ~title:"Hello" () in
  let before = !(t.schedule_count) in
  Box.set_title box (Some "Hello");
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_title_alignment_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let box = Box.create ~parent:root () in
  let before = !(t.schedule_count) in
  Box.set_title_alignment box `Left;
  equal ~msg:"no schedule" int before !(t.schedule_count)

(* ── Runner ── *)

let () =
  run "mosaic.box"
    [
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
          test "White border_color does not auto-enable"
            props_white_does_not_auto_enable;
        ];
      group "Construction"
        [
          test "attaches to parent" create_attaches;
          test "with border has insets" create_with_border_has_insets;
          test "without border has no insets" create_without_border_no_insets;
          test "border_sides subset" create_border_sides_subset;
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
          test "default is Bright_cyan" focused_border_color_default;
          test "explicit color differs from default"
            focused_border_color_explicit;
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
