open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

let make_scroll_box ?scroll_x ?scroll_y ?scroll ?background ?scroll_accel
    ?on_scroll () =
  let t = make_ctx () in
  let root = make_root t in
  let sb =
    Scroll_box.create ~parent:root ?scroll_x ?scroll_y ?scroll ?background
      ?scroll_accel ()
  in
  (* The application accepts proposals explicitly. *)
  Scroll_box.set_on_scroll sb
    (Some
       (fun state ->
         Scroll_box.set_scroll sb state;
         Option.iter (fun f -> f state) on_scroll));
  (t, sb)

(* ── Scroll_accel ── *)

let accel_linear_always_one () =
  let a = Scroll_box.Scroll_accel.linear () in
  let m1 = Scroll_box.Scroll_accel.tick a ~seconds:0. in
  let m2 = Scroll_box.Scroll_accel.tick a ~seconds:0.1 in
  let m3 = Scroll_box.Scroll_accel.tick a ~seconds:0.2 in
  is_true ~msg:"first = 1.0" (Float.equal m1 1.0);
  is_true ~msg:"second = 1.0" (Float.equal m2 1.0);
  is_true ~msg:"third = 1.0" (Float.equal m3 1.0)

let accel_macos_first_tick_one () =
  let a = Scroll_box.Scroll_accel.macos () in
  let m = Scroll_box.Scroll_accel.tick a ~seconds:0. in
  is_true ~msg:"first = 1.0" (Float.equal m 1.0)

let accel_macos_accelerates_on_rapid_events () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0.02 in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0.04 in
  let m = Scroll_box.Scroll_accel.tick a ~seconds:0.06 in
  satisfies ~claim:"multiplier above 1.0" float_exact (fun m -> m > 1.0) m

let accel_macos_resets_after_timeout () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0.02 in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0.04 in
  (* Long pause exceeding streak_timeout (0.15 s) *)
  let m = Scroll_box.Scroll_accel.tick a ~seconds:0.5 in
  is_true ~msg:"reset to 1.0" (Float.equal m 1.0)

let accel_macos_respects_cap () =
  let a = Scroll_box.Scroll_accel.macos ~max_multiplier:2.0 () in
  (* Rapid events to build up velocity *)
  let m = ref 1.0 in
  for i = 1 to 30 do
    m := Scroll_box.Scroll_accel.tick a ~seconds:(float_of_int i *. 0.01)
  done;
  satisfies ~claim:"capped at 2.0" float_exact (fun m -> m <= 2.0) !m

let accel_reset_clears_history () =
  let a = Scroll_box.Scroll_accel.macos () in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0. in
  let _ = Scroll_box.Scroll_accel.tick a ~seconds:0.02 in
  Scroll_box.Scroll_accel.reset a;
  let m = Scroll_box.Scroll_accel.tick a ~seconds:0.1 in
  is_true ~msg:"back to 1.0 after reset" (Float.equal m 1.0)

(* ── Props ── *)

let props_defaults () =
  let p = Scroll_box.Props.default in
  is_true ~msg:"equal to make()"
    (Scroll_box.Props.equal p (Scroll_box.Props.make ()))

let props_equal_identical () =
  let a = Scroll_box.Props.make () in
  let b = Scroll_box.Props.make () in
  is_true ~msg:"equal" (Scroll_box.Props.equal a b)

let props_detects_scroll_x_diff () =
  let a = Scroll_box.Props.make ~scroll_x:true () in
  let b = Scroll_box.Props.make ~scroll_x:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_scroll_y_diff () =
  let a = Scroll_box.Props.make ~scroll_y:true () in
  let b = Scroll_box.Props.make ~scroll_y:false () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let props_detects_background_diff () =
  let a = Scroll_box.Props.make ~background:Ansi.Color.red () in
  let b = Scroll_box.Props.make ~background:Ansi.Color.blue () in
  is_false ~msg:"different" (Scroll_box.Props.equal a b)

let create_attaches () =
  let _t, sb = make_scroll_box () in
  match Renderable.parent (Scroll_box.node sb) with
  | Some _ -> ()
  | None -> fail "expected parent"

let create_default_scroll_zero () =
  let _t, sb = make_scroll_box () in
  equal ~msg:"scroll_top = 0" int 0 (Scroll_box.scroll_top sb);
  equal ~msg:"scroll_left = 0" int 0 (Scroll_box.scroll_left sb)

let create_child_target_set () =
  let _t, sb = make_scroll_box () in
  let target = Renderable.child_target (Scroll_box.node sb) in
  let content = Scroll_box.content sb in
  is_true ~msg:"target is content node"
    (Renderable.id target = Renderable.id content)

let create_vertical_bar_attached_to_root () =
  let _t, sb = make_scroll_box () in
  let bar_node = Scroll_bar.node (Scroll_box.vertical_bar sb) in
  match Renderable.parent bar_node with
  | None -> fail "expected vertical bar parent"
  | Some parent ->
      is_true ~msg:"vertical bar is attached to scroll box root"
        (Renderable.id parent = Renderable.id (Scroll_box.node sb))

let create_viewport_has_overflow_hidden () =
  let _t, sb = make_scroll_box () in
  let vp = Scroll_box.viewport sb in
  let style = Renderable.style vp in
  let overflow = Toffee.Style.overflow style in
  is_true ~msg:"overflow x hidden" (overflow.x = Toffee.Style.Overflow.Hidden);
  is_true ~msg:"overflow y hidden" (overflow.y = Toffee.Style.Overflow.Hidden)

let vertical_bar_visible_when_content_overflows () =
  let t = make_ctx () in
  let root = make_root t in
  let sb = Scroll_box.create ~parent:root () in
  layout_node root ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.node sb) ~x:0 ~y:0 ~width:20 ~height:8;
  layout_node (Scroll_box.viewport sb) ~x:0 ~y:0 ~width:19 ~height:8;
  layout_node (Scroll_box.content sb) ~x:0 ~y:0 ~width:19 ~height:20;
  Renderable.Private.pre_render_update (Scroll_box.content sb) ~dt:0.;
  is_true ~msg:"vertical bar visible on overflow"
    (Renderable.visible (Scroll_bar.node (Scroll_box.vertical_bar sb)))

(* ── Scroll Position ── *)

let set_scroll_top_clamps () =
  let _t, sb = make_scroll_box () in
  (* Without layout, max_scroll_y is 0, so everything clamps to 0 *)
  Scroll_box.set_scroll_top sb 100;
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll_top sb (-10);
  equal ~msg:"clamped to 0" int 0 (Scroll_box.scroll_top sb)

let scroll_to_sets_both () =
  let _t, sb = make_scroll_box ~scroll_x:true () in
  Scroll_box.scroll_to sb ~x:5 ~y:10 ();
  (* Both clamped to 0 without layout *)
  equal ~msg:"x clamped" int 0 (Scroll_box.scroll_left sb);
  equal ~msg:"y clamped" int 0 (Scroll_box.scroll_top sb)

let scroll_by_adjusts_relative () =
  let _t, sb = make_scroll_box () in
  Scroll_box.scroll_by sb ~y:10 ();
  (* Clamped to max_scroll_y = 0 *)
  equal ~msg:"clamped" int 0 (Scroll_box.scroll_top sb)

let on_scroll_fires () =
  let log = ref [] in
  let _t, sb =
    make_scroll_box ~on_scroll:(fun state -> log := state :: !log) ()
  in
  (* Even though position won't change (clamped), the callback should not fire
     unless there's an actual change *)
  Scroll_box.set_scroll_top sb 0;
  equal ~msg:"no spurious fire" int 0 (List.length !log)

let render_scroll_box sb ~vh ~ch =
  layout_node (Scroll_box.node sb) ~x:0 ~y:0 ~width:20 ~height:vh;
  layout_node (Scroll_box.viewport sb) ~x:0 ~y:0 ~width:19 ~height:vh;
  layout_node (Scroll_box.content sb) ~x:0 ~y:0 ~width:19 ~height:ch;
  Renderable.Private.pre_render_update (Scroll_box.content sb) ~dt:0.

let wheel ?(shift = false) direction =
  let modifiers = { Event.Mouse.no_modifier with shift } in
  Event.Mouse.make ~x:0 ~y:0 ~modifiers (Scroll { direction; delta = 1 })

(* The wheel goes from the content up through its ancestors until one stops
   it, as the renderer delivers it. *)
let emit_wheel sb event =
  let rec bubble node =
    if not (Event.Mouse.propagation_stopped event) then begin
      Renderable.Private.emit_mouse node event;
      Option.iter bubble (Renderable.parent node)
    end
  in
  bubble (Scroll_box.content sb)

let parent_node sb =
  match Renderable.parent (Scroll_box.node sb) with
  | Some parent -> parent
  | None -> fail "expected scroll box parent"

let wheel_consumes_only_when_scroll_position_changes () =
  let _test_context, sb = make_scroll_box () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:12;
  let down = wheel Scroll_down in
  emit_wheel sb down;
  equal ~msg:"wheel advances scroll position" int 1 (Scroll_box.scroll_top sb);
  is_true ~msg:"movement consumes wheel event"
    (Event.Mouse.propagation_stopped down);
  equal ~msg:"consumed event does not reach parent" int 0 !bubbled;
  Scroll_box.set_scroll_top sb 8;
  let clamped_down = wheel Scroll_down in
  emit_wheel sb clamped_down;
  equal ~msg:"boundary does not change position" int 8
    (Scroll_box.scroll_top sb);
  is_false ~msg:"clamped event remains available to parent"
    (Event.Mouse.propagation_stopped clamped_down);
  equal ~msg:"clamped event bubbles" int 1 !bubbled

let wheel_without_usable_axis_bubbles () =
  let _test_context, sb = make_scroll_box ~scroll_x:false ~scroll_y:true () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:12;
  let horizontal = wheel Scroll_right in
  emit_wheel sb horizontal;
  equal ~msg:"horizontal wheel leaves vertical position unchanged" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"disabled horizontal axis does not consume"
    (Event.Mouse.propagation_stopped horizontal);
  let shifted_vertical = wheel ~shift:true Scroll_down in
  emit_wheel sb shifted_vertical;
  equal ~msg:"shift-remapped wheel leaves vertical position unchanged" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"shift-remapped disabled axis does not consume"
    (Event.Mouse.propagation_stopped shifted_vertical);
  equal ~msg:"both unusable wheel events bubble" int 2 !bubbled

let wheel_without_overflow_bubbles () =
  let _test_context, sb = make_scroll_box () in
  let bubbled = ref 0 in
  Renderable.on_mouse (parent_node sb) (fun _event -> incr bubbled);
  render_scroll_box sb ~vh:4 ~ch:4;
  let down = wheel Scroll_down in
  emit_wheel sb down;
  equal ~msg:"content without overflow stays at origin" int 0
    (Scroll_box.scroll_top sb);
  is_false ~msg:"non-scrollable box does not consume"
    (Event.Mouse.propagation_stopped down);
  equal ~msg:"event bubbles past non-scrollable box" int 1 !bubbled

let nested_wheel_bubbles_at_inner_boundary () =
  let test_context = make_ctx () in
  let root = make_root test_context in
  let outer = Scroll_box.create ~parent:root () in
  Scroll_box.set_on_scroll outer (Some (Scroll_box.set_scroll outer));
  let inner = Scroll_box.create ~parent:(Scroll_box.content outer) () in
  Scroll_box.set_on_scroll inner (Some (Scroll_box.set_scroll inner));
  let escaped = ref 0 in
  Renderable.on_mouse root (fun _event -> incr escaped);
  render_scroll_box outer ~vh:8 ~ch:24;
  render_scroll_box inner ~vh:4 ~ch:12;
  let handled_by_inner = wheel Scroll_down in
  emit_wheel inner handled_by_inner;
  equal ~msg:"inner scrolls first" int 1 (Scroll_box.scroll_top inner);
  equal ~msg:"outer remains parked while inner can scroll" int 0
    (Scroll_box.scroll_top outer);
  equal ~msg:"handled event does not escape nested scroll boxes" int 0 !escaped;
  Scroll_box.set_scroll_top inner 8;
  let handled_by_outer = wheel Scroll_down in
  emit_wheel inner handled_by_outer;
  equal ~msg:"inner stays at its boundary" int 8 (Scroll_box.scroll_top inner);
  equal ~msg:"outer takes over at inner boundary" int 1
    (Scroll_box.scroll_top outer);
  is_true ~msg:"outer movement consumes bubbled event"
    (Event.Mouse.propagation_stopped handled_by_outer);
  equal ~msg:"outer consumption prevents escape" int 0 !escaped;
  Scroll_box.set_scroll_top outer 16;
  let both_clamped = wheel Scroll_down in
  emit_wheel inner both_clamped;
  equal ~msg:"inner remains clamped" int 8 (Scroll_box.scroll_top inner);
  equal ~msg:"outer remains clamped" int 16 (Scroll_box.scroll_top outer);
  is_false ~msg:"event remains unconsumed when no scroll box moves"
    (Event.Mouse.propagation_stopped both_clamped);
  equal ~msg:"event escapes after every scroll box declines it" int 1 !escaped

let set_background_updates () =
  let _t, sb = make_scroll_box () in
  Scroll_box.set_background sb (Some Ansi.Color.red);
  Scroll_box.set_background sb None;
  ()

let set_on_scroll_replaces () =
  let first_count = ref 0 in
  let second_count = ref 0 in
  let _t, sb = make_scroll_box ~on_scroll:(fun _ -> incr first_count) () in
  Scroll_box.set_on_scroll sb (Some (fun _ -> incr second_count));
  (* No actual scroll changes, just verify no crash *)
  ()

let set_scroll_accel_replaces () =
  let _t, sb = make_scroll_box () in
  let new_accel = Scroll_box.Scroll_accel.macos ~max_multiplier:3.0 () in
  Scroll_box.set_scroll_accel sb new_accel;
  ()

(* ── apply_props ── *)

let apply_props_updates () =
  let _t, sb = make_scroll_box () in
  let props =
    Scroll_box.Props.make
      ~scroll:(Scroll.follow `Bottom (Scroll.at 0))
      ~background:Ansi.Color.blue ()
  in
  Scroll_box.apply_props sb props;
  ()

(* ── Pretty-printing ── *)

let pp_produces_output () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  satisfies ~claim:"non-empty" string (fun s -> String.length s > 0) s

let pp_contains_scroll_box () =
  let _t, sb = make_scroll_box () in
  let s = Format.asprintf "%a" Scroll_box.pp sb in
  is_true ~msg:"has ScrollBox prefix"
    (String.length s >= 9 && String.sub s 0 9 = "ScrollBox")

let symbolic_follow_reflow () =
  let state = Scroll.follow `Bottom (Scroll.at 0) in
  let _ctx, sb = make_scroll_box ~scroll:state () in
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"follows bottom" int 32 (Scroll_box.scroll_top sb);
  render_scroll_box sb ~vh:10 ~ch:60;
  equal ~msg:"same accepted value follows resized content" int 50
    (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll sb (Scroll.at 49);
  render_scroll_box sb ~vh:10 ~ch:80;
  equal ~msg:"absolute position never silently becomes follow" int 49
    (Scroll_box.scroll_top sb);
  Scroll_box.set_scroll sb state;
  equal ~msg:"follow can be reaccepted without a request key" int 70
    (Scroll_box.scroll_top sb)

let symbolic_reveal_is_idempotent () =
  let state = Scroll.reveal ~line:30 (Scroll.at 0) in
  let _ctx, sb = make_scroll_box ~scroll:state () in
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"nearest visible row" int 23 (Scroll_box.scroll_top sb);
  render_scroll_box sb ~vh:8 ~ch:40;
  equal ~msg:"same reveal has no repeated displacement" int 23
    (Scroll_box.scroll_top sb);
  render_scroll_box sb ~vh:10 ~ch:40;
  equal ~msg:"reveal resolves against new viewport" int 21
    (Scroll_box.scroll_top sb)

let proposals_use_accepted_state () =
  let _ctx, sb = make_scroll_box ~scroll:(Scroll.at 10) () in
  render_scroll_box sb ~vh:8 ~ch:40;
  let proposals = ref [] in
  Scroll_box.set_on_scroll sb
    (Some (fun state -> proposals := state :: !proposals));
  Scroll_box.scroll_by_unit sb ~y:1. ~unit:`Viewport ();
  Scroll_box.scroll_by_unit sb ~y:1. ~unit:`Viewport ();
  equal ~msg:"rejection preserves accepted position" int 10
    (Scroll_box.scroll_top sb);
  let offsets =
    List.map
      (fun state ->
        snd
          (Scroll.resolve state ~width:19 ~height:8 ~content_width:19
             ~content_height:40))
      !proposals
  in
  equal ~msg:"both proposals derive from accepted position" (list int)
    [ 18; 18 ] offsets;
  Scroll_box.set_scroll sb (List.hd !proposals);
  equal ~msg:"application can accept a proposal" int 18
    (Scroll_box.scroll_top sb)

let orthogonal_follow_is_preserved () =
  let state =
    Scroll.follow `Bottom (Scroll.at 0) |> Scroll.with_position ~x:5
  in
  equal ~msg:"horizontal position preserves vertical follow" (pair int int)
    (5, 30)
    (Scroll.resolve state ~width:10 ~height:10 ~content_width:30
       ~content_height:40)

let visible_bar_children bar =
  List.fold_left
    (fun count child -> if Renderable.visible child then count + 1 else count)
    0
    (Renderable.children (Scroll_bar.node bar))

let scrollbar_appearance_preserves_axes () =
  let ctx = make_ctx () in
  let root = make_root ctx in
  let sb =
    Scroll_box.create ~parent:root ~scroll_x:true
      ~scrollbar_style:(Scroll_bar.style ~show_arrows:true ())
      ()
  in
  let direction bar =
    Toffee.Style.flex_direction (Renderable.style (Scroll_bar.node bar))
  in
  is_true ~msg:"horizontal bar stays horizontal"
    (direction (Scroll_box.horizontal_bar sb) = Toffee.Style.Flex_direction.Row);
  is_true ~msg:"vertical bar stays vertical"
    (direction (Scroll_box.vertical_bar sb) = Toffee.Style.Flex_direction.Column)

let scrollbar_appearance_preserves_resolved_position () =
  let _, sb = make_scroll_box ~scroll:(Scroll.at 5) () in
  render_scroll_box sb ~vh:4 ~ch:20;
  let bar = Scroll_box.vertical_bar sb in
  equal ~msg:"initial accepted offset" int 5 (Scroll_bar.scroll_position bar);
  Scroll_box.apply_props sb
    (Scroll_box.Props.make ~scroll:(Scroll.at 5)
       ~scrollbar_style:(Scroll_bar.style ~thumb_color:Ansi.Color.cyan ())
       ());
  equal ~msg:"appearance retains content extent" int 20
    (Scroll_bar.scroll_size bar);
  equal ~msg:"appearance retains viewport extent" int 4
    (Scroll_bar.viewport_size bar);
  equal ~msg:"appearance retains accepted offset" int 5
    (Scroll_bar.scroll_position bar)

let removed_scrollbar_appearance_restores_default () =
  let _, sb = make_scroll_box () in
  Scroll_box.apply_props sb
    (Scroll_box.Props.make
       ~scrollbar_style:(Scroll_bar.style ~show_arrows:true ())
       ());
  equal ~msg:"custom arrows visible" int 3
    (visible_bar_children (Scroll_box.vertical_bar sb));
  Scroll_box.apply_props sb (Scroll_box.Props.make ());
  equal ~msg:"removed style restores default arrows" int 1
    (visible_bar_children (Scroll_box.vertical_bar sb))

let axis_styles_replace_shared_and_restore_it () =
  let ctx, sb = make_scroll_box () in
  let shared = Scroll_bar.style ~show_arrows:true () in
  let vertical = Scroll_bar.style ~thumb_color:Ansi.Color.cyan () in
  let props =
    Scroll_box.Props.make ~scrollbar_style:shared ~vertical_bar_style:vertical
      ()
  in
  Scroll_box.apply_props sb props;
  equal ~msg:"axis style replaces shared arrow setting" int 1
    (visible_bar_children (Scroll_box.vertical_bar sb));
  equal ~msg:"other axis inherits shared arrows" int 3
    (visible_bar_children (Scroll_box.horizontal_bar sb));
  let before = !(ctx.schedule_count) in
  Scroll_box.apply_props sb props;
  equal ~msg:"same appearance schedules nothing" int before
    !(ctx.schedule_count);
  Scroll_box.apply_props sb (Scroll_box.Props.make ~scrollbar_style:shared ());
  equal ~msg:"removing axis override restores shared arrows" int 3
    (visible_bar_children (Scroll_box.vertical_bar sb))

let appearance_keeps_scrollbars_hidden () =
  let _, sb = make_scroll_box () in
  render_scroll_box sb ~vh:4 ~ch:20;
  let props =
    Scroll_box.Props.make ~show_scrollbars:false
      ~scrollbar_style:(Scroll_bar.style ~show_arrows:true ())
      ()
  in
  Scroll_box.apply_props sb props;
  render_scroll_box sb ~vh:4 ~ch:30;
  is_false ~msg:"appearance does not release hidden-bar policy"
    (Renderable.visible (Scroll_bar.node (Scroll_box.vertical_bar sb)))

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.scroll_box}) or to {!Scroll_box.create},
   reaches the live scroll box ({!Test_harness.reaches}). *)

let doors_size = Mosaic.size ~width:12 ~height:4
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red
let doors_bar = Scroll_bar.style ~track_color:doors_red ()

let doors_contents =
  List.init 8 (fun i -> Printf.sprintf "line %d of the content" i)

let doors_lines = List.map (fun l -> Mosaic.text l) doors_contents

let doors_filled box =
  List.iter
    (fun content ->
      ignore (Text.create ~parent:(Scroll_box.node box) ~content () : Text.t))
    doors_contents;
  Scroll_box.node box

let doors_scroll () =
  Test_harness.reaches "scroll"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 3)
         ~on_scroll:ignore doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scroll:(Scroll.at 3) ()))

let doors_scroll_x () =
  Test_harness.reaches "scroll_x"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~scroll_x:true doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scroll_x:true ()))

let doors_scroll_y () =
  Test_harness.reaches "scroll_y"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~scroll_y:false doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scroll_y:false ()))

let doors_background () =
  Test_harness.reaches "background"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~background:doors_red doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~background:doors_red ()))

let doors_show_scrollbars () =
  Test_harness.reaches "show_scrollbars"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~show_scrollbars:false doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~show_scrollbars:false ()))

let doors_scrollbar_style () =
  Test_harness.reaches "scrollbar_style"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~scrollbar_style:doors_bar doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scrollbar_style:doors_bar
           ()))

let doors_vertical_bar_style () =
  Test_harness.reaches "vertical_bar_style"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~vertical_bar_style:doors_bar doors_lines)
    ~create_base:(fun parent ->
      doors_filled (Scroll_box.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style
           ~vertical_bar_style:doors_bar ()))

let doors_horizontal_bar_style () =
  Test_harness.reaches "horizontal_bar_style"
    ~base:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~scroll_x:true doors_lines)
    ~element:
      (Mosaic.scroll_box ~size:doors_size ~scroll:(Scroll.at 0)
         ~on_scroll:ignore ~scroll_x:true ~horizontal_bar_style:doors_bar
         doors_lines)
    ~create_base:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scroll_x:true ()))
    ~create:(fun parent ->
      doors_filled
        (Scroll_box.create ~parent ~style:doors_style ~scroll_x:true
           ~horizontal_bar_style:doors_bar ()))

let () =
  run "mosaic.scroll_box"
    [
      test "accel linear always one" accel_linear_always_one;
      test "accel macos first tick one" accel_macos_first_tick_one;
      test "accel macos accelerates on rapid events"
        accel_macos_accelerates_on_rapid_events;
      test "accel macos resets after timeout" accel_macos_resets_after_timeout;
      test "accel macos respects cap" accel_macos_respects_cap;
      test "accel reset clears history" accel_reset_clears_history;
      test "props defaults" props_defaults;
      test "props equal identical" props_equal_identical;
      test "props detects scroll x diff" props_detects_scroll_x_diff;
      test "props detects scroll y diff" props_detects_scroll_y_diff;
      test "props detects background diff" props_detects_background_diff;
      test "create attaches" create_attaches;
      test "create default scroll zero" create_default_scroll_zero;
      test "create child target set" create_child_target_set;
      test "create vertical bar attached to root"
        create_vertical_bar_attached_to_root;
      test "create viewport has overflow hidden"
        create_viewport_has_overflow_hidden;
      test "vertical bar visible when content overflows"
        vertical_bar_visible_when_content_overflows;
      test "set scroll top clamps" set_scroll_top_clamps;
      test "scroll to sets both" scroll_to_sets_both;
      test "scroll by adjusts relative" scroll_by_adjusts_relative;
      test "on scroll fires" on_scroll_fires;
      test "wheel consumes only when scroll position changes"
        wheel_consumes_only_when_scroll_position_changes;
      test "wheel without usable axis bubbles" wheel_without_usable_axis_bubbles;
      test "wheel without overflow bubbles" wheel_without_overflow_bubbles;
      test "nested wheel bubbles at inner boundary"
        nested_wheel_bubbles_at_inner_boundary;
      test "set background updates" set_background_updates;
      test "set on scroll replaces" set_on_scroll_replaces;
      test "set scroll accel replaces" set_scroll_accel_replaces;
      test "apply props updates" apply_props_updates;
      test "scrollbar appearance preserves axes"
        scrollbar_appearance_preserves_axes;
      test "scrollbar appearance preserves resolved position"
        scrollbar_appearance_preserves_resolved_position;
      test "removed scrollbar appearance restores defaults"
        removed_scrollbar_appearance_restores_default;
      test "axis appearance replaces and restores shared style"
        axis_styles_replace_shared_and_restore_it;
      test "appearance keeps scrollbars hidden"
        appearance_keeps_scrollbars_hidden;
      test "pp produces output" pp_produces_output;
      test "pp contains scroll box" pp_contains_scroll_box;
      test "symbolic follow reflow" symbolic_follow_reflow;
      test "symbolic reveal is idempotent" symbolic_reveal_is_idempotent;
      test "proposals use accepted state" proposals_use_accepted_state;
      test "orthogonal follow is preserved" orthogonal_follow_is_preserved;
      group "Both doors"
        [
          test "scroll" doors_scroll;
          test "scroll_x" doors_scroll_x;
          test "scroll_y" doors_scroll_y;
          test "background" doors_background;
          test "show_scrollbars" doors_show_scrollbars;
          test "scrollbar_style" doors_scrollbar_style;
          test "vertical_bar_style" doors_vertical_bar_style;
          test "horizontal_bar_style" doors_horizontal_bar_style;
        ];
    ]
