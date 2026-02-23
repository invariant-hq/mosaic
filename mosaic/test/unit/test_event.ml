open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let key_ev_a = Input.Key.of_char 'a'
let key_ev_b = Input.Key.of_char 'b'
let no_mod = Event.Mouse.no_modifier

let mouse_down ?(x = 0) ?(y = 0) ?target () =
  Event.Mouse.make ~x ~y ~modifiers:no_mod ?target (Down { button = Left })

(* ── Key ── *)

let key_of_input_roundtrip () =
  let ev = Event.Key.of_input key_ev_a in
  is_true ~msg:"data roundtrip"
    (Input.Key.equal_event (Event.Key.data ev) key_ev_a)

let key_fresh_propagation () =
  let ev = Event.Key.of_input key_ev_a in
  is_false ~msg:"propagation not stopped" (Event.Key.propagation_stopped ev)

let key_fresh_default () =
  let ev = Event.Key.of_input key_ev_a in
  is_false ~msg:"default not prevented" (Event.Key.default_prevented ev)

let key_stop_propagation () =
  let ev = Event.Key.of_input key_ev_a in
  Event.Key.stop_propagation ev;
  is_true ~msg:"propagation stopped" (Event.Key.propagation_stopped ev)

let key_prevent_default () =
  let ev = Event.Key.of_input key_ev_a in
  Event.Key.prevent_default ev;
  is_true ~msg:"default prevented" (Event.Key.default_prevented ev)

let key_stop_propagation_sticky () =
  let ev = Event.Key.of_input key_ev_a in
  Event.Key.stop_propagation ev;
  Event.Key.stop_propagation ev;
  is_true ~msg:"still stopped" (Event.Key.propagation_stopped ev)

let key_equal_ignores_dispatch () =
  let a = Event.Key.of_input key_ev_a in
  let b = Event.Key.of_input key_ev_a in
  Event.Key.stop_propagation a;
  Event.Key.prevent_default a;
  is_true ~msg:"equal despite dispatch" (Event.Key.equal a b)

let key_equal_different_keys () =
  let a = Event.Key.of_input key_ev_a in
  let b = Event.Key.of_input key_ev_b in
  is_false ~msg:"different keys" (Event.Key.equal a b)

(* ── Paste ── *)

let paste_roundtrip () =
  let ev = Event.Paste.of_text "hello" in
  equal ~msg:"text" string "hello" (Event.Paste.text ev)

let paste_fresh_dispatch () =
  let ev = Event.Paste.of_text "x" in
  is_false ~msg:"propagation" (Event.Paste.propagation_stopped ev);
  is_false ~msg:"default" (Event.Paste.default_prevented ev)

let paste_flags_independent () =
  let ev = Event.Paste.of_text "x" in
  Event.Paste.stop_propagation ev;
  Event.Paste.prevent_default ev;
  is_true ~msg:"propagation" (Event.Paste.propagation_stopped ev);
  is_true ~msg:"default" (Event.Paste.default_prevented ev)

let paste_equal_text_only () =
  let a = Event.Paste.of_text "same" in
  let b = Event.Paste.of_text "same" in
  Event.Paste.stop_propagation a;
  is_true ~msg:"equal" (Event.Paste.equal a b)

let paste_equal_different () =
  let a = Event.Paste.of_text "a" in
  let b = Event.Paste.of_text "b" in
  is_false ~msg:"different" (Event.Paste.equal a b)

let paste_empty () =
  let ev = Event.Paste.of_text "" in
  equal ~msg:"empty text" string "" (Event.Paste.text ev)

(* ── Mouse construction ── *)

let mouse_make_stores_fields () =
  let ev =
    Event.Mouse.make ~x:10 ~y:20
      ~modifiers:{ no_mod with ctrl = true }
      ~target:42
      (Down { button = Left })
  in
  is_true ~msg:"kind"
    (Event.Mouse.equal_kind (Event.Mouse.kind ev) (Down { button = Left }));
  equal ~msg:"x" int 10 (Event.Mouse.x ev);
  equal ~msg:"y" int 20 (Event.Mouse.y ev);
  is_true ~msg:"ctrl" (Event.Mouse.modifiers ev).ctrl;
  equal ~msg:"target" (option int) (Some 42) (Event.Mouse.target ev)

let mouse_target_default () =
  let ev = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod Move in
  equal ~msg:"no target" (option int) None (Event.Mouse.target ev)

let mouse_target_some () =
  let ev = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod ~target:7 Move in
  equal ~msg:"target" (option int) (Some 7) (Event.Mouse.target ev)

let mouse_fresh_dispatch () =
  let ev = mouse_down () in
  is_false ~msg:"propagation" (Event.Mouse.propagation_stopped ev);
  is_false ~msg:"default" (Event.Mouse.default_prevented ev)

let mouse_dispatch_control () =
  let ev = mouse_down () in
  Event.Mouse.stop_propagation ev;
  is_true ~msg:"propagation" (Event.Mouse.propagation_stopped ev);
  Event.Mouse.prevent_default ev;
  is_true ~msg:"default" (Event.Mouse.default_prevented ev)

(* ── Mouse equality ── *)

let mouse_equal_ignores_dispatch () =
  let a = mouse_down () in
  let b = mouse_down () in
  Event.Mouse.stop_propagation a;
  is_true ~msg:"equal" (Event.Mouse.equal a b)

let mouse_equal_different_kind () =
  let a =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod (Down { button = Left })
  in
  let b =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod
      (Up { button = Left; is_dragging = false })
  in
  is_false ~msg:"different kind" (Event.Mouse.equal a b)

let mouse_equal_different_coords () =
  let a = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod Move in
  let b = Event.Mouse.make ~x:1 ~y:0 ~modifiers:no_mod Move in
  is_false ~msg:"different x" (Event.Mouse.equal a b)

let mouse_equal_different_mods () =
  let a = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod Move in
  let b =
    Event.Mouse.make ~x:0 ~y:0 ~modifiers:{ no_mod with ctrl = true } Move
  in
  is_false ~msg:"different mods" (Event.Mouse.equal a b)

let mouse_equal_different_target () =
  let a = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod ~target:1 Move in
  let b = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod ~target:2 Move in
  is_false ~msg:"different target" (Event.Mouse.equal a b)

let mouse_equal_none_vs_some_target () =
  let a = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod Move in
  let b = Event.Mouse.make ~x:0 ~y:0 ~modifiers:no_mod ~target:1 Move in
  is_false ~msg:"none vs some" (Event.Mouse.equal a b)

(* ── Mouse button equality ── *)

let button_same_named () =
  is_true ~msg:"left" (Event.Mouse.equal_button Left Left);
  is_true ~msg:"middle" (Event.Mouse.equal_button Middle Middle);
  is_true ~msg:"right" (Event.Mouse.equal_button Right Right)

let button_same_numbered () =
  is_true ~msg:"button 5" (Event.Mouse.equal_button (Button 5) (Button 5))

let button_different () =
  is_false ~msg:"left/right" (Event.Mouse.equal_button Left Right);
  is_false ~msg:"button 3/4" (Event.Mouse.equal_button (Button 3) (Button 4))

let button_named_vs_numbered () =
  is_false ~msg:"left/button 0" (Event.Mouse.equal_button Left (Button 0))

(* ── Mouse kind equality ── *)

let kind_down () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind (Down { button = Left }) (Down { button = Left }));
  is_false ~msg:"diff"
    (Event.Mouse.equal_kind (Down { button = Left }) (Down { button = Right }))

let kind_up () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind
       (Up { button = Left; is_dragging = false })
       (Up { button = Left; is_dragging = false }));
  is_false ~msg:"diff dragging"
    (Event.Mouse.equal_kind
       (Up { button = Left; is_dragging = false })
       (Up { button = Left; is_dragging = true }))

let kind_move () = is_true ~msg:"move" (Event.Mouse.equal_kind Move Move)

let kind_drag () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind
       (Drag { button = Left; is_dragging = true })
       (Drag { button = Left; is_dragging = true }));
  is_false ~msg:"diff button"
    (Event.Mouse.equal_kind
       (Drag { button = Left; is_dragging = true })
       (Drag { button = Right; is_dragging = true }))

let kind_drag_end () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind
       (Drag_end { button = Left })
       (Drag_end { button = Left }));
  is_false ~msg:"diff"
    (Event.Mouse.equal_kind
       (Drag_end { button = Left })
       (Drag_end { button = Right }))

let kind_drop () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind
       (Drop { button = Left; source = Some 1 })
       (Drop { button = Left; source = Some 1 }));
  is_false ~msg:"diff source"
    (Event.Mouse.equal_kind
       (Drop { button = Left; source = Some 1 })
       (Drop { button = Left; source = None }))

let kind_over () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind (Over { source = None }) (Over { source = None }));
  is_false ~msg:"diff"
    (Event.Mouse.equal_kind (Over { source = Some 1 }) (Over { source = None }))

let kind_out () = is_true ~msg:"out" (Event.Mouse.equal_kind Out Out)

let kind_scroll () =
  is_true ~msg:"same"
    (Event.Mouse.equal_kind
       (Scroll { direction = Scroll_up; delta = 3 })
       (Scroll { direction = Scroll_up; delta = 3 }));
  is_false ~msg:"diff delta"
    (Event.Mouse.equal_kind
       (Scroll { direction = Scroll_up; delta = 3 })
       (Scroll { direction = Scroll_up; delta = 1 }));
  is_false ~msg:"diff dir"
    (Event.Mouse.equal_kind
       (Scroll { direction = Scroll_up; delta = 1 })
       (Scroll { direction = Scroll_down; delta = 1 }))

let kind_cross_variant () =
  is_false ~msg:"down/move"
    (Event.Mouse.equal_kind (Down { button = Left }) Move);
  is_false ~msg:"up/out"
    (Event.Mouse.equal_kind (Up { button = Left; is_dragging = false }) Out);
  is_false ~msg:"scroll/over"
    (Event.Mouse.equal_kind
       (Scroll { direction = Scroll_up; delta = 1 })
       (Over { source = None }))

(* ── Scroll direction ── *)

let scroll_direction_equal () =
  is_true ~msg:"up" (Event.Mouse.equal_scroll_direction Scroll_up Scroll_up);
  is_true ~msg:"down"
    (Event.Mouse.equal_scroll_direction Scroll_down Scroll_down);
  is_true ~msg:"left"
    (Event.Mouse.equal_scroll_direction Scroll_left Scroll_left);
  is_true ~msg:"right"
    (Event.Mouse.equal_scroll_direction Scroll_right Scroll_right);
  is_false ~msg:"up/down"
    (Event.Mouse.equal_scroll_direction Scroll_up Scroll_down);
  is_false ~msg:"left/right"
    (Event.Mouse.equal_scroll_direction Scroll_left Scroll_right)

(* ── Runner ── *)

let () =
  run "mosaic.event"
    [
      group "Key"
        [
          test "of_input roundtrip" key_of_input_roundtrip;
          test "fresh propagation not stopped" key_fresh_propagation;
          test "fresh default not prevented" key_fresh_default;
          test "stop_propagation sets flag" key_stop_propagation;
          test "prevent_default sets flag" key_prevent_default;
          test "stop_propagation is sticky" key_stop_propagation_sticky;
          test "equal ignores dispatch control" key_equal_ignores_dispatch;
          test "equal detects different keys" key_equal_different_keys;
        ];
      group "Paste"
        [
          test "of_text roundtrip" paste_roundtrip;
          test "fresh dispatch state" paste_fresh_dispatch;
          test "flags are independent" paste_flags_independent;
          test "equal compares text only" paste_equal_text_only;
          test "equal detects different text" paste_equal_different;
          test "empty paste" paste_empty;
        ];
      group "Mouse construction"
        [
          test "make stores fields" mouse_make_stores_fields;
          test "target defaults to None" mouse_target_default;
          test "target Some when provided" mouse_target_some;
          test "fresh dispatch state" mouse_fresh_dispatch;
          test "dispatch control" mouse_dispatch_control;
        ];
      group "Mouse equality"
        [
          test "ignores dispatch control" mouse_equal_ignores_dispatch;
          test "detects different kind" mouse_equal_different_kind;
          test "detects different coordinates" mouse_equal_different_coords;
          test "detects different modifiers" mouse_equal_different_mods;
          test "detects different target" mouse_equal_different_target;
          test "None vs Some target" mouse_equal_none_vs_some_target;
        ];
      group "Mouse button"
        [
          test "same named" button_same_named;
          test "same numbered" button_same_numbered;
          test "different" button_different;
          test "named vs numbered" button_named_vs_numbered;
        ];
      group "Mouse kind"
        [
          test "Down" kind_down;
          test "Up with is_dragging" kind_up;
          test "Move" kind_move;
          test "Drag" kind_drag;
          test "Drag_end" kind_drag_end;
          test "Drop with source" kind_drop;
          test "Over with source" kind_over;
          test "Out" kind_out;
          test "Scroll" kind_scroll;
          test "cross-variant" kind_cross_variant;
        ];
      group "Scroll direction"
        [ test "equal and not equal" scroll_direction_equal ];
    ]
