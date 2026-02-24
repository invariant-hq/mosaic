open Windtrap
open Mosaic_ui
open Test_harness

(* ── Default Focusable ── *)

(* Box defaults to non-focusable, but input/select/tab_select default to
   focusable. This is important because getting it wrong means widgets silently
   don't receive keyboard input. *)

let box_not_focusable_by_default () =
  match Vnode.box [] with
  | Vnode.Element { attrs; _ } -> is_false ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

let input_focusable_by_default () =
  match Vnode.input () with
  | Vnode.Element { attrs; _ } -> is_true ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

let select_focusable_by_default () =
  match Vnode.select () with
  | Vnode.Element { attrs; _ } -> is_true ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

let tab_select_focusable_by_default () =
  match Vnode.tab_select ~options:[] () with
  | Vnode.Element { attrs; _ } -> is_true ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

(* ── Leaf Constraints ── *)

(* Text, slider, input, select, tab_select, and canvas are leaves — they must
   not accept children. Box is the only container. Verifying this here catches
   accidental regressions if a constructor signature changes. *)

let text_has_no_children () =
  match Vnode.text "hello" with
  | Vnode.Element { children = []; _ } -> ()
  | _ -> fail "expected no children"

let box_carries_children () =
  let child = Vnode.text "child" in
  match Vnode.box [ child ] with
  | Vnode.Element { children = [ c ]; _ } -> is_true ~msg:"child" (c == child)
  | _ -> fail "expected one child"

(* ── Map — Physical Identity Sharing ── *)

(* The reconciler uses physical equality (==) on attrs and kind as a fast path
   to skip diffing. If map allocates new records, the reconciler degrades to
   full diffing on every render. These tests protect that optimization. *)

let map_preserves_attrs_identity () =
  let v = Vnode.box ~visible:false [] in
  let mapped = Vnode.map (fun x -> x) v in
  match (v, mapped) with
  | Vnode.Element orig, Vnode.Element result ->
      is_true ~msg:"attrs shared" (orig.attrs == result.attrs)
  | _ -> fail "expected Element pair"

let map_preserves_kind_identity () =
  let v = Vnode.box ~border:true [] in
  let mapped = Vnode.map (fun x -> x) v in
  match (v, mapped) with
  | Vnode.Element orig, Vnode.Element result ->
      is_true ~msg:"kind shared" (orig.kind == result.kind)
  | _ -> fail "expected Element pair"

(* ── Map — Handler Transformation ── *)

(* map must transform handler return values so the runtime can convert user
   messages to unit. Verify the transformed handler actually calls f. *)

let map_transforms_on_key () =
  let v : int Vnode.t = Vnode.box ~on_key:(fun _ -> 42) [] in
  let log = ref [] in
  let mapped : unit Vnode.t = Vnode.map (fun n -> log := n :: !log) v in
  match mapped with
  | Vnode.Element { handlers = { on_key = Some handler; _ }; _ } ->
      let fake_ev = Event.Key.of_input (Input.Key.of_char 'a') in
      handler fake_ev;
      equal ~msg:"handler ran" (list int) [ 42 ] !log
  | _ -> fail "expected on_key handler"

let map_transforms_widget_callbacks () =
  let v : int Vnode.t =
    Vnode.slider ~on_value_change:(fun v -> truncate v) ()
  in
  let log = ref [] in
  let mapped : unit Vnode.t = Vnode.map (fun n -> log := n :: !log) v in
  match mapped with
  | Vnode.Element
      { callbacks = Vnode.Slider_callbacks { on_value_change = Some cb }; _ } ->
      cb 3.14;
      equal ~msg:"callback ran" (list int) [ 3 ] !log
  | _ -> fail "expected Slider_callbacks with on_value_change"

let map_transforms_input_on_cursor_callback () =
  let v : int Vnode.t =
    Vnode.input ~on_cursor:(fun ~cursor ~selection:_ -> cursor) ()
  in
  let log = ref [] in
  let mapped : unit Vnode.t = Vnode.map (fun n -> log := n :: !log) v in
  match mapped with
  | Vnode.Element
      { callbacks = Vnode.Input_callbacks { on_cursor = Some cb; _ }; _ } ->
      cb ~cursor:7 ~selection:None;
      equal ~msg:"on_cursor mapped" (list int) [ 7 ] !log
  | _ -> fail "expected Input_callbacks with on_cursor"

let map_transforms_code_on_selection_callback () =
  let v : int Vnode.t =
    Vnode.code ~on_selection:(function Some (a, b) -> a + b | None -> 0) "x"
  in
  let log = ref [] in
  let mapped : unit Vnode.t = Vnode.map (fun n -> log := n :: !log) v in
  match mapped with
  | Vnode.Element
      { callbacks = Vnode.Code_callbacks { on_selection = Some cb }; _ } ->
      cb (Some (2, 3));
      equal ~msg:"on_selection mapped" (list int) [ 5 ] !log
  | _ -> fail "expected Code_callbacks with on_selection"

(* map on Embed must return the same node unchanged — embeds have no handlers to
   transform. *)
let map_embed_identity () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let v = Vnode.embed child in
  match Vnode.map (fun x -> x) v with
  | Vnode.Embed n -> is_true ~msg:"same node" (n == child)
  | _ -> fail "expected Embed"

let spinner_not_focusable_by_default () =
  match Vnode.spinner () with
  | Vnode.Element { attrs; _ } -> is_false ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

let spinner_live_by_default () =
  match Vnode.spinner () with
  | Vnode.Element { attrs; _ } -> is_true ~msg:"live" attrs.live
  | _ -> fail "expected Element"

let spinner_has_no_children () =
  match Vnode.spinner () with
  | Vnode.Element { children = []; _ } -> ()
  | _ -> fail "expected no children"

let progress_bar_not_focusable_by_default () =
  match Vnode.progress_bar () with
  | Vnode.Element { attrs; _ } -> is_false ~msg:"focusable" attrs.focusable
  | _ -> fail "expected Element"

let progress_bar_not_live_by_default () =
  match Vnode.progress_bar () with
  | Vnode.Element { attrs; _ } -> is_false ~msg:"live" attrs.live
  | _ -> fail "expected Element"

let progress_bar_has_no_children () =
  match Vnode.progress_bar () with
  | Vnode.Element { children = []; _ } -> ()
  | _ -> fail "expected no children"

(* ── Runner ── *)

let () =
  run "mosaic.vnode"
    [
      group "Default focusable"
        [
          test "box not focusable" box_not_focusable_by_default;
          test "input focusable" input_focusable_by_default;
          test "select focusable" select_focusable_by_default;
          test "tab_select focusable" tab_select_focusable_by_default;
          test "spinner not focusable" spinner_not_focusable_by_default;
          test "progress_bar not focusable"
            progress_bar_not_focusable_by_default;
        ];
      group "Leaf constraints"
        [
          test "text has no children" text_has_no_children;
          test "box carries children" box_carries_children;
          test "spinner has no children" spinner_has_no_children;
          test "progress_bar has no children" progress_bar_has_no_children;
        ];
      group "Live defaults"
        [
          test "spinner live by default" spinner_live_by_default;
          test "progress_bar not live by default"
            progress_bar_not_live_by_default;
        ];
      group "Map identity sharing"
        [
          test "preserves attrs identity" map_preserves_attrs_identity;
          test "preserves kind identity" map_preserves_kind_identity;
        ];
      group "Map transformation"
        [
          test "transforms on_key" map_transforms_on_key;
          test "transforms widget callbacks" map_transforms_widget_callbacks;
          test "transforms input on_cursor callback"
            map_transforms_input_on_cursor_callback;
          test "transforms code on_selection callback"
            map_transforms_code_on_selection_callback;
          test "embed unchanged" map_embed_identity;
        ];
    ]
