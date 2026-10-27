open Windtrap
open Mosaic_ui
open Test_harness

(* An element alone in a renderer's root, mounted by a reconciler, and the
   node it mounted. *)
let mount (view : unit Mosaic.t) =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  Mosaic.Reconciler.render reconciler ~viewport_width:80 view;
  match Renderable.children (Renderer.root renderer) with
  | [ node ] -> (renderer, reconciler, node)
  | _ -> fail "expected the element alone in the root"

let node view =
  let _, _, node = mount view in
  node

(* ── Default Focusable ── *)

(* Box defaults to non-focusable, but input/select/tab_select default to
   focusable. This is important because getting it wrong means widgets silently
   don't receive keyboard input. *)

let box_not_focusable_by_default () =
  is_false ~msg:"focusable" (Renderable.focusable (node (Mosaic.box [])))

let input_focusable_by_default () =
  is_true ~msg:"focusable"
    (Renderable.focusable
       (node (Mosaic.input ~state:Editor.empty ~on_change:ignore ())))

let select_focusable_by_default () =
  is_true ~msg:"focusable"
    (Renderable.focusable
       (node
          (Mosaic.select ~on_change:ignore ~equal:Int.equal ~selected:None [])))

let tab_select_focusable_by_default () =
  is_true ~msg:"focusable"
    (Renderable.focusable
       (node
          (Mosaic.tab_select ~on_change:ignore ~equal:Int.equal ~selected:None
             [])))

let spinner_not_focusable_by_default () =
  is_false ~msg:"focusable" (Renderable.focusable (node (Mosaic.spinner ())))

let progress_bar_not_focusable_by_default () =
  is_false ~msg:"focusable"
    (Renderable.focusable (node (Mosaic.progress_bar ())))

(* ── Leaf Constraints ── *)

(* Text, spinner and progress bar are leaves, and a box holds the children it
   is given. *)

let has_no_children view =
  equal ~msg:"no children" int 0 (List.length (Renderable.children (node view)))

let text_has_no_children () = has_no_children (Mosaic.text "hello")
let spinner_has_no_children () = has_no_children (Mosaic.spinner ())
let progress_bar_has_no_children () = has_no_children (Mosaic.progress_bar ())

let box_carries_children () =
  equal ~msg:"one child" int 1
    (List.length
       (Renderable.children (node (Mosaic.box [ Mosaic.text "child" ]))))

(* ── Live Defaults ── *)

let spinner_live_by_default () =
  is_true ~msg:"live" (Renderable.live (node (Mosaic.spinner ())))

let progress_bar_not_live_by_default () =
  is_false ~msg:"live" (Renderable.live (node (Mosaic.progress_bar ())))

(* ── Layout ── *)

(* A layout label sets its field of [~layout] and keeps the others, including
   the fields no label sets. *)
let layout_label_overrides_one_field () =
  let layout =
    Toffee.Style.(
      default
      |> set_flex_direction Flex_direction.Column
      |> set_flex_grow 1.
      |> set_box_generation_mode Box_generation_mode.None)
  in
  let style = Renderable.style (node (Mosaic.box ~layout ~flex_grow:2. [])) in
  is_true ~msg:"the label's field" (Toffee.Style.flex_grow style = 2.);
  is_true ~msg:"the layout's direction"
    (Toffee.Style.flex_direction style = Toffee.Style.Flex_direction.Column);
  is_true ~msg:"the layout's box generation mode"
    (Toffee.Style.box_generation_mode style
    = Toffee.Style.Box_generation_mode.None);
  is_true ~msg:"the layout itself is unchanged"
    (Toffee.Style.flex_grow layout = 1.)

(* ── Map ── *)

(* A layout holding a NaN is unequal to itself structurally, so only a
   physical comparison finds it unchanged. Mapping a view keeps its elements'
   layouts, so an element declared again under [map] is not laid out again. *)
let map_keeps_layout () =
  let layout = Toffee.Style.(default |> set_flex_grow Float.nan) in
  let view = Mosaic.box ~layout [] in
  let renderer, reconciler, node = mount (Mosaic.map ignore view) in
  Renderer.render_frame renderer ~width:20 ~height:5 ~dt:0.;
  Renderable.Private.clear_layout_dirty node;
  Mosaic.Reconciler.render reconciler ~viewport_width:80
    (Mosaic.map ignore view);
  is_false ~msg:"not laid out again" (Renderable.Private.layout_dirty node)

(* map must transform handler return values so the runtime can convert user
   messages to unit. Each test mounts a mapped view and drives its widget, and
   checks that the messages reach the mapper. *)

let map_transforms_on_key () =
  let view : int Mosaic.t =
    Mosaic.box ~focused:true
      ~on_key:(fun event ->
        if
          (Event.Key.data event).key
          = Matrix_input.Keystroke.Char (Uchar.of_char 'a')
        then Some 42
        else None)
      []
  in
  let log = ref [] in
  let renderer, reconciler, node =
    mount (Mosaic.map (fun n -> log := n :: !log) view)
  in
  ignore (Mosaic.Reconciler.focused reconciler : Renderable.t option);
  ignore (Renderer.focus renderer node : bool);
  ignore (dispatch_key renderer (char_key 'a') : Event.key);
  ignore (dispatch_key renderer (char_key 'b') : Event.key);
  equal ~msg:"only present messages reach the mapper" (list int) [ 42 ] !log

let map_transforms_widget_callbacks () =
  let proposed = ref [] in
  let view : int Mosaic.t =
    Mosaic.slider
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~min:0. ~max:10. ~state:0.
      ~on_change:(fun v ->
        proposed := v :: !proposed;
        truncate v)
      ()
  in
  let log = ref [] in
  let renderer, _, node = mount (Mosaic.map (fun n -> log := n :: !log) view) in
  Renderer.render_frame renderer ~width:20 ~height:5 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let x = Renderable.x node + 5 and y = Renderable.y node in
  ignore (dispatch_mouse renderer (mouse_report ~x ~y (Press Left)));
  is_true ~msg:"the slider proposed a value" (!proposed <> []);
  equal ~msg:"callback ran" (list int) (List.map truncate !proposed) !log

let map_transforms_input_on_change_callback () =
  let view : int Mosaic.t =
    Mosaic.input ~focused:true ~state:Editor.empty ~on_change:Editor.cursor ()
  in
  let log = ref [] in
  let renderer, _, node = mount (Mosaic.map (fun n -> log := n :: !log) view) in
  ignore (Renderer.focus renderer node : bool);
  ignore (dispatch_key renderer (char_key 'x') : Event.key);
  equal ~msg:"complete editor mapped" (list int) [ 1 ] !log

(* Drags over the widget [element reported] mounts under [map], and checks
   that each selection the widget reported reached the mapper as the message
   its callback made of it. *)
let selected_by_drag ~message element =
  let reported = ref [] and log = ref [] in
  let report selection =
    reported := selection :: !reported;
    Some (message selection)
  in
  ignore
    (through_element
       ~act:(fun renderer node -> select_text renderer node)
       ~observe:(fun _ _ -> "")
       (Mosaic.map (fun n -> log := n :: !log) (element report))
      : string);
  is_true ~msg:"the widget reported a selection" (!reported <> []);
  equal ~msg:"on_selection mapped" (list int) (List.map message !reported) !log

let map_transforms_code_on_selection_callback () =
  selected_by_drag
    ~message:(function Some (a, b) -> a + b | None -> 0)
    (fun on_selection -> Mosaic.code ~on_selection "hello world")

let map_transforms_markdown_on_selection_callback () =
  selected_by_drag
    ~message:(function Some text -> String.length text | None -> 0)
    (fun on_selection -> Mosaic.markdown ~on_selection "hello world")

(* map on an embed keeps the node: embeds have no handlers to transform. *)
let map_embed_identity () =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let child = Renderable.create ~parent:(Renderer.root renderer) () in
  Mosaic.Reconciler.render reconciler ~viewport_width:80
    (Mosaic.map Fun.id (Mosaic.embed child));
  match Renderable.children (Renderer.root renderer) with
  | [ node ] -> is_true ~msg:"same node" (node == child)
  | _ -> fail "expected the embedded node alone in the root"

let viewport_switch_maps_both_branches () =
  let view : int Mosaic.t =
    Mosaic.viewport_switch ~at_least_width:80
      ~wide:(Mosaic.box ~focused:true ~on_key:(fun _ -> Some 1) [])
      ~narrow:(Mosaic.box ~focused:true ~on_key:(fun _ -> Some 2) [])
  in
  let received = ref [] in
  let mapped =
    Mosaic.map (fun message -> received := message :: !received) view
  in
  let press viewport_width =
    let renderer = Renderer.create () in
    let reconciler =
      Mosaic.Reconciler.create ~container:(Renderer.root renderer)
        ~deliver:ignore
    in
    Mosaic.Reconciler.render reconciler ~viewport_width mapped;
    match Mosaic.Reconciler.focused reconciler with
    | Some node ->
        ignore (Renderer.focus renderer node : bool);
        ignore (dispatch_key renderer (char_key 'a') : Event.key)
    | None -> fail "expected the branch focused"
  in
  press 80;
  press 79;
  equal ~msg:"both branch handlers mapped" (list int) [ 2; 1 ] !received

(* A memo under a map whose function captures a value that changes each
   render: the map is a node whose function is stored at its place, so the
   memo below it is equal and built once, and its messages go through the
   map's newest function, as Elm applies its taggers when an event fires. *)
let memo_under_capturing_map () =
  let builds = ref 0 and delivered = ref [] in
  let item key =
    incr builds;
    Mosaic.box ~focused:true ~on_key:(fun _ -> Some key) []
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
      ~deliver:(fun message -> delivered := message :: !delivered)
  in
  let render generation =
    Mosaic.Reconciler.render reconciler ~viewport_width:80
      (Mosaic.map
         (fun key -> Printf.sprintf "%s/%d" key generation)
         (Mosaic.memo ~key:"item" item));
    match Mosaic.Reconciler.focused reconciler with
    | Some node ->
        ignore (Renderer.focus renderer node : bool);
        ignore (dispatch_key renderer (char_key 'a') : Event.key)
    | None -> fail "expected the item focused"
  in
  render 1;
  render 2;
  render 3;
  equal ~msg:"built once" int 1 !builds;
  equal ~msg:"delivered through the newest function" (list string)
    [ "item/3"; "item/2"; "item/1" ]
    !delivered

(* ── Viewport Switch ── *)

let viewport_switch_rejects_nonpositive_threshold () =
  raises_match ~msg:"positive threshold"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Mosaic.viewport_switch ~at_least_width:0 ~wide:Mosaic.empty
           ~narrow:Mosaic.empty
          : unit Mosaic.t))

(* ── Runner ── *)

let () =
  run "mosaic.elements"
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
      group "Layout"
        [
          test "a label overrides one field of the layout"
            layout_label_overrides_one_field;
        ];
      group "Map identity sharing"
        [ test "keeps an element's layout" map_keeps_layout ];
      group "Map transformation"
        [
          test "transforms on_key" map_transforms_on_key;
          test "transforms widget callbacks" map_transforms_widget_callbacks;
          test "transforms complete input callback"
            map_transforms_input_on_change_callback;
          test "transforms code on_selection callback"
            map_transforms_code_on_selection_callback;
          test "transforms markdown on_selection callback"
            map_transforms_markdown_on_selection_callback;
          test "embed unchanged" map_embed_identity;
          test "viewport switch maps both branches"
            viewport_switch_maps_both_branches;
        ];
      group "Map node"
        [ test "a memo under a capturing map" memo_under_capturing_map ];
      group "Viewport switch"
        [
          test "rejects nonpositive threshold"
            viewport_switch_rejects_nonpositive_threshold;
        ];
    ]
