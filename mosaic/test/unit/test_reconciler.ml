open Mosaic_ui
open Mosaic
open Windtrap

(* ── Helpers ── *)

let make () =
  let renderer = Renderer.create () in
  let container = Renderer.root renderer in
  let reconciler = Reconciler.create ~container in
  (renderer, reconciler)

let do_frame ?(width = 40) ?(height = 20) renderer =
  Renderer.render_frame renderer ~width ~height ~delta:0.

let children_of renderer = Renderable.children (Renderer.root renderer)
let child_count renderer = List.length (children_of renderer)

(* ── Construction ── *)

let create_returns_container () =
  let renderer, reconciler = make () in
  is_true ~msg:"container is root"
    (Reconciler.container reconciler == Renderer.root renderer)

(* ── Basic Rendering ── *)

let render_single_box () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box []);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_text () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.text "hello");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_empty () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let render_fragment () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer)

let render_nested_fragments () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.fragment
       [ Vnode.fragment [ Vnode.text "a"; Vnode.text "b" ]; Vnode.text "c" ]);
  do_frame renderer;
  equal ~msg:"three children" int 3 (child_count renderer)

(* ── Re-rendering (Update in Place) ── *)

let rerender_reuses_box () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box []);
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.box ~border:true []);
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_text () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.text "hello");
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.text "world");
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_updates_visibility () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box ~visible:true []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  is_true ~msg:"visible before" (Renderable.visible node);
  Reconciler.render reconciler (Vnode.box ~visible:false []);
  do_frame renderer;
  is_false ~msg:"hidden after" (Renderable.visible node)

let rerender_updates_z_index () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box ~z_index:0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  equal ~msg:"z_index before" int 0 (Renderable.z_index node);
  Reconciler.render reconciler (Vnode.box ~z_index:5 []);
  do_frame renderer;
  equal ~msg:"z_index after" int 5 (Renderable.z_index node)

let rerender_updates_opacity () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box ~opacity:1.0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.box ~opacity:0.5 []);
  do_frame renderer;
  is_true ~msg:"opacity" (Float.equal (Renderable.opacity node) 0.5)

(* ── Keyed Reconciliation ── *)

let keyed_reorder () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let children_before = children_of renderer in
  let node_a = List.nth children_before 0 in
  let node_b = List.nth children_before 1 in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box ~key:"b" []; Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let children_after = children_of renderer in
  let first = List.nth children_after 0 in
  let second = List.nth children_after 1 in
  is_true ~msg:"b is first" (first == node_b);
  is_true ~msg:"a is second" (second == node_a)

let keyed_removal () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let node_b = List.nth (children_of renderer) 1 in
  Reconciler.render reconciler (Vnode.fragment [ Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"b survives" (List.hd (children_of renderer) == node_b)

let keyed_addition () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.fragment [ Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let node_a = List.hd (children_of renderer) in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  is_true ~msg:"a survives" (List.nth (children_of renderer) 0 == node_a)

let unkeyed_positional () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "x" ]);
  do_frame renderer;
  let box_node = List.nth (children_of renderer) 0 in
  let text_node = List.nth (children_of renderer) 1 in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box ~border:true []; Vnode.text "y" ]);
  do_frame renderer;
  is_true ~msg:"box reused" (List.nth (children_of renderer) 0 == box_node);
  is_true ~msg:"text reused" (List.nth (children_of renderer) 1 == text_node)

(* ── Kind Mismatch ── *)

let kind_mismatch_destroys_old () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box []);
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.text "replaced");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  is_true ~msg:"new is different" (List.hd (children_of renderer) != old_node)

let kind_mismatch_text_to_slider () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.text "hello");
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.slider ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_spinner () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.spinner ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_progress_bar () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.progress_bar ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let rerender_reuses_spinner () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.spinner ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.spinner ~frame_set:Spinner.line ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_progress_bar () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.progress_bar ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.progress_bar ~value:0.5 ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let kind_mismatch_spinner_to_progress_bar () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.spinner ());
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  Reconciler.render reconciler (Vnode.progress_bar ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

(* ── Children ── *)

let fewer_children_destroys_extra () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.box [ Vnode.text "a"; Vnode.text "b"; Vnode.text "c" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"three kids" int 3 (List.length (Renderable.children box_node));
  let old_c = List.nth (Renderable.children box_node) 2 in
  Reconciler.render reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  is_true ~msg:"c destroyed" (Renderable.destroyed old_c)

let more_children_creates_new () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  Reconciler.render reconciler (Vnode.box [ Vnode.text "a"; Vnode.text "b" ]);
  do_frame renderer;
  equal ~msg:"two kids" int 2 (List.length (Renderable.children box_node))

let nested_boxes () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box [ Vnode.box [ Vnode.text "inner" ] ]);
  do_frame renderer;
  let outer = List.hd (children_of renderer) in
  equal ~msg:"outer has 1 child" int 1 (List.length (Renderable.children outer));
  let inner = List.hd (Renderable.children outer) in
  equal ~msg:"inner has 1 child" int 1 (List.length (Renderable.children inner))

(* ── Embed ── *)

let embed_attaches () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  Reconciler.render reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let embed_removed_detaches () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  Reconciler.render reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"attached" int 1 (child_count renderer);
  Reconciler.render reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"detached" int 0 (child_count renderer)

(* ── Callbacks ── *)

let ref_fires_on_creation () =
  let _renderer, reconciler = make () in
  let received = ref None in
  Reconciler.render reconciler (Vnode.box ~ref:(fun n -> received := Some n) []);
  is_some ~msg:"ref called" !received

let ref_receives_correct_node () =
  let renderer, reconciler = make () in
  let received = ref None in
  Reconciler.render reconciler (Vnode.box ~ref:(fun n -> received := Some n) []);
  do_frame renderer;
  match !received with
  | Some n -> is_true ~msg:"same node" (n == List.hd (children_of renderer))
  | None -> fail "ref not called"

let on_key_handler_fires () =
  let renderer, reconciler = make () in
  let received = ref false in
  Reconciler.render reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> received := true) []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'a') : Event.key);
  is_true ~msg:"handler fired" !received

let on_key_handler_updates_on_rerender () =
  let renderer, reconciler = make () in
  let log = ref [] in
  Reconciler.render reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> log := "first" :: !log) []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'a') : Event.key);
  equal ~msg:"first handler" (list string) [ "first" ] !log;
  Reconciler.render reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> log := "second" :: !log) []);
  ignore (Renderer.dispatch_key renderer (Input.Key.of_char 'b') : Event.key);
  equal ~msg:"second handler" (list string) [ "second"; "first" ] !log

(* ── Unmount ── *)

let unmount_empties_container () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler
    (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let unmount_destroys_nodes () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.box []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  Reconciler.unmount reconciler;
  is_true ~msg:"destroyed" (Renderable.destroyed node)

let unmount_allows_reuse () =
  let renderer, reconciler = make () in
  Reconciler.render reconciler (Vnode.text "first");
  do_frame renderer;
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"empty after unmount" int 0 (child_count renderer);
  Reconciler.render reconciler (Vnode.text "second");
  do_frame renderer;
  equal ~msg:"one child after reuse" int 1 (child_count renderer)

(* ── Runner ── *)

let () =
  run "mosaic.reconciler"
    [
      group "Construction"
        [ test "container returns root" create_returns_container ];
      group "Basic rendering"
        [
          test "single box" render_single_box;
          test "single text" render_single_text;
          test "empty" render_empty;
          test "fragment" render_fragment;
          test "nested fragments" render_nested_fragments;
          test "single spinner" render_single_spinner;
          test "single progress_bar" render_single_progress_bar;
        ];
      group "Re-rendering"
        [
          test "reuses box" rerender_reuses_box;
          test "reuses text" rerender_reuses_text;
          test "reuses spinner" rerender_reuses_spinner;
          test "reuses progress_bar" rerender_reuses_progress_bar;
          test "updates visibility" rerender_updates_visibility;
          test "updates z_index" rerender_updates_z_index;
          test "updates opacity" rerender_updates_opacity;
        ];
      group "Keyed reconciliation"
        [
          test "reorder" keyed_reorder;
          test "removal" keyed_removal;
          test "addition" keyed_addition;
          test "unkeyed positional" unkeyed_positional;
        ];
      group "Kind mismatch"
        [
          test "box to text" kind_mismatch_destroys_old;
          test "text to slider" kind_mismatch_text_to_slider;
          test "spinner to progress_bar" kind_mismatch_spinner_to_progress_bar;
        ];
      group "Children"
        [
          test "fewer children destroys extra" fewer_children_destroys_extra;
          test "more children creates new" more_children_creates_new;
          test "nested boxes" nested_boxes;
        ];
      group "Embed"
        [
          test "attaches" embed_attaches;
          test "removed detaches" embed_removed_detaches;
        ];
      group "Callbacks"
        [
          test "ref fires on creation" ref_fires_on_creation;
          test "ref receives correct node" ref_receives_correct_node;
          test "on_key handler fires" on_key_handler_fires;
          test "on_key handler updates on rerender"
            on_key_handler_updates_on_rerender;
        ];
      group "Unmount"
        [
          test "empties container" unmount_empties_container;
          test "destroys nodes" unmount_destroys_nodes;
          test "allows reuse" unmount_allows_reuse;
        ];
    ]
