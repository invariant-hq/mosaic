open Windtrap
open Mosaic_ui
open Test_harness

let key_ev_a = Test_harness.char_key 'a'

(* ── Construction ── *)

let create_default_values () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  starts_with ~affix:"node-" (Renderable.id child);
  is_true ~msg:"visible" (Renderable.visible child);
  equal ~msg:"z_index" int 0 (Renderable.z_index child);
  is_true ~msg:"opacity" (Float.equal 1.0 (Renderable.opacity child));
  is_false ~msg:"live" (Renderable.live child);
  is_false ~msg:"focusable" (Renderable.focusable child);
  is_false ~msg:"focused" (Renderable.focused child)

let create_custom_id () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"my-node" () in
  equal ~msg:"id" string "my-node" (Renderable.id child)

let create_custom_z_index () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~z_index:5 () in
  equal ~msg:"z_index" int 5 (Renderable.z_index child)

let create_custom_opacity () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~opacity:0.5 () in
  is_true ~msg:"opacity" (Float.equal 0.5 (Renderable.opacity child))

let create_custom_live () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~live:true () in
  is_true ~msg:"live" (Renderable.live child)

let create_custom_visible () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~visible:false () in
  is_false ~msg:"visible" (Renderable.visible child)

let create_attaches_to_parent () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  (match Renderable.parent child with
  | Some p ->
      equal ~msg:"parent id" string (Renderable.id root) (Renderable.id p)
  | None -> fail "expected parent");
  let ids = List.map Renderable.id (Renderable.children root) in
  is_true ~msg:"in children" (List.mem (Renderable.id child) ids)

let create_follows_child_target () =
  let t = make_ctx () in
  let root = make_root t in
  let container = Renderable.create ~parent:root ~id:"container" () in
  Renderable.set_child_target root (Some container);
  let child = Renderable.create ~parent:root ~id:"child" () in
  (match Renderable.parent child with
  | Some p ->
      equal ~msg:"parent is container" string "container" (Renderable.id p)
  | None -> fail "expected parent");
  Renderable.set_child_target root None

let set_child_target_non_descendant_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let container = Renderable.create ~parent:root ~id:"container" () in
  let sibling = Renderable.create ~parent:root ~id:"sibling" () in
  raises_match ~msg:"requires descendant target"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Renderable.set_child_target container (Some sibling))

let set_child_target_different_tree_raises () =
  let t1 = make_ctx () in
  let t2 = make_ctx () in
  let root1 = make_root t1 in
  let root2 = make_root t2 in
  raises_match ~msg:"requires same tree"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Renderable.set_child_target root1 (Some root2))

let set_child_target_destroyed_target_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let container = Renderable.create ~parent:root ~id:"container" () in
  let content = Renderable.create ~parent:container ~id:"content" () in
  Renderable.destroy content;
  raises_match ~msg:"rejects destroyed target"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Renderable.set_child_target container (Some content))

let create_destroyed_parent_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root () in
  Renderable.destroy parent;
  raises_match ~msg:"raises on destroyed"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Renderable.create ~parent () : Renderable.t))

let create_with_index () =
  let t = make_ctx () in
  let root = make_root t in
  let a = Renderable.create ~parent:root ~id:"a" () in
  let c = Renderable.create ~parent:root ~id:"c" () in
  let b = Renderable.create ~parent:root ~id:"b" ~index:1 () in
  let ids = List.map Renderable.id (Renderable.children root) in
  equal ~msg:"order" (list string)
    [ Renderable.id a; Renderable.id b; Renderable.id c ]
    ids

(* ── Hierarchy ── *)

let children_insertion_order () =
  let t = make_ctx () in
  let root = make_root t in
  let a = Renderable.create ~parent:root ~id:"a" () in
  let b = Renderable.create ~parent:root ~id:"b" () in
  let c = Renderable.create ~parent:root ~id:"c" () in
  let ids = List.map Renderable.id (Renderable.children root) in
  equal ~msg:"order" (list string)
    [ Renderable.id a; Renderable.id b; Renderable.id c ]
    ids

let attach_moves_between_parents () =
  let t = make_ctx () in
  let root1 = make_root ~style:Toffee.Style.default t in
  let root2 = make_root ~style:Toffee.Style.default t in
  let child = Renderable.create ~parent:root1 ~id:"child" () in
  Renderable.attach ~parent:root2 child;
  let r1_ids = List.map Renderable.id (Renderable.children root1) in
  let r2_ids = List.map Renderable.id (Renderable.children root2) in
  is_false ~msg:"removed from root1" (List.mem "child" r1_ids);
  is_true ~msg:"added to root2" (List.mem "child" r2_ids)

let detach_removes () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.detach child;
  is_none ~msg:"no parent" (Renderable.parent child);
  equal ~msg:"empty children" (list string) []
    (List.map Renderable.id (Renderable.children root))

let detach_blurs_focused () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  is_true ~msg:"focused before" (Renderable.focused child);
  Renderable.detach child;
  is_false ~msg:"blurred after" (Renderable.focused child)

let detach_parent_blurs_focused_descendant () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"parent" () in
  let child = Renderable.create ~parent ~id:"child" () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  is_true ~msg:"focused before" (Renderable.focused child);
  Renderable.detach parent;
  is_false ~msg:"blurred descendant" (Renderable.focused child);
  is_true ~msg:"blur delegated for child" (List.mem "child" !(t.blur_log))

let attach_self_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  raises_match ~msg:"attach to self"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Renderable.attach ~parent:child child)

let attach_destroyed_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.destroy child;
  raises_match ~msg:"attach destroyed"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Renderable.attach ~parent:root child)

(* ── Destroy ── *)

let destroy_marks_destroyed () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  is_false ~msg:"before" (Renderable.destroyed child);
  Renderable.destroy child;
  is_true ~msg:"after" (Renderable.destroyed child)

let destroy_detaches () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.destroy child;
  is_none ~msg:"no parent" (Renderable.parent child);
  equal ~msg:"empty" (list string) []
    (List.map Renderable.id (Renderable.children root))

let destroy_parent_blurs_focused_descendant () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"parent" () in
  let child = Renderable.create ~parent ~id:"child" () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  is_true ~msg:"focused before" (Renderable.focused child);
  Renderable.destroy parent;
  is_false ~msg:"blurred descendant" (Renderable.focused child);
  is_true ~msg:"blur delegated for child" (List.mem "child" !(t.blur_log))

let destroy_unregisters_node_once () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"child" () in
  Renderable.destroy child;
  let count_once =
    List.length
      (List.filter (fun id -> String.equal id "child") !(t.unregister_log))
  in
  equal ~msg:"unregister called once" int 1 count_once;
  Renderable.destroy child;
  let count_twice =
    List.length
      (List.filter (fun id -> String.equal id "child") !(t.unregister_log))
  in
  equal ~msg:"still once after idempotent destroy" int 1 count_twice

let destroy_clears_handlers () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let key_fired = ref false in
  let mouse_fired = ref false in
  let paste_fired = ref false in
  Renderable.on_key child (fun _ -> key_fired := true);
  Renderable.on_mouse child (fun _ -> mouse_fired := true);
  Renderable.on_paste child (fun _ -> paste_fired := true);
  Renderable.destroy child;
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  Renderable.Private.emit_mouse child
    (Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier Move);
  Renderable.Private.emit_paste child (Event.Paste.of_text "x");
  is_false ~msg:"key" !key_fired;
  is_false ~msg:"mouse" !mouse_fired;
  is_false ~msg:"paste" !paste_fired

let destroy_idempotent () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.destroy child;
  Renderable.destroy child;
  is_true ~msg:"still destroyed" (Renderable.destroyed child)

let create_on_destroyed_raises () =
  let t = make_ctx () in
  let root = make_root t in
  let node = Renderable.create ~parent:root () in
  Renderable.destroy node;
  raises_match ~msg:"raises"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Renderable.create ~parent:node () : Renderable.t))

let destroy_recursively_all () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"p" () in
  let child_a = Renderable.create ~parent ~id:"a" () in
  let child_b = Renderable.create ~parent ~id:"b" () in
  let grandchild = Renderable.create ~parent:child_a ~id:"gc" () in
  Renderable.destroy_recursively parent;
  is_true ~msg:"parent destroyed" (Renderable.destroyed parent);
  is_true ~msg:"child_a destroyed" (Renderable.destroyed child_a);
  is_true ~msg:"child_b destroyed" (Renderable.destroyed child_b);
  is_true ~msg:"grandchild destroyed" (Renderable.destroyed grandchild);
  equal ~msg:"detached from root" (list string) []
    (List.map Renderable.id (Renderable.children root))

(* Destroying a node with many children destroys each descendant after its
   own descendants, unregisters it once, drops its handlers and clears its
   parent link. It blurs a focused descendant and removes the subtree from the
   live count and the layout tree. *)
let destroy_recursively_wide_subtree () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"p" () in
  let n = 2000 in
  let cleaned = ref [] in
  let cleanup node =
    Renderable.Private.set_cleanup node (fun () ->
        cleaned := Renderable.id node :: !cleaned)
  in
  let pairs =
    List.init n (fun i ->
        let child =
          Renderable.create ~parent ~id:(Printf.sprintf "c%d" i)
            ~live:(i mod 3 = 0)
            ()
        in
        let grandchild =
          Renderable.create ~parent:child ~id:(Printf.sprintf "g%d" i) ()
        in
        cleanup child;
        cleanup grandchild;
        (child, grandchild))
  in
  cleanup parent;
  let child, grandchild = List.nth pairs (n / 2) in
  let key_fired = ref false in
  Renderable.on_key child (fun _ -> key_fired := true);
  Renderable.set_focusable grandchild true;
  ignore (Renderable.focus grandchild : bool);
  equal ~msg:"live before" int
    ((n + 2) / 3)
    (Renderable.Private.live_count root);
  Renderable.destroy_recursively parent;
  let nodes = parent :: List.concat_map (fun (c, g) -> [ c; g ]) pairs in
  is_true ~msg:"all destroyed" (List.for_all Renderable.destroyed nodes);
  is_true ~msg:"no parent links"
    (List.for_all (fun node -> Option.is_none (Renderable.parent node)) nodes);
  is_true ~msg:"no children"
    (List.for_all (fun node -> List.is_empty (Renderable.children node)) nodes);
  equal ~msg:"each unregistered once" (list string)
    (List.sort String.compare (List.map Renderable.id nodes))
    (List.sort String.compare !(t.unregister_log));
  equal ~msg:"cleaned up after descendants" (list string)
    (List.concat_map
       (fun i -> [ Printf.sprintf "g%d" i; Printf.sprintf "c%d" i ])
       (List.init n Fun.id)
    @ [ "p" ])
    (List.rev !cleaned);
  is_false ~msg:"blurred" (Renderable.focused grandchild);
  equal ~msg:"blur delegated" (list string)
    [ Renderable.id grandchild ]
    !(t.blur_log);
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  is_false ~msg:"handlers dropped" !key_fired;
  equal ~msg:"live after" int 0 (Renderable.Private.live_count root);
  equal ~msg:"root has no children" (list string) []
    (List.map Renderable.id (Renderable.children root));
  equal ~msg:"layout tree holds the root alone" int 1
    (Toffee.total_node_count t.ctx.tree)

(* [destroy] orphans the children of a wide node, which can then be attached
   elsewhere, in the layout tree too. *)
let destroy_orphans_wide_children () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"p" () in
  let n = 2000 in
  let children =
    List.init n (fun i ->
        Renderable.create ~parent ~id:(Printf.sprintf "c%d" i)
          ~live:(i mod 2 = 0)
          ())
  in
  Renderable.destroy parent;
  is_false ~msg:"children survive" (List.exists Renderable.destroyed children);
  is_true ~msg:"orphaned"
    (List.for_all
       (fun child -> Option.is_none (Renderable.parent child))
       children);
  equal ~msg:"live after" int 0 (Renderable.Private.live_count root);
  List.iter (fun child -> Renderable.attach ~parent:root child) children;
  equal ~msg:"reattached" (list string)
    (List.map Renderable.id children)
    (List.map Renderable.id (Renderable.children root));
  equal ~msg:"live once reattached" int (n / 2)
    (Renderable.Private.live_count root);
  equal ~msg:"in the layout tree" int n
    (match Toffee.children t.ctx.tree (Renderable.Private.toffee_node root) with
    | Ok ids -> List.length ids
    | Error _ -> -1)

(* [detach_all] detaches nodes of several parents at once: the others keep
   their order in the nodes and in the layout tree, the live counts follow, a
   focused node is blurred, and a node already detached is left alone. *)
let detach_all_wide () =
  let t = make_ctx () in
  let root = make_root t in
  let first = Renderable.create ~parent:root ~id:"first" () in
  let second = Renderable.create ~parent:root ~id:"second" () in
  let make parent prefix n =
    List.init n (fun i ->
        Renderable.create ~parent
          ~id:(Printf.sprintf "%s%d" prefix i)
          ~live:true ())
  in
  let a = make first "a" 1000 and b = make second "b" 10 in
  let lone = Renderable.create ~parent:root () in
  Renderable.detach lone;
  let gone = List.filteri (fun i _ -> i mod 2 = 0) a @ [ List.nth b 3; lone ] in
  let focused = List.hd gone in
  Renderable.set_focusable focused true;
  ignore (Renderable.focus focused : bool);
  Renderable.Private.detach_all gone;
  is_false ~msg:"blurred" (Renderable.focused focused);
  is_true ~msg:"orphaned"
    (List.for_all (fun node -> Option.is_none (Renderable.parent node)) gone);
  let ids parent = List.map Renderable.id (Renderable.children parent) in
  let left nodes = List.filter (fun n -> not (List.memq n gone)) nodes in
  equal ~msg:"first keeps the rest" (list string)
    (List.map Renderable.id (left a))
    (ids first);
  equal ~msg:"second keeps the rest" (list string)
    (List.map Renderable.id (left b))
    (ids second);
  let layout parent =
    match
      Toffee.children t.ctx.tree (Renderable.Private.toffee_node parent)
    with
    | Ok ids -> ids
    | Error _ -> []
  in
  is_true ~msg:"layout tree follows"
    (List.equal Toffee.Node_id.equal
       (List.map Renderable.Private.toffee_node (left a))
       (layout first));
  equal ~msg:"live count" int (500 + 9) (Renderable.Private.live_count root)

(* ── Visual Properties ── *)

let visible_toggle () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_visible child false;
  is_false ~msg:"hidden" (Renderable.visible child);
  Renderable.set_visible child true;
  is_true ~msg:"shown" (Renderable.visible child)

let visible_false_blurs () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  Renderable.set_visible child false;
  is_false ~msg:"blurred" (Renderable.focused child)

let visible_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_visible child true;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let set_z_index () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_z_index child 10;
  equal ~msg:"z_index" int 10 (Renderable.z_index child)

let z_index_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_z_index child 0;
  let before = !(t.schedule_count) in
  Renderable.set_z_index child 0;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let opacity_clamp () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_opacity child 2.0;
  is_true ~msg:"clamped to 1" (Float.equal 1.0 (Renderable.opacity child));
  Renderable.set_opacity child (-0.5);
  is_true ~msg:"clamped to 0" (Float.equal 0.0 (Renderable.opacity child));
  Renderable.set_opacity child 0.5;
  is_true ~msg:"0.5" (Float.equal 0.5 (Renderable.opacity child))

let opacity_noop_same_value () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_opacity child 1.0;
  equal ~msg:"no schedule" int before !(t.schedule_count)

let buffered_toggle () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_buffered child true;
  is_true ~msg:"on" (Renderable.buffered child);
  Renderable.set_buffered child false;
  is_false ~msg:"off" (Renderable.buffered child)

let live_toggle () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_live child true;
  is_true ~msg:"on" (Renderable.live child);
  Renderable.set_live child false;
  is_false ~msg:"off" (Renderable.live child)

let set_child_clip_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_child_clip child (Some (fun _ -> None));
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Focus ── *)

let focus_not_focusable () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  is_false ~msg:"returns false" (Renderable.focus child)

let focus_focusable () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_focusable child true;
  is_true ~msg:"returns true" (Renderable.focus child);
  is_true ~msg:"focused" (Renderable.focused child)

let focus_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"child" () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  is_true ~msg:"logged" (List.mem "child" !(t.focus_log))

let blur_removes_focus () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  Renderable.blur child;
  is_false ~msg:"not focused" (Renderable.focused child)

let blur_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"child" () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  Renderable.blur child;
  is_true ~msg:"logged" (List.mem "child" !(t.blur_log))

let set_focusable_false_blurs () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"child" () in
  Renderable.set_focusable child true;
  ignore (Renderable.focus child : bool);
  Renderable.set_focusable child false;
  is_false ~msg:"blurred when unfocusable" (Renderable.focused child);
  is_true ~msg:"blur delegated" (List.mem "child" !(t.blur_log))

let blur_noop_if_not_focused () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~id:"child" () in
  Renderable.blur child;
  is_false ~msg:"not logged" (List.mem "child" !(t.blur_log))

let cursor_with_provider () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let c : Renderable.cursor =
    { x = 1; y = 2; style = `Block; color = Ansi.Color.white; blinking = false }
  in
  Renderable.set_cursor_provider child (fun _ -> Some c);
  match Renderable.cursor child with
  | Some got -> is_true ~msg:"cursor matches" (Renderable.equal_cursor c got)
  | None -> fail "expected cursor"

let cursor_none_without_provider () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  is_none ~msg:"no cursor" (Renderable.cursor child)

let clear_cursor_provider () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_cursor_provider child (fun _ ->
      Some
        {
          x = 0;
          y = 0;
          style = `Line;
          color = Ansi.Color.white;
          blinking = true;
        });
  Renderable.clear_cursor_provider child;
  is_none ~msg:"cleared" (Renderable.cursor child)

let set_cursor_provider_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_cursor_provider child (fun _ -> None);
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let clear_cursor_provider_schedules_when_present () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_cursor_provider child (fun _ -> None);
  let before = !(t.schedule_count) in
  Renderable.clear_cursor_provider child;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

(* ── Key Events ── *)

let on_key_order () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let order = ref [] in
  Renderable.on_key child (fun _ -> order := "first" :: !order);
  Renderable.on_key child (fun _ -> order := "second" :: !order);
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  (* newest first, so "second" runs before "first" *)
  equal ~msg:"order" (list string) [ "first"; "second" ] !order

let emit_key_prevent_default_preserves_handlers () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let old_fired = ref false in
  Renderable.on_key child (fun _ -> old_fired := true);
  Renderable.on_key child (fun ev -> Event.Key.prevent_default ev);
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  is_true ~msg:"prevent_default only suppresses widget defaults" !old_fired

let emit_key_runs_handlers_after_stop_propagation () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let old_fired = ref false in
  Renderable.on_key child (fun _ -> old_fired := true);
  Renderable.on_key child Event.Key.stop_propagation;
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  is_true ~msg:"stopping never hides an event from its node's handlers"
    !old_fired

let run_key_behavior_runs () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let fired = ref false in
  Renderable.set_key_behavior child (Some (fun _ -> fired := true));
  Renderable.Private.run_key_behavior child (Event.Key.of_input key_ev_a);
  is_true ~msg:"fired" !fired

let emit_key_does_not_run_default () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let default_fired = ref false in
  Renderable.set_key_behavior child (Some (fun _ -> default_fired := true));
  Renderable.Private.emit_key child (Event.Key.of_input key_ev_a);
  is_false ~msg:"default not fired" !default_fired

(* ── Mouse Events ── *)

let on_mouse_accumulates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let count = ref 0 in
  Renderable.on_mouse child (fun _ -> incr count);
  Renderable.on_mouse child (fun _ -> incr count);
  Renderable.Private.emit_mouse child
    (Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier Move);
  equal ~msg:"both fired" int 2 !count

let emit_mouse_runs_only_the_node () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root ~id:"parent" () in
  let child = Renderable.create ~parent ~id:"child" () in
  let parent_fired = ref false and child_fired = ref false in
  Renderable.on_mouse parent (fun _ -> parent_fired := true);
  Renderable.on_mouse child (fun _ -> child_fired := true);
  Renderable.Private.emit_mouse child
    (Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier Move);
  is_true ~msg:"child fired" !child_fired;
  is_false ~msg:"the renderer, not the node, walks to the parent" !parent_fired

(* ── Paste Events ── *)

let paste_replaces () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let first_fired = ref false in
  let second_fired = ref false in
  Renderable.set_paste_behavior child (Some (fun _ -> first_fired := true));
  Renderable.set_paste_behavior child (Some (fun _ -> second_fired := true));
  Renderable.Private.run_paste_behavior child (Event.Paste.of_text "x");
  is_false ~msg:"first not fired" !first_fired;
  is_true ~msg:"second fired" !second_fired

let paste_none_clears () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let fired = ref false in
  Renderable.set_paste_behavior child (Some (fun _ -> fired := true));
  Renderable.set_paste_behavior child None;
  Renderable.Private.run_paste_behavior child (Event.Paste.of_text "x");
  is_false ~msg:"not fired" !fired

(* ── Live Count ── *)

let live_count_contributes () =
  let t = make_ctx () in
  let root = make_root t in
  let _child = Renderable.create ~parent:root ~live:true () in
  ge ~msg:"root live_count" ~than:1 (Renderable.Private.live_count root)

let live_count_hide_reduces () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~live:true () in
  let before = Renderable.Private.live_count root in
  Renderable.set_visible child false;
  let after = Renderable.Private.live_count root in
  lt ~msg:"live_count reduced" ~than:before after

let live_count_propagates () =
  let t = make_ctx () in
  let root = make_root t in
  let parent = Renderable.create ~parent:root () in
  let _child = Renderable.create ~parent ~live:true () in
  ge ~msg:"root sees child (live_count)" ~than:1
    (Renderable.Private.live_count root);
  ge ~msg:"parent sees child (live_count)" ~than:1
    (Renderable.Private.live_count parent)

let live_count_detach_adjusts () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~live:true () in
  let before = Renderable.Private.live_count root in
  Renderable.detach child;
  let after = Renderable.Private.live_count root in
  lt ~msg:"live_count decreased" ~than:before after

let live_count_attach_adjusts () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~live:true () in
  Renderable.detach child;
  let before = Renderable.Private.live_count root in
  Renderable.attach ~parent:root child;
  let after = Renderable.Private.live_count root in
  gt ~msg:"live_count increased" ~than:before after

let live_count_change_callback () =
  let t = make_ctx () in
  let root = make_root t in
  let seen = ref 0 in
  Renderable.Private.set_on_live_count_change root (Some (fun _ -> incr seen));
  let _child = Renderable.create ~parent:root ~live:true () in
  gt ~msg:"callback fired" ~than:0 !seen

(* ── Selection ── *)

let not_selectable_default () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  is_false ~msg:"not selectable" (Renderable.selectable child)

let set_selection_makes_selectable () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  is_true ~msg:"selectable" (Renderable.selectable child)

let unset_selection_clears () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  Renderable.unset_selection child;
  is_false ~msg:"not selectable" (Renderable.selectable child)

let should_start_selection_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_selection child
    ~should_start:(fun ~x ~y:_ -> x > 5)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  is_false ~msg:"x=3"
    (Renderable.Private.should_start_selection child ~x:3 ~y:0);
  is_true ~msg:"x=6" (Renderable.Private.should_start_selection child ~x:6 ~y:0)

let get_selected_text_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "hello");
  equal ~msg:"text" string "hello" (Renderable.Private.get_selected_text child)

let emit_selection_changed_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let received = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ ->
      received := true;
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  let result = Renderable.Private.emit_selection_changed child None in
  is_true ~msg:"callback ran" !received;
  is_true ~msg:"returned true" result

let emit_selection_changed_none_without_handler () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let result = Renderable.Private.emit_selection_changed child None in
  is_false ~msg:"returns false" result

let clear_selection_delegates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let cleared = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> cleared := true)
    ~get_text:(fun () -> "");
  Renderable.Private.clear_selection child;
  is_true ~msg:"cleared" !cleared

(* ── Lifecycle ── *)

let on_frame_runs () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root ~live:true () in
  let fired = ref false in
  Renderable.set_on_frame child (Some (fun _ ~dt:_ -> fired := true));
  Renderable.Private.pre_render_update child ~dt:16.0;
  is_true ~msg:"fired" !fired

let on_resize_fires () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let fired = ref false in
  Renderable.set_on_resize child (Some (fun _ -> fired := true));
  layout_node child ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.Private.pre_render_update child ~dt:16.0;
  is_true ~msg:"fired" !fired

(* ── Layout ── *)

let layout_defaults () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  equal ~msg:"x" int 0 (Renderable.x child);
  equal ~msg:"y" int 0 (Renderable.y child);
  equal ~msg:"width" int 0 (Renderable.width child);
  equal ~msg:"height" int 0 (Renderable.height child)

let layout_update () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:10 ~y:20 ~width:30 ~height:40;
  equal ~msg:"x" int 10 (Renderable.x child);
  equal ~msg:"y" int 20 (Renderable.y child);
  equal ~msg:"width" int 30 (Renderable.width child);
  equal ~msg:"height" int 40 (Renderable.height child)

let zero_layout_keeps_zero_extent () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.Private.update_layout child ~x:10.4 ~y:20.4 ~width:0. ~height:0.;
  equal ~msg:"width" int 0 (Renderable.width child);
  equal ~msg:"height" int 0 (Renderable.height child)

let adjacent_fractional_edges_share_cell_boundary () =
  let t = make_ctx () in
  let root = make_root t in
  let left = Renderable.create ~parent:root () in
  let right = Renderable.create ~parent:root () in
  let below = Renderable.create ~parent:root () in
  Renderable.Private.update_layout left ~x:0.6 ~y:0.6 ~width:3.6 ~height:3.6;
  Renderable.Private.update_layout right ~x:4.2 ~y:0.6 ~width:2.6 ~height:3.6;
  Renderable.Private.update_layout below ~x:0.6 ~y:4.2 ~width:3.6 ~height:2.6;
  equal ~msg:"horizontal shared edge" int (Renderable.x right)
    (Renderable.x left + Renderable.width left);
  equal ~msg:"vertical shared edge" int (Renderable.y below)
    (Renderable.y left + Renderable.height left)

let bounds_returns_clip_rect () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:5 ~y:10 ~width:20 ~height:15;
  let b = Renderable.bounds child in
  equal ~msg:"x" int 5 b.x;
  equal ~msg:"y" int 10 b.y;
  equal ~msg:"width" int 20 b.width;
  equal ~msg:"height" int 15 b.height

let translate_offsets () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:10 ~y:20 ~width:30 ~height:40;
  Renderable.set_translate child ~x:5 ~y:3;
  equal ~msg:"x" int 15 (Renderable.x child);
  equal ~msg:"y" int 23 (Renderable.y child);
  equal ~msg:"translation preserves width" int 30 (Renderable.width child);
  equal ~msg:"translation preserves height" int 40 (Renderable.height child)

let mark_dirty () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.Private.clear_layout_dirty child;
  is_false ~msg:"clean" (Renderable.Private.layout_dirty child);
  Renderable.mark_dirty child;
  is_true ~msg:"dirty" (Renderable.Private.layout_dirty child)

let hidden_zero_dimensions () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:0 ~y:0 ~width:10 ~height:5;
  Renderable.set_visible child false;
  equal ~msg:"width" int 0 (Renderable.width child);
  equal ~msg:"height" int 0 (Renderable.height child)

let set_style_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_style child Toffee.Style.default;
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let style_roundtrip () =
  let t = make_ctx () in
  let root = make_root t in
  let s = Toffee.Style.set_flex_grow 2.0 Toffee.Style.default in
  let child = Renderable.create ~parent:root ~style:s () in
  let got = Renderable.style child in
  is_true ~msg:"flex_grow" (Float.equal 2.0 (Toffee.Style.flex_grow got))

let set_style_display_none_updates_visible () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let hidden =
    Toffee.Style.set_display Toffee.Style.Display.None Toffee.Style.default
  in
  Renderable.set_style child hidden;
  is_false ~msg:"visible flag follows display:none" (Renderable.visible child)

let set_style_while_hidden_stays_hidden () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_visible child false;
  Renderable.set_style child Toffee.Style.default;
  is_false ~msg:"remains hidden" (Renderable.visible child);
  let display = Toffee.Style.display (Renderable.style child) in
  is_true ~msg:"effective style keeps display:none"
    (Toffee.Style.Display.is_none display)

let set_style_while_hidden_restores_on_show () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  Renderable.set_visible child false;
  let s = Toffee.Style.set_flex_grow 2.0 Toffee.Style.default in
  Renderable.set_style child s;
  Renderable.set_visible child true;
  is_true ~msg:"visible after show" (Renderable.visible child);
  let got = Renderable.style child in
  is_true ~msg:"restored style is preserved"
    (Float.equal 2.0 (Toffee.Style.flex_grow got))

(* ── Children z-order ── *)

let children_z_sorted () =
  let t = make_ctx () in
  let root = make_root t in
  let a = Renderable.create ~parent:root ~id:"a" ~z_index:3 () in
  let b = Renderable.create ~parent:root ~id:"b" ~z_index:1 () in
  let c = Renderable.create ~parent:root ~id:"c" ~z_index:2 () in
  let arr = Renderable.Private.children_z root in
  let ids = Array.to_list (Array.map Renderable.id arr) in
  equal ~msg:"z-sorted" (list string)
    [ Renderable.id b; Renderable.id c; Renderable.id a ]
    ids

let children_z_empty () =
  let t = make_ctx () in
  let root = make_root t in
  let arr = Renderable.Private.children_z root in
  equal ~msg:"empty" int 0 (Array.length arr)

let iter_children_z_visits_all () =
  let t = make_ctx () in
  let root = make_root t in
  let _a = Renderable.create ~parent:root ~id:"a" ~z_index:2 () in
  let _b = Renderable.create ~parent:root ~id:"b" ~z_index:1 () in
  let visited = ref [] in
  Renderable.Private.iter_children_z root (fun c ->
      visited := Renderable.id c :: !visited);
  equal ~msg:"count" int 2 (List.length !visited)

let children_in_viewport_filters_small_sets () =
  let t = make_ctx () in
  let root = make_root t in
  let length n = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  let placed ~id ~z_index ~x =
    let style =
      Toffee.Style.default
      |> Toffee.Style.set_position Absolute
      |> Toffee.Style.set_inset
           {
             left = length x;
             top = length 1;
             right = Toffee.Style.Length_percentage_auto.auto;
             bottom = Toffee.Style.Length_percentage_auto.auto;
           }
      |> Toffee.Style.set_width (Toffee.Style.Dimension.length 2.)
      |> Toffee.Style.set_height (Toffee.Style.Dimension.length 2.)
    in
    Renderable.create ~parent:root ~id ~z_index ~style ()
  in
  let _ = placed ~id:"near-high" ~z_index:3 ~x:1 in
  let _ = placed ~id:"near-low" ~z_index:1 ~x:2 in
  let _ = placed ~id:"far" ~z_index:0 ~x:30 in
  ignore
    (Toffee.compute_layout t.ctx.tree
       (Renderable.Private.toffee_node root)
       (Toffee.Geometry.Size.make
          (Toffee.Available_space.of_float 40.)
          (Toffee.Available_space.of_float 10.))
      : unit Toffee.result);
  layout_node root ~x:0 ~y:0 ~width:40 ~height:10;
  let visible =
    Renderable.Private.children_in_viewport ~parent:root
      ~viewport:{ x = 0; y = 0; width = 10; height = 5 }
  in
  equal ~msg:"visible children are filtered and z-sorted" (list string)
    [ "near-low"; "near-high" ]
    (List.map Renderable.id visible)

(* ── Render Hooks ── *)

let render_before_hook () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:0 ~y:0 ~width:10 ~height:5;
  let fired = ref false in
  Renderable.set_render_before child (Some (fun _ _ ~dt:_ -> fired := true));
  let grid = make_grid () in
  Renderable.Private.render_full child ~grid ~dt:16.0;
  is_true ~msg:"before hook ran" !fired

let render_after_hook () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:0 ~y:0 ~width:10 ~height:5;
  let fired = ref false in
  Renderable.set_render_after child (Some (fun _ _ ~dt:_ -> fired := true));
  let grid = make_grid () in
  Renderable.Private.render_full child ~grid ~dt:16.0;
  is_true ~msg:"after hook ran" !fired

let render_hook_order () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:0 ~y:0 ~width:10 ~height:5;
  let order = ref [] in
  Renderable.set_render_before child
    (Some (fun _ _ ~dt:_ -> order := "before" :: !order));
  Renderable.set_render child (fun _ _ ~dt:_ -> order := "render" :: !order);
  Renderable.set_render_after child
    (Some (fun _ _ ~dt:_ -> order := "after" :: !order));
  let grid = make_grid () in
  Renderable.Private.render_full child ~grid ~dt:16.0;
  equal ~msg:"order" (list string) [ "after"; "render"; "before" ] !order

let set_render_before_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_render_before child (Some (fun _ _ ~dt:_ -> ()));
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let set_render_after_schedules () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  let before = !(t.schedule_count) in
  Renderable.set_render_after child (Some (fun _ _ ~dt:_ -> ()));
  gt ~msg:"scheduled" ~than:before !(t.schedule_count)

let buffered_render_full_uses_local_coordinates () =
  let t = make_ctx () in
  let root = make_root t in
  let child = Renderable.create ~parent:root () in
  layout_node child ~x:2 ~y:1 ~width:5 ~height:3;
  Renderable.set_buffered child true;
  let seen = ref None in
  Renderable.set_render child (fun node _ ~dt:_ ->
      seen := Some (Renderable.x node, Renderable.y node));
  let grid = make_grid ~width:12 ~height:6 () in
  Renderable.Private.render_full child ~grid ~dt:0.;
  equal ~msg:"buffered render sees local coords"
    (option (pair int int))
    (Some (0, 0))
    !seen;
  equal ~msg:"x restored after render" int 2 (Renderable.x child);
  equal ~msg:"y restored after render" int 1 (Renderable.y child)

(* ── Runner ── *)

let mouse_unmount_stops_remaining_handlers () =
  let context = make_ctx () in
  let root = make_root context in
  let child = Renderable.create ~parent:root () in
  let default_ran = ref false in
  Renderable.on_mouse child (fun _ -> default_ran := true);
  Renderable.on_mouse child (fun _ -> Renderable.destroy child);
  Renderable.Private.emit_mouse child
    (Event.Mouse.make ~x:0 ~y:0 ~modifiers:Event.Mouse.no_modifier
       (Down { button = Left }));
  is_false ~msg:"remaining handlers cannot mutate an unmounted widget"
    !default_ran

let () =
  run "mosaic.renderable"
    [
      test "mouse unmount stops remaining handlers"
        mouse_unmount_stops_remaining_handlers;
      group "Construction"
        [
          test "default values" create_default_values;
          test "custom id" create_custom_id;
          test "custom z_index" create_custom_z_index;
          test "custom opacity" create_custom_opacity;
          test "custom live" create_custom_live;
          test "custom visible" create_custom_visible;
          test "attaches to parent" create_attaches_to_parent;
          test "follows child_target" create_follows_child_target;
          test "child_target rejects non-descendant"
            set_child_target_non_descendant_raises;
          test "child_target rejects different tree"
            set_child_target_different_tree_raises;
          test "child_target rejects destroyed target"
            set_child_target_destroyed_target_raises;
          test "destroyed parent raises" create_destroyed_parent_raises;
          test "with index inserts at position" create_with_index;
        ];
      group "Hierarchy"
        [
          test "children insertion order" children_insertion_order;
          test "attach moves between parents" attach_moves_between_parents;
          test "detach removes from parent" detach_removes;
          test "detach blurs focused node" detach_blurs_focused;
          test "detach parent blurs focused descendant"
            detach_parent_blurs_focused_descendant;
          test "attach to self raises" attach_self_raises;
          test "attach destroyed raises" attach_destroyed_raises;
        ];
      group "Destroy"
        [
          test "marks destroyed" destroy_marks_destroyed;
          test "detaches from parent" destroy_detaches;
          test "destroy parent blurs focused descendant"
            destroy_parent_blurs_focused_descendant;
          test "destroy unregisters node once" destroy_unregisters_node_once;
          test "clears handlers" destroy_clears_handlers;
          test "is idempotent" destroy_idempotent;
          test "create on destroyed raises" create_on_destroyed_raises;
          test "recursively destroys subtree" destroy_recursively_all;
          test "recursively destroys a wide subtree"
            destroy_recursively_wide_subtree;
          test "orphans the children of a wide node"
            destroy_orphans_wide_children;
          test "detach_all detaches from several parents" detach_all_wide;
        ];
      group "Visual properties"
        [
          test "set_visible toggle" visible_toggle;
          test "set_visible false blurs" visible_false_blurs;
          test "set_visible no-op on same value" visible_noop_same_value;
          test "set_z_index" set_z_index;
          test "set_z_index no-op on same value" z_index_noop_same_value;
          test "set_opacity clamps 0-1" opacity_clamp;
          test "set_opacity no-op on same value" opacity_noop_same_value;
          test "set_buffered toggle" buffered_toggle;
          test "set_live toggle" live_toggle;
          test "set_child_clip schedules render" set_child_clip_schedules;
        ];
      group "Focus"
        [
          test "returns false if not focusable" focus_not_focusable;
          test "returns true if focusable" focus_focusable;
          test "delegates to context" focus_delegates;
          test "set_focusable false blurs focused node"
            set_focusable_false_blurs;
          test "blur removes focus" blur_removes_focus;
          test "blur delegates to context" blur_delegates;
          test "blur is no-op if not focused" blur_noop_if_not_focused;
          test "cursor with provider" cursor_with_provider;
          test "cursor None without provider" cursor_none_without_provider;
          test "clear_cursor_provider" clear_cursor_provider;
          test "set_cursor_provider schedules render"
            set_cursor_provider_schedules;
          test "clear_cursor_provider schedules when present"
            clear_cursor_provider_schedules_when_present;
        ];
      group "Key events"
        [
          test "on_key accumulates newest first" on_key_order;
          test "prevent_default preserves regular key handlers"
            emit_key_prevent_default_preserves_handlers;
          test "stop_propagation keeps the node's other handlers"
            emit_key_runs_handlers_after_stop_propagation;
          test "run_key_behavior runs the behavior" run_key_behavior_runs;
          test "emit_key does not run the behavior"
            emit_key_does_not_run_default;
        ];
      group "Mouse events"
        [
          test "on_mouse accumulates handlers" on_mouse_accumulates;
          test "emit_mouse runs only the node's handlers"
            emit_mouse_runs_only_the_node;
        ];
      group "Paste events"
        [
          test "set_paste_behavior replaces" paste_replaces;
          test "set_paste_behavior None clears" paste_none_clears;
        ];
      group "Live count"
        [
          test "live visible contributes 1" live_count_contributes;
          test "hide reduces live_count" live_count_hide_reduces;
          test "propagates up tree" live_count_propagates;
          test "detach adjusts ancestors" live_count_detach_adjusts;
          test "attach adjusts ancestors" live_count_attach_adjusts;
          test "on_live_count_change callback" live_count_change_callback;
        ];
      group "Selection"
        [
          test "not selectable by default" not_selectable_default;
          test "set_selection makes selectable" set_selection_makes_selectable;
          test "unset_selection clears" unset_selection_clears;
          test "should_start delegates" should_start_selection_delegates;
          test "get_selected_text delegates" get_selected_text_delegates;
          test "emit_selection_changed delegates"
            emit_selection_changed_delegates;
          test "emit_selection_changed None without handler"
            emit_selection_changed_none_without_handler;
          test "clear_selection delegates" clear_selection_delegates;
        ];
      group "Lifecycle"
        [
          test "on_frame runs in pre_render_update" on_frame_runs;
          test "on_resize fires when dimensions change" on_resize_fires;
        ];
      group "Layout"
        [
          test "defaults to 0,0,0,0" layout_defaults;
          test "update_layout sets position and size" layout_update;
          test "zero layout keeps zero extent" zero_layout_keeps_zero_extent;
          test "adjacent fractional edges share a cell boundary"
            adjacent_fractional_edges_share_cell_boundary;
          test "bounds returns clip_rect" bounds_returns_clip_rect;
          test "set_translate offsets position" translate_offsets;
          test "mark_dirty sets layout_dirty" mark_dirty;
          test "hidden returns zero dimensions" hidden_zero_dimensions;
          test "set_style schedules render" set_style_schedules;
          test "style roundtrip" style_roundtrip;
          test "set_style display:none updates visible flag"
            set_style_display_none_updates_visible;
          test "set_style while hidden stays hidden"
            set_style_while_hidden_stays_hidden;
          test "set_style while hidden restores on show"
            set_style_while_hidden_restores_on_show;
        ];
      group "Children z-order"
        [
          test "sorted by z_index" children_z_sorted;
          test "empty for no children" children_z_empty;
          test "iter_children_z visits all" iter_children_z_visits_all;
          test "children_in_viewport filters for small sets"
            children_in_viewport_filters_small_sets;
        ];
      group "Render hooks"
        [
          test "render_before fires" render_before_hook;
          test "render_after fires" render_after_hook;
          test "before -> render -> after order" render_hook_order;
          test "set_render_before schedules render" set_render_before_schedules;
          test "set_render_after schedules render" set_render_after_schedules;
          test "buffered render_full uses local coordinates"
            buffered_render_full_uses_local_coordinates;
        ];
    ]
