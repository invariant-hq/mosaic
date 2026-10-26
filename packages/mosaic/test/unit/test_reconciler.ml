open Mosaic_ui
open Mosaic
open Windtrap

(* ── Helpers ── *)

let make () =
  let renderer = Renderer.create () in
  let container = Renderer.root renderer in
  let reconciler = Reconciler.create ~container in
  (renderer, reconciler)

let render_view ?(viewport_width = 40) reconciler vnode =
  Reconciler.render reconciler ~viewport_width vnode

let render_focused_view ?viewport_width renderer reconciler vnode =
  render_view ?viewport_width reconciler vnode;
  match Reconciler.focused reconciler with
  | Some node -> ignore (Renderer.focus renderer node : bool)
  | None -> Renderer.blur renderer

let do_frame ?(width = 40) ?(height = 20) renderer =
  Renderer.render_frame renderer ~width ~height ~dt:0.

let children_of renderer = Renderable.children (Renderer.root renderer)
let child_count renderer = List.length (children_of renderer)

let screen_row renderer row =
  do_frame renderer;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  String.trim
    (String.concat ""
       (List.init (Matrix_grid.width grid) (fun x ->
            match
              Matrix_grid.get_text grid ((row * Matrix_grid.width grid) + x)
            with
            | "" -> " "
            | text -> text)))

let column children =
  Vnode.box
    ~style:
      (Toffee.Style.default
      |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column)
    children

(* ── Construction ── *)

let create_returns_container () =
  let renderer, reconciler = make () in
  is_true ~msg:"container is root"
    (Reconciler.container reconciler == Renderer.root renderer)

let render_rejects_nonpositive_viewport_width () =
  let _renderer, reconciler = make () in
  raises_match ~msg:"positive viewport width"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Reconciler.render reconciler ~viewport_width:0 (Vnode.box []))

(* ── Basic Rendering ── *)

let render_single_box () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_text () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let render_empty () =
  let renderer, reconciler = make () in
  render_view reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let render_fragment () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer)

let render_nested_fragments () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment
       [ Vnode.fragment [ Vnode.text "a"; Vnode.text "b" ]; Vnode.text "c" ]);
  do_frame renderer;
  equal ~msg:"three children" int 3 (child_count renderer)

let viewport_switch_selects_only_matching_branch () =
  let renderer, reconciler = make () in
  let view =
    Vnode.viewport_switch ~at_least_width:80 ~wide:(Vnode.spinner ())
      ~narrow:
        (Vnode.input ~state:Editor.empty ~on_change:ignore ~focused:true ())
  in
  render_focused_view ~viewport_width:79 renderer reconciler view;
  do_frame ~width:79 renderer;
  equal ~msg:"only the narrow branch is mounted" int 1 (child_count renderer);
  equal ~msg:"hidden live branch inactive" int 0
    (Renderable.Private.live_count (Renderer.root renderer));
  match Renderer.focused renderer with
  | Some focused ->
      is_true ~msg:"selected branch receives declared focus"
        (List.hd (children_of renderer) == focused)
  | None -> fail "expected the narrow input to be focused"

let viewport_switch_reconciles_selected_branch_on_resize () =
  let renderer, reconciler = make () in
  let last_scroll = ref None in
  let scroll_style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:4)
  in
  let column_style =
    Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
  in
  let content =
    Vnode.box ~key:"transcript-content"
      ~style:(column_style |> Toffee.Style.set_flex_shrink 0.)
      (List.init 20 (fun index -> Vnode.text (string_of_int index)))
  in
  let scroll = ref (Scroll.at 0) in
  let transcript () =
    Vnode.scroll_box ~key:"transcript" ~style:scroll_style ~focusable:false
      ~show_scrollbars:false ~scroll:!scroll
      ~on_scroll:(fun next ->
        scroll := next;
        last_scroll :=
          Some
            (snd
               (Scroll.resolve next ~width:10 ~height:4 ~content_width:10
                  ~content_height:20)))
      [ content ]
  in
  let root children =
    Vnode.box ~key:"responsive-root" ~style:column_style children
  in
  let view () =
    Vnode.viewport_switch ~at_least_width:80
      ~wide:(root [ transcript (); Vnode.spinner () ])
      ~narrow:
        (root
           [
             transcript ();
             Vnode.input ~state:Editor.empty ~on_change:ignore ~focused:true ();
           ])
  in
  render_focused_view ~viewport_width:79 renderer reconciler (view ());
  do_frame ~width:79 renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let first_root = List.hd (children_of renderer) in
  let first_transcript = List.hd (Renderable.children first_root) in
  let first_narrow = List.nth (Renderable.children first_root) 1 in
  let mouse = Test_harness.mouse_report ~x:1 ~y:1 (Scroll `Down) in
  ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse);
  let first_scroll =
    match !last_scroll with
    | Some y when y > 0 -> y
    | _ -> fail "expected the transcript to scroll before resize"
  in
  render_focused_view ~viewport_width:80 renderer reconciler (view ());
  do_frame ~width:80 renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let wide_root = List.hd (children_of renderer) in
  let wide_transcript = List.hd (Renderable.children wide_root) in
  let wide_only = List.nth (Renderable.children wide_root) 1 in
  is_true ~msg:"compatible keyed root retained" (first_root == wide_root);
  is_true ~msg:"keyed stateful child retained"
    (first_transcript == wide_transcript);
  is_true ~msg:"narrow branch destroyed" (Renderable.destroyed first_narrow);
  is_none ~msg:"focus cleared with narrow branch" (Renderer.focused renderer);
  equal ~msg:"only wide live branch active" int 1
    (Renderable.Private.live_count (Renderer.root renderer));
  ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse);
  (match !last_scroll with
  | Some y ->
      satisfies
        ~claim:
          (Printf.sprintf
             "retained transcript continues past its scroll position (%d)"
             first_scroll)
        int
        (fun pos -> pos > first_scroll)
        y
  | None -> fail "expected the retained transcript to keep scrolling");
  render_focused_view ~viewport_width:79 renderer reconciler (view ());
  do_frame ~width:79 renderer;
  let second_root = List.hd (children_of renderer) in
  let second_transcript = List.hd (Renderable.children second_root) in
  let second_narrow = List.nth (Renderable.children second_root) 1 in
  is_true ~msg:"root survives both crossings" (first_root == second_root);
  is_true ~msg:"stateful child survives both crossings"
    (first_transcript == second_transcript);
  is_true ~msg:"wide branch destroyed" (Renderable.destroyed wide_only);
  is_false ~msg:"narrow branch remounted" (first_narrow == second_narrow);
  equal ~msg:"hidden wide live branch stopped" int 0
    (Renderable.Private.live_count (Renderer.root renderer));
  match Renderer.focused renderer with
  | Some focused ->
      is_true ~msg:"remounted narrow branch receives focus"
        (focused == second_narrow)
  | None -> fail "expected remounted narrow branch to be focused"

let non_focusable_scroll_box_still_scrolls_with_wheel () =
  let renderer, reconciler = make () in
  let last_scroll = ref None in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:3)
  in
  let content =
    Vnode.box
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_flex_shrink 0.)
      (List.init 10 (fun index -> Vnode.text (string_of_int index)))
  in
  render_view reconciler
    (Vnode.scroll_box ~style ~focusable:false ~show_scrollbars:false
       ~scroll:(Scroll.at 0)
       ~on_scroll:(fun next ->
         last_scroll :=
           Some
             (snd
                (Scroll.resolve next ~width:10 ~height:3 ~content_width:10
                   ~content_height:10)))
       [ content ]);
  do_frame renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let node = List.hd (children_of renderer) in
  is_false ~msg:"scroll box is not focusable" (Renderable.focusable node);
  let mouse = Test_harness.mouse_report ~x:1 ~y:1 (Scroll `Down) in
  ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse);
  is_none ~msg:"wheel does not take focus" (Renderer.focused renderer);
  match !last_scroll with
  | Some y ->
      satisfies ~claim:"wheel advances scroll position (> 0)" int
        (fun pos -> pos > 0)
        y
  | None -> fail "expected wheel scroll callback"

let declared_scroll_survives_rerender_and_remount () =
  let renderer, reconciler = make () in
  let proposals = ref [] in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:10 ~height:4)
  in
  let content =
    Vnode.box
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Column
        |> Toffee.Style.set_flex_shrink 0.)
      (List.init 20 (fun i -> Vnode.text (string_of_int i)))
  in
  let view key scroll =
    Vnode.scroll_box ~key ~style ~show_scrollbars:false ~scroll
      ~on_scroll:(fun next ->
        proposals :=
          snd
            (Scroll.resolve next ~width:10 ~height:4 ~content_width:10
               ~content_height:20)
          :: !proposals)
      [ content ]
  in
  let wheel () =
    let mouse = Test_harness.mouse_report ~x:1 ~y:1 (Scroll `Up) in
    ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse)
  in
  let show key scroll =
    render_view reconciler (view key scroll);
    do_frame renderer;
    ignore (Renderer.render ~full:true renderer : string);
    wheel ()
  in
  show "first" (Scroll.at 4);
  show "first" (Scroll.at 4);
  show "remounted" (Scroll.at 4);
  equal
    ~msg:
      "rejected proposals leave accepted position intact across rerender and \
       remount"
    (list int) [ 3; 3; 3 ] (List.rev !proposals);
  show "remounted" (Scroll.follow `Bottom (Scroll.at 0));
  equal ~msg:"follow resolves from measured content" int 15 (List.hd !proposals);
  show "remounted" (Scroll.reveal ~line:10 (Scroll.at 0));
  equal ~msg:"reveal resolves nearest visible row" int 6 (List.hd !proposals)

(* ── Re-rendering (Update in Place) ── *)

let rerender_reuses_box () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.box ~border:true []);
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_text () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.text "world");
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_updates_visibility () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~visible:true []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  is_true ~msg:"visible before" (Renderable.visible node);
  render_view reconciler (Vnode.box ~visible:false []);
  do_frame renderer;
  is_false ~msg:"hidden after" (Renderable.visible node)

let rerender_updates_z_index () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~z_index:0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  equal ~msg:"z_index before" int 0 (Renderable.z_index node);
  render_view reconciler (Vnode.box ~z_index:5 []);
  do_frame renderer;
  equal ~msg:"z_index after" int 5 (Renderable.z_index node)

let rerender_updates_opacity () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box ~opacity:1.0 []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.box ~opacity:0.5 []);
  do_frame renderer;
  is_true ~msg:"opacity" (Float.equal (Renderable.opacity node) 0.5)

let rejected_input_preserves_accepted_state () =
  let renderer, reconciler = make () in
  let inputs = ref [] in
  let submissions = ref [] in
  let view () =
    Vnode.input ~key:"controlled-input" ~focused:true ~state:Editor.empty
      ~on_change:(fun state -> inputs := Editor.text state :: !inputs)
      ~on_submit:(fun value -> submissions := value :: !submissions)
      ()
  in
  render_focused_view renderer reconciler (view ());
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'x') : Event.key);
  equal ~msg:"input proposes the edit" (list string) [ "x" ] (List.rev !inputs);
  render_focused_view renderer reconciler (view ());
  ignore
    (Test_harness.dispatch_key renderer
       (Matrix_input.Keystroke.make Matrix_input.Keystroke.Enter)
      : Event.key);
  equal ~msg:"rejected edit leaves submitted text unchanged" (list string)
    [ "" ] (List.rev !submissions)

let rejected_textarea_preserves_accepted_state () =
  let renderer, reconciler = make () in
  let inputs = ref [] in
  let submissions = ref [] in
  let view () =
    Vnode.textarea ~key:"controlled-textarea" ~focused:true ~state:Editor.empty
      ~on_change:(fun state -> inputs := Editor.text state :: !inputs)
      ~on_submit:(fun value -> submissions := value :: !submissions)
      ()
  in
  render_focused_view renderer reconciler (view ());
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'x') : Event.key);
  equal ~msg:"textarea proposes the edit" (list string) [ "x" ]
    (List.rev !inputs);
  do_frame renderer;
  render_focused_view renderer reconciler (view ());
  let modifier = { Matrix_input.Keystroke.no_mods with ctrl = true } in
  ignore
    (Test_harness.dispatch_key renderer
       (Matrix_input.Keystroke.make ~mods:modifier Matrix_input.Keystroke.Enter)
      : Event.key);
  equal ~msg:"rejected edit leaves submitted text unchanged" (list string)
    [ "" ] (List.rev !submissions)

let accepted_editor_state_preserves_cursor () =
  let renderer, reconciler = make () in
  let state = ref (Editor.of_string "ab") in
  let view () =
    Vnode.textarea ~focused:true ~state:!state
      ~on_change:(fun next -> state := next)
      ()
  in
  render_focused_view renderer reconciler (view ());
  ignore
    (Test_harness.dispatch_key renderer
       (Matrix_input.Keystroke.make Matrix_input.Keystroke.Left)
      : Event.key);
  equal ~msg:"cursor proposal accepted by model" int 1 (Editor.cursor !state);
  render_focused_view renderer reconciler (view ());
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'x') : Event.key);
  equal ~msg:"accepted caret guides next edit" string "axb" (Editor.text !state)

(* ── Keyed Reconciliation ── *)

let keyed_reorder () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let children_before = children_of renderer in
  let node_a = List.nth children_before 0 in
  let node_b = List.nth children_before 1 in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"b" []; Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let children_after = children_of renderer in
  let first = List.nth children_after 0 in
  let second = List.nth children_after 1 in
  is_true ~msg:"b is first" (first == node_b);
  is_true ~msg:"a is second" (second == node_a)

let keyed_removal () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  let node_b = List.nth (children_of renderer) 1 in
  render_view reconciler (Vnode.fragment [ Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"b survives" (List.hd (children_of renderer) == node_b)

let keyed_addition () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box ~key:"a" [] ]);
  do_frame renderer;
  let node_a = List.hd (children_of renderer) in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~key:"a" []; Vnode.box ~key:"b" [] ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  is_true ~msg:"a survives" (List.nth (children_of renderer) 0 == node_a)

let keyed_reorder_preserves_focus () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.fragment
       [
         Vnode.box ~key:"a" ~focusable:true [];
         Vnode.box ~key:"b" ~focusable:true [];
       ]);
  do_frame renderer;
  let node_a = List.nth (children_of renderer) 0 in
  let node_b = List.nth (children_of renderer) 1 in
  ignore (Renderer.focus renderer node_b : bool);
  render_view reconciler
    (Vnode.fragment
       [
         Vnode.box ~key:"b" ~focusable:true [];
         Vnode.box ~key:"a" ~focusable:true [];
       ]);
  do_frame renderer;
  is_true ~msg:"b remains focused" (Renderable.focused node_b);
  is_true ~msg:"renderer still points at b"
    (match Renderer.focused renderer with
    | Some node -> node == node_b
    | None -> false);
  is_false ~msg:"a remains unfocused" (Renderable.focused node_a)

let duplicate_keys_first_occurrence_stays_stable () =
  (* Duplicate-keyed siblings are a view bug, but the failure mode is pinned:
     the first occurrence keeps its identity across renders; later duplicates
     cannot match a consumed fiber and remount. *)
  let renderer, reconciler = make () in
  let dup () =
    Vnode.fragment [ Vnode.box ~key:"dup" []; Vnode.box ~key:"dup" [] ]
  in
  render_view reconciler (dup ());
  do_frame renderer;
  let first = List.nth (children_of renderer) 0 in
  let second = List.nth (children_of renderer) 1 in
  render_view reconciler (dup ());
  do_frame renderer;
  equal ~msg:"still two children" int 2 (child_count renderer);
  is_true ~msg:"first occurrence keeps its node"
    (List.nth (children_of renderer) 0 == first);
  is_false ~msg:"later duplicate remounts"
    (List.nth (children_of renderer) 1 == second)

let unkeyed_positional () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "x" ]);
  do_frame renderer;
  let box_node = List.nth (children_of renderer) 0 in
  let text_node = List.nth (children_of renderer) 1 in
  render_view reconciler
    (Vnode.fragment [ Vnode.box ~border:true []; Vnode.text "y" ]);
  do_frame renderer;
  is_true ~msg:"box reused" (List.nth (children_of renderer) 0 == box_node);
  is_true ~msg:"text reused" (List.nth (children_of renderer) 1 == text_node)

(* ── Kind Mismatch ── *)

let kind_mismatch_destroys_old () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.text "replaced");
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer);
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  is_true ~msg:"new is different" (List.hd (children_of renderer) != old_node)

let kind_mismatch_text_to_slider () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "hello");
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.slider ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

let render_single_spinner () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

(* A spinner animates only while its element is live and its frame set has
   frames to cycle: [~live:false] holds it, at mount and across a change of
   its props, and [~live:true] does not animate a single frame. *)
let spinner_follows_declared_live () =
  let renderer, reconciler = make () in
  let live () = Renderable.live (List.hd (children_of renderer)) in
  render_view reconciler (Vnode.spinner ~live:false ());
  do_frame renderer;
  is_false ~msg:"held at mount" (live ());
  render_view reconciler (Vnode.spinner ~live:false ~color:Ansi.Color.red ());
  do_frame renderer;
  is_false ~msg:"held across a props change" (live ());
  render_view reconciler (Vnode.spinner ~color:Ansi.Color.red ());
  do_frame renderer;
  is_true ~msg:"released" (live ());
  let still = { Spinner.frames = [| "*" |]; period = 0.1 } in
  render_view reconciler (Vnode.spinner ~frame_set:still ());
  do_frame renderer;
  is_false ~msg:"a single frame never animates" (live ());
  render_view reconciler (Vnode.spinner ~live:false ());
  render_view reconciler (Vnode.spinner ~frame_set:still ());
  do_frame renderer;
  is_false ~msg:"released onto a single frame" (live ())

(* A table that replaces another at the same position without a key reports
   its own row under the still pointer, at the old table's index. *)
let unkeyed_table_reports_its_own_hover () =
  let renderer, reconciler = make () in
  let log = ref [] in
  let table name =
    Vnode.table ~show_header:false
      ~columns:[ Table.column "c" ]
      ~rows:
        (List.init 4 (fun i -> [| Table.cell (Printf.sprintf "%s%d" name i) |]))
      ~on_hover:(fun row ->
        log :=
          Option.fold ~none:"none" ~some:(Printf.sprintf "%s%d" name) row
          :: !log;
        None)
      ()
  in
  render_view reconciler (table "a");
  do_frame renderer;
  ignore (Renderer.render ~full:true renderer : string);
  let move = Test_harness.mouse_report ~x:1 ~y:2 (Motion None) in
  ignore (Test_harness.dispatch_mouse renderer move : Event.mouse);
  render_view reconciler (table "b");
  do_frame renderer;
  equal ~msg:"the new table's row" (list string) [ "b1"; "a1" ] !log

let render_single_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let flex_direction_of node = Toffee.Style.flex_direction (Renderable.style node)

let scroll_bar_orientation_applies_at_mount () =
  (* The reconciler creates the widget with defaults and applies the vnode
     props afterwards, so apply_props must honor orientation. *)
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.scroll_bar ~orientation:`Horizontal ());
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  is_true ~msg:"lays out as a row"
    (flex_direction_of node = Toffee.Style.Flex_direction.Row)

let scroll_bar_orientation_updates_on_rerender () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.scroll_bar ~orientation:`Horizontal ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.scroll_bar ~orientation:`Vertical ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after);
  is_true ~msg:"lays out as a column"
    (flex_direction_of node_after = Toffee.Style.Flex_direction.Column)

let rerender_reuses_spinner () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.spinner ~frame_set:Spinner.line ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let rerender_reuses_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  let node_before = List.hd (children_of renderer) in
  render_view reconciler (Vnode.progress_bar ~value:0.5 ());
  do_frame renderer;
  let node_after = List.hd (children_of renderer) in
  is_true ~msg:"same node" (node_before == node_after)

let kind_mismatch_spinner_to_progress_bar () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.spinner ());
  do_frame renderer;
  let old_node = List.hd (children_of renderer) in
  render_view reconciler (Vnode.progress_bar ());
  do_frame renderer;
  is_true ~msg:"old destroyed" (Renderable.destroyed old_node);
  equal ~msg:"one child" int 1 (child_count renderer)

(* ── Children ── *)

let fewer_children_destroys_extra () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.box [ Vnode.text "a"; Vnode.text "b"; Vnode.text "c" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"three kids" int 3 (List.length (Renderable.children box_node));
  let old_c = List.nth (Renderable.children box_node) 2 in
  render_view reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  is_true ~msg:"c destroyed" (Renderable.destroyed old_c)

let more_children_creates_new () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box [ Vnode.text "a" ]);
  do_frame renderer;
  let box_node = List.hd (children_of renderer) in
  equal ~msg:"one kid" int 1 (List.length (Renderable.children box_node));
  render_view reconciler (Vnode.box [ Vnode.text "a"; Vnode.text "b" ]);
  do_frame renderer;
  equal ~msg:"two kids" int 2 (List.length (Renderable.children box_node))

let nested_boxes () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box [ Vnode.box [ Vnode.text "inner" ] ]);
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
  render_view reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"one child" int 1 (child_count renderer)

let embed_removed_detaches () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  render_view reconciler (Vnode.embed embedded);
  do_frame renderer;
  equal ~msg:"attached" int 1 (child_count renderer);
  render_view reconciler Vnode.empty;
  do_frame renderer;
  equal ~msg:"detached" int 0 (child_count renderer)

(* ── Callbacks ── *)

let on_key_handler_fires () =
  let renderer, reconciler = make () in
  let received = ref false in
  render_view reconciler
    (Vnode.box ~focusable:true ~on_key:(fun _ -> Some (received := true)) []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'a') : Event.key);
  is_true ~msg:"handler fired" !received

let on_key_handler_updates_on_rerender () =
  let renderer, reconciler = make () in
  let log = ref [] in
  render_view reconciler
    (Vnode.box ~focusable:true
       ~on_key:(fun _ -> Some (log := "first" :: !log))
       []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  ignore (Renderer.focus renderer node : bool);
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'a') : Event.key);
  equal ~msg:"first handler" (list string) [ "first" ] !log;
  render_view reconciler
    (Vnode.box ~focusable:true
       ~on_key:(fun _ -> Some (log := "second" :: !log))
       []);
  ignore
    (Test_harness.dispatch_key renderer (Test_harness.char_key 'b') : Event.key);
  equal ~msg:"second handler" (list string) [ "second"; "first" ] !log

(* ── Unmount ── *)

let unmount_empties_container () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.fragment [ Vnode.box []; Vnode.text "hi" ]);
  do_frame renderer;
  equal ~msg:"two children" int 2 (child_count renderer);
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"no children" int 0 (child_count renderer)

let unmount_destroys_nodes () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.box []);
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  Reconciler.unmount reconciler;
  is_true ~msg:"destroyed" (Renderable.destroyed node)

let unmount_allows_reuse () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.text "first");
  do_frame renderer;
  Reconciler.unmount reconciler;
  do_frame renderer;
  equal ~msg:"empty after unmount" int 0 (child_count renderer);
  render_view reconciler (Vnode.text "second");
  do_frame renderer;
  equal ~msg:"one child after reuse" int 1 (child_count renderer)

(* Unmounting a wide view destroys every row and its handlers, and detaches
   the embedded nodes, which the application still owns, without destroying
   them. *)
let unmount_tears_down_wide_view () =
  let renderer, reconciler = make () in
  let embedded = Renderable.create ~parent:(Renderer.root renderer) () in
  let nested = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.detach embedded;
  Renderable.detach nested;
  let key_fired = ref false in
  let n = 2000 in
  render_view reconciler
    (Vnode.box
       (List.init n (fun i ->
            Vnode.text ~key:(string_of_int i)
              ~on_key:(fun _ -> Some (key_fired := true))
              (string_of_int i))
       @ [ Vnode.embed embedded; Vnode.box [ Vnode.embed nested ] ]));
  do_frame renderer;
  let box = List.hd (children_of renderer) in
  let rows = Renderable.children box in
  equal ~msg:"mounted" int (n + 2) (List.length rows);
  Reconciler.unmount reconciler;
  is_true ~msg:"box destroyed" (Renderable.destroyed box);
  let rows = List.filter (fun node -> not (node == embedded)) rows in
  is_true ~msg:"rows destroyed" (List.for_all Renderable.destroyed rows);
  is_true ~msg:"rows orphaned"
    (List.for_all (fun node -> Option.is_none (Renderable.parent node)) rows);
  List.iter
    (fun row ->
      Renderable.Private.emit_key row
        (Event.Key.of_input (Test_harness.char_key 'a')))
    rows;
  is_false ~msg:"handlers dropped" !key_fired;
  is_false ~msg:"embedded kept"
    (Renderable.destroyed embedded || Renderable.destroyed nested);
  is_true ~msg:"embedded detached"
    (Option.is_none (Renderable.parent embedded)
    && Option.is_none (Renderable.parent nested));
  equal ~msg:"container empty" int 0 (child_count renderer)

(* Shrinking a wide list destroys the rows it drops and keeps the others in
   order, and the embedded nodes it drops are detached without being
   destroyed. Emptying it then destroys the rest. *)
let shrink_wide_list () =
  let renderer, reconciler = make () in
  let n = 2000 in
  let embeds =
    List.init 500 (fun _ ->
        let node = Renderable.create ~parent:(Renderer.root renderer) () in
        Renderable.detach node;
        node)
  in
  let row i = Vnode.text ~key:(string_of_int i) (string_of_int i) in
  render_view reconciler
    (Vnode.box (List.init n row @ List.map Vnode.embed embeds));
  let box = List.hd (children_of renderer) in
  let rows = List.filteri (fun i _ -> i < n) (Renderable.children box) in
  let kept = List.filteri (fun i _ -> i mod 10 = 0) rows in
  let dropped = List.filteri (fun i _ -> i mod 10 <> 0) rows in
  render_view reconciler
    (Vnode.box
       (List.filter_map
          (fun i -> if i mod 10 = 0 then Some (row i) else None)
          (List.init n Fun.id)));
  is_true ~msg:"kept rows in order"
    (List.equal ( == ) kept (Renderable.children box));
  is_false ~msg:"kept rows alive" (List.exists Renderable.destroyed kept);
  is_true ~msg:"dropped rows destroyed"
    (List.for_all Renderable.destroyed dropped);
  is_true ~msg:"embeds detached and kept"
    (List.for_all
       (fun node ->
         Option.is_none (Renderable.parent node)
         && not (Renderable.destroyed node))
       embeds);
  do_frame renderer;
  render_view reconciler (Vnode.box []);
  is_true ~msg:"emptied" (List.is_empty (Renderable.children box));
  is_true ~msg:"all rows destroyed" (List.for_all Renderable.destroyed rows)

(* Unmounting rows declared at the root destroys them and detaches the
   embedded nodes beside them, which stay alive. *)
let unmount_wide_root () =
  let renderer, reconciler = make () in
  let embeds =
    List.init 500 (fun _ ->
        let node = Renderable.create ~parent:(Renderer.root renderer) () in
        Renderable.detach node;
        node)
  in
  render_view reconciler
    (Vnode.fragment
       (List.init 2000 (fun i -> Vnode.text ~key:(string_of_int i) "row")
       @ List.map Vnode.embed embeds));
  let rows = children_of renderer in
  Reconciler.unmount reconciler;
  equal ~msg:"container empty" int 0 (child_count renderer);
  is_true ~msg:"rows destroyed"
    (List.for_all
       (fun node ->
         Renderable.destroyed node || List.exists (( == ) node) embeds)
       rows);
  is_true ~msg:"embeds detached and kept"
    (List.for_all
       (fun node ->
         Option.is_none (Renderable.parent node)
         && not (Renderable.destroyed node))
       embeds)

(* ── Runner ── *)

let duplicate_visible_focus_is_rejected () =
  let renderer, reconciler = make () in
  render_view reconciler
    (Vnode.box
       [ Vnode.text ~focused:true "first"; Vnode.text ~focused:true "second" ]);
  equal ~msg:"declarations are mounted" int 1 (child_count renderer);
  raises_match ~msg:"visible focus must be unique"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Reconciler.focused reconciler : Renderable.t option))

let hidden_subtrees_do_not_contribute_focus () =
  let renderer, reconciler = make () in
  let view visible =
    Vnode.box
      [
        Vnode.text ~key:"active" ~focused:true "active";
        Vnode.box ~key:"hidden" ~visible
          [
            Vnode.text ~focused:true "hidden first";
            Vnode.text ~focused:true "hidden second";
          ];
      ]
  in
  render_view reconciler (view false);
  let active =
    match Renderable.children (List.hd (children_of renderer)) with
    | active :: _ -> active
    | [] -> fail "missing active element"
  in
  is_true ~msg:"hidden descendants do not conflict with visible focus"
    (match Reconciler.focused reconciler with
    | Some focused -> focused == active
    | None -> false);
  render_view reconciler (view true);
  raises_match ~msg:"revealing conflicting declarations rejects ambiguity"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Reconciler.focused reconciler : Renderable.t option));
  render_view reconciler (view false);
  is_true ~msg:"hiding the subtree restores its focus exclusion"
    (match Reconciler.focused reconciler with
    | Some focused -> focused == active
    | None -> false)

(* Inert subtrees, and everything outside a shown modal, declare no focus;
   of two modals the later is the one. *)
let inert_elements_do_not_contribute_focus () =
  let _renderer, reconciler = make () in
  let focused_text name = Vnode.text ~key:name ~focused:true name in
  let focused_count view =
    render_view reconciler view;
    Option.is_some (Reconciler.focused reconciler)
  in
  is_true ~msg:"an inert box hides its declaration"
    (focused_count
       (Vnode.box
          [
            focused_text "outside"; Vnode.box ~inert:true [ focused_text "in" ];
          ]));
  render_view reconciler
    (Vnode.box
       [
         focused_text "outside";
         Vnode.modal (Vnode.box [ Vnode.text ~key:"inside" "inside" ]);
       ]);
  is_true ~msg:"a modal leaves the focus outside it undeclared"
    (Option.is_none (Reconciler.focused reconciler));
  is_true ~msg:"and is the modal" (Option.is_some (Reconciler.modal reconciler));
  render_view reconciler
    (Vnode.box
       [
         focused_text "outside";
         Vnode.modal (Vnode.box [ focused_text "first" ]);
         Vnode.modal (Vnode.box [ Vnode.text "second" ]);
       ]);
  is_true ~msg:"the later modal is the one"
    (Option.is_none (Reconciler.focused reconciler));
  render_view reconciler
    (Vnode.box [ Vnode.box ~inert:true [ Vnode.modal (focused_text "in") ] ]);
  is_true ~msg:"a modal in an inert box is none"
    (Option.is_none (Reconciler.modal reconciler))

let memo_reuses_equal_value_keys () =
  let renderer, reconciler = make () in
  let builds = ref 0 in
  let build values =
    incr builds;
    Vnode.text (String.concat "," values)
  in
  let view values = Vnode.memo ~key:values build in
  render_view reconciler (view [ String.make 1 'a' ]);
  render_view reconciler (view [ String.make 1 'a' ]);
  equal ~msg:"fresh structurally equal key skips view" int 1 !builds;
  render_view reconciler (view [ "b" ]);
  equal ~msg:"changed key rebuilds" int 2 !builds;
  equal ~msg:"one retained child" int 1 (child_count renderer)

let memo_refreshes_message_mapper () =
  let renderer, reconciler = make () in
  let builds = ref 0 and messages = ref [] in
  let build _ =
    incr builds;
    Vnode.box ~focusable:true ~on_key:(fun _ -> Some "event") []
  in
  let view () = Vnode.memo ~key:0 build in
  let first text = messages := ("first " ^ text) :: !messages in
  let second text = messages := ("second " ^ text) :: !messages in
  render_view reconciler (Vnode.map first (view ()));
  ignore (Renderer.focus renderer (List.hd (children_of renderer)) : bool);
  let key () =
    ignore
      (Test_harness.dispatch_key renderer (Test_harness.char_key 'x')
        : Event.key)
  in
  key ();
  render_view reconciler (Vnode.map second (view ()));
  key ();
  equal ~msg:"new mapper refreshes mounted callback" (list string)
    [ "second event"; "first event" ]
    !messages;
  equal ~msg:"mapping invalidates memo" int 2 !builds

let nested_memo_keeps_independent_keys () =
  let renderer, reconciler = make () in
  let outer_builds = ref 0 and inner_builds = ref 0 in
  let inner value =
    incr inner_builds;
    Vnode.text value
  in
  let outer (_, value) =
    incr outer_builds;
    Vnode.box [ Vnode.memo ~key:value inner ]
  in
  let view epoch value = Vnode.memo ~key:(epoch, value) outer in
  render_view reconciler (view 0 "a");
  render_view reconciler (view 1 "a");
  equal ~msg:"outer rebuilt" int 2 !outer_builds;
  equal ~msg:"equal nested key retained" int 1 !inner_builds;
  render_view reconciler (view 1 "b");
  equal ~msg:"changed nested key rebuilt" int 2 !inner_builds;
  equal ~msg:"one outer child" int 1 (child_count renderer)

let memo_belongs_to_keyed_parent () =
  let renderer, reconciler = make () in
  let builds_a = ref 0 and builds_b = ref 0 in
  let build_a _ =
    incr builds_a;
    Vnode.text "A"
  in
  let build_b _ =
    incr builds_b;
    Vnode.text "B"
  in
  let a key = Vnode.box ~key [ Vnode.memo ~key:0 build_a ] in
  let b () = Vnode.box ~key:"b" [ Vnode.memo ~key:0 build_b ] in
  render_view reconciler (Vnode.fragment [ a "a"; b () ]);
  render_view reconciler (Vnode.fragment [ b (); a "a" ]);
  equal ~msg:"memo follows its keyed parent" string "BA" (screen_row renderer 0);
  equal ~msg:"reorder retains A memo" int 1 !builds_a;
  equal ~msg:"reorder retains B memo" int 1 !builds_b;
  render_view reconciler (Vnode.fragment [ b (); a "new-a" ]);
  equal ~msg:"new parent owns fresh memo" int 2 !builds_a

let a_view () = Vnode.text "AAA"
let b_view () = Vnode.text "BBB"

let memos_in_one_slot_keep_their_views () =
  let renderer, reconciler = make () in
  let view show_a =
    column
      [
        Vnode.memo ~key:() b_view;
        (if show_a then Vnode.memo ~key:() a_view else Vnode.empty);
      ]
  in
  render_view reconciler (view true);
  equal ~msg:"both memos" string "BBB" (screen_row renderer 0);
  equal ~msg:"second memo" string "AAA" (screen_row renderer 1);
  render_view reconciler (view false);
  equal ~msg:"remaining memo keeps its view" string "BBB"
    (screen_row renderer 0);
  equal ~msg:"removed memo" string "" (screen_row renderer 1)

let show_block id = Vnode.text ("show " ^ id)
let edit_block id = Vnode.text ("edit " ^ id)

let memo_view_function_is_part_of_identity () =
  let renderer, reconciler = make () in
  let view ~editing id =
    if editing then Vnode.memo ~key:id edit_block
    else Vnode.memo ~key:id show_block
  in
  render_view reconciler (view ~editing:false "b1");
  equal ~msg:"show mode" string "show b1" (screen_row renderer 0);
  render_view reconciler (view ~editing:true "b1");
  equal ~msg:"same key, other view" string "edit b1" (screen_row renderer 0)

let count_view n = Vnode.text (Printf.sprintf "count=%d" n)

let option_view = function
  | None -> Vnode.text "none"
  | Some () -> Vnode.text "some"

let memo_keys_of_different_types_do_not_alias () =
  let renderer, reconciler = make () in
  render_view reconciler (Vnode.memo ~key:0 count_view);
  equal ~msg:"integer key" string "count=0" (screen_row renderer 0);
  render_view reconciler (Vnode.memo ~key:None option_view);
  equal ~msg:"None after 0" string "none" (screen_row renderer 0)

let float_view x = Vnode.text (Printf.sprintf "%g" x)

let memo_compares_float_bits () =
  let renderer, reconciler = make () in
  let builds = ref 0 in
  let counted x =
    incr builds;
    float_view x
  in
  render_view reconciler (Vnode.memo ~key:Float.nan counted);
  render_view reconciler (Vnode.memo ~key:Float.nan counted);
  equal ~msg:"nan equals itself" int 1 !builds;
  render_view reconciler (Vnode.memo ~key:0. float_view);
  equal ~msg:"zero" string "0" (screen_row renderer 0);
  render_view reconciler (Vnode.memo ~key:(-0.) float_view);
  equal ~msg:"negative zero is another key" string "-0" (screen_row renderer 0)

let memo_rejects_functions_in_key () =
  let rejects key =
    raises_match ~msg:"rejected at memo"
      (function Invalid_argument _ -> true | _ -> false)
      (fun () -> ignore (Vnode.memo ~key (fun _ -> Vnode.empty) : unit Vnode.t))
  in
  rejects (1, fun x -> x + 1);
  rejects [ Some succ ];
  rejects object end

let list_view () = column [ Vnode.text "one"; Vnode.text "two" ]
let rows_view () = Vnode.fragment [ Vnode.text "one"; Vnode.text "two" ]

let memo_spans_keep_sibling_order () =
  let renderer, reconciler = make () in
  let view ~top ~bottom =
    column
      [
        (if top then Vnode.text "top" else Vnode.empty);
        Vnode.memo ~key:() rows_view;
        (if bottom then Vnode.text "bottom" else Vnode.empty);
        Vnode.memo ~key:() list_view;
      ]
  in
  let rows () = List.init 5 (screen_row renderer) in
  render_view reconciler (view ~top:false ~bottom:false);
  equal ~msg:"memos alone" (list string)
    [ "one"; "two"; "one"; "two"; "" ]
    (rows ());
  render_view reconciler (view ~top:true ~bottom:true);
  equal ~msg:"elements around reused spans" (list string)
    [ "top"; "one"; "two"; "bottom"; "one" ]
    (rows ());
  render_view reconciler (view ~top:false ~bottom:true);
  equal ~msg:"removal before a reused span" (list string)
    [ "one"; "two"; "bottom"; "one"; "two" ]
    (rows ())

let switch_view () =
  Vnode.viewport_switch ~at_least_width:30 ~wide:(Vnode.text "wide")
    ~narrow:(Vnode.text "narrow")

let reused_memo_follows_viewport () =
  let renderer, reconciler = make () in
  let builds = ref 0 in
  let counted () =
    incr builds;
    switch_view ()
  in
  render_view ~viewport_width:40 reconciler (Vnode.memo ~key:() counted);
  equal ~msg:"wide" string "wide" (screen_row renderer 0);
  render_view ~viewport_width:20 reconciler (Vnode.memo ~key:() counted);
  equal ~msg:"narrow" string "narrow" (screen_row renderer 0);
  equal ~msg:"view built once" int 1 !builds

let removed_memo_releases_view () =
  let renderer, reconciler = make () in
  let weak = Weak.create 1 in
  let mount () =
    let captured = ref 42 in
    Weak.set weak 0 (Some captured);
    render_view reconciler
      (Vnode.memo ~key:0 (fun _ -> Vnode.text (string_of_int !captured)))
  in
  mount ();
  render_view reconciler Vnode.empty;
  Gc.full_major ();
  is_false ~msg:"removed memo does not retain captured state"
    (Weak.check weak 0);
  equal ~msg:"removed subtree" int 0 (child_count renderer)

let plain_rich_transitions_reuse_text_and_select_wrapped_content () =
  let renderer, reconciler = make () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 3.)
  in
  render_view reconciler (Vnode.text ~key:"text" ~style "before");
  do_frame renderer;
  let node = List.hd (children_of renderer) in
  let fragments = Mosaic_ui.Text.Fragment.[ text "ab"; bold [ text "cdef" ] ] in
  render_view reconciler
    (Vnode.rich_text ~key:"text" ~style ~wrap:`Char
       ~text_style:(Ansi.Style.make ~fg:Ansi.Color.red ())
       fragments);
  do_frame renderer;
  is_true ~msg:"plain to rich retains native node"
    (node == List.hd (children_of renderer));
  let rows = Option.get (Renderable.text_rows node) in
  equal ~msg:"wrapping crosses style boundaries" int 2
    (Matrix_text.Layout.row_count rows.layout);
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  is_true ~msg:"nested style rendered"
    (Ansi.Attr.mem Ansi.Attr.Bold (Matrix_grid.get_style grid 2).attrs);
  let x = Renderable.x node and y = Renderable.y node in
  let selection =
    Selection.create ~anchor:{ x; y } ~focus:{ x = x + 2; y = y + 1 } ()
  in
  ignore (Renderable.Private.emit_selection_changed node (Some selection));
  equal ~msg:"selection crosses wrapping and fragment boundaries" string
    "abcdef"
    (Renderable.Private.get_selected_text node);
  render_view reconciler (Vnode.text ~key:"text" ~style "after");
  do_frame renderer;
  is_true ~msg:"rich to plain retains native node"
    (node == List.hd (children_of renderer));
  equal ~msg:"plain content displayed" string "a"
    (Matrix_grid.get_text
       (Matrix_screen.next_grid (Renderer.screen renderer))
       0)

let () =
  run "mosaic.reconciler"
    [
      test "plain/rich transitions reuse text and select wrapped content"
        plain_rich_transitions_reuse_text_and_select_wrapped_content;
      group "Focus"
        [
          test "duplicate visible focus is rejected"
            duplicate_visible_focus_is_rejected;
          test "hidden subtrees do not contribute focus"
            hidden_subtrees_do_not_contribute_focus;
          test "inert elements do not contribute focus"
            inert_elements_do_not_contribute_focus;
        ];
      group "Memo"
        [
          test "equal value keys" memo_reuses_equal_value_keys;
          test "message mapper refresh" memo_refreshes_message_mapper;
          test "nested memo keys" nested_memo_keeps_independent_keys;
          test "keyed parent ownership" memo_belongs_to_keyed_parent;
          test "removed memo releases view" removed_memo_releases_view;
          test "memos in one slot keep their views"
            memos_in_one_slot_keep_their_views;
          test "view function is part of identity"
            memo_view_function_is_part_of_identity;
          test "keys of different types do not alias"
            memo_keys_of_different_types_do_not_alias;
          test "float keys compare by bits" memo_compares_float_bits;
          test "spans keep sibling order" memo_spans_keep_sibling_order;
          test "reused memo follows the viewport" reused_memo_follows_viewport;
          test "functions in keys are rejected at memo"
            memo_rejects_functions_in_key;
        ];
      group "Construction"
        [
          test "container returns root" create_returns_container;
          test "render rejects nonpositive viewport width"
            render_rejects_nonpositive_viewport_width;
        ];
      group "Basic rendering"
        [
          test "single box" render_single_box;
          test "single text" render_single_text;
          test "empty" render_empty;
          test "fragment" render_fragment;
          test "nested fragments" render_nested_fragments;
          test "viewport switch selects only matching branch"
            viewport_switch_selects_only_matching_branch;
          test "viewport switch reconciles across resize threshold"
            viewport_switch_reconciles_selected_branch_on_resize;
          test "non-focusable scroll box still wheels"
            non_focusable_scroll_box_still_scrolls_with_wheel;
          test "declared scroll survives rerender and remount"
            declared_scroll_survives_rerender_and_remount;
          test "single spinner" render_single_spinner;
          test "spinner follows declared live" spinner_follows_declared_live;
          test "unkeyed table reports its own hover"
            unkeyed_table_reports_its_own_hover;
          test "single progress_bar" render_single_progress_bar;
          test "scroll_bar orientation applies at mount"
            scroll_bar_orientation_applies_at_mount;
          test "scroll_bar orientation updates on rerender"
            scroll_bar_orientation_updates_on_rerender;
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
          test "rejected input preserves accepted state"
            rejected_input_preserves_accepted_state;
          test "rejected textarea preserves accepted state"
            rejected_textarea_preserves_accepted_state;
          test "accepted editor state preserves cursor"
            accepted_editor_state_preserves_cursor;
        ];
      group "Keyed reconciliation"
        [
          test "reorder" keyed_reorder;
          test "removal" keyed_removal;
          test "addition" keyed_addition;
          test "reorder preserves focus" keyed_reorder_preserves_focus;
          test "unkeyed positional" unkeyed_positional;
          test "duplicate keys keep the first occurrence stable"
            duplicate_keys_first_occurrence_stays_stable;
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
          test "on_key handler fires" on_key_handler_fires;
          test "on_key handler updates on rerender"
            on_key_handler_updates_on_rerender;
        ];
      group "Unmount"
        [
          test "empties container" unmount_empties_container;
          test "destroys nodes" unmount_destroys_nodes;
          test "allows reuse" unmount_allows_reuse;
          test "tears down a wide view" unmount_tears_down_wide_view;
          test "tears down rows at the root" unmount_wide_root;
          test "shrinks a wide list" shrink_wide_list;
        ];
    ]
