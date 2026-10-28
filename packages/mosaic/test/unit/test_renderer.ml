open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let make_renderer () = Renderer.create ()

let do_frame ?(width = 80) ?(height = 24) ?(dt = 16.0) t =
  Renderer.render_frame t ~width ~height ~dt;
  ignore (Renderer.render ~full:true t : string)

let make_child ~parent ~x ~y ~w ~h ?focusable () =
  let lpa = Toffee.Style.Length_percentage_auto.length in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_position Absolute
    |> Toffee.Style.set_inset
         {
           left = lpa (Float.of_int x);
           top = lpa (Float.of_int y);
           right = Toffee.Style.Length_percentage_auto.auto;
           bottom = Toffee.Style.Length_percentage_auto.auto;
         }
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int w))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int h))
  in
  let child = Renderable.create ~parent ~style () in
  (match focusable with
  | Some v -> Renderable.set_focusable child v
  | None -> ());
  child

let mouse ?(modifiers = Matrix_input.Keystroke.no_mods) ~x ~y kind :
    Matrix_input.Mouse_report.t =
  {
    kind;
    x;
    y;
    shift = modifiers.shift;
    alt = modifiers.alt;
    ctrl = modifiers.ctrl;
  }

let mouse_press ?(button = Matrix_input.Mouse_report.Left) ?modifiers ~x ~y () =
  mouse ?modifiers ~x ~y (Press button)

let mouse_release ?(button = Matrix_input.Mouse_report.Left) ?modifiers ~x ~y ()
    =
  mouse ?modifiers ~x ~y (Release (Some button))

let mouse_motion ?(left = false) ?modifiers ~x ~y () =
  mouse ?modifiers ~x ~y (Motion (if left then Some Left else None))

let mouse_scroll ?modifiers ~x ~y direction =
  mouse ?modifiers ~x ~y (Scroll direction)

(* Unit-returning dispatch wrappers: these tests assert via tree state, not
   the event [Test_harness.dispatch_mouse]/[dispatch_paste] return. *)
let dispatch_mouse t ev =
  ignore (Test_harness.dispatch_mouse t ev : Event.mouse)

let dispatch_paste t text =
  ignore (Test_harness.dispatch_paste t text : Event.paste)

let record_mouse node =
  let log = ref [] in
  Renderable.on_mouse node (fun ev -> log := ev :: !log);
  log

let record_keys node =
  let log = ref [] in
  Renderable.on_key node (fun ev -> log := ev :: !log);
  log

let key_a = Matrix_input.Keystroke.make (Char (Uchar.of_char 'a'))

(* ── Lifecycle ── *)

let creates_with_default_style () =
  let t = make_renderer () in
  let root = Renderer.root t in
  is_true ~msg:"root exists" (not (Renderable.destroyed root));
  let _screen = Renderer.screen t in
  ()

let creates_with_custom_style () =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 40.)
  in
  let t = Renderer.create ~style () in
  let _root = Renderer.root t in
  ()

let width_method_reaches_widgets () =
  List.iter
    (fun width_method ->
      let t = Renderer.create ~width_method () in
      is_true ~msg:"owned screen's method"
        (Renderable.width_method (Renderer.root t) = width_method);
      let host = Matrix_screen.create ~width_method () in
      let adopted = Renderer.create ~screen:host () in
      is_true ~msg:"adopted screen's method"
        (Renderable.width_method (Renderer.root adopted) = width_method))
    [ `Wcwidth; `Unicode_wide ]

let adopted_screen_render_is_guarded () =
  let host = Matrix_screen.create () in
  let t = Renderer.create ~screen:host () in
  let _child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:4 ~h:2 () in
  Renderer.render_frame t ~width:10 ~height:4 ~dt:0.;
  is_true ~msg:"renderer adopted the host screen" (Renderer.screen t == host);
  satisfies ~claim:"frame built into the host's hit grid (hit id > 0)" int
    (fun id -> id > 0)
    (Matrix_screen.Hit_grid.get (Matrix_screen.next_hit_grid host) ~x:1 ~y:1);
  raises_match ~msg:"presentation belongs to the host"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Renderer.render t : string))

let zero_sized_clipping_node_hides_children () =
  (* A zero-width overflow-hidden container must report honest bounds and skip
     its children rather than fabricating a phantom 1-cell scissor. *)
  let t = make_renderer () in
  let hidden =
    Toffee.Geometry.Point.
      { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
  in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_overflow hidden
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 0.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.)
  in
  let container = Renderable.create ~parent:(Renderer.root t) ~style () in
  let child_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 5.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
  in
  let child = Renderable.create ~parent:container ~style:child_style () in
  let child_num = Renderable.uid child in
  do_frame t;
  equal ~msg:"honest zero width" int 0 (Renderable.width container);
  let hit = Matrix_screen.query_hit (Renderer.screen t) ~x:0 ~y:0 in
  is_false ~msg:"child not hit-testable through the empty clip" (hit = child_num)

let starts_dirty () =
  let t = make_renderer () in
  is_true ~msg:"needs_render" (Renderer.needs_render t)

let clean_after_render () =
  let t = make_renderer () in
  do_frame t;
  is_false ~msg:"not needs_render" (Renderer.needs_render t)

let dirty_after_schedule () =
  let t = make_renderer () in
  do_frame t;
  is_false ~msg:"clean before" (Renderer.needs_render t);
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  Renderable.request_render child;
  is_true ~msg:"dirty after schedule" (Renderer.needs_render t)

let render_request_during_frame_survives () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  let requested = ref false in
  Renderer.add_frame_callback t (fun ~dt:_ ->
      if not !requested then begin
        requested := true;
        Renderable.request_render child
      end);
  do_frame t;
  is_true ~msg:"follow-up frame requested" (Renderer.needs_render t);
  is_false ~msg:"renderer remains unsettled" (Renderer.is_settled t);
  do_frame t;
  is_false ~msg:"follow-up frame consumed" (Renderer.needs_render t);
  is_true ~msg:"renderer settled" (Renderer.is_settled t)

let callback_runs_each_frame () =
  let t = make_renderer () in
  let count = ref 0 in
  Renderer.add_frame_callback t (fun ~dt:_ -> incr count);
  do_frame t;
  equal ~msg:"one frame" int 1 !count;
  do_frame t;
  equal ~msg:"two frames" int 2 !count

(* A frame that does not animate, as a host draws early for input, runs no
   frame callback and no live node's on_frame hook, and the next frame that
   animates runs both. *)
let frame_without_animation () =
  let t = make_renderer () in
  let callbacks = ref 0 and hooks = ref 0 in
  Renderer.add_frame_callback t (fun ~dt:_ -> incr callbacks);
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  Renderable.set_live child true;
  Renderable.set_on_frame child (Some (fun _ ~dt:_ -> incr hooks));
  do_frame t;
  Renderer.render_frame ~animate:false t ~width:80 ~height:24 ~dt:0.;
  equal ~msg:"frame callbacks" int 1 !callbacks;
  equal ~msg:"on_frame hooks" int 1 !hooks;
  do_frame t;
  equal ~msg:"frame callbacks, then" int 2 !callbacks;
  equal ~msg:"on_frame hooks, then" int 2 !hooks

let callback_receives_delta () =
  let t = make_renderer () in
  let received = ref 0.0 in
  Renderer.add_frame_callback t (fun ~dt -> received := dt);
  do_frame ~dt:42.0 t;
  is_true ~msg:"delta" (Float.equal !received 42.0)

let remove_callback_stops_it () =
  let t = make_renderer () in
  let count = ref 0 in
  let f ~dt =
    ignore (dt : float);
    incr count
  in
  Renderer.add_frame_callback t f;
  do_frame t;
  equal ~msg:"ran once" int 1 !count;
  Renderer.remove_frame_callback t f;
  do_frame t;
  equal ~msg:"not called again" int 1 !count

let clear_removes_all_callbacks () =
  let t = make_renderer () in
  let c1 = ref 0 in
  let c2 = ref 0 in
  Renderer.add_frame_callback t (fun ~dt:_ -> incr c1);
  Renderer.add_frame_callback t (fun ~dt:_ -> incr c2);
  do_frame t;
  equal ~msg:"c1 ran" int 1 !c1;
  equal ~msg:"c2 ran" int 1 !c2;
  Renderer.clear_frame_callbacks t;
  do_frame t;
  equal ~msg:"c1 not called" int 1 !c1;
  equal ~msg:"c2 not called" int 1 !c2

(* ── Settlement ── *)

type async_highlight_job = {
  notify : unit -> unit;
  mutable outcome : unit Code.Highlighter.result option;
}

let async_highlighter () =
  let jobs = ref [] in
  let highlighter =
    Code.Highlighter.async (fun _request ~notify ->
        let job = { notify; outcome = None } in
        jobs := !jobs @ [ job ];
        Code.Highlighter.job
          ~poll:(fun () ->
            match job.outcome with
            | None -> None
            | Some outcome ->
                job.outcome <- None;
                Some outcome)
          ~cancel:(fun () -> ()))
  in
  (jobs, highlighter)

let complete_highlight job outcome =
  job.outcome <- Some (Result.map (fun captures -> (captures, ())) outcome);
  job.notify ()

let pending_provider_reports_work () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  Renderable.set_pending_provider child
    (Some
       (fun () ->
         Some (Renderable.Pending.make ~label:"example" ~kind:"test.work" ())));
  match Renderer.pending_work t with
  | [ pending ] ->
      is_true ~msg:"node" (Renderer.Pending.node pending == child);
      let work = Renderer.Pending.work pending in
      equal ~msg:"kind" string "test.work" work.kind;
      equal ~msg:"label" (option string) (Some "example") work.label
  | pending ->
      fail
        (Printf.sprintf "expected one pending work item, got %d"
           (List.length pending))

let render_frame_until_settled_reports_pending_code () =
  let t = make_renderer () in
  let _jobs, highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _code =
    Code.create ~parent:(Renderer.root t)
      (Code.Props.make ~content:"let x" ~syntax ())
  in
  match
    Renderer.render_frame_until_settled ~max_passes:1 t ~width:40 ~height:5
      ~dt:0.
  with
  | `Settled -> fail "expected pending highlighting"
  | `Pending [ pending ] ->
      let work = Renderer.Pending.work pending in
      equal ~msg:"kind" string "code.highlight" work.kind;
      equal ~msg:"label" (option string) (Some "ocaml") work.label
  | `Pending pending ->
      fail
        (Printf.sprintf "expected one pending work item, got %d"
           (List.length pending))

let render_frame_until_settled_completes_code () =
  let t = make_renderer () in
  let jobs, highlighter = async_highlighter () in
  let highlights =
    Syntax_highlight.of_triples ~content:"let x" [ (0, 3, "keyword") ]
  in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let code =
    Code.create ~parent:(Renderer.root t)
      (Code.Props.make ~content:"let x" ~syntax ())
  in
  complete_highlight (List.hd !jobs) (Ok highlights);
  match Renderer.render_frame_until_settled t ~width:40 ~height:5 ~dt:0. with
  | `Pending pending ->
      fail
        (Printf.sprintf "expected settlement, got %d pending item(s)"
           (List.length pending))
  | `Settled ->
      is_false ~msg:"not highlighting" (Code.is_highlighting code);
      equal ~msg:"pending work" int 0 (List.length (Renderer.pending_work t))

let settlement_replaces_superseded_frame_cells () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:1 ~h:1 () in
  let passes = ref 0 in
  Renderable.set_render child (fun self grid ~dt:_ ->
      incr passes;
      if !passes = 1 then begin
        Matrix_grid.draw_text grid ~x:0 ~y:0 ~text:"X";
        Renderable.request_render self
      end);
  (match Renderer.render_frame_until_settled t ~width:1 ~height:1 ~dt:0. with
  | `Settled -> ()
  | `Pending pending ->
      fail
        (Printf.sprintf "expected settlement, got %d pending item(s)"
           (List.length pending)));
  equal ~msg:"render passes" int 2 !passes;
  let grid = Matrix_screen.next_grid (Renderer.screen t) in
  equal ~msg:"superseded glyph cleared" string " " (Matrix_grid.get_text grid 0)

let presented_cells_cleared_before_next_frame () =
  (* The post-present clear of the next buffer is deferred by Screen; a
     glyph drawn in a presented frame must not leak into the following
     frame's canvas. *)
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:1 ~h:1 () in
  let draw = ref true in
  Renderable.set_render child (fun _ grid ~dt:_ ->
      if !draw then Matrix_grid.draw_text grid ~x:0 ~y:0 ~text:"X");
  Renderer.render_frame t ~width:1 ~height:1 ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen t) in
  equal ~msg:"first frame drew" string "X" (Matrix_grid.get_text grid 0);
  ignore (Renderer.render ~full:true t : string);
  draw := false;
  Renderable.request_render child;
  Renderer.render_frame t ~width:1 ~height:1 ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen t) in
  equal ~msg:"stale glyph cleared" string " " (Matrix_grid.get_text grid 0)

(* ── Viewport culling ── *)

let overflow_visible_descendant_reaching_viewport_renders () =
  let t = make_renderer () in
  let parent = make_child ~parent:(Renderer.root t) ~x:80 ~y:0 ~w:10 ~h:5 () in
  let child = make_child ~parent ~x:(-80) ~y:0 ~w:10 ~h:5 () in
  let renders = ref 0 in
  Renderable.set_render child (fun _ _ ~dt:_ -> incr renders);
  do_frame ~width:20 ~height:10 t;
  equal ~msg:"overflow-visible descendant callback" int 1 !renders

let clipped_offscreen_subtree_is_not_traversed_for_rendering () =
  let t = make_renderer () in
  let hidden = Toffee.Style.Overflow.Hidden in
  let parent_style =
    Toffee.Style.default
    |> Toffee.Style.set_position Absolute
    |> Toffee.Style.set_inset
         {
           left = Toffee.Style.Length_percentage_auto.length 80.;
           top = Toffee.Style.Length_percentage_auto.length 0.;
           right = Toffee.Style.Length_percentage_auto.auto;
           bottom = Toffee.Style.Length_percentage_auto.auto;
         }
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 5.)
    |> Toffee.Style.set_overflow { x = hidden; y = hidden }
  in
  let parent =
    Renderable.create ~parent:(Renderer.root t) ~style:parent_style ()
  in
  let child = make_child ~parent ~x:(-80) ~y:0 ~w:10 ~h:5 () in
  let renders = ref 0 in
  Renderable.set_render child (fun _ _ ~dt:_ -> incr renders);
  do_frame ~width:20 ~height:10 t;
  equal ~msg:"clipped descendant callback" int 0 !renders

(* A scroll box whose rows report render invocations. Rows are 1 cell tall,
   the viewport is [vh] rows, scrollbars are hidden so rows span the full
   width. *)
let make_culled_transcript ?(rows = 40) ?(vw = 20) ?(vh = 5) t =
  let box_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int vh))
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style:box_style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let row_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
  in
  let renders = Array.make rows 0 in
  let row_nodes =
    Array.init rows (fun i ->
        let row =
          Renderable.create ~parent:(Scroll_box.node sb) ~style:row_style ()
        in
        Renderable.set_render row (fun _ _ ~dt:_ ->
            renders.(i) <- renders.(i) + 1);
        row)
  in
  (sb, renders, row_nodes)

let scroll_box_culls_offscreen_children () =
  let t = make_renderer () in
  let _sb, renders, _rows = make_culled_transcript t in
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"first visible row rendered" int 1 renders.(0);
  equal ~msg:"last visible row rendered" int 1 renders.(4);
  equal ~msg:"offscreen row not rendered" int 0 renders.(20);
  equal ~msg:"final row not rendered" int 0 renders.(39)

let scrolling_paints_revealed_children () =
  let t = make_renderer () in
  let sb, renders, _rows = make_culled_transcript t in
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"row 20 culled before scroll" int 0 renders.(20);
  Scroll_box.scroll_to sb ~y:20 ();
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"row 20 painted after scroll" int 1 renders.(20);
  equal ~msg:"row 24 painted after scroll" int 1 renders.(24);
  equal ~msg:"row 0 culled after scroll" int 1 renders.(0)

let culled_child_is_not_hit_testable () =
  let t = make_renderer () in
  let sb, _renders, rows = make_culled_transcript t in
  let count_downs log =
    List.length
      (List.filter
         (fun ev ->
           match Event.Mouse.kind ev with
           | Event.Mouse.Down _ -> true
           | _ -> false)
         !log)
  in
  let hits_visible = record_mouse rows.(2) in
  let hits_hidden = record_mouse rows.(22) in
  do_frame ~width:20 ~height:5 t;
  dispatch_mouse t (mouse_press ~x:3 ~y:2 ());
  equal ~msg:"visible row hit" int 1 (count_downs hits_visible);
  equal ~msg:"hidden row not hit" int 0 (count_downs hits_hidden);
  Scroll_box.scroll_to sb ~y:20 ();
  do_frame ~width:20 ~height:5 t;
  dispatch_mouse t (mouse_press ~x:3 ~y:2 ());
  equal ~msg:"revealed row hit after scroll" int 1 (count_downs hits_hidden)

let culled_live_child_does_not_tick () =
  let t = make_renderer () in
  let sb, _renders, rows = make_culled_transcript t in
  let visible_ticks = ref 0 and hidden_ticks = ref 0 in
  Renderable.set_live rows.(1) true;
  Renderable.set_on_frame rows.(1) (Some (fun _ ~dt:_ -> incr visible_ticks));
  Renderable.set_live rows.(30) true;
  Renderable.set_on_frame rows.(30) (Some (fun _ ~dt:_ -> incr hidden_ticks));
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"visible live row ticked" int 1 !visible_ticks;
  equal ~msg:"culled live row did not tick" int 0 !hidden_ticks;
  Scroll_box.scroll_to sb ~y:30 ();
  do_frame ~width:20 ~height:5 t;
  equal ~msg:"revealed live row ticks" int 1 !hidden_ticks

(* ── Viewport culling: what the intersection test keeps ──

   Culling decides which children a frame descends into, so it answers "can
   anything under this child paint inside the viewport", not "is this child's
   own box visible". Overflow is visible by default: a
   child's subtree is free to paint outside the child's box, and a box with
   zero extent on an axis has no interior at all — every one of its
   descendants paints outside it. *)

let dim n = Toffee.Style.Dimension.length (Float.of_int n)

(* A column of children with the given (width, height) extents. Culling reads
   cached layouts, so a frame runs before the predicate is asked. *)
let culling_column t extents =
  let parent_style =
    Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_width (dim 10)
    |> Toffee.Style.set_height (dim 20)
  in
  let parent =
    Renderable.create ~parent:(Renderer.root t) ~style:parent_style ()
  in
  let children =
    List.map
      (fun (w, h) ->
        let style =
          Toffee.Style.default
          |> Toffee.Style.set_width (dim w)
          |> Toffee.Style.set_height (dim h)
          |> Toffee.Style.set_flex_shrink 0.
        in
        Renderable.create ~parent ~style ())
      extents
  in
  do_frame ~width:20 ~height:20 t;
  (parent, Array.of_list children)

(* The children of the culling [parent] that a frame descends into, in the
   order it renders them, when the root clips its children to the viewport
   and the frame holds the viewport, so that the viewport alone decides. A
   frame does not cull to an empty viewport: it skips the children. *)
let kept_in t ~parent ~x ~y ~width ~height =
  assert (x >= 0 && y >= 0 && width > 0 && height > 0);
  let root = Renderer.root t in
  let rendered = ref [] in
  List.iter
    (fun child ->
      Renderable.set_render child (fun self _ ~dt:_ ->
          rendered := self :: !rendered))
    (Renderable.children parent);
  Renderable.set_viewport_cull parent true;
  Renderable.set_style root
    (Toffee.Style.set_overflow
       { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
       (Renderable.style root));
  Renderable.set_child_clip root
    (Some (fun _ -> Some { Matrix_grid.x; y; width; height }));
  Renderer.render_frame t
    ~width:(max 20 (x + width))
    ~height:(max 20 (y + height))
    ~dt:0.;
  Renderable.set_child_clip root None;
  List.rev !rendered

let zero_height_child_on_the_viewport_edge_is_kept () =
  (* Rows: a zero-height one at y=0, then a one-cell row also at y=0. The
     first row's leading edge coincides with the viewport's, and its trailing
     edge does too, so a strict overlap test drops it — along with every
     descendant that would have painted into the viewport. *)
  let t = make_renderer () in
  let parent, kids = culling_column t [ (10, 0); (10, 1) ] in
  let kept = kept_in t ~parent ~x:0 ~y:0 ~width:10 ~height:20 in
  is_true ~msg:"zero-height child on the viewport's top edge is traversed"
    (List.memq kids.(0) kept)

let zero_width_child_on_the_viewport_edge_is_kept () =
  (* Cross axis, same shape: a zero-width child sits flush with the
     viewport's left edge, which every child of a column does. *)
  let t = make_renderer () in
  let parent, kids = culling_column t [ (10, 1); (0, 1) ] in
  let kept = kept_in t ~parent ~x:0 ~y:0 ~width:10 ~height:20 in
  is_true ~msg:"zero-width child on the viewport's left edge is traversed"
    (List.memq kids.(1) kept)

let child_straddling_the_viewport_edge_is_kept () =
  (* Rows 0..2 and 2..5 against a viewport of rows 0..3: the second row is
     half in. Nonzero extents overlap strictly, so this one is safe. *)
  let t = make_renderer () in
  let parent, kids = culling_column t [ (10, 2); (10, 3) ] in
  let kept = kept_in t ~parent ~x:0 ~y:0 ~width:10 ~height:3 in
  is_true ~msg:"partially visible child is traversed" (List.memq kids.(1) kept)

let child_ending_on_the_viewport_edge_is_culled () =
  (* Row 0..2 against a viewport starting at y=2: nothing of it, and nothing
     under it, can reach the viewport. Culling it is the point. *)
  let t = make_renderer () in
  let parent, kids = culling_column t [ (10, 2); (10, 3) ] in
  let kept = kept_in t ~parent ~x:0 ~y:2 ~width:10 ~height:3 in
  is_false ~msg:"child entirely above the viewport is culled"
    (List.memq kids.(0) kept)

let child_far_before_the_viewport_is_kept () =
  (* A tall child sorts first and reaches the viewport, but 200 one-row
     siblings that end above the viewport sort between it and the viewport.
     Culling must not give up on it after a run of misses. *)
  let t = make_renderer () in
  let parent = culling_column t (List.init 300 (fun _ -> (10, 1))) |> fst in
  let tall = make_child ~parent ~x:0 ~y:0 ~w:10 ~h:300 () in
  do_frame ~width:20 ~height:20 t;
  let kept =
    kept_in t ~parent ~x:0 ~y:(Renderable.y parent + 200) ~width:10 ~height:10
  in
  is_true ~msg:"tall child reaching the viewport is traversed"
    (List.memq tall kept)

(* Children at arbitrary positions, some overlapping, against arbitrary
   viewports: culling keeps exactly the children whose box meets the viewport
   under the overlap rule, in z order, then in child order. *)
let culling_matches_a_scan =
  let gen =
    Gen.pair
      (Gen.list ~size:(Gen.int_range 0 60)
         (Gen.quad (Gen.int_range 0 9) (Gen.int_range (-5) 60)
            (Gen.int_range 0 12) (Gen.int_range 0 3)))
      (Gen.quad (Gen.int_range (-5) 15) (Gen.int_range (-5) 65)
         (Gen.int_range 1 12) (Gen.int_range 1 12))
  in
  prop ~count:200 "culling matches a scan of every child" gen
    (fun (children, (vx, vy, vw, vh)) ->
      let t = make_renderer () in
      (* The column is placed 10 cells in, so that viewports and children
         reach before it and stay on the frame. *)
      let ten = Toffee.Style.Length_percentage.length 10. in
      let zero = Toffee.Style.Length_percentage.length 0. in
      Renderable.set_style (Renderer.root t)
        (Toffee.Style.set_padding
           { left = ten; top = ten; right = zero; bottom = zero }
           (Renderable.style (Renderer.root t)));
      let parent, _ = culling_column t [] in
      let nodes =
        List.map
          (fun (x, y, h, z_index) ->
            let child = make_child ~parent ~x ~y ~w:(1 + (x mod 3)) ~h () in
            Renderable.set_z_index child z_index;
            child)
          children
      in
      do_frame ~width:20 ~height:20 t;
      let overlaps a0 a1 lo hi =
        if a0 = a1 then a0 >= lo && a0 <= hi else a1 > lo && a0 < hi
      in
      let vx = vx + 10 and vy = vy + 10 in
      let expected =
        List.filter
          (fun child ->
            let b = Renderable.bounds child in
            overlaps b.y (b.y + b.height) vy (vy + vh)
            && overlaps b.x (b.x + b.width) vx (vx + vw))
          nodes
        |> List.stable_sort (fun a b ->
            Int.compare (Renderable.z_index a) (Renderable.z_index b))
      in
      let kept = kept_in t ~parent ~x:vx ~y:vy ~width:vw ~height:vh in
      is_true ~msg:"same children in the same order"
        (List.length kept = List.length expected
        && List.for_all2 ( == ) kept expected))

(* ── Node hooks and the tree's liveness ── *)

(* A lifecycle pass runs at each frame while its node is attached, before
   the layout, and stops once the node leaves the tree. *)
let lifecycle_pass_runs_each_frame () =
  let t = make_renderer () in
  let node = Renderable.create ~parent:(Renderer.root t) () in
  let passes = ref 0 in
  Renderable.set_on_lifecycle_pass node (Some (fun _ -> incr passes));
  do_frame t;
  do_frame t;
  equal ~msg:"each frame" int 2 !passes;
  Renderable.detach node;
  do_frame t;
  equal ~msg:"not once detached" int 2 !passes

(* The tree is live while one of its nodes is, and its observer hears each
   node that starts or stops being live. *)
let tree_liveness () =
  let t = make_renderer () in
  let heard = ref [] in
  Renderer.set_on_live_change t
    (Some (fun () -> heard := Renderer.live t :: !heard));
  is_false ~msg:"not live" (Renderer.live t);
  let a = Renderable.create ~parent:(Renderer.root t) ()
  and b = Renderable.create ~parent:(Renderer.root t) () in
  Renderable.set_live a true;
  Renderable.set_live b true;
  is_true ~msg:"live" (Renderer.live t);
  Renderable.set_live a false;
  Renderable.set_live b false;
  is_false ~msg:"not live again" (Renderer.live t);
  equal ~msg:"each change heard" (list bool) [ false; true; true; true ] !heard

(* Nodes are reached by place and known by a number of their own. *)
let child_at_and_uid () =
  let t = make_renderer () in
  let a = Renderable.create ~parent:(Renderer.root t) ()
  and b = Renderable.create ~parent:(Renderer.root t) () in
  is_true ~msg:"first"
    (match Renderable.child_at (Renderer.root t) 0 with
    | Some n -> n == a
    | None -> false);
  is_true ~msg:"second"
    (match Renderable.child_at (Renderer.root t) 1 with
    | Some n -> n == b
    | None -> false);
  is_true ~msg:"past the end" (Renderable.child_at (Renderer.root t) 2 = None);
  is_true ~msg:"numbers of their own" (Renderable.uid a <> Renderable.uid b)

(* ── Viewport culling: chunks ──

   A culling column holds its contained children in chunks in the layout
   tree; every child keeps the box it has in a flat column. Two columns of
   the same style and children, one culling and one not, are changed alike
   and compared box for box. 300 rows span more than one chunk. *)

let chunks_keep_every_box =
  prop ~count:12 "a chunked column lays its children out as a flat one"
    (Gen.int_range 0 1_000_000) (fun seed ->
      let rng = Random.State.make [| seed |] in
      let t = make_renderer () in
      Renderable.set_style (Renderer.root t)
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row
        |> Toffee.Style.set_align_items
             (Some Toffee.Style.Align_items.Flex_start));
      (* A width of 0 sizes the column by its content. *)
      let column_style width =
        Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_flex_shrink 0.
        |> Toffee.Style.set_width
             (if width = 0 then Toffee.Style.Dimension.auto else dim width)
      in
      let column () =
        Renderable.create ~parent:(Renderer.root t) ~style:(column_style 30) ()
      in
      let chunked = column () and flat = column () in
      Renderable.set_viewport_cull chunked true;
      let lpa = Toffee.Style.Length_percentage_auto.length in
      let one = Toffee.Style.Length_percentage.length 1. in
      let rect ?(left = lpa 0.) ?(top = lpa 0.) () : _ Toffee.Geometry.Rect.t =
        { left; right = lpa 0.; top; bottom = lpa 0. }
      in
      (* Each row is a box holding a wrapped text. Contained kinds first: no
         style, a border, a margin, a fractional height, a relative offset; then percentage
         sizes, a growing box, an absolute box and an [auto] top margin. *)
      let styled kind =
        let s = Toffee.Style.default |> Toffee.Style.set_flex_shrink 0. in
        match kind with
        | 1 ->
            Toffee.Style.set_border
              { left = one; right = one; top = one; bottom = one }
              s
        | 2 -> Toffee.Style.set_margin (rect ~left:(lpa 2.) ~top:(lpa 1.) ()) s
        | 3 ->
            s
            |> Toffee.Style.set_width (dim 7)
            |> Toffee.Style.set_aspect_ratio (Some 3.)
        | 4 ->
            s
            |> Toffee.Style.set_position Relative
            |> Toffee.Style.set_inset (rect ~top:(lpa (-1.)) ())
        | 5 ->
            s
            |> Toffee.Style.set_min_width (Toffee.Style.Dimension.percent 0.3)
            |> Toffee.Style.set_max_width (Toffee.Style.Dimension.percent 0.8)
            |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 0.2)
        | 6 -> Toffee.Style.set_flex_grow 1. s
        | 7 ->
            s
            |> Toffee.Style.set_position Absolute
            |> Toffee.Style.set_inset (rect ~left:(lpa 3.) ~top:(lpa 5.) ())
        | _ ->
            Toffee.Style.set_margin
              (rect ~top:Toffee.Style.Length_percentage_auto.auto ())
              s
      in
      let words () =
        String.concat " "
          (List.init
             (1 + Random.State.int rng 8)
             (fun _ -> String.make (1 + Random.State.int rng 6) 'w'))
      in
      let add ~index kind text =
        List.iter
          (fun parent ->
            let row =
              Renderable.create ~parent ~index ~style:(styled kind) ()
            in
            ignore
              (Text.create ~parent:row
                 (Text.Props.make ~content:text ~wrap:`Word ())
                : Text.t))
          [ chunked; flat ]
      in
      let row parent i = Option.get (Renderable.child_at parent i) in
      let count () = List.length (Renderable.children flat) in
      let kind () =
        if Random.State.int rng 4 > 0 then Random.State.int rng 5
        else 5 + Random.State.int rng 4
      in
      for i = 0 to 299 do
        add ~index:i (kind ()) (words ())
      done;
      (* A hidden node keeps the box of the last frame that showed it. *)
      let rec boxes node =
        if not (Renderable.visible node) then []
        else
          let b = Renderable.bounds node in
          (b.x, b.y, b.width, b.height)
          :: List.concat_map boxes (Renderable.children node)
      in
      let relative parent =
        Renderable.refresh_positions parent;
        let b = Renderable.bounds parent in
        List.map
          (fun (x, y, w, h) -> (x - b.x, y - b.y, w, h))
          (List.concat_map boxes (Renderable.children parent))
      in
      let same = ref true in
      for _ = 1 to 16 do
        (match Random.State.int rng 9 with
        | 0 ->
            for _ = 1 to 1 + Random.State.int rng 40 do
              add ~index:(count ()) (kind ()) (words ())
            done
        | 1 -> add ~index:(Random.State.int rng (count ())) (kind ()) (words ())
        | 2 ->
            let i = Random.State.int rng (count ()) in
            List.iter
              (fun p -> Renderable.destroy_recursively (row p i))
              [ chunked; flat ]
        | 3 ->
            let i = Random.State.int rng (count ()) and k = kind () in
            List.iter
              (fun p -> Renderable.set_style (row p i) (styled k))
              [ chunked; flat ]
        | 4 ->
            let i = Random.State.int rng (count ()) in
            List.iter
              (fun p ->
                let r = row p i in
                Renderable.set_visible r (not (Renderable.visible r)))
              [ chunked; flat ]
        | 5 ->
            (* A capped log: the first rows leave, as many come at the end. *)
            let n = 1 + Random.State.int rng 3 in
            for _ = 1 to n do
              List.iter
                (fun p -> Renderable.destroy_recursively (row p 0))
                [ chunked; flat ];
              add ~index:(count ()) (kind ()) (words ())
            done
        | 7 ->
            (* Rows leave together, as a render's stale nodes do. *)
            let picks =
              List.sort_uniq compare
                (List.init
                   (1 + Random.State.int rng 3)
                   (fun _ -> Random.State.int rng (count ())))
            in
            List.iter
              (fun p -> Renderable.detach_all (List.map (row p) picks))
              [ chunked; flat ]
        | 6 ->
            (* The last row moves up to a random place. *)
            let i = Random.State.int rng (count ()) in
            List.iter
              (fun p ->
                let children = Renderable.children p in
                let tail = List.filteri (fun j _ -> j >= i) children in
                let last = List.nth tail (List.length tail - 1) in
                let order = last :: List.filter (fun c -> c != last) tail in
                Renderable.place_children p i (fun f -> List.iter f order))
              [ chunked; flat ]
        | _ ->
            let w =
              if Random.State.bool rng then 0 else 10 + Random.State.int rng 40
            in
            List.iter
              (fun p -> Renderable.set_style p (column_style w))
              [ chunked; flat ]);
        do_frame ~width:120 ~height:20 t;
        if relative chunked <> relative flat then same := false;
        (* Culling keeps the rows whose boxes, their text's included, meet a
           viewport. *)
        let top = Renderable.y chunked + Random.State.int rng 400 in
        let meets node =
          List.exists
            (fun (x, y, w, h) ->
              let overlaps a0 a1 lo hi =
                if a0 = a1 then a0 >= lo && a0 <= hi else a1 > lo && a0 < hi
              in
              overlaps y (y + h) top (top + 20) && overlaps x (x + w) 0 120)
            (boxes node)
        in
        let expected = List.filter meets (Renderable.children chunked) in
        let kept =
          kept_in t ~parent:chunked ~x:0 ~y:top ~width:120 ~height:20
        in
        if
          List.sort compare (List.map Renderable.uid kept)
          <> List.sort compare (List.map Renderable.uid expected)
        then same := false
      done;
      is_true ~msg:"every box as in the flat column, culled as a scan" !same)

(* A member that stops being contained leaves its chunk: an absolute row
   is placed against the column, as in a flat column, and not against the
   chunk that held it. *)
let member_leaves_its_chunk () =
  let t = make_renderer () in
  let column () =
    Renderable.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_width (dim 10))
      ()
  in
  let chunked = column () and flat = column () in
  Renderable.set_viewport_cull chunked true;
  let row =
    Toffee.Style.default
    |> Toffee.Style.set_height (dim 1)
    |> Toffee.Style.set_flex_shrink 0.
  in
  let rows p =
    Array.init 600 (fun _ -> Renderable.create ~parent:p ~style:row ())
  in
  let a = rows chunked and b = rows flat in
  do_frame ~width:20 ~height:20 t;
  let lpa = Toffee.Style.Length_percentage_auto.length in
  let absolute =
    row
    |> Toffee.Style.set_position Absolute
    |> Toffee.Style.set_inset
         { left = lpa 0.; right = lpa 0.; top = lpa 5.; bottom = lpa 0. }
  in
  Renderable.set_style a.(450) absolute;
  Renderable.set_style b.(450) absolute;
  do_frame ~width:20 ~height:20 t;
  equal ~msg:"placed against the column" int
    (Renderable.y b.(450) - Renderable.y flat)
    (Renderable.y a.(450) - Renderable.y chunked)

(* A chunk's member grows while the chunk's first member keeps its layout:
   culling finds the grown member where it now is, and the member after it
   where it moved. *)
let chunk_follows_a_grown_member () =
  let t = make_renderer () in
  let column =
    Renderable.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
        |> Toffee.Style.set_width (dim 10))
      ()
  in
  Renderable.set_viewport_cull column true;
  let row h =
    Toffee.Style.default
    |> Toffee.Style.set_width (dim 10)
    |> Toffee.Style.set_height (dim h)
    |> Toffee.Style.set_flex_shrink 0.
  in
  let rows =
    Array.init 10 (fun _ -> Renderable.create ~parent:column ~style:(row 1) ())
  in
  do_frame ~width:20 ~height:20 t;
  Renderable.set_style rows.(4) (row 5);
  do_frame ~width:20 ~height:20 t;
  (* Row 4 spans rows 4 to 9 of the column, row 5 starts at 9. *)
  let kept =
    kept_in t ~parent:column ~x:0
      ~y:(Renderable.y column + 7)
      ~width:10 ~height:1
  in
  is_true ~msg:"the grown member is kept" (List.memq rows.(4) kept);
  is_false ~msg:"the moved member is culled" (List.memq rows.(5) kept);
  is_true ~msg:"the moved member is kept where it is"
    (List.memq rows.(5)
       (kept_in t ~parent:column ~x:0
          ~y:(Renderable.y column + 9)
          ~width:10 ~height:1))

(* ── Viewport culling: frames ── *)

(* One frame's rows. The grid is read before presentation swaps the screen's
   buffers, so this renders rather than reusing [do_frame]. *)
let frame_rows t ~width ~height =
  Renderer.render_frame t ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen t) in
  String.concat "\n"
    (List.init height (fun y ->
         String.concat ""
           (List.init width (fun x ->
                match Matrix_grid.get_text grid ((y * width) + x) with
                | "" -> " "
                | s -> s))))

let label node text =
  Renderable.set_render node (fun self grid ~dt:_ ->
      Matrix_grid.draw_text grid ~x:(Renderable.x self) ~y:(Renderable.y self)
        ~text)

let row ~parent ~width text =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (dim width)
    |> Toffee.Style.set_height (dim 1)
    |> Toffee.Style.set_flex_shrink 0.
  in
  let node = Renderable.create ~parent ~style () in
  label node text;
  node

let zero_width_wrapper_keeps_its_overflowing_row () =
  (* The spice witness shape: a header, a flex-grown scroll box, a footer, in
     a frame short enough that the box gets exactly three rows. The box's
     first child is a wrapper with no width of its own — a gutter, a badge
     layer, anything whose content sizes itself — holding a row that paints
     past it. Geometry puts that row on frame row 1; culling blanks it while
     the rows around it are untouched. *)
  let t = make_renderer () in
  Renderable.set_style (Renderer.root t)
    (Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_width (dim 20)
    |> Toffee.Style.set_height (dim 5));
  ignore (row ~parent:(Renderer.root t) ~width:20 "HEADER");
  let sb =
    Scroll_box.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_grow 1.
        |> Toffee.Style.set_width (dim 20)
        |> Toffee.Style.set_height (dim 0))
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let wrapper =
    Renderable.create ~parent:(Scroll_box.node sb)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row
        |> Toffee.Style.set_width (dim 0)
        |> Toffee.Style.set_flex_shrink 0.)
      ()
  in
  ignore (row ~parent:wrapper ~width:6 "inside");
  ignore (row ~parent:(Scroll_box.node sb) ~width:20 "body-1");
  ignore (row ~parent:(Scroll_box.node sb) ~width:20 "body-2");
  ignore (row ~parent:(Renderer.root t) ~width:20 "FOOTER");
  equal ~msg:"the wrapper's row occupies frame row 1" string
    "HEADER              \n\
     inside              \n\
     body-1              \n\
     body-2              \n\
     FOOTER              "
    (frame_rows t ~width:20 ~height:5)

let descendant_reaching_the_viewport_survives_its_offscreen_parent () =
  (* [overflow_visible_descendant_reaching_viewport_renders] states the
     contract for an ordinary parent. A scroll box's content node must not
     weaken it: culling tests where a child's subtree may paint, not the
     child's own box, so a descendant positioned back into view survives its
     offscreen parent. *)
  let t = make_renderer () in
  Renderable.set_style (Renderer.root t)
    (Toffee.Style.default
    |> Toffee.Style.set_width (dim 20)
    |> Toffee.Style.set_height (dim 3));
  let sb =
    Scroll_box.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_width (dim 20)
        |> Toffee.Style.set_height (dim 3))
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let rows =
    Array.init 8 (fun i ->
        row ~parent:(Scroll_box.node sb) ~width:20 (Printf.sprintf "row-%d" i))
  in
  let escapee = make_child ~parent:rows.(6) ~x:8 ~y:(-5) ~w:6 ~h:1 () in
  label escapee "HERE";
  equal ~msg:"the escapee lands on frame row 1" string
    "row-0               \nrow-1   HERE        \nrow-2               "
    (frame_rows t ~width:20 ~height:3)

let selection_reads_rows_out_of_view () =
  (* Ten rows, each a text in a box, are painted at the bottom of a scroll
     box and then at its top. Ten more are inserted above them while the view
     stays at the top, so the first ten are never painted where they now are.
     A selection of the five rows in view selects those five alone. *)
  let t = make_renderer () in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_width (dim 20)
        |> Toffee.Style.set_height (dim 5))
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let row index text =
    let wrapper =
      Renderable.create ~parent:(Scroll_box.node sb) ~index
        ~style:(Toffee.Style.default |> Toffee.Style.set_flex_shrink 0.)
        ()
    in
    ignore
      (Text.create ~parent:wrapper
         (Text.Props.make ~content:text ~selectable:true ())
        : Text.t)
  in
  for i = 0 to 9 do
    row i (Printf.sprintf "old-%d" i)
  done;
  Scroll_box.scroll_to sb ~y:5 ();
  do_frame ~width:20 ~height:5 t;
  Scroll_box.scroll_to sb ~y:0 ();
  do_frame ~width:20 ~height:5 t;
  for i = 0 to 9 do
    row i (Printf.sprintf "new-%d" i)
  done;
  do_frame ~width:20 ~height:5 t;
  dispatch_mouse t (mouse_press ~x:0 ~y:0 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:19 ~y:4 ());
  equal ~msg:"the rows in view" (option string)
    (Some "new-0new-1new-2new-3new-4")
    (Renderer.selection_text t)

let selection_reads_the_top_layer () =
  (* A node in the top layer, which holds a text, is declared in a
     scroll box three cells from the left, a row from the top, and scrolled by
     two rows. Laid out against the root, it is painted at the root's origin,
     out of the scroll box's clip. A drag from the text's first cell to a
     cell of the next row, where no node takes the pointer, selects that text
     alone: the selection grows to the node in the top layer, and reads its
     place and its text's from where they are painted. *)
  let t = make_renderer () in
  let lpa = Toffee.Style.Length_percentage_auto.length in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_width (dim 12)
        |> Toffee.Style.set_height (dim 3)
        |> Toffee.Style.set_margin
             { left = lpa 3.; right = lpa 0.; top = lpa 1.; bottom = lpa 0. })
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  for i = 0 to 9 do
    ignore
      (Text.create ~parent:(Scroll_box.node sb)
         (Text.Props.make
            ~content:(Printf.sprintf "row-%d" i)
            ~selectable:true ())
        : Text.t)
  done;
  let top =
    Renderable.create ~parent:(Scroll_box.node sb)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_position Toffee.Style.Position.Absolute)
      ()
  in
  ignore
    (Text.create ~parent:top
       (Text.Props.make ~content:"copy me" ~selectable:true ())
      : Text.t);
  Renderer.set_modal t (Some top);
  do_frame ~width:20 ~height:9 t;
  Scroll_box.scroll_to sb ~y:2 ();
  do_frame ~width:20 ~height:9 t;
  equal ~msg:"scrolled" int (-2)
    (snd (Renderable.translate (Scroll_box.content sb)));
  let frame = frame_rows t ~width:20 ~height:9 in
  let rows = String.split_on_char '\n' frame in
  let y, x =
    let rec find y = function
      | [] -> failf "the text is not painted:\n%s" frame
      | row :: rest -> (
          match String.index_opt row 'c' with
          | Some x
            when x + 7 <= String.length row && String.sub row x 7 = "copy me" ->
              (y, x)
          | _ -> find (y + 1) rest)
    in
    find 0 rows
  in
  equal ~msg:"painted at the root's origin" (pair int int) (0, 0) (x, y);
  do_frame ~width:20 ~height:9 t;
  dispatch_mouse t (mouse_press ~x ~y ());
  dispatch_mouse t (mouse_motion ~left:true ~x:(x + 6) ~y:(y + 1) ());
  equal ~msg:"the text under the drag" (option string) (Some "copy me")
    (Renderer.selection_text t)

(* ── Scroll hints ── *)

(* A full-width scroll box of [rows] one-cell text rows in a [vw]x[vh]
   frame, presented once so the diff baseline and scroll metrics are
   established. *)
let make_hint_transcript ?(rows = 20) ?(vw = 20) ?(vh = 6) t =
  let box_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int vh))
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style:box_style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let row_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int vw))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
  in
  for i = 0 to rows - 1 do
    ignore
      (Text.create ~parent:(Scroll_box.node sb) ~style:row_style
         (Text.Props.make
            ~content:(Printf.sprintf "row %d" i)
            ~selectable:false ())
        : Text.t)
  done;
  Renderer.render_frame t ~width:vw ~height:vh ~dt:0.;
  ignore (Renderer.render t : string);
  sb

let scroll_produces_hint () =
  let t = make_renderer () in
  let sb = make_hint_transcript t in
  Scroll_box.scroll_by sb ~y:1 ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  (match Renderer.take_scroll_hint t with
  | Some { Matrix_screen.top; bottom; delta } ->
      equal ~msg:"top" int 0 top;
      equal ~msg:"bottom" int 5 bottom;
      equal ~msg:"delta" int 1 delta
  | None -> fail "expected a scroll hint");
  is_none ~msg:"take consumes the hint" (Renderer.take_scroll_hint t)

let opposing_scrolls_cancel () =
  let t = make_renderer () in
  let sb = make_hint_transcript t in
  Scroll_box.scroll_by sb ~y:2 ();
  Scroll_box.scroll_by sb ~y:(-2) ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  is_none ~msg:"net-zero scroll has no hint" (Renderer.take_scroll_hint t)

let two_scrollers_disqualify () =
  (* Column root so both boxes are onscreen, stacked. *)
  let root_style =
    Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_size (Toffee.Style.Size_dim.pct ~w:100. ~h:100.)
  in
  let t = Renderer.create ~style:root_style () in
  let sb1 = make_hint_transcript ~vh:3 t in
  let sb2 = make_hint_transcript ~vh:3 t in
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  ignore (Renderer.render t : string);
  ignore (Renderer.take_scroll_hint t : Matrix_screen.scroll_hint option);
  Scroll_box.scroll_by sb1 ~y:1 ();
  Scroll_box.scroll_by sb2 ~y:1 ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  is_none ~msg:"two movers disqualify" (Renderer.take_scroll_hint t)

let horizontal_scroll_disqualifies () =
  let t = make_renderer () in
  let box_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 6.)
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style:box_style
      (Scroll_box.Props.make ~scroll_x:true ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 60.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  ignore
    (Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
      : Renderable.t);
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  ignore (Renderer.render t : string);
  Scroll_box.scroll_by sb ~x:1 ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  is_none ~msg:"horizontal scroll has no hint" (Renderer.take_scroll_hint t)

let narrow_box_disqualifies () =
  let t = make_renderer () in
  let sb = make_hint_transcript ~vw:10 t in
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  ignore (Renderer.render t : string);
  Scroll_box.scroll_by sb ~y:1 ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  is_none ~msg:"narrow box has no hint" (Renderer.take_scroll_hint t)

let hinted_scroll_emits_decstbm_and_correct_rows () =
  let t = make_renderer () in
  let sb = make_hint_transcript t in
  let vte = Vte.create ~rows:6 ~cols:20 () in
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  ignore (Vte.feed_string vte (Renderer.render ~full:true t) : _ list);
  Scroll_box.scroll_by sb ~y:1 ();
  Renderer.render_frame t ~width:20 ~height:6 ~dt:0.;
  let out = Renderer.render t in
  contains ~msg:"output contains DECSTBM" ~sub:"\x1b[1;6r" out;
  ignore (Vte.feed_string vte out : _ list);
  let lines = String.split_on_char '\n' (Vte.to_string vte) in
  List.iteri
    (fun i line ->
      equal
        ~msg:(Printf.sprintf "revealed row %d" i)
        string
        (Printf.sprintf "row %d" (i + 1))
        (String.trim line))
    lines

(* ── Layout dirtiness ──

   Layout computation is skipped on clean frames; each mutation kind must
   re-enable it. Every test renders two frames first so the skip is armed
   before the mutation. *)

let sized_child ~parent w h =
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length (Float.of_int w))
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length (Float.of_int h))
  in
  Renderable.create ~parent ~style ()

let style_change_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  do_frame t;
  equal ~msg:"b after a" int 10 (Renderable.x b);
  let style =
    Renderable.style a
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 15.)
  in
  Renderable.set_style a style;
  do_frame t;
  equal ~msg:"a resized" int 15 (Renderable.width a);
  equal ~msg:"b moved" int 15 (Renderable.x b)

let measure_change_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let child = Renderable.create ~parent:(Renderer.root t) () in
  let w = ref 10. in
  Renderable.set_measure child
    (Some
       (fun ~known_dimensions:_ ~available_space:_ ~style:_ ->
         Toffee.Geometry.Size.{ width = !w; height = 1. }));
  do_frame t;
  do_frame t;
  equal ~msg:"measured width" int 10 (Renderable.width child);
  w := 14.;
  (* Content-driven size changes invalidate via mark_dirty, as
     text surfaces do. *)
  Renderable.mark_dirty child;
  do_frame t;
  equal ~msg:"remeasured width" int 14 (Renderable.width child)

let add_remove_child_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  do_frame t;
  do_frame t;
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  equal ~msg:"new child laid out" int 10 (Renderable.x b);
  Renderable.detach a;
  do_frame t;
  equal ~msg:"sibling reflows on removal" int 0 (Renderable.x b)

let resize_relayouts_after_clean_frame () =
  let t = make_renderer () in
  do_frame ~width:80 ~height:24 t;
  do_frame ~width:80 ~height:24 t;
  equal ~msg:"root at 80" int 80 (Renderable.width (Renderer.root t));
  do_frame ~width:100 ~height:30 t;
  equal ~msg:"root at 100" int 100 (Renderable.width (Renderer.root t))

let visibility_toggle_relayouts_after_clean_frame () =
  let t = make_renderer () in
  let a = sized_child ~parent:(Renderer.root t) 10 2 in
  let b = sized_child ~parent:(Renderer.root t) 5 2 in
  do_frame t;
  do_frame t;
  equal ~msg:"b after a" int 10 (Renderable.x b);
  Renderable.set_visible a false;
  do_frame t;
  equal ~msg:"b moves to front" int 0 (Renderable.x b)

(* ── Focus ── *)

let focus_returns_true_for_focusable () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  is_true ~msg:"focus ok" (Renderer.focus t child)

let focus_returns_false_for_non_focusable () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  is_false ~msg:"not focusable" (Renderer.focus t child)

let focused_returns_focused_node () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  match Renderer.focused t with
  | Some n -> is_true ~msg:"same node" (n == child)
  | None -> fail "expected focused node"

let blur_clears_focus () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  Renderer.blur t;
  is_none ~msg:"no focus" (Renderer.focused t)

let focus_different_node_blurs_previous () =
  let t = make_renderer () in
  let a =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let b =
    make_child ~parent:(Renderer.root t) ~x:10 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  ignore (Renderer.focus t a : bool);
  ignore (Renderer.focus t b : bool);
  is_false ~msg:"a blurred" (Renderable.focused a);
  is_true ~msg:"b focused" (Renderable.focused b)

(* A click requests focus; an imperative owner grants it by focusing. *)
let grant_focus_on_request t node =
  Renderable.set_on_focus_request node
    (Some (fun () -> ignore (Renderer.focus t node : bool)))

let left_click_requests_focus () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  grant_focus_on_request t child;
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"child focused" (n == child)
  | None -> fail "expected focus after click"

let left_click_without_request_does_not_focus () =
  let t = make_renderer () in
  let _child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_none ~msg:"a click does not take focus" (Renderer.focused t)

let left_click_on_non_focusable_does_not_focus () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_none ~msg:"no focus" (Renderer.focused t)

let click_walks_up_to_focusable_parent () =
  let t = make_renderer () in
  let parent =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 ~focusable:true
      ()
  in
  grant_focus_on_request t parent;
  let _child = make_child ~parent ~x:2 ~y:2 ~w:5 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:3 ~y:3 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"parent focused" (n == parent)
  | None -> fail "expected parent to be focused"

let left_click_focuses_selectable_node () =
  (* Starting a text selection consumes the Down event; the focus request
     still runs before it. *)
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  grant_focus_on_request t child;
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection started" (Renderer.selection t);
  match Renderer.focused t with
  | Some n -> is_true ~msg:"selectable child focused" (n == child)
  | None -> fail "expected focus after click on selectable node"

let right_click_does_not_auto_focus () =
  let t = make_renderer () in
  let _child =
    make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 ~focusable:true ()
  in
  do_frame t;
  dispatch_mouse t (mouse_press ~button:Right ~x:7 ~y:7 ());
  is_none ~msg:"no focus from right click" (Renderer.focused t)

let shift_left_click_preserves_existing_focus () =
  let t = make_renderer () in
  let composer =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let _target =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  do_frame t;
  ignore (Renderer.focus t composer : bool);
  let modifiers = { Matrix_input.Keystroke.no_mods with shift = true } in
  dispatch_mouse t (mouse_press ~modifiers ~x:25 ~y:2 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"composer remains focused" (n == composer)
  | None -> fail "expected focus to be preserved"

let prevent_default_mouse_down_blocks_auto_focus () =
  let t = make_renderer () in
  let composer =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let target =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  Renderable.on_mouse target (fun ev ->
      match Event.Mouse.kind ev with
      | Event.Mouse.Down { button = Left } -> Event.Mouse.prevent_default ev
      | _ -> ());
  do_frame t;
  ignore (Renderer.focus t composer : bool);
  dispatch_mouse t (mouse_press ~x:25 ~y:2 ());
  match Renderer.focused t with
  | Some n -> is_true ~msg:"composer remains focused" (n == composer)
  | None -> fail "expected focus to be preserved"

let focus_fires_on_node () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  is_true ~msg:"child focused" (Renderable.focused child)

let blur_fires_on_previous () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  ignore (Renderer.focus t child : bool);
  Renderer.blur t;
  is_false ~msg:"child blurred" (Renderable.focused child)

let refocus_fires_blur_then_focus () =
  let t = make_renderer () in
  let a =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let b =
    make_child ~parent:(Renderer.root t) ~x:10 ~y:0 ~w:10 ~h:5 ~focusable:true
      ()
  in
  ignore (Renderer.focus t a : bool);
  is_true ~msg:"a focused" (Renderable.focused a);
  ignore (Renderer.focus t b : bool);
  is_false ~msg:"a blurred" (Renderable.focused a);
  is_true ~msg:"b focused" (Renderable.focused b)

(* ── Input ── *)

let key_dispatches_to_focused () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let log = record_keys child in
  ignore (Renderer.focus t child : bool);
  ignore (Test_harness.dispatch_key t key_a : Event.key);
  equal ~msg:"received one key" int 1 (List.length !log)

let key_does_nothing_when_no_focus () =
  let t = make_renderer () in
  ignore (Test_harness.dispatch_key t key_a : Event.key)

let key_multiple_handlers_run () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let h1 = ref 0 in
  let h2 = ref 0 in
  Renderable.on_key child (fun _ev -> incr h1);
  Renderable.on_key child (fun _ev -> incr h2);
  ignore (Renderer.focus t child : bool);
  ignore (Test_harness.dispatch_key t key_a : Event.key);
  equal ~msg:"handler 1 ran" int 1 !h1;
  equal ~msg:"handler 2 ran" int 1 !h2

let prevent_default_stops_default_handler () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let default_ran = ref false in
  Renderable.on_key child (fun ev -> Event.Key.prevent_default ev);
  Renderable.set_key_behavior child (Some (fun _ev -> default_ran := true));
  ignore (Renderer.focus t child : bool);
  ignore (Test_harness.dispatch_key t key_a : Event.key);
  is_false ~msg:"default not called" !default_ran

let paste_dispatches_to_focused () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let received = ref None in
  Renderable.set_paste_behavior child
    (Some (fun ev -> received := Some (Event.Paste.text ev)));
  ignore (Renderer.focus t child : bool);
  dispatch_paste t "hello";
  match !received with
  | Some s -> equal ~msg:"text" string "hello" s
  | None -> fail "expected paste event"

let paste_does_nothing_when_no_focus () =
  let t = make_renderer () in
  dispatch_paste t "hello"

let paste_contains_correct_text () =
  let t = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 ~focusable:true ()
  in
  let received = ref "" in
  Renderable.set_paste_behavior child
    (Some (fun ev -> received := Event.Paste.text ev));
  ignore (Renderer.focus t child : bool);
  dispatch_paste t "world\n123";
  equal ~msg:"text" string "world\n123" !received

(* A focused child under a parent under the root, each logging the keys its
   handlers see, and the child's behavior, which acts on 'a' only. *)
let key_tree () =
  let t = make_renderer () in
  let log = ref [] in
  let parent = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  let child = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:1 ~focusable:true () in
  let record name node =
    Renderable.on_key node (fun _ -> log := name :: !log)
  in
  record "root" (Renderer.root t);
  record "parent" parent;
  record "child" child;
  Renderable.set_key_behavior child
    (Some
       (fun ev ->
         log := "behavior" :: !log;
         if
           Event.Key.data ev
           = Matrix_input.Keystroke.make (Char (Uchar.of_char 'a'))
         then Event.Key.prevent_default ev));
  ignore (Renderer.focus t child : bool);
  (t, parent, child, log)

let send t key =
  let event = Event.Key.of_input key in
  Renderer.dispatch_key t (Renderer.focus_path t) event;
  event

let unused_key_bubbles_to_ancestors () =
  let t, _, _, log = key_tree () in
  let event = send t (Test_harness.char_key 'b') in
  equal ~msg:"target, behavior, then ancestors innermost first" (list string)
    [ "child"; "behavior"; "parent"; "root" ]
    (List.rev !log);
  is_false ~msg:"nothing consumed it" (Event.Key.propagation_stopped event)

let acting_behavior_consumes () =
  let t, _, _, log = key_tree () in
  let event = send t key_a in
  equal ~msg:"ancestors never see a key the behavior acted on" (list string)
    [ "child"; "behavior" ] (List.rev !log);
  is_true ~msg:"prevented" (Event.Key.default_prevented event);
  is_true ~msg:"stopped" (Event.Key.propagation_stopped event)

let stop_keeps_behavior () =
  let t, _, child, log = key_tree () in
  Renderable.on_key child Event.Key.stop_propagation;
  let _ = send t (Test_harness.char_key 'b') in
  equal ~msg:"stopping hides the key from ancestors, not from the behavior"
    (list string) [ "child"; "behavior" ] (List.rev !log)

let path_is_fixed_behavior_follows_focus () =
  let t, _, child, log = key_tree () in
  let other =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:6 ~w:5 ~h:1 ~focusable:true ()
  in
  Renderable.set_key_behavior other
    (Some (fun _ -> log := "other behavior" :: !log));
  Renderable.on_key child (fun _ -> ignore (Renderer.focus t other : bool));
  let path = Renderer.focus_path t in
  Renderer.dispatch_key t path (Event.Key.of_input (Test_harness.char_key 'b'));
  equal ~msg:"handlers keep the path taken; the behavior runs on the focus now"
    (list string)
    [ "child"; "other behavior"; "parent"; "root" ]
    (List.rev !log);
  log := [];
  Renderable.destroy child;
  Renderer.dispatch_key t path (Event.Key.of_input (Test_harness.char_key 'c'));
  equal ~msg:"a node destroyed since the path was taken is skipped"
    (list string)
    [ "other behavior"; "parent"; "root" ]
    (List.rev !log)

let mouse_bubbles_until_stopped () =
  let t = make_renderer () in
  let log = ref [] in
  let parent = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:5 () in
  let child = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:2 () in
  Renderable.on_mouse (Renderer.root t) (fun _ -> log := "root" :: !log);
  Renderable.on_mouse parent (fun ev ->
      log := "parent" :: !log;
      Event.Mouse.stop_propagation ev);
  Renderable.on_mouse parent (fun _ -> log := "parent, newer" :: !log);
  Renderable.on_mouse child (fun _ -> log := "child" :: !log);
  do_frame t;
  dispatch_mouse t (mouse_scroll ~x:1 ~y:1 `Down);
  equal ~msg:"stopping ends the walk after the stopping node's handlers"
    (list string)
    [ "child"; "parent, newer"; "parent" ]
    (List.rev !log)

let prevented_press_starts_no_selection () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  Renderable.on_mouse child Event.Mouse.prevent_default;
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_none ~msg:"a selection is a default a handler can prevent"
    (Renderer.selection t)

let click_on_selectable_node () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  let clicks = ref 0 in
  Renderable.set_on_click child (Some (fun _ -> incr clicks));
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  equal ~msg:"a press that starts a selection still clicks" int 1 !clicks

(* ── Mouse ── *)

(* Each handler on a path reads the event in its own node's cells, so a
   canvas compares the pointer with what it drew at its own origin. *)
let mouse_events_carry_node_cells () =
  let t = make_renderer () in
  let parent = make_child ~parent:(Renderer.root t) ~x:5 ~y:3 ~w:20 ~h:10 () in
  let child = make_child ~parent ~x:2 ~y:1 ~w:6 ~h:4 () in
  let seen = ref [] in
  let record name ev =
    seen := (name, Event.Mouse.local ev, Event.Mouse.size ev) :: !seen
  in
  Renderable.on_mouse child (fun ev ->
      match Event.Mouse.kind ev with
      | Event.Mouse.Down _ -> record "child" ev
      | _ -> ());
  Renderable.on_mouse parent (fun ev ->
      match Event.Mouse.kind ev with
      | Event.Mouse.Down _ ->
          record "parent" ev;
          equal ~msg:"screen x" int 8 (Event.Mouse.x ev);
          equal ~msg:"screen y" int 6 (Event.Mouse.y ev)
      | _ -> ());
  Renderable.set_on_click child (Some (fun ev -> record "click" ev));
  do_frame t;
  dispatch_mouse t (mouse_press ~x:8 ~y:6 ());
  dispatch_mouse t (mouse_release ~x:8 ~y:6 ());
  let cell = pair int int in
  equal ~msg:"node cells"
    (list (triple string cell cell))
    [
      ("child", (1, 2), (6, 4));
      ("parent", (3, 3), (20, 10));
      ("click", (1, 2), (6, 4));
    ]
    (List.rev !seen)

let click_dispatches_to_positioned_child () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  satisfies ~claim:"at least one event received" int
    (fun n -> n > 0)
    (List.length !log);
  let ev = List.hd (List.rev !log) in
  match Event.Mouse.kind ev with
  | Event.Mouse.Down { button = Left } -> ()
  | _ -> fail "expected Down Left"

let click_on_empty_area_does_not_crash () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Click outside all children — should not crash *)
  dispatch_mouse t (mouse_press ~x:0 ~y:0 ())

let overlapping_children_last_child_wins () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 () in
  let b = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:20 ~h:20 () in
  let log_a = record_mouse a in
  let log_b = record_mouse b in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:5 ~y:5 ());
  satisfies ~claim:"b received at least one event" int
    (fun n -> n > 0)
    (List.length !log_b);
  (* a should not receive the event directly — it may get it via bubbling from
     b's event dispatch, but the target hit should be b *)
  let _ = log_a in
  ()

let click_coordinates_are_absolute () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:8 ());
  satisfies ~claim:"at least one event received" int
    (fun n -> n > 0)
    (List.length !log);
  let ev = List.hd (List.rev !log) in
  equal ~msg:"x" int 7 (Event.Mouse.x ev);
  equal ~msg:"y" int 8 (Event.Mouse.y ev)

let press_and_release_sequence () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  equal ~msg:"two events" int 2 (List.length !log);
  let events = List.rev !log in
  (match Event.Mouse.kind (List.nth events 0) with
  | Event.Mouse.Down { button = Left } -> ()
  | _ -> fail "expected Down");
  match Event.Mouse.kind (List.nth events 1) with
  | Event.Mouse.Up { button = Left; _ } -> ()
  | _ -> fail "expected Up"

let move_fires_move_event () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let found_move =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Move -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Move" found_move

let mouse_event_has_correct_modifiers () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  let mods = { Matrix_input.Keystroke.no_mods with shift = true } in
  do_frame t;
  dispatch_mouse t (mouse_press ~modifiers:mods ~x:7 ~y:7 ());
  satisfies ~claim:"at least one event received" int
    (fun n -> n > 0)
    (List.length !log);
  let ev = List.hd !log in
  is_true ~msg:"shift" (Event.Mouse.modifiers ev).shift;
  log := [];
  dispatch_mouse t (mouse_motion ~modifiers:mods ~x:7 ~y:7 ());
  satisfies ~claim:"at least one motion event received" int
    (fun n -> n > 0)
    (List.length !log);
  let ev = List.hd !log in
  is_true ~msg:"motion shift" (Event.Mouse.modifiers ev).shift

let move_over_child_fires_over () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let found_over =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Over _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Over" found_over

let move_away_fires_out_then_over () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_a = record_mouse a in
  let log_b = record_mouse b in
  do_frame t;
  (* Move to a *)
  dispatch_mouse t (mouse_motion ~x:5 ~y:5 ());
  (* Move to b *)
  dispatch_mouse t (mouse_motion ~x:25 ~y:5 ());
  let a_got_out =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Out -> true | _ -> false)
      !log_a
  in
  let b_got_over =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Over _ -> true | _ -> false)
      !log_b
  in
  is_true ~msg:"a got Out" a_got_out;
  is_true ~msg:"b got Over" b_got_over

let move_within_same_child_no_over_out () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  let log = record_mouse child in
  dispatch_mouse t (mouse_motion ~x:8 ~y:8 ());
  let got_over_or_out =
    List.exists
      (fun ev ->
        match Event.Mouse.kind ev with Over _ | Out -> true | _ -> false)
      !log
  in
  is_false ~msg:"no Over/Out" got_over_or_out

let hover_returns_hovered_node () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  match Renderer.hover t with
  | Some n -> is_true ~msg:"hover is child" (n == child)
  | None -> fail "expected hover node"

let hover_none_when_pointer_leaves () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~x:7 ~y:7 ());
  is_some ~msg:"hovering" (Renderer.hover t);
  (* Move outside all children — root hover is technically not None since root
     covers everything, but hover tracks specific children. The hover node will
     be whatever the hit grid returns. Let's just move to a corner that the
     child doesn't cover. *)
  dispatch_mouse t (mouse_motion ~x:0 ~y:0 ());
  (* Hover should change from the child *)
  match Renderer.hover t with
  | Some n -> is_true ~msg:"not the child" (not (n == _child))
  | None -> ()

let scroll_dispatches_to_hit_target () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_scroll ~x:7 ~y:7 `Up);
  let found_scroll =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Scroll _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Scroll" found_scroll

let scroll_with_modifiers () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  let mods = { Matrix_input.Keystroke.no_mods with shift = true } in
  do_frame t;
  dispatch_mouse t (mouse_scroll ~modifiers:mods ~x:7 ~y:7 `Down);
  satisfies ~claim:"at least one event received" int
    (fun n -> n > 0)
    (List.length !log);
  let ev = List.hd !log in
  is_true ~msg:"shift" (Event.Mouse.modifiers ev).shift

let scroll_on_empty_area () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Should not crash *)
  dispatch_mouse t (mouse_scroll ~x:0 ~y:0 `Up)

(* ── Drag ── *)

let drag_sets_captured () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Drag = motion with left button pressed *)
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  match Renderer.captured t with
  | Some n -> is_true ~msg:"captured child" (n == child)
  | None -> fail "expected drag capture"

let captured_receives_all_mouse_events () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_a = record_mouse a in
  do_frame t;
  (* Start drag on a *)
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  is_some ~msg:"captured" (Renderer.captured t);
  let count_before = List.length !log_a in
  (* Move to b's area — a should still receive *)
  dispatch_mouse t (mouse_motion ~left:true ~x:25 ~y:5 ());
  satisfies
    ~claim:
      (Printf.sprintf "a received more than the %d events before" count_before)
    int
    (fun n -> n > count_before)
    (List.length !log_a);
  ignore b

let release_clears_capture () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  is_some ~msg:"captured" (Renderer.captured t);
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  is_none ~msg:"released" (Renderer.captured t)

let drag_end_fires_on_release () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let log = record_mouse child in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_release ~x:7 ~y:7 ());
  let found_drag_end =
    List.exists
      (fun ev ->
        match Event.Mouse.kind ev with Drag_end _ -> true | _ -> false)
      !log
  in
  is_true ~msg:"got Drag_end" found_drag_end

let drop_fires_on_target () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_b = record_mouse b in
  do_frame t;
  (* Start drag on a *)
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  is_some ~msg:"captured a" (Renderer.captured t);
  ignore a;
  (* Release over b *)
  dispatch_mouse t (mouse_release ~x:25 ~y:5 ());
  let found_drop =
    List.exists
      (fun ev -> match Event.Mouse.kind ev with Drop _ -> true | _ -> false)
      !log_b
  in
  is_true ~msg:"b got Drop" found_drop

let drop_source_identifies_origin () =
  let t = make_renderer () in
  let a = make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 () in
  let b = make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 () in
  let log_b = record_mouse b in
  do_frame t;
  dispatch_mouse t (mouse_motion ~left:true ~x:5 ~y:5 ());
  let a_num = Renderable.uid a in
  dispatch_mouse t (mouse_release ~x:25 ~y:5 ());
  let drop_source =
    List.find_map
      (fun ev ->
        match Event.Mouse.kind ev with
        | Drop { source; _ } -> source
        | _ -> None)
      !log_b
  in
  match drop_source with
  | Some src -> equal ~msg:"source" int a_num src
  | None -> fail "expected Drop with source"

let right_button_does_not_capture () =
  let t = make_renderer () in
  let _child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  do_frame t;
  (* Right button motion *)
  dispatch_mouse t (mouse ~x:7 ~y:7 (Motion (Some Right)));
  is_none ~msg:"no capture from right" (Renderer.captured t)

(* ── Selection ── *)

let selection_starts_none () =
  let t = make_renderer () in
  is_none ~msg:"no selection" (Renderer.selection t)

let click_on_selectable_starts_selection () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection started" (Renderer.selection t)

let drag_updates_selection_focus () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection active" (Renderer.selection t);
  dispatch_mouse t (mouse_motion ~left:true ~x:12 ~y:8 ());
  match Renderer.selection t with
  | Some sel ->
      let f = Selection.focus sel in
      equal ~msg:"focus x" int 12 f.x;
      equal ~msg:"focus y" int 8 f.y
  | None -> fail "expected active selection"

let release_finishes_selection () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:12 ~y:8 ());
  dispatch_mouse t (mouse_release ~x:12 ~y:8 ());
  match Renderer.selection t with
  | Some sel -> is_false ~msg:"not dragging" (Selection.is_dragging sel)
  | None -> fail "expected selection"

let unrelated_button_release_preserves_selection_drag () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  let decoder = Matrix_input.Decoder.create () in
  let feed sequence =
    let bytes = Bytes.of_string sequence in
    Matrix_input.Decoder.feed decoder bytes ~off:0 ~len:(Bytes.length bytes)
      (function
      | Mouse mouse -> dispatch_mouse t mouse
      | event ->
          failf "unexpected input: %a" Matrix_input.Decoder.pp_event event)
  in
  feed "\027[<0;8;8M\027[<32;10;8M\027[<2;10;8M\027[<2;10;8m";
  let sel =
    match Renderer.selection t with
    | Some sel -> sel
    | None -> fail "right-button click cleared the selection"
  in
  is_true ~msg:"right-button release leaves selection dragging"
    (Selection.is_dragging sel);
  feed "\027[<32;13;9M";
  equal ~msg:"left drag continues after right release" (pair int int) (12, 8)
    ((Selection.focus sel).x, (Selection.focus sel).y);
  feed "\027[<0;13;9m";
  is_false ~msg:"left-button release finishes selection"
    (Selection.is_dragging sel)

let clear_selection_resets () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_some ~msg:"selection exists" (Renderer.selection t);
  Renderer.clear_selection t;
  is_none ~msg:"cleared" (Renderer.selection t)

let scroll_on_dead_space_reaches_focused_scrollable () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 10.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.)
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style
      (Scroll_box.Props.make ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 8.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let _content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  do_frame t;
  ignore (Renderer.focus t (Scroll_box.node sb) : bool);
  dispatch_mouse t (mouse_scroll ~x:30 ~y:8 `Down);
  satisfies ~claim:"focused scroll box scrolled (scroll_top > 0)" int
    (fun top -> top > 0)
    (Scroll_box.scroll_top sb)

let selection_drag_auto_scrolls_scroll_box () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style
      (Scroll_box.Props.make ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  let changes = ref 0 in
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ ->
      incr changes;
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:8 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:7 ());
  do_frame ~width:20 ~height:8 ~dt:1. t;
  satisfies ~claim:"scroll box moved down (scroll_top > 0)" int
    (fun top -> top > 0)
    (Scroll_box.scroll_top sb);
  satisfies ~claim:"selection was refreshed (more than 2 changes)" int
    (fun n -> n > 2)
    !changes

let selection_drag_auto_scrolls_scroll_box_when_pointer_leaves_it () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style
      (Scroll_box.Props.make ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:12 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:10 ());
  do_frame ~width:20 ~height:12 ~dt:1. t;
  satisfies ~claim:"scroll box moved down (scroll_top > 0)" int
    (fun top -> top > 0)
    (Scroll_box.scroll_top sb)

let selection_release_after_leaving_scroll_box_goes_to_anchor_ancestors () =
  let t = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 20.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 8.)
  in
  let sb =
    Scroll_box.create ~parent:(Renderer.root t) ~style
      (Scroll_box.Props.make ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let content_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 18.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 30.)
  in
  let content =
    Renderable.create ~parent:(Scroll_box.node sb) ~style:content_style ()
  in
  let released = ref false in
  Renderable.on_mouse (Scroll_box.node sb) (fun event ->
      match Event.Mouse.kind event with
      | Up { button = Left; is_dragging = true } -> released := true
      | _ -> ());
  Renderable.set_selection content
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame ~width:20 ~height:12 t;
  dispatch_mouse t (mouse_press ~x:1 ~y:1 ());
  dispatch_mouse t (mouse_motion ~left:true ~x:1 ~y:10 ());
  dispatch_mouse t (mouse_release ~x:1 ~y:10 ());
  is_true ~msg:"release bubbled through scroll box" !released

let selectable_child_receives_changed () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let changed = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _sel ->
      changed := true;
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  is_true ~msg:"on_change called" !changed

let clear_notifies_selectables () =
  let t = make_renderer () in
  let child = make_child ~parent:(Renderer.root t) ~x:5 ~y:5 ~w:10 ~h:5 () in
  let clear_called = ref false in
  Renderable.set_selection child
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> clear_called := true)
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:7 ~y:7 ());
  Renderer.clear_selection t;
  is_true ~msg:"clear called" !clear_called

let click_on_non_selectable_clears_selection () =
  let t = make_renderer () in
  let selectable =
    make_child ~parent:(Renderer.root t) ~x:0 ~y:0 ~w:10 ~h:10 ()
  in
  let non_selectable =
    make_child ~parent:(Renderer.root t) ~x:20 ~y:0 ~w:10 ~h:10 ()
  in
  Renderable.set_selection selectable
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun _ -> true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  do_frame t;
  dispatch_mouse t (mouse_press ~x:5 ~y:5 ());
  is_some ~msg:"selection active" (Renderer.selection t);
  ignore non_selectable;
  (* Click on non-selectable area *)
  dispatch_mouse t (mouse_press ~x:25 ~y:5 ());
  is_none ~msg:"selection cleared" (Renderer.selection t)

(* ── Run ── *)

let geometry_changes_route_mouse_before_paint () =
  let renderer = make_renderer () in
  let node =
    make_child ~parent:(Renderer.root renderer) ~x:0 ~y:0 ~w:5 ~h:3 ()
  in
  let presses = ref 0 in
  Renderable.on_mouse node (fun event ->
      match Event.Mouse.kind event with Down _ -> incr presses | _ -> ());
  do_frame ~width:30 ~height:6 renderer;
  let inset = Toffee.Style.Length_percentage_auto.length in
  Renderable.set_style node
    (Renderable.style node
    |> Toffee.Style.set_inset
         {
           left = inset 10.;
           top = inset 0.;
           right = Toffee.Style.Length_percentage_auto.auto;
           bottom = Toffee.Style.Length_percentage_auto.auto;
         });
  Renderer.prepare_layout renderer ~width:30 ~height:6;
  dispatch_mouse renderer (mouse_press ~x:11 ~y:1 ());
  dispatch_mouse renderer (mouse_press ~x:1 ~y:1 ());
  equal ~msg:"fresh bounds are used without painting" int 1 !presses

(* Between frames a press hits the topmost node under it, inside every
   scissor that encloses that node, as the next frame's hit grid would. *)
let fresh_hits_respect_stacking_and_scissors () =
  let renderer = make_renderer () in
  let root = Renderer.root renderer in
  let back = make_child ~parent:root ~x:0 ~y:0 ~w:20 ~h:5 () in
  let clipper = make_child ~parent:root ~x:0 ~y:0 ~w:6 ~h:3 () in
  Renderable.set_style clipper
    (Renderable.style clipper
    |> Toffee.Style.set_overflow
         { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
    );
  let wide = make_child ~parent:clipper ~x:0 ~y:0 ~w:12 ~h:2 () in
  let above = make_child ~parent:root ~x:2 ~y:1 ~w:2 ~h:2 () in
  let presses node =
    let count = ref 0 in
    Renderable.on_mouse node (fun event ->
        match Event.Mouse.kind event with
        | Down _ ->
            incr count;
            Event.Mouse.stop_propagation event
        | _ -> ());
    count
  in
  let back_presses = presses back and clipper_presses = presses clipper in
  let wide_presses = presses wide and above_presses = presses above in
  do_frame ~width:20 ~height:5 renderer;
  Renderer.prepare_layout renderer ~width:20 ~height:5;
  let press x y = dispatch_mouse renderer (mouse_press ~x ~y ()) in
  press 1 0;
  press 8 0;
  press 3 1;
  press 1 2;
  press 25 0;
  equal ~msg:"inside its scissor" int 1 !wide_presses;
  equal ~msg:"a later sibling on top" int 1 !above_presses;
  equal ~msg:"below the clipped child" int 1 !clipper_presses;
  equal ~msg:"clipped away, and nothing outside the frame" int 1 !back_presses

(* The screen owns the width method and can change it while the tree lives;
   every measurement made by the old method is made again. *)
let width_method_change_remeasures () =
  let renderer = Renderer.create ~width_method:`Unicode () in
  let root = Renderer.root renderer in
  (* The root fills the screen's width; its children keep their own *)
  Renderable.set_style root
    (Toffee.Style.default
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Start));
  let emoji = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" in
  let text = Text.create ~parent:root (Text.Props.make ~content:emoji ()) in
  let spinner =
    Spinner.create ~parent:root
      (Spinner.Props.make
         ~frame_set:{ Spinner.frames = [| emoji |]; period = 0. }
         ())
  in
  let widths () =
    (Renderable.width (Text.node text), Renderable.width (Spinner.node spinner))
  in
  do_frame ~width:10 ~height:3 renderer;
  equal ~msg:"one cluster" (pair int int) (2, 2) (widths ());
  Matrix_screen.set_width_method (Renderer.screen renderer) `Wcwidth;
  do_frame ~width:10 ~height:3 renderer;
  equal ~msg:"each scalar's width" (pair int int) (4, 4) (widths ())

let layout_updates_settle_before_first_paint () =
  let renderer = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root renderer) ~x:0 ~y:0 ~w:1 ~h:1 ()
  in
  let resized = ref false and frames = ref 0 and painted_widths = ref [] in
  Renderable.set_live child true;
  Renderable.set_on_frame child (Some (fun _ ~dt:_ -> incr frames));
  Renderable.set_on_update child
    (Some
       (fun node ~dt:_ ->
         if not !resized then begin
           resized := true;
           Renderable.set_style node
             (Renderable.style node
             |> Toffee.Style.set_width (Toffee.Style.Dimension.length 5.))
         end));
  Renderable.set_render child (fun node grid ~dt:_ ->
      painted_widths := Renderable.width node :: !painted_widths;
      Matrix_grid.draw_text grid ~x:0 ~y:0 ~text:"ready");
  Renderer.render_frame renderer ~width:8 ~height:3 ~dt:16.;
  equal ~msg:"first paint uses settled geometry" (list int) [ 5 ]
    !painted_widths;
  equal ~msg:"layout retries do not advance live animation twice" int 1 !frames;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  equal ~msg:"first frame contains final content" string "ready"
    (String.trim (Matrix_grid.to_text grid))

let layout_preparation_does_not_advance_live_widgets () =
  let renderer = make_renderer () in
  let child =
    make_child ~parent:(Renderer.root renderer) ~x:0 ~y:0 ~w:1 ~h:1 ()
  in
  let deltas = ref [] in
  Renderable.set_live child true;
  Renderable.set_on_frame child (Some (fun _ ~dt -> deltas := dt :: !deltas));
  Renderer.prepare_layout renderer ~width:8 ~height:3;
  Renderer.prepare_layout renderer ~width:10 ~height:4;
  equal ~msg:"layout-only synchronization does not tick animation"
    (list float_exact) [] !deltas;
  Renderer.render_frame renderer ~width:10 ~height:4 ~dt:16.;
  equal ~msg:"one actual frame delivers one animation delta" (list float_exact)
    [ 16. ] !deltas

let scrollbar_geometry_settles_before_next_input () =
  let renderer = make_renderer () in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 8.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 3.)
  in
  let box =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style
      (Scroll_box.Props.make ())
  in
  let row_style =
    Toffee.Style.default
    |> Toffee.Style.set_width (Toffee.Style.Dimension.length 7.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)
    |> Toffee.Style.set_flex_shrink 0.
  in
  let add_row i =
    ignore
      (Text.create ~parent:(Scroll_box.node box) ~style:row_style
         (Text.Props.make
            ~content:(Printf.sprintf "row %d" i)
            ~selectable:false ())
        : Text.t)
  in
  add_row 0;
  do_frame ~width:8 ~height:3 renderer;
  equal ~msg:"initial content fits without a scrollbar" int 8
    (Scroll_box.viewport_width box);
  for i = 1 to 5 do
    add_row i
  done;
  let bar_moves = ref 0 in
  Renderable.on_mouse
    (Scroll_bar.node (Scroll_box.vertical_bar box))
    (fun event ->
      match Event.Mouse.kind event with Move -> incr bar_moves | _ -> ());
  Renderer.prepare_layout renderer ~width:8 ~height:3;
  equal ~msg:"scrollbar reveal is resolved before input" int 7
    (Scroll_box.viewport_width box);
  dispatch_mouse renderer (mouse_motion ~x:7 ~y:1 ());
  equal ~msg:"next mouse event reaches the newly allocated scrollbar" int 1
    !bar_moves

let paste_behavior_runs_on_the_focus_current_then () =
  let renderer = make_renderer () in
  let parent = Renderer.root renderer in
  let first = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:1 ~focusable:true () in
  let second = make_child ~parent ~x:6 ~y:0 ~w:5 ~h:1 ~focusable:true () in
  let first_edits = ref 0 and second_edits = ref 0 in
  Renderable.set_paste_behavior first (Some (fun _ -> incr first_edits));
  Renderable.set_paste_behavior second (Some (fun _ -> incr second_edits));
  Renderable.on_paste first (fun _ ->
      ignore (Renderer.focus renderer second : bool));
  ignore (Renderer.focus renderer first : bool);
  dispatch_paste renderer "first";
  equal ~msg:"the editor focus left does not insert" int 0 !first_edits;
  equal ~msg:"the editor focused when the insertion runs inserts" int 1
    !second_edits;
  dispatch_paste renderer "second";
  equal ~msg:"next event uses accepted focus" int 2 !second_edits

let accepted_focus_does_not_retarget_press () =
  let renderer = make_renderer () in
  let parent = Renderer.root renderer in
  let underneath = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:2 ~focusable:true () in
  let target = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:2 ~focusable:true () in
  let received = record_mouse underneath in
  Renderable.set_on_focus_request target
    (Some
       (fun () ->
         ignore (Renderer.focus renderer target : bool);
         let inset = Toffee.Style.Length_percentage_auto.length in
         Renderable.set_style target
           (Renderable.style target
           |> Toffee.Style.set_inset
                {
                  left = inset 10.;
                  top = inset 0.;
                  right = Toffee.Style.Length_percentage_auto.auto;
                  bottom = Toffee.Style.Length_percentage_auto.auto;
                });
         Renderer.prepare_layout renderer ~width:20 ~height:4));
  do_frame ~width:20 ~height:4 renderer;
  List.iter
    (fun mouse ->
      let event = Test_harness.dispatch_mouse renderer mouse in
      is_true ~msg:"retargeted gesture suppressed"
        (Event.Mouse.default_prevented event))
    [
      mouse_press ~x:1 ~y:1 ();
      mouse_motion ~left:true ~x:1 ~y:1 ();
      mouse_release ~x:1 ~y:1 ();
    ];
  equal ~msg:"underlying target receives no part of rejected gesture" int 0
    (List.length !received)

let selection_cleanup_revalidates_focus_target () =
  List.iter
    (fun change ->
      let renderer = make_renderer () in
      let parent = Renderer.root renderer in
      let previous =
        make_child ~parent ~x:0 ~y:0 ~w:5 ~h:2 ~focusable:true ()
      in
      let target = make_child ~parent ~x:10 ~y:0 ~w:5 ~h:2 ~focusable:true () in
      let armed = ref false in
      Renderable.set_selection previous
        ~should_start:(fun ~x:_ ~y:_ -> true)
        ~on_change:(fun selection ->
          if !armed && Option.is_none selection then begin
            armed := false;
            (match change with
            | `Hide -> Renderable.set_visible target false
            | `Destroy -> Renderable.destroy_recursively target
            | `Refocus | `Refocus_existing ->
                ignore (Renderer.focus renderer previous : bool));
            Renderer.prepare_layout renderer ~width:20 ~height:4
          end;
          true)
        ~clear:(fun () -> ())
        ~get_text:(fun () -> "");
      Renderable.set_selection target
        ~should_start:(fun ~x:_ ~y:_ -> true)
        ~on_change:(fun _ -> true)
        ~clear:(fun () -> ())
        ~get_text:(fun () -> "");
      Renderable.set_on_focus_request target
        (Some (fun () -> ignore (Renderer.focus renderer target : bool)));
      do_frame ~width:20 ~height:4 renderer;
      dispatch_mouse renderer (mouse_press ~x:1 ~y:1 ());
      if change = `Refocus_existing then
        ignore (Renderer.focus renderer target : bool);
      armed := true;
      let received = record_mouse target in
      let event =
        Test_harness.dispatch_mouse renderer (mouse_press ~x:11 ~y:1 ())
      in
      is_true ~msg:"cleanup invalidates the press"
        (Event.Mouse.default_prevented event);
      is_none ~msg:"no selection starts on the invalidated target"
        (Renderer.selection renderer);
      (* The handlers see the press before its default starts the selection,
         which the cleanup then invalidates. *)
      equal ~msg:"invalidated target's handlers saw only the press" int 1
        (List.length !received))
    [ `Hide; `Destroy; `Refocus; `Refocus_existing ]

let selection_notification_revalidates_focus_target () =
  let renderer = make_renderer () in
  let parent = Renderer.root renderer in
  let modal = make_child ~parent ~x:10 ~y:0 ~w:5 ~h:2 ~focusable:true () in
  let target = make_child ~parent ~x:0 ~y:0 ~w:5 ~h:2 ~focusable:true () in
  Renderable.set_selection target
    ~should_start:(fun ~x:_ ~y:_ -> true)
    ~on_change:(fun selection ->
      if Option.is_some selection then
        ignore (Renderer.focus renderer modal : bool);
      true)
    ~clear:(fun () -> ())
    ~get_text:(fun () -> "");
  Renderable.set_on_focus_request target
    (Some (fun () -> ignore (Renderer.focus renderer target : bool)));
  let received = record_mouse target in
  do_frame ~width:20 ~height:4 renderer;
  List.iter
    (fun mouse ->
      let event = Test_harness.dispatch_mouse renderer mouse in
      is_true ~msg:"selection callback invalidates the gesture"
        (Event.Mouse.default_prevented event))
    [
      mouse_press ~x:1 ~y:1 ();
      mouse_motion ~left:true ~x:2 ~y:1 ();
      mouse_release ~x:2 ~y:1 ();
    ];
  is_none ~msg:"invalidated selection is cleared" (Renderer.selection renderer);
  (* The handlers see the press before its default starts the selection; the
     invalidated press takes its drag and release with it. *)
  equal ~msg:"target receives only the press" int 1 (List.length !received)

let () =
  run "mosaic.renderer"
    [
      test "accepted focus does not retarget press"
        accepted_focus_does_not_retarget_press;
      test "selection cleanup revalidates focus target"
        selection_cleanup_revalidates_focus_target;
      test "selection notification revalidates focus target"
        selection_notification_revalidates_focus_target;
      test "mouse uses changed geometry before paint"
        geometry_changes_route_mouse_before_paint;
      test "fresh hits respect stacking and scissors"
        fresh_hits_respect_stacking_and_scissors;
      test "a width method change re-measures" width_method_change_remeasures;
      test "layout updates settle before first paint"
        layout_updates_settle_before_first_paint;
      test "layout preparation never advances live widgets"
        layout_preparation_does_not_advance_live_widgets;
      test "scrollbar geometry settles before next input"
        scrollbar_geometry_settles_before_next_input;
      test "paste behavior runs on the focus current then"
        paste_behavior_runs_on_the_focus_current_then;
      group "Lifecycle"
        [
          test "creates with default style" creates_with_default_style;
          test "creates with custom style" creates_with_custom_style;
          test "width method reaches widgets" width_method_reaches_widgets;
          test "adopted screen render is guarded"
            adopted_screen_render_is_guarded;
          test "zero-sized clipping node hides children"
            zero_sized_clipping_node_hides_children;
          test "starts dirty" starts_dirty;
          test "clean after render" clean_after_render;
          test "dirty after schedule" dirty_after_schedule;
          test "render request during frame survives"
            render_request_during_frame_survives;
          test "callback runs each frame" callback_runs_each_frame;
          test "a frame without animation" frame_without_animation;
          test "callback receives delta" callback_receives_delta;
          test "remove callback stops it" remove_callback_stops_it;
          test "clear removes all callbacks" clear_removes_all_callbacks;
          test "a lifecycle pass runs each frame" lifecycle_pass_runs_each_frame;
          test "the tree's liveness" tree_liveness;
          test "child_at and uid" child_at_and_uid;
        ];
      group "Viewport culling"
        [
          test "overflow-visible descendant reaching viewport renders"
            overflow_visible_descendant_reaching_viewport_renders;
          test "clipped offscreen subtree is not traversed for rendering"
            clipped_offscreen_subtree_is_not_traversed_for_rendering;
          test "scroll box culls offscreen children"
            scroll_box_culls_offscreen_children;
          test "scrolling paints revealed children"
            scrolling_paints_revealed_children;
          test "culled child is not hit-testable"
            culled_child_is_not_hit_testable;
          test "culled live child does not tick" culled_live_child_does_not_tick;
          test "child straddling the viewport edge is kept"
            child_straddling_the_viewport_edge_is_kept;
          test "child ending on the viewport edge is culled"
            child_ending_on_the_viewport_edge_is_culled;
          test "zero-height child on the viewport edge is kept"
            zero_height_child_on_the_viewport_edge_is_kept;
          test "zero-width child on the viewport edge is kept"
            zero_width_child_on_the_viewport_edge_is_kept;
          test "zero-width wrapper keeps its overflowing row"
            zero_width_wrapper_keeps_its_overflowing_row;
          test "descendant reaching the viewport survives its offscreen parent"
            descendant_reaching_the_viewport_survives_its_offscreen_parent;
          test "child far before the viewport is kept"
            child_far_before_the_viewport_is_kept;
          test "a selection reads rows out of view where they are"
            selection_reads_rows_out_of_view;
          test "a selection reads the top layer's positions"
            selection_reads_the_top_layer;
          culling_matches_a_scan;
          chunks_keep_every_box;
          test "a chunk follows a grown member" chunk_follows_a_grown_member;
          test "a member that stops being contained leaves its chunk"
            member_leaves_its_chunk;
        ];
      group "Scroll hints"
        [
          test "scroll produces hint" scroll_produces_hint;
          test "opposing scrolls cancel" opposing_scrolls_cancel;
          test "two scrollers disqualify" two_scrollers_disqualify;
          test "horizontal scroll disqualifies" horizontal_scroll_disqualifies;
          test "narrow box disqualifies" narrow_box_disqualifies;
          test "hinted scroll emits DECSTBM and correct rows"
            hinted_scroll_emits_decstbm_and_correct_rows;
        ];
      group "Layout dirtiness"
        [
          test "style change relayouts after clean frame"
            style_change_relayouts_after_clean_frame;
          test "measure change relayouts after clean frame"
            measure_change_relayouts_after_clean_frame;
          test "add/remove child relayouts after clean frame"
            add_remove_child_relayouts_after_clean_frame;
          test "resize relayouts after clean frame"
            resize_relayouts_after_clean_frame;
          test "visibility toggle relayouts after clean frame"
            visibility_toggle_relayouts_after_clean_frame;
        ];
      group "Settlement"
        [
          test "pending provider reports work" pending_provider_reports_work;
          test "render_frame_until_settled reports pending code"
            render_frame_until_settled_reports_pending_code;
          test "render_frame_until_settled completes code"
            render_frame_until_settled_completes_code;
          test "settlement replaces superseded frame cells"
            settlement_replaces_superseded_frame_cells;
          test "presented cells cleared before next frame"
            presented_cells_cleared_before_next_frame;
        ];
      group "Focus"
        [
          test "focus returns true for focusable"
            focus_returns_true_for_focusable;
          test "focus returns false for non-focusable"
            focus_returns_false_for_non_focusable;
          test "focused returns focused node" focused_returns_focused_node;
          test "blur clears focus" blur_clears_focus;
          test "focus different node blurs previous"
            focus_different_node_blurs_previous;
          test "left click requests focus" left_click_requests_focus;
          test "left click without request does not focus"
            left_click_without_request_does_not_focus;
          test "left click on non-focusable does not focus"
            left_click_on_non_focusable_does_not_focus;
          test "click walks up to focusable parent"
            click_walks_up_to_focusable_parent;
          test "left click focuses selectable node"
            left_click_focuses_selectable_node;
          test "right click does not auto-focus" right_click_does_not_auto_focus;
          test "shift left click preserves existing focus"
            shift_left_click_preserves_existing_focus;
          test "prevent default mouse down blocks auto-focus"
            prevent_default_mouse_down_blocks_auto_focus;
          test "focus fires on node" focus_fires_on_node;
          test "blur fires on previous" blur_fires_on_previous;
          test "refocus fires blur then focus" refocus_fires_blur_then_focus;
        ];
      group "Input"
        [
          test "key dispatches to focused" key_dispatches_to_focused;
          test "key does nothing when no focus" key_does_nothing_when_no_focus;
          test "key multiple handlers run" key_multiple_handlers_run;
          test "prevent default stops default handler"
            prevent_default_stops_default_handler;
          test "paste dispatches to focused" paste_dispatches_to_focused;
          test "paste does nothing when no focus"
            paste_does_nothing_when_no_focus;
          test "paste contains correct text" paste_contains_correct_text;
          test "an unused key bubbles to the ancestors"
            unused_key_bubbles_to_ancestors;
          test "a behavior that acts consumes the key" acting_behavior_consumes;
          test "stopping a key keeps its behavior" stop_keeps_behavior;
          test "the path is fixed and the behavior follows focus"
            path_is_fixed_behavior_follows_focus;
          test "mouse handlers bubble until stopped" mouse_bubbles_until_stopped;
          test "a prevented press starts no selection"
            prevented_press_starts_no_selection;
          test "a click on a selectable node" click_on_selectable_node;
        ];
      group "Mouse"
        [
          test "mouse events carry node cells" mouse_events_carry_node_cells;
          test "click dispatches to positioned child"
            click_dispatches_to_positioned_child;
          test "click on empty area does not crash"
            click_on_empty_area_does_not_crash;
          test "overlapping children last child wins"
            overlapping_children_last_child_wins;
          test "click coordinates are absolute" click_coordinates_are_absolute;
          test "press and release sequence" press_and_release_sequence;
          test "move fires Move event" move_fires_move_event;
          test "mouse event has correct modifiers"
            mouse_event_has_correct_modifiers;
          test "move over child fires Over" move_over_child_fires_over;
          test "move away fires Out then Over" move_away_fires_out_then_over;
          test "move within same child no Over/Out"
            move_within_same_child_no_over_out;
          test "hover returns hovered node" hover_returns_hovered_node;
          test "hover None when pointer leaves" hover_none_when_pointer_leaves;
          test "scroll dispatches to hit target" scroll_dispatches_to_hit_target;
          test "scroll with modifiers" scroll_with_modifiers;
          test "scroll on empty area" scroll_on_empty_area;
          test "scroll on dead space reaches focused scrollable"
            scroll_on_dead_space_reaches_focused_scrollable;
        ];
      group "Drag"
        [
          test "drag sets captured" drag_sets_captured;
          test "captured receives all mouse events"
            captured_receives_all_mouse_events;
          test "release clears capture" release_clears_capture;
          test "drag-end fires on release" drag_end_fires_on_release;
          test "drop fires on target" drop_fires_on_target;
          test "drop source identifies origin" drop_source_identifies_origin;
          test "right button does not capture" right_button_does_not_capture;
        ];
      group "Selection"
        [
          test "selection starts None" selection_starts_none;
          test "click on selectable starts selection"
            click_on_selectable_starts_selection;
          test "drag updates selection focus" drag_updates_selection_focus;
          test "release finishes selection" release_finishes_selection;
          test "unrelated button release preserves selection drag"
            unrelated_button_release_preserves_selection_drag;
          test "clear selection resets" clear_selection_resets;
          test "selection drag auto-scrolls scroll box"
            selection_drag_auto_scrolls_scroll_box;
          test "selection drag auto-scrolls scroll box after leaving it"
            selection_drag_auto_scrolls_scroll_box_when_pointer_leaves_it;
          test
            "selection release after leaving scroll box goes to anchor \
             ancestors"
            selection_release_after_leaving_scroll_box_goes_to_anchor_ancestors;
          test "selectable child receives changed"
            selectable_child_receives_changed;
          test "clear notifies selectables" clear_notifies_selectables;
          test "click on non-selectable clears selection"
            click_on_non_selectable_clears_selection;
        ];
    ]
