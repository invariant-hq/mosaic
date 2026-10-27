(* What repeated work costs, against what the view hides. A check counts the
   words one call allocates at two sizes, in one process: equal counts show
   that the work follows what is visible, on any machine, compiler and
   profile, with no baseline. Where words cannot tell linear work from
   quadratic, a check compares the time of one call at two sizes. The memory
   checks are fixed caps. *)

open Windtrap
open Mosaic_ui

(* The words [f ()] allocates, after a first call that warms its caches. *)
let words f =
  f ();
  let minor, promoted, major = Gc.counters () in
  f ();
  let minor', promoted', major' = Gc.counters () in
  Float.to_int (minor' -. minor +. (major' -. major) -. (promoted' -. promoted))

let same what ~small ~large =
  if small <> large then
    failf "%s: %d words at the small size, %d at the large one" what small large

(* ───── Repainting a text surface ─────

   A 80x20 viewport over text whose rows, columns or overlays mostly lie
   outside it. Setup builds and draws a frame once; the measured call draws
   the frame again with the unchanged view asked to repaint, through its
   renderer. Paint allocates per visited row and per entered style run, and
   the rest of the frame the same at either size, so a hidden row, column or
   overlay costs nothing if paint never visits it. *)

let make_surface spans =
  let renderer = Renderer.create () in
  Test_harness.register renderer;
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  Text_buffer.set_styled_text buffer spans;
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  (renderer, surface)

let span text = [ { Text_buffer.text; style = Ansi.Style.default } ]
let tall lines = String.concat "" (List.init lines (fun _ -> "one line\n"))
let megabyte = String.make 1_000_000 'x'

let styled =
  List.init 10_000 (fun i ->
      {
        Text_buffer.text = "word text ";
        style =
          (if i mod 2 = 0 then Ansi.Style.default
           else Ansi.Style.make ~bold:true ());
      })

let viewport ?(wrap = `None) ?(rows = 20) ?(scroll = `Top) ?(scroll_x = 0)
    ?(setup = ignore) spans =
  let renderer, surface = make_surface spans in
  Text_surface.set_wrap surface wrap;
  setup (Text_surface.buffer surface);
  let node = Text_surface.node surface in
  Test_harness.layout_node node ~x:0 ~y:0 ~width:80 ~height:rows;
  (match scroll with
  | `Top -> ()
  | `Middle ->
      Text_surface.set_scroll_y surface (Text_surface.max_scroll_y surface / 2));
  Text_surface.set_scroll_x surface scroll_x;
  Renderer.render_frame renderer ~width:80 ~height:rows ~dt:0.;
  (renderer, node, rows)

(* [count] highlights on the first [count] lines of [tall], each with its own
   color, so that none reaches the middle of the text. *)
let hidden_overlays count buffer =
  for i = 0 to count - 1 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:(i * 9)
         ~end_offset:((i * 9) + 4)
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.indexed (i mod 256)) ())
         ~ref_id:i ())
  done

(* [count] nested highlights over the visible text, alternating colors, so
   that every column starts a style run under up to [count] overlays. *)
let dense_overlays count buffer =
  let n = 20 * 81 in
  for i = 0 to count - 1 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:(i * 4)
         ~end_offset:(n - (i * 4))
         ~style:(Ansi.Style.make ~fg:(Ansi.Color.indexed (i mod 256)) ())
         ~priority:i ~ref_id:i ())
  done

let visible_lines =
  String.concat "\n" (List.init 20 (fun _ -> String.make 80 'x'))

let repaint (renderer, node, rows) =
  words (fun () ->
      Renderable.request_render node;
      Renderer.render_frame renderer ~width:80 ~height:rows ~dt:0.)

let hidden_rows () =
  same "repaint grows with hidden rows"
    ~small:(repaint (viewport ~scroll:`Middle (span (tall 1_000))))
    ~large:(repaint (viewport ~scroll:`Middle (span (tall 100_000))))

let hidden_columns () =
  same "repaint grows with hidden columns"
    ~small:
      (repaint
         (viewport ~rows:1 ~scroll_x:99_920 (span (String.make 100_000 'x'))))
    ~large:(repaint (viewport ~rows:1 ~scroll_x:999_920 (span megabyte)))

let hidden_overlay_count () =
  same "repaint grows with hidden overlays"
    ~small:(repaint (viewport ~scroll:`Middle (span (tall 100_000))))
    ~large:
      (repaint
         (viewport ~scroll:`Middle ~setup:(hidden_overlays 10_000)
            (span (tall 100_000))))

let hidden_wrapped_rows () =
  same "repaint grows with hidden wrapped rows"
    ~small:
      (repaint
         (viewport ~wrap:`Word ~scroll:`Middle
            (List.filteri (fun i _ -> i < 1_000) styled)))
    ~large:(repaint (viewport ~wrap:`Word ~scroll:`Middle styled))

(* The converse: what paint does visit, it pays for. *)
let visible_work () =
  let ten = repaint (viewport ~rows:10 (span visible_lines)) in
  let twenty = repaint (viewport (span visible_lines)) in
  let dense =
    repaint (viewport ~setup:(dense_overlays 200) (span visible_lines))
  in
  is_true
    ~msg:
      (Printf.sprintf "visible rows cost: 10 rows %d words, 20 rows %d" ten
         twenty)
    (twenty > ten);
  is_true
    ~msg:
      (Printf.sprintf "visible style runs cost: plain %d words, 200 overlays %d"
         twenty dense)
    (dense > twenty)

(* ───── Frames of a scrolled transcript ───── *)

let render_frame renderer =
  Renderer.render_frame renderer ~width:80 ~height:24 ~dt:16.;
  ignore (Renderer.render renderer : string)

(* [rows] laid-out rows behind a 24-row viewport. *)
let transcript rows =
  let renderer = Renderer.create () in
  let px n = Toffee.Style.Dimension.length (Float.of_int n) in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer)
      ~style:Toffee.Style.(default |> set_width (px 80) |> set_height (px 24))
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  for _ = 1 to rows do
    ignore
      (Text.create ~parent:(Scroll_box.node scroll)
         ~style:Toffee.Style.(default |> set_width (px 80) |> set_height (px 1))
         (Text.Props.make ~content:"A representative transcript row."
            ~selectable:false ())
        : Text.t)
  done;
  render_frame renderer;
  renderer

let hidden_transcript_rows () =
  let frame rows =
    let renderer = transcript rows in
    words (fun () -> render_frame renderer)
  in
  same "a steady frame grows with the rows the viewport hides"
    ~small:(frame 1_000) ~large:(frame 10_000)

(* ───── A key on a collection ─────

   A cursor finds its item at the position where it last saw it, so a key
   reads the items it moves over whatever their number and the cursor's
   position. The check counts both the words and the item comparisons of
   Down then Up, from the middle and from next to the last item. *)

let cursor_keys () =
  let compared = ref 0 in
  let equal a b =
    incr compared;
    String.equal a b
  in
  let step items c a = Cursor.step ~equal ~name:Fun.id ~page:20 items c a in
  let cost n position =
    let items =
      Items.of_array (Array.init n (fun i -> Printf.sprintf "item-%d" i))
    in
    let c =
      match step items (Cursor.at "item-0") (By position) with
      | Moved c -> c
      | _ -> fail "a move"
    in
    let key () =
      match step items c Next with
      | Moved c -> ignore (Sys.opaque_identity (step items c Prev))
      | _ -> fail "a move"
    in
    let words = words key in
    compared := 0;
    key ();
    (words, !compared)
  in
  let words, compared = cost 100 50 in
  List.iter
    (fun (n, position) ->
      let words', compared' = cost n position in
      let what = Printf.sprintf "item %d of %d" position n in
      same (what ^ ", words") ~small:words ~large:words';
      same (what ^ ", comparisons") ~small:compared ~large:compared')
    [ (100, 98); (100_000, 50_000); (100_000, 99_998) ]

(* ───── An element that declares nothing ─────

   An element that declares no layout label, attribute or handler shares
   one layout, one record of attributes and one record of handlers with
   every such element, so declaring it allocates only its own props and the
   element around them: the element's record (7 words), its constructor (2),
   its kind's (2) and the option that gives the props its content (2). A
   layout record alone is 39 words. *)

let default_text () =
  let alone f = words f - words ignore in
  let text = alone (fun () -> ignore (Mosaic.text "x" : unit Mosaic.t))
  and props =
    alone (fun () -> ignore (Text.Props.make ~content:"x" () : Text.Props.t))
  in
  if text - props > 13 then
    failf
      "a text that declares nothing allocates %d words besides its props, over \
       13"
      (text - props)

(* ───── Reconciling an unchanged list ─────

   A view builds its elements anew on every render. When they equal the
   ones mounted, the reconciler keeps each fiber's records and its parent's
   child list, and matches unkeyed elements with no table, so reconciling
   allocates nothing per element: a fresh but equal list of 1,000 and of
   16,000 texts reconciles in the same words, its own words excepted. *)

let reconcile_equal_list n =
  let view () =
    Mosaic.box (List.init n (fun i -> Mosaic.text (string_of_int i)))
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let render view =
    Mosaic.Reconciler.render reconciler ~viewport_width:80 view
  in
  render (view ());
  let views = Array.init 3 (fun _ -> view ()) and next = ref 0 in
  words (fun () ->
      render views.(!next);
      incr next)

let unchanged_list () =
  same "reconciling a fresh but equal list of texts"
    ~small:(reconcile_equal_list 1_000)
    ~large:(reconcile_equal_list 16_000)

(* ───── Maps ─────

   A map is a node: a render stores its function at its place, and a
   handler's message goes through the functions of the maps above it when
   it fires. A render costs each map a few words of its own and nothing per
   element under it: reconciling a fresh but equal subtree under four maps
   allocates the same words for 1,000 texts and for 16,000. *)

let under_maps depth n =
  let rec wrap depth view =
    if depth = 0 then view else wrap (depth - 1) (Mosaic.map Fun.id view)
  in
  let view () =
    wrap depth
      (Mosaic.box (List.init n (fun i -> Mosaic.text (string_of_int i))))
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let render view =
    Mosaic.Reconciler.render reconciler ~viewport_width:80 view
  in
  render (view ());
  let views = Array.init 3 (fun _ -> view ()) and next = ref 0 in
  words (fun () ->
      render views.(!next);
      incr next)

let nested_maps () =
  same "reconciling texts under four maps" ~small:(under_maps 4 1_000)
    ~large:(under_maps 4 16_000)

(* ───── A keyed log ─────

   [keyed] compares the map a view declares with the one it declared last,
   skipping the subtrees they share, so a render costs what changed in the
   map. Appending a line, appending one and dropping the oldest, and editing
   the newest allocate within a factor 2 at 5,000 and at 50,000 lines: the
   logarithm of the size grows, the work per line does not. *)

let log_line _ text = Mosaic.text text

(* The words of the render that declares the second change of [lines]. *)
let keyed_log n change =
  let lines =
    Mosaic.Keyed.of_list ~compare:Int.compare
      (List.init n (fun i -> (i, Printf.sprintf "line %d" i)))
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let render lines =
    Mosaic.Reconciler.render reconciler ~viewport_width:80
      (Mosaic.box [ Mosaic.keyed lines log_line ])
  in
  render lines;
  let next = ref 0 in
  let states = Array.make 3 lines in
  for i = 1 to 2 do
    states.(i) <- change (n + i - 1) states.(i - 1)
  done;
  words (fun () ->
      incr next;
      render states.(!next))

let append n lines = Mosaic.Keyed.add n "new line" lines
let capped n lines = Mosaic.Keyed.remove 0 (Mosaic.Keyed.add n "new line" lines)

let streamed next lines =
  match Mosaic.Keyed.max_binding_opt lines with
  | Some (newest, text) -> Mosaic.Keyed.add newest (text ^ ".") lines
  | None ->
      ignore next;
      lines

let keyed_log_changes () =
  List.iter
    (fun (what, change) ->
      let small = keyed_log 5_000 change and large = keyed_log 50_000 change in
      if large > 2 * small then
        failf "%s: %d words at 5,000 lines, %d at 50,000, over twice" what small
          large)
    [
      ("appending a line", append);
      ("appending a line and dropping the oldest", capped);
      ("editing the newest line", streamed);
    ]

(* ───── A capped log's frame ─────

   A keyed log in a scroll box that follows its bottom, over a 120x40
   screen, appends a line and drops the oldest, and the measured call
   reconciles, lays out, culls and paints the frame. The column holds its
   lines in chunks of 256: the chunks after the first move up a row, and the
   layout tree keeps their members' rounded layouts, the column's regroup
   passes over them whole and the cull index keeps their indexes, so the
   frame's words follow what changed, the chunks and the lines shown. Only
   the column's own layout, one item per chunk, grows with the log: 50,000
   lines allocate under 1.5 times the words of 5,000, where each line cost
   about 90 words and the ratio was 3.6. *)

let capped_log_frame n =
  let line i = Printf.sprintf "L%06d a line of the log" i in
  let lines =
    Mosaic.Keyed.of_list ~compare:Int.compare
      (List.init n (fun i -> (i, line i)))
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let render lines =
    Mosaic.Reconciler.render reconciler ~viewport_width:120
      (Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
         ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
         [
           Mosaic.scroll_box ~flex_grow:1. ~scroll_x:false
             ~min_size:(Mosaic.size ~width:0 ~height:0)
             ~scroll:(Mosaic.Scroll.follow `Bottom (Mosaic.Scroll.at 0))
             ~on_scroll:(fun _ -> None)
             [ Mosaic.keyed lines log_line ];
         ]);
    Renderer.render_frame renderer ~width:120 ~height:40 ~dt:0.;
    ignore (Renderer.render renderer : string)
  in
  render lines;
  let lines = ref lines and next = ref n in
  words (fun () ->
      lines :=
        Mosaic.Keyed.remove (!next - n)
          (Mosaic.Keyed.add !next (line !next) !lines);
      incr next;
      render !lines)

let capped_log_frames () =
  let small = capped_log_frame 5_000 and large = capped_log_frame 50_000 in
  if 2 * large > 3 * small then
    failf
      "a capped log's frame: %d words at 5,000 lines, %d at 50,000, over 1.5 \
       times"
      small large

(* ───── A shared layout ─────

   An element given [~layout] and no layout label uses that record itself,
   so a layout built once and shared by many elements costs no words per
   element: a text so declared allocates one layout record fewer than a text
   whose label overrides one field of the same layout, which costs one copy
   of the record, as the field's setter does. The node it mounts holds the
   record itself. The reconciler compares a layout physically before it
   compares it by value. A layout holding a NaN is unequal to itself by
   value, so only the physical comparison finds it unchanged when the text
   is declared again, and the text is not laid out again. *)

let shared_layout () =
  let layout = Toffee.Style.(default |> set_flex_grow Float.nan) in
  (* [words] counts what reading the counters allocates too, which the
     difference of two counts cancels. *)
  let copy =
    words (fun () ->
        ignore (Toffee.Style.set_flex_grow 2. layout : Toffee.Style.t))
    - words ignore
  in
  let text f = words (fun () -> ignore (f () : unit Mosaic.t)) in
  let shared = text (fun () -> Mosaic.text ~layout "x")
  and overridden = text (fun () -> Mosaic.text ~layout ~flex_grow:2. "x") in
  if overridden - shared <> copy then
    failf
      "a text given a shared layout allocates %d words, and with one label \
       overriding a field %d, where a copy of the layout is %d"
      shared overridden copy;
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let declare () =
    Mosaic.Reconciler.render reconciler ~viewport_width:80
      (Mosaic.text ~layout "x")
  in
  declare ();
  let node = List.hd (Renderable.children (Renderer.root renderer)) in
  is_true ~msg:"the node holds the shared layout"
    (Renderable.style node == layout);
  Test_harness.register renderer;
  is_false ~msg:"declared again, the text is not laid out again"
    (Test_harness.measured_again node declare);
  is_true ~msg:"declared with a layout of its own, the text is laid out again"
    (Test_harness.measured_again node (fun () ->
         Mosaic.Reconciler.render reconciler ~viewport_width:80
           (Mosaic.text
              ~layout:(Toffee.Style.set_flex_grow Float.nan layout)
              "x")))

(* ───── Placing reordered children ─────

   Rotating a keyed list by one moves every child, so placement must cost
   time linear in the children whatever their order. Allocation cannot show
   it, as a rotation allocates in proportion to the children whether placement
   is linear or quadratic. The check compares the time of a rotation at 1,000
   and at 16,000 children. Linear placement measures a ratio of 16 to 40,
   more than 16 as the larger tree leaves the processor's caches, and
   quadratic placement about 260. The threshold, 96, leaves each a margin of
   2.4 or more.

   It reads the process's CPU time, which other processes do not inflate as
   they do the wall clock when tests run in parallel. A sample repeats the
   rotation for at least 50 ms, so that a clock that advances by a
   millisecond or more, as on Windows, still reads it, and the check keeps
   the best of five. It catches a change of complexity only. A constant
   factor leaves the ratio as it is; the reconciler benchmark's time
   baselines, per machine, catch those. *)

let cpu_time_per f =
  let start = Sys.time () in
  let rec repeat runs =
    f ();
    let elapsed = Sys.time () -. start in
    if elapsed < 0.05 then repeat (runs + 1) else elapsed /. Float.of_int runs
  in
  repeat 1

(* A step renders the list rotated by one, then in order again, both views
   built before it is timed. *)
let rotation_time n =
  let view offset =
    Mosaic.box
      (List.init n (fun i ->
           let key = string_of_int ((i + offset) mod n) in
           Mosaic.text ~key key))
  in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let render view = Mosaic.Reconciler.render reconciler ~viewport_width:80 view
  and in_order = view 0
  and rotated = view 1 in
  render in_order;
  let step () =
    render rotated;
    render in_order
  in
  let best = ref infinity in
  for _ = 1 to 5 do
    best := Float.min !best (cpu_time_per step)
  done;
  !best

let rotated_children () =
  let small = rotation_time 1_000 and large = rotation_time 16_000 in
  let ratio = large /. small in
  if ratio > 96. then
    failf
      "rotating 16,000 children takes %.0f times as long as 1,000 (%.3f ms, \
       %.3f ms), over 96"
      ratio (large *. 1e3) (small *. 1e3)

(* ───── A memo's key ─────

   A memo's key is checked for functions in full when the memo mounts, and
   afterwards only where it is not the key compared last, physically. A
   keystroke beside a memo keyed on the lines of a transcript therefore
   costs the same time whatever the number of lines: the time of 16,000
   lines over that of 1,000 is near 1, where walking the key each render
   makes it 16. The threshold, 4, leaves each a margin of 4. *)

let keystroke_time lines =
  let lines = List.init lines (fun i -> Printf.sprintf "line %d" i) in
  let transcript lines = Mosaic.box (List.map (fun l -> Mosaic.text l) lines) in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let view input =
    Mosaic.box [ Mosaic.memo ~key:lines transcript; Mosaic.text input ]
  in
  let render input =
    Mosaic.Reconciler.render reconciler ~viewport_width:80 (view input)
  in
  render "a";
  let step () =
    render "ab";
    render "a"
  in
  let best = ref infinity in
  for _ = 1 to 5 do
    best := Float.min !best (cpu_time_per step)
  done;
  !best

let memo_key_check () =
  let small = keystroke_time 1_000 and large = keystroke_time 16_000 in
  let ratio = large /. small in
  if ratio > 4. then
    failf
      "a keystroke beside a memo keyed on 16,000 lines takes %.1f times as \
       long as on 1,000 (%.3f ms, %.3f ms), over 4"
      ratio (large *. 1e3) (small *. 1e3)

(* ───── Removing children ─────

   Removing children compacts their parent's children in one pass, in the
   node and in the layout tree, so it allocates nothing for the children it
   keeps. Removing the first, the last, every other or ten scattered children
   of 1,000 and of 16,000 allocates the same words. In a culling column, whose
   layout tree holds the children in chunks, a removed child costs words of
   its own, so every other child is left out there. The time of removing
   every other child, then placing it back, is linear in the children, as
   with rotations above. *)

let rows ~cull n =
  let renderer = Renderer.create () in
  let column =
    Renderable.create ~parent:(Renderer.root renderer)
      ~style:Toffee.Style.(default |> set_flex_direction Flex_direction.Column)
      ()
  in
  if cull then Renderable.set_viewport_cull column true;
  let row =
    Toffee.Style.(
      default
      |> set_size { width = Dimension.length 10.; height = Dimension.length 1. })
  in
  let children =
    Array.init n (fun _ -> Renderable.create ~parent:column ~style:row ())
  in
  Renderer.render_frame renderer ~width:20 ~height:10 ~dt:0.;
  (renderer, column, children)

let first children = [ List.hd children ]
let last children = [ List.nth children (List.length children - 1) ]
let every_other children = List.filteri (fun i _ -> i mod 2 = 0) children

let scattered children =
  let step = List.length children / 10 in
  List.filteri (fun i _ -> i mod step = 3 && i / step < 10) children

(* The words of each removal, made one after the other from the same
   column. *)
let removal_words ~cull n removals =
  let renderer, column, _ = rows ~cull n in
  List.map
    (fun (what, pick) ->
      let gone = pick (Renderable.children column) in
      let minor, promoted, major = Gc.counters () in
      Renderable.detach_all gone;
      let minor', promoted', major' = Gc.counters () in
      ignore (Sys.opaque_identity renderer : Renderer.t);
      ( what,
        Float.to_int
          (minor' -. minor +. (major' -. major) -. (promoted' -. promoted)) ))
    removals

let removal_time ~cull n =
  let renderer, column, children = rows ~cull n in
  let gone = every_other (Array.to_list children) in
  let step () =
    Renderable.detach_all gone;
    Renderable.place_children column 0 (fun f -> Array.iter f children)
  in
  let best = ref infinity in
  for _ = 1 to 5 do
    best := Float.min !best (cpu_time_per step)
  done;
  ignore (Sys.opaque_identity renderer : Renderer.t);
  !best

let removed_children ~cull () =
  let removals =
    [
      ("the first child", first);
      ("the last child", last);
      ("ten scattered children", scattered);
    ]
    @ if cull then [] else [ ("every other child", every_other) ]
  in
  List.iter2
    (fun (what, small) (_, large) -> same ("removing " ^ what) ~small ~large)
    (removal_words ~cull 1_000 removals)
    (removal_words ~cull 16_000 removals);
  let small = removal_time ~cull 1_000 and large = removal_time ~cull 16_000 in
  let ratio = large /. small in
  if ratio > 96. then
    failf
      "removing every other of 16,000 children takes %.0f times as long as of \
       1,000 (%.3f ms, %.3f ms), over 96"
      ratio (large *. 1e3) (small *. 1e3)

(* ───── Memory ───── *)

let mib = 1024 * 1024

let at_most ~msg cap bytes =
  if bytes > cap then failf "%s: %d bytes, over the cap of %d" msg bytes cap

let live_bytes () = (Gc.stat ()).live_words * (Sys.word_size / 8)
let peak_bytes () = (Gc.stat ()).top_heap_words * (Sys.word_size / 8)

(* A megabyte at width 1 is a million rows, which a layout builds when one of
   them is asked. *)
let million_rows () =
  let module L = Matrix_text.Layout in
  Gc.full_major ();
  let before = live_bytes () in
  let layout = L.create (L.Source.of_string megabyte) ~width:1 ~wrap:`Char in
  ignore (L.row_start layout (L.row_count layout - 1) : int);
  Gc.full_major ();
  let retained = live_bytes () - before in
  let replacement =
    L.create (L.Source.of_string (megabyte ^ "y")) ~width:1 ~wrap:`Char
  in
  Gc.full_major ();
  let peak = peak_bytes () in
  ignore (Sys.opaque_identity (layout, replacement));
  at_most ~msg:"a source and its million rows, retained" (128 * mib) retained;
  at_most ~msg:"heap peak while a second layout is built" (256 * mib) peak

(* A megabyte of prose in paragraphs of 585 bytes, laid out at the four widths
   a text surface caches, every row asked for. The caps date from a source
   that indexed every grapheme and a layout that built every row, measured at
   20.4 MiB retained and 42.0 MiB at peak. Laid out per line, with rows built
   when asked, it is 1.2 MiB and 5.2 MiB; one more array per grapheme
   (7.6 MiB) now exceeds the per-line caps below instead. The peak allows the
   old and the new text's geometry together. *)
let prose =
  let sentence = "The quick brown fox jumps over the lazy dog; " in
  let paragraph = String.concat "" (List.init 13 (fun _ -> sentence)) in
  String.concat "\n" (List.init 1_709 (fun _ -> paragraph))

let cached_widths () =
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  Gc.full_major ();
  let base = live_bytes () in
  Text_buffer.set_text buffer prose;
  List.iter
    (fun width ->
      Text_surface.set_wrap_width surface (Some width);
      let layout = Text_surface.layout surface in
      for row = 0 to Matrix_text.Layout.row_count layout - 1 do
        ignore (Matrix_text.Layout.row_start layout row : int)
      done)
    [ 40; 60; 80; 120 ];
  ignore
    (Matrix_text.Layout.measure
       (Text_buffer.source buffer)
       ~width:100 ~wrap:`Word
      : int * int);
  Gc.full_major ();
  let retained = live_bytes () - base in
  Text_buffer.set_text buffer (prose ^ "y");
  ignore (Text_surface.display_line_count surface : int);
  Gc.full_major ();
  let peak = peak_bytes () in
  ignore (Sys.opaque_identity (renderer, surface));
  at_most ~msg:"a megabyte of prose at four cached widths, retained" (24 * mib)
    retained;
  at_most ~msg:"heap peak while the text is replaced" (48 * mib) peak

(* A logical line costs five words at one geometry: its byte start, its first
   cluster and its index in the source, and the rows before it and its rows in
   the layout. A line asked about that holds more than printable ASCII adds
   two words per cluster and three, and a shown line that wraps two words per
   row. The caps allow one word a line more. Indexing the whole text and its rows, as a source did before
   it indexed lines apart, held 106 bytes a line on lines of 2 bytes, 938 on
   lines of 50 and 846 on lines of Greek and CJK. *)
let per_line () =
  let module L = Matrix_text.Layout in
  let retained ~shown text =
    Gc.full_major ();
    let before = live_bytes () in
    let source = L.Source.of_string text in
    let layout = L.create source ~width:80 ~wrap:`Word in
    let first = if shown = `Forty then L.row_count layout / 2 else 0 in
    let last = if shown = `Forty then first + 39 else L.row_count layout - 1 in
    for row = first to last do
      L.iter_cells layout ~row ~first_column:0 ~last_column:80
        (fun ~first:_ ~last:_ ~column:_ ~width:_ ~ellipsis:_ -> ())
    done;
    Gc.full_major ();
    let bytes = live_bytes () - before in
    ignore (Sys.opaque_identity layout);
    (bytes, L.Source.line_count source, L.Source.length source)
  in
  let short = String.concat "" (List.init 500_000 (fun _ -> "x\n")) in
  let prose =
    String.concat ""
      (List.init 10_000 (fun i ->
           String.sub "The quick brown fox jumps over the lazy dog; it naps.\n"
             (i mod 20)
             (54 - (i mod 20))))
  in
  let other =
    String.concat ""
      (List.init 6_000 (fun i ->
           Printf.sprintf "%05d αβγδε ζηθ 漢字かな交じり文 ιкλμ %s\n" i
             (String.make (i mod 30) 'x')))
  in
  List.iter
    (fun (what, text) ->
      let bytes, lines, _ = retained ~shown:`Forty text in
      at_most ~msg:(what ^ ", forty rows shown") (48 * lines) bytes)
    [
      ("lines of 2 bytes", short);
      ("lines of 50 bytes", prose);
      ("lines of Greek and CJK", other);
    ];
  let bytes, lines, clusters = retained ~shown:`All other in
  at_most ~msg:"lines of Greek and CJK, every row shown"
    ((72 * lines) + (16 * clusters))
    bytes

let () =
  run "mosaic.cost"
    [
      (* First, in this order: a heap peak is the process's highest yet. *)
      group "Memory"
        [
          test "four cached widths of a megabyte" cached_widths;
          test "a million rows" million_rows;
          test "per line" per_line;
        ];
      group "Repaint follows the visible area"
        [
          test "hidden rows" hidden_rows;
          test "hidden columns" hidden_columns;
          test "hidden overlays" hidden_overlay_count;
          test "hidden wrapped rows" hidden_wrapped_rows;
          test "visible rows and runs" visible_work;
        ];
      group "Frames follow the visible area"
        [ test "a scrolled transcript" hidden_transcript_rows ];
      group "An element that declares nothing shares its records"
        [ test "a text" default_text ];
      group "An unchanged list costs nothing per element"
        [ test "a fresh but equal list of texts" unchanged_list ];
      group "A map costs nothing per element"
        [ test "under four maps" nested_maps ];
      group "A memo's key is checked where it changed"
        [ test "a keystroke beside a memo" memo_key_check ];
      group "A keyed log costs what changed"
        [ test "append, capped append and edit" keyed_log_changes ];
      group "A capped log's frame costs what changed"
        [ test "5,000 and 50,000 lines" capped_log_frames ];
      group "A shared layout costs nothing per element"
        [ test "a text given ~layout" shared_layout ];
      group "Keys follow the items moved over"
        [ test "a cursor's Down and Up" cursor_keys ];
      group "Placement is linear in the children"
        [ test "rotated keyed children" rotated_children ];
      group "Removal keeps nothing per kept child"
        [
          test "a column" (removed_children ~cull:false);
          test "a culling column" (removed_children ~cull:true);
        ];
    ]
