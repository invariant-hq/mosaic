(* The view's text selection (RFC 0006): its model, region and band (§3.1,
   §3.2), copy (§3.3), selectability (§3.4), units and the modified presses
   (§3.5), keys (§3.6), changes (§3.7), change under a selection (§3.8),
   highlight (§3.9) and the editors' own selection (§3.10). *)

open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let width = 40
let height = 12

(* A frame drawn and presented, so that the pointer finds what it shows. *)
let frame r =
  Renderer.render_frame r ~width ~height ~dt:0.;
  ignore (Renderer.render ~full:true r : string)

let send r kind ~x ~y =
  ignore
    (Test_harness.dispatch_mouse r (Test_harness.mouse_report ~x ~y kind)
      : Event.mouse)

let press r ~x ~y = send r (Press Left) ~x ~y
let drag_to r ~x ~y = send r (Motion (Some Left)) ~x ~y
let release r ~x ~y = send r (Release (Some Left)) ~x ~y

(* A drag from one cell to another, released there. *)
let select r (x0, y0) (x1, y1) =
  press r ~x:x0 ~y:y0;
  drag_to r ~x:x1 ~y:y1;
  release r ~x:x1 ~y:y1

let copied r = Renderer.selection_text r

(* The [n] presses of a series at one cell, 50 ms apart, each released but
   the last, which stays held. *)
let presses ?(shift = false) ?(ctrl = false) r ~x ~y n =
  let start = Test_harness.tick () in
  for i = 1 to n do
    let now = start + (i * 50_000_000) in
    let dispatch kind =
      ignore
        (Test_harness.dispatch_mouse ~now r
           (Test_harness.mouse_report ~shift ~ctrl ~x ~y kind)
          : Event.mouse)
    in
    dispatch (Press Left);
    if i < n then dispatch (Release (Some Left))
  done

(* [n] clicks of a series at one cell, the last released. *)
let clicks ?shift r ~x ~y n =
  presses ?shift r ~x ~y n;
  release r ~x ~y

let place ~x ~y ~w ~h =
  let lpa = Toffee.Style.Length_percentage_auto.length in
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

let text_at ?(props = fun content -> Text.Props.make ~content ()) parent ~x ~y
    ~w ~h content =
  Text.create ~parent ~style:(place ~x ~y ~w ~h) (props content)

(* A column of the full width, its children at their own height. *)
let column ?(gap = 0) parent =
  Renderable.create ~parent
    ~style:
      (Toffee.Style.default
      |> Toffee.Style.set_flex_direction Column
      |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
      |> Toffee.Style.set_gap
           {
             width = Toffee.Style.Length_percentage.length 0.;
             height = Toffee.Style.Length_percentage.length (Float.of_int gap);
           })
    ()

let line parent content = Text.create ~parent (Text.Props.make ~content ())

(* A scroll box of [w] by [h] cells, without bars, that takes its own
   proposals. *)
let scroll_box ?style parent ~w ~h =
  let style =
    match style with
    | Some style -> style
    | None ->
        Toffee.Style.default
        |> Toffee.Style.set_width
             (Toffee.Style.Dimension.length (Float.of_int w))
        |> Toffee.Style.set_height
             (Toffee.Style.Dimension.length (Float.of_int h))
        |> Toffee.Style.set_flex_shrink 0.
  in
  let sb =
    Scroll_box.create ~parent ~style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  sb

(* A view mounted in a renderer of its own, whose messages are logged. *)
let mount view =
  let r = Renderer.create () in
  let log = ref [] in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root r) ~deliver:(fun m ->
        log := m :: !log)
  in
  Mosaic.Reconciler.render reconciler ~viewport_width:width view;
  (match Mosaic.Reconciler.focused reconciler with
  | Some node -> ignore (Renderer.focus r node : bool)
  | None -> ());
  frame r;
  (r, reconciler, log)

(* Whether the frame drawn last, not yet presented, shows the cell in
   reverse, as the plain theme's [Highlight] does. *)
let inverse r ~x ~y = Test_harness.has Inverse (Test_harness.cell_style r ~x ~y)
let paint r = Renderer.render_frame r ~width ~height ~dt:0.

(* ── §3.1 Model ── *)

(* A press in a margin starts at the nearest text of its region: the first
   at or after the pressed cell, in row-major order. *)
let a_press_in_a_margin_starts_at_the_nearest_text () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:5 ~y:0 ~w:10 ~h:1 "hello" : Text.t);
  frame r;
  select r (0, 0) (7, 0);
  equal ~msg:"from the left margin" (option string) (Some "hel") (copied r);
  select r (20, 0) (6, 0);
  equal ~msg:"from the right margin, backwards" (option string) (Some "ello")
    (copied r)

(* With no text at or after the pressed cell, the press finds the last text
   before it, at its end. *)
let a_press_below_the_last_text_starts_at_its_end () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:1 "hello" : Text.t);
  frame r;
  select r (3, 5) (2, 0);
  equal ~msg:"up to the text's end" (option string) (Some "llo") (copied r)

(* A press in a region that holds no text clears the selection. *)
let a_press_with_no_text_clears () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:1 "hello" : Text.t);
  let sb =
    scroll_box (Renderer.root r) ~w:0 ~h:0 ~style:(place ~x:0 ~y:3 ~w:20 ~h:5)
  in
  ignore (sb : Scroll_box.t);
  frame r;
  select r (0, 0) (2, 0);
  equal ~msg:"selected" (option string) (Some "hel") (copied r);
  press r ~x:2 ~y:4;
  is_none ~msg:"cleared" (copied r)

(* A press beside a scroll box passes over the texts of that box, a region
   of its own, to the nearest text of the pressed cell's region. *)
let a_press_beside_a_nested_box_resolves_past_its_texts () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "above" : Text.t);
  let sb = scroll_box col ~w:20 ~h:3 in
  for i = 0 to 5 do
    ignore (line (Scroll_box.node sb) (Printf.sprintf "in-%d" i) : Text.t)
  done;
  ignore (line col "below" : Text.t);
  frame r;
  select r (25, 2) (2, 4);
  equal ~msg:"from the text below the box" (option string) (Some "bel")
    (copied r)

(* Law 1: the ends name text, so a rewrap leaves the selection on the words
   it held. *)
let the_ends_name_text_through_a_rewrap () =
  let r = Renderer.create () in
  let t =
    text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:3
      ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
      "alpha beta gamma"
  in
  frame r;
  select r (6, 0) (9, 0);
  equal ~msg:"before" (option string) (Some "beta") (copied r);
  Renderable.set_style (Text.node t) (place ~x:0 ~y:0 ~w:6 ~h:3);
  frame r;
  equal ~msg:"rewrapped" (option string) (Some "beta") (copied r)

(* §3.1 Focus: at the release the focus becomes the band's far end, so the
   band does not move. A far end where a row wraps stays at the end of that
   row, and the text beside it stays unselected. *)
let the_release_keeps_the_band_where_it_was () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore
    (text_at root ~x:0 ~y:0 ~w:6 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "hello world"
      : Text.t);
  ignore (text_at root ~x:10 ~y:0 ~w:5 ~h:2 "abc\ndef" : Text.t);
  frame r;
  press r ~x:0 ~y:0;
  drag_to r ~x:5 ~y:0;
  let dragging = copied r in
  equal ~msg:"while dragging" (option string) (Some "hello ") dragging;
  release r ~x:5 ~y:0;
  equal ~msg:"after the release" (option string) dragging (copied r)

(* ── §3.2 Region and band ── *)

(* Ten rows in a scroll box four rows tall, with a footer below it. *)
let rows_in_a_box () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  let sb = scroll_box col ~w:20 ~h:4 in
  for i = 0 to 9 do
    ignore (line (Scroll_box.node sb) (Printf.sprintf "row-%d" i) : Text.t)
  done;
  ignore (line col "footer" : Text.t);
  frame r;
  (r, sb)

(* Law 3: a drag that leaves the scroll box downward is held at its last
   row, and never reaches the footer below. *)
let a_drag_out_of_a_scroll_box_downward_stays_in_it () =
  let r, _ = rows_in_a_box () in
  press r ~x:0 ~y:1;
  drag_to r ~x:3 ~y:4;
  equal ~msg:"held at the box's last row" (option string)
    (Some "row-1\nrow-2\nrow-") (copied r)

(* Sideways, the pointer is held at the view's last column, short of the
   text the view does not show. *)
let a_drag_out_of_a_scroll_box_sideways_stays_in_it () =
  let r = Renderer.create () in
  let sb =
    Scroll_box.create ~parent:(Renderer.root r)
      ~style:(place ~x:0 ~y:0 ~w:10 ~h:2)
      (Scroll_box.Props.make ~scroll_x:true ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  ignore (line (Scroll_box.node sb) "0123456789abcdefghij" : Text.t);
  frame r;
  press r ~x:0 ~y:0;
  drag_to r ~x:30 ~y:0;
  equal ~msg:"to the view's last column" (option string) (Some "0123456789")
    (copied r)

(* Law 3: the band at rest does not move under the wheel. *)
let a_selection_at_rest_stays_under_the_wheel () =
  let r, sb = rows_in_a_box () in
  select r (0, 1) (4, 2);
  equal ~msg:"before" (option string) (Some "row-1\nrow-2") (copied r);
  send r (Scroll `Down) ~x:1 ~y:1;
  send r (Scroll `Down) ~x:1 ~y:1;
  frame r;
  equal ~msg:"scrolled" int 2 (Scroll_box.scroll_top sb);
  equal ~msg:"after" (option string) (Some "row-1\nrow-2") (copied r)

(* A text hidden, or below a hidden node, takes no part. *)
let hidden_text_takes_no_part () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "one" : Text.t);
  let two = line col "two" in
  ignore (line col "three" : Text.t);
  Renderable.set_visible (Text.node two) false;
  frame r;
  select r (0, 0) (4, 1);
  equal ~msg:"without the hidden text" (option string) (Some "one\nthree")
    (copied r)

(* §3.2 Region: a selection in the root's layer holds nothing of the top
   layer, which is a region of its own, painted over it. *)
let a_selection_leaves_the_top_layer_out () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "one" : Text.t);
  ignore (line col "two" : Text.t);
  frame r;
  select r (0, 0) (2, 1);
  let top =
    Renderable.create ~parent:col ~style:(place ~x:0 ~y:0 ~w:5 ~h:2) ()
  in
  ignore (line top "modal" : Text.t);
  Renderer.set_modal r (Some top);
  frame r;
  equal ~msg:"the root layer's text alone" (option string) (Some "one\ntwo")
    (copied r)

(* A selection that starts in the top layer has the modal node for its
   region: it holds nothing of the layer below, though that layer's text
   lies between its ends on the screen, and the pointer is held in the
   modal's box. *)
let a_selection_in_the_top_layer_stays_in_it () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:10 ~y:0 ~w:5 ~h:2 "R0\nR1" : Text.t);
  let top =
    Renderable.create ~parent:root
      ~style:
        (place ~x:0 ~y:0 ~w:8 ~h:2 |> Toffee.Style.set_flex_direction Column)
      ()
  in
  ignore (line top "alpha" : Text.t);
  ignore (line top "beta" : Text.t);
  Renderer.set_modal r (Some top);
  frame r;
  select r (0, 0) (2, 1);
  equal ~msg:"the modal's text alone" (option string) (Some "alpha\nbet")
    (copied r);
  press r ~x:0 ~y:0;
  drag_to r ~x:2 ~y:8;
  equal ~msg:"held at the modal's last row" (option string) (Some "alpha\nbet")
    (copied r)

(* A text above a scroll box of six rows, three in view, and a text below
   it. *)
let nested_box () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "above" : Text.t);
  let sb = scroll_box col ~w:20 ~h:3 in
  for i = 0 to 5 do
    ignore (line (Scroll_box.node sb) (Printf.sprintf "in-%d" i) : Text.t)
  done;
  ignore (line col "below" : Text.t);
  frame r;
  r

(* §3.2 Nested box: a scroll box the band covers is selected whole, its rows
   out of view included (Law 2). *)
let a_nested_box_covered_is_selected_whole () =
  let r = nested_box () in
  select r (0, 0) (2, 4);
  equal ~msg:"the box's text whole" (option string)
    (Some "above\nin-0\nin-1\nin-2\nin-3\nin-4\nin-5\nbel") (copied r)

(* A scroll box the band covers in part is not selected, and a pointer over
   it is as over a blank. *)
let a_nested_box_partly_covered_is_left_out () =
  let r = nested_box () in
  press r ~x:0 ~y:0;
  drag_to r ~x:2 ~y:2;
  equal ~msg:"while dragging" (option string) (Some "above") (copied r);
  release r ~x:2 ~y:2;
  equal ~msg:"settled" (option string) (Some "above") (copied r)

(* The band is highlighted as it is copied: the covered box's rows in view
   are painted with the [Highlight] role. *)
let a_nested_box_covered_is_highlighted () =
  let r = nested_box () in
  select r (0, 0) (2, 4);
  paint r;
  is_true ~msg:"a row of the box" (inverse r ~x:1 ~y:2);
  is_true ~msg:"the text below, to the pointer" (inverse r ~x:2 ~y:4);
  is_false ~msg:"after the pointer" (inverse r ~x:3 ~y:4)

(* ── §3.3 Copy ── *)

(* "foo" above "bar" copies as [foo\nbar]. *)
let stacked_texts_copy_on_their_own_lines () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "foo" : Text.t);
  ignore (line col "bar" : Text.t);
  frame r;
  select r (0, 0) (2, 1);
  equal ~msg:"a line each" (option string) (Some "foo\nbar") (copied r)

(* Two columns side by side, selected from L1 to R2, copy row by row, the
   cells between as spaces, as a terminal copies them. *)
let side_by_side_text_copies_row_by_row () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:2 ~h:5 "L0\nL1\nL2\nL3\nL4" : Text.t);
  ignore (text_at root ~x:5 ~y:0 ~w:2 ~h:5 "R0\nR1\nR2\nR3\nR4" : Text.t);
  frame r;
  select r (0, 1) (6, 2);
  equal ~msg:"row by row" (option string) (Some "L1   R1\nL2   R2") (copied r)

(* Paragraphs a gap apart copy with a blank line between them. *)
let paragraphs_copy_with_a_blank_line () =
  let r = Renderer.create () in
  let col = column ~gap:1 (Renderer.root r) in
  ignore (line col "first para" : Text.t);
  ignore (line col "second para" : Text.t);
  frame r;
  select r (0, 0) (5, 2);
  equal ~msg:"a blank line" (option string) (Some "first para\n\nsecond")
    (copied r)

(* A bordered box between paragraphs reads as a block of its own: its border
   rows are its own, the gaps around it are margins. *)
let a_bordered_box_between_paragraphs () =
  let r = Renderer.create () in
  let col = column ~gap:1 (Renderer.root r) in
  ignore (line col "intro" : Text.t);
  let box = Box.create ~parent:col (Box.Props.make ~border:true ()) in
  ignore (line (Box.node box) "code" : Text.t);
  ignore (line col "outro" : Text.t);
  frame r;
  select r (0, 0) (4, 6);
  equal ~msg:"blocks a blank line apart" (option string)
    (Some "intro\n\ncode\n\noutro") (copied r)

(* The markdown table [| a | b |] with rows [1 2] and [3 4] copies its cells
   with the cells between as spaces, and its bordered rows a line apart. *)
let the_markdown_table_copies_row_by_row () =
  let r = Renderer.create () in
  ignore
    (Markdown.create ~parent:(Renderer.root r)
       (Markdown.Props.make
          ~content:"| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |" ())
      : Markdown.t);
  frame r;
  select r (0, 0) (width - 1, height - 1);
  let lines =
    match copied r with
    | Some text -> String.split_on_char '\n' text
    | None -> []
  in
  let words line =
    List.filter (fun w -> w <> "") (String.split_on_char ' ' line)
  in
  equal ~msg:"a row a line, the cells apart"
    (list (list string))
    [ [ "a"; "b" ]; [ "1"; "2" ]; [ "3"; "4" ] ]
    (List.map words lines)

(* An ellipsis copies the text it hides. *)
let an_ellipsis_copies_what_it_hides () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:1
       ~props:(fun content -> Text.Props.make ~content ~truncate:true ())
       "hello world"
      : Text.t);
  frame r;
  select r (0, 0) (5, 0);
  equal ~msg:"the hidden suffix" (option string) (Some "hello world") (copied r)

(* A soft wrap adds no line break. *)
let a_soft_wrap_adds_nothing () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "hello world"
      : Text.t);
  frame r;
  select r (0, 0) (4, 1);
  equal ~msg:"no break" (option string) (Some "hello world") (copied r)

(* A line terminator that begins a fragment is dropped, as one that ends
   it: the separator stands for it. *)
let a_fragment_drops_a_terminator_at_either_end () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:2 "ab\ncd" : Text.t);
  frame r;
  select r (4, 0) (1, 1);
  equal ~msg:"from after the first row" (option string) (Some "cd") (copied r);
  select r (0, 0) (4, 0);
  equal ~msg:"to the first row's end" (option string) (Some "ab") (copied r)

(* Trailing whitespace is copied. *)
let trailing_whitespace_is_copied () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:1 "ab   " : Text.t);
  frame r;
  select r (0, 0) (9, 0);
  equal ~msg:"kept" (option string) (Some "ab   ") (copied r)

(* §3.4 Semantics: a selection across text it does not reach leaves that
   text out. *)
let a_selection_across_unselectable_text_leaves_it_out () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:1 ~h:1 "a" : Text.t);
  ignore
    (text_at root ~x:1 ~y:0 ~w:1 ~h:1
       ~props:(fun content -> Text.Props.make ~content ~selectable:false ())
       "|"
      : Text.t);
  ignore (text_at root ~x:2 ~y:0 ~w:1 ~h:1 "b" : Text.t);
  frame r;
  select r (0, 0) (2, 0);
  equal ~msg:"its cell a space" (option string) (Some "a b") (copied r)

(* ── §3.4 Selectability ── *)

(* An element that acts on a press declares itself unselectable, and keeps
   it without [selectable]; [selectable] declares otherwise, and dropping it
   gives the default back. *)
let an_absent_selectable_keeps_a_buttons_default () =
  let button ?selectable () = Mosaic.button ?selectable ~on_activate:() "OK" in
  let r, reconciler, _ = mount (button ()) in
  let node () =
    match Renderable.children (Renderer.root r) with
    | [ node ] -> node
    | _ -> fail "expected the button alone"
  in
  is_false ~msg:"a button refuses" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width
    (button ~selectable:true ());
  is_true ~msg:"declared selectable" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width (button ());
  is_false ~msg:"the default again" (Renderable.selectable (node ()))

let an_absent_selectable_keeps_a_sliders_default () =
  let slider ?selectable () =
    Mosaic.slider ?selectable ~state:0. ~on_change:(fun _ -> ()) ()
  in
  let r, reconciler, _ = mount (slider ()) in
  let node () =
    match Renderable.children (Renderer.root r) with
    | [ node ] -> node
    | _ -> fail "expected the slider alone"
  in
  is_false ~msg:"a slider refuses" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width
    (slider ~selectable:true ());
  is_true ~msg:"declared selectable" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width (slider ());
  is_false ~msg:"the default again" (Renderable.selectable (node ()))

(* A kind of the application's own whose widget declares itself
   unselectable when it is created. *)
let pad : (unit, unit, Renderable.t) Mosaic.Element.kind =
  Mosaic.Element.kind ~name:"pad"
    ~create:(fun ~parent ~layout () ->
      let node = Renderable.create ~parent ~style:layout () in
      Renderable.set_selectable node (Some false);
      node)
    ~node:Fun.id
    ~update:(fun _ () -> ())
    ~equal:(fun () () -> true)
    ()

(* An application's widget keeps its own declaration once its element stops
   declaring another. *)
let an_absent_selectable_keeps_a_user_kinds_declaration () =
  let pad ?selectable () = Mosaic.element ?selectable pad () [] in
  let r, reconciler, _ = mount (pad ()) in
  let node () =
    match Renderable.children (Renderer.root r) with
    | [ node ] -> node
    | _ -> fail "expected the pad alone"
  in
  is_false ~msg:"its widget refuses" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width
    (pad ~selectable:true ());
  is_true ~msg:"declared selectable" (Renderable.selectable (node ()));
  Mosaic.Reconciler.render reconciler ~viewport_width:width (pad ());
  is_false ~msg:"its widget's declaration again"
    (Renderable.selectable (node ()))

(* Law 4: a press on a button, a canvas or a line number's gutter, with a
   selection present, leaves the selection, and the button still acts. *)
let a_press_on_unselectable_content_changes_nothing () =
  let view =
    Mosaic.box ~flex_direction:Column
      [
        Mosaic.text "hello";
        Mosaic.button ~on_activate:"activated" "OK";
        Mosaic.canvas ~size:(Mosaic.size ~width:4 ~height:1) (fun _ ~dt:_ -> ());
        Mosaic.line_number (Mosaic.text "numbered");
      ]
  in
  let r, _, log = mount view in
  select r (0, 0) (2, 0);
  equal ~msg:"selected" (option string) (Some "hel") (copied r);
  List.iter
    (fun (what, (x, y)) ->
      press r ~x ~y;
      release r ~x ~y;
      equal ~msg:what (option string) (Some "hel") (copied r))
    [ ("button", (0, 1)); ("canvas", (0, 2)); ("gutter", (0, 3)) ];
  equal ~msg:"the button acted" (list string) [ "activated" ] !log

(* A text that declares itself selectable inside a box that refuses
   selection is selectable. *)
let a_selectable_text_inside_an_unselectable_box () =
  let view =
    Mosaic.box ~selectable:false ~flex_direction:Column
      [ Mosaic.text ~selectable:true "inner"; Mosaic.text "other" ]
  in
  let r, _, _ = mount view in
  select r (0, 0) (4, 0);
  equal ~msg:"the inner text" (option string) (Some "inner") (copied r);
  press r ~x:1 ~y:1;
  release r ~x:1 ~y:1;
  equal ~msg:"a press on the refusing text changes nothing" (option string)
    (Some "inner") (copied r)

(* A line number's gutter refuses selection, and its child follows the
   declarations of its ancestors: selectable by default, not inside a box
   that refuses selection. *)
let a_line_numbers_child_follows_its_ancestors () =
  let numbered = Mosaic.line_number (Mosaic.text "numbered") in
  let r, _, _ = mount (Mosaic.box [ numbered ]) in
  select r (5, 0) (9, 0);
  is_some ~msg:"the child is selectable" (copied r);
  Renderer.clear_selection r;
  let r, _, _ = mount (Mosaic.box ~selectable:false [ numbered ]) in
  select r (5, 0) (9, 0);
  is_none ~msg:"not inside a refusing box" (copied r)

(* ── §3.1 Anchor, §3.5 Pointer gestures ── *)

(* A drag that leaves the pressed cell and comes back to it selects the
   pressed grapheme: only a single click the pointer never left is empty. *)
let a_drag_back_to_the_pressed_cell_selects_its_grapheme () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:1 "hello" : Text.t);
  frame r;
  press r ~x:1 ~y:0;
  is_none ~msg:"a click" (copied r);
  drag_to r ~x:3 ~y:0;
  drag_to r ~x:1 ~y:0;
  equal ~msg:"back at the pressed cell" (option string) (Some "e") (copied r);
  release r ~x:1 ~y:0;
  equal ~msg:"released there" (option string) (Some "e") (copied r)

(* The band ends before the cell after the selection's last grapheme: a text
   right beside the end holds none of it, while dragging or at rest. *)
let the_band_stops_at_the_text_beside_its_end () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:3 ~h:2 "abc\nghi" : Text.t);
  ignore (text_at root ~x:3 ~y:0 ~w:3 ~h:2 "def\njkl" : Text.t);
  frame r;
  press r ~x:0 ~y:0;
  drag_to r ~x:2 ~y:0;
  equal ~msg:"dragging" (option string) (Some "abc") (copied r);
  release r ~x:2 ~y:0;
  equal ~msg:"at rest" (option string) (Some "abc") (copied r);
  clicks r ~x:1 ~y:0 2;
  equal ~msg:"a word beside the other text" (option string) (Some "abc")
    (copied r)

(* §3.5 Units: a double click selects a word, the run of whitespace, a
   boundary character, or the run of anything else under the pointer. *)
let a_double_click_selects_a_word () =
  let words content cells =
    let r = Renderer.create () in
    ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 content : Text.t);
    frame r;
    List.iter
      (fun (x, word) ->
        clicks r ~x ~y:0 2;
        equal
          ~msg:(Printf.sprintf "%S at %d" content x)
          (option string) (Some word) (copied r))
      cells
  in
  words "key: value"
    [ (0, "key"); (2, "key"); (3, ":"); (4, " "); (5, "value"); (9, "value") ];
  words "f(a, b)"
    [ (0, "f"); (1, "("); (2, "a"); (3, ","); (4, " "); (5, "b"); (6, ")") ];
  words "src/main.ml" [ (0, "src/main.ml"); (4, "src/main.ml") ];
  words "a  ::b" [ (1, "  "); (3, ":"); (4, ":") ]

(* The word shows at the press, before any release, and while a drag after
   it is painted. *)
let a_double_click_selects_at_the_press () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "alpha beta gamma" : Text.t);
  frame r;
  presses r ~x:7 ~y:0 2;
  equal ~msg:"held" (option string) (Some "beta") (copied r);
  paint r;
  is_false ~msg:"before the word" (inverse r ~x:5 ~y:0);
  is_true ~msg:"the word" (inverse r ~x:6 ~y:0);
  is_true ~msg:"to its end" (inverse r ~x:9 ~y:0);
  is_false ~msg:"after it" (inverse r ~x:10 ~y:0);
  drag_to r ~x:12 ~y:0;
  paint r;
  is_true ~msg:"the word dragged to, whole" (inverse r ~x:15 ~y:0)

(* A double click beside a row that wraps inside a word selects that word;
   past the end of a line, it selects nothing. *)
let a_double_click_past_a_rows_end () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore
    (text_at root ~x:0 ~y:0 ~w:3 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Char ())
       "abcdef"
      : Text.t);
  ignore (text_at root ~x:0 ~y:3 ~w:6 ~h:2 "ab\ncd" : Text.t);
  frame r;
  clicks r ~x:5 ~y:0 2;
  equal ~msg:"the word that wraps" (option string) (Some "abcdef") (copied r);
  clicks r ~x:4 ~y:3 2;
  is_none ~msg:"past a line's end" (copied r)

(* A triple click selects the logical line, however it wraps, without its
   terminator; a fourth click still does. *)
let a_triple_click_selects_the_logical_line () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:3
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "alpha beta\nnext"
      : Text.t);
  frame r;
  clicks r ~x:1 ~y:1 3;
  equal ~msg:"three clicks" (option string) (Some "alpha beta") (copied r);
  clicks r ~x:1 ~y:1 4;
  equal ~msg:"four clicks" (option string) (Some "alpha beta") (copied r)

(* A triple click in a margin selects the line of the text it resolves to:
   the first at or after the pressed cell, else the last before it. *)
let a_triple_click_in_a_margin_selects_the_nearest_line () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:5 ~y:0 ~w:10 ~h:1 "first" : Text.t);
  ignore (text_at root ~x:5 ~y:1 ~w:10 ~h:1 "second" : Text.t);
  frame r;
  clicks r ~x:0 ~y:1 3;
  equal ~msg:"the text after the cell" (option string) (Some "second")
    (copied r);
  clicks r ~x:20 ~y:0 3;
  equal ~msg:"after the first row" (option string) (Some "second") (copied r);
  clicks r ~x:0 ~y:5 3;
  equal ~msg:"below every text, the last before" (option string) (Some "second")
    (copied r)

(* A drag after a double click extends by words either way, and the word
   pressed stays selected. *)
let a_drag_after_a_double_click_extends_by_words () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma delta"
      : Text.t);
  frame r;
  presses r ~x:7 ~y:0 2;
  drag_to r ~x:13 ~y:0;
  equal ~msg:"forward" (option string) (Some "beta gamma") (copied r);
  drag_to r ~x:2 ~y:0;
  equal ~msg:"backward" (option string) (Some "alpha beta") (copied r);
  drag_to r ~x:8 ~y:0;
  equal ~msg:"inside the word pressed" (option string) (Some "beta") (copied r);
  drag_to r ~x:12 ~y:0;
  release r ~x:12 ~y:0;
  equal ~msg:"released" (option string) (Some "beta gamma") (copied r)

(* Over its text, the band reaches the far end of the unit under the
   pointer, so the text beside a line that wraps on is highlighted as it
   will be at the release. *)
let the_band_reaches_the_unit_under_the_pointer () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:20 ~h:1 "top" : Text.t);
  ignore
    (text_at root ~x:0 ~y:1 ~w:6 ~h:3
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "alpha beta gamma"
      : Text.t);
  ignore (text_at root ~x:10 ~y:1 ~w:5 ~h:3 "U0\nU1\nU2" : Text.t);
  frame r;
  presses r ~x:1 ~y:0 3;
  drag_to r ~x:1 ~y:1;
  let dragging = copied r in
  let holds piece =
    match dragging with
    | Some text ->
        let n = String.length piece in
        let rec at i =
          i + n <= String.length text
          && (String.sub text i n = piece || at (i + 1))
        in
        at 0
    | None -> false
  in
  is_true ~msg:"the line's last row" (holds "gamma");
  is_true ~msg:"the text beside its rows" (holds "U1");
  is_false ~msg:"none beside its last row" (holds "U2");
  release r ~x:1 ~y:1;
  equal ~msg:"after the release" (option string) dragging (copied r)

(* A pointer over a scroll box is as over a blank for a word too: the box is
   selected only once the band covers it. *)
let a_word_drag_selects_a_nested_box_once_covered () =
  let r = nested_box () in
  presses r ~x:1 ~y:0 2;
  drag_to r ~x:2 ~y:2;
  equal ~msg:"over the box" (option string) (Some "above") (copied r);
  drag_to r ~x:2 ~y:4;
  release r ~x:2 ~y:4;
  equal ~msg:"past it" (option string)
    (Some "above\nin-0\nin-1\nin-2\nin-3\nin-4\nin-5\nbelow") (copied r)

(* §3.1 Focus: a line drag over a nested box ends at the pointer's cell, and
   the release keeps it there: neither the box nor the text after it is
   selected. *)
let a_line_drag_over_a_nested_box_settles_where_it_was () =
  let r = nested_box () in
  presses r ~x:1 ~y:0 3;
  drag_to r ~x:2 ~y:2;
  equal ~msg:"dragging" (option string) (Some "above") (copied r);
  paint r;
  is_false ~msg:"the box, dragging" (inverse r ~x:0 ~y:1);
  release r ~x:2 ~y:2;
  equal ~msg:"settled" (option string) (Some "above") (copied r);
  paint r;
  is_false ~msg:"the box, settled" (inverse r ~x:0 ~y:1)

(* A line drag into a gap reaches the far end of the line the gap resolves
   to, and the release keeps the band there, with the text beside that line's
   rows. *)
let a_line_drag_into_a_gap_settles_where_it_was () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:10 ~h:1 "first" : Text.t);
  ignore
    (text_at root ~x:0 ~y:2 ~w:6 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "alpha beta"
      : Text.t);
  ignore (text_at root ~x:8 ~y:2 ~w:4 ~h:1 "SIDE" : Text.t);
  frame r;
  presses r ~x:1 ~y:0 3;
  drag_to r ~x:1 ~y:1;
  let dragging = copied r in
  paint r;
  let side = inverse r ~x:8 ~y:2 in
  release r ~x:1 ~y:1;
  equal ~msg:"the copy" (option string) dragging (copied r);
  paint r;
  equal ~msg:"the text beside" bool side (inverse r ~x:8 ~y:2);
  equal ~msg:"settled" (option string) (Some "first\n\nalpha    SIDE\nbeta")
    (copied r)

(* A drag after a triple click extends by lines. *)
let a_drag_after_a_triple_click_extends_by_lines () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:3 "one\ntwo\nthree" : Text.t);
  frame r;
  presses r ~x:1 ~y:1 3;
  equal ~msg:"pressed" (option string) (Some "two") (copied r);
  drag_to r ~x:0 ~y:2;
  equal ~msg:"down" (option string) (Some "two\nthree") (copied r);
  drag_to r ~x:4 ~y:0;
  equal ~msg:"up" (option string) (Some "one\ntwo") (copied r);
  release r ~x:4 ~y:0;
  equal ~msg:"released" (option string) (Some "one\ntwo") (copied r)

(* §3.1 Focus: the unit under a still pointer is found again when content
   scrolls under it. *)
let the_unit_under_the_pointer_follows_a_scroll () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  let sb = scroll_box col ~w:20 ~h:10 in
  for i = 0 to 29 do
    ignore (line (Scroll_box.node sb) (Printf.sprintf "row-%d" i) : Text.t)
  done;
  frame r;
  presses r ~x:1 ~y:4 3;
  drag_to r ~x:0 ~y:5;
  equal ~msg:"before" (option string) (Some "row-4\nrow-5") (copied r);
  send r (Scroll `Down) ~x:1 ~y:5;
  send r (Scroll `Down) ~x:1 ~y:5;
  frame r;
  equal ~msg:"scrolled" int 2 (Scroll_box.scroll_top sb);
  equal ~msg:"after" (option string) (Some "row-4\nrow-5\nrow-6\nrow-7")
    (copied r)

(* A ten-letter text, with "cdefg" selected forward ([backward = false]) or
   backward. *)
let letters ~backward =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "abcdefghij" : Text.t);
  frame r;
  if backward then select r (6, 0) (2, 0) else select r (2, 0) (6, 0);
  equal ~msg:"selected" (option string) (Some "cdefg") (copied r);
  r

(* §3.5 Shift+press: the end nearer the press moves to it, and the other end
   becomes the anchor; a drag goes on from there. *)
let a_shift_press_moves_the_nearer_end () =
  let r = letters ~backward:false in
  clicks ~shift:true r ~x:8 ~y:0 1;
  equal ~msg:"the far end, nearer" (option string) (Some "cdefghi") (copied r);
  clicks ~shift:true r ~x:0 ~y:0 1;
  equal ~msg:"the near end, nearer" (option string) (Some "abcdefghi")
    (copied r);
  presses ~shift:true r ~x:1 ~y:0 1;
  drag_to r ~x:5 ~y:0;
  release r ~x:5 ~y:0;
  equal ~msg:"dragged on from the press" (option string) (Some "fghi")
    (copied r)

(* On a tie the focus moves, whichever way the selection runs. *)
let a_shift_press_moves_the_focus_on_a_tie () =
  let r = letters ~backward:false in
  clicks ~shift:true r ~x:4 ~y:0 1;
  equal ~msg:"forward" (option string) (Some "cde") (copied r);
  let r = letters ~backward:true in
  clicks ~shift:true r ~x:4 ~y:0 1;
  equal ~msg:"backward" (option string) (Some "efg") (copied r)

(* The unit is kept: after a double click, a Shift+press adds words. *)
let a_shift_press_adds_words_after_a_double_click () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma delta"
      : Text.t);
  frame r;
  clicks r ~x:7 ~y:0 2;
  equal ~msg:"a word" (option string) (Some "beta") (copied r);
  clicks ~shift:true r ~x:19 ~y:0 1;
  equal ~msg:"to the word pressed" (option string) (Some "beta gamma delta")
    (copied r)

(* A Shift+press of two clicks or more sets the unit. *)
let a_shift_double_click_sets_the_unit () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma delta"
      : Text.t);
  frame r;
  select r (6, 0) (7, 0);
  equal ~msg:"selected" (option string) (Some "be") (copied r);
  presses ~shift:true r ~x:13 ~y:0 1;
  release r ~x:13 ~y:0;
  equal ~msg:"one click" (option string) (Some "beta gam") (copied r);
  clicks ~shift:true r ~x:13 ~y:0 2;
  equal ~msg:"two clicks" (option string) (Some "beta gamma") (copied r)

(* With no selection, a Shift+press is an ordinary press. *)
let a_shift_press_without_a_selection_is_ordinary () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "abcdefghij" : Text.t);
  frame r;
  presses ~shift:true r ~x:2 ~y:0 1;
  drag_to r ~x:4 ~y:0;
  release r ~x:4 ~y:0;
  equal ~msg:"from the press" (option string) (Some "cde") (copied r)

(* A Shift+press is held in the selection's region: one on the footer below
   a scroll box moves an end to the box's last row, the end nearer the cell
   it is held at. *)
let a_shift_press_is_held_in_the_region () =
  let r, _ = rows_in_a_box () in
  select r (0, 1) (2, 1);
  equal ~msg:"selected" (option string) (Some "row") (copied r);
  clicks ~shift:true r ~x:2 ~y:4 1;
  equal ~msg:"held at the box's last row" (option string)
    (Some "row-1\nrow-2\nrow") (copied r);
  select r (1, 3) (4, 3);
  equal ~msg:"on the last row" (option string) (Some "ow-3") (copied r);
  clicks ~shift:true r ~x:0 ~y:4 1;
  equal ~msg:"the start, nearer the cell held" (option string) (Some "row-3")
    (copied r)

(* A selection behind a modal is inert: a Shift+press in the modal is an
   ordinary press there. *)
let a_shift_press_over_an_inert_selection_is_ordinary () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:5 ~w:10 ~h:1 "behind" : Text.t);
  let top =
    Renderable.create ~parent:root ~style:(place ~x:0 ~y:0 ~w:10 ~h:1) ()
  in
  ignore (line top "modal" : Text.t);
  frame r;
  select r (0, 5) (2, 5);
  equal ~msg:"selected" (option string) (Some "beh") (copied r);
  Renderer.set_modal r (Some top);
  frame r;
  presses ~shift:true r ~x:1 ~y:0 1;
  drag_to r ~x:3 ~y:0;
  release r ~x:3 ~y:0;
  equal ~msg:"a new selection in the modal" (option string) (Some "oda")
    (copied r)

(* §3.5 Ctrl+press: an ordinary press, which starts a new selection. *)
let a_ctrl_press_is_ordinary () =
  let r = letters ~backward:false in
  presses ~ctrl:true r ~x:8 ~y:0 1;
  drag_to r ~x:9 ~y:0;
  release r ~x:9 ~y:0;
  equal ~msg:"a new selection" (option string) (Some "ij") (copied r)

(* ── §3.9 Highlight ── *)

(* The blank cells after a selected hard line break are filled to the node's
   right edge. *)
let the_fill_after_a_hard_break () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:2 "ab\ncd" : Text.t);
  frame r;
  select r (0, 0) (0, 1);
  paint r;
  is_true ~msg:"the text" (inverse r ~x:1 ~y:0);
  is_true ~msg:"after the break" (inverse r ~x:5 ~y:0);
  is_false ~msg:"beyond the node" (inverse r ~x:6 ~y:0);
  is_true ~msg:"the last grapheme" (inverse r ~x:0 ~y:1);
  is_false ~msg:"after the last row's selection" (inverse r ~x:3 ~y:1)

(* A soft wrap copies nothing and fills nothing. *)
let no_fill_after_a_soft_wrap () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "abc def"
      : Text.t);
  frame r;
  select r (0, 0) (1, 1);
  paint r;
  is_true ~msg:"the text" (inverse r ~x:2 ~y:0);
  is_false ~msg:"after the wrap" (inverse r ~x:5 ~y:0)

(* Law 2: a buffered text paints in its own buffer's coordinates, but the
   band and the texts' places stay in the region's: a buffered end
   highlights none of the text beside it, and a buffered text between the
   ends is highlighted whole. *)
let buffered_texts_are_highlighted_where_they_lie () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  let buffered text = Renderable.set_buffered (Text.node text) true in
  buffered (text_at root ~x:5 ~y:2 ~w:10 ~h:1 "0123456789");
  ignore (text_at root ~x:0 ~y:2 ~w:2 ~h:1 "pq" : Text.t);
  buffered (text_at root ~x:5 ~y:3 ~w:10 ~h:1 "abcdefghij");
  ignore (text_at root ~x:5 ~y:4 ~w:10 ~h:1 "klmnopqrst" : Text.t);
  frame r;
  select r (8, 2) (7, 4);
  equal ~msg:"copied" (option string) (Some "3456789\nabcdefghij\nklm")
    (copied r);
  paint r;
  is_false ~msg:"the text beside the buffered end" (inverse r ~x:0 ~y:2);
  is_true ~msg:"the buffered end" (inverse r ~x:8 ~y:2);
  is_true ~msg:"the buffered text between, from its start" (inverse r ~x:5 ~y:3);
  is_true ~msg:"to its end" (inverse r ~x:14 ~y:3);
  is_true ~msg:"the other end" (inverse r ~x:7 ~y:4);
  is_false ~msg:"after the other end" (inverse r ~x:8 ~y:4)

(* ── §3.10 Editors ── *)

let selected_input value ~anchor ~cursor =
  Editor.select ~anchor ~cursor (Editor.of_string value)

(* A press in an editor clears the view selection. *)
let a_press_in_an_editor_clears_the_view_selection () =
  let view =
    Mosaic.box ~flex_direction:Column
      [
        Mosaic.text "hello";
        Mosaic.input
          ~size:(Mosaic.size ~width:10 ~height:1)
          ~state:(Editor.of_string "typed")
          ~on_change:(fun _ -> ())
          ();
      ]
  in
  let r, _, _ = mount view in
  select r (0, 0) (2, 0);
  equal ~msg:"selected" (option string) (Some "hel") (copied r);
  press r ~x:1 ~y:1;
  release r ~x:1 ~y:1;
  is_none ~msg:"cleared" (copied r)

(* The view selection never includes an editor's text. *)
let the_view_selection_leaves_editors_out () =
  let view =
    Mosaic.box
      [
        Mosaic.text "ab";
        Mosaic.input
          ~size:(Mosaic.size ~width:3 ~height:1)
          ~state:(Editor.of_string "XYZ")
          ~on_change:(fun _ -> ())
          ();
        Mosaic.text "cd";
      ]
  in
  let r, _, _ = mount view in
  select r (0, 0) (6, 0);
  equal ~msg:"the editor's cells as spaces" (option string) (Some "ab   cd")
    (copied r)

(* §3.10 Copy: with no view selection, the copy is the focused editor's
   selection, which a view selection takes over. *)
let the_copy_falls_back_to_the_focused_editor () =
  let view =
    Mosaic.box ~flex_direction:Column
      [
        Mosaic.text "hello";
        Mosaic.input ~focused:true
          ~size:(Mosaic.size ~width:10 ~height:1)
          ~state:(selected_input "abcd" ~anchor:1 ~cursor:3)
          ~on_change:(fun _ -> ())
          ();
      ]
  in
  let r, _, _ = mount view in
  equal ~msg:"the editor's" (option string) (Some "bc") (copied r);
  select r (0, 0) (2, 0);
  equal ~msg:"the view's" (option string) (Some "hel") (copied r);
  Renderer.clear_selection r;
  equal ~msg:"the editor's again" (option string) (Some "bc") (copied r)

(* An editor drags its own selection between the carets the pointer hits,
   as a text field does, and proposes it. *)
let an_editor_drags_its_own_selection () =
  let proposed = ref None in
  let view =
    Mosaic.input
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~state:(Editor.of_string "hello world")
      ~on_change:(fun e -> proposed := Some e)
      ()
  in
  let r, _, _ = mount view in
  select r (0, 0) (5, 0);
  match !proposed with
  | Some editor ->
      equal ~msg:"between the carets"
        (option (pair int int))
        (Some (0, 5))
        (Editor.selection editor)
  | None -> fail "the drag proposed nothing"

(* Another button's press and release in the middle of an editor's drag
   leave the drag alone. *)
let an_editors_drag_survives_a_chorded_press () =
  let proposed = ref None in
  let view =
    Mosaic.input
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~state:(Editor.of_string "hello world")
      ~on_change:(fun e -> proposed := Some e)
      ()
  in
  let r, _, _ = mount view in
  press r ~x:0 ~y:0;
  drag_to r ~x:3 ~y:0;
  send r (Press Right) ~x:3 ~y:0;
  send r (Release (Some Right)) ~x:3 ~y:0;
  drag_to r ~x:5 ~y:0;
  release r ~x:5 ~y:0;
  match !proposed with
  | Some editor ->
      equal ~msg:"between the first press and the last drag"
        (option (pair int int))
        (Some (0, 5))
        (Editor.selection editor)
  | None -> fail "the drag proposed nothing"

(* The proposals of an editor of [state] mounted alone, the last first. *)
let mounted_editor ?(multiline = false) state =
  let proposed = ref [] in
  let on_change e = proposed := e :: !proposed in
  let size = Mosaic.size ~width:20 ~height:3 in
  let view =
    if multiline then Mosaic.textarea ~size ~state ~on_change ()
    else Mosaic.input ~size ~state ~on_change ()
  in
  let r, _, _ = mount view in
  (r, proposed)

let last_selection proposed =
  match !proposed with
  | editor :: _ -> (Editor.selection editor, Editor.cursor editor)
  | [] -> fail "nothing proposed"

(* §3.10: an editor's double click selects a word, with the shared units,
   and a drag after it extends by words either way. *)
let an_editor_selects_words () =
  let r, proposed = mounted_editor (Editor.of_string "alpha beta gamma") in
  presses r ~x:7 ~y:0 2;
  equal ~msg:"the word"
    (pair (option (pair int int)) int)
    (Some (6, 10), 10)
    (last_selection proposed);
  drag_to r ~x:13 ~y:0;
  equal ~msg:"forward"
    (pair (option (pair int int)) int)
    (Some (6, 16), 16)
    (last_selection proposed);
  drag_to r ~x:1 ~y:0;
  equal ~msg:"backward, the caret first"
    (pair (option (pair int int)) int)
    (Some (0, 10), 0)
    (last_selection proposed);
  release r ~x:1 ~y:0

(* A triple click selects the logical line without its terminator, and a
   drag after it extends by lines. *)
let an_editor_selects_lines () =
  let r, proposed =
    mounted_editor ~multiline:true (Editor.of_string "one two\nthree\nfour")
  in
  presses r ~x:1 ~y:0 3;
  equal ~msg:"the line"
    (pair (option (pair int int)) int)
    (Some (0, 7), 7)
    (last_selection proposed);
  drag_to r ~x:0 ~y:1;
  equal ~msg:"to the next line"
    (pair (option (pair int int)) int)
    (Some (0, 13), 13)
    (last_selection proposed);
  release r ~x:0 ~y:1

(* ── §3.6 Keys ── *)

(* "hello world" alone, on the first row. *)
let hello () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "hello world" : Text.t);
  frame r;
  r

(* §3.6 Home, Inert: the keys act on a selection that holds text and is not
   behind the modal node, and one behind it is still there once it closes. *)
let a_selection_is_live_while_it_holds_text_outside_a_modal () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:5 ~w:10 ~h:1 "behind" : Text.t);
  let top =
    Renderable.create ~parent:root ~style:(place ~x:0 ~y:0 ~w:10 ~h:1) ()
  in
  ignore (line top "modal" : Text.t);
  frame r;
  is_false ~msg:"no selection" (Renderer.selection_live r);
  press r ~x:0 ~y:5;
  is_false ~msg:"a click holds nothing" (Renderer.selection_live r);
  drag_to r ~x:2 ~y:5;
  is_true ~msg:"dragging" (Renderer.selection_live r);
  release r ~x:2 ~y:5;
  is_true ~msg:"at rest" (Renderer.selection_live r);
  Renderer.set_modal r (Some top);
  frame r;
  is_false ~msg:"behind the modal" (Renderer.selection_live r);
  Renderer.extend_selection r `Right;
  equal ~msg:"no extension behind the modal" (option string) (Some "beh")
    (copied r);
  Renderer.set_modal r None;
  frame r;
  is_true ~msg:"once the modal closes" (Renderer.selection_live r);
  Renderer.clear_selection r;
  is_false ~msg:"cleared" (Renderer.selection_live r)

(* §3.6 Extension: Shift with an arrow moves the focus by a grapheme, and the
   anchor's grapheme stays selected. *)
let extension_moves_the_focus_by_a_grapheme () =
  let r = hello () in
  select r (0, 0) (2, 0);
  Renderer.extend_selection r `Right;
  equal ~msg:"right" (option string) (Some "hell") (copied r);
  Renderer.extend_selection r `Left;
  Renderer.extend_selection r `Left;
  equal ~msg:"left" (option string) (Some "he") (copied r);
  Renderer.extend_selection r `Left;
  Renderer.extend_selection r `Left;
  equal ~msg:"onto the anchor" (option string) (Some "h") (copied r)

(* After a double click, a key extension drops the unit to the grapheme: the
   word collapses to its grapheme on the side away from the focus, which
   stays selected as a grapheme anchor's does. *)
let extension_after_a_word_keeps_its_first_grapheme () =
  let words () =
    let r = Renderer.create () in
    ignore
      (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "alpha beta gamma"
        : Text.t);
    frame r;
    r
  in
  let r = words () in
  clicks r ~x:7 ~y:0 2;
  equal ~msg:"the word" (option string) (Some "beta") (copied r);
  Renderer.extend_selection r `Left;
  equal ~msg:"a grapheme back" (option string) (Some "bet") (copied r);
  let r = words () in
  clicks r ~x:7 ~y:0 2;
  Renderer.extend_selection r `Row_start;
  equal ~msg:"to the row's start, the word's first grapheme kept"
    (option string) (Some "alpha b") (copied r);
  let r = words () in
  presses r ~x:7 ~y:0 2;
  drag_to r ~x:1 ~y:0;
  release r ~x:1 ~y:0;
  equal ~msg:"a backward word drag" (option string) (Some "alpha beta")
    (copied r);
  Renderer.extend_selection r `Right;
  equal ~msg:"a grapheme forward" (option string) (Some "lpha beta") (copied r);
  Renderer.extend_selection r `Row_end;
  equal ~msg:"past the anchor, its last grapheme kept" (option string)
    (Some "a gamma") (copied r)

(* Shift+Home and Shift+End move the focus to its row's ends. *)
let extension_moves_the_focus_to_its_rows_ends () =
  let r = hello () in
  select r (2, 0) (4, 0);
  Renderer.extend_selection r `Row_end;
  equal ~msg:"the row's end" (option string) (Some "llo world") (copied r);
  Renderer.extend_selection r `Row_start;
  equal ~msg:"the row's start, past the anchor" (option string) (Some "hel")
    (copied r);
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:6 ~h:3
       ~props:(fun content -> Text.Props.make ~content ~wrap:`Word ())
       "ab\ncd ef gh"
      : Text.t);
  frame r;
  select r (0, 0) (0, 1);
  Renderer.extend_selection r `Row_end;
  equal ~msg:"the end of a row that wraps" (option string) (Some "ab\ncd ef ")
    (copied r)

(* Shift+Up and Shift+Down move the focus to its column in the row above or
   below, in its text and from text to text. *)
let extension_moves_the_focus_by_a_row () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:3 "one\ntwo\nthree" : Text.t);
  frame r;
  select r (0, 0) (1, 0);
  Renderer.extend_selection r `Down;
  equal ~msg:"in the text" (option string) (Some "one\ntw") (copied r);
  Renderer.extend_selection r `Down;
  equal ~msg:"again" (option string) (Some "one\ntwo\nth") (copied r);
  Renderer.extend_selection r `Up;
  equal ~msg:"up" (option string) (Some "one\ntw") (copied r);
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  List.iter (fun s -> ignore (line col s : Text.t)) [ "foo"; "bar"; "baz" ];
  frame r;
  select r (0, 0) (1, 0);
  Renderer.extend_selection r `Down;
  equal ~msg:"to the text below" (option string) (Some "foo\nba") (copied r);
  Renderer.extend_selection r `Down;
  equal ~msg:"and the next" (option string) (Some "foo\nbar\nba") (copied r);
  Renderer.extend_selection r `Down;
  equal ~msg:"none below the last" (option string) (Some "foo\nbar\nba")
    (copied r);
  Renderer.extend_selection r `Up;
  equal ~msg:"back up" (option string) (Some "foo\nba") (copied r)

(* Past the edge of its text, a grapheme move reaches the next text in
   row-major order, at its start, or the one before, at its end. *)
let extension_crosses_to_the_next_text () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "foo" : Text.t);
  ignore (line col "bar" : Text.t);
  frame r;
  select r (0, 0) (2, 0);
  Renderer.extend_selection r `Right;
  equal ~msg:"at the next text's start" (option string) (Some "foo") (copied r);
  Renderer.extend_selection r `Right;
  equal ~msg:"its first grapheme" (option string) (Some "foo\nb") (copied r);
  Renderer.extend_selection r `Left;
  Renderer.extend_selection r `Left;
  Renderer.extend_selection r `Left;
  equal ~msg:"back in the text before" (option string) (Some "fo") (copied r)

(* Two texts of three rows side by side. *)
let columns () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:5 ~h:3 "L0\nL1\nL2" : Text.t);
  ignore (text_at root ~x:6 ~y:0 ~w:5 ~h:3 "R0\nR1\nR2" : Text.t);
  frame r;
  r

(* Past the edge of its text, a grapheme move goes to the boundary at the
   cell beside it in row-major order, on the same row of the text beside,
   not to that text's far end. *)
let extension_crosses_to_the_text_beside_by_a_grapheme () =
  let r = columns () in
  select r (6, 0) (8, 0);
  for _ = 1 to 4 do
    Renderer.extend_selection r `Left
  done;
  equal ~msg:"leftward, the row before" (option string) (Some "0    R")
    (copied r);
  let dragged = columns () in
  select dragged (6, 0) (1, 0);
  equal ~msg:"as a drag there" (option string) (copied dragged) (copied r);
  let r = columns () in
  select r (0, 2) (1, 2);
  Renderer.extend_selection r `Right;
  equal ~msg:"at the start of the row after" (option string) (Some "L2")
    (copied r);
  Renderer.extend_selection r `Right;
  equal ~msg:"rightward, its first grapheme" (option string) (Some "L2    R")
    (copied r);
  let dragged = columns () in
  select dragged (0, 2) (6, 2);
  equal ~msg:"as a drag there" (option string) (copied dragged) (copied r)

(* A focus left in a scroll box selected whole moves out of it, from its
   view: the box stays whole. *)
let extension_crosses_out_of_a_nested_box () =
  let r = nested_box () in
  select r (0, 0) (25, 3);
  let whole = "above\nin-0\nin-1\nin-2\nin-3\nin-4\nin-5" in
  equal ~msg:"the box at the far end" (option string) (Some whole) (copied r);
  Renderer.extend_selection r `Down;
  equal ~msg:"the row below" (option string)
    (Some (whole ^ "\nbelow"))
    (copied r)

(* A row move that takes the focus out of a scroll box's view scrolls the box
   a row, by its proposal, and the focus never leaves the box. *)
let a_row_move_past_the_view_scrolls_the_region () =
  let r, sb = rows_in_a_box () in
  select r (0, 1) (4, 3);
  Renderer.extend_selection r `Down;
  frame r;
  equal ~msg:"a row" int 1 (Scroll_box.scroll_top sb);
  equal ~msg:"the row below the view" (option string)
    (Some "row-1\nrow-2\nrow-3\nrow-4") (copied r);
  for _ = 1 to 8 do
    Renderer.extend_selection r `Down;
    frame r
  done;
  equal ~msg:"to the box's last row" int 6 (Scroll_box.scroll_top sb);
  equal ~msg:"never the footer" (option string)
    (Some "row-1\nrow-2\nrow-3\nrow-4\nrow-5\nrow-6\nrow-7\nrow-8\nrow-9")
    (copied r)

(* An extension selects by graphemes from then on: a Shift+press after it
   reaches the grapheme pressed, not its word. *)
let extension_selects_by_graphemes () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "alpha beta gamma" : Text.t);
  frame r;
  clicks r ~x:7 ~y:0 2;
  Renderer.extend_selection r `Right;
  equal ~msg:"a grapheme, not a word" (option string) (Some "beta ") (copied r);
  clicks ~shift:true r ~x:13 ~y:0 1;
  equal ~msg:"a Shift+press by graphemes" (option string) (Some "beta gam")
    (copied r)

(* While a gesture drags the selection, the pointer is its focus, which the
   keys leave alone. *)
let a_drag_keeps_its_focus_from_the_keys () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "alpha beta gamma" : Text.t);
  frame r;
  presses r ~x:7 ~y:0 2;
  drag_to r ~x:13 ~y:0;
  Renderer.extend_selection r `Right;
  equal ~msg:"dragging" (option string) (Some "beta gamma") (copied r);
  release r ~x:13 ~y:0;
  equal ~msg:"released" (option string) (Some "beta gamma") (copied r)

(* ── §3.7 Changes ── *)

let changes r =
  List.map
    (function
      | Renderer.Settled text -> "settled " ^ text
      | Renderer.Cleared -> "cleared")
    (Renderer.take_selection_changes r)

(* A release settles the selection with its text; the motion before it
   queues nothing, and a take forgets what it took. *)
let a_release_settles_the_selection () =
  let r = hello () in
  press r ~x:0 ~y:0;
  drag_to r ~x:4 ~y:0;
  equal ~msg:"dragging" (list string) [] (changes r);
  release r ~x:4 ~y:0;
  equal ~msg:"released" (list string) [ "settled hello" ] (changes r);
  equal ~msg:"taken" (list string) [] (changes r)

(* A click selects nothing and settles nothing, but a selection that held
   text and that a press replaces with nothing is cleared. *)
let a_click_settles_nothing () =
  let r = hello () in
  select r (0, 0) (4, 0);
  ignore (changes r : string list);
  press r ~x:7 ~y:0;
  equal ~msg:"the press clears" (list string) [ "cleared" ] (changes r);
  release r ~x:7 ~y:0;
  equal ~msg:"the click settles nothing" (list string) [] (changes r);
  is_none ~msg:"and selects nothing" (copied r);
  press r ~x:7 ~y:0;
  release r ~x:7 ~y:0;
  equal ~msg:"nor clears again" (list string) [] (changes r)

(* A double click settles its word at its release, and the third click of a
   series replaces the word with its line, with no clear between. *)
let a_double_click_settles_at_its_release () =
  let r = hello () in
  presses r ~x:7 ~y:0 2;
  equal ~msg:"held" (list string) [] (changes r);
  release r ~x:7 ~y:0;
  equal ~msg:"released" (list string) [ "settled world" ] (changes r);
  let r = hello () in
  presses r ~x:7 ~y:0 3;
  equal ~msg:"the third press" (list string) [ "settled world" ] (changes r);
  release r ~x:7 ~y:0;
  equal ~msg:"the line" (list string) [ "settled hello world" ] (changes r)

(* §3.5 Release: a press of the held button means its release was lost: the
   gesture settles, then the new press clears what it held. A focus loss
   settles it too. *)
let a_lost_release_settles () =
  let r = hello () in
  press r ~x:0 ~y:0;
  drag_to r ~x:4 ~y:0;
  press r ~x:8 ~y:0;
  equal ~msg:"a new press" (list string)
    [ "settled hello"; "cleared" ]
    (changes r);
  release r ~x:8 ~y:0;
  press r ~x:0 ~y:0;
  drag_to r ~x:1 ~y:0;
  Renderer.pointer_lost r;
  equal ~msg:"a focus loss" (list string) [ "settled he" ] (changes r)

(* A clear, during a gesture or at rest, is heard once, and the gesture it
   ends settles nothing. *)
let a_clear_is_heard_once () =
  let r = hello () in
  press r ~x:0 ~y:0;
  drag_to r ~x:4 ~y:0;
  Renderer.clear_selection r;
  equal ~msg:"during a drag" (list string) [ "cleared" ] (changes r);
  drag_to r ~x:6 ~y:0;
  release r ~x:6 ~y:0;
  equal ~msg:"the gesture settles nothing" (list string) [] (changes r);
  is_none ~msg:"and selects nothing" (copied r);
  select r (0, 0) (4, 0);
  ignore (changes r : string list);
  Renderer.clear_selection r;
  Renderer.clear_selection r;
  equal ~msg:"at rest" (list string) [ "cleared" ] (changes r)

(* A Shift+press that moves an end settles the new selection, which it does
   not clear first. *)
let a_shift_press_settles_the_moved_selection () =
  let r = letters ~backward:false in
  ignore (changes r : string list);
  clicks ~shift:true r ~x:8 ~y:0 1;
  equal ~msg:"settled" (list string) [ "settled cdefghi" ] (changes r)

(* A keyboard extension settles nothing; one that leaves the selection no
   text clears it. *)
let an_extension_settles_nothing () =
  let r = hello () in
  select r (0, 0) (2, 0);
  ignore (changes r : string list);
  Renderer.extend_selection r `Right;
  equal ~msg:"extended" (option string) (Some "hell") (copied r);
  equal ~msg:"nothing queued" (list string) [] (changes r);
  (* A press past the end of a row anchors at a boundary, which holds no
     text. *)
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "abc" : Text.t);
  ignore (line col "def" : Text.t);
  frame r;
  select r (5, 0) (0, 1);
  equal ~msg:"from a boundary" (option string) (Some "d") (copied r);
  ignore (changes r : string list);
  Renderer.extend_selection r `Left;
  is_none ~msg:"no text left" (copied r);
  is_false ~msg:"cleared" (Renderer.selection_live r);
  equal ~msg:"heard" (list string) [ "cleared" ] (changes r);
  Renderer.extend_selection r `Right;
  is_none ~msg:"gone" (copied r)

(* A selection of line breaks alone holds them all: their fill is
   highlighted, so they are copied (Law 2). *)
let line_breaks_alone_are_held () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:10 ~h:4 "ab\n\n\n" : Text.t);
  frame r;
  press r ~x:5 ~y:0;
  drag_to r ~x:0 ~y:1;
  equal ~msg:"one copies itself" (option string) (Some "\n") (copied r);
  is_true ~msg:"is live" (Renderer.selection_live r);
  release r ~x:0 ~y:1;
  equal ~msg:"and settles" (list string) [ "settled \n" ] (changes r);
  press r ~x:5 ~y:0;
  drag_to r ~x:0 ~y:3;
  equal ~msg:"three copy all three" (option string) (Some "\n\n\n") (copied r);
  is_true ~msg:"which is live" (Renderer.selection_live r);
  release r ~x:0 ~y:3;
  equal ~msg:"the first cleared, then settles" (list string)
    [ "cleared"; "settled \n\n\n" ]
    (changes r)

(* Law 4: a press on unselectable content changes nothing, and queues
   nothing. *)
let a_press_on_unselectable_content_queues_nothing () =
  let r = Renderer.create () in
  let root = Renderer.root r in
  ignore (text_at root ~x:0 ~y:0 ~w:20 ~h:1 "hello world" : Text.t);
  let fenced = text_at root ~x:0 ~y:2 ~w:20 ~h:1 "fenced" in
  Renderable.set_selectable (Text.node fenced) (Some false);
  frame r;
  select r (0, 0) (4, 0);
  ignore (changes r : string list);
  press r ~x:1 ~y:2;
  drag_to r ~x:3 ~y:2;
  release r ~x:3 ~y:2;
  equal ~msg:"nothing" (list string) [] (changes r);
  equal ~msg:"kept" (option string) (Some "hello") (copied r)

(* ── §3.8 Change under a selection ── *)

(* "alpha beta gamma" alone on the first row, its "beta" selected at rest,
   with no change left to take. *)
let beta () =
  let r = Renderer.create () in
  let t = text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma" in
  frame r;
  select r (6, 0) (9, 0);
  ignore (changes r : string list);
  (r, t)

(* Whether the frame drawn last shows the cells from [x0] to [x1] of row
   [y] in reverse, and no other cell of the row. *)
let highlighted r ~y x0 x1 =
  paint r;
  List.for_all
    (fun x -> Bool.equal (inverse r ~x ~y) (x >= x0 && x <= x1))
    (List.init width Fun.id)

(* §3.8 Scrolling: a scroll box that follows its bottom as rows are appended
   keeps the selection on the rows it held, out of its view. *)
let following_and_appending_keep_the_selection () =
  let r = Renderer.create () in
  let sb =
    Scroll_box.create ~parent:(Renderer.root r)
      ~style:(place ~x:0 ~y:0 ~w:20 ~h:4)
      (Scroll_box.Props.make ~show_scrollbars:false
         ~scroll:(Scroll.follow `Bottom (Scroll.at 0))
         ())
  in
  Scroll_box.set_on_scroll sb (Some (Scroll_box.set_scroll sb));
  let row i =
    ignore (line (Scroll_box.node sb) (Printf.sprintf "row-%d" i) : Text.t)
  in
  for i = 0 to 3 do
    row i
  done;
  frame r;
  select r (0, 1) (4, 2);
  ignore (changes r : string list);
  for i = 4 to 9 do
    row i;
    frame r
  done;
  equal ~msg:"followed" int 6 (Scroll_box.scroll_top sb);
  equal ~msg:"the rows it held" (option string) (Some "row-1\nrow-2") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

(* A resize rewraps the text, and the selection stays on the word it
   held. *)
let a_resize_keeps_the_selection () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore
    (Text.create ~parent:col
       (Text.Props.make ~content:"alpha beta gamma delta" ~wrap:`Word ())
      : Text.t);
  frame r;
  clicks r ~x:12 ~y:0 2;
  equal ~msg:"before" (option string) (Some "gamma") (copied r);
  Renderer.render_frame r ~width:8 ~height ~dt:0.;
  equal ~msg:"after" (option string) (Some "gamma") (copied r);
  is_true ~msg:"highlighted where it now lies" (inverse r ~x:0 ~y:2);
  is_false ~msg:"and not where it was" (inverse r ~x:0 ~y:1)

(* §3.8 Edits: an edit before the ends moves them by the change in length;
   one after them leaves them. *)
let an_edit_before_the_ends_moves_them () =
  let r, t = beta () in
  Text.set_content t "and alpha beta gamma";
  equal ~msg:"an insertion before" (option string) (Some "beta") (copied r);
  is_true ~msg:"highlighted where it now lies" (highlighted r ~y:0 10 13);
  Text.set_content t "alpha beta gamma";
  equal ~msg:"a deletion before" (option string) (Some "beta") (copied r);
  Text.set_content t "alpha beta gamma delta";
  equal ~msg:"an insertion after" (option string) (Some "beta") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

(* The changed span counts clusters, whatever their bytes: a combining mark
   that joins the cluster before an end makes that cluster part of the span,
   so text inserted after it stays out of a selection that starts there; wide
   clusters inserted before the ends move them by their count. *)
let an_edit_counts_clusters () =
  let r = Renderer.create () in
  let t = text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "cafe ok" in
  frame r;
  select r (4, 0) (6, 0);
  equal ~msg:"before" (option string) (Some " ok") (copied r);
  Text.set_content t "cafe\u{0301}! ok";
  equal ~msg:"a mark joins a cluster" (option string) (Some " ok") (copied r);
  Text.set_content t "\u{65E5}\u{672C} cafe\u{0301}! ok";
  equal ~msg:"wide clusters inserted" (option string) (Some " ok") (copied r)

(* An end strictly inside the changed span moves to its start. *)
let an_end_inside_the_edit_moves_to_its_start () =
  let r = Renderer.create () in
  let t = text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma" in
  frame r;
  select r (7, 0) (15, 0);
  equal ~msg:"before" (option string) (Some "eta gamma") (copied r);
  Text.set_content t "alpha BETA gamma";
  equal ~msg:"from the span's start" (option string) (Some "BETA gamma")
    (copied r)

(* An end at the old end of the changed span moves by the change in
   length, and one at its start stays. *)
let an_end_at_the_edits_end_moves_with_it () =
  let r, t = beta () in
  Text.set_content t "alpha BETA! gamma";
  equal ~msg:"the span replaced" (option string) (Some "BETA!") (copied r)

(* An insertion at an end leaves the end in place: text inserted at the
   selection's start falls inside it, and text at its end outside. *)
let an_insertion_at_an_end_leaves_it () =
  let r, t = beta () in
  Text.set_content t "alpha betas gamma";
  equal ~msg:"at the end" (option string) (Some "beta") (copied r);
  Text.set_content t "alpha Xbetas gamma";
  equal ~msg:"at the start" (option string) (Some "Xbeta") (copied r)

(* Law 1: concealed markdown rewrites the text before an end when a [**]
   closes, and the end stays on the word it held. *)
let a_streaming_paragraph_closes_bold_before_an_end () =
  let r = Renderer.create () in
  let md =
    Markdown.create ~parent:(Renderer.root r)
      (Markdown.Props.make ~content:"a **b c" ~streaming:true ())
  in
  frame r;
  clicks r ~x:6 ~y:0 2;
  equal ~msg:"before" (option string) (Some "c") (copied r);
  Markdown.set_content md "a **b** c";
  frame r;
  equal ~msg:"the bold closed" (option string) (Some "c") (copied r);
  is_true ~msg:"highlighted where it now lies" (highlighted r ~y:0 4 4)

(* The selection clears when its ends meet: an edit removes all it held. *)
let the_selection_clears_when_its_ends_meet () =
  let r, t = beta () in
  Text.set_content t "alpha  gamma";
  is_none ~msg:"nothing held" (copied r);
  is_false ~msg:"not live" (Renderer.selection_live r);
  equal ~msg:"heard" (list string) [ "cleared" ] (changes r);
  Text.set_content t "alpha beta gamma";
  is_none ~msg:"gone" (copied r)

(* A gesture keeps its anchor through an edit, even one that leaves it no
   text for a while, and settles the text the anchor names. *)
let an_edit_during_a_drag_follows_its_anchor () =
  let r = Renderer.create () in
  let t = text_at (Renderer.root r) ~x:0 ~y:0 ~w:30 ~h:1 "alpha beta gamma" in
  frame r;
  press r ~x:6 ~y:0;
  drag_to r ~x:9 ~y:0;
  Text.set_content t "and alpha beta gamma";
  frame r;
  drag_to r ~x:13 ~y:0;
  release r ~x:13 ~y:0;
  equal ~msg:"settled" (list string) [ "settled beta" ] (changes r);
  press r ~x:10 ~y:0;
  ignore (changes r : string list);
  drag_to r ~x:11 ~y:0;
  Text.set_content t "and alpha ";
  frame r;
  is_none ~msg:"no text for a while" (copied r);
  Text.set_content t "and alpha beta gamma";
  frame r;
  drag_to r ~x:13 ~y:0;
  release r ~x:13 ~y:0;
  equal ~msg:"settled again" (list string) [ "settled beta" ] (changes r)

(* §3.8 Unmount, hide: an end whose node is hidden, or below a hidden node,
   clears the selection, which showing the node again does not bring back.
   A node hidden between the ends, or outside them, leaves it. *)
let an_ends_node_hidden_clears_the_selection () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "zero" : Text.t);
  let one = line col "one" in
  let two = line col "two" in
  let three = line col "three" in
  frame r;
  let reselect () =
    select r (1, 1) (2, 3);
    ignore (changes r : string list)
  in
  let cleared msg =
    is_none ~msg (copied r);
    equal ~msg:(msg ^ ": heard") (list string) [ "cleared" ] (changes r);
    paint r;
    for y = 0 to 3 do
      for x = 0 to 5 do
        is_false
          ~msg:(Printf.sprintf "%s: no highlight at %d,%d" msg x y)
          (inverse r ~x ~y)
      done
    done
  in
  reselect ();
  equal ~msg:"selected" (option string) (Some "ne\ntwo\nthr") (copied r);
  Renderable.set_visible (Text.node one) false;
  cleared "the anchor's node hidden";
  Renderable.set_visible (Text.node one) true;
  frame r;
  is_none ~msg:"shown again" (copied r);
  reselect ();
  Renderable.set_visible (Text.node three) false;
  cleared "the focus's node hidden";
  Renderable.set_visible (Text.node three) true;
  frame r;
  reselect ();
  Renderable.set_visible col false;
  cleared "an ancestor hidden";
  Renderable.set_visible col true;
  frame r;
  reselect ();
  Renderable.set_visible (Text.node two) false;
  frame r;
  equal ~msg:"a node between hidden" (option string) (Some "ne\nthr") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

(* An end whose node is detached, destroyed or remounted clears the
   selection; a render that keeps the node keeps it. *)
let an_ends_node_unmounted_clears_the_selection () =
  let r = Renderer.create () in
  let node =
    Text.node (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "hello world")
  in
  let unmounted msg unmount =
    if Option.is_none (Renderable.parent node) then
      Renderable.attach ~parent:(Renderer.root r) node;
    frame r;
    select r (0, 0) (4, 0);
    ignore (changes r : string list);
    unmount node;
    is_none ~msg (copied r);
    equal ~msg:(msg ^ ": heard") (list string) [ "cleared" ] (changes r)
  in
  unmounted "detached" Renderable.detach;
  unmounted "detached among others" (fun node -> Renderable.detach_all [ node ]);
  unmounted "destroyed" Renderable.destroy;
  let view key = Mosaic.box [ Mosaic.text ~key "hello world" ] in
  let r, reconciler, _ = mount (view "a") in
  select r (0, 0) (4, 0);
  ignore (changes r : string list);
  Mosaic.Reconciler.render reconciler ~viewport_width:width (view "a");
  frame r;
  equal ~msg:"kept" (option string) (Some "hello") (copied r);
  Mosaic.Reconciler.render reconciler ~viewport_width:width (view "b");
  frame r;
  is_none ~msg:"remounted" (copied r);
  equal ~msg:"remounted: heard" (list string) [ "cleared" ] (changes r)

(* §3.5 Release: an end's unmount during a drag ends its gesture: its later
   motion selects nothing and its release settles nothing. *)
let an_ends_unmount_ends_its_drag () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  let first = line col "hello" in
  ignore (line col "world" : Text.t);
  frame r;
  press r ~x:0 ~y:0;
  drag_to r ~x:2 ~y:1;
  Renderable.destroy_recursively (Text.node first);
  equal ~msg:"heard" (list string) [ "cleared" ] (changes r);
  frame r;
  drag_to r ~x:4 ~y:0;
  release r ~x:4 ~y:0;
  is_none ~msg:"nothing selected" (copied r);
  equal ~msg:"nothing settled" (list string) [] (changes r)

(* §3.8 Markdown: list, table and block quote blocks update in place while
   streaming, so an end in one survives a chunk. *)
let an_end_in_a_streaming_list_survives_a_chunk () =
  let r = Renderer.create () in
  let md =
    Markdown.create ~parent:(Renderer.root r)
      (Markdown.Props.make ~content:"- one\n- two" ~streaming:true ())
  in
  frame r;
  clicks r ~x:2 ~y:1 2;
  equal ~msg:"before" (option string) (Some "two") (copied r);
  ignore (changes r : string list);
  Markdown.set_content md "- one\n- two\n- three";
  frame r;
  equal ~msg:"an item added" (option string) (Some "two") (copied r);
  Markdown.set_content md "- one\n- two\n- three\n\n  more";
  frame r;
  equal ~msg:"the list loosened" (option string) (Some "two") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

let an_end_in_a_streaming_table_survives_a_chunk () =
  let r = Renderer.create () in
  let table = "| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |\n" in
  let md =
    Markdown.create ~parent:(Renderer.root r)
      (Markdown.Props.make ~content:table ~streaming:true ())
  in
  frame r;
  clicks r ~x:1 ~y:3 2;
  equal ~msg:"before" (option string) (Some "1") (copied r);
  ignore (changes r : string list);
  Markdown.set_content md (table ^ "| 5 | 6 |\n");
  frame r;
  equal ~msg:"a row added" (option string) (Some "1") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

let an_end_in_a_streaming_quote_survives_a_chunk () =
  let r = Renderer.create () in
  let quote = "> alpha\n>\n> beta" in
  let md =
    Markdown.create ~parent:(Renderer.root r)
      (Markdown.Props.make ~content:quote ~streaming:true ())
  in
  frame r;
  clicks r ~x:2 ~y:0 2;
  equal ~msg:"before" (option string) (Some "alpha") (copied r);
  ignore (changes r : string list);
  Markdown.set_content md (quote ^ " gamma\n>\n> delta");
  frame r;
  equal ~msg:"a paragraph added" (option string) (Some "alpha") (copied r);
  equal ~msg:"nothing heard" (list string) [] (changes r)

(* A copy lays out a text that no paint reached at the width its layout
   gives it, as a paint would show it: here two wrapped texts side by side
   below a nested box's view, which copy row by row. *)
let a_copy_lays_out_a_text_no_paint_reached () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "before" : Text.t);
  let sb = scroll_box col ~w:20 ~h:2 in
  ignore (line (Scroll_box.node sb) "top" : Text.t);
  ignore (line (Scroll_box.node sb) "mid" : Text.t);
  let pair =
    Renderable.create ~parent:(Scroll_box.node sb)
      ~style:
        (Toffee.Style.default
        |> Toffee.Style.set_flex_direction Row
        |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.))
      ()
  in
  let wrapped content =
    ignore
      (Text.create ~parent:pair
         ~style:
           (Toffee.Style.default
           |> Toffee.Style.set_width (Toffee.Style.Dimension.length 3.)
           |> Toffee.Style.set_flex_shrink 0.)
         (Text.Props.make ~content ~wrap:`Char ())
        : Text.t)
  in
  wrapped "aaabbbcc";
  wrapped "dddeeeff";
  ignore (line col "after" : Text.t);
  frame r;
  select r (0, 0) (4, 3);
  equal ~msg:"row by row" (option string)
    (Some "before\ntop\nmid\naaaddd\nbbbeee\ncc ff\nafter") (copied r)

(* ── Cells to source (RFC 0003 §2.4) ── *)

(* A drag takes both end cells' coverage: the space under the pointer is in. *)
let a_drag_takes_both_end_cells () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "hello world" : Text.t);
  frame r;
  select r (0, 0) (5, 0);
  equal ~msg:"through the space" (option string) (Some "hello ") (copied r)

(* A drag that leaves a text below or above selects to its end or start. *)
let a_drag_past_the_rows_selects_to_the_end () =
  let r = Renderer.create () in
  ignore (text_at (Renderer.root r) ~x:0 ~y:2 ~w:20 ~h:2 "ab\ncd" : Text.t);
  frame r;
  select r (1, 2) (0, 6);
  equal ~msg:"below" (option string) (Some "b\ncd") (copied r);
  select r (0, 3) (5, 0);
  equal ~msg:"above" (option string) (Some "ab\nc") (copied r)

(* A truncated line's ellipsis stands for its hidden suffix alone: the next
   line starts at its own source. *)
let a_truncated_line_leaves_the_next_line_whole () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:3 ~h:2
       ~props:(fun content -> Text.Props.make ~content ~truncate:true ())
       "abcdef\nXY"
      : Text.t);
  frame r;
  select r (0, 1) (2, 1);
  equal ~msg:"the second line" (option string) (Some "XY") (copied r)

(* A zero-width cluster between two cells is copied with them. *)
let a_zero_width_cluster_is_copied () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:20 ~h:1 "a\226\128\139b" : Text.t);
  frame r;
  select r (0, 0) (1, 0);
  equal ~msg:"with the space" (option string) (Some "a\226\128\139b") (copied r)

(* A click on an ellipsis selects nothing: a click never selects hidden
   text. *)
let a_click_on_an_ellipsis_selects_nothing () =
  let r = Renderer.create () in
  ignore
    (text_at (Renderer.root r) ~x:0 ~y:0 ~w:3 ~h:1
       ~props:(fun content -> Text.Props.make ~content ~truncate:true ())
       "abcdef"
      : Text.t);
  frame r;
  press r ~x:2 ~y:0;
  release r ~x:2 ~y:0;
  equal ~msg:"nothing" (option string) None (copied r)

let () =
  run "mosaic.selection"
    [
      group "Cells to source"
        [
          test "a drag takes both end cells" a_drag_takes_both_end_cells;
          test "a drag past the rows selects to the end"
            a_drag_past_the_rows_selects_to_the_end;
          test "a truncated line leaves the next line whole"
            a_truncated_line_leaves_the_next_line_whole;
          test "a zero-width cluster is copied" a_zero_width_cluster_is_copied;
          test "a click on an ellipsis selects nothing"
            a_click_on_an_ellipsis_selects_nothing;
        ];
      group "Model"
        [
          test "a press in a margin starts at the nearest text"
            a_press_in_a_margin_starts_at_the_nearest_text;
          test "a press below the last text starts at its end"
            a_press_below_the_last_text_starts_at_its_end;
          test "a press with no text in its region clears"
            a_press_with_no_text_clears;
          test "a press beside a nested box resolves past its texts"
            a_press_beside_a_nested_box_resolves_past_its_texts;
          test "the ends name text through a rewrap"
            the_ends_name_text_through_a_rewrap;
          test "the release keeps the band where it was"
            the_release_keeps_the_band_where_it_was;
        ];
      group "Region and band"
        [
          test "a drag out of a scroll box downward stays in it"
            a_drag_out_of_a_scroll_box_downward_stays_in_it;
          test "a drag out of a scroll box sideways stays in it"
            a_drag_out_of_a_scroll_box_sideways_stays_in_it;
          test "a selection at rest stays under the wheel"
            a_selection_at_rest_stays_under_the_wheel;
          test "hidden text takes no part" hidden_text_takes_no_part;
          test "a selection leaves the top layer out"
            a_selection_leaves_the_top_layer_out;
          test "a selection in the top layer stays in it"
            a_selection_in_the_top_layer_stays_in_it;
          test "a nested box covered is selected whole"
            a_nested_box_covered_is_selected_whole;
          test "a nested box partly covered is left out"
            a_nested_box_partly_covered_is_left_out;
          test "a nested box covered is highlighted"
            a_nested_box_covered_is_highlighted;
        ];
      group "Copy"
        [
          test "stacked texts copy on their own lines"
            stacked_texts_copy_on_their_own_lines;
          test "side-by-side text copies row by row"
            side_by_side_text_copies_row_by_row;
          test "paragraphs copy with a blank line"
            paragraphs_copy_with_a_blank_line;
          test "a bordered box between paragraphs"
            a_bordered_box_between_paragraphs;
          test "the markdown table copies row by row"
            the_markdown_table_copies_row_by_row;
          test "an ellipsis copies what it hides"
            an_ellipsis_copies_what_it_hides;
          test "a soft wrap adds nothing" a_soft_wrap_adds_nothing;
          test "a fragment drops a terminator at either end"
            a_fragment_drops_a_terminator_at_either_end;
          test "trailing whitespace is copied" trailing_whitespace_is_copied;
          test "a selection across unselectable text leaves it out"
            a_selection_across_unselectable_text_leaves_it_out;
        ];
      group "Selectability"
        [
          test "an absent selectable keeps a button's default"
            an_absent_selectable_keeps_a_buttons_default;
          test "an absent selectable keeps a slider's default"
            an_absent_selectable_keeps_a_sliders_default;
          test "an absent selectable keeps a user kind's declaration"
            an_absent_selectable_keeps_a_user_kinds_declaration;
          test "a press on unselectable content changes nothing"
            a_press_on_unselectable_content_changes_nothing;
          test "a selectable text inside an unselectable box"
            a_selectable_text_inside_an_unselectable_box;
          test "a line number's child follows its ancestors"
            a_line_numbers_child_follows_its_ancestors;
        ];
      group "Pointer gestures"
        [
          test "a drag back to the pressed cell selects its grapheme"
            a_drag_back_to_the_pressed_cell_selects_its_grapheme;
          test "the band stops at the text beside its end"
            the_band_stops_at_the_text_beside_its_end;
          test "a double click selects a word" a_double_click_selects_a_word;
          test "a double click selects at the press"
            a_double_click_selects_at_the_press;
          test "a double click past a row's end" a_double_click_past_a_rows_end;
          test "a triple click selects the logical line"
            a_triple_click_selects_the_logical_line;
          test "a triple click in a margin selects the nearest line"
            a_triple_click_in_a_margin_selects_the_nearest_line;
          test "a drag after a double click extends by words"
            a_drag_after_a_double_click_extends_by_words;
          test "the band reaches the unit under the pointer"
            the_band_reaches_the_unit_under_the_pointer;
          test "a word drag selects a nested box once covered"
            a_word_drag_selects_a_nested_box_once_covered;
          test "a line drag over a nested box settles where it was"
            a_line_drag_over_a_nested_box_settles_where_it_was;
          test "a line drag into a gap settles where it was"
            a_line_drag_into_a_gap_settles_where_it_was;
          test "a drag after a triple click extends by lines"
            a_drag_after_a_triple_click_extends_by_lines;
          test "the unit under the pointer follows a scroll"
            the_unit_under_the_pointer_follows_a_scroll;
          test "a shift press moves the nearer end"
            a_shift_press_moves_the_nearer_end;
          test "a shift press moves the focus on a tie"
            a_shift_press_moves_the_focus_on_a_tie;
          test "a shift press adds words after a double click"
            a_shift_press_adds_words_after_a_double_click;
          test "a shift double click sets the unit"
            a_shift_double_click_sets_the_unit;
          test "a shift press without a selection is ordinary"
            a_shift_press_without_a_selection_is_ordinary;
          test "a shift press is held in the region"
            a_shift_press_is_held_in_the_region;
          test "a shift press over an inert selection is ordinary"
            a_shift_press_over_an_inert_selection_is_ordinary;
          test "a ctrl press is ordinary" a_ctrl_press_is_ordinary;
        ];
      group "Highlight"
        [
          test "the fill after a hard break" the_fill_after_a_hard_break;
          test "no fill after a soft wrap" no_fill_after_a_soft_wrap;
          test "buffered texts are highlighted where they lie"
            buffered_texts_are_highlighted_where_they_lie;
        ];
      group "Editors"
        [
          test "a press in an editor clears the view selection"
            a_press_in_an_editor_clears_the_view_selection;
          test "the view selection leaves editors out"
            the_view_selection_leaves_editors_out;
          test "the copy falls back to the focused editor"
            the_copy_falls_back_to_the_focused_editor;
          test "an editor drags its own selection"
            an_editor_drags_its_own_selection;
          test "an editor's drag survives a chorded press"
            an_editors_drag_survives_a_chorded_press;
          test "an editor selects words" an_editor_selects_words;
          test "an editor selects lines" an_editor_selects_lines;
        ];
      group "Keys"
        [
          test "a selection is live while it holds text outside a modal"
            a_selection_is_live_while_it_holds_text_outside_a_modal;
          test "extension moves the focus by a grapheme"
            extension_moves_the_focus_by_a_grapheme;
          test "an extension after a word keeps its first grapheme"
            extension_after_a_word_keeps_its_first_grapheme;
          test "extension moves the focus to its row's ends"
            extension_moves_the_focus_to_its_rows_ends;
          test "extension moves the focus by a row"
            extension_moves_the_focus_by_a_row;
          test "extension crosses to the next text"
            extension_crosses_to_the_next_text;
          test "extension crosses to the text beside by a grapheme"
            extension_crosses_to_the_text_beside_by_a_grapheme;
          test "extension crosses out of a nested box"
            extension_crosses_out_of_a_nested_box;
          test "a row move past the view scrolls the region"
            a_row_move_past_the_view_scrolls_the_region;
          test "extension selects by graphemes" extension_selects_by_graphemes;
          test "a drag keeps its focus from the keys"
            a_drag_keeps_its_focus_from_the_keys;
        ];
      group "Changes"
        [
          test "a release settles the selection" a_release_settles_the_selection;
          test "a click settles nothing" a_click_settles_nothing;
          test "a double click settles at its release"
            a_double_click_settles_at_its_release;
          test "a lost release settles" a_lost_release_settles;
          test "a clear is heard once" a_clear_is_heard_once;
          test "a shift press settles the moved selection"
            a_shift_press_settles_the_moved_selection;
          test "an extension settles nothing" an_extension_settles_nothing;
          test "line breaks alone are held" line_breaks_alone_are_held;
          test "a press on unselectable content queues nothing"
            a_press_on_unselectable_content_queues_nothing;
        ];
      group "Change under a selection"
        [
          test "following and appending keep the selection"
            following_and_appending_keep_the_selection;
          test "a resize keeps the selection" a_resize_keeps_the_selection;
          test "an edit before the ends moves them"
            an_edit_before_the_ends_moves_them;
          test "an edit counts clusters" an_edit_counts_clusters;
          test "an end inside the edit moves to its start"
            an_end_inside_the_edit_moves_to_its_start;
          test "an end at the edit's end moves with it"
            an_end_at_the_edits_end_moves_with_it;
          test "an insertion at an end leaves it"
            an_insertion_at_an_end_leaves_it;
          test "a streaming paragraph closes bold before an end"
            a_streaming_paragraph_closes_bold_before_an_end;
          test "the selection clears when its ends meet"
            the_selection_clears_when_its_ends_meet;
          test "an edit during a drag follows its anchor"
            an_edit_during_a_drag_follows_its_anchor;
          test "an end's node hidden clears the selection"
            an_ends_node_hidden_clears_the_selection;
          test "an end's node unmounted clears the selection"
            an_ends_node_unmounted_clears_the_selection;
          test "an end's unmount ends its drag" an_ends_unmount_ends_its_drag;
          test "an end in a streaming list survives a chunk"
            an_end_in_a_streaming_list_survives_a_chunk;
          test "an end in a streaming table survives a chunk"
            an_end_in_a_streaming_table_survives_a_chunk;
          test "an end in a streaming quote survives a chunk"
            an_end_in_a_streaming_quote_survives_a_chunk;
          test "a copy lays out a text no paint reached"
            a_copy_lays_out_a_text_no_paint_reached;
        ];
    ]
