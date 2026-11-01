(* The view's text selection (RFC 0006): its model, region and band (§3.1,
   §3.2), copy (§3.3), selectability (§3.4), highlight (§3.9) and the editors'
   own selection (§3.10). *)

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

(* An end whose node is hidden names no visible text: the selection shows
   and copies nothing. *)
let an_ends_node_hidden_shows_and_copies_nothing () =
  let r = Renderer.create () in
  let col = column (Renderer.root r) in
  ignore (line col "zero" : Text.t);
  let one = line col "one" in
  ignore (line col "two" : Text.t);
  let three = line col "three" in
  frame r;
  let nothing_shown msg =
    is_none ~msg (copied r);
    paint r;
    for y = 0 to 3 do
      for x = 0 to 5 do
        is_false
          ~msg:(Printf.sprintf "%s: no highlight at %d,%d" msg x y)
          (inverse r ~x ~y)
      done
    done
  in
  select r (1, 1) (2, 3);
  equal ~msg:"selected" (option string) (Some "ne\ntwo\nthr") (copied r);
  Renderable.set_visible (Text.node one) false;
  nothing_shown "the anchor's node hidden";
  Renderable.set_visible (Text.node one) true;
  frame r;
  select r (1, 1) (2, 3);
  Renderable.set_visible (Text.node three) false;
  nothing_shown "the focus's node hidden"

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

let () =
  run "mosaic.selection"
    [
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
          test "an end's node hidden shows and copies nothing"
            an_ends_node_hidden_shows_and_copies_nothing;
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
        ];
    ]
