open Windtrap
open Mosaic_ui
module Patch = Diff.Patch

(* ── Helpers ── *)

let parse s =
  match Patch.of_unified s with
  | Ok patch -> patch
  | Error message -> failwith ("parse failed: " ^ message)

let base_diff =
  {|--- a/test.js
+++ b/test.js
@@ -1,3 +1,3 @@
 function hello() {
-  console.log("Hello");
+  console.log("Hello, World!");
 }
|}

let grown_diff =
  {|--- a/test.js
+++ b/test.js
@@ -1,3 +1,4 @@
 function hello() {
-  console.log("Hello");
+  console.log("Hello, World!");
+  console.log("Streamed");
 }
|}

let rec collect_nodes node =
  node :: List.concat_map collect_nodes (Renderable.children node)

let full_style =
  let full = Toffee.Style.Dimension.percent 1. in
  Toffee.Style.make ~size:(Toffee.Geometry.Size.make full full) ()

let span line start_byte end_byte =
  { Diff.side = New; line; start_byte; end_byte; color = Ansi.Color.red }

let render renderer ~width ~height =
  (match Renderer.render_frame_until_settled renderer ~width ~height ~dt:0. with
  | `Settled -> ()
  | `Pending _ -> failwith "render did not settle");
  Matrix_screen.next_grid (Renderer.screen renderer)

let row_text grid y =
  let b = Buffer.create 80 in
  for x = 0 to Matrix_grid.width grid - 1 do
    let i = Matrix_grid.idx grid ~x ~y in
    if not (Matrix_grid.is_continuation grid i) then
      match Matrix_grid.get_text grid i with
      | "" -> Buffer.add_char b ' '
      | text -> Buffer.add_string b text
  done;
  Buffer.contents b

let count text sub =
  let n = String.length sub in
  let rec loop acc i =
    if i + n > String.length text then acc
    else if String.sub text i n = sub then loop (acc + 1) (i + n)
    else loop acc (i + 1)
  in
  loop 0 0

let contains text sub = count text sub > 0

(* The first row whose text contains [sub]. *)
let row_with grid sub =
  let rec find y =
    if y >= Matrix_grid.height grid then failwith ("no row has " ^ sub)
    else if contains (row_text grid y) sub then y
    else find (y + 1)
  in
  find 0

(* Whether the first cell holding grapheme [text] has the span color. *)
let colored grid text =
  let rec find i =
    if i >= Matrix_grid.width grid * Matrix_grid.height grid then
      failwith ("no cell holds " ^ text)
    else if Matrix_grid.get_text grid i = text then
      Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_bg grid i)
    else find (i + 1)
  in
  find 0

let added lines =
  Printf.sprintf "--- a/t\n+++ b/t\n@@ -0,0 +1,%d @@\n%s" (List.length lines)
    (String.concat "" (List.map (fun line -> "+" ^ line ^ "\n") lines))

let unified ?(width = 20) ?(height = 4) ?wrap ?(line_spans = []) patch =
  let renderer = Renderer.create () in
  let diff =
    Diff.create ~parent:(Renderer.root renderer) ~style:full_style
      (Diff.Props.make ~patch:(parse patch) ?wrap ~line_spans ())
  in
  (diff, render renderer ~width ~height)

(* ── Reconciliation ── *)

let unified_update_reuses_children () =
  (* A streaming patch update in the default Unified layout must reconcile the
     existing Line_number and Code children in place rather than destroying
     and recreating them, so highlight jobs and streaming anti-flash state
     survive each chunk. *)
  let renderer = Renderer.create () in
  let d =
    Diff.create ~parent:(Renderer.root renderer)
      (Diff.Props.make ~patch:(parse base_diff) ())
  in
  Renderer.render_frame renderer ~width:60 ~height:10 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let before = collect_nodes (Diff.node d) in
  Diff.set_patch d (parse grown_diff);
  let after = collect_nodes (Diff.node d) in
  equal ~msg:"same subtree shape" int (List.length before) (List.length after);
  is_true ~msg:"every node is reused" (List.for_all2 ( == ) before after)

let empty_patch_drops_children () =
  let renderer = Renderer.create () in
  let d =
    Diff.create ~parent:(Renderer.root renderer)
      (Diff.Props.make ~patch:(parse base_diff) ())
  in
  Diff.set_patch d (Patch.make []);
  equal ~msg:"no children" int 0
    (List.length (Renderable.children (Diff.node d)))

(* ── Spans ── *)

(* A span's bytes map to the surface's graphemes, which index a cluster of no
   width as any other: the span on "b" after a zero-width space colors "b". *)
let span_counts_surface_graphemes () =
  let renderer = Renderer.create () in
  let patch = parse "--- a/t\n+++ b/t\n@@ -0,0 +1,1 @@\n+\u{200B}ab\n" in
  let span =
    {
      Diff.side = New;
      line = 1;
      start_byte = 4;
      end_byte = 5;
      color = Ansi.Color.red;
    }
  in
  ignore
    (Diff.create ~parent:(Renderer.root renderer)
       (Diff.Props.make ~patch ~show_line_numbers:false ~line_spans:[ span ] ())
      : Diff.t);
  Renderer.render_frame renderer ~width:20 ~height:3 ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  let bg_of text =
    let rec find i =
      if Matrix_grid.get_text grid i = text then Matrix_grid.get_bg grid i
      else find (i + 1)
    in
    find 0
  in
  is_true ~msg:"b is colored" (Ansi.Color.equal Ansi.Color.red (bg_of "b"));
  is_false ~msg:"a is not" (Ansi.Color.equal Ansi.Color.red (bg_of "a"))

(* A row of zero-width content still holds a grapheme, so the next row's span
   lands on its own byte. *)
let span_after_zero_width_row () =
  let _, grid =
    unified ~line_spans:[ span 2 1 2 ] (added [ "\u{200B}"; "ab" ])
  in
  is_true ~msg:"b is colored" (colored grid "b");
  is_false ~msg:"a is not" (colored grid "a")

(* CRLF is one grapheme of two bytes: a CRLF-terminated row does not shift the
   next row's span. *)
let span_after_crlf_row () =
  let _, grid = unified ~line_spans:[ span 2 1 2 ] (added [ "a\r"; "bc\r" ]) in
  is_true ~msg:"c is colored" (colored grid "c");
  is_false ~msg:"b is not" (colored grid "b")

(* A combining mark joins its base: the span after it starts past the
   cluster, on this row and the next. *)
let span_after_combining_mark () =
  let _, grid =
    unified
      ~line_spans:[ span 1 3 4; span 2 3 4 ]
      (added [ "e\u{301}x"; "o\u{308}y" ])
  in
  is_true ~msg:"x is colored" (colored grid "x");
  is_true ~msg:"y is colored" (colored grid "y");
  is_false ~msg:"e is not" (colored grid "e\u{301}");
  is_false ~msg:"o is not" (colored grid "o\u{308}")

(* A wide character is one grapheme of three bytes and two cells. *)
let span_after_wide_character () =
  let _, grid =
    unified ~line_spans:[ span 1 3 4; span 2 0 3 ] (added [ "中x"; "文y" ])
  in
  is_true ~msg:"x is colored" (colored grid "x");
  is_false ~msg:"中 is not" (colored grid "中");
  is_true ~msg:"文 is colored" (colored grid "文");
  is_false ~msg:"y is not" (colored grid "y")

(* Word wrap moves a span's word to the continuation row with it. *)
let span_follows_word_wrap () =
  let _, grid =
    unified ~width:16 ~wrap:`Word
      ~line_spans:[ span 1 11 16 ]
      (added [ "alpha beta gamma delta" ])
  in
  is_true ~msg:"gamma wraps" (row_with grid "gamma" > row_with grid "alpha");
  is_true ~msg:"g is colored" (colored grid "g");
  is_false ~msg:"b is not" (colored grid "b")

(* ── Rows and lines ── *)

(* A final CR ends a logical line, so the pane holds an empty line past the
   last row. It continues that row and shows no line number of its own. *)
let final_cr_adds_no_line_number () =
  let _, grid = unified (added [ "a\r"; "bc\r" ]) in
  equal ~msg:"bc on row 1" int 1 (row_with grid "bc");
  is_false ~msg:"no number 3" (contains (row_text grid 2) "3")

(* A lone CR splits its row into two logical lines. The gutter numbers the row
   once, and the next row keeps its own number and hit. *)
let lone_cr_row_keeps_numbers () =
  let diff, grid = unified (added [ "a\rz"; "bc" ]) in
  let z = row_with grid "z" and bc = row_with grid "bc" in
  is_false ~msg:"z is unnumbered" (contains (row_text grid z) "2");
  is_true ~msg:"bc is line 2" (contains (row_text grid bc) "2");
  match Diff.hit_test diff ~x:0 ~y:bc with
  | Some { source = Some { side = New; line }; logical_row; _ } ->
      equal ~msg:"hit line" int 2 line;
      equal ~msg:"hit row" int 1 logical_row
  | Some _ | None -> fail "no hit on bc"

(* ── Split alignment ── *)

let asymmetric_wrap_diff =
  {|--- a/wrap.txt
+++ b/wrap.txt
@@ -1,3 +1,3 @@
 start
-short
+abcdefghijklmnopqrstuvwxyz0123456789
 end
|}

let end_is_aligned grid =
  equal ~msg:"end in both panes" int 2
    (count (row_text grid (row_with grid "end")) "end")

(* Resizing re-wraps only the right pane; alignment and the span follow the
   new rows at each width. *)
let asymmetric_resize_realigns () =
  let renderer = Renderer.create () in
  ignore
    (Diff.create ~parent:(Renderer.root renderer) ~style:full_style
       (Diff.Props.make
          ~patch:(parse asymmetric_wrap_diff)
          ~layout:Split ~wrap:`Char
          ~line_spans:[ span 2 0 100 ]
          ())
      : Diff.t);
  List.iter
    (fun width ->
      let grid = render renderer ~width ~height:8 in
      end_is_aligned grid;
      is_true ~msg:"last byte colored" (colored grid "9"))
    [ 44; 30; 60; 36 ]

(* A lone CR gives its row two logical lines; alignment counts both for the
   row, so the other pane pads after it. *)
let split_aligns_lone_cr_row () =
  let renderer = Renderer.create () in
  ignore
    (Diff.create ~parent:(Renderer.root renderer) ~style:full_style
       (Diff.Props.make
          ~patch:(parse "--- a/t\n+++ b/t\n@@ -1,2 +1,2 @@\n-a\rb\n+c\n end\n")
          ~layout:Split ~wrap:`Char ())
      : Diff.t);
  List.iter
    (fun width -> end_is_aligned (render renderer ~width ~height:6))
    [ 40; 30 ]

(* An asynchronous highlighter whose jobs complete only when the test says
   so. *)
let manual_highlighter () =
  let started = ref 0 and outstanding = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify:_ ->
        incr started;
        let outcome = ref None in
        outstanding :=
          (fun () ->
            outcome :=
              Some
                (Ok
                   ( Syntax_highlight.make
                       ~content:request.Code.Highlighter.content [],
                     () )))
          :: !outstanding;
        Code.Highlighter.job
          ~poll:(fun () ->
            let result = !outcome in
            outcome := None;
            result)
          ~cancel:(fun () -> ()))
  in
  let complete_all () =
    let jobs = !outstanding in
    outstanding := [];
    List.iter (fun complete -> complete ()) jobs
  in
  (started, complete_all, highlighter)

(* Alignment inserts blank rows into each pane's content, which must not
   restart the highlighting that alignment waits for. *)
let async_wrapped_split_settles () =
  let started, complete_all, highlighter = manual_highlighter () in
  let syntax = Diff.syntax ~language:"text" highlighter in
  let renderer = Renderer.create () in
  ignore
    (Diff.create ~parent:(Renderer.root renderer) ~style:full_style
       (Diff.Props.make
          ~patch:(parse asymmetric_wrap_diff)
          ~layout:Split ~wrap:`Char
          ~highlight:{ Diff.old = syntax; new_ = syntax }
          ())
      : Diff.t);
  let frame () =
    Renderer.render_frame renderer ~width:30 ~height:8 ~dt:0.;
    complete_all ()
  in
  for _ = 1 to 10 do
    frame ()
  done;
  let settled = !started in
  for _ = 1 to 10 do
    frame ()
  done;
  equal ~msg:"no highlighting once aligned" int settled !started;
  end_is_aligned (render renderer ~width:30 ~height:8)

let syntax_compares_styles_by_value () =
  let highlighter =
    Code.Highlighter.sync (fun ~language:_ ~content ->
        Syntax_highlight.make ~content [])
  in
  let props color =
    let style =
      Syntax_style.make ~base:Ansi.Style.default
        [ ("keyword", Ansi.Style.fg color Ansi.Style.default) ]
    in
    let syntax = Diff.syntax ~language:"js" ~style highlighter in
    Diff.Props.make ~patch:(parse base_diff)
      ~highlight:{ Diff.old = syntax; new_ = syntax }
      ()
  in
  is_true ~msg:"rebuilt equal styles are equal props"
    (Diff.Props.equal (props Ansi.Color.red) (props Ansi.Color.red));
  is_false ~msg:"a changed style is a change"
    (Diff.Props.equal (props Ansi.Color.red) (props Ansi.Color.blue))

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.diff}) or to {!Diff.Props.make}
   for {!Diff.create}, reaches the live diff ({!Test_harness.reaches}). *)

let doors_size = Mosaic.size ~width:24 ~height:6
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red

let doors_patch =
  Diff.Patch.of_strings ~old:"a\nb with a long line to wrap\n"
    ~new_:"a\nc with a long line to wrap\n" ()

let doors_other_patch = Diff.Patch.of_strings ~old:"x\n" ~new_:"y\n" ()

let doors_highlighter =
  Code.Highlighter.sync (fun ~language:_ ~content ->
      Syntax_highlight.of_triples ~content [ (0, 1, "keyword") ])

let doors_highlight =
  let syntax =
    Diff.syntax ~language:"text"
      ~style:
        (Syntax_style.make ~base:Ansi.Style.default
           [ ("keyword", Ansi.Style.make ~fg:doors_red ()) ])
      doors_highlighter
  in
  { Diff.old = syntax; new_ = syntax }

(* Drags over the first changed line's content, which a code view shows. *)
let doors_select renderer node =
  ignore (Renderer.render ~full:true renderer : string);
  let x = Renderable.x node + 8 and y = Renderable.y node + 1 in
  ignore
    (Test_harness.dispatch_mouse renderer
       (Test_harness.mouse_report ~x ~y (Press Left)));
  ignore
    (Test_harness.dispatch_mouse renderer
       (Test_harness.mouse_report ~x:(x + 4) ~y (Motion (Some Left))))

let doors_selected renderer _ =
  Option.value ~default:"(none)" (Renderer.selection_text renderer)

let doors_patch_label () =
  Test_harness.reaches "patch"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:(Mosaic.diff ~size:doors_size doors_other_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_other_patch ())))

let doors_layout () =
  Test_harness.reaches "layout"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:(Mosaic.diff ~size:doors_size ~view:Diff.Split doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ~layout:Diff.Split ())))

let doors_theme () =
  Test_harness.reaches "theme"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size
         ~theme:{ Diff.default_theme with added_bg = doors_red }
         doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch
              ~theme:{ Diff.default_theme with added_bg = doors_red }
              ())))

let doors_highlight () =
  Test_harness.reaches "highlight"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size ~highlight:doors_highlight doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ~highlight:doors_highlight ())))

let doors_line_highlights () =
  Test_harness.reaches "line_highlights"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size
         ~line_highlights:
           [
             {
               Diff.side = New;
               first = 2;
               last = 2;
               color = { Line_number.gutter = doors_red; content = None };
             };
           ]
         doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch
              ~line_highlights:
                [
                  {
                    Diff.side = New;
                    first = 2;
                    last = 2;
                    color = { Line_number.gutter = doors_red; content = None };
                  };
                ]
              ())))

let doors_line_spans () =
  Test_harness.reaches "line_spans"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size
         ~line_spans:
           [
             {
               Diff.side = New;
               line = 2;
               start_byte = 0;
               end_byte = 1;
               color = doors_red;
             };
           ]
         doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch
              ~line_spans:
                [
                  {
                    Diff.side = New;
                    line = 2;
                    start_byte = 0;
                    end_byte = 1;
                    color = doors_red;
                  };
                ]
              ())))

let doors_line_signs () =
  Test_harness.reaches "line_signs"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size
         ~line_signs:
           [
             {
               Diff.side = New;
               first = 2;
               last = 2;
               sign =
                 {
                   Line_number.before = Some "!";
                   after = None;
                   before_color = None;
                   after_color = None;
                 };
             };
           ]
         doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch
              ~line_signs:
                [
                  {
                    Diff.side = New;
                    first = 2;
                    last = 2;
                    sign =
                      {
                        Line_number.before = Some "!";
                        after = None;
                        before_color = None;
                        after_color = None;
                      };
                  };
                ]
              ())))

let doors_show_line_numbers () =
  Test_harness.reaches "show_line_numbers"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:(Mosaic.diff ~size:doors_size ~show_line_numbers:false doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ~show_line_numbers:false ())))

let doors_wrap () =
  Test_harness.reaches "wrap"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:(Mosaic.diff ~size:doors_size ~wrap:`Word doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ~wrap:`Word ())))

let doors_selectable () =
  Test_harness.reaches ~act:doors_select ~observe:doors_selected "selectable"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:(Mosaic.diff ~size:doors_size ~selectable:false doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ~selectable:false ())))

let doors_text_style () =
  Test_harness.reaches "text_style"
    ~base:(Mosaic.diff ~size:doors_size doors_patch)
    ~element:
      (Mosaic.diff ~size:doors_size
         ~text_style:(Ansi.Style.make ~bold:true ())
         doors_patch)
    ~create_base:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch ())))
    ~create:(fun parent ->
      Diff.node
        (Diff.create ~parent ~style:doors_style
           (Diff.Props.make ~patch:doors_patch
              ~text_style:(Ansi.Style.make ~bold:true ())
              ())))

let () =
  run "mosaic.diff"
    [
      group "Reconciliation"
        [
          test "unified update reuses children" unified_update_reuses_children;
          test "empty patch drops children" empty_patch_drops_children;
          test "syntax compares styles by value" syntax_compares_styles_by_value;
        ];
      group "Spans"
        [
          test "span counts surface graphemes" span_counts_surface_graphemes;
          test "span after zero-width row" span_after_zero_width_row;
          test "span after CRLF row" span_after_crlf_row;
          test "span after combining mark" span_after_combining_mark;
          test "span after wide character" span_after_wide_character;
          test "span follows word wrap" span_follows_word_wrap;
        ];
      group "Rows and lines"
        [
          test "final CR adds no line number" final_cr_adds_no_line_number;
          test "lone CR row keeps numbers" lone_cr_row_keeps_numbers;
        ];
      group "Split alignment"
        [
          test "asymmetric resize realigns" asymmetric_resize_realigns;
          test "split aligns lone CR row" split_aligns_lone_cr_row;
          test "async wrapped split settles" async_wrapped_split_settles;
        ];
      group "Both doors"
        [
          test "patch" doors_patch_label;
          test "layout" doors_layout;
          test "theme" doors_theme;
          test "highlight" doors_highlight;
          test "line_highlights" doors_line_highlights;
          test "line_spans" doors_line_spans;
          test "line_signs" doors_line_signs;
          test "show_line_numbers" doors_show_line_numbers;
          test "wrap" doors_wrap;
          test "selectable" doors_selectable;
          test "text_style" doors_text_style;
        ];
    ]
