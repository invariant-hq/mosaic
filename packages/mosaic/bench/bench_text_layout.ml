open Mosaic_ui

let make_surface spans =
  let renderer = Renderer.create () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  let buffer = Text_buffer.create () in
  Text_buffer.set_styled_text buffer spans;
  let surface = Text_surface.create node buffer in
  Text_surface.set_wrap surface `Word;
  (renderer, surface)

let plain =
  [
    { Text_buffer.text = "A short terminal label"; style = Ansi.Style.default };
  ]

let styled =
  List.init 10_000 (fun i ->
      {
        Text_buffer.text = "word text ";
        style =
          (if i mod 2 = 0 then Ansi.Style.default
           else Ansi.Style.make ~bold:true ());
      })

let reflow (renderer, surface) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Text_surface.set_wrap_width surface (Some 79);
  let a = Text_surface.display_line_count surface in
  Text_surface.set_wrap_width surface (Some 81);
  a + Text_surface.display_line_count surface

let cold_reflow ((_, surface) as input) =
  Text_surface.set_wrap surface `Char;
  Text_surface.set_wrap surface `Word;
  reflow input

(* Five widths cycled through a cache of four: every width lays out again. *)
let alternating_widths (renderer, surface) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let rows = ref 0 in
  for width = 76 to 80 do
    Text_surface.set_wrap_width surface (Some width);
    rows := !rows + Text_surface.display_line_count surface
  done;
  !rows

let editor_text = String.make 1_000_000 'x'

let edit editor =
  let edited = Editor.insert "y" editor in
  Editor.length edited + Editor.length (Editor.undo edited)

let paint_surface content ~height ~scroll_x =
  let renderer, surface =
    make_surface [ { Text_buffer.text = content; style = Ansi.Style.default } ]
  in
  Text_surface.set_wrap surface `None;
  let node = Text_surface.node surface in
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80.
    ~height:(float_of_int height);
  Text_surface.set_scroll_x surface scroll_x;
  let grid = Matrix_grid.create ~width:80 ~height:20 () in
  Renderable.Private.render node grid ~dt:0.;
  (renderer, node, grid)

let paint (renderer, node, grid) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Renderable.Private.render node grid ~dt:0.

let tall lines = String.concat "" (List.init lines (fun _ -> "one line\n"))

let overlay_revision buffer =
  Text_buffer.clear_highlights buffer;
  for i = 0 to 199 do
    Text_buffer.add_highlight buffer
      (Text_buffer.Highlight.make ~start_offset:i ~end_offset:(400 - i)
         ~style:(Ansi.Style.make ~bold:true ())
         ~ref_id:i ())
  done;
  Text_buffer.style_at buffer 200

let focus_surface length =
  let renderer = Renderer.create () in
  let area =
    Textarea.create ~parent:(Renderer.root renderer) ~on_change:ignore
      (Textarea.Props.make ~focused_text_color:Ansi.Color.cyan
         ~state:(Editor.of_string (String.make length 'x'))
         ())
  in
  let node = Textarea.node area in
  Renderable.set_focusable node true;
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80. ~height:20.;
  ignore (Text_surface.layout (Textarea.surface area) : Matrix_text.Layout.t);
  (renderer, area)

let focus_layout (renderer, area) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let node = Textarea.node area and surface = Textarea.surface area in
  ignore (Renderable.focus node : bool);
  let focused = Text_surface.display_line_count surface in
  Renderable.blur node;
  focused + Text_surface.display_line_count surface

let restyle_surface () =
  let renderer, surface = make_surface styled in
  Text_surface.set_wrap_width surface (Some 80);
  ignore (Text_surface.layout surface : Matrix_text.Layout.t);
  let alternate =
    List.map
      (fun (span : Text_buffer.span) ->
        { span with style = Ansi.Style.make ~italic:true () })
      styled
  in
  (renderer, surface, alternate)

let restyle_layout (renderer, surface, alternate) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let buffer = Text_surface.buffer surface in
  Text_buffer.set_styled_text buffer alternate;
  let first = Text_surface.display_line_count surface in
  Text_buffer.set_styled_text buffer styled;
  first + Text_surface.display_line_count surface

let fragment_text count =
  let renderer = Renderer.create () in
  let text =
    Text.create ~parent:(Renderer.root renderer) (Text.Props.make ())
  in
  let first = List.init count (fun _ -> Text.Fragment.text "word ") in
  let second = List.init count (fun _ -> Text.Fragment.text "text ") in
  (renderer, text, first, second)

let replace_fragments (renderer, text, first, second) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Text.set_fragments text first;
  Text.set_fragments text second

(* ───── Reference paint workloads ─────

   A 80x20 viewport over text whose rows, columns or overlays mostly lie
   outside it. Setup builds and paints once; the measured call repaints the
   unchanged view. *)

let viewport ?(wrap = `None) ?(rows = 20) ?(scroll = `Top) ?(scroll_x = 0)
    ?(setup = ignore) spans =
  let renderer, surface = make_surface spans in
  Text_surface.set_wrap surface wrap;
  setup (Text_surface.buffer surface);
  let node = Text_surface.node surface in
  Renderable.Private.update_layout node ~x:0. ~y:0. ~width:80.
    ~height:(float_of_int rows);
  (match scroll with
  | `Top -> ()
  | `Middle ->
      Text_surface.set_scroll_y surface (Text_surface.max_scroll_y surface / 2));
  Text_surface.set_scroll_x surface scroll_x;
  let grid = Matrix_grid.create ~width:80 ~height:rows () in
  Renderable.Private.render node grid ~dt:0.;
  (renderer, node, grid)

let span text = [ { Text_buffer.text; style = Ansi.Style.default } ]

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

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "text_layout"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words; promoted_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
         ]))
    [
      Thumper.bench "text/plain-props" (fun () ->
          Text.Props.make ~content:(Thumper.black_box "plain label") ());
      Thumper.bench_with_setup
        ~setup:(fun () -> fragment_text 1_000)
        "fragments/normalize-1k" replace_fragments;
      Thumper.bench_with_setup
        ~setup:(fun () -> fragment_text 10_000)
        "fragments/normalize-10k" replace_fragments;
      Thumper.bench_with_setup
        ~setup:(fun () -> focus_surface 128)
        "focus/layout-128b" focus_layout;
      Thumper.bench_with_setup
        ~setup:(fun () -> focus_surface 1_000_000)
        "focus/layout-1mb" focus_layout;
      Thumper.bench_with_setup ~setup:restyle_surface
        "styles/same-text-layout-100k" restyle_layout;
      Thumper.bench_with_setup
        ~setup:(fun () -> editor_text)
        "index/create-1mb" Editor.of_string;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_surface plain)
        "reflow/label" reflow;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_surface styled)
        "reflow/styled-100k" reflow;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_surface styled)
        "reflow/styled-100k-cold" cold_reflow;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          Editor.of_string editor_text |> Editor.set_cursor 500_000)
        "edit/1mb-middle-undo" edit;
      Thumper.bench_with_setup
        ~setup:(fun () -> paint_surface (tall 1_000) ~height:1_000 ~scroll_x:0)
        "paint/tall-1k" paint;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          paint_surface (tall 100_000) ~height:100_000 ~scroll_x:0)
        "paint/tall-100k" paint;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          paint_surface (String.make 100_000 'x') ~height:1 ~scroll_x:99_900)
        "paint/horizontal-100k" paint;
      Thumper.bench_with_setup
        ~setup:(fun () -> paint_surface editor_text ~height:1 ~scroll_x:999_900)
        "paint/horizontal-1m" paint;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_surface styled)
        "reflow/alternating-widths-100k" alternating_widths;
      Thumper.bench_with_setup
        ~setup:(fun () -> viewport plain)
        "paint/label" paint;
      Thumper.bench_with_setup
        ~setup:(fun () -> viewport ~scroll:`Middle (span (tall 100_000)))
        "paint/tall-100k-middle" paint;
      Thumper.bench_with_setup
        ~setup:(fun () -> viewport ~wrap:`Word ~scroll:`Middle styled)
        "paint/styled-100k-middle" paint;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          viewport ~setup:(dense_overlays 200) (span visible_lines))
        "paint/overlays-dense-200" paint;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          viewport ~scroll:`Middle ~setup:(hidden_overlays 10_000)
            (span (tall 100_000)))
        "paint/overlays-hidden-10k" paint;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          let b = Text_buffer.create () in
          Text_buffer.set_text b (String.make 400 'x');
          b)
        "styles/nested-revision-200" overlay_revision;
    ]
