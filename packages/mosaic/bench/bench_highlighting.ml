open Mosaic_ui

let captures size =
  List.init 1_000 (fun i ->
      let start = i * (size / 1_000) in
      (start, start + 4, "keyword"))

let highlighted_code size =
  let content = String.make size 'x' in
  let ranges = Syntax_highlight.of_triples ~content (captures size) in
  let syntax color =
    Code.syntax
      ~style:
        (Syntax_style.make ~base:Ansi.Style.default
           [ ("keyword", Ansi.Style.make ~fg:color ()) ])
      ranges
  in
  let first = Some (syntax Ansi.Color.red) in
  let second = Some (syntax Ansi.Color.blue) in
  let renderer = Renderer.create () in
  let code =
    Code.create ~parent:(Renderer.root renderer)
      (Code.Props.make ~content ?syntax:first ())
  in
  ignore (Text_surface.layout (Code.surface code) : Matrix_text.Layout.t);
  ignore (Text_buffer.style_at (Code.buffer code) 0 : Ansi.Style.t * int);
  (renderer, code, first, second)

let repaint (renderer, code, first, second) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Code.set_syntax code second;
  ignore (Text_buffer.style_at (Code.buffer code) 0 : Ansi.Style.t * int);
  Code.set_syntax code first;
  Text_buffer.style_at (Code.buffer code) 0

let plain_code size =
  let content = String.make size 'x' in
  let renderer = Renderer.create () in
  let code =
    Code.create ~parent:(Renderer.root renderer) (Code.Props.make ~content ())
  in
  ignore (Text_surface.layout (Code.surface code) : Matrix_text.Layout.t);
  let style =
    Syntax_style.make ~base:Ansi.Style.default
      [ ("keyword", Ansi.Style.make ~fg:Ansi.Color.red ()) ]
  in
  let first = captures size in
  let second = List.map (fun (a, b, scope) -> (a + 1, b + 1, scope)) first in
  (renderer, code, content, first, second, style)

let fresh_captures (renderer, code, content, first, second, style) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  let highlights = Syntax_highlight.of_triples ~content second in
  Code.set_syntax code (Some (Code.syntax ~style highlights));
  ignore (Text_buffer.style_at (Code.buffer code) 0 : Ansi.Style.t * int);
  let highlights = Syntax_highlight.of_triples ~content first in
  Code.set_syntax code (Some (Code.syntax ~style highlights));
  Text_buffer.style_at (Code.buffer code) 0

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "highlighting"
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
      Thumper.bench_with_setup
        ~setup:(fun () -> highlighted_code 100_000)
        "theme-roundtrip/100kb-1000-ranges" repaint;
      Thumper.bench_with_setup
        ~setup:(fun () -> highlighted_code 1_000_000)
        "theme-roundtrip/1mb-1000-ranges" repaint;
      Thumper.bench_with_setup
        ~setup:(fun () -> plain_code 100_000)
        "capture-roundtrip/100kb-1000-ranges" fresh_captures;
      Thumper.bench_with_setup
        ~setup:(fun () -> plain_code 1_000_000)
        "capture-roundtrip/1mb-1000-ranges" fresh_captures;
    ]
