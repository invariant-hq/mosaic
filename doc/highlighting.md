# Syntax highlighting with tree-sitter

Mosaic colors source code from scope-annotated byte ranges and does not depend
on a parser. An application that wants tree-sitter highlighting writes the glue
between the parser and Mosaic itself. This page is that glue; the rules it
relies on are stated in the interfaces listed at the end.

## The glue

The independently packaged [Tree-sitter bindings](https://github.com/invariant-hq/ocaml-tree-sitter)
provide `tree-sitter`, `tree-sitter.ocaml` and `tree-sitter.json`. Add those
libraries to your application's Dune stanza alongside `mosaic`. The
[runnable integration example](https://github.com/invariant-hq/ocaml-tree-sitter/tree/main/examples/mosaic)
uses this complete glue:

```ocaml
module Ts = Tree_sitter

(* Tree-sitter's point for [byte] in [s]: its row, and its column in bytes. *)
let point s byte =
  let row = ref 0 and line = ref 0 in
  for i = 0 to byte - 1 do
    if s.[i] = '\n' then begin
      incr row;
      line := i + 1
    end
  done;
  { Ts.row = !row; column = byte - !line }

(* A grammar parses again from the tree of the view's latest result, edited as
   the view says its content changed, then queries the new tree for the
   captures that meet the bytes the view needs. A highlighter makes one call at
   a time, so one parser per grammar is enough. *)
let grammar language query =
  let parser = Ts.Parser.create language in
  let query = Ts.Query.create language ~source:query in
  fun ~content ~range
      (previous : Ts.Tree.t option Mosaic.Code.Highlighter.previous option) ->
    let old =
      match previous with
      | Some
          {
            content = source;
            state = Some tree;
            edit = { start; removed; inserted };
          } ->
          (* After a failure the view passes the same tree again, so the edit
             goes to a copy. *)
          let old = Ts.Tree.copy tree in
          Ts.Tree.edit old ~start_byte:start ~old_end_byte:(start + removed)
            ~new_end_byte:(start + inserted) ~start_point:(point content start)
            ~old_end_point:(point source (start + removed))
            ~new_end_point:(point content (start + inserted));
          Some old
      | Some { state = None; _ } | None -> None
    in
    let tree = Ts.Parser.parse_string ?old parser content in
    let start_byte, end_byte = range in
    (Ts.highlight_range query tree ~start_byte ~end_byte, Some tree)

let ocaml =
  grammar (Tree_sitter_ocaml.ocaml ()) Tree_sitter_ocaml.ocaml_highlights_query

let interface =
  grammar
    (Tree_sitter_ocaml.interface ())
    Tree_sitter_ocaml.interface_highlights_query

let json = grammar (Tree_sitter_json.language ()) Tree_sitter_json.highlights_query

let highlight { Mosaic.Code.Highlighter.language; content; range; previous } =
  let captures, tree =
    match language with
    | "ocaml" | "ml" -> ocaml ~content ~range previous
    | "mli" -> interface ~content ~range previous
    | "json" -> json ~content ~range previous
    | _ -> ([], None)
  in
  (Mosaic.Syntax_highlight.of_triples ~content captures, tree)

let highlighter = Mosaic.Code.Highlighter.threaded_incremental highlight

let code_view src =
  Mosaic.code
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    src

let markdown_view md = Mosaic.markdown ~highlighter md
```

`highlight` is the application's resolver. The language names, aliases and
filename extensions your application uses go there, and unknown languages stay
plain. Its state is the tree of the view's latest result. A keystroke in a large
file is parsed again around the edit, and the query runs over the whole new
tree but returns only the captures that meet the request's `range`: the whole
text on the view's first request, and afterwards the rows the view shows and a
screen above and below them. A capture that meets the range comes whole, and
the range never changes what a capture is, as the result type of
[`Code.Highlighter`](../packages/mosaic/lib/ui/renderables/code.mli) requires.
`threaded_incremental` runs `highlight` on a worker thread and hands it each
request with the view's latest result.

A grammar whose highlights resolve local variables, as tree-sitter's
`locals.scm` does, still runs that query over the whole tree after each parse,
and keeps the scopes and definitions it finds in its state; a reference in the
range is then coloured by its definition wherever that lies, as in a highlight
of the whole tree. The bindings used here have no locals resolution, so the
glue above has nothing to keep.

A highlighter that keeps nothing between results, such as a lexical one, is a
function of the language and the content, and `threaded` or `sync` takes it as
it is. Such a highlighter is always asked for the whole text, so scrolling the
view never asks it again. This one marks every `let`:

```ocaml
let lets ~language:_ ~content =
  let rec find from acc =
    match String.index_from_opt content from 'l' with
    | Some i when i + 3 <= String.length content && String.sub content i 3 = "let"
      ->
        find (i + 3) ((i, i + 3, "keyword") :: acc)
    | Some i -> find (i + 1) acc
    | None -> List.rev acc
  in
  Mosaic.Syntax_highlight.of_triples ~content (find 0 [])

let lexical_highlighter = Mosaic.Code.Highlighter.threaded lets
```

## Editable text

A textarea takes the same syntax settings as a code view, and may share its
highlighter:

```ocaml
let editor_view highlighter state =
  Mosaic.textarea ~state
    ~syntax:(Mosaic.Code.with_highlighter ~language:"ocaml" highlighter)
    ~on_change:(fun state -> Edit state)
    ()
```

Styles the application resolves itself, such as search matches or
diagnostics, go in `~annotations`, which the textarea shows over the syntax
styles.

## Where the rules live

- [`Code`](../packages/mosaic/lib/ui/renderables/code.mli): highlighters, their
  requests, states and failures, what a result must hold, the worker of
  `Highlighter.threaded_incremental`, and what a view shows while a result is pending.
- [`Syntax_highlight`](../packages/mosaic/lib/ui/syntax_highlight.mli): byte ranges,
  captures bound to their source, and their resolution into annotations.
- [`Syntax_style`](../packages/mosaic/lib/ui/syntax_style.mli): the scope vocabulary
  shared by highlighters and styles, and how scopes fall back.
- [`Markdown`](../packages/mosaic/lib/ui/renderables/markdown.mli) and
  [`Diff`](../packages/mosaic/lib/ui/renderables/diff.mli): how fenced code blocks and
  diff panes use a highlighter.
- [`Textarea`](../packages/mosaic/lib/ui/renderables/textarea.mli) and
  [`Text_buffer.Annotations`](../packages/mosaic/lib/ui/text_buffer.mli): syntax and
  annotations on editable text.

Mosaic's [examples](../packages/mosaic/examples/common/toy_highlighter.ml) use a small
lexical highlighter in place of a parser package.
