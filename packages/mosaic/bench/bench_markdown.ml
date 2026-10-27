open Mosaic_ui

let viewport_width = 80
let viewport_height = 24

let render_frame renderer =
  Renderer.render_frame renderer ~width:viewport_width ~height:viewport_height
    ~dt:16.;
  ignore (Renderer.render renderer : string)

(* A highlighter that keeps code blocks on the Code-widget path without
   computing highlights. A highlighter is a resource, so create and props share
   one. *)
let highlighter =
  Code.Highlighter.sync (fun ~language:_ ~content ->
      Syntax_highlight.make ~content [])

let chunk i =
  Printf.sprintf
    "## Section %d\n\n\
     Paragraph with **bold** and `code` spans, number %d.\n\n\
     > A quoted line %d\n\n\
     - item a %d\n\
     - item b %d\n\n\
     ```ocaml\n\
     let f_%d x = x + %d\n\
     ```\n\n"
    i i i i i i i

let transcript n =
  let buf = Buffer.create (n * 200) in
  for i = 1 to n do
    Buffer.add_string buf (chunk i)
  done;
  Buffer.contents buf

type state = {
  renderer : Renderer.t;
  md : Markdown.t;
  base : string;
  appended : string;
}

let make_stream sections () =
  let renderer = Renderer.create () in
  let base = transcript (Thumper.black_box sections) in
  let md =
    Markdown.create ~parent:(Renderer.root renderer)
      (Markdown.Props.make ~content:base ~streaming:true ~highlighter ())
  in
  render_frame renderer;
  { renderer; md; base; appended = base ^ "\n\nstreamed tail paragraph" }

(* Append a paragraph to the transcript, then take it back: both directions
   exercise the content-only reconcile path over a prefix full of quote,
   list, and highlighted code blocks — the streaming chat-transcript shape
   this widget is built for. A sample does both, so that every sample starts
   from the same document and allocates the same. *)
let set_content st content =
  Markdown.apply_props st.md
    (Markdown.Props.make ~content ~streaming:true ~highlighter ());
  render_frame st.renderer

let streaming_append st =
  set_content st st.appended;
  set_content st st.base

(* The pair shows whether an append costs the tail or the whole document. *)
let steady_stream sections =
  Thumper.bench_with_setup ~setup:(make_stream sections)
    (Printf.sprintf "update/streaming-append-and-back-%dblocks" sections)
    streaming_append

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "markdown"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else
                Thumper.Metric.
                  [ wall_time; alloc_words; promoted_words; minor_collections ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           (* Promotions and minor collections depend on where the minor
              heap stands when a sample starts, so they are rarely exact:
              kept, with room for that noise. *)
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
           Thumper.Budget.relative ~metric:Thumper.Metric.minor_collections
             ~max_regression:0.5 ();
         ]))
    [ Thumper.group "transcript" [ steady_stream 16; steady_stream 256 ] ]
