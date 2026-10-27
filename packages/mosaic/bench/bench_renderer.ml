open Mosaic_ui

let transcript_rows = 1_000
let viewport_width = 80
let viewport_height = 24

let row_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int viewport_width))
  |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)

let viewport_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int viewport_width))
  |> Toffee.Style.set_height
       (Toffee.Style.Dimension.length (Float.of_int viewport_height))

let row_text =
  Thumper.black_box
    "A representative transcript row with enough text to exercise grapheme \
     batching."

let render_frame renderer =
  Renderer.render_frame renderer ~width:viewport_width ~height:viewport_height
    ~dt:16.;
  ignore (Renderer.render renderer : string)

let make_transcript_pair () =
  let renderer = Renderer.create () in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style:viewport_style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll scroll (Some (Scroll_box.set_scroll scroll));
  for _ = 1 to transcript_rows do
    ignore
      (Text.create ~parent:(Scroll_box.node scroll) ~style:row_style
         (Text.Props.make ~content:row_text ~selectable:false ())
        : Text.t)
  done;
  render_frame renderer;
  (renderer, scroll)

let make_transcript () = fst (make_transcript_pair ())

let steady_scrolled_transcript =
  Thumper.bench_with_setup ~setup:make_transcript
    "frame/scrolled-transcript-1000" render_frame

(* One-row wheel tick in a full-screen transcript. With the DECSTBM scroll
   hint the presentation shifts the region in hardware and rewrites only the
   revealed row; without it the scrolled content changes every viewport row.
   Rows carry distinct content — a transcript of identical rows diffs to
   nothing when shifted and would measure an empty present. A sample
   scrolls one row down and back, two frames, so that every sample starts
   at the same position and allocates the same. *)
type scroll_state = { scroll_renderer : Renderer.t; scroll_box : Scroll_box.t }

let scroll_phrases =
  Thumper.black_box
    [|
      "the daemon reported a checkpoint and forty-one workers resumed cleanly";
      "compaction finished in the background while queries kept their latency";
      "a replica lagged behind the quorum until the log shipped its segment";
      "the scheduler rebalanced shards across nodes without dropping a frame";
      "cache pressure stayed flat as the resident set crossed the threshold";
    |]

let make_scroll_by_one () =
  let renderer = Renderer.create () in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style:viewport_style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll scroll (Some (Scroll_box.set_scroll scroll));
  for i = 1 to transcript_rows do
    let content =
      Printf.sprintf "%04d %s" i
        scroll_phrases.(i mod Array.length scroll_phrases)
    in
    ignore
      (Text.create ~parent:(Scroll_box.node scroll) ~style:row_style
         (Text.Props.make ~content ~selectable:false ())
        : Text.t)
  done;
  render_frame renderer;
  Scroll_box.scroll_to scroll ~y:(transcript_rows / 2) ();
  render_frame renderer;
  { scroll_renderer = renderer; scroll_box = scroll }

let scroll_by_one_frames st =
  Scroll_box.scroll_by st.scroll_box ~y:1 ();
  render_frame st.scroll_renderer;
  Scroll_box.scroll_by st.scroll_box ~y:(-1) ();
  render_frame st.scroll_renderer

let scroll_by_one =
  Thumper.bench_with_setup ~setup:make_scroll_by_one
    "frame/scroll-one-row-and-back" scroll_by_one_frames

(* Steady full-frame scenario at 200x50: no scrolling, no layout changes.
   Exercises the per-frame fixed costs — buffer clears, command build, text
   painting, and the diff scan — at a size where clear cost is visible. *)
let idle_width = 200
let idle_height = 50

let idle_root_style =
  Toffee.Style.default
  |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
  |> Toffee.Style.set_size (Toffee.Style.Size_dim.pct ~w:100. ~h:100.)

let idle_row_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int idle_width))
  |> Toffee.Style.set_height (Toffee.Style.Dimension.length 1.)

let idle_text =
  Thumper.black_box
    "A steady full-width row whose glyphs are repainted every frame while \
     nothing changes."

let render_idle_frame renderer =
  Renderer.render_frame renderer ~width:idle_width ~height:idle_height ~dt:16.;
  ignore (Renderer.render renderer : string)

let make_idle_screen () =
  let renderer = Renderer.create ~style:idle_root_style () in
  for _ = 1 to idle_height do
    ignore
      (Text.create ~parent:(Renderer.root renderer) ~style:idle_row_style
         (Text.Props.make ~content:idle_text ~selectable:false ())
        : Text.t)
  done;
  render_idle_frame renderer;
  renderer

let idle_full_frame =
  Thumper.bench_with_setup ~setup:make_idle_screen "frame/full-200x50"
    render_idle_frame

(* Editor-typing scenario: a word-wrapped document whose content changes
   every frame, as a textarea does on every keystroke. Each frame pays the
   full measure + display rebuild for the new buffer version; the display
   cache must make measurement and rendering share one rebuild per width. *)
let typing_width = 80
let typing_height = 24

let typing_doc suffix =
  String.concat "\n"
    (List.init 150 (fun i ->
         Printf.sprintf
           "line %03d: the quick brown fox jumps over the lazy dog while the \
            lazy dog naps under the quick brown fox, wrapping past eighty \
            columns %s"
           i suffix))

let typing_a = Thumper.black_box (typing_doc "a")
let typing_b = Thumper.black_box (typing_doc "ab")

type typing_state = {
  renderer : Renderer.t;
  text : Text.t;
  mutable flip : bool;
}

let typing_frame st =
  st.flip <- not st.flip;
  Text.set_content st.text (if st.flip then typing_b else typing_a);
  Renderer.render_frame st.renderer ~width:typing_width ~height:typing_height
    ~dt:16.;
  ignore (Renderer.render st.renderer : string)

let make_typing () =
  let renderer = Renderer.create ~style:idle_root_style () in
  let text =
    Text.create ~parent:(Renderer.root renderer)
      (Text.Props.make ~content:typing_a ~wrap:`Word ~selectable:false ())
  in
  let st = { renderer; text; flip = false } in
  typing_frame st;
  st

let editor_typing =
  Thumper.bench_with_setup ~setup:make_typing "typing-wrapped-150-lines"
    typing_frame

(* Primary-mode frame: an inline region under the shell's output, laid out
   against the terminal's 50 rows and drawn into a region as tall as its
   content, as a primary-mode host does. A transcript viewport sits above a
   prompt line that changes every frame. *)
let primary_terminal_rows = 50

let primary_root_style =
  Toffee.Style.default
  |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
  |> Toffee.Style.set_width (Toffee.Style.Dimension.pct 100.)
  |> Toffee.Style.set_height Toffee.Style.Dimension.auto

type primary_state = {
  primary_renderer : Renderer.t;
  prompt : Text.t;
  mutable typed : bool;
}

let primary_frame st =
  st.typed <- not st.typed;
  Text.set_content st.prompt (if st.typed then "> ab" else "> a");
  let renderer = st.primary_renderer in
  Renderer.layout_frame renderer ~width:viewport_width
    ~height:primary_terminal_rows ~dt:16.;
  let rows = Renderable.height (Renderer.root renderer) in
  Renderer.paint_frame renderer ~height:rows;
  ignore (Renderer.render renderer : string)

let make_primary () =
  let renderer = Renderer.create ~style:primary_root_style () in
  let scroll =
    Scroll_box.create ~parent:(Renderer.root renderer) ~style:viewport_style
      (Scroll_box.Props.make ~show_scrollbars:false ())
  in
  Scroll_box.set_on_scroll scroll (Some (Scroll_box.set_scroll scroll));
  for _ = 1 to transcript_rows do
    ignore
      (Text.create ~parent:(Scroll_box.node scroll) ~style:row_style
         (Text.Props.make ~content:row_text ~selectable:false ())
        : Text.t)
  done;
  let prompt =
    Text.create ~parent:(Renderer.root renderer)
      (Text.Props.make ~content:"> a" ~selectable:false ())
  in
  let st = { primary_renderer = renderer; prompt; typed = false } in
  primary_frame st;
  st

let primary_typing =
  Thumper.bench_with_setup ~setup:make_primary "typing-inline-transcript-1000"
    primary_frame

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  (* Frame loops care about retention as much as speed: promotions are
     objects escaping the minor heap mid-frame, and minor collections count
     GC pressure per frame. *)
  Thumper.run "renderer"
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
    [
      Thumper.group "viewport" [ steady_scrolled_transcript; scroll_by_one ];
      Thumper.group "idle" [ idle_full_frame ];
      Thumper.group "editor" [ editor_typing ];
      Thumper.group "primary" [ primary_typing ];
    ]
