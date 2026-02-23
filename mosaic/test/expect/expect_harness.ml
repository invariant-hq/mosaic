module Cell_grid = Grid
open Mosaic_ui
open Mosaic
module Canvas = Mosaic_ui.Canvas
module Markdown = Mosaic_ui.Markdown

(* CR: we shouldn't have harness per components, see how we can unify to have a
   single harness that works for canvas, markdown, etc. also probably don't want
   a different harness for interactive. why don't we just go through the
   reconciler all the time (once mosaic runtime is added back) *)

(* ── Grid to Text ── *)

let rstrip s =
  let len = String.length s in
  let i = ref (len - 1) in
  while !i >= 0 && s.[!i] = ' ' do
    decr i
  done;
  if !i = len - 1 then s else String.sub s 0 (!i + 1)

let grid_to_text grid =
  let w = Cell_grid.width grid in
  let h = Cell_grid.height grid in
  let line = Buffer.create w in
  let buf = Buffer.create (w * h) in
  for y = 0 to h - 1 do
    Buffer.clear line;
    for x = 0 to w - 1 do
      let idx = (y * w) + x in
      if Cell_grid.is_continuation grid idx then ()
      else
        let text = Cell_grid.get_text grid idx in
        if String.length text = 0 then Buffer.add_char line ' '
        else Buffer.add_string line text
    done;
    Buffer.add_string buf (rstrip (Buffer.contents line));
    if y < h - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let grid_to_ansi grid = Cell_grid.to_ansi ~reset:false grid

(* ── Layout ── *)

(* Force the vnode to fill the render area by setting width:100% height:100%. *)
let fill vnode =
  match vnode with
  | Vnode.Element elem ->
      let style =
        elem.attrs.style
        |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
        |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.)
      in
      Vnode.Element { elem with attrs = { elem.attrs with style } }
  | other -> other

(* Set the root node's style to explicit pixel dimensions so that child
   percentage dimensions (used by [fill]) resolve correctly. *)
let set_viewport renderer ~width ~height =
  let root = Renderer.root renderer in
  let style =
    Renderable.style root
    |> Toffee.Style.set_width
         (Toffee.Style.Dimension.length (Float.of_int width))
    |> Toffee.Style.set_height
         (Toffee.Style.Dimension.length (Float.of_int height))
  in
  Renderable.set_style root style

(* ── Static Rendering ── *)

let render ~width ~height vnode =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  Reconciler.render reconciler (fill vnode);
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let render_ansi ~width ~height vnode =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  Reconciler.render reconciler (fill vnode);
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_ansi grid)

(* ── Interactive Rendering ── *)

type app = { renderer : Renderer.t; reconciler : Reconciler.t }

let make_app () =
  let renderer = Renderer.create () in
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  { renderer; reconciler }

let reconcile app vnode = Reconciler.render app.reconciler (fill vnode)

let frame app ~width ~height =
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen app.renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let frame_ansi app ~width ~height =
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen app.renderer) in
  print_newline ();
  print_string (grid_to_ansi grid)

let focus app node = ignore (Renderer.focus app.renderer node : bool)
let no_mod = Input.Key.no_modifier

let send_char app c =
  let text = String.make 1 c in
  ignore
    (Renderer.dispatch_key app.renderer
       (Input.Key.of_char ~associated_text:text c)
      : Event.key)

let send_key app key =
  ignore (Renderer.dispatch_key app.renderer (Input.Key.make key) : Event.key)

let send_key_with_mod app ~modifier key =
  ignore
    (Renderer.dispatch_key app.renderer (Input.Key.make ~modifier key)
      : Event.key)

(* ── Canvas Helpers ── *)

let fill_node node =
  let style =
    Renderable.style node
    |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.)
  in
  Renderable.set_style node style

let print_canvas ?(width = 20) ?(height = 6) draw () =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  draw canvas;
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  let grid = Canvas.grid canvas in
  print_newline ();
  print_string (grid_to_text grid)

let print_canvas_ansi ?(width = 20) ?(height = 6) draw () =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  draw canvas;
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  let grid = Canvas.grid canvas in
  print_newline ();
  print_string (grid_to_ansi grid)

(* ── Markdown Helpers ── *)

type markdown_app = { md_renderer : Renderer.t; md : Markdown.t }

let render_markdown ?(width = 60) ?(height = 20) ?style ?conceal ?streaming
    ?render_node ?render_code content =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ?style ?conceal ?streaming
      ?render_node ?render_code ~content ()
  in
  fill_node (Markdown.node md);
  Renderer.render_frame renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let make_markdown_app ?(width = 60) ?(height = 20) ?style ?conceal ?streaming
    ?render_node ?render_code content =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ?style ?conceal ?streaming
      ?render_node ?render_code ~content ()
  in
  fill_node (Markdown.node md);
  { md_renderer = renderer; md }

let markdown_frame app ~width ~height =
  set_viewport app.md_renderer ~width ~height;
  Renderer.render_frame app.md_renderer ~width ~height ~delta:0.;
  let grid = Screen.grid (Renderer.screen app.md_renderer) in
  print_newline ();
  print_string (grid_to_text grid)
