module Cell_grid = Matrix_grid
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

let grid_to_ansi grid = Cell_grid.to_ansi grid

(* ── Layout ── *)

(* Force a node to fill the render area by setting width:100% height:100%. *)
let fill_node node =
  let style =
    Renderable.style node
    |> Toffee.Style.set_width (Toffee.Style.Dimension.percent 1.)
    |> Toffee.Style.set_height (Toffee.Style.Dimension.percent 1.)
  in
  Renderable.set_style node style

(* Force the element a view mounted alone in the root to fill the render
   area. The reconciler sets the element's own layout again only when it
   changes, so this runs after each reconciliation. *)
let fill_mounted renderer =
  match Renderable.children (Renderer.root renderer) with
  | [ node ] ->
      let full = Toffee.Style.Dimension.percent 1. in
      let { Toffee.Geometry.Size.width; height } =
        Toffee.Style.size (Renderable.style node)
      in
      if not (width = full && height = full) then fill_node node
  | _ -> ()

(* Set the root node's style to explicit pixel dimensions so that child
   percentage dimensions (used by [fill_mounted]) resolve correctly. *)
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

let render ?(fill = true) ~width ~height vnode =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  Reconciler.render reconciler ~viewport_width:width vnode;
  if fill then fill_mounted renderer;
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let render_ansi ~width ~height vnode =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  Reconciler.render reconciler ~viewport_width:width vnode;
  fill_mounted renderer;
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_ansi grid)

(* ── Interactive Rendering ── *)

type app = {
  renderer : Renderer.t;
  reconciler : Reconciler.t;
  mutable viewport_width : int;
}

let make_app () =
  let renderer = Renderer.create () in
  let reconciler = Reconciler.create ~container:(Renderer.root renderer) in
  { renderer; reconciler; viewport_width = 80 }

let reconcile ?(fill = true) ?viewport_width app vnode =
  let viewport_width =
    Option.value viewport_width ~default:app.viewport_width
  in
  app.viewport_width <- viewport_width;
  Reconciler.render app.reconciler ~viewport_width vnode;
  if fill then fill_mounted app.renderer

let frame app ~width ~height =
  app.viewport_width <- width;
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen app.renderer) in
  print_newline ();
  print_string (grid_to_text grid)

(* Windtrap's convergence verb moved to its cookbook (recipe 7): probe first,
   then step, so a budget of [attempts] probes drives [attempts - 1] steps. *)
let eventually ~msg ~attempts ~step ~diagnose probe =
  let rec go n =
    match probe () with
    | Some v -> v
    | None when n >= attempts ->
        Windtrap.failf "%s: no convergence in %d attempts\n%s" msg attempts
          (String.concat "\n" (diagnose ()))
    | None ->
        step ();
        go (n + 1)
  in
  go 1

(* One line per pending work item, for [eventually]'s failure report: the
   renderer already knows which nodes are holding the frame up, and a bare
   "did not settle" throws that away. *)
let pending_lines renderer =
  match Renderer.pending_work renderer with
  | [] ->
      [ "no node reports pending work — a renderable requested another pass" ]
  | pending ->
      "still pending:"
      :: List.map
           (fun p ->
             let w = Renderer.Pending.work p in
             match w.Renderable.Pending.label with
             | Some label -> Printf.sprintf "  %s (%s)" w.kind label
             | None -> Printf.sprintf "  %s" w.kind)
           pending

(* The pass budget matches Renderer.render_frame_until_settled's default of 4.
   The first pass runs before probing: a renderer that has not rendered yet
   reports nothing pending and would read as settled. *)
let settled_frame app ~width ~height =
  app.viewport_width <- width;
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~dt:0.;
  eventually ~msg:"renderer settles within its pass budget" ~attempts:4
    ~step:(fun () -> Renderer.render_frame app.renderer ~width ~height ~dt:0.)
    ~diagnose:(fun () -> pending_lines app.renderer)
    (fun () -> if Renderer.is_settled app.renderer then Some () else None);
  let grid = Matrix_screen.next_grid (Renderer.screen app.renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let frame_ansi app ~width ~height =
  app.viewport_width <- width;
  set_viewport app.renderer ~width ~height;
  Renderer.render_frame app.renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen app.renderer) in
  print_newline ();
  print_string (grid_to_ansi grid)

let focus app node = ignore (Renderer.focus app.renderer node : bool)

(* The node of the reconciled element: the root's only child. *)
let mounted app = List.hd (Renderable.children (Renderer.root app.renderer))
let no_mod = Matrix_input.Keystroke.no_mods

let send key app =
  let event = Event.Key.of_input key in
  Renderer.dispatch_key app.renderer (Renderer.focus_path app.renderer) event

let send_char app c =
  send (Matrix_input.Keystroke.make (Char (Uchar.of_char c))) app

let send_key app key = send (Matrix_input.Keystroke.make key) app

let send_key_with_mod app ~modifier key =
  send (Matrix_input.Keystroke.make ~mods:modifier key) app

let mouse_report ~x ~y kind : Matrix_input.Mouse_report.t =
  { kind; x; y; shift = false; alt = false; ctrl = false }

(* ── Canvas Helpers ── *)

let print_canvas ?(width = 20) ?(height = 6) draw () =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  draw canvas;
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  let grid = Canvas.grid canvas in
  print_newline ();
  print_string (grid_to_text grid)

let print_canvas_ansi ?(width = 20) ?(height = 6) draw () =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let canvas = Canvas.create ~parent:(Renderer.root renderer) () in
  fill_node (Canvas.node canvas);
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  draw canvas;
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  let grid = Canvas.grid canvas in
  print_newline ();
  print_string (grid_to_ansi grid)

(* ── Markdown Helpers ── *)

type markdown_app = { md_renderer : Renderer.t; md : Markdown.t }

let render_markdown ?(width = 60) ?(height = 20) ?style ?conceal ?streaming
    ?render_node ?render_code ?highlighter ?syntax_style content =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ?style ?conceal ?streaming
      ?render_node ?render_code ?highlighter ?syntax_style ~content ()
  in
  fill_node (Markdown.node md);
  Renderer.render_frame renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  print_newline ();
  print_string (grid_to_text grid)

let make_markdown_app ?(width = 60) ?(height = 20) ?style ?conceal ?streaming
    ?render_node ?render_code ?highlighter ?syntax_style content =
  let renderer = Renderer.create () in
  set_viewport renderer ~width ~height;
  let md =
    Markdown.create ~parent:(Renderer.root renderer) ?style ?conceal ?streaming
      ?render_node ?render_code ?highlighter ?syntax_style ~content ()
  in
  fill_node (Markdown.node md);
  { md_renderer = renderer; md }

let markdown_frame app ~width ~height =
  set_viewport app.md_renderer ~width ~height;
  Renderer.render_frame app.md_renderer ~width ~height ~dt:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen app.md_renderer) in
  print_newline ();
  print_string (grid_to_text grid)
