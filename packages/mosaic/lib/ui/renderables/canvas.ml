(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  grid : Matrix_grid.t;
  mutable on_draw : (t -> dt:float -> unit) option;
  mutable on_resize : (t -> unit) option;
}

(* ───── Accessors ───── *)

let node t = t.node
let grid t = t.grid
let width t = Matrix_grid.width t.grid
let height t = Matrix_grid.height t.grid
let theme t = Renderable.theme t.node

(* ───── Drawing ───── *)

let draw_text ?style ?tab_width t ~x ~y ~text =
  Matrix_grid.draw_text ?style ?tab_width t.grid ~x ~y ~text

let fill_rect t ~x ~y ~width ~height ~color =
  Matrix_grid.fill_rect t.grid ~x ~y ~width ~height ~color

let draw_box t ~x ~y ~width ~height ?border ?sides ?style ?fill ?title
    ?title_alignment ?title_style () =
  Matrix_grid.draw_box t.grid ~x ~y ~width ~height ?border ?sides ?style ?fill
    ?title ?title_alignment ?title_style ()

let draw_line t ~x1 ~y1 ~x2 ~y2 ?style ?symbols ?kind () =
  Matrix_grid.draw_line t.grid ~x1 ~y1 ~x2 ~y2 ?style ?symbols ?kind ()

let set_uchar t ~x ~y ~uchar ~fg ~bg ~attrs ?link ?blend () =
  Matrix_grid.set_uchar t.grid ~x ~y ~uchar ~fg ~bg ~attrs ?link ?blend ()

let clear ?color t =
  Matrix_grid.clear ?color t.grid;
  Renderable.request_render t.node

(* ───── Rendering ───── *)

let render t _self parent_grid ~dt =
  let w = Renderable.width t.node and h = Renderable.height t.node in
  if w > 0 && h > 0 then begin
    (* Resize lazily during render so the grid always matches layout dimensions
       before blitting. The on_resize callback lets users redraw in the same
       frame. *)
    if Matrix_grid.width t.grid <> w || Matrix_grid.height t.grid <> h then begin
      Matrix_grid.resize t.grid ~width:w ~height:h;
      Option.iter (fun f -> f t) t.on_resize
    end;
    (* The grid lays cells down by the screen's method, which can change while
       the canvas lives; cells drawn before a change keep their widths until
       they are drawn again. *)
    Matrix_grid.set_width_method t.grid (Renderable.Private.width_method t.node);
    (* on_draw fires after resize so the callback sees up-to-date dimensions. *)
    Option.iter (fun f -> f t ~dt) t.on_draw;
    Matrix_grid.draw_region ~src:t.grid ~dst:parent_grid ~src_x:0 ~src_y:0
      ~width:w ~height:h ~dst_x:(Renderable.x t.node)
      ~dst_y:(Renderable.y t.node)
  end

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* 1×1 placeholder; resized to match layout on the first render pass. The
     grid follows the screen's width method so drawing lays cells down the
     same way the parent grid does. *)
  let grid =
    Matrix_grid.create ~width:1 ~height:1
      ~width_method:(Renderable.Private.width_method node)
      ()
  in
  let t = { node; grid; on_draw = None; on_resize = None } in
  Renderable.set_render node (render t);
  t

(* ───── Callbacks ───── *)

let set_on_draw t cb = t.on_draw <- cb
let set_on_resize t cb = t.on_resize <- cb

(* ───── Render Control ───── *)

let request_render t = Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Canvas(%s, %dx%d)" (Renderable.id t.node)
    (Matrix_grid.width t.grid)
    (Matrix_grid.height t.grid)
