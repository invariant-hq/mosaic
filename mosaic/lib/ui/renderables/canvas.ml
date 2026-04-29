(* ───── Props ───── *)

module Props = struct
  type t = { respect_alpha : bool }

  let make ?(respect_alpha = false) () = { respect_alpha }
  let default = make ()
  let equal a b = a.respect_alpha = b.respect_alpha
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  grid : Grid.t;
  mutable props : Props.t;
  mutable on_draw : (t -> delta:float -> unit) option;
  mutable on_resize : (t -> unit) option;
}

(* ───── Accessors ───── *)

let node t = t.node
let grid t = t.grid
let width t = Grid.width t.grid
let height t = Grid.height t.grid

(* ───── Drawing ───── *)

let draw_text ?style ?tab_width t ~x ~y ~text =
  Grid.draw_text ?style ?tab_width t.grid ~x ~y ~text

let fill_rect t ~x ~y ~width ~height ~color =
  Grid.fill_rect t.grid ~x ~y ~width ~height ~color

let draw_box t ~x ~y ~width ~height ?border ?sides ?style ?fill ?title
    ?title_alignment ?title_style () =
  Grid.draw_box t.grid ~x ~y ~width ~height ?border ?sides ?style ?fill ?title
    ?title_alignment ?title_style ()

let draw_line t ~x1 ~y1 ~x2 ~y2 ?style ?symbols ?kind () =
  Grid.draw_line t.grid ~x1 ~y1 ~x2 ~y2 ?style ?symbols ?kind ()

let set_cell t ~x ~y ~cell ~fg ~bg ~attrs ?link ?blend () =
  Grid.set_cell t.grid ~x ~y ~cell ~fg ~bg ~attrs ?link ?blend ()

let clear ?color t =
  Grid.clear ?color t.grid;
  Renderable.request_render t.node

(* ───── Rendering ───── *)

let render t _self parent_grid ~delta =
  let w = Renderable.width t.node and h = Renderable.height t.node in
  if w > 0 && h > 0 then begin
    (* Resize lazily during render so the grid always matches layout dimensions
       before blitting. The on_resize callback lets users redraw in the same
       frame. *)
    if Grid.width t.grid <> w || Grid.height t.grid <> h then begin
      Grid.resize t.grid ~width:w ~height:h;
      Option.iter (fun f -> f t) t.on_resize
    end;
    (* on_draw fires after resize so the callback sees up-to-date dimensions. *)
    Option.iter (fun f -> f t ~delta) t.on_draw;
    Grid.blit_region ~src:t.grid ~dst:parent_grid ~src_x:0 ~src_y:0 ~width:w
      ~height:h ~dst_x:(Renderable.x t.node) ~dst_y:(Renderable.y t.node)
  end

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?respect_alpha
    () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let respect_alpha = Option.value ~default:false respect_alpha in
  (* 1×1 placeholder; resized to match layout on the first render pass. *)
  let grid =
    Grid.create ~width:1 ~height:1 ~width_method:`Unicode ~respect_alpha ()
  in
  let props = Props.make ~respect_alpha () in
  let t = { node; grid; props; on_draw = None; on_resize = None } in
  Renderable.set_render node (render t);
  t

(* ───── Callbacks ───── *)

let set_on_draw t cb = t.on_draw <- cb
let set_on_resize t cb = t.on_resize <- cb

(* ───── Render Control ───── *)

let request_render t = Renderable.request_render t.node

(* ───── Properties ───── *)

let set_respect_alpha t v =
  if t.props.respect_alpha <> v then begin
    t.props <- { respect_alpha = v };
    Grid.set_respect_alpha t.grid v;
    Renderable.request_render t.node
  end

let respect_alpha t = t.props.respect_alpha

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  if t.props.respect_alpha <> props.respect_alpha then begin
    Grid.set_respect_alpha t.grid props.respect_alpha;
    Renderable.request_render t.node
  end;
  t.props <- props

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Canvas(%s, %dx%d)" (Renderable.id t.node)
    (Grid.width t.grid) (Grid.height t.grid)
