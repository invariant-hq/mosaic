module Props = struct
  type t = {
    respect_alpha : bool;
    width_method : Glyph.width_method option;
    initial_width : int;
    initial_height : int;
  }

  let make ?(respect_alpha = false) ?width_method ?(initial_width = 1)
      ?(initial_height = 1) () =
    { respect_alpha; width_method; initial_width; initial_height }

  let default = make ()

  let equal a b =
    Bool.equal a.respect_alpha b.respect_alpha
    && Option.equal ( = ) a.width_method b.width_method
    && Int.equal a.initial_width b.initial_width
    && Int.equal a.initial_height b.initial_height
end

type t = {
  node : Renderable.t;
  surface : Grid.t;
  mutable intrinsic_width : int option;
  mutable intrinsic_height : int option;
  mutable draw : (Grid.t -> width:int -> height:int -> unit) option;
  mutable on_resize : (width:int -> height:int -> unit) option;
}

let node t = t.node
let width t = Grid.width t.surface
let height t = Grid.height t.surface
let grid t = t.surface
let request_render t = Renderable.request_render t.node

let resize_surface_to_layout t =
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  if lw > 0 && lh > 0 then
    let sw = Grid.width t.surface and sh = Grid.height t.surface in
    if lw <> sw || lh <> sh then Grid.resize t.surface ~width:lw ~height:lh

let current_dimensions t =
  let lw = Renderable.width t.node in
  let lh = Renderable.height t.node in
  let width = if lw > 0 then lw else width t in
  let height = if lh > 0 then lh else height t in
  (width, height)

let run_draw t ~width ~height =
  match t.draw with
  | Some draw when width > 0 && height > 0 ->
      let w = width and h = height in
      Grid.clear t.surface;
      let rect = Grid.{ x = 0; y = 0; width = w; height = h } in
      Grid.clip t.surface rect (fun () -> draw t.surface ~width:w ~height:h)
  | _ -> ()

let set_draw t draw =
  t.draw <- draw;
  match draw with
  | None -> ()
  | Some _ ->
      let width, height = current_dimensions t in
      run_draw t ~width ~height

let mark_layout_dirty t =
  match Renderable.mark_layout_dirty t.node with Ok () -> () | Error _ -> ()

let set_intrinsic_size t ~width ~height =
  t.intrinsic_width <- Some (max 1 width);
  t.intrinsic_height <- Some (max 1 height);
  mark_layout_dirty t;
  request_render t

let clear_intrinsic_size t =
  t.intrinsic_width <- None;
  t.intrinsic_height <- None;
  mark_layout_dirty t;
  request_render t

let measure t ~(known_dimensions : float option Toffee.Geometry.Size.t)
    ~(available_space : Toffee.Available_space.t Toffee.Geometry.Size.t)
    ~style:_ =
  let open Toffee.Geometry.Size in
  let resolve_dimension known available intrinsic =
    match known with
    | Some value when value > 0. -> value
    | _ -> (
        match Toffee.Available_space.to_option available with
        | Some value when value > 0. -> value
        | _ -> ( match intrinsic with Some v when v > 0 -> float v | _ -> 1.))
  in
  {
    width =
      resolve_dimension known_dimensions.width available_space.width
        t.intrinsic_width;
    height =
      resolve_dimension known_dimensions.height available_space.height
        t.intrinsic_height;
  }

let render_canvas t renderable grid ~delta:_ =
  let lx = Renderable.x renderable in
  let ly = Renderable.y renderable in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  if lw <= 0 || lh <= 0 then ()
  else (
    resize_surface_to_layout t;
    (match t.draw with None -> () | Some _ -> run_draw t ~width:lw ~height:lh);
    let src = t.surface in
    let blit_w = min lw (Grid.width src) in
    let blit_h = min lh (Grid.height src) in
    if blit_w > 0 && blit_h > 0 then
      let rect = Grid.{ x = lx; y = ly; width = blit_w; height = blit_h } in
      Grid.clip grid rect (fun () ->
          Grid.blit_region ~src ~dst:grid ~src_x:0 ~src_y:0 ~width:blit_w
            ~height:blit_h ~dst_x:lx ~dst_y:ly))

let set_on_resize t callback = t.on_resize <- callback

let mount ?(props = Props.default) node =
  let iw = max 0 props.initial_width in
  let ih = max 0 props.initial_height in
  let glyph_pool = Renderable.Internal.glyph_pool node in
  let surface =
    Grid.create ~width:(max 1 iw) ~height:(max 1 ih) ?glyph_pool
      ?width_method:props.width_method ~respect_alpha:props.respect_alpha ()
  in
  let canvas =
    {
      node;
      surface;
      intrinsic_width = (if iw > 0 then Some iw else None);
      intrinsic_height = (if ih > 0 then Some ih else None);
      draw = None;
      on_resize = None;
    }
  in
  Renderable.set_render node (render_canvas canvas);
  Renderable.set_measure node (Some (measure canvas));
  Renderable.set_on_size_change node
    (Some
       (fun _ ->
         resize_surface_to_layout canvas;
         request_render canvas;
         (* Invoke user's on_resize callback if set *)
         match canvas.on_resize with
         | Some cb ->
             let width = Renderable.width canvas.node in
             let height = Renderable.height canvas.node in
             if width > 0 && height > 0 then cb ~width ~height
         | None -> ()));
  request_render canvas;
  canvas

let apply_props t (props : Props.t) =
  (* Canvas props are effectively creation-time only for surface size and
     width_method; updating them at runtime would require reallocating the
     underlying grid. For now, we treat apply_props as a no-op beyond updating
     respect_alpha and draw callback. *)
  if not (Bool.equal props.respect_alpha (Grid.respect_alpha t.surface)) then
    Grid.set_respect_alpha t.surface props.respect_alpha
(* We intentionally do not change width_method or intrinsic dimensions here to
   avoid reallocations; callers should recreate the canvas if needed. *)
