(* ───── Defaults ───── *)

let default_filled_color = Ansi.Color.of_rgb 154 158 163
let default_empty_color = Ansi.Color.of_rgb 37 37 39

(* ───── Props ───── *)

type orientation = [ `Horizontal | `Vertical ]

module Props = struct
  type t = {
    value : float;
    min : float;
    max : float;
    orientation : orientation;
    filled_color : Ansi.Color.t;
    empty_color : Ansi.Color.t;
  }

  let make ?(value = 0.) ?(min = 0.) ?(max = 1.) ?(orientation = `Horizontal)
      ?(filled_color = default_filled_color)
      ?(empty_color = default_empty_color) () =
    { value; min; max; orientation; filled_color; empty_color }

  let default = make ()

  let equal a b =
    Float.equal a.value b.value
    && Float.equal a.min b.min && Float.equal a.max b.max
    && a.orientation = b.orientation
    && Ansi.Color.equal a.filled_color b.filled_color
    && Ansi.Color.equal a.empty_color b.empty_color
end

(* ───── Types ───── *)

type t = { node : Renderable.t; mutable props : Props.t }

let node t = t.node

(* ───── Ratio Computation ───── *)

let clamp_ratio props =
  let range = props.Props.max -. props.Props.min in
  if Float.equal range 0. then 1.
  else
    let clamped = Float.max props.min (Float.min props.max props.value) in
    (clamped -. props.min) /. range

(* ───── Rendering ───── *)

let render_horizontal t grid =
  let x = Renderable.x t.node in
  let y = Renderable.y t.node in
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  let ratio = clamp_ratio t.props in
  let virtual_track = w * 2 in
  let virtual_filled =
    int_of_float (Float.round (ratio *. Float.of_int virtual_track))
  in
  Grid.fill_rect grid ~x ~y ~width:w ~height:h ~color:t.props.empty_color;
  let full_cells = virtual_filled / 2 in
  let has_half = virtual_filled mod 2 = 1 in
  if full_cells > 0 then
    Grid.fill_rect grid ~x ~y ~width:full_cells ~height:h
      ~color:t.props.filled_color;
  if has_half && full_cells < w then
    let cell = Grid.Cell.of_uchar (Uchar.of_int 0x258C) in
    for row = 0 to h - 1 do
      Grid.set_cell ~blend:true grid ~x:(x + full_cells) ~y:(y + row) ~cell
        ~fg:t.props.filled_color ~bg:t.props.empty_color ~attrs:Ansi.Attr.empty
        ()
    done

let render_vertical t grid =
  let x = Renderable.x t.node in
  let y = Renderable.y t.node in
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  let ratio = clamp_ratio t.props in
  let virtual_track = h * 2 in
  let virtual_filled =
    int_of_float (Float.round (ratio *. Float.of_int virtual_track))
  in
  Grid.fill_rect grid ~x ~y ~width:w ~height:h ~color:t.props.empty_color;
  let full_cells = virtual_filled / 2 in
  let has_half = virtual_filled mod 2 = 1 in
  if full_cells > 0 then
    Grid.fill_rect grid ~x
      ~y:(y + h - full_cells)
      ~width:w ~height:full_cells ~color:t.props.filled_color;
  if has_half && full_cells < h then
    let boundary_y = y + h - full_cells - 1 in
    let cell = Grid.Cell.of_uchar (Uchar.of_int 0x2584) in
    for col = 0 to w - 1 do
      Grid.set_cell ~blend:true grid ~x:(x + col) ~y:boundary_y ~cell
        ~fg:t.props.filled_color ~bg:t.props.empty_color ~attrs:Ansi.Attr.empty
        ()
    done

let render t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    match t.props.orientation with
    | `Horizontal -> render_horizontal t grid
    | `Vertical -> render_vertical t grid

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?(value = 0.)
    ?(min = 0.) ?(max = 1.) ?(orientation = `Horizontal)
    ?(filled_color = default_filled_color) ?(empty_color = default_empty_color)
    () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ~value ~min ~max ~orientation ~filled_color ~empty_color ()
  in
  let t = { node; props } in
  Renderable.set_render node (render t);
  t

(* ───── Accessors ───── *)

let value t = t.props.value
let min t = t.props.min
let max t = t.props.max

(* ───── Setters ───── *)

let request_render t = Renderable.request_render t.node

let set_value t v =
  if not (Float.equal t.props.value v) then (
    t.props <- { t.props with value = v };
    request_render t)

let set_min t v =
  if not (Float.equal t.props.min v) then (
    t.props <- { t.props with min = v };
    request_render t)

let set_max t v =
  if not (Float.equal t.props.max v) then (
    t.props <- { t.props with max = v };
    request_render t)

let set_orientation t o =
  if t.props.orientation <> o then (
    t.props <- { t.props with orientation = o };
    request_render t)

let set_filled_color t c =
  if not (Ansi.Color.equal t.props.filled_color c) then (
    t.props <- { t.props with filled_color = c };
    request_render t)

let set_empty_color t c =
  if not (Ansi.Color.equal t.props.empty_color c) then (
    t.props <- { t.props with empty_color = c };
    request_render t)

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  t.props <- props;
  request_render t

(* ───── Pretty-printing ───── *)

let pp ppf t =
  let orient =
    match t.props.orientation with `Horizontal -> "h" | `Vertical -> "v"
  in
  Format.fprintf ppf "Progress_bar(%s, %s, %.1f/[%.1f..%.1f])"
    (Renderable.id t.node) orient t.props.value t.props.min t.props.max
