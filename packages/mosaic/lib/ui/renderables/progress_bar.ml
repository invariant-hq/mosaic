(* ───── Props ───── *)

type orientation = [ `Horizontal | `Vertical ]

(* [Props.make] is the one place the progress bar's labels get their
   defaults: [create] and [Mosaic.progress_bar] pass their labels on unset. *)
module Props = struct
  type t = {
    value : float;
    min : float;
    max : float;
    orientation : orientation;
    filled_color : Ansi.Color.t option;
    empty_color : Ansi.Color.t option;
  }

  let make ?(value = 0.) ?(min = 0.) ?(max = 1.) ?(orientation = `Horizontal)
      ?filled_color ?empty_color () =
    { value; min; max; orientation; filled_color; empty_color }

  let default = make ()

  let equal a b =
    Float.equal a.value b.value
    && Float.equal a.min b.min && Float.equal a.max b.max
    && a.orientation = b.orientation
    && Option.equal Ansi.Color.equal a.filled_color b.filled_color
    && Option.equal Ansi.Color.equal a.empty_color b.empty_color
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

(* Labels first, then roles: a given color fills its part, as a background;
   without [empty_color] the empty part is a line in the [Border] role, and
   without [filled_color] the filled part is full blocks in the [Plain]
   role, so the bar shows its value whatever the styling. The cell the value
   ends in is a half block in the filled color over the empty one. *)
(* [cells grid style uchar ~x ~y ~width ~height] draws [uchar] in each cell
   of the rectangle in [style], over the cells' backgrounds. *)
let cells grid (style : Ansi.Style.t) uchar ~x ~y ~width ~height =
  let fg = Option.value style.fg ~default:Ansi.Color.default_fg in
  let bg = Option.value style.bg ~default:Ansi.Color.transparent in
  for row = y to y + height - 1 do
    for col = x to x + width - 1 do
      Matrix_grid.set_uchar ~blend:true grid ~x:col ~y:row ~uchar ~fg ~bg
        ~attrs:style.attrs ()
    done
  done

let paint t grid ~x ~y ~width ~height ~line ~filled ~full ~half ~half_cell =
  let theme = Renderable.theme t.node in
  let plain = Theme.style theme Plain in
  let empty_bg =
    match t.props.empty_color with
    | Some color ->
        Matrix_grid.fill_rect grid ~x ~y ~width ~height ~color;
        color
    | None ->
        cells grid (Theme.style theme Border) line ~x ~y ~width ~height;
        Ansi.Color.transparent
  in
  let filled_fg =
    match t.props.filled_color with
    | Some color ->
        if full then
          Matrix_grid.fill_rect grid ~x:filled.Matrix_grid.x ~y:filled.y
            ~width:filled.width ~height:filled.height ~color;
        color
    | None ->
        if full then
          cells grid plain (Uchar.of_int 0x2588) ~x:filled.x ~y:filled.y
            ~width:filled.width ~height:filled.height;
        Option.value plain.fg ~default:Ansi.Color.default_fg
  in
  Option.iter
    (fun (hx, hy, hw, hh) ->
      for row = hy to hy + hh - 1 do
        for col = hx to hx + hw - 1 do
          Matrix_grid.set_uchar ~blend:true grid ~x:col ~y:row ~uchar:half_cell
            ~fg:filled_fg ~bg:empty_bg ~attrs:plain.attrs ()
        done
      done)
    half

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
  let full_cells = virtual_filled / 2 in
  let has_half = virtual_filled mod 2 = 1 in
  paint t grid ~x ~y ~width:w ~height:h ~line:(Uchar.of_int 0x2500)
    ~filled:{ Matrix_grid.x; y; width = full_cells; height = h }
    ~full:(full_cells > 0)
    ~half:
      (if has_half && full_cells < w then Some (x + full_cells, y, 1, h)
       else None)
    ~half_cell:(Uchar.of_int 0x258C)

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
  let full_cells = virtual_filled / 2 in
  let has_half = virtual_filled mod 2 = 1 in
  paint t grid ~x ~y ~width:w ~height:h ~line:(Uchar.of_int 0x2502)
    ~filled:
      { Matrix_grid.x; y = y + h - full_cells; width = w; height = full_cells }
    ~full:(full_cells > 0)
    ~half:
      (if has_half && full_cells < h then Some (x, y + h - full_cells - 1, w, 1)
       else None)
    ~half_cell:(Uchar.of_int 0x2584)

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    match t.props.orientation with
    | `Horizontal -> render_horizontal t grid
    | `Vertical -> render_vertical t grid

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?value ?min ?max
    ?orientation ?filled_color ?empty_color () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?value ?min ?max ?orientation ?filled_color ?empty_color ()
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
  if not (Option.equal Ansi.Color.equal t.props.filled_color (Some c)) then (
    t.props <- { t.props with filled_color = Some c };
    request_render t)

let set_empty_color t c =
  if not (Option.equal Ansi.Color.equal t.props.empty_color (Some c)) then (
    t.props <- { t.props with empty_color = Some c };
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
