(* ───── Props ───── *)

type orientation = [ `Horizontal | `Vertical ]
type glyphs = { filled : string; half : string; empty : string }

let equal_glyphs a b =
  String.equal a.filled b.filled
  && String.equal a.half b.half
  && String.equal a.empty b.empty

(* A glyph is drawn in each cell of its part, so one wider than a cell
   would be cut by its neighbour's, and an empty one would show nothing:
   each must take one cell by every width method a screen may measure by. *)
let one_cell glyph =
  List.for_all
    (fun width_method ->
      Matrix.Text.measure ~width_method ~tab_width:2 glyph = 1)
    [ `Unicode; `Unicode_wide; `Wcwidth ]

let check_glyphs g =
  List.iter
    (fun glyph ->
      if not (one_cell glyph) then
        invalid_arg
          (Printf.sprintf "Progress_bar: glyph %S is not one cell wide" glyph))
    [ g.filled; g.half; g.empty ];
  g

(* [Props.make] is the one place the progress bar's labels get their
   defaults: [Mosaic.progress_bar] passes its labels on unset. *)
module Props = struct
  type t = {
    value : float;
    min : float;
    max : float;
    orientation : orientation;
    filled_color : Ansi.Color.t option;
    empty_color : Ansi.Color.t option;
    glyphs : glyphs option;
  }

  let make ?(value = 0.) ?(min = 0.) ?(max = 1.) ?(orientation = `Horizontal)
      ?filled_color ?empty_color ?glyphs () =
    let glyphs = Option.map check_glyphs glyphs in
    { value; min; max; orientation; filled_color; empty_color; glyphs }

  let default = make ()

  let equal a b =
    Float.equal a.value b.value
    && Float.equal a.min b.min && Float.equal a.max b.max
    && a.orientation = b.orientation
    && Option.equal Ansi.Color.equal a.filled_color b.filled_color
    && Option.equal Ansi.Color.equal a.empty_color b.empty_color
    && Option.equal equal_glyphs a.glyphs b.glyphs
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

(* Without given glyphs the bar draws full blocks over a line, and the
   half block of its orientation: the left half across, the lower half up. *)
let horizontal_blocks =
  { filled = "\u{2588}"; half = "\u{258C}"; empty = "\u{2500}" }

let vertical_blocks =
  { filled = "\u{2588}"; half = "\u{2584}"; empty = "\u{2502}" }

(* [texts grid style text ~x ~y ~width ~height] draws the one-cell [text] in
   each cell of the rectangle in [style], over the cells' backgrounds. A
   glyph of one scalar, as most are, is decoded once and written as it; a
   longer cluster is drawn as text, measured in each cell. *)
let texts grid (style : Ansi.Style.t) text ~x ~y ~width ~height =
  let d = String.get_utf_8_uchar text 0 in
  if Uchar.utf_decode_length d = String.length text then
    let uchar = Uchar.utf_decode_uchar d in
    let fg = Option.value style.fg ~default:Ansi.Color.default_fg in
    let bg = Option.value style.bg ~default:Ansi.Color.transparent in
    for row = y to y + height - 1 do
      for col = x to x + width - 1 do
        Matrix_grid.set_uchar ~blend:true grid ~x:col ~y:row ~uchar ~fg ~bg
          ~attrs:style.attrs ()
      done
    done
  else
    for row = y to y + height - 1 do
      for col = x to x + width - 1 do
        Matrix_grid.draw_text ~style grid ~x:col ~y:row ~text
      done
    done

(* Each part draws its glyph in its role, [Plain] filled and [Border]
   empty, so the bar shows its value whatever the styling. A color given
   is its part's foreground, and the half glyph is the filled part's. *)
let paint t ~blocks grid ~x ~y ~width ~height ~filled ~half =
  let g = Option.value t.props.glyphs ~default:blocks in
  let theme = Renderable.theme t.node in
  let style role color =
    let s = Theme.style theme role in
    match color with Some c -> Ansi.Style.fg c s | None -> s
  in
  let filled_style = style Plain t.props.filled_color in
  texts grid (style Border t.props.empty_color) g.empty ~x ~y ~width ~height;
  texts grid filled_style g.filled ~x:filled.Matrix_grid.x ~y:filled.y
    ~width:filled.width ~height:filled.height;
  Option.iter
    (fun (x, y, width, height) ->
      texts grid filled_style g.half ~x ~y ~width ~height)
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
  paint t ~blocks:horizontal_blocks grid ~x ~y ~width:w ~height:h
    ~filled:{ Matrix_grid.x; y; width = full_cells; height = h }
    ~half:
      (if has_half && full_cells < w then Some (x + full_cells, y, 1, h)
       else None)

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
  paint t ~blocks:vertical_blocks grid ~x ~y ~width:w ~height:h
    ~filled:
      { Matrix_grid.x; y = y + h - full_cells; width = w; height = full_cells }
    ~half:
      (if has_half && full_cells < h then Some (x, y + h - full_cells - 1, w, 1)
       else None)

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    match t.props.orientation with
    | `Horizontal -> render_horizontal t grid
    | `Vertical -> render_vertical t grid

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    (props : Props.t) =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* It acts on a press, or paints no source text: a press on it neither
     starts nor clears a text selection. *)
  Renderable.set_selectable node (Some `Off);
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

let set_glyphs t g =
  let g = Option.map check_glyphs g in
  if not (Option.equal equal_glyphs t.props.glyphs g) then (
    t.props <- { t.props with glyphs = g };
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
