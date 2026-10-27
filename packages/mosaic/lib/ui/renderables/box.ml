(* Props *)

module Lp = Toffee.Style.Length_percentage

let cells_of_border_width w =
  let cells = Lp.value w in
  if Lp.is_length w && Float.is_integer cells && cells >= 0. then
    Float.to_int cells
  else
    invalid_arg
      (Format.asprintf "Box: border width %a is not a whole number of cells"
         Lp.pp w)

(* [Props.make] is the one place the box's labels get their defaults.
   [create] and the element ([Vnode.box], which [Mosaic.box] and the MLX box
   call) pass their labels on unset, and the reconciler applies the props
   the element made with [apply_props]. *)
module Props = struct
  type t = {
    border : bool;
    border_style : Matrix_grid.Border.t;
    border_sides : Matrix_grid.Border.side list;
    border_width : int Toffee.Geometry.Rect.t option;
    border_color : Ansi.Color.t option;
    focused_border_color : Ansi.Color.t option;
    background : Ansi.Color.t option;
    fill : bool;
    title : string option;
    title_alignment : [ `Left | `Center | `Right ];
    inert : bool;
  }

  let make ?(border = false) ?(border_style = Matrix_grid.Border.single)
      ?(border_sides = Matrix_grid.Border.all) ?border_width ?border_color
      ?focused_border_color ?background ?(fill = true) ?title
      ?(title_alignment = `Left) ?(inert = false) () =
    let border_width =
      Option.map (Toffee.Geometry.Rect.map cells_of_border_width) border_width
    in
    let has_border_opts =
      Option.is_some focused_border_color
      || border_style <> Matrix_grid.Border.single
      || Option.is_some border_color
    in
    let border = border || has_border_opts in
    {
      border;
      border_style;
      border_sides;
      border_width;
      border_color;
      focused_border_color;
      background;
      fill;
      title;
      title_alignment;
      inert;
    }

  let default = make ()
  let inert p = p.inert

  let equal a b =
    a.border = b.border
    && a.border_style = b.border_style
    && a.border_sides = b.border_sides
    && a.border_width = b.border_width
    && Option.equal Ansi.Color.equal a.border_color b.border_color
    && Option.equal Ansi.Color.equal a.focused_border_color
         b.focused_border_color
    && Option.equal Ansi.Color.equal a.background b.background
    && a.fill = b.fill && a.title = b.title
    && a.title_alignment = b.title_alignment
    && a.inert = b.inert
end

(* Types *)

type t = { node : Renderable.t; mutable props : Props.t }

let node t = t.node

(* Border geometry *)

(* The cells a side's border takes: its border width, one cell by default,
   while the box has a border on that side, and none otherwise, as a CSS
   border whose style is none has width 0. A side of width 0 draws no line. *)
let side_width t side =
  if t.props.border && List.mem side t.props.border_sides then
    match t.props.border_width with
    | None -> 1
    | Some r -> (
        match side with
        | `Top -> r.top
        | `Right -> r.right
        | `Bottom -> r.bottom
        | `Left -> r.left)
  else 0

let drawn_sides t =
  List.filter (fun s -> side_width t s > 0) t.props.border_sides

(* Child clipping *)

let child_clip t _node =
  let x = Renderable.x t.node and y = Renderable.y t.node in
  let w = Renderable.width t.node and h = Renderable.height t.node in
  let top = side_width t `Top and right = side_width t `Right in
  let bottom = side_width t `Bottom and left = side_width t `Left in
  Some
    {
      Matrix_grid.x = x + left;
      y = y + top;
      width = max 0 (w - left - right);
      height = max 0 (h - top - bottom);
    }

(* Border style *)

let style_with_border t style =
  let width side = Lp.length (Float.of_int (side_width t side)) in
  Toffee.Style.set_border
    {
      Toffee.Geometry.Rect.top = width `Top;
      right = width `Right;
      bottom = width `Bottom;
      left = width `Left;
    }
    style

let apply_border_style t =
  let updated = style_with_border t (Renderable.style t.node) in
  Renderable.set_style t.node updated

(* Rendering *)

let transparent = Ansi.Color.of_rgba 0 0 0 0

(* The border's style: the [Border] role, or [border_color] over it. While
   focused, [focused_border_color] where given, and otherwise the role in
   bold, drawn in heavy lines ([border_lines]), so focus shows as a glyph
   with no styling and also as an attribute that changes no color. A style
   without a background leaves the border's cells transparent, as the
   unfilled inside. The unfocused border without a label allocates nothing. *)
let border_style t =
  let role = Theme.style (Renderable.theme t.node) Border in
  let style =
    match t.props.border_color with
    | Some c -> Ansi.Style.fg c role
    | None -> role
  in
  if not (Renderable.focused t.node) then style
  else
    match t.props.focused_border_color with
    | Some c -> Ansi.Style.fg c style
    | None -> Ansi.Style.with_bold true style

let border_lines t =
  if Renderable.focused t.node && Option.is_none t.props.focused_border_color
  then Matrix_grid.Border.heavy
  else t.props.border_style

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    let bg_color = Option.value t.props.background ~default:transparent in
    let fill = if t.props.fill then Some bg_color else None in
    Matrix_grid.draw_box grid ~x:(Renderable.x t.node) ~y:(Renderable.y t.node)
      ~width:w ~height:h ~border:(border_lines t) ~sides:(drawn_sides t)
      ~style:(border_style t) ?fill ?title:t.props.title
      ~title_alignment:t.props.title_alignment ()

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?border
    ?border_style ?border_sides ?border_width ?border_color
    ?focused_border_color ?background ?fill ?title ?title_alignment ?inert () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?border ?border_style ?border_sides ?border_width ?border_color
      ?focused_border_color ?background ?fill ?title ?title_alignment ?inert ()
  in
  let t = { node; props } in
  Renderable.set_inert node props.inert;
  Renderable.set_render node (render t);
  Renderable.set_child_clip node (Some (child_clip t));
  apply_border_style t;
  t

(* Setters *)

let request_render t = Renderable.request_render t.node

let ensure_border_enabled t =
  if not t.props.border then (
    t.props <- { t.props with border = true };
    apply_border_style t)

let set_border t v =
  if t.props.border <> v then (
    t.props <- { t.props with border = v };
    apply_border_style t;
    request_render t)

let set_border_style t chars =
  if t.props.border_style <> chars then (
    t.props <- { t.props with border_style = chars };
    ensure_border_enabled t;
    request_render t)

let set_border_sides t sides =
  if t.props.border_sides <> sides then (
    t.props <- { t.props with border_sides = sides };
    apply_border_style t;
    request_render t)

let set_border_color t color =
  if not (Option.equal Ansi.Color.equal t.props.border_color (Some color)) then (
    t.props <- { t.props with border_color = Some color };
    ensure_border_enabled t;
    request_render t)

let set_focused_border_color t color =
  if not (Option.equal Ansi.Color.equal t.props.focused_border_color color) then (
    t.props <- { t.props with focused_border_color = color };
    if Option.is_some color then ensure_border_enabled t;
    if Renderable.focused t.node then request_render t)

let set_background t color =
  if not (Option.equal Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    request_render t)

let set_fill t v =
  if t.props.fill <> v then (
    t.props <- { t.props with fill = v };
    request_render t)

let set_title t text =
  if t.props.title <> text then (
    t.props <- { t.props with title = text };
    request_render t)

let set_title_alignment t align =
  if t.props.title_alignment <> align then (
    t.props <- { t.props with title_alignment = align };
    request_render t)

let set_style t style =
  let updated = style_with_border t style in
  Renderable.set_style t.node updated

(* Apply props *)

let apply_props t props =
  t.props <- props;
  Renderable.set_inert t.node props.inert;
  apply_border_style t;
  request_render t

(* Pretty-printing *)

let pp ppf t =
  Format.fprintf ppf "Box(%s" (Renderable.id t.node);
  if t.props.border then Format.pp_print_string ppf ", border";
  if Option.is_some t.props.background then Format.pp_print_string ppf ", bg";
  Option.iter (Format.fprintf ppf ", title=%S") t.props.title;
  Format.pp_print_char ppf ')'
