(* Props *)

module Props = struct
  type t = {
    border : bool;
    border_style : Grid.Border.t;
    border_sides : Grid.Border.side list;
    border_color : Ansi.Color.t;
    focused_border_color : Ansi.Color.t option;
    background : Ansi.Color.t option;
    fill : bool;
    title : string option;
    title_alignment : [ `Left | `Center | `Right ];
  }

  let default_focused_border_color = Ansi.Color.Bright_cyan

  let make ?(border = false) ?(border_style = Grid.Border.single)
      ?(border_sides = Grid.Border.all) ?(border_color = Ansi.Color.White)
      ?focused_border_color ?background ?(fill = true) ?title
      ?(title_alignment = `Left) () =
    let has_border_opts =
      Option.is_some focused_border_color
      || border_style <> Grid.Border.single
      || border_color <> Ansi.Color.White
    in
    let border = border || has_border_opts in
    let focused_border_color =
      match focused_border_color with
      | Some _ -> focused_border_color
      | None -> Some default_focused_border_color
    in
    {
      border;
      border_style;
      border_sides;
      border_color;
      focused_border_color;
      background;
      fill;
      title;
      title_alignment;
    }

  let default = make ()

  let equal a b =
    a.border = b.border
    && a.border_style = b.border_style
    && a.border_sides = b.border_sides
    && Ansi.Color.equal a.border_color b.border_color
    && Option.equal Ansi.Color.equal a.focused_border_color
         b.focused_border_color
    && Option.equal Ansi.Color.equal a.background b.background
    && a.fill = b.fill && a.title = b.title
    && a.title_alignment = b.title_alignment
end

(* Types *)

type t = { node : Renderable.t; mutable props : Props.t }

let node t = t.node

(* Border geometry *)

let effective_sides t = if t.props.border then t.props.border_sides else []
let fold_sides t ~init ~f = List.fold_left f init (effective_sides t)

let calculate_insets t =
  fold_sides t ~init:(0, 0, 0, 0) ~f:(fun (top, right, bottom, left) -> function
    | `Top -> (1, right, bottom, left)
    | `Right -> (top, 1, bottom, left)
    | `Bottom -> (top, right, 1, left)
    | `Left -> (top, right, bottom, 1))

(* Child clipping *)

let child_clip t _node =
  let x = Renderable.x t.node and y = Renderable.y t.node in
  let w = Renderable.width t.node and h = Renderable.height t.node in
  let top, right, bottom, left = calculate_insets t in
  Some
    {
      Grid.x = x + left;
      y = y + top;
      width = max 0 (w - left - right);
      height = max 0 (h - top - bottom);
    }

(* Border style *)

let style_with_border t style =
  let module Lp = Toffee.Style.Length_percentage in
  let one = Lp.length 1. in
  let border_rect =
    fold_sides t ~init:(Toffee.Geometry.Rect.all Lp.zero)
      ~f:(fun rect -> function
      | `Top -> { rect with Toffee.Geometry.Rect.top = one }
      | `Right -> { rect with right = one }
      | `Bottom -> { rect with bottom = one }
      | `Left -> { rect with left = one })
  in
  Toffee.Style.set_border border_rect style

let apply_border_style t =
  let updated = style_with_border t (Renderable.style t.node) in
  Renderable.set_style t.node updated

(* Rendering *)

let transparent = Ansi.Color.of_rgba 0 0 0 0

let resolve_border_color t =
  match (Renderable.focused t.node, t.props.focused_border_color) with
  | true, Some c -> c
  | _ -> t.props.border_color

let render t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    let bg_color = Option.value t.props.background ~default:transparent in
    let fill = if t.props.fill then Some bg_color else None in
    let border_style =
      Ansi.Style.make ~fg:(resolve_border_color t) ~bg:transparent ()
    in
    Grid.draw_box grid ~x:(Renderable.x t.node) ~y:(Renderable.y t.node)
      ~width:w ~height:h ~border:t.props.border_style ~sides:(effective_sides t)
      ~style:border_style ?fill ?title:t.props.title
      ~title_alignment:t.props.title_alignment ()

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?border ?border_style ?border_sides ?border_color
      ?focused_border_color ?background ?fill ?title ?title_alignment ()
  in
  let t = { node; props } in
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
  if not (Ansi.Color.equal t.props.border_color color) then (
    t.props <- { t.props with border_color = color };
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
  apply_border_style t;
  request_render t

(* Pretty-printing *)

let pp ppf t =
  Format.fprintf ppf "Box(%s" (Renderable.id t.node);
  if t.props.border then Format.pp_print_string ppf ", border";
  if Option.is_some t.props.background then Format.pp_print_string ppf ", bg";
  Option.iter (Format.fprintf ppf ", title=%S") t.props.title;
  Format.pp_print_char ppf ')'
