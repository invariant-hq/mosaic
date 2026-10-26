type t = { left : float; right : float; top : float; bottom : float }

let zero = { left = 0.0; right = 0.0; top = 0.0; bottom = 0.0 }
let make ~left ~right ~top ~bottom = { left; right; top; bottom }
let all value = { left = value; right = value; top = value; bottom = value }

let add a b =
  {
    left = a.left +. b.left;
    right = a.right +. b.right;
    top = a.top +. b.top;
    bottom = a.bottom +. b.bottom;
  }

let ( + ) = add
let horizontal_axis_sum rect = rect.left +. rect.right
let vertical_axis_sum rect = rect.top +. rect.bottom

let sum_axes rect =
  Sizef.{ width = horizontal_axis_sum rect; height = vertical_axis_sum rect }

let grid_axis_sum axis rect =
  match axis with
  | Absolute_axis.Horizontal -> rect.left +. rect.right
  | Absolute_axis.Vertical -> rect.top +. rect.bottom

let of_rect (rect : float Rect.t) =
  { left = rect.left; right = rect.right; top = rect.top; bottom = rect.bottom }

let to_rect rect : float Rect.t =
  { left = rect.left; right = rect.right; top = rect.top; bottom = rect.bottom }

let equal a b =
  a.left = b.left && a.right = b.right && a.top = b.top && a.bottom = b.bottom

let compare a b =
  let c = Float.compare a.left b.left in
  if c <> 0 then c
  else
    let c = Float.compare a.right b.right in
    if c <> 0 then c
    else
      let c = Float.compare a.top b.top in
      if c <> 0 then c else Float.compare a.bottom b.bottom

let to_string rect =
  Printf.sprintf "{ left: %s; right: %s; top: %s; bottom: %s }"
    (Float.to_string rect.left)
    (Float.to_string rect.right)
    (Float.to_string rect.top)
    (Float.to_string rect.bottom)

let pp fmt rect = Format.pp_print_string fmt (to_string rect)
