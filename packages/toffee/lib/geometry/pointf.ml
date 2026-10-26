type t = { x : float; y : float }

let zero = { x = 0.0; y = 0.0 }
let make x y = { x; y }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let ( + ) = add
let ( - ) = sub
let mul_scalar point scalar = { x = point.x *. scalar; y = point.y *. scalar }
let div_scalar point scalar = { x = point.x /. scalar; y = point.y /. scalar }
let ( * ) = mul_scalar
let ( / ) = div_scalar

let get axis point =
  match axis with
  | Abstract_axis.Inline -> point.x
  | Abstract_axis.Block -> point.y

let of_point (point : float Point.t) = { x = point.x; y = point.y }
let to_point point : float Point.t = { x = point.x; y = point.y }
let equal a b = a.x = b.x && a.y = b.y

let compare a b =
  let c = Float.compare a.x b.x in
  if c <> 0 then c else Float.compare a.y b.y

let to_string point =
  Printf.sprintf "{ x: %s; y: %s }" (Float.to_string point.x)
    (Float.to_string point.y)

let pp fmt point = Format.pp_print_string fmt (to_string point)
