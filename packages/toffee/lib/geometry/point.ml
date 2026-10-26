type 'a t = { x : 'a; y : 'a }

let none = { x = None; y = None }

(* Creation *)

let make x y = { x; y }

(* Mapping *)

let map f point = { x = f point.x; y = f point.y }
let map2 f p1 p2 = { x = f p1.x p2.x; y = f p1.y p2.y }

(* Axis accessors *)

let get axis point =
  match axis with
  | Abstract_axis.Inline -> point.x
  | Abstract_axis.Block -> point.y

let set axis value point =
  match axis with
  | Abstract_axis.Inline -> { point with x = value }
  | Abstract_axis.Block -> { point with y = value }

(* Transformations *)

let transpose point = { x = point.y; y = point.x }

(* Conversions *)

let to_size point = { Size.width = point.x; height = point.y }
let of_size size = { x = size.Size.width; y = size.Size.height }

(* Comparison and string functions *)

let compare cmp a b =
  let cmp_x = cmp a.x b.x in
  if cmp_x <> 0 then cmp_x else cmp a.y b.y

let equal eq a b = eq a.x b.x && eq a.y b.y

let to_string f point =
  Printf.sprintf "{ x: %s; y: %s }" (f point.x) (f point.y)

let pp f fmt point = Format.fprintf fmt "{ x: %a; y: %a }" f point.x f point.y
