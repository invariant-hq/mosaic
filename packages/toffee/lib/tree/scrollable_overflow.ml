module Rectf = Toffee_geometry.Rectf

(* The rectangle with its right and bottom edges carrying, in their sign,
   whether the scroll origin is reversed in that axis: the rectangle contains
   the origin, so the edges' magnitudes are never negative. *)
type t = Rectf.t

let zero = Rectf.zero

let make (rect : Rectf.t) ~reversed_x ~reversed_y =
  if
    Bool.equal (Float.sign_bit rect.right) reversed_x
    && Bool.equal (Float.sign_bit rect.bottom) reversed_y
  then rect
  else
    {
      rect with
      right = Float.copy_sign rect.right (if reversed_x then -1.0 else 1.0);
      bottom = Float.copy_sign rect.bottom (if reversed_y then -1.0 else 1.0);
    }

let left (t : t) = t.left [@@inline]
let right (t : t) = Float.abs t.right [@@inline]
let top (t : t) = t.top [@@inline]
let bottom (t : t) = Float.abs t.bottom [@@inline]
let reversed_x (t : t) = Float.sign_bit t.right [@@inline]
let reversed_y (t : t) = Float.sign_bit t.bottom [@@inline]
let packed (t : t) : Rectf.t = t

let rect (t : t) : Rectf.t =
  if Float.sign_bit t.right || Float.sign_bit t.bottom then
    { t with right = Float.abs t.right; bottom = Float.abs t.bottom }
  else t

let equal (a : t) (b : t) =
  Rectf.equal a b
  && Bool.equal (reversed_x a) (reversed_x b)
  && Bool.equal (reversed_y a) (reversed_y b)

let compare (a : t) (b : t) =
  let c = Rectf.compare (rect a) (rect b) in
  if c <> 0 then c
  else
    let c = Bool.compare (reversed_x a) (reversed_x b) in
    if c <> 0 then c else Bool.compare (reversed_y a) (reversed_y b)

let to_string t =
  Printf.sprintf "%s%s%s"
    (Rectf.to_string (rect t))
    (if reversed_x t then " reversed-x" else "")
    (if reversed_y t then " reversed-y" else "")
