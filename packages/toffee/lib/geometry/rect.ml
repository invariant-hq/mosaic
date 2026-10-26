type 'a t = { left : 'a; right : 'a; top : 'a; bottom : 'a }

let all value = { left = value; right = value; top = value; bottom = value }

(* Creation *)

let make ~left ~right ~top ~bottom = { left; right; top; bottom }

(* Mapping functions *)

let map f rect =
  {
    left = f rect.left;
    right = f rect.right;
    top = f rect.top;
    bottom = f rect.bottom;
  }

let map2 f r1 r2 =
  {
    left = f r1.left r2.left;
    right = f r1.right r2.right;
    top = f r1.top r2.top;
    bottom = f r1.bottom r2.bottom;
  }

let zip_size size f rect =
  {
    left = f rect.left size.Size.width;
    right = f rect.right size.Size.width;
    top = f rect.top size.height;
    bottom = f rect.bottom size.height;
  }

(* Component extraction *)

let horizontal_components rect = Line.{ start = rect.left; end_ = rect.right }
let vertical_components rect = Line.{ start = rect.top; end_ = rect.bottom }

(* Comparison and string functions *)

let compare cmp a b =
  let cmp_left = cmp a.left b.left in
  if cmp_left <> 0 then cmp_left
  else
    let cmp_right = cmp a.right b.right in
    if cmp_right <> 0 then cmp_right
    else
      let cmp_top = cmp a.top b.top in
      if cmp_top <> 0 then cmp_top else cmp a.bottom b.bottom

let equal eq a b =
  eq a.left b.left && eq a.right b.right && eq a.top b.top
  && eq a.bottom b.bottom

let to_string f rect =
  Printf.sprintf "{ left: %s; right: %s; top: %s; bottom: %s }" (f rect.left)
    (f rect.right) (f rect.top) (f rect.bottom)

let pp f fmt rect =
  Format.fprintf fmt "{ left: %a; right: %a; top: %a; bottom: %a }" f rect.left
    f rect.right f rect.top f rect.bottom
