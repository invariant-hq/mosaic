(* ───── Points And Bounds ───── *)

type point = { x : int; y : int }

let pp_point ppf p = Format.fprintf ppf "(%d, %d)" p.x p.y
let equal_point a b = a.x = b.x && a.y = b.y

type bounds = { x : int; y : int; width : int; height : int }

let pp_bounds ppf b =
  Format.fprintf ppf "{x=%d; y=%d; w=%d; h=%d}" b.x b.y b.width b.height

let equal_bounds a b =
  a.x = b.x && a.y = b.y && a.width = b.width && a.height = b.height

type local_bounds = { anchor : point; focus : point }

let pp_local_bounds ppf lb =
  Format.fprintf ppf "{anchor=%a; focus=%a}" pp_point lb.anchor pp_point
    lb.focus

let equal_local_bounds a b =
  equal_point a.anchor b.anchor && equal_point a.focus b.focus

(* ───── Selections ───── *)

(* anchor_position is a thunk rather than a plain point so that scrollable
   containers can install a callback that recomputes the anchor relative to the
   current scroll offset. set_anchor replaces it with a constant. *)
type t = {
  mutable anchor_position : unit -> point;
  mutable focus : point;
  mutable is_active : bool;
  mutable is_dragging : bool;
  mutable is_start : bool;
}

let pp ppf t =
  Format.fprintf ppf "Selection(anchor=%a, focus=%a, active=%b, dragging=%b)"
    pp_point (t.anchor_position ()) pp_point t.focus t.is_active t.is_dragging

let create ?anchor_position ~anchor ~focus () =
  let anchor_position =
    Option.value anchor_position ~default:(fun () -> anchor)
  in
  {
    anchor_position;
    focus;
    is_active = true;
    is_dragging = true;
    is_start = true;
  }

(* ───── Position ───── *)

let anchor t = t.anchor_position ()
let focus t = t.focus
let set_anchor t p = t.anchor_position <- (fun () -> p)
let set_focus t p = t.focus <- p

let bounds t =
  let a = anchor t and f = focus t in
  let x0 = min a.x f.x and y0 = min a.y f.y in
  let x1 = max a.x f.x and y1 = max a.y f.y in
  { x = x0; y = y0; width = x1 - x0 + 1; height = y1 - y0 + 1 }

(* ───── State ───── *)

let is_active t = t.is_active
let set_is_active t v = t.is_active <- v
let is_dragging t = t.is_dragging
let set_is_dragging t v = t.is_dragging <- v
let is_start t = t.is_start
let set_is_start t v = t.is_start <- v

(* ───── Coordinate Transformation ───── *)

let to_local t ~(origin : point) =
  let a = anchor t and f = focus t in
  {
    anchor = { x = a.x - origin.x; y = a.y - origin.y };
    focus = { x = f.x - origin.x; y = f.y - origin.y };
  }
