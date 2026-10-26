type 'a t = { width : 'a; height : 'a }

let none = { width = None; height = None }

(* Creation *)

let make width height = { width; height }
let square side = { width = side; height = side }

(* Mapping functions *)

let map f size = { width = f size.width; height = f size.height }
let map_width f size = { width = f size.width; height = size.height }
let map_height f size = { width = size.width; height = f size.height }

let zip_map other f size =
  { width = f size.width other.width; height = f size.height other.height }

let map2 f s1 s2 =
  { width = f s1.width s2.width; height = f s1.height s2.height }

(* Axis accessors *)

let get axis size =
  match axis with
  | Abstract_axis.Inline -> size.width
  | Abstract_axis.Block -> size.height

let set axis value size =
  match axis with
  | Abstract_axis.Inline -> { size with width = value }
  | Abstract_axis.Block -> { size with height = value }

let get_absolute axis size =
  match axis with
  | Absolute_axis.Horizontal -> size.width
  | Absolute_axis.Vertical -> size.height

(* Option-specific functions *)

let unwrap_or alt size =
  {
    width = (match size.width with Some w -> w | None -> alt.width);
    height = (match size.height with Some h -> h | None -> alt.height);
  }

let choose_first size alt =
  {
    width = (match size.width with Some _ as w -> w | None -> alt.width);
    height = (match size.height with Some _ as h -> h | None -> alt.height);
  }

let both_axis_defined size =
  match (size.width, size.height) with Some _, Some _ -> true | _ -> false

(* Utility operations *)

let apply_aspect_ratio aspect_ratio size =
  match aspect_ratio with
  | None -> size
  | Some ratio -> (
      match (size.width, size.height) with
      | Some width, None ->
          { width = Some width; height = Some (width /. ratio) }
      | None, Some height ->
          { width = Some (height *. ratio); height = Some height }
      | _ -> size)

(* Comparison and string functions *)

let compare cmp a b =
  let cmp_width = cmp a.width b.width in
  if cmp_width <> 0 then cmp_width else cmp a.height b.height

let equal_with eq a b = eq a.width b.width && eq a.height b.height

let to_string f size =
  Printf.sprintf "{ width: %s; height: %s }" (f size.width) (f size.height)

let pp f fmt size =
  Format.fprintf fmt "{ width: %a; height: %a }" f size.width f size.height

(* Operations with optional values - implementing Taffy's MaybeMath patterns *)

(* Option → Option operations *)

let min_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (Float.min l r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (Float.min l r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let max_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (Float.max l r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (Float.max l r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let add_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (l +. r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (l +. r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let sub_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (l -. r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (l -. r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let clamp_option min max size =
  {
    width =
      (match (size.width, min.width, max.width) with
      | Some base, Some min, Some max ->
          Some (Float.max min (Float.min max base))
      | Some base, None, Some max -> Some (Float.min max base)
      | Some base, Some min, None -> Some (Float.max min base)
      | Some _, None, None -> size.width
      | None, _, _ -> None);
    height =
      (match (size.height, min.height, max.height) with
      | Some base, Some min, Some max ->
          Some (Float.max min (Float.min max base))
      | Some base, None, Some max -> Some (Float.min max base)
      | Some base, Some min, None -> Some (Float.max min base)
      | Some _, None, None -> size.height
      | None, _, _ -> None);
  }
