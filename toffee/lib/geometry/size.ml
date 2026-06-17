type 'a t = { width : 'a; height : 'a }

(* Constants *)

let zero = { width = 0.0; height = 0.0 }
let none = { width = None; height = None }

(* Creation *)

let make width height = { width; height }
let square side = { width = side; height = side }

(* Arithmetic operations *)

let add a b = { width = a.width +. b.width; height = a.height +. b.height }
let sub a b = { width = a.width -. b.width; height = a.height -. b.height }
let ( + ) = add
let ( - ) = sub

let mul_scalar size scalar =
  { width = size.width *. scalar; height = size.height *. scalar }

let div_scalar size scalar =
  { width = size.width /. scalar; height = size.height /. scalar }

let ( * ) = mul_scalar
let ( / ) = div_scalar

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

(* Float-specific functions *)

let max a b = { width = max a.width b.width; height = max a.height b.height }
let min a b = { width = min a.width b.width; height = min a.height b.height }
let has_non_zero_area size = size.width > 0.0 && size.height > 0.0
let equal a b = a.width = b.width && a.height = b.height

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

(* Concrete + Option → Option operations *)

let min_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (Float.min concrete.width opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (Float.min concrete.height opt_val)
      | None -> Some concrete.height);
  }

let max_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (Float.max concrete.width opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (Float.max concrete.height opt_val)
      | None -> Some concrete.height);
  }

let add_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (concrete.width +. opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (concrete.height +. opt_val)
      | None -> Some concrete.height);
  }

let sub_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (concrete.width -. opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (concrete.height -. opt_val)
      | None -> Some concrete.height);
  }

(* Concrete operations with optional parameters *)

let clamp min max size =
  {
    width =
      (match (min.width, max.width) with
      | Some min_val, Some max_val ->
          Float.max min_val (Float.min max_val size.width)
      | None, Some max_val -> Float.min max_val size.width
      | Some min_val, None -> Float.max min_val size.width
      | None, None -> size.width);
    height =
      (match (min.height, max.height) with
      | Some min_val, Some max_val ->
          Float.max min_val (Float.min max_val size.height)
      | None, Some max_val -> Float.min max_val size.height
      | Some min_val, None -> Float.max min_val size.height
      | None, None -> size.height);
  }

let min_or_self optional size =
  {
    width =
      (match optional.width with
      | Some val_ -> Float.min size.width val_
      | None -> size.width);
    height =
      (match optional.height with
      | Some val_ -> Float.min size.height val_
      | None -> size.height);
  }

let max_or_self optional size =
  {
    width =
      (match optional.width with
      | Some val_ -> Float.max size.width val_
      | None -> size.width);
    height =
      (match optional.height with
      | Some val_ -> Float.max size.height val_
      | None -> size.height);
  }

let add_or_zero optional size =
  {
    width =
      (match optional.width with
      | Some val_ -> size.width +. val_
      | None -> size.width);
    height =
      (match optional.height with
      | Some val_ -> size.height +. val_
      | None -> size.height);
  }

let sub_or_zero optional size =
  {
    width =
      (match optional.width with
      | Some val_ -> size.width -. val_
      | None -> size.width);
    height =
      (match optional.height with
      | Some val_ -> size.height -. val_
      | None -> size.height);
  }

let max_concrete a b =
  { width = Float.max a.width b.width; height = Float.max a.height b.height }

(* Option + Concrete -> Option operations *)
(* These implement Rust's MaybeMath for Option<f32> + f32 -> Option<f32> *)

let maybe_add concrete_size opt_size =
  {
    width = Option.map (fun v -> v +. concrete_size.width) opt_size.width;
    height = Option.map (fun v -> v +. concrete_size.height) opt_size.height;
  }

let maybe_sub concrete_size opt_size =
  {
    width = Option.map (fun v -> v -. concrete_size.width) opt_size.width;
    height = Option.map (fun v -> v -. concrete_size.height) opt_size.height;
  }

let maybe_min concrete_size opt_size =
  {
    width = Option.map (fun v -> Float.min v concrete_size.width) opt_size.width;
    height =
      Option.map (fun v -> Float.min v concrete_size.height) opt_size.height;
  }

let maybe_max concrete_size opt_size =
  {
    width = Option.map (fun v -> Float.max v concrete_size.width) opt_size.width;
    height =
      Option.map (fun v -> Float.max v concrete_size.height) opt_size.height;
  }
