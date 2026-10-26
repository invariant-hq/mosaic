type t = { width : float; height : float }

(* Constants and creation *)

let zero = { width = 0.0; height = 0.0 }
let make width height = { width; height }
let square side = { width = side; height = side }

(* Arithmetic *)

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

(* Stdlib's max and min, on floats: the larger (smaller) of the two, the first
   when they compare equal. *)
let fmax (a : float) b = if a >= b then a else b
let fmin (a : float) b = if a <= b then a else b
let max a b = { width = fmax a.width b.width; height = fmax a.height b.height }
let min a b = { width = fmin a.width b.width; height = fmin a.height b.height }
let has_non_zero_area size = size.width > 0.0 && size.height > 0.0

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

(* Conversions *)

let of_size (size : float Size.t) = { width = size.width; height = size.height }
let to_size size : float Size.t = { width = size.width; height = size.height }

let some size : float option Size.t =
  { width = Some size.width; height = Some size.height }

(* With optional sizes *)

let unwrap_or alt (size : float option Size.t) =
  {
    width = (match size.width with Some w -> w | None -> alt.width);
    height = (match size.height with Some h -> h | None -> alt.height);
  }

let clamp (min : float option Size.t) (max : float option Size.t) size =
  let clamp value min max =
    match (min, max) with
    | Some min, Some max -> Float.max min (Float.min max value)
    | None, Some max -> Float.min max value
    | Some min, None -> Float.max min value
    | None, None -> value
  in
  {
    width = clamp size.width min.width max.width;
    height = clamp size.height min.height max.height;
  }

let min_or_self (optional : float option Size.t) size =
  {
    width =
      (match optional.width with
      | Some v -> Float.min size.width v
      | None -> size.width);
    height =
      (match optional.height with
      | Some v -> Float.min size.height v
      | None -> size.height);
  }

let max_or_self (optional : float option Size.t) size =
  {
    width =
      (match optional.width with
      | Some v -> Float.max size.width v
      | None -> size.width);
    height =
      (match optional.height with
      | Some v -> Float.max size.height v
      | None -> size.height);
  }

let add_or_zero (optional : float option Size.t) size =
  {
    width =
      (match optional.width with
      | Some v -> size.width +. v
      | None -> size.width);
    height =
      (match optional.height with
      | Some v -> size.height +. v
      | None -> size.height);
  }

let sub_or_zero (optional : float option Size.t) size =
  {
    width =
      (match optional.width with
      | Some v -> size.width -. v
      | None -> size.width);
    height =
      (match optional.height with
      | Some v -> size.height -. v
      | None -> size.height);
  }

let min_with_option size (optional : float option Size.t) : float option Size.t
    =
  {
    width =
      Some
        (match optional.width with
        | Some v -> Float.min size.width v
        | None -> size.width);
    height =
      Some
        (match optional.height with
        | Some v -> Float.min size.height v
        | None -> size.height);
  }

let max_with_option size (optional : float option Size.t) : float option Size.t
    =
  {
    width =
      Some
        (match optional.width with
        | Some v -> Float.max size.width v
        | None -> size.width);
    height =
      Some
        (match optional.height with
        | Some v -> Float.max size.height v
        | None -> size.height);
  }

let add_with_option size (optional : float option Size.t) : float option Size.t
    =
  {
    width =
      Some
        (match optional.width with
        | Some v -> size.width +. v
        | None -> size.width);
    height =
      Some
        (match optional.height with
        | Some v -> size.height +. v
        | None -> size.height);
  }

let sub_with_option size (optional : float option Size.t) : float option Size.t
    =
  {
    width =
      Some
        (match optional.width with
        | Some v -> size.width -. v
        | None -> size.width);
    height =
      Some
        (match optional.height with
        | Some v -> size.height -. v
        | None -> size.height);
  }

let maybe_add size (opt : float option Size.t) : float option Size.t =
  {
    width =
      (match opt.width with Some v -> Some (v +. size.width) | None -> None);
    height =
      (match opt.height with Some v -> Some (v +. size.height) | None -> None);
  }

let maybe_sub size (opt : float option Size.t) : float option Size.t =
  {
    width =
      (match opt.width with Some v -> Some (v -. size.width) | None -> None);
    height =
      (match opt.height with Some v -> Some (v -. size.height) | None -> None);
  }

let maybe_min size (opt : float option Size.t) : float option Size.t =
  {
    width =
      (match opt.width with
      | Some v -> Some (Float.min v size.width)
      | None -> None);
    height =
      (match opt.height with
      | Some v -> Some (Float.min v size.height)
      | None -> None);
  }

let maybe_max size (opt : float option Size.t) : float option Size.t =
  {
    width =
      (match opt.width with
      | Some v -> Some (Float.max v size.width)
      | None -> None);
    height =
      (match opt.height with
      | Some v -> Some (Float.max v size.height)
      | None -> None);
  }

(* Comparison and formatting *)

let equal a b = a.width = b.width && a.height = b.height

let compare a b =
  let c = Float.compare a.width b.width in
  if c <> 0 then c else Float.compare a.height b.height

let to_string size =
  Printf.sprintf "{ width: %s; height: %s }"
    (Float.to_string size.width)
    (Float.to_string size.height)

let pp fmt size = Format.pp_print_string fmt (to_string size)
