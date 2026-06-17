(* available_space.ml *)

(* Style type for representing available space as a sizing constraint *)

type t =
  | Definite of float
    (* The amount of space available is the specified number of pixels *)
  | Min_content
    (* The amount of space available is indefinite and the node should be laid
       out under a min-content constraint *)
  | Max_content
(* The amount of space available is indefinite and the node should be laid out
   under a max-content constraint *)

let zero = Definite 0.0
let max_content = Max_content
let min_content = Min_content
let of_length value = Definite value
let is_definite = function Definite _ -> true | _ -> false
let to_option = function Definite value -> Some value | _ -> None

let unwrap_or t default =
  match to_option t with Some value -> value | None -> default

let unwrap t =
  match to_option t with
  | Some value -> value
  | None -> invalid_arg "unwrap called on non-definite Available_space.t"

let or_ t default = match t with Definite _ -> t | _ -> default
let or_else t default_cb = match t with Definite _ -> t | _ -> default_cb ()

let unwrap_or_else t default_cb =
  match to_option t with Some value -> value | None -> default_cb ()

let set_or_self t value =
  match value with Some value -> Definite value | None -> t

let map_definite_value t map_function =
  match t with Definite value -> Definite (map_function value) | _ -> t

let compute_free_space t used_space =
  match t with
  | Max_content -> Float.infinity
  | Min_content -> 0.0
  | Definite available_space -> available_space -. used_space

let is_roughly_equal t other =
  match (t, other) with
  | Definite a, Definite b -> Float.abs (a -. b) < Float.epsilon
  | Min_content, Min_content -> true
  | Max_content, Max_content -> true
  | _ -> false

let of_float value = Definite value

let of_option option =
  match option with Some value -> Definite value | None -> Max_content

type size = { width : t; height : t }

let size_to_options (t : size) : float option Geometry.size =
  { width = to_option t.width; height = to_option t.height }

let size_maybe_set (t : size) (value : float option Geometry.size) : size =
  {
    width = set_or_self t.width value.width;
    height = set_or_self t.height value.height;
  }

let to_string = function
  | Definite value -> Printf.sprintf "Definite(%g)" value
  | Min_content -> "MinContent"
  | Max_content -> "MaxContent"

let equal a b =
  match (a, b) with
  | Definite x, Definite y -> Float.abs (x -. y) < Float.epsilon
  | Min_content, Min_content -> true
  | Max_content, Max_content -> true
  | _ -> false

let compare a b =
  match (a, b) with
  | Definite x, Definite y -> Float.compare x y
  | Definite _, _ -> -1
  | _, Definite _ -> 1
  | Min_content, Min_content -> 0
  | Min_content, Max_content -> -1
  | Max_content, Min_content -> 1
  | Max_content, Max_content -> 0

let pp fmt t = Format.pp_print_string fmt (to_string t)

(* Operations with concrete values *)

let min t rhs =
  match t with
  | Definite val1 -> Definite (Float.min val1 rhs)
  | Min_content -> Definite rhs
  | Max_content -> Definite rhs

let max t rhs =
  match t with
  | Definite val1 -> Definite (Float.max val1 rhs)
  | Min_content -> Min_content
  | Max_content -> Max_content

let clamp t min max =
  match t with
  | Definite val1 -> Definite (Float.max min (Float.min max val1))
  | Min_content -> Min_content
  | Max_content -> Max_content

let add t rhs =
  match t with
  | Definite val1 -> Definite (val1 +. rhs)
  | Min_content -> Min_content
  | Max_content -> Max_content

let sub t rhs =
  match t with
  | Definite val1 -> Definite (val1 -. rhs)
  | Min_content -> Min_content
  | Max_content -> Max_content

(* Operations with optional values *)

let min_or_self t rhs =
  match (t, rhs) with
  | Definite val1, Some val2 -> Definite (Float.min val1 val2)
  | Definite val1, None -> Definite val1
  | Min_content, Some val2 -> Definite val2
  | Min_content, None -> Min_content
  | Max_content, Some val2 -> Definite val2
  | Max_content, None -> Max_content

let max_or_self t rhs =
  match (t, rhs) with
  | Definite val1, Some val2 -> Definite (Float.max val1 val2)
  | Definite val1, None -> Definite val1
  | Min_content, _ -> Min_content
  | Max_content, _ -> Max_content

let clamp_or_self t min max =
  match (t, min, max) with
  | Definite val1, Some min_val, Some max_val ->
      Definite (Float.max min_val (Float.min max_val val1))
  | Definite val1, None, Some max_val -> Definite (Float.min max_val val1)
  | Definite val1, Some min_val, None -> Definite (Float.max min_val val1)
  | Definite val1, None, None -> Definite val1
  | Min_content, _, _ -> Min_content
  | Max_content, _, _ -> Max_content

let add_or_zero t rhs =
  match (t, rhs) with
  | Definite val1, Some val2 -> Definite (val1 +. val2)
  | Definite val1, None -> Definite val1
  | Min_content, _ -> Min_content
  | Max_content, _ -> Max_content

let sub_or_zero t rhs =
  match (t, rhs) with
  | Definite val1, Some val2 -> Definite (val1 -. val2)
  | Definite val1, None -> Definite val1
  | Min_content, _ -> Min_content
  | Max_content, _ -> Max_content
