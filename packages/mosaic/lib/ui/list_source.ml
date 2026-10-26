(* A source keeps the application's list, whatever its element type, with
   the function that turns an element into what the widget shows. *)
type 'b t = Source : 'a list * ('a -> 'b) -> 'b t

(* Physical equality of two values of any types. It only compares the values'
   addresses or immediate representations, as [==] does. *)
external physically_equal : 'a -> 'b -> bool = "%eq"

let of_list l = Source (l, Fun.id)
let map f l = Source (l, f)

let same (Source (l, f)) (Source (l', f')) =
  physically_equal l l' && physically_equal f f'

let rec pairwise eq f f' l l' =
  match (l, l') with
  | [], [] -> true
  | x :: l, x' :: l' -> eq (f x) (f' x') && pairwise eq f f' l l'
  | _ -> false

let equal eq (Source (l, f) as s) (Source (l', f') as s') =
  same s s' || pairwise eq f f' l l'

let length (Source (l, _)) = List.length l

let to_array (Source (l, f)) =
  match l with
  | [] -> [||]
  | x :: rest ->
      let a = Array.make (List.length l) (f x) in
      List.iteri (fun i x -> a.(i + 1) <- f x) rest;
      a

let to_list (Source (l, f)) = List.map f l
