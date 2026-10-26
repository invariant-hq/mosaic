(* Items keep the application's data, whatever its element type, with the
   function that turns an element into an item. Each value is one block, so
   making items from a list costs what the widgets' list sources cost before
   they were public. *)
type 'a t =
  | List : 'b list * ('b -> 'a) -> 'a t
  | Array : 'b array * ('b -> 'a) -> 'a t
  | Make : int * (int -> 'a) -> 'a t

(* Physical equality of two values of any types. It only compares the values'
   addresses or immediate representations, as [==] does. *)
external physically_equal : 'a -> 'b -> bool = "%eq"

let of_list l = List (l, Fun.id)
let of_array a = Array (a, Fun.id)
let map_list f l = List (l, f)

let make ~length f =
  if length < 0 then invalid_arg "Items.make: negative length";
  Make (length, f)

let length = function
  | List (l, _) -> List.length l
  | Array (a, _) -> Array.length a
  | Make (n, _) -> n

let out_of_range () = invalid_arg "Items.get: no item at this position"

let rec nth l i =
  match l with
  | [] -> out_of_range ()
  | x :: l -> if i = 0 then x else nth l (i - 1)

let get s i =
  if i < 0 then out_of_range ();
  match s with
  | List (l, f) -> f (nth l i)
  | Array (a, f) -> if i < Array.length a then f a.(i) else out_of_range ()
  | Make (n, f) -> if i < n then f i else out_of_range ()

let same s s' =
  match (s, s') with
  | List (l, f), List (l', f') -> physically_equal l l' && physically_equal f f'
  | Array (a, f), Array (a', f') ->
      physically_equal a a' && physically_equal f f'
  | Make (n, f), Make (n', f') -> n = n' && physically_equal f f'
  | _ -> false

let to_array = function
  | List (l, f) -> (
      match l with
      | [] -> [||]
      | x :: rest ->
          let a = Array.make (List.length l) (f x) in
          List.iteri (fun i x -> a.(i + 1) <- f x) rest;
          a)
  | Array (a, f) -> Array.map f a
  | Make (n, f) -> Array.init n f

let to_list = function
  | List (l, f) -> List.map f l
  | Array (a, f) -> List.init (Array.length a) (fun i -> f a.(i))
  | Make (n, f) -> List.init n f

let rec pairwise eq f f' l l' =
  match (l, l') with
  | [], [] -> true
  | x :: l, x' :: l' -> eq (f x) (f' x') && pairwise eq f f' l l'
  | _ -> false

let equal eq s s' =
  same s s'
  ||
  match (s, s') with
  | List (l, f), List (l', f') -> pairwise eq f f' l l'
  | List _, _ | _, List _ ->
      (* Reading a list by position costs the position. *)
      List.equal eq (to_list s) (to_list s')
  | _ ->
      let n = length s in
      n = length s'
      &&
      let rec from i = i = n || (eq (get s i) (get s' i) && from (i + 1)) in
      from 0
