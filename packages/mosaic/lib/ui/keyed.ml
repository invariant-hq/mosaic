(* An AVL tree with the size of each subtree, for a constant [cardinal] and a
   logarithmic [nth]. A map holds its order with its tree, so that two maps
   made from one share the order and their diff can trust it. *)

type ('k, 'v) tree =
  | Leaf
  | Node of {
      l : ('k, 'v) tree;
      k : 'k;
      v : 'v;
      r : ('k, 'v) tree;
      h : int;
      s : int;
    }

type ('k, 'v) t = { compare : 'k -> 'k -> int; tree : ('k, 'v) tree }

let empty ~compare = { compare; tree = Leaf }
let height = function Leaf -> 0 | Node n -> n.h
let size = function Leaf -> 0 | Node n -> n.s

let create l k v r =
  let hl = height l and hr = height r in
  Node
    {
      l;
      k;
      v;
      r;
      h = (if hl >= hr then hl + 1 else hr + 1);
      s = size l + size r + 1;
    }

let bal l k v r =
  let hl = height l and hr = height r in
  if hl > hr + 2 then
    match l with
    | Node { l = ll; k = lk; v = lv; r = lr; _ } -> (
        if height ll >= height lr then create ll lk lv (create lr k v r)
        else
          match lr with
          | Node { l = lrl; k = lrk; v = lrv; r = lrr; _ } ->
              create (create ll lk lv lrl) lrk lrv (create lrr k v r)
          | Leaf -> assert false)
    | Leaf -> assert false
  else if hr > hl + 2 then
    match r with
    | Node { l = rl; k = rk; v = rv; r = rr; _ } -> (
        if height rr >= height rl then create (create l k v rl) rk rv rr
        else
          match rl with
          | Node { l = rll; k = rlk; v = rlv; r = rlr; _ } ->
              create (create l k v rll) rlk rlv (create rlr rk rv rr)
          | Leaf -> assert false)
    | Leaf -> assert false
  else create l k v r

let add k v m =
  let rec add = function
    | Leaf -> Node { l = Leaf; k; v; r = Leaf; h = 1; s = 1 }
    | Node n as t ->
        let c = m.compare k n.k in
        if c = 0 then if n.v == v then t else create n.l k v n.r
        else if c < 0 then
          let l = add n.l in
          if l == n.l then t else bal l n.k n.v n.r
        else
          let r = add n.r in
          if r == n.r then t else bal n.l n.k n.v r
  in
  let tree = add m.tree in
  if tree == m.tree then m else { m with tree }

let rec min_node = function
  | Leaf -> None
  | Node { l = Leaf; k; v; _ } -> Some (k, v)
  | Node { l; _ } -> min_node l

let rec max_node = function
  | Leaf -> None
  | Node { r = Leaf; k; v; _ } -> Some (k, v)
  | Node { r; _ } -> max_node r

let rec remove_min = function
  | Leaf -> Leaf
  | Node { l = Leaf; r; _ } -> r
  | Node n -> bal (remove_min n.l) n.k n.v n.r

let merge l r =
  match (l, r) with
  | Leaf, t | t, Leaf -> t
  | _ -> (
      match min_node r with
      | Some (k, v) -> bal l k v (remove_min r)
      | None -> l)

let remove k m =
  let rec remove = function
    | Leaf -> Leaf
    | Node n as t ->
        let c = m.compare k n.k in
        if c = 0 then merge n.l n.r
        else if c < 0 then
          let l = remove n.l in
          if l == n.l then t else bal l n.k n.v n.r
        else
          let r = remove n.r in
          if r == n.r then t else bal n.l n.k n.v r
  in
  let tree = remove m.tree in
  if tree == m.tree then m else { m with tree }

let find_opt k m =
  let rec find = function
    | Leaf -> None
    | Node n ->
        let c = m.compare k n.k in
        if c = 0 then Some n.v else find (if c < 0 then n.l else n.r)
  in
  find m.tree

let cardinal m = size m.tree
let min_binding_opt m = min_node m.tree
let max_binding_opt m = max_node m.tree

let fold f m acc =
  let rec fold t acc =
    match t with Leaf -> acc | Node n -> fold n.r (f n.k n.v (fold n.l acc))
  in
  fold m.tree acc

let of_list ~compare l =
  List.fold_left (fun m (k, v) -> add k v m) (empty ~compare) l

module Private = struct
  let iter f m =
    let rec iter = function
      | Leaf -> ()
      | Node n ->
          iter n.l;
          f n.k n.v;
          iter n.r
    in
    iter m.tree

  let nth m i =
    if i < 0 || i >= size m.tree then invalid_arg "Keyed.nth: no binding at i";
    let rec nth t i =
      match t with
      | Leaf -> assert false
      | Node n ->
          let left = size n.l in
          if i < left then nth n.l i
          else if i = left then (n.k, n.v)
          else nth n.r (i - left - 1)
    in
    nth m.tree i

  type ('a, 'b) change = Removed of 'a | Added of 'b | Changed of 'a * 'b

  let empty_like m = { compare = m.compare; tree = Leaf }

  (* Physical equality of two values of any types, as [==] compares two
     values of one type. *)
  external same : 'a -> 'b -> bool = "%eq"

  let same_order a b = a.compare == b.compare

  (* In-order enumerations: the next binding, its right subtree, the rest. *)
  type ('k, 'v) enum = End | More of 'k * 'v * ('k, 'v) tree * ('k, 'v) enum

  let rec cons t e =
    match t with Leaf -> e | Node n -> cons n.l (More (n.k, n.v, n.r, e))

  (* Two subtrees at the same place of two maps are shared when they are the
     same block; the walk then skips them whole, as Base's [symmetric_diff]
     does. *)
  let diff a b f acc =
    if not (same_order a b) then
      invalid_arg "Keyed: the maps were made with different orders";
    if same a.tree b.tree then acc
    else
      let compare = a.compare in
      let rec loop e1 e2 acc =
        match (e1, e2) with
        | End, End -> acc
        | End, More (k, v, r, e) -> loop End (cons r e) (f k (Added v) acc)
        | More (k, v, r, e), End -> loop (cons r e) End (f k (Removed v) acc)
        | More (k1, v1, r1, e1'), More (k2, v2, r2, e2') ->
            let c = compare k1 k2 in
            if c = 0 then
              let acc =
                if same v1 v2 then acc else f k1 (Changed (v1, v2)) acc
              in
              if same r1 r2 then loop e1' e2' acc
              else loop (cons r1 e1') (cons r2 e2') acc
            else if c < 0 then loop (cons r1 e1') e2 (f k1 (Removed v1) acc)
            else loop e1 (cons r2 e2') (f k2 (Added v2) acc)
      in
      loop (cons a.tree End) (cons b.tree End) acc

  let find_map f m =
    let rec go = function
      | Leaf -> None
      | Node n -> (
          match go n.l with
          | Some _ as found -> found
          | None -> (
              match f n.v with Some _ as found -> found | None -> go n.r))
    in
    go m.tree
end
