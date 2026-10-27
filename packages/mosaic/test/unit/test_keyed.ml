open Windtrap
open Mosaic_ui

let ints l = Keyed.of_list ~compare:Int.compare l
let bindings m = Keyed.fold (fun k v acc -> (k, v) :: acc) m [] |> List.rev

let ordered () =
  let m = ints [ (3, "c"); (1, "a"); (2, "b") ] in
  equal ~msg:"in order"
    (list (pair int string))
    [ (1, "a"); (2, "b"); (3, "c") ]
    (bindings m);
  equal ~msg:"cardinal" int 3 (Keyed.cardinal m);
  equal ~msg:"min"
    (option (pair int string))
    (Some (1, "a"))
    (Keyed.min_binding_opt m);
  equal ~msg:"max"
    (option (pair int string))
    (Some (3, "c"))
    (Keyed.max_binding_opt m)

let add_and_remove () =
  let m = ints (List.init 100 (fun i -> (i, string_of_int i))) in
  let m = Keyed.remove 50 (Keyed.add 100 "100" m) in
  equal ~msg:"cardinal" int 100 (Keyed.cardinal m);
  equal ~msg:"removed" (option string) None (Keyed.find_opt 50 m);
  equal ~msg:"added" (option string) (Some "100") (Keyed.find_opt 100 m);
  equal ~msg:"kept" (option string) (Some "49") (Keyed.find_opt 49 m);
  is_true ~msg:"removing an absent key is the map" (Keyed.remove 1000 m == m);
  let v = Option.get (Keyed.find_opt 3 m) in
  is_true ~msg:"adding the same value is the map" (Keyed.add 3 v m == m)

(* Of two bindings of one key, the later one stays. *)
let duplicates () =
  equal
    (list (pair int string))
    [ (1, "b") ]
    (bindings (ints [ (1, "a"); (1, "b") ]))

(* Positions: [nth], [rank], [fold_range] and [iter] agree with the list of
   bindings. *)
let positions () =
  let m = ints (List.init 50 (fun i -> (i * 2, string_of_int i))) in
  let l = bindings m in
  List.iteri
    (fun i (k, v) ->
      equal ~msg:"nth" (pair int string) (k, v) (Keyed.nth m i);
      equal ~msg:"rank of a key" int i (Keyed.rank m k);
      equal ~msg:"rank of a key not bound" int (i + 1) (Keyed.rank m (k + 1)))
    l;
  raises_match ~msg:"nth past the end"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Keyed.nth m 50));
  let range lo hi =
    List.rev (Keyed.fold_range (fun k v acc -> (k, v) :: acc) m lo hi [])
  in
  equal ~msg:"a range"
    (list (pair int string))
    (List.filteri (fun i _ -> i >= 10 && i < 13) l)
    (range 10 13);
  equal ~msg:"an empty range" (list (pair int string)) [] (range 5 5);
  equal ~msg:"the whole map" (list (pair int string)) l (range 0 50);
  let seen = ref [] in
  Keyed.iter (fun k v -> seen := (k, v) :: !seen) m;
  equal ~msg:"iter" (list (pair int string)) l (List.rev !seen);
  equal ~msg:"find_map, the first in order" (option int) (Some 42)
    (Keyed.find_map
       (fun v ->
         if int_of_string v > 20 then Some (2 * int_of_string v) else None)
       m)

(* [symmetric_diff a b] is the naive difference of the two maps' bindings,
   whatever they share, for maps of one order. *)
let symmetric_diff_is_the_difference seed =
  let rng = Random.State.make [| seed |] in
  let base =
    ints (List.init (Random.State.int rng 40) (fun i -> (i, string_of_int i)))
  in
  let edit m =
    let m = ref m in
    for _ = 1 to Random.State.int rng 6 do
      let k = Random.State.int rng 50 in
      m :=
        if Random.State.bool rng then Keyed.add k (Printf.sprintf "%d'" k) !m
        else Keyed.remove k !m
    done;
    !m
  in
  let a = edit base and b = edit base in
  let naive =
    let keys =
      List.sort_uniq compare
        (List.map fst (bindings a) @ List.map fst (bindings b))
    in
    List.filter_map
      (fun k ->
        match (Keyed.find_opt k a, Keyed.find_opt k b) with
        | Some x, None -> Some (k, `Removed x)
        | None, Some y -> Some (k, `Added y)
        | Some x, Some y when x != y -> Some (k, `Changed (x, y))
        | _ -> None)
      keys
  in
  let diff =
    Keyed.symmetric_diff a b
      (fun k change acc ->
        let c =
          match change with
          | Keyed.Removed x -> `Removed x
          | Added y -> `Added y
          | Changed (x, y) -> `Changed (x, y)
        in
        (k, c) :: acc)
      []
    |> List.rev
  in
  is_true ~msg:"the naive difference" (diff = naive)

let orders () =
  let a = ints [ (1, "a") ] and b = ints [ (2, "b") ] in
  is_true ~msg:"one compare function, one order" (Keyed.order a == Keyed.order b);
  let reversed = Keyed.of_list ~compare:(fun x y -> Int.compare y x) [] in
  raises_match ~msg:"maps of two orders"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Keyed.symmetric_diff a reversed (fun _ _ n -> n + 1) 0));
  let copy = Keyed.add 3 "c" (Keyed.empty ~compare:(Keyed.order a)) in
  equal ~msg:"a map made in another's order" int 2
    (Keyed.symmetric_diff a copy (fun _ _ n -> n + 1) 0)

let items_of_keyed () =
  let m = ints [ (2, "b"); (1, "a"); (3, "c") ] in
  let items = Items.of_keyed m in
  equal ~msg:"length" int 3 (Items.length items);
  equal ~msg:"by position" string "b" (Items.get items 1);
  equal ~msg:"in order" (list string) [ "a"; "b"; "c" ] (Items.to_list items);
  is_true ~msg:"the same map is the same items"
    (Items.same items (Items.of_keyed m));
  is_false ~msg:"another map is other items"
    (Items.same items (Items.of_keyed (Keyed.add 4 "d" m)))

let () =
  run "mosaic.keyed"
    [
      group "Maps"
        [
          test "ordered" ordered;
          test "add and remove" add_and_remove;
          test "duplicates" duplicates;
          test "positions" positions;
          prop ~count:300 "symmetric_diff is the difference" Gen.int
            symmetric_diff_is_the_difference;
          test "orders" orders;
        ];
      group "Items" [ test "of_keyed" items_of_keyed ];
    ]
