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
        ];
      group "Items" [ test "of_keyed" items_of_keyed ];
    ]
