open Windtrap
open Mosaic_ui

let contents items = List.init (Items.length items) (Items.get items)

let reading () =
  let l = [ "a"; "b"; "c" ] in
  equal (list string) l (contents (Items.of_list l));
  equal (list string) l (contents (Items.of_array (Array.of_list l)));
  equal (list string) l
    (contents (Items.make ~length:3 (fun i -> List.nth l i)));
  equal int 0 (Items.length (Items.of_list []));
  raises_match (Exn.invalid_arg ?substring:None) (fun () ->
      Items.get (Items.of_list l) 3);
  raises_match (Exn.invalid_arg ?substring:None) (fun () ->
      Items.get (Items.of_array [| 1 |]) (-1));
  raises_match (Exn.invalid_arg ?substring:None) (fun () ->
      Items.make ~length:(-1) Fun.id)

let sameness () =
  let l = List.init 3 succ in
  is_true ~msg:"one list" (Items.same (Items.of_list l) (Items.of_list l));
  is_false ~msg:"an equal list made again"
    (Items.same (Items.of_list l) (Items.of_list (List.init 3 succ)));
  let a = [| 1; 2; 3 |] in
  is_true ~msg:"one array" (Items.same (Items.of_array a) (Items.of_array a));
  is_false ~msg:"a list and an array of its items"
    (Items.same (Items.of_list l) (Items.of_array a));
  let f i = i + 1 in
  is_true ~msg:"one function"
    (Items.same (Items.make ~length:3 f) (Items.make ~length:3 f));
  is_false ~msg:"another length"
    (Items.same (Items.make ~length:3 f) (Items.make ~length:2 f));
  is_false ~msg:"a function made again"
    (Items.same
       (Items.make ~length:3 (fun i -> i + 1))
       (Items.make ~length:3 (fun i -> i + 1)))

(* A list mapped into items is read through the function when an item is
   read, and is the same items as another of one list and one function. *)
let mapped () =
  let reads = ref 0 in
  let f i =
    incr reads;
    string_of_int i
  in
  let l = [ 1; 2; 3 ] in
  let items = Items.map_list f l in
  equal ~msg:"no item read yet" int 0 !reads;
  equal ~msg:"length" int 3 (Items.length items);
  equal ~msg:"an item" string "2" (Items.get items 1);
  equal ~msg:"one read" int 1 !reads;
  is_true ~msg:"one list and one function"
    (Items.same items (Items.map_list f l));
  is_false ~msg:"another function"
    (Items.same items (Items.map_list (fun i -> string_of_int i) l));
  is_false ~msg:"an equal list made again"
    (Items.same items (Items.map_list f (List.init 3 succ)))

let equality () =
  let eq = Items.equal Int.equal in
  is_true ~msg:"equal lists made apart"
    (eq (Items.of_list [ 1; 2 ]) (Items.of_list [ 1; 2 ]));
  is_true ~msg:"across representations"
    (eq (Items.of_list [ 1; 2 ]) (Items.make ~length:2 (fun i -> i + 1)));
  is_false ~msg:"another item"
    (eq (Items.of_array [| 1; 2 |]) (Items.of_array [| 1; 3 |]));
  is_false ~msg:"another length"
    (eq (Items.of_list [ 1; 2 ]) (Items.of_list [ 1; 2; 3 ]))

let () =
  run "mosaic.items"
    [
      group "Items"
        [
          test "reading by position" reading;
          test "the same items" sameness;
          test "equal items" equality;
          test "a mapped list" mapped;
        ];
    ]
