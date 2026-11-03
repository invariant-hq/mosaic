open Windtrap

let traversal () =
  let order = [ "composer"; "history"; "search" ] in
  let next = Mosaic.Focus.next ~equal:String.equal order in
  let prev = Mosaic.Focus.prev ~equal:String.equal order in
  List.iter
    (fun (current, forward, backward) ->
      equal ~msg:"next" (option string) forward (next current);
      equal ~msg:"previous" (option string) backward (prev current))
    [
      (None, Some "composer", Some "search");
      (Some "removed", Some "composer", Some "search");
      (Some "composer", Some "history", Some "search");
      (Some "history", Some "search", Some "composer");
      (Some "search", Some "composer", Some "history");
    ]

let empty_and_singleton () =
  List.iter
    (fun step ->
      List.iter
        (fun current ->
          equal (option int) None (step ~equal:Int.equal [] current);
          equal (option int) (Some 1) (step ~equal:Int.equal [ 1 ] current))
        [ None; Some 1; Some 9 ])
    [ Mosaic.Focus.next; Mosaic.Focus.prev ]

let explicit_equality () =
  let first = ("A", fun () -> ()) and second = ("B", fun () -> ()) in
  let current = Some ("a", fun () -> ()) in
  let same_id (a, _) (b, _) =
    String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)
  in
  List.iter
    (fun step ->
      equal (option string) (Some "B")
        (Option.map fst (step ~equal:same_id [ first; second ] current)))
    [ Mosaic.Focus.next; Mosaic.Focus.prev ]

let () =
  exit
    (run "Focus"
       [
         test "ordered traversal and wrapping" traversal;
         test "empty and singleton rings" empty_and_singleton;
         test "identifiers use the supplied equality" explicit_equality;
       ])
