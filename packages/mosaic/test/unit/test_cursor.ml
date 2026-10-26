open Windtrap
open Mosaic_ui
module Keystroke = Matrix_input.Keystroke

let names =
  [| "apple"; "apricot"; "banana"; "blueberry"; "cherry"; "Date"; "elder" |]

let items = Items.of_array names
let page = 3

let step ?(items = items) c a =
  Cursor.step ~equal:String.equal ~name:Fun.id ~page items c a

let pp_cursor ppf c =
  Format.fprintf ppf "%s"
    (match Cursor.item c with None -> "none" | Some x -> x)

let pp_outcome ppf = function
  | Cursor.Moved c -> Format.fprintf ppf "Moved %a" pp_cursor c
  | Activated x -> Format.fprintf ppf "Activated %s" x
  | Changed (change, c) ->
      let items l = String.concat "," l in
      Format.fprintf ppf "Changed (%s, %a)"
        (match change with
        | Toggle x -> "Toggle " ^ x
        | Add l -> "Add " ^ items l
        | Remove l -> "Remove " ^ items l)
        pp_cursor c
  | Nothing -> Format.fprintf ppf "Nothing"

let outcome = Testable.make ~pp:pp_outcome ~equal:( = )

(* The cursor item an outcome lands on. *)
let landed = function
  | Cursor.Moved c | Changed (_, c) -> Cursor.item c
  | Activated _ | Nothing -> None

let lands ?msg ?items c a expected =
  equal ?msg (option string) expected (landed (step ?items c a))

let cursor x = Cursor.at x

(* ───── The keys of a listbox ───── *)

let bound shortcut keys =
  match List.find_opt (fun (s, _) -> Shortcut.equal s shortcut) keys with
  | Some (_, a) -> Some a
  | None -> None

let key_table () =
  let action =
    Testable.make
      ~pp:(fun ppf _ -> Format.pp_print_string ppf "action")
      ~equal:( = )
  in
  let check ?(keys = Cursor.default_keys) shortcut a =
    equal (option action) (Some a) (bound shortcut keys)
  in
  let key = Shortcut.key in
  check Shortcut.down Next;
  check Shortcut.up Prev;
  check (key Home) First;
  check (key End) Last;
  check (key Page_down) Page_down;
  check (key Page_up) Page_up;
  check Shortcut.enter Activate;
  check (Shortcut.ctrl 'j') Activate;
  check Shortcut.space Toggle;
  check (key ~shift:true Down) Extend_next;
  check (key ~shift:true Up) Extend_prev;
  check (Shortcut.ctrl 'a') Select_all;
  is_none ~msg:"letters are typed ahead"
    (bound (Shortcut.char 'j') Cursor.default_keys);
  check ~keys:Cursor.wrapping_keys Shortcut.down Next_wrap;
  check ~keys:Cursor.wrapping_keys Shortcut.up Prev_wrap;
  check ~keys:Cursor.wrapping_keys (key Home) First;
  equal int (List.length Cursor.default_keys) (List.length Cursor.wrapping_keys)

let moves () =
  lands (cursor "banana") Next (Some "blueberry");
  lands (cursor "banana") Prev (Some "apricot");
  lands ~msg:"no wrap at the last" (cursor "elder") Next None;
  equal outcome Nothing (step (cursor "elder") Next);
  equal outcome Nothing (step (cursor "apple") Prev);
  lands (cursor "elder") Next_wrap (Some "apple");
  lands (cursor "apple") Prev_wrap (Some "elder");
  lands (cursor "cherry") First (Some "apple");
  lands (cursor "apple") Last (Some "elder");
  lands (cursor "apple") (By 2) (Some "banana");
  lands ~msg:"clamped" (cursor "apple") (By 100) (Some "elder");
  lands (cursor "elder") (By (-2)) (Some "cherry");
  equal outcome Nothing (step (cursor "banana") (By 0));
  lands ~msg:"page - 1" (cursor "apple") Page_down (Some "banana");
  lands (cursor "elder") Page_up (Some "cherry");
  equal ~msg:"a page of one moves one" (option string) (Some "apricot")
    (landed
       (Cursor.step ~equal:String.equal ~name:Fun.id ~page:1 items
          (cursor "apple") Page_down));
  equal outcome Nothing (step (cursor "apple") First);
  equal outcome Nothing (step ~items:(Items.of_list []) Cursor.none Next)

let from_no_cursor () =
  List.iter
    (fun a -> lands Cursor.none a (Some "apple"))
    Cursor.[ Next; Next_wrap; First; By 1; Page_down ];
  List.iter
    (fun a -> lands Cursor.none a (Some "elder"))
    Cursor.[ Prev; Prev_wrap; Last; By (-1); Page_up ];
  lands ~msg:"an absent cursor item acts as none" (cursor "fig") Next
    (Some "apple");
  equal outcome Nothing (step Cursor.none Activate);
  equal outcome Nothing (step Cursor.none Toggle)

let identity () =
  let reordered = Items.of_array [| "elder"; "cherry"; "apple" |] in
  let c =
    match step (cursor "banana") Next with Moved c -> c | _ -> fail "a move"
  in
  equal (option int) (Some 3) (Cursor.index ~equal:String.equal items c);
  is_none ~msg:"absent after a filter"
    (Cursor.index ~equal:String.equal reordered c);
  lands ~msg:"the item, not the position, after a reorder" ~items:reordered
    (cursor "cherry") Next (Some "apple")

let reveal () =
  match step (cursor "apple") Last with
  | Moved c ->
      equal (pair int int) (0, 4)
        (Scroll.resolve (Cursor.scroll c) ~width:10 ~height:3 ~content_width:10
           ~content_height:7)
  | _ -> fail "a move"

let activation () =
  equal outcome (Activated "cherry") (step (cursor "cherry") Activate);
  match step (cursor "cherry") Toggle with
  | Changed (Toggle "cherry", c) ->
      equal (option string) (Some "cherry") (Cursor.anchor c)
  | o -> failf "%a" pp_outcome o

(* ───── Typeahead ───── *)

let type_ c s =
  let rec go c i =
    if i = String.length s then c
    else
      match step c (Typeahead (Uchar.of_char s.[i])) with
      | Moved c -> go c (i + 1)
      | _ -> failf "%S: nothing at %d" s i
  in
  go c 0

let typeahead () =
  equal (option string) (Some "apple") (Cursor.item (type_ Cursor.none "a"));
  equal ~msg:"extends while it matches" (option string) (Some "apricot")
    (Cursor.item (type_ Cursor.none "apr"));
  equal ~msg:"the extended prefix keeps a matching cursor item" (option string)
    (Some "banana")
    (Cursor.item (type_ (cursor "apple") "ba"));
  equal ~msg:"one character again moves on" (option string) (Some "blueberry")
    (Cursor.item (type_ (cursor "banana") "b"));
  equal ~msg:"wraps" (option string) (Some "apple")
    (Cursor.item (type_ (cursor "elder") "a"));
  equal ~msg:"case folded" (option string) (Some "Date")
    (Cursor.item (type_ Cursor.none "d"));
  equal ~msg:"restarts at a character no name continues with" (option string)
    (Some "cherry")
    (Cursor.item (type_ Cursor.none "apc"));
  equal ~msg:"no name starts with it" outcome Nothing
    (step (cursor "apple") (Typeahead (Uchar.of_char 'z')));
  let typed = type_ Cursor.none "ap" in
  is_false ~msg:"the prefix is part of the value"
    (Cursor.equal String.equal typed (cursor "apple"));
  (match step typed Next with
  | Moved c ->
      equal ~msg:"another action clears it" (option string) (Some "banana")
        (Cursor.item (type_ c "b"))
  | _ -> fail "a move");
  equal ~msg:"after a clear, the search starts after the cursor item"
    (option string) (Some "apricot")
    (Cursor.item
       (type_
          (match step (type_ Cursor.none "a") First with
          | Moved c -> c
          | Nothing -> cursor "apple"
          | _ -> fail "a move")
          "a"))

(* ───── Ranges ───── *)

let changes c actions =
  let rec go c acc = function
    | [] -> List.rev acc
    | a :: rest -> (
        match step c a with
        | Changed (change, c) -> go c (change :: acc) rest
        | Moved c -> go c acc rest
        | o -> failf "%a" pp_outcome o)
  in
  go c [] actions

let change =
  Testable.make
    ~pp:(fun ppf c -> pp_outcome ppf (Changed (c, Cursor.none)))
    ~equal:( = )

let ranges () =
  equal (list change)
    [
      Add [ "banana"; "blueberry" ];
      Add [ "cherry" ];
      Remove [ "cherry" ];
      Remove [ "blueberry" ];
      Add [ "apricot" ];
    ]
    (changes (cursor "banana")
       Cursor.
         [ Extend_next; Extend_next; Extend_prev; Extend_prev; Extend_prev ]);
  equal ~msg:"from a toggled anchor" (list change)
    [ Toggle "banana"; Add [ "blueberry" ] ]
    (changes (cursor "banana") Cursor.[ Toggle; Extend_next ]);
  equal ~msg:"a move clears the anchor" (list change)
    [ Toggle "banana"; Add [ "cherry"; "Date" ] ]
    (changes (cursor "banana") Cursor.[ Toggle; Next; Next; Extend_next ]);
  equal ~msg:"from no cursor item" (list change) [ Add [ "apple" ] ]
    (changes Cursor.none Cursor.[ Extend_next ]);
  equal outcome Nothing (step (cursor "elder") Extend_next);
  equal (list change)
    [ Add (Array.to_list names) ]
    (changes (cursor "banana") Cursor.[ Select_all ])

(* ───── Nearest ───── *)

let nearest () =
  let before = items in
  let after l = Items.of_list l in
  let near l c =
    Cursor.item (Cursor.nearest ~equal:String.equal ~before (after l) c)
  in
  equal ~msg:"kept" (option string) (Some "cherry")
    (near [ "banana"; "cherry" ] (cursor "cherry"));
  equal ~msg:"the next survivor" (option string) (Some "Date")
    (near [ "apple"; "banana"; "Date"; "elder" ] (cursor "cherry"));
  equal ~msg:"skips removed followers" (option string) (Some "elder")
    (near [ "apple"; "elder" ] (cursor "banana"));
  equal ~msg:"the last before when none follows" (option string) (Some "banana")
    (near [ "apple"; "banana" ] (cursor "cherry"));
  equal ~msg:"an inserted item is passed over" (option string) (Some "elder")
    (near [ "apple"; "fig"; "elder" ] (cursor "cherry"));
  is_none ~msg:"none left" (near [ "fig" ] (cursor "cherry"));
  is_none ~msg:"no cursor item" (near [ "apple" ] Cursor.none);
  match
    Cursor.index ~equal:String.equal
      (after [ "apple"; "Date" ])
      (Cursor.nearest ~equal:String.equal ~before
         (after [ "apple"; "Date" ])
         (cursor "cherry"))
  with
  | Some 1 -> ()
  | _ -> fail "the position of the new cursor item"

let () =
  run "mosaic.cursor"
    [
      group "Keys"
        [
          test "APG listbox keys" key_table;
          test "moves" moves;
          test "from no cursor item" from_no_cursor;
          test "identity" identity;
          test "a move reveals" reveal;
          test "activation and toggling" activation;
        ];
      group "Typeahead" [ test "prefix" typeahead ];
      group "Ranges" [ test "extend from the anchor" ranges ];
      group "Nearest" [ test "after a removal" nearest ];
    ]
