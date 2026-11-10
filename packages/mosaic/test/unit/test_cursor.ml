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

(* The rows a value shows: its scroll resolved over [page] of [n] items. *)
let shown ~page ~n c =
  snd
    (Scroll.resolve (Cursor.scroll c) ~width:1 ~height:page ~content_width:1
       ~content_height:n)

(* After [Last] on 7 items, 3 shown, rows 4-6 show: [Prev] lands on a row
   shown, so the view stays. *)
let reveal_from_shown () =
  let c =
    match step (cursor "apple") Last with Moved c -> c | _ -> fail "a move"
  in
  match step c Prev with
  | Moved c -> equal ~msg:"rows 4-6" int 4 (shown ~page ~n:7 c)
  | _ -> fail "a move"

let pp_action ppf (a : Cursor.action) =
  Format.pp_print_string ppf
    (match a with
    | Next -> "Next"
    | Prev -> "Prev"
    | First -> "First"
    | Last -> "Last"
    | Page_down -> "Page_down"
    | Page_up -> "Page_up"
    | By k -> Printf.sprintf "By %d" k
    | _ -> "another action")

(* Where a value starts scrolled: declared at a row, possibly past the
   last, following an edge, or revealing a row from a declared one. *)
type start = At of int | Follow_top | Follow_bottom | Reveal of int * int

let start_scroll = function
  | At y -> Scroll.at y
  | Follow_top -> Scroll.follow `Top (Scroll.at 0)
  | Follow_bottom -> Scroll.follow `Bottom (Scroll.at 0)
  | Reveal (y, line) -> Scroll.reveal ~line (Scroll.at y)

let pp_start ppf = function
  | At y -> Format.fprintf ppf "At %d" y
  | Follow_top -> Format.pp_print_string ppf "Follow_top"
  | Follow_bottom -> Format.pp_print_string ppf "Follow_bottom"
  | Reveal (y, line) -> Format.fprintf ppf "Reveal (%d, %d)" y line

let pp_case ppf (n, page, start, actions) =
  Format.fprintf ppf "%d items, page %d, %a, [%a]" n page pp_start start
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
       pp_action)
    actions

let moves_gen =
  let open Gen in
  let action =
    one_of
      [
        of_list ~pp:pp_action Cursor.[ Next; Prev; First; Last; Page_down ];
        of_list ~pp:pp_action Cursor.[ Page_up ];
        map (fun k -> Cursor.By k) (int_range (-12) 12) |> with_pp pp_action;
      ]
  in
  (let* n = int_range 1 12 in
   let* page = int_range 0 6 in
   let row = int_range 0 (n + page) in
   let+ start =
     one_of
       [
         map (fun y -> At y) row;
         constant Follow_top;
         constant Follow_bottom;
         map (fun (y, line) -> Reveal (y, line)) (pair row row);
       ]
   and+ actions = list ~size:(int_range 1 12) action in
   (n, page, start, actions))
  |> with_pp pp_case

(* A move shows the item it lands on with the smallest movement from the rows
   shown before it, whatever the value declared: the rows stay when they hold
   the item, else the item becomes the first row shown or the last. A page
   of no rows shows the item at its first, as {!Scroll.resolve} does. *)
let reveal_law (n, page, start, actions) =
  let items = Items.of_list (List.init n string_of_int) in
  let step c a = Cursor.step ~equal:String.equal ~name:Fun.id ~page items c a in
  let kept_past_first = ref false in
  let landed c =
    match Cursor.index ~equal:String.equal items c with
    | Some i -> i
    | None -> fail "a move lands on an item"
  in
  let rec go c = function
    | [] -> ()
    | a :: rest -> (
        match step c a with
        | Cursor.Moved c' ->
            let before = shown ~page ~n c in
            let i = landed c' in
            let expected =
              if i < before then i
              else if i >= before + page then i - max 1 page + 1
              else before
            in
            if expected = before && before > 0 then kept_past_first := true;
            equal ~msg:"the rows shown" int expected (shown ~page ~n c');
            go c' rest
        | _ -> go c rest)
  in
  cover "started following the end" (start = Follow_bottom && n > page);
  cover "started past the last row"
    (match start with At y -> y > n - page | _ -> false);
  cover "a page of no rows" (page = 0);
  go (Cursor.with_scroll (start_scroll start) Cursor.none) actions;
  cover "rows past the first page kept" !kept_past_first

(* A move declares the rows it shows: [Last] from the first row declares
   rows 4-6, so it equals [Last] from rows 4-6. *)
let move_declares_shown () =
  let last c = match step c Last with Moved c -> c | _ -> fail "a move" in
  let scroll =
    Testable.make
      ~pp:(fun ppf s ->
        Format.fprintf ppf "shows row %d"
          (shown ~page ~n:7 (Cursor.with_scroll s Cursor.none)))
      ~equal:Scroll.equal
  in
  let value = Testable.make ~pp:pp_cursor ~equal:(Cursor.equal String.equal) in
  let from_top = last (cursor "apple") in
  equal ~msg:"the scroll declared" scroll
    (Scroll.reveal ~line:6 (Scroll.at 4))
    (Cursor.scroll from_top);
  equal ~msg:"equal values" value from_top
    (last (Cursor.with_scroll (Scroll.at 4) (cursor "apple")))

(* A move ends a follow: from a value following the end of 7 items, 3
   shown, [Prev] keeps rows 4-6, and they stay shown when items are added. *)
let move_ends_follow () =
  let c =
    Cursor.with_scroll (Scroll.follow `Bottom (Scroll.at 0)) (cursor "Date")
  in
  equal ~msg:"following" int 4 (shown ~page ~n:7 c);
  match step c Prev with
  | Moved c ->
      equal ~msg:"after the move" int 4 (shown ~page ~n:7 c);
      equal ~msg:"over more items" int 4 (shown ~page ~n:12 c)
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
  exit
    (run "mosaic.cursor"
       [
         group "Keys"
           [
             test "APG listbox keys" key_table;
             test "moves" moves;
             test "from no cursor item" from_no_cursor;
             test "identity" identity;
             test "a move reveals" reveal;
             test "a move reveals from the rows shown" reveal_from_shown;
             test "a move ends a follow" move_ends_follow;
             test "a move declares the rows it shows" move_declares_shown;
             prop ~count:300 "a move reveals with the smallest movement"
               moves_gen reveal_law;
             test "activation and toggling" activation;
           ];
         group "Typeahead" [ test "prefix" typeahead ];
         group "Ranges" [ test "extend from the anchor" ranges ];
         group "Nearest" [ test "after a removal" nearest ];
       ])
