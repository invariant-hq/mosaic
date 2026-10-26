(* A cursor value. [hint] and [anchor_hint] are the positions where [item]
   and [anchor] were last seen; they only spare a search, so [equal] ignores
   them. [prefix] is the typed prefix, folded to lower case. *)
type 'a t = {
  item : 'a option;
  hint : int;
  scroll : Scroll.t;
  anchor : 'a option;
  anchor_hint : int;
  prefix : string;
}

let none =
  {
    item = None;
    hint = 0;
    scroll = Scroll.at 0;
    anchor = None;
    anchor_hint = 0;
    prefix = "";
  }

let at x = { none with item = Some x }
let item c = c.item
let scroll c = c.scroll
let anchor c = c.anchor
let with_item item c = { c with item }
let with_scroll scroll c = { c with scroll }

let equal eq a b =
  Option.equal eq a.item b.item
  && Option.equal eq a.anchor b.anchor
  && String.equal a.prefix b.prefix
  && Scroll.equal a.scroll b.scroll

(* The position of [x] in [items], trying [hint] first. Top-level loops, so a
   search allocates nothing whatever the number of items. *)
let rec search equal items x n i =
  if i = n then -1
  else if equal (Items.get items i) x then i
  else search equal items x n (i + 1)

let find equal items x hint =
  let n = Items.length items in
  if hint >= 0 && hint < n && equal (Items.get items hint) x then hint
  else search equal items x n 0

let position equal items = function
  | None, _ -> -1
  | Some x, hint -> find equal items x hint

let index ~equal items c =
  match position equal items (c.item, c.hint) with -1 -> None | i -> Some i

(* ───── Steps ───── *)

type 'a change = Toggle of 'a | Add of 'a list | Remove of 'a list

type action =
  | Next
  | Prev
  | Next_wrap
  | Prev_wrap
  | First
  | Last
  | By of int
  | Page_down
  | Page_up
  | Typeahead of Uchar.t
  | Activate
  | Toggle
  | Extend_next
  | Extend_prev
  | Select_all

type 'a outcome =
  | Moved of 'a t
  | Activated of 'a
  | Changed of 'a change * 'a t
  | Nothing

(* [c] on the item at [i], revealed, with no anchor and no typed prefix. *)
let put items c i =
  {
    item = Some (Items.get items i);
    hint = i;
    scroll = Scroll.reveal ~line:i c.scroll;
    anchor = None;
    anchor_hint = 0;
    prefix = "";
  }

(* A move from the cursor item at [current] (-1 for none) to [target]. *)
let move items c ~current target =
  if target = current && c.anchor = None && c.prefix = "" then Nothing
  else Moved (put items c target)

(* Whether [s] starts with [p], ASCII letters of [s] folded to lower case;
   [p] is folded already. *)
let rec prefix_from p s lp i =
  i = lp
  || Char.equal
       (Char.lowercase_ascii (String.unsafe_get s i))
       (String.unsafe_get p i)
     && prefix_from p s lp (i + 1)

let starts_with p s =
  let lp = String.length p in
  String.length s >= lp && prefix_from p s lp 0

(* The first position, from [start] on and wrapping, of an item whose name
   starts with [p], or -1. *)
let rec match_from name items p n start k =
  if k = n then -1
  else
    let i = (start + k) mod n in
    if starts_with p (name (Items.get items i)) then i
    else match_from name items p n start (k + 1)

let folded u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  String.lowercase_ascii (Buffer.contents b)

let typeahead ~name items c ~current n u =
  let typed = folded u in
  let prefix = c.prefix ^ typed in
  let after = if current < 0 then 0 else (current + 1) mod n in
  let start = if c.prefix = "" || current < 0 then after else current in
  let found p start = match_from name items p n start 0 in
  let i, prefix =
    match found prefix start with
    | -1 when c.prefix <> "" -> (found typed after, typed)
    | i -> (i, prefix)
  in
  if i < 0 then Nothing
  else if i = current && String.equal prefix c.prefix then Nothing
  else Moved { (put items c i) with prefix }

(* The change to the range from the anchor at [a] when the cursor moves from
   [i] to [j], one item apart. *)
let extend items c ~equal ~current ~target =
  let x = Items.get items target in
  let anchor_at =
    match c.anchor with
    | None -> -1
    | Some a -> find equal items a c.anchor_hint
  in
  let next a anchor_hint =
    { (put items c target) with anchor = Some a; anchor_hint }
  in
  if current < 0 then Changed (Add [ x ], next x target)
  else
    let y = Items.get items current in
    if anchor_at < 0 then Changed (Add [ y; x ], next y current)
    else
      let a = Option.get c.anchor in
      if abs (target - anchor_at) > abs (current - anchor_at) then
        Changed (Add [ x ], next a anchor_at)
      else Changed (Remove [ y ], next a anchor_at)

let rec all items i acc =
  if i < 0 then acc else all items (i - 1) (Items.get items i :: acc)

let step ~equal ~name ~page items c action =
  let n = Items.length items in
  if n = 0 then Nothing
  else
    let current = position equal items (c.item, c.hint) in
    let by k =
      if k = 0 then Nothing
      else if current < 0 then
        move items c ~current (if k > 0 then 0 else n - 1)
      else move items c ~current (max 0 (min (n - 1) (current + k)))
    in
    let page = max 1 (page - 1) in
    match action with
    | (Next | Prev | Extend_next | Extend_prev) when current < 0 -> (
        let target = match action with Next | Extend_next -> 0 | _ -> n - 1 in
        match action with
        | Next | Prev -> move items c ~current target
        | _ -> extend items c ~equal ~current ~target)
    | Next ->
        if current = n - 1 then Nothing else move items c ~current (current + 1)
    | Prev ->
        if current = 0 then Nothing else move items c ~current (current - 1)
    | Next_wrap ->
        move items c ~current (if current < 0 then 0 else (current + 1) mod n)
    | Prev_wrap ->
        move items c ~current (if current <= 0 then n - 1 else current - 1)
    | First -> move items c ~current 0
    | Last -> move items c ~current (n - 1)
    | By k -> by k
    | Page_down -> by page
    | Page_up -> by (-page)
    | Typeahead u -> typeahead ~name items c ~current n u
    | Activate ->
        if current < 0 then Nothing else Activated (Items.get items current)
    | Toggle ->
        if current < 0 then Nothing
        else
          let x = Items.get items current in
          Changed
            ( Toggle x,
              {
                c with
                hint = current;
                anchor = Some x;
                anchor_hint = current;
                prefix = "";
              } )
    | Extend_next ->
        if current = n - 1 then Nothing
        else extend items c ~equal ~current ~target:(current + 1)
    | Extend_prev ->
        if current = 0 then Nothing
        else extend items c ~equal ~current ~target:(current - 1)
    | Select_all ->
        Changed
          (Add (all items (n - 1) []), { c with anchor = None; prefix = "" })

(* ───── Nearest ───── *)

(* The position of [x] in [before] from [p] on, or -1. *)
let rec find_from equal before x n p =
  if p = n then -1
  else if equal (Items.get before p) x then p
  else find_from equal before x n (p + 1)

(* Walks [items] in order beside a position [p] in [before]: each item found
   at [q] in [before] moves [p] past [q]. The first item found after [k], the
   cursor item's position in [before], follows it; the last found before
   precedes it. An item absent from [before] leaves [p] where it is. *)
let rec neighbour equal before items k nb m i p preceding =
  if i = m then preceding
  else
    match find_from equal before (Items.get items i) nb p with
    | -1 -> neighbour equal before items k nb m (i + 1) p preceding
    | q when q > k -> i
    | q -> neighbour equal before items k nb m (i + 1) (q + 1) i

let nearest ~equal ~before items c =
  match c.item with
  | None -> c
  | Some x -> (
      let reveal i =
        if i < 0 then { c with item = None; prefix = "" }
        else
          {
            (put items c i) with
            anchor = c.anchor;
            anchor_hint = c.anchor_hint;
          }
      in
      match find equal items x c.hint with
      | i when i >= 0 -> if i = c.hint then c else reveal i
      | _ -> (
          match find equal before x c.hint with
          | -1 -> reveal (-1)
          | k ->
              reveal
                (neighbour equal before items k (Items.length before)
                   (Items.length items) 0 0 (-1))))

(* ───── Keys ───── *)

let default_keys =
  Shortcut.
    [
      (down, Next);
      (up, Prev);
      (key Home, First);
      (key End, Last);
      (key Page_down, Page_down);
      (key Page_up, Page_up);
      (enter, Activate);
      (ctrl 'j', Activate);
      (space, (Toggle : action));
      (key ~shift:true Down, Extend_next);
      (key ~shift:true Up, Extend_prev);
      (ctrl 'a', Select_all);
    ]

let wrapping_keys =
  List.map
    (function
      | k, Next -> (k, Next_wrap)
      | k, Prev -> (k, Prev_wrap)
      | binding -> binding)
    default_keys
