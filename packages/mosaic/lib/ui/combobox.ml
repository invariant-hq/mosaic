type 'a t = { text : Editor.t; cursor : 'a Cursor.t; expanded : bool }

let make ?(text = Editor.empty) ?(cursor = Cursor.none) ?(expanded = false) () =
  { text; cursor; expanded }

let text c = c.text
let cursor c = c.cursor
let expanded c = c.expanded
let with_cursor cursor c = { c with cursor }

let edit text c =
  let expanded =
    c.expanded || not (String.equal (Editor.text text) (Editor.text c.text))
  in
  { c with text; expanded }

type action = Next | Prev | Page_down | Page_up | Commit | Dismiss | Leave
type 'a outcome = Changed of 'a t | Committed of 'a * 'a t | Nothing

let list_step ~equal ~name ~page items c action =
  match Cursor.step ~equal ~name ~page items c.cursor action with
  | Cursor.Moved cursor -> Changed { c with cursor }
  | _ -> Nothing

(* Expanding keeps a cursor item that [items] holds and puts the cursor on
   the first item or the last otherwise. *)
let expand ~equal ~name ~page items c action =
  let c = { c with expanded = true } in
  if Option.is_some (Cursor.index ~equal items c.cursor) then Changed c
  else
    match list_step ~equal ~name ~page items c action with
    | Nothing -> Changed c
    | outcome -> outcome

let step ~equal ~name ~page items c action =
  match action with
  | Next when not c.expanded -> expand ~equal ~name ~page items c Next
  | Prev when not c.expanded -> expand ~equal ~name ~page items c Prev
  | Next -> list_step ~equal ~name ~page items c Next
  | Prev -> list_step ~equal ~name ~page items c Prev
  | (Page_down | Page_up) when not c.expanded -> Nothing
  | Page_down -> list_step ~equal ~name ~page items c Page_down
  | Page_up -> list_step ~equal ~name ~page items c Page_up
  | Commit -> (
      match Cursor.index ~equal items c.cursor with
      | Some i when c.expanded ->
          let x = Items.get items i in
          let text =
            Editor.move_end (Editor.insert (name x) (Editor.select_all c.text))
          in
          Committed (x, { c with text; expanded = false })
      | _ -> Nothing)
  | Dismiss ->
      if c.expanded then Changed { c with expanded = false }
      else if Editor.length c.text > 0 then
        Changed { c with text = Editor.clear c.text }
      else Nothing
  | Leave -> if c.expanded then Changed { c with expanded = false } else Nothing

let default_keys =
  Shortcut.
    [
      (down, Next);
      (up, Prev);
      (key Page_down, Page_down);
      (key Page_up, Page_up);
      (enter, Commit);
      (escape, Dismiss);
      (tab, Leave);
      (key ~shift:true Tab, Leave);
    ]
