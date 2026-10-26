open Windtrap
open Mosaic_ui

let models = [| "haiku"; "opus"; "sonnet" |]
let items = Items.of_array models
let step c a = Combobox.step ~equal:String.equal ~name:Fun.id ~page:10 items c a

let changed = function
  | Combobox.Changed c -> c
  | Committed _ -> fail "a commit"
  | Nothing -> fail "nothing"

let cursor_item c = Cursor.item (Combobox.cursor c)
let typed s = Combobox.edit (Editor.of_string s) (Combobox.make ())

let editing () =
  let c = typed "so" in
  is_true ~msg:"typing expands the list" (Combobox.expanded c);
  equal string "so" (Editor.text (Combobox.text c));
  let closed = Combobox.make ~text:(Editor.of_string "so") () in
  is_false ~msg:"a caret move keeps the list collapsed"
    (Combobox.expanded
       (Combobox.edit (Editor.move_home (Combobox.text closed)) closed));
  let on = Combobox.make ~cursor:(Cursor.at "opus") () in
  equal ~msg:"editing keeps the cursor" (option string) (Some "opus")
    (cursor_item (Combobox.edit (Editor.of_string "o") on))

let opening () =
  let c = changed (step (Combobox.make ()) Next) in
  is_true ~msg:"Down expands" (Combobox.expanded c);
  equal ~msg:"on the first item" (option string) (Some "haiku") (cursor_item c);
  let c = changed (step c Next) in
  equal ~msg:"then moves" (option string) (Some "opus") (cursor_item c);
  let up = changed (step (Combobox.make ()) Prev) in
  equal ~msg:"Up expands on the last item" (option string) (Some "sonnet")
    (cursor_item up);
  let kept =
    changed (step (Combobox.make ~cursor:(Cursor.at "opus") ()) Next)
  in
  equal ~msg:"an expanding Down keeps a cursor item that is shown"
    (option string) (Some "opus") (cursor_item kept);
  is_true ~msg:"PageDown on a collapsed list does nothing"
    (step (Combobox.make ()) Page_down = Nothing)

let committing () =
  let c = changed (step (changed (step (typed "o") Next)) Next) in
  match step c Commit with
  | Committed (x, c) ->
      equal string "opus" x;
      equal ~msg:"the text is the item's name" string "opus"
        (Editor.text (Combobox.text c));
      equal ~msg:"caret at the end" int 4 (Editor.cursor (Combobox.text c));
      is_none ~msg:"no selection" (Editor.selection (Combobox.text c));
      is_false ~msg:"collapsed" (Combobox.expanded c);
      equal ~msg:"undo gives the typed text back" string "o"
        (Editor.text (Editor.undo (Combobox.text c)));
      is_true ~msg:"Enter on a collapsed list does nothing"
        (step c Commit = Nothing)
  | _ -> fail "a commit"

let escaping () =
  let c = changed (step (typed "so") Next) in
  let c = changed (step c Dismiss) in
  is_false ~msg:"Escape collapses" (Combobox.expanded c);
  equal string "so" (Editor.text (Combobox.text c));
  let c = changed (step c Dismiss) in
  equal ~msg:"a second Escape clears" string "" (Editor.text (Combobox.text c));
  is_true ~msg:"a third does nothing" (step c Dismiss = Nothing)

let leaving () =
  let c = changed (step (typed "so") Next) in
  let left = changed (step c Leave) in
  is_false ~msg:"Tab collapses" (Combobox.expanded left);
  equal ~msg:"without committing" string "so" (Editor.text (Combobox.text left));
  is_true ~msg:"Tab on a collapsed list does nothing" (step left Leave = Nothing)

let keys () =
  let bound s =
    List.find_map
      (fun (s', a) -> if Shortcut.equal s s' then Some a else None)
      Combobox.default_keys
  in
  is_true (bound Shortcut.down = Some Combobox.Next);
  is_true (bound Shortcut.up = Some Combobox.Prev);
  is_true (bound Shortcut.enter = Some Combobox.Commit);
  is_true (bound Shortcut.escape = Some Combobox.Dismiss);
  is_true (bound Shortcut.tab = Some Combobox.Leave);
  is_true (bound (Shortcut.key ~shift:true Tab) = Some Combobox.Leave);
  is_true ~msg:"letters go to the text" (bound (Shortcut.char 'a') = None)

let () =
  run "mosaic.combobox"
    [
      group "Combobox"
        [
          test "editing" editing;
          test "Down and Up" opening;
          test "Enter commits" committing;
          test "Escape" escaping;
          test "Tab leaves" leaving;
          test "keys" keys;
        ];
    ]
