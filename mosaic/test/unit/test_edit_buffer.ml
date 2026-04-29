open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let sel_testable =
  Testable.make
    ~pp:(fun fmt -> function
      | None -> Format.fprintf fmt "None"
      | Some (a, b) -> Format.fprintf fmt "Some (%d, %d)" a b)
    ~equal:(fun a b ->
      match (a, b) with
      | None, None -> true
      | Some (a1, b1), Some (a2, b2) -> a1 = a2 && b1 = b2
      | _ -> false)
    ()

(* ── Construction ── *)

let create_empty () =
  let buf = Edit_buffer.create "" in
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf);
  equal ~msg:"length" int 0 (Edit_buffer.length buf);
  is_true ~msg:"is_empty" (Edit_buffer.is_empty buf)

let create_hello () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"text" string "hello" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 5 (Edit_buffer.cursor buf);
  equal ~msg:"length" int 5 (Edit_buffer.length buf)

let create_preserves_newlines () =
  let buf = Edit_buffer.create "a\nb\nc" in
  equal ~msg:"text" string "a\nb\nc" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 5 (Edit_buffer.cursor buf)

let create_max_length_truncates_initial () =
  let buf = Edit_buffer.create ~max_length:3 "abcdef" in
  equal ~msg:"length" int 3 (Edit_buffer.length buf);
  equal ~msg:"text" string "abc" (Edit_buffer.text buf)

let create_negative_max_length_clamped () =
  let buf = Edit_buffer.create ~max_length:(-3) "abc" in
  equal ~msg:"max_length clamped" int 0 (Edit_buffer.max_length buf);
  equal ~msg:"content truncated" string "" (Edit_buffer.text buf);
  equal ~msg:"cursor at end" int 0 (Edit_buffer.cursor buf)

let create_multibyte_unicode () =
  (* "café" — the é is a single grapheme *)
  let buf = Edit_buffer.create "caf\xc3\xa9" in
  equal ~msg:"length" int 4 (Edit_buffer.length buf);
  equal ~msg:"text" string "caf\xc3\xa9" (Edit_buffer.text buf)

(* ── Content ── *)

let text_returns_content () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"text" string "hello" (Edit_buffer.text buf)

let set_text_replaces_content () =
  let buf = Edit_buffer.create "old" in
  Edit_buffer.set_text buf "new text";
  equal ~msg:"text" string "new text" (Edit_buffer.text buf);
  equal ~msg:"cursor at end" int 8 (Edit_buffer.cursor buf)

let set_text_enforces_max_length () =
  let buf = Edit_buffer.create ~max_length:5 "old" in
  Edit_buffer.set_text buf "new text";
  equal ~msg:"text truncated" string "new t" (Edit_buffer.text buf);
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let set_text_clears_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  is_true ~msg:"has selection before" (Edit_buffer.has_selection buf);
  Edit_buffer.set_text buf "world";
  is_false ~msg:"no selection after" (Edit_buffer.has_selection buf)

let set_text_preserves_newlines () =
  let buf = Edit_buffer.create "" in
  Edit_buffer.set_text buf "a\nb\nc";
  equal ~msg:"text" string "a\nb\nc" (Edit_buffer.text buf)

let length_returns_grapheme_count () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"length" int 5 (Edit_buffer.length buf)

let display_width_and_is_empty () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"display_width" int 5 (Edit_buffer.display_width buf);
  is_false ~msg:"not empty" (Edit_buffer.is_empty buf);
  let buf2 = Edit_buffer.create "" in
  is_true ~msg:"empty" (Edit_buffer.is_empty buf2)

(* ── Cursor ── *)

let cursor_at_end_after_create () =
  let buf = Edit_buffer.create "abc" in
  equal ~msg:"cursor" int 3 (Edit_buffer.cursor buf)

let set_cursor_moves () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 2;
  equal ~msg:"cursor" int 2 (Edit_buffer.cursor buf)

let set_cursor_clamps_negative () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf (-5);
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let set_cursor_clamps_above_length () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 100;
  equal ~msg:"cursor" int 5 (Edit_buffer.cursor buf)

let set_cursor_clears_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  is_true ~msg:"has selection before" (Edit_buffer.has_selection buf);
  Edit_buffer.set_cursor buf 2;
  is_false ~msg:"no selection after" (Edit_buffer.has_selection buf)

let cursor_display_offset_ascii () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 3;
  equal ~msg:"offset" int 3 (Edit_buffer.cursor_display_offset buf)

(* ── Selection ── *)

let no_selection_initially () =
  let buf = Edit_buffer.create "hello" in
  is_false ~msg:"no selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selection" sel_testable None (Edit_buffer.selection buf)

let select_all_sets_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  equal ~msg:"selection" sel_testable (Some (0, 5)) (Edit_buffer.selection buf);
  equal ~msg:"cursor at end" int 5 (Edit_buffer.cursor buf)

let selected_text_returns_substring () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.select_all buf;
  equal ~msg:"selected" string "hello world" (Edit_buffer.selected_text buf)

let selected_text_no_selection () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"selected" string "" (Edit_buffer.selected_text buf)

let clear_selection_removes_it () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  let cursor_before = Edit_buffer.cursor buf in
  Edit_buffer.clear_selection buf;
  is_false ~msg:"no selection" (Edit_buffer.has_selection buf);
  equal ~msg:"cursor unchanged" int cursor_before (Edit_buffer.cursor buf)

let forward_selection_via_move_right () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let _ = Edit_buffer.move_right ~select:true buf in
  let _ = Edit_buffer.move_right ~select:true buf in
  equal ~msg:"selection" sel_testable (Some (0, 2)) (Edit_buffer.selection buf)

let backward_selection_via_move_left () =
  let buf = Edit_buffer.create "hello" in
  (* cursor starts at 5 *)
  let _ = Edit_buffer.move_left ~select:true buf in
  let _ = Edit_buffer.move_left ~select:true buf in
  equal ~msg:"selection" sel_testable (Some (3, 5)) (Edit_buffer.selection buf)

let selection_normalized () =
  (* Selection built backwards still returns (lo, hi) *)
  let buf = Edit_buffer.create "hello" in
  (* cursor at 5, move left with select: anchor=5, cursor=3 *)
  let _ = Edit_buffer.move_left ~select:true buf in
  let _ = Edit_buffer.move_left ~select:true buf in
  match Edit_buffer.selection buf with
  | Some (lo, hi) -> is_true ~msg:"start < end" (lo < hi)
  | None -> fail "expected selection"

let select_then_move_clears () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let _ = Edit_buffer.move_right ~select:true buf in
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  let _ = Edit_buffer.move_right buf in
  is_false ~msg:"no selection after move" (Edit_buffer.has_selection buf)

let select_all_empty () =
  let buf = Edit_buffer.create "" in
  Edit_buffer.select_all buf;
  (* anchor=0, cursor=0 → selection returns None because lo=hi *)
  is_false ~msg:"no selection on empty" (Edit_buffer.has_selection buf)

(* ── Editing — Insert ── *)

let insert_at_end_appends () =
  let buf = Edit_buffer.create "hello" in
  let changed = Edit_buffer.insert buf " world" in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "hello world" (Edit_buffer.text buf)

let insert_at_middle () =
  let buf = Edit_buffer.create "helo" in
  Edit_buffer.set_cursor buf 2;
  let _ = Edit_buffer.insert buf "l" in
  equal ~msg:"text" string "hello" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 3 (Edit_buffer.cursor buf)

let insert_empty_no_selection_returns_false () =
  let buf = Edit_buffer.create "hello" in
  let changed = Edit_buffer.insert buf "" in
  is_false ~msg:"not changed" changed

let insert_replaces_selection () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.select_all buf;
  let _ = Edit_buffer.insert buf "bye" in
  equal ~msg:"text" string "bye" (Edit_buffer.text buf)

let insert_preserves_newlines () =
  let buf = Edit_buffer.create "" in
  let _ = Edit_buffer.insert buf "a\nb\nc" in
  equal ~msg:"text" string "a\nb\nc" (Edit_buffer.text buf)

let insert_enforces_max_length () =
  let buf = Edit_buffer.create ~max_length:5 "" in
  let _ = Edit_buffer.insert buf "abcdefgh" in
  equal ~msg:"length" int 5 (Edit_buffer.length buf);
  equal ~msg:"text" string "abcde" (Edit_buffer.text buf)

let insert_full_buffer_no_room () =
  let buf = Edit_buffer.create ~max_length:5 "" in
  let _ = Edit_buffer.insert buf "abcde" in
  equal ~msg:"at limit" int 5 (Edit_buffer.length buf);
  (* Insert more with no selection -> false no-op: unchanged and no undo
     point *)
  let changed = Edit_buffer.insert buf "x" in
  is_false ~msg:"returns false" changed;
  equal ~msg:"text unchanged" string "abcde" (Edit_buffer.text buf)

let insert_saves_undo () =
  let buf = Edit_buffer.create "hello" in
  let _ = Edit_buffer.insert buf " world" in
  equal ~msg:"after insert" string "hello world" (Edit_buffer.text buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"after undo" string "hello" (Edit_buffer.text buf)

let insert_clears_redo () =
  let buf = Edit_buffer.create "a" in
  let _ = Edit_buffer.insert buf "b" in
  let _ = Edit_buffer.undo buf in
  equal ~msg:"after undo" string "a" (Edit_buffer.text buf);
  let _ = Edit_buffer.insert buf "c" in
  let redone = Edit_buffer.redo buf in
  is_false ~msg:"redo cleared" redone

let insert_moves_cursor () =
  let buf = Edit_buffer.create "" in
  let _ = Edit_buffer.insert buf "abc" in
  equal ~msg:"cursor" int 3 (Edit_buffer.cursor buf)

(* ── Editing — Delete ── *)

let delete_backward_removes_before_cursor () =
  let buf = Edit_buffer.create "hello" in
  let changed = Edit_buffer.delete_backward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "hell" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 4 (Edit_buffer.cursor buf)

let delete_backward_at_zero () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let changed = Edit_buffer.delete_backward buf in
  is_false ~msg:"not changed" changed

let delete_backward_with_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  let changed = Edit_buffer.delete_backward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "" (Edit_buffer.text buf)

let delete_forward_removes_after_cursor () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let changed = Edit_buffer.delete_forward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "ello" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let delete_forward_at_end () =
  let buf = Edit_buffer.create "hello" in
  let changed = Edit_buffer.delete_forward buf in
  is_false ~msg:"not changed" changed

let delete_forward_with_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.select_all buf;
  let changed = Edit_buffer.delete_forward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "" (Edit_buffer.text buf)

let delete_word_backward_basic () =
  let buf = Edit_buffer.create "hello world" in
  let changed = Edit_buffer.delete_word_backward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "hello " (Edit_buffer.text buf)

let delete_word_backward_repeated () =
  let buf = Edit_buffer.create "hello world test" in
  let changed = Edit_buffer.delete_word_backward buf in
  is_true ~msg:"changed1" changed;
  equal ~msg:"after first" string "hello world " (Edit_buffer.text buf);
  let changed = Edit_buffer.delete_word_backward buf in
  is_true ~msg:"changed2" changed;
  equal ~msg:"after second" string "hello " (Edit_buffer.text buf)

let delete_word_backward_multiple_spaces () =
  (* Cursor after spaces: skip breaks then word (readline-style) *)
  let buf = Edit_buffer.create "hello   world" in
  let changed = Edit_buffer.delete_word_backward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"after first" string "hello   " (Edit_buffer.text buf);
  (* Now cursor is right after spaces — should skip all spaces + word *)
  let changed = Edit_buffer.delete_word_backward buf in
  is_true ~msg:"changed2" changed;
  equal ~msg:"after second" string "" (Edit_buffer.text buf)

let delete_word_backward_at_start () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let changed = Edit_buffer.delete_word_backward buf in
  is_false ~msg:"not changed" changed

let delete_word_forward_basic () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.set_cursor buf 0;
  let changed = Edit_buffer.delete_word_forward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "world" (Edit_buffer.text buf)

let delete_word_forward_at_end () =
  let buf = Edit_buffer.create "hello" in
  let changed = Edit_buffer.delete_word_forward buf in
  is_false ~msg:"not changed" changed

let delete_to_start_basic () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.set_cursor buf 5;
  let changed = Edit_buffer.delete_to_start buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string " world" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let delete_to_end_basic () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.set_cursor buf 5;
  let changed = Edit_buffer.delete_to_end buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "hello" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 5 (Edit_buffer.cursor buf)

let delete_line_clears_all () =
  let buf = Edit_buffer.create "hello world" in
  let changed = Edit_buffer.delete_line buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let delete_line_empty () =
  let buf = Edit_buffer.create "" in
  let changed = Edit_buffer.delete_line buf in
  is_false ~msg:"not changed" changed

(* ── Cursor Movement ── *)

let move_left_decrements () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_left buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 4 (Edit_buffer.cursor buf)

let move_left_at_zero () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_left buf in
  is_false ~msg:"not moved" moved

let move_left_with_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 1;
  let _ = Edit_buffer.move_right ~select:true buf in
  let _ = Edit_buffer.move_right ~select:true buf in
  (* selection is (1, 3), cursor at 3 *)
  let moved = Edit_buffer.move_left buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor at selection start" int 1 (Edit_buffer.cursor buf);
  is_false ~msg:"selection cleared" (Edit_buffer.has_selection buf)

let move_right_increments () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_right buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 1 (Edit_buffer.cursor buf)

let move_right_at_end () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_right buf in
  is_false ~msg:"not moved" moved

let move_right_with_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 1;
  let _ = Edit_buffer.move_right ~select:true buf in
  let _ = Edit_buffer.move_right ~select:true buf in
  (* selection is (1, 3), cursor at 3 *)
  let moved = Edit_buffer.move_right buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor at selection end" int 3 (Edit_buffer.cursor buf);
  is_false ~msg:"selection cleared" (Edit_buffer.has_selection buf)

let move_word_forward_basic () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_word_forward buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"cursor advanced" (Edit_buffer.cursor buf > 0)

let move_word_backward_basic () =
  let buf = Edit_buffer.create "hello world" in
  let moved = Edit_buffer.move_word_backward buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"cursor moved back" (Edit_buffer.cursor buf < 11)

let move_home_moves_to_zero () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_home buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let move_home_at_zero () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_home buf in
  is_false ~msg:"not moved" moved

let move_end_moves_to_length () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_end buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 5 (Edit_buffer.cursor buf)

let move_end_at_end () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_end buf in
  is_false ~msg:"not moved" moved

let move_left_select_creates_selection () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_left ~select:true buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selection" sel_testable (Some (4, 5)) (Edit_buffer.selection buf)

let move_right_select_creates_selection () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_right ~select:true buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selection" sel_testable (Some (0, 1)) (Edit_buffer.selection buf)

let move_home_select () =
  let buf = Edit_buffer.create "hello" in
  let moved = Edit_buffer.move_home ~select:true buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selection" sel_testable (Some (0, 5)) (Edit_buffer.selection buf)

let move_end_select () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_end ~select:true buf in
  is_true ~msg:"moved" moved;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selection" sel_testable (Some (0, 5)) (Edit_buffer.selection buf)

(* ── Undo / Redo ── *)

let undo_no_history () =
  let buf = Edit_buffer.create "hello" in
  let undone = Edit_buffer.undo buf in
  is_false ~msg:"nothing to undo" undone

let insert_then_undo () =
  let buf = Edit_buffer.create "hello" in
  let _ = Edit_buffer.insert buf " world" in
  equal ~msg:"after insert" string "hello world" (Edit_buffer.text buf);
  let undone = Edit_buffer.undo buf in
  is_true ~msg:"undone" undone;
  equal ~msg:"after undo" string "hello" (Edit_buffer.text buf)

let undo_restores_cursor () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor buf 2;
  let _ = Edit_buffer.insert buf "X" in
  equal ~msg:"cursor after insert" int 3 (Edit_buffer.cursor buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"cursor after undo" int 2 (Edit_buffer.cursor buf)

let undo_restores_no_selection () =
  let buf = Edit_buffer.create "hello" in
  let _ = Edit_buffer.insert buf " world" in
  Edit_buffer.select_all buf;
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  let _ = Edit_buffer.undo buf in
  is_false ~msg:"no selection after undo" (Edit_buffer.has_selection buf)

let undo_restores_selection () =
  let buf = Edit_buffer.create "hello world" in
  (* Select "world" (offsets 6..11) *)
  Edit_buffer.set_cursor buf 6;
  Edit_buffer.set_cursor_offset ~select:true buf 11;
  is_true ~msg:"has selection before insert" (Edit_buffer.has_selection buf);
  (* Delete selection via insert (saves undo with selection) *)
  let _ = Edit_buffer.insert buf "earth" in
  is_false ~msg:"no selection after insert" (Edit_buffer.has_selection buf);
  (* Undo should restore the selection *)
  let _ = Edit_buffer.undo buf in
  is_true ~msg:"selection restored after undo" (Edit_buffer.has_selection buf);
  equal ~msg:"selection range"
    (option (pair int int))
    (Some (6, 11))
    (Edit_buffer.selection buf)

let redo_after_undo () =
  let buf = Edit_buffer.create "hello" in
  let _ = Edit_buffer.insert buf " world" in
  let _ = Edit_buffer.undo buf in
  equal ~msg:"after undo" string "hello" (Edit_buffer.text buf);
  let redone = Edit_buffer.redo buf in
  is_true ~msg:"redone" redone;
  equal ~msg:"after redo" string "hello world" (Edit_buffer.text buf)

let redo_no_history () =
  let buf = Edit_buffer.create "hello" in
  let redone = Edit_buffer.redo buf in
  is_false ~msg:"nothing to redo" redone

let edit_after_undo_clears_redo () =
  let buf = Edit_buffer.create "a" in
  let _ = Edit_buffer.insert buf "b" in
  let _ = Edit_buffer.undo buf in
  let _ = Edit_buffer.insert buf "c" in
  let redone = Edit_buffer.redo buf in
  is_false ~msg:"redo cleared" redone

let multiple_undo_steps () =
  let buf = Edit_buffer.create "" in
  let _ = Edit_buffer.insert buf "a" in
  let _ = Edit_buffer.insert buf "b" in
  let _ = Edit_buffer.insert buf "c" in
  equal ~msg:"after inserts" string "abc" (Edit_buffer.text buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"undo 1" string "ab" (Edit_buffer.text buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"undo 2" string "a" (Edit_buffer.text buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"undo 3" string "" (Edit_buffer.text buf)

let delete_backward_then_undo () =
  let buf = Edit_buffer.create "hello" in
  let _ = Edit_buffer.delete_backward buf in
  equal ~msg:"after delete" string "hell" (Edit_buffer.text buf);
  let _ = Edit_buffer.undo buf in
  equal ~msg:"after undo" string "hello" (Edit_buffer.text buf)

let delete_word_backward_then_undo () =
  let buf = Edit_buffer.create "hello world" in
  let _ = Edit_buffer.delete_word_backward buf in
  let after_delete = Edit_buffer.text buf in
  is_true ~msg:"text changed" (not (String.equal after_delete "hello world"));
  let _ = Edit_buffer.undo buf in
  equal ~msg:"after undo" string "hello world" (Edit_buffer.text buf)

(* ── Max Length ── *)

let max_length_returns_configured () =
  let buf = Edit_buffer.create ~max_length:42 "" in
  equal ~msg:"max_length" int 42 (Edit_buffer.max_length buf)

let set_max_length_smaller_truncates () =
  let buf = Edit_buffer.create "hello world" in
  Edit_buffer.set_max_length buf 5;
  equal ~msg:"length" int 5 (Edit_buffer.length buf);
  equal ~msg:"text" string "hello" (Edit_buffer.text buf)

let set_max_length_larger_no_truncate () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_max_length buf 100;
  equal ~msg:"text" string "hello" (Edit_buffer.text buf);
  equal ~msg:"length" int 5 (Edit_buffer.length buf)

let set_max_length_adjusts_cursor () =
  let buf = Edit_buffer.create "hello world" in
  (* cursor is at 11 (end) *)
  Edit_buffer.set_max_length buf 3;
  is_true ~msg:"cursor <= new length"
    (Edit_buffer.cursor buf <= Edit_buffer.length buf)

let set_max_length_negative_clamped () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_max_length buf (-10);
  equal ~msg:"max_length clamped" int 0 (Edit_buffer.max_length buf);
  equal ~msg:"text truncated to zero" string "" (Edit_buffer.text buf);
  equal ~msg:"cursor clamped" int 0 (Edit_buffer.cursor buf)

let insert_at_limit_truncates () =
  let buf = Edit_buffer.create ~max_length:5 "" in
  let _ = Edit_buffer.insert buf "abc" in
  let _ = Edit_buffer.insert buf "defgh" in
  equal ~msg:"length" int 5 (Edit_buffer.length buf);
  equal ~msg:"text" string "abcde" (Edit_buffer.text buf)

(* ── Unicode / Wide Characters ── *)

let wide_chars_length_and_width () =
  (* Two CJK characters: each is 1 grapheme, 2 columns *)
  let buf = Edit_buffer.create "\xe4\xbd\xa0\xe5\xa5\xbd" in
  equal ~msg:"length" int 2 (Edit_buffer.length buf);
  equal ~msg:"display_width" int 4 (Edit_buffer.display_width buf)

let cursor_movement_over_wide_chars () =
  let buf = Edit_buffer.create "\xe4\xbd\xa0\xe5\xa5\xbd" in
  (* cursor at 2 (end) *)
  let moved = Edit_buffer.move_left buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 1 (Edit_buffer.cursor buf);
  let moved2 = Edit_buffer.move_left buf in
  is_true ~msg:"moved again" moved2;
  equal ~msg:"cursor at 0" int 0 (Edit_buffer.cursor buf)

let insert_wide_char () =
  let buf = Edit_buffer.create "" in
  let _ = Edit_buffer.insert buf "\xe4\xbd\xa0" in
  equal ~msg:"length" int 1 (Edit_buffer.length buf);
  equal ~msg:"display_width" int 2 (Edit_buffer.display_width buf)

let delete_wide_char () =
  let buf = Edit_buffer.create "\xe4\xbd\xa0\xe5\xa5\xbd" in
  let _ = Edit_buffer.delete_backward buf in
  equal ~msg:"length" int 1 (Edit_buffer.length buf);
  equal ~msg:"text" string "\xe4\xbd\xa0" (Edit_buffer.text buf)

let mixed_ascii_wide_display_offset () =
  (* "hi你" — 'h' width 1, 'i' width 1, '你' width 2 *)
  let buf = Edit_buffer.create "hi\xe4\xbd\xa0" in
  equal ~msg:"length" int 3 (Edit_buffer.length buf);
  equal ~msg:"display_width" int 4 (Edit_buffer.display_width buf);
  Edit_buffer.set_cursor buf 2;
  equal ~msg:"cursor_display_offset at 2" int 2
    (Edit_buffer.cursor_display_offset buf);
  Edit_buffer.set_cursor buf 3;
  equal ~msg:"cursor_display_offset at 3" int 4
    (Edit_buffer.cursor_display_offset buf)

let emoji_length_and_width () =
  (* Waving hand emoji: single grapheme, display width 2 *)
  let buf = Edit_buffer.create "\xf0\x9f\x91\x8b" in
  equal ~msg:"length" int 1 (Edit_buffer.length buf);
  equal ~msg:"display_width" int 2 (Edit_buffer.display_width buf)

(* ── strip_newlines helper ── *)

let strip_newlines_removes_lf () =
  equal ~msg:"lf" string "abc" (Edit_buffer.strip_newlines "a\nb\nc")

let strip_newlines_removes_cr () =
  equal ~msg:"cr" string "abc" (Edit_buffer.strip_newlines "a\rb\rc")

let strip_newlines_removes_crlf () =
  equal ~msg:"crlf" string "abc" (Edit_buffer.strip_newlines "a\r\nb\r\nc")

let strip_newlines_noop_no_newlines () =
  let s = "hello world" in
  let result = Edit_buffer.strip_newlines s in
  is_true ~msg:"same string" (result == s)

(* ── Multi-line content ── *)

let multiline_create () =
  let buf = Edit_buffer.create "line1\nline2\nline3" in
  equal ~msg:"text" string "line1\nline2\nline3" (Edit_buffer.text buf);
  equal ~msg:"length" int 17 (Edit_buffer.length buf)

let multiline_line_count () =
  let buf = Edit_buffer.create "a\nb\nc" in
  equal ~msg:"line_count" int 3 (Edit_buffer.line_count buf)

let multiline_line_count_single () =
  let buf = Edit_buffer.create "hello" in
  equal ~msg:"line_count" int 1 (Edit_buffer.line_count buf)

let multiline_line_count_empty () =
  let buf = Edit_buffer.create "" in
  equal ~msg:"line_count" int 1 (Edit_buffer.line_count buf)

let multiline_line_count_trailing_newline () =
  let buf = Edit_buffer.create "hello\n" in
  equal ~msg:"line_count" int 2 (Edit_buffer.line_count buf)

(* ── Cursor 2D ── *)

let cursor_row_col_start () =
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 0;
  equal ~msg:"line" int 0 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 0 (Edit_buffer.cursor_col buf)

let cursor_row_col_second_line () =
  (* "abc\ndef\nghi" — cursor at 'd' (pos 4) *)
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 4;
  equal ~msg:"line" int 1 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 0 (Edit_buffer.cursor_col buf)

let cursor_row_col_mid_line () =
  (* "abc\ndef\nghi" — cursor at 'e' (pos 5) *)
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 5;
  equal ~msg:"line" int 1 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 1 (Edit_buffer.cursor_col buf)

let cursor_row_col_at_end () =
  let buf = Edit_buffer.create "abc\ndef" in
  (* cursor at end = pos 7 *)
  equal ~msg:"line" int 1 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 3 (Edit_buffer.cursor_col buf)

let cursor_row_col_at_newline () =
  (* "abc\ndef" — cursor on the newline char itself (pos 3) *)
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 3;
  equal ~msg:"line" int 0 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 3 (Edit_buffer.cursor_col buf)

let cursor_row_col_after_insert_newline () =
  let buf = Edit_buffer.create "ab" in
  Edit_buffer.set_cursor buf 1;
  let _ = Edit_buffer.insert buf "\n" in
  (* now content is "a\nb", cursor at 2 *)
  equal ~msg:"text" string "a\nb" (Edit_buffer.text buf);
  equal ~msg:"line" int 1 (Edit_buffer.cursor_line buf);
  equal ~msg:"col" int 0 (Edit_buffer.cursor_col buf)

(* ── Line movement ── *)

let move_line_start_basic () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 5;
  (* cursor at 'e' on line 1 *)
  let moved = Edit_buffer.move_line_start buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 4 (Edit_buffer.cursor buf)

let move_line_start_already_at_start () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let moved = Edit_buffer.move_line_start buf in
  is_false ~msg:"not moved" moved

let move_line_start_first_line () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 2;
  let moved = Edit_buffer.move_line_start buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 0 (Edit_buffer.cursor buf)

let move_line_start_with_select () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 6;
  let _ = Edit_buffer.move_line_start ~select:true buf in
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected" string "de" (Edit_buffer.selected_text buf)

let move_line_end_basic () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let moved = Edit_buffer.move_line_end buf in
  is_true ~msg:"moved" moved;
  equal ~msg:"cursor" int 7 (Edit_buffer.cursor buf)

let move_line_end_first_line () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 0;
  let moved = Edit_buffer.move_line_end buf in
  is_true ~msg:"moved" moved;
  (* end of first line is before the newline = pos 3 *)
  equal ~msg:"cursor" int 3 (Edit_buffer.cursor buf)

let move_line_end_already_at_end () =
  let buf = Edit_buffer.create "abc\ndef" in
  (* cursor at end of buffer on line 1 *)
  let moved = Edit_buffer.move_line_end buf in
  is_false ~msg:"not moved" moved

let move_line_end_with_select () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let _ = Edit_buffer.move_line_end ~select:true buf in
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected" string "def" (Edit_buffer.selected_text buf)

(* ── Line deletion ── *)

let delete_to_line_start_basic () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 6;
  let changed = Edit_buffer.delete_to_line_start buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abc\nf" (Edit_buffer.text buf);
  equal ~msg:"cursor" int 4 (Edit_buffer.cursor buf)

let delete_to_line_start_at_line_start () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let changed = Edit_buffer.delete_to_line_start buf in
  is_false ~msg:"not changed" changed

let delete_to_line_end_basic () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let changed = Edit_buffer.delete_to_line_end buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abc\n" (Edit_buffer.text buf)

let delete_to_line_end_at_line_end () =
  (* When cursor is at end of line, delete_to_line_end does nothing *)
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 3;
  let changed = Edit_buffer.delete_to_line_end buf in
  is_false ~msg:"not changed" changed;
  equal ~msg:"text unchanged" string "abc\ndef" (Edit_buffer.text buf)

let delete_to_line_end_at_buffer_end () =
  let buf = Edit_buffer.create "abc\ndef" in
  (* cursor at end of buffer *)
  let changed = Edit_buffer.delete_to_line_end buf in
  is_false ~msg:"not changed" changed

(* ── delete_line multi-line ── *)

let delete_line_first_line () =
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 1;
  let changed = Edit_buffer.delete_line buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "def\nghi" (Edit_buffer.text buf)

let delete_line_middle_line () =
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 5;
  let changed = Edit_buffer.delete_line buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abc\nghi" (Edit_buffer.text buf)

let delete_line_last_line () =
  let buf = Edit_buffer.create "abc\ndef\nghi" in
  Edit_buffer.set_cursor buf 9;
  let changed = Edit_buffer.delete_line buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abc\ndef" (Edit_buffer.text buf)

(* ── Newline editing ── *)

let insert_newline () =
  let buf = Edit_buffer.create "abcd" in
  Edit_buffer.set_cursor buf 2;
  let changed = Edit_buffer.insert buf "\n" in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "ab\ncd" (Edit_buffer.text buf);
  equal ~msg:"line_count" int 2 (Edit_buffer.line_count buf)

let backspace_at_line_start_joins () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 4;
  let changed = Edit_buffer.delete_backward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abcdef" (Edit_buffer.text buf);
  equal ~msg:"line_count" int 1 (Edit_buffer.line_count buf)

let delete_at_line_end_joins () =
  let buf = Edit_buffer.create "abc\ndef" in
  Edit_buffer.set_cursor buf 3;
  let changed = Edit_buffer.delete_forward buf in
  is_true ~msg:"changed" changed;
  equal ~msg:"text" string "abcdef" (Edit_buffer.text buf);
  equal ~msg:"line_count" int 1 (Edit_buffer.line_count buf)

(* ── set_cursor_offset ── *)

let set_cursor_offset_basic () =
  let buf = Edit_buffer.create "hello" in
  Edit_buffer.set_cursor_offset buf 2;
  equal ~msg:"cursor" int 2 (Edit_buffer.cursor buf);
  is_false ~msg:"no selection" (Edit_buffer.has_selection buf)

let set_cursor_offset_with_select () =
  let buf = Edit_buffer.create "hello" in
  (* cursor starts at 5 (end), anchor=5 after select, cursor moves to 2 *)
  Edit_buffer.set_cursor_offset ~select:true buf 2;
  equal ~msg:"cursor" int 2 (Edit_buffer.cursor buf);
  is_true ~msg:"has selection" (Edit_buffer.has_selection buf);
  equal ~msg:"selected" string "llo" (Edit_buffer.selected_text buf)

let set_cursor_offset_clamps () =
  let buf = Edit_buffer.create "abc" in
  Edit_buffer.set_cursor_offset buf 100;
  equal ~msg:"clamped" int 3 (Edit_buffer.cursor buf);
  Edit_buffer.set_cursor_offset buf (-5);
  equal ~msg:"clamped neg" int 0 (Edit_buffer.cursor buf)

(* ── Runner ── *)

let () =
  run "mosaic.edit-buffer"
    [
      group "Construction"
        [
          test "create empty" create_empty;
          test "create hello" create_hello;
          test "create preserves newlines" create_preserves_newlines;
          test "create max_length truncates initial"
            create_max_length_truncates_initial;
          test "create negative max_length is clamped"
            create_negative_max_length_clamped;
          test "create multibyte Unicode" create_multibyte_unicode;
        ];
      group "Content"
        [
          test "text returns content" text_returns_content;
          test "set_text replaces content" set_text_replaces_content;
          test "set_text enforces max_length" set_text_enforces_max_length;
          test "set_text clears selection" set_text_clears_selection;
          test "set_text preserves newlines" set_text_preserves_newlines;
          test "length returns grapheme count" length_returns_grapheme_count;
          test "display_width and is_empty" display_width_and_is_empty;
        ];
      group "Cursor"
        [
          test "cursor at end after create" cursor_at_end_after_create;
          test "set_cursor moves" set_cursor_moves;
          test "set_cursor clamps negative" set_cursor_clamps_negative;
          test "set_cursor clamps above length" set_cursor_clamps_above_length;
          test "set_cursor clears selection" set_cursor_clears_selection;
          test "cursor_display_offset with ASCII" cursor_display_offset_ascii;
        ];
      group "Selection"
        [
          test "no selection initially" no_selection_initially;
          test "select_all sets selection" select_all_sets_selection;
          test "selected_text returns substring" selected_text_returns_substring;
          test "selected_text no selection" selected_text_no_selection;
          test "clear_selection removes it" clear_selection_removes_it;
          test "forward selection via move_right"
            forward_selection_via_move_right;
          test "backward selection via move_left"
            backward_selection_via_move_left;
          test "selection normalized" selection_normalized;
          test "select then move clears" select_then_move_clears;
          test "select_all on empty" select_all_empty;
        ];
      group "Editing — Insert"
        [
          test "insert at end appends" insert_at_end_appends;
          test "insert at middle" insert_at_middle;
          test "insert empty no selection returns false"
            insert_empty_no_selection_returns_false;
          test "insert replaces selection" insert_replaces_selection;
          test "insert preserves newlines" insert_preserves_newlines;
          test "insert enforces max_length" insert_enforces_max_length;
          test "insert full buffer no room" insert_full_buffer_no_room;
          test "insert saves undo" insert_saves_undo;
          test "insert clears redo" insert_clears_redo;
          test "insert moves cursor" insert_moves_cursor;
        ];
      group "Editing — Delete"
        [
          test "delete_backward removes before cursor"
            delete_backward_removes_before_cursor;
          test "delete_backward at 0" delete_backward_at_zero;
          test "delete_backward with selection" delete_backward_with_selection;
          test "delete_forward removes after cursor"
            delete_forward_removes_after_cursor;
          test "delete_forward at end" delete_forward_at_end;
          test "delete_forward with selection" delete_forward_with_selection;
          test "delete_word_backward basic" delete_word_backward_basic;
          test "delete_word_backward repeated" delete_word_backward_repeated;
          test "delete_word_backward multiple spaces"
            delete_word_backward_multiple_spaces;
          test "delete_word_backward at start" delete_word_backward_at_start;
          test "delete_word_forward basic" delete_word_forward_basic;
          test "delete_word_forward at end" delete_word_forward_at_end;
          test "delete_to_start basic" delete_to_start_basic;
          test "delete_to_end basic" delete_to_end_basic;
          test "delete_line clears all" delete_line_clears_all;
          test "delete_line empty" delete_line_empty;
        ];
      group "Cursor Movement"
        [
          test "move_left decrements" move_left_decrements;
          test "move_left at 0" move_left_at_zero;
          test "move_left with selection" move_left_with_selection;
          test "move_right increments" move_right_increments;
          test "move_right at end" move_right_at_end;
          test "move_right with selection" move_right_with_selection;
          test "move_word_forward basic" move_word_forward_basic;
          test "move_word_backward basic" move_word_backward_basic;
          test "move_home moves to 0" move_home_moves_to_zero;
          test "move_home at 0" move_home_at_zero;
          test "move_end moves to length" move_end_moves_to_length;
          test "move_end at end" move_end_at_end;
          test "move_left ~select creates selection"
            move_left_select_creates_selection;
          test "move_right ~select creates selection"
            move_right_select_creates_selection;
          test "move_home ~select selects to start" move_home_select;
          test "move_end ~select selects to end" move_end_select;
        ];
      group "Undo / Redo"
        [
          test "undo with no history" undo_no_history;
          test "insert then undo" insert_then_undo;
          test "undo restores cursor" undo_restores_cursor;
          test "undo restores no selection" undo_restores_no_selection;
          test "undo restores selection" undo_restores_selection;
          test "redo after undo" redo_after_undo;
          test "redo with no history" redo_no_history;
          test "edit after undo clears redo" edit_after_undo_clears_redo;
          test "multiple undo steps" multiple_undo_steps;
          test "delete_backward then undo" delete_backward_then_undo;
          test "delete_word_backward then undo" delete_word_backward_then_undo;
        ];
      group "Max Length"
        [
          test "max_length returns configured" max_length_returns_configured;
          test "set_max_length smaller truncates"
            set_max_length_smaller_truncates;
          test "set_max_length larger no truncate"
            set_max_length_larger_no_truncate;
          test "set_max_length adjusts cursor" set_max_length_adjusts_cursor;
          test "set_max_length negative is clamped"
            set_max_length_negative_clamped;
          test "insert at limit truncates" insert_at_limit_truncates;
        ];
      group "Unicode / Wide Characters"
        [
          test "wide chars length and width" wide_chars_length_and_width;
          test "cursor movement over wide chars" cursor_movement_over_wide_chars;
          test "insert wide char" insert_wide_char;
          test "delete wide char" delete_wide_char;
          test "mixed ASCII + wide display offset"
            mixed_ascii_wide_display_offset;
          test "emoji length and width" emoji_length_and_width;
        ];
      group "strip_newlines helper"
        [
          test "removes LF" strip_newlines_removes_lf;
          test "removes CR" strip_newlines_removes_cr;
          test "removes CRLF" strip_newlines_removes_crlf;
          test "noop when no newlines" strip_newlines_noop_no_newlines;
        ];
      group "Multi-line content"
        [
          test "create with newlines" multiline_create;
          test "line_count multi" multiline_line_count;
          test "line_count single" multiline_line_count_single;
          test "line_count empty" multiline_line_count_empty;
          test "line_count trailing newline"
            multiline_line_count_trailing_newline;
        ];
      group "Cursor 2D"
        [
          test "cursor_row/col at start" cursor_row_col_start;
          test "cursor_row/col second line" cursor_row_col_second_line;
          test "cursor_row/col mid line" cursor_row_col_mid_line;
          test "cursor_row/col at end" cursor_row_col_at_end;
          test "cursor_row/col at newline" cursor_row_col_at_newline;
          test "cursor_row/col after insert newline"
            cursor_row_col_after_insert_newline;
        ];
      group "Line movement"
        [
          test "move_line_start basic" move_line_start_basic;
          test "move_line_start already at start"
            move_line_start_already_at_start;
          test "move_line_start first line" move_line_start_first_line;
          test "move_line_start with select" move_line_start_with_select;
          test "move_line_end basic" move_line_end_basic;
          test "move_line_end first line" move_line_end_first_line;
          test "move_line_end already at end" move_line_end_already_at_end;
          test "move_line_end with select" move_line_end_with_select;
        ];
      group "Line deletion"
        [
          test "delete_to_line_start basic" delete_to_line_start_basic;
          test "delete_to_line_start at line start"
            delete_to_line_start_at_line_start;
          test "delete_to_line_end basic" delete_to_line_end_basic;
          test "delete_to_line_end at line end" delete_to_line_end_at_line_end;
          test "delete_to_line_end at buffer end"
            delete_to_line_end_at_buffer_end;
        ];
      group "delete_line multi-line"
        [
          test "first line" delete_line_first_line;
          test "middle line" delete_line_middle_line;
          test "last line" delete_line_last_line;
        ];
      group "Newline editing"
        [
          test "insert newline" insert_newline;
          test "backspace at line start joins" backspace_at_line_start_joins;
          test "delete at line end joins" delete_at_line_end_joins;
        ];
      group "set_cursor_offset"
        [
          test "basic" set_cursor_offset_basic;
          test "with select" set_cursor_offset_with_select;
          test "clamps" set_cursor_offset_clamps;
        ];
    ]
