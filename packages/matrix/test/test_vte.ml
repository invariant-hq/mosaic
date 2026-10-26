module Grid = Matrix_grid
open Windtrap

(** Test utilities *)

(* A resize reports nothing while mode 2048 is off. *)
let resize vte ~rows ~cols =
  match Vte.resize vte ~rows ~cols with
  | [] -> ()
  | _ -> fail "a resize report without mode 2048"

(* Feed output whose replies the test does not read. *)
let feed vte s = ignore (Vte.feed_string vte s : _ list)

(* The bytes of the replies to output, as a program reads them. *)
let answers vte s = Matrix_input.Reply.encode (Vte.feed_string vte s)

(* The scrollback's lines, oldest first. *)
let scrollback_lines vte =
  let n = Vte.scrollback_size vte in
  List.init n (fun i -> Vte.text vte (i - n))

let get_char_at grid row col =
  let idx = (row * Grid.width grid) + col in
  let s = Grid.get_text grid idx in
  if s = "" then ' ' else s.[0]

let get_line grid row =
  let buf = Buffer.create (Grid.width grid) in
  for col = 0 to Grid.width grid - 1 do
    Buffer.add_char buf (get_char_at grid row col)
  done;
  let str = Buffer.contents buf in
  (* Trim trailing spaces *)
  let rec find_end i =
    if i < 0 then 0 else if str.[i] = ' ' then find_end (i - 1) else i + 1
  in
  String.sub str 0 (find_end (String.length str - 1))

(** {1 Creation and Configuration} *)

let create_with_defaults () =
  let vte = Vte.create ~rows:24 ~cols:80 () in
  equal ~msg:"rows" int 24 (Vte.rows vte);
  equal ~msg:"cols" int 80 (Vte.cols vte);
  equal ~msg:"scrollback capacity" int 10000 (Vte.scrollback_capacity vte);
  equal ~msg:"scrollback size" int 0 (Vte.scrollback_size vte);
  is_false ~msg:"not dirty initially" (Vte.is_dirty vte);
  is_false ~msg:"not alternate screen" (Vte.modes vte).alternate_screen;
  equal ~msg:"cursor at origin" (pair int int) (0, 0) (Vte.cursor_pos vte);
  is_true ~msg:"cursor visible" (Vte.modes vte).cursor_visible

let create_with_no_scrollback () =
  let vte = Vte.create ~scrollback:0 ~rows:10 ~cols:20 () in
  equal ~msg:"no scrollback capacity" int 0 (Vte.scrollback_capacity vte);
  equal ~msg:"no scrollback size" int 0 (Vte.scrollback_size vte)

let create_with_custom_scrollback () =
  let vte = Vte.create ~scrollback:500 ~rows:10 ~cols:20 () in
  equal ~msg:"custom scrollback capacity" int 500 (Vte.scrollback_capacity vte)

(** {1 Basic Text Input} *)

let feed_simple_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello World";
  is_true ~msg:"marked dirty" (Vte.is_dirty vte);
  let line = get_line (Vte.grid vte) 0 in
  equal ~msg:"text rendered" string "Hello World" line;
  equal ~msg:"cursor advanced" (pair int int) (0, 11) (Vte.cursor_pos vte)

let feed_multiline_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3";
  equal ~msg:"first line" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line" string "Line2" (get_line (Vte.grid vte) 1);
  equal ~msg:"third line" string "Line3" (get_line (Vte.grid vte) 2);
  equal ~msg:"cursor position" (pair int int) (2, 5) (Vte.cursor_pos vte)

let feed_carriage_return () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\rWorld";
  equal ~msg:"CR overwrites" string "World" (get_line (Vte.grid vte) 0);
  equal ~msg:"cursor after CR" (pair int int) (0, 5) (Vte.cursor_pos vte)

let feed_tab () =
  let vte = Vte.create ~rows:5 ~cols:40 () in
  feed vte "A\tB\tC";
  equal ~msg:"first char" char 'A' (get_char_at (Vte.grid vte) 0 0);
  equal ~msg:"after first tab" char 'B' (get_char_at (Vte.grid vte) 0 8);
  equal ~msg:"after second tab" char 'C' (get_char_at (Vte.grid vte) 0 16)

let tab_stops () =
  let line ?(cols = 20) s =
    let vte = Vte.create ~rows:2 ~cols () in
    feed vte s;
    Vte.to_string vte
  in
  let row0 s = s ^ "\n" ^ String.make 20 ' ' in
  equal ~msg:"HTS sets a stop" string
    (row0 "   Y                ")
    (line "\x1b[1;4H\x1bH\r\tY");
  equal ~msg:"TBC 0 clears the stop at the cursor" string
    (row0 "                Y   ")
    (line "\x1b[1;9H\x1b[g\r\tY");
  equal ~msg:"TBC 3 clears every stop" string
    (row0 "                   Y")
    (line "\x1b[3g\tY");
  equal ~msg:"CHT" string (row0 "                Y   ") (line "\x1b[2IY");
  equal ~msg:"CBT" string
    (row0 "        Y           ")
    (line "\x1b[1;15H\x1b[ZY");
  equal ~msg:"HT keeps a pending wrap" string "0123456789\nX         "
    (line ~cols:10 "0123456789\tX")

let feed_backspace () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\bX";
  equal ~msg:"backspace moves cursor" string "HellX" (get_line (Vte.grid vte) 0)

(** {1 Cursor Movement} *)

let cursor_up () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2";
  feed vte "\x1b[A";
  feed vte "X";
  equal ~msg:"cursor up modifies previous line" string "Line1X"
    (get_line (Vte.grid vte) 0)

let cursor_down () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\x1b[B\rLine2";
  equal ~msg:"first line" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line" string "Line2" (get_line (Vte.grid vte) 1)

let cursor_forward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\r\x1b[3CX";
  equal ~msg:"cursor forward" string "HelXo" (get_line (Vte.grid vte) 0)

let cursor_backward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\x1b[2DX";
  equal ~msg:"cursor backward" string "HelXo" (get_line (Vte.grid vte) 0)

let cursor_position () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  feed vte "\x1b[3;5HX";
  equal ~msg:"positioned character" char 'X' (get_char_at (Vte.grid vte) 2 4)

let origin_mode_positions () =
  (* Region rows 2..3 (1-based): in origin mode CUP and VPA count from the top
     margin and stay inside the region, and setting the mode homes the
     cursor there. *)
  let pos s =
    let vte = Vte.create ~rows:4 ~cols:3 () in
    feed vte ("\x1b[2;3r\x1b[?6h" ^ s);
    Vte.cursor_pos vte
  in
  let vte = Vte.create ~rows:4 ~cols:3 () in
  feed vte "\x1b[?6h";
  is_true ~msg:"DECOM is reported" (Vte.modes vte).origin;
  equal ~msg:"DECOM homes to the top margin" (pair int int) (1, 0) (pos "");
  equal ~msg:"CUP is relative" (pair int int) (1, 0) (pos "\x1b[1;1H");
  equal ~msg:"CUP is clamped" (pair int int) (2, 0) (pos "\x1b[9;1H");
  equal ~msg:"VPA is relative" (pair int int) (2, 0) (pos "\x1b[2d");
  equal ~msg:"DECSTBM homes to the top margin" (pair int int) (2, 0)
    (pos "\x1b[3;4r");
  equal ~msg:"DECRC restores the mode" (pair int int) (0, 0)
    (pos "\x1b[?6l\x1b7\x1b[?6h\x1b8\x1b[1;1H");
  equal ~msg:"leaving DECOM homes the cursor" (pair int int) (0, 0)
    (pos "\x1b[2;2H\x1b[?6l")

let cursor_horizontal_absolute () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\x1b[10GX";
  equal ~msg:"horizontal absolute" char 'X' (get_char_at (Vte.grid vte) 0 9)

let ecma48_position_forms () =
  let screen s =
    let vte = Vte.create ~rows:4 ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  let blank = "      " in
  equal ~msg:"HPA" string
    (String.concat "\n" [ "aXc   "; blank; blank; blank ])
    (screen "abc\x1b[2`X");
  equal ~msg:"HPR" string
    (String.concat "\n" [ "abc  X"; blank; blank; blank ])
    (screen "abc\x1b[2aX");
  equal ~msg:"VPR" string
    (String.concat "\n" [ "ab    "; blank; "  X   "; blank ])
    (screen "ab\x1b[2eX");
  (* Unlike CUD, VPR is not stopped by the bottom margin. *)
  equal ~msg:"VPR crosses the bottom margin" string
    (String.concat "\n" [ blank; blank; blank; "X     " ])
    (screen "\x1b[1;2r\x1b[3eX")

let cursor_next_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\x1b[EX";
  equal ~msg:"next line moves down and to column 0" (pair int int) (1, 1)
    (Vte.cursor_pos vte)

let cursor_previous_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\r\nHello\x1b[FX";
  equal ~msg:"previous line moves up and to column 0" (pair int int) (0, 1)
    (Vte.cursor_pos vte)

let vertical_moves_stop_at_margins () =
  (* Region rows 2..3 (1-based): a move that starts inside it stops at its
     margin; one that starts outside stops at the screen edge. *)
  let pos s =
    let vte = Vte.create ~rows:4 ~cols:3 () in
    feed vte ("\x1b[2;3r" ^ s);
    Vte.cursor_pos vte
  in
  equal ~msg:"CUU stops at the top margin" (pair int int) (1, 0)
    (pos "\x1b[3;1H\x1b[5A");
  equal ~msg:"CPL stops at the top margin" (pair int int) (1, 0)
    (pos "\x1b[3;2H\x1b[5F");
  equal ~msg:"CUD stops at the bottom margin" (pair int int) (2, 0)
    (pos "\x1b[2;1H\x1b[5B");
  equal ~msg:"CUD from above the region" (pair int int) (2, 0) (pos "\x1b[5B");
  equal ~msg:"CNL stops at the bottom margin" (pair int int) (2, 0)
    (pos "\x1b[2;2H\x1b[5E");
  equal ~msg:"CUU from above the region" (pair int int) (0, 0)
    (pos "\x1b[1;1H\x1b[5A");
  equal ~msg:"CUD from below the region" (pair int int) (3, 0)
    (pos "\x1b[4;1H\x1b[5B")

(** {1 Screen Manipulation} *)

let erase_display_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3\x1b[2;5H\x1b[J";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line partial" string "Line" (get_line (Vte.grid vte) 1);
  equal ~msg:"third line cleared" string "" (get_line (Vte.grid vte) 2)

let erase_display_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3\x1b[2;3H\x1b[1J";
  equal ~msg:"first line cleared" string "" (get_line (Vte.grid vte) 0);
  let line2 = get_line (Vte.grid vte) 1 in
  satisfies ~claim:"second line partially cleared (at least 2 chars left)"
    string
    (fun s -> String.length s >= 2)
    line2;
  equal ~msg:"third line unchanged" string "Line3" (get_line (Vte.grid vte) 2)

let erase_display_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3\x1b[2J";
  equal ~msg:"line 0 cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"line 1 cleared" string "" (get_line (Vte.grid vte) 1);
  equal ~msg:"line 2 cleared" string "" (get_line (Vte.grid vte) 2)

let erase_line_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello World\r\x1b[5C\x1b[K";
  equal ~msg:"erase to end of line" string "Hello" (get_line (Vte.grid vte) 0)

let erase_line_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello World\r\x1b[5C\x1b[1K";
  let line = get_line (Vte.grid vte) 0 in
  is_true ~msg:"beginning erased" (String.sub line 0 5 = "     ");
  equal ~msg:"end preserved" string " World" (String.sub line 5 6)

let erase_line_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello World\r\x1b[5C\x1b[2K";
  equal ~msg:"entire line erased" string "" (get_line (Vte.grid vte) 0)

let erase_characters () =
  let screen s =
    let vte = Vte.create ~rows:1 ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"ECH erases from the cursor" string "a  def"
    (screen "abcdef\x1b[1;2H\x1b[2X");
  equal ~msg:"ECH stops at the end of the line" string "abcd  "
    (screen "abcdef\x1b[1;5H\x1b[99X");
  equal ~msg:"ECH blanks a wide character it cuts" string "a  b  "
    (screen "a\xe4\xb8\xadb\x1b[1;3H\x1b[X")

let default_background_spaces_overwrite_text () =
  let vte = Vte.create ~rows:3 ~cols:30 () in
  let replacement = " 1 + let status" in
  feed vte "› Find and fix a bug";
  feed vte ("\x1b[1;1H\x1b[49m" ^ replacement);
  let line = get_line (Vte.grid vte) 0 in
  equal ~msg:"leading and interior spaces overwrite previous glyphs" string
    replacement
    (String.sub line 0 (String.length replacement))

let insert_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3\x1b[2;1H\x1b[LNew";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"new line inserted" string "New" (get_line (Vte.grid vte) 1);
  equal ~msg:"second line pushed down" string "Line2"
    (get_line (Vte.grid vte) 2)

let delete_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nLine2\r\nLine3\x1b[2;1H\x1b[M";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"third line moved up" string "Line3" (get_line (Vte.grid vte) 1);
  equal ~msg:"bottom line empty" string "" (get_line (Vte.grid vte) 2)

let insert_delete_lines_in_region () =
  (* Rows 1 to 4 with the region on rows 2 and 3: IL and DL act only from
     inside the region, and return the cursor to the first column. *)
  let run s =
    let vte = Vte.create ~rows:4 ~cols:3 () in
    feed vte ("1\r\n2\r\n3\r\n4\x1b[2;3r" ^ s);
    (Vte.to_string vte, Vte.cursor_pos vte)
  in
  equal ~msg:"IL"
    (pair string (pair int int))
    ("1  \n   \n2  \n4  ", (1, 0))
    (run "\x1b[2;3H\x1b[L");
  equal ~msg:"DL"
    (pair string (pair int int))
    ("1  \n3  \n   \n4  ", (1, 0))
    (run "\x1b[2;3H\x1b[M");
  equal ~msg:"IL above the region"
    (pair string (pair int int))
    ("1  \n2  \n3  \n4  ", (0, 1))
    (run "\x1b[1;2H\x1b[L");
  equal ~msg:"DL below the region"
    (pair string (pair int int))
    ("1  \n2  \n3  \n4  ", (3, 1))
    (run "\x1b[4;2H\x1b[M")

let insert_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello\r\x1b[3C\x1b[2@XX";
  let line = get_line (Vte.grid vte) 0 in
  satisfies ~claim:"characters inserted (at least 7 chars)" string
    (fun s -> String.length s >= 7)
    line;
  equal ~msg:"insert at position" string "HelXXlo" (String.sub line 0 7)

let delete_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello World\r\x1b[7C\x1b[5P";
  equal ~msg:"characters deleted" string "Hello W" (get_line (Vte.grid vte) 0)

let repeat_preceding_character () =
  let screen s =
    let vte = Vte.create ~rows:2 ~cols:5 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"REP" string "abbbb\n     " (screen "ab\x1b[3b");
  equal ~msg:"REP wraps" string "abbbb\nbb   " (screen "ab\x1b[5b");
  equal ~msg:"REP copies the combining marks" string
    "e\xcc\x81e\xcc\x81e\xcc\x81e\xcc\x81 \n     "
    (screen "e\xcc\x81\x1b[3b");
  equal ~msg:"REP after a control repeats nothing" string "a    \n     "
    (screen "a\r\x1b[3b")

let backspace_moves_without_erasing () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  feed vte "ABC";
  feed vte "\b";
  let grid = Vte.grid vte in
  equal ~msg:"A kept" char 'A' (get_char_at grid 0 0);
  equal ~msg:"B kept" char 'B' (get_char_at grid 0 1);
  equal ~msg:"cell under cursor kept" char 'C' (get_char_at grid 0 2);
  equal ~msg:"cursor moved left" (pair int int) (0, 2) (Vte.cursor_pos vte);
  (* Overstrike (nroff bold "C\bX"): the redraw lands on the same cell. *)
  feed vte "X";
  equal ~msg:"overstrike hits same cell" char 'X' (get_char_at grid 0 2)

let line_feed_preserves_column () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "abc\ndef";
  equal ~msg:"first line" string "abc" (get_line (Vte.grid vte) 0);
  equal ~msg:"LF moved straight down" string "   def"
    (get_line (Vte.grid vte) 1);
  equal ~msg:"cursor position" (pair int int) (1, 6) (Vte.cursor_pos vte)

let line_feed_scroll_region () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  feed vte "A\r\nB\r\nC\r\nD\r\nE";
  (* Region rows 2..3 (1-based). *)
  feed vte "\x1b[2;3r";
  (* Below the region (classic status-line setup): LF must not scroll. *)
  feed vte "\x1b[5;1H\n";
  equal ~msg:"region untouched below" string "B" (get_line (Vte.grid vte) 1);
  equal ~msg:"cursor stays at screen bottom" (pair int int) (4, 0)
    (Vte.cursor_pos vte);
  (* At the region bottom: LF scrolls the region only. *)
  feed vte "\x1b[3;1H\n";
  equal ~msg:"region scrolled" string "C" (get_line (Vte.grid vte) 1);
  equal ~msg:"region bottom blank" string "" (get_line (Vte.grid vte) 2);
  equal ~msg:"row above region untouched" string "A" (get_line (Vte.grid vte) 0);
  equal ~msg:"row below region untouched" string "D" (get_line (Vte.grid vte) 3)

let delete_characters_wide () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  feed vte "中A";
  feed vte "\r\x1b[P";
  let grid = Vte.grid vte in
  equal ~msg:"half left behind blanked" string " A" (get_line grid 0);
  is_false ~msg:"no orphan continuation" (Grid.is_continuation grid 0)

let insert_characters_wide_boundary () =
  let vte = Vte.create ~rows:1 ~cols:4 () in
  feed vte "AB中";
  feed vte "\r\x1b[@";
  let grid = Vte.grid vte in
  equal ~msg:"shift respects graphemes" string " AB" (get_line grid 0);
  let last_idx = Grid.width grid - 1 in
  is_false ~msg:"last cell not continuation"
    (Grid.is_continuation grid last_idx)

let edit_on_wide_right_half () =
  (* With the cursor on the right half of a wide character, DCH and ICH act
     from that cell and blank the half left behind. *)
  let screen s =
    let vte = Vte.create ~rows:1 ~cols:6 () in
    feed vte ("a\xe4\xb8\xadb\x1b[1;3H" ^ s);
    Vte.to_string vte
  in
  equal ~msg:"DCH" string "a b   " (screen "\x1b[P");
  equal ~msg:"ICH" string "a   b " (screen "\x1b[@")

let dch_ending_on_wide_left_half () =
  (* DCH deletes the cells asked for and blanks the half left behind. *)
  let vte = Vte.create ~rows:1 ~cols:8 () in
  feed vte "a中b\r\x1b[2P";
  equal ~msg:"two cells" string " b      " (Vte.to_string vte)

let shift_wide_over_wide () =
  (* A wide character moved onto the halves of another keeps both halves. *)
  let continuation s =
    let vte = Vte.create ~rows:1 ~cols:6 () in
    feed vte ("中你\r" ^ s);
    Grid.is_continuation (Vte.grid vte) 3
  in
  is_true ~msg:"ICH" (continuation "\x1b[2@");
  is_true ~msg:"insert mode" (continuation "\x1b[4h中")

(* DECAWM off: printing at the right margin overwrites the last column and
   the cursor stays pinned; the run must not be dropped. Found by the split
   invariance property (seed 496960259): a dropped base made a later chunk's
   combining mark attach to the wrong cell. *)
let margin_overwrite_with_wrap_off () =
  let vte = Vte.create ~rows:1 ~cols:3 () in
  feed vte "\x1b[?7labcdef";
  equal ~msg:"last column holds the run's final char" string "abf"
    (get_line (Vte.grid vte) 0);
  let whole = Vte.create ~rows:1 ~cols:1 () in
  feed whole "\r\n \x1b[?7le\xCC\x81     ";
  let split = Vte.create ~rows:1 ~cols:1 () in
  feed split "\r\n \x1b[?7le";
  feed split "\xCC\x81     ";
  equal ~msg:"whole and split feeds converge" string (Vte.to_string whole)
    (Vte.to_string split);
  equal ~msg:"final overwrite is a space" string " " (Vte.to_string whole)

(* After a write to the last column the cursor waits to wrap. Cursor motion,
   LF and erases act from the last column and cancel the wrap, as in xterm;
   explicit motion never reaches the pending-wrap position. *)
let pending_wrap_cleared_by_controls () =
  let after_last_column s =
    let vte = Vte.create ~rows:3 ~cols:5 () in
    feed vte ("abcde" ^ s);
    vte
  in
  let vte = after_last_column "\x1b[DX" in
  equal ~msg:"CUB from the last column" string "abcXe"
    (get_line (Vte.grid vte) 0);
  let vte = after_last_column "\bX" in
  equal ~msg:"BS from the last column" string "abcXe"
    (get_line (Vte.grid vte) 0);
  let vte = after_last_column "\nX" in
  equal ~msg:"LF keeps the last column" string "    X"
    (get_line (Vte.grid vte) 1);
  equal ~msg:"LF does not wrap" string "" (get_line (Vte.grid vte) 2);
  let vte = after_last_column "\x1b[AX" in
  equal ~msg:"CUU at the top overwrites the last column" string "abcdX"
    (get_line (Vte.grid vte) 0);
  let vte = after_last_column "\x1b[K" in
  equal ~msg:"EL erases the last column" string "abcd"
    (get_line (Vte.grid vte) 0);
  let vte = Vte.create ~rows:3 ~cols:5 () in
  feed vte "\x1b[1;9HX";
  equal ~msg:"CUP clamps to the last column" string "    X"
    (get_line (Vte.grid vte) 0);
  feed vte "\r\x1b[9GY";
  equal ~msg:"CHA clamps to the last column" string "    Y"
    (get_line (Vte.grid vte) 0);
  let vte = Vte.create ~rows:3 ~cols:5 () in
  feed vte "ab\x1b[9CX";
  equal ~msg:"CUF stops at the last column" string "ab  X"
    (get_line (Vte.grid vte) 0);
  equal ~msg:"CUF does not wrap" string "" (get_line (Vte.grid vte) 1)

let wrap_below_scroll_region () =
  let vte = Vte.create ~rows:5 ~cols:3 () in
  feed vte "A\r\nB\r\nC\r\nD\r\nE";
  (* Region rows 2..3 (1-based); write below it at the screen bottom. *)
  feed vte "\x1b[2;3r";
  feed vte "\x1b[5;1Habcd";
  (* Auto-wrap at the screen bottom outside the region must neither scroll
     the region nor yank the cursor up into it: the cursor wraps in place. *)
  equal ~msg:"region row untouched" string "B" (get_line (Vte.grid vte) 1);
  equal ~msg:"region row untouched 2" string "C" (get_line (Vte.grid vte) 2);
  equal ~msg:"bottom row wrapped in place" string "dbc"
    (get_line (Vte.grid vte) 4);
  equal ~msg:"cursor on bottom row" (pair int int) (4, 1) (Vte.cursor_pos vte)

(** {1 SGR (Styling)} *)

let sgr_bold () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[1mBold\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"bold attribute set" (Ansi.Attr.mem Ansi.Attr.Bold attrs)

let sgr_italic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[3mItalic\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"italic attribute set" (Ansi.Attr.mem Ansi.Attr.Italic attrs)

let sgr_underline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[4mUnderline\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"underline attribute set"
    (Ansi.Attr.mem Ansi.Attr.Underline attrs)

let sgr_foreground_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[31mRed\x1b[0m";
  let grid = Vte.grid vte in
  let r, _, _ = Grid.get_fg grid 0 |> Ansi.Color.to_rgb in
  (* Standard ANSI red fallback = (128, 0, 0) *)
  equal ~msg:"red foreground" int 128 r

let sgr_background_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[42mGreen BG\x1b[0m";
  let grid = Vte.grid vte in
  let _, g, _ = Grid.get_bg grid 0 |> Ansi.Color.to_rgb in
  (* Standard ANSI green fallback = (0, 128, 0) *)
  equal ~msg:"green background" int 128 g

let erase_keeps_background_index () =
  (* Erased cells and the lines a scroll brings in take the current
     background, palette index included (bce). *)
  let bg s row =
    let vte = Vte.create ~rows:2 ~cols:3 () in
    feed vte s;
    match Ansi.Color.intent (Grid.get_bg (Vte.grid vte) (row * 3)) with
    | Indexed i -> Some i
    | Default | Rgb -> None
  in
  equal ~msg:"ED" (option int) (Some 1) (bg "\x1b[41m\x1b[2J" 0);
  equal ~msg:"EL" (option int) (Some 1) (bg "\x1b[41m\x1b[K" 0);
  equal ~msg:"LF scroll" (option int) (Some 1) (bg "\x1b[41m\n\n" 1);
  equal ~msg:"RI scroll" (option int) (Some 1) (bg "\x1b[41m\x1bM" 0)

let erase_cutting_wide_takes_background () =
  (* An erasure from the right half of a wide character blanks its left half
     in the erasure's colour, as the cells it erases. *)
  let bg s =
    let vte = Vte.create ~rows:1 ~cols:4 () in
    feed vte ("\xe4\xb8\xad\x1b[1;2H\x1b[41m" ^ s);
    match Ansi.Color.intent (Grid.get_bg (Vte.grid vte) 0) with
    | Indexed i -> Some i
    | Default | Rgb -> None
  in
  equal ~msg:"EL" (option int) (Some 1) (bg "\x1b[K");
  equal ~msg:"ECH" (option int) (Some 1) (bg "\x1b[X")

let terminal_defaults_replace_cells () =
  (* Cells keep the default colours as such: erasing and printing replace
     the cells instead of blending over them. *)
  let vte () = Vte.create ~rows:1 ~cols:8 () in
  let screen s =
    let vte = vte () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"ECH" string "  cde   " (screen "abcde\r\x1b[2X");
  equal ~msg:"EL" string "        " (screen "abcde\r\x1b[K");
  equal ~msg:"space" string " bc     " (screen "abc\r ");
  let vte = vte () in
  feed vte "\x1b[41ma\x1b[49m\rb";
  is_true ~msg:"default background replaces red"
    (Ansi.Color.intent (Grid.get_bg (Vte.grid vte) 0) = Default)

let sgr_24_ends_double_underline () =
  let vte = Vte.create ~rows:1 ~cols:4 () in
  feed vte "\x1b[21;24ma";
  equal ~msg:"SGR 24" int 0 (Grid.get_attrs (Vte.grid vte) 0)

let sgr_reset () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[1;31mBold Red\x1b[0mNormal";
  let grid = Vte.grid vte in
  let normal_attrs = Grid.get_attrs grid 8 |> Ansi.Attr.unpack in
  is_true ~msg:"attributes reset" (Ansi.Attr.is_empty normal_attrs)

(** {1 Cursor Visibility} *)

let cursor_hide_show () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_true ~msg:"initially visible" (Vte.modes vte).cursor_visible;
  feed vte "\x1b[?25l";
  is_false ~msg:"hidden after DECTCEM hide" (Vte.modes vte).cursor_visible;
  feed vte "\x1b[?25h";
  is_true ~msg:"shown after DECTCEM show" (Vte.modes vte).cursor_visible

(* The cursor is shown and hidden only by the program (DECTCEM); a change
   marks the cursor changed, and setting the same visibility does not. *)
let set_cursor_visible_programmatic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  feed vte "\x1b[?25h";
  is_false ~msg:"showing a shown cursor changes nothing"
    (Vte.is_cursor_dirty vte);
  feed vte "\x1b[?25l";
  is_false ~msg:"hidden" (Vte.modes vte).cursor_visible;
  is_true ~msg:"hiding marks the cursor" (Vte.is_cursor_dirty vte)

(** {1 Cursor Save/Restore} *)

let cursor_save_restore () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  feed vte "\x1b[5;10HHello\x1b7";
  let pos1 = Vte.cursor_pos vte in
  feed vte "\x1b[1;1HWorld\x1b8";
  let pos2 = Vte.cursor_pos vte in
  equal ~msg:"cursor restored" (pair int int) pos1 pos2

let cursor_save_restore_style () =
  (* DECRC restores the SGR state saved by DECSC, not just the cursor. *)
  let vte = Vte.create ~rows:3 ~cols:20 () in
  feed vte "\x1b[31m\x1b7\x1b[0m\x1b8X";
  let r, _, _ = Grid.get_fg (Vte.grid vte) 0 |> Ansi.Color.to_rgb in
  equal ~msg:"DECRC restores saved SGR" int 128 r

let saved_cursor_per_screen () =
  let run s =
    let vte = Vte.create ~rows:3 ~cols:5 () in
    feed vte s;
    vte
  in
  is_false ~msg:"DECRC keeps the cursor hidden"
    (Vte.modes (run "\x1b7\x1b[?25l\x1b8")).cursor_visible;
  is_false ~msg:"leaving 1049 keeps the cursor hidden"
    (Vte.modes (run "\x1b[?1049h\x1b[?25l\x1b[?1049l")).cursor_visible;
  let vte = run "\x1b[2;3H\x1b7\x1b[?47h\x1b[3;5H\x1b7\x1b[?47l\x1b8X" in
  equal ~msg:"each screen keeps its saved cursor" string "     \n  X  \n     "
    (Vte.to_string vte);
  let vte = run "\x1b[31mab\x1b8X" in
  equal ~msg:"DECRC with nothing saved goes home" string "Xb   \n     \n     "
    (Vte.to_string vte);
  is_true ~msg:"and resets SGR"
    (Ansi.Color.equal (Grid.get_fg (Vte.grid vte) 0) Ansi.Color.default_fg)

let alternate_screen_restores_style () =
  (* Leaving the alternate screen (DECRST 1049) restores the SGR state saved
     on entry, so a full-screen app can't leak its last style. *)
  let vte = Vte.create ~rows:3 ~cols:20 () in
  feed vte "\x1b[31m\x1b[?1049h\x1b[0m\x1b[?1049lX";
  is_false ~msg:"back on primary screen" (Vte.modes vte).alternate_screen;
  let r, _, _ = Grid.get_fg (Vte.grid vte) 0 |> Ansi.Color.to_rgb in
  equal ~msg:"1049 exit restores saved SGR" int 128 r

let alternate_screen_keeps_cursor () =
  (* Switching screens leaves the cursor where it is; only 1049 restores the
     cursor saved on entry. *)
  let screen s =
    let vte = Vte.create ~rows:1 ~cols:8 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"47" string "abc   X " (screen "abc\x1b[?47hALT\x1b[?47lX");
  equal ~msg:"1047" string "abc   X " (screen "abc\x1b[?1047hALT\x1b[?1047lX");
  equal ~msg:"1049 entry" string "   X    " (screen "abc\x1b[?1049hX");
  equal ~msg:"1049 exit" string "abcX    "
    (screen "abc\x1b[?1049hALT\x1b[?1049lX")

(** {1 Scrolling} *)

let scroll_up_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  feed vte "\x1b[2S";
  equal ~msg:"line moved up" string "L3" (get_line (Vte.grid vte) 0);
  equal ~msg:"bottom cleared" string "" (get_line (Vte.grid vte) 3)

let scroll_down_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "L1\r\nL2\r\nL3";
  feed vte "\x1b[T";
  equal ~msg:"top cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"line moved down" string "L1" (get_line (Vte.grid vte) 1)

let csi_scroll_sequences () =
  let vte = Vte.create ~scrollback:100 ~rows:5 ~cols:20 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  feed vte "\x1b[2S";
  equal ~msg:"CSI S moves rows up" string "L3" (get_line (Vte.grid vte) 0);
  equal ~msg:"CSI S records native scrollback" int 2 (Vte.scrollback_size vte);
  feed vte "\x1b[1T";
  equal ~msg:"CSI T inserts a blank top row" string ""
    (get_line (Vte.grid vte) 0);
  equal ~msg:"CSI T moves content down" string "L3" (get_line (Vte.grid vte) 1)

let scroll_on_bottom_line () =
  let vte = Vte.create ~rows:3 ~cols:10 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4";
  equal ~msg:"first line scrolled off" string "L2" (get_line (Vte.grid vte) 0);
  equal ~msg:"new line at bottom" string "L4" (get_line (Vte.grid vte) 2)

let alignment_and_column_edits () =
  let screen s =
    let vte = Vte.create ~rows:2 ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"DECALN" string "EEEEEE\nEEEEEE" (screen "ab\x1b#8");
  equal ~msg:"SL" string "bcdef \nhijkl " (screen "abcdef\r\nghijkl\x1b[1 @");
  equal ~msg:"SR" string " abcde\n ghijk" (screen "abcdef\r\nghijkl\x1b[1 A");
  equal ~msg:"DECIC" string "ab cde\ngh ijk"
    (screen "abcdef\r\nghijkl\x1b[1;3H\x1b[1'}");
  equal ~msg:"DECDC" string "abdef \nghjkl "
    (screen "abcdef\r\nghijkl\x1b[1;3H\x1b[1'~");
  let vte = Vte.create ~rows:3 ~cols:6 () in
  feed vte "abcdef\r\nghijkl\r\nmnopqr\x1b[1;2r\x1b[3;1H\x1b[1 @";
  equal ~msg:"SL from outside the region" string "abcdef\nghijkl\nmnopqr"
    (Vte.to_string vte)

let reverse_index_moves_cursor_up () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  feed vte "\x1b[3;4H\x1bM";
  equal ~msg:"RI moves cursor up" (pair int int) (1, 3) (Vte.cursor_pos vte)

let reverse_index_scrolls_region_down () =
  let vte = Vte.create ~rows:4 ~cols:10 () in
  feed vte "\x1b[1;1HA\x1b[2;1HB\x1b[3;1HC\x1b[4;1HD";
  feed vte "\x1b[2;4r\x1b[2;1H\x1bM";
  equal ~msg:"line above region unchanged" string "A"
    (get_line (Vte.grid vte) 0);
  equal ~msg:"top margin cleared" string "" (get_line (Vte.grid vte) 1);
  equal ~msg:"region scrolled down" string "B" (get_line (Vte.grid vte) 2);
  equal ~msg:"region bottom receives previous row" string "C"
    (get_line (Vte.grid vte) 3);
  equal ~msg:"cursor remains at top margin" (pair int int) (1, 0)
    (Vte.cursor_pos vte)

let index_and_next_line () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  feed vte "ab\x1bDcd";
  equal ~msg:"IND moves down a row" string "ab   \n  cd \n     "
    (Vte.to_string vte);
  let vte = Vte.create ~rows:3 ~cols:5 () in
  feed vte "ab\x1bEcd";
  equal ~msg:"NEL moves to the start of the next row" string
    "ab   \ncd   \n     " (Vte.to_string vte);
  let vte = Vte.create ~rows:2 ~cols:5 () in
  feed vte "a\r\nb\x1bDc\x1bEd";
  equal ~msg:"IND and NEL scroll at the bottom margin" string " c   \nd    "
    (Vte.to_string vte)

(** {1 Scrollback} *)

let scrollback_accumulates () =
  let vte = Vte.create ~scrollback:100 ~rows:3 ~cols:10 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  equal ~msg:"scrollback has lines" int 2 (Vte.scrollback_size vte);
  let lines = scrollback_lines vte in
  equal ~msg:"scrollback list length" int 2 (List.length lines)

let one_row_scroll () =
  (* On a one-row screen, a scroll clears the row it moves out. *)
  let screen s =
    let vte = Vte.create ~rows:1 ~cols:3 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"LF" string "d  " (screen "abc\r\nd");
  equal ~msg:"SU" string "   " (screen "abc\x1b[S");
  equal ~msg:"RI" string "   " (screen "abc\x1bM")

let erase_scrollback_sequence () =
  let vte = Vte.create ~scrollback:100 ~rows:3 ~cols:10 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  satisfies ~claim:"fixture has scrollback (> 0)" int
    (fun n -> n > 0)
    (Vte.scrollback_size vte);
  let visible = get_line (Vte.grid vte) 0 in
  feed vte "\x1b[3J";
  equal ~msg:"ED 3 clears native scrollback" int 0 (Vte.scrollback_size vte);
  equal ~msg:"ED 3 preserves the visible screen" string visible
    (get_line (Vte.grid vte) 0)

let scrollback_ring_buffer () =
  let vte = Vte.create ~scrollback:2 ~rows:2 ~cols:5 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  equal ~msg:"scrollback at capacity" int 2 (Vte.scrollback_size vte);
  let lines = scrollback_lines vte in
  equal ~msg:"oldest in ring" string "L2" (List.nth lines 0);
  equal ~msg:"newest in ring" string "L3" (List.nth lines 1)

let blit_view_order () =
  let vte = Vte.create ~scrollback:100 ~rows:3 ~cols:10 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  equal ~msg:"two lines in scrollback" int 2 (Vte.scrollback_size vte);
  let dst = Grid.create ~width:10 ~height:3 () in
  Vte.blit_view vte ~offset:2 dst;
  equal ~msg:"oldest visible line on top" string "L1" (get_line dst 0);
  equal ~msg:"newer line below" string "L2" (get_line dst 1);
  equal ~msg:"screen top adjacent to history" string "L3" (get_line dst 2);
  Vte.blit_view vte ~offset:1 dst;
  equal ~msg:"offset 1 shows newest history line" string "L2" (get_line dst 0);
  equal ~msg:"offset 1 screen row 0" string "L3" (get_line dst 1);
  equal ~msg:"offset 1 screen row 1" string "L4" (get_line dst 2)

let blit_view_wrapped_ring () =
  let vte = Vte.create ~scrollback:2 ~rows:2 ~cols:5 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4\r\nL5";
  (* Ring holds L2 (oldest) and L3 (newest); L1 was evicted. *)
  let dst = Grid.create ~width:5 ~height:3 () in
  Vte.blit_view vte ~offset:2 dst;
  equal ~msg:"wrapped oldest on top" string "L2" (get_line dst 0);
  equal ~msg:"wrapped newest below" string "L3" (get_line dst 1);
  equal ~msg:"wrapped screen top" string "L4" (get_line dst 2)

(* An offset past the scrollback shows its oldest line on top, a negative
   one the screen, and the alternate screen has no scrollback to show. *)
let blit_view_clamps () =
  let vte = Vte.create ~scrollback:100 ~rows:2 ~cols:5 () in
  feed vte "L1\r\nL2\r\nL3";
  let dst = Grid.create ~width:5 ~height:2 () in
  let view () = [ get_line dst 0; get_line dst 1 ] in
  Vte.blit_view vte ~offset:10 dst;
  equal ~msg:"past the scrollback" (list string) [ "L1"; "L2" ] (view ());
  Vte.blit_view vte ~offset:(-3) dst;
  equal ~msg:"negative" (list string) [ "L2"; "L3" ] (view ());
  feed vte "\027[?1049h\027[HA";
  Vte.blit_view vte ~offset:1 dst;
  equal ~msg:"alternate screen" (list string) [ "A"; "" ] (view ())

(* Rows run from the screen up into the scrollback: each has its text, its
   wrap mark and its cells, the same before and after it scrolls off. Other
   rows are blank. *)
let rows_and_scrollback () =
  let vte = Vte.create ~scrollback:4 ~rows:2 ~cols:4 () in
  feed vte
    "\x1b[31;1ma\x1b]8;;http://l.test\x07\xe4\xb8\xad\x1b]8;;\x07\x1b[mxyzw ";
  let row = Grid.create ~width:6 ~height:1 () in
  let styles g = List.init 4 (Grid.get_style g) in
  let screen_styles = styles (Vte.grid vte) in
  Vte.blit_row vte 0 row ~y:0;
  equal ~msg:"a screen row's cells" (list string)
    [ "a"; "\xe4\xb8\xad"; ""; "x" ]
    (List.init 4 (Grid.get_text row));
  equal ~msg:"screen texts" (list string)
    [ "a\xe4\xb8\xadx"; "yzw" ]
    [ Vte.text vte 0; Vte.text vte 1 ];
  is_true ~msg:"screen row 0 wrapped" (Vte.wrapped vte 0);
  feed vte "\r\n\r\n";
  equal ~msg:"two lines scrolled off" int 2 (Vte.scrollback_size vte);
  equal ~msg:"scrollback texts" (list string)
    [ "a\xe4\xb8\xadx"; "yzw" ]
    [ Vte.text vte (-2); Vte.text vte (-1) ];
  is_true ~msg:"row -2 keeps its wrap mark" (Vte.wrapped vte (-2));
  is_false ~msg:"row -1 has none" (Vte.wrapped vte (-1));
  Vte.blit_row vte (-2) row ~y:0;
  equal ~msg:"a scrollback row's cells" (list string)
    [ "a"; "\xe4\xb8\xad"; ""; "x"; " "; " " ]
    (List.init 6 (Grid.get_text row));
  equal ~msg:"its widths" (list int) [ 1; 2; 0; 1 ]
    (List.init 4 (Grid.cell_width row));
  is_true ~msg:"its rendition"
    (List.for_all2 Ansi.Style.equal screen_styles (styles row));
  equal ~msg:"its link" string "http://l.test" (Grid.get_link row 1);
  is_true ~msg:"blanks past it, on the default background"
    (Ansi.Color.equal Ansi.Color.default_bg (Grid.get_bg row 5));
  equal ~msg:"rows outside are blank" (list string) [ ""; "" ]
    [ Vte.text vte (-3); Vte.text vte 2 ];
  Vte.blit_row vte (-3) row ~y:0;
  equal ~msg:"blit of a row outside" string "" (Vte.text vte (-3));
  is_true ~msg:"blank cells" (Grid.get_text row 0 = " ");
  let narrow = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text narrow ~x:0 ~y:0 ~text:"ZZ";
  Vte.blit_row vte (-2) narrow ~y:0;
  equal ~msg:"a wide grapheme past a narrow grid is a blank" (list string)
    [ "a"; " " ]
    (List.init 2 (Grid.get_text narrow));
  raises ~msg:"y outside dst"
    (Invalid_argument "Vte.blit_row: y = 1, not a row of a grid 1 high")
    (fun () -> Vte.blit_row vte 0 row ~y:1);
  feed vte "\x1b[?1049h";
  equal ~msg:"no scrollback rows on the alternate screen" string ""
    (Vte.text vte (-1))

let scrollback_disabled () =
  let vte = Vte.create ~scrollback:0 ~rows:3 ~cols:10 () in
  feed vte "L1\r\nL2\r\nL3\r\nL4";
  equal ~msg:"no scrollback" int 0 (Vte.scrollback_size vte);
  equal ~msg:"empty scrollback" (list string) [] (scrollback_lines vte)

(** {1 Terminal Modes} *)

let auto_wrap_mode () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  is_true ~msg:"auto wrap on by default" (Vte.modes vte).auto_wrap;
  feed vte "HelloWorld";
  equal ~msg:"wrapped to next line" string "World" (get_line (Vte.grid vte) 1)

let insert_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"insert mode off by default" (Vte.modes vte).insert;
  feed vte "\x1b[4h";
  is_true ~msg:"insert mode enabled" (Vte.modes vte).insert;
  feed vte "\x1b[4l";
  is_false ~msg:"insert mode disabled" (Vte.modes vte).insert

let insert_mode_wrap_targets_new_row () =
  (* After an insert-mode wrap, clusters must land on the row the cursor
     wrapped to, not back on the previous row at column 0. *)
  let vte = Vte.create ~rows:2 ~cols:3 () in
  feed vte "\x1b[4h";
  feed vte "abcde";
  equal ~msg:"first row keeps pre-wrap text" string "abc\nde "
    (Vte.to_string vte);
  equal ~msg:"cursor on wrapped row" (pair int int) (1, 2) (Vte.cursor_pos vte)

let insert_mode_clips_wide_grapheme_at_margin () =
  (* Shrunk from a grid-invariants property counterexample: inserting into a
     line whose wide grapheme would be pushed across the right margin copied
     only its start cell, leaving a wide start with no continuation in the
     last column. The clipped grapheme must be blanked instead. *)
  let vte = Vte.create ~rows:1 ~cols:3 () in
  feed vte "a\xE4\xBD\xA0";
  feed vte "\x1b[1;2H\x1b[4h";
  feed vte "x";
  let grid = Vte.grid vte in
  satisfies ~claim:"no wide start in the last column (cell width <= 1)" int
    (fun w -> w <= 1)
    (Grid.cell_width grid 2);
  is_false ~msg:"no orphan continuation" (Grid.is_continuation grid 2);
  equal ~msg:"clipped wide grapheme is blanked" string "ax " (Vte.to_string vte)

let cursor_key_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"cursor key mode off by default" (Vte.modes vte).cursor_keys;
  feed vte "\x1b[?1h";
  is_true ~msg:"cursor key mode enabled" (Vte.modes vte).cursor_keys;
  feed vte "\x1b[?1l";
  is_false ~msg:"cursor key mode disabled" (Vte.modes vte).cursor_keys

let combined_mode_sequence () =
  (* ECMA-48 allows several modes in one CSI; none may be dropped. *)
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[?1049;2004h";
  is_true ~msg:"alternate screen enabled" (Vte.modes vte).alternate_screen;
  is_true ~msg:"bracketed paste enabled" (Vte.modes vte).bracketed_paste;
  feed vte "\x1b[?1049;2004l";
  is_false ~msg:"alternate screen disabled" (Vte.modes vte).alternate_screen;
  is_false ~msg:"bracketed paste disabled" (Vte.modes vte).bracketed_paste

let bracketed_paste_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"bracketed paste off by default" (Vte.modes vte).bracketed_paste;
  feed vte "\x1b[?2004h";
  is_true ~msg:"bracketed paste enabled" (Vte.modes vte).bracketed_paste;
  feed vte "\x1b[?2004l";
  is_false ~msg:"bracketed paste disabled" (Vte.modes vte).bracketed_paste

(** {1 OSC Sequences} *)

let osc_set_title () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  equal ~msg:"title empty initially" string "" (Vte.title vte);
  feed vte "\x1b]0;My Title\x07";
  equal ~msg:"title set via OSC 0" string "My Title" (Vte.title vte);
  feed vte "\x1b]2;New Title\x07";
  equal ~msg:"title set via OSC 2" string "New Title" (Vte.title vte)

(* [feed] is the replies, in order; [[]] when nothing is owed. The identity
   is a constant: a VT100 with Advanced Video. *)
let query_replies () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  let replies =
    Testable.make ~equal:( = )
      ~pp:
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           Matrix_input.Reply.pp)
  in
  equal ~msg:"typed replies" replies
    [
      Primary_attributes [ 1; 2 ];
      Secondary_attributes [ 0; 0; 0 ];
      Operating_status `Ok;
      Cursor { row = 0; col = 0; page = None };
      Cursor { row = 0; col = 0; page = Some 1 };
      Mode { dec = true; mode = 7; setting = Set };
      Color { target = `Background; rgb = (0, 0, 0); terminator = `Bel };
    ]
    (Vte.feed_string vte
       "\x1b[c\x1b[>c\x1b[5n\x1b[6n\x1b[?6n\x1b[?7$p\x1b]11;?\x07");
  equal ~msg:"nothing owed" string "" (answers vte "ab\r\n");
  equal ~msg:"DA1, DA2 and DSR 5" string "\x1b[?1;2c\x1b[>0;0;0c\x1b[0n"
    (answers vte "\x1b[c\x1b[>c\x1b[5n");
  equal ~msg:"CPR and DECXCPR" string "\x1b[2;1R\x1b[?2;1;1R"
    (answers vte "\x1b[6n\x1b[?6n");
  equal ~msg:"CPR with the Last Column Flag" string "\x1b[2;5R"
    (answers vte "abcde\x1b[6n");
  equal ~msg:"DA with parameter 0" string "\x1b[?1;2c\x1b[>0;0;0c"
    (answers vte "\x1b[0c\x1b[>0c");
  equal ~msg:"DECID, an obsolete form of DA" string "\x1b[?1;2c"
    (answers vte "\x1bZ");
  equal ~msg:"a query split across feeds is answered by the last" string
    "\x1b[0n"
    (feed vte "\x1b[5";
     answers vte "n")

(* The identity is the terminal's, but the replies are the caller's to relay:
   a recorder that passes for xterm.js answers DA2 as xterm.js 5 does, and
   the program reads that answer. *)
let rewritten_reply_reaches_the_program () =
  let module I = Matrix_input in
  let vte = Vte.create ~rows:3 ~cols:5 () in
  let as_xterm_js : I.Reply.t -> I.Reply.t = function
    | Secondary_attributes _ -> Secondary_attributes [ 0; 276; 0 ]
    | r -> r
  in
  let bytes =
    I.Reply.encode (List.map as_xterm_js (Vte.feed_string vte "\x1b[>c\x1b[c"))
  in
  equal ~msg:"the bytes relayed" string "\x1b[>0;276;0c\x1b[?1;2c" bytes;
  let program = I.Decoder.create () in
  let read = ref [] in
  I.Decoder.feed program (Bytes.of_string bytes) ~off:0
    ~len:(String.length bytes) (fun e ->
      read := Format.asprintf "%a" I.Decoder.pp_event e :: !read);
  equal ~msg:"what the program reads" (list string)
    [ "Reply Secondary_attributes 0;276;0"; "Reply Primary_attributes 1;2" ]
    (List.rev !read)

(* LNM makes LF, VT and FF return to the first column, as a tty's ONLCR
   makes LF; it leaves IND alone, RIS resets it and DECSTR keeps it. *)
let line_feed_new_line_mode () =
  let screen s =
    let vte = Vte.create ~rows:4 ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"LF keeps the column" string "ab    \n  cd  \n      \n      "
    (screen "ab\ncd");
  equal ~msg:"LF, VT and FF in LNM" string "ab    \ncd    \nef    \ngh    "
    (screen "\x1b[20hab\ncd\x0bef\x0cgh");
  equal ~msg:"IND in LNM keeps the column" string
    "ab    \n  cd  \n      \n      "
    (screen "\x1b[20hab\x1bDcd");
  equal ~msg:"RM 20" string "ab    \n  cd  \n      \n      "
    (screen "\x1b[20h\x1b[20lab\ncd");
  equal ~msg:"RIS resets LNM" string "ab    \n  cd  \n      \n      "
    (screen "\x1b[20h\x1bcab\ncd");
  equal ~msg:"DECSTR keeps LNM" string "ab    \ncd    \n      \n      "
    (screen "\x1b[20h\x1b[!pab\ncd");
  let vte = Vte.create ~rows:2 ~cols:4 () in
  is_false ~msg:"off at power-up" (Vte.modes vte).new_line;
  equal ~msg:"DECRQM 20" string "\x1b[20;2$y\x1b[20;1$y"
    (answers vte "\x1b[20$p\x1b[20h\x1b[20$p");
  is_true ~msg:"in the modes" (Vte.modes vte).new_line

(* DECSCUSR sets the cursor's shape and blinking; RIS restores the blinking
   block and DECSTR keeps the style. *)
let cursor_style () =
  let style s =
    let vte = Vte.create ~rows:2 ~cols:4 () in
    feed vte s;
    Vte.cursor_style vte
  in
  let shape =
    Testable.make ~equal:( = ) ~pp:(fun ppf (s, b) ->
        Format.fprintf ppf "(%s, %b)"
          (match s with
          | `Block -> "Block"
          | `Underline -> "Underline"
          | `Line -> "Line")
          b)
  in
  equal ~msg:"power-up" shape (`Block, true) (style "");
  List.iter
    (fun (ps, expected) ->
      equal ~msg:("DECSCUSR " ^ ps) shape expected
        (style ("\x1b[6 q\x1b[" ^ ps ^ " q")))
    [
      ("", (`Block, true));
      ("0", (`Block, true));
      ("1", (`Block, true));
      ("2", (`Block, false));
      ("3", (`Underline, true));
      ("4", (`Underline, false));
      ("5", (`Line, true));
      ("7", (`Block, true));
      ("8", (`Line, false));
    ];
  equal ~msg:"RIS" shape (`Block, true) (style "\x1b[4 q\x1bc");
  equal ~msg:"DECSTR keeps it" shape (`Underline, false)
    (style "\x1b[4 q\x1b[!p");
  equal ~msg:"both screens share it" shape (`Underline, true)
    (style "\x1b[3 q\x1b[?1049h\x1b[?1049l");
  equal ~msg:"DECSC does not save it" shape (`Line, true)
    (style "\x1b[3 q\x1b7\x1b[5 q\x1b8");
  let vte = Vte.create ~rows:2 ~cols:4 () in
  Vte.clear_dirty vte;
  feed vte "\x1b[5 q";
  is_true ~msg:"a change marks the cursor dirty" (Vte.is_cursor_dirty vte);
  Vte.clear_dirty vte;
  feed vte "\x1b[5 q";
  is_false ~msg:"no change, nothing dirty" (Vte.is_dirty vte)

(* The dynamic colors are walked from the OSC's number, so an empty spec
   moves on to the next color, and OSC 13, which is not one of them, asks
   nothing. *)
let dynamic_color_walk () =
  let vte = Vte.create ~rows:1 ~cols:4 () in
  let replies =
    Testable.make ~equal:( = )
      ~pp:
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           Matrix_input.Reply.pp)
  in
  let cursor = (Vte.palette vte).cursor in
  equal ~msg:"OSC 11 ; ; ? reports the cursor's color" replies
    [ Color { target = `Cursor; rgb = cursor; terminator = `St } ]
    (Vte.feed_string vte "\x1b]11;;?\x1b\\");
  equal ~msg:"OSC 13 ; ? gets no answer" replies []
    (Vte.feed_string vte "\x1b]13;?\x1b\\")

(* DECRQM reports a mode as set (1), reset (2) or not recognized (0); OSC 10,
   11 and 4 with [?] report a colour with the query's terminator. *)
let mode_and_colour_queries () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  equal ~msg:"DEC modes" string
    "\x1b[?2004;2$y\x1b[?7;1$y\x1b[?1049;2$y\x1b[?1016;0$y"
    (answers vte "\x1b[?2004$p\x1b[?7$p\x1b[?1049$p\x1b[?1016$p");
  equal ~msg:"DEC modes after setting them" string
    "\x1b[?2004;1$y\x1b[?7;2$y\x1b[?1049;1$y\x1b[?47;1$y"
    (answers vte
       "\x1b[?2004h\x1b[?7l\x1b[?1049h\x1b[?2004$p\x1b[?7$p\x1b[?1049$p\x1b[?47$p");
  equal ~msg:"ANSI modes" string "\x1b[4;2$y\x1b[4;1$y\x1b[2;0$y"
    (answers vte "\x1b[4$p\x1b[4h\x1b[4$p\x1b[2$p");
  equal ~msg:"dynamic colours, BEL and ST" string
    "\x1b]10;rgb:c0c0/c0c0/c0c0\x07\x1b]11;rgb:0000/0000/0000\x1b\\"
    (answers vte "\x1b]10;?\x07\x1b]11;?\x1b\\");
  equal ~msg:"successive dynamic colours" string
    "\x1b]10;rgb:c0c0/c0c0/c0c0\x07\x1b]11;rgb:0000/0000/0000\x07\x1b]12;rgb:c0c0/c0c0/c0c0\x07"
    (answers vte "\x1b]10;?;?;?;?\x07");
  equal ~msg:"palette colours" string
    "\x1b]4;1;rgb:8080/0000/0000\x07\x1b]4;196;rgb:ffff/0000/0000\x07"
    (answers vte "\x1b]4;1;?;2;red;196;?;256;?;+1;?\x07");
  equal ~msg:"no answer: a set, a query cut short" string ""
    (answers vte "\x1b]11;#ffffff\x07\x1b]10;?\x18")

(* The palette is the one create was given; OSC 4, 10, 11 and 12 set it and
   ask for it, OSC 104 and 110 to 112 reset it to that one, as RIS does. *)
let palette_set_query_reset () =
  let colors = Array.init 256 (fun i -> (i, i, i)) in
  let given =
    {
      Vte.colors;
      foreground = (1, 2, 3);
      background = (4, 5, 6);
      cursor = (7, 8, 9);
    }
  in
  let vte = Vte.create ~palette:given ~rows:2 ~cols:10 () in
  colors.(1) <- (9, 9, 9);
  equal ~msg:"the palette create was given, copied" string
    "\x1b]4;1;rgb:0101/0101/0101\x07\x1b]10;rgb:0101/0202/0303\x07\x1b]11;rgb:0404/0505/0606\x07\x1b]12;rgb:0707/0808/0909\x07"
    (answers vte "\x1b]4;1;?\x07\x1b]10;?;?;?\x07");
  colors.(1) <- (1, 1, 1);
  feed vte
    "\x1b]4;1;rgb:ff/00/00;2;#00ff00\x1b\\\x1b]10;#abcdef;rgb:1/2/3\x07\x1b]12;rgb:ffff/0000/ffff\x07";
  equal ~msg:"set, then asked, with the query's terminator" string
    "\x1b]4;1;rgb:ffff/0000/0000\x1b\\\x1b]4;2;rgb:0000/ffff/0000\x1b\\\x1b]10;rgb:abab/cdcd/efef\x1b\\\x1b]11;rgb:1111/2222/3333\x1b\\\x1b]12;rgb:ffff/0000/ffff\x1b\\"
    (answers vte "\x1b]4;1;?;2;?\x1b\\\x1b]10;?;?;?\x1b\\");
  let p = Vte.palette vte in
  equal ~msg:"the palette as set"
    (list (triple int int int))
    [
      (255, 0, 0); (0, 255, 0); (0xab, 0xcd, 0xef); (17, 34, 51); (255, 0, 255);
    ]
    [ p.colors.(1); p.colors.(2); p.foreground; p.background; p.cursor ];
  equal ~msg:"a set then a query in one OSC, in order" string
    "\x1b]4;3;rgb:0101/0202/0303\x07"
    (answers vte "\x1b]4;3;#010203;3;?\x07");
  let colours l = List.map (fun i -> (Vte.palette vte).colors.(i)) l in
  let rgbs = list (triple int int int) in
  feed vte "\x1b]4;5;#050500;6;no such colour;7;#070700\x07";
  equal ~msg:"OSC 4 stops at a spec that names no colour, as xterm" rgbs
    [ (5, 5, 0); (6, 6, 6); (7, 7, 7) ]
    (colours [ 5; 6; 7 ]);
  feed vte "\x1b]4;256;#ffffff;8;#080800\x07\x1b]4;x;#ffffff;9;#090900\x07";
  equal ~msg:"and at an index that names no colour" rgbs
    [ (8, 8, 8); (9, 9, 9) ]
    (colours [ 8; 9 ]);
  equal ~msg:"a query after the error is not answered" string ""
    (answers vte "\x1b]4;1;nope;1;?\x07");
  equal ~msg:"X11 colour names" string
    "\x1b]4;7;rgb:f8f8/f8f8/ffff\x07\x1b]11;rgb:bebe/bebe/bebe\x07"
    (answers vte "\x1b]4;7;Ghost White;7;?\x07\x1b]11;x11gray\x07\x1b]11;?\x07");
  feed vte "\x1b]104;1;300;2;x;3\x07";
  equal
    ~msg:
      "OSC 104 resets the colours listed, past the table skipped, to the first \
       entry that is no number"
    rgbs
    [ (1, 1, 1); (2, 2, 2); (1, 2, 3) ]
    (colours [ 1; 2; 3 ]);
  feed vte "\x1b]104\x07";
  equal ~msg:"OSC 104 with none resets all" (triple int int int) (3, 3, 3)
    (Vte.palette vte).colors.(3);
  feed vte "\x1b]110\x07\x1b]111;\x1b\\\x1b]112\x07";
  is_true ~msg:"OSC 110, 111 and 112 reset the dynamic colours"
    (Vte.palette vte = given);
  feed vte "\x1b]11;#ffffff\x07\x1b[!p";
  equal ~msg:"DECSTR keeps the palette" (triple int int int) (255, 255, 255)
    (Vte.palette vte).background;
  feed vte "\x1bc";
  is_true ~msg:"RIS restores the palette create was given"
    (Vte.palette vte = given);
  Vte.clear_dirty vte;
  feed vte "\x1b]11;#000000\x07";
  equal ~msg:"a change marks every row" (list int) [ 0; 1 ] (Vte.dirty_rows vte);
  is_true ~msg:"and the cursor" (Vte.is_cursor_dirty vte);
  Vte.clear_dirty vte;
  feed vte "\x1b]11;#000000\x07";
  is_false ~msg:"setting the same colour changes nothing" (Vte.is_dirty vte);
  raises_match ~msg:"255 colours" (Exn.invalid_arg ~substring:"256") (fun () ->
      Vte.create
        ~palette:{ given with colors = Array.make 255 (0, 0, 0) }
        ~rows:1 ~cols:1 ());
  raises_match ~msg:"a channel of 256" (Exn.invalid_arg ~substring:"channel")
    (fun () ->
      Vte.create ~palette:{ given with cursor = (256, 0, 0) } ~rows:1 ~cols:1 ())

(* An OSC acts only when it ends with ST or BEL; one cut short does nothing.
   An OSC's text after its number is kept up to 64 KiB. *)
let osc_acts_only_when_terminated () =
  let vte = Vte.create ~rows:1 ~cols:20 () in
  feed vte "\x1b]2;kept\x1b\\";
  List.iter
    (fun cut ->
      feed vte cut;
      equal ~msg:(Printf.sprintf "%S" cut) string "kept" (Vte.title vte))
    [
      "\x1b]2;cut\x1b[m";
      "\x1b]2;cut\x18";
      "\x1b]2;cut\x1a";
      "\x1b]0;cut\x1b7";
      "\x1b]1;icon\x07";
      "\x1b]2cut\x07";
      "\x1b];cut\x07";
    ];
  (* Fed a byte at a time. *)
  String.iter (fun c -> feed vte (String.make 1 c)) "\x1b]0;by\xc3\xa9tes\x07";
  equal ~msg:"split title" string "by\xc3\xa9tes" (Vte.title vte);
  let budget = 65536 in
  feed vte ("\x1b]2;" ^ String.make budget 'a' ^ "\x07");
  equal ~msg:"title at the budget" int budget (String.length (Vte.title vte));
  feed vte ("\x1b]2;" ^ String.make (budget + 1) 'b' ^ "\x07");
  equal ~msg:"title over the budget is dropped whole" int budget
    (String.length (Vte.title vte));
  (* A hyperlink cut short is not installed. *)
  let vte = Vte.create ~rows:1 ~cols:8 () in
  feed vte "\x1b]8;;http://exa\x1b[ma\x1b]8;;http://b.test\x07b";
  equal ~msg:"cut link" string "" (Grid.get_link (Vte.grid vte) 0);
  equal ~msg:"BEL link" string "http://b.test" (Grid.get_link (Vte.grid vte) 1);
  feed vte "\x1b]8;id=1;\x1b\\c";
  equal ~msg:"empty URI closes, with parameters" string ""
    (Grid.get_link (Vte.grid vte) 2)

(* Counts and positions read 0 as their default, 1. *)
let zero_counts_mean_one () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[3;5H\x1b[0A";
  equal ~msg:"CUU 0" (pair int int) (1, 4) (Vte.cursor_pos vte);
  feed vte "\x1b[0C";
  equal ~msg:"CUF 0" (pair int int) (1, 5) (Vte.cursor_pos vte);
  feed vte "\x1b[0I";
  equal ~msg:"CHT 0" (pair int int) (1, 8) (Vte.cursor_pos vte);
  feed vte "\x1b[0;0H";
  equal ~msg:"CUP 0;0" (pair int int) (0, 0) (Vte.cursor_pos vte);
  feed vte "abcd\x1b[0b";
  equal ~msg:"REP 0" string "abcdd" (get_line (Vte.grid vte) 0);
  feed vte "\x1b[1G\x1b[0P";
  equal ~msg:"DCH 0" string "bcdd" (get_line (Vte.grid vte) 0);
  feed vte "\x1b[0X";
  equal ~msg:"ECH 0" string " cdd" (get_line (Vte.grid vte) 0)

(* DA2 and DECXCPR are recognised by their parameter's value. *)
let replies_by_parameter_value () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  equal ~msg:"DA2 and DECXCPR" string "\x1b[>0;0;0c\x1b[?1;1;1R"
    (answers vte "\x1b[>00c\x1b[?6;1n\x1b[>1c\x1b[?5n")

(* In UTF-8, U+0080..U+009F are not controls: they do nothing. *)
let utf8_c1_is_not_a_control () =
  let vte = Vte.create ~rows:3 ~cols:10 () in
  feed vte "a\xc2\x8db\xc2\x9b2Cc\xc2\x9d0;t\xc2\x9c";
  equal ~msg:"nothing performed" string "ab2Cc0;t" (get_line (Vte.grid vte) 0);
  equal ~msg:"title untouched" string "" (Vte.title vte)

(* A designation with a two-byte designator is one sequence: neither of its
   bytes prints. *)
let two_byte_designators () =
  let vte = Vte.create ~rows:1 ~cols:10 () in
  feed vte "\x1b(%6a\x1b(\">q\x1b%/Gr";
  equal ~msg:"designators not printed" string "aqr" (get_line (Vte.grid vte) 0)

(** {1 Resize} *)

(* Resizing keeps the text: a shell's earlier output never repaints. *)
let resize_preserves_content () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  feed vte "Hello\r\nWorld";
  resize vte ~rows:10 ~cols:20;
  equal ~msg:"rows resized" int 10 (Vte.rows vte);
  equal ~msg:"cols resized" int 20 (Vte.cols vte);
  equal ~msg:"content kept" string "Hello" (get_line (Vte.grid vte) 0);
  equal ~msg:"content kept line 2" string "World" (get_line (Vte.grid vte) 1);
  equal ~msg:"cursor kept" (pair int int) (1, 5) (Vte.cursor_pos vte)

let resize_clamps_cursor () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  feed vte "\x1b[8;15H";
  resize vte ~rows:5 ~cols:10;
  let row, col = Vte.cursor_pos vte in
  satisfies ~claim:"cursor row clamped (< 5 rows)" int (fun r -> r < 5) row;
  satisfies ~claim:"cursor col clamped (< 10 cols)" int (fun c -> c < 10) col

(** {1 Reset} *)

let reset_clears_state () =
  let vte = Vte.create ~scrollback:100 ~rows:5 ~cols:20 () in
  feed vte "Hello\r\nWorld";
  feed vte "\x1b[1;31m";
  feed vte "\x1b]0;Title\x07";
  feed vte "\x1b[S";
  Vte.reset vte;
  equal ~msg:"grid cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"title cleared" string "" (Vte.title vte);
  equal ~msg:"cursor at origin" (pair int int) (0, 0) (Vte.cursor_pos vte);
  equal ~msg:"scrollback cleared" int 0 (Vte.scrollback_size vte);
  is_true ~msg:"auto wrap reset" (Vte.modes vte).auto_wrap

let selective_erase () =
  let screen ?(rows = 1) s =
    let vte = Vte.create ~rows ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"DECSEL spares protected cells" string "abc   "
    (screen "\x1b[1\"qabc\x1b[0\"qdef\r\x1b[?2K");
  equal ~msg:"EL erases them" string "      "
    (screen "\x1b[1\"qabc\x1b[0\"qdef\r\x1b[2K");
  equal ~msg:"DECSED spares protected cells" string "ab    \n      "
    (screen ~rows:2 "\x1b[1\"qab\x1b[0\"qc\r\ndef\x1b[?2J");
  equal ~msg:"an overwrite unprotects" string " b    "
    (screen "\x1b[1\"qab\x1b[0\"q\rX\x1b[?2K");
  equal ~msg:"protection moves with ICH" string "  ab  "
    (screen "\x1b[1\"qab\x1b[0\"q\r\x1b[2@\x1b[?2K");
  equal ~msg:"protection moves with IL" string "      \nab    \n      "
    (screen ~rows:3 "\x1b[1\"qab\x1b[0\"qc\r\x1b[L\x1b[?2J");
  equal ~msg:"DECSC saves the attribute" string "ab    "
    (screen "\x1b[1\"q\x1b7\x1b[0\"q\x1b8ab\x1b[?2K");
  (* DECSTR makes the characters written next erasable (VT510 DECSTR,
     DECSCA); those already protected stay protected. *)
  equal ~msg:"DECSTR keeps protected cells" string "ab    "
    (screen "\x1b[1\"qab\x1b[!p\x1b[?2K");
  equal ~msg:"DECSTR protects nothing written next" string "ab    "
    (screen "\x1b[1\"qab\x1b[!pcd\x1b[?2K")

let reverse_wrap () =
  let screen s =
    let vte = Vte.create ~rows:2 ~cols:4 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"BS crosses an automatic wrap" string "abcX\nZ   "
    (screen "\x1b[?45habcdZ\b\bX");
  equal ~msg:"not without mode 45" string "abcd\nX   " (screen "abcdZ\b\bX");
  equal ~msg:"nor a line feed" string "abc \nX   "
    (screen "\x1b[?45habc\r\nZ\b\bX");
  equal ~msg:"a pending wrap takes the first step" string "abcX\n    "
    (screen "\x1b[?45habcd\bX");
  equal ~msg:"CUB crosses it too" string "abXd\nZ   "
    (screen "\x1b[?45habcdZ\x1b[3DX");
  equal ~msg:"DECSTR resets the mode" string "abcd\nX   "
    (screen "\x1b[?45h\x1b[!pabcdZ\b\bX");
  equal ~msg:"a wide character wrapped early" string "abcX"
    (List.hd
       (String.split_on_char '\n' (screen "\x1b[?45habc\xe4\xb8\xad\b\b\bX")))

let ris_and_decstr () =
  let run s =
    let vte = Vte.create ~rows:2 ~cols:10 () in
    feed vte s;
    vte
  in
  let screen s = Vte.to_string (run s) in
  equal ~msg:"RIS turns auto-wrap back on" string "0123456789\nq         "
    (screen "\x1b[?7l\x1bc0123456789q");
  is_true ~msg:"RIS shows the cursor"
    (Vte.modes (run "\x1b[?25l\x1bc")).cursor_visible;
  equal ~msg:"RIS resets the tab stops" string "        X \n          "
    (screen "\x1b[3g\x1bc\tX");
  equal ~msg:"DECSTR resets insert mode and auto-wrap" string
    "Xb        \n          "
    (screen "\x1b[4h\x1b[?7l\x1b[!pab\rX");
  equal ~msg:"DECSTR keeps the cursor and resets the saved one" string
    "X Y       \n          "
    (screen "\x1b[2;5H\x1b7\x1b[1;3H\x1b[!pY\x1b8X");
  let vte = run "\x1b[?25l\x1b[1m\x1b[!pX" in
  is_true ~msg:"DECSTR shows the cursor" (Vte.modes vte).cursor_visible;
  is_true ~msg:"DECSTR resets SGR"
    (Ansi.Attr.is_empty (Ansi.Attr.unpack (Grid.get_attrs (Vte.grid vte) 0)))

(** {1 Dirty Tracking} *)

let dirty_flag_on_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"clean initially" (Vte.is_dirty vte);
  feed vte "Hello";
  is_true ~msg:"dirty after text" (Vte.is_dirty vte);
  Vte.clear_dirty vte;
  is_false ~msg:"clean after clear" (Vte.is_dirty vte)

let dirty_flag_on_cursor_move () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  feed vte "\x1b[5;10H";
  is_true ~msg:"dirty after cursor move" (Vte.is_dirty vte)

let dirty_flag_on_style_change () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  feed vte "\x1b[1m";
  is_true ~msg:"dirty after style change" (Vte.is_dirty vte)

let dirty_row_tracking () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Write to row 0 *)
  feed vte "Hello";
  is_true ~msg:"dirty after write" (Vte.is_dirty vte);
  equal ~msg:"row 0 dirty" (list int) [ 0 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Move to row 5 and write *)
  feed vte "\x1b[6;1HWorld";
  equal ~msg:"row 5 dirty" (list int) [ 5 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Erase display (affects multiple rows) *)
  feed vte "\x1b[2J";
  let rows = Vte.dirty_rows vte in
  satisfies ~claim:"multiple rows dirty (> 1)" (list int)
    (fun l -> List.length l > 1)
    rows;
  mem ~msg:"includes row 0" int 0 rows

let dirty_cursor_tracking () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Move cursor *)
  feed vte "\x1b[3;5H";
  is_true ~msg:"cursor dirty after move" (Vte.is_cursor_dirty vte);

  Vte.clear_dirty vte;
  is_false ~msg:"cursor clean after clear" (Vte.is_cursor_dirty vte);

  (* Hide cursor *)
  feed vte "\x1b[?25l";
  is_true ~msg:"cursor dirty after visibility change" (Vte.is_cursor_dirty vte)

(** {1 Unicode Support} *)

let unicode_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello 世界 🌍";
  let output = Vte.to_string vte in
  equal ~msg:"unicode text" string "Hello 世界 🌍" (String.trim output)

let unicode_with_escapes () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "\x1b[31mRed 文字\x1b[0m Normal";
  let output = Vte.to_string vte in
  equal ~msg:"unicode with ANSI" string "Red 文字 Normal" (String.trim output)

let unicode_multiline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Line1\r\nΓειά σου\r\n日本語";
  let output = Vte.to_string vte in
  equal ~msg:"multiline unicode" string
    "Line1               \nΓειά σου            \n日本語" (String.trim output)

let unicode_wide_char_cursor () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  feed vte "Hello 世界";
  let row, col = Vte.cursor_pos vte in
  (* "Hello " = 6 cols, "世界" = 4 cols (2 wide chars × 2 cols each) *)
  equal ~msg:"cursor after wide chars" (pair int int) (0, 10) (row, col)

let unicode_wide_char_wraps_at_margin () =
  (* A wide grapheme that doesn't fit at the right margin wraps to the next
     row; the rest of the input must not be dropped. *)
  let vte = Vte.create ~rows:2 ~cols:4 () in
  feed vte "abc全xy";
  equal ~msg:"wide grapheme wraps with following text" string "abc \n全xy"
    (Vte.to_string vte);
  equal ~msg:"cursor on the last column" (pair int int) (1, 3)
    (Vte.cursor_pos vte);
  (* With auto-wrap off the wide grapheme ends at the margin, replacing what
     is there; every following character replaces the last column in turn,
     and blanks the half it cuts. *)
  let vte = Vte.create ~rows:2 ~cols:4 () in
  feed vte "\x1b[?7l";
  feed vte "abc全";
  equal ~msg:"ends at the margin" string "ab全\n    " (Vte.to_string vte);
  feed vte "xy";
  equal ~msg:"no wrap when DECAWM off" string "ab y\n    " (Vte.to_string vte)

let unicode_wide_char_wider_than_line () =
  (* Shrunk from a split-invariance property counterexample: on a 1x1 grid,
     "你x" fed whole dropped the "x" while "你" then "x" printed it. A
     grapheme wider than the whole line is skipped; the rest of the run must
     still be processed. *)
  let vte = Vte.create ~rows:1 ~cols:1 () in
  feed vte "\xE4\xBD\xA0x";
  equal ~msg:"text after unfittable wide grapheme survives" string "x"
    (Vte.to_string vte);
  equal ~msg:"cursor on the last column" (pair int int) (0, 0)
    (Vte.cursor_pos vte);
  let split = Vte.create ~rows:1 ~cols:1 () in
  feed split "\xE4\xBD\xA0";
  feed split "x";
  equal ~msg:"split feed matches whole feed" string (Vte.to_string vte)
    (Vte.to_string split)

let unicode_combining_mark_across_writes () =
  (* Shrunk from a split-invariance property counterexample: a combining
     mark that arrives in a later write must attach to the grapheme in the
     preceding cell, as real terminals do, instead of drawing an orphan
     cluster at the cursor. *)
  let whole = Vte.create ~rows:1 ~cols:4 () in
  feed whole "e\xCC\x81x";
  let split = Vte.create ~rows:1 ~cols:4 () in
  feed split "e";
  feed split "\xCC\x81x";
  equal ~msg:"split combining mark matches whole feed" string
    (Vte.to_string whole) (Vte.to_string split);
  equal ~msg:"cursor matches" (pair int int) (Vte.cursor_pos whole)
    (Vte.cursor_pos split);
  (* A mark after a wide grapheme attaches through the continuation cell and
     the cluster ends up merged in the start cell. *)
  let whole = Vte.create ~rows:1 ~cols:4 () in
  feed whole "\xE4\xBD\xA0\xCC\x81";
  let split = Vte.create ~rows:1 ~cols:4 () in
  feed split "\xE4\xBD\xA0";
  feed split "\xCC\x81";
  equal ~msg:"wide grapheme keeps its mark across writes" string
    (Vte.to_string whole) (Vte.to_string split);
  equal ~msg:"cluster merged into the start cell" string "\xE4\xBD\xA0\xCC\x81"
    (Grid.get_text (Vte.grid split) 0)

let unicode_prepend_across_writes () =
  (* A Prepend character starts the grapheme cluster of the character after
     it (UAX #29 GB9b), and a read split between them draws the same screen.
     A wcwidth terminal places it with the character before it, as one of no
     width. *)
  let cells ?(cols = 4) width_method chunks =
    let vte = Vte.create ~width_method ~rows:1 ~cols () in
    List.iter (feed vte) chunks;
    List.init cols (fun i -> Grid.get_text (Vte.grid vte) i)
  in
  let bytes s = List.init (String.length s) (fun i -> String.make 1 s.[i]) in
  List.iter
    (fun (width_method, name) ->
      List.iter
        (fun (cols, s, expected) ->
          let msg = Printf.sprintf "%s %S" name s in
          equal ~msg (list string) expected (cells ~cols width_method [ s ]);
          equal ~msg:(msg ^ " split") (list string) expected
            (cells ~cols width_method (bytes s)))
        (match width_method with
        | `Wcwidth ->
            [
              (4, "a\u{06DD}q", [ "a\u{06DD}"; "q"; " "; " " ]);
              (4, "\u{06DD}q", [ "q"; " "; " "; " " ]);
              (2, "\x1b[?7lab\u{06DD}q", [ "a"; "q" ]);
              (4, "\u{0D4E}x", [ "\u{0D4E}"; "x"; " "; " " ]);
            ]
        | _ ->
            [
              (4, "a\u{06DD}q", [ "a"; "\u{06DD}q"; " "; " " ]);
              (4, "\u{06DD}q", [ "\u{06DD}q"; " "; " "; " " ]);
              (2, "\x1b[?7lab\u{06DD}q", [ "a"; "\u{06DD}q" ]);
              (4, "\u{0D4E}x", [ "\u{0D4E}x"; " "; " "; " " ]);
            ]))
    [ (`Unicode, "unicode"); (`Wcwidth, "wcwidth") ]

let unicode_zero_width_joins_cell () =
  (* U+200B and U+2060 join the cell before them, in the same write or a
     later one, also in insert mode. *)
  let cells s =
    let vte = Vte.create ~rows:1 ~cols:4 () in
    List.iter (feed vte) s;
    List.init 4 (fun i -> Grid.get_text (Vte.grid vte) i)
  in
  equal ~msg:"same write" (list string)
    [ "a"; "b\u{200B}"; "c"; " " ]
    (cells [ "ab\u{200B}c" ]);
  equal ~msg:"later write" (list string)
    [ "a"; "b\u{2060}"; " "; " " ]
    (cells [ "ab"; "\u{2060}" ]);
  equal ~msg:"insert mode" (list string)
    [ "a\u{200B}"; "b"; "x"; " " ]
    (cells [ "x\r\x1b[4ha\u{200B}b" ])

let unicode_mark_after_dropped_wide () =
  (* With auto-wrap off, a wide character that does not fit at the right
     margin is dropped, and so is a mark that follows it in a later write. *)
  let screen chunks =
    let vte = Vte.create ~rows:1 ~cols:3 () in
    List.iter (feed vte) chunks;
    Vte.to_string vte
  in
  let wide = "\x1b[?7l\xE4\xBD\xA0" and mark = "\xCC\x81" in
  equal ~msg:"last column" string
    (screen [ "\x1b[1;3H" ^ wide ^ mark ])
    (screen [ "\x1b[1;3H" ^ wide; mark ]);
  equal ~msg:"past the last column" string
    (screen [ "abc" ^ wide ^ mark ])
    (screen [ "abc" ^ wide; mark ])

let unicode_malformed_utf8 () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  let malformed = Bytes.create 5 in
  Bytes.set malformed 0 'A';
  Bytes.set malformed 1 '\xff';
  (* Invalid UTF-8 byte *)
  Bytes.set malformed 2 '\xfe';
  (* Invalid UTF-8 byte *)
  Bytes.set malformed 3 'B';
  Bytes.set malformed 4 'C';
  ignore (Vte.feed vte malformed 0 5 : _ list);
  let output = Vte.to_string vte in
  (* Invalid UTF-8 bytes are replaced with U+FFFD (replacement character) *)
  (* U+FFFD in UTF-8 is \xEF\xBF\xBD (\239\191\189) *)
  equal ~msg:"malformed UTF-8" string "A\239\191\189\239\191\189BC"
    (String.trim output)

let hyperlinks_survive_styles_and_scrollback () =
  let vte = Vte.create ~rows:1 ~cols:8 () in
  feed vte "\027]8;;https://example.test\027\\a\027[31mb\027[0mc\027]8;;\027\\d";
  let check grid =
    for i = 0 to 2 do
      equal ~msg:"link survives SGR changes" string "https://example.test"
        (Grid.get_link grid i)
    done;
    equal ~msg:"OSC8 closes link" string "" (Grid.get_link grid 3)
  in
  check (Vte.grid vte);
  feed vte "\r\n";
  let dst = Grid.create ~width:8 ~height:1 () in
  Vte.blit_view vte ~offset:1 dst;
  check dst

let scrollback_preserves_decorated_spaces () =
  let vte = Vte.create ~rows:1 ~cols:8 () in
  feed vte
    "x\027[44m  \027[0;4m \027[0m\027]8;;https://space.test\027\\ \
     \027]8;;\027\\";
  let before = Vte.grid vte in
  let styles = Array.init 5 (Grid.get_style before) in
  feed vte "\r\n";
  equal ~msg:"plain history remains trimmed" (list string) [ "x" ]
    (scrollback_lines vte);
  let dst = Grid.create ~width:8 ~height:1 () in
  Vte.blit_view vte ~offset:1 dst;
  Array.iteri
    (fun i expected ->
      is_true ~msg:"paint, attributes and links survive compression"
        (Ansi.Style.equal expected (Grid.get_style dst i)))
    styles

let clearing_scrollback_releases_text () =
  List.iter
    (fun reset ->
      let vte = Vte.create ~rows:1 ~cols:80 () in
      let weak = Weak.create 1 in
      let fill () =
        feed vte (String.make 40 'x' ^ "\r\n");
        Weak.set weak 0 (Some (List.hd (scrollback_lines vte)))
      in
      fill ();
      Gc.full_major ();
      is_true ~msg:"live history owns its text" (Weak.check weak 0);
      if reset then Vte.reset vte else feed vte "\027[3J";
      Gc.full_major ();
      is_false ~msg:"cleared history releases its text" (Weak.check weak 0);
      equal ~msg:"history cleared" int 0 (Vte.scrollback_size vte))
    [ false; true ]

let wcwidth_combining_at_margin_and_insert () =
  let vte = Vte.create ~width_method:`Wcwidth ~rows:1 ~cols:2 () in
  feed vte "\027[?7labć";
  equal ~msg:"mark attaches to overwritten margin glyph" string "ć"
    (Grid.get_text (Vte.grid vte) 1);
  Vte.reset vte;
  feed vte "\027[4hé";
  equal ~msg:"insert preserves the entire cluster" string "é"
    (Grid.get_text (Vte.grid vte) 0);
  equal ~msg:"insert measures the cluster" (pair int int) (0, 1)
    (Vte.cursor_pos vte)

let wcwidth_insert_measures_complete_cluster () =
  let vte = Vte.create ~width_method:`Wcwidth ~rows:1 ~cols:8 () in
  feed vte "ABCDEFGH\r\027[4h👨‍💻";
  equal ~msg:"insert shifts by the entire four-column cluster" string "A"
    (Grid.get_text (Vte.grid vte) 4);
  (* A wcwidth terminal places each emoji in its own cells, with the ZWJ after
     the first. *)
  equal ~msg:"insert places wcwidth units" (pair string string)
    ("👨\u{200D}", "💻")
    (Grid.get_text (Vte.grid vte) 0, Grid.get_text (Vte.grid vte) 2);
  equal ~msg:"cursor uses the complete width" (pair int int) (0, 4)
    (Vte.cursor_pos vte)

let long_runs_match_fragmented_runs () =
  List.iter
    (fun width_method ->
      List.iter
        (fun insert ->
          let whole = Vte.create ~width_method ~rows:3 ~cols:13 () in
          let split = Vte.create ~width_method ~rows:3 ~cols:13 () in
          if insert then begin
            feed whole "\027[4h";
            feed split "\027[4h"
          end;
          let unit = "ab中é" in
          let text = String.concat "" (List.init 150 (fun _ -> unit)) in
          feed whole text;
          List.iter (feed split) (List.init 150 (fun _ -> unit));
          equal ~msg:"row slices match separate runs" string
            (Vte.to_string split) (Vte.to_string whole);
          equal ~msg:"cursor matches separate runs" (pair int int)
            (Vte.cursor_pos split) (Vte.cursor_pos whole);
          equal ~msg:"history matches separate runs" (list string)
            (scrollback_lines split) (scrollback_lines whole))
        [ false; true ])
    [ `Unicode; `Wcwidth ]

let character_sets () =
  let line s =
    let vte = Vte.create ~rows:1 ~cols:6 () in
    feed vte s;
    Vte.to_string vte
  in
  equal ~msg:"DEC Special Graphics in G0" string
    "\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x90\xe2\x94\x82q "
    (line "\x1b(0lqkx\x1b(Bq");
  equal ~msg:"SO and SI" string "a\xe2\x94\x80q   " (line "\x1b)0a\x0eq\x0fq");
  equal ~msg:"LS2" string "\xe2\x94\x80     " (line "\x1b*0\x1bnq");
  equal ~msg:"British" string "a\xc2\xa3    " (line "\x1b(Aa#");
  let line s = line ("\x1b[?42h" ^ s) in
  equal ~msg:"German" string "\xc3\x84\xc3\x9c\xc3\xa4\xc3\xbc  "
    (line "\x1b(K[]{}");
  equal ~msg:"Dutch and French" string "\u{0133}\u{00e0}    "
    (line "\x1b(4[\x1b(R@");
  equal ~msg:"Swedish in G1" string "\u{00c9}\u{00fc}    " (line "\x1b)7\x0e@~");
  equal ~msg:"Swiss" string "\u{00f9}\u{00e8}    " (line "\x1b(=#_");
  equal ~msg:"DECRC restores the designations" string "\xe2\x94\x80     "
    (line "\x1b(0\x1b7\x1b(B\x1b8q")

(** {1 The model} *)

let screen_of ?(rows = 1) ?(cols = 6) ?width_method chunks =
  let vte = Vte.create ?width_method ~rows ~cols () in
  List.iter (feed vte) chunks;
  vte

(* The cells, cursor, wrap marks and scrollback that make the screen. *)
let state vte =
  let g = Vte.grid vte in
  let cells =
    List.init
      (Grid.width g * Grid.height g)
      (fun i ->
        Printf.sprintf "%S/%d/%b" (Grid.get_text g i) (Grid.cell_width g i)
          (Grid.is_continuation g i))
  in
  let marks =
    List.init
      (Vte.rows vte + Vte.scrollback_size vte)
      (fun i -> Vte.wrapped vte (i - Vte.scrollback_size vte))
  in
  (String.concat " " cells, (Vte.cursor_pos vte, (marks, scrollback_lines vte)))

let state_t = pair string (pair (pair int int) (pair (list bool) (list string)))

(* Every split of [s] in two gives the screen of [s] fed whole. *)
let check_splits ?(rows = 2) ?(cols = 6) ?width_method msg s =
  let whole = state (screen_of ~rows ~cols ?width_method [ s ]) in
  for i = 1 to String.length s - 1 do
    let split =
      screen_of ~rows ~cols ?width_method
        [ String.sub s 0 i; String.sub s i (String.length s - i) ]
    in
    equal
      ~msg:(Printf.sprintf "%s, split at %d" msg i)
      state_t whole (state split)
  done

(* DECSC saves the Last Column Flag: a character after DECRC wraps. Every
   function that moves the cursor or changes cells clears it; HT, CHT, SGR,
   settings and reports keep it; resetting DECAWM clears it. *)
let last_column_flag () =
  let pos s =
    Vte.cursor_pos (screen_of ~rows:3 ~cols:10 [ "\x1b[1;9HAB" ^ s ])
  in
  equal ~msg:"set at the last column" (pair int int) (0, 9) (pos "");
  is_true ~msg:"which last_column shows"
    (Vte.last_column (screen_of ~rows:3 ~cols:10 [ "\x1b[1;9HAB" ]));
  is_false ~msg:"and not before"
    (Vte.last_column (screen_of ~rows:3 ~cols:10 [ "\x1b[1;9HA" ]));
  equal ~msg:"DECSC saves it" (pair int int) (1, 1)
    (pos "\x1b7\x1b[3;1HQ\x1b8X");
  equal ~msg:"HT keeps it" (pair int int) (1, 1) (pos "\tX");
  equal ~msg:"CHT keeps it" (pair int int) (1, 1) (pos "\x1b[2IX");
  equal ~msg:"SGR keeps it" (pair int int) (1, 1) (pos "\x1b[1mX");
  equal ~msg:"a report keeps it" (pair int int) (1, 1) (pos "\x1b[6nX");
  equal ~msg:"CBT clears it and moves" (pair int int) (0, 9) (pos "\x1b[ZX");
  equal ~msg:"resetting DECAWM clears it" (pair int int) (0, 9)
    (pos "\x1b[?7l\x1b[?7hX");
  equal ~msg:"SU clears it" (pair int int) (0, 9) (pos "\x1b[SX");
  equal ~msg:"ICH clears it" (pair int int) (0, 9) (pos "\x1b[@X");
  equal ~msg:"CUF clears it" (pair int int) (0, 9) (pos "\x1b[CX");
  List.iter
    (fun f ->
      equal
        ~msg:(f ^ ", which changes no cell, keeps it")
        (pair int int) (1, 1)
        (pos (f ^ "X")))
    [ "\x1b[3J"; "\x1b[4J"; "\x1b[3K" ];
  let vte = screen_of ~rows:2 ~cols:4 [ "abcd\x1b[?7lX" ] in
  equal ~msg:"inert with autowrap off" string "abcX\n    " (Vte.to_string vte);
  let vte = screen_of ~rows:2 ~cols:4 [ "abcd\x1b7\x1b[?7l\x1b8" ] in
  is_false ~msg:"DECRC with autowrap off restores no flag" (Vte.last_column vte);
  feed vte "\x1b[?7hX";
  equal ~msg:"so the next character replaces the last column" string
    "abcX\n    " (Vte.to_string vte)

(* VPR stops at the bottom margin in origin mode. *)
let vpr_in_origin_mode () =
  equal ~msg:"VPR" (pair int int) (2, 0)
    (Vte.cursor_pos (screen_of ~rows:6 [ "\x1b[2;3r\x1b[?6h\x1b[5e" ]))

(* Modes 47, 1047, 1048 and 1049 compose switching, erasing, DECSC and
   DECRC. *)
let alternate_screen_modes () =
  let text s = Vte.to_string (screen_of ~rows:1 [ s ]) in
  equal ~msg:"47 keeps the alternate screen" string "alt   "
    (text "\x1b[?47halt\x1b[?47l\x1b[?47h");
  equal ~msg:"1047 erases it on leaving" string "      "
    (text "\x1b[?1047halt\x1b[?1047l\x1b[?47h");
  equal ~msg:"1049 erases it on entering" string "      "
    (text "\x1b[?47halt\x1b[?47l\x1b[?1049h");
  equal ~msg:"1048 saves and restores the cursor" (pair int int) (1, 2)
    (Vte.cursor_pos
       (screen_of ~rows:6 [ "\x1b[2;3H\x1b[?1048h\x1b[5;5H\x1b[?1048l" ]));
  let vte = screen_of ~rows:2 [ "ab\x1b[?1049h\x1b[2;4Hz\x1b[?1049lX" ] in
  equal ~msg:"1049 restores the cursor" string "abX   \n      "
    (Vte.to_string vte);
  is_false ~msg:"and the primary screen" (Vte.modes vte).alternate_screen;
  equal ~msg:"leaving 1049 on the primary screen restores the cursor"
    (pair int int) (0, 0)
    (Vte.cursor_pos (screen_of [ "a\x1b[?1049l" ]));
  equal ~msg:"1049 on the alternate screen erases it" string "      "
    (text "\x1b[?1047ha\x1b[?1049h");
  equal ~msg:"entering 1049 clears the Last Column Flag" (pair int int) (0, 5)
    (Vte.cursor_pos (screen_of ~rows:2 [ "abcdef\x1b[?1049ha" ]))

(* REP repeats the unit before it, exactly, whatever the count. *)
let repeat_exactly () =
  let state_of ?(setup = "") ?(rows = 2) ?(cols = 5) s =
    let vte = Vte.create ~scrollback:3 ~rows ~cols () in
    feed vte (setup ^ s);
    state vte
  in
  (* A literal run against REP, for counts past the screen and the
     scrollback. *)
  List.iter
    (fun (setup, unit) ->
      for n = 1 to 60 do
        let literal = String.concat "" (List.init (n + 1) (fun _ -> unit)) in
        equal
          ~msg:(Printf.sprintf "%S %S REP %d" setup unit n)
          state_t (state_of ~setup literal)
          (state_of ~setup (Printf.sprintf "%s\x1b[%db" unit n))
      done)
    [
      ("", "a");
      ("", "\xe4\xb8\xad");
      ("", "e\xcc\x81");
      ("\x1b[?7l", "a");
      ("\x1b[?7l", "\xe4\xb8\xad");
      ("\x1b[4h", "a");
      ("\x1b[4hxyzw\r", "\xe4\xb8\xad");
      ("\x1b[2;2r\x1b[2;1H", "a");
    ];
  equal ~msg:"no cap"
    (pair string (pair int int))
    ("aaaaa\naaa  ", (1, 3))
    (let vte = screen_of ~rows:2 ~cols:5 [ "a\x1b[12b" ] in
     (Vte.to_string vte, Vte.cursor_pos vte));
  let huge = state_of (Printf.sprintf "a\x1b[%db" max_int) in
  (* 1 + max_int prints are 4 mod 5, as 39 are: once the screen and the
     scrollback hold the same rows, only the count modulo the 5 units of a
     row matters. *)
  equal ~msg:"a saturated count" state_t (state_of (String.make 39 'a')) huge;
  let cells s = Vte.to_string (screen_of ~rows:1 ~cols:8 [ s ]) in
  equal ~msg:"a keycap" string "1\xef\xb8\x8f1\xef\xb8\x8f1\xef\xb8\x8f  "
    (cells "1\xef\xb8\x8f\x1b[2b");
  equal ~msg:"a regional indicator stays a unit" string "\u{1F1EB}\u{1F1EB}"
    (String.concat ""
       (List.filter
          (fun s -> s <> " ")
          (let g = Vte.grid (screen_of ~rows:1 ~cols:8 [ "\u{1F1EB}\x1b[b" ]) in
           List.init 8 (Grid.get_text g))));
  equal ~msg:"REP after REP repeats nothing" string "aa      "
    (cells "a\x1b[b\x1b[b");
  equal ~msg:"a mark after REP joins the last repeat" string "aaee\xcc\x81    "
    (cells "aae\x1b[b\xcc\x81")

(* Every edit's blank has the colour ED's blank has, and no attributes, also
   where the edit cuts a wide character. *)
let one_blank () =
  let rows text = String.concat "\r\n" [ text; text; text ] in
  let blank_bg =
    Grid.get_bg (Vte.grid (screen_of ~rows:3 [ "\x1b[41m\x1b[2J" ])) 0
  in
  List.iter
    (fun (fill, col) ->
      List.iter
        (fun (name, op) ->
          let vte =
            screen_of ~rows:3
              [ fill; Printf.sprintf "\x1b[2;%dH\x1b[1;41m" col; op ]
          in
          let g = Vte.grid vte in
          for i = 0 to 17 do
            if Grid.get_text g i = " " then (
              is_true
                ~msg:
                  (Printf.sprintf "%s, %S at column %d: cell %d colour" name
                     fill col i)
                (Ansi.Color.equal (Grid.get_bg g i) blank_bg);
              is_true
                ~msg:
                  (Printf.sprintf "%s, %S at column %d: cell %d attributes" name
                     fill col i)
                (Grid.get_attrs g i = 0))
          done)
        [
          ("ED 0", "\x1b[J");
          ("ED 1", "\x1b[1J");
          ("ED 2", "\x1b[2J");
          ("EL 0", "\x1b[K");
          ("EL 1", "\x1b[1K");
          ("EL 2", "\x1b[2K");
          ("ECH", "\x1b[X");
          ("ICH", "\x1b[@");
          ("DCH", "\x1b[P");
          ("IL", "\x1b[L");
          ("DL", "\x1b[M");
          ("SU", "\x1b[S");
          ("SD", "\x1b[T");
          ("SL", "\x1b[ @");
          ("SR", "\x1b[ A");
          ("DECIC", "\x1b['}");
          ("DECDC", "\x1b['~");
          ("LF", "\x1b[3H\n");
          ("RI", "\x1b[H\x1bM");
          ("insert mode", "\x1b[4hx");
          ("a wrap", "\x1b[2;6H\xe4\xb8\xad");
          ("a cut by printing", "x");
        ])
    [
      (rows "\xe4\xb8\xad\xe4\xb8\xad\xe4\xb8\xad", 3);
      (rows "\xe4\xb8\xad\xe4\xb8\xad\xe4\xb8\xad", 4);
      (rows "abcdef", 3);
    ]

(* A unit of no width that continues nothing joins the cell before the
   cursor, keeping its width and rendition, or is dropped; it is taken back
   if it gains a width. *)
let zero_width_units () =
  let cells ?(cols = 8) chunks =
    let g = Vte.grid (screen_of ~cols chunks) in
    List.init cols (Grid.get_text g)
  in
  equal ~msg:"after SGR" (list string)
    [ "a\u{200B}"; "b"; " "; " "; " "; " "; " "; " " ]
    (cells [ "a\x1b[0m\u{200B}b" ]);
  equal ~msg:"after a cursor motion" (list string)
    [ "a"; " "; " "; " "; " "; " \u{200B}"; "b"; " " ]
    (cells [ "a\x1b[5C\u{200B}b" ]);
  equal ~msg:"a mark after a control function" (list string)
    [ "e\xcc\x81"; " "; " "; " "; " "; " "; " "; " " ]
    (cells [ "e\x1b[1m\xcc\x81" ]);
  let vte = screen_of [ "e\x1b[1m\xcc\x81" ] in
  equal ~msg:"keeps the rendition" int 0 (Grid.get_attrs (Vte.grid vte) 0);
  equal ~msg:"at the first column it is dropped" (list string)
    [ "x"; " "; " "; " "; " "; " "; " "; " " ]
    (cells [ "\u{200B}x" ]);
  equal ~msg:"with the Last Column Flag" (list string)
    [ "a"; "b"; "c"; "d\u{200B}" ]
    (cells ~cols:4 [ "abcd\u{200B}" ]);
  equal ~msg:"keeps a wide character's width" (list string)
    [ "\xe4\xb8\xad\u{2060}"; ""; "x"; " " ]
    (cells ~cols:4 [ "\xe4\xb8\xad"; "\u{2060}"; "x" ]);
  check_splits "a Prepend taken back" "x\u{0600}ab";
  equal ~msg:"a Prepend taken back" (list string)
    [ "x"; "\u{0600}a"; "b"; " "; " "; " "; " "; " " ]
    (cells [ "x"; "\u{0600}"; "ab" ])

(* Placement does not depend on how the output was split. *)
let split_counterexamples () =
  List.iter
    (fun width_method ->
      check_splits ~width_method "Malayalam dot reph" "\u{0D4E}ab";
      check_splits ~width_method "Sharada jihvamuliya" "x\u{111C2}ab";
      check_splits ~width_method "a keycap" "a1\u{FE0F}";
      check_splits ~width_method "a keycap at the margin" "abcde1\u{FE0F}x";
      check_splits ~width_method "a keycap at the margin, no autowrap"
        "\x1b[?7labcde1\u{FE0F}x";
      check_splits ~width_method "a keycap in insert mode"
        "uvwxyz\r\x1b[4ha1\u{FE0F}b";
      check_splits ~width_method "a ZWJ sequence"
        "abcd\u{1F468}\u{200D}\u{1F4BB}e";
      check_splits ~width_method "VS16 and VS15"
        "abcde\u{2764}\u{FE0F}\u{231A}\u{FE0E}";
      check_splits ~width_method "a flag at the margin"
        "abcde\u{1F1EB}\u{1F1F7}x";
      check_splits ~width_method "Hangul jamo" "a\u{1100}\u{1161}\u{11A8}b";
      check_splits ~width_method "marks after a wrap"
        "abcde\xe4\xb8\xad\xcc\x81\xcc\x81x")
    [ `Unicode; `Unicode_wide; `Wcwidth ];
  let g = Vte.grid (screen_of [ "\u{0D4E}ab" ]) in
  equal ~msg:"a Prepend letter starts the cluster" (pair string string)
    ("\u{0D4E}a", "b")
    (Grid.get_text g 0, Grid.get_text g 1)

(* A unit that does not fit, with autowrap, erases to the margin before it
   wraps; without, it ends at the margin. *)
let placement_rules () =
  let text ?(cols = 4) s = Vte.to_string (screen_of ~rows:2 ~cols [ s ]) in
  equal ~msg:"erase before a wrap" string "abc \n\xe4\xb8\xad  "
    (text "abcd\r\x1b[3C\xe4\xb8\xad");
  equal ~msg:"ends at the margin" string "ab\xe4\xb8\xad\n    "
    (text "\x1b[?7labc\xe4\xb8\xad");
  equal ~msg:"narrowed to the line" string "\xe4\xb8\xad\n "
    (text ~cols:1 "\xe4\xb8\xad");
  equal ~msg:"insert mode moves the line by the unit" string
    "\xe4\xb8\xadab\n    "
    (text "ab\r\x1b[4h\xe4\xb8\xad");
  equal ~msg:"insert mode wraps a unit at the last column" string
    "    \n\u{ff21}  "
    (text "\x1b[4G\x1b[4h\u{ff21}")

(* Wrap marks: set by autowrap only, kept in the scrollback, moved by a
   scroll, cleared by an edit that reaches the last column. *)
let wrap_marks () =
  let marks s =
    let vte = Vte.create ~scrollback:4 ~rows:3 ~cols:4 () in
    feed vte s;
    List.init
      (3 + Vte.scrollback_size vte)
      (fun i -> Vte.wrapped vte (i - Vte.scrollback_size vte))
  in
  equal ~msg:"autowrap" (list bool) [ true; false; false ] (marks "abcdef");
  equal ~msg:"not a line feed" (list bool) [ false; false; false ]
    (marks "abcd\r\nef");
  equal ~msg:"into the scrollback" (list bool)
    [ true; true; true; false ]
    (marks "abcdefghijklmn");
  equal ~msg:"moved by IL" (list bool) [ false; true; false ]
    (marks "abcdef\x1b[H\x1b[L");
  equal ~msg:"cleared by EL" (list bool) [ false; false; false ]
    (marks "abcdef\x1b[H\x1b[K");
  equal ~msg:"kept by an erase short of the end" (list bool)
    [ true; false; false ]
    (marks "abcdef\x1b[H\x1b[1K");
  equal ~msg:"cleared by DCH" (list bool) [ false; false; false ]
    (marks "abcdef\x1b[H\x1b[P");
  equal ~msg:"no next row below the region" (list bool) [ false; false; false ]
    (marks "\x1b[1;2r\x1b[3Habcdef")

(* Counts are clamped before arithmetic: a saturated parameter acts like the
   largest move. *)
let huge_parameters () =
  let pos s = Vte.cursor_pos (screen_of ~rows:4 ~cols:6 [ "\x1b[2;3H" ^ s ]) in
  let big = "99999999999999999999999" in
  List.iter
    (fun (name, s, expected) ->
      equal ~msg:name (pair int int) expected (pos (Printf.sprintf s big)))
    [
      ("CUF", "\x1b[%sC", (1, 5));
      ("CUB", "\x1b[%sD", (1, 0));
      ("CUU", "\x1b[%sA", (0, 2));
      ("CUD", "\x1b[%sB", (3, 2));
      ("VPR", "\x1b[%se", (3, 2));
      ("HPR", "\x1b[%sa", (1, 5));
      ("CUP", "\x1b[%s;99999999999999999999H", (3, 5));
      ("CHT", "\x1b[%sI", (1, 5));
      ("CBT", "\x1b[%sZ", (1, 0));
      ("CNL", "\x1b[%sE", (3, 0));
    ];
  let text s =
    Vte.to_string (screen_of ~rows:2 ~cols:4 [ "abcd\r\nefgh\x1b[1;2H" ^ s ])
  in
  equal ~msg:"ICH" string "a   \nefgh" (text ("\x1b[" ^ big ^ "@"));
  equal ~msg:"DCH" string "a   \nefgh" (text ("\x1b[" ^ big ^ "P"));
  equal ~msg:"ECH" string "a   \nefgh" (text ("\x1b[" ^ big ^ "X"));
  equal ~msg:"IL" string "    \n    " (text ("\x1b[" ^ big ^ "L"));
  equal ~msg:"SU" string "    \n    " (text ("\x1b[" ^ big ^ "S"))

(* A parameter with sub-parameters makes a sequence other than SGR malformed
   (ECMA-48 §5.4.2): it has no effect, and the unit in progress goes on. *)
let sub_parameters () =
  let vte s = screen_of ~rows:2 ~cols:6 [ s ] in
  let pos s = Vte.cursor_pos (vte s) in
  equal ~msg:"CUP 2:3" (pair int int) (0, 1) (pos "a\x1b[2:3H");
  equal ~msg:"CUF 1:2" (pair int int) (0, 1) (pos "a\x1b[1:2C");
  equal ~msg:"CUP 2;3:1" (pair int int) (0, 1) (pos "a\x1b[2;3:1H");
  equal ~msg:"ED 2:0" string "a     \n      " (Vte.to_string (vte "a\x1b[2:0J"));
  equal ~msg:"REP 2:1" string "a     \n      "
    (Vte.to_string (vte "a\x1b[2:1b"));
  is_true ~msg:"DECRST 7:1" (Vte.modes (vte "\x1b[?7:1l")).auto_wrap;
  equal ~msg:"the unit goes on" string "e\u{301}"
    (Grid.get_text (Vte.grid (vte "e\x1b[1:2C\u{301}")) 0);
  let g = Vte.grid (vte "\x1b[38:5:1ma") in
  is_true ~msg:"SGR takes sub-parameters"
    (Ansi.Color.equal (Grid.get_fg g 0) (Ansi.Color.of_palette_index 1))

(* The control functions of the parser's probes, at the VTE. *)
let parser_probes () =
  let vte s = screen_of ~rows:2 ~cols:8 [ s ] in
  let text s = Vte.to_string (vte s) in
  equal ~msg:"a UTF-8 C1 ends a CSI" string "2Cb     \n        "
    (text "\x1b[1\xc2\x9b2Cb");
  equal ~msg:"a UTF-8 C1 in an OSC is dropped" string "t2Cb"
    (Vte.title (vte "\x1b]0;t\xc2\x9b2Cb\x07"));
  equal ~msg:"ESC ends ESC %" string " b      \n        " (text "\x1b%\x1b[Cb");
  equal ~msg:"ESC ends a designation" string " b      \n        "
    (text "\x1b(\x1b[Cb");
  equal ~msg:"ESC ESC" string " b      \n        " (text "\x1b\x1b[Cb");
  equal ~msg:"a non-ASCII character ends an escape" string "b       \n        "
    (text "\x1b\xc3\xa9b");
  equal ~msg:"C0 inside an escape sequence" string "EEEEEEEE\nEEEEEEEE"
    (text "\x1b#\n8");
  equal ~msg:"an APC cut short does nothing" string " b      \n        "
    (text "\x1b_abc\x1b[Cb");
  let pos s = Vte.cursor_pos (vte ("\x1b[2;5H" ^ s)) in
  equal ~msg:"CNL 0" (pair int int) (1, 0) (pos "\x1b[0E");
  equal ~msg:"CPL 0" (pair int int) (0, 0) (pos "\x1b[0F");
  equal ~msg:"VPR 0" (pair int int) (1, 4) (pos "\x1b[0e");
  equal ~msg:"HPR 0" (pair int int) (1, 5) (pos "\x1b[0a");
  equal ~msg:"CBT 0" (pair int int) (1, 0) (pos "\x1b[0Z");
  equal ~msg:"VPR saturated" (pair int int) (1, 4)
    (pos "\x1b[4611686018427387904e")

(* DECALN fills the screen with E and, as xterm, resets origin mode, the
   rendition, the margins and the Last Column Flag. *)
let decaln_resets () =
  let vte =
    screen_of ~rows:3 ~cols:4 [ "\x1b[2;3r\x1b[?6h\x1b[1;41mabcd\x1b#8X" ]
  in
  equal ~msg:"homes to the screen's first row" string "XEEE\nEEEE\nEEEE"
    (Vte.to_string vte);
  is_false ~msg:"origin mode off" (Vte.modes vte).origin;
  equal ~msg:"rendition reset" int 0 (Grid.get_attrs (Vte.grid vte) 0);
  equal ~msg:"margins reset" (pair int int) (2, 0)
    (Vte.cursor_pos (screen_of ~rows:3 ~cols:4 [ "\x1b[1;2r\x1b#8\x1b[3H\n" ]))

(* DECSTR resets both DECSC slots. *)
let decstr_slots () =
  equal ~msg:"both slots"
    (pair (pair int int) (pair int int))
    ((0, 0), (0, 0))
    (let vte =
       screen_of ~rows:3 [ "\x1b[2;2H\x1b7\x1b[?47h\x1b[3;3H\x1b7\x1b[!p\x1b8" ]
     in
     let alt = Vte.cursor_pos vte in
     feed vte "\x1b[?47l\x1b8";
     (alt, Vte.cursor_pos vte))

let more_character_sets () =
  let line s = Vte.to_string (screen_of [ s ]) in
  equal ~msg:"DEC graphics 0x5F is a blank" string
    "\xe2\x94\x80 \xe2\x94\x80   " (line "\x1b(0q_q");
  equal ~msg:"SS2" string "\xe2\x94\x80q    " (line "\x1b*0\x1bNqq");
  equal ~msg:"SS3" string "\xe2\x94\x80q    " (line "\x1b+0\x1bOqq");
  equal ~msg:"SS2 consumed by a non-ASCII character" string "\xc3\xa9q    "
    (line "\x1b*0\x1bN\xc3\xa9q");
  let national s = line ("\x1b[?42h" ^ s) in
  equal ~msg:"French Canadian 9" string "\u{00e0}     " (national "\x1b(9@");
  equal ~msg:"French f" string "\u{00e0}     " (national "\x1b(f@");
  equal ~msg:"Norwegian/Danish `" string "\u{00c6}     " (national "\x1b(`[");
  equal ~msg:"a set without a table maps nothing" string "q     "
    (line "\x1b(0\x1b(<q");
  equal ~msg:"a 96-character set maps nothing" string "q     "
    (line "\x1b)0\x1b-A\x0eq")

(* Resizing keeps the text around the cursor and moves rows to and from the
   scrollback. *)
let resize_keeps_text () =
  let vte = Vte.create ~scrollback:10 ~rows:5 ~cols:4 () in
  feed vte "1\r\n2\r\n3";
  resize vte ~rows:3 ~cols:4;
  equal ~msg:"rows below the cursor go first" string "1   \n2   \n3   "
    (Vte.to_string vte);
  resize vte ~rows:2 ~cols:4;
  equal ~msg:"then rows off the top"
    (pair string (list string))
    ("2   \n3   ", [ "1" ])
    (Vte.to_string vte, scrollback_lines vte);
  equal ~msg:"the cursor moves with its row" (pair int int) (1, 1)
    (Vte.cursor_pos vte);
  resize vte ~rows:4 ~cols:4;
  equal ~msg:"growing takes them back"
    (pair string (list string))
    ("1   \n2   \n3   \n    ", [])
    (Vte.to_string vte, scrollback_lines vte);
  let vte = Vte.create ~scrollback:10 ~rows:3 ~cols:4 () in
  feed vte "abcdef\x1b[2;2H\x1b7\x1b[3H";
  resize vte ~rows:2 ~cols:4;
  is_true ~msg:"marks go into the scrollback" (Vte.wrapped vte (-1));
  feed vte "\x1b8X";
  equal ~msg:"DECSC slots move with their rows" string "eX  \n    "
    (Vte.to_string vte);
  resize vte ~rows:3 ~cols:4;
  is_true ~msg:"and come back" (Vte.wrapped vte 0);
  let vte = screen_of ~rows:3 ~cols:4 [ "abcd" ] in
  resize vte ~rows:3 ~cols:6;
  feed vte "X";
  equal ~msg:"the Last Column Flag goes" string "abcX  \n      \n      "
    (Vte.to_string vte);
  let vte = screen_of ~rows:4 ~cols:4 [ "\x1b[2;3r\x1b[?1049hA\r\nB" ] in
  resize vte ~rows:2 ~cols:4;
  equal ~msg:"the alternate screen keeps its text" string "A   \nB   "
    (Vte.to_string vte);
  equal ~msg:"and has no scrollback" int 0 (Vte.scrollback_size vte);
  feed vte "\x1b[H\n\n";
  equal ~msg:"the margins are the screen" string "B   \n    "
    (Vte.to_string vte);
  let vte = screen_of ~rows:1 ~cols:2 [ "\u{1F31C}\u{FE0E}" ] in
  resize vte ~rows:16 ~cols:49;
  equal ~msg:"a grapheme of several characters keeps its text" string
    "\u{1F31C}\u{FE0E}"
    (Grid.get_text (Vte.grid vte) 0);
  let vte = screen_of ~rows:1 ~cols:4 [ "a\xe4\xb8\xad" ] in
  resize vte ~rows:1 ~cols:2;
  equal ~msg:"a wide character cut by the edge goes" string "a "
    (Vte.to_string vte);
  let vte = screen_of ~rows:2 ~cols:4 [ "e" ] in
  resize vte ~rows:2 ~cols:5;
  feed vte "\xcc\x81";
  equal ~msg:"the unit in progress is ended" string "e\xcc\x81"
    (Grid.get_text (Vte.grid vte) 0)

(* A line comes back from the scrollback with the cells it left: each grapheme
   whole, at its column and width, never segmented again. Two graphemes UAX
   #29 would join, apart because a control function came between them, stay
   apart; a unit clamped to the line keeps its width. *)
let scrollback_keeps_cells () =
  let row g y =
    List.init (Grid.width g) (fun x ->
        let i = (y * Grid.width g) + x in
        Printf.sprintf "%S/%d/%b/%s" (Grid.get_text g i) (Grid.cell_width g i)
          (Grid.is_continuation g i)
          (Format.asprintf "%a" Ansi.Style.pp (Grid.get_style g i)))
  in
  let check ?(cols = 8) width_method s =
    let vte = Vte.create ~width_method ~scrollback:8 ~rows:2 ~cols () in
    feed vte s;
    let cells = row (Vte.grid vte) 0 in
    let text =
      String.concat ""
        (List.init cols (fun x -> Grid.get_text (Vte.grid vte) x))
    in
    feed vte "\x1b[m\r\n\r\n";
    equal ~msg:"scrollback_lines" (list string)
      [ String.trim text ]
      (scrollback_lines vte);
    let dst = Grid.create ~width:cols ~height:2 () in
    Vte.blit_view vte ~offset:1 dst;
    equal ~msg:"blit_view" (list string) cells (row dst 0);
    resize vte ~rows:3 ~cols;
    equal ~msg:"resize grow" (list string) cells (row (Vte.grid vte) 0)
  in
  List.iter
    (fun m ->
      check m "\u{1100}\x07\u{1100}xy";
      check m "\x1b[31m\u{1100}\x1b[32m\u{1100}\x1b[44mxy";
      check m "\u{1F1FA}\x07\u{1F1F8}xy";
      check m "a\u{200D}\x07\u{1F600}";
      check ~cols:1 m "\u{4E2D}";
      check ~cols:1 m "\x1b[?7l\u{1F600}")
    [ `Unicode; `Unicode_wide; `Wcwidth ]

(* A width the grid cannot hold is refused before anything changes. *)
let too_wide () =
  let cols = Grid.max_width + 1 in
  raises_match (Exn.invalid_arg ~substring:"Vte.create") (fun () ->
      Vte.create ~rows:1 ~cols ());
  let vte = Vte.create ~scrollback:10 ~rows:3 ~cols:4 () in
  feed vte "1\r\n2\r\n3";
  raises_match (Exn.invalid_arg ~substring:"Vte.resize") (fun () ->
      resize vte ~rows:1 ~cols);
  equal ~msg:"resize changed nothing"
    (pair string (list string))
    ("1   \n2   \n3   ", [])
    (Vte.to_string vte, scrollback_lines vte);
  resize vte ~rows:1 ~cols:Grid.max_width;
  equal ~msg:"the widest grid" int Grid.max_width (Vte.cols vte)

(* CAN ends a sequence or string and shows nothing; SUB shows the error
   character, also in the ground state. *)
let can_and_sub () =
  let line s =
    List.hd
      (String.split_on_char '\n'
         (Vte.to_string (screen_of ~rows:2 ~cols:8 [ s ])))
  in
  equal ~msg:"CAN shows nothing" string "a       " (line "\x1b[3\x18a");
  equal ~msg:"CAN ends an OSC" string "abc     " (line "\x1b]0;t\x18abc");
  equal ~msg:"SUB shows the error character" string "\u{2E2E}a\u{2E2E}     "
    (line "\x1b[3\x1aa\x1a");
  equal ~msg:"SUB ends a DCS" string "\u{2E2E}abc    " (line "\x1bPq\x1aabc");
  equal ~msg:"SUB ends an OSC" string "\u{2E2E}       " (line "\x1b]0;t\x1a")

(* National replacement sets need national mode (DECNRCM). *)
let national_mode () =
  let line s = Vte.to_string (screen_of [ s ]) in
  equal ~msg:"outside national mode" string "`     " (line "\x1b(H`");
  equal ~msg:"in national mode" string "\u{00e9}     " (line "\x1b[?42h\x1b(H`");
  equal ~msg:"a designation waits for the mode" string "`\u{00e9}    "
    (line "\x1b(H`\x1b[?42h`");
  equal ~msg:"UK needs no national mode" string "\u{00a3}     " (line "\x1b(A#");
  equal ~msg:"DEC graphics need none" string "\u{2500}     " (line "\x1b(0q");
  is_false ~msg:"DECSTR resets it"
    (Vte.modes (screen_of [ "\x1b[?42h\x1b[!p" ])).national;
  is_false ~msg:"RIS resets it"
    (Vte.modes (screen_of [ "\x1b[?42h\x1bc" ])).national

(** {1 Test Suite} *)

(* Regression: ED 3 emptied the scrollback and marked nothing changed. *)
let erase_scrollback_marks_a_change () =
  let vte = Vte.create ~rows:2 ~cols:5 () in
  feed vte "a\r\nb\r\nc\r\nd";
  equal ~msg:"lines in the scrollback" int 2 (Vte.scrollback_size vte);
  Vte.clear_dirty vte;
  feed vte "\027[3J";
  equal ~msg:"scrollback emptied" int 0 (Vte.scrollback_size vte);
  is_true ~msg:"marked changed" (Vte.is_dirty vte);
  equal ~msg:"no row of the screen changed" (list int) [] (Vte.dirty_rows vte);
  Vte.clear_dirty vte;
  feed vte "\027[3J";
  is_false ~msg:"erasing an empty scrollback changes nothing" (Vte.is_dirty vte)

(* A size report that repeats the size changes nothing, as resize states. *)
let resize_to_the_same_size () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  feed vte "\027[2;4r\027[1;10Hx";
  Vte.clear_dirty vte;
  resize vte ~rows:5 ~cols:10;
  is_false ~msg:"no change marked" (Vte.is_dirty vte);
  is_true ~msg:"Last Column Flag kept" (Vte.last_column vte);
  feed vte "\027[4;1H\n";
  equal ~msg:"margins kept: a line feed at the bottom margin scrolls"
    (pair int int) (3, 0) (Vte.cursor_pos vte)

let tests =
  [
    test "resize to the same size" resize_to_the_same_size;
    test "ED 3 marks a change" erase_scrollback_marks_a_change;
    test "hyperlinks survive styles and scrollback"
      hyperlinks_survive_styles_and_scrollback;
    test "scrollback preserves decorated spaces"
      scrollback_preserves_decorated_spaces;
    test "clearing scrollback releases text" clearing_scrollback_releases_text;
    test "long runs match fragmented runs" long_runs_match_fragmented_runs;
    test "wcwidth combining at margin and insert"
      wcwidth_combining_at_margin_and_insert;
    test "wcwidth insert measures complete cluster"
      wcwidth_insert_measures_complete_cluster;
    test "margin overwrite with wrap off" margin_overwrite_with_wrap_off;
    test "create with defaults" create_with_defaults;
    test "create with no scrollback" create_with_no_scrollback;
    test "create with custom scrollback" create_with_custom_scrollback;
    test "feed simple text" feed_simple_text;
    test "feed multiline text" feed_multiline_text;
    test "feed carriage return" feed_carriage_return;
    test "feed tab" feed_tab;
    test "tab stops" tab_stops;
    test "feed backspace" feed_backspace;
    test "cursor up" cursor_up;
    test "cursor down" cursor_down;
    test "cursor forward" cursor_forward;
    test "cursor backward" cursor_backward;
    test "cursor position" cursor_position;
    test "origin mode positions" origin_mode_positions;
    test "cursor horizontal absolute" cursor_horizontal_absolute;
    test "ECMA-48 position forms" ecma48_position_forms;
    test "cursor next line" cursor_next_line;
    test "cursor previous line" cursor_previous_line;
    test "vertical moves stop at margins" vertical_moves_stop_at_margins;
    test "erase display to end" erase_display_to_end;
    test "erase display to beginning" erase_display_to_beginning;
    test "erase display all" erase_display_all;
    test "erase line to end" erase_line_to_end;
    test "erase line to beginning" erase_line_to_beginning;
    test "erase line all" erase_line_all;
    test "erase characters" erase_characters;
    test "default background spaces overwrite text"
      default_background_spaces_overwrite_text;
    test "insert lines" insert_lines;
    test "delete lines" delete_lines;
    test "insert and delete lines in region" insert_delete_lines_in_region;
    test "insert characters" insert_characters;
    test "delete characters" delete_characters;
    test "repeat preceding character" repeat_preceding_character;
    test "backspace moves without erasing" backspace_moves_without_erasing;
    test "line feed preserves column" line_feed_preserves_column;
    test "line feed scroll region" line_feed_scroll_region;
    test "wrap below scroll region" wrap_below_scroll_region;
    test "pending wrap cleared by controls" pending_wrap_cleared_by_controls;
    test "delete characters wide grapheme" delete_characters_wide;
    test "insert characters wide boundary" insert_characters_wide_boundary;
    test "edit on wide right half" edit_on_wide_right_half;
    test "erase cutting wide takes background"
      erase_cutting_wide_takes_background;
    test "DCH ending on wide left half" dch_ending_on_wide_left_half;
    test "shift wide over wide" shift_wide_over_wide;
    test "SGR bold" sgr_bold;
    test "SGR italic" sgr_italic;
    test "SGR underline" sgr_underline;
    test "SGR foreground color" sgr_foreground_color;
    test "SGR background color" sgr_background_color;
    test "erase keeps background index" erase_keeps_background_index;
    test "terminal defaults replace cells" terminal_defaults_replace_cells;
    test "SGR 24 ends a double underline" sgr_24_ends_double_underline;
    test "SGR reset" sgr_reset;
    test "cursor hide/show" cursor_hide_show;
    test "set cursor visible" set_cursor_visible_programmatic;
    test "cursor save/restore" cursor_save_restore;
    test "cursor save/restore style" cursor_save_restore_style;
    test "saved cursor per screen" saved_cursor_per_screen;
    test "alternate screen restores style" alternate_screen_restores_style;
    test "alternate screen keeps cursor" alternate_screen_keeps_cursor;
    test "scroll up basic" scroll_up_basic;
    test "scroll down basic" scroll_down_basic;
    test "CSI scroll sequences" csi_scroll_sequences;
    test "scroll on bottom line" scroll_on_bottom_line;
    test "alignment and column edits" alignment_and_column_edits;
    test "reverse index moves cursor up" reverse_index_moves_cursor_up;
    test "reverse index scrolls region down" reverse_index_scrolls_region_down;
    test "index and next line" index_and_next_line;
    test "scrollback accumulates" scrollback_accumulates;
    test "one-row scroll" one_row_scroll;
    test "erase scrollback sequence" erase_scrollback_sequence;
    test "scrollback ring buffer" scrollback_ring_buffer;
    test "scrollback disabled" scrollback_disabled;
    test "rows and the scrollback" rows_and_scrollback;
    test "blit_view order" blit_view_order;
    test "blit_view wrapped ring" blit_view_wrapped_ring;
    test "blit_view clamps" blit_view_clamps;
    test "auto wrap mode" auto_wrap_mode;
    test "reverse wrap" reverse_wrap;
    test "insert mode" insert_mode;
    test "insert mode wrap targets new row" insert_mode_wrap_targets_new_row;
    test "insert mode clips wide grapheme at margin"
      insert_mode_clips_wide_grapheme_at_margin;
    test "cursor key mode" cursor_key_mode;
    test "bracketed paste mode" bracketed_paste_mode;
    test "combined mode sequence" combined_mode_sequence;
    test "OSC set title" osc_set_title;
    test "query replies" query_replies;
    test "palette set, query and reset" palette_set_query_reset;
    test "a rewritten reply reaches the program"
      rewritten_reply_reaches_the_program;
    test "mode and colour queries" mode_and_colour_queries;
    test "line feed/new line mode" line_feed_new_line_mode;
    test "cursor style" cursor_style;
    test "dynamic color walk" dynamic_color_walk;
    test "OSC acts only when terminated" osc_acts_only_when_terminated;
    test "zero counts mean one" zero_counts_mean_one;
    test "replies by parameter value" replies_by_parameter_value;
    test "UTF-8 C1 is not a control" utf8_c1_is_not_a_control;
    test "two-byte designators" two_byte_designators;
    test "resize preserves content" resize_preserves_content;
    test "resize clamps cursor" resize_clamps_cursor;
    test "reset clears state" reset_clears_state;
    test "RIS and DECSTR" ris_and_decstr;
    test "selective erase" selective_erase;
    test "dirty flag on text" dirty_flag_on_text;
    test "dirty flag on cursor move" dirty_flag_on_cursor_move;
    test "dirty flag on style change" dirty_flag_on_style_change;
    test "dirty row tracking" dirty_row_tracking;
    test "dirty cursor tracking" dirty_cursor_tracking;
    test "unicode basic" unicode_basic;
    test "unicode with escapes" unicode_with_escapes;
    test "unicode multiline" unicode_multiline;
    test "unicode wide char cursor" unicode_wide_char_cursor;
    test "unicode wide char wraps at margin" unicode_wide_char_wraps_at_margin;
    test "unicode wide char wider than line" unicode_wide_char_wider_than_line;
    test "unicode combining mark across writes"
      unicode_combining_mark_across_writes;
    test "unicode zero-width joins cell" unicode_zero_width_joins_cell;
    test "unicode prepend across writes" unicode_prepend_across_writes;
    test "unicode mark after dropped wide" unicode_mark_after_dropped_wide;
    test "unicode malformed utf8" unicode_malformed_utf8;
    test "character sets" character_sets;
    test "last column flag" last_column_flag;
    test "VPR in origin mode" vpr_in_origin_mode;
    test "alternate screen modes" alternate_screen_modes;
    test "REP repeats exactly" repeat_exactly;
    test "one blank" one_blank;
    test "zero-width units" zero_width_units;
    test "split counterexamples" split_counterexamples;
    test "placement rules" placement_rules;
    test "wrap marks" wrap_marks;
    test "huge parameters" huge_parameters;
    test "parser probes" parser_probes;
    test "sub-parameters outside SGR" sub_parameters;
    test "DECALN resets" decaln_resets;
    test "DECSTR resets both slots" decstr_slots;
    test "more character sets" more_character_sets;
    test "resize keeps text" resize_keeps_text;
    test "scrollback keeps cells" scrollback_keeps_cells;
    test "too wide" too_wide;
    test "CAN and SUB" can_and_sub;
    test "national mode" national_mode;
  ]

(* The user's side, identity and requests --------------------------------- *)

module K = Matrix_input.Keystroke

let pp_keyboard ppf (e : K.encoding) =
  Format.fprintf ppf "ckm %b kp %b lnm %b mok %d kitty %d" e.cursor_keys
    e.keypad e.new_line e.modify_other_keys e.kitty

let keyboard_is msg vte expected =
  equal ~msg string expected
    (Format.asprintf "%a" pp_keyboard (Vte.keyboard vte))

(* The modes a program sets for keys, and the kitty protocol's stacks, one
   for each screen. *)
let keyboard_state () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  keyboard_is "power-up" vte "ckm false kp false lnm false mok 0 kitty 0";
  feed vte "\x1b[?1h\x1b=\x1b[20h\x1b[>4;1m";
  keyboard_is "DECCKM, DECKPAM, LNM, modifyOtherKeys" vte
    "ckm true kp true lnm true mok 1 kitty 0";
  feed vte "\x1b>\x1b[>4m";
  keyboard_is "DECKPNM and XTMODKEYS without a value" vte
    "ckm true kp false lnm true mok 0 kitty 0";
  feed vte "\x1b[>1u\x1b[>5u";
  equal ~msg:"the current flags" string "\x1b[?5u" (answers vte "\x1b[?u");
  feed vte "\x1b[=2;2u";
  equal ~msg:"flags added" string "\x1b[?7u" (answers vte "\x1b[?u");
  feed vte "\x1b[=1;3u";
  equal ~msg:"flags taken away" string "\x1b[?6u" (answers vte "\x1b[?u");
  feed vte "\x1b[<u";
  equal ~msg:"popped" string "\x1b[?1u" (answers vte "\x1b[?u");
  feed vte "\x1b[?1049h";
  equal ~msg:"the alternate screen's stack" string "\x1b[?0u"
    (answers vte "\x1b[?u");
  feed vte "\x1b[>31u\x1b[?1049l";
  equal ~msg:"the primary screen's again" string "\x1b[?1u"
    (answers vte "\x1b[?u");
  feed vte "\x1b[<5u";
  equal ~msg:"popping past the stack empties it" string "\x1b[?0u"
    (answers vte "\x1b[?u");
  feed vte "\x1b[>1u\x1bc";
  keyboard_is "RIS" vte "ckm false kp false lnm false mok 0 kitty 0"

let mouse_state () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  let mouse () =
    match Vte.mouse vte with
    | None -> "off"
    | Some { tracking; format } -> (
        (match tracking with
          | `X10 -> "x10"
          | `Normal -> "normal"
          | `Button -> "button"
          | `Any -> "any")
        ^ " "
        ^
        match format with
        | `X10 -> "bytes"
        | `Sgr -> "sgr"
        | `Urxvt -> "urxvt")
  in
  equal ~msg:"power-up" string "off" (mouse ());
  feed vte "\x1b[?1000h\x1b[?1002h\x1b[?1006h";
  equal ~msg:"the last tracking mode set" string "button sgr" (mouse ());
  equal ~msg:"DECRQM reports the one in use" string
    "\x1b[?1000;2$y\x1b[?1002;1$y\x1b[?1006;1$y"
    (answers vte "\x1b[?1000$p\x1b[?1002$p\x1b[?1006$p");
  feed vte "\x1b[?1015h";
  equal ~msg:"URXVT replaces SGR" string "button urxvt" (mouse ());
  feed vte "\x1b[?1006l";
  equal ~msg:"resetting a format not in use" string "button urxvt" (mouse ());
  feed vte "\x1b[?1000l";
  equal ~msg:"resetting any tracking mode" string "off" (mouse ())

(* A terminal without an extension behaves as one that never had it. *)
let extensions () =
  let queries =
    "\x1b[?2026$p\x1b[?1004$p\x1b[?2031$p\x1b[?2048$p\x1b[?2027$p"
  in
  let full = Vte.create ~rows:2 ~cols:10 () in
  feed full "\x1b[?2026h\x1b[?1004h";
  equal ~msg:"every extension" string
    "\x1b[?2026;1$y\x1b[?1004;1$y\x1b[?2031;2$y\x1b[?2048;2$y\x1b[?2027;3$y"
    (answers full queries);
  equal ~msg:"Alt is ESC" string "\x1b[?1036;3$y" (answers full "\x1b[?1036$p");
  let bare = Vte.create ~extensions:[] ~rows:2 ~cols:10 () in
  feed bare "\x1b[?2026h\x1b[?1004h\x1b[>1u\x1b[>4;2m";
  equal ~msg:"no extension" string
    "\x1b[?2026;0$y\x1b[?1004;0$y\x1b[?2031;0$y\x1b[?2048;0$y\x1b[?2027;0$y"
    (answers bare queries);
  is_false ~msg:"its modes stay off" (Vte.modes bare).synchronized_output;
  equal ~msg:"no kitty flags, no answer" string "" (answers bare "\x1b[?u");
  keyboard_is "no modifyOtherKeys" bare
    "ckm false kp false lnm false mok 0 kitty 0";
  equal ~msg:"no color scheme answer" string "" (answers bare "\x1b[?996n");
  let wcwidth = Vte.create ~width_method:`Wcwidth ~rows:2 ~cols:10 () in
  equal ~msg:"2027 reset for good under wcwidth" string "\x1b[?2027;4$y"
    (answers wcwidth "\x1b[?2027$p")

let identity () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  equal ~msg:"a VT100, no XTVERSION" string "\x1b[?1;2c\x1b[>0;0;0c"
    (answers vte "\x1b[c\x1b[>q\x1b[>c");
  let identity =
    {
      Vte.attributes = [ 62; 22 ];
      secondary = [ 1; 4000; 0 ];
      version = Some "x(1)";
    }
  in
  let vte = Vte.create ~identity ~rows:2 ~cols:10 () in
  equal ~msg:"its own" string "\x1b[?62;22c\x1bP>|x(1)\x1b\\\x1b[>1;4000;0c"
    (answers vte "\x1b[c\x1b[>0q\x1b[>c")

(* Mode 2048 reports each resize while set; mode 2031 each change of scheme,
   which [CSI ? 996 n] asks for. *)
let reports () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  let replies =
    Testable.make ~equal:( = )
      ~pp:
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           Matrix_input.Reply.pp)
  in
  feed vte "\x1b[?2048h";
  equal ~msg:"a resize" replies
    [ Resize { rows = 3; cols = 12; height = 0; width = 0 } ]
    (Vte.resize vte ~rows:3 ~cols:12);
  equal ~msg:"the same size" replies [] (Vte.resize vte ~rows:3 ~cols:12);
  equal ~msg:"the scheme" string "\x1b[?997;1n" (answers vte "\x1b[?996n");
  equal ~msg:"a change, mode 2031 off" replies []
    (Vte.set_color_scheme vte `Light);
  feed vte "\x1b[?2031h";
  equal ~msg:"a change" replies [ Color_scheme `Dark ]
    (Vte.set_color_scheme vte `Dark);
  equal ~msg:"no change" replies [] (Vte.set_color_scheme vte `Dark)

let requests () =
  let vte = Vte.create ~rows:2 ~cols:10 () in
  let show () =
    List.map
      (function
        | Vte.Bell -> "bell"
        | Notification { title; body } ->
            Printf.sprintf "notify %S %S" title body
        | Clipboard { selection; data } ->
            Printf.sprintf "clipboard %S %S" selection data)
      (Vte.requests vte)
  in
  feed vte
    "\x07\x1b]9;done: 3;4\x07\x1b]777;notify;mentat;done; \
     ok\x1b\\\x1b]52;c;aGk=\x07\x1b]52;c;?\x07\x1b]0;t\x07";
  equal ~msg:"in order" (list string)
    [
      "bell";
      "notify \"\" \"done: 3;4\"";
      "notify \"mentat\" \"done; ok\"";
      "clipboard \"c\" \"hi\"";
    ]
    (show ());
  equal ~msg:"a BEL that ends an OSC rings nothing" string "t" (Vte.title vte);
  Vte.clear_requests vte;
  equal ~msg:"cleared" (list string) [] (show ())

let user_tests =
  [
    test "the keyboard's state" keyboard_state;
    test "the mouse's state" mouse_state;
    test "extensions" extensions;
    test "identity" identity;
    test "resize and color scheme reports" reports;
    test "requests to the host" requests;
  ]

(* Kitty graphics: the query, images held without their pixels, Unicode
   placeholders that read U+FFFC while their image is held, and the cell's
   size. *)
let kitty_graphics () =
  let t = Vte.create ~cell_size:(9, 18) ~rows:2 ~cols:6 () in
  equal ~msg:"the query" string "\x1b_Gi=31337;OK\x1b\\"
    (answers t "\x1b_Gi=31337,s=1,v=1,a=q,t=d,f=24;AAAA\x1b\\");
  equal ~msg:"the cell's size, and the text area's" string
    "\x1b[6;18;9t\x1b[4;36;54t"
    (answers t "\x1b[16t\x1b[14t");
  let placeholder = "\x1b[38;2;0;0;7m\u{10EEEE}\u{0305}\u{0305}\x1b[39m" in
  feed t placeholder;
  equal ~msg:"an image not held" string "\u{FFFD}" (Vte.text t 0);
  (* Two chunks of a 1x1 RGBA image, four bytes in base64. *)
  feed t "\x1b_Ga=T,U=1,f=32,s=1,v=1,i=7,c=1,r=1,q=2,m=1;AAAA\x1b\\";
  feed t "\x1b_Gm=0;AAAA\x1b\\";
  feed t ("\r" ^ placeholder);
  equal ~msg:"an image held" string "\u{FFFC}" (Vte.text t 0);
  feed t "\x1b_Ga=d,d=I,i=7,q=2\x1b\\";
  equal ~msg:"deleted" string "\u{FFFD}" (Vte.text t 0);
  feed t "\x1b_Ga=T,U=1,f=32,s=2,v=1,i=7,c=1,r=1,q=2;AAAA\x1b\\";
  feed t ("\r" ^ placeholder);
  equal ~msg:"a payload too short holds nothing" string "\u{FFFD}"
    (Vte.text t 0);
  let bare = Vte.create ~extensions:[] ~rows:2 ~cols:6 () in
  equal ~msg:"no extension, no answer" string "\x1b[?1;2c"
    (answers bare "\x1b_Gi=31337,s=1,v=1,a=q,t=d,f=24;AAAA\x1b\\\x1b[c\x1b[16t")

(* Updates ---------------------------------------------------------------- *)

let bsu = "\x1b[?2026h"
let esu = "\x1b[?2026l"

(* Output with every way to open or end an update, and bytes that look like
   one and do not: replies inside and outside updates, nested and stray
   brackets, a DECSET and a DECRST of several modes, the text of a string,
   a sequence cancelled by CAN, an ESC that ends an OSC, RIS, DECSTR, and
   UTF-8 cut by a bracket. *)
let update_stream =
  String.concat ""
    [
      "plain\x1b[6n";
      bsu;
      "\x1b[Hframe-1\x1b[?2026$p";
      esu;
      "\x1b[?2026$p";
      bsu;
      "\x1b[Hframe-2";
      bsu;
      esu;
      esu;
      "\x1b[?25;2026h\x1b[Hframe-3";
      "\x1b]0;[?2026l\x07\x1bPq?2026l\x1b\\";
      "\x1b[?2026;25l";
      bsu;
      "\x1b[Hframe-4\x1b[?2026\x18l";
      "\x1b]0;t\x1b[?2026l";
      bsu;
      "\x1b[Hframe-5\x1bc";
      bsu;
      "\x1b[Hframe-6\x1b[!p\x1b[6n";
      esu;
      "\xc3\xa9";
      bsu;
      "\xe2\x82";
      esu;
      "tail";
    ]

(* What a reader looping on [Vte.advance] sees of [chunks]: each update
   opened or ended, with the offset in the output just after it and, at an
   end, the first row; the replies of the bytes between two such points;
   and the screen at the end. *)
let updates ?extensions chunks =
  let t = Vte.create ?extensions ~rows:2 ~cols:20 () in
  let log = Buffer.create 256 and replies = Buffer.create 64 in
  let take_replies () =
    if Buffer.length replies > 0 then (
      Printf.bprintf log "replies %S\n" (Buffer.contents replies);
      Buffer.clear replies)
  in
  let pos = ref 0 in
  List.iter
    (fun chunk ->
      let b = Bytes.of_string chunk in
      let rec go off =
        if off < Bytes.length b then (
          let was = (Vte.modes t).synchronized_output in
          let n = Vte.advance t b off (Bytes.length b - off) in
          Buffer.add_string replies (Matrix_input.Reply.encode (Vte.replies t));
          let now = (Vte.modes t).synchronized_output in
          if was <> now then (
            take_replies ();
            if now then Printf.bprintf log "open at %d\n" (!pos + off + n)
            else
              Printf.bprintf log "end at %d: %S\n"
                (!pos + off + n)
                (Vte.text t 0))
          else if off + n < Bytes.length b then
            Printf.bprintf log "a stop with no change at %d\n" (!pos + off + n);
          go (off + n))
      in
      go 0;
      pos := !pos + Bytes.length b)
    chunks;
  take_replies ();
  Printf.bprintf log "screen %S" (Vte.to_string t);
  Buffer.contents log

let updates_stops () =
  equal string
    (String.concat "\n"
       [
         {|replies "\027[1;6R"|};
         "open at 17";
         {|replies "\027[?2026;1$y"|};
         {|end at 44: "frame-1"|};
         {|replies "\027[?2026;2$y"|};
         "open at 61";
         {|end at 87: "frame-2"|};
         "open at 106";
         {|end at 150: "frame-3"|};
         "open at 158";
         {|end at 190: "frame-4l"|};
         "open at 198";
         {|end at 210: ""|};
         "open at 218";
         {|replies "\027[1;8R"|};
         {|end at 244: "frame-6"|};
         "open at 254";
         {|end at 264: "frame-6\195\169\239\191\189"|};
         {|screen "frame-6\195\169\239\191\189tail       \n                    "|};
       ])
    (updates [ update_stream ])

(* The reader sees the same at every split of the output in two, and when
   it comes a byte at a time. *)
let updates_split () =
  let whole = updates [ update_stream ] in
  let n = String.length update_stream in
  for i = 1 to n - 1 do
    equal
      ~msg:(Printf.sprintf "split at %d" i)
      string whole
      (updates
         [ String.sub update_stream 0 i; String.sub update_stream i (n - i) ])
  done;
  equal ~msg:"a byte at a time" string whole
    (updates (List.init n (fun i -> String.make 1 update_stream.[i])))

(* [feed] is [advance] until every byte is applied, then [replies]. *)
let updates_feed () =
  let t = Vte.create ~rows:2 ~cols:20 () in
  let replies = Matrix_input.Reply.encode (Vte.feed_string t update_stream) in
  equal ~msg:"the replies" string
    "\027[1;6R\027[?2026;1$y\027[?2026;2$y\027[1;8R" replies;
  equal ~msg:"the screen" string
    "frame-6\195\169\239\191\189tail       \n                    "
    (Vte.to_string t)

let updates_without_the_extension () =
  let extensions =
    List.filter (( <> ) Vte.Synchronized_output) Vte.extensions
  in
  equal string
    (String.concat "\n"
       [
         {|replies "\027[1;6R\027[?2026;0$y\027[?2026;0$y\027[1;8R"|};
         {|screen "frame-6\195\169\239\191\189tail       \n                    "|};
       ])
    (updates ~extensions [ update_stream ])

(* An update's end and the next one's start in one piece of output: the
   reader sees the finished frame between them. *)
let updates_end_then_open () =
  let t = Vte.create ~rows:1 ~cols:20 () in
  let b =
    Bytes.of_string (bsu ^ "frame-1" ^ esu ^ bsu ^ "\x1b[Hframe-2" ^ esu)
  in
  let n = Vte.advance t b 0 (Bytes.length b) in
  equal ~msg:"the first opens" int (String.length bsu) n;
  let n' = Vte.advance t b n (Bytes.length b - n) in
  equal ~msg:"the frame" string "frame-1" (Vte.text t 0);
  is_false ~msg:"no update open" (Vte.modes t).synchronized_output;
  equal ~msg:"its end" int (String.length (bsu ^ "frame-1" ^ esu)) (n + n')

let update_tests =
  [
    test "where updates open and end" updates_stops;
    test "split anywhere" updates_split;
    test "feed applies every byte" updates_feed;
    test "without the extension" updates_without_the_extension;
    test "an end, then an opening" updates_end_then_open;
  ]

let () =
  run "matrix.vte"
    [
      group "vte" tests;
      group "user and host" user_tests;
      group "graphics" [ test "kitty graphics" kitty_graphics ];
      group "updates" update_tests;
    ]
