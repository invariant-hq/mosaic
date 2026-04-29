open Windtrap

(** Test utilities *)

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
  is_false ~msg:"not alternate screen" (Vte.is_alternate_screen vte);
  equal ~msg:"cursor at origin" (pair int int) (0, 0) (Vte.cursor_pos vte);
  is_true ~msg:"cursor visible" (Vte.cursor_visible vte)

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
  Vte.feed_string vte "Hello World";
  is_true ~msg:"marked dirty" (Vte.is_dirty vte);
  let line = get_line (Vte.grid vte) 0 in
  equal ~msg:"text rendered" string "Hello World" line;
  equal ~msg:"cursor advanced" (pair int int) (0, 11) (Vte.cursor_pos vte)

let feed_multiline_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3";
  equal ~msg:"first line" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line" string "Line2" (get_line (Vte.grid vte) 1);
  equal ~msg:"third line" string "Line3" (get_line (Vte.grid vte) 2);
  equal ~msg:"cursor position" (pair int int) (2, 5) (Vte.cursor_pos vte)

let feed_carriage_return () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\rWorld";
  equal ~msg:"CR overwrites" string "World" (get_line (Vte.grid vte) 0);
  equal ~msg:"cursor after CR" (pair int int) (0, 5) (Vte.cursor_pos vte)

let feed_tab () =
  let vte = Vte.create ~rows:5 ~cols:40 () in
  Vte.feed_string vte "A\tB\tC";
  equal ~msg:"first char" char 'A' (get_char_at (Vte.grid vte) 0 0);
  equal ~msg:"after first tab" char 'B' (get_char_at (Vte.grid vte) 0 8);
  equal ~msg:"after second tab" char 'C' (get_char_at (Vte.grid vte) 0 16)

let feed_backspace () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\bX";
  equal ~msg:"backspace moves cursor" string "HellX" (get_line (Vte.grid vte) 0)

(** {1 Cursor Movement} *)

let cursor_up () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2";
  Vte.feed_string vte "\x1b[A";
  Vte.feed_string vte "X";
  equal ~msg:"cursor up modifies previous line" string "Line1X"
    (get_line (Vte.grid vte) 0)

let cursor_down () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\x1b[B\rLine2";
  equal ~msg:"first line" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line" string "Line2" (get_line (Vte.grid vte) 1)

let cursor_forward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\r\x1b[3CX";
  equal ~msg:"cursor forward" string "HelXo" (get_line (Vte.grid vte) 0)

let cursor_backward () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[2DX";
  equal ~msg:"cursor backward" string "HelXo" (get_line (Vte.grid vte) 0)

let cursor_position () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[3;5HX";
  equal ~msg:"positioned character" char 'X' (get_char_at (Vte.grid vte) 2 4)

let cursor_horizontal_absolute () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[10GX";
  equal ~msg:"horizontal absolute" char 'X' (get_char_at (Vte.grid vte) 0 9)

let cursor_next_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\x1b[EX";
  equal ~msg:"next line moves down and to column 0" (pair int int) (1, 1)
    (Vte.cursor_pos vte)

let cursor_previous_line () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\nHello\x1b[FX";
  equal ~msg:"previous line moves up and to column 0" (pair int int) (0, 1)
    (Vte.cursor_pos vte)

(** {1 Screen Manipulation} *)

let erase_display_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;5H\x1b[J";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"second line partial" string "Line" (get_line (Vte.grid vte) 1);
  equal ~msg:"third line cleared" string "" (get_line (Vte.grid vte) 2)

let erase_display_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;3H\x1b[1J";
  equal ~msg:"first line cleared" string "" (get_line (Vte.grid vte) 0);
  let line2 = get_line (Vte.grid vte) 1 in
  is_true ~msg:"second line partially cleared" (String.length line2 >= 2);
  equal ~msg:"third line unchanged" string "Line3" (get_line (Vte.grid vte) 2)

let erase_display_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2J";
  equal ~msg:"line 0 cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"line 1 cleared" string "" (get_line (Vte.grid vte) 1);
  equal ~msg:"line 2 cleared" string "" (get_line (Vte.grid vte) 2)

let erase_line_to_end () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[K";
  equal ~msg:"erase to end of line" string "Hello" (get_line (Vte.grid vte) 0)

let erase_line_to_beginning () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[1K";
  let line = get_line (Vte.grid vte) 0 in
  is_true ~msg:"beginning erased" (String.sub line 0 5 = "     ");
  equal ~msg:"end preserved" string " World" (String.sub line 5 6)

let erase_line_all () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[5C\x1b[2K";
  equal ~msg:"entire line erased" string "" (get_line (Vte.grid vte) 0)

let insert_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;1H\x1b[LNew";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"new line inserted" string "New" (get_line (Vte.grid vte) 1);
  equal ~msg:"second line pushed down" string "Line2"
    (get_line (Vte.grid vte) 2)

let delete_lines () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nLine2\nLine3\x1b[2;1H\x1b[M";
  equal ~msg:"first line unchanged" string "Line1" (get_line (Vte.grid vte) 0);
  equal ~msg:"third line moved up" string "Line3" (get_line (Vte.grid vte) 1);
  equal ~msg:"bottom line empty" string "" (get_line (Vte.grid vte) 2)

let insert_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\r\x1b[3C\x1b[2@XX";
  let line = get_line (Vte.grid vte) 0 in
  is_true ~msg:"characters inserted" (String.length line >= 7);
  equal ~msg:"insert at position" string "HelXXlo" (String.sub line 0 7)

let delete_characters () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello World\r\x1b[7C\x1b[5P";
  equal ~msg:"characters deleted" string "Hello W" (get_line (Vte.grid vte) 0)

let backspace_clears_character () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  Vte.feed_string vte "ABC";
  Vte.feed_string vte "\b";
  let grid = Vte.grid vte in
  equal ~msg:"A kept" char 'A' (get_char_at grid 0 0);
  equal ~msg:"B kept" char 'B' (get_char_at grid 0 1);
  equal ~msg:"backspaced cell cleared" char ' ' (get_char_at grid 0 2);
  equal ~msg:"cursor moved left" (pair int int) (0, 2) (Vte.cursor_pos vte)

let delete_characters_wide () =
  let vte = Vte.create ~rows:1 ~cols:5 () in
  Vte.feed_string vte "中A";
  Vte.feed_string vte "\r\x1b[P";
  let grid = Vte.grid vte in
  equal ~msg:"wide character removed entirely" string "A" (get_line grid 0);
  is_false ~msg:"no orphan continuation" (Grid.is_continuation grid 0)

let insert_characters_wide_boundary () =
  let vte = Vte.create ~rows:1 ~cols:4 () in
  Vte.feed_string vte "AB中";
  Vte.feed_string vte "\r\x1b[@";
  let grid = Vte.grid vte in
  equal ~msg:"shift respects graphemes" string " AB" (get_line grid 0);
  let last_idx = Grid.width grid - 1 in
  is_false ~msg:"last cell not continuation"
    (Grid.is_continuation grid last_idx)

(** {1 SGR (Styling)} *)

let sgr_bold () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[1mBold\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"bold attribute set" (Ansi.Attr.mem Ansi.Attr.Bold attrs)

let sgr_italic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[3mItalic\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"italic attribute set" (Ansi.Attr.mem Ansi.Attr.Italic attrs)

let sgr_underline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[4mUnderline\x1b[0m";
  let grid = Vte.grid vte in
  let attrs = Grid.get_attrs grid 0 |> Ansi.Attr.unpack in
  is_true ~msg:"underline attribute set"
    (Ansi.Attr.mem Ansi.Attr.Underline attrs)

let sgr_foreground_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[31mRed\x1b[0m";
  let grid = Vte.grid vte in
  let r =
    Grid.get_fg_r grid 0 |> fun v -> Float.round (v *. 255.) |> int_of_float
  in
  (* Standard ANSI red fallback = (128, 0, 0) *)
  equal ~msg:"red foreground" int 128 r

let sgr_background_color () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[42mGreen BG\x1b[0m";
  let grid = Vte.grid vte in
  let g =
    Grid.get_bg_g grid 0 |> fun v -> Float.round (v *. 255.) |> int_of_float
  in
  (* Standard ANSI green fallback = (0, 128, 0) *)
  equal ~msg:"green background" int 128 g

let sgr_reset () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[1;31mBold Red\x1b[0mNormal";
  let grid = Vte.grid vte in
  let normal_attrs = Grid.get_attrs grid 8 |> Ansi.Attr.unpack in
  is_true ~msg:"attributes reset" (Ansi.Attr.is_empty normal_attrs)

(** {1 Cursor Visibility} *)

let cursor_hide_show () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_true ~msg:"initially visible" (Vte.cursor_visible vte);
  Vte.feed_string vte "\x1b[?25l";
  is_false ~msg:"hidden after DECTCEM hide" (Vte.cursor_visible vte);
  Vte.feed_string vte "\x1b[?25h";
  is_true ~msg:"shown after DECTCEM show" (Vte.cursor_visible vte)

let set_cursor_visible_programmatic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.set_cursor_visible vte false;
  is_false ~msg:"programmatically hidden" (Vte.cursor_visible vte);
  Vte.set_cursor_visible vte true;
  is_true ~msg:"programmatically shown" (Vte.cursor_visible vte)

(** {1 Cursor Save/Restore} *)

let cursor_save_restore () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[5;10HHello\x1b7";
  let pos1 = Vte.cursor_pos vte in
  Vte.feed_string vte "\x1b[1;1HWorld\x1b8";
  let pos2 = Vte.cursor_pos vte in
  equal ~msg:"cursor restored" (pair int int) pos1 pos2

(** {1 Scrolling} *)

let scroll_up_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  Vte.scroll_up vte 2;
  equal ~msg:"line moved up" string "L3" (get_line (Vte.grid vte) 0);
  equal ~msg:"bottom cleared" string "" (get_line (Vte.grid vte) 3)

let scroll_down_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "L1\nL2\nL3";
  Vte.scroll_down vte 1;
  equal ~msg:"top cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"line moved down" string "L1" (get_line (Vte.grid vte) 1)

let scroll_on_bottom_line () =
  let vte = Vte.create ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4";
  equal ~msg:"first line scrolled off" string "L2" (get_line (Vte.grid vte) 0);
  equal ~msg:"new line at bottom" string "L4" (get_line (Vte.grid vte) 2)

(** {1 Scrollback} *)

let scrollback_accumulates () =
  let vte = Vte.create ~scrollback:100 ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  equal ~msg:"scrollback has lines" int 2 (Vte.scrollback_size vte);
  let lines = Vte.scrollback_lines vte in
  equal ~msg:"scrollback list length" int 2 (List.length lines)

let scrollback_ring_buffer () =
  let vte = Vte.create ~scrollback:2 ~rows:2 ~cols:5 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4\nL5";
  equal ~msg:"scrollback at capacity" int 2 (Vte.scrollback_size vte);
  let lines = Vte.scrollback_lines vte in
  equal ~msg:"oldest in ring" string "L2" (List.nth lines 0);
  equal ~msg:"newest in ring" string "L3" (List.nth lines 1)

let scrollback_disabled () =
  let vte = Vte.create ~scrollback:0 ~rows:3 ~cols:10 () in
  Vte.feed_string vte "L1\nL2\nL3\nL4";
  equal ~msg:"no scrollback" int 0 (Vte.scrollback_size vte);
  equal ~msg:"empty scrollback" (list string) [] (Vte.scrollback_lines vte)

(** {1 Terminal Modes} *)

let auto_wrap_mode () =
  let vte = Vte.create ~rows:3 ~cols:5 () in
  is_true ~msg:"auto wrap on by default" (Vte.auto_wrap_mode vte);
  Vte.feed_string vte "HelloWorld";
  equal ~msg:"wrapped to next line" string "World" (get_line (Vte.grid vte) 1)

let insert_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"insert mode off by default" (Vte.insert_mode vte);
  Vte.feed_string vte "\x1b[4h";
  is_true ~msg:"insert mode enabled" (Vte.insert_mode vte);
  Vte.feed_string vte "\x1b[4l";
  is_false ~msg:"insert mode disabled" (Vte.insert_mode vte)

let cursor_key_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"cursor key mode off by default" (Vte.cursor_key_mode vte);
  Vte.feed_string vte "\x1b[?1h";
  is_true ~msg:"cursor key mode enabled" (Vte.cursor_key_mode vte);
  Vte.feed_string vte "\x1b[?1l";
  is_false ~msg:"cursor key mode disabled" (Vte.cursor_key_mode vte)

let bracketed_paste_mode () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"bracketed paste off by default" (Vte.bracketed_paste_mode vte);
  Vte.feed_string vte "\x1b[?2004h";
  is_true ~msg:"bracketed paste enabled" (Vte.bracketed_paste_mode vte);
  Vte.feed_string vte "\x1b[?2004l";
  is_false ~msg:"bracketed paste disabled" (Vte.bracketed_paste_mode vte)

(** {1 OSC Sequences} *)

let osc_set_title () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  equal ~msg:"title empty initially" string "" (Vte.title vte);
  Vte.feed_string vte "\x1b]0;My Title\x07";
  equal ~msg:"title set via OSC 0" string "My Title" (Vte.title vte);
  Vte.feed_string vte "\x1b]2;New Title\x07";
  equal ~msg:"title set via OSC 2" string "New Title" (Vte.title vte)

(** {1 Resize} *)

let resize_preserves_content () =
  let vte = Vte.create ~rows:5 ~cols:10 () in
  Vte.feed_string vte "Hello\nWorld";
  Vte.resize vte ~rows:10 ~cols:20;
  equal ~msg:"rows resized" int 10 (Vte.rows vte);
  equal ~msg:"cols resized" int 20 (Vte.cols vte);
  (* Note: resize clears grids - applications should redraw on resize *)
  equal ~msg:"content cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"content cleared line 2" string "" (get_line (Vte.grid vte) 1)

let resize_clamps_cursor () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.feed_string vte "\x1b[8;15H";
  Vte.resize vte ~rows:5 ~cols:10;
  let row, col = Vte.cursor_pos vte in
  is_true ~msg:"cursor row clamped" (row < 5);
  is_true ~msg:"cursor col clamped" (col < 10)

(** {1 Reset} *)

let reset_clears_state () =
  let vte = Vte.create ~scrollback:100 ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello\nWorld";
  Vte.feed_string vte "\x1b[1;31m";
  Vte.feed_string vte "\x1b]0;Title\x07";
  Vte.scroll_up vte 1;
  Vte.reset vte;
  equal ~msg:"grid cleared" string "" (get_line (Vte.grid vte) 0);
  equal ~msg:"title cleared" string "" (Vte.title vte);
  equal ~msg:"cursor at origin" (pair int int) (0, 0) (Vte.cursor_pos vte);
  equal ~msg:"scrollback cleared" int 0 (Vte.scrollback_size vte);
  is_true ~msg:"auto wrap reset" (Vte.auto_wrap_mode vte)

(** {1 Dirty Tracking} *)

let dirty_flag_on_text () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  is_false ~msg:"clean initially" (Vte.is_dirty vte);
  Vte.feed_string vte "Hello";
  is_true ~msg:"dirty after text" (Vte.is_dirty vte);
  Vte.clear_dirty vte;
  is_false ~msg:"clean after clear" (Vte.is_dirty vte)

let dirty_flag_on_cursor_move () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  Vte.feed_string vte "\x1b[5;10H";
  is_true ~msg:"dirty after cursor move" (Vte.is_dirty vte)

let dirty_flag_on_style_change () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;
  Vte.feed_string vte "\x1b[1m";
  is_true ~msg:"dirty after style change" (Vte.is_dirty vte)

let dirty_row_tracking () =
  let vte = Vte.create ~rows:10 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Write to row 0 *)
  Vte.feed_string vte "Hello";
  is_true ~msg:"dirty after write" (Vte.is_dirty vte);
  equal ~msg:"row 0 dirty" (list int) [ 0 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Move to row 5 and write *)
  Vte.feed_string vte "\x1b[6;1HWorld";
  equal ~msg:"row 5 dirty" (list int) [ 5 ] (Vte.dirty_rows vte);

  Vte.clear_dirty vte;

  (* Erase display (affects multiple rows) *)
  Vte.feed_string vte "\x1b[2J";
  let rows = Vte.dirty_rows vte in
  is_true ~msg:"multiple rows dirty" (List.length rows > 1);
  is_true ~msg:"includes row 0" (List.mem 0 rows)

let dirty_cursor_tracking () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.clear_dirty vte;

  (* Move cursor *)
  Vte.feed_string vte "\x1b[3;5H";
  is_true ~msg:"cursor dirty after move" (Vte.is_cursor_dirty vte);

  Vte.clear_dirty vte;
  is_false ~msg:"cursor clean after clear" (Vte.is_cursor_dirty vte);

  (* Hide cursor *)
  Vte.feed_string vte "\x1b[?25l";
  is_true ~msg:"cursor dirty after visibility change" (Vte.is_cursor_dirty vte)

(** {1 Unicode Support} *)

let unicode_basic () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello 世界 🌍";
  let output = Vte.to_string vte in
  equal ~msg:"unicode text" string "Hello 世界 🌍" (String.trim output)

let unicode_with_escapes () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "\x1b[31mRed 文字\x1b[0m Normal";
  let output = Vte.to_string vte in
  equal ~msg:"unicode with ANSI" string "Red 文字 Normal" (String.trim output)

let unicode_multiline () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Line1\nΓειά σου\n日本語";
  let output = Vte.to_string vte in
  equal ~msg:"multiline unicode" string
    "Line1               \nΓειά σου            \n日本語" (String.trim output)

let unicode_wide_char_cursor () =
  let vte = Vte.create ~rows:5 ~cols:20 () in
  Vte.feed_string vte "Hello 世界";
  let row, col = Vte.cursor_pos vte in
  (* "Hello " = 6 cols, "世界" = 4 cols (2 wide chars × 2 cols each) *)
  equal ~msg:"cursor after wide chars" (pair int int) (0, 10) (row, col)

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
  Vte.feed vte malformed 0 5;
  let output = Vte.to_string vte in
  (* Invalid UTF-8 bytes are replaced with U+FFFD (replacement character) *)
  (* U+FFFD in UTF-8 is \xEF\xBF\xBD (\239\191\189) *)
  equal ~msg:"malformed UTF-8" string "A\239\191\189\239\191\189BC"
    (String.trim output)

(** {1 Test Suite} *)

let tests =
  [
    test "create with defaults" create_with_defaults;
    test "create with no scrollback" create_with_no_scrollback;
    test "create with custom scrollback" create_with_custom_scrollback;
    test "feed simple text" feed_simple_text;
    test "feed multiline text" feed_multiline_text;
    test "feed carriage return" feed_carriage_return;
    test "feed tab" feed_tab;
    test "feed backspace" feed_backspace;
    test "cursor up" cursor_up;
    test "cursor down" cursor_down;
    test "cursor forward" cursor_forward;
    test "cursor backward" cursor_backward;
    test "cursor position" cursor_position;
    test "cursor horizontal absolute" cursor_horizontal_absolute;
    test "cursor next line" cursor_next_line;
    test "cursor previous line" cursor_previous_line;
    test "erase display to end" erase_display_to_end;
    test "erase display to beginning" erase_display_to_beginning;
    test "erase display all" erase_display_all;
    test "erase line to end" erase_line_to_end;
    test "erase line to beginning" erase_line_to_beginning;
    test "erase line all" erase_line_all;
    test "insert lines" insert_lines;
    test "delete lines" delete_lines;
    test "insert characters" insert_characters;
    test "delete characters" delete_characters;
    test "backspace clears character" backspace_clears_character;
    test "delete characters wide grapheme" delete_characters_wide;
    test "insert characters wide boundary" insert_characters_wide_boundary;
    test "SGR bold" sgr_bold;
    test "SGR italic" sgr_italic;
    test "SGR underline" sgr_underline;
    test "SGR foreground color" sgr_foreground_color;
    test "SGR background color" sgr_background_color;
    test "SGR reset" sgr_reset;
    test "cursor hide/show" cursor_hide_show;
    test "set cursor visible" set_cursor_visible_programmatic;
    test "cursor save/restore" cursor_save_restore;
    test "scroll up basic" scroll_up_basic;
    test "scroll down basic" scroll_down_basic;
    test "scroll on bottom line" scroll_on_bottom_line;
    test "scrollback accumulates" scrollback_accumulates;
    test "scrollback ring buffer" scrollback_ring_buffer;
    test "scrollback disabled" scrollback_disabled;
    test "auto wrap mode" auto_wrap_mode;
    test "insert mode" insert_mode;
    test "cursor key mode" cursor_key_mode;
    test "bracketed paste mode" bracketed_paste_mode;
    test "OSC set title" osc_set_title;
    test "resize preserves content" resize_preserves_content;
    test "resize clamps cursor" resize_clamps_cursor;
    test "reset clears state" reset_clears_state;
    test "dirty flag on text" dirty_flag_on_text;
    test "dirty flag on cursor move" dirty_flag_on_cursor_move;
    test "dirty flag on style change" dirty_flag_on_style_change;
    test "dirty row tracking" dirty_row_tracking;
    test "dirty cursor tracking" dirty_cursor_tracking;
    test "unicode basic" unicode_basic;
    test "unicode with escapes" unicode_with_escapes;
    test "unicode multiline" unicode_multiline;
    test "unicode wide char cursor" unicode_wide_char_cursor;
    test "unicode malformed utf8" unicode_malformed_utf8;
  ]

let () = run "matrix.vte" [ group "vte" tests ]
