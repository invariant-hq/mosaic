module Grid = Matrix_grid
module Screen = Matrix_screen
open Windtrap

(* Test Helpers *)

let create_renderer ?(width = 10) ?(height = 10) () =
  let r = Screen.create () in
  (* Build and render a frame to initialize buffers *)
  let _ =
    Screen.build r ~width ~height (fun _grid _hits -> ());
    Screen.render r
  in
  r

let build_screen r ~width ~height f =
  Screen.build r ~width ~height f;
  r

let count_cursor_moves output =
  (* Count cursor position sequences (ESC[row;colH) *)
  let re = Str.regexp "\027\\[[0-9]+;[0-9]+H" in
  let rec count acc pos =
    try
      let _ = Str.search_forward re output pos in
      count (acc + 1) (Str.match_end ())
    with Not_found -> acc
  in
  count 0 0

let add_unique_by_phys acc v =
  if List.exists (fun existing -> existing == v) acc then acc else v :: acc

let viewport height = { Screen.y = 0; height }

(* A grid handed out after a render is cleared then, once: a build that
   follows draws over it without clearing it again, and a second build
   replaces the first. *)
let test_handed_out_grid_is_cleared_once () =
  let r = create_renderer ~width:4 ~height:1 () in
  Screen.build r ~width:4 ~height:1 (fun grid _ ->
      Grid.draw_text grid ~x:0 ~y:0 ~text:"old");
  ignore (Screen.render r : string);
  let grid = Screen.next_grid r in
  equal ~msg:"the presented frame is cleared from the handed-out grid" string ""
    (String.trim (Grid.to_text grid));
  Grid.draw_text grid ~x:3 ~y:0 ~text:"h";
  Screen.build r ~width:4 ~height:1 (fun grid _ ->
      Grid.draw_text grid ~x:0 ~y:0 ~text:"b");
  equal ~msg:"the build draws over the handed-out grid" string "b  h"
    (Grid.to_text (Screen.next_grid r));
  Screen.build r ~width:4 ~height:1 (fun grid _ ->
      Grid.draw_text grid ~x:1 ~y:0 ~text:"c");
  equal ~msg:"a second build replaces the first" string " c"
    (Grid.to_text (Screen.next_grid r))

(* 1. Core Rendering Tests *)

let test_create_renderer () =
  let r = Screen.create () in
  is_true ~msg:"renderer created" (r != Obj.magic 0)

(* Screen rendering is pure diff output; no terminal-side effects are
   emitted. *)
let zero_frame_expected = ""

let test_zero_sized_frame () =
  (* Edge case: 0x0 frame should not crash *)
  let r = Screen.create () in
  let frame = build_screen r ~width:0 ~height:0 (fun _ _ -> ()) in
  let output = Screen.render frame in
  equal ~msg:"empty output" string zero_frame_expected output

let test_single_cell_frame () =
  let r = Screen.create () in
  let frame =
    build_screen r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let output = Screen.render frame in
  satisfies ~claim:"non-empty output" string
    (fun s -> String.length s > 0)
    output;
  is_true ~msg:"contains X" (String.contains output 'X')

let test_simple_text_rendering () =
  let r = Screen.create () in
  let frame =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let output = Screen.render frame in
  satisfies ~claim:"non-empty output" string
    (fun s -> String.length s > 0)
    output;
  (* Check that all characters from "Hello" appear in output *)
  is_true ~msg:"contains H" (String.contains output 'H');
  is_true ~msg:"contains e" (String.contains output 'e');
  is_true ~msg:"contains l" (String.contains output 'l');
  is_true ~msg:"contains o" (String.contains output 'o')

let test_hyperlink_rendering () =
  let r = Screen.create () in
  let frame1 =
    build_screen r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let _ = Screen.render frame1 in
  let link_style =
    Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
  in
  let frame2 =
    build_screen r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A" ~style:link_style)
  in
  let output = Screen.render frame2 in
  contains ~msg:"contains hyperlink start" ~sub:"\027]8;;https://example.com"
    output

let test_row_offset_applied () =
  let r = create_renderer ~width:2 ~height:2 () in
  Screen.set_row_offset r 3;
  let frame =
    build_screen r ~width:2 ~height:2 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let output = Screen.render frame in
  contains ~msg:"cursor moved with offset" ~sub:"\027[4;1H" output;
  contains ~msg:"character rendered" ~sub:"A" output

let test_viewport_expansion_renders_new_rows () =
  let r = Screen.create () in
  let draw_rows grid =
    Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
    Grid.draw_text grid ~x:0 ~y:1 ~text:"B";
    Grid.draw_text grid ~x:0 ~y:2 ~text:"C"
  in
  let frame1 =
    build_screen r ~width:1 ~height:3 (fun grid _hits -> draw_rows grid)
  in
  let output1 = Screen.render ~viewport:(viewport 1) frame1 in
  is_true ~msg:"first visible row rendered" (String.contains output1 'A');
  is_false ~msg:"second row initially clipped" (String.contains output1 'B');

  let frame2 =
    build_screen r ~width:1 ~height:3 (fun grid _hits -> draw_rows grid)
  in
  let output2 = Screen.render ~viewport:(viewport 3) frame2 in
  is_true ~msg:"newly exposed row rendered after expansion"
    (String.contains output2 'B');
  is_true ~msg:"newly exposed lower row rendered after expansion"
    (String.contains output2 'C')

let test_viewport_clips_active_hit_grid () =
  let r = Screen.create () in
  let frame =
    build_screen r ~width:4 ~height:4 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:0 ~y:2 ~width:4 ~height:1 ~id:77)
  in
  let _ = Screen.render ~viewport:(viewport 1) frame in
  equal ~msg:"hit below rendered height is inactive" int 0
    (Screen.query_hit frame ~x:0 ~y:2)

let test_styled_frame_resets_sgr () =
  let r = Screen.create () in
  let style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 0 0) ~bold:true () in
  let frame =
    build_screen r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"R" ~style)
  in
  let output = Screen.render frame in
  contains ~msg:"styled output resets SGR" ~sub:"\027[0m" output

let test_color_depth_reaches_sgr_emission () =
  let r = Screen.create () in
  Screen.apply_capabilities r ~explicit_width:false
    ~explicit_cursor_positioning:false ~hyperlinks:true ~color_depth:`Ansi256
    ~graphics:`Blocks ~cell_size:None;
  let style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 0 0) () in
  let frame =
    build_screen r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"R" ~style)
  in
  let output = Screen.render frame in
  contains ~msg:"screen downgraded rgb to ansi256" ~sub:"\027[0;38;5;196m"
    output;
  not_contains ~msg:"screen did not emit truecolor" ~sub:"38;2;255;0;0" output

(* 2. Diff Algorithm Tests *)

let test_diff_only_changed_cells () =
  (* Render should only output changed cells *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* First frame *)
  let f1 =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXX")
  in
  let output1 = Screen.render f1 in
  let moves1 = count_cursor_moves output1 in

  (* Second frame - only change one cell *)
  let f2 =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXY")
  in
  let output2 = Screen.render f2 in
  let moves2 = count_cursor_moves output2 in

  (* Should not require additional cursor moves once diff is warmed up *)
  satisfies
    ~claim:(Printf.sprintf "at most the first frame's %d cursor moves" moves1)
    int
    (fun moves -> moves <= moves1)
    moves2;
  contains ~msg:"moves cursor to changed cell" ~sub:"\027[1;5H" output2;
  satisfies ~claim:"non-empty second frame" string
    (fun s -> String.length s > 0)
    output2

let test_no_diff_when_unchanged () =
  let r = create_renderer ~width:5 ~height:5 () in

  (* First render *)
  let f1 =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  (* Second render with same content; expect no output *)
  let f2 =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let output2 = Screen.render f2 in

  equal ~msg:"no diff output" string "" output2

let test_wide_char_diff () =
  (* Test that wide characters are diffed correctly *)
  let r = create_renderer () in

  let f1 =
    build_screen r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    build_screen r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"你好")
  in
  (* Chinese chars, 2 cells each *)
  let output2 = Screen.render f2 in

  satisfies ~claim:"non-empty wide-char output" string
    (fun s -> String.length s > 0)
    output2

(* Regression: the defensive clear for changed zero-width (null) cells used
   to emit its space with whatever SGR state was left over from the previous
   run, painting the cell with stale colors. *)
let test_null_cell_clear_uses_cell_colors () =
  let r = create_renderer ~width:2 ~height:1 () in
  let f1 =
    build_screen r ~width:2 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let _output1 = Screen.render f1 in
  let green = Ansi.Color.of_rgb 0 200 0 in
  let f2 =
    build_screen r ~width:2 ~height:1 (fun grid _hits ->
        Grid.set_uchar grid ~x:0 ~y:0 ~uchar:Uchar.min ~fg:Ansi.Color.white
          ~bg:green ~attrs:Ansi.Attr.empty ())
  in
  let output2 = Screen.render f2 in
  satisfies ~claim:"null cell clear emitted (non-empty output)" string
    (fun s -> String.length s > 0)
    output2;
  contains ~msg:"clear uses the cell's background" ~sub:"48;2;0;200;0" output2

(* Regression: complex graphemes must be diffed by content. With buffers
   backed by independent grapheme stores, the first grapheme interned each
   frame received the same store id in both stores, so a changed grapheme
   compared equal and the diff emitted nothing. *)
let test_changed_grapheme_diff () =
  let fr =
    "\xF0\x9F\x87\xAB\xF0\x9F\x87\xB7"
    (* FR flag *)
  in
  let de =
    "\xF0\x9F\x87\xA9\xF0\x9F\x87\xAA"
    (* DE flag *)
  in
  let r = create_renderer ~width:4 ~height:1 () in
  let f1 =
    build_screen r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:fr)
  in
  let _output1 = Screen.render f1 in
  let f2 =
    build_screen r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:de)
  in
  let output2 = Screen.render f2 in
  contains ~msg:"changed grapheme re-emitted" ~sub:de output2

(* Regression: hyperlinks must be diffed by URL, not by per-buffer registry
   ids that realign across frames. *)
let test_changed_hyperlink_diff () =
  let r = create_renderer ~width:8 ~height:1 () in
  let draw url grid =
    Grid.draw_text grid ~x:0 ~y:0 ~text:"link"
      ~style:(Ansi.Style.make ~link:url ())
  in
  let f1 =
    build_screen r ~width:8 ~height:1 (fun grid _hits -> draw "http://a" grid)
  in
  let _output1 = Screen.render f1 in
  let f2 =
    build_screen r ~width:8 ~height:1 (fun grid _hits -> draw "http://b" grid)
  in
  let output2 = Screen.render f2 in
  contains ~msg:"changed hyperlink re-emitted" ~sub:"http://b" output2

(* 3. Frame Building Tests *)

let test_build_visual () =
  (* build_visual should not require hit grid function *)
  let r = Screen.create () in
  let frame =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi")
  in
  let output = Screen.render frame in
  satisfies ~claim:"visual build produces output" string
    (fun s -> String.length s > 0)
    output

(* A resize leaves the terminal's cells where they were, whatever the new
   frame has there, so the render after it paints every cell. *)
let test_resize_repaints_every_cell () =
  let r = create_renderer ~width:5 ~height:2 () in
  let vte = Vte.create ~rows:2 ~cols:6 () in
  let show output = ignore (Vte.feed_string vte output : _ list) in
  show
    (Screen.render
       (build_screen r ~width:5 ~height:2 (fun grid _hits ->
            Grid.draw_text grid ~x:0 ~y:0 ~text:"OLD")));
  show (Screen.render (build_screen r ~width:6 ~height:2 (fun _ _ -> ())));
  equal ~msg:"the old cells are gone" string "      \n      "
    (Vte.to_string vte)

(* An empty cell shows as a space in its colors, as Matrix_grid.to_text and
   to_ansi write it. *)
let empty_at grid x =
  Grid.set_uchar grid ~x ~y:0 ~uchar:(Uchar.of_int 0x200B) ~fg:Ansi.Color.white
    ~bg:(Ansi.Color.of_rgb 0 200 0)
    ~attrs:Ansi.Attr.empty ()

let test_full_render_writes_empty_cells () =
  let r = create_renderer ~width:3 ~height:1 () in
  let vte = Vte.create ~rows:1 ~cols:3 () in
  let show output = ignore (Vte.feed_string vte output : _ list) in
  show
    (Screen.render
       (build_screen r ~width:3 ~height:1 (fun grid _hits ->
            Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ")));
  let frame =
    build_screen r ~width:3 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
        empty_at grid 1)
  in
  show (Screen.render ~full:true frame);
  equal ~msg:"the text under the empty cell is erased" string "X Z"
    (Vte.to_string vte);
  equal ~msg:"in the empty cell's background" string "#00c800"
    (Ansi.Color.to_hex (Grid.get_bg (Vte.grid vte) 1));
  equal ~msg:"three columns written" int 3 (Screen.last_metrics r).cells

let test_empty_cells_reset_the_rendition () =
  let r = create_renderer ~width:3 ~height:1 () in
  ignore
    (Screen.render
       (build_screen r ~width:3 ~height:1 (fun grid _hits ->
            Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ"))
      : string);
  let output =
    Screen.render
      (build_screen r ~width:3 ~height:1 (fun grid _hits ->
           Grid.draw_text grid ~x:0 ~y:0 ~text:"XY";
           empty_at grid 2))
  in
  equal ~msg:"one empty cell written" int 1 (Screen.last_metrics r).cells;
  is_true ~msg:"the render ends with SGR 0"
    (String.ends_with ~suffix:"\027[0m" output)

(* The terminal cuts the bytes it receives into display units as the grid's
   width method does. Two cells written one after the other must not make one
   unit there. *)
let test_adjacent_cells_stay_apart () =
  List.iter
    (fun (first, second) ->
      let r = Screen.create () in
      let output =
        Screen.render
          (build_screen r ~width:4 ~height:1 (fun grid _hits ->
               Grid.draw_text grid ~x:0 ~y:0 ~text:first;
               Grid.draw_text grid ~x:1 ~y:0 ~text:second))
      in
      let vte = Vte.create ~rows:1 ~cols:4 () in
      ignore (Vte.feed_string vte output : _ list);
      let g = Vte.grid vte in
      equal ~msg:(first ^ " stays in its cell") string first (Grid.get_text g 0);
      equal
        ~msg:(second ^ " stays in its cell")
        string second (Grid.get_text g 1))
    [
      ("a", "\u{1F3FB}") (* an emoji modifier extends any character *);
      ("\u{0D4E}", "a") (* a Prepend character extends the next one *);
      ("a", "\u{302E}")
      (* a spacing mark, wide: a narrow one alone has no cell to keep *);
    ]

let test_resize_smaller () =
  (* Edge case: shrinking grid *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"BigText")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    build_screen r ~width:3 ~height:3 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Sm")
  in
  let output2 = Screen.render f2 in

  satisfies ~claim:"non-empty output after shrink" string
    (fun s -> String.length s > 0)
    output2

let test_resize_clears_both_buffers () =
  (* Test that resize clears both current and next buffers, ensuring proper
     diffing *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* Fill the screen with content *)
  let f1 =
    build_screen r ~width:5 ~height:5 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:5 ~height:5
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _output1 = Screen.render f1 in

  (* Resize to larger dimensions *)
  Screen.resize r ~width:10 ~height:10;

  (* Build a frame that only draws a small region *)
  let f2 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Small")
  in
  let output2 = Screen.render f2 in

  (* The output should not contain the old red background content *)
  (* Check that we don't have excessive output (which would indicate stale content).
     A full 10x10 redraw would produce ~1000+ bytes; the 5x5 overlap diff is ~250. *)
  satisfies
    ~claim:"fewer than 400 bytes (a 5x5 overlap diff, not a stale full redraw)"
    string
    (fun s -> String.length s < 400)
    output2;

  (* Verify the new content is rendered *)
  is_true ~msg:"new content rendered" (String.contains output2 'S')

let test_resize_clears_pending_next_buffer () =
  let r = Screen.create () in
  Screen.build r ~width:5 ~height:1 (fun grid _hits ->
      Grid.draw_text grid ~x:0 ~y:0 ~text:"STALE");
  Screen.resize r ~width:10 ~height:1;
  let f = build_screen r ~width:10 ~height:1 (fun _grid _hits -> ()) in
  let grid = Screen.next_grid f in
  equal ~msg:"pending content cleared from next grid" string " "
    (Grid.get_text grid 0);
  let output = Screen.render f in
  is_false ~msg:"stale text not rendered" (String.contains output 'S')

let test_resize_same_size_preserves_diff_baseline () =
  let r = create_renderer ~width:8 ~height:3 () in
  let bg = Ansi.Color.of_rgb 28 28 28 in
  let f1 =
    build_screen r ~width:8 ~height:3 (fun grid _hits ->
        Grid.fill_rect grid ~x:3 ~y:1 ~width:2 ~height:1 ~color:bg)
  in
  ignore (Screen.render f1 : string);
  Screen.resize r ~width:8 ~height:3;
  let f2 = build_screen r ~width:8 ~height:3 (fun _grid _hits -> ()) in
  let output = Screen.render f2 in
  satisfies ~claim:"same-size resize keeps baseline for clears (non-empty)"
    string
    (fun s -> String.length s > 0)
    output

let test_cursor_clamped_on_resize () =
  let r = create_renderer ~width:3 ~height:3 () in
  let cursor = Screen.cursor r in
  Screen.set_cursor r { cursor with position = Some (4, 4) };
  Screen.resize r ~width:2 ~height:1;
  let _frame = build_screen r ~width:2 ~height:1 (fun _grid _hits -> ()) in
  let cursor = Screen.cursor r in
  equal ~msg:"cursor clamped"
    (option (pair int int))
    (Some (1, 0))
    cursor.position

(* 5. Hit Grid Tests *)

let test_hit_grid_integration () =
  let r = Screen.create () in
  let hit_id = ref 0 in

  let frame =
    build_screen r ~width:10 ~height:10 (fun grid hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Button";
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:6 ~height:1 ~id:42)
  in
  let _ = Screen.render frame in
  hit_id := Screen.query_hit frame ~x:0 ~y:0;
  equal ~msg:"hit id at (0,0)" int 42 !hit_id;

  hit_id := Screen.query_hit frame ~x:7 ~y:0;
  equal ~msg:"no hit at (7,0)" int 0 !hit_id

let test_single_cell_frame_keeps_hits () =
  let r = Screen.create () in
  Screen.build r ~width:1 ~height:1 (fun _grid hits ->
      Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:1 ~height:1 ~id:5);
  ignore (Screen.render r : string);
  equal ~msg:"hit id at (0,0) of a 1x1 frame" int 5
    (Screen.query_hit r ~x:0 ~y:0)

(* Regression: a render read the next buffer without the clear a render
   leaves pending, so a second render with no build between showed the frame
   before last again. *)
let test_render_without_build_is_blank () =
  let r = Screen.create () in
  let frame text =
    Screen.build r ~width:3 ~height:1 (fun grid _ ->
        Grid.draw_text grid ~x:0 ~y:0 ~text)
  in
  frame "X";
  ignore (Screen.render r : string);
  frame "Y";
  ignore (Screen.render r : string);
  let output = Screen.render r in
  not_contains ~msg:"the frame before last is not shown" ~sub:"X" output;
  equal ~msg:"the current frame is blank" string ""
    (Grid.get_text (Screen.current_grid r) 0 |> String.trim)

(* The ids of a hit grid, row by row, "." for an empty cell. *)
let show_hits hits ~width ~height =
  String.concat "/"
    (List.init height (fun y ->
         String.concat ""
           (List.init width (fun x ->
                match Screen.Hit_grid.get hits ~x ~y with
                | 0 -> "."
                | id -> string_of_int id))))

let test_hit_grid_draw () =
  let module H = Screen.Hit_grid in
  let sub () =
    (* 2x2: one element on the left column, the right column empty. *)
    let g = H.create ~width:2 ~height:2 in
    H.add g ~x:0 ~y:0 ~width:1 ~height:2 ~id:7;
    g
  in
  let parent () =
    let g = H.create ~width:4 ~height:3 in
    H.add g ~x:0 ~y:0 ~width:4 ~height:3 ~id:1;
    g
  in
  let dst = parent () in
  H.draw ~src:(sub ()) ~dst ~x:1 ~y:1;
  equal ~msg:"empty cells keep what is under them" string "1111/1711/1711"
    (show_hits dst ~width:4 ~height:3);
  let dst = parent () in
  H.draw ~src:(sub ()) ~dst ~x:(-1) ~y:2;
  equal ~msg:"cells outside the grid are skipped" string "1111/1111/1111"
    (show_hits dst ~width:4 ~height:3);
  let dst = parent () in
  H.draw ~src:(sub ()) ~dst ~x:3 ~y:2;
  equal ~msg:"clipped at the far edges" string "1111/1111/1117"
    (show_hits dst ~width:4 ~height:3);
  let dst = parent () in
  H.with_clip dst { Grid.x = 0; y = 0; width = 4; height = 2 } (fun () ->
      H.draw ~src:(sub ()) ~dst ~x:0 ~y:1);
  equal ~msg:"the active clip applies" string "1111/7111/1111"
    (show_hits dst ~width:4 ~height:3);
  (* A copy of the whole grid: resize clears, then draw at the origin. *)
  let src = H.create ~width:3 ~height:1 in
  H.add src ~x:1 ~y:0 ~width:1 ~height:1 ~id:5;
  let dst = parent () in
  H.resize dst ~width:3 ~height:1;
  H.draw ~src ~dst ~x:0 ~y:0;
  equal ~msg:"resize then draw copies" string ".5."
    (show_hits dst ~width:3 ~height:1);
  (* Drawing a grid onto itself reads the cells before any is written. *)
  let g = H.create ~width:3 ~height:1 in
  H.add g ~x:0 ~y:0 ~width:1 ~height:1 ~id:1;
  H.add g ~x:1 ~y:0 ~width:1 ~height:1 ~id:2;
  H.draw ~src:g ~dst:g ~x:1 ~y:0;
  equal ~msg:"self draw reads a snapshot" string "112"
    (show_hits g ~width:3 ~height:1)

let test_hit_grid_cleared_each_frame () =
  let r = Screen.create () in

  (* First frame with hit region *)
  let f1 =
    build_screen r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:5 ~height:1 ~id:1)
  in
  let _o1 = Screen.render f1 in

  (* Second frame without hit region *)
  let f2 = build_screen r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  let _o2 = Screen.render f2 in

  let hit_id = Screen.query_hit f2 ~x:0 ~y:0 in
  equal ~msg:"hit cleared" int 0 hit_id

let test_hit_grid_swap_on_render () =
  let r = Screen.create () in

  let frame1 =
    build_screen r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:1)
  in
  let _ = Screen.render frame1 in

  (* Building the next frame should not swap hits until render runs. *)
  let frame2 =
    build_screen r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:2)
  in
  let before = Screen.query_hit frame2 ~x:1 ~y:1 in
  equal ~msg:"previous hit active before swap" int 1 before;

  let _ = Screen.render frame2 in
  let after = Screen.query_hit frame2 ~x:1 ~y:1 in
  equal ~msg:"next hit active after swap" int 2 after

let test_add_hit_region_helper () =
  let r = Screen.create () in
  let frame1 = build_screen r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  Screen.add_hit_region frame1 ~x:5 ~y:5 ~width:2 ~height:2 ~id:99;
  let frame2 = frame1 in

  let _ = Screen.render frame2 in
  let hit_id = Screen.query_hit frame2 ~x:5 ~y:5 in
  equal ~msg:"hit region added" int 99 hit_id

(* 6. Statistics Tests *)

let test_stats_tracking () =
  let r = Screen.create () in

  let f1 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  let stats = Screen.stats r in
  let metrics = Screen.last_metrics r in
  equal ~msg:"frame count" int 1 stats.frame_count;
  satisfies ~claim:"cells updated (> 0)" int (fun n -> n > 0) metrics.cells;
  satisfies ~claim:"output bytes (> 0)" int (fun n -> n > 0) metrics.bytes

(* 7. Configuration Tests *)

let test_update_config () =
  let r = Screen.create () in

  let cursor = Screen.cursor r in
  Screen.set_cursor r { cursor with visible = false };

  let frame = build_screen r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame in
  let metrics = Screen.last_metrics r in
  is_false ~msg:"cursor visibility updated" metrics.cursor_visible

let test_reset () =
  let r = Screen.create () in

  (* Render a few frames *)
  for _i = 1 to 3 do
    let f = build_screen r ~width:5 ~height:5 (fun _grid _hits -> ()) in
    let _o = Screen.render f in
    ()
  done;

  let stats_before = Screen.stats r in
  equal ~msg:"frames before reset" int 3 stats_before.frame_count;

  Screen.reset r;

  let stats_after = Screen.stats r in
  equal ~msg:"frames after reset" int 0 stats_after.frame_count;
  equal ~msg:"cells after reset" int 0 stats_after.total_cells

let test_reset_triggers_next_diff () =
  let r = create_renderer ~width:1 ~height:1 () in

  let f1 =
    build_screen r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let _ = Screen.render f1 in

  Screen.reset r;

  let f2 = build_screen r ~width:1 ~height:1 (fun _ _ -> ()) in
  let output2 = Screen.render f2 in

  satisfies ~claim:"at least 1 cursor move after reset" int
    (fun moves -> moves >= 1)
    (count_cursor_moves output2);
  is_true ~msg:"clears previous glyph" (not (String.contains output2 'X'))

(* 8. Edge Cases and Boundary Conditions *)

let test_extremely_wide_char () =
  (* Test rendering with emoji that might have unusual widths *)
  let r = create_renderer () in
  let f =
    build_screen r ~width:20 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"🎨🚀✨")
  in
  let output = Screen.render f in
  satisfies ~claim:"emoji renders (non-empty output)" string
    (fun s -> String.length s > 0)
    output

(* A full writer fails the frame, which commits nothing: the same frame
   renders again, whole, into a writer it fits. *)
let test_render_into_full_writer () =
  let build screen =
    Screen.build screen ~width:20 ~height:2 (fun grid hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"bounded 世界"
          ~style:(Ansi.Style.make ~fg:Ansi.Color.red ~bold:true ());
        Grid.draw_text grid ~x:0 ~y:1 ~text:"output";
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:2 ~height:1 ~id:9)
  in
  let reference = Screen.create () in
  build reference;
  let expected = Screen.render reference in
  let screen = Screen.create () in
  build screen;
  let small = Ansi.Writer.of_bytes (Bytes.create 16) in
  raises Ansi.Writer.Buffer_full (fun () -> Screen.render_into screen small);
  equal ~msg:"nothing committed" int 0 (Screen.stats screen).frame_count;
  equal ~msg:"no hit published" int 0 (Screen.query_hit screen ~x:0 ~y:0);
  let bytes = Bytes.create (String.length expected) in
  let fits = Ansi.Writer.of_bytes bytes in
  Screen.render_into screen fits;
  equal ~msg:"the same frame, whole" string expected (Ansi.Writer.contents fits);
  equal ~msg:"committed once" int 1 (Screen.stats screen).frame_count;
  equal ~msg:"hit published" int 9 (Screen.query_hit screen ~x:0 ~y:0)

let test_buffer_overflow_prevention () =
  (* Test with large frame (200x60) and complex content to ensure 2MB buffer
     doesn't overflow *)
  let r = create_renderer ~width:200 ~height:60 () in
  let f =
    build_screen r ~width:200 ~height:60 (fun grid _hits ->
        (* Fill with complex content: mixed text, colors, attributes *)
        let rec fill y =
          if y >= 60 then ()
          else
            let style =
              Ansi.Style.make
                ~fg:(Ansi.Color.of_rgb (y * 4) (y * 4 mod 255) (255 - (y * 4)))
                ~bg:(Ansi.Color.of_rgb (y * 4 mod 255) (y * 4) (y * 4 mod 255))
                ~bold:(y mod 2 = 0)
                ~italic:(y mod 3 = 0)
                ()
            in
            Grid.draw_text grid ~x:0 ~y ~text:(String.make 200 'A') ~style;
            fill (y + 1)
        in
        fill 0)
  in
  (* Should not raise buffer overflow *)
  let output = Screen.render f in
  satisfies
    ~claim:"more than 1000 bytes (large frame rendered without overflow)" int
    (fun len -> len > 1000)
    (String.length output)

let test_render_into_appends_and_commits_once () =
  let build screen id =
    Screen.build screen ~width:32 ~height:2 (fun grid hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com/target" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A styled frame with 世界" ~style;
        Grid.draw_text grid ~x:0 ~y:1 ~text:"second row";
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:2 ~height:1 ~id)
  in
  let reference = Screen.create () in
  build reference 42;
  let expected = Screen.render reference in
  let screen = Screen.create () in
  build screen 42;
  let writer = Ansi.Writer.create 1 in
  Ansi.Writer.write_string writer "prefix:";
  let initial_capacity = Bytes.length (Ansi.Writer.buffer writer) in
  Screen.render_into screen writer;
  equal ~msg:"caller prefix survives growth and frame output matches render"
    string ("prefix:" ^ expected)
    (Ansi.Writer.contents writer);
  satisfies ~claim:"frame grows the caller's tiny writer" int
    (fun capacity -> capacity > initial_capacity)
    (Bytes.length (Ansi.Writer.buffer writer));
  equal ~msg:"frame metrics exclude the caller prefix" int
    (String.length expected) (Screen.last_metrics screen).bytes;
  equal ~msg:"first frame commits once" int 1 (Screen.stats screen).frame_count;
  equal ~msg:"first hit grid is published" int 42
    (Screen.query_hit screen ~x:0 ~y:0);
  build screen 77;
  equal ~msg:"next hit grid stays private until rendering" int 42
    (Screen.query_hit screen ~x:0 ~y:0);
  Ansi.Writer.reset_pos writer;
  Ansi.Writer.write_string writer "next:";
  Screen.render_into screen writer;
  equal ~msg:"committed baseline makes the identical frame empty" string "next:"
    (Ansi.Writer.contents writer);
  equal ~msg:"unchanged frame reports zero output bytes" int 0
    (Screen.last_metrics screen).bytes;
  equal ~msg:"second frame commits once" int 2 (Screen.stats screen).frame_count;
  equal ~msg:"hit changes publish even without visual output" int 77
    (Screen.query_hit screen ~x:0 ~y:0)

let test_render_large_hyperlinked_frame_succeeds () =
  let r = Screen.create () in
  let width = 96 in
  let height = 96 in
  let payload = String.make 240 'x' in
  let frame =
    build_screen r ~width ~height (fun grid _hits ->
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let style =
              Ansi.Style.hyperlink
                (Printf.sprintf "https://example.com/%d/%s"
                   ((y * width) + x)
                   payload)
                Ansi.Style.default
            in
            Grid.draw_text grid ~x ~y ~text:"X" ~style
          done
        done)
  in
  let output = Screen.render frame in
  satisfies ~claim:"above the old fixed buffer's 2 MiB" int
    (fun len -> len > 2 * 1024 * 1024)
    (String.length output)

let test_resize_full_redraw () =
  (* Test that first frame after resize is full redraw *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* First frame *)
  let f1 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Initial")
  in
  let _ = Screen.render f1 in

  (* Resize *)
  Screen.resize r ~width:20 ~height:20;

  (* Second frame - should be full redraw *)
  let f2 =
    build_screen r ~width:20 ~height:20 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Resized")
  in
  let output2 = Screen.render f2 in

  (* Should contain some cursor movements for the diff *)
  let moves = count_cursor_moves output2 in
  satisfies ~claim:"contains cursor moves (> 0)" int (fun n -> n > 0) moves

let test_resize_hit_grid_cleared () =
  (* Test that hit grids don't leak stale IDs after resize *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* Add hit region *)
  let f1 =
    build_screen r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:5 ~y:5 ~width:2 ~height:2 ~id:99)
  in
  let _ = Screen.render f1 in

  (* Verify hit exists *)
  let hit_before = Screen.query_hit f1 ~x:5 ~y:5 in
  equal ~msg:"hit exists before resize" int 99 hit_before;

  (* Resize *)
  Screen.resize r ~width:5 ~height:5;

  (* Build new frame *)
  let f2 = build_screen r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render f2 in

  (* Hit should be gone (coordinates out of bounds) *)
  let hit_after = Screen.query_hit f2 ~x:0 ~y:0 in
  equal ~msg:"hit cleared after resize" int 0 hit_after

let test_wide_grapheme_emission () =
  let r = Screen.create ~width_method:`Unicode_wide ~explicit_width:true () in
  let frame =
    build_screen r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"രിX";
        equal int 2 (Grid.cell_width grid 0);
        is_true (Grid.is_continuation grid 1);
        equal string "X" (Grid.get_text grid 2))
  in
  contains ~sub:"\027]66;w=2;രി\027\\" (Screen.render frame)

let test_explicit_width_sequences () =
  (* Test that explicit width OSC sequences are emitted when enabled *)
  let r = Screen.create ~explicit_width:true () in
  let f =
    build_screen r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"中")
    (* 2-cell wide character *)
  in
  let output = Screen.render f in

  (* Should contain explicit width sequence for the wide character *)
  let contains_explicit_width =
    try
      let _ =
        Str.search_forward (Str.regexp "\027]66;w=[0-9]+;.*\027\\\\") output 0
      in
      true
    with Not_found -> false
  in
  is_true ~msg:"contains explicit width sequence" contains_explicit_width

let test_explicit_width_off_repositions () =
  let r = Screen.create ~explicit_width:true () in
  Screen.apply_capabilities r ~explicit_width:true
    ~explicit_cursor_positioning:true ~hyperlinks:true ~color_depth:`Truecolor
    ~graphics:`Blocks ~cell_size:None;
  Screen.set_explicit_width r false;
  Screen.build r ~width:10 ~height:1 (fun grid _hits ->
      Grid.draw_text grid ~x:0 ~y:0 ~text:"\u{4E2D}");
  let output = Screen.render r in
  not_contains ~msg:"no OSC 66" ~sub:"\027]66;" output;
  contains ~msg:"the cursor moves past the wide grapheme" ~sub:"\027[1;3H"
    output

let test_hyperlink_capability_gating () =
  (* Test that hyperlinks are only emitted when capability is enabled *)
  let r1 = Screen.create () in
  (* hyperlinks_capable defaults to true *)
  let f1 =
    build_screen r1 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output1 = Screen.render f1 in

  (* Should contain hyperlink sequences *)
  contains ~msg:"hyperlink emitted when capable" ~sub:"\027]8;;" output1;

  (* Now test with capability disabled *)
  let r2 = Screen.create () in
  Screen.apply_capabilities r2 ~explicit_width:false
    ~explicit_cursor_positioning:false ~hyperlinks:false ~color_depth:`Truecolor
    ~graphics:`Blocks ~cell_size:None;
  let f2 =
    build_screen r2 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output2 = Screen.render f2 in

  (* Should NOT contain hyperlink sequences *)
  not_contains ~msg:"hyperlink not emitted when incapable" ~sub:"\027]8;;"
    output2

let test_scroll_hint_clips_to_viewport () =
  let r = Screen.create () in
  let f1 =
    build_screen r ~width:1 ~height:4 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"B";
        Grid.draw_text grid ~x:0 ~y:2 ~text:"C";
        Grid.draw_text grid ~x:0 ~y:3 ~text:"D")
  in
  let _ = Screen.render ~viewport:(viewport 2) f1 in
  let f2 =
    build_screen r ~width:1 ~height:4 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"B";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"E";
        Grid.draw_text grid ~x:0 ~y:2 ~text:"C";
        Grid.draw_text grid ~x:0 ~y:3 ~text:"D")
  in
  let output =
    Screen.render ~viewport:(viewport 2)
      ~scroll_hint:{ Screen.top = 0; bottom = 3; delta = 1 }
      f2
  in
  contains ~msg:"scroll region clipped to rendered rows" ~sub:"\027[1;2r" output;
  not_contains ~msg:"scroll region does not include clipped rows"
    ~sub:"\027[1;4r" output

let test_scroll_hint_applies_row_offset () =
  let r = Screen.create () in
  Screen.set_row_offset r 3;
  let f1 =
    build_screen r ~width:1 ~height:3 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"B";
        Grid.draw_text grid ~x:0 ~y:2 ~text:"C")
  in
  let _ = Screen.render f1 in
  let f2 =
    build_screen r ~width:1 ~height:3 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"B";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"C";
        Grid.draw_text grid ~x:0 ~y:2 ~text:"D")
  in
  let output =
    Screen.render ~scroll_hint:{ Screen.top = 0; bottom = 2; delta = 1 } f2
  in
  contains ~msg:"scroll region includes row offset" ~sub:"\027[4;6r" output;
  not_contains ~msg:"scroll region is not absolute top" ~sub:"\027[1;3r" output

(* A shift by the whole region keeps none of its rows. xterm 398 also blanks
   the rows outside the region when SU scrolls by its whole height, which the
   diff would not repaint. *)
let test_scroll_hint_by_whole_region_is_ignored () =
  let r = Screen.create () in
  let rows texts =
    build_screen r ~width:1 ~height:4 (fun grid _hits ->
        List.iteri (fun y text -> Grid.draw_text grid ~x:0 ~y ~text) texts)
  in
  let _ = Screen.render (rows [ "A"; "B"; "C"; "D" ]) in
  let output =
    Screen.render
      ~scroll_hint:{ Screen.top = 1; bottom = 2; delta = 2 }
      (rows [ "A"; "E"; "F"; "D" ])
  in
  not_contains ~msg:"no scroll region" ~sub:"\027[2;3r" output;
  not_contains ~msg:"no scroll" ~sub:"\027[2S" output;
  in_order ~msg:"the region's rows are repainted" ~subs:[ "E"; "F" ] output

let test_cursor_style_and_color () =
  (* Test cursor style and color state *)
  let r = create_renderer () in
  let cursor = Screen.cursor r in
  Screen.set_cursor r
    {
      cursor with
      position = Some (9, 4);
      style = `Underline;
      blinking = false;
      color = Some (255, 0, 128);
    };

  let _f = build_screen r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let info = Screen.cursor r in
  is_true ~msg:"cursor visible" info.visible;
  equal ~msg:"cursor position stored"
    (option (pair int int))
    (Some (4, 4))
    info.position;
  is_true ~msg:"cursor underline" (info.style = `Underline);
  is_true ~msg:"cursor non-blinking" (not info.blinking);
  is_true ~msg:"cursor color stored" (info.color = Some (255, 0, 128));
  Screen.set_cursor r
    { info with position = Some (-2, -3); color = Some (-10, 300, 128) };
  Screen.reset r;
  let normalized = Screen.cursor r in
  equal ~msg:"reset preserves normalized cursor position"
    (option (pair int int))
    (Some (0, 0))
    normalized.position;
  is_true ~msg:"reset preserves normalized cursor color"
    (normalized.color = Some (0, 255, 128))

let test_all_cells_changed () =
  (* Worst case: every cell changes *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:10
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _o1 = Screen.render f1 in

  let f2 =
    build_screen r ~width:10 ~height:10 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:10
          ~color:(Ansi.Color.of_rgb 0 255 0))
  in
  let _output2 = Screen.render f2 in

  let metrics = Screen.last_metrics r in
  (* All 100 cells should have changed *)
  equal ~msg:"all cells changed" int 100 metrics.cells

let test_partial_row_update () =
  (* Test that we only render changed portions of a row *)
  let r = create_renderer ~width:10 ~height:1 () in

  let f1 =
    build_screen r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXXXXXXX")
  in
  let _o1 = Screen.render f1 in

  let f2 =
    build_screen r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXXAXXXX")
  in
  let _output2 = Screen.render f2 in

  let metrics = Screen.last_metrics r in
  (* Should only update the changed cell *)
  equal ~msg:"only changed cell" int 1 metrics.cells

let test_color_only_change () =
  (* Test that color changes trigger diff *)
  let r = create_renderer () in

  let style1 = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 0 0) () in
  let f1 =
    build_screen r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style1)
  in
  let _o1 = Screen.render f1 in

  let style2 = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0 255 0) () in
  let f2 =
    build_screen r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style2)
  in
  let output2 = Screen.render f2 in

  (* Should detect color change *)
  satisfies ~claim:"more than 10 bytes (color change detected)" string
    (fun s -> String.length s > 10)
    output2

let test_attribute_only_change () =
  (* Test that attribute changes (bold, italic, etc) trigger diff *)
  let r = create_renderer () in

  let f1 =
    build_screen r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _o1 = Screen.render f1 in

  let style_bold = Ansi.Style.make ~bold:true () in
  let f2 =
    build_screen r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style_bold)
  in
  let output2 = Screen.render f2 in

  (* Should detect attribute change *)
  satisfies ~claim:"more than 10 bytes (attribute change detected)" string
    (fun s -> String.length s > 10)
    output2

(* 9. Performance Characteristics Tests *)

let test_double_buffer_reuse () =
  let r = Screen.create () in
  let grids = ref [] in
  let hits = ref [] in

  let record frame =
    grids := add_unique_by_phys !grids (Screen.next_grid frame);
    hits := add_unique_by_phys !hits (Screen.next_hit_grid frame)
  in

  let render_once () =
    let frame = build_screen r ~width:4 ~height:2 (fun _grid _hits -> ()) in
    record frame;
    let _ = Screen.render frame in
    ()
  in

  render_once ();
  render_once ();
  render_once ();

  equal ~msg:"reuses two grid buffers" int 2 (List.length !grids);
  equal ~msg:"reuses two hit grids" int 2 (List.length !hits)

let test_transform_before_render () =
  let screen = Screen.create () in
  Screen.build screen ~width:5 ~height:1 (fun grid _ ->
      Grid.draw_text grid ~x:0 ~y:0 ~text:"before");
  let transform grid = Grid.draw_text grid ~x:0 ~y:0 ~text:"after" in
  transform (Screen.next_grid screen);
  let output = Screen.render screen in
  contains ~msg:"render emits transformed contents" ~sub:"after" output;
  equal ~msg:"transformed contents become the diff baseline" string "after"
    (Grid.to_text (Screen.current_grid screen))

(* Test Suite *)

(* Bytes per change: what a 160x48 frame writes to the terminal for a
   change of each size. Output is deterministic for fixed capabilities, so
   a change in what a frame costs shows here as a changed number. *)

let bytes_style =
  Ansi.Style.make
    ~fg:(Ansi.Color.of_rgb 232 232 232)
    ~bg:(Ansi.Color.of_rgb 18 20 24)
    ~bold:true ()

let bytes_line =
  String.init 158 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

(* The bytes of each frame of [changes] after a full frame of text. *)
let frame_bytes changes =
  let screen = Screen.create () in
  let frame draw =
    Screen.build screen ~width:160 ~height:48 (fun grid _hits ->
        Grid.clear grid ~color:(Ansi.Color.of_rgb 0 0 0);
        for row = 0 to 47 do
          Grid.draw_text ~style:bytes_style grid ~x:0 ~y:row ~text:bytes_line
        done;
        draw grid);
    String.length (Screen.render screen)
  in
  let full = frame ignore in
  full :: List.map frame changes

let bytes_per_change () =
  let cell grid =
    Grid.draw_text ~style:bytes_style grid ~x:80 ~y:24 ~text:"X"
  in
  let line grid =
    Grid.draw_text
      ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 80 80) ~bold:true ())
      grid ~x:0 ~y:24 ~text:(String.make 158 'Z')
  in
  match frame_bytes [ ignore; cell; line ] with
  | [ full; unchanged; one_cell; one_line ] ->
      equal int ~msg:"a full frame of text" 10_507 full;
      equal int ~msg:"an unchanged frame" 0 unchanged;
      equal int ~msg:"one cell changed" 50 one_cell;
      equal int ~msg:"one line restyled" 204 one_line
  | _ -> assert false

(* Pictures. Each tile of a tile-coded picture is one solid color, on the
   levels of the sixel cube, so that a cell's colors, or a sixel's pixels,
   say which tile the terminal shows there. *)

let tile_colors =
  [|
    (255, 0, 0);
    (0, 255, 0);
    (0, 0, 255);
    (255, 255, 0);
    (0, 255, 255);
    (255, 0, 255);
    (255, 255, 255);
    (0, 0, 0);
  |]

(* [cols] by [rows] tiles of 10 by 20 pixels, row-major in [tile_colors]. *)
let tiled ~cols ~rows =
  let w = 10 * cols and h = 20 * rows in
  Grid.picture ~width:w ~height:h
    (String.init
       (4 * w * h)
       (fun i ->
         let x = i / 4 mod w and y = i / 4 / w in
         let r, g, b = tile_colors.((y / 20 * cols) + (x / 10)) in
         Char.chr (match i mod 4 with 0 -> r | 1 -> g | 2 -> b | _ -> 255)))

let picture_screen form =
  let r = Screen.create () in
  Screen.apply_capabilities r ~explicit_width:false
    ~explicit_cursor_positioning:false ~hyperlinks:true ~color_depth:`Truecolor
    ~graphics:form
    ~cell_size:(Some (10, 20));
  r

let rgb c =
  let r, g, b, _ = Ansi.Color.to_rgba c in
  (r, g, b)

let triple = Testable.(triple int int int)

(* The sixels of [s], as the cell they start at, from 1, and their pixels,
   each a register's color, [None] where none is drawn. *)
let sixels s =
  let re = Str.regexp "\027\\[\\([0-9]+\\);\\([0-9]+\\)H\027P0;1;0q" in
  let rec all pos acc =
    match Str.search_forward re s pos with
    | exception Not_found -> List.rev acc
    | _ ->
        let row = int_of_string (Str.matched_group 1 s)
        and col = int_of_string (Str.matched_group 2 s) in
        let start = Str.match_end () in
        let stop = Str.search_forward (Str.regexp_string "\027\\") s start in
        all stop
          (((row, col), decode (String.sub s start (stop - start))) :: acc)
  and decode body =
    (* Raster attributes, then registers, then bands. *)
    let w, h = Scanf.sscanf body "\"1;1;%d;%d" (fun w h -> (w, h)) in
    let px = Array.make_matrix h w None in
    let regs = Hashtbl.create 16 in
    let pct v = v * 255 / 100 in
    let i = ref (String.index body '#')
    and band = ref 0
    and x = ref 0
    and reg = ref 0 in
    let number () =
      let j = ref !i in
      while !j < String.length body && body.[!j] >= '0' && body.[!j] <= '9' do
        incr j
      done;
      let n = int_of_string (String.sub body !i (!j - !i)) in
      i := !j;
      n
    in
    let paint count c =
      let bits = Char.code c - 63 in
      for _ = 1 to count do
        for k = 0 to 5 do
          let y = (!band * 6) + k in
          if bits land (1 lsl k) <> 0 && y < h && !x < w then
            px.(y).(!x) <- Some (Hashtbl.find regs !reg)
        done;
        incr x
      done
    in
    while !i < String.length body do
      match body.[!i] with
      | '#' ->
          incr i;
          let n = number () in
          if !i < String.length body && body.[!i] = ';' then (
            incr i;
            let _ = number () in
            incr i;
            let r = number () in
            incr i;
            let g = number () in
            incr i;
            let b = number () in
            Hashtbl.replace regs n (pct r, pct g, pct b));
          reg := n
      | '$' ->
          x := 0;
          incr i
      | '-' ->
          x := 0;
          incr band;
          incr i
      | '!' ->
          incr i;
          let n = number () in
          paint n body.[!i];
          incr i
      | c ->
          paint 1 c;
          incr i
    done;
    px
  in
  all 0 []

let test_picture_blocks () =
  let r = picture_screen `Blocks and pic = tiled ~cols:4 ~rows:2 in
  let frame () =
    Screen.build r ~width:8 ~height:4 (fun g _ ->
        Grid.draw_picture g ~x:1 ~y:1 ~width:4 ~height:2 pic)
  in
  frame ();
  let vte = Vte.create ~rows:4 ~cols:8 () in
  ignore (Vte.feed_string vte (Screen.render r) : _ list);
  let g = Vte.grid vte in
  for i = 0 to 7 do
    let idx = ((1 + (i / 4)) * 8) + 1 + (i mod 4) in
    equal ~msg:"a half block" string "\u{2580}" (Grid.get_text g idx);
    equal ~msg:"upper half" triple tile_colors.(i) (rgb (Grid.get_fg g idx));
    equal ~msg:"lower half" triple tile_colors.(i) (rgb (Grid.get_bg g idx))
  done;
  frame ();
  equal ~msg:"an unchanged frame writes nothing" string "" (Screen.render r)

let test_picture_kitty () =
  let r = picture_screen `Kitty in
  let pic = tiled ~cols:2 ~rows:1 in
  let frame show =
    Screen.build r ~width:6 ~height:2 (fun g _ ->
        if show then Grid.draw_picture g ~x:1 ~y:0 ~width:2 ~height:1 pic)
  in
  let vte = Vte.create ~rows:2 ~cols:6 () in
  let show output = ignore (Vte.feed_string vte output : _ list) in
  frame true;
  let first = Screen.render r in
  show first;
  is_true ~msg:"the box's pixels, sent once"
    (Str.string_match (Str.regexp "\027_Ga=T,U=1,f=32,s=20,v=20,") first 0);
  equal ~msg:"placeholders of a held image" string " \u{FFFC}\u{FFFC}"
    (Vte.text vte 0);
  frame true;
  equal ~msg:"an unchanged frame writes nothing" string "" (Screen.render r);
  frame false;
  show (Screen.render r);
  frame false;
  let next = Screen.render r in
  is_true ~msg:"the image is deleted before the next frame"
    (Str.string_match (Str.regexp "\027_Ga=d,d=I,i=[0-9]+,q=2\027\\\\$") next 0);
  frame true;
  show (Screen.render r);
  equal ~msg:"sent again" string " \u{FFFC}\u{FFFC}" (Vte.text vte 0);
  Screen.invalidate_presented r;
  frame true;
  let again = Screen.render r in
  is_true ~msg:"after an invalidation, deleted and sent again"
    (Str.string_match
       (Str.regexp "\027_Ga=d,d=I,i=[0-9]+,q=2\027\\\\\027_Ga=T")
       again 0)

(* A sixel covers only the picture cells the frame wrote, in rectangles, and
   never the last row, which keeps its half blocks. *)
let test_picture_sixel () =
  let r = picture_screen `Sixel in
  let pic = tiled ~cols:4 ~rows:2 in
  let frame ?(over = false) () =
    Screen.build r ~width:8 ~height:3 (fun g _ ->
        Grid.draw_picture g ~x:1 ~y:1 ~width:4 ~height:2 pic;
        if over then Grid.draw_text g ~x:2 ~y:1 ~text:"x")
  in
  frame ();
  let first = Screen.render r in
  (match sixels first with
  | [ ((2, 2), px) ] ->
      equal ~msg:"one cell high, above the last row" int 20 (Array.length px);
      equal ~msg:"four cells wide" int 40 (Array.length px.(0));
      for u = 0 to 39 do
        for v = 0 to 19 do
          equal ~msg:"each pixel is its tile's color" (option triple)
            (Some tile_colors.(u / 10))
            px.(v).(u)
        done
      done
  | l -> failf "%d sixels" (List.length l));
  let vte = Vte.create ~rows:3 ~cols:8 () in
  ignore (Vte.feed_string vte first : _ list);
  equal ~msg:"the last row in half blocks" triple tile_colors.(4)
    (rgb (Grid.get_fg (Vte.grid vte) ((2 * 8) + 1)));
  frame ();
  equal ~msg:"an unchanged frame writes nothing" string "" (Screen.render r);
  frame ~over:true ();
  equal ~msg:"text over a picture draws no sixel" int 0
    (List.length (sixels (Screen.render r)));
  frame ();
  match sixels (Screen.render r) with
  | [ ((2, 3), px) ] ->
      equal ~msg:"only the cell uncovered" int 10 (Array.length px.(0));
      equal ~msg:"its tile" (option triple) (Some tile_colors.(1)) px.(0).(0)
  | l -> failf "%d sixels" (List.length l)

(* Static rows with pictures are written as rows, then a sixel over the rows
   still on the screen, then the cursor back below them. *)
let test_picture_static_rows () =
  let r = picture_screen `Sixel in
  let g = Grid.create ~width:4 ~height:2 () in
  equal ~msg:"rows without pictures are to_ansi's" string
    (Grid.to_ansi ~complete_rows:true ~rows:2 g)
    (Screen.static_rows r g ~rows:2 ~top:0);
  Grid.draw_picture g ~x:0 ~y:0 ~width:2 ~height:2 (tiled ~cols:2 ~rows:2);
  let s = Screen.static_rows r g ~rows:2 ~top:(-1) in
  (match sixels s with
  | [ ((1, 1), px) ] ->
      equal ~msg:"the row on the screen" int 20 (Array.length px)
  | l -> failf "%d sixels" (List.length l));
  is_true ~msg:"the cursor below the rows"
    (String.ends_with ~suffix:"\027\\\027[2;1H" s)

let () =
  run "matrix.screen"
    [
      group "Bytes per change" [ test "a 160x48 frame" bytes_per_change ];
      group "Core Rendering"
        [
          test "Create renderer" test_create_renderer;
          test "Zero-sized frame" test_zero_sized_frame;
          test "Single cell frame" test_single_cell_frame;
          test "Simple text rendering" test_simple_text_rendering;
          test "Hyperlink rendering" test_hyperlink_rendering;
          test "Row offset applied" test_row_offset_applied;
          test "Viewport expansion renders new rows"
            test_viewport_expansion_renders_new_rows;
          test "Viewport clips active hit grid"
            test_viewport_clips_active_hit_grid;
          test "Styled frame resets SGR" test_styled_frame_resets_sgr;
          test "Color depth reaches SGR emission"
            test_color_depth_reaches_sgr_emission;
        ];
      group "Diff Algorithm"
        [
          test "Diff only changed cells" test_diff_only_changed_cells;
          test "No diff when unchanged" test_no_diff_when_unchanged;
          test "Wide character diff" test_wide_char_diff;
          test "Changed grapheme diff" test_changed_grapheme_diff;
          test "Changed hyperlink diff" test_changed_hyperlink_diff;
          test "Null cell clear uses cell colors"
            test_null_cell_clear_uses_cell_colors;
          test "All cells changed" test_all_cells_changed;
          test "Partial row update" test_partial_row_update;
          test "Color only change" test_color_only_change;
          test "Attribute only change" test_attribute_only_change;
          test "Adjacent cells stay apart" test_adjacent_cells_stay_apart;
          test "A full render writes empty cells"
            test_full_render_writes_empty_cells;
          test "Empty cells reset the rendition"
            test_empty_cells_reset_the_rendition;
        ];
      group "Frame Building"
        [
          test "handed-out grid is cleared once"
            test_handed_out_grid_is_cleared_once;
          test "Build visual (no hits)" test_build_visual;
          test "A render without a build is blank"
            test_render_without_build_is_blank;
          test "Resize repaints every cell" test_resize_repaints_every_cell;
          test "Resize smaller" test_resize_smaller;
          test "Resize clears both buffers" test_resize_clears_both_buffers;
          test "Resize clears pending next buffer"
            test_resize_clears_pending_next_buffer;
          test "Resize same size preserves diff baseline"
            test_resize_same_size_preserves_diff_baseline;
          test "Cursor clamped on resize" test_cursor_clamped_on_resize;
        ];
      group "Hit Grid Integration"
        [
          test "Hit grid integration" test_hit_grid_integration;
          test "Hit grid draw" test_hit_grid_draw;
          test "Hit grid cleared each frame" test_hit_grid_cleared_each_frame;
          test "Hit grid swap happens on render" test_hit_grid_swap_on_render;
          test "Add hit region helper" test_add_hit_region_helper;
          test "A 1x1 frame keeps its hit regions"
            test_single_cell_frame_keeps_hits;
        ];
      group "Statistics" [ test "Stats tracking" test_stats_tracking ];
      group "Configuration"
        [
          test "Update config" test_update_config;
          test "Reset" test_reset;
          test "Reset triggers diff" test_reset_triggers_next_diff;
        ];
      group "Edge Cases"
        [
          test "Extremely wide characters" test_extremely_wide_char;
          test "Buffer overflow prevention" test_buffer_overflow_prevention;
          test "Render into a full writer commits nothing"
            test_render_into_full_writer;
          test "Render into caller writer appends and commits once"
            test_render_into_appends_and_commits_once;
          test "Large hyperlinked Screen.render succeeds"
            test_render_large_hyperlinked_frame_succeeds;
          test "Resize triggers full redraw" test_resize_full_redraw;
          test "Resize clears hit grids" test_resize_hit_grid_cleared;
          test "Wide grapheme emission" test_wide_grapheme_emission;
          test "Explicit width sequences" test_explicit_width_sequences;
          test "Turning explicit width off repositions the cursor"
            test_explicit_width_off_repositions;
          test "Hyperlink capability gating" test_hyperlink_capability_gating;
          test "Scroll hint clips to viewport"
            test_scroll_hint_clips_to_viewport;
          test "Scroll hint applies row offset"
            test_scroll_hint_applies_row_offset;
          test "Scroll hint by the whole region is ignored"
            test_scroll_hint_by_whole_region_is_ignored;
          test "Cursor style and color" test_cursor_style_and_color;
        ];
      group "Pictures"
        [
          test "Half blocks" test_picture_blocks;
          test "Kitty placeholders" test_picture_kitty;
          test "Sixel over half blocks" test_picture_sixel;
          test "Static rows" test_picture_static_rows;
        ];
      group "Performance Characteristics"
        [
          test "Double buffer reuse" test_double_buffer_reuse;
          test "Transform before render" test_transform_before_render;
        ];
    ]
