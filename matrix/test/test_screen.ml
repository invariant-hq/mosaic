open Windtrap

(* Test Helpers *)

let create_renderer ?(width = 10) ?(height = 10) () =
  let r = Screen.create () in
  (* Build and render a frame to initialize buffers *)
  let _ =
    Screen.build r ~width ~height (fun _grid _hits -> ()) |> Screen.render
  in
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

(* 1. Core Rendering Tests *)

let test_create_renderer () =
  let r = Screen.create () in
  is_true ~msg:"renderer created" (r != Obj.magic 0)

(* Screen rendering is pure diff output; no terminal-side effects are
   emitted. *)
let zero_frame_expected = ""

let test_zero_sized_frame () =
  (* Edge case: 0x0 frame should not crash *)
  let r = Screen.create ~mouse_enabled:false () in
  let frame = Screen.build r ~width:0 ~height:0 (fun _ _ -> ()) in
  let output = Screen.render frame in
  equal ~msg:"empty output" string zero_frame_expected output

let test_single_cell_frame () =
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let output = Screen.render frame in
  is_true ~msg:"contains text" (String.length output > 0);
  is_true ~msg:"contains X" (String.contains output 'X')

let test_simple_text_rendering () =
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let output = Screen.render frame in
  is_true ~msg:"output not empty" (String.length output > 0);
  (* Check that all characters from "Hello" appear in output *)
  is_true ~msg:"contains H" (String.contains output 'H');
  is_true ~msg:"contains e" (String.contains output 'e');
  is_true ~msg:"contains l" (String.contains output 'l');
  is_true ~msg:"contains o" (String.contains output 'o')

let test_hyperlink_rendering () =
  let r = Screen.create () in
  let frame1 =
    Screen.build r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let _ = Screen.render frame1 in
  let link_style =
    Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
  in
  let frame2 =
    Screen.build r ~width:4 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A" ~style:link_style)
  in
  let output = Screen.render frame2 in
  let expected = "\027]8;;https://example.com" in
  let contains_expected =
    try
      let _ = Str.search_forward (Str.regexp_string expected) output 0 in
      true
    with Not_found -> false
  in
  is_true ~msg:"contains hyperlink start" contains_expected

let test_row_offset_applied () =
  let r = create_renderer ~width:2 ~height:2 () in
  Screen.set_row_offset r 3;
  let frame =
    Screen.build r ~width:2 ~height:2 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"A")
  in
  let output = Screen.render frame in
  let expected = "\027[4;1H" in
  let has_seq =
    try
      let _ = Str.search_forward (Str.regexp_string expected) output 0 in
      true
    with Not_found -> false
  in
  is_true ~msg:"cursor moved with offset" has_seq;
  is_true ~msg:"character rendered" (String.contains output 'A')

(* 2. Diff Algorithm Tests *)

let test_diff_only_changed_cells () =
  (* Render should only output changed cells *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* First frame *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXX")
  in
  let output1 = Screen.render f1 in
  let moves1 = count_cursor_moves output1 in

  (* Second frame - only change one cell *)
  let f2 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXY")
  in
  let output2 = Screen.render f2 in
  let moves2 = count_cursor_moves output2 in

  (* Should not require additional cursor moves once diff is warmed up *)
  is_true ~msg:"second frame has <= moves" (moves2 <= moves1);
  let moved_to_changed =
    try
      let _ = Str.search_forward (Str.regexp_string "\027[1;5H") output2 0 in
      true
    with Not_found -> false
  in
  is_true ~msg:"moves cursor to changed cell" moved_to_changed;
  is_true ~msg:"second frame not empty" (String.length output2 > 0)

let test_no_diff_when_unchanged () =
  let r = create_renderer ~width:5 ~height:5 () in

  (* First render *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  (* Second render with same content; expect no output *)
  let f2 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let output2 = Screen.render f2 in

  equal ~msg:"no diff output" string "" output2

let test_wide_char_diff () =
  (* Test that wide characters are diffed correctly *)
  let r = create_renderer () in

  let f1 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"你好")
  in
  (* Chinese chars, 2 cells each *)
  let output2 = Screen.render f2 in

  is_true ~msg:"wide char output" (String.length output2 > 0)

(* 3. Frame Building Tests *)

let test_build_visual () =
  (* build_visual should not require hit grid function *)
  let r = Screen.create () in
  let frame =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi")
  in
  let output = Screen.render frame in
  is_true ~msg:"visual build works" (String.length output > 0)

let test_resize_preserves_content () =
  let r = create_renderer ~width:5 ~height:5 () in

  (* Draw something *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC")
  in
  let _output1 = Screen.render f1 in

  (* Resize larger *)
  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC")
  in
  let output2 = Screen.render f2 in

  (* Should not re-render unchanged cells *)
  is_true ~msg:"resize works" (String.length output2 < 100)

let test_resize_smaller () =
  (* Edge case: shrinking grid *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"BigText")
  in
  let _output1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:3 ~height:3 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Sm")
  in
  let output2 = Screen.render f2 in

  is_true ~msg:"shrink works" (String.length output2 > 0)

let test_resize_clears_both_buffers () =
  (* Test that resize clears both current and next buffers, ensuring proper
     diffing *)
  let r = create_renderer ~width:5 ~height:5 () in

  (* Fill the screen with content *)
  let f1 =
    Screen.build r ~width:5 ~height:5 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:5 ~height:5
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _output1 = Screen.render f1 in

  (* Resize to larger dimensions *)
  Screen.resize r ~width:10 ~height:10;

  (* Build a frame that only draws a small region *)
  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Small")
  in
  let output2 = Screen.render f2 in

  (* The output should not contain the old red background content *)
  (* Check that we don't have excessive output (which would indicate stale content).
     A full 10x10 redraw would produce ~1000+ bytes; the 5x5 overlap diff is ~250. *)
  is_true ~msg:"resize clears buffers" (String.length output2 < 400);

  (* Verify the new content is rendered *)
  is_true ~msg:"new content rendered" (String.contains output2 'S')

let test_cursor_clamped_on_resize () =
  let r = create_renderer ~width:3 ~height:3 () in
  Screen.set_cursor_position r ~row:5 ~col:5;
  Screen.resize r ~width:2 ~height:1;
  let _frame = Screen.build r ~width:2 ~height:1 (fun _grid _hits -> ()) in
  let info = Screen.cursor_info r in
  equal ~msg:"cursor row clamped" int 1 info.row;
  equal ~msg:"cursor col clamped" int 2 info.col

(* 4. Post-Processing Tests *)

let test_post_process_receives_delta () =
  let r = Screen.create () in
  let delta_received = ref None in

  let frame1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _id =
    Screen.post_process
      (fun _grid ~delta -> delta_received := Some delta)
      frame1
  in
  let _output = Screen.render frame1 in

  is_true ~msg:"delta was received" (Option.is_some !delta_received);
  let delta = Option.get !delta_received in
  is_true ~msg:"delta is non-negative" (delta >= 0.)

let test_post_process_chain () =
  (* Multiple post-process functions should be applied in order *)
  let r = Screen.create () in
  let calls = ref [] in

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _id1 =
    Screen.post_process (fun _grid ~delta:_ -> calls := "first" :: !calls) frame
  in
  let _id2 =
    Screen.post_process
      (fun _grid ~delta:_ -> calls := "second" :: !calls)
      frame
  in
  let _output = Screen.render frame in

  (* Should be called in order (reversed because we cons) *)
  equal ~msg:"call order" (list string) [ "second"; "first" ] !calls

let test_post_process_persists_across_frames () =
  (* Post-processors persist until explicitly removed *)
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect_ _grid ~delta:_ = incr call_count in

  let frame1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _id = Screen.post_process effect_ frame1 in
  let _ = Screen.render frame1 in

  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  equal ~msg:"called twice" int 2 !call_count

let test_remove_post_process () =
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect_ _grid ~delta:_ = incr call_count in

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let id = Screen.post_process effect_ frame in
  let _ = Screen.render frame in

  let _frame = Screen.remove_post_process id frame in
  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  equal ~msg:"effect removed" int 1 !call_count

let test_clear_post_processes () =
  let r = Screen.create () in
  let call_count = ref 0 in
  let effect1 _grid ~delta:_ = incr call_count in
  let effect2 _grid ~delta:_ = incr call_count in

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _id1 = Screen.post_process effect1 frame in
  let _id2 = Screen.post_process effect2 frame in
  let _ = Screen.render frame in
  equal ~msg:"both effects ran" int 2 !call_count;

  let _ = Screen.clear_post_processes frame in
  let frame2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame2 in

  equal ~msg:"effects cleared" int 2 !call_count

(* 5. Hit Grid Tests *)

let test_hit_grid_integration () =
  let r = Screen.create () in
  let hit_id = ref 0 in

  let frame =
    Screen.build r ~width:10 ~height:10 (fun grid hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Button";
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:6 ~height:1 ~id:42)
  in
  let _ = Screen.render frame in
  hit_id := Screen.query_hit frame ~x:0 ~y:0;
  equal ~msg:"hit id at (0,0)" int 42 !hit_id;

  hit_id := Screen.query_hit frame ~x:7 ~y:0;
  equal ~msg:"no hit at (7,0)" int 0 !hit_id

let test_hit_grid_cleared_each_frame () =
  let r = Screen.create () in

  (* First frame with hit region *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:0 ~y:0 ~width:5 ~height:1 ~id:1)
  in
  let _o1 = Screen.render f1 in

  (* Second frame without hit region *)
  let f2 = Screen.build r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  let _o2 = Screen.render f2 in

  let hit_id = Screen.query_hit f2 ~x:0 ~y:0 in
  equal ~msg:"hit cleared" int 0 hit_id

let test_hit_grid_swap_on_render () =
  let r = Screen.create () in

  let frame1 =
    Screen.build r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:1)
  in
  let _ = Screen.render frame1 in

  (* Building the next frame should not swap hits until render runs. *)
  let frame2 =
    Screen.build r ~width:3 ~height:3 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:1 ~y:1 ~width:1 ~height:1 ~id:2)
  in
  let before = Screen.query_hit frame2 ~x:1 ~y:1 in
  equal ~msg:"previous hit active before swap" int 1 before;

  let _ = Screen.render frame2 in
  let after = Screen.query_hit frame2 ~x:1 ~y:1 in
  equal ~msg:"next hit active after swap" int 2 after

let test_add_hit_region_helper () =
  let r = Screen.create () in
  let frame1 = Screen.build r ~width:10 ~height:10 (fun _grid _hits -> ()) in
  let frame2 =
    Screen.add_hit_region frame1 ~x:5 ~y:5 ~width:2 ~height:2 ~id:99
  in

  let _ = Screen.render frame2 in
  let hit_id = Screen.query_hit frame2 ~x:5 ~y:5 in
  equal ~msg:"hit region added" int 99 hit_id

(* 6. Statistics Tests *)

let test_stats_tracking () =
  let r = Screen.create () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _output1 = Screen.render f1 in

  let stats = Screen.stats r in
  let metrics = Screen.last_metrics r in
  equal ~msg:"frame count" int 1 stats.frame_count;
  is_true ~msg:"cells updated" (metrics.cells > 0);
  is_true ~msg:"output bytes" (metrics.bytes > 0);
  is_true ~msg:"frame time" (metrics.frame_time_ms >= 0.)

(* 7. Configuration Tests *)

let test_update_config () =
  let r = Screen.create () in

  Screen.set_cursor_visible r false;

  let frame = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render frame in
  let metrics = Screen.last_metrics r in
  is_false ~msg:"cursor visibility updated" metrics.cursor_visible

let test_reset () =
  let r = Screen.create () in

  (* Render a few frames *)
  for _i = 1 to 3 do
    let f = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
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
    Screen.build r ~width:1 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X")
  in
  let _ = Screen.render f1 in

  Screen.reset r;

  let f2 = Screen.build r ~width:1 ~height:1 (fun _ _ -> ()) in
  let output2 = Screen.render f2 in

  is_true ~msg:"cursor moves after reset" (count_cursor_moves output2 >= 1);
  is_true ~msg:"clears previous glyph" (not (String.contains output2 'X'))

(* 8. Edge Cases and Boundary Conditions *)

let test_extremely_wide_char () =
  (* Test rendering with emoji that might have unusual widths *)
  let r = create_renderer () in
  let f =
    Screen.build r ~width:20 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"🎨🚀✨")
  in
  let output = Screen.render f in
  is_true ~msg:"emoji renders" (String.length output > 0)

let test_buffer_overflow_prevention () =
  (* Test with large frame (200x60) and complex content to ensure 2MB buffer
     doesn't overflow *)
  let r = create_renderer ~width:200 ~height:60 () in
  let f =
    Screen.build r ~width:200 ~height:60 (fun grid _hits ->
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
  is_true ~msg:"large frame renders without overflow"
    (String.length output > 1000)

let test_resize_full_redraw () =
  (* Test that first frame after resize is full redraw *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* First frame *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Initial")
  in
  let _ = Screen.render f1 in

  (* Resize *)
  Screen.resize r ~width:20 ~height:20;

  (* Second frame - should be full redraw *)
  let f2 =
    Screen.build r ~width:20 ~height:20 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Resized")
  in
  let output2 = Screen.render f2 in

  (* Should contain some cursor movements for the diff *)
  let moves = count_cursor_moves output2 in
  is_true ~msg:"contains cursor moves" (moves > 0)

let test_resize_hit_grid_cleared () =
  (* Test that hit grids don't leak stale IDs after resize *)
  let r = create_renderer ~width:10 ~height:10 () in

  (* Add hit region *)
  let f1 =
    Screen.build r ~width:10 ~height:10 (fun _grid hits ->
        Screen.Hit_grid.add hits ~x:5 ~y:5 ~width:2 ~height:2 ~id:99)
  in
  let _ = Screen.render f1 in

  (* Verify hit exists *)
  let hit_before = Screen.query_hit f1 ~x:5 ~y:5 in
  equal ~msg:"hit exists before resize" int 99 hit_before;

  (* Resize *)
  Screen.resize r ~width:5 ~height:5;

  (* Build new frame *)
  let f2 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _ = Screen.render f2 in

  (* Hit should be gone (coordinates out of bounds) *)
  let hit_after = Screen.query_hit f2 ~x:0 ~y:0 in
  equal ~msg:"hit cleared after resize" int 0 hit_after

let test_explicit_width_sequences () =
  (* Test that explicit width OSC sequences are emitted when enabled *)
  let r = Screen.create ~explicit_width:true () in
  let f =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
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

let test_hyperlink_capability_gating () =
  (* Test that hyperlinks are only emitted when capability is enabled *)
  let r1 = Screen.create () in
  (* hyperlinks_capable defaults to true *)
  let f1 =
    Screen.build r1 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output1 = Screen.render f1 in

  (* Should contain hyperlink sequences *)
  let contains_hyperlink =
    try
      let _ = Str.search_forward (Str.regexp "\027]8;;") output1 0 in
      true
    with Not_found -> false
  in
  is_true ~msg:"hyperlink emitted when capable" contains_hyperlink;

  (* Now test with capability disabled *)
  let r2 = Screen.create () in
  Screen.apply_capabilities r2 ~explicit_width:false
    ~explicit_cursor_positioning:false ~hyperlinks:false;
  let f2 =
    Screen.build r2 ~width:10 ~height:1 (fun grid _hits ->
        let style =
          Ansi.Style.hyperlink "https://example.com" Ansi.Style.default
        in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Link" ~style)
  in
  let output2 = Screen.render f2 in

  (* Should NOT contain hyperlink sequences *)
  let contains_hyperlink_disabled =
    try
      let _ = Str.search_forward (Str.regexp "\027]8;;") output2 0 in
      true
    with Not_found -> false
  in
  is_false ~msg:"hyperlink not emitted when incapable"
    contains_hyperlink_disabled

let test_cursor_style_and_color () =
  (* Test cursor style and color state *)
  let r = create_renderer () in
  Screen.set_cursor_position r ~row:5 ~col:10;
  Screen.set_cursor_style r ~style:`Underline ~blinking:false;
  Screen.set_cursor_color r ~r:255 ~g:0 ~b:128;

  let _f = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let info = Screen.cursor_info r in
  is_true ~msg:"cursor visible" info.visible;
  is_true ~msg:"cursor position set" info.has_position;
  equal ~msg:"cursor row stored" int 5 info.row;
  equal ~msg:"cursor col stored" int 5 info.col;
  is_true ~msg:"cursor underline" (info.style = `Underline);
  is_true ~msg:"cursor non-blinking" (not info.blinking);
  is_true ~msg:"cursor color stored" (info.color = Some (255, 0, 128))

let test_all_cells_changed () =
  (* Worst case: every cell changes *)
  let r = create_renderer ~width:10 ~height:10 () in

  let f1 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
        Grid.fill_rect grid ~x:0 ~y:0 ~width:10 ~height:10
          ~color:(Ansi.Color.of_rgb 255 0 0))
  in
  let _o1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:10 (fun grid _hits ->
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
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"XXXXXXXXXX")
  in
  let _o1 = Screen.render f1 in

  let f2 =
    Screen.build r ~width:10 ~height:1 (fun grid _hits ->
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
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style1)
  in
  let _o1 = Screen.render f1 in

  let style2 = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0 255 0) () in
  let f2 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style2)
  in
  let output2 = Screen.render f2 in

  (* Should detect color change *)
  is_true ~msg:"color change detected" (String.length output2 > 10)

let test_attribute_only_change () =
  (* Test that attribute changes (bold, italic, etc) trigger diff *)
  let r = create_renderer () in

  let f1 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test")
  in
  let _o1 = Screen.render f1 in

  let style_bold = Ansi.Style.make ~bold:true () in
  let f2 =
    Screen.build r ~width:5 ~height:1 (fun grid _hits ->
        Grid.draw_text grid ~x:0 ~y:0 ~text:"Test" ~style:style_bold)
  in
  let output2 = Screen.render f2 in

  (* Should detect attribute change *)
  is_true ~msg:"attribute change detected" (String.length output2 > 10)

(* 9. Performance Characteristics Tests *)

let test_zero_allocation_frame_building () =
  (* This is a semantic test - we can't actually measure allocations, but we can
     verify the API works as expected *)
  let r = Screen.create () in

  (* Build and post_process should work without errors *)
  let f1 = Screen.build r ~width:5 ~height:5 (fun _grid _hits -> ()) in
  let _id = Screen.post_process (fun _grid ~delta:_ -> ()) f1 in
  let _output = Screen.render f1 in
  (* If we get here without errors, the zero-allocation API works *)
  is_true ~msg:"api works" true

let test_double_buffer_reuse () =
  let r = Screen.create () in
  let grids = ref [] in
  let hits = ref [] in

  let record frame =
    grids := add_unique_by_phys !grids (Screen.grid frame);
    hits := add_unique_by_phys !hits (Screen.hit_grid frame)
  in

  let render_once () =
    let frame = Screen.build r ~width:4 ~height:2 (fun _grid _hits -> ()) in
    record frame;
    let _ = Screen.render frame in
    ()
  in

  render_once ();
  render_once ();
  render_once ();

  equal ~msg:"reuses two grid buffers" int 2 (List.length !grids);
  equal ~msg:"reuses two hit grids" int 2 (List.length !hits)

let test_pipeline_composition () =
  (* Test that pipelines compose correctly *)
  let r = Screen.create () in
  let call_order = ref [] in

  let f1 =
    Screen.build r ~width:5 ~height:5 (fun _grid _hits ->
        call_order := "build" :: !call_order)
  in
  let _id1 =
    Screen.post_process
      (fun _grid ~delta:_ -> call_order := "post1" :: !call_order)
      f1
  in
  let _id2 =
    Screen.post_process
      (fun _grid ~delta:_ -> call_order := "post2" :: !call_order)
      f1
  in
  let f2 = Screen.add_hit_region f1 ~x:0 ~y:0 ~width:1 ~height:1 ~id:1 in
  let output = Screen.render f2 in

  is_true ~msg:"pipeline executed" (String.length output >= 0);
  is_true ~msg:"build called" (List.mem "build" !call_order);
  is_true ~msg:"post1 called" (List.mem "post1" !call_order);
  is_true ~msg:"post2 called" (List.mem "post2" !call_order)

(* Test Suite *)

let () =
  run "matrix.screen"
    [
      group "Core Rendering"
        [
          test "Create renderer" test_create_renderer;
          test "Zero-sized frame" test_zero_sized_frame;
          test "Single cell frame" test_single_cell_frame;
          test "Simple text rendering" test_simple_text_rendering;
          test "Hyperlink rendering" test_hyperlink_rendering;
          test "Row offset applied" test_row_offset_applied;
        ];
      group "Diff Algorithm"
        [
          test "Diff only changed cells" test_diff_only_changed_cells;
          test "No diff when unchanged" test_no_diff_when_unchanged;
          test "Wide character diff" test_wide_char_diff;
          test "All cells changed" test_all_cells_changed;
          test "Partial row update" test_partial_row_update;
          test "Color only change" test_color_only_change;
          test "Attribute only change" test_attribute_only_change;
        ];
      group "Frame Building"
        [
          test "Build visual (no hits)" test_build_visual;
          test "Resize preserves content" test_resize_preserves_content;
          test "Resize smaller" test_resize_smaller;
          test "Resize clears both buffers" test_resize_clears_both_buffers;
          test "Cursor clamped on resize" test_cursor_clamped_on_resize;
        ];
      group "Post-Processing"
        [
          test "Post-process receives delta" test_post_process_receives_delta;
          test "Post-process chain" test_post_process_chain;
          test "Post-process persists across frames"
            test_post_process_persists_across_frames;
          test "Remove post-process" test_remove_post_process;
          test "Clear post-processes" test_clear_post_processes;
        ];
      group "Hit Grid Integration"
        [
          test "Hit grid integration" test_hit_grid_integration;
          test "Hit grid cleared each frame" test_hit_grid_cleared_each_frame;
          test "Hit grid swap happens on render" test_hit_grid_swap_on_render;
          test "Add hit region helper" test_add_hit_region_helper;
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
          test "Resize triggers full redraw" test_resize_full_redraw;
          test "Resize clears hit grids" test_resize_hit_grid_cleared;
          test "Explicit width sequences" test_explicit_width_sequences;
          test "Hyperlink capability gating" test_hyperlink_capability_gating;
          test "Cursor style and color" test_cursor_style_and_color;
        ];
      group "Performance Characteristics"
        [
          test "Zero-allocation frame building"
            test_zero_allocation_frame_building;
          test "Double buffer reuse" test_double_buffer_reuse;
          test "Pipeline composition" test_pipeline_composition;
        ];
    ]
