open Windtrap
open Mosaic_ui
open Test_harness

(* ── Helpers ── *)

module Layout = Matrix_text.Layout

let row_text layout row =
  let out = Buffer.create 32 in
  let source = Layout.source layout in
  Layout.iter_cells layout ~row ~first_column:0 ~last_column:max_int
    (fun ~first ~last ~column:_ ~width:_ ~ellipsis ->
      Buffer.add_string out
        (if ellipsis then "…" else Layout.Source.slice source ~first ~last));
  Buffer.contents out

let make_surface ?(width = 40) ?(height = 10) ?content ?wrap () =
  let t = make_ctx () in
  let root = make_root t in
  let node = Renderable.create ~parent:root () in
  layout_node node ~x:0 ~y:0 ~width ~height;
  let buffer = Text_buffer.create () in
  (match content with Some s -> Text_buffer.set_text buffer s | None -> ());
  let surface = Text_surface.create node buffer in
  (match wrap with
  | Some mode -> Text_surface.set_wrap surface mode
  | None -> ());
  surface

(* ── Construction ── *)

let width_cache_reuses_and_releases () =
  let surface = make_surface ~wrap:`Char ~content:(String.make 100 'x') () in
  Text_surface.set_wrap_width surface (Some 5);
  let first = Text_surface.layout surface in
  Text_surface.set_wrap_width surface (Some 7);
  ignore (Text_surface.layout surface : Layout.t);
  Text_surface.set_wrap_width surface (Some 5);
  is_true (first == Text_surface.layout surface);
  let old = Weak.create 1 in
  let retain_old () = Weak.set old 0 (Some (Text_surface.layout surface)) in
  retain_old ();
  Text_buffer.set_text (Text_surface.buffer surface) "replacement";
  ignore (Text_surface.layout surface : Layout.t);
  ignore (Sys.opaque_identity first);
  (* Check reachability after this scope drops the explicit geometry reference. *)
  (surface, old)

let replacement_releases_geometry () =
  let surface, old = width_cache_reuses_and_releases () in
  Gc.full_major ();
  is_false (Weak.check old 0);
  ignore (Sys.opaque_identity surface)

let oversized_layout_is_released_on_width_switch () =
  (* At width 1, 40 MiB of rows: a built row takes two words. *)
  let length = 40 * 1024 * 1024 / (2 * (Sys.word_size / 8)) in
  let surface = make_surface ~wrap:`Char ~content:(String.make length 'x') () in
  Text_surface.set_wrap_width surface (Some 80);
  ignore (Text_surface.layout surface : Layout.t);
  let old = Weak.create 1 in
  Text_surface.set_wrap_width surface (Some 1);
  (* A layout builds a line's rows when they are asked: asking for the last
     row builds all of them. *)
  let shown layout =
    ignore (Layout.row_start layout (Layout.row_count layout - 1) : int);
    layout
  in
  Weak.set old 0 (Some (shown (Text_surface.layout surface)));
  Text_surface.set_wrap_width surface (Some 80);
  equal int (length / 80) (Text_surface.display_line_count surface);
  Gc.full_major ();
  is_false ~msg:"oversized inactive rows are collectible" (Weak.check old 0);
  ignore (Sys.opaque_identity surface)

let selection_updates_compare_normalized_ranges () =
  let surface = make_surface ~content:"abcdef" () in
  is_false ~msg:"empty to empty"
    (Text_surface.set_selection surface ~start:2 ~end_:2);
  is_true (Text_surface.set_selection surface ~start:1 ~end_:4);
  is_false ~msg:"same normalized range"
    (Text_surface.set_selection surface ~start:4 ~end_:1);
  Text_surface.reset_selection surface;
  is_false ~msg:"cleared stays empty"
    (Text_surface.set_selection surface ~start:0 ~end_:0)

let defaults () =
  let s = make_surface () in
  equal ~msg:"wrap" int 0
    (match Text_surface.wrap s with `None -> 0 | `Char -> 1 | `Word -> 2);
  equal ~msg:"scroll_x" int 0 (Text_surface.scroll_x s);
  equal ~msg:"scroll_y" int 0 (Text_surface.scroll_y s);
  is_false ~msg:"no selection" (Text_surface.has_selection s);
  is_false ~msg:"truncate" (Text_surface.truncate s)

(* ── Wrapping — None ── *)

let wrap_none_single_line () =
  let s = make_surface ~content:"hello world" () in
  equal ~msg:"display_line_count" int 1 (Text_surface.display_line_count s)

let wrap_none_multiline () =
  let s = make_surface ~content:"line1\nline2\nline3" () in
  equal ~msg:"display_line_count" int 3 (Text_surface.display_line_count s)

(* ── Wrapping — Char ── *)

let wrap_char_splits () =
  (* 10 chars wide, content is 20 chars -> should wrap to 2 display lines *)
  let s =
    make_surface ~width:10 ~content:"abcdefghijklmnopqrst" ~wrap:`Char ()
  in
  equal ~msg:"display_line_count" int 2 (Text_surface.display_line_count s)

let wrap_char_short_line_no_wrap () =
  let s = make_surface ~width:40 ~content:"short" ~wrap:`Char () in
  equal ~msg:"display_line_count" int 1 (Text_surface.display_line_count s)

let wrap_char_line_sources () =
  let s = make_surface ~width:5 ~content:"abcdefghij\nxy" ~wrap:`Char () in
  let info = Text_surface.layout s in
  (* "abcdefghij" wraps into 2 display lines, "xy" is 1 *)
  equal ~msg:"display_line_count" int 3 (Layout.row_count info);
  equal ~msg:"source 0" int 0 (Layout.logical_line info 0);
  equal ~msg:"source 1" int 0 (Layout.logical_line info 1);
  equal ~msg:"source 2" int 1 (Layout.logical_line info 2)

let wrap_char_wrap_indices () =
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Char () in
  let info = Text_surface.layout s in
  equal ~msg:"wrap_index 0" int 0 (Layout.continuation info 0);
  equal ~msg:"wrap_index 1" int 1 (Layout.continuation info 1)

(* ── Wrapping — Word ── *)

let wrap_word_at_boundary () =
  let s = make_surface ~width:10 ~content:"hello world foo" ~wrap:`Word () in
  let info = Text_surface.layout s in
  (* "hello " fits in 10, "world foo" doesn't all fit, should wrap *)
  ge ~msg:"wraps" ~than:2 (Layout.row_count info)

let wrap_word_preserves_sources () =
  let s =
    make_surface ~width:10 ~content:"hello world\nfoo bar baz" ~wrap:`Word ()
  in
  let info = Text_surface.layout s in
  (* First logical line wraps, second might wrap too *)
  let last_source = Layout.logical_line info (Layout.row_count info - 1) in
  equal ~msg:"last source is line 1" int 1 last_source

(* ── set_wrap ── *)

let set_wrap_changes_mode () =
  let s = make_surface ~content:"abcdefghij" ~width:5 () in
  Text_surface.set_wrap s `Char;
  equal ~msg:"display_line_count" int 2 (Text_surface.display_line_count s);
  Text_surface.set_wrap s `None;
  equal ~msg:"back to 1" int 1 (Text_surface.display_line_count s)

(* ── wrap_width ── *)

let explicit_wrap_width () =
  let s = make_surface ~width:40 ~content:"abcdefghij" ~wrap:`Char () in
  Text_surface.set_wrap_width s (Some 5);
  equal ~msg:"wraps at 5" int 2 (Text_surface.display_line_count s)

let wrap_width_none_derives_from_node () =
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Char () in
  Text_surface.set_wrap_width s None;
  equal ~msg:"wraps at node width" int 2 (Text_surface.display_line_count s)

(* ── Truncation ── *)

let truncate_long_line () =
  let s = make_surface ~width:5 ~content:"hello world" () in
  Text_surface.set_truncate s true;
  let info = Text_surface.layout s in
  (* Should still be 1 display line, but truncated *)
  equal ~msg:"1 display line" int 1 (Layout.row_count info);
  (* The text should end with ellipsis (U+2026 = \xe2\x80\xa6) *)
  equal ~msg:"truncated source with marker" string "hell…" (row_text info 0)

let truncate_short_line_unchanged () =
  let s = make_surface ~width:40 ~content:"hi" () in
  Text_surface.set_truncate s true;
  let info = Text_surface.layout s in
  equal ~msg:"no truncation" string "hi" (row_text info 0)

(* ── Display Info ── *)

let display_info_grapheme_offsets () =
  let s = make_surface ~content:"abc\ndef" () in
  let info = Text_surface.layout s in
  equal ~msg:"offset line 0" int 0 (Layout.row_start info 0);
  (* "abc" = 3 graphemes + 1 for newline = offset 4 for "def" *)
  equal ~msg:"offset line 1" int 4 (Layout.row_start info 1)

let display_info_max_line_width () =
  let s = make_surface ~content:"hi\nhello world\nbye" () in
  let info = Text_surface.layout s in
  equal ~msg:"max_line_width" int 11 (Layout.max_width info)

(* ── Viewport / Scroll ── *)

let scroll_x_clamps () =
  let s = make_surface ~width:5 ~content:"hello" () in
  Text_surface.set_scroll_x s (-10);
  equal ~msg:"clamped to 0" int 0 (Text_surface.scroll_x s);
  (* Content is 5 wide, viewport is 5 wide, max scroll_x = 0 *)
  Text_surface.set_scroll_x s 100;
  equal ~msg:"clamped to max" int 0 (Text_surface.scroll_x s)

let scroll_y_clamps () =
  let s = make_surface ~height:3 ~content:"a\nb\nc" () in
  Text_surface.set_scroll_y s (-5);
  equal ~msg:"clamped to 0" int 0 (Text_surface.scroll_y s);
  Text_surface.set_scroll_y s 100;
  equal ~msg:"clamped to max" int 0 (Text_surface.scroll_y s)

let scroll_height_equals_display_lines () =
  let s = make_surface ~content:"a\nb\nc\nd" () in
  equal ~msg:"scroll_height" int 4 (Text_surface.scroll_height s)

let scroll_width_equals_max_width () =
  let s = make_surface ~content:"hi\nhello world" () in
  equal ~msg:"scroll_width" int 11 (Text_surface.scroll_width s)

(* ── Selection — set_selection ── *)

let selection_set_and_get () =
  let s = make_surface ~content:"hello world" () in
  let changed = Text_surface.set_selection s ~start:2 ~end_:7 in
  is_true ~msg:"changed" changed;
  is_true ~msg:"has_selection" (Text_surface.has_selection s);
  equal ~msg:"selection"
    (option (pair int int))
    (Some (2, 7))
    (Text_surface.selection s)

let selection_clamps_to_bounds () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.set_selection s ~start:(-5) ~end_:100 in
  equal ~msg:"clamped"
    (option (pair int int))
    (Some (0, 5))
    (Text_surface.selection s)

let selection_normalized () =
  let s = make_surface ~content:"hello world" () in
  (* Reversed: end < start *)
  let _ = Text_surface.set_selection s ~start:7 ~end_:2 in
  equal ~msg:"normalized"
    (option (pair int int))
    (Some (2, 7))
    (Text_surface.selection s)

let selection_returns_true_when_changed () =
  let s = make_surface ~content:"hello" () in
  let c1 = Text_surface.set_selection s ~start:0 ~end_:3 in
  is_true ~msg:"first set" c1;
  let c2 = Text_surface.set_selection s ~start:0 ~end_:3 in
  is_false ~msg:"same again" c2

let selected_text_basic () =
  let s = make_surface ~content:"hello world" () in
  let _ = Text_surface.set_selection s ~start:6 ~end_:11 in
  equal ~msg:"text" string "world" (Text_surface.selected_text s)

let selected_text_empty_when_no_selection () =
  let s = make_surface ~content:"hello" () in
  equal ~msg:"empty" string "" (Text_surface.selected_text s)

(* ── Selection — reset ── *)

let reset_selection_clears () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.set_selection s ~start:0 ~end_:3 in
  Text_surface.reset_selection s;
  is_false ~msg:"not active" (Text_surface.has_selection s);
  equal ~msg:"empty text" string "" (Text_surface.selected_text s)

(* ── Selection — local ── *)

let set_local_selection_basic () =
  let s = make_surface ~width:40 ~content:"hello world" () in
  let changed =
    Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:5
      ~focus_y:0
  in
  is_true ~msg:"changed" changed;
  is_true ~msg:"has_selection" (Text_surface.has_selection s);
  (* Static drags occupy their endpoint cells, including the space at column 5. *)
  equal ~msg:"selected text" string "hello " (Text_surface.selected_text s)

let update_local_selection_extends () =
  let s = make_surface ~width:40 ~content:"hello world" () in
  let _ =
    Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:5
      ~focus_y:0
  in
  let changed =
    Text_surface.update_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:11
      ~focus_y:0
  in
  is_true ~msg:"changed" changed;
  equal ~msg:"full text" string "hello world" (Text_surface.selected_text s)

(* A drag that leaves the text above or below selects to its start or end. *)
let local_selection_past_the_rows () =
  let s = make_surface ~width:40 ~content:"ab\ncd" () in
  ignore
    (Text_surface.set_local_selection s ~anchor_x:1 ~anchor_y:0 ~focus_x:0
       ~focus_y:5);
  equal ~msg:"below" string "b\ncd" (Text_surface.selected_text s);
  ignore
    (Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:1 ~focus_x:5
       ~focus_y:(-1));
  equal ~msg:"above" string "ab\nc" (Text_surface.selected_text s)

(* ── Cache Invalidation ── *)

let invalidate_clears_cache () =
  let s = make_surface ~content:"hello" () in
  (* Access display info to populate cache *)
  let _ = Text_surface.layout s in
  (* Change buffer content directly *)
  Text_buffer.set_text (Text_surface.buffer s) "hello\nworld";
  Text_surface.invalidate s;
  equal ~msg:"updated" int 2 (Text_surface.display_line_count s)

let restyling_retains_geometry () =
  let s = make_surface ~width:4 ~wrap:`Char ~content:"a\195\169\tb\nc" () in
  let node = Text_surface.node s in
  layout_width node ~x:0 ~y:0 ~width:4;
  let buffer = Text_surface.buffer s in
  let source = Text_buffer.source buffer in
  let geometry = Text_surface.layout s in
  let version = Text_buffer.version buffer in
  ignore (Text_buffer.style_at buffer 0);
  let red = Ansi.Style.make ~fg:Ansi.Color.red () in
  let blue = Ansi.Style.make ~fg:Ansi.Color.blue () in
  is_false ~msg:"layout stays clean"
    (measured_again node (fun () ->
         (* Split a UTF-8 sequence across spans: only concatenated bytes
            define geometry. *)
         Text_buffer.set_styled_text buffer
           [
             { text = "a\195"; style = red };
             { text = "\169\tb\nc"; style = blue };
           ];
         Text_surface.invalidate s;
         equal ~msg:"paint-only version" int version
           (Text_buffer.version buffer);
         is_true ~msg:"indexed source retained"
           (source == Text_buffer.source buffer);
         is_true ~msg:"geometry retained" (geometry == Text_surface.layout s)));
  is_true ~msg:"new style applied"
    (Ansi.Style.equal red (fst (Text_buffer.style_at buffer 0)));
  Text_buffer.set_default_style buffer blue;
  Text_buffer.set_text buffer "a\195\169\tb\nc";
  Text_surface.invalidate s;
  is_true ~msg:"plain restyle retains geometry"
    (geometry == Text_surface.layout s);
  is_true ~msg:"plain style applied"
    (Ansi.Style.equal blue (fst (Text_buffer.style_at buffer 0)));
  is_true ~msg:"content marks layout dirty"
    (measured_again node (fun () ->
         Text_buffer.set_styled_text buffer
           [ { text = "a\195\169\tb\nd"; style = red } ];
         Text_surface.invalidate s;
         is_true ~msg:"different bytes invalidate geometry"
           (geometry != Text_surface.layout s)))

let reading_new_geometry_does_not_skip_layout_invalidation () =
  let s = make_surface ~content:"hello" () in
  let node = Text_surface.node s in
  layout_width node ~x:0 ~y:0 ~width:40;
  ignore (Text_surface.layout s);
  is_true ~msg:"reading geometry cannot acknowledge intrinsic-size changes"
    (measured_again node (fun () ->
         Text_buffer.set_text (Text_surface.buffer s) "hello\nworld";
         equal int 2 (Text_surface.display_line_count s);
         Text_surface.invalidate s))

let buffer_version_triggers_recompute () =
  let s = make_surface ~content:"hello" () in
  let _ = Text_surface.layout s in
  Text_buffer.set_text (Text_surface.buffer s) "ab\ncd\nef";
  let info = Text_surface.layout s in
  equal ~msg:"3 lines" int 3 (Layout.row_count info)

let buffer_tab_width_triggers_recompute () =
  let s = make_surface ~content:"a\tb" () in
  let info = Text_surface.layout s in
  equal ~msg:"width with default tab" int 4 (Layout.max_width info);
  Text_buffer.set_tab_width (Text_surface.buffer s) 8;
  let info = Text_surface.layout s in
  equal ~msg:"width with updated tab" int 10 (Layout.max_width info)

(* ── Measurement ── *)

let call_measure surface ~available_width ~available_height =
  let node = Text_surface.node surface in
  match Renderable.measure node with
  | None -> failwith "no measure function registered"
  | Some m ->
      m
        ~known_dimensions:{ Toffee.Geometry.Size.width = None; height = None }
        ~available_space:
          {
            Toffee.Geometry.Size.width = available_width;
            height = available_height;
          }
        ~style:Toffee.Style.default

let call_measure_with_known surface ~known_w ~known_h ~available_width
    ~available_height =
  let node = Text_surface.node surface in
  match Renderable.measure node with
  | None -> failwith "no measure function registered"
  | Some m ->
      m
        ~known_dimensions:
          { Toffee.Geometry.Size.width = known_w; height = known_h }
        ~available_space:
          {
            Toffee.Geometry.Size.width = available_width;
            height = available_height;
          }
        ~style:Toffee.Style.default

(* ── Shared display cache ── *)

let measure_probe_does_not_poison_display () =
  (* Measurement and rendering share the display cache; a probe at another
     width must not leak its result into the render path. *)
  let s =
    make_surface ~width:10 ~content:"abcdefghijklmnopqrst" ~wrap:`Char ()
  in
  let _ =
    call_measure s ~available_width:(Toffee.Available_space.Definite 5.)
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"display uses node width" int 2 (Text_surface.display_line_count s)

let alternating_width_probes_stay_consistent () =
  let s =
    make_surface ~width:10 ~content:"abcdefghijklmnopqrst" ~wrap:`Char ()
  in
  let height_at w =
    let r =
      call_measure s ~available_width:(Toffee.Available_space.Definite w)
        ~available_height:Toffee.Available_space.Max_content
    in
    int_of_float r.Toffee.Geometry.Size.height
  in
  equal ~msg:"4 lines at width 5" int 4 (height_at 5.);
  equal ~msg:"2 lines at width 10" int 2 (height_at 10.);
  equal ~msg:"4 lines at width 5 again" int 4 (height_at 5.)

let truncation_is_keyed_by_width () =
  let s = make_surface ~width:10 ~content:"abcdefghijklmnopqrst" () in
  Text_surface.set_truncate s true;
  let info = Text_surface.layout s in
  equal ~msg:"truncated to node width" int 10 (Layout.max_width info);
  layout_node (Text_surface.node s) ~x:0 ~y:0 ~width:25 ~height:10;
  let info = Text_surface.layout s in
  equal ~msg:"wider layout sees full line" int 20 (Layout.max_width info)

let measure_max_content_returns_natural_width () =
  let s = make_surface ~content:"hello world" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* "hello world" = 11 columns *)
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_max_content_multiline () =
  let s = make_surface ~content:"hi\nhello world\nbye" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 3 (int_of_float result.height)

let measure_min_content_char_wrap () =
  let s = make_surface ~content:"hello world" ~wrap:`Char () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Min_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Min_content with char wrap: wrap_width=1, each grapheme gets its own
     line *)
  equal ~msg:"width" int 1 (int_of_float result.width)

let measure_min_content_word_wrap () =
  let s = make_surface ~content:"hi there" ~wrap:`Word () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Min_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Min_content with word wrap: wraps at width=1, words break to char level *)
  satisfies ~claim:"width at least 1" float_exact
    (fun w -> w >= 1.)
    result.width

let measure_definite_caps_with_wrap () =
  let s = make_surface ~content:"hello world" ~wrap:`Word () in
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 8.)
      ~available_height:Toffee.Available_space.Max_content
  in
  (* With wrapping: width should not exceed available width of 8 *)
  satisfies ~claim:"width at most the available 8" float_exact
    (fun w -> w <= 8.)
    result.width;
  (* Content wraps: "hello" + "world" -> at least 2 lines *)
  satisfies ~claim:"height at least 2" float_exact
    (fun h -> h >= 2.)
    result.height

let measure_definite_no_wrap_uncapped () =
  let s = make_surface ~content:"hello world" () in
  (* wrap = `None by default for surface *)
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 5.)
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Without wrapping: width should be natural width, not capped *)
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_truncate_ignores_previous_layout_width () =
  let s = make_surface ~content:"dune" () in
  Text_surface.set_truncate s true;
  (* A previous render laid the node out at its then-content's width. *)
  Text_surface.set_wrap_width s (Some 4);
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width before growth" int 4 (int_of_float result.width);
  (* The reconciler reuses the node for longer content: the intrinsic width
     must be the new content's, not the stale layout width. *)
  Text_buffer.set_text (Text_surface.buffer s) "review_compose.ml";
  Text_surface.invalidate s;
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width after growth" int 17 (int_of_float result.width)

let measure_word_max_content_ignores_layout_width () =
  (* The node was last laid out narrower than the content. Max-content never
     wraps, so the intrinsic width is the longest unwrapped line, not a wrap
     at the stale laid-out width — otherwise a container that shrinks and
     grows back leaves the text clamped at the narrow width forever. *)
  let s = make_surface ~width:5 ~content:"hello world" ~wrap:`Word () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_word_max_content_unbreakable_run () =
  (* A single unbreakable run (a rule built from repeated glyphs) after a
     narrower layout; exercises the char-fallback path inside word wrap. *)
  let s = make_surface ~width:5 ~content:(String.make 11 '-') ~wrap:`Word () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 11 (int_of_float result.width);
  equal ~msg:"height" int 1 (int_of_float result.height)

let measure_known_width_overrides () =
  let s = make_surface ~content:"hello world" () in
  let result =
    call_measure_with_known s ~known_w:(Some 42.) ~known_h:None
      ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"width" int 42 (int_of_float result.width)

let measure_known_height_overrides () =
  let s = make_surface ~content:"a\nb\nc" () in
  let result =
    call_measure_with_known s ~known_w:None ~known_h:(Some 10.)
      ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"height" int 10 (int_of_float result.height)

let measure_empty_buffer () =
  let s = make_surface ~content:"" () in
  let result =
    call_measure s ~available_width:Toffee.Available_space.Max_content
      ~available_height:Toffee.Available_space.Max_content
  in
  (* Empty buffer should return minimum 1x1 *)
  satisfies ~claim:"width at least 1" float_exact
    (fun w -> w >= 1.)
    result.width;
  satisfies ~claim:"height at least 1" float_exact
    (fun h -> h >= 1.)
    result.height

let measure_height_grows_for_wrapped () =
  (* 20 chars in 10-wide viewport should produce 2 lines *)
  let s = make_surface ~content:"abcdefghijklmnopqrst" ~wrap:`Char () in
  let result =
    call_measure s ~available_width:(Toffee.Available_space.Definite 10.)
      ~available_height:Toffee.Available_space.Max_content
  in
  equal ~msg:"height" int 2 (int_of_float result.height)

(* ── Convenience Getters ── *)

let max_scroll_x_wider_content () =
  (* Content "hello world" = 11 cols, viewport width = 5 *)
  let s = make_surface ~width:5 ~content:"hello world" () in
  equal ~msg:"max_scroll_x" int 6 (Text_surface.max_scroll_x s)

let max_scroll_x_fits () =
  let s = make_surface ~width:40 ~content:"hi" () in
  equal ~msg:"max_scroll_x" int 0 (Text_surface.max_scroll_x s)

let max_scroll_y_taller_content () =
  let s = make_surface ~height:2 ~content:"a\nb\nc\nd" () in
  equal ~msg:"max_scroll_y" int 2 (Text_surface.max_scroll_y s)

let max_scroll_y_fits () =
  let s = make_surface ~height:10 ~content:"a\nb" () in
  equal ~msg:"max_scroll_y" int 0 (Text_surface.max_scroll_y s)

(* ── Selection — multi-line ── *)

let selection_multiline_text () =
  let s = make_surface ~content:"hello\nworld" () in
  let _ = Text_surface.set_selection s ~start:3 ~end_:9 in
  equal ~msg:"selected" string "lo\nwor" (Text_surface.selected_text s)

let selection_full_content () =
  let s = make_surface ~content:"abc" () in
  let _ = Text_surface.set_selection s ~start:0 ~end_:3 in
  equal ~msg:"full" string "abc" (Text_surface.selected_text s)

let selection_with_styled_text () =
  let s = make_surface () in
  let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default in
  Text_buffer.set_styled_text (Text_surface.buffer s)
    [
      { Text_buffer.text = "hello "; style = red };
      { Text_buffer.text = "world"; style = Ansi.Style.default };
    ];
  Text_surface.invalidate s;
  let _ = Text_surface.set_selection s ~start:4 ~end_:8 in
  equal ~msg:"across spans" string "o wo" (Text_surface.selected_text s)

(* ── Wrapping — edge cases ── *)

let wrap_word_long_word_fallback () =
  (* Word exceeding wrap width should fall back to char break *)
  let s = make_surface ~width:5 ~content:"abcdefghij" ~wrap:`Word () in
  let info = Text_surface.layout s in
  (* "abcdefghij" (10 chars) can't break at word boundaries, must char-break *)
  ge ~msg:"wraps" ~than:2 (Layout.row_count info)

let wrap_word_empty_lines_preserved () =
  let s = make_surface ~width:40 ~content:"a\n\nb" ~wrap:`Word () in
  equal ~msg:"3 lines" int 3 (Text_surface.display_line_count s)

let wrap_word_single_char_words () =
  let s = make_surface ~width:5 ~content:"a b c d e f" ~wrap:`Word () in
  let info = Text_surface.layout s in
  ge ~msg:"wraps" ~than:2 (Layout.row_count info)

(* ── Property Tests ── *)

(* Small printable strings *)
let small_string = Gen.string_of (Gen.char_range 'a' 'z')

(* ── Runner ── *)

let truncated_source_selection () =
  let s = make_surface ~width:3 ~content:"abcdef\nXY" () in
  Text_surface.set_truncate s true;
  ignore
    (Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:1 ~focus_x:2
       ~focus_y:1);
  equal string "XY" (Text_surface.selected_text s)

let zero_width_source_selection () =
  let s = make_surface ~content:"a\226\128\139b" () in
  ignore
    (Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:2
       ~focus_y:0);
  equal string "a\226\128\139b" (Text_surface.selected_text s)

let styled_cluster_paints_as_one () =
  let s = make_surface ~width:8 () in
  let red = Ansi.Style.make ~fg:Ansi.Color.red () in
  Text_buffer.set_styled_text (Text_surface.buffer s)
    [
      { Text_buffer.text = "e"; style = red };
      { Text_buffer.text = "\204\129👩‍💻x"; style = Ansi.Style.default };
    ];
  let grid = make_grid ~width:8 ~height:2 ~width_method:`Wcwidth () in
  Test_harness.render (Text_surface.node s) grid ~dt:0.;
  equal string "e\204\129" (Matrix_grid.get_text grid 0);
  is_true (Ansi.Color.equal Ansi.Color.red (Matrix_grid.get_fg grid 0));
  equal string "👩‍💻" (Matrix_grid.get_text grid 1);
  equal string "x" (Matrix_grid.get_text grid 3)

let cjk_wrap_across_styles () =
  let s = make_surface ~width:6 ~wrap:`Word () in
  let red = Ansi.Style.make ~fg:Ansi.Color.red () in
  Text_buffer.set_styled_text (Text_surface.buffer s)
    [
      { Text_buffer.text = "AB 测"; style = red };
      { Text_buffer.text = "试文"; style = Ansi.Style.default };
      { Text_buffer.text = "字"; style = red };
    ];
  let layout = Text_surface.layout s in
  equal int 2 (Layout.row_count layout);
  equal string "AB 测" (row_text layout 0);
  equal string "试文字" (row_text layout 1);
  let grid = make_grid ~width:6 ~height:2 () in
  Test_harness.render (Text_surface.node s) grid ~dt:0.;
  List.iter
    (fun (x, y, text, color) ->
      let cell = Matrix_grid.idx grid ~x ~y in
      equal string text (Matrix_grid.get_text grid cell);
      is_true (Ansi.Color.equal color (Matrix_grid.get_fg grid cell)))
    [
      (3, 0, "测", Ansi.Color.red);
      (0, 1, "试", Ansi.Color.default_fg);
      (2, 1, "文", Ansi.Color.default_fg);
      (4, 1, "字", Ansi.Color.red);
    ];
  ignore (Text_surface.set_selection s ~start:3 ~end_:7);
  equal string "测试文字" (Text_surface.selected_text s)

let clipped_text_uses_absolute_source_rows () =
  (* The surface is drawn under a node that clips its children to the third
     row's first three cells. *)
  let t = make_ctx () in
  let root = make_root t in
  let clip = Renderable.create ~parent:root () in
  Renderable.set_style clip
    (Toffee.Style.set_overflow
       { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
       (Renderable.style clip));
  layout_node clip ~x:0 ~y:0 ~width:10 ~height:4;
  Renderable.set_child_clip clip
    (Some (fun _ -> Some { Matrix_grid.x = 0; y = 2; width = 3; height = 1 }));
  let node = Renderable.create ~parent:clip () in
  layout_node node ~x:0 ~y:0 ~width:10 ~height:10000;
  let buffer = Text_buffer.create () in
  Text_buffer.set_text buffer "zero\none\ntwo\nthree";
  let s = Text_surface.create node buffer in
  let grid = make_grid ~width:10 ~height:4 () in
  Test_harness.render (Text_surface.node s) grid ~dt:0.;
  equal string "t" (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:0 ~y:2));
  equal string " " (Matrix_grid.get_text grid 0)

let clipped_wide_start_stays_inside_node () =
  let s = make_surface ~width:2 ~height:1 ~content:"界abc" () in
  layout_node (Text_surface.node s) ~x:2 ~y:0 ~width:2 ~height:1;
  Text_surface.set_scroll_x s 1;
  (* A node drawn before the surface writes N on the cell left of it. *)
  let root = Option.get (Renderable.parent (Text_surface.node s)) in
  let before = Renderable.create ~parent:root ~index:0 () in
  layout_node before ~x:1 ~y:0 ~width:1 ~height:1;
  Renderable.set_render before (fun self grid ~dt:_ ->
      Matrix_grid.draw_text grid ~x:(Renderable.x self) ~y:(Renderable.y self)
        ~text:"N");
  let grid = make_grid ~width:6 ~height:1 () in
  Test_harness.render (Text_surface.node s) grid ~dt:0.;
  equal string "N" (Matrix_grid.get_text grid 1);
  equal string "a" (Matrix_grid.get_text grid 3)

let ellipsis_drag_and_press () =
  let s = make_surface ~width:3 ~content:"abcdef" () in
  Text_surface.set_truncate s true;
  ignore
    (Text_surface.set_local_selection s ~anchor_x:2 ~anchor_y:0 ~focus_x:2
       ~focus_y:0);
  is_false (Text_surface.has_selection s);
  ignore
    (Text_surface.set_local_selection s ~anchor_x:0 ~anchor_y:0 ~focus_x:2
       ~focus_y:0);
  equal string "abcdef" (Text_surface.selected_text s)

let () =
  run "mosaic.text_surface"
    [
      group "Shared geometry paint"
        [
          test "style boundaries and grid defaults cannot split geometry"
            styled_cluster_paints_as_one;
          test "scissor rows retain source identity"
            clipped_text_uses_absolute_source_rows;
          test "clipped wide start cannot paint before its node"
            clipped_wide_start_stays_inside_node;
          test "ellipsis drag copies hidden source, press stays empty"
            ellipsis_drag_and_press;
        ];
      group "Construction"
        [
          test "defaults" defaults;
          test "oversized geometry is released after switching widths"
            oversized_layout_is_released_on_width_switch;
          test "selection changes compare normalized ranges"
            selection_updates_compare_normalized_ranges;
        ];
      group "Wrapping — None"
        [
          test "single line" wrap_none_single_line;
          test "multiline" wrap_none_multiline;
        ];
      group "Wrapping — Char"
        [
          test "splits at width" wrap_char_splits;
          test "short line no wrap" wrap_char_short_line_no_wrap;
          test "line_sources map" wrap_char_line_sources;
          test "wrap_indices" wrap_char_wrap_indices;
        ];
      group "Wrapping — Word"
        [
          test "CJK wraps across styles" cjk_wrap_across_styles;
          test "breaks at word boundary" wrap_word_at_boundary;
          test "preserves line sources" wrap_word_preserves_sources;
        ];
      group "set_wrap"
        [ test "changes mode and recomputes" set_wrap_changes_mode ];
      group "wrap_width"
        [
          test "explicit override" explicit_wrap_width;
          test "None derives from node" wrap_width_none_derives_from_node;
        ];
      group "Truncation"
        [
          test "long line truncated with ellipsis" truncate_long_line;
          test "short line unchanged" truncate_short_line_unchanged;
        ];
      group "Display info"
        [
          test "grapheme offsets" display_info_grapheme_offsets;
          test "max_line_width" display_info_max_line_width;
        ];
      group "Viewport / scroll"
        [
          test "scroll_x clamps" scroll_x_clamps;
          test "scroll_y clamps" scroll_y_clamps;
          test "scroll_height = display lines"
            scroll_height_equals_display_lines;
          test "scroll_width = max width" scroll_width_equals_max_width;
        ];
      group "Selection — set_selection"
        [
          test "set and get" selection_set_and_get;
          test "clamps to bounds" selection_clamps_to_bounds;
          test "normalized" selection_normalized;
          test "returns true when changed" selection_returns_true_when_changed;
          test "selected_text" selected_text_basic;
          test "empty when no selection" selected_text_empty_when_no_selection;
        ];
      group "Selection — reset"
        [ test "clears active state" reset_selection_clears ];
      group "Selection — local"
        [
          test "set_local_selection" set_local_selection_basic;
          test "update_local_selection extends" update_local_selection_extends;
          test "a drag past the rows selects to the ends"
            local_selection_past_the_rows;
        ];
      group "Cache invalidation"
        [
          test "invalidate clears cache" invalidate_clears_cache;
          test "restyling retains geometry" restyling_retains_geometry;
          test "reading geometry preserves layout invalidation"
            reading_new_geometry_does_not_skip_layout_invalidation;
          test "buffer version triggers recompute"
            buffer_version_triggers_recompute;
          test "buffer tab width triggers recompute"
            buffer_tab_width_triggers_recompute;
          test "measure probe does not poison display"
            measure_probe_does_not_poison_display;
          test "width cache reuses and releases obsolete geometry"
            replacement_releases_geometry;
          test "alternating width probes stay consistent"
            alternating_width_probes_stay_consistent;
          test "truncation is keyed by width" truncation_is_keyed_by_width;
        ];
      group "Measurement"
        [
          test "max_content returns natural width"
            measure_max_content_returns_natural_width;
          test "max_content multiline" measure_max_content_multiline;
          test "min_content char wrap" measure_min_content_char_wrap;
          test "min_content word wrap" measure_min_content_word_wrap;
          test "definite caps with wrap" measure_definite_caps_with_wrap;
          test "definite no wrap uncapped" measure_definite_no_wrap_uncapped;
          test "truncate ignores previous layout width"
            measure_truncate_ignores_previous_layout_width;
          test "word max_content ignores layout width"
            measure_word_max_content_ignores_layout_width;
          test "word max_content unbreakable run"
            measure_word_max_content_unbreakable_run;
          test "known width overrides" measure_known_width_overrides;
          test "known height overrides" measure_known_height_overrides;
          test "empty buffer" measure_empty_buffer;
          test "height grows for wrapped" measure_height_grows_for_wrapped;
        ];
      group "Convenience getters"
        [
          test "max_scroll_x wider content" max_scroll_x_wider_content;
          test "max_scroll_x fits" max_scroll_x_fits;
          test "max_scroll_y taller content" max_scroll_y_taller_content;
          test "max_scroll_y fits" max_scroll_y_fits;
        ];
      group "Selection — multi-line"
        [
          test "multi-line selection" selection_multiline_text;
          test "full content selection" selection_full_content;
          test "selection with styled text" selection_with_styled_text;
        ];
      group "Wrapping — edge cases"
        [
          test "long word falls back to char" wrap_word_long_word_fallback;
          test "empty lines preserved" wrap_word_empty_lines_preserved;
          test "single char words" wrap_word_single_char_words;
        ];
      group "Source geometry regressions"
        [
          test "truncation keeps later source offsets"
            truncated_source_selection;
          test "invisible clusters stay in source ranges"
            zero_width_source_selection;
        ];
      group "Properties"
        [
          prop "display_line_count >= line_count" small_string (fun s ->
              let surface = make_surface ~width:5 ~content:s ~wrap:`Char () in
              let buf = Text_surface.buffer surface in
              let lines = Text_buffer.line_count buf in
              ge ~msg:"display lines cover the buffer" ~than:lines
                (Text_surface.display_line_count surface));
          prop "scroll_x always >= 0" small_string (fun s ->
              let surface = make_surface ~width:5 ~content:s () in
              Text_surface.set_scroll_x surface (-100);
              ge ~than:0 (Text_surface.scroll_x surface));
          prop "scroll_y always >= 0" small_string (fun s ->
              let surface = make_surface ~height:3 ~content:s () in
              Text_surface.set_scroll_y surface (-100);
              ge ~than:0 (Text_surface.scroll_y surface));
        ];
    ]
