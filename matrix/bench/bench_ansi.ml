(* bench_ansi.ml *)

module S = Ansi.Style
module C = Ansi.Color

(* Helpers *)

let repeat chunk count =
  let len = String.length chunk in
  let buf = Buffer.create (len * count) in
  for _ = 1 to count do
    Buffer.add_string buf chunk
  done;
  Buffer.contents buf

(* Sample payloads *)

(* A realistic log line used across benchmarks *)
let plain_log_line =
  "[2024-01-01T00:00:00Z] INFO  http-server: Listening on \
   http://127.0.0.1:8080 (pid=1234, env=prod)"

let plain_log_block =
  (* Simulate a CI log chunk *)
  repeat (plain_log_line ^ "\n") 128

(* Predefined styles reused in all styled / segment benchmarks *)
let style_timestamp = S.make ~fg:C.bright_black ()
let style_info = S.make ~fg:C.bright_green ~bold:true ()
let style_debug = S.make ~fg:C.bright_blue ()
let style_context = S.make ~fg:C.cyan ()
let style_plain = S.default

(* Log line decomposed into styled segments (typical logging use‑case) *)
let log_segments : (S.t * string) list =
  [
    (style_timestamp, "[2024-01-01T00:00:00Z] ");
    (style_info, "INFO ");
    (style_plain, "http-server: Listening on ");
    (style_debug, "http://127.0.0.1:8080 ");
    (style_context, "(pid=1234, env=prod)");
  ]

let ansi_log_line = Ansi.render log_segments
let ansi_log_block = repeat (ansi_log_line ^ "\n") 128

(* A synthetic "frame" of TUI output with cursor movement, RGB background, etc.
   This approximates what a full-screen TUI would emit for one frame. *)
let tui_frame =
  let rows = 24 in
  let buf = Buffer.create 16_384 in
  Buffer.add_string buf Ansi.(to_string (enable Alternate_screen));
  Buffer.add_string buf Ansi.(to_string (disable Cursor_visible));
  for row = 1 to rows do
    (* Move to row start *)
    Buffer.add_string buf Ansi.(to_string (cursor_position ~row ~col:1));

    (* Vary the background color a bit by row to keep SGRs non-trivial *)
    let r = row * 10 land 0xFF in
    let g = (255 - (row * 7)) land 0xFF in
    let b = (50 + (row * 3)) land 0xFF in
    Buffer.add_string buf Ansi.(to_string (set_background ~r ~g ~b));

    (* Emit a status line / row payload *)
    Buffer.add_string buf "row ";
    Buffer.add_string buf (string_of_int row);
    Buffer.add_string buf " │ ";
    Buffer.add_string buf plain_log_line;
    Buffer.add_string buf Ansi.(to_string reset);
    Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf Ansi.(to_string (enable Cursor_visible));
  Buffer.add_string buf Ansi.(to_string (disable Alternate_screen));
  Buffer.contents buf

(* Bench group: styled output (producers) *)

let styled_inline =
  Thumper.bench "inline_styled" (fun () ->
      (* Logging-style single-call styling, e.g. direct use from application
         code *)
      let s =
        S.styled ~reset:true
          (S.make ~fg:C.red ~bold:true ())
          "ERROR: failed to connect to upstream (status=502, retries=3)"
      in
      ignore (Sys.opaque_identity s))

let styled_segments =
  Thumper.bench "render_log_segments" (fun () ->
      (* Renderer-style segmented output, e.g. logging framework or TUI
         header *)
      let out = Ansi.render log_segments in
      ignore (Sys.opaque_identity out))

(* Bench group: stripping & parsing (consumers / CI log tools) *)

let strip_plain_block_bench =
  Thumper.bench "strip_plain_block" (fun () ->
      (* Fast path: no escape sequences *)
      let s = Ansi.strip plain_log_block in
      ignore (Sys.opaque_identity s))

let strip_ansi_block_bench =
  Thumper.bench "strip_ansi_block" (fun () ->
      (* Heavy path: lots of SGR sequences, typical CI log chunk *)
      let s = Ansi.strip ansi_log_block in
      ignore (Sys.opaque_identity s))

let parse_ansi_block_bench =
  Thumper.bench "parse_ansi_block" (fun () ->
      (* Parsing a chunk of colored logs for later reformatting / filtering *)
      let tokens = Ansi.parse ansi_log_block in
      ignore (Sys.opaque_identity tokens))

let parse_tui_frame_bench =
  Thumper.bench "parse_tui_frame" (fun () ->
      (* Parsing a TUI frame: lots of CSI, SGR and control sequences mixed with
         text. Representative of terminal emulators or log recorders. *)
      let tokens = Ansi.parse tui_frame in
      ignore (Sys.opaque_identity tokens))

(* Pre-allocated parser for callback benchmarks - reuse across iterations *)
let iter_parser = Ansi.Parser.create ()
let ansi_log_block_bytes = Bytes.unsafe_of_string ansi_log_block
let tui_frame_bytes = Bytes.unsafe_of_string tui_frame

let parse_ansi_block_iter_bench =
  Thumper.bench "parse_ansi_block_iter" (fun () ->
      (* Callback-based parsing - avoids list allocation *)
      Ansi.Parser.reset iter_parser;
      let count = ref 0 in
      Ansi.Parser.feed iter_parser ansi_log_block_bytes ~off:0
        ~len:(Bytes.length ansi_log_block_bytes) (fun _ -> incr count);
      ignore (Sys.opaque_identity !count))

let parse_tui_frame_iter_bench =
  Thumper.bench "parse_tui_frame_iter" (fun () ->
      (* Callback-based parsing - avoids list allocation *)
      Ansi.Parser.reset iter_parser;
      let count = ref 0 in
      Ansi.Parser.feed iter_parser tui_frame_bytes ~off:0
        ~len:(Bytes.length tui_frame_bytes) (fun _ -> incr count);
      ignore (Sys.opaque_identity !count))

(* Bench group: control / cursor sequences *)

let cursor_script_80x24 =
  Thumper.bench "cursor_script_80x24" (fun () ->
      (* Typical TUI "paint the grid" pattern using cursor_position *)
      let buf = Buffer.create 32_000 in
      for row = 1 to 24 do
        for col = 1 to 80 do
          Buffer.add_string buf Ansi.(to_string (cursor_position ~row ~col));
          Buffer.add_char buf 'X'
        done
      done;
      ignore (Sys.opaque_identity (Buffer.length buf)))

(* Bench group: Sgr_state (terminal state diffing for render loops) *)

module Sgr = Ansi.Sgr_state
module Attr = Ansi.Attr

(* Shared buffer for all Sgr_state benchmarks - reused per iteration.
   Alternating styles at 1920 cells can emit ~50 bytes per cell. *)
let sgr_buf = Bytes.create 131072

(* Palette of styles for realistic TUI rendering *)
let palette =
  [|
    (* fg, bg, attrs *)
    (C.of_rgb 255 255 255, C.default, 0);
    (* white on default *)
    (C.of_rgb 255 0 0, C.default, Attr.pack Attr.bold);
    (* red bold *)
    (C.of_rgb 0 255 0, C.default, 0);
    (* green *)
    (C.of_rgb 0 0 255, C.of_rgb 51 51 51, 0);
    (* blue on gray *)
    (C.of_rgb 255 255 0, C.of_rgb 0 0 128, Attr.pack Attr.underline);
    (* yellow underline on blue *)
    (C.of_rgb 128 128 128, C.default, Attr.pack Attr.dim);
    (* dim gray *)
    (C.of_rgb 255 128 0, C.default, Attr.pack Attr.italic);
    (* orange italic *)
    (C.of_rgb 0 255 255, C.of_rgb 26 0 26, 0);
    (* cyan on dark purple *)
  |]

(* Pre-allocated state and writer for zero-allocation benchmarks *)
let sgr_state_tui = Sgr.create ()
let sgr_writer_tui = Ansi.Writer.make sgr_buf

let sgr_tui_frame_80x24 =
  Thumper.bench "sgr_tui_frame_80x24" (fun () ->
      (* Simulate rendering a full 80x24 TUI frame with varied styles. This is
         the primary use case for Sgr_state: rendering a grid where adjacent
         cells often share styles (state diffing wins). *)
      Sgr.reset sgr_state_tui;
      Ansi.Writer.reset_pos sgr_writer_tui;
      for row = 0 to 23 do
        for col = 0 to 79 do
          (* Pick style based on position - simulates UI regions *)
          let idx = ((row / 6) + (col / 20)) mod Array.length palette in
          let fg, bg, attrs = palette.(idx) in
          Sgr.update sgr_state_tui sgr_writer_tui ~fg ~bg ~attrs ~link:""
        done;
        (* Reset at end of row like Screen.render does *)
        Sgr.reset sgr_state_tui
      done;
      ignore (Sys.opaque_identity (Ansi.Writer.len sgr_writer_tui)))

let sgr_state_same = Sgr.create ()
let sgr_writer_same = Ansi.Writer.make sgr_buf
let sgr_white = C.of_rgb 255 255 255

let sgr_same_style_1920 =
  Thumper.bench "sgr_same_style_1920" (fun () ->
      (* Best case: 1920 cells (one row at 1920px) with identical style. After
         the first update, all subsequent calls should be no-ops. Tests the fast
         path where state diffing avoids all output. *)
      Sgr.reset sgr_state_same;
      Ansi.Writer.reset_pos sgr_writer_same;
      for _ = 0 to 1919 do
        Sgr.update sgr_state_same sgr_writer_same ~fg:sgr_white ~bg:C.default
          ~attrs:0 ~link:""
      done;
      ignore (Sys.opaque_identity (Ansi.Writer.len sgr_writer_same)))

let sgr_state_alt = Sgr.create ()
let sgr_writer_alt = Ansi.Writer.make sgr_buf
let sgr_red = C.of_rgb 255 0 0
let sgr_green = C.of_rgb 0 255 0
let sgr_alt_bg = C.of_rgb 26 26 26

let sgr_alternating_styles_1920 =
  Thumper.bench "sgr_alternating_styles_1920" (fun () ->
      (* Worst case: style changes on every cell (checkerboard pattern). Every
         update emits a full SGR sequence. Tests raw throughput. *)
      Sgr.reset sgr_state_alt;
      Ansi.Writer.reset_pos sgr_writer_alt;
      for i = 0 to 1919 do
        if i land 1 = 0 then
          Sgr.update sgr_state_alt sgr_writer_alt ~fg:sgr_red ~bg:C.default
            ~attrs:(Attr.pack Attr.bold) ~link:""
        else
          Sgr.update sgr_state_alt sgr_writer_alt ~fg:sgr_green ~bg:sgr_alt_bg
            ~attrs:0 ~link:""
      done;
      ignore (Sys.opaque_identity (Ansi.Writer.len sgr_writer_alt)))

(* Pre-allocated options to avoid boxing in the hot loop *)
let link_url = "https://example.com/path/to/resource"

(* Pre-allocated literal sequence *)
let link_text = Ansi.literal "link text here"
let sgr_state_link = Sgr.create ()
let sgr_writer_link = Ansi.Writer.make sgr_buf
let sgr_link_fg = C.of_rgb 77 128 255

let sgr_hyperlink_transitions =
  Thumper.bench "sgr_hyperlink_transitions" (fun () ->
      (* Hyperlink-heavy scenario: alternating between linked and non-linked
         text. Simulates a log viewer or markdown renderer with many links. *)
      Sgr.reset sgr_state_link;
      Ansi.Writer.reset_pos sgr_writer_link;
      for i = 0 to 199 do
        let link = if i mod 5 < 2 then link_url else "" in
        Sgr.update sgr_state_link sgr_writer_link ~fg:sgr_link_fg ~bg:C.default
          ~attrs:(if i mod 5 < 2 then Attr.pack Attr.underline else 0)
          ~link;
        (* Simulate emitting some text *)
        Ansi.emit link_text sgr_writer_link
      done;
      Sgr.close_link sgr_state_link sgr_writer_link;
      ignore (Sys.opaque_identity (Ansi.Writer.len sgr_writer_link)))

(* Pre-allocated colors array for partial changes benchmark *)
let partial_colors =
  [| C.of_rgb 255 0 0; C.of_rgb 0 255 0; C.of_rgb 0 0 255; C.of_rgb 255 255 0 |]

let sgr_state_partial = Sgr.create ()
let sgr_writer_partial = Ansi.Writer.make sgr_buf
let sgr_partial_bg = C.of_rgb 26 26 38

let sgr_partial_changes =
  Thumper.bench "sgr_partial_changes" (fun () ->
      (* Partial style changes: only foreground changes, background stays.
         Common in syntax highlighting where bg is uniform. *)
      Sgr.reset sgr_state_partial;
      Ansi.Writer.reset_pos sgr_writer_partial;
      for i = 0 to 999 do
        Sgr.update sgr_state_partial sgr_writer_partial
          ~fg:partial_colors.(i mod 4)
          ~bg:sgr_partial_bg ~attrs:0 ~link:""
      done;
      ignore (Sys.opaque_identity (Ansi.Writer.len sgr_writer_partial)))

(* Registration *)

let benchmarks =
  Thumper.
    [
      group "styled" [ styled_inline; styled_segments ];
      group "strip_parse"
        [
          strip_plain_block_bench;
          strip_ansi_block_bench;
          parse_ansi_block_bench;
          parse_ansi_block_iter_bench;
          parse_tui_frame_bench;
          parse_tui_frame_iter_bench;
        ];
      group "control" [ cursor_script_80x24 ];
      group "sgr_state"
        [
          sgr_tui_frame_80x24;
          sgr_same_style_1920;
          sgr_alternating_styles_1920;
          sgr_hyperlink_transitions;
          sgr_partial_changes;
        ];
    ]

let () = Thumper.run "ansi" benchmarks
