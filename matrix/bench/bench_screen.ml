module C = Ansi.Color
module S = Ansi.Style

let grid_width = 160
let grid_height = 48

let ascii_style =
  S.make ~fg:(C.of_rgb 232 232 232) ~bg:(C.of_rgb 18 20 24) ~bold:true ()

let make_ascii_line len =
  String.init len (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let ascii_line = make_ascii_line (grid_width - 2)

let emoji_line =
  let fragments = [| "👩‍🚀"; "🛰️"; "🌌"; "✨"; "🚀"; "🪐"; "🌠"; "👨‍💻" |] in
  let buf = Buffer.create (Array.length fragments * 32) in
  for i = 0 to 31 do
    Buffer.add_string buf fragments.(i land 7)
  done;
  Buffer.contents buf

module Hit_grid = Screen.Hit_grid

(* Diff micro-benchmarks *)

(* Full-screen change: typical "mode switch" or theme change. *)
let diff_full_screen =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let screen = Screen.create () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          let bg0 = C.of_rgb 0 0 0 in
          Grid.clear grid ~color:bg0;
          for row = 0 to grid_height - 1 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.diff/full-screen"
    (fun screen ->
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          let bg1 = C.of_rgb 18 20 24 in
          Grid.clear grid ~color:bg1;
          for row = 0 to grid_height - 1 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done);
      ignore (Screen.render screen))

(* Single line change in a full-screen text buffer. *)
let diff_single_line =
  let changed_row = grid_height / 2 in
  let clear_color = C.of_rgb 0 0 0 in
  let changed_style =
    S.make ~fg:(C.of_rgb 255 80 80) ~bg:clear_color ~bold:true ()
  in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let screen = Screen.create () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:clear_color;
          for row = 0 to grid_height - 1 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.diff/single-line"
    (fun screen ->
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:clear_color;
          for row = 0 to grid_height - 1 do
            if row = changed_row then
              Grid.draw_text ~style:changed_style grid ~x:0 ~y:row
                ~text:"CHANGED LINE"
            else
              Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row
                ~text:ascii_line
          done);
      ignore (Screen.render screen))

(* Sparse cells only: cursor + a few status cells. *)
let diff_sparse_cells =
  let clear_color = C.of_rgb 0 0 0 in
  let update_positions =
    [| (40, 12); (0, 0); (grid_width / 2, 0); (grid_width - 1, 0) |]
  in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let screen = Screen.create () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:clear_color;
          for row = 0 to grid_height - 1 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.diff/sparse-cells"
    (fun screen ->
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:clear_color;
          for row = 0 to grid_height - 1 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done;
          for i = 0 to Array.length update_positions - 1 do
            let x, y = update_positions.(i) in
            Grid.set_cell grid ~x ~y
              ~glyph:(Glyph.of_uchar (Uchar.of_int (Char.code 'A' + i)))
              ~fg:(C.of_rgb 255 255 0) ~bg:clear_color ~attrs:Ansi.Attr.empty ()
          done);
      ignore (Screen.render screen))

(* Nothing changes: cost of diffing two identical full frames. *)
let diff_no_changes =
  let clear_color = C.of_rgb 0 0 0 in
  let build_full_ascii screen =
    Screen.build screen ~width:grid_width ~height:grid_height (fun grid _hits ->
        Grid.clear grid ~color:clear_color;
        for row = 0 to grid_height - 1 do
          Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
        done)
  in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let screen = Screen.create () in
      build_full_ascii screen;
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.diff/no-changes"
    (fun screen ->
      build_full_ascii screen;
      ignore (Screen.render screen))

(* Scenario: text editor / TUI layout *)

let scenario_text_editor =
  let toggle = ref false in
  let code_bg = C.of_rgb 18 20 24 in
  let status_fg = C.of_rgb 0 0 0 in
  let status_bg = C.of_rgb 200 200 200 in
  let status_style = S.make ~fg:status_fg ~bg:status_bg ~bold:true () in
  let cursor_row = 12 in
  let cursor_col = 8 in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      toggle := false;
      let screen = Screen.create () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid hits ->
          Grid.clear grid ~color:code_bg;
          (* Code area *)
          for row = 0 to grid_height - 2 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done;
          (* Initial cursor line + status bar *)
          let cursor_style =
            S.make ~fg:(C.of_rgb 255 255 0) ~bg:(C.of_rgb 40 40 0) ~bold:true ()
          in
          Grid.draw_text ~style:cursor_style grid ~x:cursor_col ~y:cursor_row
            ~text:"let value = 42 in";
          Grid.draw_text ~style:status_style grid ~x:0 ~y:(grid_height - 1)
            ~text:"-- NORMAL --  matrix.ml  42L, 1C  All tests passing";
          Hit_grid.add hits ~x:0 ~y:(grid_height - 1) ~width:grid_width
            ~height:1 ~id:1);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.scenario/text-editor"
    (fun screen ->
      toggle := not !toggle;
      let mode_label = if !toggle then "-- INSERT --" else "-- NORMAL --" in
      let cursor_style =
        if !toggle then
          S.make ~fg:(C.of_rgb 255 255 0) ~bg:(C.of_rgb 40 40 0) ~bold:true ()
        else
          S.make ~fg:(C.of_rgb 200 200 255) ~bg:(C.of_rgb 30 30 40) ~italic:true
            ()
      in
      let status_text =
        mode_label ^ "  matrix.ml  42L, 1C  All tests passing"
      in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid hits ->
          Grid.clear grid ~color:code_bg;
          for row = 0 to grid_height - 2 do
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
          done;
          Grid.draw_text ~style:cursor_style grid ~x:cursor_col ~y:cursor_row
            ~text:"let value = 42 in";
          Grid.draw_text ~style:status_style grid ~x:0 ~y:(grid_height - 1)
            ~text:status_text;
          Hit_grid.add hits ~x:0 ~y:(grid_height - 1) ~width:grid_width
            ~height:1 ~id:1);
      ignore (Screen.render screen))

(* Scenario: CI log viewer / heavy style + hyperlinks *)

type log_line = { level : string; message : string; path : string option }

let log_lines : log_line array =
  [|
    { level = "INFO"; message = "Starting build (matrix)"; path = None };
    {
      level = "INFO";
      message = "Running tests (unit / integration)";
      path = Some "tests/test_matrix.ml:42";
    };
    {
      level = "WARN";
      message = "Flaky test detected; retry scheduled";
      path = Some "tests/flaky_pipeline.ml:10";
    };
    {
      level = "ERROR";
      message = "Compilation failed: unresolved reference";
      path = Some "src/main.ml:120";
    };
  |]

let log_timestamp = "[2025-11-20T12:34:56Z]"
let log_timestamp_width = String.length log_timestamp

let level_tag level =
  match level with
  | "INFO" -> "[INFO]"
  | "WARN" -> "[WARN]"
  | "ERROR" -> "[ERROR]"
  | other -> "[" ^ other ^ "]"

let level_style level =
  match level with
  | "INFO" -> S.make ~fg:(C.of_rgb 120 200 120) ~bold:true ()
  | "WARN" -> S.make ~fg:(C.of_rgb 230 200 80) ~bold:true ()
  | "ERROR" -> S.make ~fg:(C.of_rgb 255 80 80) ~bold:true ()
  | _ -> S.make ~fg:(C.of_rgb 180 180 180) ()

let timestamp_style = S.make ~fg:(C.of_rgb 130 130 130) ()
let message_style = ascii_style

let render_ci_logs grid ~progress_line =
  let rows_for_logs = grid_height - 1 in
  for row = 0 to rows_for_logs - 1 do
    let entry = log_lines.(row mod Array.length log_lines) in
    let y = row in
    (* timestamp *)
    Grid.draw_text ~style:timestamp_style grid ~x:0 ~y ~text:log_timestamp;
    (* level *)
    let level_txt = level_tag entry.level in
    let x_level = log_timestamp_width + 1 in
    Grid.draw_text ~style:(level_style entry.level) grid ~x:x_level ~y
      ~text:level_txt;
    (* message *)
    let x_msg = x_level + String.length level_txt + 1 in
    Grid.draw_text ~style:message_style grid ~x:x_msg ~y ~text:entry.message;
    (* optional clickable path at the right edge *)
    match entry.path with
    | None -> ()
    | Some path ->
        let path_text = " → " ^ path in
        let w = String.length path_text in
        let x_path = max x_msg (grid_width - w) in
        let link = "file://" ^ path in
        let path_style =
          S.make ~fg:(C.of_rgb 140 180 255) ~underline:true ~link ()
        in
        Grid.draw_text ~style:path_style grid ~x:x_path ~y ~text:path_text
  done;
  (* bottom progress line *)
  let progress_style = S.make ~fg:(C.of_rgb 200 200 200) ~bold:true () in
  Grid.draw_text ~style:progress_style grid ~x:0 ~y:(grid_height - 1)
    ~text:progress_line

let progress_line_a =
  "Progress: [##########..........]  42%  (5/12 jobs succeeded)"

let progress_line_b =
  "Progress: [############........]  58%  (7/12 jobs succeeded)"

let scenario_ci_log_view =
  let toggle = ref false in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      toggle := false;
      let screen = Screen.create () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:(C.of_rgb 0 0 0);
          render_ci_logs grid ~progress_line:progress_line_a);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.scenario/ci-log-view"
    (fun screen ->
      toggle := not !toggle;
      let progress_line =
        if !toggle then progress_line_b else progress_line_a
      in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:(C.of_rgb 0 0 0);
          render_ci_logs grid ~progress_line);
      ignore (Screen.render screen))

(* Emoji / explicit-width full-screen render *)

let emoji_full_screen =
  let toggle = ref false in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      toggle := false;
      let screen = Screen.create ~explicit_width:true () in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          let bg = C.of_rgb 0 0 0 in
          Grid.clear grid ~color:bg;
          for row = 0 to grid_height - 1 do
            let text = if row land 1 = 0 then emoji_line else ascii_line in
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text
          done);
      let _ = Screen.render screen in
      screen)
    ~teardown:(fun _ -> ())
    "render.emoji/full-screen"
    (fun screen ->
      toggle := not !toggle;
      let bg = if !toggle then C.of_rgb 0 0 0 else C.of_rgb 10 10 20 in
      Screen.build screen ~width:grid_width ~height:grid_height
        (fun grid _hits ->
          Grid.clear grid ~color:bg;
          for row = 0 to grid_height - 1 do
            let text = if row land 1 = 0 then emoji_line else ascii_line in
            Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text
          done);
      ignore (Screen.render screen))

(* Group + entry point *)

let benchmarks =
  [
    (* Diff micro-benchmarks *)
    diff_full_screen;
    diff_single_line;
    diff_sparse_cells;
    diff_no_changes;
    (* Realistic scenarios *)
    scenario_text_editor;
    scenario_ci_log_view;
    emoji_full_screen;
  ]
  |> Thumper.group "render"

let () = Thumper.run "screen" [ benchmarks ]
