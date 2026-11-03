module Grid = Matrix_grid
module C = Ansi.Color
module S = Ansi.Style

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"
let grid_width = 160
let grid_height = 48
let solid_fill = C.of_rgb 28 32 36
let translucent_fill = C.of_rgba 64 128 224 110

let ascii_style =
  S.make ~fg:(C.of_rgb 232 232 232) ~bg:(C.of_rgb 18 20 24) ~bold:true ()

let make_ascii_line len =
  String.init len (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

let ascii_line = make_ascii_line (grid_width - 2)

let emoji_line =
  (* Mixed emoji payload to stress grapheme/width handling *)
  let fragments = [| "👩‍🚀"; "🛰️"; "🌌"; "✨"; "🚀"; "🪐"; "🌠"; "👨‍💻" |] in
  let buf = Buffer.create (Array.length fragments * 32) in
  for i = 0 to 31 do
    Buffer.add_string buf fragments.(i land 7)
  done;
  Buffer.contents buf

let scroll_seed =
  (* Representative line for terminal scroll / log output *)
  let base = "Matrix terminal benchmark line " in
  let target = grid_width in
  let buf = Buffer.create (String.length base * 4) in
  while Buffer.length buf < target do
    Buffer.add_string buf base
  done;
  let contents = Buffer.contents buf in
  String.sub contents 0 target

let make_grid () = Grid.create ~width:grid_width ~height:grid_height ()

(* Bulk fills: opaque & translucent overlays *)

(* Five bulk Bigarray fills complete in about 4 us on this host. Their
   throughput follows the current memory-frequency state closely enough to
   vary by more than 60% across otherwise isolated CI runs. Keep allocations
   exact, and retain a timing guard wide enough to reject the old per-cell path
   without turning normal frequency scaling into a flaky gate. *)
let opaque_fill_budgets =
  Thumper.Budget.no_more_alloc_than 0.
  :: (if exact then [] else [ Thumper.Budget.no_slower_than 1. ])

let fill_rect_opaque_full =
  Thumper.bench_with_setup ~budgets:opaque_fill_budgets
    ~setup:(fun () -> make_grid ())
    ~teardown:(fun _ -> ())
    "grid.fill_rect/opaque-full"
    (fun grid ->
      Grid.fill_rect grid ~x:0 ~y:0 ~width:grid_width ~height:grid_height
        ~color:solid_fill)

let fill_rect_translucent_overlay =
  Thumper.bench_with_setup
    ~setup:(fun () -> make_grid ())
    ~teardown:(fun _ -> ())
    "grid.fill_rect/translucent-overlay"
    (fun grid ->
      Grid.fill_rect grid ~x:0 ~y:0 ~width:grid_width ~height:grid_height
        ~color:translucent_fill)

(* Full-screen text: ASCII vs emoji-heavy *)

let draw_text_ascii_full =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      grid)
    ~teardown:(fun _ -> ())
    "grid.draw_text/ascii-full-screen"
    (fun grid ->
      (* Typical "code editor" / log viewer workload: full ASCII text. *)
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:ascii_line
      done)

let draw_text_emoji_full =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      grid)
    ~teardown:(fun _ -> ())
    "grid.draw_text/emoji-full-screen"
    (fun grid ->
      (* Emoji-heavy / chat-like workload: full screen mixed-width graphemes. *)
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:emoji_line
      done)

(* Scrolling region: terminal-like scrollback *)

let scroll_terminal_region =
  let top = 0 in
  let bottom = grid_height - 2 in
  let iterations = 20 in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let grid = make_grid () in
      Grid.clear grid ~color:(C.of_rgb 0 0 0);
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:row ~text:scroll_seed
      done;
      grid)
    ~teardown:(fun _ -> ())
    "grid.scroll/terminal-region"
    (fun grid ->
      (* Simulate a burst of log lines arriving. *)
      for _ = 1 to iterations do
        Grid.shift grid
          { x = 0; y = top; width = grid_width; height = bottom - top + 1 }
          ~dx:0 ~dy:(-1) ~color:C.transparent;
        Grid.draw_text ~style:ascii_style grid ~x:0 ~y:bottom ~text:scroll_seed
      done)

(* Partial updates: status line + sparse cells (cursor/status) *)

let partial_status_line =
  let status_text_1 =
    "matrix.ml  [NORMAL]  line 42, col 7   3 warnings (F5: build)"
  in
  let status_text_2 =
    "matrix.ml  [INSERT]  line 42, col 9   modified   (Ctrl+S: save)"
  in
  let toggle = ref false in
  let grid_ref = ref None in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let grid =
        match !grid_ref with
        | Some g -> g
        | None ->
            let g = make_grid () in
            Grid.clear g ~color:(C.of_rgb 0 0 0);
            (* Fill main area once with ASCII text to approximate editor
               body. *)
            for row = 0 to grid_height - 2 do
              Grid.draw_text ~style:ascii_style g ~x:0 ~y:row ~text:ascii_line
            done;
            grid_ref := Some g;
            g
      in
      toggle := false;
      grid)
    ~teardown:(fun _ -> ())
    "grid.partial_update/status-line"
    (fun grid ->
      toggle := not !toggle;
      let status_bg = C.of_rgb 200 200 200 in
      let status_fg = C.of_rgb 0 0 0 in
      let status_style = S.make ~fg:status_fg ~bg:status_bg ~bold:true () in
      let y = grid_height - 1 in
      let text = if !toggle then status_text_1 else status_text_2 in
      Grid.draw_text ~style:status_style grid ~x:0 ~y ~text)

let partial_update_sparse_cells =
  (* Cursor location + three status bar "slots" across the top row. *)
  let update_positions =
    [| (40, 12); (0, 0); (grid_width / 2, 0); (grid_width - 1, 0) |]
  in
  let grid_ref = ref None in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let grid =
        match !grid_ref with
        | Some g -> g
        | None ->
            let g = make_grid () in
            Grid.clear g ~color:(C.of_rgb 0 0 0);
            for row = 0 to grid_height - 1 do
              Grid.draw_text ~style:ascii_style g ~x:0 ~y:row ~text:ascii_line
            done;
            grid_ref := Some g;
            g
      in
      grid)
    ~teardown:(fun _ -> ())
    "grid.partial_update/sparse-cells"
    (fun grid ->
      (* Small, scattered updates as you'd get from a cursor + tiny UI
         chrome. *)
      for i = 0 to Array.length update_positions - 1 do
        let x, y = update_positions.(i) in
        Grid.set_uchar grid ~x ~y ~blend:true
          ~uchar:(Uchar.of_int (Char.code 'A' + i))
          ~fg:(C.of_rgb 255 255 0) ~bg:(C.of_rgb 0 0 0) ~attrs:Ansi.Attr.empty
          ()
      done)

(* Independent storage must compare text and URLs without materializing cells. *)
let independent_storage_equality =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let a = make_grid () and b = make_grid () in
      let style = S.make ~link:"https://example.com" () in
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style a ~x:0 ~y:row ~text:emoji_line;
        Grid.draw_text ~style b ~x:0 ~y:row ~text:emoji_line
      done;
      (a, b))
    ~teardown:(fun _ -> ())
    "grid.cells_equal/independent-stores"
    (fun (a, b) ->
      for i = 0 to (grid_width * grid_height) - 1 do
        if not (Grid.cells_equal a i b i) then failwith "unequal grid contents"
      done)

(* Retained URL references: cycling a bounded working set keeps the benchmark
   stationary while exercising replacement, bulk copy, and GC write barriers. *)
let hyperlink_styles =
  Array.init 256 (fun i ->
      S.make
        ~link:
          (Printf.sprintf "https://example.com/items/%d/%s" i
             (String.make 96 'x'))
        ())

let ownership_budgets = [ Thumper.Budget.no_more_alloc_than 0. ]

let rotating_hyperlinks =
  Thumper.bench_with_setup ~budgets:ownership_budgets
    ~setup:(fun () -> (make_grid (), ref 0))
    ~teardown:(fun _ -> ())
    "grid.ownership/rotating-hyperlinks"
    (fun (grid, tick) ->
      incr tick;
      for row = 0 to grid_height - 1 do
        Grid.draw_text
          ~style:hyperlink_styles.((!tick + row) land 255)
          grid ~x:0 ~y:row ~text:ascii_line
      done)

let linked_region_copy shared =
  Thumper.bench_with_setup ~budgets:ownership_budgets
    ~setup:(fun () ->
      let src = make_grid () in
      let dst =
        if shared then
          Grid.create_like src ~width:grid_width ~height:grid_height
        else make_grid ()
      in
      for row = 0 to grid_height - 1 do
        Grid.draw_text ~style:hyperlink_styles.(row) src ~x:0 ~y:row
          ~text:ascii_line
      done;
      (src, dst))
    ~teardown:(fun _ -> ())
    (if shared then "grid.ownership/linked-copy-shared"
     else "grid.ownership/linked-copy-independent")
    (fun (src, dst) ->
      Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:grid_width
        ~height:grid_height ~dst_x:0 ~dst_y:0)

(* Group + entry point *)

let benchmarks =
  [
    (* Bulk operations *)
    fill_rect_opaque_full;
    fill_rect_translucent_overlay;
    (* Full-screen text workloads *)
    draw_text_ascii_full;
    draw_text_emoji_full;
    (* Terminal-style scrollback *)
    scroll_terminal_region;
    (* Fine-grained partial updates *)
    partial_status_line;
    partial_update_sparse_cells;
    independent_storage_equality;
    rotating_hyperlinks;
    linked_region_copy true;
    linked_region_copy false;
  ]
  |> Thumper.group "grid"

let () =
  exit
    (Thumper.run "grid"
       ~config:
         Thumper.Config.(
           default
           |> metrics
                (if exact then [ Thumper.Metric.alloc_words ]
                 else Thumper.Metric.[ wall_time; alloc_words ]))
       ~budgets:
         (Thumper.Budget.no_more_alloc_than 0.
         :: (if exact then [] else [ Thumper.Budget.no_slower_than 0.05 ]))
       [ benchmarks ])
