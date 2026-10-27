open Mosaic_ui

let data_rows = 1_000
let data_cols = 4
let viewport_width = 80
let viewport_height = 24

let table_style =
  Toffee.Style.default
  |> Toffee.Style.set_width
       (Toffee.Style.Dimension.length (Float.of_int viewport_width))
  |> Toffee.Style.set_height
       (Toffee.Style.Dimension.length (Float.of_int viewport_height))

let cell_text =
  Thumper.black_box (fun r c -> Printf.sprintf "cell %d/%d payload" r c)

let render_frame renderer =
  Renderer.render_frame renderer ~width:viewport_width ~height:viewport_height
    ~dt:16.;
  ignore (Renderer.render renderer : string)

let make_table () =
  let renderer = Renderer.create () in
  let columns =
    List.init data_cols (fun c -> Table.column ("Column " ^ string_of_int c))
  in
  let rows =
    List.init data_rows (fun r ->
        Array.init data_cols (fun c ->
            (* One rich column per row: measuring rich cells flattens the
               fragment tree to plain text, the allocation-heavy path. *)
            if c = data_cols - 1 then
              Table.rich
                [ Text.Fragment.bold [ Text.Fragment.text (cell_text r c) ] ]
            else Table.cell (cell_text r c)))
  in
  let table =
    Table.create ~parent:(Renderer.root renderer) ~style:table_style
      (Table.Props.make ~columns ~rows ())
  in
  render_frame renderer;
  (renderer, table)

(* A hover- or selection-driven re-render: the data is unchanged, only the
   frame is redrawn. This is the hot path a mouse move exercises on every
   hovered-row change. *)
let hover_rerender (renderer, table) =
  Renderable.request_render (Table.node table);
  render_frame renderer

let steady_hover =
  Thumper.bench_with_setup ~setup:make_table "frame/hover-rerender-1000x4"
    hover_rerender

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "table"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else
                Thumper.Metric.
                  [ wall_time; alloc_words; promoted_words; minor_collections ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           (* Promotions and minor collections depend on where the minor
              heap stands when a sample starts, so they are rarely exact:
              kept, with room for that noise. *)
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
           Thumper.Budget.relative ~metric:Thumper.Metric.minor_collections
             ~max_regression:0.5 ();
         ]))
    [ Thumper.group "viewport" [ steady_hover ] ]
