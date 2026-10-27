open Mosaic_ui

(* Declarations reuse 5000 preconstructed rows/options. The selected value is
   last so the value adapter scans the complete collection. Each positional
   case builds the widget's props with the same presentation, by position,
   without the value lookup or conversion and without the element around
   them; the difference is the element's cost, the application adapter's
   above all. *)
let count = 5000
let last = count - 1
let labels = Array.init count (fun i -> Printf.sprintf "item-%d" i)
let selected = Some labels.(last)

let select_options =
  Thumper.black_box
    (List.init count (fun i ->
         Select.{ label = labels.(i); description = None }))

let select_items =
  Thumper.black_box
    (List.init count (fun i -> Mosaic.Select.item labels.(i) labels.(i)))

let tab_options =
  Thumper.black_box
    (List.init count (fun i ->
         Tab_select.{ label = labels.(i); description = "" }))

let tab_items =
  Thumper.black_box
    (List.init count (fun i -> Mosaic.Tab_select.item labels.(i) labels.(i)))

let columns = [ Table.column ~width:(`Fixed 20) "Item" ]

let rows =
  Thumper.black_box (Array.map (fun label -> [| Table.cell label |]) labels)

let positional_rows = Array.to_list rows

let value_rows =
  Thumper.black_box
    (List.init count (fun i -> Mosaic.Table.row labels.(i) rows.(i)))

(* Selection changes share the same hierarchy and expanded IDs. Only the
   selected ID changes; input construction is outside the measured roundtrip. *)
let make_tree_selection count =
  let renderer = Renderer.create () in
  let children =
    List.init (count - 1) (fun i ->
        let id = Printf.sprintf "child-%d" i in
        Tree.item ~id id)
  in
  let items = Thumper.black_box [ Tree.item ~id:"root" ~children "Root" ] in
  let first = Tree.State.make ~selected:"root" ~expanded:[ "root" ] () in
  let last =
    Tree.State.select (Some (Printf.sprintf "child-%d" (count - 2))) first
  in
  let first_props =
    Thumper.black_box (Tree.Props.make ~items ~state:first ())
  in
  let last_props = Thumper.black_box (Tree.Props.make ~items ~state:last ()) in
  let tree =
    Tree.create ~parent:(Renderer.root renderer)
      (Tree.Props.make ~items ~state:first ())
  in
  (renderer, tree, first_props, last_props)

let tree_selection_roundtrip (renderer, tree, first, last) =
  ignore (Sys.opaque_identity renderer : Renderer.t);
  Tree.apply_props tree last;
  let last_index = Tree.selected_index tree in
  Tree.apply_props tree first;
  last_index + Tree.selected_index tree

(* A key on 100,000 items: Down then Up from the middle, with the cursor's
   hint at its item, as after an earlier key. *)
let cursor_items =
  Thumper.black_box
    (Items.of_array (Array.init 100_000 (Printf.sprintf "item-%d")))

let cursor_step c a =
  Cursor.step ~equal:String.equal ~name:Fun.id ~page:20 cursor_items c a

let cursor_middle =
  match cursor_step (Cursor.at "item-0") (By 50_000) with
  | Moved c -> c
  | _ -> assert false

let cursor_down_up () =
  match cursor_step cursor_middle Next with
  | Moved c -> cursor_step c Prev
  | outcome -> outcome

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "selection"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words; promoted_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
         ]))
    [
      Thumper.bench "select/positional-5000" (fun () ->
          Select.Props.make ~options:select_options ~selected_index:last ());
      Thumper.bench "select/value-5000" (fun () ->
          Mosaic.select ~equal:String.equal ~selected ~on_change:Fun.id
            ~on_activate:Fun.id select_items);
      Thumper.bench "tabs/positional-5000" (fun () ->
          Tab_select.Props.make ~options:tab_options ~selected:last ());
      Thumper.bench "tabs/value-5000" (fun () ->
          Mosaic.tab_select ~equal:String.equal ~selected ~on_change:Fun.id
            ~on_activate:Fun.id tab_items);
      Thumper.bench "table/positional-5000" (fun () ->
          Table.Props.make ~columns ~rows:positional_rows ~selected_row:last ());
      Thumper.bench "table/value-5000" (fun () ->
          Mosaic.table ~columns ~rows:value_rows ~equal:String.equal ~selected
            ~on_change:Fun.id ~on_activate:Fun.id ());
      Thumper.bench "cursor/step-100000" cursor_down_up;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_tree_selection 1_000)
        "tree/selection-roundtrip-1000" tree_selection_roundtrip;
      Thumper.bench_with_setup
        ~setup:(fun () -> make_tree_selection 10_000)
        "tree/selection-roundtrip-10000" tree_selection_roundtrip;
    ]
