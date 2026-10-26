open Windtrap
open Mosaic_ui
module Ansi = Mosaic.Ansi

type message = Changed of string | Activated of string

let selected_bg = Ansi.Color.of_rgb 17 83 149
let changed value = Changed value
let activated value = Activated value

(* [f values], the same list while [values] is the same list, as an
   application keeps its items in its model. *)
let kept f =
  let last = ref None in
  fun values ->
    match !last with
    | Some (previous, items) when previous == values -> items
    | _ ->
        let items = f values in
        last := Some (values, items);
        items

let select_items = kept (List.map (fun value -> Mosaic.Select.item value value))

let tab_items =
  kept (List.map (fun value -> Mosaic.Tab_select.item value value))

let table_rows =
  kept
    (List.map (fun value ->
         Mosaic.Table.row value [| Mosaic.Table.cell value |]))

let select ?key ~equal ~selected values =
  Mosaic.select ?key
    ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
    ~equal ~selected ~focused:true ~on_change:changed ~on_activate:activated
    ~show_description:false ~background:Ansi.Color.black
    ~focused_background:Ansi.Color.black ~selected_background:selected_bg
    (select_items values)

let tabs ?key ~equal ~selected values =
  Mosaic.tab_select ?key
    ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
    ~equal ~selected ~focused:true ~on_change:changed ~on_activate:activated
    ~show_description:false ~show_underline:false ~tab_width:8
    ~background:Ansi.Color.black ~focused_background:Ansi.Color.black
    ~selected_background:selected_bg (tab_items values)

let table ?key ~equal ~selected values =
  Mosaic.table ?key
    ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
    ~equal ~selected ~focused:true ~on_change:changed ~on_activate:activated
    ~show_header:false ~border:false ~cell_padding:0
    ~background:Ansi.Color.black ~selected_background:selected_bg
    ~focused_selected_background:selected_bg
    ~columns:[ Mosaic.Table.column ~width:(`Fixed 8) "" ]
    ~rows:(table_rows values) ()

let tree ?key ~equal:_ ~selected values =
  Mosaic.tree ?key
    ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
    ~focused:true ~background:Ansi.Color.black ~selected_background:selected_bg
    ~focused_selected_background:selected_bg
    ~items:(List.map (fun value -> Mosaic.Tree.item ~id:value value) values)
    ~state:(Mosaic.Tree.State.make ?selected ())
    ~on_change:(fun state ->
      changed (Option.value ~default:"" (Mosaic.Tree.State.selected state)))
    ()

let widgets =
  [
    ( "select",
      select ?key:None,
      Matrix_input.Keystroke.Down,
      Matrix_input.Keystroke.Up );
    ( "tabs",
      tabs ?key:None,
      Matrix_input.Keystroke.Right,
      Matrix_input.Keystroke.Left );
    ( "table",
      table ?key:None,
      Matrix_input.Keystroke.Down,
      Matrix_input.Keystroke.Up );
  ]

let fixture build =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  let messages = ref [] in
  let receive = function
    | Changed value -> messages := ("change:" ^ value) :: !messages
    | Activated value -> messages := ("activate:" ^ value) :: !messages
  in
  let render ?(equal = String.equal) selected values =
    let view = build ~equal ~selected values in
    Mosaic.Reconciler.render reconciler ~viewport_width:24
      (Vnode.map receive view);
    (match Mosaic.Reconciler.focused reconciler with
    | Some node -> ignore (Renderer.focus renderer node : bool)
    | None -> Renderer.blur renderer);
    Renderer.render_frame renderer ~width:24 ~height:6 ~dt:0.
  in
  let key key =
    ignore
      (Test_harness.dispatch_key renderer (Matrix_input.Keystroke.make key)
        : Event.key)
  in
  let take () =
    let result = List.rev !messages in
    messages := [];
    result
  in
  let highlighted () =
    let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
    let found = ref false in
    for i = 0 to (Matrix_grid.width grid * Matrix_grid.height grid) - 1 do
      if Ansi.Color.equal (Matrix_grid.get_bg grid i) selected_bg then
        found := true
    done;
    !found
  in
  (render, key, take, highlighted)

(* A list longer than its viewport, rendered under [key], with the screen it
   shows. A wheel over it proposes a value the caller may reject. *)
let viewport_fixture build =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  let messages = ref [] in
  let receive = function
    | Changed value -> messages := value :: !messages
    | Activated _ -> ()
  in
  let values = List.init 20 (Printf.sprintf "v%02d") in
  let render ~key selected =
    Mosaic.Reconciler.render reconciler ~viewport_width:24
      (Vnode.map receive
         (build ?key:(Some key) ~equal:String.equal ~selected:(Some selected)
            values));
    Renderer.render_frame renderer ~width:24 ~height:6 ~dt:0.;
    ignore (Renderer.render ~full:true renderer : string);
    let grid = Matrix_screen.current_grid (Renderer.screen renderer) in
    String.concat ""
      (List.init
         (Matrix_grid.width grid * Matrix_grid.height grid)
         (Matrix_grid.get_text grid))
  in
  let wheel direction =
    ignore
      (Test_harness.dispatch_mouse renderer
         (Test_harness.mouse_report ~x:2 ~y:2 (Scroll direction))
        : Event.mouse);
    let proposed = !messages in
    messages := [];
    proposed
  in
  (render, wheel)

(* The viewport follows the accepted value and the geometry alone: a remounted
   widget shows the same rows, and a rejected wheel proposal scrolls nothing. *)
let viewport_follows_value ~wheel_proposes build =
  let render, wheel = viewport_fixture build in
  let shown = render ~key:"a" "v15" in
  equal ~msg:"remounted widget keeps its viewport" string shown
    (render ~key:"b" "v15");
  let proposed = wheel `Down in
  equal ~msg:"rejected wheel proposal does not scroll" string shown
    (render ~key:"b" "v15");
  if wheel_proposes then begin
    equal ~msg:"the wheel proposes the next value" (list string) [ "v16" ]
      proposed;
    is_false ~msg:"accepted wheel proposal scrolls"
      (String.equal shown (render ~key:"b" "v16"))
  end
  else equal ~msg:"no wheel proposal" (list string) [] proposed

let reorder_and_filter build forward =
  let render, key, take, highlighted = fixture build in
  render (Some "B") [ "A"; "B"; "C" ];
  is_true ~msg:"accepted value is highlighted" (highlighted ());
  key Enter;
  equal (list string) [ "activate:B" ] (take ());
  render (Some "B") [ "C"; "A"; "B" ];
  key Enter;
  equal ~msg:"reorder preserves domain selection" (list string) [ "activate:B" ]
    (take ());
  render (Some "B") [ "A"; "C" ];
  is_false ~msg:"filtered value does not select another item" (highlighted ());
  key Enter;
  equal ~msg:"missing value cannot activate an unrelated row" (list string) []
    (take ());
  key forward;
  equal ~msg:"navigation proposes a current value" (list string) [ "change:A" ]
    (take ());
  key Enter;
  equal ~msg:"unaccepted navigation does not create selection" (list string) []
    (take ());
  render (Some "A") [ "C"; "A" ];
  key Enter;
  equal ~msg:"accepted proposal activates its value after reorder" (list string)
    [ "activate:A" ] (take ())

let absent_and_empty build forward backward =
  let render, key, take, highlighted = fixture build in
  render None [ "A"; "B"; "C" ];
  is_false ~msg:"None has no highlight" (highlighted ());
  key Enter;
  equal (list string) [] (take ());
  key backward;
  equal ~msg:"backward navigation begins at last item" (list string)
    [ "change:C" ] (take ());
  key forward;
  equal ~msg:"rejected backward proposal leaves no selection" (list string)
    [ "change:A" ] (take ());
  render (Some "A") [];
  key Enter;
  key forward;
  key backward;
  equal ~msg:"empty collection produces no value callbacks" (list string) []
    (take ());
  is_false ~msg:"empty collection has no highlight" (highlighted ())

let custom_equality build =
  let render, key, take, highlighted = fixture build in
  let equal_value a b =
    String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)
  in
  render ~equal:equal_value (Some "b") [ "A"; "B" ];
  is_true ~msg:"selection uses caller equality" (highlighted ());
  key Enter;
  equal ~msg:"callback returns current collection value" (list string)
    [ "activate:B" ] (take ())

(* A widget declared again with the same list, physically, reuses its
   conversion of the list. Its selection and the values of its callbacks
   follow each declaration as they do for a new list: after a step, a jump,
   an absent value, another list, a copy of the list and a new equality, and
   the first of two equal values is the one selected. *)
let same_list_declared_again build =
  let render, key, take, highlighted = fixture build in
  let values prefix = List.init 200 (Printf.sprintf "%s%03d" prefix) in
  let first = values "v" and second = values "w" in
  let activates ?by selected values expected =
    render ?equal:by selected values;
    key Enter;
    equal
      ~msg:("activates " ^ String.concat "," expected)
      (list string)
      (List.map (( ^ ) "activate:") expected)
      (take ())
  in
  activates (Some "v100") first [ "v100" ];
  activates (Some "v101") first [ "v101" ];
  activates (Some "v000") first [ "v000" ];
  activates (Some "v199") first [ "v199" ];
  render (Some "x") first;
  is_false ~msg:"an absent value has no highlight" (highlighted ());
  activates (Some "x") first [];
  activates (Some "w150") second [ "w150" ];
  activates (Some "v020") first [ "v020" ];
  activates (Some "v050") (values "v") [ "v050" ];
  let same_hundred a b = String.sub a 0 2 = String.sub b 0 2 in
  activates ~by:same_hundred (Some "v150") first [ "v100" ];
  activates ~by:same_hundred (Some "v100") first [ "v100" ]

let mlx_selection_values () =
  let build ~equal ~selected values =
    Mosaic_mlx.select
      ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
      ~equal ~selected ~focused:true ~on_change:changed ~on_activate:activated
      (List.map (fun value -> Mosaic_mlx.Select.item value value) values)
  in
  let render, key, take, highlighted = fixture build in
  render (Some "B") [ "A"; "B" ];
  ignore highlighted;
  key Enter;
  equal (list string) [ "activate:B" ] (take ())

let table_press_reconciliation ~trigger ~replacement expected =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  let messages = ref [] in
  let rec render rows =
    let view =
      match rows with
      | None -> Mosaic.empty
      | Some values ->
          Mosaic.table
            ~size:{ width = Mosaic.px 24; height = Mosaic.px 6 }
            ~equal:String.equal ~selected:None ~focused:true
            ~on_change:(fun value -> "change:" ^ value)
            ~on_activate:(fun value -> "activate:" ^ value)
            ~on_hover:(Option.map (fun value -> "hover:" ^ value))
            ~activate_on_click:true ~show_header:false ~border:false
            ~cell_padding:0
            ~columns:[ Mosaic.Table.column ~width:(`Fixed 8) "" ]
            ~rows:
              (List.map
                 (fun value ->
                   Mosaic.Table.row value [| Mosaic.Table.cell "same" |])
                 values)
            ()
    in
    Mosaic.Reconciler.render reconciler ~viewport_width:24
      (Vnode.map receive view);
    (match Mosaic.Reconciler.focused reconciler with
    | Some node -> ignore (Renderer.focus renderer node : bool)
    | None -> Renderer.blur renderer);
    Renderer.prepare_layout renderer ~width:24 ~height:6
  and receive message =
    messages := message :: !messages;
    if String.equal message trigger then render replacement
  in
  render (Some [ "A"; "B" ]);
  Renderer.render_frame renderer ~width:24 ~height:6 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let mouse = Test_harness.mouse_report ~x:1 ~y:1 (Press Left) in
  ignore (Test_harness.dispatch_mouse renderer mouse : Event.mouse);
  equal (list string) expected (List.rev !messages)

let table_press_tests =
  let complete = [ "hover:B"; "change:B"; "activate:B" ] in
  [
    test "table hover preserves clicked value across identical presentations"
      (fun () ->
        table_press_reconciliation ~trigger:"hover:B"
          ~replacement:(Some [ "C"; "D" ])
          complete);
    test "table change preserves clicked value across identical presentations"
      (fun () ->
        table_press_reconciliation ~trigger:"change:B"
          ~replacement:(Some [ "C"; "D" ])
          complete);
    test "table change preserves clicked value after filtering all rows"
      (fun () ->
        table_press_reconciliation ~trigger:"change:B" ~replacement:(Some [])
          complete);
    test "table hover preserves clicked value after filtering all rows"
      (fun () ->
        table_press_reconciliation ~trigger:"hover:B" ~replacement:(Some [])
          complete);
    test "table destruction during hover cancels remaining press callbacks"
      (fun () ->
        table_press_reconciliation ~trigger:"hover:B" ~replacement:None
          [ "hover:B" ]);
    test "table destruction during change cancels activation" (fun () ->
        table_press_reconciliation ~trigger:"change:B" ~replacement:None
          [ "hover:B"; "change:B" ]);
  ]

let () =
  run "value selection"
    (table_press_tests
    @ test "MLX preserves domain callback values" mlx_selection_values
      :: List.map
           (fun (name, build, forward, backward) ->
             group name
               [
                 test "reorder, filter and accepted activation" (fun () ->
                     reorder_and_filter build forward);
                 test "absent and empty selection" (fun () ->
                     absent_and_empty build forward backward);
                 test "custom equality" (fun () -> custom_equality build);
                 test "same list declared again" (fun () ->
                     same_list_declared_again build);
               ])
           widgets
    @ List.map
        (fun (name, wheel_proposes, build) ->
          test (name ^ " viewport follows the accepted value") (fun () ->
              viewport_follows_value ~wheel_proposes build))
        [
          ("select", true, select);
          ("tabs", false, tabs);
          ("table", true, table);
          ("tree", true, tree);
        ])
