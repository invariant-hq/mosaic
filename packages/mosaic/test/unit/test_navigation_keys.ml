open Windtrap
open Mosaic_ui
module Key = Matrix_input.Keystroke

type bindings = Default | Override | Replace | Precedence

let bindings mode ~key ~forward ~backward =
  match mode with
  | Default -> []
  | Override ->
      [
        (Mosaic.Shortcut.key key, backward); (Mosaic.Shortcut.char 'n', forward);
      ]
  | Replace -> [ (Mosaic.Shortcut.char 'n', backward) ]
  | Precedence ->
      [
        (Shortcut.char 'q', backward);
        (Shortcut.char 'Q', backward);
        (Shortcut.char 'q', forward);
        (Shortcut.char 'x', backward);
      ]

let on_key prevent event =
  if prevent then Event.Key.prevent_default event;
  None

let change value = "change:" ^ value
let activate value = "activate:" ^ value
let values = [ "A"; "B"; "C" ]
let size = Mosaic.size ~width:30 ~height:6

let tabs ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.tab_select else Mosaic.tab_select in
  create ~size ~focused:true ~equal:String.equal ~selected:(Some selected)
    ~on_change:change ~on_activate:activate ~on_key:(on_key prevent)
    ~show_description:false ~tab_width:8
    ~key_bindings:
      (bindings mode ~key:Key.Right ~forward:Mosaic.Tab_select.Move_right
         ~backward:Mosaic.Tab_select.Move_left)
    (List.map (fun value -> Mosaic.Tab_select.item value value) values)

let table ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.table else Mosaic.table in
  create ~size ~focused:true ~equal:String.equal ~selected:(Some selected)
    ~on_change:change ~on_activate:activate ~on_key:(on_key prevent)
    ~show_header:false ~border:false
    ~key_bindings:
      (bindings mode ~key:Key.Down ~forward:Mosaic.Table.Move_down
         ~backward:Mosaic.Table.Move_up)
    ~columns:[ Mosaic.Table.column "Value" ]
    ~rows:
      (List.map
         (fun value -> Mosaic.Table.row value [| Mosaic.Table.cell value |])
         values)
    ()

let tree ~mlx ~mode ~selected ~prevent =
  let create = if mlx then Mosaic_mlx.tree else Mosaic.tree in
  create ~size ~focused:true
    ~state:(Mosaic.Tree.State.make ~selected ())
    ~on_change:(fun state ->
      change (Option.get (Mosaic.Tree.State.selected state)))
    ~on_activate:(fun item -> activate item.Mosaic.Tree.id)
    ~on_key:(on_key prevent)
    ~key_bindings:
      (bindings mode ~key:Key.Down ~forward:Mosaic.Tree.Move_down
         ~backward:Mosaic.Tree.Move_up)
    ~items:(List.map (fun id -> Mosaic.Tree.item ~id id) values)
    ()

let widgets =
  [
    ("tabs", tabs, Key.Right);
    ("table", table, Key.Down);
    ("tree", tree, Key.Down);
  ]

let fixture build ~mlx =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let received = ref [] in
  let render ?(mode = Default) ?(selected = "B") ?(prevent = false) () =
    Mosaic.Reconciler.render reconciler ~viewport_width:30
      (Mosaic.map
         (fun message -> received := message :: !received)
         (build ~mlx ~mode ~selected ~prevent));
    (match Mosaic.Reconciler.focused reconciler with
    | Some node -> ignore (Renderer.focus renderer node : bool)
    | None -> failwith "navigation fixture has no focused widget");
    Renderer.render_frame renderer ~width:30 ~height:6 ~dt:0.
  in
  let send ?mods ?repeat ?base ?keypad key =
    let event = Event.Key.of_input (Key.make ?mods ?repeat ?base ?keypad key) in
    Renderer.dispatch_key renderer (Renderer.focus_path renderer) event
  in
  let take () =
    let result = List.rev !received in
    received := [];
    result
  in
  (render, send, take)

(* A release is not a key event: widgets never see one. *)
let exact_matching_and_accepted_state build forward ~mlx =
  let render, send, take = fixture build ~mlx in
  render ();
  List.iter
    (fun mods ->
      send ~mods forward;
      if not mods.ctrl then send ~mods (Key.Char (Uchar.of_char 'j'));
      equal ~msg:"modified navigation has no action" (list string) [] (take ()))
    [
      { Key.no_mods with ctrl = true };
      { Key.no_mods with alt = true };
      { Key.no_mods with hyper = true };
    ];
  send ~mods:{ Key.no_mods with ctrl = true } (Key.Char (Uchar.of_char 'j'));
  equal ~msg:"Ctrl+J, a line feed, activates as Enter does" (list string)
    [ "activate:B" ] (take ());
  send ~repeat:true forward;
  send ~repeat:true forward;
  equal ~msg:"each repeat proposes from accepted selection" (list string)
    [ "change:C"; "change:C" ] (take ());
  send ~keypad:true Key.Enter;
  equal ~msg:"unaccepted proposals do not alter activation" (list string)
    [ "activate:B" ] (take ());
  render ~selected:"C" ();
  send Key.Enter;
  equal ~msg:"accepted declaration changes activation" (list string)
    [ "activate:C" ] (take ());
  render ();
  send ~keypad:true forward;
  equal ~msg:"keypad navigation shares typed matching" (list string)
    [ "change:C" ] (take ());
  render ~prevent:true ();
  send forward;
  send Key.Enter;
  equal ~msg:"ordinary on_key prevention suppresses widget defaults"
    (list string) [] (take ())

let custom_binding_lifecycle build forward ~mlx =
  let render, send, take = fixture build ~mlx in
  render ~mode:Override ();
  send forward;
  equal ~msg:"custom binding overrides default" (list string) [ "change:A" ]
    (take ());
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"custom binding adds a key" (list string) [ "change:C" ] (take ());
  send ~base:(Uchar.of_char 'n') (Key.Char (Uchar.of_int 0x043d));
  equal ~msg:"custom binding uses base-layout fallback" (list string)
    [ "change:C" ] (take ());
  render ~mode:Replace ();
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"replacement changes the existing key action" (list string)
    [ "change:A" ] (take ());
  send forward;
  equal ~msg:"replacement restores omitted default override" (list string)
    [ "change:C" ] (take ());
  render ();
  send (Key.Char (Uchar.of_char 'n'));
  equal ~msg:"removing custom bindings removes the added key" (list string) []
    (take ());
  send forward;
  equal ~msg:"default navigation remains after removal" (list string)
    [ "change:C" ] (take ())

let subscriptions_and_widgets_agree build ~mlx =
  let render, send, take = fixture build ~mlx in
  render ~mode:Precedence ();
  let bindings =
    bindings Precedence ~key:Key.Down ~forward:"change:C" ~backward:"change:A"
  in
  let dispatch =
    match Mosaic.Sub.on_keys bindings with
    | Mosaic.Sub.On_key (_, f) -> f
    | _ -> fail "expected key subscription"
  in
  List.iter
    (fun (key, base, repeat) ->
      let data = Key.make ?base ~repeat key in
      let expected = dispatch (Event.Key.of_input data) in
      send ?base ~repeat key;
      equal ~msg:"subscription and compiled widget resolve the same binding"
        (list string) (Option.to_list expected) (take ()))
    [
      (Key.Char (Uchar.of_char 'q'), None, false);
      (Key.Char (Uchar.of_char 'Q'), None, true);
      (Key.Char (Uchar.of_char 'x'), Some (Uchar.of_char 'q'), false);
      (Key.Char (Uchar.of_char 'X'), Some (Uchar.of_char 'q'), false);
      (Key.Char (Uchar.of_int 0x0441), Some (Uchar.of_char 'Q'), false);
    ]

(* The keys a widget's interface lists are the keys its behavior consumes;
   any other key passes on to its ancestors and the application. *)
let consumes view ~acts ~passes () =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  Mosaic.Reconciler.render reconciler ~viewport_width:30
    (Mosaic.map ignore view);
  (match Mosaic.Reconciler.focused reconciler with
  | Some node -> ignore (Renderer.focus renderer node : bool)
  | None -> failwith "consumption fixture has no focused widget");
  Renderer.render_frame renderer ~width:30 ~height:6 ~dt:0.;
  let consumed (mods, key) =
    let event = Event.Key.of_input (Key.make ~mods key) in
    Renderer.dispatch_key renderer (Renderer.focus_path renderer) event;
    Event.Key.propagation_stopped event
  in
  List.iter
    (fun key -> is_true ~msg:"a listed key is consumed" (consumed key))
    acts;
  List.iter
    (fun key -> is_false ~msg:"an unlisted key passes on" (consumed key))
    passes

let plain key = (Key.no_mods, key)
let shift key = ({ Key.no_mods with shift = true }, key)
let ctrl c = ({ Key.no_mods with ctrl = true }, Key.Char (Uchar.of_char c))
let alt key = ({ Key.no_mods with alt = true }, key)
let super key = ({ Key.no_mods with super = true }, key)
let char c = plain (Key.Char (Uchar.of_char c))

let consumed_keys =
  let items = List.map (fun v -> Mosaic.Select.item v v) values in
  [
    ( "select",
      consumes
        (Mosaic.select ~size ~focused:true ~equal:String.equal
           ~selected:(Some "A") ~on_change:ignore items)
        ~acts:
          [
            plain Key.Up;
            plain Key.Down;
            char 'k';
            char 'j';
            shift Key.Up;
            shift Key.Down;
            plain Key.Enter;
          ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Left; plain Key.Tab ] );
    ( "tab_select",
      consumes
        (Mosaic.tab_select ~size ~focused:true ~equal:String.equal
           ~selected:(Some "A") ~on_change:ignore
           (List.map (fun v -> Mosaic.Tab_select.item v v) values))
        ~acts:
          [
            plain Key.Left;
            plain Key.Right;
            char '[';
            char ']';
            plain Key.Enter;
            ctrl 'j';
          ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Up ] );
    ( "table",
      consumes
        (Mosaic.table ~size ~focused:true ~equal:String.equal
           ~selected:(Some "A") ~on_change:ignore
           ~columns:[ Mosaic.Table.column "Value" ]
           ~rows:
             (List.map
                (fun v -> Mosaic.Table.row v [| Mosaic.Table.cell v |])
                values)
           ())
        ~acts:
          [
            plain Key.Up;
            plain Key.Down;
            char 'k';
            char 'j';
            shift Key.Up;
            shift Key.Down;
            plain Key.Page_up;
            plain Key.Page_down;
            plain Key.Enter;
            ctrl 'j';
          ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Left ] );
    ( "tree",
      consumes
        (Mosaic.tree ~size ~focused:true
           ~state:(Mosaic.Tree.State.make ~selected:"A" ())
           ~on_change:ignore
           ~items:(List.map (fun id -> Mosaic.Tree.item ~id id) values)
           ())
        ~acts:
          [
            plain Key.Up;
            plain Key.Down;
            char 'k';
            char 'j';
            shift Key.Up;
            shift Key.Down;
            plain Key.Right;
            plain Key.Left;
            char ' ';
            plain Key.Enter;
            ctrl 'j';
          ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Page_up ] );
    (* The content fits: arrows at the edge stay consumed along a scrolled
       axis, and pass on along the other. *)
    ( "scroll_box, vertical",
      consumes
        (Mosaic.scroll_box ~size ~focused:true ~scroll:(Mosaic.Scroll.at 0)
           ~on_scroll:ignore
           [ Mosaic.text "content" ])
        ~acts:
          [
            plain Key.Up;
            char 'k';
            plain Key.Down;
            char 'j';
            plain Key.Page_up;
            plain Key.Page_down;
            plain Key.Home;
            plain Key.End;
          ]
        ~passes:
          [
            plain Key.Left;
            char 'h';
            plain Key.Right;
            char 'l';
            char 'q';
            plain Key.Escape;
            plain Key.Enter;
          ] );
    ( "scroll_box, horizontal",
      consumes
        (Mosaic.scroll_box ~size ~focused:true ~scroll_x:true ~scroll_y:false
           ~scroll:(Mosaic.Scroll.at 0) ~on_scroll:ignore
           [ Mosaic.text "content" ])
        ~acts:
          [
            plain Key.Left;
            char 'h';
            plain Key.Right;
            char 'l';
            plain Key.Page_up;
            plain Key.Page_down;
            plain Key.Home;
            plain Key.End;
          ]
        ~passes:[ plain Key.Up; char 'k'; plain Key.Down; char 'j'; char 'q' ]
    );
    ( "scroll_box, both axes",
      consumes
        (Mosaic.scroll_box ~size ~focused:true ~scroll_x:true
           ~scroll:(Mosaic.Scroll.at 0) ~on_scroll:ignore
           [ Mosaic.text "content" ])
        ~acts:
          [
            plain Key.Up;
            char 'k';
            plain Key.Down;
            char 'j';
            plain Key.Left;
            char 'h';
            plain Key.Right;
            char 'l';
            plain Key.Page_up;
            plain Key.Home;
          ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Enter ] );
    ( "scroll_box, no axis",
      consumes
        (Mosaic.scroll_box ~size ~focused:true ~scroll_y:false
           ~scroll:(Mosaic.Scroll.at 0) ~on_scroll:ignore
           [ Mosaic.text "content" ])
        ~acts:[]
        ~passes:
          [ plain Key.Up; plain Key.Left; plain Key.Page_down; plain Key.End ]
    );
    ( "input",
      consumes
        (Mosaic.input ~size ~focused:true ~state:Mosaic.Editor.empty
           ~on_change:ignore ())
        ~acts:
          [
            char 'a';
            plain Key.Left;
            plain Key.Backspace;
            ctrl 'a';
            plain Key.Enter;
            ctrl 'j';
            ctrl 'z';
            super (Key.Char (Uchar.of_char 'a'));
          ]
        ~passes:
          [
            plain Key.Up;
            plain Key.Down;
            shift Key.Up;
            plain Key.Tab;
            plain Key.Escape;
            plain (Key.F 5);
            ctrl 'q';
          ] );
    ( "textarea",
      consumes
        (Mosaic.textarea ~size ~focused:true ~state:Mosaic.Editor.empty
           ~on_change:ignore ())
        ~acts:
          [
            char 'a';
            plain Key.Up;
            plain Key.Down;
            plain Key.Enter;
            ctrl 'k';
            alt Key.Enter;
          ]
        ~passes:[ plain Key.Tab; plain Key.Escape; plain (Key.F 5); ctrl 'q' ]
    );
    ( "button",
      consumes
        (Mosaic.button ~size ~focused:true ~on_activate:() "Save")
        ~acts:[ plain Key.Enter; char ' ' ]
        ~passes:[ char 'q'; plain Key.Escape; plain Key.Tab ] );
  ]

let () =
  run "mosaic.navigation-keys"
    (List.concat_map
       (fun (name, build, forward) ->
         List.map
           (fun mlx ->
             let api = if mlx then "MLX" else "Mosaic" in
             group
               (name ^ " " ^ api)
               [
                 test "matching, interception and model ownership" (fun () ->
                     exact_matching_and_accepted_state build forward ~mlx);
                 test "custom binding lifecycle" (fun () ->
                     custom_binding_lifecycle build forward ~mlx);
                 test "subscription and widget resolution agree" (fun () ->
                     subscriptions_and_widgets_agree build ~mlx);
               ])
           [ false; true ])
       widgets
    @ [
        group "consumed keys"
          (List.map (fun (name, check) -> test name check) consumed_keys);
      ])
