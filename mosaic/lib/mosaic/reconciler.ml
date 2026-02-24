open Mosaic_ui

(* ───── Instance Operations ───── *)

type instance =
  | Box_instance of Box.t
  | Text_instance of Text.t
  | Slider_instance of Slider.t
  | Text_input_instance of Text_input.t
  | Select_instance of Select.t
  | Tab_select_instance of Tab_select.t
  | Canvas_instance of Canvas.t
  | Spinner_instance of Spinner.t
  | Progress_bar_instance of Progress_bar.t
  | Scroll_bar_instance of Scroll_bar.t
  | Scroll_box_instance of Scroll_box.t
  | Textarea_instance of Textarea.t
  | Table_instance of Table.t
  | Code_instance of Code.t
  | Line_number_instance of Line_number.t
  | Markdown_instance of Markdown.t
  | Tree_instance of Tree.t

let node_of = function
  | Box_instance b -> Box.node b
  | Text_instance t -> Text.node t
  | Slider_instance s -> Slider.node s
  | Text_input_instance i -> Text_input.node i
  | Select_instance s -> Select.node s
  | Tab_select_instance ts -> Tab_select.node ts
  | Canvas_instance c -> Canvas.node c
  | Spinner_instance s -> Spinner.node s
  | Progress_bar_instance pb -> Progress_bar.node pb
  | Scroll_bar_instance sb -> Scroll_bar.node sb
  | Scroll_box_instance sb -> Scroll_box.node sb
  | Textarea_instance ta -> Textarea.node ta
  | Table_instance t -> Table.node t
  | Code_instance c -> Code.node c
  | Line_number_instance ln -> Line_number.node ln
  | Markdown_instance md -> Markdown.node md
  | Tree_instance tr -> Tree.node tr

let create_instance ~(parent : Renderable.t) (kind : Vnode.kind)
    (attrs : Vnode.attrs) : instance =
  let inst =
    match kind with
    | Vnode.Box spec ->
        let box =
          Box.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Box.apply_props box spec;
        Box_instance box
    | Vnode.Text spec ->
        let text =
          Text.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Text.apply_props text spec;
        Text_instance text
    | Vnode.Slider spec ->
        let slider =
          Slider.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Slider.apply_props slider spec;
        Slider_instance slider
    | Vnode.Text_input spec ->
        let input =
          Text_input.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Text_input.apply_props input spec;
        Text_input_instance input
    | Vnode.Canvas spec ->
        let canvas =
          Canvas.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Canvas.apply_props canvas spec;
        Canvas_instance canvas
    | Vnode.Select spec ->
        let select =
          Select.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Select.apply_props select spec;
        Select_instance select
    | Vnode.Tab_select spec ->
        let tab_select =
          Tab_select.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ~options:[] ()
        in
        Tab_select.apply_props tab_select spec;
        Tab_select_instance tab_select
    | Vnode.Spinner spec ->
        let spinner =
          Spinner.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Spinner.apply_props spinner spec;
        Spinner_instance spinner
    | Vnode.Progress_bar spec ->
        let progress_bar =
          Progress_bar.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Progress_bar.apply_props progress_bar spec;
        Progress_bar_instance progress_bar
    | Vnode.Scroll_bar spec ->
        let scroll_bar =
          Scroll_bar.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Scroll_bar.apply_props scroll_bar spec;
        Scroll_bar_instance scroll_bar
    | Vnode.Scroll_box spec ->
        let scroll_box =
          Scroll_box.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Scroll_box.apply_props scroll_box spec;
        Scroll_box_instance scroll_box
    | Vnode.Textarea spec ->
        let textarea =
          Textarea.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Textarea.apply_props textarea spec;
        Textarea_instance textarea
    | Vnode.Table spec ->
        let table =
          Table.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Table.apply_props table spec;
        Table_instance table
    | Vnode.Code spec ->
        let code =
          Code.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Code.apply_props code spec;
        Code_instance code
    | Vnode.Line_number spec ->
        let ln =
          Line_number.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Line_number.apply_props ln spec;
        Line_number_instance ln
    | Vnode.Markdown spec ->
        let md =
          Markdown.create ~parent ?id:attrs.id ~layout_style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Markdown.apply_props md spec;
        Markdown_instance md
    | Vnode.Tree spec ->
        let tree =
          Tree.create ~parent ?id:attrs.id ~style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Tree.apply_props tree spec;
        Tree_instance tree
  in
  let node = node_of inst in
  if attrs.focusable then Renderable.set_focusable node true;
  (* Autofocus is deferred to after commit_placement in reconcile_flattened, so
     the node is fully attached to the render tree when focus is applied. *)
  if attrs.buffered then Renderable.set_buffered node true;
  if attrs.live then Renderable.set_live node true;
  inst

let update_common (node : Renderable.t) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) : bool =
  let changed = ref false in
  if old_attrs.visible <> new_attrs.visible then (
    Renderable.set_visible node new_attrs.visible;
    changed := true);
  if old_attrs.z_index <> new_attrs.z_index then (
    Renderable.set_z_index node new_attrs.z_index;
    changed := true);
  if old_attrs.opacity <> new_attrs.opacity then (
    Renderable.set_opacity node new_attrs.opacity;
    changed := true);
  if old_attrs.focusable <> new_attrs.focusable then (
    Renderable.set_focusable node new_attrs.focusable;
    changed := true);
  if old_attrs.buffered <> new_attrs.buffered then (
    Renderable.set_buffered node new_attrs.buffered;
    changed := true);
  if old_attrs.live <> new_attrs.live then (
    Renderable.set_live node new_attrs.live;
    changed := true);
  (* Style is handled in update_instance for widget-specific behavior.
     Box.set_style adds border insets; Renderable.set_style does not. *)
  !changed

let set_instance_style (inst : instance) (style : Toffee.Style.t) : unit =
  match inst with
  | Box_instance box -> Box.set_style box style
  | Markdown_instance md -> Markdown.set_layout_style md style
  | _ -> Renderable.set_style (node_of inst) style

(* Physical equality check (==) on attrs and kind is the fast path: Vnode.map
   shares both records by reference, so unchanged subtrees skip all diffing. *)
let update_instance (inst : instance) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) ~(old_kind : Vnode.kind) ~(new_kind : Vnode.kind)
    : bool =
  if old_attrs == new_attrs && old_kind == new_kind then false
  else
    let common_changed =
      if old_attrs == new_attrs then false
      else update_common (node_of inst) ~old_attrs ~new_attrs
    in
    let style_changed =
      if old_attrs == new_attrs then false
      else if old_attrs.style <> new_attrs.style then (
        set_instance_style inst new_attrs.style;
        true)
      else false
    in
    let kind_changed =
      if old_kind == new_kind then false
      else
        match (inst, old_kind, new_kind) with
        | Box_instance box, Vnode.Box old_spec, Vnode.Box new_spec ->
            if Box.Props.equal old_spec new_spec then false
            else (
              Box.apply_props box new_spec;
              true)
        | Text_instance text, Vnode.Text old_spec, Vnode.Text new_spec ->
            if Text.Props.equal old_spec new_spec then false
            else (
              Text.apply_props text new_spec;
              true)
        | Slider_instance slider, Vnode.Slider old_spec, Vnode.Slider new_spec
          ->
            if Slider.Props.equal old_spec new_spec then false
            else (
              Slider.apply_props slider new_spec;
              true)
        | ( Text_input_instance input,
            Vnode.Text_input old_spec,
            Vnode.Text_input new_spec ) ->
            if Text_input.Props.equal old_spec new_spec then false
            else (
              Text_input.apply_props input new_spec;
              true)
        | Select_instance select, Vnode.Select old_spec, Vnode.Select new_spec
          ->
            if Select.Props.equal old_spec new_spec then false
            else (
              Select.apply_props select new_spec;
              true)
        | ( Tab_select_instance tab_select,
            Vnode.Tab_select old_spec,
            Vnode.Tab_select new_spec ) ->
            if Tab_select.Props.equal old_spec new_spec then false
            else (
              Tab_select.apply_props tab_select new_spec;
              true)
        | Canvas_instance canvas, Vnode.Canvas old_spec, Vnode.Canvas new_spec
          ->
            if Canvas.Props.equal old_spec new_spec then false
            else (
              Canvas.apply_props canvas new_spec;
              true)
        | ( Spinner_instance spinner,
            Vnode.Spinner old_spec,
            Vnode.Spinner new_spec ) ->
            if Spinner.Props.equal old_spec new_spec then false
            else (
              Spinner.apply_props spinner new_spec;
              true)
        | ( Progress_bar_instance pb,
            Vnode.Progress_bar old_spec,
            Vnode.Progress_bar new_spec ) ->
            if Progress_bar.Props.equal old_spec new_spec then false
            else (
              Progress_bar.apply_props pb new_spec;
              true)
        | ( Scroll_bar_instance scroll_bar,
            Vnode.Scroll_bar old_spec,
            Vnode.Scroll_bar new_spec ) ->
            if Scroll_bar.Props.equal old_spec new_spec then false
            else (
              Scroll_bar.apply_props scroll_bar new_spec;
              true)
        | ( Scroll_box_instance scroll_box,
            Vnode.Scroll_box old_spec,
            Vnode.Scroll_box new_spec ) ->
            if Scroll_box.Props.equal old_spec new_spec then false
            else (
              Scroll_box.apply_props scroll_box new_spec;
              true)
        | ( Textarea_instance textarea,
            Vnode.Textarea old_spec,
            Vnode.Textarea new_spec ) ->
            if Textarea.Props.equal old_spec new_spec then false
            else (
              Textarea.apply_props textarea new_spec;
              true)
        | Table_instance table, Vnode.Table old_spec, Vnode.Table new_spec ->
            if Table.Props.equal old_spec new_spec then false
            else (
              Table.apply_props table new_spec;
              true)
        | Code_instance code, Vnode.Code old_spec, Vnode.Code new_spec ->
            if Code.Props.equal old_spec new_spec then false
            else (
              Code.apply_props code new_spec;
              true)
        | ( Line_number_instance ln,
            Vnode.Line_number old_spec,
            Vnode.Line_number new_spec ) ->
            if Line_number.Props.equal old_spec new_spec then false
            else (
              Line_number.apply_props ln new_spec;
              true)
        | Markdown_instance md, Vnode.Markdown old_spec, Vnode.Markdown new_spec
          ->
            if Markdown.Props.equal old_spec new_spec then false
            else (
              Markdown.apply_props md new_spec;
              true)
        | Tree_instance tree, Vnode.Tree old_spec, Vnode.Tree new_spec ->
            if Tree.Props.equal old_spec new_spec then false
            else (
              Tree.apply_props tree new_spec;
              true)
        | _ ->
            invalid_arg "Reconciler: instance and kind mismatch during update"
    in
    common_changed || style_changed || kind_changed

let commit_update (inst : instance) : unit =
  Renderable.request_render (node_of inst)

(* ───── Fiber Tracking ───── *)

type callback_refs =
  | No_callback_refs
  | Slider_callback_refs of { value_change_ref : (float -> unit) option ref }
  | Input_callback_refs of {
      input_ref : (string -> unit) option ref;
      change_ref : (string -> unit) option ref;
      submit_ref : (string -> unit) option ref;
    }
  | Select_callback_refs of {
      select_change_ref : (int -> unit) option ref;
      select_activate_ref : (int -> unit) option ref;
    }
  | Tab_select_callback_refs of {
      tab_select_change_ref : (int -> unit) option ref;
      tab_select_activate_ref : (int -> unit) option ref;
    }
  | Canvas_callback_refs of {
      draw_ref : (Canvas.t -> delta:float -> unit) option ref;
    }
  | Scroll_bar_callback_refs of {
      scroll_bar_change_ref : (int -> unit) option ref;
    }
  | Scroll_box_callback_refs of {
      scroll_ref : (x:int -> y:int -> unit) option ref;
    }
  | Textarea_callback_refs of {
      textarea_input_ref : (string -> unit) option ref;
      textarea_change_ref : (string -> unit) option ref;
      textarea_submit_ref : (string -> unit) option ref;
      textarea_cursor_ref :
        (cursor:int -> selection:(int * int) option -> unit) option ref;
    }
  | Table_callback_refs of {
      table_change_ref : (int -> unit) option ref;
      table_activate_ref : (int -> unit) option ref;
    }
  | Tree_callback_refs of {
      tree_change_ref : (int -> unit) option ref;
      tree_activate_ref : (int -> unit) option ref;
      tree_expand_ref : (int -> bool -> unit) option ref;
    }

type child = Fiber of fiber | Embedded of Renderable.t

and fiber = {
  mutable kind : Vnode.kind;
  key : string option;
  mutable attrs : Vnode.attrs;
  instance : instance;
  mutable children : child list;
  mouse_ref : (Event.mouse -> unit) option ref;
  key_ref : (Event.key -> unit) option ref;
  paste_ref : (Event.paste -> unit) option ref;
  callback_refs : callback_refs;
}

let fiber_node f = node_of f.instance

(* Two kinds match when they are the same widget variant (Box/Text), regardless
   of their props. *)
let kind_matches (a : Vnode.kind) (b : Vnode.kind) : bool =
  match (a, b) with
  | Vnode.Box _, Vnode.Box _
  | Vnode.Text _, Vnode.Text _
  | Vnode.Slider _, Vnode.Slider _
  | Vnode.Text_input _, Vnode.Text_input _
  | Vnode.Select _, Vnode.Select _
  | Vnode.Tab_select _, Vnode.Tab_select _
  | Vnode.Canvas _, Vnode.Canvas _
  | Vnode.Spinner _, Vnode.Spinner _
  | Vnode.Progress_bar _, Vnode.Progress_bar _
  | Vnode.Scroll_bar _, Vnode.Scroll_bar _
  | Vnode.Scroll_box _, Vnode.Scroll_box _
  | Vnode.Textarea _, Vnode.Textarea _
  | Vnode.Table _, Vnode.Table _
  | Vnode.Code _, Vnode.Code _
  | Vnode.Line_number _, Vnode.Line_number _
  | Vnode.Markdown _, Vnode.Markdown _
  | Vnode.Tree _, Vnode.Tree _ ->
      true
  | _ -> false

(* Register a single stable closure per callback type, dispatching through a
   mutable ref. This avoids re-registering listeners on every render -- only the
   ref target is swapped in [update_fiber]. *)
let create_callback_refs (instance : instance)
    (callbacks : unit Vnode.widget_callbacks) : callback_refs =
  match callbacks with
  | Vnode.No_callbacks -> No_callback_refs
  | Vnode.Slider_callbacks { on_value_change } ->
      let value_change_ref = ref on_value_change in
      (match instance with
      | Slider_instance slider ->
          Slider.set_on_change slider
            (Some
               (fun v ->
                 match !value_change_ref with Some h -> h v | None -> ()))
      | _ -> ());
      Slider_callback_refs { value_change_ref }
  | Vnode.Input_callbacks { on_input; on_change; on_submit } ->
      let input_ref = ref on_input in
      let change_ref = ref on_change in
      let submit_ref = ref on_submit in
      (match instance with
      | Text_input_instance input ->
          Text_input.set_on_input input
            (Some (fun s -> match !input_ref with Some h -> h s | None -> ()));
          Text_input.set_on_change input
            (Some (fun s -> match !change_ref with Some h -> h s | None -> ()));
          Text_input.set_on_submit input
            (Some (fun s -> match !submit_ref with Some h -> h s | None -> ()))
      | _ -> ());
      Input_callback_refs { input_ref; change_ref; submit_ref }
  | Vnode.Select_callbacks { on_change; on_activate } ->
      let select_change_ref = ref on_change in
      let select_activate_ref = ref on_activate in
      (match instance with
      | Select_instance select ->
          Select.set_on_change select
            (Some
               (fun i ->
                 match !select_change_ref with Some h -> h i | None -> ()));
          Select.set_on_activate select
            (Some
               (fun i ->
                 match !select_activate_ref with Some h -> h i | None -> ()))
      | _ -> ());
      Select_callback_refs { select_change_ref; select_activate_ref }
  | Vnode.Tab_select_callbacks { on_change; on_activate } ->
      let tab_select_change_ref = ref on_change in
      let tab_select_activate_ref = ref on_activate in
      (match instance with
      | Tab_select_instance tab_select ->
          Tab_select.set_on_change tab_select
            (Some
               (fun i ->
                 match !tab_select_change_ref with Some h -> h i | None -> ()));
          Tab_select.set_on_activate tab_select
            (Some
               (fun i ->
                 match !tab_select_activate_ref with
                 | Some h -> h i
                 | None -> ()))
      | _ -> ());
      Tab_select_callback_refs
        { tab_select_change_ref; tab_select_activate_ref }
  | Vnode.Canvas_callbacks { on_draw } ->
      let draw_ref = ref on_draw in
      (match instance with
      | Canvas_instance canvas ->
          Canvas.set_on_draw canvas
            (Some
               (fun c ~delta ->
                 match !draw_ref with Some h -> h c ~delta | None -> ()))
      | _ -> ());
      Canvas_callback_refs { draw_ref }
  | Vnode.Scroll_bar_callbacks { on_change } ->
      let scroll_bar_change_ref = ref on_change in
      (match instance with
      | Scroll_bar_instance scroll_bar ->
          Scroll_bar.set_on_change scroll_bar
            (Some
               (fun i ->
                 match !scroll_bar_change_ref with Some h -> h i | None -> ()))
      | _ -> ());
      Scroll_bar_callback_refs { scroll_bar_change_ref }
  | Vnode.Scroll_box_callbacks { on_scroll } ->
      let scroll_ref = ref on_scroll in
      (match instance with
      | Scroll_box_instance scroll_box ->
          Scroll_box.set_on_scroll scroll_box
            (Some
               (fun ~x ~y ->
                 match !scroll_ref with Some h -> h ~x ~y | None -> ()))
      | _ -> ());
      Scroll_box_callback_refs { scroll_ref }
  | Vnode.Textarea_callbacks { on_input; on_change; on_submit; on_cursor } ->
      let textarea_input_ref = ref on_input in
      let textarea_change_ref = ref on_change in
      let textarea_submit_ref = ref on_submit in
      let textarea_cursor_ref = ref on_cursor in
      (match instance with
      | Textarea_instance textarea ->
          Textarea.set_on_input textarea
            (Some
               (fun s ->
                 match !textarea_input_ref with Some h -> h s | None -> ()));
          Textarea.set_on_change textarea
            (Some
               (fun s ->
                 match !textarea_change_ref with Some h -> h s | None -> ()));
          Textarea.set_on_submit textarea
            (Some
               (fun s ->
                 match !textarea_submit_ref with Some h -> h s | None -> ()));
          Textarea.set_on_cursor textarea
            (Some
               (fun ~cursor ~selection ->
                 match !textarea_cursor_ref with
                 | Some h -> h ~cursor ~selection
                 | None -> ()))
      | _ -> ());
      Textarea_callback_refs
        {
          textarea_input_ref;
          textarea_change_ref;
          textarea_submit_ref;
          textarea_cursor_ref;
        }
  | Vnode.Table_callbacks { on_change; on_activate } ->
      let table_change_ref = ref on_change in
      let table_activate_ref = ref on_activate in
      (match instance with
      | Table_instance table ->
          Table.set_on_change table
            (Some
               (fun i ->
                 match !table_change_ref with Some h -> h i | None -> ()));
          Table.set_on_activate table
            (Some
               (fun i ->
                 match !table_activate_ref with Some h -> h i | None -> ()))
      | _ -> ());
      Table_callback_refs { table_change_ref; table_activate_ref }
  | Vnode.Tree_callbacks { on_change; on_activate; on_expand } ->
      let tree_change_ref = ref on_change in
      let tree_activate_ref = ref on_activate in
      let tree_expand_ref = ref on_expand in
      (match instance with
      | Tree_instance tree ->
          Tree.set_on_change tree
            (Some
               (fun i ->
                 match !tree_change_ref with Some h -> h i | None -> ()));
          Tree.set_on_activate tree
            (Some
               (fun i ->
                 match !tree_activate_ref with Some h -> h i | None -> ()));
          Tree.set_on_expand tree
            (Some
               (fun i expanded ->
                 match !tree_expand_ref with
                 | Some h -> h i expanded
                 | None -> ()))
      | _ -> ());
      Tree_callback_refs { tree_change_ref; tree_activate_ref; tree_expand_ref }

let create_fiber ~(parent : Renderable.t) (elem : unit Vnode.element) : fiber =
  let instance = create_instance ~parent elem.kind elem.attrs in
  let node = node_of instance in
  let mouse_ref = ref elem.handlers.on_mouse in
  let key_ref = ref elem.handlers.on_key in
  let paste_ref = ref elem.handlers.on_paste in
  let callback_refs = create_callback_refs instance elem.callbacks in
  Renderable.on_mouse node (fun ev ->
      match !mouse_ref with Some h -> h ev | None -> ());
  Renderable.on_key node (fun ev ->
      match !key_ref with Some h -> h ev | None -> ());
  (* For Input/Textarea instances, chain paste handling: the widget processes
     the paste first, then the user's on_paste handler runs. *)
  (match instance with
  | Text_input_instance input ->
      Renderable.set_paste_handler node
        (Some
           (fun ev ->
             if not (Event.Paste.default_prevented ev) then
               Text_input.handle_paste input (Event.Paste.text ev);
             match !paste_ref with Some h -> h ev | None -> ()))
  | Textarea_instance textarea ->
      Renderable.set_paste_handler node
        (Some
           (fun ev ->
             if not (Event.Paste.default_prevented ev) then
               Textarea.handle_paste textarea (Event.Paste.text ev);
             match !paste_ref with Some h -> h ev | None -> ()))
  | _ ->
      Renderable.set_paste_handler node
        (Some (fun ev -> match !paste_ref with Some h -> h ev | None -> ())));
  {
    kind = elem.kind;
    key = elem.key;
    attrs = elem.attrs;
    instance;
    children = [];
    mouse_ref;
    key_ref;
    paste_ref;
    callback_refs;
  }

let update_callback_refs (callback_refs : callback_refs)
    (callbacks : unit Vnode.widget_callbacks) : unit =
  match (callback_refs, callbacks) with
  | Slider_callback_refs { value_change_ref }, Vnode.Slider_callbacks e ->
      value_change_ref := e.on_value_change
  | ( Input_callback_refs { input_ref; change_ref; submit_ref },
      Vnode.Input_callbacks e ) ->
      input_ref := e.on_input;
      change_ref := e.on_change;
      submit_ref := e.on_submit
  | ( Select_callback_refs { select_change_ref; select_activate_ref },
      Vnode.Select_callbacks e ) ->
      select_change_ref := e.on_change;
      select_activate_ref := e.on_activate
  | ( Tab_select_callback_refs { tab_select_change_ref; tab_select_activate_ref },
      Vnode.Tab_select_callbacks e ) ->
      tab_select_change_ref := e.on_change;
      tab_select_activate_ref := e.on_activate
  | Canvas_callback_refs { draw_ref }, Vnode.Canvas_callbacks e ->
      draw_ref := e.on_draw
  | ( Scroll_bar_callback_refs { scroll_bar_change_ref },
      Vnode.Scroll_bar_callbacks e ) ->
      scroll_bar_change_ref := e.on_change
  | Scroll_box_callback_refs { scroll_ref }, Vnode.Scroll_box_callbacks e ->
      scroll_ref := e.on_scroll
  | ( Textarea_callback_refs
        {
          textarea_input_ref;
          textarea_change_ref;
          textarea_submit_ref;
          textarea_cursor_ref;
        },
      Vnode.Textarea_callbacks e ) ->
      textarea_input_ref := e.on_input;
      textarea_change_ref := e.on_change;
      textarea_submit_ref := e.on_submit;
      textarea_cursor_ref := e.on_cursor
  | ( Table_callback_refs { table_change_ref; table_activate_ref },
      Vnode.Table_callbacks e ) ->
      table_change_ref := e.on_change;
      table_activate_ref := e.on_activate
  | ( Tree_callback_refs { tree_change_ref; tree_activate_ref; tree_expand_ref },
      Vnode.Tree_callbacks e ) ->
      tree_change_ref := e.on_change;
      tree_activate_ref := e.on_activate;
      tree_expand_ref := e.on_expand
  | No_callback_refs, Vnode.No_callbacks -> ()
  | _ -> ()

let update_fiber (f : fiber) (elem : unit Vnode.element) : bool =
  f.mouse_ref := elem.handlers.on_mouse;
  f.key_ref := elem.handlers.on_key;
  f.paste_ref := elem.handlers.on_paste;
  update_callback_refs f.callback_refs elem.callbacks;
  let changed =
    update_instance f.instance ~old_attrs:f.attrs ~new_attrs:elem.attrs
      ~old_kind:f.kind ~new_kind:elem.kind
  in
  f.attrs <- elem.attrs;
  f.kind <- elem.kind;
  if changed then commit_update f.instance;
  changed

let rec destroy_fiber (f : fiber) : unit =
  List.iter
    (function
      | Fiber child -> destroy_fiber child | Embedded n -> Renderable.detach n)
    f.children;
  Renderable.destroy_recursively (fiber_node f)

(* ───── Vnode Flattening ───── *)

(* Fragments and Empty nodes are structural -- they group vnodes but don't
   correspond to real widgets. Flattening collapses the tree into a flat list of
   concrete children (elements and embeds) for positional matching. *)
type flattened =
  | Flat_element of unit Vnode.element
  | Flat_embed of Renderable.t

let rec flatten_vnode (vnode : unit Vnode.t) (acc : flattened list) :
    flattened list =
  match vnode with
  | Vnode.Empty -> acc
  | Vnode.Embed node -> Flat_embed node :: acc
  | Vnode.Element elem -> Flat_element elem :: acc
  | Vnode.Fragment children ->
      List.fold_right (fun child acc -> flatten_vnode child acc) children acc

let flatten (vnode : unit Vnode.t) : flattened list = flatten_vnode vnode []

(* ───── Fiber Maps ───── *)

(* Keyed lookup + positional array for O(1) matching during reconciliation. *)
type fiber_maps = {
  by_key : (string, fiber * int) Hashtbl.t;
  by_index : fiber array;
  used : bool array;
}

let fibers_of_children (children : child list) : fiber list =
  List.filter_map (function Fiber f -> Some f | Embedded _ -> None) children

let build_maps (fibers : fiber list) : fiber_maps =
  let arr = Array.of_list fibers in
  let len = Array.length arr in
  let by_key = Hashtbl.create len in
  Array.iteri
    (fun i f ->
      match f.key with Some k -> Hashtbl.replace by_key k (f, i) | None -> ())
    arr;
  { by_key; by_index = arr; used = Array.make len false }

let can_reuse (f : fiber) (elem : unit Vnode.element) : bool =
  kind_matches f.kind elem.kind && f.key = elem.key

(* Keyed elements match by key, unkeyed elements match by position among their
   siblings. This mirrors React's reconciliation heuristic: keys give stable
   identity across reorders, positional matching is the fallback. *)
let find_match (maps : fiber_maps) (pos : int) (elem : unit Vnode.element) :
    fiber option =
  let old_len = Array.length maps.by_index in
  match elem.key with
  | Some k -> (
      match Hashtbl.find_opt maps.by_key k with
      | Some (f, idx) when can_reuse f elem && not maps.used.(idx) ->
          maps.used.(idx) <- true;
          Some f
      | _ -> None)
  | None ->
      if pos < old_len && not maps.used.(pos) then
        let f = maps.by_index.(pos) in
        if can_reuse f elem then (
          maps.used.(pos) <- true;
          Some f)
        else None
      else None

(* ───── Commit Placement ───── *)

let child_node = function Fiber f -> fiber_node f | Embedded n -> n

(* Two-pass placement: first detach stale managed children, then insert/reorder
   the new managed children. We intentionally avoid scanning all concrete
   Renderable children under [parent], because some widgets (e.g. Markdown)
   manage internal children outside the reconciler. *)
let commit_placement (parent_node : Renderable.t) ~(old_children : child list)
    ~(new_children : child list) : unit =
  let parent = Renderable.child_target parent_node in
  let target_set = Hashtbl.create (List.length new_children) in
  List.iter (fun c -> Hashtbl.replace target_set (child_node c) ()) new_children;
  (* Pass 1: detach stale reconciler-managed children *)
  List.iter
    (fun c ->
      let node = child_node c in
      if not (Hashtbl.mem target_set node) then Renderable.detach node)
    old_children;
  (* Pass 2: place each child at its target index. We re-snapshot children after
     each mutation because attach/detach change the list. *)
  List.iteri
    (fun i c ->
      let node = child_node c in
      let current = Renderable.children parent in
      let already_correct =
        match List.nth_opt current i with Some n -> n == node | None -> false
      in
      if not already_correct then (
        Renderable.detach node;
        Renderable.attach ~parent ~index:i node))
    new_children

(* ───── Reconciliation ───── *)

type t = {
  container : Renderable.t;
  mutable root_children : child list;
  mutable pending_autofocus : Renderable.t list;
}

let create ~container =
  { container; root_children = []; pending_autofocus = [] }

let container t = t.container

let apply_pending_autofocus t =
  match t.pending_autofocus with
  | [] -> ()
  | nodes ->
      List.iter (fun node -> ignore (Renderable.focus node : bool)) nodes;
      t.pending_autofocus <- []

let rec reconcile_element (t : t) ~(parent_node : Renderable.t)
    (maps : fiber_maps) (pos : int) (elem : unit Vnode.element) : fiber =
  match find_match maps pos elem with
  | Some f ->
      ignore (update_fiber f elem : bool);
      let old_children = f.children in
      let flattened = flatten_children elem.children in
      let new_children =
        reconcile_flattened t ~parent_node:(fiber_node f) old_children flattened
      in
      f.children <- new_children;
      f
  | None ->
      let f = create_fiber ~parent:parent_node elem in
      (match elem.attrs.ref with
      | Some ref_cb -> ref_cb (fiber_node f)
      | None -> ());
      if elem.attrs.autofocus && elem.attrs.focusable then
        t.pending_autofocus <- fiber_node f :: t.pending_autofocus;
      let flattened = flatten_children elem.children in
      let new_children =
        reconcile_flattened t ~parent_node:(fiber_node f) [] flattened
      in
      f.children <- new_children;
      f

and flatten_children (vnodes : unit Vnode.t list) : flattened list =
  List.fold_right (fun v acc -> flatten_vnode v acc) vnodes []

and reconcile_flattened (t : t) ~(parent_node : Renderable.t)
    (old_children : child list) (flattened : flattened list) : child list =
  let old_fibers = fibers_of_children old_children in
  let maps = build_maps old_fibers in
  let old_len = Array.length maps.by_index in
  (* elem_idx tracks position among elements only -- embedded nodes are not
     managed by fiber maps, so they don't consume a positional slot. *)
  let elem_idx = ref 0 in
  let new_children =
    List.map
      (fun fc ->
        match fc with
        | Flat_element elem ->
            let idx = !elem_idx in
            incr elem_idx;
            Fiber (reconcile_element t ~parent_node maps idx elem)
        | Flat_embed n -> Embedded n)
      flattened
  in
  (* Destroy unused old fibers *)
  for i = 0 to old_len - 1 do
    if not maps.used.(i) then destroy_fiber maps.by_index.(i)
  done;
  commit_placement parent_node ~old_children ~new_children;
  (* Apply deferred autofocus after placement, so the node is fully attached to
     the render tree when focus is applied. *)
  apply_pending_autofocus t;
  new_children

(* ───── Root Reconciliation ───── *)

let render (t : t) (vnode : unit Vnode.t) : unit =
  let flattened = flatten vnode in
  let new_children =
    reconcile_flattened t ~parent_node:t.container t.root_children flattened
  in
  t.root_children <- new_children;
  Renderable.request_render t.container

let unmount (t : t) : unit =
  List.iter
    (function Fiber f -> destroy_fiber f | Embedded n -> Renderable.detach n)
    t.root_children;
  t.root_children <- [];
  Renderable.request_render t.container
