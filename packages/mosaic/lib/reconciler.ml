open Mosaic_ui

(* ───── Instance Operations ───── *)

(* A picture's node, its picture, and the style its element declares. *)
type picture = {
  node : Renderable.t;
  mutable picture : Matrix_grid.picture;
  mutable style : Toffee.Style.t;
}

type instance =
  | Box_instance of Box.t
  | Text_instance of Text.t
  | Slider_instance of Slider.t
  | Text_input_instance of Text_input.t
  | Select_instance of Select.t
  | Tab_select_instance of Tab_select.t
  | Canvas_instance of Canvas.t
  | Picture_instance of picture
  | Spinner_instance of Spinner.t
  | Progress_bar_instance of Progress_bar.t
  | Scroll_bar_instance of Scroll_bar.t
  | Scroll_box_instance of Scroll_box.t
  | Textarea_instance of Textarea.t
  | Table_instance of Table.t
  | Code_instance of Code.t
  | Line_number_instance of Line_number.t
  | Markdown_instance of Markdown.t
  | Diff_instance of Diff.t
  | Tree_instance of Tree.t

let node_of = function
  | Box_instance b -> Box.node b
  | Text_instance t -> Text.node t
  | Slider_instance s -> Slider.node s
  | Text_input_instance i -> Text_input.node i
  | Select_instance s -> Select.node s
  | Tab_select_instance ts -> Tab_select.node ts
  | Canvas_instance c -> Canvas.node c
  | Picture_instance p -> p.node
  | Spinner_instance s -> Spinner.node s
  | Progress_bar_instance pb -> Progress_bar.node pb
  | Scroll_bar_instance sb -> Scroll_bar.node sb
  | Scroll_box_instance sb -> Scroll_box.node sb
  | Textarea_instance ta -> Textarea.node ta
  | Table_instance t -> Table.node t
  | Code_instance c -> Code.node c
  | Line_number_instance ln -> Line_number.node ln
  | Markdown_instance md -> Markdown.node md
  | Diff_instance d -> Diff.node d
  | Tree_instance tr -> Tree.node tr

(* A spinner's node is live while its element is declared live and its
   frame set animates. The widget sets the node live from its frame set alone
   whenever its props are applied, so this runs after it. *)
let set_spinner_live spinner (attrs : Vnode.attrs) =
  Renderable.set_live (Spinner.node spinner)
    (attrs.live && Spinner.animates spinner)

(* A picture's ratio of columns to rows, at the terminal's cell size. *)
let picture_ratio p =
  let pw, ph = Matrix_grid.picture_size p.picture in
  let cw, ch =
    Option.value (Renderable.Private.cell_size p.node) ~default:(10, 20)
  in
  float pw /. float cw /. (float ph /. float ch)

(* A picture keeps its ratio through layout, as a replaced element does: the
   style it declares, with that ratio when it sets none. *)
let picture_style p =
  match Toffee.Style.aspect_ratio p.style with
  | Some _ -> p.style
  | None -> Toffee.Style.set_aspect_ratio (Some (picture_ratio p)) p.style

let apply_picture_style p = Renderable.set_style p.node (picture_style p)

(* Where the layout leaves its size to the content: the picture's size in
   cells, cut to the space available, its ratio kept. *)
let picture_measure p ~known_dimensions ~available_space ~style:_ =
  let pw, _ = Matrix_grid.picture_size p.picture in
  let cw =
    Option.value (Renderable.Private.cell_size p.node) ~default:(10, 20) |> fst
  in
  let ratio = picture_ratio p in
  let open Toffee.Geometry.Size in
  let avail d = Toffee.Available_space.to_option d in
  match (known_dimensions.width, known_dimensions.height) with
  | Some width, Some height -> { width; height }
  | Some width, None -> { width; height = Float.ceil (width /. ratio) }
  | None, Some height -> { width = Float.ceil (height *. ratio); height }
  | None, None -> (
      let cols = Float.ceil (float pw /. float cw) in
      (* Its content can shrink to a cell, as an image's in a column. *)
      let width =
        match available_space.width with
        | Toffee.Available_space.Definite w when w > 0. -> Float.min cols w
        | Min_content -> 1.
        | _ -> cols
      in
      let height = Float.ceil (width /. ratio) in
      match avail available_space.height with
      | Some h when h > 0. && height > h ->
          { width = Float.min width (Float.ceil (h *. ratio)); height = h }
      | _ -> { width; height })

(* A cell size learnt or changed since the style was set changes the ratio,
   which the next layout takes. *)
let picture_render p self grid ~dt:_ =
  let style = picture_style p in
  if
    Toffee.Style.aspect_ratio style
    <> Toffee.Style.aspect_ratio (Renderable.style p.node)
  then (
    Renderable.set_style p.node style;
    Renderable.request_render p.node);
  Matrix_grid.draw_picture grid ~x:(Renderable.x self) ~y:(Renderable.y self)
    ~width:(Renderable.width self) ~height:(Renderable.height self) p.picture

let create_instance ~(parent : Renderable.t) (kind : Vnode.kind)
    (attrs : Vnode.attrs) : instance =
  let inst =
    match kind with
    | Vnode.Box spec ->
        let box =
          Box.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Box.apply_props box spec;
        Box_instance box
    | Vnode.Text spec ->
        let text =
          Text.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Text.apply_props text spec;
        Text_instance text
    | Vnode.Slider spec ->
        let slider =
          Slider.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Slider.apply_props slider spec;
        Slider_instance slider
    | Vnode.Text_input spec ->
        let input =
          Text_input.create ~state:Editor.empty ~on_change:(Fun.const ())
            ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Text_input.apply_props input spec;
        Text_input_instance input
    | Vnode.Canvas ->
        let canvas =
          Canvas.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Canvas_instance canvas
    | Vnode.Picture p ->
        let node =
          Renderable.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        let p = { node; picture = p; style = attrs.style } in
        apply_picture_style p;
        Renderable.set_measure node (Some (picture_measure p));
        Renderable.set_render node (picture_render p);
        Picture_instance p
    | Vnode.Select spec ->
        let select =
          Select.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Select.apply_props select spec;
        Select_instance select
    | Vnode.Tab_select spec ->
        let tab_select =
          Tab_select.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ~options:[] ()
        in
        Tab_select.apply_props tab_select spec;
        Tab_select_instance tab_select
    | Vnode.Spinner spec ->
        let spinner =
          Spinner.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Spinner.apply_props spinner spec;
        Spinner_instance spinner
    | Vnode.Progress_bar spec ->
        let progress_bar =
          Progress_bar.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Progress_bar.apply_props progress_bar spec;
        Progress_bar_instance progress_bar
    | Vnode.Scroll_bar spec ->
        let scroll_bar =
          Scroll_bar.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Scroll_bar.apply_props scroll_bar spec;
        Scroll_bar_instance scroll_bar
    | Vnode.Scroll_box spec ->
        let scroll_box =
          Scroll_box.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Scroll_box.apply_props scroll_box spec;
        Scroll_box_instance scroll_box
    | Vnode.Textarea spec ->
        let textarea =
          Textarea.create ~state:Editor.empty ~on_change:(Fun.const ()) ~parent
            ~style:attrs.style ~visible:attrs.visible ~z_index:attrs.z_index
            ~opacity:attrs.opacity ()
        in
        Textarea.apply_props textarea spec;
        Textarea_instance textarea
    | Vnode.Table spec ->
        let table =
          Table.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Table.apply_props table spec;
        Table_instance table
    | Vnode.Code spec ->
        let code =
          Code.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Code.apply_props code spec;
        Code_instance code
    | Vnode.Line_number spec ->
        let ln =
          Line_number.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Line_number.apply_props ln spec;
        Line_number_instance ln
    | Vnode.Markdown spec ->
        let md =
          Markdown.create ~parent ~layout_style:attrs.style
            ~visible:attrs.visible ~z_index:attrs.z_index ~opacity:attrs.opacity
            ()
        in
        Markdown.apply_props md spec;
        Markdown_instance md
    | Vnode.Diff spec ->
        let d =
          Diff.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity (Diff.Patch.make [])
        in
        Diff.apply_props d spec;
        Diff_instance d
    | Vnode.Tree spec ->
        let tree =
          Tree.create ~parent ~style:attrs.style ~visible:attrs.visible
            ~z_index:attrs.z_index ~opacity:attrs.opacity ()
        in
        Tree.apply_props tree spec;
        Tree_instance tree
  in
  let node = node_of inst in
  Renderable.set_focusable node (attrs.focusable || attrs.focused);
  if attrs.buffered then Renderable.set_buffered node true;
  (match inst with
  | Spinner_instance spinner -> set_spinner_live spinner attrs
  | _ -> if attrs.live then Renderable.set_live node true);
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
  if
    old_attrs.focusable <> new_attrs.focusable
    || old_attrs.focused <> new_attrs.focused
  then (
    Renderable.set_focusable node (new_attrs.focusable || new_attrs.focused);
    changed := true);
  if old_attrs.buffered <> new_attrs.buffered then (
    Renderable.set_buffered node new_attrs.buffered;
    changed := true);
  if old_attrs.live <> new_attrs.live then (
    Renderable.set_live node new_attrs.live;
    changed := true);
  (* Style is handled in update_instance for widget-specific behavior.
     Box.set_style adds border insets and Scroll_box.set_style its root
     overrides; Renderable.set_style does not. *)
  !changed

let set_instance_style (inst : instance) (style : Toffee.Style.t) : unit =
  match inst with
  | Box_instance box -> Box.set_style box style
  | Scroll_box_instance scroll_box -> Scroll_box.set_style scroll_box style
  | Markdown_instance md -> Markdown.set_layout_style md style
  | Picture_instance p ->
      p.style <- style;
      apply_picture_style p
  | _ -> Renderable.set_style (node_of inst) style

(* Accepted state only changes through declarations, so physical sharing is
   a valid fast path for every widget. *)
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
      match (inst, old_kind, new_kind) with
      | _ when old_kind == new_kind -> false
      | _ -> (
          match (inst, old_kind, new_kind) with
          | ( Text_input_instance input,
              Vnode.Text_input old_spec,
              Vnode.Text_input new_spec ) ->
              if Text_input.Props.equal old_spec new_spec then false
              else (
                Text_input.apply_props input new_spec;
                true)
          | ( Textarea_instance textarea,
              Vnode.Textarea old_spec,
              Vnode.Textarea new_spec ) ->
              if Textarea.Props.equal old_spec new_spec then false
              else (
                Textarea.apply_props textarea new_spec;
                true)
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
          | Canvas_instance _, Vnode.Canvas, Vnode.Canvas -> false
          | Picture_instance p, Vnode.Picture _, Vnode.Picture picture ->
              if p.picture == picture then false
              else (
                p.picture <- picture;
                apply_picture_style p;
                Renderable.mark_dirty p.node;
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
          | ( Markdown_instance md,
              Vnode.Markdown old_spec,
              Vnode.Markdown new_spec ) ->
              if Markdown.Props.equal old_spec new_spec then false
              else (
                Markdown.apply_props md new_spec;
                true)
          | Diff_instance d, Vnode.Diff old_spec, Vnode.Diff new_spec ->
              if Diff.Props.equal old_spec new_spec then false
              else (
                Diff.apply_props d new_spec;
                true)
          | Tree_instance tree, Vnode.Tree old_spec, Vnode.Tree new_spec ->
              if Tree.Props.equal old_spec new_spec then false
              else (
                Tree.apply_props tree new_spec;
                true)
          | _ ->
              invalid_arg "Reconciler: instance and kind mismatch during update"
          )
    in
    (match inst with
    | Spinner_instance spinner -> set_spinner_live spinner new_attrs
    | _ -> ());
    common_changed || style_changed || kind_changed

let commit_update (inst : instance) : unit =
  Renderable.request_render (node_of inst)

(* ───── Fiber Tracking ───── *)

type callback_refs = {
  callbacks : unit Vnode.widget_callbacks ref;
  update_presence : unit Vnode.widget_callbacks -> unit;
}

(* The focus a subtree declares. A shown modal in it makes every element
   outside the modal inert, so its focus is the modal's alone. *)
type declared_focus =
  | Focus_none
  | Focus_one of Renderable.t
  | Focus_many
  | Modal of modal

and modal = {
  modal_node : Renderable.t;
  modal_callbacks : unit Vnode.widget_callbacks ref;
  inside : declared_focus;  (** Never [Modal]. *)
}

(* A parent's children: elements, embedded nodes, and memo spans. A span owns
   the children its memo's view produced, so an unchanged memo keeps them
   without visiting them. *)
type child = Fiber of fiber | Embedded of Renderable.t | Memo_span of span

and fiber = {
  mutable kind : Vnode.kind;
  key : string option;
  mutable attrs : Vnode.attrs;
  instance : instance;
  mutable children : child list;
  mouse_ref : (Event.mouse -> unit option) option ref;
  key_ref : (Event.key -> unit option) option ref;
  paste_ref : (Event.paste -> unit option) option ref;
  click_ref : (Event.mouse -> unit option) option ref;
  size_ref : (w:int -> h:int -> unit option) option ref;
  mutable declared_children : unit Vnode.t list;
  mutable viewport_width : int;
  mutable declared_focus : declared_focus;
  callback_refs : callback_refs;
}

and span = {
  mutable memo : unit Vnode.memo;
  mutable span_theme : (Theme.t -> Theme.t) option;
      (** The theme its place declares, given to the nodes it places. *)
  mutable view : unit Vnode.t;  (** The memo's view, built once. *)
  mutable span_viewport_width : int;
  mutable span_children : child list;
  mutable node_count : int;
      (** Renderable nodes in [span_children], spans flattened. *)
  mutable first_node : Renderable.t option;
  mutable span_focus : declared_focus;
  mutable reused : bool;
      (** The current render kept the span's children unchanged. *)
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
  | Vnode.Canvas, Vnode.Canvas
  | Vnode.Picture _, Vnode.Picture _
  | Vnode.Spinner _, Vnode.Spinner _
  | Vnode.Progress_bar _, Vnode.Progress_bar _
  | Vnode.Scroll_bar _, Vnode.Scroll_bar _
  | Vnode.Scroll_box _, Vnode.Scroll_box _
  | Vnode.Textarea _, Vnode.Textarea _
  | Vnode.Table _, Vnode.Table _
  | Vnode.Code _, Vnode.Code _
  | Vnode.Line_number _, Vnode.Line_number _
  | Vnode.Markdown _, Vnode.Markdown _
  | Vnode.Diff _, Vnode.Diff _
  | Vnode.Tree _, Vnode.Tree _ ->
      true
  | _ -> false

(* Most widgets use stable callbacks dispatching through a mutable ref.
   Table instead installs callbacks directly so a press can retain its original
   row-to-value mapping across synchronous reconciliation. *)
let create_callback_refs (instance : instance)
    (initial : unit Vnode.widget_callbacks) : callback_refs =
  let callbacks = ref initial in
  let update_presence =
    match instance with
    | Slider_instance widget ->
        Slider.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Slider_callbacks c ->
                   Option.iter (fun f -> f value) c.on_value_change
               | _ -> ()));
        Fun.const ()
    | Text_input_instance widget ->
        Text_input.set_on_change widget (fun value ->
            match !callbacks with
            | Vnode.Input_callbacks c ->
                Option.iter (fun f -> f value) c.on_change
            | _ -> ());
        Text_input.set_on_submit widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Input_callbacks c ->
                   Option.iter (fun f -> f value) c.on_submit
               | _ -> ()));
        Fun.const ()
    | Textarea_instance widget ->
        Textarea.set_on_change widget (fun value ->
            match !callbacks with
            | Vnode.Textarea_callbacks c ->
                Option.iter (fun f -> f value) c.on_change
            | _ -> ());
        Textarea.set_on_submit widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Textarea_callbacks c ->
                   Option.iter (fun f -> f value) c.on_submit
               | _ -> ()));
        Fun.const ()
    | Select_instance widget ->
        Select.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Select.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Tab_select_instance widget ->
        Tab_select.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tab_select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Tab_select.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tab_select_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Scroll_bar_instance widget ->
        Scroll_bar.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Scroll_bar_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Fun.const ()
    | Scroll_box_instance widget ->
        Scroll_box.set_on_scroll widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Scroll_box_callbacks c ->
                   Option.iter (fun f -> f value) c.on_scroll
               | _ -> ()));
        Fun.const ()
    | Code_instance widget ->
        Code.set_on_selection widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Code_callbacks c ->
                   Option.iter
                     (fun f -> ignore (f value : unit option))
                     c.on_selection
               | _ -> ()));
        Fun.const ()
    | Markdown_instance widget ->
        Markdown.set_on_selection widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Markdown_callbacks c ->
                   Option.iter
                     (fun f -> ignore (f value : unit option))
                     c.on_selection
               | _ -> ()));
        Fun.const ()
    | Table_instance widget -> (
        (* A press snapshots these callbacks before user code can reconcile a
           different domain-to-row mapping. *)
        function
        | Vnode.Table_callbacks c ->
            Table.set_on_change widget c.on_change;
            Table.set_on_activate widget c.on_activate;
            Table.set_on_hover widget
              (Option.map
                 (fun f value -> ignore (f value : unit option))
                 c.on_hover)
        | _ ->
            Table.set_on_change widget None;
            Table.set_on_activate widget None;
            Table.set_on_hover widget None)
    | Tree_instance widget ->
        Tree.set_on_change widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tree_callbacks c ->
                   Option.iter (fun f -> f value) c.on_change
               | _ -> ()));
        Tree.set_on_activate widget
          (Some
             (fun value ->
               match !callbacks with
               | Vnode.Tree_callbacks c ->
                   Option.iter (fun f -> f value) c.on_activate
               | _ -> ()));
        Fun.const ()
    | Canvas_instance widget ->
        Canvas.set_on_draw widget
          (Some
             (fun canvas ~dt ->
               match !callbacks with
               | Vnode.Canvas_callbacks c ->
                   Option.iter (fun f -> f canvas ~dt) c.on_draw
               | _ -> ()));
        Fun.const ()
    | Diff_instance widget -> (
        let dispatch hit =
          match !callbacks with
          | Vnode.Diff_callbacks c ->
              Option.iter
                (fun f -> ignore (f hit : unit option))
                c.on_line_click
          | _ -> ()
        in
        fun current ->
          match current with
          | Vnode.Diff_callbacks c ->
              Diff.set_on_line_click widget
                (Option.map (Fun.const dispatch) c.on_line_click)
          | _ -> Diff.set_on_line_click widget None)
    | Text_instance widget -> (
        function
        | Vnode.Text_callbacks c ->
            Text.set_on_selection widget
              (Option.map
                 (fun f value -> ignore (f value : unit option))
                 c.on_selection)
        | _ -> Text.set_on_selection widget None)
    | Box_instance _ | Spinner_instance _ | Progress_bar_instance _
    | Line_number_instance _ | Picture_instance _ ->
        Fun.const ()
  in
  update_presence initial;
  { callbacks; update_presence }

let create_fiber ~(parent : Renderable.t) (elem : unit Vnode.element) : fiber =
  let instance = create_instance ~parent elem.kind elem.attrs in
  let node = node_of instance in
  Renderable.Private.set_focus_request node elem.handlers.on_focus_request;
  let mouse_ref = ref elem.handlers.on_mouse in
  let key_ref = ref elem.handlers.on_key in
  let paste_ref = ref elem.handlers.on_paste in
  let click_ref = ref elem.handlers.on_click in
  let size_ref = ref elem.handlers.on_size in
  let callback_refs = create_callback_refs instance elem.callbacks in
  Renderable.on_mouse node (fun ev ->
      Option.iter (fun f -> ignore (f ev : unit option)) !mouse_ref);
  Renderable.Private.set_click_handler node
    (Some
       (fun ev -> Option.iter (fun f -> ignore (f ev : unit option)) !click_ref));
  Renderable.Private.set_size_observer node
    (Some
       (fun node ->
         Option.iter
           (fun f ->
             ignore
               (f ~w:(Renderable.width node) ~h:(Renderable.height node)
                 : unit option))
           !size_ref));
  Renderable.on_key node (fun ev ->
      match !key_ref with Some h -> ignore (h ev : unit option) | None -> ());
  Renderable.on_paste node (fun ev ->
      match !paste_ref with Some h -> ignore (h ev : unit option) | None -> ());
  {
    kind = elem.kind;
    key = elem.key;
    attrs = elem.attrs;
    instance;
    children = [];
    mouse_ref;
    key_ref;
    paste_ref;
    click_ref;
    size_ref;
    declared_children = [];
    viewport_width = 0;
    declared_focus = Focus_none;
    callback_refs;
  }

let update_callback_refs refs callbacks =
  refs.callbacks := callbacks;
  refs.update_presence callbacks

let update_fiber (f : fiber) (elem : unit Vnode.element) : bool =
  Renderable.Private.set_focus_request (fiber_node f)
    elem.handlers.on_focus_request;
  f.mouse_ref := elem.handlers.on_mouse;
  f.key_ref := elem.handlers.on_key;
  f.paste_ref := elem.handlers.on_paste;
  f.click_ref := elem.handlers.on_click;
  f.size_ref := elem.handlers.on_size;
  update_callback_refs f.callback_refs elem.callbacks;
  let changed =
    update_instance f.instance ~old_attrs:f.attrs ~new_attrs:elem.attrs
      ~old_kind:f.kind ~new_kind:elem.kind
  in
  f.attrs <- elem.attrs;
  f.kind <- elem.kind;
  if changed then commit_update f.instance;
  changed

(* A fiber's descendants have their nodes under its node, except embedded
   nodes, which the application owns and which outlive the fiber. Fibers are
   destroyed together: their nodes and the embedded nodes under them are
   detached in one pass, then each fiber's node is destroyed with the nodes
   under it. Detaching or destroying them one by one would detach each node
   from its parent alone, shifting the siblings after it, and the teardown
   would be quadratic in their number. *)
let rec embeds_under acc = function
  | Fiber f -> List.fold_left embeds_under acc f.children
  | Embedded n -> n :: acc
  | Memo_span span -> List.fold_left embeds_under acc span.span_children

(* Destroys [fibers], in order, and detaches [embeds] with them. *)
let destroy_fibers ?(embeds = []) fibers =
  let embeds =
    List.fold_left
      (fun acc f -> List.fold_left embeds_under acc f.children)
      (List.rev embeds) fibers
  in
  Renderable.Private.detach_all
    (List.rev_append embeds (List.map fiber_node fibers));
  List.iter (fun f -> Renderable.destroy_recursively (fiber_node f)) fibers

(* The fibers among [children], spans flattened, and the embedded nodes, both
   in order. *)
let fibers_and_embeds children =
  let rec go (fibers, embeds) = function
    | Fiber f -> (f :: fibers, embeds)
    | Embedded n -> (fibers, n :: embeds)
    | Memo_span span -> List.fold_left go (fibers, embeds) span.span_children
  in
  let fibers, embeds = List.fold_left go ([], []) children in
  (List.rev fibers, List.rev embeds)

(* ───── Vnode Flattening ───── *)

(* Fragments, themed vnodes and Empty nodes are structural -- they group
   vnodes but don't correspond to real widgets. Flattening collapses them
   into the list of concrete children: elements, embeds, and memos, whose
   views are built only when they cannot be reused. A child inside [themed]
   wrappers carries their functions composed, outermost first, which the
   reconciler gives its node as its place's
   ([Renderable.Private.set_place_theme]), apart from its own; a child outside every
   one is not wrapped, so a view without [themed] allocates nothing for
   them. *)
type flattened =
  | Flat_element of unit Vnode.element
  | Flat_embed of Renderable.t
  | Flat_memo of unit Vnode.memo
  | Flat_themed of (Theme.t -> Theme.t) * flattened
      (** Never holds another [Flat_themed]. *)

let in_theme theme flat =
  match theme with None -> flat | Some f -> Flat_themed (f, flat)

(* [inner] applied to what [outer] gives. *)
let compose outer inner =
  match outer with None -> inner | Some outer -> fun t -> inner (outer t)

let rec flatten_vnode ~viewport_width ~theme (vnode : unit Vnode.t)
    (acc : flattened list) : flattened list =
  match vnode with
  | Vnode.Empty -> acc
  | Vnode.Memo memo -> in_theme theme (Flat_memo memo) :: acc
  | Vnode.Embed node -> in_theme theme (Flat_embed node) :: acc
  | Vnode.Element elem -> in_theme theme (Flat_element elem) :: acc
  | Vnode.Themed (f, child) ->
      flatten_vnode ~viewport_width ~theme:(Some (compose theme f)) child acc
  | Vnode.Fragment children -> flatten_list ~viewport_width ~theme children acc
  | Vnode.Viewport_switch { at_least_width; wide; narrow } ->
      let selected =
        if viewport_width >= at_least_width then wide else narrow
      in
      flatten_vnode ~viewport_width ~theme selected acc

(* [List.fold_right] over [flatten_vnode], without the closure. *)
and flatten_list ~viewport_width ~theme children acc =
  match children with
  | [] -> acc
  | child :: rest ->
      flatten_vnode ~viewport_width ~theme child
        (flatten_list ~viewport_width ~theme rest acc)

let flatten_children ~viewport_width children =
  flatten_list ~viewport_width ~theme:None children []

let same_theme a b =
  match (a, b) with
  | None, None -> true
  | Some f, Some g -> f == g
  | Some _, None | None, Some _ -> false

(* ───── Fiber Maps ───── *)

(* Keyed lookup + positional array for O(1) matching during reconciliation. *)
type fiber_maps = {
  by_key : (string, fiber * int) Hashtbl.t;
  by_index : fiber array;
  used : bool array;
}

let build_maps (children : child list) : fiber_maps =
  let arr =
    Array.of_list
      (List.filter_map
         (function Fiber f -> Some f | Embedded _ | Memo_span _ -> None)
         children)
  in
  let len = Array.length arr in
  let by_key = Hashtbl.create len in
  Array.iteri
    (fun i f ->
      match f.key with
      | Some k ->
          (* First occurrence wins on duplicate keys, so when a view
             accidentally emits duplicate-keyed siblings the first keeps its
             identity across renders and only the later duplicates remount. *)
          if not (Hashtbl.mem by_key k) then Hashtbl.add by_key k (f, i)
      | None -> ())
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

(* Placement of a parent's children happens after all of them are reconciled.
   Unmatched fibers were destroyed, which detaches them; an embedded node that
   left its list is detached unless it reappears elsewhere among the parent's
   children. Then each child is placed at its target index.

   We intentionally avoid scanning all concrete Renderable children under
   [parent], because some widgets (e.g. Markdown) manage internal children
   outside the reconciler. Position probes go through the O(1) [child_at]
   accessor, and node sets are keyed by [Renderable.Private.num] -- hashing raw
   nodes would apply polymorphic hash and compare to cyclic records of
   closures. A reused span whose first node is in place is skipped whole: its
   nodes were placed contiguously, and placement before it only shifts them. *)
let rec iter_embeds f = function
  | Fiber _ -> ()
  | Embedded n -> f n
  | Memo_span span -> List.iter (iter_embeds f) span.span_children

let detach_stale_embeds ~stale (new_children : child list) =
  if stale <> [] then begin
    let kept = Hashtbl.create 8 in
    List.iter
      (iter_embeds (fun n -> Hashtbl.replace kept (Renderable.Private.num n) ()))
      new_children;
    Renderable.Private.detach_all
      (List.filter
         (fun n -> not (Hashtbl.mem kept (Renderable.Private.num n)))
         stale)
  end

let place_node parent index node =
  match Renderable.Private.child_at parent index with
  | Some n when n == node -> ()
  | Some _ | None -> Renderable.attach ~parent ~index node

let rec place parent index = function
  | [] -> index
  | Fiber f :: rest ->
      place_node parent index (fiber_node f);
      place parent (index + 1) rest
  | Embedded n :: rest ->
      place_node parent index n;
      place parent (index + 1) rest
  | Memo_span span :: rest ->
      let in_place =
        span.reused
        &&
        match (span.first_node, Renderable.Private.child_at parent index) with
        | Some first, Some n -> n == first
        | None, _ -> true
        | Some _, None -> false
      in
      let index =
        if in_place then index + span.node_count
        else place parent index span.span_children
      in
      place parent index rest

let commit_placement parent_node ~stale new_children =
  detach_stale_embeds ~stale new_children;
  ignore (place (Renderable.child_target parent_node) 0 new_children : int)

(* ───── Reconciliation ───── *)

type t = { container : Renderable.t; mutable root_children : child list }

let create ~container = { container; root_children = [] }
let container t = t.container

(* Of two modals the later in tree order is the one, as the dialog opened
   last is on top. *)
let combine_focus left right =
  match (left, right) with
  | _, (Modal _ as modal) | (Modal _ as modal), _ -> modal
  | Focus_none, focus | focus, Focus_none -> focus
  | _ -> Focus_many

(* Only a box holds children, so only a box is inert. *)
let inert_kind = function
  | Vnode.Box props -> Box.Props.inert props
  | _ -> false

let is_modal fiber =
  match !(fiber.callback_refs.callbacks) with
  | Vnode.Modal_callbacks _ -> true
  | _ -> false

let children_focus children =
  List.fold_left
    (fun focus -> function
      | Embedded _ -> focus
      | Fiber fiber -> combine_focus focus fiber.declared_focus
      | Memo_span span -> combine_focus focus span.span_focus)
    Focus_none children

let update_declared_focus fiber =
  fiber.declared_focus <-
    (if (not fiber.attrs.visible) || inert_kind fiber.kind then Focus_none
     else
       let focus =
         combine_focus
           (if fiber.attrs.focused then Focus_one (fiber_node fiber)
            else Focus_none)
           (children_focus fiber.children)
       in
       match focus with
       | Modal _ -> focus
       | (Focus_none | Focus_one _ | Focus_many) as inside ->
           if is_modal fiber then
             Modal
               {
                 modal_node = fiber_node fiber;
                 modal_callbacks = fiber.callback_refs.callbacks;
                 inside;
               }
           else focus)

let rec node_count children =
  List.fold_left
    (fun count -> function
      | Fiber _ | Embedded _ -> count + 1
      | Memo_span span -> count + span.node_count)
    0 children

and first_node = function
  | [] -> None
  | Fiber f :: _ -> Some (fiber_node f)
  | Embedded n :: _ -> Some n
  | Memo_span span :: rest -> (
      match span.first_node with
      | Some _ as node -> node
      | None -> first_node rest)

let rec reconcile_element ~(parent_node : Renderable.t) (maps : fiber_maps)
    (pos : int) ~viewport_width (elem : unit Vnode.element) : fiber =
  match find_match maps pos elem with
  | Some f ->
      let was_modal = is_modal f in
      let focus_changed =
        f.attrs.focused <> elem.attrs.focused
        || f.attrs.visible <> elem.attrs.visible
        || inert_kind f.kind <> inert_kind elem.kind
      in
      let children_changed =
        f.declared_children != elem.children
        || f.viewport_width <> viewport_width
      in
      ignore (update_fiber f elem : bool);
      if children_changed then begin
        reconcile_children f ~viewport_width elem.children;
        f.declared_children <- elem.children;
        f.viewport_width <- viewport_width
      end;
      if focus_changed || children_changed || was_modal <> is_modal f then
        update_declared_focus f;
      f
  | None ->
      let f = create_fiber ~parent:parent_node elem in
      reconcile_children f ~viewport_width elem.children;
      f.declared_children <- elem.children;
      f.viewport_width <- viewport_width;
      update_declared_focus f;
      f

and reconcile_children f ~viewport_width children =
  let stale = ref [] in
  let flattened = flatten_children ~viewport_width children in
  let children =
    reconcile_list ~parent_node:(fiber_node f) ~viewport_width ~stale f.children
      flattened
  in
  f.children <- children;
  commit_placement (fiber_node f) ~stale:!stale children

(* Reconciles one list of children: a parent's, or a memo span's. Elements
   match the list's old fibers, memos its old spans by position among memos.
   Embedded nodes that leave the list are collected in [stale]. *)
and reconcile_list ~(parent_node : Renderable.t) ~viewport_width ~stale
    (old_children : child list) (flattened : flattened list) : child list =
  let maps = build_maps old_children in
  let old_spans =
    ref
      (List.filter_map
         (function Memo_span span -> Some span | Fiber _ | Embedded _ -> None)
         old_children)
  in
  List.iter
    (function
      | Embedded n -> stale := n :: !stale | Fiber _ | Memo_span _ -> ())
    old_children;
  (* elem_idx tracks position among elements only -- embedded nodes and memos
     are not managed by fiber maps, so they don't consume a positional slot. *)
  let elem_idx = ref 0 in
  let new_children =
    List.map
      (fun flat ->
        reconcile_item ~parent_node maps elem_idx old_spans ~viewport_width
          ~stale None flat)
      flattened
  in
  (* A discarded span's embedded nodes may reappear elsewhere, so they are
     left to placement. *)
  let unused = ref [] in
  for i = Array.length maps.by_index - 1 downto 0 do
    if not maps.used.(i) then unused := maps.by_index.(i) :: !unused
  done;
  (match (!unused, !old_spans) with
  | [], [] -> ()
  | unused, spans ->
      let span_fibers, span_embeds =
        fibers_and_embeds (List.map (fun span -> Memo_span span) spans)
      in
      stale := List.rev_append span_embeds !stale;
      destroy_fibers (unused @ span_fibers));
  new_children

(* One child of [reconcile_list], under the theme its place declares. *)
and reconcile_item ~parent_node maps elem_idx old_spans ~viewport_width ~stale
    theme = function
  | Flat_element elem ->
      let idx = !elem_idx in
      incr elem_idx;
      let fiber =
        reconcile_element ~parent_node maps idx ~viewport_width elem
      in
      Renderable.Private.set_place_theme (fiber_node fiber) theme;
      Fiber fiber
  | Flat_embed n ->
      Renderable.Private.set_place_theme n theme;
      Embedded n
  | Flat_memo memo ->
      let previous =
        match !old_spans with
        | span :: rest ->
            old_spans := rest;
            Some span
        | [] -> None
      in
      Memo_span
        (reconcile_span ~parent_node ~viewport_width ~stale ~theme previous memo)
  | Flat_themed (f, flat) ->
      reconcile_item ~parent_node maps elem_idx old_spans ~viewport_width ~stale
        (Some f) flat

(* Like Elm's lazy nodes, an equal memo stops the diff: its span keeps its
   children untouched. Under another theme its view, kept, is reconciled
   again, so that its nodes take the theme. *)
and reconcile_span ~parent_node ~viewport_width ~stale ~theme previous memo =
  match previous with
  | Some span
    when Vnode.memo_equal span.memo memo
         && span.span_viewport_width = viewport_width
         && same_theme span.span_theme theme ->
      span.memo <- memo;
      span.reused <- true;
      span
  | Some span ->
      if not (Vnode.memo_equal span.memo memo) then
        span.view <- Vnode.memo_view memo;
      span.memo <- memo;
      span.span_viewport_width <- viewport_width;
      span.span_theme <- theme;
      update_span ~parent_node ~viewport_width ~stale span;
      span
  | None ->
      let span =
        {
          memo;
          span_theme = theme;
          view = Vnode.memo_view memo;
          span_viewport_width = viewport_width;
          span_children = [];
          node_count = 0;
          first_node = None;
          span_focus = Focus_none;
          reused = false;
        }
      in
      update_span ~parent_node ~viewport_width ~stale span;
      span

and update_span ~parent_node ~viewport_width ~stale span =
  let children =
    reconcile_list ~parent_node ~viewport_width ~stale span.span_children
      (flatten_vnode ~viewport_width ~theme:span.span_theme span.view [])
  in
  span.span_children <- children;
  span.node_count <- node_count children;
  span.first_node <- first_node children;
  span.span_focus <- children_focus children;
  span.reused <- false

(* ───── Root Reconciliation ───── *)

let render (t : t) ~viewport_width (vnode : unit Vnode.t) : unit =
  if viewport_width < 1 then
    invalid_arg "Reconciler.render: viewport_width must be positive";
  let stale = ref [] in
  let children =
    reconcile_list ~parent_node:t.container ~viewport_width ~stale
      t.root_children
      (flatten_vnode ~viewport_width ~theme:None vnode [])
  in
  t.root_children <- children;
  commit_placement t.container ~stale:!stale children;
  Renderable.request_render t.container

let unmount (t : t) : unit =
  let fibers, embeds = fibers_and_embeds t.root_children in
  destroy_fibers ~embeds fibers;
  t.root_children <- [];
  Renderable.request_render t.container

let focused t =
  let focus =
    match children_focus t.root_children with
    | Modal { inside; _ } -> inside
    | focus -> focus
  in
  match focus with
  | Focus_none | Modal _ -> None
  | Focus_one node -> Some node
  | Focus_many -> invalid_arg "Mosaic: more than one focused element"

let modal t =
  match children_focus t.root_children with
  | Modal { modal_node; _ } -> Some modal_node
  | Focus_none | Focus_one _ | Focus_many -> None

let request_close t =
  match children_focus t.root_children with
  | Modal { modal_callbacks; _ } -> (
      match !modal_callbacks with
      | Vnode.Modal_callbacks { on_close = Some close } ->
          close ();
          true
      | _ -> false)
  | Focus_none | Focus_one _ | Focus_many -> false
