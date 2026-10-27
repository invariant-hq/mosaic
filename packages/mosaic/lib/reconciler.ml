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

(* A picture is a replaced element, as an image is in CSS: its parent's layout
   decides whether it stretches (a flex container's does, a block's and a
   grid's [normal] alignment do not), and its measure function gives its size
   where nothing does. It keeps its ratio through layout: the style it
   declares, with that ratio when it sets none. *)
let picture_style p =
  let style = Toffee.Style.set_item_is_replaced true p.style in
  match Toffee.Style.aspect_ratio style with
  | Some _ -> style
  | None -> Toffee.Style.set_aspect_ratio (Some (picture_ratio p)) style

let apply_picture_style p = Renderable.set_style p.node (picture_style p)

(* Where the layout leaves its size to the content: the picture's size in
   cells, cut to the space available, its ratio kept, as Flutter's RenderImage
   fits an image's size into its constraints. A replaced element is given the
   same known size when it is measured and when it is laid out, so the two
   agree. *)
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

(* What [update_instance] found changed: the attributes, the kind, both or
   neither. *)
let attrs_changed = 1
let kind_changed_bit = 2

(* Accepted state only changes through declarations, so physical sharing is
   a valid fast path for every widget. The result says which of the
   element's records differ from the fiber's: a record that does not differ
   equals the fiber's in every field, so the fiber keeps its own. *)
let update_instance (inst : instance) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) ~(old_kind : Vnode.kind) ~(new_kind : Vnode.kind)
    : int =
  if old_attrs == new_attrs && old_kind == new_kind then 0
  else
    let common_changed =
      if old_attrs == new_attrs then false
      else update_common (node_of inst) ~old_attrs ~new_attrs
    in
    (* A layout shared by many elements is the same record on every render,
       and comparing it physically first costs nothing per field. *)
    let style_changed =
      if old_attrs == new_attrs || old_attrs.style == new_attrs.style then false
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
    (if common_changed || style_changed then attrs_changed else 0)
    lor if kind_changed then kind_changed_bit else 0

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
  handlers : unit Vnode.handlers ref;
      (** The handlers the element declared last, which the node's handlers read
          when an event comes. *)
  mutable declared_children : unit Vnode.t list;
  mutable viewport_width : int;
  mutable declared_focus : declared_focus;
  callback_refs : callback_refs;
  mutable pass : int;  (** The last pass that matched it ({!next_pass}). *)
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
  mutable span_pass : int;  (** The last pass that matched it. *)
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
  let handlers = ref elem.handlers in
  let callback_refs = create_callback_refs instance elem.callbacks in
  Renderable.on_mouse node (fun ev ->
      Option.iter (fun f -> ignore (f ev : unit option)) !handlers.on_mouse);
  Renderable.Private.set_click_handler node
    (Some
       (fun ev ->
         Option.iter (fun f -> ignore (f ev : unit option)) !handlers.on_click));
  Renderable.Private.set_size_observer node
    (Some
       (fun node ->
         Option.iter
           (fun f ->
             ignore
               (f ~w:(Renderable.width node) ~h:(Renderable.height node)
                 : unit option))
           !handlers.on_size));
  Renderable.on_key node (fun ev ->
      match !handlers.on_key with
      | Some h -> ignore (h ev : unit option)
      | None -> ());
  Renderable.on_paste node (fun ev ->
      match !handlers.on_paste with
      | Some h -> ignore (h ev : unit option)
      | None -> ());
  {
    kind = elem.kind;
    key = elem.key;
    attrs = elem.attrs;
    instance;
    children = [];
    handlers;
    declared_children = [];
    viewport_width = 0;
    declared_focus = Focus_none;
    callback_refs;
    pass = 0;
  }

let update_callback_refs refs callbacks =
  if !(refs.callbacks) != callbacks then begin
    refs.callbacks := callbacks;
    refs.update_presence callbacks
  end

(* A fiber keeps its records while the element's equal them, so the
   element's fresh records die young and an unchanged element writes
   nothing into the fiber. *)
let update_fiber (f : fiber) (elem : unit Vnode.element) : bool =
  if !(f.handlers) != elem.handlers then begin
    Renderable.Private.set_focus_request (fiber_node f)
      elem.handlers.on_focus_request;
    f.handlers := elem.handlers
  end;
  update_callback_refs f.callback_refs elem.callbacks;
  let changed =
    update_instance f.instance ~old_attrs:f.attrs ~new_attrs:elem.attrs
      ~old_kind:f.kind ~new_kind:elem.kind
  in
  if changed land attrs_changed <> 0 then f.attrs <- elem.attrs;
  if changed land kind_changed_bit <> 0 then f.kind <- elem.kind;
  if changed <> 0 then commit_update f.instance;
  changed <> 0

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

(* [inner] applied to what [outer] gives. *)
let compose outer inner =
  match outer with None -> inner | Some outer -> fun t -> inner (outer t)

let same_theme a b =
  match (a, b) with
  | None, None -> true
  | Some f, Some g -> f == g
  | Some _, None | None, Some _ -> false

(* ───── Matching ───── *)

(* Each reconciliation of one list of children is a pass with a number of its
   own. The fibers and spans a pass matches carry its number, so the old
   children nothing matched are found by one walk of the old list, with no
   table and no array.

   Keyed elements match by key, unkeyed elements by position among the
   list's elements. This mirrors React's reconciliation heuristic: keys give
   stable identity across reorders, positional matching is the fallback.
   Memos match by position among the list's memos. A match is the cell of
   the old list that holds the old child, so a kept child is the old list's
   own value and the matcher allocates nothing for it. The key table is
   built the first time an element with a key asks for it. *)
let current_pass = ref 0

type matcher = {
  pass : int;
  old_children : child list;
  mutable next_element : child list;
      (** The old children from the next element position on. *)
  mutable next_memo : child list;
      (** The old children from the next memo position on. *)
  mutable keys : (string, child list) Hashtbl.t option;
}

let matcher old_children =
  incr current_pass;
  {
    pass = !current_pass;
    old_children;
    next_element = old_children;
    next_memo = old_children;
    keys = None;
  }

(* The cell of the old fiber at the next element position, or [], moving
   the position past it. *)
let rec take_element m =
  match m.next_element with
  | [] -> []
  | Fiber _ :: rest as cell ->
      m.next_element <- rest;
      cell
  | (Embedded _ | Memo_span _) :: rest ->
      m.next_element <- rest;
      take_element m

let rec take_memo m =
  match m.next_memo with
  | [] -> []
  | Memo_span _ :: rest as cell ->
      m.next_memo <- rest;
      cell
  | (Embedded _ | Fiber _) :: rest ->
      m.next_memo <- rest;
      take_memo m

(* First occurrence wins on duplicate keys, so when a view accidentally
   emits duplicate-keyed siblings the first keeps its identity across
   renders and only the later duplicates remount. *)
let keys m =
  match m.keys with
  | Some keys -> keys
  | None ->
      let keys = Hashtbl.create 16 in
      let rec add cells =
        match cells with
        | [] -> ()
        | Fiber { key = Some k; _ } :: rest ->
            if not (Hashtbl.mem keys k) then Hashtbl.add keys k cells;
            add rest
        | _ :: rest -> add rest
      in
      add m.old_children;
      m.keys <- Some keys;
      keys

(* The cell of the old fiber [elem] matches, or []. The fiber takes the
   pass's number. *)
let find_match m (elem : unit Vnode.element) : child list =
  let positional = take_element m in
  let cell =
    match elem.key with
    | None -> (
        match positional with
        | Fiber { key = None; _ } :: _ -> positional
        | _ -> [])
    | Some k -> (
        match Hashtbl.find_opt (keys m) k with Some cell -> cell | None -> [])
  in
  match cell with
  | Fiber f :: _ when f.pass <> m.pass && kind_matches f.kind elem.kind ->
      f.pass <- m.pass;
      cell
  | _ -> []

(* ───── Building the new list ───── *)

(* The new children of a list, built as they are reconciled. While each
   child is the old list's child at the same index, physically, nothing is
   allocated, and a list whose children all stayed in place is the old list
   itself. *)
type builder = {
  old : child list;
  mutable rest : child list;  (** The old children after those kept. *)
  mutable kept : int;
  mutable diverged : bool;
  mutable acc : child list;  (** Reversed, once [diverged]. *)
}

let builder old = { old; rest = old; kept = 0; diverged = false; acc = [] }

let rec rev_prefix list n acc =
  if n = 0 then acc
  else
    match list with
    | [] -> acc
    | child :: rest -> rev_prefix rest (n - 1) (child :: acc)

let emit b child =
  if b.diverged then b.acc <- child :: b.acc
  else
    match b.rest with
    | c :: rest when c == child ->
        b.rest <- rest;
        b.kept <- b.kept + 1
    | _ ->
        b.diverged <- true;
        b.acc <- child :: rev_prefix b.old b.kept []

let finish b =
  if b.diverged then List.rev b.acc
  else
    match b.rest with [] -> b.old | _ -> List.rev (rev_prefix b.old b.kept [])

(* ───── Commit Placement ───── *)

(* Placement of a parent's children happens after all of them are reconciled.
   Unmatched fibers were destroyed, which detaches them; an embedded node that
   left its list is detached unless it reappears elsewhere among the parent's
   children. Then the children are placed in order from index 0.

   We intentionally avoid scanning all concrete Renderable children under
   [parent], because some widgets (e.g. Markdown) manage internal children
   outside the reconciler. A first pass compares each child with the node at
   its index through the O(1) [child_at] probe, and a reused span whose first
   node is in place is skipped whole: its nodes were placed contiguously, and
   placement before it only shifts them. From the first child out of place,
   the children are placed in one pass ([Renderable.Private.place_children]), so
   any order costs time linear in the number of children. Node sets are keyed
   by [Renderable.Private.num] -- hashing raw nodes would apply polymorphic
   hash and compare to cyclic records of closures. *)
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

(* The index of the first of [children] that is not at its index among
   [parent]'s children, counting from [index], if any. *)
let rec first_misplaced parent index = function
  | [] -> None
  | Fiber f :: rest -> probe parent index (fiber_node f) rest
  | Embedded n :: rest -> probe parent index n rest
  | Memo_span span :: rest -> (
      let in_place =
        span.reused
        &&
        match (span.first_node, Renderable.Private.child_at parent index) with
        | Some first, Some n -> n == first
        | None, _ -> true
        | Some _, None -> false
      in
      let misplaced =
        if in_place then None
        else first_misplaced parent index span.span_children
      in
      match misplaced with
      | None -> first_misplaced parent (index + span.node_count) rest
      | Some _ -> misplaced)

and probe parent index node rest =
  match Renderable.Private.child_at parent index with
  | Some n when n == node -> first_misplaced parent (index + 1) rest
  | Some _ | None -> Some index

(* [iter_from from children f] applies [f] to the nodes of [children] from
   the [from]th on, in order. *)
let iter_from from children f =
  let position = ref 0 in
  let visit node =
    if !position >= from then f node;
    incr position
  in
  let rec go = function
    | [] -> ()
    | Fiber fiber :: rest ->
        visit (fiber_node fiber);
        go rest
    | Embedded n :: rest ->
        visit n;
        go rest
    | Memo_span span :: rest ->
        if !position + span.node_count <= from then
          position := !position + span.node_count
        else go span.span_children;
        go rest
  in
  go children

let commit_placement parent_node ~stale new_children =
  detach_stale_embeds ~stale new_children;
  let parent = Renderable.child_target parent_node in
  match first_misplaced parent 0 new_children with
  | None -> ()
  | Some index ->
      Renderable.Private.place_children parent index
        (iter_from index new_children)

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

let rec reconcile_element ~(parent_node : Renderable.t) m ~viewport_width
    (elem : unit Vnode.element) : child =
  match find_match m elem with
  | (Fiber f as child) :: _ ->
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
      child
  | _ ->
      let f = create_fiber ~parent:parent_node elem in
      f.pass <- m.pass;
      reconcile_children f ~viewport_width elem.children;
      f.declared_children <- elem.children;
      f.viewport_width <- viewport_width;
      update_declared_focus f;
      Fiber f

and reconcile_children f ~viewport_width children =
  let stale = ref [] in
  let children =
    reconcile_list ~stale f.children (fun m b ->
        walk_list ~parent_node:(fiber_node f) m b ~viewport_width ~stale
          ~theme:None children)
  in
  if children != f.children then f.children <- children;
  commit_placement (fiber_node f) ~stale:!stale children

(* Reconciles one list of children, a parent's or a memo span's, against
   [old_children]: [walk] reconciles the new children through a matcher and
   gives them to a builder. Embedded nodes that leave the list are collected
   in [stale]. *)
and reconcile_list ~stale (old_children : child list) walk : child list =
  let m = matcher old_children in
  let b = builder old_children in
  walk m b;
  (* A discarded span's embedded nodes may reappear elsewhere, so they are
     left to placement. *)
  let unused = ref [] and spans = ref [] in
  List.iter
    (function
      | Embedded n -> stale := n :: !stale
      | Fiber f -> if f.pass <> m.pass then unused := f :: !unused
      | Memo_span span ->
          if span.span_pass <> m.pass then spans := Memo_span span :: !spans)
    old_children;
  (match (!unused, !spans) with
  | [], [] -> ()
  | unused, spans ->
      let span_fibers, span_embeds = fibers_and_embeds (List.rev spans) in
      stale := List.rev_append span_embeds !stale;
      destroy_fibers (List.rev_append unused span_fibers));
  finish b

(* Fragments, themed vnodes and Empty nodes are structural: they group
   children but are no widgets. The walk goes through them to the concrete
   children (elements, embeds, and memos, whose views are built only when
   they cannot be reused) in order. A child inside [themed] wrappers takes
   their functions composed, outermost first, as its place's theme
   ([Renderable.Private.set_place_theme]); a view without [themed] allocates
   nothing for them. *)
and walk ~parent_node m b ~viewport_width ~stale ~theme (vnode : unit Vnode.t) =
  match vnode with
  | Vnode.Empty -> ()
  | Vnode.Element elem ->
      let child = reconcile_element ~parent_node m ~viewport_width elem in
      (match child with
      | Fiber f -> Renderable.Private.set_place_theme (fiber_node f) theme
      | Embedded _ | Memo_span _ -> ());
      emit b child
  | Vnode.Embed n ->
      Renderable.Private.set_place_theme n theme;
      let child =
        match b.rest with
        | (Embedded n' as child) :: _ when n' == n -> child
        | _ -> Embedded n
      in
      emit b child
  | Vnode.Memo memo ->
      let child =
        match take_memo m with
        | (Memo_span span as child) :: _ ->
            span.span_pass <- m.pass;
            let span' =
              reconcile_span ~parent_node ~viewport_width ~stale ~theme
                (Some span) memo
            in
            if span' == span then child else Memo_span span'
        | _ ->
            let span =
              reconcile_span ~parent_node ~viewport_width ~stale ~theme None
                memo
            in
            span.span_pass <- m.pass;
            Memo_span span
      in
      emit b child
  | Vnode.Themed (f, child) ->
      walk ~parent_node m b ~viewport_width ~stale
        ~theme:(Some (compose theme f))
        child
  | Vnode.Fragment children ->
      walk_list ~parent_node m b ~viewport_width ~stale ~theme children
  | Vnode.Viewport_switch { at_least_width; wide; narrow } ->
      walk ~parent_node m b ~viewport_width ~stale ~theme
        (if viewport_width >= at_least_width then wide else narrow)

and walk_list ~parent_node m b ~viewport_width ~stale ~theme children =
  match children with
  | [] -> ()
  | child :: rest ->
      walk ~parent_node m b ~viewport_width ~stale ~theme child;
      walk_list ~parent_node m b ~viewport_width ~stale ~theme rest

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
          span_pass = 0;
        }
      in
      update_span ~parent_node ~viewport_width ~stale span;
      span

and update_span ~parent_node ~viewport_width ~stale span =
  let children =
    reconcile_list ~stale span.span_children (fun m b ->
        walk ~parent_node m b ~viewport_width ~stale ~theme:span.span_theme
          span.view)
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
    reconcile_list ~stale t.root_children (fun m b ->
        walk ~parent_node:t.container m b ~viewport_width ~stale ~theme:None
          vnode)
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
