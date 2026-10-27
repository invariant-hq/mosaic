(* The kinds of the built-in elements: each widget's create, update and
   layout, and the events it reports. *)

open Mosaic_ui

(* The events of a widget that reports none. *)
type nothing = |

(* The events of the widgets that report more than one. *)
type 'a choice = Change of 'a | Activate of 'a
type edit = Edit of Editor.t | Submit of string
type hover = Choose of int choice | Hover of int option
type tree = Tree_change of Tree.State.t | Tree_activate of Tree.item

let box : (Box.Props.t, nothing, Box.t) Kind.t =
  Kind.make ~name:"box"
    ~create:(fun ~parent ~layout props ->
      Box.create ~parent ~style:layout props)
    ~node:Box.node ~layout:Box.set_style ~update:Box.apply_props
    ~equal:Box.Props.equal ()

(* A modal is a box that the reconciler shows in the top layer, whose event
   is a request to close it. *)
let modal : (Box.Props.t, unit, Box.t) Kind.t =
  Kind.make ~name:"modal" ~create:box.create ~node:Box.node
    ~layout:Box.set_style ~update:Box.apply_props ~equal:Box.Props.equal ()

let text =
  Kind.make ~name:"text"
    ~create:(fun ~parent ~layout props ->
      Text.create ~parent ~style:layout props)
    ~node:Text.node ~update:Text.apply_props ~equal:Text.Props.equal
    ~handler:(fun text emit -> Text.set_on_selection text emit)
    ()

let slider =
  Kind.make ~name:"slider"
    ~create:(fun ~parent ~layout props ->
      Slider.create ~parent ~style:layout props)
    ~node:Slider.node ~update:Slider.apply_props ~equal:Slider.Props.equal
    ~events:(fun slider emit -> Slider.set_on_change slider (Some emit))
    ()

let input =
  Kind.make ~name:"input"
    ~create:(fun ~parent ~layout props ->
      Text_input.create ~on_change:ignore ~parent ~style:layout props)
    ~node:Text_input.node ~update:Text_input.apply_props
    ~equal:Text_input.Props.equal ~focusable:true
    ~events:(fun input emit ->
      Text_input.set_on_change input (fun editor -> emit (Edit editor));
      Text_input.set_on_submit input (Some (fun text -> emit (Submit text))))
    ()

let textarea =
  Kind.make ~name:"textarea"
    ~create:(fun ~parent ~layout props ->
      Textarea.create ~on_change:ignore ~parent ~style:layout props)
    ~node:Textarea.node ~update:Textarea.apply_props ~equal:Textarea.Props.equal
    ~focusable:true
    ~events:(fun textarea emit ->
      Textarea.set_on_change textarea (fun editor -> emit (Edit editor));
      Textarea.set_on_submit textarea (Some (fun text -> emit (Submit text))))
    ()

let select =
  Kind.make ~name:"select"
    ~create:(fun ~parent ~layout props ->
      Select.create ~parent ~style:layout props)
    ~node:Select.node ~update:Select.apply_props ~equal:Select.Props.equal
    ~focusable:true
    ~events:(fun select emit ->
      Select.set_on_change select (Some (fun i -> emit (Change i)));
      Select.set_on_activate select (Some (fun i -> emit (Activate i))))
    ()

let tab_select =
  Kind.make ~name:"tab_select"
    ~create:(fun ~parent ~layout props ->
      Tab_select.create ~parent ~style:layout props)
    ~node:Tab_select.node ~update:Tab_select.apply_props
    ~equal:Tab_select.Props.equal ~focusable:true
    ~events:(fun tab_select emit ->
      Tab_select.set_on_change tab_select (Some (fun i -> emit (Change i)));
      Tab_select.set_on_activate tab_select (Some (fun i -> emit (Activate i))))
    ()

(* A canvas draws with the function its element declares last. *)
type canvas = { canvas : Canvas.t; mutable draw : Canvas.t -> dt:float -> unit }

let canvas : (Canvas.t -> dt:float -> unit, nothing, canvas) Kind.t =
  Kind.make ~name:"canvas"
    ~create:(fun ~parent ~layout draw ->
      let canvas = Canvas.create ~parent ~style:layout () in
      let c = { canvas; draw } in
      Canvas.set_on_draw canvas (Some (fun canvas ~dt -> c.draw canvas ~dt));
      c)
    ~node:(fun c -> Canvas.node c.canvas)
    ~update:(fun c draw -> c.draw <- draw)
    ~equal:( == ) ()

(* A picture's node, its picture, and the style its element declares. *)
type picture = {
  node : Renderable.t;
  mutable picture : Matrix_grid.picture;
  mutable style : Toffee.Style.t;
}

(* A picture's ratio of columns to rows, at the terminal's cell size. *)
let picture_ratio p =
  let pw, ph = Matrix_grid.picture_size p.picture in
  let cw, ch = Option.value (Renderable.cell_size p.node) ~default:(10, 20) in
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
    Option.value (Renderable.cell_size p.node) ~default:(10, 20) |> fst
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

let picture : (Matrix_grid.picture, nothing, picture) Kind.t =
  Kind.make ~name:"picture"
    ~create:(fun ~parent ~layout picture ->
      let node = Renderable.create ~parent ~style:layout () in
      let p = { node; picture; style = layout } in
      apply_picture_style p;
      Renderable.set_measure node (Some (picture_measure p));
      Renderable.set_render node (picture_render p);
      p)
    ~node:(fun p -> p.node)
    ~layout:(fun p style ->
      p.style <- style;
      apply_picture_style p)
    ~update:(fun p picture ->
      p.picture <- picture;
      apply_picture_style p;
      Renderable.mark_dirty p.node)
    ~equal:( == ) ()

let spinner : (Spinner.Props.t, nothing, Spinner.t) Kind.t =
  Kind.make ~name:"spinner"
    ~create:(fun ~parent ~layout props ->
      Spinner.create ~parent ~style:layout props)
    ~node:Spinner.node ~update:Spinner.apply_props ~equal:Spinner.Props.equal ()

let progress_bar : (Progress_bar.Props.t, nothing, Progress_bar.t) Kind.t =
  Kind.make ~name:"progress_bar"
    ~create:(fun ~parent ~layout props ->
      Progress_bar.create ~parent ~style:layout props)
    ~node:Progress_bar.node ~update:Progress_bar.apply_props
    ~equal:Progress_bar.Props.equal ()

let scroll_bar =
  Kind.make ~name:"scroll_bar"
    ~create:(fun ~parent ~layout props ->
      Scroll_bar.create ~parent ~style:layout props)
    ~node:Scroll_bar.node ~update:Scroll_bar.apply_props
    ~equal:Scroll_bar.Props.equal
    ~events:(fun scroll_bar emit ->
      Scroll_bar.set_on_change scroll_bar (Some emit))
    ()

let scroll_box =
  Kind.make ~name:"scroll_box"
    ~create:(fun ~parent ~layout props ->
      Scroll_box.create ~parent ~style:layout props)
    ~node:Scroll_box.node ~layout:Scroll_box.set_style
    ~update:Scroll_box.apply_props ~equal:Scroll_box.Props.equal ~focusable:true
    ~events:(fun scroll_box emit ->
      Scroll_box.set_on_scroll scroll_box (Some emit))
    ()

(* A press reports with the handler declared when it began, since its hover
   and change can make the application declare other rows before its
   activation. *)
let table =
  Kind.make ~name:"table"
    ~create:(fun ~parent ~layout props ->
      Table.create ~parent ~style:layout props)
    ~node:Table.node ~update:Table.apply_props ~equal:Table.Props.equal
    ~focusable:true
    ~handler:(fun table emit ->
      Table.set_on_change table
        (Option.map (fun emit i -> emit (Choose (Change i))) emit);
      Table.set_on_activate table
        (Option.map (fun emit i -> emit (Choose (Activate i))) emit);
      Table.set_on_hover table (Option.map (fun emit i -> emit (Hover i)) emit))
    ()

let code =
  Kind.make ~name:"code"
    ~create:(fun ~parent ~layout props ->
      Code.create ~parent ~style:layout props)
    ~node:Code.node ~update:Code.apply_props ~equal:Code.Props.equal
    ~events:(fun code emit -> Code.set_on_selection code (Some emit))
    ()

let line_number : (Line_number.Props.t, nothing, Line_number.t) Kind.t =
  Kind.make ~name:"line_number"
    ~create:(fun ~parent ~layout props ->
      Line_number.create ~parent ~style:layout props)
    ~node:Line_number.node ~update:Line_number.apply_props
    ~equal:Line_number.Props.equal ()

let markdown =
  Kind.make ~name:"markdown"
    ~create:(fun ~parent ~layout props ->
      Markdown.create ~parent ~layout_style:layout props)
    ~node:Markdown.node ~layout:Markdown.set_layout_style
    ~update:Markdown.apply_props ~equal:Markdown.Props.equal
    ~events:(fun markdown emit ->
      Markdown.set_on_selection markdown (Some emit))
    ()

let diff =
  Kind.make ~name:"diff"
    ~create:(fun ~parent ~layout props ->
      Diff.create ~parent ~style:layout props)
    ~node:Diff.node ~update:Diff.apply_props ~equal:Diff.Props.equal
    ~handler:(fun diff emit -> Diff.set_on_line_click diff emit)
    ()

let tree =
  Kind.make ~name:"tree"
    ~create:(fun ~parent ~layout props ->
      Tree.create ~parent ~style:layout props)
    ~node:Tree.node ~update:Tree.apply_props ~equal:Tree.Props.equal
    ~focusable:true
    ~events:(fun tree emit ->
      Tree.set_on_change tree (Some (fun state -> emit (Tree_change state)));
      Tree.set_on_activate tree (Some (fun item -> emit (Tree_activate item))))
    ()
