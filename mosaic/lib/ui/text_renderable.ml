type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  surface : Text_surface.t;
  mutable selectable : bool;
  mutable on_selection : ((int * int) option -> unit) option;
  mutable last_selection : (int * int) option;
}

(* Accessors *)

let node t = t.node
let buffer t = t.buffer
let surface t = t.surface
let set_on_selection t h = t.on_selection <- h

(* Selection *)

let fire_on_selection t =
  let selection = Text_surface.selection t.surface in
  if selection <> t.last_selection then begin
    t.last_selection <- selection;
    Option.iter (fun f -> f selection) t.on_selection;
    true
  end
  else false

let register_selection t =
  if t.selectable then
    Renderable.set_selection t.node
      ~should_start:(fun ~x ~y ->
        let nx = Renderable.x t.node in
        let ny = Renderable.y t.node in
        let w = Renderable.width t.node in
        let h = Renderable.height t.node in
        x >= nx && x < nx + w && y >= ny && y < ny + h)
      ~on_change:(fun sel ->
        match sel with
        | None ->
            Text_surface.reset_selection t.surface;
            ignore (fire_on_selection t : bool);
            true
        | Some sel ->
            let nx = Renderable.x t.node in
            let ny = Renderable.y t.node in
            let anchor = Selection.anchor sel in
            let focus = Selection.focus sel in
            let ax = anchor.x - nx and ay = anchor.y - ny in
            let fx = focus.x - nx and fy = focus.y - ny in
            let changed =
              if Selection.is_start sel then
                Text_surface.set_local_selection t.surface ~anchor_x:ax
                  ~anchor_y:ay ~focus_x:fx ~focus_y:fy
              else
                Text_surface.update_local_selection t.surface ~anchor_x:ax
                  ~anchor_y:ay ~focus_x:fx ~focus_y:fy
            in
            if changed then Renderable.request_render t.node;
            ignore (fire_on_selection t : bool);
            Text_surface.has_selection t.surface)
      ~clear:(fun () ->
        Text_surface.reset_selection t.surface;
        ignore (fire_on_selection t : bool))
      ~get_text:(fun () -> Text_surface.selected_text t.surface)
  else begin
    Text_surface.reset_selection t.surface;
    ignore (fire_on_selection t : bool);
    Renderable.unset_selection t.node
  end

let set_selectable t selectable =
  if t.selectable <> selectable then begin
    t.selectable <- selectable;
    register_selection t
  end

let selected_text t = Text_surface.selected_text t.surface

(* Highlights *)

let add_highlight t h =
  Text_buffer.add_highlight t.buffer h;
  Renderable.request_render t.node

let remove_highlights_by_ref t ref_id =
  Text_buffer.remove_highlights_by_ref t.buffer ref_id;
  Renderable.request_render t.node

let clear_highlights t =
  Text_buffer.clear_highlights t.buffer;
  Renderable.request_render t.node

(* Line information *)

let register_line_info t =
  Renderable.set_line_info_provider t.node
    (Some
       (fun () ->
         let di = Text_surface.display_info t.surface in
         {
           Renderable.line_count = Text_buffer.line_count t.buffer;
           display_line_count = Array.length di.lines;
           line_sources = di.line_sources;
           line_wrap_indices = di.line_wrap_indices;
           scroll_y = Text_surface.scroll_y t.surface;
         }))

(* Content *)

let set_text t s =
  Text_surface.set_render_enabled t.surface true;
  Text_buffer.set_text t.buffer s;
  Text_surface.invalidate t.surface

let set_styled_text t spans =
  Text_surface.set_render_enabled t.surface true;
  Text_buffer.set_styled_text t.buffer spans;
  Text_surface.invalidate t.surface

let set_render_enabled t enabled =
  Text_surface.set_render_enabled t.surface enabled

let set_text_style ?restyle t style =
  if not (Ansi.Style.equal (Text_buffer.default_style t.buffer) style) then begin
    Text_buffer.set_default_style t.buffer style;
    match restyle with None -> () | Some s -> set_text t s
  end

(* Configuration *)

let set_wrap t wrap = Text_surface.set_wrap t.surface wrap

let set_tab_width t width =
  let before = Text_buffer.tab_width t.buffer in
  Text_buffer.set_tab_width t.buffer width;
  if Text_buffer.tab_width t.buffer <> before then
    Text_surface.invalidate t.surface

let set_truncate t truncate = Text_surface.set_truncate t.surface truncate
let set_selection_bg t color = Text_surface.set_selection_bg t.surface color
let set_selection_fg t color = Text_surface.set_selection_fg t.surface color

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?text_style
    ?wrap ?tab_width ?truncate ?selectable ?selection_bg ?selection_fg
    ?on_selection () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let text_style = Option.value text_style ~default:Ansi.Style.default in
  let tab_width = Option.value tab_width ~default:2 in
  let wrap = Option.value wrap ~default:`None in
  let truncate = Option.value truncate ~default:false in
  let selectable = Option.value selectable ~default:true in
  let buffer = Text_buffer.create ~default_style:text_style ~tab_width () in
  let surface = Text_surface.create node buffer in
  let t =
    { node; buffer; surface; selectable; on_selection; last_selection = None }
  in
  if wrap <> `None then Text_surface.set_wrap surface wrap;
  if truncate then Text_surface.set_truncate surface true;
  Text_surface.set_selection_bg surface selection_bg;
  Text_surface.set_selection_fg surface selection_fg;
  register_selection t;
  register_line_info t;
  t

(* Query *)

let line_count t = Text_buffer.line_count t.buffer
let display_line_count t = Text_surface.display_line_count t.surface
