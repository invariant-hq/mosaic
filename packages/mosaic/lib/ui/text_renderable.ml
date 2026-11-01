type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  surface : Text_surface.t;
}

(* Accessors *)

let node t = t.node
let buffer t = t.buffer
let surface t = t.surface

(* Selection *)

let set_selectable t declaration = Renderable.set_selectable t.node declaration

let selected_text t =
  match Renderable.Private.selected_range t.node with
  | None -> ""
  | Some (first, last) ->
      Text_buffer.text_in_range t.buffer ~start:first ~len:(last - first)

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

(* Text rows *)

let register_text_rows t =
  Renderable.set_text_rows_provider t.node
    (Some
       (fun () ->
         {
           Renderable.layout = Text_surface.layout t.surface;
           scroll_y = Text_surface.scroll_y t.surface;
         }))

(* Content *)

let set_text t s =
  Text_buffer.set_text t.buffer s;
  Text_surface.invalidate t.surface

let set_styled_text t spans =
  Text_buffer.set_styled_text t.buffer spans;
  Text_surface.invalidate t.surface

let set_annotations t annotations =
  let content = Text_buffer.Annotations.content annotations in
  let current = Text_buffer.plain_text t.buffer in
  if current != content && not (String.equal current content) then
    Text_buffer.set_text t.buffer content;
  Text_buffer.set_annotations t.buffer annotations;
  Text_surface.invalidate t.surface

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

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?text_style
    ?wrap ?tab_width ?truncate ?selectable () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let text_style = Option.value text_style ~default:Ansi.Style.default in
  let tab_width = Option.value tab_width ~default:2 in
  let wrap = Option.value wrap ~default:`None in
  let truncate = Option.value truncate ~default:false in
  let buffer = Text_buffer.create ~default_style:text_style ~tab_width () in
  let surface = Text_surface.create node buffer in
  let t = { node; buffer; surface } in
  if wrap <> `None then Text_surface.set_wrap surface wrap;
  if truncate then Text_surface.set_truncate surface true;
  Renderable.set_selectable node selectable;
  register_text_rows t;
  t

(* Query *)

let line_count t = Text_buffer.line_count t.buffer
let display_line_count t = Text_surface.display_line_count t.surface
