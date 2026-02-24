(* ───── Props ───── *)

module Props = struct
  type t = {
    content : string;
    highlights : Text_buffer.span list;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?(highlights = []) ?(text_style = Ansi.Style.default)
      ?(wrap = `None) ?(tab_width = 4) ?(truncate = false) ?(selectable = true)
      ?selection_bg ?selection_fg () =
    {
      content;
      highlights;
      text_style;
      wrap;
      tab_width;
      truncate;
      selectable;
      selection_bg;
      selection_fg;
    }

  let default = make ()

  let spans_equal a b =
    List.compare_length_with a (List.length b) = 0
    && List.for_all2
         (fun (a : Text_buffer.span) (b : Text_buffer.span) ->
           String.equal a.text b.text && Ansi.Style.equal a.style b.style)
         a b

  let equal a b =
    String.equal a.content b.content
    && spans_equal a.highlights b.highlights
    && Ansi.Style.equal a.text_style b.text_style
    && a.wrap = b.wrap && a.tab_width = b.tab_width && a.truncate = b.truncate
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  buf : Text_buffer.t;
  surface : Text_surface.t;
  mutable props : Props.t;
  mutable has_highlights : bool;
  mutable on_selection : ((int * int) option -> unit) option;
  mutable last_selection : (int * int) option;
}

(* ───── Accessors ───── *)

let node t = t.node
let buffer t = t.buf
let surface t = t.surface
let set_on_selection t h = t.on_selection <- h

let fire_on_selection t =
  let selection = Text_surface.selection t.surface in
  if selection <> t.last_selection then begin
    t.last_selection <- selection;
    (match t.on_selection with Some f -> f selection | None -> ());
    true
  end
  else false

(* ───── Line Info Provider ───── *)

let register_line_info t =
  Renderable.set_line_info_provider t.node
    (Some
       (fun () ->
         let di = Text_surface.display_info t.surface in
         {
           Renderable.line_count = Text_buffer.line_count t.buf;
           display_line_count = Array.length di.lines;
           line_sources = di.line_sources;
           line_wrap_indices = di.line_wrap_indices;
           scroll_y = Text_surface.scroll_y t.surface;
         }))

(* ───── Selection Callbacks ───── *)

let register_selection t =
  if t.props.selectable then
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

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content
    ?highlights ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?content ?highlights ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let buf =
    Text_buffer.create ~default_style:props.text_style
      ~tab_width:props.tab_width ()
  in
  let surface = Text_surface.create node buf in
  let t =
    {
      node;
      buf;
      surface;
      props;
      has_highlights = false;
      on_selection;
      last_selection = None;
    }
  in
  (* Set initial content or highlights *)
  if props.highlights <> [] then begin
    Text_buffer.set_styled_text buf props.highlights;
    Text_surface.invalidate surface;
    t.has_highlights <- true
  end
  else if props.content <> "" then begin
    Text_buffer.set_text buf props.content;
    Text_surface.invalidate surface
  end;
  (* Set initial wrap mode *)
  if props.wrap <> `None then Text_surface.set_wrap surface props.wrap;
  (* Set initial truncate *)
  if props.truncate then Text_surface.set_truncate surface true;
  (* Set initial selection colors *)
  Text_surface.set_selection_bg surface props.selection_bg;
  Text_surface.set_selection_fg surface props.selection_fg;
  (* Register selection callbacks *)
  register_selection t;
  (* Register line info provider *)
  register_line_info t;
  t

(* ───── Content ───── *)

let set_content t s =
  t.has_highlights <- false;
  Text_buffer.set_text t.buf s;
  Text_surface.invalidate t.surface

let set_highlights t spans =
  t.has_highlights <- true;
  Text_buffer.set_styled_text t.buf spans;
  Text_surface.invalidate t.surface

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  (* Highlights take priority over plain content *)
  if props.highlights <> [] then begin
    if not (Props.spans_equal t.props.highlights props.highlights) then begin
      t.has_highlights <- true;
      Text_buffer.set_styled_text t.buf props.highlights;
      Text_surface.invalidate t.surface
    end
  end
  else if t.has_highlights then begin
    (* Switching from highlights to plain content *)
    t.has_highlights <- false;
    Text_buffer.set_text t.buf props.content;
    Text_surface.invalidate t.surface
  end
  else if not (String.equal t.props.content props.content) then begin
    Text_buffer.set_text t.buf props.content;
    Text_surface.invalidate t.surface
  end;
  (* Text style *)
  if not (Ansi.Style.equal t.props.text_style props.text_style) then
    Text_buffer.set_default_style t.buf props.text_style;
  (* Wrap mode *)
  if t.props.wrap <> props.wrap then Text_surface.set_wrap t.surface props.wrap;
  (* Tab width *)
  if t.props.tab_width <> props.tab_width then
    Text_buffer.set_tab_width t.buf props.tab_width;
  (* Truncate *)
  if t.props.truncate <> props.truncate then
    Text_surface.set_truncate t.surface props.truncate;
  (* Selection colors *)
  if not (Option.equal Ansi.Color.equal t.props.selection_bg props.selection_bg)
  then Text_surface.set_selection_bg t.surface props.selection_bg;
  if not (Option.equal Ansi.Color.equal t.props.selection_fg props.selection_fg)
  then Text_surface.set_selection_fg t.surface props.selection_fg;
  (* Selectable *)
  if t.props.selectable <> props.selectable then begin
    t.props <- { t.props with selectable = props.selectable };
    register_selection t
  end;
  t.props <- props

(* ───── Query ───── *)

let line_count t = Text_buffer.line_count t.buf
let display_line_count t = Text_surface.display_line_count t.surface
