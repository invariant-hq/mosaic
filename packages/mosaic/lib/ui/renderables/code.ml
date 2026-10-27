module Highlighter = Syntax_session.Highlighter

(* Syntax *)

type syntax = Syntax_session.syntax

let syntax = Syntax_session.syntax
let with_highlighter = Syntax_session.with_highlighter

(* Props *)

module Props = struct
  type t = {
    content : string;
    syntax : syntax option;
    conceal : bool;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?syntax ?(conceal = true)
      ?(text_style = Ansi.Style.default) ?(wrap = `None) ?(tab_width = 4)
      ?(truncate = false) ?(selectable = true) ?selection_bg ?selection_fg () =
    if tab_width < 1 then invalid_arg "Code.Props.make: tab width below 1";
    {
      content;
      syntax;
      conceal;
      text_style;
      wrap;
      tab_width;
      truncate;
      selectable;
      selection_bg;
      selection_fg;
    }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && Option.equal Syntax_session.equal a.syntax b.syntax
    && Bool.equal a.conceal b.conceal
    && Ansi.Style.equal a.text_style b.text_style
    && a.wrap = b.wrap && a.tab_width = b.tab_width && a.truncate = b.truncate
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
end

(* Types *)

type t = {
  text : Text_renderable.t;
  mutable props : Props.t;
  session : Syntax_session.t;
  mutable on_rows_change : (unit -> unit) option;
  mutable lines : int * int;
      (* The logical lines whose captures the view last needed. *)
  mutable shown_style : Syntax_style.t;
  mutable styled : bool;
      (* Whether captures are shown, and the syntax style they were last shown
         in, to show them again when the theme in scope gives another. *)
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text
let set_on_selection t h = Text_renderable.set_on_selection t.text h
let set_on_rows_change t h = t.on_rows_change <- h
let notify_rows_change t = Option.iter (fun f -> f ()) t.on_rows_change

let pending_work t =
  if Syntax_session.is_highlighting t.session then
    let label = Option.bind t.props.syntax Syntax_session.language in
    Some (Renderable.Pending.make ?label ~kind:"code.highlight" ())
  else None

(* Rendering *)

let request_render t = Renderable.request_render (Text_renderable.node t.text)

(* The style of [syntax]: the one it was given, or the theme's in scope. *)
let syntax_style t syntax =
  match Syntax_session.style syntax with
  | Some style -> style
  | None -> Theme.syntax (Renderable.theme (Text_renderable.node t.text))

(* [show t] paints the content with the captures that describe it, or plain. *)
let show t =
  let content = t.props.content in
  match
    (Syntax_session.shown t.session ~conceal:t.props.conceal, t.props.syntax)
  with
  | Some captures, Some syntax ->
      let style = syntax_style t syntax in
      t.shown_style <- style;
      t.styled <- true;
      Text_renderable.set_annotations t.text
        (Syntax_highlight.to_annotations ~conceal:t.props.conceal ~style
           captures)
  | Some _, None | None, _ ->
      t.styled <- false;
      Text_renderable.set_text t.text content

(* The bytes of [content] from the start of logical line [first] to the end
   of line [last], found by its line breaks. *)
let lines_range content (first, last) =
  let n = String.length content in
  let rec start_of line i =
    if line = 0 || i >= n then i
    else
      match content.[i] with
      | '\n' -> start_of (line - 1) (i + 1)
      | '\r' when i + 1 < n && content.[i + 1] = '\n' -> start_of line (i + 1)
      | '\r' -> start_of (line - 1) (i + 1)
      | _ -> start_of line (i + 1)
  in
  let a = start_of first 0 in
  (a, if last = max_int then n else start_of (last + 1 - first) a)

let render_content t (props : Props.t) =
  t.props <- props;
  Syntax_session.set t.session ~content:props.content
    ~range:
      (if Syntax_session.needs_range t.session then
         lines_range props.content t.lines
       else (0, 0))
    props.syntax;
  show t;
  if not (Syntax_session.is_highlighting t.session) then notify_rows_change t

(* The bytes whose captures a highlighter is asked for: those of the logical
   lines of the rows that show through the grid's clip and of a screen above
   and below them, or all of the content when concealment changed the bytes
   shown. *)
let view_range t grid =
  let content = t.props.content in
  if Text_buffer.plain_text (buffer t) != content then begin
    t.lines <- (0, max_int);
    (0, String.length content)
  end
  else
    let module Layout = Matrix.Text.Layout in
    let node = node t and surface = surface t in
    let clip = Matrix_grid.clip_region grid in
    let y = Renderable.y node in
    let first = max y clip.y in
    let shown =
      max 1 (min (y + Renderable.height node) (clip.y + clip.height) - first)
    in
    let top = Text_surface.scroll_y surface + first - y in
    let layout = Text_surface.layout surface in
    let source = Layout.source layout in
    let first = Layout.logical_line layout (top - shown) in
    let last = Layout.logical_line layout (top + (2 * shown) - 1) in
    t.lines <- (first, last);
    ( Layout.Source.byte_offset source (Layout.Source.line_start source first),
      Layout.Source.byte_offset source (Layout.Source.line_end source last) )

let render_before t _node grid ~dt:_ =
  let polled = Syntax_session.poll t.session in
  let viewed =
    Syntax_session.needs_range t.session
    && Syntax_session.view t.session ~range:(view_range t grid)
  in
  (* A style the theme gives that differs from the one shown restyles the
     captures, without asking for them again. *)
  let restyled =
    match t.props.syntax with
    | Some syntax when t.styled ->
        let style = syntax_style t syntax in
        style != t.shown_style && not (Syntax_style.equal style t.shown_style)
    | _ -> false
  in
  if polled || viewed || restyled then begin
    show t;
    if not (Syntax_session.is_highlighting t.session) then notify_rows_change t;
    request_render t
  end

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content ?syntax
    ?conceal ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let props =
    Props.make ?content ?syntax ?conceal ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~tab_width:props.tab_width
      ~truncate:props.truncate ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ?on_selection ()
  in
  let node = Text_renderable.node text in
  let session =
    Syntax_session.create
      ~notify:(fun () -> Renderable.request_render node)
      ~off_loop:(Renderable.off_loop node)
  in
  let t =
    {
      text;
      props;
      session;
      on_rows_change = None;
      lines = (0, max_int);
      shown_style = Syntax_style.plain;
      styled = false;
    }
  in
  Renderable.set_render_before node (Some (render_before t));
  Renderable.set_pending_provider node (Some (fun () -> pending_work t));
  Renderable.Private.set_cleanup node (fun () -> Syntax_session.cancel session);
  render_content t props;
  t

(* Content *)

let set_content t s =
  let props = { t.props with content = s } in
  render_content t props

let set_syntax t syntax =
  let props = { t.props with syntax } in
  render_content t props

(* Apply Props *)

let apply_props t (props : Props.t) =
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  let metrics_changed = ref false in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  let content_changed =
    (not (String.equal t.props.content props.content))
    || (not (Option.equal Syntax_session.equal t.props.syntax props.syntax))
    || (not (Bool.equal t.props.conceal props.conceal))
    || style_changed
  in

  (* Wrap mode *)
  if t.props.wrap <> props.wrap then begin
    Text_renderable.set_wrap t.text props.wrap;
    metrics_changed := true
  end;
  (* Tab width *)
  if t.props.tab_width <> props.tab_width then begin
    Text_renderable.set_tab_width t.text props.tab_width;
    metrics_changed := true
  end;
  (* Truncate *)
  if t.props.truncate <> props.truncate then begin
    Text_renderable.set_truncate t.text props.truncate;
    metrics_changed := true
  end;
  (* Selection colors *)
  if not (Option.equal Ansi.Color.equal t.props.selection_bg props.selection_bg)
  then Text_renderable.set_selection_bg t.text props.selection_bg;
  if not (Option.equal Ansi.Color.equal t.props.selection_fg props.selection_fg)
  then Text_renderable.set_selection_fg t.text props.selection_fg;
  (* Selectable *)
  if t.props.selectable <> props.selectable then
    Text_renderable.set_selectable t.text props.selectable;
  if content_changed then render_content t props else t.props <- props;
  if !metrics_changed then notify_rows_change t

(* Query *)

let is_highlighting t = Syntax_session.is_highlighting t.session
let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text
