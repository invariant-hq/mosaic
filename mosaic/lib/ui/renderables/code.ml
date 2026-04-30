(* Syntax *)

type syntax = {
  language : string option;
  style : Syntax_style.t;
  highlights : Syntax_highlight.t;
  conceal : bool;
  draw_unstyled : bool;
  streaming : bool;
}

let syntax ?language ?(style = Syntax_style.default) ?(conceal = true)
    ?(draw_unstyled = true) ?(streaming = false) highlights =
  { language; style; highlights; conceal; draw_unstyled; streaming }

let syntax_equal a b =
  Option.equal String.equal a.language b.language
  && a.style == b.style
  && a.highlights = b.highlights
  && Bool.equal a.conceal b.conceal
  && Bool.equal a.draw_unstyled b.draw_unstyled
  && Bool.equal a.streaming b.streaming

(* Props *)

module Props = struct
  type t = {
    content : string;
    syntax : syntax option;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?syntax ?(text_style = Ansi.Style.default)
      ?(wrap = `None) ?(tab_width = 4) ?(truncate = false) ?(selectable = true)
      ?selection_bg ?selection_fg () =
    {
      content;
      syntax;
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
    && Option.equal syntax_equal a.syntax b.syntax
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
  highlighting : bool;
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text
let set_on_selection t h = Text_renderable.set_on_selection t.text h

(* Rendering *)

let spans_for_syntax content syntax =
  try
    Some
      (Syntax_highlight.to_spans ~conceal:syntax.conceal ~style:syntax.style
         ~content syntax.highlights)
  with _ -> None

let render_content t (props : Props.t) =
  if String.equal props.content "" then begin
    Text_renderable.set_text t.text ""
  end
  else
    match props.syntax with
    | Some syntax -> begin
        match spans_for_syntax props.content syntax with
        | Some spans -> Text_renderable.set_styled_text t.text spans
        | None -> Text_renderable.set_text t.text props.content
      end
    | None -> Text_renderable.set_text t.text props.content

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content ?syntax
    ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let props =
    Props.make ?content ?syntax ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~tab_width:props.tab_width
      ~truncate:props.truncate ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ?on_selection ()
  in
  let t = { text; props; highlighting = false } in
  render_content t props;
  t

(* Content *)

let set_content t s =
  let props = { t.props with content = s } in
  render_content t props;
  t.props <- props

let set_syntax t syntax =
  let props = { t.props with syntax } in
  render_content t props;
  t.props <- props

let set_highlights t highlights =
  let syntax =
    match t.props.syntax with
    | Some syntax -> { syntax with highlights }
    | None -> syntax highlights
  in
  set_syntax t (Some syntax)

(* Apply Props *)

let apply_props t (props : Props.t) =
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  if
    (not (String.equal t.props.content props.content))
    || (not (Option.equal syntax_equal t.props.syntax props.syntax))
    || style_changed
  then render_content t props;
  (* Wrap mode *)
  if t.props.wrap <> props.wrap then Text_renderable.set_wrap t.text props.wrap;
  (* Tab width *)
  if t.props.tab_width <> props.tab_width then
    Text_renderable.set_tab_width t.text props.tab_width;
  (* Truncate *)
  if t.props.truncate <> props.truncate then
    Text_renderable.set_truncate t.text props.truncate;
  (* Selection colors *)
  if not (Option.equal Ansi.Color.equal t.props.selection_bg props.selection_bg)
  then Text_renderable.set_selection_bg t.text props.selection_bg;
  if not (Option.equal Ansi.Color.equal t.props.selection_fg props.selection_fg)
  then Text_renderable.set_selection_fg t.text props.selection_fg;
  (* Selectable *)
  if t.props.selectable <> props.selectable then
    Text_renderable.set_selectable t.text props.selectable;
  t.props <- props

(* Query *)

let is_highlighting t = t.highlighting
let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text
