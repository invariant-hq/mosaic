(* Props *)

module Props = struct
  type t = {
    content : string;
    spans : Text_buffer.span list;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?(spans = []) ?(text_style = Ansi.Style.default)
      ?(wrap = `None) ?(tab_width = 4) ?(truncate = false) ?(selectable = true)
      ?selection_bg ?selection_fg () =
    {
      content;
      spans;
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
    && spans_equal a.spans b.spans
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
  mutable has_spans : bool;
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text
let set_on_selection t h = Text_renderable.set_on_selection t.text h

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content ?spans
    ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let props =
    Props.make ?content ?spans ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~tab_width:props.tab_width
      ~truncate:props.truncate ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ?on_selection ()
  in
  let t = { text; props; has_spans = false } in
  if props.spans <> [] then begin
    Text_renderable.set_styled_text text props.spans;
    t.has_spans <- true
  end
  else if props.content <> "" then Text_renderable.set_text text props.content;
  t

(* Content *)

let set_content t s =
  t.has_spans <- false;
  Text_renderable.set_text t.text s

let set_spans t spans =
  t.has_spans <- true;
  Text_renderable.set_styled_text t.text spans

(* Apply Props *)

let apply_props t (props : Props.t) =
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  (* Styled spans take priority over plain content *)
  if props.spans <> [] then begin
    if not (Props.spans_equal t.props.spans props.spans) then begin
      t.has_spans <- true;
      Text_renderable.set_styled_text t.text props.spans
    end
  end
  else if t.has_spans then begin
    (* Switching from styled spans to plain content *)
    t.has_spans <- false;
    Text_renderable.set_text t.text props.content
  end
  else if not (String.equal t.props.content props.content) then begin
    Text_renderable.set_text t.text props.content
  end
  else if style_changed then begin
    Text_renderable.set_text t.text props.content
  end;
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

let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text
