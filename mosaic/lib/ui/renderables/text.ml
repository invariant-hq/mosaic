(* Fragment type *)

type fragment =
  | Text of { text : string; style : Ansi.Style.t option }
  | Span of { style : Ansi.Style.t option; children : fragment list }

type span = { text : string; style : Ansi.Style.t option }

(* Props *)

module Props = struct
  type t = {
    content : string;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    tab_width : int;
    truncate : bool;
  }

  let make ?(content = "") ?(text_style = Ansi.Style.default) ?(wrap = `None)
      ?(selectable = true) ?selection_bg ?selection_fg ?(tab_width = 2)
      ?(truncate = false) () =
    {
      content;
      text_style;
      wrap;
      selectable;
      selection_bg;
      selection_fg;
      tab_width;
      truncate;
    }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && Ansi.Style.equal a.text_style b.text_style
    && a.wrap = b.wrap
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && a.tab_width = b.tab_width && a.truncate = b.truncate
end

(* Fragment builder *)

module Fragment = struct
  type t = fragment

  let text ?style text = Text { text; style }
  let span ?style children = Span { style; children }
  let span_style style children = span ~style children
  let bold children = span_style (Ansi.Style.make ~bold:true ()) children
  let italic children = span_style (Ansi.Style.make ~italic:true ()) children

  let underline children =
    span_style (Ansi.Style.make ~underline:true ()) children

  let dim children = span_style (Ansi.Style.make ~dim:true ()) children
  let blink children = span_style (Ansi.Style.make ~blink:true ()) children
  let inverse children = span_style (Ansi.Style.make ~inverse:true ()) children
  let hidden children = span_style (Ansi.Style.make ~hidden:true ()) children

  let strikethrough children =
    span_style (Ansi.Style.make ~strikethrough:true ()) children

  let bold_italic children =
    span_style (Ansi.Style.make ~bold:true ~italic:true ()) children

  let bold_underline children =
    span_style (Ansi.Style.make ~bold:true ~underline:true ()) children

  let italic_underline children =
    span_style (Ansi.Style.make ~italic:true ~underline:true ()) children

  let bold_italic_underline children =
    span_style
      (Ansi.Style.make ~bold:true ~italic:true ~underline:true ())
      children

  let fg color children = span_style (Ansi.Style.make ~fg:color ()) children
  let bg color children = span_style (Ansi.Style.make ~bg:color ()) children
  let color = fg
  let bg_color = bg
  let styled style children = span ~style children
end

(* Types *)

type t = {
  text : Text_renderable.t;
  mutable props : Props.t;
  mutable fragments : fragment list;
  mutable flat_cache : span list option;
  mutable has_styled_text : bool;
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text

(* Style merging *)

let merge_style base = function
  | None -> base
  | Some overlay -> Ansi.Style.merge ~base ~overlay

(* Fragment equality and normalization *)

let rec fragments_equal a b =
  match (a, b) with
  | [], [] -> true
  | Text left :: rest_left, Text right :: rest_right ->
      String.equal left.text right.text
      && Option.equal Ansi.Style.equal left.style right.style
      && fragments_equal rest_left rest_right
  | Span left :: rest_left, Span right :: rest_right ->
      Option.equal Ansi.Style.equal left.style right.style
      && fragments_equal left.children right.children
      && fragments_equal rest_left rest_right
  | _ -> false

let normalize_fragments fragments =
  let rec aux acc = function
    | [] -> List.rev acc
    | Text { text = ""; _ } :: rest -> aux acc rest
    | Text { text; style } :: rest ->
        let acc =
          match acc with
          | Text { text = prev_text; style = prev_style } :: tail
            when Option.equal Ansi.Style.equal style prev_style ->
              Text { text = prev_text ^ text; style = prev_style } :: tail
          | _ -> Text { text; style } :: acc
        in
        aux acc rest
    | Span { style; children } :: rest ->
        let normalized_children = aux [] children in
        if normalized_children = [] then aux acc rest
        else aux (Span { style; children = normalized_children } :: acc) rest
  in
  aux [] fragments

(* Fragment to buffer conversion *)

let rec collect_flat_spans acc default_style current_style = function
  | [] -> acc
  | fragment :: rest ->
      let acc =
        match fragment with
        | Text { text; style } ->
            if text = "" then acc
            else
              let effective = merge_style current_style style in
              let buf_span = Text_buffer.{ text; style = effective } in
              buf_span :: acc
        | Span { style; children } ->
            let next_style = merge_style current_style style in
            collect_flat_spans acc default_style next_style children
      in
      collect_flat_spans acc default_style current_style rest

let rebuild_buffer t =
  let default_style = Text_buffer.default_style (buffer t) in
  let buf_spans =
    List.rev (collect_flat_spans [] default_style default_style t.fragments)
  in
  Text_renderable.set_styled_text t.text buf_spans

(* Fragment span cache *)

let flat_style_option ~default_style effective =
  if Ansi.Style.equal effective default_style then None else Some effective

let rec collect_spans acc default_style current_style = function
  | [] -> acc
  | fragment :: rest ->
      let acc =
        match fragment with
        | Text { text; style } ->
            if text = "" then acc
            else
              let effective = merge_style current_style style in
              let style_opt = flat_style_option ~default_style effective in
              { text; style = style_opt } :: acc
        | Span { style; children } ->
            let next_style = merge_style current_style style in
            collect_spans acc default_style next_style children
      in
      collect_spans acc default_style current_style rest

let spans t =
  match t.flat_cache with
  | Some spans -> spans
  | None ->
      let default_style = Text_buffer.default_style (buffer t) in
      let collected =
        List.rev (collect_spans [] default_style default_style t.fragments)
      in
      t.flat_cache <- Some collected;
      collected

let fragments t = t.fragments

let plain_text t =
  let buf = Buffer.create 32 in
  let rec append = function
    | [] -> ()
    | Text { text; _ } :: rest ->
        Buffer.add_string buf text;
        append rest
    | Span { children; _ } :: rest ->
        append children;
        append rest
  in
  append t.fragments;
  Buffer.contents buf

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content
    ?text_style ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width
    ?truncate () =
  let props =
    Props.make ?content ?text_style ?wrap ?selectable ?selection_bg
      ?selection_fg ?tab_width ?truncate ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ~tab_width:props.tab_width ~truncate:props.truncate ()
  in
  let t =
    {
      text;
      props;
      fragments = [];
      flat_cache = Some [];
      has_styled_text = false;
    }
  in
  if props.content <> "" then Text_renderable.set_text text props.content;
  t

(* Content *)

let set_content t s =
  t.has_styled_text <- false;
  let frag = if s = "" then [] else [ Fragment.text s ] in
  t.fragments <- frag;
  t.flat_cache <-
    (if s = "" then Some [] else Some [ { text = s; style = None } ]);
  Text_renderable.set_text t.text s

let set_fragments t fragments =
  let normalized = normalize_fragments fragments in
  if fragments_equal t.fragments normalized then ()
  else begin
    t.has_styled_text <- true;
    t.fragments <- normalized;
    t.flat_cache <- None;
    rebuild_buffer t
  end

let fragment_of_span { text; style } =
  match style with
  | None -> Fragment.text text
  | Some style -> Fragment.text ~style text

let set_spans t new_spans =
  let fragments = List.map fragment_of_span new_spans in
  set_fragments t fragments;
  ignore (spans t)

let set_styled_text t buf_spans =
  t.has_styled_text <- true;
  t.fragments <- [];
  t.flat_cache <- None;
  Text_renderable.set_styled_text t.text buf_spans

let append_span t s =
  let updated = spans t @ [ s ] in
  set_spans t updated

let clear_spans t =
  t.fragments <- [];
  t.flat_cache <- Some [];
  t.has_styled_text <- false;
  Text_renderable.set_text t.text ""

let set_text_style t s =
  let current = Text_buffer.default_style (buffer t) in
  if Ansi.Style.equal current s then ()
  else begin
    Text_renderable.set_text_style t.text s;
    t.flat_cache <- None
  end

(* Wrapping *)

let set_wrap t mode = Text_renderable.set_wrap t.text mode
let set_tab_width t w = Text_renderable.set_tab_width t.text w

(* Selection *)

let set_selectable t v =
  if t.props.selectable <> v then begin
    t.props <- { t.props with selectable = v };
    Text_renderable.set_selectable t.text v
  end

let set_selection_bg t color = Text_renderable.set_selection_bg t.text color
let set_selection_fg t color = Text_renderable.set_selection_fg t.text color
let selected_text t = Text_renderable.selected_text t.text

(* Highlights *)

let add_highlight t h = Text_renderable.add_highlight t.text h

let remove_highlights_by_ref t ref_id =
  Text_renderable.remove_highlights_by_ref t.text ref_id

let clear_highlights t = Text_renderable.clear_highlights t.text

(* Query *)

let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text

(* Apply Props *)

let apply_props t (props : Props.t) =
  (* Text style — must be set before content so set_text picks up the new
     default_style when stamping spans. *)
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  (* Content — only update if not using styled text *)
  let content_changed =
    (not t.has_styled_text) && not (String.equal t.props.content props.content)
  in
  if content_changed then begin
    Text_renderable.set_text t.text props.content
  end;
  (* When the style changed but content didn't, re-stamp the existing content so
     the spans carry the updated default_style. *)
  if style_changed && (not content_changed) && not t.has_styled_text then begin
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
  if t.props.selectable <> props.selectable then begin
    t.props <- { t.props with selectable = props.selectable };
    Text_renderable.set_selectable t.text props.selectable
  end;
  t.props <- props

(* Pretty-printing *)

let pp ppf t =
  Format.fprintf ppf "Text(%s" (Renderable.id (node t));
  let content = Text_buffer.plain_text (buffer t) in
  if String.length content > 0 then begin
    let display =
      if String.length content > 20 then String.sub content 0 20 ^ "..."
      else content
    in
    Format.fprintf ppf ", %S" display
  end;
  if t.props.wrap <> `None then
    Format.fprintf ppf ", wrap=%s"
      (match t.props.wrap with
      | `Char -> "char"
      | `Word -> "word"
      | `None -> "none");
  Format.pp_print_char ppf ')'
