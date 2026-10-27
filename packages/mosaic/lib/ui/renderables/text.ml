(* Fragment type *)

type fragment =
  | Text of { text : string; style : Ansi.Style.t option }
  | Span of { style : Ansi.Style.t option; children : fragment list }

type span = { text : string; style : Ansi.Style.t option }

type content =
  | Plain of string
  | Fragments of fragment list
  | Styled of Text_buffer.span list

let plain_content text = if text = "" then Plain "" else Plain text

let rec fragments_equal a b =
  a == b
  ||
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

let content_equal a b =
  a == b
  ||
  match (a, b) with
  | Plain a, Plain b -> String.equal a b
  | Fragments a, Fragments b -> fragments_equal a b
  | Styled a, Styled b ->
      List.equal
        (fun (a : Text_buffer.span) (b : Text_buffer.span) ->
          String.equal a.text b.text && Ansi.Style.equal a.style b.style)
        a b
  | _ -> false

(* Props *)

module Props = struct
  type t =
    | Plain of {
        content : string;
        text_style : Ansi.Style.t;
        wrap : Text_surface.wrap;
        selectable : bool;
        selection_bg : Ansi.Color.t option;
        selection_fg : Ansi.Color.t option;
        tab_width : int;
        truncate : bool;
      }
    | Rich of {
        fragments : fragment list;
        text_style : Ansi.Style.t;
        wrap : Text_surface.wrap;
        selectable : bool;
        selection_bg : Ansi.Color.t option;
        selection_fg : Ansi.Color.t option;
        tab_width : int;
        truncate : bool;
      }

  let check_tab_width name w =
    if w < 1 then invalid_arg ("Text.Props." ^ name ^ ": tab width below 1")

  (* [make] and [make_rich] take the same labels of presentation, which get
     their defaults here alone; [k] makes the props of [x] with them. [k] is
     a function of the module, so passing it allocates nothing. *)
  let presentation name k x ?(text_style = Ansi.Style.default) ?(wrap = `None)
      ?(selectable = true) ?selection_bg ?selection_fg ?(tab_width = 2)
      ?(truncate = false) () =
    check_tab_width name tab_width;
    k x ~text_style ~wrap ~selectable ~selection_bg ~selection_fg ~tab_width
      ~truncate

  let plain content ~text_style ~wrap ~selectable ~selection_bg ~selection_fg
      ~tab_width ~truncate =
    Plain
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

  let rich fragments ~text_style ~wrap ~selectable ~selection_bg ~selection_fg
      ~tab_width ~truncate =
    Rich
      {
        fragments;
        text_style;
        wrap;
        selectable;
        selection_bg;
        selection_fg;
        tab_width;
        truncate;
      }

  let make ?(content = "") ?text_style ?wrap ?selectable ?selection_bg
      ?selection_fg ?tab_width ?truncate () =
    presentation "make" plain content ?text_style ?wrap ?selectable
      ?selection_bg ?selection_fg ?tab_width ?truncate ()

  let make_k k ?content ?text_style ?wrap ?selectable ?selection_bg
      ?selection_fg ?tab_width ?truncate () =
    k
      (make ?content ?text_style ?wrap ?selectable ?selection_bg ?selection_fg
         ?tab_width ?truncate ())

  let make_rich ?text_style ?wrap ?selectable ?selection_bg ?selection_fg
      ?tab_width ?truncate fragments =
    presentation "make_rich" rich fragments ?text_style ?wrap ?selectable
      ?selection_bg ?selection_fg ?tab_width ?truncate ()

  let default = make ()

  let text_style = function
    | Plain { text_style; _ } | Rich { text_style; _ } -> text_style

  let wrap = function Plain { wrap; _ } | Rich { wrap; _ } -> wrap

  let selectable = function
    | Plain { selectable; _ } | Rich { selectable; _ } -> selectable

  let selection_bg = function
    | Plain { selection_bg; _ } | Rich { selection_bg; _ } -> selection_bg

  let selection_fg = function
    | Plain { selection_fg; _ } | Rich { selection_fg; _ } -> selection_fg

  let tab_width = function
    | Plain { tab_width; _ } | Rich { tab_width; _ } -> tab_width

  let truncate = function
    | Plain { truncate; _ } | Rich { truncate; _ } -> truncate

  let content = function
    | Plain { content; _ } -> plain_content content
    | Rich { fragments; _ } -> Fragments fragments

  let equal a b =
    a == b
    || (match (a, b) with
         | Plain a, Plain b -> String.equal a.content b.content
         | Rich a, Rich b -> fragments_equal a.fragments b.fragments
         | _ -> false)
       && Ansi.Style.equal (text_style a) (text_style b)
       && wrap a = wrap b
       && selectable a = selectable b
       && Option.equal Ansi.Color.equal (selection_bg a) (selection_bg b)
       && Option.equal Ansi.Color.equal (selection_fg a) (selection_fg b)
       && tab_width a = tab_width b
       && truncate a = truncate b
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
  mutable content : content;
  mutable flat_cache : span list option;
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

let rec normalize_fragments fragments =
  let rec aux acc = function
    | [] -> List.rev acc
    | Text { text = ""; _ } :: rest -> aux acc rest
    | (Text { text; style } as first) :: rest ->
        let finish = function
          | None -> first :: acc
          | Some buffer -> Text { text = Buffer.contents buffer; style } :: acc
        in
        let rec run buffer = function
          | Text { text = ""; _ } :: rest -> run buffer rest
          | Text next :: rest
            when Option.equal Ansi.Style.equal style next.style ->
              let buffer =
                match buffer with
                | Some b ->
                    Buffer.add_string b next.text;
                    buffer
                | None ->
                    let b =
                      Buffer.create
                        (String.length text + String.length next.text)
                    in
                    Buffer.add_string b text;
                    Buffer.add_string b next.text;
                    Some b
              in
              run buffer rest
          | Span { style; children } :: rest ->
              let children = normalize_fragments children in
              if children = [] then run buffer rest
              else aux (Span { style; children } :: finish buffer) rest
          | rest -> aux (finish buffer) rest
        in
        run None rest
    | Span { style; children } :: rest ->
        let children = normalize_fragments children in
        aux
          (if children = [] then acc else Span { style; children } :: acc)
          rest
  in
  aux [] fragments

(* Fragment to buffer conversion *)

let rec collect_flat_spans acc current_style = function
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
            collect_flat_spans acc next_style children
      in
      collect_flat_spans acc current_style rest

let rebuild_buffer t =
  match t.content with
  | Plain text -> Text_renderable.set_text t.text text
  | Styled spans -> Text_renderable.set_styled_text t.text spans
  | Fragments fragments ->
      Text_renderable.set_styled_text t.text
        (List.rev
           (collect_flat_spans []
              (Text_buffer.default_style (buffer t))
              fragments))

let spans t =
  match t.flat_cache with
  | Some spans -> spans
  | None ->
      let spans =
        match t.content with
        | Plain "" -> []
        | _ ->
            List.map
              (fun (span : Text_buffer.span) ->
                let style =
                  match t.content with
                  | Styled _ -> Some span.style
                  | Plain _ | Fragments _ ->
                      if
                        Ansi.Style.equal span.style
                          (Text_buffer.default_style (buffer t))
                      then None
                      else Some span.style
                in
                { text = span.text; style })
              (Text_buffer.styled_text (buffer t))
      in
      t.flat_cache <- Some spans;
      spans

let fragments t =
  match t.content with
  | Plain "" -> []
  | Plain text -> [ Fragment.text text ]
  | Fragments fragments -> fragments
  | Styled spans ->
      List.map
        (fun (span : Text_buffer.span) ->
          Fragment.text ~style:span.style span.text)
        spans

let plain_text t = Text_buffer.plain_text (buffer t)

(* Content installation *)

let install_content t ~text_style content =
  let content =
    if content_equal t.content content then t.content
    else
      match content with
      | Fragments fragments -> Fragments (normalize_fragments fragments)
      | Plain _ | Styled _ -> content
  in
  let style_changed =
    not (Ansi.Style.equal (Text_buffer.default_style (buffer t)) text_style)
  in
  let content_changed = not (content_equal t.content content) in
  if style_changed then Text_renderable.set_text_style t.text text_style;
  t.content <- content;
  if
    content_changed
    || (style_changed && match content with Styled _ -> false | _ -> true)
  then begin
    t.flat_cache <- None;
    rebuild_buffer t
  end

let apply_props t (props : Props.t) =
  Text_renderable.set_wrap t.text (Props.wrap props);
  Text_renderable.set_tab_width t.text (Props.tab_width props);
  Text_renderable.set_truncate t.text (Props.truncate props);
  Text_renderable.set_selection_bg t.text (Props.selection_bg props);
  Text_renderable.set_selection_fg t.text (Props.selection_fg props);
  Text_renderable.set_selectable t.text (Props.selectable props);
  install_content t ~text_style:(Props.text_style props) (Props.content props)

(* Construction *)

let of_props ~parent ?index ?id ?style ?visible ?z_index ?opacity ?on_selection
    (props : Props.t) =
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:(Props.text_style props) ~wrap:(Props.wrap props)
      ~selectable:(Props.selectable props)
      ?selection_bg:(Props.selection_bg props)
      ?selection_fg:(Props.selection_fg props)
      ~tab_width:(Props.tab_width props) ~truncate:(Props.truncate props)
      ?on_selection ()
  in
  let t = { text; content = Props.content props; flat_cache = None } in
  if t.content <> Plain "" then rebuild_buffer t;
  t

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?on_selection =
  Props.make_k (fun props ->
      of_props ~parent ?index ?id ?style ?visible ?z_index ?opacity
        ?on_selection props)

(* Content *)

let set_content t text =
  install_content t
    ~text_style:(Text_buffer.default_style (buffer t))
    (plain_content text)

let set_fragments t fragments =
  install_content t
    ~text_style:(Text_buffer.default_style (buffer t))
    (Fragments fragments)

let set_spans t spans =
  set_fragments t
    (List.map (fun { text; style } -> Fragment.text ?style text) spans)

let set_styled_text t spans =
  install_content t
    ~text_style:(Text_buffer.default_style (buffer t))
    (Styled spans)

let append_span t { text; style } =
  match t.content with
  | Styled spans ->
      set_styled_text t
        (spans
        @ [
            Text_buffer.
              {
                text;
                style = merge_style (Text_buffer.default_style (buffer t)) style;
              };
          ])
  | Plain _ | Fragments _ ->
      set_fragments t (fragments t @ [ Fragment.text ?style text ])

let clear_spans t = set_content t ""
let set_text_style t text_style = install_content t ~text_style t.content

(* Wrapping *)

let set_wrap t wrap = Text_renderable.set_wrap t.text wrap
let set_tab_width t tab_width = Text_renderable.set_tab_width t.text tab_width

(* Selection *)

let set_selectable t selectable =
  Text_renderable.set_selectable t.text selectable

let set_selection_bg t color = Text_renderable.set_selection_bg t.text color
let set_selection_fg t color = Text_renderable.set_selection_fg t.text color
let set_on_selection t f = Text_renderable.set_on_selection t.text f
let selected_text t = Text_renderable.selected_text t.text

(* Highlights *)

let add_highlight t h = Text_renderable.add_highlight t.text h

let remove_highlights_by_ref t ref_id =
  Text_renderable.remove_highlights_by_ref t.text ref_id

let clear_highlights t = Text_renderable.clear_highlights t.text

(* Query *)

let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text

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
  if Text_surface.wrap (surface t) <> `None then
    Format.fprintf ppf ", wrap=%s"
      (match Text_surface.wrap (surface t) with
      | `Char -> "char"
      | `Word -> "word"
      | `None -> "none");
  Format.pp_print_char ppf ')'

module Private = struct
  let of_props = of_props
end
