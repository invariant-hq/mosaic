(* ───── Style ───── *)

type style_key =
  | Default
  | Heading of int
  | Emphasis
  | Strong
  | Code_span
  | Code_block
  | Link
  | Image
  | Blockquote
  | Thematic_break
  | List_marker
  | Strikethrough
  | Task_marker
  | Table_border
  | Conceal_punctuation

module Style = struct
  module Keys = Map.Make (struct
    type t = style_key

    let compare = Stdlib.compare
  end)

  type t = Ansi.Style.t Keys.t

  let check = function
    | Heading n when n < 1 || n > 6 ->
        invalid_arg
          (Printf.sprintf "Markdown.Style: heading level %d is not in 1..6" n)
    | _ -> ()

  let bind style bindings =
    List.fold_left
      (fun style (key, s) ->
        check key;
        Keys.add key s style)
      style bindings

  let default =
    let heading r g b =
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb r g b) ~bold:true ()
    in
    bind Keys.empty
      [
        (Default, Ansi.Style.default);
        (Heading 1, heading 0 215 175);
        (Heading 2, heading 95 175 255);
        (Heading 3, heading 200 140 255);
        (Heading 4, heading 135 175 255);
        (Heading 5, heading 135 175 255);
        (Heading 6, heading 135 175 255);
        (Emphasis, Ansi.Style.make ~italic:true ());
        (Strong, Ansi.Style.make ~bold:true ());
        (Code_span, Ansi.Style.make ~fg:(Ansi.Color.of_rgb 180 200 220) ());
        (Code_block, Ansi.Style.make ~fg:(Ansi.Color.of_rgb 180 200 220) ());
        ( Link,
          Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 175 255) ~underline:true ()
        );
        ( Image,
          Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 175 255) ~underline:true ()
        );
        ( Blockquote,
          Ansi.Style.make ~fg:(Ansi.Color.of_rgb 140 150 165) ~italic:true () );
        (Thematic_break, Ansi.Style.make ~fg:(Ansi.Color.of_rgb 80 90 100) ());
        (List_marker, Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 135 215) ());
        ( Strikethrough,
          Ansi.Style.make
            ~fg:(Ansi.Color.of_rgb 140 150 165)
            ~strikethrough:true () );
        (Task_marker, heading 0 215 175);
        (Table_border, Ansi.Style.make ~fg:(Ansi.Color.of_rgb 80 90 100) ());
        ( Conceal_punctuation,
          Ansi.Style.make ~fg:(Ansi.Color.of_rgb 70 80 90) () );
      ]

  let make ?(base = default) bindings = bind base bindings

  (* Each key by its role: text in [Plain], headings in [Emphasis], the
     first underlined too, code spans in [Accent], links and images in
     [Link], quotes, list markers, struck text and concealed punctuation in
     [Muted], rules and table lines in [Border], task markers in [Ok];
     emphasis, strength and striking are their attributes. *)
  let of_theme theme =
    let role = Theme.style theme in
    let with_ role attrs =
      Ansi.Style.merge ~base:(Theme.style theme role) ~overlay:attrs
    in
    let emphasis = role Emphasis in
    bind Keys.empty
      [
        (Default, role Plain);
        (Heading 1, with_ Emphasis (Ansi.Style.make ~underline:true ()));
        (Heading 2, emphasis);
        (Heading 3, emphasis);
        (Heading 4, emphasis);
        (Heading 5, emphasis);
        (Heading 6, emphasis);
        (Emphasis, with_ Plain (Ansi.Style.make ~italic:true ()));
        (Strong, with_ Plain (Ansi.Style.make ~bold:true ()));
        (Code_span, role Accent);
        (Code_block, role Plain);
        (Link, role Link);
        (Image, role Link);
        (Blockquote, with_ Muted (Ansi.Style.make ~italic:true ()));
        (Thematic_break, role Border);
        (List_marker, role Muted);
        (Strikethrough, with_ Muted (Ansi.Style.make ~strikethrough:true ()));
        (Task_marker, role Ok);
        (Table_border, role Border);
        (Conceal_punctuation, role Muted);
      ]

  let get t key =
    check key;
    Keys.find key t

  let equal = Keys.equal Ansi.Style.equal
end

(* ───── Props ───── *)

type render_node =
  Cmarkit.Block.t -> parent:Renderable.t -> is_last:bool -> Renderable.t option

type render_code =
  parent:Renderable.t ->
  language:string option ->
  content:string ->
  Renderable.t

(* [Props.make] is the one place the markdown's labels get their defaults:
   [create] and [Mosaic.markdown] pass their labels on unset. *)
module Props = struct
  type t = {
    content : string;
    conceal : bool;
    streaming : bool;
    style : Style.t option;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    render_node : render_node option;
    render_code : render_code option;
    highlighter : Code.Highlighter.t option;
    syntax_style : Syntax_style.t option;
  }

  let make ?(content = "") ?(conceal = true) ?(streaming = false) ?style
      ?(selectable = true) ?selection_bg ?selection_fg ?render_node ?render_code
      ?highlighter ?syntax_style () =
    {
      content;
      conceal;
      streaming;
      style;
      selectable;
      selection_bg;
      selection_fg;
      render_node;
      render_code;
      highlighter;
      syntax_style;
    }

  let make_k k ?content ?conceal ?streaming ?style ?selectable ?selection_bg
      ?selection_fg ?render_node ?render_code ?highlighter ?syntax_style () =
    k
      (make ?content ?conceal ?streaming ?style ?selectable ?selection_bg
         ?selection_fg ?render_node ?render_code ?highlighter ?syntax_style ())

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && a.conceal = b.conceal && a.streaming = b.streaming
    && Option.equal Style.equal a.style b.style
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && Option.equal ( == ) a.render_node b.render_node
    && Option.equal ( == ) a.render_code b.render_code
    && Option.equal ( == ) a.highlighter b.highlighter
    && Option.equal Syntax_style.equal a.syntax_style b.syntax_style
end

(* ───── Hooks ───── *)

(* ───── Widget Types ───── *)

type block_tag =
  | Para_tag
  | Head_tag
  | Code_tag
  | Quote_tag
  | List_tag
  | Hr_tag
  | Table_tag
  | Html_tag
  | Custom_tag

type widget =
  | Text_widget of Text.t
  | Box_widget of Box.t * widget list
  | Container_widget of Renderable.t * widget list
  | Code_widget of Code.t

type tagged_block = {
  mutable tag : block_tag;
  mutable widget : widget;
  mutable ast : Cmarkit.Block.t;
      (* The block this widget was rendered from. Its textloc identifies the
         source bytes so unchanged prefix blocks can be reused verbatim on
         streaming appends. *)
}

(* Placeholder for tagged blocks not backed by a parsed block (the raw-text
   fallback). Compared physically to skip them in re-render passes. *)
let empty_ast : Cmarkit.Block.t = Cmarkit.Block.empty

(* ───── Render Environment ───── *)

type render_env = {
  style : style_key -> Ansi.Style.t;
  conceal : bool;
  defs : Cmarkit.Label.defs;
  streaming : bool;
  selectable : bool;
  selection_bg : Ansi.Color.t option;
  selection_fg : Ansi.Color.t option;
  on_text_selection : ((int * int) option -> unit) option;
  render_node : render_node option;
  render_code : render_code option;
  highlighter : Code.Highlighter.t option;
  syntax_style : Syntax_style.t option;
}

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable content : string;
  mutable style : Style.t option;
  mutable conceal : bool;
  mutable streaming : bool;
  mutable selectable : bool;
  mutable selection_bg : Ansi.Color.t option;
  mutable selection_fg : Ansi.Color.t option;
  mutable on_selection : (string option -> unit) option;
  mutable blocks : tagged_block list;
  mutable last_defs : Cmarkit.Label.defs;
  mutable render_node : render_node option;
  mutable render_code : render_code option;
  mutable highlighter : Code.Highlighter.t option;
  mutable syntax_style : Syntax_style.t option;
  mutable themed : (Theme.t * Style.t) option;
      (* The theme in scope the element styles were last derived from, and
         the styles, for a markdown given no [style]. *)
}

let node t = t.node
let content t = t.content

let rec selected_text_of_widget acc = function
  | Text_widget text ->
      let selected = String.trim (Text.selected_text text) in
      if String.equal selected "" then acc else selected :: acc
  | Code_widget _ -> acc
  | Box_widget (_, children) | Container_widget (_, children) ->
      List.fold_left selected_text_of_widget acc children

let selected_text t =
  t.blocks
  |> List.fold_left
       (fun acc block -> selected_text_of_widget acc block.widget)
       []
  |> List.rev |> String.concat "\n"

let fire_on_selection t =
  match t.on_selection with
  | None -> ()
  | Some f ->
      let text = String.trim (selected_text t) in
      f (if String.equal text "" then None else Some text)

let configure_text (env : render_env) text =
  Text.set_selectable text env.selectable;
  Text.set_selection_bg text env.selection_bg;
  Text.set_selection_fg text env.selection_fg;
  Text.set_on_selection text env.on_text_selection

let create_text ~(env : render_env) ~parent ?index ?id ?style ?visible ?z_index
    ?opacity ?content ?text_style ?(wrap = `None) ?tab_width ?truncate () =
  Text.Private.of_props ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ?on_selection:env.on_text_selection
    (Text.Props.make ?content ?text_style ~wrap ~selectable:env.selectable
       ?selection_bg:env.selection_bg ?selection_fg:env.selection_fg ?tab_width
       ?truncate ())

(* ───── Inline Rendering ───── *)

let link_dest (defs : Cmarkit.Label.defs) (link : Cmarkit.Inline.Link.t) =
  match Cmarkit.Inline.Link.reference link with
  | `Inline (ld, _meta) -> (
      match Cmarkit.Link_definition.dest ld with
      | Some (url, _) -> Some url
      | None -> None)
  | `Ref (_layout, _label, def_label) -> (
      match Cmarkit.Label.Map.find_opt (Cmarkit.Label.key def_label) defs with
      | Some (Cmarkit.Link_definition.Def (ld, _meta)) -> (
          match Cmarkit.Link_definition.dest ld with
          | Some (url, _) -> Some url
          | None -> None)
      | _ -> None)

let conceal_punct ~(style : style_key -> Ansi.Style.t) text =
  { Text_buffer.text; style = style Conceal_punctuation }

let rec inline_to_spans ~(style : style_key -> Ansi.Style.t) ~conceal
    ~(base : Ansi.Style.t) ~(defs : Cmarkit.Label.defs)
    (inline : Cmarkit.Inline.t) =
  match inline with
  | Cmarkit.Inline.Text (s, _meta) -> [ { Text_buffer.text = s; style = base } ]
  | Cmarkit.Inline.Emphasis (em, _meta) ->
      let child_base = Ansi.Style.merge ~base ~overlay:(style Emphasis) in
      let inner = Cmarkit.Inline.Emphasis.inline em in
      if conceal then
        inline_to_spans ~style ~conceal ~base:child_base ~defs inner
      else
        let delim = String.make 1 (Cmarkit.Inline.Emphasis.delim em) in
        let punct = conceal_punct ~style delim in
        (punct :: inline_to_spans ~style ~conceal ~base:child_base ~defs inner)
        @ [ punct ]
  | Cmarkit.Inline.Strong_emphasis (em, _meta) ->
      let child_base = Ansi.Style.merge ~base ~overlay:(style Strong) in
      let inner = Cmarkit.Inline.Emphasis.inline em in
      if conceal then
        inline_to_spans ~style ~conceal ~base:child_base ~defs inner
      else
        let delim = String.make 2 (Cmarkit.Inline.Emphasis.delim em) in
        let punct = conceal_punct ~style delim in
        (punct :: inline_to_spans ~style ~conceal ~base:child_base ~defs inner)
        @ [ punct ]
  | Cmarkit.Inline.Code_span (cs, _meta) ->
      let code = Cmarkit.Inline.Code_span.code cs in
      let s = Ansi.Style.merge ~base ~overlay:(style Code_span) in
      if conceal then [ { Text_buffer.text = code; style = s } ]
      else
        let tick = conceal_punct ~style "`" in
        [ tick; { text = code; style = s }; tick ]
  | Cmarkit.Inline.Link (link, _meta) ->
      let link_style = Ansi.Style.merge ~base ~overlay:(style Link) in
      let url = link_dest defs link in
      let link_style =
        match url with
        | Some url -> Ansi.Style.hyperlink url link_style
        | None -> link_style
      in
      let text_spans =
        inline_to_spans ~style ~conceal ~base:link_style ~defs
          (Cmarkit.Inline.Link.text link)
      in
      if conceal then
        (* Conceal mode: show link text followed by (url) *)
        match url with
        | Some url ->
            text_spans
            @ [
                conceal_punct ~style " (";
                { Text_buffer.text = url; style = link_style };
                conceal_punct ~style ")";
              ]
        | None -> text_spans
      else
        let close_and_url =
          match url with
          | Some url ->
              [
                conceal_punct ~style "](";
                { Text_buffer.text = url; style = link_style };
                conceal_punct ~style ")";
              ]
          | None -> [ conceal_punct ~style "]" ]
        in
        (conceal_punct ~style "[" :: text_spans) @ close_and_url
  | Cmarkit.Inline.Image (link, _meta) ->
      let img_style = Ansi.Style.merge ~base ~overlay:(style Image) in
      let url = link_dest defs link in
      let img_style =
        match url with
        | Some url -> Ansi.Style.hyperlink url img_style
        | None -> img_style
      in
      let text_spans =
        inline_to_spans ~style ~conceal ~base:img_style ~defs
          (Cmarkit.Inline.Link.text link)
      in
      if conceal then text_spans
      else
        let suffix =
          match url with
          | Some url ->
              [
                conceal_punct ~style "](";
                { text = url; style = img_style };
                conceal_punct ~style ")";
              ]
          | None -> [ conceal_punct ~style "]" ]
        in
        (conceal_punct ~style "![" :: text_spans) @ suffix
  | Cmarkit.Inline.Autolink (al, _meta) ->
      let url, _ = Cmarkit.Inline.Autolink.link al in
      let link_style =
        Ansi.Style.merge ~base ~overlay:(style Link) |> Ansi.Style.hyperlink url
      in
      if conceal then [ { Text_buffer.text = url; style = link_style } ]
      else
        [
          conceal_punct ~style "<";
          { text = url; style = link_style };
          conceal_punct ~style ">";
        ]
  | Cmarkit.Inline.Break (br, _meta) -> (
      match Cmarkit.Inline.Break.type' br with
      | `Hard -> [ { Text_buffer.text = "\n"; style = base } ]
      | `Soft -> [ { Text_buffer.text = " "; style = base } ])
  | Cmarkit.Inline.Raw_html (lines, _meta) ->
      let text =
        String.concat "" (List.map Cmarkit.Block_line.tight_to_string lines)
      in
      [ { Text_buffer.text; style = base } ]
  | Cmarkit.Inline.Inlines (inlines, _meta) ->
      List.concat_map (inline_to_spans ~style ~conceal ~base ~defs) inlines
  | Cmarkit.Inline.Ext_strikethrough (st, _meta) ->
      let child_base = Ansi.Style.merge ~base ~overlay:(style Strikethrough) in
      let inner = Cmarkit.Inline.Strikethrough.inline st in
      if conceal then
        inline_to_spans ~style ~conceal ~base:child_base ~defs inner
      else
        let tilde = conceal_punct ~style "~~" in
        (tilde :: inline_to_spans ~style ~conceal ~base:child_base ~defs inner)
        @ [ tilde ]
  | _ -> []

(* ───── Layout Helpers ───── *)

let margin_bottom n =
  let open Toffee.Style in
  let m = Length_percentage_auto.length (Float.of_int n) in
  {
    (Toffee.Geometry.Rect.all Length_percentage_auto.zero) with
    Toffee.Geometry.Rect.bottom = m;
  }

let full_width = Toffee.Style.Dimension.pct 100.

let column_style =
  Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
    ~size:(Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
    ()

(* Border style for non-first table columns: T-joins connect with previous
   column *)
let inner_column_border =
  Matrix_grid.Border.modify
    ~top_left:(Uchar.of_int 0x252C) (* ┬ *)
    ~bottom_left:(Uchar.of_int 0x2534) (* ┴ *)
    Matrix_grid.Border.single

let block_style ~is_last =
  let margin = if is_last then margin_bottom 0 else margin_bottom 1 in
  Toffee.Style.make
    ~size:(Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
    ~margin ()

let bordered_block_style ~is_last =
  block_style ~is_last
  |> Toffee.Style.set_padding_left (Toffee.Style.Length_percentage.length 1.)

let bordered_column_block_style ~is_last =
  bordered_block_style ~is_last
  |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column

(* ───── Code Block Helpers ───── *)

let code_block_language (cb : Cmarkit.Block.Code_block.t) =
  match Cmarkit.Block.Code_block.info_string cb with
  | Some (info, _) -> (
      match Cmarkit.Block.Code_block.language_of_info_string info with
      | Some (lang, _) -> Some lang
      | None -> if info <> "" then Some info else None)
  | None -> None

let code_block_text (cb : Cmarkit.Block.Code_block.t) =
  String.concat "\n"
    (List.map Cmarkit.Block_line.to_string (Cmarkit.Block.Code_block.code cb))

(* ───── Block Rendering ───── *)

let render_paragraph ~(env : render_env) ~parent ?index ~is_last
    (p : Cmarkit.Block.Paragraph.t) =
  let inline = Cmarkit.Block.Paragraph.inline p in
  let base = env.style Default in
  let spans =
    inline_to_spans ~style:env.style ~conceal:env.conceal ~base ~defs:env.defs
      inline
  in
  let text =
    create_text ~env ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Para_tag; widget = Text_widget text; ast = empty_ast }

let render_heading ~(env : render_env) ~parent ?index ~is_last
    (h : Cmarkit.Block.Heading.t) =
  let level = Cmarkit.Block.Heading.level h in
  let inline = Cmarkit.Block.Heading.inline h in
  let heading_style = env.style (Heading level) in
  let base =
    Ansi.Style.merge ~base:(env.style Default) ~overlay:heading_style
  in
  let spans =
    inline_to_spans ~style:env.style ~conceal:env.conceal ~base ~defs:env.defs
      inline
  in
  let spans =
    if env.conceal then spans
    else
      let marker = String.make level '#' ^ " " in
      { Text_buffer.text = marker; style = heading_style } :: spans
  in
  let text =
    create_text ~env ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Head_tag; widget = Text_widget text; ast = empty_ast }

let render_code_block_default ~(env : render_env) ~parent ?index ~is_last
    (cb : Cmarkit.Block.Code_block.t) =
  let code_text = code_block_text cb in
  let title = code_block_language cb in
  let code_style = env.style Code_block in
  let box =
    Box.Private.of_props ~parent ?index
      ~style:(bordered_block_style ~is_last)
      (Box.Props.make ~border:true ~border_sides:[ `Left ] ?title ())
  in
  let text =
    create_text ~env ~parent:(Box.node box) ~content:code_text
      ~text_style:code_style
      ~style:
        (Toffee.Style.make
           ~size:
             (Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
           ())
      ()
  in
  {
    tag = Code_tag;
    widget = Box_widget (box, [ Text_widget text ]);
    ast = empty_ast;
  }

(* A fence names its language in its info string; a fence without one is
   shown plain. *)
let fence_syntax ~(env : render_env) highlighter cb =
  Option.map
    (fun language ->
      Code.with_highlighter ~language ?style:env.syntax_style highlighter)
    (code_block_language cb)

(* Render a fenced code block as a borderless {!Code} view highlighted by
   [highlighter]. *)
let render_code_highlighted ~env ~parent ?index ~is_last highlighter
    (cb : Cmarkit.Block.Code_block.t) =
  let content = code_block_text cb in
  let syntax = fence_syntax ~env highlighter cb in
  let code =
    Code.Private.of_props ~parent ?index ~style:(block_style ~is_last)
      (Code.Props.make ~content ?syntax ())
  in
  { tag = Code_tag; widget = Code_widget code; ast = empty_ast }

let render_thematic_break ~(env : render_env) ~parent ?index ~is_last
    (_tb : Cmarkit.Block.Thematic_break.t) =
  let hr_style = env.style Thematic_break in
  let text =
    create_text ~env ~parent ?index
      ~content:"───────────────────────────────────────" ~text_style:hr_style
      ~style:(block_style ~is_last) ~truncate:true ()
  in
  { tag = Hr_tag; widget = Text_widget text; ast = empty_ast }

let render_html_block ~(env : render_env) ~parent ?index ~is_last
    (lines : Cmarkit.Block.Html_block.t) =
  let text_content =
    String.concat "\n" (List.map Cmarkit.Block_line.to_string lines)
  in
  let text =
    create_text ~env ~parent ?index ~content:text_content
      ~text_style:(env.style Default) ~style:(block_style ~is_last) ()
  in
  { tag = Html_tag; widget = Text_widget text; ast = empty_ast }

let rec render_list ~(env : render_env) ~parent ?index ~is_last
    (lst : Cmarkit.Block.List'.t) =
  let container =
    Renderable.create ~parent ?index
      ~style:
        (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
           ~size:
             (Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
           ~margin:(if is_last then margin_bottom 0 else margin_bottom 1)
           ())
      ()
  in
  let items = Cmarkit.Block.List'.items lst in
  let tight = Cmarkit.Block.List'.tight lst in
  let list_type = Cmarkit.Block.List'.type' lst in
  let item_count = List.length items in
  let item_widgets =
    List.mapi
      (fun i (item, _meta) ->
        let is_last_item = i = item_count - 1 in
        render_list_item ~env ~parent:container ~is_last:is_last_item ~tight
          ~list_type ~index:i item)
      items
  in
  {
    tag = List_tag;
    widget = Container_widget (container, item_widgets);
    ast = empty_ast;
  }

and render_list_item ~(env : render_env) ~parent ~is_last ~tight ~list_type
    ~index (item : Cmarkit.Block.List_item.t) =
  let item_margin =
    if is_last || tight then margin_bottom 0 else margin_bottom 1
  in
  let row =
    Renderable.create ~parent
      ~style:
        (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Row
           ~size:
             (Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
           ~margin:item_margin ())
      ()
  in
  let task_prefix =
    match Cmarkit.Block.List_item.ext_task_marker item with
    | Some (uchar, _meta) -> (
        match Cmarkit.Block.List_item.task_status_of_task_marker uchar with
        | `Checked -> Some "[x] "
        | `Unchecked -> Some "[ ] "
        | `Cancelled -> Some "[~] "
        | `Other _ -> Some "[?] ")
    | None -> None
  in
  let marker_text =
    match list_type with
    | `Unordered _ -> "• "
    | `Ordered (start, _) ->
        let n = start + index in
        string_of_int n ^ ". "
  in
  let _marker =
    create_text ~env ~parent:row ~content:marker_text
      ~text_style:(env.style List_marker)
      ~style:
        (Toffee.Style.make
           ~size:
             (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                Toffee.Style.Dimension.auto)
           ~flex_shrink:0. ())
      ()
  in
  let item_content =
    Renderable.create ~parent:row
      ~style:
        (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
           ~flex_grow:1.
           ~size:
             (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                Toffee.Style.Dimension.auto)
           ())
      ()
  in
  let block = Cmarkit.Block.List_item.block item in
  let children = render_block_children ~tight ~env ~parent:item_content block in
  (match (task_prefix, children) with
  | Some prefix, { widget = Text_widget text; _ } :: _ ->
      let buf = Text.buffer text in
      let existing_spans = Text_buffer.styled_text buf in
      let task_span =
        { Text_buffer.text = prefix; style = env.style Task_marker }
      in
      Text.set_styled_text text (task_span :: existing_spans)
  | _ -> ());
  Container_widget (row, List.map (fun tb -> tb.widget) children)

and render_blockquote ~(env : render_env) ~parent ?index ~is_last
    (bq : Cmarkit.Block.Block_quote.t) =
  let bq_style = env.style Blockquote in
  let box =
    Box.Private.of_props ~parent ?index
      ~style:(bordered_column_block_style ~is_last)
      (Box.Props.make ~border:true ~border_sides:[ `Left ]
         ?border_color:bq_style.fg ())
  in
  let inner_block = Cmarkit.Block.Block_quote.block bq in
  let children =
    render_block_children ~env ~parent:(Box.node box) inner_block
  in
  {
    tag = Quote_tag;
    widget = Box_widget (box, List.map (fun tb -> tb.widget) children);
    ast = empty_ast;
  }

and render_table ~(env : render_env) ~parent ?index ~is_last
    (table : Cmarkit.Block.Table.t) =
  let all_rows = Cmarkit.Block.Table.rows table in
  (* Streaming: skip the last row which may be incomplete *)
  let rows =
    if env.streaming && all_rows <> [] then
      match List.rev all_rows with _ :: rest -> List.rev rest | [] -> []
    else all_rows
  in
  (* Classify rows into header, separator, and data *)
  let header_cells = ref None in
  let data_rows = ref [] in
  List.iter
    (fun ((row, _meta), _blanks) ->
      match row with
      | `Sep _ -> ()
      | `Header cells -> header_cells := Some cells
      | `Data cells -> data_rows := cells :: !data_rows)
    rows;
  let data_rows = List.rev !data_rows in
  let col_count = Cmarkit.Block.Table.col_count table in
  (* Fallback: no columns, or no header and no data rows *)
  if col_count = 0 || (Option.is_none !header_cells && data_rows = []) then begin
    let text =
      create_text ~env ~parent ?index ~content:"" ~style:(block_style ~is_last)
        ()
    in
    { tag = Table_tag; widget = Text_widget text; ast = empty_ast }
  end
  else
    let border_style = env.style Table_border in
    let border_color = border_style.fg in
    let cell_text_style =
      Toffee.Style.make
        ~size:
          (Toffee.Geometry.Size.make full_width
             (Toffee.Style.Dimension.length 1.))
        ~overflow:
          (Toffee.Geometry.Point.make Toffee.Style.Overflow.Hidden
             Toffee.Style.Overflow.Hidden)
        ()
    in
    let table_box =
      Renderable.create ~parent ?index
        ~style:
          (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Row
             ~size:
               (Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
             ~margin:(if is_last then margin_bottom 0 else margin_bottom 1)
             ())
        ()
    in
    let col_widgets =
      List.init col_count (fun col_idx ->
          let is_first_col = col_idx = 0 in
          let is_last_col = col_idx = col_count - 1 in
          let border_chars =
            if is_first_col then Matrix_grid.Border.single
            else inner_column_border
          in
          let border_sides =
            if is_last_col then Matrix_grid.Border.all
            else [ `Top; `Bottom; `Left ]
          in
          let col_box =
            Box.Private.of_props ~parent:table_box
              ~style:
                (Toffee.Style.make
                   ~flex_direction:Toffee.Style.Flex_direction.Column
                   ~flex_grow:1.
                   ~size:
                     (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                        Toffee.Style.Dimension.auto)
                   ())
              (Box.Props.make ~border:true ~border_style:border_chars
                 ~border_sides ?border_color ())
          in
          let get_cell cells =
            if col_idx < List.length cells then
              Some (fst (List.nth cells col_idx))
            else None
          in
          (* Header cell *)
          let header_widget =
            match !header_cells with
            | Some cells ->
                let base =
                  Ansi.Style.merge ~base:(env.style Default)
                    ~overlay:(Ansi.Style.make ~bold:true ())
                in
                let spans =
                  match get_cell cells with
                  | Some inline ->
                      inline_to_spans ~style:env.style ~conceal:env.conceal
                        ~base ~defs:env.defs inline
                  | None -> []
                in
                let header_box =
                  Box.Private.of_props ~parent:(Box.node col_box)
                    (Box.Props.make ~border:true ~border_sides:[ `Bottom ]
                       ?border_color ())
                in
                let text =
                  create_text ~env ~parent:(Box.node header_box)
                    ~style:cell_text_style ~truncate:true ()
                in
                Text.set_styled_text text spans;
                [ Box_widget (header_box, [ Text_widget text ]) ]
            | None -> []
          in
          (* Data rows *)
          let data_count = List.length data_rows in
          let row_widgets =
            List.mapi
              (fun row_idx cells ->
                let base = env.style Default in
                let spans =
                  match get_cell cells with
                  | Some inline ->
                      inline_to_spans ~style:env.style ~conceal:env.conceal
                        ~base ~defs:env.defs inline
                  | None -> []
                in
                let is_last_row = row_idx = data_count - 1 in
                if is_last_row then begin
                  let text =
                    create_text ~env ~parent:(Box.node col_box)
                      ~style:cell_text_style ~truncate:true ()
                  in
                  Text.set_styled_text text spans;
                  Text_widget text
                end
                else begin
                  let cell_box =
                    Box.Private.of_props ~parent:(Box.node col_box)
                      (Box.Props.make ~border:true ~border_sides:[ `Bottom ]
                         ?border_color ())
                  in
                  let text =
                    create_text ~env ~parent:(Box.node cell_box)
                      ~style:cell_text_style ~truncate:true ()
                  in
                  Text.set_styled_text text spans;
                  Box_widget (cell_box, [ Text_widget text ])
                end)
              data_rows
          in
          Box_widget (col_box, header_widget @ row_widgets))
    in
    {
      tag = Table_tag;
      widget = Container_widget (table_box, col_widgets);
      ast = empty_ast;
    }

(* Dispatch a single block to the appropriate renderer, checking hooks first *)
and render_block ~(env : render_env) ~parent ?index ~is_last
    (block : Cmarkit.Block.t) : tagged_block option =
  let tb = render_block_dispatch ~env ~parent ?index ~is_last block in
  Option.iter (fun tb -> tb.ast <- block) tb;
  tb

and render_block_dispatch ~(env : render_env) ~parent ?index ~is_last
    (block : Cmarkit.Block.t) : tagged_block option =
  (* 1. Full code block override *)
  match (env.render_code, block) with
  | Some render_code, Cmarkit.Block.Code_block (cb, _meta) ->
      let language = code_block_language cb in
      let content = code_block_text cb in
      let custom_node = render_code ~parent ~language ~content in
      Some
        {
          tag = Code_tag;
          widget = Container_widget (custom_node, []);
          ast = empty_ast;
        }
  | _ -> (
      (* 2. Highlighted code block *)
      match (env.highlighter, block) with
      | Some highlighter, Cmarkit.Block.Code_block (cb, _meta) ->
          Some
            (render_code_highlighted ~env ~parent ?index ~is_last highlighter cb)
      | _ ->
          (* 3. General render hook *)
          general_render ~env ~parent ?index ~is_last block)

and general_render ~(env : render_env) ~parent ?index ~is_last block =
  let custom =
    match env.render_node with
    | Some hook -> (
        match hook block ~parent ~is_last with
        | Some custom_node ->
            Some
              {
                tag = Custom_tag;
                widget = Container_widget (custom_node, []);
                ast = empty_ast;
              }
        | None -> None)
    | None -> None
  in
  match custom with
  | Some tb -> Some tb
  | None ->
      (* Default rendering *)
      render_block_default ~env ~parent ?index ~is_last block

and render_block_default ~(env : render_env) ~parent ?index ~is_last
    (block : Cmarkit.Block.t) : tagged_block option =
  match block with
  | Cmarkit.Block.Paragraph (p, _meta) ->
      Some (render_paragraph ~env ~parent ?index ~is_last p)
  | Cmarkit.Block.Heading (h, _meta) ->
      Some (render_heading ~env ~parent ?index ~is_last h)
  | Cmarkit.Block.Code_block (cb, _meta) ->
      Some (render_code_block_default ~env ~parent ?index ~is_last cb)
  | Cmarkit.Block.Block_quote (bq, _meta) ->
      Some (render_blockquote ~env ~parent ?index ~is_last bq)
  | Cmarkit.Block.List (lst, _meta) ->
      Some (render_list ~env ~parent ?index ~is_last lst)
  | Cmarkit.Block.Thematic_break (tb, _meta) ->
      Some (render_thematic_break ~env ~parent ?index ~is_last tb)
  | Cmarkit.Block.Html_block (lines, _meta) ->
      Some (render_html_block ~env ~parent ?index ~is_last lines)
  | Cmarkit.Block.Ext_table (table, _meta) ->
      Some (render_table ~env ~parent ?index ~is_last table)
  | Cmarkit.Block.Blocks (blocks, _meta) ->
      let block_list = render_block_list ~env ~parent blocks in
      if block_list = [] then None
      else
        let container =
          Renderable.create ~parent ?index ~style:(block_style ~is_last) ()
        in
        let children = render_block_list ~env ~parent:container blocks in
        Some
          {
            tag = Custom_tag;
            widget =
              Container_widget
                (container, List.map (fun tb -> tb.widget) children);
            ast = empty_ast;
          }
  | Cmarkit.Block.Blank_line _ | Cmarkit.Block.Link_reference_definition _
  | Cmarkit.Block.Ext_footnote_definition _ ->
      None
  | _ -> None

and render_block_list ?(tight = false) ~(env : render_env) ~parent blocks =
  let blocks =
    List.filter
      (fun b ->
        match b with
        | Cmarkit.Block.Blank_line _ | Cmarkit.Block.Link_reference_definition _
        | Cmarkit.Block.Ext_footnote_definition _ ->
            false
        | _ -> true)
      blocks
  in
  let count = List.length blocks in
  List.concat_map
    (fun (i, block) ->
      let is_last = i = count - 1 || tight in
      match render_block ~env ~parent ~is_last block with
      | Some tb -> [ tb ]
      | None -> [])
    (List.mapi (fun i b -> (i, b)) blocks)

and render_block_children ?(tight = false) ~(env : render_env) ~parent block =
  match block with
  | Cmarkit.Block.Blocks (blocks, _meta) ->
      render_block_list ~tight ~env ~parent blocks
  | other -> render_block_list ~tight ~env ~parent [ other ]

(* ───── In-place Update ───── *)

let update_leaf_in_place ~(env : render_env) ~is_last (tb : tagged_block)
    (block : Cmarkit.Block.t) : bool =
  match (tb.tag, block, tb.widget) with
  | Para_tag, Cmarkit.Block.Paragraph (p, _), Text_widget text ->
      let inline = Cmarkit.Block.Paragraph.inline p in
      let base = env.style Default in
      let spans =
        inline_to_spans ~style:env.style ~conceal:env.conceal ~base
          ~defs:env.defs inline
      in
      Text.set_styled_text text spans;
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | Head_tag, Cmarkit.Block.Heading (h, _), Text_widget text ->
      let level = Cmarkit.Block.Heading.level h in
      let heading_style = env.style (Heading level) in
      let base =
        Ansi.Style.merge ~base:(env.style Default) ~overlay:heading_style
      in
      let inline = Cmarkit.Block.Heading.inline h in
      let spans =
        inline_to_spans ~style:env.style ~conceal:env.conceal ~base
          ~defs:env.defs inline
      in
      let spans =
        if env.conceal then spans
        else
          let marker = String.make level '#' ^ " " in
          { Text_buffer.text = marker; style = heading_style } :: spans
      in
      Text.set_styled_text text spans;
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | ( Code_tag,
      Cmarkit.Block.Code_block (cb, _),
      Box_widget (box, [ Text_widget text ]) )
    when Option.is_none env.render_code && Option.is_none env.highlighter ->
      let code_text = code_block_text cb in
      let title = code_block_language cb in
      Text.set_content text code_text;
      Text.set_text_style text (env.style Code_block);
      Box.set_title box title;
      Renderable.set_style (Box.node box) (block_style ~is_last);
      true
  | Code_tag, Cmarkit.Block.Code_block (cb, _), Code_widget code
    when Option.is_none env.render_code -> (
      match env.highlighter with
      | Some highlighter ->
          let content = code_block_text cb in
          let syntax = fence_syntax ~env highlighter cb in
          (* Code.apply_props reconciles content and syntax in place, so the
             widget keeps styling the unchanged bytes of a growing fence. *)
          Code.apply_props code (Code.Props.make ~content ?syntax ());
          Renderable.set_style (Code.node code) (block_style ~is_last);
          true
      | None -> false)
  | Hr_tag, Cmarkit.Block.Thematic_break _, Text_widget text ->
      Text.set_text_style text (env.style Thematic_break);
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | Html_tag, Cmarkit.Block.Html_block (lines, _), Text_widget text ->
      let text_content =
        String.concat "\n" (List.map Cmarkit.Block_line.to_string lines)
      in
      Text.set_content text text_content;
      Text.set_text_style text (env.style Default);
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | _ -> false

(* ───── Widget Destruction ───── *)

let rec destroy_widget = function
  | Text_widget text -> Renderable.destroy_recursively (Text.node text)
  | Code_widget code -> Renderable.destroy_recursively (Code.node code)
  | Box_widget (box, children) ->
      List.iter destroy_widget children;
      Renderable.destroy_recursively (Box.node box)
  | Container_widget (node, children) ->
      List.iter destroy_widget children;
      Renderable.destroy_recursively node

(* ───── Block Tag ───── *)

let tag_of_block (block : Cmarkit.Block.t) : block_tag =
  match block with
  | Cmarkit.Block.Paragraph _ -> Para_tag
  | Cmarkit.Block.Heading _ -> Head_tag
  | Cmarkit.Block.Code_block _ -> Code_tag
  | Cmarkit.Block.Block_quote _ -> Quote_tag
  | Cmarkit.Block.List _ -> List_tag
  | Cmarkit.Block.Thematic_break _ -> Hr_tag
  | Cmarkit.Block.Ext_table _ -> Table_tag
  | Cmarkit.Block.Html_block _ -> Html_tag
  | _ -> Custom_tag

let renderable_blocks (block : Cmarkit.Block.t) : Cmarkit.Block.t list =
  let filter bs =
    List.filter
      (fun b ->
        match b with
        | Cmarkit.Block.Blank_line _ | Cmarkit.Block.Link_reference_definition _
        | Cmarkit.Block.Ext_footnote_definition _ ->
            false
        | _ -> true)
      bs
  in
  match block with
  | Cmarkit.Block.Blocks (blocks, _meta) -> filter blocks
  | Cmarkit.Block.Blank_line _ | Cmarkit.Block.Link_reference_definition _
  | Cmarkit.Block.Ext_footnote_definition _ ->
      []
  | other -> [ other ]

(* The element styles: [style] where given, and otherwise those the theme in
   scope gives ({!Style.of_theme}), derived again only when the theme is
   another value. *)
let element_style t =
  match t.style with
  | Some style -> style
  | None -> (
      let theme = Renderable.theme t.node in
      match t.themed with
      | Some (seen, style) when seen == theme -> style
      | _ ->
          let style = Style.of_theme theme in
          t.themed <- Some (theme, style);
          style)

let render_env t defs =
  {
    style = Style.get (element_style t);
    conceal = t.conceal;
    defs;
    streaming = t.streaming;
    selectable = t.selectable;
    selection_bg = t.selection_bg;
    selection_fg = t.selection_fg;
    on_text_selection =
      (if Option.is_some t.on_selection then Some (fun _ -> fire_on_selection t)
       else None);
    render_node = t.render_node;
    render_code = t.render_code;
    highlighter = t.highlighter;
    syntax_style = t.syntax_style;
  }

(* ───── Reconciliation ───── *)

let block_textloc block =
  match Cmarkit.Block.meta block with
  | meta -> Cmarkit.Meta.textloc meta
  | exception Invalid_argument _ -> Cmarkit.Textloc.none

(* [same_source_block ~unchanged_prefix old new_block] is [true] when
   [new_block] denotes exactly the byte range [old] was rendered from and that
   range lies entirely within the prefix of the content that did not change.
   Such a block cannot render differently, so its widget is reused verbatim. *)
let same_source_block ~unchanged_prefix old_block new_block =
  let otl = block_textloc old_block in
  let ntl = block_textloc new_block in
  (not (Cmarkit.Textloc.is_none otl))
  && (not (Cmarkit.Textloc.is_none ntl))
  && Cmarkit.Textloc.first_byte otl = Cmarkit.Textloc.first_byte ntl
  && Cmarkit.Textloc.last_byte otl = Cmarkit.Textloc.last_byte ntl
  && Cmarkit.Textloc.last_byte ntl < unchanged_prefix

(* Link reference definitions are resolved document-wide, so prefix blocks may
   only be reused when the definitions their links resolve against are
   unchanged. Rendering uses a definition's destination only. *)
let defs_compatible a b =
  let dest = function
    | Cmarkit.Link_definition.Def (ld, _) ->
        Some (Option.map fst (Cmarkit.Link_definition.dest ld))
    | _ -> None
  in
  Cmarkit.Label.Map.equal
    (fun da db ->
      match (dest da, dest db) with
      | Some da, Some db -> Option.equal String.equal da db
      | _ -> false)
    a b

let common_prefix_length a b =
  let n = Stdlib.min (String.length a) (String.length b) in
  let i = ref 0 in
  while !i < n && String.unsafe_get a !i = String.unsafe_get b !i do
    incr i
  done;
  !i

let reconcile_blocks ~(env : render_env) ~reusable ~parent old_blocks
    new_ast_blocks =
  let total_old = List.length old_blocks in
  let total_new = List.length new_ast_blocks in
  let rec go i child_idx olds news =
    match (olds, news) with
    | [], [] -> []
    | [], new_block :: rest -> (
        let is_last = i = total_new - 1 in
        let tb =
          render_block ~env ~parent ~index:child_idx ~is_last new_block
        in
        let more = go (i + 1) (child_idx + 1) [] rest in
        match tb with Some tb -> tb :: more | None -> more)
    | old :: olds_rest, [] ->
        destroy_widget old.widget;
        go i child_idx olds_rest []
    | old :: olds_rest, new_block :: news_rest ->
        let new_tag = tag_of_block new_block in
        let is_last = i = total_new - 1 in
        let was_last = i = total_old - 1 in
        if is_last = was_last && reusable old new_block then begin
          (* Unchanged source, unchanged margin: keep the widget untouched.
             Refresh the stored ast so the next parse's textlocs compare. *)
          old.ast <- new_block;
          old :: go (i + 1) (child_idx + 1) olds_rest news_rest
        end
        else if
          old.tag = new_tag && update_leaf_in_place ~env ~is_last old new_block
        then begin
          old.ast <- new_block;
          old :: go (i + 1) (child_idx + 1) olds_rest news_rest
        end
        else begin
          destroy_widget old.widget;
          let tb =
            render_block ~env ~parent ~index:child_idx ~is_last new_block
          in
          let rest = go (i + 1) (child_idx + 1) olds_rest news_rest in
          match tb with Some tb -> tb :: rest | None -> rest
        end
  in
  go 0 0 old_blocks new_ast_blocks

(* ───── Content Update ───── *)

(* [update_blocks ?old_content t] reparses [t.content] and reconciles the
   widget list. [old_content] is the previous content when only the content
   changed (presentation props untouched): blocks whose source bytes lie in
   the unchanged prefix are then reused without touching their widgets, so a
   streaming append re-renders only the tail. *)
let update_blocks ?old_content t =
  let doc = Cmarkit.Doc.of_string ~locs:true ~strict:false t.content in
  let defs = Cmarkit.Doc.defs doc in
  let block = Cmarkit.Doc.block doc in
  let block = Cmarkit.Block.normalize block in
  let new_ast_blocks = renderable_blocks block in
  let reusable =
    match old_content with
    | Some old_content when defs_compatible t.last_defs defs ->
        let unchanged_prefix = common_prefix_length old_content t.content in
        fun old_tb new_block ->
          same_source_block ~unchanged_prefix old_tb.ast new_block
    | Some _ | None -> fun _ _ -> false
  in
  t.last_defs <- defs;
  let env = render_env t defs in
  let new_blocks =
    reconcile_blocks ~env ~reusable ~parent:t.node t.blocks new_ast_blocks
  in
  t.blocks <- new_blocks;
  (* Fallback: if parse produced no blocks but content is non-empty, render
     raw *)
  if new_blocks = [] && t.content <> "" then begin
    let text =
      create_text ~env ~parent:t.node ~content:t.content
        ~style:(block_style ~is_last:true)
        ~wrap:`Word ()
    in
    t.blocks <-
      [ { tag = Para_tag; widget = Text_widget text; ast = empty_ast } ]
  end;
  Renderable.request_render t.node

(* Re-render existing blocks with new style/conceal without re-parsing. The
   raw-text fallback block carries no parsed ast and is left as is. *)
let rerender_blocks t =
  let env = render_env t t.last_defs in
  let total = List.length t.blocks in
  List.iteri
    (fun i tb ->
      if tb.ast != empty_ast then begin
        let is_last = i = total - 1 in
        if not (update_leaf_in_place ~env ~is_last tb tb.ast) then begin
          destroy_widget tb.widget;
          match render_block ~env ~parent:t.node ~index:i ~is_last tb.ast with
          | Some new_tb ->
              tb.tag <- new_tb.tag;
              tb.widget <- new_tb.widget
          | None -> ()
        end
      end)
    t.blocks;
  Renderable.request_render t.node

(* A markdown given no [style] follows the theme in scope: another theme
   that gives other element styles renders the blocks again, before they
   are laid out. *)
let follow_theme t =
  match (t.style, t.themed) with
  | None, Some (seen, shown) ->
      let theme = Renderable.theme t.node in
      if theme != seen then begin
        let style = Style.of_theme theme in
        t.themed <- Some (theme, style);
        if (not (Style.equal style shown)) && t.blocks <> [] then
          rerender_blocks t
      end
  | _ -> ()

(* ───── Construction ───── *)

let of_props ~parent ?index ?id ?(layout_style = column_style) ?visible ?z_index
    ?opacity ?on_selection (props : Props.t) =
  let ({
         content;
         conceal;
         streaming;
         style;
         selectable;
         selection_bg;
         selection_fg;
         render_node;
         render_code;
         highlighter;
         syntax_style;
       }
        : Props.t) =
    props
  in
  let node =
    Renderable.create ~parent ?index ?id ~style:layout_style ?visible ?z_index
      ?opacity ()
  in
  (* Ensure the markdown node always uses column layout with full width,
     regardless of the layout_style passed by the reconciler. *)
  let effective_style =
    Renderable.style node
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_width full_width
  in
  Renderable.set_style node effective_style;
  let t =
    {
      node;
      content;
      style;
      conceal;
      streaming;
      selectable;
      selection_bg;
      selection_fg;
      on_selection;
      blocks = [];
      last_defs = Cmarkit.Label.Map.empty;
      render_node;
      render_code;
      highlighter;
      syntax_style;
      themed = None;
    }
  in
  Renderable.set_on_update node (Some (fun _ ~dt:_ -> follow_theme t));
  if content <> "" then update_blocks t;
  t

let create ~parent ?index ?id ?layout_style ?visible ?z_index ?opacity
    ?on_selection =
  Props.make_k (fun props ->
      of_props ~parent ?index ?id ?layout_style ?visible ?z_index ?opacity
        ?on_selection props)

(* ───── Layout Style ───── *)

let set_layout_style t style =
  let effective =
    style
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_width full_width
  in
  Renderable.set_style t.node effective

(* ───── Setters ───── *)

let set_content t s =
  if not (String.equal t.content s) then begin
    let old_content = t.content in
    t.content <- s;
    update_blocks ~old_content t
  end

let set_style t style =
  if not (Option.equal Style.equal t.style (Some style)) then begin
    t.style <- Some style;
    if t.blocks <> [] then rerender_blocks t else update_blocks t
  end

let set_highlighter t highlighter =
  if not (Option.equal ( == ) t.highlighter highlighter) then begin
    t.highlighter <- highlighter;
    if t.blocks <> [] then rerender_blocks t else update_blocks t
  end

let set_syntax_style t style =
  if not (Option.equal Syntax_style.equal t.syntax_style (Some style)) then begin
    t.syntax_style <- Some style;
    if t.blocks <> [] then rerender_blocks t else update_blocks t
  end

let set_conceal t v =
  if t.conceal <> v then begin
    t.conceal <- v;
    if t.blocks <> [] then rerender_blocks t else update_blocks t
  end

let set_streaming t v =
  if t.streaming <> v then begin
    t.streaming <- v;
    update_blocks t
  end

let rec apply_selection_to_widget env = function
  | Text_widget text -> configure_text env text
  | Code_widget _ -> ()
  | Box_widget (_, children) | Container_widget (_, children) ->
      List.iter (apply_selection_to_widget env) children

let apply_selection t =
  let env = render_env t t.last_defs in
  List.iter (fun block -> apply_selection_to_widget env block.widget) t.blocks

let set_selectable t v =
  if t.selectable <> v then begin
    t.selectable <- v;
    apply_selection t
  end

let set_selection_bg t color =
  if not (Option.equal Ansi.Color.equal t.selection_bg color) then begin
    t.selection_bg <- color;
    apply_selection t
  end

let set_selection_fg t color =
  if not (Option.equal Ansi.Color.equal t.selection_fg color) then begin
    t.selection_fg <- color;
    apply_selection t
  end

let set_on_selection t f =
  t.on_selection <- f;
  apply_selection t

(* ───── Props Application ───── *)

let apply_props t (props : Props.t) =
  (* Collapse every change into at most one pass over the blocks: a content
     append must not pay for unconditional style re-renders, and a combined
     change must not re-render twice. A block a custom renderer made cannot
     be updated in place, so a changed renderer renders every block again. *)
  let renderers_changed =
    (not (Option.equal ( == ) t.render_node props.render_node))
    || not (Option.equal ( == ) t.render_code props.render_code)
  in
  let presentation_changed =
    (not (Option.equal Style.equal t.style props.style))
    || (not (Option.equal ( == ) t.highlighter props.highlighter))
    || (not (Option.equal Syntax_style.equal t.syntax_style props.syntax_style))
    || t.conceal <> props.conceal
    || t.streaming <> props.streaming
  in
  let content_changed = not (String.equal t.content props.content) in
  let old_content = t.content in
  t.style <- props.style;
  t.render_node <- props.render_node;
  t.render_code <- props.render_code;
  t.highlighter <- props.highlighter;
  t.syntax_style <- props.syntax_style;
  t.conceal <- props.conceal;
  t.streaming <- props.streaming;
  t.content <- props.content;
  set_selectable t props.selectable;
  set_selection_bg t props.selection_bg;
  set_selection_fg t props.selection_fg;
  if renderers_changed then begin
    List.iter (fun tb -> destroy_widget tb.widget) t.blocks;
    t.blocks <- [];
    update_blocks t
  end
  else if content_changed then
    if presentation_changed then update_blocks t
    else update_blocks ~old_content t
  else if presentation_changed then
    if t.blocks <> [] then rerender_blocks t else update_blocks t

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Markdown(%s" (Renderable.id t.node);
  let len = String.length t.content in
  if len > 0 then begin
    let display =
      if len > 30 then String.sub t.content 0 30 ^ "..." else t.content
    in
    Format.fprintf ppf ", %S" display
  end;
  Format.fprintf ppf ", blocks=%d)" (List.length t.blocks)

module Private = struct
  let of_props = of_props
end
