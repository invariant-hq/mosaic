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
   [Mosaic.markdown] passes its labels on unset. *)
module Props = struct
  type t = {
    content : string;
    conceal : bool;
    streaming : bool;
    style : Style.t option;
    selectable : [ `Off | `Text | `Cells ] option;
    render_node : render_node option;
    render_code : render_code option;
    highlighter : Code.Highlighter.t option;
    syntax_style : Syntax_style.t option;
  }

  let make ?(content = "") ?(conceal = true) ?(streaming = false) ?style
      ?selectable ?render_node ?render_code ?highlighter ?syntax_style () =
    {
      content;
      conceal;
      streaming;
      style;
      selectable;
      render_node;
      render_code;
      highlighter;
      syntax_style;
    }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && a.conceal = b.conceal && a.streaming = b.streaming
    && Option.equal Style.equal a.style b.style
    && Option.equal ( = ) a.selectable b.selectable
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

(* A list, a block quote and a table keep their nodes and update them in
   place, as a paragraph does, so that a selection end in them survives a
   streaming chunk. *)
type widget =
  | Text_widget of Text.t
  | Box_widget of Box.t * widget list
  | Container_widget of Renderable.t * widget list
  | Code_widget of Code.t
  | Quote_widget of { box : Box.t; mutable blocks : tagged_block list }
  | List_widget of { node : Renderable.t; mutable items : list_item list }
  | Table_widget of { node : Renderable.t; columns : table_column list }

and tagged_block = {
  mutable tag : block_tag;
  mutable widget : widget;
  mutable ast : Cmarkit.Block.t;
      (* The block this widget was rendered from. Its textloc identifies the
         source bytes so unchanged prefix blocks can be reused verbatim on
         streaming appends. *)
  mutable last : bool;
      (* Whether the widget is laid out as the last of its siblings, with no
         margin below it. *)
}

(* A list item: its row, whether a margin follows it, the marker beside its
   content, and the blocks of its content. *)
and list_item = {
  row : Renderable.t;
  mutable spaced : bool;
  marker : Text.t;
  content : Renderable.t;
  mutable blocks : tagged_block list;
}

(* A table column: its box, the box and text of its header, if the table has
   one, and those of each data cell, every cell but the last bordered
   below. *)
and table_column = {
  column : Box.t;
  header : (Box.t * Text.t) option;
  mutable cells : (Box.t * Text.t) list;
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

(* A block's text declares nothing of the selection: it reads as the
   markdown does. *)
let create_text ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content
    ?text_style ?(wrap = `None) ?tab_width ?truncate () =
  Text.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    (Text.Props.make ?content ?text_style ~wrap ?tab_width ?truncate ())

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
    create_text ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Para_tag; widget = Text_widget text; ast = empty_ast; last = is_last }

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
    create_text ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Head_tag; widget = Text_widget text; ast = empty_ast; last = is_last }

let render_code_block_default ~(env : render_env) ~parent ?index ~is_last
    (cb : Cmarkit.Block.Code_block.t) =
  let code_text = code_block_text cb in
  let title = code_block_language cb in
  let code_style = env.style Code_block in
  let box =
    Box.create ~parent ?index
      ~style:(bordered_block_style ~is_last)
      (Box.Props.make ~border:true ~border_sides:[ `Left ] ?title ())
  in
  let text =
    create_text ~parent:(Box.node box) ~content:code_text ~text_style:code_style
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
    last = is_last;
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
    Code.create ~parent ?index ~style:(block_style ~is_last)
      (Code.Props.make ~content ?syntax ())
  in
  { tag = Code_tag; widget = Code_widget code; ast = empty_ast; last = is_last }

let render_thematic_break ~(env : render_env) ~parent ?index ~is_last
    (_tb : Cmarkit.Block.Thematic_break.t) =
  let hr_style = env.style Thematic_break in
  let text =
    create_text ~parent ?index
      ~content:"───────────────────────────────────────" ~text_style:hr_style
      ~style:(block_style ~is_last) ~truncate:true ()
  in
  { tag = Hr_tag; widget = Text_widget text; ast = empty_ast; last = is_last }

let render_html_block ~(env : render_env) ~parent ?index ~is_last
    (lines : Cmarkit.Block.Html_block.t) =
  let text_content =
    String.concat "\n" (List.map Cmarkit.Block_line.to_string lines)
  in
  let text =
    create_text ~parent ?index ~content:text_content
      ~text_style:(env.style Default) ~style:(block_style ~is_last) ()
  in
  { tag = Html_tag; widget = Text_widget text; ast = empty_ast; last = is_last }

(* ───── Lists ───── *)

let list_style ~is_last =
  Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
    ~size:(Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
    ~margin:(if is_last then margin_bottom 0 else margin_bottom 1)
    ()

let item_style ~spaced =
  Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Row
    ~size:(Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
    ~margin:(margin_bottom (if spaced then 1 else 0))
    ()

let marker_text (list_type : Cmarkit.Block.List'.type') index =
  match list_type with
  | `Unordered _ -> "• "
  | `Ordered (start, _) -> string_of_int (start + index) ^ ". "

(* A new item of a list, its content empty. *)
let new_list_item ~(env : render_env) ~parent ~spaced ~list_type ~index =
  let row = Renderable.create ~parent ~style:(item_style ~spaced) () in
  let marker =
    create_text ~parent:row
      ~content:(marker_text list_type index)
      ~text_style:(env.style List_marker)
      ~style:
        (Toffee.Style.make
           ~size:
             (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                Toffee.Style.Dimension.auto)
           ~flex_shrink:0. ())
      ()
  in
  let content =
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
  { row; spaced; marker; content; blocks = [] }

(* The task marker of an item, which leads the text of its first block. *)
let task_lead ~(env : render_env) (item : Cmarkit.Block.List_item.t) =
  match Cmarkit.Block.List_item.ext_task_marker item with
  | Some (uchar, _meta) ->
      let marker =
        match Cmarkit.Block.List_item.task_status_of_task_marker uchar with
        | `Checked -> "[x] "
        | `Unchecked -> "[ ] "
        | `Cancelled -> "[~] "
        | `Other _ -> "[?] "
      in
      [ { Text_buffer.text = marker; style = env.style Task_marker } ]
  | None -> []

(* ───── Tables ───── *)

let table_style ~is_last =
  Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Row
    ~size:(Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
    ~margin:(if is_last then margin_bottom 0 else margin_bottom 1)
    ()

let cell_text_style =
  Toffee.Style.make
    ~size:
      (Toffee.Geometry.Size.make full_width (Toffee.Style.Dimension.length 1.))
    ~overflow:
      (Toffee.Geometry.Point.make Toffee.Style.Overflow.Hidden
         Toffee.Style.Overflow.Hidden)
    ()

(* The rows a table shows: its column count, its header's cells, if any, and
   its data rows' cells, the last row left out while streaming, which may be
   incomplete. [None] when it has no column, or neither header nor data. *)
let table_rows ~(env : render_env) (table : Cmarkit.Block.Table.t) =
  let all_rows = Cmarkit.Block.Table.rows table in
  let rows =
    if env.streaming && all_rows <> [] then
      match List.rev all_rows with _ :: rest -> List.rev rest | [] -> []
    else all_rows
  in
  let header = ref None and data = ref [] in
  List.iter
    (fun ((row, _meta), _blanks) ->
      match row with
      | `Sep _ -> ()
      | `Header cells -> header := Some cells
      | `Data cells -> data := cells :: !data)
    rows;
  let data = List.rev !data in
  let col_count = Cmarkit.Block.Table.col_count table in
  if col_count = 0 || (Option.is_none !header && data = []) then None
  else Some (col_count, !header, data)

let cell_spans ~(env : render_env) ~base cells col =
  match List.nth_opt cells col with
  | Some (inline, _) ->
      inline_to_spans ~style:env.style ~conceal:env.conceal ~base ~defs:env.defs
        inline
  | None -> []

(* Column [col] of [col_count], with a header box when [header], its cells
   empty. *)
let new_table_column ~(env : render_env) ~parent ~col ~col_count ~header =
  let border_color = (env.style Table_border).fg in
  let column =
    Box.create ~parent
      ~style:
        (Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
           ~flex_grow:1.
           ~size:
             (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                Toffee.Style.Dimension.auto)
           ())
      (Box.Props.make ~border:true
         ~border_style:
           (if col = 0 then Matrix_grid.Border.single else inner_column_border)
         ~border_sides:
           (if col = col_count - 1 then Matrix_grid.Border.all
            else [ `Top; `Bottom; `Left ])
         ?border_color ())
  in
  let header =
    if header then
      let box =
        Box.create ~parent:(Box.node column)
          (Box.Props.make ~border:true ~border_sides:[ `Bottom ] ?border_color
             ())
      in
      Some
        ( box,
          create_text ~parent:(Box.node box) ~style:cell_text_style
            ~truncate:true () )
    else None
  in
  { column; header; cells = [] }

(* The columns show [header] and [data]: each cell's text is set, a cell is
   added for each new row and removed for each row gone, and every cell but
   the last is bordered below. *)
let fill_table ~(env : render_env) columns header data =
  let border_color = (env.style Table_border).fg in
  let header_base =
    Ansi.Style.merge ~base:(env.style Default)
      ~overlay:(Ansi.Style.make ~bold:true ())
  in
  let count = List.length data in
  List.iteri
    (fun col c ->
      Option.iter (Box.set_border_color c.column) border_color;
      (match (c.header, header) with
      | Some (box, text), Some cells ->
          Option.iter (Box.set_border_color box) border_color;
          Text.set_styled_text text
            (cell_spans ~env ~base:header_base cells col)
      | Some _, None | None, _ -> ());
      let rec fill i cells rows =
        match (cells, rows) with
        | [], [] -> []
        | (box, _) :: cells, [] ->
            Renderable.destroy_recursively (Box.node box);
            fill i cells []
        | cells, row :: rows ->
            let (box, text), cells =
              match cells with
              | cell :: cells -> (cell, cells)
              | [] ->
                  let box =
                    Box.create ~parent:(Box.node c.column)
                      (Box.Props.make ~border_sides:[ `Bottom ] ())
                  in
                  ( ( box,
                      create_text ~parent:(Box.node box) ~style:cell_text_style
                        ~truncate:true () ),
                    [] )
            in
            let bordered = i < count - 1 in
            Box.set_border box bordered;
            if bordered then Option.iter (Box.set_border_color box) border_color;
            Text.set_styled_text text
              (cell_spans ~env ~base:(env.style Default) row col);
            (box, text) :: fill (i + 1) cells rows
      in
      c.cells <- fill 0 c.cells data)
    columns

let render_table ~(env : render_env) ~parent ?index ~is_last
    (table : Cmarkit.Block.Table.t) =
  match table_rows ~env table with
  | None ->
      let text =
        create_text ~parent ?index ~content:"" ~style:(block_style ~is_last) ()
      in
      {
        tag = Table_tag;
        widget = Text_widget text;
        ast = empty_ast;
        last = is_last;
      }
  | Some (col_count, header, data) ->
      let node =
        Renderable.create ~parent ?index ~style:(table_style ~is_last) ()
      in
      let columns =
        List.init col_count (fun col ->
            new_table_column ~env ~parent:node ~col ~col_count
              ~header:(Option.is_some header))
      in
      fill_table ~env columns header data;
      {
        tag = Table_tag;
        widget = Table_widget { node; columns };
        ast = empty_ast;
        last = is_last;
      }

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
  | Quote_widget { box; _ } -> Renderable.destroy_recursively (Box.node box)
  | List_widget { node; _ } | Table_widget { node; _ } ->
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

(* A block rendered anew is reused by nothing. *)
let never _ _ = false

(* [lead] leads the text of a block rendered anew. *)
let with_lead lead tb =
  match (lead, tb.widget) with
  | _ :: _, Text_widget text ->
      Text.set_styled_text text
        (lead @ Text_buffer.styled_text (Text.buffer text))
  | [], _ | _ :: _, _ -> ()

(* ───── Rendering and Update ───── *)

let rec render_list ~(env : render_env) ~parent ?index ~is_last
    (lst : Cmarkit.Block.List'.t) =
  let node = Renderable.create ~parent ?index ~style:(list_style ~is_last) () in
  let items = list_items ~env ~reusable:never ~parent:node [] lst in
  {
    tag = List_tag;
    widget = List_widget { node; items };
    ast = empty_ast;
    last = is_last;
  }

(* The items of [lst] under [parent], from the items [olds] shown there
   before: each kept item is updated in place, by the index of the item it
   shows, the items added are rendered after them and those gone are
   destroyed. *)
and list_items ~(env : render_env) ~reusable ~parent olds
    (lst : Cmarkit.Block.List'.t) =
  let items = Cmarkit.Block.List'.items lst in
  let tight = Cmarkit.Block.List'.tight lst in
  let list_type = Cmarkit.Block.List'.type' lst in
  let count = List.length items in
  let rec go index olds items =
    match (olds, items) with
    | [], [] -> []
    | old :: olds, [] ->
        Renderable.destroy_recursively old.row;
        go index olds []
    | olds, (item, _meta) :: items ->
        let spaced = not (index = count - 1 || tight) in
        let shown, olds =
          match olds with
          | shown :: olds ->
              if not (Bool.equal shown.spaced spaced) then begin
                shown.spaced <- spaced;
                Renderable.set_style shown.row (item_style ~spaced)
              end;
              Text.set_content shown.marker (marker_text list_type index);
              Text.set_text_style shown.marker (env.style List_marker);
              (shown, olds)
          | [] -> (new_list_item ~env ~parent ~spaced ~list_type ~index, [])
        in
        shown.blocks <-
          reconcile_blocks ~env ~reusable ~tight ~lead:(task_lead ~env item)
            ~parent:shown.content shown.blocks
            (renderable_blocks (Cmarkit.Block.List_item.block item));
        shown :: go (index + 1) olds items
  in
  go 0 olds items

and render_blockquote ~(env : render_env) ~parent ?index ~is_last
    (bq : Cmarkit.Block.Block_quote.t) =
  let box =
    Box.create ~parent ?index
      ~style:(bordered_column_block_style ~is_last)
      (Box.Props.make ~border:true ~border_sides:[ `Left ]
         ?border_color:(env.style Blockquote).fg ())
  in
  let blocks =
    reconcile_blocks ~env ~reusable:never ~parent:(Box.node box) []
      (renderable_blocks (Cmarkit.Block.Block_quote.block bq))
  in
  {
    tag = Quote_tag;
    widget = Quote_widget { box; blocks };
    ast = empty_ast;
    last = is_last;
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
          last = is_last;
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
                last = is_last;
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
            last = is_last;
          }
  | Cmarkit.Block.Blank_line _ | Cmarkit.Block.Link_reference_definition _
  | Cmarkit.Block.Ext_footnote_definition _ ->
      None
  | _ -> None

and render_block_list ~(env : render_env) ~parent blocks =
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
      let is_last = i = count - 1 in
      match render_block ~env ~parent ~is_last block with
      | Some tb -> [ tb ]
      | None -> [])
    (List.mapi (fun i b -> (i, b)) blocks)

(* [update_in_place ~env ~reusable ~is_last ~lead tb block] shows [block]
   in [tb]'s widget, which the block's own tag made, and is [false] when the
   widget cannot show it. [lead] leads the text of a paragraph or a
   heading. *)
and update_in_place ~(env : render_env) ~reusable ~is_last ~lead
    (tb : tagged_block) (block : Cmarkit.Block.t) : bool =
  match (tb.tag, block, tb.widget) with
  | Para_tag, Cmarkit.Block.Paragraph (p, _), Text_widget text ->
      let inline = Cmarkit.Block.Paragraph.inline p in
      let base = env.style Default in
      let spans =
        inline_to_spans ~style:env.style ~conceal:env.conceal ~base
          ~defs:env.defs inline
      in
      Text.set_styled_text text (lead @ spans);
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
      Text.set_styled_text text (lead @ spans);
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | _ when lead <> [] -> false
  | ( Code_tag,
      Cmarkit.Block.Code_block (cb, _),
      Box_widget (box, [ Text_widget text ]) )
    when Option.is_none env.render_code && Option.is_none env.highlighter ->
      let code_text = code_block_text cb in
      let title = code_block_language cb in
      Text.set_content text code_text;
      Text.set_text_style text (env.style Code_block);
      Box.set_title box title;
      Box.set_style box (bordered_block_style ~is_last);
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
  | Quote_tag, Cmarkit.Block.Block_quote (bq, _), Quote_widget quote ->
      let bq_style = env.style Blockquote in
      Box.set_style quote.box (bordered_column_block_style ~is_last);
      Option.iter (Box.set_border_color quote.box) bq_style.fg;
      quote.blocks <-
        reconcile_blocks ~env ~reusable ~parent:(Box.node quote.box)
          quote.blocks
          (renderable_blocks (Cmarkit.Block.Block_quote.block bq));
      true
  | List_tag, Cmarkit.Block.List (lst, _), List_widget l ->
      Renderable.set_style l.node (list_style ~is_last);
      l.items <- list_items ~env ~reusable ~parent:l.node l.items lst;
      true
  | ( Table_tag,
      Cmarkit.Block.Ext_table (table, _),
      Table_widget { node; columns } ) -> (
      let with_header =
        match columns with c :: _ -> Option.is_some c.header | [] -> false
      in
      match table_rows ~env table with
      | Some (col_count, header, data)
        when col_count = List.length columns
             && Bool.equal (Option.is_some header) with_header ->
          Renderable.set_style node (table_style ~is_last);
          fill_table ~env columns header data;
          true
      | Some _ | None -> false)
  | Table_tag, Cmarkit.Block.Ext_table (table, _), Text_widget text
    when Option.is_none (table_rows ~env table) ->
      Renderable.set_style (Text.node text) (block_style ~is_last);
      true
  | _ -> false

(* [reconcile_blocks ~env ~reusable ~parent olds blocks] are the blocks
   that show [blocks] under [parent], from [olds] shown there before, in
   order: a block whose source is unchanged keeps its widget untouched, one
   of the same kind updates it in place, and any other replaces it. A block
   is the last of its siblings when [tight]. [lead] leads the text of the
   first block (a task marker). *)
and reconcile_blocks ~(env : render_env) ~reusable ?(tight = false) ?(lead = [])
    ~parent old_blocks new_ast_blocks =
  let total = List.length new_ast_blocks in
  let rec go i placed olds news =
    match (olds, news) with
    | [], [] -> []
    | old :: olds, [] ->
        destroy_widget old.widget;
        go i placed olds []
    | olds, block :: news -> (
        let is_last = i = total - 1 || tight in
        let lead = if placed = 0 then lead else [] in
        match olds with
        | old :: olds when old.last = is_last && reusable old block ->
            (* Unchanged source, unchanged margin: keep the widget untouched.
               Refresh the stored ast so the next parse's textlocs compare. *)
            old.ast <- block;
            old :: go (i + 1) (placed + 1) olds news
        | old :: olds
          when old.tag = tag_of_block block
               && update_in_place ~env ~reusable ~is_last ~lead old block ->
            old.ast <- block;
            old.last <- is_last;
            old :: go (i + 1) (placed + 1) olds news
        | olds -> (
            let olds =
              match olds with
              | old :: olds ->
                  destroy_widget old.widget;
                  olds
              | [] -> []
            in
            match render_block ~env ~parent ~index:placed ~is_last block with
            | Some tb ->
                with_lead lead tb;
                tb :: go (i + 1) (placed + 1) olds news
            | None -> go (i + 1) placed olds news))
  in
  go 0 0 old_blocks new_ast_blocks

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
      create_text ~parent:t.node ~content:t.content
        ~style:(block_style ~is_last:true)
        ~wrap:`Word ()
    in
    t.blocks <-
      [
        {
          tag = Para_tag;
          widget = Text_widget text;
          ast = empty_ast;
          last = true;
        };
      ]
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
        if update_in_place ~env ~reusable:never ~is_last ~lead:[] tb tb.ast then
          tb.last <- is_last
        else begin
          destroy_widget tb.widget;
          match render_block ~env ~parent:t.node ~index:i ~is_last tb.ast with
          | Some new_tb ->
              tb.tag <- new_tb.tag;
              tb.widget <- new_tb.widget;
              tb.last <- is_last
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

let create ~parent ?index ?id ?(layout_style = column_style) ?visible ?z_index
    ?opacity (props : Props.t) =
  let ({
         content;
         conceal;
         streaming;
         style;
         selectable;
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
  Renderable.set_selectable node selectable;
  let t =
    {
      node;
      content;
      style;
      conceal;
      streaming;
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
  Renderable.set_selectable t.node props.selectable;
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
