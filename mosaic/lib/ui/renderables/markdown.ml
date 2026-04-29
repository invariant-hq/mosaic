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

type style = style_key -> Ansi.Style.t

let default_style = function
  | Default -> Ansi.Style.default
  | Heading 1 -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0 215 175) ~bold:true ()
  | Heading 2 ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 175 255) ~bold:true ()
  | Heading 3 ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 200 140 255) ~bold:true ()
  | Heading _ ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 135 175 255) ~bold:true ()
  | Emphasis -> Ansi.Style.make ~italic:true ()
  | Strong -> Ansi.Style.make ~bold:true ()
  | Code_span -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 180 200 220) ()
  | Code_block -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 180 200 220) ()
  | Link ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 175 255) ~underline:true ()
  | Image ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 175 255) ~underline:true ()
  | Blockquote ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 140 150 165) ~italic:true ()
  | Thematic_break -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 80 90 100) ()
  | List_marker -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 95 135 215) ()
  | Strikethrough ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 140 150 165) ~strikethrough:true ()
  | Task_marker ->
      Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0 215 175) ~bold:true ()
  | Table_border -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 80 90 100) ()
  | Conceal_punctuation -> Ansi.Style.make ~fg:(Ansi.Color.of_rgb 70 80 90) ()

(* ───── Props ───── *)

module Props = struct
  type t = { content : string; conceal : bool; streaming : bool; style : style }

  let make ?(content = "") ?(conceal = true) ?(streaming = false)
      ?(style = default_style) () =
    { content; conceal; streaming; style }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && a.conceal = b.conceal && a.streaming = b.streaming && a.style == b.style
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

type tagged_block = { mutable tag : block_tag; mutable widget : widget }

(* ───── Render Environment ───── *)

type render_env = {
  style : style;
  conceal : bool;
  defs : Cmarkit.Label.defs;
  streaming : bool;
  render_node :
    (Cmarkit.Block.t ->
    parent:Renderable.t ->
    is_last:bool ->
    Renderable.t option)
    option;
  render_code :
    (parent:Renderable.t ->
    language:string option ->
    content:string ->
    Renderable.t)
    option;
}

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable content : string;
  mutable style : style;
  mutable conceal : bool;
  mutable streaming : bool;
  mutable blocks : tagged_block list;
  mutable last_defs : Cmarkit.Label.defs;
  mutable last_ast_blocks : Cmarkit.Block.t list;
  render_node :
    (Cmarkit.Block.t ->
    parent:Renderable.t ->
    is_last:bool ->
    Renderable.t option)
    option;
  render_code :
    (parent:Renderable.t ->
    language:string option ->
    content:string ->
    Renderable.t)
    option;
}

let node t = t.node
let content t = t.content

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

let conceal_punct ~(style : style) text =
  { Text_buffer.text; style = style Conceal_punctuation }

let rec inline_to_spans ~(style : style) ~conceal ~(base : Ansi.Style.t)
    ~(defs : Cmarkit.Label.defs) (inline : Cmarkit.Inline.t) =
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
  Grid.Border.modify
    ~top_left:(Uchar.of_int 0x252C) (* ┬ *)
    ~bottom_left:(Uchar.of_int 0x2534) (* ┴ *)
    Grid.Border.single

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
    Text.create ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Para_tag; widget = Text_widget text }

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
    Text.create ~parent ?index ~style:(block_style ~is_last) ~wrap:`Word ()
  in
  Text.set_styled_text text spans;
  { tag = Head_tag; widget = Text_widget text }

let render_code_block_default ~(env : render_env) ~parent ?index ~is_last
    (cb : Cmarkit.Block.Code_block.t) =
  let code_text = code_block_text cb in
  let title = code_block_language cb in
  let code_style = env.style Code_block in
  let box =
    Box.create ~parent ?index
      ~style:(bordered_block_style ~is_last)
      ~border:true ~border_sides:[ `Left ] ~border_color:Ansi.Color.white ?title
      ()
  in
  let text =
    Text.create ~parent:(Box.node box) ~content:code_text ~text_style:code_style
      ~style:
        (Toffee.Style.make
           ~size:
             (Toffee.Geometry.Size.make full_width Toffee.Style.Dimension.auto)
           ())
      ()
  in
  { tag = Code_tag; widget = Box_widget (box, [ Text_widget text ]) }

let render_thematic_break ~(env : render_env) ~parent ?index ~is_last
    (_tb : Cmarkit.Block.Thematic_break.t) =
  let hr_style = env.style Thematic_break in
  let text =
    Text.create ~parent ?index
      ~content:"───────────────────────────────────────" ~text_style:hr_style
      ~style:(block_style ~is_last) ~truncate:true ()
  in
  { tag = Hr_tag; widget = Text_widget text }

let render_html_block ~(env : render_env) ~parent ?index ~is_last
    (lines : Cmarkit.Block.Html_block.t) =
  let text_content =
    String.concat "\n" (List.map Cmarkit.Block_line.to_string lines)
  in
  let text =
    Text.create ~parent ?index ~content:text_content
      ~text_style:(env.style Default) ~style:(block_style ~is_last) ()
  in
  { tag = Html_tag; widget = Text_widget text }

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
  { tag = List_tag; widget = Container_widget (container, item_widgets) }

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
    Text.create ~parent:row ~content:marker_text
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
      let existing_spans =
        let lc = Text_buffer.line_count buf in
        if lc > 0 then
          List.concat (List.init lc (fun i -> Text_buffer.line_spans buf i))
        else []
      in
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
    Box.create ~parent ?index
      ~style:(bordered_column_block_style ~is_last)
      ~border:true ~border_sides:[ `Left ]
      ~border_color:
        (match bq_style.fg with Some c -> c | None -> Ansi.Color.yellow)
      ()
  in
  let inner_block = Cmarkit.Block.Block_quote.block bq in
  let children =
    render_block_children ~env ~parent:(Box.node box) inner_block
  in
  {
    tag = Quote_tag;
    widget = Box_widget (box, List.map (fun tb -> tb.widget) children);
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
      Text.create ~parent ?index ~content:"" ~style:(block_style ~is_last) ()
    in
    { tag = Table_tag; widget = Text_widget text }
  end
  else
    let border_style = env.style Table_border in
    let border_color =
      match border_style.fg with Some c -> c | None -> Ansi.Color.white
    in
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
            if is_first_col then Grid.Border.single else inner_column_border
          in
          let border_sides =
            if is_last_col then Grid.Border.all else [ `Top; `Bottom; `Left ]
          in
          let col_box =
            Box.create ~parent:table_box ~border:true ~border_style:border_chars
              ~border_sides ~border_color
              ~style:
                (Toffee.Style.make
                   ~flex_direction:Toffee.Style.Flex_direction.Column
                   ~flex_grow:1.
                   ~size:
                     (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                        Toffee.Style.Dimension.auto)
                   ())
              ()
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
                  Box.create ~parent:(Box.node col_box) ~border:true
                    ~border_sides:[ `Bottom ] ~border_color ()
                in
                let text =
                  Text.create ~parent:(Box.node header_box)
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
                    Text.create ~parent:(Box.node col_box)
                      ~style:cell_text_style ~truncate:true ()
                  in
                  Text.set_styled_text text spans;
                  Text_widget text
                end
                else begin
                  let cell_box =
                    Box.create ~parent:(Box.node col_box) ~border:true
                      ~border_sides:[ `Bottom ] ~border_color ()
                  in
                  let text =
                    Text.create ~parent:(Box.node cell_box)
                      ~style:cell_text_style ~truncate:true ()
                  in
                  Text.set_styled_text text spans;
                  Box_widget (cell_box, [ Text_widget text ])
                end)
              data_rows
          in
          Box_widget (col_box, header_widget @ row_widgets))
    in
    { tag = Table_tag; widget = Container_widget (table_box, col_widgets) }

(* Dispatch a single block to the appropriate renderer, checking hooks first *)
and render_block ~(env : render_env) ~parent ?index ~is_last
    (block : Cmarkit.Block.t) : tagged_block option =
  (* 1. Code block hook *)
  match (env.render_code, block) with
  | Some render_code, Cmarkit.Block.Code_block (cb, _meta) ->
      let language = code_block_language cb in
      let content = code_block_text cb in
      let custom_node = render_code ~parent ~language ~content in
      Some { tag = Code_tag; widget = Container_widget (custom_node, []) }
  | _ -> (
      (* 2. General render hook *)
      let custom =
        match env.render_node with
        | Some hook -> (
            match hook block ~parent ~is_last with
            | Some custom_node ->
                Some
                  {
                    tag = Custom_tag;
                    widget = Container_widget (custom_node, []);
                  }
            | None -> None)
        | None -> None
      in
      match custom with
      | Some tb -> Some tb
      | None ->
          (* 3. Default rendering *)
          render_block_default ~env ~parent ?index ~is_last block)

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
    when env.render_code = None ->
      let code_text = code_block_text cb in
      let title = code_block_language cb in
      Text.set_content text code_text;
      Text.set_text_style text (env.style Code_block);
      Box.set_title box title;
      Box.set_border_color box Ansi.Color.white;
      Renderable.set_style (Box.node box) (block_style ~is_last);
      true
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

(* ───── Reconciliation ───── *)

let reconcile_blocks ~(env : render_env) ~parent old_blocks new_ast_blocks =
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
        if old.tag = new_tag && update_leaf_in_place ~env ~is_last old new_block
        then old :: go (i + 1) (child_idx + 1) olds_rest news_rest
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

let update_blocks t =
  let doc = Cmarkit.Doc.of_string ~strict:false t.content in
  let defs = Cmarkit.Doc.defs doc in
  let block = Cmarkit.Doc.block doc in
  let block = Cmarkit.Block.normalize block in
  let new_ast_blocks = renderable_blocks block in
  t.last_defs <- defs;
  t.last_ast_blocks <- new_ast_blocks;
  let env =
    {
      style = t.style;
      conceal = t.conceal;
      defs;
      streaming = t.streaming;
      render_node = t.render_node;
      render_code = t.render_code;
    }
  in
  let new_blocks =
    reconcile_blocks ~env ~parent:t.node t.blocks new_ast_blocks
  in
  t.blocks <- new_blocks;
  (* Fallback: if parse produced no blocks but content is non-empty, render
     raw *)
  if new_blocks = [] && t.content <> "" then begin
    let text =
      Text.create ~parent:t.node ~content:t.content
        ~style:(block_style ~is_last:true)
        ~wrap:`Word ()
    in
    t.blocks <- [ { tag = Para_tag; widget = Text_widget text } ]
  end;
  Renderable.request_render t.node

(* Re-render existing blocks with new style/conceal without re-parsing. *)
let rerender_blocks t =
  let env =
    {
      style = t.style;
      conceal = t.conceal;
      defs = t.last_defs;
      streaming = t.streaming;
      render_node = t.render_node;
      render_code = t.render_code;
    }
  in
  let total = List.length t.blocks in
  let ast_blocks = t.last_ast_blocks in
  let rec go i blocks ast =
    match (blocks, ast) with
    | [], _ | _, [] -> ()
    | tb :: blocks_rest, ast_block :: ast_rest ->
        let is_last = i = total - 1 in
        if not (update_leaf_in_place ~env ~is_last tb ast_block) then begin
          destroy_widget tb.widget;
          match
            render_block ~env ~parent:t.node ~index:i ~is_last ast_block
          with
          | Some new_tb ->
              tb.tag <- new_tb.tag;
              tb.widget <- new_tb.widget
          | None -> ()
        end;
        go (i + 1) blocks_rest ast_rest
  in
  go 0 t.blocks ast_blocks;
  Renderable.request_render t.node

(* ───── Construction ───── *)

let create ~parent ?index ?id ?(layout_style = column_style) ?visible ?z_index
    ?opacity ?(content = "") ?(style = default_style) ?(conceal = true)
    ?(streaming = false) ?render_node ?render_code () =
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
      blocks = [];
      last_defs = Cmarkit.Label.Map.empty;
      last_ast_blocks = [];
      render_node;
      render_code;
    }
  in
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
    t.content <- s;
    update_blocks t
  end

let set_style t f =
  t.style <- f;
  if t.blocks <> [] then rerender_blocks t else update_blocks t

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
  set_style t props.style;
  set_conceal t props.conceal;
  set_streaming t props.streaming;
  set_content t props.content

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
