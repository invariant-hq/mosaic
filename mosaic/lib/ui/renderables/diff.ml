module Patch = Diff_patch

type layout = Unified | Split

type theme = {
  added_bg : Ansi.Color.t;
  removed_bg : Ansi.Color.t;
  context_bg : Ansi.Color.t option;
  added_content_bg : Ansi.Color.t option;
  removed_content_bg : Ansi.Color.t option;
  context_content_bg : Ansi.Color.t option;
  added_sign_color : Ansi.Color.t;
  removed_sign_color : Ansi.Color.t;
  added_line_number_bg : Ansi.Color.t option;
  removed_line_number_bg : Ansi.Color.t option;
  line_number_fg : Ansi.Color.t;
  line_number_bg : Ansi.Color.t option;
}

let default_theme =
  {
    added_bg = Ansi.Color.of_rgb 26 77 26;
    removed_bg = Ansi.Color.of_rgb 77 26 26;
    context_bg = None;
    added_content_bg = None;
    removed_content_bg = None;
    context_content_bg = None;
    added_sign_color = Ansi.Color.of_rgb 34 197 94;
    removed_sign_color = Ansi.Color.of_rgb 239 68 68;
    added_line_number_bg = None;
    removed_line_number_bg = None;
    line_number_fg = Ansi.Color.grayscale ~level:12;
    line_number_bg = None;
  }

let theme_equal a b =
  Ansi.Color.equal a.added_bg b.added_bg
  && Ansi.Color.equal a.removed_bg b.removed_bg
  && Option.equal Ansi.Color.equal a.context_bg b.context_bg
  && Option.equal Ansi.Color.equal a.added_content_bg b.added_content_bg
  && Option.equal Ansi.Color.equal a.removed_content_bg b.removed_content_bg
  && Option.equal Ansi.Color.equal a.context_content_bg b.context_content_bg
  && Ansi.Color.equal a.added_sign_color b.added_sign_color
  && Ansi.Color.equal a.removed_sign_color b.removed_sign_color
  && Option.equal Ansi.Color.equal a.added_line_number_bg b.added_line_number_bg
  && Option.equal Ansi.Color.equal a.removed_line_number_bg
       b.removed_line_number_bg
  && Ansi.Color.equal a.line_number_fg b.line_number_fg
  && Option.equal Ansi.Color.equal a.line_number_bg b.line_number_bg

type highlight = { old : Code.syntax; new_ : Code.syntax }

let highlight_equal a b = a.old = b.old && a.new_ = b.new_

module Props = struct
  type t = {
    patch : Patch.t;
    layout : layout;
    theme : theme;
    highlight : highlight option;
    show_line_numbers : bool;
    wrap : Text_surface.wrap;
    selectable : bool;
    text_style : Ansi.Style.t;
  }

  let empty_patch = Patch.empty

  let make ?(patch = empty_patch) ?(layout = Unified) ?(theme = default_theme)
      ?highlight ?(show_line_numbers = true) ?(wrap = `None)
      ?(selectable = true) ?(text_style = Ansi.Style.default) () =
    {
      patch;
      layout;
      theme;
      highlight;
      show_line_numbers;
      wrap;
      selectable;
      text_style;
    }

  let default = make ()

  let equal a b =
    Patch.equal a.patch b.patch
    && a.layout = b.layout
    && theme_equal a.theme b.theme
    && Option.equal highlight_equal a.highlight b.highlight
    && a.show_line_numbers = b.show_line_numbers
    && a.wrap = b.wrap
    && a.selectable = b.selectable
    && Ansi.Style.equal a.text_style b.text_style
end

module View = struct
  let transparent = Ansi.Color.default

  let line_color_of ~theme : Patch.tag -> Line_number.line_color = function
    | Added ->
        {
          gutter = Option.value theme.added_line_number_bg ~default:transparent;
          content =
            Some (Option.value theme.added_content_bg ~default:theme.added_bg);
        }
    | Removed ->
        {
          gutter =
            Option.value theme.removed_line_number_bg ~default:transparent;
          content =
            Some
              (Option.value theme.removed_content_bg ~default:theme.removed_bg);
        }
    | Context ->
        {
          gutter = Option.value theme.line_number_bg ~default:transparent;
          content =
            (match theme.context_content_bg with
            | Some _ as c -> c
            | None -> Some (Option.value theme.context_bg ~default:transparent));
        }

  let after_sign ~color text : Line_number.line_sign =
    {
      before = None;
      after = Some text;
      before_color = None;
      after_color = Some color;
    }

  let added_sign theme = after_sign ~color:theme.added_sign_color " +"
  let removed_sign theme = after_sign ~color:theme.removed_sign_color " -"

  type unified = {
    content : string;
    line_colors : (int * Line_number.line_color) list;
    line_signs : (int * Line_number.line_sign) list;
    line_numbers : (int * int) list;
  }

  let unified ~theme (patch : Patch.t) =
    let buf = Buffer.create 256 in
    let line_colors = ref [] in
    let line_signs = ref [] in
    let line_numbers = ref [] in
    let line_index = ref 0 in
    let push_line tag line_no (line : Patch.line) =
      let index = !line_index in
      if index > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf line.content;
      line_colors := (index, line_color_of ~theme tag) :: !line_colors;
      line_numbers := (index, line_no) :: !line_numbers;
      (match tag with
      | Patch.Added -> line_signs := (index, added_sign theme) :: !line_signs
      | Patch.Removed ->
          line_signs := (index, removed_sign theme) :: !line_signs
      | Patch.Context -> ());
      incr line_index
    in
    List.iter
      (fun (hunk : Patch.hunk) ->
        let old_line = ref hunk.old_start in
        let new_line = ref hunk.new_start in
        List.iter
          (fun (line : Patch.line) ->
            match line.tag with
            | Added ->
                push_line Added !new_line line;
                incr new_line
            | Removed ->
                push_line Removed !old_line line;
                incr old_line
            | Context ->
                push_line Context !new_line line;
                incr old_line;
                incr new_line)
          hunk.lines)
      (Patch.hunks patch);
    {
      content = Buffer.contents buf;
      line_colors = List.rev !line_colors;
      line_signs = List.rev !line_signs;
      line_numbers = List.rev !line_numbers;
    }

  type split_kind = Context | Added | Removed | Blank

  type split_line = {
    content : string;
    line_num : int option;
    kind : split_kind;
  }

  type split = { left : split_line list; right : split_line list }

  let blank_line = { content = ""; line_num = None; kind = Blank }

  let context_line ~line_num content =
    { content; line_num = Some line_num; kind = Context }

  let split_run_lines (run : Patch.line array) =
    let removes = ref [] in
    let adds = ref [] in
    Array.iter
      (fun (line : Patch.line) ->
        match line.tag with
        | Removed -> removes := line :: !removes
        | Added -> adds := line :: !adds
        | Context -> ())
      run;
    (List.rev !removes, List.rev !adds)

  let pair_with_blanks a b =
    let rec loop acc_a acc_b a b =
      match (a, b) with
      | [], [] -> (List.rev acc_a, List.rev acc_b)
      | x :: xs, y :: ys -> loop (x :: acc_a) (y :: acc_b) xs ys
      | x :: xs, [] -> loop (x :: acc_a) (blank_line :: acc_b) xs []
      | [], y :: ys -> loop (blank_line :: acc_a) (y :: acc_b) [] ys
    in
    loop [] [] a b

  let split (patch : Patch.t) =
    let left = ref [] in
    let right = ref [] in
    let push_left line = left := line :: !left in
    let push_right line = right := line :: !right in
    List.iter
      (fun (hunk : Patch.hunk) ->
        let old_line = ref hunk.old_start in
        let new_line = ref hunk.new_start in
        let lines = Array.of_list hunk.lines in
        let len = Array.length lines in
        let i = ref 0 in
        while !i < len do
          match lines.(!i).tag with
          | Context ->
              let content = lines.(!i).content in
              push_left (context_line ~line_num:!old_line content);
              push_right (context_line ~line_num:!new_line content);
              incr old_line;
              incr new_line;
              incr i
          | Added | Removed ->
              let start = !i in
              while !i < len && lines.(!i).tag <> Context do
                incr i
              done;
              let run = Array.sub lines start (!i - start) in
              let removes, adds = split_run_lines run in
              let lefts =
                List.map
                  (fun (line : Patch.line) ->
                    let line_num = !old_line in
                    incr old_line;
                    {
                      content = line.content;
                      line_num = Some line_num;
                      kind = Removed;
                    })
                  removes
              in
              let rights =
                List.map
                  (fun (line : Patch.line) ->
                    let line_num = !new_line in
                    incr new_line;
                    {
                      content = line.content;
                      line_num = Some line_num;
                      kind = Added;
                    })
                  adds
              in
              let lefts, rights = pair_with_blanks lefts rights in
              List.iter push_left lefts;
              List.iter push_right rights
        done)
      (Patch.hunks patch);
    { left = List.rev !left; right = List.rev !right }

  let content lines =
    String.concat "\n"
      (List.map (fun (line : split_line) -> line.content) lines)

  let line_number_props ~theme ~show_line_numbers lines =
    let line_colors = ref [] in
    let line_signs = ref [] in
    let line_numbers = ref [] in
    let hidden_line_numbers = ref [] in
    List.iteri
      (fun index (line : split_line) ->
        (match line.line_num with
        | Some line_num -> line_numbers := (index, line_num) :: !line_numbers
        | None -> hidden_line_numbers := index :: !hidden_line_numbers);
        match line.kind with
        | Added ->
            line_colors :=
              (index, line_color_of ~theme Patch.Added) :: !line_colors;
            line_signs := (index, added_sign theme) :: !line_signs
        | Removed ->
            line_colors :=
              (index, line_color_of ~theme Patch.Removed) :: !line_colors;
            line_signs := (index, removed_sign theme) :: !line_signs
        | Context ->
            line_colors :=
              (index, line_color_of ~theme Patch.Context) :: !line_colors
        | Blank -> ())
      lines;
    Line_number.Props.make ~fg:theme.line_number_fg ?bg:theme.line_number_bg
      ~show_line_numbers ~line_colors:(List.rev !line_colors)
      ~line_signs:(List.rev !line_signs) ~line_numbers:(List.rev !line_numbers)
      ~hidden_line_numbers:(List.rev !hidden_line_numbers)
      ()
end

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable left_side : Line_number.t option;
  mutable right_side : Line_number.t option;
}

let node t = t.node
let patch t = t.props.patch

let full_style =
  let pct100 = Toffee.Style.Dimension.pct 100. in
  Toffee.Style.make ~size:(Toffee.Geometry.Size.make pct100 pct100) ()

let half_style =
  let pct50 = Toffee.Style.Dimension.pct 50. in
  let pct100 = Toffee.Style.Dimension.pct 100. in
  Toffee.Style.make ~size:(Toffee.Geometry.Size.make pct50 pct100) ()

let set_flex_direction t direction =
  Renderable.set_style t.node
    (Toffee.Style.set_flex_direction direction (Renderable.style t.node))

let destroy_side = function
  | None -> ()
  | Some side -> Renderable.destroy_recursively (Line_number.node side)

let destroy_children t =
  destroy_side t.left_side;
  destroy_side t.right_side;
  t.left_side <- None;
  t.right_side <- None

let make_code ~parent (props : Props.t) ~content ?syntax () =
  Code.create ~parent ~style:full_style ~content ?syntax
    ~text_style:props.text_style ~wrap:props.wrap ~selectable:props.selectable
    ()

let old_syntax = function Some { old; _ } -> Some old | None -> None
let new_syntax = function Some { new_; _ } -> Some new_ | None -> None

let make_side ~parent ~style ~theme ~props =
  let side =
    Line_number.create ~parent ~style ~fg:theme.line_number_fg
      ?bg:theme.line_number_bg ()
  in
  Line_number.apply_props side props;
  side

let build_unified_view t (props : Props.t) =
  destroy_children t;
  set_flex_direction t Toffee.Style.Flex_direction.Column;
  let unified = View.unified ~theme:props.theme props.patch in
  let side =
    Line_number.create ~parent:t.node ~style:full_style
      ~fg:props.theme.line_number_fg ?bg:props.theme.line_number_bg
      ~show_line_numbers:props.show_line_numbers
      ~line_colors:unified.View.line_colors ~line_signs:unified.View.line_signs
      ~line_numbers:unified.View.line_numbers ()
  in
  let _code =
    Code.create ~parent:(Line_number.node side) ~style:full_style
      ~content:unified.View.content ~text_style:props.text_style
      ~wrap:props.wrap ~selectable:props.selectable ()
  in
  t.left_side <- Some side

let build_split_view t (props : Props.t) =
  destroy_children t;
  set_flex_direction t Toffee.Style.Flex_direction.Row;
  let split = View.split props.patch in
  let left_props =
    View.line_number_props ~theme:props.theme
      ~show_line_numbers:props.show_line_numbers split.View.left
  in
  let right_props =
    View.line_number_props ~theme:props.theme
      ~show_line_numbers:props.show_line_numbers split.View.right
  in
  let left_side =
    make_side ~parent:t.node ~style:half_style ~theme:props.theme
      ~props:left_props
  in
  let _left_code =
    make_code
      ~parent:(Line_number.node left_side)
      props
      ~content:(View.content split.View.left)
      ?syntax:(old_syntax props.highlight)
      ()
  in
  let right_side =
    make_side ~parent:t.node ~style:half_style ~theme:props.theme
      ~props:right_props
  in
  let _right_code =
    make_code
      ~parent:(Line_number.node right_side)
      props
      ~content:(View.content split.View.right)
      ?syntax:(new_syntax props.highlight)
      ()
  in
  t.left_side <- Some left_side;
  t.right_side <- Some right_side

let rebuild t =
  if Patch.is_empty t.props.patch then begin
    destroy_children t;
    set_flex_direction t
      (match t.props.layout with
      | Unified -> Toffee.Style.Flex_direction.Column
      | Split -> Toffee.Style.Flex_direction.Row)
  end
  else
    match t.props.layout with
    | Unified -> build_unified_view t t.props
    | Split -> build_split_view t t.props

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?layout ?theme
    ?highlight ?show_line_numbers ?wrap ?selectable ?text_style patch =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ~patch ?layout ?theme ?highlight ?show_line_numbers ?wrap
      ?selectable ?text_style ()
  in
  let t = { node; props; left_side = None; right_side = None } in
  rebuild t;
  t

let update t props =
  if not (Props.equal t.props props) then begin
    t.props <- props;
    rebuild t;
    Renderable.request_render t.node
  end

let set_patch t patch = update t { t.props with patch }
let set_layout t layout = update t { t.props with layout }
let set_theme t theme = update t { t.props with theme }
let set_highlight t highlight = update t { t.props with highlight }
let apply_props = update
