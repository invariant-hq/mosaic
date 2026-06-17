module Patch = Diff_patch

type layout = Unified | Split
type side = Old | New

type line_highlight = {
  side : side;
  first : int;
  last : int;
  color : Line_number.line_color;
}

type line_sign = {
  side : side;
  first : int;
  last : int;
  sign : Line_number.line_sign;
}

type source_line = { side : side; line : int }
type line_kind = Context | Added | Removed | Blank
type hit_region = Gutter | Sign | Content | Padding

type line_hit = {
  source : source_line option;
  kind : line_kind;
  region : hit_region;
  logical_row : int;
  visual_row : int;
}

let equal_side a b =
  match (a, b) with Old, Old | New, New -> true | _ -> false

let equal_line_color (a : Line_number.line_color) (b : Line_number.line_color) =
  Ansi.Color.equal a.gutter b.gutter
  && Option.equal Ansi.Color.equal a.content b.content

let equal_line_highlight (a : line_highlight) (b : line_highlight) =
  equal_side a.side b.side && a.first = b.first && a.last = b.last
  && equal_line_color a.color b.color

let equal_line_sign_value (a : Line_number.line_sign)
    (b : Line_number.line_sign) =
  Option.equal String.equal a.before b.before
  && Option.equal String.equal a.after b.after
  && Option.equal Ansi.Color.equal a.before_color b.before_color
  && Option.equal Ansi.Color.equal a.after_color b.after_color

let equal_line_sign (a : line_sign) (b : line_sign) =
  equal_side a.side b.side && a.first = b.first && a.last = b.last
  && equal_line_sign_value a.sign b.sign

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

type syntax = {
  language : string;
  style : Syntax_style.t;
  highlighter : Code.Highlighter.t;
  conceal : bool;
  draw_unstyled : bool;
  streaming : bool;
}

let syntax ~language ?(style = Syntax_style.default) ?(conceal = true)
    ?(draw_unstyled = true) ?(streaming = false) highlighter =
  { language; style; highlighter; conceal; draw_unstyled; streaming }

let syntax_equal a b =
  String.equal a.language b.language
  && a.style == b.style
  && a.highlighter == b.highlighter
  && Bool.equal a.conceal b.conceal
  && Bool.equal a.draw_unstyled b.draw_unstyled
  && Bool.equal a.streaming b.streaming

let code_syntax ?draw_unstyled syntax =
  let draw_unstyled =
    Option.value draw_unstyled ~default:syntax.draw_unstyled
  in
  Code.with_highlighter ~language:syntax.language ~style:syntax.style
    ~conceal:syntax.conceal ~draw_unstyled ~streaming:syntax.streaming
    syntax.highlighter

type highlight = { old : syntax; new_ : syntax }

let highlight_equal a b = syntax_equal a.old b.old && syntax_equal a.new_ b.new_

module Props = struct
  type t = {
    patch : Patch.t;
    layout : layout;
    theme : theme;
    highlight : highlight option;
    line_highlights : line_highlight list;
    line_signs : line_sign list;
    show_line_numbers : bool;
    wrap : Text_surface.wrap;
    selectable : bool;
    text_style : Ansi.Style.t;
  }

  let empty_patch = Patch.empty

  let make ?(patch = empty_patch) ?(layout = Unified) ?(theme = default_theme)
      ?highlight ?(line_highlights = []) ?(line_signs = [])
      ?(show_line_numbers = true) ?(wrap = `None) ?(selectable = true)
      ?(text_style = Ansi.Style.default) () =
    {
      patch;
      layout;
      theme;
      highlight;
      line_highlights;
      line_signs;
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
    && List.equal equal_line_highlight a.line_highlights b.line_highlights
    && List.equal equal_line_sign a.line_signs b.line_signs
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

  type source_line = { side : side; number : int }

  let highlight_matches source (highlight : line_highlight) =
    source.side = highlight.side
    && source.number >= highlight.first
    && source.number <= highlight.last

  let sign_matches source (sign : line_sign) =
    source.side = sign.side
    && source.number >= sign.first
    && source.number <= sign.last

  let blend_channel base overlay alpha =
    Float.round
      ((float_of_int overlay *. alpha) +. (float_of_int base *. (1. -. alpha)))
    |> int_of_float

  let blend_color ~base overlay =
    let _, _, _, overlay_alpha = Ansi.Color.to_rgba overlay in
    if overlay_alpha <= 0 then base
    else if overlay_alpha >= 255 then overlay
    else
      let _, _, _, base_alpha = Ansi.Color.to_rgba base in
      if base_alpha <= 0 then overlay
      else
        let br, bg, bb = Ansi.Color.to_rgb base in
        let or_, og, ob = Ansi.Color.to_rgb overlay in
        let alpha = float_of_int overlay_alpha /. 255. in
        Ansi.Color.of_rgb
          (blend_channel br or_ alpha)
          (blend_channel bg og alpha)
          (blend_channel bb ob alpha)

  let blend_content_color (base : Line_number.line_color)
      (overlay : Line_number.line_color) =
    match (base.content, overlay.content) with
    | None, None -> None
    | Some base, None -> Some (blend_color ~base overlay.gutter)
    | None, Some overlay -> Some overlay
    | Some base, Some overlay -> Some (blend_color ~base overlay)

  let blend_line_color ~(base : Line_number.line_color)
      (overlay : Line_number.line_color) : Line_number.line_color =
    {
      gutter = blend_color ~base:base.gutter overlay.gutter;
      content = blend_content_color base overlay;
    }

  let find_highlight sources highlights =
    List.find_map
      (fun highlight ->
        if
          List.exists (fun source -> highlight_matches source highlight) sources
        then Some highlight.color
        else None)
      highlights

  let line_color_of_sources ~theme ~line_highlights tag sources =
    let base = line_color_of ~theme tag in
    match find_highlight sources line_highlights with
    | Some color -> blend_line_color ~base color
    | None -> base

  let find_sign sources signs =
    List.find_map
      (fun sign ->
        if List.exists (fun source -> sign_matches source sign) sources then
          Some sign.sign
        else None)
      signs

  let merge_sign custom builtin =
    match (custom, builtin) with
    | None, None -> None
    | Some sign, None | None, Some sign -> Some sign
    | Some (custom : Line_number.line_sign), Some builtin ->
        Some
          {
            before = custom.before;
            before_color = custom.before_color;
            after =
              (match builtin.after with
              | Some _ as after -> after
              | None -> custom.after);
            after_color =
              (match builtin.after_color with
              | Some _ as color -> color
              | None -> custom.after_color);
          }

  let line_sign_of_sources ~line_signs ~builtin sources =
    merge_sign (find_sign sources line_signs) builtin

  type unified = {
    content : string;
    line_colors : (int * Line_number.line_color) list;
    line_signs : (int * Line_number.line_sign) list;
    line_numbers : (int * int) list;
  }

  let unified ~theme ~line_highlights ~line_signs (patch : Patch.t) =
    let buf = Buffer.create 256 in
    let line_colors = ref [] in
    let line_sign_rows = ref [] in
    let line_numbers = ref [] in
    let line_index = ref 0 in
    let push_line tag line_no sources (line : Patch.line) =
      let index = !line_index in
      if index > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf line.content;
      line_colors :=
        (index, line_color_of_sources ~theme ~line_highlights tag sources)
        :: !line_colors;
      line_numbers := (index, line_no) :: !line_numbers;
      let builtin =
        match tag with
        | Patch.Added -> Some (added_sign theme)
        | Patch.Removed -> Some (removed_sign theme)
        | Patch.Context -> None
      in
      (match line_sign_of_sources ~line_signs ~builtin sources with
      | None -> ()
      | Some sign -> line_sign_rows := (index, sign) :: !line_sign_rows);
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
                push_line Added !new_line
                  [ { side = New; number = !new_line } ]
                  line;
                incr new_line
            | Removed ->
                push_line Removed !old_line
                  [ { side = Old; number = !old_line } ]
                  line;
                incr old_line
            | Context ->
                push_line Context !new_line
                  [
                    { side = Old; number = !old_line };
                    { side = New; number = !new_line };
                  ]
                  line;
                incr old_line;
                incr new_line)
          hunk.lines)
      (Patch.hunks patch);
    {
      content = Buffer.contents buf;
      line_colors = List.rev !line_colors;
      line_signs = List.rev !line_sign_rows;
      line_numbers = List.rev !line_numbers;
    }

  type split_kind = Context | Added | Removed | Blank

  type split_line = {
    content : string;
    line_num : int option;
    side : side option;
    kind : split_kind;
  }

  type split = { left : split_line list; right : split_line list }

  let blank_line = { content = ""; line_num = None; side = None; kind = Blank }

  let context_line ~side ~line_num content =
    { content; line_num = Some line_num; side = Some side; kind = Context }

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
              push_left (context_line ~side:Old ~line_num:!old_line content);
              push_right (context_line ~side:New ~line_num:!new_line content);
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
                      side = Some Old;
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
                      side = Some New;
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

  let line_number_props ~theme ~line_highlights ~line_signs ~show_line_numbers
      lines =
    let line_colors = ref [] in
    let line_sign_rows = ref [] in
    let line_numbers = ref [] in
    let hidden_line_numbers = ref [] in
    List.iteri
      (fun index (line : split_line) ->
        (match line.line_num with
        | Some line_num -> line_numbers := (index, line_num) :: !line_numbers
        | None -> hidden_line_numbers := index :: !hidden_line_numbers);
        let color tag =
          let sources =
            match (line.side, line.line_num) with
            | Some side, Some number -> [ { side; number } ]
            | Some _, None | None, Some _ | None, None -> []
          in
          line_color_of_sources ~theme ~line_highlights tag sources
        in
        let sources =
          match (line.side, line.line_num) with
          | Some side, Some number -> [ { side; number } ]
          | Some _, None | None, Some _ | None, None -> []
        in
        let custom_sign builtin =
          line_sign_of_sources ~line_signs ~builtin sources
        in
        match line.kind with
        | Added -> (
            line_colors := (index, color Patch.Added) :: !line_colors;
            match custom_sign (Some (added_sign theme)) with
            | None -> ()
            | Some sign -> line_sign_rows := (index, sign) :: !line_sign_rows)
        | Removed -> (
            line_colors := (index, color Patch.Removed) :: !line_colors;
            match custom_sign (Some (removed_sign theme)) with
            | None -> ()
            | Some sign -> line_sign_rows := (index, sign) :: !line_sign_rows)
        | Context -> (
            line_colors := (index, color Patch.Context) :: !line_colors;
            match custom_sign None with
            | None -> ()
            | Some sign -> line_sign_rows := (index, sign) :: !line_sign_rows)
        | Blank -> ())
      lines;
    Line_number.Props.make ~fg:theme.line_number_fg ?bg:theme.line_number_bg
      ~show_line_numbers ~line_colors:(List.rev !line_colors)
      ~line_signs:(List.rev !line_sign_rows)
      ~line_numbers:(List.rev !line_numbers)
      ~hidden_line_numbers:(List.rev !hidden_line_numbers)
      ()
end

let line_matches source (line : View.source_line) =
  source.side = line.side && source.line = line.number

let source_line_row_unified patch source =
  let row = ref 0 in
  let result = ref None in
  List.iter
    (fun (hunk : Patch.hunk) ->
      let old_line = ref hunk.old_start in
      let new_line = ref hunk.new_start in
      List.iter
        (fun (line : Patch.line) ->
          (if Option.is_none !result then
             let sources =
               match line.tag with
               | Added -> [ { View.side = New; number = !new_line } ]
               | Removed -> [ { View.side = Old; number = !old_line } ]
               | Context ->
                   [
                     { View.side = Old; number = !old_line };
                     { View.side = New; number = !new_line };
                   ]
             in
             if List.exists (line_matches source) sources then
               result := Some !row);
          (match line.tag with
          | Added -> incr new_line
          | Removed -> incr old_line
          | Context ->
              incr old_line;
              incr new_line);
          incr row)
        hunk.lines)
    (Patch.hunks patch);
  !result

let source_line_row_split patch source =
  let split = View.split patch in
  let find lines =
    let rec loop row = function
      | [] -> None
      | line :: rest -> (
          match (line.View.side, line.line_num) with
          | Some side, Some number
            when source.side = side && source.line = number ->
              Some row
          | Some _, Some _ | Some _, None | None, Some _ | None, None ->
              loop (row + 1) rest)
    in
    loop 0 lines
  in
  match find split.left with
  | Some _ as location -> location
  | None -> find split.right

let source_line_row patch ~layout source =
  match layout with
  | Unified -> source_line_row_unified patch source
  | Split -> source_line_row_split patch source

let line_kind_of_patch_tag = function
  | Patch.Context -> Context
  | Patch.Added -> Added
  | Patch.Removed -> Removed

let line_kind_of_split_kind = function
  | View.Context -> Context
  | Added -> Added
  | Removed -> Removed
  | Blank -> Blank

let source_line_at_row_unified patch target =
  let row = ref 0 in
  let result = ref None in
  List.iter
    (fun (hunk : Patch.hunk) ->
      let old_line = ref hunk.old_start in
      let new_line = ref hunk.new_start in
      List.iter
        (fun (line : Patch.line) ->
          (if Option.is_none !result && !row = target then
             let source =
               match line.tag with
               | Patch.Added -> Some { side = New; line = !new_line }
               | Patch.Removed -> Some { side = Old; line = !old_line }
               | Patch.Context -> Some { side = New; line = !new_line }
             in
             result := Some (line_kind_of_patch_tag line.tag, source));
          (match line.tag with
          | Patch.Added -> incr new_line
          | Patch.Removed -> incr old_line
          | Patch.Context ->
              incr old_line;
              incr new_line);
          incr row)
        hunk.lines)
    (Patch.hunks patch);
  !result

let split_line_at_row lines target =
  if target < 0 then None
  else
    let rec loop row = function
      | [] -> None
      | line :: rest ->
          if row = target then
            let source =
              match (line.View.side, line.line_num) with
              | Some side, Some number -> Some { side; line = number }
              | Some _, None | None, Some _ | None, None -> None
            in
            Some (line_kind_of_split_kind line.kind, source)
          else loop (row + 1) rest
    in
    loop 0 lines

let source_line_at_row_split patch ?side row =
  let split = View.split patch in
  match side with
  | Some Old -> split_line_at_row split.left row
  | Some New -> split_line_at_row split.right row
  | None -> (
      match split_line_at_row split.left row with
      | Some (Blank, None) | None -> split_line_at_row split.right row
      | Some _ as result -> result)

let source_line_at_row_with_kind patch ~layout ?side row =
  if row < 0 then None
  else
    match layout with
    | Unified -> source_line_at_row_unified patch row
    | Split -> source_line_at_row_split patch ?side row

module Split_layout = struct
  let visual_counts line_count (info : Renderable.line_info) =
    let counts = Array.make line_count 0 in
    Array.iter
      (fun source ->
        if source >= 0 && source < line_count then
          counts.(source) <- counts.(source) + 1)
      info.line_sources;
    for i = 0 to line_count - 1 do
      if counts.(i) = 0 then counts.(i) <- 1
    done;
    counts

  let pad acc n =
    let rec loop acc n =
      if n = 0 then acc else loop (View.blank_line :: acc) (n - 1)
    in
    loop acc n

  let align (split : View.split) ~left_info ~right_info =
    let left_count = List.length split.left in
    let right_count = List.length split.right in
    let left_visual_counts = visual_counts left_count left_info in
    let right_visual_counts = visual_counts right_count right_info in
    let rec loop acc_left acc_right left_visual right_visual i left right =
      match (left, right) with
      | [], [] ->
          if left_visual < right_visual then
            {
              View.left = List.rev (pad acc_left (right_visual - left_visual));
              right = List.rev acc_right;
            }
          else if right_visual < left_visual then
            {
              left = List.rev acc_left;
              right = List.rev (pad acc_right (left_visual - right_visual));
            }
          else { left = List.rev acc_left; right = List.rev acc_right }
      | left_line :: left_tail, right_line :: right_tail ->
          let acc_left, left_visual =
            if left_visual < right_visual then
              let n = right_visual - left_visual in
              (pad acc_left n, left_visual + n)
            else (acc_left, left_visual)
          in
          let acc_right, right_visual =
            if right_visual < left_visual then
              let n = left_visual - right_visual in
              (pad acc_right n, right_visual + n)
            else (acc_right, right_visual)
          in
          let left_visual =
            left_visual + if i < left_count then left_visual_counts.(i) else 1
          in
          let right_visual =
            right_visual
            + if i < right_count then right_visual_counts.(i) else 1
          in
          loop (left_line :: acc_left) (right_line :: acc_right) left_visual
            right_visual (i + 1) left_tail right_tail
      | _ -> split
    in
    loop [] [] 0 0 0 split.left split.right
end

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable left_side : Line_number.t option;
  mutable right_side : Line_number.t option;
  mutable left_code : Code.t option;
  mutable right_code : Code.t option;
  mutable left_lines : View.split_line list;
  mutable right_lines : View.split_line list;
  mutable on_line_click : (line_hit -> unit) option;
  mutable mouse_down : (int * int) option;
  mutable pending_rebuild : bool;
  mutable waiting_for_highlight : bool;
}

let node t = t.node
let patch t = t.props.patch

let inside_side ~side_node x =
  let side_x = Renderable.x side_node in
  let side_w = Renderable.width side_node in
  x >= side_x && x < side_x + side_w

let region_of_x ~code_node kind x =
  let code_x = Renderable.x code_node in
  let code_w = Renderable.width code_node in
  if x >= code_x && x < code_x + code_w then Content
  else if x = code_x - 1 then Padding
  else
    match kind with
    | (Added | Removed) when x >= code_x - 3 && x < code_x - 1 -> Sign
    | Context | Added | Removed | Blank -> Gutter

let logical_row_at_visual code visual_row =
  if visual_row < 0 then None
  else
    match Renderable.line_info (Code.node code) with
    | Some info ->
        let display_row = info.scroll_y + visual_row in
        if display_row < 0 || display_row >= Array.length info.line_sources then
          None
        else Some info.line_sources.(display_row)
    | None -> Some visual_row

let side_hit t ~x ~y side line_number code =
  let side_node = Line_number.node line_number in
  let code_node = Code.node code in
  if not (inside_side ~side_node x) then None
  else
    let visual_row = y - Renderable.y code_node in
    Option.bind (logical_row_at_visual code visual_row) (fun logical_row ->
        let line =
          match t.props.layout with
          | Unified ->
              source_line_at_row_with_kind t.props.patch ~layout:Unified
                logical_row
          | Split ->
              let lines =
                match side with Old -> t.left_lines | New -> t.right_lines
              in
              split_line_at_row lines logical_row
        in
        match line with
        | None -> None
        | Some (kind, source) ->
            let region = region_of_x ~code_node kind x in
            Some { source; kind; region; logical_row; visual_row })

let hit_test t ~x ~y =
  let nx = Renderable.x t.node in
  let ny = Renderable.y t.node in
  let nw = Renderable.width t.node in
  let nh = Renderable.height t.node in
  if x < nx || x >= nx + nw || y < ny || y >= ny + nh then None
  else
    match t.props.layout with
    | Unified -> (
        match (t.left_side, t.left_code) with
        | Some side, Some code -> side_hit t ~x ~y New side code
        | _ -> None)
    | Split -> (
        match (t.left_side, t.left_code, t.right_side, t.right_code) with
        | Some left_side, Some left_code, Some right_side, Some right_code -> (
            match side_hit t ~x ~y Old left_side left_code with
            | Some _ as hit -> hit
            | None -> side_hit t ~x ~y New right_side right_code)
        | _ -> None)

let click_distance_ok (down_x, down_y) ~x ~y =
  abs (x - down_x) <= 1 && abs (y - down_y) <= 1

let handle_mouse t event =
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      t.mouse_down <- Some (Event.Mouse.x event, Event.Mouse.y event)
  | Up { button = Left; is_dragging = _ } -> (
      let x = Event.Mouse.x event in
      let y = Event.Mouse.y event in
      let down = t.mouse_down in
      t.mouse_down <- None;
      match down with
      | Some down when click_distance_ok down ~x ~y -> (
          match (t.on_line_click, hit_test t ~x ~y) with
          | Some on_line_click, Some hit ->
              on_line_click hit;
              Event.Mouse.stop_propagation event
          | Some _, None | None, Some _ | None, None -> ())
      | Some _ | None -> ())
  | Drag { button = Left; _ } | Drag_end { button = Left } ->
      t.mouse_down <- None
  | Down _ | Up _ | Move | Drag _ | Drag_end _ | Drop _ | Over _ | Out
  | Scroll _ ->
      ()

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

let destroy_code = function
  | None -> ()
  | Some code -> Code.set_on_line_info_change code None

let destroy_side = function
  | None -> ()
  | Some side -> Renderable.destroy_recursively (Line_number.node side)

let destroy_children t =
  destroy_code t.left_code;
  destroy_code t.right_code;
  destroy_side t.left_side;
  destroy_side t.right_side;
  t.left_side <- None;
  t.right_side <- None;
  t.left_code <- None;
  t.right_code <- None;
  t.left_lines <- [];
  t.right_lines <- [];
  t.pending_rebuild <- false;
  t.waiting_for_highlight <- false

let code_props (props : Props.t) ~content ?syntax () =
  Code.Props.make ~content ?syntax ~text_style:props.text_style ~wrap:props.wrap
    ~selectable:props.selectable ()

let make_code ~parent (props : Props.t) ~content ?syntax () =
  Code.create ~parent ~style:full_style ~content ?syntax
    ~text_style:props.text_style ~wrap:props.wrap ~selectable:props.selectable
    ()

let make_or_update_code existing ~parent (props : Props.t) ~content ?syntax () =
  match existing with
  | None -> make_code ~parent props ~content ?syntax ()
  | Some code ->
      Renderable.set_style (Code.node code) full_style;
      Code.apply_props code (code_props props ~content ?syntax ());
      code

let old_syntax = function Some { old; _ } -> Some old | None -> None
let new_syntax = function Some { new_; _ } -> Some new_ | None -> None

let needs_stable_concealment (props : Props.t) (syntax : syntax) =
  props.wrap <> `None && syntax.conceal

let code_syntax_for_split props syntax =
  let draw_unstyled =
    if needs_stable_concealment props syntax then Some false else None
  in
  code_syntax ?draw_unstyled syntax

let old_code_syntax props highlight =
  Option.map (code_syntax_for_split props) (old_syntax highlight)

let new_code_syntax props highlight =
  Option.map (code_syntax_for_split props) (new_syntax highlight)

let unified_code_syntax highlight =
  Option.map code_syntax (new_syntax highlight)

let make_side ~parent ~style ~theme ~props =
  let side =
    Line_number.create ~parent ~style ~fg:theme.line_number_fg
      ?bg:theme.line_number_bg ()
  in
  Line_number.apply_props side props;
  side

let make_or_update_side existing ~parent ~style ~theme ~props =
  match existing with
  | None -> make_side ~parent ~style ~theme ~props
  | Some side ->
      Renderable.set_style (Line_number.node side) style;
      Line_number.apply_props side props;
      side

let request_split_rebuild t =
  if t.props.layout = Split && t.props.wrap <> `None then begin
    t.pending_rebuild <- true;
    Renderable.set_live t.node true;
    Renderable.request_render t.node
  end

let set_code_line_info_callback t code =
  Code.set_on_line_info_change code
    (Some (fun () -> if t.waiting_for_highlight then request_split_rebuild t))

let update_waiting_for_highlight t (props : Props.t) left_code right_code =
  t.waiting_for_highlight <-
    props.wrap <> `None
    && Renderable.width t.node > 0
    && Renderable.width (Code.node left_code) > 0
    && Renderable.width (Code.node right_code) > 0
    && ((not (Code.line_info_stable left_code))
       || not (Code.line_info_stable right_code))

let pending_work t =
  if t.pending_rebuild then
    Some (Renderable.Pending.make ~kind:"diff.rebuild" ())
  else if t.waiting_for_highlight then
    Some (Renderable.Pending.make ~kind:"diff.highlight" ())
  else None

let build_unified_view t (props : Props.t) =
  destroy_children t;
  set_flex_direction t Toffee.Style.Flex_direction.Column;
  let unified =
    View.unified ~theme:props.theme ~line_highlights:props.line_highlights
      ~line_signs:props.line_signs props.patch
  in
  let side =
    Line_number.create ~parent:t.node ~style:full_style
      ~fg:props.theme.line_number_fg ?bg:props.theme.line_number_bg
      ~show_line_numbers:props.show_line_numbers
      ~line_colors:unified.View.line_colors ~line_signs:unified.View.line_signs
      ~line_numbers:unified.View.line_numbers ()
  in
  let code =
    Code.create ~parent:(Line_number.node side) ~style:full_style
      ~content:unified.View.content ~text_style:props.text_style
      ?syntax:(unified_code_syntax props.highlight)
      ~wrap:props.wrap ~selectable:props.selectable ()
  in
  t.left_side <- Some side;
  t.left_code <- Some code;
  t.left_lines <- [];
  t.right_lines <- []

let should_align_split t (props : Props.t) left_code right_code =
  props.wrap <> `None
  && Code.line_info_stable left_code
  && Code.line_info_stable right_code
  && Renderable.width t.node > 0
  && Renderable.width (Code.node left_code) > 0
  && Renderable.width (Code.node right_code) > 0

let build_split_view t (props : Props.t) =
  set_flex_direction t Toffee.Style.Flex_direction.Row;
  let initial = View.split props.patch in
  let initial_left_props =
    View.line_number_props ~theme:props.theme
      ~line_highlights:props.line_highlights ~line_signs:props.line_signs
      ~show_line_numbers:props.show_line_numbers initial.View.left
  in
  let initial_right_props =
    View.line_number_props ~theme:props.theme
      ~line_highlights:props.line_highlights ~line_signs:props.line_signs
      ~show_line_numbers:props.show_line_numbers initial.View.right
  in
  let left_side =
    make_or_update_side t.left_side ~parent:t.node ~style:half_style
      ~theme:props.theme ~props:initial_left_props
  in
  let left_code =
    make_or_update_code t.left_code
      ~parent:(Line_number.node left_side)
      props
      ~content:(View.content initial.View.left)
      ?syntax:(old_code_syntax props props.highlight)
      ()
  in
  let right_side =
    make_or_update_side t.right_side ~parent:t.node ~style:half_style
      ~theme:props.theme ~props:initial_right_props
  in
  let right_code =
    make_or_update_code t.right_code
      ~parent:(Line_number.node right_side)
      props
      ~content:(View.content initial.View.right)
      ?syntax:(new_code_syntax props props.highlight)
      ()
  in
  set_code_line_info_callback t left_code;
  set_code_line_info_callback t right_code;
  Renderable.set_on_resize (Code.node left_code)
    (Some (fun _ -> request_split_rebuild t));
  Renderable.set_on_resize (Code.node right_code)
    (Some (fun _ -> request_split_rebuild t));
  let split =
    match
      ( Renderable.line_info (Code.node left_code),
        Renderable.line_info (Code.node right_code) )
    with
    | Some left_info, Some right_info
      when should_align_split t props left_code right_code ->
        Split_layout.align initial ~left_info ~right_info
    | _ -> initial
  in
  let left_props =
    View.line_number_props ~theme:props.theme
      ~line_highlights:props.line_highlights ~line_signs:props.line_signs
      ~show_line_numbers:props.show_line_numbers split.View.left
  in
  let right_props =
    View.line_number_props ~theme:props.theme
      ~line_highlights:props.line_highlights ~line_signs:props.line_signs
      ~show_line_numbers:props.show_line_numbers split.View.right
  in
  Line_number.apply_props left_side left_props;
  Line_number.apply_props right_side right_props;
  Code.apply_props left_code
    (code_props props
       ~content:(View.content split.View.left)
       ?syntax:(old_code_syntax props props.highlight)
       ());
  Code.apply_props right_code
    (code_props props
       ~content:(View.content split.View.right)
       ?syntax:(new_code_syntax props props.highlight)
       ());
  update_waiting_for_highlight t props left_code right_code;
  t.left_side <- Some left_side;
  t.right_side <- Some right_side;
  t.left_code <- Some left_code;
  t.right_code <- Some right_code;
  t.left_lines <- split.View.left;
  t.right_lines <- split.View.right

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
    ?highlight ?line_highlights ?line_signs ?show_line_numbers ?wrap ?selectable
    ?text_style patch =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ~patch ?layout ?theme ?highlight ?line_highlights ?line_signs
      ?show_line_numbers ?wrap ?selectable ?text_style ()
  in
  let t =
    {
      node;
      props;
      left_side = None;
      right_side = None;
      left_code = None;
      right_code = None;
      left_lines = [];
      right_lines = [];
      on_line_click = None;
      mouse_down = None;
      pending_rebuild = false;
      waiting_for_highlight = false;
    }
  in
  Renderable.on_mouse node (handle_mouse t);
  Renderable.set_on_frame node
    (Some
       (fun _ ~delta:_ ->
         if t.pending_rebuild then begin
           t.pending_rebuild <- false;
           rebuild t;
           Renderable.request_render t.node
         end;
         if not t.pending_rebuild then begin
           Renderable.set_live t.node false
         end));
  Renderable.set_pending_provider node (Some (fun () -> pending_work t));
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

let set_line_highlights t line_highlights =
  update t { t.props with line_highlights }

let set_line_signs t line_signs = update t { t.props with line_signs }
let apply_props = update
let set_on_line_click t callback = t.on_line_click <- callback
