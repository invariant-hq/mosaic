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

type line_span = {
  side : side;
  line : int;
  start_byte : int;
  end_byte : int;
  color : Ansi.Color.t;
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

let equal_line_span (a : line_span) (b : line_span) =
  equal_side a.side b.side && a.line = b.line
  && a.start_byte = b.start_byte
  && a.end_byte = b.end_byte
  && Ansi.Color.equal a.color b.color

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
}

let syntax ~language ?(style = Syntax_style.default) ?(conceal = true)
    highlighter =
  { language; style; highlighter; conceal }

let syntax_equal a b =
  String.equal a.language b.language
  && Syntax_style.equal a.style b.style
  && a.highlighter == b.highlighter
  && Bool.equal a.conceal b.conceal

let code_syntax syntax =
  Code.with_highlighter ~language:syntax.language ~style:syntax.style
    syntax.highlighter

(* A pane without syntax has nothing to conceal. *)
let code_conceal = function Some syntax -> syntax.conceal | None -> true

type highlight = { old : syntax; new_ : syntax }

let highlight_equal a b = syntax_equal a.old b.old && syntax_equal a.new_ b.new_

(* [Props.make] is the one place the diff's labels get their defaults:
   [create] and [Vnode.diff] pass their labels on unset. *)
module Props = struct
  type t = {
    patch : Patch.t;
    layout : layout;
    theme : theme;
    highlight : highlight option;
    line_highlights : line_highlight list;
    line_spans : line_span list;
    line_signs : line_sign list;
    show_line_numbers : bool;
    wrap : Text_surface.wrap;
    selectable : bool;
    text_style : Ansi.Style.t;
  }

  let empty_patch = Patch.empty

  let make ?(patch = empty_patch) ?(layout = Unified) ?(theme = default_theme)
      ?highlight ?(line_highlights = []) ?(line_spans = []) ?(line_signs = [])
      ?(show_line_numbers = true) ?(wrap = `None) ?(selectable = true)
      ?(text_style = Ansi.Style.default) () =
    {
      patch;
      layout;
      theme;
      highlight;
      line_highlights;
      line_spans;
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
    && List.equal equal_line_span a.line_spans b.line_spans
    && List.equal equal_line_sign a.line_signs b.line_signs
    && a.show_line_numbers = b.show_line_numbers
    && a.wrap = b.wrap
    && a.selectable = b.selectable
    && Ansi.Style.equal a.text_style b.text_style
end

(* A pane's buffer text joins diff rows with LF; patch lines hold none, so
   each LF ends a row. The buffer's source index decides where logical lines
   start (RFC 0003 §5.1): a CR also ends one, so a row holding a lone CR spans
   two lines and a final CR adds an empty one. Rows carry the diff's metadata;
   logical lines and graphemes are buffer positions. This map is the only
   crossing between them. *)
module Pane = struct
  module Source = Matrix.Text.Layout.Source

  type t = {
    source : Source.t;
    row_starts : int array; (* The first byte of each row. *)
    line_rows : int array; (* The row holding each logical line. *)
  }

  let join contents = String.concat "\n" contents

  (* A line after an LF starts a row; a line after a lone CR continues one. *)
  let of_source source =
    let text = Source.text source in
    let line_rows = Array.make (Source.line_count source) 0 in
    let starts = ref [ 0 ] and row = ref 0 in
    for line = 1 to Array.length line_rows - 1 do
      let start = Source.byte_offset source (Source.line_start source line) in
      if text.[start - 1] = '\n' then begin
        starts := start :: !starts;
        incr row
      end;
      line_rows.(line) <- !row
    done;
    { source; row_starts = Array.of_list (List.rev !starts); line_rows }

  let row_count t = Array.length t.row_starts
  let line_count t = Array.length t.line_rows

  let row_of_line t line =
    if line < 0 || line >= line_count t then None else Some t.line_rows.(line)

  let starts_row t line =
    line = 0 || t.line_rows.(line - 1) <> t.line_rows.(line)

  (* The source graphemes of bytes [start_byte, end_byte) of [row]'s content,
     clamped to it, or [None] if empty. A boundary inside a cluster moves past
     it. *)
  let graphemes t ~row ~start_byte ~end_byte =
    let first = t.row_starts.(row) in
    let last =
      if row + 1 < row_count t then t.row_starts.(row + 1) - 1
      else String.length (Source.text t.source)
    in
    let grapheme byte =
      Source.grapheme_of_byte t.source ~affinity:Downstream
        (first + max 0 (min byte (last - first)))
    in
    let start = grapheme start_byte and stop = grapheme end_byte in
    if stop <= start then None else Some (start, stop)
end

module View = struct
  let transparent = Ansi.Color.transparent

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

  type gutter_row = {
    color : Line_number.line_color option;
    sign : Line_number.line_sign option;
    number : int option;
  }

  let gutter_row ~theme ~line_highlights ~line_signs tag sources number =
    match tag with
    | None -> { color = None; sign = None; number }
    | Some tag ->
        let builtin =
          match tag with
          | Patch.Added -> Some (added_sign theme)
          | Patch.Removed -> Some (removed_sign theme)
          | Patch.Context -> None
        in
        {
          color =
            Some (line_color_of_sources ~theme ~line_highlights tag sources);
          sign = line_sign_of_sources ~line_signs ~builtin sources;
          number;
        }

  type row = {
    content : string;
    sources : source_line list;
    gutter : gutter_row;
  }

  let unified ~theme ~line_highlights ~line_signs (patch : Patch.t) =
    let rows = ref [] in
    let push_line tag number sources (line : Patch.line) =
      let gutter =
        gutter_row ~theme ~line_highlights ~line_signs (Some tag) sources
          (Some number)
      in
      rows := { content = line.content; sources; gutter } :: !rows
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
    List.rev !rows

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

  let split_rows ~theme ~line_highlights ~line_signs lines =
    List.map
      (fun (line : split_line) ->
        let sources =
          match (line.side, line.line_num) with
          | Some side, Some number -> [ { side; number } ]
          | Some _, None | None, Some _ | None, None -> []
        in
        let tag =
          match line.kind with
          | Added -> Some Patch.Added
          | Removed -> Some Patch.Removed
          | Context -> Some Patch.Context
          | Blank -> None
        in
        {
          content = line.content;
          sources;
          gutter =
            gutter_row ~theme ~line_highlights ~line_signs tag sources
              line.line_num;
        })
      lines

  type emphasis = (int * int * Ansi.Color.t) list

  (* Grapheme ranges of the spans on each row, in row then span order. *)
  let emphasis ~line_spans pane rows =
    let ranges = ref [] in
    Array.iteri
      (fun row ({ sources; _ } : row) ->
        List.iter
          (fun (span : line_span) ->
            if
              List.exists
                (fun (source : source_line) ->
                  source.side = span.side && source.number = span.line)
                sources
            then
              match
                Pane.graphemes pane ~row ~start_byte:span.start_byte
                  ~end_byte:span.end_byte
              with
              | Some (start, stop) ->
                  ranges := (start, stop, span.color) :: !ranges
              | None -> ())
          line_spans)
      rows;
    List.rev !ranges

  (* Gutter entries keyed by the pane's logical lines. A row's later lines,
     from a CR in its content, keep its color and hide their number. *)
  let gutter_props ~theme ~show_line_numbers pane (rows : row array) =
    let colors = ref [] and signs = ref [] in
    let numbers = ref [] and hidden = ref [] in
    for line = Pane.line_count pane - 1 downto 0 do
      match Pane.row_of_line pane line with
      | Some row when row < Array.length rows ->
          let gutter = rows.(row).gutter in
          Option.iter (fun c -> colors := (line, c) :: !colors) gutter.color;
          if not (Pane.starts_row pane line) then hidden := line :: !hidden
          else begin
            Option.iter (fun s -> signs := (line, s) :: !signs) gutter.sign;
            match gutter.number with
            | Some n -> numbers := (line, n) :: !numbers
            | None -> hidden := line :: !hidden
          end
      | Some _ | None -> ()
    done;
    Line_number.Props.make ~fg:theme.line_number_fg ?bg:theme.line_number_bg
      ~show_line_numbers ~line_colors:!colors ~line_signs:!signs
      ~line_numbers:!numbers ~hidden_line_numbers:!hidden ()

  let text (rows : row array) =
    Pane.join (Array.to_list (Array.map (fun (row : row) -> row.content) rows))
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
  (* Visual rows per diff row. This traverses the pane's complete layout, which
     RFC 0003 §5.4 allows when pane geometry changes. *)
  let visual_counts pane code =
    let counts = Array.make (Pane.row_count pane) 0 in
    Option.iter
      (fun (rows : Renderable.text_rows) ->
        Matrix.Text.Layout.iter_rows rows.layout ~first:0 ~count:max_int
          (fun ~row:_ ~logical_line ~continuation:_ ->
            match Pane.row_of_line pane logical_line with
            | Some row -> counts.(row) <- counts.(row) + 1
            | None -> ()))
      (Renderable.text_rows (Code.node code));
    Array.map (max 1) counts

  let pad acc n =
    let rec loop acc n =
      if n = 0 then acc else loop (View.blank_line :: acc) (n - 1)
    in
    loop acc n

  let align (split : View.split) ~left_visual_counts ~right_visual_counts =
    let left_count = Array.length left_visual_counts in
    let right_count = Array.length right_visual_counts in
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

(* A split aligned for [left_text] and [right_text], the unaligned pane texts,
   from their visual rows at these pane widths under these settings. *)
type alignment = {
  left_text : string;
  right_text : string;
  left_width : int;
  right_width : int;
  wrap : Text_surface.wrap;
  highlight : highlight option;
  aligned : View.split;
}

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable left_side : Line_number.t option;
  mutable right_side : Line_number.t option;
  mutable left_code : Code.t option;
  mutable right_code : Code.t option;
  mutable left_pane : Pane.t option;
  mutable right_pane : Pane.t option;
  mutable left_lines : View.split_line list;
  mutable right_lines : View.split_line list;
  mutable on_line_click : (line_hit -> unit) option;
  mutable mouse_down : (int * int) option;
  mutable pending_rebuild : bool;
  mutable waiting_for_highlight : bool;
  mutable alignment : alignment option;
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

let row_at_visual code pane visual_row =
  if visual_row < 0 then None
  else
    match Renderable.text_rows (Code.node code) with
    | Some rows ->
        let row = ref None in
        Matrix.Text.Layout.iter_rows rows.layout
          ~first:(rows.scroll_y + visual_row) ~count:1
          (fun ~row:_ ~logical_line ~continuation:_ ->
            row := Pane.row_of_line pane logical_line);
        !row
    | None -> Some visual_row

let side_hit t ~x ~y side line_number code pane =
  let side_node = Line_number.node line_number in
  let code_node = Code.node code in
  if not (inside_side ~side_node x) then None
  else
    let visual_row = y - Renderable.y code_node in
    Option.bind (row_at_visual code pane visual_row) (fun logical_row ->
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
        match (t.left_side, t.left_code, t.left_pane) with
        | Some side, Some code, Some pane -> side_hit t ~x ~y New side code pane
        | _ -> None)
    | Split -> (
        match
          ( (t.left_side, t.left_code, t.left_pane),
            (t.right_side, t.right_code, t.right_pane) )
        with
        | ( (Some left_side, Some left_code, Some left_pane),
            (Some right_side, Some right_code, Some right_pane) ) -> (
            match side_hit t ~x ~y Old left_side left_code left_pane with
            | Some _ as hit -> hit
            | None -> side_hit t ~x ~y New right_side right_code right_pane)
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
  | Some code -> Code.set_on_rows_change code None

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
  t.left_pane <- None;
  t.right_pane <- None;
  t.left_lines <- [];
  t.right_lines <- [];
  t.pending_rebuild <- false;
  t.waiting_for_highlight <- false;
  t.alignment <- None

(* Intra-line emphasis rides the code buffer's highlight overlays: they are
   rendered by the text surface itself, so placement is wrap-correct and syntax
   foregrounds survive (the overlay carries only a background). Split reuses its
   code buffers across rebuilds, so stale ranges are dropped by ref first. *)
let emphasis_ref_id = 1

let set_code_emphasis code (ranges : View.emphasis) =
  let buffer = Code.buffer code in
  Text_buffer.remove_highlights_by_ref buffer emphasis_ref_id;
  List.iter
    (fun (start_offset, end_offset, color) ->
      Text_buffer.add_highlight buffer
        (Text_buffer.Highlight.make ~start_offset ~end_offset
           ~style:(Ansi.Style.make ~bg:color ())
           ~ref_id:emphasis_ref_id ()))
    ranges

(* The pane installed as [text] in [code], read through the index its buffer
   and layout use. While Code conceals, the buffer holds other text, so [text]
   is indexed on its own. *)
let code_pane code text =
  let buffer = Code.buffer code in
  Pane.of_source
    (if String.equal (Text_buffer.plain_text buffer) text then
       Text_buffer.source buffer
     else Pane.Source.of_string text)

(* Key [rows]' gutter entries and emphasis to the pane installed as [text]. *)
let decorate_pane (props : Props.t) side code ~text rows =
  let pane = code_pane code text in
  Line_number.apply_props side
    (View.gutter_props ~theme:props.theme
       ~show_line_numbers:props.show_line_numbers pane rows);
  set_code_emphasis code (View.emphasis ~line_spans:props.line_spans pane rows);
  pane

let code_props (props : Props.t) ~content ?syntax () =
  Code.Props.make ~content
    ?syntax:(Option.map code_syntax syntax)
    ~conceal:(code_conceal syntax) ~text_style:props.text_style ~wrap:props.wrap
    ~selectable:props.selectable ()

let make_code ~parent (props : Props.t) ~content ?syntax () =
  Code.create ~parent ~style:full_style ~content
    ?syntax:(Option.map code_syntax syntax)
    ~conceal:(code_conceal syntax) ~text_style:props.text_style ~wrap:props.wrap
    ~selectable:props.selectable ()

let make_or_update_code existing ~parent (props : Props.t) ~content ?syntax () =
  match existing with
  | None -> make_code ~parent props ~content ?syntax ()
  | Some code ->
      Renderable.set_style (Code.node code) full_style;
      Code.apply_props code (code_props props ~content ?syntax ());
      code

let old_syntax = function Some { old; _ } -> Some old | None -> None
let new_syntax = function Some { new_; _ } -> Some new_ | None -> None

let make_or_update_side existing ~parent ~style ~theme =
  match existing with
  | None ->
      Line_number.create ~parent ~style ~fg:theme.line_number_fg
        ?bg:theme.line_number_bg ()
  | Some side ->
      Renderable.set_style (Line_number.node side) style;
      side

let request_split_rebuild t =
  if t.props.layout = Split && t.props.wrap <> `None then begin
    t.pending_rebuild <- true;
    Renderable.set_live t.node true;
    Renderable.request_render t.node
  end

let set_code_rows_callback t code =
  Code.set_on_rows_change code
    (Some (fun () -> if t.waiting_for_highlight then request_split_rebuild t))

let update_waiting_for_highlight t (props : Props.t) left_code right_code =
  t.waiting_for_highlight <-
    props.wrap <> `None
    && Renderable.width t.node > 0
    && Renderable.width (Code.node left_code) > 0
    && Renderable.width (Code.node right_code) > 0
    && (Code.is_highlighting left_code || Code.is_highlighting right_code)

let pending_work t =
  if t.pending_rebuild then
    Some (Renderable.Pending.make ~kind:"diff.rebuild" ())
  else if t.waiting_for_highlight then
    Some (Renderable.Pending.make ~kind:"diff.highlight" ())
  else None

let build_unified_view t (props : Props.t) =
  (* Coming from Split: drop the split children so exactly one side remains.
     A unified rebuild otherwise reconciles its Line_number and Code in place,
     which keeps Code's latest captures and running highlight jobs alive
     across streaming patch updates. *)
  if t.right_side <> None || t.right_code <> None then destroy_children t;
  set_flex_direction t Toffee.Style.Flex_direction.Column;
  let rows =
    Array.of_list
      (View.unified ~theme:props.theme ~line_highlights:props.line_highlights
         ~line_signs:props.line_signs props.patch)
  in
  let text = View.text rows in
  let side =
    make_or_update_side t.left_side ~parent:t.node ~style:full_style
      ~theme:props.theme
  in
  let code =
    make_or_update_code t.left_code ~parent:(Line_number.node side) props
      ~content:text
      ?syntax:(new_syntax props.highlight)
      ()
  in
  t.left_pane <- Some (decorate_pane props side code ~text rows);
  t.left_side <- Some side;
  t.left_code <- Some code;
  t.left_lines <- [];
  t.right_lines <- []

let should_align_split t (props : Props.t) left_code right_code =
  props.wrap <> `None
  && (not (Code.is_highlighting left_code))
  && (not (Code.is_highlighting right_code))
  && Renderable.width t.node > 0
  && Renderable.width (Code.node left_code) > 0
  && Renderable.width (Code.node right_code) > 0

(* The alignment [t] computed for these pane texts, when the panes have kept
   their widths and the settings that shape their rows. Installing the aligned
   texts directly keeps the panes' content, and so their highlighting, stable
   across rebuilds. *)
let reusable_alignment t (props : Props.t) ~left_text ~right_text =
  match (t.alignment, t.left_code, t.right_code) with
  | Some a, Some left, Some right
    when String.equal a.left_text left_text
         && String.equal a.right_text right_text
         && a.left_width = Renderable.width (Code.node left)
         && a.right_width = Renderable.width (Code.node right)
         && a.wrap = props.wrap
         && Option.equal highlight_equal a.highlight props.highlight ->
      Some a.aligned
  | _ -> None

let build_split_view t (props : Props.t) =
  set_flex_direction t Toffee.Style.Flex_direction.Row;
  let split_rows lines =
    Array.of_list
      (View.split_rows ~theme:props.theme ~line_highlights:props.line_highlights
         ~line_signs:props.line_signs lines)
  in
  let initial = View.split props.patch in
  let initial_left = split_rows initial.View.left in
  let initial_right = split_rows initial.View.right in
  let initial_left_text = View.text initial_left in
  let initial_right_text = View.text initial_right in
  let reused =
    reusable_alignment t props ~left_text:initial_left_text
      ~right_text:initial_right_text
  in
  let installed_left_text, installed_right_text =
    match reused with
    | Some split ->
        ( View.text (split_rows split.View.left),
          View.text (split_rows split.View.right) )
    | None -> (initial_left_text, initial_right_text)
  in
  let left_side =
    make_or_update_side t.left_side ~parent:t.node ~style:half_style
      ~theme:props.theme
  in
  let left_code =
    make_or_update_code t.left_code
      ~parent:(Line_number.node left_side)
      props ~content:installed_left_text
      ?syntax:(old_syntax props.highlight)
      ()
  in
  let right_side =
    make_or_update_side t.right_side ~parent:t.node ~style:half_style
      ~theme:props.theme
  in
  let right_code =
    make_or_update_code t.right_code
      ~parent:(Line_number.node right_side)
      props ~content:installed_right_text
      ?syntax:(new_syntax props.highlight)
      ()
  in
  set_code_rows_callback t left_code;
  set_code_rows_callback t right_code;
  Renderable.set_on_resize (Code.node left_code)
    (Some (fun _ -> request_split_rebuild t));
  Renderable.set_on_resize (Code.node right_code)
    (Some (fun _ -> request_split_rebuild t));
  let split, left_rows, right_rows =
    match reused with
    | Some split ->
        (split, split_rows split.View.left, split_rows split.View.right)
    | None when should_align_split t props left_code right_code ->
        let visual_counts code text =
          Split_layout.visual_counts (code_pane code text) code
        in
        let split =
          Split_layout.align initial
            ~left_visual_counts:(visual_counts left_code initial_left_text)
            ~right_visual_counts:(visual_counts right_code initial_right_text)
        in
        t.alignment <-
          Some
            {
              left_text = initial_left_text;
              right_text = initial_right_text;
              left_width = Renderable.width (Code.node left_code);
              right_width = Renderable.width (Code.node right_code);
              wrap = props.wrap;
              highlight = props.highlight;
              aligned = split;
            };
        (split, split_rows split.View.left, split_rows split.View.right)
    | None -> (initial, initial_left, initial_right)
  in
  let left_text = View.text left_rows in
  let right_text = View.text right_rows in
  Code.apply_props left_code
    (code_props props ~content:left_text
       ?syntax:(old_syntax props.highlight)
       ());
  Code.apply_props right_code
    (code_props props ~content:right_text
       ?syntax:(new_syntax props.highlight)
       ());
  t.left_pane <-
    Some (decorate_pane props left_side left_code ~text:left_text left_rows);
  t.right_pane <-
    Some (decorate_pane props right_side right_code ~text:right_text right_rows);
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
    ?highlight ?line_highlights ?line_spans ?line_signs ?show_line_numbers ?wrap
    ?selectable ?text_style patch =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ~patch ?layout ?theme ?highlight ?line_highlights ?line_spans
      ?line_signs ?show_line_numbers ?wrap ?selectable ?text_style ()
  in
  let t =
    {
      node;
      props;
      left_side = None;
      right_side = None;
      left_code = None;
      right_code = None;
      left_pane = None;
      right_pane = None;
      left_lines = [];
      right_lines = [];
      on_line_click = None;
      mouse_down = None;
      pending_rebuild = false;
      waiting_for_highlight = false;
      alignment = None;
    }
  in
  Renderable.on_mouse node (handle_mouse t);
  Renderable.set_on_frame node
    (Some
       (fun _ ~dt:_ ->
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

let set_line_spans t line_spans = update t { t.props with line_spans }
let set_line_signs t line_signs = update t { t.props with line_signs }
let apply_props = update
let set_on_line_click t callback = t.on_line_click <- callback
