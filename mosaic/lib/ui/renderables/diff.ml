module Patch = struct
  type tag = Context | Added | Removed
  type line = { tag : tag; content : string }

  type hunk = {
    old_start : int;
    old_lines : int;
    new_start : int;
    new_lines : int;
    lines : line list;
  }

  type t = { hunks : hunk list }

  let count_tags lines =
    List.fold_left
      (fun (ctx, add, rem) { tag; _ } ->
        match tag with
        | Context -> (ctx + 1, add, rem)
        | Added -> (ctx, add + 1, rem)
        | Removed -> (ctx, add, rem + 1))
      (0, 0, 0) lines

  let valid_start start lines = start >= 1 || (start = 0 && lines = 0)

  let validate_hunk h =
    if not (valid_start h.old_start h.old_lines) then
      invalid_arg
        "Diff.Patch.make: old_start must be >= 1, or 0 for an empty old range";
    if not (valid_start h.new_start h.new_lines) then
      invalid_arg
        "Diff.Patch.make: new_start must be >= 1, or 0 for an empty new range";
    let ctx, add, rem = count_tags h.lines in
    if h.old_lines <> ctx + rem || h.new_lines <> ctx + add then
      invalid_arg "Diff.Patch.make: hunk line counts disagree with tags"

  let rec validate_order = function
    | [] | [ _ ] -> ()
    | a :: (b :: _ as rest) ->
        if b.old_start < a.old_start then
          invalid_arg "Diff.Patch.make: hunks must be sorted by old_start";
        if a.old_start + a.old_lines > b.old_start then
          invalid_arg "Diff.Patch.make: hunk old ranges overlap";
        validate_order rest

  let make hunks =
    List.iter validate_hunk hunks;
    validate_order hunks;
    { hunks }

  let hunks t = t.hunks
  let is_empty t = t.hunks = []
  let char_of_tag = function Context -> ' ' | Added -> '+' | Removed -> '-'

  let pp_hunk ppf h =
    Format.fprintf ppf "@@@@ -%d,%d +%d,%d @@@@\n" h.old_start h.old_lines
      h.new_start h.new_lines;
    List.iter
      (fun { tag; content } ->
        Format.fprintf ppf "%c%s\n" (char_of_tag tag) content)
      h.lines

  let pp ppf t = List.iter (pp_hunk ppf) t.hunks

  let split_lines s =
    if s = "" then [||]
    else
      let lines = String.split_on_char '\n' s in
      let lines =
        match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
      in
      Array.of_list lines

  let starts_with s prefix =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix

  let parse_hunk_header s =
    let len = String.length s in
    if not (starts_with s "@@") then None
    else
      let i = ref 2 in
      let skip_spaces () =
        while !i < len && s.[!i] = ' ' do
          incr i
        done
      in
      let read_int () =
        let start = !i in
        while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
          incr i
        done;
        if !i = start then None
        else Some (int_of_string (String.sub s start (!i - start)))
      in
      let read_pair () =
        match read_int () with
        | None -> None
        | Some a ->
            if !i < len && s.[!i] = ',' then begin
              incr i;
              Option.map (fun b -> (a, b)) (read_int ())
            end
            else Some (a, 1)
      in
      skip_spaces ();
      if !i >= len || s.[!i] <> '-' then None
      else begin
        incr i;
        match read_pair () with
        | None -> None
        | Some (old_start, old_lines) ->
            skip_spaces ();
            if !i >= len || s.[!i] <> '+' then None
            else begin
              incr i;
              match read_pair () with
              | None -> None
              | Some (new_start, new_lines) ->
                  Some (old_start, old_lines, new_start, new_lines)
            end
      end

  let drop_trailing_empty = function
    | [] -> []
    | xs -> ( match List.rev xs with "" :: rest -> List.rev rest | _ -> xs)

  let rec drop_until_hunk = function
    | [] -> []
    | line :: _ as lines when starts_with line "@@" -> lines
    | _ :: rest -> drop_until_hunk rest

  let of_unified s =
    let raw = drop_trailing_empty (String.split_on_char '\n' s) in
    if raw = [] then Ok (make [])
    else
      let ( let* ) = Result.bind in
      let rec take_lines acc remaining input =
        if remaining = (0, 0) then Ok (List.rev acc, input)
        else
          match input with
          | [] -> Error "Diff.Patch.of_unified: hunk truncated"
          | l :: tail when starts_with l "\\" -> take_lines acc remaining tail
          | l :: tail ->
              let tag, content =
                if l = "" then (Context, "")
                else
                  let rest = String.sub l 1 (String.length l - 1) in
                  match l.[0] with
                  | ' ' -> (Context, rest)
                  | '+' -> (Added, rest)
                  | '-' -> (Removed, rest)
                  | _ ->
                      invalid_arg
                        "Diff.Patch.of_unified: unexpected line prefix"
              in
              let remaining' =
                let old_remaining, new_remaining = remaining in
                match tag with
                | Context -> (old_remaining - 1, new_remaining - 1)
                | Added -> (old_remaining, new_remaining - 1)
                | Removed -> (old_remaining - 1, new_remaining)
              in
              if fst remaining' < 0 || snd remaining' < 0 then
                Error "Diff.Patch.of_unified: hunk line counts disagree"
              else take_lines ({ tag; content } :: acc) remaining' tail
      in
      let rec parse_hunks acc = function
        | [] -> Ok (List.rev acc)
        | line :: rest -> (
            match parse_hunk_header line with
            | None when acc <> [] -> Ok (List.rev acc)
            | None ->
                Error
                  ("Diff.Patch.of_unified: expected hunk header, got: " ^ line)
            | Some (old_start, old_lines, new_start, new_lines) -> (
                match take_lines [] (old_lines, new_lines) rest with
                | Error _ as e -> e
                | Ok (lines, rest') ->
                    parse_hunks
                      ({ old_start; old_lines; new_start; new_lines; lines }
                      :: acc)
                      rest'))
      in
      match drop_until_hunk raw with
      | [] -> Error "Diff.Patch.of_unified: expected hunk header"
      | lines -> (
          try
            let* hunks = parse_hunks [] lines in
            try Ok (make hunks) with Invalid_argument message -> Error message
          with Invalid_argument message -> Error message)

  let myers_script (a : string array) (b : string array) : line list =
    let n = Array.length a and m = Array.length b in
    if n = 0 && m = 0 then []
    else if n = 0 then
      Array.fold_right (fun s acc -> { tag = Added; content = s } :: acc) b []
    else if m = 0 then
      Array.fold_right (fun s acc -> { tag = Removed; content = s } :: acc) a []
    else
      let max_d = n + m in
      let v_size = (2 * max_d) + 1 in
      let offset = max_d in
      let trace = Array.make (max_d + 1) [||] in
      let v = Array.make v_size 0 in
      let final_d = ref (-1) in
      (try
         for d = 0 to max_d do
           let k = ref (-d) in
           while !k <= d do
             let x =
               if
                 !k = -d
                 || (!k <> d && v.(!k - 1 + offset) < v.(!k + 1 + offset))
               then v.(!k + 1 + offset)
               else v.(!k - 1 + offset) + 1
             in
             let x = ref x in
             let y = ref (!x - !k) in
             while !x < n && !y < m && String.equal a.(!x) b.(!y) do
               incr x;
               incr y
             done;
             v.(!k + offset) <- !x;
             if !x >= n && !y >= m then begin
               trace.(d) <- Array.copy v;
               final_d := d;
               raise Exit
             end;
             k := !k + 2
           done;
           trace.(d) <- Array.copy v
         done
       with Exit -> ());
      assert (!final_d >= 0);
      let rec walk d x y acc =
        if d = 0 then
          let rec emit_context i j acc =
            if i = 0 && j = 0 then acc
            else
              emit_context (i - 1) (j - 1)
                ({ tag = Context; content = a.(i - 1) } :: acc)
          in
          emit_context x y acc
        else
          let v = trace.(d - 1) in
          let k = x - y in
          let prev_k =
            if k = -d || (k <> d && v.(k - 1 + offset) < v.(k + 1 + offset))
            then k + 1
            else k - 1
          in
          let prev_x = v.(prev_k + offset) in
          let prev_y = prev_x - prev_k in
          let mid_x, mid_y =
            if prev_k = k - 1 then (prev_x + 1, prev_y) else (prev_x, prev_y + 1)
          in
          let rec emit_snake i j acc =
            if i <= mid_x && j <= mid_y then acc
            else
              emit_snake (i - 1) (j - 1)
                ({ tag = Context; content = a.(i - 1) } :: acc)
          in
          let acc = emit_snake x y acc in
          let acc =
            if prev_k = k - 1 then
              { tag = Removed; content = a.(prev_x) } :: acc
            else { tag = Added; content = b.(prev_y) } :: acc
          in
          walk (d - 1) prev_x prev_y acc
      in
      walk !final_d n m []

  let is_change line =
    match line.tag with Added | Removed -> true | Context -> false

  let script_to_hunks ~context script =
    let arr = Array.of_list script in
    let len = Array.length arr in
    if len = 0 then []
    else
      let changes =
        let rec find acc i =
          if i >= len then List.rev acc
          else if is_change arr.(i) then (
            let j = ref i in
            while !j < len && is_change arr.(!j) do
              incr j
            done;
            find ((i, !j - 1) :: acc) !j)
          else find acc (i + 1)
        in
        find [] 0
      in
      if changes = [] then []
      else
        let ranges =
          List.map
            (fun (start, stop) ->
              let low = max 0 (start - context) in
              let high =
                if context > len || stop + context > len - 1 then len - 1
                else stop + context
              in
              (low, high))
            changes
        in
        let rec merge = function
          | (a, b) :: (c, d) :: rest when c <= b + 1 ->
              merge ((a, max b d) :: rest)
          | range :: rest -> range :: merge rest
          | [] -> []
        in
        let old_at = Array.make (len + 1) 1 in
        let new_at = Array.make (len + 1) 1 in
        for i = 0 to len - 1 do
          let old_line = old_at.(i) and new_line = new_at.(i) in
          match arr.(i).tag with
          | Context ->
              old_at.(i + 1) <- old_line + 1;
              new_at.(i + 1) <- new_line + 1
          | Removed ->
              old_at.(i + 1) <- old_line + 1;
              new_at.(i + 1) <- new_line
          | Added ->
              old_at.(i + 1) <- old_line;
              new_at.(i + 1) <- new_line + 1
        done;
        List.map
          (fun (low, high) ->
            let lines = Array.sub arr low (high - low + 1) |> Array.to_list in
            let ctx, add, rem = count_tags lines in
            let old_lines = ctx + rem in
            let new_lines = ctx + add in
            let old_start =
              if old_lines = 0 then max 0 (old_at.(low) - 1) else old_at.(low)
            in
            let new_start =
              if new_lines = 0 then max 0 (new_at.(low) - 1) else new_at.(low)
            in
            { old_start; old_lines; new_start; new_lines; lines })
          (merge ranges)

  let of_strings ~old ~new_ ?(context = 3) () =
    let script = myers_script (split_lines old) (split_lines new_) in
    make (script_to_hunks ~context script)
end

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

type highlight = {
  old_spans : Text_buffer.span list;
  new_spans : Text_buffer.span list;
}

let spans_equal a b =
  List.compare_lengths a b = 0
  && List.for_all2
       (fun (x : Text_buffer.span) (y : Text_buffer.span) ->
         String.equal x.text y.text && Ansi.Style.equal x.style y.style)
       a b

let highlight_equal a b =
  spans_equal a.old_spans b.old_spans && spans_equal a.new_spans b.new_spans

let line_equal (a : Patch.line) (b : Patch.line) =
  a.tag = b.tag && String.equal a.content b.content

let hunk_equal (a : Patch.hunk) (b : Patch.hunk) =
  a.old_start = b.old_start && a.old_lines = b.old_lines
  && a.new_start = b.new_start && a.new_lines = b.new_lines
  && List.compare_lengths a.lines b.lines = 0
  && List.for_all2 line_equal a.lines b.lines

let patch_equal a b =
  let ah = Patch.hunks a and bh = Patch.hunks b in
  List.compare_lengths ah bh = 0 && List.for_all2 hunk_equal ah bh

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

  let empty_patch = Patch.make []

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
    patch_equal a.patch b.patch
    && a.layout = b.layout
    && theme_equal a.theme b.theme
    && Option.equal highlight_equal a.highlight b.highlight
    && a.show_line_numbers = b.show_line_numbers
    && a.wrap = b.wrap
    && a.selectable = b.selectable
    && Ansi.Style.equal a.text_style b.text_style
end

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
        gutter = Option.value theme.removed_line_number_bg ~default:transparent;
        content =
          Some (Option.value theme.removed_content_bg ~default:theme.removed_bg);
      }
  | Context ->
      {
        gutter = Option.value theme.line_number_bg ~default:transparent;
        content =
          (match theme.context_content_bg with
          | Some _ as c -> c
          | None -> theme.context_bg);
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

type unified_build = {
  content : string;
  line_colors : (int * Line_number.line_color) list;
  line_signs : (int * Line_number.line_sign) list;
  line_numbers : (int * int) list;
}

let build_unified ~theme (patch : Patch.t) =
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
    | Patch.Removed -> line_signs := (index, removed_sign theme) :: !line_signs
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
type split_line = { content : string; line_num : int option; kind : split_kind }

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

let build_split (patch : Patch.t) =
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
  (List.rev !left, List.rev !right)

let split_props ~theme ~show_line_numbers lines =
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

let spans_or_content ~content = function
  | [] -> (content, [])
  | spans -> ("", spans)

let make_code ~parent (props : Props.t) ~content ~spans =
  let content, spans = spans_or_content ~content spans in
  Code.create ~parent ~style:full_style ~content ~spans
    ~text_style:props.text_style ~wrap:props.wrap ~selectable:props.selectable
    ()

let join_contents lines =
  String.concat "\n" (List.map (fun (line : split_line) -> line.content) lines)

let old_spans = function Some { old_spans; _ } -> old_spans | None -> []
let new_spans = function Some { new_spans; _ } -> new_spans | None -> []

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
  let { content; line_colors; line_signs; line_numbers } =
    build_unified ~theme:props.theme props.patch
  in
  let side =
    Line_number.create ~parent:t.node ~style:full_style
      ~fg:props.theme.line_number_fg ?bg:props.theme.line_number_bg
      ~show_line_numbers:props.show_line_numbers ~line_colors ~line_signs
      ~line_numbers ()
  in
  let _code =
    Code.create ~parent:(Line_number.node side) ~style:full_style ~content
      ~text_style:props.text_style ~wrap:props.wrap ~selectable:props.selectable
      ()
  in
  t.left_side <- Some side

let build_split_view t (props : Props.t) =
  destroy_children t;
  set_flex_direction t Toffee.Style.Flex_direction.Row;
  let left, right = build_split props.patch in
  let left_props =
    split_props ~theme:props.theme ~show_line_numbers:props.show_line_numbers
      left
  in
  let right_props =
    split_props ~theme:props.theme ~show_line_numbers:props.show_line_numbers
      right
  in
  let left_side =
    make_side ~parent:t.node ~style:half_style ~theme:props.theme
      ~props:left_props
  in
  let _left_code =
    make_code
      ~parent:(Line_number.node left_side)
      props ~content:(join_contents left)
      ~spans:(old_spans props.highlight)
  in
  let right_side =
    make_side ~parent:t.node ~style:half_style ~theme:props.theme
      ~props:right_props
  in
  let _right_code =
    make_code
      ~parent:(Line_number.node right_side)
      props ~content:(join_contents right)
      ~spans:(new_spans props.highlight)
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
