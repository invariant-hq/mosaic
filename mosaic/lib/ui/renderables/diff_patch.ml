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

let empty = make []
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
  if raw = [] then Ok empty
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
                    invalid_arg "Diff.Patch.of_unified: unexpected line prefix"
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
              Error ("Diff.Patch.of_unified: expected hunk header, got: " ^ line)
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
             if !k = -d || (!k <> d && v.(!k - 1 + offset) < v.(!k + 1 + offset))
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
          if k = -d || (k <> d && v.(k - 1 + offset) < v.(k + 1 + offset)) then
            k + 1
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
          if prev_k = k - 1 then { tag = Removed; content = a.(prev_x) } :: acc
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

let line_equal (a : line) (b : line) =
  a.tag = b.tag && String.equal a.content b.content

let hunk_equal (a : hunk) (b : hunk) =
  a.old_start = b.old_start && a.old_lines = b.old_lines
  && a.new_start = b.new_start && a.new_lines = b.new_lines
  && List.compare_lengths a.lines b.lines = 0
  && List.for_all2 line_equal a.lines b.lines

let equal a b =
  List.compare_lengths a.hunks b.hunks = 0
  && List.for_all2 hunk_equal a.hunks b.hunks
