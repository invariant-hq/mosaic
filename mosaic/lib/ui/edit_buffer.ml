(* ───── Helpers ───── *)

let strip_newlines s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = String.unsafe_get s !i in
    if c = '\r' then begin
      if !i + 1 < len && String.unsafe_get s (!i + 1) = '\n' then i := !i + 2
      else incr i
    end
    else if c = '\n' then incr i
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  if Buffer.length buf = len then s else Buffer.contents buf

let truncate_graphemes ~width_method ~tab_width s max_graphemes =
  if max_graphemes <= 0 then ""
  else
    let result_end = ref 0 in
    let idx = ref 0 in
    let truncated = ref false in
    Glyph.String.iter_grapheme_info ~width_method ~tab_width
      (fun ~offset:_ ~len ~width:_ ->
        if !idx < max_graphemes then result_end := !result_end + len
        else truncated := true;
        incr idx)
      s;
    if !truncated then String.sub s 0 !result_end else s

(* ───── Types ───── *)

type snapshot = {
  content : string;
  cursor_pos : int;
  selection_anchor : int option;
}

type cache = {
  offsets : int array;
  widths : int array;
  count : int;
  total_width : int;
  line_starts : int array;
  line_count : int;
}

type t = {
  mutable content : string;
  mutable cursor_pos : int;
  mutable selection_anchor : int option;
  mutable max_length : int;
  mutable undo_stack : snapshot list;
  mutable redo_stack : snapshot list;
  mutable cache : cache option;
  tab_width : int;
  width_method : Glyph.width_method;
}

(* ───── Cache ───── *)

let build_cache ~width_method ~tab_width content =
  let n = Glyph.String.grapheme_count content in
  let offsets = Array.make n 0 in
  let widths = Array.make n 0 in
  let idx = ref 0 in
  let total_width = ref 0 in
  let line_starts_rev = ref [ 0 ] in
  (* iter_grapheme_info skips zero-width graphemes (newlines, control chars), so
     we use iter_graphemes and measure_sub for per-grapheme width. *)
  Glyph.String.iter_graphemes
    (fun ~offset ~len ->
      let i = !idx in
      offsets.(i) <- offset;
      let w =
        Glyph.String.measure_sub ~width_method ~tab_width content
          ~pos:offset ~len
      in
      widths.(i) <- w;
      total_width := !total_width + w;
      let c = String.unsafe_get content offset in
      if c = '\n' || c = '\r' then
        line_starts_rev := (i + 1) :: !line_starts_rev;
      incr idx)
    content;
  let line_starts = Array.of_list (List.rev !line_starts_rev) in
  let line_count = Array.length line_starts in
  {
    offsets;
    widths;
    count = n;
    total_width = !total_width;
    line_starts;
    line_count;
  }

let ensure_cache t =
  match t.cache with
  | Some c -> c
  | None ->
      let c =
        build_cache ~width_method:t.width_method ~tab_width:t.tab_width
          t.content
      in
      t.cache <- Some c;
      c

let invalidate_cache t = t.cache <- None

let find_line cache pos =
  let lo = ref 0 in
  let hi = ref (cache.line_count - 1) in
  while !lo < !hi do
    let mid = !lo + ((!hi - !lo + 1) / 2) in
    if cache.line_starts.(mid) <= pos then lo := mid else hi := mid - 1
  done;
  !lo

(* ───── Byte offset helpers ───── *)

let byte_offset_of_grapheme_in_content t cache i =
  if i >= cache.count then String.length t.content else cache.offsets.(i)

(* ───── Construction ───── *)

let create ?(max_length = 1000) ?(width_method = `Unicode) ?(tab_width = 2)
    initial =
  let max_length = Int.max 0 max_length in
  let tab_width = Int.max 1 tab_width in
  let content = truncate_graphemes ~width_method ~tab_width initial max_length in
  let t =
    {
      content;
      cursor_pos = 0;
      selection_anchor = None;
      max_length;
      undo_stack = [];
      redo_stack = [];
      cache = None;
      tab_width;
      width_method;
    }
  in
  let c = ensure_cache t in
  t.cursor_pos <- c.count;
  t

(* ───── Content ───── *)

let text t = t.content

let set_text t s =
  let content =
    truncate_graphemes ~width_method:t.width_method ~tab_width:t.tab_width s
      t.max_length
  in
  t.content <- content;
  invalidate_cache t;
  let c = ensure_cache t in
  t.cursor_pos <- c.count;
  t.selection_anchor <- None

let length t =
  let c = ensure_cache t in
  c.count

let display_width t =
  let c = ensure_cache t in
  c.total_width

let is_empty t = String.length t.content = 0

(* ───── Line information ───── *)

let line_count t =
  let c = ensure_cache t in
  c.line_count

let cursor_line t =
  let c = ensure_cache t in
  find_line c t.cursor_pos

let cursor_col t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  t.cursor_pos - c.line_starts.(line)

(* ───── Cursor ───── *)

let cursor t = t.cursor_pos

let set_cursor t pos =
  let c = ensure_cache t in
  t.cursor_pos <- Int.max 0 (Int.min pos c.count);
  t.selection_anchor <- None

let cursor_display_offset t =
  let c = ensure_cache t in
  let w = ref 0 in
  for i = 0 to t.cursor_pos - 1 do
    if i < c.count then w := !w + c.widths.(i)
  done;
  !w

(* ───── Selection ───── *)

let selection t =
  match t.selection_anchor with
  | None -> None
  | Some anchor ->
      let lo = Int.min anchor t.cursor_pos in
      let hi = Int.max anchor t.cursor_pos in
      if lo = hi then None else Some (lo, hi)

let has_selection t = Option.is_some (selection t)

let selected_text t =
  match selection t with
  | None -> ""
  | Some (lo, hi) ->
      let c = ensure_cache t in
      let byte_lo = byte_offset_of_grapheme_in_content t c lo in
      let byte_hi = byte_offset_of_grapheme_in_content t c hi in
      String.sub t.content byte_lo (byte_hi - byte_lo)

let clear_selection t = t.selection_anchor <- None

let select_all t =
  let c = ensure_cache t in
  t.selection_anchor <- Some 0;
  t.cursor_pos <- c.count

(* ───── Undo / Redo ───── *)

let save_undo t =
  t.undo_stack <-
    { content = t.content; cursor_pos = t.cursor_pos;
      selection_anchor = t.selection_anchor }
    :: t.undo_stack;
  t.redo_stack <- []

let undo t =
  match t.undo_stack with
  | [] -> false
  | snap :: rest ->
      let old_content = t.content in
      t.redo_stack <-
        { content = t.content; cursor_pos = t.cursor_pos;
          selection_anchor = t.selection_anchor }
        :: t.redo_stack;
      t.undo_stack <- rest;
      t.content <- snap.content;
      t.cursor_pos <- snap.cursor_pos;
      t.selection_anchor <- snap.selection_anchor;
      invalidate_cache t;
      not (String.equal old_content snap.content)

let redo t =
  match t.redo_stack with
  | [] -> false
  | snap :: rest ->
      let old_content = t.content in
      t.undo_stack <-
        { content = t.content; cursor_pos = t.cursor_pos;
          selection_anchor = t.selection_anchor }
        :: t.undo_stack;
      t.redo_stack <- rest;
      t.content <- snap.content;
      t.cursor_pos <- snap.cursor_pos;
      t.selection_anchor <- snap.selection_anchor;
      invalidate_cache t;
      not (String.equal old_content snap.content)

(* ───── Internal mutation ───── *)

let delete_grapheme_range t lo hi =
  let c = ensure_cache t in
  let byte_lo = byte_offset_of_grapheme_in_content t c lo in
  let byte_hi = byte_offset_of_grapheme_in_content t c hi in
  let total_len = String.length t.content in
  t.content <-
    String.sub t.content 0 byte_lo
    ^ String.sub t.content byte_hi (total_len - byte_hi);
  t.cursor_pos <- lo;
  t.selection_anchor <- None;
  invalidate_cache t

let delete_selection t =
  match selection t with
  | None -> false
  | Some (lo, hi) ->
      save_undo t;
      delete_grapheme_range t lo hi;
      true

(* ───── Max length ───── *)

let max_length t = t.max_length

let set_max_length t n =
  let n = Int.max 0 n in
  t.max_length <- n;
  let c = ensure_cache t in
  if c.count > n then begin
    let byte_end = byte_offset_of_grapheme_in_content t c n in
    t.content <- String.sub t.content 0 byte_end;
    invalidate_cache t;
    let new_len = n in
    if t.cursor_pos > new_len then t.cursor_pos <- new_len;
    t.selection_anchor <- None
  end

(* ───── Input Truncation ───── *)

let truncate_to_fit t s =
  let c = ensure_cache t in
  let remaining = t.max_length - c.count in
  if remaining <= 0 then ""
  else
    let input_count = Glyph.String.grapheme_count s in
    if input_count <= remaining then s
    else begin
      let result_end = ref 0 in
      let idx = ref 0 in
      Glyph.String.iter_grapheme_info ~width_method:t.width_method
        ~tab_width:t.tab_width (fun ~offset:_ ~len ~width:_ ->
          if !idx < remaining then begin
            result_end := !result_end + len;
            incr idx
          end)
        s;
      String.sub s 0 !result_end
    end

(* ───── Editing ───── *)

let insert t s =
  match selection t with
  | Some (lo, hi) ->
      save_undo t;
      delete_grapheme_range t lo hi;
      if String.length s = 0 then true
      else
        let s = truncate_to_fit t s in
        if String.length s = 0 then true
        else begin
          let c = ensure_cache t in
          let byte_pos = byte_offset_of_grapheme_in_content t c t.cursor_pos in
          let total_len = String.length t.content in
          t.content <-
            String.sub t.content 0 byte_pos
            ^ s
            ^ String.sub t.content byte_pos (total_len - byte_pos);
          let inserted_count = Glyph.String.grapheme_count s in
          t.cursor_pos <- t.cursor_pos + inserted_count;
          invalidate_cache t;
          true
        end
  | None ->
      if String.length s = 0 then false
      else
        let s = truncate_to_fit t s in
        if String.length s = 0 then false
        else begin
          save_undo t;
          let c = ensure_cache t in
          let byte_pos = byte_offset_of_grapheme_in_content t c t.cursor_pos in
          let total_len = String.length t.content in
          t.content <-
            String.sub t.content 0 byte_pos
            ^ s
            ^ String.sub t.content byte_pos (total_len - byte_pos);
          let inserted_count = Glyph.String.grapheme_count s in
          t.cursor_pos <- t.cursor_pos + inserted_count;
          invalidate_cache t;
          true
        end

let delete_backward t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    save_undo t;
    delete_grapheme_range t (t.cursor_pos - 1) t.cursor_pos;
    true
  end

let delete_forward t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= c.count then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos (t.cursor_pos + 1);
      true
    end

(* ───── Word boundaries ───── *)

let next_word_boundary t =
  let c = ensure_cache t in
  let result = ref c.count in
  let found = ref false in
  Glyph.String.iter_wrap_breaks
    (fun ~break_byte_offset:_ ~next_byte_offset:_ ~grapheme_offset ->
      if (not !found) && grapheme_offset > t.cursor_pos then begin
        result := grapheme_offset;
        found := true
      end)
    t.content;
  !result

let prev_word_boundary t =
  let result = ref 0 in
  Glyph.String.iter_wrap_breaks
    (fun ~break_byte_offset:_ ~next_byte_offset:_ ~grapheme_offset ->
      if grapheme_offset < t.cursor_pos then result := grapheme_offset)
    t.content;
  !result

(* ───── Word deletion ───── *)

let delete_word_backward t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    let target = prev_word_boundary t in
    save_undo t;
    delete_grapheme_range t target t.cursor_pos;
    true
  end

let delete_word_forward t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= c.count then false
    else begin
      let target = next_word_boundary t in
      save_undo t;
      delete_grapheme_range t t.cursor_pos target;
      true
    end

let delete_to_line_start t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    let line = find_line c t.cursor_pos in
    let line_start = c.line_starts.(line) in
    if t.cursor_pos <= line_start then false
    else begin
      save_undo t;
      delete_grapheme_range t line_start t.cursor_pos;
      true
    end

let delete_to_line_end t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    let line = find_line c t.cursor_pos in
    let line_end =
      if line + 1 < c.line_count then c.line_starts.(line + 1) - 1 else c.count
    in
    if t.cursor_pos >= line_end then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos line_end;
      true
    end

let delete_to_start t =
  if has_selection t then delete_selection t
  else if t.cursor_pos = 0 then false
  else begin
    save_undo t;
    delete_grapheme_range t 0 t.cursor_pos;
    true
  end

let delete_to_end t =
  if has_selection t then delete_selection t
  else
    let c = ensure_cache t in
    if t.cursor_pos >= c.count then false
    else begin
      save_undo t;
      delete_grapheme_range t t.cursor_pos c.count;
      true
    end

let delete_line t =
  let c = ensure_cache t in
  if c.count = 0 then false
  else begin
    save_undo t;
    let line = find_line c t.cursor_pos in
    let start, stop =
      if c.line_count = 1 then (0, c.count)
      else if line + 1 < c.line_count then
        (c.line_starts.(line), c.line_starts.(line + 1))
      else (c.line_starts.(line) - 1, c.count)
    in
    delete_grapheme_range t start stop;
    true
  end

(* ───── Cursor movement ───── *)

let update_cursor_with_selection t ~select new_pos =
  let old_pos = t.cursor_pos in
  if select then begin
    (match t.selection_anchor with
    | None -> t.selection_anchor <- Some old_pos
    | Some _ -> ());
    t.cursor_pos <- new_pos;
    (* Clear selection if anchor equals cursor *)
    match t.selection_anchor with
    | Some a when a = t.cursor_pos -> t.selection_anchor <- None
    | _ -> ()
  end
  else begin
    t.cursor_pos <- new_pos;
    t.selection_anchor <- None
  end;
  old_pos <> new_pos

let move_left ?(select = false) t =
  if (not select) && has_selection t then begin
    let lo, _ = Option.get (selection t) in
    t.cursor_pos <- lo;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos = 0 then false
  else update_cursor_with_selection t ~select (t.cursor_pos - 1)

let move_right ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    let _, hi = Option.get (selection t) in
    t.cursor_pos <- hi;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos >= c.count then false
  else update_cursor_with_selection t ~select (t.cursor_pos + 1)

let move_word_forward ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    let _, hi = Option.get (selection t) in
    t.cursor_pos <- hi;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos >= c.count then false
  else
    let target = next_word_boundary t in
    update_cursor_with_selection t ~select target

let move_word_backward ?(select = false) t =
  if (not select) && has_selection t then begin
    let lo, _ = Option.get (selection t) in
    t.cursor_pos <- lo;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos = 0 then false
  else
    let target = prev_word_boundary t in
    update_cursor_with_selection t ~select target

let move_home ?(select = false) t =
  if (not select) && has_selection t then begin
    t.cursor_pos <- 0;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos = 0 then false
  else update_cursor_with_selection t ~select 0

let move_end ?(select = false) t =
  let c = ensure_cache t in
  if (not select) && has_selection t then begin
    t.cursor_pos <- c.count;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos >= c.count then false
  else update_cursor_with_selection t ~select c.count

let move_line_start ?(select = false) t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  let target = c.line_starts.(line) in
  if (not select) && has_selection t then begin
    t.cursor_pos <- target;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos = target then false
  else update_cursor_with_selection t ~select target

let move_line_end ?(select = false) t =
  let c = ensure_cache t in
  let line = find_line c t.cursor_pos in
  let target =
    if line + 1 < c.line_count then c.line_starts.(line + 1) - 1 else c.count
  in
  if (not select) && has_selection t then begin
    t.cursor_pos <- target;
    t.selection_anchor <- None;
    true
  end
  else if t.cursor_pos >= target then false
  else update_cursor_with_selection t ~select target

let set_cursor_offset ?(select = false) t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos c.count) in
  ignore (update_cursor_with_selection t ~select pos : bool)

(* ───── Offset conversions ───── *)

let line_of_offset t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos c.count) in
  find_line c pos

let col_of_offset t pos =
  let c = ensure_cache t in
  let pos = Int.max 0 (Int.min pos c.count) in
  let line = find_line c pos in
  pos - c.line_starts.(line)

let line_start t line =
  let c = ensure_cache t in
  let line = Int.max 0 (Int.min line (c.line_count - 1)) in
  c.line_starts.(line)

let line_end t line =
  let c = ensure_cache t in
  let line = Int.max 0 (Int.min line (c.line_count - 1)) in
  if line + 1 < c.line_count then c.line_starts.(line + 1) - 1 else c.count

let byte_offset t ~grapheme =
  let c = ensure_cache t in
  byte_offset_of_grapheme_in_content t c grapheme
