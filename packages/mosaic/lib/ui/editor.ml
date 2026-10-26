module Engine = struct
  module Layout = Matrix.Text.Layout
  module Source = Layout.Source

  (* ───── Helpers ───── *)

  let indexed_text s max_graphemes =
    let source = Source.of_string s in
    if Source.length source <= max_graphemes then source
    else Source.of_string (Source.slice source ~first:0 ~last:max_graphemes)

  (* ───── Types ───── *)

  (* An undo or redo entry is the inverse of one step, a contiguous text
     change: replacing the [inserted] bytes at byte [at] with [removed]
     restores the text, and the caret and selection are restored as recorded.
     An entry costs the size of its change, not of the document. *)
  type change = {
    at : int;
    removed : string;
    inserted : int;
    cursor_pos : int;
    affinity : Layout.affinity;
    selection_anchor : int option;
  }

  (* The edits that can extend the latest undo step: typing one grapheme
     cluster, and deleting one backward or forward. *)
  type step = Typing | Deleting_backward | Deleting_forward

  (* The display position at which a surface shows the value, and whether it
     follows the caret. Shared between versions until it changes. *)
  type viewport = { x : int; y : int; follow : bool }

  type t = {
    mutable content : string;
    mutable cursor_pos : int;
    mutable affinity : Layout.affinity;
    mutable selection_anchor : int option;
    max_length : int;
    mutable undo_stack : change list;
    mutable redo_stack : change list;
    mutable open_step : step option;
        (* The kind of the latest undo step while it can still grow: set by
           the edit that recorded it, cleared by any operation that records
           nothing, and by undo and redo. *)
    mutable index : Source.t; (* The indexed text, shared by copies. *)
    mutable preferred_col : int option;
    mutable viewport : viewport;
  }

  (* ───── Index ───── *)

  let source t = t.index

  (* Replaces the bytes [first, last) with [text], indexing again only the
     lines the replacement touches. *)
  let splice t ~first ~last text =
    let source = Source.edit t.index ~first ~last text in
    t.content <- Source.text source;
    t.index <- source;
    source

  let find_line = Source.line_of_offset
  let byte_offset_of_grapheme_in_content c i = Source.byte_offset c i

  (* ───── Construction ───── *)

  let create ~max_length initial =
    let max_length = Int.max 0 max_length in
    let source = indexed_text initial max_length in
    {
      content = Source.text source;
      cursor_pos = Source.length source;
      affinity = Layout.Downstream;
      selection_anchor = None;
      max_length;
      undo_stack = [];
      redo_stack = [];
      open_step = None;
      index = source;
      preferred_col = None;
      viewport = { x = 0; y = 0; follow = false };
    }

  (* Copies share immutable content, source indexes and history entries. *)
  let copy t = { t with content = t.content }

  let same a b =
    a.content == b.content
    && a.cursor_pos = b.cursor_pos
    && a.affinity = b.affinity
    && a.selection_anchor = b.selection_anchor
    && a.max_length = b.max_length
    && a.undo_stack == b.undo_stack
    && a.redo_stack == b.redo_stack
    && a.viewport = b.viewport

  (* ───── Content ───── *)

  let text t = t.content

  let length t =
    let c = t.index in
    Source.length c

  let is_empty t = String.length t.content = 0

  (* ───── Cursor ───── *)

  let cursor t = t.cursor_pos

  let set_cursor t pos =
    t.affinity <- Layout.Downstream;
    let c = t.index in
    t.cursor_pos <- Int.max 0 (Int.min pos (Source.length c));
    t.selection_anchor <- None

  let caret t = Layout.{ grapheme = t.cursor_pos; affinity = t.affinity }

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
        let c = t.index in
        let byte_lo = byte_offset_of_grapheme_in_content c lo in
        let byte_hi = byte_offset_of_grapheme_in_content c hi in
        String.sub t.content byte_lo (byte_hi - byte_lo)

  let clear_selection t = t.selection_anchor <- None

  let select_all t =
    t.affinity <- Layout.Downstream;
    let c = t.index in
    t.selection_anchor <- Some 0;
    t.cursor_pos <- Source.length c

  (* ───── Undo / Redo ───── *)

  let breaks_line s = String.exists (fun c -> c = '\n' || c = '\r') s

  (* Whether replacing bytes [first, last), which hold [removed], with [text]
     continues the open step of kind [step] whose entry is [top]: a deletion
     at the step's seam, or typing at the end of what it typed. Neither
     crosses a line break, and typing crosses no word boundary: a space after
     other text, or other text after two or more spaces. *)
  let continues t step top ~first ~last ~removed text =
    match step with
    | Deleting_backward -> last = top.at && not (breaks_line removed)
    | Deleting_forward -> first = top.at && not (breaks_line removed)
    | Typing ->
        let space i = i >= top.at && t.content.[i] = ' ' in
        first = last
        && first = top.at + top.inserted
        && (not (breaks_line text))
        &&
        if String.equal text " " then space (first - 1)
        else not (space (first - 1) && space (first - 2))

  (* Records the inverse of replacing bytes [first, last) with [text], before
     the replacement happens, as a new undo step or merged into the open one
     when [step] continues it. Following prosemirror-history, which stores
     inverted steps, the history grows with the edits made, not with the
     document; no entry is ever dropped. *)
  let record t ~step ~first ~last text =
    let removed = String.sub t.content first (last - first) in
    t.undo_stack <-
      (match (step, t.open_step, t.undo_stack) with
      | Some step, Some open_step, top :: rest
        when step = open_step && continues t step top ~first ~last ~removed text
        ->
          let top =
            match step with
            | Typing ->
                { top with inserted = top.inserted + String.length text }
            | Deleting_backward ->
                { top with at = first; removed = removed ^ top.removed }
            | Deleting_forward -> { top with removed = top.removed ^ removed }
          in
          top :: rest
      | _ ->
          {
            at = first;
            removed;
            inserted = String.length text;
            cursor_pos = t.cursor_pos;
            affinity = t.affinity;
            selection_anchor = t.selection_anchor;
          }
          :: t.undo_stack);
    t.open_step <- step;
    t.redo_stack <- []

  (* Applies [change] and returns its inverse, which reapplies the current
     state. *)
  let apply t change =
    let inverse =
      {
        at = change.at;
        removed = String.sub t.content change.at change.inserted;
        inserted = String.length change.removed;
        cursor_pos = t.cursor_pos;
        affinity = t.affinity;
        selection_anchor = t.selection_anchor;
      }
    in
    ignore
      (splice t ~first:change.at
         ~last:(change.at + change.inserted)
         change.removed
        : Source.t);
    t.cursor_pos <- change.cursor_pos;
    t.affinity <- change.affinity;
    t.selection_anchor <- change.selection_anchor;
    t.open_step <- None;
    inverse

  let undo t =
    match t.undo_stack with
    | [] -> false
    | change :: rest ->
        t.undo_stack <- rest;
        let inverse = apply t change in
        t.redo_stack <- inverse :: t.redo_stack;
        not (String.equal inverse.removed change.removed)

  let redo t =
    match t.redo_stack with
    | [] -> false
    | change :: rest ->
        t.redo_stack <- rest;
        let inverse = apply t change in
        t.undo_stack <- inverse :: t.undo_stack;
        not (String.equal inverse.removed change.removed)

  (* ───── Internal mutation ───── *)

  let delete_grapheme_range ?step t lo hi =
    let c = t.index in
    let byte_lo = byte_offset_of_grapheme_in_content c lo in
    let byte_hi = byte_offset_of_grapheme_in_content c hi in
    record t ~step ~first:byte_lo ~last:byte_hi "";
    let source = splice t ~first:byte_lo ~last:byte_hi "" in
    t.selection_anchor <- None;
    t.cursor_pos <- Source.grapheme_of_byte source ~affinity:Downstream byte_lo;
    t.affinity <- Layout.Downstream

  let delete_selection ?step t =
    match selection t with
    | None -> false
    | Some (lo, hi) ->
        delete_grapheme_range ?step t lo hi;
        true

  let clear t =
    if is_empty t then false
    else begin
      delete_grapheme_range t 0 (length t);
      true
    end

  (* ───── Editing ───── *)

  let insert t inserted =
    let old = t.index in
    let first, last =
      match selection t with
      | Some range -> range
      | None -> (t.cursor_pos, t.cursor_pos)
    in
    let a = Source.byte_offset old first and b = Source.byte_offset old last in
    let candidate = Source.edit old ~first:a ~last:b inserted in
    let candidate, admitted =
      if Source.length candidate <= t.max_length then (candidate, inserted)
      else
        let available =
          max 0 (t.max_length - (Source.length old - (last - first)))
        in
        let admitted =
          Source.slice (Source.of_string inserted) ~first:0 ~last:available
        in
        (Source.edit old ~first:a ~last:b admitted, admitted)
    in
    if String.equal admitted (String.sub t.content a (b - a)) then false
    else begin
      (* One grapheme cluster is typing; several at once, a paste. *)
      let step =
        if Matrix.Text.grapheme_count admitted = 1 then Some Typing else None
      in
      record t ~step ~first:a ~last:b admitted;
      t.content <- Source.text candidate;
      t.index <- candidate;
      t.cursor_pos <-
        Source.grapheme_of_byte candidate ~affinity:Downstream
          (a + String.length admitted);
      t.affinity <- Layout.Downstream;
      t.selection_anchor <- None;
      true
    end

  let delete_backward t =
    let step = Deleting_backward in
    if has_selection t then delete_selection ~step t
    else if t.cursor_pos = 0 then false
    else begin
      delete_grapheme_range ~step t (t.cursor_pos - 1) t.cursor_pos;
      true
    end

  let delete_forward t =
    let step = Deleting_forward in
    if has_selection t then delete_selection ~step t
    else
      let c = t.index in
      if t.cursor_pos >= Source.length c then false
      else begin
        delete_grapheme_range ~step t t.cursor_pos (t.cursor_pos + 1);
        true
      end

  (* ───── Word boundaries ───── *)

  let grapheme_contains_line_break t c i =
    let start = byte_offset_of_grapheme_in_content c i in
    let stop = byte_offset_of_grapheme_in_content c (i + 1) in
    let rec loop byte_offset =
      byte_offset < stop
      &&
      match String.unsafe_get t.content byte_offset with
      | '\n' | '\r' -> true
      | _ -> loop (byte_offset + 1)
    in
    loop start

  let word_break_set t =
    let c = t.index in
    let s = Hashtbl.create 16 in
    Matrix.Text.iter_word_breaks
      (fun ~break_byte_offset ~next_byte_offset ~grapheme_offset ->
        ignore break_byte_offset;
        ignore next_byte_offset;
        Hashtbl.replace s grapheme_offset ())
      t.content;
    for i = 0 to Source.length c - 1 do
      if grapheme_contains_line_break t c i then Hashtbl.replace s i ()
    done;
    s

  let next_word_boundary t =
    let c = t.index in
    let breaks = word_break_set t in
    let pos = ref t.cursor_pos in
    while !pos < Source.length c && not (Hashtbl.mem breaks !pos) do
      incr pos
    done;
    while !pos < Source.length c && Hashtbl.mem breaks !pos do
      incr pos
    done;
    !pos

  let prev_word_boundary t =
    let breaks = word_break_set t in
    let pos = ref t.cursor_pos in
    while !pos > 0 && Hashtbl.mem breaks (!pos - 1) do
      decr pos
    done;
    while !pos > 0 && not (Hashtbl.mem breaks (!pos - 1)) do
      decr pos
    done;
    !pos

  (* ───── Word deletion ───── *)

  let delete_word_backward t =
    if has_selection t then delete_selection t
    else if t.cursor_pos = 0 then false
    else begin
      let pos = prev_word_boundary t in
      delete_grapheme_range t pos t.cursor_pos;
      true
    end

  let delete_word_forward t =
    if has_selection t then delete_selection t
    else
      let c = t.index in
      if t.cursor_pos >= Source.length c then false
      else begin
        let pos = next_word_boundary t in
        delete_grapheme_range t t.cursor_pos pos;
        true
      end

  let delete_to_line_start t =
    if has_selection t then delete_selection t
    else
      let c = t.index in
      let line = find_line c t.cursor_pos in
      let line_start = Source.line_start c line in
      if t.cursor_pos <= line_start then false
      else begin
        delete_grapheme_range t line_start t.cursor_pos;
        true
      end

  let delete_to_line_end t =
    if has_selection t then delete_selection t
    else
      let c = t.index in
      let line = find_line c t.cursor_pos in
      let line_end =
        if line + 1 < Source.line_count c then
          Source.line_start c (line + 1) - 1
        else Source.length c
      in
      if t.cursor_pos >= line_end then false
      else begin
        delete_grapheme_range t t.cursor_pos line_end;
        true
      end

  let delete_line t =
    let c = t.index in
    if Source.length c = 0 then false
    else begin
      let line = find_line c t.cursor_pos in
      let start, stop =
        if Source.line_count c = 1 then (0, Source.length c)
        else if line + 1 < Source.line_count c then
          (Source.line_start c line, Source.line_start c (line + 1))
        else (Source.line_start c line - 1, Source.length c)
      in
      delete_grapheme_range t start stop;
      true
    end

  (* ───── Cursor movement ───── *)

  let update_cursor_with_selection t ~select new_pos =
    t.affinity <- Layout.Downstream;
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
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos = 0 then false
    else update_cursor_with_selection t ~select (t.cursor_pos - 1)

  let move_right ?(select = false) t =
    let c = t.index in
    if (not select) && has_selection t then begin
      let _, hi = Option.get (selection t) in
      t.cursor_pos <- hi;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos >= Source.length c then false
    else update_cursor_with_selection t ~select (t.cursor_pos + 1)

  let move_word_forward ?(select = false) t =
    let c = t.index in
    if (not select) && has_selection t then begin
      let _, hi = Option.get (selection t) in
      t.cursor_pos <- hi;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos >= Source.length c then false
    else
      let target = next_word_boundary t in
      update_cursor_with_selection t ~select target

  let move_word_backward ?(select = false) t =
    if (not select) && has_selection t then begin
      let lo, _ = Option.get (selection t) in
      t.cursor_pos <- lo;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
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
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos = 0 then false
    else update_cursor_with_selection t ~select 0

  let move_end ?(select = false) t =
    let c = t.index in
    if (not select) && has_selection t then begin
      t.cursor_pos <- Source.length c;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos >= Source.length c then false
    else update_cursor_with_selection t ~select (Source.length c)

  let move_line_start ?(select = false) t =
    let c = t.index in
    let line = find_line c t.cursor_pos in
    let target = Source.line_start c line in
    if (not select) && has_selection t then begin
      t.cursor_pos <- target;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos = target then false
    else update_cursor_with_selection t ~select target

  let move_line_end ?(select = false) t =
    let c = t.index in
    let line = find_line c t.cursor_pos in
    let target =
      if line + 1 < Source.line_count c then Source.line_start c (line + 1) - 1
      else Source.length c
    in
    if (not select) && has_selection t then begin
      t.cursor_pos <- target;
      t.selection_anchor <- None;
      t.affinity <- Layout.Downstream;
      true
    end
    else if t.cursor_pos >= target then false
    else update_cursor_with_selection t ~select target

  let set_cursor_offset ?(select = false) t pos =
    let c = t.index in
    let pos = Int.max 0 (Int.min pos (Source.length c)) in
    ignore (update_cursor_with_selection t ~select pos : bool)

  let set_caret ?(select = false) t (caret : Layout.caret) =
    set_cursor_offset ~select t caret.grapheme;
    t.affinity <- caret.affinity
end

type t = Engine.t

let of_string ?(max_length = max_int) text = Engine.create ~max_length text
let empty = of_string ""
let text = Engine.text
let length = Engine.length
let cursor = Engine.cursor
let caret = Engine.caret
let selection = Engine.selection
let selected_text = Engine.selected_text
let source = Engine.source

(* An operation that changes nothing returns its argument, so it leaves an open
   undo step open; one that changes something but records no text change, such
   as a caret move, ends it. *)
let transform f t =
  let next = Engine.copy t in
  f next;
  if Engine.same t next && t.Engine.preferred_col = None then t
  else begin
    if next.undo_stack == t.undo_stack then next.open_step <- None;
    next.preferred_col <- None;
    if not next.viewport.follow then
      next.viewport <- { next.viewport with follow = true };
    next
  end

let insert text = transform (fun t -> ignore (Engine.insert t text : bool))
let clear = transform (fun t -> ignore (Engine.clear t : bool))

let set_cursor ?(select = false) pos =
  transform (fun t -> Engine.set_cursor_offset ~select t pos)

let set_caret ?(select = false) caret =
  transform (fun t -> Engine.set_caret ~select t caret)

let select ~anchor ~cursor =
  transform (fun t ->
      Engine.set_cursor t anchor;
      Engine.set_cursor_offset ~select:true t cursor)

let clear_selection = transform Engine.clear_selection
let select_all = transform Engine.select_all

let delete_backward =
  transform (fun t -> ignore (Engine.delete_backward t : bool))

let delete_forward =
  transform (fun t -> ignore (Engine.delete_forward t : bool))

let delete_word_backward =
  transform (fun t -> ignore (Engine.delete_word_backward t : bool))

let delete_word_forward =
  transform (fun t -> ignore (Engine.delete_word_forward t : bool))

let delete_to_line_start =
  transform (fun t -> ignore (Engine.delete_to_line_start t : bool))

let delete_to_line_end =
  transform (fun t -> ignore (Engine.delete_to_line_end t : bool))

let delete_line = transform (fun t -> ignore (Engine.delete_line t : bool))
let undo = transform (fun t -> ignore (Engine.undo t : bool))
let redo = transform (fun t -> ignore (Engine.redo t : bool))

let move_left ?(select = false) =
  transform (fun t -> ignore (Engine.move_left ~select t : bool))

let move_right ?(select = false) =
  transform (fun t -> ignore (Engine.move_right ~select t : bool))

let move_word_backward ?(select = false) =
  transform (fun t -> ignore (Engine.move_word_backward ~select t : bool))

let move_word_forward ?(select = false) =
  transform (fun t -> ignore (Engine.move_word_forward ~select t : bool))

let move_home ?(select = false) =
  transform (fun t -> ignore (Engine.move_home ~select t : bool))

let move_end ?(select = false) =
  transform (fun t -> ignore (Engine.move_end ~select t : bool))

let move_line_start ?(select = false) =
  transform (fun t -> ignore (Engine.move_line_start ~select t : bool))

let move_line_end ?(select = false) =
  transform (fun t -> ignore (Engine.move_line_end ~select t : bool))

let preferred_col t = t.Engine.preferred_col

let viewport t =
  let { Engine.x; y; follow } = t.Engine.viewport in
  (x, y, follow)

let with_viewport ~x ~y ~follow t =
  let viewport = { Engine.x = max 0 x; y = max 0 y; follow } in
  if viewport = t.Engine.viewport then t else { t with viewport }

let select_carets ~anchor ~cursor =
  transform (fun t ->
      Engine.set_caret t anchor;
      Engine.set_caret ~select:true t cursor)

(* A proposal that changes nothing is [base] itself, so the surface sees no
   change; otherwise it is [next] with the column vertical motion keeps. *)
let propose ~base ~preferred_col next =
  if Engine.same base next && base.Engine.preferred_col = preferred_col then
    base
  else if next.Engine.preferred_col = preferred_col then next
  else { next with preferred_col }
