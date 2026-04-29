(* ───── Types ───── *)

type wrap = [ `None | `Char | `Word ]
type display_line = Text_buffer.span list

type display_info = {
  lines : display_line array;
  line_sources : int array;
  line_grapheme_offsets : int array;
  line_wrap_indices : int array;
  max_line_width : int;
}

type selection_state = {
  mutable anchor_offset : int;
  mutable focus_offset : int;
  mutable active : bool;
}

type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  mutable wrap : wrap;
  mutable wrap_width : int option;
  mutable truncate : bool;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable selection_bg : Ansi.Color.t option;
  mutable selection_fg : Ansi.Color.t option;
  selection : selection_state;
  mutable cached_display_info : (int * display_info) option;
      (* (effective_wrap_width, info) — keyed on width to invalidate on resize *)
  mutable measure_cache :
    (wrap * int * int * int * int)
    (* wrap_mode, width, version -> lines, max_w *)
    option;
}

(* ───── Accessors ───── *)

let buffer t = t.buffer
let node t = t.node
let wrap t = t.wrap

(* ───── Span slicing ───── *)

(* Extract the sub-spans of [spans] that cover the byte range [start_byte,
   end_byte) within the concatenated text. *)
let slice_spans spans ~start_byte ~end_byte =
  let result = ref [] in
  let offset = ref 0 in
  List.iter
    (fun (s : Text_buffer.span) ->
      let slen = String.length s.text in
      let s_start = !offset in
      let s_end = s_start + slen in
      offset := s_end;
      let lo = max s_start start_byte in
      let hi = min s_end end_byte in
      if lo < hi then begin
        let sub = String.sub s.text (lo - s_start) (hi - lo) in
        result := { Text_buffer.text = sub; style = s.style } :: !result
      end)
    spans;
  List.rev !result

(* ───── Display Line Computation ───── *)

(* Split a single logical line's spans into display lines based on wrapping.
   Returns a list of display lines (each a span list). *)
let wrap_none_line spans =
  (* No wrapping: entire logical line is one display line *)
  [| spans |]

(* Wrap a logical line at character (grapheme) boundaries to fit within width *)
let wrap_char_line ~width ~tab_width ~width_method spans =
  if width <= 0 then [| spans |]
  else begin
    let result = ref [] in
    let current_spans = ref [] in
    let current_width = ref 0 in
    List.iter
      (fun (span : Text_buffer.span) ->
        let text = span.text in
        let text_len = String.length text in
        if text_len = 0 then ()
        else begin
          let chunk_start = ref 0 in
          Matrix.Text.iter_grapheme_info ~width_method ~tab_width
            (fun ~offset ~len ~width:gw ->
              if !current_width + gw > width && !current_width > 0 then begin
                (* Emit current chunk before wrap *)
                if !chunk_start < offset then begin
                  let sub =
                    String.sub text !chunk_start (offset - !chunk_start)
                  in
                  current_spans :=
                    { Text_buffer.text = sub; style = span.style }
                    :: !current_spans
                end;
                result := List.rev !current_spans :: !result;
                current_spans := [];
                current_width := 0;
                chunk_start := offset
              end;
              current_width := !current_width + gw;
              ignore len)
            text;
          (* Emit remaining text from this span *)
          if !chunk_start < text_len then begin
            let sub = String.sub text !chunk_start (text_len - !chunk_start) in
            current_spans :=
              { Text_buffer.text = sub; style = span.style } :: !current_spans
          end
        end)
      spans;
    (* Emit final line *)
    result := List.rev !current_spans :: !result;
    Array.of_list (List.rev !result)
  end

(* Wrap a logical line at word boundaries to fit within width *)
let wrap_word_line ~width ~tab_width ~width_method spans =
  if width <= 0 then [| spans |]
  else begin
    (* Concatenate span texts to get the full logical line *)
    let full_text =
      match spans with
      | [] -> ""
      | [ (s : Text_buffer.span) ] -> s.text
      | _ ->
          let buf = Buffer.create 128 in
          List.iter
            (fun (s : Text_buffer.span) -> Buffer.add_string buf s.text)
            spans;
          Buffer.contents buf
    in
    let full_len = String.length full_text in
    if full_len = 0 then [| spans |]
    else begin
      (* Collect word break opportunities with their byte offsets and the
         grapheme offset at the break *)
      let breaks = ref [] in
      Matrix.Text.iter_wrap_breaks ~width_method
        (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset:_ ->
          breaks := next_byte_offset :: !breaks)
        full_text;
      let breaks = Array.of_list (List.rev !breaks) in
      if Array.length breaks = 0 then
        (* No break opportunities: try char wrap as fallback *)
        wrap_char_line ~width ~tab_width ~width_method spans
      else begin
        (* Find wrap points: walk graphemes, track width, use breaks. Use a
           cursor into the sorted breaks array for O(g + b) total. *)
        let wrap_points = ref [] in
        let line_width = ref 0 in
        let absolute_col = ref 0 in
        let last_break_byte = ref 0 in
        let best_break = ref (-1) in
        let best_break_col = ref 0 in
        let break_cursor = ref 0 in
        let nbreaks = Array.length breaks in
        Matrix.Text.iter_grapheme_info ~width_method ~tab_width
          (fun ~offset ~len:_ ~width:gw ->
            (* Advance cursor past any breaks at or before this offset *)
            while !break_cursor < nbreaks && breaks.(!break_cursor) <= offset do
              if breaks.(!break_cursor) = offset && offset > !last_break_byte
              then begin
                best_break := offset;
                best_break_col := !absolute_col
              end;
              incr break_cursor
            done;
            if !line_width + gw > width && !line_width > 0 then begin
              (* Need to wrap *)
              let wrap_at =
                if !best_break > !last_break_byte then !best_break
                else offset (* Fall back to char break *)
              in
              wrap_points := wrap_at :: !wrap_points;
              last_break_byte := wrap_at;
              line_width :=
                if wrap_at = offset then gw
                else (!absolute_col - !best_break_col) + gw
            end
            else line_width := !line_width + gw;
            absolute_col := !absolute_col + gw)
          full_text;
        let wrap_points = List.rev !wrap_points in
        if wrap_points = [] then [| spans |]
        else begin
          let all_points = wrap_points @ [ full_len ] in
          let lines = ref [] in
          let line_start_byte = ref 0 in
          List.iter
            (fun end_byte ->
              lines :=
                slice_spans spans ~start_byte:!line_start_byte ~end_byte
                :: !lines;
              line_start_byte := end_byte)
            all_points;
          Array.of_list (List.rev !lines)
        end
      end
    end
  end

(* Truncate a display line to fit within [width] columns, appending an ellipsis
   character when content is cut. The ellipsis occupies one column. *)
let truncate_line ~width ~tab_width ~width_method spans =
  if width <= 0 then spans
  else begin
    let total_width =
      List.fold_left
        (fun acc (s : Text_buffer.span) ->
          acc + Matrix.Text.measure ~width_method ~tab_width s.text)
        0 spans
    in
    if total_width <= width then spans
    else begin
      (* Reserve one column for the ellipsis *)
      let target = width - 1 in
      let result = ref [] in
      let col = ref 0 in
      let done_ = ref false in
      let last_style = ref Ansi.Style.default in
      List.iter
        (fun (span : Text_buffer.span) ->
          if not !done_ then begin
            last_style := span.style;
            let text = span.text in
            let text_len = String.length text in
            if text_len > 0 then begin
              let chunk_end = ref 0 in
              Matrix.Text.iter_grapheme_info ~width_method ~tab_width
                (fun ~offset ~len ~width:gw ->
                  if not !done_ then begin
                    if !col + gw > target then begin
                      if !chunk_end > 0 then
                        result :=
                          {
                            Text_buffer.text = String.sub text 0 !chunk_end;
                            style = span.style;
                          }
                          :: !result;
                      done_ := true
                    end
                    else begin
                      col := !col + gw;
                      chunk_end := offset + len
                    end
                  end)
                text;
              if not !done_ then
                result := { Text_buffer.text; style = span.style } :: !result
            end
          end)
        spans;
      let ellipsis =
        { Text_buffer.text = "\xe2\x80\xa6"; style = !last_style }
      in
      List.rev (ellipsis :: !result)
    end
  end

let effective_wrap_width t ?override () =
  match override with
  | Some w -> w
  | None -> (
      match t.wrap_width with Some w -> w | None -> Renderable.width t.node)

let compute_display_info t ?wrap_width () =
  let tab_width = Text_buffer.tab_width t.buffer in
  let width_method = Text_buffer.width_method t.buffer in
  let line_count = Text_buffer.line_count t.buffer in
  let all_lines = ref [] in
  let all_sources = ref [] in
  let all_grapheme_offsets = ref [] in
  let all_wrap_indices = ref [] in
  let grapheme_offset = ref 0 in
  for i = 0 to line_count - 1 do
    (* Account for the newline grapheme between logical lines *)
    if i > 0 then incr grapheme_offset;
    let spans = Text_buffer.line_spans t.buffer i in
    let display_lines =
      match t.wrap with
      | `None ->
          let lines = wrap_none_line spans in
          if t.truncate then begin
            let w = effective_wrap_width t ?override:wrap_width () in
            Array.map (truncate_line ~width:w ~tab_width ~width_method) lines
          end
          else lines
      | `Char ->
          let w = effective_wrap_width t ?override:wrap_width () in
          wrap_char_line ~width:w ~tab_width ~width_method spans
      | `Word ->
          let w = effective_wrap_width t ?override:wrap_width () in
          wrap_word_line ~width:w ~tab_width ~width_method spans
    in
    let wrap_idx = ref 0 in
    Array.iter
      (fun line ->
        all_grapheme_offsets := !grapheme_offset :: !all_grapheme_offsets;
        all_wrap_indices := !wrap_idx :: !all_wrap_indices;
        incr wrap_idx;
        let line_graphemes =
          List.fold_left
            (fun acc (s : Text_buffer.span) ->
              acc + Matrix.Text.grapheme_count s.text)
            0 line
        in
        grapheme_offset := !grapheme_offset + line_graphemes;
        all_lines := line :: !all_lines;
        all_sources := i :: !all_sources)
      display_lines
  done;
  let lines = Array.of_list (List.rev !all_lines) in
  let line_sources = Array.of_list (List.rev !all_sources) in
  let line_grapheme_offsets = Array.of_list (List.rev !all_grapheme_offsets) in
  let line_wrap_indices = Array.of_list (List.rev !all_wrap_indices) in
  let max_line_width =
    Array.fold_left
      (fun acc spans ->
        let w =
          List.fold_left
            (fun acc (s : Text_buffer.span) ->
              acc + Matrix.Text.measure ~width_method ~tab_width s.text)
            0 spans
        in
        max acc w)
      0 lines
  in
  {
    lines;
    line_sources;
    line_grapheme_offsets;
    line_wrap_indices;
    max_line_width;
  }

let display_info t =
  let ew = effective_wrap_width t () in
  match t.cached_display_info with
  | Some (w, info) when w = ew -> info
  | _ ->
      let info = compute_display_info t () in
      t.cached_display_info <- Some (ew, info);
      info

let display_line_count t = Array.length (display_info t).lines

(* ───── Scroll ───── *)

let scroll_x t = t.scroll_x
let scroll_y t = t.scroll_y
let scroll_height t = display_line_count t
let scroll_width t = (display_info t).max_line_width
let max_scroll_x t = max 0 (scroll_width t - Renderable.width t.node)
let max_scroll_y t = max 0 (scroll_height t - Renderable.height t.node)

let set_scroll_x t x =
  let clamped = max 0 (min x (max_scroll_x t)) in
  if t.scroll_x <> clamped then begin
    t.scroll_x <- clamped;
    Renderable.request_render t.node
  end

let set_scroll_y t y =
  let clamped = max 0 (min y (max_scroll_y t)) in
  if t.scroll_y <> clamped then begin
    t.scroll_y <- clamped;
    Renderable.request_render t.node
  end

(* ───── Wrapping ───── *)

let set_wrap t mode =
  if t.wrap <> mode then begin
    t.wrap <- mode;
    t.cached_display_info <- None;
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

let wrap_width t = t.wrap_width

let set_wrap_width t w =
  if t.wrap_width <> w then begin
    t.wrap_width <- w;
    t.cached_display_info <- None;
    t.measure_cache <- None;
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

let truncate t = t.truncate

let set_truncate t v =
  if t.truncate <> v then begin
    t.truncate <- v;
    t.cached_display_info <- None;
    t.measure_cache <- None;
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

(* ───── Invalidation ───── *)

let invalidate t =
  t.cached_display_info <- None;
  t.measure_cache <- None;
  Renderable.mark_dirty t.node;
  Renderable.request_render t.node

(* ───── Selection ───── *)

let set_selection_bg t bg =
  if not (Option.equal Ansi.Color.equal t.selection_bg bg) then begin
    t.selection_bg <- bg;
    if t.selection.active then Renderable.request_render t.node
  end

let set_selection_fg t fg =
  if not (Option.equal Ansi.Color.equal t.selection_fg fg) then begin
    t.selection_fg <- fg;
    if t.selection.active then Renderable.request_render t.node
  end

let normalize_selection sel =
  if sel.anchor_offset <= sel.focus_offset then
    (sel.anchor_offset, sel.focus_offset)
  else (sel.focus_offset, sel.anchor_offset)

(* Convert viewport-local (x, y) coordinates to a grapheme offset in the buffer.
   x is a column within the viewport, y is a row within the viewport. Both are
   adjusted for scroll offsets. Returns the grapheme offset of the character at
   that position, clamped to content bounds. *)
let local_coords_to_offset t ~x ~y =
  let info = display_info t in
  let display_y = t.scroll_y + y in
  let target_col = x + t.scroll_x in
  let nlines = Array.length info.lines in
  if display_y < 0 then 0
  else if display_y >= nlines then Text_buffer.grapheme_count t.buffer
  else begin
    let base_offset = info.line_grapheme_offsets.(display_y) in
    if target_col <= 0 then base_offset
    else begin
      let spans = info.lines.(display_y) in
      let tab_width = Text_buffer.tab_width t.buffer in
      let width_method = Text_buffer.width_method t.buffer in
      let col = ref 0 in
      let gi = ref 0 in
      let stop = ref false in
      List.iter
        (fun (span : Text_buffer.span) ->
          if not !stop then
            Matrix.Text.iter_grapheme_info ~width_method ~tab_width
              (fun ~offset:_ ~len:_ ~width:gw ->
                if not !stop then begin
                  if !col + gw > target_col then stop := true
                  else begin
                    col := !col + gw;
                    incr gi
                  end
                end)
              span.text)
        spans;
      base_offset + !gi
    end
  end

let set_selection t ~start ~end_ =
  let len = Text_buffer.grapheme_count t.buffer in
  let s = max 0 (min start len) in
  let e = max 0 (min end_ len) in
  let changed =
    (not t.selection.active)
    || t.selection.anchor_offset <> s
    || t.selection.focus_offset <> e
  in
  t.selection.anchor_offset <- s;
  t.selection.focus_offset <- e;
  t.selection.active <- s <> e;
  if changed then Renderable.request_render t.node;
  changed

let selection t =
  if t.selection.active then
    let s, e = normalize_selection t.selection in
    Some (s, e)
  else None

let set_local_selection t ~anchor_x ~anchor_y ~focus_x ~focus_y =
  let ao = local_coords_to_offset t ~x:anchor_x ~y:anchor_y in
  let fo = local_coords_to_offset t ~x:focus_x ~y:focus_y in
  let changed =
    (not t.selection.active)
    || t.selection.anchor_offset <> ao
    || t.selection.focus_offset <> fo
  in
  t.selection.anchor_offset <- ao;
  t.selection.focus_offset <- fo;
  t.selection.active <- ao <> fo;
  changed

let update_local_selection t ~anchor_x ~anchor_y ~focus_x ~focus_y =
  let ao = local_coords_to_offset t ~x:anchor_x ~y:anchor_y in
  let fo = local_coords_to_offset t ~x:focus_x ~y:focus_y in
  let changed =
    t.selection.anchor_offset <> ao || t.selection.focus_offset <> fo
  in
  t.selection.anchor_offset <- ao;
  t.selection.focus_offset <- fo;
  t.selection.active <- ao <> fo;
  changed

let reset_selection t =
  if t.selection.active then begin
    t.selection.active <- false;
    Renderable.request_render t.node
  end

let has_selection t = t.selection.active

let selected_text t =
  if not t.selection.active then ""
  else begin
    let start_off, end_off = normalize_selection t.selection in
    let len = end_off - start_off in
    if len <= 0 then ""
    else Text_buffer.text_in_range t.buffer ~start:start_off ~len
  end

(* ───── Rendering ───── *)

(* Compute the effective style for a single grapheme, layering highlights and
   selection on top of the span's base style. *)
let grapheme_style ~highlights ~sel_active ~sel_start ~sel_end ~sel_bg ~sel_fg
    ~base_style ~global_grapheme =
  let style =
    List.fold_left
      (fun acc hl ->
        if
          Text_buffer.Highlight.start_offset hl <= global_grapheme
          && Text_buffer.Highlight.end_offset hl > global_grapheme
        then
          Ansi.Style.merge ~base:acc ~overlay:(Text_buffer.Highlight.style hl)
        else acc)
      base_style highlights
  in
  if sel_active && global_grapheme >= sel_start && global_grapheme < sel_end
  then
    let s =
      match sel_bg with Some bg -> Ansi.Style.bg bg style | None -> style
    in
    match sel_fg with Some fg -> Ansi.Style.fg fg s | None -> s
  else style

let render t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else begin
    let info = display_info t in
    let lines = info.lines in
    let nlines = Array.length lines in
    let tab_width = Text_buffer.tab_width t.buffer in
    let width_method = Text_buffer.width_method t.buffer in
    let ox = Renderable.x t.node in
    let oy = Renderable.y t.node in
    (* Determine selection range for overlay (grapheme offsets) *)
    let sel_active = t.selection.active in
    let sel_start, sel_end =
      if sel_active then normalize_selection t.selection else (0, 0)
    in
    (* Collect highlights intersecting the visible display lines *)
    let first_visible = max 0 t.scroll_y in
    let last_visible = min (t.scroll_y + h - 1) (nlines - 1) in
    let highlights =
      if first_visible <= last_visible then begin
        let start_g = info.line_grapheme_offsets.(first_visible) in
        let last_line_graphemes =
          List.fold_left
            (fun acc (s : Text_buffer.span) ->
              acc + Matrix.Text.grapheme_count s.text)
            0 lines.(last_visible)
        in
        let end_g =
          info.line_grapheme_offsets.(last_visible) + last_line_graphemes
        in
        let len = end_g - start_g in
        if len > 0 then
          Text_buffer.highlights_in_range t.buffer ~start:start_g ~len
        else []
      end
      else []
    in
    for row = 0 to h - 1 do
      let display_y = t.scroll_y + row in
      if display_y >= 0 && display_y < nlines then begin
        let spans = lines.(display_y) in
        let col = ref (-t.scroll_x) in
        let grapheme_idx = ref 0 in
        let base_grapheme = info.line_grapheme_offsets.(display_y) in
        (* Batch state: accumulate contiguous same-style byte ranges within each
           span and flush once per style transition. *)
        let batch_start_col = ref 0 in
        let batch_start_byte = ref 0 in
        let batch_end_byte = ref 0 in
        let batch_style = ref Ansi.Style.default in
        let batch_active = ref false in
        let batch_text = ref "" in
        let flush () =
          if !batch_active && !batch_end_byte > !batch_start_byte then begin
            let sub =
              String.sub !batch_text !batch_start_byte
                (!batch_end_byte - !batch_start_byte)
            in
            Grid.draw_text ~style:!batch_style ~tab_width grid
              ~x:(ox + !batch_start_col) ~y:(oy + row) ~text:sub
          end;
          batch_active := false
        in
        List.iter
          (fun (span : Text_buffer.span) ->
            let text = span.text in
            if String.length text > 0 then begin
              (* New span: flush any pending batch from previous span *)
              flush ();
              batch_text := text;
              Matrix.Text.iter_grapheme_info ~width_method ~tab_width
                (fun ~offset ~len ~width:gw ->
                  if !col >= 0 && !col < w then begin
                    let abs_col = !col in
                    let global_grapheme = base_grapheme + !grapheme_idx in
                    let style =
                      grapheme_style ~highlights ~sel_active ~sel_start ~sel_end
                        ~sel_bg:t.selection_bg ~sel_fg:t.selection_fg
                        ~base_style:span.style ~global_grapheme
                    in
                    if
                      !batch_active
                      && Ansi.Style.equal !batch_style style
                      && !batch_end_byte = offset
                    then batch_end_byte := offset + len
                    else begin
                      flush ();
                      batch_start_col := abs_col;
                      batch_start_byte := offset;
                      batch_end_byte := offset + len;
                      batch_style := style;
                      batch_active := true
                    end
                  end;
                  incr grapheme_idx;
                  col := !col + gw)
                text
            end)
          spans;
        flush ()
      end
    done
  end

(* ───── Measurement ───── *)

let measure t ~known_dimensions ~available_space ~style:_ =
  let wrap_width =
    match available_space.Toffee.Geometry.Size.width with
    | Toffee.Available_space.Definite w ->
        let w = int_of_float w in
        if w > 0 then Some w else None
    | Min_content -> Some 1
    | Max_content -> None
  in
  let w_int = match wrap_width with Some w -> w | None -> 0 in
  let ver = Text_buffer.version t.buffer in
  let nlines, max_w =
    match t.measure_cache with
    | Some (wm, cw, cv, cl, cmw) when wm = t.wrap && cw = w_int && cv = ver ->
        (cl, cmw)
    | _ ->
        let info = compute_display_info t ?wrap_width () in
        let nl = Array.length info.lines in
        let mw = info.max_line_width in
        t.measure_cache <- Some (t.wrap, w_int, ver, nl, mw);
        (nl, mw)
  in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some w -> w
    | None -> (
        let w = float_of_int (max 1 max_w) in
        match available_space.width with
        | Definite aw when t.wrap <> `None -> Float.min w aw
        | _ -> w)
  in
  let height =
    match known_dimensions.Toffee.Geometry.Size.height with
    | Some h -> h
    | None -> float_of_int (max 1 nlines)
  in
  { Toffee.Geometry.Size.width; height }

(* ───── Construction ───── *)

let create node buffer =
  let t =
    {
      node;
      buffer;
      wrap = `None;
      wrap_width = None;
      truncate = false;
      scroll_x = 0;
      scroll_y = 0;
      selection_bg = None;
      selection_fg = None;
      selection = { anchor_offset = 0; focus_offset = 0; active = false };
      cached_display_info = None;
      measure_cache = None;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  t
