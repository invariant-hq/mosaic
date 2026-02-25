(* ───── Spans ───── *)

type span = { text : string; style : Ansi.Style.t }

(* ───── Highlights ───── *)

module Highlight = struct
  type t = {
    start_offset : int;
    end_offset : int;
    style : Ansi.Style.t;
    priority : int;
    ref_id : int;
  }

  let make ~start_offset ~end_offset ~style ?(priority = 0) ~ref_id () =
    { start_offset; end_offset; style; priority; ref_id }

  let start_offset h = h.start_offset
  let end_offset h = h.end_offset
  let style h = h.style
  let priority h = h.priority
  let ref_id h = h.ref_id
end

(* ───── Line Cache ───── *)

(* Line metrics paired with per-line byte-range bounds, computed together to
   avoid scanning for line breaks twice. *)
type line_cache = {
  line_count : int;
  line_widths : int array;
  max_line_width : int;
  bounds : (int * int) array;
}

(* ───── Buffer ───── *)

type t = {
  mutable spans : span list;
  mutable spans_rev : bool;
  mutable default_style : Ansi.Style.t;
  mutable highlights : Highlight.t list;
  mutable tab_width : int;
  mutable width_method : Glyph.width_method;
  mutable cached_plain_text : string option;
  mutable cached_lines : line_cache option;
  mutable cached_grapheme_count : int option;
  mutable version : int;
}

let create ?(default_style = Ansi.Style.default) ?(width_method = `Unicode)
    ?(tab_width = 2) () =
  {
    spans = [];
    spans_rev = false;
    default_style;
    highlights = [];
    tab_width = max 1 tab_width;
    width_method;
    cached_plain_text = None;
    cached_lines = None;
    cached_grapheme_count = None;
    version = 0;
  }

(* ───── Invalidation ───── *)

let invalidate t =
  t.cached_plain_text <- None;
  t.cached_lines <- None;
  t.cached_grapheme_count <- None;
  t.version <- t.version + 1

(* ───── Span Order ───── *)

let ensure_span_order t =
  if t.spans_rev then begin
    t.spans <- List.rev t.spans;
    t.spans_rev <- false
  end

let ensure_spans_rev t =
  if not t.spans_rev then begin
    t.spans <- List.rev t.spans;
    t.spans_rev <- true
  end

(* ───── Content ───── *)

let set_text t s =
  t.spans <- [ { text = s; style = t.default_style } ];
  t.spans_rev <- false;
  invalidate t

let set_styled_text t spans =
  t.spans <- spans;
  t.spans_rev <- false;
  invalidate t

let append t s =
  ensure_spans_rev t;
  t.spans <- { text = s; style = t.default_style } :: t.spans;
  invalidate t

let append_styled t new_spans =
  ensure_spans_rev t;
  t.spans <- List.rev_append new_spans t.spans;
  invalidate t

let clear t =
  t.spans <- [];
  t.spans_rev <- false;
  t.highlights <- [];
  invalidate t

let plain_text t =
  match t.cached_plain_text with
  | Some s -> s
  | None ->
      ensure_span_order t;
      let s =
        match t.spans with
        | [] -> ""
        | [ s ] -> s.text
        | spans ->
            let buf = Buffer.create 256 in
            List.iter (fun s -> Buffer.add_string buf s.text) spans;
            Buffer.contents buf
      in
      t.cached_plain_text <- Some s;
      s

let grapheme_count t =
  match t.cached_grapheme_count with
  | Some n -> n
  | None ->
      let n =
        List.fold_left
          (fun acc s -> acc + Glyph.String.grapheme_count s.text)
          0 t.spans
      in
      t.cached_grapheme_count <- Some n;
      n

(* ───── Default Style ───── *)

let default_style t = t.default_style
let set_default_style t s = t.default_style <- s

(* ───── Line Info ───── *)

(* Compute logical lines by scanning for line breaks, returning both metrics and
   byte-range bounds. The bounds are reused by line_spans to avoid rescanning
   the full text for every line lookup. *)
let compute_lines t =
  let tab_width = t.tab_width in
  let full_text = plain_text t in
  let text_len = String.length full_text in
  if text_len = 0 then
    {
      line_count = 1;
      line_widths = [| 0 |];
      max_line_width = 0;
      bounds = [| (0, 0) |];
    }
  else begin
    let breaks = ref [] in
    Glyph.String.iter_line_breaks
      (fun ~pos ~kind -> breaks := (pos, kind) :: !breaks)
      full_text;
    let breaks = List.rev !breaks in
    let lines = ref [] in
    let bounds = ref [] in
    let line_start = ref 0 in
    List.iter
      (fun (brk_pos, kind) ->
        (* For CRLF, the CR precedes the LF at brk_pos; exclude it *)
        let line_end =
          match kind with `CRLF -> brk_pos - 1 | `LF | `CR -> brk_pos
        in
        let line = String.sub full_text !line_start (line_end - !line_start) in
        lines := line :: !lines;
        bounds := (!line_start, line_end) :: !bounds;
        line_start := brk_pos + 1)
      breaks;
    let last =
      if !line_start < text_len then
        String.sub full_text !line_start (text_len - !line_start)
      else ""
    in
    lines := last :: !lines;
    bounds := (!line_start, text_len) :: !bounds;
    let lines = Array.of_list (List.rev !lines) in
    let bounds = Array.of_list (List.rev !bounds) in
    let width_method = t.width_method in
    let widths =
      Array.map
        (fun line -> Glyph.String.measure ~width_method ~tab_width line)
        lines
    in
    let max_w = Array.fold_left max 0 widths in
    {
      line_count = Array.length widths;
      line_widths = widths;
      max_line_width = max_w;
      bounds;
    }
  end

let ensure_line_cache t =
  match t.cached_lines with
  | Some _ -> ()
  | None -> t.cached_lines <- Some (compute_lines t)

let line_count t =
  ensure_line_cache t;
  (Option.get t.cached_lines).line_count

let line_width t n =
  ensure_line_cache t;
  let cache = Option.get t.cached_lines in
  if n < 0 || n >= cache.line_count then 0 else cache.line_widths.(n)

let max_line_width t =
  ensure_line_cache t;
  (Option.get t.cached_lines).max_line_width

(* ───── Line Spans ───── *)

(* Return styled spans for a specific logical line using cached byte-range
   bounds, then mapping that range back to the original spans. *)
let line_spans t line_idx =
  ensure_span_order t;
  let full_text = plain_text t in
  let text_len = String.length full_text in
  if text_len = 0 then
    if line_idx = 0 then [ { text = ""; style = t.default_style } ] else []
  else begin
    ensure_line_cache t;
    let cache = Option.get t.cached_lines in
    if line_idx < 0 || line_idx >= cache.line_count then []
    else begin
      let start_byte, end_byte = cache.bounds.(line_idx) in
      if start_byte >= end_byte then [ { text = ""; style = t.default_style } ]
      else begin
        let result = ref [] in
        let offset = ref 0 in
        List.iter
          (fun (s : span) ->
            let slen = String.length s.text in
            let s_start = !offset in
            let s_end = s_start + slen in
            offset := s_end;
            let lo = max s_start start_byte in
            let hi = min s_end end_byte in
            if lo < hi then begin
              let sub = String.sub s.text (lo - s_start) (hi - lo) in
              result := { text = sub; style = s.style } :: !result
            end)
          t.spans;
        List.rev !result
      end
    end
  end

(* ───── Text In Range ───── *)

let text_in_range t ~start ~len =
  if len <= 0 then ""
  else
    let full = plain_text t in
    let text_len = String.length full in
    if text_len = 0 then ""
    else
      let byte_start = ref (-1) in
      let byte_end = ref text_len in
      let gi = ref 0 in
      Glyph.String.iter_graphemes
        (fun ~offset ~len:_ ->
          if !gi = start then byte_start := offset;
          if !gi = start + len then byte_end := offset;
          incr gi)
        full;
      if !byte_start < 0 then ""
      else String.sub full !byte_start (!byte_end - !byte_start)

(* ───── Highlights ───── *)

let add_highlight t h = t.highlights <- h :: t.highlights

let remove_highlights_by_ref t ref_id =
  t.highlights <-
    List.filter (fun h -> Highlight.ref_id h <> ref_id) t.highlights

let clear_highlights t = t.highlights <- []

let highlights_in_range t ~start ~len =
  let end_offset = start + len in
  t.highlights
  |> List.filter (fun h ->
      Highlight.start_offset h < end_offset && Highlight.end_offset h > start)
  |> List.sort (fun a b ->
      compare (Highlight.priority a) (Highlight.priority b))

(* ───── Tab Width ───── *)

let tab_width t = t.tab_width

let set_tab_width t w =
  let w = max 1 w in
  if t.tab_width <> w then begin
    t.tab_width <- w;
    t.cached_lines <- None;
    t.version <- t.version + 1
  end

(* ───── Width Method ───── *)

let width_method t = t.width_method

let set_width_method t m =
  if t.width_method <> m then begin
    t.width_method <- m;
    t.cached_lines <- None;
    t.version <- t.version + 1
  end

(* ───── Versioning ───── *)

let version t = t.version
