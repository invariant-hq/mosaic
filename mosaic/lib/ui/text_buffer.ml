open Bigarray

module Selection = struct
  type linear = { start : int; stop : int; style : Ansi.Style.t }

  type local = {
    anchor_x : int;
    anchor_y : int;
    focus_x : int;
    focus_y : int;
    style : Ansi.Style.t;
  }

  type t = None | Linear of linear | Local of local
end

module Chunk = struct
  type t = {
    text : bytes;
    fg : Ansi.Color.t option;
    bg : Ansi.Color.t option;
    attrs : Ansi.Attr.t;
    link : string option;
  }
end

module Virtual_line = struct
  type t = {
    line_index : int;
    start_index : int;
    length : int;
    width : int;
    char_offset : int;
    source_col_offset : int;
  }
end

type line_info = { starts : int array; widths : int array; max_width : int }
type width_method = Glyph.width_method
type wrap_mode = [ `Char | `Word ]

type style_snapshot = {
  fg : (float, float32_elt, c_layout) Array1.t;
  fg_mask : (int, int8_unsigned_elt, c_layout) Array1.t;
  bg : (float, float32_elt, c_layout) Array1.t;
  bg_mask : (int, int8_unsigned_elt, c_layout) Array1.t;
  attrs : (int32, int32_elt, c_layout) Array1.t;
  links : (int, int_elt, c_layout) Array1.t;
}

let is_grapheme_id = Glyph.is_start
let is_continuation = Glyph.is_continuation
let min_wrap_width = 1

type cell = {
  mutable code : int;
  mutable width : int;
  mutable base_style : Ansi.Style.t;
}

let empty_cell () = { code = 0; width = 0; base_style = Ansi.Style.default }

type highlight = {
  col_start : int;
  col_end : int;
  style : Ansi.Style.t;
  priority : int;
  ref_id : int;
}

type style_span = { col : int; next_col : int; style : Ansi.Style.t option }
type highlight_event = { col : int; is_start : bool; hl : highlight }

type virtual_lines_snapshot = {
  lines : Virtual_line.t array;
  line_starts : int array;
  line_widths : int array;
  line_first_vline : int array;
  line_vline_counts : int array;
}

module Color_plane = struct
  let channels = 4

  let create capacity =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (capacity * channels)

  let resize plane capacity =
    let new_plane =
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
        (capacity * channels)
    in
    let copy =
      Int.min (Bigarray.Array1.dim plane) (Bigarray.Array1.dim new_plane)
    in
    Bigarray.Array1.blit
      (Bigarray.Array1.sub plane 0 copy)
      (Bigarray.Array1.sub new_plane 0 copy);
    new_plane

  let base idx = idx * channels

  let write plane idx color =
    let br = base idx in
    let r, g, b, a = Ansi.Color.to_rgba_f color in
    Bigarray.Array1.unsafe_set plane br r;
    Bigarray.Array1.unsafe_set plane (br + 1) g;
    Bigarray.Array1.unsafe_set plane (br + 2) b;
    Bigarray.Array1.unsafe_set plane (br + 3) a

  let[@warning "-32"] read plane idx =
    let br = base idx in
    let clamp f =
      Float.max 0. (Float.min 1. (Bigarray.Array1.unsafe_get plane f))
    in
    let r = clamp br in
    let g = clamp (br + 1) in
    let b = clamp (br + 2) in
    let a = clamp (br + 3) in
    let to_byte x = int_of_float (Float.round (x *. 255.)) in
    Ansi.Color.of_rgba (to_byte r) (to_byte g) (to_byte b) (to_byte a)
end

type line = { start : int; length : int }
type chunk_group = { mutable cg_start : int; mutable cg_length : int }

let empty_chunk_group () = { cg_start = 0; cg_length = 0 }

type t = {
  pool : Glyph.Pool.t;
  mutable width_method : width_method;
  (* Tab handling *)
  mutable tab_width : int;
  mutable tab_indicator : int option;
  mutable tab_indicator_color : Ansi.Color.t option;
  mutable default_fg : Ansi.Color.t option;
  mutable default_bg : Ansi.Color.t option;
  mutable default_attrs : Ansi.Attr.t option;
  mutable cells : cell array;
  mutable capacity : int;
  mutable length : int;
  mutable chars : (int, int_elt, c_layout) Array1.t;
  mutable fg : (float, float32_elt, c_layout) Array1.t;
  mutable fg_mask : (int, int8_unsigned_elt, c_layout) Array1.t;
  mutable bg : (float, float32_elt, c_layout) Array1.t;
  mutable bg_mask : (int, int8_unsigned_elt, c_layout) Array1.t;
  mutable attrs : (int32, int32_elt, c_layout) Array1.t;
  mutable link_indices : (int, int_elt, c_layout) Array1.t;
  link_lookup : (string, int) Hashtbl.t;
  mutable link_pool : string array;
  mutable link_pool_count : int;
  mutable widths : (int, int8_unsigned_elt, c_layout) Array1.t;
  mutable chunk_end_flags : (int, int8_unsigned_elt, c_layout) Array1.t;
  mutable lines : line array;
  mutable line_count : int;
  mutable lines_dirty : bool;
  mutable chunk_groups : chunk_group array;
  mutable chunk_group_count : int;
  mutable version : int;
  mutable styles_dirty : bool;
  mutable line_highlights : highlight list array;
  mutable line_spans : style_span list array;
  mutable highlight_batch_depth : int;
  dirty_span_lines : (int, unit) Hashtbl.t;
  (* Cached wrap break flags *)
  mutable wrap_break_flags : (int, int8_unsigned_elt, c_layout) Array1.t option;
  mutable wrap_flags_version : int;
  (* Cached virtual lines *)
  vlines_cache :
    (wrap_mode * int option, int * virtual_lines_snapshot) Hashtbl.t;
}

let mark_dirty t =
  t.lines_dirty <- true;
  t.version <- t.version + 1;
  t.styles_dirty <- true;
  Hashtbl.clear t.vlines_cache;
  ()

let version t = t.version

let resize_int32_ba ?(default = 0l) arr len =
  let new_arr = Array1.create int32 c_layout len in
  let copy = min (Array1.dim arr) len in
  Array1.blit (Array1.sub arr 0 copy) (Array1.sub new_arr 0 copy);
  if copy < len then
    for i = copy to len - 1 do
      Array1.unsafe_set new_arr i default
    done;
  new_arr

let resize_int_ba ?(default = 0) arr len =
  let new_arr = Array1.create int c_layout len in
  let copy = min (Array1.dim arr) len in
  Array1.blit (Array1.sub arr 0 copy) (Array1.sub new_arr 0 copy);
  if copy < len then
    for i = copy to len - 1 do
      Array1.unsafe_set new_arr i default
    done;
  new_arr

let resize_int8_ba arr len =
  let new_arr = Array1.create int8_unsigned c_layout len in
  let copy = min (Array1.dim arr) len in
  Array1.blit (Array1.sub arr 0 copy) (Array1.sub new_arr 0 copy);
  new_arr

let ensure_link_pool_capacity t required =
  if required > Array.length t.link_pool then (
    let new_capacity =
      if Array.length t.link_pool = 0 then max 16 required
      else max required (Array.length t.link_pool * 2)
    in
    let new_pool = Array.make new_capacity "" in
    Array.blit t.link_pool 0 new_pool 0 t.link_pool_count;
    t.link_pool <- new_pool)

let intern_link t url =
  match Hashtbl.find_opt t.link_lookup url with
  | Some idx -> idx
  | None ->
      let idx = t.link_pool_count in
      ensure_link_pool_capacity t (idx + 1);
      t.link_pool.(idx) <- url;
      t.link_pool_count <- idx + 1;
      Hashtbl.add t.link_lookup url idx;
      idx

let write_color plane mask idx = function
  | Some color ->
      Color_plane.write plane idx color;
      Array1.unsafe_set mask idx 1
  | None -> Array1.unsafe_set mask idx 0

let write_attrs arr idx attrs =
  Array1.unsafe_set arr idx (Int32.of_int (Ansi.Attr.pack attrs))

let write_link t idx = function
  | None -> Array1.unsafe_set t.link_indices idx (-1)
  | Some url ->
      let link_idx = intern_link t url in
      Array1.unsafe_set t.link_indices idx link_idx

let ensure_cell_capacity t required =
  if required > t.capacity then (
    let new_capacity = max (t.capacity * 2) required in
    let new_cells =
      Array.init new_capacity (fun i ->
          if i < t.length then t.cells.(i) else empty_cell ())
    in
    Array.blit t.cells 0 new_cells 0 t.length;
    t.cells <- new_cells;
    t.capacity <- new_capacity;
    t.chars <- resize_int_ba t.chars new_capacity;
    t.fg <- Color_plane.resize t.fg new_capacity;
    t.fg_mask <- resize_int8_ba t.fg_mask new_capacity;
    t.bg <- Color_plane.resize t.bg new_capacity;
    t.bg_mask <- resize_int8_ba t.bg_mask new_capacity;
    t.attrs <- resize_int32_ba t.attrs new_capacity;
    t.link_indices <- resize_int_ba ~default:(-1) t.link_indices new_capacity;
    t.widths <- resize_int8_ba t.widths new_capacity;
    t.chunk_end_flags <- resize_int8_ba t.chunk_end_flags new_capacity)

let ensure_chunk_group_capacity t required =
  let current = Array.length t.chunk_groups in
  if required > current then
    let new_capacity =
      let base = if current = 0 then 4 else current * 2 in
      max required base
    in
    let new_groups =
      Array.init new_capacity (fun i ->
          if i < t.chunk_group_count then t.chunk_groups.(i)
          else empty_chunk_group ())
    in
    t.chunk_groups <- new_groups

let create_internal ~capacity ~width_method pool =
  let capacity = max 1 capacity in
  {
    pool;
    width_method;
    tab_width = 2;
    tab_indicator = None;
    tab_indicator_color = None;
    default_fg = None;
    default_bg = None;
    default_attrs = None;
    cells = Array.init capacity (fun _ -> empty_cell ());
    capacity;
    length = 0;
    chars = Array1.create int c_layout capacity;
    fg = Color_plane.create capacity;
    fg_mask = Array1.create int8_unsigned c_layout capacity;
    bg = Color_plane.create capacity;
    bg_mask = Array1.create int8_unsigned c_layout capacity;
    attrs = Array1.create int32 c_layout capacity;
    link_indices =
      (let arr = Array1.create int c_layout capacity in
       for i = 0 to capacity - 1 do
         Array1.unsafe_set arr i (-1)
       done;
       arr);
    link_lookup = Hashtbl.create 16;
    link_pool = [||];
    link_pool_count = 0;
    widths = Array1.create int8_unsigned c_layout capacity;
    chunk_end_flags = Array1.create int8_unsigned c_layout capacity;
    lines = [||];
    line_count = 0;
    lines_dirty = true;
    chunk_groups = [||];
    chunk_group_count = 0;
    version = 0;
    styles_dirty = true;
    line_highlights = [||];
    line_spans = [||];
    highlight_batch_depth = 0;
    dirty_span_lines = Hashtbl.create 16;
    wrap_break_flags = None;
    wrap_flags_version = 0;
    vlines_cache = Hashtbl.create 16;
  }

let create ?glyph_pool ~capacity ~width_method () =
  let pool =
    match glyph_pool with Some p -> p | None -> Glyph.Pool.create ()
  in
  create_internal ~capacity ~width_method pool

let release_cell pool cell =
  let g = Glyph.unsafe_of_int cell.code in
  if is_grapheme_id g then Glyph.Pool.decref pool g

let clear_cell cell =
  cell.code <- 0;
  cell.width <- 0;
  cell.base_style <- Ansi.Style.default

let reset t =
  for i = 0 to t.length - 1 do
    let cell = t.cells.(i) in
    release_cell t.pool cell;
    clear_cell cell
  done;
  Hashtbl.clear t.link_lookup;
  t.link_pool <- [||];
  t.link_pool_count <- 0;
  t.length <- 0;
  t.line_count <- 0;
  t.lines_dirty <- true;
  t.chunk_group_count <- 0;
  t.styles_dirty <- true;
  t.version <- t.version + 1;
  t.line_highlights <- [||];
  t.line_spans <- [||];
  t.highlight_batch_depth <- 0;
  Hashtbl.clear t.dirty_span_lines;
  t.wrap_break_flags <- None;
  t.wrap_flags_version <- 0;
  Hashtbl.clear t.vlines_cache;
  (* Clear chunk boundary flags *)
  let len = Array1.dim t.chunk_end_flags in
  for i = 0 to len - 1 do
    Array1.unsafe_set t.chunk_end_flags i 0
  done;
  Array1.fill t.fg_mask 0;
  Array1.fill t.bg_mask 0;
  Array1.fill t.attrs Int32.zero;
  Array1.fill t.widths 0;
  Array1.fill t.link_indices (-1)

let set_width_method t method_ =
  if t.width_method <> method_ then (
    t.width_method <- method_;
    mark_dirty t)

let encode_chunk t chunk =
  let attrs = chunk.Chunk.attrs in
  let style =
    Ansi.Style.make ?fg:chunk.fg ?bg:chunk.bg ~bold:(Ansi.Attr.mem Bold attrs)
      ~dim:(Ansi.Attr.mem Dim attrs)
      ~italic:(Ansi.Attr.mem Italic attrs)
      ~underline:(Ansi.Attr.mem Underline attrs)
      ~blink:(Ansi.Attr.mem Blink attrs)
      ~inverse:(Ansi.Attr.mem Inverse attrs)
      ~hidden:(Ansi.Attr.mem Hidden attrs)
      ~strikethrough:(Ansi.Attr.mem Strikethrough attrs)
      ?link:chunk.link ()
  in
  let style =
    match chunk.link with
    | None -> style
    | Some url -> Ansi.Style.hyperlink url style
  in
  let codes = ref [] in
  let widths = ref [] in
  let styles = ref [] in
  let push code width st =
    codes := code :: !codes;
    widths := width :: !widths;
    styles := st :: !styles
  in
  let text = Bytes.to_string chunk.Chunk.text in
  let encode_segment segment =
    Glyph.Pool.encode t.pool ~width_method:t.width_method ~tab_width:t.tab_width
      (fun code ->
        let width = Glyph.width code in
        (* Keep continuations with width 0 so indices stay aligned. *)
        if width > 0 || Glyph.is_continuation code then (
          if Glyph.is_start code then Glyph.Pool.incref t.pool code;
          push (Glyph.to_int code) width style))
      segment
  in
  (* Track Windows CRLF and old Mac CR line breaks: normalize to a single LF. *)
  let prev_was_cr = ref false in
  Glyph.String.iter_graphemes
    (fun ~offset:off ~len ->
      let segment = String.sub text off len in
      let seg_len = String.length segment in
      if seg_len = 1 then
        let b = Char.code segment.[0] in
        (* Collapse CRLF into a single LF by skipping LF after a CR *)
        if !prev_was_cr && b = 10 then prev_was_cr := false
        else (
          prev_was_cr := false;
          if b = 13 then (
            (* CR -> normalize to LF *)
            push 10 0 style;
            prev_was_cr := true)
          else if b = 10 then
            (* LF *)
            push 10 0 style
          else if b = 9 then
            (* TAB placeholder; dynamic width elsewhere *)
            push 9 1 style
          else encode_segment segment)
      else if seg_len = 2 && segment.[0] = '\r' && segment.[1] = '\n' then (
        (* Grapheme clustering may combine CRLF; normalize to a single LF. *)
        push 10 0 style;
        prev_was_cr := false)
      else (
        prev_was_cr := false;
        encode_segment segment))
    text;
  let codes = Array.of_list (List.rev !codes) in
  let widths = Array.of_list (List.rev !widths) in
  let styles = Array.of_list (List.rev !styles) in
  (codes, widths, styles)

let insert_cells t ~index codes widths styles ~end_flags =
  let len = Array.length codes in
  if len > 0 then (
    ensure_cell_capacity t (t.length + len);
    for i = t.length - 1 downto index do
      let src = t.cells.(i) in
      let dst = t.cells.(i + len) in
      dst.code <- src.code;
      dst.width <- src.width;
      dst.base_style <- src.base_style;
      Array1.unsafe_set t.chars (i + len) (Array1.unsafe_get t.chars i);
      Array1.unsafe_set t.widths (i + len) (Array1.unsafe_get t.widths i);
      Array1.unsafe_set t.chunk_end_flags (i + len)
        (Array1.unsafe_get t.chunk_end_flags i)
    done;
    for i = 0 to len - 1 do
      let cell = t.cells.(index + i) in
      let code = codes.(i) in
      let width = widths.(i) in
      let style = styles.(i) in
      cell.code <- code;
      cell.width <- width;
      cell.base_style <- style;
      Array1.unsafe_set t.chars (index + i) code;
      Array1.unsafe_set t.widths (index + i) width;
      Array1.unsafe_set t.chunk_end_flags (index + i) end_flags.(i)
    done;
    t.length <- t.length + len;
    mark_dirty t)

let cell_index_of_chunk_group t index =
  if index < 0 || index > t.chunk_group_count then
    invalid_arg "Text_buffer.cell_index_of_chunk_group"
  else if index = t.chunk_group_count then t.length
  else t.chunk_groups.(index).cg_start

(* Compute line metadata. Placed early to satisfy forward references. *)
let newline_code = 10

let compute_lines t =
  if t.lines_dirty then (
    let lines = ref [] in
    let start = ref 0 in
    for idx = 0 to t.length - 1 do
      let code = Array1.unsafe_get t.chars idx in
      let is_newline = code = newline_code in
      if is_newline then (
        let line_len = idx - !start + 1 in
        lines := { start = !start; length = line_len } :: !lines;
        start := idx + 1)
    done;
    if !start < t.length then
      lines := { start = !start; length = t.length - !start } :: !lines
    else if !lines <> [] then lines := { start = !start; length = 0 } :: !lines;
    let lines = List.rev !lines in
    t.lines <- Array.of_list lines;
    t.line_count <- Array.length t.lines;
    t.lines_dirty <- false)

let line_count t =
  compute_lines t;
  t.line_count

let insert_chunk_group t ~index chunk =
  if index < 0 || index > t.chunk_group_count then
    invalid_arg "Text_buffer.insert_chunk_group"
  else
    let insert_pos = cell_index_of_chunk_group t index in
    let codes, widths, styles = encode_chunk t chunk in
    let len = Array.length codes in
    let end_flags = Array.make len 0 in
    if len > 0 then end_flags.(len - 1) <- 1;
    insert_cells t ~index:insert_pos codes widths styles ~end_flags;
    ensure_chunk_group_capacity t (t.chunk_group_count + 1);
    for i = t.chunk_group_count - 1 downto index do
      let dst = t.chunk_groups.(i + 1) in
      let src = t.chunk_groups.(i) in
      dst.cg_start <- src.cg_start;
      dst.cg_length <- src.cg_length
    done;
    let group = t.chunk_groups.(index) in
    group.cg_start <- insert_pos;
    group.cg_length <- len;
    t.chunk_group_count <- t.chunk_group_count + 1;
    for i = index + 1 to t.chunk_group_count - 1 do
      t.chunk_groups.(i).cg_start <- t.chunk_groups.(i).cg_start + len
    done;
    len

let write_chunk t chunk = insert_chunk_group t ~index:t.chunk_group_count chunk

let set_tab_width t width =
  (* Tab width must be at least 2 and even for consistent alignment. *)
  let clamped = if width < 2 then 2 else width in
  let width' = if clamped land 1 = 0 then clamped else clamped + 1 in
  if t.tab_width <> width' then (
    t.tab_width <- width';
    t.version <- t.version + 1;
    t.lines_dirty <- true)

let set_tab_indicator t indicator = t.tab_indicator <- indicator
let set_tab_indicator_color t color = t.tab_indicator_color <- color
let length t = t.length

(* definition moved earlier *)

let line_body_end t line =
  let raw_end = line.start + line.length in
  if line.length > 0 && Array1.unsafe_get t.chars (raw_end - 1) = newline_code
  then raw_end - 1
  else raw_end

(* Linear selection rendering is handled at draw time in Text_surface *)

(* Precompute word-wrap break flags for the buffer. A flag at index [i]
   indicates that a break is allowed after consuming the grapheme at [i]. *)
let compute_wrap_break_flags t : (int, int8_unsigned_elt, c_layout) Array1.t =
  match t.wrap_break_flags with
  | Some flags
    when t.wrap_flags_version = t.version && Array1.dim flags = t.length ->
      flags
  | _ ->
      let flags = Array1.create int8_unsigned c_layout t.length in
      for i = 0 to t.length - 1 do
        Array1.unsafe_set flags i 0
      done;
      (* Use Glyph.String.iter_wrap_breaks semantics: mark break opportunities
         after graphemes containing wrap-break characters (spaces, punctuation,
         etc.). For each grapheme, check if it contains any wrap-break
         codepoints. *)
      for i = 0 to t.length - 1 do
        let code = Array1.unsafe_get t.chars i in
        let g = Glyph.unsafe_of_int code in
        let width = Array1.unsafe_get t.widths i in
        if code = newline_code then
          (* Newline: always allow a break here. *)
          Array1.unsafe_set flags i 1
        else if width > 0 then (
          if
            (* Continuation slots carry packed glyph metadata; only start slots
               should be used to compute wrap breaks. *)
            Glyph.is_continuation g
          then ()
          else
            (* Check if this grapheme contains wrap-break characters *)
            let segment =
              if is_grapheme_id g then Glyph.Pool.to_string t.pool g
              else if code >= 0 && code < 128 then String.make 1 (Char.chr code)
              else if code <= 0x10FFFF then (
                let buf = Buffer.create 4 in
                Buffer.add_utf_8_uchar buf (Uchar.of_int code);
                Buffer.contents buf)
              else ""
            in
            let has_break = ref false in
            Glyph.String.iter_wrap_breaks
              (fun ~byte_offset:_ ~grapheme_offset:_ -> has_break := true)
              segment;
            if !has_break then Array1.unsafe_set flags i 1)
      done;
      (* Also mark chunk ends as break opportunities to preserve chunk
         semantics. *)
      for i = 0 to t.length - 1 do
        if Array1.unsafe_get t.chunk_end_flags i <> 0 then
          Array1.unsafe_set flags i 1
      done;
      t.wrap_break_flags <- Some flags;
      t.wrap_flags_version <- t.version;
      flags

let build_virtual_lines t ~wrap_mode ~wrap_width =
  let key = (wrap_mode, wrap_width) in
  match Hashtbl.find_opt t.vlines_cache key with
  | Some (cached_version, snapshot) when cached_version = t.version -> snapshot
  | _ ->
      compute_lines t;
      let wrap_break_flags = compute_wrap_break_flags t in
      let wrap_limit =
        match wrap_width with
        | Some w when w > 0 -> Some (max w min_wrap_width)
        | _ -> None
      in
      let virtuals = ref [] in
      let global_char_offset = ref 0 in
      let total_vlines = ref 0 in
      let first_vline = Array.make t.line_count 0 in
      let vline_counts = Array.make t.line_count 0 in
      let push line_idx start length width col_offset =
        virtuals :=
          {
            Virtual_line.line_index = line_idx;
            start_index = start;
            length;
            width;
            char_offset = !global_char_offset;
            source_col_offset = col_offset;
          }
          :: !virtuals;
        incr total_vlines;
        vline_counts.(line_idx) <- vline_counts.(line_idx) + 1;
        global_char_offset := !global_char_offset + width
      in
      for line_idx = 0 to t.line_count - 1 do
        let line = t.lines.(line_idx) in
        let raw_start = line.start in
        let raw_end = line.start + line.length in
        let body_end = line_body_end t line in
        let line_col_offset = ref 0 in
        first_vline.(line_idx) <- !total_vlines;
        let rec emit segment_start idx current_width last_break last_width =
          if idx >= body_end then
            let segment_length = raw_end - segment_start in
            push line_idx segment_start segment_length current_width
              !line_col_offset
          else
            let code = Array1.unsafe_get t.chars idx in
            let base_width = Array1.unsafe_get t.widths idx in
            let delta =
              if code = 9 then
                let w = max 1 t.tab_width in
                let next = ((current_width / w) + 1) * w in
                max 1 (next - current_width)
              else if base_width > 0 then base_width
              else 0
            in
            let next_width = current_width + delta in
            let limit_exceeded =
              match wrap_limit with
              | Some limit -> next_width > limit && limit > 0
              | None -> false
            in
            let is_chunk_boundary =
              Array1.unsafe_get t.chunk_end_flags idx <> 0
            in
            let updated_break, updated_break_width =
              if
                wrap_mode = `Word && delta > 0
                && (Array1.unsafe_get wrap_break_flags idx <> 0
                   || is_chunk_boundary)
              then (Some idx, next_width)
              else (last_break, last_width)
            in
            if limit_exceeded then
              match (wrap_mode, updated_break) with
              | `Word, Some break_idx when break_idx >= segment_start ->
                  let segment_len = break_idx - segment_start + 1 in
                  let segment_width = updated_break_width in
                  push line_idx segment_start segment_len segment_width
                    !line_col_offset;
                  line_col_offset := !line_col_offset + segment_width;
                  emit (break_idx + 1) (break_idx + 1) 0 None 0
              | _ ->
                  let segment_len = idx - segment_start in
                  let segment_width = current_width in
                  if segment_len <= 0 then
                    if delta > 0 then (
                      push line_idx segment_start 1 delta !line_col_offset;
                      line_col_offset := !line_col_offset + delta;
                      emit (idx + 1) (idx + 1) 0 None 0)
                    else emit (idx + 1) (idx + 1) 0 None 0
                  else (
                    push line_idx segment_start segment_len segment_width
                      !line_col_offset;
                    line_col_offset := !line_col_offset + segment_width;
                    emit idx idx 0 None 0)
            else
              let next_idx = idx + 1 in
              emit segment_start next_idx next_width updated_break
                updated_break_width
        in
        emit raw_start raw_start 0 None 0;
        if line_idx < t.line_count - 1 then
          global_char_offset := !global_char_offset + 1
      done;
      let virtuals =
        match List.rev !virtuals with
        | [] ->
            [
              {
                Virtual_line.line_index = 0;
                start_index = 0;
                length = 0;
                width = 0;
                char_offset = 0;
                source_col_offset = 0;
              };
            ]
        | lst -> lst
      in
      let lines = Array.of_list virtuals in
      let count = Array.length lines in
      let starts =
        Array.init count (fun i -> lines.(i).Virtual_line.char_offset)
      in
      let widths = Array.init count (fun i -> lines.(i).Virtual_line.width) in
      let snapshot =
        {
          lines;
          line_starts = starts;
          line_widths = widths;
          line_first_vline = first_vline;
          line_vline_counts = vline_counts;
        }
      in
      Hashtbl.replace t.vlines_cache key (t.version, snapshot);
      snapshot

let ensure_highlight_storage t line_idx =
  let grow arr =
    if line_idx < Array.length arr then arr
    else
      let old_len = Array.length arr in
      let new_len =
        if old_len = 0 then max 4 (line_idx + 1)
        else max (line_idx + 1) (old_len * 2)
      in
      let expanded = Array.make new_len [] in
      Array.blit arr 0 expanded 0 old_len;
      expanded
  in
  t.line_highlights <- grow t.line_highlights;
  t.line_spans <- grow t.line_spans

let logical_line_width t line_idx =
  compute_lines t;
  if line_idx < 0 || line_idx >= t.line_count then 0
  else
    let line = t.lines.(line_idx) in
    let raw_start = line.start in
    let raw_end = line.start + line.length in
    let width = ref 0 in
    for idx = raw_start to raw_end - 1 do
      let code = Array1.unsafe_get t.chars idx in
      let base_width = Array1.unsafe_get t.widths idx in
      if code = 9 then
        let w = max 1 t.tab_width in
        let next = ((!width / w) + 1) * w in
        width := !width + max 1 (next - !width)
      else if base_width > 0 then width := !width + base_width
    done;
    !width

let mark_line_spans_dirty t line_idx =
  Hashtbl.replace t.dirty_span_lines line_idx ()

let rebuild_line_spans t line_idx =
  if line_idx < Array.length t.line_spans then (
    let highlights =
      if line_idx < Array.length t.line_highlights then
        t.line_highlights.(line_idx)
      else []
    in
    if highlights = [] then t.line_spans.(line_idx) <- []
    else
      let events : highlight_event list =
        let gather acc hl =
          if hl.col_end <= hl.col_start then acc
          else
            { col = hl.col_start; is_start = true; hl }
            :: { col = hl.col_end; is_start = false; hl }
            :: acc
        in
        List.fold_left gather [] highlights
        |> List.sort (fun a b ->
            if a.col <> b.col then compare a.col b.col
            else if a.is_start = b.is_start then 0
            else if a.is_start then 1
            else -1)
      in
      (* Choose the style with highest priority. For equal priorities, the most
         recently added highlight wins (due to cons order). *)
      let highest_style active =
        let rec aux best_style best_priority = function
          | [] -> best_style
          | hl :: rest ->
              if hl.priority > best_priority then
                aux (Some hl.style) hl.priority rest
              else aux best_style best_priority rest
        in
        aux None (-1) active
      in
      let active : highlight list ref = ref [] in
      let current_col = ref 0 in
      let spans_rev = ref [] in
      List.iter
        (fun event ->
          let before_style = highest_style !active in
          if event.col > !current_col then
            spans_rev :=
              { col = !current_col; next_col = event.col; style = before_style }
              :: !spans_rev;
          if event.is_start then active := event.hl :: !active
          else active := List.filter (fun h -> h != event.hl) !active;
          current_col := event.col)
        events;
      let final_width = logical_line_width t line_idx in
      let trailing_style = highest_style !active in
      if !current_col < final_width then
        spans_rev :=
          { col = !current_col; next_col = final_width; style = trailing_style }
          :: !spans_rev;
      t.line_spans.(line_idx) <- List.rev !spans_rev)

let start_highlights_transaction t =
  t.highlight_batch_depth <- t.highlight_batch_depth + 1

let end_highlights_transaction t =
  if t.highlight_batch_depth > 0 then (
    t.highlight_batch_depth <- t.highlight_batch_depth - 1;
    if t.highlight_batch_depth = 0 then (
      Hashtbl.iter
        (fun line_idx _ -> rebuild_line_spans t line_idx)
        t.dirty_span_lines;
      Hashtbl.clear t.dirty_span_lines))

let add_highlight t ~line_idx ~col_start ~col_end ~style ~priority ~ref_id =
  if col_end <= col_start then ()
  else (
    ensure_highlight_storage t line_idx;
    let hl = { col_start; col_end; style; priority; ref_id } in
    let existing = t.line_highlights.(line_idx) in
    t.line_highlights.(line_idx) <- hl :: existing;
    if t.highlight_batch_depth = 0 then rebuild_line_spans t line_idx
    else mark_line_spans_dirty t line_idx)

let remove_highlights_by_ref t ~ref_id =
  for line_idx = 0 to Array.length t.line_highlights - 1 do
    let before = t.line_highlights.(line_idx) in
    let after = List.filter (fun hl -> hl.ref_id <> ref_id) before in
    if before != after then (
      t.line_highlights.(line_idx) <- after;
      if t.highlight_batch_depth = 0 then rebuild_line_spans t line_idx
      else mark_line_spans_dirty t line_idx)
  done

let clear_line_highlights t ~line_idx =
  if line_idx < Array.length t.line_highlights then (
    t.line_highlights.(line_idx) <- [];
    t.line_spans.(line_idx) <- [])

let clear_all_highlights t =
  for i = 0 to Array.length t.line_highlights - 1 do
    t.line_highlights.(i) <- [];
    t.line_spans.(i) <- []
  done;
  Hashtbl.clear t.dirty_span_lines;
  t.highlight_batch_depth <- 0

let line_spans t ~line_idx =
  if line_idx < Array.length t.line_spans then t.line_spans.(line_idx) else []

let rebuild_styles t =
  let default_attrs = Option.value t.default_attrs ~default:Ansi.Attr.empty in
  let default_style =
    Ansi.Style.make ?fg:t.default_fg ?bg:t.default_bg
      ~bold:(Ansi.Attr.mem Bold default_attrs)
      ~dim:(Ansi.Attr.mem Dim default_attrs)
      ~italic:(Ansi.Attr.mem Italic default_attrs)
      ~underline:(Ansi.Attr.mem Underline default_attrs)
      ~blink:(Ansi.Attr.mem Blink default_attrs)
      ~inverse:(Ansi.Attr.mem Inverse default_attrs)
      ~hidden:(Ansi.Attr.mem Hidden default_attrs)
      ~strikethrough:(Ansi.Attr.mem Strikethrough default_attrs)
      ()
  in
  for i = 0 to t.length - 1 do
    let overlay = t.cells.(i).base_style in
    let effective = Ansi.Style.merge ~base:default_style ~overlay in
    write_color t.fg t.fg_mask i effective.Ansi.Style.fg;
    write_color t.bg t.bg_mask i effective.Ansi.Style.bg;
    write_attrs t.attrs i effective.Ansi.Style.attrs;
    write_link t i effective.Ansi.Style.link
  done;
  t.styles_dirty <- false

let logical_line_info t =
  (* Ensure styles are up to date so width calculations (e.g. tabs) are
     consistent. *)
  if t.styles_dirty then rebuild_styles t;
  compute_lines t;
  let count = t.line_count in
  if count = 0 then { starts = [||]; widths = [||]; max_width = 0 }
  else
    let starts = Array.make count 0 in
    let widths = Array.make count 0 in
    let max_w = ref 0 in
    let running_offset = ref 0 in
    for line_idx = 0 to count - 1 do
      let line = t.lines.(line_idx) in
      let raw_start = line.start in
      let raw_end = line.start + line.length in
      let current_width = ref 0 in
      (* Record global char offset at start of this logical line *)
      starts.(line_idx) <- !running_offset;
      (* Sum visual widths across the logical line, expanding TABs to the next
         tab stop. *)
      for idx = raw_start to raw_end - 1 do
        let code = Array1.unsafe_get t.chars idx in
        let base_width = Array1.unsafe_get t.widths idx in
        if code = 9 then
          let w = max 1 t.tab_width in
          let next = ((!current_width / w) + 1) * w in
          current_width := !current_width + max 1 (next - !current_width)
        else if base_width > 0 then current_width := !current_width + base_width
        else ()
      done;
      widths.(line_idx) <- !current_width;
      if !current_width > !max_w then max_w := !current_width;
      (* Advance by line width + newline weight between lines *)
      if line_idx < count - 1 then
        running_offset := !running_offset + !current_width + 1
    done;
    { starts; widths; max_width = !max_w }

let line_info t = logical_line_info t

let char_offset_to_line_col t char_offset =
  let info = logical_line_info t in
  let rec find_line line_idx =
    if line_idx >= Array.length info.starts then None
    else
      let line_start = info.starts.(line_idx) in
      let line_end =
        if line_idx + 1 < Array.length info.starts then
          info.starts.(line_idx + 1) - 1
        else
          (* Last line: need to compute its end *)
          let line = t.lines.(line_idx) in
          line_start + line.length
      in
      if char_offset >= line_start && char_offset < line_end then
        Some (line_idx, char_offset - line_start)
      else find_line (line_idx + 1)
  in
  find_line 0

let add_highlight_by_char_range t ~char_start ~char_end ~style ~priority ~ref_id
    =
  match
    (char_offset_to_line_col t char_start, char_offset_to_line_col t char_end)
  with
  | Some (line_idx1, col_start), Some (line_idx2, col_end)
    when line_idx1 = line_idx2 ->
      add_highlight t ~line_idx:line_idx1 ~col_start ~col_end ~style ~priority
        ~ref_id
  | _ -> ()
(* For now, only handle single-line highlights. Multi-line would need
   splitting. *)

let finalise t = if t.styles_dirty then rebuild_styles t

let drawing_chars t =
  finalise t;
  t.chars

let drawing_styles t =
  finalise t;
  {
    fg = t.fg;
    fg_mask = t.fg_mask;
    bg = t.bg;
    bg_mask = t.bg_mask;
    attrs = t.attrs;
    links = t.link_indices;
  }

let drawing_widths t =
  finalise t;
  t.widths

let link_at_index t idx =
  if idx < 0 || idx >= t.link_pool_count then None else Some t.link_pool.(idx)

module View = struct
  type snapshot = {
    chars : (int, int_elt, c_layout) Bigarray.Array1.t;
    widths : (int, int8_unsigned_elt, c_layout) Bigarray.Array1.t;
    styles : style_snapshot;
  }

  let create buffer =
    finalise buffer;
    {
      chars = buffer.chars;
      widths = buffer.widths;
      styles =
        {
          fg = buffer.fg;
          fg_mask = buffer.fg_mask;
          bg = buffer.bg;
          bg_mask = buffer.bg_mask;
          attrs = buffer.attrs;
          links = buffer.link_indices;
        };
    }

  let[@inline] code view idx = Bigarray.Array1.unsafe_get view.chars idx
  let[@inline] width view idx = Bigarray.Array1.unsafe_get view.widths idx

  let[@inline] raw_attr_bits view idx =
    Bigarray.Array1.unsafe_get view.styles.attrs idx

  let attrs view idx =
    raw_attr_bits view idx |> Int32.to_int |> Ansi.Attr.unpack

  let[@inline] fg_mask view idx =
    Bigarray.Array1.unsafe_get view.styles.fg_mask idx <> 0

  let[@inline] bg_mask view idx =
    Bigarray.Array1.unsafe_get view.styles.bg_mask idx <> 0

  let fg_opt view idx =
    if fg_mask view idx then Some (Color_plane.read view.styles.fg idx)
    else None

  let bg_opt view idx =
    if bg_mask view idx then Some (Color_plane.read view.styles.bg idx)
    else None

  let fg_with_default view idx default =
    match fg_opt view idx with Some c -> c | None -> default

  let bg_with_default view idx default =
    match bg_opt view idx with Some c -> c | None -> default

  let raw_link view idx = Bigarray.Array1.unsafe_get view.styles.links idx
end

let append_code t buf code =
  let g = Glyph.unsafe_of_int code in
  if is_continuation g then ()
  else if code = 10 then Buffer.add_char buf '\n'
  else if is_grapheme_id g then
    Buffer.add_string buf (Glyph.Pool.to_string t.pool g)
  else if Uchar.is_valid code then
    Buffer.add_utf_8_uchar buf (Uchar.of_int code)

let get_plain_text t =
  let buf = Buffer.create (max 16 (t.length * 2)) in
  for idx = 0 to t.length - 1 do
    let code = Array1.unsafe_get t.chars idx in
    append_code t buf code
  done;
  Buffer.contents buf

let get_text_range t ~start ~stop =
  let start = max 0 (min start t.length) in
  let stop = max start (min stop t.length) in
  if start >= stop then ""
  else
    let buf = Buffer.create ((stop - start) * 2) in
    for idx = start to stop - 1 do
      let code = Array1.unsafe_get t.chars idx in
      append_code t buf code
    done;
    Buffer.contents buf

let set_default_fg t fg =
  t.default_fg <- fg;
  t.styles_dirty <- true

let set_default_bg t bg =
  t.default_bg <- bg;
  t.styles_dirty <- true

let set_default_attrs t attrs =
  t.default_attrs <- attrs;
  t.styles_dirty <- true

let grapheme_pool t = t.pool
let tab_width t = t.tab_width
let tab_indicator t = t.tab_indicator
let tab_indicator_color t = t.tab_indicator_color
