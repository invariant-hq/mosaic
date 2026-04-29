type cursor = { mutable row : int; mutable col : int; mutable visible : bool }
(* Internal cursor state *)

(* Style run for compressed scrollback - represents contiguous bytes with same
   style *)
type style_run = {
  start_byte : int; (* Byte offset in UTF-8 string *)
  end_byte : int; (* Byte offset (exclusive) *)
  fg : Ansi.Color.t;
  bg : Ansi.Color.t;
  attrs : Ansi.Attr.t;
}

(* Compressed scrollback line - text + sparse style runs *)
type scrollback_line = {
  text : string; (* UTF-8 encoded text *)
  styles : style_run array; (* Sorted by start_byte, non-overlapping *)
}

type scrollback = {
  lines : scrollback_line array; (* Ring buffer of compressed lines *)
  mutable head : int;
  mutable count : int;
  capacity : int;
}
(* Scrollback ring buffer with compressed storage *)

type scroll_region = { mutable top : int; mutable bottom : int }
(* Scroll region bounds *)

module Int_set = Set.Make (Int)

(* Compress a grid row into text + style runs with byte offsets using the new
   Grid/Text API. We avoid raw Bigarray access and rely on per-cell accessors.
   Continuation cells are skipped; each starting cell contributes exactly one
   grapheme string (including spaces). *)
(* Compress a grid row into text + style runs using public Grid/Text APIs. *)
let compress_row grid row cols =
  let buf = Buffer.create (cols * 2) in
  let styles_rev = ref [] in

  (* key = fg_r, fg_g, fg_b, fg_a, bg_r, bg_g, bg_b, bg_a, attrs_packed *)
  let current_key :
      (int * int * int * int * int * int * int * int * int) option ref =
    ref None
  in
  let current_fg = ref Ansi.Color.default in
  let current_bg = ref Ansi.Color.default in
  let current_attrs = ref Ansi.Attr.empty in
  let style_start_byte = ref 0 in

  let scale v = Float.round (v *. 255.) |> int_of_float in

  let flush_style_run end_byte =
    match !current_key with
    | None -> ()
    | Some _ ->
        let fg = !current_fg in
        let bg = !current_bg in
        let attrs = !current_attrs in
        styles_rev :=
          { start_byte = !style_start_byte; end_byte; fg; bg; attrs }
          :: !styles_rev
  in

  for col = 0 to cols - 1 do
    let idx = (row * cols) + col in

    (* Skip continuation cells: grapheme already handled at its start cell. *)
    if not (Grid.is_continuation grid idx) then
      (* Determine grapheme text for this cell. *)
      let text =
        if Grid.is_empty grid idx then " " else Grid.get_text grid idx
      in

      if text <> "" then (
        (* Extract raw style from cell. *)
        let fg_r = scale (Grid.get_fg_r grid idx) in
        let fg_g = scale (Grid.get_fg_g grid idx) in
        let fg_b = scale (Grid.get_fg_b grid idx) in
        let fg_a = scale (Grid.get_fg_a grid idx) in
        let bg_r = scale (Grid.get_bg_r grid idx) in
        let bg_g = scale (Grid.get_bg_g grid idx) in
        let bg_b = scale (Grid.get_bg_b grid idx) in
        let bg_a = scale (Grid.get_bg_a grid idx) in
        let attrs_packed = Grid.get_attrs grid idx in

        let key =
          (fg_r, fg_g, fg_b, fg_a, bg_r, bg_g, bg_b, bg_a, attrs_packed)
        in

        (* Style run change detection. *)
        (match !current_key with
        | None ->
            current_key := Some key;
            current_fg := Ansi.Color.of_rgba fg_r fg_g fg_b fg_a;
            current_bg := Ansi.Color.of_rgba bg_r bg_g bg_b bg_a;
            current_attrs := Ansi.Attr.unpack attrs_packed;
            style_start_byte := Buffer.length buf
        | Some prev when prev <> key ->
            (* Close previous run. *)
            let end_byte = Buffer.length buf in
            flush_style_run end_byte;
            current_key := Some key;
            current_fg := Ansi.Color.of_rgba fg_r fg_g fg_b fg_a;
            current_bg := Ansi.Color.of_rgba bg_r bg_g bg_b bg_a;
            current_attrs := Ansi.Attr.unpack attrs_packed;
            style_start_byte := end_byte
        | Some _ -> ());

        (* Append grapheme text. *)
        Buffer.add_string buf text)
  done;

  (* Finalize text and style runs. *)
  let text = Buffer.contents buf in
  let len = String.length text in

  (* Trim trailing spaces from text. *)
  let rec find_end i =
    if i < 0 then 0
    else if String.unsafe_get text i = ' ' then find_end (i - 1)
    else i + 1
  in
  let trimmed_len = find_end (len - 1) in
  let trimmed =
    if trimmed_len = len then text else String.sub text 0 trimmed_len
  in

  (* Flush final style run, trimmed to text length. *)
  flush_style_run trimmed_len;

  let trimmed_styles =
    List.rev_map
      (fun run -> { run with end_byte = min run.end_byte trimmed_len })
      !styles_rev
    |> Array.of_list
  in

  { text = trimmed; styles = trimmed_styles }

(* Decompress a scrollback line into a grid row using the public Grid/Text API.
   We assume the destination grid has been cleared beforehand. *)
let decompress_line line grid row _cols =
  if Array.length line.styles = 0 || String.length line.text = 0 then ()
  else
    let text = line.text in
    let text_len = String.length text in
    let col = ref 0 in
    let max_cols = Grid.width grid in
    let width_method = Grid.width_method grid in

    Array.iter
      (fun run ->
        let start_byte = max 0 run.start_byte in
        let end_byte = min run.end_byte text_len in

        if start_byte < end_byte && end_byte <= text_len && !col < max_cols then
          let seg_len = end_byte - start_byte in
          if seg_len > 0 then
            try
              let segment = String.sub text start_byte seg_len in
              if segment <> "" then (
                let style =
                  Ansi.Style.default |> Ansi.Style.fg run.fg
                  |> Ansi.Style.bg run.bg
                  |> Ansi.Style.with_attrs run.attrs
                in
                Grid.draw_text grid ~x:!col ~y:row ~style ~text:segment;
                let w = Text.measure ~width_method ~tab_width:2 segment in
                col := min max_cols (!col + w))
            with Invalid_argument _ ->
              (* Skip problematic segments *)
              ())
      line.styles

type dirty_state = {
  mutable dirty : bool;
  mutable dirty_rows : Int_set.t;
  mutable dirty_cursor : bool;
}

type t = {
  (* Screen grids *)
  primary : Grid.t;
  alternate : Grid.t;
  mutable active_grid : Grid.t;
  (* Dimensions *)
  mutable rows : int;
  mutable cols : int;
  (* Cursor state *)
  cursor : cursor;
  mutable saved_cursor : (int * int * bool) option;
  (* Graphics state *)
  mutable style : Ansi.Style.t;
  mutable saved_style : Ansi.Style.t option;
  (* Parser *)
  parser : Ansi.Parser.t;
  (* Terminal state *)
  mutable title : string;
  dirty_state : dirty_state;
  (* Scrollback *)
  scrollback : scrollback option;
  (* Scroll region *)
  scroll_region : scroll_region;
  (* Terminal modes *)
  mutable origin_mode : bool;
  mutable auto_wrap_mode : bool;
  mutable cursor_key_mode : bool;
  mutable insert_mode : bool;
  mutable bracketed_paste : bool;
  default_fg : Ansi.Color.t;
  default_bg : Ansi.Color.t;
}
(* VTE instance *)

let create ?(scrollback = 10000) ?width_method ?(respect_alpha = false)
    ?(default_fg = Ansi.Color.white) ?(default_bg = Ansi.Color.black) ~rows
    ~cols () =
  let rows = max 1 rows in
  let cols = max 1 cols in
  let primary =
    Grid.create ~width:cols ~height:rows ?width_method ~respect_alpha ()
  in
  let alternate =
    Grid.create ~width:cols ~height:rows ?width_method ~respect_alpha ()
  in
  let scrollback_buffer =
    if scrollback <= 0 then None
    else
      let empty_line = { text = ""; styles = [||] } in
      let lines = Array.make scrollback empty_line in
      Some { lines; head = 0; count = 0; capacity = scrollback }
  in
  Grid.clear ~color:default_bg primary;
  Grid.clear ~color:default_bg alternate;
  {
    primary;
    alternate;
    active_grid = primary;
    rows;
    cols;
    cursor = { row = 0; col = 0; visible = true };
    saved_cursor = None;
    style = Ansi.Style.default;
    saved_style = None;
    parser = Ansi.Parser.create ();
    title = "";
    dirty_state =
      { dirty = false; dirty_rows = Int_set.empty; dirty_cursor = false };
    scrollback = scrollback_buffer;
    scroll_region = { top = 0; bottom = rows - 1 };
    origin_mode = false;
    auto_wrap_mode = true;
    cursor_key_mode = false;
    insert_mode = false;
    bracketed_paste = false;
    default_fg;
    default_bg;
  }

(* Efficient destructive erase using the new Grid API: replace region with
   spaces, with BG matching current style (or default), using an opaque
   background color to avoid alpha blending quirks. *)
let erase_region t ~x ~y ~width ~height =
  if width <= 0 || height <= 0 then ()
  else
    let base_bg =
      match t.style.Ansi.Style.bg with Some c -> c | None -> t.default_bg
    in
    let r, g, b, a = Ansi.Color.to_rgba base_bg in
    let bg_color = Ansi.Color.of_rgba r g b a in
    Grid.fill_rect t.active_grid ~x ~y ~width ~height ~color:bg_color

(* Dirty tracking helpers *)
let mark_dirty t = t.dirty_state.dirty <- true

let mark_row_dirty t row =
  if row >= 0 && row < t.rows then (
    t.dirty_state.dirty <- true;
    t.dirty_state.dirty_rows <- Int_set.add row t.dirty_state.dirty_rows)

let mark_rows_dirty t start_row end_row =
  for row = start_row to end_row do
    mark_row_dirty t row
  done

let mark_cursor_dirty t =
  t.dirty_state.dirty <- true;
  t.dirty_state.dirty_cursor <- true

let grapheme_start_and_width grid row col =
  let width = Grid.width grid in
  if width = 0 then (0, 1)
  else
    let row_start = row * width in
    let col = min (max col 0) (width - 1) in
    let idx = ref (row_start + col) in
    while !idx >= row_start && Grid.is_continuation grid !idx do
      idx := !idx - 1
    done;
    let start_idx = !idx in
    let start_col = start_idx - row_start in
    let w = max 1 (Grid.cell_width grid start_idx) in
    (start_col, w)

let rows t = t.rows
let cols t = t.cols

let resize t ~rows ~cols =
  let rows = max 1 rows in
  let cols = max 1 cols in
  if t.rows = rows && t.cols = cols then ()
  else (
    Grid.resize t.primary ~width:cols ~height:rows;
    Grid.resize t.alternate ~width:cols ~height:rows;
    (* Resizing leaves existing content intact; terminals expect a clear grid
       after a size change so applications repaint. Explicitly clear both
       buffers. *)
    Grid.clear ~color:t.default_bg t.primary;
    Grid.clear ~color:t.default_bg t.alternate;

    (* Scrollback doesn't need resize - it's already compressed text *)
    (* Width changes are handled automatically during decompression *)
    (match t.scrollback with
    | Some _ -> () (* Compressed lines adapt to any width *)
    | None -> ());

    t.rows <- rows;
    t.cols <- cols;

    (* Clamp cursor *)
    if t.cursor.row >= rows then t.cursor.row <- rows - 1;
    if t.cursor.col >= cols then t.cursor.col <- cols - 1;

    (* Adjust scroll region *)
    if t.scroll_region.top >= rows then t.scroll_region.top <- 0;
    t.scroll_region.bottom <- rows - 1;

    mark_rows_dirty t 0 (rows - 1))

let grid t = t.active_grid
let title t = t.title
let is_dirty t = t.dirty_state.dirty

let clear_dirty t =
  t.dirty_state.dirty <- false;
  t.dirty_state.dirty_rows <- Int_set.empty;
  t.dirty_state.dirty_cursor <- false

let dirty_rows t = Int_set.elements t.dirty_state.dirty_rows
let is_cursor_dirty t = t.dirty_state.dirty_cursor
let is_alternate_screen t = t.active_grid == t.alternate
let cursor_pos t = (t.cursor.row, t.cursor.col)
let cursor_visible t = t.cursor.visible

let set_cursor_pos t ~row ~col =
  let row = max 0 (min row (t.rows - 1)) in
  (* Allow cursor at t.cols for autowrap - it wraps on next character *)
  let col = max 0 (min col t.cols) in
  if t.cursor.row <> row || t.cursor.col <> col then (
    t.cursor.row <- row;
    t.cursor.col <- col;
    mark_cursor_dirty t)

let set_cursor_visible t visible =
  if t.cursor.visible <> visible then (
    t.cursor.visible <- visible;
    mark_cursor_dirty t)

let scrollback_capacity t =
  match t.scrollback with None -> 0 | Some sb -> sb.capacity

let scrollback_size t =
  if t.active_grid != t.primary then 0
  else match t.scrollback with None -> 0 | Some sb -> sb.count

let scrollback_lines t =
  if t.active_grid != t.primary then []
  else
    match t.scrollback with
    | None -> []
    | Some sb when sb.count = 0 -> []
    | Some sb ->
        let lines = ref [] in
        let start_idx =
          if sb.count < sb.capacity then 0
          else (sb.head + sb.capacity - sb.count) mod sb.capacity
        in
        for i = 0 to sb.count - 1 do
          let idx = (start_idx + i) mod sb.capacity in
          let line = sb.lines.(idx) in
          lines := line.text :: !lines
        done;
        List.rev !lines

let render_with_scrollback t ~offset dst =
  (* Only works on primary screen with scrollback *)
  if t.active_grid != t.primary then Grid.blit ~src:t.active_grid ~dst
  else
    match t.scrollback with
    | None -> Grid.blit ~src:t.active_grid ~dst
    | Some sb ->
        let offset = max 0 (min offset sb.count) in
        let dst_height = Grid.height dst in

        (* Clear destination grid up-front so we don't need per-row clearing. *)
        Grid.clear ~color:t.default_bg dst;

        (* Calculate how many lines come from scrollback vs screen *)
        let scrollback_lines = min offset dst_height in
        let screen_lines = dst_height - scrollback_lines in

        (* Render scrollback lines by decompressing them *)
        for i = 0 to scrollback_lines - 1 do
          (* Calculate which scrollback line to read from (newest first) *)
          let scrollback_offset = offset - scrollback_lines + i in
          let scrollback_idx =
            if sb.count < sb.capacity then
              (* Not wrapped yet - simple offset from head *)
              sb.count - 1 - scrollback_offset
            else
              (* Wrapped - use modulo arithmetic *)
              (sb.head - 1 - scrollback_offset + sb.capacity) mod sb.capacity
          in
          let line = sb.lines.(scrollback_idx) in
          decompress_line line dst i t.cols
        done;

        (* Render visible screen lines *)
        if screen_lines > 0 then
          Grid.blit_region ~src:t.active_grid ~dst ~src_x:0 ~src_y:0
            ~width:t.cols ~height:(min screen_lines t.rows) ~dst_x:0
            ~dst_y:scrollback_lines

let cursor_key_mode t = t.cursor_key_mode
let insert_mode t = t.insert_mode
let auto_wrap_mode t = t.auto_wrap_mode
let bracketed_paste_mode t = t.bracketed_paste
let origin_mode t = t.origin_mode

(* Save a row to scrollback *)
let push_to_scrollback t row =
  match t.scrollback with
  | None -> ()
  | Some _ when t.active_grid != t.primary -> ()
  | Some sb ->
      (* Compress the row and store it *)
      let compressed = compress_row t.active_grid row t.cols in
      sb.lines.(sb.head) <- compressed;
      sb.head <- (sb.head + 1) mod sb.capacity;
      if sb.count < sb.capacity then sb.count <- sb.count + 1

let scroll_up t n =
  if n <= 0 then ()
  else
    let region_height = t.scroll_region.bottom - t.scroll_region.top + 1 in
    let n = min n region_height in

    (* Save scrolled rows to scrollback if at screen top *)
    if t.active_grid == t.primary && t.scroll_region.top = 0 then
      for i = 0 to n - 1 do
        push_to_scrollback t (t.scroll_region.top + i)
      done;

    (* Use Grid's optimized scroll operation *)
    Grid.scroll t.active_grid ~top:t.scroll_region.top
      ~bottom:t.scroll_region.bottom n;

    mark_rows_dirty t t.scroll_region.top t.scroll_region.bottom

let scroll_down t n =
  if n <= 0 then ()
  else
    let n = min n (t.scroll_region.bottom - t.scroll_region.top + 1) in

    (* Use Grid's optimized scroll operation *)
    Grid.scroll t.active_grid ~top:t.scroll_region.top
      ~bottom:t.scroll_region.bottom (-n);

    mark_rows_dirty t t.scroll_region.top t.scroll_region.bottom

(* Write printable text using Grid and Text APIs. We manually segment the input
   string into pieces that fit in the current line, using an ASCII fast path and
   falling back to grapheme segmentation for complex text. *)
let put_text t text =
  let fg = Option.value t.style.Ansi.Style.fg ~default:t.default_fg in
  let bg = Option.value t.style.Ansi.Style.bg ~default:t.default_bg in
  let attrs = t.style.Ansi.Style.attrs in
  let style =
    Ansi.Style.default |> Ansi.Style.fg fg |> Ansi.Style.bg bg
    |> Ansi.Style.with_attrs attrs
  in
  let width_method = Grid.width_method t.active_grid in
  if t.insert_mode then
    let line_width = t.cols in
    let row = t.cursor.row in
    let copy_cell src_idx dst_x =
      let cell = Grid.get_cell t.active_grid src_idx in
      let attrs = Grid.get_attrs t.active_grid src_idx in
      let link = Grid.get_link t.active_grid src_idx in
      let fg_r = Grid.get_fg_r t.active_grid src_idx in
      let fg_g = Grid.get_fg_g t.active_grid src_idx in
      let fg_b = Grid.get_fg_b t.active_grid src_idx in
      let fg_a = Grid.get_fg_a t.active_grid src_idx in
      let bg_r = Grid.get_bg_r t.active_grid src_idx in
      let bg_g = Grid.get_bg_g t.active_grid src_idx in
      let bg_b = Grid.get_bg_b t.active_grid src_idx in
      let bg_a = Grid.get_bg_a t.active_grid src_idx in
      let fg_color =
        Ansi.Color.of_rgba
          (int_of_float (fg_r *. 255.))
          (int_of_float (fg_g *. 255.))
          (int_of_float (fg_b *. 255.))
          (int_of_float (fg_a *. 255.))
      in
      let bg_color =
        Ansi.Color.of_rgba
          (int_of_float (bg_r *. 255.))
          (int_of_float (bg_g *. 255.))
          (int_of_float (bg_b *. 255.))
          (int_of_float (bg_a *. 255.))
      in
      let link_url = Grid.hyperlink_url t.active_grid link in
      Grid.set_cell t.active_grid ~x:dst_x ~y:row ~cell ~fg:fg_color
        ~bg:bg_color ~attrs:(Ansi.Attr.unpack attrs) ?link:link_url ()
    in

    Text.iter_graphemes
      (fun ~offset:off ~len:l ->
        if t.cursor.col >= line_width && t.auto_wrap_mode then (
          if t.cursor.row >= t.scroll_region.bottom then scroll_up t 1;
          set_cursor_pos t
            ~row:(min (t.cursor.row + 1) t.scroll_region.bottom)
            ~col:0);

        if row < t.rows && t.cursor.col < line_width then
          let w =
            Text.width_at ~width_method ~tab_width:2 text ~byte_offset:off
          in
          let insert_w = min w (line_width - t.cursor.col) in

          if insert_w > 0 then (
            (* Shift existing cells to the right, grapheme-aware, from end to
               start. *)
            let rec shift x =
              if x < t.cursor.col then ()
              else
                let idx = (row * line_width) + x in
                if Grid.is_continuation t.active_grid idx then shift (x - 1)
                else
                  let gw = max 1 (Grid.cell_width t.active_grid idx) in
                  let dest_start = x + insert_w in
                  (if dest_start < line_width then
                     let copy_span = min gw (line_width - dest_start) in
                     for k = copy_span - 1 downto 0 do
                       copy_cell (idx + k) (dest_start + k)
                     done);
                  shift (x - gw)
            in
            shift (line_width - 1);

            (* Clear the newly opened gap and draw the cluster. *)
            erase_region t ~x:t.cursor.col ~y:row ~width:insert_w ~height:1;
            let cluster = String.sub text off l in
            Grid.draw_text t.active_grid ~x:t.cursor.col ~y:row ~text:cluster
              ~style;
            mark_row_dirty t row;
            set_cursor_pos t ~row
              ~col:(min line_width (t.cursor.col + insert_w))))
      text
  else
    let remaining = ref text in
    while String.length !remaining > 0 do
      (* Handle pending wrap *)
      if t.cursor.col >= t.cols && t.auto_wrap_mode then (
        if t.cursor.row >= t.scroll_region.bottom then scroll_up t 1;
        set_cursor_pos t
          ~row:(min (t.cursor.row + 1) t.scroll_region.bottom)
          ~col:0);

      if t.cursor.row >= t.rows then remaining := ""
      else
        let available = t.cols - t.cursor.col in
        if available <= 0 then remaining := ""
        else
          let s = !remaining in
          let len = String.length s in

          (* Fast path: if the whole string fits in remaining columns, draw
             once. *)
          let total_w = Text.measure ~width_method ~tab_width:2 s in
          if total_w <= available then (
            Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row ~text:s
              ~style;
            mark_row_dirty t t.cursor.row;
            set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col + total_w);
            remaining := "")
          else
            let fit =
              Text.find_wrap_pos ~width_method ~tab_width:2 s
                ~max_columns:available
            in
            if fit.byte_offset = 0 && len > 0 then (
              (* First grapheme doesn't fit in remaining width. Defer to
                 Grid.draw_text to handle truncation/clearing semantics for this
                 line, then stop. *)
              Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row
                ~text:s ~style;
              mark_row_dirty t t.cursor.row;
              remaining := "")
            else
              let segment = String.sub s 0 fit.byte_offset in
              Grid.draw_text t.active_grid ~x:t.cursor.col ~y:t.cursor.row
                ~text:segment ~style;
              mark_row_dirty t t.cursor.row;
              set_cursor_pos t ~row:t.cursor.row
                ~col:(t.cursor.col + fit.columns_used);
              remaining := String.sub s fit.byte_offset (len - fit.byte_offset)
    done

(* Handle control characters *)
let handle_control_char t code =
  match code with
  | 0x07 -> () (* BEL *)
  | 0x08 ->
      (* BS: move left and clear the grapheme at the new cursor position. *)
      let row = t.cursor.row in
      let target_col = t.cursor.col - 1 in
      if row >= 0 && row < t.rows && target_col >= 0 then (
        let start_col, w =
          grapheme_start_and_width t.active_grid row target_col
        in
        erase_region t ~x:start_col ~y:row ~width:w ~height:1;
        set_cursor_pos t ~row ~col:start_col;
        mark_row_dirty t row)
  | 0x09 ->
      (* HT *)
      let next_tab = ((t.cursor.col / 8) + 1) * 8 in
      set_cursor_pos t ~row:t.cursor.row ~col:(min next_tab (t.cols - 1))
  | 0x0A | 0x0B | 0x0C ->
      (* LF, VT, FF *)
      if t.cursor.row >= t.scroll_region.bottom then scroll_up t 1
      else set_cursor_pos t ~row:(t.cursor.row + 1) ~col:t.cursor.col;
      set_cursor_pos t ~row:t.cursor.row ~col:0
  | 0x0D -> (* CR *) set_cursor_pos t ~row:t.cursor.row ~col:0
  | 0x0E | 0x0F -> () (* SO, SI *)
  | _ -> ()

let handle_text t text =
  let len = String.length text in
  let text_start = ref 0 in

  let flush_text i =
    if i > !text_start then
      put_text t (String.sub text !text_start (i - !text_start));
    text_start := i + 1
  in

  for i = 0 to len - 1 do
    let code = Char.code text.[i] in
    if code < 0x20 || code = 0x7F then (
      flush_text i;
      handle_control_char t code)
  done;
  flush_text len

let apply_sgr_attr style attr =
  match attr with
  | `Reset -> Ansi.Style.default
  | `Bold -> Ansi.Style.with_bold true style
  | `Dim -> Ansi.Style.with_dim true style
  | `Italic -> Ansi.Style.with_italic true style
  | `Underline -> Ansi.Style.with_underline true style
  | `Double_underline -> Ansi.Style.with_double_underline true style
  | `Blink -> Ansi.Style.with_blink true style
  | `Inverse -> Ansi.Style.with_inverse true style
  | `Hidden -> Ansi.Style.with_hidden true style
  | `Strikethrough -> Ansi.Style.with_strikethrough true style
  | `Overline -> Ansi.Style.with_overline true style
  | `Framed -> Ansi.Style.with_framed true style
  | `Encircled -> Ansi.Style.with_encircled true style
  | `No_bold -> Ansi.Style.with_bold false style
  | `No_dim -> Ansi.Style.with_dim false style
  | `No_italic -> Ansi.Style.with_italic false style
  | `No_underline -> Ansi.Style.with_underline false style
  | `No_blink -> Ansi.Style.with_blink false style
  | `No_inverse -> Ansi.Style.with_inverse false style
  | `No_hidden -> Ansi.Style.with_hidden false style
  | `No_strikethrough -> Ansi.Style.with_strikethrough false style
  | `No_overline -> Ansi.Style.with_overline false style
  | `No_framed -> Ansi.Style.with_framed false style
  | `No_encircled -> Ansi.Style.with_encircled false style
  | `Fg color -> Ansi.Style.fg color style
  | `Bg color -> Ansi.Style.bg color style

let _switch_to_alternate t save_cursor =
  if t.active_grid != t.alternate then (
    if save_cursor then (
      t.saved_cursor <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
      t.saved_style <- Some t.style);
    t.active_grid <- t.alternate;
    Grid.clear ~color:t.default_bg t.active_grid;
    set_cursor_pos t ~row:0 ~col:0;
    t.scroll_region.top <- 0;
    t.scroll_region.bottom <- t.rows - 1;
    mark_rows_dirty t 0 (t.rows - 1))

let switch_to_primary t restore_cursor =
  if t.active_grid == t.alternate then (
    t.active_grid <- t.primary;
    (if restore_cursor then
       match t.saved_cursor with
       | Some (row, col, visible) ->
           t.cursor.row <- row;
           t.cursor.col <- col;
           t.cursor.visible <- visible;
           mark_cursor_dirty t
       | None -> (
           match t.saved_style with
           | Some style -> t.style <- style
           | None -> ()));
    mark_rows_dirty t 0 (t.rows - 1))

let handle_control t ctrl =
  match ctrl with
  | Ansi.Parser.CUU n ->
      set_cursor_pos t ~row:(t.cursor.row - n) ~col:t.cursor.col
  | Ansi.Parser.CUD n ->
      set_cursor_pos t ~row:(t.cursor.row + n) ~col:t.cursor.col
  | Ansi.Parser.CUF n ->
      set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col + n)
  | Ansi.Parser.CUB n ->
      set_cursor_pos t ~row:t.cursor.row ~col:(t.cursor.col - n)
  | Ansi.Parser.CNL n -> set_cursor_pos t ~row:(t.cursor.row + n) ~col:0
  | Ansi.Parser.CPL n -> set_cursor_pos t ~row:(t.cursor.row - n) ~col:0
  | Ansi.Parser.CHA n -> set_cursor_pos t ~row:t.cursor.row ~col:(n - 1)
  | Ansi.Parser.VPA n -> set_cursor_pos t ~row:(n - 1) ~col:t.cursor.col
  | Ansi.Parser.CUP (r, c) -> set_cursor_pos t ~row:(r - 1) ~col:(c - 1)
  | Ansi.Parser.ED n -> (
      match n with
      | 0 ->
          (* Clear from cursor to end (destructive erase) on default/theme bg *)
          erase_region t ~x:t.cursor.col ~y:t.cursor.row
            ~width:(t.cols - t.cursor.col) ~height:1;
          erase_region t ~x:0 ~y:(t.cursor.row + 1) ~width:t.cols
            ~height:(t.rows - (t.cursor.row + 1));
          mark_rows_dirty t t.cursor.row (t.rows - 1)
      | 1 ->
          (* Clear from start to cursor (destructive erase) *)
          erase_region t ~x:0 ~y:0 ~width:t.cols ~height:t.cursor.row;
          erase_region t ~x:0 ~y:t.cursor.row ~width:(t.cursor.col + 1)
            ~height:1;
          mark_rows_dirty t 0 t.cursor.row
      | 2 | 3 ->
          (* Clear entire screen *)
          erase_region t ~x:0 ~y:0 ~width:t.cols ~height:t.rows;
          mark_rows_dirty t 0 (t.rows - 1)
      | _ -> ())
  | Ansi.Parser.EL n ->
      (match n with
      | 0 ->
          erase_region t ~x:t.cursor.col ~y:t.cursor.row
            ~width:(t.cols - t.cursor.col) ~height:1
      | 1 ->
          erase_region t ~x:0 ~y:t.cursor.row ~width:(t.cursor.col + 1)
            ~height:1
      | 2 -> erase_region t ~x:0 ~y:t.cursor.row ~width:t.cols ~height:1
      | _ -> ());
      mark_row_dirty t t.cursor.row
  | Ansi.Parser.IL n ->
      (* Insert lines *)
      if n > 0 && t.cursor.row <= t.scroll_region.bottom then (
        let insert_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
        for row = t.scroll_region.bottom downto t.cursor.row + insert_count do
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:0
            ~src_y:(row - insert_count) ~width:t.cols ~height:1 ~dst_x:0
            ~dst_y:row
        done;
        erase_region t ~x:0 ~y:t.cursor.row ~width:t.cols ~height:insert_count;
        mark_rows_dirty t t.cursor.row t.scroll_region.bottom)
  | Ansi.Parser.DL n ->
      (* Delete lines *)
      if n > 0 && t.cursor.row <= t.scroll_region.bottom then (
        let delete_count = min n (t.scroll_region.bottom - t.cursor.row + 1) in
        for row = t.cursor.row to t.scroll_region.bottom - delete_count do
          Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x:0
            ~src_y:(row + delete_count) ~width:t.cols ~height:1 ~dst_x:0
            ~dst_y:row
        done;
        erase_region t ~x:0
          ~y:(t.scroll_region.bottom - delete_count + 1)
          ~width:t.cols ~height:delete_count;
        mark_rows_dirty t t.cursor.row t.scroll_region.bottom)
  | Ansi.Parser.DCH n ->
      (* Delete characters *)
      if n > 0 then
        let row = t.cursor.row in
        if row >= 0 && row < t.rows then
          let start_col, _ =
            grapheme_start_and_width t.active_grid row t.cursor.col
          in
          if start_col < t.cols then
            let delete_span =
              let rec loop col remaining acc =
                if col >= t.cols || remaining <= 0 then acc
                else
                  let start, w =
                    grapheme_start_and_width t.active_grid row col
                  in
                  let next_col = start + w in
                  loop next_col (remaining - w) (acc + w)
              in
              let span = loop start_col n 0 in
              min span (t.cols - start_col)
            in
            if delete_span > 0 then (
              let src_x = start_col + delete_span in
              let move_width = t.cols - src_x in
              if move_width > 0 then
                Grid.blit_region ~src:t.active_grid ~dst:t.active_grid ~src_x
                  ~src_y:row ~width:move_width ~height:1 ~dst_x:start_col
                  ~dst_y:row;
              erase_region t ~x:(t.cols - delete_span) ~y:row ~width:delete_span
                ~height:1;
              mark_row_dirty t row)
  | Ansi.Parser.ICH n ->
      (* Insert characters: shift content right by insert_span, clear the gap.
         Mirror of DCH which shifts left. Grid.blit_region handles overlapping
         same-grid blits by iterating right-to-left when src_x < dst_x. *)
      if n > 0 then
        let row = t.cursor.row in
        if row >= 0 && row < t.rows then
          let start_col, _ =
            grapheme_start_and_width t.active_grid row t.cursor.col
          in
          if start_col < t.cols then
            let insert_span = min n (t.cols - start_col) in
            if insert_span > 0 then (
              let move_width = t.cols - start_col - insert_span in
              if move_width > 0 then (
                Grid.blit_region ~src:t.active_grid ~dst:t.active_grid
                  ~src_x:start_col ~src_y:row ~width:move_width ~height:1
                  ~dst_x:(start_col + insert_span) ~dst_y:row;
                (* If a wide character at the right edge got truncated by the
                   shift, erase the orphaned cells. *)
                let right_start, right_w =
                  grapheme_start_and_width t.active_grid row (t.cols - 1)
                in
                if right_start + right_w > t.cols then
                  erase_region t ~x:right_start ~y:row
                    ~width:(t.cols - right_start) ~height:1);
              erase_region t ~x:start_col ~y:row ~width:insert_span ~height:1;
              mark_row_dirty t row)
  | Ansi.Parser.OSC (0, title) | Ansi.Parser.OSC (2, title) -> t.title <- title
  | Ansi.Parser.OSC _ -> () (* Ignore other OSC *)
  | Ansi.Parser.DCS _ | Ansi.Parser.APC _ | Ansi.Parser.PM _ | Ansi.Parser.SOS _
    ->
      ()
  | Ansi.Parser.Hyperlink payload -> (
      match payload with
      | None -> t.style <- Ansi.Style.unlink t.style
      | Some (_, url) -> t.style <- Ansi.Style.hyperlink url t.style)
  | Ansi.Parser.DECSC ->
      t.saved_cursor <- Some (t.cursor.row, t.cursor.col, t.cursor.visible);
      t.saved_style <- Some t.style
  | Ansi.Parser.DECRC -> (
      match t.saved_cursor with
      | Some (row, col, visible) ->
          set_cursor_pos t ~row ~col;
          set_cursor_visible t visible
      | None -> (
          match t.saved_style with Some style -> t.style <- style | None -> ()))
  | Ansi.Parser.Reset ->
      t.style <- Ansi.Style.default;
      Grid.clear ~color:t.default_bg t.active_grid;
      set_cursor_pos t ~row:0 ~col:0;
      mark_rows_dirty t 0 (t.rows - 1)
  | Ansi.Parser.Unknown seq -> (
      if
        (* Parse mode set/reset sequences - parser includes "CSI[" prefix *)
        String.length seq > 5 && String.sub seq 0 4 = "CSI["
      then
        let params = String.sub seq 4 (String.length seq - 4) in
        if String.length params >= 3 && params.[0] = '?' then
          (* DECSET/DECRST: CSI ? Pm h/l *)
          let mode_num = String.sub params 1 (String.length params - 2) in
          let set = params.[String.length params - 1] = 'h' in
          match mode_num with
          | "1" -> t.cursor_key_mode <- set
          | "7" -> t.auto_wrap_mode <- set
          | "25" -> set_cursor_visible t set
          | "47" | "1047" | "1049" ->
              if set then _switch_to_alternate t (mode_num = "1049")
              else switch_to_primary t (mode_num = "1049")
          | "2004" -> t.bracketed_paste <- set
          | _ -> ()
        else if String.length params >= 2 then
          (* SM/RM: CSI Pm h/l *)
          let mode_num = String.sub params 0 (String.length params - 1) in
          let set = params.[String.length params - 1] = 'h' in
          match mode_num with "4" -> t.insert_mode <- set | _ -> ())

let handle_token t token =
  match token with
  | Ansi.Parser.Text s -> handle_text t s
  | Ansi.Parser.SGR attrs ->
      let new_style = List.fold_left apply_sgr_attr t.style attrs in
      if new_style <> t.style then (
        t.style <- new_style;
        mark_dirty t)
  | Ansi.Parser.Control ctrl -> handle_control t ctrl

let feed t bytes ofs len =
  Ansi.Parser.feed t.parser bytes ~off:ofs ~len (handle_token t)

let feed_string t str =
  let bytes = Bytes.unsafe_of_string str in
  feed t bytes 0 (String.length str)

let reset t =
  if t.active_grid == t.alternate then switch_to_primary t false;
  Grid.clear ~color:t.default_bg t.primary;
  Grid.clear ~color:t.default_bg t.alternate;
  set_cursor_pos t ~row:0 ~col:0;
  t.style <- Ansi.Style.default;
  t.saved_cursor <- None;
  t.saved_style <- None;
  Ansi.Parser.reset t.parser;
  t.title <- "";

  (* Clear scrollback *)
  (match t.scrollback with
  | Some sb ->
      sb.head <- 0;
      sb.count <- 0
  | None -> ());

  t.scroll_region.top <- 0;
  t.scroll_region.bottom <- t.rows - 1;
  t.origin_mode <- false;
  t.auto_wrap_mode <- true;
  t.cursor_key_mode <- false;
  t.insert_mode <- false;
  t.bracketed_paste <- false;
  mark_rows_dirty t 0 (t.rows - 1)

let to_string t =
  let grid = t.active_grid in
  let rows = t.rows in
  let cols = t.cols in
  let lines = ref [] in

  for row = 0 to rows - 1 do
    let buf = Buffer.create cols in
    let col = ref 0 in
    while !col < cols do
      let idx = (row * cols) + !col in
      if Grid.is_empty grid idx then (
        Buffer.add_char buf ' ';
        incr col)
      else if Grid.is_continuation grid idx then
        (* Part of a wide grapheme already emitted. *)
        incr col
      else
        let s = Grid.get_text grid idx in
        if s = "" then (
          Buffer.add_char buf ' ';
          incr col)
        else (
          Buffer.add_string buf s;
          let w = Grid.cell_width grid idx in
          let adv = if w <= 0 then 1 else w in
          col := !col + adv)
    done;
    let line = Buffer.contents buf in
    lines := line :: !lines
  done;
  String.concat "\n" (List.rev !lines)
