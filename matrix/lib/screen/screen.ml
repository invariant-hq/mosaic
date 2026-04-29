(* matrix/lib/screen.ml *)

(* Allocation-conscious terminal screen with direct buffer mutation. *)

open StdLabels
module Hit_grid = Hit_grid
module Glyph_pool = Glyph.Pool

(* --- Types & Metrics --- *)

type stats_state = {
  mutable frame_count : int;
  mutable total_cells : int;
  mutable total_bytes : int;
}

type stats = { frame_count : int; total_cells : int; total_bytes : int }

type frame_metrics = {
  frame_count : int;
  cells : int;
  bytes : int;
  frame_time_ms : float;
  interval_ms : float;
  reset_ms : float;
  cursor_visible : bool;
  timestamp_s : float;
}

type cursor_style = [ `Block | `Line | `Underline ]

type cursor = {
  position : (int * int) option;
  style : cursor_style;
  blinking : bool;
  color : (int * int * int) option;
  visible : bool;
}

(* screen state - mutable internal state for maximum performance *)
type t = {
  (* Configuration *)
  glyph_pool : Glyph_pool.t;
  stats : stats_state;
  mutable last_metrics : frame_metrics;
  (* Buffers (Double Buffering) *)
  mutable current : Grid.t;
  mutable next : Grid.t;
  mutable hit_current : Hit_grid.t;
  mutable hit_next : Hit_grid.t;
  (* State *)
  cursor : Cursor_state.t;
  sgr_state : Ansi.Sgr_state.t;
  mutable row_offset : int;
  mutable scratch_bytes : bytes;
  mutable last_render_time : float option;
  (* Capabilities *)
  mutable prefer_explicit_width : bool;
  mutable explicit_width_capable : bool;
  mutable use_explicit_width : bool;
  mutable use_explicit_cursor_positioning : bool;
  mutable hyperlinks_capable : bool;
  (* Post-processing *)
  mutable post_process_fns : (int * (Grid.t -> delta:float -> unit)) list;
  mutable post_process_cache : (Grid.t -> delta:float -> unit) list;
  mutable post_process_dirty : bool;
  mutable next_effect_id : int;
}

(* --- Constants & Inline Helpers --- *)

let[@inline] width_step w = if w <= 0 then 1 else w
let default_render_buffer_size = 2 * 1024 * 1024

(* --- Writer Logic --- *)

(* Writes a grid cell's content to the output buffer. For wide graphemes, uses
   explicit_width (OSC 66) when available, otherwise falls back to cursor
   repositioning to prevent column drift in terminals that miscalculate grapheme
   widths. *)
let[@inline] add_code_to_writer ~explicit_width ~explicit_cursor_positioning
    ~row_offset ~y ~x ~grid_width ~cell_width pool (scratch : bytes ref)
    (w : Ansi.writer) grid idx =
  let glyph = Grid.get_glyph grid idx in

  if Glyph.is_empty glyph || Grid.is_continuation grid idx then
    Ansi.emit (Ansi.char ' ') w
  else
    let len = Glyph_pool.length pool glyph in
    if len <= 0 then Ansi.emit (Ansi.char ' ') w
    else if len = 1 && (glyph :> int) < 128 then
      Ansi.emit (Ansi.char (Char.chr (glyph :> int))) w
    else (
      if len > Bytes.length !scratch then
        scratch := Bytes.create (max (Bytes.length !scratch * 2) len);

      let written = Glyph_pool.blit pool glyph !scratch ~pos:0 in

      if written <= 0 then Ansi.emit (Ansi.char ' ') w
      else if explicit_width && cell_width >= 2 then
        Ansi.emit
          (Ansi.explicit_width_bytes ~width:cell_width ~bytes:!scratch ~off:0
             ~len:written)
          w
      else (
        Ansi.emit (Ansi.bytes !scratch ~off:0 ~len:written) w;
        (* Fallback: reposition cursor after wide graphemes to prevent drift in
           terminals that support cursor addressing but not OSC 66. *)
        if explicit_cursor_positioning && cell_width >= 2 then
          let next_x = x + cell_width in
          if next_x < grid_width then
            Ansi.cursor_position ~row:(row_offset + y + 1) ~col:(next_x + 1) w))

(* --- Core Rendering Logic --- *)

type render_mode = [ `Diff | `Full ]

(* The hot loop. Scans grid, checks dirty flags, diffs against previous frame,
   and emits sequences while reusing renderer state and scratch buffers. *)
let render_generic ~pool ~row_offset ~use_explicit_width
    ~use_explicit_cursor_positioning ~use_hyperlinks ~mode ~height_limit ~writer
    ~scratch ~sgr_state ~prev ~curr =
  let width = Grid.width curr in
  let curr_height = Grid.height curr in
  let height =
    match height_limit with
    | None -> curr_height
    | Some limit -> max 0 (min curr_height limit)
  in
  let row_offset = max 0 row_offset in

  (* Extract prev grid dimensions once - no tuple allocation *)
  let prev_width = match prev with None -> 0 | Some p -> Grid.width p in
  let prev_height_raw = match prev with None -> 0 | Some p -> Grid.height p in
  let prev_height =
    match height_limit with
    | None -> prev_height_raw
    | Some _ -> min prev_height_raw height
  in

  (* Inline cell change detection - no closure allocation *)
  let[@inline] is_cell_changed y x idx curr_width =
    if curr_width <= 0 then false
    else
      match mode with
      | `Full -> true
      | `Diff -> (
          if y >= prev_height || x >= prev_width then true
          else
            match prev with
            | None -> true
            | Some p ->
                let prev_idx = (y * prev_width) + x in
                not (Grid.cells_equal p prev_idx curr idx))
  in

  (* SGR State Tracking - use pre-allocated state *)
  Ansi.Sgr_state.reset sgr_state;

  (* Inner loop: Write consecutive changed cells, return new x position. Cells
     written = new_x - start_x (no tuple allocation needed). *)
  let rec write_run y x =
    if x >= width then x
    else
      let idx = (y * width) + x in
      let curr_width = Grid.cell_width curr idx in
      let step = width_step curr_width in

      if curr_width <= 0 then x
      else if not (is_cell_changed y x idx curr_width) then x
      else
        (* Emit Style/Color using zero-alloc accessors *)
        let attrs = Grid.get_attrs curr idx in
        let link =
          if use_hyperlinks then
            Grid.hyperlink_url_direct curr (Grid.get_link curr idx)
          else ""
        in

        (* Individual component access to avoid tuple allocation *)
        let fg_r = Grid.get_fg_r curr idx in
        let fg_g = Grid.get_fg_g curr idx in
        let fg_b = Grid.get_fg_b curr idx in
        let fg_a = Grid.get_fg_a curr idx in

        let bg_r = Grid.get_bg_r curr idx in
        let bg_g = Grid.get_bg_g curr idx in
        let bg_b = Grid.get_bg_b curr idx in
        let bg_a = Grid.get_bg_a curr idx in

        Ansi.Sgr_state.update sgr_state writer ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r
          ~bg_g ~bg_b ~bg_a ~attrs ~link;

        (* Emit Content *)
        add_code_to_writer ~explicit_width:use_explicit_width
          ~explicit_cursor_positioning:use_explicit_cursor_positioning
          ~row_offset ~y ~x ~grid_width:width ~cell_width:curr_width pool
          scratch writer curr idx;

        write_run y (x + step)
  in

  (* Process columns in a row, return total cells updated in this row *)
  let rec process_cols y x row_cells =
    if x >= width then row_cells
    else
      let idx = (y * width) + x in
      let curr_width = Grid.cell_width curr idx in

      if curr_width <= 0 then (
        (* Continuation cell: defensively clear if it differs from the previous
           frame. When a wide character is replaced by narrow ones, the stale
           continuation marker would otherwise leave the old right-half visible
           on terminals that don't implicitly clear it. *)
        (if mode = `Diff && y < prev_height && x < prev_width then
           match prev with
           | None -> ()
           | Some p ->
               let prev_idx = (y * prev_width) + x in
               if not (Grid.cells_equal p prev_idx curr idx) then (
                 Ansi.cursor_position
                   ~row:(row_offset + y + 1)
                   ~col:(x + 1) writer;
                 Ansi.emit (Ansi.char ' ') writer));
        process_cols y (x + 1) row_cells)
      else if is_cell_changed y x idx curr_width then (
        (* Move cursor to start of changed run *)
        let target_row = row_offset + y + 1 in
        Ansi.cursor_position ~row:target_row ~col:(x + 1) writer;

        (* Write consecutive changed cells *)
        let start_x = x in
        let new_x = write_run y x in
        let cells_in_run = new_x - start_x in

        (* Close active hyperlink. SGR state is preserved across the gap
           so the next run on this row skips re-emission when unchanged. *)
        Ansi.Sgr_state.close_link sgr_state writer;

        process_cols y new_x (row_cells + cells_in_run))
      else process_cols y (x + width_step curr_width) row_cells
  in

  (* Process all rows, accumulate total cells *)
  let rec process_rows y total_cells =
    if y >= height then total_cells
    else
      let row_cells = process_cols y 0 0 in
      process_rows (y + 1) (total_cells + row_cells)
  in

  let total = process_rows 0 0 in

  (* Clear any cells that were present in the previous frame but are now outside
     the current grid bounds. This prevents stale rows/columns from lingering
     when the grid shrinks. *)
  (if prev_width > width then
     let start_col = width + 1 in
     let rows = min height prev_height in
     for y = 0 to rows - 1 do
       Ansi.cursor_position ~row:(row_offset + y + 1) ~col:start_col writer;
       Ansi.erase_line ~mode:`Right writer
     done);

  if prev_height > height then
    for y = height to prev_height - 1 do
      Ansi.cursor_position ~row:(row_offset + y + 1) ~col:1 writer;
      Ansi.erase_line ~mode:`All writer
    done;

  Ansi.Sgr_state.close_link sgr_state writer;
  if total > 0 then Ansi.emit Ansi.reset writer;
  Ansi.Sgr_state.reset sgr_state;
  total

(* --- Frame Lifecycle --- *)

let[@inline] swap_buffers r =
  let old_current = r.current in
  r.current <- r.next;
  r.next <- old_current;
  let old_hit_current = r.hit_current in
  r.hit_current <- r.hit_next;
  r.hit_next <- old_hit_current;
  Hit_grid.clear r.hit_next;
  Grid.clear r.next

let clear_unpresented_rows r presented_height =
  let height = Grid.height r.next in
  if presented_height < height then (
    let y = max 0 presented_height in
    let h = height - y in
    Grid.clear_rect r.next ~x:0 ~y ~width:(Grid.width r.next) ~height:h;
    Hit_grid.add r.hit_next ~x:0 ~y ~width:(Grid.width r.next) ~height:h
      ~id:Hit_grid.empty_id)

let post_processes r =
  if r.post_process_dirty then (
    r.post_process_cache <- List.rev_map ~f:snd r.post_process_fns;
    r.post_process_dirty <- false);
  r.post_process_cache

let prepare_frame r =
  let now = Unix.gettimeofday () in
  let delta_seconds =
    match r.last_render_time with
    | None ->
        r.last_render_time <- Some now;
        0.
    | Some prev ->
        let delta = now -. prev in
        r.last_render_time <- Some now;
        if delta <= 0. then 0. else delta
  in
  let delta_ms = delta_seconds *. 1000. in
  List.iter ~f:(fun fn -> fn r.next ~delta:delta_ms) (post_processes r);
  (now, delta_seconds)

let finalize_frame r ~now ~delta_seconds ~elapsed_ms ~cells ~output_len =
  let t_reset_start = Unix.gettimeofday () in

  (* Swap buffers; [next] is cleared to provide a fresh canvas for the
     builder. *)
  swap_buffers r;

  let t_reset_end = Unix.gettimeofday () in
  let reset_ms = (t_reset_end -. t_reset_start) *. 1000. in

  (* Update Stats *)
  r.stats.frame_count <- r.stats.frame_count + 1;
  r.stats.total_cells <- r.stats.total_cells + cells;
  r.stats.total_bytes <- r.stats.total_bytes + output_len;

  (* Snapshot Metrics *)
  let next_m =
    {
      frame_count = r.stats.frame_count;
      cells;
      bytes = output_len;
      frame_time_ms = elapsed_ms;
      interval_ms = delta_seconds *. 1000.;
      reset_ms;
      cursor_visible = Cursor_state.is_visible r.cursor;
      timestamp_s = now;
    }
  in
  r.last_metrics <- next_m

(* --- Input / Cursor Handling --- *)

(* --- Public API --- *)

type scroll_hint = { top : int; bottom : int; delta : int }

let normalize_scroll_hint ~row_offset ~height_limit ~current hint =
  let { top; bottom; delta } = hint in
  let current_height = Grid.height current in
  let render_height =
    match height_limit with
    | None -> current_height
    | Some limit -> max 0 (min current_height limit)
  in
  let top = max 0 top in
  let bottom = min (render_height - 1) bottom in
  if delta = 0 || top >= bottom then None
  else
    let region_h = bottom - top + 1 in
    let delta =
      if delta > 0 then min delta region_h else max delta (-region_h)
    in
    Some (top, bottom, delta, max 0 row_offset)

let apply_scroll_hint ~(writer : Ansi.writer) ~row_offset ~height_limit ~current
    hint =
  match normalize_scroll_hint ~row_offset ~height_limit ~current hint with
  | None -> ()
  | Some (top, bottom, delta, row_offset) ->
      (* Shift the previous buffer to match the hardware scroll. After this,
         the diff loop sees only the newly-revealed edge rows as changes. *)
      Grid.scroll current ~top ~bottom delta;
      (* Tell the terminal to perform the same shift via DECSTBM. *)
      Ansi.emit
        (Ansi.set_scrolling_region
           ~top:(row_offset + top + 1)
           ~bottom:(row_offset + bottom + 1))
        writer;
      if delta > 0 then Ansi.emit (Ansi.scroll_up ~n:delta) writer
      else Ansi.emit (Ansi.scroll_down ~n:(-delta)) writer;
      Ansi.emit Ansi.reset_scrolling_region writer;
      Ansi.cursor_position ~row:(row_offset + 1) ~col:1 writer

let submit ~(mode : render_mode) ?scroll_hint ?height_limit
    ~(writer : Ansi.writer) r =
  let now, delta_seconds = prepare_frame r in
  (* Use Fun.protect to guarantee SGR cleanup even if render raises. *)
  let cells = ref 0 in
  let elapsed_ms = ref 0. in
  Fun.protect
    ~finally:(fun () ->
      Ansi.Sgr_state.close_link r.sgr_state writer;
      Ansi.Sgr_state.reset r.sgr_state)
    (fun () ->
      let scratch = ref r.scratch_bytes in
      let render_start = Unix.gettimeofday () in
      (* Apply DECSTBM scroll hint before diffing. The hint shifts the
         previous buffer and emits hardware scroll sequences so the diff
         loop only sees the newly-revealed rows as changes. *)
      (match (mode, scroll_hint) with
      | `Diff, Some hint ->
          apply_scroll_hint ~writer ~row_offset:r.row_offset ~height_limit
            ~current:r.current hint
      | _ -> ());
      let prev = match mode with `Diff -> Some r.current | `Full -> None in

      cells :=
        render_generic ~pool:r.glyph_pool ~row_offset:r.row_offset
          ~use_explicit_width:r.use_explicit_width
          ~use_explicit_cursor_positioning:r.use_explicit_cursor_positioning
          ~use_hyperlinks:r.hyperlinks_capable ~mode ~height_limit ~writer
          ~scratch ~sgr_state:r.sgr_state ~prev ~curr:r.next;

      elapsed_ms := (Unix.gettimeofday () -. render_start) *. 1000.;
      r.scratch_bytes <- !scratch);

  let output_len = Ansi.Writer.len writer in
  let presented_height =
    match height_limit with
    | None -> Grid.height r.next
    | Some limit -> max 0 (min (Grid.height r.next) limit)
  in
  clear_unpresented_rows r presented_height;
  finalize_frame r ~now ~delta_seconds ~elapsed_ms:!elapsed_ms ~cells:!cells
    ~output_len

let render_to_bytes ?(full = false) ?scroll_hint ?height_limit frame bytes =
  let writer = Ansi.Writer.make bytes in
  let mode = if full then `Full else `Diff in
  submit frame ~mode ?scroll_hint ?height_limit ~writer;
  Ansi.Writer.len writer

let render ?(full = false) ?scroll_hint ?height_limit frame =
  let bytes = Bytes.create default_render_buffer_size in
  let len = render_to_bytes ~full ?scroll_hint ?height_limit frame bytes in
  Bytes.sub_string bytes ~pos:0 ~len

let glyph_pool t = t.glyph_pool

(* Creation & Management *)

let create ?glyph_pool ?width_method ?respect_alpha ?(cursor_visible = true)
    ?(explicit_width = false) () =
  let glyph_pool =
    match glyph_pool with Some p -> p | None -> Glyph_pool.create ()
  in
  let w_method = match width_method with Some m -> m | None -> `Unicode in
  let r_alpha = match respect_alpha with Some r -> r | None -> false in

  let t =
    {
      glyph_pool;
      stats = { frame_count = 0; total_cells = 0; total_bytes = 0 };
      last_metrics =
        {
          frame_count = 0;
          cells = 0;
          bytes = 0;
          frame_time_ms = 0.;
          interval_ms = 0.;
          reset_ms = 0.;
          cursor_visible;
          timestamp_s = 0.;
        };
      current =
        Grid.create ~width:1 ~height:1 ~glyph_pool ~width_method:w_method
          ~respect_alpha:r_alpha ();
      next =
        Grid.create ~width:1 ~height:1 ~glyph_pool ~width_method:w_method
          ~respect_alpha:r_alpha ();
      hit_current = Hit_grid.create ~width:0 ~height:0;
      hit_next = Hit_grid.create ~width:0 ~height:0;
      cursor = Cursor_state.create ();
      sgr_state = Ansi.Sgr_state.create ();
      row_offset = 0;
      post_process_fns = [];
      post_process_cache = [];
      post_process_dirty = false;
      next_effect_id = 0;
      prefer_explicit_width = explicit_width;
      explicit_width_capable = true;
      use_explicit_width = explicit_width;
      use_explicit_cursor_positioning = false;
      hyperlinks_capable = true;
      scratch_bytes = Bytes.create 1024;
      (* Large enough for any grapheme *)
      last_render_time = None;
    }
  in
  Cursor_state.set_visible t.cursor cursor_visible;
  t

let reset t =
  Grid.clear t.next;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  t.last_render_time <- None;
  t.stats.frame_count <- 0;
  t.stats.total_cells <- 0;
  t.stats.total_bytes <- 0;
  Cursor_state.reset t.cursor;
  Ansi.Sgr_state.reset t.sgr_state

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Screen.resize: width and height must be > 0";
  if width <> Grid.width t.current || height <> Grid.height t.current then (
    Grid.resize t.current ~width ~height;
    Grid.clear t.current;
    Grid.resize t.next ~width ~height;
    (* Hit_grid.resize already clears unconditionally, no need to clear again *)
    Hit_grid.resize t.hit_current ~width ~height;
    Hit_grid.resize t.hit_next ~width ~height;
    Cursor_state.clamp_to_bounds t.cursor ~max_row:height ~max_col:width)

let internal_build t ~width ~height f =
  if width <= 0 || height <= 0 then (
    Hit_grid.clear t.hit_next;
    t)
  else (
    if width <> Grid.width t.next || height <> Grid.height t.next then
      resize t ~width ~height;
    Hit_grid.clear t.hit_next;
    f t.next t.hit_next;
    t)

let build t ~width ~height f =
  ignore (internal_build t ~width ~height (fun grid hits -> f grid hits) : t)

let next_grid frame = frame.next
let next_hit_grid frame = frame.hit_next
let query_hit frame ~x ~y = Hit_grid.get frame.hit_current ~x ~y
let row_offset t = t.row_offset
let set_row_offset t offset = t.row_offset <- max 0 offset

let invalidate_presented t =
  (* Clear the current buffer so diff sees all cells as changed. This maintains
     the invariant: current = what's on terminal. After erasing the terminal
     region, the terminal is "blank", so current should also be blank. *)
  Grid.clear t.current

let active_height (t : t) = Grid.active_height t.next

let stats t =
  {
    frame_count = t.stats.frame_count;
    total_cells = t.stats.total_cells;
    total_bytes = t.stats.total_bytes;
  }

let last_metrics t = t.last_metrics
let clamp_byte v = max 0 (min 255 v)

let set_cursor t cursor =
  (match cursor.position with
  | None -> Cursor_state.clear_position t.cursor
  | Some (x, y) ->
      Cursor_state.set_position t.cursor ~row:(max 0 y + 1) ~col:(max 0 x + 1));
  Cursor_state.set_visible t.cursor cursor.visible;
  Cursor_state.set_style t.cursor ~style:cursor.style ~blinking:cursor.blinking;
  Cursor_state.set_color t.cursor
    (Option.map
       (fun (r, g, b) -> (clamp_byte r, clamp_byte g, clamp_byte b))
       cursor.color)

let cursor t =
  let s = Cursor_state.snapshot t.cursor in
  {
    position =
      (if s.has_position then Some (max 0 (s.col - 1), max 0 (s.row - 1))
       else None);
    style = s.style;
    blinking = s.blinking;
    color =
      Option.map
        (fun (r, g, b) -> (clamp_byte r, clamp_byte g, clamp_byte b))
        s.color;
    visible = s.visible;
  }

let apply_capabilities r ~explicit_width ~explicit_cursor_positioning
    ~hyperlinks =
  r.explicit_width_capable <- explicit_width;
  r.use_explicit_width <- r.prefer_explicit_width && explicit_width;
  r.use_explicit_cursor_positioning <-
    (not r.use_explicit_width) && explicit_cursor_positioning;
  r.hyperlinks_capable <- hyperlinks

let set_explicit_width t flag =
  t.prefer_explicit_width <- flag;
  t.use_explicit_width <- flag && t.explicit_width_capable

let set_width_method (t : t) (method_ : Glyph.width_method) =
  Grid.set_width_method t.current method_;
  Grid.set_width_method t.next method_

type effect_id = int

let post_process f frame =
  let id = frame.next_effect_id in
  frame.next_effect_id <- id + 1;
  frame.post_process_fns <- (id, f) :: frame.post_process_fns;
  frame.post_process_dirty <- true;
  id

let remove_post_process id frame =
  frame.post_process_fns <-
    List.filter ~f:(fun (eid, _) -> eid <> id) frame.post_process_fns;
  frame.post_process_dirty <- true

let clear_post_processes frame =
  frame.post_process_fns <- [];
  frame.post_process_cache <- [];
  frame.post_process_dirty <- false

let add_hit_region frame ~x ~y ~width ~height ~id =
  Hit_grid.add frame.hit_next ~x ~y ~width ~height ~id
