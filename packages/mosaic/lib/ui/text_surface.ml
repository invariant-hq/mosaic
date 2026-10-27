module Layout = Matrix.Text.Layout

type wrap = Layout.wrap

type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  mutable notified_version : int;
  mutable wrap : wrap;
  mutable wrap_width : int option;
  mutable truncate : bool;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable ground : Theme.role option;
  (* The styles of the render in progress, resolved from the theme in scope
     as it starts ([render]), and the content's style of the run at hand. *)
  mutable role : Ansi.Style.t;
  mutable ground_style : Ansi.Style.t;
  mutable highlight : Ansi.Style.t;
  mutable run_style : Ansi.Style.t;
  mutable selection_bg : Ansi.Color.t option;
  mutable selection_fg : Ansi.Color.t option;
  mutable selection : (int * int) option;
  (* Retain a bounded set of geometries, keyed by source version, the
     screen's width method and width, so that reading one again allocates
     nothing. The buffer's source keeps them too, and an edit carries them
     to the source it makes. Measurement probes keep their sizes unless
     geometry already exists. Wrap-mode and truncation changes invalidate
     both caches. *)
  mutable display_cache : (geometry_key * Layout.t) list;
  mutable measure_cache : (geometry_key * (int * int)) list;
}

(* Source version, width method and cache width. *)
and geometry_key = int * Matrix.Text.width_method * int

let display_cache_capacity = 4

(* ───── Accessors ───── *)

let buffer t = t.buffer
let node t = t.node
let wrap t = t.wrap

let effective_wrap_width t ?override () =
  match override with
  | Some w -> w
  | None -> Option.value ~default:(Renderable.width t.node) t.wrap_width

(* The method is the screen's now: it can change while the text lives. *)
let geometry_key t width =
  ( Text_buffer.version t.buffer,
    Renderable.Private.width_method t.node,
    match t.wrap with `None when not t.truncate -> 0 | _ -> max 0 width )

(* Whether a cached geometry was built from the same text by the same method,
   whatever its width. *)
let same_text (version, width_method, _) (version', width_method', _) =
  version = version' && width_method = width_method'

let layout_for t ~width =
  let ((_, width_method, _) as key) = geometry_key t width in
  match List.assoc_opt key t.display_cache with
  | Some layout -> layout
  | None ->
      let layout =
        Layout.create
          (Text_buffer.source t.buffer)
          ~width_method
          ~tab_width:(Text_buffer.tab_width t.buffer)
          ~width ~wrap:t.wrap ~truncate:t.truncate
      in
      (* Cap all retained rows at 32 MiB. An oversized active layout is kept
         alone, so changing the active width cannot leave oversized inactive
         rows behind, even on a cache hit. Eight words a row count more than
         a layout's rows take, so what the surface keeps stays within the
         budget. *)
      let row_bytes layout =
        Layout.row_count layout * 8 * (Sys.word_size / 8)
      in
      let budget = ref (max 0 ((32 * 1024 * 1024) - row_bytes layout)) in
      let count = ref 1 in
      let retained =
        List.filter
          (fun (old_key, old) ->
            let bytes = row_bytes old in
            if
              same_text old_key key
              && !count < display_cache_capacity
              && bytes <= !budget
            then (
              incr count;
              budget := !budget - bytes;
              true)
            else false)
          t.display_cache
      in
      t.display_cache <- (key, layout) :: retained;
      layout

let measure_for t ~width =
  let ((_, width_method, _) as key) = geometry_key t width in
  match List.assoc_opt key t.display_cache with
  | Some geometry -> (Layout.row_count geometry, Layout.max_width geometry)
  | None -> (
      match List.assoc_opt key t.measure_cache with
      | Some size -> size
      | None ->
          let size =
            Layout.measure
              (Text_buffer.source t.buffer)
              ~width_method
              ~tab_width:(Text_buffer.tab_width t.buffer)
              ~width ~wrap:t.wrap
          in
          let count = ref 1 in
          let retained =
            List.filter
              (fun (old_key, _) ->
                if same_text old_key key && !count < display_cache_capacity then (
                  incr count;
                  true)
                else false)
              t.measure_cache
          in
          t.measure_cache <- (key, size) :: retained;
          size)

let layout t = layout_for t ~width:(effective_wrap_width t ())
let display_line_count t = Layout.row_count (layout t)

(* ───── Scroll ───── *)

let scroll_x t = t.scroll_x
let scroll_y t = t.scroll_y
let scroll_height t = display_line_count t
let scroll_width t = Layout.max_width (layout t)
let max_scroll_x t = max 0 (scroll_width t - Renderable.width t.node)
let max_scroll_y t = max 0 (scroll_height t - Renderable.height t.node)
let max_cursor_scroll_x t = max 0 (scroll_width t - Renderable.width t.node + 1)

let set_scroll_x t x =
  let clamped = max 0 (min x (max_scroll_x t)) in
  if t.scroll_x <> clamped then begin
    t.scroll_x <- clamped;
    Renderable.request_render t.node
  end

let set_scroll_x_for_cursor t x =
  let clamped = max 0 (min x (max_cursor_scroll_x t)) in
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
    t.display_cache <- [];
    t.measure_cache <- [];
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

let wrap_width t = t.wrap_width

let set_wrap_width t w =
  if t.wrap_width <> w then begin
    t.wrap_width <- w;
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

let truncate t = t.truncate

let set_truncate t v =
  if t.truncate <> v then begin
    t.truncate <- v;
    t.display_cache <- [];
    t.measure_cache <- [];
    Renderable.mark_dirty t.node;
    Renderable.request_render t.node
  end

(* ───── Invalidation ───── *)

let invalidate t =
  let version = Text_buffer.version t.buffer in
  if version <> t.notified_version then begin
    t.notified_version <- version;
    t.display_cache <- [];
    t.measure_cache <- [];
    Renderable.mark_dirty t.node
  end;
  Renderable.request_render t.node

(* ───── Roles ───── *)

let set_ground t ground =
  if t.ground <> ground then begin
    t.ground <- ground;
    Renderable.request_render t.node
  end

(* ───── Selection ───── *)

let set_selection_bg t bg =
  if not (Option.equal Ansi.Color.equal t.selection_bg bg) then begin
    t.selection_bg <- bg;
    if Option.is_some t.selection then Renderable.request_render t.node
  end

let set_selection_fg t fg =
  if not (Option.equal Ansi.Color.equal t.selection_fg fg) then begin
    t.selection_fg <- fg;
    if Option.is_some t.selection then Renderable.request_render t.node
  end

let local_caret t ~x ~y =
  Layout.hit (layout t) ~row:(t.scroll_y + y) ~column:(t.scroll_x + x)

let set_selection t ~start ~end_ =
  let len = Text_buffer.grapheme_count t.buffer in
  let s = max 0 (min start len) and e = max 0 (min end_ len) in
  let selection = if s = e then None else Some (min s e, max s e) in
  let changed = t.selection <> selection in
  if changed then begin
    t.selection <- selection;
    Renderable.request_render t.node
  end;
  changed

let selection t = t.selection

let set_local_selection t ~anchor_x ~anchor_y ~focus_x ~focus_y =
  let geometry = layout t in
  let range x y =
    Layout.coverage geometry ~row:(t.scroll_y + y) ~column:(t.scroll_x + x)
  in
  let a, ae = range anchor_x anchor_y in
  let f, fe = range focus_x focus_y in
  let first, last =
    if anchor_x = focus_x && anchor_y = focus_y then (a, a)
    else (min a f, max ae fe)
  in
  set_selection t ~start:first ~end_:last

let update_local_selection = set_local_selection

let reset_selection t =
  if Option.is_some t.selection then begin
    t.selection <- None;
    Renderable.request_render t.node
  end

let has_selection t = Option.is_some t.selection

let selected_text t =
  match t.selection with
  | None -> ""
  | Some (first, last) ->
      Text_buffer.text_in_range t.buffer ~start:first ~len:(last - first)

(* ───── Rendering ───── *)

(* [over base style] is [style] merged over [base], and [style] itself when
   [base] is the default style, which merges as nothing. *)
let over base style =
  if Ansi.Style.equal base Ansi.Style.default then style
  else Ansi.Style.merge ~base ~overlay:style

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node and h = Renderable.height t.node in
  let ox = Renderable.x t.node and oy = Renderable.y t.node in
  let clip = Matrix_grid.clip_region grid in
  let x0 = max ox clip.x and y0 = max oy clip.y in
  let x1 = min (ox + w) (clip.x + clip.width) in
  let y1 = min (oy + h) (clip.y + clip.height) in
  if x0 < x1 && y0 < y1 then begin
    let geometry = layout t in
    let src = Layout.source geometry in
    let text = Layout.Source.text src in
    let tab_width = Text_buffer.tab_width t.buffer in
    let width_method = Renderable.Private.width_method t.node in
    let sel_start, sel_end = Option.value ~default:(0, 0) t.selection in
    (* The text's paint, resolved against the theme in scope: the content's
       style merged over the [Plain] role, then that over the surface the
       text lies on, or over [Highlight] where it is selected and no
       selection color is given. A default style merges as nothing, so the
       default themes paint without allocating, and the styles are kept in
       [t] so that a row's function captures nothing more. *)
    let theme = Renderable.theme t.node in
    t.role <- Theme.style theme Plain;
    t.ground_style <-
      (match t.ground with
      | None -> Ansi.Style.default
      | Some s -> Theme.style theme s);
    t.highlight <- Theme.style theme Highlight;
    for y = y0 to y1 - 1 do
      let row = t.scroll_y + y - oy in
      let first_byte = ref 0 and last_byte = ref 0 and batch_x = ref 0 in
      let batch_style = ref Ansi.Style.default and active = ref false in
      let style_end = ref (-1) and base_style = ref Ansi.Style.default in
      let flush () =
        if !active then begin
          Matrix_grid.draw_text ~style:!batch_style ~tab_width ~width_method
            grid ~x:!batch_x ~y
            ~text:(String.sub text !first_byte (!last_byte - !first_byte));
          active := false
        end
      in
      Layout.iter_cells geometry ~row
        ~first_column:(t.scroll_x + x0 - ox)
        ~last_column:(t.scroll_x + x1 - ox)
        (fun ~first ~last ~column ~width ~ellipsis ->
          if first >= !style_end then begin
            let style, stop = Text_buffer.style_at t.buffer first in
            t.run_style <- over t.role style;
            base_style := over t.ground_style t.run_style;
            style_end := stop
          end;
          let style =
            if first < sel_end && last > sel_start then
              match (t.selection_bg, t.selection_fg) with
              | None, None ->
                  Ansi.Style.merge ~base:t.highlight ~overlay:t.run_style
              | bg, fg -> (
                  let s =
                    match bg with
                    | None -> !base_style
                    | Some bg -> Ansi.Style.bg bg !base_style
                  in
                  match fg with None -> s | Some fg -> Ansi.Style.fg fg s)
            else !base_style
          in
          let x = ox + column - t.scroll_x in
          if x < ox then ()
          else if ellipsis then begin
            flush ();
            Matrix_grid.draw_text ~style ~tab_width ~width_method grid ~x ~y
              ~text:"…"
          end
          else if x + width > x1 then begin
            (* The grid would write the cells of a grapheme past the clip to
               keep it whole, over a clipping box's border. A grapheme the
               visible columns cut shows blanks in its style instead, as a
               terminal shows a cut wide character. *)
            flush ();
            Matrix_grid.draw_text ~style grid ~x ~y
              ~text:(String.make (x1 - x) ' ')
          end
          else begin
            let a = Layout.Source.byte_offset src first in
            let b = Layout.Source.byte_offset src last in
            if !active && !last_byte = a && Ansi.Style.equal style !batch_style
            then last_byte := b
            else begin
              flush ();
              active := true;
              first_byte := a;
              last_byte := b;
              batch_style := style;
              batch_x := x
            end
          end);
      flush ()
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
  let nlines, max_w =
    match (t.wrap, wrap_width) with
    | `None, _ | (`Char | `Word), None ->
        (* One display line per logical line, at the buffer's untruncated
           widths. For [`None] truncation is a render concern; for
           [`Char]/[`Word] at max-content, wrapping only ever narrows a
           line, so the intrinsic width is the longest unwrapped line.
           Measuring either through the node's last laid-out width would
           clamp the intrinsic width to a stale value that never recovers
           when the container grows. Both reads are O(1) on the buffer's
           line cache. *)
        ( Text_buffer.line_count t.buffer,
          Text_buffer.max_line_width t.buffer
            ~width_method:(Renderable.Private.width_method t.node) )
    | (`Char | `Word), Some w ->
        (* Count-only probes share indexing and reuse an admitted layout. *)
        let width = Option.value ~default:w t.wrap_width in
        measure_for t ~width
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
      notified_version = Text_buffer.version buffer;
      wrap = `None;
      wrap_width = None;
      truncate = false;
      scroll_x = 0;
      scroll_y = 0;
      ground = None;
      role = Ansi.Style.default;
      ground_style = Ansi.Style.default;
      highlight = Ansi.Style.default;
      run_style = Ansi.Style.default;
      selection_bg = None;
      selection_fg = None;
      selection = None;
      display_cache = [];
      measure_cache = [];
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  t
