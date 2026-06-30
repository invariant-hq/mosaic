type terminal_op =
  | Move_cursor of { row : int; col : int }
  | Reset_sgr
  | Write of string
  | Erase_line
  | Erase_below
  | Clear_and_home
  | Scroll_up of int
  | Set_scroll_region of { top : int; bottom : int }
  | Reset_scroll_region

type plan = {
  terminal_ops : terminal_op list;
  region_changed : bool;
  invalidate_presented : bool;
  force_full_redraw : bool;
}

type region = { row_offset : int; height : int }
type static_write = { text : string; rows : int }

type cursor_anchor = {
  render_offset : int;
  static_needs_newline : bool;
  scroll_bottom : bool;
}

type t = {
  terminal_height : int;
  min_live_height : int;
  render_offset : int;
  static_needs_newline : bool;
  static_queue : static_write list;
}

let empty_plan =
  {
    terminal_ops = [];
    region_changed = false;
    invalidate_presented = false;
    force_full_redraw = false;
  }

let clamp lo hi v = max lo (min hi v)
let live_height t = t.terminal_height - t.render_offset

let make ~terminal_height ~min_live_height ~render_offset ~static_needs_newline
    ~static_queue =
  let terminal_height = max 1 terminal_height in
  let min_live_height = min terminal_height (max 1 min_live_height) in
  let render_offset =
    clamp 0 (max 0 (terminal_height - min_live_height)) render_offset
  in
  {
    terminal_height;
    min_live_height;
    render_offset;
    static_needs_newline;
    static_queue;
  }

let create ~terminal_height ~min_live_height ~render_offset
    ~static_needs_newline =
  make ~terminal_height ~min_live_height ~render_offset ~static_needs_newline
    ~static_queue:[]

let anchor_of_cursor ~terminal_height ~row ~col =
  let terminal_height = max 1 terminal_height in
  let row = clamp 1 terminal_height row in
  let col = max 1 col in
  if row >= terminal_height then
    {
      render_offset = terminal_height - 1;
      static_needs_newline = false;
      scroll_bottom = true;
    }
  else
    let render_offset, static_needs_newline =
      if col = 1 then (max 0 (row - 1), true) else (row, true)
    in
    { render_offset; static_needs_newline; scroll_bottom = false }

let terminal_height t = t.terminal_height
let min_live_height t = t.min_live_height
let render_offset t = t.render_offset
let static_needs_newline t = t.static_needs_newline
let live_region t = { row_offset = t.render_offset; height = live_height t }
let size t ~width = (width, live_height t)

let starts_with_newline s =
  let len = String.length s in
  if len = 0 then false
  else if Char.equal (String.unsafe_get s 0) '\n' then true
  else
    len > 1
    && Char.equal (String.unsafe_get s 0) '\r'
    && Char.equal (String.unsafe_get s 1) '\n'

let ends_with_newline s =
  let len = String.length s in
  len > 0 && Char.equal (String.unsafe_get s (len - 1)) '\n'

type static_step = {
  base : int;
  needs_newline : bool;
  payload_rows : int;
  next_offset : int;
  next_static_needs_newline : bool;
}

let static_step t ~offset ~static_needs_newline { text; rows } =
  let max_offset = max 0 (t.terminal_height - t.min_live_height) in
  let base = if offset = 0 then 1 else offset in
  let needs_newline = static_needs_newline && not (starts_with_newline text) in
  let payload_rows = rows + if needs_newline then 1 else 0 in
  let grow_by = min payload_rows (max 0 (max_offset - base)) in
  {
    base;
    needs_newline;
    payload_rows;
    next_offset = (if max_offset = 0 then 0 else base + grow_by);
    next_static_needs_newline = not (ends_with_newline text);
  }

let projected_after_static t =
  List.fold_left
    (fun (offset, static_needs_newline) write ->
      let step = static_step t ~offset ~static_needs_newline write in
      (step.next_offset, step.next_static_needs_newline))
    (t.render_offset, t.static_needs_newline)
    (List.rev t.static_queue)

let effective_region t =
  match t.static_queue with
  | [] -> live_region t
  | _ ->
      let offset, _ = projected_after_static t in
      {
        row_offset = offset;
        height = max t.min_live_height (t.terminal_height - offset);
      }

let effective_size t ~width =
  let region = effective_region t in
  (width, region.height)

let resize t ~terminal_height =
  let next =
    make ~terminal_height ~min_live_height:t.min_live_height
      ~render_offset:t.render_offset
      ~static_needs_newline:t.static_needs_newline ~static_queue:t.static_queue
  in
  let region_changed = live_region next <> live_region t in
  (next, { empty_plan with region_changed; force_full_redraw = region_changed })

let reanchor t ~render_offset ~static_needs_newline =
  make ~terminal_height:t.terminal_height ~min_live_height:t.min_live_height
    ~render_offset ~static_needs_newline ~static_queue:t.static_queue

let enqueue_static t ({ text; rows } as write) =
  if rows < 0 then invalid_arg "Primary.enqueue_static: rows must be >= 0";
  if String.length text = 0 then t
  else { t with static_queue = write :: t.static_queue }

let has_pending_static t = t.static_queue <> []

let static_ops step text =
  let move = Move_cursor { row = step.base; col = 1 } in
  if step.needs_newline then [ move; Reset_sgr; Write "\r\n"; Write text ]
  else [ move; Reset_sgr; Write text ]

let flush_one t ({ text; _ } as write) =
  let step =
    static_step t ~offset:t.render_offset
      ~static_needs_newline:t.static_needs_newline write
  in
  let ops = static_ops step text in
  let next =
    {
      t with
      render_offset = step.next_offset;
      static_needs_newline = step.next_static_needs_newline;
    }
  in
  (next, ops)

let flush_pinned_static t queue =
  let max_offset = max 0 (t.terminal_height - t.min_live_height) in
  let ops_rev, offset, static_needs_newline =
    List.fold_left
      (fun (ops_rev, offset, static_needs_newline) ({ text; _ } as write) ->
        let step = static_step t ~offset ~static_needs_newline write in
        let ops_rev = List.rev_append (static_ops step text) ops_rev in
        (ops_rev, step.next_offset, step.next_static_needs_newline))
      ([], t.render_offset, t.static_needs_newline)
      queue
  in
  let next =
    { t with render_offset = offset; static_needs_newline; static_queue = [] }
  in
  let terminal_ops =
    Set_scroll_region { top = 1; bottom = max_offset }
    :: List.rev_append ops_rev [ Reset_scroll_region ]
  in
  ( next,
    {
      empty_plan with
      terminal_ops;
      invalidate_presented = true;
      force_full_redraw = true;
    } )

let full_height_static_ops step text =
  let ops =
    [ Move_cursor { row = 1; col = 1 }; Reset_sgr; Erase_line; Write text ]
  in
  if step.payload_rows > 0 then ops @ [ Scroll_up step.payload_rows ] else ops

let flush_full_height_static t queue =
  let ops_rev, static_needs_newline =
    List.fold_left
      (fun (ops_rev, static_needs_newline) ({ text; rows } as write) ->
        let step = static_step t ~offset:0 ~static_needs_newline write in
        let step = { step with payload_rows = rows } in
        let ops_rev =
          List.rev_append (full_height_static_ops step text) ops_rev
        in
        (ops_rev, step.next_static_needs_newline))
      ([], t.static_needs_newline)
      queue
  in
  let next =
    { t with render_offset = 0; static_needs_newline; static_queue = [] }
  in
  ( next,
    {
      empty_plan with
      terminal_ops = List.rev ops_rev;
      invalidate_presented = true;
      force_full_redraw = true;
    } )

let flush_static t =
  match t.static_queue with
  | [] -> (t, empty_plan)
  | rev_queue ->
      let queue = List.rev rev_queue in
      let max_offset = max 0 (t.terminal_height - t.min_live_height) in
      if max_offset = 0 then flush_full_height_static t queue
      else if t.render_offset = max_offset then flush_pinned_static t queue
      else
        let projected_offset, _ = projected_after_static t in
        let reaches_pin = projected_offset = max_offset in
        let start_plan =
          {
            empty_plan with
            terminal_ops =
              [
                Move_cursor { row = t.render_offset + 1; col = 1 }; Erase_below;
              ];
            invalidate_presented = true;
            force_full_redraw = true;
          }
        in
        let start = { t with static_queue = [] } in
        let next, static_ops_rev =
          List.fold_left
            (fun (state, ops_rev) write ->
              let state, ops = flush_one state write in
              (state, List.rev_append ops ops_rev))
            (start, []) queue
        in
        let region_changed = live_region next <> live_region t in
        let static_ops = List.rev static_ops_rev in
        let terminal_ops =
          if reaches_pin then
            start_plan.terminal_ops
            @ [ Set_scroll_region { top = 1; bottom = max_offset } ]
            @ static_ops @ [ Reset_scroll_region ]
          else start_plan.terminal_ops @ static_ops
        in
        (next, { start_plan with terminal_ops; region_changed })

let clear_static t =
  let next =
    make ~terminal_height:t.terminal_height ~min_live_height:t.min_live_height
      ~render_offset:0 ~static_needs_newline:false ~static_queue:[]
  in
  ( next,
    {
      terminal_ops = [ Clear_and_home ];
      region_changed = true;
      invalidate_presented = true;
      force_full_redraw = true;
    } )

let apply_required_rows t ~active_rows ~required_rows =
  let required =
    max (max 1 active_rows)
      (match required_rows with Some rows -> max 1 rows | None -> 0)
  in
  let current_height = live_height t in
  let max_rows = t.terminal_height in
  if required > current_height then
    let new_offset = max 0 (t.terminal_height - required) in
    if new_offset < t.render_offset then
      let rows_to_claim = t.render_offset - new_offset in
      let newline_ops = List.init rows_to_claim (fun _ -> Write "\r\n") in
      let rec erase_rows row acc =
        if row > t.render_offset then List.rev acc
        else
          erase_rows (row + 1)
            (Erase_line :: Move_cursor { row; col = 1 } :: acc)
      in
      let next = { t with render_offset = new_offset } in
      let render_height = if required > max_rows then Some max_rows else None in
      ( next,
        {
          terminal_ops =
            (Move_cursor { row = t.terminal_height; col = 1 } :: newline_ops)
            @ erase_rows (new_offset + 1) [];
          region_changed = true;
          invalidate_presented = true;
          force_full_redraw = true;
        },
        render_height )
    else
      let render_height = if required > max_rows then Some max_rows else None in
      (t, empty_plan, render_height)
  else (t, empty_plan, None)

let terminal_cursor_row t ~live_row ~live_height =
  t.render_offset + clamp 0 (max 0 (live_height - 1)) live_row + 1

let default_terminal_cursor_row t ~live_height =
  t.render_offset + max 1 live_height

let map_mouse_y t y =
  let offset = t.render_offset in
  if y <= offset then -1 else y - offset
