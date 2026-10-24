type terminal_op =
  | Move_cursor of { row : int; col : int }
  | Reset_sgr
  | Write of string
  | Draw of (unit -> string)
  | Clear_and_home
  | Clear_scrollback

type region = { row_offset : int; height : int }

type static_write = {
  draw : top:int -> string;
  rows : int;
  preserve_live_region : bool;
}

type t = {
  terminal_height : int;
  min_live_height : int;
  render_offset : int;
  queue : static_write list; (* Newest first. *)
  replace : bool;
}

let clamp lo hi v = max lo (min hi v)
let live_height t = t.terminal_height - t.render_offset

let create ~terminal_height ~min_live_height =
  {
    terminal_height = max 1 terminal_height;
    min_live_height = max 1 min_live_height;
    render_offset = 0;
    queue = [];
    replace = false;
  }

(* [settle t ~offset ~rows] starts the live region at row [offset], which may
   be the terminal height when no row is left, and gives it at least [rows]
   rows, and at least the minimum, by scrolling the rows above it into the
   scrollback: each line feed on the bottom row moves the screen up a row.
   It never shrinks the region. *)
let settle t ~offset ~rows =
  let h = t.terminal_height in
  let offset = clamp 0 h offset in
  let rows = clamp 1 h (max rows t.min_live_height) in
  let settled = min offset (h - rows) in
  let scroll = offset - settled in
  let ops =
    if scroll = 0 then []
    else
      [
        Move_cursor { row = h; col = 1 };
        Write (String.concat "" (List.init scroll (fun _ -> "\r\n")));
      ]
  in
  ({ t with render_offset = settled }, ops)

let anchor t ~row ~col =
  let row = clamp 1 t.terminal_height row in
  settle t ~offset:(if col <= 1 then row - 1 else row) ~rows:0

let resize t ~terminal_height =
  settle
    { t with terminal_height = max 1 terminal_height }
    ~offset:t.render_offset ~rows:0

let enqueue_static t ({ rows; _ } as write) =
  if rows < 0 then invalid_arg "Primary.enqueue_static: rows must be >= 0";
  if rows = 0 then t else { t with queue = write :: t.queue }

let replace_static t ({ rows; _ } as write) =
  if rows < 0 then invalid_arg "Primary.replace_static: rows must be >= 0";
  { t with queue = (if rows = 0 then [] else [ write ]); replace = true }

(* The rows go at the top of the live region. Each ends with a line feed,
   which scrolls the screen once the bottom row is reached, so the live
   region starts on the row the cursor ends on, and the rows end on the rows
   above it. *)
let write_static t { draw; rows; preserve_live_region } =
  let below = min (t.render_offset + rows) (t.terminal_height - 1) in
  let write =
    [
      Move_cursor { row = t.render_offset + 1; col = 1 };
      Reset_sgr;
      Draw (fun () -> draw ~top:(below - rows));
    ]
  in
  let rows = if preserve_live_region then live_height t else 0 in
  let t, scroll = settle t ~offset:below ~rows in
  (t, write @ scroll)

let flush_static t =
  let start, ops =
    if t.replace then
      ({ t with render_offset = 0 }, [ Clear_and_home; Clear_scrollback ])
    else (t, [])
  in
  let flushed, ops =
    List.fold_left
      (fun (t, ops) write ->
        let t, write = write_static t write in
        (t, ops @ write))
      (start, ops) (List.rev t.queue)
  in
  ({ flushed with queue = []; replace = false }, ops)

let clear_static t =
  ({ t with render_offset = 0; queue = []; replace = false }, [ Clear_and_home ])

let require_rows t ~rows = settle t ~offset:t.render_offset ~rows
let render_offset t = t.render_offset
let live_region t = { row_offset = t.render_offset; height = live_height t }
let effective_region t = live_region (fst (flush_static t))

let terminal_cursor_row t ~live_row =
  t.render_offset + clamp 0 (live_height t - 1) live_row + 1

let map_mouse_y t y = if y < t.render_offset then -1 else y - t.render_offset
