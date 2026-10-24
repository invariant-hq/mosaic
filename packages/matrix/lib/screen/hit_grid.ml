module Grid = Matrix_grid

(* Rectangles are Matrix_grid.region. Mosaic_ui.Selection.bounds is a
   structurally identical {x; y; width; height} record in cell coordinates;
   unifying them should move it to Matrix_grid.region rather than introduce
   another copy. *)

type id = int
type ints = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  mutable width : int;
  mutable height : int;
  mutable capacity : int;
  mutable data : ints;
  clip_stack : Grid.region Dynarray.t;
}

let empty_id = 0

let ensure_capacity t cells =
  if cells > t.capacity then (
    (* Geometric growth to reduce allocation churn on frequent resizes *)
    let new_capacity = max cells (t.capacity * 2) in
    t.data <- Bigarray.Array1.create Bigarray.int Bigarray.c_layout new_capacity;
    t.capacity <- new_capacity)

(* The element type is stated so the stores compile to plain stores; a
   polymorphic bigarray store calls into the runtime for every cell. *)
let linear_fill (arr : ints) start len value =
  let limit = start + len in
  let rec loop i =
    if i < limit then (
      Bigarray.Array1.unsafe_set arr i value;
      loop (i + 1))
  in
  loop start

let clear t =
  let len = t.width * t.height in
  if len > 0 then linear_fill t.data 0 len 0

let resize t ~width ~height =
  let width = max 0 width in
  let height = max 0 height in
  let cells = width * height in
  ensure_capacity t cells;
  t.width <- width;
  t.height <- height;
  (* Always clear after resize to ensure deterministic state *)
  clear t

let create ~width ~height =
  let t =
    {
      width = 0;
      height = 0;
      capacity = 0;
      data = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 0;
      clip_stack = Dynarray.create ();
    }
  in
  resize t ~width ~height;
  t

(* Scissor stack *)

let rect_intersection (a : Grid.region) (b : Grid.region) =
  let x = max a.x b.x in
  let y = max a.y b.y in
  let w = min (a.x + a.width) (b.x + b.width) - x in
  let h = min (a.y + a.height) (b.y + b.height) - y in
  if w > 0 && h > 0 then Some { Grid.x; y; width = w; height = h } else None

let current_clip s = if Dynarray.is_empty s then None else Dynarray.find_last s

let push_clip t rect =
  let r =
    match current_clip t.clip_stack with
    | None -> rect
    | Some c -> (
        match rect_intersection c rect with
        | Some i -> i
        | None -> { Grid.x = 0; y = 0; width = 0; height = 0 })
  in
  Dynarray.add_last t.clip_stack r

let pop_clip t = ignore (Dynarray.pop_last_opt t.clip_stack)
let clear_clip t = Dynarray.clear t.clip_stack

let with_clip t rect f =
  push_clip t rect;
  Fun.protect ~finally:(fun () -> pop_clip t) f

(* Clip a rectangle against grid bounds and the active scissor. Returns (x0, y0,
   x1, y1) with x0 < x1 && y0 < y1 when the region is non-empty. *)
let clip_region t ~x ~y ~width ~height =
  let x0 = max 0 x in
  let y0 = max 0 y in
  let x1 = min t.width (x + width) in
  let y1 = min t.height (y + height) in
  match current_clip t.clip_stack with
  | None -> (x0, y0, x1, y1)
  | Some s ->
      let x0 = max x0 s.x in
      let y0 = max y0 s.y in
      let x1 = min x1 (s.x + s.width) in
      let y1 = min y1 (s.y + s.height) in
      (x0, y0, x1, y1)

let add t ~x ~y ~width ~height ~id =
  let x0, y0, x1, y1 = clip_region t ~x ~y ~width ~height in
  if x0 < x1 && y0 < y1 then
    let stride = t.width in
    let row_width = x1 - x0 in
    let rec loop_rows row =
      if row < y1 then (
        let start = (row * stride) + x0 in
        linear_fill t.data start row_width id;
        loop_rows (row + 1))
    in
    loop_rows y0

let get t ~x ~y =
  if x < 0 || y < 0 || x >= t.width || y >= t.height then empty_id
  else
    let idx = (y * t.width) + x in
    Bigarray.Array1.unsafe_get t.data idx

let draw ~src ~dst ~x ~y =
  let src =
    if src != dst then src
    else
      let len = src.width * src.height in
      let copy = Bigarray.Array1.create Bigarray.int Bigarray.c_layout len in
      Bigarray.Array1.blit (Bigarray.Array1.sub src.data 0 len) copy;
      { src with capacity = len; data = copy }
  in
  let x0, y0, x1, y1 =
    clip_region dst ~x ~y ~width:src.width ~height:src.height
  in
  for row = y0 to y1 - 1 do
    let s = ((row - y) * src.width) - x and d = row * dst.width in
    for col = x0 to x1 - 1 do
      let id = Bigarray.Array1.unsafe_get src.data (s + col) in
      if id <> empty_id then Bigarray.Array1.unsafe_set dst.data (d + col) id
    done
  done
