open Matrix

type draw_context = {
  grid : Grid.t;
  hits : Screen.Hit_grid.t;
  cols : int;
  rows : int;
}

module Coord = struct
  type t = int * int

  let compare (a1, b1) (a2, b2) =
    let c = Stdlib.compare a1 a2 in
    if c <> 0 then c else Stdlib.compare b1 b2
end

module CSet = struct
  include Set.Make (Coord)

  let of_list lst = List.fold_left (fun acc elt -> add elt acc) empty lst
end

let erem x y = ((x mod y) + y) mod y
let torus (w, h) (a, b) = (erem a w, erem b h)

let neighbors dim (a, b) =
  let pts =
    [
      (a - 1, b);
      (a + 1, b);
      (a - 1, b - 1);
      (a - 1, b + 1);
      (a, b - 1);
      (a, b + 1);
      (a + 1, b - 1);
      (a + 1, b + 1);
    ]
  in
  List.map (torus dim) pts

let step dim life =
  let counts = Hashtbl.create (max 8 (CSet.cardinal life * 3)) in
  let incr pt =
    let current =
      match Hashtbl.find_opt counts pt with Some n -> n | None -> 0
    in
    Hashtbl.replace counts pt (current + 1)
  in
  CSet.iter
    (fun pt ->
      let neigh = neighbors dim pt in
      List.iter incr neigh)
    life;
  let next = ref CSet.empty in
  Hashtbl.iter
    (fun pt n ->
      if n = 3 || (n = 2 && CSet.mem pt life) then next := CSet.add pt !next)
    counts;
  !next

let initial_pattern dim =
  (* Acorn pattern: small seed with long evolution. *)
  let w, h = dim in
  let base_x = (w / 2) - 3 in
  let base_y = (h / 2) - 1 in
  let rel = [ (1, 0); (3, 1); (0, 2); (1, 2); (4, 2); (5, 2); (6, 2) ] in
  rel
  |> List.map (fun (dx, dy) -> (base_x + dx, base_y + dy))
  |> List.map (torus dim)
  |> CSet.of_list

type state = { dim : int * int; generation : int; life : CSet.t; paused : bool }

let max_gray_level = 23

let background_styles =
  let open Image in
  Array.init (max_gray_level + 1) (fun level ->
      let color = Color.grayscale ~level in
      Style.make ~fg:color ())

let dot_image =
  let open Image in
  let color = Color.bright_magenta in
  let style = Style.make ~fg:color () in
  string ~style "●"

let background_cell ~step x y =
  let open Image in
  let angle = float_of_int (step + x + y) /. 10.0 in
  let s = sin angle in
  let level_float = float_of_int max_gray_level *. s in
  let level =
    int_of_float (Float.round level_float) |> max 0 |> min max_gray_level
  in
  let style = background_styles.(level) in
  string ~style "."

let render st ctx =
  let open Image in
  let cols = ctx.cols in
  let rows = ctx.rows in
  let grid_rows = if rows > 0 then rows - 1 else 0 in
  let row_images =
    let rec build_rows y acc =
      if y >= grid_rows then List.rev acc
      else
        let rec build_cols x acc_cells =
          if x >= cols then List.rev acc_cells
          else
            let cell =
              if CSet.mem (x, y) st.life then dot_image
              else background_cell ~step:st.generation x y
            in
            build_cols (x + 1) (cell :: acc_cells)
        in
        let row = hcat (build_cols 0 []) in
        build_rows (y + 1) (row :: acc)
    in
    build_rows 0 []
  in
  let world = match row_images with [] -> empty | rows_img -> vcat rows_img in
  let status_text =
    if st.paused then "Game of Life – paused"
    else Printf.sprintf "Game of Life – generation %04d" st.generation
  in
  let status_style = Style.make ~fg:Color.bright_black () in
  let status_line =
    string ~style:status_style status_text |> hsnap ~align:`Left cols
  in
  let bg = fill ~color:Color.black ~width:cols ~height:rows () in
  let stacked = vcat [ world; status_line ] in
  let img = overlay [ bg; stacked ] in
  draw img ctx.grid ctx.hits

let initial_state ~cols ~rows =
  let grid_rows = if rows > 0 then rows - 1 else 0 in
  let dim = if grid_rows <= 0 then (max 1 cols, 1) else (cols, grid_rows) in
  { dim; generation = 0; life = initial_pattern dim; paused = false }

let resize_state st ~cols ~rows =
  let grid_rows = if rows > 0 then rows - 1 else 0 in
  let dim = if grid_rows <= 0 then (max 1 cols, 1) else (cols, grid_rows) in
  let w, h = dim in
  let life =
    CSet.fold
      (fun (x, y) acc ->
        let pt = torus (w, h) (x, y) in
        CSet.add pt acc)
      st.life CSet.empty
  in
  { st with dim; life }

let () =
  let frame_interval = 0.1 in
  let target_fps = 1. /. frame_interval in
  let app = Matrix.create ~target_fps:(Some target_fps) () in
  let state = ref (initial_state ~cols:1 ~rows:1) in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ ->
      if not !state.paused then
        let w, h = !state.dim in
        let life = step (w, h) !state.life in
        state := { !state with life; generation = !state.generation + 1 })
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char uchar; modifier; _ } ->
          let code = Uchar.to_int uchar in
          let ctrl_c =
            modifier.Input.Modifier.ctrl
            && (code = Char.code 'c' || code = Char.code 'C')
          in
          if ctrl_c then Matrix.stop app
          else if code = Char.code ' ' then
            state := { !state with paused = not !state.paused }
          else if code = Char.code 'c' || code = Char.code 'C' then
            state := { !state with life = CSet.empty; generation = 0 }
      | Input.Mouse
          {
            Input.Mouse.x;
            y;
            kind = Down { button = Left } | Drag { button = Left };
            _;
          } ->
          let w, h = !state.dim in
          if y < h then
            let pt = torus (w, h) (x, y) in
            let life = CSet.add pt !state.life in
            state := { !state with life }
      | _ -> ())
    ~on_resize:(fun _ ~cols ~rows -> state := resize_state !state ~cols ~rows)
    ~on_render:(fun app ->
      let cols, rows = Matrix.size app in
      let ctx =
        { grid = Matrix.grid app; hits = Matrix.hits app; cols; rows }
      in
      render !state ctx)
