let ensure_dir dir =
  if Sys.file_exists dir then (
    if not (Sys.is_directory dir) then
      failwith
        (Printf.sprintf "Frame_dump: %s exists but is not a directory" dir))
  else
    try Sys.mkdir dir 0o755
    with Sys_error _ ->
      if Sys.file_exists dir && Sys.is_directory dir then ()
      else
        failwith (Printf.sprintf "Frame_dump: cannot create directory %s" dir)

let write_file path contents =
  let oc = open_out_bin path in
  try
    output_string oc contents;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let default_dir = "frames"
let default_pattern = "frame_"
let dump_index = ref 0

let next_index () =
  incr dump_index;
  !dump_index

let make_path ?(dir = default_dir) ?(pattern = default_pattern) index =
  ensure_dir dir;
  let name = Printf.sprintf "%s%06d.ansi" pattern index in
  Filename.concat dir name

let hit_grid_text (screen : Screen.t) : string =
  let grid = Screen.next_grid screen in
  let hits = Screen.next_hit_grid screen in
  let cols = Grid.width grid in
  let rows = Grid.height grid in
  if cols <= 0 || rows <= 0 then ""
  else
    let max_id =
      let m = ref 0 in
      for y = 0 to rows - 1 do
        for x = 0 to cols - 1 do
          let id = Screen.Hit_grid.get hits ~x ~y in
          if id > !m then m := id
        done
      done;
      !m
    in
    let pad = max 1 (String.length (string_of_int max_id)) in
    let b = Buffer.create (rows * (cols * (pad + 1))) in
    let pp_cell id =
      if id = 0 then (
        Buffer.add_char b '.';
        for _ = 1 to pad - 1 do
          Buffer.add_char b ' '
        done)
      else
        let s = string_of_int id in
        Buffer.add_string b s;
        for _ = 1 to pad - String.length s do
          Buffer.add_char b ' '
        done
    in
    for y = 0 to rows - 1 do
      for x = 0 to cols - 1 do
        let id = Screen.Hit_grid.get hits ~x ~y in
        pp_cell id;
        if x < cols - 1 then Buffer.add_char b ' '
      done;
      if y < rows - 1 then Buffer.add_char b '\n'
    done;
    Buffer.contents b

let snapshot ?dir ?pattern ?(hits = false) (screen : Screen.t) =
  let idx = next_index () in
  let grid = Screen.next_grid screen in
  let ansi = Grid.to_ansi grid in
  let base = make_path ?dir ?pattern idx in
  write_file base ansi;
  let stem =
    Filename.chop_suffix_opt ~suffix:".ansi" base |> Option.value ~default:base
  in
  if hits then
    let path = stem ^ ".hits.txt" in
    write_file path (hit_grid_text screen)

let on_frame ?dir ?pattern ?(hits = false) ~every () : Screen.t -> unit =
  if every <= 0 then invalid_arg "Frame_dump.on_frame: every must be > 0";
  let frame_counter = ref 0 in
  fun (screen : Screen.t) ->
    incr frame_counter;
    if !frame_counter mod every = 0 then snapshot ?dir ?pattern ~hits screen
