module Style = Ansi.Style

let is_blank s = String.length (String.trim s) = 0
let clamp_nonneg n = max 0 n

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
type section = { title : string option; rows : (string * string) list }

type theme = {
  background : Ansi.Color.t;
  foreground : Ansi.Color.t;
  accent : Ansi.Color.t;
}

type t = {
  mutable sections : section list;
  theme : theme;
  padding : int;
  gap : int;
}

type line_style = [ `Title | `Row | `Spacer ]
type line = { text : string; style : line_style }
type layout = { lines : line list; width : int; height : int }

let default_theme =
  {
    background = Ansi.Color.of_rgba 20 20 40 200;
    foreground = Ansi.Color.of_rgb 230 230 230;
    accent = Ansi.Color.of_rgb 150 200 255;
  }

let create ?(sections = []) ?(theme = default_theme) ?(padding = 1) ?(gap = 1)
    () =
  { sections; theme; padding = clamp_nonneg padding; gap = clamp_nonneg gap }

let set_sections t sections = t.sections <- sections
let section ?title rows = { title; rows }

let normalize_title = function
  | None -> None
  | Some title when is_blank title -> None
  | some -> some

let format_row (label, value) =
  match (is_blank label, is_blank value) with
  | true, true -> ""
  | true, false -> value
  | false, true -> label
  | false, false -> Printf.sprintf "%s: %s" label value

let line_of_row row =
  let text = format_row row in
  if text = "" then None else Some { text; style = `Row }

let lines_of_section section =
  let rows = List.filter_map line_of_row section.rows in
  match normalize_title section.title with
  | None -> rows
  | Some title -> { text = title; style = `Title } :: rows

let blank_lines gap =
  if gap <= 0 then []
  else List.init gap (fun _ -> { text = ""; style = `Spacer })

let build_lines t =
  let gap_lines = blank_lines t.gap in
  let rec aux acc = function
    | [] -> List.rev acc
    | section :: rest ->
        let acc = List.rev_append (lines_of_section section) acc in
        let acc =
          if rest <> [] && gap_lines <> [] then List.rev_append gap_lines acc
          else acc
        in
        aux acc rest
  in
  aux [] t.sections

let layout t =
  match build_lines t with
  | [] -> None
  | lines ->
      let content_width =
        List.fold_left
          (fun acc line ->
            match line.style with
            | `Spacer -> acc
            | _ ->
                (* Use Glyph.String.measure for correct Unicode width
                   calculation *)
                let w =
                  Glyph.String.measure ~width_method:`Unicode ~tab_width:2
                    line.text
                in
                max acc w)
          0 lines
      in
      let base_width = max 1 content_width in
      let width = base_width + (t.padding * 2) in
      let height = List.length lines + (t.padding * 2) in
      Some { lines; width; height }

let draw_layout t grid { lines; width; height } ~x ~y =
  Grid.fill_rect grid ~x ~y ~width ~height ~color:t.theme.background;
  let text_x = x + t.padding in
  let text_y = y + t.padding in
  let normal_style =
    Style.make ~fg:t.theme.foreground ~bg:t.theme.background ()
  in
  let title_style =
    Style.make ~fg:t.theme.accent ~bg:t.theme.background ~bold:true ()
  in
  List.iteri
    (fun idx line ->
      let row = text_y + idx in
      match line.style with
      | `Spacer -> ()
      | `Title ->
          Grid.draw_text grid ~x:text_x ~y:row ~style:title_style
            ~text:line.text
      | `Row ->
          Grid.draw_text grid ~x:text_x ~y:row ~style:normal_style
            ~text:line.text)
    lines

let render ?corner ?(offset = (0, 0)) t grid =
  match layout t with
  | None -> ()
  | Some l -> (
      let dx, dy = offset in
      match corner with
      | None -> draw_layout t grid l ~x:dx ~y:dy
      | Some corner ->
          let grid_width = Grid.width grid in
          let grid_height = Grid.height grid in
          let base_x =
            match corner with
            | `Top_left | `Bottom_left -> 0
            | `Top_right | `Bottom_right -> max 0 (grid_width - l.width)
          in
          let base_y =
            match corner with
            | `Top_left | `Top_right -> 0
            | `Bottom_left | `Bottom_right -> max 0 (grid_height - l.height)
          in
          draw_layout t grid l ~x:(base_x + dx) ~y:(base_y + dy))

module Avg_ring = struct
  type t = {
    capacity : int;
    samples : float array;
    mutable count : int;
    mutable head : int;
    mutable sum : float; (* Running sum for O(1) average *)
  }

  let create capacity =
    if capacity <= 0 then
      { capacity = 0; samples = [||]; count = 0; head = 0; sum = 0. }
    else
      {
        capacity;
        samples = Array.make capacity 0.;
        count = 0;
        head = 0;
        sum = 0.;
      }

  let push t value =
    if t.capacity = 0 then ()
    else if t.count < t.capacity then (
      (* Buffer not yet full: just add the new value *)
      t.samples.(t.head) <- value;
      t.sum <- t.sum +. value;
      t.head <- (t.head + 1) mod t.capacity;
      t.count <- t.count + 1)
    else
      (* Buffer full: subtract old value, add new value *)
      let old_value = t.samples.(t.head) in
      t.samples.(t.head) <- value;
      t.sum <- t.sum -. old_value +. value;
      t.head <- (t.head + 1) mod t.capacity

  let average t = if t.count = 0 then None else Some (t.sum /. float t.count)
end

let fmt_ms value = Printf.sprintf "%.2f ms" value
let bytes_per_word = Sys.word_size / 8

let words_to_mb words =
  float_of_int words *. float bytes_per_word /. (1024. *. 1024.)

let words_to_mb_f words = words *. float bytes_per_word /. (1024. *. 1024.)
let fmt_mb value = Printf.sprintf "%.2f MB" value

let fmt_words_mb words =
  if words <= 0 then "0.00 MB" else fmt_mb (words_to_mb words)

let on_frame ?(corner = `Bottom_right) ?(padding = 1) ?(gap = 1)
    ?(capacity = 120) () : Screen.t -> unit =
  let frame_ring = Avg_ring.create capacity in
  let interval_ring = Avg_ring.create capacity in
  let overlay = create ~padding ~gap () in
  (* Initialize to None to detect first frame; avoids showing misleading "delta
     since program start" on the first render. *)
  let last_minor_words = ref None in
  let last_major_words = ref None in
  fun (screen : Screen.t) ->
    let metrics = Screen.last_metrics screen in
    Avg_ring.push frame_ring metrics.frame_time_ms;
    Avg_ring.push interval_ring metrics.interval_ms;
    let frame_ms =
      if metrics.interval_ms > 0. then metrics.interval_ms
      else metrics.frame_time_ms
    in
    let fps = if frame_ms > 0. then 1000. /. frame_ms else 0. in
    let base_rows =
      [
        ("frames", string_of_int metrics.frame_count);
        ("fps", Printf.sprintf "%.1f" fps);
        ("frame", fmt_ms frame_ms);
        ("render", fmt_ms metrics.frame_time_ms);
        ("cells", string_of_int metrics.cells);
        ("output", Printf.sprintf "%d bytes" metrics.bytes);
        ("cursor", if metrics.cursor_visible then "visible" else "hidden");
      ]
    in
    let frame_rows = base_rows @ [ ("reset", fmt_ms metrics.reset_ms) ] in
    let perf_rows =
      List.concat
        [
          (match Avg_ring.average frame_ring with
          | Some avg -> [ ("avg render", fmt_ms avg) ]
          | None -> []);
          (match Avg_ring.average interval_ring with
          | Some avg_interval when avg_interval > 0. ->
              [
                ("avg frame", fmt_ms avg_interval);
                ("avg fps", Printf.sprintf "%.1f" (1000. /. avg_interval));
              ]
          | _ -> []);
        ]
    in
    let sections = [ section ~title:"frame" frame_rows ] in
    let sections =
      match perf_rows with
      | [] -> sections
      | _ -> sections @ [ section ~title:"perf" perf_rows ]
    in
    let sections =
      let stats = Gc.quick_stat () in
      (* Compute deltas only if we have a previous sample *)
      let delta_minor_mb, delta_major_mb =
        match (!last_minor_words, !last_major_words) with
        | Some prev_minor, Some prev_major ->
            let dm = max 0. (stats.minor_words -. prev_minor) in
            let dM = max 0. (stats.major_words -. prev_major) in
            (Some (words_to_mb_f dm), Some (words_to_mb_f dM))
        | _ -> (None, None)
      in
      last_minor_words := Some stats.minor_words;
      last_major_words := Some stats.major_words;
      let gc_rows =
        [
          ("heap", fmt_words_mb stats.heap_words);
          ("live", fmt_words_mb stats.live_words);
        ]
        @ List.map
            (fun mb -> ("minor Δ", fmt_mb mb))
            (Option.to_list delta_minor_mb)
        @ List.map
            (fun mb -> ("major Δ", fmt_mb mb))
            (Option.to_list delta_major_mb)
      in
      sections @ [ section ~title:"gc" gc_rows ]
    in
    set_sections overlay sections;
    let grid = Screen.next_grid screen in
    render ~corner overlay grid
