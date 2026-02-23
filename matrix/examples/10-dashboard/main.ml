open Matrix

(* Dashboard: system monitor style UI with progress bars, stats panel, and
   scrolling logs *)

type metric = {
  name : string;
  value : float;
  max_value : float;
  color : Ansi.Color.t;
}

type log_entry = { timestamp : float; level : string; message : string }

type state = {
  metrics : metric list;
  logs : log_entry list;
  start_time : float;
  scroll_offset : int;
  requests_total : int;
  errors_total : int;
  uptime : float;
}

let max_logs = 100

let metric_names =
  [
    ("CPU", Ansi.Color.bright_cyan);
    ("Memory", Ansi.Color.bright_green);
    ("Disk I/O", Ansi.Color.bright_yellow);
    ("Network", Ansi.Color.bright_magenta);
  ]

let log_messages =
  [|
    ("INFO", "System initialized successfully");
    ("DEBUG", "Cache hit ratio: 94.2%");
    ("INFO", "Processing batch job #4521");
    ("WARN", "High memory usage detected");
    ("INFO", "Connection established to server");
    ("DEBUG", "Query executed in 12ms");
    ("INFO", "User session started");
    ("ERROR", "Failed to connect to backup server");
    ("INFO", "Scheduled task completed");
    ("DEBUG", "GC cycle completed, freed 42MB");
    ("INFO", "Configuration reloaded");
    ("WARN", "Disk space below 20%");
    ("INFO", "New worker thread spawned");
    ("DEBUG", "Request latency: 8ms");
    ("INFO", "Health check passed");
  |]

let initial_state () =
  let now = Unix.gettimeofday () in
  {
    metrics =
      List.map
        (fun (name, color) ->
          { name; value = 20.0 +. Random.float 30.0; max_value = 100.0; color })
        metric_names;
    logs = [];
    start_time = now;
    scroll_offset = 0;
    requests_total = 0;
    errors_total = 0;
    uptime = 0.0;
  }

let update_metrics metrics ~dt =
  List.map
    (fun m ->
      let delta = (Random.float 10.0 -. 5.0) *. dt in
      let new_value = max 5.0 (min m.max_value (m.value +. delta)) in
      { m with value = new_value })
    metrics

let maybe_add_log state =
  if Random.float 1.0 < 0.02 then
    let level, message =
      log_messages.(Random.int (Array.length log_messages))
    in
    let entry =
      { timestamp = Unix.gettimeofday () -. state.start_time; level; message }
    in
    let logs = entry :: state.logs in
    let logs =
      if List.length logs > max_logs then
        List.filteri (fun i _ -> i < max_logs) logs
      else logs
    in
    let errors_total =
      if level = "ERROR" then state.errors_total + 1 else state.errors_total
    in
    let requests_total = state.requests_total + 1 in
    { state with logs; errors_total; requests_total }
  else state

let update ~dt state =
  let metrics = update_metrics state.metrics ~dt in
  let uptime = state.uptime +. dt in
  let state = { state with metrics; uptime } in
  maybe_add_log state

let draw_box grid ~x ~y ~width ~height ~title =
  let border_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) () in
  let title_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bold:true ()
  in
  (* Top border *)
  Grid.draw_text ~style:border_style grid ~x ~y ~text:"┌";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i) ~y ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y ~text:"┐";
  (* Title *)
  if String.length title > 0 then (
    let title_x = x + 2 in
    Grid.draw_text ~style:border_style grid ~x:title_x ~y ~text:" ";
    Grid.draw_text ~style:title_style grid ~x:(title_x + 1) ~y ~text:title;
    Grid.draw_text ~style:border_style grid
      ~x:(title_x + 1 + String.length title)
      ~y ~text:" ");
  (* Sides *)
  for row = 1 to height - 2 do
    Grid.draw_text ~style:border_style grid ~x ~y:(y + row) ~text:"│";
    Grid.draw_text ~style:border_style grid
      ~x:(x + width - 1)
      ~y:(y + row) ~text:"│"
  done;
  (* Bottom border *)
  Grid.draw_text ~style:border_style grid ~x ~y:(y + height - 1) ~text:"└";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i)
      ~y:(y + height - 1)
      ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid
    ~x:(x + width - 1)
    ~y:(y + height - 1)
    ~text:"┘"

let draw_header grid ~cols =
  let title = "System Dashboard" in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true () in
  Grid.draw_text ~style:title_style grid
    ~x:((cols - String.length title) / 2)
    ~y:0 ~text:title;
  let help = "↑/↓: scroll | Q: quit" in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  Grid.draw_text ~style:help_style grid
    ~x:(cols - String.length help - 2)
    ~y:0 ~text:help

let draw_progress_bar grid ~x ~y ~width ~value ~max_value ~color ~label =
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:color ~bold:true () in
  let bar_width = width - 18 in
  let bar_width = max 5 bar_width in
  let filled = int_of_float (value /. max_value *. float_of_int bar_width) in
  let filled = max 0 (min bar_width filled) in
  (* Draw label *)
  Grid.draw_text ~style:label_style grid ~x ~y
    ~text:(Printf.sprintf "%-8s" label);
  (* Draw bar background *)
  let bg_style = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
  for i = 0 to bar_width - 1 do
    Grid.draw_text ~style:bg_style grid ~x:(x + 9 + i) ~y ~text:" "
  done;
  (* Draw filled portion *)
  let fill_style = Ansi.Style.make ~bg:color () in
  for i = 0 to filled - 1 do
    Grid.draw_text ~style:fill_style grid ~x:(x + 9 + i) ~y ~text:" "
  done;
  (* Draw percentage *)
  let pct = value /. max_value *. 100.0 in
  Grid.draw_text ~style:value_style grid
    ~x:(x + 10 + bar_width)
    ~y
    ~text:(Printf.sprintf "%5.1f%%" pct)

let draw_metrics grid ~x ~y ~width state =
  List.iteri
    (fun i m ->
      draw_progress_bar grid ~x:(x + 1)
        ~y:(y + 1 + i)
        ~width:(width - 2) ~value:m.value ~max_value:m.max_value ~color:m.color
        ~label:m.name)
    state.metrics

let format_uptime seconds =
  let hours = int_of_float seconds / 3600 in
  let minutes = int_of_float seconds / 60 mod 60 in
  let secs = int_of_float seconds mod 60 in
  Printf.sprintf "%02d:%02d:%02d" hours minutes secs

let draw_stats_panel grid ~x ~y ~width ~height state =
  draw_box grid ~x ~y ~width ~height ~title:"Statistics";
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true () in
  let error_style = Ansi.Style.make ~fg:Ansi.Color.bright_red ~bold:true () in
  let ok_style = Ansi.Style.make ~fg:Ansi.Color.bright_green ~bold:true () in
  (* Uptime *)
  Grid.draw_text ~style:label_style grid ~x:(x + 2) ~y:(y + 2) ~text:"Uptime:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 2)
    ~text:(format_uptime state.uptime);
  (* Requests *)
  Grid.draw_text ~style:label_style grid ~x:(x + 2) ~y:(y + 4) ~text:"Requests:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 4)
    ~text:(string_of_int state.requests_total);
  (* Errors *)
  Grid.draw_text ~style:label_style grid ~x:(x + 2) ~y:(y + 6) ~text:"Errors:";
  let err_style = if state.errors_total > 0 then error_style else ok_style in
  Grid.draw_text ~style:err_style grid ~x:(x + 12) ~y:(y + 6)
    ~text:(string_of_int state.errors_total);
  (* Status *)
  Grid.draw_text ~style:label_style grid ~x:(x + 2) ~y:(y + 8) ~text:"Status:";
  let status, status_style =
    let cpu = (List.hd state.metrics).value in
    if cpu > 80.0 then ("HIGH LOAD", error_style)
    else if cpu > 60.0 then
      ("MODERATE", Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bold:true ())
    else ("HEALTHY", ok_style)
  in
  Grid.draw_text ~style:status_style grid ~x:(x + 12) ~y:(y + 8) ~text:status

let draw_logs grid ~x ~y ~width ~height state =
  draw_box grid ~x ~y ~width ~height ~title:"Activity Log";
  let time_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  let visible_logs = height - 2 in
  let total_logs = List.length state.logs in
  (* Draw scroll indicator in title area *)
  (if total_logs > 0 then
     let scroll_info =
       Printf.sprintf "[%d/%d]"
         (min total_logs (state.scroll_offset + 1))
         total_logs
     in
     let scroll_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
     Grid.draw_text ~style:scroll_style grid
       ~x:(x + width - String.length scroll_info - 3)
       ~y ~text:scroll_info);
  let logs_to_show =
    state.logs
    |> List.filteri (fun i _ -> i >= state.scroll_offset)
    |> List.filteri (fun i _ -> i < visible_logs)
  in
  List.iteri
    (fun i entry ->
      let row = y + 1 + i in
      if row < y + height - 1 then (
        (* Timestamp *)
        let time_str = Printf.sprintf "[%5.0fs]" entry.timestamp in
        Grid.draw_text ~style:time_style grid ~x:(x + 1) ~y:row ~text:time_str;
        (* Level with color *)
        let level_color, level_str =
          match entry.level with
          | "ERROR" -> (Ansi.Color.bright_red, "ERR")
          | "WARN" -> (Ansi.Color.bright_yellow, "WRN")
          | "INFO" -> (Ansi.Color.bright_green, "INF")
          | "DEBUG" -> (Ansi.Color.bright_cyan, "DBG")
          | _ -> (Ansi.Color.white, entry.level)
        in
        let level_style = Ansi.Style.make ~fg:level_color ~bold:true () in
        Grid.draw_text ~style:level_style grid ~x:(x + 9) ~y:row ~text:level_str;
        (* Message *)
        let msg_style = Ansi.Style.make ~fg:Ansi.Color.white () in
        let max_msg_len = width - 16 in
        let msg =
          if String.length entry.message > max_msg_len then
            String.sub entry.message 0 (max max_msg_len 0 - 3) ^ "..."
          else entry.message
        in
        Grid.draw_text ~style:msg_style grid ~x:(x + 13) ~y:row ~text:msg))
    logs_to_show

let () =
  Random.self_init ();
  let app =
    Matrix.create ~target_fps:(Some 30.) ~mouse_enabled:false
      ~debug_overlay:false ()
  in
  let state = ref (initial_state ()) in
  Matrix.run app
    ~on_frame:(fun _ ~dt -> state := update ~dt !state)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; _ }
        when Uchar.to_int u = Char.code 'q' || Uchar.to_int u = Char.code 'Q' ->
          Matrix.stop app
      | Input.Key { key = Input.Key.Up; _ } ->
          state :=
            { !state with scroll_offset = max 0 (!state.scroll_offset - 1) }
      | Input.Key { key = Input.Key.Down; _ } ->
          let max_scroll = max 0 (List.length !state.logs - 10) in
          state :=
            {
              !state with
              scroll_offset = min max_scroll (!state.scroll_offset + 1);
            }
      | Input.Key { key = Input.Key.Page_up; _ } ->
          state :=
            { !state with scroll_offset = max 0 (!state.scroll_offset - 10) }
      | Input.Key { key = Input.Key.Page_down; _ } ->
          let max_scroll = max 0 (List.length !state.logs - 10) in
          state :=
            {
              !state with
              scroll_offset = min max_scroll (!state.scroll_offset + 10);
            }
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      draw_header grid ~cols;
      (* Layout: left panel (metrics + logs), right panel (stats) *)
      let right_panel_width = 26 in
      let left_width = cols - right_panel_width - 1 in
      let metrics_height = 6 in
      (* Left side: Metrics box *)
      draw_box grid ~x:0 ~y:1 ~width:left_width ~height:metrics_height
        ~title:"System Metrics";
      draw_metrics grid ~x:0 ~y:1 ~width:left_width !state;
      (* Left side: Logs box *)
      let logs_y = 1 + metrics_height in
      let logs_height = rows - logs_y in
      draw_logs grid ~x:0 ~y:logs_y ~width:left_width ~height:logs_height !state;
      (* Right side: Stats panel *)
      let stats_height = 11 in
      draw_stats_panel grid ~x:left_width ~y:1 ~width:right_panel_width
        ~height:stats_height !state)
