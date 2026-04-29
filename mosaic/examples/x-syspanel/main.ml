(** System metrics panel UI. Uses Sysstat library for data collection. *)

module Cell_grid = Grid
open Mosaic
module Charts = Matrix_charts

(* ---------- Model ---------- *)
type model = {
  cpu : Sysstat.Cpu.stats;
  cpu_per_core : Sysstat.Cpu.stats array;
  memory : Sysstat.Mem.t;
  disk : Sysstat.Fs.t;
  network : Sysstat.Net.stats;
  process : Sysstat.Proc.Self.stats;
  processes : Sysstat.Proc.Table.stats list;
  cpu_prev : Sysstat.Cpu.t;
  cpu_per_core_prev : Sysstat.Cpu.t array;
  net_prev : Sysstat.Net.t;
  proc_self_prev : Sysstat.Proc.Self.t;
  proc_list_prev : Sysstat.Proc.Table.t list;
  sample_acc : float;
  sparkline_cpu : Charts.Sparkline.t;
  sparkline_memory : Charts.Sparkline.t;
  sparkline_disk : Charts.Sparkline.t;
}

(* Helper functions for display formatting *)
let bytes_to_gb b = Int64.to_float b /. (1024. *. 1024. *. 1024.)
let bytes_to_mb b = Int64.to_float b /. (1024. *. 1024.)

let disk_used_percent (disk : Sysstat.Fs.t) =
  if disk.total_bytes > 0L then
    Int64.to_float disk.used_bytes /. Int64.to_float disk.total_bytes *. 100.
  else 0.0

let partition_used_percent (p : Sysstat.Fs.partition) =
  if p.total_bytes > 0L then
    Int64.to_float p.used_bytes /. Int64.to_float p.total_bytes *. 100.
  else 0.0

let mem_total_gb (m : Sysstat.Mem.t) = bytes_to_gb m.total
let mem_used_gb (m : Sysstat.Mem.t) = bytes_to_gb m.used

let mem_used_percent (m : Sysstat.Mem.t) =
  if m.total > 0L then Int64.to_float m.used /. Int64.to_float m.total *. 100.
  else 0.0

let mem_swap_used_gb (m : Sysstat.Mem.t) = bytes_to_gb m.swap_used

let mem_swap_used_percent (m : Sysstat.Mem.t) =
  if m.swap_total > 0L then
    Int64.to_float m.swap_used /. Int64.to_float m.swap_total *. 100.
  else 0.0

(* Network I/O formatting helpers *)
let bytes_per_sec_to_mbps bps = bps /. (1024. *. 1024.)

type msg = Quit | Tick of float

(* ---------- Init ---------- *)
let init () =
  let sparkline_cpu =
    Charts.Sparkline.create
      ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  let sparkline_memory =
    Charts.Sparkline.create
      ~style:(Ansi.Style.make ~fg:Ansi.Color.magenta ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  let sparkline_disk =
    Charts.Sparkline.create
      ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  (* Get initial CPU sample *)
  let cpu_prev = Sysstat.Cpu.sample () in
  let cpu_per_core_prev = Sysstat.Cpu.sample_per_core () in
  (* Wait a tiny bit and sample again for initial delta *)
  Unix.sleepf 0.1;
  let cpu_next = Sysstat.Cpu.sample () in
  let cpu_per_core_next = Sysstat.Cpu.sample_per_core () in
  let cpu = Sysstat.Cpu.compute ~prev:cpu_prev ~next:cpu_next in
  let cpu_per_core =
    Array.map2
      (fun p n -> Sysstat.Cpu.compute ~prev:p ~next:n)
      cpu_per_core_prev cpu_per_core_next
  in
  let cpu_prev = cpu_next in
  let cpu_per_core_prev = cpu_per_core_next in
  let memory = Sysstat.Mem.sample () in
  let disk = Sysstat.Fs.sample () in
  (* Get initial network sample *)
  let net_prev = Sysstat.Net.sample () in
  let network =
    (* Initial network stats are zero (no delta yet) *)
    {
      Sysstat.Net.rx_bytes_per_sec = 0.0;
      rx_packets_per_sec = 0.0;
      tx_bytes_per_sec = 0.0;
      tx_packets_per_sec = 0.0;
    }
  in
  (* Get initial process sample *)
  let proc_self_prev = Sysstat.Proc.Self.sample () in
  let process =
    { Sysstat.Proc.Self.cpu_percent = 0.0; rss_bytes = 0L; vsize_bytes = 0L }
  in
  (* Push initial values to sparklines *)
  let total_cpu = cpu.user +. cpu.system in
  let mem_used_pct =
    if memory.total > 0L then
      Int64.to_float memory.used /. Int64.to_float memory.total *. 100.
    else 0.0
  in
  Charts.Sparkline.push sparkline_cpu total_cpu;
  Charts.Sparkline.push sparkline_memory mem_used_pct;
  Charts.Sparkline.push sparkline_disk (disk_used_percent disk);
  (* Sample initial process list for native CPU delta calculation *)
  let proc_list_prev = Sysstat.Proc.Table.sample () in
  ( {
      cpu;
      cpu_per_core;
      memory;
      disk;
      network;
      process;
      processes = [];
      (* Will be populated on first tick *)
      cpu_prev;
      cpu_per_core_prev;
      net_prev;
      proc_self_prev;
      proc_list_prev;
      sample_acc = 0.0;
      sparkline_cpu;
      sparkline_memory;
      sparkline_disk;
    },
    Cmd.none )

(* ---------- Update ---------- *)
let update msg m =
  match msg with
  | Quit -> (m, Cmd.quit)
  | Tick dt ->
      (* Sample at ~5Hz (every 0.2s) *)
      let sample_acc = m.sample_acc +. dt in
      if sample_acc < 0.2 then ({ m with sample_acc }, Cmd.none)
      else
        (* Update CPU, Memory, Disk, and Process *)
        let cpu_next = Sysstat.Cpu.sample () in
        let cpu_per_core_next = Sysstat.Cpu.sample_per_core () in
        let cpu = Sysstat.Cpu.compute ~prev:m.cpu_prev ~next:cpu_next in
        let cpu_per_core =
          Array.map2
            (fun p n -> Sysstat.Cpu.compute ~prev:p ~next:n)
            m.cpu_per_core_prev cpu_per_core_next
        in
        let cpu_prev = cpu_next in
        let cpu_per_core_prev = cpu_per_core_next in
        let memory = Sysstat.Mem.sample () in
        let disk = Sysstat.Fs.sample () in
        (* Update network metrics *)
        let net_next = Sysstat.Net.sample () in
        let network =
          Sysstat.Net.compute ~prev:m.net_prev ~next:net_next ~dt:sample_acc
        in
        let net_prev = net_next in
        (* Update process metrics *)
        let proc_self_next = Sysstat.Proc.Self.sample () in
        (* Get number of CPU cores for normalization *)
        let num_cores = Array.length m.cpu_per_core in
        let process =
          Sysstat.Proc.Self.compute ~prev:m.proc_self_prev ~next:proc_self_next
            ~dt:sample_acc
            ~num_cores:(if num_cores > 0 then Some num_cores else None)
        in
        let proc_self_prev = proc_self_next in
        (* Update process list using native API *)
        let proc_list_next = Sysstat.Proc.Table.sample () in
        let processes =
          Sysstat.Proc.Table.compute ~prev:m.proc_list_prev ~next:proc_list_next
            ~dt:sample_acc
          |> List.sort
               (fun
                 (a : Sysstat.Proc.Table.stats)
                 (b : Sysstat.Proc.Table.stats)
               -> compare b.cpu_percent a.cpu_percent)
          |> fun l -> List.filteri (fun i _ -> i < 10) l
        in
        (* Push values to sparklines *)
        let total_cpu = cpu.user +. cpu.system in
        let mem_used_pct =
          if memory.total > 0L then
            Int64.to_float memory.used /. Int64.to_float memory.total *. 100.
          else 0.0
        in
        Charts.Sparkline.push m.sparkline_cpu total_cpu;
        Charts.Sparkline.push m.sparkline_memory mem_used_pct;
        Charts.Sparkline.push m.sparkline_disk (disk_used_percent disk);
        ( {
            cpu;
            cpu_per_core;
            memory;
            disk;
            network;
            process;
            processes;
            cpu_prev;
            cpu_per_core_prev;
            net_prev;
            proc_self_prev;
            proc_list_prev = proc_list_next;
            sample_acc = 0.0;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
            sparkline_disk = m.sparkline_disk;
          },
          Cmd.none )

(* ---------- View ---------- *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Helper function to draw a horizontal progress bar *)
let draw_progress_bar grid ~width ~height ~value ~max_value ~fill_color =
  let bar_width = width in
  let bar_height = height in
  if bar_width > 0 && bar_height > 0 then (
    (* Draw background *)
    Cell_grid.fill_rect grid ~x:0 ~y:0 ~width:bar_width ~height:bar_height
      ~color:(Ansi.Color.grayscale ~level:3);
    (* Draw filled portion *)
    let filled_width =
      int_of_float (value /. max_value *. float_of_int bar_width)
    in
    let filled_width = max 0 (min bar_width filled_width) in
    if filled_width > 0 then
      Cell_grid.fill_rect grid ~x:0 ~y:0 ~width:filled_width ~height:bar_height
        ~color:fill_color)

(* ---------- View Components ---------- *)

let view_header () =
  box ~padding:(padding 1) ~background:header_bg
    [
      box ~flex_direction:Row ~justify_content:Space_between ~align_items:Center
        ~size:{ width = pct 100; height = auto }
        [
          text ~style:(Ansi.Style.make ~bold:true ()) "▸ System Panel";
          text ~style:muted "▄▀ mosaic";
        ];
    ]

let view_footer () =
  box ~padding:(padding 1) ~background:footer_bg [ text ~style:hint "q quit" ]

let view_per_core_cpu (cpu_per_core : Sysstat.Cpu.stats array) =
  if Array.length cpu_per_core > 0 then
    box ~flex_direction:Column ~gap:(gap 1)
      ~size:{ width = pct 100; height = auto }
      [
        text ~style:muted "Per-Core Usage:";
        scroll_box ~scroll_y:true ~scroll_x:false
          ~size:{ width = pct 100; height = px 12 }
          (let cores =
             Array.to_list (Array.mapi (fun i stats -> (i, stats)) cpu_per_core)
           in
           (* Group cores into pairs for rows *)
           let rec chunk_pairs = function
             | [] -> []
             | [ x ] -> [ [ x ] ]
             | x :: y :: rest -> [ x; y ] :: chunk_pairs rest
           in
           let rows = chunk_pairs cores in
           List.mapi
             (fun row_idx row ->
               box
                 ~key:(Printf.sprintf "row-%d" row_idx)
                 ~flex_direction:Row ~gap:(gap 1)
                 ~size:{ width = pct 100; height = auto }
                 (List.mapi
                    (fun _col_idx (i, (core_stats : Sysstat.Cpu.stats)) ->
                      let total_usage = core_stats.user +. core_stats.system in
                      let bar_color =
                        if total_usage > 80. then Ansi.Color.red
                        else if total_usage > 50. then Ansi.Color.yellow
                        else Ansi.Color.green
                      in
                      box
                        ~key:(Printf.sprintf "core-%d" i)
                        ~padding:(padding 1)
                        ~background:
                          (if i mod 2 = 0 then Ansi.Color.default
                           else Ansi.Color.grayscale ~level:3)
                        ~size:{ width = pct 50; height = auto }
                        [
                          box ~flex_direction:Column ~gap:(gap 0)
                            ~size:{ width = pct 100; height = auto }
                            [
                              (* Core label and percentage *)
                              box ~flex_direction:Row
                                ~justify_content:Space_between
                                ~align_items:Center
                                ~size:{ width = pct 100; height = auto }
                                [
                                  text
                                    ~style:(Ansi.Style.make ~fg:bar_color ())
                                    (string_of_int i);
                                  text
                                    ~style:
                                      (Ansi.Style.make ~bold:true ~fg:bar_color
                                         ())
                                    (Printf.sprintf "%.1f%%" total_usage);
                                ];
                              (* Progress bar *)
                              canvas
                                ~size:{ width = pct 100; height = px 1 }
                                (fun c ~delta:_ ->
                                  Canvas.clear c;
                                  draw_progress_bar (Canvas.grid c)
                                    ~width:(Canvas.width c)
                                    ~height:(Canvas.height c) ~value:total_usage
                                    ~max_value:100.0 ~fill_color:bar_color);
                            ];
                        ])
                    row))
             rows);
      ]
  else box ~size:{ width = pct 100; height = px 0 } []

let view_cpu_usage (cpu : Sysstat.Cpu.stats)
    (cpu_per_core : Sysstat.Cpu.stats array)
    (sparkline_cpu : Charts.Sparkline.t) =
  let is_linux = Sys.file_exists "/proc" in
  box ~border:true ~padding:(padding 1) ~title:"CPU Usage"
    ~background:Ansi.Color.default
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Column ~gap:(gap 0)
        ~size:{ width = pct 100; height = auto }
        [
          (* Top: Overall CPU Metrics and Graph *)
          box ~flex_direction:Row ~gap:(gap 3)
            ~size:{ width = pct 100; height = auto }
            [
              (* Left: CPU Metrics *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = pct 55; height = auto }
                [
                  (* User CPU *)
                  box ~flex_direction:Row ~justify_content:Space_between
                    ~align_items:Center
                    [
                      text ~style:muted "User:";
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
                        (Printf.sprintf "%.1f%%" cpu.user);
                    ];
                  (* Nice CPU *)
                  (if cpu.nice >= 0.00 then
                     box ~flex_direction:Row ~justify_content:Space_between
                       ~align_items:Center
                       [
                         text ~style:muted "Nice:";
                         text
                           ~style:
                             (Ansi.Style.make ~bold:true ~fg:Ansi.Color.blue ())
                           (Printf.sprintf "%.1f%%" cpu.nice);
                       ]
                   else box ~size:{ width = pct 100; height = px 0 } []);
                  (* System CPU *)
                  box ~flex_direction:Row ~justify_content:Space_between
                    ~align_items:Center
                    [
                      text ~style:muted "System:";
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                        (Printf.sprintf "%.1f%%" cpu.system);
                    ];
                  (* Linux-only fields *)
                  (if is_linux then
                     box ~flex_direction:Column ~gap:(gap 1)
                       ~size:{ width = pct 100; height = auto }
                       [
                         (* IOWait CPU *)
                         (if cpu.iowait > 0.01 then
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              [
                                text ~style:muted "IOWait:";
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.yellow ())
                                  (Printf.sprintf "%.1f%%" cpu.iowait);
                              ]
                          else box ~size:{ width = pct 100; height = px 0 } []);
                         (* IRQ CPU *)
                         (if cpu.irq > 0.01 then
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              [
                                text ~style:muted "IRQ:";
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.red ())
                                  (Printf.sprintf "%.1f%%" cpu.irq);
                              ]
                          else box ~size:{ width = pct 100; height = px 0 } []);
                         (* SoftIRQ CPU *)
                         (if cpu.softirq > 0.01 then
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              [
                                text ~style:muted "SoftIRQ:";
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.red ())
                                  (Printf.sprintf "%.1f%%" cpu.softirq);
                              ]
                          else box ~size:{ width = pct 100; height = px 0 } []);
                         (* Steal CPU *)
                         (if cpu.steal > 0.01 then
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              [
                                text ~style:muted "Steal:";
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.yellow ())
                                  (Printf.sprintf "%.1f%%" cpu.steal);
                              ]
                          else box ~size:{ width = pct 100; height = px 0 } []);
                         (* Guest CPU *)
                         (if cpu.guest > 0.01 then
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              [
                                text ~style:muted "Guest:";
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.cyan ())
                                  (Printf.sprintf "%.1f%%" cpu.guest);
                              ]
                          else box ~size:{ width = pct 100; height = px 0 } []);
                       ]
                   else box ~size:{ width = pct 100; height = px 0 } []);
                  (* Idle CPU *)
                  box ~flex_direction:Row ~justify_content:Space_between
                    ~align_items:Center
                    [
                      text ~style:muted "Idle:";
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                        (Printf.sprintf "%.1f%%" cpu.idle);
                    ];
                ];
              (* Right: CPU Usage Graph *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = pct 45; height = auto }
                [
                  text ~style:muted "CPU Load:";
                  canvas
                    ~size:{ width = pct 100; height = px 8 }
                    (fun c ~delta:_ ->
                      Canvas.clear c;
                      Charts.Sparkline.draw sparkline_cpu ~kind:`Braille
                        (Canvas.grid c) ~width:(Canvas.width c)
                        ~height:(Canvas.height c));
                ];
            ];
          (* Bottom: Per-Core CPU Usage *)
          view_per_core_cpu cpu_per_core;
        ];
    ]

let view_top_processes (processes : Sysstat.Proc.Table.stats list) =
  box ~border:true ~padding:(padding 1) ~title:"Top Processes"
    ~size:{ width = pct 100; height = auto }
    [
      (if List.length processes > 0 then
         scroll_box ~scroll_y:true ~scroll_x:false
           ~size:{ width = pct 100; height = px 10 }
           (List.mapi
              (fun i (proc : Sysstat.Proc.Table.stats) ->
                box
                  ~key:(Printf.sprintf "process-%d" proc.pid)
                  ~padding:(padding 1)
                  ~background:
                    (if i mod 2 = 0 then Ansi.Color.default
                     else Ansi.Color.grayscale ~level:3)
                  [
                    box ~flex_direction:Row ~justify_content:Space_between
                      ~align_items:Center
                      ~size:{ width = pct 100; height = auto }
                      [
                        (* Left: Process name and PID *)
                        box ~flex_direction:Column ~gap:(gap 0)
                          ~size:{ width = pct 50; height = auto }
                          [
                            (* Process name with potential wrapping *)
                            box
                              ~size:{ width = pct 100; height = auto }
                              [
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.white ())
                                  proc.name;
                              ];
                            text ~style:muted
                              (Printf.sprintf "PID: %d" proc.pid);
                          ];
                        (* Right: CPU usage *)
                        text
                          ~style:
                            (Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
                          (Printf.sprintf "%.1f%%" proc.cpu_percent);
                      ];
                  ])
              processes)
       else
         box ~flex_direction:Row ~justify_content:Center ~align_items:Center
           ~padding:(padding 1)
           [ text ~style:muted "no processes found" ]);
    ]

let view_memory_usage (memory : Sysstat.Mem.t)
    (sparkline_memory : Charts.Sparkline.t) =
  let is_linux = Sys.file_exists "/proc" in
  box ~border:true ~padding:(padding 1) ~title:"Memory Usage"
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Row ~gap:(gap 3)
        ~size:{ width = pct 100; height = pct 100 }
        [
          (* Left: Memory Metrics *)
          box ~flex_direction:Column ~gap:(gap 1)
            ~size:{ width = pct 70; height = auto }
            [
              (* Total Memory *)
              box ~flex_direction:Row ~justify_content:Space_between
                ~align_items:Center
                [
                  text ~style:muted "Total:";
                  text
                    ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                    (Printf.sprintf "%.1f GB" (mem_total_gb memory));
                ];
              (* Used Memory with inline percentage *)
              box ~flex_direction:Row ~justify_content:Space_between
                ~align_items:Center
                [
                  text ~style:muted "Used:";
                  box ~flex_direction:Row ~gap:(gap 0) ~align_items:Center
                    [
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                        (Printf.sprintf "%.1f GB" (mem_used_gb memory));
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                        (Printf.sprintf " (%.1f%%)" (mem_used_percent memory));
                    ];
                ];
              (* Free Memory - common on both platforms *)
              (if memory.free > 0L then
                 box ~flex_direction:Row ~justify_content:Space_between
                   ~align_items:Center
                   [
                     text ~style:muted "Free:";
                     text
                       ~style:
                         (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                       (Printf.sprintf "%.1f GB" (bytes_to_gb memory.free));
                   ]
               else box ~size:{ width = pct 100; height = px 0 } []);
              (* Available Memory - common on both platforms *)
              (if memory.available > 0L then
                 box ~flex_direction:Row ~justify_content:Space_between
                   ~align_items:Center
                   [
                     text ~style:muted "Available:";
                     text
                       ~style:
                         (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                       (Printf.sprintf "%.1f GB" (bytes_to_gb memory.available));
                   ]
               else box ~size:{ width = pct 100; height = px 0 } []);
              (* Platform-specific fields *)
              (if is_linux then
                 (* Linux-specific fields *)
                 box ~flex_direction:Column ~gap:(gap 1)
                   ~size:{ width = pct 100; height = auto }
                   [
                     (* Cached (inactive on Linux) *)
                     (if memory.inactive > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Cached:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true
                                   ~fg:Ansi.Color.yellow ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.inactive));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                     (* Buffers (purgeable on Linux) *)
                     (if memory.purgeable > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Buffers:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true ~fg:Ansi.Color.blue
                                   ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.purgeable));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                   ]
               else
                 (* macOS-specific fields *)
                 box ~flex_direction:Column ~gap:(gap 1)
                   ~size:{ width = pct 100; height = auto }
                   [
                     (* Active Memory *)
                     (if memory.active > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Active:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green
                                   ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.active));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                     (* Inactive Memory *)
                     (if memory.inactive > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Inactive:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true
                                   ~fg:Ansi.Color.yellow ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.inactive));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                     (* Wired Memory *)
                     (if memory.wired > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Wired:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true ~fg:Ansi.Color.red
                                   ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.wired));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                     (* Compressed Memory *)
                     (if memory.compressed > 0L then
                        box ~flex_direction:Row ~justify_content:Space_between
                          ~align_items:Center
                          [
                            text ~style:muted "Compressed:";
                            text
                              ~style:
                                (Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan
                                   ())
                              (Printf.sprintf "%.1f GB"
                                 (bytes_to_gb memory.compressed));
                          ]
                      else box ~size:{ width = pct 100; height = px 0 } []);
                   ]);
              (* Swap Used with inline percentage *)
              box ~flex_direction:Row ~justify_content:Space_between
                ~align_items:Center
                [
                  text ~style:muted "Swap Used:";
                  box ~flex_direction:Row ~gap:(gap 0) ~align_items:Center
                    [
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                        (Printf.sprintf "%.1f GB" (mem_swap_used_gb memory));
                      text
                        ~style:
                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                        (Printf.sprintf " (%.1f%%)"
                           (mem_swap_used_percent memory));
                    ];
                ];
            ];
          (* Right: Memory Usage Graph *)
          box ~flex_direction:Column ~gap:(gap 1)
            ~size:{ width = pct 30; height = auto }
            [
              text ~style:muted "Memory Load:";
              canvas
                ~size:{ width = pct 100; height = px 4 }
                (fun c ~delta:_ ->
                  Canvas.clear c;
                  Charts.Sparkline.draw sparkline_memory ~kind:`Braille
                    (Canvas.grid c) ~width:(Canvas.width c)
                    ~height:(Canvas.height c));
            ];
        ];
    ]

let view_disk_usage (disk : Sysstat.Fs.t) =
  box ~border:true ~padding:(padding 1) ~title:"Disk Usage"
    ~size:{ width = pct 100; height = auto }
    [
      (* Disk Metrics *)
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [
          (* Total Disk *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "Total:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                (Printf.sprintf "%.1f GB" (bytes_to_gb disk.total_bytes));
            ];
          (* Used Disk *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "Used:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                (Printf.sprintf "%.1f GB" (bytes_to_gb disk.used_bytes));
            ];
          (* Available Disk *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "Avail:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                (Printf.sprintf "%.1f GB" (bytes_to_gb disk.avail_bytes));
            ];
          (* Usage Percentage *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "Usage:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                (Printf.sprintf "%.1f%%" (disk_used_percent disk));
            ];
          (* Partitions *)
          (if List.length disk.partitions > 0 then
             box ~flex_direction:Column ~gap:(gap 1)
               ~size:{ width = pct 100; height = auto }
               [
                 text ~style:muted "Partitions:";
                 scroll_box ~scroll_y:true ~scroll_x:false
                   ~size:{ width = pct 100; height = px 8 }
                   (List.mapi
                      (fun i (part : Sysstat.Fs.partition) ->
                        box
                          ~key:(Printf.sprintf "partition-%d" i)
                          ~padding:(padding 1)
                          ~background:
                            (if i mod 2 = 0 then Ansi.Color.default
                             else Ansi.Color.grayscale ~level:3)
                          [
                            box ~flex_direction:Row
                              ~justify_content:Space_between ~align_items:Center
                              ~size:{ width = pct 100; height = auto }
                              [
                                (* Left: Mount point *)
                                text
                                  ~style:
                                    (Ansi.Style.make ~bold:true
                                       ~fg:Ansi.Color.white ())
                                  part.mount_point;
                                (* Right: Used and percentage *)
                                box ~flex_direction:Row ~gap:(gap 1)
                                  ~align_items:Center
                                  [
                                    text
                                      ~style:
                                        (Ansi.Style.make ~bold:true
                                           ~fg:Ansi.Color.yellow ())
                                      (Printf.sprintf "%.1f GB"
                                         (bytes_to_gb part.used_bytes));
                                    text
                                      ~style:
                                        (Ansi.Style.make ~bold:true
                                           ~fg:Ansi.Color.yellow ())
                                      (Printf.sprintf "(%.1f%%)"
                                         (partition_used_percent part));
                                  ];
                              ];
                          ])
                      disk.partitions);
               ]
           else
             box ~flex_direction:Row ~justify_content:Center ~align_items:Center
               ~padding:(padding 1)
               [ text ~style:muted "no partitions found" ]);
        ];
    ]

let view_network_io (network : Sysstat.Net.stats) =
  box ~border:true ~padding:(padding 1) ~title:"Network I/O"
    ~size:{ width = pct 100; height = auto }
    [
      (* Network Metrics *)
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [
          (* Receive *)
          box ~flex_direction:Column ~gap:(gap 0)
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:muted "Receive:";
              box ~flex_direction:Row ~justify_content:Space_between
                ~align_items:Center
                [
                  text
                    ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                    (Printf.sprintf "%.2f MB/s"
                       (bytes_per_sec_to_mbps network.rx_bytes_per_sec));
                  text ~style:muted
                    (Printf.sprintf "%.0f pkt/s" network.rx_packets_per_sec);
                ];
            ];
          (* Transmit *)
          box ~flex_direction:Column ~gap:(gap 0)
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:muted "Transmit:";
              box ~flex_direction:Row ~justify_content:Space_between
                ~align_items:Center
                [
                  text
                    ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                    (Printf.sprintf "%.2f MB/s"
                       (bytes_per_sec_to_mbps network.tx_bytes_per_sec));
                  text ~style:muted
                    (Printf.sprintf "%.0f pkt/s" network.tx_packets_per_sec);
                ];
            ];
        ];
    ]

let view_process_self (process : Sysstat.Proc.Self.stats) =
  box ~border:true ~padding:(padding 1) ~title:"Process (self)"
    ~size:{ width = pct 100; height = auto }
    [
      (* Process Metrics *)
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [
          (* CPU Usage *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "CPU:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
                (Printf.sprintf "%.1f%%" process.cpu_percent);
            ];
          (* RSS Memory *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            [
              text ~style:muted "RSS:";
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                (Printf.sprintf "%.1f MB" (bytes_to_mb process.rss_bytes));
            ];
          (* Virtual Memory Size *)
          (if process.vsize_bytes > 0L then
             box ~flex_direction:Row ~justify_content:Space_between
               ~align_items:Center
               [
                 text ~style:muted "VSIZE:";
                 text
                   ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.blue ())
                   (Printf.sprintf "%.1f GB" (bytes_to_gb process.vsize_bytes));
               ]
           else box ~size:{ width = pct 100; height = px 0 } []);
        ];
    ]

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      view_header ();
      (* Content - Merged CPU Column Layout *)
      scroll_box ~scroll_y:true ~scroll_x:false ~flex_grow:1.
        ~size:{ width = pct 100; height = pct 100 }
        [
          box ~padding:(padding 1)
            ~size:{ width = pct 100; height = auto }
            [
              box ~flex_direction:Row ~gap:(gap 1)
                ~size:{ width = pct 100; height = auto }
                [
                  (* Left: CPU, Processes, and Network Column *)
                  box ~flex_direction:Column ~gap:(gap 1)
                    ~size:{ width = pct 50; height = auto }
                    [
                      view_cpu_usage model.cpu model.cpu_per_core
                        model.sparkline_cpu;
                      view_top_processes model.processes;
                      view_network_io model.network;
                    ];
                  (* Right: Memory, Disk, and Process Column *)
                  box ~flex_direction:Column ~gap:(gap 1) ~flex_grow:1.
                    ~background:Ansi.Color.default
                    ~size:{ width = pct 50; height = auto }
                    [
                      view_memory_usage model.memory model.sparkline_memory;
                      view_disk_usage model.disk;
                      view_process_self model.process;
                    ];
                ];
            ];
        ];
      (* Footer *)
      view_footer ();
    ]

(* ---------- Subscriptions ---------- *)
let subscriptions _model =
  Sub.batch
    [
      Sub.on_tick (fun ~dt -> Tick dt);
      Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ];
    ]

let () = run { init; update; view; subscriptions }
