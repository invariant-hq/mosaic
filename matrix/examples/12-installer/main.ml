open Matrix

(* Installer: package manager simulation demonstrating static_print *)

type package = { name : string; version : string; size : int (* KB *) }

type package_state =
  | Pending
  | Downloading of float (* progress 0.0 - 1.0 *)
  | Installing of float
  | Done

type state = {
  packages : (package * package_state) list;
  current_idx : int;
  elapsed : float;
}

let packages_to_install =
  [
    { name = "base"; version = "5.0.0"; size = 1200 };
    { name = "dune"; version = "3.14.0"; size = 850 };
    { name = "ppx_deriving"; version = "5.2.1"; size = 320 };
    { name = "lwt"; version = "5.7.0"; size = 680 };
    { name = "cmdliner"; version = "1.2.0"; size = 180 };
    { name = "fmt"; version = "0.9.0"; size = 95 };
    { name = "logs"; version = "0.7.0"; size = 72 };
    { name = "yojson"; version = "2.1.0"; size = 245 };
  ]

let initial_state =
  {
    packages = List.map (fun p -> (p, Pending)) packages_to_install;
    current_idx = 0;
    elapsed = 0.0;
  }

let download_speed = 2.5 (* progress per second *)
let install_speed = 3.0

let update_package_state pkg_state dt =
  match pkg_state with
  | Pending -> Downloading 0.0
  | Downloading p ->
      let new_p = p +. (download_speed *. dt) in
      if new_p >= 1.0 then Installing 0.0 else Downloading new_p
  | Installing p ->
      let new_p = p +. (install_speed *. dt) in
      if new_p >= 1.0 then Done else Installing new_p
  | Done -> Done

let update ~dt state =
  let elapsed = state.elapsed +. dt in
  let packages =
    List.mapi
      (fun i (pkg, pkg_state) ->
        if i = state.current_idx then (pkg, update_package_state pkg_state dt)
        else (pkg, pkg_state))
      state.packages
  in
  (* Check if current package is done, move to next *)
  let current_done =
    match List.nth_opt packages state.current_idx with
    | Some (_, Done) -> true
    | _ -> false
  in
  let current_idx =
    if current_done && state.current_idx < List.length packages - 1 then
      state.current_idx + 1
    else state.current_idx
  in
  { packages; current_idx; elapsed }

let all_done state = List.for_all (fun (_, s) -> s = Done) state.packages

let render grid state =
  let cols = Grid.width grid in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let progress_style = Ansi.Style.make ~fg:Ansi.Color.bright_green () in
  let pending_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  let done_style = Ansi.Style.make ~fg:Ansi.Color.green () in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  (* Title *)
  Grid.draw_text ~style:title_style grid ~x:0 ~y:0 ~text:"Package Installer";
  (* Count completed *)
  let completed =
    List.length (List.filter (fun (_, s) -> s = Done) state.packages)
  in
  let total = List.length state.packages in
  let status =
    if all_done state then "Installation complete!"
    else Printf.sprintf "Installing packages... (%d/%d)" completed total
  in
  Grid.draw_text ~style:label_style grid ~x:0 ~y:1 ~text:status;
  (* Draw each package *)
  List.iteri
    (fun i (pkg, pkg_state) ->
      let y = 3 + i in
      let name_text = Printf.sprintf "%-16s %s" pkg.name pkg.version in
      let status_x = max 25 (2 + String.length name_text + 1) in
      let style =
        match pkg_state with
        | Pending -> pending_style
        | Downloading _ | Installing _ -> progress_style
        | Done -> done_style
      in
      Grid.draw_text ~style grid ~x:2 ~y ~text:name_text;
      (* Progress bar for active package *)
      match pkg_state with
      | Downloading p ->
          let bar_width = max 0 (min 30 (cols - (status_x + 10))) in
          let filled = int_of_float (p *. float_of_int bar_width) in
          let bar_bg = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
          let bar_fg = Ansi.Style.make ~bg:Ansi.Color.bright_blue () in
          for j = 0 to bar_width - 1 do
            let bar_style = if j < filled then bar_fg else bar_bg in
            Grid.draw_text ~style:bar_style grid ~x:(status_x + j) ~y ~text:" "
          done;
          let pct = Printf.sprintf " %3.0f%% DL" (p *. 100.) in
          Grid.draw_text ~style:progress_style grid
            ~x:(status_x + 1 + bar_width)
            ~y ~text:pct
      | Installing p ->
          let bar_width = max 0 (min 30 (cols - (status_x + 10))) in
          let filled = int_of_float (p *. float_of_int bar_width) in
          let bar_bg = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
          let bar_fg = Ansi.Style.make ~bg:Ansi.Color.bright_green () in
          for j = 0 to bar_width - 1 do
            let bar_style = if j < filled then bar_fg else bar_bg in
            Grid.draw_text ~style:bar_style grid ~x:(status_x + j) ~y ~text:" "
          done;
          let pct = Printf.sprintf " %3.0f%% inst" (p *. 100.) in
          Grid.draw_text ~style:progress_style grid
            ~x:(status_x + 1 + bar_width)
            ~y ~text:pct
      | Done ->
          Grid.draw_text ~style:done_style grid ~x:status_x ~y
            ~text:"✓ installed"
      | Pending ->
          Grid.draw_text ~style:pending_style grid ~x:status_x ~y
            ~text:"waiting...")
    state.packages;
  (* Write completed packages to static output *)
  let newly_done =
    List.filter_map
      (fun (pkg, pkg_state) ->
        match pkg_state with Done -> Some pkg | _ -> None)
      state.packages
  in
  if completed > 0 && not (all_done state) then
    (* We only write once per package completion - track this properly *)
    ();
  (* Help text *)
  let help_y = 3 + List.length state.packages + 1 in
  Grid.draw_text ~style:help_style grid ~x:0 ~y:help_y ~text:"Press Q to quit";
  (* Show elapsed time *)
  let time_text = Printf.sprintf "Elapsed: %.1fs" state.elapsed in
  Grid.draw_text ~style:help_style grid ~x:0 ~y:(help_y + 1) ~text:time_text;
  ignore newly_done

let () =
  let app =
    Matrix.create ~mode:`Primary ~target_fps:(Some 30.) ~mouse_enabled:false
      ~debug_overlay:false ()
  in
  let state = ref initial_state in
  let last_completed = ref 0 in
  let completed_at = ref None in
  Matrix.run app
    ~on_frame:(fun app ~dt ->
      let prev_state = !state in
      state := update ~dt !state;
      (* Write static output when a package completes *)
      let prev_completed =
        List.length (List.filter (fun (_, s) -> s = Done) prev_state.packages)
      in
      let curr_completed =
        List.length (List.filter (fun (_, s) -> s = Done) !state.packages)
      in
      if curr_completed > prev_completed && curr_completed > !last_completed
      then (
        let just_completed =
          List.filter_map
            (fun ((pkg, s), (_, prev_s)) ->
              if s = Done && prev_s <> Done then Some pkg else None)
            (List.combine !state.packages prev_state.packages)
        in
        List.iter
          (fun pkg ->
            let msg =
              Printf.sprintf "✓ %s %s installed (%dKB)" pkg.name pkg.version
                pkg.size
            in
            Matrix.static_print app msg)
          just_completed;
        last_completed := curr_completed);
      (* Stop when all done after a brief delay *)
      if all_done !state then (
        if Option.is_none !completed_at then completed_at := Some !state.elapsed;
        match !completed_at with
        | Some done_at when !state.elapsed -. done_at >= 1.0 -> Matrix.stop app
        | _ -> ())
      else completed_at := None)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; _ }
        when Uchar.to_int u = Char.code 'q' || Uchar.to_int u = Char.code 'Q' ->
          Matrix.stop app
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let rows_needed = 6 + List.length !state.packages in
      Grid.resize grid ~width:(Grid.width grid) ~height:rows_needed;
      Grid.clear grid;
      render grid !state)
