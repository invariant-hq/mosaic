open Matrix

(* Keyboard tester: visualize all key events with modifiers and Kitty
   metadata *)

type event_record = { event : Input.t; timestamp : float }

let max_history = 20
let history : event_record list ref = ref []

let add_event event =
  let record = { event; timestamp = Unix.gettimeofday () } in
  history := record :: List.filteri (fun i _ -> i < max_history - 1) !history

let event_to_string event = Format.asprintf "%a" Input.pp event

let draw_header grid ~cols =
  let title = "Keyboard & Input Tester" in
  let subtitle = "Press keys, move mouse, or paste text. Escape to quit." in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  let sub_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  Grid.draw_text ~style:title_style grid ~x:2 ~y:1 ~text:title;
  Grid.draw_text ~style:sub_style grid ~x:2 ~y:2 ~text:subtitle;
  (* Draw separator *)
  let sep = String.make (cols - 4) '-' in
  Grid.draw_text ~style:sub_style grid ~x:2 ~y:3 ~text:sep

let draw_modifier_panel grid ~y =
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let on_style =
    Ansi.Style.make ~fg:Ansi.Color.black ~bg:Ansi.Color.bright_green ~bold:true
      ()
  in
  let off_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_black
      ~bg:(Ansi.Color.grayscale ~level:5)
      ()
  in
  Grid.draw_text ~style:label_style grid ~x:2 ~y ~text:"Modifiers: ";
  let modifiers =
    [ "Ctrl"; "Alt"; "Shift"; "Super"; "Hyper"; "Meta"; "CapsLk"; "NumLk" ]
  in
  let x = ref 13 in
  (* Check current modifier state from most recent key event *)
  let current_mods =
    match !history with
    | { event = Key ke; _ } :: _ -> Some ke.modifier
    | _ -> None
  in
  List.iter
    (fun name ->
      let is_on =
        match current_mods with
        | None -> false
        | Some m -> (
            match name with
            | "Ctrl" -> m.ctrl
            | "Alt" -> m.alt
            | "Shift" -> m.shift
            | "Super" -> m.super
            | "Hyper" -> m.hyper
            | "Meta" -> m.meta
            | "CapsLk" -> m.caps_lock
            | "NumLk" -> m.num_lock
            | _ -> false)
      in
      let style = if is_on then on_style else off_style in
      let text = Printf.sprintf " %s " name in
      Grid.draw_text ~style grid ~x:!x ~y ~text;
      x := !x + String.length text + 1)
    modifiers

let draw_event_history grid ~y ~rows =
  let header_style = Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true () in
  Grid.draw_text ~style:header_style grid ~x:2 ~y ~text:"Event History:";
  let available_rows = rows - y - 2 in
  let events_to_show = min available_rows (List.length !history) in
  let event_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let dim_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  List.iteri
    (fun i record ->
      if i < events_to_show then
        let age = Unix.gettimeofday () -. record.timestamp in
        let style = if age < 0.5 then event_style else dim_style in
        let text = event_to_string record.event in
        Grid.draw_text ~style grid ~x:4 ~y:(y + 1 + i) ~text)
    !history

let () =
  let app = Matrix.create ~target_fps:(Some 30.) ~debug_overlay:false () in
  Matrix.run app
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | _ -> add_event event)
    ~on_resize:(fun _ ~cols ~rows -> add_event (Input.Resize (cols, rows)))
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      draw_header grid ~cols;
      draw_modifier_panel grid ~y:5;
      draw_event_history grid ~y:7 ~rows)
