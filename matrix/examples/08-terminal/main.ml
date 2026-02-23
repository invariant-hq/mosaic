open Matrix

(* Split terminal: embed a PTY shell using VTE *)

let shell_prog = try Sys.getenv "SHELL" with Not_found -> "/bin/sh"

type state = { mutable pty : Pty.t option; vte : Vte.t; cols : int; rows : int }

let create_state ~cols ~rows =
  let vte = Vte.create ~rows ~cols () in
  let winsize = { Pty.rows; cols; xpixel = 0; ypixel = 0 } in
  let pty =
    try
      let p = Pty.spawn ~prog:shell_prog ~args:[] ~winsize () in
      Pty.set_nonblock p;
      Some p
    with _ -> None
  in
  { pty; vte; cols; rows }

let cleanup state =
  match state.pty with Some pty -> Pty.close pty | None -> ()

let read_pty_output state =
  match state.pty with
  | None -> ()
  | Some pty ->
      let buf = Bytes.create 4096 in
      let rec read_loop () =
        match Pty.read pty buf 0 (Bytes.length buf) with
        | n when n > 0 ->
            Vte.feed state.vte buf 0 n;
            read_loop ()
        | _ -> ()
        | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
        | exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
        | exception Unix.Unix_error (Unix.EIO, _, _) ->
            Pty.close pty;
            state.pty <- None
        | exception _ -> ()
      in
      read_loop ()

let send_key state (ke : Input.Key.event) =
  match state.pty with
  | None -> ()
  | Some pty -> (
      let data =
        match ke.key with
        | Input.Key.Enter -> Some "\r"
        | Input.Key.Tab -> Some "\t"
        | Input.Key.Backspace -> Some "\x7f"
        | Input.Key.Escape -> Some "\x1b"
        | Input.Key.Up -> Some "\x1b[A"
        | Input.Key.Down -> Some "\x1b[B"
        | Input.Key.Right -> Some "\x1b[C"
        | Input.Key.Left -> Some "\x1b[D"
        | Input.Key.Home -> Some "\x1b[H"
        | Input.Key.End -> Some "\x1b[F"
        | Input.Key.Page_up -> Some "\x1b[5~"
        | Input.Key.Page_down -> Some "\x1b[6~"
        | Input.Key.Delete -> Some "\x1b[3~"
        | Input.Key.Insert -> Some "\x1b[2~"
        | Input.Key.F n when n >= 1 && n <= 4 ->
            Some (Printf.sprintf "\x1bO%c" (Char.chr (Char.code 'P' + n - 1)))
        | Input.Key.F n when n >= 5 && n <= 12 ->
            let codes = [| "15"; "17"; "18"; "19"; "20"; "21"; "23"; "24" |] in
            if n - 5 < Array.length codes then
              Some (Printf.sprintf "\x1b[%s~" codes.(n - 5))
            else None
        | Input.Key.Char u ->
            let code = Uchar.to_int u in
            if ke.modifier.ctrl && code >= 0x40 && code <= 0x7f then
              (* Ctrl+letter -> control code *)
              let ctrl_code = code land 0x1f in
              Some (String.make 1 (Char.chr ctrl_code))
            else if code < 128 then Some (String.make 1 (Char.chr code))
            else
              (* UTF-8 encode *)
              let buf = Buffer.create 4 in
              Buffer.add_utf_8_uchar buf u;
              Some (Buffer.contents buf)
        | _ -> None
      in
      match data with
      | Some s -> (
          try
            let _ = Pty.write_string pty s 0 (String.length s) in
            ()
          with Unix.Unix_error (Unix.EIO, _, _) ->
            Pty.close pty;
            state.pty <- None)
      | None -> ())

let resize_state state ~cols ~rows =
  if cols <> state.cols || rows <> state.rows then (
    Vte.resize state.vte ~rows ~cols;
    (match state.pty with
    | Some pty ->
        let winsize = { Pty.rows; cols; xpixel = 0; ypixel = 0 } in
        Pty.set_winsize pty winsize
    | None -> ());
    { state with cols; rows })
  else state

let draw_border grid ~cols ~rows =
  let border_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_black
      ~bg:(Ansi.Color.grayscale ~level:2)
      ()
  in
  (* Top border *)
  let title = " Embedded Terminal (Ctrl+D to exit shell, Esc to quit) " in
  let padding = (cols - String.length title) / 2 in
  let top_line =
    String.make padding '-' ^ title
    ^ String.make (cols - padding - String.length title) '-'
  in
  Grid.draw_text ~style:border_style grid ~x:0 ~y:0 ~text:top_line;
  (* Bottom border *)
  let bottom = String.make cols '-' in
  Grid.draw_text ~style:border_style grid ~x:0 ~y:(rows - 1) ~text:bottom

let draw_vte grid state ~offset_y =
  let vte_grid = Vte.grid state.vte in
  let vte_rows = min (Grid.height vte_grid) (Grid.height grid - offset_y - 1) in
  let vte_cols = min (Grid.width vte_grid) (Grid.width grid) in
  Grid.blit_region ~src:vte_grid ~dst:grid ~src_x:0 ~src_y:0 ~width:vte_cols
    ~height:vte_rows ~dst_x:0 ~dst_y:offset_y

let draw_no_pty grid ~cols ~rows =
  let msg = "Failed to spawn shell. Press Esc to quit." in
  let style = Ansi.Style.make ~fg:Ansi.Color.bright_red () in
  Grid.draw_text ~style grid
    ~x:((cols - String.length msg) / 2)
    ~y:(rows / 2) ~text:msg

let () =
  let app =
    Matrix.create ~target_fps:(Some 30.) ~mouse_enabled:false
      ~debug_overlay:false ()
  in
  let state = ref (create_state ~cols:1 ~rows:1) in
  Matrix.run app
    ~on_frame:(fun app ~dt:_ ->
      read_pty_output !state;
      if !state.pty = None then Matrix.stop app)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key ke -> send_key !state ke
      | _ -> ())
    ~on_resize:(fun _ ~cols ~rows ->
      let term_rows = max 1 (rows - 2) in
      state := resize_state !state ~cols ~rows:term_rows)
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      draw_border grid ~cols ~rows;
      match !state.pty with
      | Some _ -> draw_vte grid !state ~offset_y:1
      | None -> draw_no_pty grid ~cols ~rows);
  cleanup !state
