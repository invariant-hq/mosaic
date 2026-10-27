(* A frame of the loop: a turn that finds a frame asked for returns it, and
   the frame is prepared and presented. The backend's clock moves a second
   at every read, so every frame is due at once and no turn waits. *)

(* The bytes the loop has written: setup checks that a frame writes what it
   changes, and the measured frames only add to the count. *)
let written = ref 0

(* A run on device [d], with [config], as [Matrix.run] gives one. *)
let run_on d ?stop ?(config = Matrix.config ()) f =
  Matrix.run ?stop ~config:(Matrix.on_device d config) f

let backend () : Matrix.Device.t =
  let clock = ref 0 in
  {
    now =
      (fun () ->
        clock := !clock + 1_000_000_000;
        !clock);
    wait = (fun ~until:_ ~reading:_ -> false);
    wake = (fun () -> ());
    take_signals = (fun () -> []);
    read_input = (fun _ _ _ -> -1);
    write_output = (fun _bytes _off len -> written := !written + len);
    fds = (fun ~reading:_ -> []);
    terminal_size = (fun () -> Some (160, 48));
    window_pixels = (fun () -> None);
    set_raw_mode = (fun _ -> ());
    flush_input = (fun () -> ());
    input_is_tty = false;
    output_is_tty = false;
    controlling = false;
    on_fatal = ignore;
    cleanup = (fun () -> ());
    env = Matrix.Terminal.Caps.env (fun _ -> None) ~win32:false;
    in_process = None;
  }

(* The output is no terminal, which a run writes no style to unless told:
   frames are written with their styles, as to a terminal. *)
let config =
  Matrix.config ~mode:`Alt ~raw_mode:false ~mouse:`Off ~bracketed_paste:false
    ~kitty_keyboard:`Off ~color:`Always ()

let frame app draw =
  Matrix.request_redraw app;
  match Matrix.next app with
  | Frame -> Matrix.draw app draw
  | _ -> failwith "runtime benchmark: no frame"

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

(* Every cell of the 160x48 terminal, in one of two texts. *)
let full_text =
  Array.map
    (fun first -> String.init 160 (fun i -> Char.chr (first + (i mod 26))))
    [| Char.code 'a'; Char.code 'A' |]

let fill text grid =
  for y = 0 to 47 do
    Matrix.Grid.draw_text grid ~x:0 ~y ~text
  done

(* The bytes [draw]'s frame writes. *)
let bytes app draw =
  let before = !written in
  frame app draw;
  !written - before

let require condition message =
  if not condition then failwith ("runtime benchmark: " ^ message)

let () =
  run_on (backend ()) ~config @@ fun app ->
  frame app ignore;
  frame app ignore;
  require (bytes app ignore = 0) "an unchanged frame wrote bytes";
  require (bytes app (fill full_text.(1)) > 0) "a full frame wrote nothing";
  require (bytes app (fill full_text.(0)) > 0) "a full frame wrote nothing";
  require (bytes app ignore > 0) "clearing the frame wrote nothing";
  (* A sample draws both texts, so it ends where it started. *)
  let full =
    Thumper.bench "frame/full-160x48-and-back" (fun () ->
        frame app (fill full_text.(1));
        frame app (fill full_text.(0)))
  in
  let no_changes =
    Thumper.bench "frame/no-changes" (fun () -> frame app ignore)
  in
  let single_cell =
    let next = ref false in
    Thumper.bench "frame/single-cell" (fun () ->
        next := not !next;
        let char = if !next then 'X' else 'Y' in
        frame app (fun grid ->
            Matrix.Grid.set_uchar grid ~x:0 ~y:0 ~uchar:(Uchar.of_char char)
              ~fg:Matrix.Ansi.Color.white ~bg:Matrix.Ansi.Color.black
              ~attrs:Matrix.Ansi.Attr.empty ()))
  in
  Thumper.run "runtime"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      :: (if exact then [] else [ Thumper.Budget.no_slower_than 0.05 ]))
    [ Thumper.group "render" [ no_changes; single_cell; full ] ]
