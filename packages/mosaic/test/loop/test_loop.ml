(* Full-loop tests: a real Mosaic application driven in a matrix.test session,
   a terminal emulated in the test's process with a clock only the test
   moves. Input is the bytes a terminal sends; every call settles, so every
   step observes a fully rendered, quiescent frame at the session's time. *)

open Windtrap

type model = {
  keys : string list; (* decoded key names, oldest first *)
  ticks : int;
  size : (int * int) option;
  note : string;
}

type msg =
  | Pressed of string
  | Tick
  | Ticks of int
  | Resized of int * int
  | Note of string
  | Ignored

let initial = { keys = []; ticks = 0; size = None; note = "-" }

let key_name key =
  let data = Mosaic.Event.Key.data key in
  match data.Matrix.Input.Keystroke.key with
  | Char u when Uchar.is_char u -> String.make 1 (Uchar.to_char u)
  | Up -> "<up>"
  | Escape -> "<esc>"
  | _ -> "<other>"

let update msg model =
  match msg with
  | Pressed name ->
      ({ model with keys = model.keys @ [ name ] }, Mosaic.Cmd.none)
  | Tick -> ({ model with ticks = model.ticks + 1 }, Mosaic.Cmd.none)
  | Ticks n -> ({ model with ticks = model.ticks + n }, Mosaic.Cmd.none)
  | Resized (w, h) -> ({ model with size = Some (w, h) }, Mosaic.Cmd.none)
  | Note note -> ({ model with note }, Mosaic.Cmd.none)
  | Ignored -> (model, Mosaic.Cmd.none)

let view model =
  Mosaic.text
    (Printf.sprintf "keys:[%s] ticks:%d size:%s note:%s"
       (String.concat "," model.keys)
       model.ticks
       (match model.size with
       | None -> "-"
       | Some (w, h) -> Printf.sprintf "%dx%d" w h)
       model.note)

let app ?(subs = fun _ -> Mosaic.Sub.none) ?(init_cmd = Mosaic.Cmd.none) () =
  {
    Mosaic.init = (fun () -> (initial, init_cmd));
    update;
    view;
    subscriptions = subs;
  }

(* A plain modern terminal's capabilities, assumed where the terminal says
   nothing, so that rendering never depends on the environment. The terminal
   is an output that is not a terminal, which the run asks nothing. *)
let caps : Matrix.Terminal.capabilities =
  {
    colors = `Truecolor;
    kitty_keyboard = None;
    kitty_graphics = false;
    bracketed_paste = true;
    focus_tracking = true;
    unicode_width = `Unicode;
    grapheme_mode = None;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = false;
    scaled_text = false;
    sixel = false;
    graphics = `Blocks;
    sync = false;
    hyperlinks = false;
    notification = `Osc9;
    meta_sends_escape = None;
    multiplexer = None;
    identity = None;
  }

let profile = { Matrix_test.Profile.pipe with width_method = `Unicode }

(* A session and its terminal. A test that takes the program's steps itself
   uses the session made with the terminal; [run] gives the one it runs the
   program on. *)
type t = { mutable s : Matrix_test.t; term : Matrix_test.Terminal.t }

let base ?(mode = `Alt) () = Matrix.config ~mode ~caps ()

let terminal ?(profile = profile) ~width ~height () =
  let term = Matrix_test.Terminal.create ~profile ~cols:width ~rows:height () in
  { s = Matrix_test.create ~config:(base ()) term; term }

let config t = Matrix_test.config t.s
let screen_value t = Matrix_test.Terminal.screen t.term

(* The screen's rows without their trailing blanks, one a line. *)
let screen t =
  let s = screen_value t in
  String.concat "\n"
    (List.init (Matrix_test.Screen.rows s) (Matrix_test.Screen.row s))

let written t = Matrix_test.Terminal.output t.term
let elapsed t = Matrix_test.now t.s
let feed t bytes = Matrix_test.input t.s bytes

let snap t =
  screen t |> String.split_on_char '\n'
  |> List.iter (fun row -> print_endline ("|" ^ row))

(* The rows of [s] without their trailing blanks. *)
let rstrip_rows s =
  String.split_on_char '\n' s
  |> List.map (fun row ->
      let n = ref (String.length row) in
      while !n > 0 && row.[!n - 1] = ' ' do
        decr n
      done;
      String.sub row 0 !n)
  |> String.concat "\n" |> String.trim

let ns seconds = Float.to_int (Float.round (seconds *. 1e9))

(* The times [sub] occurs in [s] from [from]. *)
let occurrences ?(from = 0) s sub =
  let n = String.length sub in
  let count = ref 0 in
  for i = from to String.length s - n do
    if String.sub s i n = sub then incr count
  done;
  !count

(* Runs [application] with [Mosaic.run] on [t]'s terminal, then [f], and
   ends the input. Work off the loop runs on the session's queue unless
   [process_perform] says otherwise, with [quiesce] for it. *)
let run ?mode ?process_perform ?quiesce t application f =
  Mosaic_test.run ~config:(base ?mode ()) ?process_perform ?quiesce t.term
    application (fun s ->
      t.s <- s;
      f s)

let drive ?mode ?(width = 48) ?(height = 2) ?process_perform ?quiesce
    application steps =
  let t = terminal ~width ~height () in
  run ?mode ?process_perform ?quiesce t application (fun _ ->
      ( function
      | `Feed bytes -> feed t bytes
      | `Resize (width, height) ->
          Matrix_test.resize t.s ~cols:width ~rows:height
      | `Advance dt -> Matrix_test.advance t.s (ns dt)
      | `Step_to target ->
          Matrix_test.advance t.s (max 0 (ns target - elapsed t))
      | `Snap -> snap t
      | `Run f ->
          f t;
          if not (Matrix_test.ended t.s) then Matrix_test.settle t.s )
      |> fun step ->
      List.iter
        (fun s ->
          step s;
          (* The oracle: the terminal shows the frame the run presented. *)
          Matrix_test.check_presented t.s)
        steps)

let on_keys _ = Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)))

let%expect_test "boots, renders, and reacts to byte-decoded input" =
  drive (app ~subs:on_keys ()) [ `Snap; `Feed "a"; `Snap; `Feed "b"; `Snap ];
  [%expect
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[a] ticks:0 size:- note:-
    |
    |keys:[a,b] ticks:0 size:- note:-
    |
    |}]

let%expect_test "in-frame geometry changes settle without input" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _model ->
          Mosaic.scroll_box ~scroll:(Mosaic.Scroll.at 0)
            ~on_scroll:(fun _ -> Ignored)
            ~size:(Mosaic.size ~width:20 ~height:4)
            (List.init 10 (fun i -> Mosaic.text (Printf.sprintf "Line %d" i))));
    }
  in
  drive ~width:20 ~height:4 application [ `Snap ];
  [%expect_exact
    {||Line 0             █
|Line 1             ▀
|Line 2
|Line 3
|}]

let%expect_test "escape sequences decode as single keys" =
  (* "\027[A" is three bytes but one Up key: the real parser runs. *)
  drive (app ~subs:on_keys ()) [ `Feed "\027[A"; `Snap ];
  [%expect {|
    |keys:[<up>] ticks:0 size:- note:-
    |
    |}]

let%expect_test "virtual time drives every-subscriptions deterministically" =
  drive
    (app
       ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 1.0 (fun k -> Ticks k))
       ())
    [
      `Snap;
      `Advance 1.0;
      `Snap;
      `Advance 1.0;
      `Snap;
      `Advance 0.5;
      `Snap;
      `Advance 0.5;
      `Snap;
    ];
  [%expect
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[] ticks:1 size:- note:-
    |
    |keys:[] ticks:2 size:- note:-
    |
    |keys:[] ticks:2 size:- note:-
    |
    |keys:[] ticks:3 size:- note:-
    |
    |}]

let%expect_test "live animation frames deliver their delta in seconds" =
  (* Matrix keeps integer nanoseconds; the render pipeline (renderable.mli)
     takes the time between frames in seconds. While a live widget (the
     spinner) animates, the virtual clock advances at the 60 fps render
     cadence, so widgets must observe 1/60 s frames. A canvas records the
     delta it is drawn with; once the timer fires the spinner leaves the view,
     the loop goes idle, and the recording is checked. A settle draws the
     first frame only: the spinner's next frames come as the clock moves. *)
  let last_delta = ref Float.nan in
  let application =
    {
      (app
         ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 0.2 (fun k -> Ticks k))
         ())
      with
      Mosaic.view =
        (fun model ->
          if model.ticks = 0 then
            Mosaic.box
              [
                Mosaic.spinner ();
                Mosaic.canvas ~size:(Mosaic.size ~width:2 ~height:1)
                  (fun _ ~dt -> last_delta := dt);
              ]
          else Mosaic.text "done");
    }
  in
  drive application
    [
      `Advance 0.2;
      `Run (fun _ -> Printf.printf "live frame delta: %.4f s\n" !last_delta);
    ];
  [%expect {| live frame delta: 0.0167 s |}]

let%expect_test "a pending every-timer renders only when it fires" =
  (* A timer is not an animation: while it is merely pending, the loop stays
     idle — no view pass, no reconcile, no paint. The view-call count is the
     witness: it must not move as virtual time approaches the deadline, and
     must move exactly once when the timer fires. *)
  let views = ref 0 in
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 1.0 (fun k -> Ticks k)) ()
  in
  let application =
    {
      base with
      Mosaic.view =
        (fun model ->
          incr views;
          base.Mosaic.view model);
    }
  in
  let baseline = ref 0 in
  drive application
    [
      `Run (fun _ -> baseline := !views);
      `Advance 0.4;
      `Advance 0.4;
      `Run
        (fun _ ->
          Printf.printf "renders while pending: %d\n" (!views - !baseline));
      `Advance 0.4;
      `Run
        (fun _ -> Printf.printf "renders after fire: %d\n" (!views - !baseline));
      `Snap;
    ];
  [%expect
    {|
    renders while pending: 0
    renders after fire: 1
    |keys:[] ticks:1 size:- note:-
    |
    |}]

let%expect_test "resize reaches the application and the grid" =
  drive
    (app
       ~subs:(fun _ ->
         Mosaic.Sub.on_resize (fun ~width ~height -> Resized (width, height)))
       ())
    [ `Snap; `Resize (60, 3); `Snap ];
  [%expect
    {|
    |keys:[] ticks:0 size:48x2 note:-
    |
    |keys:[] ticks:0 size:60x3 note:-
    |
    |
    |}]

let%expect_test "a transparent table realigns every row when it widens" =
  let columns =
    [
      Mosaic.Table.column ~width:(`Fixed 2) "";
      Mosaic.Table.column ~width:(`Flex 4.) "session";
      Mosaic.Table.column ~width:`Auto "lifecycle";
      Mosaic.Table.column ~width:`Auto "phase";
      Mosaic.Table.column ~width:`Auto ~alignment:`Right "turns";
      Mosaic.Table.column ~width:`Auto "updated";
      Mosaic.Table.column ~width:`Auto "recency";
    ]
  in
  let rows =
    [
      Mosaic.Table.row "first"
        [|
          Mosaic.Table.cell "❯";
          Mosaic.Table.cell "Retained parser investigation";
          Mosaic.Table.cell "active";
          Mosaic.Table.cell "idle";
          Mosaic.Table.cell "1 turn";
          Mosaic.Table.cell "just now";
          Mosaic.Table.cell "today";
        |];
      Mosaic.Table.row "second"
        [|
          Mosaic.Table.cell "";
          Mosaic.Table.cell "session-visual-2";
          Mosaic.Table.cell "active";
          Mosaic.Table.cell "idle";
          Mosaic.Table.cell "1 turn";
          Mosaic.Table.cell "just now";
          Mosaic.Table.cell "today";
        |];
    ]
  in
  let base = app ~subs:on_keys () in
  let application =
    {
      base with
      Mosaic.view =
        (fun model ->
          let width = if model.keys = [] then 76 else 80 in
          Mosaic.table
            ~size:(Mosaic.size ~width ~height:3)
            ~columns ~rows ~border:false ~equal:String.equal
            ~selected:(Some "first")
            ~on_change:(fun _ -> Ignored)
            ~show_header:true ~cell_padding:0
            ~selected_background:(Matrix.Ansi.Color.grayscale ~level:3)
            ());
    }
  in
  drive ~width:80 ~height:3 application [ `Snap; `Feed "w"; `Snap ];
  [%expect
    {|
    |   session                           lifecycle phase  turns updated  recency
    |❯  Retained parser investigation     active    idle  1 turn just now today
    |   session-visual-2                  active    idle  1 turn just now today
    |   session                               lifecycle phase  turns updated  recency
    |❯  Retained parser investigation         active    idle  1 turn just now today
    |   session-visual-2                      active    idle  1 turn just now today
    |}]

(* A box lays its border out from [border_width]: a side of width 0 draws no
   line and takes no cell, and a side of width 2 leaves a blank cell inside
   its line. *)
let%expect_test "a box draws the border widths it is given" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.box ~border:true
            ~border_width:(Mosaic.padding_lrtb 0 2 1 1)
            ~size:(Mosaic.size ~width:6 ~height:3)
            [ Mosaic.text "x" ]);
    }
  in
  drive ~width:8 ~height:3 application [ `Snap ];
  [%expect {|
    |─────┐
    |x    │
    |─────┘
    |}]

(* A hover handler that dispatches a message, in a view that rebuilds the
   rows on every update: the hovered row is rebuilt equal while another row
   counts the updates. The table reports a hover only when the hovered row
   differs, by index or by value, so the run settles after one report. *)
let%expect_test "a hover message with rebuilt rows settles" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun model ->
          Mosaic.table
            ~size:(Mosaic.size ~width:20 ~height:3)
            ~columns:[ Mosaic.Table.column "c" ]
            ~rows:
              [
                Mosaic.Table.row "a" [| Mosaic.Table.cell "a" |];
                Mosaic.Table.row "b" [| Mosaic.Table.cell "b" |];
                Mosaic.Table.row "n"
                  [| Mosaic.Table.cell (string_of_int model.ticks) |];
              ]
            ~border:false ~show_header:false ~equal:String.equal ~selected:None
            ~on_change:(fun _ -> Ignored)
            ~on_hover:(fun _ -> Some Tick)
            ());
    }
  in
  drive ~width:20 ~height:3 application [ `Feed "\027[<35;2;1M"; `Snap ];
  [%expect {|
    |a
    |b
    |1
    |}]

(* What [Mosaic.Probe] told: whether a perform is in flight, or a message
   not yet applied. The program's executor tells the first, and the run's
   steps the second. *)
let%expect_test "a queued perform and its message settle with the session" =
  let pending : (unit -> unit) Queue.t = Queue.create () in
  drive
    ~process_perform:(fun thunk -> Queue.push thunk pending)
    ~quiesce:ignore
    (app
       ~init_cmd:(Mosaic.Cmd.perform (fun dispatch -> dispatch (Note "loaded")))
       ())
    [
      `Snap;
      `Run
        (fun _ ->
          equal ~msg:"held while the perform is queued" int 1
            (Queue.length pending));
      `Run (fun _ -> Queue.pop pending ());
      `Snap;
    ];
  [%expect
    {|
    |keys:[] ticks:0 size:- note:-
    |
    |keys:[] ticks:0 size:- note:loaded
    |
    |}]

let%expect_test "replaying the output stream through a VTE matches the grid" =
  let result = ref None in
  drive (app ~subs:on_keys ())
    [
      `Feed "a";
      `Feed "b";
      `Run
        (fun t ->
          let vte = Vte.create ~rows:2 ~cols:48 () in
          ignore (Vte.feed_string vte (written t) : _ list);
          result := Some (Vte.to_string vte, screen t));
    ];
  let replayed, screen = Option.get !result in
  equal ~msg:"replaying the output stream reconstructs the screen" text
    (rstrip_rows screen) (rstrip_rows replayed);
  print_endline "replayed:";
  String.split_on_char '\n' replayed
  |> List.iter (fun row -> print_endline ("|" ^ row));
  [%expect {|
    replayed:
    |keys:[a,b] ticks:0 size:- note:-
    |
    |}]

let%expect_test
    "startup focus emits blinking cursor style when it becomes visible" =
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.input ~state:Mosaic.Editor.empty
            ~on_change:(fun _ -> Ignored)
            ~focused:true ~cursor_style:`Block ~cursor_blinking:true
            ~cursor_color:Matrix.Ansi.Color.red ());
    }
  in
  drive application
    [
      `Run
        (fun t ->
          let output = written t in
          in_order ~msg:"blinking style follows cursor visibility"
            ~subs:[ "\027[?25h"; "\027[1 q" ]
            output);
    ];
  [%expect {| |}]

let%expect_test "an every-timer stepped exactly onto its deadline keeps firing"
    =
  (* Advancing by exactly the interval makes the frame delta [now -. last_time]
     the subtraction of two accumulated floats, which can fall an ULP short of
     the interval (0.1 +. 0.1 +. ... never lands on a clean multiple). A strict
     deadline test would then skip the fire while the wakeup re-arms as
     immediately due — a busy loop. The tick count must keep climbing one per
     step, never stalling and never doubling. *)
  let steps = ref [] in
  for _ = 1 to 8 do
    steps := `Snap :: `Advance 0.1 :: !steps
  done;
  drive
    (app
       ~subs:(fun _ -> Mosaic.Sub.every ~id:"timer" 0.1 (fun k -> Ticks k))
       ())
    (List.rev !steps);
  [%expect
    {|
    |keys:[] ticks:1 size:- note:-
    |
    |keys:[] ticks:2 size:- note:-
    |
    |keys:[] ticks:3 size:- note:-
    |
    |keys:[] ticks:4 size:- note:-
    |
    |keys:[] ticks:5 size:- note:-
    |
    |keys:[] ticks:6 size:- note:-
    |
    |keys:[] ticks:7 size:- note:-
    |
    |keys:[] ticks:8 size:- note:-
    |
    |}]

let%expect_test "a lone ESC is Escape in the turn that reads it" =
  (* A bare ESC is ambiguous (Escape vs. the start of an Alt/CSI sequence), so
     the decoder holds it until the terminal is silent. The session's
     terminal writes each input whole, so nothing follows the ESC and it is
     Escape at once; the race of a real terminal's latency is the pty
     tier's. *)
  drive
    (app
       ~subs:(fun _ ->
         Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key))))
       ())
    [ `Feed "\027"; `Snap; `Advance 0.06; `Snap ];
  [%expect
    {|
    |keys:[<esc>] ticks:0 size:- note:-
    |
    |keys:[<esc>] ticks:0 size:- note:-
    |
    |}]

let%expect_test "perform dispatches from concurrent threads are all delivered" =
  (* Cmd.perform runs its callback on a fresh thread (the default
     [process_perform]), and mosaic.mli promises dispatch may be called from
     any thread. Several threads hammer dispatch while the loop drains
     concurrently; systhread preemption lands inside queue operations, so a
     non-atomic pending queue loses (or resurrects) messages. The final tick
     count must be exact. *)
  (* Each burst must outlast the ~50 ms systhread tick so preemption lands
     mid-push with other threads still active — a shorter burst completes
     within one scheduler slice and never races. *)
  let thread_count = 3 in
  let per_thread = 2_000_000 in
  let hammer =
    Mosaic.Cmd.batch
      (List.init thread_count (fun _ ->
           Mosaic.Cmd.perform (fun dispatch ->
               for _ = 1 to per_thread do
                 dispatch Tick
               done)))
  in
  (* The work runs on threads the test keeps, and quiescence is their end:
     each dispatch has reached the loop's queue once its thread is joined. *)
  let threads = ref [] in
  let process_perform job = threads := Thread.create job () :: !threads in
  let quiesce () =
    List.iter Thread.join !threads;
    threads := []
  in
  let t = terminal ~width:48 ~height:2 () in
  run ~process_perform ~quiesce t (app ~init_cmd:hammer ()) (fun _ -> snap t);
  [%expect {|
    |keys:[] ticks:6000000 size:- note:-
    |
    |}]

let%expect_test "paste capture runs before the focused editor" =
  (* Capture listeners run in declaration order before the insertion. A later
     editor proposal cannot retroactively consume their delivery. *)
  let order = ref [] in
  let record step = order := step :: !order in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_paste ~phase:Capture (fun ev ->
            record "capture";
            Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
        Mosaic.Sub.on_paste ~phase:Capture (fun ev ->
            record "all";
            Some (Pressed ("P:" ^ Mosaic.Event.Paste.text ev)));
      ]
  in
  let base = app ~subs () in
  let with_editor =
    {
      base with
      Mosaic.view =
        (fun model ->
          Mosaic.box ~flex_direction:Row
            [
              Mosaic.textarea ~state:Mosaic.Editor.empty
                ~on_change:(fun editor ->
                  record "editor";
                  equal ~msg:"paste reaches the editor proposal" string "hi"
                    (Mosaic.Editor.text editor);
                  Ignored)
                ~size:(Mosaic.size ~width:10 ~height:1)
                ~focused:true ();
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 with_editor [ `Feed "\027[200~hi\027[201~"; `Snap ];
  equal ~msg:"capture precedes the editor default action" (list string)
    [ "capture"; "all"; "editor" ]
    (List.rev !order);
  order := [];
  drive base [ `Feed "\027[200~hi\027[201~"; `Snap ];
  equal ~msg:"unfocused delivery keeps declaration order" (list string)
    [ "capture"; "all" ] (List.rev !order);
  [%expect
    {|
    |          keys:[p:hi,P:hi] ticks:0 size:- note:-
    |
    |
    |keys:[p:hi,P:hi] ticks:0 size:- note:-
    |
    |}]

let%expect_test "a mouse press a widget stops skips on_mouse" =
  (* A capture listener sees the press before the widget; a bubble listener
     after it, unless the widget stopped it, and reads whether the widget
     prevented it ("m!"). *)
  let log_downs tag ev =
    match Mosaic.Event.Mouse.kind ev with
    | Mosaic.Event.Mouse.Down _ ->
        let used = Mosaic.Event.Mouse.default_prevented ev in
        Some (Pressed (if used then tag ^ "!" else tag))
    | _ -> None
  in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_mouse (log_downs "m");
        Mosaic.Sub.on_mouse ~phase:Capture (log_downs "M");
      ]
  in
  let base = app ~subs () in
  let on_down f ev =
    (match Mosaic.Event.Mouse.kind ev with
    | Mosaic.Event.Mouse.Down _ -> f ev
    | _ -> ());
    None
  in
  let view handler model =
    Mosaic.box
      ~size:(Mosaic.size ~width:48 ~height:2)
      ?on_mouse:handler
      [ base.Mosaic.view model ]
  in
  let press = "\027[<0;41;2M\027[<0;41;2m" in
  List.iter
    (fun handler ->
      drive { base with Mosaic.view = view handler } [ `Feed press; `Snap ])
    [
      Some (on_down Mosaic.Event.Mouse.stop_propagation);
      Some (on_down Mosaic.Event.Mouse.prevent_default);
      None;
    ];
  [%expect
    {|
    |keys:[M] ticks:0 size:- note:-
    |
    |keys:[M,m!] ticks:0 size:- note:-
    |
    |keys:[M,m] ticks:0 size:- note:-
    |
    |}]

let%expect_test "model focus follows a remounted editor" =
  let observed = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> ((0, Mosaic.Editor.empty), Mosaic.Cmd.none));
      update =
        (fun msg (generation, editor) ->
          match msg with
          | `Remount -> ((generation + 1, editor), Mosaic.Cmd.none)
          | `Edited editor ->
              observed := editor;
              ((generation, editor), Mosaic.Cmd.none));
      view =
        (fun (generation, editor) ->
          Mosaic.input ~key:(string_of_int generation) ~focused:true
            ~state:editor
            ~on_change:(fun editor -> `Edited editor)
            ());
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_key (fun ev ->
              if Mosaic.Shortcut.matches (Mosaic.Shortcut.f 2) ev then (
                Mosaic.Event.Key.prevent_default ev;
                Some `Remount)
              else None));
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "a";
      `Feed "\027OQ";
      `Feed "b";
      `Run
        (fun t ->
          equal ~msg:"accepted editor survives remount" string "ab"
            (Mosaic.Editor.text !observed);
          equal ~msg:"remounted focused editor paints accepted state" string
            "ab"
            (String.trim (screen t)));
    ];
  [%expect {| |}]

let%expect_test "same-interval timers keep independent phases" =
  (* Two Sub.every timers with the same interval must not share deadline
     state: a timer added mid-flight (here at t=1.6 via a key) is due one
     interval later, at t=2.6 — not at the first timer's t=2.0 deadline.
     Elapsed-time bookkeeping matched previous timers by interval alone, so
     the newcomer inherited the first timer's phase and fired early. *)
  let subs model =
    Mosaic.Sub.batch
      (Mosaic.Sub.on_key (fun _ -> Some (Note "on"))
      :: Mosaic.Sub.every ~id:"first" 1.0 (fun _ -> Pressed "a")
      ::
      (if String.equal model.note "on" then
         [ Mosaic.Sub.every ~id:"second" 1.0 (fun _ -> Pressed "b") ]
       else []))
  in
  drive (app ~subs ())
    [
      `Advance 1.0 (* t=1.0: a fires *);
      `Advance 0.6 (* t=1.6 *);
      `Feed "x" (* adds the second timer mid-interval *);
      `Advance 0.4 (* t=2.0: a fires; b must NOT *);
      `Snap;
      `Advance 0.6 (* t=2.6: b fires *);
      `Snap;
    ];
  [%expect
    {|
    |keys:[a,a] ticks:0 size:- note:on
    |
    |keys:[a,a,b] ticks:0 size:- note:on
    |
    |}]

let%expect_test "a timer stepped deadline by deadline keeps the nominal grid" =
  (* A driver that moves the clock to each armed wakeup and stops exactly at
     t=2.0 must see a 0.1 s timer fire twenty times: the twentieth deadline is
     [20 * 0.1], the nominal instant. Carrying the deadline forward by
     repeated addition drifted it past 2.0 by a few ULPs after twenty steps,
     and an exact comparison then lost the final fire beyond the stop (19,
     then 29). The accumulating driver above is the mirror image: there the
     clock is the side that drifts. *)
  let subs _ = Mosaic.Sub.every ~id:"timer" 0.1 (fun k -> Ticks k) in
  drive (app ~subs ()) [ `Step_to 2.0; `Snap; `Step_to 3.0; `Snap ];
  [%expect
    {||keys:[] ticks:20 size:- note:-
|
|keys:[] ticks:30 size:- note:-
||}]

let%expect_test "a late unfocused editor never steals focus" =
  let base =
    app ~subs:(fun _ -> Mosaic.Sub.on_key (fun _ -> Some (Note "shown"))) ()
  in
  let application =
    {
      base with
      Mosaic.init =
        (fun () -> ({ initial with note = "hidden" }, Mosaic.Cmd.none));
      view =
        (fun model ->
          Mosaic.box
            [
              (if model.note = "shown" then
                 Mosaic.input ~state:Mosaic.Editor.empty ~focused:false
                   ~on_change:(fun _ -> Ignored)
                   ()
               else Mosaic.empty);
              base.Mosaic.view model;
            ]);
    }
  in
  drive ~height:3 application
    [
      `Feed "x";
      `Run
        (fun t ->
          not_contains ~msg:"late element must not steal focus" ~sub:"\027[?25h"
            (written t));
    ];
  [%expect {| |}]

let%expect_test "a late width method re-measures existing text" =
  (* The terminal reports wcwidth (DECRPM 2027) after the first frame. A
     joined emoji is one 2-cell cluster under Unicode and 4 cells under
     wcwidth, so every widget that measured it must measure it again: the
     bars after Text, Code and an input move, and wrapped text takes more
     rows. *)
  let emoji = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" in
  let row child =
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
      [ child; Mosaic.text "|" ]
  in
  let base = app () in
  let application =
    {
      base with
      Mosaic.view =
        (fun _ ->
          Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
            [
              row (Mosaic.text (emoji ^ "t"));
              row (Mosaic.code (emoji ^ "c"));
              row
                (Mosaic.input ~flex_shrink:0.
                   ~state:(Mosaic.Editor.of_string (emoji ^ "e"))
                   ~on_change:(fun _ -> Ignored)
                   ());
              row
                (Mosaic.box
                   ~size:(Mosaic.size_wh (Mosaic.px 4) Mosaic.auto)
                   [ Mosaic.text ~wrap:`Char (emoji ^ emoji) ]);
              Mosaic.text "end";
            ]);
    }
  in
  (* Each cell a grapheme starts, and [-] for the cells it spans after its
     first, as a terminal that measures by [width_method] shows the output:
     the session's, which measures by grapheme, before the answer, and one
     that measures by character, the output replayed into it, after. *)
  let cells ?(width_method = `Unicode) t =
    let vte = Vte.create ~width_method ~rows:7 ~cols:12 () in
    ignore (Vte.feed_string vte (written t) : _ list);
    let grid = Vte.grid vte in
    let width = Matrix.Grid.width grid in
    for y = 0 to Matrix.Grid.height grid - 1 do
      let row = Buffer.create width in
      for x = 0 to width - 1 do
        let i = (y * width) + x in
        Buffer.add_string row
          (if Matrix.Grid.is_continuation grid i then "-"
           else
             match Matrix.Grid.get_text grid i with "" -> " " | text -> text)
      done;
      print_endline ("|" ^ String.trim (Buffer.contents row))
    done
  in
  drive ~width:12 ~height:7 application
    [
      `Run (fun t -> cells t);
      `Feed "\027[?2027;4$y";
      `Run (fun t -> cells ~width_method:`Wcwidth t);
    ];
  [%expect
    {|
    |👩‍💻-t|
    |👩‍💻-c|
    |👩‍💻-e |
    |👩‍💻-👩‍💻-|
    |end
    |
    |
    |👩‍-💻-t|
    |👩‍-💻-c|
    |👩‍-💻-e |
    |👩‍-💻-|
    |👩‍-💻-
    |end
    |
    |}]

let%expect_test "static output follows the terminal's width method" =
  (* Once the terminal reports wcwidth, a static commit lays its text out by
     it too: two joined emoji take 8 cells, which wrap in 6 columns. *)
  let emoji = "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" in
  let base = app ~subs:on_keys () in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          match msg with
          | Pressed "s" ->
              ( model,
                Mosaic.Cmd.static_commit
                  (Mosaic.text ~wrap:`Char (emoji ^ emoji)) )
          | _ -> (model, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "live");
    }
  in
  drive ~mode:`Primary ~width:6 ~height:4 application
    [
      `Feed "\027[?2027;4$y";
      `Feed "s";
      `Run
        (fun t ->
          let vte = Vte.create ~rows:4 ~cols:6 () in
          ignore (Vte.feed_string vte (written t) : _ list);
          print_string (rstrip_rows (Vte.to_string vte)));
    ];
  [%expect {|
    👩‍💻
    👩‍💻
    live
    |}]

let%expect_test "static commit emits content taller than the viewport" =
  (* Cmd.static_commit renders offscreen at the content's natural height:
     every line of a static view taller than the terminal must reach the
     output stream, exercising the measure-then-grow pass. *)
  let lines = List.init 5 (fun i -> Printf.sprintf "static line %d" i) in
  let static_view = Mosaic.fragment (List.map Mosaic.text lines) in
  drive ~mode:`Primary ~width:40 ~height:3
    (app ~init_cmd:(Mosaic.Cmd.static_commit static_view) ())
    [
      `Run
        (fun t ->
          let out = written t in
          List.iter
            (fun line -> contains ~msg:(line ^ " emitted") ~sub:line out)
            lines);
    ];
  [%expect {| |}]

let join_worker worker =
  let joined = Atomic.make false in
  ignore
    (Thread.create
       (fun () ->
         Thread.join worker;
         Atomic.set joined true)
       ()
      : Thread.t);
  let tries = ref 0 in
  while (not (Atomic.get joined)) && !tries < 5000 do
    incr tries;
    Thread.delay 0.001
  done;
  is_true ~msg:"worker exits within five seconds" (Atomic.get joined)

(* An executor that runs each piece of work on a thread of its own. *)
let threads work = ignore (Thread.create work () : Thread.t)

let%expect_test "threaded highlighting runs on the program's executor" =
  (* On the session's queue, the highlighting is done by the time the run
     settles: the first settled frame shows it, with no thread. *)
  let calls = ref 0 in
  let highlighter =
    Mosaic.Code.Highlighter.threaded_incremental (fun { content; _ } ->
        incr calls;
        let meta =
          { Mosaic.Syntax_highlight.default_meta with conceal = Some "done" }
        in
        ( Mosaic.Syntax_highlight.make ~content
            [
              Mosaic.Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3
                ~scope:"keyword" ();
            ],
          () ))
  in
  let syntax = Mosaic.Code.with_highlighter ~language:"example" highlighter in
  let base = app () in
  drive ~width:20 ~height:1
    { base with Mosaic.view = (fun _ -> Mosaic.code ~syntax "let x") }
    [ `Snap; `Run (fun _ -> Printf.printf "calls: %d\n" !calls) ];
  [%expect {|
    |done x
    calls: 1
    |}]

let%expect_test "threaded highlighting wakes an idle application" =
  let released = Atomic.make false in
  let worker = Atomic.make None in
  let highlighter =
    Mosaic.Code.Highlighter.threaded_incremental (fun { content; _ } ->
        Atomic.set worker (Some (Thread.self ()));
        while not (Atomic.get released) do
          Thread.delay 0.001
        done;
        let meta =
          { Mosaic.Syntax_highlight.default_meta with conceal = Some "done" }
        in
        ( Mosaic.Syntax_highlight.make ~content
            [
              Mosaic.Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3
                ~scope:"keyword" ();
            ],
          () ))
  in
  let syntax = Mosaic.Code.with_highlighter ~language:"example" highlighter in
  let base = app () in
  let application =
    { base with Mosaic.view = (fun _ -> Mosaic.code ~syntax "let x") }
  in
  let t = terminal ~width:20 ~height:1 () in
  Fun.protect
    ~finally:(fun () -> Atomic.set released true)
    (fun () ->
      (* The work blocks until the test releases it, so it runs on a thread
         the program's executor starts, and the session waits for nothing. *)
      run ~process_perform:threads ~quiesce:ignore t application (fun _ ->
          equal ~msg:"plain initial frame" string "let x"
            (String.trim (screen t));
          Atomic.set released true;
          let tries = ref 0 in
          while Atomic.get worker = None && !tries < 5000 do
            incr tries;
            Thread.delay 0.001
          done;
          let thread =
            match Atomic.get worker with
            | Some thread -> thread
            | None -> fail "worker did not start"
          in
          join_worker thread;
          (* The worker's result is posted to the loop. *)
          Matrix_test.settle t.s;
          equal ~msg:"highlight applied without user input" string "done x"
            (String.trim (screen t))));
  [%expect {| |}]

let%expect_test "application exit cancels queued highlighting" =
  List.iter
    (fun exceptional ->
      let released = Atomic.make false in
      let worker = Atomic.make None in
      let calls = Atomic.make [] in
      let highlighter =
        Mosaic.Code.Highlighter.threaded_incremental (fun { content; _ } ->
            Atomic.set worker (Some (Thread.self ()));
            Atomic.set calls (content :: Atomic.get calls);
            while not (Atomic.get released) do
              Thread.delay 0.001
            done;
            (Mosaic.Syntax_highlight.make ~content [], ()))
      in
      let syntax =
        Mosaic.Code.with_highlighter ~language:"example" highlighter
      in
      let base = app () in
      let application =
        {
          base with
          Mosaic.view =
            (fun _ ->
              Mosaic.box
                [ Mosaic.code ~syntax "first"; Mosaic.code ~syntax "queued" ]);
        }
      in
      let body _ =
        let tries = ref 0 in
        while Atomic.get worker = None && !tries < 5000 do
          incr tries;
          Thread.delay 0.001
        done;
        is_true ~msg:"first worker started" (Option.is_some (Atomic.get worker));
        if exceptional then failwith "application failed"
      in
      let t = terminal ~width:20 ~height:2 () in
      Fun.protect
        ~finally:(fun () -> Atomic.set released true)
        (fun () ->
          if exceptional then
            raises_match ~msg:"application failure preserved"
              (function
                | Failure message -> message = "application failed" | _ -> false)
              (fun () ->
                run ~process_perform:threads ~quiesce:ignore t application body)
          else run ~process_perform:threads ~quiesce:ignore t application body);
      join_worker (Option.get (Atomic.get worker));
      equal ~msg:"closed application leaves no queued work" (list string)
        [ "first" ] (Atomic.get calls))
    [ false; true ];
  [%expect {| |}]

let%expect_test
    "rejected edits do not become the base of the next key in a burst" =
  let proposals = ref [] in
  let accepted = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun proposal editor ->
          let text = Mosaic.Editor.text proposal in
          proposals := text :: !proposals;
          let editor = if text = "a" then editor else proposal in
          accepted := editor;
          (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input ~state:editor ~focused:true
            ~on_change:(fun state -> state)
            ());
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "ab";
      `Run
        (fun t ->
          equal ~msg:"second proposal starts from the accepted empty state"
            (list string) [ "a"; "b" ] (List.rev !proposals);
          equal ~msg:"model accepts only second key" string "b"
            (Mosaic.Editor.text !accepted);
          equal ~msg:"screen agrees with model" string "b"
            (String.trim (screen t)));
    ];
  [%expect {| |}]

let%expect_test "normalization is authoritative before the next key in a burst"
    =
  let proposals = ref [] in
  let accepted = ref Mosaic.Editor.empty in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun proposal _ ->
          let text = Mosaic.Editor.text proposal in
          proposals := text :: !proposals;
          let editor = Mosaic.Editor.of_string (String.uppercase_ascii text) in
          accepted := editor;
          (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input ~state:editor ~focused:true
            ~on_change:(fun state -> state)
            ());
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Feed "ab";
      `Run
        (fun t ->
          equal ~msg:"second proposal starts from normalized text" (list string)
            [ "a"; "Ab" ] (List.rev !proposals);
          equal ~msg:"model retains normalization" string "AB"
            (Mosaic.Editor.text !accepted);
          equal ~msg:"screen agrees with normalized model" string "AB"
            (String.trim (screen t)));
    ];
  [%expect {| |}]

let%expect_test "global capture updates focus and editor before default editing"
    =
  let accepted = ref (Mosaic.Editor.empty, Mosaic.Editor.empty) in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( (true, Mosaic.Editor.of_string "L", Mosaic.Editor.empty),
            Mosaic.Cmd.none ));
      update =
        (fun msg (left_focused, left, right) ->
          let model =
            match msg with
            | `Transfer -> (false, left, Mosaic.Editor.of_string "R")
            | `Left editor -> (left_focused, editor, right)
            | `Right editor -> (left_focused, left, editor)
          in
          let _, left, right = model in
          accepted := (left, right);
          (model, Mosaic.Cmd.none));
      view =
        (fun (left_focused, left, right) ->
          Mosaic.box ~flex_direction:Column
            [
              Mosaic.input ~state:left ~focused:left_focused
                ~on_change:(fun state -> `Left state)
                ();
              Mosaic.input ~state:right ~focused:(not left_focused)
                ~on_change:(fun state -> `Right state)
                ();
            ]);
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_key ~phase:Capture (fun ev ->
              if Mosaic.Shortcut.matches (Mosaic.Shortcut.char 'x') ev then
                Some `Transfer
              else None));
    }
  in
  drive ~width:20 ~height:2 application
    [
      `Feed "x";
      `Run
        (fun t ->
          let left, right = !accepted in
          equal ~msg:"previously focused editor is untouched" string "L"
            (Mosaic.Editor.text left);
          equal ~msg:"default action edits newly accepted target" string "Rx"
            (Mosaic.Editor.text right);
          equal ~msg:"both accepted editors are painted" string "L\nRx"
            (String.trim (screen t)));
    ];
  [%expect {| |}]

let%expect_test "non-key subscriptions accumulate in declaration order" =
  let observed = ref initial in
  let base =
    app
      ~subs:(fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_resize (fun ~width:_ ~height:_ -> Pressed "r1");
            Mosaic.Sub.on_resize (fun ~width:_ ~height:_ -> Pressed "r2");
            Mosaic.Sub.on_focus (Pressed "f1");
            Mosaic.Sub.on_focus (Pressed "f2");
            Mosaic.Sub.on_blur (Pressed "b1");
            Mosaic.Sub.on_blur (Pressed "b2");
            Mosaic.Sub.on_color_scheme (fun _ -> Some (Pressed "c1"));
            Mosaic.Sub.on_color_scheme (fun _ -> Some (Pressed "c2"));
          ])
      ()
  in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Run
        (fun _ ->
          equal ~msg:"startup geometry reaches both resize subscribers"
            (list string) [ "r1"; "r2" ] !observed.keys);
      `Resize (71, 1);
      `Feed (Matrix.Input.Decoder.encode_focus false);
      `Feed (Matrix.Input.Decoder.encode_focus true);
      `Feed (Matrix.Input.Reply.encode [ Color_scheme `Dark ]);
      `Run
        (fun t ->
          equal ~msg:"every subscriber sees its event in source order"
            (list string)
            [ "r1"; "r2"; "r1"; "r2"; "b1"; "b2"; "f1"; "f2"; "c1"; "c2" ]
            !observed.keys;
          contains ~msg:"screen reflects all delivered messages"
            ~sub:"keys:[r1,r2,r1,r2,b1,b2,f1,f2,c1,c2]" (screen t));
    ];
  [%expect {| |}]

let%expect_test
    "reordering timer IDs preserves independent equal-interval phases" =
  let observed = ref initial in
  let subs model =
    let a = Mosaic.Sub.every ~id:"a" 1.0 (fun _ -> Pressed "a") in
    let b = Mosaic.Sub.every ~id:"b" 1.0 (fun _ -> Pressed "b") in
    let timers =
      match model.note with "-" -> [ a ] | "added" -> [ a; b ] | _ -> [ b; a ]
    in
    Mosaic.Sub.batch
      (Mosaic.Sub.on_key (fun _ ->
           Some (Note (if model.note = "-" then "added" else "reordered")))
      :: timers)
  in
  let base = app ~subs () in
  let application =
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Step_to 1.0;
      `Step_to 1.4;
      `Feed "+";
      `Step_to 1.6;
      `Feed "+";
      `Step_to 2.0;
      `Run
        (fun _ ->
          equal ~msg:"reorder preserves first deadline" (list string)
            [ "a"; "a" ] !observed.keys);
      `Step_to 2.4;
      `Run
        (fun t ->
          equal ~msg:"newer timer keeps its later phase" (list string)
            [ "a"; "a"; "b" ] !observed.keys;
          contains ~msg:"screen records timer order" ~sub:"keys:[a,a,b]"
            (screen t));
    ];
  [%expect {| |}]

let%expect_test
    "pointer focus requests precede caret defaults and keyboard input" =
  let observed =
    ref (true, Mosaic.Editor.of_string "L", Mosaic.Editor.of_string "old")
  in
  let events = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (!observed, Mosaic.Cmd.none));
      update =
        (fun msg (left_focused, left, right) ->
          let model =
            match msg with
            | `Focus ->
                events := "focus" :: !events;
                (false, left, Mosaic.Editor.of_string "abcd")
            | `Left next -> (left_focused, next, right)
            | `Right next ->
                events := "edit" :: !events;
                (left_focused, left, next)
          in
          observed := model;
          (model, Mosaic.Cmd.none));
      view =
        (fun (left_focused, left, right) ->
          Mosaic.box ~flex_direction:Column
            [
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:left ~focused:left_focused
                ~on_change:(fun next -> `Left next)
                ();
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:right ~focused:(not left_focused)
                ~on_focus_request:`Focus
                ~on_change:(fun next -> `Right next)
                ();
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:2 application
    [
      `Feed "\027[<0;2;2M\027[<0;2;2mX";
      `Run
        (fun _ ->
          let left_focused, left, right = !observed in
          is_false left_focused;
          equal string "L" (Mosaic.Editor.text left);
          equal ~msg:"caret uses the source accepted during focus" string
            "aXbcd" (Mosaic.Editor.text right);
          equal int 2 (Mosaic.Editor.cursor right);
          equal ~msg:"focus accepted before any editor proposal" string "focus"
            (List.hd (List.rev !events)));
    ];
  [%expect {| |}]

let%expect_test "rejected or unmounted focus targets cannot edit or steal focus"
    =
  List.iter
    (fun unmount ->
      let right = Mosaic.Editor.of_string "right" in
      let observed = ref (true, Mosaic.Editor.of_string "L", right) in
      let requested = ref 0 and edited_right = ref 0 in
      let clicks = ref 0 in
      let application =
        {
          Mosaic.init = (fun () -> (!observed, Mosaic.Cmd.none));
          update =
            (fun msg (visible, left, right) ->
              let model =
                match msg with
                | `Focus ->
                    incr requested;
                    (not unmount, left, right)
                | `Left next -> (visible, next, right)
                | `Right next ->
                    incr edited_right;
                    (visible, left, next)
              in
              observed := model;
              (model, Mosaic.Cmd.none));
          view =
            (fun (visible, left, right) ->
              Mosaic.box ~flex_direction:Column
                [
                  Mosaic.input
                    ~size:(Mosaic.size ~width:20 ~height:1)
                    ~state:left ~focused:true
                    ~on_change:(fun next -> `Left next)
                    ();
                  (if visible then
                     Mosaic.input
                       ~size:(Mosaic.size ~width:20 ~height:1)
                       ~state:right ~focused:false ~on_focus_request:`Focus
                       ~on_click:(fun _ ->
                         incr clicks;
                         None)
                       ~on_change:(fun next -> `Right next)
                       ()
                   else Mosaic.empty);
                ]);
          subscriptions = (fun _ -> Mosaic.Sub.none);
        }
      in
      drive ~width:20 ~height:2 application
        [
          `Feed "\027[<0;2;2M\027[<0;2;2mX";
          `Run
            (fun _ ->
              let _, left, current_right = !observed in
              equal int 1 !requested;
              equal ~msg:"no stale default editor callback" int 0 !edited_right;
              equal ~msg:"rejected press does not click on release" int 0
                !clicks;
              equal string "LX" (Mosaic.Editor.text left);
              is_true ~msg:"unaccepted target remains unchanged"
                (current_right == right));
        ])
    [ false; true ];
  [%expect {| |}]

let%expect_test "memo and message mapping defer focus requests until input" =
  let mapped = ref 0 and delivered = ref 0 in
  let map_message message =
    incr mapped;
    message
  in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update =
        (fun message () ->
          (match message with
          | `Focus -> incr delivered
          | `Edited -> fail "unaccepted edit");
          ((), Mosaic.Cmd.none));
      view =
        (fun () ->
          Mosaic.memo ~key:() (fun () ->
              Mosaic.input
                ~size:(Mosaic.size ~width:20 ~height:1)
                ~state:Mosaic.Editor.empty ~focused:false
                ~on_focus_request:`Focus
                ~on_change:(fun _ -> `Edited)
                ())
          |> Mosaic.map map_message);
      subscriptions = (fun () -> Mosaic.Sub.none);
    }
  in
  drive ~width:20 ~height:1 application
    [
      `Run
        (fun _ ->
          equal int 0 !mapped;
          equal int 0 !delivered);
      `Feed "\027[<0;1;1M\027[<0;1;1m";
      `Run
        (fun _ ->
          equal int 1 !mapped;
          equal int 1 !delivered);
    ];
  [%expect {| |}]

(* Size reports settle by the depth rule: a response that resizes a node at
   the depth already reported waits for the frame's paint, so a layout that
   oscillates with its own size still presents every frame. *)
let%expect_test "an on_size response that resizes its own node presents frames"
    =
  let views = ref 0 and reports = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (true, Mosaic.Cmd.none));
      update =
        (fun (`Size w) _wide ->
          reports := w :: !reports;
          ( w < 30,
            if List.length !reports = 6 then Mosaic.Cmd.quit
            else Mosaic.Cmd.none ));
      view =
        (fun wide ->
          incr views;
          if !views > 1_000 then failwith "the size loop never presents";
          Mosaic.box
            [
              Mosaic.box
                ~size:(Mosaic.size ~width:(if wide then 40 else 20) ~height:1)
                ~on_size:(fun ~w ~h:_ -> Some (`Size w))
                [ Mosaic.text (if wide then "wide" else "narrow") ];
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  let screens = ref [] in
  (* A report a frame's layout delivers is applied after the frame, and the
     frame it asks for waits its interval: the clock moves for each. *)
  let t = terminal ~width:60 ~height:2 () in
  run t
    {
      application with
      update =
        (fun message model ->
          screens := String.trim (screen t) :: !screens;
          application.update message model);
    }
    (fun _ -> Matrix_test.advance t.s (ns 1.));
  Printf.printf "reports: %s\n"
    (String.concat " " (List.rev_map string_of_int !reports));
  Printf.printf "views per report: %d/%d\n" !views (List.length !reports);
  Printf.printf "presented before each report: %s\n"
    (String.concat " | " (List.rev !screens));
  [%expect
    {|
    reports: 40 20 40 20 40 20
    views per report: 7/6
    presented before each report:  |  | wide | wide | wide | narrow
    |}]

let%expect_test "on_size reports settle within the frame or the next one" =
  let application =
    {
      Mosaic.init = (fun () -> ((None, None, None), Mosaic.Cmd.none));
      update =
        (fun message (outer, inner, sibling) ->
          ( (match message with
            | `Outer w -> (Some w, inner, sibling)
            | `Inner w -> (outer, Some w, sibling)
            | `Sibling w -> (outer, inner, Some w)),
            Mosaic.Cmd.none ));
      view =
        (fun (outer, inner, sibling) ->
          let show = function None -> "-" | Some w -> string_of_int w in
          (* The inner width follows the outer report, one level deeper; the
             sibling's width follows the inner report at the outer's depth. *)
          let inner_width = Option.fold ~none:4 ~some:(fun w -> w / 2) outer in
          let sibling_width =
            Option.fold ~none:4 ~some:(fun w -> w + 1) inner
          in
          Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
            ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
            [
              Mosaic.box
                ~on_size:(fun ~w ~h:_ -> Some (`Outer w))
                [
                  Mosaic.box
                    ~size:(Mosaic.size ~width:inner_width ~height:1)
                    ~on_size:(fun ~w ~h:_ -> Some (`Inner w))
                    [];
                ];
              Mosaic.box
                ~size:(Mosaic.size ~width:sibling_width ~height:1)
                ~on_size:(fun ~w ~h:_ -> Some (`Sibling w))
                [
                  Mosaic.text
                    (Printf.sprintf "%s %s %s" (show outer) (show inner)
                       (show sibling));
                ];
            ]);
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  (* The frame after the one whose layout reported comes one frame interval
     later. *)
  drive ~width:30 ~height:3 application
    [ `Advance 0.017; `Snap; `Resize (40, 3); `Advance 0.017; `Snap ];
  [%expect {|
    |
    |30 15 16
    |
    |
    |40 20 21
    |
    |}]

let%expect_test "timer ids must be unique across every and on_tick" =
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.every ~id:"clock" 1. (fun k -> Ticks k);
        Mosaic.Sub.on_tick ~id:"clock" (fun ~dt:_ -> Tick);
      ]
  in
  (match drive (app ~subs ()) [] with
  | () -> print_endline "accepted"
  | exception Invalid_argument message -> print_endline message);
  [%expect {| Mosaic.Sub: empty or duplicate id: clock |}]

(* An application that records every message it receives, by name. *)
let recording subs =
  let observed = ref initial in
  let base = app ~subs () in
  ( observed,
    {
      base with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = update msg model in
          observed := model;
          (model, cmd));
    } )

let%expect_test "a paste is delivered whole and a cut paste is dropped" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.on_paste (fun ev ->
            Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev))))
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "\027[200~ab";
      `Feed "cd";
      `Run
        (fun _ ->
          equal ~msg:"nothing is delivered before the end marker" (list string)
            [] !observed.keys);
      `Feed "\027[201~";
      `Run
        (fun _ ->
          equal ~msg:"the pieces are delivered as one paste" (list string)
            [ "p:abcd" ] !observed.keys);
      `Feed "\027[200~he";
      `Feed "llo\027[201~";
      (* Text an input method composed, as the kitty protocol sends it. *)
      `Feed "\027[0;;233u";
      `Run
        (fun _ ->
          equal
            ~msg:
              "a bracketed paste split across writes arrives whole, and text \
               without a key is a paste"
            (list string)
            [ "p:abcd"; "p:hello"; "p:é" ]
            !observed.keys);
    ];
  [%expect {| |}]

let%expect_test "a stalled paste is reported, and the application aborts it" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_paste_stalled 2. (Pressed "<stalled>");
            Mosaic.Sub.on_paste (fun ev ->
                Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
            Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)));
          ])
  in
  let application =
    {
      application with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = application.Mosaic.update msg model in
          match msg with
          | Pressed "<stalled>" -> (model, Mosaic.Cmd.abort_paste)
          | _ -> (model, cmd));
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "\027[200~ab";
      `Step_to 1.;
      `Feed "c\003";
      `Step_to 2.5;
      `Run
        (fun _ ->
          equal ~msg:"data, Ctrl+C too, keeps the paste going" (list string) []
            !observed.keys);
      `Step_to 3.5;
      `Feed "x";
      `Step_to 8.;
      `Run
        (fun _ ->
          equal ~msg:"reported once, aborted and dropped, then typing"
            (list string) [ "<stalled>"; "x" ] !observed.keys);
    ];
  [%expect {| |}]

(* With no stall declared, the program ends a paste silent for 5 s, and
   the keys typed after it reach the program again. *)
let%expect_test "a stalled paste ends after 5 s by default" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_paste (fun ev ->
                Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
            Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)));
          ])
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "\027[200~ab";
      `Step_to 1.;
      `Feed "c";
      `Step_to 5.9;
      `Feed "q";
      `Step_to 10.8;
      `Run
        (fun _ ->
          equal ~msg:"data keeps the paste going" (list string) []
            !observed.keys);
      `Step_to 11.;
      `Feed "x";
      `Step_to 12.;
      `Run
        (fun _ ->
          equal ~msg:"ended and dropped, then typing" (list string) [ "x" ]
            !observed.keys);
    ];
  [%expect {| |}]

(* A stall the application declares replaces the default: the paste goes on
   past 5 s until its end marker. *)
let%expect_test "a declared stall replaces the default" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_paste_stalled 1. (Pressed "<stalled>");
            Mosaic.Sub.on_paste (fun ev ->
                Some (Pressed ("p:" ^ Mosaic.Event.Paste.text ev)));
          ])
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "\027[200~ab";
      `Step_to 8.;
      `Feed "c\027[201~";
      `Step_to 9.;
      `Run
        (fun _ ->
          equal ~msg:"reported, then whole" (list string)
            [ "<stalled>"; "p:abc" ] !observed.keys);
    ];
  [%expect {| |}]

let%expect_test "key releases reach on_key_release, not key handlers" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key)));
            Mosaic.Sub.on_key_release (fun release ->
                match release.key with
                | Char u when Uchar.is_char u ->
                    Some (Pressed ("r:" ^ String.make 1 (Uchar.to_char u)))
                | _ -> None);
          ])
  in
  let release : Matrix.Input.Keystroke.release =
    {
      key = Char (Uchar.of_char 'q');
      mods = Matrix.Input.Keystroke.no_mods;
      base = None;
      keypad = false;
    }
  in
  drive ~width:70 ~height:1 application
    [
      `Feed "q";
      (* As a terminal reports releases, under the kitty protocol's event
         types with every key a sequence. *)
      `Feed
        (Matrix.Input.Keystroke.encode_release
           { Matrix.Input.Keystroke.legacy with kitty = 0b1011 }
           release);
      `Run
        (fun _ ->
          equal ~msg:"the press reaches key handlers, the release does not"
            (list string) [ "q"; "r:q" ] !observed.keys);
    ];
  [%expect {| |}]

(* ── Event delivery ── *)

(* Two inputs, A focused above B, each accepting its proposals and focus
   requests, under [subs]. Returns the texts accepted after [bytes]. *)
let two_inputs ?(subs = fun _ -> Mosaic.Sub.none) bytes =
  let texts = ref ("", "") in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ((Mosaic.Editor.empty, Mosaic.Editor.empty, `A), Mosaic.Cmd.none));
      update =
        (fun msg (a, b, focus) ->
          let ((a, b, _) as model) =
            match msg with
            | `Edit_a a -> (a, b, focus)
            | `Edit_b b -> (a, b, focus)
            | `Focus focus -> (a, b, focus)
          in
          texts := (Mosaic.Editor.text a, Mosaic.Editor.text b);
          (model, Mosaic.Cmd.none));
      view =
        (fun (a, b, focus) ->
          let input state name edit =
            Mosaic.input
              ~size:(Mosaic.size ~width:20 ~height:1)
              ~state ~focused:(focus = name) ~on_focus_request:(`Focus name)
              ~on_change:edit ()
          in
          Mosaic.box ~flex_direction:Column
            [ input a `A (fun a -> `Edit_a a); input b `B (fun b -> `Edit_b b) ]);
      subscriptions = subs;
    }
  in
  drive ~width:20 ~height:2 application [ `Feed bytes ];
  !texts

let pair = pair string string

let%expect_test "K1: a capture listener that moves focus moves the typing too" =
  (* The elements' handlers keep the path taken; the insertion runs on the
     element focused when it runs. *)
  let subs _ =
    Mosaic.Sub.on_key ~phase:Capture (fun key ->
        if Mosaic.Shortcut.matches (Mosaic.Shortcut.char 'x') key then
          Some (`Focus `B)
        else None)
  in
  equal ~msg:"x lands in B" pair ("", "x") (two_inputs ~subs "x");
  [%expect {| |}]

let%expect_test "K2: a key that moves focus moves the next key of the read" =
  let subs _ = Mosaic.Sub.on_keys [ (Mosaic.Shortcut.tab, `Focus `B) ] in
  equal ~msg:"Tab types nothing, bubbles, and focuses B for x" pair ("", "x")
    (two_inputs ~subs "\tx");
  [%expect {| |}]

let%expect_test "K3: a click's focus request moves the next key of the read" =
  equal ~msg:"x goes to B, focused by the click" pair ("", "x")
    (two_inputs "\027[<0;3;2M\027[<0;3;2mx");
  [%expect {| |}]

let%expect_test "K4: a keyed reorder keeps focus for the next key of the read" =
  let final = ref [] in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( [
              ("k1", Mosaic.Editor.empty);
              ("k2", Mosaic.Editor.empty);
              ("k3", Mosaic.Editor.empty);
            ],
            Mosaic.Cmd.none ));
      update =
        (fun msg items ->
          let items =
            match msg with
            | `Reverse -> List.rev items
            | `Edit (key, editor) ->
                List.map
                  (fun (k, e) -> if k = key then (k, editor) else (k, e))
                  items
          in
          final := List.map (fun (k, e) -> (k, Mosaic.Editor.text e)) items;
          (items, Mosaic.Cmd.none));
      view =
        (fun items ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun (key, state) ->
                 Mosaic.input ~key
                   ~size:(Mosaic.size ~width:20 ~height:1)
                   ~state ~focused:(key = "k1")
                   ~on_change:(fun e -> `Edit (key, e))
                   ())
               items));
      subscriptions =
        (fun _ -> Mosaic.Sub.on_keys [ (Mosaic.Shortcut.ctrl 'r', `Reverse) ]);
    }
  in
  drive ~width:20 ~height:3 application [ `Feed "\018x" ];
  equal ~msg:"x goes to k1, wherever it moved" (list pair)
    [ ("k3", ""); ("k2", ""); ("k1", "x") ]
    !final;
  [%expect {| |}]

let%expect_test "K5: a click hits the layout the previous key left" =
  let clicked = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> ([ "r0"; "r1"; "r2" ], Mosaic.Cmd.none));
      update =
        (fun msg rows ->
          match msg with
          | `Delete -> (List.tl rows, Mosaic.Cmd.none)
          | `Click row ->
              clicked := row :: !clicked;
              (rows, Mosaic.Cmd.none));
      view =
        (fun rows ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun row ->
                 Mosaic.box ~key:row
                   ~size:(Mosaic.size ~width:10 ~height:1)
                   ~on_click:(fun _ -> Some (`Click row))
                   [ Mosaic.text row ])
               rows));
      subscriptions =
        (fun _ -> Mosaic.Sub.on_keys [ (Mosaic.Shortcut.char 'd', `Delete) ]);
    }
  in
  drive ~width:10 ~height:3 application [ `Feed "d\027[<0;2;1M\027[<0;2;1m" ];
  equal ~msg:"the click hits r1, now on the first row" (list string) [ "r1" ]
    !clicked;
  [%expect {| |}]

(* Messages by name, in the order [update] received them. *)
let names subs view bytes =
  let seen = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update =
        (fun name () ->
          seen := name :: !seen;
          ((), Mosaic.Cmd.none));
      view = (fun () -> view);
      subscriptions = (fun () -> subs);
    }
  in
  drive ~width:10 ~height:1 application [ `Feed bytes ];
  List.rev !seen

let downs name ev =
  match Mosaic.Event.Mouse.kind ev with Down _ -> Some name | _ -> None

let%expect_test
    "E1: a widget that stops a press hides it from bubble listeners only" =
  let view =
    Mosaic.box
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~on_mouse:(fun ev ->
        Mosaic.Event.Mouse.stop_propagation ev;
        downs "widget" ev)
      [ Mosaic.text "w" ]
  in
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_mouse ~phase:Capture (downs "capture-1");
        Mosaic.Sub.on_mouse (downs "bubble");
        Mosaic.Sub.on_mouse ~phase:Capture (downs "capture-2");
      ]
  in
  equal ~msg:"both capture listeners, then the widget" (list string)
    [ "capture-1"; "capture-2"; "widget" ]
    (names subs view "\027[<0;2;1M");
  [%expect {| |}]

let%expect_test
    "E2: a capture listener that stops a key hides it from no other capture \
     listener" =
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_key ~phase:Capture (fun key ->
            Mosaic.Event.Key.stop_propagation key;
            Some "stops");
        Mosaic.Sub.on_key (fun _ -> Some "bubble");
        Mosaic.Sub.on_key ~phase:Capture (fun _ -> Some "sees");
      ]
  in
  equal ~msg:"the application is one node" (list string) [ "stops"; "sees" ]
    (names subs (Mosaic.text "e2") "z");
  [%expect {| |}]

let%expect_test "E3: listeners run by phase, then in declaration order" =
  let subs =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.on_key (fun _ -> Some "a");
        Mosaic.Sub.map
          (fun s -> "map:" ^ s)
          (Mosaic.Sub.batch
             [
               Mosaic.Sub.on_key (fun _ -> Some "b");
               Mosaic.Sub.on_key ~phase:Capture (fun _ -> Some "c");
             ]);
        Mosaic.Sub.on_key (fun _ -> Some "d");
      ]
  in
  equal
    ~msg:"capture first, each phase in declaration order through batch and map"
    (list string)
    [ "map:c"; "a"; "map:b"; "d" ]
    (names subs (Mosaic.text "e3") "k");
  [%expect {| |}]

let%expect_test "S1: a shortcut does not take a key typed into an input" =
  let text = ref "" and quit = ref 0 in
  let application =
    {
      Mosaic.init = (fun () -> (Mosaic.Editor.empty, Mosaic.Cmd.none));
      update =
        (fun msg editor ->
          match msg with
          | `Quit ->
              incr quit;
              (editor, Mosaic.Cmd.none)
          | `Edit editor ->
              text := Mosaic.Editor.text editor;
              (editor, Mosaic.Cmd.none));
      view =
        (fun editor ->
          Mosaic.input
            ~size:(Mosaic.size ~width:20 ~height:1)
            ~state:editor ~focused:true
            ~on_change:(fun e -> `Edit e)
            ());
      subscriptions =
        (fun _ ->
          Mosaic.Sub.on_keys
            [
              (Mosaic.Shortcut.char 'q', `Quit);
              (Mosaic.Shortcut.ctrl 'q', `Quit);
            ]);
    }
  in
  drive ~width:20 ~height:1 application [ `Feed "aq" ];
  equal ~msg:"q is typed" string "aq" !text;
  equal ~msg:"q does not quit" int 0 !quit;
  drive ~width:20 ~height:1 application [ `Feed "\017" ];
  equal ~msg:"Ctrl+Q types nothing, so it bubbles to the shortcut" int 1 !quit;
  [%expect {| |}]

let%expect_test "removing the focused element leaves nothing focused" =
  (* The update that removes the focused input does not move focus: keys
     then reach the application only, until the model declares another. *)
  let keys = ref [] and inputs = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (([ "a"; "b" ], Some "a"), Mosaic.Cmd.none));
      update =
        (fun msg (rows, focus) ->
          match msg with
          | `Remove ->
              (List.filter (fun r -> Some r <> focus) rows, focus)
              |> fun model -> (model, Mosaic.Cmd.none)
          | `Focus row -> ((rows, Some row), Mosaic.Cmd.none)
          | `Key name ->
              keys := name :: !keys;
              ((rows, focus), Mosaic.Cmd.none)
          | `Edit (row, text) ->
              inputs := (row, text) :: !inputs;
              ((rows, focus), Mosaic.Cmd.none));
      view =
        (fun (rows, focus) ->
          Mosaic.box ~flex_direction:Column
            (List.map
               (fun row ->
                 Mosaic.input ~key:row
                   ~size:(Mosaic.size ~width:20 ~height:1)
                   ~state:Mosaic.Editor.empty ~focused:(focus = Some row)
                   ~on_change:(fun e -> `Edit (row, Mosaic.Editor.text e))
                   ())
               rows));
      subscriptions =
        (fun _ ->
          Mosaic.Sub.batch
            [
              Mosaic.Sub.on_keys
                [
                  (Mosaic.Shortcut.f 2, `Remove);
                  (Mosaic.Shortcut.f 3, `Focus "b");
                ];
              Mosaic.Sub.on_key (fun key ->
                  match key_name key with
                  | "<other>" -> None
                  | name -> Some (`Key name));
            ]);
    }
  in
  drive ~width:20 ~height:2 application [ `Feed "\027OQx\027OR" ];
  equal ~msg:"x reached the application, no element" (list string) [ "x" ] !keys;
  equal ~msg:"no input took x" int 0 (List.length !inputs);
  drive ~width:20 ~height:2 application [ `Feed "\027OQ\027ORy" ];
  equal ~msg:"declaring b focused gives it the next key" (list pair)
    [ ("b", "y") ]
    !inputs;
  [%expect {| |}]

let%expect_test "a bubble listener sees a prevented key; a shortcut does not" =
  (* The input's own handler prevents z without stopping it: the input does
     not type it, a bubble listener sees it and can read the flag, and a
     shortcut, a default action of the key, does not fire. *)
  let seen = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update =
        (fun name () ->
          seen := name :: !seen;
          ((), Mosaic.Cmd.none));
      view =
        (fun () ->
          Mosaic.input
            ~size:(Mosaic.size ~width:20 ~height:1)
            ~state:Mosaic.Editor.empty ~focused:true
            ~on_key:(fun key ->
              Mosaic.Event.Key.prevent_default key;
              None)
            ~on_change:(fun _ -> "edit")
            ());
      subscriptions =
        (fun () ->
          Mosaic.Sub.batch
            [
              Mosaic.Sub.on_keys [ (Mosaic.Shortcut.char 'z', "shortcut") ];
              Mosaic.Sub.on_key (fun key ->
                  Some
                    (if Mosaic.Event.Key.default_prevented key then "prevented"
                     else "unused"));
            ]);
    }
  in
  drive ~width:20 ~height:1 application [ `Feed "z" ];
  equal ~msg:"only the plain listener, told the key was used" (list string)
    [ "prevented" ] (List.rev !seen);
  [%expect {| |}]

(* An exception in [init], which runs before the terminal is taken, or in
   the first subscriptions, which run after, leaves Mosaic.run with the
   terminal given back. *)
let%expect_test
    "exceptions in init and the first subscriptions give the terminal back" =
  (* What the run wrote: nothing when it never took the terminal, and the
     alternate screen left when it gave it back. *)
  let raises ~msg application =
    let t = terminal ~profile:Matrix_test.Profile.vte ~width:20 ~height:1 () in
    (match run t application ignore with
    | () -> fail (msg ^ ": no exception")
    | exception Failure m -> equal ~msg string msg m);
    is_false ~msg:"cooked mode" (Matrix_test.Terminal.raw t.term);
    t
  in
  let base = app () in
  let t =
    raises ~msg:"init" { base with Mosaic.init = (fun () -> failwith "init") }
  in
  equal ~msg:"the terminal was never taken" string "" (written t);
  let t =
    raises ~msg:"subscriptions"
      { base with Mosaic.subscriptions = (fun _ -> failwith "subscriptions") }
  in
  is_true ~msg:"the terminal was taken" (written t <> "");
  is_false ~msg:"and given back"
    (Matrix_test.Terminal.modes t.term).alternate_screen;
  [%expect {| |}]

(* Ctrl+C quits unless a handler prevented it. *)
let%expect_test "an unprevented Ctrl+C quits" =
  let run ~prevent =
    let observed, application =
      recording (fun _ ->
          Mosaic.Sub.on_key ~phase:Capture (fun key ->
              if prevent then Mosaic.Event.Key.prevent_default key;
              Some (Pressed (key_name key))))
    in
    drive ~width:70 ~height:1 application [ `Feed "a\003b" ];
    !observed.keys
  in
  equal ~msg:"quits at Ctrl+C" (list string) [ "a"; "c" ] (run ~prevent:false);
  equal ~msg:"prevented, the run goes on" (list string) [ "a"; "c"; "b" ]
    (run ~prevent:true);
  [%expect {| |}]

(* Time. An update that moves the virtual clock stands for one that takes
   that long to run: the loop cannot run meanwhile, so every deadline in
   between is due at once when it returns. *)

let current = ref None

(* The clock moves while the update runs: the loop takes its next step at
   the later time. *)
let stall seconds =
  let t = Option.get !current in
  Matrix_test.set_time t.s (elapsed t + ns seconds)

let clock () = elapsed (Option.get !current)

type timed = Stall | Fired of string * int

let timed_app ~subs ~log =
  {
    Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
    update =
      (fun msg views ->
        (match msg with
        | Stall -> stall 1.0
        | Fired (id, periods) -> log := (id, periods, clock ()) :: !log);
        (views, Mosaic.Cmd.none));
    view = (fun _ -> Mosaic.text "timers");
    subscriptions = subs;
  }

(* Runs [application], the clock moving through each deadline, frames
   included, until [seconds]. *)
let run_until seconds application =
  let t = terminal ~width:20 ~height:1 () in
  current := Some t;
  run t application (fun _ -> Matrix_test.advance t.s (ns seconds))

let on_s msg =
  Mosaic.Sub.on_key (fun key -> if key_name key = "s" then Some msg else None)

let print_fires log =
  List.rev log
  |> List.iter (fun (id, periods, at) ->
      Printf.printf "%s x%d at %d ms\n" id periods (at / 1_000_000))

let%expect_test "T1: timers due together fire once each, in deadline order" =
  let log = ref [] in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.every ~id:"A" 0.1 (fun k -> Fired ("A", k));
        Mosaic.Sub.every ~id:"B" 0.25 (fun k -> Fired ("B", k));
        on_s Stall;
      ]
  in
  drive (timed_app ~subs ~log)
    [
      `Run (fun t -> current := Some t); `Advance 0.05; `Feed "s"; `Step_to 1.5;
    ];
  print_fires !log;
  [%expect
    {|
    A x10 at 1050 ms
    B x4 at 1050 ms
    A x1 at 1100 ms
    A x1 at 1200 ms
    B x1 at 1250 ms
    A x1 at 1300 ms
    A x1 at 1400 ms
    A x1 at 1500 ms
    B x1 at 1500 ms
    |}]

let%expect_test "T2: timers keep their grid over 600 s of frames" =
  let fires = Hashtbl.create 2 and error = ref 0 in
  let period = function "A" -> ns 1.0 | _ -> ns 0.1 in
  let log = ref [] in
  let subs _ =
    Mosaic.Sub.batch
      [
        Mosaic.Sub.every ~id:"A" 1.0 (fun k -> Fired ("A", k));
        Mosaic.Sub.every ~id:"B" 0.1 (fun k -> Fired ("B", k));
        Mosaic.Sub.on_tick ~id:"frames" (fun ~dt:_ -> Stall);
      ]
  in
  let application =
    {
      (timed_app ~subs ~log) with
      update =
        (fun msg views ->
          (match msg with
          | Fired (id, periods) ->
              let n =
                periods + Option.value ~default:0 (Hashtbl.find_opt fires id)
              in
              Hashtbl.replace fires id n;
              error := max !error (abs (clock () - (n * period id)))
          | Stall -> ());
          (views, Mosaic.Cmd.none));
    }
  in
  run_until 600. application;
  Printf.printf "A %d, B %d, largest error %d ns\n" (Hashtbl.find fires "A")
    (Hashtbl.find fires "B") !error;
  [%expect {| A 600, B 6000, largest error 0 ns |}]

let%expect_test "T3: a timer that missed periods fires once with their count" =
  let log = ref [] and views = ref 0 in
  let subs _ =
    Mosaic.Sub.batch
      [ Mosaic.Sub.every ~id:"A" 0.1 (fun k -> Fired ("A", k)); on_s Stall ]
  in
  let application =
    {
      (timed_app ~subs ~log) with
      update =
        (fun msg views ->
          (match msg with
          | Stall -> stall 10.
          | Fired (id, periods) -> log := (id, periods, clock ()) :: !log);
          (views + 1, Mosaic.Cmd.none));
      view =
        (fun _ ->
          incr views;
          Mosaic.text "timers");
    }
  in
  let before = ref 0 in
  drive application
    [
      `Run (fun t -> current := Some t);
      `Advance 0.05;
      `Run (fun _ -> before := !views);
      `Feed "s";
      `Run
        (fun _ ->
          Printf.printf "views after the stall: %d\n" (!views - !before));
    ];
  print_fires !log;
  [%expect {|
    views after the stall: 2
    A x100 at 10050 ms
    |}]

let%expect_test "after fires once, and a new id restarts it, as a debounce" =
  (* Each key restarts a 0.3 s wait by declaring it under a new id: the
     message comes 0.3 s after the last key, once: the frames the keys
     bring take no time, so the last key comes at 400 ms. *)
  let log = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
      update =
        (fun msg keys ->
          match msg with
          | Stall -> (keys + 1, Mosaic.Cmd.none)
          | Fired (id, _) ->
              log := (id, 1, clock ()) :: !log;
              (keys, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "after");
      subscriptions =
        (fun keys ->
          let id = Printf.sprintf "wait-%d" keys in
          Mosaic.Sub.batch
            [
              on_s Stall;
              (if keys > 0 then Mosaic.Sub.after ~id 0.3 (Fired (id, 1))
               else Mosaic.Sub.none);
            ]);
    }
  in
  drive application
    [
      `Run (fun t -> current := Some t);
      `Feed "s";
      `Advance 0.2;
      `Feed "s";
      `Advance 0.2;
      `Feed "s";
      `Step_to 2.0;
    ];
  print_fires !log;
  [%expect {| wait-3 x1 at 700 ms |}]

let%expect_test
    "after is cancelled by dropping its id, and fires again on return" =
  let log = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (false, Mosaic.Cmd.none));
      update =
        (fun msg on ->
          match msg with
          | Stall -> (not on, Mosaic.Cmd.none)
          | Fired (id, _) ->
              log := (id, 1, clock ()) :: !log;
              (on, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "after");
      subscriptions =
        (fun on ->
          Mosaic.Sub.batch
            [
              on_s Stall;
              (if on then Mosaic.Sub.after ~id:"once" 0.5 (Fired ("once", 1))
               else Mosaic.Sub.none);
            ]);
    }
  in
  drive application
    [
      `Run (fun t -> current := Some t);
      `Feed "s" (* declared at 0 *);
      `Advance 0.4;
      `Feed "s" (* dropped at 0.4: cancelled *);
      `Step_to 1.0;
      `Feed "s" (* declared again at 1.0 *);
      `Step_to 3.0 (* fires at 1.5, then stays inert *);
    ];
  print_fires !log;
  [%expect {| once x1 at 1500 ms |}]

let%expect_test "T4: a tick's first dt is 0, after any idle time" =
  let dts = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (false, Mosaic.Cmd.none));
      update =
        (fun msg ticking ->
          match msg with
          | `Start -> (true, Mosaic.Cmd.none)
          | `Tick dt ->
              dts := dt :: !dts;
              (ticking, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "ticks");
      subscriptions =
        (fun ticking ->
          Mosaic.Sub.batch
            [
              on_s `Start;
              (if ticking then Mosaic.Sub.on_tick ~id:"T" (fun ~dt -> `Tick dt)
               else Mosaic.Sub.none);
            ]);
    }
  in
  let t = terminal ~width:20 ~height:1 () in
  run t application (fun _ ->
      Matrix_test.advance t.s (ns 10.);
      feed t "s";
      Matrix_test.advance t.s (ns 0.05));
  List.rev !dts
  |> List.iteri (fun i dt -> if i < 3 then Printf.printf "dt %.4f\n" dt);
  [%expect {|
    dt 0.0000
    dt 0.0167
    dt 0.0167
    |}]

let%expect_test "T5: a tick fires once per frame, whatever else wakes the loop"
    =
  (* A frame the timer's message asks for is drawn when asked, on a terminal
     in the process: 200 of them, and the 59 of the tick's own interval, each
     with one tick. *)
  let ticks = ref 0 and total = ref 0. in
  let application =
    {
      Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
      update =
        (fun msg n ->
          match msg with
          | `Tick dt ->
              incr ticks;
              total := !total +. dt;
              (n, Mosaic.Cmd.none)
          | `Every k -> (n + k, Mosaic.Cmd.none));
      view = (fun n -> Mosaic.text (string_of_int n));
      subscriptions =
        (fun _ ->
          Mosaic.Sub.batch
            [
              Mosaic.Sub.on_tick ~id:"T" (fun ~dt -> `Tick dt);
              Mosaic.Sub.every ~id:"E" 0.005 (fun k -> `Every k);
            ]);
    }
  in
  run_until 1.0 application;
  Printf.printf "%d ticks, %.3f s of dt\n" !ticks !total;
  [%expect {| 260 ticks, 1.000 s of dt |}]

(* Work done off the loop. *)

let%expect_test "frames keep coming while posted messages are applied" =
  (* A worker posts 1,000 messages whose updates take 1 ms each. A message
     is applied when the loop takes it, inside the turn's budget, so a frame
     comes about every frame interval; applied at the next frame, the whole
     backlog ran inside one frame, a second without one. *)
  let views = ref [] in
  let application =
    {
      Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
      update =
        (fun msg n ->
          match msg with
          | `Go ->
              ( n,
                Mosaic.Cmd.perform (fun dispatch ->
                    for _ = 1 to 1000 do
                      dispatch `Work
                    done) )
          | `Work ->
              stall 0.001;
              (n + 1, Mosaic.Cmd.none));
      view =
        (fun n ->
          views := (n, clock ()) :: !views;
          Mosaic.text (string_of_int n));
      subscriptions = (fun _ -> on_s `Go);
    }
  in
  let t = terminal ~width:20 ~height:1 () in
  current := Some t;
  run t ~process_perform:(fun work -> work ()) application (fun _ -> feed t "s");
  let views = List.rev !views in
  let gap =
    List.fold_left
      (fun (gap, last) (_, at) -> (max gap (at - last), at))
      (0, 0) views
    |> fst
  in
  Printf.printf "applied %d; %d views; longest gap %d ms\n"
    (fst (List.hd (List.rev views)))
    (List.length views) (gap / 1_000_000);
  [%expect {| applied 1000; 60 views; longest gap 17 ms |}]

(* The run's standard error, captured: an exception off the loop must not be
   written over the screen. *)
let capturing_stderr f =
  let file = Filename.temp_file "mosaic" ".err" in
  let fd = Unix.openfile file [ O_WRONLY; O_TRUNC ] 0o600 in
  let saved = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr;
      Unix.close saved;
      Unix.close fd)
    f;
  let err = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  err

let%expect_test "X2: an exception in a perform ends the run" =
  Printexc.record_backtrace true;
  let application =
    app
      ~init_cmd:(Mosaic.Cmd.perform (fun _ -> failwith "perform-thread"))
      ~subs:(fun _ -> Mosaic.Sub.every ~id:"e" 0.1 (fun k -> Ticks k))
      ()
  in
  (* The perform runs on the session's queue, and the run ends at the step
     that takes its exception. *)
  let t = terminal ~profile:Matrix_test.Profile.vte ~width:20 ~height:1 () in
  let err =
    capturing_stderr (fun () ->
        match run t application ignore with
        | () -> print_endline "the run ended without the exception"
        | exception Failure m ->
            let bt = Printexc.get_backtrace () in
            let names_the_raise =
              List.exists
                (fun line ->
                  String.starts_with ~prefix:"Raised at Stdlib.failwith" line)
                (String.split_on_char '\n' bt)
            in
            Printf.printf "raised %S, from the worker's raise: %b\n" m
              names_the_raise)
  in
  Printf.printf "terminal given back: %b; standard error: %S\n"
    (not
       (Matrix_test.Terminal.raw t.term
       || (Matrix_test.Terminal.modes t.term).alternate_screen))
    err;
  [%expect
    {|
    raised "perform-thread", from the worker's raise: true
    terminal given back: true; standard error: ""
    |}]

(* Workers run as long as the program declares them, so they cannot run on
   the session's queue, whose work must return: they run on the program's
   own executor, threads unless a test says otherwise, and the test waits for what they do,
   settling as their messages come. A minute a step guards against what
   never happens; it measures nothing, so a loaded host only makes a test
   slower. *)
type live_step = Until of (unit -> bool) | Type of string | End

let run_live ?process_perform steps application =
  let t = terminal ~width:20 ~height:1 () in
  let process_perform = Option.value process_perform ~default:threads in
  Mosaic_test.run ~config:(base ()) ~process_perform ~quiesce:ignore t.term
    application
  @@ fun s ->
  let rec go = function
    | [] | End :: _ -> ()
    | Type bytes :: rest ->
        Matrix_test.input s bytes;
        go rest
    | Until ready :: rest ->
        let since = Unix.gettimeofday () in
        let rec wait () =
          Matrix_test.settle s;
          if ready () then true
          else if Unix.gettimeofday () -. since > 60. then (
            print_endline "timed out";
            false)
          else (
            Thread.delay 0.001;
            wait ())
        in
        if wait () then go rest
  in
  go steps

let locked = Mutex.create ()
let record log s = Mutex.protect locked (fun () -> log := s :: !log)
let read log = Mutex.protect locked (fun () -> List.rev !log)

let%expect_test "a worker runs while declared and stops when dropped" =
  let log = ref [] and received = Atomic.make 0 and late = Atomic.make 0 in
  let ended = Atomic.make false in
  let worker stop (mailbox : _ Mosaic.mailbox) =
    Mosaic.Sub.Stop.on_request stop (fun () -> record log "stop requested");
    while not (Mosaic.Sub.Stop.requested stop) do
      mailbox.dispatch `Tick;
      Thread.delay 0.001
    done;
    (* Dispatching after the stop does nothing. *)
    mailbox.dispatch `Tick;
    Atomic.set ended true
  in
  let application =
    {
      Mosaic.init = (fun () -> (true, Mosaic.Cmd.none));
      update =
        (fun msg on ->
          match msg with
          | `Toggle ->
              record log (if on then "dropped" else "declared");
              (not on, Mosaic.Cmd.none)
          | `Tick ->
              if on then Atomic.incr received else Atomic.incr late;
              (on, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "worker");
      subscriptions =
        (fun on ->
          Mosaic.Sub.batch
            [
              on_s `Toggle;
              (if on then Mosaic.Sub.worker ~id:"w" worker else Mosaic.Sub.none);
            ]);
    }
  in
  run_live
    [
      Until (fun () -> Atomic.get received >= 5);
      Type "s";
      Until (fun () -> Atomic.get ended);
      End;
    ]
    application;
  List.iter print_endline (read log);
  Printf.printf "messages after the drop: %d\n" (Atomic.get late);
  [%expect
    {|
    dropped
    stop requested
    messages after the drop: 0
    |}]

let%expect_test "send returns once update has applied the message" =
  let log = ref [] in
  let worker _ (mailbox : _ Mosaic.mailbox) =
    for i = 1 to 3 do
      mailbox.send i;
      record log (Printf.sprintf "send %d returned" i)
    done
  in
  let application =
    {
      Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
      update =
        (fun i _ ->
          (* The worker cannot return from [send] while [update] runs. *)
          Thread.delay 0.005;
          record log (Printf.sprintf "applied %d" i);
          (i, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "send");
      subscriptions = (fun _ -> Mosaic.Sub.worker ~id:"w" worker);
    }
  in
  run_live [ Until (fun () -> List.length (read log) = 6); End ] application;
  List.iter print_endline (read log);
  [%expect
    {|
    applied 1
    send 1 returned
    applied 2
    send 2 returned
    applied 3
    send 3 returned
    |}]

(* A thread the program did not start gives it messages with the dispatch
   a perform handed it, after the perform has returned. *)
let%expect_test "a perform's dispatch serves a thread after it returned" =
  let log = ref [] and returned = Atomic.make false in
  let thread = Atomic.make None in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( 0,
            Mosaic.Cmd.perform (fun dispatch ->
                let feed () =
                  while not (Atomic.get returned) do
                    Thread.yield ()
                  done;
                  Thread.delay 0.01;
                  dispatch 1;
                  dispatch 2
                in
                Atomic.set thread (Some (Thread.create feed ()));
                Atomic.set returned true) ));
      update =
        (fun i _ ->
          record log (Printf.sprintf "applied %d" i);
          (i, Mosaic.Cmd.none));
      view = (fun _ -> Mosaic.text "feed");
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  run_live [ Until (fun () -> List.length (read log) = 2); End ] application;
  Option.iter Thread.join (Atomic.get thread);
  List.iter print_endline (read log);
  [%expect {|
    applied 1
    applied 2
    |}]

let%expect_test "send on the loop's thread raises" =
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update = (fun () () -> ((), Mosaic.Cmd.none));
      view = (fun () -> Mosaic.text "send");
      subscriptions =
        (fun () ->
          Mosaic.Sub.worker ~id:"w" (fun _ (mailbox : _ Mosaic.mailbox) ->
              mailbox.send ()));
    }
  in
  (match
     run_live ~process_perform:(fun work -> work ()) [ End ] application
   with
  | () -> print_endline "no exception"
  | exception Invalid_argument m -> print_endline m);
  [%expect {| Mosaic: send on the loop's thread |}]

let%expect_test
    "a worker's exception ends the run, unless its stop was requested" =
  let failing ~after_stop stop _ =
    let unblocked = Atomic.make false in
    Mosaic.Sub.Stop.on_request stop (fun () -> Atomic.set unblocked true);
    if after_stop then
      while not (Atomic.get unblocked) do
        Thread.delay 0.001
      done;
    failwith "worker"
  in
  let run ~after_stop =
    let application =
      {
        Mosaic.init = (fun () -> (true, Mosaic.Cmd.none));
        update = (fun `Toggle on -> (not on, Mosaic.Cmd.none));
        view = (fun _ -> Mosaic.text "worker");
        subscriptions =
          (fun on ->
            Mosaic.Sub.batch
              [
                on_s `Toggle;
                (if on then Mosaic.Sub.worker ~id:"w" (failing ~after_stop)
                 else Mosaic.Sub.none);
              ]);
      }
    in
    let steps =
      if after_stop then
        [
          Type "s";
          Until
            (fun () ->
              Thread.delay 0.05;
              true);
          End;
        ]
      else [ Until (fun () -> false) ]
    in
    match run_live steps application with
    | () -> print_endline "the run ended normally"
    | exception Failure m -> Printf.printf "the run raised %S\n" m
  in
  run ~after_stop:false;
  run ~after_stop:true;
  [%expect {|
    the run raised "worker"
    the run ended normally
    |}]

let%expect_test "the run's end requests every worker's stop" =
  let stopped = Atomic.make false and started = Atomic.make false in
  let application =
    {
      Mosaic.init = (fun () -> ((), Mosaic.Cmd.none));
      update = (fun () () -> ((), Mosaic.Cmd.none));
      view = (fun () -> Mosaic.text "worker");
      subscriptions =
        (fun () ->
          Mosaic.Sub.worker ~id:"w" (fun stop _ ->
              Atomic.set started true;
              while not (Mosaic.Sub.Stop.requested stop) do
                Thread.delay 0.001
              done;
              Atomic.set stopped true));
    }
  in
  run_live [ Until (fun () -> Atomic.get started); End ] application;
  let deadline = Unix.gettimeofday () +. 60. in
  while (not (Atomic.get stopped)) && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  Printf.printf "stopped: %b\n" (Atomic.get stopped);
  [%expect {| stopped: true |}]

let%expect_test
    "exec gives the terminal to a function and dispatches its result" =
  (* The key typed after [e] in the same read was typed for the child: the
     release drops it. *)
  let ran = ref 0 in
  let application =
    {
      (app ()) with
      Mosaic.update =
        (fun msg model ->
          match msg with
          | Pressed "e" ->
              ( { model with keys = model.keys @ [ "e" ] },
                Mosaic.Cmd.exec (fun () ->
                    incr ran;
                    Note "back") )
          | msg -> update msg model);
      subscriptions = on_keys;
    }
  in
  drive ~width:40 ~height:1 application [ `Feed "ex"; `Snap ];
  Printf.printf "ran %d time\n" !ran;
  [%expect {|
    |keys:[e] ticks:0 size:- note:back
    ran 1 time
    |}]

let%expect_test "a posted message that changes nothing asks for no frame" =
  (* The frames the run begins count what it was asked for: a frame asked
     for by a message is drawn when the loop settles. *)
  let dispatch = ref ignore in
  let application =
    app ~init_cmd:(Mosaic.Cmd.perform (fun d -> dispatch := d)) ()
  in
  let t = terminal ~width:40 ~height:1 () in
  run t
    ~process_perform:(fun work -> work ())
    application
    (fun _ ->
      let frames = Matrix_test.frames t.s in
      List.iter !dispatch [ Ignored; Ignored; Ignored ];
      Matrix_test.settle t.s;
      Printf.printf "frames after messages that change nothing: %d\n"
        (Matrix_test.frames t.s - frames);
      !dispatch (Note "changed");
      Matrix_test.settle t.s;
      Printf.printf "after one that changes the model: %d, %s\n"
        (Matrix_test.frames t.s - frames)
        (match Matrix_test.deadline t.s with
        | None -> "no frame owed"
        | Some _ -> "a frame owed");
      Printf.printf "on the screen: %s\n" (String.trim (screen t)));
  [%expect
    {|
    frames after messages that change nothing: 0
    after one that changes the model: 1, no frame owed
    on the screen: keys:[] ticks:0 size:- note:changed
    |}]

(* Cost and order. *)

let%expect_test "C1: a read of 1,000 keys is 1,000 updates, painted each frame"
    =
  (* Each update takes 0.1 ms: the burst takes 100 ms, and a frame is
     painted each frame interval of it. A frame is one write. *)
  let updates = ref 0 and views = ref 0 and writes = ref 0 in
  let counting = ref false in
  let application =
    {
      Mosaic.init = (fun () -> (0, Mosaic.Cmd.none));
      update =
        (fun () n ->
          incr updates;
          stall 0.0001;
          (n + 1, Mosaic.Cmd.none));
      view =
        (fun n ->
          incr views;
          Mosaic.text (string_of_int n));
      subscriptions = (fun _ -> Mosaic.Sub.on_key (fun _ -> Some ()));
    }
  in
  let mark = ref 0 in
  let t = terminal ~width:20 ~height:1 () in
  current := Some t;
  run t application (fun _ ->
      counting := true;
      views := 0;
      mark := String.length (written t);
      feed t (String.make 1000 'a'));
  (* Every frame written begins by hiding the cursor. *)
  writes := occurrences ~from:!mark (written t) "\027[?25l";
  Printf.printf "%d updates, %d views, %d paints in %d ms\n" !updates !views
    !writes
    (elapsed t / 1_000_000);
  [%expect {| 1000 updates, 1000 views, 6 paints in 100 ms |}]

let%expect_test "C2: quit ends the run; keys read after it are dropped" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.on_key (fun key -> Some (Pressed (key_name key))))
  in
  let application =
    {
      application with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = application.Mosaic.update msg model in
          (model, if msg = Pressed "q" then Mosaic.Cmd.quit else cmd));
    }
  in
  drive ~width:70 ~height:1 application [ `Feed "aqbc" ];
  print_endline (String.concat "," !observed.keys);
  [%expect {| a,q |}]

(* Nothing after the update that quits is applied, even a message the
   same key gives a later listener. *)
let%expect_test "C2: a message after the quitting update is dropped" =
  let observed, application =
    recording (fun _ ->
        Mosaic.Sub.batch
          [
            Mosaic.Sub.on_key ~phase:Capture (fun key ->
                Some (Pressed (key_name key)));
            Mosaic.Sub.on_key ~phase:Bubble (fun _ -> Some (Note "after"));
          ])
  in
  let application =
    {
      application with
      Mosaic.update =
        (fun msg model ->
          let model, cmd = application.Mosaic.update msg model in
          (model, if msg = Pressed "q" then Mosaic.Cmd.quit else cmd));
    }
  in
  drive ~width:70 ~height:1 application [ `Feed "q" ];
  print_endline !observed.note;
  [%expect {| - |}]

let%expect_test "C3: a batch starts its commands in list order, depth first" =
  let order = ref [] in
  let started name = Mosaic.Cmd.perform (fun _ -> order := name :: !order) in
  let application =
    app
      ~init_cmd:
        (Mosaic.Cmd.batch
           [
             started "p1";
             Mosaic.Cmd.batch [ started "p2"; started "p3" ];
             Mosaic.Cmd.set_title "title";
             started "p4";
           ])
      ()
  in
  drive ~process_perform:(fun work -> work ()) application [];
  print_endline (String.concat "," (List.rev !order));
  [%expect {| p1,p2,p3,p4 |}]

(* Timers against a reference model. A case declares random sets of
   timers (ids from a pool, periods from 1 ms to 10 s, a period changed now
   and then, timers dropped) at random times, the clock jumping by up to a
   minute between them as when the loop is stalled. The reference fires, at
   each time the loop ran, every timer whose grid point it reached, once,
   with the periods that ended, in the order of their grid points then of
   their declaration; a timer kept keeps its grid, a changed period starts a
   new one, and a dropped one never fires again. *)

let reference_timers events =
  (* Timers: id, period, origin, fired. *)
  let timers = ref [] and fires = ref [] in
  let fire_due t =
    List.mapi
      (fun i (id, p, o, f) -> (o + ((f + 1) * p), i, id, p, o, f))
      !timers
    |> List.filter (fun (deadline, _, _, _, _, _) -> deadline <= t)
    |> List.sort compare
    |> List.iter (fun (_, _, id, p, o, f) ->
        let ended = (t - o) / p in
        fires := (t, id, ended - f) :: !fires;
        timers :=
          List.map
            (fun ((id', _, _, _) as timer) ->
              if id' = id then (id, p, o, ended) else timer)
            !timers)
  in
  List.iter
    (function
      | `Turn t -> fire_due t
      | `Declare (t, set) ->
          timers :=
            List.map
              (fun (id, p) ->
                match
                  List.find_opt
                    (fun (id', p', _, _) -> id' = id && p' = p)
                    !timers
                with
                | Some kept -> kept
                | None -> (id, p, t, 0))
              set)
    events;
  List.rev !fires

let declare = ref []

let%expect_test "timers follow the reference model" =
  let rand = Random.State.make [| 6 |] in
  let pool = [ "a"; "b"; "c"; "d" ] in
  let period () =
    match Random.State.int rand 3 with
    | 0 -> ns 0.001 * (1 + Random.State.int rand 20)
    | 1 -> ns 0.01 * (1 + Random.State.int rand 100)
    | _ -> ns 0.1 * (1 + Random.State.int rand 100)
  in
  let next_set previous =
    List.filter_map
      (fun id ->
        if Random.State.int rand 4 = 0 then None
        else
          match List.assoc_opt id previous with
          | Some p when Random.State.int rand 4 > 0 -> Some (id, p)
          | _ -> Some (id, period ()))
      pool
  in
  let gap () =
    match Random.State.int rand 4 with
    | 0 -> Random.State.full_int rand (ns 0.05)
    | 1 -> Random.State.full_int rand (ns 2.)
    | 2 -> Random.State.full_int rand (ns 10.)
    | _ -> Random.State.full_int rand (ns 60.)
  in
  let cases = 10_000 and failures = ref 0 in
  let total = ref 0 and counted = ref 0 in
  for case = 1 to cases do
    let sets = List.init (2 + Random.State.int rand 6) (fun _ -> ()) in
    let sets =
      List.fold_left
        (fun acc () ->
          next_set (match acc with s :: _ -> s | [] -> []) :: acc)
        [] sets
      |> List.rev
    in
    let script = ref (List.map (fun set -> (set, gap ())) sets) in
    let events = ref [] and fires = ref [] in
    let application =
      {
        Mosaic.init = (fun () -> ([], Mosaic.Cmd.none));
        update =
          (fun msg set ->
            match msg with
            | `Declare set -> (set, Mosaic.Cmd.none)
            | `Fire (id, k) ->
                fires := (clock (), id, k) :: !fires;
                (set, Mosaic.Cmd.none));
        view = (fun set -> Mosaic.text (string_of_int (List.length set)));
        subscriptions =
          (fun set ->
            Mosaic.Sub.batch
              (List.map
                 (fun (id, p) ->
                   Mosaic.Sub.every ~id
                     (Float.of_int p *. 1e-9)
                     (fun k -> `Fire (id, k)))
                 set));
      }
    in
    let pending = ref None in
    let t = terminal ~width:20 ~height:1 () in
    current := Some t;
    let application =
      {
        application with
        subscriptions =
          (fun set ->
            Mosaic.Sub.batch
              [
                Mosaic.Sub.on_key (fun _ -> Some (`Declare !declare));
                application.subscriptions set;
              ]);
      }
    in
    (* The loop runs at each time the driver moves the clock to, the loop's
       deadlines as often as past them. *)
    run t application (fun _ ->
        let rec loop () =
          let now = elapsed t in
          events := `Turn now :: !events;
          match !pending with
          | Some target when now < target ->
              let step =
                match Matrix_test.deadline t.s with
                | Some until when until > now && Random.State.bool rand ->
                    min (until - now) (target - now)
                | _ -> target - now
              in
              Matrix_test.set_time t.s (now + step);
              Matrix_test.settle t.s;
              loop ()
          | _ -> (
              match !script with
              | [] -> ()
              | (set, gap) :: rest ->
                  script := rest;
                  pending := Some (now + gap);
                  events := `Declare (now, set) :: !events;
                  declare := set;
                  feed t "x";
                  loop ())
        in
        loop ());
    let expected = reference_timers (List.rev !events) in
    total := !total + List.length expected;
    List.iter (fun (_, _, k) -> if k > 1 then incr counted) expected;
    if expected <> List.rev !fires then (
      incr failures;
      if !failures = 1 then Printf.printf "case %d differs\n" case)
  done;
  Printf.printf "%d of %d cases differ\n" !failures cases;
  Printf.printf "%d fires, %d with a count above 1\n" !total !counted;
  [%expect
    {|
    0 of 10000 cases differ
    128264 fires, 79002 with a count above 1
    |}]

let%expect_test "an animation that never ends settles at each instant" =
  (* A tick subscription asks for a frame from every frame: that frame waits
     its interval, so a settle draws one frame and a virtual second at 60
     frames a second draws 59 more, the 60th coming 20 ns after it, as the
     interval is 1/60 s rounded to the nanosecond. *)
  let frames = ref [] in
  drive
    (app ~subs:(fun _ -> Mosaic.Sub.on_tick ~id:"t" (fun ~dt:_ -> Ignored)) ())
    [
      `Run (fun t -> frames := [ Matrix_test.frames t.s ]);
      `Advance 1.0;
      `Run (fun t -> frames := Matrix_test.frames t.s :: !frames);
    ];
  (match !frames with
  | [ after; before ] ->
      Printf.printf "%d frames, then %d\n" before (after - before)
  | _ -> ());
  [%expect {| 1 frames, then 59 |}]

(* A program run with [start] is driven by its caller: [step] never waits,
   [`Idle] leaves the waiting to the caller until [deadline], a message
   dispatched from elsewhere reaches [update], and the end of the input is
   [`Stopped]. *)
let%expect_test "a caller drives a started program by its steps" =
  if Sys.win32 then
    Windtrap.skip ~reason:"Backend.unix, which needs a wake pipe Windows lacks"
      ();
  (* On a pipe the test writes and a file it reads: the program waits for
     nothing but its frame interval, on the run's own clock. *)
  let input, writer = Unix.pipe ~cloexec:true () in
  let path = Filename.temp_file "test_loop" ".out" in
  let output = Unix.openfile path [ O_RDWR; O_TRUNC; O_CLOEXEC ] 0o600 in
  Unix.unlink path;
  (* A pipe has no window: its terminal reports the size in band. *)
  let report = "\027[48;1;48;0;0t" in
  ignore (Unix.write_substring writer report 0 (String.length report) : int);
  let screen () =
    let vte = Vte.create ~rows:1 ~cols:48 () in
    let length = (Unix.fstat output).st_size in
    let b = Bytes.create length in
    ignore (Unix.lseek output 0 SEEK_SET : int);
    let rec read off =
      if off < length then read (off + Unix.read output b off (length - off))
    in
    read 0;
    ignore (Vte.feed vte b 0 length : _ list);
    String.trim (Vte.text vte 0)
  in
  Mosaic.start ~input ~output ~config:(Matrix.config ~caps ())
    (app ~subs:on_keys ()) (fun r ->
      let rec until_idle () =
        match Mosaic.step r ~now:(Matrix.clock ()) with
        | `Stepped -> until_idle ()
        | `Stopped -> print_endline "stopped"
        | `Idle -> (
            match Mosaic.deadline r with
            | Some deadline ->
                Unix.sleepf
                  (Float.max 0.
                     (Float.of_int (deadline - Matrix.clock ()) *. 1e-9));
                until_idle ()
            | None -> print_endline (screen ()))
      in
      until_idle ();
      Mosaic.dispatch r (Note "dispatched");
      until_idle ();
      ignore (Unix.write_substring writer "a" 0 1 : int);
      until_idle ();
      Unix.close writer;
      until_idle ());
  Unix.close input;
  Unix.close output;
  [%expect
    {|
    keys:[] ticks:0 size:- note:-
    keys:[] ticks:0 size:- note:dispatched
    keys:[a] ticks:0 size:- note:dispatched
    stopped
    |}]

(* A stop requested before the program starts ends it at its first step:
   it is never idle, and no update runs. *)
let%expect_test "a stop requested before the start ends the program" =
  let stop = Matrix.Stop.create () in
  Matrix.Stop.request stop;
  let t = terminal ~width:48 ~height:1 () in
  let updates = ref 0 in
  let base = app ~subs:on_keys () in
  Mosaic.start ~config:(config t) ~stop
    {
      base with
      update =
        (fun msg m ->
          incr updates;
          base.update msg m);
    }
    (fun r ->
      (* A message for the program before its first step. *)
      Mosaic.dispatch r (Pressed "a");
      Matrix_test.attach t.s ~step:(Mosaic.step r) ~deadline:(fun () ->
          Mosaic.deadline r);
      is_true ~msg:"ended at its first step" (Matrix_test.ended t.s));
  Printf.printf "updates: %d\n" !updates;
  [%expect {| updates: 0 |}]
