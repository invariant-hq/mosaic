open Windtrap

(* The scrollback's lines, oldest first. *)
let scrollback_lines vte =
  let n = Vte.scrollback_size vte in
  List.init n (fun i -> Vte.text vte (i - n))

exception Injected_startup_failure

let[@inline never] raise_injected_startup_failure () =
  raise Injected_startup_failure

let ms n = n * 1_000_000

(* The environment, as a value: the tests never read the process's. *)
let env vars =
  Matrix.Terminal.Caps.env (fun name -> List.assoc_opt name vars) ~win32:false

let no_env = env []
let key ?mods key = Matrix.Key (Matrix.Input.Keystroke.make ?mods key)
let char c = key (Char (Uchar.of_char c))
let input_event = Testable.make ~pp:Matrix.pp_event ~equal:( = )

(* A scripted terminal. Input is a list of chunks, read one per read; an
   empty chunk is a wait that reads nothing. [answer] is what the terminal
   writes back for each write of the runtime. The clock moves only when the
   loop would block: [on_wait] decides, and by default moves it to the
   deadline, or fails when there is none, as the loop would wait forever. *)
type fake = {
  mutable now : int;
  mutable input : string list;
  mutable eof : bool;
  mutable signals : Matrix.Backend.signal list;
  mutable size : int * int;
  output : Buffer.t;
  mutable reads : int;
  mutable waits : (int * int option) list;
      (** When, until what; latest first. *)
  mutable raw : bool list;  (** Raw mode changes, latest first. *)
  mutable flushes : int;
  mutable cleanups : int;
  wakes : int Atomic.t;
  mutable answer : string -> string;
  mutable on_wait : fake -> until:int option -> unit;
  mutable fail_write : string -> unit;
  tty : bool;
}

let block_forever _ ~until =
  match until with Some _ -> () | None -> fail "the loop would wait forever"

let fake ?(tty = false) ?(size = (80, 24)) ?(input = []) ?(answer = fun _ -> "")
    () =
  {
    now = 0;
    input;
    eof = false;
    signals = [];
    size;
    output = Buffer.create 256;
    reads = 0;
    waits = [];
    raw = [];
    flushes = 0;
    cleanups = 0;
    wakes = Atomic.make 0;
    answer;
    on_wait = block_forever;
    fail_write = ignore;
    tty;
  }

let output f = Buffer.contents f.output

let backend f : Matrix.Backend.t =
  let wait ~until ~reading =
    f.waits <- (f.now, until) :: f.waits;
    let readable () = reading && (f.input <> [] || f.eof) in
    match f.input with
    | "" :: rest when reading ->
        f.input <- rest;
        false
    | _ when readable () -> true
    | _ -> (
        match until with
        | Some until when f.now >= until -> false
        | _ ->
            f.on_wait f ~until;
            (match until with
            | Some until when f.now < until && not (readable ()) ->
                f.now <- until
            | _ -> ());
            readable ())
  in
  let read_input buf off len =
    f.reads <- f.reads + 1;
    match f.input with
    | [] -> if f.eof then 0 else -1
    | chunk :: rest ->
        let n = min len (String.length chunk) in
        Bytes.blit_string chunk 0 buf off n;
        f.input <-
          (if n < String.length chunk then
             String.sub chunk n (String.length chunk - n) :: rest
           else rest);
        n
  in
  {
    now = (fun () -> f.now);
    wait;
    wake = (fun () -> Atomic.incr f.wakes);
    take_signals =
      (fun () ->
        let s = f.signals in
        f.signals <- [];
        s);
    read_input;
    write_output =
      (fun buf off len ->
        let s = Bytes.sub_string buf off len in
        f.fail_write s;
        Buffer.add_string f.output s;
        match f.answer s with "" -> () | r -> f.input <- f.input @ [ r ]);
    fds = (fun ~reading:_ -> []);
    terminal_size = (fun () -> Some f.size);
    window_pixels = (fun () -> None);
    set_raw_mode = (fun raw -> f.raw <- raw :: f.raw);
    flush_input = (fun () -> f.flushes <- f.flushes + 1);
    input_is_tty = f.tty;
    output_is_tty = f.tty;
    controlling = f.tty;
    on_fatal = ignore;
    cleanup = (fun () -> f.cleanups <- f.cleanups + 1);
    env = no_env;
  }

let quiet ?(mode = `Alt) ?(fps = 60.) ?caps ?(min_tui_height = 1) () =
  Matrix.config ~mode ~fps ~mouse:`Off ~bracketed_paste:false
    ~kitty_keyboard:`Off ?caps ~min_tui_height ()

let run ?config f body = Matrix.run_on (backend f) ?config body

(* The next step that is not a frame; frames met meanwhile are drawn
   empty. *)
let rec next_event app =
  match Matrix.next app with
  | Frame ->
      Matrix.draw app ignore;
      next_event app
  | step -> step

(* The events of the run until it stops. *)
let drain app =
  let rec go acc =
    match next_event app with
    | Stopped -> List.rev acc
    | Event e -> go (e :: acc)
    | Timer _ | Posted _ | Frame -> go acc
  in
  go []

let draw_rows grid rows =
  List.iteri (fun y text -> Matrix.Grid.draw_text grid ~x:0 ~y ~text) rows

(* Asks for a frame and draws [rows] in it; the events met before it are
   returned. *)
let frame ?required ?(draw = ignore) app rows =
  Matrix.request_redraw app;
  let rec go events =
    match Matrix.next app with
    | Frame ->
        Matrix.draw app ?primary_required_rows:required (fun grid ->
            draw_rows grid rows;
            draw grid);
        List.rev events
    | Event e -> go (e :: events)
    | Timer _ | Posted _ -> go events
    | Stopped -> fail "the run stopped before the frame"
  in
  go []

let present ?required app rows = ignore (frame ?required app rows : _ list)

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let static_grid text =
  let lines = String.split_on_char '\n' text in
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  let width =
    List.fold_left (fun width line -> max width (String.length line)) 1 lines
  in
  let grid = Matrix.Grid.create ~width ~height:(max 1 (List.length lines)) () in
  List.iteri (fun y text -> Matrix.Grid.draw_text grid ~x:0 ~y ~text) lines;
  grid

let set_sync_capable app =
  let terminal = Matrix.terminal app in
  let caps = Matrix.Terminal.capabilities terminal in
  Matrix.Terminal.set_capabilities terminal { caps with sync = true }

let index_of s sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else go (i + 1)
  in
  go 0

(* The loop *)

(* Frames come only when asked for: a run draws its first frame, then
   waits with no deadline. *)
let test_frames_on_demand () =
  let f = fake () in
  let frames = ref 0 in
  f.on_wait <-
    (fun f ~until ->
      equal ~msg:"an idle loop waits with no deadline" (option int) None until;
      f.eof <- true);
  run ~config:(quiet ()) f (fun app ->
      let rec loop () =
        match Matrix.next app with
        | Frame ->
            incr frames;
            Matrix.draw app ignore;
            loop ()
        | Stopped -> ()
        | _ -> loop ()
      in
      loop ());
  equal ~msg:"one frame" int 1 !frames

(* Requests until a frame are one; a request made while a frame is drawn
   asks for the next, at least one interval later; the first frame after a
   pause is drawn at once. *)
let test_frame_requests () =
  let f = fake () in
  let times = ref [] in
  run ~config:(quiet ~fps:10. ()) f (fun app ->
      let draw () =
        times := f.now :: !times;
        Matrix.draw app ignore
      in
      let rec frames n =
        if n > 0 then
          match Matrix.next app with
          | Frame ->
              draw ();
              if n > 1 then Matrix.request_redraw app;
              frames (n - 1)
          | _ -> frames n
      in
      Matrix.request_redraw app;
      Matrix.request_redraw app;
      frames 3;
      equal ~msg:"three frames an interval apart" (list int)
        [ 0; ms 100; ms 200 ]
        (List.rev !times);
      (* A pause, then a request: the frame is drawn in the turn that
         asked. *)
      f.now <- f.now + ms 1000;
      Matrix.request_redraw app;
      times := [];
      frames 1;
      equal ~msg:"the first frame after a pause is at once" (list int)
        [ ms 1200 ]
        !times)

(* [frame_dt] is the time between the latest two frames: 0 for the first,
   one interval while an animation asks from each frame, and the whole pause
   for the first frame after one. *)
let test_frame_dt () =
  let f = fake () in
  let dts = ref [] in
  run ~config:(quiet ~fps:10. ()) f (fun app ->
      let rec frames n =
        if n > 0 then
          match Matrix.next app with
          | Frame ->
              dts := Matrix.frame_dt app :: !dts;
              Matrix.draw app ignore;
              if n > 1 then Matrix.request_redraw app;
              frames (n - 1)
          | _ -> frames n
      in
      equal ~msg:"before the first frame" (float 1e-9) 0. (Matrix.frame_dt app);
      frames 3;
      f.now <- f.now + ms 1000;
      Matrix.request_redraw app;
      frames 1;
      equal ~msg:"0, two intervals, then the pause"
        (list (float 1e-9))
        [ 0.; 0.1; 0.1; 1.0 ] (List.rev !dts))

(* A turn: one step of each kind that has some, in kind order, then the
   events, the timers and the posts, until all are done or one frame interval
   has passed since the clock was read; then the frame, once its interval
   has passed when the work ends. Each step here costs 10 ms; the interval is
   16.7 ms: the second turn's work ends at 1,050 ms, past the frame due at
   1,046.7 ms. The key c asks for a frame before the grid's next one, which
   comes early, in c's turn. *)
let test_turn_order () =
  let f = fake ~input:[] () in
  let log = Buffer.create 64 in
  let note s =
    Buffer.add_string log s;
    Buffer.add_char log ' '
  in
  run ~config:(quiet ()) f (fun app ->
      (* The first frame. *)
      (match Matrix.next app with
      | Frame -> Matrix.draw app ignore
      | _ -> fail "expected the first frame");
      let timer = Matrix.at app (ms 500) in
      f.on_wait <-
        (fun f ~until:_ ->
          if f.now < ms 1000 then (
            f.input <- [ "abc" ];
            Matrix.post app (fun () -> note "p1");
            Matrix.post app (fun () -> note "p2");
            f.now <- ms 1000)
          else f.eof <- true);
      let rec loop () =
        match Matrix.next app with
        | Stopped -> note "stopped"
        | Event (Key { key = Char u; _ }) ->
            note (String.make 1 (Uchar.to_char u));
            f.now <- f.now + ms 10;
            Matrix.request_redraw app;
            loop ()
        | Timer t ->
            note (if t = timer then "timer" else "?");
            f.now <- f.now + ms 10;
            loop ()
        | Posted p ->
            p ();
            f.now <- f.now + ms 10;
            loop ()
        | Frame ->
            note (Printf.sprintf "frame@%d" (f.now / ms 1));
            Matrix.draw app ignore;
            loop ()
        | Event _ -> loop ()
      in
      loop ());
  equal string "a timer p1 frame@1030 b p2 frame@1050 c frame@1060 stopped "
    (Buffer.contents log)

(* Discrete input *)

(* A run at 60 frames a second on a scripted terminal. Each of [arrivals],
   [(time, bytes)], comes while the run waits; a key costs [cost] and asks
   for a frame, and [animate now] says whether a frame asks for the next. The
   frames, as [(time, on the grid, keys handled)], the keys, and the
   deadlines of the waits after the last arrival. *)
let paced ?(cost = 0) ?(animate = fun _ -> false) arrivals =
  let f = fake () in
  let arrivals = ref arrivals in
  let frames = ref [] and keys = ref [] and after = ref [] in
  f.on_wait <-
    (fun f ~until ->
      match !arrivals with
      | (at, bytes) :: rest
        when match until with Some u -> at <= u | None -> true ->
          arrivals := rest;
          f.now <- max f.now at;
          f.input <- [ bytes ]
      | [] ->
          after := until :: !after;
          if until = None then f.eof <- true
      | _ -> ());
  run ~config:(quiet ()) f (fun app ->
      let rec loop () =
        match Matrix.next app with
        | Stopped -> ()
        | Event (Key { key = Char u; _ }) ->
            keys := Uchar.to_char u :: !keys;
            f.now <- f.now + cost;
            Matrix.request_redraw app;
            loop ()
        | Frame ->
            frames :=
              (f.now, not (Matrix.early app), List.length !keys) :: !frames;
            Matrix.draw app ignore;
            if animate f.now then Matrix.request_redraw app;
            loop ()
        | _ -> loop ()
      in
      loop ());
  (List.rev !frames, List.rev !keys, List.rev !after)

let interval_60 = 16_666_667

(* A key is drawn in the turn that read it, unless a frame came early less
   than an interval before: then once that interval has passed, or with the
   grid's frame if it comes first. An early frame leaves the grid. The key
   at 50 ms comes after a pause, so its frame is on the grid. *)
let test_discrete_input_early () =
  let frames, _, _ =
    paced [ (ms 50, "a"); (ms 55, "b"); (ms 60, "c"); (ms 70, "d") ]
  in
  equal
    (list (triple int bool int))
    [
      (0, true, 0);
      (ms 50, true, 1);
      (ms 55, false, 2);
      (ms 50 + interval_60, true, 3);
      (ms 55 + interval_60, false, 4);
    ]
    frames

(* A flood of keys, one a millisecond for a second: at most twice 60 frames
   in any second, the keys in order, the last key's frame drawn, and then no
   wakeup. *)
let test_discrete_flood () =
  let sent = List.init 1000 (fun i -> Char.chr (Char.code 'a' + (i mod 26))) in
  let frames, keys, after =
    paced ~cost:200_000
      (List.mapi (fun i c -> (ms (i + 1), String.make 1 c)) sent)
  in
  equal ~msg:"the keys, in order" (list Testable.char) sent keys;
  let _, _, last = List.nth frames (List.length frames - 1) in
  equal ~msg:"the last frame shows every key" int 1000 last;
  List.iter
    (fun (t, _, _) ->
      let n =
        List.length
          (List.filter (fun (u, _, _) -> u >= t && u < t + ms 1000) frames)
      in
      if n > 120 then failf "%d frames in the second from %d ns" n t)
    frames;
  let apart which =
    let times =
      List.filter_map
        (fun (t, g, _) -> if g = which then Some t else None)
        frames
    in
    ignore
      (List.fold_left
         (fun previous t ->
           if t - previous < interval_60 then
             failf "two frames %d ns apart" (t - previous);
           t)
         (min_int / 2) times
        : int);
    List.length times
  in
  let grid = apart true and early = apart false in
  is_true ~msg:"frames on the grid" (grid >= 55);
  is_true ~msg:"early frames" (early >= 55);
  equal ~msg:"idle after the last frame, with no deadline" (option int) None
    (List.nth after (List.length after - 1));
  is_true ~msg:"at most the last frame's wakeup" (List.length after <= 2)

(* An animation that asks for a frame from each frame has the same frames on
   the grid with keys typed 30 times a second as without: early frames add
   to them and move none. *)
let test_discrete_keeps_the_grid () =
  let animate now = now < ms 1000 in
  let typed = List.init 30 (fun i -> (ms 5 + (i * 33_333_333), "k")) in
  let on_grid frames =
    List.filter_map (fun (t, g, _) -> if g then Some t else None) frames
  in
  let alone, _, _ = paced ~animate [] in
  let typing, keys, _ = paced ~animate typed in
  equal ~msg:"keys" int 30 (List.length keys);
  equal ~msg:"the frames on the grid" (list int) (on_grid alone)
    (on_grid typing);
  equal ~msg:"an early frame for each key" int 30
    (List.length (List.filter (fun (_, g, _) -> not g) typing))

(* While an event decoded is still waiting, the input is not read: the rest
   stays in the operating system's buffer, which a terminal fills no faster
   than the loop takes it. *)
let test_input_read_when_nothing_is_queued () =
  let f = fake ~input:[ "ab"; "cd" ] () in
  f.eof <- true;
  run ~config:(quiet ()) f (fun app ->
      match next_event app with
      | Event _ ->
          equal ~msg:"one read for the first two keys" int 1 f.reads;
          ignore (next_event app);
          equal ~msg:"no read while b waits" int 1 f.reads;
          ignore (next_event app);
          equal ~msg:"the next read once both are taken" int 2 f.reads
      | _ -> fail "expected a key")

(* Timers due in one turn come in deadline order, then in the order they were
   set; a cancelled one never comes. *)
let test_timers () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      let a = Matrix.at app (ms 30) in
      let b = Matrix.at app (ms 10) in
      let c = Matrix.at app (ms 10) in
      let d = Matrix.at app (ms 20) in
      Matrix.cancel app d;
      Matrix.cancel app d;
      f.on_wait <- (fun f ~until:_ -> f.now <- ms 50);
      let names = [ (a, "a"); (b, "b"); (c, "c"); (d, "d") ] in
      let rec loop acc =
        if List.length acc = 3 then List.rev acc
        else
          match next_event app with
          | Timer t -> loop (List.assoc t names :: acc)
          | _ -> loop acc
      in
      equal ~msg:"deadline order, then set order" (list string)
        [ "b"; "c"; "a" ] (loop []);
      equal ~msg:"the turn's time" int (ms 50) (Matrix.now app))

(* The heap against a sorted list, under random sets and cancels. *)
let test_timer_order_property () =
  let rand = Random.State.make [| 42 |] in
  for _ = 1 to 200 do
    let f = fake () in
    run ~config:(quiet ()) f (fun app ->
        let live = ref [] and seq = ref 0 in
        for _ = 1 to 1 + Random.State.int rand 40 do
          if !live <> [] && Random.State.int rand 4 = 0 then (
            let t, _, _ =
              List.nth !live (Random.State.int rand (List.length !live))
            in
            Matrix.cancel app t;
            live := List.filter (fun (t', _, _) -> t' <> t) !live)
          else
            let deadline = ms (Random.State.int rand 50) in
            live := (Matrix.at app deadline, deadline, !seq) :: !live;
            incr seq
        done;
        let expected =
          List.sort (fun (_, d, s) (_, d', s') -> compare (d, s) (d', s')) !live
          |> List.map (fun (t, _, _) -> t)
        in
        f.on_wait <- (fun f ~until:_ -> f.now <- ms 100);
        let rec take n acc =
          if n = 0 then List.rev acc
          else
            match next_event app with
            | Timer t -> take (n - 1) (t :: acc)
            | _ -> take n acc
        in
        let got = take (List.length expected) [] in
        is_true ~msg:"the heap returns the sorted list" (got = expected))
  done

(* A backend whose turns are recorded: the time each wait returned, which is
   the clock the turn reads. *)
let recording_turns ?(on_turn = ignore) f =
  let turns = ref [] in
  let b = backend f in
  ( turns,
    {
      b with
      wait =
        (fun ~until ~reading ->
          let ready = b.wait ~until ~reading in
          turns := f.now :: !turns;
          on_turn f.now;
          ready);
    } )

(* Frames under random requests, idle times and resizes: frames are an
   interval apart; a frame is drawn only when asked for; one asked for is
   drawn at the end of the first turn whose time is an interval past the
   previous frame (handlers here take no time, so in that turn: the first
   frame after a pause is drawn in the turn that asked); a request made
   while a frame is drawn asks for the next; a resize is reported with the
   latest size, before the frame drawn at it. *)
let test_frame_property () =
  let rand = Random.State.make [| 7 |] in
  let interval = 16_666_667 in
  for _ = 1 to 300 do
    let f = fake () in
    let frames = ref [] and pending = ref true and last = ref (min_int / 2) in
    (* The turns that began owing a frame whose interval had passed. *)
    let due = ref [] in
    let turns, backend =
      recording_turns f ~on_turn:(fun now ->
          if !pending && now >= !last + interval && not f.eof then
            due := now :: !due)
    in
    let waits = ref 0 in
    let resizes_ok = ref true in
    Matrix.run_on backend ~config:(quiet ()) (fun app ->
        f.on_wait <-
          (fun f ~until ->
            incr waits;
            if !waits > 60 then f.eof <- true
            else (
              (match Random.State.int rand 4 with
              | 0 -> ( match until with Some u -> f.now <- u | None -> ())
              | 1 ->
                  f.now <- f.now + Random.State.int rand (ms 50);
                  Matrix.post app `Request
              | 2 ->
                  f.size <-
                    (10 + Random.State.int rand 70, 5 + Random.State.int rand 20);
                  f.signals <- [ Matrix.Backend.Winch ];
                  (* A resize asks for a frame. *)
                  pending := true;
                  f.now <- f.now + Random.State.int rand (ms 5)
              | _ -> f.now <- f.now + Random.State.int rand (ms 5));
              if until = None && f.now = 0 then f.now <- 1));
        let rec loop () =
          match Matrix.next app with
          | Stopped -> ()
          | Frame ->
              if not !pending then fail "a frame no one asked for";
              frames := f.now :: !frames;
              last := f.now;
              pending := false;
              if Random.State.int rand 3 = 0 then (
                Matrix.request_redraw app;
                pending := true);
              Matrix.draw app ignore;
              loop ()
          | Posted `Request ->
              Matrix.request_redraw app;
              pending := true;
              loop ()
          | Event (Resize (w, h)) ->
              if (w, h) <> f.size then resizes_ok := false;
              loop ()
          | Event _ | Timer _ -> loop ()
        in
        loop ());
    ignore !turns;
    List.iter
      (fun t ->
        if not (List.mem t !frames) then
          failf "a frame due at %d ns was not drawn in that turn" t)
      !due;
    let frames = List.rev !frames in
    ignore
      (List.fold_left
         (fun previous t ->
           if t - previous < interval then
             failf "frames %d ns apart, under the interval" (t - previous);
           t)
         (min_int / 2) frames
        : int);
    is_true ~msg:"a resize reports the latest size" !resizes_ok
  done

(* A step that says [None] leaves nothing due: right after it, the deadline
   is later than its time, or there is none. Random frames, timers, posts,
   resizes, partial sequences, in-band size reports and queries a terminal
   answers or leaves unanswered, at random times, in both modes. *)
let idle_deadline_property
    ~(run : config:_ -> _ -> (unit Matrix.app -> unit) -> unit) () =
  let rand = Random.State.make [| 13 |] in
  let pick l = List.nth l (Random.State.int rand (List.length l)) in
  for _ = 1 to 300 do
    let tty = Random.State.bool rand and answers = Random.State.bool rand in
    let answer bytes =
      if not answers then ""
      else if index_of bytes "\027[6n" <> None then "\027[3;1R"
      else if String.ends_with ~suffix:"\027[c" bytes then "\027[?62c"
      else ""
    in
    let f = fake ~tty ~answer () in
    let mode = pick [ `Alt; `Primary ] in
    run ~config:(quiet ~mode ()) f (fun app ->
        f.on_wait <- (fun _ ~until:_ -> fail "a step waited");
        let now = ref f.now in
        for _ = 1 to 40 do
          (match Random.State.int rand 7 with
          | 0 -> Matrix.request_redraw app
          | 1 ->
              ignore
                (Matrix.at app (!now + Random.State.int rand (ms 100))
                  : Matrix.timer)
          | 2 ->
              f.input <-
                f.input
                @ [
                    pick [ "a"; "\027"; "\027["; "\027[A"; "\027[48;9;40;0;0t" ];
                  ]
          | 3 -> Matrix.post app ()
          | 4 ->
              f.size <-
                (10 + Random.State.int rand 70, 5 + Random.State.int rand 20);
              f.signals <- [ Matrix.Backend.Winch ]
          | 5 ->
              if tty then
                Matrix.query app
                  ~owed:{ cursor_reports = 0; string_replies = 0 }
                  ""
          | _ -> ());
          (* A driver steps again at the deadline, or when something it
             waits for comes earlier. *)
          (now :=
             match Matrix.deadline app with
             | Some d when Random.State.bool rand -> max !now d
             | _ -> !now + Random.State.int rand (ms 30));
          f.now <- !now;
          let rec until_idle () =
            match Matrix.step app ~now:!now with
            | Some Frame ->
                Matrix.draw app ignore;
                until_idle ()
            | Some Stopped -> fail "the run stopped"
            | Some _ -> until_idle ()
            | None -> (
                match Matrix.deadline app with
                | Some d when d <= !now ->
                    failf "idle at %d with a deadline at %d" !now d
                | _ -> ())
          in
          until_idle ()
        done)
  done

let test_idle_deadline_property =
  idle_deadline_property ~run:(fun ~config f body -> run ~config f body)

(* A terminal in the program's own process ------------------------------- *)

(* [f] as a device: each input chunk is a whole write, a wait for an answer
   with nothing queued moves the clock to its end, and the frames begun are
   counted. *)
let device ?(frames = ref 0) f : Matrix.device =
  let b = backend f in
  {
    read_clock = (fun () -> f.now);
    wait =
      (fun ~until ->
        if f.input <> [] then true
        else (
          (match until with
          | Some u when u > f.now -> f.now <- u
          | Some _ -> ()
          | None -> fail "the run waits forever");
          false));
    read = b.read_input;
    write = b.write_output;
    window = (fun () -> Some f.size);
    resized =
      (fun () ->
        let w = List.mem Matrix.Backend.Winch f.signals in
        f.signals <- [];
        w);
    set_raw = b.set_raw_mode;
    flush = (fun () -> f.input <- []);
    tty = f.tty;
    env = no_env;
    frame = (fun () -> incr frames);
    idle = (fun ~until:_ ~stepped:_ -> fail "the run waits");
    presented = ignore;
  }

let run_device ?(frames = ref 0) ?(config = quiet ()) f body =
  Matrix.run ~config:(Matrix.on_device (device ~frames f) config) body

let test_device_idle_deadline_property =
  idle_deadline_property ~run:(fun ~config f body -> run_device ~config f body)

(* Steps at [f.now] until the run is idle, drawing each frame and handing
   every other step to [on], with [animate] asking for the next frame after
   each is drawn, as an animation does. *)
let settle ?(animate = fun () -> false) ?(on = ignore) f app =
  let rec go () =
    match Matrix.step app ~now:f.now with
    | Some Matrix.Frame ->
        Matrix.draw app ignore;
        if animate () then Matrix.request_redraw app;
        go ()
    | Some step ->
        on step;
        go ()
    | None -> ()
  in
  go ()

(* A frame asked for by input, a timer or a post is drawn in the turn that
   asked, without the clock moving. *)
let test_device_frames_asked_outside () =
  let f = fake () and frames = ref 0 in
  run_device ~frames ~config:(quiet ~fps:30. ()) f (fun app ->
      let on = function
        | Matrix.Event _ | Timer _ | Posted _ -> Matrix.request_redraw app
        | _ -> ()
      in
      settle ~on f app;
      equal ~msg:"the first frame" int 1 !frames;
      f.input <- [ "a" ];
      settle ~on f app;
      ignore (Matrix.at app f.now : Matrix.timer);
      settle ~on f app;
      Matrix.post app ();
      settle ~on f app;
      equal ~msg:"a frame each, at once" int 4 !frames;
      equal ~msg:"the clock" int 0 f.now)

(* An animation asks for its next frame from its Frame step, after drawing
   it: that frame waits its interval, so a virtual second at 30 frames a
   second draws 30, and a frame input brings in between keeps the grid. *)
let test_device_animation () =
  let f = fake () and frames = ref 0 and times = ref [] in
  run_device ~frames ~config:(quiet ~fps:30. ()) f (fun app ->
      let on = function
        | Matrix.Event _ -> Matrix.request_redraw app
        | _ -> ()
      in
      let animate () =
        times := f.now :: !times;
        true
      in
      let advance_to target =
        let rec go () =
          settle ~animate ~on f app;
          match Matrix.deadline app with
          | Some d when d <= target ->
              f.now <- d;
              go ()
          | _ ->
              f.now <- target;
              settle ~animate ~on f app
        in
        go ()
      in
      settle ~animate ~on f app;
      equal ~msg:"settled after one frame" int 1 !frames;
      advance_to (ms 50);
      f.input <- [ "a" ];
      settle ~animate ~on f app;
      advance_to (ms 1000);
      let interval = 33_333_333 in
      equal ~msg:"frames: the grid, and the one input brought at 50 ms"
        (list int)
        ([ 0; interval; ms 50 ] @ List.init 29 (fun k -> (k + 2) * interval))
        (List.rev !times))

(* A resize, which the runtime asks a frame for itself, is drawn at once. *)
let test_device_resize () =
  let f = fake () and frames = ref 0 in
  run_device ~frames f (fun app ->
      let resized = ref [] in
      let on = function
        | Matrix.Event (Resize (c, r)) -> resized := (c, r) :: !resized
        | _ -> ()
      in
      settle ~on f app;
      f.size <- (40, 10);
      f.signals <- [ Matrix.Backend.Winch ];
      settle ~on f app;
      equal ~msg:"the resize" (list (pair int int)) [ (40, 10) ] !resized;
      equal ~msg:"its frame, at once" int 2 !frames)

(* Input is decoded whole once nothing follows it: a lone ESC is Escape in
   the turn that reads it, and a paste longer than a read ends. *)
let test_device_whole_writes () =
  let paste = "\027[200~" ^ String.make 10_000 'x' ^ "\027[201~" in
  let f = fake ~input:[ "\027"; paste ] () in
  run_device f (fun app ->
      let events = ref [] in
      settle ~on:(function Event e -> events := e :: !events | _ -> ()) f app;
      let shown =
        List.rev_map
          (function
            | Matrix.Paste_data s -> Printf.sprintf "data %d" (String.length s)
            | e -> Format.asprintf "%a" Matrix.pp_event e)
          !events
      in
      let rec join = function
        | a :: b :: rest
          when String.starts_with ~prefix:"data" a
               && String.starts_with ~prefix:"data" b ->
            let n s = int_of_string (String.sub s 5 (String.length s - 5)) in
            join (Printf.sprintf "data %d" (n a + n b) :: rest)
        | a :: rest -> a :: join rest
        | [] -> []
      in
      equal ~msg:"events" (list string)
        [ "Key Escape"; "Paste_start"; "data 10000"; "Paste_end Complete" ]
        (join shown);
      equal ~msg:"nothing due" (option int) None (Matrix.deadline app))

(* A terminal that never answers costs the run's wait for an answer: the
   device's clock moves to its end, and the run takes no process signal. *)
let test_device_silent () =
  let f = fake ~tty:true () in
  let before = Sys.signal Sys.sigterm Sys.Signal_default in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigterm before)
  @@ fun () ->
  run_device f (fun _ ->
      equal ~msg:"the probe's wait" int (ms 500) f.now;
      let during = Sys.signal Sys.sigterm Sys.Signal_default in
      is_true ~msg:"SIGTERM is left alone" (during = Sys.Signal_default))

(* The turn budget under random input, timers and posts from three
   producers, each step taking up to 10 ms: events and each producer's posts
   come in order, and a turn's work ends within one frame interval and one
   step of its clock, unless the steps it owes each kind take longer. *)
let test_turn_budget_property () =
  let rand = Random.State.make [| 11 |] in
  let interval = 16_666_667 and cost = ms 10 in
  for _ = 1 to 300 do
    let f = fake () in
    let turns, backend = recording_turns f in
    let keys = ref [] and posts = Array.make 3 [] in
    let typed = ref 0 and posted = Array.make 3 0 and waits = ref 0 in
    let work = ref [] in
    Matrix.run_on backend ~config:(quiet ()) (fun app ->
        f.on_wait <-
          (fun f ~until ->
            incr waits;
            if !waits > 30 then f.eof <- true
            else
              let chunk =
                String.init (Random.State.int rand 6) (fun _ ->
                    let c = Char.chr (Char.code 'a' + (!typed mod 26)) in
                    incr typed;
                    c)
              in
              if chunk <> "" then f.input <- f.input @ [ chunk ];
              for p = 0 to 2 do
                for _ = 1 to Random.State.int rand 4 do
                  Matrix.post app (p, posted.(p));
                  posted.(p) <- posted.(p) + 1
                done
              done;
              (match until with
              | Some u when Random.State.bool rand -> f.now <- max f.now u
              | _ -> ());
              f.now <- f.now + Random.State.int rand (ms 30));
        let step () =
          let start = f.now in
          f.now <- f.now + Random.State.int rand cost;
          work := (List.hd !turns, start, f.now) :: !work
        in
        for _ = 1 to Random.State.int rand 4 do
          ignore (Matrix.at app (ms (Random.State.int rand 200)) : Matrix.timer)
        done;
        let rec loop () =
          match Matrix.next app with
          | Stopped -> ()
          | Frame ->
              Matrix.draw app ignore;
              loop ()
          | Event (Key { text; _ }) ->
              keys := text :: !keys;
              step ();
              if Random.State.int rand 3 = 0 then
                ignore
                  (Matrix.at app (f.now + ms (Random.State.int rand 100))
                    : Matrix.timer);
              loop ()
          | Posted (p, n) ->
              posts.(p) <- n :: posts.(p);
              step ();
              loop ()
          | Timer _ ->
              step ();
              loop ()
          | Event _ -> loop ()
        in
        loop ());
    let keys = String.concat "" (List.rev !keys) in
    equal ~msg:"events in order" string
      (String.init (String.length keys) (fun i ->
           Char.chr (Char.code 'a' + (i mod 26))))
      keys;
    Array.iteri
      (fun p got ->
        equal ~msg:"a producer's posts in order" (list int)
          (List.init (List.length got) Fun.id)
          (List.rev got);
        ignore p)
      posts;
    (* The steps of each turn, by the clock the turn read. *)
    let by_turn = Hashtbl.create 16 in
    List.iter
      (fun (turn, start, stop) ->
        let first, last, n =
          Option.value ~default:(start, stop, 0) (Hashtbl.find_opt by_turn turn)
        in
        Hashtbl.replace by_turn turn (min first start, max last stop, n + 1))
      !work;
    Hashtbl.iter
      (fun turn (_, last, n) ->
        let bound = max (interval + cost) (min n 3 * cost) in
        if last - turn > bound then
          failf "a turn's work took %d ns, over %d" (last - turn) bound)
      by_turn
  done

(* [post] is the one call another thread may make: each producer's posts
   come in order, and a post wakes the loop. Every other call raises off the
   loop's thread. *)
let test_post_from_other_domains () =
  let f = fake () in
  let producers = 4 and each = 2_000 in
  let seen = Array.make producers (-1) in
  let disordered = ref 0 in
  let received = ref 0 in
  f.on_wait <-
    (fun f ~until:_ ->
      (* The producers' posts end the wait with a wake. *)
      while Atomic.get f.wakes = 0 do
        Domain.cpu_relax ()
      done;
      Atomic.set f.wakes 0);
  run ~config:(quiet ()) f (fun app ->
      let domains =
        List.init producers (fun p ->
            Domain.spawn (fun () ->
                for i = 0 to each - 1 do
                  Matrix.post app (fun () ->
                      if seen.(p) <> i - 1 then incr disordered;
                      seen.(p) <- i;
                      incr received)
                done))
      in
      let raised =
        Domain.join
          (Domain.spawn (fun () ->
               match Matrix.request_redraw app with
               | () -> false
               | exception Invalid_argument _ -> true))
      in
      is_true ~msg:"a call off the loop's thread raises" raised;
      List.iter Domain.join domains;
      while !received < producers * each do
        match Matrix.next app with
        | Posted p -> p ()
        | Frame -> Matrix.draw app ignore
        | _ -> ()
      done);
  equal ~msg:"every post" int (producers * each) !received;
  equal ~msg:"in order per producer" int 0 !disordered

(* An app belongs to the thread that runs it: every function that takes
   one, but [post], raises on another thread, the readers and the cursor's
   setters included. *)
let test_calls_off_the_loop_raise () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      let calls =
        [
          ("step", fun () -> ignore (Matrix.step app ~now:0));
          ("fds", fun () -> ignore (Matrix.fds app));
          ("deadline", fun () -> ignore (Matrix.deadline app));
          ("now", fun () -> ignore (Matrix.now app));
          ("at", fun () -> ignore (Matrix.at app 0));
          ("request_redraw", fun () -> Matrix.request_redraw app);
          ("frame_dt", fun () -> ignore (Matrix.frame_dt app));
          ("early", fun () -> ignore (Matrix.early app));
          ("current_grid", fun () -> ignore (Matrix.current_grid app));
          ("hits", fun () -> ignore (Matrix.hits app));
          ("screen", fun () -> ignore (Matrix.screen app));
          ("mode", fun () -> ignore (Matrix.mode app));
          ("size", fun () -> ignore (Matrix.size app));
          ("full_size", fun () -> ignore (Matrix.full_size app));
          ("effective_size", fun () -> ignore (Matrix.effective_size app));
          ("pixel_resolution", fun () -> ignore (Matrix.pixel_resolution app));
          ("terminal", fun () -> ignore (Matrix.terminal app));
          ("capabilities", fun () -> ignore (Matrix.capabilities app));
          ("set_cursor", fun () -> Matrix.set_cursor app ~visible:true);
          ( "set_cursor_style",
            fun () -> Matrix.set_cursor_style app ~style:`Block ~blinking:false
          );
          ( "set_cursor_position",
            fun () -> Matrix.set_cursor_position app ~row:1 ~col:1 );
          ( "set_cursor_color",
            fun () -> Matrix.set_cursor_color app ~r:0. ~g:0. ~b:0. );
        ]
      in
      let not_raised =
        Domain.join
          (Domain.spawn (fun () ->
               List.filter_map
                 (fun (name, call) ->
                   match call () with
                   | () -> Some name
                   | exception Invalid_argument _ -> None)
                 calls))
      in
      equal ~msg:"calls that did not raise off the loop's thread" (list string)
        [] not_raised)

(* The caller drives the run: [step] never waits, says [None] when the run
   has nothing to do, and [deadline] says until when the caller may wait.
   The run's time is the steps': a frame asked for comes once the caller's
   time reaches the frame's deadline. *)
let test_step_never_waits () =
  let f = fake () in
  f.on_wait <- (fun _ ~until:_ -> fail "a step waited");
  run ~config:(quiet ~fps:10. ()) f (fun app ->
      let rec until_idle ~now frames =
        match Matrix.step app ~now with
        | Some Frame ->
            Matrix.draw app ignore;
            until_idle ~now (frames + 1)
        | Some _ -> until_idle ~now frames
        | None -> frames
      in
      equal ~msg:"the first frame, then nothing" int 1 (until_idle ~now:0 0);
      equal ~msg:"an idle run has no deadline" (option int) None
        (Matrix.deadline app);
      Matrix.request_redraw app;
      equal ~msg:"no frame before its interval" int 0
        (until_idle ~now:(ms 50) 0);
      equal ~msg:"the deadline is the frame's" (option int)
        (Some (ms 100))
        (Matrix.deadline app);
      equal ~msg:"the frame at its deadline" int 1 (until_idle ~now:(ms 100) 0);
      raises_match ~msg:"a step earlier than the last raises"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> ignore (Matrix.step app ~now:(ms 99))))

(* A size the terminal reported before the run, as the transport of a served
   session writes it ahead of the run, is the run's size from the start on
   an output that is not a terminal: no frame is drawn at another size. *)
let test_size_reported_before_the_run () =
  let f = fake ~input:[ "\027[48;5;30;0;0t" ] () in
  run ~config:(quiet ()) f (fun app ->
      equal ~msg:"the reported size" (pair int int) (30, 5) (Matrix.size app);
      match Matrix.next app with
      | Frame -> Matrix.draw app ignore
      | Event e -> failf "expected the first frame, got %a" Matrix.pp_event e
      | _ -> fail "expected the first frame")

(* A stop requested before the run is the run's first step. *)
let test_stop_before_the_run () =
  let stop = Matrix.Stop.create () in
  Matrix.Stop.request stop;
  let f = fake () in
  Matrix.run_on (backend f) ~stop ~config:(quiet ()) (fun app ->
      match Matrix.next app with
      | Stopped -> ()
      | _ -> fail "expected the run to stop")

let test_post_after_the_run () =
  let f = fake () in
  let app = run ~config:(quiet ()) f Fun.id in
  let wakes = Atomic.get f.wakes in
  Matrix.post app (fun () -> fail "a post after the run ran");
  equal ~msg:"no wake" int wakes (Atomic.get f.wakes)

(* Off the loop's thread, a post waits while 65,536 posts are pending: a
   producer that floods a loop that takes nothing stops there, and while
   the loop takes them, the posts made and not yet taken never pass the
   bound. [spawn] runs the producer on a systhread or a domain, and returns
   its join. *)
let test_posts_wait_above_the_bound spawn () =
  let bound = 65_536 and total = 200_000 in
  let f = fake () in
  f.on_wait <-
    (fun f ~until:_ ->
      while Atomic.get f.wakes = 0 do
        Thread.yield ()
      done;
      Atomic.set f.wakes 0);
  let made = Atomic.make 0 in
  run ~config:(quiet ()) f (fun app ->
      let join =
        spawn (fun () ->
            for i = 1 to total do
              Matrix.post app i;
              Atomic.incr made
            done)
      in
      (* The loop takes nothing until the producer has stopped making
         posts. *)
      let rec settle last =
        Unix.sleepf 0.05;
        let n = Atomic.get made in
        if n < total && n <> last then settle n
      in
      settle (-1);
      equal ~msg:"posts made before the loop takes any" int bound
        (Atomic.get made);
      let received = ref 0 and most = ref 0 in
      while !received < total do
        match Matrix.next app with
        | Posted i ->
            incr received;
            equal ~msg:"in order" int !received i;
            most := max !most (Atomic.get made - !received)
        | Frame -> Matrix.draw app ignore
        | _ -> ()
      done;
      join ();
      is_true
        ~msg:(Printf.sprintf "%d posts pending at most" !most)
        (!most <= bound))

(* A post on the loop's own thread never waits, whatever is pending. *)
let test_loop_posts_never_wait () =
  let f = fake () in
  let total = 100_000 in
  run ~config:(quiet ()) f (fun app ->
      for i = 1 to total do
        Matrix.post app i
      done;
      let received = ref 0 in
      while !received < total do
        match Matrix.next app with
        | Posted _ -> incr received
        | Frame -> Matrix.draw app ignore
        | _ -> ()
      done)

(* The run's end lets a waiting post return, and the posts after it do
   nothing. *)
let test_run_end_ends_the_wait () =
  let f = fake () in
  let made = Atomic.make 0 in
  let producer =
    run ~config:(quiet ()) f (fun app ->
        let producer =
          Domain.spawn (fun () ->
              for i = 1 to 100_000 do
                Matrix.post app i;
                Atomic.incr made
              done)
        in
        while Atomic.get made < 65_536 do
          Unix.sleepf 0.01
        done;
        producer)
  in
  Domain.join producer;
  equal ~msg:"every post returned" int 100_000 (Atomic.get made)

(* A frame is drawn after [Frame]; outside, [draw] raises. A frame not
   drawn is dropped. *)
let test_frame_calls_outside_a_frame () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      raises_match ~msg:"draw"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Matrix.draw app ignore);
      (match Matrix.next app with
      | Frame -> ()
      | _ -> fail "expected the first frame");
      (* The frame is not drawn: the next call drops it. *)
      f.eof <- true;
      (match Matrix.next app with
      | Stopped -> ()
      | _ -> fail "expected the end");
      equal ~msg:"nothing presented" string "" (output f);
      raises_match ~msg:"draw after a dropped frame"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Matrix.draw app ignore);
      match Matrix.next app with Stopped -> () | _ -> fail "Stopped again")

let test_run_gives_back_on_exception () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s -> if String.ends_with ~suffix:"\027[c" s then "\027[?62c" else "");
  (match run f (fun _ -> failwith "boom") with
  | () -> fail "expected the exception"
  | exception Failure _ -> ());
  equal ~msg:"cleanup once" int 1 f.cleanups;
  equal ~msg:"raw mode on, then off" (list bool) [ false; true ] f.raw

(* The end of the input finalizes the decoder: a held paste is cut, and the
   run stops. *)
let test_end_of_input_finalizes_parser () =
  let f = fake ~input:[ "\027[200~partial" ] () in
  f.eof <- true;
  let events = run ~config:(quiet ()) f drain in
  equal ~msg:"a cut paste" (list input_event)
    [ Paste_start; Paste_data "partial"; Paste_end `Cut ]
    events;
  equal ~msg:"cleanup once" int 1 f.cleanups

let test_escape_expires_without_more_input () =
  let f = fake ~input:[ "\027" ] () in
  let events = ref [] in
  run ~config:(quiet ()) f (fun app ->
      match next_event app with
      | Event e -> events := [ e ]
      | _ -> fail "expected a key");
  equal ~msg:"the silence delivers the Escape" (list input_event)
    [ key Escape ]
    !events;
  equal ~msg:"after 50 ms" int (ms 50) f.now

let probe_answer s =
  if String.starts_with ~prefix:"\027 F" s then "\027[?62c" else ""

(* Input read while the probe is answered is the application's, first. *)
let test_startup_preserves_input_and_partial_paste () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun bytes ->
      if String.starts_with ~prefix:"\027 F" bytes then "\027[?62ca\027[200~par"
      else "");
  let events =
    run ~config:(quiet ()) f (fun app ->
        f.input <- f.input @ [ "tial\027[201~b" ];
        f.eof <- true;
        drain app)
  in
  equal ~msg:"one parser, in input order" (list input_event)
    [
      char 'a';
      Paste_start;
      Paste_data "par";
      Paste_data "tial";
      Paste_end `Complete;
      char 'b';
    ]
    events

let test_startup_eof_finalizes_input () =
  let f = fake ~tty:true ~input:[ "a\027[200~par" ] () in
  f.eof <- true;
  let events = run ~config:(quiet ~mode:`Primary ()) f drain in
  equal ~msg:"completed input kept, the partial paste cut" (list input_event)
    [ char 'a'; Paste_start; Paste_data "par"; Paste_end `Cut ]
    events;
  equal ~msg:"cleanup once" int 1 f.cleanups

(* Giving the terminal back runs every undo even if one raises, then
   raises the first exception. *)
let test_give_back_runs_every_undo () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  (match
     run f (fun _ -> f.fail_write <- (fun _ -> failwith "closed output"))
   with
  | () -> fail "expected the failure"
  | exception Failure msg -> equal string "closed output" msg);
  equal ~msg:"raw mode restored" (list bool) [ false; true ] f.raw;
  equal ~msg:"cleanup once" int 1 f.cleanups

(* A step of setup that raises: the undos made so far run in reverse, and
   the exception propagates with its backtrace. *)
let test_setup_rolls_back () =
  let f = fake ~tty:true () in
  let writes = ref 0 in
  let trace = Buffer.create 256 in
  let backend =
    {
      (backend f) with
      write_output =
        (fun buf off len ->
          incr writes;
          Buffer.add_subbytes trace buf off len;
          if !writes = 3 then raise_injected_startup_failure ();
          Buffer.add_subbytes f.output buf off len);
      set_raw_mode =
        (fun raw ->
          Buffer.add_string trace (if raw then "<raw:on>" else "<raw:off>"));
      cleanup = (fun () -> Buffer.add_string trace "<cleanup>");
    }
  in
  let recording = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  let failure =
    Fun.protect
      ~finally:(fun () -> Printexc.record_backtrace recording)
      (fun () ->
        match Matrix.run_on backend ignore with
        | () -> None
        | exception exn -> Some (exn, Printexc.get_raw_backtrace ()))
  in
  (match failure with
  | Some (Injected_startup_failure, bt) ->
      contains ~msg:"the backtrace is the failure's"
        ~sub:"raise_injected_startup_failure"
        (Printexc.raw_backtrace_to_string bt)
  | Some (exn, _) -> failf "unexpected: %s" (Printexc.to_string exn)
  | None -> fail "setup did not fail");
  let trace = Buffer.contents trace in
  in_order ~msg:"raw mode first, given back last before cleanup"
    ~subs:[ "<raw:on>"; "\027[?1049h"; "\027[?1049l"; "<raw:off>"; "<cleanup>" ]
    trace

(* One run holds the terminal at a time; one may start inside [release],
   where the outer run gave it away. *)
let test_nested_runs () =
  let f = fake () in
  let controlling f = { (backend f) with controlling = true } in
  Matrix.run_on (controlling f) ~config:(quiet ()) (fun app ->
      raises_match ~msg:"a run inside a run"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () ->
          Matrix.run_on (controlling (fake ())) ~config:(quiet ()) ignore);
      run ~config:(quiet ()) (fake ()) ignore;
      let inner = fake () in
      Matrix.release app (fun () ->
          Matrix.run_on (controlling inner) ~config:(quiet ()) ignore);
      equal ~msg:"the inner run ran" int 1 inner.cleanups;
      raises_match ~msg:"a release inside a release"
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Matrix.release app (fun () -> Matrix.release app ignore)))

(* Runs on terminals other than the controlling one, each on its domain,
   run side by side. *)
let test_runs_on_other_terminals () =
  let session () =
    Domain.spawn (fun () ->
        let f = fake () in
        f.on_wait <- (fun f ~until:_ -> f.eof <- true);
        run ~config:(quiet ()) f (fun app ->
            Unix.sleepf 0.05;
            ignore (drain app : _ list));
        f.cleanups)
  in
  let a = session () and b = session () in
  equal ~msg:"both ran" (pair int int) (1, 1) (Domain.join a, Domain.join b)

let test_focus_restore_runs_only_after_blur_once () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  let config = Matrix.config ~mouse:`Off ~kitty_keyboard:`Off () in
  run ~config f (fun app ->
      Buffer.clear f.output;
      f.input <- [ "\027[I" ];
      ignore (next_event app);
      not_contains ~msg:"focus without a blur restores nothing"
        ~sub:"\027[?2004h" (output f);
      f.input <- [ "\027[O" ];
      ignore (next_event app);
      not_contains ~msg:"blur restores nothing" ~sub:"\027[?2004h" (output f);
      f.input <- [ "\027[I" ];
      ignore (next_event app);
      contains ~msg:"the first focus after a blur restores the modes"
        ~sub:"\027[?2004h" (output f);
      let after = output f in
      f.input <- [ "\027[I" ];
      ignore (next_event app);
      equal ~msg:"once" string after (output f))

let test_late_capability_response_requests_a_frame () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  run ~config:(quiet ()) f (fun app ->
      present app [];
      let terminal = Matrix.terminal app in
      let caps = Matrix.Terminal.capabilities terminal in
      Matrix.Terminal.set_capabilities terminal { caps with sync = false };
      f.input <- [ "\027[?2026;1$y" ];
      (match Matrix.next app with Frame -> () | _ -> fail "expected a frame");
      is_true ~msg:"the late answer is folded"
        (Matrix.Terminal.capabilities terminal).sync)

(* A reply split across reads, with a read of nothing between, is one
   reply. *)
let test_startup_capability_window_defers_split_response () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  run ~config:(quiet ()) f (fun app ->
      let terminal = Matrix.terminal app in
      let caps = Matrix.Terminal.capabilities terminal in
      Matrix.Terminal.set_capabilities terminal { caps with sync = false };
      f.input <- [ "\027[?2026;"; ""; "1$y" ];
      f.on_wait <- (fun f ~until:_ -> if f.input = [] then f.eof <- true);
      ignore (drain app : _ list);
      is_true ~msg:"the split answer is folded"
        (Matrix.Terminal.capabilities terminal).sync)

(* Regression: a stray cursor report (xterm's Shift+F3, a shell's leftover)
   must not flip the width capabilities. *)
let test_stray_cpr_does_not_flip_capabilities () =
  let f = fake ~input:[ "\027[1;2R"; "\027[1;3R" ] () in
  f.eof <- true;
  run ~config:(quiet ()) f (fun app ->
      ignore (drain app : _ list);
      let caps = Matrix.capabilities app in
      is_false ~msg:"no explicit width" caps.explicit_width;
      is_false ~msg:"no scaled text" caps.scaled_text)

let test_startup_and_late_cpr_do_not_enable_width () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s ->
      if String.starts_with ~prefix:"\027 F" s then "\027[?62;4c"
      else if String.starts_with ~prefix:"\r\027]66" s then "\027[2;1R\027[2;1R"
      else if s = "\027[6n" then "\027[1;3R"
      else "");
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      let check phase =
        let caps = Matrix.capabilities app in
        equal ~msg:phase (pair bool bool) (false, false)
          (caps.explicit_width, caps.scaled_text)
      in
      check "a startup cursor report is no width evidence";
      f.input <- [ "\027[1;2R"; "\027[1;3R" ];
      f.eof <- true;
      ignore (drain app : _ list);
      check "late reports reopen no probe")

let test_ghostty_profile_reaches_runtime () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s ->
      if String.starts_with ~prefix:"\027 F" s then "\027[?2027;2$y\027[?62;4c"
      else "");
  let backend =
    {
      (backend f) with
      env =
        env
          [
            ("TERM", "xterm-ghostty");
            ("TERM_PROGRAM", "ghostty");
            ("TERM_PROGRAM_VERSION", "1.3.1");
          ];
    }
  in
  Matrix.run_on backend (fun app ->
      is_true ~msg:"the probe keeps the wide profile"
        ((Matrix.capabilities app).unicode_width = `Unicode_wide);
      let grid = Matrix.Screen.next_grid (Matrix.screen app) in
      is_true ~msg:"the screen uses it"
        (Matrix.Grid.width_method grid = `Unicode_wide));
  contains ~sub:"\027[?2027h" (output f)

(* The runtime leaves the terminal's grapheme mode as it found it. One that
   answers mode 2027 reset has grapheme clustering, off: the runtime sets the
   mode, measures by grapheme clusters, and resets the mode when it gives the
   terminal back. One that answers it set is left alone both ways. *)
let test_grapheme_mode_as_found () =
  let run setting =
    let f = fake ~tty:true () in
    f.answer <-
      (fun s ->
        if String.starts_with ~prefix:"\027 F" s then
          "\027[?2027;" ^ setting ^ "$y\027[?62;4c"
        else "");
    let backend =
      { (backend f) with env = env [ ("TERM", "xterm-256color") ] }
    in
    Matrix.run_on backend (fun app ->
        is_true ~msg:"grapheme clusters"
          ((Matrix.capabilities app).unicode_width = `Unicode));
    output f
  in
  let reset = run "2" in
  (match (index_of reset "\027[?2027h", index_of reset "\027[?2027l") with
  | Some set, Some unset ->
      is_true ~msg:"answered reset: set, then reset" (unset > set)
  | _ -> fail "answered reset: the mode is set and reset");
  let set = run "1" in
  not_contains ~msg:"answered set: left alone" ~sub:"\027[?2027h" set;
  not_contains ~msg:"answered set: left alone" ~sub:"\027[?2027l" set

let test_cursor_visibility_preserves_blinking () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  run ~config:(quiet ()) f (fun app ->
      Matrix.set_cursor_style app ~style:`Line ~blinking:true;
      Matrix.set_cursor ~visible:true app;
      Matrix.set_cursor_position app ~row:1 ~col:1;
      present app []);
  contains ~msg:"the blinking style" ~sub:"\027[5 q" (output f)

(* Regression: giving the terminal back must not clobber appearance the
   application never touched. *)
let test_give_back_leaves_untouched_appearance_alone () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  run ~config:(quiet ()) f (fun app ->
      present app [];
      Buffer.clear f.output);
  let out = output f in
  not_contains ~msg:"title" ~sub:"\027]0;" out;
  not_contains ~msg:"cursor color" ~sub:"\027]112" out;
  not_contains ~msg:"cursor style" ~sub:"\027[0 q" out

let test_give_back_restores_touched_cursor_appearance () =
  let f = fake ~tty:true () in
  f.answer <- probe_answer;
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present app [ "x" ] ~required:1;
      Matrix.set_cursor ~visible:true app;
      Matrix.set_cursor_style app ~style:`Line ~blinking:false;
      Matrix.set_cursor_color app ~r:1. ~g:0. ~b:0.;
      Matrix.set_cursor_position app ~row:1 ~col:1;
      present app [ "x" ];
      Buffer.clear f.output);
  let out = output f in
  contains ~msg:"style" ~sub:"\027[0 q" out;
  contains ~msg:"color" ~sub:"\027]112" out

(* Frame output *)

let test_unchanged_frame_emits_no_bytes () =
  List.iter
    (fun mode ->
      let f = fake () in
      run ~config:(quiet ~mode ()) f (fun app ->
          present app [ "stable" ];
          is_true ~msg:"the first frame writes" (output f <> "");
          Buffer.clear f.output;
          present app [ "stable" ];
          equal ~msg:"an unchanged frame writes nothing" string "" (output f)))
    [ `Primary; `Alt ]

let major_words_allocated f =
  Gc.full_major ();
  let before = Gc.quick_stat () in
  f ();
  let after = Gc.quick_stat () in
  after.major_words -. before.major_words

let test_frame_reuses_render_capacity () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      present app [];
      present app [];
      let unchanged = major_words_allocated (fun () -> present app []) in
      let changed = major_words_allocated (fun () -> present app [ "X" ]) in
      is_true
        ~msg:(Printf.sprintf "unchanged frame: %.0f major words" unchanged)
        (unchanged <= 1024.);
      is_true
        ~msg:(Printf.sprintf "changed frame: %.0f major words" changed)
        (changed <= 1024.))

let test_cursor_only_frame_emits_output () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      Matrix.set_cursor app ~visible:true;
      present app [ "stable" ];
      Buffer.clear f.output;
      Matrix.set_cursor_position app ~row:1 ~col:3;
      present app [ "stable" ];
      contains ~msg:"the cursor moves" ~sub:"\027[3G" (output f))

(* The terminal's cursor style at startup is unknown: the first frame emits
   it even when it matches the default. *)
let test_first_frame_emits_cursor_style () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      Matrix.set_cursor ~visible:true app;
      Matrix.set_cursor_style app ~style:`Block ~blinking:true;
      Matrix.set_cursor_position app ~row:1 ~col:1;
      present app [ "stable" ];
      contains ~sub:"\027[1 q" (output f))

let test_visible_cursor_reemits_hidden_visual_state () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      let draw ~visible =
        Matrix.set_cursor ~visible app;
        Matrix.set_cursor_style app ~style:`Block ~blinking:true;
        Matrix.set_cursor_color app ~r:1. ~g:0. ~b:0.;
        Matrix.set_cursor_position app ~row:1 ~col:1;
        present app [ "stable" ]
      in
      draw ~visible:false;
      not_contains ~msg:"hidden: no style" ~sub:"\027[1 q" (output f);
      not_contains ~msg:"hidden: no color" ~sub:"\027]12;#ff0000\007" (output f);
      Buffer.clear f.output;
      draw ~visible:true;
      contains ~msg:"visible: style" ~sub:"\027[1 q" (output f);
      contains ~msg:"visible: color" ~sub:"\027]12;#ff0000\007" (output f))

let test_frame_above_default_capacity () =
  let width = 96 and height = 96 in
  let f = fake ~size:(width, height) () in
  let caps =
    {
      (Matrix.Terminal.Caps.derive (Matrix.Terminal.Caps.evidence no_env)) with
      hyperlinks = true;
    }
  in
  let payload = String.make 240 'x' in
  let draw grid =
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let style =
          Matrix.Ansi.Style.hyperlink
            (Printf.sprintf "https://example.com/%d/%s"
               ((y * width) + x)
               payload)
            Matrix.Ansi.Style.default
        in
        Matrix.Grid.draw_text grid ~x ~y ~text:"X" ~style
      done
    done
  in
  run ~config:(quiet ~mode:`Primary ~caps ()) f (fun app ->
      ignore (frame ~required:height ~draw app [] : _ list);
      is_true ~msg:"a frame above 2 MiB"
        (String.length (output f) > 2 * 1024 * 1024);
      Buffer.clear f.output;
      Matrix.request_redraw app;
      let rec frame () =
        match Matrix.next app with
        | Frame -> ()
        | Event (Resize _) -> frame ()
        | _ -> fail "expected a frame"
      in
      frame ();
      (* The frame's drawing allocates; its presentation is measured. *)
      let words = ref 0. in
      Matrix.draw app (fun grid ->
          draw grid;
          Gc.full_major ();
          words := (Gc.quick_stat ()).major_words);
      let words = (Gc.quick_stat ()).major_words -. !words in
      equal ~msg:"unchanged" int 0 (String.length (output f));
      is_true ~msg:(Printf.sprintf "%.0f major words" words) (words <= 1024.))

(* A partial frame write leaves synchronized output, a scroll region or
   autowrap off: giving the terminal back, or away, resets each, every one
   tried even if one fails. *)
let test_partial_frame_restores_transient_modes () =
  List.iter
    (fun (cut, fail_restore) ->
      List.iter
        (fun release ->
          let f = fake () in
          let bytes = Buffer.create 128 in
          let failing = ref false and restore_failed = ref false in
          f.fail_write <-
            (fun frame ->
              if !failing then (
                let stop =
                  Option.get (index_of frame cut) + String.length cut
                in
                Buffer.add_substring bytes frame 0 stop;
                failwith "partial frame write")
              else (
                Buffer.add_string bytes frame;
                if fail_restore && frame = "\027[?2026l" then (
                  restore_failed := true;
                  failwith "failed synchronized output reset")));
          let caps =
            {
              (Matrix.Terminal.Caps.derive
                 (Matrix.Terminal.Caps.evidence no_env))
              with
              sync = true;
            }
          in
          let backend = { (backend f) with output_is_tty = true } in
          (try
             Matrix.run_on backend ~config:(quiet ~mode:`Primary ~caps ())
               (fun app ->
                 Matrix.static_write app
                   (static_grid
                      (String.concat "\n" (List.init 24 (fun _ -> "row"))));
                 Matrix.request_redraw app;
                 (match Matrix.next app with
                 | Frame -> ()
                 | _ -> fail "expected a frame");
                 (match
                    Matrix.draw app (fun _ ->
                        Buffer.clear bytes;
                        failing := true)
                  with
                 | () -> fail "expected the partial write"
                 | exception Failure m -> equal string "partial frame write" m);
                 failing := false;
                 if release then Matrix.release app ignore)
           with Failure _ -> ());
          in_order ~msg:"every transient mode is reset"
            ~subs:[ cut; "\027[?2026l"; "\027[?7h" ]
            (Buffer.contents bytes);
          (* Resetting a scrolling region would move the cursor to the
             first row, away from the region's origin. *)
          not_contains ~msg:"no scrolling region to reset" ~sub:"\027[r"
            (Buffer.contents bytes);
          equal ~msg:"the failing reset is exercised" bool fail_restore
            !restore_failed;
          equal ~msg:"cleanup once" int 1 f.cleanups)
        [ false; true ])
    [ ("\027[?2026h", false); ("\027[?7l", false); ("row", true) ]

(* Resize *)

(* The terminal's size is applied when a frame is next drawn: a storm of
   reports is one [Resize], returned just before the frame drawn at that
   size. *)
let test_resize_before_its_frame () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      present app [];
      f.size <- (100, 30);
      f.signals <- [ Winch ];
      Matrix.request_redraw app;
      (match Matrix.next app with
      | Event (Resize (100, 30)) -> ()
      | _ -> fail "expected the resize");
      equal ~msg:"applied" (pair int int) (100, 30) (Matrix.size app);
      (match Matrix.next app with
      | Frame -> ()
      | _ -> fail "expected its frame");
      Matrix.draw app (fun grid ->
          equal ~msg:"the grid has the size" int 100 (Matrix.Grid.width grid));
      (* A storm: three reports, one resize. *)
      f.size <- (110, 30);
      f.signals <- [ Winch; Winch ];
      f.input <- [ "\027[48;32;120;0;0t" ];
      let events = frame app [] in
      equal ~msg:"the last size, once" (list input_event)
        [ Resize (120, 32) ]
        events)

let test_resize_zero_and_same_size_are_ignored () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      present app [];
      f.size <- (0, 0);
      f.signals <- [ Winch ];
      equal ~msg:"zero" (list input_event) [] (frame app []);
      f.size <- (80, 24);
      f.signals <- [ Winch ];
      equal ~msg:"same size" (list input_event) [] (frame app []);
      f.eof <- true;
      f.on_wait <-
        (fun _ ~until -> equal ~msg:"no spin" (option int) None until);
      equal ~msg:"settled" (list input_event) [] (drain app))

(* In [`Primary] mode a resize reports the usable size, the live region. *)
let test_resize_reports_usable_size () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s ->
      if String.starts_with ~prefix:"\027 F" s then "\027[?62c"
      else if String.ends_with ~suffix:"\027[6n" s then "\027[15;1R"
      else "");
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present app [ "x" ];
      f.size <- (100, 30);
      f.signals <- [ Winch ];
      match frame app [ "x" ] with
      | [ Resize (cols, rows) ] ->
          equal ~msg:"the usable size" (pair int int) (Matrix.size app)
            (cols, rows);
          is_true ~msg:"not the terminal's rows" (rows <> 30)
      | _ -> fail "expected one resize")

let test_resize_clamps_primary_render_offset () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s ->
      if String.starts_with ~prefix:"\027 F" s then "\027[?62c"
      else if s = "\027[6n" then "\027[21;1R"
      else "");
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present app [];
      f.answer <- (fun _ -> "");
      f.size <- (80, 12);
      f.signals <- [ Winch ];
      ignore (frame app [] : _ list);
      equal ~msg:"the terminal" (pair int int) (80, 12) (Matrix.full_size app);
      equal ~msg:"the minimum live height" (pair int int) (80, 1)
        (Matrix.size app))

(* Primary sizing *)

(* Through a terminal: a VTE fed everything the runtime writes, whose
   replies, cursor reports among them, are the runtime's input. [shell] is
   the output before the application starts. *)
type term = { vte : Vte.t; f : fake }

let vte_term ?(shell = "") ~cols ~rows () =
  let vte = Vte.create ~scrollback:100 ~rows ~cols () in
  ignore (Vte.feed_string vte shell : _ list);
  let f = fake ~tty:true ~size:(cols, rows) () in
  f.answer <- (fun s -> Matrix_input.Reply.encode (Vte.feed_string vte s));
  { vte; f }

let run_term ?(mode = `Primary) ?(min_tui_height = 1) ?caps ?(mouse = `Off) term
    body =
  let config =
    Matrix.config ~mode ~mouse ~bracketed_paste:false ~kitty_keyboard:`Off ?caps
      ~min_tui_height ()
  in
  run ~config term.f body

let resize term ~cols ~rows =
  ignore (Vte.resize term.vte ~rows ~cols : Matrix_input.Reply.t list);
  term.f.size <- (cols, rows);
  term.f.signals <- term.f.signals @ [ Matrix.Backend.Winch ]

let screen_rows term =
  List.map rtrim (String.split_on_char '\n' (Vte.to_string term.vte))

(* In [`Primary] mode the live region is drawn below its origin, a blank
   row of its own, on which the cursor rests while it is hidden, as it is
   by default. The transcript is everything above the origin, the
   scrollback first. *)
let origin term = fst (Vte.cursor_pos term.vte)

let transcript term =
  scrollback_lines term.vte
  @ List.filteri (fun y _ -> y < origin term) (screen_rows term)

let origin_row term = List.nth (screen_rows term) (origin term)

let live_rows app term =
  let o = origin term and _, live = Matrix.size app in
  List.filteri (fun y _ -> y > o && y <= o + live) (screen_rows term)

let primary_at row =
  let f = fake ~tty:true () in
  f.answer <-
    (fun s ->
      if String.starts_with ~prefix:"\027 F" s then "\027[?62c"
      else if s = "\027[6n" then Printf.sprintf "\027[%d;1R" row
      else "");
  f

let test_primary_required_rows_expand_the_region () =
  let f = primary_at 23 in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present ~required:8 app [];
      equal ~msg:"the region grows" int 8 (snd (Matrix.size app)))

let test_primary_required_rows_ignored_in_alt_mode () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      present ~required:3 app [];
      equal ~msg:"the whole terminal" int 24 (snd (Matrix.size app)))

(* Static rows go above the live region, which keeps its rows. *)
let test_static_rows_take_no_live_rows () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ~min_tui_height:3 ()) f (fun app ->
      equal ~msg:"the rows asked for" (pair int int) (80, 3) (Matrix.size app);
      Matrix.static_write app (static_grid "alpha\nbeta\n");
      equal ~msg:"pending rows take none of them" (pair int int) (80, 3)
        (Matrix.effective_size app);
      present app [];
      equal ~msg:"nor written ones" (pair int int) (80, 3) (Matrix.size app))

let test_prepare_uses_pending_static_effective_region () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      Matrix.static_write app (static_grid "alpha\nbeta\n");
      let _, h = Matrix.effective_size app in
      ignore
        (frame app [] ~draw:(fun grid ->
             equal ~msg:"the grid's height" int h (Matrix.Grid.height grid);
             Matrix.Grid.draw_text grid ~x:0 ~y:(h - 1) ~text:"live")
          : _ list);
      in_order ~msg:"static, then live" ~subs:[ "alpha"; "live" ] (output f))

let test_static_writes_are_flushed_fifo () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      Matrix.static_write app (static_grid "first\n");
      Matrix.static_write app (static_grid "second\n");
      present app []);
  in_order ~subs:[ "first"; "second" ] (output f)

let test_static_write_snapshots_selected_rows () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      let grid = static_grid "first\nsecond\nignored" in
      Matrix.static_write ~rows:2 app grid;
      Matrix.Grid.clear grid;
      present app []);
  in_order ~subs:[ "first"; "\r\n"; "second"; "\r\n" ] (output f);
  not_contains ~sub:"ignored" (output f)

(* The erase is written at once, and the next frame draws the live region
   afresh from the screen's first row. *)
let test_static_clear_is_immediate_and_forces_full_repaint () =
  let f = fake () in
  let backend = { (backend f) with output_is_tty = true } in
  Matrix.run_on backend ~config:(quiet ~mode:`Primary ()) (fun app ->
      present app [ "live" ];
      Buffer.clear f.output;
      Matrix.static_clear app;
      equal ~msg:"at once" string "\027[0m\027[H\027[2J" (output f);
      Buffer.clear f.output;
      present app [ "live" ];
      contains ~msg:"repainted" ~sub:"live" (output f);
      not_contains ~msg:"not queued again" ~sub:"\027[H\027[2J" (output f))

let test_static_write_and_live_repaint_share_sync_frame () =
  let f = primary_at 24 in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      set_sync_capable app;
      present app [];
      Buffer.clear f.output;
      let writes = ref 0 in
      f.fail_write <- (fun _ -> incr writes);
      Matrix.static_write app (static_grid "pinned\n");
      present app [ "live" ];
      equal ~msg:"one write" int 1 !writes);
  let out = output f in
  in_order ~subs:[ "\027[?2026h"; "pinned"; "live"; "\027[?2026l" ] out

let test_pinned_static_write_keeps_live_size () =
  let f = primary_at 24 in
  run ~config:(quiet ~mode:`Primary ~min_tui_height:2 ()) f (fun app ->
      present app [];
      let before = Matrix.size app in
      Matrix.static_write app (static_grid "a\nb\nc\n");
      equal ~msg:"pending" (pair int int) before (Matrix.effective_size app);
      present app [];
      equal ~msg:"after" (pair int int) before (Matrix.size app))

let test_primary_required_rows_apply_after_static_flush () =
  let f = primary_at 11 in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present app [];
      Buffer.clear f.output;
      Matrix.static_write app (static_grid "prompt\n");
      present ~required:20 app
        (List.init 20 (fun i -> if i = 19 then "live" else ""));
      equal ~msg:"the rows it requires" (pair int int) (80, 20)
        (Matrix.size app));
  in_order ~msg:"static output, then the frame" ~subs:[ "prompt"; "live" ]
    (output f)

let test_static_snapshots_scroll_exactly_once_per_row () =
  List.iter
    (fun rows ->
      let f = fake ~size:(6, 3) () in
      let lines = List.init rows (fun i -> Printf.sprintf "R%02d" i) in
      run ~config:(quiet ~mode:`Primary ~min_tui_height:3 ()) f (fun app ->
          Matrix.static_write app (static_grid (String.concat "\n" lines));
          present app [ "live" ]);
      let terminal = Vte.create ~rows:3 ~cols:6 ~scrollback:32 () in
      ignore (Vte.feed_string terminal (output f) : _ list);
      equal
        ~msg:(Printf.sprintf "%d rows reach history once" rows)
        (list string) lines
        (scrollback_lines terminal))
    [ 1; 3; 4; 8 ]

(* Static rows replace the rows the live region had, whole, blank rows
   included. *)
let test_static_snapshot_clears_suffixes_and_keeps_blank_rows () =
  let f = fake ~size:(6, 3) () in
  run ~config:(quiet ~mode:`Primary ~min_tui_height:2 ()) f (fun app ->
      present app [ "LIVE!!"; "LIVE!!" ];
      Matrix.static_write app (static_grid "A\n\nB");
      present app []);
  let terminal = Vte.create ~rows:3 ~cols:6 ~scrollback:16 () in
  ignore (Vte.feed_string terminal (output f) : _ list);
  equal (list string) [ "A"; ""; "B" ] (scrollback_lines terminal)

(* A static row is a line the terminal wraps as any output: one wider than a
   terminal narrowed since it was queued takes more rows, and loses
   nothing. *)
let test_queued_static_snapshot_wraps_after_resize () =
  let term = vte_term ~cols:6 ~rows:3 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [];
      Matrix.static_write app (static_grid "ABCDEF\nGHIJKL");
      resize term ~cols:3 ~rows:3;
      ignore (frame app [] : _ list);
      equal ~msg:"the resize applies before the frame" (pair int int) (3, 3)
        (Matrix.full_size app));
  equal ~msg:"each line wrapped, none lost" (list string)
    [ "ABC"; "DEF"; "GHI"; "JKL" ]
    (List.filter (fun l -> l <> "") (scrollback_lines term.vte))

let test_static_write_ignored_in_alt_mode () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      Matrix.static_write app (static_grid "ignored\n");
      equal ~msg:"size" (pair int int) (80, 24) (Matrix.effective_size app);
      present app []);
  not_contains ~sub:"ignored" (output f)

let test_static_replace_replays_inside_next_frame () =
  let f = primary_at 11 in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      set_sync_capable app;
      present app [];
      Buffer.clear f.output;
      Matrix.static_replace app (static_grid "replacement-a\nreplacement-b\n");
      equal ~msg:"deferred" string "" (output f);
      let writes = ref 0 in
      f.fail_write <- (fun _ -> incr writes);
      present app [ "live-after" ];
      equal ~msg:"one write" int 1 !writes);
  let out = output f in
  contains ~sub:"\027[3J" out;
  in_order
    ~subs:
      [
        "\027[?2026h";
        "\027[2J";
        "replacement-a";
        "replacement-b";
        "live-after";
        "\027[?2026l";
      ]
    out

(* [static_clear] erases the screen and drops the static rows queued; the
   live region keeps its rows and is drawn afresh from the screen's first
   row. *)
let test_static_clear_resets_primary_layout () =
  let term = vte_term ~shell:"$0\r\n$1\r\n" ~cols:8 ~rows:6 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "pending\n");
      Matrix.static_clear app;
      equal ~msg:"the screen erased" (list string)
        (List.init 6 (fun _ -> ""))
        (screen_rows term);
      equal ~msg:"the live region keeps its rows" (pair int int) (8, 2)
        (Matrix.size app);
      present app [ "after" ];
      equal ~msg:"afresh below the first row" (list string)
        [ ""; "after"; ""; ""; ""; "" ]
        (screen_rows term))

(* A frame drawn afresh writes the rows down to the last that shows
   anything: the region erased the rest. *)
let test_primary_whole_frame_writes_rows_that_show () =
  let f = fake () in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present ~required:10 app [ "top" ];
      Buffer.clear f.output;
      Matrix.static_write app (static_grid "x\n");
      present app [ "top" ]);
  contains ~sub:"top" (output f);
  not_contains ~msg:"the blank rows are not written" ~sub:(String.make 80 ' ')
    (output f)

(* The live region never asks where the cursor is: no frame waits for a
   report, and none is sent while the mouse is off, but for the two the
   measure of text sizing reads at the start. *)
let test_primary_asks_no_cursor_report () =
  let term = vte_term ~shell:"$0\r\n" ~cols:10 ~rows:6 () in
  run_term term (fun app ->
      present app [ "live" ];
      resize term ~cols:8 ~rows:5;
      present app [ "live" ];
      Matrix.release app ignore;
      present app [ "live" ]);
  let rec count i n =
    match String.index_from_opt (output term.f) i '\027' with
    | Some j
      when j + 3 < String.length (output term.f)
           && String.sub (output term.f) j 4 = "\027[6n" ->
        count (j + 1) (n + 1)
    | Some j -> count (j + 1) n
    | None -> n
  in
  equal ~msg:"the measure's reports alone" int 2 (count 0 0)

(* The kitty protocol keeps a stack of flags for each screen ("The main and
   alternate screens in the terminal emulator must maintain their own,
   independent, keyboard mode stacks", kitty keyboard protocol, "Progressive
   enhancement"): the run pushes its flags on the screen it draws on, and
   pops them there, leaving the shell's screen as it found it. *)
let test_kitty_flags_on_the_run's_screen () =
  let term = vte_term ~cols:20 ~rows:4 () in
  let flags () = (Vte.keyboard term.vte).kitty in
  run ~config:(Matrix.config ~mouse:`Off ()) term.f (fun app ->
      is_true ~msg:"the alternate screen" (Vte.modes term.vte).alternate_screen;
      equal ~msg:"the run's flags, on its screen" int 5 (flags ());
      ignore (Matrix.step app ~now:term.f.now : unit Matrix.step option);
      is_true ~msg:"the capabilities say so, once read"
        ((Matrix.capabilities app).kitty_keyboard = Some 5));
  is_false ~msg:"the primary screen" (Vte.modes term.vte).alternate_screen;
  equal ~msg:"the shell's flags" int 0 (flags ())

(* A screen full of shell output, the prompt on the bottom row. *)
let full_shell = "$0\r\n$1\r\n$2\r\n$ app"

let test_pinned_static_write_scrolls_transcript () =
  let term = vte_term ~shell:full_shell ~cols:8 ~rows:4 () in
  run_term term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "pinned\n");
      present app [ "live" ];
      equal ~msg:"the transcript" (list string)
        [ "$0"; "$1"; "$2"; "$ app"; "pinned" ]
        (transcript term);
      equal ~msg:"the live row" (list string) [ "live" ] (live_rows app term))

let test_multiline_static_write_at_the_pin () =
  let term = vte_term ~shell:full_shell ~cols:8 ~rows:4 () in
  run_term term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "line-a\nline-b\n");
      present app [ "live" ];
      equal (list string)
        [ "$0"; "$1"; "$2"; "$ app"; "line-a"; "line-b" ]
        (transcript term);
      equal (pair int int) (8, 1) (Matrix.size app);
      equal (list string) [ "live" ] (live_rows app term))

let test_static_write_moves_live_region_down () =
  let term = vte_term ~shell:"$ cmd\r\n" ~cols:8 ~rows:6 () in
  run_term term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "settle\n");
      present app [ "live" ];
      equal (list string) [ "$ cmd"; "settle" ] (transcript term);
      equal ~msg:"a blank origin" string "" (origin_row term);
      equal (pair int int) (8, 1) (Matrix.size app);
      equal (list string) [ "live" ] (live_rows app term))

let test_full_height_static_write_scrolls_into_scrollback () =
  let term = vte_term ~cols:12 ~rows:4 () in
  run_term ~min_tui_height:4 term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "INSERTED00\nINSERTED01\n");
      equal ~msg:"the rows below the origin" (pair int int) (12, 3)
        (Matrix.effective_size app);
      present app [ "live" ];
      equal (list string)
        [ "INSERTED00"; "INSERTED01" ]
        (scrollback_lines term.vte);
      equal (list string) [ "live"; ""; "" ] (live_rows app term))

let test_required_rows_show_in_their_frame () =
  let term = vte_term ~shell:"$ ls\r\n$ \r\n$ \r\n$ \r\n" ~cols:12 ~rows:6 () in
  run_term term (fun app ->
      present app [ "one" ];
      equal (pair int int) (12, 1) (Matrix.size app);
      present ~required:4 app [ "one"; "two"; "three"; "four" ];
      equal (list string) [ "one"; "two"; "three"; "four" ] (live_rows app term);
      equal (list string) [ "$ ls"; "$"; "$"; "$" ] (transcript term))

(* The live region begins on a row of its own: the cursor's, at the start
   of a row, or the row below the prompt the cursor ends. *)
let test_primary_starts_below_the_cursor () =
  let term = vte_term ~shell:"$ cmd\r\n" ~cols:10 ~rows:6 () in
  run_term term (fun app ->
      Matrix.static_write app (static_grid "HEADER\n");
      present app [ "live" ];
      equal (list string) [ "$ cmd"; "HEADER" ] (transcript term);
      equal ~msg:"a blank origin" string "" (origin_row term);
      equal (list string) [ "live" ] (live_rows app term));
  let term = vte_term ~shell:"$ cmd\r\n$ " ~cols:10 ~rows:6 () in
  run_term term (fun app ->
      present app [ "live" ];
      equal ~msg:"below the prompt" (list string) [ "$ cmd"; "$" ]
        (transcript term);
      equal (list string) [ "live" ] (live_rows app term))

let test_preserved_static_write_keeps_live_region () =
  let term = vte_term ~cols:12 ~rows:4 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [ "live" ];
      Matrix.static_write ~preserve_live_region:true app
        (static_grid "PRESERVED00\nPRESERVED01\n");
      equal (pair int int) (12, 2) (Matrix.effective_size app);
      present app [ "live" ];
      equal (list string) [ "PRESERVED00"; "PRESERVED01" ] (transcript term);
      equal (pair int int) (12, 2) (Matrix.size app);
      equal (list string) [ "live"; "" ] (live_rows app term))

let test_preserved_static_write_after_transcript_rows () =
  let term = vte_term ~shell:"$0\r\n$1\r\n$2\r\n" ~cols:8 ~rows:6 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [ "live" ];
      Matrix.static_write ~preserve_live_region:true app
        (static_grid "AAAA\nBBBB\n");
      present app [ "live" ];
      equal (list string) [ "$0"; "$1"; "$2"; "AAAA"; "BBBB" ] (transcript term);
      equal (pair int int) (8, 2) (Matrix.size app))

let test_mixed_static_writes () =
  let term = vte_term ~cols:8 ~rows:6 () in
  run_term ~min_tui_height:5 term (fun app ->
      present app [ "live" ];
      Matrix.static_write ~preserve_live_region:true app (static_grid "KEEP\n");
      Matrix.static_write app (static_grid "GROW\n");
      present app [ "live" ];
      equal (list string) [ "KEEP"; "GROW" ] (transcript term);
      equal (pair int int) (8, 5) (Matrix.size app))

let test_full_height_consecutive_static_writes () =
  let term = vte_term ~cols:8 ~rows:4 () in
  run_term ~min_tui_height:4 term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "first");
      Matrix.static_write app (static_grid "second\n");
      present app [ "live" ];
      equal (list string) [ "first"; "second" ] (scrollback_lines term.vte))

let test_primary_required_rows_to_terminal_height () =
  let term = vte_term ~shell:"$0\r\n" ~cols:8 ~rows:6 () in
  run_term term (fun app ->
      present ~required:6 app
        (List.init 6 (fun i -> if i = 4 then "bottom" else ""));
      equal ~msg:"every row but the origin" (pair int int) (8, 5)
        (Matrix.size app);
      equal (list string) [ ""; ""; ""; ""; "bottom" ] (live_rows app term);
      equal ~msg:"the shell's row in the scrollback" (list string) [ "$0" ]
        (transcript term))

let test_primary_one_row_static_area () =
  let term = vte_term ~cols:4 ~rows:2 () in
  run_term term (fun app ->
      present app [ "live" ];
      Matrix.static_write app (static_grid "S0\n");
      present app [ "live" ];
      equal (list string) [ "S0" ] (transcript term);
      equal (list string) [ "live" ] (live_rows app term))

let test_alt_default_cursor_is_shown () =
  let term = vte_term ~cols:6 ~rows:3 () in
  run_term ~mode:`Alt term (fun app ->
      let screen = Matrix.screen app in
      Matrix.Screen.set_cursor screen
        { (Matrix.Screen.cursor screen) with position = None; visible = true };
      present app [ "x" ];
      is_true ~msg:"shows" (Vte.modes term.vte).cursor_visible;
      equal ~msg:"on the bottom row" (pair int int) (2, 0)
        (Vte.cursor_pos term.vte))

(* Hidden, as by default in [`Primary] mode, the cursor rests on the origin
   and never shows; shown, it is on the cell the application puts it, and
   moving it alone is a frame. *)
let test_primary_cursor () =
  let term = vte_term ~cols:6 ~rows:6 () in
  run_term term (fun app ->
      let shows = ref false in
      let has sub s =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
        in
        go 0
      in
      term.f.fail_write <-
        (fun bytes -> if has "\027[?25h" bytes then shows := true);
      present ~required:3 app [ "x" ];
      is_false ~msg:"never shown while hidden" !shows;
      is_false ~msg:"hidden" (Vte.modes term.vte).cursor_visible;
      equal ~msg:"resting on the origin" (pair int int) (0, 0)
        (Vte.cursor_pos term.vte);
      Matrix.set_cursor app ~visible:true;
      Matrix.set_cursor_position app ~row:3 ~col:3;
      present app [ "x" ];
      is_true ~msg:"shown" (Vte.modes term.vte).cursor_visible;
      equal ~msg:"on its cell" (pair int int) (3, 2) (Vte.cursor_pos term.vte);
      Buffer.clear term.f.output;
      Matrix.set_cursor_position app ~row:1 ~col:5;
      present app [ "x" ];
      equal ~msg:"moved" (pair int int) (1, 4) (Vte.cursor_pos term.vte);
      not_contains ~msg:"the frame unchanged, only the cursor" ~sub:"x"
        (output term.f);
      equal ~msg:"the frame stays" (list string) [ "x"; ""; "" ]
        (List.filteri (fun y _ -> y >= 1 && y <= 3) (screen_rows term)))

let test_primary_full_redraw_keeps_cursor_row () =
  let term = vte_term ~cols:6 ~rows:4 () in
  run_term term (fun app ->
      let screen = Matrix.screen app in
      Matrix.Screen.set_cursor screen
        {
          (Matrix.Screen.cursor screen) with
          position = Some (2, 3);
          visible = true;
        };
      present ~required:3 app [ "x" ];
      equal ~msg:"clamped to the last row" (pair int int) (3, 2)
        (Vte.cursor_pos term.vte);
      resize term ~cols:7 ~rows:4;
      present app [ "x" ];
      equal ~msg:"after a redraw afresh" (pair int int) (3, 2)
        (Vte.cursor_pos term.vte);
      equal ~msg:"below the same origin" (list string) [ "x"; ""; "" ]
        (List.filteri (fun y _ -> y >= 1) (screen_rows term)))

let test_primary_full_redraw_paints_background_rows () =
  let term = vte_term ~cols:6 ~rows:4 () in
  run_term term (fun app ->
      ignore
        (frame ~required:2 app [ "x" ] ~draw:(fun grid ->
             Matrix.Grid.fill_rect grid ~x:0 ~y:1 ~width:6 ~height:1
               ~color:Matrix.Ansi.Color.blue)
          : _ list);
      let g = Vte.grid term.vte in
      is_true ~msg:"a background-only row is painted"
        (Matrix.Ansi.Color.equal
           (Matrix.Grid.get_bg g (Matrix.Grid.idx g ~x:5 ~y:2))
           Matrix.Ansi.Color.blue))

(* Rows the frame no longer shows are erased: by the diff, and by a frame
   drawn afresh. *)
let test_primary_erases_stale_rows () =
  let term = vte_term ~cols:8 ~rows:8 () in
  run_term term (fun app ->
      present ~required:3 app [ "a"; "b"; "c" ];
      present app [ "x" ];
      equal ~msg:"by the diff" (list string) [ "x"; ""; "" ]
        (live_rows app term);
      present app [ "a"; "b"; "c" ];
      resize term ~cols:9 ~rows:8;
      present app [ "y" ];
      equal ~msg:"afresh" (list string) [ "y"; ""; "" ] (live_rows app term))

let test_primary_anchor_scrolls_shell_rows () =
  let term = vte_term ~shell:"$0\r\n$1" ~cols:4 ~rows:4 () in
  run_term ~min_tui_height:3 term (fun app ->
      present app [ "live" ];
      equal (list string) [ "$0"; "$1" ] (transcript term);
      equal (pair int int) (4, 3) (Matrix.size app))

let test_primary_minimum_height_survives_shrink () =
  let term = vte_term ~cols:8 ~rows:8 () in
  run_term ~min_tui_height:3 term (fun app ->
      present app [ "live" ];
      resize term ~cols:8 ~rows:2;
      present app [ "live" ];
      resize term ~cols:8 ~rows:8;
      present app [ "live" ];
      Matrix.static_write app
        (static_grid (String.concat "\n" (List.init 8 (Printf.sprintf "S%d"))));
      present app [ "live" ];
      equal ~msg:"the minimum" (pair int int) (8, 3) (Matrix.size app))

(* A terminal that grows pulls history back from its scrollback, and one
   that shrinks drops the rows below its cursor, then moves the top ones
   into its scrollback: the live region moves with its origin's row. *)
let test_primary_resize_keeps_live_region_below_transcript () =
  let history = List.init 12 (Printf.sprintf "history %d") in
  let shell =
    String.concat "" (List.map (fun line -> line ^ "\r\n") history) ^ "$ app"
  in
  let term = vte_term ~shell ~cols:16 ~rows:8 () in
  let ui = [ "+------+"; "| UI   |"; "+------+" ] in
  run_term term (fun app ->
      let expect what =
        equal
          ~msg:(what ^ ": the transcript")
          (list string) (history @ [ "$ app" ]) (transcript term);
        equal
          ~msg:(what ^ ": the live region")
          (list string) ui (live_rows app term)
      in
      present ~required:3 app ui;
      expect "before";
      resize term ~cols:16 ~rows:11;
      present ~required:3 app ui;
      expect "grown";
      resize term ~cols:16 ~rows:6;
      present ~required:3 app ui;
      expect "shrunk")

(* A frame after a resize waits for no answer from the terminal. *)
let test_frame_after_a_resize_waits_for_nothing () =
  let shell = "$0\r\n$1\r\n$2\r\n$3\r\n$4" in
  let term = vte_term ~shell ~cols:5 ~rows:7 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [];
      (* Nothing the terminal answers reaches the run. *)
      term.f.answer <-
        (fun bytes ->
          ignore (Vte.feed_string term.vte bytes : _ list);
          "");
      resize term ~cols:5 ~rows:4;
      let before = term.f.now in
      present app [ "live" ];
      is_true ~msg:"on the frame grid, not after an answer's wait"
        (term.f.now - before <= ms 17);
      equal ~msg:"below the transcript" (list string) [ "live"; "" ]
        (live_rows app term))

let test_resize_before_the_first_frame () =
  let term = vte_term ~shell:"$0\r\n" ~cols:4 ~rows:2 () in
  run_term ~min_tui_height:2 term (fun app ->
      resize term ~cols:4 ~rows:3;
      present app [ "live" ];
      equal ~msg:"the shell's row stays" (list string) [ "$0" ]
        (transcript term);
      equal ~msg:"the live region is below it" (list string) [ "live"; "" ]
        (live_rows app term))

(* An input that is not a terminal leaves the region where the cursor is:
   no line of the shell's is erased. *)
let test_primary_input_not_a_terminal () =
  let term = vte_term ~shell:"$0\r\n$1\r\n$2\r\n" ~cols:8 ~rows:6 () in
  let backend = { (backend term.f) with input_is_tty = false } in
  Matrix.run_on backend ~config:(quiet ~mode:`Primary ()) (fun app ->
      present app [ "live" ];
      equal ~msg:"the shell's lines" (list string) [ "$0"; "$1"; "$2" ]
        (transcript term);
      equal (list string) [ "live" ] (live_rows app term))

(* A SIGCONT the run did not cause, as after a stop from elsewhere: the
   shell may have written where the cursor rested, and the next frame draws
   below what it wrote. *)
let test_primary_continued_below_the_shell () =
  let term = vte_term ~shell:"$ app\r\n" ~cols:12 ~rows:8 () in
  run_term term (fun app ->
      present app [ "live" ];
      ignore (Vte.feed_string term.vte "[1]+ Stopped\r\n$ fg" : _ list);
      term.f.signals <- [ Cont ];
      present app [ "live" ];
      equal ~msg:"the shell's lines stay" (list string)
        [ "$ app"; "[1]+ Stopped"; "$ fg" ]
        (transcript term);
      equal (list string) [ "live" ] (live_rows app term))

(* A line another program writes where the cursor rests stays, above the
   live region the next frame draws: frames that write only the cells that
   changed, then one that writes static rows. Raw mode leaves the line feed
   without its carriage return. With the live region on the screen's last
   rows, the line moves the region's rows below the screen, and the next
   frame goes down to them by line feeds, which scroll, where a cursor
   motion would stop on the last row and the way back up would end above
   the origin. *)
let test_primary_foreign_line_stays () =
  let term = vte_term ~shell:"$ app\r\n" ~cols:12 ~rows:5 () in
  run_term term (fun app ->
      present ~required:3 app [ "a"; "b"; "c" ];
      ignore (Vte.feed_string term.vte "F1\n" : _ list);
      present app [ "a"; "b"; "d" ];
      present app [ "a"; "e"; "d" ];
      Matrix.static_write app (static_grid "T1\n");
      present ~required:3 app [ "a"; "e"; "f" ];
      equal ~msg:"the line stays, above the static row" (list string)
        [ "$ app"; "F1"; "T1" ]
        (List.map String.trim (transcript term));
      equal (list string) [ "a"; "e"; "f" ] (live_rows app term))

(* The run ends: by default the rows of its last frame that show anything
   stay right below the transcript, the cursor below them, and static rows
   queued go above them first; erased ([keep_frame false]), the cursor is
   where the region began. *)
let test_primary_end_erases_or_keeps () =
  let term = vte_term ~shell:"$ app\r\n" ~cols:10 ~rows:8 () in
  run_term term (fun app -> present ~required:3 app [ "a"; "b" ]);
  ignore (Vte.feed_string term.vte "$ " : _ list);
  equal ~msg:"kept, the prompt below" (list string) [ "$ app"; "a"; "b"; "$" ]
    (List.filter (( <> ) "") (screen_rows term));
  equal ~msg:"no blank row left" (list string) [ "$ app"; "a"; "b"; "$" ]
    (List.filteri (fun y _ -> y < 4) (screen_rows term));
  let term = vte_term ~shell:"$ app\r\n" ~cols:10 ~rows:8 () in
  run_term term (fun app ->
      present ~required:3 app [ "a"; "b" ];
      Matrix.keep_frame app false);
  equal ~msg:"erased" (list string) [ "$ app" ]
    (List.filter (( <> ) "") (screen_rows term));
  equal ~msg:"the cursor where the region began" (pair int int) (1, 0)
    (Vte.cursor_pos term.vte);
  is_true ~msg:"shown" (Vte.modes term.vte).cursor_visible;
  let term = vte_term ~shell:"$ app\r\n" ~cols:10 ~rows:8 () in
  run_term term (fun app ->
      present ~required:2 app [ "a"; "b" ];
      Matrix.static_write app (static_grid "done\n"));
  equal ~msg:"static rows queued go above" (list string)
    [ "$ app"; "done"; "a"; "b"; "" ]
    (List.filteri (fun y _ -> y < 5) (screen_rows term));
  equal ~msg:"the cursor below the frame" (pair int int) (4, 0)
    (Vte.cursor_pos term.vte)

(* A run whose application states no height, neither [min_tui_height] nor
   [primary_required_rows]. *)
let run_unstated ?(input_is_tty = true) term body =
  let config =
    Matrix.config ~mode:`Primary ~mouse:`Off ~bracketed_paste:false
      ~kitty_keyboard:`Off ()
  in
  Matrix.run_on { (backend term.f) with input_is_tty } ~config body

let reports term =
  let out = output term.f and n = ref 0 in
  String.iteri
    (fun i c ->
      if
        c = '\027'
        && i + 3 < String.length out
        && String.sub out i 4 = "\027[6n"
      then incr n)
    out;
  !n

(* With no height stated, the live region takes the rows from the cursor's
   row at the start down to the terminal's last row, as many as the live
   region had when it was drawn from the cursor's row: the first frame asks
   where its origin is, has one row until the answer comes, and grows then,
   scrolling one row above it into the scrollback for its origin. *)
let test_primary_unstated_height_fills () =
  let term = vte_term ~shell:"$0\r\n$1\r\n" ~cols:10 ~rows:8 () in
  run_unstated term (fun app ->
      equal ~msg:"one row before the answer" (pair int int) (10, 1)
        (Matrix.size app);
      present app [ "live" ];
      equal ~msg:"none waits for it" (pair int int) (10, 1) (Matrix.size app);
      let events = frame app [ "live" ] in
      equal ~msg:"the rows from the cursor's row down" (list input_event)
        [ Resize (10, 6) ]
        events;
      present app [ "live" ];
      equal ~msg:"the shell's rows above" (list string) [ "$0"; "$1" ]
        (transcript term);
      equal ~msg:"the rows the live region had from the cursor's row"
        (list string)
        [ "$1"; ""; "live"; ""; ""; ""; ""; "" ]
        (screen_rows term));
  (* The cursor past the first column: below its row. *)
  let term = vte_term ~shell:"$0\r\n$ " ~cols:10 ~rows:8 () in
  run_unstated term (fun app ->
      present app [ "live" ];
      ignore (frame app [ "live" ] : _ list);
      equal ~msg:"below the prompt's row" (pair int int) (10, 6)
        (Matrix.size app))

(* An application that states a height asks nothing: [min_tui_height], or
   [primary_required_rows] from its first frame on. *)
let test_primary_stated_height_asks_nothing () =
  let term = vte_term ~cols:10 ~rows:8 () in
  run_term ~min_tui_height:2 term (fun app ->
      present app [ "live" ];
      present app [ "live" ];
      equal (pair int int) (10, 2) (Matrix.size app));
  equal ~msg:"the measure's reports alone" int 2 (reports term);
  let term = vte_term ~cols:10 ~rows:8 () in
  run_unstated term (fun app ->
      present ~required:3 app [ "live" ];
      present app [ "live" ];
      equal (pair int int) (10, 3) (Matrix.size app));
  equal ~msg:"the measure's reports alone" int 2 (reports term)

(* An input that cannot answer, not a terminal or in cooked mode, leaves the
   live region its stated minimum, one row. *)
let test_primary_unstated_without_answers () =
  let term = vte_term ~shell:"$0\r\n" ~cols:10 ~rows:8 () in
  run_unstated ~input_is_tty:false term (fun app ->
      present app [ "live" ];
      present app [ "live" ];
      equal (pair int int) (10, 1) (Matrix.size app));
  equal ~msg:"no report asked" int 0 (reports term)

(* After a resize the live region takes the rows below its origin to the
   terminal's last row again, from the report the next frame asks for; a
   report asked before the resize, which comes after it, gives it no
   rows. *)
let test_primary_unstated_after_resize () =
  let term = vte_term ~shell:"$0\r\n" ~cols:10 ~rows:8 () in
  run_unstated term (fun app ->
      present app [ "live" ];
      ignore (frame app [ "live" ] : _ list);
      equal ~msg:"filled" (pair int int) (10, 7) (Matrix.size app);
      resize term ~cols:10 ~rows:12;
      present app [ "live" ];
      ignore (frame app [ "live" ] : _ list);
      equal ~msg:"to the new last row" int 11
        (origin term + snd (Matrix.size app));
      (* A report held past a resize. *)
      let held = Buffer.create 16 in
      term.f.answer <-
        (fun bytes ->
          Buffer.add_string held
            (Matrix_input.Reply.encode (Vte.feed_string term.vte bytes));
          "");
      resize term ~cols:10 ~rows:10;
      present app [ "live" ];
      resize term ~cols:10 ~rows:6;
      present app [ "live" ];
      term.f.answer <-
        (fun s -> Matrix_input.Reply.encode (Vte.feed_string term.vte s));
      term.f.input <- term.f.input @ [ Buffer.contents held ];
      present app [ "live" ];
      is_true ~msg:"the stale report gives no row the terminal lacks"
        (snd (Matrix.size app) <= 5);
      ignore (frame app [ "live" ] : _ list);
      equal ~msg:"the fresh one fills" int 5
        (origin term + snd (Matrix.size app)))

(* Static rows take theirs from the live region, down to its minimum, and
   [preserve_live_region] keeps its height. *)
let test_primary_static_rows_take_live_rows () =
  let term = vte_term ~cols:10 ~rows:10 () in
  run_term ~min_tui_height:2 term (fun app ->
      present ~required:5 app [ "live" ];
      Matrix.static_write app (static_grid "a\nb\n");
      equal ~msg:"pending rows take theirs" (pair int int) (10, 3)
        (Matrix.effective_size app);
      present app [ "live" ];
      equal ~msg:"written" (pair int int) (10, 3) (Matrix.size app);
      Matrix.static_write app (static_grid "c\nd\ne\n");
      present app [ "live" ];
      equal ~msg:"down to the minimum" (pair int int) (10, 2) (Matrix.size app);
      present ~required:5 app [ "live" ];
      Matrix.static_write ~preserve_live_region:true app (static_grid "f\ng\n");
      equal ~msg:"preserved" (pair int int) (10, 5) (Matrix.effective_size app);
      present app [ "live" ];
      equal (pair int int) (10, 5) (Matrix.size app);
      equal ~msg:"every row in order" (list string)
        [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]
        (transcript term))

(* The bytes an abort writes end the region's write and leave the cursor
   below its rows, as the last write left them; they never reset a
   scrolling region, which would move the cursor to the first row. *)
let test_primary_reset_bytes () =
  let f = fake ~tty:true () in
  let reset = ref "" in
  let backend = { (backend f) with on_fatal = (fun b -> reset := b) } in
  Matrix.run_on backend ~config:(quiet ~mode:`Primary ()) (fun app ->
      present ~required:3 app [ "a" ];
      contains ~msg:"below the three rows" ~sub:"\027[3B\r\n" !reset;
      not_contains ~msg:"no scrolling region" ~sub:"\027[r" !reset;
      Matrix.set_cursor app ~visible:true;
      Matrix.set_cursor_position app ~row:2 ~col:1;
      present app [ "a" ];
      contains ~msg:"from the cursor's row" ~sub:"\027[1B\r\n" !reset)

(* On a terminal that moves a cursor resting in the first column up a row
   when its width changes, the cursor rests in the second column. *)
let test_primary_rest_in_the_second_column () =
  let term = vte_term ~cols:10 ~rows:6 () in
  let caps =
    {
      (Matrix.Terminal.Caps.derive (Matrix.Terminal.Caps.evidence no_env)) with
      lifts_resting_cursor = true;
    }
  in
  run_term ~caps term (fun app ->
      present app [ "live" ];
      equal ~msg:"the second column" (pair int int) (0, 1)
        (Vte.cursor_pos term.vte))

(* With the mouse on, a write that may move the region's origin is followed
   by a cursor report, from which mouse rows count: the live region's first
   row is row 0, and the rows above it are -1, as they are before any
   report. *)
let test_primary_mouse_rows () =
  let term = vte_term ~shell:"$0\r\n$1\r\n" ~cols:20 ~rows:10 () in
  run_term ~mouse:`Sgr_any term (fun app ->
      let mouse row =
        term.f.input <- term.f.input @ [ Printf.sprintf "\027[<0;5;%dM" row ];
        match next_event app with
        | Event (Mouse { y; _ }) -> y
        | _ -> fail "expected a mouse event"
      in
      equal ~msg:"before any report" int (-1) (mouse 4);
      present ~required:2 app [ "a"; "b" ];
      equal ~msg:"the live region's first row" int 0 (mouse 4);
      equal ~msg:"its second" int 1 (mouse 5);
      equal ~msg:"its origin" int (-1) (mouse 3);
      equal ~msg:"the shell's row" int (-1) (mouse 1);
      Matrix.static_write app (static_grid "x\ny\n");
      present app [ "a"; "b" ];
      equal ~msg:"once the origin moved down" int 0 (mouse 6))

(* Input on a virtual clock. Bytes arrive at their times: [typed] as the user
   types them, and the terminal's replies, which [answer] gives for each write
   of the runtime, [rtt] after it. The clock moves only when the runtime
   waits: to the next arrival before its deadline, or to the deadline. At
   [limit] the input ends. *)
type clocked = {
  mutable now : int;
  mutable arrivals : (int * string) list;
  written : Buffer.t;
  mutable waits : (int * int option) list;
  mutable seen : (int * Matrix.event) list;  (** Latest first. *)
  mutable frames : int list;  (** When frames were drawn, latest first. *)
  limit : int;
}

let s seconds = Float.to_int (Float.round (seconds *. 1e9))

let clocked ?(limit = 10.) ?(rtt = 0.) ?(answer = fun _ -> []) typed =
  let c =
    {
      now = 0;
      arrivals = [];
      written = Buffer.create 256;
      waits = [];
      seen = [];
      frames = [];
      limit = s limit;
    }
  in
  let arrive at bytes =
    c.arrivals <-
      List.stable_sort
        (fun (a, _) (b, _) -> compare a b)
        (c.arrivals @ [ (at, bytes) ])
  in
  List.iter (fun (at, bytes) -> arrive (s at) bytes) typed;
  let wait ~until ~reading:_ =
    c.waits <- (c.now, until) :: c.waits;
    let deadline = Option.value until ~default:max_int in
    match c.arrivals with
    | (at, _) :: _ when at <= deadline && at <= c.limit ->
        c.now <- max c.now at;
        true
    | _ when deadline > c.limit ->
        c.now <- max c.now c.limit;
        true
    | _ ->
        c.now <- max c.now deadline;
        false
  in
  let read_input buf off len =
    let due, later = List.partition (fun (at, _) -> at <= c.now) c.arrivals in
    let bytes = String.concat "" (List.map snd due) in
    let n = min len (String.length bytes) in
    c.arrivals <-
      (if n < String.length bytes then
         (c.now, String.sub bytes n (String.length bytes - n)) :: later
       else later);
    Bytes.blit_string bytes 0 buf off n;
    if n > 0 then n else if c.now >= c.limit then 0 else -1
  in
  let backend : Matrix.Backend.t =
    {
      now = (fun () -> c.now);
      wait;
      wake = ignore;
      take_signals = (fun () -> []);
      read_input;
      write_output =
        (fun buf off len ->
          let bytes = Bytes.sub_string buf off len in
          Buffer.add_string c.written bytes;
          List.iter (arrive (c.now + s rtt)) (answer bytes));
      fds = (fun ~reading:_ -> []);
      terminal_size = (fun () -> Some (80, 24));
      window_pixels = (fun () -> None);
      set_raw_mode = ignore;
      flush_input = ignore;
      input_is_tty = true;
      output_is_tty = true;
      controlling = true;
      on_fatal = ignore;
      cleanup = ignore;
      env = no_env;
    }
  in
  (c, backend)

let run_clocked ?(mode = `Alt) ?(env = no_env) ?(start = fun _ -> ())
    ?(on_event = fun _ _ -> ()) (c, backend) =
  Matrix.run_on { backend with env } ~config:(Matrix.config ~mode ())
    (fun app ->
      start app;
      let rec loop () =
        match Matrix.next app with
        | Stopped -> ()
        | Event e ->
            c.seen <- (c.now, e) :: c.seen;
            on_event app e;
            loop ()
        | Frame ->
            c.frames <- c.now :: c.frames;
            Matrix.draw app ignore;
            loop ()
        | Timer _ | Posted _ -> loop ()
      in
      loop ();
      app)

let events c = List.rev_map snd c.seen

(* An xterm-like terminal: it answers the startup probe, which ends with DA1,
   with DA1, and the measure of text sizing with its two cursor reports. *)
let answers_probe bytes =
  if
    String.ends_with ~suffix:"\027[c" bytes
    && String.starts_with ~prefix:"\027 F" bytes
  then [ "\027[?64;1c" ]
  else if String.starts_with ~prefix:"\r\027]66" bytes then
    [ "\027[2;1R\027[2;1R" ]
  else []

(* An ESC waits max(50 ms, 2 RTT) for what follows, and a sequence begun max
   (500 ms, 4 RTT), the round trip measured from the probe's DA1. *)
let test_waits_follow_round_trip () =
  let c, backend =
    clocked ~rtt:0.2 ~answer:answers_probe ~limit:5.
      [ (1., "\027"); (3., "\027[1;") ]
  in
  ignore (run_clocked (c, backend) : _ Matrix.app);
  (match c.seen with
  | [ (at, Key { key = Escape; _ }) ] ->
      equal ~msg:"Escape after 2 RTT of silence" int (s 1.4) at
  | _ -> fail "expected one Escape");
  is_true ~msg:"the sequence waits 4 RTT"
    (List.exists (fun (at, until) -> at = s 3. && until = Some (s 3.8)) c.waits);
  let c, backend = clocked ~limit:2. [ (1., "\027") ] in
  ignore (run_clocked (c, backend) : _ Matrix.app);
  match c.seen with
  | [ (at, Key { key = Escape; _ }) ] ->
      equal ~msg:"50 ms with no round trip" int (s 1.05) at
  | _ -> fail "expected one Escape"

(* A batch is owed until its answer; one the terminal never answers is given
   up after 5 s, and what comes after decodes as input. *)
let test_unanswered_batch_is_given_up () =
  let c, backend =
    clocked ~limit:7.
      [ (1., "\027P>|XTerm(1)\027\\"); (6., "\027[1;2R\027]1x") ]
  in
  ignore
    (run_clocked
       ~start:(fun _ ->
         equal ~msg:"startup waits 500 ms for the probe" int (s 0.5) c.now)
       (c, backend)
      : _ Matrix.app);
  let alt = { Matrix.Input.Keystroke.no_mods with alt = true } in
  let shift = { Matrix.Input.Keystroke.no_mods with shift = true } in
  equal (list input_event)
    [
      key ~mods:shift (F 3);
      key ~mods:alt (Char (Uchar.of_char ']'));
      key (Char (Uchar.of_char '1'));
      key (Char (Uchar.of_char 'x'));
    ]
    (events c)

(* A terminal that answers every query it knows, as xterm does. *)
let xterm ?(xtversion = true) bytes =
  let module P = Matrix.Ansi.Parser in
  let row = ref 10 and out = Buffer.create 64 in
  P.parse bytes
  |> List.iter (function
    | P.Csi { final = 'H'; marker = None; params; _ } ->
        row := P.Params.pn params 0 ~default:1
    | Csi { final = 'n'; marker = None; params; _ }
      when P.Params.value params 0 = Some 6 ->
        Buffer.add_string out (Printf.sprintf "\027[%d;1R" !row)
    | Csi { final = 'p'; marker = Some '?'; intermediates = "$"; params } ->
        Buffer.add_string out
          (Printf.sprintf "\027[?%d;2$y" (P.Params.ps params 0 ~default:0))
    | Csi { final = 'q'; marker = Some '>'; _ } ->
        if xtversion then Buffer.add_string out "\027P>|XTerm(398)\027\\"
    | Csi { final = 'c'; marker = None; params; _ }
      when P.Params.count params = 0 ->
        Buffer.add_string out "\027[?64;1c"
    | Csi { final = 't'; marker = None; params; _ }
      when P.Params.value params 0 = Some 14 ->
        Buffer.add_string out "\027[4;480;800t"
    | _ -> ());
  if Buffer.length out = 0 then [] else [ Buffer.contents out ]

(* However slow the link, up to the 5 s a batch stays owed, no reply is
   typed. *)
let test_late_replies_never_typed () =
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let typed =
    List.init 26 (fun i ->
        (0.1 +. (0.25 *. Float.of_int i), String.make 1 letters.[i]))
  in
  List.iter
    (fun mode ->
      List.iter
        (fun latency ->
          let c, backend =
            clocked ~rtt:latency ~answer:xterm ~limit:12. typed
          in
          let app = run_clocked ~mode (c, backend) in
          let msg =
            Printf.sprintf "%s, latency %g s"
              (match mode with `Alt -> "alt" | `Primary -> "primary")
              latency
          in
          let keys =
            List.filter_map
              (function
                | Matrix.Key { key = Char u; mods; _ }
                  when mods = Matrix.Input.Keystroke.no_mods ->
                    Some (Uchar.to_char u)
                | Resize _ | Color_scheme _ -> None
                | e -> Some (failf "%s: %a" msg Matrix.pp_event e))
              (events c)
          in
          equal ~msg string letters (String.of_seq (List.to_seq keys));
          if mode = `Primary then
            equal
              ~msg:(msg ^ ": down to the last row from row 10")
              (pair int int) (80, 15) (Matrix.size app))
        [ 0.01; 0.3; 0.55; 0.8; 1.; 2.; 3.; 4.; 4.9 ])
    [ `Alt; `Primary ]

let test_probe_reports_never_keys () =
  List.iter
    (fun latency ->
      let c, backend =
        clocked ~rtt:latency ~answer:(xterm ~xtversion:false) ~limit:20. []
      in
      ignore (run_clocked (c, backend) : _ Matrix.app);
      equal
        ~msg:(Printf.sprintf "latency %g s" latency)
        (list input_event) [] (events c))
    [ 6.; 10. ]

let test_application_query () =
  let c, backend = clocked ~rtt:0.1 ~answer:xterm ~limit:2. [] in
  let app =
    run_clocked
      ~start:(fun app ->
        Matrix.query app
          ~owed:{ cursor_reports = 1; string_replies = 1 }
          "\027[>0q\027[6n")
      (c, backend)
  in
  equal (list input_event)
    [
      Reply (Version "XTerm(398)");
      Reply (Cursor { row = 9; col = 0; page = None });
      Reply (Primary_attributes [ 64; 1 ]);
    ]
    (events c);
  is_true ~msg:"the runtime still has the pixel size"
    (Matrix.pixel_resolution app = Some (800, 480));
  let c, backend = clocked ~rtt:0.1 ~answer:xterm ~limit:2. [] in
  ignore
    (run_clocked
       ~start:(fun app -> Matrix.Terminal.send (Matrix.terminal app) "\027[>0q")
       (c, backend)
      : _ Matrix.app);
  is_true ~msg:"typed"
    (List.mem
       (key
          ~mods:{ Matrix.Input.Keystroke.no_mods with alt = true }
          (Char (Uchar.of_char 'P')))
       (events c))

let test_late_answer_changes_modes () =
  let c, backend =
    clocked ~limit:3.
      ~answer:(fun bytes ->
        if
          String.ends_with ~suffix:"\027[c" bytes
          && not (String.starts_with ~prefix:"\027 F" bytes)
        then [ "\027[?64;1c" ]
        else [])
      [ (1., "\027[?0u\027[?1036;2$y\027[?64;1c") ]
  in
  let startup = ref "" in
  let app =
    run_clocked
      ~start:(fun _ ->
        startup := Buffer.contents c.written;
        Buffer.clear c.written)
      (c, backend)
  in
  let startup = !startup in
  is_true ~msg:"S7C1T first" (String.starts_with ~prefix:"\027 F" startup);
  List.iter
    (fun sub -> contains ~msg:"startup" ~sub startup)
    [ "\027[?1004h"; "\027[?2031h"; "\027[?2048h"; "\027[?2004h"; "\027[>4;1m" ];
  not_contains ~msg:"no kitty flags before an answer" ~sub:"\027[>5u" startup;
  not_contains ~msg:"metaSendsEscape untouched" ~sub:"\027[?1036h" startup;
  let late = Buffer.contents c.written in
  (match (index_of late "\027[>5u", index_of late "\027[?u") with
  | Some push, Some query ->
      is_true ~msg:"flags asked after the push" (query > push)
  | _ -> fail "expected the kitty flags pushed and asked for");
  contains ~msg:"modifyOtherKeys off" ~sub:"\027[>4;0m" late;
  contains ~msg:"metaSendsEscape set" ~sub:"\027[?1036h" late;
  equal ~msg:"capabilities" (option int) (Some 0)
    (Matrix.capabilities app).kitty_keyboard;
  List.iter
    (fun sub -> contains ~msg:"given back" ~sub late)
    [ "\027[<u"; "\027[?1036l"; "\027[?1004l"; "\027[?2031l"; "\027[?2048l" ]

let test_meta_sends_escape_kept () =
  let c, backend =
    clocked ~limit:1.
      ~answer:(fun bytes ->
        if String.starts_with ~prefix:"\027 F" bytes then
          [ "\027[?1036;1$y\027[?64;1c" ]
        else [])
      []
  in
  Matrix.run_on backend ignore;
  not_contains ~sub:"\027[?1036"
    ( Buffer.contents c.written |> fun w ->
      match index_of w "\027[?1036$p" with
      | Some i -> String.sub w (i + 9) (String.length w - i - 9)
      | None -> w )

let test_paste_streams_and_aborts () =
  let big = String.make (1024 * 1024) 'p' in
  let c, backend =
    clocked ~limit:5.
      [
        (1., "\027[200~abc");
        (2., "def");
        (3., "\027[201~x");
        (4., "\027[200~");
        (4.1, big);
        (4.2, "\027[201~");
      ]
  in
  ignore
    (run_clocked
       ~on_event:(fun app -> function
         | Paste_data "def" -> Matrix.abort_paste app | _ -> ())
       (c, backend)
      : _ Matrix.app);
  let events = events c in
  let rec take n = function
    | x :: rest when n > 0 -> x :: take (n - 1) rest
    | _ -> []
  in
  equal ~msg:"cut by the application" (list input_event)
    [
      Paste_start;
      Paste_data "abc";
      Paste_data "def";
      Paste_end `Cut;
      char 'x';
      Paste_start;
    ]
    (take 6 events);
  let data =
    String.concat ""
      (List.filter_map
         (function Matrix.Paste_data d -> Some d | _ -> None)
         (List.filteri (fun i _ -> i >= 6) events))
  in
  equal ~msg:"a large paste whole" int (String.length big) (String.length data);
  is_true ~msg:"complete" (List.mem (Matrix.Paste_end `Complete) events)

(* The replies that are application facts reach it: the color scheme, and
   the terminal's own size report, applied at the next frame. *)
let test_application_replies () =
  let c, backend =
    clocked ~limit:3.
      [
        (1., "\027[?997;1n\027[48;24;80;480;800t\027[?2026;1$y\027[8;30;90t");
        (2., "\027[48;30;100;600;1000t\027[?64;1c\027[1;5R");
      ]
  in
  let app = run_clocked (c, backend) in
  equal (list input_event)
    [
      Color_scheme `Dark;
      key ~mods:{ Matrix.Input.Keystroke.no_mods with ctrl = true } (F 3);
      Resize (100, 30);
    ]
    (events c);
  equal ~msg:"pixels"
    (option (pair int int))
    (Some (1000, 600))
    (Matrix.pixel_resolution app)

(* Raw mode makes Ctrl+C a key: matrix gives it no meaning. *)
let test_ctrl_c_is_a_key () =
  let c, backend = clocked ~limit:3. [ (1., "\003"); (2., "x") ] in
  ignore (run_clocked (c, backend) : _ Matrix.app);
  equal (list input_event)
    [
      key
        ~mods:{ Matrix.Input.Keystroke.no_mods with ctrl = true }
        (Char (Uchar.of_char 'c'));
      char 'x';
    ]
    (events c)

let test_tmux_groups () =
  let wrapped_da1 = "\027Ptmux;\027\027[c\027\\" in
  let tmux ~passthrough bytes =
    if String.starts_with ~prefix:"\027 F" bytes then
      [ "\027P>|tmux 3.5a\027\\\027[?1;2;4c" ]
    else if bytes = wrapped_da1 && passthrough then [ "\027[?64;1c" ]
    else if
      String.starts_with ~prefix:"\027Ptmux;\027\027_G" bytes && passthrough
    then [ "\027_Gi=31337;OK\027\\\027[?64;1c" ]
    else if String.ends_with ~suffix:"\027[c" bytes then [ "\027[?1;2;4c" ]
    else []
  in
  let c, backend =
    clocked ~rtt:0.01 ~limit:2. ~answer:(tmux ~passthrough:true) []
  in
  let app = run_clocked ~env:(env [ ("TMUX", "s") ]) (c, backend) in
  let out = Buffer.contents c.written in
  (match (index_of out wrapped_da1, index_of out "\027Ptmux;\027\027_G") with
  | Some check, Some outer ->
      is_true ~msg:"outer after the check" (outer > check)
  | _ -> fail "expected the passthrough check and the outer group");
  let caps = Matrix.capabilities app in
  is_true ~msg:"outer graphics" caps.kitty_graphics;
  is_true ~msg:"tmux" (caps.multiplexer = Some `Tmux);
  is_true ~msg:"tmux's widths" (caps.unicode_width = `Wcwidth);
  let c, backend =
    clocked ~rtt:0.01 ~limit:2. ~answer:(tmux ~passthrough:false) []
  in
  let app = run_clocked ~env:(env [ ("TMUX", "s") ]) (c, backend) in
  let out = Buffer.contents c.written in
  contains ~msg:"passthrough asked" ~sub:wrapped_da1 out;
  not_contains ~msg:"nothing wrapped" ~sub:"\027_G" out;
  is_false ~msg:"no graphics" (Matrix.capabilities app).kitty_graphics

(* A resize while a query is owed on a slow link: the key typed meanwhile is
   applied at once, frames wait for the cursor report that places the live
   region, and the replies stay replies. *)
let test_resize_while_a_query_is_owed () =
  let c, backend =
    clocked ~rtt:0.7 ~answer:xterm ~limit:6.
      [ (3.0, "\027[48;30;100;600;1000t"); (3.1, "k") ]
  in
  ignore
    (run_clocked ~mode:`Primary
       ~start:(fun app ->
         Matrix.query app
           ~owed:{ cursor_reports = 0; string_replies = 1 }
           "\027[>0q")
       (c, backend)
      : _ Matrix.app);
  (match List.find_opt (fun (_, e) -> e = char 'k') c.seen with
  | Some (at, _) -> equal ~msg:"the key at once" int (s 3.1) at
  | None -> fail "the key was not delivered");
  is_true ~msg:"replies are replies"
    (List.mem (Matrix.Reply (Version "XTerm(398)")) (events c));
  is_true ~msg:"the frame at once, waiting for no answer"
    (List.exists (fun at -> at >= s 3.0 && at < s 3.1) c.frames)

(* Giving the terminal away *)

(* A frame drawn across a release is not presented; the next is whole. *)
let test_release_during_a_frame () =
  let f = fake () in
  run ~config:(quiet ()) f (fun app ->
      present app [ "one" ];
      Matrix.request_redraw app;
      (match Matrix.next app with Frame -> () | _ -> fail "expected a frame");
      Matrix.draw app (fun grid ->
          draw_rows grid [ "two" ];
          Matrix.release app ignore;
          Buffer.clear f.output);
      equal ~msg:"not presented" string "" (output f);
      present app [ "one" ];
      contains ~msg:"the next frame is whole" ~sub:"one" (output f))

(* The loop does not run while the terminal is given away: nothing reads
   it. *)
let test_release_reads_nothing () =
  let f = fake ~input:[ "x" ] () in
  run ~config:(quiet ()) f (fun app ->
      present app [];
      let reads = f.reads in
      Matrix.release app (fun () -> f.input <- [ "typed for the child" ]);
      equal ~msg:"no read during f" int reads f.reads;
      equal ~msg:"input left unread is discarded" int 1 f.flushes)

(* Keys decoded and not yet returned when the terminal is given away were
   typed for the program that gets it, which cannot have them: they are
   dropped. Keys typed once it is taken back are the application's. *)
let test_release_drops_typeahead () =
  let f = fake ~input:[ "ab" ] () in
  run ~config:(quiet ()) f (fun app ->
      (match next_event app with
      | Event e -> equal ~msg:"a" input_event (char 'a') e
      | _ -> fail "expected a");
      Matrix.release app ignore;
      f.input <- [ "k" ];
      match next_event app with
      | Event e -> equal ~msg:"b is dropped, k is read" input_event (char 'k') e
      | _ -> fail "expected k")

(* Before giving the terminal away, the runtime waits for an answer only if
   a reply is owed; replies decoded meanwhile are kept, other input dropped. *)
(* A batch still owed when the run ends was sent, and waited for, already:
   its answer is waited for until as long after it was sent as any answer,
   so a silent terminal's probe is not waited for a second time. *)
let test_settle_counts_from_the_send () =
  let f = fake ~tty:true () in
  run ~config:(quiet ()) f (fun _ -> ());
  equal ~msg:"the run ends at the probe's deadline" int (ms 500) f.now

let test_release_settles_only_when_owed () =
  let f = fake ~tty:true () in
  f.answer <-
    (fun bytes ->
      if String.starts_with ~prefix:"\027 F" bytes then "\027[?62c"
      else if index_of bytes "\027[>0q" <> None then
        "x\027P>|XTerm(398)\027\\\027[?62c"
      else if String.ends_with ~suffix:"\027[c" bytes then "\027[?62c"
      else "");
  run ~config:(quiet ()) f (fun app ->
      present app [];
      ignore (frame app [] : _ list);
      let waits = List.length f.waits in
      Matrix.release app ignore;
      equal ~msg:"nothing owed: no wait" int waits (List.length f.waits);
      Matrix.query app
        ~owed:{ cursor_reports = 0; string_replies = 1 }
        "\027[>0q";
      Matrix.release app ignore;
      match next_event app with
      | Event e ->
          equal ~msg:"the reply is kept, the key dropped" input_event
            (Reply (Version "XTerm(398)")) e
      | _ -> fail "expected the reply")

(* What the decoder held is dropped; a paste the application began to
   receive is cut. *)
let test_release_discards_partial_input () =
  List.iter
    (fun (partial, expected) ->
      let f = fake ~input:[ partial ] () in
      run ~config:(quiet ()) f (fun app ->
          let before =
            if String.length partial > 2 then [ next_event app; next_event app ]
            else (
              present app [];
              [])
          in
          ignore before;
          Matrix.release app ignore;
          f.input <- [ "q" ];
          f.eof <- true;
          equal
            ~msg:(Printf.sprintf "%S across a release" partial)
            (list input_event) expected (drain app)))
    [
      ("\027", [ char 'q' ]);
      ("\226", [ char 'q' ]);
      ("\027[200~unfinished", [ Paste_end `Cut; char 'q' ]);
    ]

(* A program run while the terminal is given away goes on from the
   transcript: the static rows queued for it are written first, the live
   region is erased and the cursor shown at its start. *)
let test_release_hands_over_below_the_transcript () =
  let term = vte_term ~shell:"$ ls\r\n" ~cols:12 ~rows:6 () in
  run_term term (fun app ->
      present ~required:2 app [ "ui one"; "ui two" ];
      Matrix.static_write app (static_grid "done\n");
      Matrix.release app (fun () ->
          equal ~msg:"the transcript, then nothing" (list string)
            [ "$ ls"; "done"; ""; ""; ""; "" ]
            (screen_rows term);
          equal ~msg:"the cursor waits where the live region began"
            (pair int int) (2, 0) (Vte.cursor_pos term.vte);
          is_true ~msg:"and shows" (Vte.modes term.vte).cursor_visible;
          ignore (Vte.feed_string term.vte "$ make\r\nok\r\n" : _ list));
      present ~required:2 app [ "ui one"; "ui two" ];
      equal ~msg:"the program's output joins the transcript" (list string)
        [ "$ ls"; "done"; "$ make"; "ok" ]
        (transcript term);
      equal ~msg:"the live region is below it" (list string)
        [ "ui one"; "ui two" ] (live_rows app term))

(* After a release the live region is drawn from a row of its own, below a
   line the program left without its end. *)
let test_release_draws_below_the_program () =
  let term = vte_term ~shell:"$0\r\n" ~cols:12 ~rows:6 () in
  run_term term (fun app ->
      present app [ "live" ];
      Matrix.release app (fun () ->
          ignore (Vte.feed_string term.vte "no end" : _ list));
      present app [ "live" ];
      equal (list string) [ "$0"; "no end" ] (transcript term);
      equal (list string) [ "live" ] (live_rows app term))

(* The size is read when the terminal is taken back: a resize meanwhile is
   returned before the next frame, which waits for no answer. *)
let test_release_reads_the_size () =
  let f = primary_at 1 in
  run ~config:(quiet ~mode:`Primary ()) f (fun app ->
      present app [];
      f.answer <- (fun _ -> "");
      Matrix.release app (fun () -> f.size <- (100, 30));
      let before = f.now in
      match frame app [] with
      | [ Resize (100, _) ] ->
          equal ~msg:"the terminal" (pair int int) (100, 30)
            (Matrix.full_size app);
          equal ~msg:"at once" int before f.now
      | _ -> fail "expected the resize")

(* Linux's view of what the process catches, from /proc/self/status. *)
let caught signo =
  let line =
    In_channel.with_open_text "/proc/self/status" In_channel.input_lines
    |> List.find (String.starts_with ~prefix:"SigCgt:")
  in
  let mask =
    Int64.of_string
      ("0x" ^ String.trim (String.sub line 7 (String.length line - 7)))
  in
  Int64.logand mask (Int64.shift_left 1L (signo - 1)) <> 0L

let linux = Sys.file_exists "/proc/self/status"
let sigint, sigquit, sigterm, sighup, sigtstp, sigwinch = (2, 3, 15, 1, 20, 28)

(* The runtime takes the signals that end or stop the process at their
   default action; while the terminal is given away, the suspend and
   termination signals are default again (the program that has the terminal
   gets them), while interrupt and quit, which the terminal sends it too, are
   caught and discarded. *)
let test_release_signal_dispositions () =
  if linux && not (caught sigterm) then (
    let f = fake ~tty:true () in
    f.answer <- probe_answer;
    run f (fun app ->
        List.iter
          (fun (name, s) -> is_true ~msg:("taken: " ^ name) (caught s))
          [
            ("INT", sigint);
            ("TERM", sigterm);
            ("HUP", sighup);
            ("TSTP", sigtstp);
            ("WINCH", sigwinch);
          ];
        Matrix.release app (fun () ->
            List.iter
              (fun (name, s) ->
                is_false ~msg:("default during f: " ^ name) (caught s))
              [ ("TERM", sigterm); ("HUP", sighup); ("TSTP", sigtstp) ];
            List.iter
              (fun (name, s) ->
                is_true ~msg:("caught during f: " ^ name) (caught s))
              [ ("INT", sigint); ("QUIT", sigquit) ]);
        is_true ~msg:"TERM taken again" (caught sigterm));
    is_false ~msg:"TERM given back" (caught sigterm))

(* A terminal that is not the controlling one, such as a pseudo-terminal the
   process serves, gets none of the window and job-control signals: its run
   takes only those that end the process. Two runs take them together, and
   the action comes back when the last one ends. *)
let test_signals_of_a_served_terminal () =
  if linux && (not (caught sigterm)) && not (caught sigwinch) then (
    let served () =
      let f = fake ~tty:true () in
      f.answer <- probe_answer;
      { (backend f) with controlling = false }
    in
    Matrix.run_on (served ()) (fun _ ->
        List.iter
          (fun (name, s) -> is_true ~msg:("taken: " ^ name) (caught s))
          [ ("TERM", sigterm); ("HUP", sighup); ("INT", sigint) ];
        List.iter
          (fun (name, s) -> is_false ~msg:("not taken: " ^ name) (caught s))
          [ ("WINCH", sigwinch); ("TSTP", sigtstp) ];
        Matrix.run_on (served ()) ignore;
        is_true ~msg:"still taken after the other run" (caught sigterm));
    is_false ~msg:"given back with the last run" (caught sigterm))

let peek_signal number =
  let behavior = Sys.signal number Sys.Signal_ignore in
  ignore (Sys.signal number behavior : Sys.signal_behavior);
  behavior

(* An application that installed its own action keeps it, and the actions
   the run took are restored when it ends, its own handler for SIGWINCH
   included. *)
let test_signals_left_to_the_application () =
  let signals = [ Sys.sigterm; Sys.sigint; Sys.sighup; Sys.sigwinch ] in
  let handlers = List.map (fun s -> (s, fun _ -> ())) signals in
  let previous =
    List.map (fun (s, h) -> (s, Sys.signal s (Sys.Signal_handle h))) handlers
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (s, p) -> ignore (Sys.signal s p : Sys.signal_behavior))
        previous)
    (fun () ->
      let f = fake ~tty:true () in
      f.answer <- probe_answer;
      run f ignore;
      List.iter
        (fun (s, h) ->
          match peek_signal s with
          | Sys.Signal_handle h' -> is_true ~msg:"the handler is kept" (h == h')
          | _ -> fail "the application's handler was lost")
        handlers)

(* Nothing could continue a job whose SIGTSTP is ignored: no stop. *)
let test_stop_job_when_tstp_is_ignored () =
  let previous = Sys.signal Sys.sigtstp Sys.Signal_ignore in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.signal Sys.sigtstp previous : Sys.signal_behavior))
    (fun () ->
      let f = fake ~tty:true () in
      f.answer <- probe_answer;
      run f (fun app ->
          let raw = f.raw in
          Matrix.stop_job app;
          equal ~msg:"the terminal was not released" (list bool) raw f.raw))

(* Unix backend *)

(* A post from another thread wakes a loop that waits for input that never
   comes. *)
(* A pseudo-terminal the process opened is a terminal, not its controlling
   terminal. *)
let test_unix_controlling () =
  let master, slave = Pty.open_pty () in
  Fun.protect
    ~finally:(fun () ->
      Pty.close master;
      Pty.close slave)
    (fun () ->
      let fd = Pty.file_descr slave in
      let b = Matrix.Backend.unix ~input:fd ~output:fd () in
      is_true ~msg:"a terminal" b.input_is_tty;
      is_false ~msg:"not the controlling one" b.controlling)

(* Two runs on Backend.unix, each on its domain, each with its own wake: a
   post to one is never lost to the other's loop draining a wake. One run
   is posted to with short gaps, waiting often; each post to the other is
   waited for. *)
let test_unix_wakes_per_run () =
  let session () =
    let input, writer = Unix.pipe ~cloexec:true () in
    let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
    let app = ref None
    and handled = Atomic.make 0
    and ready = Atomic.make false in
    let domain =
      Domain.spawn (fun () ->
          Matrix.run ~input ~output ~config:(quiet ()) (fun a ->
              app := Some a;
              Atomic.set ready true;
              let rec loop () =
                match Matrix.next a with
                | Posted 0 -> ()
                | Posted n ->
                    Atomic.set handled n;
                    loop ()
                | Frame ->
                    Matrix.draw a ignore;
                    loop ()
                | Stopped -> ()
                | Event _ | Timer _ -> loop ()
              in
              loop ()))
    in
    while not (Atomic.get ready) do
      Domain.cpu_relax ()
    done;
    (Option.get !app, handled, domain, [ input; writer; output ])
  in
  let a, handled, da, fds_a = session () in
  let b, _, db, fds_b = session () in
  let busy = Atomic.make true in
  let hammer =
    Domain.spawn (fun () ->
        while Atomic.get busy do
          Matrix.post b 1;
          (* Short gaps, so that the other run waits often. *)
          for _ = 1 to 2_000 do
            Domain.cpu_relax ()
          done
        done)
  in
  (* A post whose wake another run took is never handled, however long
     the wait: the guard is a minute, which no host takes to run a turn. *)
  let rec posts i =
    if i <= 200 then (
      Matrix.post a i;
      let start = Unix.gettimeofday () in
      while Atomic.get handled < i && Unix.gettimeofday () -. start < 60. do
        Domain.cpu_relax ()
      done;
      if Atomic.get handled < i then Some i else posts (i + 1))
    else None
  in
  let lost = posts 1 in
  equal ~msg:"no post lost" (option int) None lost;
  Atomic.set busy false;
  Domain.join hammer;
  Matrix.post a 0;
  Matrix.post b 0;
  Domain.join da;
  Domain.join db;
  List.iter Unix.close (fds_a @ fds_b)

let test_unix_post_wakes_the_loop () =
  let input_r, input_w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ input_r; input_w; devnull ])
    (fun () ->
      let backend = Matrix.Backend.unix ~input:input_r ~output:devnull () in
      (* A loop the post did not wake would wait for input forever: the
         test's timeout fails it. *)
      let woken =
        Matrix.run_on backend ~config:(quiet ()) (fun app ->
            let poster =
              Thread.create
                (fun () ->
                  Thread.delay 0.05;
                  Matrix.post app ignore)
                ()
            in
            let rec loop () =
              match Matrix.next app with
              | Posted p ->
                  p ();
                  true
              | Frame ->
                  Matrix.draw app ignore;
                  loop ()
              | _ -> loop ()
            in
            let woken = loop () in
            Thread.join poster;
            woken)
      in
      is_true ~msg:"woken without input" woken)

(* A stop requested from another thread wakes a run that waits for input
   that never comes. *)
let test_unix_stop_wakes_the_loop () =
  let input, writer = Unix.pipe ~cloexec:true () in
  let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ input; writer; output ])
    (fun () ->
      let stop = Matrix.Stop.create () in
      let stopper =
        Thread.create
          (fun () ->
            Thread.delay 0.05;
            Matrix.Stop.request stop)
          ()
      in
      Matrix.run ~input ~output ~stop ~config:(quiet ()) (fun app ->
          let rec loop () =
            match Matrix.next app with
            | Stopped -> ()
            | Frame ->
                Matrix.draw app ignore;
                loop ()
            | _ -> loop ()
          in
          loop ());
      Thread.join stopper)

(* A run takes the environment of the session its terminal runs, a served
   session's client's, in place of the process's. *)
let test_unix_session_environment () =
  let devnull = Unix.openfile "/dev/null" [ O_RDWR; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close devnull)
    (fun () ->
      let truecolor vars =
        Matrix.run ~input:devnull ~output:devnull ~env:(env vars)
          ~config:(quiet ()) (fun app ->
            (Matrix.capabilities app).colors = `Truecolor)
      in
      is_true ~msg:"a truecolor session"
        (truecolor [ ("COLORTERM", "truecolor") ]);
      is_false ~msg:"a session that says nothing" (truecolor []))

(* An output with no window keeps the size its terminal reported in band:
   giving the terminal away and taking it back reads no size to replace it
   with. *)
let test_unix_reported_size_survives_a_release () =
  let input, writer = Unix.pipe ~cloexec:true () in
  let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ input; writer; output ])
    (fun () ->
      let report = "\027[48;5;30;0;0t" in
      ignore (Unix.write_substring writer report 0 (String.length report) : int);
      Matrix.run ~input ~output ~config:(quiet ()) (fun app ->
          equal ~msg:"the reported size" (pair int int) (30, 5)
            (Matrix.size app);
          Matrix.release app ignore;
          equal ~msg:"after a release" (pair int int) (30, 5) (Matrix.size app)))

(* A write the terminal is slow to take is interrupted by a signal the
   process handles, here with an OCaml handler, which resumes no system
   call: the write goes on from the byte where it stopped. The pipe takes
   the first 64 KiB, then the write waits; the signal goes to the writing
   thread alone, the others blocking it. *)
let test_unix_write_cut_by_a_signal () =
  let r, w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ O_RDONLY; O_CLOEXEC ] 0 in
  let previous = Sys.signal Sys.sigusr1 (Sys.Signal_handle ignore) in
  let mask = Thread.sigmask SIG_BLOCK [ Sys.sigusr1 ] in
  Fun.protect
    ~finally:(fun () ->
      ignore (Thread.sigmask SIG_SETMASK mask : int list);
      ignore (Sys.signal Sys.sigusr1 previous : Sys.signal_behavior);
      List.iter Unix.close [ r; devnull ])
    (fun () ->
      let backend = Matrix.Backend.unix ~input:devnull ~output:w () in
      let n = 200_000 in
      let sent = Bytes.init n (fun i -> Char.chr (i * 7 land 255)) in
      let writer =
        Thread.create
          (fun () ->
            ignore (Thread.sigmask SIG_UNBLOCK [ Sys.sigusr1 ] : int list);
            backend.write_output sent 0 n;
            backend.cleanup ();
            Unix.close w)
          ()
      in
      Thread.delay 0.2;
      Unix.kill (Unix.getpid ()) Sys.sigusr1;
      Thread.delay 0.1;
      let received = Buffer.create n and buf = Bytes.create 65536 in
      let rec drain () =
        match Unix.read r buf 0 65536 with
        | 0 -> ()
        | k ->
            Buffer.add_subbytes received buf 0 k;
            drain ()
      in
      drain ();
      Thread.join writer;
      equal ~msg:"the bytes written, once" int n (Buffer.length received);
      is_true ~msg:"in order" (Buffer.to_bytes received = sent))

(* A run whose output is a terminal and whose input is not (a program
   given its input by a pipe) sets modes on that terminal all the same: an
   abort resets them, as for any run. *)
let test_unix_reset_without_terminal_input () =
  let master, slave = Pty.open_pty () in
  let devnull = Unix.openfile "/dev/null" [ O_RDONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close devnull;
      Pty.close master;
      Pty.close slave)
    (fun () ->
      let output = Pty.file_descr slave in
      let b = Matrix.Backend.unix ~input:devnull ~output () in
      Fun.protect ~finally:b.cleanup (fun () ->
          b.on_fatal "RESET";
          (* What an abort does, from OCaml. *)
          Tty.Signal.restore_all ();
          b.on_fatal "";
          let master = Pty.file_descr master in
          let buf = Bytes.create 64 in
          let n =
            match Unix.select [ master ] [] [] 1. with
            | [], _, _ -> 0
            | _ -> Unix.read master buf 0 64
          in
          equal ~msg:"the reset bytes reach the terminal" string "RESET"
            (Bytes.sub_string buf 0 n)))

(* The unix backend's environment is the process's, with the description
   of its TERM. *)
let test_unix_description () =
  let vars =
    [
      "TERMINFO";
      "TERM";
      "COLORTERM";
      "TERM_PROGRAM";
      "TERM_PROGRAM_VERSION";
      "KITTY_WINDOW_ID";
      "TERMUX_VERSION";
    ]
  in
  let saved = List.map (fun v -> (v, Sys.getenv_opt v)) vars in
  let set v x = Unix.putenv v x in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (v, x) -> set v (Option.value x ~default:"")) saved)
    (fun () ->
      List.iter (fun v -> set v "") vars;
      set "TERMINFO" (Filename.concat (Sys.getcwd ()) "data/terminfo");
      let sync term =
        set "TERM" term;
        let devnull = Unix.openfile "/dev/null" [ O_RDWR; O_CLOEXEC ] 0 in
        let b = Matrix.Backend.unix ~input:devnull ~output:devnull () in
        let caps =
          Matrix.Terminal.Caps.derive (Matrix.Terminal.Caps.evidence b.env)
        in
        b.cleanup ();
        Unix.close devnull;
        caps.sync
      in
      is_true ~msg:"a TERM whose description has Sync" (sync "matrix-test");
      is_false ~msg:"a TERM with no description" (sync "matrix-none"))

(* [MATRIX_DEBUG] set to [value] while [f] runs. *)
let with_debug value f =
  Unix.putenv "MATRIX_DEBUG" value;
  Fun.protect ~finally:(fun () -> Unix.putenv "MATRIX_DEBUG" "") f

let contains s sub =
  let n = String.length sub in
  Seq.exists
    (fun i -> String.sub s i n = sub)
    (Seq.init (max 0 (String.length s - n + 1)) Fun.id)

(* A run on descriptors draws the panel over each frame the application
   draws, and dumps the frame before, without it. From the second frame the
   panel shows the time the application spent before the frame. *)
let test_unix_debug_tools () =
  let input, writer = Unix.pipe ~cloexec:true () in
  let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
  let dir = Filename.temp_dir "matrix_debug" "" in
  Fun.protect ~finally:(fun () ->
      List.iter Unix.close [ input; writer; output ])
  @@ fun () ->
  with_debug ("overlay,frames=" ^ dir) @@ fun () ->
  let shown =
    Matrix.run ~input ~output ~config:(quiet ()) (fun app ->
        let rec loop frames =
          match Matrix.next app with
          | Frame ->
              Matrix.draw app (fun grid -> draw_rows grid [ "hello" ]);
              if frames = 1 then Matrix.Grid.to_text (Matrix.current_grid app)
              else (
                Matrix.request_redraw app;
                loop (frames + 1))
          | _ -> loop frames
        in
        loop 0)
  in
  is_true ~msg:"the frame" (contains shown "hello");
  is_true ~msg:"the panel over it" (contains shown "fps");
  is_true ~msg:"the application's time" (contains shown "app:");
  let files = Sys.readdir dir in
  Array.sort compare files;
  equal ~msg:"a dump per frame" (list string)
    [ ".ansi"; ".hits.txt"; ".txt"; ".ansi"; ".hits.txt"; ".txt" ]
    (List.map
       (fun file -> String.sub file 12 (String.length file - 12))
       (Array.to_list files));
  let text =
    In_channel.with_open_bin
      (Filename.concat dir files.(2))
      In_channel.input_all
  in
  is_true ~msg:"the dump holds the frame" (contains text "hello");
  is_false ~msg:"without the panel" (contains text "fps")

(* A [MATRIX_DEBUG] that names something other than a tool fails the run
   before it takes the terminal. *)
let test_unix_debug_unknown () =
  let devnull = Unix.openfile "/dev/null" [ O_RDWR; O_CLOEXEC ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close devnull) @@ fun () ->
  with_debug "overlay,frame" @@ fun () ->
  raises_match ~msg:"the item named"
    (function
      | Invalid_argument m ->
          m = "Matrix.run: MATRIX_DEBUG names no tool \"frame\""
      | _ -> false)
    (fun () ->
      Matrix.run ~input:devnull ~output:devnull ~config:(quiet ()) ignore)

(* A run on a test's terminal reads no [MATRIX_DEBUG]. *)
let test_device_debug_ignored () =
  with_debug "overlay" @@ fun () ->
  let f = fake () in
  run_device f (fun app ->
      settle f app;
      is_false ~msg:"no panel"
        (contains (Matrix.Grid.to_text (Matrix.current_grid app)) "fps"))

(* What Windows lacks: the process signals are POSIX's, and Backend.unix
   waits on Tty.Signal's wake pipe, which Windows has not (matrix.ml). *)
let posix_signals f () =
  if Sys.win32 then skip ~reason:"POSIX signals, which Windows lacks" ()
  else f ()

let unix_backend f () =
  if Sys.win32 then
    skip ~reason:"Backend.unix, which needs a wake pipe Windows lacks" ()
  else f ()

let () =
  Windtrap.run "matrix.runtime"
    [
      group "Loop"
        [
          test "frames on demand" test_frames_on_demand;
          test "frame requests" test_frame_requests;
          test "frame_dt" test_frame_dt;
          test "turn order" test_turn_order;
          test "discrete input is drawn early" test_discrete_input_early;
          test "a flood of discrete input" test_discrete_flood;
          test "discrete input keeps the grid" test_discrete_keeps_the_grid;
          test "input read when nothing is queued"
            test_input_read_when_nothing_is_queued;
          test "timers" test_timers;
          test "timer order against a sorted list" test_timer_order_property;
          test "frames, a property" test_frame_property;
          test "the turn budget, a property" test_turn_budget_property;
          test "post from other domains" test_post_from_other_domains;
          test "calls off the loop raise" test_calls_off_the_loop_raise;
          test "post after the run" test_post_after_the_run;
          test "a post from a systhread waits above the bound"
            (test_posts_wait_above_the_bound (fun f ->
                 let t = Thread.create f () in
                 fun () -> Thread.join t));
          test "a post from a domain waits above the bound"
            (test_posts_wait_above_the_bound (fun f ->
                 let d = Domain.spawn f in
                 fun () -> Domain.join d));
          test "a post on the loop's thread never waits"
            test_loop_posts_never_wait;
          test "the run's end ends a post's wait" test_run_end_ends_the_wait;
          test "a step never waits" test_step_never_waits;
          test "a stop requested before the run" test_stop_before_the_run;
          test "an idle step leaves nothing due, a property"
            test_idle_deadline_property;
          test "frame calls outside a frame" test_frame_calls_outside_a_frame;
        ];
      group "A terminal in process"
        [
          test "a frame asked for outside a frame is drawn at once"
            test_device_frames_asked_outside;
          test "an animation keeps its interval and its grid"
            test_device_animation;
          test "a resize is drawn at once" test_device_resize;
          test "input is decoded whole" test_device_whole_writes;
          test "a silent terminal costs its wait" test_device_silent;
          test "an idle step leaves nothing due, a property"
            test_device_idle_deadline_property;
        ];
      group "Lifecycle"
        [
          test "kitty flags are pushed on the run's screen"
            test_kitty_flags_on_the_run's_screen;
          test "the terminal is given back on an exception"
            test_run_gives_back_on_exception;
          test "end of input finalizes the parser"
            test_end_of_input_finalizes_parser;
          test "Escape expires without more input"
            test_escape_expires_without_more_input;
          test "startup preserves input and partial paste"
            test_startup_preserves_input_and_partial_paste;
          test "startup EOF finalizes input" test_startup_eof_finalizes_input;
          test "a size reported before the run"
            test_size_reported_before_the_run;
          test "giving back runs every undo" test_give_back_runs_every_undo;
          test "setup rolls back" test_setup_rolls_back;
          test "nested runs" test_nested_runs;
          test "runs on other terminals" test_runs_on_other_terminals;
          test "focus restore runs only after blur once"
            test_focus_restore_runs_only_after_blur_once;
          test "late capability response requests a frame"
            test_late_capability_response_requests_a_frame;
          test "split capability response"
            test_startup_capability_window_defers_split_response;
          test "stray CPR does not flip capabilities"
            test_stray_cpr_does_not_flip_capabilities;
          test "startup and late CPR do not enable width"
            test_startup_and_late_cpr_do_not_enable_width;
          test "Ghostty profile reaches runtime"
            test_ghostty_profile_reaches_runtime;
          test "grapheme mode left as found" test_grapheme_mode_as_found;
          test "cursor visibility preserves blinking"
            test_cursor_visibility_preserves_blinking;
          test "giving back leaves untouched appearance alone"
            test_give_back_leaves_untouched_appearance_alone;
          test "giving back restores touched cursor appearance"
            test_give_back_restores_touched_cursor_appearance;
        ];
      group "Frame output"
        [
          test "unchanged frame emits no bytes"
            test_unchanged_frame_emits_no_bytes;
          test "frame reuses render capacity" test_frame_reuses_render_capacity;
          test "cursor-only frame emits output"
            test_cursor_only_frame_emits_output;
          test "first frame emits cursor style"
            test_first_frame_emits_cursor_style;
          test "visible cursor re-emits hidden visual state"
            test_visible_cursor_reemits_hidden_visual_state;
          test "frame above default capacity" test_frame_above_default_capacity;
          test "partial frame restores transient modes"
            test_partial_frame_restores_transient_modes;
        ];
      group "Resize"
        [
          test "resize before its frame" test_resize_before_its_frame;
          test "zero and same size are ignored"
            test_resize_zero_and_same_size_are_ignored;
          test "resize reports usable size" test_resize_reports_usable_size;
          test "primary render offset clamps after resize"
            test_resize_clamps_primary_render_offset;
        ];
      group "Primary sizing"
        [
          test "required rows expand the region"
            test_primary_required_rows_expand_the_region;
          test "required rows ignored in alt mode"
            test_primary_required_rows_ignored_in_alt_mode;
          test "static rows take no live rows"
            test_static_rows_take_no_live_rows;
          test "prepare uses pending static effective region"
            test_prepare_uses_pending_static_effective_region;
          test "static_clear erases and draws afresh"
            test_static_clear_resets_primary_layout;
          test "static_replace replays inside next frame"
            test_static_replace_replays_inside_next_frame;
          test "a whole frame writes the rows that show"
            test_primary_whole_frame_writes_rows_that_show;
          test "stale rows are erased" test_primary_erases_stale_rows;
          test "required rows to the terminal's height"
            test_primary_required_rows_to_terminal_height;
          test "no cursor report" test_primary_asks_no_cursor_report;
        ];
      group "Primary static output"
        [
          test "static writes are flushed FIFO"
            test_static_writes_are_flushed_fifo;
          test "static write snapshots selected rows"
            test_static_write_snapshots_selected_rows;
          test "the live region starts below the cursor"
            test_primary_starts_below_the_cursor;
          test "static_clear is immediate and forces a full repaint"
            test_static_clear_is_immediate_and_forces_full_repaint;
          test "pinned static write scrolls the transcript"
            test_pinned_static_write_scrolls_transcript;
          test "multiline static write at the pin"
            test_multiline_static_write_at_the_pin;
          test "static write moves the live region down"
            test_static_write_moves_live_region_down;
          test "one-row static area" test_primary_one_row_static_area;
          test "static write and live repaint share one write"
            test_static_write_and_live_repaint_share_sync_frame;
          test "pinned static write keeps live size"
            test_pinned_static_write_keeps_live_size;
          test "required rows apply after static flush"
            test_primary_required_rows_apply_after_static_flush;
          test "required rows show in their frame"
            test_required_rows_show_in_their_frame;
          test "full-height static write scrolls into scrollback"
            test_full_height_static_write_scrolls_into_scrollback;
          test "a static row wider than the terminal wraps"
            test_queued_static_snapshot_wraps_after_resize;
          test "static snapshots scroll exactly once per row"
            test_static_snapshots_scroll_exactly_once_per_row;
          test "static snapshots clear suffixes and keep blank rows"
            test_static_snapshot_clears_suffixes_and_keeps_blank_rows;
          test "preserved static write keeps live region"
            test_preserved_static_write_keeps_live_region;
          test "preserved static write after transcript rows"
            test_preserved_static_write_after_transcript_rows;
          test "mixed static writes" test_mixed_static_writes;
          test "full-height consecutive static writes"
            test_full_height_consecutive_static_writes;
          test "static_write is ignored in alt mode"
            test_static_write_ignored_in_alt_mode;
        ];
      group "Through a terminal"
        [
          test "alt default cursor is shown" test_alt_default_cursor_is_shown;
          test "the cursor hidden, or where it is put" test_primary_cursor;
          test "full redraw keeps the cursor row"
            test_primary_full_redraw_keeps_cursor_row;
          test "full redraw paints background rows"
            test_primary_full_redraw_paints_background_rows;
          test "anchor scrolls shell rows"
            test_primary_anchor_scrolls_shell_rows;
          test "minimum height survives a shrink"
            test_primary_minimum_height_survives_shrink;
          test "resize keeps the live region below the transcript"
            test_primary_resize_keeps_live_region_below_transcript;
          test "a frame after a resize waits for nothing"
            test_frame_after_a_resize_waits_for_nothing;
          test "resize before the first frame"
            test_resize_before_the_first_frame;
          test "an input that is not a terminal"
            test_primary_input_not_a_terminal;
          test "continued below what the shell wrote"
            test_primary_continued_below_the_shell;
          test "a line another program writes stays"
            test_primary_foreign_line_stays;
          test "the end keeps the live region, or erases it"
            test_primary_end_erases_or_keeps;
          test "an unstated height takes the rows to the bottom"
            test_primary_unstated_height_fills;
          test "a stated height asks nothing"
            test_primary_stated_height_asks_nothing;
          test "an unstated height without answers"
            test_primary_unstated_without_answers;
          test "an unstated height after a resize"
            test_primary_unstated_after_resize;
          test "static rows take theirs from the live region"
            test_primary_static_rows_take_live_rows;
          test "the reset bytes" test_primary_reset_bytes;
          test "resting in the second column"
            test_primary_rest_in_the_second_column;
        ];
      group "Input"
        [
          test "waits follow the round trip" test_waits_follow_round_trip;
          test "an unanswered batch is given up"
            test_unanswered_batch_is_given_up;
          test "late replies are never typed" test_late_replies_never_typed;
          test "probe reports are never keys" test_probe_reports_never_keys;
          test "the application's queries" test_application_query;
          test "a late answer changes modes" test_late_answer_changes_modes;
          test "metaSendsEscape set is kept" test_meta_sends_escape_kept;
          test "paste streams and aborts" test_paste_streams_and_aborts;
          test "Ctrl+C is a key" test_ctrl_c_is_a_key;
          test "replies that are application facts" test_application_replies;
          test "tmux groups" test_tmux_groups;
          test "resize while a query is owed" test_resize_while_a_query_is_owed;
          test "primary mouse rows" test_primary_mouse_rows;
        ];
      group "Release"
        [
          test "release during a frame" test_release_during_a_frame;
          test "release reads nothing" test_release_reads_nothing;
          test "release drops typeahead" test_release_drops_typeahead;
          test "release settles only when owed"
            test_release_settles_only_when_owed;
          test "the settle counts from the send"
            test_settle_counts_from_the_send;
          test "release discards partial input"
            test_release_discards_partial_input;
          test "release hands over below the transcript"
            test_release_hands_over_below_the_transcript;
          test "release draws below the program"
            test_release_draws_below_the_program;
          test "release reads the size" test_release_reads_the_size;
        ];
      group "Signals"
        [
          test "dispositions while released"
            (posix_signals test_release_signal_dispositions);
          test "signals of a served terminal"
            (posix_signals test_signals_of_a_served_terminal);
          test "signals left to the application"
            (posix_signals test_signals_left_to_the_application);
          test "stop_job when SIGTSTP is ignored"
            (posix_signals test_stop_job_when_tstp_is_ignored);
        ];
      group "Unix backend"
        [
          test "a post wakes the loop"
            (unix_backend test_unix_post_wakes_the_loop);
          test "the session's environment"
            (unix_backend test_unix_session_environment);
          test "a stop wakes the loop"
            (unix_backend test_unix_stop_wakes_the_loop);
          test "a reported size survives a release"
            (unix_backend test_unix_reported_size_survives_a_release);
          test "each run has its own wake"
            (unix_backend test_unix_wakes_per_run);
          test "a pseudo-terminal is not the controlling terminal"
            (unix_backend test_unix_controlling);
          test "a write cut by a signal goes on"
            (unix_backend test_unix_write_cut_by_a_signal);
          test "reset with no terminal input"
            (unix_backend test_unix_reset_without_terminal_input);
          test "the description of TERM" (unix_backend test_unix_description);
          test "MATRIX_DEBUG's tools" (unix_backend test_unix_debug_tools);
          test "MATRIX_DEBUG naming no tool"
            (unix_backend test_unix_debug_unknown);
          test "a test's terminal reads no MATRIX_DEBUG"
            test_device_debug_ignored;
        ];
    ]
