(* Suspend only at the presented idle boundary. Mounting and the preflight
   oracle run in setup; a sample resumes the real parser/update/view/
   layout/paint/submit loop and returns at the next presented idle. *)
type _ Effect.t += Presented : unit Effect.t

(* The terminal the benchmark drives: a pipe it writes the input to and an
   unlinked file it reads the output from, which a run takes as it takes any
   descriptors, with the size reported in band and a clock that only the
   benchmark moves. The run measured is the one every program runs; an
   emulator would add its own cost to each sample. *)
module Term = struct
  type t = {
    input_r : Unix.file_descr;
    input_w : Unix.file_descr;
    output : Unix.file_descr;
    mutable now : int;
    mutable ended : bool;
  }

  let rec write_all fd s off =
    if off < String.length s then
      match Unix.write_substring fd s off (String.length s - off) with
      | n -> write_all fd s (off + n)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off

  let resize t ~width ~height =
    write_all t.input_w (Printf.sprintf "\027[48;%d;%d;0;0t" height width) 0

  (* An unlinked file the run writes to. Its name's length follows the
     temporary directory's, so it is opened outside any sample. *)
  let output_file () =
    let path = Filename.temp_file "bench_runtime" ".out" in
    let output =
      Unix.openfile path [ O_RDWR; O_APPEND; O_TRUNC; O_CLOEXEC ] 0o600
    in
    Unix.unlink path;
    output

  (* A terminal writing to [output], emptied, or to a file of its own. *)
  let create ?output ~width ~height () =
    let input_r, input_w = Unix.pipe ~cloexec:true () in
    let output =
      match output with
      | Some output ->
          Unix.ftruncate output 0;
          output
      | None -> output_file ()
    in
    let t = { input_r; input_w; output; now = 0; ended = false } in
    resize t ~width ~height;
    t

  (* Closes the input once the run has ended. *)
  let release t = try Unix.close t.input_r with Unix.Unix_error _ -> ()
  let feed t s = write_all t.input_w s 0
  let advance t ns = t.now <- t.now + ns

  let stop t =
    if not t.ended then (
      t.ended <- true;
      Unix.close t.input_w)

  let written t =
    let length = (Unix.fstat t.output).st_size in
    let bytes = Bytes.create length in
    ignore (Unix.lseek t.output 0 SEEK_SET : int);
    let rec go off =
      if off < length then
        match Unix.read t.output bytes off (length - off) with
        | 0 -> ()
        | n -> go (off + n)
    in
    go 0;
    Bytes.to_string bytes

  (* What a terminal of [width] by [height] shows of the output. *)
  let screen t ~width ~height =
    let vte = Vte.create ~rows:height ~cols:width () in
    ignore (Vte.feed_string vte (written t) : Matrix.Input.Reply.t list);
    Vte.to_string vte
end

type model = { editor : Mosaic.Editor.t; scroll : Mosaic.Scroll.t }
type msg = Edit of Mosaic.Editor.t | Scroll of Mosaic.Scroll.t

(* A resize alternates the terminal between its initial size and the one
   it carries. *)
type workload = Keystroke | Wheel_burst | Resize of int * int

type driver = {
  terminal : Term.t;
  continuation : (unit, unit) Effect.Deep.continuation option ref;
  wheel_down : string;
  wheel_up : string;
  mutable alternate : bool;
}

(* A plain modern terminal: truecolor and Unicode width tables, no kitty
   extensions, so that the measures never depend on the environment. *)
let caps : Matrix.Terminal.capabilities =
  {
    rgb = true;
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

(* An empty environment: the run reads no TERM, so loads no terminfo
   description, whose size would follow the machine's database. *)
let env = Matrix.Terminal.Caps.env (fun _ -> None) ~win32:false
let initial_width = 250
let initial_height = 70

(* In [`Primary] mode the application is an inline region under 20 rows of
   earlier output, so its live region is shorter than the terminal: its scroll
   box takes 40 of the terminal's 70 rows. *)
let primary_viewport_height = 40

let earlier_output =
  Mosaic.fragment
    (List.init 20 (fun i ->
         Mosaic.text (Printf.sprintf "earlier output %02d" i)))

let none = Mosaic.Cmd.none

let block i =
  Mosaic.box ~key:(string_of_int i) ~flex_direction:Mosaic.Flex_direction.Column
    ~flex_shrink:0.
    [
      Mosaic.text (Printf.sprintf "message %04d" i);
      Mosaic.text
        "The worker completed its checkpoint; the next request can reuse the \
         retained result.";
      Mosaic.text "";
    ]

let blocks_view blocks =
  Mosaic.fragment (List.init blocks (fun i -> Mosaic.memo ~key:i block))

let transcript_view blocks = Mosaic.memo ~key:blocks blocks_view

let view ~mode blocks model =
  let size, flex_grow, flex_basis, flex_shrink, scroll_size =
    match mode with
    | `Alt ->
        ( Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100),
          Some 1.,
          Some (Mosaic.px 0),
          None,
          None )
    | `Primary ->
        ( Mosaic.size_wh (Mosaic.pct 100) Mosaic.auto,
          None,
          None,
          Some 0.,
          Some
            (Mosaic.size_wh (Mosaic.pct 100)
               (Mosaic.px primary_viewport_height)) )
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~size
    [
      Mosaic.scroll_box ?flex_grow ?flex_basis ?flex_shrink ?size:scroll_size
        ~min_size:(Mosaic.size ~width:0 ~height:0)
        ~scroll:model.scroll
        ~on_scroll:(fun next -> Scroll next)
        ~show_scrollbars:false
        [ transcript_view blocks ];
      Mosaic.input
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1))
        ~flex_shrink:0. ~focused:true ~state:model.editor
        ~on_change:(fun next -> Edit next)
        ();
    ]

let resume driver =
  match !(driver.continuation) with
  | None -> failwith "runtime benchmark: missing presented continuation"
  | Some continuation ->
      driver.continuation := None;
      Effect.Deep.continue continuation ()

(* Eight wheel events over the transcript's second row, [row] counting the
   terminal's rows from 1. *)
let wheel ~button ~row =
  String.concat ""
    (List.init 8 (fun _ -> Printf.sprintf "\027[<%d;2;%dM" button row))

let step workload driver =
  driver.alternate <- not driver.alternate;
  (match workload with
  | Keystroke ->
      Term.feed driver.terminal (if driver.alternate then "x" else "\127")
  | Wheel_burst ->
      Term.feed driver.terminal
        (if driver.alternate then driver.wheel_down else driver.wheel_up)
  | Resize (width, height) ->
      let width, height =
        if driver.alternate then (width, height)
        else (initial_width, initial_height)
      in
      Term.resize driver.terminal ~width ~height);
  resume driver;
  if !(driver.continuation) = None then
    failwith "runtime benchmark: loop ended before presentation"

(* Every sample returns to where it started: a key and its erasure, a wheel
   burst and its reverse, a resize and its restoration. Alternating
   directions across samples would make each sample's allocation depend on
   its parity, and thumper could not count it exactly. *)
let measure workload driver =
  step workload driver;
  step workload driver

let close driver =
  Term.stop driver.terminal;
  resume driver

let require condition message =
  if not condition then failwith ("runtime benchmark: " ^ message)

(* Runs [application] over [t], the benchmark driving its steps at [t]'s
   clock: [on_idle] runs whenever the program has nothing to do and nothing
   has come for it, given the run's deadline. *)
let drive ?(mode = `Alt) ?process_perform ~on_idle (t : Term.t) application =
  Mosaic.start ~input:t.input_r ~output:t.output ~env
    ~config:(Matrix.config ~mode ~caps ()) ?process_perform application
    (fun r ->
      (* The clock starts at the run's time, which the steps never precede. *)
      t.now <- Matrix.clock ();
      let rec loop () =
        match Mosaic.step r ~now:t.now with
        | `Stepped -> loop ()
        | `Stopped -> ()
        | `Idle ->
            (match Mosaic.deadline r with
            | Some deadline when deadline <= t.now -> ()
            | until ->
                if not (List.exists Matrix.Tty.readable (Mosaic.fds r)) then
                  on_idle t ~until);
            loop ()
      in
      loop ())

let setup ~mode ~blocks workload () =
  let continuation : (unit, unit) Effect.Deep.continuation option ref =
    ref None
  in
  let accepted =
    ref { editor = Mosaic.Editor.of_string "a"; scroll = Mosaic.Scroll.at 7500 }
  in
  let terminal = Term.create ~width:initial_width ~height:initial_height () in
  let on_idle (terminal : Term.t) ~until =
    match until with
    | Some until -> Term.advance terminal (max 1 (until - terminal.now))
    | None -> Effect.perform Presented
  in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( !accepted,
            match mode with
            | `Alt -> none
            | `Primary -> Mosaic.Cmd.static_commit earlier_output ));
      update =
        (fun message model ->
          let model =
            match message with
            | Edit editor -> { model with editor }
            | Scroll scroll -> { model with scroll }
          in
          accepted := model;
          (model, none));
      view = view ~mode blocks;
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  Effect.Deep.match_with
    (fun () -> drive ~mode ~on_idle terminal application)
    ()
    {
      retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Presented ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  continuation := Some k)
          | _ -> None);
    };
  (* The live region ends at the terminal's last row, below the earlier
     output in [`Primary] mode. *)
  let top = match mode with `Alt -> 0 | `Primary -> 20 in
  let viewport_height =
    match mode with
    | `Alt -> initial_height - 1
    | `Primary -> primary_viewport_height
  in
  let driver =
    {
      terminal;
      continuation;
      wheel_down = wheel ~button:65 ~row:(top + 2);
      wheel_up = wheel ~button:64 ~row:(top + 2);
      alternate = false;
    }
  in
  (* What the terminal shows, at the size last reported. *)
  let screen () =
    let width, height =
      match workload with
      | Resize (width, height) when driver.alternate -> (width, height)
      | _ -> (initial_width, initial_height)
    in
    Term.screen terminal ~width ~height
  in
  require
    (mode = `Alt
    || List.exists
         (String.starts_with ~prefix:"earlier output 19")
         (String.split_on_char '\n' (screen ())))
    "the earlier output is above the live region";
  (* Validate both directions before measuring. Captured ANSI and frame text
     are deliberately inspected here, never copied in the measured loop. *)
  for _ = 1 to 2 do
    let before = screen () in
    let written = String.length (Term.written terminal) in
    step workload driver;
    require
      (not (String.equal before (screen ())))
      "input did not change the presented frame";
    require
      (String.length (Term.written terminal) > written)
      "changed frame emitted no ANSI output";
    match workload with
    | Keystroke ->
        require
          (Mosaic.Editor.text !accepted.editor
          = if driver.alternate then "ax" else "a")
          "editor proposal was not accepted"
    | Wheel_burst ->
        let _, y =
          Mosaic.Scroll.resolve !accepted.scroll ~width:250
            ~height:viewport_height ~content_width:250
            ~content_height:(blocks * 3)
        in
        require
          (y = if driver.alternate then 7508 else 7500)
          (Printf.sprintf "wheel burst resolved to %d" y)
    | Resize _ -> ()
  done;
  driver

(* The loop's own costs, on a small program: the run suspends whenever
   [pause] says so when the loop goes idle, and a sample resumes it. *)

type loop = {
  t : Term.t;
  suspended : (unit, unit) Effect.Deep.continuation option ref;
  dispatch : (unit -> unit) ref;
  views : int ref;
}

let loop_resume loop =
  match !(loop.suspended) with
  | None -> failwith "runtime benchmark: the loop is not suspended"
  | Some k ->
      loop.suspended := None;
      Effect.Deep.continue k ();
      if !(loop.suspended) = None then
        failwith "runtime benchmark: loop ended before it went idle"

let loop_close loop =
  Term.stop loop.t;
  match !(loop.suspended) with
  | Some k ->
      loop.suspended := None;
      Effect.Deep.continue k ()
  | None -> ()

(* Runs [application] over [t] until it goes idle with no deadline left and
   [pause] says so, and returns where it waits: resuming it runs it to the
   next such turn. *)
let start_suspended ?(pause = fun () -> true) (t : Term.t) application =
  let suspended : (unit, unit) Effect.Deep.continuation option ref = ref None in
  let on_idle (t : Term.t) ~until =
    match until with
    | Some until -> Term.advance t (max 1 (until - t.now))
    | None -> if pause () then Effect.perform Presented
  in
  Effect.Deep.match_with
    (fun () ->
      drive ~on_idle ~process_perform:(fun work -> work ()) t application)
    ()
    {
      retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Presented ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  suspended := Some k)
          | _ -> None);
    };
  suspended

(* A program that counts keys, over a 40x2 terminal; its messages from
   elsewhere change nothing. [pause] runs at each idle turn with no deadline
   left. *)
let loop_setup ~pause () =
  let dispatch = ref ignore in
  let views = ref 0 in
  let t = Term.create ~width:40 ~height:2 () in
  let application =
    {
      Mosaic.init =
        (fun () ->
          (0, Mosaic.Cmd.perform (fun d -> dispatch := fun () -> d `Nothing)));
      update =
        (fun msg n ->
          match msg with `Key -> (n + 1, none) | `Nothing -> (n, none));
      view =
        (fun n ->
          incr views;
          Mosaic.text (string_of_int n));
      subscriptions = (fun _ -> Mosaic.Sub.on_key (fun _ -> Some `Key));
    }
  in
  let suspended = start_suspended ~pause t application in
  { t; suspended; dispatch; views }

(* The frames written in [output] from [from]: each begins by hiding the
   cursor. *)
let paints ~from output =
  let hide = "\027[?25l" in
  let n = String.length hide in
  let count = ref 0 in
  for i = from to String.length output - n do
    if String.sub output i n = hide then incr count
  done;
  !count

(* 10,000 idle turns a sample: the loop waits, finds nothing, waits again. *)
let idle_turns () =
  let turns = ref 0 in
  let loop =
    loop_setup
      ~pause:(fun () ->
        incr turns;
        !turns mod 10_000 = 0)
      ()
  in
  require (!(loop.suspended) <> None) "the loop did not go idle";
  loop

(* 1,000 keys in one read, applied, viewed and painted once. *)
let burst () =
  let loop = loop_setup ~pause:(fun () -> true) () in
  let views = !(loop.views) in
  let written = String.length (Term.written loop.t) in
  Term.feed loop.t (String.make 1000 'a');
  loop_resume loop;
  require (!(loop.views) - views = 1000) "a burst is not 1,000 views";
  require
    (paints ~from:written (Term.written loop.t) = 1)
    "a burst is not one paint";
  loop

let burst_sample loop =
  Term.feed loop.t (String.make 1000 'a');
  loop_resume loop

(* 1,000 messages posted from elsewhere, taken and applied: they change
   nothing, so nothing is viewed or painted. *)
let posts loop =
  for _ = 1 to 1000 do
    !(loop.dispatch) ()
  done;
  loop_resume loop

let loop_benches =
  Thumper.group "loop"
    [
      Thumper.bench_with_setup ~setup:idle_turns ~teardown:loop_close
        "idle-turns-10000" loop_resume;
      Thumper.bench_with_setup ~setup:burst ~teardown:loop_close
        "burst-1000-keys" burst_sample;
      Thumper.bench_with_setup
        ~setup:(loop_setup ~pause:(fun () -> true))
        ~teardown:loop_close "posts-1000" posts;
    ]

(* A key in a long list: a focused select or table of [n] items over an
   80x46 terminal, each declared by every view, as an application declares
   them today. A sample presses Down then Up, so the selection and the
   window return to where they were. *)

type list_kind = Select | Table

let list_label i = Printf.sprintf "item %06d" i

let list_application kind n =
  let size = Mosaic.size ~width:60 ~height:40 in
  let view =
    match kind with
    | Select ->
        let items =
          List.init n (fun i -> Mosaic.Select.item i (list_label i))
        in
        fun selected ->
          Mosaic.select ~size ~focused:true ~show_description:false
            ~equal:Int.equal ~selected:(Some selected) ~on_change:Fun.id items
    | Table ->
        let columns =
          [
            Mosaic.Table.column "Name";
            Mosaic.Table.column ~width:(`Fixed 8) "N";
          ]
        in
        let rows =
          List.init n (fun i ->
              Mosaic.Table.row i
                [|
                  Mosaic.Table.cell (list_label i);
                  Mosaic.Table.cell (string_of_int (i * 7));
                |])
        in
        fun selected ->
          Mosaic.table ~size ~focused:true ~columns ~rows ~equal:Int.equal
            ~selected:(Some selected) ~on_change:Fun.id ()
  in
  {
    Mosaic.init = (fun () -> (n / 2, none));
    update = (fun selected _ -> (selected, none));
    view;
    subscriptions = (fun _ -> Mosaic.Sub.none);
  }

let down = "\027[B"
let up = "\027[A"

let list_setup kind n () =
  let t = Term.create ~width:80 ~height:46 () in
  let suspended = start_suspended t (list_application kind n) in
  let loop = { t; suspended; dispatch = ref ignore; views = ref 0 } in
  (* Validate both keys before measuring. *)
  let screen () = Term.screen t ~width:80 ~height:46 in
  let before = screen () in
  Term.feed t down;
  loop_resume loop;
  let moved = screen () in
  Term.feed t up;
  loop_resume loop;
  require (not (String.equal before moved)) "Down did not move the selection";
  require (String.equal before (screen ())) "Up did not move it back";
  loop

let down_up loop =
  Term.feed loop.t down;
  loop_resume loop;
  Term.feed loop.t up;
  loop_resume loop

let list_benches =
  List.map
    (fun (kind, n) ->
      Thumper.group
        (Printf.sprintf "%s-%d"
           (match kind with Select -> "select" | Table -> "table")
           n)
        [
          Thumper.bench_with_setup ~setup:(list_setup kind n)
            ~teardown:loop_close "down-up" down_up;
        ])
    [ (Select, 10_000); (Select, 100_000); (Table, 100_000) ]

(* Startup: a run of the 5,000-message transcript from its start to its
   first presented frame, then its end. *)
let startup output =
  let t = Term.create ~output ~width:initial_width ~height:initial_height () in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( {
              editor = Mosaic.Editor.of_string "a";
              scroll = Mosaic.Scroll.at 7500;
            },
            none ));
      update =
        (fun message model ->
          match message with
          | Edit editor -> ({ model with editor }, none)
          | Scroll scroll -> ({ model with scroll }, none));
      view = view ~mode:`Alt 5000;
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  let suspended = start_suspended t application in
  if !suspended = None then
    failwith "runtime benchmark: the run did not present a frame";
  loop_close { t; suspended; dispatch = ref ignore; views = ref 0 };
  Term.release t

let startup_benches =
  Thumper.group "startup"
    [
      Thumper.bench_with_setup ~setup:Term.output_file ~teardown:Unix.close
        "transcript-5000-first-frame" startup;
    ]

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "runtime"
    ~config:
      Thumper.Config.(
        default |> deadline 300.
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words; promoted_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
         ]))
    (* Per-input cost must not grow with mounted content: the 20,000-block
       transcript repeats the input workloads. The primary-mode group runs them
       in an inline region shorter than the terminal. *)
    ((loop_benches :: startup_benches :: list_benches)
    @ List.map
        (fun (mode, blocks, workloads) ->
          Thumper.group
            (match mode with
            | `Alt -> Printf.sprintf "transcript-%d" blocks
            | `Primary -> Printf.sprintf "primary-transcript-%d" blocks)
            (List.map
               (fun (name, workload) ->
                 Thumper.bench_with_setup
                   ~setup:(setup ~mode ~blocks workload)
                   ~teardown:close name (measure workload))
               workloads))
        [
          ( `Alt,
            5000,
            [
              ("type-and-erase", Keystroke);
              ("wheel-roundtrip-2x8", Wheel_burst);
              ("resize-roundtrip-249x69", Resize (249, 69));
              ("resize-roundtrip-250x69", Resize (250, 69));
            ] );
          ( `Alt,
            20000,
            [
              ("type-and-erase", Keystroke);
              ("wheel-roundtrip-2x8", Wheel_burst);
              ("resize-roundtrip-249x69", Resize (249, 69));
              ("resize-roundtrip-250x69", Resize (250, 69));
            ] );
          ( `Primary,
            5000,
            [
              ("type-and-erase", Keystroke); ("wheel-roundtrip-2x8", Wheel_burst);
            ] );
        ])
