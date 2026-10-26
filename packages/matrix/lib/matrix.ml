module Ansi = Ansi
module Grid = Matrix_grid
module Input = Matrix_input
module Screen = Matrix_screen
module Terminal = Matrix_terminal
module Terminfo = Matrix_terminfo
module Text = Matrix_text
module Tty = Tty

(* Types *)

type kitty_keyboard = [ `Auto | `Off ]
type mode = [ `Alt | `Primary ]

type event =
  | Key of Input.Keystroke.t
  | Key_release of Input.Keystroke.release
  | Text of string
  | Mouse of Input.Mouse_report.t
  | Paste_start
  | Paste_data of string
  | Paste_end of [ `Complete | `Cut ]
  | Focus_in
  | Focus_out
  | Resize of int * int
  | Color_scheme of [ `Dark | `Light ]
  | Invalid of string
  | Reply of Input.Reply.t

let pp_event ppf = function
  | Key k -> Format.fprintf ppf "Key %a" Input.Keystroke.pp k
  | Key_release r ->
      Format.fprintf ppf "Key_release %a" Input.Keystroke.pp_release r
  | Text s -> Format.fprintf ppf "Text %S" s
  | Mouse m -> Format.fprintf ppf "Mouse %a" Input.Mouse_report.pp m
  | Paste_start -> Format.pp_print_string ppf "Paste_start"
  | Paste_data s -> Format.fprintf ppf "Paste_data %S" s
  | Paste_end `Complete -> Format.pp_print_string ppf "Paste_end Complete"
  | Paste_end `Cut -> Format.pp_print_string ppf "Paste_end Cut"
  | Focus_in -> Format.pp_print_string ppf "Focus_in"
  | Focus_out -> Format.pp_print_string ppf "Focus_out"
  | Resize (cols, rows) -> Format.fprintf ppf "Resize %dx%d" cols rows
  | Color_scheme `Dark -> Format.pp_print_string ppf "Color_scheme Dark"
  | Color_scheme `Light -> Format.pp_print_string ppf "Color_scheme Light"
  | Invalid s -> Format.fprintf ppf "Invalid %S" s
  | Reply r -> Format.fprintf ppf "Reply %a" Input.Reply.pp r

module Signal = Tty.Signal

(* The device a run drives: a terminal's input and output, a clock, and a
   way to wait for them. A run decodes, negotiates and schedules; a device
   moves bytes and waits. [unix] is the device of every run; the runtime's
   own tests build others, whose clock and input they script. *)
module Backend = struct
  type signal = Winch | Cont | Tstp | Terminate of int

  type t = {
    now : unit -> int;
    wait : until:int option -> reading:bool -> bool;
    wake : unit -> unit;
    fds : reading:bool -> Unix.file_descr list;
    take_signals : unit -> signal list;
    read_input : bytes -> int -> int -> int;
    write_output : bytes -> int -> int -> unit;
    terminal_size : unit -> (int * int) option;
    window_pixels : unit -> (int * int) option;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    input_is_tty : bool;
    output_is_tty : bool;
    controlling : bool;
    on_fatal : string -> unit;
    cleanup : unit -> unit;
    env : Terminal.Caps.env;
  }

  let drain fd =
    let buf = Bytes.create 64 in
    let rec go () =
      match Unix.read fd buf 0 64 with
      | 64 -> go ()
      | _ -> ()
      | exception Unix.Unix_error _ -> ()
    in
    go ()

  let handled =
    [|
      (Sys.sigwinch, Winch);
      (Sys.sigcont, Cont);
      (Sys.sigtstp, Tstp);
      (Sys.sigint, Terminate Sys.sigint);
      (Sys.sigterm, Terminate Sys.sigterm);
      (Sys.sighup, Terminate Sys.sighup);
      (Sys.sigquit, Terminate Sys.sigquit);
    |]

  (* The signals the handlers recorded for holder [h], in the order of
     [handled], from its [i]th on. Every turn asks, so none received
     allocates nothing. *)
  let rec received_from h i =
    if i = Array.length handled then []
    else
      let s, signal = handled.(i) in
      if Signal.received h s then signal :: received_from h (i + 1)
      else received_from h (i + 1)

  let signals_from_handlers h () = received_from h 0

  let rec read_input fd buf off len =
    match Unix.read fd buf off len with
    | n -> n
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_input fd buf off len
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1

  (* The environment of the process, with the description the terminfo
     database has for its [TERM]. *)
  let process_env () =
    let terminfo =
      match Sys.getenv_opt "TERM" with
      | Some term -> Result.to_option (Tty.terminfo term)
      | None -> None
    in
    Terminal.Caps.env ?terminfo Sys.getenv_opt ~win32:Sys.win32

  (* A run on descriptors. Its wait is [select(2)] on the input and the
     holder's wake pipe, which [post] and the signal handlers write; a wait
     until a time already past looks with [Tty.readable] instead, which
     allocates nothing. On Windows the wake would have to be a socket, the
     one kind of descriptor [Unix.select] waits on without polling, and the
     console's handle could never be among the descriptors, since waiting
     on it through [Unix.select] reads away its records that are not keys;
     [Signal.hold] fails there, so no run starts. *)
  let unix ?(input = Unix.stdin) ?(output = Unix.stdout) ?env () =
    let input_is_tty = Tty.is_tty input in
    let output_is_tty = Tty.is_tty output in
    let controlling = Tty.is_controlling input || Tty.is_controlling output in
    let env = match env with Some env -> env | None -> process_env () in
    let holder =
      Signal.hold ~controlling
        ~output:(if output_is_tty then Some output else None)
    in
    let wake_r = Signal.wake_fd holder in
    let both = [ input; wake_r ] and wake_only = [ wake_r ] in
    let saved = ref None in
    let set_raw_mode raw =
      match (raw, !saved) with
      | true, None when input_is_tty ->
          Signal.keep_settings holder ~input;
          saved := Some (Tty.raw input)
      | false, Some termios ->
          saved := None;
          (* Restoring from the background would stop the process with
             SIGTTOU, unless the signal is blocked. *)
          let mask = Thread.sigmask SIG_BLOCK [ Sys.sigttou ] in
          Fun.protect
            ~finally:(fun () -> ignore (Thread.sigmask SIG_SETMASK mask))
            (fun () -> Tty.restore input termios);
          Signal.forget_settings holder
      | _ -> ()
    in
    let wait ~until ~reading =
      let now = match until with Some _ -> Tty.now () | None -> 0 in
      match until with
      | Some until when until <= now ->
          if Tty.readable wake_r then drain wake_r;
          reading && Tty.readable input
      | _ ->
          let timeout =
            match until with
            | None -> -1.
            | Some until -> Float.of_int (until - now) *. 1e-9
          in
          let readable, _, _ =
            try Unix.select (if reading then both else wake_only) [] [] timeout
            with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
          in
          if List.mem wake_r readable then drain wake_r;
          reading && List.mem input readable
    in
    {
      now = Tty.now;
      wait;
      wake = (fun () -> Signal.wake holder);
      fds = (fun ~reading -> if reading then both else wake_only);
      take_signals = signals_from_handlers holder;
      read_input = read_input input;
      write_output = Signal.write holder output;
      terminal_size = (fun () -> Tty.window output);
      window_pixels = (fun () -> Tty.window_pixels output);
      set_raw_mode;
      flush_input = (fun () -> if input_is_tty then Tty.flush_input input);
      input_is_tty;
      output_is_tty;
      controlling;
      on_fatal = Signal.set_reset holder;
      cleanup =
        (fun () ->
          Fun.protect
            (fun () -> set_raw_mode false)
            ~finally:(fun () -> Signal.release holder));
      env;
    }
end

(* A terminal emulated in the program's own process, for [Matrix_test]. A
   run on one keeps the device's time and is driven by its steps, so the
   device decides how the run's own waits end: as it takes and gives back
   the terminal, it waits for answers until a time, and a device whose
   terminal stays silent moves its clock there, as a terminal that never
   answers leaves the wait. *)
type device = {
  read_clock : unit -> int;
  wait : until:int option -> bool;
  read : bytes -> int -> int -> int;
  write : bytes -> int -> int -> unit;
  window : unit -> (int * int) option;
  resized : unit -> bool;
  set_raw : bool -> unit;
  flush : unit -> unit;
  tty : bool;
  env : Terminal.Caps.env;
  frame : unit -> unit;
  idle : until:int option -> stepped:bool -> bool;
  presented : (unit -> Grid.t * int * [ `Kitty | `Sixel | `Blocks ]) -> unit;
}

let backend_of_device d : Backend.t =
  {
    now = d.read_clock;
    wait = (fun ~until ~reading -> d.wait ~until && reading);
    wake = ignore;
    fds = (fun ~reading:_ -> []);
    take_signals = (fun () -> if d.resized () then [ Backend.Winch ] else []);
    read_input = d.read;
    write_output = d.write;
    terminal_size = d.window;
    window_pixels = (fun () -> None);
    set_raw_mode = d.set_raw;
    flush_input = d.flush;
    input_is_tty = d.tty;
    output_is_tty = d.tty;
    controlling = false;
    on_fatal = ignore;
    cleanup = ignore;
    env = d.env;
  }

(* Stop requests. A request wakes the runs the request was given to, by
   their serial, so that a run waiting for its descriptors steps and sees
   it; a run given one already requested sees it at its first step. *)
module Stop = struct
  type t = {
    requested : bool Atomic.t;
    lock : Mutex.t;
    mutable wakes : (int * (unit -> unit)) list;
  }

  let create () =
    { requested = Atomic.make false; lock = Mutex.create (); wakes = [] }

  let request t =
    if not (Atomic.exchange t.requested true) then
      Mutex.protect t.lock (fun () ->
          List.iter (fun (_, wake) -> wake ()) t.wakes)

  let attach t serial wake =
    Mutex.protect t.lock (fun () -> t.wakes <- (serial, wake) :: t.wakes)

  let detach t serial =
    Mutex.protect t.lock (fun () ->
        t.wakes <- List.filter (fun (s, _) -> s <> serial) t.wakes)
end

(* The tools [MATRIX_DEBUG] names: a panel over each frame, which the run
   times, and a dump of each frame drawn into a directory. [app] sums the
   time the application spent handling steps since the last frame: from a
   step's return ([returned], [-1] once the next call has begun) to the
   next call of the loop or to [draw], and from the end of [draw] to the
   next call. *)
type tools = {
  overlay : Debug_overlay.t option;
  frames : string option;
  mutable app : int;
  mutable returned : int;
}

let tools_of_string s =
  let tool tools = function
    | "" -> tools
    | "overlay" -> { tools with overlay = Some (Debug_overlay.create ()) }
    | "frames" -> { tools with frames = Some "frames" }
    | item when String.starts_with ~prefix:"frames=" item ->
        let dir = String.sub item 7 (String.length item - 7) in
        { tools with frames = Some dir }
    | item ->
        invalid_arg
          (Printf.sprintf "Matrix.run: MATRIX_DEBUG names no tool %S" item)
  in
  match
    List.fold_left tool
      { overlay = None; frames = None; app = 0; returned = -1 }
      (String.split_on_char ',' s)
  with
  | { overlay = None; frames = None; _ } -> None
  | tools -> Some tools

(* Configuration *)

type config = {
  mode : mode;
  raw_mode : bool;
  mouse : Terminal.mouse_mode;
  bracketed_paste : bool;
  kitty_keyboard : kitty_keyboard;
  key_releases : bool;
  fps : float;
  explicit_width : bool;
  min_tui_height : int;
  caps : Terminal.capabilities option;
  device : device option;
  (* [MATRIX_DEBUG], which [run] reads for a run on descriptors. *)
  debug : string option;
}

let config ?(mode = `Alt) ?(raw_mode = true) ?(mouse = `Sgr_any)
    ?(bracketed_paste = true) ?(kitty_keyboard = `Auto) ?(key_releases = false)
    ?(fps = 60.) ?(explicit_width = false) ?(min_tui_height = 1) ?caps () =
  if (not (Float.is_finite fps)) || fps <= 0. then
    invalid_arg "Matrix.config: fps must be positive and finite";
  {
    mode;
    raw_mode;
    mouse;
    bracketed_paste;
    kitty_keyboard;
    key_releases;
    fps;
    explicit_width;
    min_tui_height;
    caps;
    device = None;
    debug = None;
  }

let on_device d config = { config with device = Some d }
let default_config = config

(* Queries sent together, which the terminal answers in order. A batch of
   capability queries, of the runtime's other queries or of the
   application's ends with DA1; a cursor batch is one position report, its
   own answer, which says where the live region is. *)
type question =
  | Capabilities of Terminal.Caps.batch
  | Cursor of anchor
  | Own  (** Queries whose replies the runtime takes as they come. *)
  | Application

(* What a cursor report places: the live region at the cursor, when the run
   begins or takes the terminal back; or, after a resize, the live region
   whose row [live_row] the cursor was left on. *)
and anchor = At_cursor | Moved of int

type batch = {
  question : question;
  owed : Input.Decoder.owed;
  sent : int;
  mutable reports : int;  (** Cursor reports received. *)
  mutable answered : bool;
}

(* Deadlines, in a binary heap ordered by time, then by the order they were
   set, which a timer's number is. *)
type entry = { deadline : int; id : int; mutable index : int }

type heap = {
  mutable entries : entry array;
  mutable length : int;
  by_id : (int, entry) Hashtbl.t;
  mutable next_id : int;
}

type timer = int

type 'msg step =
  | Event of event
  | Timer of timer
  | Posted of 'msg
  | Frame
  | Stopped

(* The size the terminal reported and the runtime has yet to apply. *)
type size_report = Applied | Read | Reported of int * int

type 'msg app = {
  serial : int;
  terminal : Terminal.t;
  decoder : Input.Decoder.t;
  input_buffer : bytes;
  mutable input_ended : bool;
  (* When input last arrived, from which silence is measured. *)
  mutable last_input : int;
  (* The round trip of the startup probe, 0 until measured. *)
  mutable rtt : int;
  mutable evidence : Terminal.Caps.evidence;
  (* Batches still owed, oldest first. *)
  mutable batches : batch list;
  (* Cursor reports owed by batches given up, which a late report answers
     first. *)
  mutable lost_reports : int;
  (* Batches that end with DA1 and wait for the one in flight, oldest
     first. *)
  mutable waiting : (question * (string * Input.Decoder.owed)) list;
  (* Events decoded and not yet returned. *)
  events : event Queue.t;
  config : config;
  backend : Backend.t;
  stop : Stop.t option;
  owner : int;
  screen : Screen.t;
  (* One retained writer owns the complete terminal frame. *)
  submit_buf : Ansi.Writer.t;
  mutable width : int;
  mutable height : int;
  (* Primary mode layout *)
  mutable primary : Primary.t;
  (* Layout operations [primary] already counts, sent with the next frame. *)
  mutable layout_ops : Primary.terminal_op list;
  (* The live row the last frame left the cursor on, in [`Primary] mode:
     after a resize, where the cursor went shows where the live region went.
     [None] when unknown. *)
  mutable cursor_live_row : int option;
  (* The cursor report that says where the live region is, which frames wait
     for. *)
  mutable anchor : batch option;
  mutable scroll_hint : Screen.scroll_hint option;
  (* Cursor state tracking — visual caches are valid only while the cursor is
     visible. [None] means the terminal state is unknown (startup, hidden, or
     after the terminal was given to another program), so the next visible
     frame must emit. *)
  mutable last_cursor_position : (int * int) option;
  mutable last_cursor_visible : bool option;
  mutable last_cursor_shape : Ansi.cursor_shape option;
  mutable last_cursor_color : (int * int * int) option option;
  (* Time *)
  mutable now : int;
  heap : heap;
  (* Functions posted from any thread, newest first, and those taken by the
     current turn. *)
  mailbox : 'msg list Atomic.t;
  accepting : bool Atomic.t;
  posted : 'msg Queue.t;
  (* Posts made and not yet returned as steps, in the mailbox or in
     [posted]. Off the loop's thread a post waits on [room], with
     [room_lock], while [most_pending] are pending. *)
  pending : int Atomic.t;
  room_lock : Mutex.t;
  room : Condition.t;
  (* Whether a turn has begun since the run last had nothing to do, and
     whether each kind of work still owes the current turn the step every
     turn gives it. *)
  mutable begun : bool;
  (* The time of the latest step, against which the turn's budget and the
     frame interval are measured. *)
  mutable latest : int;
  (* Whether [next]'s wait has just ended, and whether the input was
     readable then: the turn after it need not look again. *)
  mutable waited : bool;
  mutable waited_ready : bool;
  mutable owe_event : bool;
  mutable owe_timer : bool;
  mutable owe_post : bool;
  (* Frames *)
  interval : int;
  mutable dirty : bool;
  (* A run on a device ([in_process]) draws at once a frame asked for outside
     a frame: by input, a timer, a post or the runtime itself. A frame asked
     for from a Frame step until the next step is an animation's, and waits
     its interval. *)
  in_process : bool;
  mutable asked_outside : bool;
  mutable in_frame_step : bool;
  on_frame : unit -> unit;
  tools : tools option;
  (* Whether [next] returned a step since it last waited. *)
  mutable stepped : bool;
  mutable drawing : bool;
  (* The frame being drawn was drawn across a release. *)
  mutable void : bool;
  mutable last_frame : int;
  (* The time between the latest frame and the one before, 0 until the
     second. *)
  mutable frame_dt : int;
  (* When the latest frame was drawn, which [last_frame] is not for a frame
     a device draws early. *)
  mutable drawn : int;
  mutable force_full_next_frame : bool;
  mutable size_report : size_report;
  (* The usable size last returned as [Resize]. *)
  mutable usable : int * int;
  (* The cell's size in pixels as the terminal answered CSI 16 t, the
     window's as the system reports it, and the cell size these and the text
     area's size give ([cell_size]). *)
  mutable cell_answer : (int * int) option;
  mutable window_pixels : (int * int) option;
  mutable cell_size : (int * int) option;
  (* A [Resize] was returned for the frame that follows it. *)
  mutable frame_follows : bool;
  mutable frame_modes_armed : bool;
  mutable restore_modes_on_next_focus : bool;
  mutable released : bool;
  (* The modes are set once the screen is taken: the kitty keyboard
     protocol's flags belong to the screen they are pushed on. *)
  mutable modes_taken : bool;
  (* Signals the run took, to restore when it ends. *)
  mutable taken : int list;
  (* Signals received and not yet acted on, oldest first. *)
  mutable kept : Backend.signal list;
  mutable tstp_ignored : bool;
  (* Undos of the steps that took the terminal, latest first. *)
  mutable undo : (exiting:bool -> unit) list;
}

(* The last frame's time before the first frame: long enough ago that the
   first is due at once. *)
let no_frame = min_int / 2

(* The runs, latest first, under [runs_lock]: runs may start on several
   domains. A run on the controlling terminal holds it; one started inside
   [release] holds it while the outer one waits. *)
type held = Held : 'msg app -> held

let runs : held list ref = ref []
let runs_lock = Mutex.create ()
let serials = Atomic.make 0
let thread_id () = Thread.id (Thread.self ())

let check t =
  if thread_id () <> t.owner then
    invalid_arg "Matrix: called off the thread that runs the application"

(* Heap *)

let heap_create () =
  { entries = [||]; length = 0; by_id = Hashtbl.create 8; next_id = 0 }

let before a b =
  a.deadline < b.deadline || (a.deadline = b.deadline && a.id < b.id)

let heap_set h i e =
  h.entries.(i) <- e;
  e.index <- i

let rec sift_up h i =
  if i > 0 then
    let parent = (i - 1) / 2 in
    let e = h.entries.(i) and p = h.entries.(parent) in
    if before e p then (
      heap_set h parent e;
      heap_set h i p;
      sift_up h parent)

let rec sift_down h i =
  let l = (2 * i) + 1 in
  if l < h.length then
    let r = l + 1 in
    let child =
      if r < h.length && before h.entries.(r) h.entries.(l) then r else l
    in
    let e = h.entries.(i) and c = h.entries.(child) in
    if before c e then (
      heap_set h i c;
      heap_set h child e;
      sift_down h child)

let heap_add h deadline =
  let id = h.next_id in
  h.next_id <- id + 1;
  let e = { deadline; id; index = h.length } in
  if h.length = Array.length h.entries then (
    let grown = Array.make (max 8 (2 * h.length)) e in
    Array.blit h.entries 0 grown 0 h.length;
    h.entries <- grown);
  heap_set h h.length e;
  h.length <- h.length + 1;
  Hashtbl.replace h.by_id id e;
  sift_up h e.index;
  id

let heap_remove h e =
  Hashtbl.remove h.by_id e.id;
  let last = h.entries.(h.length - 1) in
  h.length <- h.length - 1;
  if e.index < h.length then (
    heap_set h e.index last;
    sift_down h last.index;
    sift_up h last.index)

(* The earliest deadline, [max_int] when there is none. *)
let heap_min h = if h.length = 0 then max_int else h.entries.(0).deadline

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)
let ms n = n * 1_000_000

(* Layout *)

let sync_primary_layout t ~resize =
  let region = Primary.live_region t.primary in
  Screen.set_row_offset t.screen region.row_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:region.height

let force_full_redraw t = t.force_full_next_frame <- true

(* The terminal's size as its device has it. An output with no window, a
   pipe, a file or a socket, keeps the size its terminal last reported in
   band (mode 2048). *)
let device_size t =
  Option.value (t.backend.terminal_size ()) ~default:(t.width, t.height)

(* Accessors *)

let size t =
  check t;
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, (Primary.live_region t.primary).height)

let full_size t =
  check t;
  (t.width, t.height)

let mode t =
  check t;
  t.config.mode

let pixel_resolution t =
  check t;
  Terminal.pixel_resolution t.terminal

let cell_size t =
  check t;
  t.cell_size

(* The cell's size: the terminal's answer, else its text area's size, else
   its window's, divided by its columns and rows and floored, which drops any
   padding under a pixel a cell. A frame follows a change while pictures
   show, which are fit at the cell size. *)
let update_cell_size t =
  let w, h =
    match (t.cell_answer, Terminal.pixel_resolution t.terminal) with
    | Some (w, h), _ when w > 0 && h > 0 -> (w, h)
    | _, Some (pw, ph) when pw >= t.width && ph >= t.height ->
        (pw / t.width, ph / t.height)
    | _ -> (
        match t.window_pixels with
        | Some (pw, ph) when pw >= t.width && ph >= t.height ->
            (pw / t.width, ph / t.height)
        | _ -> (0, 0))
  in
  (* A size that did not change allocates nothing. *)
  match t.cell_size with
  | Some (cw, ch) when cw = w && ch = h -> ()
  | None when w = 0 || h = 0 -> ()
  | _ ->
      t.cell_size <- (if w = 0 || h = 0 then None else Some (w, h));
      if Grid.Unsafe.has_pictures (Screen.current_grid t.screen) then (
        t.dirty <- true;
        t.asked_outside <- true)

let terminal t =
  check t;
  t.terminal

let capabilities t =
  check t;
  Terminal.capabilities t.terminal

let now t =
  check t;
  t.now

let frame_dt t =
  check t;
  Float.of_int t.frame_dt *. 1e-9

(* A reply needs a terminal on the input side, in raw mode. *)
let can_query t =
  t.backend.input_is_tty && t.config.raw_mode && not t.input_ended

(* A frame the runtime needs, as after a resize, is asked for outside a
   frame; the application's requests are an animation's from a Frame step
   until the next step. *)
let owe_frame t =
  t.dirty <- true;
  t.asked_outside <- true

let ask_frame t =
  if not t.in_frame_step then t.asked_outside <- true;
  t.dirty <- true

let request_redraw t =
  check t;
  ask_frame t

let at t time =
  check t;
  heap_add t.heap time

let cancel t timer =
  check t;
  match Hashtbl.find_opt t.heap.by_id timer with
  | Some e -> heap_remove t.heap e
  | None -> ()

(* Back-pressure on posts. A post off the loop's thread reserves its place
   in [pending], and waits while [most_pending] are pending, so that a
   producer faster than the loop holds a bounded backlog. The loop wakes
   the posts waiting once it has taken [pending] down to [resume_pending],
   half the bound, so that a producer refills it in one go rather than one
   post per wake. A post reads [pending] and waits with [room_lock] held,
   and the loop broadcasts with it held, so a post that read the bound
   before the loop took it down is in [Condition.wait] when the broadcast
   comes. Every post that waits read the bound, and [pending] passes
   [resume_pending] on its way back down, unless the run ends first, which
   broadcasts too. A post on the loop's thread never waits, since the loop
   would wait for itself. Nor does a post to a run on a device: there the
   test harness decides when the program steps, and it holds the program
   still while the test runs and while [quiesce] waits for work that may be
   posting, so a post that waited for steps could wait forever. *)
let most_pending = 65_536
let resume_pending = most_pending / 2

let wait_for_room t =
  Mutex.lock t.room_lock;
  while Atomic.get t.accepting && Atomic.get t.pending >= most_pending do
    Condition.wait t.room t.room_lock
  done;
  Mutex.unlock t.room_lock

let wake_posts t =
  Mutex.lock t.room_lock;
  Condition.broadcast t.room;
  Mutex.unlock t.room_lock

let rec push t msg =
  let posts = Atomic.get t.mailbox in
  if not (Atomic.compare_and_set t.mailbox posts (msg :: posts)) then push t msg
  else if posts = [] then t.backend.wake ()

let rec post t msg =
  if Atomic.get t.accepting then
    let n = Atomic.get t.pending in
    if n >= most_pending && (not t.in_process) && thread_id () <> t.owner then (
      wait_for_room t;
      post t msg)
    else if Atomic.compare_and_set t.pending n (n + 1) then push t msg
    else post t msg

(* The terminal cuts what it receives into cells by the width method the
   runtime sets, and the screen lays out its cells by the same, so a cell is
   as wide on the screen as in the terminal. A change of method voids the
   cells the terminal shows, laid out by the old one. *)
let width_method t =
  if t.config.explicit_width then `Wcwidth
  else (Terminal.capabilities t.terminal).unicode_width

(* The runtime sets the terminal's grapheme mode only where the terminal
   answered it reset, and resets it when it gives the terminal back: a
   terminal is left with the mode as it was found. *)
let set_width_method t m =
  let caps = Terminal.capabilities t.terminal in
  Terminal.enable_grapheme_mode t.terminal
    (m <> `Wcwidth && caps.grapheme_mode = Some false);
  if Grid.width_method (Screen.current_grid t.screen) <> m then (
    Screen.set_width_method t.screen m;
    force_full_redraw t)

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  let color_depth = if caps.rgb then `Truecolor else `Ansi256 in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks ~color_depth ~graphics:caps.graphics
    ~cell_size:t.cell_size;
  set_width_method t (width_method t)

(* A batch's queries are sent, and what they are owed is added to the
   decoder's counts until the batch is answered or given up. *)
let send_batch t question (bytes, owed) =
  Terminal.send t.terminal bytes;
  Input.Decoder.expect t.decoder owed;
  let b = { question; owed; sent = t.now; reports = 0; answered = false } in
  t.batches <- t.batches @ [ b ];
  b

let ends_with_da1 = function
  | Capabilities Widths | Cursor _ -> false
  | Capabilities _ | Own | Application -> true

(* A batch that ends with DA1 is sent once no other is in flight. Every DA1
   reply is alike, so a reply is told to answer a batch by the order of the
   batches alone, and a batch whose DA1 never comes, such as a passthrough
   check that tmux drops, would otherwise take the next batch's. *)
let ask_batch t question bytes_owed =
  if List.exists (fun b -> ends_with_da1 b.question) t.batches then (
    if
      (* The runtime's own query, already waiting, would be answered the
       same. *)
      not (question = Own && List.mem (question, bytes_owed) t.waiting)
    then t.waiting <- t.waiting @ [ (question, bytes_owed) ])
  else ignore (send_batch t question bytes_owed : batch)

let da1 = Ansi.(to_string (query Device_attributes))

(* A query whose reply the runtime takes as it comes, such as the pixel size,
   is a batch closed by DA1 too, so that its reply is never taken for the
   application's. *)
let ask_own t q =
  if can_query t then
    ask_batch t Own
      ( Ansi.(to_string (query q)) ^ da1,
        { Input.Decoder.cursor_reports = 0; string_replies = 0 } )

(* The text area's size and the cell's, which change with the window and the
   font. *)
let cell_pixels_da1 = "\027[16t" ^ da1

let ask_pixels t =
  if can_query t then
    ask_batch t Own
      ( Ansi.(to_string (query Pixel_size)) ^ cell_pixels_da1,
        { Input.Decoder.cursor_reports = 0; string_replies = 0 } )

(* Input modes follow the configuration and the capabilities, and are set
   again whenever the capabilities change: setting a mode a terminal has is
   idempotent, so this is the difference between what the modes were and what
   they must be. Focus, color scheme and resize reports are set without asking:
   no reference says what a terminal does with a private mode it does not
   know, but xterm (charproc.c, dpmodes: "unknown private code"), kitty
   (screen.c, set_mode_from_const: "Unsupported screen mode"), foot (csi.c,
   decset_decrst: UNHANDLED) and tmux (input.c,
   input_csi_dispatch_sm_private: "unknown") log it and do nothing else. The
   kitty keyboard flags are pushed once the terminal has answered its query,
   then asked for again, so that the capabilities say which flags hold. *)

let kitty_flags t = if t.config.key_releases then 0b11111 else 0b00101

let apply_modes t =
  let terminal = t.terminal in
  let caps = Terminal.capabilities terminal in
  Terminal.set_mouse_mode terminal t.config.mouse;
  Terminal.enable_bracketed_paste terminal t.config.bracketed_paste;
  Terminal.enable_focus_reporting terminal true;
  Terminal.enable_color_scheme_reports terminal true;
  Terminal.enable_resize_reports terminal true;
  Terminal.enable_meta_sends_escape terminal
    (caps.meta_sends_escape = Some false);
  let kitty =
    t.config.kitty_keyboard = `Auto && Option.is_some caps.kitty_keyboard
  in
  if kitty && not (Terminal.kitty_keyboard_enabled terminal) then (
    Terminal.enable_kitty_keyboard ~flags:(kitty_flags t) terminal true;
    ask_own t Csi_u_support)
  else if not kitty then Terminal.enable_kitty_keyboard terminal false;
  Terminal.enable_modify_other_keys terminal (not kitty);
  set_width_method t (width_method t)

let update_capabilities t =
  let caps = Terminal.Caps.derive t.evidence in
  if caps <> Terminal.capabilities t.terminal then (
    Terminal.set_capabilities t.terminal caps;
    if not t.released then (
      if t.modes_taken then apply_modes t;
      refresh_capabilities t;
      force_full_redraw t;
      owe_frame t))

(* Input

   The decoder holds no clock: the runtime says when the terminal has been
   silent. Silence is measured from the last bytes read, and only a wait that
   read nothing ends it; how long a wait must be scales with the round trip
   of the startup probe, the time a terminal takes to write what it began. An
   ESC that may begin a sequence waits max(50 ms, 2 RTT), the 50 ms that
   libtermkey and neovim wait; a sequence begun, which a terminal writes
   whole, waits max(500 ms, 4 RTT).

   A batch of queries is owed until it is answered, by its DA1 or, for a
   cursor batch, its report, and while it is owed the decoder reads its
   replies as replies: a reply is never typed for arriving late. The runtime
   waits for an answer only max(500 ms, 4 RTT), then goes on without it, and
   the batch stays owed. A terminal that never answers would then keep
   [ESC P], [ESC \]] and [ESC _] from being Alt keys, and a modified F3 from
   being F3, for the whole session, so a batch is given up after max(5 s,
   4 RTT). No reference bounds it: it must exceed any real answer's delay,
   since a reply after it decodes as input, and it costs only those keys while
   it lasts. 5 s is ten times the wait for an answer, and 4 RTT keeps it above
   a slow link's round trip as the sequence wait does. The cursor reports a
   given-up batch was owed are counted: a report that comes later answers
   them first, so that it never answers a later batch, such as the one that
   anchors the live region. *)

let silence t =
  match Input.Decoder.pending t.decoder with
  | Nothing -> None
  | Ambiguous -> Some (max (ms 50) (2 * t.rtt))
  | Sequence -> Some (max (ms 500) (4 * t.rtt))

let answer_wait t = max (ms 500) (4 * t.rtt)
let owed_bound t = max (ms 5000) (4 * t.rtt)
let push t event = Queue.push event t.events

(* A batch leaves the decoder's counts once, answered or given up. *)
let close t b =
  if List.memq b t.batches then (
    Input.Decoder.settle t.decoder b.owed;
    t.batches <- List.filter (fun b' -> b' != b) t.batches;
    match t.waiting with
    | (question, bytes_owed) :: rest when ends_with_da1 b.question ->
        t.waiting <- rest;
        ignore (send_batch t question bytes_owed : batch)
    | _ -> ())

let give_up t b =
  t.lost_reports <-
    t.lost_reports + b.owed.Input.Decoder.cursor_reports - b.reports;
  close t b

let ask t batch =
  ask_batch t (Capabilities batch) (Terminal.Caps.ask t.evidence batch)

let ask_cursor t anchor =
  send_batch t (Cursor anchor)
    ( Ansi.(to_string (query Cursor_position)),
      { Input.Decoder.cursor_reports = 1; string_replies = 0 } )

(* Adopts a primary layout transition: its operations go out with the next
   frame, and a frame after operations or a move of the live region redraws
   it whole. *)
let relayout t (primary, ops) =
  if ops <> [] || Primary.live_region primary <> Primary.live_region t.primary
  then force_full_redraw t;
  t.primary <- primary;
  t.layout_ops <- t.layout_ops @ ops

(* The live region starts where the cursor report says. When the run begins
   or takes the terminal back, it starts at the cursor; after a resize, the
   cursor was left on a known live row. What the terminal did meanwhile voids
   the operations planned before. *)
let anchored t anchor ~row ~col =
  match anchor with
  | At_cursor ->
      t.layout_ops <- [];
      relayout t (Primary.anchor t.primary ~row:(row + 1) ~col:(col + 1))
  | Moved live_row ->
      t.layout_ops <- [];
      relayout t (Primary.anchor t.primary ~row:(row + 1 - live_row) ~col:1);
      sync_primary_layout t ~resize:true

(* A cursor report answers the oldest query still owed one, a given-up one
   first. *)
let cursor_report t ~row ~col ~page =
  if t.lost_reports > 0 then t.lost_reports <- t.lost_reports - 1
  else
    match
      List.find_opt
        (fun b -> b.reports < b.owed.Input.Decoder.cursor_reports)
        t.batches
    with
    | None -> ()
    | Some b -> (
        let index = b.reports in
        b.reports <- index + 1;
        match b.question with
        | Capabilities q ->
            t.evidence <- Terminal.Caps.cursor t.evidence q ~index ~row ~col;
            if
              b.reports = b.owed.cursor_reports
              && not (ends_with_da1 b.question)
            then (
              b.answered <- true;
              close t b)
        | Cursor anchor -> (
            b.answered <- true;
            close t b;
            (* An answer later than the wait for it places nothing: the
               live region stays where it was planned. *)
            match t.anchor with
            | Some a when a == b && t.now < b.sent + answer_wait t ->
                t.anchor <- None;
                if not t.released then anchored t anchor ~row ~col;
                owe_frame t
            | _ -> ())
        | Application -> push t (Reply (Cursor { row; col; page }))
        | Own -> ())

(* The batch that ends with DA1 in flight, which the replies that are not
   cursor reports answer. *)
let replying t = List.find_opt (fun b -> ends_with_da1 b.question) t.batches

(* A DA1 reply answers the batch in flight that asked for one. The terminal
   answers in order, so every query sent before it has been answered or never
   will be. *)
let attributes_report t attributes =
  match replying t with
  | None -> ()
  | Some b -> (
      let rec before = function
        | b' :: rest when b' != b -> b' :: before rest
        | _ -> []
      in
      List.iter (close t) (before t.batches);
      t.lost_reports <- 0;
      b.answered <- true;
      close t b;
      match b.question with
      | Capabilities q ->
          if q = Probe then t.rtt <- max 0 (t.now - b.sent);
          t.evidence <- Terminal.Caps.answered t.evidence q ~attributes;
          Option.iter (ask t) (Terminal.Caps.next t.evidence q)
      | Application -> push t (Reply (Primary_attributes attributes))
      | Own | Cursor _ -> ())

(* A reply is the runtime's to act on, and the application's too while its
   batch is the one being answered. The reports a terminal sends on its own
   that are application facts are events. *)
let reply t (r : Input.Reply.t) =
  (match r with
  | Cursor { row; col; page } -> cursor_report t ~row ~col ~page
  | Primary_attributes attributes -> attributes_report t attributes
  | Color_scheme scheme -> push t (Color_scheme scheme)
  | Resize { rows; cols; height; width } ->
      Terminal.set_pixel_resolution t.terminal (Some (width, height));
      update_cell_size t;
      if (cols, rows) <> (t.width, t.height) then (
        t.size_report <- Reported (cols, rows);
        owe_frame t)
  | r -> (
      (match r with
      | Text_area_pixels { height; width } ->
          Terminal.set_pixel_resolution t.terminal (Some (width, height));
          update_cell_size t
      | Cell_pixels { height; width } ->
          t.cell_answer <- Some (width, height);
          update_cell_size t
      | r -> t.evidence <- Terminal.Caps.add t.evidence r);
      match replying t with
      | Some { question = Application; _ } -> push t (Reply r)
      | _ -> ()));
  update_capabilities t

let decoded t : Input.Decoder.event -> unit = function
  | Reply r -> reply t r
  | Key k -> push t (Key k)
  | Key_release r -> push t (Key_release r)
  | Text s -> push t (Text s)
  | Mouse m -> push t (Mouse m)
  | Focus_in -> push t Focus_in
  | Focus_out -> push t Focus_out
  | Paste_start -> push t Paste_start
  | Paste_data s -> push t (Paste_data s)
  | Paste_end ending -> push t (Paste_end ending)
  | Invalid s -> push t (Invalid s)
  (* What was cut or over the budget is in error, and dropped. *)
  | Cut _ | Too_long _ -> ()

let rec expire_batches t = function
  | [] -> ()
  | b :: rest ->
      if t.now >= b.sent + owed_bound t then give_up t b;
      expire_batches t rest

let expire t = expire_batches t t.batches

(* The time the input next needs attention, [max_int] when it needs none:
   a silence that resolves what the decoder holds, or a batch given up. *)
let input_deadline t =
  let silence =
    match silence t with Some wait -> t.last_input + wait | None -> max_int
  in
  match t.batches with
  | [] -> silence
  | b :: _ -> min silence (b.sent + owed_bound t)

(* Waits in the backend, which says whether the input is readable. A wait
   until a time already past ([past]) looks without waiting. *)
let wait_only t ~until = t.backend.wait ~until ~reading:(not t.input_ended)
let past = Some min_int

(* Reads what the input has, once, if [ready]; a wait that read nothing is
   silence, which decides what the decoder holds once it has lasted. *)
let read_input t ~ready =
  let n =
    if ready && not t.input_ended then
      t.backend.read_input t.input_buffer 0 (Bytes.length t.input_buffer)
    else -1
  in
  (if n = 0 then (
     t.input_ended <- true;
     Input.Decoder.finish t.decoder (decoded t))
   else if n > 0 then (
     t.last_input <- t.now;
     Input.Decoder.feed t.decoder t.input_buffer ~off:0 ~len:n (decoded t);
     (* An emulated terminal has written all it sent once nothing follows. *)
     if t.in_process && not (wait_only t ~until:past) then
       Input.Decoder.idle t.decoder (decoded t))
   else
     match silence t with
     | Some wait when t.now >= t.last_input + wait ->
         Input.Decoder.idle t.decoder (decoded t)
     | _ -> ());
  expire t

(* The run's own waits, as it takes and gives back the terminal, are on its
   clock. *)
let wait t ~until =
  let ready = wait_only t ~until in
  t.now <- t.backend.now ();
  ready

(* Reads input until every batch in [batches] is answered, the wait for an
   answer since [from] is over, or the input ends. The wait grows if the
   round trip is measured meanwhile. Events decoded meanwhile are kept. *)
let await t ~from batches =
  let pending () = List.exists (fun b -> not b.answered) batches in
  while pending () && (not t.input_ended) && t.now < from + answer_wait t do
    let until = min (from + answer_wait t) (input_deadline t) in
    let ready = wait t ~until:(Some until) in
    read_input t ~ready
  done

let abort_paste t =
  check t;
  Input.Decoder.cut_paste t.decoder (decoded t)

let query t ~owed bytes =
  check t;
  if can_query t then ask_batch t Application (bytes ^ da1, owed)

let refresh_render_region t =
  match t.config.mode with
  | `Alt ->
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      relayout t (Primary.resize t.primary ~terminal_height:(max 1 t.height));
      sync_primary_layout t ~resize:true

(* Static output (primary mode only) *)

let sgr_reset = Ansi.(to_string reset)
let sync_output_on = Ansi.(to_string (enable Sync_output))
let sync_output_off = Ansi.(to_string (disable Sync_output))
let cursor_hide_seq = Ansi.(to_string (disable Cursor_visible))
let cursor_show_seq = Ansi.(to_string (enable Cursor_visible))
let erase_below_seq = Ansi.(to_string erase_below_cursor)
let clear_and_home_seq = Ansi.(to_string clear_and_home)
let clear_scrollback_seq = Ansi.(to_string (erase_display ~mode:`Scrollback))
let reset_scroll_region_seq = Ansi.(to_string reset_scrolling_region)

(* Helpers for buffered frame output. All frame content is accumulated into
   an ANSI writer and flushed as a single write in [draw]. *)

let buf_cursor_position buf ~row ~col =
  Ansi.cursor_position ~row:(max 1 row) ~col:(max 1 col) buf

(* Inside tmux, the kitty graphics commands among [s] are wrapped for its
   passthrough, as kitty's icat wraps them; the placeholders are text, which
   tmux draws. *)
let wrap_graphics t s =
  if (Terminal.capabilities t.terminal).multiplexer <> Some `Tmux then s
  else
    let b = Buffer.create (String.length s + 64) in
    let rec go i =
      match String.index_from_opt s i '\027' with
      | Some j
        when j + 2 < String.length s && s.[j + 1] = '_' && s.[j + 2] = 'G' ->
          Buffer.add_substring b s i (j - i);
          let rec stop k =
            match String.index_from_opt s k '\027' with
            | Some k when k + 1 < String.length s && s.[k + 1] = '\\' -> k + 2
            | Some k -> stop (k + 1)
            | None -> String.length s
          in
          let e = stop (j + 1) in
          Buffer.add_string b
            Ansi.(
              to_string (tmux_passthrough (literal (String.sub s j (e - j)))));
          go e
      | Some j ->
          Buffer.add_substring b s i (j + 1 - i);
          go (j + 1)
      | None -> Buffer.add_substring b s i (String.length s - i)
    in
    go 0;
    Buffer.contents b

let apply_primary_op buf = function
  | Primary.Move_cursor { row; col } -> buf_cursor_position buf ~row ~col
  | Primary.Reset_sgr -> Ansi.Writer.write_string buf sgr_reset
  | Primary.Write s -> Ansi.Writer.write_string buf s
  | Primary.Draw f -> Ansi.Writer.write_string buf (f ())
  | Primary.Clear_and_home -> Ansi.Writer.write_string buf clear_and_home_seq
  | Primary.Clear_scrollback ->
      Ansi.Writer.write_string buf clear_scrollback_seq

(* kitty keeps the images of the rows an erase of the scrollback takes away
   (their placeholders are virtual placements, which no erase deletes), so
   the run deletes them with it. *)
let apply_primary_ops t buf ops =
  if List.exists (function Primary.Clear_scrollback -> true | _ -> false) ops
  then (
    let w = Ansi.Writer.create 0 in
    Screen.delete_transcript t.screen w;
    if Ansi.Writer.len w > 0 then
      Ansi.Writer.write_string buf (wrap_graphics t (Ansi.Writer.contents w)));
  List.iter (apply_primary_op buf) ops

let effective_size t =
  check t;
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, (Primary.effective_region t.primary).height)

(* Static rows never wrap: a row wider than a terminal narrowed since the
   snapshot overwrites its last column. Rows with pictures are copied and
   written when they are flushed, by the screen, which then knows the rows
   they end on; others are written now. *)
let static_snapshot t ?rows grid =
  if Grid.width grid > t.width then
    invalid_arg "Matrix.static_write: grid is wider than the terminal";
  let rows = Option.value rows ~default:(Grid.height grid) in
  let rows_of text = "\027[?7l" ^ text ^ "\027[?7h" in
  if Grid.Unsafe.has_pictures grid then (
    if rows < 0 || rows > Grid.height grid then
      invalid_arg "Matrix_grid.to_ansi: rows must be within grid height";
    let grid = Grid.copy grid in
    ( rows,
      fun ~top ->
        rows_of (wrap_graphics t (Screen.static_rows t.screen grid ~rows ~top))
    ))
  else
    let text = rows_of (Grid.to_ansi ~complete_rows:true ~rows grid) in
    (rows, fun ~top:_ -> text)

let static_write ?(preserve_live_region = false) ?rows t grid =
  check t;
  if t.config.mode <> `Alt then
    let rows, draw = static_snapshot t ?rows grid in
    if rows > 0 then (
      t.primary <-
        Primary.enqueue_static t.primary { draw; rows; preserve_live_region };
      ask_frame t)

let static_replace ?(preserve_live_region = false) ?rows t grid =
  check t;
  if t.config.mode <> `Alt then (
    let rows, draw = static_snapshot t ?rows grid in
    t.primary <-
      Primary.replace_static t.primary { draw; rows; preserve_live_region };
    ask_frame t)

(* Immediate action, not part of the frame pipeline: the plan's terminal ops
   are encoded and sent directly, not queued into the frame buffer, so the
   primary screen clears before the next frame renders. *)
let static_clear t =
  check t;
  if t.config.mode = `Alt then ()
  else
    let primary, ops = Primary.clear_static t.primary in
    t.primary <- primary;
    (* The erase supersedes the layout operations not yet sent, and the
       place a cursor report owed would give the live region. *)
    t.layout_ops <- [];
    t.anchor <- None;
    let buf = Ansi.Writer.create 16 in
    apply_primary_ops t buf ops;
    Terminal.send t.terminal (Ansi.Writer.contents buf);
    t.last_cursor_position <- Some (1, 1);
    t.cursor_live_row <- Some 0;
    force_full_redraw t;
    let cols, rows = device_size t in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    refresh_render_region t;
    ask_frame t

let set_scroll_hint t hint =
  check t;
  t.scroll_hint <- Some hint

(* Frames *)

(* No eager buffer clear here: Screen defers the post-present clear of the
   next grid and hit grid to their first observation ([Screen.next_grid],
   [Screen.build], ...), so both direct-draw apps given the grid and
   renderer hosts building through [Screen.build] pay for exactly one clear
   per frame instead of clearing here and again downstream. *)
let prepare ?primary_required_rows t =
  refresh_capabilities t;
  if t.config.mode = `Primary then begin
    relayout t (Primary.flush_static t.primary);
    Option.iter
      (fun rows -> relayout t (Primary.require_rows t.primary ~rows))
      primary_required_rows
  end;
  refresh_render_region t

let current_grid t =
  check t;
  Screen.current_grid t.screen

let hits t =
  check t;
  Screen.next_hit_grid t.screen

let screen t =
  check t;
  t.screen

(* Apply cursor position, style, and color into the frame buffer.
   Style and color are only emitted when they differ from the last frame. *)
let cursor_shape (cursor : Screen.cursor) : Ansi.cursor_shape =
  Ansi.cursor_shape_of_style ~style:cursor.style ~blinking:cursor.blinking

(* In [`Primary] mode the cursor always ends a frame at a known place, so
   that after a resize, where the terminal took it shows where the live
   region went. Hidden, it waits at the top of the live region: a terminal
   that shrinks drops the rows below its cursor first, which there are only
   live rows. *)
let cursor_output_position t ~(cursor : Screen.cursor) ~cursor_max_row =
  match (cursor.position, t.config.mode) with
  | Some (x, y), `Primary ->
      Some (Primary.terminal_cursor_row t.primary ~live_row:y, max 1 (x + 1))
  | Some (x, y), `Alt -> Some (clamp 0 (cursor_max_row - 1) y + 1, max 1 (x + 1))
  | None, `Primary ->
      let live_row = if cursor.visible then cursor_max_row - 1 else 0 in
      Some (Primary.terminal_cursor_row t.primary ~live_row, 1)
  | None, `Alt -> if cursor.visible then Some (cursor_max_row, 1) else None

(* Allocation-free "changed since last frame" checks: comparing through the
   stored options directly avoids wrapping the current value in [Some]. *)
let[@inline] visible_dirty t visible =
  match t.last_cursor_visible with Some v -> v <> visible | None -> true

let[@inline] shape_dirty t shape =
  match t.last_cursor_shape with Some s -> s <> shape | None -> true

let[@inline] color_dirty t color =
  match t.last_cursor_color with Some c -> c <> color | None -> true

let cursor_dirty t ~(cursor : Screen.cursor) ~position =
  position <> t.last_cursor_position
  || visible_dirty t cursor.visible
  || cursor.visible
     && (shape_dirty t (cursor_shape cursor) || color_dirty t cursor.color)

let apply_cursor_state t ~buf ~(cursor : Screen.cursor) ~position =
  (match position with
  | Some (row, col) ->
      buf_cursor_position buf ~row ~col;
      if cursor.visible then Ansi.Writer.write_string buf cursor_show_seq
  | None -> ());
  if cursor.visible then begin
    let shape = cursor_shape cursor in
    if shape_dirty t shape then begin
      Terminal.write_cursor_style t.terminal ~shape buf;
      t.last_cursor_shape <- Some shape
    end;
    if color_dirty t cursor.color then begin
      Terminal.write_cursor_color t.terminal cursor.color buf;
      t.last_cursor_color <- Some cursor.color
    end
  end
  else begin
    t.last_cursor_shape <- None;
    t.last_cursor_color <- None
  end;
  t.last_cursor_position <- position;
  t.last_cursor_visible <- Some cursor.visible

(* All frame output is accumulated into a single buffer and flushed as one
   write. Skips the write entirely when nothing changed. *)
let submit t =
  let cursor = Screen.cursor t.screen in
  let caps = Terminal.capabilities t.terminal in
  let use_sync = Terminal.tty t.terminal && caps.sync in
  let buf = t.submit_buf in
  Ansi.Writer.reset_pos buf;
  (* The pixels of the pictures the frame shows go before the synchronized
     update: they show nothing yet, and a large image would hold the update
     past the terminal's timeout. *)
  Screen.send_pictures t.screen buf;
  let pictures = Ansi.Writer.len buf in
  if pictures > 0 && caps.multiplexer = Some `Tmux then (
    let sent = wrap_graphics t (Ansi.Writer.contents buf) in
    Ansi.Writer.reset_pos buf;
    Ansi.Writer.write_string buf sent);
  let pictures = Ansi.Writer.len buf in

  (* Preamble: BSU + cursor hide. *)
  if use_sync then Ansi.Writer.write_string buf sync_output_on;
  Ansi.Writer.write_string buf cursor_hide_seq;
  let preamble_len = Ansi.Writer.len buf in

  (* No cursor homing here: the diff renderer emits an absolute
     cursor_position for every run it touches, so anything written between
     the preamble capture and the skip check would defeat the
     unchanged-frame write skip. *)
  apply_primary_ops t buf t.layout_ops;
  t.layout_ops <- [];
  let forced_full =
    let ff = t.force_full_next_frame in
    if ff then t.force_full_next_frame <- false;
    ff
  in
  let scroll_hint =
    let h = t.scroll_hint in
    t.scroll_hint <- None;
    match t.config.mode with `Alt -> h | `Primary -> None
  in
  (* In [`Primary] mode the frame paints the live region. A full redraw
     paints it down to its last row that shows anything, and erases the
     rest. *)
  let viewport, erase_from =
    match t.config.mode with
    | `Alt -> (None, None)
    | `Primary ->
        let y = Primary.render_offset t.primary in
        let live = (Primary.live_region t.primary).height in
        if forced_full then
          let height = min live (max 1 (Screen.active_height t.screen)) in
          ( Some { Screen.y; height },
            if height < live then Some (y + height + 1) else None )
        else (Some { Screen.y; height = live }, None)
  in
  Screen.render_into ~full:forced_full ?scroll_hint ?viewport t.screen buf;
  Option.iter
    (fun row ->
      buf_cursor_position buf ~row ~col:1;
      Ansi.Writer.write_string buf erase_below_seq)
    erase_from;

  (* Skip the write when nothing beyond the preamble was produced and cursor
     state is unchanged. The cursor lives in the live region, whichever of
     its rows this frame painted. *)
  let cursor_max_row =
    match t.config.mode with
    | `Primary -> (Primary.live_region t.primary).height
    | `Alt -> t.height
  in
  let position = cursor_output_position t ~cursor ~cursor_max_row in
  if
    Ansi.Writer.len buf > preamble_len
    || pictures > 0
    || cursor_dirty t ~cursor ~position
  then begin
    if t.config.mode = `Alt then Ansi.cursor_position ~row:t.height ~col:1 buf;
    apply_cursor_state t ~buf ~cursor ~position;
    (match (t.config.mode, position) with
    | `Primary, Some (row, _) ->
        t.cursor_live_row <- Some (row - 1 - Primary.render_offset t.primary)
    | _ -> ());
    if use_sync then Ansi.Writer.write_string buf sync_output_off;

    t.frame_modes_armed <- true;
    t.backend.write_output (Ansi.Writer.buffer buf) 0 (Ansi.Writer.len buf)
  end

(* A frame drawn across a release is not presented: what the screen holds
   is unknown, and the next frame is drawn whole. *)
let present t =
  t.drawing <- false;
  if t.void then (
    t.void <- false;
    t.dirty <- true)
  else submit t

(* The tools draw after the application, the dump first so that it holds
   the application's frame alone. The application may have built the next
   grid anew ([Screen.build]), so the panel draws on the one it left. *)
let draw_with_tools t tools f =
  let start = Tty.now () in
  if tools.returned >= 0 then tools.app <- tools.app + (start - tools.returned);
  f (Screen.next_grid t.screen);
  Option.iter
    (fun dir -> Frame_dump.snapshot ~dir ~hits:true t.screen)
    tools.frames;
  Option.iter
    (fun o ->
      Debug_overlay.draw o
        ~metrics:(Screen.last_metrics t.screen)
        ~gc:(Gc.quick_stat ())
        (Screen.next_grid t.screen))
    tools.overlay;
  present t;
  let stop = Tty.now () in
  Option.iter
    (fun o -> Debug_overlay.record o ~app:tools.app ~start ~ns:(stop - start))
    tools.overlay;
  tools.app <- 0;
  tools.returned <- stop

let draw t ?primary_required_rows f =
  check t;
  if not t.drawing then invalid_arg "Matrix.draw: no frame is being drawn";
  prepare ?primary_required_rows t;
  match t.tools with
  | None ->
      f (Screen.next_grid t.screen);
      present t
  | Some tools -> draw_with_tools t tools f

(* Cursor *)

(* Cursor setters update Screen state only. The actual terminal output
   is deferred to [draw], which writes everything into the frame buffer
   as a single write. *)

let set_cursor ?visible ?style t =
  check t;
  let cursor = Screen.cursor t.screen in
  let visible = Option.value visible ~default:cursor.visible in
  let style = Option.value style ~default:cursor.style in
  Screen.set_cursor t.screen { cursor with visible; style }

let set_cursor_style t ~style ~blinking =
  check t;
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with style; blinking }

let set_cursor_position t ~row ~col =
  check t;
  let max_row =
    if t.config.mode = `Primary then (Primary.live_region t.primary).height
    else max 1 t.height
  in
  let row = clamp 1 max_row row in
  let col = max 1 col in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with position = Some (col - 1, row - 1) }

let set_cursor_color t ~r ~g ~b =
  check t;
  let clamp_01 f = Float.max 0. (Float.min 1. f) in
  let to_byte f = int_of_float (Float.round (f *. 255.)) |> clamp 0 255 in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen
    {
      cursor with
      color =
        Some (to_byte (clamp_01 r), to_byte (clamp_01 g), to_byte (clamp_01 b));
    }

(* Resize *)

(* The terminal's size, applied when a frame is next drawn: a storm of
   reports costs one layout. A terminal moves its rows as it resizes, by
   rules of its own, and the cursor moves with its row, so in [`Primary]
   mode frames wait for a cursor report that says where the live region
   went: from the live row the last frame left the cursor on, or, before any
   frame since the region was placed, from the cursor, which is still where
   the region starts. The layout operations planned for the old size were
   not sent, and the report voids them. *)
let apply_size t =
  let cols, rows =
    match t.size_report with
    | Applied -> (t.width, t.height)
    | Read -> device_size t
    | Reported (cols, rows) -> (cols, rows)
  in
  t.size_report <- Applied;
  if cols > 0 && rows > 0 && (cols, rows) <> (t.width, t.height) then (
    t.width <- cols;
    t.height <- rows;
    t.window_pixels <- t.backend.window_pixels ();
    update_cell_size t;
    ask_pixels t;
    force_full_redraw t;
    refresh_render_region t;
    if t.config.mode = `Primary && can_query t then
      let anchor =
        match t.cursor_live_row with
        | Some live_row -> Moved live_row
        | None -> At_cursor
      in
      t.anchor <- Some (ask_cursor t anchor))

(* Event delivery *)

let adjust_event_for_offset t event =
  match (t.config.mode, event) with
  | `Primary, Mouse mouse ->
      Mouse { mouse with y = Primary.map_mouse_y t.primary mouse.y }
  | _ -> event

(* A Focus_in after a Focus_out sets the modes again, once: a multiplexer
   that was detached meanwhile may have lost them. *)
let deliver t event =
  (match event with
  | Focus_in ->
      if t.restore_modes_on_next_focus then (
        t.restore_modes_on_next_focus <- false;
        Terminal.restore_modes t.terminal)
  | Focus_out -> t.restore_modes_on_next_focus <- true
  | _ -> ());
  Event (adjust_event_for_offset t event)

(* Taking the terminal and giving it back

   Each step that takes something pushes the undo that gives it back; giving
   the terminal back runs the undos, latest first, each even if another
   raises. [exiting] tells the end of the run from a release, which hands the
   terminal to another program and takes it again. *)

let push_undo t undo = t.undo <- undo :: t.undo

let run_undos t ~exiting =
  let undos = t.undo in
  t.undo <- [];
  let first = ref None in
  List.iter
    (fun undo ->
      try undo ~exiting
      with exn ->
        if !first = None then first := Some (exn, Printexc.get_raw_backtrace ()))
    undos;
  Option.iter (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt) !first

(* Signals, which the runtime takes as Tty.Signal's policy says, for
   a run on a terminal. *)

let stopping_signals = [ Sys.sigtstp; Sys.sigterm; Sys.sighup ]
let interrupting_signals = [ Sys.sigint; Sys.sigquit ]
let take_signal t s = if Signal.take s then t.taken <- s :: t.taken

let take_signals t =
  t.tstp_ignored <- Signal.ignored Sys.sigtstp;
  if
    (not t.in_process)
    && (t.backend.input_is_tty || t.backend.output_is_tty
      || t.backend.controlling)
  then
    List.iter (take_signal t)
      (Signal.signals ~controlling:t.backend.controlling);
  push_undo t (fun ~exiting ->
      if exiting then (
        List.iter Signal.give_back t.taken;
        t.taken <- []))

(* Raw mode. Leaving it waits for the output to be sent and keeps input not
   yet read for whoever reads next; at the end of the run that is the shell,
   which must not read the runtime's mouse reports. *)
let take_raw_mode t =
  if t.backend.input_is_tty && t.config.raw_mode then (
    t.backend.set_raw_mode true;
    push_undo t (fun ~exiting ->
        (if exiting then try t.backend.flush_input () with _ -> ());
        t.backend.set_raw_mode false;
        if exiting then t.backend.flush_input ()))

(* Frame writes bypass the terminal handle and may stop between mode changes.
   Attempt every reset independently, before handing the terminal back. *)
let restore_frame_modes t =
  if t.frame_modes_armed then (
    t.frame_modes_armed <- false;
    (try Terminal.send t.terminal sync_output_off with _ -> ());
    (try Terminal.send t.terminal reset_scroll_region_seq with _ -> ());
    try Terminal.send t.terminal "\027[?7h" with _ -> ())

(* The screen a program run in the application's place finds. In [`Primary]
   mode that is the transcript, the static rows queued for it included, and
   below it the live region, erased, with the cursor at its start: the
   program's output goes on from the transcript. In [`Alt] mode it is the
   primary screen as the shell left it. The cursor is shown. *)
let give_screen t =
  (* The live frame's images are deleted, which the terminal would keep; the
     next frame is drawn whole. *)
  if Grid.Unsafe.has_pictures (Screen.current_grid t.screen) then (
    Grid.clear (Screen.next_grid t.screen);
    Screen.invalidate_presented t.screen;
    let w = Ansi.Writer.create 0 in
    Screen.send_pictures t.screen w;
    if Ansi.Writer.len w > 0 then
      Terminal.send t.terminal (wrap_graphics t (Ansi.Writer.contents w)));
  match t.config.mode with
  | `Alt -> Terminal.leave_alternate_screen t.terminal
  | `Primary ->
      relayout t (Primary.flush_static t.primary);
      let w = Ansi.Writer.create 64 in
      apply_primary_ops t w t.layout_ops;
      t.layout_ops <- [];
      buf_cursor_position w ~row:(Primary.render_offset t.primary + 1) ~col:1;
      Ansi.Writer.write_string w sgr_reset;
      Ansi.Writer.write_string w erase_below_seq;
      Ansi.Writer.write_string w cursor_show_seq;
      Terminal.send t.terminal (Ansi.Writer.contents w)

(* What the screen held is unknown once another program had it: the next
   frame is drawn whole and sets the cursor again. *)
let forget_screen t =
  force_full_redraw t;
  t.last_cursor_position <- None;
  t.cursor_live_row <- None;
  t.last_cursor_visible <- None;
  t.last_cursor_shape <- None;
  t.last_cursor_color <- None;
  Grid.clear (Screen.next_grid t.screen);
  Screen.Hit_grid.clear (Screen.next_hit_grid t.screen)

(* A step that writes may fail halfway: its undo is pushed first, and undoes
   whatever part of it was done. *)
let take_screen t =
  push_undo t (fun ~exiting:_ -> give_screen t);
  match t.config.mode with
  | `Alt ->
      Terminal.enter_alternate_screen t.terminal;
      let cursor = Screen.cursor t.screen in
      Screen.set_cursor t.screen { cursor with visible = false }
  | `Primary -> Terminal.leave_alternate_screen t.terminal

let reset_modes t ~exiting =
  restore_frame_modes t;
  if exiting then Terminal.close t.terminal
  else
    List.iter
      (fun reset -> try reset () with _ -> ())
      [
        (fun () -> Terminal.set_mouse_mode t.terminal `Off);
        (fun () -> Terminal.enable_bracketed_paste t.terminal false);
        (fun () -> Terminal.enable_focus_reporting t.terminal false);
        (fun () -> Terminal.enable_kitty_keyboard t.terminal false);
        (fun () -> Terminal.enable_modify_other_keys t.terminal false);
        (fun () -> Terminal.enable_color_scheme_reports t.terminal false);
        (fun () -> Terminal.enable_resize_reports t.terminal false);
        (fun () -> Terminal.enable_meta_sends_escape t.terminal false);
        (fun () -> Terminal.enable_grapheme_mode t.terminal false);
        (fun () -> Terminal.send t.terminal cursor_show_seq);
      ]

let take_modes t =
  push_undo t (fun ~exiting -> reset_modes t ~exiting);
  t.modes_taken <- true;
  refresh_capabilities t;
  apply_modes t;
  refresh_render_region t;
  force_full_redraw t

(* The bytes that reset what a run may set, which the SIGABRT handler writes
   itself. *)
let abort_bytes mode =
  String.concat ""
    [
      sync_output_off;
      reset_scroll_region_seq;
      "\027[?7h";
      "\027[?1000l\027[?1002l\027[?1003l\027[?1006l";
      "\027[?2004l\027[?1004l\027[?2031l\027[?2048l\027[?1036r";
      "\027[<u\027[>4;0m";
      sgr_reset;
      cursor_show_seq;
      (match mode with `Alt -> "\027[?1049l" | `Primary -> "\r\n");
    ]

(* What restores the terminal if the process ends where no OCaml code runs:
   the backend writes these bytes itself, and they go once the terminal is
   given back. *)
let take_fatal_reset t =
  t.backend.on_fatal (abort_bytes t.config.mode);
  push_undo t (fun ~exiting:_ -> t.backend.on_fatal "")

(* Waits, when a reply is owed, for the terminal to answer what was sent:
   the replies that come after the terminal is given away would reach the
   shell, or the program run in the application's place, as typing. Every
   event decoded and not yet returned, meanwhile or before, is dropped, but
   for replies, and a paste in progress is cut ([release] says why). Batches
   still owed are dropped: their answers went to whoever has the terminal. *)
let settle t =
  (* Each batch is waited for until as long after it was sent as any answer
     is: the probe's, already waited for as the run began, is not waited for
     again. *)
  if t.batches <> [] && can_query t then
    await t
      ~from:(List.fold_left (fun from b -> max from b.sent) min_int t.batches)
      t.batches;
  let kept = Queue.create () in
  Queue.iter (function Reply _ as e -> Queue.push e kept | _ -> ()) t.events;
  Queue.clear t.events;
  Queue.transfer kept t.events;
  List.iter (fun b -> Input.Decoder.settle t.decoder b.owed) t.batches;
  t.batches <- [];
  t.lost_reports <- 0;
  t.anchor <- None;
  (* A paste the application began to receive ends. *)
  Input.Decoder.cut_paste t.decoder (decoded t);
  Input.Decoder.reset t.decoder

(* The screen is given back where the terminal has it: its size is read,
   which the loop may not have seen change, and in [`Primary] mode the cursor
   report that says where the live region went is waited for with the
   replies owed. *)
let give_back t ~exiting =
  (try
     t.size_report <- Read;
     apply_size t;
     settle t
   with _ -> ());
  run_undos t ~exiting

(* The batch that waited for the ones dropped is sent. *)
let send_waiting t =
  match t.waiting with
  | (question, bytes_owed) :: rest when t.batches = [] ->
      t.waiting <- rest;
      ignore (send_batch t question bytes_owed : batch)
  | _ -> ()

(* Takes the terminal again after another program had it: its settings are
   the shell's now, input it left is discarded, the size is read, and in
   [`Primary] mode frames wait for a cursor report that says where the
   program's output ended. *)
let take_again t =
  take_fatal_reset t;
  take_raw_mode t;
  t.backend.flush_input ();
  let cols, rows = device_size t in
  t.width <- max 1 cols;
  t.height <- max 1 rows;
  t.size_report <- Applied;
  forget_screen t;
  take_screen t;
  take_modes t;
  if t.config.mode = `Primary && can_query t then
    t.anchor <- Some (ask_cursor t At_cursor);
  send_waiting t;
  owe_frame t

let release t f =
  check t;
  if t.released then invalid_arg "Matrix.release: the terminal is released";
  if t.drawing then t.void <- true;
  t.now <- t.backend.now ();
  give_back t ~exiting:false;
  t.released <- true;
  (* POSIX system(): the child gets the terminal's suspend and termination
     signals; the interrupt and quit characters reach the parent too, which
     ignores them. *)
  let stopping = List.filter (fun s -> List.mem s t.taken) stopping_signals in
  List.iter Signal.give_back stopping;
  t.taken <- List.filter (fun s -> not (List.mem s stopping)) t.taken;
  let take_back () =
    List.iter (take_signal t) stopping;
    t.kept <-
      t.kept
      @ List.filter
          (function
            | Backend.Terminate s -> not (List.mem s interrupting_signals)
            | _ -> true)
          (t.backend.take_signals ());
    t.released <- false;
    t.now <- t.backend.now ();
    take_again t
  in
  match f () with
  | result ->
      take_back ();
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try take_back () with _ -> ());
      Printexc.raise_with_backtrace exn bt

let stop_job t =
  check t;
  if t.backend.controlling && not t.tstp_ignored then
    release t Signal.stop_group

(* The loop *)

let has_due_timer t = t.heap.length > 0 && heap_min t.heap <= t.now

let take_timer t =
  let e = t.heap.entries.(0) in
  heap_remove t.heap e;
  Timer e.id

(* Frames wait for the cursor report that says where the live region is,
   at most as long as for any answer; then the layout stays as planned. *)
let render_blocked t =
  match t.anchor with
  | Some b when (not b.answered) && t.now < b.sent + answer_wait t -> true
  | Some _ ->
      t.anchor <- None;
      false
  | None -> false

(* A frame is due once its interval has passed by the latest step's time:
   work that took the turn's budget may have taken the interval with it. *)
let frame_due t =
  t.dirty && (not t.released)
  && (t.latest >= t.last_frame + t.interval || (t.in_process && t.asked_outside))
  && not (render_blocked t)

(* The time the run next needs a step if nothing it waits for comes first:
   now when work is left, otherwise the earliest deadline, if any. *)
let wait_deadline t =
  if
    (not (Queue.is_empty t.events))
    || (not (Queue.is_empty t.posted))
    || has_due_timer t
    || Atomic.get t.mailbox != []
  then Some t.now
  else
    let deadline = min (heap_min t.heap) (input_deadline t) in
    let deadline =
      if t.dirty then
        let frame = t.last_frame + t.interval in
        match t.anchor with
        | Some b when not b.answered ->
            min deadline (max frame (b.sent + answer_wait t))
        | _ -> min deadline frame
      else deadline
    in
    if deadline = max_int then None else Some deadline

(* Whether a run is ending the process on a termination signal. *)
let ending = Atomic.make false

(* The signals received since the last call, kept ones first. *)
let received t =
  let s = t.kept @ t.backend.take_signals () in
  t.kept <- [];
  s

let rec end_run t s =
  if Atomic.compare_and_set ending false true then (
    t.now <- t.backend.now ();
    (try give_back t ~exiting:true with _ -> ());
    List.iter Signal.give_back t.taken;
    t.taken <- [];
    (* The other runs' terminals, as an abort restores them. *)
    Signal.restore_all ();
    Signal.raise_default s;
    (* The default action ends the process; nothing returns here. *)
    exit 2)
  else
    (* Another run ends the process. *)
    let m = Mutex.create () and never = Condition.create () in
    Mutex.lock m;
    while true do
      Condition.wait never m
    done

(* A SIGCONT the run did not cause: whoever continued the process has put
   back the shell's settings. *)
and continued t =
  if not t.released then (
    if t.backend.input_is_tty && t.config.raw_mode then (
      t.backend.set_raw_mode false;
      t.backend.set_raw_mode true);
    Terminal.restore_modes t.terminal;
    forget_screen t;
    t.size_report <- Read;
    owe_frame t)

and on_signals t signals =
  List.iter
    (function
      | Backend.Winch ->
          t.size_report <- Read;
          owe_frame t
      | Cont -> continued t
      | Tstp ->
          if not t.released then (
            release t (fun () -> Signal.raise_default Sys.sigtstp);
            (* The SIGCONT that ended the stop is this one's. *)
            on_signals t
              (List.filter
                 (function Backend.Cont -> false | _ -> true)
                 (received t)))
      | Terminate s -> end_run t s)
    signals

let take_posts t =
  match Atomic.exchange t.mailbox [] with
  | [] -> ()
  | posts -> List.iter (fun f -> Queue.push f t.posted) (List.rev posts)

(* The next post taken, returned as a step. *)
let take_post t =
  let msg = Queue.pop t.posted in
  if Atomic.fetch_and_add t.pending (-1) = resume_pending + 1 then wake_posts t;
  Posted msg

(* A turn begins at [now]. The input is looked at without waiting, unless
   [next]'s wait has just said whether it is readable; then the signals, the
   input and the posts received are taken. *)
let begin_turn t ~now =
  let ready =
    if t.waited then (
      t.waited <- false;
      t.waited_ready)
    else wait_only t ~until:past
  in
  t.now <- now;
  (match received t with [] -> () | s -> on_signals t s);
  if Queue.is_empty t.events then read_input t ~ready else expire t;
  take_posts t;
  t.owe_event <- not (Queue.is_empty t.events);
  t.owe_timer <- has_due_timer t;
  t.owe_post <- not (Queue.is_empty t.posted)

(* Frames are an interval apart from when they are drawn, which a turn's
   work may delay past the time the turn read. *)
let start_frame t =
  t.dirty <- false;
  t.asked_outside <- false;
  t.in_frame_step <- true;
  t.drawing <- true;
  t.void <- false;
  if t.drawn <> no_frame then t.frame_dt <- t.latest - t.drawn;
  t.drawn <- t.latest;
  (* A frame a device draws before the interval has passed keeps the
     animation's grid. *)
  if t.latest >= t.last_frame + t.interval then t.last_frame <- t.latest;
  t.on_frame ();
  Frame

(* Work goes on until one frame interval has passed since the turn began. *)
let within_budget t = t.latest - t.now < t.interval

(* The turn has no step left: the run waits for what {!fds} and
   {!deadline} say. Raised rather than returned in an option, so that
   [next]'s idle turns allocate nothing. *)
exception Idle

(* Whether the run's stop was requested. *)
let stop_requested t =
  match t.stop with Some s -> Atomic.get s.Stop.requested | None -> false

let rec work t =
  if stop_requested t then Stopped
  else if t.owe_event && not (Queue.is_empty t.events) then (
    t.owe_event <- false;
    deliver t (Queue.pop t.events))
  else if t.owe_timer && has_due_timer t then (
    t.owe_timer <- false;
    take_timer t)
  else if t.owe_post && not (Queue.is_empty t.posted) then (
    t.owe_post <- false;
    take_post t)
  else if (not (Queue.is_empty t.events)) && within_budget t then
    deliver t (Queue.pop t.events)
  else if has_due_timer t && within_budget t then take_timer t
  else if (not (Queue.is_empty t.posted)) && within_budget t then take_post t
  else if t.input_ended && Queue.is_empty t.events then Stopped
  else if frame_due t then begin_frame t
  else raise_notrace Idle

(* The latest size is applied before a frame; a change of the usable size is
   returned first. The frame follows unless it must wait for the cursor
   report that says where the live region went. *)
and begin_frame t =
  if t.size_report <> Applied then apply_size t;
  let usable = size t in
  if usable <> t.usable then (
    t.usable <- usable;
    t.frame_follows <- not (render_blocked t);
    Event (Resize (fst usable, snd usable)))
  else if render_blocked t then raise_notrace Idle
  else start_frame t

(* A turn begins after every time the run had nothing to do, and when the
   work of the last one is done. The first has the input read while the
   terminal was taken, and what was posted and set since. *)
let turn t =
  begin_turn t ~now:t.latest;
  match work t with
  | step -> step
  | exception Idle ->
      t.begun <- false;
      raise_notrace Idle

(* The application's time between steps, for the overlay's [app]. *)
let entered t =
  match t.tools with
  | Some tools when tools.returned >= 0 ->
      tools.app <- tools.app + (Tty.now () - tools.returned);
      tools.returned <- -1
  | _ -> ()

let returned t =
  match t.tools with Some tools -> tools.returned <- Tty.now () | None -> ()

let step_exn t ~now =
  check t;
  entered t;
  if now < t.latest then
    invalid_arg "Matrix.step: now is earlier than an earlier step's";
  t.latest <- now;
  t.drawing <- false;
  t.in_frame_step <- false;
  if t.frame_follows then (
    t.frame_follows <- false;
    start_frame t)
  else if not t.begun then (
    t.begun <- true;
    turn t)
  else match work t with step -> step | exception Idle -> turn t

let step t ~now =
  match step_exn t ~now with
  | step ->
      returned t;
      Some step
  | exception Idle -> None

let deadline t =
  check t;
  wait_deadline t

let fds t =
  check t;
  t.backend.fds ~reading:(not t.input_ended)

let clock = Tty.now

(* [step] waiting with [select(2)], through the backend: the wait says
   whether the input is readable, so the turn after it looks no further. *)
let rec next t =
  match step_exn t ~now:(t.backend.now ()) with
  | step ->
      t.stepped <- true;
      returned t;
      step
  | exception Idle ->
      let until = wait_deadline t in
      t.waited_ready <-
        (match t.config.device with
        | Some d ->
            (* A device decides how a run that waits goes on. *)
            let stepped = t.stepped in
            t.stepped <- false;
            d.idle ~until ~stepped && not t.input_ended
        | None -> wait_only t ~until);
      t.waited <- true;
      next t

(* Running *)

let make backend ?stop config =
  let evidence =
    match config.caps with
    | Some caps -> Terminal.Caps.assume caps
    | None -> Terminal.Caps.evidence backend.Backend.env
  in
  let terminal =
    Terminal.make ~tty:backend.output_is_tty
      ~output:(fun s ->
        backend.write_output (Bytes.unsafe_of_string s) 0 (String.length s))
      (Terminal.Caps.derive evidence)
  in
  (* An output with no window has the size of a classic terminal until its
     terminal reports one. *)
  let cols, rows = Option.value (backend.terminal_size ()) ~default:(80, 24) in
  let width = max 1 cols and height = max 1 rows in
  let screen =
    Screen.create ~cursor_visible:false ~explicit_width:config.explicit_width ()
  in
  let now = backend.now () in
  {
    serial = Atomic.fetch_and_add serials 1;
    terminal;
    decoder = Input.Decoder.create ();
    input_buffer = Bytes.create 4096;
    input_ended = false;
    last_input = now;
    rtt = 0;
    evidence;
    batches = [];
    lost_reports = 0;
    waiting = [];
    events = Queue.create ();
    config;
    backend;
    stop;
    owner = thread_id ();
    screen;
    submit_buf = Ansi.Writer.create 4096;
    width;
    height;
    primary =
      Primary.create ~terminal_height:height
        ~min_live_height:config.min_tui_height;
    layout_ops = [];
    cursor_live_row = None;
    anchor = None;
    scroll_hint = None;
    last_cursor_position = None;
    last_cursor_visible = None;
    last_cursor_shape = None;
    last_cursor_color = None;
    now;
    heap = heap_create ();
    mailbox = Atomic.make [];
    accepting = Atomic.make true;
    posted = Queue.create ();
    pending = Atomic.make 0;
    room_lock = Mutex.create ();
    room = Condition.create ();
    begun = false;
    latest = now;
    waited = false;
    waited_ready = false;
    owe_event = false;
    owe_timer = false;
    owe_post = false;
    interval = Float.to_int (Float.round (1e9 /. config.fps));
    dirty = true;
    in_process = Option.is_some config.device;
    asked_outside = true;
    in_frame_step = false;
    stepped = false;
    on_frame =
      (match config.device with Some d -> d.frame | None -> Fun.const ());
    tools = Option.bind config.debug tools_of_string;
    drawing = false;
    void = false;
    last_frame = no_frame;
    frame_dt = 0;
    drawn = no_frame;
    force_full_next_frame = true;
    size_report = Applied;
    usable = (width, height);
    cell_answer = None;
    window_pixels = backend.window_pixels ();
    cell_size = None;
    frame_follows = false;
    frame_modes_armed = false;
    restore_modes_on_next_focus = false;
    released = false;
    modes_taken = false;
    taken = [];
    kept = [];
    tstp_ignored = false;
    undo = [];
  }

(* A terminal answers queries only on a raw input: the probe waits for its
   answers, and input that comes meanwhile is kept for the run. Whether the
   terminal answered it is returned. *)
let probe t =
  if not (can_query t) then false
  else
    let b =
      send_batch t (Capabilities Probe) (Terminal.Caps.ask t.evidence Probe)
    in
    await t ~from:b.sent [ b ];
    let cols, rows = device_size t in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    t.primary <-
      Primary.create ~terminal_height:t.height
        ~min_live_height:t.config.min_tui_height;
    if t.config.mode = `Primary && not t.input_ended then (
      let b = ask_cursor t At_cursor in
      t.anchor <- Some b;
      await t ~from:b.sent [ b ];
      t.anchor <- None);
    b.answered

(* The measure of text sizing writes on the screen, from the row the cursor
   is on to the end, so it is written once the screen is taken, on rows the
   run owns, and only to a terminal that answered the probe. In [`Alt] mode
   that is the alternate screen's second row, so that a late report,
   [CSI 2 ; col R], is never the modified F3 [CSI 1 ; m R]. In [`Primary]
   mode it is the live region's first row, the region given the two rows a
   scaled space takes. *)
let measure_widths t =
  if can_query t then (
    let row =
      match t.config.mode with
      | `Alt -> 2
      | `Primary ->
          relayout t (Primary.require_rows t.primary ~rows:2);
          Primary.render_offset t.primary + 1
    in
    let w = Ansi.Writer.create 64 in
    apply_primary_ops t w t.layout_ops;
    t.layout_ops <- [];
    buf_cursor_position w ~row ~col:1;
    Terminal.send t.terminal (Ansi.Writer.contents w);
    let b =
      send_batch t (Capabilities Widths) (Terminal.Caps.ask t.evidence Widths)
    in
    await t ~from:b.sent [ b ])

(* An output that is not a terminal has no window to read a size from. The
   size its terminal has already reported in band, as the transport of a
   served session writes it ahead of the run, is taken before the first
   frame: the input there is read and decoded, and kept for the run. *)
let take_reported_size t =
  if not t.backend.output_is_tty then (
    read_input t ~ready:(wait_only t ~until:past);
    match t.size_report with
    | Reported (cols, rows) ->
        t.width <- max 1 cols;
        t.height <- max 1 rows;
        t.size_report <- Applied;
        t.primary <-
          Primary.create ~terminal_height:t.height
            ~min_live_height:t.config.min_tui_height
    | Applied | Read -> ())

let setup t =
  take_signals t;
  take_fatal_reset t;
  take_raw_mode t;
  take_reported_size t;
  let answered = probe t in
  take_screen t;
  if answered then measure_widths t;
  take_modes t;
  update_cell_size t;
  ask_pixels t;
  t.usable <- size t

(* Registers [t], unless its terminal is the controlling one and another
   run holds it. *)
let hold t =
  Mutex.protect runs_lock (fun () ->
      if
        t.backend.controlling
        && List.exists
             (fun (Held app) -> app.backend.controlling && not app.released)
             !runs
      then false
      else (
        runs := Held t :: !runs;
        true))

let unhold t =
  Mutex.protect runs_lock (fun () ->
      runs := List.filter (fun (Held app) -> app.serial <> t.serial) !runs)

(* The process can end while a run holds its terminal: a program that calls
   [exit] in a run, or whose main domain returns while a run on another
   domain is still giving its terminal back. Its exit path restores every
   terminal not yet given back, as an abort does; one given back is left
   alone. *)
let exit_restores = Atomic.make false

let restore_on_exit () =
  if not (Atomic.exchange exit_restores true) then at_exit Signal.restore_all

let run_on backend ?stop ?(config = config ()) f =
  restore_on_exit ();
  let t =
    try make backend ?stop config
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try backend.Backend.cleanup () with _ -> ());
      Printexc.raise_with_backtrace exn bt
  in
  if not (hold t) then (
    (try backend.cleanup () with _ -> ());
    invalid_arg "Matrix.run: another run holds the terminal");
  Option.iter (fun s -> Stop.attach s t.serial t.backend.wake) stop;
  (* A device reads the frame the run presented, and the terminal row its
     first row is on. *)
  Option.iter
    (fun d ->
      d.presented (fun () ->
          ( current_grid t,
            (match t.config.mode with
            | `Alt -> 0
            | `Primary -> Primary.render_offset t.primary),
            (* The screen writes half blocks without a cell size. *)
            match Screen.cell_size t.screen with
            | None -> `Blocks
            | Some _ -> (Terminal.capabilities t.terminal).graphics )))
    config.device;
  let finish () =
    Atomic.set t.accepting false;
    wake_posts t;
    Option.iter (fun s -> Stop.detach s t.serial) stop;
    unhold t;
    t.backend.cleanup ()
  in
  let teardown () =
    Fun.protect ~finally:finish (fun () ->
        t.now <- t.backend.now ();
        give_back t ~exiting:true)
  in
  match
    setup t;
    f t
  with
  | result ->
      teardown ();
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try teardown () with _ -> ());
      Printexc.raise_with_backtrace exn bt

let run ?input ?output ?env ?stop ?config f =
  let backend =
    match (config, input, output, env) with
    | Some { device = Some d; _ }, None, None, None -> backend_of_device d
    | Some { device = Some _; _ }, _, _, _ ->
        invalid_arg "Matrix.run: a run on a device takes no descriptors"
    | _ -> Backend.unix ?input ?output ?env ()
  in
  let config =
    match config with
    | Some { device = Some _; _ } -> config
    | _ ->
        let c = Option.value config ~default:(default_config ()) in
        Some { c with debug = Sys.getenv_opt "MATRIX_DEBUG" }
  in
  run_on backend ?stop ?config f

(* Diagnostics *)

module Debug_overlay = Debug_overlay

(* The frame being drawn is in the screen's next grid until [draw]
   presents it; after, that grid is the next frame's, cleared when next
   read. *)
let dump_frame ?(hits = false) ?dir ?pattern t =
  check t;
  if not t.drawing then invalid_arg "Matrix.dump_frame: no frame is being drawn";
  Frame_dump.snapshot ?dir ?pattern ~hits t.screen
