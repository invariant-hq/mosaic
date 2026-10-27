module Caps = Matrix_terminal.Caps
module Region = Matrix_screen.Region
module Signal = Tty.Signal

(* A region is drawn at most this often, unless text goes above it: ten
   times a second, as cargo throttles its progress bar
   (src/util/progress.rs, [Throttle::allowed], "Duration::from_millis(100)"),
   as Rich's Progress refreshes by default ("refresh_per_second ... default
   (10)") and as ocaml-console redraws ("redraws the progress rows ten times
   a second"). indicatif draws up to 20 times a second ("at a max of 20
   times a second"); the majority's rate is kept. *)
let interval = 100_000_000

(* How long a process that ends on a signal waits for a write in progress
   and for the terminal to take its last bytes: a terminal that stops
   reading, as under Ctrl+S, must not keep the process from ending. *)
let teardown_wait = 500_000_000

(* Writing *)

let rec write_all fd s off len =
  if len > 0 then
    match Unix.single_write_substring fd s off len with
    | n -> write_all fd s (off + n) (len - n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off len
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (try ignore (Unix.select [] [ fd ] [] (-1.))
         with Unix.Unix_error (Unix.EINTR, _, _) -> ());
        write_all fd s off len

(* The bytes a process writes as it ends: as many as the terminal takes
   before [until]. *)
let write_until fd s until =
  let rec go off =
    let left = until - Tty.now () in
    if off < String.length s && left > 0 then
      match Unix.select [] [ fd ] [] (Float.of_int left *. 1e-9) with
      | _, [], _ -> ()
      | _ -> (
          match
            Unix.single_write_substring fd s off (String.length s - off)
          with
          | n -> go (off + n)
          | exception Unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
              go off)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go off
  in
  try go 0 with Unix.Unix_error _ -> ()

(* What the program printed through the standard channels goes out first,
   so that [2>&1] keeps the order of the two outputs. *)
let flush_std () =
  (try Stdlib.flush Stdlib.stdout with Sys_error _ -> ());
  try Stdlib.flush Stdlib.stderr with Sys_error _ -> ()

(* And what it printed through the standard formatters, such as with
   [Fmt.pr], before the standard channels. Formatters are not thread-safe,
   so this runs on the thread of the program's call only, and before the
   device's lock is taken, since an adapted formatter writes through an
   output, which takes it. *)
let flush_formatters () =
  (try Format.pp_print_flush Format.std_formatter () with Sys_error _ -> ());
  try Format.pp_print_flush Format.err_formatter () with Sys_error _ -> ()

(* The clock *)

(* A thread that calls [fire] whenever the time [due] gives has come, with
   [lock] held, as both are called. It sleeps until then, at most [interval]
   at a time, so that a time made earlier while it sleeps comes at most
   [interval] late, and waits for [wake] while [due] gives none. It runs on
   every system, since a region's frames and ticks need no signal. *)
type clock = { ready : Condition.t; mutable stopped : bool }

let clock lock ~due fire =
  let c = { ready = Condition.create (); stopped = false } in
  let rec run () =
    if not c.stopped then
      match due () with
      | None ->
          Condition.wait c.ready lock;
          run ()
      | Some at ->
          let left = at - Tty.now () in
          if left <= 0 then (
            fire ();
            run ())
          else (
            Mutex.unlock lock;
            Thread.delay (Float.of_int (min left interval) *. 1e-9);
            Mutex.lock lock;
            run ())
  in
  ignore
    (Thread.create
       (fun () ->
         Mutex.lock lock;
         Fun.protect ~finally:(fun () -> Mutex.unlock lock) run)
       ()
      : Thread.t);
  c

let wake c = Condition.signal c.ready

let stop c =
  c.stopped <- true;
  Condition.signal c.ready

(* Devices *)

(* Descriptors that reach one terminal, or one file, share a device: its
   lock orders their writes, and a region on it takes the lines printed on
   any of them. The controlling terminal is one device whatever the
   descriptor's file, /dev/tty included. *)
type key = Controlling | File of int * int

type device = {
  key : key;
  lock : Mutex.t;
  mutable session : session option;
  mutable users : int;
  mutable live : state option;
  mutable prompt : prompt option;
  next_id : int Atomic.t;  (** The id of the next task shown on it. *)
  mutable asking : int;  (** The prompts that ask on it. *)
  mutable waiting : (t * string) list;
      (** What was written while a prompt asked, newest first. *)
  mutable work_time : (unit -> float) option;
      (** The clock of the work shown on it, if one is. *)
}

(* The signals of a terminal, while a region or a prompt uses it: a holder,
   with a descriptor of its own for the bytes an abort writes, and the
   thread that acts on the signals. *)
and session = {
  holder : Signal.holder;
  output : Unix.file_descr;
  taken : int list;
  mutable thread : Thread.t option;
  mutable stopping : bool;
}

(* A live region, while its scope runs. *)
and state = {
  device : device;
  out : t;  (** The output of the scope that began the region. *)
  region : Region.t;
  mutable parts : part list;
      (** The scopes that show rows in it, in the order they began: a [live]
          inside another on the same terminal joins its region. *)
  mutable cached : (int * int * Matrix_grid.t option) option;
      (** The rows of the parts at a width and a height. *)
  mutable failed : (exn * Printexc.raw_backtrace) option;
  mutable on_screen : bool;
  mutable suspended : int;
  mutable held : string;  (** A line printed above without its end. *)
  mutable last_draw : int;
  mutable pending : bool;
  mutable last_tick : int;  (** When the clock last asked for a draw. *)
  mutable published : string;  (** The teardown of the last whole write. *)
  mutable clock : clock option;
      (** The thread that draws a frame that waits, and the ticks. *)
}

(* The rows of one [live] scope, below those of the scopes before it. *)
and part = {
  mutable frame : width:int -> height:int -> Matrix_grid.t list;
  mutable ended : bool;
  tick : int option;  (** The time between two draws it asks for. *)
}

and prompt = { on_stop : unit -> unit; on_continue : unit -> unit }

(* An output: its descriptor and what was decided for it. *)
and t = {
  fd : Unix.file_descr;
  tty : bool;
  caps : Caps.t;
  styling : Ansi.Styling.t;
  progress : Matrix_cli.Progress.t;
  made : int;  (** When it was made, the origin of its events' times. *)
  columns : int option;
  lines : int option;
  adapter : Matrix_cli.adapter;
  out_device : device;
}

let devices : device list ref = ref []
let devices_lock = Mutex.create ()

let key fd =
  if Tty.is_controlling fd then Controlling
  else
    match Unix.fstat fd with
    | { st_kind = S_CHR; st_rdev; _ } -> File (-1, st_rdev)
    | { st_dev; st_ino; _ } -> File (st_dev, st_ino)
    | exception Unix.Unix_error _ -> File (-2, Hashtbl.hash fd)

let device fd =
  let key = key fd in
  Mutex.protect devices_lock @@ fun () ->
  match List.find_opt (fun d -> d.key = key) !devices with
  | Some d -> d
  | None ->
      let d =
        {
          key;
          lock = Mutex.create ();
          session = None;
          users = 0;
          live = None;
          prompt = None;
          next_id = Atomic.make 0;
          asking = 0;
          waiting = [];
          work_time = None;
        }
      in
      devices := d :: !devices;
      d

(* Takes the lock, or gives up at [until]: a write blocked on a terminal
   that does not read must not keep a signal from being acted on. *)
let lock_until m until =
  let rec go () =
    if Mutex.try_lock m then true
    else if Tty.now () >= until then false
    else (
      Thread.delay 0.002;
      go ())
  in
  go ()

(* Regions *)

let window l = Tty.window l.out.fd

(* The teardown of the write that has just returned, for a signal that ends
   the process before the next: none while the region is off the screen. *)
let publish l =
  l.published <- (if l.on_screen then Region.teardown l.region else "");
  match l.device.session with
  | Some s -> Signal.set_reset s.holder l.published
  | None -> ()

let write l bytes =
  match l.device.session with
  | Some s ->
      let b = Bytes.unsafe_of_string bytes in
      Signal.write s.holder l.out.fd b 0 (Bytes.length b)
  | None -> write_all l.out.fd bytes 0 (String.length bytes)

(* Grids one under another, as the rows of a frame and the frames of the
   parts are. *)
let stack = function
  | [] -> None
  | [ g ] -> Some g
  | gs ->
      let width =
        List.fold_left (fun w g -> max w (Matrix_grid.width g)) 1 gs
      in
      let height = List.fold_left (fun h g -> h + Matrix_grid.height g) 0 gs in
      let dst = Matrix_grid.create ~width ~height () in
      ignore
        (List.fold_left
           (fun y src ->
             let height = Matrix_grid.height src in
             Matrix_grid.blit_region ~src ~dst ~src_x:0 ~src_y:0
               ~width:(Matrix_grid.width src) ~height ~dst_x:0 ~dst_y:y;
             y + height)
           0 gs
          : int);
      Some dst

(* A part's rows at [width] and [height], from the program's function, which
   runs on whichever thread draws: one that raises is dropped, and raised
   again at the program's next call. *)
let part_frame l p ~width ~height =
  match p.frame ~width ~height with
  | gs -> stack gs
  | exception e ->
      if l.failed = None then l.failed <- Some (e, Printexc.get_raw_backtrace ());
      None

(* The region's rows, [height] of which show: each part is given the rows
   left below the parts before it, at least one. *)
let frame_at l ~width ~height =
  match l.cached with
  | Some (w, h, g) when w = width && h = height -> g
  | _ when width < 1 -> None
  | _ ->
      let _, gs =
        List.fold_left
          (fun (above, gs) p ->
            match part_frame l p ~width ~height:(max 1 (height - above)) with
            | Some g -> (above + Matrix_grid.height g, g :: gs)
            | None -> (above, gs))
          (0, []) l.parts
      in
      let g = stack (List.rev gs) in
      l.cached <- Some (width, height, g);
      g

(* The size of the region's rows on a terminal [cols] by [rows]: [rows - 1]
   show below the origin. *)
let region_size l ~cols ~rows = (Region.width l.region ~cols, max 1 (rows - 1))

let reraise l =
  match l.failed with
  | Some (e, bt) ->
      l.failed <- None;
      Printexc.raise_with_backtrace e bt
  | None -> ()

(* One write with [above], the complete lines printed since the last, then
   the frame, the cursor hidden until the region ends or goes off the
   screen, as Rich's Live hides it ("self.console.show_cursor(False)" in
   [Live.start], rich/live.py) and log-update does; called with the
   device's lock held. A region that cannot be drawn now, while it is
   suspended, while the job is in the background or on a terminal of no
   size, lets the lines go out as they are. *)
let draw l ~above =
  flush_std ();
  l.pending <- false;
  let size = window l in
  match size with
  | Some (cols, rows) when l.suspended = 0 && not (Tty.background l.out.fd) ->
      let width, height = region_size l ~cols ~rows in
      let frame = frame_at l ~width ~height in
      let bytes =
        Region.draw l.region ~cols ~rows ~above ~cursor:Hidden frame
      in
      if bytes <> "" then (
        write l bytes;
        l.on_screen <- true;
        publish l);
      l.last_draw <- Tty.now ()
  | _ ->
      if above <> "" then write l above;
      if l.on_screen then Region.moved l.region

(* The lines printed above while a region lives: the complete ones go out
   with the next write, and a line without its end waits for it. *)
let above l bytes =
  let s = l.held ^ bytes in
  match String.rindex_opt s '\n' with
  | None -> l.held <- s
  | Some i ->
      l.held <- String.sub s (i + 1) (String.length s - i - 1);
      draw l ~above:(String.sub s 0 (i + 1))

(* Erases the region, with the line it holds back after it, so that what
   is written next continues that line. *)
let take_off l =
  flush_std ();
  if l.on_screen then
    match window l with
    | Some (cols, _) ->
        write l (Region.suspend l.region ~cols ^ l.held);
        l.held <- "";
        l.on_screen <- false;
        publish l
    | None -> ()
  else if l.held <> "" then (
    write l l.held;
    l.held <- "")

(* Signals *)

let ending = Atomic.make false

let rec forever () =
  Thread.delay 3600.;
  forever ()

let terminate d s =
  if Atomic.compare_and_set ending false true then (
    (* The lock is taken, if the write that holds it ends in time, so that
       the teardown does not land inside a write. *)
    ignore (lock_until d.lock (Tty.now () + teardown_wait) : bool);
    (match d.live with
    | Some l when l.on_screen ->
        write_until l.out.fd l.published (Tty.now () + teardown_wait)
    | _ -> ());
    Option.iter (fun s -> Signal.set_reset s.holder "") d.session;
    (* The settings a prompt changed, and the other holders' terminals. *)
    Signal.restore_all ();
    Signal.raise_default s;
    exit 2)
  else forever ()

(* After a stop, or a SIGCONT that ended someone else's stop: a prompt sets
   its settings again, and the region draws itself from a row of its own;
   called with the device's lock held. *)
let continued d =
  Option.iter (fun p -> p.on_continue ()) d.prompt;
  match d.live with
  | Some l ->
      Region.moved l.region;
      if l.suspended = 0 then draw l ~above:""
  | None -> ()

(* SIGTSTP: the region off the screen and a prompt's settings put back,
   then the stop. It is [true] when the lock was held, and [continued] has
   then run. *)
let stop_job d session =
  let locked = lock_until d.lock (Tty.now () + teardown_wait) in
  Fun.protect
    ~finally:(fun () -> if locked then Mutex.unlock d.lock)
    (fun () ->
      if locked then Option.iter take_off d.live;
      Option.iter (fun p -> p.on_stop ()) d.prompt;
      Signal.raise_default Sys.sigtstp;
      ignore (Signal.take Sys.sigtstp : bool);
      (* The SIGCONT that ended the stop is this one's. *)
      ignore (Signal.received session.holder Sys.sigcont : bool);
      if locked then continued d;
      locked)

let ends = [ Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]

(* When the clock asks for the region's next draw: the shortest time
   between two draws its scopes ask for, at least [interval], after its last
   draw or tick, or never when none asks. *)
let next_tick l =
  let shortest =
    List.fold_left
      (fun shortest p ->
        match (p.tick, shortest) with
        | Some k, Some s -> Some (min k s)
        | Some k, None -> Some k
        | None, _ -> shortest)
      None l.parts
  in
  Option.map (fun k -> max l.last_draw l.last_tick + max interval k) shortest

(* When the region draws next on its clock: when a frame that waits may be
   drawn, or at its next tick. *)
let due l =
  let pending = if l.pending then Some (l.last_draw + interval) else None in
  match (pending, next_tick l) with
  | Some a, Some b -> Some (min a b)
  | (Some _ as at), None | None, (Some _ as at) -> at
  | None, None -> None

(* The clock's draw: on a tick, the frames are called again. *)
let fire l =
  (match next_tick l with
  | Some at when Tty.now () >= at ->
      l.last_tick <- Tty.now ();
      l.cached <- None
  | _ -> ());
  if Tty.now () >= l.last_draw + interval then draw l ~above:""

(* A frame waits for its time, which the clock keeps. *)
let defer l =
  if not l.pending then (
    l.pending <- true;
    Option.iter wake l.clock)

(* The session's thread. It waits on the holder's wake pipe until a signal,
   and never waits for the device's lock before acting on a signal that
   ends the process: the work that needs the lock waits until it can be
   taken. *)
let rec watch d session ~continue ~redraw =
  let wake = Signal.wake_fd session.holder in
  let timeout = if continue || redraw then 0.01 else -1. in
  (try ignore (Unix.select [ wake ] [] [] timeout)
   with Unix.Unix_error (Unix.EINTR, _, _) -> ());
  let buf = Bytes.create 64 in
  (try
     while Unix.read wake buf 0 64 > 0 do
       ()
     done
   with Unix.Unix_error _ -> ());
  if not session.stopping then (
    let h = session.holder in
    List.iter (fun s -> if Signal.received h s then terminate d s) ends;
    let continue =
      if Signal.received h Sys.sigtstp then not (stop_job d session)
      else continue
    in
    let continue = Signal.received h Sys.sigcont || continue in
    let redraw = Signal.received h Sys.sigwinch || redraw in
    if (continue || redraw) && Mutex.try_lock d.lock then (
      Fun.protect
        ~finally:(fun () -> Mutex.unlock d.lock)
        (fun () ->
          if continue then continued d
          else
            match d.live with
            | Some l ->
                if Tty.now () >= l.last_draw + interval then draw l ~above:""
                else defer l
            | None -> ());
      watch d session ~continue:false ~redraw:false)
    else watch d session ~continue ~redraw)

(* [exit] runs the program's [at_exit] functions and ends the process,
   leaving every [live] scope that runs unended. What ends them is set once
   they are defined, below. *)
let end_scopes = ref ignore
let exit_registered = Atomic.make false

(* The first region, prompt or scope registers what [exit] does: the scopes
   end, then the terminals' settings are put back. A program's [at_exit]
   function registered before then runs after, with no region on the
   screen. *)
let register_exit () =
  if not (Atomic.exchange exit_registered true) then
    at_exit (fun () ->
        !end_scopes ();
        if not Sys.win32 then Signal.restore_all ())

(* A use of the device's signals begins: the first takes them, for the
   controlling terminal its job-control and window signals too. *)
let enter d fd =
  d.users <- d.users + 1;
  register_exit ();
  if d.session = None && not Sys.win32 then (
    let output = Unix.dup ~cloexec:true fd in
    let controlling = d.key = Controlling in
    let holder = Signal.hold ~controlling ~output:(Some output) in
    let taken = List.filter Signal.take (Signal.signals ~controlling) in
    let session = { holder; output; taken; thread = None; stopping = false } in
    d.session <- Some session;
    session.thread <-
      Some
        (Thread.create
           (fun () -> watch d session ~continue:false ~redraw:false)
           ()))

(* A use ends; called without the device's lock, which the thread may
   wait for. *)
let leave d =
  let last =
    Mutex.protect d.lock (fun () ->
        d.users <- d.users - 1;
        if d.users = 0 then (
          let s = d.session in
          d.session <- None;
          s)
        else None)
  in
  match last with
  | None -> ()
  | Some session ->
      session.stopping <- true;
      Signal.wake session.holder;
      Option.iter Thread.join session.thread;
      List.iter Signal.give_back session.taken;
      Signal.release session.holder;
      Unix.close session.output

(* Outputs *)

(* The Windows consoles whose VT processing an output turned on, which the
   exit path turns back off: the console is the shell's too. *)
let consoles = ref []

let make ?color ?progress fd =
  let getenv = Sys.getenv_opt in
  let tty = Tty.is_tty fd in
  let win32 = Sys.win32 && tty in
  if win32 && not (List.mem fd !consoles) then (
    consoles := fd :: !consoles;
    let saved = Tty.enable_vt fd in
    at_exit (fun () -> Tty.restore_console fd saved));
  let terminfo =
    match getenv "TERM" with
    | Some term when term <> "" -> Result.to_option (Tty.terminfo term)
    | _ -> None
  in
  let caps = Caps.(derive (evidence (env ?terminfo getenv ~win32))) in
  let columns =
    match Option.bind (getenv "COLUMNS") int_of_string_opt with
    | Some n when n > 0 -> Some (min n Matrix_grid.max_width)
    | _ -> None
  in
  let lines =
    match Option.bind (getenv "LINES") int_of_string_opt with
    | Some n when n > 0 -> Some n
    | _ -> None
  in
  let styling =
    Ansi.Styling.decide ?color getenv ~tty ~dumb:caps.dumb caps.colors
  in
  {
    fd;
    tty;
    caps;
    styling;
    progress = Matrix_cli.Progress.decide ?progress getenv ~tty ~dumb:caps.dumb;
    made = Tty.now ();
    columns;
    lines;
    adapter = Matrix_cli.adapter styling;
    out_device = device fd;
  }

let progress t = t.progress
let stdout ?color ?progress () = make ?color ?progress Unix.stdout
let stderr ?color ?progress () = make ?color ?progress Unix.stderr
let styling t = t.styling

let width t =
  match t.columns with
  | Some _ as w -> w
  | None -> Option.map fst (Tty.window t.fd)

(* Writes bytes already adapted, above the region of the device if one is
   on the screen; called with the device's lock held. *)
let emit t bytes =
  flush_std ();
  let d = t.out_device in
  if d.asking > 0 then d.waiting <- (t, bytes) :: d.waiting
  else
    match d.live with
    | Some l when l.on_screen && l.suspended = 0 -> above l bytes
    | _ -> write_all t.fd bytes 0 (String.length bytes)

(* JSON events, one object on a line. A string is valid UTF-8, as
   Ansi.sanitize leaves it, with its C0 controls and DEL escaped. A time is
   in seconds, rounded to the millisecond, as ocaml-console writes them. *)

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 || c = '\127' ->
          Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let json_number f =
  let f = if Float.is_finite f then Float.round (f *. 1000.) /. 1000. else 0. in
  Printf.sprintf "%.15g" f

let event members =
  "{"
  ^ String.concat "," (List.map (fun (k, v) -> json_string k ^ ":" ^ v) members)
  ^ "}\n"

(* What a program prints on a [`Json] output: an output event of no task for
   each of its lines, and for what follows its last line end. *)
let output_events t s =
  let time =
    json_number
      (match t.out_device.work_time with
      | Some now -> now ()
      | None -> Float.of_int (Tty.now () - t.made) *. 1e-9)
  in
  let lines = String.split_on_char '\n' (Ansi.sanitize s) in
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  String.concat ""
    (List.map
       (fun l ->
         event
           [
             ("time", time);
             ("event", json_string "output");
             ("text", json_string l);
           ])
       lines)

let print t s =
  flush_formatters ();
  Mutex.protect t.out_device.lock (fun () ->
      let bytes =
        match t.progress with
        | `Json -> output_events t s
        | _ -> Matrix_cli.adapt t.adapter s
      in
      if bytes <> "" then emit t bytes)

let out_functions t =
  let pending = Buffer.create 256 in
  let out_string s off len = Buffer.add_substring pending s off len in
  (* A flush with nothing to write writes nothing: [print] flushes the
     standard formatters, which may be this one, flushing. *)
  let out_flush () =
    if Buffer.length pending > 0 then (
      let s = Buffer.contents pending in
      Buffer.clear pending;
      print t s)
  in
  let spaces n = if n > 0 then out_string (String.make n ' ') 0 n in
  {
    Format.out_string;
    out_flush;
    out_newline = (fun () -> out_string "\n" 0 1);
    out_spaces = spaces;
    out_indent = spaces;
    out_width =
      (fun s ~pos ~len ->
        Matrix_text.measure_sub ~width_method:t.caps.unicode_width ~tab_width:8
          s ~pos ~len);
  }

let set_margin ppf t =
  match width t with Some w -> Format.pp_set_margin ppf (max 2 w) | None -> ()

let formatter t =
  let ppf = Format.formatter_of_out_functions (out_functions t) in
  set_margin ppf t;
  ppf

let adapt_std_formatters ?color () =
  let out = stdout ?color () and err = stderr ?color () in
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  Format.pp_set_formatter_out_functions Format.std_formatter (out_functions out);
  Format.pp_set_formatter_out_functions Format.err_formatter (out_functions err);
  set_margin Format.std_formatter out;
  set_margin Format.err_formatter err

let write_raw t s =
  flush_formatters ();
  Mutex.protect t.out_device.lock (fun () ->
      flush_std ();
      match t.out_device.live with
      | Some l when l.on_screen && l.suspended = 0 ->
          take_off l;
          write l s;
          draw l ~above:""
      | _ -> write_all t.fd s 0 (String.length s))

(* A notification and the bell move no cursor, so they go out between two
   writes of a region as they are. *)
let control t f =
  if t.tty && not t.caps.dumb then
    Mutex.protect t.out_device.lock (fun () ->
        flush_std ();
        let output s = write_all t.fd s 0 (String.length s) in
        f (Matrix_terminal.make ~output t.caps))

let notify t ~title ~body =
  control t (fun term -> Matrix_terminal.notify term ~title ~body)

let bell t = control t Matrix_terminal.bell

(* Live regions *)

type live = {
  state : state option;  (** [None] where no region may be drawn. *)
  part : part;
  mutable last : width:int -> height:int -> Matrix_grid.t list;
}

let no_frame ~width:_ ~height:_ = []

(* A scope's rows join the region that lives on the terminal, or begin
   one. *)
let join t p =
  let d = t.out_device in
  Mutex.protect d.lock (fun () ->
      let l =
        match d.live with
        | Some l ->
            l.parts <- l.parts @ [ p ];
            l.cached <- None;
            l
        | None ->
            let l =
              {
                device = d;
                out = t;
                region =
                  Region.create ~eager_wrap:t.caps.eager_wrap
                    ~lifts_resting_cursor:t.caps.lifts_resting_cursor
                    ~styling:t.styling ();
                parts = [ p ];
                cached = None;
                failed = None;
                on_screen = false;
                suspended = 0;
                held = "";
                last_draw = 0;
                pending = false;
                last_tick = 0;
                published = "";
                clock = None;
              }
            in
            enter d t.fd;
            l.clock <-
              Some (clock d.lock ~due:(fun () -> due l) (fun () -> fire l));
            d.live <- Some l;
            l
      in
      (* The scope's tick may come before the clock's next draw. *)
      Option.iter wake l.clock;
      l)

let plain_width t = max 1 (Option.value (width t) ~default:80)

(* The height of the last rows printed where no region is drawn: [LINES],
   POSIX's "user's preferred number of lines on a page or in a window",
   else the terminal's, else the 24 rows that pair with 80 columns. *)
let plain_height t =
  match t.lines with
  | Some n -> n
  | None -> Option.fold ~none:24 ~some:snd (Tty.window t.fd)

(* Rows as lines, each ended where it shows nothing more. *)
let lines t = function
  | Some g -> Matrix_grid.to_ansi ~styling:t.styling ~trim:true g ^ "\n"
  | None -> ""

(* The bytes that end the region, its last rows being [frame]'s: its last
   write where it can be drawn, else the rows as lines after the line it
   holds back. The device then has no region; called with its lock held. *)
let closing l frame =
  l.device.live <- None;
  Option.iter stop l.clock;
  let held = if l.held = "" then "" else l.held ^ "\n" in
  l.held <- "";
  let drawable = l.suspended = 0 && not (Tty.background l.out.fd) in
  match window l with
  | Some (cols, rows) when drawable ->
      let width, height = region_size l ~cols ~rows in
      Region.close l.region ~cols ~rows ~above:held (frame ~width ~height)
  | _ ->
      held
      ^ lines l.out
          (frame ~width:(plain_width l.out) ~height:(plain_height l.out))

(* A scope ends. While other scopes show rows, its own last rows go above
   the region, which stays for them. The last scope to end closes the
   region, its last frame staying below the output. *)
let finish t l p =
  let d = l.device in
  let last =
    Mutex.protect d.lock (fun () ->
        p.ended <- true;
        l.parts <- List.filter (fun q -> q != p) l.parts;
        l.cached <- None;
        flush_std ();
        let drawable = l.suspended = 0 && not (Tty.background l.out.fd) in
        match (l.parts, window l) with
        | _ :: _, size ->
            let width, height =
              match size with
              | Some (cols, rows) -> region_size l ~cols ~rows
              | None -> (plain_width t, plain_height t)
            in
            let above = lines t (part_frame l p ~width ~height) in
            if l.on_screen && drawable then draw l ~above
            else if above <> "" then write l above;
            false
        | [], _ ->
            write l (closing l (part_frame l p));
            Option.iter (fun s -> Signal.set_reset s.holder "") d.session;
            true)
  in
  if last then leave d

(* The scopes where no region may be drawn, in the order they began: each
   prints its last rows when it ends. *)
let plain_scopes : (t * live) list ref = ref []
let plain_lock = Mutex.create ()

let print_last t v =
  flush_formatters ();
  Mutex.protect t.out_device.lock (fun () ->
      let lines =
        lines t (stack (v.last ~width:(plain_width t) ~height:(plain_height t)))
      in
      if lines <> "" then emit t lines)

let live_region t part f =
  let l = join t part in
  match f { state = Some l; part; last = no_frame } with
  | result ->
      flush_formatters ();
      finish t l part;
      reraise l;
      result
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      (try
         flush_formatters ();
         finish t l part
       with _ -> ());
      Printexc.raise_with_backtrace e bt

let live ?tick t f =
  let tick =
    match tick with
    | Some s when not (s > 0.) ->
        invalid_arg "Matrix_cli_unix.live: tick must be positive"
    | Some s -> Some (Float.to_int (Float.min (s *. 1e9) 1e18))
    | None -> None
  in
  flush_formatters ();
  let part = { frame = no_frame; ended = false; tick } in
  match t.progress with
  | `Quiet | `Json -> f { state = None; part; last = no_frame }
  | `Tty -> live_region t part f
  | `Plain -> (
      let v = { state = None; part; last = no_frame } in
      register_exit ();
      Mutex.protect plain_lock (fun () ->
          plain_scopes := !plain_scopes @ [ (t, v) ]);
      let ended () =
        Mutex.protect plain_lock (fun () ->
            plain_scopes := List.filter (fun (_, w) -> w != v) !plain_scopes);
        print_last t v
      in
      match f v with
      | result ->
          ended ();
          result
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          (try ended () with _ -> ());
          Printexc.raise_with_backtrace e bt)

(* At [exit], each region ends as its last scope's end would end it, with
   the rows of every scope that runs, within the wait a signal's teardown
   has; one whose lock that wait cannot take is left to the teardown that
   [Signal.restore_all] writes next. Then each scope where no region may be
   drawn prints its last rows. A process that a signal ends has torn its
   regions down already. *)
let end_at_exit () =
  if not (Atomic.get ending) then (
    flush_formatters ();
    let devices = Mutex.protect devices_lock (fun () -> !devices) in
    List.iter
      (fun d ->
        let until = Tty.now () + teardown_wait in
        if lock_until d.lock until then
          Fun.protect ~finally:(fun () -> Mutex.unlock d.lock) @@ fun () ->
          match d.live with
          | None -> ()
          | Some l ->
              flush_std ();
              l.cached <- None;
              write_until l.out.fd (closing l (frame_at l)) until;
              Option.iter (fun s -> Signal.set_reset s.holder "") d.session)
      devices;
    let plain =
      Mutex.protect plain_lock (fun () ->
          let p = !plain_scopes in
          plain_scopes := [];
          p)
    in
    List.iter (fun (t, v) -> try print_last t v with _ -> ()) plain)

let () = end_scopes := end_at_exit

let show v frame =
  v.last <- frame;
  match v.state with
  | None -> ()
  | Some l ->
      Mutex.protect l.device.lock (fun () ->
          if v.part.ended then
            invalid_arg "Matrix_cli_unix.show: the region's scope has ended";
          reraise l;
          v.part.frame <- frame;
          l.cached <- None;
          if Tty.now () >= l.last_draw + interval then draw l ~above:""
          else defer l)

let print_grid t g =
  flush_formatters ();
  Mutex.protect t.out_device.lock (fun () -> emit t (lines t (Some g)))

(* Runs [f] with the region of [d], if one lives, off the screen. *)
let off_screen d f =
  flush_formatters ();
  match Mutex.protect d.lock (fun () -> d.live) with
  | None -> f ()
  | Some l ->
      Mutex.protect d.lock (fun () ->
          take_off l;
          l.suspended <- l.suspended + 1);
      Fun.protect f ~finally:(fun () ->
          Mutex.protect d.lock (fun () ->
              l.suspended <- l.suspended - 1;
              Region.moved l.region;
              if l.suspended = 0 then draw l ~above:""))

let suspend v f =
  match v.state with None -> f () | Some l -> off_screen l.device f

(* Tasks *)

type output = t

(* What a program writes through an output that is already events or lines
   of its own: adapted to the output's styling, and as it is on a [`Json]
   output. *)
let write_lines t s =
  flush_formatters ();
  Mutex.protect t.out_device.lock (fun () ->
      let bytes =
        match t.progress with `Json -> s | _ -> Matrix_cli.adapt t.adapter s
      in
      if bytes <> "" then emit t bytes)

(* The glyphs of tasks, all in one place, for the day a terminal that does
   not show UTF-8 is told apart. Their meanings are also in words where a
   line is read without them: [failed] and [cancelled] end a line. *)
module Glyph = struct
  let running = "\u{2022}"
  let succeeded = "\u{2713}"
  let failed = "\u{2717}"
  let cancelled = "-"
  let path = " \u{25B8} "
  let output = "\u{203A}"

  let spinner =
    [|
      "\u{280B}";
      "\u{2819}";
      "\u{2839}";
      "\u{2838}";
      "\u{283C}";
      "\u{2834}";
      "\u{2826}";
      "\u{2827}";
      "\u{2807}";
      "\u{280F}";
    |]

  let bar_full = "\u{2501}"
  let bar_empty = "\u{2500}"
end

module Task = struct
  type status = [ `Running | `Succeeded | `Failed | `Cancelled ]

  type t = {
    work : work;
    id : int;  (** [-1] for the root. *)
    parent : t option;
    mutable label : string;
    transient : bool;
    lines : int;  (** The lines of output its window shows. *)
    started : float;
    mutable count : int;
    mutable total : int option;
    mutable bytes : bool;
    mutable counted : (float * int) option;
        (** When it was last counted, and its count then. *)
    mutable rate : float option;
    mutable output : string list;  (** Newest first. *)
    mutable status : status;
    mutable message : string option;
    mutable ended : float option;
    mutable children : t list;  (** Newest first. *)
  }

  (* The work as a whole, which its root stands for. [lock] guards the
     fields of its tasks. [order] keeps the lines of two changes in the
     order of the changes, since a change writes them after it lets [lock]
     go, so that a frame, which takes [lock] while the device's lock is
     held, never waits for a write. The order of the locks is [order],
     then [lock], let go before the device's. *)
  and work = {
    out : output;
    now : unit -> float;
    lock : Mutex.t;
    order : Mutex.t;
    mutable live : live option;
    mutable frame : width:int -> height:int -> Matrix_grid.t list;
  }

  type state = {
    label : string;
    status : status;
    message : string option;
    count : int;
    total : int option;
    unit : [ `Items | `Bytes ];
    elapsed : float;
    rate : float option;
    output : string list;
    children : t list;
  }

  let elapsed (t : t) =
    Option.value t.ended ~default:(t.work.now ()) -. t.started

  let state (t : t) =
    Mutex.protect t.work.lock @@ fun () ->
    {
      label = t.label;
      status = t.status;
      message = t.message;
      count = t.count;
      total = t.total;
      unit = (if t.bytes then `Bytes else `Items);
      elapsed = elapsed t;
      rate = t.rate;
      output = List.rev t.output;
      children = List.rev t.children;
    }

  (* A label shows as text, on one line. *)
  let one_line s =
    String.map (function '\n' -> ' ' | c -> c) (Ansi.sanitize s)

  (* How tasks show. *)

  let style ?fg ?bold ?dim () = Ansi.Style.make ?fg ?bold ?dim ()
  let accent = style ~fg:Ansi.Color.cyan ()
  let faint = style ~dim:true ()
  let strong = style ~bold:true ()

  let mark = function
    | `Running -> Ansi.span ~style:accent Glyph.running
    | `Succeeded ->
        Ansi.span ~style:(style ~fg:Ansi.Color.green ()) Glyph.succeeded
    | `Failed -> Ansi.span ~style:(style ~fg:Ansi.Color.red ()) Glyph.failed
    | `Cancelled ->
        Ansi.span ~style:(style ~fg:Ansi.Color.yellow ()) Glyph.cancelled

  let bytes_text n =
    let n = Float.of_int n in
    if n >= 1073741824. then Printf.sprintf "%.2f GiB" (n /. 1073741824.)
    else if n >= 1048576. then Printf.sprintf "%.2f MiB" (n /. 1048576.)
    else if n >= 1024. then Printf.sprintf "%.2f KiB" (n /. 1024.)
    else Printf.sprintf "%.0f B" n

  let counts (t : t) =
    match (t.counted, t.total) with
    | None, _ -> ""
    | Some _, None ->
        if t.bytes then bytes_text t.count else string_of_int t.count
    | Some _, Some total ->
        if t.bytes then bytes_text t.count ^ " / " ^ bytes_text total
        else Printf.sprintf "%d/%d" t.count total

  let seconds f = Printf.sprintf "%.1fs" f

  (* The labels from the top task down to [t]. *)
  let rec path (t : t) =
    match t.parent with None -> [] | Some p -> path p @ [ t.label ]

  let name t = Ansi.span ~style:strong (String.concat Glyph.path (path t))

  (* A line of spans, its columns two cells apart. *)
  let line head columns =
    Ansi.render
      (head
      @ List.concat_map
          (fun c -> [ Ansi.span "  "; c ])
          (List.filter (fun (c : Ansi.span) -> c.text <> "") columns))
    ^ "\n"

  let output_line ?of_task l =
    match of_task with
    | Some t ->
        line
          [
            Ansi.span ~style:faint ("  " ^ Glyph.output ^ " ");
            name t;
            Ansi.span ~style:faint ": ";
            Ansi.span l;
          ]
          []
    | None ->
        line
          [ Ansi.span ~style:faint ("  " ^ Glyph.output ^ " "); Ansi.span l ]
          []

  (* The line of a task that ended. *)
  let ending (t : t) =
    line
      [ mark t.status; Ansi.span " "; name t ]
      [
        Ansi.span (Option.value t.message ~default:"");
        Ansi.span ~style:faint (counts t);
        Ansi.span ~style:faint (seconds (elapsed t));
        Ansi.span
          (match t.status with
          | `Failed -> "failed"
          | `Cancelled -> "cancelled"
          | _ -> "");
      ]

  let rec all (t : t) =
    List.concat_map (fun c -> c :: all c) (List.rev t.children)

  (* The line that says how far the work is. *)
  let summary (root : t) =
    let all = all root in
    let ended = List.filter (fun (t : t) -> t.status <> `Running) all in
    [ mark root.status; Ansi.span " " ]
    @ List.concat_map
        (fun c -> [ c; Ansi.span "  " ])
        (List.filter
           (fun (c : Ansi.span) -> c.text <> "")
           [
             Ansi.span ~style:strong root.label;
             Ansi.span (Option.value root.message ~default:"");
             Ansi.span ~style:faint (seconds (elapsed root));
           ])
    @ [
        Ansi.span ~style:faint
          (Printf.sprintf "%d/%d" (List.length ended) (List.length all));
      ]

  (* The rows of [`Tty]. A task's row is its mark, its label behind its
     guide, a bar when it has a total, then its message, counts, rate and
     time, each column as wide as its widest cell, so the rows line up.
     Under a running task come its latest lines of output, in its window,
     and the rows of the tasks under it. *)

  type row = {
    head : Ansi.span list;
    fraction : float option;
    columns : (Ansi.span * [ `Left | `Right ]) list;
  }

  type item = Row of row | Text of Ansi.span list

  let mark_at now = function
    | `Running ->
        let n = Array.length Glyph.spinner in
        Ansi.span ~style:accent
          Glyph.spinner.(((Float.to_int (now *. 10.) mod n) + n) mod n)
    | status -> mark status

  let worst ts =
    if List.exists (fun (t : t) -> t.status = `Failed) ts then `Failed
    else if List.exists (fun (t : t) -> t.status = `Cancelled) ts then
      `Cancelled
    else `Succeeded

  (* The tasks that ended under a task, as one line, such as [10 done,
     2 failed]. *)
  let folded ended =
    let n status =
      List.length (List.filter (fun (t : t) -> t.status = status) ended)
    in
    String.concat ", "
      (List.filter_map
         (fun (k, word) ->
           if k > 0 then Some (Printf.sprintf "%d %s" k word) else None)
         [
           (n `Succeeded, "done");
           (n `Failed, "failed");
           (n `Cancelled, "cancelled");
         ])

  (* [t]'s rows behind the guide of [lasts], its window at most [window]
     lines, and the tasks that ended under a running one folded when
     [fold]. *)
  let rec tree ~fold ~window lasts (t : t) =
    let guide = Matrix_cli.Tree.single and now = t.work.now () in
    let running = t.status = `Running in
    let row =
      Row
        {
          head =
            [
              Ansi.span ~style:faint (Matrix_cli.Tree.prefix guide lasts);
              mark_at now t.status;
              Ansi.span " ";
              Ansi.span ~style:strong t.label;
            ];
          fraction =
            (match t.total with
            | Some total when running && total > 0 ->
                Some (Float.of_int (min t.count total) /. Float.of_int total)
            | _ -> None);
          columns =
            [
              (Ansi.span (Option.value t.message ~default:""), `Left);
              (Ansi.span ~style:faint (counts t), `Right);
              ( Ansi.span ~style:faint
                  (match t.rate with
                  | Some r when running && t.bytes ->
                      bytes_text (Float.to_int r) ^ "/s"
                  | _ -> ""),
                `Right );
              (Ansi.span ~style:faint (seconds (elapsed t)), `Right);
            ];
        }
    in
    let rails = Matrix_cli.Tree.rails guide lasts in
    let lines =
      if not running then []
      else
        List.rev_map
          (fun l ->
            Text
              [
                Ansi.span ~style:faint (rails ^ "  " ^ Glyph.output ^ " ");
                Ansi.span l;
              ])
          (List.filteri (fun i _ -> i < min t.lines window) t.output)
    in
    (* A transient task that succeeded leaves nothing. *)
    let kids =
      List.filter
        (fun (c : t) -> not (c.transient && c.status = `Succeeded))
        (List.rev t.children)
    in
    let ended, kids =
      if fold && running then
        ( List.filter (fun (c : t) -> c.status <> `Running) kids,
          List.filter (fun (c : t) -> c.status = `Running) kids )
      else ([], kids)
    in
    let last = List.length kids - 1 in
    let fold_line =
      if ended = [] then []
      else
        [
          Text
            [
              Ansi.span ~style:faint
                (Matrix_cli.Tree.prefix guide ((kids = []) :: lasts));
              mark (worst ended);
              Ansi.span ~style:faint (" " ^ folded ended);
            ];
        ]
    in
    (row :: lines) @ fold_line
    @ List.concat
        (List.mapi (fun i c -> tree ~fold ~window ((i = last) :: lasts) c) kids)

  let cells_of spans =
    List.fold_left
      (fun w (s : Ansi.span) ->
        w + Matrix_text.measure ~width_method:`Unicode ~tab_width:2 s.text)
      0 spans

  (* Draws [items] into [g], one a row from [y], in [width] cells: each
     column as wide as its widest cell, the labels giving way to a bar of 10
     cells, and the bar growing to 30 with the room they leave. *)
  let draw_items g ~y ~width items =
    let ncols = 4 in
    let widths = Array.make (ncols + 1) 0 in
    List.iter
      (function
        | Row r ->
            widths.(0) <- max widths.(0) (cells_of r.head);
            List.iteri
              (fun i (c, _) ->
                widths.(i + 1) <- max widths.(i + 1) (cells_of [ c ]))
              r.columns
        | Text _ -> ())
      items;
    let bars =
      List.exists
        (function Row { fraction = Some _; _ } -> true | _ -> false)
        items
    in
    let others =
      Array.fold_left (fun a w -> if w > 0 then a + w + 2 else a) 0 widths
      - if widths.(0) > 0 then widths.(0) + 2 else 0
    in
    let head =
      max 8 (min widths.(0) (width - others - if bars then 12 else 0))
    in
    let cells =
      if bars then max 0 (min 30 (width - others - head - 2)) else 0
    in
    let repeat n s = String.concat "" (List.init (max 0 n) (fun _ -> s)) in
    List.iteri
      (fun i item ->
        let y = y + i in
        match item with
        | Text spans ->
            ignore (Matrix_grid.draw_spans ~width g ~x:0 ~y spans : int)
        | Row r ->
            ignore (Matrix_grid.draw_spans ~width:head g ~x:0 ~y r.head : int);
            let x = ref (head + 2) in
            (match r.fraction with
            | Some f ->
                let full = Float.to_int (f *. Float.of_int cells) in
                ignore
                  (Matrix_grid.draw_spans g ~x:!x ~y
                     [
                       Ansi.span ~style:accent (repeat full Glyph.bar_full);
                       Ansi.span ~style:faint
                         (repeat (cells - full) Glyph.bar_empty);
                     ]
                    : int)
            | None -> ());
            if cells > 0 then x := !x + cells + 2;
            List.iteri
              (fun i (c, align) ->
                let w = widths.(i + 1) in
                if w > 0 then (
                  let pad = w - cells_of [ c ] in
                  let at = if align = `Right then !x + max 0 pad else !x in
                  ignore (Matrix_grid.draw_spans g ~x:at ~y [ c ] : int);
                  x := !x + w + 2))
              r.columns)
      items

  let grid ~width items =
    let g =
      Matrix_grid.create ~width:(max 1 width)
        ~height:(max 1 (List.length items))
        ()
    in
    draw_items g ~y:0 ~width items;
    g

  (* The rows of the running tasks under [t], and the summary under them if
     [t] is the root, which give way in [height] rows: first the tasks that
     ended under each task fold into one line, then the windows shrink, the
     longest first. *)
  let rows (t : t) ~width ~height =
    Mutex.protect t.work.lock @@ fun () ->
    let running =
      List.filter (fun (c : t) -> c.status = `Running) (List.rev t.children)
    in
    let summary = if t.id < 0 then [ Text (summary t) ] else [] in
    let items ~fold ~window =
      List.concat_map (tree ~fold ~window []) running @ summary
    in
    let longest =
      List.fold_left
        (fun a (c : t) -> max a (min c.lines (List.length c.output)))
        0
        (List.concat_map (fun c -> c :: all c) running)
    in
    let rec fit window =
      let items = items ~fold:true ~window in
      if List.length items <= height || window = 0 then items
      else fit (window - 1)
    in
    let whole = items ~fold:false ~window:max_int in
    grid ~width (if List.length whole <= height then whole else fit longest)

  (* The lines a top task that ended leaves above the region: its rows as it
     ended, then the whole output of each task in it that failed. *)
  let leaves (t : t) =
    let width = Option.value (width t.work.out) ~default:80 in
    let rec failed (t : t) =
      (if t.status = `Failed then
         List.rev_map
           (fun l ->
             Ansi.render
               [
                 Ansi.span ~style:faint
                   ("  "
                   ^ String.concat Glyph.path (path t)
                   ^ " " ^ Glyph.output ^ " ");
                 Ansi.span l;
               ]
             ^ "\n")
           t.output
       else [])
      @ List.concat_map failed (List.rev t.children)
    in
    if t.transient && t.status = `Succeeded then ""
    else
      Matrix_grid.to_ansi ~styling:`Truecolor ~trim:true
        (grid ~width (tree ~fold:false ~window:0 [] t))
      ^ "\n"
      ^ String.concat "" (failed t)

  (* JSON events. *)

  let event (t : t) kind members =
    let task =
      if t.id < 0 then []
      else
        [
          ("id", string_of_int t.id);
          ("task", "[" ^ String.concat "," (List.map json_string (path t)) ^ "]");
        ]
    in
    event
      (("time", json_number (t.work.now ()))
       :: ("event", json_string kind)
       :: task
      @ members)

  let status_name = function
    | `Running -> "running"
    | `Succeeded -> "succeeded"
    | `Failed -> "failed"
    | `Cancelled -> "cancelled"

  (* Changes *)

  (* Makes a change under the work's lock, then shows the frame again and
     writes the lines the change gives, in the order of the changes. *)
  let change (w : work) f =
    Mutex.protect w.order @@ fun () ->
    let lines = Mutex.protect w.lock f in
    Option.iter (fun l -> show l w.frame) w.live;
    if lines <> "" then write_lines w.out lines

  let shows_lines (t : t) =
    match t.work.out.progress with
    | `Plain -> not t.transient
    | `Tty | `Quiet | `Json -> false

  let start ?(transient = false) ?(lines = 8) parent label =
    if lines < 0 then invalid_arg "Matrix_cli_unix.Task.start: negative lines";
    let w = parent.work in
    let label = one_line label in
    let made = ref None in
    change w (fun () ->
        let running = parent.status = `Running and now = w.now () in
        let t =
          {
            work = w;
            id = Atomic.fetch_and_add w.out.out_device.next_id 1;
            parent = Some parent;
            label;
            transient;
            lines;
            started = now;
            count = 0;
            total = None;
            bytes = false;
            counted = None;
            rate = None;
            output = [];
            status = (if running then `Running else `Cancelled);
            message = None;
            ended = (if running then None else Some now);
            children = [];
          }
        in
        parent.children <- t :: parent.children;
        made := Some t;
        if not running then ""
        else if shows_lines t then
          line [ mark `Running; Ansi.span " "; name t ] []
        else
          match w.out.progress with
          | `Json ->
              event t "start"
                (if parent.id >= 0 then [ ("parent", string_of_int parent.id) ]
                 else [])
          | _ -> "");
    Option.get !made

  let count ~bytes (t : t) ?total count =
    if count < 0 || Option.fold ~none:false ~some:(fun n -> n < 0) total then
      invalid_arg "Matrix_cli_unix.Task: negative count";
    change t.work @@ fun () ->
    if t.status <> `Running then ""
    else
      let now = t.work.now () in
      (* The rate weighs the past by exp (-dt / 2) for dt seconds ago, and
         starts again at a count below the last, such as a download tried
         again. *)
      (match t.counted with
      | Some (_, before) when count < before || bytes <> t.bytes ->
          t.rate <- None
      | Some (at, before) when now > at ->
          let r = Float.of_int (count - before) /. (now -. at) in
          let k = exp (-.(now -. at) /. 2.) in
          t.rate <-
            Some
              (match t.rate with
              | Some r0 -> (k *. r0) +. ((1. -. k) *. r)
              | None -> r)
      | _ -> ());
      let changed =
        t.counted = None || count <> t.count || total <> t.total
        || bytes <> t.bytes
      in
      t.counted <- Some (now, count);
      t.count <- count;
      t.total <- total;
      t.bytes <- bytes;
      match t.work.out.progress with
      | `Json when changed ->
          event t "progress"
            ([ ("count", string_of_int count) ]
            @ Option.fold ~none:[]
                ~some:(fun n -> [ ("total", string_of_int n) ])
                total
            @ [ ("unit", json_string (if bytes then "bytes" else "items")) ])
      | _ -> ""

  let items t ?total n = count ~bytes:false t ?total n
  let bytes t ?total n = count ~bytes:true t ?total n

  (* The lines of a text, without the empty one after its last line end. *)
  let lines_of text =
    match List.rev (String.split_on_char '\n' (Ansi.sanitize text)) with
    | "" :: rest -> List.rev rest
    | ls -> List.rev ls

  let log (t : t) text =
    let ls = lines_of text in
    change t.work @@ fun () ->
    if t.status <> `Running then ""
    else (
      t.output <- List.rev_append ls t.output;
      if shows_lines t then
        String.concat "" (List.map (output_line ~of_task:t) ls)
      else
        match t.work.out.progress with
        | `Tty when t.id < 0 -> String.concat "" (List.map output_line ls)
        | `Json ->
            String.concat ""
              (List.map
                 (fun l -> event t "output" [ ("text", json_string l) ])
                 ls)
        | _ -> "")

  let rename (t : t) label =
    let label = one_line label in
    change t.work @@ fun () ->
    if t.status <> `Running then ""
    else (
      t.label <- label;
      match t.work.out.progress with
      | `Json -> event t "label" [ ("text", json_string label) ]
      | _ -> "")

  (* Ends [t], and first the tasks under it that run, and is the lines
     their endings write. *)
  let rec close (t : t) status message =
    if t.status <> `Running then ""
    else
      let under = if status = `Succeeded then `Succeeded else `Cancelled in
      let first =
        String.concat "" (List.rev_map (fun c -> close c under None) t.children)
      in
      t.status <- status;
      t.message <- Option.map one_line message;
      t.ended <- Some (t.work.now ());
      let failure () =
        if status = `Failed then
          ending t
          ^ String.concat "" (List.rev_map (fun l -> output_line l) t.output)
        else ""
      in
      let end_event members =
        event t "end"
          ([
             ("status", json_string (status_name status));
             ("elapsed", json_number (elapsed t));
           ]
          @ members
          @ Option.fold ~none:[]
              ~some:(fun m -> [ ("message", json_string m) ])
              t.message)
      in
      let own =
        match t.work.out.progress with
        | `Json -> end_event []
        | _ when t.id < 0 -> ""
        | `Tty -> (
            match t.parent with Some p when p.id < 0 -> leaves t | _ -> "")
        | `Plain when not t.transient -> ending t
        | `Plain | `Quiet -> failure ()
      in
      first ^ own

  let succeed ?message t = change t.work (fun () -> close t `Succeeded message)
  let fail ?message t = change t.work (fun () -> close t `Failed message)
  let cancel ?message t = change t.work (fun () -> close t `Cancelled message)

  let run ?transient ?lines parent label f =
    let t = start ?transient ?lines parent label in
    match f t with
    | result ->
        succeed t;
        result
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        fail ~message:(Printexc.to_string e) t;
        Printexc.raise_with_backtrace e bt
end

let tasks ?(label = "") ?now ?frame out f =
  let now =
    match now with
    | Some now -> now
    | None ->
        let t0 = Tty.now () in
        fun () -> Float.of_int (Tty.now () - t0) *. 1e-9
  in
  let w =
    {
      Task.out;
      now;
      lock = Mutex.create ();
      order = Mutex.create ();
      live = None;
      frame = no_frame;
    }
  in
  let root : Task.t =
    {
      work = w;
      id = -1;
      parent = None;
      label = Task.one_line label;
      transient = false;
      lines = 0;
      started = now ();
      count = 0;
      total = None;
      bytes = false;
      counted = None;
      rate = None;
      output = [];
      status = `Running;
      message = None;
      ended = None;
      children = [];
    }
  in
  w.frame <-
    (match frame with
    | Some frame -> frame root
    | None -> fun ~width ~height -> [ Task.rows root ~width ~height ]);
  (* What the program prints on a [`Json] output while the work runs is
     timed by the work's clock, as its tasks are. *)
  let d = out.out_device in
  let timed =
    Mutex.protect d.lock (fun () ->
        if Option.is_none d.work_time then (
          d.work_time <- Some now;
          true)
        else false)
  in
  Fun.protect ~finally:(fun () ->
      if timed then Mutex.protect d.lock (fun () -> d.work_time <- None))
  @@ fun () ->
  live ~tick:0.1 out @@ fun l ->
  Mutex.protect w.order (fun () -> w.live <- Some l);
  show l w.frame;
  let ended () = Mutex.protect w.order (fun () -> w.live <- None) in
  match f root with
  | result ->
      Task.succeed root;
      ended ();
      result
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Task.fail root;
      ended ();
      Printexc.raise_with_backtrace e bt

(* Prompts *)

module Prompt = struct
  type error = [ `Not_interactive | `End_of_input ]

  (* Setting a terminal's settings from the background would stop the
     process with SIGTTOU. *)
  let restore fd saved =
    let mask = Thread.sigmask SIG_BLOCK [ Sys.sigttou ] in
    Fun.protect
      ~finally:(fun () -> ignore (Thread.sigmask SIG_SETMASK mask))
      (fun () -> try Tty.restore fd saved with Unix.Unix_error _ -> ())

  (* The controlling terminal, for what only its user answers. *)
  let with_terminal f =
    match Unix.openfile "/dev/tty" [ O_RDWR; O_CLOEXEC ] 0 with
    | exception Unix.Unix_error _ -> Error `Not_interactive
    | fd -> Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)

  (* Runs [f] with the terminal [fd] in settings that [set] gives, put back
     on every way out, before the process stops and when it ends on a
     signal; [again] runs when it continues. *)
  let in_settings fd ~set ~again f =
    let d = device fd in
    Mutex.protect d.lock (fun () -> enter d fd);
    Fun.protect ~finally:(fun () -> leave d) @@ fun () ->
    let holder = Option.map (fun s -> s.holder) d.session in
    Option.iter (fun h -> Signal.keep_settings h ~input:fd) holder;
    let saved = set fd in
    Mutex.protect d.lock (fun () ->
        d.prompt <-
          Some
            {
              on_stop = (fun () -> restore fd saved);
              on_continue =
                (fun () ->
                  ignore (set fd : Tty.settings);
                  again ());
            });
    Fun.protect
      ~finally:(fun () ->
        Mutex.protect d.lock (fun () -> d.prompt <- None);
        restore fd saved;
        Option.iter Signal.forget_settings holder)
      f

  let rec read_line fd buf b =
    match Unix.read fd b 0 (Bytes.length b) with
    | 0 -> Error `End_of_input
    | n -> (
        Buffer.add_subbytes buf b 0 n;
        let s = Buffer.contents buf in
        match String.index_opt s '\n' with
        | Some i -> Ok (String.sub s 0 i)
        | None -> read_line fd buf b)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_line fd buf b

  (* While a prompt asks on [d], what is written there waits, so that no
     line lands inside the question or its answer, and goes out once the
     prompt ends. *)
  let holding d f =
    Mutex.protect d.lock (fun () -> d.asking <- d.asking + 1);
    Fun.protect f ~finally:(fun () ->
        Mutex.protect d.lock (fun () ->
            d.asking <- d.asking - 1;
            if d.asking = 0 then (
              let waiting = List.rev d.waiting in
              d.waiting <- [];
              List.iter (fun (t, bytes) -> emit t bytes) waiting)))

  (* Writes the question, adapted to [o]'s styling, past the lines that
     wait. *)
  let ask_on o question =
    Mutex.protect o.out_device.lock (fun () ->
        flush_std ();
        let bytes = Matrix_cli.adapt o.adapter question in
        write_all o.fd bytes 0 (String.length bytes))

  (* The question as a prompt writes it: its hint in brackets, if any, then
     a space. *)
  let with_hint question = function
    | None | Some "" -> question ^ " "
    | Some hint -> question ^ " [" ^ hint ^ "] "

  let ask ?color question =
    if not (Tty.is_tty Unix.stdin) then Error `Not_interactive
    else
      let o = stderr ?color () in
      holding o.out_device @@ fun () ->
      off_screen (device Unix.stdin) @@ fun () ->
      ask_on o question;
      match In_channel.input_line Stdlib.stdin with
      | Some l -> Ok l
      | None -> Error `End_of_input

  let line ?color ?default question =
    match (ask ?color (with_hint question default), default) with
    | (Error `Not_interactive | Ok ""), Some d -> Ok d
    | answer, _ -> answer

  (* An affirmative answer starts with y or Y, a negative one with n or N,
     as the POSIX locale's yesexpr and noexpr have them ("^[yY]",
     "^[nN]"). *)
  let confirm ?color ?default question =
    let hint =
      match default with
      | None -> "y/n"
      | Some true -> "Y/n"
      | Some false -> "y/N"
    in
    let rec go () =
      match ask ?color (with_hint question (Some hint)) with
      | Error `Not_interactive when Option.is_some default ->
          Ok (Option.get default)
      | Error e -> Error e
      | Ok a -> (
          match (String.trim a, default) with
          | "", Some d -> Ok d
          | a, _ when a <> "" && (a.[0] = 'y' || a.[0] = 'Y') -> Ok true
          | a, _ when a <> "" && (a.[0] = 'n' || a.[0] = 'N') -> Ok false
          | _ -> go ())
    in
    go ()

  (* The events read at [fd], decoded, as a sequence read once. *)
  let read_events fd =
    let module D = Matrix_input.Decoder in
    let d = D.create () and queue = Queue.create () in
    let buf = Bytes.create 256 in
    let push e = Queue.push e queue in
    let rec next () =
      match Queue.take_opt queue with
      | Some e -> Seq.Cons (e, next)
      | None -> (
          let timeout = if D.pending d = D.Nothing then -1. else 0.05 in
          match Unix.select [ fd ] [] [] timeout with
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> next ()
          | [], _, _ ->
              D.idle d push;
              next ()
          | _ -> (
              match Unix.read fd buf 0 (Bytes.length buf) with
              | 0 ->
                  D.finish d push;
                  if Queue.is_empty queue then Seq.Nil else next ()
              | n ->
                  D.feed d buf ~off:0 ~len:n push;
                  next ()
              | exception Unix.Unix_error (Unix.EINTR, _, _) -> next ()))
    in
    next

  (* The characters of [s], one string each. *)
  let chars s =
    let rec go i acc =
      if i >= String.length s then List.rev acc
      else
        let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
        go (i + n) (String.sub s i n :: acc)
    in
    go 0 []

  (* A line typed with a star shown for each character, one for each code
     point as sudo's pwfeedback counts them, and for each ill-formed byte,
     kept as typed so that a masked and an unmasked read agree. The keys
     are decoded, so a key's sequence types nothing, and the text of a key,
     Alt's included, of an input method and of a paste counts; a line end
     in a paste ends the line. The terminal's erase character and Backspace
     erase the last character, its kill character every one, and Ctrl+W
     the last word; its end-of-file character ends the input on an empty
     line. A character the terminal disabled is NUL, as Linux's
     _POSIX_VDISABLE is, and matches no key. Erasing moves the cursor back
     by rows, over the rows the question and the stars take at the
     terminal's width, so it works where they wrap, which one BS a star,
     as sudo erases them, does not. [shown] is the stars on the
     terminal. *)
  let masked fd o ~question ~width ~typed ~shown events =
    let tio = Unix.tcgetattr fd in
    let is c k = c <> '\000' && c = k in
    let write s = write_all fd s 0 (String.length s) in
    let cols () =
      match Tty.window fd with Some (c, _) when c > 0 -> c | _ -> 80
    in
    (* The rows above the cursor that the question and [n] stars take. *)
    let above n =
      let w = width + n in
      if w = 0 then 0
      else if o.caps.eager_wrap then w / cols ()
      else (w - 1) / cols ()
    in
    let redraw n =
      let up = above !shown in
      write
        ("\r"
        ^ (if up > 0 then Printf.sprintf "\027[%dA" up else "")
        ^ "\027[J" ^ question ^ String.make n '*');
      shown := n
    in
    let add c =
      typed := c :: !typed;
      write "*";
      incr shown
    in
    let erase () =
      match !typed with
      | [] -> ()
      | _ :: rest ->
          typed := rest;
          redraw (List.length rest)
    in
    let rec erase_word blank =
      match !typed with
      | c :: rest when (c = " " || c = "\t") = blank ->
          typed := rest;
          erase_word blank
      | _ -> if blank then erase_word false
    in
    let line () =
      redraw 0;
      Ok (String.concat "" (List.rev !typed))
    in
    let control (k : Matrix_input.Keystroke.t) =
      match k.key with
      | Backspace -> Some '\127'
      | Enter -> Some '\r'
      | Char u when k.mods.ctrl ->
          let c = Uchar.to_int u in
          if c >= Char.code 'a' && c <= Char.code 'z' then
            Some (Char.chr (c - Char.code 'a' + 1))
          else None
      | _ -> None
    in
    let rec go events =
      match events () with
      | Seq.Nil ->
          redraw 0;
          Error `End_of_input
      | Seq.Cons (e, events) -> (
          match (e : Matrix_input.Decoder.event) with
          | Key k -> (
              match control k with
              | Some ('\r' | '\n') -> line ()
              | Some c when c = '\127' || c = '\b' || is tio.c_verase c ->
                  erase ();
                  go events
              | Some c when is tio.c_vkill c ->
                  typed := [];
                  redraw 0;
                  go events
              | Some '\023' ->
                  erase_word true;
                  redraw (List.length !typed);
                  go events
              | Some c when is tio.c_veof c ->
                  if !typed = [] then (
                    redraw 0;
                    Error `End_of_input)
                  else go events
              | Some _ -> go events
              | None ->
                  (* An Alt-modified key types its character too. *)
                  (match (k.text, k.key) with
                  | "", Char u ->
                      let b = Buffer.create 4 in
                      Buffer.add_utf_8_uchar b u;
                      add (Buffer.contents b)
                  | text, _ -> List.iter add (chars text));
                  go events)
          | Text s ->
              List.iter add (chars s);
              go events
          | Invalid s ->
              add s;
              go events
          | Paste_data s -> (
              match
                (String.index_from_opt s 0 '\n', String.index_from_opt s 0 '\r')
              with
              | None, None ->
                  List.iter add (chars s);
                  go events
              | a, b ->
                  let i =
                    min
                      (Option.value a ~default:max_int)
                      (Option.value b ~default:max_int)
                  in
                  List.iter add (chars (String.sub s 0 i));
                  line ())
          | _ -> go events)
    in
    go events

  let ask_secret ?color ~mask question =
    with_terminal @@ fun fd ->
    let o = make ?color fd in
    let question = question ^ " " in
    let bytes = Matrix_cli.adapt o.adapter question in
    let ask () = write_all fd bytes 0 (String.length bytes) in
    holding o.out_device @@ fun () ->
    off_screen (device fd) @@ fun () ->
    let answer =
      if mask && not o.caps.dumb then
        let typed = ref [] and shown = ref 0 in
        let width =
          Matrix_text.measure ~width_method:o.caps.unicode_width ~tab_width:8
            (Ansi.sanitize question)
        in
        (* What was typed before the question is discarded, as [Tty.no_echo]
           discards it. *)
        in_settings fd
          ~set:(fun fd ->
            Unix.tcflush fd Unix.TCIFLUSH;
            Tty.keys fd)
          ~again:(fun () ->
            write_all fd "\r\n" 0 2;
            ask ();
            shown := List.length !typed;
            write_all fd (String.make !shown '*') 0 !shown)
          (fun () ->
            ask ();
            masked fd o ~question:bytes ~width ~typed ~shown (read_events fd))
      else
        (* The echo goes off, and what was typed before is discarded, before
           the question shows, so that nothing typed once it shows is echoed
           or discarded. *)
        in_settings fd ~set:Tty.no_echo
          ~again:(fun () ->
            write_all fd "\r\n" 0 2;
            ask ())
          (fun () ->
            ask ();
            read_line fd (Buffer.create 64) (Bytes.create 256))
    in
    write_all fd "\r\n" 0 2;
    answer

  let secret ?color ?default ?(mask = false) question =
    match (ask_secret ?color ~mask question, default) with
    | (Error `Not_interactive | Ok ""), Some d -> Ok d
    | answer, _ -> answer

  (* A menu drawn in a region on the controlling terminal, the terminal its
     keys are typed at, made to redraw there whatever CI or the program's
     --progress choice say. *)
  let choose ?color ?default question items =
    let items = Array.of_list items in
    let n = Array.length items in
    if n = 0 then invalid_arg "Matrix_cli_unix.Prompt.choose: no item";
    (match default with
    | Some d when d < 0 || d >= n ->
        invalid_arg "Matrix_cli_unix.Prompt.choose: default out of range"
    | _ -> ());
    let accent = Ansi.Style.make ~fg:Ansi.Color.cyan ~bold:true ()
    and faint = Ansi.Style.make ~dim:true () in
    let menu selected ~width ~height:_ =
      let g = Matrix_grid.create ~width:(max 1 width) ~height:(n + 1) () in
      let draw y spans =
        ignore (Matrix_grid.draw_spans g ~x:0 ~y spans : int)
      in
      draw 0
        [
          Ansi.span question;
          Ansi.span ~style:faint "  (\u{2191}\u{2193} to move, Enter to pick)";
        ];
      Array.iteri
        (fun i item ->
          draw (i + 1)
            (if i = selected then
               [ Ansi.span ~style:accent ("\u{276F} " ^ item) ]
             else [ Ansi.span ("  " ^ item) ]))
        items;
      [ g ]
    in
    let answered i ~width ~height:_ =
      let g = Matrix_grid.create ~width:(max 1 width) ~height:1 () in
      ignore
        (Matrix_grid.draw_spans g ~x:0 ~y:0
           [
             Ansi.span question;
             Ansi.span " ";
             Ansi.span ~style:accent items.(i);
           ]
          : int);
      [ g ]
    in
    let asked =
      with_terminal @@ fun fd ->
      let o = make ?color ~progress:`Tty fd in
      live o @@ fun l ->
      in_settings fd ~set:Tty.keys ~again:ignore @@ fun () ->
      let rec pick i events =
        show l (menu i);
        match events () with
        | Seq.Nil -> Error `End_of_input
        | Seq.Cons (Matrix_input.Decoder.Key k, events) -> (
            let plain = k.mods = Matrix_input.Keystroke.no_mods in
            match k.key with
            | Up -> pick (max 0 (i - 1)) events
            | Down -> pick (min (n - 1) (i + 1)) events
            | Char u when plain && Uchar.to_int u = Char.code 'k' ->
                pick (max 0 (i - 1)) events
            | Char u when plain && Uchar.to_int u = Char.code 'j' ->
                pick (min (n - 1) (i + 1)) events
            | Enter -> Ok i
            | Escape when Option.is_some default -> Ok (Option.get default)
            | _ -> pick i events)
        | Seq.Cons (_, events) -> pick i events
      in
      let answer = pick (Option.value default ~default:0) (read_events fd) in
      show l
        (match answer with
        | Ok i -> answered i
        | Error _ -> fun ~width:_ ~height:_ -> []);
      answer
    in
    match (asked, default) with
    | Error `Not_interactive, Some d -> Ok d
    | answer, _ -> answer

  let keys f =
    with_terminal @@ fun fd ->
    in_settings fd ~set:Tty.keys ~again:ignore @@ fun () ->
    Ok
      (f
         (Seq.filter_map
            (function Matrix_input.Decoder.Key k -> Some k | _ -> None)
            (read_events fd)))
end
