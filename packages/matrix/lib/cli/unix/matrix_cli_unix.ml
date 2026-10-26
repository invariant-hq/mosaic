module Policy = Matrix_cli.Policy
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
}

(* The signals of a terminal, while a region or a prompt uses it: a holder,
   with a descriptor of its own for the bytes an abort writes, and the
   thread that acts on the signals and draws the frames that wait. *)
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
  fd : Unix.file_descr;
  region : Region.t;
  mutable frame : width:int -> Matrix_grid.t option;
  mutable cached : (int * Matrix_grid.t option) option;
  mutable failed : (exn * Printexc.raw_backtrace) option;
  mutable on_screen : bool;
  mutable suspended : int;
  mutable held : string;  (** A line printed above without its end. *)
  mutable last_draw : int;
  mutable pending : bool;
  mutable published : string;  (** The teardown of the last whole write. *)
  mutable closed : bool;  (** Its scope has ended. *)
}

and prompt = { on_stop : unit -> unit; on_continue : unit -> unit }

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

let window l = Tty.window l.fd

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
      Signal.write s.holder l.fd b 0 (Bytes.length b)
  | None -> write_all l.fd bytes 0 (String.length bytes)

(* The frame at [width], from the program's function, which runs on
   whichever thread draws: one that raises is dropped, and raised again at
   the program's next call. *)
let frame_at l ~width =
  match l.cached with
  | Some (w, g) when w = width -> g
  | _ when width < 1 -> None
  | _ ->
      let g =
        match l.frame ~width with
        | g -> g
        | exception e ->
            if l.failed = None then
              l.failed <- Some (e, Printexc.get_raw_backtrace ());
            None
      in
      l.cached <- Some (width, g);
      g

let reraise l =
  match l.failed with
  | Some (e, bt) ->
      l.failed <- None;
      Printexc.raise_with_backtrace e bt
  | None -> ()

(* One write with [above], the complete lines printed since the last, then
   the frame; called with the device's lock held. A region that cannot be
   drawn now, while it is suspended, while the job is in the background or
   on a terminal of no size, lets the lines go out as they are. *)
let draw l ~above =
  flush_std ();
  l.pending <- false;
  let size = window l in
  match size with
  | Some (cols, rows) when l.suspended = 0 && not (Tty.background l.fd) ->
      let frame = frame_at l ~width:(Region.width l.region ~cols) in
      let bytes = Region.draw l.region ~cols ~rows ~above frame in
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
        write_until l.fd l.published (Tty.now () + teardown_wait)
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
let stop d session =
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

(* The session's thread. It waits on the holder's wake pipe until a signal
   or a frame that waits for its time, and never waits for the device's
   lock before acting on a signal that ends the process: the work that
   needs the lock is [owed] until it can be taken. The fields it reads
   without the lock only time its wait. *)
let rec watch d session ~continue ~redraw =
  let wake = Signal.wake_fd session.holder in
  let timeout =
    if continue || redraw then 0.01
    else
      match d.live with
      | Some l when l.pending ->
          Float.of_int (max 0 (l.last_draw + interval - Tty.now ())) *. 1e-9
      | _ -> -1.
  in
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
      if Signal.received h Sys.sigtstp then not (stop d session) else continue
    in
    let continue = Signal.received h Sys.sigcont || continue in
    let redraw = Signal.received h Sys.sigwinch || redraw in
    let due =
      match d.live with
      | Some l -> l.pending && Tty.now () >= l.last_draw + interval
      | None -> false
    in
    if (continue || redraw || due) && Mutex.try_lock d.lock then (
      Fun.protect
        ~finally:(fun () -> Mutex.unlock d.lock)
        (fun () ->
          if continue then continued d
          else
            match d.live with
            | Some l when redraw || l.pending ->
                if Tty.now () >= l.last_draw + interval then draw l ~above:""
                else l.pending <- true
            | _ -> ());
      watch d session ~continue:false ~redraw:false)
    else watch d session ~continue ~redraw:(redraw || due))

let exit_restores = Atomic.make false

(* A use of the device's signals begins: the first takes them, for the
   controlling terminal its job-control and window signals too. *)
let enter d fd =
  d.users <- d.users + 1;
  if d.session = None && not Sys.win32 then (
    if not (Atomic.exchange exit_restores true) then at_exit Signal.restore_all;
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

type t = {
  fd : Unix.file_descr;
  tty : bool;
  caps : Caps.t;
  styling : Matrix_cli.styling;
  live_ok : bool;
  columns : int option;
  adapter : Matrix_cli.adapter;
  out_device : device;
}

(* The Windows consoles whose VT processing an output turned on, which the
   exit path turns back off: the console is the shell's too. *)
let consoles = ref []

let make ?color fd =
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
  let styling = Policy.styling ?color getenv ~tty caps in
  {
    fd;
    tty;
    caps;
    styling;
    live_ok = Policy.live getenv ~tty caps;
    columns;
    adapter = Matrix_cli.adapter styling;
    out_device = device fd;
  }

let stdout ?color () = make ?color Unix.stdout
let stderr ?color () = make ?color Unix.stderr
let styling t = t.styling

let width t =
  match t.columns with
  | Some _ as w -> w
  | None -> Option.map fst (Tty.window t.fd)

(* Writes bytes already adapted, above the region of the device if one is
   on the screen; called with the device's lock held. *)
let emit t bytes =
  flush_std ();
  match t.out_device.live with
  | Some l when l.on_screen && l.suspended = 0 -> above l bytes
  | _ -> write_all t.fd bytes 0 (String.length bytes)

let print t s =
  Mutex.protect t.out_device.lock (fun () ->
      let bytes = Matrix_cli.adapt t.adapter s in
      if bytes <> "" then emit t bytes)

let out_functions t =
  let pending = Buffer.create 256 in
  let out_string s off len = Buffer.add_substring pending s off len in
  let out_flush () =
    let s = Buffer.contents pending in
    Buffer.clear pending;
    print t s
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
  mutable last : width:int -> Matrix_grid.t option;
}

let no_frame ~width:_ = None

let start t =
  let d = t.out_device in
  Mutex.protect d.lock (fun () ->
      if Option.is_some d.live then
        invalid_arg "Matrix_cli_unix.live: a region lives on this terminal";
      let l =
        {
          device = d;
          fd = t.fd;
          region =
            Region.create ~eager_wrap:t.caps.eager_wrap
              ~lifts_resting_cursor:t.caps.lifts_resting_cursor
              ~styling:t.styling ();
          frame = no_frame;
          cached = None;
          failed = None;
          on_screen = false;
          suspended = 0;
          held = "";
          last_draw = 0;
          pending = false;
          published = "";
          closed = false;
        }
      in
      enter d t.fd;
      d.live <- Some l;
      l)

(* The rows of the last frame as lines, for an output where no region is
   drawn. *)
let frame_lines t frame =
  match frame ~width:(max 1 (Option.value (width t) ~default:80)) with
  | Some g -> Matrix_grid.to_ansi ~styling:t.styling ~trim:true g ^ "\n"
  | None -> ""

let finish t l =
  let d = l.device in
  Mutex.protect d.lock (fun () ->
      d.live <- None;
      l.closed <- true;
      flush_std ();
      let above = if l.held = "" then "" else l.held ^ "\n" in
      l.held <- "";
      (match window l with
      | Some (cols, rows) when l.suspended = 0 && not (Tty.background l.fd) ->
          l.cached <- None;
          let frame = frame_at l ~width:(Region.width l.region ~cols) in
          write l (Region.close l.region ~cols ~rows ~above frame)
      | _ -> write l (above ^ frame_lines t l.frame));
      Option.iter (fun s -> Signal.set_reset s.holder "") d.session);
  leave d

let live t f =
  match if t.live_ok then Some (start t) else None with
  | None ->
      let v = { state = None; last = no_frame } in
      let result = f v in
      Mutex.protect t.out_device.lock (fun () ->
          let lines = frame_lines t v.last in
          if lines <> "" then emit t lines);
      result
  | Some l -> (
      match f { state = Some l; last = no_frame } with
      | result ->
          finish t l;
          reraise l;
          result
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          (try finish t l with _ -> ());
          Printexc.raise_with_backtrace e bt)

let show v frame =
  v.last <- frame;
  match v.state with
  | None -> ()
  | Some l ->
      Mutex.protect l.device.lock (fun () ->
          if l.closed then
            invalid_arg "Matrix_cli_unix.show: the region's scope has ended";
          reraise l;
          l.frame <- frame;
          l.cached <- None;
          if Tty.now () >= l.last_draw + interval then draw l ~above:""
          else if not l.pending then (
            l.pending <- true;
            Option.iter (fun s -> Signal.wake s.holder) l.device.session))

(* Runs [f] with the region of [d], if one lives, off the screen. *)
let off_screen d f =
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

  let line question =
    if not (Tty.is_tty Unix.stdin) then Error `Not_interactive
    else
      off_screen (device Unix.stdin) @@ fun () ->
      print (stderr ()) question;
      match In_channel.input_line Stdlib.stdin with
      | Some l -> Ok l
      | None -> Error `End_of_input

  let secret question =
    with_terminal @@ fun fd ->
    let ask () = write_all fd question 0 (String.length question) in
    off_screen (device fd) @@ fun () ->
    (* The echo goes off, and what was typed before is discarded, before the
       question shows, so that nothing typed once it shows is echoed or
       discarded. *)
    let answer =
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

  let keys f =
    with_terminal @@ fun fd ->
    in_settings fd ~set:Tty.keys ~again:ignore @@ fun () ->
    let module D = Matrix_input.Decoder in
    let d = D.create () and queue = Queue.create () in
    let buf = Bytes.create 256 in
    let push = function D.Key k -> Queue.push k queue | _ -> () in
    let rec next () =
      match Queue.take_opt queue with
      | Some k -> Seq.Cons (k, next)
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
    Ok (f next)
end
