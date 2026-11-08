(* The child of a {!spawn} handle. [Running] is a child the handle has not
   seen reaped: [keeper] is the process it reaps, which leads the program's
   session ({!spawn}), [program] is the program's process id and process
   group, which it signals, and [status] is the pipe on which the keeper
   writes the program's status. Until the keeper is reaped, its id names it
   and no other process. The program's names the program and its group
   while the keeper lives, which leaves it unreaped until the end: so
   [signal] asks whether the keeper has exited just before it signals, and
   what remains is a keeper that reaps the program between the two, and a
   program id given out again as a process group's in those microseconds.
   [Exited status] is a child the handle reaped while checking on it, whose
   status {!wait} returns. *)
type child =
  | No_child
  | Running of { keeper : int; program : int; status : Unix.file_descr }
  | Exited of Unix.process_status

type t = {
  fd : Unix.file_descr;
  mutable child : child;
  (* [close] must be a true no-op the second time: the kernel can reallocate
     the fd number to anything else in the process, and a second [Unix.close]
     would then close that unrelated descriptor. *)
  mutable closed : bool;
}

type winsize = { rows : int; cols : int; xpixel : int; ypixel : int }

(* External C functions *)
external open_pty_raw : unit -> Unix.file_descr * Unix.file_descr
  = "ocaml_pty_open"

external get_winsize_raw : Unix.file_descr -> winsize = "ocaml_pty_get_winsize"

external set_winsize_raw : Unix.file_descr -> winsize -> unit
  = "ocaml_pty_set_winsize_byte" "ocaml_pty_set_winsize"

external fork_exec :
  Unix.file_descr ->
  string array ->
  string array ->
  string array ->
  string option ->
  int * int * Unix.file_descr = "ocaml_pty_fork_exec"

external read_status : Unix.file_descr -> Unix.process_status option
  = "ocaml_pty_read_status"

let file_descr t = t.fd
let in_fd t = t.fd
let out_fd t = t.fd
let pid t = match t.child with Running { program; _ } -> Some program | _ -> None

(* Every fd-consuming operation goes through this guard so a closed handle
   raises EBADF instead of touching a possibly-reused fd number. *)
let live_fd t op =
  if t.closed then raise (Unix.Unix_error (Unix.EBADF, op, ""));
  t.fd

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* The status of the program of a keeper that has exited with
   [keeper_status]: the one the keeper wrote, or the keeper's own if it was
   killed before the program exited. *)
let program_status status keeper_status =
  let program = read_status status in
  close_fd status;
  Option.value program ~default:keeper_status

(* Asks the system, without blocking, what became of a running child. One
   that has exited is reaped here and its status kept. ECHILD means the
   calling program reaped the keeper itself, with [Unix.wait], and its id may
   now name another process, so the handle forgets it. *)
let rec check t =
  match t.child with
  | No_child | Exited _ -> ()
  | Running { keeper; status; _ } -> (
      match Unix.waitpid [ WNOHANG ] keeper with
      | 0, _ -> ()
      | _, keeper_status ->
          t.child <- Exited (program_status status keeper_status)
      | exception Unix.Unix_error (EINTR, _, _) -> check t
      | exception Unix.Unix_error (ECHILD, _, _) ->
          close_fd status;
          t.child <- No_child)

(* Sends [signal] to the program's process group if it is still running. *)
let signal t signal =
  check t;
  match t.child with
  | Running { program; _ } -> (
      try Unix.kill (-program) signal with Unix.Unix_error _ -> ())
  | No_child | Exited _ -> ()

let terminate t =
  match t.child with
  | No_child -> invalid_arg "Pty.terminate: no child process"
  | Running _ | Exited _ -> signal t Sys.sigterm

let kill t =
  match t.child with
  | No_child -> invalid_arg "Pty.kill: no child process"
  | Running _ | Exited _ -> signal t Sys.sigkill

(* Reaps the child with [waitpid flags], where [WNOHANG] gives [None] while
   it runs. *)
let rec reap name t flags =
  match t.child with
  | No_child -> invalid_arg (name ^ ": no child process")
  | Exited status ->
      t.child <- No_child;
      Some status
  | Running { keeper; status; _ } -> (
      match Unix.waitpid flags keeper with
      | 0, _ -> None
      | _, keeper_status ->
          t.child <- No_child;
          Some (program_status status keeper_status)
      | exception Unix.Unix_error (EINTR, _, _) -> reap name t flags
      | exception (Unix.Unix_error (ECHILD, _, _) as e) ->
          close_fd status;
          t.child <- No_child;
          raise e)

let wait t =
  match reap "Pty.wait" t [] with Some status -> status | None -> assert false

let poll t = reap "Pty.poll" t [ WNOHANG ]

(* After close's hangup, the child has about 100 ms to exit, polled
   without blocking, before SIGKILL. *)
let rec grace t attempts =
  check t;
  match t.child with
  | Running { program; _ } when attempts <= 0 -> (
      try Unix.kill (-program) Sys.sigkill with Unix.Unix_error _ -> ())
  | Running _ ->
      Unix.sleepf 0.01;
      grace t (attempts - 1)
  | No_child | Exited _ -> ()

let close ?wait:(reaps = true) t =
  if t.closed then ()
  else (
    t.closed <- true;
    (* Hang up: the program receives SIGHUP, with time to act on it, as a
       SIGTERM right behind it would end a shell before its trap for SIGHUP
       ran, and the keeper's drain ends, so the reap below does not wait for
       the output to be read. The keeper is told too, as its own unreaped
       child: when another process holds the master, closing it here hangs
       nothing up. *)
    close_fd t.fd;
    check t;
    (match t.child with
    | Running { keeper; _ } -> (
        try Unix.kill keeper Sys.sighup with Unix.Unix_error _ -> ())
    | No_child | Exited _ -> ());
    grace t 10;
    match t.child with
    | No_child -> ()
    | Running _ when not reaps -> ()
    | Running _ | Exited _ -> (
        match reap "Pty.close" t [] with
        | Some status -> t.child <- Exited status
        | None -> ()
        | exception Unix.Unix_error _ -> ()))

let get_winsize t = get_winsize_raw (live_fd t "get_winsize")
let set_winsize t ws = set_winsize_raw (live_fd t "set_winsize") ws

let resize t ~rows ~cols =
  let ws = { rows; cols; xpixel = 0; ypixel = 0 } in
  set_winsize t ws

let inherit_size ~src ~dst =
  let ws = get_winsize src in
  set_winsize dst ws

let open_pty ?winsize () =
  let master_fd, slave_fd = open_pty_raw () in
  let master = { fd = master_fd; child = No_child; closed = false } in
  let slave = { fd = slave_fd; child = No_child; closed = false } in
  (* Set initial window size if provided *)
  (match winsize with
  | Some ws -> (
      try set_winsize slave ws
      with e ->
        close master;
        close slave;
        raise e)
  | None -> ());
  (master, slave)

(* Where [execvp] looks for [prog], in order: [prog] itself when it names a
   path, else [prog] in each directory of the [PATH] of [env], the program's
   environment, where an empty directory is the current one. *)
let search_path env prog =
  if String.contains prog '/' then [| prog |]
  else
    let path =
      Array.fold_left
        (fun path var ->
          match path with
          | Some _ -> path
          | None when String.starts_with ~prefix:"PATH=" var ->
              Some (String.sub var 5 (String.length var - 5))
          | None -> None)
        None env
      |> Option.value ~default:"/bin:/usr/bin"
    in
    String.split_on_char ':' path
    |> List.map (fun dir -> if dir = "" then prog else Filename.concat dir prog)
    |> Array.of_list

let spawn ?env ?cwd ?winsize ~prog ~args () =
  let master, slave = open_pty ?winsize () in
  let env = match env with Some env -> env | None -> Unix.environment () in
  let argv = Array.of_list (prog :: args) in
  match fork_exec slave.fd (search_path env prog) argv env cwd with
  | keeper, program, status ->
      close slave;
      { master with child = Running { keeper; program; status } }
  | exception e ->
      close master;
      close slave;
      raise e

let with_pty ?winsize f =
  let master, slave = open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      (try close master with Unix.Unix_error _ -> ());
      try close slave with Unix.Unix_error _ -> ())
    (fun () -> f master slave)

let with_spawn ?env ?cwd ?winsize ~prog ~args f =
  let pty = spawn ?env ?cwd ?winsize ~prog ~args () in
  Fun.protect
    ~finally:(fun () -> try close pty with Unix.Unix_error _ -> ())
    (fun () -> f pty)

(* I/O operations *)
let read t buf ofs len = Unix.read (live_fd t "read") buf ofs len
let write t buf ofs len = Unix.write (live_fd t "write") buf ofs len

let write_string t str ofs len =
  Unix.write_substring (live_fd t "write") str ofs len

(* Non-blocking mode *)
let set_nonblock t = Unix.set_nonblock (live_fd t "set_nonblock")
let clear_nonblock t = Unix.clear_nonblock (live_fd t "clear_nonblock")
