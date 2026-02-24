(** Pseudo-terminals.

    This module creates and manages pseudo-terminals (PTYs) for terminal
    emulation. A PTY is a bidirectional channel with a {e master} side (used by
    the terminal emulator to send input and receive output) and a {e slave} side
    (connected to a child process as its controlling terminal).

    {b Warning.} {!spawn} and {!with_spawn} use [Unix.fork] and are only
    available on POSIX systems (Linux, macOS, BSD). {!open_pty} works on Windows
    10 1809+ via ConPTY with {{!platform}limitations}.

    {1:creating Creating}

    The simplest way to run a program in a PTY is {!with_spawn}:
    {[
      Pty.with_spawn ~prog:"/bin/echo" ~args:[ "hello" ] (fun pty ->
          let buf = Bytes.create 1024 in
          let n = Pty.read pty buf 0 1024 in
          Bytes.sub_string buf 0 n)
    ]}

    For manual lifetime control use {!spawn} and {!close}:
    {[
      let pty = Pty.spawn ~prog:"/bin/bash" ~args:[] () in
      (* … interact … *)
      Pty.close pty
    ]}

    {1:io I/O}

    {!read} and {!write} are blocking by default. Enable non-blocking mode with
    {!set_nonblock}; I/O then raises [Unix.Unix_error (EAGAIN, _, _)] instead of
    blocking.

    The file descriptor returned by {!file_descr} works with [Unix.select],
    [Unix.poll], and async wrappers such as [Lwt_unix.of_unix_file_descr].

    {1:winsize Window size}

    {!set_winsize} (or the convenience {!resize}) updates the PTY dimensions and
    delivers [SIGWINCH] to the child process group on POSIX systems.
    {!inherit_size} copies the size from one PTY to another, useful for
    forwarding resize events.

    {1:platform Platform notes}

    {2 POSIX (Linux, macOS, BSD)}

    Full support via [posix_openpt], [grantpt], [unlockpt]. File descriptors are
    real Unix file descriptors; [Unix.select] and [Unix.poll] work as expected.
    [SIGWINCH] is delivered to child processes on resize.

    {2 Windows (ConPTY)}

    Supported on Windows 10 version 1809 and later. Current limitations:
    - {!spawn} is not available (requires [CreateProcess] integration).
    - {!get_winsize} returns the last size set via {!set_winsize} or the initial
      default, as ConPTY has no query API.
    - I/O uses Windows named pipes wrapped as Unix file descriptors.

    {1:thread_safety Thread safety}

    This module is {b not} thread-safe. The {!t} type contains mutable state
    that must not be accessed concurrently without external synchronization.
    However, C stubs release the OCaml runtime lock during blocking I/O so other
    OCaml threads can run concurrently.

    {1:resources Resource management}

    Each {!t} owns a file descriptor and possibly a child process. Always call
    {!close} or use {!with_pty}/{!with_spawn} to prevent resource leaks and
    zombie processes. {!close} is idempotent. *)

(** {1:types Types} *)

type t
(** The type for pseudo-terminal handles.

    A handle owns a file descriptor and possibly a child process (for PTYs
    created by {!spawn}). After {!close} the handle is invalidated; further I/O
    raises [Unix.Unix_error (EBADF, _, _)]. *)

type winsize = {
  rows : int;  (** Row count (lines). *)
  cols : int;  (** Column count (characters per line). *)
  xpixel : int;  (** Width in pixels. Often unused; set to [0]. *)
  ypixel : int;  (** Height in pixels. Often unused; set to [0]. *)
}
(** The type for terminal window sizes. All fields are non-negative. *)

(** {1:creating Creating} *)

val open_pty : ?winsize:winsize -> unit -> t * t
(** [open_pty ()] is [(master, slave)], a fresh PTY pair.

    Both handles must be closed with {!close}. The slave should be connected to
    a child process via [Unix.dup2].

    [winsize] sets the initial terminal size. Omit to use the system default.

    Raises [Unix.Unix_error] if PTY creation fails. *)

val spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  unit ->
  t
(** [spawn ~prog ~args ()] is the master handle of a new PTY with [prog] running
    in it.

    Creates a PTY pair, forks, configures the child with the slave as its
    controlling terminal ([setsid] + [TIOCSCTTY]), redirects the child's
    stdin/stdout/stderr to the slave, and executes [prog]. The slave is closed
    in the parent. The child's PID is available via {!pid}.

    Optional arguments:
    - [env] is the child environment as ["KEY=value"] strings. Defaults to the
      parent environment.
    - [cwd] is the child working directory. Defaults to the parent's.
    - [winsize] is the initial terminal size. Defaults to the system default.

    [args] are the command-line arguments {e excluding} [argv[0]] which is set
    to [prog]. If [prog] is relative it is searched in [PATH].

    Raises [Unix.Unix_error] if PTY creation or [fork] fails. Exec failures in
    the child are not reported as exceptions; the child exits with the errno
    value as its exit code. Monitor via [Unix.waitpid] on {!pid}.

    {b Note.} The child receives [SIGHUP] when the master is closed. *)

val with_pty : ?winsize:winsize -> (t -> t -> 'a) -> 'a
(** [with_pty f] is [f master slave] on a fresh PTY pair. Both handles are
    closed via [Fun.protect] when [f] returns or raises.

    [winsize] sets the initial terminal size. Raises [Unix.Unix_error] if PTY
    creation fails. *)

val with_spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  (t -> 'a) ->
  'a
(** [with_spawn ~prog ~args f] is [f pty] where [pty] is a {!spawn} handle. The
    PTY is closed and the child reaped via [Fun.protect] when [f] returns or
    raises.

    See {!spawn} for the meaning of optional arguments and error conditions. *)

(** {1:process Process management} *)

val pid : t -> int option
(** [pid pty] is [Some pid] for PTYs created by {!spawn} (until {!close} reaps
    the child), [None] otherwise. *)

val close : ?wait:bool -> t -> unit
(** [close pty] closes the file descriptor and, for {!spawn} PTYs, terminates
    and reaps the child process:
    - Sends [SIGTERM].
    - If [wait] is [true] (the default), sleeps 100ms, checks [waitpid WNOHANG],
      and escalates to [SIGKILL] if the child is still running.
    - Reaps with [waitpid] to prevent zombies.

    [wait] defaults to [true]. Set to [false] for non-blocking close; you must
    then call [Unix.waitpid] yourself.

    Idempotent: subsequent calls are no-ops. Never raises. After {!close}, I/O
    operations raise [Unix.Unix_error (EBADF, _, _)]. *)

val terminate : t -> unit
(** [terminate pty] sends [SIGTERM] to the child without closing the PTY. Unix
    errors from [kill] are silently ignored.

    Raises [Invalid_argument] if [pty] has no child (created via {!open_pty} or
    already closed). *)

val kill : t -> unit
(** [kill pty] sends [SIGKILL] to the child. Unlike {!terminate} this signal
    cannot be caught. Unix errors from [kill] are silently ignored.

    Raises [Invalid_argument] if [pty] has no child. *)

(** {1:fd File descriptors} *)

val file_descr : t -> Unix.file_descr
(** [file_descr pty] is the underlying Unix file descriptor. Invalid after
    {!close}. *)

val in_fd : t -> Unix.file_descr
(** [in_fd pty] is {!file_descr}[ pty]. PTYs are bidirectional so the read and
    write descriptors are the same. *)

val out_fd : t -> Unix.file_descr
(** [out_fd pty] is {!file_descr}[ pty]. *)

(** {1:winsize_ops Window size} *)

val get_winsize : t -> winsize
(** [get_winsize pty] is the current terminal size.

    On Windows (ConPTY) returns the last size set via {!set_winsize} or the
    initial default, as ConPTY has no query API.

    Raises [Unix.Unix_error] on failure. *)

val set_winsize : t -> winsize -> unit
(** [set_winsize pty ws] sets the terminal size to [ws] and delivers [SIGWINCH]
    to the child process group on POSIX.

    Raises [Unix.Unix_error] on failure. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize pty ~rows ~cols] is
    {!set_winsize}[ pty { rows; cols; xpixel = 0; ypixel = 0 }]. *)

val inherit_size : src:t -> dst:t -> unit
(** [inherit_size ~src ~dst] copies the window size from [src] to [dst].
    Equivalent to [set_winsize dst (get_winsize src)].

    Raises [Unix.Unix_error] if reading [src] or writing [dst] fails. *)

(** {1:io I/O} *)

val read : t -> bytes -> int -> int -> int
(** [read pty buf off len] reads up to [len] bytes into [buf] starting at [off].
    Returns the number of bytes read, or [0] on EOF (child exited).

    Blocks unless non-blocking mode is enabled via {!set_nonblock}, in which
    case raises [Unix.Unix_error (EAGAIN, _, _)] when no data is ready.

    Raises [Unix.Unix_error] on error. *)

val write : t -> bytes -> int -> int -> int
(** [write pty buf off len] writes up to [len] bytes from [buf] starting at
    [off]. Returns the number of bytes written.

    Blocks unless non-blocking mode is enabled.

    Raises [Unix.Unix_error] on error (e.g. [EPIPE] if the child closed its
    end). *)

val write_string : t -> string -> int -> int -> int
(** [write_string pty s off len] is like {!write} but reads from a string. *)

(** {1:nonblock Non-blocking mode} *)

val set_nonblock : t -> unit
(** [set_nonblock pty] enables non-blocking I/O. {!read} and {!write} raise
    [Unix.Unix_error (EAGAIN, _, _)] instead of blocking. Persists until
    {!clear_nonblock} or {!close}.

    Raises [Unix.Unix_error] on failure. *)

val clear_nonblock : t -> unit
(** [clear_nonblock pty] restores blocking I/O.

    Raises [Unix.Unix_error] on failure. *)
