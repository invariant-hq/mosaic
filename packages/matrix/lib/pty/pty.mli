(** Pseudo-terminals, and programs run on them.

    A {e pseudo-terminal} is a pair of devices that behaves as a terminal. Its
    {e slave} is the terminal a program runs on, with the line discipline,
    window size and signals of a terminal, and serves as the program's standard
    input, output and error. Its {e master} is the other end, which a terminal
    emulator holds. What the master writes, the program reads as typed input,
    and what the program writes, the master reads. A {e handle} ({!t}) holds the
    descriptor of one side and, when {!spawn} made it, the {e child}, the
    process that runs the program on the slave. The terminal {e hangs up} once
    the processes running on it have closed it or exited, and the master then
    reads the {e end of output} ({!section-io}).

    The life cycle of a handle from {!spawn} usually goes like this:
    + {!spawn} opens a pseudo-terminal and starts a program on its slave.
    + {!read} reads the program's output from the master, {!write} writes its
      input, and {!set_winsize} tells it the terminal's new size.
    + {!read} reaches the end of output.
    + {!close} hangs up the terminal, then ends and reaps the child.

    {!with_spawn} does the first step and the last around a function.

    Unless a value's doc says otherwise, an operation on a handle raises
    [Unix.Unix_error] with the error of the system call that failed, and
    [Unix.Unix_error (EBADF, _, _)] once the handle is closed. A handle starts
    in blocking mode ({!section-nonblock}).

    Two threads may use one handle concurrently when one of them only calls
    {!read} and the other calls neither {!read}, {!close}, {!set_nonblock} nor
    {!clear_nonblock}. Reading shares no state with the handle's other
    operations but the descriptor, and the system serves the two directions of a
    terminal independently. A terminal emulator can thus read in one thread and
    write, resize the terminal and signal the child in another. A {!close} would
    free the descriptor's number while the read may still use it, and a mode
    change applies to the read too. Every other concurrent use of a handle needs
    a lock. {!read}, {!write} and {!spawn} release the runtime lock while they
    block, so other threads run.

    The module works on Linux, macOS, FreeBSD, DragonFly, NetBSD and OpenBSD. On
    any other platform, Windows included, {!open_pty} and {!spawn} raise
    [Unix.Unix_error (ENOSYS, _, _)].

    This function runs a program and returns all it writes. It reads until the
    end of output, which {!read} reports as [0] on some kernels and as [EIO] on
    others, then {!with_spawn} closes the handle.
    {[
    let output prog args =
      Pty.with_spawn ~prog ~args (fun pty ->
          let out = Buffer.create 256 and b = Bytes.create 4096 in
          let rec loop () =
            match Pty.read pty b 0 (Bytes.length b) with
            | 0 -> Buffer.contents out
            | n ->
                Buffer.add_subbytes out b 0 n;
                loop ()
            | exception Unix.Unix_error (Unix.EIO, _, _) -> Buffer.contents out
          in
          loop ())
    ]} *)

(** {1:types Handles and sizes} *)

type t
(** The type for pseudo-terminal handles. *)

type winsize = {
  rows : int;  (** The number of rows. *)
  cols : int;  (** The number of columns. *)
  xpixel : int;  (** The width in pixels, or [0] when it is not known. *)
  ypixel : int;  (** The height in pixels, or [0] when it is not known. *)
}
(** The type for terminal sizes. Each field must be in 0..65535, the range of
    the kernel's [struct winsize]. A field outside it is set modulo 65536. *)

(** {1:creating Opening}

    The descriptors of a new pseudo-terminal are close-on-exec, so no program
    that the process starts inherits them by accident. While any process holds
    the slave open, the terminal stays open and the master reads no end of
    output. [winsize] is the initial size, and the kernel's default size when it
    is omitted. *)

val open_pty : ?winsize:winsize -> unit -> t * t
(** [open_pty ()] is [(master, slave)], the two sides of a new pseudo-terminal,
    as handles without a child. Both must be closed with {!close}.

    To give the slave to a child, a program must copy it onto the child's
    standard descriptors with [Unix.dup2 ~cloexec:false]. A plain [Unix.dup2],
    like [Unix.create_process], clears the flag on a copy but leaves it set when
    the slave already is the target descriptor, as when the process runs with
    its standard descriptors closed.

    On macOS, OpenBSD and NetBSD the slave's last close discards the output the
    master has not read. A program that runs its own child on the slave keeps
    that output by holding its slave handle until it has read it, as {!spawn}
    does. *)

val with_pty : ?winsize:winsize -> (t -> t -> 'a) -> 'a
(** [with_pty f] is [f master slave] on a new pair from [open_pty ?winsize ()].
    It closes both handles when [f] returns or raises. *)

(** {1:spawning Running a program}

    {!spawn} starts the program afresh, whatever state the parent is in. Exec
    passes on every descriptor not marked close-on-exec, the signal mask, and
    the signals set to be ignored, such as [SIGPIPE] in many programs. So the
    child closes every descriptor but its standard ones, and holds open no pipe
    or terminal whose reader waits for its end. It also gives every signal its
    default action and blocks none. The child leads a new session, whose
    controlling terminal is the slave, and the slave is its standard input,
    output and error.

    The child runs no OCaml code between the fork and the exec, only system
    calls that are safe in the child of a multithreaded process. So [spawn]
    works while other domains run, and after they have run, where [Unix.fork]
    fails.

    The program's argument vector is [prog :: args]. As with [execvp], a [prog]
    without ['/'] is searched for in the directories of the parent's [PATH]
    ([/bin:/usr/bin] when it is unset), and a file that the kernel cannot
    execute, a script without [#!], runs under [/bin/sh]. *)

val spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  unit ->
  t
(** [spawn ~prog ~args ()] is the master handle of a new pseudo-terminal with
    [prog] running on its slave, in a child whose process id is {!pid}. [env] is
    the child's environment as ["KEY=value"] strings, and defaults to the
    parent's. [cwd] is its working directory, and defaults to the parent's.
    [winsize] is as for {!open_pty}.

    [spawn] returns once [prog] runs.

    Raises [Unix.Unix_error] if opening the pseudo-terminal or [fork] fails,
    with [EINVAL] if [prog], an element of [args] or of [env] holds a NUL byte,
    with [ENOENT] if [cwd] does, or with the child's error if the child cannot
    be set up or [prog] cannot be run. That error names the step that failed:
    [setsid], [ioctl(TIOCSCTTY)], [dup2], [chdir] with [cwd] as argument, or
    [execvp] with [prog], for instance with [ENOENT] when no such program is
    found. The child has then exited and been reaped. *)

val with_spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  (t -> 'a) ->
  'a
(** [with_spawn ~prog ~args f] is [f pty] where [pty] is
    [spawn ?env ?cwd ?winsize ~prog ~args ()]. It closes [pty] when [f] returns
    or raises, which ends and reaps the child ({!close}).

    Raises as {!spawn} does. *)

(** {1:io Reading and writing}

    What the child writes stays readable from the master until it is read. As
    with a pipe, the child blocks in a write while the terminal's buffer is
    full. The master reads the end of output once the terminal has hung up, and
    {!read} then returns [0] or raises [Unix.Unix_error (EIO, _, _)].

    Kernels differ in what becomes of the output once the child has exited.
    Linux keeps it until it is read. On macOS, OpenBSD and NetBSD the child
    leads the terminal's session, and its exit waits until the master has read
    all its output, even after {!kill}. A child whose output nobody reads may
    thus never finish exiting. FreeBSD and DragonFly may discard what the master
    has not read within about a second of the child's exit. A program must
    therefore read to the end of output, or {!close} the handle, before it waits
    for the child ({!wait}). *)

val read : t -> bytes -> int -> int -> int
(** [read pty b off len] reads at most [len] bytes into [b] from [off] and
    returns the number read, or [0] at the end of output.

    In blocking mode it blocks until a byte or the end of output is ready. In
    non-blocking mode it raises [Unix.Unix_error (EAGAIN, _, _)] when none is.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

val write : t -> bytes -> int -> int -> int
(** [write pty b off len] writes the [len] bytes of [b] from [off] and returns
    [len]. In non-blocking mode it returns the number of bytes written before
    the terminal's buffer was full, and raises [Unix.Unix_error (EAGAIN, _, _)]
    when that number is [0].

    In blocking mode it blocks until every byte is written.

    Raises [Invalid_argument] if [off] and [len] do not designate a valid range
    of [b]. *)

val write_string : t -> string -> int -> int -> int
(** [write_string pty s off len] is {!write} on the bytes of [s]. *)

(** {1:nonblock Blocking and non-blocking mode}

    In blocking mode, which a new handle is in, {!read} and {!write} block until
    they can proceed. In non-blocking mode they raise
    [Unix.Unix_error (EAGAIN, _, _)] where they would block. *)

val set_nonblock : t -> unit
(** [set_nonblock pty] puts [pty] in non-blocking mode until {!clear_nonblock}
    or {!close}. *)

val clear_nonblock : t -> unit
(** [clear_nonblock pty] puts [pty] back in blocking mode. *)

(** {1:winsize_ops Window size}

    When the size of the terminal changes, the kernel sends [SIGWINCH] to the
    terminal's foreground process group (POSIX, [tcsetwinsize]). Setting the
    size it already has sends nothing. *)

val get_winsize : t -> winsize
(** [get_winsize pty] is the terminal's size. *)

val set_winsize : t -> winsize -> unit
(** [set_winsize pty ws] sets the terminal's size to [ws]. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize pty ~rows ~cols] is
    {!set_winsize}[ pty { rows; cols; xpixel = 0; ypixel = 0 }]. *)

val inherit_size : src:t -> dst:t -> unit
(** [inherit_size ~src ~dst] is [set_winsize dst (get_winsize src)]. *)

(** {1:fd File descriptors}

    A handle's descriptor serves [Unix.select] and event loops. It must not be
    used after {!close}, since the system may give its number to another
    descriptor. *)

val file_descr : t -> Unix.file_descr
(** [file_descr pty] is [pty]'s descriptor. *)

val in_fd : t -> Unix.file_descr
(** [in_fd pty] is {!file_descr}[ pty]. A side of a pseudo-terminal reads and
    writes through one descriptor. *)

val out_fd : t -> Unix.file_descr
(** [out_fd pty] is {!file_descr}[ pty]. *)

(** {1:process The child}

    Closing the master hangs up the terminal, and the kernel sends [SIGHUP] to
    the child's session. A process is {e reaped} once its parent has collected
    its exit status. A handle holds its child until the handle reaps it, by
    {!wait} or {!close}. Until then the child's process id names the child and
    no other process.

    Before {!terminate}, {!kill} and {!close} signal the child, they ask the
    system without blocking whether it has exited. A child that has exited is
    reaped then and is not signalled, and {!wait} returns its status. When the
    program has reaped the child itself with [Unix.waitpid] on {!pid}, the
    system may have given its process id to another process. The handle then
    forgets the id and signals nothing. A program waits for the child with
    {!wait}, after which {!pid} is [None]. *)

val pid : t -> int option
(** [pid pty] is [Some p] while [pty] holds a child that it has not reaped,
    where [p] is the child's process id, and [None] otherwise. *)

val wait : t -> Unix.process_status
(** [wait pty] is the exit status of [pty]'s child. It reaps the child, and
    [pty] then has no child. [wait] works on an open or closed handle.

    It blocks until the child has exited, and releases the runtime lock while it
    blocks.

    Raises [Invalid_argument] if [pty] has no child, as a handle from
    {!open_pty}, a handle closed with [wait] or a handle already waited for has
    none. Raises [Unix.Unix_error (ECHILD, _, _)] if the program has reaped the
    child itself, and [pty] then has no child. *)

val close : ?wait:bool -> t -> unit
(** [close pty] closes [pty]'s descriptor and, for a handle from {!spawn}, the
    slave it holds, which hangs up the terminal. The output not yet read is
    lost, and a child whose exit waits for its output to be read can finish. For
    a handle with a child, [close] then sends [SIGTERM] to a child still running
    ({!section-process}) and, when [wait] is [true] (the default), reaps the
    child. It sends [SIGKILL] to a child that has not exited about 100 ms after
    the [SIGTERM]. A second [close] does nothing.

    With [wait], [close] blocks until the child is reaped. With [~wait:false],
    it blocks at most about 100 ms, and [pty] keeps its child, which the program
    must reap with {!wait}.

    [close] never raises. *)

val terminate : t -> unit
(** [terminate pty] sends [SIGTERM] to the child, unless it has exited or has
    been reaped ({!section-process}), and leaves the terminal open. It ignores
    the errors of [kill].

    Raises [Invalid_argument] if [pty] has no child ({!wait}). *)

val kill : t -> unit
(** [kill pty] sends [SIGKILL] to the child, which cannot catch it, unless it
    has exited or has been reaped ({!section-process}), and leaves the terminal
    open. It ignores the errors of [kill]. On some kernels a killed child
    finishes exiting only once its output is read ({!section-io}). {!close}
    hangs up the terminal first.

    Raises [Invalid_argument] if [pty] has no child. *)
