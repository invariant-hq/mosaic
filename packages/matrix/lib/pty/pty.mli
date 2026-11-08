(** Pseudo-terminals, and programs run on them.

    A {e pseudo-terminal} is a pair of devices that behaves as a terminal. Its
    {e slave} is the terminal a program runs on, with the line discipline,
    window size and signals of a terminal, and serves as the program's standard
    input, output and error. Its {e master} is the other end, which a terminal
    emulator holds. What the master writes, the program reads as typed input,
    and what the program writes, the master reads. A {e handle} ({!t}) holds the
    descriptor of one side and, when {!spawn} made it, the {e child}: the
    {e program} running on the slave and the {e keeper} of its session
    ({!section-spawning}). The terminal {e hangs up} once the processes
    running on it have closed it or exited, and the master then reads the
    {e end of output} ({!section-io}).

    The life cycle of a handle from {!spawn} usually goes like this:
    + {!spawn} opens a pseudo-terminal and starts a program on its slave.
    + {!read} reads the program's output from the master, {!write} writes its
      input, and {!set_winsize} tells it the terminal's new size.
    + {!read} reaches the end of output.
    + {!close} hangs up the terminal, then ends the program and reaps the
      child.

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
    write, resize the terminal and signal the program in another. A {!close}
    would free the descriptor's number while the read may still use it, and a
    mode change applies to the read too. Every other concurrent use of a handle needs
    a lock. {!read}, {!write} and {!spawn} release the runtime lock while they
    block, so other threads run.

    The module works on Linux, macOS, FreeBSD, DragonFly, NetBSD and OpenBSD. On
    any other platform, Windows included, {!open_pty} and {!spawn} raise
    [Unix.Unix_error (ENOSYS, _, _)].

    This function runs a program and returns all it writes, however soon the
    program exits. It reads until the end of output, which {!read} reports as
    [0] on some kernels and as [EIO] on others, then {!with_spawn} closes the
    handle.
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
    its standard descriptors closed. Some kernels discard the output the master
    has not read when the slave's last holder closes it, or when the leader of
    the terminal's session exits: a program that runs its own child on the
    slave reads the output before it lets either happen, as {!spawn}'s keeper
    does ({!section-spawning}). *)

val with_pty : ?winsize:winsize -> (t -> t -> 'a) -> 'a
(** [with_pty f] is [f master slave] on a new pair from [open_pty ?winsize ()].
    It closes both handles when [f] returns or raises. *)

(** {1:spawning Running a program}

    {!spawn} starts the program afresh, whatever state the parent is in. Exec
    passes on every descriptor not marked close-on-exec, the signal mask, and
    the signals set to be ignored, such as [SIGPIPE] in many programs. So the
    child closes every descriptor but its standard ones, and holds open no pipe
    or terminal whose reader waits for its end. It also gives every signal its
    default action and blocks none. The program runs in a new session, whose
    controlling terminal is the slave, leads the session's foreground process
    group, and has the slave as its standard input, output and error.

    The session's leader is not the program but its {e keeper}, a process that
    [spawn] forks to run the program and hold the terminal open. A kernel ties
    a terminal to its session leader: when the leader exits, FreeBSD revokes
    the terminal and drops the output the master has not read, and macOS
    holds the exit until the master has read it. The keeper leads the session
    in the program's stead: it starts the program, waits for it, and passes
    its status on to the handle. Once the master has read all the program
    wrote, or the terminal has hung up, it kills what the program left in its
    process group, so the program's job ends with it, and exits. So a
    program's output is never lost to its exit, on any kernel. The keeper is
    the program's parent, and the one process the handle reaps: a program
    waits for the child with {!wait}, never with [Unix.waitpid] on {!pid}.
    The keeper acts on no signal but the hangup's [SIGHUP], which the kernel
    sends to the session's leader and the keeper passes on to the program's
    process group.

    The child runs no OCaml code between the fork and the exec, only system
    calls that are safe in the child of a multithreaded process. So [spawn]
    works while other domains run, and after they have run, where [Unix.fork]
    fails.

    The program's argument vector is [prog :: args]. As with [execvp], a [prog]
    without ['/'] is searched for in the directories of a [PATH], here the
    one of the program's environment ([/bin:/usr/bin] when it has none), and
    a file that the kernel cannot execute, a script without [#!], runs under
    [/bin/sh]. *)

val spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  unit ->
  t
(** [spawn ~prog ~args ()] is the master handle of a new pseudo-terminal with
    [prog] running on its slave, in a process whose id is {!pid}. [env] is
    the child's environment as ["KEY=value"] strings, and defaults to the
    parent's. [cwd] is its working directory, and defaults to the parent's.
    [winsize] is as for {!open_pty}.

    [spawn] returns once [prog] runs.

    Raises [Unix.Unix_error] if opening the pseudo-terminal or [fork] fails,
    with [EINVAL] if [prog], an element of [args] or of [env] holds a NUL byte,
    with [ENOENT] if [cwd] does, or with the child's error if the child cannot
    be set up or [prog] cannot be run. That error names the step that failed:
    [setsid], [ioctl(TIOCSCTTY)], [dup2], [chdir] with [cwd] as argument,
    [fork], [setpgid], [tcsetpgrp], or [execvp] with [prog], for instance with
    [ENOENT] when no such program is found. The child has then exited and been
    reaped. *)

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
    or raises, which ends the program and reaps the child ({!close}).

    Raises as {!spawn} does. *)

(** {1:io Reading and writing}

    What the program writes stays readable from the master until it is read,
    whether the program runs or has exited, even by {!kill}. As with a pipe,
    the program blocks in a write while the terminal's buffer is full. The
    master reads the end of output once the terminal has hung up, and {!read}
    then returns [0] or raises [Unix.Unix_error (EIO, _, _)].

    The child exits once the program has exited and its unread output is safe:
    at once on a kernel that keeps it (Linux), and otherwise once the master
    has read it or the terminal has hung up ({!section-spawning}). A program
    therefore reads to the end of output, or {!close}s the handle, before it
    waits for the child ({!wait}), or {!wait} may block until another thread
    does one of them. The child's exit ends what the program left in its
    process group, so the end of output follows the program's exit. A
    process that has left the group, with [setsid] or [setpgid], and holds
    the slave is not ended: the child's exit revokes the slave from it on
    every kernel but Linux, where the end of output comes once it has closed
    the slave. *)

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

(** {1:process The program}

    Closing the master hangs up the terminal, and the program's process group
    receives [SIGHUP], and [SIGCONT] if it was stopped ({!section-spawning}).
    A process is {e reaped} once its parent has collected its exit status. A
    handle holds its child until the handle reaps it, by {!wait} or {!close}.

    {!terminate}, {!kill} and {!close} signal the program's process group, so
    the processes it started in it too, as the terminal's interrupt key does.
    Before they signal, they ask the system without blocking whether the
    child has exited. A child that has exited is reaped then and is not
    signalled, and {!wait} returns its status. So they signal only a program
    whose keeper was running a moment before, while the program's process id
    names the program and no other process: the keeper leaves the program
    unreaped until it has ended the program's group, just before it exits.
    When the calling program has reaped the child itself, with [Unix.wait],
    the handle forgets it and signals nothing. A program waits for the child
    with {!wait}, after which {!pid} is [None]. *)

val pid : t -> int option
(** [pid pty] is [Some p] while [pty] holds a child that it has not reaped,
    where [p] is the program's process id, and the id of its process group,
    and [None] otherwise. The program is not a child of the calling process
    ({!section-spawning}): [Unix.waitpid] on [p] fails with [ECHILD]. *)

val wait : t -> Unix.process_status
(** [wait pty] is the exit status of [pty]'s program: [WEXITED] with its exit
    code, or [WSIGNALED] with the signal that ended it, or the keeper's own if
    a signal from another process ended the keeper before the program. It
    reaps the child, and [pty] then has no child. On a handle that {!close}
    has reaped, it is the status [close] kept.

    It blocks until the child has exited ({!section-io}), and releases the
    runtime lock while it blocks.

    Raises [Invalid_argument] if [pty] has no child, as a handle from
    {!open_pty} or a handle already waited for has none. Raises
    [Unix.Unix_error (ECHILD, _, _)] if the program has reaped the child
    itself, and [pty] then has no child; once {!close} has found it so, [wait]
    raises [Invalid_argument]. *)

val poll : t -> Unix.process_status option
(** [poll pty] is [Some (wait pty)] if the child has exited, and [None] while
    it runs. It never blocks. Raises as {!wait} does. *)

val close : ?wait:bool -> t -> unit
(** [close pty] closes [pty]'s descriptor, which for a handle from {!spawn}
    hangs up the terminal: the output not yet read is lost, and the program
    receives [SIGHUP], even when another process holds the master too, in
    which case it may receive it twice. For a handle with a child, [close]
    then gives a program still running about 100 ms to act on the hangup and
    exit, sends [SIGKILL] to its process group if it has not
    ({!section-process}), and, when [wait] is [true] (the default), reaps the
    child, keeping the program's status for {!wait}. A second [close] does
    nothing.

    With [wait], [close] blocks until the child is reaped, which a program
    that has left its process group, or that [SIGKILL] from this process
    cannot reach, as a set-user-ID program can, delays until it exits. With
    [~wait:false], it blocks at most about 100 ms, and [pty] keeps its child,
    which the program must reap with {!wait}.

    [close] never raises. *)

val terminate : t -> unit
(** [terminate pty] sends [SIGTERM] to the program's process group, unless the
    child has exited or has been reaped ({!section-process}), and leaves the
    terminal open. It ignores the errors of [kill].

    Raises [Invalid_argument] if [pty] has no child ({!wait}). *)

val kill : t -> unit
(** [kill pty] sends [SIGKILL], which cannot be caught, to the program's
    process group, unless the child has exited or has been reaped
    ({!section-process}), and leaves the terminal open. It ignores the errors
    of [kill]. The output a killed program wrote stays readable to its end
    ({!section-io}).

    Raises [Invalid_argument] if [pty] has no child. *)
