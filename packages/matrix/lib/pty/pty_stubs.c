// POSIX PTY stubs. On platforms without POSIX PTYs — including Windows —
// every entry point raises Unix_error(ENOSYS, ...). A previous ConPTY
// backend stored a custom block where the OCaml side expects a
// Unix.file_descr, which made every I/O call reinterpret unrelated memory
// as a handle; failing loudly is the honest scope until a sound Windows
// implementation exists.

#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define MATRIX_HAS_POSIX_PTY 1
#endif

#ifdef MATRIX_HAS_POSIX_PTY
#define _GNU_SOURCE  // For ptsname on Linux
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/prctl.h>
#include <sys/syscall.h>
#endif
#endif  // MATRIX_HAS_POSIX_PTY

#include <errno.h>

// OCaml C FFI headers
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifdef MATRIX_HAS_POSIX_PTY

// A C struct to hold the file descriptor pair.
struct pty_fds {
  int master_fd;
  int slave_fd;
};

// This is the modern, portable way to open a PTY.
struct pty_fds c_open_pty_posix() {
  struct pty_fds fds = {-1, -1};

  // Enter blocking section for potentially blocking syscalls
  caml_enter_blocking_section();

  // Both ends are close-on-exec (see open_pty in pty.mli), set by the open
  // itself so that no child forked meanwhile by another thread inherits them.
  int master_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master_fd == -1) {
    caml_leave_blocking_section();
    return fds;
  }

  // Grant permissions to the slave device
  if (grantpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  // Unlock the slave device
  if (unlockpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  caml_leave_blocking_section();

  // Get slave name - use thread-safe ptsname_r where available
  char *slave_name = NULL;
#ifdef __GLIBC__
  char slave_name_buf[PATH_MAX];
  if (ptsname_r(master_fd, slave_name_buf, sizeof(slave_name_buf)) != 0) {
    close(master_fd);
    return fds;
  }
  slave_name = slave_name_buf;
#else
  slave_name = ptsname(master_fd);
  if (slave_name == NULL) {
    close(master_fd);
    return fds;
  }
#endif

  caml_enter_blocking_section();
  fds.slave_fd = open(slave_name, O_RDWR | O_NOCTTY | O_CLOEXEC);
  caml_leave_blocking_section();

  if (fds.slave_fd == -1) {
    close(master_fd);
    // CRITICAL FIX: Reset master_fd to indicate failure
    fds.master_fd = -1;
    fds.slave_fd = -1;
    return fds;
  }

  fds.master_fd = master_fd;
  return fds;
}

CAMLprim value ocaml_pty_open(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);

  struct pty_fds fds = c_open_pty_posix();

  if (fds.master_fd == -1) {
    uerror("open_pty", Nothing);
  }

  res = caml_alloc(2, 0);
  Store_field(res, 0, Val_int(fds.master_fd));
  Store_field(res, 1, Val_int(fds.slave_fd));
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_get_winsize(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);
  struct winsize ws;

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCGWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCGWINSZ)", Nothing);
  }

  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_int(ws.ws_row));
  Store_field(res, 1, Val_int(ws.ws_col));
  Store_field(res, 2, Val_int(ws.ws_xpixel));
  Store_field(res, 3, Val_int(ws.ws_ypixel));
  CAMLreturn(res);
}

// Byte version for set_winsize is needed for records with non-float fields
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  CAMLparam2(fd, ws_val);
  struct winsize ws;
  ws.ws_row = Int_val(Field(ws_val, 0));
  ws.ws_col = Int_val(Field(ws_val, 1));
  ws.ws_xpixel = Int_val(Field(ws_val, 2));
  ws.ws_ypixel = Int_val(Field(ws_val, 3));

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCSWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCSWINSZ)", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}

// What the keeper and the program need between fork and exec, built by the
// parent.
struct child {
  int slave;
  int report;           // the write end of the report pipe
  const char *cwd;      // NULL to stay in the parent's directory
  char **paths;         // where to look for the program, in order
  char **argv, **envp;  // NULL-terminated
  char **sh_argv;       // {"/bin/sh", NULL, argv[1], ...}
  int fd_limit;         // no descriptor is opened at this number or above
  int last_signal;
  struct sigaction default_action;
  sigset_t no_signals;
};

// The steps of a spawn that can fail, as the error names them, and PROGRAM,
// the message that carries the program's process id.
enum step {
  SETSID,
  SETCTTY,
  DUP2,
  CHDIR,
  FORK,
  SETPGID,
  TCSETPGRP,
  EXEC,
  PROGRAM
};
static const char *const step_names[] = {
    "setsid", "ioctl(TIOCSCTTY)", "dup2",      "chdir",
    "fork",   "setpgid",          "tcsetpgrp", "execvp"};

// Writes a message on the report pipe: a step that failed with its errno,
// or PROGRAM with the program's process id. One write of a few bytes to a
// pipe is atomic, so the keeper's and the program's messages never mix.
static void report(int fd, enum step step, int value) {
  int message[2] = {step, value};
  (void)!write(fd, message, sizeof message);
}

// Closes every descriptor from [low] up. POSIX has no call for this, nor a
// way to list a process's descriptors: Linux (5.9) has close_range and the
// BSDs closefrom, one system call each. Elsewhere, as on macOS, and on older
// Linux, close each number below [limit], the RLIMIT_NOFILE soft limit. A
// descriptor opened before that limit was lowered can lie above it, and
// stays open there.
static void close_from(int low, int limit) {
#if defined(__linux__) && defined(SYS_close_range)
  if (syscall(SYS_close_range, low, ~0U, 0) == 0) return;
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
    defined(__DragonFly__)
  closefrom(low);
  return;
#endif
  for (int fd = low; fd < limit; fd++) close(fd);
}

// The program's process group, which the keeper's SIGHUP handler signals,
// with SIGCONT as a shell does, so that a stopped program takes the hangup.
static pid_t program_pid;

static void hangup(int sig) {
  kill(-program_pid, sig);
  kill(-program_pid, SIGCONT);
}

// Wakes the keeper's wait for the program, at each of its changes.
static void program_changed(int sig) { (void)sig; }

// Runs the program as execvp does: a directory without the program, or that
// is not a directory, moves on to the next; a file without permission is
// remembered; any other failure ends the search. A file the kernel cannot
// execute (ENOEXEC) is a script without #!, which /bin/sh runs. Returns
// only on failure, with errno set.
static void exec_program(struct child *c) {
  int err = ENOENT;
  for (char **path = c->paths; *path != NULL; path++) {
    execve(*path, c->argv, c->envp);
    if (errno == ENOEXEC) {
      c->sh_argv[1] = *path;
      execve("/bin/sh", c->sh_argv, c->envp);
    }
    if (errno == EACCES)
      err = EACCES;
    else if (errno != ENOENT && errno != ENOTDIR) {
      err = errno;
      break;
    }
  }
  errno = err;
}

// The child of spawn: the keeper of the program's session (see spawn in
// pty.mli). It opens the session on the slave, forks the program into the
// foreground process group, waits for it, drains the terminal and exits as
// the program did. It returns, in the keeper or in the program, only on
// failure, with the step that failed and errno set.
//
// The parent may run other threads, among them OCaml domains, whose locks
// the fork copied in whatever state they were in: POSIX lets the child of a
// multithreaded process call only async-signal-safe functions until it
// execs. Every call here is one, or a plain system call that POSIX leaves
// off its list (ioctl, close_range, closefrom, waitid, setrlimit, prctl).
// Nothing here allocates: the parent built the strings and arrays. Every
// signal has been blocked since before the fork, so no handler of the
// parent's runs here, and the keeper keeps them blocked: a signal from the
// terminal, or from the parent, is the program's.
static enum step keep(struct child *c) {
  // Keep the report off the std fds, which the slave replaces.
  int report_fd = fcntl(c->report, F_DUPFD_CLOEXEC, 3);
  if (report_fd == -1) return DUP2;
  c->report = report_fd;
  if (setsid() == -1) return SETSID;
  if (ioctl(c->slave, TIOCSCTTY, 0) == -1) return SETCTTY;
  // The slave is close-on-exec. dup2 clears the flag on a copy, but does
  // nothing when the slave already is that std fd.
  for (int fd = 0; fd <= 2; fd++)
    if ((c->slave == fd ? fcntl(fd, F_SETFD, 0) : dup2(c->slave, fd)) == -1)
      return DUP2;
  // Exec keeps every descriptor that is not close-on-exec, so the program
  // would inherit any the parent opened without the flag, and hold pipes and
  // terminals open that their readers wait to see closed. Keep only the std
  // fds: park the report at 3, still close-on-exec, and close every other.
  if (c->report != 3) {
    if (dup2(c->report, 3) == -1 || fcntl(3, F_SETFD, FD_CLOEXEC) == -1)
      return DUP2;
    c->report = 3;
  }
  close_from(4, c->fd_limit);
  if (c->cwd != NULL && chdir(c->cwd) == -1) return CHDIR;
  // Exec keeps the signals the parent ignores, such as the SIGPIPE that many
  // programs ignore: give every signal its default action, so the program
  // starts with them. The keeper needs SIGCHLD's default too: ignored, it
  // would reap the program by itself. sigaction fails, harmlessly, for
  // SIGKILL, SIGSTOP and the signals that the C library reserves.
  for (int sig = 1; sig <= c->last_signal; sig++)
    sigaction(sig, &c->default_action, NULL);
  program_pid = fork();
  if (program_pid == -1) return FORK;
  if (program_pid == 0) {
    // The program leads the foreground process group, so the terminal's
    // signals and the parent's reach it and the processes it starts, and
    // never the keeper. Every kernel lets a process with SIGTTOU blocked, as
    // it is here, take the foreground from the background.
    if (setpgid(0, 0) == -1) return SETPGID;
    if (tcsetpgrp(0, getpid()) == -1) return TCSETPGRP;
    pthread_sigmask(SIG_SETMASK, &c->no_signals, NULL);
    exec_program(c);
    return EXEC;
  }
  report(3, PROGRAM, program_pid);
  close(3);
  // The kernel sends the hangup's SIGHUP to the session's leader, the
  // keeper, which passes it on to the program's process group. The handler
  // is the keeper's alone: the program would signal its own group with it.
  struct sigaction on_hangup = c->default_action;
  on_hangup.sa_handler = hangup;
  sigaction(SIGHUP, &on_hangup, NULL);
  sigset_t hangups;
  sigemptyset(&hangups);
  sigaddset(&hangups, SIGHUP);
  pthread_sigmask(SIG_UNBLOCK, &hangups, NULL);
  // Learn of the program's exit without reaping it: unreaped, its id names
  // it and no other process for as long as the handle may signal it. macOS
  // reports a stop to waitid without WSTOPPED, at once and on every call
  // while the program is stopped, so the keeper takes an exit alone and
  // sleeps until the next SIGCHLD otherwise. SIGCHLD stays blocked but in
  // sigsuspend, so none comes between the look and the sleep unseen.
  struct sigaction on_change = c->default_action;
  on_change.sa_handler = program_changed;
  sigaction(SIGCHLD, &on_change, NULL);
  sigset_t changes;
  sigfillset(&changes);
  sigdelset(&changes, SIGHUP);
  sigdelset(&changes, SIGCHLD);
  siginfo_t status;
  for (;;) {
    status.si_pid = 0;
    if (waitid(P_PID, program_pid, &status, WEXITED | WNOWAIT | WNOHANG) ==
        -1) {
      if (errno == EINTR) continue;
      _exit(127);
    }
    if (status.si_pid == program_pid &&
        (status.si_code == CLD_EXITED || status.si_code == CLD_KILLED ||
         status.si_code == CLD_DUMPED))
      break;
    sigsuspend(&changes);
  }
  // Wait until the master has read all the program wrote, or the terminal
  // has hung up, after which the drain fails or returns at once. tcdrain
  // returns at once on Linux, which keeps unread output after the slave's
  // last close, and gives up with EAGAIN after kern.tty_drainwait (5
  // minutes) on FreeBSD, where it is retried: POSIX gives it no timeout.
  while (tcdrain(0) == -1 && (errno == EINTR || errno == EAGAIN)) {
  }
  if (status.si_code == CLD_EXITED) _exit(status.si_status);
  // Die by the program's signal, so the parent sees the status the program
  // ended with, without a core dump of the keeper: the program's, if any, is
  // enough. Linux runs its crash handler (systemd-coredump, apport) for a
  // dumpable process whatever the core limit; macOS writes a crash report
  // for the keeper, which it cannot be told not to.
  int sig = status.si_status;
  struct rlimit no_core = {0, 0};
  setrlimit(RLIMIT_CORE, &no_core);
#ifdef __linux__
  prctl(PR_SET_DUMPABLE, 0);
#endif
  sigaction(sig, &c->default_action, NULL);
  sigset_t fatal;
  sigemptyset(&fatal);
  sigaddset(&fatal, sig);
  pthread_sigmask(SIG_UNBLOCK, &fatal, NULL);
  raise(sig);
  _exit(128 + sig);
}

// A pipe whose ends are both close-on-exec. macOS has no pipe2: a fork by
// another thread between pipe and fcntl leaks the ends into that child.
static int cloexec_pipe(int fds[2]) {
#ifdef __APPLE__
  if (pipe(fds) == -1) return -1;
  fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  return 0;
#else
  return pipe2(fds, O_CLOEXEC);
#endif
}

static void check_strings(value v, const char *cmd) {
  for (mlsize_t i = 0; i < Wosize_val(v); i++)
    if (!caml_string_is_c_safe(Field(v, i)))
      caml_unix_error(EINVAL, cmd, Field(v, i));
}

// The keeper reports the program's process id, and the keeper or the
// program a failure before exec, on a close-on-exec pipe, in either order;
// the pipe ends when the keeper has closed its end and the program has
// exec'd or exited. So spawn returns (keeper, program) once the program
// runs, and raises the child's error otherwise, having reaped the keeper.
CAMLprim value ocaml_pty_fork_exec(value slave, value paths, value argv,
                                   value env, value cwd) {
  CAMLparam5(slave, paths, argv, env, cwd);
  CAMLlocal1(res);
  check_strings(paths, "execvp");
  check_strings(argv, "execvp");
  check_strings(env, "execvp");
  if (Is_some(cwd)) caml_unix_check_path(Some_val(cwd), "chdir");

  int pipe_fds[2];
  if (cloexec_pipe(pipe_fds) == -1) caml_uerror("pipe", Nothing);
  struct child c;
  c.slave = Int_val(slave);
  c.report = pipe_fds[1];
  c.cwd = Is_some(cwd) ? caml_stat_strdup(String_val(Some_val(cwd))) : NULL;
  c.paths = caml_unix_cstringvect(paths, "execvp");
  c.argv = caml_unix_cstringvect(argv, "execvp");
  c.envp = caml_unix_cstringvect(env, "execvp");
  mlsize_t argc = Wosize_val(argv);
  c.sh_argv = caml_stat_alloc((argc + 2) * sizeof(char *));
  c.sh_argv[0] = "/bin/sh";
  c.sh_argv[1] = NULL;
  for (mlsize_t i = 1; i <= argc; i++) c.sh_argv[i + 1] = c.argv[i];
  struct rlimit nofile;
  c.fd_limit = getrlimit(RLIMIT_NOFILE, &nofile) == 0 &&
                       nofile.rlim_cur != RLIM_INFINITY &&
                       nofile.rlim_cur < INT_MAX
                   ? (int)nofile.rlim_cur
                   : INT_MAX;
#ifdef SIGRTMAX
  c.last_signal = SIGRTMAX > NSIG - 1 ? SIGRTMAX : NSIG - 1;
#else
  c.last_signal = NSIG - 1;
#endif
  memset(&c.default_action, 0, sizeof c.default_action);
  c.default_action.sa_handler = SIG_DFL;
  sigemptyset(&c.default_action.sa_mask);
  sigemptyset(&c.no_signals);

  sigset_t all_signals, parent_signals;
  sigfillset(&all_signals);
  pthread_sigmask(SIG_SETMASK, &all_signals, &parent_signals);
  pid_t keeper = fork();
  if (keeper == 0) {
    enum step failed = keep(&c);
    report(c.report, failed, errno);
    _exit(127);
  }
  int fork_errno = errno;
  pthread_sigmask(SIG_SETMASK, &parent_signals, NULL);
  close(pipe_fds[1]);
  // A keeper that is gone before it reported the program is a failed fork.
  int message[2], failure[2] = {FORK, ECHILD}, program = 0, failed = 0;
  if (keeper != -1) {
    caml_enter_blocking_section();
    for (;;) {
      ssize_t n = read(pipe_fds[0], message, sizeof message);
      if (n == -1 && errno == EINTR) continue;
      if (n != sizeof message) break;
      if (message[0] == PROGRAM)
        program = message[1];
      else {
        failed = 1;
        failure[0] = message[0];
        failure[1] = message[1];
      }
    }
    if (program == 0) failed = 1;
    if (failed)
      while (waitpid(keeper, NULL, 0) == -1 && errno == EINTR);
    caml_leave_blocking_section();
  }
  close(pipe_fds[0]);

  caml_stat_free((char *)c.cwd);
  caml_unix_cstringvect_free(c.paths);
  caml_unix_cstringvect_free(c.argv);
  caml_unix_cstringvect_free(c.envp);
  caml_stat_free(c.sh_argv);
  if (keeper == -1) caml_unix_error(fork_errno, "fork", Nothing);
  if (failed) {
    enum step step = failure[0];
    value arg = step == CHDIR  ? Some_val(cwd)
                : step == EXEC ? Field(argv, 0)
                               : Nothing;
    caml_unix_error(failure[1], step_names[step], arg);
  }
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_int(keeper));
  Store_field(res, 1, Val_int(program));
  CAMLreturn(res);
}

#else  // Unsupported platforms, including Windows: fail loudly.

static value pty_unsupported(const char *cmd) {
  errno = ENOSYS;
  uerror(cmd, caml_copy_string("PTYs are not supported on this platform"));
  return Val_unit;  // Unreachable
}

CAMLprim value ocaml_pty_open(value unit) {
  (void)unit;
  return pty_unsupported("open_pty");
}
CAMLprim value ocaml_pty_get_winsize(value fd) {
  (void)fd;
  return pty_unsupported("get_winsize");
}
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  (void)fd;
  (void)ws_val;
  return pty_unsupported("set_winsize");
}
CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}
CAMLprim value ocaml_pty_fork_exec(value slave, value paths, value argv,
                                   value env, value cwd) {
  (void)slave;
  (void)paths;
  (void)argv;
  (void)env;
  (void)cwd;
  return pty_unsupported("spawn");
}

#endif  // MATRIX_HAS_POSIX_PTY
