/* The terminal device: its window, its line settings, the monotonic clock,
   and the process's signals as the holders of terminals share them. */

#define _GNU_SOURCE
// CAML_INTERNALS declares the runtime's map from OCaml's signal numbers to
// the system's, which system_signal uses.
#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
/* On the Windows ports Unix.file_descr is a custom block wrapping a HANDLE,
   accessed with Handle_val; on Unix it is an immediate int. Both ports ship
   caml/unixsupport.h. */
#include <caml/unixsupport.h>
#include <caml/version.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#ifndef DISABLE_NEWLINE_AUTO_RETURN
#define DISABLE_NEWLINE_AUTO_RETURN 0x0008
#endif
#ifndef ENABLE_VIRTUAL_TERMINAL_INPUT
#define ENABLE_VIRTUAL_TERMINAL_INPUT 0x0200
#endif
#ifndef CREATE_WAITABLE_TIMER_HIGH_RESOLUTION
#define CREATE_WAITABLE_TIMER_HIGH_RESOLUTION 0x00000002
#endif
#ifndef ALTNUMPAD_BIT
#define ALTNUMPAD_BIT 0x04000000
#endif
#ifndef STATUS_CONTROL_C_EXIT
#define STATUS_CONTROL_C_EXIT 0xC000013AL
#endif
#include "console_text.h"
#else
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#endif

#ifdef _WIN32
static void raise_win32(const char *call) {
  caml_win32_maperr(GetLastError());
  caml_uerror(call, Nothing);
}
#endif

/* The window, [Some (cols, rows)], or [None] where the descriptor reports
   none. */
CAMLprim value matrix_tty_window(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(size, result);
  int cols = 0, rows = 0;
#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE h = Handle_val(fd);
  if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
    cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
  }
#else
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == 0) {
    cols = ws.ws_col;
    rows = ws.ws_row;
  }
#endif
  if (cols <= 0 || rows <= 0) CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(cols));
  Store_field(size, 1, Val_int(rows));
  result = caml_alloc_some(size);
  CAMLreturn(result);
}

/* The window's size in pixels, [Some (width, height)], or [None] where the
   descriptor reports none or a zero size. Windows reports none. */
CAMLprim value matrix_tty_window_pixels(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(size, result);
  int width = 0, height = 0;
#ifndef _WIN32
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == 0) {
    width = ws.ws_xpixel;
    height = ws.ws_ypixel;
  }
#endif
  if (width <= 0 || height <= 0) CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(width));
  Store_field(size, 1, Val_int(height));
  result = caml_alloc_some(size);
  CAMLreturn(result);
}

/* The consoles enable_vt changed

   A console is shared by every process attached to it, the shell among
   them, so the process gives back each console it changed, also when it ends
   where no OCaml code runs: Matrix's console control handler and its abort
   handler, and the end on a signal, restore those not yet given back. A
   record is published whole before it is marked used; the handlers read the
   records without the lock. The latest change is undone first, so that a
   console changed twice gets the mode it had before the first. */
#ifdef _WIN32
#define CONSOLES 16

struct console_change {
  HANDLE out;
  DWORD mode;
  UINT code_page;
  LONG order;
  volatile LONG used;
};

static struct console_change changes[CONSOLES];
static LONG changes_made = 0;
static SRWLOCK changes_lock = SRWLOCK_INIT;

static void restore_consoles(void) {
  int done[CONSOLES] = {0};
  for (;;) {
    int last = -1;
    for (int i = 0; i < CONSOLES; i++)
      if (changes[i].used && !done[i] &&
          (last < 0 || changes[i].order > changes[last].order))
        last = i;
    if (last < 0) return;
    done[last] = 1;
    SetConsoleMode(changes[last].out, changes[last].mode);
    SetConsoleOutputCP(changes[last].code_page);
  }
}
#endif

/* The index of the record of what enable_vt changed, [Some i], or [None]
   where nothing changed: on other systems, or when [fd] is no console. */
CAMLprim value matrix_tty_enable_vt(value fd) {
#ifdef _WIN32
  CAMLparam1(fd);
  HANDLE h = Handle_val(fd);
  DWORD mode;
  if (h == INVALID_HANDLE_VALUE || !GetConsoleMode(h, &mode))
    CAMLreturn(Val_none);
  AcquireSRWLockExclusive(&changes_lock);
  int i = 0;
  while (i < CONSOLES && changes[i].used) i++;
  if (i == CONSOLES) {
    ReleaseSRWLockExclusive(&changes_lock);
    caml_failwith("Tty.enable_vt: too many consoles not given back");
  }
  changes[i].out = h;
  changes[i].mode = mode;
  changes[i].code_page = GetConsoleOutputCP();
  changes[i].order = changes_made++;
  InterlockedExchange(&changes[i].used, 1);
  /* DISABLE_NEWLINE_AUTO_RETURN defers the wrap after the last column until
     the next printable character, as a VT100 does (terminfo's xenl): without
     it, a line that fills the width moves the cursor down at once, and the
     line feed that ends it leaves a blank row. */
  if (!SetConsoleMode(h, mode | ENABLE_PROCESSED_OUTPUT |
                             ENABLE_VIRTUAL_TERMINAL_PROCESSING |
                             DISABLE_NEWLINE_AUTO_RETURN)) {
    InterlockedExchange(&changes[i].used, 0);
    ReleaseSRWLockExclusive(&changes_lock);
    CAMLreturn(Val_none);
  }
  ReleaseSRWLockExclusive(&changes_lock);
  /* UTF-8 output. This affects the whole process, not only this handle. */
  SetConsoleOutputCP(CP_UTF8);
  CAMLreturn(caml_alloc_some(Val_int(i)));
#else
  (void)fd;
  return Val_none;
#endif
}

CAMLprim value matrix_tty_restore_console(value fd, value change) {
#ifdef _WIN32
  if (Is_some(change)) {
    int i = Int_val(Some_val(change));
    AcquireSRWLockExclusive(&changes_lock);
    if (changes[i].used) {
      HANDLE h = Handle_val(fd);
      if (h != INVALID_HANDLE_VALUE) SetConsoleMode(h, changes[i].mode);
      SetConsoleOutputCP(changes[i].code_page);
      InterlockedExchange(&changes[i].used, 0);
    }
    ReleaseSRWLockExclusive(&changes_lock);
  }
#else
  (void)fd;
  (void)change;
#endif
  return Val_unit;
}

/* A console's input mode, for raw mode, the key interval or no echo; the
   order of Tty's [console_mode]. Echo needs line input, so the key interval
   clears both. */
#ifdef _WIN32
static DWORD console_mode(DWORD mode, int kind) {
  switch (kind) {
  case 0:
    mode &= ~(DWORD)(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT |
                     ENABLE_PROCESSED_INPUT | ENABLE_MOUSE_INPUT);
    mode |= ENABLE_WINDOW_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;
    /* GetConsoleMode reports quick edit only with ENABLE_EXTENDED_FLAGS:
       without it, what it was could not be put back. */
    if (mode & ENABLE_EXTENDED_FLAGS) mode &= ~(DWORD)ENABLE_QUICK_EDIT_MODE;
    return mode;
  case 1:
    mode &= ~(DWORD)(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
    return mode | ENABLE_WINDOW_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;
  default:
    return mode & ~(DWORD)ENABLE_ECHO_INPUT;
  }
}
#endif

/* Sets the console [fd]'s input mode for [kind] and gives the mode it had. */
CAMLprim value matrix_tty_console_set(value fd, value kind) {
#ifdef _WIN32
  HANDLE h = Handle_val(fd);
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) raise_win32("GetConsoleMode");
  if (!SetConsoleMode(h, console_mode(mode, Int_val(kind)))) {
    if (Int_val(kind) != 2)
      caml_failwith("Tty: the console takes no virtual terminal input");
    raise_win32("SetConsoleMode");
  }
  return Val_long(mode);
#else
  (void)fd;
  (void)kind;
  caml_failwith("Tty: no console on this system");
#endif
}

CAMLprim value matrix_tty_console_restore(value fd, value mode) {
#ifdef _WIN32
  if (!SetConsoleMode(Handle_val(fd), (DWORD)Long_val(mode)))
    raise_win32("SetConsoleMode");
  return Val_unit;
#else
  (void)fd;
  (void)mode;
  caml_failwith("Tty: no console on this system");
#endif
}

/* Discards the records a console holds, if [fd] is one. */
CAMLprim value matrix_tty_console_flush(value fd) {
#ifdef _WIN32
  DWORD mode;
  HANDLE h = Handle_val(fd);
  if (GetConsoleMode(h, &mode)) FlushConsoleInputBuffer(h);
  return Val_unit;
#else
  (void)fd;
  caml_failwith("Tty: no console on this system");
#endif
}

/* IEXTEN is not representable in OCaml's Unix.terminal_io, so the settings
   reach it through these. With IEXTEN set, the line discipline processes
   VDISCARD (^O) and VLNEXT (^V) even in non-canonical mode, and those bytes
   never reach the program. */

CAMLprim value matrix_tty_get_iexten(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  CAMLreturn(Val_false);
#else
  struct termios tio;
  int r;
  do r = tcgetattr(Int_val(fd), &tio);
  while (r != 0 && errno == EINTR);
  if (r != 0) caml_uerror("tcgetattr", Nothing);
  CAMLreturn(Val_bool((tio.c_lflag & IEXTEN) != 0));
#endif
}

CAMLprim value matrix_tty_set_iexten(value fd, value on) {
  CAMLparam2(fd, on);
#ifndef _WIN32
  struct termios tio;
  int r;
  do r = tcgetattr(Int_val(fd), &tio);
  while (r != 0 && errno == EINTR);
  if (r != 0) caml_uerror("tcgetattr", Nothing);
  if (Bool_val(on))
    tio.c_lflag |= IEXTEN;
  else
    tio.c_lflag &= ~(tcflag_t)IEXTEN;
  do r = tcsetattr(Int_val(fd), TCSANOW, &tio);
  while (r != 0 && errno == EINTR);
  if (r != 0) caml_uerror("tcsetattr", Nothing);
#endif
  CAMLreturn(Val_unit);
}

/* Setting a terminal's settings from the background would stop the process
   with SIGTTOU, unless the signal is blocked (POSIX.1-2024, General
   Terminal Interface, 11.1.4). The first blocks it in the calling thread and
   says whether it was blocked before; the second puts that back. */
CAMLprim value matrix_tty_block_ttou(value unit) {
  (void)unit;
#ifdef _WIN32
  return Val_false;
#else
  sigset_t set, old;
  sigemptyset(&set);
  sigaddset(&set, SIGTTOU);
  pthread_sigmask(SIG_BLOCK, &set, &old);
  return Val_bool(sigismember(&old, SIGTTOU) == 1);
#endif
}

CAMLprim value matrix_tty_unblock_ttou(value was_blocked) {
#ifndef _WIN32
  if (!Bool_val(was_blocked)) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGTTOU);
    pthread_sigmask(SIG_UNBLOCK, &set, NULL);
  }
#else
  (void)was_blocked;
#endif
  return Val_unit;
}

/* Whether [fd] is the process's controlling terminal, the one terminal
   associated with its session. POSIX.1-2024, tcgetsid(): it "shall obtain
   the process group ID of the session for which the terminal specified by
   fildes is the controlling terminal", and fails with ENOTTY when "the file
   is not the controlling terminal". Linux answers tcgetsid() and tcgetpgrp()
   for the master of a pseudo-terminal with its slave's session and group, so
   a master whose slave controls the process's session would pass: a master
   is never a controlling terminal, and ptsname() names a slave only for a
   master.

   On Windows a process has at most one console, whose control events it
   receives: a handle of it is the counterpart. */
CAMLprim value matrix_tty_is_controlling(value fd) {
#ifdef _WIN32
  DWORD mode;
  if (Descr_kind_val(fd) == KIND_SOCKET) return Val_false;
  return Val_bool(GetConsoleMode(Handle_val(fd), &mode));
#else
  int f = Int_val(fd);
  pid_t sid = tcgetsid(f);
  if (sid == -1 || sid != getsid(0)) return Val_false;
#ifdef __GLIBC__
  char name[128];
  int master = ptsname_r(f, name, sizeof name) == 0;
#else
  int master = ptsname(f) != NULL;
#endif
  return Val_bool(!master);
#endif
}

/* Whether [fd] is the controlling terminal and its foreground process group
   is not the process's, as for a job a shell runs in the background. */
CAMLprim value matrix_tty_background(value fd) {
#ifdef _WIN32
  (void)fd;
  return Val_false;
#else
  if (!Bool_val(matrix_tty_is_controlling(fd))) return Val_false;
  pid_t fg = tcgetpgrp(Int_val(fd));
  return Val_bool(fg != -1 && fg != getpgrp());
#endif
}

/* OCaml has no monotonic clock in its distribution. */
static intnat now_ns(void) {
#ifdef _WIN32
  static LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  if (frequency.QuadPart == 0) QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&counter);
  return (intnat)(counter.QuadPart / frequency.QuadPart) * 1000000000 +
         (intnat)(counter.QuadPart % frequency.QuadPart) * 1000000000 /
             frequency.QuadPart;
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (intnat)ts.tv_sec * 1000000000 + ts.tv_nsec;
#endif
}

CAMLprim value matrix_tty_now(value unit) {
  (void)unit;
  return Val_long(now_ns());
}

/* Whether a read would return at once, without waiting. [Unix.select]
   allocates its answer; a run looks at its input on every step. On Windows
   a socket is asked with select(), and another handle whether it is
   signaled, which takes no record from a console as reading it would. */
CAMLprim value matrix_tty_readable(value fd) {
#ifdef _WIN32
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    fd_set set;
    struct timeval zero = {0, 0};
    FD_ZERO(&set);
    FD_SET(Socket_val(fd), &set);
    return Val_bool(select(0, &set, NULL, NULL, &zero) > 0);
  }
  return Val_bool(WaitForSingleObject(Handle_val(fd), 0) == WAIT_OBJECT_0);
#else
  struct pollfd p = {.fd = Int_val(fd), .events = POLLIN, .revents = 0};
  int n;
  do n = poll(&p, 1, 0);
  while (n < 0 && errno == EINTR);
  return Val_bool(n > 0);
#endif
}

/* Signals

   An OCaml signal handler runs only at the next point where OCaml code may
   run: a signal that lands after a loop's last check and before the loop
   blocks is not seen until something else wakes the loop, and SIGABRT, which
   abort() raises and never returns from, reaches no such point at all. These
   handlers are C: they record the signal for each holder it concerns and
   wake that holder, which they can do from a signal handler on POSIX (one
   byte written to a pipe) and from any thread on Windows (an event set), so
   a waiting loop always wakes.

   Windows has none of these signals but SIGINT and SIGABRT. A console
   control handler, which the system runs on a thread of its own, records
   Ctrl+C and Ctrl+Break as SIGINT and the console's closing as SIGHUP. There
   the holders' records are indexed by OCaml's numbers for the signals,
   negated: Sys.sigint is -6. */

#ifdef _WIN32
#define PENDING 31
#define P_ABRT 1
#define P_HUP 4
#define P_INT 6
#define P_QUIT 9
#define P_TERM 11
#define P_CONT 15
#define P_TSTP 17
#define P_WINCH 30
#else
#define PENDING 65
#define P_INT SIGINT
#define P_QUIT SIGQUIT
#define P_TERM SIGTERM
#define P_HUP SIGHUP
#endif

/* The holders of the process. A holder registers its wake and whether its
   terminal is the controlling one; the handlers record each signal for the
   holders it concerns and wake them. The window and job-control signals
   concern the controlling terminal's holders; the others every holder.

   A holder also registers what restores its terminal if the process ends
   where no OCaml code runs: its terminal's saved settings, on its input, and
   the bytes that reset what it set, written to its output. An abort restores
   every holder's terminal so, and {!restore_all} does it from OCaml for a
   holder that ends the process on a termination signal.

   The list only grows, from its head, under [holders_lock]; a holder that
   ends leaves its node to the next one. A handler walks it without the lock:
   it reads the head once, and a node's fields are set before the node is
   published or marked active. A holder is known by its id, which no other
   holder has, the next one on the same node included, so that a holder used
   after its release is found to be released.

   The reset bytes are as long as the holder needs, so they are allocated,
   and a handler may be writing them when the holder replaces them: new bytes
   are published whole, by one pointer, and the old ones are freed only once
   no reader is counted. A reader counts itself before it reads the pointer,
   and the holder counts the readers after it replaced it, so a reader it does
   not count reads the new bytes. Bytes a counted reader may hold wait in
   [retired] for a moment when none is counted.

   Four things differ between the systems: the lock, the wake (a non-blocking
   pipe, or a manual-reset event), the settings saved (termios, or the
   console's input mode), and how the reset bytes are written. */
struct reset {
  struct reset *next;
  size_t length;
  char bytes[];
};

struct holder {
  struct holder *next;
  intnat id;
  volatile sig_atomic_t active;
  volatile sig_atomic_t controlling;
  volatile sig_atomic_t pending[PENDING];
  volatile sig_atomic_t have_settings;
  struct reset *reset;
  int readers;
  struct reset *retired;
#ifdef _WIN32
  /* The wake, and the timer a wait with a deadline also waits for. */
  HANDLE wake, timer;
  HANDLE volatile in, out;
  DWORD settings;
  /* The text a read took from the console and has not given yet. */
  struct console_text text;
#else
  int wake_r, wake;
  volatile int in, out;
  struct termios settings;
#endif
};

static struct holder *holders = NULL;
static intnat holds = 0;

#ifdef _WIN32
static SRWLOCK holders_lock = SRWLOCK_INIT;
static void lock_holders(void) { AcquireSRWLockExclusive(&holders_lock); }
static void unlock_holders(void) { ReleaseSRWLockExclusive(&holders_lock); }
#else
static pthread_mutex_t holders_lock = PTHREAD_MUTEX_INITIALIZER;
static void lock_holders(void) { pthread_mutex_lock(&holders_lock); }
static void unlock_holders(void) { pthread_mutex_unlock(&holders_lock); }
#endif

static struct holder *holders_head(void) {
  return __atomic_load_n(&holders, __ATOMIC_ACQUIRE);
}

/* The holder [id], which must not be released. */
static struct holder *find(intnat id) {
  for (struct holder *h = holders_head(); h != NULL; h = h->next)
    if (__atomic_load_n(&h->id, __ATOMIC_ACQUIRE) == id &&
        __atomic_load_n(&h->active, __ATOMIC_ACQUIRE))
      return h;
  return NULL;
}

static struct holder *find_holder(value id) {
  struct holder *h = find(Long_val(id));
  if (h == NULL) caml_invalid_argument("Tty.Signal: a released holder");
  return h;
}

CAMLprim value matrix_tty_check(value id) {
  find_holder(id);
  return Val_unit;
}

/* Wakes [h]. A full pipe has a wake owed already. */
static void wake_holder(struct holder *h) {
#ifdef _WIN32
  SetEvent(h->wake);
#else
  char byte = 0;
  ssize_t n = write(h->wake, &byte, 1);
  (void)n;
#endif
}

#ifdef _WIN32

/* A new node's wake event and timer. The timer is a high-resolution one: a
   plain one ends its wait on the system's tick, 15.6 ms by default, so that
   frames due every 16.7 ms would come 15.6 and 31.2 ms apart. Windows has
   high-resolution timers since Windows 10 1803. */
static void make_wake(struct holder *h) {
  h->wake = CreateEventW(NULL, TRUE, FALSE, NULL);
  if (h->wake == NULL) {
    DWORD err = GetLastError();
    free(h);
    unlock_holders();
    caml_win32_maperr(err);
    caml_uerror("CreateEvent", Nothing);
  }
  h->timer = CreateWaitableTimerExW(
      NULL, NULL, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION, TIMER_ALL_ACCESS);
  if (h->timer == NULL) {
    CloseHandle(h->wake);
    free(h);
    unlock_holders();
    caml_failwith("Tty.Signal.hold: no high-resolution timer, which Windows "
                  "has since Windows 10 1803");
  }
}

static void empty_wake(struct holder *h) {
  ResetEvent(h->wake);
  memset(&h->text, 0, sizeof h->text);
}

static void ctrl_handler_held(int holding);

#else

/* A wake pipe: close-on-exec, so no child inherits it, and non-blocking at
   both ends. macOS has no pipe2(): there a fork by another thread between
   pipe() and fcntl() leaks the ends into that child. */
static int wake_pipe(int fds[2]) {
#ifdef __APPLE__
  if (pipe(fds) != 0) return -1;
  for (int i = 0; i < 2; i++)
    if (fcntl(fds[i], F_SETFD, FD_CLOEXEC) == -1 ||
        fcntl(fds[i], F_SETFL, fcntl(fds[i], F_GETFL) | O_NONBLOCK) == -1) {
      int err = errno;
      close(fds[0]);
      close(fds[1]);
      errno = err;
      return -1;
    }
  return 0;
#else
  return pipe2(fds, O_CLOEXEC | O_NONBLOCK);
#endif
}

static void make_wake(struct holder *h) {
  int fds[2];
  if (wake_pipe(fds) != 0) {
    int err = errno;
    free(h);
    unlock_holders();
    caml_unix_error(err, "pipe", Nothing);
  }
  h->wake_r = fds[0];
  h->wake = fds[1];
}

static void empty_wake(struct holder *h) {
  char buf[64];
  while (read(h->wake_r, buf, sizeof buf) > 0) {
  }
}

#endif

/* Registers a holder whose reset bytes go to [output], if [Some], and gives
   its id and, on POSIX, its wake pipe's read end, [(id, Some read)]. A node
   is reused once its holder has ended, with its wake, emptied: a wake is
   never closed, so a handler that wakes a holder as it ends never writes to
   a descriptor or sets a handle reused since. */
CAMLprim value matrix_tty_hold(value controlling, value output) {
  CAMLparam2(controlling, output);
  CAMLlocal2(result, wake);
  lock_holders();
  struct holder *h = holders_head();
  while (h != NULL && h->active) h = h->next;
  if (h == NULL) {
    h = calloc(1, sizeof *h);
    if (h == NULL) {
      unlock_holders();
      caml_raise_out_of_memory();
    }
    make_wake(h);
    h->next = holders;
    __atomic_store_n(&holders, h, __ATOMIC_RELEASE);
  } else
    empty_wake(h);
  for (int s = 0; s < PENDING; s++) h->pending[s] = 0;
  h->have_settings = 0;
  h->controlling = Bool_val(controlling);
#ifdef _WIN32
  h->in = INVALID_HANDLE_VALUE;
  h->out =
      Is_some(output) ? Handle_val(Some_val(output)) : INVALID_HANDLE_VALUE;
  ctrl_handler_held(1);
#else
  h->in = -1;
  h->out = Is_some(output) ? Int_val(Some_val(output)) : -1;
  int rd = h->wake_r;
#endif
  intnat id = holds++;
  __atomic_store_n(&h->id, id, __ATOMIC_RELEASE);
  __atomic_store_n(&h->active, 1, __ATOMIC_RELEASE);
  unlock_holders();
#ifdef _WIN32
  wake = Val_none;
#else
  wake = caml_alloc_some(Val_int(rd));
#endif
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_long(id));
  Store_field(result, 1, wake);
  CAMLreturn(result);
}

/* Publishes [r] as [h]'s reset bytes and retires the ones it replaces.
   Called under [holders_lock]. */
static void publish_reset(struct holder *h, struct reset *r) {
  struct reset *old = __atomic_exchange_n(&h->reset, r, __ATOMIC_SEQ_CST);
  if (old != NULL) {
    old->next = h->retired;
    h->retired = old;
  }
  if (__atomic_load_n(&h->readers, __ATOMIC_SEQ_CST) == 0) {
    while (h->retired != NULL) {
      struct reset *next = h->retired->next;
      free(h->retired);
      h->retired = next;
    }
  }
}

CAMLprim value matrix_tty_unhold(value id) {
  struct holder *h = find_holder(id);
  lock_holders();
  h->active = 0;
  h->have_settings = 0;
  publish_reset(h, NULL);
#ifdef _WIN32
  ctrl_handler_held(0);
#endif
  unlock_holders();
  return Val_unit;
}

/* Wakes the holder [id], unless it is released. Under the lock, its node
   cannot pass to the next holder meanwhile. */
CAMLprim value matrix_tty_wake(value id) {
  lock_holders();
  struct holder *h = find(Long_val(id));
  if (h != NULL) wake_holder(h);
  unlock_holders();
  return Val_unit;
}

/* Whether a signal that ends the process was received for the holder and
   not yet taken. */
CAMLprim value matrix_tty_terminating(value id) {
  struct holder *h = find_holder(id);
  return Val_bool(h->pending[P_INT] || h->pending[P_TERM] ||
                  h->pending[P_HUP] || h->pending[P_QUIT]);
}

/* Saves the settings of the holder's terminal on [input], restored if the
   process ends where no OCaml code runs. */
CAMLprim value matrix_tty_keep_settings(value id, value input) {
  struct holder *h = find_holder(id);
  h->have_settings = 0;
#ifdef _WIN32
  DWORD mode;
  if (Descr_kind_val(input) == KIND_SOCKET) return Val_unit;
  h->in = Handle_val(input);
  if (GetConsoleMode(h->in, &mode)) {
    h->settings = mode;
    h->have_settings = 1;
  }
#else
  struct termios t;
  h->in = Int_val(input);
  if (tcgetattr(h->in, &t) == 0) {
    h->settings = t;
    h->have_settings = 1;
  }
#endif
  return Val_unit;
}

CAMLprim value matrix_tty_forget_settings(value id) {
  struct holder *h = find_holder(id);
  h->have_settings = 0;
  return Val_unit;
}

/* The bytes that reset what the holder set; empty once it gave the terminal
   back. */
CAMLprim value matrix_tty_set_reset(value id, value bytes) {
  struct holder *h = find_holder(id);
  size_t length = caml_string_length(bytes);
  struct reset *r = NULL;
  if (length > 0) {
    r = malloc(sizeof *r + length);
    if (r == NULL) caml_raise_out_of_memory();
    r->next = NULL;
    r->length = length;
    memcpy(r->bytes, String_val(bytes), length);
  }
  lock_holders();
  publish_reset(h, r);
  unlock_holders();
  return Val_unit;
}

static void restore_holder(struct holder *h) {
  __atomic_add_fetch(&h->readers, 1, __ATOMIC_SEQ_CST);
  struct reset *r = __atomic_load_n(&h->reset, __ATOMIC_SEQ_CST);
#ifdef _WIN32
  HANDLE out = h->out;
  if (out != INVALID_HANDLE_VALUE && r != NULL) {
    size_t off = 0;
    while (off < r->length) {
      DWORD n = 0;
      if (!WriteFile(out, r->bytes + off, (DWORD)(r->length - off), &n, NULL) ||
          n == 0)
        break;
      off += n;
    }
  }
#else
  int out = h->out;
  if (out >= 0 && r != NULL) {
    size_t off = 0;
    while (off < r->length) {
      ssize_t n = write(out, r->bytes + off, r->length - off);
      if (n > 0)
        off += (size_t)n;
      else if (n < 0 && errno == EINTR)
        continue;
      else
        break;
    }
  }
#endif
  __atomic_sub_fetch(&h->readers, 1, __ATOMIC_SEQ_CST);
#ifdef _WIN32
  if (h->have_settings && h->in != INVALID_HANDLE_VALUE)
    SetConsoleMode(h->in, h->settings);
#else
  if (h->have_settings && h->in >= 0) tcsetattr(h->in, TCSANOW, &h->settings);
#endif
}

/* Restores every holder's terminal that has not been given back. */
static void restore_holders(void) {
  for (struct holder *h = holders_head(); h != NULL; h = h->next)
    if (h->active) restore_holder(h);
}

CAMLprim value matrix_tty_restore_all(value unit) {
  (void)unit;
  restore_holders();
  return Val_unit;
}

#ifdef _WIN32

/* The signals of the policy, by their index. */
static int in_policy(int p) {
  return p == P_INT || p == P_TERM || p == P_HUP || p == P_QUIT ||
         p == P_ABRT || p == P_TSTP || p == P_WINCH || p == P_CONT;
}

static int pending_index(value s) {
  int n = Int_val(s);
  if (n >= 0 || n <= -PENDING)
    caml_invalid_argument("Tty.Signal: signal number");
  return -n;
}

/* Records [p] for every holder and wakes it. The control handler calls it,
   on a thread of the system's, which touches no OCaml value. */
static void deliver(int p) {
  for (struct holder *h = holders_head(); h != NULL; h = h->next) {
    if (!h->active) continue;
    h->pending[p] = 1;
    SetEvent(h->wake);
  }
}

/* The takes of SIGINT and of SIGABRT, under [takers_lock]. */
static SRWLOCK takers_lock = SRWLOCK_INIT;
static volatile LONG sigint_takers = 0;
static int abort_takers = 0;

/* Whether Ctrl+Break would end the process if Matrix did not take it. From
   OCaml 5.0 to 5.5, Sys.signal sets the C runtime's SIGINT, and Ctrl+Break
   reaches the C runtime as SIGBREAK, which OCaml cannot name: it ends the
   process unless C code set SIGBREAK's action (ocaml/ocaml#15267). From
   5.6, OCaml takes Ctrl+Break as SIGINT. The action is read when a holder
   registers the handler and when SIGINT is taken. */
#if OCAML_VERSION < 50600
static volatile LONG break_is_default = 0;
static void read_break(void) {
  void (*old)(int) = signal(SIGBREAK, SIG_DFL);
  if (old != SIG_ERR) signal(SIGBREAK, old);
  break_is_default = old == SIG_DFL;
}
static int break_ours(void) { return break_is_default; }
#else
static void read_break(void) {}
static int break_ours(void) { return sigint_takers > 0; }
#endif

/* The console control handler. Ctrl+C reaches it only when the console
   processes input, as it does outside raw mode; Ctrl+Break always does.
   When the console closes, Windows ends the process as soon as a handler
   returns, and after 5 s whatever the handlers do, so the handler does not
   return: the holders give their terminals back and end the process
   meanwhile. Logoff and shutdown reach only services. */
static BOOL WINAPI on_ctrl(DWORD event) {
  switch (event) {
  case CTRL_C_EVENT:
    if (sigint_takers == 0) return FALSE;
    deliver(P_INT);
    return TRUE;
  case CTRL_BREAK_EVENT:
    if (!break_ours()) return FALSE;
    deliver(P_INT);
    return TRUE;
  case CTRL_CLOSE_EVENT:
    deliver(P_HUP);
    Sleep(INFINITE);
    return TRUE;
  default:
    return FALSE;
  }
}

/* The handler is registered while a holder is active. Called under
   [holders_lock]. */
static int held = 0;
static void ctrl_handler_held(int holding) {
  if (holding) {
    if (held++ == 0) {
      read_break();
      SetConsoleCtrlHandler(on_ctrl, TRUE);
    }
  } else if (--held == 0)
    SetConsoleCtrlHandler(on_ctrl, FALSE);
}

/* The C runtime's handler of SIGABRT. Once it returns, abort() ends the
   process. */
static void on_abort(int s) {
  (void)s;
  restore_holders();
  restore_consoles();
}

/* Takes [s] as the policy says. SIGINT is taken when [sigint_default],
   OCaml's action for it being the default, which C cannot read. */
CAMLprim value matrix_tty_take(value vs, value sigint_default) {
  int p = pending_index(vs);
  if (!in_policy(p))
    caml_invalid_argument("Tty.Signal.take: not a terminal's signal");
  int taken = 0;
  AcquireSRWLockExclusive(&takers_lock);
  if (p == P_INT && Bool_val(sigint_default)) {
    read_break();
    sigint_takers++;
    taken = 1;
  } else if (p == P_ABRT) {
    if (abort_takers > 0) {
      abort_takers++;
      taken = 1;
    } else {
      void (*old)(int) = signal(SIGABRT, on_abort);
      if (old == SIG_DFL) {
        abort_takers = 1;
        taken = 1;
      } else if (old != SIG_ERR)
        signal(SIGABRT, old);
    }
  }
  ReleaseSRWLockExclusive(&takers_lock);
  return Val_bool(taken);
}

CAMLprim value matrix_tty_ignored(value vs) {
  pending_index(vs);
  return Val_false;
}

CAMLprim value matrix_tty_give_back(value vs) {
  int p = pending_index(vs);
  AcquireSRWLockExclusive(&takers_lock);
  if (p == P_INT && sigint_takers > 0)
    sigint_takers--;
  else if (p == P_ABRT && abort_takers > 0 && --abort_takers == 0)
    signal(SIGABRT, SIG_DFL);
  ReleaseSRWLockExclusive(&takers_lock);
  return Val_unit;
}

CAMLprim value matrix_tty_received(value id, value vs) {
  int p = pending_index(vs);
  struct holder *h = find_holder(id);
  return Val_bool(__atomic_exchange_n(&h->pending[p], 0, __ATOMIC_SEQ_CST));
}

/* Ends the process as the signal would, after giving back the consoles
   enable_vt changed. The default control handler ends a process on Ctrl+C
   with STATUS_CONTROL_C_EXIT, the status cmd.exe prints ^C for.
   TerminateProcess runs no DLL's detach code, which could wait for a lock
   that a thread in an unknown state holds (ExitProcess's documentation), as
   raise() runs no exit code on POSIX. */
CAMLprim value matrix_tty_raise(value vs) {
  int p = pending_index(vs);
  if (p == P_INT || p == P_HUP || p == P_TERM || p == P_QUIT) {
    restore_consoles();
    TerminateProcess(GetCurrentProcess(), STATUS_CONTROL_C_EXIT);
  } else if (p == P_ABRT) {
    restore_consoles();
    signal(SIGABRT, SIG_DFL);
    raise(SIGABRT);
  }
  return Val_unit;
}

CAMLprim value matrix_tty_stop_group(value unit) {
  (void)unit;
  return Val_unit;
}

/* The wait. A console's handle is signaled while it holds records, which
   may be resizes or focus changes with no text, so there is something to
   read when it holds records or the holder holds text it took. The wake is
   reset only after the wait returns, so that the wake a read sets for a
   resize ends the next wait. A file is always readable. */
CAMLprim value matrix_tty_wait(value id, value input, value until) {
  struct holder *h = find_holder(id);
  HANDLE in = INVALID_HANDLE_VALUE;
  DWORD mode, count = 0;
  if (Is_some(input)) {
    value fd = Some_val(input);
    if (Descr_kind_val(fd) == KIND_SOCKET)
      caml_invalid_argument("Tty.Signal.wait: the input is a socket");
    in = Handle_val(fd);
    if (!GetConsoleMode(in, &mode)) {
      if (GetFileType(in) != FILE_TYPE_DISK)
        caml_invalid_argument(
            "Tty.Signal.wait: on Windows the input is a console or a file");
      ResetEvent(h->wake);
      return Val_true;
    }
    if (console_text_pending(&h->text)) {
      ResetEvent(h->wake);
      return Val_true;
    }
  }
  HANDLE handles[3];
  DWORD n = 0, timeout = INFINITE;
  int timed = 0;
  handles[n++] = h->wake;
  if (in != INVALID_HANDLE_VALUE) handles[n++] = in;
  if (Is_some(until)) {
    intnat left = Long_val(Some_val(until)) - now_ns();
    if (left <= 0)
      timeout = 0;
    else {
      LARGE_INTEGER due;
      due.QuadPart = -(LONGLONG)((left + 99) / 100);
      if (!SetWaitableTimer(h->timer, &due, 0, NULL, NULL, FALSE))
        raise_win32("SetWaitableTimer");
      handles[n++] = h->timer;
      timed = 1;
    }
  }
  caml_enter_blocking_section();
  DWORD r = WaitForMultipleObjects(n, handles, FALSE, timeout);
  DWORD err = GetLastError();
  caml_leave_blocking_section();
  if (timed) CancelWaitableTimer(h->timer);
  if (r == WAIT_FAILED) {
    caml_win32_maperr(err);
    caml_uerror("WaitForMultipleObjects", Nothing);
  }
  ResetEvent(h->wake);
  return Val_bool(in != INVALID_HANDLE_VALUE &&
                  GetNumberOfConsoleInputEvents(in, &count) && count > 0);
}

/* The record [in] as console_text.h takes it. The console gives an
   Alt+numpad character in its input code page (src/host/stream.cpp). */
static void record_of(const INPUT_RECORD *in, struct console_record *r) {
  memset(r, 0, sizeof *r);
  if (in->EventType == KEY_EVENT) {
    const KEY_EVENT_RECORD *k = &in->Event.KeyEvent;
    DWORD s = k->dwControlKeyState;
    r->kind = CONSOLE_KEY;
    r->down = k->bKeyDown;
    r->vk = k->wVirtualKeyCode;
    r->mods =
        (s & SHIFT_PRESSED ? CONSOLE_SHIFT : 0) |
        (s & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED) ? CONSOLE_CTRL : 0) |
        (s & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED) ? CONSOLE_ALT : 0);
    r->unit = k->uChar.UnicodeChar;
    r->repeat = k->wRepeatCount;
    if (!k->bKeyDown && k->wVirtualKeyCode == VK_MENU &&
        (s & ALTNUMPAD_BIT) && r->unit != 0) {
      char b[2];
      int len = 0;
      WCHAR w = 0;
      if (r->unit >> 8) b[len++] = (char)(r->unit >> 8);
      b[len++] = (char)(r->unit & 0xFF);
      if (MultiByteToWideChar(GetConsoleCP(), 0, b, len, &w, 1) == 1)
        r->unit = w;
    }
  } else if (in->EventType == WINDOW_BUFFER_SIZE_EVENT)
    r->kind = CONSOLE_RESIZE;
  else
    r->kind = CONSOLE_OTHER;
}

/* The read. On a console it looks at the records waiting, takes those whose
   text fits, and leaves the others; a record whose text does not fit whole
   is taken, and the rest kept for the next read. It never waits, given the
   count it reads, and never calls ReadFile, which drops the records that
   are not keys and waits while only those are there. A resize is received
   as SIGWINCH and wakes the holder. A file is read with ReadFile. */
CAMLprim value matrix_tty_read(value id, value fd, value buf, value voff,
                               value vlen) {
  struct holder *h = find_holder(id);
  HANDLE in = Handle_val(fd);
  size_t off = Long_val(voff), len = Long_val(vlen);
  DWORD mode;
  if (!GetConsoleMode(in, &mode)) {
    char tmp[4096];
    DWORD want = len < sizeof tmp ? (DWORD)len : (DWORD)sizeof tmp, got = 0;
    caml_enter_blocking_section();
    BOOL ok = ReadFile(in, tmp, want, &got, NULL);
    DWORD err = GetLastError();
    caml_leave_blocking_section();
    if (!ok) {
      if (err == ERROR_BROKEN_PIPE || err == ERROR_HANDLE_EOF)
        return Val_long(0);
      caml_win32_maperr(err);
      caml_uerror("ReadFile", Nothing);
    }
    memcpy(Bytes_val(buf) + off, tmp, got);
    return Val_long(got);
  }
  unsigned char *out = Bytes_val(buf) + off;
  unsigned zero_key = (unsigned)(VkKeyScanW(0) & 0xFFFF);
  size_t n = console_text_give(&h->text, out, len);
  int resized = 0;
  INPUT_RECORD recs[64];
  struct console_record records[64];
  while (n < len && !console_text_pending(&h->text)) {
    DWORD count = 0, got = 0, dropped = 0;
    size_t taken = 0;
    if (!GetNumberOfConsoleInputEvents(in, &count))
      raise_win32("GetNumberOfConsoleInputEvents");
    if (count == 0) break;
    if (!PeekConsoleInputW(in, recs, count < 64 ? count : 64, &got))
      raise_win32("PeekConsoleInput");
    for (DWORD i = 0; i < got; i++) record_of(&recs[i], &records[i]);
    n += console_text_read(&h->text, records, got, zero_key, out + n, len - n,
                           &taken, &resized);
    if (taken > 0 && !ReadConsoleInputW(in, recs, (DWORD)taken, &dropped))
      raise_win32("ReadConsoleInput");
  }
  if (resized) {
    h->pending[P_WINCH] = 1;
    SetEvent(h->wake);
  }
  return Val_long(n > 0 ? (intnat)n : -1);
}

#else

/* The policy, one per signal. SIGWINCH and SIGCONT are taken whatever their
   action, a handler installed before still called; the others only at
   their default action. A signal that ends the process resumes no system
   call it interrupts; the others resume them. */
static int in_policy(int s) {
  return s == SIGINT || s == SIGTERM || s == SIGHUP || s == SIGQUIT ||
         s == SIGABRT || s == SIGTSTP || s == SIGWINCH || s == SIGCONT;
}

static int taken_always(int s) { return s == SIGWINCH || s == SIGCONT; }

static int resumes(int s) {
  return s == SIGWINCH || s == SIGCONT || s == SIGTSTP || s == SIGABRT;
}

static int concerns_controlling_only(int s) {
  return s == SIGWINCH || s == SIGCONT || s == SIGTSTP;
}

static struct sigaction previous[PENDING];
static volatile sig_atomic_t ours[PENDING];

static void on_signal(int s, siginfo_t *info, void *context) {
  int saved_errno = errno;
  if (s == SIGABRT) {
    restore_holders();
    /* Blocked while this handler runs, the signal raised again is delivered
       with its default action once the handler returns. */
    struct sigaction dfl;
    memset(&dfl, 0, sizeof dfl);
    dfl.sa_handler = SIG_DFL;
    sigemptyset(&dfl.sa_mask);
    sigaction(SIGABRT, &dfl, NULL);
    raise(SIGABRT);
  } else if (taken_always(s)) {
    /* A handler installed before is still called. */
    struct sigaction *p = &previous[s];
    if (p->sa_flags & SA_SIGINFO) {
      if (p->sa_sigaction != NULL) p->sa_sigaction(s, info, context);
    } else if (p->sa_handler != SIG_DFL && p->sa_handler != SIG_IGN) {
      p->sa_handler(s);
    }
  }
  if (s > 0 && s < PENDING) {
    for (struct holder *h = holders_head(); h != NULL; h = h->next) {
      if (!h->active) continue;
      if (concerns_controlling_only(s) && !h->controlling) continue;
      h->pending[s] = 1;
      wake_holder(h);
    }
  }
  errno = saved_errno;
}

static int system_signal(value s) {
  int n = caml_convert_signal_number(Int_val(s));
  if (n <= 0 || n >= PENDING)
    caml_invalid_argument("Tty.Signal: signal number");
  return n;
}

/* Takes [s] as the policy says. Holders side by side take a signal
   together: the first installs the handler, and the action it replaced
   comes back when the last gives it back. Every action is read here, so
   OCaml's word on SIGINT's is not needed. */
static int takers[PENDING];
static pthread_mutex_t takers_lock = PTHREAD_MUTEX_INITIALIZER;

CAMLprim value matrix_tty_take(value vs, value sigint_default) {
  (void)sigint_default;
  int s = system_signal(vs);
  if (!in_policy(s))
    caml_invalid_argument("Tty.Signal.take: not a terminal's signal");
  struct sigaction old, sa;
  int taken = 0;
  pthread_mutex_lock(&takers_lock);
  if (ours[s]) {
    takers[s]++;
    taken = 1;
  } else if (sigaction(s, NULL, &old) == 0) {
    int is_default = !(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_DFL;
    if (taken_always(s) || is_default) {
      memset(&sa, 0, sizeof sa);
      sa.sa_sigaction = on_signal;
      sigemptyset(&sa.sa_mask);
      sa.sa_flags = SA_SIGINFO | (resumes(s) ? SA_RESTART : 0);
      previous[s] = old;
      ours[s] = 1;
      if (sigaction(s, &sa, NULL) == 0) {
        takers[s] = 1;
        taken = 1;
      } else
        ours[s] = 0;
    }
  }
  pthread_mutex_unlock(&takers_lock);
  return Val_bool(taken);
}

/* Whether the action of [s] is to ignore it. */
CAMLprim value matrix_tty_ignored(value vs) {
  int s = system_signal(vs);
  struct sigaction old;
  if (sigaction(s, NULL, &old) != 0) return Val_false;
  return Val_bool(!(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_IGN);
}

/* Gives [s] back: the action it had before it was taken comes back once no
   holder holds it. */
CAMLprim value matrix_tty_give_back(value vs) {
  int s = system_signal(vs);
  pthread_mutex_lock(&takers_lock);
  if (ours[s] && --takers[s] == 0) {
    ours[s] = 0;
    sigaction(s, &previous[s], NULL);
  }
  pthread_mutex_unlock(&takers_lock);
  return Val_unit;
}

/* Whether [s] was received for the holder since the last call. The record
   is read and cleared in one exchange: a handler that sets it between a
   read and a clear would be cleared unseen. */
CAMLprim value matrix_tty_received(value id, value vs) {
  int s = system_signal(vs);
  struct holder *h = find_holder(id);
  return Val_bool(__atomic_exchange_n(&h->pending[s], 0, __ATOMIC_SEQ_CST));
}

static void unblock(int s) {
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, s);
  pthread_sigmask(SIG_UNBLOCK, &set, NULL);
}

/* Raises [s] with its default action. */
CAMLprim value matrix_tty_raise(value vs) {
  int s = system_signal(vs);
  struct sigaction dfl;
  memset(&dfl, 0, sizeof dfl);
  dfl.sa_handler = SIG_DFL;
  sigemptyset(&dfl.sa_mask);
  sigaction(s, &dfl, NULL);
  ours[s] = 0;
  takers[s] = 0;
  unblock(s);
  raise(s);
  return Val_unit;
}

/* Sends SIGTSTP to the process's group, as the terminal's suspend character
   does. */
CAMLprim value matrix_tty_stop_group(value unit) {
  (void)unit;
  unblock(SIGTSTP);
  kill(0, SIGTSTP);
  return Val_unit;
}

/* A console's wait and read, which POSIX does in OCaml, over the wake
   pipe. */
CAMLprim value matrix_tty_wait(value id, value input, value until) {
  (void)id;
  (void)input;
  (void)until;
  caml_failwith("Tty.Signal.wait: no console on this system");
}

CAMLprim value matrix_tty_read(value id, value fd, value buf, value off,
                               value len) {
  (void)id;
  (void)fd;
  (void)buf;
  (void)off;
  (void)len;
  caml_failwith("Tty.Signal.read: no console on this system");
}

#endif
