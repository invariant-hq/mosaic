#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32
#include <io.h>  // For _get_osfhandle
#include <windows.h>
#else
#include <sys/ioctl.h>
#include <unistd.h>
#endif

CAMLprim value terminal_get_size(value fd_val) {
  CAMLparam1(fd_val);
  CAMLlocal1(result);
  int fd = Int_val(fd_val);
  int width = 80, height = 24;

#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE h = (HANDLE)_get_osfhandle(fd);
  if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
    width = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    height = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
  }
#else
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
    width = ws.ws_col;
    height = ws.ws_row;
  }
#endif

  if (width <= 0 || height <= 0) {
    width = 80;
    height = 24;
  }

  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(width));
  Store_field(result, 1, Val_int(height));
  CAMLreturn(result);
}

CAMLprim value terminal_enable_vt(value fd_val) {
  CAMLparam1(fd_val);
#ifdef _WIN32
  int fd = Int_val(fd_val);
  HANDLE h = (HANDLE)_get_osfhandle(fd);
  if (h == INVALID_HANDLE_VALUE) {
    caml_failwith("Invalid handle");
  }
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) {
    caml_failwith("GetConsoleMode failed");
  }
  // Enable VT processing and UTF-8 support
  if (!SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
    caml_failwith("SetConsoleMode failed");
  }
  // Enable UTF-8 output for better Unicode support.
  // Note: This affects the entire process, not just this terminal handle.
  SetConsoleOutputCP(CP_UTF8);
#endif
  CAMLreturn(Val_unit);
}
