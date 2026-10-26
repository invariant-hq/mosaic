/* The call that makes a terminal the controlling terminal of the calling
   session leader, which OCaml's Unix library lacks. */

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

value test_tty_set_controlling(value fd) {
  if (ioctl(Int_val(fd), TIOCSCTTY, 0) != 0) uerror("ioctl", Nothing);
  return Val_unit;
}

/* Puts the calling process in a process group of its own, which OCaml's Unix
   library cannot either. */
value test_tty_new_group(value unit) {
  (void)unit;
  if (setpgid(0, 0) != 0) uerror("setpgid", Nothing);
  return Val_unit;
}
