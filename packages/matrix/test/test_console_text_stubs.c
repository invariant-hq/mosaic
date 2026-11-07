/* The reads a Windows console's records give, run on any system over
   records the test makes. */

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>

#include "console_text.h"

/* [records] holds six ints a record: its kind, whether it is a key-down,
   its virtual key, its modifiers, its UTF-16 unit and its repeat count. The
   result is each read's bytes, the last read first, and whether a resize
   was read. Reads go on until no record is left and none gives a byte. */
value test_console_reads(value records, value zero_key, value room) {
  CAMLparam3(records, zero_key, room);
  CAMLlocal3(reads, read, result);
  size_t count = Wosize_val(records) / 6, len = Long_val(room), at = 0;
  struct console_record *recs = calloc(count + 1, sizeof *recs);
  unsigned char *out = malloc(len + 1);
  struct console_text t = {0};
  int resized = 0;
  for (size_t i = 0; i < count; i++) {
    recs[i].kind = (enum console_kind)Long_val(Field(records, 6 * i));
    recs[i].down = Long_val(Field(records, 6 * i + 1));
    recs[i].vk = Long_val(Field(records, 6 * i + 2));
    recs[i].mods = Long_val(Field(records, 6 * i + 3));
    recs[i].unit = Long_val(Field(records, 6 * i + 4));
    recs[i].repeat = Long_val(Field(records, 6 * i + 5));
  }
  reads = Val_emptylist;
  for (;;) {
    size_t taken = 0;
    size_t n = console_text_read(&t, recs + at, count - at, Long_val(zero_key),
                                 out, len, &taken, &resized);
    at += taken;
    if (n == 0 && taken == 0) break;
    read = caml_alloc_initialized_string(n, (const char *)out);
    value cell = caml_alloc_small(2, Tag_cons);
    Field(cell, 0) = read;
    Field(cell, 1) = reads;
    reads = cell;
  }
  free(recs);
  free(out);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, reads);
  Store_field(result, 1, Val_bool(resized));
  CAMLreturn(result);
}
