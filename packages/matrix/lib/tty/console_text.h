/* The text a Windows console's input records carry, as the UTF-8 bytes a run
   decodes. It uses no Windows type, so that a test feeds it records on any
   system: the Windows read turns each INPUT_RECORD into a [console_record].

   In virtual terminal input mode, conhost and ConPTY store every key,
   mouse report, paste and focus report as key-down records of one UTF-16
   unit each, repeat 1 (microsoft/terminal, src/host/inputBuffer.cpp,
   _writeString). Older consoles stored key records as typed, with a repeat
   count and the Alt+numpad character on the Alt key's release. A record
   carries text when conhost's own reader of characters takes it
   (src/host/stream.cpp, GetChar): a key-down with a character, a key-down
   that is the key the layout types NUL with, and a release of Alt with a
   character. */

#ifndef MATRIX_CONSOLE_TEXT_H
#define MATRIX_CONSOLE_TEXT_H

#include <stddef.h>

enum console_kind { CONSOLE_KEY, CONSOLE_RESIZE, CONSOLE_OTHER };

#define CONSOLE_SHIFT 0x1
#define CONSOLE_CTRL 0x2
#define CONSOLE_ALT 0x4
#define CONSOLE_VK_MENU 0x12

struct console_record {
  enum console_kind kind;
  int down;
  unsigned vk;
  unsigned mods;
  unsigned unit;
  unsigned repeat;
};

/* What a reader has taken but not yet given: a high surrogate waiting for
   its low one, bytes to give once, and a character to give [count] more
   times, from [unit_off] in its first copy. */
struct console_text {
  unsigned high;
  unsigned char once[8];
  int once_len, once_off;
  unsigned char unit[4];
  int unit_len, unit_off;
  unsigned count;
};

static int console_utf8(unsigned cp, unsigned char *b) {
  if (cp < 0x80) {
    b[0] = (unsigned char)cp;
    return 1;
  }
  if (cp < 0x800) {
    b[0] = (unsigned char)(0xC0 | (cp >> 6));
    b[1] = (unsigned char)(0x80 | (cp & 0x3F));
    return 2;
  }
  if (cp < 0x10000) {
    b[0] = (unsigned char)(0xE0 | (cp >> 12));
    b[1] = (unsigned char)(0x80 | ((cp >> 6) & 0x3F));
    b[2] = (unsigned char)(0x80 | (cp & 0x3F));
    return 3;
  }
  b[0] = (unsigned char)(0xF0 | (cp >> 18));
  b[1] = (unsigned char)(0x80 | ((cp >> 12) & 0x3F));
  b[2] = (unsigned char)(0x80 | ((cp >> 6) & 0x3F));
  b[3] = (unsigned char)(0x80 | (cp & 0x3F));
  return 4;
}

/* Whether [t] has bytes to give. A high surrogate alone is no text yet. */
static int console_text_pending(const struct console_text *t) {
  return t->once_off < t->once_len || t->count > 0;
}

/* Gives at most [room] of the bytes [t] holds to [out], and says how many. */
static size_t console_text_give(struct console_text *t, unsigned char *out,
                                size_t room) {
  size_t n = 0;
  while (n < room && t->once_off < t->once_len)
    out[n++] = t->once[t->once_off++];
  if (t->once_off == t->once_len) t->once_off = t->once_len = 0;
  while (n < room && t->count > 0) {
    out[n++] = t->unit[t->unit_off++];
    if (t->unit_off == t->unit_len) {
      t->unit_off = 0;
      t->count--;
    }
  }
  return n;
}

/* Whether the key record [r] carries text, given [zero_key], what
   VkKeyScanW(0) is: the key in its low byte, and the Shift, Ctrl and Alt
   it needs in the bits 0x100, 0x200 and 0x400. */
static int console_has_text(const struct console_record *r,
                            unsigned zero_key) {
  if (r->unit != 0) return r->down || r->vk == CONSOLE_VK_MENU;
  return r->down && r->vk == (zero_key & 0xFF) &&
         !(r->mods & CONSOLE_SHIFT) == !(zero_key & 0x100) &&
         !(r->mods & CONSOLE_CTRL) == !(zero_key & 0x200) &&
         !(r->mods & CONSOLE_ALT) == !(zero_key & 0x400);
}

/* Takes the record [r] into [t], which holds no bytes to give, and says
   whether [r] reports a resize. An unpaired surrogate is U+FFFD. */
static int console_text_take(struct console_text *t,
                             const struct console_record *r,
                             unsigned zero_key) {
  if (r->kind == CONSOLE_RESIZE) return 1;
  if (r->kind != CONSOLE_KEY || !console_has_text(r, zero_key)) return 0;
  unsigned u = r->unit, cp;
  unsigned repeat = r->repeat == 0 ? 1 : r->repeat;
  if (u >= 0xD800 && u < 0xDC00) {
    if (t->high != 0)
      t->once_len += console_utf8(0xFFFD, t->once + t->once_len);
    t->high = u;
    return 0;
  }
  if (u >= 0xDC00 && u < 0xE000) {
    cp = t->high != 0 ? 0x10000 + ((t->high - 0xD800) << 10) + (u - 0xDC00)
                      : 0xFFFD;
    t->high = 0;
    repeat = 1;
  } else {
    if (t->high != 0) {
      t->once_len += console_utf8(0xFFFD, t->once + t->once_len);
      t->high = 0;
    }
    cp = u;
  }
  t->once_len += console_utf8(cp, t->once + t->once_len);
  if (repeat > 1) {
    t->unit_len = console_utf8(cp, t->unit);
    t->unit_off = 0;
    t->count = repeat - 1;
  }
  return 0;
}

/* A read of the [count] records [recs] waiting in the console, into [room]
   bytes of [out]: it gives what [t] holds, then takes records while their
   text fits, the one that does not fit whole included, whose rest [t] keeps.
   It is the number of bytes given, sets [*taken] to the number of records
   taken, which the console must then drop, and [*resized] if one of them
   reports a resize. */
static size_t console_text_read(struct console_text *t,
                                const struct console_record *recs,
                                size_t count, unsigned zero_key,
                                unsigned char *out, size_t room, size_t *taken,
                                int *resized) {
  size_t n = console_text_give(t, out, room), i = 0;
  while (i < count && n < room && !console_text_pending(t)) {
    *resized |= console_text_take(t, &recs[i++], zero_key);
    n += console_text_give(t, out + n, room - n);
  }
  *taken = i;
  return n;
}

#endif
