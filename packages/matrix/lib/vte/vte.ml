module Grid = Matrix_grid
module Text = Matrix_text
module Params = Ansi.Parser.Params
module Reply = Matrix_input.Reply

(* {1:state State} *)

(* [last_column] is DEC's Last Column Flag: the cursor is on the character just
   printed in the last column, and the next one wraps first. It implies
   [col = cols - 1] and autowrap on. Only [set_cursor] changes the cursor, for
   [goto] and printing; a saved cursor is a copy, never changed. *)
type cursor = {
  mutable row : int;
  mutable col : int;
  mutable last_column : bool;
}

(* [style] is the SGR rendition, never with a link: OSC 8 sets [link]. *)
type pen = { style : Ansi.Style.t; link : string option; protected : bool }

(* A 94-character set: ASCII, or the code points that replace 0x20..0x7F; a
   national replacement set only replaces them in national mode. *)
type charset = Ascii | Mapped of { table : int array; national : bool }

(* [g] is never mutated: a designation makes a new array. [single] is the set a
   single shift invokes for the next character. *)
type charsets = { g : charset array; gl : int; single : int option }

(* What DECSC saves. *)
type saved = { cursor : cursor; pen : pen; charsets : charsets; origin : bool }

(* A screen's cells, the wrap mark of each row (a byte), and its DECSC slot.
   The screen functions below are the only ones that change [grid] and
   [wrapped]. *)
type screen = {
  grid : Grid.t;
  mutable wrapped : Bytes.t;
  mutable saved : saved;
}

type margins = { top : int; bottom : int }

type modes = {
  insert : bool;
  new_line : bool;
  origin : bool;
  auto_wrap : bool;
  reverse_wrap : bool;
  cursor_keys : bool;
  cursor_visible : bool;
  bracketed_paste : bool;
  alternate_screen : bool;
  national : bool;
  keypad : bool;
  focus_reports : bool;
  synchronized_output : bool;
  color_scheme_reports : bool;
  resize_reports : bool;
}

type identity = {
  attributes : int list;
  secondary : int list;
  version : string option;
}

type extension =
  | Kitty_keyboard
  | Modify_other_keys
  | Synchronized_output
  | Grapheme_clusters
  | Color_scheme_reports
  | In_band_resize
  | Focus_reports
  | Kitty_graphics

type request =
  | Bell
  | Notification of { title : string; body : string }
  | Clipboard of { selection : string; data : string }

(* xterm keeps one mouse tracking mode and one format: setting one replaces
   the other, and resetting it turns tracking off. *)
type tracking = [ `Off | `X10 | `Normal | `Button | `Any ]
type mouse_format = [ `X10 | `Sgr | `Urxvt ]

(* A scrollback line: the text of its graphemes; in [cells], the byte length
   and the width of each, in order, as two varints, so that each comes back
   whole at the column it left; the rendition of each run of them from
   [start_byte]; and its wrap mark. *)
type style_run = {
  start_byte : int;
  fg : Ansi.Color.t;
  bg : Ansi.Color.t;
  attrs : Ansi.Attr.t;
  link : string;
}

type line = {
  text : string;
  cells : string;
  styles : style_run array;
  wrapped : bool;
}

(* A ring of the newest lines: [head] is where the next one goes. *)
type history = { lines : line array; mutable head : int; mutable count : int }

(* The display unit in progress, the text printed since the last unit boundary
   or control function: its bytes after translation, its width as the
   segmenter measures it, and whether it [grew] since it was last placed.
   Placed, it covers [cells] cells from [(row, col)]; joined, having no width,
   its bytes follow those of the grapheme at [(row, col)], whose text was
   [prev]. *)
type placement = Nowhere | Cells | Joined

type cluster = {
  seg : Text.Segmenter.t;
  mutable text : Bytes.t;
  mutable len : int;
  mutable width : int;
  mutable grown : bool;
  mutable at : placement;
  mutable row : int;
  mutable col : int;
  mutable cells : int;
  mutable prev : string;
}

type palette = {
  colors : (int * int * int) array;
  foreground : int * int * int;
  background : int * int * int;
  cursor : int * int * int;
}

(* An OSC is kept only when the VTE performs it: [Osc_number n] while its
   number is read (-1 before the first digit), [Osc_text n] once it is known
   to be kept, and [Osc_skip] otherwise. *)
type osc = Osc_skip | Osc_number of int | Osc_text of int

(* A kitty graphics command, an APC that starts with [G]: its keys while
   they are read, then its payload, of which only the length is kept. *)
type apc = Apc_skip | Apc_keys of Buffer.t | Apc_payload of string * int

(* A kitty image in transmission: its id, the bytes its keys promise, the
   payload's length so far, and its virtual placement, if any. *)
type transmission = {
  image : int;
  expected : int;
  mutable payload : int;
  virtual_placement : bool;
}

(* The rows changed, a byte a row. *)
type dirty = {
  mutable any : bool;
  mutable cursor : bool;
  mutable rows : Bytes.t;
}

type t = {
  mutable rows : int;
  mutable cols : int;
  primary : screen;
  alternate : screen;
  history : history;
  cursor : cursor;
  (* DECSCUSR's style: its shape, and whether it blinks. *)
  mutable cursor_style : Ansi.cursor_style * bool;
  mutable pen : pen;
  mutable charsets : charsets;
  mutable modes : modes;
  mutable margins : margins;
  mutable tabs : bool array;
  mutable title : string;
  cluster : cluster;
  (* The palette [create] was given, and the one programs set. Neither's
     colours are ever written: a change makes a new array. *)
  initial : palette;
  mutable palette : palette;
  parser : Ansi.Parser.t;
  mutable osc : osc;
  osc_text : Buffer.t;
  (* The replies owed while [feed] runs, the latest first; empty between
     calls. *)
  mutable replies : Reply.t list;
  dirty : dirty;
  identity : identity;
  extensions : extension list;
  (* The kitty keyboard protocol's stacks of flags, the current first, one
     for each screen. *)
  mutable kitty_primary : int list;
  mutable kitty_alternate : int list;
  mutable modify_other_keys : int;
  mutable tracking : tracking;
  mutable mouse_format : mouse_format;
  mutable color_scheme : [ `Dark | `Light ];
  (* The requests to the host, the latest first. *)
  mutable requests : request list;
  (* The cell's size in pixels, which CSI 16 t and 14 t report. *)
  cell_size : (int * int) option;
  (* Kitty graphics: the command being read, the transmission in progress,
     and the images held, each with whether it has a virtual placement. *)
  mutable apc : apc;
  mutable transmission : transmission option;
  mutable images : (int * bool) list;
}

let top_left = { row = 0; col = 0; last_column = false }
let plain_pen = { style = Ansi.Style.default; link = None; protected = false }
let ascii_sets = { g = [| Ascii; Ascii; Ascii; Ascii |]; gl = 0; single = None }

let power_up =
  { cursor = top_left; pen = plain_pen; charsets = ascii_sets; origin = false }

let power_up_modes =
  {
    insert = false;
    new_line = false;
    origin = false;
    auto_wrap = true;
    reverse_wrap = false;
    cursor_keys = false;
    cursor_visible = true;
    bracketed_paste = false;
    alternate_screen = false;
    national = false;
    keypad = false;
    focus_reports = false;
    synchronized_output = false;
    color_scheme_reports = false;
    resize_reports = false;
  }

(* A VT100 with Advanced Video, and DA2's type 0, version 0, no options. *)
let vt100 = { attributes = [ 1; 2 ]; secondary = [ 0; 0; 0 ]; version = None }

let extensions =
  [
    Kitty_keyboard;
    Modify_other_keys;
    Synchronized_output;
    Grapheme_clusters;
    Color_scheme_reports;
    In_band_resize;
    Focus_reports;
    Kitty_graphics;
  ]

let has t e = List.mem e t.extensions

(* A blinking block, DECSCUSR's default (VT510 DECSCUSR). *)
let power_up_cursor_style = (`Block, true)

(* Tab stops every eight columns, as at power-up. *)
let default_tabs cols = Array.init cols (fun i -> i > 0 && i mod 8 = 0)
let empty_line = { text = ""; cells = ""; styles = [||]; wrapped = false }
let screen t = if t.modes.alternate_screen then t.alternate else t.primary
let grid t = (screen t).grid
let full_margins t = { top = 0; bottom = t.rows - 1 }

(* Cells keep the default colours as such: a renderer resolves them through
   the palette. *)
let default_fg = Ansi.Color.default_fg
let default_bg = Ansi.Color.default_bg

(* [Some default_bg], made once: an optional argument allocates. *)
let default_bg_opt = Some default_bg

(* A colour of the pen, or the default for none and for SGR 39/49. *)
let resolve ~default = function
  | None -> default
  | Some c -> (
      match Ansi.Color.intent c with Default -> default | Indexed _ | Rgb -> c)

let fg t = resolve ~default:default_fg t.pen.style.fg

(* The background of what is printed, and of every blank. *)
let bg t = resolve ~default:default_bg t.pen.style.bg
let protection t = if t.pen.protected then Some true else None

(* {1:dirty Changes} *)

let clear_dirty t =
  t.dirty.any <- false;
  t.dirty.cursor <- false;
  Bytes.fill t.dirty.rows 0 t.rows '\000'

let touch_row t row =
  t.dirty.any <- true;
  Bytes.unsafe_set t.dirty.rows row '\001'

let touch_rows t top bottom =
  if top <= bottom then (
    t.dirty.any <- true;
    Bytes.fill t.dirty.rows top (bottom - top + 1) '\001')

let set_cursor t ~row ~col ~last_column =
  let c = t.cursor in
  if row <> c.row || col <> c.col then (
    t.dirty.any <- true;
    t.dirty.cursor <- true);
  c.row <- row;
  c.col <- col;
  c.last_column <- last_column

(* Every function that sets the cursor position does it here, so it clears
   the Last Column Flag. *)
let goto t ~row ~col =
  let row = max 0 (min row (t.rows - 1))
  and col = max 0 (min col (t.cols - 1)) in
  set_cursor t ~row ~col ~last_column:false

(* {1:history Scrollback} *)

(* LEB128: seven bits a byte, the low ones first. *)
let rec add_varint b n =
  if n < 0x80 then Buffer.add_char b (Char.unsafe_chr n)
  else (
    Buffer.add_char b (Char.unsafe_chr (n land 0x7F lor 0x80));
    add_varint b (n lsr 7))

let read_varint s i =
  let n = ref 0 and shift = ref 0 and more = ref true in
  while !more do
    let c = Char.code s.[!i] in
    incr i;
    n := !n lor ((c land 0x7F) lsl !shift);
    shift := !shift + 7;
    more := c >= 0x80
  done;
  !n

(* A cell shows nothing when it is a space with no attribute and no link, on
   the terminal's background or a transparent one. *)
let shows_nothing g idx =
  let cp = Grid.get_codepoint g idx and bg = Grid.get_bg g idx in
  (cp = 0x20 || cp = 0)
  && Grid.get_attrs g idx = 0
  && Grid.get_link g idx = ""
  && (Ansi.Color.Packed.alpha (Ansi.Color.Packed.encode bg) = 0
     || Ansi.Color.equal bg default_bg)

(* The row up to its last cell that shows something. Styles are stored only
   when they change. *)
let compress_row t (s : screen) row =
  let g = s.grid and base = row * t.cols in
  let last = ref (t.cols - 1) in
  while !last >= 0 && shows_nothing g (base + !last) do
    decr last
  done;
  let text = Buffer.create t.cols and cells = Buffer.create (2 * t.cols) in
  let styles = ref [] in
  for idx = base to base + !last do
    if not (Grid.is_continuation g idx) then begin
      let grapheme = if Grid.is_empty g idx then " " else Grid.get_text g idx in
      let fg = Grid.get_fg g idx and bg = Grid.get_bg g idx in
      let attrs = Ansi.Attr.unpack (Grid.get_attrs g idx) in
      let link = Grid.get_link g idx in
      (match !styles with
      | r :: _
        when Ansi.Color.equal r.fg fg && Ansi.Color.equal r.bg bg
             && Ansi.Attr.equal r.attrs attrs
             && r.link = link ->
          ()
      | _ ->
          styles :=
            { start_byte = Buffer.length text; fg; bg; attrs; link } :: !styles);
      Buffer.add_string text grapheme;
      add_varint cells (String.length grapheme);
      add_varint cells (max 1 (Grid.cell_width g idx))
    end
  done;
  {
    text = Buffer.contents text;
    cells = Buffer.contents cells;
    styles = Array.of_list (List.rev !styles);
    wrapped = Bytes.get s.wrapped row <> '\000';
  }

(* Write [line] on row [row] of [grid]: each grapheme as it was, at the column
   it left, as far as the grid is wide. The result is the columns it covers. *)
let decompress_line line grid row =
  let cell = ref 0 and x = ref 0 and byte = ref 0 and covered = ref 0 in
  let width = Grid.width grid in
  let runs = Array.length line.styles in
  for k = 0 to runs - 1 do
    let r = line.styles.(k) in
    let stop =
      if k + 1 < runs then line.styles.(k + 1).start_byte
      else String.length line.text
    in
    let link = if r.link = "" then None else Some r.link in
    while !byte < stop do
      let len = read_varint line.cells cell in
      let w = read_varint line.cells cell in
      if !x + w <= width then (
        Grid.set_grapheme grid ~x:!x ~y:row ~width:w ~fg:r.fg ~bg:r.bg
          ~attrs:r.attrs ?link line.text ~pos:!byte ~len;
        covered := !x + w);
      x := !x + w;
      byte := !byte + len
    done
  done;
  !covered

let push_line t s row =
  let h = t.history in
  let capacity = Array.length h.lines in
  if capacity > 0 then (
    h.lines.(h.head) <- compress_row t s row;
    h.head <- (h.head + 1) mod capacity;
    h.count <- min capacity (h.count + 1))

(* The [i]th newest line, [i < count]. *)
let newest h i =
  let capacity = Array.length h.lines in
  h.lines.((h.head - 1 - i + capacity) mod capacity)

let pop_line h =
  let line = newest h 0 in
  let capacity = Array.length h.lines in
  h.lines.((h.head - 1 + capacity) mod capacity) <- empty_line;
  h.head <- (h.head - 1 + capacity) mod capacity;
  h.count <- h.count - 1;
  line

let clear_history t =
  let h = t.history in
  Array.fill h.lines 0 (Array.length h.lines) empty_line;
  h.head <- 0;
  h.count <- 0

(* {1:screen Changing the screen}

   Every change of cells goes through these functions, and each clears the
   Last Column Flag: with [goto], that is the rule "every function that sets
   the cursor position or changes cells clears it". Printing sets the flag
   after its changes. Every blank is on the pen's background. *)

(* Rows [top] to [bottom] changed: they are dirty, and the Last Column Flag
   goes. *)
let cells_changed t ~top ~bottom =
  touch_rows t top bottom;
  t.cursor.last_column <- false

let unwrap t ~top ~bottom =
  Bytes.fill (screen t).wrapped top (bottom - top + 1) '\000'

(* A wrap mark asserts that the row's last cell is followed by the next row,
   so an erase that reaches the last column clears it. *)
let erase_with f t ~x ~y ~width ~height =
  if width > 0 && height > 0 then (
    f (grid t) { Grid.x; y; width; height };
    if x + width = t.cols then unwrap t ~top:y ~bottom:(y + height - 1);
    cells_changed t ~top:y ~bottom:(y + height - 1))

let erase t =
  erase_with
    (fun g { Grid.x; y; width; height } ->
      Grid.clear_rect ~color:(bg t) g ~x ~y ~width ~height)
    t

let erase_unprotected = erase_with Grid.erase_unprotected

(* Move rows [top] to [bottom] up by [n] rows, or down for [n < 0], with
   their marks. *)
let shift_rows t ~top ~bottom n =
  let height = bottom - top + 1 in
  let k = min (abs n) height in
  Grid.shift (grid t)
    { Grid.x = 0; y = top; width = t.cols; height }
    ~dx:0 ~dy:(-n) ~color:(bg t);
  let marks = (screen t).wrapped in
  if n > 0 then (
    Bytes.blit marks (top + k) marks top (height - k);
    Bytes.fill marks (bottom - k + 1) k '\000')
  else (
    Bytes.blit marks top marks (top + k) (height - k);
    Bytes.fill marks top k '\000');
  cells_changed t ~top ~bottom

(* Move the cells of rows [top] to [bottom] from column [x] by [dx] columns.
   The rows' last cells change, so their marks go. *)
let shift_cols t ~x ~top ~bottom dx =
  if dx <> 0 then (
    Grid.shift (grid t)
      { Grid.x; y = top; width = t.cols - x; height = bottom - top + 1 }
      ~dx ~dy:0 ~color:(bg t);
    unwrap t ~top ~bottom;
    cells_changed t ~top ~bottom)

let mark_wrapped t row = Bytes.set (screen t).wrapped row '\001'

(* {1:scroll Scrolling} *)

(* Rows leaving the top of a scroll region that starts at the first row of the
   primary screen enter the scrollback. *)
let scroll_up t n =
  let { top; bottom } = t.margins in
  let n = min n (bottom - top + 1) in
  if top = 0 && not t.modes.alternate_screen then
    for row = 0 to n - 1 do
      push_line t t.primary row
    done;
  shift_rows t ~top ~bottom n

let scroll_down t n =
  let { top; bottom } = t.margins in
  shift_rows t ~top ~bottom (-min n (bottom - top + 1))

(* IND: down a row, scrolling at the bottom margin. Below the region the
   cursor stops at the last row. *)
let index t =
  if t.cursor.row = t.margins.bottom then scroll_up t 1
  else goto t ~row:(t.cursor.row + 1) ~col:t.cursor.col

let reverse_index t =
  if t.cursor.row = t.margins.top then scroll_down t 1
  else goto t ~row:(t.cursor.row - 1) ~col:t.cursor.col

(* Autowrap: the text goes on at the start of the next row, which the row
   records. At the last row below the region there is no next row: the line
   starts over on the same row. *)
let wrap t =
  let row = t.cursor.row in
  if row = t.margins.bottom || row < t.rows - 1 then mark_wrapped t row;
  index t;
  goto t ~row:t.cursor.row ~col:0

(* {1:charsets Character sets} *)

(* The set whose [chars] are replaced by [by], in order. *)
let replacing ?(national = true) chars by =
  let table = Array.init 96 (fun i -> i + 0x20) in
  let i = ref 0 in
  String.iter
    (fun c ->
      let d = String.get_utf_8_uchar by !i in
      table.(Char.code c - 0x20) <- Uchar.to_int (Uchar.utf_decode_uchar d);
      i := !i + Uchar.utf_decode_length d)
    chars;
  Mapped { table; national }

(* DEC Special Graphics (VT100 User Guide table 3-9): 0x5F is a blank. *)
let dec_graphics =
  replacing ~national:false "_`abcdefghijklmnopqrstuvwxyz{|}~"
    " \
     \u{25c6}\u{2592}\u{2409}\u{240c}\u{240d}\u{240a}\u{00b0}\u{00b1}\u{2424}\u{240b}\u{2518}\u{2510}\u{250c}\u{2514}\u{253c}\u{23ba}\u{23bb}\u{2500}\u{23bc}\u{23bd}\u{251c}\u{2524}\u{2534}\u{252c}\u{2502}\u{2264}\u{2265}\u{03c0}\u{2260}\u{00a3}\u{00b7}"

(* The UK set is the VT100's (VT100 User Guide table 3-9), not one of the
   national replacement sets that need national mode. *)
let uk = replacing ~national:false "#" "£"
let dutch = replacing "#@[\\]{|}~" "£¾ĳ½|¨ƒ¼´"
let finnish = replacing "[\\]^`{|}~" "ÄÖÅÜéäöåü"
let french = replacing "#@[\\]{|}~" "£à°ç§éùè¨"
let french_canadian = replacing "@[\\]^`{|}~" "àâçêîôéùèû"
let german = replacing "@[\\]{|}~" "§ÄÖÜäöüß"
let italian = replacing "#@[\\]`{|}~" "£§°çéùàòèì"
let norwegian_danish = replacing "@[\\]^`{|}~" "ÄÆØÅÜäæøåü"
let spanish = replacing "#@[\\]{|}" "£§¡Ñ¿°ñç"
let swedish = replacing "@[\\]^`{|}~" "ÉÄÖÅÜéäöåü"
let swiss = replacing "#@[\\]^_`{|}~" "ùàéçêîèôäöüû"

(* The 94-character sets by their one-byte designators (ctlseqs, SCS). *)
let charset_94 = function
  | '0' -> dec_graphics
  | 'A' -> uk
  | '4' -> dutch
  | 'C' | '5' -> finnish
  | 'R' | 'f' -> french
  | 'Q' | '9' -> french_canadian
  | 'K' -> german
  | 'Y' -> italian
  | 'E' | '6' | '`' -> norwegian_danish
  | 'Z' -> spanish
  | 'H' | '7' -> swedish
  | '=' -> swiss
  | _ -> Ascii

(* The national replacement sets "are available when you select national
   mode" (VT220 Programmer Reference Manual §2.4.3, DECNRCM §4.6.19). Out of
   it, a slot designated with one leaves characters unchanged, as ASCII, and
   it translates again once national mode is selected. *)
let in_effect t = function
  | Mapped { national = true; _ } when not t.modes.national -> Ascii
  | set -> set

let map t set u =
  match in_effect t set with
  | Ascii -> u
  | Mapped { table; _ } ->
      let c = Uchar.to_int u in
      if c >= 0x20 && c < 0x80 then Uchar.unsafe_of_int table.(c - 0x20) else u

(* A single shift is consumed by the next character, translated or not. *)
let translate t u =
  let cs = t.charsets in
  match cs.single with
  | None -> map t cs.g.(cs.gl) u
  | Some g ->
      t.charsets <- { cs with single = None };
      map t cs.g.(g) u

let gl_is_ascii t =
  t.charsets.single = None
  &&
  match in_effect t t.charsets.g.(t.charsets.gl) with
  | Ascii -> true
  | Mapped _ -> false

(* {1:print Printing} *)

(* The number a kitty placeholder's foreground names: its RGB, or its palette
   entry. The high byte of an id, in a third mark, is not read: an image is
   named by its id's other bytes, as a placeholder in 256 colors names it. *)
let named_image c =
  let p = Ansi.Color.Packed.encode c in
  match Ansi.Color.Packed.intent p with
  | Indexed n -> Some n
  | Rgb ->
      Some
        ((Ansi.Color.Packed.red p lsl 16)
        lor (Ansi.Color.Packed.green p lsl 8)
        lor Ansi.Color.Packed.blue p)
  | Default -> None

let holds t c =
  match named_image c with
  | Some n -> List.exists (fun (id, v) -> v && id land 0xFFFFFF = n) t.images
  | None -> false

(* A kitty Unicode placeholder (U+10EEEE and its marks) reads as U+FFFC
   while the terminal holds the image its foreground names with a virtual
   placement, and as U+FFFD otherwise. *)
let placeholder = "\xF4\x8E\xBB\xAE"

let write_cluster t ~row ~col ~width =
  let cl = t.cluster in
  let text, len =
    if
      has t Kitty_graphics && cl.len >= 4
      && Bytes.get cl.text 0 = placeholder.[0]
      && Bytes.get cl.text 1 = placeholder.[1]
      && Bytes.get cl.text 2 = placeholder.[2]
      && Bytes.get cl.text 3 = placeholder.[3]
    then ((if holds t (fg t) then "\u{FFFC}" else "\u{FFFD}"), 3)
    else (Bytes.unsafe_to_string cl.text, cl.len)
  in
  Grid.set_grapheme (grid t) ~x:col ~y:row ~width ~fg:(fg t) ~bg:(bg t)
    ~attrs:t.pen.style.attrs ?link:t.pen.link ?protected:(protection t) text
    ~pos:0 ~len;
  touch_row t row

(* Place the cluster from its position, by the four rules of vte.mli. With
   autowrap off it ends at the margin; otherwise, if it does not fit, the rest
   of the line is erased and it moves to the next. In insert mode the line
   moves right by the cells it gains. xterm, a datapoint, leaves the skipped
   cells alone; erasing them leaves the same screen whether a unit arrived
   whole or was placed narrow and then widened. *)
let place t =
  let cl = t.cluster and m = t.modes in
  let width = min cl.width t.cols in
  if cl.col + width > t.cols && not m.auto_wrap then (
    write_cluster t ~row:cl.row ~col:(t.cols - width) ~width;
    cl.cells <- width;
    set_cursor t ~row:cl.row ~col:(t.cols - 1) ~last_column:false)
  else begin
    if cl.col + width > t.cols then begin
      erase t ~x:cl.col ~y:cl.row ~width:(t.cols - cl.col) ~height:1;
      wrap t;
      cl.row <- t.cursor.row;
      cl.col <- 0;
      cl.cells <- 0
    end;
    if m.insert then
      shift_cols t ~x:(cl.col + cl.cells) ~top:cl.row ~bottom:cl.row
        (width - cl.cells);
    write_cluster t ~row:cl.row ~col:cl.col ~width;
    cl.cells <- width;
    let last = cl.col + width - 1 in
    if last = t.cols - 1 then
      set_cursor t ~row:cl.row ~col:last ~last_column:m.auto_wrap
    else set_cursor t ~row:cl.row ~col:(last + 1) ~last_column:false
  end

(* The grapheme at the cluster's [(row, col)], with [text] and its own width
   and rendition. *)
let rewrite_joined t text =
  let cl = t.cluster and g = grid t in
  let idx = (cl.row * t.cols) + cl.col in
  let link = Grid.get_link g idx in
  Grid.set_grapheme g ~x:cl.col ~y:cl.row
    ~width:(max 1 (Grid.cell_width g idx))
    ~fg:(Grid.get_fg g idx) ~bg:(Grid.get_bg g idx)
    ~attrs:(Ansi.Attr.unpack (Grid.get_attrs g idx))
    ?link:(if link = "" then None else Some link)
    ~protected:(Grid.is_protected g idx) text ~pos:0 ~len:(String.length text);
  touch_row t cl.row

(* A unit of no width that continues nothing joins the grapheme in the cell
   before the cursor, or under it at the last column; with no such cell it
   is dropped. xterm, a datapoint, instead joins the cell it last wrote, or
   after a cursor motion the cell at the cursor: after [ab], CUB and a
   combining acute accent it accents [b] where this terminal accents [a]. It
   is joined as soon as it is seen, so a reader of the cells finds it, and
   taken back if it gains a width. *)
let join t =
  let cl = t.cluster and c = t.cursor in
  let x = if c.last_column then c.col else c.col - 1 in
  if cl.at = Nowhere && x >= 0 then begin
    let g = grid t and base = c.row * t.cols in
    let x = ref x in
    while !x > 0 && Grid.is_continuation g (base + !x) do
      decr x
    done;
    cl.at <- Joined;
    cl.row <- c.row;
    cl.col <- !x;
    cl.prev <- Grid.get_text g (base + !x)
  end;
  if cl.at = Joined then
    rewrite_joined t (cl.prev ^ Bytes.sub_string cl.text 0 cl.len)

(* Place the cluster if it grew since it was last placed. Its first
   placement starts where the cursor is, after the wrap the Last Column Flag
   asks for. *)
let place_if_grown t =
  let cl = t.cluster in
  if cl.grown then begin
    cl.grown <- false;
    if cl.width = 0 then join t
    else begin
      if cl.at = Joined then (
        rewrite_joined t cl.prev;
        cl.at <- Nowhere);
      if cl.at = Nowhere then begin
        if t.cursor.last_column then wrap t;
        cl.at <- Cells;
        cl.row <- t.cursor.row;
        cl.col <- t.cursor.col;
        cl.cells <- 0
      end;
      place t
    end
  end

(* The unit in progress is complete. *)
let end_unit t =
  place_if_grown t;
  let cl = t.cluster in
  cl.len <- 0;
  cl.width <- 0;
  cl.at <- Nowhere

(* A control function ends the unit and the segmentation context: the next
   character starts a unit. *)
let break_text t =
  end_unit t;
  Text.Segmenter.reset t.cluster.seg

let append cl u =
  let n = Uchar.utf_8_byte_length u in
  if cl.len + n > Bytes.length cl.text then begin
    let text = Bytes.create (2 * (cl.len + n)) in
    Bytes.blit cl.text 0 text 0 cl.len;
    cl.text <- text
  end;
  ignore (Bytes.set_utf_8_uchar cl.text cl.len u : int);
  cl.len <- cl.len + n

(* The cluster is placed at boundaries and at the end of a text, not at each
   character, so a run of marks is written once. *)
let print_scalar t u =
  let cl = t.cluster in
  if Text.Segmenter.add cl.seg u then end_unit t;
  append cl u;
  cl.width <- Text.Segmenter.width cl.seg;
  cl.grown <- true

(* The [k] bytes of [s] from [pos] are units of one ASCII character each
   ({!Text.Segmenter.ascii_run}), which the segmenter has read: each is a
   unit, as {!print_scalar} would make it. *)
let print_run t s pos k =
  let cl = t.cluster in
  for i = pos to pos + k - 1 do
    end_unit t;
    Bytes.unsafe_set cl.text 0 (String.unsafe_get s i);
    cl.len <- 1;
    cl.width <- 1;
    cl.grown <- true
  done

let print t s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let k =
      if gl_is_ascii t then
        Text.Segmenter.ascii_run t.cluster.seg s ~pos:!i ~len:(len - !i)
      else 0
    in
    if k > 0 then (
      print_run t s !i k;
      i := !i + k)
    else begin
      let d = String.get_utf_8_uchar s !i in
      print_scalar t (translate t (Uchar.utf_decode_uchar d));
      i := !i + Uchar.utf_decode_length d
    end
  done;
  place_if_grown t

(* REP prints the unit before it [n] more times. Without autowrap the cursor
   reaches the margin within [cols] prints and each print after it is the
   same. With autowrap, once every row the prints can reach (the screen, the
   scrollback and the row being written) holds the same [per_row] units, the
   state depends on [n] only modulo [per_row], so the work is bounded by the
   screen and the scrollback, whatever [n]: a unit [w] cells wide is placed at
   most about [cols / w * (rows + scrollback)] times. That bound is still
   large: a REP of a few bytes on an 80-column screen with 10,000 lines of
   scrollback places about 800,000 units, a tenth of a second. *)
let repetitions t n width =
  if not t.modes.auto_wrap then min n (t.cols + 1)
  else
    let per_row = t.cols / width in
    let saturated = per_row * (t.rows + Array.length t.history.lines + 2) in
    if n <= saturated then n else saturated + ((n - saturated) mod per_row)

let rep t n =
  place_if_grown t;
  let cl = t.cluster in
  if cl.at = Cells then
    for _ = 1 to repetitions t n (min cl.width t.cols) do
      cl.at <- Nowhere;
      cl.grown <- true;
      place_if_grown t
    done

(* {1:motion Cursor motion} *)

(* Counts are clamped to the distance they act on before any arithmetic, so a
   saturated parameter never overflows. *)
let toward ~from ~limit n =
  if limit >= from then from + min n (limit - from)
  else from - min n (from - limit)

(* Up and down stop at the margin when they start inside the region, and at
   the screen edge otherwise. *)
let cuu t n =
  let { top; _ } = t.margins and row = t.cursor.row in
  let limit = if row >= top then top else 0 in
  goto t ~row:(toward ~from:row ~limit n) ~col:t.cursor.col

let cud t n =
  let { bottom; _ } = t.margins and row = t.cursor.row in
  let limit = if row <= bottom then bottom else t.rows - 1 in
  goto t ~row:(toward ~from:row ~limit n) ~col:t.cursor.col

let cuf t n =
  goto t ~row:t.cursor.row
    ~col:(toward ~from:t.cursor.col ~limit:(t.cols - 1) n)

let wrapped_row t row = row >= 0 && Bytes.get (screen t).wrapped row <> '\000'

(* BS and CUB: [n] columns left. With reverse-wrap (and autowrap) the Last
   Column Flag takes the first step, and a move past the first column goes on
   from the end of the row above while that row wrapped onto this one. *)
let cub t n =
  let c = t.cursor in
  if not (t.modes.reverse_wrap && t.modes.auto_wrap) then
    goto t ~row:c.row ~col:(toward ~from:c.col ~limit:0 n)
  else begin
    let row = ref c.row and col = ref c.col in
    let n = ref (if c.last_column then n - 1 else n) in
    while !n > 0 do
      if !n <= !col then (
        col := !col - !n;
        n := 0)
      else if wrapped_row t (!row - 1) then (
        n := !n - !col - 1;
        decr row;
        col := t.cols - 1)
      else (
        col := 0;
        n := 0)
    done;
    goto t ~row:!row ~col:!col
  end

let cnl t n =
  cud t n;
  goto t ~row:t.cursor.row ~col:0

let cpl t n =
  cuu t n;
  goto t ~row:t.cursor.row ~col:0

let cha t n = goto t ~row:t.cursor.row ~col:(min n t.cols - 1)

(* In origin mode (DECOM) rows count from the top margin and stay in the
   region. *)
let origin_row t n =
  if t.modes.origin then
    let { top; bottom } = t.margins in
    top + min (n - 1) (bottom - top)
  else min n t.rows - 1

let cup t row col = goto t ~row:(origin_row t row) ~col:(min col t.cols - 1)
let vpa t n = goto t ~row:(origin_row t n) ~col:t.cursor.col

(* VPR: down [n] rows, past the bottom margin, but not out of the region in
   origin mode. *)
let vpr t n =
  let limit = if t.modes.origin then t.margins.bottom else t.rows - 1 in
  let row = t.cursor.row in
  goto t
    ~row:(if row >= limit then row else toward ~from:row ~limit n)
    ~col:t.cursor.col

let home t = goto t ~row:(if t.modes.origin then t.margins.top else 0) ~col:0

(* {1:tabs Tab stops} *)

let next_tab t col =
  let rec go x =
    if x >= t.cols - 1 then t.cols - 1 else if t.tabs.(x) then x else go (x + 1)
  in
  go (col + 1)

let prev_tab t col =
  let rec go x = if x <= 0 then 0 else if t.tabs.(x) then x else go (x - 1) in
  go (col - 1)

let tab_steps t n next =
  let col = ref t.cursor.col in
  for _ = 1 to min n t.cols do
    col := next t !col
  done;
  goto t ~row:t.cursor.row ~col:!col

(* HT and CHT stop at the last column, so with the Last Column Flag set they
   do nothing and keep it. *)
let cht t n = if not t.cursor.last_column then tab_steps t n next_tab
let cbt t n = tab_steps t n prev_tab
let hts t = t.tabs.(t.cursor.col) <- true

let tbc t = function
  | 0 -> t.tabs.(t.cursor.col) <- false
  | 3 -> Array.fill t.tabs 0 t.cols false
  | _ -> ()

(* {1:edit Editing} *)

(* ED and DECSED, EL and DECSEL, by the erase they use. *)
let erase_display t erase = function
  | 0 ->
      let ({ row; col; _ } : cursor) = t.cursor in
      erase t ~x:col ~y:row ~width:(t.cols - col) ~height:1;
      erase t ~x:0 ~y:(row + 1) ~width:t.cols ~height:(t.rows - row - 1)
  | 1 ->
      let ({ row; col; _ } : cursor) = t.cursor in
      erase t ~x:0 ~y:0 ~width:t.cols ~height:row;
      erase t ~x:0 ~y:row ~width:(col + 1) ~height:1
  | 2 -> erase t ~x:0 ~y:0 ~width:t.cols ~height:t.rows
  | 3 ->
      if t.history.count > 0 then (
        clear_history t;
        t.dirty.any <- true)
  | _ -> ()

let erase_line t erase n =
  let ({ row; col; _ } : cursor) = t.cursor in
  match n with
  | 0 -> erase t ~x:col ~y:row ~width:(t.cols - col) ~height:1
  | 1 -> erase t ~x:0 ~y:row ~width:(col + 1) ~height:1
  | 2 -> erase t ~x:0 ~y:row ~width:t.cols ~height:1
  | _ -> ()

let ed t n = erase_display t erase n
let decsed t n = erase_display t erase_unprotected n
let el t n = erase_line t erase n
let decsel t n = erase_line t erase_unprotected n

let ech t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  erase t ~x:col ~y:row ~width:(min n (t.cols - col)) ~height:1

let in_region t =
  t.cursor.row >= t.margins.top && t.cursor.row <= t.margins.bottom

(* IL and DL act from a cursor inside the region and return it to the first
   column. *)
let il t n =
  if in_region t then (
    let row = t.cursor.row in
    shift_rows t ~top:row ~bottom:t.margins.bottom
      (-min n (t.margins.bottom - row + 1));
    goto t ~row ~col:0)

let dl t n =
  if in_region t then (
    let row = t.cursor.row in
    shift_rows t ~top:row ~bottom:t.margins.bottom
      (min n (t.margins.bottom - row + 1));
    goto t ~row ~col:0)

let ich t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  shift_cols t ~x:col ~top:row ~bottom:row (min n (t.cols - col))

let dch t n =
  let ({ row; col; _ } : cursor) = t.cursor in
  shift_cols t ~x:col ~top:row ~bottom:row (-min n (t.cols - col))

(* SL, SR, DECIC and DECDC shift the columns of the region's rows, from a
   cursor inside it. *)
let shift_region_cols t ~x dx =
  if in_region t then
    shift_cols t ~x ~top:t.margins.top ~bottom:t.margins.bottom dx

let sl t n = shift_region_cols t ~x:0 (-min n t.cols)
let sr t n = shift_region_cols t ~x:0 (min n t.cols)

let decic t n =
  shift_region_cols t ~x:t.cursor.col (min n (t.cols - t.cursor.col))

let decdc t n =
  shift_region_cols t ~x:t.cursor.col (-min n (t.cols - t.cursor.col))

let su t n = scroll_up t n
let sd t n = scroll_down t n

let decstbm t top bottom =
  let top = min top t.rows and bottom = min bottom t.rows in
  if top < bottom then (
    t.margins <- { top = top - 1; bottom = bottom - 1 };
    home t)

(* DECALN fills the screen with E in the default rendition, resets origin
   mode, the rendition, the margins and the Last Column Flag, and homes the
   cursor. xterm, a datapoint (charproc.c, CASE_DECALN, citing DEC STD 070),
   does the same, except that it keeps the pen's colours. *)
let decaln t =
  t.modes <- { t.modes with origin = false };
  t.pen <- { t.pen with style = Ansi.Style.default };
  t.margins <- full_margins t;
  let g = grid t in
  for y = 0 to t.rows - 1 do
    for x = 0 to t.cols - 1 do
      Grid.set_grapheme g ~x ~y ~width:1 ~fg:default_fg ~bg:default_bg
        ~attrs:Ansi.Attr.empty "E" ~pos:0 ~len:1
    done
  done;
  unwrap t ~top:0 ~bottom:(t.rows - 1);
  cells_changed t ~top:0 ~bottom:(t.rows - 1);
  goto t ~row:0 ~col:0

(* {1:modes Modes and resets} *)

let decsc t =
  let { row; col; last_column } = t.cursor in
  (screen t).saved <-
    {
      cursor = { row; col; last_column };
      pen = t.pen;
      charsets = t.charsets;
      origin = t.modes.origin;
    }

(* DECRC restores the Last Column Flag with the position, unless autowrap is
   off, which never leaves the flag set. Autowrap stays as it is: DEC's VT220,
   VT420 and VT510, measured, restore the flag and not the mode. *)
let decrc t =
  let s = (screen t).saved in
  t.pen <- s.pen;
  t.charsets <- s.charsets;
  t.modes <- { t.modes with origin = s.origin };
  let row =
    if s.origin then max t.margins.top (min t.margins.bottom s.cursor.row)
    else s.cursor.row
  in
  set_cursor t ~row ~col:s.cursor.col
    ~last_column:(s.cursor.last_column && t.modes.auto_wrap)

let set_cursor_visible t visible =
  if t.modes.cursor_visible <> visible then (
    t.modes <- { t.modes with cursor_visible = visible };
    t.dirty.any <- true;
    t.dirty.cursor <- true)

let set_cursor_style t style =
  if t.cursor_style <> style then (
    t.cursor_style <- style;
    t.dirty.any <- true;
    t.dirty.cursor <- true)

(* DECSCUSR: 0 to 4 are the VT510's, 5 and 6 (bars) and 7 (the initial
   style) xterm's (ctlseqs). Other values have no effect. *)
let decscusr t = function
  | 0 | 1 | 7 -> set_cursor_style t power_up_cursor_style
  | 2 -> set_cursor_style t (`Block, false)
  | 3 -> set_cursor_style t (`Underline, true)
  | 4 -> set_cursor_style t (`Underline, false)
  | 5 -> set_cursor_style t (`Line, true)
  | 6 -> set_cursor_style t (`Line, false)
  | _ -> ()

let switch_screen t ~alternate =
  if t.modes.alternate_screen <> alternate then (
    t.modes <- { t.modes with alternate_screen = alternate };
    touch_rows t 0 (t.rows - 1))

let erase_screen t = erase t ~x:0 ~y:0 ~width:t.cols ~height:t.rows

let leave_alternate t =
  if t.modes.alternate_screen then erase_screen t;
  switch_screen t ~alternate:false

let set_ansi_mode t mode on =
  match mode with
  | 4 -> t.modes <- { t.modes with insert = on }
  | 20 -> t.modes <- { t.modes with new_line = on }
  | _ -> ()

let set_dec_mode t mode on =
  let m = t.modes in
  match mode with
  | 1 -> t.modes <- { m with cursor_keys = on }
  | 6 ->
      t.modes <- { m with origin = on };
      home t
  | 7 ->
      t.modes <- { m with auto_wrap = on };
      if not on then t.cursor.last_column <- false
  | 25 -> set_cursor_visible t on
  | 42 -> t.modes <- { m with national = on }
  | 45 -> t.modes <- { m with reverse_wrap = on }
  | 47 -> switch_screen t ~alternate:on
  | 1047 -> if on then switch_screen t ~alternate:true else leave_alternate t
  | 1048 -> if on then decsc t else decrc t
  | 1049 ->
      if on then (
        decsc t;
        switch_screen t ~alternate:true;
        erase_screen t)
      else (
        leave_alternate t;
        decrc t)
  | 2004 -> t.modes <- { m with bracketed_paste = on }
  | 66 -> t.modes <- { m with keypad = on }
  | 9 | 1000 | 1002 | 1003 ->
      t.tracking <-
        (if not on then `Off
         else
           match mode with
           | 9 -> `X10
           | 1000 -> `Normal
           | 1002 -> `Button
           | _ -> `Any)
  | 1006 ->
      if on then t.mouse_format <- `Sgr
      else if t.mouse_format = `Sgr then t.mouse_format <- `X10
  | 1015 ->
      if on then t.mouse_format <- `Urxvt
      else if t.mouse_format = `Urxvt then t.mouse_format <- `X10
  | 1004 when has t Focus_reports -> t.modes <- { m with focus_reports = on }
  | 2026 when has t Synchronized_output ->
      t.modes <- { m with synchronized_output = on }
  | 2031 when has t Color_scheme_reports ->
      t.modes <- { m with color_scheme_reports = on }
  | 2048 when has t In_band_resize -> t.modes <- { m with resize_reports = on }
  | _ -> ()

(* SM, RM, DECSET and DECRST: every mode listed, absent ones skipped. *)
let set_modes t p set on =
  for i = 0 to Params.count p - 1 do
    Option.iter (fun mode -> set t mode on) (Params.value p i)
  done

(* RIS: the power-up state. *)
let ris t =
  List.iter
    (fun s ->
      Grid.clear ~color:default_bg s.grid;
      Bytes.fill s.wrapped 0 t.rows '\000';
      s.saved <- power_up)
    [ t.primary; t.alternate ];
  clear_history t;
  set_cursor_visible t true;
  set_cursor_style t power_up_cursor_style;
  t.modes <- power_up_modes;
  set_cursor t ~row:0 ~col:0 ~last_column:false;
  t.pen <- plain_pen;
  t.charsets <- ascii_sets;
  t.margins <- full_margins t;
  t.tabs <- default_tabs t.cols;
  t.title <- "";
  t.kitty_primary <- [];
  t.kitty_alternate <- [];
  t.modify_other_keys <- 0;
  t.tracking <- `Off;
  t.mouse_format <- `X10;
  if t.palette != t.initial then (
    t.palette <- t.initial;
    t.dirty.cursor <- true);
  touch_rows t 0 (t.rows - 1)

(* A grid is at most [Grid.max_width] wide; the check comes before any
   change. *)
let check_cols fn cols =
  if cols > Grid.max_width then
    invalid_arg (Printf.sprintf "Vte.%s: cols > %d" fn Grid.max_width)

(* The colours of Ansi.Color.indexed, white (7) on black (0), and the
   cursor the foreground's. *)
let default_palette =
  let colors =
    Array.init 256 (fun i -> Ansi.Color.to_rgb (Ansi.Color.indexed i))
  in
  {
    colors;
    foreground = colors.(7);
    background = colors.(0);
    cursor = colors.(7);
  }

let check_palette p =
  let channel v = v >= 0 && v <= 255 in
  let rgb (r, g, b) = channel r && channel g && channel b in
  if Array.length p.colors <> 256 then
    invalid_arg "Vte.create: the palette does not have 256 colours";
  if
    not
      (Array.for_all rgb p.colors && rgb p.foreground && rgb p.background
     && rgb p.cursor)
  then invalid_arg "Vte.create: a palette channel is outside 0..255";
  { p with colors = Array.copy p.colors }

(* The structure, which RIS gives its power-up state. *)
let create ?(identity = vt100) ?(extensions = extensions) ?(scrollback = 10000)
    ?(width_method = `Unicode) ?(palette = default_palette) ?cell_size ~rows
    ~cols () =
  check_cols "create" cols;
  let initial = check_palette palette in
  let rows = max 1 rows and cols = max 1 cols in
  let screen () =
    let grid = Grid.create ~width:cols ~height:rows ~width_method () in
    { grid; wrapped = Bytes.make rows '\000'; saved = power_up }
  in
  let t =
    {
      rows;
      cols;
      primary = screen ();
      alternate = screen ();
      history =
        {
          lines = Array.make (max 0 scrollback) empty_line;
          head = 0;
          count = 0;
        };
      cursor = { row = 0; col = 0; last_column = false };
      cursor_style = power_up_cursor_style;
      pen = plain_pen;
      charsets = ascii_sets;
      modes = power_up_modes;
      margins = { top = 0; bottom = 0 };
      tabs = [||];
      title = "";
      cluster =
        {
          seg = Text.Segmenter.create width_method;
          text = Bytes.create 16;
          len = 0;
          width = 0;
          grown = false;
          at = Nowhere;
          row = 0;
          col = 0;
          cells = 0;
          prev = "";
        };
      initial;
      palette = initial;
      parser = Ansi.Parser.create ();
      osc = Osc_skip;
      osc_text = Buffer.create 64;
      replies = [];
      dirty = { any = false; cursor = false; rows = Bytes.make rows '\000' };
      identity;
      extensions;
      kitty_primary = [];
      kitty_alternate = [];
      modify_other_keys = 0;
      tracking = `Off;
      mouse_format = `X10;
      color_scheme = `Dark;
      requests = [];
      cell_size;
      apc = Apc_skip;
      transmission = None;
      images = [];
    }
  in
  ris t;
  clear_dirty t;
  t

(* DECSTR: the settings, the pen and the character sets to their power-up
   values (VT510 DECSTR, Table 5-9), autowrap on as at power-up. LNM is not
   in the table, and stays. xterm, a datapoint, resets the cursor style and
   the indexed colours there too. *)
let decstr t =
  set_cursor_visible t true;
  t.modes <-
    {
      power_up_modes with
      new_line = t.modes.new_line;
      bracketed_paste = t.modes.bracketed_paste;
      alternate_screen = t.modes.alternate_screen;
      focus_reports = t.modes.focus_reports;
      synchronized_output = t.modes.synchronized_output;
      color_scheme_reports = t.modes.color_scheme_reports;
      resize_reports = t.modes.resize_reports;
    };
  t.margins <- full_margins t;
  t.pen <- plain_pen;
  t.charsets <- ascii_sets;
  t.primary.saved <- power_up;
  t.alternate.saved <- power_up

(* {1:charset_functions Character set functions} *)

(* SCS: [ESC I F] with I one of ( ) * + designates a 94-character set as G0
   to G3, and with - . / a 96-character set as G1 to G3. A set without a
   table here (a 96-character set, or one named by a two-byte designator such
   as [% 6]) maps nothing. *)
let scs t intermediates final =
  let designate g set =
    let sets = Array.copy t.charsets.g in
    sets.(g) <- set;
    t.charsets <- { t.charsets with g = sets }
  in
  let set =
    if String.length intermediates = 1 then charset_94 final else Ascii
  in
  match intermediates.[0] with
  | '(' -> designate 0 set
  | ')' -> designate 1 set
  | '*' -> designate 2 set
  | '+' -> designate 3 set
  | '-' -> designate 1 Ascii
  | '.' -> designate 2 Ascii
  | '/' -> designate 3 Ascii
  | _ -> ()

(* SI, SO, LS2 and LS3 invoke G0, G1, G2 and G3 into GL; SS2 and SS3 invoke
   G2 or G3 for the next character. *)
let ls t g = t.charsets <- { t.charsets with gl = g }
let ss t single = t.charsets <- { t.charsets with single }

(* {1:replies Replies} *)

let reply t r = t.replies <- r :: t.replies
let da t = reply t (Primary_attributes t.identity.attributes)
let da2 t = reply t (Secondary_attributes t.identity.secondary)
let xtversion t = Option.iter (fun v -> reply t (Version v)) t.identity.version

(* The position cursor reports give: in origin mode the row counts from the
   top margin. *)
let report_row t = t.cursor.row - if t.modes.origin then t.margins.top else 0

let dsr t = function
  | 5 -> reply t (Operating_status `Ok)
  | 6 ->
      reply t (Cursor { row = report_row t; col = t.cursor.col; page = None })
  | _ -> ()

let decxcpr t =
  reply t (Cursor { row = report_row t; col = t.cursor.col; page = Some 1 })

(* DECRQM: DECRPM's Pm is 1 for set, 2 for reset, 0 for a mode not
   recognized (VT320 Programmer Information, Table 2), and 3 and 4 for one set
   or reset for good. *)
let setting : bool -> Reply.setting = function true -> Set | false -> Reset

let ansi_mode t : int -> Reply.setting = function
  | 4 -> setting t.modes.insert
  | 20 -> setting t.modes.new_line
  | _ -> Not_recognized

let dec_mode t mode : Reply.setting =
  let m = t.modes in
  let ext e on = if has t e then setting on else Not_recognized in
  match mode with
  | 1 -> setting m.cursor_keys
  | 6 -> setting m.origin
  | 7 -> setting m.auto_wrap
  | 25 -> setting m.cursor_visible
  | 42 -> setting m.national
  | 45 -> setting m.reverse_wrap
  | 47 | 1047 | 1049 -> setting m.alternate_screen
  | 2004 -> setting m.bracketed_paste
  | 66 -> setting m.keypad
  | 9 -> setting (t.tracking = `X10)
  | 1000 -> setting (t.tracking = `Normal)
  | 1002 -> setting (t.tracking = `Button)
  | 1003 -> setting (t.tracking = `Any)
  | 1006 -> setting (t.mouse_format = `Sgr)
  | 1015 -> setting (t.mouse_format = `Urxvt)
  (* Alt is always an ESC before the key. *)
  | 1036 -> Permanently_set
  | 1004 -> ext Focus_reports m.focus_reports
  | 2026 -> ext Synchronized_output m.synchronized_output
  | 2031 -> ext Color_scheme_reports m.color_scheme_reports
  | 2048 -> ext In_band_resize m.resize_reports
  | 2027 when has t Grapheme_clusters -> (
      match Grid.width_method t.primary.grid with
      | `Unicode -> Permanently_set
      | `Wcwidth -> Permanently_reset
      | `Unicode_wide -> Not_recognized)
  | _ -> Not_recognized

let decrqm t ~private_ mode =
  let setting = (if private_ then dec_mode else ansi_mode) t mode in
  reply t (Mode { dec = private_; mode; setting })

(* {2 The keyboard} *)

let kitty_stack t =
  if t.modes.alternate_screen then t.kitty_alternate else t.kitty_primary

let set_kitty_stack t stack =
  if t.modes.alternate_screen then t.kitty_alternate <- stack
  else t.kitty_primary <- stack

let kitty_flags t = match kitty_stack t with flags :: _ -> flags | [] -> 0

(* The kitty protocol's progressive enhancement: [CSI > flags u] pushes,
   [CSI < n u] pops n entries, [CSI = flags ; mode u] sets the current flags
   (1), adds them (2) or takes them away (3), and [CSI ? u] asks for them. *)
let kitty_push t flags = set_kitty_stack t (flags :: kitty_stack t)

let kitty_pop t n =
  let rec drop n l =
    if n <= 0 then l else match l with [] -> [] | _ :: l -> drop (n - 1) l
  in
  set_kitty_stack t (drop n (kitty_stack t))

let kitty_set t flags mode =
  let current = kitty_flags t in
  let flags =
    match mode with
    | 2 -> current lor flags
    | 3 -> current land lnot flags
    | _ -> flags
  in
  set_kitty_stack t
    (match kitty_stack t with _ :: rest -> flags :: rest | [] -> [ flags ])

(* xterm's XTMODKEYS, [CSI > 4 ; v m], sets modifyOtherKeys; with no value
   it resets it. *)
let xtmodkeys t p =
  if Params.ps p 0 ~default:0 = 4 then
    t.modify_other_keys <- Params.ps p 1 ~default:0

(* {1:dispatch Dispatch}

   A function is named by the private marker, the intermediates and the final
   byte (ECMA-48 §5.4; ECMA-35 §13.1). Counts and positions read 0 as their
   default (Params.pn); selectors do not (Params.ps). *)

let sgr t p =
  let style = Ansi.Style.apply_sgr p t.pen.style in
  if not (Ansi.Style.equal style t.pen.style) then (
    t.pen <- { t.pen with style };
    t.dirty.any <- true)

let decsca t = function
  | 0 | 2 -> t.pen <- { t.pen with protected = false }
  | 1 -> t.pen <- { t.pen with protected = true }
  | _ -> ()

(* SUB shows the error character (vte.mli). xterm, a datapoint, shows a
   checkerboard for CAN and SUB at VT100 level, and a SUB symbol above it. *)
let sub t =
  print t "\u{2E2E}";
  break_text t

(* LF, VT and FF are one function, a new line in LNM (vte.mli). *)
let line_feed t =
  index t;
  if t.modes.new_line then goto t ~row:t.cursor.row ~col:0

let control t = function
  | '\x08' -> cub t 1
  | '\x09' -> cht t 1
  | '\x0a' | '\x0b' | '\x0c' -> line_feed t
  | '\x0d' -> goto t ~row:t.cursor.row ~col:0
  | '\x0e' -> ls t 1
  | '\x0f' -> ls t 0
  | '\x1a' -> sub t
  | '\x84' -> index t
  | '\x85' ->
      index t;
      goto t ~row:t.cursor.row ~col:0
  | '\x88' -> hts t
  | '\x8d' -> reverse_index t
  | '\x8e' -> ss t (Some 2)
  | '\x8f' -> ss t (Some 3)
  | '\x9a' -> da t (* ESC Z, DECID: an obsolete form of DA (ctlseqs) *)
  | '\x07' -> t.requests <- Bell :: t.requests
  | _ -> ()

let esc t intermediates final =
  match (intermediates, final) with
  | "", 'c' -> ris t
  | "", '=' -> t.modes <- { t.modes with keypad = true } (* DECKPAM *)
  | "", '>' -> t.modes <- { t.modes with keypad = false } (* DECKPNM *)
  | "", '7' -> decsc t
  | "", '8' -> decrc t
  | "", 'n' -> ls t 2
  | "", 'o' -> ls t 3
  | "#", '8' -> decaln t
  | "", _ -> ()
  | _ -> scs t intermediates final

let csi t (s : Ansi.Parser.sequence) =
  let p = s.params in
  let pn i = Params.pn p i ~default:1 and ps i = Params.ps p i ~default:0 in
  (* REP, [CSI b], is not here: it reads the unit in progress before the
     sequence ends it ({!handle_token}). *)
  match (s.marker, s.intermediates, s.final) with
  | None, "", 'A' -> cuu t (pn 0)
  | None, "", 'B' -> cud t (pn 0)
  | None, "", ('C' | 'a') -> cuf t (pn 0) (* HPR is CUF on this device *)
  | None, "", 'D' -> cub t (pn 0)
  | None, "", 'E' -> cnl t (pn 0)
  | None, "", 'F' -> cpl t (pn 0)
  | None, "", ('G' | '`') -> cha t (pn 0) (* HPA is CHA on this device *)
  | None, "", 'd' -> vpa t (pn 0)
  | None, "", 'e' -> vpr t (pn 0)
  | None, "", ('H' | 'f') -> cup t (pn 0) (pn 1) (* HVP is CUP *)
  | None, "", 'I' -> cht t (pn 0)
  | None, "", 'Z' -> cbt t (pn 0)
  | None, "", 'g' -> tbc t (ps 0)
  | None, "", 'J' -> ed t (ps 0)
  | None, "", 'K' -> el t (ps 0)
  | None, "", 'X' -> ech t (pn 0)
  | None, "", 'L' -> il t (pn 0)
  | None, "", 'M' -> dl t (pn 0)
  | None, "", 'P' -> dch t (pn 0)
  | None, "", '@' -> ich t (pn 0)
  | None, "", 'r' -> decstbm t (pn 0) (Params.pn p 1 ~default:t.rows)
  | None, "", 'S' -> su t (pn 0)
  | None, "", 'T' -> sd t (pn 0)
  | None, "", 'm' -> sgr t p
  | None, "", 'c' when ps 0 = 0 -> da t
  | None, "", 'n' -> dsr t (ps 0)
  | None, "", 's' -> decsc t (* SCOSC *)
  | None, "", 'u' -> decrc t (* SCORC *)
  | None, "", 'h' -> set_modes t p set_ansi_mode true
  | None, "", 'l' -> set_modes t p set_ansi_mode false
  | Some '?', "", 'h' -> set_modes t p set_dec_mode true
  | Some '?', "", 'l' -> set_modes t p set_dec_mode false
  | Some '?', "", 'J' -> decsed t (ps 0)
  | Some '?', "", 'K' -> decsel t (ps 0)
  | Some '?', "", 'n' when ps 0 = 6 -> decxcpr t
  | Some '>', "", 'c' when ps 0 = 0 -> da2 t
  | None, "", 't' when ps 0 = 16 ->
      Option.iter
        (fun (w, h) -> reply t (Cell_pixels { height = h; width = w }))
        t.cell_size
  | None, "", 't' when ps 0 = 14 ->
      Option.iter
        (fun (w, h) ->
          reply t (Text_area_pixels { height = h * t.rows; width = w * t.cols }))
        t.cell_size
  | Some '>', "", 'q' when ps 0 = 0 -> xtversion t
  | Some '?', "", 'n' when ps 0 = 996 && has t Color_scheme_reports ->
      reply t (Color_scheme t.color_scheme)
  | Some '>', "", 'u' when has t Kitty_keyboard -> kitty_push t (ps 0)
  | Some '<', "", 'u' when has t Kitty_keyboard -> kitty_pop t (pn 0)
  | Some '=', "", 'u' when has t Kitty_keyboard ->
      kitty_set t (ps 0) (Params.ps p 1 ~default:1)
  | Some '?', "", 'u' when has t Kitty_keyboard ->
      reply t (Keyboard_flags (kitty_flags t))
  | Some '>', "", 'm' when has t Modify_other_keys -> xtmodkeys t p
  | Some '>', "", 'n' when has t Modify_other_keys && ps 0 = 4 ->
      t.modify_other_keys <- 0
  | None, "$", 'p' -> decrqm t ~private_:false (ps 0)
  | Some '?', "$", 'p' -> decrqm t ~private_:true (ps 0)
  | None, "!", 'p' -> decstr t
  | None, "\"", 'q' -> decsca t (ps 0)
  | None, " ", 'q' -> decscusr t (ps 0)
  | None, " ", '@' -> sl t (pn 0)
  | None, " ", 'A' -> sr t (pn 0)
  | None, "'", '}' -> decic t (pn 0)
  | None, "'", '~' -> decdc t (pn 0)
  | _ -> ()

(* {1:strings Strings}

   Only an OSC the VTE performs is kept, and only up to [osc_budget] bytes; a
   longer one is dropped whole, as xterm drops an over-long string. It is
   performed when it ends with ST or BEL, never when it is cut short. OSC 0 and
   2 set the title; OSC 8 opens a hyperlink, [8 ; params ; URI], or closes it
   when the URI is empty; OSC 4, 10, 11 and 12 set and ask for the palette's
   colours, and OSC 104 and 110 to 112 reset them. *)

let osc_budget = 65536

let osc_drop t =
  t.osc <- Osc_skip;
  Buffer.reset t.osc_text

let osc_keep t s off =
  let len = String.length s - off in
  if Buffer.length t.osc_text + len > osc_budget then osc_drop t
  else Buffer.add_substring t.osc_text s off len

(* The OSCs performed: titles, links, the palette's colours, and the requests
   to the host. *)
let osc_kept = function
  | 0 | 2 | 4 | 8 | 9 | 10 | 11 | 12 | 52 | 104 | 110 | 111 | 112 | 777 -> true
  | _ -> false

(* Ps, the number before the first [;], decides whether the rest is kept. *)
let rec osc_number t s i n =
  if i >= String.length s then t.osc <- Osc_number n
  else
    match s.[i] with
    | '0' .. '9' as c ->
        osc_number t s (i + 1) (min 1000 ((max n 0 * 10) + Char.code c - 48))
    | ';' when osc_kept n ->
        t.osc <- Osc_text n;
        osc_keep t s (i + 1)
    | _ -> osc_drop t

let osc_hyperlink t text =
  match String.index_opt text ';' with
  | None -> ()
  | Some i ->
      let uri = String.sub text (i + 1) (String.length text - i - 1) in
      t.pen <- { t.pen with link = (if uri = "" then None else Some uri) }

let decimal s =
  if s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  then int_of_string_opt s
  else None

(* {2 The palette} *)

let palette t = t.palette

let palette_colour p = function
  | `Foreground -> p.foreground
  | `Background -> p.background
  | `Cursor -> p.cursor
  | `Palette i -> p.colors.(i)

(* Every cell, and the cursor, may show a colour that changed. *)
let set_palette t p =
  if p <> t.palette then (
    t.palette <- p;
    t.dirty.cursor <- true;
    touch_rows t 0 (t.rows - 1))

let set_colour t target rgb =
  let p = t.palette in
  set_palette t
    (match target with
    | `Foreground -> { p with foreground = rgb }
    | `Background -> { p with background = rgb }
    | `Cursor -> { p with cursor = rgb }
    | `Palette i ->
        let colors = Array.copy p.colors in
        colors.(i) <- rgb;
        { p with colors })

let reset_colour t target =
  set_colour t target (palette_colour t.initial target)

(* A spec [?] asks for the colour, answered as the OSC that would set it,
   with the query's terminator; another spec sets it, if it names one.
   [false] if the spec names no colour. *)
let colour t terminator target spec =
  if spec = "?" then (
    reply t
      (Color { target; rgb = palette_colour t.palette target; terminator });
    true)
  else
    match Reply.rgb_of_spec spec with
    | Some rgb ->
        set_colour t target rgb;
        true
    | None -> false

(* OSC 4 is pairs [c ; spec]. OSC 10, 11 and 12 are specs for the dynamic
   colours from Ps on, the foreground, the background and the cursor; one past
   them names a colour this terminal does not have. OSC 104 resets the listed
   colours, all of them with none; OSC 110, 111 and 112 reset one dynamic colour
   (ctlseqs).

   OSC 4 and 104 stop at the first entry in error, as xterm does ("quit on any
   error", misc.c): in OSC 4 an index that is not a number from 0 to 255 or a
   spec that names no colour, in OSC 104 an entry that is not a number, while
   an index past the table is skipped. Measured against xterm 398, two
   departures: xterm's indices 256 to 260 are colours this terminal does not
   have, so here they end OSC 4; and xterm reads an index with atoi, so [x]
   is colour 0 there and ends OSC 4 here. A dynamic colour's spec that names
   no colour is skipped, as in xterm. *)
let osc_colours t n terminator text =
  let rec pairs = function
    | c :: spec :: rest -> (
        match decimal c with
        | Some i when i <= 255 && colour t terminator (`Palette i) spec ->
            pairs rest
        | _ -> ())
    | _ -> ()
  in
  let rec resets = function
    | c :: rest -> (
        match decimal c with
        | Some i ->
            if i <= 255 then reset_colour t (`Palette i);
            resets rest
        | None -> ())
    | [] -> ()
  in
  let rec dynamic n = function
    | spec :: rest ->
        (match n with
        | 10 -> ignore (colour t terminator `Foreground spec : bool)
        | 11 -> ignore (colour t terminator `Background spec : bool)
        | 12 -> ignore (colour t terminator `Cursor spec : bool)
        | _ -> ());
        dynamic (n + 1) rest
    | [] -> ()
  in
  match n with
  | 4 -> pairs (String.split_on_char ';' text)
  | 104 when text = "" ->
      set_palette t { t.palette with colors = t.initial.colors }
  | 104 -> resets (String.split_on_char ';' text)
  | 110 -> reset_colour t `Foreground
  | 111 -> reset_colour t `Background
  | 112 -> reset_colour t `Cursor
  | n -> dynamic n (String.split_on_char ';' text)

let open_string t (o : Ansi.Parser.opening) =
  t.osc <- (match o with Osc -> Osc_number (-1) | _ -> Osc_skip);
  t.apc <-
    (match o with
    | Apc when has t Kitty_graphics -> Apc_keys (Buffer.create 64)
    | _ -> Apc_skip)

let string_data t s =
  (match t.osc with
  | Osc_skip -> ()
  | Osc_number n -> osc_number t s 0 n
  | Osc_text _ -> osc_keep t s 0);
  match t.apc with
  | Apc_skip -> ()
  | Apc_keys b -> (
      match String.index_opt s ';' with
      | None -> Buffer.add_string b s
      | Some i ->
          Buffer.add_substring b s 0 i;
          t.apc <- Apc_payload (Buffer.contents b, String.length s - i - 1))
  | Apc_payload (keys, n) -> t.apc <- Apc_payload (keys, n + String.length s)

(* {2 Kitty graphics}

   A command's keys are [k=v] pairs; this terminal reads the query, the
   transmission of raw pixels in chunks with an optional virtual placement,
   a virtual placement of a held image, and deletion by id or of all. It
   keeps no pixel: a transmission whose payload is not the length its keys
   promise holds no image. *)

let graphics_key keys k =
  List.find_map
    (fun kv ->
      match String.index_opt kv '=' with
      | Some i when String.sub kv 0 i = k ->
          Some (String.sub kv (i + 1) (String.length kv - i - 1))
      | _ -> None)
    keys

let int_key keys k =
  Option.bind (graphics_key keys k) int_of_string_opt |> Option.value ~default:0

(* The bytes [n] base64 characters decode to. *)
let decoded n = n / 4 * 3

let forget_image t id =
  t.images <- List.filter (fun (i, _) -> i <> id) t.images;
  (* Placeholders that named it read as such no more. *)
  let g = grid t in
  for idx = 0 to (Grid.width g * Grid.height g) - 1 do
    if Grid.get_text g idx = "\u{FFFC}" then
      match named_image (Grid.get_fg g idx) with
      | Some n when n = id land 0xFFFFFF ->
          let link = Grid.get_link g idx in
          Grid.set_grapheme g ~x:(idx mod t.cols) ~y:(idx / t.cols) ~width:1
            ~fg:(Grid.get_fg g idx) ~bg:(Grid.get_bg g idx)
            ~attrs:(Ansi.Attr.unpack (Grid.get_attrs g idx))
            ?link:(if link = "" then None else Some link)
            "\u{FFFD}" ~pos:0 ~len:3;
          touch_row t (idx / t.cols)
      | _ -> ()
  done

let end_transmission t (tr : transmission) =
  t.transmission <- None;
  if decoded tr.payload >= tr.expected && decoded tr.payload - tr.expected < 3
  then
    t.images <-
      (tr.image, tr.virtual_placement) :: List.remove_assoc tr.image t.images

let graphics_command t keys payload =
  let keys = String.split_on_char ',' keys in
  let more = int_key keys "m" = 1 in
  match (t.transmission, graphics_key keys "a") with
  | Some tr, None ->
      tr.payload <- tr.payload + payload;
      if not more then end_transmission t tr
  | _, Some "q" ->
      reply t (Graphics { id = Some (int_key keys "i"); message = "OK" })
  | _, Some (("t" | "T") as a) ->
      let bytes = if int_key keys "f" = 24 then 3 else 4 in
      let tr =
        {
          image = int_key keys "i";
          expected = bytes * int_key keys "s" * int_key keys "v";
          payload;
          virtual_placement = a = "T" && int_key keys "U" = 1;
        }
      in
      if more then t.transmission <- Some tr else end_transmission t tr
  | _, Some "p" when int_key keys "U" = 1 ->
      let id = int_key keys "i" in
      if List.mem_assoc id t.images then
        t.images <- (id, true) :: List.remove_assoc id t.images
  | _, Some "d" -> (
      match graphics_key keys "d" with
      | Some ("i" | "I") -> forget_image t (int_key keys "i")
      | Some ("a" | "A") | None ->
          List.iter (fun (id, _) -> forget_image t id) t.images
      | Some _ -> ())
  | _ -> ()

(* A request to the host: OSC 9 (iTerm2) is a notification's body, OSC 777
   (urxvt) [notify ; title ; body], and OSC 52 [selection ; data] a clipboard
   write with its data in base64; a read ([?]) asks for a clipboard this
   terminal does not have. *)
let host_request t n ending text =
  let request =
    match n with
    | 9 -> Some (Notification { title = ""; body = text })
    | 777 -> (
        match String.split_on_char ';' text with
        | "notify" :: title :: body ->
            Some (Notification { title; body = String.concat ";" body })
        | _ -> None)
    | _ -> (
        match Reply.of_osc ending ("52;" ^ text) with
        | Clipboard { selection; data; _ } ->
            Some (Clipboard { selection; data })
        | _ -> None)
  in
  Option.iter (fun r -> t.requests <- r :: t.requests) request

let close_string t ending =
  (match (t.osc, ending) with
  | Osc_text (0 | 2), (`St | `Bel) -> t.title <- Buffer.contents t.osc_text
  | Osc_text 8, (`St | `Bel) -> osc_hyperlink t (Buffer.contents t.osc_text)
  | Osc_text ((9 | 52 | 777) as n), ((`St | `Bel) as ending) ->
      host_request t n ending (Buffer.contents t.osc_text)
  | Osc_text n, ((`St | `Bel) as ending) ->
      osc_colours t n ending (Buffer.contents t.osc_text)
  | Osc_number ((104 | 110 | 111 | 112) as n), ((`St | `Bel) as ending) ->
      osc_colours t n ending ""
  | _ -> ());
  osc_drop t;
  (match (t.apc, ending) with
  | Apc_payload (keys, n), `St when String.length keys > 0 && keys.[0] = 'G' ->
      graphics_command t (String.sub keys 1 (String.length keys - 1)) n
  | Apc_keys b, `St when Buffer.length b > 0 && Buffer.nth b 0 = 'G' ->
      graphics_command t (Buffer.sub b 1 (Buffer.length b - 1)) 0
  | _ -> ());
  t.apc <- Apc_skip

(* ECMA-48 §5.4.2 lets a parameter be a string of sub-parameters, separated by
   colons, whose meaning the function defines. Only SGR defines any here (T.416
   colours, underline styles): in any other control sequence a parameter with
   sub-parameters makes the sequence malformed, so it has no effect, as one the
   parser rejects has none. xterm, a datapoint, does the same (charproc.c,
   "reject any controls that do not accept subparameters"). *)
let malformed (s : Ansi.Parser.sequence) =
  (not (s.marker = None && s.intermediates = "" && s.final = 'm'))
  &&
  let p = s.params in
  let rec go i = i < Params.count p && (Params.fields p i > 1 || go (i + 1)) in
  go 0

(* An update opens or ends where mode 2026 changes, which only a control
   sequence (DECSET, DECRST) or an escape sequence (RIS) does: the feed stops
   there. *)
let stop_at_update t was =
  if t.modes.synchronized_output <> was then Ansi.Parser.stop t.parser

(* Every token but text ends the unit in progress. REP reads it first. *)
let handle_token t = function
  | Ansi.Parser.Text s -> print t s
  | Csi s when malformed s -> ()
  | Csi { marker = None; intermediates = ""; final = 'b'; params } ->
      rep t (Params.pn params 0 ~default:1);
      break_text t
  | Csi s ->
      break_text t;
      let was = t.modes.synchronized_output in
      csi t s;
      stop_at_update t was
  | Control c ->
      break_text t;
      control t c
  | Esc { intermediates; final } ->
      break_text t;
      let was = t.modes.synchronized_output in
      esc t intermediates final;
      stop_at_update t was
  | Open o ->
      break_text t;
      open_string t o
  | Data s -> string_data t s
  | Close ending -> close_string t ending

let advance t bytes off len =
  Ansi.Parser.feed t.parser bytes ~off ~len (handle_token t)

let replies t =
  match t.replies with
  | [] -> []
  | replies ->
      t.replies <- [];
      List.rev replies

let rec advance_all t bytes off len =
  if len > 0 then
    let n = advance t bytes off len in
    advance_all t bytes (off + n) (len - n)

(* The first [advance] checks the range, even an empty one. *)
let feed t bytes off len =
  let n = advance t bytes off len in
  advance_all t bytes (off + n) (len - n);
  replies t

let feed_string t s = feed t (Bytes.unsafe_of_string s) 0 (String.length s)

let reset t =
  Ansi.Parser.reset t.parser;
  osc_drop t;
  break_text t;
  ris t

(* {1:resize Resizing} *)

(* Resize screen [s], keeping the rows around the cursor on [row]: a shrink
   drops rows below it first, then moves rows off the top (into the
   scrollback for the primary screen); a growth takes scrollback lines back.
   The result is how far the rows moved down. *)
let resize_screen t s ~row ~rows ~cols =
  let primary = s == t.primary in
  let old_rows = t.rows and old_cols = t.cols in
  let up =
    if rows >= old_rows then 0
    else max 0 (old_rows - rows - (old_rows - 1 - row))
  in
  if up > 0 then begin
    if primary then
      for r = 0 to up - 1 do
        push_line t s r
      done;
    Grid.shift s.grid
      { Grid.x = 0; y = 0; width = old_cols; height = old_rows }
      ~dx:0 ~dy:(-up) ~color:default_bg;
    Bytes.blit s.wrapped up s.wrapped 0 (old_rows - up)
  end;
  Grid.resize s.grid ~width:cols ~height:rows;
  if cols > old_cols then
    Grid.clear_rect ~color:default_bg s.grid ~x:old_cols ~y:0
      ~width:(cols - old_cols) ~height:rows;
  if rows > old_rows then
    Grid.clear_rect ~color:default_bg s.grid ~x:0 ~y:old_rows ~width:cols
      ~height:(rows - old_rows);
  let wrapped = Bytes.make rows '\000' in
  Bytes.blit s.wrapped 0 wrapped 0 (min rows (old_rows - up));
  s.wrapped <- wrapped;
  let down =
    if primary && rows > old_rows then min (rows - old_rows) t.history.count
    else 0
  in
  if down > 0 then begin
    Grid.shift s.grid
      { Grid.x = 0; y = 0; width = cols; height = rows }
      ~dx:0 ~dy:down ~color:default_bg;
    Bytes.blit wrapped 0 wrapped down (rows - down);
    for r = down - 1 downto 0 do
      let line = pop_line t.history in
      ignore (decompress_line line s.grid r : int);
      Bytes.set wrapped r (if line.wrapped then '\001' else '\000')
    done
  end;
  down - up

let resize t ~rows ~cols =
  check_cols "resize" cols;
  let rows = max 1 rows and cols = max 1 cols in
  if rows = t.rows && cols = t.cols then []
  else begin
    break_text t;
    let move (c : cursor) down =
      {
        row = max 0 (min (rows - 1) (c.row + down));
        col = min c.col (cols - 1);
        last_column = false;
      }
    in
    let active = screen t in
    let inactive = if active == t.primary then t.alternate else t.primary in
    let down = resize_screen t active ~row:t.cursor.row ~rows ~cols in
    let down' =
      resize_screen t inactive ~row:inactive.saved.cursor.row ~rows ~cols
    in
    active.saved <- { active.saved with cursor = move active.saved.cursor down };
    inactive.saved <-
      { inactive.saved with cursor = move inactive.saved.cursor down' };
    let tabs = default_tabs cols in
    Array.blit t.tabs 0 tabs 0 (min cols (Array.length t.tabs));
    t.rows <- rows;
    t.cols <- cols;
    t.tabs <- tabs;
    t.margins <- full_margins t;
    t.dirty.rows <- Bytes.make rows '\001';
    t.dirty.any <- true;
    t.dirty.cursor <- true;
    let c = move t.cursor down in
    set_cursor t ~row:c.row ~col:c.col ~last_column:false;
    (* The report of mode 2048, with no pixel size, which this terminal does
       not have. *)
    if t.modes.resize_reports then
      [ Reply.Resize { rows; cols; height = 0; width = 0 } ]
    else []
  end

(* {1:access Access} *)

let rows t = t.rows
let cols t = t.cols
let cursor_pos t = (t.cursor.row, t.cursor.col)
let last_column t = t.cursor.last_column
let cursor_style t = t.cursor_style
let modes t = t.modes
let title t = t.title
let identity t = t.identity
let requests t = List.rev t.requests
let clear_requests t = t.requests <- []
let color_scheme t = t.color_scheme

let set_color_scheme t scheme =
  if scheme = t.color_scheme then []
  else (
    t.color_scheme <- scheme;
    if t.modes.color_scheme_reports then [ Reply.Color_scheme scheme ] else [])

let keyboard t : Matrix_input.Keystroke.encoding =
  {
    cursor_keys = t.modes.cursor_keys;
    keypad = t.modes.keypad;
    new_line = t.modes.new_line;
    modify_other_keys = t.modify_other_keys;
    kitty = kitty_flags t;
  }

let mouse t : Matrix_input.Mouse_report.encoding option =
  match t.tracking with
  | `Off -> None
  | (`X10 | `Normal | `Button | `Any) as tracking ->
      Some { tracking; format = t.mouse_format }

let scrollback_capacity t = Array.length t.history.lines
let scrollback_size t = if t.modes.alternate_screen then 0 else t.history.count

(* Rows: [0] to [rows - 1] on the screen, [-1] the newest scrollback line
   and [-scrollback_size] the oldest. *)

(* The scrollback line of row [y < 0], the empty line if there is none. *)
let history_line t y =
  if y < 0 && y >= -scrollback_size t then newest t.history (-y - 1)
  else empty_line

let wrapped t y =
  if y >= 0 then y < t.rows && wrapped_row t y else (history_line t y).wrapped

(* The graphemes of screen row [row], a space for an empty cell. *)
let screen_text t row =
  let g = grid t and buf = Buffer.create t.cols in
  for col = 0 to t.cols - 1 do
    let idx = (row * t.cols) + col in
    if not (Grid.is_continuation g idx) then
      let s = Grid.get_text g idx in
      Buffer.add_string buf (if s = "" then " " else s)
  done;
  Buffer.contents buf

let trim_spaces s =
  let stop = ref (String.length s) in
  while !stop > 0 && s.[!stop - 1] = ' ' do
    decr stop
  done;
  if !stop = String.length s then s else String.sub s 0 !stop

let text t y =
  if y >= 0 then if y < t.rows then trim_spaces (screen_text t y) else ""
  else trim_spaces (history_line t y).text

(* The cells of the row, then blanks: those past the cells the row has are
   cleared alone, so that a row costs what it holds. *)
let blit_row t row dst ~y =
  if y < 0 || y >= Grid.height dst then
    invalid_arg
      (Printf.sprintf "Vte.blit_row: y = %d, not a row of a grid %d high" y
         (Grid.height dst));
  let written =
    if row >= t.rows then 0
    else if row >= 0 then (
      Grid.blit_region ~src:(grid t) ~dst ~src_x:0 ~src_y:row ~width:t.cols
        ~height:1 ~dst_x:0 ~dst_y:y;
      t.cols)
    else decompress_line (history_line t row) dst y
  in
  Grid.clear_rect ?color:default_bg_opt dst ~x:written ~y
    ~width:(Grid.width dst - written)
    ~height:1

let blit_view t ~offset dst =
  let offset = Int.max 0 (Int.min offset (scrollback_size t)) in
  for y = 0 to Grid.height dst - 1 do
    blit_row t (y - offset) dst ~y
  done

let is_dirty t = t.dirty.any

let dirty_rows t =
  let rows = ref [] in
  for row = t.rows - 1 downto 0 do
    if Bytes.get t.dirty.rows row <> '\000' then rows := row :: !rows
  done;
  !rows

let is_cursor_dirty t = t.dirty.cursor
let to_string t = String.concat "\n" (List.init t.rows (screen_text t))
