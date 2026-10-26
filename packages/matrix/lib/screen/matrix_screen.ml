(* packages/matrix/lib/screen/matrix_screen.ml *)

(* Allocation-conscious terminal screen with direct buffer mutation. *)

module Grid = Matrix_grid
open StdLabels
module Hit_grid = Hit_grid
module Region = Region

(* --- Types & Metrics --- *)

type stats_state = {
  mutable frame_count : int;
  mutable total_cells : int;
  mutable total_bytes : int;
}

type stats = { frame_count : int; total_cells : int; total_bytes : int }

type frame_metrics = {
  frame_count : int;
  cells : int;
  bytes : int;
  cursor_visible : bool;
}

type cursor = {
  position : (int * int) option;
  style : Ansi.cursor_style;
  blinking : bool;
  color : (int * int * int) option;
  visible : bool;
}

(* Lifecycle of the next buffer between presents. [Stale] holds previously
   presented cells after a swap and must be cleared before the buffer is
   observed or mutated; [Pristine] is cleared, and drawn into only through
   {!next_grid}; [Touched] has been built into or had a hit region added. The
   swap defers its clear to the next observation so a frame pays for exactly
   one clear no matter how many layers (the runtime handing the grid out, a
   renderer building into it, the swap) take part in it. *)
type next_state = Stale | Pristine | Touched
type render_mode = [ `Diff | `Full ]

(* --- Pictures ---

   A picture cell ([Grid.Unsafe.is_picture]) is written in the screen's
   [form]: the [graphics] it was given, or [`Blocks] without a cell size.
   Each (picture, box) the frame shows has an [entry], the picture fit in the
   box and resampled once: to the box's pixels, [cols * cw] by [rows * ch],
   for [`Kitty] and [`Sixel], and to two samples a cell for [`Blocks]. *)

type form = [ `Kitty | `Sixel | `Blocks ]

type entry = {
  pixels : bytes; (* RGBA, [w] by [h], straight alpha. *)
  w : int;
  h : int;
  mutable image : int; (* The kitty image the terminal holds, or [0]. *)
  mutable seen : bool; (* Shown by the frame being rendered. *)
}

type pictures = {
  mutable graphics : form;
  mutable cell : (int * int) option;
  mutable form : form;
  mutable direct : bool; (* Kitty ids at 24 bits, else 8 bits and a mark. *)
  entries : (int * int * int, entry) Hashtbl.t;
      (* By picture id, box columns and box rows. *)
  mutable stale : int list; (* Kitty images to delete before the next frame. *)
  mutable count : int; (* Kitty images made, for the next id. *)
  mutable transcript : int list; (* Kitty images of static rows. *)
  mutable base : int;
      (* The first id's number, random so that runs rarely meet, drawn when
         the first is made; [-1] before. *)
  sixels : int Dynarray.t; (* Picture cells the frame wrote, under [`Sixel]. *)
  mutable upper : Ansi.Color.t; (* The halves {!halves} last averaged. *)
  mutable lower : Ansi.Color.t;
}

(* All state the diff loop threads through its recursion, retained on the
   screen so a render allocates nothing. The emit path re-points the mutable
   fields at each frame's inputs before running the loop. *)
type diff_ctx = {
  mutable mode : render_mode;
  mutable prev : Grid.t; (* Diff baseline; never consulted in [`Full]. *)
  mutable curr : Grid.t;
  mutable out : Ansi.writer;
  sgr : Ansi.Sgr_state.t;
  mutable scratch : bytes; (* Grapheme byte scratch; high-water growth. *)
  mutable row_offset : int;
  mutable width : int;
  mutable height : int;
  mutable prev_width : int;
  mutable prev_height : int;
  mutable explicit_width : bool;
  mutable explicit_cursor_positioning : bool;
  mutable hyperlinks : bool;
  mutable units : Matrix_text.Segmenter.t;
      (* Display units of [curr]'s width method, to tell where the terminal
         would join two cells. *)
  mutable units_method : Matrix_text.width_method;
  mutable joint : bytes; (* The previous cell's scalars, for [units]. *)
  mutable pictures : pictures;
      (* [no_pictures] until the screen meets a picture or a form of its
         own, so a screen that shows none allocates nothing for them. *)
}

(* screen state - mutable internal state for maximum performance *)
type t = {
  stats : stats_state;
  (* The last frame's [frame_metrics], materialized only on demand. *)
  mutable m_cells : int;
  mutable m_bytes : int;
  mutable m_cursor_visible : bool;
  mutable e_cells : int;
  mutable e_output_len : int;
  mutable e_presented_height : int;
  (* Desired cursor state. The runtime owns terminal cursor emission. *)
  mutable cursor : cursor;
  (* Buffers. [current] is the last committed terminal state. [next] is the
     frame being built. [scroll_baseline] is scratch space for the
     scroll-adjusted previous state used while emitting a scroll-hinted diff. *)
  mutable current : Grid.t;
  mutable next : Grid.t;
  (* [false] after a resize: [current] no longer says what the terminal
     shows, so the next render paints every cell. *)
  mutable current_valid : bool;
  mutable next_state : next_state;
  scroll_baseline : Grid.t;
  mutable hit_current : Hit_grid.t;
  mutable hit_next : Hit_grid.t;
  (* State *)
  sgr_state : Ansi.Sgr_state.t;
  mutable row_offset : int;
  diff : diff_ctx;
  (* Retained ANSI output capacity; grows only past its high-water mark.
     [render_writer] is used only by the string-returning convenience API. *)
  render_writer : Ansi.writer;
  (* Capabilities: what the program asks for and what the terminal takes.
     Whether a render writes OSC 66 or repositions the cursor is derived from
     these in [emit_frame] alone. *)
  mutable prefer_explicit_width : bool;
  mutable explicit_width_capable : bool;
  mutable explicit_cursor_positioning_capable : bool;
  mutable hyperlinks_capable : bool;
}

(* --- Constants & Inline Helpers --- *)

let[@inline] clamp_height grid height = max 0 (min (Grid.height grid) height)

(* --- Core Rendering Logic ---

   The diff reads cells with [Grid.Unsafe], which skips the index check. Every
   index it reads is [y * width + x] for a grid of that width with [x] below
   the width and [y] below the grid's height: [emit_frame] sets [c.width] and
   [c.prev_width] to the grids' widths and clamps [c.height] and
   [c.prev_height] to their heights, and the loops stop at those bounds. *)

let[@inline] cell_changed (c : diff_ctx) y x idx curr_width =
  if curr_width <= 0 then false
  else
    match c.mode with
    | `Full -> true
    | `Diff ->
        if y >= c.prev_height || x >= c.prev_width then true
        else
          not
            (Grid.Unsafe.cells_equal c.prev ((y * c.prev_width) + x) c.curr idx)

(* The columns cell [idx] takes when written: its grapheme's width, [1] for an
   empty cell, which shows as a space as [Grid.to_text] writes it, and [0] for
   a continuation cell, which the run writes with its start cell. *)
let[@inline] cell_columns (c : diff_ctx) idx =
  let w = Grid.Unsafe.cell_width c.curr idx in
  if w > 0 || not (Grid.Unsafe.is_empty c.curr idx) then w else 1

let[@inline] cell_link (c : diff_ctx) idx =
  if c.hyperlinks then Grid.Unsafe.get_link c.curr idx else ""

let[@inline] ascii cp = cp land lnot 0x7f = 0

(* [buf], or a larger buffer when [buf] is shorter than [len]. *)
let[@inline] room buf len =
  if len <= Bytes.length buf then buf
  else Bytes.create (max (Bytes.length buf * 2) len)

let[@inline] first_scalar buf =
  Uchar.utf_decode_uchar (Bytes.get_utf_8_uchar buf 0)

let last_scalar buf n =
  let rec start i =
    if i > 0 && Char.code (Bytes.unsafe_get buf i) land 0xC0 = 0x80 then
      start (i - 1)
    else i
  in
  Uchar.utf_decode_uchar (Bytes.get_utf_8_uchar buf (start (n - 1)))

(* Writes the cell's scalars: [n] bytes in [c.scratch], or [cp] when it is
   ASCII. For wide graphemes, uses explicit_width (OSC 66) when available,
   otherwise falls back to cursor repositioning to prevent column drift in
   terminals that miscalculate grapheme widths. Emitters are called saturated
   so no closures are built. *)
let write_cell (c : diff_ctx) ~y ~x ~cell_width ~cp ~n =
  let w = c.out in
  if ascii cp then
    Ansi.Writer.write_char w (if cp = 0 then ' ' else Char.chr cp)
  else if n <= 0 then Ansi.Writer.write_char w ' '
  else if c.explicit_width && cell_width >= 2 then
    Ansi.explicit_width_bytes ~width:cell_width ~bytes:c.scratch ~off:0 ~len:n w
  else begin
    Ansi.Writer.write_subbytes w c.scratch 0 n;
    (* Fallback: reposition cursor after wide graphemes to prevent drift in
       terminals that support cursor addressing but not OSC 66. *)
    if c.explicit_cursor_positioning && cell_width >= 2 then
      let next_x = x + cell_width in
      if next_x < c.width then
        Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(next_x + 1) w
  end

(* kitty's row and column marks, gen/rowcolumn-diacritics.txt of its
   graphics protocol: mark [n] says [n]. *)
let diacritics =
  [|
    0x0305;
    0x030D;
    0x030E;
    0x0310;
    0x0312;
    0x033D;
    0x033E;
    0x033F;
    0x0346;
    0x034A;
    0x034B;
    0x034C;
    0x0350;
    0x0351;
    0x0352;
    0x0357;
    0x035B;
    0x0363;
    0x0364;
    0x0365;
    0x0366;
    0x0367;
    0x0368;
    0x0369;
    0x036A;
    0x036B;
    0x036C;
    0x036D;
    0x036E;
    0x036F;
    0x0483;
    0x0484;
    0x0485;
    0x0486;
    0x0487;
    0x0592;
    0x0593;
    0x0594;
    0x0595;
    0x0597;
    0x0598;
    0x0599;
    0x059C;
    0x059D;
    0x059E;
    0x059F;
    0x05A0;
    0x05A1;
    0x05A8;
    0x05A9;
    0x05AB;
    0x05AC;
    0x05AF;
    0x05C4;
    0x0610;
    0x0611;
    0x0612;
    0x0613;
    0x0614;
    0x0615;
    0x0616;
    0x0617;
    0x0657;
    0x0658;
    0x0659;
    0x065A;
    0x065B;
    0x065D;
    0x065E;
    0x06D6;
    0x06D7;
    0x06D8;
    0x06D9;
    0x06DA;
    0x06DB;
    0x06DC;
    0x06DF;
    0x06E0;
    0x06E1;
    0x06E2;
    0x06E4;
    0x06E7;
    0x06E8;
    0x06EB;
    0x06EC;
    0x0730;
    0x0732;
    0x0733;
    0x0735;
    0x0736;
    0x073A;
    0x073D;
    0x073F;
    0x0740;
    0x0741;
    0x0743;
    0x0745;
    0x0747;
    0x0749;
    0x074A;
    0x07EB;
    0x07EC;
    0x07ED;
    0x07EE;
    0x07EF;
    0x07F0;
    0x07F1;
    0x07F3;
    0x0816;
    0x0817;
    0x0818;
    0x0819;
    0x081B;
    0x081C;
    0x081D;
    0x081E;
    0x081F;
    0x0820;
    0x0821;
    0x0822;
    0x0823;
    0x0825;
    0x0826;
    0x0827;
    0x0829;
    0x082A;
    0x082B;
    0x082C;
    0x082D;
    0x0951;
    0x0953;
    0x0954;
    0x0F82;
    0x0F83;
    0x0F86;
    0x0F87;
    0x135D;
    0x135E;
    0x135F;
    0x17DD;
    0x193A;
    0x1A17;
    0x1A75;
    0x1A76;
    0x1A77;
    0x1A78;
    0x1A79;
    0x1A7A;
    0x1A7B;
    0x1A7C;
    0x1B6B;
    0x1B6D;
    0x1B6E;
    0x1B6F;
    0x1B70;
    0x1B71;
    0x1B72;
    0x1B73;
    0x1CD0;
    0x1CD1;
    0x1CD2;
    0x1CDA;
    0x1CDB;
    0x1CE0;
    0x1DC0;
    0x1DC1;
    0x1DC3;
    0x1DC4;
    0x1DC5;
    0x1DC6;
    0x1DC7;
    0x1DC8;
    0x1DC9;
    0x1DCB;
    0x1DCC;
    0x1DD1;
    0x1DD2;
    0x1DD3;
    0x1DD4;
    0x1DD5;
    0x1DD6;
    0x1DD7;
    0x1DD8;
    0x1DD9;
    0x1DDA;
    0x1DDB;
    0x1DDC;
    0x1DDD;
    0x1DDE;
    0x1DDF;
    0x1DE0;
    0x1DE1;
    0x1DE2;
    0x1DE3;
    0x1DE4;
    0x1DE5;
    0x1DE6;
    0x1DFE;
    0x20D0;
    0x20D1;
    0x20D4;
    0x20D5;
    0x20D6;
    0x20D7;
    0x20DB;
    0x20DC;
    0x20E1;
    0x20E7;
    0x20E9;
    0x20F0;
    0x2CEF;
    0x2CF0;
    0x2CF1;
    0x2DE0;
    0x2DE1;
    0x2DE2;
    0x2DE3;
    0x2DE4;
    0x2DE5;
    0x2DE6;
    0x2DE7;
    0x2DE8;
    0x2DE9;
    0x2DEA;
    0x2DEB;
    0x2DEC;
    0x2DED;
    0x2DEE;
    0x2DEF;
    0x2DF0;
    0x2DF1;
    0x2DF2;
    0x2DF3;
    0x2DF4;
    0x2DF5;
    0x2DF6;
    0x2DF7;
    0x2DF8;
    0x2DF9;
    0x2DFA;
    0x2DFB;
    0x2DFC;
    0x2DFD;
    0x2DFE;
    0x2DFF;
    0xA66F;
    0xA67C;
    0xA67D;
    0xA6F0;
    0xA6F1;
    0xA8E0;
    0xA8E1;
    0xA8E2;
    0xA8E3;
    0xA8E4;
    0xA8E5;
    0xA8E6;
    0xA8E7;
    0xA8E8;
    0xA8E9;
    0xA8EA;
    0xA8EB;
    0xA8EC;
    0xA8ED;
    0xA8EE;
    0xA8EF;
    0xA8F0;
    0xA8F1;
    0xAAB0;
    0xAAB2;
    0xAAB3;
    0xAAB7;
    0xAAB8;
    0xAABE;
    0xAABF;
    0xAAC1;
    0xFE20;
    0xFE21;
    0xFE22;
    0xFE23;
    0xFE24;
    0xFE25;
    0xFE26;
    0x10A0F;
    0x10A38;
    0x1D185;
    0x1D186;
    0x1D187;
    0x1D188;
    0x1D189;
    0x1D1AA;
    0x1D1AB;
    0x1D1AC;
    0x1D1AD;
    0x1D242;
    0x1D243;
    0x1D244;
  |]

let fresh_pictures () =
  {
    graphics = `Blocks;
    cell = None;
    form = `Blocks;
    direct = true;
    entries = Hashtbl.create 8;
    stale = [];
    count = 0;
    transcript = [];
    base = -1;
    sixels = Dynarray.create ();
    upper = Ansi.Color.transparent;
    lower = Ansi.Color.transparent;
  }

(* The state of every screen that has met no picture, which is never
   written: [own] gives a screen its own before a write. *)
let no_pictures = fresh_pictures ()

let own (c : diff_ctx) =
  if c.pictures == no_pictures then c.pictures <- fresh_pictures ()

let placeholder = Uchar.of_int 0x10EEEE
let alpha c = Ansi.Color.Packed.alpha (Ansi.Color.Packed.encode c)
let red c = Ansi.Color.Packed.red (Ansi.Color.Packed.encode c)
let green c = Ansi.Color.Packed.green (Ansi.Color.Packed.encode c)
let blue c = Ansi.Color.Packed.blue (Ansi.Color.Packed.encode c)

(* The [w] by [h] samples of picture [p] fit in a box of [bw] by [bh] pixels,
   aspect ratio kept and centered, each the area average of the pixels it
   covers, with what falls outside the picture transparent. *)
let resample p ~bw ~bh ~w ~h =
  let pw, ph = Grid.picture_size p and rgba = Grid.Unsafe.picture_rgba p in
  let out = Bytes.make (4 * w * h) '\000' in
  let fpw = float pw and fph = float ph in
  let s = Float.min (float bw /. fpw) (float bh /. fph) in
  let ox = (float bw -. (fpw *. s)) /. 2.
  and oy = (float bh -. (fph *. s)) /. 2. in
  (* The source pixels one sample covers, across and down. *)
  let sw = float bw /. float w /. s and sh = float bh /. float h /. s in
  let area = sw *. sh in
  for j = 0 to h - 1 do
    let y0 = ((float j *. float bh /. float h) -. oy) /. s in
    let cy0 = Float.max 0. y0 and cy1 = Float.min fph (y0 +. sh) in
    for i = 0 to w - 1 do
      let x0 = ((float i *. float bw /. float w) -. ox) /. s in
      let cx0 = Float.max 0. x0 and cx1 = Float.min fpw (x0 +. sw) in
      if cx1 > cx0 && cy1 > cy0 then begin
        let sa = ref 0. and sr = ref 0. and sg = ref 0. and sb = ref 0. in
        for y = int_of_float cy0 to int_of_float (Float.ceil cy1) - 1 do
          let wy = Float.min cy1 (float (y + 1)) -. Float.max cy0 (float y) in
          for x = int_of_float cx0 to int_of_float (Float.ceil cx1) - 1 do
            let wx = Float.min cx1 (float (x + 1)) -. Float.max cx0 (float x) in
            let k = 4 * ((y * pw) + x) in
            let a = float (Char.code rgba.[k + 3]) *. wx *. wy in
            sa := !sa +. a;
            sr := !sr +. (a *. float (Char.code rgba.[k]));
            sg := !sg +. (a *. float (Char.code rgba.[k + 1]));
            sb := !sb +. (a *. float (Char.code rgba.[k + 2]))
          done
        done;
        if !sa > 0. then begin
          let o = 4 * ((j * w) + i) in
          let byte v = Char.chr (max 0 (min 255 (Float.to_int (v +. 0.5)))) in
          Bytes.set out o (byte (!sr /. !sa));
          Bytes.set out (o + 1) (byte (!sg /. !sa));
          Bytes.set out (o + 2) (byte (!sb /. !sa));
          Bytes.set out (o + 3) (byte (!sa /. area))
        end
      end
    done
  done;
  out

let entry (p : pictures) (tile : Grid.Unsafe.tile) =
  let key = (Grid.Unsafe.picture_id tile.picture, tile.cols, tile.rows) in
  match Hashtbl.find_opt p.entries key with
  | Some e -> e
  | None ->
      (* Without a cell size, a cell is taken as 10 by 20 pixels. *)
      let cw, ch = Option.value p.cell ~default:(10, 20) in
      let bw = tile.cols * cw and bh = tile.rows * ch in
      let w, h =
        match p.form with
        | `Blocks -> (tile.cols, 2 * tile.rows)
        | `Kitty | `Sixel -> (bw, bh)
      in
      let pixels = resample tile.picture ~bw ~bh ~w ~h in
      let e = { pixels; w; h; image = 0; seen = false } in
      Hashtbl.add p.entries key e;
      e

(* The area average of the samples of [e] in columns [x0] to [x1 - 1] and
   rows [y0] to [y1 - 1]. *)
let average e x0 x1 y0 y1 =
  let sa = ref 0 and sr = ref 0 and sg = ref 0 and sb = ref 0 in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let k = 4 * ((y * e.w) + x) in
      let a = Char.code (Bytes.get e.pixels (k + 3)) in
      sa := !sa + a;
      sr := !sr + (a * Char.code (Bytes.get e.pixels k));
      sg := !sg + (a * Char.code (Bytes.get e.pixels (k + 1)));
      sb := !sb + (a * Char.code (Bytes.get e.pixels (k + 2)))
    done
  done;
  let n = max 1 ((x1 - x0) * (y1 - y0)) in
  if !sa = 0 then Ansi.Color.transparent
  else
    Ansi.Color.of_rgba (!sr / !sa) (!sg / !sa) (!sb / !sa) ((!sa + (n / 2)) / n)

(* The upper and lower halves of the tile, into [p.upper] and [p.lower]. *)
let halves p e (tile : Grid.Unsafe.tile) =
  let x0 = tile.col * e.w / tile.cols
  and x1 = (tile.col + 1) * e.w / tile.cols in
  let y0 = tile.row * e.h / tile.rows
  and y1 = (tile.row + 1) * e.h / tile.rows in
  let ym = (y0 + y1 + 1) / 2 in
  p.upper <- average e x0 (max x1 (x0 + 1)) y0 (max ym (y0 + 1));
  p.lower <- average e x0 (max x1 (x0 + 1)) (min ym (y1 - 1)) (max y1 (ym + 1))

let write_uchar (c : diff_ctx) u =
  let n = Bytes.set_utf_8_uchar c.scratch 0 u in
  Ansi.Writer.write_subbytes c.out c.scratch 0 n

(* A half block: U+2580 with the upper half as the foreground and the lower
   as the background, each composited over the cell's background, U+2584
   when only the lower half shows, and a space when neither does. *)
let write_halves (c : diff_ctx) idx =
  let p = c.pictures in
  let bg = Grid.Unsafe.get_bg c.curr idx in
  let up = p.upper and lo = p.lower in
  let style fg bg =
    Ansi.Sgr_state.update c.sgr c.out ~fg ~bg ~attrs:0 ~link:""
  in
  if alpha up = 0 && alpha lo = 0 then (
    style Ansi.Color.default_fg bg;
    Ansi.Writer.write_char c.out ' ')
  else if alpha up = 0 then (
    style (Ansi.Color.blend ~src:lo ~dst:bg) bg;
    write_uchar c (Uchar.of_int 0x2584))
  else (
    style
      (Ansi.Color.blend ~src:up ~dst:bg)
      (if alpha lo = 0 then bg else Ansi.Color.blend ~src:lo ~dst:bg);
    write_uchar c (Uchar.of_int 0x2580))

(* A kitty image id: at 24 bits, the foreground's RGB, its high byte 0; at
   8 bits, the foreground's palette entry and the high byte in a third
   mark, its middle bytes 0. Either way it never has the two non-zero middle
   bytes and high byte that kitty's icat gives its own. *)
let image_id p n =
  if p.base < 0 then
    p.base <- Random.State.bits (Random.State.make_self_init ()) land 0xFFFFFF;
  if p.direct then 1 + ((p.base + n) mod 0xFFFFFF)
  else
    let n = p.base + n in
    ((n / 255 mod 256) lsl 24) lor (1 + (n mod 255))

let id_color p id =
  if p.direct then
    Ansi.Color.of_rgb ((id lsr 16) land 255) ((id lsr 8) land 255) (id land 255)
  else Ansi.Color.indexed (id land 255)

(* A kitty Unicode placeholder: U+10EEEE with the row's and column's marks,
   and the high byte's when it is not 0, in the image's id color, on the
   cell's background. A box past the marks' 297 rows or columns is written
   in half blocks there. *)
let write_placeholder (c : diff_ctx) idx e (tile : Grid.Unsafe.tile) =
  let n = Array.length diacritics in
  if tile.row >= n || tile.col >= n then (
    halves c.pictures e tile;
    write_halves c idx)
  else begin
    Ansi.Sgr_state.update c.sgr c.out
      ~fg:(id_color c.pictures e.image)
      ~bg:(Grid.Unsafe.get_bg c.curr idx)
      ~attrs:0 ~link:"";
    write_uchar c placeholder;
    write_uchar c (Uchar.of_int diacritics.(tile.row));
    write_uchar c (Uchar.of_int diacritics.(tile.col));
    let high = e.image lsr 24 in
    if high <> 0 then write_uchar c (Uchar.of_int diacritics.(high))
  end

(* Writes picture cell [idx], at column [x] of row [y], which shows [tile] of
   entry [e]. *)
let write_tile (c : diff_ctx) ~y ~x idx tile e =
  let p = c.pictures in
  match p.form with
  | `Kitty -> write_placeholder c idx e tile
  | `Blocks ->
      halves p e tile;
      write_halves c idx
  | `Sixel ->
      halves p e tile;
      write_halves c idx;
      (* The frame's last row is the terminal's, where a sixel would scroll
         the screen (xterm's graphics_sixel.c, at each DECGNL). *)
      if y + 1 < c.height then Dynarray.add_last p.sixels ((y * c.width) + x)

let base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

(* [b64 w s ~pos ~len] writes [len] bytes of [s] from [pos] in base64. *)
let b64 w s ~pos ~len =
  let i = ref pos and stop = pos + len in
  while !i < stop do
    let byte k =
      if !i + k < stop then Char.code (Bytes.get s (!i + k)) else 0
    in
    let n = (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2 in
    let rem = stop - !i in
    Ansi.Writer.write_char w base64.[(n lsr 18) land 63];
    Ansi.Writer.write_char w base64.[(n lsr 12) land 63];
    Ansi.Writer.write_char w
      (if rem > 1 then base64.[(n lsr 6) land 63] else '=');
    Ansi.Writer.write_char w (if rem > 2 then base64.[n land 63] else '=');
    i := !i + 3
  done

(* Sends the box's pixels as image [id] with a virtual placement of [cols] by
   [rows] cells, quietly, in chunks of 4096 base64 bytes. *)
let transmit w e ~id ~cols ~rows =
  let total = Bytes.length e.pixels and chunk = 3072 in
  let rec go pos first =
    let len = min chunk (total - pos) in
    let more = if pos + len < total then 1 else 0 in
    Ansi.Writer.write_string w
      (if first then
         Printf.sprintf "\027_Ga=T,U=1,f=32,s=%d,v=%d,i=%d,c=%d,r=%d,q=2,m=%d;"
           e.w e.h id cols rows more
       else Printf.sprintf "\027_Gm=%d;" more);
    b64 w e.pixels ~pos ~len;
    Ansi.Writer.write_string w "\027\\";
    if more = 1 then go (pos + len) false
  in
  go 0 true

let delete w id =
  Ansi.Writer.write_string w (Printf.sprintf "\027_Ga=d,d=I,i=%d,q=2\027\\" id)

(* Sixel: a 6x7x6 cube of 252 registers, which an ordered 4x4 dither phased
   on the box's pixels fills in, so that crops cut apart join with no
   seam. *)
let bayer = [| 0; 8; 2; 10; 12; 4; 14; 6; 3; 11; 1; 9; 15; 7; 13; 5 |]

let level v levels t =
  let x =
    (float v *. float (levels - 1) /. 255.) +. ((float t -. 7.5) /. 16.)
  in
  max 0 (min (levels - 1) (Float.to_int (Float.round x)))

(* The sixel of the cells of columns [x0] to [x1] and rows [y0] to [y1] of
   the frame, which show tiles from column [col] and row [row] of the box of
   entry [e], each pixel composited over its cell's background. A pixel that
   stays mostly transparent is left out, so the half block under it shows. *)
let write_sixel (c : diff_ctx) e ~cw ~ch ~x0 ~x1 ~y0 ~y1 ~col ~row =
  let w = (x1 - x0 + 1) * cw and h = (y1 - y0 + 1) * ch in
  let px0 = col * cw and py0 = row * ch in
  let index = Bytes.make (w * h) '\255' in
  let used = Array.make 252 false in
  for v = 0 to h - 1 do
    for u = 0 to w - 1 do
      let bx = px0 + u and by = py0 + v in
      let k = 4 * ((by * e.w) + bx) in
      let a = Char.code (Bytes.get e.pixels (k + 3)) in
      if a > 0 then begin
        let bg =
          Grid.Unsafe.get_bg c.curr (((y0 + (v / ch)) * c.width) + x0 + (u / cw))
        in
        let px =
          Ansi.Color.blend
            ~src:
              (Ansi.Color.of_rgba
                 (Char.code (Bytes.get e.pixels k))
                 (Char.code (Bytes.get e.pixels (k + 1)))
                 (Char.code (Bytes.get e.pixels (k + 2)))
                 a)
            ~dst:bg
        in
        if alpha px >= 128 then begin
          let t = bayer.((by land 3 * 4) + (bx land 3)) in
          let n =
            (level (red px) 6 t * 42)
            + (level (green px) 7 t * 6)
            + level (blue px) 6 t
          in
          used.(n) <- true;
          Bytes.set index ((v * w) + u) (Char.unsafe_chr n)
        end
      end
    done
  done;
  let out = c.out in
  Ansi.cursor_position ~row:(c.row_offset + y0 + 1) ~col:(x0 + 1) out;
  Ansi.Writer.write_string out (Printf.sprintf "\027P0;1;0q\"1;1;%d;%d" w h);
  let percent l levels = l * 100 / (levels - 1) in
  Array.iteri
    ~f:(fun n u ->
      if u then
        Ansi.Writer.write_string out
          (Printf.sprintf "#%d;2;%d;%d;%d" n
             (percent (n / 42) 6)
             (percent (n / 6 mod 7) 7)
             (percent (n mod 6) 6)))
    used;
  let band_used = Array.make 252 false in
  let run ch count =
    if count > 3 then
      Ansi.Writer.write_string out (Printf.sprintf "!%d%c" count ch)
    else
      for _ = 1 to count do
        Ansi.Writer.write_char out ch
      done
  in
  for band = 0 to ((h + 5) / 6) - 1 do
    Array.fill band_used ~pos:0 ~len:252 false;
    for v = band * 6 to min h ((band * 6) + 6) - 1 do
      for u = 0 to w - 1 do
        let n = Char.code (Bytes.get index ((v * w) + u)) in
        if n < 252 then band_used.(n) <- true
      done
    done;
    let first = ref true in
    Array.iteri
      ~f:(fun n u ->
        if u then begin
          if not !first then Ansi.Writer.write_char out '$';
          first := false;
          Ansi.Writer.write_string out (Printf.sprintf "#%d" n);
          (* The run of the same sixel character so far, dropped at the
             line's end when it is empty. *)
          let prev = ref '?' and count = ref 0 in
          for u = 0 to w - 1 do
            let bits = ref 0 in
            for k = 0 to 5 do
              let v = (band * 6) + k in
              if v < h && Char.code (Bytes.get index ((v * w) + u)) = n then
                bits := !bits lor (1 lsl k)
            done;
            let ch = Char.chr (63 + !bits) in
            if ch = !prev then incr count
            else (
              run !prev !count;
              prev := ch;
              count := 1)
          done;
          if !prev <> '?' then run !prev !count
        end)
      band_used;
    Ansi.Writer.write_char out '-'
  done;
  Ansi.Writer.write_string out "\027\\"

(* A rectangle of picture cells of one box, columns [x0] to [x1] and rows
   [y0] to [y1] of the frame, whose first cell shows [tile]. *)
type rect = {
  x0 : int;
  x1 : int;
  y0 : int;
  mutable y1 : int;
  tile : Grid.Unsafe.tile;
}

let same_box (a : Grid.Unsafe.tile) (b : Grid.Unsafe.tile) =
  a.picture == b.picture && a.cols = b.cols && a.rows = b.rows

(* The sixels of the picture cells the frame wrote, as rectangles: runs of
   a row that show consecutive tiles of one box, each merged with the run
   below it while that one has the same columns and shows the next row of
   tiles. *)
let write_sixels (c : diff_ctx) =
  let p = c.pictures and cells = c.pictures.sixels in
  (match p.cell with
  | None -> ()
  | Some (cw, ch) ->
      let n = Dynarray.length cells and rects = ref [] in
      let i = ref 0 in
      while !i < n do
        let first = Dynarray.get cells !i in
        let tile = Grid.Unsafe.tile c.curr first in
        let y = first / c.width and x0 = first mod c.width in
        let j = ref (!i + 1) in
        while
          !j < n
          &&
          let k = !j - !i in
          let idx = Dynarray.get cells !j in
          idx = first + k
          && x0 + k < c.width
          &&
          let t = Grid.Unsafe.tile c.curr idx in
          same_box t tile && t.row = tile.row && t.col = tile.col + k
        do
          incr j
        done;
        let x1 = x0 + (!j - !i) - 1 in
        (match
           List.find_opt
             ~f:(fun r ->
               r.x0 = x0 && r.x1 = x1
               && r.y1 = y - 1
               && same_box r.tile tile && r.tile.col = tile.col
               && tile.row = r.tile.row + (y - r.y0))
             !rects
         with
        | Some r -> r.y1 <- y
        | None -> rects := { x0; x1; y0 = y; y1 = y; tile } :: !rects);
        i := !j
      done;
      List.iter
        ~f:(fun r ->
          write_sixel c (entry p r.tile) ~cw ~ch ~x0:r.x0 ~x1:r.x1 ~y0:r.y0
            ~y1:r.y1 ~col:r.tile.col ~row:r.tile.row)
        (List.rev !rects));
  Dynarray.clear cells

(* Adds the [n] bytes of scalars in [buf] from [pos] to [units]. *)
let rec add_scalars units buf pos n =
  if pos < n then begin
    let d = Bytes.get_utf_8_uchar buf pos in
    ignore (Matrix_text.Segmenter.add units (Uchar.utf_decode_uchar d) : bool);
    add_scalars units buf (pos + Uchar.utf_decode_length d) n
  end

(* Whether a cell whose first scalar is [first] would join cell [prev],
   written just before it and ending with the scalar [last]: whether the
   terminal, cutting the scalars it receives into display units as the
   grid's width method does, would make one unit of their graphemes, as of a
   letter and an emoji modifier, or of a Prepend character and a letter. A
   control function, which ends the unit in progress, keeps two joining cells
   apart.

   [prev] starts a unit, by itself or after such a control, so the units
   depend on its scalars only. Most pairs of scalars never join, which
   [Segmenter.may_continue] tells from the pair; the others go through
   [c.units]. *)
let joins (c : diff_ctx) ~prev ~prev_cp ~last first =
  Matrix_text.Segmenter.may_continue c.units_method last first
  &&
  let n =
    if prev_cp >= 0 then 0
    else (
      c.joint <- room c.joint (Grid.Unsafe.cell_text_length c.curr prev);
      Grid.Unsafe.blit_cell_text c.curr prev c.joint ~pos:0)
  in
  Matrix_text.Segmenter.reset c.units;
  if n > 0 then add_scalars c.units c.joint 0 n
  else ignore (Matrix_text.Segmenter.add c.units last : bool);
  not (Matrix_text.Segmenter.add c.units first)

(* Write consecutive changed cells, return the new x position. Cells written =
   new_x - start_x. [prev] is the cell written just before, of code point
   [prev_cp], width [prev_width] and last scalar [last], or [-1] after a
   cursor motion. *)
let rec write_run (c : diff_ctx) y x ~prev ~prev_cp ~prev_width ~last =
  if x >= c.width then x
  else
    let idx = (y * c.width) + x in
    let cell_width = cell_columns c idx in
    if cell_width <= 0 then x
    else if not (cell_changed c y x idx cell_width) then x
    else
      let cp = Grid.Unsafe.get_codepoint c.curr idx in
      if cp < 0 && Grid.Unsafe.is_picture c.curr idx then begin
        own c;
        let tile = Grid.Unsafe.tile c.curr idx in
        write_tile c ~y ~x idx tile (entry c.pictures tile);
        write_run c y (x + 1) ~prev:(-1) ~prev_cp:0 ~prev_width:1
          ~last:Uchar.min
      end
      else begin
        (* The cell's scalars: [n] bytes in [c.scratch], or [cp] when it is
         ASCII; a space when there are none. *)
        let n =
          if ascii cp then 0
          else (
            c.scratch <-
              room c.scratch (Grid.Unsafe.cell_text_length c.curr idx);
            Grid.Unsafe.blit_cell_text c.curr idx c.scratch ~pos:0)
        in
        let simple = cp >= 0 || n <= 0 in
        let first =
          if simple then Uchar.unsafe_of_int (if cp < 0x20 then 0x20 else cp)
          else first_scalar c.scratch
        in
        let final = if simple then first else last_scalar c.scratch n in
        if
          prev >= 0
          (* Two ASCII cells never join. *)
          && (not (ascii cp && ascii prev_cp))
          (* A control stands between a wide cell and its neighbors when it
           is written in OSC 66, or followed by a cursor motion. *)
          && (not
                (prev_width >= 2
                 && (c.explicit_width || c.explicit_cursor_positioning)
                || (c.explicit_width && cell_width >= 2)))
          && joins c ~prev ~prev_cp ~last first
        then Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
        (* Emit style/color through zero-alloc accessors. *)
        Ansi.Sgr_state.update c.sgr c.out
          ~fg:(Grid.Unsafe.get_fg c.curr idx)
          ~bg:(Grid.Unsafe.get_bg c.curr idx)
          ~attrs:(Grid.Unsafe.get_attrs c.curr idx)
          ~link:(cell_link c idx);
        write_cell c ~y ~x ~cell_width ~cp ~n;
        write_run c y (x + cell_width) ~prev:idx ~prev_cp:cp
          ~prev_width:cell_width ~last:final
      end

(* Process columns in a row, return total cells updated in this row. *)
let rec process_cols (c : diff_ctx) y x row_cells =
  if x >= c.width then row_cells
  else
    let idx = (y * c.width) + x in
    let cell_width = cell_columns c idx in
    (* A continuation cell outside a run would belong to no start cell, which
       a grid never holds. *)
    if cell_width <= 0 then process_cols c y (x + 1) row_cells
    else if cell_changed c y x idx cell_width then begin
      (* Move cursor to start of changed run, then write it. *)
      Ansi.cursor_position ~row:(c.row_offset + y + 1) ~col:(x + 1) c.out;
      let start_x = x in
      let new_x =
        write_run c y x ~prev:(-1) ~prev_cp:0 ~prev_width:0 ~last:Uchar.min
      in
      (* Close active hyperlink. SGR state is preserved across the gap so the
         next run on this row skips re-emission when unchanged. *)
      Ansi.Sgr_state.close_link c.sgr c.out;
      process_cols c y new_x (row_cells + (new_x - start_x))
    end
    else process_cols c y (x + cell_width) row_cells

(* Process all rows, accumulate total cells updated. *)
let rec process_rows (c : diff_ctx) y total_cells =
  if y >= c.height then total_cells
  else process_rows c (y + 1) (total_cells + process_cols c y 0 0)

(* The hot loop. Scans the grid, diffs against the previous frame, and emits
   sequences while reusing the retained context and scratch buffers. *)
let run_diff (c : diff_ctx) =
  Ansi.Sgr_state.reset c.sgr;
  let total = process_rows c 0 0 in
  Ansi.Sgr_state.close_link c.sgr c.out;
  if total > 0 then Ansi.emit Ansi.reset c.out;
  Ansi.Sgr_state.reset c.sgr;
  total

(* --- Frame Lifecycle --- *)

let[@inline] swap_buffers r =
  let old_current = r.current in
  r.current <- r.next;
  r.next <- old_current;
  let old_hit_current = r.hit_current in
  r.hit_current <- r.hit_next;
  r.hit_next <- old_hit_current;
  (* Defer the clear of the swapped-in buffers to their next observation. *)
  r.next_state <- Stale

let refresh_next r =
  Grid.clear r.next;
  Hit_grid.clear r.hit_next;
  r.next_state <- Pristine

let[@inline] ensure_next_fresh r = if r.next_state = Stale then refresh_next r

let clear_unpresented_rows r presented_height =
  let height = Grid.height r.next in
  if presented_height < height then (
    let y = max 0 presented_height in
    let h = height - y in
    Grid.clear_rect r.next ~x:0 ~y ~width:(Grid.width r.next) ~height:h;
    Hit_grid.add r.hit_next ~x:0 ~y ~width:(Grid.width r.next) ~height:h
      ~id:Hit_grid.empty_id)

let finalize_frame r =
  (* Swap buffers; [next] is cleared to provide a fresh canvas for the
     builder. *)
  swap_buffers r;

  (* Update Stats *)
  r.stats.frame_count <- r.stats.frame_count + 1;
  r.stats.total_cells <- r.stats.total_cells + r.e_cells;
  r.stats.total_bytes <- r.stats.total_bytes + r.e_output_len;

  (* Snapshot Metrics *)
  r.m_cells <- r.e_cells;
  r.m_bytes <- r.e_output_len;
  r.m_cursor_visible <- r.cursor.visible

let presented_height r height = clamp_height r.next height

(* --- Input / Cursor Handling --- *)

(* --- Public API --- *)

type scroll_hint = { top : int; bottom : int; delta : int }
type viewport = { y : int; height : int }

let normalize_scroll_hint ~row_offset ~height ~current hint =
  let { top; bottom; delta } = hint in
  let current_height = Grid.height current in
  let render_height = max 0 (min current_height height) in
  let top = max 0 top in
  let bottom = min (render_height - 1) bottom in
  (* A shift by the region's height or more keeps none of its rows, so the
     diff repaints them all anyway; xterm 398 also blanks the rows outside
     the region on such an SU. *)
  if delta = 0 || top >= bottom || abs delta >= bottom - top + 1 then None
  else Some (top, bottom, delta, max 0 row_offset)

let apply_scroll_hint ~(writer : Ansi.writer) ~row_offset ~height ~current hint
    =
  match normalize_scroll_hint ~row_offset ~height ~current hint with
  | None -> ()
  | Some (top, bottom, delta, row_offset) ->
      (* Shift the previous buffer to match the hardware scroll. After this,
         the diff loop sees only the newly-revealed edge rows as changes. *)
      Grid.shift current
        {
          x = 0;
          y = top;
          width = Grid.width current;
          height = bottom - top + 1;
        }
        ~dx:0 ~dy:(-delta) ~color:Ansi.Color.transparent;
      (* Tell the terminal to perform the same shift via DECSTBM. *)
      Ansi.emit
        (Ansi.set_scrolling_region
           ~top:(row_offset + top + 1)
           ~bottom:(row_offset + bottom + 1))
        writer;
      if delta > 0 then Ansi.emit (Ansi.scroll_up ~n:delta) writer
      else Ansi.emit (Ansi.scroll_down ~n:(-delta)) writer;
      Ansi.emit Ansi.reset_scrolling_region writer;
      Ansi.cursor_position ~row:(row_offset + 1) ~col:1 writer

(* In [`Full] mode the returned grid is never consulted; [r.current] stands
   in so the caller needs no option. *)
let prepare_diff_baseline (r : t) ~mode ~scroll_hint ~row_offset ~height
    ~(writer : Ansi.writer) =
  match (mode, scroll_hint) with
  | `Full, _ -> r.current
  | `Diff, None -> r.current
  | `Diff, Some hint ->
      (* Scroll hints describe terminal-side movement. Apply them to a
         temporary baseline so output failures cannot corrupt [current]. *)
      Grid.blit ~src:r.current ~dst:r.scroll_baseline;
      apply_scroll_hint ~writer ~row_offset ~height ~current:r.scroll_baseline
        hint;
      r.scroll_baseline

(* Marks the entries of the pictures the next buffer shows. *)
let scan_pictures r =
  let g = r.next in
  if Grid.Unsafe.has_pictures g then begin
    own r.diff;
    let p = r.diff.pictures in
    for idx = 0 to (Grid.width g * Grid.height g) - 1 do
      if Grid.Unsafe.is_picture g idx then
        (entry p (Grid.Unsafe.tile g idx)).seen <- true
    done
  end

(* Deletes the kitty images the screen let go, then sends those the next
   buffer shows that the terminal does not hold. *)
let send_into r w =
  let p = r.diff.pictures in
  if p.stale <> [] then (
    List.iter ~f:(delete w) (List.rev p.stale);
    p.stale <- []);
  if p.form = `Kitty then begin
    scan_pictures r;
    Hashtbl.iter
      (fun (_, cols, rows) e ->
        if e.seen && e.image = 0 then begin
          let id = image_id p p.count in
          p.count <- p.count + 1;
          e.image <- id;
          transmit w e ~id ~cols ~rows
        end)
      p.entries
  end

(* After the frame's cells: the sixels over the picture cells it wrote, and
   the entries of the pictures it no longer shows let go, their kitty
   images deleted before the next frame. *)
let finish_pictures r =
  let p = r.diff.pictures in
  if Dynarray.length p.sixels > 0 then write_sixels r.diff;
  if Hashtbl.length p.entries > 0 then begin
    scan_pictures r;
    Hashtbl.filter_map_inplace
      (fun _ e ->
        if e.seen then (
          e.seen <- false;
          Some e)
        else (
          if e.image <> 0 then p.stale <- e.image :: p.stale;
          None))
      p.entries
  end

(* The terminal holds none of the images the screen sent: they are deleted
   before the next frame and sent again as it needs them. *)
let forget_images r =
  let p = r.diff.pictures in
  Hashtbl.iter
    (fun _ e ->
      if e.image <> 0 then (
        p.stale <- e.image :: p.stale;
        e.image <- 0))
    p.entries

let emit_frame (r : t) ~(mode : render_mode) ~scroll_hint ~viewport
    ~(writer : Ansi.writer) =
  let row_offset =
    match viewport with None -> max 0 r.row_offset | Some v -> max 0 v.y
  in
  let height =
    match viewport with
    | None -> Grid.height r.next
    | Some v -> clamp_height r.next v.height
  in
  let output_start = Ansi.Writer.len writer in
  let c = r.diff in
  c.mode <- mode;
  c.curr <- r.next;
  c.out <- writer;
  c.row_offset <- row_offset;
  c.width <- Grid.width r.next;
  c.height <- clamp_height r.next height;
  c.explicit_width <- r.prefer_explicit_width && r.explicit_width_capable;
  c.explicit_cursor_positioning <-
    (not c.explicit_width) && r.explicit_cursor_positioning_capable;
  c.hyperlinks <- r.hyperlinks_capable;
  let width_method = Grid.width_method r.next in
  if width_method <> c.units_method then (
    c.units <- Matrix_text.Segmenter.create width_method;
    c.units_method <- width_method);
  let prev =
    prepare_diff_baseline r ~mode ~scroll_hint ~row_offset ~height ~writer
  in
  c.prev <- prev;
  c.prev_width <- Grid.width prev;
  c.prev_height <- min (Grid.height prev) c.height;
  send_into r writer;
  let cells = run_diff c in
  finish_pictures r;
  r.e_cells <- cells;
  r.e_output_len <- Ansi.Writer.len writer - output_start;
  r.e_presented_height <- presented_height r height

let commit_frame r =
  clear_unpresented_rows r r.e_presented_height;
  finalize_frame r

let render_into ?(full = false) ?scroll_hint ?viewport frame writer =
  (* A render with no build since the last one renders the blank frame the
     last one left, so the clear the swap deferred happens now. *)
  ensure_next_fresh frame;
  let mode = if full || not frame.current_valid then `Full else `Diff in
  (try emit_frame frame ~mode ~scroll_hint ~viewport ~writer
   with exn ->
     let bt = Printexc.get_raw_backtrace () in
     (* Images sent in the bytes given up may not have reached the terminal,
        so they are sent again, in a full render. *)
     if Hashtbl.length frame.diff.pictures.entries > 0 then (
       forget_images frame;
       Dynarray.clear frame.diff.pictures.sixels;
       frame.current_valid <- false);
     frame.diff.out <- frame.render_writer;
     Ansi.Sgr_state.reset frame.sgr_state;
     Printexc.raise_with_backtrace exn bt);
  frame.diff.out <- frame.render_writer;
  frame.current_valid <- true;
  commit_frame frame

let render ?full ?scroll_hint ?viewport frame =
  let writer = frame.render_writer in
  Ansi.Writer.reset_pos writer;
  render_into ?full ?scroll_hint ?viewport frame writer;
  Ansi.Writer.contents writer

(* Creation & Management *)

let create ?width_method ?(cursor_visible = true) ?(explicit_width = false) () =
  let w_method = match width_method with Some m -> m | None -> `Unicode in

  (* All buffers share one grapheme store and link registry so the diff can
     compare complex cells and hyperlinks by handle across frames. *)
  let current = Grid.create ~width:1 ~height:1 ~width_method:w_method () in
  let sgr_state = Ansi.Sgr_state.create () in
  let render_writer = Ansi.Writer.create 0 in
  {
    stats = { frame_count = 0; total_cells = 0; total_bytes = 0 };
    m_cells = 0;
    m_bytes = 0;
    m_cursor_visible = cursor_visible;
    e_cells = 0;
    e_output_len = 0;
    e_presented_height = 0;
    cursor =
      {
        position = None;
        style = `Block;
        blinking = false;
        color = None;
        visible = cursor_visible;
      };
    current;
    next = Grid.create_like current ~width:1 ~height:1;
    current_valid = true;
    next_state = Pristine;
    scroll_baseline = Grid.create_like current ~width:1 ~height:1;
    hit_current = Hit_grid.create ~width:1 ~height:1;
    hit_next = Hit_grid.create ~width:1 ~height:1;
    sgr_state;
    row_offset = 0;
    prefer_explicit_width = explicit_width;
    explicit_width_capable = true;
    explicit_cursor_positioning_capable = false;
    hyperlinks_capable = true;
    diff =
      {
        mode = `Diff;
        prev = current;
        curr = current;
        out = render_writer;
        sgr = sgr_state;
        (* Large enough for any grapheme. *)
        scratch = Bytes.create 1024;
        row_offset = 0;
        width = 0;
        height = 0;
        prev_width = 0;
        prev_height = 0;
        explicit_width = false;
        explicit_cursor_positioning = false;
        hyperlinks = true;
        units = Matrix_text.Segmenter.create w_method;
        units_method = w_method;
        joint = Bytes.create 64;
        pictures = no_pictures;
      };
    render_writer;
  }

let reset t =
  Grid.clear t.next;
  Grid.clear t.scroll_baseline;
  Hit_grid.clear t.hit_current;
  Hit_grid.clear t.hit_next;
  t.next_state <- Pristine;
  t.stats.frame_count <- 0;
  t.stats.total_cells <- 0;
  t.stats.total_bytes <- 0;
  Ansi.Sgr_state.reset t.sgr_state

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_screen.resize: width and height must be > 0";
  if width <> Grid.width t.current || height <> Grid.height t.current then (
    Grid.resize_clear t.current ~width ~height;
    Grid.resize_clear t.next ~width ~height;
    Grid.resize_clear t.scroll_baseline ~width ~height;
    (* Hit_grid.resize already clears unconditionally, no need to clear again *)
    Hit_grid.resize t.hit_current ~width ~height;
    Hit_grid.resize t.hit_next ~width ~height;
    t.next_state <- Pristine;
    t.current_valid <- false;
    t.cursor <-
      {
        t.cursor with
        position =
          Option.map
            (fun (x, y) -> (min (width - 1) x, min (height - 1) y))
            t.cursor.position;
      })

(* Each build owns the complete frame: the next grid and hit grid are
   guaranteed cleared (or freshly resized) before [f] runs, so a second
   build before a present replaces the superseded pass rather than
   compositing over it. A [Pristine] buffer skips the redundant clear. *)
let internal_build t ~width ~height f =
  if width <= 0 || height <= 0 then (
    if t.next_state <> Pristine then refresh_next t;
    t)
  else (
    if width <> Grid.width t.next || height <> Grid.height t.next then
      resize t ~width ~height
    else if t.next_state <> Pristine then refresh_next t;
    t.next_state <- Touched;
    f t.next t.hit_next;
    t)

let build t ~width ~height f =
  ignore (internal_build t ~width ~height (fun grid hits -> f grid hits) : t)

(* Handing out the next buffers applies any pending post-present clear and
   marks them mutated, so the observable contract — the next buffer is clear
   after a render — holds without an eager clear in the swap. *)
let next_grid frame =
  ensure_next_fresh frame;
  frame.next

let current_grid frame = frame.current

let next_hit_grid frame =
  ensure_next_fresh frame;
  frame.hit_next

let query_hit frame ~x ~y = Hit_grid.get frame.hit_current ~x ~y
let row_offset t = t.row_offset
let set_row_offset t offset = t.row_offset <- max 0 offset

let invalidate_presented t =
  (* Clear the current buffer so diff sees all cells as changed. This maintains
     the invariant: current = what's on terminal. After erasing the terminal
     region, the terminal is "blank", so current should also be blank. *)
  Grid.clear t.current;
  forget_images t

let active_height (t : t) =
  ensure_next_fresh t;
  Grid.active_height t.next

let stats t =
  {
    frame_count = t.stats.frame_count;
    total_cells = t.stats.total_cells;
    total_bytes = t.stats.total_bytes;
  }

let last_metrics t =
  {
    frame_count = t.stats.frame_count;
    cells = t.m_cells;
    bytes = t.m_bytes;
    cursor_visible = t.m_cursor_visible;
  }

let clamp_byte v = max 0 (min 255 v)

let set_cursor t cursor =
  let color =
    Option.map
      (fun (r, g, b) -> (clamp_byte r, clamp_byte g, clamp_byte b))
      cursor.color
  in
  (* Store the normalized state so [cursor] reads allocate nothing. *)
  let position =
    match cursor.position with
    | Some (x, y) when x < 0 || y < 0 -> Some (max 0 x, max 0 y)
    | p -> p
  in
  t.cursor <- { cursor with position; color }

let cursor t = t.cursor

let apply_capabilities r ~explicit_width ~explicit_cursor_positioning
    ~hyperlinks ~color_depth ~graphics ~cell_size =
  r.explicit_width_capable <- explicit_width;
  r.explicit_cursor_positioning_capable <- explicit_cursor_positioning;
  r.hyperlinks_capable <- hyperlinks;
  Ansi.Sgr_state.set_color_depth r.sgr_state color_depth;
  let p = r.diff.pictures in
  let direct = color_depth = `Truecolor in
  (* A kitty id needs at least 256 colors to be written. *)
  let form =
    match (graphics, cell_size, color_depth) with
    | _, None, _ | `Kitty, _, (`Ansi16 | `Ansi8 | `Mono) -> `Blocks
    | g, Some _, _ -> g
  in
  if
    graphics <> p.graphics || form <> p.form || cell_size <> p.cell
    || direct <> p.direct
  then begin
    own r.diff;
    let p = r.diff.pictures in
    p.graphics <- graphics;
    p.form <- form;
    p.cell <- cell_size;
    p.direct <- direct;
    (* The pictures shown are written again in the new form. *)
    if Hashtbl.length p.entries > 0 then (
      forget_images r;
      Hashtbl.reset p.entries;
      r.current_valid <- false)
  end

let cell_size t = t.diff.pictures.cell
let set_explicit_width t flag = t.prefer_explicit_width <- flag

let set_width_method (t : t) (method_ : Matrix_text.width_method) =
  Grid.set_width_method t.current method_;
  Grid.set_width_method t.scroll_baseline method_;
  Grid.set_width_method t.next method_

let width_method t = Grid.width_method t.next

let add_hit_region frame ~x ~y ~width ~height ~id =
  ensure_next_fresh frame;
  frame.next_state <- Touched;
  Hit_grid.add frame.hit_next ~x ~y ~width ~height ~id

let delete_transcript t w =
  let p = t.diff.pictures in
  if p.transcript <> [] then (
    List.iter ~f:(delete w) p.transcript;
    p.transcript <- [])

let send_pictures t w =
  ensure_next_fresh t;
  send_into t w

(* Static rows are written as [Grid.to_ansi ~complete_rows:true] writes
   them, with a tracker of their own, and their picture cells in the form.
   Their kitty images are the transcript's, which the screen deletes only
   when the transcript is erased ([delete_transcript]). *)
let static_rows t g ~rows ~top =
  if rows < 0 || rows > Grid.height g then
    invalid_arg "Matrix_screen.static_rows: rows must be within grid height";
  if not (Grid.Unsafe.has_pictures g) then
    Grid.to_ansi ~complete_rows:true ~rows g
  else begin
    own t.diff;
    let p = t.diff.pictures in
    let w = Ansi.Writer.create 1024 in
    let c =
      {
        t.diff with
        curr = g;
        out = w;
        sgr = Ansi.Sgr_state.create ();
        width = Grid.width g;
        (* Every row may take a sixel: the rows end above the cursor. *)
        height = rows + 1;
        row_offset = top;
      }
    in
    let width = Grid.width g in
    let owned = Hashtbl.create 4 in
    let tile_entry (tile : Grid.Unsafe.tile) =
      if p.form <> `Kitty then entry p tile
      else
        let key = (Grid.Unsafe.picture_id tile.picture, tile.cols, tile.rows) in
        match Hashtbl.find_opt owned key with
        | Some e -> e
        | None ->
            let e = { (entry p tile) with image = image_id p p.count } in
            p.count <- p.count + 1;
            Hashtbl.add owned key e;
            p.transcript <- e.image :: p.transcript;
            transmit w e ~id:e.image ~cols:tile.cols ~rows:tile.rows;
            e
    in
    for idx = 0 to (rows * width) - 1 do
      if Grid.Unsafe.is_picture g idx then
        ignore (tile_entry (Grid.Unsafe.tile g idx) : entry)
    done;
    for y = 0 to rows - 1 do
      Ansi.emit (Ansi.erase_line ~mode:`All) w;
      let x = ref 0 in
      while !x < width do
        let idx = (y * width) + !x in
        if Grid.Unsafe.is_picture g idx then begin
          let tile = Grid.Unsafe.tile g idx in
          write_tile c ~y ~x:!x idx tile (tile_entry tile);
          incr x
        end
        else begin
          let cw = Grid.Unsafe.cell_width g idx in
          Ansi.Sgr_state.update c.sgr w ~fg:(Grid.Unsafe.get_fg g idx)
            ~bg:(Grid.Unsafe.get_bg g idx)
            ~attrs:(Grid.Unsafe.get_attrs g idx)
            ~link:(Grid.Unsafe.get_link g idx);
          let len = Grid.Unsafe.cell_text_length g idx in
          if cw <= 0 || len <= 0 then Ansi.Writer.write_char w ' '
          else (
            c.scratch <- room c.scratch len;
            let n = Grid.Unsafe.blit_cell_text g idx c.scratch ~pos:0 in
            Ansi.Writer.write_subbytes w c.scratch 0 n);
          x := !x + max 1 cw
        end
      done;
      Ansi.Sgr_state.close_link c.sgr w;
      Ansi.emit Ansi.reset w;
      Ansi.Sgr_state.reset c.sgr;
      Ansi.Writer.write_string w "\r\n"
    done;
    (* The sixels of the rows still on the screen, then the cursor back where
       the rows left it. *)
    if Dynarray.length p.sixels > 0 then begin
      let shown =
        Dynarray.filter (fun idx -> top + (idx / width) >= 0) p.sixels
      in
      Dynarray.clear p.sixels;
      Dynarray.append p.sixels shown;
      write_sixels c;
      Ansi.cursor_position ~row:(top + rows + 1) ~col:1 w
    end;
    Ansi.Writer.contents w
  end
