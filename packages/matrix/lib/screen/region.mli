(** Regions, rows that a program redraws at the bottom of a terminal's
    scrollback while its other output goes above them.

    A command-line program that shows its progress shares the terminal with the
    shell and with other programs. What they all print stays in the scrollback,
    and the program redraws a few rows below it until it is done. These rows are
    its {e region}. The program does not own the terminal. It cannot tell where
    the cursor is on the screen, another process may write between two of its
    writes, and many terminals rewrap their lines when their width changes. So
    the region holds no mode between two writes, and it is drawn relative to a
    row of its own, its {e origin}.

    Between two writes the cursor rests at the start of the origin, a blank row
    above the region's rows, unless the program places it elsewhere (below). A
    {e write} returns to the start of the origin and erases from there to the
    end of the screen. It then prints the lines that go {e above} the region,
    which the terminal wraps and keeps as it keeps any output, and the row after
    them is the new origin. It draws the region's rows below the origin with the
    terminal's autowrap off, so a row too wide for the terminal is cut at its
    edge. It goes down by line feeds, which scroll the screen on its last row,
    where a cursor motion would stop, so the cursor goes back up exactly as many
    rows as it went down. A write never moves the cursor above the origin.
    Whatever moves the cursor behind the region's back, such as a line another
    process writes or a terminal that rewraps its rows, moves it down, and the
    next write then erases less. At worst a stale copy of the region's rows
    stays in the scrollback, and no line of anyone else's is erased. A terminal
    that moves a cursor resting in the first column up a row when its width
    changes, as WezTerm does, would have the next write erase the line above. On
    such a terminal the cursor rests in the second column of the origin instead,
    and what another writer prints there starts one column in.

    A line that another process leaves unended on the origin is erased by the
    next write drawn afresh, as each {!draw} is, since the region cannot see it.
    When the program knows that someone else may have written, as after the
    process was stopped and continued, after a prompt, or after bytes it wrote
    itself, it says so with {!moved}. The next write then first moves to the
    start of a row of its own by writing as many spaces as a row takes past the
    column where the region left the cursor. From that column they reach the end
    of the row, and from any column further right they wrap below the unended
    line, which stays.

    On a terminal [rows] high, at most [rows - 1] rows show under the origin,
    the last rows of a taller frame, so the way back to the origin never meets
    the top of the screen. A frame drawn for a height that the terminal has
    already left behind can still push region rows into the scrollback, where no
    write reaches them. The last write goes back up to no row, so it draws every
    row of its frame, and the rows the screen cannot hold go to the scrollback.

    The cursor shows where it rests, unless the program hides it, and the
    program may have it rest on one of the region's cells instead, as a program
    that edits text there places it ({!type-cursor}). A write then starts by
    going back up from that cell to the origin. A line another process writes
    while the cursor rests in the region lands on the region's rows, which the
    next write erases.

    The life cycle of a region:
    + {!create} makes one for the terminal and the styling of its output.
    + {!draw} gives the bytes of a write whenever the frame, the lines above it
      or the terminal's size change. A program that writes the rows itself, such
      as one that writes only the cells that changed, brackets them with
      {!start} and {!finish} instead.
    + {!suspend} erases the region while something else writes, such as a child
      process, and {!moved} says that something else may have written.
    + {!close} leaves the last frame in the scrollback, or erases it, with the
      cursor below. {!keep} leaves the rows the last write drew where they are.

    A region does no I/O. Each function returns the bytes of one write, which
    the caller must write whole and in order, or appends them to a writer, and
    the size it takes is the terminal's size read just before. A write hides the
    cursor while it lasts and is one synchronized update (mode 2026), which a
    terminal without the mode ignores. A region is mutable and must not be used
    by two threads in parallel.

    This shows a counter, prints a line above it as the count changes, and
    leaves the last count in the scrollback, on a terminal of 80 by 24 cells,
    where [write] writes to the terminal:
    {[
    let open Matrix_screen in
    let counter n =
      let g = Matrix_grid.create ~width:20 ~height:1 () in
      Matrix_grid.draw_text g ~x:0 ~y:0 ~text:(Printf.sprintf "%d done" n);
      Some g
    in
    let r = Region.create () in
    write (Region.draw r ~cols:80 ~rows:24 (counter 0));
    write (Region.draw r ~cols:80 ~rows:24 ~above:"a.ml\n" (counter 1));
    write (Region.close r ~cols:80 ~rows:24 (counter 1))
    ]} *)

type t
(** The type for regions. *)

(** The type for where the cursor rests between writes. *)
type cursor =
  | Rest  (** Shown at the start of the origin, where it rests. *)
  | Hidden  (** Hidden, resting at the start of the origin. *)
  | At of { x : int; y : int }
      (** Shown in column [x] of the region's row [y], both counted from 0 and
          clamped to the rows the write shows. With no row shown, it rests. *)

val create :
  ?eager_wrap:bool ->
  ?lifts_resting_cursor:bool ->
  ?styling:Ansi.Styling.t ->
  unit ->
  t
(** [create ~eager_wrap ~lifts_resting_cursor ~styling ()] is a region not yet
    drawn, whose rows are written at [styling] ({!Matrix_grid.to_ansi}),
    [`Truecolor] by default. [eager_wrap] says that the terminal moves to the
    next line as soon as it prints in the last column
    ([Matrix_terminal.Caps.t]'s [eager_wrap]), [false] by default. The region
    then writes nothing in that column. [lifts_resting_cursor] says that the
    terminal moves a cursor resting in the first column up a row when its width
    changes ([Matrix_terminal.Caps.t]'s [lifts_resting_cursor]), [false] by
    default. The cursor then rests in the second column. Its first write moves
    to a row of its own, as after {!moved}. *)

val width : t -> cols:int -> int
(** [width r ~cols] is the number of columns a row of [r] takes on a terminal
    [cols] wide, [cols], or [cols - 1] when the terminal wraps eagerly. A frame
    that wide shows whole. *)

val draw :
  t ->
  cols:int ->
  rows:int ->
  ?above:string ->
  ?cursor:cursor ->
  Matrix_grid.t option ->
  string
(** [draw r ~cols ~rows ~above ~cursor frame] is the bytes of a write on a
    terminal [cols] wide and [rows] high, which prints [above] above the region,
    shows [frame]'s rows in it, none for [None], and leaves the cursor as
    [cursor] says, [Rest] by default. [above] is lines, each ended by LF, and a
    last line without one is ended all the same. It must hold no control
    function but SGR, OSC 8 and HT, since another would move the cursor the
    region counts from. Each row of [frame] is cut to {!width}[ r ~cols] cells,
    and a frame taller than [rows - 1] shows its last rows. It is [""] when the
    write would change nothing, with no line above, the rows and the cursor of
    the last write at the same width, and no {!moved} since.

    Raises [Invalid_argument] if [cols] or [rows] is not positive. *)

val start :
  t ->
  cols:int ->
  rows:int ->
  ?above:string ->
  ?whole:bool ->
  height:int ->
  Ansi.Writer.t ->
  bool
(** [start r ~cols ~rows ~above ~whole ~height w] appends to [w] the start of a
    write whose rows the caller writes, [height] of them at most [rows - 1], on
    a terminal [cols] wide and [rows] high. It prints [above] as {!draw} does,
    and leaves the cursor on the origin's row, with autowrap off and the
    region's rows below it on the screen. The caller then writes the rows by
    relative motions within them ({!Matrix_screen.render}'s [relative]). It must
    go down by line feeds, since a cursor motion stops on the screen's last row
    and the way back up would then end above the origin, and must leave the
    cursor on the origin's row for {!finish}.

    It is [true] when the write draws the region afresh, with the rows below the
    origin erased, so the caller writes every cell. That is with [whole], with
    lines above, after {!moved}, at the first write and the first after
    {!suspend}, {!close}, {!keep} or {!clear}, and when the width or the number
    of rows differs from the last write's. It is [false] when the rows show what
    the last write left, so the caller writes only what changed. A write that is
    not afresh, whose rows the caller left as they were, with the cursor as the
    last write left it, changes nothing on the screen, and the caller may leave
    its bytes unwritten.

    Raises [Invalid_argument] if [cols] or [rows] is not positive. *)

val finish : t -> ?cursor:cursor -> Ansi.Writer.t -> unit
(** [finish r ~cursor w] appends to [w] the end of the write {!start} began,
    which leaves the cursor as [cursor] says, [Rest] by default. *)

val moved : t -> unit
(** [moved r] says that the cursor may have moved since [r]'s last write, so the
    next write first moves to the start of a row of its own. *)

val suspend : t -> cols:int -> string
(** [suspend r ~cols] is the bytes that erase [r]'s rows and leave the cursor at
    the start of the origin, for something else to write there. The next {!draw}
    draws [r] afresh from a row of its own.

    Raises [Invalid_argument] if [cols] is not positive. *)

val close :
  t -> cols:int -> rows:int -> ?above:string -> Matrix_grid.t option -> string
(** [close r ~cols ~rows ~above frame] is the bytes of [r]'s last write. It is
    {!draw}'s write with all of [frame]'s rows drawn from the origin itself and
    the cursor left at the start of the row below them, so the frame stays whole
    as the program's last output. With [None] the region is erased and the
    cursor left at the start of the origin. [r] is then as {!create} made it.

    Raises [Invalid_argument] if [cols] or [rows] is not positive. *)

val keep : t -> int -> string
(** [keep r n] is the bytes of [r]'s last write that leaves the first [n] rows
    the write before drew where they are, as the program's last output, for a
    program whose rows a redraw would not give back as they are, such as rows
    with pictures. The origin's row is deleted (DL), so the rows below move up
    onto it, the rows after the first [n] are erased, and the cursor is left at
    the start of the row below them, shown. With [n] at [0] the region is erased
    and the cursor left at the start of the origin, as {!close} does with
    [None]. [r] is then as {!create} made it. *)

val clear : t -> string
(** [clear r] is the bytes that erase the screen (ED 2) and leave the cursor at
    the start of its first row, where [r] then draws afresh. The scrollback
    stays. *)

val teardown : t -> string
(** [teardown r] is the bytes that end [r]'s last write wherever a signal cut it
    and leave the cursor at the start of the row below [r]'s rows, for a process
    that ends without another write. They are CAN, which ends a control function
    the cut left open, the end of the synchronized update, autowrap on, SGR 0,
    the cursor shown, then the motion. They describe [r] once its last write has
    been written whole, so a caller that keeps them for a signal handler must
    take them only after that write has returned. *)
