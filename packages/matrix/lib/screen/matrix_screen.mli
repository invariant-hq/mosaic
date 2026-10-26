(** Screens, which turn each frame a program draws into the bytes that bring a
    terminal from the previous frame to it.

    A {e frame} is what a program shows at one moment, drawn as the cells of a
    {!Matrix_grid.t} together with a {!Hit_grid.t} that says which element lies
    under each cell. A screen keeps two such pairs. The {e next buffer} is the
    frame being drawn. The {e current buffer} is the frame last rendered, which
    the screen takes as what the terminal shows, and so it is the
    {e diff baseline}. To {e render} is to compare the next buffer with the
    baseline cell by cell, write the bytes that change the cells that differ,
    and then {e swap} the buffers. The frame just drawn becomes the current
    buffer, and the next buffer is blank again, in the sense that whatever first
    reads or writes it after a render finds it cleared. A frame is therefore
    drawn whole, and each frame redraws everything it shows.

    A screen does no I/O and reads no clock. A render returns its bytes, and the
    caller writes them to the terminal and measures time. The screen keeps the
    {e cursor} the caller asks for ({!set_cursor}) for the code that writes the
    terminal, and never writes it itself. Hit testing ({!query_hit}) reads the
    current hit grid, so the regions of a frame answer mouse reports once that
    frame has been rendered.

    The life cycle of a screen usually goes like this:
    + {!create} a screen, and {!apply_capabilities} once the terminal's
      capabilities are known.
    + For each frame, {!build} it at the terminal's size. The function given to
      {!build} draws the cells into the grid and the elements' regions into the
      hit grid.
    + {!render} it, or {!render_into} a writer, and write the bytes to the
      terminal.
    + Answer mouse reports with {!query_hit} until the next frame.
    + After something else has changed the terminal, such as an erase,
      {!invalidate_presented} so that the baseline matches it again.

    Unless a value's doc says otherwise, coordinates are cells counted from 0 at
    the top-left of the frame, a region outside the grid is clipped to it, and
    no value raises. A screen is mutable and must not be used by two domains
    concurrently.

    This program draws a one-word button, renders the frame, and finds the
    button under column 2 of row 0:
    {[
    let screen = Matrix_screen.create () in
    Matrix_screen.build screen ~width:80 ~height:24 (fun grid hits ->
        Matrix_grid.draw_text grid ~x:0 ~y:0 ~text:"Quit";
        Matrix_screen.Hit_grid.add hits ~x:0 ~y:0 ~width:4 ~height:1 ~id:1);
    print_string (Matrix_screen.render screen);
    assert (Matrix_screen.query_hit screen ~x:2 ~y:0 = 1)
    ]} *)

module Hit_grid = Hit_grid
(** Hit grids. *)

module Region = Region
(** Regions, rows redrawn at the bottom of a terminal's scrollback. *)

(** {1:types Types} *)

type t
(** The type for screens. *)

type stats = {
  frame_count : int;  (** Frames rendered since creation or {!reset}. *)
  total_cells : int;
      (** Columns written by those frames, as {!frame_metrics} counts them. *)
  total_bytes : int;  (** Bytes those frames produced. *)
}
(** The type for the totals of the frames rendered since creation or the last
    {!reset}. *)

type frame_metrics = {
  frame_count : int;  (** Frames rendered since creation or {!reset}. *)
  cells : int;
      (** Columns the frame wrote, a wide character counting each column it
          covers. *)
  bytes : int;  (** Bytes the frame produced. *)
  cursor_visible : bool;
      (** The cursor's [visible] ({!type-cursor}) when the frame was rendered.
      *)
}
(** The type for the metrics of the last frame rendered. *)

(** {1:constructors Screens} *)

val create :
  ?width_method:Matrix_text.width_method ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  unit ->
  t
(** [create ~width_method ~cursor_visible ~explicit_width ()] is a screen whose
    buffers are one cell wide and high and blank. Its cursor has position
    [None], style [`Block], [blinking] [false] and color [None].
    - [width_method] is how the grids measure graphemes ({!set_width_method}).
      Defaults to [`Unicode].
    - [cursor_visible] is the cursor's [visible]. Defaults to [true].
    - [explicit_width] is the preference {!set_explicit_width} sets. Defaults to
      [false]. *)

(** {1:building Building frames}

    A build draws the whole frame. When the next buffer holds cells or regions
    that an earlier {!build} or {!add_hit_region} drew since the last render, a
    build first clears them, so a second build before a render replaces the
    first. Cells and regions drawn through {!next_grid} and {!next_hit_grid}
    stay, and a build draws over them, unless the build changes the screen's
    size ({!resize}), which clears every buffer. *)

val build :
  t -> width:int -> height:int -> (Matrix_grid.t -> Hit_grid.t -> unit) -> unit
(** [build t ~width ~height f] resizes [t] to [width] by [height] as {!resize}
    does when its size differs, clears the next buffer as the section says, and
    calls [f] with the next grid and hit grid to draw the frame into. When
    [width] or [height] is [0] or less, it clears and does not call [f].

    [f] must not keep the grids it receives. After the next {!render} they are
    the diff baseline, and a change to them makes later frames show wrong cells.
*)

val add_hit_region :
  t -> x:int -> y:int -> width:int -> height:int -> id:int -> unit
(** [add_hit_region t ~x ~y ~width ~height ~id] is {!Hit_grid.add} on the next
    hit grid, for a region drawn outside a {!build}'s [f]. A {!build} that
    follows before the next render clears it. *)

(** {1:rendering Rendering}

    A render compares the next buffer with the diff baseline and writes each run
    of cells that differ after moving the cursor to its start. It writes an
    empty cell ({!Matrix_grid.is_empty}) as a space in the cell's colors. A
    {e full} render writes every cell. The render after a {!resize} is full,
    since the baseline then no longer says what the terminal shows. A render
    ends with no hyperlink open, and with the rendition reset (SGR 0) when it
    wrote a run of cells. It leaves the cursor wherever its last write put it.
    It then swaps the buffers and counts the frame in the {!section-statistics}.

    A render writes the rows of a {e viewport}, a band of the terminal's rows
    given by its first row and its height. Without one, the viewport starts at
    {!row_offset} and is as high as the grid. Rows of the grid below the
    viewport are not written, and after the render they are blank in the current
    grid and empty in the current hit grid.

    A render reaches a run by the run's terminal row and column (CUP). A
    {e relative} render reaches it from the row the cursor is on when its bytes
    are written, by rows down (line feeds) or up (CUU) and then the column
    (CHA), so that it draws right whatever row of the screen that is. Its
    viewport's first row is then counted from the cursor's row, and it ends with
    the cursor back on that row. A line feed on the screen's last row scrolls
    the screen, where a cursor motion would stop, so the way back up is as many
    rows as the render went down even on a screen shorter than the frame. A
    program that draws below a line of its own, such as the rows of a {!Region},
    renders relatively. *)

type scroll_hint = {
  top : int;  (** The region's first row of the frame, from 0. *)
  bottom : int;  (** The region's last row of the frame, from 0. *)
  delta : int;
      (** Rows the content moved up, or down when negative, as when the user
          scrolls down by [delta]. *)
}
(** The type for scroll hints. A hint says that the content of rows [top] to
    [bottom] moved by [delta] rows since the last frame, as in a scrolled pane.
    The render then scrolls that region on the terminal (DECSTBM, then SU or SD)
    and in its baseline alike, resets the scrolling region to the whole screen,
    homes the cursor to the viewport's first row, and diffs as usual, so only
    the rows the scroll left out of place are written. A hint is ignored in a
    full render, when [delta] is [0], when the region, clipped to the viewport,
    has fewer than two rows, and when [delta] covers the whole region. A caller
    must not pass a hint while it keeps a scrolling region of its own, which the
    reset removes. *)

type viewport = {
  y : int;  (** The terminal row of the frame's first row, from 0. *)
  height : int;  (** The rows of the frame written. *)
}
(** The type for viewports. A viewport writes the first [height] rows of the
    frame at terminal row [y]. A negative [y] is taken as [0], and [height] is
    clipped to the grid. *)

val render :
  ?full:bool ->
  ?scroll_hint:scroll_hint ->
  ?viewport:viewport ->
  ?relative:bool ->
  t ->
  string
(** [render ~full ~scroll_hint ~viewport ~relative t] renders the next buffer
    and is the bytes it produces. It is {!render_into} on a writer [t] keeps,
    copied to a new string on each call.
    - [full] makes the render full. Defaults to [false].
    - [scroll_hint] is the content's scroll since the last frame. Defaults to
      none. A relative render ignores it, since a scroll hint sets a scrolling
      region by terminal rows.
    - [viewport] is the viewport. Defaults to the one at {!row_offset}.
    - [relative] makes the render relative. Defaults to [false]. *)

val render_into :
  ?full:bool ->
  ?scroll_hint:scroll_hint ->
  ?viewport:viewport ->
  ?relative:bool ->
  t ->
  Ansi.Writer.t ->
  unit
(** [render_into ~full ~scroll_hint ~viewport ~relative t w] is {!render} with
    the bytes appended to [w], after what [w] already holds. It never resets
    [w]. The frame's [bytes] ({!frame_metrics}) count only those appended. A
    writer kept across frames keeps its capacity.

    When a write to [w] raises, such as {!Ansi.Writer.Buffer_full} from a writer
    of {!Ansi.Writer.of_bytes}, [render_into] raises it and changes nothing in
    [t], so the next render produces the same frame. The bytes appended before
    the exception are an unfinished frame and must not be sent. *)

val set_row_offset : t -> int -> unit
(** [set_row_offset t n] makes the terminal row [n] the first row of the
    viewport of each later render that is given none. A negative [n] is taken as
    [0]. *)

val row_offset : t -> int
(** [row_offset t] is the row {!set_row_offset} set, initially [0]. *)

val query_hit : t -> x:int -> y:int -> int
(** [query_hit t ~x ~y] is the element the current hit grid holds at [(x, y)],
    the one of the frame last rendered. It is {!Hit_grid.empty_id} when that
    cell is empty or outside the grid. *)

(** {1:cursor The cursor}

    A frame's cursor is the place, shape and color it asks the terminal's cursor
    to take. *)

type cursor = {
  position : (int * int) option;
      (** The cell [(x, y)] of the frame the cursor should be at, or [None] when
          the frame asks for no position. *)
  style : Ansi.cursor_style;  (** The cursor's shape. *)
  blinking : bool;  (** [true] iff the cursor blinks. *)
  color : (int * int * int) option;
      (** The cursor's color as [(r, g, b)], or [None] for the terminal's. *)
  visible : bool;  (** [true] iff the cursor is shown. *)
}
(** The type for cursors. *)

val set_cursor : t -> cursor -> unit
(** [set_cursor t c] makes [c] the cursor of [t], with each color channel
    clamped to \[[0];[255]\] and each negative coordinate taken as [0]. A later
    {!resize} moves a position outside the new size to its last row or column.
*)

val cursor : t -> cursor
(** [cursor t] is the cursor of [t]. *)

(** {1:capabilities Capabilities and widths}

    A screen writes for the terminal {!apply_capabilities} describes. Until it
    is called, the terminal takes explicit widths and hyperlinks, needs no
    repositioning after wide graphemes, and shows 24-bit color. A capability
    change applies from the next render. *)

val apply_capabilities :
  t ->
  explicit_width:bool ->
  explicit_cursor_positioning:bool ->
  hyperlinks:bool ->
  color_depth:Ansi.Sgr_state.color_depth ->
  graphics:[ `Kitty | `Sixel | `Blocks ] ->
  cell_size:(int * int) option ->
  unit
(** [apply_capabilities t ~explicit_width ~explicit_cursor_positioning
     ~hyperlinks ~color_depth ~graphics ~cell_size] makes [t] write for a
    terminal with these capabilities.
    - [explicit_width] says the terminal takes OSC 66, which states the width of
      a grapheme. A grapheme two or more columns wide is then written in OSC 66
      when {!set_explicit_width} also asks for it.
    - [explicit_cursor_positioning] says to move the cursor to the next cell
      after each grapheme two or more columns wide when OSC 66 is not in use,
      for a terminal that may measure the grapheme differently.
    - [hyperlinks] says the terminal takes OSC 8. Without it, cells are written
      without their hyperlinks.
    - [color_depth] is the color depth colors are reduced to.
    - [graphics] is the form of picture cells ({!section-pictures}).
    - [cell_size] is the size of the terminal's cells in pixels,
      [(width, height)], or [None] when it is not known.

    A change of [graphics] or [cell_size] while the frame last rendered shows
    pictures makes the next render full. *)

(** {1:pictures Pictures}

    A picture cell ({!Matrix_grid.section-pictures}) is written in one of three
    forms, the [graphics] of {!apply_capabilities}. A screen without a cell
    size, or with the [`Kitty] form and fewer than 256 colors, writes [`Blocks].
    The picture is fit in its box at the terminal's cell size, 10 by 20 pixels
    when it is not known.
    - [`Blocks] writes U+2580 with the tile's upper half as the foreground and
      its lower half as the background, each composited over the cell's
      background. It writes U+2584 when only the lower half shows, and a space
      when neither does.
    - [`Kitty] writes the cell as a kitty Unicode placeholder, U+10EEEE with the
      marks of its row and column, the image's id in the foreground, at 24 bits
      in direct color and otherwise as 8 bits and a third mark. A box wider or
      taller than the 297 marks is written as [`Blocks] beyond them. A render
      first sends the terminal the pixels of each box it shows that the terminal
      does not hold, an image with a virtual placement, and deletes the images
      of boxes the frame before it stopped showing.
    - [`Sixel] writes the cell as [`Blocks] does, then, after the frame's cells,
      draws sixel images of the box's pixels over the picture cells it wrote,
      composited over their backgrounds, in rectangles. It never draws on the
      frame's last row, which is the terminal's last row, where a sixel would
      scroll the screen. A terminal that loses an image shows the half blocks
      under it.

    A frame whose cells equal the baseline writes no byte in every form. *)

val cell_size : t -> (int * int) option
(** [cell_size t] is the [cell_size] {!apply_capabilities} last gave [t], [None]
    before. *)

val set_explicit_width : t -> bool -> unit
(** [set_explicit_width t b] sets whether [t] writes wide graphemes in OSC 66,
    which it does when [b] is [true] and the terminal takes OSC 66
    ({!apply_capabilities}). *)

val set_width_method : t -> Matrix_text.width_method -> unit
(** [set_width_method t m] makes [m] the method every buffer of [t] measures
    graphemes with. Setting it on {!next_grid} alone would leave the other
    buffer with the old method after the next swap. *)

val width_method : t -> Matrix_text.width_method
(** [width_method t] is the method [t]'s buffers measure graphemes with. It
    leaves the next buffer as it is, where reading it from {!next_grid} clears a
    buffer a render left. *)

(** {1:layout Size and baseline} *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] makes every buffer of [t] [width] by [height] and
    blank, and makes the next render full. When [t] already has that size it
    does nothing, and the baseline stays. {!build} resizes as it needs.

    Raises [Invalid_argument] if [width] or [height] is [0] or less. *)

val reset : t -> unit
(** [reset t] clears the next buffer and the current hit grid and sets the
    totals of {!val-stats} to [0]. The current grid stays as the baseline, so
    the next render erases the cells the last frame showed. The cursor, the
    capabilities and the row offset stay. *)

val invalidate_presented : t -> unit
(** [invalidate_presented t] makes the baseline blank, so that the next render
    writes every cell of its frame that is not blank, and takes the terminal to
    hold none of the images [t] sent, which the next render deletes and sends
    again as it needs them. A caller that has erased the terminal's rows must
    call it, or the next render leaves out the cells it takes as already shown.
    It writes no bytes. *)

(** {1:statistics Statistics} *)

val stats : t -> stats
(** [stats t] is the totals of the frames [t] rendered since creation or the
    last {!reset}. *)

val last_metrics : t -> frame_metrics
(** [last_metrics t] is the metrics of the last frame [t] rendered. Before the
    first render its counts are [0]. *)

(** {1:direct Direct access}

    These values hand out the buffers themselves, for code that draws or reads a
    frame outside {!build}. *)

val next_grid : t -> Matrix_grid.t
(** [next_grid t] is the grid of [t]'s next buffer, cleared if a render left it.
    A {!build} that follows draws over what is drawn through it.

    The grid must not be changed after the next {!render}, when it becomes the
    diff baseline. *)

val current_grid : t -> Matrix_grid.t
(** [current_grid t] is the grid of the frame last rendered, the diff baseline,
    which is blank before the first render.

    The grid must not be changed, or the next render leaves out the cells it
    takes as already shown. *)

val next_hit_grid : t -> Hit_grid.t
(** [next_hit_grid t] is the hit grid of [t]'s next buffer, cleared as
    {!next_grid}'s is.

    The hit grid must not be changed after the next {!render}, when it becomes
    the one {!query_hit} reads. *)

val active_height : t -> int
(** [active_height t] is {!Matrix_grid.active_height} of the next grid, the rows
    from the top down to the last one that shows anything, such as the height an
    inline frame needs. *)

(**/**)

(* For matrix's runtime, which writes the frame inside a synchronized update
   and writes static rows into the transcript. *)

val send_pictures : t -> Ansi.Writer.t -> unit
(* [send_pictures t w] appends to [w] what the next render would write
   first under [`Kitty], the deletion of the images [t] no longer needs and
   the pixels of the boxes the next buffer shows that the terminal does not
   hold. The render then writes none of it. These bytes show nothing, so a
   caller that writes a frame inside a synchronized update writes them
   before it. *)

val delete_transcript : t -> Ansi.Writer.t -> unit
(* [delete_transcript t w] appends to [w] the deletion of the images of the
   static rows [t] wrote, for a terminal whose scrollback was erased. *)

val static_rows : t -> Matrix_grid.t -> rows:int -> top:int -> string
(* [static_rows t g ~rows ~top] is the first [rows] rows of [g] as
   [Matrix_grid.to_ansi ~complete_rows:true] writes them, for rows that
   scroll into the transcript and end on the terminal's rows from [top],
   from [0], which is negative when some scroll off. A grid without picture
   cells gives exactly those bytes. Picture cells are written in the
   screen's form. Under [`Kitty] the rows are preceded by the pixels of
   their boxes, as images [t] deletes only by [delete_transcript]. Under
   [`Sixel] they are followed by sixels over the picture cells of the rows
   from [top] that are on the screen, and by a cursor motion to the start of
   the row after them. Raises [Invalid_argument] if [rows] is negative or
   greater than the height of [g]. *)

(**/**)
