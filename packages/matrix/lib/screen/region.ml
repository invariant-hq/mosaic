(* The region's state is what the terminal shows of it since its last
   write: the rows drawn under the origin, the width they were cut to, the
   bytes of the rows a [draw] wrote, so that a write that would change
   nothing writes nothing, and where the cursor rests. *)

type cursor = Rest | Hidden | At of { x : int; y : int }

type t = {
  eager_wrap : bool;
  rest_column : int;  (** Where a write leaves a resting cursor: 0, or 1. *)
  styling : Ansi.Styling.t;
  mutable height : int;  (** Rows drawn under the origin. *)
  mutable width : int;  (** The width of the last write, [-1] before one. *)
  mutable rows : string list option;
      (** The rows of the last [draw], while they show. *)
  mutable cursor : cursor;  (** The cursor the last write left. *)
  mutable moved : bool;  (** The next write moves to a row of its own. *)
  mutable rest : int;  (** The column where the last write left the cursor. *)
  mutable rest_row : int;
      (** The rows from the origin down to the cursor's, [0] when it rests on
          the origin. *)
}

let create ?(eager_wrap = false) ?(lifts_resting_cursor = false)
    ?(styling = `Truecolor) () =
  {
    eager_wrap;
    rest_column = (if lifts_resting_cursor then 1 else 0);
    styling;
    height = 0;
    width = -1;
    rows = None;
    cursor = Rest;
    moved = true;
    rest = 0;
    rest_row = 0;
  }

let width r ~cols = max 0 (if r.eager_wrap then cols - 1 else cols)
let moved r = r.moved <- true

let check ~cols ~rows =
  if cols < 1 || rows < 1 then
    invalid_arg "Matrix_screen.Region: the size must be positive"

(* The rows of [frame] the region shows: its last [height] rows, each cut to
   [width] cells and written at the styling. A grid cell holds no control,
   so a row's bytes hold none but SGR and OSC 8. *)
let rows_of r ~width ~height = function
  | None -> []
  | Some _ when width = 0 || height = 0 -> []
  | Some g ->
      let h = Matrix_grid.height g in
      let shown = min h height in
      let text =
        if Matrix_grid.width g <= width && shown = h then
          Matrix_grid.to_ansi ~styling:r.styling ~trim:true g
        else
          let w = min width (Matrix_grid.width g) in
          let dst = Matrix_grid.create_like g ~width:w ~height:shown in
          Matrix_grid.blit_region ~src:g ~dst ~src_x:0 ~src_y:(h - shown)
            ~width:w ~height:shown ~dst_x:0 ~dst_y:0;
          let text = Matrix_grid.to_ansi ~styling:r.styling ~trim:true dst in
          (* The copy is dropped here: give back its graphemes now rather
             than when the GC finalises it. *)
          Matrix_grid.clear dst;
          text
      in
      String.split_on_char '\n' text

let str = Ansi.Writer.write_string
let bsu = "\027[?2026h"
let esu = "\027[?2026l"
let sgr0 = "\027[0m"

(* Back from where the cursor rests to the origin's row. *)
let to_origin r w = Ansi.cursor_up ~n:r.rest_row w

(* To a row of its own: as many spaces as a row takes past the column where
   the last write left the cursor, then CR. From that column they reach the
   end of the row, the last one leaving its wrap pending, which the CR
   cancels; from any column further right they wrap below the line someone
   left there, which stays. Counted from the first column while the cursor
   rests in the second, they would leave a blank row whenever nobody
   wrote. On a terminal that wraps eagerly a row takes one column less, so
   the spaces stop short of the last column. *)
let to_own_row r w ~cols =
  if r.moved then
    for _ = 1 to width r ~cols - r.rest do
      Ansi.Writer.write_char w ' '
    done;
  Ansi.Writer.write_char w '\r'

(* Erases from the start of the origin to the end of the screen. ED from
   the first column of the screen's first row clears the whole screen, which
   tmux keeps as history, so the first column is erased by EL and the rest
   from the second column. *)
let erase w = str w "\027[2K\027[C\027[J\r"

(* The lines above, each ended by CR LF, with autowrap on: the terminal
   wraps them as any output, and the row after them is the origin. *)
let add_above w above =
  if above <> "" then (
    let lines = String.split_on_char '\n' above in
    let lines =
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
    in
    List.iter
      (fun line ->
        str w line;
        str w "\r\n")
      lines;
    str w sgr0)

(* The start of every write: the cursor hidden while it lasts, back on the
   origin's row. *)
let opening r w =
  str w bsu;
  str w "\027[?25l";
  to_origin r w

(* The start of a write that draws the region afresh: from a row of its
   own, everything below erased, the lines above written. It resets the
   rendition first, since a background another process left set would
   paint the erase; a write that is not afresh erases nothing, and its
   caller sets the rendition of what it writes. *)
let afresh r w ~cols ~above =
  str w bsu;
  str w sgr0;
  str w "\027[?25l";
  to_origin r w;
  to_own_row r w ~cols;
  erase w;
  add_above w above

(* The cursor [cursor] asks for, on a region of [height] rows: one placed
   below them, or on a region of none, is placed on its last row, or rests. *)
let placed cursor ~height =
  match cursor with
  | At { x; y } when height > 0 ->
      At { x = max 0 x; y = max 0 (min y (height - 1)) }
  | At _ -> Rest
  | (Rest | Hidden) as c -> c

(* The end of every write, from the start of the origin: the cursor where
   it rests between writes, shown unless it is hidden. On a terminal that
   moves a cursor resting in the first column up a row when its width
   changes (WezTerm, issue 7994), a resting cursor is left in the second
   column, or the next write would erase from a line above. *)
let closing r w cursor =
  (match cursor with
  | At { x; y } ->
      (* Down by line feeds, which scroll where a motion would stop, so the
         next write goes back up as many rows as this one went down. *)
      for _ = 0 to y do
        Ansi.Writer.write_char w '\n'
      done;
      Ansi.cursor_horizontal_absolute (x + 1) w;
      r.rest <- x;
      r.rest_row <- y + 1
  | Rest | Hidden ->
      if r.rest_column = 1 then str w "\027[C";
      r.rest <- r.rest_column;
      r.rest_row <- 0);
  str w "\027[?7h";
  if cursor <> Hidden then str w "\027[?25h";
  str w esu;
  r.cursor <- cursor;
  r.moved <- false

let draw r ~cols ~rows ?(above = "") ?(cursor = Rest) frame =
  check ~cols ~rows;
  let width = width r ~cols in
  let shown = rows_of r ~width ~height:(rows - 1) frame in
  let height = List.length shown in
  let cursor = placed cursor ~height in
  if
    above = "" && (not r.moved) && r.width = width && r.rows = Some shown
    && r.cursor = cursor
  then ""
  else
    let w = Ansi.Writer.create 256 in
    afresh r w ~cols ~above;
    (* Autowrap off: a row the terminal finds too wide is cut at its edge,
       and the cursor goes back up as many rows as it went down. *)
    str w "\027[?7l";
    List.iter
      (fun row ->
        str w "\r\n";
        str w row)
      shown;
    Ansi.cursor_up ~n:height w;
    Ansi.Writer.write_char w '\r';
    r.height <- height;
    r.width <- width;
    r.rows <- Some shown;
    closing r w cursor;
    Ansi.Writer.contents w

let start r ~cols ~rows ?(above = "") ?(whole = false) ~height w =
  check ~cols ~rows;
  let width = width r ~cols in
  let height = max 0 (min height (rows - 1)) in
  let whole =
    whole || above <> "" || r.moved || r.width <> width || r.height <> height
  in
  if whole then (
    afresh r w ~cols ~above;
    str w "\027[?7l";
    for _ = 1 to height do
      str w "\r\n"
    done;
    Ansi.cursor_up ~n:height w)
  else (
    opening r w;
    str w "\027[?7l");
  r.height <- height;
  r.width <- width;
  r.rows <- None;
  whole

let finish r ?(cursor = Rest) w =
  Ansi.Writer.write_char w '\r';
  closing r w (placed cursor ~height:r.height)

let reset r =
  r.height <- 0;
  r.width <- -1;
  r.rows <- None;
  r.cursor <- Rest;
  r.moved <- true;
  r.rest <- 0;
  r.rest_row <- 0

let suspend r ~cols =
  if cols < 1 then invalid_arg "Matrix_screen.Region: the size must be positive";
  let w = Ansi.Writer.create 64 in
  str w sgr0;
  to_origin r w;
  to_own_row r w ~cols;
  erase w;
  (* What writes next, such as a child process, finds the cursor shown,
     whatever the last write left. *)
  str w "\027[?25h";
  reset r;
  Ansi.Writer.contents w

let close r ~cols ~rows ?(above = "") frame =
  check ~cols ~rows;
  let width = width r ~cols in
  (* The frame is drawn from the origin row down and the cursor never goes
     back up, so every row shows: those the screen cannot hold scroll into
     the scrollback above the last ones. *)
  let shown = rows_of r ~width ~height:max_int frame in
  let w = Ansi.Writer.create 256 in
  afresh r w ~cols ~above;
  (match shown with
  | [] -> ()
  | first :: rest ->
      str w "\027[?7l";
      str w first;
      List.iter
        (fun row ->
          str w "\r\n";
          str w row)
        rest;
      str w "\027[?7h\r\n");
  str w "\027[?25h";
  str w esu;
  reset r;
  Ansi.Writer.contents w

let keep r n =
  let n = max 0 (min n r.height) in
  let w = Ansi.Writer.create 64 in
  str w sgr0;
  to_origin r w;
  Ansi.Writer.write_char w '\r';
  if n = 0 then erase w
  else (
    (* The origin row deleted (DL), the rows below move up onto it: the
       first [n] stay, and what follows them is erased. *)
    str w "\027[M";
    for _ = 2 to n do
      Ansi.Writer.write_char w '\n'
    done;
    str w "\r\n\027[J");
  str w "\027[?7h\027[?25h";
  reset r;
  Ansi.Writer.contents w

let clear r =
  reset r;
  r.moved <- false;
  String.concat "" [ sgr0; "\027[H\027[2J" ]

let teardown r =
  let below =
    if r.height > 0 then (
      let w = Ansi.Writer.create 16 in
      Ansi.cursor_down ~n:(r.height - r.rest_row) w;
      str w "\r\n";
      Ansi.Writer.contents w)
    else "\r"
  in
  String.concat "" [ "\024"; esu; "\027[?7h"; sgr0; "\027[?25h"; below ]
