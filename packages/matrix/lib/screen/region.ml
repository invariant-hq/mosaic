(* The region's state is what the terminal shows of it since its last
   write: the rows drawn under the origin, the width they were cut to and
   their bytes, so that a write that would change nothing writes nothing. *)

type t = {
  eager_wrap : bool;
  rest_column : int;  (** Where a write leaves the cursor: 0, or 1. *)
  styling : [ `Plain | Ansi.Color.depth ];
  mutable height : int;  (** Rows drawn under the origin. *)
  mutable shown : (int * string list) option;
      (** The width and the rows of the last write, while it shows. *)
  mutable moved : bool;  (** The next write moves to a row of its own. *)
  mutable rest : int;  (** The column where the last write left the cursor. *)
}

let create ?(eager_wrap = false) ?(lifts_resting_cursor = false)
    ?(styling = `Truecolor) () =
  {
    eager_wrap;
    rest_column = (if lifts_resting_cursor then 1 else 0);
    styling;
    height = 0;
    shown = None;
    moved = true;
    rest = 0;
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
      let g =
        if Matrix_grid.width g <= width && shown = h then g
        else
          let w = min width (Matrix_grid.width g) in
          let dst = Matrix_grid.create_like g ~width:w ~height:shown in
          Matrix_grid.blit_region ~src:g ~dst ~src_x:0 ~src_y:(h - shown)
            ~width:w ~height:shown ~dst_x:0 ~dst_y:0;
          dst
      in
      String.split_on_char '\n' (Matrix_grid.to_ansi ~styling:r.styling g)

let bsu = "\027[?2026h"
let esu = "\027[?2026l"
let sgr0 = "\027[0m"

(* To a row of its own: as many spaces as a row takes past the column where
   the last write left the cursor, then CR. From that column they reach the
   end of the row, the last one leaving its wrap pending, which the CR
   cancels; from any column further right they wrap below the line someone
   left there, which stays. Counted from the first column while the cursor
   rests in the second, they would leave a blank row whenever nobody
   wrote. On a terminal that wraps eagerly a row takes one
   column less, so the spaces stop short of the last column. *)
let to_own_row r b ~cols =
  if r.moved then
    Buffer.add_string b (String.make (max 0 (width r ~cols - r.rest)) ' ');
  Buffer.add_char b '\r'

(* Erases from the start of the origin to the end of the screen. ED from
   the first column of the screen's first row clears the whole screen, which
   tmux keeps as history, so the first column is erased by EL and the rest
   from the second column. *)
let erase b = Buffer.add_string b "\027[2K\027[C\027[J\r"

(* The lines above, each ended by CR LF, with autowrap on: the terminal
   wraps them as any output, and the row after them is the origin. *)
let add_above b above =
  if above <> "" then (
    let lines = String.split_on_char '\n' above in
    let lines =
      match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
    in
    List.iter
      (fun line ->
        Buffer.add_string b line;
        Buffer.add_string b "\r\n")
      lines;
    Buffer.add_string b sgr0)

let start r b ~cols ~above =
  Buffer.add_string b bsu;
  Buffer.add_string b sgr0;
  Buffer.add_string b "\027[?25l";
  to_own_row r b ~cols;
  erase b;
  add_above b above

let finish b =
  Buffer.add_string b "\027[?25h";
  Buffer.add_string b esu

let draw r ~cols ~rows ?(above = "") frame =
  check ~cols ~rows;
  let width = width r ~cols in
  let shown = rows_of r ~width ~height:(rows - 1) frame in
  if above = "" && (not r.moved) && r.shown = Some (width, shown) then ""
  else
    let b = Buffer.create 256 in
    start r b ~cols ~above;
    (* Autowrap off: a row the terminal finds too wide is cut at its edge,
       and the cursor goes back up as many rows as it went down. *)
    Buffer.add_string b "\027[?7l";
    List.iter
      (fun row ->
        Buffer.add_string b "\r\n";
        Buffer.add_string b row)
      shown;
    let height = List.length shown in
    if height > 0 then Buffer.add_string b (Printf.sprintf "\027[%dA" height);
    (* On a terminal that moves a cursor resting in the first column up a row
       when its width changes (WezTerm, issue 7994), the cursor rests in the
       second column, or the next write would erase from a line above. *)
    Buffer.add_char b '\r';
    if r.rest_column = 1 then Buffer.add_string b "\027[C";
    Buffer.add_string b "\027[?7h";
    finish b;
    r.height <- height;
    r.shown <- Some (width, shown);
    r.moved <- false;
    r.rest <- r.rest_column;
    Buffer.contents b

let reset r =
  r.height <- 0;
  r.shown <- None;
  r.moved <- true;
  r.rest <- 0

let suspend r ~cols =
  if cols < 1 then invalid_arg "Matrix_screen.Region: the size must be positive";
  let b = Buffer.create 64 in
  Buffer.add_string b sgr0;
  to_own_row r b ~cols;
  erase b;
  reset r;
  Buffer.contents b

let close r ~cols ~rows ?(above = "") frame =
  check ~cols ~rows;
  let width = width r ~cols in
  (* The origin row itself is the frame's first, so it may show [rows]. *)
  let shown = rows_of r ~width ~height:rows frame in
  let b = Buffer.create 256 in
  start r b ~cols ~above;
  (match shown with
  | [] -> ()
  | first :: rest ->
      Buffer.add_string b "\027[?7l";
      Buffer.add_string b first;
      List.iter
        (fun row ->
          Buffer.add_string b "\r\n";
          Buffer.add_string b row)
        rest;
      Buffer.add_string b "\027[?7h\r\n");
  finish b;
  reset r;
  Buffer.contents b

let teardown r =
  let below =
    if r.height > 0 then Printf.sprintf "\027[%dB\r\n" r.height else "\r"
  in
  String.concat "" [ "\024"; esu; "\027[?7h"; sgr0; "\027[?25h"; below ]
