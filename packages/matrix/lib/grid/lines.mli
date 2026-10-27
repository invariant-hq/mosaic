(* The line sets of [Matrix_grid.Lines], whose signature there documents them,
   with the cell codes [Matrix_grid.draw_lines] reads. A private module of
   matrix.grid. *)

type weight = [ `Light | `Heavy | `Double ]
type t

val create : x:int -> y:int -> width:int -> height:int -> t
val x : t -> int
val y : t -> int
val width : t -> int
val height : t -> int
val clear : t -> unit

val hline :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  length:int ->
  unit

val vline :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  length:int ->
  unit

val arm :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  [ `Up | `Down | `Left | `Right ] ->
  unit

type set = {
  horizontal : string;
  vertical : string;
  top_left : string;
  top_right : string;
  bottom_left : string;
  bottom_right : string;
  top_t : string;
  bottom_t : string;
  left_t : string;
  right_t : string;
  cross : string;
}

type glyphs = Unicode | Rounded | Set of set

val ascii : glyphs
val of_border : Border.t -> glyphs

(* The cell codes [Matrix_grid.draw_lines] reads. The code of a cell is [0]
   when no line passes through it. *)

val code : t -> int -> int
(** [code l idx] is the code of the cell of column [x l + i] and row [y l + j],
    where [idx] is [j * width l + i]. *)

val style : t -> int -> Ansi.Style.t
(** [style l code] is the style of a cell of code [code], not [0]. *)

val scalar : rounded:bool -> int -> Uchar.t
(** [scalar ~rounded code] is the box-drawing character of [code], not [0]. *)

val glyph : set -> int -> string
(** [glyph s code] is the glyph of [code], not [0], in [s]. *)
