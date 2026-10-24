(** The cells of a plot as marks paint them, before they reach a grid.

    An ink is a rectangle of cells, each with two {e planes}: a glyph with its
    foreground and attributes, and a background. A write replaces the plane it
    paints, except that dots of the resolution, foreground and attributes
    already in a cell merge into its glyph. Each plane keeps the {e owner}, the
    mark and row, of its last write. Writes outside the rectangle are left out.
*)

type res =
  | Half
  | Quadrant
  | Sextant
  | Braille
  | Octant
      (** The type for dot resolutions: 1×2, 2×2, 2×3, 2×4 (braille) and 2×4
          (octants) dots per cell. *)

val cols : res -> int
(** [cols r] is the number of dot columns per cell at [r]. *)

val rows : res -> int
(** [rows r] is the number of dot rows per cell at [r]. *)

type t
(** The type for inks. *)

val create : width:int -> height:int -> t
val width : t -> int
val height : t -> int

val owner : mark:int -> row:int -> int
(** [owner ~mark ~row] is the owner of a write by row [row] of mark [mark]. *)

val owner_mark : int -> int
val owner_row : int -> int

val dot :
  t ->
  res ->
  x:int ->
  y:int ->
  fg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  owner:int ->
  unit
(** [dot t r ~x ~y] inks the dot of column [x] and row [y], counted in dots of
    [r] from the top-left dot. *)

val glyph :
  t ->
  x:int ->
  y:int ->
  string ->
  fg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  owner:int ->
  unit
(** [glyph t ~x ~y s] writes [s], one cell wide, in the glyph plane. *)

val text :
  t ->
  width_method:Matrix_text.width_method ->
  x:int ->
  y:int ->
  string ->
  fg:Ansi.Color.t ->
  attrs:Ansi.Attr.t ->
  owner:int ->
  unit
(** [text t ~x ~y s] writes the units of [s] from column [x], each in as many
    cells as it is wide; a unit that does not fit whole is left out. *)

val fill : t -> x:int -> y:int -> bg:Ansi.Color.t -> owner:int -> unit
(** [fill t ~x ~y ~bg] writes [bg] in the background plane. *)

val glyph_owner : t -> x:int -> y:int -> int
(** [glyph_owner t ~x ~y] is the owner of the glyph plane of the cell, else
    [-1]. *)

val owner_at : t -> x:int -> y:int -> int
(** [owner_at t ~x ~y] is the owner of the glyph plane of the cell, else of its
    background plane, else [-1]. *)

val commit : t -> Matrix_grid.t -> x:int -> y:int -> unit
(** [commit t g ~x ~y] draws the cells of [t] whose planes are written into [g]
    with [t]'s top-left cell at [(x, y)]. A glyph without a background is drawn
    over the background already in [g]. *)
