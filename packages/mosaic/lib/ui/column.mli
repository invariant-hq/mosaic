(** Columns of a collection's rows: what each shows of an item, and how wide.

    A {e column} gives a {e cell} for each item, by a function of the item, and
    a header. A collection with columns shows each item as a row of cells, one
    per column, drawn from left to right with one cell of space between adjacent
    columns.

    A column's width is resolved over the width the columns share, as CSS sizes
    the columns of a table laid out automatically. Each column first takes its
    {e padding}, a number of cells on each side of its cells. A [`Fixed n]
    column then takes [n] cells. An [`Auto] column takes the width of its widest
    cell or header, which costs a pass over every item. Both are at least one
    cell wide, padding included. The width left after them is shared among the
    [`Fraction] columns in proportion to their fractions, rounded down, each at
    least one cell wider than its padding. An [`Auto] or [`Fraction] column's
    [min_width] and [max_width] then bound its width without padding; they do
    not apply to a [`Fixed] column. The resolved widths can add up to more than
    the width shared, which then cuts the columns at the right. *)

(** {1:cells Cells} *)

type cell
(** The type for cells: text, with an optional style, or styled fragments. *)

val text : ?style:Ansi.Style.t -> string -> cell
(** [text s] is the cell of [s], drawn in [style] when given. *)

val rich : Text.fragment list -> cell
(** [rich fragments] is the cell of [fragments]. *)

val empty : cell
(** [empty] is the cell of no text. *)

val cell_equal : cell -> cell -> bool
(** [cell_equal c c'] is [true] iff [c] and [c'] have equal text and styles. *)

(** {1:columns Columns} *)

type width = [ `Auto | `Fixed of int | `Fraction of float ]
(** The type for the ways a column's width is resolved. *)

type alignment = [ `Left | `Center | `Right ]
(** The type for the place of a cell's text in its column. *)

type overflow = [ `Ellipsis | `Crop ]
(** The type for what a cell does with text wider than its column: [`Ellipsis]
    ends it with an ellipsis, [`Crop] cuts it at the column's edge. *)

type 'a t = {
  header : string;  (** The header's text. *)
  width : width;  (** How the width is resolved. *)
  alignment : alignment;  (** Where text sits in a cell. *)
  overflow : overflow;  (** What text wider than the column does. *)
  min_width : int option;  (** The least width, without padding. *)
  max_width : int option;  (** The greatest width, without padding. *)
  cell : 'a -> cell;  (** The cell of an item. *)
}
(** The type for columns of ['a] items. [cell] must return equal cells for one
    item every time. *)

val make :
  ?header:string ->
  ?width:width ->
  ?alignment:alignment ->
  ?overflow:overflow ->
  ?min_width:int ->
  ?max_width:int ->
  ('a -> cell) ->
  'a t
(** [make cell] is the column of [cell]. [header] defaults to [""], [width] to
    [`Auto], [alignment] to [`Left] and [overflow] to [`Ellipsis]; [min_width]
    and [max_width] default to no bound. *)

(** {1:widths Widths} *)

val widths :
  width_method:Matrix.Text.width_method ->
  ?padding:int ->
  available:int ->
  'a t list ->
  'a Items.t ->
  int array
(** [widths ~width_method ~padding ~available columns items] is the width of
    each of [columns], padding included, when they share [available] cells, the
    spaces between them included, and show [items]. Text is measured by
    [width_method]. [padding] defaults to [0]. *)
