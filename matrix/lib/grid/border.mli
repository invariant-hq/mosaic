(** Box-drawing border character sets.

    A border defines the 11 Unicode scalars needed to draw a complete box: four
    corners, horizontal and vertical edges, four T-junctions, and a cross
    intersection. *)

(** {1:borders Borders} *)

type t = {
  top_left : Uchar.t;
  top_right : Uchar.t;
  bottom_left : Uchar.t;
  bottom_right : Uchar.t;
  horizontal : Uchar.t;
  vertical : Uchar.t;
  top_t : Uchar.t;
  bottom_t : Uchar.t;
  left_t : Uchar.t;
  right_t : Uchar.t;
  cross : Uchar.t;
}
(** The type for border character sets. *)

(** {1:presets Presets} *)

val single : t
(** Light box drawing: ┌ ┐ └ ┘ ─ │ ┬ ┴ ├ ┤ ┼ *)

val double : t
(** Double box drawing: ╔ ╗ ╚ ╝ ═ ║ ╦ ╩ ╠ ╣ ╬ *)

val rounded : t
(** Rounded box drawing: ╭ ╮ ╰ ╯ ─ │ ┬ ┴ ├ ┤ ┼ *)

val heavy : t
(** Heavy box drawing: ┏ ┓ ┗ ┛ ━ ┃ ┳ ┻ ┣ ┫ ╋ *)

val ascii : t
(** ASCII fallback: [+ + + + - | + + + + +]. *)

val empty : t
(** Invisible border (spaces). *)

(** {1:customizing Customizing} *)

val modify :
  ?top_left:Uchar.t ->
  ?top_right:Uchar.t ->
  ?bottom_left:Uchar.t ->
  ?bottom_right:Uchar.t ->
  ?horizontal:Uchar.t ->
  ?vertical:Uchar.t ->
  ?top_t:Uchar.t ->
  ?bottom_t:Uchar.t ->
  ?left_t:Uchar.t ->
  ?right_t:Uchar.t ->
  ?cross:Uchar.t ->
  t ->
  t
(** [modify ?top_left ... base] is [base] with the specified characters
    overridden. *)

(** {1:sides Sides} *)

type side = [ `Top | `Right | `Bottom | `Left ]
(** The type for box sides. *)

val all : side list
(** [all] is [[\`Top; \`Right; \`Bottom; \`Left]]. *)
