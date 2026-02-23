(** Border character sets for terminal box drawing.

    This module provides Unicode box-drawing character presets (single, double,
    rounded, heavy) and ASCII fallbacks for drawing boxes in terminal UIs. Use
    these presets with {!Grid.draw_box} to create bordered regions, panels, and
    dialogs.

    {1 Overview}

    A border character set defines the 11 characters needed to draw a complete
    box: four corners, two edge types (horizontal and vertical), four
    T-junctions (top, bottom, left, right), and one cross intersection. All
    characters are stored as Unicode codepoints ([int]) for efficient rendering.

    {1 Usage Basics}

    Draw a box with a single-line border:
    {[
      Grid.draw_box grid ~x:5 ~y:2 ~width:40 ~height:10
        ~fill:(Ansi.Color.of_rgb 20 20 20)
        ()
    ]}

    Customize a preset with different characters:
    {[
      let dashed = Border.modify ~horizontal:(Uchar.of_int 0x2504) Border.single
    ]}

    {1 Border Characters} *)

type t = {
  top_left : int;
  top_right : int;
  bottom_left : int;
  bottom_right : int;
  horizontal : int;
  vertical : int;
  top_t : int;
  bottom_t : int;
  left_t : int;
  right_t : int;
  cross : int;
}
(** The set of characters used to draw a box. Stored as [int] for efficient
    rendering, but manageable via {!modify}. *)

(** {1 Presets} *)

val single : t
(** Light box drawing: ┌ ┐ └ ┘ ─ │ ┬ ┴ ├ ┤ ┼ *)

val double : t
(** Double box drawing: ╔ ╗ ╚ ╝ ═ ║ ╦ ╩ ╠ ╣ ╬ *)

val rounded : t
(** Rounded box drawing: ╭ ╮ ╰ ╯ ─ │ ┬ ┴ ├ ┤ ┼ *)

val heavy : t
(** Heavy box drawing: ┏ ┓ ┗ ┛ ━ ┃ ┳ ┻ ┣ ┫ ╋ *)

val ascii : t
(** ASCII fallback: + + + + - | + + + + + *)

val empty : t
(** Invisible border (spaces). Use for spacing without visible borders. *)

(** {1 Customization} *)

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
(** Create a new border style by overriding characters in an existing one.
    Accepts standard [Uchar.t] for convenience.

    Example:
    {[
      let dashed = Border.modify ~horizontal:(Uchar.of_int 0x2504) Border.single
    ]} *)

(** {1 Sides} *)

type side = [ `Top | `Right | `Bottom | `Left ]
(** Sides of a box. *)

val all : side list
(** [`Top; `Right; `Bottom; `Left] *)
