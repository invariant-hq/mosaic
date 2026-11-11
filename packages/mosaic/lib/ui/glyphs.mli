(** The characters widgets draw to show state.

    A {e glyph} is a string a widget draws for a state rather than for content:
    the mark before a collection's cursor item, a checkbox's box, a tree node's
    expansion. A set of glyphs gives one for each such state, and a program
    picks the set its terminal can show: {!unicode} by default, {!ascii} for a
    terminal or a font without the Unicode characters. Glyphs keep state visible
    when an output shows no colour or no attribute at all.

    {!Select}, {!Tab_select}, {!Tree}, {!Scroll_bar}, {!Progress_bar} and
    {!Spinner} draw from the set of the theme in scope ({!Theme.glyphs}). A
    progress bar or a spinner given glyphs of its own draws those instead. The
    checks and the ellipsis are for a program's own views.

    A widget measures each glyph by the screen's width method, so glyphs of one
    set may differ in width. The glyphs a widget draws in one cell, named so
    below, must be one cell wide. *)

type progress = {
  filled : string;  (** Each cell of the filled portion. *)
  half : string;
      (** The cell where the value ends half way. In {!unicode} it is a half
          block on the filled side, the left half across and the lower half up;
          in {!ascii} it is [=]. *)
  empty : string;  (** Each cell of the unfilled portion. *)
}
(** The type for the glyphs of a progress bar's cells, each in one cell. *)

type spinner = {
  frames : string array;  (** Drawn in turn, each in one cell. *)
  period : float;  (** The seconds each frame shows. *)
}
(** The type for a spinner's frames and the time each one shows. *)

type t = {
  cursor : string;
      (** Before the cursor item of a focused collection: [❯] (U+276F). *)
  cursor_inactive : string;
      (** Before the cursor item of an unfocused collection: [›] (U+203A). *)
  checked : string;  (** A chosen item or a checked checkbox: [☑] (U+2611). *)
  unchecked : string;
      (** An item not chosen or an unchecked checkbox: [☐] (U+2610). *)
  expand : string;  (** A tree node that can be expanded: [▶] (U+25B6). *)
  collapse : string;  (** An expanded tree node: [▼] (U+25BC). *)
  leaf : string;  (** A tree node without children: a space. *)
  ellipsis : string;  (** The end of text cut to fit: […] (U+2026). *)
  thumb : string;
      (** A scroll indicator's position, in one cell: [█] (U+2588). *)
  more_before : string;
      (** Items scrolled off before a row of tabs, in one cell: [‹] (U+2039). *)
  more_after : string;
      (** Items scrolled off after a row of tabs, in one cell: [›] (U+203A). *)
  underline : string;
      (** A line under the current tab, repeated per cell: [▬] (U+25AC). *)
  arrow_up : string;  (** A scroll bar's button, in one cell: [▲] (U+25B2). *)
  arrow_down : string;  (** [▼] (U+25BC), in one cell. *)
  arrow_left : string;  (** [◀] (U+25C0), in one cell. *)
  arrow_right : string;  (** [▶] (U+25B6), in one cell. *)
  progress_horizontal : progress;
      (** A horizontal progress bar's: [█] (U+2588) filled, [▌] (U+258C) half
          and [─] (U+2500) empty. *)
  progress_vertical : progress;
      (** A vertical progress bar's: [█] (U+2588) filled, [▄] (U+2584) half and
          [│] (U+2502) empty. *)
  spinner : spinner;
      (** A spinner's: the braille patterns [⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏] (U+280B to U+280F), one
          every 80 ms. *)
}
(** The type for glyph sets. Each field documents its glyph in {!unicode}. *)

val unicode : t
(** [unicode] is the set each field of {!t} documents. *)

val ascii : t
(** [ascii] is the set of ASCII characters: [>] and [-] for the cursor, [[x]]
    and [[ ]] for checks, [+], [-] and a space for tree nodes, [...] for the
    ellipsis, [#] for the thumb, [<] and [>] for tabs scrolled off, [-] under
    the current tab, [^], [v], [<] and [>] for arrows, [#] filled, [=] half and
    [-] empty across ([|] up) for progress bars, and [-], a backslash, [|] and
    [/] for a spinner, one every 130 ms. *)

val equal : t -> t -> bool
(** [equal g g'] is [true] iff [g] and [g'] have equal glyphs. *)
