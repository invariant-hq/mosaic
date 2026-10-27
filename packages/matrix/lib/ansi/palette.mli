(** Palettes, the colors a terminal's user configured.

    A terminal shows the default color ([39], [49]) and each palette entry
    ([38;5;n]) in a color its user configured, and programs may change them (OSC
    4, 10, 11 and 12). Its {e palette} is these colors: the default foreground
    and background, which together are the user's {e pair}, the color of the
    text cursor, and the 256 {e entries}, entry [i] being the color of [38;5;i].
    xterm calls the first ones its dynamic colors and the entries its color
    palette, and saves them together as one palette ("Push current dynamic- and
    ANSI-palette colors onto stack", ctlseqs, XTPUSHCOLORS).

    A program knows a terminal's palette only from what the terminal reports
    when asked (OSC 10, 11, 12 and 4 with [?]), so a palette also records what
    is known of one. Its pair is always known. Its cursor color and each of its
    entries is a color or {e unknown}, and a terminal that does not report a
    color leaves it unknown.

    The palette changes when the user picks another theme. A terminal with color
    scheme reports (mode 2031) then reports its color scheme, the light or dark
    preference, which says that the palette may have changed.

    A color is {e shown on} a palette [p] as the terminal draws it. The default
    color, and a transparent RGB color, which SGR writes as the default, are
    [p]'s foreground or background. An entry is [p]'s entry, unknown where [p]
    does not know it, and any other color is the RGB of its snapshot ({!Color}).

    No entry is legible on both a light and a dark background, so a program that
    draws text in an entry checks the entry against the background. An entry is
    legible when its contrast ({!Color.contrast}) against the background reaches
    a {e target}. The target of text is the lower of [4.5] and the contrast of
    the pair, so a user who chose a pair of lower contrast gets entries down to
    their own choice.

    This draws a link in entry 4 where it is legible on the terminal's
    background, and underlined otherwise:
    {[
    let link p =
      match Ansi.Palette.legible p 4 with
      | Some blue -> Ansi.Style.make ~fg:blue ()
      | None -> Ansi.Style.make ~underline:true ()
    ]} *)

type t = {
  foreground : int * int * int;  (** The default foreground, OSC 10. *)
  background : int * int * int;  (** The default background, OSC 11. *)
  cursor : (int * int * int) option;
      (** The text cursor's color, OSC 12, or [None] if unknown. *)
  entries : (int * int * int) option array;
      (** Entry [i] at index [i], OSC 4, or [None] if unknown. An entry past the
          array is unknown, and the array has at most 256 elements. *)
}
(** The type for palettes. Each color is red, green and blue, from 0 to 255. *)

val default : t
(** [default] is the palette {!Color} assumes: every entry is that of
    {!Color.indexed}, the foreground and the cursor are entry 7 and the
    background entry 0. The caller must not change its [entries]. *)

val entry : t -> int -> (int * int * int) option
(** [entry p i] is entry [i] of [p], or [None] if [p] does not know it or [i] is
    outside \[[0];[255]\]. *)

val contrast : t -> fg:Color.t -> bg:Color.t -> float option
(** [contrast p ~fg ~bg] is the contrast ({!Color.contrast}) of [fg] drawn as a
    foreground on [bg] drawn as a background, both shown on [p]. It is [None] if
    [fg] or [bg] is an entry [p] does not know. *)

val legible : ?target:float -> t -> int -> Color.t option
(** [legible ~target p i] is entry [i], or else, for [i] below 8, its bright
    variant [i + 8], the first that [p] knows and whose contrast against [p]'s
    background reaches [target]. It is [None] if neither does. [target] defaults
    to the target of text. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same pair and cursor and know
    the same entries with the same colors. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a palette for debugging. *)
