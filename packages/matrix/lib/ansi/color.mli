(** Colors, as a terminal is told to show them.

    A terminal is told a color in one of three ways, each with its own SGR
    codes. It is told an RGB value ([38;2;r;g;b]), an entry of its palette
    ([38;5;n]), or its default color ([39] or [49]). A color of this module
    records which, its {e intent} ({!type-intent}), so that SGR output names the
    color the program chose. It also records an RGBA value, its {e snapshot}.
    The RGB of the snapshot is what this module assumes the color looks like,
    and its alpha weights the color when it is {e blended} over another, by
    source-over compositing ({!blend}). Blending and comparing work on
    snapshots, and SGR output works on intents.

    The snapshot of a palette entry is the RGB the section {!section-depth}
    gives it. The snapshot of the default foreground is white and that of the
    default background black, since a program cannot know the terminal's. A
    color whose alpha is [0] is {e transparent}, and SGR writes a transparent
    RGB color as the default color.

    Constructors clamp out-of-range components and do not raise, except
    {!of_hex_exn}.

    This blends a translucent red over the default background. The result is an
    RGB color, half the red's intensity over black:
    {[
    let c =
      Ansi.Color.blend
        ~src:(Ansi.Color.of_rgba 255 0 0 128)
        ~dst:Ansi.Color.default_bg
    in
    assert (Ansi.Color.to_sgr_codes ~bg:true c = [ 48; 2; 128; 0; 0 ])
    ]} *)

(** {1:colors Colors} *)

type t
(** The type for colors. *)

(** The type for intents. *)
type intent =
  | Default  (** The terminal's default color, SGR [39] or [49]. *)
  | Indexed of int
      (** Palette entry [i], in \[[0];[255]\], SGR [38;5;i] or [48;5;i]. *)
  | Rgb
      (** The snapshot's RGB, SGR [38;2;r;g;b] or [48;2;r;g;b], or the default
          color when the color is transparent. *)

(** {1:constructors Constructors} *)

val default_fg : t
(** [default_fg] is the opaque default foreground, with a white snapshot. *)

val default_bg : t
(** [default_bg] is the opaque default background, with a black snapshot. *)

val transparent : t
(** [transparent] is the transparent RGB color with a black snapshot. Blending
    it over a color leaves that color unchanged. *)

(** {2:named Palette colors}

    [black] to [white] are palette entries 0 to 7, and [bright_black] to
    [bright_white] entries 8 to 15, as {!indexed} gives them. *)

val black : t
val red : t
val green : t
val yellow : t
val blue : t
val magenta : t
val cyan : t
val white : t
val bright_black : t
val bright_red : t
val bright_green : t
val bright_yellow : t
val bright_blue : t
val bright_magenta : t
val bright_cyan : t
val bright_white : t

(** {2:making Other colors} *)

val of_rgb : int -> int -> int -> t
(** [of_rgb r g b] is the opaque RGB color [(r, g, b)], each component clamped
    to \[[0];[255]\]. *)

val of_rgba : int -> int -> int -> int -> t
(** [of_rgba r g b a] is the RGB color [(r, g, b)] with alpha [a], each
    component clamped to \[[0];[255]\]. *)

val of_rgba_f : float -> float -> float -> float -> t
(** [of_rgba_f r g b a] is {!of_rgba} of the components clamped to
    \[[0.0];[1.0]\], scaled to \[[0];[255]\] and rounded. For every color [c],
    [of_rgba_f] of the components of {!to_rgba_f}[ c] has [c]'s snapshot. *)

val indexed : int -> t
(** [indexed i] is palette entry [i], clamped to \[[0];[255]\], with the
    snapshot {!section-depth} gives it. *)

val of_palette_index : int -> t
(** [of_palette_index] is {!indexed}. *)

val grayscale : level:int -> t
(** [grayscale ~level] is palette entry [232 + level], the gray of level
    [level], clamped to \[[0];[23]\], from darkest to lightest. *)

val of_hsl : h:float -> s:float -> l:float -> ?a:float -> unit -> t
(** [of_hsl ~h ~s ~l ?a ()] is the RGB color of hue [h] in degrees, saturation
    [s] and lightness [l], with alpha [a]. [h] is taken modulo 360, and [s], [l]
    and [a] are clamped to \[[0.0];[1.0]\]. [a] defaults to [1.0]. *)

val of_hex : string -> t option
(** [of_hex s] is the RGB color written in [s] as [RGB], [RGBA], [RRGGBB] or
    [RRGGBBAA] in hexadecimal digits, optionally after a ['#'], and [None] if
    [s] has another form. A digit of the short forms stands for two equal
    digits, and a form without alpha is opaque. *)

val of_hex_exn : string -> t
(** [of_hex_exn s] is like {!of_hex}.

    Raises [Invalid_argument] if [s] is none of the forms {!of_hex} reads. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same intent and the same
    snapshot. *)

val equal_rgba : t -> t -> bool
(** [equal_rgba a b] is [true] iff [a] and [b] have the same snapshot, whatever
    their intents. *)

val compare : t -> t -> int
(** [compare a b] is a total order on colors, compatible with {!equal}. *)

val hash : t -> int
(** [hash c] is a hash of [c], compatible with {!equal}. *)

(** {1:properties Properties} *)

val intent : t -> intent
(** [intent c] is [c]'s intent. *)

val alpha : t -> float
(** [alpha c] is [c]'s alpha, in \[[0.0];[1.0]\]. *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the RGB of [c]'s snapshot. *)

val to_rgba : t -> int * int * int * int
(** [to_rgba c] is [c]'s snapshot. *)

val to_rgba_f : t -> float * float * float * float
(** [to_rgba_f c] is [c]'s snapshot with each component in \[[0.0];[1.0]\]. *)

val with_rgba_f : t -> (float -> float -> float -> float -> 'a) -> 'a
(** [with_rgba_f c f] is [f r g b a], where [(r, g, b, a)] is {!to_rgba_f}[ c].
    It builds no tuple. *)

val to_hsl : t -> float * float * float * float
(** [to_hsl c] is [(h, s, l, a)], the hue in \[[0.0];[360.0]\), the saturation
    and the lightness in \[[0.0];[1.0]\] of [c]'s snapshot, and its alpha. *)

(** {1:operations Operations} *)

val blend : src:t -> dst:t -> t
(** [blend ~src ~dst] is [src] composited over [dst] by source-over blending of
    their snapshots. It is [dst] if [src] is transparent and [src] if [src] is
    opaque, intents included. Otherwise it is an RGB color whose alpha combines
    the two. *)

val invert : t -> t
(** [invert c] is the opaque RGB color whose components are [255 - v] for each
    component [v] of the RGB of [c]'s snapshot. *)

val contrast : t -> t -> float
(** [contrast a b] is the contrast ratio of the RGB of [a]'s and [b]'s
    snapshots, from [1.0] for two equal colors to [21.0] for black and white. It
    is WCAG 2's ratio, [(l1 + 0.05) / (l2 + 0.05)], where [l1] is the higher
    relative luminance of the two and [l2] the lower, and it is symmetric. WCAG
    asks at least [4.5] of text against its background and [3.0] of a graphical
    object. Alpha and intent are ignored, so a palette entry or a default color
    is judged by the RGB this module assumes for it ({!section-depth}), and a
    program that knows the terminal's colors judges {!of_rgb} colors of them. *)

(** {1:depth Color depth}

    A terminal shows colors at one of a few depths. With direct color it shows
    any RGB value ([38;2]). With 256 colors it shows the entries of xterm's
    palette ([38;5]). Entries 0 to 15 are the 16 colors below, 16 to 231 a 6×6×6
    cube whose levels in each channel are 0, 95, 135, 175, 215 and 255, and 232
    to 255 a ramp of grays from 8 to 238 by steps of 10, which leaves out black
    and white. With 16 colors it shows entries 0 to 15 ([30]..[37], [90]..[97]),
    with 8 colors entries 0 to 7, and a monochrome terminal shows only its
    default colors.

    Entries 16 to 255 are the same in every terminal that has them. Entries 0 to
    15 vary, since xterm takes them from its resources and users and themes set
    them, so a program cannot know them. {!indexed} gives them the 16 colors of
    HTML 4.01 (maroon [#800000], green [#008000], ..., silver [#c0c0c0], gray
    [#808080], red [#ff0000], ..., white [#ffffff]), which this module uses
    wherever it needs their RGB. *)

type depth = [ `Truecolor | `Ansi256 | `Ansi16 | `Ansi8 | `Mono ]
(** The type for color depths, which are direct color, 256, 16 and 8 colors, and
    only the default colors. *)

val to_depth : depth -> t -> t
(** [to_depth d c] is [c] as a terminal of depth [d] can show it:
    - A {!Default} color is unchanged at every depth, and so is every color at
      [`Truecolor].
    - At [`Mono] every color becomes the default color with [c]'s snapshot.
    - At [`Ansi256], [`Ansi16] and [`Ansi8], a color already {!Indexed} within
      the depth, or with alpha [0], is unchanged. Any other color becomes the
      palette entry nearest to the RGB of its snapshot, one of 16 to 255 at
      [`Ansi256], one of 0 to 15 at [`Ansi16] and one of 0 to 7 at [`Ansi8]. It
      keeps [c]'s alpha.

    Nearest is by the squared Euclidean distance between RGB values, a tie going
    to the higher entry. xterm shows a direct color this way when it is built
    without direct-color support: "it uses the closest match in its palette for
    the given RGB" (ctlseqs, SGR 38). At [`Ansi256] the search leaves out
    entries 0 to 15, whose colors the user sets, so the result shows the color
    it was chosen for. At [`Ansi16] and [`Ansi8] those are the only entries, and
    the result is nearest by the colors {!indexed} assumes.

    [to_depth] reads no environment. The caller gives the depth, from what the
    terminal reports. *)

(** {1:terminal_colors Reported colors}

    A terminal's default foreground and background, the user's {e pair}, and
    entries 0 to 15 of its palette are colors its user chose, and a program
    knows them only if the terminal reports them (OSC 10, 11 and 4). A color is
    {e shown on} a terminal of reported colors [c] as the terminal draws it. The
    default color, and a transparent RGB color, which SGR writes as the default,
    are [c]'s foreground or background. An entry 0 to 15 is [c]'s report of it,
    and any other color is the RGB of its snapshot.

    No palette entry is legible on both a light and a dark background, so a
    program that draws text in an entry checks the entry against the background
    its terminal reports. An entry is legible when its contrast ({!contrast})
    against that background reaches a {e target}. The target of text is the
    lower of [4.5] and the contrast of the pair, so a user who chose a pair of
    lower contrast gets entries down to their own choice. *)

type reported = {
  foreground : int * int * int;  (** The default foreground, OSC 10. *)
  background : int * int * int;  (** The default background, OSC 11. *)
  palette : (int * int * int) option array;
      (** Entries 0 to 15 of the palette, OSC 4, [None] where the terminal did
          not report one. An entry past the array is not reported, and entries
          past 15 are ignored. *)
}
(** The type for the colors a terminal reported, each red, green and blue from 0
    to 255. *)

val contrast_on : reported -> fg:t -> bg:t -> float option
(** [contrast_on c ~fg ~bg] is the contrast of [fg] drawn as a foreground on
    [bg] drawn as a background, both shown on a terminal of reported colors [c].
    It is [None] if [fg] or [bg] is an entry 0 to 15 [c] does not report. *)

val legible : ?target:float -> reported -> int -> t option
(** [legible ~target c i] is palette entry [i], or else, for [i] below 8, its
    bright variant [i + 8], the first that [c] reports and whose contrast
    against [c]'s background reaches [target]. It is [None] if neither does.
    [target] defaults to the target of text. *)

(** {1:sgr SGR codes} *)

val to_sgr_codes : ?depth:depth -> bg:bool -> t -> int list
(** [to_sgr_codes ~depth ~bg c] is the SGR codes of {!to_depth}[ depth c]'s
    intent ({!type-intent}), for the foreground, or for the background if [bg]
    is [true]. At [`Ansi16] and [`Ansi8] a palette entry [i] is written as
    [30 + i] when [i < 8] and as [82 + i] otherwise, or [40 + i] and [92 + i]
    for the background, and at the other depths as [38;5;i] or [48;5;i]. [depth]
    defaults to [`Truecolor]. *)

val emit_sgr_codes : ?depth:depth -> bg:bool -> (int -> unit) -> t -> unit
(** [emit_sgr_codes ~depth ~bg push c] calls [push] on each code of
    {!to_sgr_codes}[ ~depth ~bg c], in order, and builds no list. *)

(** {1:packed Packed colors} *)

module Packed : sig
  (** Colors as immediate integers, for code that stores many colors, such as
      the planes of a grid.

      A packed color is {!encode}[ c]. Its bit layout is private to this module,
      so code stores and compares packed colors and reads them only with these
      functions. The module requires a 64-bit platform ([Sys.int_size >= 62]).
  *)

  type color = t
  (** The type for colors, {!Color.t}. *)

  val encode : color -> int
  (** [encode c] is [c] packed. *)

  val decode : int -> color
  (** [decode p] is the color [p] packs. Bits outside those of a packed color
      are ignored, and an integer whose intent bits are invalid decodes to
      {!default_bg}. *)

  val red : int -> int
  val green : int -> int
  val blue : int -> int

  val alpha : int -> int
  (** [red p], [green p], [blue p] and [alpha p] are the components of the
      snapshot of [p], in \[[0];[255]\]. *)

  val red_f : int -> float
  val green_f : int -> float
  val blue_f : int -> float

  val alpha_f : int -> float
  (** [red_f p], [green_f p], [blue_f p] and [alpha_f p] are the components of
      the snapshot of [p], in \[[0.0];[1.0]\]. *)

  val intent : int -> intent
  (** [intent p] is the intent of [p]. *)

  val indexed_slot : int -> int
  (** [indexed_slot p] is the palette entry of [p] when [intent p] is
      [Indexed _], and meaningless otherwise. *)

  val with_alpha : int -> int -> int
  (** [with_alpha p a] is [p] with alpha [a], clamped to \[[0];[255]\], and the
      same RGB and intent. *)

  val blend : src:int -> dst:int -> int
  (** [blend ~src ~dst] is {!Color.blend}[ ~src ~dst] on packed colors. It
      allocates nothing. *)

  val of_rgba_f : float -> float -> float -> float -> int
  (** [of_rgba_f r g b a] is {!Color.of_rgba_f}[ r g b a] packed. *)
end

(** {1:converting Converting} *)

val to_hex : t -> string
(** [to_hex c] is ["#rrggbb"], the RGB of [c]'s snapshot in lowercase
    hexadecimal digits. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] formats [c] for inspection, such as [Red], [Indexed(42)],
    [Rgb(1,2,3)], [Rgba(1,2,3,4)] or [Default]. *)
