(** Terminal colors.

    Colors are represented as a sum type supporting 16 basic ANSI colors,
    256-color palette indices, and 24-bit truecolor with optional alpha.
    Constructors clamp components to their valid ranges instead of raising.

    The distinguished {!default} color is [Rgba \{r=0; g=0; b=0; a=0\}] (fully
    transparent). Renderers treat alpha = 0 as "use terminal default" and emit
    SGR 39/49.

    {b Note.} {!pack}/{!unpack} require a 64-bit platform
    ([Sys.int_size >= 62]). *)

(** {1:colors Colors} *)

(** The type for terminal colors. *)
type t =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black  (** Often rendered as gray. *)
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Extended of int
      (** Extended palette index in \[0, 255\]. Indices 0–15 are basic colors,
          16–231 the 6×6×6 RGB cube ([r*36 + g*6 + b + 16] where [r],[g],[b] ∈
          \[0,5\]), 232–255 a 24-level grayscale ramp. Out-of-range values are
          clamped. *)
  | Rgb of { r : int; g : int; b : int }
      (** Truecolor RGB. Components in \[0, 255\], clamped. *)
  | Rgba of { r : int; g : int; b : int; a : int }
      (** Truecolor RGBA. Alpha in \[0, 255\] where 0 is fully transparent and
          255 fully opaque. Alpha is used for blending but not directly emitted
          to the terminal. *)

(** {1:constructors Constructors} *)

val default : t
(** [default] is [Rgba \{r=0; g=0; b=0; a=0\}]. *)

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

val of_rgb : int -> int -> int -> t
(** [of_rgb r g b] is a truecolor RGB value. Components are clamped to \[0,
    255\]. The result is opaque (alpha = 255). *)

val of_rgba : int -> int -> int -> int -> t
(** [of_rgba r g b a] is a truecolor RGBA value. Components are clamped to \[0,
    255\]. *)

val of_rgba_f : float -> float -> float -> float -> t
(** [of_rgba_f r g b a] is a color from normalized RGBA floats in \[0.0, 1.0\].
    Components are clamped and converted to 8-bit integers. Inverse of
    {!to_rgba_f} within rounding tolerance. *)

val of_palette_index : int -> t
(** [of_palette_index idx] is a color from the 256-color palette at [idx],
    clamped to \[0, 255\]. Indices 0–15 return the corresponding basic color
    variant. *)

val grayscale : level:int -> t
(** [grayscale ~level] is a grayscale color. [level] is in \[0, 23\] where 0 is
    darkest and 23 lightest; out-of-range values are clamped. Maps to palette
    indices 232–255. *)

val of_hsl : h:float -> s:float -> l:float -> ?a:float -> unit -> t
(** [of_hsl ~h ~s ~l ?a ()] is a color from HSL values with:
    - [h] is hue in degrees \[0.0, 360.0\), wrapped. Negative values are
      normalized (e.g. [-10] becomes [350]).
    - [s] is saturation in \[0.0, 1.0\], clamped.
    - [l] is lightness in \[0.0, 1.0\], clamped.
    - [a] is alpha in \[0.0, 1.0\], defaults to [1.0], clamped.

    Returns {!Rgb} if alpha is [1.0], {!Rgba} otherwise. *)

val of_hex : string -> t option
(** [of_hex s] parses a hex color string. Accepted formats (with or without
    ['#'] prefix):
    - 3 digits: ["RGB"] expanded to ["RRGGBB"].
    - 4 digits: ["RGBA"] expanded to ["RRGGBBAA"].
    - 6 digits: ["RRGGBB"] (opaque).
    - 8 digits: ["RRGGBBAA"] (with alpha).

    Returns [None] on invalid format or non-hex characters. *)

val of_hex_exn : string -> t
(** [of_hex_exn s] is like {!of_hex} but raises [Invalid_argument] on failure.
*)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have identical RGBA components. Colors
    with different representations but identical RGBA values are equal (e.g.
    [Extended 0] equals [Black]). *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by RGBA components. The order is compatible
    with {!equal}. *)

val hash : t -> int
(** [hash c] is a hash of [c]'s RGBA components. Compatible with {!equal}. *)

(** {1:properties Properties} *)

val alpha : t -> float
(** [alpha c] is the alpha channel of [c] as a float in \[0.0, 1.0\]. Non-RGBA
    variants return [1.0]. {!default} returns [0.0]. *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the RGB components of [c] in \[0, 255\], discarding alpha.
    {!default} maps to [(0, 0, 0)]; use {!alpha} or {!to_rgba} to distinguish it
    from explicit black. *)

val to_rgba : t -> int * int * int * int
(** [to_rgba c] is the RGBA components of [c] in \[0, 255\]. Non-RGBA variants
    return alpha = 255. *)

val to_rgba_f : t -> float * float * float * float
(** [to_rgba_f c] is the RGBA components of [c] as normalized floats in \[0.0,
    1.0\]. *)

val with_rgba_f : t -> (float -> float -> float -> float -> 'a) -> 'a
(** [with_rgba_f c f] calls [f r g b a] with normalized RGBA floats. Avoids the
    tuple allocation of {!to_rgba_f}. *)

val to_hsl : t -> float * float * float * float
(** [to_hsl c] is [(h, s, l, a)] where [h] is hue in \[0.0, 360.0\), [s] and [l]
    are in \[0.0, 1.0\], and [a] is alpha in \[0.0, 1.0\]. Achromatic colors
    have hue = 0. *)

(** {1:operations Operations} *)

val blend : ?mode:[ `Linear | `Perceptual ] -> src:t -> dst:t -> unit -> t
(** [blend ~mode ~src ~dst ()] alpha-blends [src] over [dst] with:
    - [`Linear] standard alpha compositing.
    - [`Perceptual] adjusts alpha using perceptual curves for more natural
      appearance on semi-transparent overlays.

    If [src] alpha ≥ 0.999, returns [src] RGB unchanged. If alpha ≤ ε, returns
    [dst] unchanged. *)

val downgrade : ?level:[ `Ansi16 | `Ansi256 | `Truecolor ] -> t -> t
(** [downgrade ~level c] converts [c] to the specified color depth using
    nearest-neighbor matching in RGB space (squared Euclidean distance). [level]
    defaults to detection from environment variables ([COLORTERM], [TERM]).
    [`Truecolor] returns [c] unchanged. Transparent colors ({!default}) pass
    through unchanged. *)

val invert : t -> t
(** [invert c] maps each RGB component [v] to [255 - v]. The result is always
    opaque {!Rgb}; alpha is discarded. *)

(** {1:sgr ANSI SGR codes} *)

val to_sgr_codes : bg:bool -> t -> int list
(** [to_sgr_codes ~bg c] is the SGR parameter codes for [c]. If [bg] is [true],
    generates background codes; otherwise foreground codes. Uses the most
    compact encoding for each color variant. *)

val emit_sgr_codes : bg:bool -> (int -> unit) -> t -> unit
(** [emit_sgr_codes ~bg push c] emits SGR codes for [c] via the [push] callback.
    Zero-allocation alternative to {!to_sgr_codes}. *)

(** {1:encoding Binary encoding} *)

val pack : t -> int
(** [pack c] encodes [c] to an unboxed integer for compact storage. Uses a
    tagged representation (3 tag bits + data).

    [equal c (unpack (pack c))] holds for all [c].

    {b Note.} Requires a 64-bit platform. *)

val unpack : int -> t
(** [unpack bits] decodes a packed color. Invalid tags decode to {!default}. *)

(** {1:converting Converting} *)

val to_hex : t -> string
(** [to_hex c] is the ["#RRGGBB"] hex string for [c]. Alpha is ignored. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a color for inspection (e.g. ["Red"], ["Rgb(100,150,200)"],
    ["Extended(42)"]). *)
