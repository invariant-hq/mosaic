(** Terminal colors.

    Colors preserve their terminal emission intent: literal RGB, indexed
    palette, or terminal default. The RGB channels are also stored as a visual
    snapshot so callers can still blend, compare, and inspect colors without
    losing whether the renderer should emit [38;2], [38;5], or [39]/[49].

    Constructors clamp components to their valid ranges instead of raising. *)

(** {1:colors Colors} *)

type t
(** The type for terminal colors.

    Invariant: values built by this module contain clamped 8-bit RGBA channels
    and one of the intents in {!type-intent}. *)

type intent =
  | Default
  | Indexed of int
  | Rgb
      (** The terminal emission intent of a color. [Indexed i] has [i] in
          \[0,255\]. [Rgb] denotes literal truecolor RGBA. [Default] denotes
          terminal default foreground/background. *)

(** {1:constructors Constructors} *)

val default : t
(** [default] is the terminal-default color. It has alpha [0] for visual
    composition and emits SGR [39] or [49]. *)

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
(** [of_rgb r g b] is a literal truecolor RGB value. Components are clamped to
    \[0,255\]. The result is opaque. *)

val of_rgba : int -> int -> int -> int -> t
(** [of_rgba r g b a] is a literal truecolor RGBA value. Components are clamped
    to \[0,255\]. Alpha is used for blending and transparent SGR defaults. *)

val of_rgba_f : float -> float -> float -> float -> t
(** [of_rgba_f r g b a] is a color from normalized RGBA floats in \[0.0,1.0\].
    Components are clamped and converted to 8-bit integers. Inverse of
    {!to_rgba_f} within rounding tolerance. *)

val indexed : int -> t
(** [indexed i] is a color from the 256-color palette at [i], clamped to
    \[0,255\]. The color keeps the indexed intent and an RGB fallback snapshot.
*)

val of_palette_index : int -> t
(** [of_palette_index] is {!indexed}. *)

val grayscale : level:int -> t
(** [grayscale ~level] is a grayscale palette color. [level] is in \[0,23\]
    where [0] is darkest and [23] lightest; out-of-range values are clamped. *)

val of_hsl : h:float -> s:float -> l:float -> ?a:float -> unit -> t
(** [of_hsl ~h ~s ~l ?a ()] is a literal RGB/RGBA color from HSL values with:
    - [h] is hue in degrees \[0.0,360.0\), wrapped.
    - [s] is saturation in \[0.0,1.0\], clamped.
    - [l] is lightness in \[0.0,1.0\], clamped.
    - [a] is alpha in \[0.0,1.0\], defaults to [1.0], clamped. *)

val of_hex : string -> t option
(** [of_hex s] parses a literal RGB/RGBA hex color string. Accepted formats with
    or without ['#'] prefix are ["RGB"], ["RGBA"], ["RRGGBB"], and ["RRGGBBAA"].
    Returns [None] on invalid input. *)

val of_hex_exn : string -> t
(** [of_hex_exn s] is like {!of_hex} but raises [Invalid_argument] on invalid
    input. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have identical RGBA channels and
    identical emission intent. The order is compatible with {!compare}. *)

val equal_rgba : t -> t -> bool
(** [equal_rgba a b] is [true] iff [a] and [b] have identical RGBA channels,
    ignoring emission intent. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by channels and emission intent. The order
    is compatible with {!equal}. *)

val hash : t -> int
(** [hash c] hashes [c]'s channels and emission intent. Compatible with
    {!equal}. *)

(** {1:properties Properties} *)

val intent : t -> intent
(** [intent c] is [c]'s terminal emission intent. *)

val alpha : t -> float
(** [alpha c] is the alpha channel of [c] as a float in \[0.0,1.0\]. *)

val to_rgb : t -> int * int * int
(** [to_rgb c] is the RGB snapshot of [c] in \[0,255\], discarding alpha. *)

val to_rgba : t -> int * int * int * int
(** [to_rgba c] is the RGBA snapshot of [c] in \[0,255\]. *)

val to_rgba_f : t -> float * float * float * float
(** [to_rgba_f c] is the RGBA snapshot of [c] as normalized floats in
    \[0.0,1.0\]. *)

val with_rgba_f : t -> (float -> float -> float -> float -> 'a) -> 'a
(** [with_rgba_f c f] calls [f r g b a] with normalized RGBA floats. Avoids the
    tuple allocation of {!to_rgba_f}. *)

val to_hsl : t -> float * float * float * float
(** [to_hsl c] is [(h, s, l, a)] where [h] is hue in \[0.0,360.0\), [s] and [l]
    are in \[0.0,1.0\], and [a] is alpha in \[0.0,1.0\]. *)

(** {1:operations Operations} *)

val blend : ?mode:[ `Linear | `Perceptual ] -> src:t -> dst:t -> unit -> t
(** [blend ~mode ~src ~dst ()] alpha-blends [src] over [dst]. Fully opaque
    results are literal RGB colors. Fully transparent [src] returns [dst]. *)

val invert : t -> t
(** [invert c] maps each RGB component [v] to [255 - v]. The result is an opaque
    literal RGB color. *)

(** {1:sgr ANSI SGR codes} *)

val to_sgr_codes : bg:bool -> t -> int list
(** [to_sgr_codes ~bg c] is the SGR parameter codes for [c]. If [bg] is [true],
    generates background codes; otherwise foreground codes. *)

val emit_sgr_codes : bg:bool -> (int -> unit) -> t -> unit
(** [emit_sgr_codes ~bg push c] emits SGR codes for [c] via [push].
    Zero-allocation alternative to {!to_sgr_codes}. *)

(** {1:packed Packed representation} *)

module Packed : sig
  (** Low-level packed color operations.

      Packed colors are unboxed integers for render and grid hot paths. The bit
      layout is private to this module; callers may store and compare packed
      values, but must use these operations to inspect or emit them.

      Requires a 64-bit platform ([Sys.int_size >= 62]). *)

  type color = t
  (** The unpacked color type. *)

  val encode : color -> int
  (** [encode c] is [c] encoded as an unboxed integer. *)

  val decode : int -> color
  (** [decode bits] decodes [bits]. Invalid intent tags decode to {!default}. *)

  val red : int -> int
  val green : int -> int
  val blue : int -> int

  val alpha : int -> int
  (** Color channels in \[0,255\]. *)

  val red_f : int -> float
  val green_f : int -> float
  val blue_f : int -> float

  val alpha_f : int -> float
  (** Color channels in \[0.0,1.0\]. *)

  val intent : int -> intent
  (** [intent bits] is the terminal emission intent encoded in [bits]. *)

  val indexed_slot : int -> int
  (** [indexed_slot bits] is the indexed color slot encoded in [bits]. The value
      is meaningful only when [intent bits] is [Indexed _]. *)

  val of_rgba_f : float -> float -> float -> float -> int
  (** [of_rgba_f r g b a] is a packed literal RGBA color from normalized
      channels. *)
end

(** {1:converting Converting} *)

val to_hex : t -> string
(** [to_hex c] is the ["#RRGGBB"] hex string for [c]. Alpha and intent are
    ignored. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats [c] for inspection. *)
