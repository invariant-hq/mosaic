(** Roles, the purposes a program gives its text, and themes that give each role
    a style.

    A {e role} ({!type-role}) names why text looks as it does: [Muted] text
    matters less than the text around it, [Error] text reports a failure. A
    {e theme} ({!type-t}) gives each role a style, so a program styles by role
    and one value restyles all it prints, a command's output and a full-screen
    interface alike. A theme gives a role two styles. Its {e text} style
    ({!style}) draws words, such as a message. Its {e mark} style ({!mark})
    draws a {e state glyph}, a symbol that shows a state, such as [✗] for an
    error, [✓] for success or [!] for a warning.

    The terminal's user chose its colors, and a program knows them only if the
    terminal reports them ({!type-colors}). Its default foreground and
    background, the user's {e pair}, are legible together. No palette entry is
    legible on both a light and a dark background, and terminals do not agree on
    the entries. So a role's text keeps the pair, and takes a palette entry only
    where the entry is reported and reaches the role's {e target}, a contrast
    ratio ({!Color.contrast}) against the background. The target of text is the
    lower of [4.5] and the contrast of the pair, and that of a line the lower of
    [3] and the pair's, so a user who chose a pair of lower contrast gets
    entries down to their own choice. A role whose colors come from the palette
    is written as the palette's index, so the terminal shows the color its user
    set, and a 16-color terminal shows it exactly.

    {!default} is the theme of a terminal whose colors are reported, and
    {!plain} the theme of one whose colors are not. Neither combines bold with a
    palette entry, since many terminals draw bold text in entries 0 to 7 with
    entries 8 to 15, and neither uses dim, which some terminals draw as no
    change and others as text that cannot be read. A program that sets its own
    styles ({!with_style}) judges them with {!check}. Nothing in this module
    reads the environment or the terminal. *)

(** {1:roles Roles} *)

(** The type for roles. The first ten are roles of text, and [Border] is the
    role of lines, such as a box's. *)
type role =
  | Plain  (** Text with no particular purpose. *)
  | Muted  (** Text that matters less, such as a hint or a placeholder. *)
  | Emphasis  (** Text that matters more, such as a heading. *)
  | Accent  (** Text that draws the eye, such as a current choice. *)
  | Link  (** Text that leads elsewhere, such as a URL. *)
  | Ok  (** A success. *)
  | Warning  (** A warning. *)
  | Error  (** A failure. *)
  | Added  (** Added content, such as a diff's added lines. *)
  | Removed  (** Removed content, such as a diff's removed lines. *)
  | Border  (** Lines that separate or frame. *)

val roles : role list
(** [roles] is every role, in the order of {!type-role}. *)

(** {1:colors Colors} *)

type colors = Color.reported = {
  foreground : int * int * int;
  background : int * int * int;
  palette : (int * int * int) option array;
}
(** The type for the colors a terminal reported, {!Color.reported}. *)

(** {1:themes Themes} *)

type t
(** The type for themes. *)

val plain : t
(** [plain] is the theme of a terminal whose colors are not reported. Text is
    drawn in the pair, with bold for [Emphasis], [Accent], [Warning] and [Error]
    and underline for [Link]. A state glyph of [Ok], [Warning], [Error], [Link],
    [Accent], [Added] and [Removed] is drawn in palette entry 2, 3, 1, 4, 6, 2
    and 1, unless it is the only sign of its state ({!mark}). *)

val default : colors -> t
(** [default c] is {!plain} with the palette entries [c] reports where they
    reach their target against [c.background] ({!Color.legible}):
    - [Link], [Added] and [Removed] text is drawn in entry 4, 2 and 1, or else
      in entry 12, 10 and 9, the first that reaches the target of text. Link
      text stays underlined.
    - [Muted] text is drawn in entry 8 when it reaches the target of text and
      its contrast is below the pair's, so it is legible and quieter than the
      text around it.
    - A state glyph that is the only sign of its state ({!mark}) is drawn in its
      role's entry, or else in that entry plus 8, the first that reaches [3].
      The other roles' text stays as in {!plain}, since text whose meaning its
      words carry, such as an error message, is legible in the pair whatever the
      palette. *)

val style : t -> role -> Style.t
(** [style t role] is the style [t] gives text of [role]. *)

val mark : ?alone:bool -> t -> role -> Style.t
(** [mark ~alone t role] is the style [t] gives a state glyph of [role]. A glyph
    that a word repeats, such as [✗] before an error message, shows a state that
    the text also shows, and takes its role's palette entry whatever the
    terminal's colors. With [~alone:true] ([false] by default), the glyph is the
    only sign of the state, and takes an entry only where {!default} found one
    that reaches [3], and the role's text style otherwise. A role without a
    palette entry, such as [Muted], draws its glyphs in its text style. *)

val with_style : role -> Style.t -> t -> t
(** [with_style role s t] is [t] with the text and the state glyphs of [role]
    drawn in [s]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] give every role the same styles. *)

val check : colors -> t -> (role * float) list
(** [check c t] is each role whose text [t] draws below its target on a terminal
    of colors [c], with the contrast it has, in the order of {!type-role}. The
    text's foreground and background are the style's, or the pair where the
    style has none. A role whose colors are a palette entry that [c] does not
    report is left out, since its contrast is unknown. *)
