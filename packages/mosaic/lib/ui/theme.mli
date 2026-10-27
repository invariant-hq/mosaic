(** Themes, the styles an interface draws each role in.

    A {e role} ({!type-role}) names why something looks as it does. A role of
    text names why text is drawn as it is: [Muted] text matters less than the
    text around it, and [Error] text reports a failure. A {e surface} is the
    role of an area widgets draw text on, such as [Field], where the user enters
    text. A surface's style is a pair, a background and the foreground drawn on
    it, or neither, so that its text stays legible whatever the terminal's
    colors. A {e theme} ({!type-t}) gives each role a style, and holds the glyph
    set ({!Glyphs}) widgets show state with and the style of highlighted source
    ({!Syntax_style}). Drawing by role lets the theme choose the styles, so one
    value restyles a whole interface.

    A theme gives a role two styles. Its text style ({!style}) draws words, such
    as a message. Its {e mark} style ({!mark}) draws a {e state glyph}, a symbol
    that shows a state, such as [✗] for an error, [✓] for success or [!] for a
    warning.

    The terminal's user chose its colors, and a program knows them only if the
    terminal reports them ({!Ansi.Color.reported}). Its default foreground and
    background, the user's {e pair}, are legible together. No palette entry is
    legible on both a light and a dark background, so a role's text keeps the
    pair and takes a palette entry only where the entry is legible
    ({!Ansi.Color.legible}). A role whose colors come from the palette is
    written as the palette's index, so the terminal shows the color its user
    set. {!default} is the theme of a terminal whose colors are reported, and
    {!plain} the theme of one whose colors are not. Neither combines bold with a
    palette entry, since many terminals draw bold text in entries 0 to 7 with
    entries 8 to 15, and neither uses dim, which some terminals draw as no
    change and others as text that cannot be read. A program that sets its own
    styles ({!with_style}) judges them with {!check}.

    An element is drawn with the theme {e in scope} where it is. The root's
    theme is the program's: {!default} of the colors the terminal reports, and
    {!plain} until it reports them or when it reports none. An element inside
    [themed f] ([Mosaic.themed], {!Vnode.themed}), or a node given [f] with
    {!Renderable.set_theme}, is drawn with [f] applied to the theme in scope
    around it, so the nearest such [f] decides last, and an application can
    change one role of the root's theme without knowing the terminal's colors.
    {!Renderable.theme} is the theme in scope at a node, and a widget reads it
    as it draws, so a theme that changes redraws the elements in its scope
    without building the view again.

    An element's own style comes before the theme in scope: a color it is given
    by argument applies to it alone, before the nearest [themed], and the style
    of its content, such as a text's style, is merged over its role
    ({!Ansi.Style.merge}): the content's colors where it has them, and the
    attributes of both.

    The built-in widgets read the theme in scope as they draw, and their
    documents state the roles and glyphs they draw with. A canvas reads it with
    {!Canvas.theme}. *)

(** {1:roles Roles} *)

(** The type for roles. The first ten are roles of text, [Border] is the role of
    lines, such as a box's, and the last six are surfaces. *)
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
  | Canvas  (** The interface's background, where no other surface is. *)
  | Panel  (** An area set apart from the canvas, such as a dialog. *)
  | Field  (** Where the user enters text, such as an input. *)
  | Cursor  (** The item a focused collection's keys act on. *)
  | Selected  (** The items the user has chosen. *)
  | Highlight  (** Text the user has selected. *)

val roles : role list
(** [roles] is every role, in the order of {!type-role}. *)

(** {1:themes Themes} *)

type t
(** The type for themes. *)

val plain : t
(** [plain] is the theme of a terminal whose colors are not reported. Text is
    drawn in the pair, with bold for [Emphasis], [Accent], [Warning] and [Error]
    and underline for [Link]. A state glyph of [Ok], [Warning], [Error], [Link],
    [Accent], [Added] and [Removed] is drawn in palette entry 2, 3, 1, 4, 6, 2
    and 1, unless it is the only sign of its state ({!mark}). Surfaces are drawn
    in the pair, with reverse for [Cursor] and [Highlight] and bold and
    underline for [Selected]. The glyphs are {!Glyphs.unicode} and the syntax
    style {!Syntax_style.plain}. *)

val default : Ansi.Color.reported -> t
(** [default c] is {!plain} with the palette entries [c] reports where they are
    legible on [c]'s background ({!Ansi.Color.legible}), and the syntax style
    {!Syntax_style.of_colors}[ c]:
    - [Link], [Added] and [Removed] text is drawn in entry 4, 2 and 1, or else
      in entry 12, 10 and 9, the first that is legible. Link text stays
      underlined.
    - [Muted] text is drawn in entry 8 when it is legible and its contrast is
      below the pair's, so it is quieter than the text around it.
    - A state glyph that is the only sign of its state ({!mark}) is drawn in its
      role's entry, or else in that entry plus 8, the first whose contrast
      reaches [3]. The other roles stay as in {!plain}, since text whose meaning
      its words carry, such as an error message, is legible in the pair whatever
      the palette. *)

val style : t -> role -> Ansi.Style.t
(** [style t role] is the style [t] gives text of [role]. For a surface it is
    the style of the surface's area and the text drawn on it. *)

val mark : ?alone:bool -> t -> role -> Ansi.Style.t
(** [mark ~alone t role] is the style [t] gives a state glyph of [role]. A glyph
    that a word repeats, such as [✗] before an error message, shows a state that
    the text also shows, and takes its role's palette entry whatever the
    terminal's colors. With [~alone:true] ([false] by default), the glyph is the
    only sign of the state, and takes an entry only where {!default} found one
    whose contrast reaches [3], and the role's text style otherwise. A role
    without a palette entry, such as [Muted], draws its glyphs in its text
    style. *)

val glyphs : t -> Glyphs.t
(** [glyphs t] is the glyph set of [t]. *)

val syntax : t -> Syntax_style.t
(** [syntax t] is the style [t] gives highlighted source. *)

val with_style : role -> Ansi.Style.t -> t -> t
(** [with_style role s t] is [t] with [role] drawn in [s], its text and its
    state glyphs alike. *)

val with_glyphs : Glyphs.t -> t -> t
(** [with_glyphs g t] is [t] with the glyph set [g]. *)

val with_syntax : Syntax_style.t -> t -> t
(** [with_syntax s t] is [t] with highlighted source styled by [s]. *)

val check : Ansi.Color.reported -> t -> (role * float) list
(** [check c t] is each role whose text [t] draws below its target on a terminal
    of reported colors [c], with the contrast it has
    ({!Ansi.Color.contrast_on}), in the order of {!type-role}. The text's
    foreground and background are the style's, or the pair where the style has
    none, so a surface is judged by the text drawn on it. The target of [Border]
    is the lower of [3] and the contrast of the pair, and that of the other
    roles the target of text. A role whose colors are a palette entry that [c]
    does not report is left out, since its contrast is unknown. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] give every role the same styles and
    have equal glyph sets and syntax styles. *)
