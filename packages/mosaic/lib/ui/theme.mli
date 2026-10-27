(** Themes, the styles an interface's roles are drawn in.

    A theme gives each role of text ({!Ansi.Theme.role}) its styles, as
    {!Ansi.Theme} does for printed output, and adds what only an interface has:
    the {e surfaces} ({!type-surface}), the areas widgets draw text on, and the
    glyph set ({!Glyphs}) widgets show state with. A surface's style is a pair,
    a background and the foreground drawn on it, or neither, so that its text
    stays legible whatever the terminal's colors. Drawing by role and surface
    lets the theme choose the styles, so one value restyles a whole interface.

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

    Text ({!Text}, {!Code}), boxes ({!Box}), editors ({!Text_input},
    {!Textarea}) and lists ({!Select}, {!Tab_select}, {!Table}, {!Tree}) read
    the theme in scope as they draw, and their documents state the roles and
    surfaces they draw with. The other built-in widgets do not read it yet, and
    draw with their own arguments and defaults. A canvas reads it with
    {!Canvas.theme}. *)

(** {1:surfaces Surfaces} *)

(** The type for surfaces. *)
type surface =
  | Canvas  (** The interface's background, where no other surface is. *)
  | Panel  (** An area set apart from the canvas, such as a dialog. *)
  | Field  (** Where the user enters text, such as an input. *)
  | Cursor  (** The item a focused collection's keys act on. *)
  | Selected  (** The items the user has chosen. *)
  | Highlight  (** Text the user has selected. *)

val surfaces : surface list
(** [surfaces] is every surface, in the order of {!type-surface}. *)

(** {1:themes Themes} *)

type t
(** The type for themes. *)

val plain : t
(** [plain] is the theme of a terminal whose colors are not reported: the text
    of {!Ansi.Theme.plain}, the glyphs of {!Glyphs.unicode}, and surfaces in the
    terminal's own pair, with reverse for [Cursor] and [Highlight] and bold and
    underline for [Selected]. *)

val default : Ansi.Theme.colors -> t
(** [default c] is {!plain} with the text of {!Ansi.Theme.default}[ c]. *)

val text : t -> Ansi.Theme.t
(** [text t] is the styles [t] gives the roles of text. *)

val style : t -> Ansi.Theme.role -> Ansi.Style.t
(** [style t role] is {!Ansi.Theme.style}[ (text t) role]. *)

val mark : ?alone:bool -> t -> Ansi.Theme.role -> Ansi.Style.t
(** [mark ~alone t role] is {!Ansi.Theme.mark}[ ~alone (text t) role]. *)

val surface : t -> surface -> Ansi.Style.t
(** [surface t s] is the style [t] gives surface [s]. *)

val glyphs : t -> Glyphs.t
(** [glyphs t] is the glyph set of [t]. *)

val with_text : Ansi.Theme.t -> t -> t
(** [with_text text t] is [t] with the roles of text styled by [text]. *)

val with_style : Ansi.Theme.role -> Ansi.Style.t -> t -> t
(** [with_style role s t] is [t] with the text and the state glyphs of [role]
    drawn in [s] ({!Ansi.Theme.with_style}). *)

val with_surface : surface -> Ansi.Style.t -> t -> t
(** [with_surface surface s t] is [t] with [surface] drawn in [s]. *)

val with_glyphs : Glyphs.t -> t -> t
(** [with_glyphs g t] is [t] with the glyph set [g]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] give every role and every surface the
    same styles and have equal glyph sets. *)
