(** Syntax styles: maps capture-group names to terminal styles.

    A syntax style resolves dot-separated group names with hierarchical
    fallback: ["keyword.control.flow"] tries ["keyword.control.flow"], then
    ["keyword.control"], then ["keyword"]. This follows the TextMate/tree-sitter
    scope convention.

    {1:scopes Scope vocabulary}

    Portable highlighters emit scopes whose first segment is one of: [comment],
    [string], [number], [constant], [keyword], [operator], [punctuation],
    [function], [type], [constructor], [variable], [property], [module],
    [attribute], [label], [embedded], or [tag]. Dotted refinements, such as
    [function.builtin] and [keyword.control], retain the corresponding base
    category. [property] names a field, record label or object key;
    [constructor] names a variant or data constructor. [embedded] identifies an
    embedded-language region; its contents may carry more specific scopes.

    These are the highlight names of tree-sitter's documentation ("Syntax
    Highlighting",
    {:https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html}) and
    of its command-line tool's default theme, plus [label]. Some query
    collections name fields [variable.member]; producers should emit [property]
    instead. {!val-default} styles [variable.member] as a [property] so that
    fields from such producers still read as fields.

    This vocabulary is the backend/theme contract, not a language registry.
    Producers should normalize backend-specific captures into these categories.
    Applications may define additional scopes in a custom style. Unmapped scopes
    are retained and resolve to the base style; they are never dropped. Language
    names remain opaque application-defined strings. *)

(** {1:types Types} *)

type t
(** The type for syntax styles. *)

(** {1:constructors Constructors} *)

val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
(** [make ~base mappings] is a syntax style with [base] as the default style for
    unstyled text and [mappings] associating capture-group names to overlay
    styles. When a group name appears more than once, its last mapping wins.

    Group names follow the tree-sitter/TextMate dot-separated convention:
    ["keyword"], ["keyword.control"], ["string.special.regex"], etc.

    See also {!val-default}. *)

val default : t
(** [default] is a style for dark terminals in fixed RGB colors, with mappings
    for common capture groups. A view given no style draws with [Theme.syntax]
    of the theme in scope instead. *)

val plain : t
(** [plain] is the style of a terminal whose colors are not reported, in
    attributes that change no color: [keyword] bold and [comment] italic, the
    other scopes in the base style. *)

val of_colors : Ansi.Color.reported -> t
(** [of_colors c] is {!plain} with the palette entry of a scope's hue where [c]
    reports it and it is legible ({!Ansi.Color.legible}), as
    {!Ansi.Theme.default} chooses the entries of text: [keyword] and [tag] take
    entry 5, [string] and [attribute] 2, [number], [constant], [constructor],
    [type] and [module] 3, [function] 4, each trying the entry and then the
    entry plus 8, without bold. A [comment] takes entry 8 where it is legible
    and its contrast is below the pair's, and stays italic. Colors are written
    as palette indices. *)

(** {1:predicates Predicates} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have equal base styles and map the
    same group names to equal overlays. Styles are values: two styles built by
    separate calls from the same mappings are equal, so a view may rebuild its
    style on every render without restyling anything. *)

(** {1:resolving Resolving} *)

val base : t -> Ansi.Style.t
(** [base style] is the default style for text outside highlighted ranges. *)

val resolve_overlay : t -> string -> Ansi.Style.t
(** [resolve_overlay style group] is the raw overlay style for [group], using
    hierarchical fallback. Returns {!Ansi.Style.default} when no match is found.

    Unlike {!val-resolve}, the result is {e not} merged with the base style,
    which is useful for composing multiple overlays manually. *)

val resolve : t -> string -> Ansi.Style.t
(** [resolve style group] is the complete style for [group]: the group's overlay
    merged on top of the base style, using hierarchical fallback. Returns the
    base style when no match is found.

    See also {!val-resolve_overlay}. *)
