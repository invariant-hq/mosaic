(** Syntax themes: maps capture-group names to terminal styles.

    A theme resolves dot-separated group names with hierarchical fallback:
    ["keyword.control.flow"] tries ["keyword.control.flow"], then
    ["keyword.control"], then ["keyword"]. This matches the TextMate/tree-sitter
    scope convention. *)

(** {1:types Types} *)

type t
(** The type for syntax themes. *)

(** {1:constructors Constructors} *)

val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
(** [make ~base mappings] is a theme with [base] as the default style for
    unstyled text and [mappings] associating capture-group names to overlay
    styles.

    Group names follow the tree-sitter/TextMate dot-separated convention:
    ["keyword"], ["keyword.control"], ["string.special.regex"], etc.

    See also {!val-default}. *)

val default : t
(** [default] is the built-in dark theme with mappings for common capture
    groups. *)

(** {1:resolving Resolving} *)

val resolve_overlay : t -> string -> Ansi.Style.t
(** [resolve_overlay theme group] is the raw overlay style for [group] in
    [theme], using hierarchical fallback. Returns {!Ansi.Style.default} when no
    match is found.

    Unlike {!val-resolve}, the result is {e not} merged with the base style,
    which is useful for composing multiple overlays manually. *)

val resolve : t -> string -> Ansi.Style.t
(** [resolve theme group] is the complete style for [group] in [theme]: the
    group's overlay merged on top of the base style, using hierarchical
    fallback. Returns the base style when no match is found.

    See also {!val-resolve_overlay}. *)

(** {1:applying Applying} *)

val apply :
  t -> content:string -> (int * int * string) list -> Text_buffer.span list
(** [apply theme ~content ranges] is the list of styled {!Text_buffer.span}
    values for [content] under [theme].

    Each element of [ranges] is [(start_byte, end_byte, group_name)], where
    [start_byte] and [end_byte] are byte offsets into [content]. Ranges may
    overlap; overlapping groups are cascade-merged in specificity order
    (least-specific first), following CSS/TextMate semantics: child scopes
    inherit properties from parent scopes and override only what they define.
    Text outside any range receives the base style.

    Raises [Invalid_argument] if any byte offset is out of bounds for [content].
*)
