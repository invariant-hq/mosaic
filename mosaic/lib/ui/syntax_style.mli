(** Syntax styles: maps capture-group names to terminal styles.

    A syntax style resolves dot-separated group names with hierarchical
    fallback: ["keyword.control.flow"] tries ["keyword.control.flow"], then
    ["keyword.control"], then ["keyword"]. This follows the TextMate/tree-sitter
    scope convention. *)

(** {1:types Types} *)

type t
(** The type for syntax styles. *)

(** {1:constructors Constructors} *)

val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
(** [make ~base mappings] is a syntax style with [base] as the default style for
    unstyled text and [mappings] associating capture-group names to overlay
    styles.

    Group names follow the tree-sitter/TextMate dot-separated convention:
    ["keyword"], ["keyword.control"], ["string.special.regex"], etc.

    See also {!val-default}. *)

val default : t
(** [default] is the built-in dark syntax style with mappings for common capture
    groups. *)

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
