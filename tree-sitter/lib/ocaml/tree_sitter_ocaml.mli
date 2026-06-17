(** Tree-sitter OCaml grammars with built-in highlighting.

    This module provides grammars for three OCaml dialects — implementation
    files ([.ml]), interface files ([.mli]), and standalone type expressions —
    together with convenience highlighting functions.

    {[
    let ranges = Tree_sitter_ocaml.highlight_ocaml "let x = 1"
    (* ranges : (int * int * string) list *)
    ]} *)

(** {1:languages Languages} *)

val ocaml : unit -> Tree_sitter.Language.t
(** [ocaml ()] is the grammar for OCaml implementation files ([.ml]). *)

val interface : unit -> Tree_sitter.Language.t
(** [interface ()] is the grammar for OCaml interface files ([.mli]). *)

val type_ : unit -> Tree_sitter.Language.t
(** [type_ ()] is the grammar for standalone OCaml type expressions. *)

(** {1:highlighting Highlighting} *)

val ocaml_highlights_query : string
(** [ocaml_highlights_query] is the highlight query source for OCaml
    implementation files. Targets comments, strings, types, keywords, variables,
    and numbers. *)

val interface_highlights_query : string
(** [interface_highlights_query] is the highlight query source for OCaml
    interface files. Currently identical to {!ocaml_highlights_query}. *)

val highlight_ocaml : string -> (int * int * string) list
(** [highlight_ocaml content] parses [content] as OCaml and returns
    [(start_byte, end_byte, capture_group)] triples. Equivalent to parsing with
    {!ocaml} and running {!Tree_sitter.highlight} with
    {!ocaml_highlights_query}. *)

val highlight_interface : string -> (int * int * string) list
(** [highlight_interface content] parses [content] as an OCaml interface and
    returns [(start_byte, end_byte, capture_group)] triples. Equivalent to
    parsing with {!interface} and running {!Tree_sitter.highlight} with
    {!interface_highlights_query}. *)
