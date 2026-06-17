(** Tree-sitter JSON grammar with built-in highlighting.

    This module provides the JSON language grammar and a convenience
    {!highlight} function that parses and highlights in one step.

    {[
    let ranges = Tree_sitter_json.highlight {|{"key": "value"}|}
    (* ranges : (int * int * string) list *)
    ]} *)

(** {1:language Language} *)

val language : unit -> Tree_sitter.Language.t
(** [language ()] is the Tree-sitter JSON language grammar. *)

(** {1:highlighting Highlighting} *)

val highlights_query : string
(** [highlights_query] is the highlight query source for JSON. Targets strings,
    numbers, constants ([true], [false], [null]), and property keys. *)

val highlight : string -> (int * int * string) list
(** [highlight content] parses [content] as JSON and returns
    [(start_byte, end_byte, capture_group)] triples. Equivalent to parsing with
    {!language} and running {!Tree_sitter.highlight} with {!highlights_query}.
*)
