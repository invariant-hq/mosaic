external ocaml_ptr : unit -> nativeint = "caml_tree_sitter_ocaml_language"

external interface_ptr : unit -> nativeint
  = "caml_tree_sitter_ocaml_interface_language"

external type_ptr : unit -> nativeint = "caml_tree_sitter_ocaml_type_language"

let ocaml () = Tree_sitter.Language.of_address (ocaml_ptr ())
let interface () = Tree_sitter.Language.of_address (interface_ptr ())
let type_ () = Tree_sitter.Language.of_address (type_ptr ())

let ocaml_highlights_query =
  {|
  (comment) @comment
  (string) @string
  (character) @string
  (constructor_name) @type
  (type_constructor) @type
  (module_name) @type
  ["let" "in" "match" "with" "function" "fun" "if" "then" "else"
   "type" "module" "open" "struct" "end" "sig" "val" "and" "rec" "of"
   "true" "false"] @keyword
  (value_name) @variable
  (number) @number
  |}

let interface_highlights_query = ocaml_highlights_query

let mk_highlighter lang_fn query_str =
  let parser = lazy (Tree_sitter.Parser.create (lang_fn ())) in
  let query = lazy (Tree_sitter.Query.create (lang_fn ()) ~source:query_str) in
  fun content ->
    let parser = Lazy.force parser in
    let query = Lazy.force query in
    let tree = Tree_sitter.Parser.parse_string parser content in
    Tree_sitter.highlight query tree

let highlight_ocaml = mk_highlighter ocaml ocaml_highlights_query
let highlight_interface = mk_highlighter interface interface_highlights_query
