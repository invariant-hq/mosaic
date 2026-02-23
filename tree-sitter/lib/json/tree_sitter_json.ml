external language_ptr : unit -> nativeint = "caml_tree_sitter_json_language"

let language () = Tree_sitter.Language.of_address (language_ptr ())

let highlights_query =
  {|
  (document (string) @string)
  (pair value: (string) @string)
  (array (string) @string)
  (number) @number
  (null) @constant
  (true) @constant
  (false) @constant
  (pair key: (string) @property)
  |}

let parser = lazy (Tree_sitter.Parser.create (language ()))

let query =
  lazy (Tree_sitter.Query.create (language ()) ~source:highlights_query)

let highlight content =
  let parser = Lazy.force parser in
  let query = Lazy.force query in
  let tree = Tree_sitter.Parser.parse_string parser content in
  Tree_sitter.highlight query tree
