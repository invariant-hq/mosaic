(** OCaml bindings for Tree-sitter.

    Tree-sitter is an incremental parsing library that builds concrete syntax
    trees for source code and efficiently updates them as the code changes.

    The main workflow is:

    + Obtain a {!Language.t} from a grammar package (e.g. {!Tree_sitter_json} or
      {!Tree_sitter_ocaml}).
    + Create a {!Parser.t} and call {!Parser.parse_string}.
    + Walk the resulting {!Tree.t} via {!Node} accessors or {!Tree_cursor}.
    + Optionally, compile a {!Query.t} and run it with a {!Query_cursor} for
      pattern matching. *)

(** {1:types Types} *)

type point = { row : int; column : int }
(** A position in source text. [row] and [column] are both zero-indexed.
    [column] is measured in bytes, not characters. *)

type range = {
  start_byte : int;
  end_byte : int;
  start_point : point;
  end_point : point;
}
(** A contiguous range in source text, expressed as both byte offsets and
    row-column {!point}s. *)

(** The type of a grammar symbol. *)
type symbol_type =
  | Regular  (** Named grammar rules (e.g. [string], [object]). *)
  | Anonymous  (** Literal tokens (e.g. [{], [,]). *)
  | Supertype  (** Abstract grouping rules. *)
  | Auxiliary  (** Compiler-internal symbols. *)

(** {1:language Language grammars} *)

(** Language grammars.

    A {!Language.t} describes the grammar rules, symbols, and fields for a
    particular programming language. Obtain one from a grammar package (e.g.
    [Tree_sitter_json.language ()]) rather than calling {!of_address} directly.
*)
module Language : sig
  type t
  (** The type for Tree-sitter language grammars. *)

  (** {2:constructors Constructors} *)

  val of_address : nativeint -> t
  (** [of_address addr] is a language from a native pointer [addr].

      {b Warning.} [addr] must point to a valid [TSLanguage] returned by a
      [tree_sitter_<lang>()] C function. Prefer grammar-package helpers (e.g.
      [Tree_sitter_json.language ()]) instead. *)

  (** {2:properties Properties} *)

  val name : t -> string
  (** [name lang] is the language name (e.g. ["json"], ["ocaml"]). *)

  val version : t -> int
  (** [version lang] is the ABI version of the grammar. *)

  val symbol_count : t -> int
  (** [symbol_count lang] is the total number of symbols in the grammar. *)

  val field_count : t -> int
  (** [field_count lang] is the number of named fields in the grammar. *)

  (** {2:symbols Symbol lookups} *)

  val symbol_name : t -> int -> string
  (** [symbol_name lang id] is the name of symbol [id]. *)

  val symbol_type : t -> int -> symbol_type
  (** [symbol_type lang id] is the {!type:symbol_type} of symbol [id]. *)

  val symbol_for_name : t -> string -> named:bool -> int option
  (** [symbol_for_name lang name ~named] is the symbol ID for [name], if any.
      When [~named] is [true], only named (non-anonymous) symbols are searched.
  *)

  (** {2:fields Field lookups} *)

  val field_name_for_id : t -> int -> string option
  (** [field_name_for_id lang id] is the field name for [id], if any. *)

  val field_id_for_name : t -> string -> int option
  (** [field_id_for_name lang name] is the field ID for [name], if any. *)
end

(** {1:node Syntax tree nodes} *)

(** Syntax tree nodes.

    A node represents a single construct in the syntax tree: a keyword, an
    expression, a statement, etc. Nodes expose their grammar {!kind}, source
    {!start_byte}/{!end_byte} span, and structural relationships (children,
    siblings, parent).

    Nodes hold an internal reference to their parent {!Tree.t} to prevent
    premature garbage collection. *)
module Node : sig
  type t
  (** The type for syntax tree nodes. *)

  (** {2:properties Properties} *)

  val kind : t -> string
  (** [kind node] is the grammar rule name (e.g. ["string"], ["object"]). *)

  val symbol : t -> int
  (** [symbol node] is the grammar symbol ID of [node]. *)

  val is_named : t -> bool
  (** [is_named node] is [true] iff [node] corresponds to a named grammar rule
      (as opposed to an anonymous literal token). *)

  val is_missing : t -> bool
  (** [is_missing node] is [true] iff [node] was inserted by the parser to
      recover from an error (it has no corresponding source text). *)

  val is_extra : t -> bool
  (** [is_extra node] is [true] iff [node] represents something not required by
      the grammar, such as a comment. *)

  val is_error : t -> bool
  (** [is_error node] is [true] iff [node] is an error node (a region the parser
      could not assign a grammar rule to). *)

  val has_error : t -> bool
  (** [has_error node] is [true] iff [node] or any of its descendants is an
      error node. *)

  val has_changes : t -> bool
  (** [has_changes node] is [true] iff [node] has been marked as changed by
      {!Tree.edit}. Only meaningful between an {!Tree.edit} call and the
      subsequent reparse. *)

  (** {2:position Position} *)

  val start_byte : t -> int
  (** [start_byte node] is the byte offset of the start of [node]. *)

  val end_byte : t -> int
  (** [end_byte node] is the byte offset of the end of [node] (exclusive). *)

  val start_point : t -> point
  (** [start_point node] is the row-column position of the start of [node]. *)

  val end_point : t -> point
  (** [end_point node] is the row-column position of the end of [node]. *)

  (** {2:children Children} *)

  val child_count : t -> int
  (** [child_count node] is the total number of children (named and anonymous).
  *)

  val named_child_count : t -> int
  (** [named_child_count node] is the number of named children. *)

  val child : t -> int -> t option
  (** [child node i] is the [i]-th child (zero-indexed, including anonymous
      nodes), if any. *)

  val named_child : t -> int -> t option
  (** [named_child node i] is the [i]-th named child (zero-indexed), if any. *)

  val child_by_field_name : t -> string -> t option
  (** [child_by_field_name node name] is the child assigned to field [name] in
      the grammar, if any. *)

  (** {2:navigation Navigation} *)

  val parent : t -> t option
  (** [parent node] is the parent of [node], if any. *)

  val next_sibling : t -> t option
  (** [next_sibling node] is the next sibling (named or anonymous), if any. *)

  val prev_sibling : t -> t option
  (** [prev_sibling node] is the previous sibling (named or anonymous), if any.
  *)

  val next_named_sibling : t -> t option
  (** [next_named_sibling node] is the next named sibling, if any. *)

  val prev_named_sibling : t -> t option
  (** [prev_named_sibling node] is the previous named sibling, if any. *)

  (** {2:descendants Descendants} *)

  val descendant_for_byte_range : t -> start:int -> end_:int -> t option
  (** [descendant_for_byte_range node ~start ~end_] is the smallest descendant
      (named or anonymous) that spans the byte range \[[start];[end_]\], if any.
  *)

  val descendant_for_point_range : t -> start:point -> end_:point -> t option
  (** [descendant_for_point_range node ~start ~end_] is like
      {!descendant_for_byte_range} but addresses by {!point}. *)

  val named_descendant_for_byte_range : t -> start:int -> end_:int -> t option
  (** [named_descendant_for_byte_range node ~start ~end_] is like
      {!descendant_for_byte_range} but only considers named nodes. *)

  val named_descendant_for_point_range :
    t -> start:point -> end_:point -> t option
  (** [named_descendant_for_point_range node ~start ~end_] is like
      {!descendant_for_point_range} but only considers named nodes. *)

  (** {2:serialization Serialization and comparison} *)

  val to_sexp : t -> string
  (** [to_sexp node] is an S-expression representation of [node]'s subtree.
      Useful for debugging. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] refer to the same node in the same
      tree. *)
end

(** {1:tree Parsed syntax trees} *)

(** Parsed syntax trees.

    A {!Tree.t} is the result of parsing. It is immutable once created. To
    perform incremental reparsing, call {!edit} to describe the text change,
    then pass the edited tree as [~old] to {!Parser.parse_string}. *)
module Tree : sig
  type t
  (** The type for parsed syntax trees. Automatically garbage collected. *)

  (** {2:access Accessors} *)

  val root_node : t -> Node.t
  (** [root_node tree] is the root node of [tree]. *)

  val root_sexp : t -> string
  (** [root_sexp tree] is an S-expression representation of the entire tree.
      Useful for debugging. *)

  val language : t -> Language.t
  (** [language tree] is the language that was used to parse [tree]. *)

  val copy : t -> t
  (** [copy tree] is a deep copy of [tree]. *)

  (** {2:incremental Incremental editing} *)

  val edit :
    t ->
    start_byte:int ->
    old_end_byte:int ->
    new_end_byte:int ->
    start_point:point ->
    old_end_point:point ->
    new_end_point:point ->
    unit
  (** [edit tree ~start_byte ~old_end_byte ~new_end_byte ~start_point
       ~old_end_point ~new_end_point] marks a region as edited. Call this
      {e before} reparsing with {!Parser.parse_string} [~old:tree] so the parser
      can reuse unaffected subtrees. *)

  val changed_ranges : old:t -> t -> range array
  (** [changed_ranges ~old new_tree] is the array of ranges that differ between
      [old] and [new_tree]. Both trees must have been produced by the same
      parser, with [old] passed as [~old] to the second {!Parser.parse_string}
      call. *)

  val included_ranges : t -> range array
  (** [included_ranges tree] is the array of source ranges that the parser was
      configured to include (see {!Parser.set_included_ranges}). *)
end

(** {1:parser Parsers} *)

(** Parsers.

    A parser drives the Tree-sitter parsing engine. Create one with {!create},
    then call {!parse_string} to obtain a {!Tree.t}. For incremental reparsing,
    pass the previous tree as [~old]. *)
module Parser : sig
  type t
  (** The type for parsers. Automatically garbage collected. *)

  val create : Language.t -> t
  (** [create lang] is a new parser configured for [lang]. *)

  val language : t -> Language.t
  (** [language parser] is the language the parser is configured for. *)

  val set_language : t -> Language.t -> unit
  (** [set_language parser lang] reconfigures [parser] for [lang]. *)

  val parse_string : ?old:Tree.t -> t -> string -> Tree.t
  (** [parse_string ?old parser source] parses [source] and returns the syntax
      tree. When [~old] is provided, the parser performs an incremental reparse,
      reusing unchanged subtrees from the previous tree (see {!Tree.edit}). *)

  val reset : t -> unit
  (** [reset parser] clears the parser's internal state. Call this when
      switching to a completely unrelated document. *)

  val set_included_ranges : t -> range array -> unit
  (** [set_included_ranges parser ranges] restricts parsing to the given source
      ranges. Useful for parsing embedded languages (e.g. JavaScript inside
      HTML).

      Raises [Failure] if the ranges overlap or are not in ascending order. *)
end

(** {1:query Pattern queries} *)

(** Compiled queries for pattern matching on syntax trees.

    A query is compiled from Tree-sitter's
    {{:https://tree-sitter.github.io/tree-sitter/syntax-highlighting/using-queries}
     S-expression query language}. It defines patterns with named captures.
    Execute queries with a {!Query_cursor}. *)
module Query : sig
  type t
  (** The type for compiled queries. Automatically garbage collected. *)

  (** {2:constructors Constructors} *)

  val create : Language.t -> source:string -> t
  (** [create lang ~source] compiles the query [source] for [lang].

      Raises [Failure] on syntax errors or invalid node types. The error message
      includes the byte offset of the problem. *)

  (** {2:properties Properties} *)

  val capture_count : t -> int
  (** [capture_count query] is the number of captures in [query]. *)

  val pattern_count : t -> int
  (** [pattern_count query] is the number of patterns in [query]. *)

  (** {2:captures Capture lookups} *)

  val capture_name_for_id : t -> int -> string option
  (** [capture_name_for_id query id] is the name of capture [id], if any. *)

  val capture_index_for_name : t -> string -> int option
  (** [capture_index_for_name query name] is the index of the capture named
      [name], if any. *)

  val string_value_for_id : t -> int -> string option
  (** [string_value_for_id query id] is the string literal at index [id] in the
      query's string table, if any. Used to resolve {!String} predicate steps.
  *)

  (** {2:disable Disabling captures and patterns} *)

  val disable_capture : t -> name:string -> unit
  (** [disable_capture query ~name] prevents captures named [name] from
      appearing in future query results. *)

  val disable_pattern : t -> pattern:int -> unit
  (** [disable_pattern query ~pattern] disables pattern [pattern] so it no
      longer produces matches. *)

  (** {2:predicates Predicates} *)

  (** A step in a query predicate. Predicates are stored as flat arrays of
      steps, separated by {!Done} sentinels. *)
  type predicate_step =
    | Done  (** End of a predicate. *)
    | Capture of int  (** A capture reference, by index. *)
    | String of int
        (** A string literal reference, by index (resolve with
            {!string_value_for_id}). *)

  val predicates_for_pattern : t -> int -> predicate_step array
  (** [predicates_for_pattern query pattern_index] is the predicate steps for
      pattern [pattern_index]. *)
end

(** {1:query_cursor Query cursors} *)

(** Query cursors for iterating over query results.

    A cursor manages the state of a running query. Create one with {!create},
    bind it to a node with {!exec}, then iterate with {!next_match} or
    {!next_capture}. Cursors are reusable: call {!exec} again to run a new
    query. *)
module Query_cursor : sig
  type t
  (** The type for query cursors. Automatically garbage collected. *)

  type capture = {
    capture_index : int;  (** Index into {!Query}'s capture list. *)
    pattern_index : int;  (** Index of the matching pattern. *)
    node : Node.t;  (** The captured node. *)
  }
  (** The type for a single capture result. *)

  type match_result = {
    pattern_index : int;  (** Index of the matching pattern. *)
    captures : (int * Node.t) array;
        (** Array of [(capture_index, node)] pairs. *)
  }
  (** The type for a complete match result. *)

  val create : unit -> t
  (** [create ()] is a new query cursor. *)

  val exec : t -> Query.t -> Node.t -> unit
  (** [exec cursor query node] executes [query] on [node]'s subtree. Subsequent
      {!next_match} or {!next_capture} calls iterate over the results. *)

  val set_byte_range : t -> start:int -> end_:int -> unit
  (** [set_byte_range cursor ~start ~end_] restricts subsequent matches to nodes
      within the byte range \[[start];[end_]\]. Call before {!exec}. *)

  val set_point_range : t -> start:point -> end_:point -> unit
  (** [set_point_range cursor ~start ~end_] restricts subsequent matches to
      nodes within the point range. Call before {!exec}. *)

  val next_match : t -> match_result option
  (** [next_match cursor] is the next full match, if any. *)

  val next_capture : t -> Query.t -> capture option
  (** [next_capture cursor query] is the next individual capture, if any.
      Captures are yielded one at a time, even when a pattern has multiple
      captures per match. *)
end

(** {1:tree_cursor Tree traversal cursors} *)

(** Efficient tree traversal cursors.

    A {!Tree_cursor.t} walks a syntax tree without allocating a new {!Node.t} at
    each step. Prefer this over {!Node} navigation functions (e.g.
    {!Node.child}, {!Node.next_sibling}) when visiting many nodes. *)
module Tree_cursor : sig
  type t
  (** The type for tree cursors. Automatically garbage collected, but {!delete}
      can release resources sooner. *)

  (** {2:constructors Constructors} *)

  val create : Node.t -> t
  (** [create node] is a cursor starting at [node]. The cursor cannot walk above
      [node]. *)

  val delete : t -> unit
  (** [delete cursor] frees resources immediately. The cursor must not be used
      after this call. *)

  val reset : t -> Node.t -> unit
  (** [reset cursor node] repositions [cursor] to start at [node]. *)

  (** {2:current Current position} *)

  val current_node : t -> Node.t
  (** [current_node cursor] is the node the cursor is on. *)

  val current_field_name : t -> string option
  (** [current_field_name cursor] is the field name of the current node in its
      parent, if any. *)

  val current_field_id : t -> int
  (** [current_field_id cursor] is the field ID of the current node, or [0] if
      it has no field. *)

  val current_depth : t -> int
  (** [current_depth cursor] is the depth of the current node relative to the
      cursor's starting node. *)

  (** {2:movement Movement} *)

  val goto_parent : t -> bool
  (** [goto_parent cursor] moves to the parent. Returns [false] if already at
      the starting node. *)

  val goto_first_child : t -> bool
  (** [goto_first_child cursor] moves to the first child. Returns [false] if the
      current node has no children. *)

  val goto_last_child : t -> bool
  (** [goto_last_child cursor] moves to the last child. Returns [false] if the
      current node has no children. *)

  val goto_next_sibling : t -> bool
  (** [goto_next_sibling cursor] moves to the next sibling. Returns [false] if
      there is no next sibling. *)

  val goto_previous_sibling : t -> bool
  (** [goto_previous_sibling cursor] moves to the previous sibling. Returns
      [false] if there is no previous sibling. *)

  val goto_first_child_for_byte : t -> int -> int
  (** [goto_first_child_for_byte cursor byte] moves to the first child that
      contains or starts after [byte]. Returns the child index, or [-1] if no
      such child exists. *)
end

(** {1:highlighting Highlighting} *)

val highlight : Query.t -> Tree.t -> (int * int * string) list
(** [highlight query tree] is the list of [(start_byte, end_byte, group_name)]
    triples for all captures in [tree]. Captures whose name starts with [_] are
    filtered out. Empty ranges (where [start_byte >= end_byte]) are also
    dropped.

    The result is sorted by ascending [start_byte]. *)

val highlight_range :
  Query.t ->
  Tree.t ->
  start_byte:int ->
  end_byte:int ->
  (int * int * string) list
(** [highlight_range query tree ~start_byte ~end_byte] is like {!highlight} but
    restricted to nodes within the byte range \[[start_byte];[end_byte]\]. *)
