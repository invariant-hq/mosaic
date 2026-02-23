# tree-sitter

OCaml bindings for [Tree-sitter](https://tree-sitter.github.io/tree-sitter/),
an incremental parsing library that builds concrete syntax trees for source code
and efficiently updates them as the code changes.

## Quick start

Parse a JSON string and inspect the root node:

```ocaml
open Tree_sitter

let () =
  let lang = Tree_sitter_json.language () in
  let parser = Parser.create lang in
  let tree = Parser.parse_string parser {|{"key": "value"}|} in
  let root = Tree.root_node tree in
  assert (Node.kind root = "document");
  assert (not (Node.has_error root));
  Printf.printf "%s\n" (Node.to_sexp root)
```

## Highlighting

Compile a query with `@name` captures and run it over the tree:

```ocaml
open Tree_sitter

let () =
  let lang = Tree_sitter_json.language () in
  let parser = Parser.create lang in
  let query =
    Query.create lang ~source:{|(string) @string (number) @number|}
  in
  let tree = Parser.parse_string parser {|{"a": 42}|} in
  let ranges = highlight query tree in
  List.iter
    (fun (s, e, name) -> Printf.printf "%d-%d %s\n" s e name)
    ranges
```

Or use the one-shot helpers from a grammar package:

```ocaml
let ranges = Tree_sitter_json.highlight {|{"a": 42}|}
```

## Incremental reparsing

After a text edit, describe the change and reparse. The parser reuses unchanged
subtrees:

```ocaml
open Tree_sitter

let () =
  let lang = Tree_sitter_json.language () in
  let parser = Parser.create lang in
  let tree = Parser.parse_string parser {|{"a": 1}|} in
  let old = Tree.copy tree in
  (* Replace "1" (byte 6) with "\"hello\"" (7 bytes) *)
  Tree.edit tree
    ~start_byte:6 ~old_end_byte:7 ~new_end_byte:13
    ~start_point:{ row = 0; column = 6 }
    ~old_end_point:{ row = 0; column = 7 }
    ~new_end_point:{ row = 0; column = 13 };
  let tree2 =
    Parser.parse_string ~old:tree parser {|{"a": "hello"}|}
  in
  let changed = Tree.changed_ranges ~old tree2 in
  Printf.printf "%d changed range(s)\n" (Array.length changed)
```

## Tree cursors

Use `Tree_cursor` for efficient bulk traversal without per-node allocation:

```ocaml
open Tree_sitter

let () =
  let lang = Tree_sitter_json.language () in
  let parser = Parser.create lang in
  let tree = Parser.parse_string parser {|[1, 2, 3]|} in
  let cursor = Tree_cursor.create (Tree.root_node tree) in
  ignore (Tree_cursor.goto_first_child cursor);
  ignore (Tree_cursor.goto_first_child cursor);
  let rec walk () =
    let node = Tree_cursor.current_node cursor in
    if Node.is_named node then
      Printf.printf "%s (%d-%d)\n"
        (Node.kind node)
        (Node.start_byte node)
        (Node.end_byte node);
    if Tree_cursor.goto_next_sibling cursor then walk ()
  in
  walk ();
  Tree_cursor.delete cursor
```

## Libraries

| Library             | opam package        | Description                                           |
| ------------------- | ------------------- | ----------------------------------------------------- |
| `tree_sitter`       | `tree-sitter`       | Core bindings: Parser, Tree, Node, Query, Tree_cursor |
| `tree_sitter_json`  | `tree-sitter.json`  | JSON grammar and highlighting                         |
| `tree_sitter_ocaml` | `tree-sitter.ocaml` | OCaml grammar (.ml, .mli, type) and highlighting      |

## License

This library is licensed under the ISC license. See [LICENSE](LICENSE) for details.
