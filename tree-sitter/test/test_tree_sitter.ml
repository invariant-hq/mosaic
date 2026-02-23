open Windtrap
open Tree_sitter

(* Helpers *)

let json_parser () = Parser.create (Tree_sitter_json.language ())
let ocaml_parser () = Parser.create (Tree_sitter_ocaml.ocaml ())

let parse_json src =
  let p = json_parser () in
  Parser.parse_string p src

let parse_ocaml src =
  let p = ocaml_parser () in
  Parser.parse_string p src

(* Language *)

let language_name () =
  let lang = Tree_sitter_json.language () in
  equal ~msg:"json name" string "json" (Language.name lang)

let language_version () =
  let lang = Tree_sitter_json.language () in
  is_true ~msg:"version >= 14" (Language.version lang >= 14)

let language_symbol_count () =
  let lang = Tree_sitter_json.language () in
  is_true ~msg:"has symbols" (Language.symbol_count lang > 0)

let language_field_count () =
  let lang = Tree_sitter_json.language () in
  is_true ~msg:"has fields" (Language.field_count lang >= 0)

let language_field_lookup () =
  let lang = Tree_sitter_ocaml.ocaml () in
  let count = Language.field_count lang in
  is_true ~msg:"ocaml has fields" (count > 0);
  match Language.field_name_for_id lang 1 with
  | Some name ->
      let found = Language.field_id_for_name lang name in
      equal ~msg:"field roundtrip" (option int) (Some 1) found
  | None -> ()

(* Parser *)

let parser_create () =
  let lang = Tree_sitter_json.language () in
  let p = Parser.create lang in
  let lang' = Parser.language p in
  equal ~msg:"parser language" string (Language.name lang) (Language.name lang')

let parser_set_language () =
  let p = json_parser () in
  let ocaml_lang = Tree_sitter_ocaml.ocaml () in
  Parser.set_language p ocaml_lang;
  equal ~msg:"switched to ocaml" string "ocaml"
    (Language.name (Parser.language p))

let parser_reset () =
  let p = json_parser () in
  Parser.reset p;
  let tree = Parser.parse_string p {|"hello"|} in
  is_true ~msg:"parse after reset" (Node.kind (Tree.root_node tree) = "document")

(* Parsing JSON *)

let parse_simple_json () =
  let tree = parse_json {|{"key": "value"}|} in
  let root = Tree.root_node tree in
  equal ~msg:"root kind" string "document" (Node.kind root);
  is_true ~msg:"root is named" (Node.is_named root);
  is_true ~msg:"no errors" (not (Node.has_error root))

let parse_json_array () =
  let tree = parse_json {|[1, 2, 3]|} in
  let root = Tree.root_node tree in
  equal ~msg:"root kind" string "document" (Node.kind root);
  is_true ~msg:"has children" (Node.child_count root > 0)

let parse_invalid_json () =
  let tree = parse_json {|{invalid}|} in
  let root = Tree.root_node tree in
  is_true ~msg:"has error" (Node.has_error root)

(* Node traversal *)

let node_byte_offsets () =
  let tree = parse_json {|"hello"|} in
  let root = Tree.root_node tree in
  equal ~msg:"root start" int 0 (Node.start_byte root);
  equal ~msg:"root end" int 7 (Node.end_byte root)

let node_points () =
  let tree = parse_json {|"hi"|} in
  let root = Tree.root_node tree in
  let sp = Node.start_point root in
  let ep = Node.end_point root in
  equal ~msg:"start row" int 0 sp.row;
  equal ~msg:"start col" int 0 sp.column;
  equal ~msg:"end row" int 0 ep.row;
  equal ~msg:"end col" int 4 ep.column

let node_children () =
  let tree = parse_json {|{"a": 1, "b": 2}|} in
  let root = Tree.root_node tree in
  match Node.named_child root 0 with
  | Some obj ->
      equal ~msg:"child kind" string "object" (Node.kind obj);
      is_true ~msg:"object has named children" (Node.named_child_count obj >= 2)
  | None -> fail "expected object child"

let node_child_by_field () =
  let tree = parse_json {|{"a": 1}|} in
  let root = Tree.root_node tree in
  match Node.named_child root 0 with
  | Some obj -> (
      match Node.named_child obj 0 with
      | Some pair -> (
          match Node.child_by_field_name pair "key" with
          | Some key -> equal ~msg:"key kind" string "string" (Node.kind key)
          | None -> fail "expected key field")
      | None -> fail "expected pair child")
  | None -> fail "expected object child"

let node_siblings () =
  let tree = parse_json {|[1, 2, 3]|} in
  let root = Tree.root_node tree in
  match Node.named_child root 0 with
  | Some arr -> (
      match Node.named_child arr 0 with
      | Some first -> (
          is_true ~msg:"first has next" (Node.next_named_sibling first <> None);
          match Node.next_named_sibling first with
          | Some second ->
              is_true ~msg:"second has prev"
                (Node.prev_named_sibling second <> None)
          | None -> fail "expected second element")
      | None -> fail "expected first element")
  | None -> fail "expected array"

let node_parent () =
  let tree = parse_json {|[1]|} in
  let root = Tree.root_node tree in
  match Node.named_child root 0 with
  | Some arr -> (
      match Node.named_child arr 0 with
      | Some num -> (
          match Node.parent num with
          | Some p -> equal ~msg:"parent is array" string "array" (Node.kind p)
          | None -> fail "expected parent")
      | None -> fail "expected number")
  | None -> fail "expected array"

let node_descendant () =
  let src = {|{"key": "val"}|} in
  let tree = parse_json src in
  let root = Tree.root_node tree in
  match Node.named_descendant_for_byte_range root ~start:1 ~end_:6 with
  | Some desc -> is_true ~msg:"found descendant" (Node.is_named desc)
  | None -> fail "expected descendant"

let node_equality () =
  let tree = parse_json {|[1]|} in
  let root = Tree.root_node tree in
  let root2 = Tree.root_node tree in
  is_true ~msg:"same root" (Node.equal root root2)

let node_to_sexp () =
  let tree = parse_json {|42|} in
  let root = Tree.root_node tree in
  let sexp = Node.to_sexp root in
  is_true ~msg:"sexp contains document" (String.length sexp > 0)

let node_flags () =
  let tree = parse_json {|42|} in
  let root = Tree.root_node tree in
  is_true ~msg:"not missing" (not (Node.is_missing root));
  is_true ~msg:"not extra" (not (Node.is_extra root));
  is_true ~msg:"not error" (not (Node.is_error root));
  is_true ~msg:"no changes" (not (Node.has_changes root))

(* Tree *)

let tree_copy () =
  let tree = parse_json {|[1]|} in
  let copy = Tree.copy tree in
  let r1 = Tree.root_node tree in
  let r2 = Tree.root_node copy in
  equal ~msg:"same structure" string (Node.kind r1) (Node.kind r2)

let tree_language () =
  let tree = parse_json {|null|} in
  let lang = Tree.language tree in
  equal ~msg:"tree language" string "json" (Language.name lang)

let tree_root_sexp () =
  let tree = parse_json {|null|} in
  let sexp = Tree.root_sexp tree in
  is_true ~msg:"sexp not empty" (String.length sexp > 0)

let tree_included_ranges () =
  let tree = parse_json {|null|} in
  let ranges = Tree.included_ranges tree in
  is_true ~msg:"has included range" (Array.length ranges > 0)

(* Incremental parsing *)

let incremental_parse () =
  let p = json_parser () in
  (* Start with {"a": 1} and change to {"a": "hello"} — structural change *)
  let src1 = {|{"a": 1}|} in
  let tree1 = Parser.parse_string p src1 in
  let old = Tree.copy tree1 in
  (* Replace byte range [6,7) "1" with "\"hello\"" (7 bytes) *)
  Tree.edit tree1 ~start_byte:6 ~old_end_byte:7 ~new_end_byte:13
    ~start_point:{ row = 0; column = 6 } ~old_end_point:{ row = 0; column = 7 }
    ~new_end_point:{ row = 0; column = 13 };
  let src2 = {|{"a": "hello"}|} in
  let tree2 = Parser.parse_string ~old:tree1 p src2 in
  let root = Tree.root_node tree2 in
  is_true ~msg:"no errors" (not (Node.has_error root));
  let changed = Tree.changed_ranges ~old tree2 in
  is_true ~msg:"has changed ranges" (Array.length changed > 0)

(* Query *)

let query_create () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @str|} in
  is_true ~msg:"has captures" (Query.capture_count q > 0);
  equal ~msg:"1 pattern" int 1 (Query.pattern_count q)

let query_capture_names () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @str (number) @num|} in
  equal ~msg:"str name" (option string) (Some "str")
    (Query.capture_name_for_id q 0);
  equal ~msg:"num name" (option string) (Some "num")
    (Query.capture_name_for_id q 1);
  equal ~msg:"str index" (option int) (Some 0)
    (Query.capture_index_for_name q "str");
  equal ~msg:"num index" (option int) (Some 1)
    (Query.capture_index_for_name q "num");
  equal ~msg:"missing" (option int) None (Query.capture_index_for_name q "nope")

let query_invalid () =
  let lang = Tree_sitter_json.language () in
  let raised = ref false in
  (try ignore (Query.create lang ~source:"(((invalid")
   with Failure _ -> raised := true);
  is_true ~msg:"invalid query raises" !raised

let query_disable_capture () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @str|} in
  Query.disable_capture q ~name:"str";
  let tree = parse_json {|"hello"|} in
  let cursor = Query_cursor.create () in
  Query_cursor.exec cursor q (Tree.root_node tree);
  let cap = Query_cursor.next_capture cursor q in
  equal ~msg:"no captures after disable" (option int) None
    (Option.map (fun c -> c.Query_cursor.capture_index) cap)

(* Query_cursor *)

let cursor_next_capture () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @str|} in
  let tree = parse_json {|{"a": "b", "c": "d"}|} in
  let cursor = Query_cursor.create () in
  Query_cursor.exec cursor q (Tree.root_node tree);
  let rec count n =
    match Query_cursor.next_capture cursor q with
    | Some _ -> count (n + 1)
    | None -> n
  in
  let n = count 0 in
  is_true ~msg:"captured strings" (n >= 4)

let cursor_next_match () =
  let lang = Tree_sitter_json.language () in
  let q =
    Query.create lang ~source:{|(pair key: (string) @key value: (_) @val)|}
  in
  let tree = parse_json {|{"x": 1, "y": 2}|} in
  let cursor = Query_cursor.create () in
  Query_cursor.exec cursor q (Tree.root_node tree);
  let rec count n =
    match Query_cursor.next_match cursor with
    | Some m ->
        is_true ~msg:"match has captures" (Array.length m.captures >= 2);
        count (n + 1)
    | None -> n
  in
  let n = count 0 in
  equal ~msg:"2 pairs matched" int 2 n

let cursor_byte_range () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(number) @num|} in
  (* [1, 2, 3] - numbers at offsets 1, 4, 7 *)
  let tree = parse_json {|[1, 2, 3]|} in
  let cursor = Query_cursor.create () in
  Query_cursor.set_byte_range cursor ~start:0 ~end_:3;
  Query_cursor.exec cursor q (Tree.root_node tree);
  let rec count n =
    match Query_cursor.next_capture cursor q with
    | Some _ -> count (n + 1)
    | None -> n
  in
  let n = count 0 in
  is_true ~msg:"range-limited captures" (n >= 1 && n <= 2)

(* Highlight *)

let highlight_json () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @string (number) @number|} in
  let tree = parse_json {|{"key": 42}|} in
  let ranges = highlight q tree in
  is_true ~msg:"has highlight ranges" (List.length ranges > 0);
  List.iter
    (fun (s, e, name) ->
      is_true ~msg:"valid range" (s < e);
      is_true ~msg:"has name" (String.length name > 0))
    ranges

let highlight_sorted () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @string (number) @number|} in
  let tree = parse_json {|{"a": 1, "b": 2}|} in
  let ranges = highlight q tree in
  let rec is_sorted = function
    | [] | [ _ ] -> true
    | (s1, _, _) :: ((s2, _, _) :: _ as rest) -> s1 <= s2 && is_sorted rest
  in
  is_true ~msg:"ranges sorted" (is_sorted ranges)

let highlight_skips_underscore () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(string) @_internal (number) @number|} in
  let tree = parse_json {|{"a": 42}|} in
  let ranges = highlight q tree in
  List.iter
    (fun (_, _, name) ->
      is_true ~msg:"no underscore captures" (name.[0] <> '_'))
    ranges

let highlight_range_restricted () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|(number) @number|} in
  let tree = parse_json {|[1, 2, 3]|} in
  let all = highlight q tree in
  let restricted = highlight_range q tree ~start_byte:0 ~end_byte:3 in
  is_true ~msg:"restricted <= all" (List.length restricted <= List.length all)

(* Language extensions *)

let language_symbol_for_name () =
  let lang = Tree_sitter_json.language () in
  let found = Language.symbol_for_name lang "string" ~named:true in
  is_true ~msg:"found string symbol" (found <> None);
  let not_found = Language.symbol_for_name lang "nonexistent_xyz" ~named:true in
  equal ~msg:"not found returns None" (option int) None not_found

let language_symbol_type () =
  let lang = Tree_sitter_json.language () in
  match Language.symbol_for_name lang "string" ~named:true with
  | Some id ->
      let typ = Language.symbol_type lang id in
      equal ~msg:"named symbol is Regular" int 0
        (match typ with
        | Regular -> 0
        | Anonymous -> 1
        | Supertype -> 2
        | Auxiliary -> 3)
  | None -> fail "expected to find string symbol"

(* Parser extensions *)

let parser_set_included_ranges () =
  let p = json_parser () in
  let ranges =
    [|
      {
        start_byte = 0;
        end_byte = 5;
        start_point = { row = 0; column = 0 };
        end_point = { row = 0; column = 5 };
      };
    |]
  in
  Parser.set_included_ranges p ranges;
  let tree = Parser.parse_string p {|"hi" extra|} in
  let root = Tree.root_node tree in
  is_true ~msg:"parsed with ranges" (Node.is_named root)

(* Query predicates *)

let query_predicates () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|((string) @str (#eq? @str "hello"))|} in
  let steps = Query.predicates_for_pattern q 0 in
  is_true ~msg:"has predicate steps" (Array.length steps > 0);
  let has_done = Array.exists (fun s -> s = Query.Done) steps in
  is_true ~msg:"has Done sentinel" has_done

let query_string_values () =
  let lang = Tree_sitter_json.language () in
  let q = Query.create lang ~source:{|((string) @str (#eq? @str "hello"))|} in
  (* String id 0 should be "eq?", string id 1 should be "hello" *)
  let s0 = Query.string_value_for_id q 0 in
  let s1 = Query.string_value_for_id q 1 in
  is_true ~msg:"string 0 exists" (s0 <> None);
  is_true ~msg:"string 1 exists" (s1 <> None);
  equal ~msg:"string 0 is eq?" (option string) (Some "eq?") s0;
  equal ~msg:"string 1 is hello" (option string) (Some "hello") s1

(* Tree_cursor *)

let tree_cursor_walk () =
  let tree = parse_json {|{"a": 1, "b": 2}|} in
  let root = Tree.root_node tree in
  let c = Tree_cursor.create root in
  equal ~msg:"starts at root" string "document"
    (Node.kind (Tree_cursor.current_node c));
  equal ~msg:"depth 0" int 0 (Tree_cursor.current_depth c);
  is_true ~msg:"has first child" (Tree_cursor.goto_first_child c);
  equal ~msg:"child is object" string "object"
    (Node.kind (Tree_cursor.current_node c));
  equal ~msg:"depth 1" int 1 (Tree_cursor.current_depth c);
  is_true ~msg:"has first grandchild" (Tree_cursor.goto_first_child c);
  is_true ~msg:"can go to parent" (Tree_cursor.goto_parent c);
  equal ~msg:"back to object" string "object"
    (Node.kind (Tree_cursor.current_node c));
  Tree_cursor.delete c

let tree_cursor_siblings () =
  let tree = parse_json {|[1, 2, 3]|} in
  let root = Tree.root_node tree in
  let c = Tree_cursor.create root in
  ignore (Tree_cursor.goto_first_child c);
  ignore (Tree_cursor.goto_first_child c);
  (* Now on "[" *)
  let rec count_siblings n =
    if Tree_cursor.goto_next_sibling c then count_siblings (n + 1) else n
  in
  let n = count_siblings 0 in
  is_true ~msg:"has siblings" (n > 0);
  Tree_cursor.delete c

let tree_cursor_field_name () =
  let tree = parse_json {|{"a": 1}|} in
  let root = Tree.root_node tree in
  let c = Tree_cursor.create root in
  ignore (Tree_cursor.goto_first_child c);
  (* object *)
  ignore (Tree_cursor.goto_first_child c);
  (* { *)
  ignore (Tree_cursor.goto_next_sibling c);
  (* pair *)
  ignore (Tree_cursor.goto_first_child c);
  (* key *)
  let field = Tree_cursor.current_field_name c in
  equal ~msg:"field is key" (option string) (Some "key") field;
  Tree_cursor.delete c

let tree_cursor_reset () =
  let tree = parse_json {|[1, 2]|} in
  let root = Tree.root_node tree in
  let c = Tree_cursor.create root in
  ignore (Tree_cursor.goto_first_child c);
  ignore (Tree_cursor.goto_first_child c);
  Tree_cursor.reset c root;
  equal ~msg:"reset to root" string "document"
    (Node.kind (Tree_cursor.current_node c));
  Tree_cursor.delete c

let tree_cursor_first_child_for_byte () =
  let tree = parse_json {|[1, 2, 3]|} in
  let root = Tree.root_node tree in
  let c = Tree_cursor.create root in
  ignore (Tree_cursor.goto_first_child c);
  (* array *)
  let idx = Tree_cursor.goto_first_child_for_byte c 4 in
  is_true ~msg:"found child for byte" (idx >= 0);
  Tree_cursor.delete c

(* OCaml grammar *)

let parse_ocaml_impl () =
  let tree = parse_ocaml "let x = 1" in
  let root = Tree.root_node tree in
  is_true ~msg:"ocaml root is named" (Node.is_named root);
  is_true ~msg:"no errors" (not (Node.has_error root))

let parse_ocaml_interface () =
  let p = Parser.create (Tree_sitter_ocaml.interface ()) in
  let tree = Parser.parse_string p "val x : int" in
  let root = Tree.root_node tree in
  is_true ~msg:"interface parses" (Node.is_named root);
  is_true ~msg:"no errors" (not (Node.has_error root))

let parse_ocaml_type () =
  let p = Parser.create (Tree_sitter_ocaml.type_ ()) in
  let tree = Parser.parse_string p "int -> string -> bool" in
  let root = Tree.root_node tree in
  is_true ~msg:"type parses" (Node.is_named root)

let ocaml_languages_distinct () =
  let a = Tree_sitter_ocaml.ocaml () in
  let b = Tree_sitter_ocaml.interface () in
  let c = Tree_sitter_ocaml.type_ () in
  equal ~msg:"ocaml name" string "ocaml" (Language.name a);
  equal ~msg:"interface name" string "ocaml_interface" (Language.name b);
  equal ~msg:"type name" string "ocaml_type" (Language.name c)

(* Test registration *)

let tests =
  [
    group "Language"
      [
        test "name" language_name;
        test "version" language_version;
        test "symbol_count" language_symbol_count;
        test "field_count" language_field_count;
        test "field lookup roundtrip" language_field_lookup;
        test "symbol_for_name" language_symbol_for_name;
        test "symbol_type" language_symbol_type;
      ];
    group "Parser"
      [
        test "create" parser_create;
        test "set_language" parser_set_language;
        test "reset" parser_reset;
        test "set_included_ranges" parser_set_included_ranges;
      ];
    group "Parsing"
      [
        test "simple object" parse_simple_json;
        test "array" parse_json_array;
        test "invalid json" parse_invalid_json;
      ];
    group "Node"
      [
        test "byte offsets" node_byte_offsets;
        test "points" node_points;
        test "children" node_children;
        test "child_by_field" node_child_by_field;
        test "siblings" node_siblings;
        test "parent" node_parent;
        test "descendant" node_descendant;
        test "equality" node_equality;
        test "to_sexp" node_to_sexp;
        test "flags" node_flags;
      ];
    group "Tree"
      [
        test "copy" tree_copy;
        test "language" tree_language;
        test "root_sexp" tree_root_sexp;
        test "included_ranges" tree_included_ranges;
        test "incremental parse" incremental_parse;
      ];
    group "Query"
      [
        test "create" query_create;
        test "capture names" query_capture_names;
        test "invalid source" query_invalid;
        test "disable capture" query_disable_capture;
        test "predicates" query_predicates;
        test "string values" query_string_values;
      ];
    group "Query_cursor"
      [
        test "next_capture" cursor_next_capture;
        test "next_match" cursor_next_match;
        test "byte range" cursor_byte_range;
      ];
    group "Highlight"
      [
        test "json highlights" highlight_json;
        test "sorted output" highlight_sorted;
        test "skips underscore" highlight_skips_underscore;
        test "range restricted" highlight_range_restricted;
      ];
    group "Tree_cursor"
      [
        test "walk" tree_cursor_walk;
        test "siblings" tree_cursor_siblings;
        test "field name" tree_cursor_field_name;
        test "reset" tree_cursor_reset;
        test "first child for byte" tree_cursor_first_child_for_byte;
      ];
    group "OCaml grammar"
      [
        test "parse implementation" parse_ocaml_impl;
        test "parse interface" parse_ocaml_interface;
        test "parse type" parse_ocaml_type;
        test "distinct languages" ocaml_languages_distinct;
      ];
  ]

let () = run "tree-sitter" tests
