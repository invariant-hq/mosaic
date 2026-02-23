type point = { row : int; column : int }

type range = {
  start_byte : int;
  end_byte : int;
  start_point : point;
  end_point : point;
}

(* Tree handle type, defined early so Node can reference it. *)
type tree_handle
type symbol_type = Regular | Anonymous | Supertype | Auxiliary

module Language = struct
  type t

  external of_address : nativeint -> t = "caml_ts_language_of_address"
  external name : t -> string = "caml_ts_language_name"
  external version : t -> int = "caml_ts_language_version"
  external symbol_count : t -> int = "caml_ts_language_symbol_count"
  external field_count : t -> int = "caml_ts_language_field_count"
  external symbol_name : t -> int -> string = "caml_ts_language_symbol_name"

  external field_name_for_id : t -> int -> string option
    = "caml_ts_language_field_name_for_id"

  external field_id_for_name : t -> string -> int option
    = "caml_ts_language_field_id_for_name"

  external symbol_for_name_raw : t -> string -> bool -> int option
    = "caml_ts_language_symbol_for_name"

  let symbol_for_name t name ~named = symbol_for_name_raw t name named

  external symbol_type_raw : t -> int -> int = "caml_ts_language_symbol_type"

  let symbol_type t id =
    match symbol_type_raw t id with
    | 1 -> Anonymous
    | 2 -> Supertype
    | 3 -> Auxiliary
    | _ -> Regular
end

module Node = struct
  type node_handle
  type t = { node : node_handle; tree : tree_handle }

  external kind_handle : node_handle -> string = "caml_ts_node_type"
  external is_named_handle : node_handle -> bool = "caml_ts_node_is_named"
  external is_missing_handle : node_handle -> bool = "caml_ts_node_is_missing"
  external is_extra_handle : node_handle -> bool = "caml_ts_node_is_extra"
  external is_error_handle : node_handle -> bool = "caml_ts_node_is_error"
  external has_error_handle : node_handle -> bool = "caml_ts_node_has_error"
  external has_changes_handle : node_handle -> bool = "caml_ts_node_has_changes"
  external symbol_handle : node_handle -> int = "caml_ts_node_symbol"
  external start_byte_handle : node_handle -> int = "caml_ts_node_start_byte"
  external end_byte_handle : node_handle -> int = "caml_ts_node_end_byte"

  external start_point_handle : node_handle -> point
    = "caml_ts_node_start_point"

  external end_point_handle : node_handle -> point = "caml_ts_node_end_point"
  external child_count_handle : node_handle -> int = "caml_ts_node_child_count"

  external named_child_count_handle : node_handle -> int
    = "caml_ts_node_named_child_count"

  external child_handle : node_handle -> int -> node_handle option
    = "caml_ts_node_child"

  external named_child_handle : node_handle -> int -> node_handle option
    = "caml_ts_node_named_child"

  external child_by_field_name_handle :
    node_handle -> string -> node_handle option
    = "caml_ts_node_child_by_field_name"

  external parent_handle : node_handle -> node_handle option
    = "caml_ts_node_parent"

  external next_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_next_sibling"

  external prev_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_prev_sibling"

  external next_named_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_next_named_sibling"

  external prev_named_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_prev_named_sibling"

  external descendant_for_byte_range_handle :
    node_handle -> int -> int -> node_handle option
    = "caml_ts_node_descendant_for_byte_range"

  external descendant_for_point_range_handle :
    node_handle -> point -> point -> node_handle option
    = "caml_ts_node_descendant_for_point_range"

  external named_descendant_for_byte_range_handle :
    node_handle -> int -> int -> node_handle option
    = "caml_ts_node_named_descendant_for_byte_range"

  external named_descendant_for_point_range_handle :
    node_handle -> point -> point -> node_handle option
    = "caml_ts_node_named_descendant_for_point_range"

  external to_sexp_handle : node_handle -> string = "caml_ts_node_to_sexp"
  external equal_handle : node_handle -> node_handle -> bool = "caml_ts_node_eq"

  let wrap tree = function None -> None | Some node -> Some { node; tree }
  let kind t = kind_handle t.node
  let is_named t = is_named_handle t.node
  let is_missing t = is_missing_handle t.node
  let is_extra t = is_extra_handle t.node
  let is_error t = is_error_handle t.node
  let has_error t = has_error_handle t.node
  let has_changes t = has_changes_handle t.node
  let symbol t = symbol_handle t.node
  let start_byte t = start_byte_handle t.node
  let end_byte t = end_byte_handle t.node
  let start_point t = start_point_handle t.node
  let end_point t = end_point_handle t.node
  let child_count t = child_count_handle t.node
  let named_child_count t = named_child_count_handle t.node
  let child t idx = wrap t.tree (child_handle t.node idx)
  let named_child t idx = wrap t.tree (named_child_handle t.node idx)

  let child_by_field_name t name =
    wrap t.tree (child_by_field_name_handle t.node name)

  let parent t = wrap t.tree (parent_handle t.node)
  let next_sibling t = wrap t.tree (next_sibling_handle t.node)
  let prev_sibling t = wrap t.tree (prev_sibling_handle t.node)
  let next_named_sibling t = wrap t.tree (next_named_sibling_handle t.node)
  let prev_named_sibling t = wrap t.tree (prev_named_sibling_handle t.node)

  let descendant_for_byte_range t ~start ~end_ =
    wrap t.tree (descendant_for_byte_range_handle t.node start end_)

  let descendant_for_point_range t ~start ~end_ =
    wrap t.tree (descendant_for_point_range_handle t.node start end_)

  let named_descendant_for_byte_range t ~start ~end_ =
    wrap t.tree (named_descendant_for_byte_range_handle t.node start end_)

  let named_descendant_for_point_range t ~start ~end_ =
    wrap t.tree (named_descendant_for_point_range_handle t.node start end_)

  let to_sexp t = to_sexp_handle t.node
  let equal a b = equal_handle a.node b.node
end

module Tree = struct
  type t = tree_handle

  external root_node_handle : t -> Node.node_handle = "caml_ts_tree_root_node"
  external root_sexp : t -> string = "caml_ts_tree_root_sexp"
  external language : t -> Language.t = "caml_ts_tree_language"
  external copy : t -> t = "caml_ts_tree_copy"

  external edit :
    t ->
    start_byte:int ->
    old_end_byte:int ->
    new_end_byte:int ->
    start_point:point ->
    old_end_point:point ->
    new_end_point:point ->
    unit = "caml_ts_tree_edit_bytecode" "caml_ts_tree_edit_native"

  external changed_ranges_native : t -> t -> range array
    = "caml_ts_tree_get_changed_ranges"

  external included_ranges : t -> range array = "caml_ts_tree_included_ranges"

  let root_node tree : Node.t =
    let node = root_node_handle tree in
    { Node.node; tree }

  let changed_ranges ~old new_tree = changed_ranges_native old new_tree
end

module Parser = struct
  type t

  external create_with_language : Language.t -> t
    = "caml_ts_parser_create_with_language"

  external language : t -> Language.t = "caml_ts_parser_language"

  external set_language : t -> Language.t -> unit
    = "caml_ts_parser_set_language"

  external parse_string_simple : t -> string -> Tree.t
    = "caml_ts_parser_parse_string"

  external parse_string_with_old : t -> Tree.t option -> string -> Tree.t
    = "caml_ts_parser_parse_string_old"

  external reset : t -> unit = "caml_ts_parser_reset"

  external set_included_ranges : t -> range array -> unit
    = "caml_ts_parser_set_included_ranges"

  let create = create_with_language

  let parse_string ?old parser source =
    match old with
    | None -> parse_string_simple parser source
    | Some _ -> parse_string_with_old parser old source
end

module Query = struct
  type t

  external create_native : Language.t -> string -> t = "caml_ts_query_new"
  external capture_count : t -> int = "caml_ts_query_capture_count"
  external pattern_count : t -> int = "caml_ts_query_pattern_count"

  external capture_name_for_id : t -> int -> string option
    = "caml_ts_query_capture_name_for_id"

  external capture_index_for_name : t -> string -> int option
    = "caml_ts_query_capture_index_for_name"

  external disable_capture_native : t -> string -> unit
    = "caml_ts_query_disable_capture"

  external disable_pattern_native : t -> int -> unit
    = "caml_ts_query_disable_pattern"

  external string_value_for_id : t -> int -> string option
    = "caml_ts_query_string_value_for_id"

  external predicates_raw : t -> int -> (int * int) array
    = "caml_ts_query_predicates_for_pattern"

  let create language ~source = create_native language source
  let disable_capture t ~name = disable_capture_native t name
  let disable_pattern t ~pattern = disable_pattern_native t pattern

  type predicate_step = Done | Capture of int | String of int

  let predicates_for_pattern t pattern =
    Array.map
      (fun (typ, value_id) ->
        match typ with
        | 1 -> Capture value_id
        | 2 -> String value_id
        | _ -> Done)
      (predicates_raw t pattern)
end

module Query_cursor = struct
  type cursor_handle
  type t = { cursor : cursor_handle; mutable tree : Tree.t option }
  type capture = { capture_index : int; pattern_index : int; node : Node.t }
  type match_result = { pattern_index : int; captures : (int * Node.t) array }

  external create_handle : unit -> cursor_handle = "caml_ts_query_cursor_new"

  external exec_handle : cursor_handle -> Query.t -> Node.node_handle -> unit
    = "caml_ts_query_cursor_exec"

  external set_byte_range_native : cursor_handle -> int -> int -> unit
    = "caml_ts_query_cursor_set_byte_range"

  external set_point_range_native : cursor_handle -> point -> point -> unit
    = "caml_ts_query_cursor_set_point_range"

  external next_match_raw :
    cursor_handle -> (int * (int * Node.node_handle) array) option
    = "caml_ts_query_cursor_next_match"

  external next_capture_raw :
    cursor_handle -> Query.t -> (int * int * Node.node_handle) option
    = "caml_ts_query_cursor_next_capture"

  let create () = { cursor = create_handle (); tree = None }

  let exec t query (node : Node.t) =
    t.tree <- Some node.tree;
    exec_handle t.cursor query node.node

  let set_byte_range t ~start ~end_ = set_byte_range_native t.cursor start end_

  let set_point_range t ~start ~end_ =
    set_point_range_native t.cursor start end_

  let next_match t =
    match (next_match_raw t.cursor, t.tree) with
    | None, _ -> None
    | Some _, None ->
        failwith "Query_cursor.next_match: cursor not initialized with exec"
    | Some (pattern_index, captures), Some tree ->
        let captures =
          Array.map
            (fun (idx, node_handle) ->
              (idx, ({ Node.node = node_handle; tree } : Node.t)))
            captures
        in
        Some { pattern_index; captures }

  let next_capture t query =
    match (next_capture_raw t.cursor query, t.tree) with
    | None, _ -> None
    | Some _, None ->
        failwith "Query_cursor.next_capture: cursor not initialized with exec"
    | Some (capture_index, pattern_index, node_handle), Some tree ->
        Some
          {
            capture_index;
            pattern_index;
            node = { Node.node = node_handle; tree };
          }
end

module Tree_cursor = struct
  type cursor_handle
  type t = { cursor : cursor_handle; tree : tree_handle }

  external create_handle : Node.node_handle -> cursor_handle
    = "caml_ts_tree_cursor_new"

  external delete_handle : cursor_handle -> unit = "caml_ts_tree_cursor_delete"

  external reset_handle : cursor_handle -> Node.node_handle -> unit
    = "caml_ts_tree_cursor_reset"

  external current_node_handle : cursor_handle -> Node.node_handle
    = "caml_ts_tree_cursor_current_node"

  external current_field_name : cursor_handle -> string option
    = "caml_ts_tree_cursor_current_field_name"

  external current_field_id : cursor_handle -> int
    = "caml_ts_tree_cursor_current_field_id"

  external current_depth : cursor_handle -> int
    = "caml_ts_tree_cursor_current_depth"

  external goto_parent : cursor_handle -> bool
    = "caml_ts_tree_cursor_goto_parent"

  external goto_first_child : cursor_handle -> bool
    = "caml_ts_tree_cursor_goto_first_child"

  external goto_last_child : cursor_handle -> bool
    = "caml_ts_tree_cursor_goto_last_child"

  external goto_next_sibling : cursor_handle -> bool
    = "caml_ts_tree_cursor_goto_next_sibling"

  external goto_previous_sibling : cursor_handle -> bool
    = "caml_ts_tree_cursor_goto_previous_sibling"

  external goto_first_child_for_byte : cursor_handle -> int -> int
    = "caml_ts_tree_cursor_goto_first_child_for_byte"

  let create (node : Node.t) =
    { cursor = create_handle node.node; tree = node.tree }

  let delete t = delete_handle t.cursor
  let reset t (node : Node.t) = reset_handle t.cursor node.node

  let current_node t : Node.t =
    { Node.node = current_node_handle t.cursor; tree = t.tree }

  let current_field_name t = current_field_name t.cursor
  let current_field_id t = current_field_id t.cursor
  let current_depth t = current_depth t.cursor
  let goto_parent t = goto_parent t.cursor
  let goto_first_child t = goto_first_child t.cursor
  let goto_last_child t = goto_last_child t.cursor
  let goto_next_sibling t = goto_next_sibling t.cursor
  let goto_previous_sibling t = goto_previous_sibling t.cursor
  let goto_first_child_for_byte t byte = goto_first_child_for_byte t.cursor byte
end

(* --- Highlighting --- *)

let capture_names query =
  let n = Query.capture_count query in
  Array.init n (fun i -> Query.capture_name_for_id query i)

let highlight_impl query tree ~set_range =
  let names = capture_names query in
  let cursor = Query_cursor.create () in
  set_range cursor;
  let root = Tree.root_node tree in
  Query_cursor.exec cursor query root;
  let rec loop acc =
    match Query_cursor.next_capture cursor query with
    | None -> List.rev acc
    | Some cap -> (
        match names.(cap.capture_index) with
        | None -> loop acc
        | Some name ->
            if String.length name > 0 && name.[0] = '_' then loop acc
            else
              let s = Node.start_byte cap.node in
              let e = Node.end_byte cap.node in
              if s < e then loop ((s, e, name) :: acc) else loop acc)
  in
  loop []

let highlight query tree = highlight_impl query tree ~set_range:(fun _ -> ())

let highlight_range query tree ~start_byte ~end_byte =
  highlight_impl query tree ~set_range:(fun cursor ->
      Query_cursor.set_byte_range cursor ~start:start_byte ~end_:end_byte)
