(* Toffee: An OCaml port of Taffy's tree module *)

open Toffee_tree
open Toffee_geometry
open Toffee_compute
module Geometry = Toffee_geometry
module Style = Toffee_style

(* Tree submodule exports *)
module Node_id = Node_id
module Layout = Layout
module Available_space = Available_space
module Layout_input = Layout_input
module Layout_output = Layout_output
module Run_mode = Run_mode
module Cache = Cache

module Error = struct
  (* Error types *)
  type t =
    | Child_index_out_of_bounds of {
        parent : Node_id.t;
        child_index : int;
        child_count : int;
      }
    | Invalid_parent_node of Node_id.t
    | Invalid_child_node of Node_id.t
    | Invalid_input_node of Node_id.t

  let to_string = function
    | Child_index_out_of_bounds { parent; child_index; child_count } ->
        Printf.sprintf
          "Index (is %d) should be < child_count (%d) for parent node %d"
          child_index child_count (Node_id.to_int parent)
    | Invalid_parent_node parent ->
        Printf.sprintf "Parent Node %d is not in the TaffyTree instance"
          (Node_id.to_int parent)
    | Invalid_child_node child ->
        Printf.sprintf "Child Node %d is not in the TaffyTree instance"
          (Node_id.to_int child)
    | Invalid_input_node node ->
        Printf.sprintf "Supplied Node %d is not in the TaffyTree instance"
          (Node_id.to_int node)
end

open Error

type nonrec 'a result = ('a, Error.t) result
type config = { use_rounding : bool }
(* Configuration for TaffyTree *)

let default_config = { use_rounding = true }

(* Node data stored for each node. [rounded] holds when [final_layout] and
   every final layout below it were rounded from the current unrounded
   layouts, with the node's parent at the position [rounded_at] (x, y).
   Storing a different unrounded layout, or changing the node's style,
   context or children, clears it on the node and its ancestors, so [rounded]
   on a node implies it on every descendant. *)
type node_data = {
  style : Style.t;
  mutable unrounded_layout : Layout.t;
  mutable final_layout : Layout.t;
  has_context : bool;
  cache : Cache.t;
  mutable rounded : bool;
  rounded_at : float array;
}

let new_node_data style =
  {
    style;
    cache = Cache.make ();
    unrounded_layout = Layout.default;
    final_layout = Layout.default;
    has_context = false;
    rounded = false;
    rounded_at = [| 0.; 0. |];
  }

let mark_dirty node_data = Cache.clear node_data.cache

(* Growable children buffer for each node *)
type children_buffer = { mutable data : Node_id.t array; mutable len : int }

let empty_children () = { data = [||]; len = 0 }
let clear_children buffer = buffer.len <- 0

let ensure_child_capacity buffer required =
  let capacity = Array.length buffer.data in
  if required <= capacity then ()
  else
    let next_capacity = max required (max 1 (capacity * 2)) in
    let placeholder = Node_id.make 0 in
    let new_data = Array.make next_capacity placeholder in
    Array.blit buffer.data 0 new_data 0 buffer.len;
    buffer.data <- new_data

let append_child buffer child =
  ensure_child_capacity buffer (buffer.len + 1);
  buffer.data.(buffer.len) <- child;
  buffer.len <- buffer.len + 1

let insert_child buffer index child =
  ensure_child_capacity buffer (buffer.len + 1);
  Array.blit buffer.data index buffer.data (index + 1) (buffer.len - index);
  buffer.data.(index) <- child;
  buffer.len <- buffer.len + 1

let child_at buffer index = buffer.data.(index)

let replace_child_at buffer index child =
  let old = buffer.data.(index) in
  buffer.data.(index) <- child;
  old

let remove_child_at buffer index =
  let removed = buffer.data.(index) in
  let tail_len = buffer.len - index - 1 in
  if tail_len > 0 then
    Array.blit buffer.data (index + 1) buffer.data index tail_len;
  buffer.len <- buffer.len - 1;
  removed

let remove_children_range_in_buffer buffer start_index end_index =
  let keep_from = end_index + 1 in
  let tail_len = buffer.len - keep_from in
  if tail_len > 0 then
    Array.blit buffer.data keep_from buffer.data start_index tail_len;
  buffer.len <- buffer.len - (end_index - start_index + 1)

let find_child_index buffer child =
  let rec loop idx =
    if idx >= buffer.len then None
    else if Node_id.equal buffer.data.(idx) child then Some idx
    else loop (idx + 1)
  in
  loop 0

let remove_child_value buffer child =
  match find_child_index buffer child with
  | None -> None
  | Some idx -> Some (remove_child_at buffer idx)

let children_to_seq buffer =
  let rec aux idx () =
    if idx >= buffer.len then Seq.Nil
    else Seq.Cons (buffer.data.(idx), aux (idx + 1))
  in
  aux 0

let children_to_list buffer =
  let rec build acc idx =
    if idx < 0 then acc else build (buffer.data.(idx) :: acc) (idx - 1)
  in
  build [] (buffer.len - 1)

type 'context tree = {
  mutable nodes : node_data array;
  mutable parents : Node_id.t option array;
  mutable children : children_buffer array;
  mutable node_context_data : 'context option array;
  mutable generations : int array;
  mutable free_list : int list;
  mutable next_index : int;
  mutable live_node_count : int;
  config : config;
}
(* The main TaffyTree type *)

let default_capacity = 16

let make_arrays capacity config =
  {
    nodes = Array.init capacity (fun _ -> new_node_data Style.default);
    parents = Array.make capacity None;
    children = Array.init capacity (fun _ -> empty_children ());
    node_context_data = Array.make capacity None;
    generations = Array.make capacity 0;
    free_list = [];
    next_index = 0;
    live_node_count = 0;
    config;
  }

let grow tree required_index =
  let old_capacity = Array.length tree.nodes in
  let new_capacity = max (required_index + 1) (old_capacity * 2) in
  let copy_or_default arr default =
    Array.init new_capacity (fun idx ->
        if idx < old_capacity then arr.(idx) else default ())
  in
  tree.nodes <-
    copy_or_default tree.nodes (fun () -> new_node_data Style.default);
  tree.parents <- copy_or_default tree.parents (fun () -> None);
  tree.children <- copy_or_default tree.children empty_children;
  tree.node_context_data <-
    copy_or_default tree.node_context_data (fun () -> None);
  tree.generations <- copy_or_default tree.generations (fun () -> 0)

let ensure_capacity tree required_index =
  if required_index < Array.length tree.nodes then ()
  else grow tree required_index

(* Create a new TaffyTree *)
let new_tree () = make_arrays default_capacity default_config
let with_capacity capacity = make_arrays (max capacity 0) default_config

(* Enable/disable rounding *)
let enable_rounding tree = { tree with config = { use_rounding = true } }
let disable_rounding tree = { tree with config = { use_rounding = false } }

let is_valid tree node =
  let index = Node_id.index node in
  index < tree.next_index && tree.generations.(index) = Node_id.generation node

let node_index tree node =
  let idx = Node_id.index node in
  if is_valid tree node then Ok idx else Error (Invalid_input_node node)

(* Layout looks nodes up with it many times per node: no result to build. *)
let node_index_exn tree node =
  if is_valid tree node then Node_id.index node
  else invalid_arg "Invalid node id"

let parent_index tree parent =
  let idx = Node_id.index parent in
  if is_valid tree parent then Ok idx else Error (Invalid_parent_node parent)

let child_index tree child =
  let idx = Node_id.index child in
  if is_valid tree child then Ok idx else Error (Invalid_child_node child)

let id_of_index tree idx =
  Node_id.make_with_generation idx tree.generations.(idx)

let rec mark_dirty_recursive tree idx =
  match mark_dirty tree.nodes.(idx) with
  | Cache.Already_empty -> ()
  | Cache.Cleared -> (
      match tree.parents.(idx) with
      | Some parent -> mark_dirty_recursive tree (Node_id.index parent)
      | None -> ())

(* Clears [rounded] on the node at [idx] and its ancestors. A node that is not
   rounded has no rounded ancestor, so clearing stops there. *)
let rec unround tree idx =
  let node_data = tree.nodes.(idx) in
  if node_data.rounded then begin
    node_data.rounded <- false;
    match tree.parents.(idx) with
    | Some parent -> unround tree (Node_id.index parent)
    | None -> ()
  end

(* The node at [idx] changed through this module: its style, its context, its
   children, or a [mark_dirty]. Its cached layouts and the final layouts
   rounded at it and above are out of date. *)
let changed tree idx =
  mark_dirty_recursive tree idx;
  unround tree idx

(* [same_float x y] holds when [x] and [y] are equal bit for bit, so equal
   zeros of opposite signs differ. Inlined, it compares the floats of a
   layout without boxing them. *)
let[@inline] same_float (x : float) y =
  x = y && (x <> 0. || Float.sign_bit x = Float.sign_bit y)

let[@inline] same_size (a : Sizef.t) (b : Sizef.t) =
  same_float a.width b.width && same_float a.height b.height

let[@inline] same_rect (a : Rectf.t) (b : Rectf.t) =
  same_float a.left b.left && same_float a.right b.right
  && same_float a.top b.top
  && same_float a.bottom b.bottom

(* [same_layout a b] holds when [a] and [b] are equal bit for bit, since a
   stored layout keeps the sign of its zeros. *)
let same_layout a b =
  let open Layout in
  order a = order b
  && same_float (location a).x (location b).x
  && same_float (location a).y (location b).y
  && same_size (size a) (size b)
  && same_size (content_size a) (content_size b)
  && same_size (scrollbar_size a) (scrollbar_size b)
  && same_rect (border a) (border b)
  && same_rect (padding a) (padding b)
  && same_rect (margin a) (margin b)

let allocate_index tree =
  match tree.free_list with
  | idx :: rest ->
      tree.free_list <- rest;
      tree.live_node_count <- tree.live_node_count + 1;
      idx
  | [] ->
      let idx = tree.next_index in
      ensure_capacity tree idx;
      tree.next_index <- tree.next_index + 1;
      tree.live_node_count <- tree.live_node_count + 1;
      idx

let reset_slot tree idx node_data context_opt =
  tree.nodes.(idx) <- node_data;
  tree.parents.(idx) <- None;
  tree.node_context_data.(idx) <- context_opt;
  clear_children tree.children.(idx)

(* Node creation *)
let new_leaf tree style =
  let idx = allocate_index tree in
  reset_slot tree idx (new_node_data style) None;
  Ok (id_of_index tree idx)

let new_leaf_with_context tree style context =
  let idx = allocate_index tree in
  let node_data = { (new_node_data style) with has_context = true } in
  reset_slot tree idx node_data (Some context);
  Ok (id_of_index tree idx)

let validate_children tree children =
  let rec loop index =
    if index = Array.length children then Ok ()
    else
      match child_index tree children.(index) with
      | Ok _ -> loop (index + 1)
      | Error _ as e -> e
  in
  loop 0

let new_with_children tree style children =
  match validate_children tree children with
  | Error _ as e -> e
  | Ok () ->
      let idx = allocate_index tree in
      reset_slot tree idx (new_node_data style) None;
      Array.iter
        (fun child ->
          let child_idx = Node_id.index child in
          tree.parents.(child_idx) <- Some (id_of_index tree idx))
        children;
      let buffer = tree.children.(idx) in
      Array.iter (append_child buffer) children;
      Ok (id_of_index tree idx)

let invalidate_node tree idx =
  tree.generations.(idx) <- tree.generations.(idx) + 1;
  tree.free_list <- idx :: tree.free_list;
  tree.live_node_count <- tree.live_node_count - 1;
  tree.parents.(idx) <- None;
  tree.node_context_data.(idx) <- None;
  clear_children tree.children.(idx);
  tree.nodes.(idx) <- new_node_data Style.default

(* Tree operations *)
let clear tree =
  for idx = 0 to tree.next_index - 1 do
    tree.generations.(idx) <- tree.generations.(idx) + 1;
    tree.parents.(idx) <- None;
    tree.node_context_data.(idx) <- None;
    clear_children tree.children.(idx);
    tree.nodes.(idx) <- new_node_data Style.default
  done;
  tree.live_node_count <- 0;
  tree.free_list <- [];
  tree.next_index <- 0

let remove tree node =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx ->
      (match tree.parents.(idx) with
      | Some parent ->
          let parent_idx = Node_id.index parent in
          ignore (remove_child_value tree.children.(parent_idx) node);
          changed tree parent_idx
      | None -> ());

      let children = tree.children.(idx) in
      for child_idx = 0 to children.len - 1 do
        let child = child_at children child_idx in
        tree.parents.(Node_id.index child) <- None
      done;

      invalidate_node tree idx;
      Ok node

(* Context operations *)
let set_node_context tree node context_opt =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx ->
      let has_context = Option.is_some context_opt in
      let node_data = tree.nodes.(idx) in
      tree.nodes.(idx) <- { node_data with has_context };
      tree.node_context_data.(idx) <- context_opt;
      changed tree idx;
      Ok ()

let get_node_context tree node =
  match node_index tree node with
  | Ok idx -> tree.node_context_data.(idx)
  | Error _ -> None

let get_node_context_mut = get_node_context

(* Child management *)
let add_child tree parent child =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> (
      match child_index tree child with
      | Error _ as e -> e
      | Ok child_idx ->
          tree.parents.(child_idx) <- Some parent;
          append_child tree.children.(parent_idx) child;
          changed tree parent_idx;
          Ok ())

let insert_child_at_index tree parent child_position child =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> (
      match child_index tree child with
      | Error _ as e -> e
      | Ok child_idx ->
          let children = tree.children.(parent_idx) in
          let child_count = children.len in
          if child_position > child_count then
            Error
              (Child_index_out_of_bounds
                 { parent; child_index = child_position; child_count })
          else (
            tree.parents.(child_idx) <- Some parent;
            insert_child children child_position child;
            changed tree parent_idx;
            Ok ()))

let set_children tree parent new_children =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> (
      match validate_children tree new_children with
      | Error _ as e -> e
      | Ok () ->
          let current_children = tree.children.(parent_idx) in
          for i = 0 to current_children.len - 1 do
            let child = child_at current_children i in
            tree.parents.(Node_id.index child) <- None
          done;

          Array.iter
            (fun child ->
              let child_idx = Node_id.index child in
              (match tree.parents.(child_idx) with
              | Some old_parent when not (Node_id.equal old_parent parent) ->
                  let old_parent_idx = Node_id.index old_parent in
                  let old_children = tree.children.(old_parent_idx) in
                  ignore (remove_child_value old_children child);
                  changed tree old_parent_idx
              | _ -> ());
              tree.parents.(child_idx) <- Some parent)
            new_children;

          clear_children current_children;
          Array.iter (append_child current_children) new_children;
          changed tree parent_idx;
          Ok ())

let remove_child tree parent child =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> (
      let children = tree.children.(parent_idx) in
      match find_child_index children child with
      | None -> Error (Invalid_child_node child)
      | Some idx ->
          let removed_child = remove_child_at children idx in
          tree.parents.(Node_id.index removed_child) <- None;
          changed tree parent_idx;
          Ok removed_child)

let remove_child_at_index tree parent child_index =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx ->
      let children = tree.children.(parent_idx) in
      let child_count = children.len in
      if child_index >= child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else
        let removed_child = remove_child_at children child_index in
        tree.parents.(Node_id.index removed_child) <- None;
        changed tree parent_idx;
        Ok removed_child

let remove_children_range tree parent (start_idx, end_idx) =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx ->
      let children = tree.children.(parent_idx) in
      let child_count = children.len in
      if start_idx < 0 || end_idx < start_idx || end_idx >= child_count then
        Error
          (Child_index_out_of_bounds
             { parent; child_index = end_idx; child_count })
      else (
        for idx = start_idx to end_idx do
          let child = child_at children idx in
          tree.parents.(Node_id.index child) <- None
        done;
        remove_children_range_in_buffer children start_idx end_idx;
        changed tree parent_idx;
        Ok ())

let replace_child_at_index tree parent child_position new_child =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> (
      match child_index tree new_child with
      | Error _ as e -> e
      | Ok new_child_idx ->
          let children = tree.children.(parent_idx) in
          let child_count = children.len in
          if child_position >= child_count then
            Error
              (Child_index_out_of_bounds
                 { parent; child_index = child_position; child_count })
          else
            let old_child =
              replace_child_at children child_position new_child
            in
            tree.parents.(Node_id.index old_child) <- None;
            tree.parents.(new_child_idx) <- Some parent;
            changed tree parent_idx;
            Ok old_child)

(* Query operations *)
let child_at_index tree parent child_index =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx ->
      let children = tree.children.(parent_idx) in
      let child_count = children.len in
      if child_index >= child_count then
        Error (Child_index_out_of_bounds { parent; child_index; child_count })
      else Ok (child_at children child_index)

let total_node_count tree = tree.live_node_count

let parent tree child =
  match node_index tree child with
  | Error _ -> None
  | Ok idx -> tree.parents.(idx)

let children tree parent =
  match parent_index tree parent with
  | Error _ as e -> e
  | Ok parent_idx -> Ok (children_to_list tree.children.(parent_idx))

(* Style operations *)
let set_style tree node style =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx ->
      let node_data = tree.nodes.(idx) in
      tree.nodes.(idx) <- { node_data with style };
      changed tree idx;
      Ok ()

let style tree node =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx -> Ok tree.nodes.(idx).style

(* Layout operations *)
let layout tree node =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx ->
      let node_data = tree.nodes.(idx) in
      if tree.config.use_rounding then Ok node_data.final_layout
      else Ok node_data.unrounded_layout

let unrounded_layout tree node =
  if is_valid tree node then tree.nodes.(Node_id.index node).unrounded_layout
  else Layout.default

let mark_dirty tree node =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx ->
      changed tree idx;
      Ok ()

let dirty tree node =
  match node_index tree node with
  | Error _ as e -> e
  | Ok idx -> Ok (Cache.is_empty tree.nodes.(idx).cache)

type 'context measure_function =
  float option size ->
  Available_space.t size ->
  Node_id.t ->
  'context option ->
  Style.t ->
  Sizef.t
(* Type of measure functions *)

(* View - a view over the tree for layout computation *)
module View = struct
  (* The algorithms take the view as first-class modules. They are packed
     once per layout, in [compute_layout_with_measure], rather than at every
     node a layout visits. *)
  type 'context t = {
    taffy : 'context tree;
    measure_function : 'context measure_function;
    cache_tree : (module CACHE_TREE with type t = 'context t);
    hidden_tree : (module CACHE_LAYOUT_PARTIAL_TREE with type t = 'context t);
    layout_tree : (module LAYOUT_PARTIAL_TREE with type t = 'context t);
  }

  (* Traverse_partial_tree implementation *)
  let child_ids tree parent_node_id =
    let idx = node_index_exn tree.taffy parent_node_id in
    children_to_seq tree.taffy.children.(idx)

  let child_count tree parent_node_id =
    let idx = node_index_exn tree.taffy parent_node_id in
    tree.taffy.children.(idx).len

  let get_child_id tree parent_node_id index =
    let idx = node_index_exn tree.taffy parent_node_id in
    child_at tree.taffy.children.(idx) index

  (* LayoutPartialTree implementation *)
  let get_core_container_style tree node_id =
    tree.taffy.nodes.(node_index_exn tree.taffy node_id).style

  (* A layout equal to the stored one keeps the stored record and the final
     layouts rounded from it, so a relayout that changes nothing rounds
     nothing again and every layout stays physically the same. *)
  let set_unrounded_layout tree node_id layout =
    let taffy = tree.taffy in
    let idx = node_index_exn taffy node_id in
    let node_data = taffy.nodes.(idx) in
    if not (same_layout node_data.unrounded_layout layout) then begin
      node_data.unrounded_layout <- layout;
      unround taffy idx
    end

  let resolve_calc_value _tree _val _basis = 0.0

  (* Cache operations *)
  let cache_get tree node_id inputs =
    let node_data = tree.taffy.nodes.(node_index_exn tree.taffy node_id) in
    Cache.get node_data.cache node_data.style inputs

  let cache_store tree node_id inputs layout_output =
    let idx = node_index_exn tree.taffy node_id in
    let node_data = tree.taffy.nodes.(idx) in
    Cache.store node_data.cache node_data.style inputs layout_output

  let cache_clear tree node_id =
    let idx = node_index_exn tree.taffy node_id in
    let node_data = tree.taffy.nodes.(idx) in
    ignore (Cache.clear node_data.cache)

  let rec compute_child_layout tree node inputs =
    if Layout_input.run_mode inputs = Run_mode.Perform_hidden_layout then
      compute_hidden_layout tree.hidden_tree tree node
    else compute_cached_layout tree.cache_tree tree node inputs compute_uncached

  and compute_uncached tree node inputs =
    let node_idx = node_index_exn tree.taffy node in
    let node_data = tree.taffy.nodes.(node_idx) in
    let display_mode = Style.display node_data.style in
    let has_children = child_count tree node > 0 in
    match (display_mode, has_children) with
    | None, _ -> compute_hidden_layout tree.hidden_tree tree node
    | Block, true -> compute_block_layout tree.layout_tree tree node inputs
    | Flex, true -> compute_flexbox_layout tree.layout_tree tree node inputs
    | Grid, true -> compute_grid_layout tree.layout_tree ~tree ~node ~inputs
    | _, false ->
        (* Leaf node *)
        let style = node_data.style in
        let node_context =
          if node_data.has_context then tree.taffy.node_context_data.(node_idx)
          else None
        in
        let measure_function known_dimensions available_space =
          tree.measure_function known_dimensions available_space node
            node_context style
        in
        compute_leaf_layout ~inputs ~style
          ~resolve_calc_value:(resolve_calc_value tree) ~measure_function

  (* RoundTree implementation *)
  let get_unrounded_layout tree node =
    tree.taffy.nodes.(node_index_exn tree.taffy node).unrounded_layout

  let is_rounded tree node_id ~x ~y =
    let node_data = tree.taffy.nodes.(node_index_exn tree.taffy node_id) in
    node_data.rounded
    && Float.equal node_data.rounded_at.(0) x
    && Float.equal node_data.rounded_at.(1) y

  let set_final_layout tree node_id ~x ~y layout =
    let node_data = tree.taffy.nodes.(node_index_exn tree.taffy node_id) in
    node_data.final_layout <- layout;
    node_data.rounded_at.(0) <- x;
    node_data.rounded_at.(1) <- y;
    node_data.rounded <- true
end

(* Main layout computation *)
let compute_layout_with_measure (type ctx) (tree : ctx tree) node_id
    available_space measure_function =
  let view =
    {
      View.taffy = tree;
      measure_function;
      cache_tree =
        (module struct
          type t = ctx View.t

          let cache_get = View.cache_get
          let cache_store = View.cache_store
          let cache_clear = View.cache_clear
        end);
      hidden_tree =
        (module struct
          type t = ctx View.t

          let child_ids = View.child_ids
          let child_count = View.child_count
          let get_child_id = View.get_child_id
          let get_core_container_style = View.get_core_container_style
          let set_unrounded_layout = View.set_unrounded_layout
          let compute_child_layout = View.compute_child_layout
          let resolve_calc_value = View.resolve_calc_value
          let cache_get = View.cache_get
          let cache_store = View.cache_store
          let cache_clear = View.cache_clear
        end);
      layout_tree =
        (module struct
          type t = ctx View.t

          let child_ids = View.child_ids
          let child_count = View.child_count
          let get_child_id = View.get_child_id
          let get_core_container_style = View.get_core_container_style
          let set_unrounded_layout = View.set_unrounded_layout
          let compute_child_layout = View.compute_child_layout
          let resolve_calc_value = View.resolve_calc_value
        end);
    }
  in

  compute_root_layout
    (module struct
      type t = ctx View.t

      let child_ids = View.child_ids
      let child_count = View.child_count
      let get_child_id = View.get_child_id
      let get_core_container_style = View.get_core_container_style
      let set_unrounded_layout = View.set_unrounded_layout
      let compute_child_layout = View.compute_child_layout
      let resolve_calc_value = View.resolve_calc_value
    end)
    view node_id available_space;

  if tree.config.use_rounding then
    round_layout
      (module struct
        type t = ctx View.t

        let child_ids = View.child_ids
        let child_count = View.child_count
        let get_child_id = View.get_child_id
        let get_unrounded_layout = View.get_unrounded_layout
        let is_rounded = View.is_rounded
        let set_final_layout = View.set_final_layout
      end)
      view node_id;

  Ok ()

let compute_layout tree node available_space =
  compute_layout_with_measure tree node available_space (fun _ _ _ _ _ ->
      Sizef.zero)

(* Modules for tree traversal - expose for external use *)
module Traverse_partial_tree = struct
  type 'context t = 'context tree

  let child_ids tree parent_node_id =
    children_to_seq tree.children.(node_index_exn tree parent_node_id)

  let child_count tree parent_node_id =
    tree.children.(node_index_exn tree parent_node_id).len

  let get_child_id tree parent_node_id index =
    child_at tree.children.(node_index_exn tree parent_node_id) index
end

module Cache_tree = struct
  type 'context t = 'context tree

  let cache_get tree node_id inputs =
    let node_data = tree.nodes.(node_index_exn tree node_id) in
    Cache.get node_data.cache node_data.style inputs

  let cache_store tree node_id inputs layout_output =
    let node_data = tree.nodes.(node_index_exn tree node_id) in
    Cache.store node_data.cache node_data.style inputs layout_output

  let cache_clear tree node_id =
    let node_data = tree.nodes.(node_index_exn tree node_id) in
    ignore (Cache.clear node_data.cache)
end

module Print_tree = struct
  type 'context t = 'context tree

  let child_ids = Traverse_partial_tree.child_ids
  let child_count = Traverse_partial_tree.child_count
  let get_child_id = Traverse_partial_tree.get_child_id

  let get_debug_label tree node_id =
    let node_data = tree.nodes.(node_index_exn tree node_id) in
    let display = Style.display node_data.style in
    let num_children = Traverse_partial_tree.child_count tree node_id in
    match (display, num_children) with
    | None, _ -> "NONE"
    | _, 0 -> "LEAF"
    | Block, _ -> "BLOCK"
    | Flex, _ -> (
        match Style.flex_direction node_data.style with
        | Row | Row_reverse -> "FLEX ROW"
        | Column | Column_reverse -> "FLEX COL")
    | Grid, _ -> "GRID"

  let get_final_layout tree node_id =
    let node_data = tree.nodes.(node_index_exn tree node_id) in
    if tree.config.use_rounding then node_data.final_layout
    else node_data.unrounded_layout
end

(* Print tree for debugging *)
let print_tree (type ctx) (tree : ctx tree) root_id =
  Toffee_tree.print_tree
    (module struct
      type t = ctx tree

      let child_ids = Print_tree.child_ids
      let child_count = Print_tree.child_count
      let get_child_id = Print_tree.get_child_id
      let get_debug_label = Print_tree.get_debug_label
      let get_final_layout = Print_tree.get_final_layout
    end : Toffee_tree.PRINT_TREE
      with type t = ctx tree)
    tree root_id
