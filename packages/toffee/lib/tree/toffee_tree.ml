(** Core Types *)

module Geometry = Toffee_geometry
module Node_id = Node_id
module Run_mode = Run_mode
module Sizing_mode = Sizing_mode
module Collapsible_margin_set = Collapsible_margin_set
module Requested_axis = Requested_axis
module Available_space = Available_space
module Layout_input = Layout_input
module Layout_output = Layout_output
module Layout = Layout
module Cache = Cache

(** Tree Types *)

module type TRAVERSE_PARTIAL_TREE = sig
  type t

  val child_ids : t -> Node_id.t -> Node_id.t Seq.t
  val child_count : t -> Node_id.t -> int
  val get_child_id : t -> Node_id.t -> int -> Node_id.t
end

module type TRAVERSE_TREE = sig
  include TRAVERSE_PARTIAL_TREE
end

module type LAYOUT_PARTIAL_TREE = sig
  include TRAVERSE_PARTIAL_TREE

  val get_core_container_style : t -> Node_id.t -> Toffee_style.t
  val set_unrounded_layout : t -> Node_id.t -> Layout.t -> unit
  val compute_child_layout : t -> Node_id.t -> Layout_input.t -> Layout_output.t
  val resolve_calc_value : t -> int -> float -> float
end

module type CACHE_TREE = sig
  type t

  val cache_get : t -> Node_id.t -> Layout_input.t -> Layout_output.t option
  val cache_store : t -> Node_id.t -> Layout_input.t -> Layout_output.t -> unit
  val cache_clear : t -> Node_id.t -> unit
end

module type ROUND_TREE = sig
  include TRAVERSE_TREE

  val get_unrounded_layout : t -> Node_id.t -> Layout.t
  val is_rounded : t -> Node_id.t -> x:float -> y:float -> bool

  val set_final_layout :
    t -> Node_id.t -> x:float -> y:float -> Layout.t -> unit
end

module type PRINT_TREE = sig
  include TRAVERSE_TREE

  val get_debug_label : t -> Node_id.t -> string
  val get_final_layout : t -> Node_id.t -> Layout.t
end

(** Print a debug representation of the computed layout for a tree of nodes *)
let print_tree (type a) (module Tree : PRINT_TREE with type t = a) tree root =
  Printf.printf "TREE\n";
  let rec print_node node_id has_sibling lines_string =
    let layout = Tree.get_final_layout tree node_id in
    let display = Tree.get_debug_label tree node_id in
    let num_children = Tree.child_count tree node_id in

    let fork_string = if has_sibling then "├── " else "└── " in

    (* Print the node info *)
    Printf.printf
      "%s%s %s [x: %-4g y: %-4g w: %-4g h: %-4g content_w: %-4g content_h: \
       %-4g border: l:%g r:%g t:%g b:%g, padding: l:%g r:%g t:%g b:%g] (Node \
       %d)\n"
      lines_string fork_string display layout.location.x layout.location.y
      layout.size.width layout.size.height layout.content_size.width
      layout.content_size.height layout.border.left layout.border.right
      layout.border.top layout.border.bottom layout.padding.left
      layout.padding.right layout.padding.top layout.padding.bottom
      (Node_id.to_int node_id);

    let bar = if has_sibling then "│   " else "    " in
    let new_string = lines_string ^ bar in

    (* Recurse into children *)
    for index = 0 to num_children - 1 do
      let child = Tree.get_child_id tree node_id index in
      let has_sibling = index < num_children - 1 in
      print_node child has_sibling new_string
    done
  in
  print_node root false ""
