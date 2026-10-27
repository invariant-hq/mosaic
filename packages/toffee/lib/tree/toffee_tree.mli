(** Tree abstractions for CSS layout computation.

    This module defines signatures and types for layout trees in toffee. Layout
    algorithms operate on trees by calling trait methods to access children,
    styles, and storage.

    {1 Trait Hierarchy}

    Traits form a dependency hierarchy enabling different operations:

    - {!TRAVERSE_PARTIAL_TREE}: Access immediate children of a node
    - {!LAYOUT_PARTIAL_TREE}: Run layout algorithms (Flexbox, Grid, Block)
    - {!TRAVERSE_TREE}: Guarantee full recursive tree access
    - {!CACHE_TREE}: Enable layout result caching
    - {!ROUND_TREE}: Round float layouts to integer pixels
    - {!PRINT_TREE}: Debug-print layout trees

    Layout algorithms require {!LAYOUT_PARTIAL_TREE}. Pixel rounding and debug
    printing require {!TRAVERSE_TREE}, which extends {!TRAVERSE_PARTIAL_TREE}
    with a guarantee that child access methods can recurse infinitely.

    {1 Layout Flow}

    Layout proceeds top-down: parents pass {!Layout_input} constraints to
    children via [compute_child_layout], and children return {!Layout_output}
    with computed sizes and margins. Algorithms read styles via
    [get_core_container_style] and store results via [set_unrounded_layout]. *)

(** {1 Core Types} *)

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

(** {1 Tree Signatures} *)

(** Tree traversal for immediate children.

    Provides access to a node's direct children. Implementations need only
    support access to a single container node and its immediate children, not
    full recursive traversal. *)
module type TRAVERSE_PARTIAL_TREE = sig
  type t
  (** The tree structure type. *)

  val child_ids : t -> Node_id.t -> Node_id.t Seq.t
  (** [child_ids tree node] returns a sequence of [node]'s child IDs. *)

  val child_count : t -> Node_id.t -> int
  (** [child_count tree node] returns the number of children of [node]. *)

  val get_child_id : t -> Node_id.t -> int -> Node_id.t
  (** [get_child_id tree node n] returns the [n]th child ID of [node].

      Raises [Invalid_argument] if [n] is negative or >=
      [child_count tree node]. *)
end

(** Tree traversal with full recursive access.

    Extends {!TRAVERSE_PARTIAL_TREE} with a guarantee that [child_ids],
    [child_count], and [get_child_id] can be called recursively on any
    descendant node. Required by {!ROUND_TREE} and {!PRINT_TREE}, which must
    traverse entire subtrees. *)
module type TRAVERSE_TREE = sig
  include TRAVERSE_PARTIAL_TREE
end

(** Tree interface for layout algorithms.

    Extends {!TRAVERSE_PARTIAL_TREE} with style access, layout storage, and
    recursive child layout. Required by Flexbox, Grid, and Block layout
    algorithms. *)
module type LAYOUT_PARTIAL_TREE = sig
  include TRAVERSE_PARTIAL_TREE

  val get_core_container_style : t -> Node_id.t -> Toffee_style.t
  (** [get_core_container_style tree node] returns the style for [node].

      Layout algorithms read properties such as [display], [flex_direction],
      [grid_template_rows], [padding], [margin], and box model properties. *)

  val set_unrounded_layout : t -> Node_id.t -> Layout.t -> unit
  (** [set_unrounded_layout tree node layout] stores the computed layout for
      [node].

      Called by layout algorithms with float-valued results before pixel
      rounding. *)

  val compute_child_layout : t -> Node_id.t -> Layout_input.t -> Layout_output.t
  (** [compute_child_layout tree node input] recursively computes layout for
      [node].

      Called by parent nodes to lay out their children. Implementations dispatch
      to the appropriate algorithm (Flexbox, Grid, Block) based on [node]'s
      [display] style. *)

  val resolve_calc_value : t -> int -> float -> float
  (** [resolve_calc_value tree id basis] resolves a CSS [calc()] expression.

      - [id]: Opaque identifier for the calc expression (internal pointer)
      - [basis]: Basis value for percentage resolution

      Implementations without [calc()] support can return [0.0]. *)
end

(** Tree interface for layout caching.

    Provides methods for storing and retrieving cached layout results. Enables
    memoization of layout computations across multiple passes. *)
module type CACHE_TREE = sig
  type t
  (** The tree structure type. *)

  val cache_get : t -> Node_id.t -> Layout_input.t -> Layout_output.t option
  (** [cache_get tree node inputs] retrieves a cached layout result for [node]
      under [inputs].

      Returns [None] if no cached result matches the inputs. *)

  val cache_store : t -> Node_id.t -> Layout_input.t -> Layout_output.t -> unit
  (** [cache_store tree node inputs output] stores [output] as the cached result
      for [node] under [inputs]. *)

  val cache_clear : t -> Node_id.t -> unit
  (** [cache_clear tree node] removes all cached entries for [node].

      Call when the node's style or content changes to invalidate stale results.
  *)
end

(** Tree interface for pixel rounding.

    Extends {!TRAVERSE_TREE} with access to unrounded layouts and storage for
    final pixel-snapped layouts. Used by [round_layout] to convert float layouts
    to integer coordinates. *)
module type ROUND_TREE = sig
  include TRAVERSE_TREE

  val get_unrounded_layout : t -> Node_id.t -> Layout.t
  (** [get_unrounded_layout tree node] returns the float-valued layout computed
      by layout algorithms. *)

  val is_rounded : t -> Node_id.t -> x:float -> y:float -> bool
  (** [is_rounded tree node ~x ~y] is [true] only if the final layouts of [node]
      and of all its descendants were stored by {!set_final_layout}, none of
      their unrounded layouts was set since, and rounding them with [node]'s
      parent at the unrounded absolute position [(x, y)] gives them again:
      either they were stored with the parent at [(x, y)], or that position and
      the one they were stored at are whole and every value rounding read was
      whole, so that rounding returned the unrounded values. Rounding then skips
      the subtree. A tree that does not track this returns [false]. *)

  val set_final_layout :
    t -> Node_id.t -> x:float -> y:float -> Layout.t -> unit
  (** [set_final_layout tree node ~x ~y layout] stores the final pixel-rounded
      layout for [node], rounded with [node]'s parent at the unrounded absolute
      position [(x, y)]. *)
end

(** Tree interface for debug printing.

    Extends {!TRAVERSE_TREE} with methods for pretty-printing layout tree
    structure. Used by {!print_tree}. *)
module type PRINT_TREE = sig
  include TRAVERSE_TREE

  val get_debug_label : t -> Node_id.t -> string
  (** [get_debug_label tree node] returns a human-readable label for [node].

      Typically describes the node type: "Flex", "Grid", "Block", or "Text". *)

  val get_final_layout : t -> Node_id.t -> Layout.t
  (** [get_final_layout tree node] returns the final computed layout for [node].
  *)
end

(** {1 Tree Functions} *)

val print_tree : (module PRINT_TREE with type t = 'a) -> 'a -> Node_id.t -> unit
(** [print_tree (module Tree) tree root] prints a debug representation of the
    layout tree rooted at [root] to stdout.

    Output format:
    {v
    TREE
    └── Flex [x: 0 y: 0 w: 800 h: 600 content_w: 780 content_h: 580 ...] (Node 0)
        ├── Block [x: 10 y: 10 w: 200 h: 100 ...] (Node 1)
        └── Text [x: 10 y: 120 w: 150 h: 20 ...] (Node 2)
    v}

    Each line shows the node's label, location, size, content_size, border,
    padding, and node ID. Children are indented with tree drawing characters. *)
