(** High-performance CSS layout engine implementing flexbox, CSS grid, and block
    layout.

    Toffee is an OCaml port of the Taffy layout library. It computes the spatial
    arrangement of UI nodes based on CSS layout algorithms, producing precise
    positions and dimensions for rendering.

    {1 Overview}

    Layout computation operates on a tree of nodes, where each node has a style
    (CSS properties like [display], [flex_direction], [width]) and optional
    children. The engine computes layouts by traversing the tree, applying
    layout algorithms based on the [display] property, and resolving dimensions
    under space constraints.

    Nodes are identified by {!Node_id.t}, which uses generational indices to
    prevent use-after-free errors. The tree parameterizes over a context type
    ['context] for associating custom data with nodes.

    The primary workflow: 1. Create a tree with {!new_tree}. 2. Build the node
    hierarchy with {!new_leaf}, {!new_with_children}, and {!add_child}. 3.
    Compute layout with {!compute_layout}, providing available space
    constraints. 4. Query computed layouts with {!val-layout}.

    {1 Basic Usage}

    Create a simple flexbox layout:

    {[
    let tree = Toffee.new_tree () in

    (* Create a container with two children *)
    let child1 =
      Toffee.new_leaf tree
        Style.(
          make
            ~size:Size.{ width = Dimension.px 100.; height = Dimension.px 50. }
            ())
      |> Result.get_ok
    in
    let child2 =
      Toffee.new_leaf tree Style.(make ~flex_grow:1.0 ()) |> Result.get_ok
    in

    let root =
      Toffee.new_with_children tree
        Style.(make ~display:Display.Flex ())
        [| child1; child2 |]
      |> Result.get_ok
    in

    (* Compute layout with 500x300 available space *)
    Toffee.compute_layout tree root
      Size.
        {
          width = Available_space.of_length 500.;
          height = Available_space.of_length 300.;
        }
    |> Result.get_ok;

    (* Query the computed layout *)
    let root_layout = Toffee.layout tree root |> Result.get_ok in
    Printf.printf "Root: %fx%f\n" root_layout.Layout.size.width
      root_layout.size.height
    ]}

    {1 Layout Algorithms}

    Toffee implements CSS layout algorithms selected via the [display] property:

    - [Display.Flex]: Flexbox layout (CSS Flexible Box Layout Module Level 1)
    - [Display.Grid]: CSS Grid layout (CSS Grid Layout Module Level 1)
    - [Display.Block]: Block layout (CSS Box Model)
    - [Display.None]: The node and its descendants are excluded from layout

    Each algorithm has distinct behavior for sizing, positioning, and alignment.
    Refer to {!Style} for properties controlling each algorithm.

    {1 Space Constraints}

    Layout computation requires {!Available_space.t} constraints for width and
    height. These represent:

    - [Definite n]: Exactly [n] pixels available.
    - [Min_content]: Shrink-wrap to minimum intrinsic size.
    - [Max_content]: Expand to maximum intrinsic size.

    Intrinsic sizing depends on content and measure functions for leaf nodes.

    {1 Measure Functions}

    Leaf nodes without children require measure functions to compute intrinsic
    dimensions based on content (text, images, etc.). The function receives:

    - [known_dimensions]: Dimensions already resolved (e.g., from explicit
      [width]/[height]).
    - [available_space]: Space constraints from the parent.
    - [node_id]: The node being measured.
    - [context]: Custom data attached via {!new_leaf_with_context}.
    - [style]: The node's style.

    It returns the measured {!Geometry.size}. The default measure function (used
    by {!compute_layout}) returns zero size.

    Example with text measurement:

    {[
      type text_context = { content : string; font_size : float }

      let measure known_dims available _node_id context _style =
        match context with
        | None -> Geometry.Size.zero
        | Some { content; font_size } ->
            (* Compute text dimensions based on font_size and content *)
            let char_width = font_size *. 0.6 in
            let char_count = String.length content in
            Geometry.Size.{
              width = float_of_int char_count *. char_width;
              height = font_size;
            }
      in

      let tree : text_context tree = new_tree () in
      let text_node =
        new_leaf_with_context tree Style.default
          { content = "Hello"; font_size = 16. }
        |> Result.get_ok
      in
      compute_layout_with_measure tree text_node available measure
      |> Result.get_ok
    ]}

    {1 Layout Caching and Invalidation}

    The engine caches layout results to avoid redundant computation. Caches are
    invalidated automatically when styles change via {!set_style}. Manual
    invalidation uses {!mark_dirty}, which marks the node and all ancestors as
    dirty, triggering recomputation on the next {!compute_layout}.

    {1 Rounding}

    Layout values are floating-point. Enable rounding via {!enable_rounding}
    (default) to round border box dimensions and positions to integer pixels,
    reducing sub-pixel rendering artifacts. Disable via {!disable_rounding} for
    precise measurements (e.g., testing). Query rounded layouts with
    {!val-layout} or unrounded with {!unrounded_layout}.

    {1 Error Handling}

    Operations return [('a, Error.t) result]. Errors occur when:

    - Accessing invalid node indices ({!Error.Invalid_parent_node}, etc.).
    - Accessing out-of-bounds child indices
      ({!Error.Child_index_out_of_bounds}).

    All node identifiers are validated before use. Stale identifiers from
    removed nodes produce errors.

    {1 Performance Characteristics}

    - Tree creation: O(1).
    - Node creation: O(1) amortized (slot reuse with generational indices).
    - Child operations: O(n) where n is the child count (array-based storage).
    - Layout computation: O(n) where n is the total node count, assuming cache
      hits for unchanged subtrees.

    Prefer bulk operations like {!new_with_children} and {!set_children} over
    incremental {!add_child} when constructing large subtrees. *)

module Geometry = Geometry
(** Geometric primitives for dimensions, positions, and constraints.

    Provides {!Geometry.Size}, {!Geometry.Point}, {!Geometry.Rect}, and axis
    types used throughout the API. *)

module Style = Style
(** CSS style properties for layout computation.

    Defines types for [display], [position], flexbox properties ([flex_grow],
    [flex_direction], [justify_content], etc.), grid properties
    ([grid_template_columns], etc.), and dimensions ([width], [height],
    [padding], [margin], etc.). *)

open Tree
open Geometry

(** {1 Core Types} *)

module Node_id = Node_id
(** Unique identifiers for tree nodes.

    Uses generational indices to detect stale references to removed nodes. *)

module Layout = Layout
(** Computed layout data.

    Contains the final position, size, padding, border, margin, and scroll
    dimensions for a node after layout computation. *)

module Available_space = Available_space
(** Space constraints for layout computation.

    Distinguishes between definite sizes ([Definite n]), min-content, and
    max-content constraints. See
    {{:https://www.w3.org/TR/css-sizing-3/#available}CSS Sizing Level 3}. *)

module Layout_input = Layout_input
(** Input parameters for layout algorithms. *)

module Layout_output = Layout_output
(** Output produced by layout algorithms. *)

module Run_mode = Run_mode
(** Execution mode for layout computation (e.g., perform layout vs. compute
    intrinsic size). *)

module Cache = Cache
(** Layout caching for performance. *)

(** {1 Error Handling} *)

module Error : sig
  type t =
    | Child_index_out_of_bounds of {
        parent : Node_id.t;
        child_index : int;
        child_count : int;
      }
        (** [Child_index_out_of_bounds { parent; child_index; child_count }]
            indicates an attempt to access child at [child_index] in [parent],
            which has [child_count] children. Valid indices are [0] to
            [child_count - 1]. *)
    | Invalid_parent_node of Node_id.t
        (** [Invalid_parent_node node_id] indicates [node_id] does not exist in
            the tree. The node was either never created or has been removed. *)
    | Invalid_child_node of Node_id.t
        (** [Invalid_child_node node_id] indicates [node_id] does not exist in
            the tree. The node was either never created or has been removed. *)
    | Invalid_input_node of Node_id.t
        (** [Invalid_input_node node_id] indicates [node_id] does not exist in
            the tree. The node was either never created or has been removed. *)

  val to_string : t -> string
  (** [to_string error] converts [error] to a human-readable description. *)
end

type nonrec 'a result = ('a, Error.t) result
(** [('a, Error.t) result] is the result type for tree operations.

    [Ok value] on success; [Error error] when a node is invalid or an index is
    out of bounds. *)

(** {1 Tree Type and Creation} *)

type 'context tree
(** ['context tree] represents a tree of layout nodes, parameterized by the type
    of custom data associated with each node.

    Use [unit tree] if no custom data is needed. Associate context via
    {!new_leaf_with_context} and query via {!get_node_context}. *)

val new_tree : unit -> 'context tree
(** [new_tree ()] creates an empty tree with default configuration.

    Rounding is enabled by default. The tree initially has zero nodes. *)

val with_capacity : int -> 'context tree
(** [with_capacity n] creates an empty tree with pre-allocated capacity for [n]
    nodes.

    This avoids reallocation when the node count is known in advance. The tree
    grows automatically if [n] is exceeded. *)

(** {1 Configuration} *)

type config = { use_rounding : bool }
(** [config] holds layout computation options.

    - [use_rounding]: Round layout values to integers if [true]. *)

val default_config : config
(** [default_config] is [\{ use_rounding = true \}]. *)

val enable_rounding : 'context tree -> 'context tree
(** [enable_rounding tree] sets rounding to enabled.

    Layout values (positions and sizes) are rounded to the nearest integer
    pixel. This is the default and recommended for rendering to avoid sub-pixel
    artifacts. *)

val disable_rounding : 'context tree -> 'context tree
(** [disable_rounding tree] sets rounding to disabled.

    Layout values remain as computed floating-point values. Useful for testing
    or when precise fractional layouts are required. *)

(** {1 Node Creation} *)

val new_leaf : 'context tree -> Style.t -> Node_id.t result
(** [new_leaf tree style] creates a leaf node with [style] and no children.

    Returns [Ok node_id] on success. Leaf nodes require measure functions during
    layout computation to determine intrinsic size. *)

val new_leaf_with_context :
  'context tree -> Style.t -> 'context -> Node_id.t result
(** [new_leaf_with_context tree style context] creates a leaf node with [style]
    and associates [context] data.

    The [context] is accessible in measure functions via the [context]
    parameter. *)

val new_with_children :
  'context tree -> Style.t -> Node_id.t array -> Node_id.t result
(** [new_with_children tree style children] creates a node with [style] and
    [children].

    The node becomes the parent of all [children]. Returns
    [Error (Invalid_child_node id)] if any child [id] is invalid. *)

(** {1 Tree Operations} *)

val clear : 'context tree -> unit
(** [clear tree] removes all nodes from [tree].

    After [clear], all previously created node identifiers become invalid. The
    tree's capacity is preserved. *)

val remove : 'context tree -> Node_id.t -> Node_id.t result
(** [remove tree node_id] removes [node_id] and all its descendants from [tree].

    Returns [Ok node_id] on success. The removed [node_id] and all descendant
    identifiers become invalid. Returns [Error (Invalid_input_node node_id)] if
    [node_id] does not exist. *)

val total_node_count : 'context tree -> int
(** [total_node_count tree] returns the number of nodes currently in [tree].

    Includes all nodes (roots, internal nodes, and leaves). Removed nodes are
    not counted. *)

(** {1 Node Context} *)

val set_node_context :
  'context tree -> Node_id.t -> 'context option -> unit result
(** [set_node_context tree node_id context] associates [context] data with
    [node_id].

    Pass [Some context] to set or update the context; pass [None] to remove it.
    Returns [Error (Invalid_input_node node_id)] if [node_id] does not exist. *)

val get_node_context : 'context tree -> Node_id.t -> 'context option
(** [get_node_context tree node_id] returns the context data associated with
    [node_id], or [None] if no context is set.

    Returns [None] if [node_id] is invalid. *)

val get_node_context_mut : 'context tree -> Node_id.t -> 'context option
(** [get_node_context_mut tree node_id] returns the context data associated with
    [node_id], or [None] if no context is set.

    In this functional interface, this is equivalent to {!get_node_context}. The
    name reflects the original Rust API. *)

(** {1 Child Management} *)

val add_child : 'context tree -> Node_id.t -> Node_id.t -> unit result
(** [add_child tree parent child] appends [child] to [parent]'s child list.

    Returns [Ok ()] on success. Returns [Error (Invalid_parent_node parent)] if
    [parent] does not exist, or [Error (Invalid_child_node child)] if [child]
    does not exist. *)

val insert_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t -> unit result
(** [insert_child_at_index tree parent index child] inserts [child] at [index]
    in [parent]'s child list.

    Existing children at [index] and beyond are shifted right. Returns [Ok ()]
    on success. Returns [Error (Invalid_parent_node parent)] if [parent] does
    not exist, [Error (Invalid_child_node child)] if [child] does not exist, or
    [Error (Child_index_out_of_bounds { parent; child_index = index; child_count
     })] if [index > child_count]. *)

val set_children : 'context tree -> Node_id.t -> Node_id.t array -> unit result
(** [set_children tree parent children] replaces all of [parent]'s children with
    [children].

    Returns [Ok ()] on success. Returns [Error (Invalid_parent_node parent)] if
    [parent] does not exist, or [Error (Invalid_child_node child)] if any child
    in [children] does not exist. *)

val remove_child : 'context tree -> Node_id.t -> Node_id.t -> Node_id.t result
(** [remove_child tree parent child] removes [child] from [parent]'s child list.

    Returns [Ok child] on success. Returns [Error (Invalid_parent_node parent)]
    if [parent] does not exist, or [Error (Invalid_child_node child)] if [child]
    is not a child of [parent]. *)

val remove_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t result
(** [remove_child_at_index tree parent index] removes the child at [index] from
    [parent]'s child list.

    Returns [Ok child_id] where [child_id] is the removed child. Returns
    [Error (Invalid_parent_node parent)] if [parent] does not exist, or
    [Error (Child_index_out_of_bounds { parent; child_index = index; child_count
     })] if [index >= child_count]. *)

val remove_children_range :
  'context tree -> Node_id.t -> int * int -> unit result
(** [remove_children_range tree parent (start, end)] removes children at indices
    [start] through [end] (inclusive) from [parent]'s child list.

    Returns [Ok ()] on success. Returns [Error (Invalid_parent_node parent)] if
    [parent] does not exist, or
    [Error (Child_index_out_of_bounds { parent; child_index; child_count })] if
    [start > end], [start < 0], or [end >= child_count]. *)

val replace_child_at_index :
  'context tree -> Node_id.t -> int -> Node_id.t -> Node_id.t result
(** [replace_child_at_index tree parent index new_child] replaces the child at
    [index] in [parent]'s child list with [new_child].

    Returns [Ok old_child] where [old_child] is the replaced child. Returns
    [Error (Invalid_parent_node parent)] if [parent] does not exist,
    [Error (Invalid_child_node new_child)] if [new_child] does not exist, or
    [Error (Child_index_out_of_bounds { parent; child_index = index; child_count
     })] if [index >= child_count]. *)

(** {1 Tree Queries} *)

val child_at_index : 'context tree -> Node_id.t -> int -> Node_id.t result
(** [child_at_index tree parent index] returns the child at [index] in
    [parent]'s child list.

    Returns [Ok child_id] on success. Returns
    [Error (Invalid_parent_node parent)] if [parent] does not exist, or
    [Error (Child_index_out_of_bounds { parent; child_index = index; child_count
     })] if [index < 0] or [index >= child_count]. *)

val parent : 'context tree -> Node_id.t -> Node_id.t option
(** [parent tree node_id] returns the parent of [node_id], or [None] if
    [node_id] is a root or invalid.

    Nodes can have at most one parent. *)

val children : 'context tree -> Node_id.t -> Node_id.t list result
(** [children tree parent] returns the list of [parent]'s children in order.

    Returns [Ok []] if [parent] has no children. Returns
    [Error (Invalid_parent_node parent)] if [parent] does not exist. *)

(** {1 Style Management} *)

val set_style : 'context tree -> Node_id.t -> Style.t -> unit result
(** [set_style tree node_id style] sets the style of [node_id] to [style].

    This invalidates the layout cache for [node_id] and all ancestors. Returns
    [Ok ()] on success. Returns [Error (Invalid_input_node node_id)] if
    [node_id] does not exist. *)

val style : 'context tree -> Node_id.t -> Style.t result
(** [style tree node_id] returns the style of [node_id].

    Returns [Ok style] on success. Returns [Error (Invalid_input_node node_id)]
    if [node_id] does not exist. *)

(** {1 Layout} *)

val layout : 'context tree -> Node_id.t -> Layout.t result
(** [layout tree node_id] returns the computed layout of [node_id].

    Returns the rounded layout if rounding is enabled (via {!enable_rounding}),
    otherwise the unrounded layout. Returns [Ok layout] on success. Returns
    [Error (Invalid_input_node node_id)] if [node_id] does not exist.

    The layout is valid only after calling {!compute_layout} on [node_id] or an
    ancestor. Querying before computation yields a default zero layout. *)

val unrounded_layout : 'context tree -> Node_id.t -> Layout.t
(** [unrounded_layout tree node_id] returns the unrounded computed layout of
    [node_id].

    Returns the layout with precise floating-point values regardless of the
    rounding setting. Returns a default zero layout if [node_id] is invalid or
    layout has not been computed. *)

val mark_dirty : 'context tree -> Node_id.t -> unit result
(** [mark_dirty tree node_id] invalidates the layout cache for [node_id] and all
    ancestors.

    This forces recomputation during the next {!compute_layout} call. Use when
    external factors affect layout but the style has not changed (e.g., updated
    measure function data). Returns [Ok ()] on success. Returns
    [Error (Invalid_input_node node_id)] if [node_id] does not exist. *)

val dirty : 'context tree -> Node_id.t -> bool result
(** [dirty tree node_id] checks if [node_id] needs layout recomputation.

    Returns [Ok true] if the node is dirty (cache invalid); [Ok false] if clean
    (cache valid). Returns [Error (Invalid_input_node node_id)] if [node_id]
    does not exist. *)

(** {1 Layout Computation} *)

type 'context measure_function =
  float option size ->
  Available_space.t size ->
  Node_id.t ->
  'context option ->
  Style.t ->
  float size
(** ['context measure_function] computes intrinsic size for leaf nodes.

    Parameters:
    - [known_dimensions]: Dimensions already resolved from the style (e.g.,
      explicit [width] or [height]). [Some w] indicates width is known; [None]
      requires computation.
    - [available_space]: Space constraints from the parent. [Definite n]
      provides [n] pixels; [Min_content] and [Max_content] guide intrinsic
      sizing.
    - [node_id]: The node being measured.
    - [context]: Custom data associated with the node via
      {!new_leaf_with_context}, or [None].
    - [style]: The node's style.

    Returns the measured [size]. The function should respect [known_dimensions]:
    if [known_dimensions.width] is [Some w], return [size.width = w]. When both
    dimensions are known, computation may be skipped. *)

val compute_layout_with_measure :
  'context tree ->
  Node_id.t ->
  Available_space.t size ->
  'context measure_function ->
  unit result
(** [compute_layout_with_measure tree node_id available_space measure] computes
    layout for [node_id] and all descendants using [measure] for leaf nodes.

    The [available_space] specifies width and height constraints. The [measure]
    function computes intrinsic size for leaves. Returns [Ok ()] on success.
    Returns [Error (Invalid_input_node node_id)] if [node_id] does not exist.

    After computation, query layouts with {!val-layout}. *)

val compute_layout :
  'context tree -> Node_id.t -> Available_space.t size -> unit result
(** [compute_layout tree node_id available_space] computes layout for [node_id]
    and all descendants.

    Uses a default measure function that returns zero size. This is suitable for
    layouts without leaf content (e.g., pure container hierarchies). Returns
    [Ok ()] on success. Returns [Error (Invalid_input_node node_id)] if
    [node_id] does not exist. *)

(** {1 Tree Traversal Modules}

    These modules provide low-level traversal operations used by layout
    algorithms. Most users do not need these; they are exposed for advanced use
    cases and integration with custom algorithms. *)

module Traverse_partial_tree : sig
  type 'context t = 'context tree

  val child_ids : 'context t -> Node_id.t -> Node_id.t Seq.t
  (** [child_ids tree node_id] returns a sequence of [node_id]'s children. *)

  val child_count : 'context t -> Node_id.t -> int
  (** [child_count tree node_id] returns the number of children of [node_id]. *)

  val get_child_id : 'context t -> Node_id.t -> int -> Node_id.t
  (** [get_child_id tree node_id index] returns the child at [index].

      Raises [Invalid_argument] if [index] is out of bounds. *)
end

module Cache_tree : sig
  type 'context t = 'context tree

  val cache_get :
    'context t ->
    Node_id.t ->
    known_dimensions:float option size ->
    available_space:Available_space.t size ->
    run_mode:Run_mode.t ->
    Layout_output.t option
  (** [cache_get tree node_id ~known_dimensions ~available_space ~run_mode]
      retrieves the cached layout output for [node_id] under the given
      constraints.

      Returns [Some output] if a valid cached result exists; [None] if the cache
      is invalid or empty. *)

  val cache_store :
    'context t ->
    Node_id.t ->
    known_dimensions:float option size ->
    available_space:Available_space.t size ->
    run_mode:Run_mode.t ->
    Layout_output.t ->
    unit
  (** [cache_store tree node_id ~known_dimensions ~available_space ~run_mode
       output] stores [output] in the cache for [node_id] under the given
      constraints. *)

  val cache_clear : 'context t -> Node_id.t -> unit
  (** [cache_clear tree node_id] invalidates all cached layouts for [node_id].
  *)
end

module Print_tree : sig
  type 'context t = 'context tree

  val child_ids : 'context t -> Node_id.t -> Node_id.t Seq.t
  (** [child_ids tree node_id] returns a sequence of [node_id]'s children. *)

  val child_count : 'context t -> Node_id.t -> int
  (** [child_count tree node_id] returns the number of children of [node_id]. *)

  val get_child_id : 'context t -> Node_id.t -> int -> Node_id.t
  (** [get_child_id tree node_id index] returns the child at [index].

      Raises [Invalid_argument] if [index] is out of bounds. *)

  val get_debug_label : 'context t -> Node_id.t -> string
  (** [get_debug_label tree node_id] returns a debug label for [node_id].

      The label includes the node index and generation for diagnostics. *)

  val get_final_layout : 'context t -> Node_id.t -> Layout.t
  (** [get_final_layout tree node_id] returns the final computed layout for
      [node_id].

      This returns the rounded layout if rounding is enabled, otherwise the
      unrounded layout. *)
end

(** {1 Debugging} *)

val print_tree : 'context tree -> Node_id.t -> unit
(** [print_tree tree node_id] prints a debug representation of the subtree
    rooted at [node_id] to stdout.

    The output shows the tree structure with node identifiers and computed
    layouts. Useful for visualizing layout results during development. *)
