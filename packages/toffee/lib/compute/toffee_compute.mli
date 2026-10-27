(** CSS layout algorithm orchestration.

    This module provides the entry points for CSS layout computation in toffee.
    It exports functions for computing layout across different display modes
    (Flexbox, Grid, Block) and managing the layout pipeline (caching, rounding,
    hidden nodes).

    {1 Layout Functions}

    Layout functions operate on trees implementing the
    {!Toffee_tree.LAYOUT_PARTIAL_TREE} signature, which provides access to a
    container node and its immediate children. Each function computes layout for
    a single node and returns a {!Toffee_tree.Layout_output} describing the
    node's computed size and margins.

    - {!compute_flexbox_layout}: Layout a Flexbox container
    - {!compute_grid_layout}: Layout a CSS Grid container
    - {!compute_block_layout}: Layout a Block container
    - {!compute_leaf_layout}: Layout a leaf node (text, image, etc.)
    - {!compute_root_layout}: Layout the root node of a tree

    {1 Utility Functions}

    - {!compute_cached_layout}: Wrap layout computation with caching
    - {!round_layout}: Round float layouts to integer pixels
    - {!compute_hidden_layout}: Mark nodes as hidden ([Display.None])

    {1 Usage}

    Layout proceeds top-down. Call {!compute_root_layout} on the root node with
    available space constraints. It recursively computes child layouts by
    calling [Toffee_tree.compute_child_layout], which dispatches to the
    appropriate algorithm based on each node's [display] style.

    After layout completes, call {!round_layout} to snap float coordinates to
    exact pixels, eliminating sub-pixel gaps. *)

val compute_leaf_layout :
  inputs:Toffee_tree.Layout_input.t ->
  style:Toffee_style.t ->
  resolve_calc_value:Toffee_style.calc_resolver ->
  measure_function:
    (float option Toffee_geometry.size ->
    Toffee_tree.Available_space.t Toffee_geometry.size ->
    Toffee_geometry.Sizef.t) ->
  Toffee_tree.Layout_output.t
(** [compute_leaf_layout ~inputs ~style ~resolve_calc_value ~measure_function]
    computes layout for a leaf node.

    Leaf nodes have no children and determine their size via measurement (text,
    images) or intrinsic dimensions. This function applies box model properties
    (padding, border, margin, box-sizing), resolves size styles against
    available space, and invokes [measure_function] to determine content size.

    Parameters:

    - [inputs]: Layout constraints from the parent
    - [style]: Node's style properties
    - [resolve_calc_value]: Resolver for CSS [calc()] expressions
    - [measure_function]: Closure computing content size given
      [known_dimensions] and [available_space]. For text nodes, this measures
      text; for images, it returns intrinsic dimensions.

    The function handles:

    - Padding, border, and margin resolution against parent width
    - Box-sizing adjustment ([Content_box] vs [Border_box])
    - Aspect ratio application
    - Min/max size constraints
    - Scrollbar gutter reservation for [Overflow.Scroll]
    - Early return when both dimensions are known and margins cannot collapse

    Scrollbar gutters are transposed: vertical scrolling reserves horizontal
    space for the scrollbar.

    For Block layout, the function determines whether margins can collapse
    through the node based on padding, border, overflow, position, and height
    styles. *)

val compute_block_layout :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  'a ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Layout_input.t ->
  Toffee_tree.Layout_output.t
(** [compute_block_layout (module Tree) tree node inputs] computes CSS Block
    layout for [node] and its children.

    Block layout arranges children vertically, collapsing adjacent vertical
    margins according to CSS rules. Block containers automatically stretch to
    fill available width when width is definite.

    This implements the CSS Block Formatting Context algorithm, including:

    - Vertical stacking of block-level children
    - Margin collapsing for adjacent vertical margins
    - Auto-stretching width to fill definite available space
    - Handling of floats and clearance (if supported)

    Returns a {!Toffee_tree.Layout_output} with the container's size, content
    size, and collapsible margin sets for further margin collapsing with
    siblings. *)

val compute_flexbox_layout :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  'a ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Layout_input.t ->
  Toffee_tree.Layout_output.t
(** [compute_flexbox_layout (module Tree) tree node inputs] computes CSS Flexbox
    layout for [node] and its children.

    Flexbox arranges children along a main axis with flexible sizing and
    alignment. This implements the CSS Flexible Box Layout algorithm, including:

    - Main axis and cross axis sizing
    - Flex grow and shrink distribution
    - Baseline alignment and item positioning
    - Wrapping and multi-line layout
    - Gap spacing between items

    Returns a {!Toffee_tree.Layout_output} with the container's size and content
    size. Margins do not collapse in Flexbox. *)

val compute_grid_layout :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'a) ->
  tree:'a ->
  node:Toffee_tree.Node_id.t ->
  inputs:Toffee_tree.Layout_input.t ->
  Toffee_tree.Layout_output.t
(** [compute_grid_layout (module Tree) ~tree ~node ~inputs] computes CSS Grid
    layout for [node] and its children.

    Grid layout arranges children in a two-dimensional grid with explicit track
    sizing and item placement. This implements the CSS Grid Layout algorithm,
    including:

    - Track (row/column) sizing with [fr] units, [auto], and fixed sizes
    - Implicit grid generation for auto-placed items
    - Item spanning across multiple tracks
    - Gap spacing between tracks
    - Alignment within grid areas

    Returns a {!Toffee_tree.Layout_output} with the container's size and content
    size. Margins do not collapse in Grid. *)

val compute_root_layout :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Available_space.t Toffee_geometry.size ->
  unit
(** [compute_root_layout (module Tree) tree root available_space] computes
    layout for the root node of a tree.

    This is the entry point for layout computation. It resolves root node styles
    against available space, performs recursive child layout, and stores the
    final layout (including padding, border, margin, scrollbar size) via
    [Toffee_tree.set_unrounded_layout].

    For Block display, the root node automatically stretches its width to fill
    definite available space after accounting for margins.

    The root node is always placed at the origin [(0, 0)] with [order = 0].

    Call this function once per layout pass to begin top-down layout traversal.
*)

val compute_cached_layout :
  (module Toffee_tree.CACHE_TREE with type t = 't) ->
  't ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Layout_input.t ->
  ('t ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Layout_input.t ->
  Toffee_tree.Layout_output.t) ->
  Toffee_tree.Layout_output.t
(** [compute_cached_layout (module Tree) tree node inputs compute_uncached]
    retrieves cached layout or computes and caches it.

    This function checks the layout cache for a result matching [inputs]'s
    [known_dimensions], [available_space], and [run_mode]. If found, it returns
    the cached {!Toffee_tree.Layout_output}. Otherwise, it invokes
    [compute_uncached] to compute layout, stores the result in the cache via
    [Toffee_tree.cache_store], and returns it.

    Caching eliminates redundant layout computation when a node is measured
    multiple times with identical constraints, which occurs during:

    - Flexbox flex-basis resolution
    - Grid intrinsic sizing passes
    - Multi-pass layout algorithms

    Entries are keyed on [inputs] (see {!Toffee_tree.Cache}). Changes to node
    style or structure require calling [Toffee_tree.cache_clear] to invalidate
    stale entries.

    This function is typically called by [Toffee_tree.compute_child_layout]
    implementations to wrap algorithm-specific layout functions. *)

val round_layout :
  (module Toffee_tree.ROUND_TREE with type t = 't) ->
  't ->
  Toffee_tree.Node_id.t ->
  unit
(** [round_layout (module Tree) tree node] rounds float layouts to integer
    pixels recursively.

    Float-valued layout coordinates can produce sub-pixel gaps when rendered.
    This function snaps coordinates and dimensions to exact pixels, preserving
    alignment and eliminating visual artifacts.

    Algorithm:

    - Round based on cumulative coordinates relative to the viewport, not
      parent-relative coordinates. A location is the rounded cumulative position
      of the node minus its parent's, so the sum of the locations from the root
      is the node's rounded cumulative position.
    - Compute width/height by rounding left/right and top/bottom edges, then
      taking the difference, rather than rounding width/height directly. This
      prevents accumulated rounding errors from creating gaps.

    The function reads from [Toffee_tree.get_unrounded_layout] and writes to
    [Toffee_tree.set_final_layout] to avoid re-rounding already-rounded values.

    See
    {{:https://github.com/facebook/yoga/commit/aa5b296ac78f7a22e1aeaf4891243c6bb76488e2}
     Yoga's rounding commit} for detailed rationale.

    A subtree for which [Tree.is_rounded] holds at its parent's position is
    skipped: its stored result is what rounding would produce, so the cost of a
    pass follows the nodes whose unrounded layout or position changed.

    Call this function after layout completes to prepare coordinates for
    rendering. *)

(** Combined signature for trees supporting both layout computation and caching.

    Required by {!compute_hidden_layout}, which clears cache and performs
    layout. *)
module type CACHE_LAYOUT_PARTIAL_TREE = sig
  include Toffee_tree.LAYOUT_PARTIAL_TREE
  include Toffee_tree.CACHE_TREE with type t := t
end

val compute_hidden_layout :
  (module CACHE_LAYOUT_PARTIAL_TREE with type t = 't) ->
  't ->
  Toffee_tree.Node_id.t ->
  Toffee_tree.Layout_output.t
(** [compute_hidden_layout (module Tree) tree node] marks [node] and its
    descendants as hidden recursively.

    Nodes with [Display.None] receive zero-sized layouts at the origin and do
    not participate in layout. This function:

    - Clears the layout cache for [node] via [Toffee_tree.cache_clear]
    - Sets [node]'s layout to zero size at the origin with [order = 0]
    - Recursively processes all children with {!Toffee_tree.Layout_input.hidden}

    Returns {!Toffee_tree.Layout_output.hidden}, a zero-sized output with no
    baselines or collapsible margins.

    Call this when a node's [display] style is [Display.None]. The function
    propagates hidden state to all descendants regardless of their [display]
    styles, matching CSS behavior. *)
