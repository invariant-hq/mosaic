(** CSS Grid item placement algorithm.

    Implements the 8.5 Grid Item Placement Algorithm from the CSS Grid
    specification, which determines the final position of each grid item within
    the implicit grid.

    Reference:
    {{:https://www.w3.org/TR/css-grid-2/#auto-placement-algo}CSS Grid Spec §8.5}

    {1 Algorithm Overview}

    The placement algorithm processes grid items in four phases:

    + Place items with definite positions in both axes
    + Place items with definite secondary axis positions
    + Implicit grid sizing (handled by earlier estimate)
    + Auto-place remaining items

    The algorithm operates on a {!Cell_occupancy.t} matrix that tracks which
    cells are occupied and dynamically expands the implicit grid as needed. It
    respects [grid-auto-flow] settings (row/column, dense/sparse) and resolves
    named grid lines via {!Named.t}.

    {1 Coordinate Systems}

    Uses origin-zero coordinates where explicit grid tracks are numbered from 0.
    Negative implicit tracks have negative indices; positive implicit tracks
    extend beyond the explicit grid. See {!Grid_track_counts} for coordinate
    system details.

    {1 Integration}

    Called by the grid layout algorithm after computing grid size estimates.
    Populates [items] list and [cell_occupancy_matrix] with placed items. The
    cell occupancy matrix tracks both placement state (definite vs. auto) and
    implicit grid growth. *)

(** {1 Main Entry Point} *)

val place_grid_items :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  cell_occupancy_matrix:Cell_occupancy.t ->
  items:Grid_item.t list ref ->
  tree:'tree ->
  parent_node:Toffee_tree.Node_id.t ->
  grid_auto_flow:Toffee_style.grid_auto_flow ->
  align_items:Toffee_style.align_items option ->
  justify_items:Toffee_style.align_items option ->
  named_line_resolver:Named.t ->
  unit
(** [place_grid_items (module Tree) ~cell_occupancy_matrix ~items ~tree
     ~parent_node ~grid_auto_flow ~align_items ~justify_items
     ~named_line_resolver] places all grid items into the grid.

    Processes children of [parent_node] in four phases: definite placement,
    definite secondary axis, implicit grid expansion, and auto-placement.
    Mutates [cell_occupancy_matrix] to track occupancy and expand implicit grid.
    Accumulates placed items into [items] in reverse order then reverses at end.

    The algorithm:

    - Phase 1: Items with both axes definite are placed directly
    - Phase 2: Items with secondary axis definite search along primary axis
    - Phase 3: Implicit grid sizing is already handled by pre-sizing
    - Phase 4: Remaining items are auto-placed using [grid_auto_flow] strategy

    Dense auto-flow always searches from the grid start. Sparse auto-flow
    continues from the last placed item's position. Items are filtered to
    exclude [display: none] and [position: absolute] children.

    Preconditions:

    - [cell_occupancy_matrix] pre-sized via grid size estimate
    - [items] is empty or will be cleared
    - [named_line_resolver] initialized with template and explicit counts *)
