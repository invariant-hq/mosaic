(** Grid item representation for CSS Grid layout algorithm.

    This module defines the grid item type used during track sizing and
    placement. Each item represents a single child node positioned within the
    grid, with cached intrinsic size contributions.

    {1 Key Invariants}

    - [row_indexes] and [column_indexes] are initialized during placement and
      remain valid throughout layout.
    - All contribution caches ([min_content_contribution_cache], etc.) are valid
      only for a single track-sizing run.
    - [grid_area_size_cache] holds the item's grid area for the current
      track-sizing run (the tracks of the other axis as estimated for it), or is
      [None].
    - [baseline_shim] acts as additional top margin for baseline alignment.

    {1 Coordinate Systems}

    Grid items track placement in two coordinate systems:
    - Origin-zero coordinates: [row] and [column] fields, with lines numbered
      from the grid origin.
    - Track vector indices: [row_indexes] and [column_indexes], indexing into
      [GridTrack] arrays.

    {1 Cache Invalidation}

    Intrinsic size caches are tied to [grid_area_size_cache]. When the grid area
    changes, all caches must be cleared by setting [grid_area_size_cache] to
    [None]. *)

open Toffee_geometry
open Toffee_style
open Toffee_tree

type origin_zero_line = int
(** Origin-zero line coordinate type. *)

type t = {
  node : Node_id.t;
  source_order : int;
  row : origin_zero_line line;
  column : origin_zero_line line;
  is_compressible_replaced : bool;
  overflow : overflow point;
  box_sizing : box_sizing;
  size : dimension size;
  min_size : dimension size;
  max_size : dimension size;
  aspect_ratio : float option;
  padding : length_percentage rect;
  border : length_percentage rect;
  margin : length_percentage_auto rect;
  align_self : align_self;
  justify_self : justify_self;
  mutable baseline : float option;
  mutable baseline_shim : float;
  mutable row_indexes : int line;
  mutable column_indexes : int line;
  mutable crosses_flexible_row : bool;
  mutable crosses_flexible_column : bool;
  mutable crosses_intrinsic_row : bool;
  mutable crosses_intrinsic_column : bool;
  mutable grid_area_size_cache : float option size option;
  mutable min_content_contribution_cache : float option size;
  mutable minimum_contribution_cache : float option size;
  mutable max_content_contribution_cache : float option size;
  mutable y_position : float;
  mutable height : float;
}
(** Grid item representation. *)

val new_with_placement_style_and_order :
  node:Node_id.t ->
  col_span:origin_zero_line line ->
  row_span:origin_zero_line line ->
  style:Toffee_style.t ->
  parent_align_items:align_items option ->
  parent_justify_items:justify_items option ->
  source_order:int ->
  t
(** [new_with_placement_style_and_order ~node ~col_span ~row_span ~style
     ~parent_align_items ~parent_justify_items ~source_order] creates a grid
    item with concrete placement.

    Initializes style properties from [style] and sets
    [align_self]/[justify_self] to parent values if not explicitly specified,
    and, if neither is, to [Stretch], or [Start] for a replaced element. All
    index and crossing fields are set to default values and must be initialized
    later. *)

val placement : t -> abstract_axis -> origin_zero_line line
(** [placement t axis] returns the item's placement in origin-zero coordinates
    for [axis]. *)

val placement_indexes : t -> abstract_axis -> int line
(** [placement_indexes t axis] returns the item's placement as GridTrack vector
    indices for [axis]. *)

val track_range_excluding_lines : t -> abstract_axis -> int * int
(** [track_range_excluding_lines t axis] returns [(start_idx, end_idx)] covering
    all tracks the item spans in [axis], excluding bounding lines.

    Used to slice GridTrack arrays for iteration over spanned tracks. *)

val span : t -> abstract_axis -> int
(** [span t axis] returns the number of tracks the item spans in [axis]. Returns
    0 if placement is invalid. *)

val crosses_flexible_track : t -> abstract_axis -> bool
(** [crosses_flexible_track t axis] returns the pre-computed flag indicating
    whether the item crosses a flexible track in [axis]. *)

val crosses_intrinsic_track : t -> abstract_axis -> bool
(** [crosses_intrinsic_track t axis] returns the pre-computed flag indicating
    whether the item crosses an intrinsic track in [axis]. *)

val participates_in_baseline_alignment : t -> bool
(** [participates_in_baseline_alignment t] is [true] when [t] is aligned to the
    baseline and neither of its block-axis margins is auto. An item with an auto
    margin there is aligned as start instead, outside its row's baseline group.
*)

val spanned_track_limit :
  t ->
  abstract_axis ->
  Grid_track.t array ->
  float option ->
  (int -> float -> float) ->
  float option
(** [spanned_track_limit t axis axis_tracks axis_parent_size resolve_calc_value]
    computes the upper limit for the item's min/max-content contribution.

    Returns [Some limit] if the item spans only tracks with definite max track
    sizing functions, where [limit] is the sum of those maximums. Otherwise
    returns [None].

    This implements the limited min-/max-content contribution per CSS Grid
    specification. *)

val known_dimensions :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  t ->
  'tree ->
  float option size ->
  float option size
(** [known_dimensions tree t grid_area_size] computes the known_dimensions for
    child sizing, its percentages resolved against [grid_area_size].

    Applies stretch alignment to resolve percentage sizes correctly. Algorithm:

    + Resolve margins, padding, border, and box-sizing adjustments.
    + Resolve inherent size and clamp by min/max.
    + Apply stretch alignment for width if [justify_self = Stretch] and margins
      are not auto.
    + Reapply aspect ratio.
    + Apply stretch alignment for height if [align_self = Stretch] and margins
      are not auto.
    + Reapply aspect ratio and clamp by min/max.

    Returns the final known_dimensions to pass to child layout functions. *)

val spanned_fixed_track_limit :
  t ->
  abstract_axis ->
  Grid_track.t array ->
  float option ->
  (int -> float -> float) ->
  float option
(** [spanned_fixed_track_limit t axis axis_tracks axis_parent_size
     resolve_calc_value] computes the limit for automatic minimum contributions.

    Similar to {!spanned_track_limit} but excludes fit-content arguments.
    Returns [Some limit] if all spanned tracks have definite max track sizing
    functions (excluding fit-content). *)

val min_content_contribution :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  t ->
  abstract_axis ->
  'tree ->
  float option size ->
  float option size ->
  float
(** [min_content_contribution tree t axis grid_area_size available_space]
    computes the item's min-content contribution in [axis].

    Measures the child with min-content available space where [available_space]
    is unknown, its percentages resolved against [grid_area_size]. *)

val min_content_contribution_cached :
  t ->
  abstract_axis ->
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  'tree ->
  float option size ->
  float option size ->
  float
(** [min_content_contribution_cached t axis tree grid_area_size available_space]
    retrieves the cached min-content contribution or computes it.

    Cache is invalidated when [grid_area_size_cache] changes. *)

val max_content_contribution :
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  t ->
  abstract_axis ->
  'tree ->
  float option size ->
  float option size ->
  float
(** [max_content_contribution tree t axis grid_area_size available_space]
    computes the item's max-content contribution in [axis].

    Measures the child with max-content available space where [available_space]
    is unknown, its percentages resolved against [grid_area_size]. *)

val max_content_contribution_cached :
  t ->
  abstract_axis ->
  (module Toffee_tree.LAYOUT_PARTIAL_TREE with type t = 'tree) ->
  'tree ->
  float option size ->
  float option size ->
  float
(** [max_content_contribution_cached t axis tree grid_area_size available_space]
    retrieves the cached max-content contribution or computes it.

    Cache is invalidated when [grid_area_size_cache] changes. *)

val grid_area_size :
  t ->
  abstract_axis ->
  Grid_track.t array ->
  Grid_track.t array ->
  float option size ->
  (Grid_track.t -> float option -> float option) ->
  (int -> float -> float) ->
  float option size
(** [grid_area_size t axis axis_tracks other_axis_tracks available
     get_track_size_estimate calc] is the size of [t]'s grid area while [axis]
    is sized: in [axis], the sum of the spanned tracks when each has a fixed
    size, else [None]; in the other axis, the sum of the spanned tracks' sizes
    as [get_track_size_estimate] estimates them. *)
