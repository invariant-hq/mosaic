(** Grid track alignment and item positioning.

    This module implements the final phase of CSS Grid layout: aligning grid
    tracks within the grid container and positioning items within their assigned
    grid areas. *)

open Toffee_geometry
open Toffee_style
open Toffee_tree

val align_tracks :
  grid_container_content_box_size:float ->
  padding:float Line.t ->
  border:float Line.t ->
  tracks:Grid_track.t array ->
  track_alignment_style:Align_content.t ->
  unit
(** [align_tracks ~grid_container_content_box_size ~padding ~border ~tracks
     ~track_alignment_style] aligns grid tracks within the container according
    to align-content (rows) or justify-content (columns).

    Mutates [tracks] in place, updating the [offset] field of each track.

    This only affects layout when the total size of tracks is smaller than the
    grid container's content box size in the alignment axis. Grid gaps are
    treated as full tracks rather than being applied during alignment. Layout is
    never reversed in grid.

    Algorithm:

    + Compute free space as
      [grid_container_content_box_size - sum(track.base_size)].
    + Count non-collapsed tracks (excluding gutters).
    + Apply alignment fallback to determine track alignment mode.
    + Iterate through tracks: skip gutters (even indices in zero-indexed array),
      compute alignment offset for non-gutter tracks using the first non-gutter
      track as reference, update [track.offset] to [origin + cumulative_offset].

    Invariants:
    - Odd tracks in the array are actual grid tracks; even tracks are gutters.
    - The first non-gutter track is at index 1.
    - Track offsets are cumulative and relative to the grid container's padding
      and border start. *)

val align_item_within_area :
  grid_area:float Line.t ->
  alignment_style:Align_items.t ->
  resolved_size:float ->
  position:Position.t ->
  inset:float option Line.t ->
  margin:float option Line.t ->
  baseline_shim:float ->
  float * float Line.t
(** [align_item_within_area ~grid_area ~alignment_style ~resolved_size ~position
     ~inset ~margin ~baseline_shim] computes the item's position and resolved
    margin along a single axis.

    Returns [(offset, resolved_margin)] where [offset] is the item's start
    position in the axis and [resolved_margin] contains the resolved start and
    end margins.

    Algorithm:

    + Calculate grid area size as [grid_area.end - grid_area.start].
    + Resolve non-auto margins and compute free space.
    + Distribute free space to auto margins if present.
    + Compute alignment-based offset using the alignment style (Start, End,
      Center, Stretch, Baseline).
    + Override with inset-based offset for absolutely positioned items if inset
      is set.
    + Apply relative positioning adjustment by adding inset.start or subtracting
      inset.end.

    The [baseline_shim] is added to the start margin to account for baseline
    alignment adjustments.

    For absolutely positioned items, inset values take precedence over
    alignment. For relatively positioned items, inset values are applied as an
    additional offset after alignment. *)

val align_and_position_item :
  (module LAYOUT_PARTIAL_TREE with type t = 'a) ->
  tree:'a ->
  node:Node_id.t ->
  order:int ->
  grid_area:Rectf.t ->
  container_alignment_styles:Align_items.t option In_both_abs_axis.t ->
  baseline_shim:float ->
  overflow_acc:Toffee_compute_helpers.overflow_acc ->
  overflow_flags:Toffee_compute_helpers.overflow_flags ->
  overflow_origin:Rectf.t ->
  float * float
(** [align_and_position_item (module Tree) ~tree ~node ~order ~grid_area
     ~container_alignment_styles ~baseline_shim ~overflow_acc ~overflow_flags
     ~overflow_origin] aligns and positions a grid item within its assigned grid
    area, and adds it to the container's scrollable overflow in [overflow_acc].

    Returns [(y_position, height)] for baseline alignment computation.

    Algorithm:

    + Resolve item styles (size, min/max size, margins, padding, border, inset,
      alignment).
    + Apply box-sizing adjustment to convert content-box sizes to border-box.
    + Determine alignment styles, defaulting to Start for items with inherent
      size or aspect ratio, Stretch otherwise.
    + Resolve width from inset or stretch alignment for absolutely positioned
      items or items with stretch alignment and no auto margins.
    + Apply aspect ratio after width resolution.
    + Resolve height similarly to width.
    + Clamp size to min/max constraints.
    + For absolutely positioned items with unresolved dimensions, perform a
      sizing pass to measure the item.
    + Perform layout on the item with the final known dimensions.
    + Align the item horizontally and vertically within the grid area using
      [align_item_within_area].
    + Compute scrollbar size based on overflow properties.
    + Set the item's unrounded layout with computed position, size, padding,
      border, and margin.

    Invariants:
    - Both horizontal and vertical margins resolve against the WIDTH of the grid
      area per CSS spec.
    - Default alignment is Start if the item has an inherent size or aspect
      ratio; otherwise Stretch.
    - For stretch alignment to apply, the item must not be absolutely positioned
      and must not have auto margins in the alignment axis.

    Integration:
    - Calls [Toffee_tree.compute_child_layout] for sizing and layout passes.
    - Mutates layout tree via [Toffee_tree.set_unrounded_layout]. *)
