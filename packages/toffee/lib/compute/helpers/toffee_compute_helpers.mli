(** Shared utilities for layout algorithms.

    This module provides common functions used across Flexbox, Grid, and Block
    layout implementations. All functions are ported from taffy's
    [compute/common] module. *)

val compute_content_size_contribution :
  location:Toffee_geometry.Pointf.t ->
  size:Toffee_geometry.Sizef.t ->
  content_size:Toffee_geometry.Sizef.t ->
  overflow:Toffee_style.Overflow.t Toffee_geometry.Point.t ->
  Toffee_geometry.Sizef.t
(** [compute_content_size_contribution ~location ~size ~content_size ~overflow]
    computes how much a child contributes to its parent's intrinsic content
    size.

    Algorithm:

    + For each axis, if overflow is [Visible], use [max size content_size];
      otherwise use [size]
    + If both dimensions > 0, return [location + computed_size]
    + Otherwise return zero size

    Only [Visible] overflow contributes beyond border-box. Non-visible modes
    ([Clip], [Hidden], [Scroll]) clamp contribution to border-box size. *)

val apply_alignment_fallback :
  free_space:float ->
  num_items:int ->
  alignment_mode:Toffee_style.Align_content.t ->
  is_safe:bool ->
  Toffee_style.Align_content.t
(** [apply_alignment_fallback ~free_space ~num_items ~alignment_mode ~is_safe]
    resolves alignment mode fallbacks per CSS Box Alignment spec and CSSWG issue
    10154.

    Fallback rules:

    + Distributed alignments ([Stretch], [Space_between], [Space_around],
      [Space_evenly]) fall back when [num_items <= 1] or [free_space <= 0]:
      [Stretch], [Space_between] -> [Flex_start] (safe); [Space_around],
      [Space_evenly] -> [Center] (safe)
    + When [free_space <= 0] and [is_safe] is true, all modes -> [Start]

    See {{:https://www.w3.org/TR/css-align-3/} CSS Box Alignment Module}. *)

val compute_alignment_offset :
  free_space:float ->
  num_items:int ->
  gap:float ->
  alignment_mode:Toffee_style.Align_content.t ->
  layout_is_flex_reversed:bool ->
  is_first:bool ->
  float
(** [compute_alignment_offset ~free_space ~num_items ~gap ~alignment_mode
     ~layout_is_flex_reversed ~is_first] computes the positional offset for
    align-content and justify-content in both Flexbox and Grid.

    For first item ([is_first = true]):
    - [Start], [Stretch], [Space_between]: 0
    - [Flex_start]: [free_space] if reversed, else 0
    - [End]: [free_space]
    - [Flex_end]: 0 if reversed, else [free_space]
    - [Center]: [free_space / 2]
    - [Space_around]: [free_space / num_items / 2] (if positive), else
      [free_space / 2]
    - [Space_evenly]: [free_space / (num_items + 1)] (if positive), else
      [free_space / 2]

    For subsequent items ([is_first = false]):
    - All non-distributed modes: [gap + 0]
    - [Space_between]: [gap + free_space / (num_items - 1)]
    - [Space_around]: [gap + free_space / num_items]
    - [Space_evenly]: [gap + free_space / (num_items + 1)]

    Negative [free_space] is clamped to 0 for subsequent items.

    Invariant: For Grid, [gap] must be 0 as Grid handles gaps separately. *)
