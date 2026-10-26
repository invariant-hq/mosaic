(** Leaf node layout computation.

    Computes the size of leaf nodes (nodes without children) by measuring
    content and applying CSS sizing constraints.

    {2 Algorithm}

    + Resolve percentage-based margin, padding, and border against parent's
      inline size.
    + Resolve style size constraints considering sizing mode and aspect ratio.
    + Reserve space for scrollbar gutters when [overflow] is [Scroll].
    + Early exit if both dimensions are known and margins cannot collapse.
    + Compute available space by subtracting margins and content box insets.
    + Call measure function to determine intrinsic content dimensions.
    + Combine known dimensions, style sizes, and measured sizes per CSS
      precedence.
    + Clamp to min/max constraints, apply aspect ratio.
    + Ensure size is at least padding plus border.
    + Determine margin collapsing for Block layout.

    {2 Notes}

    - Percentage padding/border resolves against parent inline size (width) per
      CSS spec.
    - Scrollbar gutters are transposed: vertical scrolling reserves horizontal
      space.
    - [Content_size] mode ignores size styles; [Inherent_size] applies all
      constraints. *)

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
    computes the size of a leaf node.

    @raise Invalid_argument if [run_mode] is [Perform_hidden_layout]. *)
