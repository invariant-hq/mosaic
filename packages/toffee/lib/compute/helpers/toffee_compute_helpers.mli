(** Shared utilities for layout algorithms.

    This module provides common functions used across Flexbox, Grid, and Block
    layout implementations. All functions are ported from taffy's
    [compute/common] module. *)

(** {1 Scrollable overflow}

    A container gathers its children's scrollable overflow
    (https://www.w3.org/TR/css-overflow-3/#scrollable) in its padding-box frame:
    positions relative to its inner border edge. *)

type overflow_acc = {
  mutable left : float;
  mutable right : float;
  mutable top : float;
  mutable bottom : float;
      (** The union of the contributions so far, a rectangle containing the
          origin. *)
  origin_x : float;
      (** The horizontal position of the container's padding-box origin in the
          frame its children are located in. *)
  origin_y : float;
      (** The vertical position of the container's padding-box origin in the
          frame its children are located in. *)
  mutable client_width : float;
      (** The width of the container's padding box, less its scrollbar: where a
          reversed horizontal scroll origin is. *)
  mutable client_height : float;
      (** The height of the container's padding box, less its scrollbar. *)
}
(** A container's scrollable overflow as its children's contributions build it,
    with the container's geometry. Its fields are all floats, so that it is a
    single block. *)

val overflow_acc :
  origin_x:float ->
  origin_y:float ->
  client_width:float ->
  client_height:float ->
  overflow_acc
(** [overflow_acc ~origin_x ~origin_y ~client_width ~client_height] is an empty
    union for a container of that geometry. *)

(** Which of a child's margins extend its margin box for a scroll container's
    end padding. *)
type overflow_margins =
  | Full_margins  (** All of them. *)
  | Negative_margins  (** Only negative ones. *)
  | No_margins  (** None. *)

type overflow_flags = int
(** A container's flags, packed in an int. *)

val overflow_flags :
  is_scroll_container:bool ->
  reversed_x:bool ->
  reversed_y:bool ->
  margins:overflow_margins ->
  overflow_flags
(** [overflow_flags ~is_scroll_container ~reversed_x ~reversed_y ~margins] are
    the flags of a container: whether it is a scroll container (which then clips
    what is before its scroll origin and extends its children's margin boxes by
    its end padding), whether its scroll origin is at the end of each axis, and
    which margins its end padding extends. *)

val with_margins : overflow_flags -> overflow_margins -> overflow_flags
(** [with_margins flags margins] is [flags] with [margins]. *)

val union :
  overflow_acc -> left:float -> right:float -> top:float -> bottom:float -> unit
(** [union acc ~left ~right ~top ~bottom] extends [acc] to the rectangle with
    those edges. *)

val add_overflow_contribution :
  overflow_acc ->
  flags:overflow_flags ->
  padding:Toffee_geometry.Rectf.t ->
  location:Toffee_geometry.Pointf.t ->
  size:Toffee_geometry.Sizef.t ->
  overflow_area:Toffee_tree.Scrollable_overflow.t ->
  border:Toffee_geometry.Rectf.t ->
  overflow:Toffee_style.Overflow.t Toffee_geometry.Point.t ->
  margin:Toffee_geometry.Rectf.t ->
  unit
(** [add_overflow_contribution acc ~flags ~padding ~location ~size
     ~overflow_area ~border ~overflow ~margin] adds to [acc] what a child at
    [location], with border-box [size], scrollable overflow [overflow_area],
    [border], [overflow] and [margin], contributes to the scrollable overflow of
    a container with [flags] and [padding].

    The child contributes its border box, and its own overflow area in each axis
    where it lets it escape: a scroll container lets none, and another box lets
    its [Visible] axes. An empty box contributes nothing, but to a scroll
    container. To a scroll container, a child wholly before its scroll origin
    contributes nothing, and a child's margin box extended by the container's
    end [padding] contributes as well. *)

val finalize_overflow :
  overflow_acc -> flags:overflow_flags -> Toffee_tree.Scrollable_overflow.t
(** [finalize_overflow acc ~flags] is the scrollable overflow area of a
    container whose children's contributions [acc] gathered. A scroll container
    keeps only what scrolling from its origin can reach, with that origin's
    directions, and has none if it and its content cover no area; another box
    keeps all of [acc], with normal origins, or none if [acc] is within its
    padding box. *)

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
