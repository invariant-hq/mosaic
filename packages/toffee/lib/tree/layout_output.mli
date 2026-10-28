(** Layout algorithm output for a single node.

    {1 Overview}

    Layout output represents the result of computing layout for a single node.
    It contains the node's outer size, scrollable overflow, baseline positions,
    and margin collapsing data for CSS block layout. Layout algorithms return
    this structure to their parent nodes during the layout computation phase.

    {1 Key Concepts}

    {2 Outer Size vs Scrollable Overflow}

    The [size] field represents the node's outer dimensions including padding
    and borders. The [scrollable_overflow] field is the area the node's content
    reaches, relative to its padding box, which may exceed the box when the
    content overflows: a scroll container scrolls to it, and another box passes
    it on to its parent's. See {!Scrollable_overflow}.

    {2 Baselines}

    Baselines indicate the line on which text sits, used for aligning inline
    content and flex items. Text nodes and nodes containing text descendants
    typically have baselines. For nodes without baselines, use
    [Toffee_geometry.(Point.none)] for the [first_baselines] field.

    {2 Margin Collapsing}

    Margin collapsing is specific to CSS block layout, where adjacent vertical
    margins collapse according to CSS rules. The [top_margin], [bottom_margin],
    and [margins_can_collapse_through] fields track collapsible margins during
    block layout computation.

    For layout modes that do not support margin collapsing (flexbox, grid), set
    [top_margin] and [bottom_margin] to [Collapsible_margin_set.zero] and
    [margins_can_collapse_through] to [false]. *)

type t = {
  size : Toffee_geometry.Sizef.t;
      (** The computed outer size of the node's box. *)
  scrollable_overflow : Scrollable_overflow.t;
      (** The area the node's content reaches, relative to its padding box.

          This may exceed the box when content overflows, giving the scroll
          extents of a scroll container. *)
  first_baselines : float option Toffee_geometry.point;
      (** The first baseline in each dimension, if any.

          Use [Toffee_geometry.(Point.none ())] for nodes without baselines. *)
  top_margin : Collapsible_margin_set.t;
      (** Top margin available for collapsing in CSS block layout.

          Set to [Collapsible_margin_set.zero] for non-block layout modes. *)
  bottom_margin : Collapsible_margin_set.t;
      (** Bottom margin available for collapsing in CSS block layout.

          Set to [Collapsible_margin_set.zero] for non-block layout modes. *)
  margins_can_collapse_through : bool;
      (** Whether margins can collapse through this node in CSS block layout.

          Set to [false] for non-block layout modes. *)
}

val make :
  size:Toffee_geometry.Sizef.t ->
  scrollable_overflow:Scrollable_overflow.t ->
  first_baselines:float option Toffee_geometry.point ->
  top_margin:Collapsible_margin_set.t ->
  bottom_margin:Collapsible_margin_set.t ->
  margins_can_collapse_through:bool ->
  t
(** [make ~size ~scrollable_overflow ~first_baselines ~top_margin ~bottom_margin
     ~margins_can_collapse_through] creates a layout output with all fields
    specified.

    Use this constructor for CSS block layout where margin collapsing is
    relevant. For flexbox and grid layouts, prefer {!from_sizes_and_baselines}
    which sets margins to zero automatically. *)

val size : t -> Toffee_geometry.Sizef.t
(** [size t] returns the outer size of [t]. *)

val scrollable_overflow : t -> Scrollable_overflow.t
(** [scrollable_overflow t] returns the scrollable overflow of [t]. *)

val first_baselines : t -> float option Toffee_geometry.point
(** [first_baselines t] returns the first baselines of [t]. *)

val top_margin : t -> Collapsible_margin_set.t
(** [top_margin t] returns the top collapsible margin set of [t]. *)

val bottom_margin : t -> Collapsible_margin_set.t
(** [bottom_margin t] returns the bottom collapsible margin set of [t]. *)

val margins_can_collapse_through : t -> bool
(** [margins_can_collapse_through t] returns whether margins can collapse
    through [t]. *)

val hidden : t
(** [hidden] is a zero-sized layout output for nodes with [Display.None].

    All sizes are zero ([size] and [scrollable_overflow]), baselines are [None],
    top and bottom margins are zero, and margins cannot collapse through. Use
    this when a node is hidden and should not contribute to layout. *)

val default : t
(** [default] is a zero-sized default layout output.

    Identical to {!hidden}. Provided as a convenient default value. *)

val from_outer_size : Toffee_geometry.Sizef.t -> t
(** [from_outer_size size] creates a layout output from just the outer size.

    Sets [size] to the provided value and [scrollable_overflow] to zero. Use
    this when short-circuiting layout computation after determining the
    container's outer dimensions but before computing content layout. Baselines
    are [None], margins are zero, and margins cannot collapse through. *)

val from_sizes_and_baselines :
  Toffee_geometry.Sizef.t ->
  Scrollable_overflow.t ->
  float option Toffee_geometry.point ->
  t
(** [from_sizes_and_baselines size scrollable_overflow first_baselines] creates
    a layout output from sizes and baselines.

    Use this for flexbox and grid layouts after computing both outer size and
    scrollable overflow.

    Margins are set to zero and margins cannot collapse through, as flexbox and
    grid do not support margin collapsing. *)

val to_string : t -> string
(** [to_string t] converts [t] to a string representation showing size,
    scrollable_overflow, and margins_can_collapse_through. *)

val compare : t -> t -> int
(** [compare a b] compares layout outputs [a] and [b] lexicographically.

    Comparison order: [size], [scrollable_overflow], [first_baselines],
    [top_margin], [bottom_margin], [margins_can_collapse_through]. *)

val equal : t -> t -> bool
(** [equal a b] tests whether [a] and [b] are structurally equal. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints [t] to formatter [fmt] using {!to_string}. *)
