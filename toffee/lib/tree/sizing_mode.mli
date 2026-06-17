(** Sizing mode for layout computation.

    Sizing mode controls whether inherent size styles (width, height, min/max
    constraints, aspect ratio) are considered during layout measurement. It is
    used in layout inputs to determine how a node's size should be computed.

    In CSS layout, certain computations need to measure content size without
    applying explicit size constraints. For example, when computing intrinsic
    sizes for CSS Grid track sizing or Flexbox item sizing, the algorithm needs
    to know the content's natural size before applying width/height styles.

    {1 Usage}

    Use [Content_size] when measuring intrinsic content dimensions for track
    sizing algorithms. Use [Inherent_size] for normal layout computation where
    all style properties apply.

    Example from CSS Grid track sizing:
    {[
      (* Measure content size without applying width/height styles *)
      let content_layout =
        Tree.compute_child_layout tree node
          (Layout_input.make
             ~run_mode:Run_mode.Compute_size
             ~sizing_mode:Sizing_mode.Content_size
             ~axis:Requested_axis.Both
             ~known_dimensions:Size.none
             ~parent_size:Size.none
             ~available_space:Size.max_content
             ~vertical_margins_are_collapsible:Line.false_)
      in
      (* Track size is based on content dimensions *)
      let track_size = Layout_output.size content_layout
    ]}

    Example from final layout:
    {[
    (* Apply all size styles during final layout *)
    let final_layout =
      Tree.compute_child_layout tree node
        (Layout_input.make ~run_mode:Run_mode.Perform_layout
           ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
           ~known_dimensions:final_size ~parent_size:container_size
           ~available_space:available
           ~vertical_margins_are_collapsible:Line.false_)
    ]} *)

type t =
  | Content_size
      (** Measure only content contributions, ignoring inherent size styles.

          The node's explicit size properties ([width], [height]), size
          constraints ([min_width], [max_width], [min_height], [max_height]),
          and [aspect_ratio] are not applied. Only the natural size of the
          content determines dimensions.

          Used during intrinsic sizing phases of CSS Grid track sizing and
          Flexbox item sizing, where the algorithm needs to measure content
          before applying size constraints. *)
  | Inherent_size
      (** Measure content with all inherent size styles applied.

          All size properties ([width], [height]), size constraints
          ([min_width], [max_width], [min_height], [max_height]), and
          [aspect_ratio] are applied to constrain or set the node's dimensions.

          This is the standard sizing mode for normal layout computation and
          final layout passes. *)

val to_string : t -> string
(** [to_string mode] returns a string representation of the sizing mode. *)

val compare : t -> t -> int
(** [compare a b] compares two sizing modes.

    Returns a negative integer if [a] is less than [b], zero if equal, and a
    positive integer if [a] is greater than [b]. The ordering is
    [Content_size < Inherent_size]. *)

val equal : t -> t -> bool
(** [equal a b] tests whether two sizing modes are equal. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt mode] prints the sizing mode to the formatter. *)
