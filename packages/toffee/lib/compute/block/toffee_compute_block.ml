(* Block layout computation algorithm *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

(* Helper module for Option<float> operations used in layout *)
module Helpers = struct
  let maybe_clamp value min max =
    match (min, max) with
    | Some min_val, Some max_val -> Float.max min_val (Float.min max_val value)
    | Some min_val, None -> Float.max min_val value
    | None, Some max_val -> Float.min max_val value
    | None, None -> value
end

(* Helper to determine if a style represents a table *)
let is_table (style : Style.t) : bool = Style.item_is_table style

(* Per-child data that is accumulated and modified over the course of the layout
   algorithm *)
type block_item = {
  node_id : Node_id.t; (* The identifier for the associated node *)
  order : int;
      (* The "source order" of the item. This is the index of the item within
         the children iterator, and controls the order in which the nodes are
         placed *)
  is_table : bool;
      (* Items that are tables don't have stretch sizing applied to them *)
  size : float option size; (* The base size of this item *)
  min_size : float option size; (* The minimum allowable size of this item *)
  max_size : float option size; (* The maximum allowable size of this item *)
  overflow : overflow point; (* The overflow style of the item *)
  scrollbar_width : float;
      (* The width of the item's scrollbars (if it has scrollbars) *)
  position : position; (* The position style of the item *)
  inset : length_percentage_auto rect; (* The final offset of this item *)
  margin : length_percentage_auto rect; (* The margin of this item *)
  padding : float rect; (* The padding of this item *)
  border : float rect; (* The border of this item *)
  padding_border_sum : float size;
      (* The sum of padding and border for this item *)
  mutable static_position : float point;
      (* The computed "static position" of this item. The static position is the
         position taking into account padding, border, margins, and
         scrollbar_gutters but not inset *)
  mutable can_be_collapsed_through : bool;
      (* Whether margins can be collapsed through this item *)
}

(* Generate a list of block items from the children of a node *)
let generate_item_list (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (node_inner_size : float option size) : block_item list =
  (* Get calc resolver from tree *)
  let calc = Tree.resolve_calc_value tree in

  let child_count = Tree.child_count tree node in
  let items = ref [] in
  let order = ref 0 in
  for i = 0 to child_count - 1 do
    let child_node_id = Tree.get_child_id tree node i in
    let child_style = Tree.get_core_container_style tree child_node_id in
    if Style.box_generation_mode child_style <> Box_generation_mode.None then (
      let aspect_ratio = Style.aspect_ratio child_style in
      let padding =
        Style.padding child_style
        |> Length_percentage.resolve_rect_or_zero node_inner_size.width calc
      in
      let border =
        Style.border child_style
        |> Length_percentage.resolve_rect_or_zero node_inner_size.width calc
      in
      let pb_sum = Rect.sum_axes (Rect.add padding border) in
      let box_sizing_adjustment =
        if Style.box_sizing child_style = Box_sizing.Content_box then pb_sum
        else Size.zero
      in

      let item =
        {
          node_id = child_node_id;
          order = !order;
          is_table = is_table child_style;
          size =
            (let size_dimensions = Style.size child_style in
             let resolved_size : float option size =
               {
                 width =
                   Dimension.maybe_resolve
                     (Size.get Inline size_dimensions)
                     node_inner_size.width calc;
                 height =
                   Dimension.maybe_resolve
                     (Size.get Block size_dimensions)
                     node_inner_size.height calc;
               }
             in
             resolved_size
             |> Size.apply_aspect_ratio aspect_ratio
             |> Size.maybe_add box_sizing_adjustment);
          min_size =
            (let min_size_dimensions = Style.min_size child_style in
             let resolved_min_size : float option size =
               {
                 width =
                   Dimension.maybe_resolve
                     (Size.get Inline min_size_dimensions)
                     node_inner_size.width calc;
                 height =
                   Dimension.maybe_resolve
                     (Size.get Block min_size_dimensions)
                     node_inner_size.height calc;
               }
             in
             resolved_min_size
             |> Size.apply_aspect_ratio aspect_ratio
             |> Size.maybe_add box_sizing_adjustment);
          max_size =
            (let max_size_dimensions = Style.max_size child_style in
             let resolved_max_size : float option size =
               {
                 width =
                   Dimension.maybe_resolve
                     (Size.get Inline max_size_dimensions)
                     node_inner_size.width calc;
                 height =
                   Dimension.maybe_resolve
                     (Size.get Block max_size_dimensions)
                     node_inner_size.height calc;
               }
             in
             resolved_max_size
             |> Size.apply_aspect_ratio aspect_ratio
             |> Size.maybe_add box_sizing_adjustment);
          overflow = Style.overflow child_style;
          scrollbar_width = Style.scrollbar_width child_style;
          position = Style.position child_style;
          inset = Style.inset child_style;
          margin = Style.margin child_style;
          padding;
          border;
          padding_border_sum = pb_sum;
          (* Fields to be computed later (for now we initialise with dummy
             values) *)
          static_position = Point.zero;
          can_be_collapsed_through = false;
        }
      in
      items := item :: !items;
      order := !order + 1)
  done;
  List.rev !items

(* Compute the content-based width when the container width is not known *)
let determine_content_based_container_width (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (items : block_item list) (available_width : Available_space.t) : float =
  let available_space =
    Size.{ width = available_width; height = Available_space.min_content }
  in

  let max_child_width = ref 0.0 in
  List.iter
    (fun item ->
      if item.position <> Position.Absolute then
        let known_dimensions =
          Size.clamp_option item.min_size item.max_size item.size
        in

        let width =
          match known_dimensions.width with
          | Some w -> w
          | None ->
              (* Get calc resolver from tree *)
              let calc = Tree.resolve_calc_value tree in
              let item_x_margin_sum =
                let parent_width =
                  Available_space.to_option available_space.width
                in
                let margin_left =
                  Length_percentage_auto.resolve_or_zero item.margin.left
                    parent_width calc
                in
                let margin_right =
                  Length_percentage_auto.resolve_or_zero item.margin.right
                    parent_width calc
                in
                margin_left +. margin_right
              in

              let available_space_for_child =
                Size.
                  {
                    width =
                      Available_space.sub_or_zero available_space.width
                        (Some item_x_margin_sum);
                    height = available_space.height;
                  }
              in

              let size_and_baselines =
                Tree.compute_child_layout tree item.node_id
                  (Layout_input.make ~run_mode:Run_mode.Perform_layout
                     ~sizing_mode:Sizing_mode.Inherent_size
                     ~axis:Requested_axis.Horizontal ~known_dimensions
                     ~parent_size:Size.none
                     ~available_space:available_space_for_child
                     ~vertical_margins_are_collapsible:
                       { start = true; end_ = true })
              in

              (Layout_output.size size_and_baselines).width +. item_x_margin_sum
        in

        let width = Float.max width item.padding_border_sum.width in
        max_child_width := Float.max !max_child_width width)
    items;

  !max_child_width

(* Perform final layout on in-flow children *)
let perform_final_layout_on_in_flow_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (items : block_item list) (container_outer_width : float)
    (content_box_inset : float rect) (resolved_content_box_inset : float rect)
    (text_align : Text_align.t) (own_margins_collapse_with_children : bool line)
    : float size * float * Collapsible_margin_set.t * Collapsible_margin_set.t =
  (* Resolve container_inner_width for sizing child nodes using initial
     content_box_inset *)
  let container_inner_width =
    container_outer_width -. Rect.horizontal_axis_sum content_box_inset
  in
  let parent_size =
    Size.{ width = Some container_outer_width; height = None }
  in
  (* Vertical available space in block flow is indefinite, not a min-content
     constraint: Max_content is how toffee says indefinite. Under Min_content
     every descendant grid believed it was sized under a min-content
     constraint, in which maximizing the tracks has no free space, so auto rows
     holding only scroll containers (whose automatic minimum size is 0)
     collapsed to zero height. Browsers size such rows to their content. *)
  let available_space =
    Size.
      {
        width = Available_space.of_float container_inner_width;
        height = Available_space.max_content;
      }
  in

  let inflow_content_size = ref Size.zero in
  let committed_y_offset = ref resolved_content_box_inset.top in
  let y_offset_for_absolute = ref resolved_content_box_inset.top in
  let first_child_top_margin_set = ref Collapsible_margin_set.zero in
  let active_collapsible_margin_set = ref Collapsible_margin_set.zero in
  let is_collapsing_with_first_margin_set = ref true in

  let calc = Tree.resolve_calc_value tree in

  List.iter
    (fun item ->
      if item.position = Position.Absolute then
        (* For absolute items, only set static position *)
        item.static_position <-
          Point.
            { x = resolved_content_box_inset.left; y = !y_offset_for_absolute }
      else
        (* Handle in-flow items *)
        let item_margin =
          item.margin
          |> Rect.map (fun margin ->
              Length_percentage_auto.resolve_to_option_with_calc margin
                container_outer_width calc)
        in
        let item_non_auto_margin =
          item_margin |> Rect.map (fun m -> Option.value m ~default:0.0)
        in
        let item_non_auto_x_margin_sum =
          Rect.horizontal_axis_sum item_non_auto_margin
        in

        let known_dimensions =
          if item.is_table then Size.none
          else
            item.size
            |> Size.map_width (fun width ->
                (* TODO: Allow stretch-sizing to be conditional, as there are
                   exceptions. e.g. Table children of blocks do not stretch
                   fit *)
                match width with
                | Some w ->
                    Some
                      (Helpers.maybe_clamp w item.min_size.width
                         item.max_size.width)
                | None ->
                    let stretched_width =
                      container_inner_width -. item_non_auto_x_margin_sum
                    in
                    Some
                      (Helpers.maybe_clamp stretched_width item.min_size.width
                         item.max_size.width))
            |> Size.clamp_option item.min_size item.max_size
        in

        let item_layout =
          Tree.compute_child_layout tree item.node_id
            (Layout_input.make ~run_mode:Run_mode.Perform_layout
               ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
               ~known_dimensions
               ~parent_size:
                 Size.
                   {
                     width = Some container_outer_width;
                     height = parent_size.height;
                   }
               ~available_space:
                 Size.
                   {
                     width =
                       Available_space.sub_or_zero available_space.width
                         (Some item_non_auto_x_margin_sum);
                     height = available_space.height;
                   }
               ~vertical_margins_are_collapsible:Line.both_true)
        in
        let final_size = Layout_output.size item_layout in

        let top_margin_set =
          Collapsible_margin_set.collapse_with_margin
            (Layout_output.top_margin item_layout)
            (Option.value item_margin.top ~default:0.0)
        in
        let bottom_margin_set =
          Collapsible_margin_set.collapse_with_margin
            (Layout_output.bottom_margin item_layout)
            (Option.value item_margin.bottom ~default:0.0)
        in

        (* Expand auto margins to fill available space *)
        (* Note: Vertical auto-margins for relatively positioned block items simply resolve to 0. *)
        (* See: https://www.w3.org/TR/CSS21/visudet.html#abs-non-replaced-width *)
        let free_x_space =
          Float.max 0.0
            (container_inner_width -. final_size.width
           -. item_non_auto_x_margin_sum)
        in
        let x_axis_auto_margin_size =
          let auto_margin_count =
            (if Option.is_none item_margin.left then 1 else 0)
            + if Option.is_none item_margin.right then 1 else 0
          in
          if auto_margin_count > 0 then
            free_x_space /. float_of_int auto_margin_count
          else 0.0
        in
        let resolved_margin =
          Rect.
            {
              left =
                Option.value item_margin.left ~default:x_axis_auto_margin_size;
              right =
                Option.value item_margin.right ~default:x_axis_auto_margin_size;
              top = Collapsible_margin_set.resolve top_margin_set;
              bottom = Collapsible_margin_set.resolve bottom_margin_set;
            }
        in

        (* Resolve item inset *)
        let inset =
          Rect.zip_size
            Size.{ width = container_inner_width; height = 0.0 }
            (fun p s -> Length_percentage_auto.maybe_resolve p (Some s) calc)
            item.inset
        in
        let inset_offset =
          Point.
            {
              x =
                (match inset.left with
                | Some l -> l
                | None -> (
                    match inset.right with Some r -> -.r | None -> 0.0));
              y =
                (match inset.top with
                | Some t -> t
                | None -> (
                    match inset.bottom with Some b -> -.b | None -> 0.0));
            }
        in

        let y_margin_offset =
          if
            !is_collapsing_with_first_margin_set
            && own_margins_collapse_with_children.start
          then 0.0
          else
            Collapsible_margin_set.resolve
              (Collapsible_margin_set.collapse_with_margin
                 !active_collapsible_margin_set
                 resolved_margin.top)
        in

        item.can_be_collapsed_through <-
          Layout_output.margins_can_collapse_through item_layout;
        item.static_position <-
          Point.
            {
              x = resolved_content_box_inset.left;
              y =
                !committed_y_offset
                +. Collapsible_margin_set.resolve !active_collapsible_margin_set;
            };
        let location =
          Point.
            {
              x =
                resolved_content_box_inset.left +. inset_offset.x
                +. resolved_margin.left;
              y = !committed_y_offset +. inset_offset.y +. y_margin_offset;
            }
        in

        (* Apply alignment *)
        let item_outer_width =
          final_size.width +. Rect.horizontal_axis_sum resolved_margin
        in
        let location =
          if item_outer_width < container_inner_width then
            match text_align with
            | Text_align.Auto -> location
            | Text_align.Legacy_left -> location (* Left aligned by default *)
            | Text_align.Legacy_right ->
                Point.
                  {
                    location with
                    x = location.x +. (container_inner_width -. item_outer_width);
                  }
            | Text_align.Legacy_center ->
                Point.
                  {
                    location with
                    x =
                      location.x
                      +. ((container_inner_width -. item_outer_width) /. 2.0);
                  }
          else location
        in

        let scrollbar_size =
          Size.
            {
              width =
                (if item.overflow.y = Overflow.Scroll then item.scrollbar_width
                 else 0.0);
              height =
                (if item.overflow.x = Overflow.Scroll then item.scrollbar_width
                 else 0.0);
            }
        in

        Tree.set_unrounded_layout tree item.node_id
          (Layout.make ~order:item.order ~size:final_size
             ~content_size:(Layout_output.content_size item_layout)
             ~scrollbar_size ~location ~padding:item.padding ~border:item.border
             ~margin:resolved_margin);

        (* Update inflow_content_size *)
        inflow_content_size :=
          Size.max !inflow_content_size
            (Toffee_compute_helpers.compute_content_size_contribution ~location
               ~size:final_size
               ~content_size:(Layout_output.content_size item_layout)
               ~overflow:item.overflow);

        (* Update first_child_top_margin_set *)
        if !is_collapsing_with_first_margin_set then
          if item.can_be_collapsed_through then
            first_child_top_margin_set :=
              !first_child_top_margin_set
              |> Collapsible_margin_set.collapse_with_set top_margin_set
              |> Collapsible_margin_set.collapse_with_set bottom_margin_set
          else (
            first_child_top_margin_set :=
              Collapsible_margin_set.collapse_with_set
                !first_child_top_margin_set
                top_margin_set;
            is_collapsing_with_first_margin_set := false);

        (* Update active_collapsible_margin_set *)
        if item.can_be_collapsed_through then (
          active_collapsible_margin_set :=
            !active_collapsible_margin_set
            |> Collapsible_margin_set.collapse_with_set top_margin_set
            |> Collapsible_margin_set.collapse_with_set bottom_margin_set;
          y_offset_for_absolute :=
            !committed_y_offset +. final_size.height +. y_margin_offset)
        else (
          committed_y_offset :=
            !committed_y_offset +. final_size.height +. y_margin_offset;
          active_collapsible_margin_set := bottom_margin_set;
          y_offset_for_absolute :=
            !committed_y_offset
            +. Collapsible_margin_set.resolve !active_collapsible_margin_set))
    items;

  let last_child_bottom_margin_set = !active_collapsible_margin_set in
  let bottom_y_margin_offset =
    if own_margins_collapse_with_children.end_ then 0.0
    else Collapsible_margin_set.resolve last_child_bottom_margin_set
  in

  committed_y_offset :=
    !committed_y_offset +. resolved_content_box_inset.bottom
    +. bottom_y_margin_offset;
  let content_height = Float.max 0.0 !committed_y_offset in
  ( !inflow_content_size,
    content_height,
    !first_child_top_margin_set,
    last_child_bottom_margin_set )

(* Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (items : block_item list) (area_size : float size)
    (area_offset : float point) : float size =
  let area_width = area_size.width in
  let area_height = area_size.height in

  let absolute_content_size = ref Size.zero in
  let calc = Tree.resolve_calc_value tree in

  List.iter
    (fun item ->
      if item.position = Position.Absolute then
        let child_style = Tree.get_core_container_style tree item.node_id in

        (* Skip items that are display:none or are not position:absolute *)
        if
          Style.box_generation_mode child_style <> Box_generation_mode.None
          && Style.position child_style = Position.Absolute
        then (
          let aspect_ratio = Style.aspect_ratio child_style in
          let margin =
            Style.margin child_style
            |> Rect.map (fun margin ->
                Length_percentage_auto.resolve_to_option_with_calc margin
                  area_width calc)
          in
          let padding =
            Style.padding child_style
            |> Length_percentage.resolve_rect_or_zero (Some area_width) calc
          in
          let border =
            Style.border child_style
            |> Length_percentage.resolve_rect_or_zero (Some area_width) calc
          in
          let padding_border_sum = Rect.sum_axes (Rect.add padding border) in
          let box_sizing_adjustment =
            if Style.box_sizing child_style = Box_sizing.Content_box then
              padding_border_sum
            else Size.zero
          in

          (* Resolve inset *)
          let inset = Style.inset child_style in
          let left =
            Length_percentage_auto.maybe_resolve inset.left (Some area_width)
              calc
          in
          let right =
            Length_percentage_auto.maybe_resolve inset.right (Some area_width)
              calc
          in
          let top =
            Length_percentage_auto.maybe_resolve inset.top (Some area_height)
              calc
          in
          let bottom =
            Length_percentage_auto.maybe_resolve inset.bottom (Some area_height)
              calc
          in

          (* Compute known dimensions from min/max/inherent size styles *)
          let style_size =
            Style.size child_style |> fun dims ->
            Size.
              {
                width =
                  Dimension.maybe_resolve (Size.get Inline dims)
                    (Some area_width) calc;
                height =
                  Dimension.maybe_resolve (Size.get Block dims)
                    (Some area_height) calc;
              }
            |> Size.apply_aspect_ratio aspect_ratio
            |> Size.maybe_add box_sizing_adjustment
          in
          let resolved_min_size =
            Style.min_size child_style |> fun dims ->
            Size.
              {
                width =
                  Dimension.maybe_resolve (Size.get Inline dims)
                    (Some area_width) calc;
                height =
                  Dimension.maybe_resolve (Size.get Block dims)
                    (Some area_height) calc;
              }
            |> Size.apply_aspect_ratio aspect_ratio
            |> Size.maybe_add box_sizing_adjustment
          in
          let min_size =
            resolved_min_size |> fun size ->
            Size.choose_first size (Size.map Option.some padding_border_sum)
            |> Size.maybe_max padding_border_sum
          in
          let max_size =
            Style.max_size child_style |> fun dims ->
            Size.
              {
                width =
                  Dimension.maybe_resolve (Size.get Inline dims)
                    (Some area_width) calc;
                height =
                  Dimension.maybe_resolve (Size.get Block dims)
                    (Some area_height) calc;
              }
            |> Size.apply_aspect_ratio aspect_ratio
            |> Size.maybe_add box_sizing_adjustment
          in
          (* If both min and max in a given axis are set and max <= min then
             this determines the size in that axis *)
          let min_max_definite_size =
            Size.map2
              (fun min max ->
                match (min, max) with
                | Some min_v, Some max_v when max_v <= min_v -> Some min_v
                | _ -> None)
              min_size max_size
          in
          let known_dimensions =
            ref
              (Size.choose_first min_max_definite_size
                 (Size.clamp_option min_size max_size style_size))
          in

          (* Fill in width from left/right and reapply aspect ratio if: - Width
             is not already known - Item has both left and right inset
             properties set *)
          (match (!known_dimensions.width, left, right) with
          | None, Some l, Some r ->
              let new_width_raw =
                area_width
                -. Option.value margin.left ~default:0.0
                -. Option.value margin.right ~default:0.0
                -. l -. r
              in
              known_dimensions :=
                {
                  !known_dimensions with
                  width = Some (Float.max new_width_raw 0.0);
                };
              known_dimensions :=
                !known_dimensions
                |> Size.apply_aspect_ratio aspect_ratio
                |> Size.clamp_option min_size max_size
                |> Size.choose_first min_max_definite_size
          | _ -> ());

          (* Fill in height from top/bottom and reapply aspect ratio if: -
             Height is not already known - Item has both top and bottom inset
             properties set *)
          (match (!known_dimensions.height, top, bottom) with
          | None, Some t, Some b ->
              let new_height_raw =
                area_height
                -. Option.value margin.top ~default:0.0
                -. Option.value margin.bottom ~default:0.0
                -. t -. b
              in
              known_dimensions :=
                {
                  !known_dimensions with
                  height = Some (Float.max new_height_raw 0.0);
                };
              known_dimensions :=
                !known_dimensions
                |> Size.apply_aspect_ratio aspect_ratio
                |> Size.clamp_option min_size max_size
                |> Size.choose_first min_max_definite_size
          | _ -> ());

          let measured_size =
            Tree.compute_child_layout tree item.node_id
              (Layout_input.make ~run_mode:Run_mode.Compute_size
                 ~sizing_mode:Sizing_mode.Content_size ~axis:Requested_axis.Both
                 ~known_dimensions:!known_dimensions
                 ~parent_size:(Size.map Option.some area_size)
                 ~available_space:
                   Size.
                     {
                       width =
                         Available_space.of_float
                           (Helpers.maybe_clamp area_width min_size.width
                              max_size.width);
                       height =
                         Available_space.of_float
                           (Helpers.maybe_clamp area_height min_size.height
                              max_size.height);
                     }
                 ~vertical_margins_are_collapsible:Line.both_false)
            |> Layout_output.size
          in
          let final_size =
            Size.unwrap_or measured_size !known_dimensions |> fun s ->
            let s_opt = Size.map Option.some s in
            Size.choose_first min_max_definite_size
              (Size.clamp_option min_size max_size s_opt)
            |> fun s_opt -> Size.unwrap_or s s_opt
          in

          let layout_output =
            Tree.compute_child_layout tree item.node_id
              (Layout_input.make ~run_mode:Run_mode.Perform_layout
                 ~sizing_mode:Sizing_mode.Content_size ~axis:Requested_axis.Both
                 ~known_dimensions:(Size.map Option.some final_size)
                 ~parent_size:(Size.map Option.some area_size)
                 ~available_space:
                   Size.
                     {
                       width =
                         Available_space.of_float
                           (Helpers.maybe_clamp area_width min_size.width
                              max_size.width);
                       height =
                         Available_space.of_float
                           (Helpers.maybe_clamp area_height min_size.height
                              max_size.height);
                     }
                 ~vertical_margins_are_collapsible:Line.both_false)
          in

          let non_auto_margin =
            Rect.
              {
                left =
                  (if Option.is_some left then
                     Option.value margin.left ~default:0.0
                   else 0.0);
                right =
                  (if Option.is_some right then
                     Option.value margin.right ~default:0.0
                   else 0.0);
                top =
                  (if Option.is_some top then
                     Option.value margin.top ~default:0.0
                   else 0.0);
                bottom =
                  (if Option.is_some bottom then
                     Option.value margin.bottom ~default:0.0
                   else 0.0);
              }
          in

          (* Expand auto margins to fill available space *)
          (* https://www.w3.org/TR/CSS21/visudet.html#abs-non-replaced-width *)
          let auto_margin =
            (* Auto margins for absolutely positioned elements in block
               containers only resolve if inset is set. Otherwise they resolve
               to 0. *)
            let absolute_auto_margin_space =
              Point.
                {
                  x =
                    (match right with
                    | Some r ->
                        area_size.width -. r -. Option.value left ~default:0.0
                    | None -> final_size.width);
                  y =
                    (match bottom with
                    | Some b ->
                        area_size.height -. b -. Option.value top ~default:0.0
                    | None -> final_size.height);
                }
            in
            let free_space =
              Size.
                {
                  width =
                    absolute_auto_margin_space.x -. final_size.width
                    -. Rect.horizontal_axis_sum non_auto_margin;
                  height =
                    absolute_auto_margin_space.y -. final_size.height
                    -. Rect.vertical_axis_sum non_auto_margin;
                }
            in

            let auto_margin_size =
              let width_auto_margin_count =
                (if Option.is_none margin.left then 1 else 0)
                + if Option.is_none margin.right then 1 else 0
              in
              let height_auto_margin_count =
                (if Option.is_none margin.top then 1 else 0)
                + if Option.is_none margin.bottom then 1 else 0
              in
              Size.
                {
                  width =
                    (if
                       width_auto_margin_count = 2
                       && (Option.is_none style_size.width
                          || Option.value style_size.width ~default:0.0
                             >= free_space.width)
                     then 0.0
                     else if width_auto_margin_count > 0 then
                       free_space.width /. float_of_int width_auto_margin_count
                     else 0.0);
                  height =
                    (if
                       height_auto_margin_count = 2
                       && (Option.is_none style_size.height
                          || Option.value style_size.height ~default:0.0
                             >= free_space.height)
                     then 0.0
                     else if height_auto_margin_count > 0 then
                       free_space.height
                       /. float_of_int height_auto_margin_count
                     else 0.0);
                }
            in

            Rect.
              {
                left =
                  (if Option.is_some margin.left then 0.0
                   else auto_margin_size.width);
                right =
                  (if Option.is_some margin.right then 0.0
                   else auto_margin_size.width);
                top =
                  (if Option.is_some margin.top then 0.0
                   else auto_margin_size.height);
                bottom =
                  (if Option.is_some margin.bottom then 0.0
                   else auto_margin_size.height);
              }
          in

          let resolved_margin =
            Rect.
              {
                left = Option.value margin.left ~default:auto_margin.left;
                right = Option.value margin.right ~default:auto_margin.right;
                top = Option.value margin.top ~default:auto_margin.top;
                bottom = Option.value margin.bottom ~default:auto_margin.bottom;
              }
          in

          let location =
            Point.
              {
                x =
                  (match left with
                  | Some l -> l +. resolved_margin.left +. area_offset.x
                  | None -> (
                      match right with
                      | Some r ->
                          area_size.width -. final_size.width -. r
                          -. resolved_margin.right +. area_offset.x
                      | None -> item.static_position.x +. resolved_margin.left));
                y =
                  (match top with
                  | Some t -> t +. resolved_margin.top +. area_offset.y
                  | None -> (
                      match bottom with
                      | Some b ->
                          area_size.height -. final_size.height -. b
                          -. resolved_margin.bottom +. area_offset.y
                      | None -> item.static_position.y +. resolved_margin.top));
              }
          in

          (* Note: axis intentionally switched here as scrollbars take up space
             in the opposite axis to the axis in which scrolling is enabled. *)
          let scrollbar_size =
            Size.
              {
                width =
                  (if item.overflow.y = Overflow.Scroll then
                     item.scrollbar_width
                   else 0.0);
                height =
                  (if item.overflow.x = Overflow.Scroll then
                     item.scrollbar_width
                   else 0.0);
              }
          in

          Tree.set_unrounded_layout tree item.node_id
            (Layout.make ~order:item.order ~size:final_size
               ~content_size:(Layout_output.content_size layout_output)
               ~scrollbar_size ~location ~padding ~border
               ~margin:resolved_margin);

          (* Update absolute_content_size *)
          absolute_content_size :=
            Size.max !absolute_content_size
              (Toffee_compute_helpers.compute_content_size_contribution
                 ~location ~size:final_size
                 ~content_size:(Layout_output.content_size layout_output)
                 ~overflow:item.overflow)))
    items;

  !absolute_content_size

(* Inner computation function - continues after initial setup *)
let compute_inner (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node_id : Node_id.t) (inputs : Layout_input.t) : Layout_output.t =
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let run_mode = Layout_input.run_mode inputs in
  let vertical_margins_are_collapsible =
    Layout_input.vertical_margins_are_collapsible inputs
  in

  let style = Tree.get_core_container_style tree node_id in
  let raw_padding = Style.padding style in
  let raw_border = Style.border style in
  let raw_margin = Style.margin style in
  let aspect_ratio = Style.aspect_ratio style in
  let calc = Tree.resolve_calc_value tree in
  let padding =
    raw_padding
    |> Style.Length_percentage.resolve_rect_or_zero parent_size.width calc
  in
  let border =
    raw_border
    |> Style.Length_percentage.resolve_rect_or_zero parent_size.width calc
  in

  (* Scrollbar gutters are reserved when the `overflow` property is set to
     `Overflow::Scroll`. However, the axis are switched (transposed) because a
     node that scrolls vertically needs *horizontal* space to be reserved for a
     scrollbar *)
  let scrollbar_gutter =
    let overflow = Style.overflow style in
    let offsets =
      Point.{ x = overflow.y; y = overflow.x }
      |> Point.map (function
        | Overflow.Scroll -> Style.scrollbar_width style
        | _ -> 0.0)
    in
    (* TODO: make side configurable based on the `direction` property *)
    Rect.{ top = 0.0; left = 0.0; right = offsets.x; bottom = offsets.y }
  in
  let padding_border = Rect.add padding border in
  let padding_border_size = Rect.sum_axes padding_border in
  let content_box_inset = Rect.add padding_border scrollbar_gutter in
  let container_content_box_size =
    Size.maybe_sub (Rect.sum_axes content_box_inset) known_dimensions
  in

  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Size.zero
  in
  let size =
    Style.size style |> fun dims ->
    Size.
      {
        width = Style.Dimension.maybe_resolve dims.width parent_size.width calc;
        height =
          Style.Dimension.maybe_resolve dims.height parent_size.height calc;
      }
    |> Size.apply_aspect_ratio aspect_ratio
    |> Size.maybe_add box_sizing_adjustment
  in
  let min_size =
    Style.min_size style |> fun dims ->
    Size.
      {
        width = Style.Dimension.maybe_resolve dims.width parent_size.width calc;
        height =
          Style.Dimension.maybe_resolve dims.height parent_size.height calc;
      }
    |> Size.apply_aspect_ratio aspect_ratio
    |> Size.maybe_add box_sizing_adjustment
  in
  let max_size =
    Style.max_size style |> fun dims ->
    Size.
      {
        width = Style.Dimension.maybe_resolve dims.width parent_size.width calc;
        height =
          Style.Dimension.maybe_resolve dims.height parent_size.height calc;
      }
    |> Size.apply_aspect_ratio aspect_ratio
    |> Size.maybe_add box_sizing_adjustment
  in

  (* Determine margin collapsing behaviour *)
  let own_margins_collapse_with_children =
    Line.
      {
        start =
          vertical_margins_are_collapsible.start
          && (not (Overflow.is_container (Style.overflow style).x))
          && (not (Overflow.is_container (Style.overflow style).y))
          && Style.position style = Position.Relative
          && padding.top = 0.0 && border.top = 0.0;
        end_ =
          vertical_margins_are_collapsible.end_
          && (not (Overflow.is_container (Style.overflow style).x))
          && (not (Overflow.is_container (Style.overflow style).y))
          && Style.position style = Position.Relative
          && padding.bottom = 0.0 && border.bottom = 0.0
          && Option.is_none size.height;
      }
  in
  let has_styles_preventing_being_collapsed_through =
    (match Style.display style with Display.Block -> false | _ -> true)
    || Overflow.is_container (Style.overflow style).x
    || Overflow.is_container (Style.overflow style).y
    || Style.position style = Position.Absolute
    || padding.top > 0.0 || padding.bottom > 0.0 || border.top > 0.0
    || border.bottom > 0.0
    || (match size.height with Some h when h > 0.0 -> true | _ -> false)
    || match min_size.height with Some h when h > 0.0 -> true | _ -> false
  in

  let text_align = text_align style in

  (* 1. Generate items *)
  let items =
    generate_item_list (module Tree) tree node_id container_content_box_size
  in

  (* 2. Compute container width *)
  let container_outer_width =
    match known_dimensions.width with
    | Some width -> width
    | None ->
        let available_width =
          Available_space.sub_or_zero available_space.width
            (Some (Rect.horizontal_axis_sum content_box_inset))
        in
        let intrinsic_width =
          determine_content_based_container_width
            (module Tree)
            tree items available_width
          +. Rect.horizontal_axis_sum content_box_inset
        in
        intrinsic_width |> fun w ->
        (match (min_size.width, max_size.width) with
          | Some min, Some max -> Float.max min (Float.min max w)
          | Some min, None -> Float.max min w
          | None, Some max -> Float.min max w
          | None, None -> w)
        |> Float.max padding_border_size.width
  in

  (* Short-circuit if computing size and both dimensions known *)
  match (run_mode, known_dimensions.height) with
  | Run_mode.Compute_size, Some container_outer_height ->
      Layout_output.from_outer_size
        Size.{ width = container_outer_width; height = container_outer_height }
  | _ ->
      (* 3. Perform final item layout and return content height *)
      let resolved_padding =
        raw_padding
        |> Style.Length_percentage.resolve_rect_or_zero
             (Some container_outer_width) calc
      in
      let resolved_border =
        raw_border
        |> Style.Length_percentage.resolve_rect_or_zero
             (Some container_outer_width) calc
      in
      let resolved_content_box_inset =
        Rect.add (Rect.add resolved_padding resolved_border) scrollbar_gutter
      in
      let ( inflow_content_size,
            intrinsic_outer_height,
            first_child_top_margin_set,
            last_child_bottom_margin_set ) =
        perform_final_layout_on_in_flow_children
          (module Tree)
          tree items container_outer_width content_box_inset
          resolved_content_box_inset text_align
          own_margins_collapse_with_children
      in
      let container_outer_height =
        known_dimensions.height
        |> Option.value
             ~default:
               ( intrinsic_outer_height |> fun h ->
                 Helpers.maybe_clamp h min_size.height max_size.height )
        |> Float.max padding_border_size.height
      in
      let final_outer_size =
        Size.{ width = container_outer_width; height = container_outer_height }
      in

      (* Short-circuit if computing size *)
      if run_mode = Run_mode.Compute_size then
        Layout_output.from_outer_size final_outer_size
      else
        (* 4. Layout absolutely positioned children *)
        let absolute_position_inset =
          Rect.add resolved_border scrollbar_gutter
        in
        let absolute_position_area =
          Size.sub final_outer_size (Rect.sum_axes absolute_position_inset)
        in
        let absolute_position_offset =
          Point.
            {
              x = absolute_position_inset.left;
              y = absolute_position_inset.top;
            }
        in
        let absolute_content_size =
          perform_absolute_layout_on_absolute_children
            (module Tree)
            tree items absolute_position_area absolute_position_offset
        in

        (* 5. Perform hidden layout on hidden children *)
        let len = Tree.child_count tree node_id in
        for order = 0 to len - 1 do
          let child = Tree.get_child_id tree node_id order in
          let child_style = Tree.get_core_container_style tree child in
          if Style.box_generation_mode child_style = Box_generation_mode.None
          then (
            Tree.set_unrounded_layout tree child (Layout.with_order order);
            (* Note: Rust uses perform_child_layout which internally uses
               RunMode::PerformLayout, not a special hidden layout mode *)
            Tree.compute_child_layout tree child
              (Layout_input.make ~run_mode:Run_mode.Perform_layout
                 ~sizing_mode:Sizing_mode.Inherent_size
                 ~axis:Requested_axis.Both ~known_dimensions:Size.none
                 ~parent_size:Size.none
                 ~available_space:
                   Size.
                     {
                       width = Available_space.max_content;
                       height = Available_space.max_content;
                     }
                 ~vertical_margins_are_collapsible:Line.both_false)
            |> ignore)
        done;

        (* 7. Determine whether this node can be collapsed through *)
        let all_in_flow_children_can_be_collapsed_through =
          List.for_all
            (fun item ->
              item.position = Position.Absolute || item.can_be_collapsed_through)
            items
        in
        let can_be_collapsed_through =
          (not has_styles_preventing_being_collapsed_through)
          && all_in_flow_children_can_be_collapsed_through
        in

        (* 8. Compute final content size *)
        let content_size = Size.max inflow_content_size absolute_content_size in

        (* 9. Return final outputs *)
        let top_margin =
          if own_margins_collapse_with_children.start then
            first_child_top_margin_set
          else
            let margin_top =
              Style.Length_percentage_auto.resolve_or_zero raw_margin.top
                parent_size.width calc
            in
            Collapsible_margin_set.from_margin margin_top
        in
        let bottom_margin =
          if own_margins_collapse_with_children.end_ then
            last_child_bottom_margin_set
          else
            let margin_bottom =
              Style.Length_percentage_auto.resolve_or_zero raw_margin.bottom
                parent_size.width calc
            in
            Collapsible_margin_set.from_margin margin_bottom
        in
        Layout_output.make ~size:final_outer_size ~content_size
          ~first_baselines:Point.none ~top_margin ~bottom_margin
          ~margins_can_collapse_through:can_be_collapsed_through

(* Computes the layout of a block container according to the block layout
   algorithm *)
let compute_block_layout (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node_id : Node_id.t) (inputs : Layout_input.t) : Layout_output.t =
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let run_mode = Layout_input.run_mode inputs in
  let sizing_mode = Layout_input.sizing_mode inputs in

  let style = Tree.get_core_container_style tree node_id in
  let calc = Tree.resolve_calc_value tree in

  (* Pull these out earlier to avoid borrowing issues *)
  let aspect_ratio = Style.aspect_ratio style in
  let padding =
    Style.padding style
    |> Length_percentage.resolve_rect_or_zero parent_size.width calc
  in
  let border =
    Style.border style
    |> Length_percentage.resolve_rect_or_zero parent_size.width calc
  in
  let padding_border_size = Rect.sum_axes (Rect.add padding border) in
  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Size.zero
  in

  let min_size =
    Style.min_size style |> fun dims ->
    Size.
      {
        width =
          Dimension.maybe_resolve (Size.get Inline dims) parent_size.width calc;
        height =
          Dimension.maybe_resolve (Size.get Block dims) parent_size.height calc;
      }
    |> Size.apply_aspect_ratio aspect_ratio
    |> Size.maybe_add box_sizing_adjustment
  in
  let max_size =
    Style.max_size style |> fun dims ->
    Size.
      {
        width =
          Dimension.maybe_resolve (Size.get Inline dims) parent_size.width calc;
        height =
          Dimension.maybe_resolve (Size.get Block dims) parent_size.height calc;
      }
    |> Size.apply_aspect_ratio aspect_ratio
    |> Size.maybe_add box_sizing_adjustment
  in
  let clamped_style_size =
    if sizing_mode = Sizing_mode.Inherent_size then
      Style.size style |> fun dims ->
      Size.
        {
          width =
            Dimension.maybe_resolve (Size.get Inline dims) parent_size.width
              calc;
          height =
            Dimension.maybe_resolve (Size.get Block dims) parent_size.height
              calc;
        }
      |> Size.apply_aspect_ratio aspect_ratio
      |> Size.maybe_add box_sizing_adjustment
      |> Size.clamp_option min_size max_size
    else Size.none
  in

  (* If both min and max in a given axis are set and max <= min then this
     determines the size in that axis *)
  let min_max_definite_size =
    Size.map2
      (fun min max ->
        match (min, max) with
        | Some min_v, Some max_v when max_v <= min_v -> Some min_v
        | _ -> None)
      min_size max_size
  in

  let styled_based_known_dimensions =
    known_dimensions |> fun dims ->
    Size.choose_first dims min_max_definite_size |> fun dims ->
    Size.choose_first dims clamped_style_size
    |> Size.maybe_max padding_border_size
  in

  (* Short-circuit layout if the container's size is fully determined by the
     container's size and the run mode is ComputeSize (and thus the container's
     size is all that we're interested in) *)
  if run_mode = Run_mode.Compute_size then
    match styled_based_known_dimensions with
    | { width = Some width; height = Some height } ->
        Layout_output.from_outer_size Size.{ width; height }
    | _ ->
        (* Continue with compute_inner even in Compute_size mode if size not
           fully determined *)
        compute_inner
          (module Tree)
          tree node_id
          (Layout_input.make ~run_mode ~sizing_mode
             ~axis:(Layout_input.axis inputs)
             ~known_dimensions:styled_based_known_dimensions ~parent_size
             ~available_space
             ~vertical_margins_are_collapsible:
               (Layout_input.vertical_margins_are_collapsible inputs))
  else
    (* TODO: debug_log "BLOCK" *)
    compute_inner
      (module Tree)
      tree node_id
      (Layout_input.make ~run_mode ~sizing_mode ~axis:(Layout_input.axis inputs)
         ~known_dimensions:styled_based_known_dimensions ~parent_size
         ~available_space
         ~vertical_margins_are_collapsible:
           (Layout_input.vertical_margins_are_collapsible inputs))
