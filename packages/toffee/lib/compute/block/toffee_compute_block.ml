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

(* Whether a block container stretches a child with an auto width to its own
   width: not a table, nor a replaced element, whose auto width is its natural
   width (CSS 2 10.3.4). *)
let is_stretched (style : Style.t) : bool =
  not (Style.item_is_table style || Style.item_is_replaced style)

(* Per-child data that is accumulated and modified over the course of the layout
   algorithm *)
type block_item = {
  node_id : Node_id.t; (* The identifier for the associated node *)
  order : int;
      (* The "source order" of the item. This is the index of the item within
         the children iterator, and controls the order in which the nodes are
         placed *)
  is_stretched : bool;
      (* Whether an auto width stretches to the container's: not for tables and
         replaced elements *)
  size : float option size; (* The base size of this item *)
  min_size : float option size; (* The minimum allowable size of this item *)
  max_size : float option size; (* The maximum allowable size of this item *)
  overflow : overflow point; (* The overflow style of the item *)
  scrollbar_width : float;
      (* The width of the item's scrollbars (if it has scrollbars) *)
  position : position; (* The position style of the item *)
  inset : length_percentage_auto rect; (* The final offset of this item *)
  margin : length_percentage_auto rect; (* The margin of this item *)
  padding : Rectf.t; (* The padding of this item *)
  border : Rectf.t; (* The border of this item *)
  padding_border_sum : Sizef.t;
      (* The sum of padding and border for this item *)
  mutable static_position : Pointf.t;
      (* The computed "static position" of this item. The static position is the
         position taking into account padding, border, margins, and
         scrollbar_gutters but not inset *)
  mutable can_be_collapsed_through : bool;
      (* Whether margins can be collapsed through this item *)
  mutable final_layout : Layout.t option;
      (* The layout of an in-flow item, which the container commits to the
         tree from its final layout alone *)
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
      let pb_sum = Rectf.sum_axes (Rectf.add padding border) in
      let box_sizing_adjustment =
        if Style.box_sizing child_style = Box_sizing.Content_box then pb_sum
        else Sizef.zero
      in

      let item =
        {
          node_id = child_node_id;
          order = !order;
          is_stretched = is_stretched child_style;
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
             |> Sizef.maybe_add box_sizing_adjustment);
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
             |> Sizef.maybe_add box_sizing_adjustment);
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
             |> Sizef.maybe_add box_sizing_adjustment);
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
          static_position = Pointf.zero;
          can_be_collapsed_through = false;
          final_layout = None;
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

        let calc = Tree.resolve_calc_value tree in
        let parent_width = Available_space.to_option available_space.width in
        let item_x_margin_sum =
          Length_percentage_auto.resolve_or_zero item.margin.left parent_width
            calc
          +. Length_percentage_auto.resolve_or_zero item.margin.right
               parent_width calc
        in
        let width =
          match known_dimensions.width with
          | Some w -> w
          | None ->
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
                  (Layout_input.make ~run_mode:Run_mode.Compute_size
                     ~sizing_mode:Sizing_mode.Inherent_size
                     ~axis:Requested_axis.Horizontal ~known_dimensions
                     ~parent_size:Size.none
                     ~available_space:available_space_for_child
                     ~vertical_margins_are_collapsible:
                       { start = true; end_ = true })
              in
              (Layout_output.size size_and_baselines).width
        in
        (* A child contributes its margin box, whether its width is known or
           measured *)
        let width =
          Float.max width item.padding_border_sum.width +. item_x_margin_sum
        in
        max_child_width := Float.max !max_child_width width)
    items;

  !max_child_width

(* Perform final layout on in-flow children *)
let perform_final_layout_on_in_flow_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (items : block_item list) (container_outer_width : float)
    (container_percentage_resolution_height : float option)
    (content_box_inset : Rectf.t) (resolved_content_box_inset : Rectf.t)
    (text_align : Text_align.t) (own_margins_collapse_with_children : bool line)
    (overflow_acc : Toffee_compute_helpers.overflow_acc)
    (overflow_flags : Toffee_compute_helpers.overflow_flags)
    (overflow_origin : Rectf.t) (overflow_padding : Rectf.t) :
    float * Collapsible_margin_set.t * Collapsible_margin_set.t =
  (* Resolve container_inner_width for sizing child nodes using initial
     content_box_inset *)
  let container_inner_width =
    container_outer_width -. Rectf.horizontal_axis_sum content_box_inset
  in
  (* The children's percentage heights and vertical insets resolve against
     the container's content box height, when it is definite. *)
  let container_percentage_resolution_height =
    Option.map
      (fun height ->
        height -. Rectf.vertical_axis_sum resolved_content_box_inset)
      container_percentage_resolution_height
  in
  let parent_size =
    Size.
      {
        width = Some container_inner_width;
        height = container_percentage_resolution_height;
      }
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
          Pointf.
            { x = resolved_content_box_inset.left; y = !y_offset_for_absolute }
      else
        (* Handle in-flow items *)
        let item_margin =
          item.margin
          |> Rect.map (fun margin ->
              Length_percentage_auto.resolve_to_option_with_calc margin
                container_inner_width calc)
        in
        let item_non_auto_margin =
          Rectf.
            {
              left = Option.value item_margin.left ~default:0.0;
              right = Option.value item_margin.right ~default:0.0;
              top = Option.value item_margin.top ~default:0.0;
              bottom = Option.value item_margin.bottom ~default:0.0;
            }
        in
        let item_non_auto_x_margin_sum =
          Rectf.horizontal_axis_sum item_non_auto_margin
        in

        let known_dimensions =
          if not item.is_stretched then Size.none
          else
            item.size
            |> Size.map_width (fun width ->
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
               ~known_dimensions ~parent_size
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
          Rectf.
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
            Size.
              {
                width = Some container_inner_width;
                height = container_percentage_resolution_height;
              }
            (fun p s -> Length_percentage_auto.maybe_resolve p s calc)
            item.inset
        in
        let inset_offset =
          Pointf.
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
          Pointf.
            {
              x = resolved_content_box_inset.left;
              y =
                !committed_y_offset
                +. Collapsible_margin_set.resolve !active_collapsible_margin_set;
            };
        let location =
          Pointf.
            {
              x =
                resolved_content_box_inset.left +. inset_offset.x
                +. resolved_margin.left;
              y = !committed_y_offset +. inset_offset.y +. y_margin_offset;
            }
        in

        (* Apply alignment *)
        let item_outer_width =
          final_size.width +. Rectf.horizontal_axis_sum resolved_margin
        in
        let location =
          if item_outer_width < container_inner_width then
            match text_align with
            | Text_align.Auto -> location
            | Text_align.Legacy_left -> location (* Left aligned by default *)
            | Text_align.Legacy_right ->
                Pointf.
                  {
                    location with
                    x = location.x +. (container_inner_width -. item_outer_width);
                  }
            | Text_align.Legacy_center ->
                Pointf.
                  {
                    location with
                    x =
                      location.x
                      +. ((container_inner_width -. item_outer_width) /. 2.0);
                  }
          else location
        in

        let scrollbar_size =
          Sizef.
            {
              width =
                (if item.overflow.y = Overflow.Scroll then item.scrollbar_width
                 else 0.0);
              height =
                (if item.overflow.x = Overflow.Scroll then item.scrollbar_width
                 else 0.0);
            }
        in

        item.final_layout <-
          Some
            (Layout.make ~order:item.order ~size:final_size
               ~scrollable_overflow:
                 (Layout_output.scrollable_overflow item_layout)
               ~scrollbar_size ~location ~padding:item.padding
               ~border:item.border ~margin:resolved_margin);

        (* Add the item to the container's scrollable overflow *)
        Toffee_compute_helpers.add_overflow_contribution overflow_acc
          ~flags:overflow_flags ~origin:overflow_origin ~client:Sizef.zero
          ~padding:overflow_padding ~location ~size:final_size
          ~overflow_area:(Layout_output.scrollable_overflow item_layout)
          ~border:item.border ~overflow:item.overflow ~margin:resolved_margin;

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
  (content_height, !first_child_top_margin_set, last_child_bottom_margin_set)

(* Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (items : block_item list) (area_size : Sizef.t) (area_offset : Pointf.t)
    (overflow_acc : Toffee_compute_helpers.overflow_acc)
    (overflow_flags : Toffee_compute_helpers.overflow_flags)
    (overflow_origin : Rectf.t) : unit =
  let area_width = area_size.width in
  let area_height = area_size.height in

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
          let padding_border_sum = Rectf.sum_axes (Rectf.add padding border) in
          let box_sizing_adjustment =
            if Style.box_sizing child_style = Box_sizing.Content_box then
              padding_border_sum
            else Sizef.zero
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
            |> Sizef.maybe_add box_sizing_adjustment
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
            |> Sizef.maybe_add box_sizing_adjustment
          in
          let min_size =
            resolved_min_size |> fun size ->
            Size.choose_first size (Sizef.some padding_border_sum)
            |> Sizef.maybe_max padding_border_sum
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
            |> Sizef.maybe_add box_sizing_adjustment
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
                 ~parent_size:(Sizef.some area_size)
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
            Sizef.unwrap_or measured_size !known_dimensions |> fun s ->
            let s_opt = Sizef.some s in
            Size.choose_first min_max_definite_size
              (Size.clamp_option min_size max_size s_opt)
            |> fun s_opt -> Sizef.unwrap_or s s_opt
          in

          let layout_output =
            Tree.compute_child_layout tree item.node_id
              (Layout_input.make ~run_mode:Run_mode.Perform_layout
                 ~sizing_mode:Sizing_mode.Content_size ~axis:Requested_axis.Both
                 ~known_dimensions:(Sizef.some final_size)
                 ~parent_size:(Sizef.some area_size)
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
            Rectf.
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
              Pointf.
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
              Sizef.
                {
                  width =
                    absolute_auto_margin_space.x -. final_size.width
                    -. Rectf.horizontal_axis_sum non_auto_margin;
                  height =
                    absolute_auto_margin_space.y -. final_size.height
                    -. Rectf.vertical_axis_sum non_auto_margin;
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
              Sizef.
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

            Rectf.
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
            Rectf.
              {
                left = Option.value margin.left ~default:auto_margin.left;
                right = Option.value margin.right ~default:auto_margin.right;
                top = Option.value margin.top ~default:auto_margin.top;
                bottom = Option.value margin.bottom ~default:auto_margin.bottom;
              }
          in

          let location =
            Pointf.
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
            Sizef.
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
               ~scrollable_overflow:
                 (Layout_output.scrollable_overflow layout_output)
               ~scrollbar_size ~location ~padding ~border
               ~margin:resolved_margin);

          (* Add the item to the container's scrollable overflow *)
          Toffee_compute_helpers.add_overflow_contribution overflow_acc
            ~flags:overflow_flags ~origin:overflow_origin ~client:Sizef.zero
            ~padding:Rectf.zero ~location ~size:final_size
            ~overflow_area:(Layout_output.scrollable_overflow layout_output)
            ~border ~overflow:item.overflow ~margin:resolved_margin))
    items

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
    let scrollbar_width = Style.scrollbar_width style in
    (* TODO: make side configurable based on the `direction` property *)
    Rectf.
      {
        top = 0.0;
        left = 0.0;
        right = (if overflow.y = Overflow.Scroll then scrollbar_width else 0.0);
        bottom = (if overflow.x = Overflow.Scroll then scrollbar_width else 0.0);
      }
  in
  let padding_border = Rectf.add padding border in
  let padding_border_size = Rectf.sum_axes padding_border in
  let content_box_inset = Rectf.add padding_border scrollbar_gutter in

  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Sizef.zero
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
    |> Sizef.maybe_add box_sizing_adjustment
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
    |> Sizef.maybe_add box_sizing_adjustment
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
    |> Sizef.maybe_add box_sizing_adjustment
  in
  (* A definite size in one axis transfers through aspect-ratio to the other
     (css-sizing-4). A block parent knows an axis only when it constrains it,
     as the stretched width at final layout does, and leaves it unknown while
     measuring the intrinsic size, so the transfer applies to real constraints
     only. Only an unknown axis takes the derived size, clamped: a known size
     stays as the parent resolved it. *)
  let ratio_known_dimensions =
    Size.choose_first known_dimensions
      (known_dimensions
      |> Size.apply_aspect_ratio aspect_ratio
      |> Size.clamp_option min_size max_size)
  in
  (* A height the ratio gives is a definite basis for the children's
     percentages, but with an automatic minimum height the box still grows
     to its content (css-sizing-4, the automatic minimum size of a box with
     a ratio), unless it is a scroll container: it then floors the height
     the content gives rather than fixing it. NaN where it does not apply. *)
  let ratio_height_floor =
    match (known_dimensions.height, ratio_known_dimensions.height) with
    | None, Some height
      when Option.is_none min_size.height
           &&
           let overflow = Style.overflow style in
           not
             (Overflow.is_container overflow.x
             || Overflow.is_container overflow.y) ->
        height
    | _ -> Float.nan
  in
  let known_dimensions =
    if Float.is_nan ratio_height_floor then ratio_known_dimensions
    else { ratio_known_dimensions with height = None }
  in
  (* A known height that is not definite, as the content height a flex
     container gives an item it does not stretch, is no basis for the
     children's percentages *)
  let percentage_basis =
    if
      (Layout_input.known_dimensions_are_definite inputs).height
      || Option.is_none ratio_known_dimensions.height
    then ratio_known_dimensions
    else { ratio_known_dimensions with height = None }
  in
  let container_content_box_size =
    Sizef.maybe_sub (Rectf.sum_axes content_box_inset) percentage_basis
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
            (Some (Rectf.horizontal_axis_sum content_box_inset))
        in
        let intrinsic_width =
          determine_content_based_container_width
            (module Tree)
            tree items available_width
          +. Rectf.horizontal_axis_sum content_box_inset
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
        Sizef.{ width = container_outer_width; height = container_outer_height }
  (* Nor is the height computed when only the width is requested. *)
  | Run_mode.Compute_size, None
    when Layout_input.axis inputs = Requested_axis.Horizontal ->
      Layout_output.from_outer_size
        Sizef.{ width = container_outer_width; height = 0.0 }
  | _ ->
      (* The height against which the children's percentages resolve: the
         container's own height when it is known. Its min-height alone is not
         a basis: percentages of an indefinite height behave as auto (CSS 2
         10.5). Under Content_size the container's height and min-height are
         ignored, so they are no basis either: the content size of a flex item
         must not depend on its specified height. *)
      let container_percentage_resolution_height =
        match (percentage_basis.height, size.height, min_size.height) with
        | (Some _ as height), _, _ -> height
        | None, _, _
          when Layout_input.sizing_mode inputs <> Sizing_mode.Inherent_size ->
            None
        | None, Some height, Some min -> Some (Float.max height min)
        | None, height, _ -> height
      in
      (* 3. Perform final item layout and return content height *)
      (* Percentage padding and border resolve against the containing block's
         width, not the box's own: they differ when the box's width does not
         stretch. Without a containing block, at the root, the box's own width
         is the basis. *)
      let percentage_resolution_width =
        Option.value parent_size.width ~default:container_outer_width
      in
      let resolved_padding =
        raw_padding
        |> Style.Length_percentage.resolve_rect_or_zero
             (Some percentage_resolution_width) calc
      in
      let resolved_border =
        raw_border
        |> Style.Length_percentage.resolve_rect_or_zero
             (Some percentage_resolution_width) calc
      in
      let resolved_content_box_inset =
        Rectf.add (Rectf.add resolved_padding resolved_border) scrollbar_gutter
      in
      let is_scroll_container =
        let overflow = Style.overflow style in
        Overflow.is_container overflow.x || Overflow.is_container overflow.y
      in
      (* The container's scrollable overflow, gathered in its padding-box
         frame. A block container's scroll origins are never reversed. *)
      let overflow_acc = Toffee_compute_helpers.overflow_acc () in
      let overflow_flags =
        Toffee_compute_helpers.overflow_flags ~is_scroll_container
          ~reversed_x:false ~reversed_y:false ~margins:Negative_margins ()
      in
      let ( intrinsic_outer_height,
            first_child_top_margin_set,
            last_child_bottom_margin_set ) =
        perform_final_layout_on_in_flow_children
          (module Tree)
          tree items container_outer_width
          container_percentage_resolution_height content_box_inset
          resolved_content_box_inset text_align
          own_margins_collapse_with_children overflow_acc overflow_flags
          resolved_border resolved_padding
      in
      let container_outer_height =
        known_dimensions.height
        |> Option.value
             ~default:
               ( (if Float.is_nan ratio_height_floor then intrinsic_outer_height
                  else Float.max intrinsic_outer_height ratio_height_floor)
               |> fun h -> Helpers.maybe_clamp h min_size.height max_size.height
               )
        |> Float.max padding_border_size.height
      in
      let final_outer_size =
        Sizef.{ width = container_outer_width; height = container_outer_height }
      in

      (* Short-circuit if computing size *)
      if run_mode = Run_mode.Compute_size then
        Layout_output.from_outer_size final_outer_size
      else (
        (* Commit the in-flow children's layouts. *)
        List.iter
          (fun item ->
            match item.final_layout with
            | Some layout -> Tree.set_unrounded_layout tree item.node_id layout
            | None -> ())
          items;
        (* 4. Layout absolutely positioned children *)
        let absolute_position_inset =
          Rectf.add resolved_border scrollbar_gutter
        in
        let absolute_position_area =
          Sizef.sub final_outer_size (Rectf.sum_axes absolute_position_inset)
        in
        let absolute_position_offset =
          Pointf.
            {
              x = absolute_position_inset.left;
              y = absolute_position_inset.top;
            }
        in
        perform_absolute_layout_on_absolute_children
          (module Tree)
          tree items absolute_position_area absolute_position_offset
          overflow_acc
          (Toffee_compute_helpers.with_margins overflow_flags No_margins)
          resolved_border;

        (* 5. Perform hidden layout on hidden children *)
        let len = Tree.child_count tree node_id in
        for order = 0 to len - 1 do
          let child = Tree.get_child_id tree node_id order in
          let child_style = Tree.get_core_container_style tree child in
          if Style.box_generation_mode child_style = Box_generation_mode.None
          then
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
            |> ignore
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

        (* 8. The container's scrollable overflow: a scroll container's inline
           padding is part of it, even without content *)
        if is_scroll_container then
          Toffee_compute_helpers.union overflow_acc ~left:0.0
            ~right:(resolved_padding.left +. resolved_padding.right)
            ~top:0.0 ~bottom:0.0;
        let scrollable_overflow =
          Toffee_compute_helpers.finalize_overflow overflow_acc
            ~flags:overflow_flags ~size:final_outer_size ~border:resolved_border
            ~client:
              (if is_scroll_container then
                 Sizef.sub final_outer_size
                   (Rectf.sum_axes absolute_position_inset)
               else Sizef.zero)
        in

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
        Layout_output.make ~size:final_outer_size ~scrollable_overflow
          ~first_baselines:Point.none ~top_margin ~bottom_margin
          ~margins_can_collapse_through:can_be_collapsed_through)

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
  let padding_border_size = Rectf.sum_axes (Rectf.add padding border) in
  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Sizef.zero
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
    |> Sizef.maybe_add box_sizing_adjustment
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
    |> Sizef.maybe_add box_sizing_adjustment
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
      |> Sizef.maybe_add box_sizing_adjustment
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
    |> Sizef.maybe_max padding_border_size
  in

  (* Short-circuit layout if the container's size is fully determined by the
     container's size and the run mode is ComputeSize (and thus the container's
     size is all that we're interested in) *)
  if run_mode = Run_mode.Compute_size then
    match styled_based_known_dimensions with
    | { width = Some width; height = Some height } ->
        Layout_output.from_outer_size Sizef.{ width; height }
    (* Nor when the width is known and only the width is requested. *)
    | { width = Some width; height = None }
      when Layout_input.axis inputs = Requested_axis.Horizontal ->
        Layout_output.from_outer_size Sizef.{ width; height = 0.0 }
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
