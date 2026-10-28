(* Alignment of tracks and final positioning of items *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

(* Align the grid tracks within the grid according to the align-content (rows)
   or justify-content (columns) property. This only does anything if the size of
   the grid is not equal to the size of the grid container in the axis being
   aligned. *)
let align_tracks ~grid_container_content_box_size ~padding ~border ~tracks
    ~track_alignment_style =
  let used_size =
    Array.fold_left
      (fun acc track -> acc +. track.Grid_track.base_size)
      0.0 tracks
  in
  let free_space = grid_container_content_box_size -. used_size in
  let origin = padding.Line.start +. border.Line.start in

  (* Count the number of non-collapsed tracks (not counting gutters) *)
  let num_tracks =
    let count = ref 0 in
    for i = 1 to Array.length tracks - 1 do
      if i mod 2 = 1 && not tracks.(i).Grid_track.is_collapsed then incr count
    done;
    !count
  in

  (* Grid layout treats gaps as full tracks rather than applying them at
     alignment so we simply pass zero here. Grid layout is never reversed. *)
  let gap = 0.0 in
  let layout_is_reversed = false in
  let is_safe = false in
  (* TODO: Implement safe alignment *)
  let track_alignment =
    Toffee_compute_helpers.apply_alignment_fallback ~free_space
      ~num_items:num_tracks ~alignment_mode:track_alignment_style ~is_safe
  in

  (* Compute offsets *)
  let total_offset = ref origin in
  Array.iteri
    (fun i track ->
      (* Odd tracks are gutters (but arrays are zero-indexed, so odd tracks have
         even indices) *)
      let is_gutter = i mod 2 = 0 in

      (* The first non-gutter track is index 1 *)
      let is_first = i = 1 in

      let offset =
        if is_gutter then 0.0
        else
          Toffee_compute_helpers.compute_alignment_offset ~free_space
            ~num_items:num_tracks ~gap ~alignment_mode:track_alignment
            ~layout_is_flex_reversed:layout_is_reversed ~is_first
      in

      track.Grid_track.offset <- !total_offset +. offset;
      total_offset := !total_offset +. offset +. track.Grid_track.base_size)
    tracks

(* Align and size a grid item along a single axis *)
let align_item_within_area ~grid_area ~(alignment_style : Align_items.t)
    ~resolved_size ~position ~inset ~margin ~baseline_shim =
  (* Calculate grid area dimension in the axis *)
  let non_auto_margin =
    Line.
      {
        start = Option.value margin.Line.start ~default:0.0 +. baseline_shim;
        end_ = Option.value margin.Line.end_ ~default:0.0;
      }
  in
  let grid_area_size =
    Float.max (grid_area.Line.end_ -. grid_area.Line.start) 0.0
  in
  let free_space =
    Float.max (grid_area_size -. resolved_size -. Line.sum non_auto_margin) 0.0
  in

  (* Expand auto margins to fill available space *)
  let auto_margin_count =
    (if Option.is_none margin.Line.start then 1 else 0)
    + if Option.is_none margin.Line.end_ then 1 else 0
  in
  let auto_margin_size =
    if auto_margin_count > 0 then free_space /. float_of_int auto_margin_count
    else 0.0
  in
  let resolved_margin =
    Line.
      {
        start =
          Option.value margin.Line.start ~default:auto_margin_size
          +. baseline_shim;
        end_ = Option.value margin.Line.end_ ~default:auto_margin_size;
      }
  in

  (* Compute offset in the axis *)
  let alignment_based_offset =
    match alignment_style with
    | Align_items.Start | Align_items.Flex_start -> resolved_margin.Line.start
    | Align_items.End | Align_items.Flex_end ->
        grid_area_size -. resolved_size -. resolved_margin.Line.end_
    | Align_items.Center ->
        (grid_area_size -. resolved_size +. resolved_margin.Line.start
       -. resolved_margin.Line.end_)
        /. 2.0
    (* TODO: Add support for baseline alignment. For now we treat it as
       "start". *)
    | Align_items.Baseline -> resolved_margin.Line.start
    | Align_items.Stretch -> resolved_margin.Line.start
  in

  let offset_within_area =
    if position = Position.Absolute then
      match inset.Line.start with
      | Some start -> start +. non_auto_margin.Line.start
      | None -> (
          match inset.Line.end_ with
          | Some end_ ->
              grid_area_size -. end_ -. resolved_size
              -. non_auto_margin.Line.end_
          | None -> alignment_based_offset)
    else alignment_based_offset
  in

  let start = ref (grid_area.Line.start +. offset_within_area) in
  (if position = Position.Relative then
     let inset_adjustment =
       match (inset.Line.start, inset.Line.end_) with
       | Some s, _ -> s
       | None, Some e -> -.e
       | None, None -> 0.0
     in
     start := !start +. inset_adjustment);

  (!start, resolved_margin)

(* Align and size a grid item into its final position *)
let align_and_position_item (type t)
    (module Tree : LAYOUT_PARTIAL_TREE with type t = t) ~tree ~node ~order
    ~grid_area
    ~(container_alignment_styles : align_items option in_both_abs_axis)
    ~baseline_shim ~overflow_acc ~overflow_flags ~overflow_origin =
  let grid_area_size =
    Sizef.
      {
        width = grid_area.Rectf.right -. grid_area.Rectf.left;
        height = grid_area.Rectf.bottom -. grid_area.Rectf.top;
      }
  in

  let style = Tree.get_core_container_style tree node in

  let overflow = Style.overflow style in
  let scrollbar_width = Style.scrollbar_width style in
  let aspect_ratio = Style.aspect_ratio style in
  let justify_self = Style.justify_self style in
  let align_self = Style.align_self style in

  let position = Style.position style in
  let inset_horizontal =
    let inset = Style.inset style in
    let line = Rect.horizontal_components inset in
    Line.map
      (fun size ->
        Length_percentage_auto.resolve_to_option_with_calc size
          grid_area_size.width
          (Tree.resolve_calc_value tree))
      line
  in
  let inset_vertical =
    let inset = Style.inset style in
    let line = Rect.vertical_components inset in
    Line.map
      (fun size ->
        Length_percentage_auto.resolve_to_option_with_calc size
          grid_area_size.height
          (Tree.resolve_calc_value tree))
      line
  in
  let padding =
    Style.padding style
    |> Length_percentage.resolve_rect_or_zero (Some grid_area_size.width)
         (Tree.resolve_calc_value tree)
  in
  let border =
    Style.border style
    |> Length_percentage.resolve_rect_or_zero (Some grid_area_size.width)
         (Tree.resolve_calc_value tree)
  in
  let padding_border_size = Rectf.sum_axes (Rectf.add padding border) in

  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Sizef.zero
  in

  let inherent_size =
    let style_size = Style.size style in
    let resolved_size =
      Size.
        {
          width =
            Dimension.maybe_resolve style_size.width (Some grid_area_size.width)
              (Tree.resolve_calc_value tree);
          height =
            Dimension.maybe_resolve style_size.height
              (Some grid_area_size.height)
              (Tree.resolve_calc_value tree);
        }
    in
    let with_aspect = Size.apply_aspect_ratio aspect_ratio resolved_size in
    Sizef.maybe_add box_sizing_adjustment with_aspect
  in
  let min_size =
    let style_min_size = Style.min_size style in
    let resolved_min_size =
      Size.
        {
          width =
            Dimension.maybe_resolve style_min_size.width
              (Some grid_area_size.width)
              (Tree.resolve_calc_value tree);
          height =
            Dimension.maybe_resolve style_min_size.height
              (Some grid_area_size.height)
              (Tree.resolve_calc_value tree);
        }
    in
    let with_box_sizing =
      Sizef.maybe_add box_sizing_adjustment resolved_min_size
    in
    let with_padding_border =
      Size.choose_first with_box_sizing (Sizef.some padding_border_size)
    in
    let max_with_pb = Sizef.maybe_max padding_border_size with_padding_border in
    Size.apply_aspect_ratio aspect_ratio max_with_pb
  in
  let max_size =
    let style_max_size = Style.max_size style in
    let resolved_max_size =
      Size.
        {
          width =
            Dimension.maybe_resolve style_max_size.width
              (Some grid_area_size.width)
              (Tree.resolve_calc_value tree);
          height =
            Dimension.maybe_resolve style_max_size.height
              (Some grid_area_size.height)
              (Tree.resolve_calc_value tree);
        }
    in
    let with_aspect = Size.apply_aspect_ratio aspect_ratio resolved_max_size in
    Sizef.maybe_add box_sizing_adjustment with_aspect
  in

  (* Resolve default alignment styles if they are set on neither the parent or
     the node itself Note: if the child has a preferred aspect ratio but neither
     width or height are set, then the width is stretched and the then height is
     calculated from the width according the aspect ratio. A replaced element is
     not stretched: it is sized as for [Start]. See:
     https://www.w3.org/TR/css-grid-1/#grid-item-sizing *)
  let is_replaced = Style.item_is_replaced style in
  let alignment_styles =
    In_both_abs_axis.
      {
        horizontal =
          (match (justify_self, container_alignment_styles.horizontal) with
          | Some js, _ -> js
          | None, Some ca -> ca
          | None, None ->
              if Option.is_some inherent_size.width || is_replaced then
                Align_items.Start
              else Align_items.Stretch);
        vertical =
          (match (align_self, container_alignment_styles.vertical) with
          | Some as_, _ -> as_
          | None, Some ca -> ca
          | None, None ->
              if
                Option.is_some inherent_size.height
                || Option.is_some aspect_ratio
                || is_replaced
              then Align_items.Start
              else Align_items.Stretch);
      }
  in

  (* Note: This is not a bug. It is part of the CSS spec that both horizontal
     and vertical margins resolve against the WIDTH of the grid area. *)
  let margin =
    Style.margin style
    |> Rect.map (fun margin ->
        Length_percentage_auto.resolve_to_option_with_calc margin
          grid_area_size.width
          (Tree.resolve_calc_value tree))
  in

  let subtract_option base opt_val =
    match opt_val with Some v -> base -. v | None -> base
  in
  let grid_area_minus_item_margins_size =
    Sizef.
      {
        width =
          ( (grid_area_size.width |> fun w -> subtract_option w margin.left)
          |> fun w -> subtract_option w margin.right );
        height =
          ( ( (grid_area_size.height |> fun h -> subtract_option h margin.top)
            |> fun h -> subtract_option h margin.bottom )
          |> fun h -> h -. baseline_shim );
      }
  in

  (* If node is absolutely positioned and width is not set explicitly, then
     deduce it from left, right and container_content_box if both are set. *)
  let width =
    match inherent_size.width with
    | Some w -> Some w
    | None ->
        (* Apply width derived from both the left and right properties of an
           absolutely positioned element being set *)
        if position = Position.Absolute then
          match (inset_horizontal.start, inset_horizontal.end_) with
          | Some left, Some right ->
              Some
                (Float.max
                   (grid_area_minus_item_margins_size.width -. left -. right)
                   0.0)
          | _ -> None
          (* Apply width based on stretch alignment if: - Alignment style is
             "stretch" - The node is not absolutely positioned - The node does
             not have auto margins in this axis. *)
        else if
          Option.is_some margin.left
          && Option.is_some margin.right
          && alignment_styles.horizontal = Align_items.Stretch
          && position <> Position.Absolute
        then Some grid_area_minus_item_margins_size.width
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position width
     adjustments *)
  let size1 =
    Size.apply_aspect_ratio aspect_ratio
      Size.{ width; height = inherent_size.height }
  in

  let height =
    match size1.height with
    | Some h -> Some h
    | None ->
        if position = Position.Absolute then
          match (inset_vertical.start, inset_vertical.end_) with
          | Some top, Some bottom ->
              Some
                (Float.max
                   (grid_area_minus_item_margins_size.height -. top -. bottom)
                   0.0)
          | _ -> None
          (* Apply height based on stretch alignment if: - Alignment style is
             "stretch" - The node is not absolutely positioned - The node does
             not have auto margins in this axis. *)
        else if
          Option.is_some margin.top
          && Option.is_some margin.bottom
          && alignment_styles.vertical = Align_items.Stretch
          && position <> Position.Absolute
        then Some grid_area_minus_item_margins_size.height
        else None
  in

  (* Reapply aspect ratio after stretch and absolute position height
     adjustments *)
  let size2 =
    Size.apply_aspect_ratio aspect_ratio Size.{ width = size1.width; height }
  in

  (* Clamp size by min and max width/height *)
  let clamped_size = Size.clamp_option min_size max_size size2 in

  (* Resolve missing size for absolutely positioned items by performing a
     sizing-only pass *)
  let size =
    if
      position = Position.Absolute
      && (Option.is_none width || Option.is_none height)
    then
      let measured_size =
        Tree.compute_child_layout tree node
          (Layout_input.make ~run_mode:Run_mode.Compute_size
             ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
             ~known_dimensions:clamped_size
             ~parent_size:(Sizef.some grid_area_size)
             ~available_space:
               {
                 width =
                   Available_space.of_float
                     grid_area_minus_item_margins_size.width;
                 height =
                   Available_space.of_float
                     grid_area_minus_item_margins_size.height;
               }
             ~vertical_margins_are_collapsible:Line.both_false)
        |> Layout_output.size
      in
      Sizef.some measured_size
    else clamped_size
  in

  (* Layout node *)
  let layout_output =
    Tree.compute_child_layout tree node
      (Layout_input.make ~run_mode:Run_mode.Perform_layout
         ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
         ~known_dimensions:size
         ~parent_size:(Sizef.some grid_area_size)
         ~available_space:
           {
             width =
               Available_space.of_float grid_area_minus_item_margins_size.width;
             height =
               Available_space.of_float grid_area_minus_item_margins_size.height;
           }
         ~vertical_margins_are_collapsible:Line.both_false)
  in

  (* Resolve final size *)
  let final_size =
    let measured_size = Layout_output.size layout_output in
    let chosen_size = Sizef.unwrap_or measured_size size in
    Sizef.clamp min_size max_size chosen_size
  in

  let x, x_margin =
    align_item_within_area
      ~grid_area:Line.{ start = grid_area.left; end_ = grid_area.right }
      ~alignment_style:
        (Option.value justify_self ~default:alignment_styles.horizontal)
      ~resolved_size:final_size.width ~position ~inset:inset_horizontal
      ~margin:(Rect.horizontal_components margin)
      ~baseline_shim:0.0
  in

  let y, y_margin =
    align_item_within_area
      ~grid_area:Line.{ start = grid_area.top; end_ = grid_area.bottom }
      ~alignment_style:
        (Option.value align_self ~default:alignment_styles.vertical)
      ~resolved_size:final_size.height ~position ~inset:inset_vertical
      ~margin:(Rect.vertical_components margin)
      ~baseline_shim
  in

  let scrollbar_size =
    Sizef.
      {
        width =
          (if overflow.Point.y = Overflow.Scroll then scrollbar_width else 0.0);
        height =
          (if overflow.Point.x = Overflow.Scroll then scrollbar_width else 0.0);
      }
  in

  let resolved_margin =
    Rectf.
      {
        left = x_margin.start;
        right = x_margin.end_;
        top = y_margin.start;
        bottom = y_margin.end_;
      }
  in

  Tree.set_unrounded_layout tree node
    (Layout.make ~order
       ~location:Pointf.{ x; y }
       ~size:final_size
       ~scrollable_overflow:(Layout_output.scrollable_overflow layout_output)
       ~scrollbar_size ~padding ~border ~margin:resolved_margin);

  (* Add the item to the container's scrollable overflow *)
  Toffee_compute_helpers.add_overflow_contribution overflow_acc
    ~flags:overflow_flags ~origin:overflow_origin ~client:Sizef.zero
    ~padding:Rectf.zero
    ~location:Pointf.{ x; y }
    ~size:final_size
    ~overflow_area:(Layout_output.scrollable_overflow layout_output)
    ~border ~overflow ~margin:resolved_margin;

  (* Return y position and height for baseline alignment *)
  (y, final_size.height)
