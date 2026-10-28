(* Grid layout algorithm implementation This module implements the CSS Grid
   Level 1 specification https://www.w3.org/TR/css-grid-1/ *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree
module Implicit_grid = Implicit_grid
module Explicit_grid = Explicit_grid
module Grid_track_counts = Grid_track_counts
module Cell_occupancy = Cell_occupancy
module Grid_item = Grid_item
module Placement = Placement
module Named = Named

(* Grid layout algorithm This consists of a few phases: - Resolving the explicit
   grid - Placing items (which also resolves the implicit grid) - Track
   (row/column) sizing - Alignment & Final item placement *)
(* [size], a border-box size, less [inset] where it is known. *)
let content_box_size (inset : Rectf.t) (size : float option size) =
  match size with
  | { width = None; height = None } -> size
  | _ -> Sizef.maybe_sub (Rectf.sum_axes inset) size

let compute_grid_layout (type t)
    (module Tree : LAYOUT_PARTIAL_TREE with type t = t) ~tree ~node ~inputs =
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let run_mode = Layout_input.run_mode inputs in

  let style = Tree.get_core_container_style tree node in

  (* 1. Compute "available grid space" *)
  (* https://www.w3.org/TR/css-grid-1/#available-grid-space *)
  let aspect_ratio = Style.aspect_ratio style in
  let calc = Tree.resolve_calc_value tree in
  let padding =
    Style.padding style
    |> Length_percentage.resolve_rect_or_zero parent_size.width calc
  in
  let border =
    Style.border style
    |> Length_percentage.resolve_rect_or_zero parent_size.width calc
  in
  let padding_border = Rectf.add padding border in
  let padding_border_size = Rectf.sum_axes padding_border in
  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_size
    else Sizef.zero
  in

  let min_size =
    Style.min_size style
    |> (fun dims ->
    Size.
      {
        width = Dimension.maybe_resolve dims.width parent_size.width calc;
        height = Dimension.maybe_resolve dims.height parent_size.height calc;
      })
    |> Size.apply_aspect_ratio aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in
  let max_size =
    Style.max_size style
    |> (fun dims ->
    Size.
      {
        width = Dimension.maybe_resolve dims.width parent_size.width calc;
        height = Dimension.maybe_resolve dims.height parent_size.height calc;
      })
    |> Size.apply_aspect_ratio aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in
  let style_size =
    Style.size style
    |> (fun dims ->
    Size.
      {
        width = Dimension.maybe_resolve dims.width parent_size.width calc;
        height = Dimension.maybe_resolve dims.height parent_size.height calc;
      })
    |> Size.apply_aspect_ratio aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in
  let preferred_size =
    if Layout_input.sizing_mode inputs = Sizing_mode.Inherent_size then
      style_size
    else Size.none
  in

  (* Scrollbar gutters are reserved when the `overflow` property is set to
     `Overflow::Scroll`. However, the axes are switched (transposed) because a
     node that scrolls vertically needs *horizontal* space to be reserved for a
     scrollbar *)
  let scrollbar_gutter =
    let overflow = Style.overflow style in
    Point.transpose overflow
    |> Point.map (function
      | Overflow.Scroll -> Style.scrollbar_width style
      | _ -> 0.0)
  in
  (* TODO: make side configurable based on the `direction` property *)
  let content_box_inset = padding_border in
  let content_box_inset =
    {
      content_box_inset with
      right = content_box_inset.right +. scrollbar_gutter.x;
      bottom = content_box_inset.bottom +. scrollbar_gutter.y;
    }
  in
  (* Track sizing works in the content box, so the border-box minimum and
     maximum sizes it stretches and limits the tracks by lose the inset *)
  let inner_min_size = content_box_size content_box_inset min_size
  and inner_max_size = content_box_size content_box_inset max_size in

  let align_content =
    Option.value (Style.align_content style) ~default:Align_content.Stretch
  in
  let justify_content =
    Option.value (Style.justify_content style) ~default:Align_content.Stretch
  in
  let align_items = Style.align_items style in
  let justify_items = Style.justify_items style in

  (* Note: we avoid accessing the grid rows/columns methods more than once as
     this can cause an expensive-ish computation In OCaml, we pass the style
     directly to functions that need it rather than extracting these values
     early *)
  let constrained_available_space =
    ( Size.map2
        (fun opt_size space ->
          match opt_size with
          | Some size -> Available_space.Definite size
          | None -> space)
        (Size.choose_first known_dimensions preferred_size)
        available_space
    |> fun space ->
      Size.
        {
          width =
            Available_space.clamp_or_self space.width min_size.width
              max_size.width;
          height =
            Available_space.clamp_or_self space.height min_size.height
              max_size.height;
        } )
    |> fun space ->
    Size.
      {
        width =
          Available_space.max_or_self space.width
            (Some padding_border_size.width);
        height =
          Available_space.max_or_self space.height
            (Some padding_border_size.height);
      }
  in

  let available_grid_space =
    Size.
      {
        width =
          Available_space.map_definite_value constrained_available_space.width
            (fun space -> space -. Rectf.horizontal_axis_sum content_box_inset);
        height =
          Available_space.map_definite_value constrained_available_space.height
            (fun space -> space -. Rectf.vertical_axis_sum content_box_inset);
      }
  in

  let outer_node_size =
    Size.choose_first known_dimensions preferred_size
    |> Size.clamp_option min_size max_size
    |> Sizef.maybe_max padding_border_size
  in
  let inner_node_size =
    Size.
      {
        width =
          Option.map
            (fun space -> space -. Rectf.horizontal_axis_sum content_box_inset)
            outer_node_size.width;
        height =
          Option.map
            (fun space -> space -. Rectf.vertical_axis_sum content_box_inset)
            outer_node_size.height;
      }
  in

  (* Short-circuit if we can compute size early *)
  match (run_mode, outer_node_size.width, outer_node_size.height) with
  | Run_mode.Compute_size, Some width, Some height ->
      Layout_output.from_outer_size Sizef.{ width; height }
  (* Nor when the width is known and only the width is requested. *)
  | Run_mode.Compute_size, Some width, None
    when Layout_input.axis inputs = Requested_axis.Horizontal ->
      Layout_output.from_outer_size Sizef.{ width; height = 0.0 }
  | _ ->
      (* 2. Resolve the explicit grid *)

      (* This is very similar to the inner_node_size except if the
         inner_node_size is not definite but the node has a min- or max- size
         style then that will be used in its place. *)
      let auto_fit_container_size =
        Size.choose_first outer_node_size max_size |> fun s ->
        Size.choose_first s min_size
        |> Size.clamp_option min_size max_size
        |> Sizef.maybe_max padding_border_size
        |> fun s -> Sizef.maybe_sub (Rectf.sum_axes content_box_inset) s
      in

      (* Determine auto-repeat strategy based on container size constraints *)
      let auto_repeat_fit_strategy =
        Size.choose_first outer_node_size max_size
        |> Size.map (function
          | Some _ -> Explicit_grid.Max_repetitions_that_do_not_overflow
          | None -> Explicit_grid.Min_repetitions_that_do_overflow)
      in

      (* Compute the number of rows and columns in the explicit grid template *)
      let col_auto_repetition_count, grid_template_col_count =
        Explicit_grid.compute_explicit_grid_size_in_axis ~style
          ~auto_fit_container_size:auto_fit_container_size.width
          ~auto_fit_strategy:auto_repeat_fit_strategy.width
          ~resolve_calc_value:calc ~axis:Absolute_axis.Horizontal
      in
      let row_auto_repetition_count, grid_template_row_count =
        Explicit_grid.compute_explicit_grid_size_in_axis ~style
          ~auto_fit_container_size:auto_fit_container_size.height
          ~auto_fit_strategy:auto_repeat_fit_strategy.height
          ~resolve_calc_value:calc ~axis:Absolute_axis.Vertical
      in

      (* Create named line resolver *)
      let name_resolver =
        Named.create style col_auto_repetition_count row_auto_repetition_count
      in

      let explicit_col_count =
        max grid_template_col_count (Named.area_column_count name_resolver)
      in
      let explicit_row_count =
        max grid_template_row_count (Named.area_row_count name_resolver)
      in

      Named.set_explicit_column_count name_resolver explicit_col_count;
      Named.set_explicit_row_count name_resolver explicit_row_count;

      (* 3. Implicit Grid: Estimate Track Counts *)
      (* Estimate the number of rows and columns in the implicit grid (= the entire grid)
         This is necessary as part of placement. Doing it early here is a perf optimisation to reduce allocations. *)
      let est_col_counts, est_row_counts =
        let child_count = Tree.child_count tree node in
        let child_styles =
          Array.make child_count (Tree.get_core_container_style tree node)
        in
        for i = 0 to child_count - 1 do
          let child_id = Tree.get_child_id tree node i in
          child_styles.(i) <- Tree.get_core_container_style tree child_id
        done;
        let child_styles_iter =
          Seq.unfold
            (fun idx ->
              if idx >= child_count then None
              else Some (child_styles.(idx), idx + 1))
            0
        in
        Implicit_grid.compute_grid_size_estimate ~explicit_col_count
          ~explicit_row_count ~child_styles_iter
      in

      (* 4. Grid Item Placement *)
      (* Match items (children) to a definite grid position (row start/end and column start/end position) *)
      let items = ref [] in
      let cell_occupancy_matrix =
        Cell_occupancy.with_track_counts est_col_counts est_row_counts
      in

      Placement.place_grid_items
        (module Tree)
        ~cell_occupancy_matrix ~items ~tree ~parent_node:node
        ~grid_auto_flow:(Style.grid_auto_flow style)
        ~align_items ~justify_items ~named_line_resolver:name_resolver;

      (* Extract track counts from previous step (auto-placement can expand the
         number of tracks) *)
      let final_col_counts =
        Cell_occupancy.track_counts cell_occupancy_matrix
          Absolute_axis.Horizontal
      in
      let final_row_counts =
        Cell_occupancy.track_counts cell_occupancy_matrix Absolute_axis.Vertical
      in

      (* 5. Initialize Tracks *)
      (* Initialize (explicit and implicit) grid tracks (and gutters)
         This resolves the min and max track sizing functions for all tracks and gutters *)
      let columns = ref [] in
      let rows = ref [] in
      Explicit_grid.initialize_grid_tracks ~tracks:columns
        ~counts:final_col_counts ~style ~axis:Absolute_axis.Horizontal
        ~track_has_items:(fun column_index ->
          Cell_occupancy.column_is_occupied cell_occupancy_matrix column_index);
      Explicit_grid.initialize_grid_tracks ~tracks:rows ~counts:final_row_counts
        ~style ~axis:Absolute_axis.Vertical ~track_has_items:(fun row_index ->
          Cell_occupancy.row_is_occupied cell_occupancy_matrix row_index);

      (* Convert lists to arrays for track sizing *)
      let columns = Array.of_list !columns in
      let rows = Array.of_list !rows in
      let items = Array.of_list !items in

      (* 6. Track Sizing *)

      (* Convert grid placements in origin-zero coordinates to indexes into the
         GridTrack (rows and columns) vectors This computation is relatively
         trivial, but it requires the final number of negative (implicit) tracks
         in each axis, and doing it up-front here means we don't have to keep
         repeating that calculation *)
      Track_sizing.resolve_item_track_indexes items final_col_counts
        final_row_counts;

      (* For each item, and in each axis, determine whether the item crosses any
         flexible (fr) tracks Record this as a boolean (per-axis) on each item
         for later use in the track-sizing algorithm *)
      Track_sizing.determine_if_item_crosses_flexible_or_intrinsic_tracks items
        columns rows;

      (* Determine if the grid has any baseline aligned items *)
      let has_baseline_aligned_item =
        Array.exists Grid_item.participates_in_baseline_alignment items
      in

      (* Run track sizing algorithm for Inline axis *)
      Track_sizing.track_sizing_algorithm
        (module Tree)
        tree Abstract_axis.Inline
        (Size.get Abstract_axis.Inline inner_min_size)
        (Size.get Abstract_axis.Inline inner_max_size)
        justify_content align_content available_grid_space inner_node_size
        columns rows items
        (fun track parent_size tree ->
          Grid.Track_sizing_function.Max.definite_value_with_calc
            track.Grid_track.track_sizing_function parent_size
            (Tree.resolve_calc_value tree))
        has_baseline_aligned_item;

      let initial_column_sum =
        Array.fold_left
          (fun acc track -> acc +. track.Grid_track.base_size)
          0.0 columns
      in
      let inner_node_size =
        {
          inner_node_size with
          width =
            Option.value inner_node_size.width ~default:initial_column_sum
            |> Option.some;
        }
      in

      (* Clear caches before block axis sizing *)
      Array.iter
        (fun item -> item.Grid_item.available_space_cache <- None)
        items;

      (* Run track sizing algorithm for Block axis *)
      Track_sizing.track_sizing_algorithm
        (module Tree)
        tree Abstract_axis.Block
        (Size.get Abstract_axis.Block inner_min_size)
        (Size.get Abstract_axis.Block inner_max_size)
        align_content justify_content available_grid_space inner_node_size rows
        columns items
        (fun track _ _ -> Some track.Grid_track.base_size)
        false;

      (* TODO: Support baseline alignment in vertical axis *)
      let initial_row_sum =
        Array.fold_left
          (fun acc track -> acc +. track.Grid_track.base_size)
          0.0 rows
      in

      (* 6. Compute container size *)
      let resolved_style_size =
        Size.choose_first known_dimensions preferred_size
      in
      let container_border_box =
        Sizef.
          {
            width =
              (match Size.get Abstract_axis.Inline resolved_style_size with
                | Some w -> w
                | None ->
                    initial_column_sum
                    +. Rectf.horizontal_axis_sum content_box_inset)
              |> (fun w ->
              match (min_size.width, max_size.width) with
              | Some min_w, Some max_w -> Float.max min_w (Float.min max_w w)
              | Some min_w, None -> Float.max min_w w
              | None, Some max_w -> Float.min max_w w
              | None, None -> w)
              |> Float.max padding_border_size.width;
            height =
              (match Size.get Abstract_axis.Block resolved_style_size with
                | Some h -> h
                | None ->
                    initial_row_sum +. Rectf.vertical_axis_sum content_box_inset)
              |> (fun h ->
              match (min_size.height, max_size.height) with
              | Some min_h, Some max_h -> Float.max min_h (Float.min max_h h)
              | Some min_h, None -> Float.max min_h h
              | None, Some max_h -> Float.min max_h h
              | None, None -> h)
              |> Float.max padding_border_size.height;
          }
      in
      let container_content_box =
        Sizef.
          {
            width =
              Float.max 0.0
                (container_border_box.width
                -. Rectf.horizontal_axis_sum content_box_inset);
            height =
              Float.max 0.0
                (container_border_box.height
                -. Rectf.vertical_axis_sum content_box_inset);
          }
      in

      (* Whether an item's min-content contribution changed when the tracks
         of the other axis were sized, which re-runs the sizing of its axis
         and sizes a container sized by its content again *)
      let column_contribution_changed = ref false
      and row_contribution_changed = ref false in
      (* The container's size is now determined: the re-runs resolve
         percentages against its content box *)
      let inner_node_size =
        Size.
          {
            width = Some container_content_box.width;
            height = Some container_content_box.height;
          }
      in

      (* An item's min-content contribution in [axis], measured in its grid
         area now that the tracks of the other axis are sized. It replaces the
         item's cached contributions in [axis] and tells whether it changed. *)
      let contribution_changed item axis axis_tracks other_axis_tracks =
        let grid_area_size =
          Grid_item.grid_area_size item axis axis_tracks other_axis_tracks
            inner_node_size
            (fun track _ -> Some track.Grid_track.base_size)
            (Tree.resolve_calc_value tree)
        in
        let available_space = Size.set axis None grid_area_size in
        let contribution =
          Grid_item.min_content_contribution
            (module Tree)
            item axis tree grid_area_size available_space
        in
        let changed =
          Some contribution
          <> Size.get axis item.Grid_item.min_content_contribution_cache
        in
        item.Grid_item.available_space_cache <- Some grid_area_size;
        item.Grid_item.min_content_contribution_cache <-
          Size.set axis (Some contribution)
            item.Grid_item.min_content_contribution_cache;
        item.Grid_item.max_content_contribution_cache <-
          Size.set axis None item.Grid_item.max_content_contribution_cache;
        item.Grid_item.minimum_contribution_cache <-
          Size.set axis None item.Grid_item.minimum_contribution_cache;
        changed
      in
      let any_contribution_changed axis axis_tracks other_axis_tracks =
        let crosses item =
          match axis with
          | Abstract_axis.Inline -> item.Grid_item.crosses_intrinsic_column
          | Abstract_axis.Block -> item.Grid_item.crosses_intrinsic_row
        in
        let changed = ref false in
        let len = Array.length items in
        let i = ref 0 in
        while (not !changed) && !i < len do
          let item = items.(!i) in
          if crosses item then
            changed :=
              contribution_changed item axis axis_tracks other_axis_tracks;
          incr i
        done;
        !changed
      in
      let clear_caches axis =
        Array.iter
          (fun item ->
            item.Grid_item.available_space_cache <- None;
            item.Grid_item.min_content_contribution_cache <-
              Size.set axis None item.Grid_item.min_content_contribution_cache;
            item.Grid_item.max_content_contribution_cache <-
              Size.set axis None item.Grid_item.max_content_contribution_cache;
            item.Grid_item.minimum_contribution_cache <-
              Size.set axis None item.Grid_item.minimum_contribution_cache)
          items
      in

      (* Column sizing is re-run (once) if the container's width was
         indefinite and a column has a percentage sizing function, which
         resolved against nothing, or if the min-content contribution of an
         item crossing an intrinsic column changed now that the rows are
         sized. The contributions are re-measured from scratch on a re-run.
         TODO: Only re-run the tracks that need it. *)
      let rerun_column_sizing =
        Option.is_none outer_node_size.width
        && Array.exists Grid_track.uses_percentage columns
        ||
        (column_contribution_changed :=
           any_contribution_changed Abstract_axis.Inline columns rows;
         !column_contribution_changed)
      in
      (* Row sizing likewise, for the container's height and percentage
         rows, or for contributions the re-sized columns changed *)
      let rerun_row_sizing =
        Option.is_none outer_node_size.height
        && Array.exists Grid_track.uses_percentage rows
      in
      if rerun_column_sizing then (
        clear_caches Abstract_axis.Inline;
        Track_sizing.track_sizing_algorithm
          (module Tree)
          tree Abstract_axis.Inline
          (Size.get Abstract_axis.Inline inner_min_size)
          (Size.get Abstract_axis.Inline inner_max_size)
          justify_content align_content available_grid_space inner_node_size
          columns rows items
          (fun track _ _ -> Some track.Grid_track.base_size)
          has_baseline_aligned_item);
      let rerun_row_sizing =
        rerun_row_sizing
        || rerun_column_sizing
           &&
           (row_contribution_changed :=
              any_contribution_changed Abstract_axis.Block rows columns;
            !row_contribution_changed)
      in
      if rerun_row_sizing then (
        clear_caches Abstract_axis.Block;
        Track_sizing.track_sizing_algorithm
          (module Tree)
          tree Abstract_axis.Block
          (Size.get Abstract_axis.Block inner_min_size)
          (Size.get Abstract_axis.Block inner_max_size)
          align_content justify_content available_grid_space inner_node_size
          rows columns items
          (fun track _ _ -> Some track.Grid_track.base_size)
          false (* TODO: Support baseline alignment in vertical axis *));

      (* A container sized by its content takes the tracks' new sizes
           where an item's contribution changed them (without percentage
           tracks, which its old size resolved) *)
      let resize changed tracks style_size min max padding_border inset_sum size
          =
        if
          !changed
          && (not (Array.exists Grid_track.uses_percentage tracks))
          && Option.is_none style_size
        then
          let sum =
            Array.fold_left
              (fun acc track -> acc +. track.Grid_track.base_size)
              0.0 tracks
            +. inset_sum
          in
          let sum =
            match max with Some max -> Float.min max sum | None -> sum
          in
          let sum =
            match min with Some min -> Float.max min sum | None -> sum
          in
          Float.max sum padding_border
        else size
      in
      let container_border_box =
        Sizef.
          {
            width =
              resize column_contribution_changed columns
                (Size.get Abstract_axis.Inline resolved_style_size)
                min_size.width max_size.width padding_border_size.width
                (Rectf.horizontal_axis_sum content_box_inset)
                container_border_box.width;
            height =
              resize row_contribution_changed rows
                (Size.get Abstract_axis.Block resolved_style_size)
                min_size.height max_size.height padding_border_size.height
                (Rectf.vertical_axis_sum content_box_inset)
                container_border_box.height;
          }
      in
      let container_content_box =
        Sizef.
          {
            width =
              Float.max 0.0
                (container_border_box.width
                -. Rectf.horizontal_axis_sum content_box_inset);
            height =
              Float.max 0.0
                (container_border_box.height
                -. Rectf.vertical_axis_sum content_box_inset);
          }
      in

      (* If only the container's size has been requested *)
      if run_mode = Run_mode.Compute_size then
        Layout_output.from_outer_size container_border_box
      else (
        (* 8. Track Alignment *)

        (* Align columns *)
        Alignment.align_tracks
          ~grid_container_content_box_size:
            (Sizef.get Abstract_axis.Inline container_content_box)
          ~padding:Line.{ start = padding.left; end_ = padding.right }
          ~border:Line.{ start = border.left; end_ = border.right }
          ~tracks:columns ~track_alignment_style:justify_content;

        (* Align rows *)
        Alignment.align_tracks
          ~grid_container_content_box_size:
            (Sizef.get Abstract_axis.Block container_content_box)
          ~padding:Line.{ start = padding.top; end_ = padding.bottom }
          ~border:Line.{ start = border.top; end_ = border.bottom }
          ~tracks:rows ~track_alignment_style:align_content;

        (* 9. Size, Align, and Position Grid Items *)
        let item_content_size_contribution = ref Sizef.zero in

        (* Sort items back into original order to allow them to be matched up
           with styles *)
        Array.sort
          (fun a b ->
            Int.compare a.Grid_item.source_order b.Grid_item.source_order)
          items;

        let container_alignment_styles =
          In_both_abs_axis.
            { horizontal = justify_items; vertical = align_items }
        in

        (* Position in-flow children (stored in items array) *)
        Array.iteri
          (fun index item ->
            let grid_area =
              {
                Rectf.top =
                  rows.(item.Grid_item.row_indexes.Line.start + 1)
                    .Grid_track.offset;
                bottom =
                  rows.(item.Grid_item.row_indexes.Line.end_).Grid_track.offset;
                left =
                  columns.(item.Grid_item.column_indexes.Line.start + 1)
                    .Grid_track.offset;
                right =
                  columns.(item.Grid_item.column_indexes.Line.end_)
                    .Grid_track.offset;
              }
            in
            let content_size_contribution, y_position, height =
              Alignment.align_and_position_item
                (module Tree)
                ~tree ~node:item.Grid_item.node ~order:index ~grid_area
                ~container_alignment_styles
                ~baseline_shim:item.Grid_item.baseline_shim
            in
            item.Grid_item.y_position <- y_position;
            item.Grid_item.height <- height;
            item_content_size_contribution :=
              Sizef.max
                !item_content_size_contribution
                content_size_contribution)
          items;

        (* Position hidden and absolutely positioned children *)
        let order = ref (Array.length items) in
        for index = 0 to Tree.child_count tree node - 1 do
          let child = Tree.get_child_id tree node index in
          let child_style = Tree.get_core_container_style tree child in

          (* Position hidden child *)
          if Style.box_generation_mode child_style = Box_generation_mode.None
          then
            let _ =
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
            in
            incr order (* Position absolutely positioned child *)
          else if Style.position child_style = Position.Absolute then (
            (* Convert grid-col-{start/end} into Option's of indexes into the
               columns vector *)
            let maybe_col_indexes =
              let grid_placement = Style.grid_column child_style in
              let resolved =
                Named.resolve_column_names name_resolver grid_placement
              in
              let oz_placement =
                Grid.Placement.Line.into_origin_zero resolved
                  (Grid_track_counts.explicit final_col_counts)
              in
              let resolved_abs =
                Named.resolve_absolutely_positioned_grid_tracks oz_placement
              in
              Line.map
                (fun maybe_line ->
                  Option.bind maybe_line (fun line ->
                      Grid_track_counts.oz_line_to_track final_col_counts line))
                resolved_abs
            in
            (* Convert grid-row-{start/end} into Option's of indexes into the
               row vector *)
            let maybe_row_indexes =
              let grid_placement = Style.grid_row child_style in
              let resolved =
                Named.resolve_row_names name_resolver grid_placement
              in
              let oz_placement =
                Grid.Placement.Line.into_origin_zero resolved
                  (Grid_track_counts.explicit final_row_counts)
              in
              let resolved_abs =
                Named.resolve_absolutely_positioned_grid_tracks oz_placement
              in
              Line.map
                (fun maybe_line ->
                  Option.bind maybe_line (fun line ->
                      Grid_track_counts.oz_line_to_track final_row_counts line))
                resolved_abs
            in

            let grid_area =
              Rectf.
                {
                  top =
                    (match maybe_row_indexes.start with
                    | Some index -> rows.(index).offset
                    | None -> border.top);
                  bottom =
                    (match maybe_row_indexes.end_ with
                    | Some index -> rows.(index).offset
                    | None ->
                        container_border_box.height -. border.bottom
                        -. scrollbar_gutter.y);
                  left =
                    (match maybe_col_indexes.start with
                    | Some index -> columns.(index).offset
                    | None -> border.left);
                  right =
                    (match maybe_col_indexes.end_ with
                    | Some index -> columns.(index).offset
                    | None ->
                        container_border_box.width -. border.right
                        -. scrollbar_gutter.x);
                }
            in

            (* TODO: Baseline alignment support for absolutely positioned
               items *)
            let content_size_contribution, _, _ =
              Alignment.align_and_position_item
                (module Tree)
                ~tree ~node:child ~order:!order ~grid_area
                ~container_alignment_styles ~baseline_shim:0.0
            in
            item_content_size_contribution :=
              Sizef.max
                !item_content_size_contribution
                content_size_contribution;
            incr order)
        done;

        (* TODO: Set detailed grid information if needed *)

        (* If there are no items then return just the container size (no
           baseline) *)
        if Array.length items = 0 then
          Layout_output.from_outer_size container_border_box
        else
          (* Determine the grid container baseline(s) (currently we only compute
             the first baseline) *)
          let grid_container_baseline =
            (* Sort items by row start position so that we can iterate items in
               groups which are in the same row *)
            let sorted_items = Array.copy items in
            Array.sort
              (fun a b ->
                Int.compare a.Grid_item.row_indexes.start
                  b.Grid_item.row_indexes.start)
              sorted_items;

            (* Get the row index of the first row containing items *)
            let first_row = sorted_items.(0).Grid_item.row_indexes.start in

            (* Get all items in the first row *)
            let first_row_items =
              sorted_items |> Array.to_list
              |> List.filter (fun item ->
                  item.Grid_item.row_indexes.start = first_row)
              |> Array.of_list
            in

            (* The first item of the row in baseline alignment, else the
               row's first item *)
            let item =
              match
                Array.find_opt Grid_item.participates_in_baseline_alignment
                  first_row_items
              with
              | Some item -> item
              | None -> first_row_items.(0)
            in

            item.Grid_item.y_position
            +. Option.value item.Grid_item.baseline
                 ~default:item.Grid_item.height
          in

          Layout_output.from_sizes_and_baselines container_border_box
            !item_content_size_contribution
            Point.{ x = None; y = Some grid_container_baseline })
