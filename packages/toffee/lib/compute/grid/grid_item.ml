(* Contains GridItem used to represent a single grid item during layout *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Style.Grid
open Tree

(* Type alias for origin zero line *)
type origin_zero_line = int

(* Represents a single grid item *)
type t = {
  node : Node_id.t; (* The id of the node that this item represents *)
  source_order : int;
      (* The order of the item in the children array We sort the list of grid
         items during track sizing. This field allows us to sort back the
         original order for final positioning *)
  row : origin_zero_line line;
      (* The item's definite row-start and row-end, as resolved by the placement
         algorithm (in origin-zero coordinates) *)
  column : origin_zero_line line;
      (* The items definite column-start and column-end, as resolved by the
         placement algorithm (in origin-zero coordinates) *)
  is_compressible_replaced : bool;
      (* Is it a compressible replaced element?
         https://drafts.csswg.org/css-sizing-3/#min-content-zero *)
  overflow : overflow point; (* The item's overflow style *)
  box_sizing : box_sizing; (* The item's box_sizing style *)
  size : dimension size; (* The item's size style *)
  min_size : dimension size; (* The item's min_size style *)
  max_size : dimension size; (* The item's max_size style *)
  aspect_ratio : float option; (* The item's aspect_ratio style *)
  padding : length_percentage rect; (* The item's padding style *)
  border : length_percentage rect; (* The item's border style *)
  margin : length_percentage_auto rect; (* The item's margin style *)
  align_self : align_self;
      (* The item's align_self property, or the parent's align_items property is
         not set *)
  justify_self : justify_self;
      (* The item's justify_self property, or the parent's justify_items
         property is not set *)
  mutable baseline : float option; (* The items first baseline (horizontal) *)
  mutable baseline_shim : float;
      (* Shim for baseline alignment that acts like an extra top margin *)
  mutable row_indexes : int line;
      (* The item's definite row-start and row-end (same as `row` field, except
         in a different coordinate system) (as indexes into the Vec<GridTrack>
         stored in a grid's AbstractAxisTracks) *)
  mutable column_indexes : int line;
      (* The items definite column-start and column-end (same as `column` field,
         except in a different coordinate system) (as indexes into the
         Vec<GridTrack> stored in a grid's AbstractAxisTracks) *)
  mutable crosses_flexible_row : bool;
      (* Whether the item crosses a flexible row *)
  mutable crosses_flexible_column : bool;
      (* Whether the item crosses a flexible column *)
  mutable crosses_intrinsic_row : bool;
      (* Whether the item crosses a intrinsic row *)
  mutable crosses_intrinsic_column : bool;
  (* Whether the item crosses a intrinsic column *)
  (* Caches for intrinsic size computation. These caches are only valid for a single run of the track-sizing algorithm. *)
  mutable available_space_cache : float option size option;
      (* Cache for the known_dimensions input to intrinsic sizing computation *)
  mutable min_content_contribution_cache : float option size;
      (* Cache for the min-content size *)
  mutable minimum_contribution_cache : float option size;
      (* Cache for the minimum contribution *)
  mutable max_content_contribution_cache : float option size;
      (* Cache for the max-content size *)
  mutable y_position : float;
      (* Final y position. Used to compute baseline alignment for the
         container *)
  mutable height : float;
      (* Final height. Used to compute baseline alignment for the container *)
}

(* An item's alignment, its own or else the container's. With neither, the
   [normal] alignment stretches an item, but sizes a replaced element as
   [Start] does (CSS Grid 6.2). *)
let self_alignment style own container =
  match (own, container) with
  | Some alignment, _ | None, Some alignment -> alignment
  | None, None ->
      if Style.item_is_replaced style then Align_items.Start
      else Align_items.Stretch

(* Create a new item given a concrete placement in both axes *)
let new_with_placement_style_and_order ~node ~col_span ~row_span ~style
    ~parent_align_items ~parent_justify_items ~source_order =
  {
    node;
    source_order;
    row = row_span;
    column = col_span;
    is_compressible_replaced = Style.is_compressible_replaced style;
    overflow = Style.overflow style;
    box_sizing = Style.box_sizing style;
    size = Style.size style;
    min_size = Style.min_size style;
    max_size = Style.max_size style;
    aspect_ratio = Style.aspect_ratio style;
    padding = Style.padding style;
    border = Style.border style;
    margin = Style.margin style;
    align_self =
      self_alignment style (Style.align_self style) parent_align_items;
    justify_self =
      self_alignment style (Style.justify_self style) parent_justify_items;
    baseline = None;
    baseline_shim = 0.0;
    row_indexes = Line.make 0 0;
    (* Properly initialised later *)
    column_indexes = Line.make 0 0;
    (* Properly initialised later *)
    crosses_flexible_row = false;
    (* Properly initialised later *)
    crosses_flexible_column = false;
    (* Properly initialised later *)
    crosses_intrinsic_row = false;
    (* Properly initialised later *)
    crosses_intrinsic_column = false;
    (* Properly initialised later *)
    available_space_cache = None;
    min_content_contribution_cache = Size.none;
    max_content_contribution_cache = Size.none;
    minimum_contribution_cache = Size.none;
    y_position = 0.0;
    height = 0.0;
  }

(* This item's placement in the specified axis in OriginZero coordinates *)
let placement t axis =
  match axis with
  | Abstract_axis.Block -> t.row
  | Abstract_axis.Inline -> t.column

(* This item's placement in the specified axis as GridTrackVec indices *)
let placement_indexes t axis =
  match axis with
  | Abstract_axis.Block -> t.row_indexes
  | Abstract_axis.Inline -> t.column_indexes

(* Returns a range which can be used as an index into the GridTrackVec in the
   specified axis which will produce a sub-slice of covering all the tracks and
   lines that this item spans excluding the lines that bound it *)
let track_range_excluding_lines t axis =
  let indexes = placement_indexes t axis in
  (indexes.start + 1, indexes.end_)

(* Returns the number of tracks that this item spans in the specified axis *)
let span t axis =
  match axis with
  | Abstract_axis.Block -> max (t.row.end_ - t.row.start) 0
  | Abstract_axis.Inline -> max (t.column.end_ - t.column.start) 0

(* Returns the pre-computed value indicating whether the grid item crosses a
   flexible track in the specified axis *)
let crosses_flexible_track t axis =
  match axis with
  | Abstract_axis.Inline -> t.crosses_flexible_column
  | Abstract_axis.Block -> t.crosses_flexible_row

(* Returns the pre-computed value indicating whether the grid item crosses an
   intrinsic track in the specified axis *)
let crosses_intrinsic_track t axis =
  match axis with
  | Abstract_axis.Inline -> t.crosses_intrinsic_column
  | Abstract_axis.Block -> t.crosses_intrinsic_row

(* For an item spanning multiple tracks, the upper limit used to calculate its
   limited min-/max-content contribution is the sum of the fixed max track
   sizing functions of any tracks it spans, and is applied if it only spans such
   tracks *)
let spanned_track_limit t axis axis_tracks axis_parent_size resolve_calc_value =
  let start_idx, end_idx = track_range_excluding_lines t axis in
  let spanned_tracks = Array.sub axis_tracks start_idx (end_idx - start_idx) in
  let tracks_all_fixed =
    Array.for_all
      (fun track ->
        (* Check if the max track sizing function has a definite limit *)
        Option.is_some
          (Track_sizing_function.Max.definite_limit_with_calc
             track.Grid_track.track_sizing_function axis_parent_size
             resolve_calc_value))
      spanned_tracks
  in
  if tracks_all_fixed then
    let limit =
      Array.fold_left
        (fun acc track ->
          let limit =
            Option.get
              (Track_sizing_function.Max.definite_limit_with_calc
                 track.Grid_track.track_sizing_function axis_parent_size
                 resolve_calc_value)
          in
          acc +. limit)
        0.0 spanned_tracks
    in
    Some limit
  else None

(* Compute the known_dimensions to be passed to the child sizing functions The
   key thing that is being done here is applying stretch alignment, which is
   necessary to allow percentage sizes further down the tree to resolve properly
   in some cases *)
let known_dimensions (type tree)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = tree) (t : t)
    (tree : tree) (inner_node_size : float option size)
    (grid_area_size : float option size) : float option size =
  let calc = Tree.resolve_calc_value tree in

  (* Compute margins *)
  let margins =
    let inner_node_width = inner_node_size.width in
    let margin_rect =
      Rectf.
        {
          left =
            Length_percentage_auto.resolve_or_zero t.margin.left (Some 0.0) calc;
          right =
            Length_percentage_auto.resolve_or_zero t.margin.right (Some 0.0)
              calc;
          top =
            Length_percentage_auto.resolve_or_zero t.margin.top inner_node_width
              calc
            +. t.baseline_shim;
          bottom =
            Length_percentage_auto.resolve_or_zero t.margin.bottom
              inner_node_width calc;
        }
    in
    Rectf.sum_axes margin_rect
  in

  (* Resolve padding and border *)
  let padding =
    Length_percentage.resolve_rect_or_zero grid_area_size.width calc t.padding
  in
  let border =
    Length_percentage.resolve_rect_or_zero grid_area_size.width calc t.border
  in
  let padding_border_size = Rectf.sum_axes (Rectf.add padding border) in

  let box_sizing_adjustment =
    if t.box_sizing = Box_sizing.Content_box then padding_border_size
    else Sizef.zero
  in

  (* Resolve inherent size *)
  let inherent_size =
    Size.map2
      (fun dim grid_size -> Dimension.maybe_resolve dim grid_size calc)
      t.size grid_area_size
    |> Size.apply_aspect_ratio t.aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in

  (* Resolve min/max sizes *)
  let min_size =
    Size.map2
      (fun dim grid_size -> Dimension.maybe_resolve dim grid_size calc)
      t.min_size grid_area_size
    |> Size.apply_aspect_ratio t.aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in

  let max_size =
    Size.map2
      (fun dim grid_size -> Dimension.maybe_resolve dim grid_size calc)
      t.max_size grid_area_size
    |> Size.apply_aspect_ratio t.aspect_ratio
    |> Sizef.maybe_add box_sizing_adjustment
  in

  let grid_area_minus_item_margins_size =
    Sizef.maybe_sub margins grid_area_size
  in

  (* Apply stretch alignment for width *)
  let width =
    match inherent_size.width with
    | Some w -> Some w
    | None ->
        if
          (not (Length_percentage_auto.is_auto t.margin.left))
          && (not (Length_percentage_auto.is_auto t.margin.right))
          && t.justify_self = Stretch
        then grid_area_minus_item_margins_size.width
        else None
  in

  (* Reapply aspect ratio after stretch *)
  let size_after_width = Size.{ width; height = inherent_size.height } in
  let size_after_width =
    Size.apply_aspect_ratio t.aspect_ratio size_after_width
  in

  (* Apply stretch alignment for height *)
  let height =
    match size_after_width.height with
    | Some h -> Some h
    | None ->
        if
          (not (Length_percentage_auto.is_auto t.margin.top))
          && (not (Length_percentage_auto.is_auto t.margin.bottom))
          && t.align_self = Stretch
        then grid_area_minus_item_margins_size.height
        else None
  in

  (* Reapply aspect ratio after stretch *)
  let final_size = Size.{ width = size_after_width.width; height } in
  let final_size = Size.apply_aspect_ratio t.aspect_ratio final_size in

  (* Clamp by min/max *)
  Size.clamp_option min_size max_size final_size

(* Similar to the spanned_track_limit, but excludes FitContent arguments from
   the limit. Used to clamp the automatic minimum contributions of an item *)
let spanned_fixed_track_limit t axis axis_tracks axis_parent_size
    resolve_calc_value =
  let start_idx, end_idx = track_range_excluding_lines t axis in
  let spanned_tracks = Array.sub axis_tracks start_idx (end_idx - start_idx) in
  let tracks_all_fixed =
    Array.for_all
      (fun track ->
        (* Check if the max track sizing function has a definite value *)
        Option.is_some
          (Track_sizing_function.Max.definite_value_with_calc
             track.Grid_track.track_sizing_function axis_parent_size
             resolve_calc_value))
      spanned_tracks
  in
  if tracks_all_fixed then
    let limit =
      Array.fold_left
        (fun acc track ->
          let limit =
            Option.get
              (Track_sizing_function.Max.definite_value_with_calc
                 track.Grid_track.track_sizing_function axis_parent_size
                 resolve_calc_value)
          in
          acc +. limit)
        0.0 spanned_tracks
    in
    Some limit
  else None

(* Compute the item's min content contribution from the provided parameters *)
let min_content_contribution (type tree)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = tree) t axis tree
    available_space inner_node_size =
  let known_dimensions =
    known_dimensions (module Tree) t tree inner_node_size available_space
  in
  Tree.compute_child_layout tree t.node
    (Layout_input.make ~known_dimensions ~parent_size:inner_node_size
       ~available_space:
         (Size.map
            (fun opt ->
              match opt with
              | Some size -> Available_space.Definite size
              | None -> Available_space.Min_content)
            available_space)
       ~sizing_mode:Sizing_mode.Inherent_size ~run_mode:Run_mode.Compute_size
       ~axis:
         (match axis with
         | Abstract_axis.Inline -> Requested_axis.Horizontal
         | Abstract_axis.Block -> Requested_axis.Vertical)
       ~vertical_margins_are_collapsible:Line.both_false)
  |> Layout_output.size
  |> fun s ->
  match axis with
  | Abstract_axis.Inline -> s.width
  | Abstract_axis.Block -> s.height

(* Retrieve the item's min content contribution from the cache or compute it
   using the provided parameters *)
let min_content_contribution_cached t axis (type tree)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = tree) tree
    available_space inner_node_size =
  match Size.get axis t.min_content_contribution_cache with
  | Some cached_value -> cached_value
  | None ->
      let contribution =
        min_content_contribution
          (module Tree)
          t axis tree available_space inner_node_size
      in
      t.min_content_contribution_cache <-
        Size.set axis (Some contribution) t.min_content_contribution_cache;
      contribution

(* Compute the item's max content contribution from the provided parameters *)
let max_content_contribution (type tree)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = tree) t axis tree
    available_space inner_node_size =
  let known_dimensions =
    known_dimensions (module Tree) t tree inner_node_size available_space
  in
  Tree.compute_child_layout tree t.node
    (Layout_input.make ~known_dimensions ~parent_size:inner_node_size
       ~available_space:
         (Size.map
            (fun opt ->
              match opt with
              | Some size -> Available_space.Definite size
              | None -> Available_space.Max_content)
            available_space)
       ~sizing_mode:Sizing_mode.Inherent_size ~run_mode:Run_mode.Compute_size
       ~axis:
         (match axis with
         | Abstract_axis.Inline -> Requested_axis.Horizontal
         | Abstract_axis.Block -> Requested_axis.Vertical)
       ~vertical_margins_are_collapsible:Line.both_false)
  |> Layout_output.size
  |> fun s ->
  match axis with
  | Abstract_axis.Inline -> s.width
  | Abstract_axis.Block -> s.height

(* Retrieve the item's max content contribution from the cache or compute it
   using the provided parameters *)
let max_content_contribution_cached t axis (type tree)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = tree) tree
    available_space inner_node_size =
  match Size.get axis t.max_content_contribution_cache with
  | Some cached_value -> cached_value
  | None ->
      let contribution =
        max_content_contribution
          (module Tree)
          t axis tree available_space inner_node_size
      in
      t.max_content_contribution_cache <-
        Size.set axis (Some contribution) t.max_content_contribution_cache;
      contribution

(* Compute the available space for an item in a given axis based on the tracks
   it spans *)
let available_space t axis other_axis_tracks other_axis_available_space
    get_track_size_estimate =
  let other_axis = Abstract_axis.other axis in
  let start_idx, end_idx = track_range_excluding_lines t other_axis in
  let spanned_tracks =
    Array.sub other_axis_tracks start_idx (end_idx - start_idx)
  in
  let item_other_axis_size =
    Array.fold_left
      (fun acc track ->
        match acc with
        | None -> None
        | Some sum -> (
            match get_track_size_estimate track other_axis_available_space with
            | None -> None
            | Some size ->
                Some
                  (sum +. size +. track.Grid_track.content_alignment_adjustment)
            ))
      (Some 0.0) spanned_tracks
  in
  Size.set other_axis item_other_axis_size Size.none

let participates_in_baseline_alignment t =
  t.align_self = Align_items.Baseline
  && (not (Length_percentage_auto.is_auto t.margin.top))
  && not (Length_percentage_auto.is_auto t.margin.bottom)
