(* Flexbox layout computation algorithm *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

(* Flex direction helpers *)
module Flex_direction_ext = struct
  open Flex_direction

  let is_row dir =
    match dir with
    | Row | Row_reverse -> true
    | Column | Column_reverse -> false

  let _is_column dir = not (is_row dir)

  let is_reverse dir =
    match dir with
    | Row_reverse | Column_reverse -> true
    | Row | Column -> false

  let main_size size dir =
    if is_row dir then size.Size.width else size.Size.height

  let _cross_size size dir =
    if is_row dir then size.Size.height else size.Size.width

  let _with_cross_size size dir value =
    if is_row dir then Size.{ size with height = value }
    else Size.{ size with width = value }

  let main_axis_sum rect dir =
    if is_row dir then Rect.horizontal_axis_sum rect
    else Rect.vertical_axis_sum rect

  let _cross_axis_sum rect dir =
    if is_row dir then Rect.vertical_axis_sum rect
    else Rect.horizontal_axis_sum rect

  (* Rect accessors based on flex direction *)
  let main_start rect dir = if is_row dir then rect.Rect.left else rect.Rect.top

  let main_end rect dir =
    if is_row dir then rect.Rect.right else rect.Rect.bottom

  let cross_start rect dir =
    if is_row dir then rect.Rect.top else rect.Rect.left

  let cross_end rect dir =
    if is_row dir then rect.Rect.bottom else rect.Rect.right

  (* Point accessors based on flex direction *)
  let _main_axis_pt pt dir = if is_row dir then pt.Point.x else pt.Point.y
  let _cross_axis_pt pt dir = if is_row dir then pt.Point.y else pt.Point.x
end

(* The intermediate results of a flexbox calculation for a single item *)
type flex_item = {
  node : Node_id.t; (* The identifier for the associated node *)
  order : int; (* The order of the node relative to its siblings *)
  (* Size constraints *)
  size : float option size; (* The base size of this item *)
  min_size : float option size; (* The minimum allowable size of this item *)
  max_size : float option size; (* The maximum allowable size of this item *)
  align_self : align_self; (* The cross-alignment of this item *)
  (* Overflow and scrollbar *)
  overflow : overflow point; (* The overflow style of the item *)
  scrollbar_width : float; (* The width of the scrollbars (if it has any) *)
  (* Flex properties *)
  flex_shrink : float; (* The flex shrink style of the item *)
  flex_grow : float; (* The flex grow style of the item *)
  (* Resolved sizes *)
  mutable resolved_minimum_main_size : float; (* The minimum size of the item *)
  (* Positioning *)
  inset : float option rect; (* The final offset of this item *)
  mutable margin : float rect; (* The margin of this item *)
  margin_is_auto : bool rect; (* Whether each margin is an auto margin or not *)
  padding : float rect; (* The padding of this item *)
  border : float rect; (* The border of this item *)
  (* Flex basis *)
  mutable flex_basis : float; (* The default size of this item *)
  mutable inner_flex_basis : float;
  (* The default size of this item, minus padding and border *)
  (* Layout state *)
  mutable violation : float;
      (* The amount by which this item has deviated from its target size *)
  mutable frozen : bool; (* Is the size of this item locked *)
  (* Content flex fraction *)
  mutable content_flex_fraction : float;
  (* Either the max- or min- content flex fraction *)
  (* Hypothetical and target sizes (flattened) *)
  mutable hypothetical_inner_width : float;
  mutable hypothetical_inner_height : float;
  mutable hypothetical_outer_width : float;
  mutable hypothetical_outer_height : float;
  mutable target_width : float; (* The size that this item wants to be *)
  mutable target_height : float;
  mutable outer_target_width : float;
  mutable outer_target_height : float;
  (* Baseline *)
  mutable baseline : float; (* The position of the bottom edge of this item *)
  (* Offsets *)
  mutable offset_main : float; (* A temporary value for the main offset *)
  mutable offset_cross : float; (* A temporary value for the cross offset *)
}

(* A line of flex_items used for intermediate computation *)
type flex_line = {
  items : flex_item array;
      (* The slice of items to iterate over during computation of this line *)
  mutable cross_size : float; (* The dimensions of the cross-axis *)
  mutable offset_cross : float; (* The relative offset of the cross-axis *)
}

(* Values that can be cached during the flexbox algorithm *)
type algo_constants = {
  (* Direction *)
  dir : flex_direction;
      (* The direction of the current segment being laid out *)
  is_row : bool; (* Is this segment a row *)
  is_wrap : bool; (* Is wrapping enabled (in either direction) *)
  is_wrap_reverse : bool; (* Is the wrap direction inverted *)
  (* Size constraints *)
  min_size : float option size; (* The item's min_size style *)
  max_size : float option size; (* The item's max_size style *)
  (* Box model *)
  border : float rect; (* The border of this section *)
  content_box_inset : float rect;
      (* The space between the content box and the border box *)
  scrollbar_gutter : float point;
  (* The size reserved for scrollbar gutters in each axis *)
  (* Layout properties *)
  mutable gap : float size; (* The gap of this section *)
  align_items : align_items; (* The align_items property of this node *)
  align_content : align_content; (* The align_content property of this node *)
  justify_content : justify_content option;
  (* The justify_content property of this node *)
  (* Computed sizes *)
  mutable node_outer_size : float option size;
      (* The border-box size of the node being laid out (if known) *)
  mutable node_inner_size : float option size;
      (* The content-box size of the node being laid out (if known) *)
  mutable container_size : float size;
      (* The size of the virtual container containing the flex items *)
  mutable inner_container_size : float size;
      (* The size of the internal container *)
}

(* Helper functions *)
let is_scroll_container (item : flex_item) : bool =
  Overflow.is_container item.overflow.x || Overflow.is_container item.overflow.y

(* Compute algorithm constants from style *)
let compute_constants (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (style : Style.t) (known_dimensions : float option size)
    (parent_size : float option size) : algo_constants =
  let dir = Style.flex_direction style in
  let is_row = Flex_direction_ext.is_row dir in
  let is_wrap =
    match Style.flex_wrap style with
    | Wrap | Wrap_reverse -> true
    | No_wrap -> false
  in
  let is_wrap_reverse = Style.flex_wrap style = Wrap_reverse in
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

  let padding_border_sum = Rect.sum_axes (Rect.add padding border) in
  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_sum
    else Size.zero
  in

  (* Handle overflow and scrollbar gutters *)
  let overflow = Style.overflow style in
  let scrollbar_gutter =
    Point.
      {
        x =
          (if overflow.y = Overflow.Scroll then Style.scrollbar_width style
           else 0.0);
        y =
          (if overflow.x = Overflow.Scroll then Style.scrollbar_width style
           else 0.0);
      }
  in

  (* Content box inset = padding + border + scrollbar gutter *)
  let content_box_inset =
    Rect.add (Rect.add padding border)
      Rect.
        {
          top = 0.0;
          left = 0.0;
          right = scrollbar_gutter.x;
          bottom = scrollbar_gutter.y;
        }
  in

  let node_outer_size = known_dimensions in
  let node_inner_size =
    Size.sub_option node_outer_size
      (Size.map Option.some (Rect.sum_axes content_box_inset))
  in

  (* Resolve gap *)
  let gap_base =
    match node_inner_size with
    | { width = Some w; height = Some h } -> Size.{ width = w; height = h }
    | _ -> Size.zero
  in
  let gap =
    Style.gap style |> fun gap_size ->
    Size.
      {
        width =
          Length_percentage.resolve_or_zero (Size.get Inline gap_size)
            (Some gap_base.width) calc;
        height =
          Length_percentage.resolve_or_zero (Size.get Block gap_size)
            (Some gap_base.height) calc;
      }
  in

  (* Resolve min/max sizes *)
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

  {
    dir;
    is_row;
    is_wrap;
    is_wrap_reverse;
    min_size;
    max_size;
    border;
    content_box_inset;
    scrollbar_gutter;
    gap;
    align_items =
      Option.value (Style.align_items style) ~default:Align_items.Stretch;
    align_content =
      Option.value (Style.align_content style) ~default:Align_content.Stretch;
    justify_content = Style.justify_content style;
    node_outer_size;
    node_inner_size;
    container_size = Size.zero;
    inner_container_size = Size.zero;
  }

(* Generate anonymous flex items *)
let generate_anonymous_flex_items (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (constants : algo_constants) : flex_item array =
  let calc = Tree.resolve_calc_value tree in
  let keep_child style =
    Style.position style <> Position.Absolute
    && Style.box_generation_mode style <> Box_generation_mode.None
  in
  let child_count = Tree.child_count tree node in
  (* First pass: count kept children *)
  let kept = ref 0 in
  for i = 0 to child_count - 1 do
    let child = Tree.get_child_id tree node i in
    let style = Tree.get_core_container_style tree child in
    if keep_child style then incr kept
  done;
  let items = if !kept = 0 then [||] else Array.make !kept (Obj.magic 0) in
  let insert_at = ref 0 in
  for index = 0 to child_count - 1 do
    let child_node = Tree.get_child_id tree node index in
    let child_style = Tree.get_core_container_style tree child_node in
    if keep_child child_style then (
      let aspect_ratio = Style.aspect_ratio child_style in
      let padding =
        Style.padding child_style
        |> Length_percentage.resolve_rect_or_zero
             constants.node_inner_size.width calc
      in
      let border =
        Style.border child_style
        |> Length_percentage.resolve_rect_or_zero
             constants.node_inner_size.width calc
      in
      let pb_sum = Rect.sum_axes (Rect.add padding border) in
      let box_sizing_adjustment =
        if Style.box_sizing child_style = Box_sizing.Content_box then pb_sum
        else Size.zero
      in

      let size =
        Style.size child_style |> fun dims ->
        Size.
          {
            width =
              Dimension.maybe_resolve (Size.get Inline dims)
                constants.node_inner_size.width calc;
            height =
              Dimension.maybe_resolve (Size.get Block dims)
                constants.node_inner_size.height calc;
          }
        |> Size.apply_aspect_ratio aspect_ratio
        |> Size.maybe_add box_sizing_adjustment
      in

      let min_size =
        Style.min_size child_style |> fun dims ->
        Size.
          {
            width =
              Dimension.maybe_resolve (Size.get Inline dims)
                constants.node_inner_size.width calc;
            height =
              Dimension.maybe_resolve (Size.get Block dims)
                constants.node_inner_size.height calc;
          }
        |> Size.apply_aspect_ratio aspect_ratio
        |> Size.maybe_add box_sizing_adjustment
      in

      let max_size =
        Style.max_size child_style |> fun dims ->
        Size.
          {
            width =
              Dimension.maybe_resolve (Size.get Inline dims)
                constants.node_inner_size.width calc;
            height =
              Dimension.maybe_resolve (Size.get Block dims)
                constants.node_inner_size.height calc;
          }
        |> Size.apply_aspect_ratio aspect_ratio
        |> Size.maybe_add box_sizing_adjustment
      in

      let inset =
        Style.inset child_style
        |> Rect.zip_size constants.node_inner_size (fun p s ->
            Length_percentage_auto.maybe_resolve p s calc)
      in

      let margin =
        Style.margin child_style
        |> Length_percentage_auto.resolve_rect_or_zero
             constants.node_inner_size.width calc
      in

      let margin_is_auto =
        Style.margin child_style |> Rect.map Length_percentage_auto.is_auto
      in

      let align_self =
        match Style.align_self child_style with
        | Some align -> align
        | None -> constants.align_items
      in

      let item =
        {
          node = child_node;
          order = index;
          size;
          min_size;
          max_size;
          align_self;
          overflow = Style.overflow child_style;
          scrollbar_width = Style.scrollbar_width child_style;
          flex_grow = Style.flex_grow child_style;
          flex_shrink = Style.flex_shrink child_style;
          flex_basis = 0.0;
          inner_flex_basis = 0.0;
          violation = 0.0;
          frozen = false;
          resolved_minimum_main_size = 0.0;
          inset;
          margin;
          margin_is_auto;
          padding;
          border;
          hypothetical_inner_width = 0.0;
          hypothetical_inner_height = 0.0;
          hypothetical_outer_width = 0.0;
          hypothetical_outer_height = 0.0;
          target_width = 0.0;
          target_height = 0.0;
          outer_target_width = 0.0;
          outer_target_height = 0.0;
          content_flex_fraction = 0.0;
          baseline = 0.0;
          offset_main = 0.0;
          offset_cross = 0.0;
        }
      in
      items.(!insert_at) <- item;
      incr insert_at)
  done;
  items

(* Determine available space *)
let determine_available_space (known_dimensions : float option size)
    (available_space : Available_space.t size) (constants : algo_constants) :
    Available_space.t size =
  (* Note: min/max/preferred size styles have already been applied to
     known_dimensions in the compute function above *)
  let width =
    match known_dimensions.width with
    | Some node_width ->
        Available_space.of_float
          (node_width -. Rect.horizontal_axis_sum constants.content_box_inset)
    | None ->
        Available_space.sub available_space.width
          (Rect.horizontal_axis_sum constants.content_box_inset)
  in

  let height =
    match known_dimensions.height with
    | Some node_height ->
        Available_space.of_float
          (node_height -. Rect.vertical_axis_sum constants.content_box_inset)
    | None ->
        Available_space.sub available_space.height
          (Rect.vertical_axis_sum constants.content_box_inset)
  in

  Size.{ width; height }

(* Determine flex base size *)
(* The used cross size of an item is its style size clamped by the item's own
   min/max constraints. A known cross size taken from the unclamped style size
   would measure content (e.g. wrapping text) at a width the final layout never
   gives the item, so content-based main sizes would disagree with the painted
   layout. *)
let clamp_known_cross_size (item : flex_item) ~(is_row : bool)
    (known : float option size) : float option size =
  let clamp value lo hi =
    Option.map
      (fun v ->
        let v = match hi with Some hi -> Float.min v hi | None -> v in
        match lo with Some lo -> Float.max v lo | None -> v)
      value
  in
  if is_row then
    {
      known with
      height = clamp known.height item.min_size.height item.max_size.height;
    }
  else
    {
      known with
      width = clamp known.width item.min_size.width item.max_size.width;
    }

let determine_flex_base_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (constants : algo_constants) (available_space : Available_space.t size)
    (flex_items : flex_item array) : unit =
  let calc = Tree.resolve_calc_value tree in

  Array.iter
    (fun child ->
      let child_style = Tree.get_core_container_style tree child.node in

      (* Parent size for child sizing *)
      let cross_axis_parent_size =
        if constants.is_row then constants.node_inner_size.height
        else constants.node_inner_size.width
      in
      let child_parent_size =
        if constants.is_row then
          Size.{ width = None; height = cross_axis_parent_size }
        else Size.{ width = cross_axis_parent_size; height = None }
      in

      (* Available space for child sizing *)
      let cross_axis_margin_sum =
        if constants.is_row then child.margin.top +. child.margin.bottom
        else child.margin.left +. child.margin.right
      in

      let child_min_cross =
        (if constants.is_row then child.min_size.height
         else child.min_size.width)
        |> Option.map (fun v -> v +. cross_axis_margin_sum)
      in
      let child_max_cross =
        (if constants.is_row then child.max_size.height
         else child.max_size.width)
        |> Option.map (fun v -> v +. cross_axis_margin_sum)
      in

      (* Clamp available space by min- and max- size *)
      let cross_axis_available_space =
        let available_cross =
          if constants.is_row then available_space.height
          else available_space.width
        in
        match available_cross with
        | Available_space.Definite val_ ->
            let clamped =
              Option.value cross_axis_parent_size ~default:val_ |> fun v ->
              match (child_min_cross, child_max_cross) with
              | Some min, Some max -> Float.max min (Float.min v max)
              | Some min, None -> Float.max min v
              | None, Some max -> Float.min v max
              | None, None -> v
            in
            Available_space.of_float clamped
        | Available_space.Min_content -> (
            match child_min_cross with
            | Some min -> Available_space.of_float min
            | None -> Available_space.min_content)
        | Available_space.Max_content -> (
            match child_max_cross with
            | Some max -> Available_space.of_float max
            | None -> Available_space.max_content)
      in

      (* Known dimensions for child sizing *)
      let child_known_dimensions =
        let base_size =
          if constants.is_row then
            { Size.width = None; height = child.size.height }
          else { Size.width = child.size.width; height = None }
        in
        (* Handle stretch alignment *)
        let cross_dim =
          if constants.is_row then base_size.height else base_size.width
        in
        (if child.align_self = Stretch && cross_dim = None then
           let cross_size =
             Available_space.to_option cross_axis_available_space
             |> Option.map (fun v -> v -. cross_axis_margin_sum)
           in
           if constants.is_row then { base_size with height = cross_size }
           else { base_size with width = cross_size }
         else base_size)
        |> clamp_known_cross_size child ~is_row:constants.is_row
      in

      (* Get container width for resolving flex basis *)
      let container_width =
        if constants.is_row then constants.node_inner_size.width
        else constants.node_inner_size.height
      in

      (* Compute box sizing adjustment *)
      let box_sizing_adjustment =
        if Style.box_sizing child_style = Box_sizing.Content_box then
          let padding_main =
            if constants.is_row then child.padding.left +. child.padding.right
            else child.padding.top +. child.padding.bottom
          in
          let border_main =
            if constants.is_row then child.border.left +. child.border.right
            else child.border.top +. child.border.bottom
          in
          padding_main +. border_main
        else 0.0
      in

      (* Resolve flex basis *)
      let flex_basis_style =
        Style.flex_basis child_style |> fun fb ->
        Dimension.maybe_resolve fb container_width calc
        |> Option.map (fun v -> v +. box_sizing_adjustment)
      in

      (* Compute flex basis according to spec *)
      child.flex_basis <-
        (* A. If the item has a definite used flex basis, that's the flex base
           size. *)
        (let main_size =
           if constants.is_row then child.size.width else child.size.height
         in
         match (flex_basis_style, main_size) with
         | Some fb, _ -> fb
         | None, Some ms -> ms
         | None, None ->
             (* E. Otherwise, size the item into the available space *)
             let child_available_space =
               let main_available =
                 match
                   if constants.is_row then available_space.width
                   else available_space.height
                 with
                 | Available_space.Min_content -> Available_space.min_content
                 | _ -> Available_space.max_content
               in
               if constants.is_row then
                 Size.
                   {
                     width = main_available;
                     height = cross_axis_available_space;
                   }
               else
                 Size.
                   {
                     width = cross_axis_available_space;
                     height = main_available;
                   }
             in

             let layout_output =
               Tree.compute_child_layout tree child.node
                 (Layout_input.make ~run_mode:Run_mode.Compute_size
                    ~sizing_mode:Sizing_mode.Content_size
                    ~axis:
                      (if constants.is_row then Requested_axis.Horizontal
                       else Requested_axis.Vertical)
                    ~known_dimensions:child_known_dimensions
                    ~parent_size:child_parent_size
                    ~available_space:child_available_space
                    ~vertical_margins_are_collapsible:Line.both_false)
             in

             let measured_size = Layout_output.size layout_output in
             if constants.is_row then measured_size.width
             else measured_size.height);

      (* Floor flex-basis by the padding_border_sum *)
      let padding_border_sum =
        if constants.is_row then
          child.padding.left +. child.padding.right +. child.border.left
          +. child.border.right
        else
          child.padding.top +. child.padding.bottom +. child.border.top
          +. child.border.bottom
      in
      child.flex_basis <- Float.max child.flex_basis padding_border_sum;

      (* Compute inner flex basis *)
      child.inner_flex_basis <- child.flex_basis -. padding_border_sum;

      (* Compute resolved minimum main size *)
      let style_min_main_size =
        let min_from_style =
          if constants.is_row then child.min_size.width
          else child.min_size.height
        in
        let auto_min =
          let overflow_main =
            if constants.is_row then child.overflow.x else child.overflow.y
          in
          match Overflow.to_automatic_min_size overflow_main with
          | dim when Dimension.is_auto dim -> None
          | dim -> Dimension.to_option dim
        in
        match (min_from_style, auto_min) with
        | Some v, _ -> Some v
        | None, Some v -> Some v
        | None, None -> None
      in

      child.resolved_minimum_main_size <-
        (match style_min_main_size with
        | Some v -> v
        | None ->
            (* Compute min-content size *)
            let child_available_space =
              let cross_available =
                if constants.is_row then
                  Size.
                    {
                      width = Available_space.min_content;
                      height = cross_axis_available_space;
                    }
                else
                  Size.
                    {
                      width = cross_axis_available_space;
                      height = Available_space.min_content;
                    }
              in
              cross_available
            in

            let layout_output =
              Tree.compute_child_layout tree child.node
                (Layout_input.make ~run_mode:Run_mode.Compute_size
                   ~sizing_mode:Sizing_mode.Content_size
                   ~axis:
                     (if constants.is_row then Requested_axis.Horizontal
                      else Requested_axis.Vertical)
                   ~known_dimensions:child_known_dimensions
                   ~parent_size:child_parent_size
                   ~available_space:child_available_space
                   ~vertical_margins_are_collapsible:Line.both_false)
            in

            let min_content_main_size =
              let measured_size = Layout_output.size layout_output in
              if constants.is_row then measured_size.width
              else measured_size.height
            in

            (* Apply automatic minimum size clamping *)
            let clamped_min =
              min_content_main_size |> fun v ->
              (match
                 if constants.is_row then child.size.width
                 else child.size.height
               with
                | Some size -> Float.min v size
                | None -> v)
              |> fun v ->
              match
                if constants.is_row then child.max_size.width
                else child.max_size.height
              with
              | Some max -> Float.min v max
              | None -> v
            in
            Float.max clamped_min padding_border_sum);

      (* Compute hypothetical sizes *)
      let hypothetical_inner_min_main =
        Float.max child.resolved_minimum_main_size padding_border_sum
      in
      let hypothetical_inner_width =
        let max_main =
          if constants.is_row then child.max_size.width
          else child.max_size.height
        in
        match max_main with
        | Some max ->
            Float.min child.flex_basis
              (Float.max hypothetical_inner_min_main max)
        | None -> Float.max child.flex_basis hypothetical_inner_min_main
      in
      let hypothetical_outer_width =
        hypothetical_inner_width
        +.
        if constants.is_row then child.margin.left +. child.margin.right
        else child.margin.top +. child.margin.bottom
      in

      if constants.is_row then (
        child.hypothetical_inner_width <- hypothetical_inner_width;
        child.hypothetical_outer_width <- hypothetical_outer_width)
      else (
        child.hypothetical_inner_height <- hypothetical_inner_width;
        child.hypothetical_outer_height <- hypothetical_outer_width))
    flex_items

(* Collect flex items into flex lines *)
let collect_flex_lines (constants : algo_constants)
    (available_space : Available_space.t size) (flex_items : flex_item array) :
    flex_line list =
  if not constants.is_wrap then
    (* Single line - all items go in one line *)
    [
      {
        items = Array.init (Array.length flex_items) (Array.get flex_items);
        cross_size = 0.0;
        offset_cross = 0.0;
      };
    ]
  else
    (* Multi-line wrapping *)
    let main_axis_available_space =
      let max_main =
        if constants.is_row then constants.max_size.width
        else constants.max_size.height
      in
      match max_main with
      | Some max_size ->
          let available_main =
            if constants.is_row then available_space.width
            else available_space.height
          in
          let resolved =
            Available_space.to_option available_main
            |> Option.value ~default:max_size
            |> fun v ->
            let min_main =
              if constants.is_row then constants.min_size.width
              else constants.min_size.height
            in
            match min_main with Some min -> Float.max v min | None -> v
          in
          Available_space.of_float resolved
      | None ->
          if constants.is_row then available_space.width
          else available_space.height
    in

    match main_axis_available_space with
    | Available_space.Max_content ->
        (* Max content - items never wrap *)
        [
          {
            items = Array.init (Array.length flex_items) (Array.get flex_items);
            cross_size = 0.0;
            offset_cross = 0.0;
          };
        ]
    | Available_space.Min_content ->
        (* Min content - each item gets its own line *)
        let lines = ref [] in
        for i = Array.length flex_items - 1 downto 0 do
          let item = flex_items.(i) in
          lines :=
            { items = [| item |]; cross_size = 0.0; offset_cross = 0.0 }
            :: !lines
        done;
        !lines
    | Available_space.Definite main_axis_available_space ->
        (* Definite size - wrap based on available space *)
        let main_axis_gap =
          if constants.is_row then constants.gap.width else constants.gap.height
        in

        (* Process flex items into lines *)
        let rec collect_lines start_index lines =
          if start_index >= Array.length flex_items then List.rev lines
          else
            let line_length = ref 0.0 in
            let rec find_split idx =
              if idx >= Array.length flex_items then idx
              else
                let item = flex_items.(idx) in
                let gap_contribution =
                  if idx = start_index then 0.0 else main_axis_gap
                in
                let item_size =
                  if constants.is_row then item.hypothetical_outer_width
                  else item.hypothetical_outer_height
                in
                let new_length =
                  !line_length +. item_size +. gap_contribution
                in

                if new_length > main_axis_available_space && idx <> start_index
                then idx
                else (
                  line_length := new_length;
                  find_split (idx + 1))
            in

            let split_idx = find_split start_index in
            let split_idx =
              if split_idx = start_index then start_index + 1 else split_idx
            in
            let line_items_len = split_idx - start_index in
            let line_items = Array.sub flex_items start_index line_items_len in

            let line =
              { items = line_items; cross_size = 0.0; offset_cross = 0.0 }
            in

            collect_lines split_idx (line :: lines)
        in

        collect_lines 0 []

(* Helper to sum axis gaps *)
let sum_axis_gaps gap num_items =
  (* Gaps only exist between items *)
  if num_items <= 1 then 0.0 else gap *. float_of_int (num_items - 1)

(* Determine container main size *)
let determine_container_main_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (available_space : Available_space.t size) (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  let main_content_box_inset =
    if constants.is_row then
      Rect.horizontal_axis_sum constants.content_box_inset
    else Rect.vertical_axis_sum constants.content_box_inset
  in

  let outer_main_size =
    let current_main_size =
      if constants.is_row then constants.node_outer_size.width
      else constants.node_outer_size.height
    in
    match current_main_size with
    | Some size -> size
    | None -> (
        let available_main =
          if constants.is_row then available_space.width
          else available_space.height
        in
        match available_main with
        | Available_space.Definite main_axis_available_space ->
            (* Calculate longest line length *)
            let longest_line_length =
              List.fold_left
                (fun max_length line ->
                  let line_main_axis_gap =
                    sum_axis_gaps
                      (if constants.is_row then constants.gap.width
                       else constants.gap.height)
                      (Array.length line.items)
                  in
                  let total_target_size =
                    Array.fold_left
                      (fun sum child ->
                        let padding_border_sum =
                          if constants.is_row then
                            child.padding.left +. child.padding.right
                            +. child.border.left +. child.border.right
                          else
                            child.padding.top +. child.padding.bottom
                            +. child.border.top +. child.border.bottom
                        in
                        let min_main =
                          if constants.is_row then child.min_size.width
                          else child.min_size.height
                        in
                        let base_size =
                          match min_main with
                          | Some min -> Float.max child.flex_basis min
                          | None -> child.flex_basis
                        in
                        let margin_sum =
                          if constants.is_row then
                            child.margin.left +. child.margin.right
                          else child.margin.top +. child.margin.bottom
                        in
                        sum
                        +. Float.max (base_size +. margin_sum)
                             padding_border_sum)
                      0.0 line.items
                  in
                  Float.max max_length (total_target_size +. line_main_axis_gap))
                0.0 flex_lines
            in
            let size = longest_line_length +. main_content_box_inset in
            if List.length flex_lines > 1 then
              Float.max size main_axis_available_space
            else size
        | Available_space.Min_content when constants.is_wrap ->
            (* Min content with wrapping *)
            let longest_line_length =
              List.fold_left
                (fun max_length line ->
                  let line_main_axis_gap =
                    sum_axis_gaps
                      (if constants.is_row then constants.gap.width
                       else constants.gap.height)
                      (Array.length line.items)
                  in
                  let total_target_size =
                    Array.fold_left
                      (fun sum child ->
                        let padding_border_sum =
                          if constants.is_row then
                            child.padding.left +. child.padding.right
                            +. child.border.left +. child.border.right
                          else
                            child.padding.top +. child.padding.bottom
                            +. child.border.top +. child.border.bottom
                        in
                        let min_main =
                          if constants.is_row then child.min_size.width
                          else child.min_size.height
                        in
                        let base_size =
                          match min_main with
                          | Some min -> Float.max child.flex_basis min
                          | None -> child.flex_basis
                        in
                        let margin_sum =
                          if constants.is_row then
                            child.margin.left +. child.margin.right
                          else child.margin.top +. child.margin.bottom
                        in
                        sum
                        +. Float.max (base_size +. margin_sum)
                             padding_border_sum)
                      0.0 line.items
                  in
                  Float.max max_length (total_target_size +. line_main_axis_gap))
                0.0 flex_lines
            in
            longest_line_length +. main_content_box_inset
        | Available_space.Min_content | Available_space.Max_content ->
            (* Content-based sizing *)
            let main_size = ref 0.0 in

            List.iter
              (fun line ->
                (* Process each item in the line *)
                Array.iter
                  (fun (flex_item : flex_item) ->
                    let style_min =
                      if constants.is_row then flex_item.min_size.width
                      else flex_item.min_size.height
                    in
                    let style_preferred =
                      if constants.is_row then flex_item.size.width
                      else flex_item.size.height
                    in
                    let style_max =
                      if constants.is_row then flex_item.max_size.width
                      else flex_item.max_size.height
                    in

                    (* The clamping basis: for a row container, the flex base
                       size or the preferred size, whichever is larger, as
                       Chrome and Firefox do; for a column container, the flex
                       base size alone, as the spec says, so that an item's
                       height does not grow the container's max-content size
                       past the item's flex basis. *)
                    let clamping_basis =
                      match style_preferred with
                      | Some pref when constants.is_row ->
                          Some (Float.max flex_item.flex_basis pref)
                      | _ -> Some flex_item.flex_basis
                    in

                    let flex_basis_min =
                      if flex_item.flex_shrink = 0.0 then clamping_basis
                      else None
                    in
                    let flex_basis_max =
                      if flex_item.flex_grow = 0.0 then clamping_basis else None
                    in

                    let min_main_size =
                      let base_min =
                        match (style_min, flex_basis_min) with
                        | Some s, Some f -> Some (Float.max s f)
                        | Some s, None -> Some s
                        | None, Some f -> Some f
                        | None, None -> None
                      in
                      match base_min with
                      | Some v ->
                          Float.max v flex_item.resolved_minimum_main_size
                      | None -> flex_item.resolved_minimum_main_size
                    in

                    let max_main_size =
                      match (style_max, flex_basis_max) with
                      | Some s, Some f -> Float.min s f
                      | Some s, None -> s
                      | None, Some f -> f
                      | None, None -> Float.infinity
                    in

                    let margin_sum =
                      if constants.is_row then
                        flex_item.margin.left +. flex_item.margin.right
                      else flex_item.margin.top +. flex_item.margin.bottom
                    in

                    let content_contribution =
                      match (style_min, style_preferred, max_main_size) with
                      (* If clamping values are such that max <= min, avoid
                         computing content size *)
                      | _, Some pref, max
                        when max <= min_main_size || max <= pref ->
                          let clamped =
                            Float.min pref max |> Float.max min_main_size
                          in
                          clamped +. margin_sum
                      | _, _, max when max <= min_main_size ->
                          min_main_size +. margin_sum
                      | _ when is_scroll_container flex_item ->
                          (* Scroll containers use their flex basis but still
                             honour min/max and padding/border insets *)
                          let content_main_size =
                            flex_item.flex_basis +. margin_sum
                          in
                          if constants.is_row then
                            let clamped =
                              match (style_min, style_max) with
                              | Some min, Some max ->
                                  Float.max min
                                    (Float.min content_main_size max)
                              | Some min, None ->
                                  Float.max min content_main_size
                              | None, Some max ->
                                  Float.min content_main_size max
                              | None, None -> content_main_size
                            in
                            Float.max clamped main_content_box_inset
                          else
                            let with_flex_basis =
                              Float.max content_main_size flex_item.flex_basis
                            in
                            let clamped =
                              match (style_min, style_max) with
                              | Some min, Some max ->
                                  Float.max min (Float.min with_flex_basis max)
                              | Some min, None -> Float.max min with_flex_basis
                              | None, Some max -> Float.min with_flex_basis max
                              | None, None -> with_flex_basis
                            in
                            Float.max clamped main_content_box_inset
                      | _, Some pref, _ ->
                          (* A definite preferred main size is the item's
                             content contribution (an inherent-size measure
                             would return it), floored by its main-axis
                             padding and border, so the item is not measured;
                             min and max clamp it as they clamp a measured
                             contribution. *)
                          let item_padding_border =
                            if constants.is_row then
                              flex_item.padding.left +. flex_item.padding.right
                              +. flex_item.border.left +. flex_item.border.right
                            else
                              flex_item.padding.top +. flex_item.padding.bottom
                              +. flex_item.border.top +. flex_item.border.bottom
                          in
                          let content_main_size =
                            Float.max pref item_padding_border +. margin_sum
                          in
                          let clamp size =
                            match (style_min, style_max) with
                            | Some min, Some max ->
                                Float.max min (Float.min size max)
                            | Some min, None -> Float.max min size
                            | None, Some max -> Float.min size max
                            | None, None -> size
                          in
                          if constants.is_row then clamp content_main_size
                          else
                            clamp
                              (Float.max content_main_size flex_item.flex_basis)
                      | _ ->
                          (* Need to compute content size *)
                          let cross_axis_parent_size =
                            if constants.is_row then
                              constants.node_inner_size.height
                            else constants.node_inner_size.width
                          in

                          let cross_axis_margin_sum =
                            if constants.is_row then
                              flex_item.margin.top +. flex_item.margin.bottom
                            else flex_item.margin.left +. flex_item.margin.right
                          in

                          let child_min_cross =
                            (if constants.is_row then flex_item.min_size.height
                             else flex_item.min_size.width)
                            |> Option.map (fun v -> v +. cross_axis_margin_sum)
                          in
                          let child_max_cross =
                            (if constants.is_row then flex_item.max_size.height
                             else flex_item.max_size.width)
                            |> Option.map (fun v -> v +. cross_axis_margin_sum)
                          in

                          let cross_axis_available_space =
                            let available_cross =
                              if constants.is_row then available_space.height
                              else available_space.width
                            in
                            match available_cross with
                            | Available_space.Definite val_ ->
                                let clamped =
                                  Option.value cross_axis_parent_size
                                    ~default:val_
                                  |> fun v ->
                                  match (child_min_cross, child_max_cross) with
                                  | Some min, Some max ->
                                      Float.max min (Float.min v max)
                                  | Some min, None -> Float.max min v
                                  | None, Some max -> Float.min v max
                                  | None, None -> v
                                in
                                Available_space.of_float clamped
                            | _ -> available_cross
                          in

                          let child_available_space =
                            if constants.is_row then
                              Size.
                                {
                                  width = available_space.width;
                                  height = cross_axis_available_space;
                                }
                            else
                              Size.
                                {
                                  width = cross_axis_available_space;
                                  height = available_space.height;
                                }
                          in

                          (* Known dimensions for child sizing *)
                          let child_known_dimensions =
                            let base_size =
                              if constants.is_row then
                                {
                                  Size.width = None;
                                  height = flex_item.size.height;
                                }
                              else
                                {
                                  Size.width = flex_item.size.width;
                                  height = None;
                                }
                            in
                            let cross_dim =
                              if constants.is_row then base_size.height
                              else base_size.width
                            in
                            (if
                               flex_item.align_self = Stretch
                               && cross_dim = None
                             then
                               let cross_size =
                                 Available_space.to_option
                                   cross_axis_available_space
                                 |> Option.map (fun v ->
                                     v
                                     -.
                                     if constants.is_row then
                                       flex_item.margin.top
                                       +. flex_item.margin.bottom
                                     else
                                       flex_item.margin.left
                                       +. flex_item.margin.right)
                               in
                               if constants.is_row then
                                 { base_size with height = cross_size }
                               else { base_size with width = cross_size }
                             else base_size)
                            |> clamp_known_cross_size flex_item
                                 ~is_row:constants.is_row
                          in

                          let layout_output =
                            Tree.compute_child_layout tree flex_item.node
                              (Layout_input.make ~run_mode:Run_mode.Compute_size
                                 ~sizing_mode:Sizing_mode.Content_size
                                 ~axis:
                                   (if constants.is_row then
                                      Requested_axis.Horizontal
                                    else Requested_axis.Vertical)
                                 ~known_dimensions:child_known_dimensions
                                 ~parent_size:constants.node_inner_size
                                 ~available_space:child_available_space
                                 ~vertical_margins_are_collapsible:
                                   Line.both_false)
                          in

                          let content_main_size =
                            let measured_size =
                              Layout_output.size layout_output
                            in
                            let measured_main_size =
                              if constants.is_row then measured_size.width
                              else measured_size.height
                            in
                            (* A known cross size, transferred through the
                               item's aspect ratio, floors the measured
                               content size. *)
                            let known_cross =
                              if constants.is_row then
                                child_known_dimensions.height
                              else child_known_dimensions.width
                            in
                            let aspect_ratio =
                              Style.aspect_ratio
                                (Tree.get_core_container_style tree
                                   flex_item.node)
                            in
                            (match (aspect_ratio, known_cross) with
                              | Some ratio, Some cross ->
                                  Float.max measured_main_size
                                    (if constants.is_row then cross *. ratio
                                     else cross /. ratio)
                              | _ -> measured_main_size)
                            +. margin_sum
                          in

                          (* Apply different clamping based on row vs column *)
                          if constants.is_row then
                            let clamped =
                              match (style_min, style_max) with
                              | Some min, Some max ->
                                  Float.max min
                                    (Float.min content_main_size max)
                              | Some min, None ->
                                  Float.max min content_main_size
                              | None, Some max ->
                                  Float.min content_main_size max
                              | None, None -> content_main_size
                            in
                            Float.max clamped main_content_box_inset
                          else
                            let with_flex_basis =
                              Float.max content_main_size flex_item.flex_basis
                            in
                            let clamped =
                              match (style_min, style_max) with
                              | Some min, Some max ->
                                  Float.max min (Float.min with_flex_basis max)
                              | Some min, None -> Float.max min with_flex_basis
                              | None, Some max -> Float.min with_flex_basis max
                              | None, None -> with_flex_basis
                            in
                            Float.max clamped main_content_box_inset
                    in

                    (* Compute content flex fraction *)
                    flex_item.content_flex_fraction <-
                      (let diff =
                         content_contribution -. flex_item.flex_basis
                       in
                       if diff > 0.0 then
                         diff /. Float.max 1.0 flex_item.flex_grow
                       else if diff < 0.0 then
                         let scaled_shrink_factor =
                           Float.max 1.0
                             (flex_item.flex_shrink
                            *. flex_item.inner_flex_basis)
                         in
                         diff /. scaled_shrink_factor
                       else 0.0))
                  line.items;

                (* Calculate line contribution *)
                let item_main_size_sum =
                  Array.fold_left
                    (fun sum item ->
                      let flex_fraction = item.content_flex_fraction in
                      let flex_contribution =
                        if item.content_flex_fraction > 0.0 then
                          Float.max 1.0 item.flex_grow *. flex_fraction
                        else if item.content_flex_fraction < 0.0 then
                          let scaled_shrink_factor =
                            Float.max 1.0 item.flex_shrink
                            *. item.inner_flex_basis
                          in
                          scaled_shrink_factor *. flex_fraction
                        else 0.0
                      in
                      let size = item.flex_basis +. flex_contribution in
                      if constants.is_row then (
                        item.outer_target_width <- size;
                        item.target_width <- size)
                      else (
                        item.outer_target_height <- size;
                        item.target_height <- size);
                      sum +. size)
                    0.0 line.items
                in

                let gap_sum =
                  sum_axis_gaps
                    (if constants.is_row then constants.gap.width
                     else constants.gap.height)
                    (Array.length line.items)
                in
                main_size := Float.max !main_size (item_main_size_sum +. gap_sum))
              flex_lines;

            !main_size +. main_content_box_inset)
  in

  (* Apply size constraints *)
  let min_main =
    if constants.is_row then constants.min_size.width
    else constants.min_size.height
  in
  let max_main =
    if constants.is_row then constants.max_size.width
    else constants.max_size.height
  in
  let scrollbar_main =
    if constants.is_row then constants.scrollbar_gutter.x
    else constants.scrollbar_gutter.y
  in

  let outer_main_size =
    let clamped =
      match (min_main, max_main) with
      | Some min, Some max -> Float.max min (Float.min outer_main_size max)
      | Some min, None -> Float.max min outer_main_size
      | None, Some max -> Float.min outer_main_size max
      | None, None -> outer_main_size
    in
    Float.max clamped (main_content_box_inset -. scrollbar_main)
  in

  let inner_main_size =
    Float.max (outer_main_size -. main_content_box_inset) 0.0
  in

  (* Update constants *)
  if constants.is_row then (
    constants.container_size <-
      { constants.container_size with width = outer_main_size };
    constants.inner_container_size <-
      { constants.inner_container_size with width = inner_main_size };
    constants.node_inner_size <-
      { constants.node_inner_size with width = Some inner_main_size })
  else (
    constants.container_size <-
      { constants.container_size with height = outer_main_size };
    constants.inner_container_size <-
      { constants.inner_container_size with height = inner_main_size };
    constants.node_inner_size <-
      { constants.node_inner_size with height = Some inner_main_size })

(* Resolve flexible lengths *)
let resolve_flexible_lengths (line : flex_line) (constants : algo_constants) :
    unit =
  let total_main_axis_gap =
    sum_axis_gaps
      (if constants.is_row then constants.gap.width else constants.gap.height)
      (Array.length line.items)
  in

  (* 1. Determine the used flex factor *)
  let total_hypothetical_outer_main_size =
    Array.fold_left
      (fun sum child ->
        sum
        +.
        if constants.is_row then child.hypothetical_outer_width
        else child.hypothetical_outer_height)
      0.0 line.items
  in
  let used_flex_factor =
    total_main_axis_gap +. total_hypothetical_outer_main_size
  in
  let inner_main_size =
    if constants.is_row then
      Option.value constants.node_inner_size.width ~default:0.0
    else Option.value constants.node_inner_size.height ~default:0.0
  in
  let growing = used_flex_factor < inner_main_size in
  let shrinking = used_flex_factor > inner_main_size in
  let exactly_sized = (not growing) && not shrinking in

  (* 2. Size inflexible items *)
  Array.iter
    (fun child ->
      let inner_target_size =
        if constants.is_row then child.hypothetical_inner_width
        else child.hypothetical_inner_height
      in
      if constants.is_row then child.target_width <- inner_target_size
      else child.target_height <- inner_target_size;

      if
        exactly_sized
        || (child.flex_grow = 0.0 && child.flex_shrink = 0.0)
        || (growing && child.flex_basis > inner_target_size)
        || (shrinking && child.flex_basis < inner_target_size)
      then (
        child.frozen <- true;
        let outer_target_size =
          inner_target_size
          +.
          if constants.is_row then child.margin.left +. child.margin.right
          else child.margin.top +. child.margin.bottom
        in
        if constants.is_row then child.outer_target_width <- outer_target_size
        else child.outer_target_height <- outer_target_size))
    line.items;

  if exactly_sized then ()
  else
    (* 3. Calculate initial free space *)
    let calc_used_space () =
      total_main_axis_gap
      +. Array.fold_left
           (fun sum child ->
             if child.frozen then
               sum
               +.
               if constants.is_row then child.outer_target_width
               else child.outer_target_height
             else
               sum +. child.flex_basis
               +.
               if constants.is_row then child.margin.left +. child.margin.right
               else child.margin.top +. child.margin.bottom)
           0.0 line.items
    in

    let initial_free_space = inner_main_size -. calc_used_space () in

    (* 4. Loop *)
    let rec flex_loop () =
      (* a. Check for flexible items *)
      if Array.for_all (fun child -> child.frozen) line.items then ()
      else
        (* b. Calculate the remaining free space *)
        let used_space = calc_used_space () in

        let sum_flex_grow, sum_flex_shrink =
          Array.fold_left
            (fun (grow, shrink) child ->
              if child.frozen then (grow, shrink)
              else (grow +. child.flex_grow, shrink +. child.flex_shrink))
            (0.0, 0.0) line.items
        in

        let free_space =
          if growing && sum_flex_grow < 1.0 then
            Float.min
              ((initial_free_space *. sum_flex_grow) -. total_main_axis_gap)
              (inner_main_size -. used_space)
          else if shrinking && sum_flex_shrink < 1.0 then
            Float.max
              ((initial_free_space *. sum_flex_shrink) -. total_main_axis_gap)
              (inner_main_size -. used_space)
          else inner_main_size -. used_space
        in

        (* c. Distribute free space proportional to the flex factors *)
        (if Float.is_finite free_space && free_space <> 0.0 then
           if growing && sum_flex_grow > 0.0 then
             Array.iter
               (fun child ->
                 if not child.frozen then
                   let new_size =
                     child.flex_basis
                     +. (free_space *. (child.flex_grow /. sum_flex_grow))
                   in
                   if constants.is_row then child.target_width <- new_size
                   else child.target_height <- new_size)
               line.items
           else if shrinking && sum_flex_shrink > 0.0 then
             let sum_scaled_shrink_factor =
               Array.fold_left
                 (fun sum child ->
                   if child.frozen then sum
                   else sum +. (child.inner_flex_basis *. child.flex_shrink))
                 0.0 line.items
             in
             if sum_scaled_shrink_factor > 0.0 then
               Array.iter
                 (fun child ->
                   if not child.frozen then
                     let scaled_shrink_factor =
                       child.inner_flex_basis *. child.flex_shrink
                     in
                     let new_size =
                       child.flex_basis
                       +. free_space
                          *. (scaled_shrink_factor /. sum_scaled_shrink_factor)
                     in
                     if constants.is_row then child.target_width <- new_size
                     else child.target_height <- new_size)
                 line.items);

        (* d. Fix min/max violations *)
        let total_violation =
          Array.fold_left
            (fun acc child ->
              if child.frozen then acc
              else
                let current_size =
                  if constants.is_row then child.target_width
                  else child.target_height
                in
                let resolved_min = Some child.resolved_minimum_main_size in
                let max_main =
                  if constants.is_row then child.max_size.width
                  else child.max_size.height
                in
                let clamped =
                  let v =
                    match (resolved_min, max_main) with
                    | Some min, Some max ->
                        Float.max min (Float.min current_size max)
                    | Some min, None -> Float.max min current_size
                    | None, Some max -> Float.min current_size max
                    | None, None -> current_size
                  in
                  Float.max 0.0 v
                in
                child.violation <- clamped -. current_size;
                if constants.is_row then (
                  child.target_width <- clamped;
                  child.outer_target_width <-
                    clamped +. child.margin.left +. child.margin.right)
                else (
                  child.target_height <- clamped;
                  child.outer_target_height <-
                    clamped +. child.margin.top +. child.margin.bottom);

                acc +. child.violation)
            0.0 line.items
        in

        (* e. Freeze over-flexed items *)
        Array.iter
          (fun child ->
            if not child.frozen then
              child.frozen <-
                (if total_violation > 0.0 then child.violation > 0.0
                 else if total_violation < 0.0 then child.violation < 0.0
                 else true))
          line.items;

        (* f. Return to the start of this loop *)
        flex_loop ()
    in

    flex_loop ()

(* Determine hypothetical cross size *)
let determine_hypothetical_cross_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (line : flex_line) (constants : algo_constants)
    (available_space : Available_space.t size) : unit =
  Array.iter
    (fun child ->
      let padding_border_sum =
        if constants.is_row then
          child.padding.top +. child.padding.bottom +. child.border.top
          +. child.border.bottom
        else
          child.padding.left +. child.padding.right +. child.border.left
          +. child.border.right
      in

      let child_known_main =
        if constants.is_row then constants.container_size.width
        else constants.container_size.height
      in

      let child_cross =
        let cross_size =
          if constants.is_row then child.size.height else child.size.width
        in
        let min_cross =
          if constants.is_row then child.min_size.height
          else child.min_size.width
        in
        let max_cross =
          if constants.is_row then child.max_size.height
          else child.max_size.width
        in
        match (cross_size, min_cross, max_cross) with
        | Some size, Some min, Some max ->
            Some
              (Float.max padding_border_sum
                 (Float.max min (Float.min size max)))
        | Some size, Some min, None ->
            Some (Float.max padding_border_sum (Float.max min size))
        | Some size, None, Some max ->
            Some (Float.max padding_border_sum (Float.min size max))
        | Some size, None, None -> Some (Float.max padding_border_sum size)
        | None, _, _ -> None
      in

      let child_available_cross =
        let available_cross =
          if constants.is_row then available_space.height
          else available_space.width
        in
        let min_cross =
          if constants.is_row then child.min_size.height
          else child.min_size.width
        in
        let max_cross =
          if constants.is_row then child.max_size.height
          else child.max_size.width
        in
        match available_cross with
        | Available_space.Definite value ->
            let clamped =
              match (min_cross, max_cross) with
              | Some min, Some max -> Float.max min (Float.min value max)
              | Some min, None -> Float.max min value
              | None, Some max -> Float.min value max
              | None, None -> value
            in
            Available_space.of_float (Float.max padding_border_sum clamped)
        | _ -> available_cross
      in

      let child_inner_cross =
        match child_cross with
        | Some cross -> cross
        | None ->
            (* Need to measure the child *)
            let child_known_dimensions =
              if constants.is_row then
                { Size.width = Some child.target_width; height = child_cross }
              else
                { Size.width = child_cross; height = Some child.target_height }
            in
            let child_available_space =
              if constants.is_row then
                {
                  Size.width = Available_space.of_float child_known_main;
                  height = child_available_cross;
                }
              else
                {
                  Size.width = child_available_cross;
                  height = Available_space.of_float child_known_main;
                }
            in
            let layout_output =
              Tree.compute_child_layout tree child.node
                (Layout_input.make ~run_mode:Run_mode.Compute_size
                   ~sizing_mode:Sizing_mode.Content_size
                   ~axis:
                     (if constants.is_row then Requested_axis.Vertical
                      else Requested_axis.Horizontal)
                   ~known_dimensions:child_known_dimensions
                   ~parent_size:constants.node_inner_size
                   ~available_space:child_available_space
                   ~vertical_margins_are_collapsible:Line.both_false)
            in
            let measured_size =
              let size = Layout_output.size layout_output in
              if constants.is_row then size.height else size.width
            in
            let min_cross =
              if constants.is_row then child.min_size.height
              else child.min_size.width
            in
            let max_cross =
              if constants.is_row then child.max_size.height
              else child.max_size.width
            in
            let clamped =
              match (min_cross, max_cross) with
              | Some min, Some max ->
                  Float.max min (Float.min measured_size max)
              | Some min, None -> Float.max min measured_size
              | None, Some max -> Float.min measured_size max
              | None, None -> measured_size
            in
            Float.max padding_border_sum clamped
      in

      let child_outer_cross =
        child_inner_cross
        +.
        if constants.is_row then child.margin.top +. child.margin.bottom
        else child.margin.left +. child.margin.right
      in

      if constants.is_row then (
        child.hypothetical_inner_height <- child_inner_cross;
        child.hypothetical_outer_height <- child_outer_cross)
      else (
        child.hypothetical_inner_width <- child_inner_cross;
        child.hypothetical_outer_width <- child_outer_cross))
    line.items

(* Calculate children baselines *)
let calculate_children_base_lines (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (known_dimensions : float option size)
    (available_space : Available_space.t size) (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  (* Only compute baselines for flex rows because we only support baseline
     alignment in the cross axis where that axis is also the inline axis *)
  if not constants.is_row then ()
  else
    List.iter
      (fun line ->
        (* If a flex line has one or zero items participating in baseline
           alignment then baseline alignment is a no-op so we skip *)
        let line_baseline_child_count =
          Array.fold_left
            (fun count child ->
              if child.align_self = Baseline then count + 1 else count)
            0 line.items
        in
        if line_baseline_child_count <= 1 then ()
        else
          Array.iter
            (fun child ->
              (* Only calculate baselines for children participating in baseline
                 alignment *)
              if child.align_self = Baseline then
                let measured_size_and_baselines =
                  Tree.compute_child_layout tree child.node
                    (Layout_input.make ~run_mode:Run_mode.Perform_layout
                       ~sizing_mode:Sizing_mode.Content_size
                       ~axis:Requested_axis.Both
                       ~known_dimensions:
                         (if constants.is_row then
                            {
                              Size.width = Some child.target_width;
                              height = Some child.hypothetical_inner_height;
                            }
                          else
                            {
                              Size.width = Some child.hypothetical_inner_width;
                              height = Some child.target_height;
                            })
                       ~parent_size:constants.node_inner_size
                       ~available_space:
                         (if constants.is_row then
                            {
                              Size.width =
                                Available_space.of_float
                                  constants.container_size.width;
                              height =
                                Available_space.set_or_self
                                  available_space.height known_dimensions.height;
                            }
                          else
                            {
                              Size.width =
                                Available_space.set_or_self
                                  available_space.width known_dimensions.width;
                              height =
                                Available_space.of_float
                                  constants.container_size.height;
                            })
                       ~vertical_margins_are_collapsible:Line.both_false)
                in

                let baseline =
                  (Layout_output.first_baselines measured_size_and_baselines).y
                in
                let height =
                  (Layout_output.size measured_size_and_baselines).height
                in

                child.baseline <-
                  Option.value baseline ~default:height +. child.margin.top)
            line.items)
      flex_lines

(* Calculate cross size *)
let calculate_cross_size (flex_lines : flex_line list)
    (known_dimensions : float option size) (constants : algo_constants) : unit =
  (* If the flex container is single-line and has a definite cross size, the
     cross size of the flex line is the flex container's inner cross size. *)
  if
    (not constants.is_wrap)
    && (if constants.is_row then known_dimensions.height
        else known_dimensions.width)
       <> None
  then
    let cross_axis_padding_border =
      if constants.is_row then
        Rect.vertical_axis_sum constants.content_box_inset
      else Rect.horizontal_axis_sum constants.content_box_inset
    in
    let cross_min_size =
      if constants.is_row then constants.min_size.height
      else constants.min_size.width
    in
    let cross_max_size =
      if constants.is_row then constants.max_size.height
      else constants.max_size.width
    in
    let node_cross_size =
      if constants.is_row then known_dimensions.height
      else known_dimensions.width
    in

    (* Apply constraints and calculate cross size *)
    let cross_size =
      match node_cross_size with
      | Some size ->
          let clamped =
            match (cross_min_size, cross_max_size) with
            | Some min, Some max -> Float.max min (Float.min size max)
            | Some min, None -> Float.max min size
            | None, Some max -> Float.min size max
            | None, None -> size
          in
          Float.max 0.0 (clamped -. cross_axis_padding_border)
      | None -> 0.0
    in

    (* Set the cross size for the single line *)
    match flex_lines with
    | line :: _ -> line.cross_size <- cross_size
    | [] -> ()
  else
    (* Otherwise, for each flex line, calculate based on items *)
    List.iter
      (fun line ->
        (* Find the maximum baseline *)
        let max_baseline =
          Array.fold_left
            (fun acc child -> Float.max acc child.baseline)
            0.0 line.items
        in

        (* Calculate cross size based on items *)
        let cross_size =
          Array.fold_left
            (fun acc child ->
              let child_cross_size =
                if
                  child.align_self = Baseline
                  && (not
                        (if constants.is_row then child.margin_is_auto.top
                         else child.margin_is_auto.left))
                  && not
                       (if constants.is_row then child.margin_is_auto.bottom
                        else child.margin_is_auto.right)
                then
                  (* Baseline aligned items with non-auto margins *)
                  max_baseline -. child.baseline
                  +.
                  if constants.is_row then child.hypothetical_outer_height
                  else child.hypothetical_outer_width
                else if
                  (* Other items *)
                  constants.is_row
                then child.hypothetical_outer_height
                else child.hypothetical_outer_width
              in
              Float.max acc child_cross_size)
            0.0 line.items
        in

        line.cross_size <- cross_size)
      flex_lines;

  (* If single-line, clamp the line's cross-size to container's min/max *)
  if not constants.is_wrap then
    match flex_lines with
    | line :: _ ->
        let cross_axis_padding_border =
          if constants.is_row then
            Rect.vertical_axis_sum constants.content_box_inset
          else Rect.horizontal_axis_sum constants.content_box_inset
        in
        let cross_min_size =
          if constants.is_row then constants.min_size.height
          else constants.min_size.width
        in
        let cross_max_size =
          if constants.is_row then constants.max_size.height
          else constants.max_size.width
        in

        let min_constraint =
          Option.map (fun v -> v -. cross_axis_padding_border) cross_min_size
        in
        let max_constraint =
          Option.map (fun v -> v -. cross_axis_padding_border) cross_max_size
        in

        line.cross_size <-
          (match (min_constraint, max_constraint) with
          | Some min, Some max -> Float.max min (Float.min line.cross_size max)
          | Some min, None -> Float.max min line.cross_size
          | None, Some max -> Float.min line.cross_size max
          | None, None -> line.cross_size)
    | [] -> ()

(* Handle align-content: stretch *)
let handle_align_content_stretch (flex_lines : flex_line list)
    (known_dimensions : float option size) (constants : algo_constants) : unit =
  match constants.align_content with
  | Style.Align_content.Stretch ->
      let cross_axis_padding_border =
        if constants.is_row then
          Rect.vertical_axis_sum constants.content_box_inset
        else Rect.horizontal_axis_sum constants.content_box_inset
      in

      let cross_min_size =
        if constants.is_row then constants.min_size.height
        else constants.min_size.width
      in

      let cross_max_size =
        if constants.is_row then constants.max_size.height
        else constants.max_size.width
      in

      let cross_dimension =
        if constants.is_row then known_dimensions.height
        else known_dimensions.width
      in

      let container_min_inner_cross =
        let size_or_min =
          match (cross_dimension, cross_min_size) with
          | Some size, _ -> Some size
          | None, min -> min
        in
        let clamped =
          match (size_or_min, cross_min_size, cross_max_size) with
          | Some size, Some min, Some max ->
              Some (Float.max min (Float.min size max))
          | Some size, Some min, None -> Some (Float.max min size)
          | Some size, None, Some max -> Some (Float.min size max)
          | Some size, None, None -> Some size
          | None, _, _ -> None
        in
        match clamped with
        | Some v -> Float.max 0.0 (v -. cross_axis_padding_border)
        | None -> 0.0
      in

      let cross_gap =
        if constants.is_row then constants.gap.height else constants.gap.width
      in
      let total_cross_axis_gap =
        if List.length flex_lines > 0 then
          cross_gap *. float_of_int (List.length flex_lines - 1)
        else 0.0
      in

      let lines_total_cross =
        List.fold_left (fun acc line -> acc +. line.cross_size) 0.0 flex_lines
        +. total_cross_axis_gap
      in

      if lines_total_cross < container_min_inner_cross then
        let remaining = container_min_inner_cross -. lines_total_cross in
        let addition = remaining /. float_of_int (List.length flex_lines) in
        List.iter
          (fun line -> line.cross_size <- line.cross_size +. addition)
          flex_lines
  | _ -> ()

(* Determine used cross size *)
let determine_used_cross_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (flex_lines : flex_line list) (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      let line_cross_size = line.cross_size in
      Array.iter
        (fun child ->
          let child_style = Tree.get_core_container_style tree child.node in

          let cross_target_size =
            if
              child.align_self = Style.Align_items.Stretch
              && (not
                    (if constants.is_row then child.margin_is_auto.top
                     else child.margin_is_auto.left))
              && (not
                    (if constants.is_row then child.margin_is_auto.bottom
                     else child.margin_is_auto.right))
              &&
              let size = Style.size child_style in
              if constants.is_row then Style.Dimension.is_auto size.height
              else Style.Dimension.is_auto size.width
            then
              (* Special case for stretch alignment *)
              (* For some reason this particular usage of max_width is an exception to the rule that max_width's transfer
             using the aspect_ratio (if set). Both Chrome and Firefox agree on this. And reading the spec, it seems like
             a reasonable interpretation. Although it seems to me that the spec *should* apply aspect_ratio here. *)
              let calc = Tree.resolve_calc_value tree in

              let padding =
                Style.padding child_style
                |> Length_percentage.resolve_rect_or_zero
                     constants.node_inner_size.width calc
              in
              let border =
                Style.border child_style
                |> Length_percentage.resolve_rect_or_zero
                     constants.node_inner_size.width calc
              in
              let pb_sum = Rect.sum_axes (Rect.add padding border) in
              let box_sizing_adjustment =
                if Style.box_sizing child_style = Style.Box_sizing.Content_box
                then pb_sum
                else Size.zero
              in
              let max_size_ignoring_aspect_ratio =
                Style.max_size child_style |> fun dims ->
                Size.
                  {
                    width =
                      Dimension.maybe_resolve (Size.get Inline dims)
                        constants.node_inner_size.width calc;
                    height =
                      Dimension.maybe_resolve (Size.get Block dims)
                        constants.node_inner_size.height calc;
                  }
                |> Size.maybe_add box_sizing_adjustment
              in

              let cross_margin_sum =
                if constants.is_row then child.margin.top +. child.margin.bottom
                else child.margin.left +. child.margin.right
              in

              let stretched_size = line_cross_size -. cross_margin_sum in
              let min_size =
                if constants.is_row then child.min_size.height
                else child.min_size.width
              in
              let max_size =
                if constants.is_row then max_size_ignoring_aspect_ratio.height
                else max_size_ignoring_aspect_ratio.width
              in

              match (min_size, max_size) with
              | Some min, Some max ->
                  Float.max min (Float.min stretched_size max)
              | Some min, None -> Float.max min stretched_size
              | None, Some max -> Float.min stretched_size max
              | None, None -> stretched_size
            else if
              (* Use hypothetical size *)
              constants.is_row
            then child.hypothetical_inner_height
            else child.hypothetical_inner_width
          in

          (* Set target size *)
          if constants.is_row then child.target_height <- cross_target_size
          else child.target_width <- cross_target_size;

          (* Set outer target size *)
          let cross_margin_sum =
            if constants.is_row then child.margin.top +. child.margin.bottom
            else child.margin.left +. child.margin.right
          in

          if constants.is_row then
            child.outer_target_height <- cross_target_size +. cross_margin_sum
          else child.outer_target_width <- cross_target_size +. cross_margin_sum)
        line.items)
    flex_lines

(* Helper function to sum axis gaps *)
let sum_axis_gaps (gap : float) (num_items : int) : float =
  (* Gaps only exist between items, so... *)
  if num_items <= 1 then
    (* ...if there are less than 2 items then there are no gaps *)
    0.0
  else
    (* ...otherwise there are (num_items - 1) gaps *)
    gap *. float_of_int (num_items - 1)

(* Helper function to apply alignment fallback *)
let apply_alignment_fallback (free_space : float) (num_items : int)
    (alignment_mode : align_content) (is_safe : bool) : align_content =
  (* Fallback occurs in two cases: *)

  (* 1. If there is only a single item being aligned and alignment is a
     distributed alignment keyword
     https://www.w3.org/TR/css-align-3/#distribution-values *)
  let alignment_mode, is_safe =
    if num_items <= 1 || free_space <= 0.0 then
      match alignment_mode with
      | Align_content.Stretch -> (Align_content.Flex_start, true)
      | Align_content.Space_between -> (Align_content.Flex_start, true)
      | Align_content.Space_around -> (Center, true)
      | Align_content.Space_evenly -> (Center, true)
      | _ -> (alignment_mode, is_safe)
    else (alignment_mode, is_safe)
  in

  (* 2. If free space is negative the "safe" alignment variants all fallback to
     Start alignment *)
  let alignment_mode =
    if free_space <= 0.0 && is_safe then Align_content.Start else alignment_mode
  in

  alignment_mode

(* Helper function to compute alignment offset *)
let compute_alignment_offset (free_space : float) (num_items : int)
    (gap : float) (alignment_mode : align_content)
    (layout_is_flex_reversed : bool) (is_first : bool) : float =
  if is_first then
    match alignment_mode with
    | Align_content.Start -> 0.0
    | Flex_start -> if layout_is_flex_reversed then free_space else 0.0
    | End -> free_space
    | Flex_end -> if layout_is_flex_reversed then 0.0 else free_space
    | Center -> free_space /. 2.0
    | Stretch -> 0.0
    | Space_between -> 0.0
    | Space_around ->
        if free_space >= 0.0 then free_space /. float_of_int (num_items * 2)
        else free_space /. 2.0
    | Space_evenly ->
        if free_space >= 0.0 then free_space /. float_of_int (num_items + 1)
        else free_space /. 2.0
  else
    (* For non-first items *)
    match alignment_mode with
    | Align_content.Space_between ->
        gap
        +.
        if num_items > 1 then free_space /. float_of_int (num_items - 1)
        else 0.0
    | Space_around ->
        gap
        +.
        if free_space >= 0.0 then free_space /. float_of_int num_items else 0.0
    | Space_evenly ->
        gap
        +.
        if free_space >= 0.0 then free_space /. float_of_int (num_items + 1)
        else 0.0
    | _ -> gap

(* Distribute remaining free space *)
let distribute_remaining_free_space (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      let main_gap =
        if constants.is_row then constants.gap.width else constants.gap.height
      in
      let total_main_axis_gap =
        sum_axis_gaps main_gap (Array.length line.items)
      in
      let used_space =
        total_main_axis_gap
        +. Array.fold_left
             (fun acc child ->
               acc
               +.
               if constants.is_row then child.outer_target_width
               else child.outer_target_height)
             0.0 line.items
      in
      let free_space =
        (if constants.is_row then constants.inner_container_size.width
         else constants.inner_container_size.height)
        -. used_space
      in

      (* Count auto margins *)
      let num_auto_margins = ref 0 in
      Array.iter
        (fun child ->
          if constants.is_row then (
            if child.margin_is_auto.left then incr num_auto_margins;
            if child.margin_is_auto.right then incr num_auto_margins)
          else (
            if child.margin_is_auto.top then incr num_auto_margins;
            if child.margin_is_auto.bottom then incr num_auto_margins))
        line.items;

      if free_space > 0.0 && !num_auto_margins > 0 then
        (* Distribute free space to auto margins *)
        let margin = free_space /. float_of_int !num_auto_margins in
        Array.iter
          (fun child ->
            if constants.is_row then (
              if child.margin_is_auto.left then
                child.margin <- { child.margin with left = margin };
              if child.margin_is_auto.right then
                child.margin <- { child.margin with right = margin })
            else (
              if child.margin_is_auto.top then
                child.margin <- { child.margin with top = margin };
              if child.margin_is_auto.bottom then
                child.margin <- { child.margin with bottom = margin }))
          line.items
      else
        (* Apply justify-content alignment *)
        let num_items = Array.length line.items in
        let layout_reverse = Flex_direction_ext.is_reverse constants.dir in
        let is_safe = false in
        (* TODO: Implement safe alignment *)
        let raw_justify_content_mode =
          match constants.justify_content with
          | Some jc -> jc
          | None -> Flex_start
        in
        let justify_content_mode =
          apply_alignment_fallback free_space num_items raw_justify_content_mode
            is_safe
        in

        let justify_item i child =
          child.offset_main <-
            compute_alignment_offset free_space num_items main_gap
              justify_content_mode layout_reverse (i = 0)
        in

        if layout_reverse then
          for i = Array.length line.items - 1 downto 0 do
            let rev_i = Array.length line.items - 1 - i in
            justify_item rev_i line.items.(i)
          done
        else Array.iteri justify_item line.items)
    flex_lines

(* Helper function to align flex items along cross axis *)
let align_flex_items_along_cross_axis (child : flex_item) (free_space : float)
    (max_baseline : float) (constants : algo_constants) : float =
  match child.align_self with
  | Style.Align_items.Start -> 0.0
  | Style.Align_items.Flex_start ->
      if constants.is_wrap_reverse then free_space else 0.0
  | Style.Align_items.End -> free_space
  | Style.Align_items.Flex_end ->
      if constants.is_wrap_reverse then 0.0 else free_space
  | Style.Align_items.Center -> free_space /. 2.0
  | Style.Align_items.Baseline ->
      if constants.is_row then max_baseline -. child.baseline
      else if
        (* Until we support vertical writing modes, baseline alignment only
           makes sense if the direction is row, so we treat it as flex-start
           alignment in columns. *)
        constants.is_wrap_reverse
      then free_space
      else 0.0
  | Style.Align_items.Stretch ->
      if constants.is_wrap_reverse then free_space else 0.0

(* Resolve cross-axis auto margins *)
let resolve_cross_axis_auto_margins (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  List.iter
    (fun line ->
      let line_cross_size = line.cross_size in
      let max_baseline =
        Array.fold_left
          (fun acc child -> Float.max acc child.baseline)
          0.0 line.items
      in

      Array.iter
        (fun child ->
          let free_space =
            line_cross_size
            -.
            if constants.is_row then child.outer_target_height
            else child.outer_target_width
          in

          let cross_start_auto =
            if constants.is_row then child.margin_is_auto.top
            else child.margin_is_auto.left
          in
          let cross_end_auto =
            if constants.is_row then child.margin_is_auto.bottom
            else child.margin_is_auto.right
          in

          if cross_start_auto && cross_end_auto then
            (* Both margins are auto - distribute free space equally *)
            if constants.is_row then (
              child.margin <- { child.margin with top = free_space /. 2.0 };
              child.margin <- { child.margin with bottom = free_space /. 2.0 })
            else (
              child.margin <- { child.margin with left = free_space /. 2.0 };
              child.margin <- { child.margin with right = free_space /. 2.0 })
          else if cross_start_auto then
            (* Only start margin is auto *)
            if constants.is_row then
              child.margin <- { child.margin with top = free_space }
            else child.margin <- { child.margin with left = free_space }
          else if cross_end_auto then
            (* Only end margin is auto *)
            if constants.is_row then
              child.margin <- { child.margin with bottom = free_space }
            else child.margin <- { child.margin with right = free_space }
          else
            (* No auto margins - align according to align-self *)
            child.offset_cross <-
              align_flex_items_along_cross_axis child free_space max_baseline
                constants)
        line.items)
    flex_lines

(* Determine container cross size *)
let determine_container_cross_size (flex_lines : flex_line list)
    (known_dimensions : float option size) (constants : algo_constants) : float
    =
  let cross_gap =
    if constants.is_row then constants.gap.height else constants.gap.width
  in
  let total_cross_axis_gap = sum_axis_gaps cross_gap (List.length flex_lines) in
  let total_line_cross_size =
    List.fold_left (fun acc line -> acc +. line.cross_size) 0.0 flex_lines
  in
  let padding_border_sum =
    if constants.is_row then Rect.vertical_axis_sum constants.content_box_inset
    else Rect.horizontal_axis_sum constants.content_box_inset
  in
  let cross_scrollbar_gutter =
    if constants.is_row then constants.scrollbar_gutter.y
    else constants.scrollbar_gutter.x
  in
  let min_cross_size =
    if constants.is_row then constants.min_size.height
    else constants.min_size.width
  in
  let max_cross_size =
    if constants.is_row then constants.max_size.height
    else constants.max_size.width
  in

  let cross_node_size =
    if constants.is_row then known_dimensions.height else known_dimensions.width
  in

  let outer_container_size =
    let base_size =
      match cross_node_size with
      | Some size -> size
      | None ->
          total_line_cross_size +. total_cross_axis_gap +. padding_border_sum
    in
    let clamped_size =
      match (min_cross_size, max_cross_size) with
      | Some min, Some max -> Float.max min (Float.min base_size max)
      | Some min, None -> Float.max min base_size
      | None, Some max -> Float.min base_size max
      | None, None -> base_size
    in
    Float.max clamped_size (padding_border_sum -. cross_scrollbar_gutter)
  in

  let inner_container_size =
    Float.max 0.0 (outer_container_size -. padding_border_sum)
  in

  (* Update constants *)
  if constants.is_row then (
    constants.container_size <-
      { constants.container_size with height = outer_container_size };
    constants.inner_container_size <-
      { constants.inner_container_size with height = inner_container_size })
  else (
    constants.container_size <-
      { constants.container_size with width = outer_container_size };
    constants.inner_container_size <-
      { constants.inner_container_size with width = inner_container_size });

  total_line_cross_size

(* Align flex lines per align-content *)
let align_flex_lines_per_align_content (flex_lines : flex_line list)
    (constants : algo_constants) (total_line_cross_size : float) : unit =
  let num_lines = List.length flex_lines in
  let gap =
    if constants.is_row then constants.gap.height else constants.gap.width
  in
  let total_cross_axis_gap = sum_axis_gaps gap num_lines in
  let free_space =
    (if constants.is_row then constants.inner_container_size.height
     else constants.inner_container_size.width)
    -. total_line_cross_size -. total_cross_axis_gap
  in
  let is_safe = false in
  (* TODO: Implement safe alignment *)
  let align_content_mode =
    apply_alignment_fallback free_space num_lines constants.align_content
      is_safe
  in

  let align_line i line =
    line.offset_cross <-
      compute_alignment_offset free_space num_lines gap align_content_mode
        constants.is_wrap_reverse (i = 0)
  in

  if constants.is_wrap_reverse then
    (* Process lines in reverse order for wrap-reverse *)
    List.rev flex_lines |> List.iteri align_line
  else List.iteri align_line flex_lines

(* Final layout pass *)
let final_layout_pass (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (flex_lines : flex_line list) (constants : algo_constants) : float size =
  (* Helper function to calculate flex item layout *)
  let calculate_flex_item item total_offset_main total_offset_cross
      line_offset_cross content_size container_size node_inner_size direction =
    (* Perform child layout *)
    let layout_output =
      Tree.compute_child_layout tree item.node
        (Layout_input.make ~run_mode:Run_mode.Perform_layout
           ~sizing_mode:Sizing_mode.Content_size ~axis:Requested_axis.Both
           ~known_dimensions:
             Size.
               {
                 width = Some item.target_width;
                 height = Some item.target_height;
               }
           ~parent_size:node_inner_size
           ~available_space:(Size.map Available_space.of_float container_size)
           ~vertical_margins_are_collapsible:Line.both_false)
    in

    let size = Layout_output.size layout_output in
    let content_size_output = Layout_output.content_size layout_output in

    (* Calculate main axis offset *)
    let offset_main =
      !total_offset_main +. item.offset_main
      +. Flex_direction_ext.main_start item.margin direction
      +.
      match Flex_direction_ext.main_start item.inset direction with
      | Some pos -> pos
      | None -> (
          match Flex_direction_ext.main_end item.inset direction with
          | Some pos -> -.pos
          | None -> 0.0)
    in

    (* Calculate cross axis offset *)
    let offset_cross =
      total_offset_cross +. item.offset_cross +. line_offset_cross
      +. Flex_direction_ext.cross_start item.margin direction
      +.
      match Flex_direction_ext.cross_start item.inset direction with
      | Some pos -> pos
      | None -> (
          match Flex_direction_ext.cross_end item.inset direction with
          | Some pos -> -.pos
          | None -> 0.0)
    in

    (* Update baseline *)
    (if Flex_direction_ext.is_row direction then
       let baseline_offset_cross =
         total_offset_cross +. item.offset_cross
         +. Flex_direction_ext.cross_start item.margin direction
       in
       let inner_baseline =
         Option.value (Layout_output.first_baselines layout_output).y
           ~default:size.height
       in
       item.baseline <- baseline_offset_cross +. inner_baseline
     else
       let baseline_offset_main =
         !total_offset_main +. item.offset_main
         +. Flex_direction_ext.main_start item.margin direction
       in
       let inner_baseline =
         Option.value (Layout_output.first_baselines layout_output).y
           ~default:size.height
       in
       item.baseline <- baseline_offset_main +. inner_baseline);

    (* Calculate location *)
    let location =
      if Flex_direction_ext.is_row direction then
        Point.{ x = offset_main; y = offset_cross }
      else Point.{ x = offset_cross; y = offset_main }
    in

    (* Calculate scrollbar size *)
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

    (* Set layout on tree *)
    Tree.set_unrounded_layout tree item.node
      (Layout.make ~order:item.order ~location ~size
         ~content_size:content_size_output ~scrollbar_size ~border:item.border
         ~padding:item.padding ~margin:item.margin);

    (* Update main offset *)
    total_offset_main :=
      !total_offset_main +. item.offset_main
      +. Flex_direction_ext.main_axis_sum item.margin direction
      +. Flex_direction_ext.main_size size direction;

    (* Update content size *)
    let contribution_width =
      match item.overflow.x with
      | Overflow.Visible -> max size.width content_size_output.width
      | _ -> size.width
    in
    let contribution_height =
      match item.overflow.y with
      | Overflow.Visible -> max size.height content_size_output.height
      | _ -> size.height
    in
    if contribution_width > 0.0 || contribution_height > 0.0 then
      let content_contribution =
        Size.
          {
            width = location.x +. contribution_width;
            height = location.y +. contribution_height;
          }
      in
      let current_size = !content_size in
      let new_width =
        max current_size.Size.width content_contribution.Size.width
      in
      let new_height =
        max current_size.Size.height content_contribution.Size.height
      in
      content_size := Size.{ width = new_width; height = new_height }
  in

  (* Helper function to calculate layout for a line *)
  let calculate_layout_line line total_offset_cross content_size container_size
      node_inner_size padding_border direction =
    let total_offset_main =
      ref
        (if constants.is_row then padding_border.Rect.left
         else padding_border.Rect.top)
    in
    let line_offset_cross = line.offset_cross in

    if Flex_direction_ext.is_reverse direction then
      for i = Array.length line.items - 1 downto 0 do
        let item = line.items.(i) in
        calculate_flex_item item total_offset_main !total_offset_cross
          line_offset_cross content_size container_size node_inner_size
          direction
      done
    else
      Array.iter
        (fun item ->
          calculate_flex_item item total_offset_main !total_offset_cross
            line_offset_cross content_size container_size node_inner_size
            direction)
        line.items;

    total_offset_cross :=
      !total_offset_cross +. line_offset_cross +. line.cross_size
  in

  (* Main implementation *)
  let total_offset_cross =
    ref
      (if constants.is_row then constants.content_box_inset.top
       else constants.content_box_inset.left)
  in
  let content_size = ref Size.zero in

  let lines =
    if constants.is_wrap_reverse then List.rev flex_lines else flex_lines
  in

  List.iter
    (fun line ->
      calculate_layout_line line total_offset_cross content_size
        constants.container_size constants.node_inner_size
        constants.content_box_inset constants.dir)
    lines;

  (* Add final adjustments to content size *)
  Size.
    {
      width =
        !content_size.width +. constants.content_box_inset.right
        -. constants.border.right -. constants.scrollbar_gutter.x;
      height =
        !content_size.height +. constants.content_box_inset.bottom
        -. constants.border.bottom -. constants.scrollbar_gutter.y;
    }

(* Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (constants : algo_constants) : float size =
  let container_width = constants.container_size.width in
  let container_height = constants.container_size.height in
  let inset_relative_size =
    Size.sub constants.container_size
      (Size.add
         (Rect.sum_axes constants.border)
         Size.
           {
             width = constants.scrollbar_gutter.x;
             height = constants.scrollbar_gutter.y;
           })
  in

  let content_size = ref Size.zero in
  let calc = Tree.resolve_calc_value tree in

  for order = 0 to Tree.child_count tree node - 1 do
    let child = Tree.get_child_id tree node order in
    let child_style = Tree.get_core_container_style tree child in

    (* Skip items that are display:none or are not position:absolute *)
    if
      Style.box_generation_mode child_style = Box_generation_mode.None
      || Style.position child_style <> Position.Absolute
    then ()
    else
      let overflow = Style.overflow child_style in
      let scrollbar_width = Style.scrollbar_width child_style in
      let aspect_ratio = Style.aspect_ratio child_style in
      let align_self =
        Option.value
          (Style.align_self child_style)
          ~default:constants.align_items
      in

      (* Resolve margin *)
      let margin =
        Style.margin child_style
        |> Rect.map (fun m ->
            Length_percentage_auto.resolve_to_option_with_calc m
              inset_relative_size.width calc)
      in

      (* Resolve padding *)
      let padding =
        Style.padding child_style
        |> Length_percentage.resolve_rect_or_zero
             (Some inset_relative_size.width) calc
      in

      (* Resolve border *)
      let border =
        Style.border child_style
        |> Length_percentage.resolve_rect_or_zero
             (Some inset_relative_size.width) calc
      in

      let padding_border_sum = Rect.add padding border |> Rect.sum_axes in
      let box_sizing_adjustment =
        if Style.box_sizing child_style = Box_sizing.Content_box then
          padding_border_sum
        else Size.zero
      in

      (* Resolve inset *)
      let left =
        Style.inset child_style |> fun inset ->
        Length_percentage_auto.maybe_resolve inset.left
          (Some inset_relative_size.width) calc
      in
      let right =
        Style.inset child_style |> fun inset ->
        Length_percentage_auto.maybe_resolve inset.right
          (Some inset_relative_size.width) calc
      in
      let top =
        Style.inset child_style |> fun inset ->
        Length_percentage_auto.maybe_resolve inset.top
          (Some inset_relative_size.height) calc
      in
      let bottom =
        Style.inset child_style |> fun inset ->
        Length_percentage_auto.maybe_resolve inset.bottom
          (Some inset_relative_size.height) calc
      in

      (* Compute known dimensions from min/max/inherent size styles *)
      let style_dims = Style.size child_style in
      let style_width =
        Dimension.maybe_resolve style_dims.width
          (Some inset_relative_size.width) calc
      in
      let style_height =
        Dimension.maybe_resolve style_dims.height
          (Some inset_relative_size.height) calc
      in
      let style_width, style_height =
        match (aspect_ratio, style_width, style_height) with
        | Some ratio, Some w, None -> (Some w, Some (w /. ratio))
        | Some ratio, None, Some h -> (Some (h *. ratio), Some h)
        | _ -> (style_width, style_height)
      in
      let style_width, style_height =
        match (box_sizing_adjustment.width, box_sizing_adjustment.height) with
        | 0.0, 0.0 -> (style_width, style_height)
        | bw, bh ->
            ( Option.map (fun w -> w +. bw) style_width,
              Option.map (fun h -> h +. bh) style_height )
      in

      let min_dims = Style.min_size child_style in
      let min_width =
        Dimension.maybe_resolve min_dims.width (Some inset_relative_size.width)
          calc
      in
      let min_height =
        Dimension.maybe_resolve min_dims.height
          (Some inset_relative_size.height) calc
      in
      let min_width, min_height =
        match (aspect_ratio, min_width, min_height) with
        | Some ratio, Some w, None -> (Some w, Some (w /. ratio))
        | Some ratio, None, Some h -> (Some (h *. ratio), Some h)
        | _ -> (min_width, min_height)
      in
      let min_width, min_height =
        match (box_sizing_adjustment.width, box_sizing_adjustment.height) with
        | 0.0, 0.0 -> (min_width, min_height)
        | bw, bh ->
            ( Option.map (fun w -> w +. bw) min_width,
              Option.map (fun h -> h +. bh) min_height )
      in
      let min_width =
        Some
          (Float.max
             (Option.value min_width ~default:padding_border_sum.width)
             padding_border_sum.width)
      in
      let min_height =
        Some
          (Float.max
             (Option.value min_height ~default:padding_border_sum.height)
             padding_border_sum.height)
      in

      let max_dims = Style.max_size child_style in
      let max_width =
        Dimension.maybe_resolve max_dims.width (Some inset_relative_size.width)
          calc
      in
      let max_height =
        Dimension.maybe_resolve max_dims.height
          (Some inset_relative_size.height) calc
      in
      let max_width, max_height =
        match (aspect_ratio, max_width, max_height) with
        | Some ratio, Some w, None -> (Some w, Some (w /. ratio))
        | Some ratio, None, Some h -> (Some (h *. ratio), Some h)
        | _ -> (max_width, max_height)
      in
      let max_width, max_height =
        match (box_sizing_adjustment.width, box_sizing_adjustment.height) with
        | 0.0, 0.0 -> (max_width, max_height)
        | bw, bh ->
            ( Option.map (fun w -> w +. bw) max_width,
              Option.map (fun h -> h +. bh) max_height )
      in

      (* Helper to clamp a value between optional min and max *)
      let clamp_opt value min_val max_val =
        match (value, min_val, max_val) with
        | Some v, Some min_v, Some max_v ->
            Some (Float.max min_v (Float.min v max_v))
        | Some v, Some min_v, None -> Some (Float.max min_v v)
        | Some v, None, Some max_v -> Some (Float.min v max_v)
        | v, _, _ -> v
      in

      (* Clamp style_size between min and max *)
      let known_dimensions =
        {
          Size.width = clamp_opt style_width min_width max_width;
          height = clamp_opt style_height min_height max_height;
        }
      in

      (* Fill in width from left/right and reapply aspect ratio if: - Width is
         not already known - Item has both left and right inset properties
         set *)
      let known_dimensions =
        match (known_dimensions.width, left, right) with
        | None, Some l, Some r ->
            let new_width_raw =
              inset_relative_size.width
              -. Option.value margin.left ~default:0.0
              -. Option.value margin.right ~default:0.0
              -. l -. r
            in
            let width = Some (Float.max new_width_raw 0.0) in
            let width, height =
              match (aspect_ratio, width, known_dimensions.height) with
              | Some ratio, Some w, None -> (Some w, Some (w /. ratio))
              | Some ratio, None, Some h -> (Some (h *. ratio), Some h)
              | _ -> (width, known_dimensions.height)
            in
            {
              Size.width = clamp_opt width min_width max_width;
              height = clamp_opt height min_height max_height;
            }
        | _ -> known_dimensions
      in

      (* Fill in height from top/bottom and reapply aspect ratio if: - Height is
         not already known - Item has both top and bottom inset properties
         set *)
      let known_dimensions =
        match (known_dimensions.height, top, bottom) with
        | None, Some t, Some b ->
            let new_height_raw =
              inset_relative_size.height
              -. Option.value margin.top ~default:0.0
              -. Option.value margin.bottom ~default:0.0
              -. t -. b
            in
            let height = Some (Float.max new_height_raw 0.0) in
            let width, height =
              match (aspect_ratio, known_dimensions.width, height) with
              | Some ratio, Some w, None -> (Some w, Some (w /. ratio))
              | Some ratio, None, Some h -> (Some (h *. ratio), Some h)
              | _ -> (known_dimensions.width, height)
            in
            {
              Size.width = clamp_opt width min_width max_width;
              height = clamp_opt height min_height max_height;
            }
        | _ -> known_dimensions
      in

      let available_space =
        Size.
          {
            width =
              Available_space.of_float
                (Option.value
                   (Option.map
                      (fun w -> Float.min w (Option.value max_width ~default:w))
                      (Option.map
                         (fun w ->
                           Float.max w (Option.value min_width ~default:w))
                         (Some container_width)))
                   ~default:container_width);
            height =
              Available_space.of_float
                (Option.value
                   (Option.map
                      (fun h ->
                        Float.min h (Option.value max_height ~default:h))
                      (Option.map
                         (fun h ->
                           Float.max h (Option.value min_height ~default:h))
                         (Some container_height)))
                   ~default:container_height);
          }
      in

      (* Measure child to determine its inherent size *)
      let measure_output =
        Tree.compute_child_layout tree child
          (Layout_input.make ~run_mode:Run_mode.Compute_size
             ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
             ~known_dimensions ~parent_size:constants.node_inner_size
             ~available_space ~vertical_margins_are_collapsible:Line.both_false)
      in

      let measured_size = Layout_output.size measure_output in
      let final_size = Size.unwrap_or measured_size known_dimensions in
      (* Clamp final size between min and max - min wins over max when they
         conflict *)
      let final_size =
        Size.
          {
            width =
              (match (final_size.width, min_width, max_width) with
              | w, Some min_w, Some max_w -> Float.max min_w (Float.min w max_w)
              | w, Some min_w, None -> Float.max min_w w
              | w, None, Some max_w -> Float.min w max_w
              | w, _, _ -> w);
            height =
              (match (final_size.height, min_height, max_height) with
              | h, Some min_h, Some max_h -> Float.max min_h (Float.min h max_h)
              | h, Some min_h, None -> Float.max min_h h
              | h, None, Some max_h -> Float.min h max_h
              | h, _, _ -> h);
          }
      in

      (* Perform child layout with resolved size *)
      let layout_output =
        Tree.compute_child_layout tree child
          (Layout_input.make ~run_mode:Run_mode.Perform_layout
             ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
             ~known_dimensions:(Size.map Option.some final_size)
             ~parent_size:constants.node_inner_size ~available_space
             ~vertical_margins_are_collapsible:Line.both_false)
      in

      (* Calculate non-auto margins *)
      let non_auto_margin =
        Rect.map (fun m -> Option.value m ~default:0.0) margin
      in

      (* Calculate free space *)
      let free_space =
        Size.
          {
            width =
              constants.container_size.width -. final_size.width
              -. (non_auto_margin.left +. non_auto_margin.right);
            height =
              constants.container_size.height -. final_size.height
              -. (non_auto_margin.top +. non_auto_margin.bottom);
          }
      in
      let free_space =
        Size.
          {
            width = Float.max free_space.width 0.0;
            height = Float.max free_space.height 0.0;
          }
      in

      (* Expand auto margins to fill available space *)
      let resolved_margin =
        let auto_margin_count_width =
          (if Option.is_none margin.left then 1 else 0)
          + if Option.is_none margin.right then 1 else 0
        in
        let auto_margin_count_height =
          (if Option.is_none margin.top then 1 else 0)
          + if Option.is_none margin.bottom then 1 else 0
        in
        let auto_margin_size =
          Size.
            {
              width =
                (if auto_margin_count_width > 0 then
                   free_space.width /. float_of_int auto_margin_count_width
                 else 0.0);
              height =
                (if auto_margin_count_height > 0 then
                   free_space.height /. float_of_int auto_margin_count_height
                 else 0.0);
            }
        in
        Rect.
          {
            left = Option.value margin.left ~default:auto_margin_size.width;
            right = Option.value margin.right ~default:auto_margin_size.width;
            top = Option.value margin.top ~default:auto_margin_size.height;
            bottom = Option.value margin.bottom ~default:auto_margin_size.height;
          }
      in

      (* Determine flex-relative insets *)
      let start_main, end_main =
        if constants.is_row then (left, right) else (top, bottom)
      in
      let start_cross, end_cross =
        if constants.is_row then (top, bottom) else (left, right)
      in

      (* Apply main-axis alignment *)
      let offset_main =
        match start_main with
        | Some start ->
            start
            +. (if constants.is_row then constants.border.left
                else constants.border.top)
            +.
            if constants.is_row then resolved_margin.left
            else resolved_margin.top
        | None -> (
            match end_main with
            | Some end_val ->
                (if constants.is_row then constants.container_size.width
                 else constants.container_size.height)
                -. (if constants.is_row then constants.border.right
                    else constants.border.bottom)
                -. (if constants.is_row then constants.scrollbar_gutter.x
                    else constants.scrollbar_gutter.y)
                -. (if constants.is_row then final_size.width
                    else final_size.height)
                -. end_val
                -.
                if constants.is_row then resolved_margin.right
                else resolved_margin.bottom
            | None -> (
                (* Handle justify_content alignment *)
                match
                  ( Option.value constants.justify_content ~default:Start,
                    constants.is_wrap_reverse )
                with
                | Space_between, _
                | Start, _
                | Stretch, false
                | Flex_start, false
                | Flex_end, true ->
                    (if constants.is_row then constants.content_box_inset.left
                     else constants.content_box_inset.top)
                    +.
                    if constants.is_row then resolved_margin.left
                    else resolved_margin.top
                | End, _ | Flex_end, false | Flex_start, true | Stretch, true ->
                    (if constants.is_row then constants.container_size.width
                     else constants.container_size.height)
                    -. (if constants.is_row then
                          constants.content_box_inset.right
                        else constants.content_box_inset.bottom)
                    -. (if constants.is_row then final_size.width
                        else final_size.height)
                    -.
                    if constants.is_row then resolved_margin.right
                    else resolved_margin.bottom
                | Space_evenly, _ | Space_around, _ | Center, _ ->
                    ((if constants.is_row then constants.container_size.width
                      else constants.container_size.height)
                    +. (if constants.is_row then
                          constants.content_box_inset.left
                        else constants.content_box_inset.top)
                    -. (if constants.is_row then
                          constants.content_box_inset.right
                        else constants.content_box_inset.bottom)
                    -. (if constants.is_row then final_size.width
                        else final_size.height)
                    +. (if constants.is_row then resolved_margin.left
                        else resolved_margin.top)
                    -.
                    if constants.is_row then resolved_margin.right
                    else resolved_margin.bottom)
                    /. 2.0))
      in

      (* Apply cross-axis alignment *)
      let offset_cross =
        match start_cross with
        | Some start ->
            start
            +. (if constants.is_row then constants.border.top
                else constants.border.left)
            +.
            if constants.is_row then resolved_margin.top
            else resolved_margin.left
        | None -> (
            match end_cross with
            | Some end_val ->
                (if constants.is_row then constants.container_size.height
                 else constants.container_size.width)
                -. (if constants.is_row then constants.border.bottom
                    else constants.border.right)
                -. (if constants.is_row then constants.scrollbar_gutter.y
                    else constants.scrollbar_gutter.x)
                -. (if constants.is_row then final_size.height
                    else final_size.width)
                -. end_val
                -.
                if constants.is_row then resolved_margin.bottom
                else resolved_margin.right
            | None -> (
                match (align_self, constants.is_wrap_reverse) with
                (* Stretch alignment does not apply to absolutely positioned
                   items *)
                | Start, _
                | Baseline, false
                | Stretch, false
                | Flex_start, false
                | Flex_end, true ->
                    (if constants.is_row then constants.content_box_inset.top
                     else constants.content_box_inset.left)
                    +.
                    if constants.is_row then resolved_margin.top
                    else resolved_margin.left
                | End, _
                | Baseline, true
                | Stretch, true
                | Flex_start, true
                | Flex_end, false ->
                    (if constants.is_row then constants.container_size.height
                     else constants.container_size.width)
                    -. (if constants.is_row then
                          constants.content_box_inset.bottom
                        else constants.content_box_inset.right)
                    -. (if constants.is_row then final_size.height
                        else final_size.width)
                    -.
                    if constants.is_row then resolved_margin.bottom
                    else resolved_margin.right
                | Center, _ ->
                    ((if constants.is_row then constants.container_size.height
                      else constants.container_size.width)
                    +. (if constants.is_row then constants.content_box_inset.top
                        else constants.content_box_inset.left)
                    -. (if constants.is_row then
                          constants.content_box_inset.bottom
                        else constants.content_box_inset.right)
                    -. (if constants.is_row then final_size.height
                        else final_size.width)
                    +. (if constants.is_row then resolved_margin.top
                        else resolved_margin.left)
                    -.
                    if constants.is_row then resolved_margin.bottom
                    else resolved_margin.right)
                    /. 2.0))
      in

      let location =
        if constants.is_row then Point.{ x = offset_main; y = offset_cross }
        else Point.{ x = offset_cross; y = offset_main }
      in

      let scrollbar_size =
        Size.
          {
            width =
              (if overflow.y = Overflow.Scroll then scrollbar_width else 0.0);
            height =
              (if overflow.x = Overflow.Scroll then scrollbar_width else 0.0);
          }
      in

      (* Set layout on tree *)
      Tree.set_unrounded_layout tree child
        (Layout.make ~order ~location ~size:final_size
           ~content_size:(Layout_output.content_size layout_output)
           ~scrollbar_size ~border ~padding ~margin:resolved_margin);

      (* Update content size *)
      let content_size_output = Layout_output.content_size layout_output in
      let size_content_size_contribution =
        Size.
          {
            width =
              (match overflow.x with
              | Overflow.Visible ->
                  Float.max final_size.width content_size_output.width
              | _ -> final_size.width);
            height =
              (match overflow.y with
              | Overflow.Visible ->
                  Float.max final_size.height content_size_output.height
              | _ -> final_size.height);
          }
      in

      if
        size_content_size_contribution.width > 0.0
        || size_content_size_contribution.height > 0.0
      then
        let content_size_contribution =
          Size.
            {
              width = location.x +. size_content_size_contribution.width;
              height = location.y +. size_content_size_contribution.height;
            }
        in
        let current_size = !content_size in
        content_size :=
          Size.
            {
              width =
                Float.max current_size.width content_size_contribution.width;
              height =
                Float.max current_size.height content_size_contribution.height;
            }
  done;

  !content_size

(* Compute a preliminary size for an item *)
let compute_preliminary (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (inputs : Layout_input.t) : Layout_output.t =
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let run_mode = Layout_input.run_mode inputs in
  let _sizing_mode = Layout_input.sizing_mode inputs in

  (* Define some general constants we will need for the remainder of the
     algorithm *)
  let constants =
    compute_constants
      (module Tree)
      tree
      (Tree.get_core_container_style tree node)
      known_dimensions parent_size
  in

  (* 9. Flex Layout Algorithm *)

  (* 9.1. Initial Setup *)

  (* 1. Generate anonymous flex items as described in §4 Flex Items *)
  let flex_items =
    generate_anonymous_flex_items (module Tree) tree node constants
  in

  (* 9.2. Line Length Determination *)

  (* 2. Determine the available main and cross space for the flex items *)
  let available_space =
    determine_available_space known_dimensions available_space constants
  in

  (* 3. Determine the flex base size and hypothetical main size of each item *)
  determine_flex_base_size
    (module Tree)
    tree constants available_space flex_items;

  (* 4. Determine the main size of the flex container *)
  (* This is done as part of compute_constants and collection of flex lines *)

  (* 9.3. Main Size Determination *)

  (* 5. Collect flex items into flex lines *)
  let flex_lines = collect_flex_lines constants available_space flex_items in

  (* If container size is undefined, determine the container's main size *)
  (match
     Flex_direction_ext.main_size constants.node_inner_size constants.dir
   with
  | Some inner_main_size ->
      let outer_main_size =
        inner_main_size
        +. Flex_direction_ext.main_axis_sum constants.content_box_inset
             constants.dir
      in
      if constants.is_row then (
        constants.inner_container_size <-
          { constants.inner_container_size with width = inner_main_size };
        constants.container_size <-
          { constants.container_size with width = outer_main_size })
      else (
        constants.inner_container_size <-
          { constants.inner_container_size with height = inner_main_size };
        constants.container_size <-
          { constants.container_size with height = outer_main_size })
  | None ->
      (* Sets constants.container_size and constants.inner_container_size *)
      determine_container_main_size
        (module Tree)
        tree available_space flex_lines constants;
      let inner_main =
        Flex_direction_ext.main_size constants.inner_container_size
          constants.dir
      in
      let outer_main =
        Flex_direction_ext.main_size constants.container_size constants.dir
      in
      if constants.is_row then (
        constants.node_inner_size <-
          { constants.node_inner_size with width = Some inner_main };
        constants.node_outer_size <-
          { constants.node_outer_size with width = Some outer_main })
      else (
        constants.node_inner_size <-
          { constants.node_inner_size with height = Some inner_main };
        constants.node_outer_size <-
          { constants.node_outer_size with height = Some outer_main });

      (* Re-resolve percentage gaps *)
      let style = Tree.get_core_container_style tree node in
      let inner_container_size =
        Flex_direction_ext.main_size constants.inner_container_size
          constants.dir
      in
      let calc = Tree.resolve_calc_value tree in
      let new_gap =
        Style.gap style |> fun gap_size ->
        match constants.dir with
        | Row | Row_reverse ->
            Length_percentage.maybe_resolve (Size.get Inline gap_size)
              (Some inner_container_size) calc
            |> Option.value ~default:0.0
        | Column | Column_reverse ->
            Length_percentage.maybe_resolve (Size.get Block gap_size)
              (Some inner_container_size) calc
            |> Option.value ~default:0.0
      in
      if constants.is_row then
        constants.gap <- { constants.gap with width = new_gap }
      else constants.gap <- { constants.gap with height = new_gap });

  (* 6. Resolve the flexible lengths of all the flex items to find their used
     main size *)
  List.iter (fun line -> resolve_flexible_lengths line constants) flex_lines;

  (* 9.4. Cross Size Determination *)

  (* 7. Determine the hypothetical cross size of each item *)
  List.iter
    (fun line ->
      determine_hypothetical_cross_size
        (module Tree)
        tree line constants available_space)
    flex_lines;

  (* Calculate child baselines *)
  calculate_children_base_lines
    (module Tree)
    tree known_dimensions available_space flex_lines constants;

  (* 8. Calculate the cross size of each flex line *)
  calculate_cross_size flex_lines known_dimensions constants;

  (* 9. Handle 'align-content: stretch' *)
  handle_align_content_stretch flex_lines known_dimensions constants;

  (* 10. Collapse visibility:collapse items - TODO: Not implemented *)

  (* 11. Determine the used cross size of each flex item *)
  determine_used_cross_size (module Tree) tree flex_lines constants;

  (* 9.5. Main-Axis Alignment *)

  (* 12. Distribute any remaining free space *)
  distribute_remaining_free_space flex_lines constants;

  (* 9.6. Cross-Axis Alignment *)

  (* 13. Resolve cross-axis auto margins *)
  resolve_cross_axis_auto_margins flex_lines constants;

  (* 15. Determine the flex container's used cross size *)
  let total_line_cross_size =
    determine_container_cross_size flex_lines known_dimensions constants
  in

  (* We have the container size. If our caller does not care about performing
     layout we are done now *)
  if run_mode = Run_mode.Compute_size then
    Layout_output.from_outer_size constants.container_size
  else (
    (* 16. Align all flex lines per align-content *)
    align_flex_lines_per_align_content flex_lines constants
      total_line_cross_size;

    (* Do a final layout pass and gather the resulting layouts *)
    let inflow_content_size =
      final_layout_pass (module Tree) tree flex_lines constants
    in

    (* Before returning we perform absolute layout on all absolutely positioned
       children *)
    let absolute_content_size =
      perform_absolute_layout_on_absolute_children
        (module Tree)
        tree node constants
    in

    (* Perform hidden layout on hidden children *)
    let len = Tree.child_count tree node in
    for order = 0 to len - 1 do
      let child = Tree.get_child_id tree node order in
      if
        Style.box_generation_mode (Tree.get_core_container_style tree child)
        = Box_generation_mode.None
      then (
        Tree.set_unrounded_layout tree child (Layout.with_order order);
        Tree.compute_child_layout tree child
          (Layout_input.make ~run_mode:Run_mode.Perform_layout
             ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
             ~known_dimensions:Size.none ~parent_size:Size.none
             ~available_space:
               Size.
                 {
                   width = Available_space.max_content;
                   height = Available_space.max_content;
                 }
             ~vertical_margins_are_collapsible:Line.both_false)
        |> ignore)
    done;

    (* Calculate flex container baselines *)
    let first_vertical_baseline =
      match flex_lines with
      | [] -> None
      | first_line :: _ -> (
          (* For row containers: find item with baseline alignment or fallback
             to first item For column containers: always use first item *)
          let items = first_line.items in
          let len = Array.length items in
          let baseline_item =
            if Flex_direction_ext.is_row constants.dir then
              let rec find i =
                if i >= len then None
                else
                  let item = items.(i) in
                  if item.align_self = Baseline then Some item else find (i + 1)
              in
              find 0
            else None
          in
          let item_opt =
            match baseline_item with
            | Some item -> Some item
            | None -> if len > 0 then Some items.(0) else None
          in
          match item_opt with
          | None -> None
          | Some item ->
              let offset_cross =
                if Flex_direction_ext.is_row constants.dir then
                  item.offset_cross
                else item.offset_main
              in
              Some (offset_cross +. item.baseline))
    in

    (* Return final layout output *)
    let content_size = Size.max inflow_content_size absolute_content_size in
    Layout_output.make ~size:constants.container_size ~content_size
      ~first_baselines:Point.{ x = None; y = first_vertical_baseline }
      ~top_margin:Collapsible_margin_set.zero
      ~bottom_margin:Collapsible_margin_set.zero
      ~margins_can_collapse_through:false)

(* Main entry point for flexbox layout computation *)
let compute_flexbox_layout (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (inputs : Layout_input.t) : Layout_output.t =
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let run_mode = Layout_input.run_mode inputs in
  let sizing_mode = Layout_input.sizing_mode inputs in

  let style = Tree.get_core_container_style tree node in
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
  let padding_border_sum = Rect.sum_axes (Rect.add padding border) in
  let box_sizing_adjustment =
    if Style.box_sizing style = Box_sizing.Content_box then padding_border_sum
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

  (* The size of the container should be floored by the padding and border *)
  let styled_based_known_dimensions =
    known_dimensions
    |> Size.choose_first min_max_definite_size
    |> Size.choose_first clamped_style_size
    |> Size.maybe_max padding_border_sum
  in

  (* Short-circuit layout if the container's size is fully determined and we're
     in ComputeSize mode *)
  if run_mode = Run_mode.Compute_size then
    match styled_based_known_dimensions with
    | { width = Some width; height = Some height } ->
        Layout_output.from_outer_size Size.{ width; height }
    | _ ->
        (* Continue with compute_preliminary *)
        compute_preliminary
          (module Tree)
          tree node
          (Layout_input.make ~run_mode ~sizing_mode
             ~axis:(Layout_input.axis inputs)
             ~known_dimensions:styled_based_known_dimensions ~parent_size
             ~available_space:(Layout_input.available_space inputs)
             ~vertical_margins_are_collapsible:
               (Layout_input.vertical_margins_are_collapsible inputs))
  else
    compute_preliminary
      (module Tree)
      tree node
      (Layout_input.make ~run_mode ~sizing_mode ~axis:(Layout_input.axis inputs)
         ~known_dimensions:styled_based_known_dimensions ~parent_size
         ~available_space:(Layout_input.available_space inputs)
         ~vertical_margins_are_collapsible:
           (Layout_input.vertical_margins_are_collapsible inputs))
