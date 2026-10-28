(* Flexbox layout computation algorithm *)

module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

(* Stdlib's max and min on floats, compiled without boxing their
   arguments. *)
let fmax (a : float) b = if a >= b then a else b
let fmin (a : float) b = if a <= b then a else b

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

  let main_size_opt size dir =
    if is_row dir then size.Size.width else size.Size.height

  let _cross_size (size : Sizef.t) dir =
    if is_row dir then size.height else size.width

  let main_axis_sum rect dir =
    if is_row dir then Rectf.horizontal_axis_sum rect
    else Rectf.vertical_axis_sum rect

  let _cross_axis_sum rect dir =
    if is_row dir then Rectf.vertical_axis_sum rect
    else Rectf.horizontal_axis_sum rect

  (* Point accessors based on flex direction *)
  let _main_axis_pt pt dir = if is_row dir then pt.Point.x else pt.Point.y
  let _cross_axis_pt pt dir = if is_row dir then pt.Point.y else pt.Point.x
end

(* The sizes a flexbox calculation computes for a single item. Every field is
   a float, so the record is one flat block that a write updates in place. A
   size the item's style leaves unset is NaN. *)
type item_sizes = {
  mutable size_width : float; (* The preferred size of this item *)
  mutable size_height : float;
  mutable min_width : float; (* The minimum allowable size of this item *)
  mutable min_height : float;
  mutable max_width : float; (* The maximum allowable size of this item *)
  mutable max_height : float;
  mutable flex_basis : float; (* The default size of this item *)
  mutable resolved_minimum_main_size : float;
      (* The minimum size of the item, NaN while its automatic minimum is not
         measured *)
  mutable violation : float;
      (* The amount by which this item has deviated from its target size *)
  mutable content_flex_fraction : float;
      (* Either the max- or min- content flex fraction *)
  mutable hypothetical_inner_width : float;
  mutable hypothetical_inner_height : float;
  mutable target_width : float; (* The size that this item wants to be *)
  mutable target_height : float;
  mutable outer_target_width : float;
  mutable outer_target_height : float;
  mutable baseline : float; (* The position of the bottom edge of this item *)
  mutable offset_main : float; (* A temporary value for the main offset *)
  mutable offset_cross : float; (* A temporary value for the cross offset *)
}

(* The intermediate results of a flexbox calculation for a single item *)
type flex_item = {
  node : Node_id.t; (* The identifier for the associated node *)
  order : int; (* The order of the node relative to its siblings *)
  style : Style.t; (* The style of this item *)
  align_self : align_self; (* The cross-alignment of this item *)
  (* The constraints the item is measured under for its flex base size and its
     min-content main size, besides its parent size (the container's inner
     cross size): its known dimensions (its cross size when its style sets
     one or it is stretched), and the available space in its cross axis,
     clamped by its own cross-axis minimum and maximum. *)
  mutable measure_known_dimensions : float option size;
  mutable measure_available_cross : Available_space.t;
  mutable margin : Rectf.t; (* The margin of this item *)
  padding : Rectf.t; (* The padding of this item *)
  border : Rectf.t; (* The border of this item *)
  mutable frozen : bool; (* Is the size of this item locked *)
  mutable flex_basis_is_definite : bool;
      (* Whether the item's used flex basis is definite rather than derived
         from its content *)
  s : item_sizes; (* The item's sizes *)
}

(* A line of flex_items used for intermediate computation *)
type flex_line = {
  items : flex_item array;
      (* The slice of items to iterate over during computation of this line *)
  mutable cross_size : float; (* The dimensions of the cross-axis *)
  mutable offset_cross : float; (* The relative offset of the cross-axis *)
}

(* The sizes of the container a flexbox calculation computes. Every field is
   a float, so the record is one flat block that a write updates in place. A
   size the container's style leaves unset is NaN. *)
type container_sizes = {
  mutable width : float;
      (* The size of the virtual container containing the flex items *)
  mutable height : float;
  mutable inner_width : float; (* The size of the internal container *)
  mutable inner_height : float;
  mutable gap_width : float; (* The gap of this section *)
  mutable gap_height : float;
  scrollbar_gutter_x : float;
      (* The size reserved for scrollbar gutters in each axis *)
  scrollbar_gutter_y : float;
  min_width : float; (* The container's min_size style *)
  min_height : float;
  max_width : float; (* The container's max_size style *)
  max_height : float;
}

(* Values that can be cached during the flexbox algorithm *)
type algo_constants = {
  (* Direction *)
  dir : flex_direction;
      (* The direction of the current segment being laid out *)
  is_row : bool; (* Is this segment a row *)
  is_wrap : bool; (* Is wrapping enabled (in either direction) *)
  is_wrap_reverse : bool; (* Is the wrap direction inverted *)
  (* Box model *)
  border : Rectf.t; (* The border of this section *)
  content_box_inset : Rectf.t;
  (* The space between the content box and the border box *)
  (* Layout properties *)
  align_items : align_items; (* The align_items property of this node *)
  align_content : align_content; (* The align_content property of this node *)
  justify_content : justify_content option;
  (* The justify_content property of this node *)
  (* Computed sizes *)
  mutable node_outer_size : float option size;
      (* The border-box size of the node being laid out (if known) *)
  mutable node_inner_size : float option size;
      (* The content-box size of the node being laid out (if known) *)
  initial_node_inner_size : float option size;
      (* The content-box size of the node as its style and its known
         dimensions give it, against which the items' insets resolve *)
  known_main_size_is_definite : bool;
      (* Whether the container's known main size, if any, is definite: a
         main size its parent derived from its content is not, so its items
         resolve no percentage against it and it does not wrap them *)
  known_cross_size_is_definite : bool;
      (* Whether the container's known cross size, if any, is definite *)
  has_definite_main_size : bool;
      (* Whether the container has a known main size that is definite *)
  has_definite_cross_size : bool;
      (* Whether the container has a known cross size that is definite, and
         not only its minimum and maximum give *)
  cross_axis_available_space_is_definite : bool;
      (* Whether the cross-axis space a non-stretched item is fit into is
         definite: the container's cross size is, or the available cross
         space is *)
  item_parent_size : float option size;
      (* The parent size the items are measured with: the container's inner
         cross size, and no main size, as a percentage size in an axis must
         not contribute to a min-content contribution in that axis *)
  n : container_sizes; (* The container's sizes *)
  mutable min_content_space : Available_space.t size;
      (* The available space the last item was measured in for its automatic
         minimum, shared by the next items measured in the same space *)
}

(* Helper functions *)

(* Whether [a] and [b] are the same float, bit for bit *)
let same_float (a : float) b =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)
[@@inline]

(* [x] as an option, [None] for NaN *)
let option x = if Float.is_nan x then None else Some x [@@inline]

let is_scroll_container (item : flex_item) : bool =
  let overflow = Style.overflow item.style in
  Overflow.is_container overflow.x || Overflow.is_container overflow.y

(* Whether a margin of [item] is auto. *)
let margin_is_auto (item : flex_item) side =
  Length_percentage_auto.is_auto (side (Style.margin item.style))
[@@inline]

let top (rect : _ rect) = rect.top [@@inline]
let bottom (rect : _ rect) = rect.bottom [@@inline]
let left (rect : _ rect) = rect.left [@@inline]
let right (rect : _ rect) = rect.right [@@inline]

(* Whether [item] takes part in baseline alignment: it is aligned to the
   baseline and neither of its cross-axis margins is auto (CSS Box Alignment
   3, 9.3). *)
let participates_in_baseline (item : flex_item) ~is_row =
  item.align_self = Style.Align_items.Baseline
  && (not (margin_is_auto item (if is_row then top else left)))
  && not (margin_is_auto item (if is_row then bottom else right))

(* [d] resolved against [context] as [Dimension.maybe_resolve] resolves it,
   NaN where that gives [None]. *)
let resolve_dimension (d : Dimension.t) (context : float option) calc =
  if Dimension.is_auto d then Float.nan
  else if Dimension.is_length d then Dimension.value d
  else if Dimension.is_percent d then
    match context with
    | None -> Float.nan
    | Some dim ->
        (* Emulate f32 arithmetic to match Taffy's behavior *)
        Int32.float_of_bits (Int32.bits_of_float (dim *. Dimension.value d))
  else if Dimension.is_calc d then
    match context with
    | None -> Float.nan
    | Some dim -> calc (Compact_length.get_calc_index d) dim
  else
    failwith
      "Dimension.maybe_resolve: unsupported tag (expected \
       length/percent/auto/calc)"
[@@inline]

(* The width ([~width:true]) or height of a container's [dims] resolved
   against [parent], with a missing axis transferred through [aspect_ratio],
   plus [adjustment] ([Size.apply_aspect_ratio], [Sizef.maybe_add]); NaN
   where unset. *)
let resolve_container_size (dims : Dimension.t size) ~width
    (parent : float option size) aspect_ratio ~adjustment calc =
  let w = resolve_dimension dims.width parent.width calc in
  let h = resolve_dimension dims.height parent.height calc in
  (if width then
     match aspect_ratio with
     | Some ratio when Float.is_nan w && not (Float.is_nan h) -> h *. ratio
     | _ -> w
   else
     match aspect_ratio with
     | Some ratio when Float.is_nan h && not (Float.is_nan w) -> w /. ratio
     | _ -> h)
  +. adjustment
[@@inline]

(* [lpa] resolved against [context] as [Length_percentage_auto.maybe_resolve]
   resolves it, NaN where that gives [None]. *)
let resolve_inset (lpa : Length_percentage_auto.t) (context : float option) calc
    =
  if Length_percentage_auto.is_auto lpa then Float.nan
  else
    match Length_percentage_auto.maybe_resolve lpa context calc with
    | Some v -> v
    | None -> Float.nan

(* Compute algorithm constants from style *)
(* A space no item is measured in: its axes are blocks of their own. *)
let no_space : Available_space.t size =
  {
    width = Available_space.Definite Float.nan;
    height = Available_space.Definite Float.nan;
  }

let compute_constants (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (style : Style.t) (known_dimensions : float option size)
    (known_dimensions_are_definite : bool size)
    (available_space : Available_space.t size) (parent_size : float option size)
    : algo_constants =
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

  (* Handle overflow and scrollbar gutters *)
  let overflow = Style.overflow style in
  let scrollbar_gutter_x =
    if overflow.y = Overflow.Scroll then Style.scrollbar_width style else 0.0
  in
  let scrollbar_gutter_y =
    if overflow.x = Overflow.Scroll then Style.scrollbar_width style else 0.0
  in

  (* Content box inset = padding + border + scrollbar gutter *)
  let content_box_inset =
    if
      padding == Rectf.zero && border == Rectf.zero && scrollbar_gutter_x = 0.0
      && scrollbar_gutter_y = 0.0
    then Rectf.zero
    else
      Rectf.
        {
          left = padding.left +. border.left +. 0.0;
          right = padding.right +. border.right +. scrollbar_gutter_x;
          top = padding.top +. border.top +. 0.0;
          bottom = padding.bottom +. border.bottom +. scrollbar_gutter_y;
        }
  in

  let node_outer_size = known_dimensions in
  let node_inner_size =
    match node_outer_size with
    | { width = None; height = None } -> node_outer_size
    | _ when content_box_inset == Rectf.zero ->
        (* A size less a zero inset is the size, bit for bit *)
        node_outer_size
    | { width; height } ->
        let inner size inset =
          match size with Some size -> Some (size -. inset) | None -> None
        in
        {
          width = inner width (content_box_inset.left +. content_box_inset.right);
          height =
            inner height (content_box_inset.top +. content_box_inset.bottom);
        }
  in

  (* Resolve gap *)
  let gap = Style.gap style in
  (* Each gap resolves against the inner size in its own axis, zero where
     that is unknown *)
  let gap_base_width = Option.value node_inner_size.width ~default:0.0 in
  let gap_base_height = Option.value node_inner_size.height ~default:0.0 in

  (* Resolve min/max sizes, adjusted by the padding and border for a content
     box ([Rectf.sum_axes (Rectf.add padding border)], summed in its order) *)
  let content_box = Style.box_sizing style = Box_sizing.Content_box in
  let adjustment_width =
    if content_box then
      padding.left +. border.left +. (padding.right +. border.right)
    else 0.0
  in
  let adjustment_height =
    if content_box then
      padding.top +. border.top +. (padding.bottom +. border.bottom)
    else 0.0
  in
  let min_size = Style.min_size style and max_size = Style.max_size style in
  let min_width =
    resolve_container_size min_size ~width:true parent_size aspect_ratio
      ~adjustment:adjustment_width calc
  and min_height =
    resolve_container_size min_size ~width:false parent_size aspect_ratio
      ~adjustment:adjustment_height calc
  and max_width =
    resolve_container_size max_size ~width:true parent_size aspect_ratio
      ~adjustment:adjustment_width calc
  and max_height =
    resolve_container_size max_size ~width:false parent_size aspect_ratio
      ~adjustment:adjustment_height calc
  in
  (* A cross size that only a minimum no smaller than the maximum gives, the
     style size being auto, is not definite, as an auto size clamped is not:
     an item stretched to it transfers nothing through its aspect ratio, as
     in Chromium *)
  let cross_size_from_min_max =
    let min = if is_row then min_height else min_width
    and max = if is_row then max_height else max_width in
    (not (Float.is_nan min))
    && (not (Float.is_nan max))
    && max <= min
    && Dimension.is_auto
         (if is_row then (Style.size style).height else (Style.size style).width)
  in

  {
    dir;
    is_row;
    is_wrap;
    is_wrap_reverse;
    border;
    content_box_inset;
    align_items =
      Option.value (Style.align_items style) ~default:Align_items.Stretch;
    align_content =
      Option.value (Style.align_content style) ~default:Align_content.Stretch;
    justify_content = Style.justify_content style;
    node_outer_size;
    node_inner_size;
    initial_node_inner_size = node_inner_size;
    known_main_size_is_definite =
      (if is_row then known_dimensions_are_definite.width
       else known_dimensions_are_definite.height);
    known_cross_size_is_definite =
      (if is_row then known_dimensions_are_definite.height
       else known_dimensions_are_definite.width);
    has_definite_main_size =
      (if is_row then
         known_dimensions_are_definite.width
         && Option.is_some known_dimensions.width
       else
         known_dimensions_are_definite.height
         && Option.is_some known_dimensions.height);
    has_definite_cross_size =
      ((not cross_size_from_min_max)
      &&
      if is_row then
        known_dimensions_are_definite.height
        && Option.is_some known_dimensions.height
      else
        known_dimensions_are_definite.width
        && Option.is_some known_dimensions.width);
    cross_axis_available_space_is_definite =
      (if is_row then
         known_dimensions_are_definite.height
         && Option.is_some known_dimensions.height
         || Available_space.is_definite available_space.height
       else
         known_dimensions_are_definite.width
         && Option.is_some known_dimensions.width
         || Available_space.is_definite available_space.width);
    item_parent_size =
      (if is_row then { width = None; height = node_inner_size.height }
       else { width = node_inner_size.width; height = None });
    n =
      {
        width = 0.0;
        height = 0.0;
        inner_width = 0.0;
        inner_height = 0.0;
        gap_width =
          Length_percentage.resolve_or_zero gap.width (Some gap_base_width) calc;
        gap_height =
          Length_percentage.resolve_or_zero gap.height (Some gap_base_height)
            calc;
        scrollbar_gutter_x;
        scrollbar_gutter_y;
        min_width;
        min_height;
        max_width;
        max_height;
      };
    min_content_space = no_space;
  }

(* Which of an item's style sizes [set_style_size] sets *)
type style_size = Preferred | Minimum | Maximum

(* Sets the [kind] size of [sizes] to [dims] resolved against the container's
   inner size, with a missing axis transferred through [aspect_ratio], plus
   the box sizing adjustment ([Size.apply_aspect_ratio], [Sizef.maybe_add]):
   the padding and border when [content_box]. *)
let set_style_size sizes kind (dims : Dimension.t size) aspect_ratio
    ~inner_width ~inner_height ~(padding : Rectf.t) ~(border : Rectf.t)
    ~content_box calc =
  let width = resolve_dimension dims.width inner_width calc in
  let height = resolve_dimension dims.height inner_height calc in
  let width, height =
    match aspect_ratio with
    | Some ratio when Float.is_nan width && not (Float.is_nan height) ->
        (height *. ratio, height)
    | Some ratio when Float.is_nan height && not (Float.is_nan width) ->
        (width, width /. ratio)
    | _ -> (width, height)
  in
  (* [Rectf.sum_axes (Rectf.add padding border)], summed in its order *)
  let width =
    width
    +.
    if content_box then
      padding.left +. border.left +. (padding.right +. border.right)
    else 0.0
  in
  let height =
    height
    +.
    if content_box then
      padding.top +. border.top +. (padding.bottom +. border.bottom)
    else 0.0
  in
  match kind with
  | Preferred ->
      sizes.size_width <- width;
      sizes.size_height <- height
  | Minimum ->
      sizes.min_width <- width;
      sizes.min_height <- height
  | Maximum ->
      sizes.max_width <- width;
      sizes.max_height <- height

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
  let inner_width = constants.node_inner_size.width in
  let inner_height = constants.node_inner_size.height in
  (* The items' percentage sizes resolve against the container's inner size
     where it is definite: a size its parent derived from the container's
     content is not *)
  let main_definite = constants.known_main_size_is_definite
  and cross_definite = constants.known_cross_size_is_definite in
  let size_basis_width =
    if if constants.is_row then main_definite else cross_definite then
      inner_width
    else None
  and size_basis_height =
    if if constants.is_row then cross_definite else main_definite then
      inner_height
    else None
  in
  for index = 0 to child_count - 1 do
    let child_node = Tree.get_child_id tree node index in
    let child_style = Tree.get_core_container_style tree child_node in
    if keep_child child_style then (
      let aspect_ratio = Style.aspect_ratio child_style in
      let padding =
        Style.padding child_style
        |> Length_percentage.resolve_rect_or_zero inner_width calc
      in
      let border =
        Style.border child_style
        |> Length_percentage.resolve_rect_or_zero inner_width calc
      in
      let content_box = Style.box_sizing child_style = Box_sizing.Content_box in
      let margin =
        Style.margin child_style
        |> Length_percentage_auto.resolve_rect_or_zero inner_width calc
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
          style = child_style;
          align_self;
          measure_known_dimensions = Size.none;
          measure_available_cross = Available_space.max_content;
          margin;
          padding;
          border;
          frozen = false;
          flex_basis_is_definite = false;
          s =
            {
              size_width = Float.nan;
              size_height = Float.nan;
              min_width = Float.nan;
              min_height = Float.nan;
              max_width = Float.nan;
              max_height = Float.nan;
              flex_basis = 0.0;
              resolved_minimum_main_size = 0.0;
              violation = 0.0;
              content_flex_fraction = 0.0;
              hypothetical_inner_width = 0.0;
              hypothetical_inner_height = 0.0;
              target_width = 0.0;
              target_height = 0.0;
              outer_target_width = 0.0;
              outer_target_height = 0.0;
              baseline = 0.0;
              offset_main = 0.0;
              offset_cross = 0.0;
            };
        }
      in
      set_style_size item.s Preferred (Style.size child_style) aspect_ratio
        ~inner_width:size_basis_width ~inner_height:size_basis_height ~padding
        ~border ~content_box calc;
      set_style_size item.s Minimum
        (Style.min_size child_style)
        aspect_ratio ~inner_width:size_basis_width
        ~inner_height:size_basis_height ~padding ~border ~content_box calc;
      set_style_size item.s Maximum
        (Style.max_size child_style)
        aspect_ratio ~inner_width:size_basis_width
        ~inner_height:size_basis_height ~padding ~border ~content_box calc;
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
          (node_width -. Rectf.horizontal_axis_sum constants.content_box_inset)
    | None ->
        Available_space.sub available_space.width
          (Rectf.horizontal_axis_sum constants.content_box_inset)
  in

  let height =
    match known_dimensions.height with
    | Some node_height ->
        Available_space.of_float
          (node_height -. Rectf.vertical_axis_sum constants.content_box_inset)
    | None ->
        Available_space.sub available_space.height
          (Rectf.vertical_axis_sum constants.content_box_inset)
  in

  Size.{ width; height }

(* Determine flex base size *)
(* The used cross size of an item is its style size clamped by the item's own
   min/max constraints. A known cross size taken from the unclamped style size
   would measure content (e.g. wrapping text) at a width the final layout never
   gives the item, so content-based main sizes would disagree with the painted
   layout. [cross] is NaN when unknown, and stays NaN. *)
let clamp_cross_size (item : flex_item) ~(is_row : bool) cross =
  if Float.is_nan cross then cross
  else
    let hi = if is_row then item.s.max_height else item.s.max_width in
    let lo = if is_row then item.s.min_height else item.s.min_width in
    let cross = if Float.is_nan hi then cross else Float.min cross hi in
    if Float.is_nan lo then cross else Float.max cross lo
[@@inline]

(* The known dimensions of an item with cross size [cross], NaN when unknown,
   and an unknown main size. *)
let cross_known_dimensions ~is_row cross : float option size =
  if Float.is_nan cross then Size.none
  else if is_row then { width = None; height = Some cross }
  else { width = Some cross; height = None }

(* Sets [child]'s measurement constraints for the current layout. Where they
   are those of the item before it, [previous], they share its values. *)
let set_measure_constraints (constants : algo_constants)
    (available_space : Available_space.t size) ~(previous : flex_item)
    (child : flex_item) =
  let is_row = constants.is_row in
  let cross_axis_parent_size =
    if is_row then constants.node_inner_size.height
    else constants.node_inner_size.width
  in
  let cross_axis_margin_sum =
    if is_row then child.margin.top +. child.margin.bottom
    else child.margin.left +. child.margin.right
  in
  let child_min_cross =
    (if is_row then child.s.min_height else child.s.min_width)
    +. cross_axis_margin_sum
  in
  let child_max_cross =
    (if is_row then child.s.max_height else child.s.max_width)
    +. cross_axis_margin_sum
  in

  (* Clamp available space by min- and max- size *)
  let cross_axis_available_space =
    match if is_row then available_space.height else available_space.width with
    | Available_space.Definite val_ -> (
        let v =
          match cross_axis_parent_size with Some v -> v | None -> val_
        in
        let clamped =
          match
            (Float.is_nan child_min_cross, Float.is_nan child_max_cross)
          with
          | false, false ->
              Float.max child_min_cross (Float.min v child_max_cross)
          | false, true -> Float.max child_min_cross v
          | true, false -> Float.min v child_max_cross
          | true, true -> v
        in
        match previous.measure_available_cross with
        | Available_space.Definite v when same_float v clamped ->
            previous.measure_available_cross
        | _ -> Available_space.of_float clamped)
    | Available_space.Min_content ->
        if Float.is_nan child_min_cross then Available_space.min_content
        else Available_space.of_float child_min_cross
    | Available_space.Max_content ->
        if Float.is_nan child_max_cross then Available_space.max_content
        else Available_space.of_float child_max_cross
  in

  (* Known dimensions for child sizing: the style's cross size, or the
     stretched one *)
  let cross =
    let size = if is_row then child.s.size_height else child.s.size_width in
    if Float.is_nan size && child.align_self = Stretch then
      match cross_axis_available_space with
      | Available_space.Definite v -> v -. cross_axis_margin_sum
      | _ -> Float.nan
    else size
  in
  let cross = clamp_cross_size child ~is_row cross in
  child.measure_known_dimensions <-
    (match
       if is_row then previous.measure_known_dimensions.height
       else previous.measure_known_dimensions.width
     with
    | Some previous_cross when same_float previous_cross cross ->
        previous.measure_known_dimensions
    | _ -> cross_known_dimensions ~is_row cross);
  child.measure_available_cross <- cross_axis_available_space

(* The hypothetical outer sizes of [child]: its hypothetical sizes plus its
   margins *)
let hypothetical_outer_main_size (constants : algo_constants) child =
  if constants.is_row then
    child.s.hypothetical_inner_width +. (child.margin.left +. child.margin.right)
  else
    child.s.hypothetical_inner_height
    +. (child.margin.top +. child.margin.bottom)
[@@inline]

let hypothetical_outer_cross_size (constants : algo_constants) child =
  if constants.is_row then
    child.s.hypothetical_inner_height
    +. (child.margin.top +. child.margin.bottom)
  else
    child.s.hypothetical_inner_width +. (child.margin.left +. child.margin.right)
[@@inline]

(* The sum of [child]'s margins in the main axis *)
let main_margin_sum ~is_row child =
  if is_row then child.margin.left +. child.margin.right
  else child.margin.top +. child.margin.bottom
[@@inline]

let main_padding_border (constants : algo_constants) (child : flex_item) =
  if constants.is_row then
    child.padding.left +. child.padding.right +. child.border.left
    +. child.border.right
  else
    child.padding.top +. child.padding.bottom +. child.border.top
    +. child.border.bottom
[@@inline]

(* An item's automatic minimum main size (CSS Flexbox 4.5) is its min-content
   main size, capped by its specified main size and its maximum main size, and
   floored by its main-axis padding and border. Measuring the min-content size
   lays the item out under a min-content constraint, so it is measured only
   when a step of the algorithm depends on it ([resolve_minimum_main_size]).
   Until then the item's [resolved_minimum_main_size] is NaN.

   The cap does not depend on the content. An automatic minimum is therefore
   at most [automatic_minimum_bound], the smaller of the specified main size
   and the maximum, floored by padding and border, or infinity when the style
   sets neither. A step that compares the minimum with a size at least this
   bound gets the same result whatever the minimum, and does not measure
   it. *)
let automatic_minimum_bound (constants : algo_constants) (child : flex_item) =
  let size =
    if constants.is_row then child.s.size_width else child.s.size_height
  in
  let max =
    if constants.is_row then child.s.max_width else child.s.max_height
  in
  let cap =
    match (Float.is_nan size, Float.is_nan max) with
    | false, false -> fmin size max
    | false, true -> size
    | true, false -> max
    | true, true -> Float.infinity
  in
  fmax cap (main_padding_border constants child)
[@@inline]

(* Measures [child]'s automatic minimum and keeps it as its resolved minimum
   main size. *)
let measure_automatic_minimum (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (constants : algo_constants) (child : flex_item) =
  (* Compute min-content size *)
  let child_available_space =
    let space = constants.min_content_space in
    if
      (if constants.is_row then space.height else space.width)
      == child.measure_available_cross
    then space
    else (
      constants.min_content_space <-
        (if constants.is_row then
           Size.
             {
               width = Available_space.min_content;
               height = child.measure_available_cross;
             }
         else
           Size.
             {
               width = child.measure_available_cross;
               height = Available_space.min_content;
             });
      constants.min_content_space)
  in
  let layout_output =
    Tree.compute_child_layout tree child.node
      (Layout_input.make ~run_mode:Run_mode.Compute_size
         ~sizing_mode:Sizing_mode.Content_size
         ~axis:
           (if constants.is_row then Requested_axis.Horizontal
            else Requested_axis.Vertical)
         ~known_dimensions:child.measure_known_dimensions
         ~parent_size:constants.item_parent_size
         ~available_space:child_available_space
         ~vertical_margins_are_collapsible:Line.both_false)
  in

  let min_content_main_size =
    let measured_size = Layout_output.size layout_output in
    if constants.is_row then measured_size.width else measured_size.height
  in

  (* Apply automatic minimum size clamping *)
  let size =
    if constants.is_row then child.s.size_width else child.s.size_height
  in
  let max =
    if constants.is_row then child.s.max_width else child.s.max_height
  in
  let clamped_min =
    let v = min_content_main_size in
    let v = if Float.is_nan size then v else Float.min v size in
    if Float.is_nan max then v else Float.min v max
  in
  child.s.resolved_minimum_main_size <-
    Float.max clamped_min (main_padding_border constants child)

(* Measures [child]'s automatic minimum and keeps it as its resolved minimum
   main size, if it does not have one yet. *)
let resolve_minimum_main_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (constants : algo_constants) (child : flex_item) =
  if Float.is_nan child.s.resolved_minimum_main_size then
    measure_automatic_minimum (module Tree) tree constants child

let is_stretched child (constants : algo_constants) =
  child.align_self = Style.Align_items.Stretch
  && (not (margin_is_auto child (if constants.is_row then top else left)))
  && (not (margin_is_auto child (if constants.is_row then bottom else right)))
  && Style.Dimension.is_auto
       (let size = Style.size child.style in
        if constants.is_row then size.height else size.width)

(* Which of the known dimensions [child] is laid out at are definite (CSS
   Flexbox 9.8): its main size where the container's main size is definite or
   its flex basis is; its cross size where it is stretched, where its style
   sets it, or, for the width of an item in a column, where the space it is
   fit into is definite. *)
let both_definite : bool size = { width = true; height = true }
let width_definite : bool size = { width = true; height = false }
let height_definite : bool size = { width = false; height = true }
let none_definite : bool size = { width = false; height = false }

let item_definiteness (constants : algo_constants) (child : flex_item) ~leaf =
  (* A leaf has no children to resolve percentages against its size, and
     keeps one cache entry for its size whichever way it is definite *)
  if leaf then both_definite
  else
    let main =
      constants.has_definite_main_size || child.flex_basis_is_definite
    in
    let cross =
      is_stretched child constants
      || (not
            (Float.is_nan
               (if constants.is_row then child.s.size_height
                else child.s.size_width)))
      || (not constants.is_row)
         && constants.cross_axis_available_space_is_definite
    in
    match (constants.is_row, main, cross) with
    | _, true, true -> both_definite
    | true, true, false | false, false, true -> width_definite
    | true, false, true | false, true, false -> height_definite
    | _, false, false -> none_definite

(* The input for laying out an item of a flexbox *)
let item_input ~run_mode ~axis ~known_dimensions ~known_dimensions_are_definite
    ~parent_size ~available_space : Layout_input.t =
  {
    run_mode;
    sizing_mode = Sizing_mode.Content_size;
    axis;
    known_dimensions;
    known_dimensions_are_definite;
    parent_size;
    available_space;
    vertical_margins_are_collapsible = Line.both_false;
  }

let determine_flex_base_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (constants : algo_constants) (available_space : Available_space.t size)
    (flex_items : flex_item array) : unit =
  let calc = Tree.resolve_calc_value tree in
  let is_row = constants.is_row in
  (* A percentage flex basis resolves against the container's inner main
     size where it is definite *)
  let container_width =
    if not constants.known_main_size_is_definite then None
    else if is_row then constants.node_inner_size.width
    else constants.node_inner_size.height
  in
  (* E. An item sized into the available space treats a value of content as
     max-content, unless the space is min-content. *)
  let main_available =
    match if is_row then available_space.width else available_space.height with
    | Available_space.Min_content -> Available_space.min_content
    | _ -> Available_space.max_content
  in

  (* The available space of the last item measured for its basis, shared by
     the next items measured in the same space *)
  let measure_space = ref no_space and measure_cross = ref no_space.width in
  for i = 0 to Array.length flex_items - 1 do
    let child = flex_items.(i) in
    let child_style = child.style in
    set_measure_constraints constants available_space
      ~previous:flex_items.(if i = 0 then 0 else i - 1)
      child;
    let padding_border_sum = main_padding_border constants child in

    (* Resolve flex basis *)
    let flex_basis_style =
      resolve_dimension (Style.flex_basis child_style) container_width calc
      +.
      if Style.box_sizing child_style = Box_sizing.Content_box then
        padding_border_sum
      else 0.0
    in

    (* Compute flex basis according to spec *)
    let main_size =
      if is_row then child.s.size_width else child.s.size_height
    in
    let flex_basis =
      (* A. If the item has a definite used flex basis, that's the flex base
         size. *)
      if not (Float.is_nan flex_basis_style) then (
        child.flex_basis_is_definite <- true;
        flex_basis_style)
      else if not (Float.is_nan main_size) then (
        child.flex_basis_is_definite <- true;
        main_size)
      else
        (* B. With an aspect ratio and a definite cross size, the flex base
           size is that cross size through the ratio, and is as definite *)
        match
          ( Style.aspect_ratio child_style,
            if is_row then child.measure_known_dimensions.height
            else child.measure_known_dimensions.width )
        with
        | Some ratio, Some cross
          when (not
                  (Float.is_nan
                     (if is_row then child.s.size_height else child.s.size_width)))
               || (not constants.is_wrap) && constants.has_definite_cross_size
                  && is_stretched child constants ->
            child.flex_basis_is_definite <- true;
            if is_row then cross *. ratio else cross /. ratio
        | _ ->
            (* E. Otherwise, size the item into the available space *)
            let child_available_space =
              if child.measure_available_cross == !measure_cross then
                !measure_space
              else (
                measure_cross := child.measure_available_cross;
                (measure_space :=
                   if is_row then
                     Size.
                       {
                         width = main_available;
                         height = child.measure_available_cross;
                       }
                   else
                     Size.
                       {
                         width = child.measure_available_cross;
                         height = main_available;
                       });
                !measure_space)
            in
            let layout_output =
              Tree.compute_child_layout tree child.node
                (Layout_input.make ~run_mode:Run_mode.Compute_size
                   ~sizing_mode:Sizing_mode.Content_size
                   ~axis:
                     (if is_row then Requested_axis.Horizontal
                      else Requested_axis.Vertical)
                   ~known_dimensions:child.measure_known_dimensions
                   ~parent_size:constants.item_parent_size
                   ~available_space:child_available_space
                   ~vertical_margins_are_collapsible:Line.both_false)
            in
            let measured_size = Layout_output.size layout_output in
            if is_row then measured_size.width else measured_size.height
    in

    (* Floor flex-basis by the padding_border_sum *)
    let flex_basis = Float.max flex_basis padding_border_sum in
    child.s.flex_basis <- flex_basis;

    (* Compute resolved minimum main size: the style's minimum, or the
       automatic minimum of a scroll container (zero), or else the automatic
       minimum, measured when needed *)
    let min_from_style =
      if is_row then child.s.min_width else child.s.min_height
    in
    let overflow = Style.overflow child_style in
    child.s.resolved_minimum_main_size <-
      (if not (Float.is_nan min_from_style) then min_from_style
       else if Overflow.is_container (if is_row then overflow.x else overflow.y)
       then 0.0
       else Float.nan);

    (* Compute hypothetical sizes: the flex base size capped by the maximum,
       then floored by the minimum (CSS Flexbox 9.2 step 3, Taffy's
       [flex_basis.maybe_clamp(min, max)]), so a minimum above the maximum
       wins. A minimum no larger than the capped size leaves it as it is, so
       an automatic minimum is measured only for a capped size below its
       bound. *)
    let max_main = if is_row then child.s.max_width else child.s.max_height in
    let capped =
      if Float.is_nan max_main then flex_basis
      else Float.min flex_basis max_main
    in
    if
      Float.is_nan child.s.resolved_minimum_main_size
      && automatic_minimum_bound constants child > capped
    then measure_automatic_minimum (module Tree) tree constants child;
    let hypothetical_inner_min_main =
      let minimum = child.s.resolved_minimum_main_size in
      if Float.is_nan minimum then padding_border_sum
      else Float.max minimum padding_border_sum
    in
    let hypothetical_inner_size =
      Float.max capped hypothetical_inner_min_main
    in
    if is_row then child.s.hypothetical_inner_width <- hypothetical_inner_size
    else child.s.hypothetical_inner_height <- hypothetical_inner_size
  done

(* Collect flex items into flex lines *)
let collect_flex_lines (constants : algo_constants)
    (available_space : Available_space.t size) (flex_items : flex_item array) :
    flex_line list =
  let max_main =
    if constants.is_row then constants.n.max_width else constants.n.max_height
  in
  if
    (not constants.is_wrap)
    || ((not constants.known_main_size_is_definite) && Float.is_nan max_main)
  then
    (* Single line - all items go in one line. Wrapping needs a definite
       main size, or a maximum: a main size derived from the container's
       content puts the items on the one line the content was measured on. *)
    [ { items = flex_items; cross_size = 0.0; offset_cross = 0.0 } ]
  else
    (* Multi-line wrapping *)
    let main_axis_available_space =
      let max_main =
        if constants.is_row then option constants.n.max_width
        else option constants.n.max_height
      in
      match max_main with
      | Some max_size ->
          let available_main =
            if constants.is_row then available_space.width
            else available_space.height
          in
          let resolved =
            let v =
              match available_main with
              | Available_space.Definite v
                when constants.known_main_size_is_definite ->
                  v
              | _ -> max_size
            in
            let min_main =
              if constants.is_row then option constants.n.min_width
              else option constants.n.min_height
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
        [ { items = flex_items; cross_size = 0.0; offset_cross = 0.0 } ]
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
          if constants.is_row then constants.n.gap_width
          else constants.n.gap_height
        in
        let count = Array.length flex_items in
        (* Process flex items into lines: from the first uncollected item,
           collect items until the next would not fit, and at least one *)
        let lines = ref [] and start = ref 0 in
        while !start < count do
          let line_length = ref 0.0
          and split = ref !start
          and fits = ref true in
          while !fits && !split < count do
            let item = flex_items.(!split) in
            let gap_contribution =
              if !split = !start then 0.0 else main_axis_gap
            in
            let item_size = hypothetical_outer_main_size constants item in
            let new_length = !line_length +. item_size +. gap_contribution in
            if new_length > main_axis_available_space && !split <> !start then
              fits := false
            else (
              line_length := new_length;
              incr split)
          done;
          let split = if !split = !start then !start + 1 else !split in
          lines :=
            {
              items = Array.sub flex_items !start (split - !start);
              cross_size = 0.0;
              offset_cross = 0.0;
            }
            :: !lines;
          start := split
        done;
        List.rev !lines

(* Helper to sum axis gaps *)
let[@inline] sum_axis_gaps gap num_items =
  (* Gaps only exist between items *)
  if num_items <= 1 then 0.0 else gap *. float_of_int (num_items - 1)

(* Determine container main size *)

(* The length of [line] made of its items' flex base sizes, floored by their
   minimum sizes, with their margins and the gaps. *)
let line_base_length (constants : algo_constants) (line : flex_line) =
  let is_row = constants.is_row in
  let total = ref 0.0 in
  for i = 0 to Array.length line.items - 1 do
    let child = line.items.(i) in
    let padding_border_sum = main_padding_border constants child in
    let min_main = if is_row then child.s.min_width else child.s.min_height in
    let max_main = if is_row then child.s.max_width else child.s.max_height in
    (* The hypothetical main size: the flex base size clamped by the item's
       minimum and maximum, the minimum winning *)
    let base_size =
      if Float.is_nan max_main then child.s.flex_basis
      else Float.min child.s.flex_basis max_main
    in
    let base_size =
      if Float.is_nan min_main then base_size else Float.max base_size min_main
    in
    let margin_sum =
      if is_row then child.margin.left +. child.margin.right
      else child.margin.top +. child.margin.bottom
    in
    total := !total +. Float.max base_size padding_border_sum +. margin_sum
  done;
  !total
  +. sum_axis_gaps
       (if is_row then constants.n.gap_width else constants.n.gap_height)
       (Array.length line.items)

(* The main size of [child]'s content contribution (CSS Flexbox 9.9.1) when
   its container is sized under [available_space]'s min-content or
   max-content main constraint. *)
let content_contribution (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (available_space : Available_space.t size) (constants : algo_constants)
    (flex_item : flex_item) =
  let is_row = constants.is_row in
  let style_min =
    if is_row then flex_item.s.min_width else flex_item.s.min_height
  in
  let style_preferred =
    if is_row then flex_item.s.size_width else flex_item.s.size_height
  in
  let style_max =
    if is_row then flex_item.s.max_width else flex_item.s.max_height
  in
  let flex_basis = flex_item.s.flex_basis in

  (* An item that cannot grow (shrink) contributes at most (least) its flex
     base size (CSS Flexbox 9.9.3), in both directions, as Chromium does: a
     row item with flex-basis: 20px and width: 50px that cannot grow
     contributes 20px. Taffy clamps a row item by the larger of its flex base
     size and its width instead. *)
  let flex_basis_min =
    if Style.flex_shrink flex_item.style = 0.0 then flex_basis else Float.nan
  in
  let flex_basis_max =
    if Style.flex_grow flex_item.style = 0.0 then flex_basis else Float.nan
  in

  let min_main_size =
    resolve_minimum_main_size (module Tree) tree constants flex_item;
    let resolved_minimum_main_size = flex_item.s.resolved_minimum_main_size in
    let base_min =
      match (Float.is_nan style_min, Float.is_nan flex_basis_min) with
      | false, false -> Float.max style_min flex_basis_min
      | false, true -> style_min
      | true, false -> flex_basis_min
      | true, true -> Float.nan
    in
    if Float.is_nan base_min then resolved_minimum_main_size
    else Float.max base_min resolved_minimum_main_size
  in
  let max_main_size =
    match (Float.is_nan style_max, Float.is_nan flex_basis_max) with
    | false, false -> Float.min style_max flex_basis_max
    | false, true -> style_max
    | true, false -> flex_basis_max
    | true, true -> Float.infinity
  in
  (* The style's minimum and maximum clamp a contribution *)
  let clamp size =
    match (Float.is_nan style_min, Float.is_nan style_max) with
    | false, false -> Float.max style_min (Float.min size style_max)
    | false, true -> Float.max style_min size
    | true, false -> Float.min size style_max
    | true, true -> size
  in

  let margin_sum =
    if is_row then flex_item.margin.left +. flex_item.margin.right
    else flex_item.margin.top +. flex_item.margin.bottom
  in

  if
    (not (Float.is_nan style_preferred))
    && (max_main_size <= min_main_size || max_main_size <= style_preferred)
  then
    (* If clamping values are such that max <= min, avoid computing content
       size *)
    Float.max (Float.min style_preferred max_main_size) min_main_size
    +. margin_sum
  else if max_main_size <= min_main_size then min_main_size +. margin_sum
  else if is_scroll_container flex_item then
    (* Scroll containers use their flex basis but still honour min/max *)
    let content_main_size = flex_basis +. margin_sum in
    if is_row then clamp content_main_size
    else clamp (Float.max content_main_size flex_basis)
  else if not (Float.is_nan style_preferred) then
    (* A definite preferred main size is the item's content contribution (an
       inherent-size measure would return it), floored by its main-axis
       padding and border, so the item is not measured; min and max clamp it
       as they clamp a measured contribution. *)
    let content_main_size =
      Float.max style_preferred (main_padding_border constants flex_item)
      +. margin_sum
    in
    if is_row then clamp content_main_size
    else clamp (Float.max content_main_size flex_basis)
  else
    (* Need to compute content size *)
    let cross_axis_parent_size =
      if is_row then constants.node_inner_size.height
      else constants.node_inner_size.width
    in
    let cross_axis_margin_sum =
      if is_row then flex_item.margin.top +. flex_item.margin.bottom
      else flex_item.margin.left +. flex_item.margin.right
    in
    let child_min_cross =
      (if is_row then flex_item.s.min_height else flex_item.s.min_width)
      +. cross_axis_margin_sum
    in
    let child_max_cross =
      (if is_row then flex_item.s.max_height else flex_item.s.max_width)
      +. cross_axis_margin_sum
    in
    let available_cross =
      if is_row then available_space.height else available_space.width
    in
    let cross_axis_available_space =
      match available_cross with
      | Available_space.Definite val_ ->
          let v =
            match cross_axis_parent_size with Some v -> v | None -> val_
          in
          Available_space.of_float
            (match
               (Float.is_nan child_min_cross, Float.is_nan child_max_cross)
             with
            | false, false ->
                Float.max child_min_cross (Float.min v child_max_cross)
            | false, true -> Float.max child_min_cross v
            | true, false -> Float.min v child_max_cross
            | true, true -> v)
      | _ -> available_cross
    in
    let child_available_space =
      if is_row then
        Size.
          { width = available_space.width; height = cross_axis_available_space }
      else
        Size.
          {
            width = cross_axis_available_space;
            height = available_space.height;
          }
    in

    (* Known dimensions for child sizing: the style's cross size, or the
       stretched one *)
    let known_cross =
      let size =
        if is_row then flex_item.s.size_height else flex_item.s.size_width
      in
      let cross =
        if Float.is_nan size && flex_item.align_self = Stretch then
          match cross_axis_available_space with
          | Available_space.Definite v -> v -. cross_axis_margin_sum
          | _ -> Float.nan
        else size
      in
      clamp_cross_size flex_item ~is_row cross
    in

    let layout_output =
      Tree.compute_child_layout tree flex_item.node
        (Layout_input.make ~run_mode:Run_mode.Compute_size
           ~sizing_mode:Sizing_mode.Content_size
           ~axis:
             (if is_row then Requested_axis.Horizontal
              else Requested_axis.Vertical)
           ~known_dimensions:(cross_known_dimensions ~is_row known_cross)
           ~parent_size:constants.node_inner_size
           ~available_space:child_available_space
           ~vertical_margins_are_collapsible:Line.both_false)
    in

    let content_main_size =
      let measured_size = Layout_output.size layout_output in
      let measured_main_size =
        if is_row then measured_size.width else measured_size.height
      in
      (* A known cross size, transferred through the item's aspect ratio,
         floors the measured content size. *)
      (match Style.aspect_ratio flex_item.style with
        | Some ratio when not (Float.is_nan known_cross) ->
            Float.max measured_main_size
              (if is_row then known_cross *. ratio else known_cross /. ratio)
        | _ -> measured_main_size)
      +. margin_sum
    in

    (* Apply different clamping based on row vs column. The container's own
       padding and border do not floor an item's contribution: the container
       adds them once, to the sum of its items. *)
    if is_row then clamp content_main_size
    else clamp (Float.max content_main_size flex_basis)

let determine_container_main_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (available_space : Available_space.t size) (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  let is_row = constants.is_row in
  let main_content_box_inset =
    if is_row then Rectf.horizontal_axis_sum constants.content_box_inset
    else Rectf.vertical_axis_sum constants.content_box_inset
  in

  let outer_main_size =
    let current_main_size =
      if is_row then constants.node_outer_size.width
      else constants.node_outer_size.height
    in
    match current_main_size with
    | Some size -> size
    | None -> (
        let available_main =
          if is_row then available_space.width else available_space.height
        in
        match available_main with
        | Available_space.Definite main_axis_available_space ->
            (* Calculate longest line length *)
            let longest_line_length =
              List.fold_left
                (fun max_length line ->
                  Float.max max_length (line_base_length constants line))
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
                  Float.max max_length (line_base_length constants line))
                0.0 flex_lines
            in
            longest_line_length +. main_content_box_inset
        | Available_space.Min_content | Available_space.Max_content ->
            (* Content-based sizing *)
            let main_size = ref 0.0 in

            List.iter
              (fun line ->
                let items = line.items in
                (* Compute content flex fraction *)
                for i = 0 to Array.length items - 1 do
                  let flex_item = items.(i) in
                  let content_contribution =
                    content_contribution
                      (module Tree)
                      tree available_space constants flex_item
                  in
                  flex_item.s.content_flex_fraction <-
                    (let diff =
                       content_contribution -. flex_item.s.flex_basis
                     in
                     if diff > 0.0 then
                       diff /. Float.max 1.0 (Style.flex_grow flex_item.style)
                     else if diff < 0.0 then
                       let scaled_shrink_factor =
                         Float.max 1.0
                           (Style.flex_shrink flex_item.style
                           *. (flex_item.s.flex_basis
                              -. main_padding_border constants flex_item))
                       in
                       diff /. scaled_shrink_factor
                     else 0.0)
                done;

                (* Calculate line contribution *)
                let item_main_size_sum = ref 0.0 in
                for i = 0 to Array.length items - 1 do
                  let item = items.(i) in
                  let flex_fraction = item.s.content_flex_fraction in
                  let flex_contribution =
                    if flex_fraction > 0.0 then
                      Float.max 1.0 (Style.flex_grow item.style)
                      *. flex_fraction
                    else if flex_fraction < 0.0 then
                      let scaled_shrink_factor =
                        Float.max 1.0 (Style.flex_shrink item.style)
                        *. (item.s.flex_basis
                           -. main_padding_border constants item)
                      in
                      scaled_shrink_factor *. flex_fraction
                    else 0.0
                  in
                  let size = item.s.flex_basis +. flex_contribution in
                  if is_row then (
                    item.s.outer_target_width <- size;
                    item.s.target_width <- size)
                  else (
                    item.s.outer_target_height <- size;
                    item.s.target_height <- size);
                  item_main_size_sum := !item_main_size_sum +. size
                done;

                let gap_sum =
                  sum_axis_gaps
                    (if is_row then constants.n.gap_width
                     else constants.n.gap_height)
                    (Array.length items)
                in
                main_size :=
                  Float.max !main_size (!item_main_size_sum +. gap_sum))
              flex_lines;

            !main_size +. main_content_box_inset)
  in

  (* Apply size constraints *)
  let min_main =
    if is_row then option constants.n.min_width
    else option constants.n.min_height
  in
  let max_main =
    if is_row then option constants.n.max_width
    else option constants.n.max_height
  in
  let scrollbar_main =
    if is_row then constants.n.scrollbar_gutter_x
    else constants.n.scrollbar_gutter_y
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
  if is_row then (
    constants.n.width <- outer_main_size;
    constants.n.inner_width <- inner_main_size;
    constants.node_inner_size <-
      { constants.node_inner_size with width = Some inner_main_size })
  else (
    constants.n.height <- outer_main_size;
    constants.n.inner_height <- inner_main_size;
    constants.node_inner_size <-
      { constants.node_inner_size with height = Some inner_main_size })

(* Resolve flexible lengths *)
let resolve_flexible_lengths (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (line : flex_line) (constants : algo_constants) : unit =
  let is_row = constants.is_row in
  let items = line.items in
  let count = Array.length items in
  let total_main_axis_gap =
    sum_axis_gaps
      (if is_row then constants.n.gap_width else constants.n.gap_height)
      count
  in

  (* 1. Determine the used flex factor *)
  let total_hypothetical_outer_main_size = ref 0.0 in
  for i = 0 to count - 1 do
    let child = items.(i) in
    total_hypothetical_outer_main_size :=
      !total_hypothetical_outer_main_size
      +. hypothetical_outer_main_size constants child
  done;
  let used_flex_factor =
    total_main_axis_gap +. !total_hypothetical_outer_main_size
  in
  let inner_main_size =
    match
      if is_row then constants.node_inner_size.width
      else constants.node_inner_size.height
    with
    | Some size -> size
    | None -> 0.0
  in
  let growing = used_flex_factor < inner_main_size in
  let shrinking = used_flex_factor > inner_main_size in
  let exactly_sized = (not growing) && not shrinking in

  (* 2. Size inflexible items *)
  for i = 0 to count - 1 do
    let child = items.(i) in
    let inner_target_size =
      if is_row then child.s.hypothetical_inner_width
      else child.s.hypothetical_inner_height
    in
    if is_row then child.s.target_width <- inner_target_size
    else child.s.target_height <- inner_target_size;

    if
      exactly_sized
      || Style.flex_grow child.style = 0.0
         && Style.flex_shrink child.style = 0.0
      || (growing && child.s.flex_basis > inner_target_size)
      || (shrinking && child.s.flex_basis < inner_target_size)
    then (
      child.frozen <- true;
      let outer_target_size =
        inner_target_size +. main_margin_sum ~is_row child
      in
      if is_row then child.s.outer_target_width <- outer_target_size
      else child.s.outer_target_height <- outer_target_size)
  done;

  if not exactly_sized then
    (* 3. Calculate initial free space *)
    let used_space () =
      let sum = ref 0.0 in
      for i = 0 to count - 1 do
        let child = items.(i) in
        sum :=
          if child.frozen then
            !sum
            +.
            if is_row then child.s.outer_target_width
            else child.s.outer_target_height
          else !sum +. child.s.flex_basis +. main_margin_sum ~is_row child
      done;
      total_main_axis_gap +. !sum
    in

    let initial_free_space = inner_main_size -. used_space () in

    (* 4. Loop *)
    let all_frozen () =
      let frozen = ref true in
      for i = 0 to count - 1 do
        if not items.(i).frozen then frozen := false
      done;
      !frozen
    in
    (* a. Check for flexible items *)
    while not (all_frozen ()) do
      (* b. Calculate the remaining free space *)
      let used_space = used_space () in

      let sum_flex_grow = ref 0.0 and sum_flex_shrink = ref 0.0 in
      for i = 0 to count - 1 do
        let child = items.(i) in
        if not child.frozen then (
          sum_flex_grow := !sum_flex_grow +. Style.flex_grow child.style;
          sum_flex_shrink := !sum_flex_shrink +. Style.flex_shrink child.style)
      done;
      let sum_flex_grow = !sum_flex_grow
      and sum_flex_shrink = !sum_flex_shrink in

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
      if Float.is_finite free_space && free_space <> 0.0 then
        if growing && sum_flex_grow > 0.0 then
          for i = 0 to count - 1 do
            let child = items.(i) in
            if not child.frozen then
              let new_size =
                child.s.flex_basis
                +. (free_space *. (Style.flex_grow child.style /. sum_flex_grow))
              in
              if is_row then child.s.target_width <- new_size
              else child.s.target_height <- new_size
          done
        else if shrinking && sum_flex_shrink > 0.0 then (
          let sum_scaled_shrink_factor = ref 0.0 in
          for i = 0 to count - 1 do
            let child = items.(i) in
            if not child.frozen then
              sum_scaled_shrink_factor :=
                !sum_scaled_shrink_factor
                +. (child.s.flex_basis -. main_padding_border constants child)
                   *. Style.flex_shrink child.style
          done;
          let sum_scaled_shrink_factor = !sum_scaled_shrink_factor in
          if sum_scaled_shrink_factor > 0.0 then
            for i = 0 to count - 1 do
              let child = items.(i) in
              if not child.frozen then
                let scaled_shrink_factor =
                  (child.s.flex_basis -. main_padding_border constants child)
                  *. Style.flex_shrink child.style
                in
                let new_size =
                  child.s.flex_basis
                  +. free_space
                     *. (scaled_shrink_factor /. sum_scaled_shrink_factor)
                in
                if is_row then child.s.target_width <- new_size
                else child.s.target_height <- new_size
            done);

      (* d. Fix min/max violations *)
      let total_violation = ref 0.0 in
      for i = 0 to count - 1 do
        let child = items.(i) in
        if not child.frozen then (
          let current_size =
            if is_row then child.s.target_width else child.s.target_height
          in
          let max_main =
            if is_row then child.s.max_width else child.s.max_height
          in
          let capped =
            if Float.is_nan max_main then current_size
            else Float.min current_size max_main
          in
          (* A minimum no larger than the capped size leaves it as it is, so
             an automatic minimum is measured only for a size below its
             bound. *)
          (* The content box is floored at zero (CSS Flexbox 9.7, step d):
             the border box at the padding and border *)
          let floor = main_padding_border constants child in
          let clamped =
            if
              Float.is_nan child.s.resolved_minimum_main_size
              && capped >= automatic_minimum_bound constants child
            then Float.max floor capped
            else (
              resolve_minimum_main_size (module Tree) tree constants child;
              Float.max floor
                (Float.max child.s.resolved_minimum_main_size capped))
          in
          child.s.violation <- clamped -. current_size;
          if is_row then (
            child.s.target_width <- clamped;
            child.s.outer_target_width <-
              clamped +. child.margin.left +. child.margin.right)
          else (
            child.s.target_height <- clamped;
            child.s.outer_target_height <-
              clamped +. child.margin.top +. child.margin.bottom);
          total_violation := !total_violation +. child.s.violation)
      done;
      let total_violation = !total_violation in

      (* e. Freeze over-flexed items *)
      for i = 0 to count - 1 do
        let child = items.(i) in
        if not child.frozen then
          child.frozen <-
            (if total_violation > 0.0 then child.s.violation > 0.0
             else if total_violation < 0.0 then child.s.violation < 0.0
             else true)
      done
      (* f. Return to the start of this loop *)
    done

(* Whether the used cross size of [child] is the cross size of its flex line:
   it has an auto cross size and align-self: stretch, and neither of its
   cross-axis margins is auto. *)
(* Determine hypothetical cross size *)
let determine_hypothetical_cross_size (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (line : flex_line) (constants : algo_constants)
    (node_size : float option size) (available_space : Available_space.t size) :
    unit =
  let is_row = constants.is_row in
  (* If the flex container is single-line and has a definite cross size, that
     is the cross size of the line, whatever the hypothetical cross sizes of
     the items are. *)
  let line_cross_size_is_known =
    (not constants.is_wrap)
    && (if is_row then node_size.height else node_size.width) <> None
  in
  let child_known_main =
    if is_row then constants.n.width else constants.n.height
  in
  let available_cross =
    if is_row then available_space.height else available_space.width
  in
  for i = 0 to Array.length line.items - 1 do
    let child = line.items.(i) in
    (* An item stretched to a line whose cross size is already known is not
       measured: its hypothetical cross size would be used neither for the
       line nor for the item (see [calculate_cross_size] and
       [determine_used_cross_size]). A stretched item has align-self:
       stretch, so it takes no part in baseline alignment, the other reader
       of the hypothetical cross size. *)
    if not (line_cross_size_is_known && is_stretched child constants) then
      let padding_border_sum =
        if is_row then
          child.padding.top +. child.padding.bottom +. child.border.top
          +. child.border.bottom
        else
          child.padding.left +. child.padding.right +. child.border.left
          +. child.border.right
      in
      let min_cross =
        if is_row then child.s.min_height else child.s.min_width
      in
      let max_cross =
        if is_row then child.s.max_height else child.s.max_width
      in
      let clamp size =
        match (Float.is_nan min_cross, Float.is_nan max_cross) with
        | false, false -> Float.max min_cross (Float.min size max_cross)
        | false, true -> Float.max min_cross size
        | true, false -> Float.min size max_cross
        | true, true -> size
      in

      let child_inner_cross =
        let cross_size =
          if is_row then child.s.size_height else child.s.size_width
        in
        if not (Float.is_nan cross_size) then
          Float.max padding_border_sum (clamp cross_size)
        else
          (* The cross size the aspect ratio gives from the item's main size,
             floored by its padding and border, on the box its box-sizing
             names; NaN without a ratio *)
          let transferred =
            match Style.aspect_ratio child.style with
            | None -> Float.nan
            | Some ratio ->
                let main_pb =
                  if is_row then
                    child.padding.left +. child.padding.right
                    +. child.border.left +. child.border.right
                  else
                    child.padding.top +. child.padding.bottom
                    +. child.border.top +. child.border.bottom
                in
                let main =
                  Float.max main_pb
                    (if is_row then child.s.target_width
                     else child.s.target_height)
                in
                let content_box =
                  Style.box_sizing child.style = Box_sizing.Content_box
                in
                let main = if content_box then main -. main_pb else main in
                let cross = if is_row then main /. ratio else main *. ratio in
                if content_box then cross +. padding_border_sum else cross
          in
          if
            (not (Float.is_nan transferred))
            && ((not (Float.is_nan min_cross)) || is_scroll_container child)
          then
            (* Without an automatic minimum to floor it by its content *)
            Float.max padding_border_sum (clamp transferred)
          else
            let measured_size =
              (* Need to measure the child *)
              let child_available_cross =
                match available_cross with
                | Available_space.Definite value ->
                    Available_space.of_float
                      (Float.max padding_border_sum (clamp value))
                | _ -> available_cross
              in
              let child_known_dimensions =
                if is_row then
                  { Size.width = Some child.s.target_width; height = None }
                else { Size.width = None; height = Some child.s.target_height }
              in
              let child_available_space =
                if is_row then
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
                  (item_input ~run_mode:Run_mode.Compute_size
                     ~axis:
                       (if is_row then Requested_axis.Vertical
                        else Requested_axis.Horizontal)
                     ~known_dimensions:child_known_dimensions
                     ~known_dimensions_are_definite:
                       (item_definiteness constants child
                          ~leaf:(Tree.child_count tree child.node = 0))
                     ~parent_size:constants.node_inner_size
                     ~available_space:child_available_space)
              in
              let measured_size =
                let size = Layout_output.size layout_output in
                if is_row then size.height else size.width
              in
              measured_size
            in
            Float.max padding_border_sum
              (clamp
                 (if Float.is_nan transferred then measured_size
                  else Float.max transferred measured_size))
      in

      if is_row then child.s.hypothetical_inner_height <- child_inner_cross
      else child.s.hypothetical_inner_width <- child_inner_cross
  done

(* Calculate children baselines *)
let calculate_children_base_lines (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (known_dimensions : float option size)
    (available_space : Available_space.t size) (flex_lines : flex_line list)
    (constants : algo_constants) : unit =
  (* Only compute baselines for flex rows because we only support baseline
     alignment in the cross axis where that axis is also the inline axis *)
  if constants.is_row then
    let lines = ref flex_lines in
    while !lines != [] do
      let line = List.hd !lines in
      lines := List.tl !lines;
      let items = line.items in
      (* If a flex line has one or zero items participating in baseline
         alignment then baseline alignment is a no-op so we skip *)
      let line_baseline_child_count = ref 0 in
      for i = 0 to Array.length items - 1 do
        if participates_in_baseline items.(i) ~is_row:true then
          incr line_baseline_child_count
      done;
      if !line_baseline_child_count > 1 then
        for i = 0 to Array.length items - 1 do
          let child = items.(i) in
          (* Only calculate baselines for children participating in baseline
             alignment *)
          if participates_in_baseline child ~is_row:true then
            let measured_size_and_baselines =
              Tree.compute_child_layout tree child.node
                (item_input ~run_mode:Run_mode.Perform_layout
                   ~axis:Requested_axis.Both
                   ~known_dimensions:
                     {
                       Size.width = Some child.s.target_width;
                       height = Some child.s.hypothetical_inner_height;
                     }
                   ~known_dimensions_are_definite:
                     (item_definiteness constants child
                        ~leaf:(Tree.child_count tree child.node = 0))
                   ~parent_size:constants.node_inner_size
                   ~available_space:
                     {
                       Size.width = Available_space.of_float constants.n.width;
                       height =
                         Available_space.set_or_self available_space.height
                           known_dimensions.height;
                     })
            in

            let baseline =
              (Layout_output.first_baselines measured_size_and_baselines).y
            in
            let height =
              (Layout_output.size measured_size_and_baselines).height
            in

            child.s.baseline <-
              Option.value baseline ~default:height +. child.margin.top
        done
    done

(* The largest baseline of [line]'s items, at least zero. *)
let max_baseline line =
  let max = ref 0.0 in
  for i = 0 to Array.length line.items - 1 do
    max := Float.max !max line.items.(i).s.baseline
  done;
  !max

(* The sum of the cross sizes of [flex_lines] *)
let total_cross_size flex_lines =
  let total = ref 0.0 and lines = ref flex_lines in
  while !lines != [] do
    total := !total +. (List.hd !lines).cross_size;
    lines := List.tl !lines
  done;
  !total

(* Calculate cross size *)
let calculate_cross_size (flex_lines : flex_line list)
    (known_dimensions : float option size) (constants : algo_constants) : unit =
  (* If the flex container is single-line and has a definite cross size, the
     cross size of the flex line is the flex container's inner cross size. *)
  (if
     (not constants.is_wrap)
     && (if constants.is_row then known_dimensions.height
         else known_dimensions.width)
        <> None
   then
     let cross_axis_padding_border =
       if constants.is_row then
         Rectf.vertical_axis_sum constants.content_box_inset
       else Rectf.horizontal_axis_sum constants.content_box_inset
     in
     let cross_min_size =
       if constants.is_row then option constants.n.min_height
       else option constants.n.min_width
     in
     let cross_max_size =
       if constants.is_row then option constants.n.max_height
       else option constants.n.max_width
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
     let lines = ref flex_lines in
     while !lines != [] do
       let line = List.hd !lines in
       lines := List.tl !lines;
       (* Find the maximum baseline *)
       let max_baseline = max_baseline line in

       (* Calculate cross size based on items *)
       let cross_size = ref 0.0 in
       for i = 0 to Array.length line.items - 1 do
         let child = line.items.(i) in
         let child_cross_size =
           if participates_in_baseline child ~is_row:constants.is_row then
             (* Baseline aligned items with non-auto margins *)
             max_baseline -. child.s.baseline
             +. hypothetical_outer_cross_size constants child
           else (* Other items *)
             hypothetical_outer_cross_size constants child
         in
         cross_size := Float.max !cross_size child_cross_size
       done;
       line.cross_size <- !cross_size
     done);

  (* If single-line, clamp the line's cross-size to container's min/max *)
  if not constants.is_wrap then
    match flex_lines with
    | line :: _ ->
        let cross_axis_padding_border =
          if constants.is_row then
            Rectf.vertical_axis_sum constants.content_box_inset
          else Rectf.horizontal_axis_sum constants.content_box_inset
        in
        let cross_min_size =
          if constants.is_row then option constants.n.min_height
          else option constants.n.min_width
        in
        let cross_max_size =
          if constants.is_row then option constants.n.max_height
          else option constants.n.max_width
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
          Rectf.vertical_axis_sum constants.content_box_inset
        else Rectf.horizontal_axis_sum constants.content_box_inset
      in

      let cross_min_size =
        if constants.is_row then option constants.n.min_height
        else option constants.n.min_width
      in

      let cross_max_size =
        if constants.is_row then option constants.n.max_height
        else option constants.n.max_width
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
        if constants.is_row then constants.n.gap_height
        else constants.n.gap_width
      in
      let total_cross_axis_gap =
        if List.length flex_lines > 0 then
          cross_gap *. float_of_int (List.length flex_lines - 1)
        else 0.0
      in

      let lines_total_cross =
        total_cross_size flex_lines +. total_cross_axis_gap
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
  let is_row = constants.is_row in
  let calc = Tree.resolve_calc_value tree in
  let inner_width = constants.node_inner_size.width in
  let lines = ref flex_lines in
  while !lines != [] do
    let line = List.hd !lines in
    lines := List.tl !lines;
    let line_cross_size = line.cross_size in
    let items = line.items in
    for i = 0 to Array.length items - 1 do
      let child = items.(i) in
      let child_style = child.style in
      let cross_margin_sum =
        if is_row then child.margin.top +. child.margin.bottom
        else child.margin.left +. child.margin.right
      in

      let cross_target_size =
        if is_stretched child constants then
          (* Special case for stretch alignment *)
          (* For some reason this particular usage of max_width is an exception to the rule that max_width's transfer
           using the aspect_ratio (if set). Both Chrome and Firefox agree on this. And reading the spec, it seems like
           a reasonable interpretation. Although it seems to me that the spec *should* apply aspect_ratio here. *)
          let max_size =
            let max_size = Style.max_size child_style in
            let max_size =
              if is_row then
                resolve_dimension max_size.height
                  constants.node_inner_size.height calc
              else resolve_dimension max_size.width inner_width calc
            in
            if Style.box_sizing child_style = Style.Box_sizing.Content_box then
              (* The padding and border resolve against the container's
                 inner width as it is now. *)
              let padding =
                Style.padding child_style
                |> Length_percentage.resolve_rect_or_zero inner_width calc
              in
              let border =
                Style.border child_style
                |> Length_percentage.resolve_rect_or_zero inner_width calc
              in
              (* [Rectf.sum_axes (Rectf.add padding border)], summed in
                 its order *)
              max_size
              +.
              if is_row then
                padding.top +. border.top +. (padding.bottom +. border.bottom)
              else padding.left +. border.left +. (padding.right +. border.right)
            else max_size +. 0.0
          in

          let stretched_size = line_cross_size -. cross_margin_sum in
          let min_size =
            if is_row then child.s.min_height else child.s.min_width
          in

          match (Float.is_nan min_size, Float.is_nan max_size) with
          | false, false ->
              Float.max min_size (Float.min stretched_size max_size)
          | false, true -> Float.max min_size stretched_size
          | true, false -> Float.min stretched_size max_size
          | true, true -> stretched_size
        else if
          (* Use hypothetical size *)
          is_row
        then child.s.hypothetical_inner_height
        else child.s.hypothetical_inner_width
      in

      (* Set target size *)
      if is_row then (
        child.s.target_height <- cross_target_size;
        child.s.outer_target_height <- cross_target_size +. cross_margin_sum)
      else (
        child.s.target_width <- cross_target_size;
        child.s.outer_target_width <- cross_target_size +. cross_margin_sum)
    done
  done

(* Helper function to sum axis gaps *)
let[@inline] sum_axis_gaps (gap : float) (num_items : int) : float =
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

  (* 1. If there is only a single item being aligned or the items overflow,
     the distributed alignment keywords fall back to a positional one
     (https://www.w3.org/TR/css-align-3/#distribution-values): the space-*
     ones safely, so that step 2 turns them to Start when the items overflow,
     and stretch to plain flex-start, so that overflowing lines of a
     wrap-reverse container and items of a reversed direction pack to the
     flex-start edge. *)
  let alignment_mode, is_safe =
    if num_items <= 1 || free_space <= 0.0 then
      match alignment_mode with
      | Align_content.Stretch -> (Align_content.Flex_start, is_safe)
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
let[@inline] compute_alignment_offset (free_space : float) (num_items : int)
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
  let is_row = constants.is_row in
  let main_gap =
    if is_row then constants.n.gap_width else constants.n.gap_height
  in
  let lines = ref flex_lines in
  while !lines != [] do
    let line = List.hd !lines in
    lines := List.tl !lines;
    let items = line.items in
    let num_items = Array.length items in
    let total_main_axis_gap = sum_axis_gaps main_gap num_items in
    let used_space =
      let sum = ref 0.0 in
      for i = 0 to num_items - 1 do
        let child = items.(i) in
        sum :=
          !sum
          +.
          if is_row then child.s.outer_target_width
          else child.s.outer_target_height
      done;
      total_main_axis_gap +. !sum
    in
    let free_space =
      (if is_row then constants.n.inner_width else constants.n.inner_height)
      -. used_space
    in

    (* Count auto margins *)
    let num_auto_margins = ref 0 in
    for i = 0 to num_items - 1 do
      let child = items.(i) in
      if is_row then (
        if margin_is_auto child left then incr num_auto_margins;
        if margin_is_auto child right then incr num_auto_margins)
      else (
        if margin_is_auto child top then incr num_auto_margins;
        if margin_is_auto child bottom then incr num_auto_margins)
    done;

    (* Positive free space goes to the auto margins, which leave none to
       justify-content; the gaps still separate the items *)
    let free_space =
      if free_space > 0.0 && !num_auto_margins > 0 then (
        let margin = free_space /. float_of_int !num_auto_margins in
        for i = 0 to num_items - 1 do
          let child = items.(i) in
          if is_row then (
            if margin_is_auto child left then
              child.margin <- { child.margin with left = margin };
            if margin_is_auto child right then
              child.margin <- { child.margin with right = margin })
          else (
            if margin_is_auto child top then
              child.margin <- { child.margin with top = margin };
            if margin_is_auto child bottom then
              child.margin <- { child.margin with bottom = margin })
        done;
        0.0)
      else free_space
    in
    (* Apply justify-content alignment *)
    let layout_reverse = Flex_direction_ext.is_reverse constants.dir in
    let is_safe = false in
    (* TODO: Implement safe alignment *)
    let raw_justify_content_mode =
      match constants.justify_content with Some jc -> jc | None -> Flex_start
    in
    let justify_content_mode =
      apply_alignment_fallback free_space num_items raw_justify_content_mode
        is_safe
    in
    for i = 0 to num_items - 1 do
      (* In reverse, the first item to justify is the last one *)
      let child = items.(if layout_reverse then num_items - 1 - i else i) in
      child.s.offset_main <-
        compute_alignment_offset free_space num_items main_gap
          justify_content_mode layout_reverse (i = 0)
    done
  done

(* Helper function to align flex items along cross axis *)
let[@inline] align_flex_items_along_cross_axis (child : flex_item)
    (free_space : float) (max_baseline : float) (max_baseline_to_bottom : float)
    (constants : algo_constants) : float =
  match child.align_self with
  | Style.Align_items.Start -> 0.0
  | Style.Align_items.Flex_start ->
      if constants.is_wrap_reverse then free_space else 0.0
  | Style.Align_items.End -> free_space
  | Style.Align_items.Flex_end ->
      if constants.is_wrap_reverse then 0.0 else free_space
  | Style.Align_items.Center -> free_space /. 2.0
  | Style.Align_items.Baseline ->
      if constants.is_row && constants.is_wrap_reverse then
        (* The cross axis runs bottom to top: the group sits at the line's
           cross-start edge, its bottom *)
        let line_cross_size = free_space +. child.s.outer_target_height in
        line_cross_size -. max_baseline_to_bottom -. child.s.baseline
      else if constants.is_row then max_baseline -. child.s.baseline
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
  let is_row = constants.is_row in
  let lines = ref flex_lines in
  while !lines != [] do
    let line = List.hd !lines in
    lines := List.tl !lines;
    let line_cross_size = line.cross_size in
    let max_baseline = max_baseline line in
    (* In a wrap-reverse row, the largest distance from an item's baseline to
       its bottom margin edge among those in baseline alignment *)
    let max_baseline_to_bottom =
      if is_row && constants.is_wrap_reverse then (
        let max = ref 0.0 in
        Array.iter
          (fun child ->
            if participates_in_baseline child ~is_row then
              max :=
                Float.max !max (child.s.outer_target_height -. child.s.baseline))
          line.items;
        !max)
      else 0.0
    in
    for i = 0 to Array.length line.items - 1 do
      let child = line.items.(i) in
      let free_space =
        line_cross_size
        -.
        if is_row then child.s.outer_target_height
        else child.s.outer_target_width
      in

      let cross_start_auto =
        if is_row then margin_is_auto child top else margin_is_auto child left
      in
      let cross_end_auto =
        if is_row then margin_is_auto child bottom
        else margin_is_auto child right
      in

      if (cross_start_auto || cross_end_auto) && free_space < 0.0 then (
        (* The item overflows its line: an auto start margin is zero and an
           auto end margin takes the (negative) rest, so the item stays at the
           line's cross-start edge (CSS Flexbox 9.6, step 13) *)
        if cross_start_auto then
          if is_row then child.margin <- { child.margin with top = 0.0 }
          else child.margin <- { child.margin with left = 0.0 };
        if cross_end_auto then
          if is_row then
            child.margin <- { child.margin with bottom = free_space }
          else child.margin <- { child.margin with right = free_space })
      else if cross_start_auto && cross_end_auto then
        (* Both margins are auto - distribute free space equally *)
        if is_row then (
          child.margin <- { child.margin with top = free_space /. 2.0 };
          child.margin <- { child.margin with bottom = free_space /. 2.0 })
        else (
          child.margin <- { child.margin with left = free_space /. 2.0 };
          child.margin <- { child.margin with right = free_space /. 2.0 })
      else if cross_start_auto then
        (* Only start margin is auto *)
        if is_row then child.margin <- { child.margin with top = free_space }
        else child.margin <- { child.margin with left = free_space }
      else if cross_end_auto then
        (* Only end margin is auto *)
        if is_row then child.margin <- { child.margin with bottom = free_space }
        else child.margin <- { child.margin with right = free_space }
      else
        (* No auto margins - align according to align-self *)
        child.s.offset_cross <-
          align_flex_items_along_cross_axis child free_space max_baseline
            max_baseline_to_bottom constants
    done
  done

(* Determine container cross size *)
let determine_container_cross_size (flex_lines : flex_line list)
    (known_dimensions : float option size) (constants : algo_constants) : float
    =
  let cross_gap =
    if constants.is_row then constants.n.gap_height else constants.n.gap_width
  in
  let total_cross_axis_gap = sum_axis_gaps cross_gap (List.length flex_lines) in
  let total_line_cross_size = total_cross_size flex_lines in
  let padding_border_sum =
    if constants.is_row then Rectf.vertical_axis_sum constants.content_box_inset
    else Rectf.horizontal_axis_sum constants.content_box_inset
  in
  let cross_scrollbar_gutter =
    if constants.is_row then constants.n.scrollbar_gutter_y
    else constants.n.scrollbar_gutter_x
  in
  let min_cross_size =
    if constants.is_row then option constants.n.min_height
    else option constants.n.min_width
  in
  let max_cross_size =
    if constants.is_row then option constants.n.max_height
    else option constants.n.max_width
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
    constants.n.height <- outer_container_size;
    constants.n.inner_height <- inner_container_size)
  else (
    constants.n.width <- outer_container_size;
    constants.n.inner_width <- inner_container_size);

  total_line_cross_size

(* Align flex lines per align-content *)
let align_flex_lines_per_align_content (flex_lines : flex_line list)
    (constants : algo_constants) (total_line_cross_size : float) : unit =
  let num_lines = List.length flex_lines in
  let gap =
    if constants.is_row then constants.n.gap_height else constants.n.gap_width
  in
  let total_cross_axis_gap = sum_axis_gaps gap num_lines in
  let free_space =
    (if constants.is_row then constants.n.inner_height
     else constants.n.inner_width)
    -. total_line_cross_size -. total_cross_axis_gap
  in
  let is_safe = false in
  (* TODO: Implement safe alignment *)
  let align_content_mode =
    apply_alignment_fallback free_space num_lines constants.align_content
      is_safe
  in

  (* Process lines in reverse order for wrap-reverse *)
  let lines =
    ref (if constants.is_wrap_reverse then List.rev flex_lines else flex_lines)
  and first = ref true in
  while !lines != [] do
    let line = List.hd !lines in
    lines := List.tl !lines;
    line.offset_cross <-
      compute_alignment_offset free_space num_lines gap align_content_mode
        constants.is_wrap_reverse !first;
    first := false
  done

(* The offset an item's insets give it on an axis: its start inset, or its end
   inset negated, or zero. *)
let inset_offset ~start ~end_ context calc =
  let start = resolve_inset start context calc in
  if not (Float.is_nan start) then start
  else
    let end_ = resolve_inset end_ context calc in
    if not (Float.is_nan end_) then -.end_ else 0.0
[@@inline]

(* Final layout pass *)
let final_layout_pass (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (flex_lines : flex_line list) (constants : algo_constants)
    (overflow_acc : Toffee_compute_helpers.overflow_acc)
    (overflow_flags : Toffee_compute_helpers.overflow_flags)
    (overflow_client : Sizef.t) (overflow_padding : Rectf.t) : unit =
  let is_row = constants.is_row in
  let calc = Tree.resolve_calc_value tree in
  let node_inner_size = constants.node_inner_size in
  let inset_width = constants.initial_node_inner_size.width in
  let inset_height = constants.initial_node_inner_size.height in
  let reverse = Flex_direction_ext.is_reverse constants.dir in
  (* Every item is laid out in the container's size *)
  let available_space =
    {
      Size.width = Available_space.of_float constants.n.width;
      height = Available_space.of_float constants.n.height;
    }
  in
  let known_dimensions = ref Size.none in
  let total_offset_cross =
    ref
      (if is_row then constants.content_box_inset.top
       else constants.content_box_inset.left)
  in
  let lines =
    ref (if constants.is_wrap_reverse then List.rev flex_lines else flex_lines)
  in
  while !lines <> [] do
    let line = List.hd !lines in
    lines := List.tl !lines;
    let items = line.items in
    let count = Array.length items in
    let line_offset_cross = line.offset_cross in
    let total_offset_main =
      ref
        (if is_row then constants.content_box_inset.left
         else constants.content_box_inset.top)
    in
    for i = 0 to count - 1 do
      let item = items.(if reverse then count - 1 - i else i) in
      let style = item.style in

      (* Perform child layout, at the item's target size: the previous item's
         known sizes where they are the same *)
      let known_dimensions =
        let previous = !known_dimensions in
        let width =
          match previous.width with
          | Some width when same_float width item.s.target_width ->
              previous.width
          | _ -> Some item.s.target_width
        in
        let height =
          match previous.height with
          | Some height when same_float height item.s.target_height ->
              previous.height
          | _ -> Some item.s.target_height
        in
        if width == previous.width && height == previous.height then previous
        else (
          (known_dimensions := Size.{ width; height });
          !known_dimensions)
      in
      let layout_output =
        Tree.compute_child_layout tree item.node
          (item_input ~run_mode:Run_mode.Perform_layout
             ~axis:Requested_axis.Both ~known_dimensions
             ~known_dimensions_are_definite:
               (item_definiteness constants item
                  ~leaf:(Tree.child_count tree item.node = 0))
             ~parent_size:node_inner_size ~available_space)
      in

      let size = Layout_output.size layout_output in
      let overflow_area = Layout_output.scrollable_overflow layout_output in
      let inner_baseline =
        match (Layout_output.first_baselines layout_output).y with
        | Some baseline -> baseline
        | None -> size.height
      in

      let inset = Style.inset style in
      let margin_main_start =
        if is_row then item.margin.left else item.margin.top
      in
      let margin_cross_start =
        if is_row then item.margin.top else item.margin.left
      in

      (* Calculate main axis offset *)
      let offset_main =
        !total_offset_main +. item.s.offset_main +. margin_main_start
        +.
        if is_row then
          inset_offset ~start:inset.left ~end_:inset.right inset_width calc
        else inset_offset ~start:inset.top ~end_:inset.bottom inset_height calc
      in

      (* Calculate cross axis offset *)
      let offset_cross =
        !total_offset_cross +. item.s.offset_cross +. line_offset_cross
        +. margin_cross_start
        +.
        if is_row then
          inset_offset ~start:inset.top ~end_:inset.bottom inset_height calc
        else inset_offset ~start:inset.left ~end_:inset.right inset_width calc
      in

      (* Update baseline *)
      item.s.baseline <-
        (if is_row then
           !total_offset_cross +. item.s.offset_cross +. margin_cross_start
         else !total_offset_main +. item.s.offset_main +. margin_main_start)
        +. inner_baseline;

      (* Calculate location *)
      let location =
        if is_row then Pointf.{ x = offset_main; y = offset_cross }
        else Pointf.{ x = offset_cross; y = offset_main }
      in

      (* Calculate scrollbar size *)
      let overflow = Style.overflow style in
      let scrollbar_size =
        if overflow.x <> Overflow.Scroll && overflow.y <> Overflow.Scroll then
          Sizef.zero
        else
          Sizef.
            {
              width =
                (if overflow.y = Overflow.Scroll then
                   Style.scrollbar_width item.style
                 else 0.0);
              height =
                (if overflow.x = Overflow.Scroll then
                   Style.scrollbar_width item.style
                 else 0.0);
            }
      in

      (* Set layout on tree *)
      Tree.set_unrounded_layout tree item.node
        (Layout.make ~order:item.order ~location ~size
           ~scrollable_overflow:overflow_area ~scrollbar_size
           ~border:item.border ~padding:item.padding ~margin:item.margin);

      (* Update main offset *)
      total_offset_main :=
        !total_offset_main +. item.s.offset_main
        +. (if is_row then item.margin.left +. item.margin.right
            else item.margin.top +. item.margin.bottom)
        +. if is_row then size.width else size.height;

      (* Add the item to the container's scrollable overflow *)
      Toffee_compute_helpers.add_overflow_contribution overflow_acc
        ~flags:overflow_flags ~origin:constants.border ~client:overflow_client
        ~padding:overflow_padding ~location ~size ~overflow_area
        ~border:item.border ~overflow
        ~margin:
          (let margin = item.margin in
           (* An auto margin that took negative free space is zero to the
              end padding, as it is to Chromium *)
           if
             margin.left >= 0.0 && margin.right >= 0.0 && margin.top >= 0.0
             && margin.bottom >= 0.0
           then margin
           else
             Rectf.
               {
                 left =
                   (if margin.left < 0.0 && margin_is_auto item left then 0.0
                    else margin.left);
                 right =
                   (if margin.right < 0.0 && margin_is_auto item right then 0.0
                    else margin.right);
                 top =
                   (if margin.top < 0.0 && margin_is_auto item top then 0.0
                    else margin.top);
                 bottom =
                   (if margin.bottom < 0.0 && margin_is_auto item bottom then
                      0.0
                    else margin.bottom);
               })
    done;

    total_offset_cross :=
      !total_offset_cross +. line_offset_cross +. line.cross_size
  done

(* Perform absolute layout on absolutely positioned children *)
let perform_absolute_layout_on_absolute_children (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) (constants : algo_constants)
    (overflow_acc : Toffee_compute_helpers.overflow_acc)
    (overflow_flags : Toffee_compute_helpers.overflow_flags)
    (overflow_client : Sizef.t) : unit =
  let has_absolute = ref false in
  for order = 0 to Tree.child_count tree node - 1 do
    let style =
      Tree.get_core_container_style tree (Tree.get_child_id tree node order)
    in
    if
      Style.box_generation_mode style <> Box_generation_mode.None
      && Style.position style = Position.Absolute
    then has_absolute := true
  done;
  if not !has_absolute then ()
  else
    let container_width = constants.n.width in
    let container_height = constants.n.height in
    let inset_relative_size =
      Sizef.
        {
          width =
            constants.n.width
            -. (constants.border.left +. constants.border.right
              +. constants.n.scrollbar_gutter_x);
          height =
            constants.n.height
            -. (constants.border.top +. constants.border.bottom
              +. constants.n.scrollbar_gutter_y);
        }
    in

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

        let padding_border_sum = Rectf.add padding border |> Rectf.sum_axes in
        let box_sizing_adjustment =
          if Style.box_sizing child_style = Box_sizing.Content_box then
            padding_border_sum
          else Sizef.zero
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
          Dimension.maybe_resolve min_dims.width
            (Some inset_relative_size.width) calc
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
          Dimension.maybe_resolve max_dims.width
            (Some inset_relative_size.width) calc
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
                        (fun w ->
                          Float.min w (Option.value max_width ~default:w))
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
               ~available_space
               ~vertical_margins_are_collapsible:Line.both_false)
        in

        let measured_size = Layout_output.size measure_output in
        let final_size = Sizef.unwrap_or measured_size known_dimensions in
        (* Clamp final size between min and max - min wins over max when they
         conflict *)
        let final_size =
          Sizef.
            {
              width =
                (match (final_size.width, min_width, max_width) with
                | w, Some min_w, Some max_w ->
                    Float.max min_w (Float.min w max_w)
                | w, Some min_w, None -> Float.max min_w w
                | w, None, Some max_w -> Float.min w max_w
                | w, _, _ -> w);
              height =
                (match (final_size.height, min_height, max_height) with
                | h, Some min_h, Some max_h ->
                    Float.max min_h (Float.min h max_h)
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
               ~known_dimensions:(Sizef.some final_size)
               ~parent_size:constants.node_inner_size ~available_space
               ~vertical_margins_are_collapsible:Line.both_false)
        in

        (* Calculate non-auto margins *)
        let non_auto_margin =
          Rectf.
            {
              left = Option.value margin.left ~default:0.0;
              right = Option.value margin.right ~default:0.0;
              top = Option.value margin.top ~default:0.0;
              bottom = Option.value margin.bottom ~default:0.0;
            }
        in

        (* Calculate free space *)
        let free_space =
          Sizef.
            {
              width =
                constants.n.width -. final_size.width
                -. (non_auto_margin.left +. non_auto_margin.right);
              height =
                constants.n.height -. final_size.height
                -. (non_auto_margin.top +. non_auto_margin.bottom);
            }
        in
        let free_space =
          Sizef.
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
            Sizef.
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
          Rectf.
            {
              left = Option.value margin.left ~default:auto_margin_size.width;
              right = Option.value margin.right ~default:auto_margin_size.width;
              top = Option.value margin.top ~default:auto_margin_size.height;
              bottom =
                Option.value margin.bottom ~default:auto_margin_size.height;
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
                  (if constants.is_row then constants.n.width
                   else constants.n.height)
                  -. (if constants.is_row then constants.border.right
                      else constants.border.bottom)
                  -. (if constants.is_row then constants.n.scrollbar_gutter_x
                      else constants.n.scrollbar_gutter_y)
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
                  | End, _ | Flex_end, false | Flex_start, true | Stretch, true
                    ->
                      (if constants.is_row then constants.n.width
                       else constants.n.height)
                      -. (if constants.is_row then
                            constants.content_box_inset.right
                          else constants.content_box_inset.bottom)
                      -. (if constants.is_row then final_size.width
                          else final_size.height)
                      -.
                      if constants.is_row then resolved_margin.right
                      else resolved_margin.bottom
                  | Space_evenly, _ | Space_around, _ | Center, _ ->
                      ((if constants.is_row then constants.n.width
                        else constants.n.height)
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
                  (if constants.is_row then constants.n.height
                   else constants.n.width)
                  -. (if constants.is_row then constants.border.bottom
                      else constants.border.right)
                  -. (if constants.is_row then constants.n.scrollbar_gutter_y
                      else constants.n.scrollbar_gutter_x)
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
                      (if constants.is_row then constants.n.height
                       else constants.n.width)
                      -. (if constants.is_row then
                            constants.content_box_inset.bottom
                          else constants.content_box_inset.right)
                      -. (if constants.is_row then final_size.height
                          else final_size.width)
                      -.
                      if constants.is_row then resolved_margin.bottom
                      else resolved_margin.right
                  | Center, _ ->
                      ((if constants.is_row then constants.n.height
                        else constants.n.width)
                      +. (if constants.is_row then
                            constants.content_box_inset.top
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
          if constants.is_row then Pointf.{ x = offset_main; y = offset_cross }
          else Pointf.{ x = offset_cross; y = offset_main }
        in

        let scrollbar_size =
          Sizef.
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
             ~scrollable_overflow:
               (Layout_output.scrollable_overflow layout_output)
             ~scrollbar_size ~border ~padding ~margin:resolved_margin);

        (* Add the item to the container's scrollable overflow *)
        Toffee_compute_helpers.add_overflow_contribution overflow_acc
          ~flags:overflow_flags ~origin:constants.border ~client:overflow_client
          ~padding:Rectf.zero ~location ~size:final_size
          ~overflow_area:(Layout_output.scrollable_overflow layout_output)
          ~border ~overflow ~margin:resolved_margin
    done

(* Compute a preliminary size for an item *)
let compute_preliminary (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) ~run_mode ~known_dimensions
    ~known_dimensions_are_definite ~parent_size ~available_space :
    Layout_output.t =
  (* Define some general constants we will need for the remainder of the
     algorithm *)
  let constants =
    compute_constants
      (module Tree)
      tree
      (Tree.get_core_container_style tree node)
      known_dimensions known_dimensions_are_definite available_space parent_size
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
     Flex_direction_ext.main_size_opt constants.node_inner_size constants.dir
   with
  | Some inner_main_size ->
      let outer_main_size =
        inner_main_size
        +. Flex_direction_ext.main_axis_sum constants.content_box_inset
             constants.dir
      in
      if constants.is_row then (
        constants.n.inner_width <- inner_main_size;
        constants.n.width <- outer_main_size)
      else (
        constants.n.inner_height <- inner_main_size;
        constants.n.height <- outer_main_size)
  | None ->
      (* Sets constants.container_size and constants.inner_container_size *)
      determine_container_main_size
        (module Tree)
        tree available_space flex_lines constants;
      let inner_main =
        if constants.is_row then constants.n.inner_width
        else constants.n.inner_height
      in
      let outer_main =
        if constants.is_row then constants.n.width else constants.n.height
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
        if constants.is_row then constants.n.inner_width
        else constants.n.inner_height
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
      if constants.is_row then constants.n.gap_width <- new_gap
      else constants.n.gap_height <- new_gap);

  (* 6. Resolve the flexible lengths of all the flex items to find their used
     main size *)
  let lines = ref flex_lines in
  while !lines != [] do
    resolve_flexible_lengths (module Tree) tree (List.hd !lines) constants;
    lines := List.tl !lines
  done;

  (* 9.4. Cross Size Determination *)

  (* 7. Determine the hypothetical cross size of each item *)
  let lines = ref flex_lines in
  while !lines != [] do
    determine_hypothetical_cross_size
      (module Tree)
      tree (List.hd !lines) constants known_dimensions available_space;
    lines := List.tl !lines
  done;

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
    Layout_output.from_outer_size
      Sizef.{ width = constants.n.width; height = constants.n.height }
  else (
    (* 16. Align all flex lines per align-content *)
    align_flex_lines_per_align_content flex_lines constants
      total_line_cross_size;

    (* The container's scrollable overflow, gathered in its padding-box
       frame. Its scroll origin is at the flex-start of a reversed axis. *)
    let overflow = Style.overflow (Tree.get_core_container_style tree node) in
    let is_scroll_container =
      Overflow.is_container overflow.x || Overflow.is_container overflow.y
    in
    let reverse = Flex_direction_ext.is_reverse constants.dir in
    let reversed_x =
      if constants.is_row then reverse else constants.is_wrap_reverse
    and reversed_y =
      if constants.is_row then constants.is_wrap_reverse else reverse
    in
    let border = constants.border and inset = constants.content_box_inset in
    let overflow_acc = Toffee_compute_helpers.overflow_acc () in
    let overflow_flags =
      Toffee_compute_helpers.overflow_flags ~is_scroll_container ~reversed_x
        ~reversed_y ~margins:Full_margins ()
    in
    (* Only a scroll container reads its client box and extends its
       children's margin boxes by its padding *)
    let client =
      if not is_scroll_container then Sizef.zero
      else
        Sizef.
          {
            width =
              constants.n.width -. border.left -. border.right
              -. constants.n.scrollbar_gutter_x;
            height =
              constants.n.height -. border.top -. border.bottom
              -. constants.n.scrollbar_gutter_y;
          }
    and padding =
      if not is_scroll_container then Rectf.zero
      else
        Rectf.
          {
            left = inset.left -. border.left;
            right =
              inset.right -. border.right -. constants.n.scrollbar_gutter_x;
            top = inset.top -. border.top;
            bottom =
              inset.bottom -. border.bottom -. constants.n.scrollbar_gutter_y;
          }
    in

    (* Do a final layout pass and gather the resulting layouts *)
    final_layout_pass
      (module Tree)
      tree flex_lines constants overflow_acc overflow_flags client padding;

    (* Before returning we perform absolute layout on all absolutely positioned
       children *)
    perform_absolute_layout_on_absolute_children
      (module Tree)
      tree node constants overflow_acc
      (Toffee_compute_helpers.with_margins overflow_flags No_margins)
      client;

    (* Perform hidden layout on hidden children *)
    let len = Tree.child_count tree node in
    for order = 0 to len - 1 do
      let child = Tree.get_child_id tree node order in
      if
        Style.box_generation_mode (Tree.get_core_container_style tree child)
        = Box_generation_mode.None
      then
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
        |> ignore
    done;

    (* Calculate flex container baselines *)
    let first_vertical_baseline =
      (* The cross-start-most line: the last of a wrap-reverse container *)
      let rec last line = function
        | [] -> line
        | line :: rest -> last line rest
      in
      match flex_lines with
      | [] -> None
      | line :: rest -> (
          let first_line =
            if constants.is_wrap_reverse then last line rest else line
          in
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
                  if participates_in_baseline item ~is_row:true then Some item
                  else find (i + 1)
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
                  item.s.offset_cross
                else item.s.offset_main
              in
              Some (offset_cross +. item.s.baseline))
    in

    (* A scroll container's inline padding is part of its scrollable
       overflow, even without content *)
    if is_scroll_container then
      Toffee_compute_helpers.union overflow_acc ~left:0.0
        ~right:(padding.left +. padding.right)
        ~top:0.0 ~bottom:0.0;
    let size =
      Sizef.{ width = constants.n.width; height = constants.n.height }
    in
    let scrollable_overflow =
      Toffee_compute_helpers.finalize_overflow overflow_acc
        ~flags:overflow_flags ~size ~border ~client
    in

    (* Return final layout output *)
    Layout_output.make ~size ~scrollable_overflow
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
  (* [Rectf.sum_axes (Rectf.add padding border)], summed in its order *)
  let padding_border_width =
    padding.left +. border.left +. (padding.right +. border.right)
  in
  let padding_border_height =
    padding.top +. border.top +. (padding.bottom +. border.bottom)
  in
  let content_box = Style.box_sizing style = Box_sizing.Content_box in
  let adjustment_width = if content_box then padding_border_width else 0.0 in
  let adjustment_height = if content_box then padding_border_height else 0.0 in
  let resolve dims ~width =
    resolve_container_size dims ~width parent_size aspect_ratio
      ~adjustment:(if width then adjustment_width else adjustment_height)
      calc
      [@@inline]
  in
  let min_width = resolve (Style.min_size style) ~width:true in
  let min_height = resolve (Style.min_size style) ~width:false in
  let max_width = resolve (Style.max_size style) ~width:true in
  let max_height = resolve (Style.max_size style) ~width:false in

  (* The size of the container on an axis, floored by the padding and border,
     NaN when none is set: the known size, or else the minimum where the
     maximum does not exceed it, or else the style's size clamped by the
     minimum and maximum (inherent sizing alone). This is the order of Taffy's
     [known_dimensions.or(min_max_definite_size.or(clamped_style_size))]: the
     parent's known size wins, as the parent resolved the style against the
     containing block, which for an absolutely positioned child is the padding
     box. Taffy floors the two styled sizes alone and relies on parents to
     floor the known size. The floor stays on the known size too: a parent can
     give one below the padding and border, as when it stretches an item
     across a narrower line, and a box is never smaller than its padding and
     border, as Chromium lays it out. *)
  let styled known ~min ~max ~size ~padding_border =
    let size =
      match known with
      | Some known -> known
      | None ->
          if (not (Float.is_nan min)) && (not (Float.is_nan max)) && max <= min
          then min
          else if Float.is_nan size then size
          else if Float.is_nan min then
            if Float.is_nan max then size else Float.min max size
          else if Float.is_nan max then Float.max min size
          else Float.max min (Float.min max size)
    in
    if Float.is_nan size then size else Float.max size padding_border
      [@@inline]
  in
  let inherent = sizing_mode = Sizing_mode.Inherent_size in
  let width =
    styled known_dimensions.width ~min:min_width ~max:max_width
      ~size:
        (if inherent then resolve (Style.size style) ~width:true else Float.nan)
      ~padding_border:padding_border_width
  in
  let height =
    styled known_dimensions.height ~min:min_height ~max:max_height
      ~size:
        (if inherent then resolve (Style.size style) ~width:false else Float.nan)
      ~padding_border:padding_border_height
  in
  (* The known dimensions, or new ones where the style changed them *)
  let same known size =
    match known with
    | Some known ->
        Int64.equal (Int64.bits_of_float known) (Int64.bits_of_float size)
    | None -> Float.is_nan size
  in
  let styled_based_known_dimensions =
    if same known_dimensions.width width && same known_dimensions.height height
    then known_dimensions
    else { width = option width; height = option height }
  in

  (* Short-circuit layout if the container's size is fully determined and we're
     in ComputeSize mode *)
  let compute_size = run_mode = Run_mode.Compute_size in
  if compute_size && not (Float.is_nan width || Float.is_nan height) then
    Layout_output.from_outer_size Sizef.{ width; height }
  else if
    (* Nor when the width is known and only the width is requested. *)
    compute_size
    && (not (Float.is_nan width))
    && Layout_input.axis inputs = Requested_axis.Horizontal
  then Layout_output.from_outer_size Sizef.{ width; height = 0.0 }
  else
    (* A dimension the style resolved, rather than the parent, is definite *)
    let known_dimensions_are_definite =
      let given = Layout_input.known_dimensions_are_definite inputs in
      if
        (given.width || Option.is_none known_dimensions.width)
        && (given.height || Option.is_none known_dimensions.height)
      then both_definite
      else if given.width || Option.is_none known_dimensions.width then
        width_definite
      else if given.height || Option.is_none known_dimensions.height then
        height_definite
      else none_definite
    in
    compute_preliminary
      (module Tree)
      tree node ~run_mode ~known_dimensions:styled_based_known_dimensions
      ~known_dimensions_are_definite ~parent_size
      ~available_space:(Layout_input.available_space inputs)
