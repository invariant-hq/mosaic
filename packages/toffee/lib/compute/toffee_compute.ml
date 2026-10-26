module Style = Toffee_style
module Tree = Toffee_tree
open Toffee_geometry
open Style
open Tree

let compute_leaf_layout = Leaf.compute_leaf_layout
let compute_block_layout = Toffee_compute_block.compute_block_layout
let compute_flexbox_layout = Toffee_compute_flexbox.compute_flexbox_layout
let compute_grid_layout = Toffee_compute_grid.compute_grid_layout

(** Compute layout for the root node in the tree *)
let compute_root_layout (type t)
    (module Tree : Tree.LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (root : Node_id.t) (available_space : Available_space.t size) : unit =
  let mut_known_dimensions = ref Size.none in

  (* Block layout specific setup *)
  let parent_size = Size.map Available_space.to_option available_space in
  let style = Tree.get_core_container_style tree root in

  (if Style.display style = Display.Block then
     (* Pull these out earlier to avoid borrowing issues *)
     let aspect_ratio = Style.aspect_ratio style in
     let calc = Tree.resolve_calc_value tree in
     let margin =
       Style.margin style
       |> Length_percentage_auto.resolve_rect_or_zero parent_size.width calc
     in
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
       if Style.box_sizing style = Box_sizing.Content_box then
         padding_border_size
       else Sizef.zero
     in

     let min_size =
       Style.min_size style |> fun dims ->
       Size.
         {
           width = Dimension.maybe_resolve dims.width parent_size.width calc;
           height = Dimension.maybe_resolve dims.height parent_size.height calc;
         }
       |> Size.apply_aspect_ratio aspect_ratio
       |> Sizef.maybe_add box_sizing_adjustment
     in
     let max_size =
       Style.max_size style |> fun dims ->
       Size.
         {
           width = Dimension.maybe_resolve dims.width parent_size.width calc;
           height = Dimension.maybe_resolve dims.height parent_size.height calc;
         }
       |> Size.apply_aspect_ratio aspect_ratio
       |> Sizef.maybe_add box_sizing_adjustment
     in
     let clamped_style_size =
       Style.size style |> fun dims ->
       Size.
         {
           width = Dimension.maybe_resolve dims.width parent_size.width calc;
           height = Dimension.maybe_resolve dims.height parent_size.height calc;
         }
       |> Size.apply_aspect_ratio aspect_ratio
       |> Sizef.maybe_add box_sizing_adjustment
       |> Size.clamp_option min_size max_size
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

     (* Block nodes automatically stretch fit their width to fit available space
        if available space is definite *)
     let available_space_based_size =
       Size.
         {
           width =
             (match available_space.width with
             | Available_space.Definite w ->
                 Some (w -. Rectf.horizontal_axis_sum margin)
             | _ -> None);
           height = None;
         }
     in

     let styled_based_known_dimensions =
       !mut_known_dimensions |> fun dims ->
       Size.choose_first dims min_max_definite_size |> fun dims ->
       Size.choose_first dims clamped_style_size |> fun dims ->
       Size.choose_first dims available_space_based_size |> fun dims ->
       Sizef.maybe_max padding_border_size dims
     in

     mut_known_dimensions := styled_based_known_dimensions);

  (* Recursively compute node layout *)
  let output =
    Tree.compute_child_layout tree root
      (Layout_input.make ~run_mode:Run_mode.Perform_layout
         ~sizing_mode:Sizing_mode.Inherent_size ~axis:Requested_axis.Both
         ~known_dimensions:!mut_known_dimensions ~parent_size ~available_space
         ~vertical_margins_are_collapsible:Line.both_false)
  in

  let style = Tree.get_core_container_style tree root in
  let calc = Tree.resolve_calc_value tree in
  let padding =
    Style.padding style
    |> Length_percentage.resolve_rect_or_zero
         (Available_space.to_option available_space.width)
         calc
  in
  let border =
    Style.border style
    |> Length_percentage.resolve_rect_or_zero
         (Available_space.to_option available_space.width)
         calc
  in
  let margin =
    Style.margin style
    |> Length_percentage_auto.resolve_rect_or_zero
         (Available_space.to_option available_space.width)
         calc
  in
  let scrollbar_size =
    Sizef.
      {
        width =
          (if (Style.overflow style).y = Overflow.Scroll then
             Style.scrollbar_width style
           else 0.0);
        height =
          (if (Style.overflow style).x = Overflow.Scroll then
             Style.scrollbar_width style
           else 0.0);
      }
  in

  Tree.set_unrounded_layout tree root
    (Layout.make ~order:0 ~location:Pointf.zero
       ~size:(Layout_output.size output)
       ~content_size:(Layout_output.content_size output)
       ~scrollbar_size ~padding
       ~border (* TODO: support auto margins for root node? *)
       ~margin)

(** Attempts to find a cached layout for the specified node and layout inputs.
    Uses the provided closure to compute the layout (and then stores the result
    in the cache) if no cached layout is found. *)
let compute_cached_layout (type t)
    (module Tree : Tree.CACHE_TREE with type t = t) (tree : t)
    (node : Node_id.t) (inputs : Layout_input.t)
    (compute_uncached : t -> Node_id.t -> Layout_input.t -> Layout_output.t) :
    Layout_output.t =
  (* First we check if we have a cached result for the given input *)
  match Tree.cache_get tree node inputs with
  | Some cached_size_and_baselines -> cached_size_and_baselines
  | None ->
      let computed_size_and_baselines = compute_uncached tree node inputs in

      (* Cache result *)
      Tree.cache_store tree node inputs computed_size_and_baselines;

      computed_size_and_baselines

(** Rounds the calculated layout to exact pixel values

    In order to ensure that no gaps in the layout are introduced we:
    - Always round based on the cumulative x/y coordinates (relative to the
      viewport) rather than parent-relative coordinates
    - Compute width/height by first rounding the top/bottom/left/right and then
      computing the difference rather than rounding the width/height directly

    See
    https://github.com/facebook/yoga/commit/aa5b296ac78f7a22e1aeaf4891243c6bb76488e2
    for more context

    In order to prevent inaccuracies caused by rounding already-rounded values,
    we read from `unrounded_layout` and write to `final_layout`. *)
let round_layout (type t) (module Tree : Tree.ROUND_TREE with type t = t)
    (tree : t) (node_id : Node_id.t) : unit =
  (* Helper for rounding - matches Rust's round function behavior *)
  let round f =
    let epsilon = 1e-6 in
    if f >= 0.0 then Float.floor (f +. 0.5 +. epsilon)
    else Float.ceil (f -. 0.5 -. epsilon)
  in

  (* Recursive function to apply rounding to all descendants. A subtree whose
     unrounded layouts and parent position are unchanged keeps its result. *)
  let rec round_layout_inner node_id parent_x parent_y =
    if not (Tree.is_rounded tree node_id ~x:parent_x ~y:parent_y) then begin
      let unrounded_layout = Tree.get_unrounded_layout tree node_id in

      let cumulative_x = parent_x +. (Layout.location unrounded_layout).x in
      let cumulative_y = parent_y +. (Layout.location unrounded_layout).y in

      let layout =
        Layout.make
          ~order:(Layout.order unrounded_layout)
          ~location:
            Pointf.
              {
                x = round (Layout.location unrounded_layout).x;
                y = round (Layout.location unrounded_layout).y;
              }
          ~size:
            Sizef.
              {
                width =
                  round (cumulative_x +. (Layout.size unrounded_layout).width)
                  -. round cumulative_x;
                height =
                  round (cumulative_y +. (Layout.size unrounded_layout).height)
                  -. round cumulative_y;
              }
          ~content_size:
            Sizef.
              {
                width =
                  round
                    (cumulative_x
                   +. (Layout.content_size unrounded_layout).width)
                  -. round cumulative_x;
                height =
                  round
                    (cumulative_y
                   +. (Layout.content_size unrounded_layout).height)
                  -. round cumulative_y;
              }
          ~scrollbar_size:
            Sizef.
              {
                width = round (Layout.scrollbar_size unrounded_layout).width;
                height = round (Layout.scrollbar_size unrounded_layout).height;
              }
          ~border:
            Rectf.
              {
                left =
                  round (cumulative_x +. (Layout.border unrounded_layout).left)
                  -. round cumulative_x;
                right =
                  round (cumulative_x +. (Layout.size unrounded_layout).width)
                  -. round
                       (cumulative_x +. (Layout.size unrounded_layout).width
                      -. (Layout.border unrounded_layout).right);
                top =
                  round (cumulative_y +. (Layout.border unrounded_layout).top)
                  -. round cumulative_y;
                bottom =
                  round (cumulative_y +. (Layout.size unrounded_layout).height)
                  -. round
                       (cumulative_y +. (Layout.size unrounded_layout).height
                      -. (Layout.border unrounded_layout).bottom);
              }
          ~padding:
            Rectf.
              {
                left =
                  round (cumulative_x +. (Layout.padding unrounded_layout).left)
                  -. round cumulative_x;
                right =
                  round (cumulative_x +. (Layout.size unrounded_layout).width)
                  -. round
                       (cumulative_x +. (Layout.size unrounded_layout).width
                      -. (Layout.padding unrounded_layout).right);
                top =
                  round (cumulative_y +. (Layout.padding unrounded_layout).top)
                  -. round cumulative_y;
                bottom =
                  round (cumulative_y +. (Layout.size unrounded_layout).height)
                  -. round
                       (cumulative_y +. (Layout.size unrounded_layout).height
                      -. (Layout.padding unrounded_layout).bottom);
              }
          ~margin:(Layout.margin unrounded_layout)
      in

      Tree.set_final_layout tree node_id ~x:parent_x ~y:parent_y layout;

      let child_count = Tree.child_count tree node_id in
      for index = 0 to child_count - 1 do
        let child = Tree.get_child_id tree node_id index in
        round_layout_inner child cumulative_x cumulative_y
      done
    end
  in

  round_layout_inner node_id 0.0 0.0

module type CACHE_LAYOUT_PARTIAL_TREE = sig
  include Tree.LAYOUT_PARTIAL_TREE
  include Tree.CACHE_TREE with type t := t
end

(** Creates a layout for this node and its children, recursively. Each hidden
    node has zero size and is placed at the origin *)
let compute_hidden_layout (type t)
    (module Tree : CACHE_LAYOUT_PARTIAL_TREE with type t = t) (tree : t)
    (node : Node_id.t) : Layout_output.t =
  (* Clear cache and set zeroed-out layout for the node *)
  Tree.cache_clear tree node;
  Tree.set_unrounded_layout tree node (Layout.with_order 0);

  (* Perform hidden layout on all children *)
  let child_count = Tree.child_count tree node in
  for index = 0 to child_count - 1 do
    let child_id = Tree.get_child_id tree node index in
    Tree.compute_child_layout tree child_id Layout_input.hidden |> ignore
  done;

  Layout_output.hidden
