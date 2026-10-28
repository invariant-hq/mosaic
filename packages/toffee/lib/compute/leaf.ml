(** Computes size using styles and measure functions *)

module Style = Toffee_style
open Toffee_geometry
open Toffee_tree

(* The height [ratio] gives from [width], taken as the box takes it (floored
   by its padding and border [pb_sum]) and on the box its box-sizing names *)
let ratio_height style ratio (pb_sum : Sizef.t) width =
  let width = Float.max width pb_sum.width in
  if Style.box_sizing style = Style.Box_sizing.Content_box then
    ((width -. pb_sum.width) /. ratio) +. pb_sum.height
  else width /. ratio

(* The width [ratio] gives from [height], as [ratio_height] does the other
   way *)
let ratio_width style ratio (pb_sum : Sizef.t) height =
  let height = Float.max height pb_sum.height in
  if Style.box_sizing style = Style.Box_sizing.Content_box then
    ((height -. pb_sum.height) *. ratio) +. pb_sum.width
  else height *. ratio

(** Compute the size of a leaf node (node with no children) *)
let compute_leaf_layout ~(inputs : Layout_input.t) ~(style : Style.t)
    ~(resolve_calc_value : Style.calc_resolver)
    ~(measure_function : float option size -> Available_space.t size -> Sizef.t)
    : Layout_output.t =
  (* Unpack inputs *)
  let known_dimensions = Layout_input.known_dimensions inputs in
  let parent_size = Layout_input.parent_size inputs in
  let available_space = Layout_input.available_space inputs in
  let sizing_mode = Layout_input.sizing_mode inputs in
  let run_mode = Layout_input.run_mode inputs in

  (* Note: both horizontal and vertical percentage padding/borders are resolved
     against the container's inline size (i.e. width). This is not a bug, but is
     how CSS is specified (see:
     https://developer.mozilla.org/en-US/docs/Web/CSS/padding#values) *)
  let parent_inline_size = parent_size.width in

  let margin =
    Style.margin style
    |> Style.Length_percentage_auto.resolve_rect_or_zero parent_inline_size
         resolve_calc_value
  in
  let padding =
    Style.padding style
    |> Style.Length_percentage.resolve_rect_or_zero parent_inline_size
         resolve_calc_value
  in
  let border =
    Style.border style
    |> Style.Length_percentage.resolve_rect_or_zero parent_inline_size
         resolve_calc_value
  in
  let padding_border = Rectf.add padding border in
  let pb_sum = Rectf.sum_axes padding_border in
  let box_sizing_adjustment =
    if Style.box_sizing style = Style.Box_sizing.Content_box then pb_sum
    else Sizef.zero
  in

  (* Resolve node's preferred/min/max sizes (width/heights) against the
     available space (percentages resolve to pixel values). For ContentSize
     mode, we pretend that the node has no size styles as these should be
     ignored. *)
  let node_size, node_min_size, node_max_size, aspect_ratio =
    match sizing_mode with
    | Sizing_mode.Content_size -> (known_dimensions, Size.none, Size.none, None)
    | Sizing_mode.Inherent_size ->
        let aspect_ratio = Style.aspect_ratio style in

        let style_size =
          Style.size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> Size.apply_aspect_ratio aspect_ratio
          |> Sizef.maybe_add box_sizing_adjustment
        in

        let style_min_size =
          Style.min_size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> Size.apply_aspect_ratio aspect_ratio
          |> Sizef.maybe_add box_sizing_adjustment
        in

        let style_max_size =
          Style.max_size style |> fun dims ->
          Size.
            {
              width =
                Style.Dimension.maybe_resolve dims.width parent_size.width
                  resolve_calc_value;
              height =
                Style.Dimension.maybe_resolve dims.height parent_size.height
                  resolve_calc_value;
            }
          |> Sizef.maybe_add box_sizing_adjustment
        in

        let node_size = Size.choose_first known_dimensions style_size in
        (node_size, style_min_size, style_max_size, aspect_ratio)
  in

  (* Scrollbar gutters are reserved when the `overflow` property is set to
     `Overflow::Scroll`. However, the axis are switched (transposed) because a
     node that scrolls vertically needs *horizontal* space to be reserved for a
     scrollbar *)
  let content_box_inset =
    let overflow = Style.overflow style in
    let scrollbar_width = Style.scrollbar_width style in
    (* TODO: make side configurable based on the `direction` property *)
    Rectf.
      {
        padding_border with
        right =
          (padding_border.right
          +. if overflow.y = Style.Overflow.Scroll then scrollbar_width else 0.0
          );
        bottom =
          (padding_border.bottom
          +. if overflow.x = Style.Overflow.Scroll then scrollbar_width else 0.0
          );
      }
  in

  let has_styles_preventing_being_collapsed_through =
    (match Style.display style with Style.Display.Block -> false | _ -> true)
    || Style.Overflow.is_container (Style.overflow style).x
    || Style.Overflow.is_container (Style.overflow style).y
    || Style.position style = Style.Position.Absolute
    || padding.top > 0.0 || padding.bottom > 0.0 || border.top > 0.0
    || border.bottom > 0.0
    || (match node_size.height with Some h when h > 0.0 -> true | _ -> false)
    ||
    match node_min_size.height with
    | Some h when h > 0.0 -> true
    | _ -> false
  in

  (* Return early if both width and height are known *)
  match
    (run_mode, has_styles_preventing_being_collapsed_through, node_size)
  with
  | Run_mode.Compute_size, true, { width = Some width; height = Some height } ->
      let size =
        Sizef.clamp node_min_size node_max_size Sizef.{ width; height }
      in
      let size =
        match aspect_ratio with
        | Some ratio ->
            {
              size with
              height =
                Float.max size.height
                  (ratio_height style ratio pb_sum size.width);
            }
        | None -> size
      in
      let size = Sizef.max size pb_sum in
      Layout_output.make ~size ~scrollable_overflow:Scrollable_overflow.zero
        ~first_baselines:Point.none ~top_margin:Collapsible_margin_set.zero
        ~bottom_margin:Collapsible_margin_set.zero
        ~margins_can_collapse_through:false
  | _ ->
      (* Normal path - compute available space and measure *)

      (* Compute available space *)
      let available_space_computed =
        let compute_axis known avail_space_axis margin_sum node_size_axis
            node_min_size_axis node_max_size_axis inset_sum =
          let open Available_space in
          (* Start with known dimensions or available space *)
          (match known with Some v -> of_float v | None -> avail_space_axis)
          (* Subtract margin *)
          |> fun space ->
          sub_or_zero space (Some margin_sum)
          (* Override with known dimensions if present *)
          |> fun space ->
          set_or_self space known
          (* Override with node size if present *)
          |> fun space ->
          set_or_self space node_size_axis
          (* Apply max constraint even for Max_content *)
          |> fun space ->
          min_or_self space node_max_size_axis
          (* Apply constraints and subtract inset *)
          |> fun space ->
          map_definite_value space (fun size ->
              (* Clamp to min/max constraints *)
              let clamped =
                match (node_min_size_axis, node_max_size_axis) with
                | Some min, Some max -> Float.max min (Float.min max size)
                | Some min, None -> Float.max min size
                | None, Some max -> Float.min max size
                | None, None -> size
              in
              (* Subtract content box inset *)
              clamped -. inset_sum)
        in

        let width =
          compute_axis known_dimensions.width available_space.width
            (Rectf.horizontal_axis_sum margin)
            node_size.width node_min_size.width node_max_size.width
            (Rectf.horizontal_axis_sum content_box_inset)
        in
        let height =
          compute_axis known_dimensions.height available_space.height
            (Rectf.vertical_axis_sum margin)
            node_size.height node_min_size.height node_max_size.height
            (Rectf.vertical_axis_sum content_box_inset)
        in
        Size.{ width; height }
      in

      (* Measure node *)
      (* A measurement gives the measure function the known size of the
         content box, clamped as the available space is. A final layout gives
         it none, and the same size as available space, so that a function
         that treats the two alike measures the leaf the same in both. A
         replaced element is given its known size, or its style's, in both:
         its natural size does not depend on the space available, so it needs
         the size the layout fixes to transfer it through its ratio (CSS 2
         10.3.2 and 10.6.2). *)
      let measured_size =
        (* The known size itself, allocating nothing, when neither the
           bounds nor the inset change it. *)
        let[@inline] content known min max inset =
          match known with
          | None -> known
          | Some size ->
              let clamped =
                match max with Some max -> Float.min size max | None -> size
              in
              let clamped =
                match min with
                | Some min -> Float.max clamped min
                | None -> clamped
              in
              let content = Float.max 0. (clamped -. inset) in
              if Float.equal content size then known else Some content
        in
        let known =
          match run_mode with
          | Run_mode.Perform_hidden_layout ->
              invalid_arg "Leaf node cannot be measured in hidden layout mode"
          | _ when Style.item_is_replaced style -> node_size
          | Run_mode.Compute_size -> known_dimensions
          | Run_mode.Perform_layout -> Size.none
        in
        let known_for_measure =
          let width =
            content known.width node_min_size.width node_max_size.width
              (Rectf.horizontal_axis_sum content_box_inset)
          and height =
            content known.height node_min_size.height node_max_size.height
              (Rectf.vertical_axis_sum content_box_inset)
          in
          if width == known.width && height == known.height then known
          else Size.{ width; height }
        in
        measure_function known_for_measure available_space_computed
      in

      (* Match Taffy's leaf computation logic exactly *)
      let clamped_size =
        let chosen = Size.choose_first known_dimensions node_size in
        let measured_with_inset =
          Sizef.add measured_size (Rectf.sum_axes content_box_inset)
        in

        (* Taffy's logic: known_dimensions.or(node_size).unwrap_or(measured_size
           + content_box_inset.sum_axes()) *)
        let base_size =
          match (chosen.width, chosen.height) with
          | Some w, Some h -> Sizef.{ width = w; height = h }
          | Some w, None ->
              Sizef.{ width = w; height = measured_with_inset.height }
          | None, Some h ->
              Sizef.{ width = measured_with_inset.width; height = h }
          | None, None -> measured_with_inset
        in

        (* Clamp to min/max constraints *)
        Sizef.clamp node_min_size node_max_size base_size
      in

      (* Apply aspect ratio AFTER clamping, matching Taffy's logic exactly *)
      let size =
        match aspect_ratio with
        | Some ratio ->
            let aspect_height =
              ratio_height style ratio pb_sum clamped_size.width
            in
            let height = Float.max clamped_size.height aspect_height in
            (* An auto width is at least what the ratio gives from the height
               the box takes, floored by its padding and border *)
            let width =
              match (Size.choose_first known_dimensions node_size).width with
              | Some _ -> clamped_size.width
              | None ->
                  let width_from_height =
                    ratio_width style ratio pb_sum height
                  in
                  let width_from_height =
                    match node_max_size.width with
                    | Some max -> Float.min width_from_height max
                    | None -> width_from_height
                  in
                  Float.max clamped_size.width width_from_height
            in
            Sizef.{ width; height }
        | None -> clamped_size
      in

      let size = Sizef.max size pb_sum in

      (* The content's overflow area, from the padding-box origin. A scroll
         container's inline-end padding is part of it, and its block-end
         padding follows its content, if it has any, as in Chromium *)
      let scrollable_overflow =
        let overflow = Style.overflow style in
        let is_scroll_container =
          Style.Overflow.is_container overflow.x
          || Style.Overflow.is_container overflow.y
        in
        let right =
          padding.left +. measured_size.width
          +. if is_scroll_container then padding.right else 0.0
        and bottom =
          if is_scroll_container && measured_size.height <= 0.0 then 0.0
          else
            padding.top +. measured_size.height
            +. if is_scroll_container then padding.bottom else 0.0
        in
        if
          (* Content within the padding box overflows nothing *)
          (not is_scroll_container)
          && right <= size.width -. border.left -. border.right
          && bottom <= size.height -. border.top -. border.bottom
          || (right <= 0.0 && bottom <= 0.0)
          (* A scroll container whose client box and content cover no area
             has nothing to scroll *)
          || is_scroll_container
             && (Float.max right
                   (size.width -. border.left -. border.right
                  -. content_box_inset.right +. padding.right +. border.right)
                 <= 0.0
                || Float.max bottom
                     (size.height -. border.top -. border.bottom
                    -. content_box_inset.bottom +. padding.bottom
                    +. border.bottom)
                   <= 0.0)
        then Scrollable_overflow.zero
        else
          Scrollable_overflow.make
            Rectf.
              {
                left = 0.0;
                right = Float.max right 0.0;
                top = 0.0;
                bottom = Float.max bottom 0.0;
              }
            ~reversed_x:false ~reversed_y:false
      in
      Layout_output.make ~size ~scrollable_overflow ~first_baselines:Point.none
        ~top_margin:Collapsible_margin_set.zero
        ~bottom_margin:Collapsible_margin_set.zero
        ~margins_can_collapse_through:
          ((not has_styles_preventing_being_collapsed_through)
          && size.height = 0.0 && measured_size.height = 0.0)
