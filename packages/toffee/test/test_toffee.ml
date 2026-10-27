(* Basic tests for the toffee layout library - Updated API *)

open Toffee
open Geometry
open Windtrap

let test_helpers =
  (* Helper to check float equality with epsilon *)
  let float_eq ?(epsilon = 0.001) a b = Float.abs (a -. b) < epsilon in

  (* Helper to print layout for debugging *)
  let print_layout layout =
    Printf.printf "Layout { location: (%.2f, %.2f), size: (%.2f x %.2f) }\n"
      (Layout.location layout).x (Layout.location layout).y
      (Layout.size layout).width (Layout.size layout).height
  in

  (float_eq, print_layout)

let test_create_tree () =
  let tree = new_tree () in
  match new_leaf tree Style.default with
  | Ok _node -> is_true ~msg:"Node created" true
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)

let test_simple_leaf_layout () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.length 50.0;
           }
  in
  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          let layout_result = layout tree node in
          match layout_result with
          | Ok layout ->
              let float_eq, print_layout = test_helpers in
              print_layout layout;
              let size = Layout.size layout in
              is_true ~msg:"Width is 100" (float_eq size.width 100.0);
              is_true ~msg:"Height is 50" (float_eq size.height 50.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

let test_flexbox_row () =
  let tree = new_tree () in

  (* Create container with flex row *)
  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Row
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 300.0;
             height = Style.Dimension.length 100.0;
           }
  in

  (* Create child nodes *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e -> failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              failf "Failed to create container: %s" (Error.to_string e)
          | Ok container -> (
              let result =
                compute_layout tree container
                  Size.
                    {
                      width = Available_space.of_float 500.0;
                      height = Available_space.of_float 500.0;
                    }
              in

              match result with
              | Ok () -> (
                  (* Check container layout *)
                  (match layout tree container with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let size = Layout.size layout in
                      Printf.printf "Container layout: size=(%.1f x %.1f)\n"
                        size.width size.height;
                      is_true ~msg:"Container width" (float_eq size.width 300.0);
                      is_true ~msg:"Container height"
                        (float_eq size.height 100.0)
                  | Error _ -> fail "Failed to get container layout");

                  (* Check child layouts *)
                  (match layout tree child1 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Child1 layout: location=(%.1f, %.1f), size=(%.1f x \
                         %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child1 x position" (float_eq location.x 0.0);
                      is_true ~msg:"Child1 width" (float_eq size.width 50.0)
                  | Error _ -> fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Child2 layout: location=(%.1f, %.1f), size=(%.1f x \
                         %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child2 x position"
                        (float_eq location.x 50.0);
                      is_true ~msg:"Child2 width" (float_eq size.width 50.0)
                  | Error _ -> fail "Failed to get child2 layout")
              | Error _ -> fail "Failed to compute layout")))

let test_flexbox_column () =
  let tree = new_tree () in

  (* Create container with flex column *)
  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Column
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.length 300.0;
           }
  in

  (* Create child nodes *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child1: %s" (Error.to_string e)
  | Ok child1 -> (
      match new_leaf tree child_style with
      | Error e -> failf "Failed to create child2: %s" (Error.to_string e)
      | Ok child2 -> (
          match new_with_children tree container_style [| child1; child2 |] with
          | Error e ->
              failf "Failed to create container: %s" (Error.to_string e)
          | Ok container -> (
              let result =
                compute_layout tree container
                  Size.
                    {
                      width = Available_space.of_float 500.0;
                      height = Available_space.of_float 500.0;
                    }
              in

              match result with
              | Ok () -> (
                  (* Check container layout *)
                  (match layout tree container with
                  | Ok layout ->
                      let size = Layout.size layout in
                      Printf.printf
                        "Column container layout: size=(%.1f x %.1f)\n"
                        size.width size.height
                  | Error _ -> ());

                  (* Check child layouts *)
                  (match layout tree child1 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Column child1 layout: location=(%.1f, %.1f), \
                         size=(%.1f x %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child1 y position" (float_eq location.y 0.0)
                  | Error _ -> fail "Failed to get child1 layout");

                  match layout tree child2 with
                  | Ok layout ->
                      let float_eq, _ = test_helpers in
                      let location = Layout.location layout in
                      let size = Layout.size layout in
                      Printf.printf
                        "Column child2 layout: location=(%.1f, %.1f), \
                         size=(%.1f x %.1f)\n"
                        location.x location.y size.width size.height;
                      is_true ~msg:"Child2 y position"
                        (float_eq location.y 50.0)
                  | Error _ -> fail "Failed to get child2 layout")
              | Error _ -> fail "Failed to compute layout")))

let test_padding () =
  let tree = new_tree () in

  let style =
    Style.(
      default
      |> set_size
           Size.
             { width = Dimension.length 100.0; height = Dimension.length 100.0 }
      |> set_padding (Rect.all (Length_percentage.length 10.0)))
  in

  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          match layout tree node with
          | Ok layout ->
              let float_eq, _ = test_helpers in
              let size = Layout.size layout in
              (* With padding, the size should remain 100x100 for content-box
                 sizing *)
              is_true ~msg:"Width with padding" (float_eq size.width 100.0);
              is_true ~msg:"Height with padding" (float_eq size.height 100.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

let test_margin () =
  let tree = new_tree () in

  (* Create parent container *)
  let parent_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 200.0;
             height = Style.Dimension.length 200.0;
           }
  in

  (* Create child with margin *)
  let child_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 50.0;
             height = Style.Dimension.length 50.0;
           }
    |> Style.set_margin (Rect.all (Style.Length_percentage_auto.length 10.0))
  in

  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree parent_style [| child |] with
      | Error e -> failf "Failed to create parent: %s" (Error.to_string e)
      | Ok parent -> (
          let result =
            compute_layout tree parent
              Size.
                {
                  width = Available_space.of_float 500.0;
                  height = Available_space.of_float 500.0;
                }
          in

          match result with
          | Ok () -> (
              match layout tree child with
              | Ok layout ->
                  let float_eq, print_layout = test_helpers in
                  print_layout layout;
                  let location = Layout.location layout in
                  (* Child should be positioned at (10, 10) due to margin *)
                  is_true ~msg:"Child x with margin" (float_eq location.x 10.0);
                  is_true ~msg:"Child y with margin" (float_eq location.y 10.0)
              | Error _ -> fail "Failed to get child layout")
          | Error _ -> fail "Failed to compute layout"))

let test_flex_grow () =
  let tree = new_tree () in

  let container_style =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_flex_direction Style.Flex_direction.Row
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 200.0;
             height = Style.Dimension.length 100.0;
           }
  in

  (* Create child with flex_grow *)
  let child_style =
    Style.default |> Style.set_flex_grow 1.0
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.auto;
             height = Style.Dimension.length 50.0;
           }
  in

  Printf.printf "Creating child with flex_grow=%.1f\n" 1.0;
  match new_leaf tree child_style with
  | Error e -> failf "Failed to create child: %s" (Error.to_string e)
  | Ok child -> (
      match new_with_children tree container_style [| child |] with
      | Error e -> failf "Failed to create container: %s" (Error.to_string e)
      | Ok container -> (
          let result =
            compute_layout tree container
              Size.
                {
                  width = Available_space.of_float 500.0;
                  height = Available_space.of_float 500.0;
                }
          in

          match result with
          | Ok () -> (
              match layout tree child with
              | Ok layout ->
                  let float_eq, print_layout = test_helpers in
                  print_layout layout;
                  let size = Layout.size layout in
                  Printf.printf
                    "Flex grow test: child size=(%.1f x %.1f), expecting \
                     width=200.0\n"
                    size.width size.height;
                  (* Child should grow to fill container width *)
                  is_true ~msg:"Child width with flex_grow"
                    (float_eq size.width 200.0)
              | Error _ -> fail "Failed to get child layout")
          | Error _ -> fail "Failed to compute layout"))

let test_aspect_ratio () =
  let tree = new_tree () in

  let style =
    Style.default
    |> Style.set_aspect_ratio (Some 2.0) (* width / height = 2 *)
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 100.0;
             height = Style.Dimension.auto;
           }
  in

  match new_leaf tree style with
  | Error e -> failf "Failed to create node: %s" (Error.to_string e)
  | Ok node -> (
      let result =
        compute_layout tree node
          Size.
            {
              width = Available_space.of_float 500.0;
              height = Available_space.of_float 500.0;
            }
      in

      match result with
      | Ok () -> (
          match layout tree node with
          | Ok layout ->
              let float_eq, print_layout = test_helpers in
              print_layout layout;
              let size = Layout.size layout in
              (* Height should be width / aspect_ratio = 100 / 2 = 50 *)
              is_true ~msg:"Width" (float_eq size.width 100.0);
              is_true ~msg:"Height from aspect ratio"
                (float_eq size.height 50.0)
          | Error _ -> fail "Failed to get layout")
      | Error _ -> fail "Failed to compute layout")

(* Manual ports of selected taffy tests *)

type measure_ctx = { mutable count : int }

let or_fail msg = function
  | Ok v -> v
  | Error e -> failf "%s: %s" msg (Error.to_string e)

let compute_or_fail tree node available msg =
  match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "%s: %s" msg (Error.to_string e)

let test_measure_count_flexbox () =
  let tree = new_tree () in
  let ctx = { count = 0 } in
  let leaf =
    new_leaf_with_context tree Style.default ctx
    |> or_fail "Failed to create measured leaf"
  in
  let rec nest depth node =
    if depth = 0 then node
    else
      new_with_children tree Style.default [| node |]
      |> or_fail "Failed to create parent"
      |> nest (depth - 1)
  in
  let root = nest 100 leaf in
  let measure_function known_dimensions _available_space _node context _style =
    (match context with
    | Some data -> data.count <- data.count + 1
    | None -> ());
    match known_dimensions with
    | Size.{ width = Some width; height = Some height } ->
        Sizef.{ width; height }
    | _ -> Sizef.{ width = 50.0; height = 50.0 }
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout_with_measure tree root available measure_function with
  | Ok () -> ()
  | Error e ->
      failf "Failed to compute layout with measure: %s" (Error.to_string e));
  match get_node_context tree leaf with
  | None -> fail "Missing node context after layout"
  | Some data ->
      (* 6, as in Taffy since its cache keys on the requested axis and the
         parent size (#911) and a stretched item of a line of known cross
         size is not measured (#1332). *)
      equal ~msg:"Measure function call count" int 6 data.count

(* The measure function of Taffy's test helpers. A node with [words] holds a
   text in the Ahem font of the fixtures: every glyph is a 10px square, and
   lines break only between the words, whose lengths [words] lists. A node
   with no words is a 50x50 box. [calls] counts the calls that are asked a
   size not already known. *)
type test_node = { mutable calls : int; words : int list }

let test_measure (known : float option size)
    (available : Available_space.t size) _node context _style : Sizef.t =
  match (known, context) with
  | { width = Some width; height = Some height }, _ -> { width; height }
  | _, None ->
      {
        width = Option.value known.width ~default:0.;
        height = Option.value known.height ~default:0.;
      }
  | _, Some node ->
      node.calls <- node.calls + 1;
      let glyph = 10. in
      let measured : Sizef.t =
        match node.words with
        | [] -> { width = 50.; height = 50. }
        | words ->
            let longest = Float.of_int (List.fold_left max 0 words) *. glyph in
            let total = Float.of_int (List.fold_left ( + ) 0 words) *. glyph in
            let width =
              match (known.width, available.width) with
              | Some width, _ -> width
              | None, Min_content -> longest
              | None, Max_content -> total
              | None, Definite width -> Float.min width total
            in
            let width = Float.max width longest in
            let per_line = int_of_float (Float.floor (width /. glyph)) in
            let lines, _ =
              List.fold_left
                (fun (lines, line) n ->
                  if line + n > per_line then
                    ((if line > 0 then lines + 1 else lines), n)
                  else (lines, line + n))
                (1, 0) words
            in
            {
              width;
              height =
                Option.value known.height ~default:(Float.of_int lines *. glyph);
            }
      in
      {
        width = Option.value known.width ~default:measured.width;
        height = Option.value known.height ~default:measured.height;
      }

let max_content =
  Size.
    {
      width = Available_space.max_content;
      height = Available_space.max_content;
    }

(* Taffy's measure_count_grid: a measured grid leaf under 101 nested flex
   containers. *)
let test_measure_count_grid () =
  let tree = new_tree () in
  let node = { calls = 0; words = [] } in
  let leaf =
    new_leaf_with_context tree
      (Style.default |> Style.set_display Style.Display.Grid)
      node
    |> or_fail "leaf"
  in
  let rec nest depth node =
    if depth = 0 then node
    else
      new_with_children tree Style.default [| node |]
      |> or_fail "parent"
      |> nest (depth - 1)
  in
  let root = nest 101 leaf in
  compute_layout_with_measure tree root max_content test_measure
  |> or_fail "compute";
  equal ~msg:"Measure function call count" int 6 node.calls

(* Taffy's measure_count_flex_in_grid: a text in a flex container that is the
   item of a 50px wide grid. Taffy counts 9 calls in a row and 10 in a column.
   Toffee's flex container also computes its items' cross sizes when it is
   asked for its main size alone, where Taffy returns early, so its counts are
   its own: 13 and 10 when the stretched leaf was measured for its
   hypothetical cross size, 11 and 9 now. *)
let test_measure_count_flex_in_grid () =
  List.iter
    (fun (direction, expected) ->
      let tree = new_tree () in
      let node = { calls = 0; words = [ 2; 2; 2; 2 ] } in
      let leaf =
        new_leaf_with_context tree Style.default node |> or_fail "leaf"
      in
      let flex =
        new_with_children tree
          (Style.default |> Style.set_flex_direction direction)
          [| leaf |]
        |> or_fail "flex"
      in
      let grid =
        new_with_children tree
          (Style.default
          |> Style.set_display Style.Display.Grid
          |> Style.set_size
               Size.
                 {
                   width = Style.Dimension.length 50.;
                   height = Style.Dimension.auto;
                 })
          [| flex |]
        |> or_fail "grid"
      in
      compute_layout_with_measure tree grid max_content test_measure
      |> or_fail "compute";
      let size = Layout.size (layout tree leaf |> or_fail "layout") in
      equal ~msg:"Leaf width" float_exact 50. size.width;
      equal ~msg:"Leaf height" float_exact 20. size.height;
      equal ~msg:"Measure function call count" int expected node.calls)
    [ (Style.Flex_direction.Row, 11); (Style.Flex_direction.Column, 9) ]

(* The cache, as Taffy's cache.rs tests it: a measurement of a [width]-wide
   node at max-content. *)
let measure_input ?(sizing_mode = Toffee_tree.Sizing_mode.Inherent_size)
    ?(axis = Toffee_tree.Requested_axis.Both) width =
  Layout_input.make ~run_mode:Run_mode.Compute_size ~sizing_mode ~axis
    ~known_dimensions:Size.{ width = Some width; height = None }
    ~parent_size:Size.none
    ~available_space:
      Size.
        {
          width = Available_space.max_content;
          height = Available_space.max_content;
        }
    ~vertical_margins_are_collapsible:Line.both_false

let measured width =
  Layout_output.from_outer_size Sizef.{ width; height = width }

let cached cache input =
  Option.map
    (fun output -> (Layout_output.size output).width)
    (Cache.get cache Style.default input)

let cached_width = option float_exact

let test_cache_second_chance () =
  let cache = Cache.make () in
  for width = 0 to Cache.cache_size do
    Cache.store cache Style.default
      (measure_input (float_of_int width))
      (measured (float_of_int width))
  done;
  (* The tenth evicted the first; using the second gives it a second chance,
     so the eleventh evicts the third. *)
  equal ~msg:"evicted" cached_width None (cached cache (measure_input 0.));
  equal ~msg:"used" cached_width (Some 1.) (cached cache (measure_input 1.));
  Cache.store cache Style.default
    (measure_input (float_of_int (Cache.cache_size + 1)))
    (measured (float_of_int (Cache.cache_size + 1)));
  equal ~msg:"kept" cached_width (Some 1.) (cached cache (measure_input 1.));
  equal ~msg:"replaced" cached_width None (cached cache (measure_input 2.))

let test_cache_update_in_place () =
  let cache = Cache.make () in
  Cache.store cache Style.default (measure_input 1.) (measured 1.);
  Cache.store cache Style.default (measure_input 2.) (measured 2.);
  Cache.store cache Style.default (measure_input 1.) (measured 3.);
  equal ~msg:"updated" cached_width (Some 3.) (cached cache (measure_input 1.));
  equal ~msg:"kept" cached_width (Some 2.) (cached cache (measure_input 2.))

let test_cache_margin_collapse_not_cached () =
  let cache = Cache.make () in
  let with_margins ~through ~top width =
    Layout_output.make
      ~size:Sizef.{ width; height = width }
      ~content_size:Sizef.{ width = 0.; height = 0. }
      ~first_baselines:Point.none ~top_margin:top
      ~bottom_margin:Toffee_tree.Collapsible_margin_set.zero
      ~margins_can_collapse_through:through
  in
  Cache.store cache Style.default (measure_input 1.)
    (with_margins ~through:true ~top:Toffee_tree.Collapsible_margin_set.zero 1.);
  equal ~msg:"collapse through" cached_width None
    (cached cache (measure_input 1.));
  Cache.store cache Style.default (measure_input 2.)
    (with_margins ~through:false
       ~top:(Toffee_tree.Collapsible_margin_set.from_margin 10.)
       2.);
  equal ~msg:"carried margin" cached_width None
    (cached cache (measure_input 2.))

let test_cache_mode_and_axis () =
  let cache = Cache.make () in
  Cache.store cache Style.default (measure_input 1.) (measured 1.);
  equal ~msg:"other sizing mode" cached_width None
    (cached cache
       (measure_input ~sizing_mode:Toffee_tree.Sizing_mode.Content_size 1.));
  Cache.store cache Style.default
    (measure_input ~sizing_mode:Toffee_tree.Sizing_mode.Content_size 1.)
    (measured 4.);
  equal ~msg:"mode change drops entries" cached_width None
    (cached cache (measure_input 1.));
  let cache = Cache.make () in
  Cache.store cache Style.default
    (measure_input ~axis:Toffee_tree.Requested_axis.Horizontal 1.)
    (measured 1.);
  equal ~msg:"same axis" cached_width (Some 1.)
    (cached cache
       (measure_input ~axis:Toffee_tree.Requested_axis.Horizontal 1.));
  equal ~msg:"other axis" cached_width None
    (cached cache (measure_input ~axis:Toffee_tree.Requested_axis.Vertical 1.));
  equal ~msg:"both axes" cached_width None (cached cache (measure_input 1.));
  Cache.store cache Style.default (measure_input 1.) (measured 2.);
  equal ~msg:"both axes answer one" cached_width (Some 2.)
    (cached cache (measure_input ~axis:Toffee_tree.Requested_axis.Vertical 1.))

(* Taffy's grid_block_item_height_not_polluted_by_width_measure (#1010): the
   block algorithm answers a width-only measurement with height 0, which a
   later height query must not reuse. *)
let test_cache_width_measure_does_not_answer_height () =
  let tree = new_tree () in
  let item row =
    let leaf =
      new_leaf_with_context tree Style.default ()
      |> or_fail "Failed to create leaf"
    in
    new_with_children tree
      (Style.make ~display:Style.Display.Block
         ~grid_row:
           Line.
             {
               start = Style.Grid_placement.line row;
               end_ = Style.Grid_placement.line (row + 1);
             }
         ~grid_column:
           Line.
             {
               start = Style.Grid_placement.line 1;
               end_ = Style.Grid_placement.line 2;
             }
         ())
      [| leaf |]
    |> or_fail "Failed to create item"
  in
  let items = [ item 1; item 2; item 3 ] in
  let container =
    new_with_children tree
      (Style.make ~display:Style.Display.Grid
         ~grid_template_columns:
           [
             Style.Grid_template_component.percent 0.6;
             Style.Grid_template_component.auto;
           ]
         ~size:
           Size.
             {
               width = Style.Dimension.length 1000.;
               height = Style.Dimension.auto;
             }
         ())
      (Array.of_list items)
    |> or_fail "Failed to create grid"
  in
  let measure known _available _node context _style =
    match context with
    | None -> Sizef.{ width = 0.; height = 0. }
    | Some () ->
        Sizef.
          {
            width = Option.value known.Size.width ~default:100.;
            height = Option.value known.Size.height ~default:19.;
          }
  in
  compute_layout_with_measure tree container
    Size.
      {
        width = Available_space.Definite 1000.;
        height = Available_space.Definite 1000.;
      }
    measure
  |> or_fail "Failed to compute layout";
  List.iteri
    (fun index node ->
      let layout = layout tree node |> or_fail "Missing layout" in
      equal ~msg:"height" float_exact 19. (Layout.size layout).height;
      equal ~msg:"y" float_exact
        (19. *. float_of_int index)
        (Layout.location layout).y)
    items

(* A leaf of [length] cells that takes the height available to it, up to its
   length. *)
let reader length (known : float option size)
    (available : Available_space.t size) =
  Sizef.
    {
      width =
        (match (known.width, available.width) with
        | Some width, _ -> width
        | None, Definite width -> Float.min width length
        | None, Min_content -> Float.min 5. length
        | None, Max_content -> length);
      height =
        (match (known.height, available.height) with
        | Some height, _ -> height
        | None, Definite height -> Float.min height length
        | None, Min_content -> 1.
        | None, Max_content -> length);
    }

(* A text of [length] cells, which wraps to the width it is given. *)
let text length (known : float option size) (available : Available_space.t size)
    =
  let width =
    match (known.width, available.width) with
    | Some width, _ -> width
    | None, Definite width -> Float.min width length
    | None, Min_content -> Float.min 5. length
    | None, Max_content -> length
  in
  Sizef.
    {
      width;
      height =
        Option.value known.height
          ~default:(Float.ceil (length /. Float.max 1. width));
    }

(* A grid holding a leaf whose style is [leaf_style] is laid out [width] wide
   at the root height [before], then at [after]. The grid measures the leaf
   against its own inner height, unknown while the grid is measured and known
   once it is laid out, and the leaf's minimum or maximum height is a
   percentage of that height: a measurement taken against one must not answer
   a query against the other. The leaf and the root get the heights a fresh
   layout at [after] gives them; the leaf's height is returned. *)
let relayout_at_another_height ~leaf_style ~measure ~width ~before ~after =
  let build () =
    let tree = new_tree () in
    let leaf = new_leaf_with_context tree leaf_style () |> or_fail "leaf" in
    let grid =
      new_with_children tree
        (Style.default |> Style.set_display Style.Display.Grid)
        [| leaf |]
      |> or_fail "grid"
    in
    let root =
      new_with_children tree Style.default [| grid |] |> or_fail "root"
    in
    (tree, root, leaf)
  in
  let lay tree root height =
    compute_layout_with_measure tree root
      Size.
        {
          width = Available_space.of_float width;
          height = Available_space.of_float height;
        }
      (fun known available _ _ _ -> measure known available)
    |> or_fail "layout"
  in
  let tree, root, leaf = build () in
  lay tree root before;
  lay tree root after;
  let fresh, fresh_root, fresh_leaf = build () in
  lay fresh fresh_root after;
  let height tree node = (Layout.size (unrounded_layout tree node)).height in
  equal ~msg:"leaf height" (float 1e-6) (height fresh fresh_leaf)
    (height tree leaf);
  equal ~msg:"root height" (float 1e-6) (height fresh fresh_root)
    (height tree root);
  height tree leaf

let test_cache_max_height_follows_parent_height () =
  let height =
    relayout_at_another_height
      ~leaf_style:
        (Style.default |> Style.set_max_height (Style.Dimension.percent 0.57))
      ~measure:(reader 22.) ~width:30. ~before:20. ~after:30.
  in
  equal ~msg:"57% of the 22-cell grid area" (float 1e-6) 12.54 height

let test_cache_min_height_follows_parent_height () =
  let height =
    relayout_at_another_height
      ~leaf_style:
        (Style.default |> Style.set_min_height (Style.Dimension.percent 0.85))
      ~measure:(reader 1.) ~width:70. ~before:40. ~after:30.
  in
  equal ~msg:"the content above 85% of the 1-cell grid area" (float 1e-6) 1.
    height

(* A block's measurement lays its children out to find its height and keeps
   their layouts from the tree, which holds the layouts of the block's final
   layout. Here a centered flex item holds a block holding a text of 8 cells.
   The block is measured at its min-content width, where the text wraps to 5
   cells and 2 lines, and laid out at 8 cells. Laid out again under another
   root height, the block's final layout comes from its cache, and the text
   keeps the layout of that final layout. *)
let test_block_measurement_keeps_child_layouts () =
  let tree = new_tree () in
  let leaf = new_leaf_with_context tree Style.default () |> or_fail "text" in
  let block =
    new_with_children tree
      (Style.default |> Style.set_display Style.Display.Block)
      [| leaf |]
    |> or_fail "block"
  in
  let item =
    new_with_children tree
      (Style.default |> Style.set_align_self (Some Style.Align_items.Center))
      [| block |]
    |> or_fail "item"
  in
  let root =
    new_with_children tree Style.default [| item |] |> or_fail "root"
  in
  let lay height =
    compute_layout_with_measure tree root
      Size.
        {
          width = Available_space.of_float 60.;
          height = Available_space.of_float height;
        }
      (fun known available _ _ _ -> text 8. known available)
    |> or_fail "layout"
  in
  let size () = Layout.size (layout tree leaf |> or_fail "text layout") in
  lay 20.;
  equal ~msg:"width" float_exact 8. (size ()).width;
  equal ~msg:"height" float_exact 1. (size ()).height;
  lay 60.;
  equal ~msg:"width after relayout" float_exact 8. (size ()).width;
  equal ~msg:"height after relayout" float_exact 1. (size ()).height

(* The leaves of these trees hold a text or a reader. *)
type content = Text of float | Reader of float

let content_measure known available _ content _ =
  match content with
  | Some (Text length) -> text length known available
  | Some (Reader length) -> reader length known available
  | None -> Sizef.zero

(* A tree that [build] makes is laid out [width] wide at the root height
   [before], changed by [change] and laid out at [after]. Each node gets the
   layout it gets in a tree that is built, changed and laid out once at
   [after]. *)
let relayout_matches_fresh ?(change = fun _ _ -> ()) ~build ~width ~before
    ~after () =
  let lay tree root height =
    compute_layout_with_measure tree root
      Size.
        {
          width = Available_space.of_float width;
          height = Available_space.of_float height;
        }
      content_measure
    |> or_fail "layout"
  in
  let tree = new_tree () in
  let root = build tree in
  lay tree root before;
  change tree root;
  lay tree root after;
  let fresh = new_tree () in
  let fresh_root = build fresh in
  change fresh fresh_root;
  lay fresh fresh_root after;
  let rec check path node fresh_node =
    let l = unrounded_layout tree node
    and f = unrounded_layout fresh fresh_node in
    let msg what = Printf.sprintf "node %s %s" path what in
    equal ~msg:(msg "x") (float 1e-6) (Layout.location f).x
      (Layout.location l).x;
    equal ~msg:(msg "y") (float 1e-6) (Layout.location f).y
      (Layout.location l).y;
    equal ~msg:(msg "width") (float 1e-6) (Layout.size f).width
      (Layout.size l).width;
    equal ~msg:(msg "height") (float 1e-6) (Layout.size f).height
      (Layout.size l).height;
    List.iteri
      (fun i (child, fresh_child) ->
        check (Printf.sprintf "%s/%d" path i) child fresh_child)
      (List.combine
         (children tree node |> or_fail "children")
         (children fresh fresh_node |> or_fail "children"))
  in
  check "root" root fresh_root

let node tree style children =
  new_with_children tree style (Array.of_list children) |> or_fail "node"

let leaf tree ?(style = Style.default) content =
  new_leaf_with_context tree style content |> or_fail "leaf"

(* A block measured to size its flex item lays out its in-flow children
   while its own height is unknown, so its child's height of 84% behaves as
   auto and the child lays its text out 1 cell tall instead of 84% of the
   block. The relayout at another root height measures the block again and
   takes its final layout from the cache, which lays nothing out again. *)
let test_block_measurement_lays_out_children () =
  relayout_matches_fresh ~width:40. ~before:60. ~after:40.
    ~build:(fun tree ->
      node tree Style.default
        [
          node tree
            (Style.default |> Style.set_display Style.Display.Block)
            [
              node tree
                (Style.default
                |> Style.set_height (Style.Dimension.percent 0.84))
                [ leaf tree (Text 1.) ];
            ];
        ])
    ()

(* A flex row aligning its items on their baselines lays the items out to
   find them, also when it is only measured. Here the row is absolutely
   positioned in a column and measured when a sibling text changes. *)
let test_baseline_measurement_lays_out_items () =
  relayout_matches_fresh ~width:70. ~before:30. ~after:30.
    ~build:(fun tree ->
      node tree
        (Style.default |> Style.set_flex_direction Style.Flex_direction.Column)
        [
          node tree
            (Style.default
            |> Style.set_position Style.Position.Absolute
            |> Style.set_align_items (Some Style.Align_items.Baseline))
            [
              node tree
                (Style.default
                |> Style.set_height (Style.Dimension.percent 0.85))
                [ leaf tree (Text 1.) ];
              leaf tree (Text 1.);
            ];
          leaf tree (Text 1.);
        ])
    ~change:(fun tree root ->
      let sibling = child_at_index tree root 1 |> or_fail "sibling" in
      set_node_context tree sibling (Some (Text 14.)) |> or_fail "context")
    ()

(* A grid aligning its items on their baselines lays them out at their
   min-content size to find them. *)
let test_grid_baseline_measurement_lays_out_items () =
  let module G = Style.Grid_template_component in
  relayout_matches_fresh ~width:50. ~before:10. ~after:36.
    ~build:(fun tree ->
      node tree Style.default
        [
          node tree
            (Style.default
            |> Style.set_display Style.Display.Grid
            |> Style.set_align_items (Some Style.Align_items.Baseline)
            |> Style.set_grid_template_columns
                 [
                   G.fr 3.;
                   G.minmax ~min:Style.Compact_length.auto
                     ~max:(Style.Compact_length.fr 1.);
                   G.minmax ~min:Style.Compact_length.auto
                     ~max:(Style.Compact_length.fr 1.);
                 ])
            [
              leaf tree
                ~style:
                  (Style.default
                  |> Style.set_padding_top
                       (Style.Length_percentage.percent 0.215))
                (Text 22.);
              node tree Style.default [ leaf tree (Reader 29.) ];
            ];
        ])
    ()

(* A percent-width item clamped by its own max-width must be measured at the
   clamped width when deriving content-based sizes, so wrapping content
   reports the height the final layout paints. Regression: the flex base size
   and container content size measured the child at the unclamped style
   width, positioning the following sibling over the child's painted rows. *)
let test_max_width_clamps_measured_wrap () =
  let tree = new_tree () in
  let auto_size =
    Size.{ width = Style.Dimension.auto; height = Style.Dimension.auto }
  in
  (* 120 columns of wrapping content: height is the line count at the width
     the layout offers. *)
  let text =
    new_leaf_with_context tree
      (Style.default |> Style.set_size auto_size)
      "text"
    |> or_fail "Failed to create text leaf"
  in
  let inset_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.percent 1.0;
             height = Style.Dimension.auto;
           }
    |> Style.set_max_size
         Size.
           {
             width = Style.Dimension.length 60.0;
             height = Style.Dimension.auto;
           }
  in
  let inset =
    new_with_children tree inset_style [| text |]
    |> or_fail "Failed to create inset"
  in
  let hint =
    new_leaf_with_context tree
      (Style.default
      |> Style.set_size
           Size.
             {
               width = Style.Dimension.percent 1.0;
               height = Style.Dimension.length 1.0;
             })
      "hint"
    |> or_fail "Failed to create hint leaf"
  in
  let root_style =
    Style.default
    |> Style.set_flex_direction Style.Flex_direction.Column
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.length 140.0;
             height = Style.Dimension.auto;
           }
  in
  let root =
    new_with_children tree root_style [| inset; hint |]
    |> or_fail "Failed to create root"
  in
  let measure_function known available _node context _style =
    match context with
    | Some "text" ->
        (* Word-wrapping text: 120 columns of content in 10-column words, so
           min-content is one word and max-content the unwrapped line. *)
        let wrap_width =
          match known.Size.width with
          | Some w -> w
          | None -> (
              match available.Size.width with
              | Available_space.Definite w -> w
              | Available_space.Min_content -> 10.0
              | Available_space.Max_content -> 120.0)
        in
        let width = Float.min 120.0 (Float.max 10.0 wrap_width) in
        let height =
          match known.Size.height with
          | Some h -> h
          | None -> ceil (120.0 /. width)
        in
        Sizef.{ width; height }
    | _ ->
        Sizef.
          {
            width = Option.value known.Size.width ~default:0.0;
            height = Option.value known.Size.height ~default:0.0;
          }
  in
  let available =
    Size.
      {
        width = Available_space.of_float 140.0;
        height = Available_space.max_content;
      }
  in
  (match compute_layout_with_measure tree root available measure_function with
  | Ok () -> ()
  | Error e ->
      failf "Failed to compute layout with measure: %s" (Error.to_string e));
  let inset_layout = Result.get_ok (layout tree inset) in
  let hint_layout = Result.get_ok (layout tree hint) in
  let float_eq, _ = test_helpers in
  is_true ~msg:"Inset width clamps to max-width 60"
    (float_eq (Layout.size inset_layout).width 60.0);
  is_true ~msg:"Inset height is the wrap count at the clamped width"
    (float_eq (Layout.size inset_layout).height 2.0);
  is_true ~msg:"Hint sits below the wrapped inset"
    (float_eq (Layout.location hint_layout).y 2.0)

let make_size width height =
  Size.
    {
      width = Style.Dimension.length width;
      height = Style.Dimension.length height;
    }

let definite available = Available_space.of_float available

let test_min_overrides_max () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_min_size (make_size 100.0 100.0)
    |> Style.set_max_size (make_size 10.0 10.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width clamped to min" (float 0.001) 100.0
    (Layout.size layout).width;
  equal ~msg:"Height clamped to min" (float 0.001) 100.0
    (Layout.size layout).height

let test_max_overrides_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_max_size (make_size 10.0 10.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width clamped to max" (float 0.001) 10.0
    (Layout.size layout).width;
  equal ~msg:"Height clamped to max" (float 0.001) 10.0
    (Layout.size layout).height

let test_min_overrides_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size (make_size 50.0 50.0)
    |> Style.set_min_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width from min" (float 0.001) 100.0 (Layout.size layout).width;
  equal ~msg:"Height from min" (float 0.001) 100.0 (Layout.size layout).height

let test_root_with_percentage_size () =
  let tree = new_tree () in
  let style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.percent 1.0;
             height = Style.Dimension.percent 1.0;
           }
  in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 200.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Percent width uses available space" (float 0.001) 100.0
    (Layout.size layout).width;
  equal ~msg:"Percent height uses available space" (float 0.001) 200.0
    (Layout.size layout).height

let test_root_with_no_size () =
  let tree = new_tree () in
  let node = new_leaf tree Style.default |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Default width is zero" (float 0.001) 0.0
    (Layout.size layout).width;
  equal ~msg:"Default height is zero" (float 0.001) 0.0
    (Layout.size layout).height

let test_root_with_larger_size () =
  let tree = new_tree () in
  let style = Style.default |> Style.set_size (make_size 200.0 200.0) in
  let node = new_leaf tree style |> or_fail "Failed to create node" in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  (match compute_layout tree node available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree node) in
  equal ~msg:"Width keeps explicit size" (float 0.001) 200.0
    (Layout.size layout).width;
  equal ~msg:"Height keeps explicit size" (float 0.001) 200.0
    (Layout.size layout).height

let test_root_padding_and_border_larger_than_definite_size () =
  let tree = new_tree () in
  let child = new_leaf tree Style.default |> or_fail "Failed to create child" in
  let padding = Rect.all (Style.Length_percentage.length 10.0) in
  let border = Rect.all (Style.Length_percentage.length 10.0) in
  let style =
    Style.default
    |> Style.set_size (make_size 10.0 10.0)
    |> Style.set_padding padding |> Style.set_border border
  in
  let root =
    new_with_children tree style [| child |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout tree root available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout = Result.get_ok (layout tree root) in
  equal ~msg:"Width accounts for padding and border" (float 0.001) 40.0
    (Layout.size layout).width;
  equal ~msg:"Height accounts for padding and border" (float 0.001) 40.0
    (Layout.size layout).height

let test_rounding_doesnt_leave_gaps () =
  let tree = new_tree () in
  let square = Style.default |> Style.set_size (make_size 100.3 100.3) in
  let child_a = new_leaf tree square |> or_fail "Failed to create child" in
  let child_b = new_leaf tree square |> or_fail "Failed to create child" in
  let root_style =
    Style.default
    |> Style.set_size (make_size 963.3333 1000.0)
    |> Style.set_justify_content (Some Style.Align_content.Center)
  in
  let root =
    new_with_children tree root_style [| child_a; child_b |]
    |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  (match compute_layout tree root available with
  | Ok () -> ()
  | Error e -> failf "Failed to compute layout: %s" (Error.to_string e));
  let layout_a = Result.get_ok (layout tree child_a) in
  let layout_b = Result.get_ok (layout tree child_b) in
  let end_of_a = (Layout.location layout_a).x +. (Layout.size layout_a).width in
  equal ~msg:"Children abut after rounding" (float 0.001) end_of_a
    (Layout.location layout_b).x

let test_toggle_root_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create node" in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree node available "Compute hidden layout";
  let layout_none = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_none).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0 (Layout.size layout_none).height;
  set_style tree node flex |> or_fail "set_style flex";
  compute_or_fail tree node available "Compute flex layout";
  let layout_flex = Result.get_ok (layout tree node) in
  equal ~msg:"Flex width" (float 0.001) 100.0 (Layout.size layout_flex).width;
  equal ~msg:"Flex height" (float 0.001) 100.0 (Layout.size layout_flex).height;
  set_style tree node hidden |> or_fail "set_style hidden";
  compute_or_fail tree node available "Compute hidden layout again";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden).height

let test_toggle_root_display_none_with_children () =
  let tree = new_tree () in
  let child_style = Style.default |> Style.set_size (make_size 800.0 100.0) in
  let child = new_leaf tree child_style |> or_fail "Failed to create child" in
  let parent_style = Style.default |> Style.set_size (make_size 800.0 100.0) in
  let parent =
    new_with_children tree parent_style [| child |]
    |> or_fail "Failed to create parent"
  in
  let root =
    new_with_children tree Style.default [| parent |] |> or_fail "Root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout visible root";
  let layout_child = Result.get_ok (layout tree child) in
  equal ~msg:"Child width" (float 0.001) 800.0 (Layout.size layout_child).width;
  equal ~msg:"Child height" (float 0.001) 100.0
    (Layout.size layout_child).height;
  let hidden_root = Style.set_display Style.Display.None Style.default in
  set_style tree root hidden_root |> or_fail "set_style hidden root";
  compute_or_fail tree root available "Compute layout hidden root";
  let hidden_child = Result.get_ok (layout tree child) in
  equal ~msg:"Hidden child width" (float 0.001) 0.0
    (Layout.size hidden_child).width;
  equal ~msg:"Hidden child height" (float 0.001) 0.0
    (Layout.size hidden_child).height;
  set_style tree root Style.default |> or_fail "set_style default root";
  compute_or_fail tree root available "Compute layout restored root";
  let restored_child = Result.get_ok (layout tree child) in
  equal ~msg:"Restored child width" (float 0.001) 800.0
    (Layout.size restored_child).width;
  equal ~msg:"Restored child height" (float 0.001) 100.0
    (Layout.size restored_child).height

let test_toggle_flex_child_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree flex [| node |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden child";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree node flex |> or_fail "set_style flex child";
  compute_or_fail tree root available "Compute layout flex child";
  let layout_flex = Result.get_ok (layout tree node) in
  equal ~msg:"Flex width" (float 0.001) 100.0 (Layout.size layout_flex).width;
  equal ~msg:"Flex height" (float 0.001) 100.0 (Layout.size layout_flex).height;
  set_style tree node hidden |> or_fail "set_style hidden child";
  compute_or_fail tree root available "Compute layout child hidden again";
  let layout_hidden_again = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_toggle_flex_container_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let flex =
    Style.default
    |> Style.set_display Style.Display.Flex
    |> Style.set_size (make_size 100.0 100.0)
  in
  let child = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree hidden [| child |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden container";
  let layout_hidden = Result.get_ok (layout tree root) in
  equal ~msg:"Root hidden width" (float 0.001) 0.0
    (Layout.size layout_hidden).width;
  equal ~msg:"Root hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree root flex |> or_fail "set_style flex container";
  compute_or_fail tree root available "Compute layout flex container";
  let layout_flex = Result.get_ok (layout tree root) in
  equal ~msg:"Root flex width" (float 0.001) 100.0
    (Layout.size layout_flex).width;
  equal ~msg:"Root flex height" (float 0.001) 100.0
    (Layout.size layout_flex).height;
  set_style tree root hidden |> or_fail "set_style hidden container";
  compute_or_fail tree root available "Compute layout hidden container again";
  let layout_hidden_again = Result.get_ok (layout tree root) in
  equal ~msg:"Root hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Root hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_toggle_grid_child_display_none () =
  let tree = new_tree () in
  let hidden =
    Style.default
    |> Style.set_display Style.Display.None
    |> Style.set_size (make_size 100.0 100.0)
  in
  let grid =
    Style.default
    |> Style.set_display Style.Display.Grid
    |> Style.set_size (make_size 100.0 100.0)
  in
  let node = new_leaf tree hidden |> or_fail "Failed to create child" in
  let root =
    new_with_children tree grid [| node |] |> or_fail "Failed to create root"
  in
  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_or_fail tree root available "Compute layout hidden grid child";
  let layout_hidden = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width" (float 0.001) 0.0 (Layout.size layout_hidden).width;
  equal ~msg:"Hidden height" (float 0.001) 0.0
    (Layout.size layout_hidden).height;
  set_style tree node grid |> or_fail "set_style grid child";
  compute_or_fail tree root available "Compute layout grid child";
  let layout_grid = Result.get_ok (layout tree node) in
  equal ~msg:"Grid width" (float 0.001) 100.0 (Layout.size layout_grid).width;
  equal ~msg:"Grid height" (float 0.001) 100.0 (Layout.size layout_grid).height;
  set_style tree node hidden |> or_fail "set_style hidden grid child";
  compute_or_fail tree root available "Compute layout grid child hidden again";
  let layout_hidden_again = Result.get_ok (layout tree node) in
  equal ~msg:"Hidden width again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).width;
  equal ~msg:"Hidden height again" (float 0.001) 0.0
    (Layout.size layout_hidden_again).height

let test_relayout_stable () =
  let tree = new_tree () in
  let node1 =
    Style.default
    |> Style.set_size (make_size 8.0 80.0)
    |> new_leaf tree |> or_fail "node1"
  in
  let node0_style =
    Style.default
    |> Style.set_align_self (Some Style.Align_items.Center)
    |> Style.set_size
         Size.{ width = Style.Dimension.auto; height = Style.Dimension.auto }
  in
  let node0 =
    new_with_children tree node0_style [| node1 |] |> or_fail "node0"
  in
  let root_style =
    Style.default
    |> Style.set_size
         Size.
           {
             width = Style.Dimension.percent 1.0;
             height = Style.Dimension.percent 1.0;
           }
  in
  let root = new_with_children tree root_style [| node0 |] |> or_fail "root" in
  let available =
    Size.
      {
        width = Available_space.of_float 100.0;
        height = Available_space.of_float 100.0;
      }
  in
  compute_or_fail tree root available "Compute layout relayout";
  let initial = Result.get_ok (layout tree root) in
  let initial0 = Result.get_ok (layout tree node0) in
  let initial1 = Result.get_ok (layout tree node1) in
  for _ = 1 to 5 do
    compute_or_fail tree root available "Recompute layout relayout";
    equal ~msg:"Root x stable" (float 0.001) (Layout.location initial).x
      (Layout.location (Result.get_ok (layout tree root))).x;
    equal ~msg:"Root y stable" (float 0.001) (Layout.location initial).y
      (Layout.location (Result.get_ok (layout tree root))).y;
    equal ~msg:"Node0 x stable" (float 0.001) (Layout.location initial0).x
      (Layout.location (Result.get_ok (layout tree node0))).x;
    equal ~msg:"Node0 y stable" (float 0.001) (Layout.location initial0).y
      (Layout.location (Result.get_ok (layout tree node0))).y;
    equal ~msg:"Node1 x stable" (float 0.001) (Layout.location initial1).x
      (Layout.location (Result.get_ok (layout tree node1))).x;
    equal ~msg:"Node1 y stable" (float 0.001) (Layout.location initial1).y
      (Layout.location (Result.get_ok (layout tree node1))).y
  done

(* Test suite *)
(* Rounding skips subtrees whose unrounded layouts and position are unchanged.
   The law: after any sequence of edits, every final layout is bit-for-bit the
   layout a full rounding pass computes from the current unrounded layouts. *)
let full_rounding tree root =
  let finals = Hashtbl.create 64 in
  Toffee_compute.round_layout
    (module struct
      type t = unit Toffee.tree

      let children tree node = children tree node |> Result.get_ok
      let child_ids tree node = List.to_seq (children tree node)
      let child_count tree node = List.length (children tree node)

      let get_child_id tree node index =
        child_at_index tree node index |> Result.get_ok

      let get_unrounded_layout = unrounded_layout
      let is_rounded _ _ ~x:_ ~y:_ = false

      let set_final_layout _ node ~x:_ ~y:_ layout =
        Hashtbl.replace finals (Node_id.to_int node) layout
    end)
    tree root;
  finals

let random_style rng =
  let fraction () = Float.of_int (Random.State.int rng 40) /. 3. in
  let dimension () =
    match Random.State.int rng 3 with
    | 0 -> Style.Dimension.auto
    | 1 -> Style.Dimension.length (fraction ())
    | _ -> Style.Dimension.pct (Float.of_int (Random.State.int rng 10) /. 7.)
  in
  let edges () =
    Rect.
      {
        left = Style.Length_percentage.length (fraction () /. 4.);
        right = Style.Length_percentage.length (fraction () /. 4.);
        top = Style.Length_percentage.length (fraction () /. 4.);
        bottom = Style.Length_percentage.length (fraction () /. 4.);
      }
  in
  Style.default
  |> Style.set_flex_direction
       (if Random.State.bool rng then Style.Flex_direction.Row
        else Style.Flex_direction.Column)
  |> Style.set_flex_grow (Float.of_int (Random.State.int rng 3) /. 2.)
  |> Style.set_size Size.{ width = dimension (); height = dimension () }
  |> Style.set_padding (edges ())
  |> Style.set_border (edges ())
  |> Style.set_margin_left
       (Style.Length_percentage_auto.length (fraction () /. 5.))
  |> Style.set_display
       (if Random.State.int rng 10 = 0 then Style.Display.None
        else Style.Display.Flex)

(* The layouts of [node]'s subtree outside hidden nodes, which keep a zero
   layout and lay out nothing below them. [layout]'s value for a node is
   replaced whenever these change, so an unchanged value shows unchanged
   bits. *)
let subtree_bits tree node =
  let buffer = Buffer.create 256 in
  let rec walk node =
    let layout = layout tree node |> Result.get_ok in
    Buffer.add_string buffer (Marshal.to_string layout [ Marshal.No_sharing ]);
    if
      not
        (Style.Display.is_none
           (Style.display (style tree node |> Result.get_ok)))
    then List.iter walk (children tree node |> Result.get_ok)
  in
  walk node;
  Buffer.contents buffer

let rec hidden tree node =
  Style.Display.is_none (Style.display (style tree node |> Result.get_ok))
  || match parent tree node with Some p -> hidden tree p | None -> false

let incremental_rounding_matches_full_rounding seed =
  let rng = Random.State.make [| seed |] in
  let previous = Hashtbl.create 64 in
  let tree : unit Toffee.tree = new_tree () in
  let root = new_leaf tree Style.default |> Result.get_ok in
  let nodes = ref [| root |] in
  let add_node () =
    let parent = !nodes.(Random.State.int rng (Array.length !nodes)) in
    let node = new_leaf tree (random_style rng) |> Result.get_ok in
    add_child tree parent node |> Result.get_ok;
    nodes := Array.append !nodes [| node |]
  in
  for _ = 1 to 1 + Random.State.int rng 30 do
    add_node ()
  done;
  let check step =
    let available =
      let space () =
        Available_space.of_float
          (50. +. (Float.of_int (Random.State.int rng 60) /. 7.))
      in
      Size.{ width = space (); height = space () }
    in
    compute_layout tree root available |> Result.get_ok;
    let finals = full_rounding tree root in
    Hashtbl.iter
      (fun index expected ->
        let actual = layout tree (Node_id.make index) |> Result.get_ok in
        let bits layout = Marshal.to_string layout [ Marshal.No_sharing ] in
        if bits actual <> bits expected then
          failf "seed %d, step %d, node %d: %s, full rounding gives %s" seed
            step index (Layout.to_string actual)
            (Layout.to_string expected))
      finals;
    Hashtbl.iter
      (fun index _ ->
        let node = Node_id.make index in
        let value = layout tree node |> Result.get_ok in
        let bits = subtree_bits tree node in
        (match Hashtbl.find_opt previous index with
        | Some (value', bits')
          when value == value' && bits <> bits' && not (hidden tree node) ->
            failf
              "seed %d, step %d, node %d: same layout value, changed subtree"
              seed step index
        | Some _ | None -> ());
        Hashtbl.replace previous index (value, bits))
      finals
  in
  check 0;
  for step = 1 to 20 do
    (match Random.State.int rng 6 with
    | 0 -> add_node ()
    | 1 | 2 ->
        let node = !nodes.(Random.State.int rng (Array.length !nodes)) in
        set_style tree node (random_style rng) |> Result.get_ok
    | 3 -> (
        let node = !nodes.(Random.State.int rng (Array.length !nodes)) in
        match parent tree node with
        | Some parent -> ignore (remove_child tree parent node |> Result.get_ok)
        | None -> ())
    | 4 ->
        (* A move, or the attachment of a removed node, under a node outside
           its subtree. *)
        let node = !nodes.(Random.State.int rng (Array.length !nodes)) in
        let target = !nodes.(Random.State.int rng (Array.length !nodes)) in
        let rec inside node' =
          Node_id.equal node' node
          || match parent tree node' with Some p -> inside p | None -> false
        in
        if not (Node_id.equal node root || inside target) then begin
          (match parent tree node with
          | Some parent ->
              ignore (remove_child tree parent node |> Result.get_ok)
          | None -> ());
          add_child tree target node |> Result.get_ok
        end
    | _ -> (* A relayout under new available space only. *) ());
    check step
  done

(* A box is drawn at the sum of the rounded locations from the root, and its
   far edges there plus its rounded size. The law: these are its edges in the
   root's coordinates, rounded, whatever the fractions of the boxes above
   it. *)
let drawn_edges_are_rounded_edges seed =
  let rng = Random.State.make [| seed |] in
  let tree : unit Toffee.tree = new_tree () in
  let root = new_leaf tree Style.default |> Result.get_ok in
  let nodes = ref [| root |] in
  for _ = 1 to 1 + Random.State.int rng 30 do
    let parent = !nodes.(Random.State.int rng (Array.length !nodes)) in
    let node = new_leaf tree (random_style rng) |> Result.get_ok in
    add_child tree parent node |> Result.get_ok;
    nodes := Array.append !nodes [| node |]
  done;
  let space () =
    Available_space.of_float
      (50. +. (Float.of_int (Random.State.int rng 60) /. 7.))
  in
  compute_layout tree root Size.{ width = space (); height = space () }
  |> Result.get_ok;
  let round f =
    if f >= 0. then Float.floor (f +. 0.5 +. 1e-6)
    else Float.ceil (f -. 0.5 -. 1e-6)
  in
  let rec walk node ~x ~y ~drawn_x ~drawn_y =
    let unrounded = unrounded_layout tree node in
    let rounded = layout tree node |> Result.get_ok in
    let x = x +. (Layout.location unrounded).x
    and y = y +. (Layout.location unrounded).y in
    let drawn_x = drawn_x +. (Layout.location rounded).x
    and drawn_y = drawn_y +. (Layout.location rounded).y in
    let check what drawn edge =
      if drawn <> round edge then
        failf "seed %d, node %d: %s drawn at %g, rounded edge %g" seed
          (Node_id.to_int node) what drawn (round edge)
    in
    check "left" drawn_x x;
    check "top" drawn_y y;
    check "right"
      (drawn_x +. (Layout.size rounded).width)
      (x +. (Layout.size unrounded).width);
    check "bottom"
      (drawn_y +. (Layout.size rounded).height)
      (y +. (Layout.size unrounded).height);
    List.iter
      (fun child -> walk child ~x ~y ~drawn_x ~drawn_y)
      (children tree node |> Result.get_ok)
  in
  walk root ~x:0. ~y:0. ~drawn_x:0. ~drawn_y:0.

(* A column ahead of a sibling that grows is laid out again under its
   parent's new height and stores layouts equal to the stored ones, which keep
   their records: nothing in it is rounded again, and each of its layouts is
   physically the layout of the previous pass. The sibling, changed through
   the tree, and the root get new values. *)
let test_equal_relayout_keeps_layouts () =
  let tree = new_tree () in
  let leaf width height =
    Style.default
    |> Style.set_size (make_size width height)
    |> new_leaf tree |> or_fail "leaf"
  in
  let column_style =
    Style.default |> Style.set_flex_direction Style.Flex_direction.Column
  in
  let a = leaf 20.6 10.3 in
  let b = leaf 30.2 10.3 in
  let column =
    new_with_children tree column_style [| a; b |] |> or_fail "column"
  in
  let tail = leaf 10.0 5.5 in
  let root =
    new_with_children tree column_style [| column; tail |] |> or_fail "root"
  in
  let available = Size.{ width = definite 200.0; height = definite 200.0 } in
  let layouts nodes =
    List.map
      (fun node ->
        (layout tree node |> Result.get_ok, unrounded_layout tree node))
      nodes
  in
  compute_or_fail tree root available "Compute layout";
  let kept = layouts [ column; a; b ] and replaced = layouts [ root; tail ] in
  set_style tree tail (Style.default |> Style.set_size (make_size 10.0 7.25))
  |> or_fail "set style";
  compute_or_fail tree root available "Recompute layout";
  List.iter2
    (fun (final, unrounded) (final', unrounded') ->
      is_true ~msg:"Final layout kept" (final == final');
      is_true ~msg:"Unrounded layout kept" (unrounded == unrounded'))
    kept
    (layouts [ column; a; b ]);
  List.iter2
    (fun (final, _) (final', _) ->
      is_true ~msg:"Final layout replaced" (final != final'))
    replaced
    (layouts [ root; tail ]);
  (* An equal style still changes the node: it and its ancestors get new
     values, its sibling keeps its own. *)
  let kept = layouts [ column ] and replaced = layouts [ root; tail ] in
  set_style tree tail (Style.default |> Style.set_size (make_size 10.0 7.25))
  |> or_fail "set equal style";
  compute_or_fail tree root available "Recompute layout";
  List.iter2
    (fun (final, _) (final', _) ->
      is_true ~msg:"Sibling layout kept" (final == final'))
    kept (layouts [ column ]);
  List.iter2
    (fun (final, _) (final', _) ->
      is_true ~msg:"Changed node's layout replaced" (final != final'))
    replaced
    (layouts [ root; tail ])

(* Two equal children swap parents at different fractional positions. Every
   unrounded layout stays equal, and each child's rounded height follows its
   new position. *)
let test_moved_child_rounded_at_new_position () =
  let tree = new_tree () in
  let box = Style.default |> Style.set_size (make_size 10.0 10.4) in
  let c1 = new_leaf tree box |> or_fail "c1" in
  let c2 = new_leaf tree box |> or_fail "c2" in
  let a = new_with_children tree box [| c1 |] |> or_fail "a" in
  let b = new_with_children tree box [| c2 |] |> or_fail "b" in
  let root =
    new_with_children tree
      (Style.default |> Style.set_flex_direction Style.Flex_direction.Column)
      [| a; b |]
    |> or_fail "root"
  in
  let available = Size.{ width = definite 100.0; height = definite 100.0 } in
  let height node = (Layout.size (layout tree node |> Result.get_ok)).height in
  compute_or_fail tree root available "Compute layout";
  equal ~msg:"c1 at y 0" float_exact 10.0 (height c1);
  equal ~msg:"c2 at y 10.4" float_exact 11.0 (height c2);
  set_children tree a [| c2 |] |> or_fail "set a";
  set_children tree b [| c1 |] |> or_fail "set b";
  compute_or_fail tree root available "Recompute layout";
  equal ~msg:"c1 at y 10.4" float_exact 11.0 (height c1);
  equal ~msg:"c2 at y 0" float_exact 10.0 (height c2)

let () =
  run "Toffee Tests"
    [
      group "basic"
        [
          test "Create tree and node" test_create_tree;
          test "Simple leaf layout" test_simple_leaf_layout;
        ];
      group "flexbox"
        [
          test "Flexbox row" test_flexbox_row;
          test "Flexbox column" test_flexbox_column;
          test "Flex grow" test_flex_grow;
        ];
      group "spacing" [ test "Padding" test_padding; test "Margin" test_margin ];
      group "sizing" [ test "Aspect ratio" test_aspect_ratio ];
      group "caching"
        [
          test "Measure count flexbox" test_measure_count_flexbox;
          test "Measure count grid" test_measure_count_grid;
          test "Measure count flex in grid" test_measure_count_flex_in_grid;
          test "Second chance eviction" test_cache_second_chance;
          test "Update in place" test_cache_update_in_place;
          test "Margin collapse not cached"
            test_cache_margin_collapse_not_cached;
          test "Mode and axis" test_cache_mode_and_axis;
          test "Width measure does not answer height"
            test_cache_width_measure_does_not_answer_height;
          test "Maximum height follows the parent height"
            test_cache_max_height_follows_parent_height;
          test "Minimum height follows the parent height"
            test_cache_min_height_follows_parent_height;
          test "Block measurement keeps its children's layouts"
            test_block_measurement_keeps_child_layouts;
          test "Block measurement lays out its children"
            test_block_measurement_lays_out_children;
          test "Baseline measurement lays out the items"
            test_baseline_measurement_lays_out_items;
          test "Grid baseline measurement lays out the items"
            test_grid_baseline_measurement_lays_out_items;
        ];
      group "min_max_overrides"
        [
          test "Min overrides max" test_min_overrides_max;
          test "Max overrides size" test_max_overrides_size;
          test "Min overrides size" test_min_overrides_size;
          test "Max width clamps measured wrap"
            test_max_width_clamps_measured_wrap;
        ];
      group "root_constraints"
        [
          test "Root with percentage size" test_root_with_percentage_size;
          test "Root with no size" test_root_with_no_size;
          test "Root with larger size" test_root_with_larger_size;
          test "Root padding and border larger than size"
            test_root_padding_and_border_larger_than_definite_size;
        ];
      group "rounding"
        [
          test "Rounding without gaps" test_rounding_doesnt_leave_gaps;
          prop ~count:300 "Incremental rounding matches full rounding" Gen.int
            incremental_rounding_matches_full_rounding;
          prop ~count:300 "Drawn edges are the rounded edges" Gen.int
            drawn_edges_are_rounded_edges;
          test "Equal relayout keeps layouts" test_equal_relayout_keeps_layouts;
          test "Moved child rounded at its new position"
            test_moved_child_rounded_at_new_position;
        ];
      group "display_none"
        [
          test "Toggle root display none" test_toggle_root_display_none;
          test "Toggle root display none with children"
            test_toggle_root_display_none_with_children;
          test "Toggle flex child display none"
            test_toggle_flex_child_display_none;
          test "Toggle flex container display none"
            test_toggle_flex_container_display_none;
          test "Toggle grid child display none"
            test_toggle_grid_child_display_none;
        ];
      group "relayout" [ test "Relayout is stable" test_relayout_stable ];
    ]
