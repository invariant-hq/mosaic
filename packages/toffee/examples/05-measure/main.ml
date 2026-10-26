(** Leaves measured by a function that reads each node's context: a text node
    that wraps to the space available, and an image. *)

open Toffee

let or_fail = function
  | Ok v -> v
  | Error e ->
      prerr_endline (Error.to_string e);
      Stdlib.exit 1

(* Text measurement helpers *)

let lorem_ipsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
   tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
   veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
   commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
   velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat \
   cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id \
   est laborum."

type font_metrics = { char_width : float; char_height : float }
type text_context = { text_content : string }

let text_measure_function (known_dimensions : float option Geometry.size)
    (available_space : Available_space.t Geometry.size)
    (text_context : text_context) (metrics : font_metrics) : Geometry.Sizef.t =
  let open Geometry in
  let inline_axis = Absolute_axis.Horizontal in
  let block_axis = Absolute_axis.other inline_axis in

  let words =
    text_context.text_content |> String.split_on_char ' '
    |> List.filter (fun s -> String.length s > 0)
  in
  if words = [] then Sizef.zero
  else
    let min_line_length =
      List.fold_left (fun acc word -> max acc (String.length word)) 0 words
    in
    let max_line_length =
      List.fold_left (fun acc word -> acc + String.length word) 0 words
    in

    let known_inline = Size.get_absolute inline_axis known_dimensions in
    let inline_size =
      match known_inline with
      | Some width -> width
      | None -> (
          let available_inline =
            Size.get_absolute inline_axis available_space
          in
          let min_width = float_of_int min_line_length *. metrics.char_width in
          let max_width = float_of_int max_line_length *. metrics.char_width in
          match available_inline with
          | Available_space.Min_content -> min_width
          | Max_content -> max_width
          | Definite value ->
              let value = Float.min value max_width in
              Float.max value min_width)
    in

    let known_block = Size.get_absolute block_axis known_dimensions in
    let block_size =
      match known_block with
      | Some height -> height
      | None ->
          let inline_line_length =
            inline_size /. metrics.char_width |> Float.floor |> int_of_float
          in
          let rec count_lines current_length line_count = function
            | [] -> line_count
            | word :: rest ->
                let word_len = String.length word in
                if current_length = 0 then count_lines word_len line_count rest
                else if current_length + word_len + 1 > inline_line_length then
                  count_lines word_len (line_count + 1) rest
                else count_lines (current_length + word_len + 1) line_count rest
          in
          let line_count = count_lines 0 1 words in
          float_of_int line_count *. metrics.char_height
    in
    Sizef.{ width = inline_size; height = block_size }

(* Image measurement helpers *)

type image_context = { width : float; height : float }

let image_measure_function (known_dimensions : float option Geometry.size)
    (ctx : image_context) : Geometry.Sizef.t =
  let open Geometry in
  match (known_dimensions.Size.width, known_dimensions.height) with
  | Some width, Some height -> Sizef.{ width; height }
  | Some width, None ->
      let height = width /. ctx.width *. ctx.height in
      Sizef.{ width; height }
  | None, Some height ->
      let width = height /. ctx.height *. ctx.width in
      Sizef.{ width; height }
  | None, None -> Sizef.{ width = ctx.width; height = ctx.height }

module Node_context = struct
  type t =
    | Text of { text : text_context }
    | Image of { image : image_context }
end

let measure_function (known_dimensions : float option Geometry.size)
    (available_space : Available_space.t Geometry.size) (_node_id : Node_id.t)
    (context : Node_context.t option) (_style : Style.t) : Geometry.Sizef.t =
  match known_dimensions with
  | { Geometry.Size.width = Some width; height = Some height } ->
      Geometry.Sizef.{ width; height }
  | _ -> (
      match context with
      | None -> Geometry.Sizef.zero
      | Some (Node_context.Text { text }) ->
          text_measure_function known_dimensions available_space text
            { char_width = 10.; char_height = 10. }
      | Some (Node_context.Image { image }) ->
          image_measure_function known_dimensions image)

let () =
  let tree : Node_context.t tree = new_tree () in
  let open Geometry in
  let open Style in
  let text_node =
    let text_context = { text_content = lorem_ipsum } in
    new_leaf_with_context tree Style.default
      (Node_context.Text { text = text_context })
    |> or_fail
  in

  let image_node =
    let image_context = { width = 400.; height = 300. } in
    new_leaf_with_context tree Style.default
      (Node_context.Image { image = image_context })
    |> or_fail
  in

  let root_style =
    Style.make ~display:Display.Flex ~flex_direction:Flex_direction.Column
      ~size:Size.{ width = Dimension.px 200.; height = Dimension.auto }
      ()
  in
  let root =
    new_with_children tree root_style [| text_node; image_node |] |> or_fail
  in

  let available =
    Size.
      {
        width = Available_space.max_content;
        height = Available_space.max_content;
      }
  in
  compute_layout_with_measure tree root available
    (fun known_dimensions available_space node_id context style ->
      measure_function known_dimensions available_space node_id context style)
  |> or_fail;

  print_tree tree root
