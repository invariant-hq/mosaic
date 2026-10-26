(* Runs the cases that ../gentest/gentest.js generates from the HTML fixtures:
   build the tree, lay it out, and compare every node with the layout Chromium
   computed. *)

open Toffee
open Geometry

(* The text of a leaf, in the Ahem font of the fixtures: every character is a
   10px square, and lines break only between the words, which zero-width spaces
   separate in the fixture. *)
type text = { words : string list; vertical : bool }
type node = { style : Style.t; text : text option; children : node list }

let node style children = { style; text = None; children }

let text ?(vertical = false) style words =
  { style; text = Some { words; vertical }; children = [] }

let measure_text (known : float option size)
    (available : Available_space.t size) { words; vertical } : Sizef.t =
  let glyph = 10. in
  let longest = List.fold_left (fun n w -> max n (String.length w)) 0 words in
  let total = List.fold_left (fun n w -> n + String.length w) 0 words in
  let inline_known, block_known, inline_available =
    if vertical then (known.height, known.width, available.height)
    else (known.width, known.height, available.width)
  in
  let inline =
    match (inline_known, inline_available) with
    | Some size, _ -> size
    | None, Min_content -> float longest *. glyph
    | None, Max_content -> float total *. glyph
    | None, Definite size -> Float.min size (float total *. glyph)
  in
  let inline = Float.max inline (float longest *. glyph) in
  let block =
    match block_known with
    | Some size -> size
    | None ->
        let per_line = int_of_float (Float.floor (inline /. glyph)) in
        let lines, _ =
          List.fold_left
            (fun (lines, line) w ->
              let n = String.length w in
              if line + n > per_line then
                ((if line > 0 then lines + 1 else lines), n)
              else (lines, line + n))
            (1, 0) words
        in
        float lines *. glyph
  in
  if vertical then { width = block; height = inline }
  else { width = inline; height = block }

let measure (known : float option size) available _ text _ : Sizef.t =
  let measured : Sizef.t =
    match (known, text) with
    | { width = Some width; height = Some height }, _ -> { width; height }
    | _, None -> { width = 0.; height = 0. }
    | _, Some text -> measure_text known available text
  in
  {
    width = Option.value known.width ~default:measured.width;
    height = Option.value known.height ~default:measured.height;
  }

(* The ids of [node] and its descendants, in document order. *)
let rec build tree node =
  let children = List.map (build tree) node.children in
  let id =
    match (node.text, children) with
    | Some text, _ -> new_leaf_with_context tree node.style text
    | None, [] -> new_leaf tree node.style
    | None, _ ->
        new_with_children tree node.style
          (Array.of_list (List.map List.hd children))
  in
  Result.get_ok id :: List.concat children

(* [expected] has a row per node, in document order: x, y, width, height, and
   for a scroll container its scroll width and height. *)
let check ~rounding ~available root expected () =
  let tree = new_tree () in
  let tree = if rounding then tree else disable_rounding tree in
  let ids = build tree root in
  let root = List.hd ids in
  compute_layout_with_measure tree root available measure |> Result.get_ok;
  print_tree tree root;
  let float = Windtrap.Testable.float (if rounding then 0.001 else 0.1) in
  List.iteri
    (fun i (id, row) ->
      let layout = layout tree id |> Result.get_ok in
      let Pointf.{ x; y } = Layout.location layout in
      let Sizef.{ width; height } = Layout.size layout in
      let actual =
        [|
          ("x", x);
          ("y", y);
          ("width", width);
          ("height", height);
          ("scroll_width", Layout.scroll_width layout);
          ("scroll_height", Layout.scroll_height layout);
        |]
      in
      Array.iteri
        (fun j expected ->
          let what, actual = actual.(j) in
          Windtrap.equal
            ~msg:(Printf.sprintf "%s of node%d" what i)
            float expected actual)
        row)
    (List.combine ids expected)

let case ?(rounding = true) ?width ?height name tree ~border_box ~content_box =
  let space = function
    | Some size -> Available_space.Definite size
    | None -> Max_content
  in
  let available : _ size = { width = space width; height = space height } in
  let test box_sizing label expected =
    Windtrap.test
      (Printf.sprintf "%s (%s)" name label)
      (check ~rounding ~available (tree box_sizing) expected)
  in
  [
    test Style.Box_sizing.Border_box "border-box" border_box;
    test Content_box "content-box" content_box;
  ]

let run category cases =
  Windtrap.run "Toffee Layout Tests"
    [ Windtrap.group category (List.concat cases) ]
