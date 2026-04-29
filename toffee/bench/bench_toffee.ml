open Toffee
module S = Style
module G = Geometry

let unwrap = function Ok v -> v | Error e -> failwith (Error.to_string e)
let px = S.Dimension.length
let pct = S.Dimension.percent
let lp = S.Length_percentage.length
let lp_auto = S.Length_percentage_auto.length
let gap v = G.Size.{ width = lp v; height = lp v }
let padding v = G.Rect.all (lp v)
let margin_auto v = G.Rect.all (lp_auto v)

let available ~width ~height =
  G.Size.
    {
      width = Available_space.of_float width;
      height = Available_space.of_float height;
    }

let run_layout tree root available_space =
  unwrap (mark_dirty tree root);
  unwrap (compute_layout tree root available_space)

let leaf_style rng =
  let width = 60. +. Random.State.float rng 140. in
  let height = 24. +. Random.State.float rng 60. in
  let grow = 0.2 +. Random.State.float rng 1.1 in
  S.make
    ~size:G.Size.{ width = px width; height = px height }
    ~flex_grow:grow ~flex_shrink:1.0 ~margin:(margin_auto 2.) ()

let flex_container_style rng direction =
  let gap_px = 6. +. Random.State.float rng 6. in
  let pad = 2. +. Random.State.float rng 4. in
  S.make ~display:S.Display.Flex ~flex_direction:direction ~gap:(gap gap_px)
    ~padding:(padding pad) ()

let grid_container_style ~columns ~row_px ~gap_px =
  S.make ~display:S.Display.Grid ~grid_template_columns:columns
    ~grid_auto_rows:[ S.Track_sizing_function.length row_px ]
    ~grid_auto_flow:S.Grid_auto_flow.Row_dense ~gap:(gap gap_px) ()

let build_deep_flex tree rng ~depth ~branching =
  let rec build level =
    if level = depth then unwrap (new_leaf tree (leaf_style rng))
    else
      let children = Array.init branching (fun _ -> build (level + 1)) in
      let direction =
        if level mod 2 = 0 then S.Flex_direction.Row
        else S.Flex_direction.Column
      in
      let style = flex_container_style rng direction in
      unwrap (new_with_children tree style children)
  in
  build 0

let build_wide_flex tree rng ~rows ~cols =
  let row_style =
    S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Row
      ~flex_wrap:S.Flex_wrap.Wrap ~gap:(gap 8.) ~padding:(padding 4.) ()
  in
  let row_children () =
    Array.init cols (fun _ ->
        let basis =
          if Random.State.bool rng then pct 0.2
          else px (80. +. Random.State.float rng 80.)
        in
        let style =
          S.make
            ~size:
              G.Size.
                {
                  width = basis;
                  height = px (60. +. Random.State.float rng 40.);
                }
            ~flex_grow:1.0 ~flex_shrink:1.0 ~margin:(margin_auto 2.) ()
        in
        unwrap (new_leaf tree style))
  in
  let rows =
    Array.init rows (fun _ ->
        let children = row_children () in
        unwrap (new_with_children tree row_style children))
  in
  let root_style =
    S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Column
      ~gap:(gap 12.) ~padding:(padding 8.) ()
  in
  unwrap (new_with_children tree root_style rows)

let build_grid_gallery tree rng ~items =
  let columns =
    let repetition =
      S.Grid_repetition.make
        ~count:(S.Repetition_count.count 4)
        ~tracks:[ S.Track_sizing_function.fr 1. ]
        ~line_names:[ []; [] ]
    in
    [ S.Grid_template_component.repeat repetition ]
  in
  let root_style = grid_container_style ~columns ~row_px:180. ~gap_px:10. in
  let make_item () =
    let ratio = 1.2 +. Random.State.float rng 0.8 in
    let style =
      S.make ~aspect_ratio:ratio
        ~size:G.Size.{ width = S.Dimension.auto; height = S.Dimension.auto }
        ~padding:(padding 4.) ()
    in
    unwrap (new_leaf tree style)
  in
  let children = Array.init items (fun _ -> make_item ()) in
  unwrap (new_with_children tree root_style children)

let build_mixed_dashboard tree rng =
  let header_item label_width =
    let style =
      S.make
        ~size:G.Size.{ width = px label_width; height = px 48. }
        ~margin:(margin_auto 2.) ()
    in
    unwrap (new_leaf tree style)
  in
  let header_style =
    S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Row
      ~gap:(gap 8.) ~padding:(padding 6.) ()
  in
  let header =
    let children =
      [|
        header_item 140.; header_item 220.; header_item 120.; header_item 180.;
      |]
    in
    unwrap (new_with_children tree header_style children)
  in

  let toolbar_style =
    S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Row
      ~gap:(gap 6.) ~padding:(padding 4.) ()
  in
  let toolbar =
    let children =
      Array.init 6 (fun _ ->
          let w = 90. +. Random.State.float rng 40. in
          header_item w)
    in
    unwrap (new_with_children tree toolbar_style children)
  in

  let cards =
    let columns =
      [
        S.Grid_template_component.length 220.;
        S.Grid_template_component.fr 1.;
        S.Grid_template_component.fr 1.;
      ]
    in
    let grid_style = grid_container_style ~columns ~row_px:160. ~gap_px:12. in
    let card count =
      Array.init count (fun _ ->
          let ratio = 1.1 +. Random.State.float rng 0.6 in
          let style =
            S.make ~aspect_ratio:ratio ~padding:(padding 6.)
              ~margin:(margin_auto 3.) ()
          in
          unwrap (new_leaf tree style))
    in
    let children = card 18 in
    unwrap (new_with_children tree grid_style children)
  in

  let activity_feed =
    let row_style =
      S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Row
        ~gap:(gap 4.) ~padding:(padding 3.) ()
    in
    let row () =
      let avatar = header_item 48. in
      let text =
        let style =
          S.make
            ~size:G.Size.{ width = pct 1.0; height = px 42. }
            ~flex_grow:1.0 ~padding:(padding 2.) ()
        in
        unwrap (new_leaf tree style)
      in
      unwrap (new_with_children tree row_style [| avatar; text |])
    in
    let rows = Array.init 12 (fun _ -> row ()) in
    let feed_style =
      S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Column
        ~gap:(gap 6.) ~padding:(padding 6.) ()
    in
    unwrap (new_with_children tree feed_style rows)
  in

  let body_style =
    let columns =
      [ S.Grid_template_component.fr 3.; S.Grid_template_component.fr 2. ]
    in
    grid_container_style ~columns ~row_px:320. ~gap_px:18.
  in
  let body =
    unwrap (new_with_children tree body_style [| cards; activity_feed |])
  in

  let root_style =
    S.make ~display:S.Display.Flex ~flex_direction:S.Flex_direction.Column
      ~gap:(gap 14.) ~padding:(padding 10.) ()
  in
  unwrap (new_with_children tree root_style [| header; toolbar; body |])

let deep_flex_benchmark =
  let available_space = available ~width:1280. ~height:900. in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let tree = new_tree () in
      let rng = Random.State.make [| 42 |] in
      let root = build_deep_flex tree rng ~depth:7 ~branching:3 in
      (tree, root))
    "flex/deep-hierarchy"
    (fun (tree, root) -> run_layout tree root available_space)

let wide_flex_benchmark =
  let available_space = available ~width:1440. ~height:900. in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let tree = new_tree () in
      let rng = Random.State.make [| 7 |] in
      let root = build_wide_flex tree rng ~rows:16 ~cols:12 in
      (tree, root))
    "flex/wide-dashboard"
    (fun (tree, root) -> run_layout tree root available_space)

let grid_gallery_benchmark =
  let available_space = available ~width:1200. ~height:900. in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let tree = new_tree () in
      let rng = Random.State.make [| 99 |] in
      let root = build_grid_gallery tree rng ~items:72 in
      (tree, root))
    "grid/auto-placement-gallery"
    (fun (tree, root) -> run_layout tree root available_space)

let mixed_dashboard_benchmark =
  let available_space = available ~width:1366. ~height:900. in
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let tree = new_tree () in
      let rng = Random.State.make [| 2024 |] in
      let root = build_mixed_dashboard tree rng in
      (tree, root))
    "mixed/dashboard"
    (fun (tree, root) -> run_layout tree root available_space)

let () =
  Thumper.run "toffee"
    [
      deep_flex_benchmark;
      wide_flex_benchmark;
      grid_gallery_benchmark;
      mixed_dashboard_benchmark;
    ]
