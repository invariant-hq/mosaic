(** Hierarchical tree view with expand/collapse and guide lines. *)

open Mosaic

type guide_style_kind = Rounded | Single | Heavy | Double

type model = {
  selected : int;
  show_guides : bool;
  guide_style_kind : guide_style_kind;
  activated : string option;
}

type msg =
  | Select of int
  | Activate of int
  | Expand of int * bool
  | Toggle_guides
  | Toggle_guide_style
  | Quit

let file_tree =
  let open Tree in
  [
    item
      ~children:
        [
          item ~children:[ item "main.ml"; item "main.mli"; item "dune" ] "src";
          item
            ~children:
              [ item "test_parser.ml"; item "test_lexer.ml"; item "dune" ]
            "test";
          item ~children:[ item "index.mld"; item "tutorial.mld" ] "doc";
          item "README.md";
          item "CHANGES.md";
          item "LICENSE";
          item "dune-project";
        ]
      "my-project";
    item
      ~children:
        [
          item ~children:[ item "lib.ml"; item "lib.mli" ] "lib";
          item "dune-project";
        ]
      "my-lib";
  ]

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let guide_style_of = function
  | Rounded -> Border.rounded
  | Single -> Border.single
  | Heavy -> Border.heavy
  | Double -> Border.double

let guide_style_name = function
  | Rounded -> "rounded"
  | Single -> "single"
  | Heavy -> "heavy"
  | Double -> "double"

let next_guide_style = function
  | Rounded -> Single
  | Single -> Heavy
  | Heavy -> Double
  | Double -> Rounded

let init () =
  ( {
      selected = 0;
      show_guides = true;
      guide_style_kind = Rounded;
      activated = None;
    },
    Cmd.none )

let item_label_at items idx =
  let flat = ref [] in
  let rec walk items =
    List.iter
      (fun (it : Tree.item) ->
        flat := it.label :: !flat;
        walk it.children)
      items
  in
  walk items;
  let arr = Array.of_list (List.rev !flat) in
  if idx >= 0 && idx < Array.length arr then Some arr.(idx) else None

let update msg model =
  match msg with
  | Select i -> ({ model with selected = i; activated = None }, Cmd.none)
  | Activate i ->
      let label = item_label_at file_tree i in
      ({ model with activated = label }, Cmd.none)
  | Expand _ -> (model, Cmd.none)
  | Toggle_guides ->
      ({ model with show_guides = not model.show_guides }, Cmd.none)
  | Toggle_guide_style ->
      ( { model with guide_style_kind = next_guide_style model.guide_style_kind },
        Cmd.none )
  | Quit -> (model, Cmd.quit)

let view model =
  let guide_style = guide_style_of model.guide_style_kind in
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Tree";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~flex_direction:Row ~gap:(gap 2) ~flex_grow:1.
            [
              (* Tree *)
              box ~border:true ~border_color ~flex_grow:1.
                [
                  tree ~autofocus:true ~items:file_tree
                    ~selected_index:model.selected ~expand_depth:(-1)
                    ~show_guides:model.show_guides ~guide_style ~indent_size:2
                    ~guide_color:(Ansi.Color.grayscale ~level:10)
                    ~icon_color:Ansi.Color.cyan ~wrap_selection:true
                    ~size:{ width = pct 100; height = pct 100 }
                    ~on_change:(fun i -> Some (Select i))
                    ~on_activate:(fun i -> Some (Activate i))
                    ~on_expand:(fun i expanded -> Some (Expand (i, expanded)))
                    ();
                ];
              (* Info panel *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = px 28; height = auto }
                [
                  text ~style:(Ansi.Style.make ~bold:true ()) "Info";
                  text ~style:hint
                    (Printf.sprintf "Selected: %d" model.selected);
                  text ~style:hint
                    (Printf.sprintf "Guides: %s"
                       (if model.show_guides then "on" else "off"));
                  text ~style:hint
                    (Printf.sprintf "Style: %s"
                       (guide_style_name model.guide_style_kind));
                  (match model.activated with
                  | Some label ->
                      text
                        ~style:
                          (Ansi.Style.make ~fg:Ansi.Color.green ~bold:true ())
                        (Printf.sprintf "Opened: %s" label)
                  | None -> text ~style:hint "Enter to open");
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "↑/↓ navigate  •  ←/→ collapse/expand  •  g guides  •  s style  •  \
             q quit";
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'g') -> Some Toggle_guides
      | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Toggle_guide_style
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
