(** Syntax-highlighted code with Tree-sitter. *)

open Mosaic

type lang = OCaml | JSON
type model = { lang : lang }
type msg = Toggle_lang | Quit

let init () = ({ lang = OCaml }, Cmd.none)

let update msg model =
  match msg with
  | Toggle_lang ->
      let lang = match model.lang with OCaml -> JSON | JSON -> OCaml in
      ({ lang }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let ocaml_sample =
  {|let greet name =
  Printf.printf "Hello, %s!\n" name

type color = Red | Green | Blue

let rgb_of_color = function
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)

let () =
  greet "World";
  let r, g, b = rgb_of_color Blue in
  Printf.printf "RGB: (%d, %d, %d)\n" r g b|}

let json_sample =
  {|{
  "name": "mosaic",
  "version": "0.1.0",
  "description": "TUI library for OCaml",
  "keywords": ["terminal", "tui", "ocaml"],
  "dependencies": {
    "matrix": "^1.0.0",
    "toffee": "^1.0.0"
  },
  "author": {
    "name": "Anonymous",
    "email": "dev@example.com"
  },
  "license": "MIT",
  "active": true,
  "stars": 42
}|}

let lang_name = function OCaml -> "OCaml" | JSON -> "JSON"
let lang_content = function OCaml -> ocaml_sample | JSON -> json_sample

let highlight_fn = function
  | OCaml -> Tree_sitter_ocaml.highlight_ocaml
  | JSON -> Tree_sitter_json.highlight

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  let content = lang_content model.lang in
  let spans =
    highlight_fn model.lang content
    |> Syntax_highlight.of_triples
    |> Syntax_highlight.to_spans ~style:Syntax_style.default ~content
  in
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
              text
                ~style:(Ansi.Style.make ~bold:true ())
                (Printf.sprintf "▸ Code (%s)" (lang_name model.lang));
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Code display *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~border:true ~border_color ~flex_grow:1.
            [ code ~spans ~size:{ width = pct 100; height = pct 100 } content ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "l toggle language  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char 'l', Toggle_lang);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
