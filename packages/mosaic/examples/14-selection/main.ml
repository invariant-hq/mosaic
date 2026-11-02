(** Text selection across renderables and scroll boxes.

    Drag to select, double-click for a word, triple-click for a line; drag after
    either to extend by words or lines. Shift+arrows extend the selection,
    Ctrl+C copies it and Escape clears it. The application copies on select, and
    shows each copy in the footer. *)

open Mosaic

type model = { scroll : Scroll.t; copied : int option }
type msg = Quit | Scrolled of Scroll.t | Copy_selection | Copied of string

let init () = ({ scroll = Scroll.at 0; copied = None }, Cmd.none)

let update msg model =
  match msg with
  | Quit -> (model, Cmd.quit)
  | Scrolled scroll -> ({ model with scroll }, Cmd.none)
  | Copy_selection -> (model, Cmd.copy_selection)
  | Copied text -> ({ model with copied = Some (String.length text) }, Cmd.none)

let section1_style = Ansi.Style.make ~fg:Ansi.Color.white ()

let lorem_lines =
  [|
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
    "Proin dictum rutrum mi, ac egestas elit dictum ac.";
    "Aliquam erat volutpat. Nullam in nisi vitae turpis consequat.";
    "Sed posuere pretium metus, a posuere est consequat nec.";
    "Curabitur nec quam sed augue congue vestibulum.";
    "Suspendisse tincidunt, augue at rhoncus cursus.";
    "Nam molestie euismod faucibus. Quisque id odio in pede.";
    "Integer consequat, quam at congue cursus, magna eros pretium.";
    "Vivamus cursus, ex eu tincidunt cursus, libero massa dictum.";
    "Morbi auctor magna a ultricies consequat.";
  |]

let box_colors =
  [|
    Ansi.Color.grayscale ~level:4;
    Ansi.Color.of_rgb 100 50 60;
    Ansi.Color.of_rgb 50 100 60;
    Ansi.Color.of_rgb 60 60 100;
    Ansi.Color.of_rgb 100 100 50;
    Ansi.Color.of_rgb 100 60 100;
    Ansi.Color.of_rgb 60 100 100;
    Ansi.Color.grayscale ~level:6;
  |]

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view { scroll; copied } =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg ~selectable:`Off
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Text Selection";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Main content area *)
      box ~flex_grow:1.
        ~min_size:{ width = px 0; height = px 0 }
        ~flex_direction:Row ~gap:(gap 1) ~padding:(padding 1)
        [
          (* Left panel: Document sections *)
          box ~flex_grow:1. ~flex_direction:Column ~gap:(gap 1)
            [
              (* Section 1: Simple text selection *)
              box ~border:true ~padding:(padding 1)
                ~background:(Ansi.Color.grayscale ~level:2)
                ~flex_direction:Column ~gap:(gap 0) ~title:"Document Section 1"
                [
                  text ~style:section1_style
                    "This is a paragraph in the first box.";
                  text ~style:section1_style
                    "It contains multiple lines of text.";
                  text ~style:section1_style
                    "You can select across these lines.";
                  text ~style:section1_style "Unicode: 世界, 你好世界, 中文, 한글";
                ];
              (* Section 2: Code example *)
              box ~border:true ~padding:(padding 1)
                ~background:(Ansi.Color.grayscale ~level:3)
                ~border_color:(Ansi.Color.of_rgb 248 81 73)
                ~flex_direction:Column ~title:"Code Example"
                [
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
                    "function handleSelection() {";
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.white ())
                    "  const selected = getSelectedText()";
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
                    "  console.log(selected)";
                  text ~style:(Ansi.Style.make ~fg:Ansi.Color.white ()) "}";
                ];
            ];
          (* Right panel: Scrollable content *)
          box ~flex_grow:1.
            ~min_size:{ width = px 0; height = px 0 }
            ~flex_direction:Column
            [
              box ~border:true ~flex_grow:1.
                ~min_size:{ width = px 0; height = px 0 }
                ~background:(Ansi.Color.grayscale ~level:1)
                ~title:"Scrollable Content"
                [
                  scroll_box ~scroll
                    ~on_scroll:(fun value -> Scrolled value)
                    ~size:{ width = pct 100; height = pct 100 }
                    ~scroll_y:true ~scroll_x:false
                    (List.init 16 (fun i ->
                         let num_lines = 2 + (i mod 4) in
                         let bg = box_colors.(i mod Array.length box_colors) in
                         box ~padding:(padding 1) ~background:bg
                           ~flex_direction:Column ~key:(string_of_int i)
                           [
                             text
                               ~style:
                                 (Ansi.Style.make ~bold:true
                                    ~fg:Ansi.Color.white ())
                               (Printf.sprintf "Box %d" (i + 1));
                             fragment
                               (List.init num_lines (fun j ->
                                    let line_idx =
                                      ((i * 3) + j) mod Array.length lorem_lines
                                    in
                                    text
                                      ~style:
                                        (Ansi.Style.make ~fg:Ansi.Color.white ())
                                      ~key:(string_of_int j)
                                      lorem_lines.(line_idx)));
                           ]));
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg ~selectable:`Off
        [
          text ~style:hint
            (match copied with
            | Some n -> Printf.sprintf "copied %d bytes  •  q quit" n
            | None ->
                "drag, double or triple click to select  •  Ctrl+C copy  •  \
                 Esc clear  •  q quit");
        ];
    ]

let subscriptions _ =
  Sub.batch
    [
      Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ];
      Sub.on_selection (function Some _ -> Some Copy_selection | None -> None);
      Sub.on_copy (fun text -> Some (Copied text));
    ]

let () = run { init; update; view; subscriptions }
