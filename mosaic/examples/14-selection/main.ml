(** Text selection across renderables and scroll boxes. *)

open Mosaic

type msg = Quit

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)
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

let view () =
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Text Selection";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Main content area *)
      box ~flex_grow:1. ~flex_direction:Row ~gap:(gap 1) ~padding:(padding 1)
        [
          (* Left panel: Document sections *)
          box ~flex_grow:1. ~flex_direction:Column ~gap:(gap 1)
            [
              (* Section 1: Simple text selection *)
              box ~border:true ~title:"Document Section 1" ~padding:(padding 1)
                ~background:(Ansi.Color.grayscale ~level:2)
                ~flex_direction:Column ~gap:(gap 0)
                [
                  text ~style:section1_style ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 88 166 255)
                    ~selection_fg:Ansi.Color.white
                    "This is a paragraph in the first box.";
                  text ~style:section1_style ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 88 166 255)
                    ~selection_fg:Ansi.Color.white
                    "It contains multiple lines of text.";
                  text ~style:section1_style ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 88 166 255)
                    ~selection_fg:Ansi.Color.white
                    "You can select across these lines.";
                  text ~style:section1_style ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 88 166 255)
                    ~selection_fg:Ansi.Color.white "Unicode: 世界, 你好世界, 中文, 한글";
                ];
              (* Section 2: Code example *)
              box ~border:true ~title:"Code Example" ~padding:(padding 1)
                ~background:(Ansi.Color.grayscale ~level:3)
                ~border_color:(Ansi.Color.of_rgb 248 81 73)
                ~flex_direction:Column
                [
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
                    ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 74 85 104)
                    "function handleSelection() {";
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.white ())
                    ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 74 85 104)
                    "  const selected = getSelectedText()";
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
                    ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 74 85 104)
                    "  console.log(selected)";
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.white ())
                    ~selectable:true
                    ~selection_bg:(Ansi.Color.of_rgb 74 85 104)
                    "}";
                ];
            ];
          (* Right panel: Scrollable content *)
          box ~flex_grow:1. ~flex_direction:Column
            [
              box ~border:true ~title:"Scrollable Content" ~flex_grow:1.
                ~background:(Ansi.Color.grayscale ~level:1)
                [
                  scroll_box ~scroll_y:true ~scroll_x:false
                    ~size:{ width = pct 100; height = pct 100 }
                    (List.init 16 (fun i ->
                         let num_lines = 2 + (i mod 4) in
                         let bg = box_colors.(i mod Array.length box_colors) in
                         box ~key:(string_of_int i) ~padding:(padding 1)
                           ~background:bg ~flex_direction:Column
                           [
                             text
                               ~style:
                                 (Ansi.Style.make ~bold:true
                                    ~fg:Ansi.Color.white ())
                               ~selectable:true
                               ~selection_bg:(Ansi.Color.of_rgb 122 162 247)
                               (Printf.sprintf "Box %d" (i + 1));
                             fragment
                               (List.init num_lines (fun j ->
                                    let line_idx =
                                      ((i * 3) + j) mod Array.length lorem_lines
                                    in
                                    text ~key:(string_of_int j)
                                      ~style:
                                        (Ansi.Style.make ~fg:Ansi.Color.white ())
                                      ~selectable:true
                                      ~selection_bg:
                                        (Ansi.Color.of_rgb 122 162 247)
                                      lorem_lines.(line_idx)));
                           ]));
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "click and drag to select  •  Ctrl+click extend  •  q quit";
        ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
