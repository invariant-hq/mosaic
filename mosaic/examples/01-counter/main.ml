(** Simple counter demonstrating TEA basics. *)

open Mosaic

type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Counter";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~align_items:Center ~gap:(gap 2)
            ~border:true ~border_color ~padding:(padding 3)
            [
              text
                ~style:(Ansi.Style.make ~bold:true ())
                (Printf.sprintf "%d" model);
              text ~style:hint "Press + or - to change";
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "+ increment  •  - decrement  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char '+', Increment);
      (Shortcut.char '-', Decrement);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
