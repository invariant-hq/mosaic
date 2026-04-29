(** Animated spinners with built-in presets. *)

open Mosaic

type model = { preset_idx : int; running : bool }
type msg = Next_preset | Toggle | Quit

let presets =
  [|
    (Spinner.dots, "Dots");
    (Spinner.line, "Line");
    (Spinner.circle, "Circle");
    (Spinner.bounce, "Bounce");
    (Spinner.arc, "Arc");
  |]

let init () = ({ preset_idx = 0; running = true }, Cmd.none)

let update msg model =
  match msg with
  | Next_preset ->
      let next_idx = (model.preset_idx + 1) mod Array.length presets in
      ({ model with preset_idx = next_idx }, Cmd.none)
  | Toggle -> ({ model with running = not model.running }, Cmd.none)
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

let view model =
  let frame_set, name = presets.(model.preset_idx) in
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Spinners";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              (* Current spinner with label *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 2)
                [
                  spinner ~frame_set ~live:model.running ~color:accent ();
                  text
                    (Printf.sprintf "%s %s" name
                       (if model.running then "(running)" else "(stopped)"));
                ];
              (* All presets in a row *)
              box ~flex_direction:Row ~gap:(gap 3)
                (Array.to_list
                   (Array.mapi
                      (fun i (fs, fs_name) ->
                        box ~flex_direction:Column ~align_items:Center
                          ~gap:(gap 1)
                          [
                            spinner ~frame_set:fs ~live:model.running ();
                            text
                              ~style:
                                (if i = model.preset_idx then
                                   Ansi.Style.make ~fg:accent ()
                                 else Ansi.Style.make ~dim:true ())
                              fs_name;
                          ])
                      presets));
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "n next  •  Space toggle  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char 'n', Next_preset);
      (Shortcut.space, Toggle);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
