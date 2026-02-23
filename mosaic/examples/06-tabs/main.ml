(** Horizontal tab bar with navigation. *)

open Mosaic

type model = { tab : int }
type msg = Quit | Tab_changed of int

let tabs : Tab_select.item list =
  [
    { label = "Home"; description = "Main dashboard" };
    { label = "Files"; description = "Browse files" };
    { label = "Settings"; description = "Configure options" };
    { label = "Network"; description = "Connection status" };
    { label = "Logs"; description = "System logs" };
    { label = "Help"; description = "Documentation" };
  ]

let init () = ({ tab = 0 }, Cmd.none)

let update msg model =
  match msg with
  | Quit -> (model, Cmd.quit)
  | Tab_changed i -> ({ tab = i }, Cmd.none)

let content_for_tab = function
  | 0 -> "Welcome to the Home tab. This is the main dashboard."
  | 1 -> "File browser content would appear here."
  | 2 -> "Application settings and preferences."
  | 3 -> "Network connection status and diagnostics."
  | 4 -> "System logs and event history."
  | 5 -> "Help documentation and user guide."
  | _ -> "Unknown tab"

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Tabs";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Tab bar *)
      box ~padding:(padding 1)
        [
          tab_select ~autofocus:true ~show_description:true ~show_underline:true
            ~show_scroll_arrows:true ~wrap_selection:true ~tab_width:12
            ~selected_background:accent ~selected_text_color:Ansi.Color.black
            ~on_change:(fun i -> Some (Tab_changed i))
            tabs;
        ];
      (* Content area *)
      box ~flex_grow:1. ~border:true ~border_color ~padding:(padding 2)
        ~margin:(margin 1)
        [
          box ~flex_direction:Column ~gap:(gap 1)
            [
              text
                ~style:(Ansi.Style.make ~bold:true ())
                (List.nth tabs model.tab).label;
              text (content_for_tab model.tab);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "←/→ navigate  •  [/] vim  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
