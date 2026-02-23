(** Vertical list selection with keyboard navigation. *)

open Mosaic

type msg = Quit

let languages : Select.item list =
  [
    { label = "OCaml"; description = Some "Functional, type-safe" };
    { label = "Rust"; description = Some "Safe systems programming" };
    { label = "Haskell"; description = Some "Pure functional" };
    { label = "TypeScript"; description = Some "Typed JavaScript" };
    { label = "Python"; description = Some "Easy to learn" };
    { label = "Go"; description = Some "Fast compilation" };
    { label = "Zig"; description = Some "Low-level control" };
    { label = "Elixir"; description = Some "Concurrent, fault-tolerant" };
  ]

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Select";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "Choose a language:";
              (* Select component *)
              select ~autofocus:true ~show_description:true
                ~show_scroll_indicator:true ~wrap_selection:true
                ~selected_background:accent
                ~selected_text_color:Ansi.Color.black
                ~selected_description_color:(Ansi.Color.of_rgb 0 50 70)
                ~description_color:(Ansi.Color.grayscale ~level:14)
                ~focused_background:(Ansi.Color.of_rgb 20 30 40)
                ~size:{ width = px 40; height = px 10 }
                languages;
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "↑/↓ navigate  •  j/k vim  •  q quit" ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
