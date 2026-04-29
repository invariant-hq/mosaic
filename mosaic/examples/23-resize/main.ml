(** Terminal resize and focus events. *)

open Mosaic

type model = {
  width : int;
  height : int;
  focused : bool;
  resize_count : int;
  resize_history : (int * int) list;
}

type msg = Resize of int * int | Focus | Blur | Quit

let init () =
  ( {
      width = 80;
      height = 24;
      focused = true;
      resize_count = 0;
      resize_history = [];
    },
    Cmd.none )

let update msg model =
  match msg with
  | Resize (width, height) ->
      let history = (width, height) :: model.resize_history in
      let history =
        if List.length history > 5 then List.filteri (fun i _ -> i < 5) history
        else history
      in
      ( {
          model with
          width;
          height;
          resize_count = model.resize_count + 1;
          resize_history = history;
        },
        Cmd.none )
  | Focus -> ({ model with focused = true }, Cmd.none)
  | Blur -> ({ model with focused = false }, Cmd.none)
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let column_box color label =
  box ~flex_grow:1. ~background:color ~padding:(padding 1) ~align_items:Center
    ~justify_content:Center
    ~size:{ width = auto; height = px 5 }
    [ text ~style:(Ansi.Style.make ~bold:true ()) label ]

let adaptive_columns width =
  let c1 = Ansi.Color.of_rgb 40 90 60 in
  let c2 = Ansi.Color.of_rgb 90 60 40 in
  let c3 = Ansi.Color.of_rgb 50 50 100 in
  if width >= 80 then
    box ~flex_direction:Column ~gap:(gap 1)
      [
        text ~style:(Ansi.Style.make ~bold:true ()) "Wide mode (>= 80 cols)";
        box ~flex_direction:Row ~gap:(gap 1)
          ~size:{ width = pct 100; height = auto }
          [
            column_box c1 "Column 1";
            column_box c2 "Column 2";
            column_box c3 "Column 3";
          ];
      ]
  else if width >= 40 then
    box ~flex_direction:Column ~gap:(gap 1)
      [
        text ~style:(Ansi.Style.make ~bold:true ()) "Normal mode (>= 40 cols)";
        box ~flex_direction:Row ~gap:(gap 1)
          ~size:{ width = pct 100; height = auto }
          [ column_box c1 "Column 1"; column_box c2 "Column 2" ];
      ]
  else
    box ~flex_direction:Column ~gap:(gap 1)
      [
        text ~style:(Ansi.Style.make ~bold:true ()) "Narrow mode (< 40 cols)";
        column_box c1 "Column 1";
      ]

let view model =
  let focus_color =
    if model.focused then Ansi.Color.green else Ansi.Color.red
  in
  let focus_label = if model.focused then "Focused" else "Blurred" in
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Resize";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            ~size:{ width = px 60; height = auto }
            [
              (* Dimensions and focus status *)
              box ~flex_direction:Row ~justify_content:Space_between
                ~size:{ width = pct 100; height = auto }
                [
                  text
                    ~style:(Ansi.Style.make ~bold:true ())
                    (Printf.sprintf "Terminal: %d x %d" model.width model.height);
                  text
                    ~style:(Ansi.Style.make ~bold:true ~fg:focus_color ())
                    focus_label;
                ];
              (* Resize counter *)
              text ~style:muted
                (Printf.sprintf "Resized %d time%s" model.resize_count
                   (if model.resize_count = 1 then "" else "s"));
              (* Resize history *)
              box ~flex_direction:Column ~gap:(gap 0)
                (text
                   ~style:(Ansi.Style.make ~bold:true ())
                   "Recent resize events:"
                ::
                (if model.resize_history = [] then
                   [ text ~style:hint "  (none yet)" ]
                 else
                   List.mapi
                     (fun i (w, h) ->
                       text ~style:hint
                         (Printf.sprintf "  #%d  %d x %d"
                            (model.resize_count - i) w h))
                     model.resize_history));
              (* Adaptive columns *)
              adaptive_columns model.width;
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "q quit  •  resize your terminal to see changes" ];
    ]

let subscriptions _model =
  Sub.batch
    [
      Sub.on_resize (fun ~width ~height -> Resize (width, height));
      Sub.on_focus Focus;
      Sub.on_blur Blur;
      Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ];
    ]

let () = run { init; update; view; subscriptions }
