(** Horizontal and vertical sliders with sub-cell precision. *)

open Mosaic

type model = {
  h_value : float;
  v_value : float;
  r : float;
  g : float;
  b : float;
}

type msg =
  | Reset
  | Set_h of float
  | Set_v of float
  | Set_r of float
  | Set_g of float
  | Set_b of float
  | Quit

let default_hv = 50.
let default_rgb = 128.

let init () =
  ( {
      h_value = default_hv;
      v_value = default_hv;
      r = default_rgb;
      g = default_rgb;
      b = default_rgb;
    },
    Cmd.none )

let update msg model =
  match msg with
  | Set_h v -> ({ model with h_value = v }, Cmd.none)
  | Set_v v -> ({ model with v_value = v }, Cmd.none)
  | Set_r v -> ({ model with r = v }, Cmd.none)
  | Set_g v -> ({ model with g = v }, Cmd.none)
  | Set_b v -> ({ model with b = v }, Cmd.none)
  | Reset ->
      ( {
          h_value = default_hv;
          v_value = default_hv;
          r = default_rgb;
          g = default_rgb;
          b = default_rgb;
        },
        Cmd.none )
  | Quit -> (model, Cmd.quit)

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Sliders";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              (* Horizontal slider *)
              box ~flex_direction:Column ~gap:(gap 1)
                [
                  text (Printf.sprintf "Horizontal: %.0f%%" model.h_value);
                  slider ~orientation:`Horizontal ~min:0. ~max:100.
                    ~value:model.h_value ~viewport_size:10.
                    ~track_color:(Ansi.Color.grayscale ~level:5)
                    ~thumb_color:accent
                    ~on_value_change:(fun v -> Some (Set_h v))
                    ~size:{ width = px 30; height = px 1 }
                    ();
                ];
              (* Vertical slider *)
              box ~flex_direction:Row ~gap:(gap 2) ~align_items:Center
                [
                  text (Printf.sprintf "Vertical: %.0f%%" model.v_value);
                  slider ~orientation:`Vertical ~min:0. ~max:100.
                    ~value:model.v_value ~viewport_size:10.
                    ~track_color:(Ansi.Color.grayscale ~level:5)
                    ~thumb_color:Ansi.Color.green
                    ~on_value_change:(fun v -> Some (Set_v v))
                    ~size:{ width = px 1; height = px 8 }
                    ();
                ];
              (* Color sliders *)
              box ~flex_direction:Column ~gap:(gap 1)
                [
                  text "RGB Color Picker:";
                  box ~flex_direction:Row ~gap:(gap 2)
                    [
                      (* Red slider *)
                      box ~flex_direction:Column ~align_items:Center
                        ~gap:(gap 1)
                        [
                          slider ~orientation:`Vertical ~min:0. ~max:255.
                            ~value:model.r
                            ~track_color:(Ansi.Color.grayscale ~level:3)
                            ~thumb_color:Ansi.Color.red
                            ~on_value_change:(fun v -> Some (Set_r v))
                            ~size:{ width = px 1; height = px 5 }
                            ();
                          text "R";
                        ];
                      (* Green slider *)
                      box ~flex_direction:Column ~align_items:Center
                        ~gap:(gap 1)
                        [
                          slider ~orientation:`Vertical ~min:0. ~max:255.
                            ~value:model.g
                            ~track_color:(Ansi.Color.grayscale ~level:3)
                            ~thumb_color:Ansi.Color.green
                            ~on_value_change:(fun v -> Some (Set_g v))
                            ~size:{ width = px 1; height = px 5 }
                            ();
                          text "G";
                        ];
                      (* Blue slider *)
                      box ~flex_direction:Column ~align_items:Center
                        ~gap:(gap 1)
                        [
                          slider ~orientation:`Vertical ~min:0. ~max:255.
                            ~value:model.b
                            ~track_color:(Ansi.Color.grayscale ~level:3)
                            ~thumb_color:Ansi.Color.blue
                            ~on_value_change:(fun v -> Some (Set_b v))
                            ~size:{ width = px 1; height = px 5 }
                            ();
                          text "B";
                        ];
                      (* Color preview box *)
                      box
                        ~background:
                          (Ansi.Color.of_rgb (int_of_float model.r)
                             (int_of_float model.g) (int_of_float model.b))
                        ~size:{ width = px 6; height = px 3 }
                        [];
                    ];
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "drag sliders  •  r reset  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'r') -> Some Reset
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
