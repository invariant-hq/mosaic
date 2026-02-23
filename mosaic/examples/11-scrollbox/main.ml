(** Scrollable content with scroll bars. *)

open Mosaic

type msg = Quit

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)

let items =
  List.init 50 (fun i ->
      Printf.sprintf "%2d. Item number %d - This is a sample list item" (i + 1)
        (i + 1))

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Scroll Box";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Scrollable content *)
      box ~flex_grow:1.
        ~min_size:{ width = px 0; height = px 0 }
        ~padding:(padding 1)
        [
          box ~border:true ~border_color ~title:"Items" ~flex_grow:1.
            ~min_size:{ width = px 0; height = px 0 }
            [
              scroll_box ~scroll_y:true ~scroll_x:false
                ~min_size:{ width = px 0; height = px 0 }
                ~size:{ width = pct 100; height = pct 100 }
                (List.mapi
                   (fun i item ->
                     box ~key:(string_of_int i) ~padding:(padding 1)
                       ~background:
                         (if i mod 2 = 0 then Ansi.Color.default
                          else Ansi.Color.grayscale ~level:3)
                       [ text item ])
                   items);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "scroll with mouse wheel  •  q quit" ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
