(** Unified and split-view diff display. *)

open Mosaic

type model = {
  layout : Diff.layout;
  show_numbers : bool;
  wrap : Text_surface.wrap;
}

type msg = Toggle_layout | Toggle_numbers | Toggle_wrap | Quit

let sample_diff =
  String.concat "\n"
    [
      "--- a/lib/session.ml";
      "+++ b/lib/session.ml";
      "@@ -1,22 +1,29 @@";
      " type status =";
      "   | Waiting";
      "   | Running";
      "+  | Cancelled";
      "   | Done";
      " ";
      " type t = {";
      "   id : int;";
      "   status : status;";
      "-  started_at : float option;";
      "+  started_at : float;";
      "+  finished_at : float option;";
      " }";
      " ";
      "-let create id = { id; status = Waiting; started_at = None }";
      "+let create ~now id =";
      "+  { id; status = Waiting; started_at = now; finished_at = None }";
      " ";
      "-let start t now =";
      "-  { t with status = Running; started_at = Some now }";
      "+let start t =";
      "+  match t.status with";
      "+  | Waiting -> { t with status = Running }";
      "+  | Running | Cancelled | Done -> t";
      " ";
      "-let finish t = { t with status = Done }";
      "+let cancel t = { t with status = Cancelled }";
      "+";
      "+let finish t ~now = { t with status = Done; finished_at = Some now }";
      " ";
      " let is_active t =";
      "   match t.status with";
      "-  | Waiting | Running -> true";
      "-  | Done -> false";
      "+  | Waiting | Running -> true";
      "+  | Cancelled | Done -> false";
      "";
    ]

let patch = Diff.Patch.of_unified sample_diff

let init () =
  ({ layout = Diff.Unified; show_numbers = true; wrap = `None }, Cmd.none)

let update msg model =
  match msg with
  | Toggle_layout ->
      let layout =
        match model.layout with Diff.Unified -> Diff.Split | Split -> Unified
      in
      ({ model with layout }, Cmd.none)
  | Toggle_numbers ->
      ({ model with show_numbers = not model.show_numbers }, Cmd.none)
  | Toggle_wrap ->
      let wrap =
        match model.wrap with `None -> `Word | `Word | `Char -> `None
      in
      ({ model with wrap }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let layout_name = function Diff.Unified -> "Unified" | Split -> "Split"
let wrap_name = function `None -> "None" | `Word -> "Word" | `Char -> "Char"

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let diff_theme =
  {
    Diff.default_theme with
    added_bg = Ansi.Color.of_rgb 20 70 32;
    removed_bg = Ansi.Color.of_rgb 82 28 36;
    added_line_number_bg = Some (Ansi.Color.of_rgb 12 48 24);
    removed_line_number_bg = Some (Ansi.Color.of_rgb 58 18 24);
    line_number_bg = Some (Ansi.Color.grayscale ~level:2);
    line_number_fg = Ansi.Color.grayscale ~level:13;
  }

let diff_view model =
  match patch with
  | Ok patch ->
      diff ~layout:model.layout ~theme:diff_theme
        ~show_line_numbers:model.show_numbers ~wrap:model.wrap
        ~text_style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:18) ())
        ~size:{ width = pct 100; height = pct 100 }
        patch
  | Error message ->
      box ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = pct 100 }
        [
          text
            ~style:(Ansi.Style.make ~fg:Ansi.Color.red ~bold:true ())
            ("Error parsing diff: " ^ message);
          code ~wrap:`None
            ~text_style:
              (Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:18) ())
            ~flex_grow:1. sample_diff;
        ]

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Diff";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      box ~padding:(padding 1) ~gap:(gap 1) ~flex_direction:Column
        [
          box ~flex_direction:Row ~gap:(gap 3)
            [
              text ~style:hint
                (Printf.sprintf "layout: %s" (layout_name model.layout));
              text ~style:hint
                (Printf.sprintf "line numbers: %s"
                   (if model.show_numbers then "on" else "off"));
              text ~style:hint
                (Printf.sprintf "wrap: %s" (wrap_name model.wrap));
            ];
        ];
      box ~flex_grow:1. ~padding:(padding_xy 1 0)
        ~min_size:{ width = px 0; height = px 0 }
        [
          box ~border:true ~border_color ~title:"lib/session.ml" ~flex_grow:1.
            ~min_size:{ width = px 0; height = px 0 }
            [ diff_view model ];
        ];
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "l layout  •  n numbers  •  w wrap  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'l') -> Some Toggle_layout
      | Char c when Uchar.equal c (Uchar.of_char 'n') -> Some Toggle_numbers
      | Char c when Uchar.equal c (Uchar.of_char 'w') -> Some Toggle_wrap
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
