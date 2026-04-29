(** CSS Grid layout with template areas and responsive switching. *)

open Mosaic

(* ---------- Model ---------- *)

type layout = Dashboard | Two_column | Holy_grail
type model = { layout : layout }
type msg = Quit | Set_layout of layout

let init () = ({ layout = Dashboard }, Cmd.none)

let update msg model =
  match msg with
  | Quit -> (model, Cmd.quit)
  | Set_layout layout -> ({ layout }, Cmd.none)

(* ---------- Palette ---------- *)

let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Area background colors *)
let area_header_bg = Ansi.Color.of_rgb 40 70 90
let area_sidebar_bg = Ansi.Color.of_rgb 50 50 70
let area_main_bg = Ansi.Color.of_rgb 35 60 50
let area_footer_bg = Ansi.Color.of_rgb 60 45 45
let area_right_bg = Ansi.Color.of_rgb 55 55 40
let area_left_bg = Ansi.Color.of_rgb 45 55 65

(* ---------- Layout configurations ---------- *)

let layout_label = function
  | Dashboard -> "Dashboard"
  | Two_column -> "Two-column"
  | Holy_grail -> "Holy Grail"

let area_box ~label ~background ~grid_row ~grid_column =
  box ~grid_row ~grid_column ~background ~padding:(padding 1) ~flex_grow:1.
    ~align_items:Center ~justify_content:Center
    [ text ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ()) label ]

let dashboard_grid _model =
  (* 3 columns, 3 rows: header spans top, sidebar left, main center+right,
     footer spans bottom *)
  box ~display:Display.Grid
    ~grid_template_columns:[ Grid.length 20.; Grid.fr 1.; Grid.fr 1. ]
    ~grid_template_rows:[ Grid.length 3.; Grid.fr 1.; Grid.length 3. ]
    ~gap:(gap 1) ~flex_grow:1.
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header: row 1, cols 1-3 *)
      area_box ~label:"Header" ~background:area_header_bg
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 1 4);
      (* Sidebar: row 2, col 1 *)
      area_box ~label:"Sidebar" ~background:area_sidebar_bg
        ~grid_row:(Grid.line_range 2 3) ~grid_column:(Grid.line_range 1 2);
      (* Main: row 2, cols 2-3 *)
      area_box ~label:"Main" ~background:area_main_bg
        ~grid_row:(Grid.line_range 2 3) ~grid_column:(Grid.line_range 2 4);
      (* Footer: row 3, cols 1-3 *)
      area_box ~label:"Footer" ~background:area_footer_bg
        ~grid_row:(Grid.line_range 3 4) ~grid_column:(Grid.line_range 1 4);
    ]

let two_column_grid _model =
  (* 2 equal columns, single row *)
  box ~display:Display.Grid
    ~grid_template_columns:[ Grid.fr 1.; Grid.fr 1. ]
    ~grid_template_rows:[ Grid.fr 1. ]
    ~gap:(gap 1) ~flex_grow:1.
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Left panel: row 1, col 1 *)
      area_box ~label:"Left Panel" ~background:area_left_bg
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 1 2);
      (* Right panel: row 1, col 2 *)
      area_box ~label:"Right Panel" ~background:area_right_bg
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 2 3);
    ]

let holy_grail_grid _model =
  (* 3 columns, 3 rows: header top, left sidebar + main + right sidebar, footer
     bottom *)
  box ~display:Display.Grid
    ~grid_template_columns:[ Grid.length 18.; Grid.fr 1.; Grid.length 18. ]
    ~grid_template_rows:[ Grid.length 3.; Grid.fr 1.; Grid.length 3. ]
    ~gap:(gap 1) ~flex_grow:1.
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header: row 1, cols 1-3 *)
      area_box ~label:"Header" ~background:area_header_bg
        ~grid_row:(Grid.line_range 1 2) ~grid_column:(Grid.line_range 1 4);
      (* Left sidebar: row 2, col 1 *)
      area_box ~label:"Left Sidebar" ~background:area_sidebar_bg
        ~grid_row:(Grid.line_range 2 3) ~grid_column:(Grid.line_range 1 2);
      (* Main: row 2, col 2 *)
      area_box ~label:"Main" ~background:area_main_bg
        ~grid_row:(Grid.line_range 2 3) ~grid_column:(Grid.line_range 2 3);
      (* Right sidebar: row 2, col 3 *)
      area_box ~label:"Right Sidebar" ~background:area_right_bg
        ~grid_row:(Grid.line_range 2 3) ~grid_column:(Grid.line_range 3 4);
      (* Footer: row 3, cols 1-3 *)
      area_box ~label:"Footer" ~background:area_footer_bg
        ~grid_row:(Grid.line_range 3 4) ~grid_column:(Grid.line_range 1 4);
    ]

(* ---------- View ---------- *)

let view model =
  let grid_content =
    match model.layout with
    | Dashboard -> dashboard_grid model
    | Two_column -> two_column_grid model
    | Holy_grail -> holy_grail_grid model
  in
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Grid Layout";
              text ~style:muted
                (Printf.sprintf "Layout: %s" (layout_label model.layout));
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~border:true ~border_color ~padding:(padding 1)
        ~margin:(margin 1) [ grid_content ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "1 dashboard  •  2 two-column  •  3 holy grail  •  q quit";
        ];
    ]

(* ---------- Subscriptions ---------- *)

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char '1', Set_layout Dashboard);
      (Shortcut.char '2', Set_layout Two_column);
      (Shortcut.char '3', Set_layout Holy_grail);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
