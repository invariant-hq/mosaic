(** Line numbers with gutter signs and per-line colors. *)

open Mosaic

type model = {
  current_line : int;
  breakpoints : int list;
  errors : int list;
  show_numbers : bool;
}

type msg =
  | Move_up
  | Move_down
  | Toggle_breakpoint
  | Toggle_error
  | Toggle_numbers
  | Quit

let sample_code =
  {|let fibonacci n =
  let rec aux a b = function
    | 0 -> a
    | n -> aux b (a + b) (n - 1)
  in
  aux 0 1 n

let () =
  List.init 10 fibonacci
  |> List.iter (fun x ->
       Printf.printf "%d " x);
  print_newline ()

type shape = Circle of float | Rect of float * float

let area = function
  | Circle r -> Float.pi *. r *. r
  | Rect (w, h) -> w *. h|}

let total_lines = String.split_on_char '\n' sample_code |> List.length

let init () =
  ( {
      current_line = 0;
      breakpoints = [ 3 ];
      errors = [ 10 ];
      show_numbers = true;
    },
    Cmd.none )

let toggle_in_list n lst =
  if List.mem n lst then List.filter (fun x -> x <> n) lst else n :: lst

let update msg model =
  match msg with
  | Move_up ->
      let current_line = max 0 (model.current_line - 1) in
      ({ model with current_line }, Cmd.none)
  | Move_down ->
      let current_line = min (total_lines - 1) (model.current_line + 1) in
      ({ model with current_line }, Cmd.none)
  | Toggle_breakpoint ->
      let breakpoints = toggle_in_list model.current_line model.breakpoints in
      ({ model with breakpoints }, Cmd.none)
  | Toggle_error ->
      let errors = toggle_in_list model.current_line model.errors in
      ({ model with errors }, Cmd.none)
  | Toggle_numbers ->
      ({ model with show_numbers = not model.show_numbers }, Cmd.none)
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Signs *)
let breakpoint_sign =
  {
    Line_number.before = Some "●";
    after = None;
    before_color = Some Ansi.Color.red;
    after_color = None;
  }

let error_sign =
  {
    Line_number.before = None;
    after = Some "✗";
    before_color = None;
    after_color = Some Ansi.Color.red;
  }

let view model =
  let line_colors =
    [
      ( model.current_line,
        {
          Line_number.gutter = Ansi.Color.of_rgb 40 40 60;
          content = Some (Ansi.Color.of_rgb 30 30 50);
        } );
    ]
  in
  let line_signs =
    List.map (fun l -> (l, breakpoint_sign)) model.breakpoints
    @ List.map (fun l -> (l, error_sign)) model.errors
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Line Numbers";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Code display *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~border:true ~border_color ~flex_grow:1.
            [
              line_number ~line_colors ~line_signs
                ~show_line_numbers:model.show_numbers ~flex_grow:1.
                (code ~size:{ width = pct 100; height = pct 100 } sample_code);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "j/k move  •  b breakpoint  •  e error  •  n numbers  •  q quit";
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'j') -> Some Move_down
      | Char c when Uchar.equal c (Uchar.of_char 'k') -> Some Move_up
      | Down -> Some Move_down
      | Up -> Some Move_up
      | Char c when Uchar.equal c (Uchar.of_char 'b') -> Some Toggle_breakpoint
      | Char c when Uchar.equal c (Uchar.of_char 'e') -> Some Toggle_error
      | Char c when Uchar.equal c (Uchar.of_char 'n') -> Some Toggle_numbers
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
