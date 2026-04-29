(** Multi-line text editing with wrapping and cursor styles. *)

open Mosaic

type wrap_mode = None | Char | Word
type model = { value : string; wrap : wrap_mode; submitted : bool }
type msg = Set_value of string | Toggle_wrap | Submitted | Quit

let initial_text =
  "Welcome to the Mosaic textarea widget!\n\n\
   This editor supports multi-line text editing with word wrapping,\n\
   character wrapping, or no wrapping at all.\n\n\
   Try pressing F2 to cycle through wrap modes,\n\
   or Ctrl+Enter to submit."

let init () =
  ({ value = initial_text; wrap = Word; submitted = false }, Cmd.none)

let update msg model =
  match msg with
  | Set_value value -> ({ model with value; submitted = false }, Cmd.none)
  | Toggle_wrap ->
      let wrap =
        match model.wrap with None -> Word | Word -> Char | Char -> None
      in
      ({ model with wrap }, Cmd.none)
  | Submitted -> ({ model with submitted = true }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let wrap_mode_to_string = function
  | None -> "None"
  | Char -> "Char"
  | Word -> "Word"

let wrap_mode_to_prop = function None -> `None | Char -> `Char | Word -> `Word

let word_count s =
  let s = String.trim s in
  if String.length s = 0 then 0
  else
    let count = ref 1 in
    let in_space = ref false in
    String.iter
      (fun c ->
        if c = ' ' || c = '\n' || c = '\t' || c = '\r' then (
          if not !in_space then incr count;
          in_space := true)
        else in_space := false)
      s;
    !count

let line_count s =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  let chars = String.length model.value in
  let words = word_count model.value in
  let lines = line_count model.value in
  let status =
    Printf.sprintf "%d chars  •  %d words  •  %d lines  •  wrap: %s" chars words
      lines
      (wrap_mode_to_string model.wrap)
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Textarea";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
        [
          box ~flex_grow:1. ~border:true ~border_color ~flex_direction:Column
            [
              textarea ~autofocus:true ~value:initial_text
                ~placeholder:"Start typing your notes..."
                ~wrap:(wrap_mode_to_prop model.wrap)
                ~cursor_blinking:true
                ~size:{ width = pct 100; height = pct 100 }
                ~on_input:(fun v -> Some (Set_value v))
                ~on_submit:(fun _v -> Some Submitted)
                ();
            ];
          (* Status bar *)
          box ~flex_direction:Row ~justify_content:Space_between
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:hint status;
              (if model.submitted then
                 text
                   ~style:(Ansi.Style.make ~fg:Ansi.Color.green ~bold:true ())
                   "Submitted!"
               else text "");
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "F2 toggle wrap  •  Ctrl+Enter submit  •  Esc quit" ];
    ]

let subscriptions _model =
  Sub.on_keys [ (Shortcut.f 2, Toggle_wrap); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
