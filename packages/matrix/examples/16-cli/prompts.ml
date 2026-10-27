(** Questions at the terminal: a line, a secret, and a choice made with keys.

    The name is a line read from standard input, the token is read from the
    terminal with the echo off, and the license is picked from a menu drawn in a
    live region while keys are read: Up and Down (or k and j) move, Enter picks,
    Escape keeps the default. Ctrl+C ends the program at any question. Where a
    question cannot be asked, such as with standard input from a file, its
    answer is the default. *)

module Out = Matrix_cli_unix
module Prompt = Matrix_cli_unix.Prompt
module Grid = Matrix_grid

let answer ~default = function
  | Ok a -> a
  | Error `Not_interactive -> default
  | Error `End_of_input -> exit 1

let licenses = [| "ISC"; "MIT"; "Apache-2.0"; "LGPL-2.1-or-later" |]
let accent = Ansi.Style.make ~fg:Ansi.Color.cyan ~bold:true ()
let dim = Ansi.Style.make ~dim:true ()

(* The menu: the question, then a row for each choice, the selected one
   marked. *)
let menu selected ~width ~height:_ =
  let g = Grid.create ~width ~height:(Array.length licenses + 1) () in
  Grid.draw_text g ~x:0 ~y:0 ~text:"? License";
  Grid.draw_text g ~x:10 ~y:0 ~style:dim ~text:"(arrows to move, Enter to pick)";
  Array.iteri
    (fun i l ->
      if i = selected then
        Grid.draw_text g ~x:0 ~y:(i + 1) ~style:accent ~text:("❯ " ^ l)
      else Grid.draw_text g ~x:2 ~y:(i + 1) ~text:l)
    licenses;
  [ g ]

(* The question once answered, which stays in the scrollback. *)
let picked i ~width ~height:_ =
  let g = Grid.create ~width ~height:1 () in
  Grid.draw_text g ~x:0 ~y:0 ~text:"? License";
  Grid.draw_text g ~x:10 ~y:0 ~style:accent ~text:licenses.(i);
  [ g ]

let choose_license err =
  let last = Array.length licenses - 1 in
  Out.live err @@ fun live ->
  let rec pick i keys =
    Out.show live (menu i);
    match keys () with
    | Seq.Nil -> 0
    | Seq.Cons ({ Matrix_input.Keystroke.key; text; _ }, keys) -> (
        match key with
        | Up -> pick (max 0 (i - 1)) keys
        | Down -> pick (min last (i + 1)) keys
        | Enter -> i
        | Escape -> 0
        | _ when text = "k" -> pick (max 0 (i - 1)) keys
        | _ when text = "j" -> pick (min last (i + 1)) keys
        | _ -> pick i keys)
  in
  let i = answer ~default:0 (Prompt.keys (pick 0)) in
  Out.show live (picked i);
  licenses.(i)

let () =
  let out = Out.stdout () and err = Out.stderr () in
  let name = answer ~default:"demo" (Prompt.line "? Project name: ") in
  let token = answer ~default:"" (Prompt.secret "? API token: ") in
  let license = choose_license err in
  Out.print out
    (Printf.sprintf "Created %s (%s), with a token of %d characters.\n" name
       license (String.length token))
