(** Questions at the terminal: a line, a masked secret, a choice from a menu and
    a confirmation.

    The name is a line read from standard input, the token is read from the
    terminal with a star for each character, the license is picked from a menu
    (Up and Down, or k and j, move, Enter picks, Escape keeps the default), and
    the confirmation takes y or n. Ctrl+C ends the program at any question.
    Where a question cannot be asked, such as with standard input from a file,
    its answer is its default. *)

module Prompt = Matrix_cli_unix.Prompt

let licenses = [ "ISC"; "MIT"; "Apache-2.0"; "LGPL-2.1-or-later" ]
let or_exit = function Ok a -> a | Error _ -> exit 1

let () =
  let out = Matrix_cli_unix.stdout () in
  let name = or_exit (Prompt.line ~default:"demo" "? Project name:") in
  let token = or_exit (Prompt.secret ~default:"" ~mask:true "? API token:") in
  let license = or_exit (Prompt.choose ~default:0 "? License" licenses) in
  let publish = or_exit (Prompt.confirm ~default:false "? Publish now?") in
  Matrix_cli_unix.print out
    (Printf.sprintf "Created %s (%s), with a token of %d characters%s.\n" name
       (List.nth licenses license)
       (String.length token)
       (if publish then ", published" else ""))
