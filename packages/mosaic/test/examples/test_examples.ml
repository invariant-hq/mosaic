(* An example that runs a program from another event loop, in a
   pseudo-terminal: the program shows its title, ends on [q], and gives the
   terminal back; with [--for] its event loop cancels it, and it gives the
   terminal back before the program writes again. The arguments are the
   example and its title. *)

open Windtrap
module T = Matrix_test

let exe = Sys.argv.(1)
let title = Sys.argv.(2)

let find out sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length out then None
    else if String.sub out i n = sub then Some i
    else go (i + 1)
  in
  go 0

(* A sandbox can forbid opening a pseudo-terminal, as opam-repository's
   macOS builders do: a test that needs one skips. *)
let pty_opens =
  lazy
    (match Pty.open_pty () with
    | master, slave ->
        Pty.close master;
        Pty.close slave;
        true
    | exception Unix.Unix_error ((EPERM | EACCES | ENOENT | ENOSYS), _, _) ->
        false)

(* Runs the example with [args], then [stty -a], which shows the terminal's
   settings as the example left them, and calls [f] once the title shows on
   the alternate screen. It is everything the session wrote. *)
let session args f =
  if not (Lazy.force pty_opens) then
    skip ~reason:"a pseudo-terminal, which this host cannot open" ();
  let term = T.Terminal.create ~cols:80 ~rows:24 () in
  let status =
    T.Child.with_spawn term
      ~env:[ ("PATH", Sys.getenv "PATH") ]
      ~prog:"/bin/sh"
      ~args:("-c" :: {|"$0" "$@"; s=$?; stty -a; exit $s|} :: exe :: args)
    @@ fun c ->
    T.Child.wait c ~within:60. (fun term ->
        (T.Terminal.modes term).alternate_screen
        && T.Screen.find (T.Terminal.screen term) title <> []);
    f c;
    T.Child.exit c ~within:60.
  in
  is_true ~msg:"a normal exit" (status = WEXITED 0);
  is_false ~msg:"the alternate screen left"
    (T.Terminal.modes term).alternate_screen;
  let out = T.Terminal.output term in
  contains ~msg:"icanon" ~sub:" icanon" out;
  List.iter
    (fun off -> is_true ~msg:off (find out off = None))
    [ "-icanon"; "-echo "; "-isig" ];
  out

let quits () = ignore (session [] (fun c -> T.Child.send c "q") : string)

let cancelled () =
  let out = session [ "--for"; "1" ] ignore in
  match
    (find out "\027[?1049l", find out "The terminal is the shell's again.")
  with
  | Some left, Some line ->
      is_true ~msg:"the program writes once the terminal is given back"
        (line > left)
  | _ -> ()

let () =
  exit
    (run
       ~argv:[| Sys.argv.(0) |]
       ("mosaic.examples " ^ Filename.basename (Filename.dirname exe))
       [
         group "Example"
           [
             test "quits on q" quits;
             test "cancelled by its event loop" cancelled;
           ];
       ])
