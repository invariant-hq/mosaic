(** A dependency tree, as [cargo tree] prints one. A crate whose dependencies
    were already listed is marked "(*)" and not expanded again. *)

type crate = { version : string; deps : string list }

let registry =
  [
    ("app", { version = "0.1.0"; deps = [ "serde"; "serde_json"; "tokio" ] });
    ("serde", { version = "1.0.210"; deps = [ "serde_derive" ] });
    ( "serde_derive",
      { version = "1.0.210"; deps = [ "proc-macro2"; "quote"; "syn" ] } );
    ("proc-macro2", { version = "1.0.86"; deps = [ "unicode-ident" ] });
    ("unicode-ident", { version = "1.0.13"; deps = [] });
    ("quote", { version = "1.0.37"; deps = [ "proc-macro2" ] });
    ( "syn",
      { version = "2.0.79"; deps = [ "proc-macro2"; "quote"; "unicode-ident" ] }
    );
    ( "serde_json",
      { version = "1.0.128"; deps = [ "itoa"; "memchr"; "ryu"; "serde" ] } );
    ("itoa", { version = "1.0.11"; deps = [] });
    ("memchr", { version = "2.7.4"; deps = [] });
    ("ryu", { version = "1.0.18"; deps = [] });
    ( "tokio",
      { version = "1.40.0"; deps = [ "pin-project-lite"; "tokio-macros" ] } );
    ("pin-project-lite", { version = "0.2.14"; deps = [] });
    ( "tokio-macros",
      { version = "2.4.0"; deps = [ "proc-macro2"; "quote"; "syn" ] } );
  ]

(* A line of the tree: a crate, whether it was listed before, its children. *)
type node = Node of string * bool * node list

let rec expand seen name =
  let { deps; _ } = List.assoc name registry in
  if deps <> [] && Hashtbl.mem seen name then Node (name, true, [])
  else (
    Hashtbl.replace seen name ();
    Node (name, false, List.map (expand seen) deps))

let () =
  let paint = Ansi.Style.styled ~reset:true in
  let dim = Ansi.Style.make ~dim:true () in
  let bold = Ansi.Style.make ~bold:true () in
  let line lasts (Node (name, seen, _)) b =
    let { version; _ } = List.assoc name registry in
    Buffer.add_string b
      (paint dim (Matrix_cli.Tree.prefix Matrix_cli.Tree.single lasts));
    Buffer.add_string b (paint bold name ^ " v" ^ version);
    if seen then Buffer.add_string b (paint dim " (*)");
    Buffer.add_char b '\n';
    b
  in
  let b =
    Matrix_cli.Tree.fold
      ~children:(fun (Node (_, _, kids)) -> kids)
      line
      [ expand (Hashtbl.create 16) "app" ]
      (Buffer.create 1024)
  in
  Matrix_cli_unix.print (Matrix_cli_unix.stdout ()) (Buffer.contents b)
