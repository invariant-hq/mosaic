(** A table of text as wide as standard output, with a header, columns aligned
    left and right, and a synopsis that wraps at the terminal's width.

    [COLUMNS=60] lays it out in 60 columns. Through a pipe, the table takes 80
    columns. *)

module Table = Matrix_cli.Table

let packages =
  [
    ( "cmdliner",
      "1.3.0",
      412,
      "Declarative definition of command line interfaces" );
    ("eio", "1.2", 1_874, "Effect-based direct-style IO API for OCaml");
    ("fmt", "0.9.0", 156, "OCaml Format pretty-printer combinators");
    ("logs", "0.7.0", 203, "Logging infrastructure for OCaml");
    ("ppxlib", "0.33.0", 4_310, "Standard infrastructure for ppx rewriters");
    ("uutf", "1.0.3", 98, "Non-blocking streaming Unicode codec for OCaml");
  ]

let () =
  let out = Matrix_cli_unix.stdout () in
  let room = Option.value (Matrix_cli_unix.width out) ~default:80 in
  let head s = Table.cell ~style:(Ansi.Style.make ~bold:true ()) s in
  let row (name, version, kib, synopsis) =
    Table.Cells
      [|
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ()) name;
        Table.cell version;
        Table.cell (Printf.sprintf "%d KiB" kib);
        Table.cell synopsis;
      |]
  in
  let rows =
    Table.Cells [| head "Name"; head "Version"; head "Size"; head "Synopsis" |]
    :: Separator `Light :: List.map row packages
  in
  let columns =
    [|
      { Table.align = `Left; overflow = `Truncate };
      { align = `Left; overflow = `Truncate };
      { align = `Right; overflow = `Truncate };
      { align = `Left; overflow = `Wrap };
    |]
  in
  let frame = { Table.edge = Some `Light; line = Some `Light; pad = 1 } in
  Matrix_cli_unix.print_grid out
    (Table.grid ~glyphs:Rounded
       ~style:(Ansi.Style.make ~dim:true ())
       frame ~room columns rows)
