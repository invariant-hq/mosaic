(** Downloads in parallel, reported as tasks, as [--progress] and the
    environment decide.

    Each download is a task that counts its bytes, run on a thread of its own.
    On a terminal ([--progress=auto], the default, or [tty]) the tasks show as
    rows redrawn ten times a second, with a bar, the bytes and their rate, and a
    download that ends goes above the rows. Through a pipe or under [CI]
    ([plain]), a line is printed as each starts and ends. [quiet] prints only
    what fails, and [json] one event a line. A line a thread prints goes above
    the rows. *)

module Out = Matrix_cli_unix
module Task = Matrix_cli_unix.Task

let progress = ref `Auto

let () =
  let choose s = progress := List.assoc s Matrix_cli.Progress.choices in
  Arg.parse
    [
      ( "--progress",
        Arg.Symbol (List.map fst Matrix_cli.Progress.choices, choose),
        " How the downloads show." );
    ]
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    "progress [--progress=MODE]"

let downloads =
  [
    ("ocaml-base-compiler", 6_200_000, 2_400_000);
    ("dune", 2_800_000, 1_900_000);
    ("ppxlib", 1_100_000, 600_000);
    ("eio", 900_000, 350_000);
    ("cmdliner", 400_000, 500_000);
  ]

(* A download: a task that counts its bytes, then verifies them. *)
let download err root (name, size, rate) =
  Task.run root name @@ fun t ->
  let rec fetch got =
    Task.bytes t ~total:size got;
    if got < size then (
      Thread.delay 0.05;
      fetch (min size (got + (rate / 20))))
  in
  fetch 0;
  Task.rename t (name ^ ", verifying");
  Thread.delay 0.4;
  if name = "eio" then
    Out.print err "warning: eio: signed with a key that has expired\n"

let () =
  let err = Out.stderr ~progress:!progress () in
  Out.tasks ~label:"Fetching" err @@ fun root ->
  List.map (Thread.create (download err root)) downloads
  |> List.iter Thread.join
