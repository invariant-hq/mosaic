(** Downloads in parallel, each on a row of a live region on standard error,
    with a spinner, a bar and a status.

    matrix.cli draws rows and leaves what they show to the program, so the task
    rows here are this program's code. A thread runs each download and publishes
    its state, and the region draws the rows ten times a second ([live]'s
    [tick]), each time from the states and the clock, so that the spinners turn
    between updates. A line a thread prints goes above the rows. Where no region
    may be drawn, as through a pipe or under [CI], the last rows are printed
    once, at the end. *)

module Out = Matrix_cli_unix
module Grid = Matrix_grid

type state = Downloading of int | Verifying | Done of float

type task = {
  name : string;
  size : int;  (** In bytes. *)
  rate : int;  (** In bytes a second. *)
  started : float;
  state : state;
}

let downloads =
  [
    ("ocaml-base-compiler", 6_200_000, 2_400_000);
    ("dune", 2_800_000, 1_900_000);
    ("ppxlib", 1_100_000, 600_000);
    ("eio", 900_000, 350_000);
    ("cmdliner", 400_000, 500_000);
  ]

(* A download that replaces its task's state as it goes. The rows read the
   states, so each is a value that never changes. *)
let download err slot =
  let t = Atomic.get slot in
  let rec fetch got =
    Atomic.set slot { t with state = Downloading got };
    if got < t.size then (
      Thread.delay 0.05;
      fetch (min t.size (got + (t.rate / 20))))
  in
  fetch 0;
  Atomic.set slot { t with state = Verifying };
  Thread.delay 0.4;
  if t.name = "eio" then
    Out.print err "warning: eio: signed with a key that has expired\n";
  Atomic.set slot { t with state = Done (Unix.gettimeofday () -. t.started) }

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let accent = Ansi.Style.make ~fg:Ansi.Color.cyan ()
let ok = Ansi.Style.make ~fg:Ansi.Color.green ()
let dim = Ansi.Style.make ~dim:true ()
let repeat n s = String.concat "" (List.init n (fun _ -> s))
let mib n = Printf.sprintf "%.1f" (float n /. 1_048_576.)
let bar_width = 30

(* A task's row: its mark, its name in a column [name_width] wide, its bar
   and its status. *)
let row g ~y ~name_width ~turn t =
  let mark, style =
    match t.state with
    | Done _ -> ("✓", ok)
    | Downloading _ | Verifying -> (spinner.(turn mod 10), accent)
  in
  Grid.draw_text g ~x:0 ~y ~style ~text:mark;
  Grid.draw_text g ~x:2 ~y ~text:t.name;
  let got = match t.state with Downloading n -> n | _ -> t.size in
  let full = bar_width * got / t.size and x = name_width + 3 in
  Grid.draw_text g ~x ~y ~style:accent ~text:(repeat full "━");
  Grid.draw_text g ~x:(x + full) ~y ~style:dim
    ~text:(repeat (bar_width - full) "─");
  let status =
    match t.state with
    | Downloading n -> Printf.sprintf "%s/%s MiB" (mib n) (mib t.size)
    | Verifying -> "verifying"
    | Done secs -> Printf.sprintf "done in %.1fs" secs
  in
  Grid.draw_text g ~x:(x + bar_width + 1) ~y ~style:dim ~text:status

(* The rows of the tasks' states as they are when the region draws, the
   spinners turning ten times a second. *)
let rows slots ~started ~width ~height:_ =
  let tasks = List.map Atomic.get slots in
  let turn = Float.to_int ((Unix.gettimeofday () -. started) *. 10.) in
  let name_width =
    List.fold_left (fun w t -> max w (String.length t.name)) 0 tasks
  in
  let g = Grid.create ~width ~height:(List.length tasks) () in
  List.iteri (fun y t -> row g ~y ~name_width ~turn t) tasks;
  [ g ]

let () =
  let err = Out.stderr () in
  let started = Unix.gettimeofday () in
  let slots =
    List.map
      (fun (name, size, rate) ->
        Atomic.make { name; size; rate; started; state = Downloading 0 })
      downloads
  in
  let threads = List.map (Thread.create (download err)) slots in
  Out.live ~tick:0.1 err (fun live ->
      Out.show live (rows slots ~started);
      List.iter Thread.join threads);
  let bytes = List.fold_left (fun n (_, size, _) -> n + size) 0 downloads in
  Out.print err
    (Printf.sprintf "Fetched %d packages (%s MiB) in %.1fs\n"
       (List.length downloads) (mib bytes)
       (Unix.gettimeofday () -. started))
