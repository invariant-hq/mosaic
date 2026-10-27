(** An imitation of [cargo build], on standard error.

    Each crate's [Compiling] line, and the warning the program's own code gets,
    stay in the scrollback, while a live row below them shows a bar and the
    crates being compiled, at most four at a time. The row is gone once the
    build ends, and [Finished] sums it up. Through a pipe or under [CI], where
    no row may be redrawn, only the lines go out. *)

module Out = Matrix_cli_unix

type crate = {
  name : string;
  version : string;
  deps : string list;
  secs : float;
}

let crate ?(deps = []) name version secs = { name; version; deps; secs }

let crates =
  [
    crate "unicode-ident" "1.0.13" 0.15;
    crate "proc-macro2" "1.0.86" 0.3 ~deps:[ "unicode-ident" ];
    crate "quote" "1.0.37" 0.2 ~deps:[ "proc-macro2" ];
    crate "syn" "2.0.79" 0.8 ~deps:[ "proc-macro2"; "quote"; "unicode-ident" ];
    crate "itoa" "1.0.11" 0.1;
    crate "ryu" "1.0.18" 0.2;
    crate "memchr" "2.7.4" 0.35;
    crate "pin-project-lite" "0.2.14" 0.1;
    crate "serde_derive" "1.0.210" 0.6 ~deps:[ "proc-macro2"; "quote"; "syn" ];
    crate "tokio-macros" "2.4.0" 0.3 ~deps:[ "proc-macro2"; "quote"; "syn" ];
    crate "serde" "1.0.210" 0.45 ~deps:[ "serde_derive" ];
    crate "tokio" "1.40.0" 0.9 ~deps:[ "pin-project-lite"; "tokio-macros" ];
    crate "serde_json" "1.0.128" 0.5 ~deps:[ "itoa"; "memchr"; "ryu"; "serde" ];
    crate "app" "0.1.0 (/home/user/app)" 0.4
      ~deps:[ "serde"; "serde_json"; "tokio" ];
  ]

let jobs = 4
let paint = Ansi.Style.styled ~reset:true
let bold_fg fg = Ansi.Style.make ~bold:true ~fg ()
let green = bold_fg Ansi.Color.bright_green
let cyan = bold_fg Ansi.Color.bright_cyan
let yellow = bold_fg Ansi.Color.bright_yellow
let blue = bold_fg Ansi.Color.bright_blue
let bold = Ansi.Style.make ~bold:true ()

(* A status line, its verb right-aligned in 12 columns. *)
let status style verb what =
  String.make (12 - String.length verb) ' '
  ^ paint style verb ^ " " ^ what ^ "\n"

let unused_variable =
  let gutter = paint blue in
  String.concat "\n"
    [
      paint yellow "warning" ^ paint bold ": unused variable: `retries`";
      gutter "  --> " ^ "src/main.rs:14:9";
      gutter "   |";
      gutter "14 |" ^ "     let retries = 3;";
      gutter "   |" ^ "         "
      ^ paint yellow "^^^^^^^ help: prefix it with an underscore: `_retries`";
      "";
      paint yellow "warning" ^ paint bold ":"
      ^ " `app` (bin \"app\") generated 1 warning";
      "";
    ]

(* The live row: a bar of the crates compiled, and those being compiled. *)
let building ~finished ~running ~width =
  let total = List.length crates and cells = 25 in
  let full = cells * finished / total in
  let bar =
    String.make full '='
    ^ if full < cells then ">" ^ String.make (cells - full - 1) ' ' else ""
  in
  let g = Matrix_grid.create ~width ~height:1 () in
  Matrix_grid.draw_text g ~x:4 ~y:0 ~style:cyan ~text:"Building";
  Matrix_grid.draw_text g ~x:13 ~y:0
    ~text:
      (Printf.sprintf "[%s] %d/%d: %s" bar finished total
         (String.concat ", " running));
  Some g

(* Starts each crate once those it depends on are compiled and a job is
   free, until every crate is compiled. *)
let compile err live =
  let rec step ~waiting ~running ~finished =
    let now = Unix.gettimeofday () in
    let ended, running =
      List.partition (fun (_, until) -> until <= now) running
    in
    let finished = List.map (fun (c, _) -> c.name) ended @ finished in
    if List.exists (fun (c, _) -> c.name = "app") ended then
      Out.print err unused_variable;
    let ready c = List.for_all (fun d -> List.mem d finished) c.deps in
    let start (running, waiting) c =
      if List.length running < jobs && ready c then (
        Out.print err (status green "Compiling" (c.name ^ " v" ^ c.version));
        (running @ [ (c, now +. c.secs) ], waiting))
      else (running, waiting @ [ c ])
    in
    let running, waiting = List.fold_left start (running, []) waiting in
    let names = List.map (fun (c, _) -> c.name) running in
    Out.show live (building ~finished:(List.length finished) ~running:names);
    if waiting <> [] || running <> [] then (
      Unix.sleepf 0.02;
      step ~waiting ~running ~finished)
  in
  step ~waiting:crates ~running:[] ~finished:[]

let () =
  let err = Out.stderr () in
  let started = Unix.gettimeofday () in
  Out.print err
    (status green "Locking"
       (Printf.sprintf "%d packages to latest compatible versions"
          (List.length crates - 1)));
  Out.live err (fun live ->
      compile err live;
      Out.show live (fun ~width:_ -> None));
  Out.print err
    (status green "Finished"
       (Printf.sprintf
          "`dev` profile [unoptimized + debuginfo] target(s) in %.2fs"
          (Unix.gettimeofday () -. started)))
