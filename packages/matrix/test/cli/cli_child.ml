(* A program for test_cli_pty, run on a pseudo-terminal or with its outputs
   piped. Its mode is its first argument, or CLI_CHILD when it has none (as
   under runtime_job.exe, which runs it with no argument). *)

module C = Matrix_cli_unix

let text_frame text ~width ~height:_ =
  let g = Matrix_grid.create ~width:(max 1 width) ~height:1 () in
  Matrix_grid.draw_text g ~x:0 ~y:0 ~text;
  [ g ]

let rows_frame rows ~width ~height:_ =
  let g =
    Matrix_grid.create ~width:(max 1 width) ~height:(List.length rows) ()
  in
  List.iteri (fun y text -> Matrix_grid.draw_text g ~x:0 ~y ~text) rows;
  [ g ]

(* ocaml-console's progress example: a step that compiles five files, and a
   layer under it that downloads 2 GiB and fails when [fail]. *)
let build ~fail root =
  let module T = C.Task in
  let step = T.start ~lines:4 root "RUN make" in
  for i = 0 to 5 do
    T.items step ~total:5 (min i 5);
    T.log step (Printf.sprintf "  CC src/file%d.c" i)
  done;
  let layer = T.start step "sha256:abcd" in
  for i = 0 to 8 do
    T.bytes layer ~total:2147483648 (i * 268435456)
  done;
  if fail then (
    T.log layer "checksum mismatch";
    T.fail layer;
    T.fail step;
    T.fail root)
  else (
    T.succeed layer;
    T.succeed step)

(* A clock that stands still, so that every time is 0. *)
let still () = 0.

let rec forever () =
  Unix.sleepf 3600.;
  forever ()

let () =
  let mode =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Option.value (Sys.getenv_opt "CLI_CHILD") ~default:""
  in
  (* The program's --progress choice, from CLI_PROGRESS. *)
  let progress =
    Option.bind (Sys.getenv_opt "CLI_PROGRESS") (fun c ->
        List.assoc_opt c Matrix_cli.Progress.choices)
  in
  let err = C.stderr ?progress () in
  match mode with
  | "progress" ->
      print_string "SHELL\n";
      C.live err (fun live ->
          for i = 1 to 3 do
            C.show live (text_frame (Printf.sprintf "P %d/3" i));
            Unix.sleepf 0.12;
            C.print err (Printf.sprintf "line %d\n" i)
          done;
          C.show live (text_frame "P done"));
      print_string "AFTER\n"
  | "partial" ->
      C.live err (fun live ->
          C.show live (text_frame "R");
          C.print err "half ";
          Unix.sleepf 0.15;
          C.print err "whole\n";
          C.show live (text_frame "R end"))
  | "wait" ->
      C.live err (fun live ->
          C.show live (rows_frame [ "WAITING"; "TWO" ]);
          forever ())
  | "stack" ->
      (* A frame of two grids, one under the other, then of none. *)
      C.live err (fun live ->
          C.show live (fun ~width ~height ->
              rows_frame [ "TITLE" ] ~width ~height
              @ rows_frame [ "ROW 1"; "ROW 2" ] ~width ~height);
          Unix.sleepf 0.15;
          C.print err "line\n");
      C.live err (fun live -> C.show live (fun ~width:_ ~height:_ -> []));
      print_string "AFTER\n"
  | ("tasks" | "tasks-fail") as mode ->
      C.tasks ~label:"Building" ~now:still err
        (build ~fail:(mode = "tasks-fail"));
      print_string "DONE\n"
  | "tasks-tree" ->
      (* A suite whose first test fails while the second runs, with a window
         of two lines; it waits for a key on its input before it ends. *)
      C.tasks ~label:"Testing" err (fun root ->
          C.Task.run ~lines:2 root "suite" (fun suite ->
              List.iter (C.Task.log suite) [ "a"; "b"; "c" ];
              (try
                 C.Task.run suite "one" (fun t ->
                     C.Task.log t "expected 1";
                     failwith "one")
               with Failure _ -> ());
              C.Task.run ~transient:true suite "quick" ignore;
              C.Task.run suite "two" (fun _ ->
                  ignore (input_line stdin : string))));
      print_string "DONE\n"
  | "tasks-fold" ->
      (* Twelve tests ended under a suite on a short terminal. *)
      C.tasks err (fun root ->
          C.Task.run root "suite" (fun suite ->
              for i = 1 to 12 do
                let t = C.Task.start suite (Printf.sprintf "test %d" i) in
                if i mod 6 = 0 then C.Task.fail t else C.Task.succeed t
              done;
              C.Task.run suite "last" (fun _ ->
                  ignore (input_line stdin : string))))
  | "tasks-nested" ->
      C.tasks ~now:still err (fun root ->
          C.Task.run root "outer" (fun _ ->
              C.tasks ~now:still err (fun root ->
                  C.Task.run root "inner" ignore)))
  | "tasks-threads" ->
      (* Four threads that each run twenty tasks. *)
      C.tasks ~now:still err (fun root ->
          List.init 4 (fun k ->
              Thread.create
                (fun () ->
                  for i = 1 to 20 do
                    C.Task.run root (Printf.sprintf "t%d-%d" k i) (fun t ->
                        C.Task.log t "x")
                  done)
                ())
          |> List.iter Thread.join)
  | "tasks-after" ->
      (* A task told things after it ended, and one started under it. *)
      C.tasks ~now:still err (fun root ->
          let t = C.Task.start root "a" in
          C.Task.succeed t;
          C.Task.log t "late";
          C.Task.items t 3;
          C.Task.rename t "b";
          C.Task.fail t;
          let u = C.Task.start t "under" in
          C.print err
            (match (C.Task.state u).status with
            | `Cancelled -> "UNDER CANCELLED\n"
            | _ -> "UNDER OTHER\n"))
  | "tasks-transient" ->
      C.tasks ~now:still err (fun root ->
          C.Task.run ~transient:true root "quick" ignore;
          C.Task.cancel (C.Task.start root "skipped");
          try
            C.Task.run ~transient:true root "broken" (fun t ->
                C.Task.log t "why";
                failwith "broken")
          with Failure _ -> ())
  | "tasks-raise" -> (
      try C.tasks ~now:still err (fun _ -> failwith "boom")
      with Failure _ -> print_string "RAISED\n")
  | "tasks-ended" ->
      (* The root ended by [f] keeps its ending when [f] raises after. *)
      (try
         C.tasks ~now:still err (fun root ->
             C.Task.succeed ~message:"early" root;
             failwith "after")
       with Failure _ -> ());
      print_string "DONE\n"
  | "tasks-back" ->
      (* A count that goes back starts the rate again. *)
      let clock = ref 0. in
      let now () =
        clock := !clock +. 1.;
        !clock
      in
      C.tasks ~now err (fun root ->
          C.Task.run root "fetch" (fun t ->
              C.Task.bytes t 0;
              C.Task.bytes t 100;
              let rated = (C.Task.state t).rate <> None in
              C.Task.bytes t 50;
              C.print err
                (Printf.sprintf "RATE %b %b\n" rated
                   ((C.Task.state t).rate <> None))))
  | "tall" ->
      C.live err (fun live ->
          C.show live (rows_frame (List.init 12 (Printf.sprintf "row %d")));
          Unix.sleepf 0.15)
  | "exit" ->
      C.live err (fun live ->
          C.show live (text_frame "EXITING");
          exit 3)
  | "exit-at-exit" ->
      at_exit (fun () -> C.print err "printed at exit\n");
      C.live err (fun live ->
          C.show live (text_frame "EXITING");
          Unix.sleepf 0.15;
          exit 3)
  | "raise" -> (
      try
        C.live err (fun live ->
            C.show live (text_frame "RAISED");
            failwith "raise")
      with Failure _ -> C.print err "CAUGHT\n")
  | "resize" ->
      C.live err (fun live ->
          C.show live (fun ~width ~height ->
              text_frame (Printf.sprintf "W=%d" width) ~width ~height);
          forever ())
  | "suspend" ->
      C.live err (fun live ->
          C.show live (text_frame "R1");
          C.suspend live (fun () ->
              ignore
                (Sys.command "printf 'from child\\n'; printf 'unended'" : int));
          C.show live (text_frame "R2"))
  | "suspend-wait" ->
      C.live err (fun live ->
          C.show live (text_frame "R1");
          C.suspend live (fun () ->
              ignore (Sys.command "printf 'IN CHILD\\n'; sleep 1" : int));
          C.show live (text_frame "R2"))
  | "raw" ->
      C.live err (fun live ->
          C.show live (text_frame "R");
          C.write_raw err "\027]0;TITLE\007";
          C.print err "after raw\n")
  | "nested" ->
      C.live err (fun outer ->
          C.show outer (text_frame "OUTER");
          C.live err (fun inner ->
              C.show inner (text_frame "INNER 1");
              C.print err "line\n";
              Unix.sleepf 0.6;
              C.show inner (text_frame "INNER done"));
          Unix.sleepf 0.3;
          C.show outer (text_frame "OUTER done"))
  | "table" ->
      let out = C.stdout () in
      let open Matrix_cli.Table in
      let frame = { edge = None; line = Some `Light; pad = 1 } in
      C.print_grid out
        (grid frame ~room:40
           [|
             { align = `Left; overflow = `Wrap };
             { align = `Right; overflow = `Wrap };
           |]
           [
             Cells [| cell "file"; cell "size" |];
             Cells [| cell "a.ml"; cell "12" |];
           ])
  | "color" ->
      let out = C.stdout () in
      C.print out "\027[1;38;2;255;0;0mBOLDRED\027[0m\n"
  | "fmt" ->
      C.adapt_std_formatters ();
      C.live err (fun live ->
          C.show live (text_frame "FRAME");
          Format.printf "out %d@." 1;
          Format.eprintf "err %d@." 2;
          Format.printf "out %d@." 3)
  | ("fmt-order" | "fmt-order-adapted") as mode ->
      (* Lines left in Format's buffers, then lines through an output. *)
      if mode = "fmt-order-adapted" then C.adapt_std_formatters ();
      Format.printf "result 1\n";
      C.print err "step a\n";
      Format.printf "result 2\n";
      C.live err (fun live ->
          C.show live (text_frame "FRAME");
          Format.eprintf "note 3\n";
          C.print err "step b\n")
  | "tick" ->
      (* A frame that changes on its own, shown once: only a region that
         draws on a clock calls it more than at its show and its end. *)
      let calls = Atomic.make 0 in
      C.live ~tick:0.1 err (fun live ->
          C.show live (fun ~width ~height ->
              text_frame
                (Printf.sprintf "CALLS %d" (Atomic.fetch_and_add calls 1 + 1))
                ~width ~height);
          Unix.sleepf 1.5)
  | "height" ->
      (* Each frame shows the height it is given; the inner scope's is what
         the outer's row leaves. *)
      C.live err (fun outer ->
          C.show outer (fun ~width ~height ->
              text_frame (Printf.sprintf "H=%d" height) ~width ~height);
          C.live err (fun inner ->
              C.show inner (fun ~width ~height ->
                  text_frame (Printf.sprintf "IH=%d" height) ~width ~height);
              if Unix.isatty Unix.stderr then forever ()))
  | "line-thread" -> (
      (* A prompt asked on a thread of its own, as Eio_unix.run_in_systhread
         asks it, while this thread keeps printing. *)
      let answer = Atomic.make None in
      let asker =
        Thread.create
          (fun () -> Atomic.set answer (Some (C.Prompt.line "Name?")))
          ()
      in
      let i = ref 0 in
      while Option.is_none (Atomic.get answer) do
        incr i;
        Printf.printf "TICK %d\n%!" !i;
        Thread.delay 0.05
      done;
      Thread.join asker;
      match Atomic.get answer with
      | Some (Ok name) -> Printf.printf "HELLO %s\n" name
      | _ -> print_string "NO ANSWER\n")
  | "progress-of" ->
      Printf.printf "PROGRESS %s\n"
        (fst
           (List.find
              (fun (_, c) -> c = (C.progress err :> Matrix_cli.Progress.choice))
              Matrix_cli.Progress.choices))
  | "notify" ->
      C.notify err ~title:"Done" ~body:"built";
      C.bell err;
      print_string "NOTIFIED\n"
  | ("line" | "line-default") as mode -> (
      let default = if mode = "line-default" then Some "demo" else None in
      match C.Prompt.line ?default "Name?" with
      | Ok name -> Printf.printf "HELLO %s\n" name
      | Error `End_of_input -> print_string "EOF\n"
      | Error `Not_interactive -> print_string "NOT INTERACTIVE\n")
  | ("confirm" | "confirm-nodefault") as mode -> (
      let default = if mode = "confirm" then Some false else None in
      match C.Prompt.confirm ?default "Publish?" with
      | Ok true -> print_string "YES\n"
      | Ok false -> print_string "NO\n"
      | Error `End_of_input -> print_string "EOF\n"
      | Error `Not_interactive -> print_string "NOT INTERACTIVE\n")
  | ("secret-mask" | "secret-styled") as mode -> (
      let question =
        if mode = "secret-styled" then "\027[31m?\027[0m Password:"
        else "Password:"
      in
      match C.Prompt.secret ~mask:(mode = "secret-mask") question with
      | Ok p -> Printf.printf "GOT %S\n" p
      | Error `End_of_input -> print_string "EOF\n"
      | Error `Not_interactive -> print_string "NOT INTERACTIVE\n")
  | ("choose" | "choose-nodefault" | "choose-detached") as mode -> (
      if mode = "choose-detached" then ignore (Unix.setsid () : int);
      let default = if mode = "choose-nodefault" then None else Some 1 in
      match
        C.Prompt.choose ?default "? License"
          [ "ISC"; "MIT"; "Apache-2.0"; "LGPL-2.1-or-later" ]
      with
      | Ok i -> Printf.printf "PICKED %d\n" i
      | Error `End_of_input -> print_string "EOF\n"
      | Error `Not_interactive -> print_string "NOT INTERACTIVE\n")
  | "line-held" -> (
      (* Lines printed on a thread while a question is asked wait for the
         answer. *)
      let answered = Atomic.make false in
      let printer =
        Thread.create
          (fun () ->
            let i = ref 0 in
            while not (Atomic.get answered) do
              incr i;
              C.print err (Printf.sprintf "TICK %d\n" !i);
              Thread.delay 0.05
            done)
          ()
      in
      Thread.delay 0.12;
      let answer = C.Prompt.line "Name?" in
      Atomic.set answered true;
      Thread.join printer;
      match answer with
      | Ok name -> C.print err (Printf.sprintf "HELLO %s\n" name)
      | Error _ -> C.print err "NO ANSWER\n")
  | "secret" -> (
      C.live err @@ fun live ->
      C.show live (text_frame "BUSY");
      match C.Prompt.secret "Password:" with
      | Ok p -> C.print err (Printf.sprintf "GOT %s\n" p)
      | Error `End_of_input -> C.print err "EOF\n"
      | Error `Not_interactive -> C.print err "NOT INTERACTIVE\n")
  | "secret-int" -> (
      (* The secret is asked by a child, which Ctrl+C ends; this process
         then reads the terminal's settings. *)
      ignore (Sys.signal Sys.sigint Sys.Signal_ignore : Sys.signal_behavior);
      match Unix.fork () with
      | 0 ->
          ignore
            (Sys.signal Sys.sigint Sys.Signal_default : Sys.signal_behavior);
          ignore (C.Prompt.secret "Password:" : (string, C.Prompt.error) result);
          exit 0
      | pid ->
          let status =
            match Unix.waitpid [] pid with
            | _, WSIGNALED s when s = Sys.sigint -> "SIGINT"
            | _ -> "OTHER"
          in
          let echo = (Unix.tcgetattr Unix.stdin).c_echo in
          Printf.printf "\r\nENDED %s ECHO %b\n" status echo)
  | "keys" -> (
      let result =
        C.Prompt.keys (fun keys ->
            print_string "READY\n";
            flush stdout;
            let b = Buffer.create 16 in
            let rec loop keys =
              match keys () with
              | Seq.Nil -> ()
              | Seq.Cons ({ Matrix_input.Keystroke.key = Char c; _ }, _)
                when Uchar.to_int c = Char.code 'q' ->
                  ()
              | Seq.Cons (k, rest) ->
                  (match k.Matrix_input.Keystroke.key with
                  | Char c -> Buffer.add_utf_8_uchar b c
                  | Enter -> Buffer.add_string b "<enter>"
                  | Escape -> Buffer.add_string b "<esc>"
                  | Up -> Buffer.add_string b "<up>"
                  | _ -> Buffer.add_string b "<key>");
                  loop rest
            in
            loop keys;
            Buffer.contents b)
      in
      match result with
      | Ok s -> Printf.printf "KEYS %s\n" s
      | Error _ -> print_string "NO KEYS\n")
  | "tstp" ->
      C.live err (fun live ->
          C.show live (text_frame "LIVE");
          forever ())
  | "tstp-bg" ->
      C.live err (fun live ->
          C.show live (text_frame "LIVE");
          Unix.sleepf 2.;
          C.show live (text_frame "LIVE END"))
  | "flood" ->
      C.live err (fun live ->
          let i = ref 0 in
          while true do
            incr i;
            C.show live (text_frame (Printf.sprintf "FRAME %d" !i));
            C.print err (Printf.sprintf "line %d\n" !i)
          done)
  | _ -> prerr_endline ("cli_child: unknown mode " ^ mode)
