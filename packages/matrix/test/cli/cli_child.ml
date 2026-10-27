(* A program for test_cli_pty, run on a pseudo-terminal or with its outputs
   piped. Its mode is its first argument, or CLI_CHILD when it has none (as
   under runtime_job.exe, which runs it with no argument). *)

module C = Matrix_cli_unix

let text_frame text ~width =
  let g = Matrix_grid.create ~width:(max 1 width) ~height:1 () in
  Matrix_grid.draw_text g ~x:0 ~y:0 ~text;
  Some g

let rows_frame rows ~width =
  let g =
    Matrix_grid.create ~width:(max 1 width) ~height:(List.length rows) ()
  in
  List.iteri (fun y text -> Matrix_grid.draw_text g ~x:0 ~y ~text) rows;
  Some g

let rec forever () =
  Unix.sleepf 3600.;
  forever ()

let () =
  let mode =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Option.value (Sys.getenv_opt "CLI_CHILD") ~default:""
  in
  let err = C.stderr () in
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
  | "tall" ->
      C.live err (fun live ->
          C.show live (rows_frame (List.init 12 (Printf.sprintf "row %d")));
          Unix.sleepf 0.15)
  | "exit" ->
      C.live err (fun live ->
          C.show live (text_frame "EXITING");
          exit 3)
  | "resize" ->
      C.live err (fun live ->
          C.show live (fun ~width ->
              text_frame (Printf.sprintf "W=%d" width) ~width);
          forever ())
  | "suspend" ->
      C.live err (fun live ->
          C.show live (text_frame "R1");
          C.suspend live (fun () ->
              ignore
                (Sys.command "printf 'from child\\n'; printf 'unended'" : int));
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
  | "notify" ->
      C.notify err ~title:"Done" ~body:"built";
      C.bell err;
      print_string "NOTIFIED\n"
  | "line" -> (
      match C.Prompt.line "Name? " with
      | Ok name -> Printf.printf "HELLO %s\n" name
      | Error `End_of_input -> print_string "EOF\n"
      | Error `Not_interactive -> print_string "NOT INTERACTIVE\n")
  | "secret" -> (
      C.live err @@ fun live ->
      C.show live (text_frame "BUSY");
      match C.Prompt.secret "Password: " with
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
          ignore
            (C.Prompt.secret "Password: " : (string, C.Prompt.error) result);
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
