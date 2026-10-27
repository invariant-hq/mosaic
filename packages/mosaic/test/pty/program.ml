(* Programs for test_runtime_pty, run in a pseudo-terminal: the first
   argument names one. Each quits on [q]; what it measured is printed once
   the terminal is given back, as "RESULT name value" lines. *)

module M = Mosaic

let result name fmt =
  Printf.ksprintf (Printf.printf "RESULT %s %s\r\n%!" name) fmt

let now () = Unix.gettimeofday ()

(* A file of /proc, which only Linux has; elsewhere what is measured from it
   reads as 0. Nothing is tested on it: "measure" prints it. *)
let proc file =
  try In_channel.with_open_text file In_channel.input_all
  with Sys_error _ -> ""

let key_is c key =
  match (M.Event.Key.data key).Matrix.Input.Keystroke.key with
  | Char u -> Uchar.equal u (Uchar.of_char c)
  | _ -> false

let on_char c msg =
  M.Sub.on_key (fun key -> if key_is c key then Some msg else None)

(* A program that shows a count of [a] keys, quits on [q], and runs [init]'s
   command and [subs]. With [count_others], the other messages change the
   model too, and the view shows how many, by the hundred thousand. *)
let counter ?(init = M.Cmd.none) ?(subs = M.Sub.none) ?(count_others = false) ()
    =
  let views = ref 0 in
  M.run
    {
      M.init = (fun () -> ((0, 0), init));
      update =
        (fun msg ((n, others) as model) ->
          match msg with
          | `Quit -> (model, M.Cmd.quit)
          | `Key -> ((n + 1, others), M.Cmd.none)
          | `Other ->
              ((if count_others then (n, others + 1) else model), M.Cmd.none));
      view =
        (fun (n, others) ->
          incr views;
          (* The last cell changes with every count, so a frame that shows
             a new count writes it. *)
          M.text
            (Printf.sprintf "N=%d B=%d %s" n (others / 100_000)
               (if n mod 2 = 0 then "\u{25CB}" else "\u{25CF}")));
      subscriptions =
        (fun _ -> M.Sub.batch [ on_char 'q' `Quit; on_char 'a' `Key; subs ]);
    };
  !views

(* Static: nothing to do between keys. *)
let idle () = ignore (counter () : int)

(* A program that has run one perform, then idles. *)
let idle_after_perform () =
  ignore (counter ~init:(M.Cmd.perform ignore) () : int)

(* A canvas that clears itself in its draw, as a full redraw does, and
   nothing else to do. *)
let idle_canvas () =
  M.run
    {
      M.init = (fun () -> ((), M.Cmd.none));
      update = (fun `Quit () -> ((), M.Cmd.quit));
      view =
        (fun () ->
          M.box
            [
              M.text "N=0";
              M.canvas
                ~size:{ width = M.px 4; height = M.px 1 }
                (fun c ~dt:_ -> M.Canvas.clear c);
            ]);
      subscriptions = (fun () -> on_char 'q' `Quit);
    }

let idle_every () =
  ignore (counter ~subs:(M.Sub.every ~id:"e" 1.0 (fun _ -> `Other)) () : int)

let idle_tick () =
  ignore (counter ~subs:(M.Sub.on_tick ~id:"t" (fun ~dt:_ -> `Other)) () : int)

(* A tick counted from the first to the last, while [a] keys are typed:
   how many, and the seconds between the first and the last. *)
let tick_typing () =
  let ticks = ref [] in
  let tick ~dt:_ =
    ticks := now () :: !ticks;
    `Other
  in
  ignore (counter ~subs:(M.Sub.on_tick ~id:"t" tick) () : int);
  match !ticks with
  | last :: _ ->
      let first = List.nth !ticks (List.length !ticks - 1) in
      result "ticks" "%d" (List.length !ticks);
      result "seconds" "%.6f" (last -. first)
  | [] -> result "ticks" "0"

(* Matrix alone, with its defaults: a frame drawn once. *)
let idle_matrix () =
  Matrix.run @@ fun app ->
  let rec loop () =
    match Matrix.next app with
    | Stopped | Event (Key { key = Char _; text = "q"; _ }) -> ()
    | Frame ->
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"static");
        loop ()
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()

(* A 0.25 s timer, until twelve periods have ended. Each fire is printed
   with the periods ended so far and its time since [init], which ran
   before the timer's origin. *)
let timer () =
  let t0 = ref 0. and fires = ref [] in
  M.run
    {
      M.init =
        (fun () ->
          t0 := now ();
          (0, M.Cmd.none));
      update =
        (fun periods n ->
          let n = n + periods in
          fires := (n, now () -. !t0) :: !fires;
          (n, if n >= 12 then M.Cmd.quit else M.Cmd.none));
      view = (fun n -> M.text (Printf.sprintf "T=%d" n));
      subscriptions = (fun _ -> M.Sub.every ~id:"e" 0.25 Fun.id);
    };
  List.rev !fires |> List.iter (fun (n, at) -> result "fire" "%d %.6f" n at)

(* A worker that dispatches 2,000 messages a second. *)
let busy_worker =
  M.Sub.worker ~id:"busy" (fun stop mailbox ->
      while not (M.Sub.Stop.requested stop) do
        mailbox.dispatch `Other;
        Thread.delay 0.0005
      done)

let latency () = ignore (counter () : int)
let latency_busy () = ignore (counter ~subs:busy_worker () : int)

(* 8 systhreads and 2 domains dispatch 20,000 messages each. *)
let storm () =
  let producers = 10 and per = 20_000 in
  let last = Array.make producers (-1) and disorder = ref 0 in
  let received = ref 0 and t0 = now () in
  let produce p dispatch =
    for i = 0 to per - 1 do
      dispatch (`Msg (p, i))
    done
  in
  let thread p = M.Cmd.perform (produce p) in
  let domain p =
    M.Cmd.perform (fun dispatch ->
        Domain.join (Domain.spawn (fun () -> produce p dispatch)))
  in
  let writes () =
    proc "/proc/self/io" |> String.split_on_char '\n'
    |> List.find_map (fun line -> Scanf.sscanf_opt line "syscw: %d" Fun.id)
    |> Option.value ~default:0
  in
  let writes_before = ref 0 in
  M.run
    {
      M.init =
        (fun () ->
          writes_before := writes ();
          ((), M.Cmd.batch (List.init 8 thread @ [ domain 8; domain 9 ])));
      update =
        (fun (`Msg (p, i)) () ->
          incr received;
          if i <> last.(p) + 1 then incr disorder;
          last.(p) <- i;
          ((), if !received = producers * per then M.Cmd.quit else M.Cmd.none));
      view = (fun () -> M.text "storm");
      subscriptions = (fun () -> M.Sub.none);
    };
  let writes = writes () - !writes_before in
  result "received" "%d" !received;
  result "disorder" "%d" !disorder;
  result "seconds" "%.3f" (now () -. t0);
  result "writes" "%d" writes

(* A worker that dispatches as fast as it can until the run ends, on a
   systhread or on a domain of its own; the view shows the keys and the
   messages applied. *)
let flood ~domain () =
  let t0 = now () in
  let produce stop (mailbox : _ M.mailbox) =
    while not (M.Sub.Stop.requested stop) do
      mailbox.dispatch `Other
    done
  in
  let worker =
    M.Sub.worker ~id:"flood" (fun stop mailbox ->
        if domain then
          Domain.join (Domain.spawn (fun () -> produce stop mailbox))
        else produce stop mailbox)
  in
  let views = counter ~subs:worker ~count_others:true () in
  let peak =
    proc "/proc/self/status" |> String.split_on_char '\n'
    |> List.find_map (fun line -> Scanf.sscanf_opt line "VmHWM: %d kB" Fun.id)
    |> Option.value ~default:0
  in
  result "views" "%d" views;
  result "seconds" "%.3f" (now () -. t0);
  result "peak_kb" "%d" peak

(* A perform that raises after the first frame. *)
let perform_exn () =
  match
    counter
      ~init:
        (M.Cmd.perform (fun _ ->
             Thread.delay 0.3;
             failwith "perform-thread"))
      ()
  with
  | _ -> result "outcome" "returned"
  | exception Failure m -> result "outcome" "raised %s" m

(* [init] raises under the default backend, and the caller catches it: the
   terminal was never taken. What it finds is printed. *)
let init_exn () =
  (match
     M.run
       {
         M.init = (fun () -> failwith "init");
         update = (fun () () -> ((), M.Cmd.none));
         view = (fun () -> M.text "x");
         subscriptions = (fun () -> M.Sub.none);
       }
   with
  | () -> result "outcome" "returned"
  | exception Failure m -> result "outcome" "raised %s" m);
  let t = Unix.tcgetattr Unix.stdin in
  result "cooked" "%b" (t.c_icanon && t.c_echo && t.c_isig)

(* [e] runs a shell command with the terminal; [z] stops the job. *)
let handoff () =
  ignore
    (M.run
       {
         M.init = (fun () -> (0, M.Cmd.none));
         update =
           (fun msg n ->
             match msg with
             | `Quit -> (n, M.Cmd.quit)
             | `Exec ->
                 ( n,
                   M.Cmd.exec (fun () ->
                       `Back (Sys.command "stty -a; printf CHILD")) )
             | `Back _ -> (n + 1, M.Cmd.none)
             | `Suspend -> (n, M.Cmd.suspend));
         view = (fun n -> M.text (Printf.sprintf "BACK %d" n));
         subscriptions =
           (fun _ ->
             M.Sub.batch
               [ on_char 'q' `Quit; on_char 'e' `Exec; on_char 'z' `Suspend ]);
       })

let () =
  let name =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Sys.getenv "MOSAIC_PTY_PROGRAM"
  in
  match name with
  | "idle" -> idle ()
  | "idle-nohup" ->
      Sys.set_signal Sys.sighup Sys.Signal_ignore;
      idle ()
  | "idle-after-perform" -> idle_after_perform ()
  | "idle-every" -> idle_every ()
  | "idle-canvas" -> idle_canvas ()
  | "idle-tick" -> idle_tick ()
  | "tick-typing" -> tick_typing ()
  | "idle-matrix" -> idle_matrix ()
  | "timer" -> timer ()
  | "latency" -> latency ()
  | "latency-busy" -> latency_busy ()
  | "storm" -> storm ()
  | "flood" -> flood ~domain:false ()
  | "flood-domain" -> flood ~domain:true ()
  | "perform-exn" -> perform_exn ()
  | "init-exn" -> init_exn ()
  | "handoff" -> handoff ()
  | name -> failwith ("program: unknown " ^ name)
