(* Tests of matrix.test: sessions on an emulated terminal, the clock and
   settling, input encoded for the program's modes, goldens, and a child on a
   pseudo-terminal. *)

open Windtrap
module T = Matrix_test
module K = Matrix.Input.Keystroke

let ms n = n * 1_000_000

(* A matrix program that waits with [Matrix.next], draws [view] on each
   frame and hands every other step to [on], which says whether the program
   goes on; [body] drives it. *)
let session ?(profile = T.Profile.vte) ?(cols = 20) ?(rows = 2)
    ?(config = Matrix.config ()) ?(view = fun _ _ -> ()) ?(on = fun _ _ -> true)
    body =
  let term = T.Terminal.create ~profile ~cols ~rows () in
  let app = ref None in
  T.run ~config term
    (fun s ->
      Matrix.run ~config:(T.config s) @@ fun a ->
      app := Some a;
      let rec loop () =
        match Matrix.next a with
        | Stopped -> ()
        | Frame ->
            Matrix.draw a (view a);
            loop ()
        | st -> if on a st then loop ()
      in
      loop ())
    (fun s -> body s term (Option.get !app));
  term

(* The same program, taking its steps through the session. *)
let stepped ?(cols = 20) ?(rows = 2) ?(view = fun _ _ -> ())
    ?(on = fun _ _ -> true) body =
  let term = T.Terminal.create ~cols ~rows () in
  let s = T.create term in
  Matrix.run ~config:(T.config s) (fun app ->
      let step ~now =
        match Matrix.step app ~now with
        | None -> `Idle
        | Some Stopped -> `Stopped
        | Some Frame ->
            Matrix.draw app (view app);
            `Stepped
        | Some st -> if on app st then `Stepped else `Stopped
      in
      T.attach s ~step ~deadline:(fun () -> Matrix.deadline app);
      body s term app);
  term

let text term = T.Screen.text (T.Terminal.screen term)
let row term y = T.Screen.row (T.Terminal.screen term) y
let draw text _app grid = Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:(text ())

(* Steps ------------------------------------------------------------------ *)

let test_settles_at_its_time () =
  let keys = ref 0 in
  let on app = function
    | Matrix.Event (Key _) ->
        incr keys;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore
  @@ session ~on ~view:(draw (fun () -> Printf.sprintf "%d keys" !keys))
  @@ fun s term _ ->
  equal ~msg:"booted" string "01 |0 keys\n02 |\ncursor hidden\n" (text term);
  T.input s (T.Terminal.text term "ab");
  equal ~msg:"typed" string "2 keys" (row term 0);
  T.input s (T.Terminal.key term Escape);
  equal ~msg:"a lone ESC is Escape at once" string "3 keys" (row term 0);
  equal ~msg:"the clock" int 0 (T.now s);
  equal ~msg:"no deadline" (option int) None (T.deadline s)

(* A program that takes its own steps settles the same way. *)
let test_stepped () =
  let keys = ref 0 in
  let on app = function
    | Matrix.Event (Key _) ->
        incr keys;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore
  @@ stepped ~on ~view:(draw (fun () -> Printf.sprintf "%d keys" !keys))
  @@ fun s term _ ->
  T.input s (T.Terminal.text term "abc");
  equal ~msg:"typed" string "3 keys" (row term 0);
  equal ~msg:"the clock" int 0 (T.now s)

(* The profile answers the program's probe as it writes it, in no time. *)
let test_probe () =
  ignore @@ session
  @@ fun s term app ->
  let caps = Matrix.capabilities app in
  is_true ~msg:"synchronized output" caps.sync;
  is_true ~msg:"kitty keyboard" (caps.kitty_keyboard <> None);
  is_true ~msg:"focus reports" caps.focus_tracking;
  is_true ~msg:"raw" (T.Terminal.raw term);
  is_true ~msg:"the alternate screen" (T.Terminal.modes term).alternate_screen;
  is_false ~msg:"no update open" (T.Terminal.modes term).synchronized_output;
  equal ~msg:"kitty flags pushed" int 5
    (Vte.keyboard (T.Terminal.vte term)).kitty;
  equal ~msg:"the clock" int 0 (T.now s)

(* A terminal that answers nothing costs the run's wait for the answer. *)
let test_silent () =
  let silent = { T.Profile.vte with answers = false } in
  ignore @@ session ~profile:silent
  @@ fun s _ app ->
  equal ~msg:"the probe's wait" int (ms 500) (T.now s);
  is_true ~msg:"no kitty protocol"
    ((Matrix.capabilities app).kitty_keyboard = None)

let test_pipe () =
  let size app () =
    let c, r = Matrix.size app in
    Printf.sprintf "%dx%d" c r
  in
  let term =
    session ~profile:T.Profile.pipe ~cols:30 ~view:(fun app grid ->
        draw (size app) app grid)
    @@ fun s term _ ->
    equal ~msg:"the size, in band" string "30x2" (row term 0);
    is_false ~msg:"no raw mode" (T.Terminal.raw term);
    T.resize s ~cols:12 ~rows:2;
    equal ~msg:"resized" string "12x2" (row term 0)
  in
  is_false ~msg:"no alternate screen" (T.Terminal.modes term).alternate_screen

(* Input ------------------------------------------------------------------ *)

let events = ref []

let record app = function
  | Matrix.Event (Resize _) as e ->
      events := e :: !events;
      ignore app;
      true
  | Matrix.Event e ->
      events := Matrix.Event e :: !events;
      true
  | _ -> true

let seen () =
  let l =
    List.rev_map
      (function
        | Matrix.Event e -> Format.asprintf "%a" Matrix.pp_event e | _ -> "")
      !events
  in
  events := [];
  l

(* The bytes of a key follow the program's modes: the kitty protocol's flags
   it pushed, or modifyOtherKeys, or neither on a terminal with none. *)
let test_keys_in_the_program's_encoding () =
  let shift_enter term =
    T.Terminal.key term ~mods:{ K.no_mods with shift = true } Enter
  in
  (ignore @@ session ~on:record
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"kitty" (list string) [ "Key Shift+Enter" ] (seen ()));
  (ignore
  @@ session ~on:record ~config:(Matrix.config ~kitty_keyboard:`Off ())
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"modifyOtherKeys" string "\027[27;2;13~" (shift_enter term);
  equal ~msg:"modifyOtherKeys, read" (list string) [ "Key Shift+Enter" ]
    (seen ()));
  ignore
  @@ session ~on:record ~profile:{ T.Profile.vte with extensions = [] }
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"legacy bytes lose Shift" (list string) [ "Key Enter" ] (seen ())

let test_mouse_paste_focus () =
  ignore
  @@ session ~on:record ~config:(Matrix.config ~mouse:`Sgr_any ())
  @@ fun s term _ ->
  events := [];
  T.input s
    (T.Terminal.mouse term
       {
         kind = Press Left;
         x = 3;
         y = 1;
         shift = false;
         alt = false;
         ctrl = true;
       });
  T.input s (T.Terminal.paste term "hi");
  T.input s (T.Terminal.focus term false);
  T.input s (T.Terminal.color_scheme term `Light);
  equal ~msg:"events" (list string)
    [
      "Mouse Press Left at 3,1 ctrl";
      "Paste_start";
      "Paste_data \"hi\"";
      "Paste_end Complete";
      "Focus_out";
      "Color_scheme Light";
    ]
    (seen ())

let test_resize () =
  ignore
  @@ session ~on:record ~cols:20
  @@ fun s term _ ->
  events := [];
  T.resize s ~cols:10 ~rows:3;
  equal ~msg:"the event" (list string) [ "Resize 10x3" ] (seen ());
  equal ~msg:"drawn at once" int 10 (T.Screen.cols (T.Terminal.screen term));
  equal ~msg:"the clock" int 0 (T.now s)

(* Time -------------------------------------------------------------------- *)

(* Every timer on the way has an instant of its own. *)
let test_advance () =
  let fired = ref [] in
  let on app = function
    | Matrix.Timer _ ->
        fired := Matrix.now app :: !fired;
        ignore (Matrix.at app (Matrix.now app + ms 100) : Matrix.timer);
        true
    | _ -> true
  in
  ignore @@ session ~on
  @@ fun s _ app ->
  ignore (Matrix.at app (ms 100) : Matrix.timer);
  T.advance s (ms 1000);
  equal ~msg:"each at its time" (list int)
    (List.init 10 (fun i -> ms (100 * (i + 1))))
    (List.rev !fired);
  equal ~msg:"the clock" int (ms 1000) (T.now s)

(* An animation asks for its next frame after drawing one: a virtual second
   at 30 frames a second draws 30, and a settle draws none. *)
let test_animation () =
  let view app grid =
    Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"*";
    ignore grid;
    Matrix.request_redraw app
  in
  let config = Matrix.config ~fps:30. () in
  ignore
  @@ session ~config ~view:(fun app grid -> view app grid)
  @@ fun s _ _ ->
  let before = T.frames s in
  T.settle s;
  equal ~msg:"a settle draws nothing" int before (T.frames s);
  T.advance s (ms 1000);
  equal ~msg:"thirty a second" int 30 (T.frames s - before)

(* A key that comes at the instant a timer is due is read in the turn that
   takes the timer, and before it. *)
let test_set_time () =
  let order = ref [] in
  let on _ = function
    | Matrix.Event (Key _) ->
        order := "key" :: !order;
        true
    | Timer _ ->
        order := "timer" :: !order;
        true
    | _ -> true
  in
  ignore @@ session ~on
  @@ fun s term app ->
  ignore (Matrix.at app (ms 100) : Matrix.timer);
  T.set_time s (ms 100);
  equal ~msg:"no step" (list string) [] !order;
  T.input s (T.Terminal.text term "a");
  equal ~msg:"the key first" (list string) [ "key"; "timer" ] (List.rev !order);
  raises_match ~msg:"back in time" (Exn.invalid_arg ~substring:"set_time")
    (fun () -> T.set_time s 0)

(* The host and the end of a run ---------------------------------------- *)

let test_requests_and_end () =
  let on app = function
    | Matrix.Event (Key { key = Char c; _ }) when Uchar.to_int c = 0x71 -> false
    | Matrix.Event (Key _) ->
        let t = Matrix.terminal app in
        Matrix.Terminal.set_title t "hello";
        Matrix.Terminal.bell t;
        Matrix.Terminal.notify t ~title:"t" ~body:"b";
        Matrix.Terminal.copy_to_clipboard t "xy";
        true
    | _ -> true
  in
  let term =
    session ~on ~view:(draw (fun () -> "running")) @@ fun s term _ ->
    T.input s (T.Terminal.text term "a");
    equal ~msg:"the title" string "hello" (T.Terminal.title term);
    equal ~msg:"the requests: a bell, one notification, a clipboard write" int 3
      (List.length (T.Terminal.requests term));
    T.input s (T.Terminal.text term "q");
    is_true ~msg:"ended" (T.ended s);
    raises_match ~msg:"input after the end" (Exn.invalid_arg ~substring:"ended")
      (fun () -> T.input s "a")
  in
  is_false ~msg:"the alternate screen is left"
    (T.Terminal.modes term).alternate_screen;
  is_false ~msg:"cooked mode" (T.Terminal.raw term);
  equal ~msg:"the title is put back" string "" (T.Terminal.title term)

(* Goldens ---------------------------------------------------------------- *)

let styled_view app grid =
  let red = Matrix.Ansi.Style.make ~fg:Matrix.Ansi.Color.red ~bold:true () in
  Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"plain ";
  Matrix.Grid.draw_text grid ~style:red ~x:6 ~y:0 ~text:"red";
  Matrix.Grid.draw_text grid ~x:0 ~y:1 ~text:"wide 日本";
  Matrix.set_cursor app ~visible:true ~style:`Line;
  Matrix.set_cursor_position app ~row:2 ~col:3

let test_goldens () =
  let term = session ~view:styled_view (fun _ _ _ -> ()) in
  ignore term;
  ignore @@ session ~view:styled_view
  @@ fun _ term _ ->
  let screen = T.Terminal.screen term in
  equal ~msg:"text" string "01 |plain red\n02 |wide 日本\ncursor 2,3 bar\n"
    (T.Screen.text screen);
  equal ~msg:"styled" string
    "01 |plain red\n   |      aaa\n02 |wide 日本\ncursor 2,3 bar\na fg=Red bold\n"
    (T.Screen.styled screen);
  equal ~msg:"find"
    (list (pair int int))
    [ (0, 6); (1, 5) ]
    (T.Screen.find screen "red" @ T.Screen.find screen "日");
  equal ~msg:"no difference" (option string) None (T.Screen.diff screen screen)

let test_diff () =
  let text = ref "abcd" and style = ref Matrix.Ansi.Style.default in
  let view _ grid =
    Matrix.Grid.draw_text grid ~style:!style ~x:0 ~y:0 ~text:!text
  in
  let on app = function
    | Matrix.Event (Key _) ->
        text := "abXd";
        style := Matrix.Ansi.Style.make ~bg:Matrix.Ansi.Color.blue ();
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore @@ session ~view ~on
  @@ fun s term _ ->
  let before = T.Terminal.screen term in
  T.input s (T.Terminal.text term "k");
  equal ~msg:"marks" (option string) (Some "01 |abXd\n   |bbXb\n")
    (T.Screen.diff before (T.Terminal.screen term))

(* A program waits through an effect that only run handles: one that waits
   with Matrix.next outside it is refused, where it would wait forever. *)
let test_wait_outside_run () =
  let term = T.Terminal.create ~cols:20 ~rows:2 () in
  let s = T.create term in
  raises_match ~msg:"refused" (Exn.invalid_arg ~substring:"outside") (fun () ->
      Matrix.run ~config:(T.config s) @@ fun app ->
      let rec loop () =
        match Matrix.next app with Stopped -> () | _ -> loop ()
      in
      loop ())

(* The oracle ---------------------------------------------------------------- *)

(* The terminal shows the frame the run presented, through input, a resize
   and in the primary screen; and a program that writes behind its frames,
   as a broken renderer does, is caught. *)
let test_presented () =
  let typed = Buffer.create 8 in
  let on app = function
    | Matrix.Event (Key { key = Char c; _ }) ->
        Buffer.add_utf_8_uchar typed c;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  let view = draw (fun () -> "typed " ^ Buffer.contents typed) in
  List.iter
    (fun mode ->
      Buffer.clear typed;
      ignore
      @@ session ~config:(Matrix.config ~mode ()) ~rows:4 ~on ~view
      @@ fun s term _ ->
      T.check_presented s;
      T.input s (T.Terminal.text term "abc");
      T.check_presented s;
      T.resize s ~cols:12 ~rows:3;
      T.check_presented s)
    [ `Alt; `Primary ];
  let broken app grid =
    view app grid;
    (* Behind the frame's back: the diff does not know of it. *)
    Matrix.Terminal.send (Matrix.terminal app) "\027[1;1HXX"
  in
  ignore @@ session ~on ~view:broken
  @@ fun s term _ ->
  T.input s (T.Terminal.text term "z");
  raises_match ~msg:"caught"
    (function
      | Failure m ->
          String.length m > 0
          && T.Screen.find (T.Terminal.screen term) "XXped" <> []
      | _ -> false)
    (fun () -> T.check_presented s)

(* A child on a pseudo-terminal ------------------------------------------ *)

let child =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_child.exe"

let test_child () =
  if Sys.win32 then skip ~reason:"a pseudo-terminal, which Windows lacks" ();
  let term = T.Terminal.create ~cols:30 ~rows:3 () in
  let shows text term = T.Screen.find (T.Terminal.screen term) text <> [] in
  let status =
    T.Child.with_spawn term ~prog:child ~args:[ "goodbye" ] @@ fun c ->
    T.Child.wait c ~within:10. (shows "SIZE 30x3");
    is_true ~msg:"answered its probe" (T.Terminal.modes term).alternate_screen;
    T.Child.resize c ~cols:25 ~rows:3;
    T.Child.wait c ~within:10. (shows "SIZE 25x3");
    T.Child.send c (T.Terminal.text term "q");
    T.Child.exit c ~within:10.
  in
  is_true ~msg:"exited" (status = Unix.WEXITED 0);
  is_false ~msg:"the alternate screen is left"
    (T.Terminal.modes term).alternate_screen;
  equal ~msg:"the goodbye, on the primary screen" string
    "01 |GOODBYE\n02 |\n03 |\ncursor 2,1 block blinking\n" (text term)

let test_child_timeout () =
  if Sys.win32 then skip ~reason:"a pseudo-terminal, which Windows lacks" ();
  let term = T.Terminal.create ~cols:30 ~rows:3 () in
  T.Child.with_spawn term ~prog:child ~args:[ "goodbye" ] @@ fun c ->
  raises_match ~msg:"never ready"
    (fun e -> match e with Failure m -> String.length m > 0 | _ -> false)
    (fun () -> T.Child.wait c ~within:0.2 (fun _ -> false));
  T.Child.send c "q";
  ignore (T.Child.exit c ~within:10. : Unix.process_status)

(* One write holds text, then an update's whole frame and the opening of the
   next, which ends once a line is read. A wait sees the text where the first
   update opens, and the frame where it ends, and the rest of the output
   waits for the next wait. *)
let test_child_updates () =
  if Sys.win32 then skip ~reason:"a pseudo-terminal, which Windows lacks" ();
  let term = T.Terminal.create ~cols:30 ~rows:3 () in
  let row0 term = T.Screen.row (T.Terminal.screen term) 0 in
  let shows text term = row0 term = text in
  let script =
    "printf \
     'ready\\033[?2026h\\033[Hframe-1\\033[?2026l\\033[?2026h\\033[Hframe-2'; \
     read line; printf '\\033[?2026l'"
  in
  let status =
    T.Child.with_spawn term ~prog:"/bin/sh" ~args:[ "-c"; script ] @@ fun c ->
    T.Child.wait c ~within:10. (shows "ready");
    T.Child.wait c ~within:10. (shows "frame-1");
    is_false ~msg:"the frame, with no update open"
      (T.Terminal.modes term).synchronized_output;
    T.Child.send c "\n";
    T.Child.wait c ~within:10. (shows "frame-2");
    T.Child.exit c ~within:10.
  in
  is_true ~msg:"exited" (status = Unix.WEXITED 0)

(* Pictures ---------------------------------------------------------------- *)

(* A tile-coded picture: [n] tiles of 10 by 20 pixels side by side, red,
   green, blue and white, so that a cell's colors say which tile shows. *)
let tiles = [| (255, 0, 0); (0, 255, 0); (0, 0, 255); (255, 255, 255) |]

let tiled n =
  let w = 10 * n and h = 20 in
  Matrix.Grid.picture ~width:w ~height:h
    (String.init
       (4 * w * h)
       (fun i ->
         let r, g, b = tiles.(i / 4 mod w / 10) in
         Char.chr (match i mod 4 with 0 -> r | 1 -> g | 2 -> b | _ -> 255)))

let kitty =
  {
    T.Profile.vte with
    identity = { Vte.vt100 with version = Some "kitty(0.49.2)" };
    cell_size = Some (10, 20);
  }

let sixel =
  {
    T.Profile.vte with
    identity = { Vte.vt100 with attributes = [ 63; 4 ] };
    cell_size = Some (10, 20);
  }

let view_picture pic _ grid =
  Matrix.Grid.draw_picture grid ~x:1 ~y:0 ~width:4 ~height:1 pic

(* A frame that draws what the frame before it drew writes nothing. *)
let unchanged s term app =
  let before = String.length (T.Terminal.output term) and frames = T.frames s in
  Matrix.request_redraw app;
  T.settle s;
  is_true ~msg:"a frame was drawn" (T.frames s > frames);
  equal ~msg:"an unchanged frame writes nothing" int before
    (String.length (T.Terminal.output term))

let half_blocks term =
  for i = 0 to 3 do
    let text, style =
      T.Screen.cell (T.Terminal.screen term) ~row:0 ~col:(1 + i)
    in
    equal ~msg:"a half block" string "\u{2580}" text;
    let rgb c =
      let r, g, b, _ = Matrix.Ansi.Color.to_rgba c in
      (r, g, b)
    in
    equal ~msg:"its tile's color" (triple int int int) tiles.(i)
      (rgb (Option.get style.fg))
  done

let test_picture_kitty () =
  let pic = tiled 4 in
  ignore
  @@ session ~profile:kitty ~view:(view_picture pic) ~rows:3
  @@ fun s term app ->
  equal ~msg:"placeholders of a held image" string
    " \u{FFFC}\u{FFFC}\u{FFFC}\u{FFFC}" (row term 0);
  equal ~msg:"the cell size"
    (option (pair int int))
    (Some (10, 20))
    (Matrix.cell_size app);
  T.check_presented s;
  unchanged s term app

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let test_picture_sixel () =
  let pic = tiled 4 in
  ignore
  @@ session ~profile:sixel ~view:(view_picture pic) ~rows:3
  @@ fun s term app ->
  is_true ~msg:"a sixel" (contains (T.Terminal.output term) "\027P0;1;0q");
  half_blocks term;
  T.check_presented s;
  unchanged s term app

let test_picture_blocks () =
  let pic = tiled 4 in
  ignore
  @@ session ~view:(view_picture pic) ~rows:3
  @@ fun s term app ->
  half_blocks term;
  T.check_presented s;
  unchanged s term app

(* A picture written to the transcript keeps its image. *)
let test_picture_static () =
  let pic = tiled 2 in
  ignore
  @@ session ~profile:kitty ~rows:4
       ~config:(Matrix.config ~mode:`Primary ())
       ~view:(draw (fun () -> "live"))
  @@ fun s term app ->
  let g = Matrix.Grid.create ~width:4 ~height:1 () in
  Matrix.Grid.draw_picture g ~x:0 ~y:0 ~width:2 ~height:1 pic;
  Matrix.static_write app g;
  T.settle s;
  is_true ~msg:"the transcript row shows the held image"
    (List.mem (0, 0)
       (T.Screen.find (T.Terminal.screen term) "\u{FFFC}\u{FFFC}"));
  let before = String.length (T.Terminal.output term) in
  Matrix.static_replace app (Matrix.Grid.create ~width:4 ~height:1 ());
  T.settle s;
  let output = T.Terminal.output term in
  is_true ~msg:"erasing the scrollback deletes its images"
    (contains
       (String.sub output before (String.length output - before))
       "\027_Ga=d,d=I,")

let () =
  run "matrix.test"
    [
      group "Sessions"
        [
          test "a session settles at its time" test_settles_at_its_time;
          test "a program that takes its own steps" test_stepped;
          test "the probe is answered in no time" test_probe;
          test "a silent terminal costs its wait" test_silent;
          test "a pipe" test_pipe;
        ];
      group "Input"
        [
          test "keys in the program's encoding"
            test_keys_in_the_program's_encoding;
          test "mouse, paste, focus and scheme" test_mouse_paste_focus;
          test "a resize" test_resize;
        ];
      group "Time"
        [
          test "advance visits each deadline" test_advance;
          test "an animation keeps its rate" test_animation;
          test "set_time puts a key before a due timer" test_set_time;
        ];
      group "Host and end"
        [
          test "requests, and the end" test_requests_and_end;
          test "a wait outside run" test_wait_outside_run;
        ];
      group "Goldens"
        [ test "text, styles, find" test_goldens; test "diff" test_diff ];
      group "Oracle"
        [ test "the terminal shows the presented frame" test_presented ];
      group "Pictures"
        [
          test "kitty placeholders" test_picture_kitty;
          test "sixel over half blocks" test_picture_sixel;
          test "half blocks" test_picture_blocks;
          test "in the transcript" test_picture_static;
        ];
      group "Child"
        [
          test "a child, resized, and its goodbye" test_child;
          test "a wait that times out" test_child_timeout;
          test "a wait sees each finished frame" test_child_updates;
        ];
    ]
