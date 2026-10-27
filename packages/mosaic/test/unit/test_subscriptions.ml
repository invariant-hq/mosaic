(* Subscription constructors, mappings and a small timer model. Complete-loop
   tests in test_loop cover dispatch ordering: earlier command messages drain
   before input, and accepted declarations refresh before the next event. *)

module Input = Matrix_input
open Windtrap
open Mosaic

(* ── Timer Model (minimal reproduction) ── *)

type timer_state = Idle | Running

type model = {
  time_remaining : int;
  state : timer_state;
  elapsed_time : float;
  tick_count : int;
}

type msg = Tick of float | Every_fired | Quit

let update msg m =
  match msg with
  | Tick dt ->
      if m.state = Running then
        let elapsed = m.elapsed_time +. dt in
        if elapsed >= 1.0 then
          let new_time = max 0 (m.time_remaining - 1) in
          let new_state = if new_time = 0 then Idle else Running in
          {
            time_remaining = new_time;
            state = new_state;
            elapsed_time = 0.0;
            tick_count = m.tick_count + 1;
          }
        else { m with elapsed_time = elapsed; tick_count = m.tick_count + 1 }
      else m
  | Every_fired -> { m with tick_count = m.tick_count + 1 }
  | Quit -> m

(* ── Subscription collectors ── *)

(* Flatten a Sub.t into a list of individual subs. *)
let rec flatten_sub (sub : msg Sub.t) : msg Sub.t list =
  match sub with
  | Sub.None -> []
  | Sub.Batch subs -> List.concat_map flatten_sub subs
  | other -> [ other ]

(* ── Tests ── *)

let sub_on_tick_produces_messages () =
  (* Verify that Sub.on_tick produces a message with the correct dt. *)
  let sub = Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt) in
  match sub with
  | Sub.On_tick (id, f) -> (
      equal string "frame" id;
      let msg = f ~dt:0.016 in
      match msg with
      | Tick dt -> equal ~msg:"dt" (float 0.001) 0.016 dt
      | _ -> fail "expected Tick msg")
  | _ -> fail "expected On_tick sub"

let sub_every_produces_messages () =
  (* Verify that Sub.every produces a message when called. *)
  let sub = Sub.every ~id:"countdown" 1.0 (fun _ -> Every_fired) in
  match sub with
  | Sub.Every (id, interval, f) -> (
      equal ~msg:"stable timer identity" string "countdown" id;
      equal ~msg:"interval" (float 0.001) 1.0 interval;
      let msg = f 1 in
      match msg with Every_fired -> () | _ -> fail "expected Every_fired msg")
  | _ -> fail "expected Every sub"

let sub_every_map_preserves_identity () =
  let original = Sub.every ~id:"refresh" 0.5 (fun periods -> 3 * periods) in
  match Sub.map (fun n -> n + 1) original with
  | Sub.Every (id, interval, f) ->
      equal string "refresh" id;
      equal float_exact 0.5 interval;
      equal ~msg:"the count reaches the mapped callback" int 7 (f 2)
  | _ -> fail "expected Every sub"

let sub_after_map_and_validation () =
  (match Sub.map (fun n -> n + 1) (Sub.after ~id:"save" 0.5 1) with
  | Sub.After (id, delay, msg) ->
      equal string "save" id;
      equal float_exact 0.5 delay;
      equal int 2 msg
  | _ -> fail "expected After sub");
  raises
    (Invalid_argument
       "Mosaic.Sub.after: nonempty id and nonnegative finite delay required")
    (fun () -> Sub.after ~id:"save" (-1.) 0)

let timer_model_accumulates_elapsed () =
  (* Verify the timer model accumulates elapsed time across ticks and decrements
     time_remaining after 1 second. *)
  let m =
    { time_remaining = 5; state = Running; elapsed_time = 0.0; tick_count = 0 }
  in
  (* Simulate 60 ticks at ~16.67ms each (= 1 second) *)
  let m = ref m in
  for _ = 1 to 59 do
    m := update (Tick (1.0 /. 60.0)) !m
  done;
  (* After 59 ticks (~983ms), time_remaining should still be 5 *)
  equal ~msg:"remaining after 59 ticks" int 5 !m.time_remaining;
  (* 60th tick pushes us past 1.0s *)
  let m' = update (Tick (1.0 /. 60.0)) !m in
  equal ~msg:"remaining after 60 ticks" int 4 m'.time_remaining;
  equal ~msg:"elapsed resets" (float 0.001) 0.0 m'.elapsed_time

let timer_model_goes_idle_at_zero () =
  (* When time_remaining reaches 0, state should become Idle. *)
  let m =
    { time_remaining = 1; state = Running; elapsed_time = 0.99; tick_count = 0 }
  in
  let m' = update (Tick 0.02) m in
  equal ~msg:"remaining" int 0 m'.time_remaining;
  is_true ~msg:"idle" (m'.state = Idle)

let sub_batch_flattens () =
  (* Verify that Sub.batch correctly groups subscriptions. *)
  let subs =
    Sub.batch
      [
        Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt);
        Sub.on_key (fun _ -> None);
        Sub.every ~id:"batch-timer" 2.0 (fun _ -> Every_fired);
      ]
  in
  let flat = flatten_sub subs in
  equal ~msg:"3 subs" int 3 (List.length flat)

let sub_map_transforms_messages () =
  (* Verify that Sub.map correctly transforms messages. *)
  let sub = Sub.on_tick ~id:"frame" (fun ~dt -> Tick dt) in
  let mapped : msg Sub.t = Sub.map (fun msg -> msg) sub in
  match mapped with
  | Sub.On_tick (id, f) -> (
      equal string "frame" id;
      match f ~dt:0.5 with
      | Tick dt -> equal ~msg:"mapped dt" (float 0.001) 0.5 dt
      | _ -> fail "expected Tick")
  | _ -> fail "expected On_tick"

let key ?mods ?repeat ?base ?keypad key =
  Event.Key.of_input (Input.Keystroke.make ?mods ?repeat ?base ?keypad key)

let char ?mods ?base c =
  key ?mods ?base (Input.Keystroke.Char (Uchar.of_char c))

let shortcut_matches_plain_keys () =
  is_true ~msg:"char shortcut" (Shortcut.matches (Shortcut.char 'q') (char 'q'));
  is_true ~msg:"escape shortcut"
    (Shortcut.matches Shortcut.escape (key Input.Keystroke.Escape));
  is_false ~msg:"wrong char" (Shortcut.matches (Shortcut.char 'q') (char 'x'))

let sub_on_key_release_delivers_releases () =
  let release : Input.Keystroke.release =
    {
      key = Char (Uchar.of_char 'q');
      mods = Input.Keystroke.no_mods;
      base = None;
      keypad = false;
    }
  in
  let sub =
    Sub.map
      (fun n -> n + 1)
      (Sub.on_key_release (fun (r : Input.Keystroke.release) ->
           if r.key = Char (Uchar.of_char 'q') then Some 1 else None))
  in
  match sub with
  | Sub.On_key_release f ->
      equal ~msg:"release delivered and mapped" (option int) (Some 2)
        (f release)
  | _ -> fail "expected On_key_release"

let shortcut_matches_ctrl_base_key () =
  let ev =
    key
      ~mods:{ Input.Keystroke.no_mods with ctrl = true }
      ~base:(Uchar.of_char 'c')
      (Input.Keystroke.Char (Uchar.of_int 0x0441))
  in
  is_true ~msg:"base-layout fallback" (Shortcut.matches (Shortcut.ctrl 'c') ev)

let shortcut_shift_letter_is_the_capital () =
  is_true ~msg:"Shift+a arrives as 'A' and matches char ~shift:true 'a'"
    (Shortcut.matches (Shortcut.char ~shift:true 'a') (char 'A'));
  is_true ~msg:"char 'a' matches Shift+a, as Caps Lock with a"
    (Shortcut.matches (Shortcut.char 'a') (char 'A'));
  is_false ~msg:"char ~shift:true 'a' does not match a"
    (Shortcut.matches (Shortcut.char ~shift:true 'a') (char 'a'))

let shortcut_alt_matches_alt () =
  is_true ~msg:"Alt matches Matrix's Alt"
    (Shortcut.matches (Shortcut.alt 'x')
       (char ~mods:{ Input.Keystroke.no_mods with alt = true } 'x'));
  is_false ~msg:"Alt with Meta is another chord"
    (Shortcut.matches (Shortcut.alt 'x')
       (char ~mods:{ Input.Keystroke.no_mods with alt = true; meta = true } 'x'))

let sub_on_keys_resolution () =
  let resolve bindings event =
    match Sub.on_keys bindings with
    | Sub.On_key (_, f) -> f event
    | _ -> fail "expected On_key"
  in
  let check message expected bindings event =
    equal ~msg:message (option int) expected (resolve bindings event);
    equal ~msg:"only bound events prevent defaults" bool
      (Option.is_some expected)
      (Event.Key.default_prevented event)
  in
  check "later equivalent bindings override earlier ones" (Some 2)
    [ (Shortcut.char 'q', 1); (Shortcut.char 'q', 2) ]
    (char 'q');
  check "a keypad key resolves its main key's binding" (Some 1)
    [ (Shortcut.enter, 1) ]
    (key ~keypad:true Input.Keystroke.Enter);
  check "exact uppercase resolves regardless of order" (Some 1)
    [ (Shortcut.char 'Q', 1); (Shortcut.char 'q', 2) ]
    (char 'Q');
  check "a capital nothing binds resolves the lowercase binding" (Some 2)
    [ (Shortcut.char 'q', 2) ]
    (char 'Q');
  check "a lowercase letter does not resolve the capital's binding" None
    [ (Shortcut.char 'Q', 1) ]
    (char 'q');
  check "direct character precedes base layout regardless of order" (Some 1)
    [ (Shortcut.char 'x', 1); (Shortcut.char 'q', 2) ]
    (char ~base:(Uchar.of_char 'q') 'x');
  check "an unbound capital takes its lowercase before the base layout" (Some 1)
    [ (Shortcut.char 'x', 1); (Shortcut.char 'q', 2) ]
    (char ~base:(Uchar.of_char 'q') 'X');
  check "an unbound capital falls back to the base layout" (Some 2)
    [ (Shortcut.char 'q', 2) ]
    (char ~base:(Uchar.of_char 'q') 'X');
  check "base-layout fallback remains available" (Some 2)
    [ (Shortcut.char 'q', 1); (Shortcut.char 'q', 2) ]
    (char ~base:(Uchar.of_char 'q') 'x');
  check "unbound modifiers remain available" None
    [ (Shortcut.char 'q', 1) ]
    (char ~mods:{ Input.Keystroke.no_mods with ctrl = true } 'q')

let sub_on_keys_does_not_compile_or_allocate_per_override () =
  let shortcut = Shortcut.char 'q' in
  let bindings = List.init 128 (fun value -> (shortcut, value)) in
  let before = Gc.allocated_bytes () in
  let sub = Sys.opaque_identity (Sub.on_keys bindings) in
  let constructed = Gc.allocated_bytes () -. before in
  is_true ~msg:"model updates do not compile a binding table"
    (constructed < 1024.);
  let measure bindings =
    let dispatch =
      match Sub.on_keys bindings with
      | Sub.On_key (_, f) -> f
      | _ -> fail "expected key subscription"
    in
    (* A fresh event each time: a binding prevents the key it takes, and a
       prevented key is not looked up again. *)
    let before = Gc.allocated_bytes () in
    for i = 1 to 10000 do
      ignore i;
      ignore (Sys.opaque_identity (dispatch (char 'q')))
    done;
    Gc.allocated_bytes () -. before
  in
  ignore sub;
  let single = measure [ (shortcut, 127) ] in
  let layered = measure bindings in
  is_true ~msg:"overrides do not add allocation to event dispatch"
    (layered <= single +. 1024.)

let cmd_copy_to_clipboard_maps_without_changing_payload () =
  match Cmd.map (fun msg -> msg) (Cmd.copy_to_clipboard "hello") with
  | Cmd.Copy_to_clipboard "hello" -> ()
  | Cmd.Copy_to_clipboard other ->
      failf "unexpected clipboard payload: %S" other
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Exec _ | Cmd.Suspend | Cmd.Quit
  | Cmd.Set_title _ | Cmd.Clear_selection | Cmd.Static_commit _
  | Cmd.Static_clear | Cmd.Copy_selection | Cmd.Query_color_scheme
  | Cmd.Query_terminal_colors | Cmd.Bell | Cmd.Notify _ | Cmd.Abort_paste
  | Cmd.Keep_frame _ | Cmd.Focus_next | Cmd.Focus_prev ->
      fail "expected Copy_to_clipboard"

let cmd_clear_selection_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.clear_selection with
  | Cmd.Clear_selection -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Exec _ | Cmd.Suspend | Cmd.Quit
  | Cmd.Set_title _ | Cmd.Copy_to_clipboard _ | Cmd.Static_commit _
  | Cmd.Static_clear | Cmd.Copy_selection | Cmd.Query_color_scheme
  | Cmd.Query_terminal_colors | Cmd.Bell | Cmd.Notify _ | Cmd.Abort_paste
  | Cmd.Keep_frame _ | Cmd.Focus_next | Cmd.Focus_prev ->
      fail "expected Clear_selection"

let cmd_copy_selection_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.copy_selection with
  | Cmd.Copy_selection -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Exec _ | Cmd.Suspend | Cmd.Quit
  | Cmd.Set_title _ | Cmd.Copy_to_clipboard _ | Cmd.Clear_selection
  | Cmd.Static_commit _ | Cmd.Static_clear | Cmd.Query_color_scheme
  | Cmd.Query_terminal_colors | Cmd.Bell | Cmd.Notify _ | Cmd.Abort_paste
  | Cmd.Keep_frame _ | Cmd.Focus_next | Cmd.Focus_prev ->
      fail "expected Copy_selection"

let cmd_query_color_scheme_maps_to_itself () =
  match Cmd.map (fun msg -> msg) Cmd.query_color_scheme with
  | Cmd.Query_color_scheme -> ()
  | Cmd.None | Cmd.Batch _ | Cmd.Perform _ | Cmd.Exec _ | Cmd.Suspend | Cmd.Quit
  | Cmd.Set_title _ | Cmd.Copy_to_clipboard _ | Cmd.Clear_selection
  | Cmd.Static_commit _ | Cmd.Static_clear | Cmd.Copy_selection
  | Cmd.Query_terminal_colors | Cmd.Bell | Cmd.Notify _ | Cmd.Abort_paste
  | Cmd.Keep_frame _ | Cmd.Focus_next | Cmd.Focus_prev ->
      fail "expected Query_color_scheme"

let sub_on_color_scheme_delivers_reports () =
  let sub =
    Sub.on_color_scheme (function `Dark -> Some Quit | `Light -> None)
  in
  match sub with
  | Sub.On_color_scheme f ->
      (match f `Dark with
      | Some Quit -> ()
      | Some _ -> fail "expected Quit for dark"
      | None -> fail "expected a message for dark");
      is_true ~msg:"light ignored" (f `Light = None)
  | _ -> fail "expected On_color_scheme sub"

let sub_on_color_scheme_maps_messages () =
  let sub = Sub.on_color_scheme (fun _ -> Some Quit) in
  let mapped : msg Sub.t = Sub.map (fun msg -> msg) sub in
  match mapped with
  | Sub.On_color_scheme f -> (
      match f `Dark with Some Quit -> () | _ -> fail "expected mapped Quit")
  | _ -> fail "expected On_color_scheme"

(* ── Terminal colors ── *)

let palette =
  let entries = Array.copy Ansi.Palette.default.entries in
  entries.(1) <- Some (0xcd, 0, 0);
  entries.(12) <- Some (0x5c, 0x5c, 0xff);
  {
    Ansi.Palette.default with
    entries;
    foreground = (0xe5, 0xe5, 0xe5);
    background = (0x10, 0x20, 0x30);
  }

(* The program asks for the pair and entries 0 to 15 of what the terminal
   holds. *)
let reported (p : Ansi.Palette.t) =
  { p with cursor = None; entries = Array.sub p.entries 0 16 }

(* Runs a program that records the colors it is given, on a terminal of
   [profile], through [steps]; the colors given, oldest first. *)
let colors_given ?(profile = { Matrix_test.Profile.vte with palette }) steps =
  let given = ref [] in
  let app =
    {
      init = (fun () -> ((), Cmd.none));
      update =
        (fun colors () ->
          given := colors :: !given;
          ((), Cmd.none));
      view = (fun () -> text "colors");
      subscriptions = (fun () -> Sub.on_terminal_colors (fun c -> Some c));
    }
  in
  let term = Matrix_test.Terminal.create ~profile ~cols:20 ~rows:2 () in
  Mosaic_test.run term app (fun s -> steps s term);
  List.rev !given

let colors_testable =
  Testable.make ~pp:Ansi.Palette.pp ~equal:Ansi.Palette.equal

(* The program asks as it begins, and the terminal's answer is the palette
   its user configured. *)
let terminal_colors_on_start () =
  equal ~msg:"the colors" (list colors_testable)
    [ reported palette ]
    (colors_given (fun _ _ -> ()))

(* The user picks another theme: the terminal reports its color scheme,
   and the program asks again and is given the new colors. *)
let light =
  {
    palette with
    entries = Array.init 256 (fun i -> Some (i, i / 2, 0));
    foreground = (0, 0, 0);
    background = (0xff, 0xff, 0xff);
  }

let terminal_colors_after_scheme () =
  let given =
    colors_given (fun s term ->
        Matrix_test.input s (Matrix_test.Terminal.set_palette term light))
  in
  equal ~msg:"the colors, then the new ones" (list colors_testable)
    [ reported palette; reported light ]
    given

(* An application that subscribes after the program began asks once, and
   the answer comes to its subscription. *)
let terminal_colors_asked_late () =
  let given = ref [] in
  let app =
    {
      init = (fun () -> (false, Cmd.none));
      update =
        (fun msg subscribed ->
          match msg with
          | `Subscribe -> (true, Cmd.query_terminal_colors)
          | `Colors c ->
              given := c :: !given;
              (subscribed, Cmd.none));
      view = (fun _ -> text "colors");
      subscriptions =
        (fun subscribed ->
          if subscribed then Sub.on_terminal_colors (fun c -> Some (`Colors c))
          else Sub.on_key (fun _ -> Some `Subscribe));
    }
  in
  let profile = { Matrix_test.Profile.vte with palette } in
  let term = Matrix_test.Terminal.create ~profile ~cols:20 ~rows:2 () in
  Mosaic_test.run term app (fun s ->
      equal ~msg:"none before" int 0 (List.length !given);
      Matrix_test.input s (Matrix_test.Terminal.text term "s"));
  equal ~msg:"the colors, once" (list colors_testable)
    [ reported palette ]
    (List.rev !given)

(* Waits until the program has asked for the colors, from a terminal that
   answers no query by itself: the run first gives up on the queries it sent
   before. *)
let asked s term =
  let output () = Matrix_test.Terminal.output term in
  let query = "\027]4;15;?" in
  let contains s sub =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
    in
    go 0
  in
  let rec wait seconds =
    if not (contains (output ()) query) then
      if seconds = 0 then fail "the program never asked"
      else (
        Matrix_test.advance s 1_000_000_000;
        wait (seconds - 1))
  in
  wait 30

(* A terminal that reports its foreground and background and no palette
   entry: its answer to DA1 ends the wait, at once, and the colors come
   without entries. A terminal that reports neither gives none. The
   terminal answers no query by itself, so the test answers the program's
   once it has asked, after the run gave up on those it sent before. *)
let terminal_colors_without_palette () =
  let profile = { Matrix_test.Profile.vte with answers = false } in
  let da1 = "\027[?62;22c" in
  let given =
    colors_given ~profile (fun s term ->
        asked s term;
        let before = Matrix_test.now s in
        Matrix_test.input s
          ("\027]10;rgb:e5e5/e5e5/e5e5\027\\\027]11;rgb:1010/2020/3030\027\\"
         ^ da1);
        equal ~msg:"no wait" int before (Matrix_test.now s))
  in
  equal ~msg:"foreground and background" (list colors_testable)
    [
      {
        Ansi.Palette.foreground = (0xe5, 0xe5, 0xe5);
        background = (0x10, 0x20, 0x30);
        cursor = None;
        entries = [||];
      };
    ]
    given;
  let given =
    colors_given ~profile (fun s term ->
        asked s term;
        Matrix_test.input s da1)
  in
  equal ~msg:"none" (list colors_testable) [] given

(* The root's theme is the default of the colors the terminal reports,
   and roles resolve as the frame is drawn: the program draws again with
   them, and builds no view. *)
(* A program whose view is a canvas that draws a link by the theme in
   scope, counting its views. *)
let link_app views =
  {
    init = (fun () -> ((), Cmd.none));
    update = (fun () () -> ((), Cmd.none));
    view =
      (fun () ->
        incr views;
        canvas ~size:(size ~width:1 ~height:1) (fun c ~dt:_ ->
            Canvas.draw_text c ~x:0 ~y:0 ~text:"x"
              ~style:(Theme.style (Canvas.theme c) Link)));
    subscriptions = (fun () -> Sub.none);
  }

(* The link's foreground and whether it is underlined, as drawn and as a
   theme gives it: the emulator shows the default colors where the style
   has none. *)
let link =
  Testable.(
    pair (option (Testable.make ~pp:Ansi.Color.pp ~equal:Ansi.Color.equal)) bool)

let drawn term =
  let (style : Ansi.Style.t) =
    snd
      (Matrix_test.Screen.cell (Matrix_test.Terminal.screen term) ~row:0 ~col:0)
  in
  (style.fg, Ansi.Attr.mem Underline style.attrs)

let link_of theme =
  let style = Theme.style theme Link in
  ( Some (Option.value style.fg ~default:Ansi.Color.default_fg),
    Ansi.Attr.mem Underline style.attrs )

let root_theme_follows_colors () =
  let profile = { Matrix_test.Profile.vte with answers = false } in
  let views = ref 0 in
  let colors =
    {
      Ansi.Palette.foreground = (0, 0, 0);
      background = (0xff, 0xff, 0xff);
      cursor = None;
      entries = Array.make 16 None;
    }
  in
  colors.entries.(4) <- Some (0, 0, 0xee);
  let term = Matrix_test.Terminal.create ~profile ~cols:4 ~rows:1 () in
  Mosaic_test.run term (link_app views) (fun s ->
      asked s term;
      equal ~msg:"plain" link (link_of Theme.plain) (drawn term);
      let views_before = !views in
      Matrix_test.input s
        ("\027]10;rgb:0000/0000/0000\027\\\027]11;rgb:ffff/ffff/ffff\027\\\027]4;4;rgb:0000/0000/eeee\027\\"
       ^ "\027[?62;22c");
      equal ~msg:"the default of the colors" link
        (link_of (Theme.default colors))
        (drawn term);
      equal ~msg:"no view built" int views_before !views)

(* The user picks another theme, in which the link's entry is another: the
   program draws with the default of the new colors, and builds no view. *)
let root_theme_follows_a_change () =
  let views = ref 0 in
  let dark =
    {
      Ansi.Palette.default with
      foreground = (0xe5, 0xe5, 0xe5);
      background = (0, 0, 0);
    }
  in
  let entries = Array.copy dark.entries in
  entries.(4) <- Some (0, 0, 0xee);
  let light =
    {
      dark with
      entries;
      foreground = (0, 0, 0);
      background = (0xff, 0xff, 0xff);
    }
  in
  let theme p = Theme.default (reported p) in
  is_false ~msg:"the themes draw the link apart"
    (link_of (theme dark) = link_of (theme light));
  let profile = { Matrix_test.Profile.vte with palette = dark } in
  let term = Matrix_test.Terminal.create ~profile ~cols:4 ~rows:1 () in
  Mosaic_test.run term (link_app views) (fun s ->
      equal ~msg:"the first colors" link (link_of (theme dark)) (drawn term);
      let views_before = !views in
      Matrix_test.input s (Matrix_test.Terminal.set_palette term light);
      equal ~msg:"the new colors" link (link_of (theme light)) (drawn term);
      equal ~msg:"no view built" int views_before !views)

let () =
  Windtrap.run "subscriptions"
    [
      test "Sub.on_tick produces messages" sub_on_tick_produces_messages;
      test "Sub.every produces messages" sub_every_produces_messages;
      test "Sub.after mapping and validation" sub_after_map_and_validation;
      test "Sub.every mapping preserves identity"
        sub_every_map_preserves_identity;
      test "timer model accumulates elapsed" timer_model_accumulates_elapsed;
      test "timer model goes idle at zero" timer_model_goes_idle_at_zero;
      test "Sub.batch flattens" sub_batch_flattens;
      test "Sub.map transforms messages" sub_map_transforms_messages;
      test "Shortcut.matches plain keys" shortcut_matches_plain_keys;
      test "Sub.on_key_release delivers releases"
        sub_on_key_release_delivers_releases;
      test "Shortcut matches ctrl base key" shortcut_matches_ctrl_base_key;
      test "Shortcut Shift with a letter is the capital"
        shortcut_shift_letter_is_the_capital;
      test "Shortcut Alt matches Matrix Alt" shortcut_alt_matches_alt;
      test "Sub.on_keys binding resolution" sub_on_keys_resolution;
      test "Sub.on_keys allocation across overrides"
        sub_on_keys_does_not_compile_or_allocate_per_override;
      test "Cmd.copy_to_clipboard maps without changing payload"
        cmd_copy_to_clipboard_maps_without_changing_payload;
      test "Cmd.clear_selection maps to itself"
        cmd_clear_selection_maps_to_itself;
      test "Cmd.copy_selection maps to itself" cmd_copy_selection_maps_to_itself;
      test "Cmd.query_color_scheme maps to itself"
        cmd_query_color_scheme_maps_to_itself;
      test "Sub.on_color_scheme delivers reports"
        sub_on_color_scheme_delivers_reports;
      test "Sub.on_color_scheme maps messages" sub_on_color_scheme_maps_messages;
      test "terminal colors as the program begins" terminal_colors_on_start;
      test "terminal colors after the user changes them"
        terminal_colors_after_scheme;
      test "terminal colors asked by a late subscriber"
        terminal_colors_asked_late;
      test "terminal colors without a palette" terminal_colors_without_palette;
      test "the root's theme follows the terminal's colors"
        root_theme_follows_colors;
      test "the root's theme follows a change of the colors"
        root_theme_follows_a_change;
    ]
