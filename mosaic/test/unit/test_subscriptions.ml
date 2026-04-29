(** Tests for the TEA subscription mechanism.

    These tests verify that [Sub.on_tick] and [Sub.every] subscriptions produce
    correct messages when driven by a simulated TEA runtime. The runtime is not
    real -- we manually call the same logic that the on_frame callback uses. *)

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

type msg = Tick of float | Every_fired | Quit | Copy | Submit

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
  | Quit | Copy | Submit -> m

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
  let sub = Sub.on_tick (fun ~dt -> Tick dt) in
  match sub with
  | Sub.On_tick f -> (
      let msg = f ~dt:0.016 in
      match msg with
      | Tick dt -> equal ~msg:"dt" (float 0.001) 0.016 dt
      | _ -> fail "expected Tick msg")
  | _ -> fail "expected On_tick sub"

let sub_every_produces_messages () =
  (* Verify that Sub.every produces a message when called. *)
  let sub = Sub.every 1.0 (fun () -> Every_fired) in
  match sub with
  | Sub.Every (interval, f) -> (
      equal ~msg:"interval" (float 0.001) 1.0 interval;
      let msg = f () in
      match msg with Every_fired -> () | _ -> fail "expected Every_fired msg")
  | _ -> fail "expected Every sub"

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
        Sub.on_tick (fun ~dt -> Tick dt);
        Sub.on_key (fun _ -> None);
        Sub.every 2.0 (fun () -> Every_fired);
      ]
  in
  let flat = flatten_sub subs in
  equal ~msg:"3 subs" int 3 (List.length flat)

let sub_map_transforms_messages () =
  (* Verify that Sub.map correctly transforms messages. *)
  let sub = Sub.on_tick (fun ~dt -> Tick dt) in
  let mapped : msg Sub.t = Sub.map (fun msg -> msg) sub in
  match mapped with
  | Sub.On_tick f -> (
      match f ~dt:0.5 with
      | Tick dt -> equal ~msg:"mapped dt" (float 0.001) 0.5 dt
      | _ -> fail "expected Tick")
  | _ -> fail "expected On_tick"

let key ?modifier ?event_type ?base_key key =
  Event.Key.of_input (Matrix.Input.Key.make ?modifier ?event_type ?base_key key)

let char ?modifier ?event_type ?base_key c =
  key ?modifier ?event_type ?base_key (Matrix.Input.Key.Char (Uchar.of_char c))

let shortcut_matches_plain_keys () =
  is_true ~msg:"char shortcut" (Shortcut.matches (Shortcut.char 'q') (char 'q'));
  is_true ~msg:"escape shortcut"
    (Shortcut.matches Shortcut.escape (key Matrix.Input.Key.Escape));
  is_false ~msg:"wrong char" (Shortcut.matches (Shortcut.char 'q') (char 'x'))

let shortcut_ignores_key_release () =
  let ev = char ~event_type:Matrix.Input.Key.Release 'q' in
  is_false ~msg:"release ignored" (Shortcut.matches (Shortcut.char 'q') ev)

let shortcut_matches_ctrl_base_key () =
  let ev =
    key
      ~modifier:{ Matrix.Input.Modifier.none with ctrl = true }
      ~base_key:(Uchar.of_char 'c')
      (Matrix.Input.Key.Char (Uchar.of_int 0x0441))
  in
  is_true ~msg:"base-layout fallback" (Shortcut.matches (Shortcut.ctrl 'c') ev)

let shortcut_matches_legacy_shift_char () =
  let ev =
    char ~modifier:{ Matrix.Input.Modifier.none with shift = true } 'A'
  in
  is_true ~msg:"legacy uppercase fallback"
    (Shortcut.matches (Shortcut.char ~shift:true 'a') ev)

let shortcut_alt_matches_matrix_alt_meta () =
  let ev =
    char
      ~modifier:{ Matrix.Input.Modifier.none with alt = true; meta = true }
      'x'
  in
  is_true ~msg:"Alt also requires Matrix meta"
    (Shortcut.matches (Shortcut.alt 'x') ev)

let sub_on_keys_uses_first_matching_binding () =
  let sub =
    Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.char 'q', Submit) ]
  in
  match sub with
  | Sub.On_key f -> (
      match f (char 'q') with
      | Some Quit -> ()
      | Some Copy -> fail "expected Quit, got Copy"
      | Some Submit -> fail "expected Quit, got Submit"
      | Some (Tick dt) -> failf "expected Quit, got Tick %f" dt
      | Some Every_fired -> fail "expected Quit, got Every_fired"
      | None -> fail "expected Quit")
  | _ -> fail "expected On_key"

let () =
  Windtrap.run "subscriptions"
    [
      test "Sub.on_tick produces messages" sub_on_tick_produces_messages;
      test "Sub.every produces messages" sub_every_produces_messages;
      test "timer model accumulates elapsed" timer_model_accumulates_elapsed;
      test "timer model goes idle at zero" timer_model_goes_idle_at_zero;
      test "Sub.batch flattens" sub_batch_flattens;
      test "Sub.map transforms messages" sub_map_transforms_messages;
      test "Shortcut.matches plain keys" shortcut_matches_plain_keys;
      test "Shortcut ignores key release" shortcut_ignores_key_release;
      test "Shortcut matches ctrl base key" shortcut_matches_ctrl_base_key;
      test "Shortcut matches legacy shift char"
        shortcut_matches_legacy_shift_char;
      test "Shortcut Alt matches Matrix Alt/Meta"
        shortcut_alt_matches_matrix_alt_meta;
      test "Sub.on_keys first match" sub_on_keys_uses_first_matching_binding;
    ]
