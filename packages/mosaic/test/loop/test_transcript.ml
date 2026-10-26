(* The runtime benchmark's transcript, on an emulated terminal: a scroll box
   of three-row messages over an input line. Per-input cost must not grow
   with the messages the viewport hides, so a key or a wheel burst allocates
   the same over 5,000 messages as over 20,000. Both counts come from one
   process, so the check holds on any machine, compiler and profile. What
   each input writes to the terminal is deterministic, and printed. *)

let caps : Matrix.Terminal.capabilities =
  {
    colors = `Truecolor;
    kitty_keyboard = None;
    kitty_graphics = false;
    bracketed_paste = true;
    focus_tracking = true;
    unicode_width = `Unicode;
    grapheme_mode = None;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = false;
    scaled_text = false;
    sixel = false;
    graphics = `Blocks;
    sync = false;
    hyperlinks = false;
    notification = `Osc9;
    meta_sends_escape = None;
    multiplexer = None;
    identity = None;
  }

type model = { editor : Mosaic.Editor.t; scroll : Mosaic.Scroll.t }
type msg = Edit of Mosaic.Editor.t | Scroll of Mosaic.Scroll.t

let block i =
  Mosaic.box ~key:(string_of_int i) ~flex_direction:Mosaic.Flex_direction.Column
    ~flex_shrink:0.
    [
      Mosaic.text (Printf.sprintf "message %04d" i);
      Mosaic.text
        "The worker completed its checkpoint; the next request can reuse the \
         retained result.";
      Mosaic.text "";
    ]

let blocks_view blocks =
  Mosaic.fragment (List.init blocks (fun i -> Mosaic.memo ~key:i block))

let view blocks model =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [
      Mosaic.scroll_box ~flex_grow:1. ~flex_basis:(Mosaic.px 0)
        ~min_size:(Mosaic.size ~width:0 ~height:0)
        ~scroll:model.scroll
        ~on_scroll:(fun next -> Scroll next)
        ~show_scrollbars:false
        [ Mosaic.memo ~key:blocks blocks_view ];
      Mosaic.input
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1))
        ~flex_shrink:0. ~focused:true ~state:model.editor
        ~on_change:(fun next -> Edit next)
        ();
    ]

let transcript blocks =
  {
    Mosaic.init =
      (fun () ->
        ( {
            editor = Mosaic.Editor.of_string "a";
            scroll = Mosaic.Scroll.at 7500;
          },
          Mosaic.Cmd.none ));
    update =
      (fun msg model ->
        match msg with
        | Edit editor -> ({ model with editor }, Mosaic.Cmd.none)
        | Scroll scroll -> ({ model with scroll }, Mosaic.Cmd.none));
    view = view blocks;
    subscriptions = (fun _ -> Mosaic.Sub.none);
  }

let profile = { Matrix_test.Profile.pipe with width_method = `Unicode }

(* Runs the transcript of [blocks] messages on a 250x70 terminal, then [f]
   on its session. *)
let with_transcript blocks f =
  let term = Matrix_test.Terminal.create ~profile ~cols:250 ~rows:70 () in
  Mosaic_test.run ~config:(Matrix.config ~mode:`Alt ~caps ())
    term (transcript blocks) (fun s -> f s term)

(* Eight wheel events over the transcript's second row. *)
let wheel button =
  String.concat ""
    (List.init 8 (fun _ -> Printf.sprintf "\027[<%d;2;2M" button))

let type_and_erase s =
  Matrix_test.input s "x";
  Matrix_test.input s "\127"

let wheel_roundtrip s =
  Matrix_test.input s (wheel 65);
  Matrix_test.input s (wheel 64)

(* The words [f] allocates on the transcript of [blocks] messages, after a
   first call that warms its caches. *)
let words blocks f =
  with_transcript blocks (fun s term ->
      let written = String.length (Matrix_test.Terminal.output term) in
      f s;
      if String.length (Matrix_test.Terminal.output term) = written then
        failwith "the input presented no frame";
      let minor, promoted, major = Gc.counters () in
      f s;
      let minor', promoted', major' = Gc.counters () in
      Float.to_int
        (minor' -. minor +. (major' -. major) -. (promoted' -. promoted)))

let%expect_test "per-input allocation does not grow with the transcript" =
  List.iter
    (fun (name, f) ->
      let small = words 5_000 f and large = words 20_000 f in
      if small = large then Printf.printf "%s: the same\n" name
      else
        Printf.printf "%s: %d words over 5,000 messages, %d over 20,000\n" name
          small large)
    [
      ("type and erase", type_and_erase); ("wheel round trip", wheel_roundtrip);
    ];
  [%expect
    {|
    type and erase: the same
    wheel round trip: the same
    |}]

(* Bytes per change: what each input has the transcript write. Output is
   deterministic for fixed capabilities, so a change in what an input costs
   the terminal shows here as a changed number. *)
let%expect_test "bytes each input writes" =
  with_transcript 5_000 (fun s term ->
      let step name f =
        let before = String.length (Matrix_test.Terminal.output term) in
        f ();
        Printf.printf "%-24s %6d bytes\n" name
          (String.length (Matrix_test.Terminal.output term) - before)
      in
      step "type a key" (fun () -> Matrix_test.input s "x");
      step "erase it" (fun () -> Matrix_test.input s "\127");
      step "wheel down 8 lines" (fun () -> Matrix_test.input s (wheel 65));
      step "wheel up 8 lines" (fun () -> Matrix_test.input s (wheel 64));
      step "resize to 249x69" (fun () ->
          Matrix_test.resize s ~cols:249 ~rows:69);
      step "resize back to 250x70" (fun () ->
          Matrix_test.resize s ~cols:250 ~rows:70);
      step "a second idle" (fun () -> Matrix_test.advance s 1_000_000_000));
  [%expect
    {|
    type a key                   49 bytes
    erase it                     42 bytes
    wheel down 8 lines          651 bytes
    wheel up 8 lines            606 bytes
    resize to 249x69          17703 bytes
    resize back to 250x70     18029 bytes
    a second idle                 0 bytes
    |}]
