(* bench_input.ml *)

module I = Input
open Thumper

(* Helpers shared with text benchmarks for consistency. *)

let repeat chunk count =
  let buffer = Buffer.create (String.length chunk * count) in
  for _ = 1 to count do
    Buffer.add_string buffer chunk
  done;
  Buffer.contents buffer

let cycle_concat parts repeat_count =
  let len = Array.length parts in
  let total_len =
    Array.fold_left (fun acc s -> acc + String.length s) 0 parts
  in
  let avg_len = if len = 0 then 0 else (total_len + len - 1) / len in
  let buffer = Buffer.create (max total_len (avg_len * repeat_count)) in
  for i = 0 to repeat_count - 1 do
    Buffer.add_string buffer parts.(i mod len)
  done;
  Buffer.contents buffer

(* Synthetic but realistic workloads *)

(* ASCII-heavy line representative of CI logs / code / typical TUI text. *)
let ascii_line =
  repeat
    "The quick brown fox jumps over the lazy dog. Pack my box with five dozen \
     liquor jugs. 2024-01-02T12:34:56Z INFO matrix.input CI job #1234 \
     succeeded. "
    8

(* A small, reusable hot set of complex graphemes: emoji, flags, Indic,
   CJK... *)
let unicode_graphemes =
  [|
    "👩\u{200D}🚀";
    "🇫🇷";
    "👍🏽";
    "🏳️‍🌈";
    "\u{0915}\u{094D}\u{0937}";
    "漢";
    "e\u{0301}";
    "🚴\u{200D}♀️";
  |]

(* Emoji / complex text heavy line: chat, dashboards, fancy prompts. *)
let complex_line = cycle_concat unicode_graphemes 256

(* 1. TUI-style typing: ASCII and Unicode heavy text *)

(* Scenario: editor / REPL / TUI with mostly ASCII text, arriving in small
   bursts (multiple feed calls, partial reads, no escapes). *)
let typing_ascii_burst =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string ascii_line in
  let len = Bytes.length bytes in
  let chunk_size = 32 in
  let count = ref 0 in
  bench "typing/ascii-burst" (fun () ->
      count := 0;
      let rec loop offset =
        if offset >= len then ()
        else begin
          let chunk = min chunk_size (len - offset) in
          I.Parser.feed parser bytes offset chunk ~now:0.0
            ~on_event:(fun _ -> incr count)
            ~on_response:(fun _ -> incr count);
          loop (offset + chunk)
        end
      in
      loop 0;
      ignore (Sys.opaque_identity !count))

(* Scenario: same as above but for Unicode-heavy input, including emoji and
   complex graphemes, with UTF-8 splits across chunk boundaries. *)
let typing_unicode_burst =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string complex_line in
  let len = Bytes.length bytes in
  let chunk_size = 48 in
  let count = ref 0 in
  bench "typing/unicode-burst" (fun () ->
      count := 0;
      let rec loop offset =
        if offset >= len then ()
        else begin
          let chunk = min chunk_size (len - offset) in
          I.Parser.feed parser bytes offset chunk ~now:0.0
            ~on_event:(fun _ -> incr count)
            ~on_response:(fun _ -> incr count);
          loop (offset + chunk)
        end
      in
      loop 0;
      ignore (Sys.opaque_identity !count))

(* 2. Legacy keyboard sequences (CSI, SS3, ESC+char) *)

(* Synthetic stream of "hot" key sequences: arrows, home/end, delete, function
   keys, and Alt-modified keys using classic ANSI encodings. *)
let legacy_keyboard_stream =
  let buf = Buffer.create 2048 in
  for _ = 1 to 128 do
    (* Arrow keys (CSI) *)
    Buffer.add_string buf "\x1b[A";
    (* Up *)
    Buffer.add_string buf "\x1b[B";
    (* Down *)
    Buffer.add_string buf "\x1b[C";
    (* Right *)
    Buffer.add_string buf "\x1b[D";
    (* Left *)
    (* Home/End and Delete via CSI/tilde codes *)
    Buffer.add_string buf "\x1b[H";
    (* Home *)
    Buffer.add_string buf "\x1b[F";
    (* End *)
    Buffer.add_string buf "\x1b[3~";
    (* Delete *)
    Buffer.add_string buf "\x1b[5~";
    (* Page Up *)
    Buffer.add_string buf "\x1b[6~";
    (* Page Down *)
    (* Modified arrow (e.g. Ctrl+Right) *)
    Buffer.add_string buf "\x1b[1;5C";
    (* Function keys via SS3 sequences *)
    Buffer.add_string buf "\x1bOP";
    (* F1 *)
    Buffer.add_string buf "\x1bOQ";
    (* F2 *)
    Buffer.add_string buf "\x1bOR";
    (* F3 *)
    Buffer.add_string buf "\x1bOS";
    (* F4 *)
    (* Alt-modified printable and Backspace: ESC prefix + char/control. *)
    Buffer.add_string buf "\x1bA";
    (* Alt+A *)
    Buffer.add_string buf "\x1bb";
    (* Alt+b *)
    Buffer.add_string buf "\x1b\x7f" (* Alt+Backspace *)
  done;
  Buffer.contents buf

let keyboard_legacy_hot =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string legacy_keyboard_stream in
  let len = Bytes.length bytes in
  let count = ref 0 in
  bench "keyboard/legacy-hot" (fun () ->
      count := 0;
      I.Parser.feed parser bytes 0 len ~now:0.0
        ~on_event:(fun _ -> incr count)
        ~on_response:(fun _ -> incr count);
      ignore (Sys.opaque_identity !count))

(* 3. Kitty keyboard protocol (CSI-u) *)

let kitty_key_seq ~code ~mods ~text =
  (* CSI code;mods;text u *)
  Printf.sprintf "\x1b[%d;%d;%su" code mods text

(* A small hot set of common Kitty-keyboard events: - Shift+A (code=65, mods=1)
   - Ctrl+Shift+C (code=67, mods=5 = shift(1) + ctrl(4)) - Shift+Enter (code=13,
   mods=1) - Alt+Tab (code=9, mods=2) *)
let kitty_keyboard_stream =
  let combos =
    [
      (65, 1, "65");
      (* Shift+A *)
      (67, 5, "67");
      (* Ctrl+Shift+C *)
      (13, 1, "13");
      (* Shift+Enter *)
      (9, 2, "9");
      (* Alt+Tab *)
    ]
  in
  let buf = Buffer.create 4096 in
  for _ = 1 to 256 do
    List.iter
      (fun (code, mods, text) ->
        Buffer.add_string buf (kitty_key_seq ~code ~mods ~text))
      combos
  done;
  Buffer.contents buf

let keyboard_kitty_hot =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string kitty_keyboard_stream in
  let len = Bytes.length bytes in
  let count = ref 0 in
  bench "keyboard/kitty-hot" (fun () ->
      count := 0;
      I.Parser.feed parser bytes 0 len ~now:0.0
        ~on_event:(fun _ -> incr count)
        ~on_response:(fun _ -> incr count);
      ignore (Sys.opaque_identity !count))

(* 4. Mouse input: X10 + SGR mixed workload *)

(* X10/normal mouse: ESC [ M <btn> <x> <y> with all fields as single bytes. *)
let x10_mouse_seq cb x y =
  let btn_ch = Char.chr (32 + cb) in
  let x_ch = Char.chr (33 + x) in
  let y_ch = Char.chr (33 + y) in
  let bytes = Bytes.create 6 in
  Bytes.set bytes 0 '\x1b';
  Bytes.set bytes 1 '[';
  Bytes.set bytes 2 'M';
  Bytes.set bytes 3 btn_ch;
  Bytes.set bytes 4 x_ch;
  Bytes.set bytes 5 y_ch;
  Bytes.unsafe_to_string bytes

(* SGR mouse: ESC [ < btn ; x ; y M/m *)
let sgr_mouse_seq btn x y final =
  Printf.sprintf "\x1b[<%d;%d;%d%c" btn x y final

(* Mixed mouse workload: - X10 presses/releases - SGR presses, motion, releases
   - Wheel scroll (up/down) *)
let mouse_stream =
  let buf = Buffer.create 4096 in
  for y = 5 to 20 do
    (* X10: left press + generic release *)
    Buffer.add_string buf (x10_mouse_seq 0 10 y);
    (* press (btn=0) *)
    Buffer.add_string buf (x10_mouse_seq 3 10 y);
    (* release (btn=3 means release) *)
    (* SGR: left press, motion with button held, release *)
    Buffer.add_string buf (sgr_mouse_seq 0 30 y 'M');
    (* press *)
    Buffer.add_string buf (sgr_mouse_seq 32 32 (y + 1) 'M');
    (* motion (btn+motion flag) *)
    Buffer.add_string buf (sgr_mouse_seq 3 30 y 'm');
    (* release *)
    (* SGR: scroll up and down (buttons 64/65) *)
    Buffer.add_string buf (sgr_mouse_seq 64 40 (y + 2) 'M');
    (* wheel up *)
    Buffer.add_string buf (sgr_mouse_seq 65 40 (y + 2) 'M')
    (* wheel down *)
  done;
  Buffer.contents buf

let mouse_mixed_hot =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string mouse_stream in
  let len = Bytes.length bytes in
  let count = ref 0 in
  bench "mouse/mixed" (fun () ->
      count := 0;
      I.Parser.feed parser bytes 0 len ~now:0.0
        ~on_event:(fun _ -> incr count)
        ~on_response:(fun _ -> incr count);
      ignore (Sys.opaque_identity !count))

(* 5. Bracketed paste with CI-style logs + ANSI stripping *)

(* CI-like log lines, including color sequences and a bit of Unicode. *)
let ci_log_line_plain =
  "2024-01-02T12:34:56Z INFO  matrix.input CI job #1234 finished in 1234ms\n"

let ci_log_line_err =
  "2024-01-02T12:34:57Z \x1b[31mERROR\x1b[0m matrix.input: Unicode 👩‍🚀 test failed\n"

let ci_log_payload = repeat (ci_log_line_plain ^ ci_log_line_err) 64

(* Bracketed paste payload: ESC[200~ <payload> ESC[201~ *)
let bracketed_paste_stream = "\x1b[200~" ^ ci_log_payload ^ "\x1b[201~"

let paste_ci_log =
  let parser = I.Parser.create () in
  let bytes = Bytes.of_string bracketed_paste_stream in
  let len = Bytes.length bytes in
  let chunk_size = 1024 in
  let count = ref 0 in
  bench "paste/ci-log" (fun () ->
      (* For bracketed paste we want a clean parser state each run. *)
      I.Parser.reset parser;
      count := 0;
      let rec loop offset =
        if offset >= len then ()
        else begin
          let chunk = min chunk_size (len - offset) in
          I.Parser.feed parser bytes offset chunk ~now:0.0
            ~on_event:(fun _ -> incr count)
            ~on_response:(fun _ -> incr count);
          loop (offset + chunk)
        end
      in
      loop 0;
      ignore (Sys.opaque_identity !count))

(* Benchmark tree *)

let benchmarks =
  [
    group "input"
      [
        (* TUI/editor typing workloads *)
        typing_ascii_burst;
        typing_unicode_burst;
        (* Keyboard hot paths: legacy + Kitty protocol *)
        keyboard_legacy_hot;
        keyboard_kitty_hot;
        (* Pointer-heavy TUI / terminal apps *)
        mouse_mixed_hot;
        (* CI logs pasted into a TUI, with ANSI stripping *)
        paste_ci_log;
      ];
  ]

let () = run "input" benchmarks
