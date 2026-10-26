module Grid = Matrix_grid

let repeat count text = String.concat "" (List.init count (fun _ -> text))

let feed_run name width_method text =
  let vte = Vte.create ~scrollback:0 ~width_method ~rows:24 ~cols:80 () in
  let bytes = Bytes.of_string text in
  Thumper.bench name (fun () ->
      Vte.reset vte;
      ignore (Sys.opaque_identity (Vte.feed vte bytes 0 (Bytes.length bytes)));
      ignore (Sys.opaque_identity (Vte.cursor_pos vte)))

let ascii size =
  feed_run (Printf.sprintf "feed/ascii-%d" size) `Unicode (String.make size 'x')

let unicode width_method name = feed_run name width_method (repeat 1024 "a中é👨‍💻")

(* Sixteen frames as a program draws them, each a synchronized update of 24
   positioned, colored rows: [feed] stops at each update's two ends. *)
let frames_text =
  let row k y =
    Printf.sprintf "\027[%d;1H\027[3%dmrow %02d of frame %02d\027[0m\027[K"
      (y + 1) (y mod 8) y k
  in
  let frame k =
    "\027[?2026h" ^ String.concat "" (List.init 24 (row k)) ^ "\027[?2026l"
  in
  String.concat "" (List.init 16 frame)

let history_text =
  repeat 256 "\027[31mcolored\027[0m plain \027[44m    \027[0m\r\n"

let scrollback_append =
  let vte = Vte.create ~scrollback:512 ~rows:24 ~cols:80 () in
  let bytes = Bytes.of_string history_text in
  Thumper.bench "scrollback/append-styled" (fun () ->
      Vte.reset vte;
      ignore (Sys.opaque_identity (Vte.feed vte bytes 0 (Bytes.length bytes)));
      ignore (Sys.opaque_identity (Vte.scrollback_size vte)))

let scrollback_render =
  let vte = Vte.create ~scrollback:512 ~rows:24 ~cols:80 () in
  ignore (Vte.feed_string vte history_text : _ list);
  let dst = Grid.create ~width:80 ~height:24 () in
  Thumper.bench "scrollback/render-styled" (fun () ->
      for y = 0 to 23 do
        Vte.blit_row vte (y - 100) dst ~y
      done)

(* Exact allocation guards remain stable when host load makes timing noisy. *)
(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "vte"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      :: (if exact then [] else [ Thumper.Budget.no_slower_than 0.05 ]))
    [
      Thumper.group "vte"
        [
          ascii 40;
          feed_run "feed/unicode-short" `Unicode (repeat 4 "a中é👨‍💻");
          ascii 4096;
          ascii 16384;
          ascii 65536;
          unicode `Unicode "feed/unicode";
          unicode `Wcwidth "feed/wcwidth";
          feed_run "feed/updates" `Unicode frames_text;
          scrollback_append;
          scrollback_render;
        ];
    ]
