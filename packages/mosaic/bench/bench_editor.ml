open Mosaic_ui

let small_text = String.make 128 'x'
let large_text = String.make 1_000_000 'x'
let persistent_cursor editor = editor |> Editor.move_left |> Editor.move_right
let persistent_insert editor = Editor.insert "y" editor

(* The document of the cross-framework benchmark's editor (B4): 10,000 lines
   of 25 to 170 bytes, about 500 KB, every tenth one wider than the 118 cells
   the editor shows. *)
let document =
  let words =
    [|
      "amber harbor orbit velvet cedar juniper.";
      "delta kestrel raven yarrow fable meadow.";
      "grove nectar umber basil indigo pepper..";
      "juniper quartz xenon ember lumen saffro.";
      "meadow tundra amber harbor orbit velvet.";
      "pepper willow delta kestrel raven yarro.";
      "saffron zephyr grove nectar umber basil.";
      "velvet cedar juniper quartz xenon ember.";
      "yarrow fable meadow tundra amber harbor.";
      "basil indigo pepper willow delta kestre.";
    |]
  in
  let line n =
    if n mod 10 = 0 then
      Printf.sprintf "%05d " n
      ^ String.concat " " (List.init 4 (fun j -> words.((n + j) mod 10)))
    else
      String.trim
        (Printf.sprintf "%05d " n
        ^ String.sub words.(n mod 10) 0 (20 + (n mod 21)))
  in
  String.concat "" (List.init 10_000 (fun n -> line (n + 1) ^ "\n"))

(* Typing at the start of line 5,000, as a key in the editor: the edited
   value, laid out at the editor's width, and the caret's row. Setup lays the
   document out, as the editor's first frame does. *)
let line_5000 wrap () =
  let editor = Editor.of_string document in
  let source = Editor.source editor in
  let editor =
    Editor.set_cursor (Matrix_text.Layout.Source.line_start source 4_999) editor
  in
  ignore
    (Matrix_text.Layout.create (Editor.source editor) ~width:118 ~wrap
      : Matrix_text.Layout.t);
  (editor, wrap)

let type_at_line (editor, wrap) =
  let edited = Editor.insert "y" editor in
  let layout =
    Matrix_text.Layout.create (Editor.source edited) ~width:118 ~wrap
  in
  Matrix_text.Layout.locate layout (Editor.caret edited)

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "editor"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words; promoted_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
         ]))
    [
      Thumper.bench_with_setup
        ~setup:(fun () -> Editor.of_string small_text)
        "cursor/persistent-128b" persistent_cursor;
      Thumper.bench_with_setup
        ~setup:(fun () -> Editor.of_string large_text)
        "cursor/persistent-1mb" persistent_cursor;
      Thumper.bench_with_setup
        ~setup:(fun () ->
          Editor.of_string large_text |> Editor.set_cursor 500_000)
        "insert/persistent-1mb" persistent_insert;
      Thumper.bench_with_setup ~setup:(line_5000 `None) "insert/b4-line-5000"
        type_at_line;
      Thumper.bench_with_setup ~setup:(line_5000 `Word)
        "insert/b4-line-5000-word" type_at_line;
    ]
