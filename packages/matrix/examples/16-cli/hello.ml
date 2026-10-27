(** Styled output on standard output, as [--color] and the environment decide.

    [--color=auto] (the default) writes colors to a terminal, bold and the other
    attributes without colors under [NO_COLOR], and plain text to a pipe or a
    file. [--color=always] and [--color=never] override the environment. The
    last line names the styling that was decided. *)

let color = ref `Auto

let () =
  let choose s = color := List.assoc s Ansi.Styling.choices in
  Arg.parse
    [
      ( "--color",
        Arg.Symbol (List.map fst Ansi.Styling.choices, choose),
        " When to write styles." );
    ]
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    "hello [--color=WHEN]"

(* Each piece resets its style, so that the next one starts from none. *)
let paint = Ansi.Style.styled ~reset:true
let bold = Ansi.Style.make ~bold:true ()
let dim = Ansi.Style.make ~dim:true ()
let ok = Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ()
let warn = Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ()
let error = Ansi.Style.make ~bold:true ~fg:Ansi.Color.red ()

(* 24-bit colors, which the output reduces to the depth it decided. *)
let gradient =
  String.concat ""
    (List.init 24 (fun i ->
         let c = Ansi.Color.of_rgb (255 - (i * 10)) (40 + (i * 8)) (i * 10) in
         paint (Ansi.Style.make ~fg:c ()) "█"))

let styling = function
  | `Plain -> "plain"
  | `Mono -> "attributes only"
  | `Ansi8 -> "8 colors"
  | `Ansi16 -> "16 colors"
  | `Ansi256 -> "256 colors"
  | `Truecolor -> "24-bit colors"

let () =
  let out = Matrix_cli_unix.stdout ~color:!color () in
  let link =
    Ansi.to_string (Ansi.hyperlink ~url:"https://no-color.org" ~text:"NO_COLOR")
  in
  Matrix_cli_unix.print out
    (String.concat "\n"
       [
         paint ok "✓" ^ " Built " ^ paint bold "12 targets" ^ " in 0.84s";
         paint warn "!" ^ " lib/parser.ml:14: unused variable "
         ^ paint bold "retries";
         paint error "✗" ^ " test/lexer.ml: " ^ paint bold "2 of 31"
         ^ " tests failed";
         "  " ^ gradient;
         paint dim ("  styling: " ^ styling (Matrix_cli_unix.styling out))
         ^ paint dim ", see " ^ link;
         "";
       ])
