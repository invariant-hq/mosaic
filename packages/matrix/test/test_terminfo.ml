open Windtrap
module T = Matrix_terminfo

(* Compiled entries of the ncurses database (data/terminfo/LICENSE). *)
let read name =
  In_channel.with_open_bin ("data/terminfo/" ^ name) In_channel.input_all

let entry name =
  match T.of_string (read name) with
  | Ok t -> t
  | Error e -> failf "%s: %s" name e

let opt_string = Testable.(option string)
let opt_int = Testable.(option int)

(* Descriptions *)

(* The 32-bit number format, with the extended section. *)
let test_extended_number_format () =
  let t = entry "x/xterm-256color" in
  equal ~msg:"names" (list string)
    [ "xterm-256color"; "xterm with 256 colors" ]
    (T.names t);
  equal ~msg:"colors" opt_int (Some 256) (T.number t "colors");
  equal ~msg:"pairs, past 16 bits" opt_int (Some 65536) (T.number t "pairs");
  equal ~msg:"lines" opt_int (Some 24) (T.number t "lines");
  is_true ~msg:"am" (T.flag t "am");
  is_true ~msg:"xenl" (T.flag t "xenl");
  is_true ~msg:"bce" (T.flag t "bce");
  is_false ~msg:"no hc" (T.flag t "hc");
  equal ~msg:"smcup" opt_string (Some "\027[?1049h\027[22;0;0t")
    (T.string t "smcup");
  equal ~msg:"rmkx" opt_string (Some "\027[?1l\027>") (T.string t "rmkx");
  is_true ~msg:"XT, a user-defined boolean" (T.flag t "XT");
  equal ~msg:"BE, a user-defined string" opt_string (Some "\027[?2004h")
    (T.string t "BE");
  equal ~msg:"kxIN" opt_string (Some "\027[I") (T.string t "kxIN");
  equal ~msg:"no RGB" opt_string None (T.string t "RGB");
  is_false ~msg:"no RGB flag" (T.flag t "RGB");
  (* As ncurses' own reader has them (infocmp -x). *)
  equal ~msg:"every capability" int 287
    (List.length (T.flags t)
    + List.length (T.numbers t)
    + List.length (T.strings t))

let test_direct_colour () =
  let t = entry "x/xterm-direct" in
  is_true ~msg:"RGB" (T.flag t "RGB");
  equal ~msg:"colors" opt_int (Some 0x1000000) (T.number t "colors");
  let setaf = Option.get (T.string t "setaf") in
  equal ~msg:"a direct colour" string "\027[38:2::18:52:86m"
    (T.expand setaf [ Int 0x123456 ]);
  equal ~msg:"an ANSI colour" string "\027[33m" (T.expand setaf [ Int 3 ])

(* The legacy format: strings after absent ones are read. *)
let test_legacy_format () =
  let t = entry "l/linux" in
  equal ~msg:"names" (list string) [ "linux"; "Linux console" ] (T.names t);
  equal ~msg:"colors" opt_int (Some 8) (T.number t "colors");
  equal ~msg:"cup" opt_string (Some "\027[%i%p1%d;%p2%dH") (T.string t "cup");
  equal ~msg:"sgr0" opt_string (Some "\027[m\015") (T.string t "sgr0");
  equal ~msg:"cnorm" opt_string (Some "\027[?25h\027[?0c") (T.string t "cnorm");
  equal ~msg:"no smcup" opt_string None (T.string t "smcup");
  let t = entry "d/dumb" in
  equal ~msg:"dumb" (list string) [ "dumb"; "80-column dumb tty" ] (T.names t);
  equal ~msg:"dumb's flags" (list string) [ "am" ] (T.flags t);
  equal ~msg:"dumb's numbers"
    (list (pair string int))
    [ ("cols", 80) ]
    (T.numbers t);
  equal ~msg:"dumb's strings"
    (list (pair string string))
    [ ("bel", "\007"); ("cr", "\r"); ("cud1", "\n"); ("ind", "\n") ]
    (T.strings t)

let test_not_an_entry () =
  (match T.of_string "#!/bin/sh\n" with
  | Error _ -> ()
  | Ok _ -> fail "a script is no entry");
  match T.of_string "" with Error _ -> () | Ok _ -> fail "nothing is no entry"

(* Every prefix of an entry, and every entry with one byte changed, is read
   or refused: never an exception. *)
let test_damaged () =
  let s = read "x/xterm-256color" in
  for n = 0 to String.length s - 1 do
    ignore (T.of_string (String.sub s 0 n) : (T.t, string) result)
  done;
  let b = Bytes.of_string s in
  for i = 0 to String.length s - 1 do
    List.iter
      (fun v ->
        let c = Bytes.get b i in
        Bytes.set b i v;
        ignore (T.of_string (Bytes.to_string b) : (T.t, string) result);
        Bytes.set b i c)
      [ '\000'; '\001'; '\127'; '\255' ]
  done

(* Parameters, each as ncurses 6.5's tparm gives it. *)

let expansions =
  [
    ("cup", "\027[%i%p1%d;%p2%dH", [ 5; 10 ], "\027[6;11H");
    ("%c of 0", "%p1%c", [ 0 ], "\128");
    ("%c", "%p1%c", [ 65 ], "A");
    ("%c of 300", "%p1%c", [ 300 ], ",");
    ("an unknown code", "a%zb", [ 1 ], "ab");
    ("a missing operand", "%d|%d", [ 1 ], "1|0");
    ( "flags, width, precision",
      "%p1%:-5d|%p1%+d|%p1% d|%p1%#x|%p1%05d|%p1%.3d|%p1%5.3d",
      [ 7 ],
      "7    |d| 7|0x7|00007|007|  007" );
    ("negative numbers", "%p1%d|%p1%o|%p1%x", [ -5 ], "-5|37777777773|fffffffb");
    ("%i", "%i%p1%d;%p2%d;%p3%d", [ 1; 2; 3 ], "2;3;3");
    ("%i once", "%i%i%p1%d", [ 1 ], "2");
    ("constants", "%{65}%c%'B'%c%{12}%d", [], "AB12");
    ("division by zero", "%p1%{0}%/%d|%p1%{0}%m%d", [ 5 ], "0|0");
    ("else-if", "%?%p1%{1}%=%tone%e%p1%{2}%=%ttwo%ethree%;", [ 2 ], "two");
    ("nested", "%?%p1%t%?%p2%tA%eB%;%eC%;", [ 1; 0 ], "B");
    ("%A %O", "%p1%p2%A%d%p1%p2%O%d", [ 1; 0 ], "01");
    ("delays kept", "x$<5*/>y", [], "x$<5*/>y");
    ("dynamic variables", "%p1%Pa%ga%ga%+%d", [ 4 ], "8");
    ("an unset variable", "%gz%d", [ 4 ], "0");
    ("%p9", "%p9%d", [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ], "9");
    ("%%", "100%%", [], "100%");
    ("a % at the end", "abc%", [], "abc");
    ("%X %x %o", "%p1%X%p1%x%p1%o", [ 255 ], "FFff377");
    (* termcap's form, no %p: the parameters are pushed, and %i sets the
       stack's two first slots from the bottom. *)
    ("termcap's form", "\027[%i%d;%dR", [ 4; 9 ], "\027[10;5R");
  ]

let test_expand (_, s, params, expected) =
  equal ~msg:s string expected (T.expand s (List.map (fun n -> T.Int n) params))

let test_string_params () =
  equal ~msg:"%s and %l" string "hello|5"
    (T.expand "%p1%s|%p1%l%d" [ String "hello" ]);
  equal ~msg:"width and precision" string "        ab|ab        |ab"
    (T.expand "%p1%10s|%p1%:-10s|%p1%.2s" [ String "ab" ]);
  equal ~msg:"a number as a string is empty" string "[]"
    (T.expand "[%p1%s]" [ Int 5 ]);
  equal ~msg:"a string as a number is 0" string "0"
    (T.expand "%p1%d" [ String "5" ])

(* The Wyse 350's sgr and setf share the static variables A and C. *)
let test_static_variables () =
  let t = entry "w/wy350" in
  let sgr = Option.get (T.string t "sgr")
  and sgr0 = Option.get (T.string t "sgr0")
  and setf = Option.get (T.string t "setf") in
  let vars = T.vars () in
  let ints = List.map (fun n -> T.Int n) in
  equal ~msg:"sgr0" string "\027G0\027(\027H\003" (T.expand ~vars sgr0 []);
  equal ~msg:"setf 2" string "\027G8" (T.expand ~vars setf (ints [ 2 ]));
  equal ~msg:"sgr reads C" string "\027G:\027(\027H\003"
    (T.expand ~vars sgr (ints [ 0; 0; 0; 1 ]));
  equal ~msg:"setf reads A" string "\027G:" (T.expand ~vars setf (ints [ 2 ]));
  equal ~msg:"fresh variables" string "\027G8" (T.expand setf (ints [ 2 ]))

(* Delays *)

let piece =
  Testable.make
    ~pp:(fun ppf -> function
      | T.Text s -> Format.fprintf ppf "Text %S" s
      | Delay { ms; per_line; mandatory } ->
          Format.fprintf ppf "Delay %g%s%s" ms
            (if per_line then "*" else "")
            (if mandatory then "/" else ""))
    ~equal:( = )

let test_pieces () =
  equal ~msg:"the visible bell" (list piece)
    [
      Text "\027[?5h";
      Delay { ms = 100.; per_line = false; mandatory = true };
      Text "\027[?5l";
    ]
    (T.pieces "\027[?5h$<100/>\027[?5l");
  equal ~msg:"per line, a decimal" (list piece)
    [ Delay { ms = 5.5; per_line = true; mandatory = false }; Text "x" ]
    (T.pieces "$<5.5*>x");
  equal ~msg:"both marks" (list piece)
    [ Delay { ms = 2.; per_line = true; mandatory = true } ]
    (T.pieces "$<2/*>");
  List.iter
    (fun s -> equal ~msg:s (list piece) [ Text s ] (T.pieces s))
    [ "$<5.55>"; "$<>"; "a$<3"; "$<3**>"; "$5" ];
  equal ~msg:"nothing" (list piece) [] (T.pieces "")

let () =
  exit
    (run "matrix.terminfo"
       [
         group "descriptions"
           [
             test "the 32-bit format" test_extended_number_format;
             test "direct colour" test_direct_colour;
             test "the legacy format" test_legacy_format;
             test "not an entry" test_not_an_entry;
             test "damaged entries" test_damaged;
           ];
         group "parameters"
           [
             cases "expand" expansions ~id:(fun (n, _, _, _) -> n) test_expand;
             test "string parameters" test_string_params;
             test "static variables" test_static_variables;
           ];
         group "delays" [ test "pieces" test_pieces ];
       ])
