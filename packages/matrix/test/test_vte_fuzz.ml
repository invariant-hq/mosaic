(** Robustness fuzzing of the escape-sequence parser and the VTE.

    A terminal reads whatever a program writes, so every byte string is an
    input, and every sequence of resizes and resets between them. These
    properties draw raw bytes, UTF-8-heavy text and escape-heavy streams, and
    check:

    - {b Totality}: nothing raises, neither feeding nor any accessor.
    - {b Invariants}, after every operation: the cursor is on the screen, and
      the Last Column Flag only in the last column with autowrap on; no cell
      holds a control character; every grapheme spans whole cells; wrap marks
      exist only on rows that exist.
    - {b Split invariance} on arbitrary bytes: the state and the replies do not
      depend on how the bytes were cut.
    - {b Parser tokens}: the same for every chunking once adjacent [Text] and
      [Data] are joined; [Text] and [Data] are valid UTF-8 of the characters
      they may hold; a string's tokens nest.

    The counts run in a few seconds. [MATRIX_FUZZ_SCALE=N] multiplies them, for
    long sessions. *)

module Grid = Matrix_grid
module Parser = Ansi.Parser
open Windtrap

let scale =
  match Sys.getenv_opt "MATRIX_FUZZ_SCALE" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 1)
  | None -> 1

let count n = n * scale

(* Generators ------------------------------------------------------------- *)

let utf8 cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

(* Any byte. *)
let raw = Gen.string_of ~size:(Gen.int_range 0 120) Gen.char

(* Scalars from the ranges that exercise the print path: ASCII, Latin-1 and
   C1, combining marks, Hangul jamo, format characters, Prepend, CJK, emoji
   with their modifiers and joiners, regional indicators, private use, the
   last planes. *)
let scalar =
  let range lo hi = Gen.map utf8 (Gen.int_range lo hi) in
  Gen.frequency
    [
      (6, range 0x20 0x7E);
      (2, range 0x80 0xFF);
      (3, range 0x300 0x36F);
      (1, range 0x1AB0 0x1AFF);
      (1, range 0x20D0 0x20FF);
      (2, range 0x1100 0x11FF);
      (1, range 0xD7B0 0xD7FF);
      (2, range 0x200B 0x206F);
      ( 1,
        Gen.of_list
          (List.map utf8
             [ 0x600; 0x601; 0x6DD; 0x70F; 0xD4E; 0x110BD; 0x111C2 ]) );
      (2, range 0x4E00 0x4E40);
      (1, range 0xFF01 0xFF60);
      (2, range 0x1F300 0x1F64F);
      (1, range 0x1F3FB 0x1F3FF);
      (1, range 0x1F1E6 0x1F1FF);
      ( 1,
        Gen.of_list
          (List.map utf8 [ 0x200D; 0xFE0E; 0xFE0F; 0x20E3; 0x2764; 0x26A0 ]) );
      (1, range 0x900 0x97F);
      (1, range 0xE000 0xE0FF);
      (1, range 0x10000 0x10FFFF);
    ]

(* Bytes that are not well-formed UTF-8. *)
let ill_formed =
  Gen.of_list
    [
      "\x80";
      "\xbf";
      "\xc0\x80";
      "\xc2";
      "\xe4\xb8";
      "\xed\xa0\x80";
      "\xef\xbf";
      "\xf0\x9f\x98";
      "\xf4\x90\x80\x80";
      "\xf5";
      "\xff";
      "\xfe";
    ]

let utf8_heavy =
  let fragment =
    Gen.frequency
      [
        (8, scalar);
        (2, ill_formed);
        (1, Gen.of_list [ "\r"; "\n"; "\b"; "\t"; "\x1b[m"; "\x1b[C" ]);
      ]
  in
  Gen.map (String.concat "") (Gen.list ~size:(Gen.int_range 0 40) fragment)

let digit_string =
  Gen.frequency
    [
      (6, Gen.map string_of_int (Gen.int_range 0 20));
      (2, Gen.map string_of_int (Gen.int_range 0 70000));
      (1, Gen.of_list [ "4294967296"; "99999999999999999999"; "" ]);
    ]

(* A control sequence with any parameter bytes, intermediates and final, in
   the right places or not. *)
let csi =
  let open Gen in
  let* marker = of_list [ ""; ""; ""; "?"; ">"; "<"; "=" ] in
  let* params = list ~size:(int_range 0 6) digit_string in
  let* sep = of_list [ ";"; ";"; ":" ] in
  let* inter = of_list [ ""; ""; ""; " "; "!"; "\""; "$"; "'"; "#"; "%" ] in
  let* final = char_range '\x40' '\x7E' in
  let* noise =
    frequency
      [
        (8, pure "");
        (1, map (String.make 1) (char_range '\x00' '\x1F'));
        (1, of_list [ "\x7f"; "\xc3\xa9"; "\xc2\x9b"; "\x9b"; ":"; "?" ]);
      ]
  in
  let* where = int_range 0 2 in
  let body = String.concat sep params in
  pure
    (match where with
    | 0 -> "\x1b[" ^ noise ^ marker ^ body ^ inter ^ String.make 1 final
    | 1 -> "\x1b[" ^ marker ^ body ^ noise ^ inter ^ String.make 1 final
    | _ -> "\x1b[" ^ marker ^ body ^ inter ^ noise ^ String.make 1 final)

let escape =
  let open Gen in
  let* inter =
    of_list [ ""; ""; "("; ")"; "*"; "+"; "-"; "#"; " "; "%"; "(%" ]
  in
  let+ final = char_range '\x30' '\x7E' in
  "\x1b" ^ inter ^ String.make 1 final

let control_string =
  let open Gen in
  let* opener = of_list [ "\x1b]"; "\x1bP"; "\x1b_"; "\x1b^"; "\x1bX" ] in
  let* head =
    of_list
      [
        "";
        "0;";
        "2;";
        "8;;";
        "8;;http://x";
        "1;2|";
        "q";
        "$q";
        (* The palette: set, ask, reset. *)
        "4;1;?;";
        "4;3;rgb:1/2/3;3;?";
        "10;?;?;?;";
        "11;#123456;";
        "12;rgb:ffff/0/0";
        "104;";
        "110";
      ]
  in
  let* body = utf8_heavy in
  let+ ending =
    of_list
      [ "\x1b\\"; "\x07"; "\x18"; "\x1a"; "\x1b["; ""; "\xc2\x9c"; "\x9c" ]
  in
  opener ^ head ^ body ^ ending

let escape_heavy =
  let fragment =
    Gen.frequency
      [
        (5, csi);
        (3, escape);
        (2, control_string);
        (2, Gen.map (String.make 1) (Gen.char_range '\x00' '\x1F'));
        (1, Gen.of_list [ "\x1b"; "\x1b["; "\x7f"; "\x18"; "\x1a" ]);
        (3, scalar);
        ( 1,
          Gen.of_list
            [
              "\x1b[?1049h";
              "\x1b[?1049l";
              "\x1b[?47h";
              "\x1b[?1047l";
              "\x1b[?7l";
              "\x1b[?7h";
              "\x1b[4h";
              "\x1b[?6h";
              "\x1b[2;4r";
              "\x1b[?45h";
              "\x1b[?42h";
              "\x1b(0";
              "\x1b*0\x1bN";
              "\x1b[3b";
              "\x1b#8";
              "\x1b[!p";
              "\x1bc";
              "\x1b[1\"q";
              "\x1b[?1J";
            ] );
      ]
  in
  Gen.map (String.concat "") (Gen.list ~size:(Gen.int_range 0 30) fragment)

let input = Gen.frequency [ (2, raw); (3, utf8_heavy); (5, escape_heavy) ]
let pp_bytes fmt s = Format.fprintf fmt "%S" s

(* Invariants ------------------------------------------------------------- *)

let is_control cp = cp < 0x20 || (cp >= 0x7F && cp <= 0x9F)

let scalars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then
        failf "cell text %S is not UTF-8" s;
      go
        (i + Uchar.utf_decode_length d)
        (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
  in
  go 0 []

(* Cells hold no control character, and graphemes span whole cells within
   their row. *)
let check_grid ~what g =
  let width = Grid.width g and height = Grid.height g in
  for r = 0 to height - 1 do
    let expected = ref 0 in
    for c = 0 to width - 1 do
      let idx = Grid.idx g ~x:c ~y:r in
      List.iter
        (fun cp ->
          if is_control cp then
            failf "%s: row %d col %d holds the control U+%04X" what r c cp)
        (scalars (Grid.get_text g idx));
      let continuation = Grid.is_continuation g idx in
      if !expected > 0 then (
        if not continuation then
          failf "%s: row %d col %d: a wide grapheme lacks its continuation" what
            r c;
        decr expected)
      else (
        if continuation then
          failf "%s: row %d col %d: orphan continuation" what r c;
        let w = Grid.cell_width g idx in
        if w > 1 then expected := w - 1)
    done;
    if !expected > 0 then
      failf "%s: row %d: a grapheme crosses the row's end" what r
  done

let check vte =
  let rows = Vte.rows vte and cols = Vte.cols vte in
  if rows < 1 || cols < 1 then failf "size %dx%d" rows cols;
  let g = Vte.grid vte in
  if Grid.width g <> cols || Grid.height g <> rows then
    failf "grid %dx%d for a %dx%d terminal" (Grid.width g) (Grid.height g) rows
      cols;
  let row, col = Vte.cursor_pos vte in
  if row < 0 || row >= rows || col < 0 || col >= cols then
    failf "cursor (%d, %d) outside %dx%d" row col rows cols;
  if Vte.last_column vte && not (col = cols - 1 && (Vte.modes vte).auto_wrap)
  then
    failf "Last Column Flag at col %d of %d, autowrap %b" col cols
      (Vte.modes vte).auto_wrap;
  check_grid ~what:"screen" g;
  let size = Vte.scrollback_size vte in
  if size < 0 || size > Vte.scrollback_capacity vte then
    failf "scrollback size %d, capacity %d" size (Vte.scrollback_capacity vte);
  (* Rows that do not exist are blank, with no wrap mark. *)
  List.iter
    (fun y ->
      if Vte.wrapped vte y then
        failf "row %d, which does not exist, is wrapped" y;
      if Vte.text vte y <> "" then
        failf "row %d, which does not exist, has text" y)
    [ rows; rows + 1; -size - 1; -size - 2; max_int; min_int + 1; min_int ];
  List.iter
    (fun r -> if r < 0 || r >= rows then failf "dirty row %d of %d" r rows)
    (Vte.dirty_rows vte)

(* Every accessor, which must not raise. *)
let read_all vte =
  ignore (Vte.to_string vte : string);
  ignore (Vte.title vte : string);
  ignore (Vte.modes vte : Vte.modes);
  ignore (Vte.is_dirty vte : bool);
  ignore (Vte.is_cursor_dirty vte : bool);
  ignore (Vte.cursor_style vte : Ansi.cursor_style * bool);
  for y = -Vte.scrollback_size vte - 1 to Vte.rows vte do
    ignore (Vte.wrapped vte y : bool);
    ignore (Vte.text vte y : string)
  done

let render vte ~offset ~height =
  let dst =
    Grid.create ~width:(Vte.cols vte) ~height
      ~width_method:(Grid.width_method (Vte.grid vte))
      ()
  in
  for y = 0 to height - 1 do
    Vte.blit_row vte (y - offset) dst ~y
  done;
  check_grid ~what:"scrollback view" dst

(* Operations --------------------------------------------------------------- *)

type op =
  | Feed of string
  | Feed_sub of string * int * int  (** A sub-range of the bytes. *)
  | Resize of int * int
  | Reset
  | Render of int * int  (** Offset and height. *)
  | Clear_dirty

let pp_op fmt = function
  | Feed s -> Format.fprintf fmt "feed %S" s
  | Feed_sub (s, off, len) -> Format.fprintf fmt "feed %S %d %d" s off len
  | Resize (r, c) -> Format.fprintf fmt "resize %dx%d" r c
  | Reset -> Format.fprintf fmt "reset"
  | Render (o, h) -> Format.fprintf fmt "render offset %d height %d" o h
  | Clear_dirty -> Format.fprintf fmt "clear_dirty"

let op =
  let open Gen in
  frequency
    [
      (10, map (fun s -> Feed s) input);
      ( 1,
        let* s = input in
        let n = String.length s in
        let* off = int_range 0 n in
        let+ len = int_range 0 (n - off) in
        Feed_sub (s, off, len) );
      ( 3,
        let+ r = frequency [ (3, int_range 1 3); (2, int_range 1 30) ]
        and+ c = frequency [ (3, int_range 1 3); (2, int_range 1 100) ] in
        Resize (r, c) );
      (1, pure Reset);
      ( 2,
        let+ o = int_range (-3) 20 and+ h = int_range 1 12 in
        Render (o, h) );
      (1, pure Clear_dirty);
    ]

type program = {
  rows : int;
  cols : int;
  scrollback : int;
  width_method : Matrix_text.width_method;
  ops : op list;
}

let pp_program fmt p =
  Format.fprintf fmt "%dx%d scrollback %d %s@\n%a" p.rows p.cols p.scrollback
    (match p.width_method with
    | `Unicode -> "unicode"
    | `Unicode_wide -> "unicode_wide"
    | `Wcwidth -> "wcwidth")
    (Format.pp_print_list pp_op)
    p.ops

let program =
  let open Gen in
  let* rows = frequency [ (3, int_range 1 3); (2, int_range 1 12) ] in
  let* cols = frequency [ (3, int_range 1 3); (2, int_range 1 40) ] in
  let* scrollback = int_range 0 8 in
  let* width_method = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  let+ ops = list ~size:(int_range 1 12) op in
  { rows; cols; scrollback; width_method; ops }

(* Replies are the answers this terminal gives, each written whole: their
   bytes read back, through a program's input decoder, as the same replies. *)
let check_replies replies =
  let module I = Matrix_input in
  let count f = List.length (List.filter f replies) in
  let d = I.Decoder.create () in
  I.Decoder.expect d
    {
      cursor_reports = count (function I.Reply.Cursor _ -> true | _ -> false);
      string_replies = count (function I.Reply.Color _ -> true | _ -> false);
    };
  let got = ref [] in
  let emit e = got := e :: !got in
  let bytes = I.Reply.encode replies in
  I.Decoder.feed d (Bytes.of_string bytes) ~off:0 ~len:(String.length bytes)
    emit;
  I.Decoder.finish d emit;
  if List.rev !got <> List.map (fun r -> I.Decoder.Reply r) replies then
    failf "replies %S read back as others" bytes

let run_op vte = function
  | Feed s ->
      (* Short inputs are fed a byte at a time, checked after each. *)
      if String.length s <= 24 then
        String.iter
          (fun c ->
            check_replies (Vte.feed_string vte (String.make 1 c));
            check vte)
          s
      else check_replies (Vte.feed_string vte s)
  | Feed_sub (s, off, len) ->
      check_replies (Vte.feed vte (Bytes.of_string s) off len)
  | Resize (rows, cols) -> check_replies (Vte.resize vte ~rows ~cols)
  | Reset -> Vte.reset vte
  | Render (offset, height) -> render vte ~offset ~height
  | Clear_dirty -> Vte.clear_dirty vte

let prop_operations p =
  let vte =
    Vte.create ~scrollback:p.scrollback ~width_method:p.width_method
      ~rows:p.rows ~cols:p.cols ()
  in
  check vte;
  List.iter
    (fun op ->
      run_op vte op;
      check vte;
      read_all vte)
    p.ops

(* Split invariance --------------------------------------------------------- *)

type split = {
  srows : int;
  scols : int;
  method_ : Matrix_text.width_method;
  bytes : string;
  cuts : int list;
}

let pp_split fmt s =
  Format.fprintf fmt "%dx%d %S cut at [%s]" s.srows s.scols s.bytes
    (String.concat ";" (List.map string_of_int s.cuts))

let split =
  let open Gen in
  let* srows = int_range 1 5 in
  let* scols = int_range 1 9 in
  let* method_ = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  let* bytes = input in
  let+ cuts = list ~size:(int_range 1 8) (int_range 0 (String.length bytes)) in
  { srows; scols; method_; bytes; cuts }

let chunks_of s cuts =
  let len = String.length s in
  let cuts =
    List.sort_uniq compare (0 :: len :: List.filter (fun c -> c <= len) cuts)
  in
  let rec slices = function
    | a :: (b :: _ as rest) -> String.sub s a (b - a) :: slices rest
    | _ -> []
  in
  slices cuts

(* What the bytes leave: every cell with its rendition and protection, the
   cursor with its flag and style, the modes, the title, the wrap marks, the scrollback,
   and the replies. *)
let state vte replies =
  let g = Vte.grid vte in
  let cells =
    List.init
      (Grid.width g * Grid.height g)
      (fun i ->
        ( Grid.get_text g i,
          Grid.get_style g i,
          Grid.is_continuation g i,
          Grid.cell_width g i,
          Grid.is_protected g i ))
  in
  let marks =
    List.init
      (Vte.rows vte + Vte.scrollback_size vte)
      (fun i -> Vte.wrapped vte (i - Vte.scrollback_size vte))
  in
  ( cells,
    (Vte.cursor_pos vte, Vte.last_column vte, Vte.cursor_style vte),
    Vte.modes vte,
    Vte.title vte,
    marks,
    List.init
      (Vte.rows vte + Vte.scrollback_size vte)
      (fun i -> Vte.text vte (i - Vte.scrollback_size vte)),
    replies )

let fed s chunks =
  let vte =
    Vte.create ~scrollback:6 ~width_method:s.method_ ~rows:s.srows ~cols:s.scols
      ()
  in
  let replies = List.concat_map (Vte.feed_string vte) chunks in
  state vte replies

let prop_split s =
  let whole = fed s [ s.bytes ] in
  let same msg other =
    if whole <> other then failf "%s differs from the whole" msg
  in
  same "random chunks" (fed s (chunks_of s.bytes s.cuts));
  let n = String.length s.bytes in
  if n <= 80 then
    for i = 1 to n - 1 do
      same
        (Printf.sprintf "split at %d" i)
        (fed s [ String.sub s.bytes 0 i; String.sub s.bytes i (n - i) ])
    done;
  same "byte by byte" (fed s (List.init n (fun i -> String.make 1 s.bytes.[i])))

(* Parser ------------------------------------------------------------------ *)

let tokens chunks =
  let p = Parser.create () in
  let out = ref [] in
  List.iter
    (fun c ->
      ignore
        (Parser.feed p (Bytes.of_string c) ~off:0 ~len:(String.length c)
           (fun t -> out := t :: !out)
          : int))
    chunks;
  List.rev !out

(* Adjacent [Text] tokens, and adjacent [Data] tokens, joined. *)
let rec joined = function
  | Parser.Text a :: Text b :: rest -> joined (Text (a ^ b) :: rest)
  | Data a :: Data b :: rest -> joined (Data (a ^ b) :: rest)
  | t :: rest -> t :: joined rest
  | [] -> []

let show tokens =
  String.concat " " (List.map (Format.asprintf "%a" Parser.pp_token) tokens)

let check_text ~what s ~allowed =
  if s = "" then failf "an empty %s token" what;
  List.iter
    (fun cp -> if not (allowed cp) then failf "%s %S holds U+%04X" what s cp)
    (scalars s)

let check_tokens toks =
  let in_string = ref false in
  List.iter
    (fun t ->
      match t with
      | Parser.Text s ->
          if !in_string then failf "Text inside a string";
          check_text ~what:"Text" s ~allowed:(fun cp -> not (is_control cp))
      | Data s ->
          if not !in_string then failf "Data outside a string";
          check_text ~what:"Data" s ~allowed:(fun cp ->
              (cp >= 0x08 && cp <= 0x0D)
              || (cp >= 0x20 && cp <= 0x7E)
              || cp > 0x9F)
      | Open _ ->
          if !in_string then failf "Open inside a string";
          in_string := true
      | Close _ ->
          if not !in_string then failf "Close outside a string";
          in_string := false
      | Control c ->
          if !in_string then failf "Control inside a string";
          if c = '\x1b' then failf "ESC reported as a control"
      | Esc { intermediates; final } ->
          if !in_string then failf "Esc inside a string";
          if String.length intermediates > 4 then
            failf "Esc with %S" intermediates;
          if final < '\x30' || final > '\x7e' then failf "Esc final %C" final
      | Csi { intermediates; final; params; _ } ->
          if !in_string then failf "Csi inside a string";
          if String.length intermediates > 4 then
            failf "Csi with %S" intermediates;
          if final < '\x40' || final > '\x7e' then failf "Csi final %C" final;
          let fields = ref 0 in
          for i = 0 to Parser.Params.count params - 1 do
            fields := !fields + max 1 (Parser.Params.fields params i)
          done;
          if !fields > 256 then failf "Csi with %d fields" !fields)
    toks

let prop_parser s =
  let whole = tokens [ s.bytes ] in
  check_tokens whole;
  let whole = joined whole in
  let same msg chunks =
    let other = tokens chunks in
    check_tokens other;
    let other = joined other in
    if whole <> other then
      failf "%s: %s@\nwhole: %s" msg (show other) (show whole)
  in
  same "random chunks" (chunks_of s.bytes s.cuts);
  let n = String.length s.bytes in
  same "byte by byte" (List.init n (fun i -> String.make 1 s.bytes.[i]));
  if n <= 80 then
    for i = 1 to n - 1 do
      same
        (Printf.sprintf "split at %d" i)
        [ String.sub s.bytes 0 i; String.sub s.bytes i (n - i) ]
    done;
  ignore (Parser.parse s.bytes : Parser.token list);
  ignore (Ansi.strip s.bytes : string)

let () =
  ignore pp_bytes;
  run "matrix.vte.fuzz"
    [
      group "parser"
        [
          prop ~count:(count 1000) "tokens do not depend on chunking"
            (Gen.with_pp pp_split split)
            prop_parser;
        ];
      group "vte"
        [
          prop ~count:(count 800) "operations keep the invariants"
            (Gen.with_pp pp_program program)
            prop_operations;
          prop ~count:(count 300) "split invariance on arbitrary bytes"
            (Gen.with_pp pp_split split)
            prop_split;
        ];
    ]
