(** Property tests for the virtual terminal emulator over untrusted bytes.

    A VTE consumes whatever the child process writes, so every byte string is a
    valid input. Three laws are checked over inputs biased toward ANSI
    escape-sequence shapes:

    - {b Total safety}: [Vte.feed] never raises.
    - {b Grid invariants}: after any input, the cursor stays in bounds, every
      wide-character start is paired with its continuation cells within the row,
      and scrollback never exceeds its capacity.
    - {b Split invariance}: feeding a byte string whole or in arbitrary chunks
      produces the same terminal state. *)

module Grid = Matrix_grid
open Windtrap

(* The scrollback's lines, oldest first. *)
let scrollback_lines vte =
  let n = Vte.scrollback_size vte in
  List.init n (fun i -> Vte.text vte (i - n))

(* Generators ------------------------------------------------------------- *)

let csi_gen =
  let open Gen in
  let* private_marker = bool in
  let* params = list ~size:(int_range 0 3) (int_range 0 200) in
  let+ final =
    of_list
      [
        'A';
        'B';
        'C';
        'D';
        'E';
        'F';
        'G';
        'H';
        'J';
        'K';
        'L';
        'M';
        'P';
        'S';
        'T';
        'X';
        '@';
        'd';
        'f';
        'h';
        'l';
        'm';
        'n';
        'r';
        's';
        'u';
      ]
  in
  Printf.sprintf "\x1b[%s%s%c"
    (if private_marker then "?" else "")
    (String.concat ";" (List.map string_of_int params))
    final

let mode_gen =
  let open Gen in
  let* set = bool in
  let* private_mode = bool in
  let+ mode = of_list [ 1; 4; 6; 7; 12; 25; 47; 1047; 1048; 1049; 2004 ] in
  Printf.sprintf "\x1b[%s%d%c"
    (if private_mode then "?" else "")
    mode
    (if set then 'h' else 'l')

let structural_fragments =
  [
    "\r\n";
    "\n";
    "\r";
    "\t";
    "\b";
    "\x07";
    "\x1b7";
    "\x1b8";
    "\x1bM";
    "\x1bc";
    "\x1b[2J";
    "\x1b[0m";
    "\x1b[1;31;42m";
    "\x1b[38;2;1;2;3m";
    "\x1b[38;5;100m";
    "\x1b]0;title\x07";
    "\x1b]2;t\x1b\\";
    "\x1b]8;;https://x\x1b\\";
  ]

let valid_utf8_fragments =
  [
    "\xE4\xBD\xA0"; "\xE5\xA5\xBD"; "\xF0\x9F\x98\x80"; "\xC3\xA9"; "e\xCC\x81";
  ]

let invalid_utf8_fragments =
  [ "\xE4\xBD"; "\xC3"; "\xF0\x9F"; "\x80"; "\xFF"; "\xC0"; "\xE0\x80\x80" ]

let partial_escape_fragments =
  [ "\x1b"; "\x1b["; "\x1b[3"; "\x1b]0;x"; "\x1bP1$r" ]

(* One generator for every property: Ansi.Parser's UTF-8 recovery and
   charset-designation handling are split-invariant, so invalid UTF-8 and
   pure random bytes run through split invariance as well as safety. *)
let fragment_gen =
  Gen.frequency
    [
      (4, Gen.string_of ~size:(Gen.int_range 0 8) (Gen.char_range ' ' '~'));
      (3, csi_gen);
      (2, mode_gen);
      (3, Gen.of_list structural_fragments);
      (2, Gen.of_list (valid_utf8_fragments @ invalid_utf8_fragments));
      (1, Gen.of_list partial_escape_fragments);
      (1, Gen.string_of ~size:(Gen.int_range 0 6) Gen.char);
    ]

let case_gen =
  let open Gen in
  let* rows = int_range 1 6 in
  let* cols = int_range 1 8 in
  let* frags = list ~size:(int_range 0 12) fragment_gen in
  let s = String.concat "" frags in
  let+ cuts = list ~size:(int_range 0 6) (int_range 0 (String.length s)) in
  (rows, cols, s, cuts)

let chunks_of s cuts =
  let len = String.length s in
  let cuts = List.sort_uniq compare (0 :: len :: cuts) in
  let rec slices = function
    | a :: (b :: _ as rest) -> String.sub s a (b - a) :: slices rest
    | _ -> []
  in
  slices cuts

let pp_case fmt (rows, cols, s, cuts) =
  Format.fprintf fmt "%dx%d %S cut at [%s]" rows cols s
    (String.concat ";" (List.map string_of_int cuts))

let vte_case = Gen.with_pp pp_case case_gen

(* Invariants ------------------------------------------------------------- *)

let check_invariants vte =
  let rows = Vte.rows vte and cols = Vte.cols vte in
  let row, col = Vte.cursor_pos vte in
  if not (row >= 0 && row < rows) then
    failf "cursor row %d out of bounds (rows=%d)" row rows;
  if not (col >= 0 && col < cols) then
    failf "cursor col %d out of bounds (cols=%d)" col cols;
  if Vte.scrollback_size vte > Vte.scrollback_capacity vte then
    failf "scrollback size %d exceeds capacity %d" (Vte.scrollback_size vte)
      (Vte.scrollback_capacity vte);
  (* Wide-character span integrity: every start cell of width w is followed
     by exactly w - 1 continuation cells, continuations never appear without
     a start, and spans never cross the row edge. *)
  let grid = Vte.grid vte in
  let width = Grid.width grid and height = Grid.height grid in
  for r = 0 to height - 1 do
    let expected = ref 0 in
    for c = 0 to width - 1 do
      let idx = (r * width) + c in
      let continuation = Grid.is_continuation grid idx in
      if !expected > 0 then begin
        if not continuation then
          failf "row %d col %d: wide start is missing its continuation" r c;
        decr expected
      end
      else begin
        if continuation then failf "row %d col %d: orphan continuation" r c;
        let w = Grid.cell_width grid idx in
        if w > 1 then expected := w - 1
      end
    done;
    if !expected > 0 then failf "row %d: wide span crosses the row edge" r
  done

let feed vte s = ignore (Vte.feed_string vte s : _ list)
let feed_chunks vte chunks = List.iter (feed vte) chunks

(* Properties ------------------------------------------------------------- *)

let prop_safety_and_invariants (rows, cols, s, cuts) =
  let vte = Vte.create ~scrollback:16 ~rows ~cols () in
  List.iter
    (fun chunk ->
      feed vte chunk;
      check_invariants vte)
    (chunks_of s cuts)

let prop_split_invariance (rows, cols, s, cuts) =
  let whole = Vte.create ~scrollback:16 ~rows ~cols () in
  feed whole s;
  let split = Vte.create ~scrollback:16 ~rows ~cols () in
  feed_chunks split (chunks_of s cuts);
  equal ~msg:"screen content" string (Vte.to_string whole) (Vte.to_string split);
  for idx = 0 to (rows * cols) - 1 do
    is_true ~msg:"grid styles and spans"
      (Grid.cells_equal (Vte.grid whole) idx (Vte.grid split) idx)
  done;
  equal ~msg:"cursor position" (pair int int) (Vte.cursor_pos whole)
    (Vte.cursor_pos split);
  is_true ~msg:"modes" (Vte.modes whole = Vte.modes split);
  equal ~msg:"title" string (Vte.title whole) (Vte.title split);
  equal ~msg:"scrollback" string
    (String.concat "\n" (scrollback_lines whole))
    (String.concat "\n" (scrollback_lines split))

(* Split invariance over the text that makes units grow: every generator
   family a terminal meets, interleaved with control functions, margins,
   autowrap on and off, and insert mode, under each width method. *)

let unit_fragments =
  [
    (* CJK *)
    "\xe4\xb8\xad";
    "\xe5\xa5\xbd";
    (* combining marks *)
    "e\xcc\x81";
    "\xcc\x81";
    "\xcc\x82";
    (* ZWJ emoji *)
    "\u{1F468}";
    "\u{200D}";
    "\u{1F4BB}";
    (* VS15, VS16 *)
    "\u{2764}";
    "\u{FE0F}";
    "\u{FE0E}";
    "\u{231A}";
    (* keycaps *)
    "1\u{FE0F}";
    "\u{20E3}";
    (* regional indicators *)
    "\u{1F1EB}";
    "\u{1F1F7}";
    (* Prepend *)
    "\u{0D4E}";
    "\u{111C2}";
    "\u{0600}";
    (* Hangul jamo *)
    "\u{1100}";
    "\u{1161}";
    "\u{11A8}";
    (* format characters *)
    "\u{200B}";
    "\u{2060}";
    (* a conjunct *)
    "\u{0915}\u{094D}";
  ]

let function_fragments =
  [
    "\r";
    "\n";
    "\b";
    "\t";
    "\x1b[m";
    "\x1b[1;41m";
    "\x1b[C";
    "\x1b[D";
    "\x1b[3b";
    "\x1b[@";
    "\x1b[P";
    "\x1b[K";
    "\x1b[X";
    "\x1b7";
    "\x1b8";
    "\x1b[1\"q";
    "\x1b[0\"q";
    "\x1b[?2K";
    "\x1b[S";
    "\x1bM";
  ]

let setting_fragments =
  [
    "\x1b[?7l";
    "\x1b[?7h";
    "\x1b[4h";
    "\x1b[4l";
    "\x1b[2;3r";
    "\x1b[r";
    "\x1b[?45h";
    "\x1b[?6h";
    "\x1b(0";
    "\x1b(B";
    "\x1b*0\x1bN";
  ]

let unit_case_gen =
  let open Gen in
  let fragment =
    frequency
      [
        (4, string_of ~size:(int_range 1 6) (char_range 'a' 'z'));
        (8, of_list unit_fragments);
        (3, of_list function_fragments);
        (2, of_list setting_fragments);
      ]
  in
  let* rows = int_range 1 4 in
  let* cols = int_range 1 7 in
  let* width_method = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  let* frags = list ~size:(int_range 1 14) fragment in
  let s = String.concat "" frags in
  let+ cuts = list ~size:(int_range 1 8) (int_range 0 (String.length s)) in
  (rows, cols, width_method, s, cuts)

let pp_unit_case fmt (rows, cols, width_method, s, cuts) =
  Format.fprintf fmt "%dx%d %s %S cut at [%s]" rows cols
    (match width_method with
    | `Unicode -> "unicode"
    | `Unicode_wide -> "unicode_wide"
    | `Wcwidth -> "wcwidth")
    s
    (String.concat ";" (List.map string_of_int cuts))

(* What a renderer and a reader of the terminal see: the cells with their
   rendition and protection, the cursor, the modes, the wrap marks and the
   scrollback. *)
let check_same ~msg whole split =
  let g = Vte.grid whole and g' = Vte.grid split in
  for idx = 0 to (Grid.width g * Grid.height g) - 1 do
    if
      not
        (Grid.cells_equal g idx g' idx
        && Grid.is_protected g idx = Grid.is_protected g' idx)
    then
      failf "%s: cell %d: %S, %S" msg idx (Grid.get_text g idx)
        (Grid.get_text g' idx)
  done;
  equal ~msg:(msg ^ ": cursor") (pair int int) (Vte.cursor_pos whole)
    (Vte.cursor_pos split);
  is_true ~msg:(msg ^ ": modes") (Vte.modes whole = Vte.modes split);
  equal ~msg:(msg ^ ": scrollback") (list string) (scrollback_lines whole)
    (scrollback_lines split);
  let marks vte =
    List.init
      (Vte.rows vte + Vte.scrollback_size vte)
      (fun i -> Vte.wrapped vte (i - Vte.scrollback_size vte))
  in
  equal ~msg:(msg ^ ": wrap marks") (list bool) (marks whole) (marks split)

let fed ~rows ~cols ~width_method chunks =
  let vte = Vte.create ~scrollback:8 ~width_method ~rows ~cols () in
  feed_chunks vte chunks;
  vte

let prop_unit_split_invariance (rows, cols, width_method, s, cuts) =
  let whole = fed ~rows ~cols ~width_method [ s ] in
  check_same ~msg:"random chunks" whole
    (fed ~rows ~cols ~width_method (chunks_of s cuts));
  for i = 1 to String.length s - 1 do
    check_same
      ~msg:(Printf.sprintf "split at %d" i)
      whole
      (fed ~rows ~cols ~width_method
         [ String.sub s 0 i; String.sub s i (String.length s - i) ])
  done

let props =
  [
    prop ~count:1000 "feed is total and preserves grid invariants" vte_case
      prop_safety_and_invariants;
    prop ~count:1000 "split invariance" vte_case prop_split_invariance;
    prop ~count:1000 "split invariance of units"
      (Gen.with_pp pp_unit_case unit_case_gen)
      prop_unit_split_invariance;
  ]

let () = exit (run "matrix.vte.props" [ group "vte" props ])
