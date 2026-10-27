(** Properties of the input decoder over untrusted bytes and silences.

    The decoder reads whatever a terminal, a multiplexer or a stray [cat]
    writes, cut into reads anywhere, with silences anywhere. These properties
    check:

    - {b Split invariance}: with no silence, the events do not depend on how the
      bytes are cut into reads, once adjacent [Paste_data] are joined.
    - {b Silence}: the events of bytes with silences at given positions equal
      those of a reference written from the rule in [decoder.mli]: a silence
      turns held ESCs into keys, cuts a sequence or character in progress, and
      leaves strings and pastes alone.
    - {b Never typed}: every reply and report cut at every position by a silence
      gives no key from its bytes once the bytes have committed it.
    - {b Totality}: no input or call order raises, and the decoder holds bounded
      memory outside a string reply.
    - {b Encoding}: a reply, keystroke or mouse report encoded reads back as
      itself, less what its encoding cannot carry; what the decoder reads of a
      keystroke's bytes encodes to bytes it reads the same.

    The counts run in a few seconds. [MATRIX_FUZZ_SCALE=N] multiplies them, for
    long sessions. *)

module I = Matrix_input
module D = I.Decoder
module K = I.Keystroke
open Windtrap

let scale =
  match Sys.getenv_opt "MATRIX_FUZZ_SCALE" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 1)
  | None -> 1

let count n = n * scale
let show e = Format.asprintf "%a" D.pp_event e

let owed ~cursor ~strings =
  { D.cursor_reports = cursor; string_replies = strings }

(* Corpus ------------------------------------------------------------------ *)

(* Control sequences: keys in every encoding, reports and replies. *)
let sequences =
  [
    "\x1b[A";
    "\x1b[1;5A";
    "\x1b[3~";
    "\x1b[1;2P";
    "\x1b[15;5~";
    "\x1b[27;5;13~";
    "\x1b[97:65;6u";
    "\x1b[97;1:3u";
    "\x1b[0;;229u";
    "\x1bOP";
    "\x1bO1;2P";
    "\x1bOj";
    "\x1b[2$";
    "\x1b[2^";
    "\x1b[[A";
    "\x1b[<0;10;20M";
    "\x1b[<35;5;5m";
    "\x1b[32;5;5M";
    "\x1b[M !!";
    "\x1b[I";
    "\x1b[200~";
    "\x1b[?64;1;2c";
    "\x1b[>41;398;0c";
    "\x1b[?2026;2$y";
    "\x1b[12;40R";
    "\x1b[1;2R";
    "\x1b[4;480;800t";
    "\x1b[48;24;80;480;800t";
    "\x1b[?1u";
    "\x1b[?997;1n";
  ]

(* Control strings: replies, which begin only while one is owed. *)
let strings =
  [
    "\x1bP>|xterm(398)\x1b\\";
    "\x1bP1+r524742=31\x1b\\";
    "\x1bP!|00000000\x1b\\";
    "\x1b]11;rgb:ffff/8080/0000\x07";
    "\x1b]52;c;aGVsbG8=\x1b\\";
    "\x1b_Gi=31337;OK\x1b\\";
  ]

(* Byte strings biased toward what decoding must get right: the corpus,
   truncated prefixes, paste markers, ESC before everything, UTF-8 and
   ill-formed bytes, long numbers, plain text and random bytes. *)
let fragment =
  Gen.frequency
    [
      (4, Gen.of_list sequences);
      (2, Gen.of_list strings);
      ( 3,
        Gen.of_list
          [
            "\x1b";
            "\x1b\x1b";
            "\x1b[";
            "\x1bO";
            "\x1b]";
            "\x1bP";
            "\x1b_";
            "\x1b[1;";
            "\x1b[<35;2";
            "\x1b[M";
            "\x1b[M!";
            "\x1b[[";
            "\x1b[2";
            "\x1b]11;rgb:1";
            "\x1bP>|x";
            "\x1b[200~";
            "\x1b[201~";
            "\x1b[20";
            "\x1b[201";
            "\x1b\\";
            "\x07";
            "\x18";
            "\x1a";
          ] );
      ( 3,
        Gen.of_list
          [
            "\xc3\xa9";
            "\xe6\x97\xa5";
            "\xf0\x9f\x98\x80";
            "\xc3";
            "\xe2\x82";
            "\xf0\x9f\x98";
            "\x80";
            "\xc0";
            "\xe0\x80";
            "\xed\xa0\x80";
            "\xf4\x90\x80\x80";
            "\xff";
            "\x9b";
            "\xc2\x9b";
          ] );
      ( 1,
        Gen.of_list
          [
            "\x1b[99999999999999999999;99999999999999999999A";
            "\x1b[27;5;55296~";
            "\x1b[1114112u";
            "\x1b[<0;0;0M";
            "\x1b[97;257u";
            "\x1b[1;2;3;4;5;6;7;8;9A";
          ] );
      (4, Gen.string_of ~size:(Gen.int_range 0 5) (Gen.char_range 'a' 'z'));
      (2, Gen.string_of ~size:(Gen.int_range 0 6) Gen.char);
      ( 1,
        Gen.of_list
          [ "\x00"; "\x01"; "\x08"; "\x09"; "\x0a"; "\x0d"; "\x7f"; " " ] );
    ]

let bytes_gen =
  let open Gen in
  let+ frags = list ~size:(int_range 0 12) fragment in
  String.concat "" frags

type split = { bytes : string; cuts : int list; cursor : int; strings : int }

let split_gen =
  let open Gen in
  let* bytes = bytes_gen in
  let+ cuts = list ~size:(int_range 0 8) (int_range 0 (String.length bytes))
  and+ cursor = int_range 0 1
  and+ strings = int_range 0 1 in
  { bytes; cuts; cursor; strings }

let chunks s cuts =
  let n = String.length s in
  let cuts = List.sort_uniq compare (0 :: n :: cuts) in
  let rec go = function
    | a :: (b :: _ as rest) -> String.sub s a (b - a) :: go rest
    | _ -> []
  in
  go cuts

let pp_split ppf s =
  Format.fprintf ppf "%S cut at [%s], %d cursor and %d string replies owed"
    s.bytes
    (String.concat ";" (List.map string_of_int s.cuts))
    s.cursor s.strings

(* Harness ------------------------------------------------------------------- *)

type op = Feed of string | Idle | Finish

(* The events of [ops] from a decoder owed [o], printed, adjacent
   [Paste_data] joined. *)
let decode o ops =
  let d = D.create () in
  D.expect d o;
  let out = ref [] in
  let emit = function
    | D.Paste_data s -> (
        match !out with
        | D.Paste_data t :: rest -> out := D.Paste_data (t ^ s) :: rest
        | _ -> out := D.Paste_data s :: !out)
    | e -> out := e :: !out
  in
  List.iter
    (function
      | Feed s ->
          D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) emit
      | Idle -> D.idle d emit
      | Finish -> D.finish d emit)
    ops;
  List.rev_map show !out

let feeds l = List.map (fun s -> Feed s) l

let same msg expected got =
  if expected <> got then
    failf "%s:@\n  expected %s@\n  got      %s" msg
      (String.concat " " expected)
      (String.concat " " got)

(* Split invariance --------------------------------------------------------- *)

let prop_split s =
  let o = owed ~cursor:s.cursor ~strings:s.strings in
  let whole = decode o [ Feed s.bytes; Finish ] in
  same "random reads" whole
    (decode o (feeds (chunks s.bytes s.cuts) @ [ Finish ]));
  let n = String.length s.bytes in
  same "byte by byte" whole
    (decode o
       (List.init n (fun i -> Feed (String.make 1 s.bytes.[i])) @ [ Finish ]))

(* Silence: a reference ------------------------------------------------------ *)

(* A stream is a list of units, each of known meaning when whole. A silence
   may fall inside a unit, between units, or after a lone ESC, which always
   has one: without it the next unit would continue the ESC. *)
type unit_ =
  | Plain of char  (** a byte that is a key alone *)
  | Utf_8 of string  (** a character of 2 to 4 bytes *)
  | Alt of char  (** ESC and a byte that is not an introducer *)
  | Lone_esc
  | Seq of string  (** a control sequence of [sequences] *)
  | Str of string  (** a control string of [strings], owed *)
  | Paste of string  (** a bracketed paste of letters and digits *)

let unit_bytes = function
  | Plain c -> String.make 1 c
  | Utf_8 s | Seq s | Str s -> s
  | Alt c -> "\x1b" ^ String.make 1 c
  | Lone_esc -> "\x1b"
  | Paste p -> "\x1b[200~" ^ p ^ "\x1b[201~"

let unit_gen =
  let open Gen in
  frequency
    [
      ( 4,
        map
          (fun c -> Plain c)
          (of_list
             (String.to_seq "abcXYZ019 ;:[]O~\x01\x0d\x09\x7f\x07"
             |> List.of_seq)) );
      ( 2,
        map
          (fun s -> Utf_8 s)
          (of_list
             [ "\xc3\xa9"; "\xe6\x97\xa5"; "\xf0\x9f\x98\x80"; "\xed\x9f\xbf" ])
      );
      ( 2,
        map
          (fun c -> Alt c)
          (of_list (String.to_seq "abcXYZ019 ;:~\x01\x0d\x7f" |> List.of_seq))
      );
      (1, pure Lone_esc);
      (* A paste's start marker alone would make the units after it
         content. *)
      ( 4,
        map
          (fun s -> Seq s)
          (of_list (List.filter (fun s -> s <> "\x1b[200~") sequences)) );
      (2, map (fun s -> Str s) (of_list strings));
      ( 1,
        map
          (fun p -> Paste p)
          (string_of ~size:(int_range 0 6) (char_range 'a' 'z')) );
    ]

(* A unit with the offsets inside it where a silence falls, and whether one
   follows it. A paste's start marker is never cut: cut, it is a sequence
   whose tail is typed, and that tail holds the end marker, a sequence too. *)
type placed = { u : unit_; inside : int list; after : bool }

let placed_gen =
  let open Gen in
  let* u = unit_gen in
  let n = String.length (unit_bytes u) in
  let lo = match u with Paste _ -> 6 | _ -> 1 in
  let+ inside =
    if n - 1 < lo then pure []
    else list ~size:(int_range 0 2) (int_range lo (n - 1))
  and+ after = bool in
  { u; inside = List.sort_uniq compare inside; after = after || u = Lone_esc }

let o_ref = owed ~cursor:0 ~strings:1

(* The events of a byte alone, or of bytes whole, from a fresh decoder. *)
let alone s = decode o_ref [ Feed s; Idle ]
let key k mods = show (D.Key (K.make ~mods k))
let escape = key K.Escape K.no_mods
let alt c = key (K.Char (Uchar.of_char c)) { K.no_mods with alt = true }

(* The tail of a cut unit, from ground: each byte a key, except an ESC with a
   byte after it and no silence between, which is Alt and that byte. *)
let rec tail s i cuts =
  let n = String.length s in
  if i >= n then []
  else if s.[i] = '\x1b' && i + 1 < n && not (List.mem (i + 1) cuts) then
    alt s.[i + 1] :: tail s (i + 2) cuts
  else if s.[i] = '\x1b' then escape :: tail s (i + 1) cuts
  else alone (String.make 1 s.[i]) @ tail s (i + 1) cuts

(* The rule of [decoder.mli], unit by unit. *)
let reference units =
  List.concat_map
    (fun { u; inside; _ } ->
      let s = unit_bytes u in
      match (u, inside) with
      | _, [] -> alone s
      | (Plain _ | Lone_esc), _ -> alone s
      | Utf_8 _, k :: _ ->
          (* The character in progress is ill formed; each byte after it is
             a continuation byte alone, ill formed too. *)
          show (D.Invalid (String.sub s 0 k))
          :: List.init
               (String.length s - k)
               (fun i -> show (D.Invalid (String.make 1 s.[k + i])))
      | Alt c, _ -> escape :: alone (String.make 1 c)
      | (Seq _ | Str _), k :: _ when k <= 2 ->
          (* ESC alone, or ESC and its introducer: keys. *)
          let held = if k = 1 then escape else alt s.[1] in
          let cuts = List.map (fun c -> c - k) inside in
          held :: tail (String.sub s k (String.length s - k)) 0 cuts
      | Seq _, k :: _ ->
          let what =
            if String.starts_with ~prefix:"\x1b[M" s then `Mouse else `Sequence
          in
          show (D.Cut what) :: tail (String.sub s k (String.length s - k)) 0 []
      | (Str _ | Paste _), _ ->
          (* Silence never ends a string or a paste. *)
          alone s)
    units

(* The decoder fed the units, silent where they say, each piece between
   silences cut into reads at [cuts] too. *)
let actual units cuts =
  let reads p =
    feeds (chunks p (List.filter (fun c -> c < String.length p) cuts))
  in
  let ops =
    List.concat_map
      (fun { u; inside; after } ->
        let pieces = chunks (unit_bytes u) inside in
        List.concat
          (List.mapi
             (fun i p -> (if i > 0 then [ Idle ] else []) @ reads p)
             pieces)
        @ if after then [ Idle ] else [])
      units
  in
  decode o_ref (ops @ [ Idle ])

let pp_units ppf (units, cuts) =
  List.iter
    (fun { u; inside; after } ->
      Format.fprintf ppf "%S%s%s " (unit_bytes u)
        (if inside = [] then ""
         else "{" ^ String.concat "," (List.map string_of_int inside) ^ "}")
        (if after then "|" else ""))
    units;
  Format.fprintf ppf "reads cut at [%s]"
    (String.concat ";" (List.map string_of_int cuts))

let units_gen =
  let open Gen in
  let+ units = list ~size:(int_range 1 8) placed_gen
  and+ cuts = list ~size:(int_range 0 3) (int_range 1 8) in
  (units, cuts)

let prop_silence (units, cuts) =
  same "silences" (reference units) (actual units cuts)

(* Never typed --------------------------------------------------------------- *)

let is_key e =
  String.starts_with ~prefix:"Key" e
  || String.starts_with ~prefix:"Text" e
  || String.starts_with ~prefix:"Invalid" e

let never_typed () =
  let o = owed ~cursor:0 ~strings:1 in
  List.iter
    (fun s ->
      let n = String.length s in
      let is_string = List.mem s strings in
      for k = 1 to n - 1 do
        let prefix = String.sub s 0 k and suffix = String.sub s k (n - k) in
        let held = decode o [ Feed prefix; Idle ] in
        let whole = decode o [ Feed prefix; Idle; Feed suffix; Idle ] in
        let msg = Printf.sprintf "%S cut at %d" s k in
        (* Before the bytes commit, ESC and its introducer are keys. *)
        if k = 1 then same msg [ escape ] held
        else if k = 2 then same msg [ alt s.[1] ] held
        else if is_string then (
          (* A string goes on through the silence. *)
          same msg [] held;
          same msg (decode o [ Feed s; Idle ]) whole)
        else (
          (match held with
          | [ e ] when String.starts_with ~prefix:"Cut" e -> ()
          | _ -> failf "%s: %s" msg (String.concat " " held));
          if List.exists is_key held then failf "%s: typed" msg);
        (* What follows a cut decodes as if from the start. *)
        if not (is_string && k > 2) then
          same msg (held @ decode o [ Feed suffix; Idle ]) whole
      done)
    (sequences @ strings)

(* Totality ------------------------------------------------------------------ *)

type action =
  | A_feed of string
  | A_idle
  | A_finish
  | A_reset
  | A_cut_paste
  | A_expect of int * int
  | A_settle

let action_gen =
  let open Gen in
  frequency
    [
      (8, map (fun s -> A_feed s) bytes_gen);
      (2, pure A_idle);
      (1, pure A_finish);
      (1, pure A_reset);
      (1, pure A_cut_paste);
      ( 1,
        map
          (fun (c, s) -> A_expect (c, s))
          (pair (int_range 0 2) (int_range 0 2)) );
      (1, pure A_settle);
    ]

let pp_action ppf = function
  | A_feed s -> Format.fprintf ppf "feed %S" s
  | A_idle -> Format.pp_print_string ppf "idle"
  | A_finish -> Format.pp_print_string ppf "finish"
  | A_reset -> Format.pp_print_string ppf "reset"
  | A_cut_paste -> Format.pp_print_string ppf "cut_paste"
  | A_expect (c, s) -> Format.fprintf ppf "expect %d %d" c s
  | A_settle -> Format.pp_print_string ppf "settle"

let actions =
  Gen.with_pp
    (fun ppf l ->
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
        pp_action ppf l)
    (Gen.list ~size:(Gen.int_range 0 20) action_gen)

(* The words a fresh decoder holds: its tables and buffers. *)
let base_words = Obj.reachable_words (Obj.repr (D.create ()))

let prop_total l =
  let d = D.create () in
  let owed_c = ref 0 and owed_s = ref 0 in
  (* Whether a string may have opened since the stream started: one goes on
     after what was owed is settled. *)
  let strings = ref false in
  let bounded what =
    let words = Obj.reachable_words (Obj.repr d) in
    (* At most a held CSI M or CSI [ header beyond the fresh decoder. *)
    if words > base_words + 16 then
      failf "%d words held %s, %d when fresh" words what base_words
  in
  List.iter
    (fun a ->
      (match a with
      | A_feed s ->
          D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) ignore
      | A_idle -> D.idle d ignore
      | A_finish ->
          D.finish d ignore;
          strings := !owed_s > 0
      | A_reset ->
          D.reset d;
          strings := !owed_s > 0
      | A_cut_paste -> D.cut_paste d ignore
      | A_expect (c, s) ->
          D.expect d (owed ~cursor:c ~strings:s);
          owed_c := !owed_c + c;
          owed_s := !owed_s + s;
          if s > 0 then strings := true
      | A_settle ->
          D.settle d (owed ~cursor:!owed_c ~strings:!owed_s);
          owed_c := 0;
          owed_s := 0);
      ignore (D.pending d : D.pending);
      (* With no string owed since the stream started, no string opens, and
         nothing the input sends is held but a header's fields, an
         unfinished character or report, and a paste's end marker. *)
      if not !strings then bounded "")
    l;
  D.finish d ignore;
  bounded "after finish"

let string_memory () =
  let d = D.create () in
  D.expect d (owed ~cursor:0 ~strings:1);
  let s = "\x1b]52;c;" ^ String.make 100_000 'A' ^ "\x07" in
  D.feed d (Bytes.of_string s) ~off:0 ~len:(String.length s) ignore;
  let words = Obj.reachable_words (Obj.repr d) in
  if words > base_words + 16 then
    failf "%d words held after a long reply, %d when fresh" words base_words

(* Round trip ---------------------------------------------------------------

   [Reply.encode] writes a reply as a terminal does, and the decoder reads it
   back: decoding [encode [r]] is [r] for every reply the decoder produces,
   among them every reply Vte answers with. *)

module R = I.Reply

(* Content a string carries: UTF-8, the command-string set's BS..CR, and the
   delimiters the decoder splits at. *)
let content =
  Gen.map (String.concat "")
    (Gen.list ~size:(Gen.int_range 0 6)
       (Gen.of_list
          [
            "a";
            "Z";
            "0";
            " ";
            ";";
            "=";
            "/";
            "|";
            "\t";
            "\n";
            "\xc3\xa9";
            "\xe6\x97\xa5";
          ]))

let number =
  Gen.frequency
    [ (4, Gen.int_range 0 1000); (1, Gen.nat); (1, Gen.of_list [ max_int ]) ]

let below_max = Gen.map (fun n -> if n = max_int then n - 1 else n) number

(* A header or sequence the decoder keeps as it is, read by Ansi.Parser. *)
let parsed s =
  match Ansi.Parser.parse s with
  | [ Csi seq ] | Open (Dcs seq) :: _ -> seq
  | _ -> invalid_arg s

let terminator = Gen.of_list [ `St; `Bel ]

let rgb =
  Gen.triple (Gen.int_range 0 255) (Gen.int_range 0 255) (Gen.int_range 0 255)

let reply : R.t Gen.t =
  let open Gen in
  one_of
    [
      map (fun l -> R.Primary_attributes l) (list ~size:(int_range 0 4) number);
      map
        (fun l -> R.Secondary_attributes l)
        (list ~size:(int_range 0 4) number);
      map (fun s -> R.Unit_id s) content;
      map
        (fun (dec, mode, setting) -> R.Mode { dec; mode; setting })
        (triple bool number
           (of_list
              R.
                [
                  Not_recognized; Set; Reset; Permanently_set; Permanently_reset;
                ]));
      map
        (fun (row, col, page) -> R.Cursor { row; col; page })
        (triple below_max below_max (option (map (fun n -> max 1 n) number)));
      map (fun s -> R.Operating_status s) (of_list [ `Ok; `Malfunction ]);
      map
        (fun (height, width) -> R.Text_area_pixels { height; width })
        (pair number number);
      map
        (fun (height, width) -> R.Cell_pixels { height; width })
        (pair number number);
      map (fun (rows, cols) -> R.Text_area { rows; cols }) (pair number number);
      map
        (fun (rows, cols, height, width) ->
          R.Resize { rows; cols; height; width })
        (quad number number number number);
      map (fun f -> R.Keyboard_flags f) number;
      map (fun s -> R.Color_scheme s) (of_list [ `Dark; `Light ]);
      map (fun s -> R.Version s) content;
      map
        (fun (valid, entries) -> R.Termcap { valid; entries })
        (pair bool
           (list ~size:(int_range 0 3)
              (pair (string_of ~size:(int_range 1 4) char) (option string))));
      map
        (fun (target, rgb, terminator) -> R.Color { target; rgb; terminator })
        (triple
           (one_of
              [
                of_list [ `Foreground; `Background; `Cursor ];
                map (fun i -> `Entry i) (int_range 0 999_999_999);
              ])
           rgb terminator);
      map
        (fun (selection, data, terminator) ->
          R.Clipboard { selection; data; terminator })
        (triple (of_list [ ""; "c"; "p"; "s0" ]) string terminator);
      map
        (fun (id, message) -> R.Graphics { id; message })
        (pair (option (int_range 0 999_999_999)) content);
      map
        (fun (content, terminator) ->
          R.Osc { content = "1337;" ^ content; terminator })
        (pair content terminator);
      map
        (fun (h, data) -> R.Dcs { header = parsed h; data })
        (pair (of_list [ "\x1bP1$r"; "\x1bP>1;2|"; "\x1bP=1s" ]) content);
      map
        (fun s -> R.Csi (parsed s))
        (of_list
           [
             "\x1b[?1a";
             "\x1b[1 q";
             "\x1b[5n";
             "\x1b[2;3:4x";
             "\x1b[;5x";
             "\x1b[>1;2;3x";
           ]);
      map (fun s -> R.Ss3 (parsed s)) (of_list [ "\x1b[1z"; "\x1b[1;2;3z" ]);
    ]

let prop_round_trip r =
  let d = D.create () in
  D.expect d (owed ~cursor:1 ~strings:1);
  let got = ref [] in
  let emit e = got := e :: !got in
  let bytes = R.encode [ r ] in
  D.feed d (Bytes.of_string bytes) ~off:0 ~len:(String.length bytes) emit;
  D.finish d emit;
  match List.rev !got with
  | [ D.Reply r' ] when r' = r -> ()
  | events ->
      failf "%S decodes to %s" bytes (String.concat "; " (List.map show events))

(* What would read back as another reply. *)
let unencodable () =
  List.iter
    (fun (msg, r) ->
      raises_match ~msg (Exn.invalid_arg ~substring:"Reply.encode") (fun () ->
          R.encode [ r ]))
    R.
      [
        ("a negative number", Primary_attributes [ -1 ]);
        ("a row of max_int", Cursor { row = max_int; col = 0; page = None });
        ("a negative row", Cursor { row = -1; col = 0; page = None });
        ("a negative column", Cursor { row = 0; col = -1; page = Some 1 });
        ("page 0", Cursor { row = 0; col = 0; page = Some 0 });
        ( "a channel above 255",
          Color { target = `Foreground; rgb = (256, 0, 0); terminator = `St } );
        ( "an empty capability name",
          Termcap { valid = true; entries = [ ("", None) ] } );
        ( "a selection holding ;",
          Clipboard { selection = "c;p"; data = ""; terminator = `St } );
        ("ESC in content", Version "a\x1b\\");
        ("BEL in content", Osc { content = "1\x07"; terminator = `Bel });
        ("UTF-8 C1 in content", Version "\xc2\x9b");
      ];
  equal ~msg:"no replies, no bytes" string "" (R.encode [])

(* Encoding keys and mouse reports -------------------------------------------- *)

module M = I.Mouse_report

let decode_all bytes =
  let d = D.create () in
  let got = ref [] in
  D.feed d (Bytes.of_string bytes) ~off:0 ~len:(String.length bytes) (fun e ->
      got := e :: !got);
  D.idle d (fun e -> got := e :: !got);
  List.rev !got

let keys : K.key list =
  K.
    [
      Enter;
      Tab;
      Backspace;
      Escape;
      Up;
      Down;
      Left;
      Right;
      Home;
      End;
      Page_up;
      Page_down;
      Insert;
      Delete;
      Begin;
      Print_screen;
      Pause;
      Menu;
      Scroll_lock;
      Caps_lock;
      Num_lock;
      Media Play;
      Media Mute;
      Modifier Left_shift;
      Modifier Right_meta;
      Unidentified 63000;
    ]
  @ List.init 35 (fun i -> K.F (i + 1))

(* A character a key types: a letter with Shift and Caps Lock applied, the
   shifted symbol of a US key, or a character Shift does not change. Shift is
   consumed when it changed the character. *)
let typed_gen =
  let open Gen in
  let lower = "abcdefghijklmnopqrstuvwxyz`1234567890-=[]\\;',./" in
  let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ~!@#$%^&*()_+{}|:\"<>?" in
  one_of
    [
      map
        (fun (i, shift, caps) ->
          let letter = i < 26 in
          let c =
            if letter then if shift <> caps then upper.[i] else lower.[i]
            else if shift then upper.[i]
            else lower.[i]
          in
          (Uchar.of_char c, false))
        (triple (int_range 0 (String.length lower - 1)) bool bool);
      map
        (fun (c, shift) -> (Uchar.of_int c, shift))
        (pair (of_list [ 0x20; 0xE9; 0x65E5; 0x1F600 ]) bool);
    ]

let mods_gen =
  Gen.map
    (fun b ->
      let bit n = b land n <> 0 in
      {
        K.shift = bit 1;
        alt = bit 2;
        ctrl = bit 4;
        super = bit 8;
        hyper = bit 16;
        meta = bit 32;
        caps_lock = bit 64;
        num_lock = bit 128;
      })
    (Gen.int_range 0 255)

let utf_8 u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

(* A keystroke as a terminal reads it: Shift set on a character only when
   it did not change it, the text typed unless it is a chord, the keypad on
   a key it has, and a base layout key on a character. *)
let keystroke_gen =
  let open Gen in
  map
    (fun ((key, shift), mods, (keypad, base), repeat) ->
      let mods = { mods with K.shift = mods.K.shift || shift } in
      let chord =
        mods.alt || mods.ctrl || mods.super || mods.hyper || mods.meta
      in
      let mods, text =
        match key with
        | K.Char c ->
            let mods = if shift then mods else { mods with shift = false } in
            (mods, if chord then "" else utf_8 c)
        | _ -> (mods, "")
      in
      let keypad =
        keypad
        &&
        match key with
        | K.Char c ->
            Uchar.to_int c < 0x80
            && String.contains "0123456789.,/*-+=" (Uchar.to_char c)
            && not mods.shift
        | Enter | Up | Down | Left | Right | Home | End | Page_up | Page_down
        | Insert | Delete ->
            true
        | _ -> false
      in
      let keypad = keypad || key = K.Begin in
      let base =
        match key with K.Char _ -> Option.map Uchar.of_char base | _ -> None
      in
      { K.key; mods; text; base; keypad; repeat })
    (quad
       (one_of
          [
            map (fun k -> (k, false)) (of_list keys);
            map (fun (c, s) -> (K.Char c, s)) typed_gen;
            map
              (fun c -> (K.Char (Uchar.of_char c), false))
              (of_list (List.init 10 (fun i -> Char.chr (0x30 + i))));
          ])
       mods_gen
       (pair bool (option (of_list [ 'c'; 'q'; '1' ])))
       bool)

let keyboard_gen =
  Gen.map
    (fun (cursor_keys, keypad, modify_other_keys, kitty) ->
      { K.cursor_keys; keypad; new_line = false; modify_other_keys; kitty })
    (Gen.quad Gen.bool Gen.bool (Gen.int_range 0 2) (Gen.int_range 0 31))

let pp_keyboard ppf (e : K.encoding) =
  Format.fprintf ppf "{ckm %b kp %b mok %d kitty %d}" e.cursor_keys e.keypad
    e.modify_other_keys e.kitty

let show_events l = String.concat "; " (List.map show l)

(* What the decoder reads from a keystroke's bytes encodes to bytes it reads
   the same: the encoding loses facts, never makes another keystroke. *)
let prop_key_projection (e, k) =
  match K.encode e k with
  | exception Invalid_argument _ -> ()
  | bytes -> (
      match decode_all bytes with
      | [ D.Key k' ] -> (
          let again = K.encode e k' in
          match decode_all again with
          | [ D.Key k'' ] when k'' = k' -> ()
          | l ->
              failf "%a: %S reads %a, which encodes to %S, read as %s" K.pp k
                bytes K.pp k' again (show_events l))
      | l -> failf "%a encodes to %S, read as %s" K.pp k bytes (show_events l))

(* With every enhancement of the kitty protocol, the bytes carry every fact
   of a keystroke and of its release. *)
let prop_key_exact ((cursor_keys, keypad), k) =
  let e =
    {
      K.cursor_keys;
      keypad;
      new_line = false;
      modify_other_keys = 0;
      kitty = 31;
    }
  in
  match K.encode e k with
  | exception Invalid_argument _ -> ()
  | bytes -> (
      (match decode_all bytes with
      | [ D.Key k' ] when k' = k -> ()
      | l -> failf "%S reads %s" bytes (show_events l));
      (* A release has no text, so it carries the key before Caps Lock. *)
      let key =
        match k.key with
        | Char c
          when k.text <> "" && k.mods.caps_lock
               && Uchar.to_int c >= 0x41
               && Uchar.to_int c <= 0x5A ->
            K.Char (Uchar.of_int (Uchar.to_int c + 32))
        | key -> key
      in
      let r = { K.key; mods = k.mods; base = k.base; keypad = k.keypad } in
      let bytes = K.encode_release e r in
      match decode_all bytes with
      | [ D.Key_release r' ] when r' = r -> ()
      | l -> failf "release %S reads %s" bytes (show_events l))

let mouse_gen =
  let open Gen in
  map
    (fun ((kind, x, y), (shift, alt, ctrl)) ->
      { M.kind; x; y; shift; alt; ctrl })
    (pair
       (triple
          (one_of
             [
               map
                 (fun b -> M.Press b)
                 (of_list M.[ Left; Middle; Right; Extra 8; Extra 11 ]);
               map
                 (fun b -> M.Release b)
                 (option (of_list M.[ Left; Right; Extra 9 ]));
               map (fun b -> M.Motion b) (option (of_list M.[ Left; Middle ]));
               map (fun d -> M.Scroll d) (of_list [ `Up; `Down; `Left; `Right ]);
             ])
          (int_range 0 400) (int_range 0 400))
       (triple bool bool bool))

let mouse_encoding_gen =
  Gen.map
    (fun (tracking, format) -> { M.tracking; format })
    (Gen.pair
       (Gen.of_list [ `X10; `Normal; `Button; `Any ])
       (Gen.of_list [ `X10; `Sgr; `Urxvt ]))

(* A report reads back as itself, less the release's button outside SGR and
   the modifiers of X10 tracking; one not reported, or out of X10's range,
   has no bytes. *)
let prop_mouse ((e : M.encoding), (m : M.t)) =
  let bytes = M.encode e m in
  let reported =
    match (m.kind, e.tracking) with
    | Press (Left | Middle | Right), _ -> true
    | _, `X10 -> false
    | (Press _ | Release _ | Scroll _), _ -> true
    | Motion (Some _), (`Button | `Any) -> true
    | Motion None, `Any -> true
    | Motion _, _ -> false
  in
  let fits = e.format <> `X10 || (m.x <= 222 && m.y <= 222) in
  if not (reported && fits) then equal string ~msg:"no report" "" bytes
  else
    let expected =
      {
        m with
        kind =
          (match m.kind with
          | Release _ when e.format <> `Sgr -> Release None
          | k -> k);
        shift = m.shift && e.tracking <> `X10;
        alt = m.alt && e.tracking <> `X10;
        ctrl = m.ctrl && e.tracking <> `X10;
      }
    in
    match decode_all bytes with
    | [ D.Mouse m' ] when m' = expected -> ()
    | l -> failf "%S reads %s" bytes (show_events l)

let () =
  run "matrix.input.decoder.props"
    [
      group "decoder"
        [
          prop ~count:(count 20_000) "split invariance"
            (Gen.with_pp pp_split split_gen)
            prop_split;
          prop ~count:(count 20_000) "silence follows the rule"
            (Gen.with_pp pp_units units_gen)
            prop_silence;
          test "a cut reply is never typed" never_typed;
          prop ~count:(count 5000) "no call order raises, memory stays bounded"
            actions prop_total;
          test "a long reply's memory is released" string_memory;
        ];
      group "encode"
        [
          prop ~count:(count 5000) "decoding an encoded reply gives it back"
            (Gen.with_pp R.pp reply) prop_round_trip;
          test "what has no bytes raises" unencodable;
          prop ~count:(count 20_000) "a keystroke read encodes to itself"
            (Gen.with_pp
               (fun ppf (e, k) ->
                 Format.fprintf ppf "%a %a" pp_keyboard e K.pp k)
               (Gen.pair keyboard_gen keystroke_gen))
            prop_key_projection;
          prop ~count:(count 20_000)
            "every kitty enhancement carries every fact"
            (Gen.with_pp
               (fun ppf (_, k) -> K.pp ppf k)
               (Gen.pair (Gen.pair Gen.bool Gen.bool) keystroke_gen))
            prop_key_exact;
          prop ~count:(count 20_000) "a mouse report reads back"
            (Gen.with_pp
               (fun ppf (_, m) -> M.pp ppf m)
               (Gen.pair mouse_encoding_gen mouse_gen))
            prop_mouse;
        ];
    ]
