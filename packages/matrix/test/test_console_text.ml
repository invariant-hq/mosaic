(* The text a Windows console's input records carry, as the reads of
   Tty.Signal.read give it. The decoder is portable C, run here over records
   the tests make, on any system. *)

open Windtrap

type kind = Key | Resize | Other

type record = {
  kind : kind;
  down : bool;
  vk : int;
  mods : int;
  unit : int;
  repeat : int;
}

external reads : int array -> int -> int -> string list * bool
  = "test_console_reads"

let shift = 0x1
let ctrl = 0x2
let alt = 0x4
let vk_menu = 0x12

(* The key a US layout types NUL with, as VkKeyScanW(0) gives it: 2, with
   Shift (0x100) and Ctrl (0x200). *)
let us_zero_key = 0x332

let key ?(down = true) ?(vk = 0) ?(mods = 0) ?(repeat = 1) unit =
  { kind = Key; down; vk; mods; unit; repeat }

let resize =
  { kind = Resize; down = false; vk = 0; mods = 0; unit = 0; repeat = 0 }

let focus = { resize with kind = Other }

(* Each read's bytes, in order, and whether a resize was read. *)
let read ?(zero_key = us_zero_key) ?(room = 64) records =
  let flat =
    Array.concat
      (List.map
         (fun r ->
           let kind = match r.kind with Key -> 0 | Resize -> 1 | Other -> 2 in
           [| kind; Bool.to_int r.down; r.vk; r.mods; r.unit; r.repeat |])
         records)
  in
  let rs, resized = reads flat zero_key room in
  (List.rev rs, resized)

let text ?zero_key ?room records =
  String.concat "" (fst (read ?zero_key ?room records))

(* The UTF-16 units of [s], a string of UTF-8. *)
let units s =
  let b = Buffer.create 16 in
  let add d = Buffer.add_utf_16be_uchar b (Uchar.utf_decode_uchar d) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      add d;
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  let u = Buffer.contents b in
  List.init (String.length u / 2) (fun i -> String.get_uint16_be u (2 * i))

let keys s = List.map key (units s)

let test_keys () =
  equal ~msg:"ASCII" string "abc" (text (keys "abc"));
  equal ~msg:"two bytes" string "é" (text (keys "é"));
  equal ~msg:"three bytes" string "€" (text (keys "€"));
  equal ~msg:"a surrogate pair" string "😀" (text (keys "😀"));
  equal ~msg:"a sequence" string "\027[A" (text (keys "\027[A"))

let test_records_with_no_text () =
  equal ~msg:"a key-up" string "" (text [ key ~down:false (Char.code 'a') ]);
  equal ~msg:"a key-down with no character" string ""
    (text [ key ~vk:0x10 ~mods:shift 0 ]);
  equal ~msg:"focus" string "a" (text [ focus; key (Char.code 'a') ]);
  let reads, resized = read [ resize ] in
  is_true ~msg:"a resize is read" resized;
  equal ~msg:"a resize gives no byte" (list string) [ "" ] reads;
  is_false ~msg:"keys alone" (snd (read (keys "ab")))

let test_nul () =
  equal ~msg:"the key the layout types NUL with" string "\000"
    (text [ key ~vk:0x32 ~mods:(shift lor ctrl) 0 ]);
  equal ~msg:"its key with other modifiers" string ""
    (text [ key ~vk:0x32 ~mods:ctrl 0 ]);
  equal ~msg:"its key released" string ""
    (text [ key ~down:false ~vk:0x32 ~mods:(shift lor ctrl) 0 ]);
  equal ~msg:"a layout with no such key" string ""
    (text ~zero_key:0xFFFF [ key ~vk:0x32 ~mods:(shift lor ctrl) 0 ])

let test_alt_numpad () =
  equal ~msg:"Alt released with a character" string "é"
    (text
       [
         key ~vk:vk_menu ~mods:alt 0;
         key ~vk:0x62 ~mods:alt 0;
         key ~down:false ~vk:0x62 ~mods:alt 0;
         key ~down:false ~vk:vk_menu 0xE9;
       ]);
  equal ~msg:"another key released with one" string ""
    (text [ key ~down:false ~vk:0x41 0xE9 ])

let test_repeat () =
  equal ~msg:"a repeated key" string "xxx" (text [ key ~repeat:3 0x78 ]);
  equal ~msg:"a repeat of 0 is once" string "x" (text [ key ~repeat:0 0x78 ]);
  equal ~msg:"cut by the room" (list string) [ "xx"; "x" ]
    (fst (read ~room:2 [ key ~repeat:3 0x78 ]));
  equal ~msg:"a character cut inside" (list string) [ "\xC3\xA9\xC3"; "\xA9" ]
    (fst (read ~room:3 [ key ~repeat:2 0xE9 ]));
  equal ~msg:"a surrogate is not repeated" string "😀"
    (text (List.map (fun r -> { r with repeat = 4 }) (keys "😀")))

let test_room () =
  equal ~msg:"records wait for the next read" (list string) [ "ab"; "c" ]
    (fst (read ~room:2 (keys "abc")));
  equal ~msg:"a character kept whole across reads" (list string)
    [ "\xE2"; "\x82"; "\xAC" ]
    (fst (read ~room:1 (keys "€")));
  equal ~msg:"a pair across reads" (list string) [ "\xF0\x9F"; "\x98\x80" ]
    (fst (read ~room:2 (keys "😀")))

let test_unpaired_surrogates () =
  let high = 0xD83D and low = 0xDE00 in
  equal ~msg:"a high one, then a character" string "\xEF\xBF\xBDa"
    (text [ key high; key 0x61 ]);
  equal ~msg:"a low one alone" string "\xEF\xBF\xBD" (text [ key low ]);
  equal ~msg:"two high ones, then a low one" string "\xEF\xBF\xBD😀"
    (text [ key high; key high; key low ]);
  equal ~msg:"a high one waits for its low one" string "" (text [ key high ])

(* A read takes records in order, and how much room each read has only cuts
   the text: the reads give the text the keys carry, whatever the room. *)
let prop_room_cuts_only (s, room) =
  let reads, _ = read ~room (keys s) in
  equal ~msg:"the text" string s (String.concat "" reads);
  List.iter
    (fun r -> is_true ~msg:"a read fits" (String.length r <= room))
    reads

let text_gen =
  let open Gen in
  let uchar =
    frequency
      [
        (4, int_range 0x20 0x7E);
        (2, int_range 0x80 0x7FF);
        (2, int_range 0x800 0xD7FF);
        (2, int_range 0x10000 0x10FFFF);
      ]
  in
  let* cps = list ~size:(int_range 0 12) uchar in
  let+ room = int_range 1 9 in
  let b = Buffer.create 16 in
  List.iter (fun cp -> Buffer.add_utf_8_uchar b (Uchar.of_int cp)) cps;
  (Buffer.contents b, room)

let () =
  exit
    (run "matrix.console_text"
       [
         group "Records"
           [
             test "keys" test_keys;
             test "records with no text" test_records_with_no_text;
             test "NUL" test_nul;
             test "Alt+numpad" test_alt_numpad;
             test "repeat counts" test_repeat;
             test "the room of a read" test_room;
             test "unpaired surrogates" test_unpaired_surrogates;
             prop ~count:1000 "the room cuts the text only"
               (Gen.with_pp
                  (fun ppf (s, room) -> Format.fprintf ppf "%S, room %d" s room)
                  text_gen)
               prop_room_cuts_only;
           ];
       ])
