include Escape
module Writer = Writer
module Color = Color
module Palette = Palette
module Attr = Attr
module Style = Style
module Parser = Parser
module Sgr_state = Sgr_state
module Styling = Styling

let render ?depth ?hyperlinks_enabled segments =
  Segment.render ?depth ?hyperlinks_enabled segments

(* A string without ESC, DEL or a non-ASCII byte has nothing to remove. *)
let rec is_plain s i =
  i >= String.length s
  ||
  let c = String.unsafe_get s i in
  c <> '\x1b' && c < '\x7f' && is_plain s (i + 1)

let strip s =
  if is_plain s 0 then s
  else
    let b = Buffer.create (String.length s) in
    let keep = function
      | Parser.Text text -> Buffer.add_string b text
      | Parser.Control c when c < ' ' -> Buffer.add_char b c
      | _ -> ()
    in
    let p = Parser.create () in
    ignore
      (Parser.feed p (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s)
         keep);
    Parser.finish p keep;
    Buffer.contents b

(* {1:sanitize Sanitizing} *)

(* Default_Ignorable_Code_Point (Unicode 17.0.0, DerivedCoreProperties.txt),
   as ranges in order, adjacent ones merged. *)
let default_ignorable =
  [|
    (0x00AD, 0x00AD);
    (0x034F, 0x034F);
    (0x061C, 0x061C);
    (0x115F, 0x1160);
    (0x17B4, 0x17B5);
    (0x180B, 0x180F);
    (0x200B, 0x200F);
    (0x202A, 0x202E);
    (0x2060, 0x206F);
    (0x3164, 0x3164);
    (0xFE00, 0xFE0F);
    (0xFEFF, 0xFEFF);
    (0xFFA0, 0xFFA0);
    (0xFFF0, 0xFFF8);
    (0x1BCA0, 0x1BCA3);
    (0x1D173, 0x1D17A);
    (0xE0000, 0xE0FFF);
  |]

let is_default_ignorable c =
  let rec find lo hi =
    lo < hi
    &&
    let mid = (lo + hi) / 2 in
    let first, last = default_ignorable.(mid) in
    if c < first then find lo mid else c <= last || find (mid + 1) hi
  in
  c >= 0xAD && find 0 (Array.length default_ignorable)

(* Join_Control and Variation_Selector (PropList.txt). *)
let is_joiner c = c = 0x200C || c = 0x200D

let is_selector c =
  (c >= 0xFE00 && c <= 0xFE0F)
  || (c >= 0x180B && c <= 0x180F && c <> 0x180E)
  || (c >= 0xE0100 && c <= 0xE01EF)

(* The RGI emoji tag sequences (emoji-sequences.txt): the black flag U+1F3F4,
   then the tags of a subdivision code and the cancel tag U+E007F. *)
let tag_flags =
  List.map
    (fun code ->
      let b = Buffer.create 28 in
      Buffer.add_utf_8_uchar b (Uchar.of_int 0x1F3F4);
      String.iter
        (fun c ->
          Buffer.add_utf_8_uchar b (Uchar.of_int (0xE0000 + Char.code c)))
        code;
      Buffer.add_utf_8_uchar b (Uchar.of_int 0xE007F);
      Buffer.contents b)
    [ "gbeng"; "gbsct"; "gbwls" ]

(* The flag of [tag_flags] at [i], where [s] has a black flag. *)
let tag_flag s i =
  List.find_opt
    (fun flag ->
      let n = String.length flag in
      i + n <= String.length s && String.equal flag (String.sub s i n))
    tag_flags

(* What the character before is, for the joiners and selectors after it. *)
type before = Blank | Shows | Selector | Joiner

(* [s] holds no control but LF and HT: keep what shows, and the joiners and
   selectors that change how what comes before it shows. *)
let drop_ignorables s =
  let b = Buffer.create (String.length s) in
  let rec go i before =
    if i < String.length s then
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let keep before' =
        Buffer.add_substring b s i n;
        go (i + n) before'
      in
      match if c = 0x1F3F4 then tag_flag s i else None with
      | Some flag ->
          Buffer.add_string b flag;
          go (i + String.length flag) Shows
      | None ->
          if is_joiner c then
            if before = Shows || before = Selector then keep Joiner
            else go (i + n) before
          else if is_selector c then
            if before = Shows then keep Selector else go (i + n) before
          else if is_default_ignorable c then go (i + n) before
          else keep (if c = 0x20 || c = 0x09 || c = 0x0A then Blank else Shows)
  in
  go 0 Blank;
  Buffer.contents b

(* Printable ASCII, LF and HT: nothing to remove. *)
let rec is_text s i =
  i >= String.length s
  ||
  let c = String.unsafe_get s i in
  ((c >= ' ' && c < '\x7f') || c = '\n' || c = '\t') && is_text s (i + 1)

let sanitize s =
  if is_text s 0 then s
  else
    let b = Buffer.create (String.length s) in
    (* A run of CRs, and the LF, VT, FF or NEL after it, end one line. *)
    let after_cr = ref false in
    let line_end ~cr =
      if not !after_cr then Buffer.add_char b '\n';
      after_cr := cr
    in
    let keep = function
      | Parser.Text text ->
          Buffer.add_string b text;
          after_cr := false
      | Parser.Control '\t' ->
          Buffer.add_char b '\t';
          after_cr := false
      | Parser.Control '\r' -> line_end ~cr:true
      | Parser.Control ('\n' | '\x0b' | '\x0c' | '\x85') -> line_end ~cr:false
      | _ -> ()
    in
    let p = Parser.create () in
    ignore
      (Parser.feed p (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s)
         keep);
    Parser.finish p keep;
    drop_ignorables (Buffer.contents b)
