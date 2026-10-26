open StdLabels

type width_method = [ `Unicode | `Unicode_wide | `Wcwidth ]
type line_break_kind = [ `LF | `CR | `CRLF ]
type position = { byte_offset : int; units : int; columns_used : int }

(* ASCII Helpers *)

(* A tab width below 1 is a mistake, raised at the call that makes it. *)
let check_tab_width name w =
  if w < 1 then invalid_arg ("Matrix_text." ^ name ^ ": tab width below 1");
  w

let check_sub name str ~pos ~len =
  let str_len = String.length str in
  if pos < 0 then invalid_arg (name ^ ": negative position");
  if len < 0 then invalid_arg (name ^ ": negative length");
  if pos > str_len || len > str_len - pos then
    invalid_arg (name ^ ": substring out of bounds")

(* Width of an ASCII byte (0-127). Tab returns tab_width, printable (0x20-0x7E)
   returns 1, control characters return 0. Two comparisons instead of a table
   lookup (which costs 3 dependent memory loads). *)
let[@inline] ascii_width ~tab_width b =
  if b = 0x09 then tab_width else if b >= 0x20 && b <= 0x7E then 1 else 0

(* Check if 4 consecutive bytes are all ASCII (< 128). Uses native int
   operations only — zero allocation on 64-bit OCaml. *)
let[@inline] is_ascii_4 str i =
  let c0 = Char.code (String.unsafe_get str i) in
  let c1 = Char.code (String.unsafe_get str (i + 1)) in
  let c2 = Char.code (String.unsafe_get str (i + 2)) in
  let c3 = Char.code (String.unsafe_get str (i + 3)) in
  c0 lor c1 lor c2 lor c3 < 128

let rec is_ascii_only_tail str len j =
  j >= len
  || Char.code (String.unsafe_get str j) < 128
     && is_ascii_only_tail str len (j + 1)

let rec is_ascii_only str len i =
  if i + 4 <= len then is_ascii_4 str i && is_ascii_only str len (i + 4)
  else is_ascii_only_tail str len i

(* Width Predicates *)

let[@inline] is_regional_indicator cp = cp >= 0x1F1E6 && cp <= 0x1F1FF
let[@inline] is_emoji_modifier cp = cp >= 0x1F3FB && cp <= 0x1F3FF

(* OpenTUI widens spacing marks with positive scalar width. Its width table
   zeroes Mc marks, unlike Uucp's tty hints. In Unicode 17, the remaining
   spacing marks are Thai SARA AM and Lao AM. *)
let[@inline] is_wide_spacing_mark cp = cp = 0x0E33 || cp = 0x0EB3

(* Codepoint Width *)

let[@inline] codepoint_width_wcwidth ~tab_width cp =
  if cp < 0x80 then
    if cp = 0x09 then tab_width else if cp < 32 || cp = 127 then 0 else 1
  else
    let w = Unicode.tty_width_hint (Uchar.unsafe_of_int cp) in
    if w = -1 then 0 else w

let uchar_width ~tab_width uchar =
  codepoint_width_wcwidth
    ~tab_width:(check_tab_width "uchar_width" tab_width)
    (Uchar.to_int uchar)

type grapheme_cluster_break =
  [ `CN
  | `CR
  | `EX
  | `L
  | `LF
  | `LV
  | `LVT
  | `PP
  | `RI
  | `SM
  | `T
  | `V
  | `XX
  | `ZWJ ]

(* Indexed by the table's value, which follows uucp's order. 3 to 6 are
   E_Base, E_Base_GAZ, E_Modifier and Glue_After_Zwj, which no character has
   had since Unicode 11.0; the table holds none of them. *)
let gcb_of_index : grapheme_cluster_break array =
  [|
    `CN;
    `CR;
    `EX;
    `XX;
    `XX;
    `XX;
    `XX;
    `L;
    `LF;
    `LV;
    `LVT;
    `PP;
    `RI;
    `SM;
    `T;
    `V;
    `XX;
    `ZWJ;
  |]

let grapheme_cluster_break u = gcb_of_index.(Unicode.grapheme_cluster_break u)

(* Unicode-width state machine.

   The [`Unicode] and [`Unicode_wide] methods fold the scalars of a unit
   through a small state machine handling VS16 emoji promotion, emoji
   modifier sequences, regional-indicator pairs and spacing marks. The whole
   state is one immediate int: the unit's width in the bits above bit 3 and
   four flags below. The width bits are [0] while [ws_has_width] is unset,
   except in a TAB's unit, which [unit_step] makes [tab] wide without it.

   The initial state is [0]; [ws_step] handles the first codepoint of a unit
   like any other (from state [0] every branch reduces to the unit-start
   behaviour). *)

let ws_has_width = 1 (* cluster has a base width *)
let ws_ri_pair = 2 (* last codepoint was the first RI of a pair *)
let ws_emoji = 4 (* last codepoint was a narrow emoji *)
let ws_base = 8 (* last codepoint was a narrow emoji modifier base *)
let ws_last = ws_emoji lor ws_base
let[@inline] ws_make w flags = (w lsl 4) lor flags
let[@inline] ws_width state = state lsr 4

(* [ws_step cp cp_w state] advances the state machine by the codepoint [cp]
   whose individual display width is [cp_w]. *)
let[@inline] ws_step ~wide cp cp_w state =
  let flags = state land 15 in
  if cp = 0xFE0F then
    if flags land ws_emoji <> 0 && ws_width state = 1 then ws_make 2 flags
    else state
  else if is_regional_indicator cp then
    if flags land ws_ri_pair <> 0 then
      ws_make (ws_width state + cp_w) ws_has_width
    else
      let w = if flags land ws_has_width = 0 then cp_w else ws_width state in
      ws_make w (flags lor ws_has_width lor ws_ri_pair)
  else if
    wide
    && flags land ws_has_width <> 0
    && not (Unicode.is_zero_in_grapheme (Uchar.unsafe_of_int cp))
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_last)
  else if flags land ws_has_width <> 0 && cp_w > 0 && is_wide_spacing_mark cp
  then ws_make (max 2 (ws_width state)) (flags land lnot ws_last)
  else if (not wide) && flags land ws_base <> 0 && is_emoji_modifier cp then
    ws_make 2 (flags land lnot ws_last)
  else if flags land ws_has_width = 0 && cp_w > 0 then
    let u = Uchar.unsafe_of_int cp in
    let emoji =
      if cp_w = 1 && Unicode.is_emoji u then
        if Unicode.is_emoji_modifier_base u then ws_last else ws_emoji
      else 0
    in
    ws_make cp_w (flags lor ws_has_width lor emoji)
  else state land lnot ws_last

(* UTF-8 decoding

   The folds below are slower with direct standard decoding (bench_text's
   position cases by 10 to 60%): even a cold decoder call spills loop state on
   valid input. Keep this inline decoder and test its malformed prefix
   consumption against the standard decoder. It returns the scalar and its byte length packed in one immediate
   int, [(cp lsl 3) lor len], so decoding allocates nothing. Bytes from
   [limit] on are not read: a scalar cut by [limit] is malformed. *)

let[@inline] is_cont_byte b = b land 0xC0 = 0x80
let[@inline] decoded_cp d = d lsr 3
let[@inline] decoded_len d = d land 7
let replacement_1 = (0xFFFD lsl 3) lor 1
let replacement_2 = (0xFFFD lsl 3) lor 2
let replacement_3 = (0xFFFD lsl 3) lor 3

let[@inline] decode_malformed str pos limit b0 =
  if b0 < 0xE0 || b0 > 0xF4 || pos + 1 >= limit then replacement_1
  else
    let b1 = Char.code (String.unsafe_get str (pos + 1)) in
    if
      (not (is_cont_byte b1))
      || (b0 = 0xE0 && b1 < 0xA0)
      || (b0 = 0xED && b1 >= 0xA0)
      || (b0 = 0xF0 && b1 < 0x90)
      || (b0 = 0xF4 && b1 >= 0x90)
    then replacement_1
    else if b0 < 0xF0 || pos + 2 >= limit then replacement_2
    else if is_cont_byte (Char.code (String.unsafe_get str (pos + 2))) then
      replacement_3
    else replacement_2

let[@inline] decode str pos limit =
  let b0 = Char.code (String.unsafe_get str pos) in
  if b0 < 0x80 then (b0 lsl 3) lor 1
  else if b0 land 0xE0 = 0xC0 then
    if pos + 1 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      if is_cont_byte b1 then
        let cp = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        if cp >= 0x80 then (cp lsl 3) lor 2
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else if b0 land 0xF0 = 0xE0 then
    if pos + 2 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      let b2 = Char.code (String.unsafe_get str (pos + 2)) in
      if is_cont_byte b1 && is_cont_byte b2 then
        let cp =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        if cp >= 0x800 && (cp < 0xD800 || cp > 0xDFFF) then (cp lsl 3) lor 3
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else if b0 land 0xF8 = 0xF0 then
    if pos + 3 < limit then
      let b1 = Char.code (String.unsafe_get str (pos + 1)) in
      let b2 = Char.code (String.unsafe_get str (pos + 2)) in
      let b3 = Char.code (String.unsafe_get str (pos + 3)) in
      if is_cont_byte b1 && is_cont_byte b2 && is_cont_byte b3 then
        let cp =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        if cp >= 0x10000 && cp <= 0x10FFFF then (cp lsl 3) lor 4
        else decode_malformed str pos limit b0
      else decode_malformed str pos limit b0
    else decode_malformed str pos limit b0
  else decode_malformed str pos limit b0

(* Display units

   A unit's state is one immediate int: the [ws_step] state of the unit in
   progress above bit 2, its kind in bits 1 and 2, and in bit 0 whether the
   last scalar started it. One transition, [unit_step], serves the streaming
   {!Segmenter} and the folds below, which keep the state in a loop variable
   so that measuring allocates nothing beyond the boundary state. *)

(* General_Category Cc: C0, DEL and C1. *)
let[@inline] is_control cp = cp < 0x20 || (cp >= 0x7F && cp < 0xA0)
let[@inline] is_printable_ascii b = b >= 0x20 && b < 0x7F
let kind_control = 1
let kind_text = 2
let[@inline] unit_ws u = u lsr 3
let[@inline] unit_kind u = (u lsr 1) land 3
let[@inline] unit_starts u = u land 1 <> 0
let[@inline] unit_width u = ws_width (unit_ws u)
let[@inline] unit_make ws kind = (ws lsl 3) lor (kind lsl 1) lor 1
let[@inline] unit_extend u ws = (ws lsl 3) lor (u land 6)

(* [unit_step seg ~wide ~wcwidth ~tab cp u] adds the scalar [cp] to the unit
   state [u]. A control is a unit of its own, of width [tab] for TAB and [0]
   otherwise. [`Wcwidth] starts a unit at each scalar of positive width;
   [seg] is unused then. *)
let[@inline] unit_step seg ~wide ~wcwidth ~tab cp u =
  if is_control cp then begin
    if not wcwidth then
      ignore
        (Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int cp)
          : bool);
    unit_make (if cp = 0x09 then ws_make tab 0 else 0) kind_control
  end
  else if wcwidth then
    let w = codepoint_width_wcwidth ~tab_width:0 cp in
    if w > 0 then unit_make (ws_make w ws_has_width) kind_text
    else if unit_kind u <> kind_text then unit_make 0 kind_text
    else u land lnot 1
  else
    let bw =
      Uuseg_grapheme_cluster.check_boundary_with_width seg
        (Uchar.unsafe_of_int cp)
    in
    let cp_w = (bw land 3) - 1 in
    (* Under [`Unicode] a spacing mark has no width of its own: one after a
       base adds none, and one that starts a cluster leaves it zero wide.
       A wide one (U+302E, U+302F) keeps its two cells. *)
    let cp_w = if (not wide) && bw land 8 <> 0 && cp_w = 1 then 0 else cp_w in
    if bw land 4 <> 0 || unit_kind u <> kind_text then
      unit_make (ws_step ~wide cp cp_w 0) kind_text
    else unit_extend u (ws_step ~wide cp cp_w (unit_ws u))

(* The unit of one printable ASCII character: [ws_step] from [0], without its
   table lookup. The emoji among them are # * and the digits, which VS16 makes
   keycap bases. *)
let[@inline] ascii_unit b =
  if b = 0x23 || b = 0x2A || (b >= 0x30 && b <= 0x39) then
    unit_make (ws_make 1 (ws_has_width lor ws_emoji)) kind_text
  else unit_make (ws_make 1 ws_has_width) kind_text

(* [ascii_run seg ~wcwidth ~ascii_left u str pos limit] is the number of
   bytes from [pos] that are complete units of one printable ASCII character
   given the state [u] and [seg]: each starts a unit, and is followed before
   [limit] by another printable ASCII byte, which therefore starts the next.
   [ascii_left] tells that the last scalar was printable ASCII. A printable
   ASCII character after another always starts a unit (GB999); the unit in
   progress can absorb the first one only if it ends with a Prepend
   character (GB9b), which [seg] knows. With [final], [limit] ends the text,
   which completes a run that reaches it: the result is then [limit - pos].
   Reads but does not update the state. *)
let[@inline] printable_at str i =
  is_printable_ascii (Char.code (String.unsafe_get str i))

let[@inline] ascii_run ?(final = false) seg ~wcwidth ~ascii_left u str pos limit
    =
  if
    pos + 1 >= limit
    || (not (printable_at str pos))
    || (not (printable_at str (pos + 1)))
    || not
         (ascii_left || wcwidth
         || unit_kind u <> kind_text
         || Uuseg_grapheme_cluster.is_boundary seg
              (Uchar.unsafe_of_int (Char.code (String.unsafe_get str pos))))
  then 0
  else begin
    let j = ref (pos + 1) in
    while !j + 1 < limit && printable_at str (!j + 1) do
      incr j
    done;
    if final && !j + 1 = limit then limit - pos else !j - pos
  end

(* The state after a run of [k] bytes from [pos] found by [ascii_run], for a
   fold, which keeps [seg] up to date. *)
let[@inline] after_ascii_run seg ~wcwidth str pos k =
  let last = Char.code (String.unsafe_get str (pos + k - 1)) in
  if not wcwidth then
    ignore
      (Uuseg_grapheme_cluster.check_boundary seg (Uchar.unsafe_of_int last)
        : bool);
  ascii_unit last

(* Never mutated: [`Wcwidth] does not segment by UAX #29. *)
let no_seg = Uuseg_grapheme_cluster.create ()

let[@inline] unit_seg width_method =
  match width_method with
  | `Wcwidth -> no_seg
  | `Unicode | `Unicode_wide -> Uuseg_grapheme_cluster.create ()

module Segmenter = struct
  type t = {
    seg : Uuseg_grapheme_cluster.t;
    wide : bool;
    wcwidth : bool;
    (* The unit in progress; [0] before the first scalar. *)
    mutable unit : int;
    (* The last scalar was printable ASCII, which [seg] may not have seen:
       every printable ASCII character leaves [seg] in the same state. *)
    mutable ascii_left : bool;
  }

  let create m =
    {
      seg = unit_seg m;
      wide = m = `Unicode_wide;
      wcwidth = m = `Wcwidth;
      unit = 0;
      ascii_left = false;
    }

  let reset s =
    if not s.wcwidth then Uuseg_grapheme_cluster.reset s.seg;
    s.unit <- 0;
    s.ascii_left <- false

  let ascii_x = Uchar.of_int 0x78

  let[@inline] sync s =
    if s.ascii_left then begin
      Uuseg_grapheme_cluster.reset s.seg;
      ignore (Uuseg_grapheme_cluster.check_boundary s.seg ascii_x : bool);
      s.ascii_left <- false
    end

  let width s = unit_width s.unit

  let add s u =
    let cp = Uchar.to_int u in
    if s.ascii_left && is_printable_ascii cp then begin
      s.unit <- ascii_unit cp;
      true
    end
    else begin
      sync s;
      let u =
        unit_step s.seg ~wide:s.wide ~wcwidth:s.wcwidth ~tab:0 cp s.unit
      in
      s.unit <- u;
      if is_printable_ascii cp && not s.wcwidth then s.ascii_left <- true;
      unit_starts u
    end

  (* A control is a unit of its own. [`Wcwidth] continues a unit with each
     scalar of no width. *)
  let may_continue m u v =
    let cu = Uchar.to_int u and cv = Uchar.to_int v in
    (not (is_control cu || is_control cv))
    &&
    match m with
    | `Wcwidth -> codepoint_width_wcwidth ~tab_width:0 cv <= 0
    | `Unicode | `Unicode_wide -> Uuseg_grapheme_cluster.may_continue u v

  let ascii_run s str ~pos ~len =
    check_sub "Matrix_text.Segmenter.ascii_run" str ~pos ~len;
    let k =
      ascii_run s.seg ~wcwidth:s.wcwidth ~ascii_left:s.ascii_left s.unit str pos
        (pos + len)
    in
    if k > 0 then begin
      s.unit <- ascii_unit (Char.code (String.unsafe_get str (pos + k - 1)));
      if not s.wcwidth then s.ascii_left <- true
    end;
    k
end

(* Grapheme Segmentation *)

let rec find_boundary_loop seg str limit pos =
  if pos >= limit then limit
  else
    let d = String.get_utf_8_uchar str pos in
    let u = Uchar.utf_decode_uchar d in
    if Uuseg_grapheme_cluster.check_boundary seg u then pos
    else find_boundary_loop seg str limit (pos + Uchar.utf_decode_length d)

(* Find the next grapheme cluster boundary starting at [start]. Returns the byte
   offset after the grapheme cluster. *)
let next_boundary seg str start limit =
  if start >= limit then limit
  else (
    Uuseg_grapheme_cluster.reset seg;
    let d = String.get_utf_8_uchar str start in
    let u = Uchar.utf_decode_uchar d in
    let _ = Uuseg_grapheme_cluster.check_boundary seg u in
    find_boundary_loop seg str limit (start + Uchar.utf_decode_length d))

(* Grapheme Iteration *)

let rec iter_graphemes_ascii str len f i =
  if i >= len then ()
  else if
    Char.code (Stdlib.String.unsafe_get str i) = 0x0D
    && i + 1 < len
    && Stdlib.String.unsafe_get str (i + 1) = '\n'
  then (
    f ~offset:i ~len:2;
    iter_graphemes_ascii str len f (i + 2))
  else (
    f ~offset:i ~len:1;
    iter_graphemes_ascii str len f (i + 1))

let rec iter_graphemes_unicode seg str len f i start =
  if i >= len then (if start < len then f ~offset:start ~len:(len - start))
  else
    let d = Stdlib.String.get_utf_8_uchar str i in
    let u = Uchar.utf_decode_uchar d in
    let next = i + Uchar.utf_decode_length d in
    if Uuseg_grapheme_cluster.check_boundary seg u then (
      f ~offset:start ~len:(i - start);
      iter_graphemes_unicode seg str len f next i)
    else iter_graphemes_unicode seg str len f next start

(* The clusters of the bytes from [pos] to [limit], as if they were the whole
   text. A layout calls it on one line, which starts and ends at a cluster
   boundary (UAX #29 GB4, GB5), so no scalar crosses [limit]. *)
let iter_graphemes_sub f str pos limit =
  if pos >= limit then ()
  else if is_ascii_only str limit pos then iter_graphemes_ascii str limit f pos
  else
    let seg = Uuseg_grapheme_cluster.create () in
    let d = Stdlib.String.get_utf_8_uchar str pos in
    let _ =
      Uuseg_grapheme_cluster.check_boundary seg (Uchar.utf_decode_uchar d)
    in
    iter_graphemes_unicode seg str limit f (pos + Uchar.utf_decode_length d) pos

let iter_graphemes f str = iter_graphemes_sub f str 0 (Stdlib.String.length str)

(* The fold of {!Segmenter} that visits units of positive width. *)
let rec visit_units f seg ~wide ~wcwidth str len tab i start u =
  if i >= len then begin
    let w = unit_width u in
    if w > 0 then f ~offset:start ~len:(len - start) ~width:w
  end
  else
    let k = ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i len in
    if k > 0 then begin
      let w = unit_width u in
      if i > start && w > 0 then f ~offset:start ~len:(i - start) ~width:w;
      let ends = i + k = len in
      for j = i to if ends then len - 1 else i + k - 2 do
        f ~offset:j ~len:1 ~width:1
      done;
      if not ends then
        visit_units f seg ~wide ~wcwidth str len tab (i + k)
          (i + k - 1)
          (after_ascii_run seg ~wcwidth str i k)
    end
    else
      let d = decode str i len in
      let cp = decoded_cp d in
      let next = i + decoded_len d in
      let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
      if unit_starts u' && i > start then begin
        let w = unit_width u in
        if w > 0 then f ~offset:start ~len:(i - start) ~width:w;
        visit_units f seg ~wide ~wcwidth str len tab next i u'
      end
      else visit_units f seg ~wide ~wcwidth str len tab next start u'

let iter_units ~width_method ~tab_width f str =
  let tab_width = check_tab_width "iter_units" tab_width in
  let len = Stdlib.String.length str in
  if len > 0 then
    visit_units f (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width 0 0 0

(* Width-position helpers *)

let zero_position = { byte_offset = 0; units = 0; columns_used = 0 }

(* Top-level loops share fitting and positioning without allocating a closure
   for each query. *)
let rec find_pos_ascii str len tab_width include_start_before columns i count
    used =
  if i >= len then { byte_offset = len; units = count; columns_used = used }
  else
    let width = ascii_width ~tab_width (Char.code (String.unsafe_get str i)) in
    if width <= 0 then
      find_pos_ascii str len tab_width include_start_before columns (i + 1)
        count used
    else
      let next_used = used + width in
      if
        (include_start_before && used >= columns)
        || ((not include_start_before) && next_used > columns)
      then { byte_offset = i; units = count; columns_used = used }
      else
        find_pos_ascii str len tab_width include_start_before columns (i + 1)
          (count + 1) next_used

(* The fold of {!Segmenter} that fits units of positive width into
   [columns]. [start] begins the unit in progress, [u]. *)
(* A unit of width [w] after [used] columns fits [columns]: it ends within
   them, or with [isb], it starts within them. *)
let[@inline] fits ~isb ~columns used w =
  not ((isb && used >= columns) || ((not isb) && used + w > columns))

let rec find_units seg ~wide ~wcwidth str len tab isb columns i start count used
    u =
  if i >= len then
    let w = unit_width u in
    if w > 0 && start < len then
      if not (fits ~isb ~columns used w) then
        { byte_offset = start; units = count; columns_used = used }
      else { byte_offset = len; units = count + 1; columns_used = used + w }
    else { byte_offset = len; units = count; columns_used = used }
  else
    let k = ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i len in
    if k > 0 then
      let w = unit_width u in
      let closes = i > start && w > 0 in
      if closes && not (fits ~isb ~columns used w) then
        { byte_offset = start; units = count; columns_used = used }
      else
        let count, used =
          if closes then (count + 1, used + w) else (count, used)
        in
        (* Complete units of width 1: all [k] at the end of the text. *)
        let ends = i + k = len in
        let complete = if ends then k else k - 1 in
        let n = min complete (max 0 (columns - used)) in
        if n < complete || ends then
          { byte_offset = i + n; units = count + n; columns_used = used + n }
        else
          find_units seg ~wide ~wcwidth str len tab isb columns (i + k)
            (i + k - 1)
            (count + n) (used + n)
            (after_ascii_run seg ~wcwidth str i k)
    else
      let d = decode str i len in
      let cp = decoded_cp d in
      let next = i + decoded_len d in
      let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
      if unit_starts u' && i > start then
        let w = unit_width u in
        if w <= 0 then
          find_units seg ~wide ~wcwidth str len tab isb columns next i count
            used u'
        else if not (fits ~isb ~columns used w) then
          { byte_offset = start; units = count; columns_used = used }
        else
          find_units seg ~wide ~wcwidth str len tab isb columns next i
            (count + 1) (used + w) u'
      else
        find_units seg ~wide ~wcwidth str len tab isb columns next start count
          used u'

let find_pos_units ~width_method ~tab_width ~include_start_before str ~columns =
  let len = Stdlib.String.length str in
  if len = 0 then zero_position
  else if is_ascii_only str len 0 then
    find_pos_ascii str len tab_width include_start_before columns 0 0 0
  else
    find_units (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width include_start_before
      columns 0 0 0 0 0

let find_wrap_pos ~width_method ~tab_width str ~max_columns =
  find_pos_units ~width_method
    ~tab_width:(check_tab_width "find_wrap_pos" tab_width)
    ~include_start_before:false str ~columns:max_columns

let find_pos ~width_method ~tab_width ?(include_start_before = false) str
    ~columns =
  find_pos_units ~width_method
    ~tab_width:(check_tab_width "find_pos" tab_width)
    ~include_start_before str ~columns

(* The width of the unit that starts at [i]. *)
let rec first_unit_width seg ~wide ~wcwidth str len tab i u =
  if i >= len then unit_width u
  else
    let d = decode str i len in
    let cp = decoded_cp d in
    let u' = unit_step seg ~wide ~wcwidth ~tab cp u in
    if unit_starts u' && u <> 0 then unit_width u
    else first_unit_width seg ~wide ~wcwidth str len tab (i + decoded_len d) u'

let width_at ~width_method ~tab_width str ~byte_offset =
  let tab_width = check_tab_width "width_at" tab_width in
  let len = Stdlib.String.length str in
  if byte_offset < 0 || byte_offset >= len then 0
  else
    let b = Char.code (String.unsafe_get str byte_offset) in
    if
      b < 0x80
      && (byte_offset + 1 = len || Char.code str.[byte_offset + 1] < 0x80)
    then ascii_width ~tab_width b
    else
      first_unit_width (unit_seg width_method)
        ~wide:(width_method = `Unicode_wide)
        ~wcwidth:(width_method = `Wcwidth) str len tab_width byte_offset 0

(* String Measurement *)

let rec measure_ascii_tail str len tab_width i total =
  if i >= len then total
  else
    let w =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    measure_ascii_tail str len tab_width (i + 1) (total + w)

let rec measure_ascii str len tab_width i total =
  if i + 4 <= len && is_ascii_4 str i then
    let w0 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str i))
    in
    let w1 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 1)))
    in
    let w2 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 2)))
    in
    let w3 =
      ascii_width ~tab_width (Char.code (Stdlib.String.unsafe_get str (i + 3)))
    in
    measure_ascii str len tab_width (i + 4) (total + w0 + w1 + w2 + w3)
  else measure_ascii_tail str len tab_width i total

(* The fold of {!Segmenter} that sums unit widths. *)
let rec measure_units seg ~wide ~wcwidth str limit tab i total u =
  if i >= limit then total + unit_width u
  else
    let k =
      ascii_run ~final:true seg ~wcwidth ~ascii_left:false u str i limit
    in
    if k > 0 && i + k = limit then total + unit_width u + k
    else if k > 0 then
      measure_units seg ~wide ~wcwidth str limit tab (i + k)
        (total + unit_width u + k - 1)
        (after_ascii_run seg ~wcwidth str i k)
    else
      let d = decode str i limit in
      let u' = unit_step seg ~wide ~wcwidth ~tab (decoded_cp d) u in
      measure_units seg ~wide ~wcwidth str limit tab
        (i + decoded_len d)
        (if unit_starts u' then total + unit_width u else total)
        u'

(* Under [`Wcwidth] a unit's width is that of its one scalar of positive
   width, so the sum over units is the sum over scalars. This loop is about
   10% faster than [measure_units] on non-ASCII text; a test checks that
   both agree. *)
let rec measure_scalars str limit tab i total =
  if i >= limit then total
  else
    let d = decode str i limit in
    let cp = decoded_cp d in
    let w =
      if cp = 0x09 then tab
      else if is_control cp then 0
      else codepoint_width_wcwidth ~tab_width:0 cp
    in
    measure_scalars str limit tab (i + decoded_len d) (total + w)

let measure ~width_method ~tab_width str =
  let tab_width = check_tab_width "measure" tab_width in
  let len = Stdlib.String.length str in
  if len = 0 then 0
  else if is_ascii_only str len 0 then measure_ascii str len tab_width 0 0
  else if width_method = `Wcwidth then measure_scalars str len tab_width 0 0
  else
    measure_units (unit_seg width_method)
      ~wide:(width_method = `Unicode_wide)
      ~wcwidth:(width_method = `Wcwidth) str len tab_width 0 0 0

let measure_sub ~width_method ~tab_width str ~pos ~len:sub_len =
  let tab_width = check_tab_width "measure_sub" tab_width in
  if sub_len <= 0 then 0
  else (
    check_sub "Matrix_text.measure_sub" str ~pos ~len:sub_len;
    let end_pos = pos + sub_len in
    if is_ascii_only str end_pos pos then
      measure_ascii str end_pos tab_width pos 0
    else if width_method = `Wcwidth then
      measure_scalars str end_pos tab_width pos 0
    else
      measure_units (unit_seg width_method)
        ~wide:(width_method = `Unicode_wide)
        ~wcwidth:(width_method = `Wcwidth) str end_pos tab_width pos 0 0)

let grapheme_count str =
  let n = ref 0 in
  iter_graphemes (fun ~offset:_ ~len:_ -> incr n) str;
  !n

(* Text Segmentation (wrap breaks, line breaks) *)

let[@inline] is_ascii_wrap_break b =
  match b with
  | 0x20 | 0x09 | 0x2D | 0x2F | 0x5C | 0x2C | 0x3B | 0x3A | 0x21 | 0x3F | 0x28
  | 0x29 | 0x5B | 0x5D | 0x7B | 0x7D ->
      true
  | _ -> false

(* Spaces and hyphens of UAX #14 class BA (break after) and ZW, and CJK
   closing punctuation. The spaces of class GL (glue), U+00A0, U+2007 and
   U+202F, forbid a break on either side and are left out. *)
let[@inline] is_unicode_wrap_break cp =
  match cp with
  | 0x1680 | 0x205F | 0x3000 | 0x200B | 0x00AD | 0x2010 | 0x3001 | 0x3002
  | 0xFF01 | 0xFF1F ->
      true
  | cp when cp >= 0x2000 && cp <= 0x200A -> cp <> 0x2007
  | _ -> false

type word_class = Ascii_word | Cjk_word | Other

let[@inline] is_ascii_word_byte b =
  (b >= 0x61 && b <= 0x7A)
  || (b >= 0x41 && b <= 0x5A)
  || (b >= 0x30 && b <= 0x39)
  || b = 0x5F

let[@inline] is_cjk_word_codepoint cp =
  (cp >= 0x3400 && cp <= 0x4DBF)
  || (cp >= 0x4E00 && cp <= 0x9FFF)
  || (cp >= 0xF900 && cp <= 0xFAFF)
  || (cp >= 0x20000 && cp <= 0x2A6DF)
  || (cp >= 0x2A700 && cp <= 0x2B73F)
  || (cp >= 0x2B740 && cp <= 0x2B81F)
  || (cp >= 0x2B820 && cp <= 0x2CEAF)
  || (cp >= 0x2CEB0 && cp <= 0x2EBEF)
  || (cp >= 0x2EBF0 && cp <= 0x2EE5D)
  || (cp >= 0x2F800 && cp <= 0x2FA1F)
  || (cp >= 0x3040 && cp <= 0x309F)
  || (cp >= 0x30A0 && cp <= 0x30FF)
  || (cp >= 0x31F0 && cp <= 0x31FF)
  || (cp >= 0xFF66 && cp <= 0xFF9D)
  || (cp >= 0x1100 && cp <= 0x11FF)
  || (cp >= 0x3130 && cp <= 0x318F)
  || (cp >= 0xA960 && cp <= 0xA97F)
  || (cp >= 0xAC00 && cp <= 0xD7AF)
  || (cp >= 0xD7B0 && cp <= 0xD7FF)

let[@inline] classify_word cp =
  if cp <= 0x7F then if is_ascii_word_byte cp then Ascii_word else Other
  else if is_cjk_word_codepoint cp then Cjk_word
  else Other

let[@inline] is_cjk_ascii_transition prev curr =
  match (prev, curr) with
  | Cjk_word, Ascii_word | Ascii_word, Cjk_word -> true
  | _ -> false

let[@inline] first_codepoint s off =
  let d = Stdlib.String.get_utf_8_uchar s off in
  Uchar.to_int (Uchar.utf_decode_uchar d)

(* The break opportunities of the bytes from [pos] to [len], as if they were
   the whole text; [~grapheme_offset] counts from [pos]. *)
let iter_breaks ~intercharacter f s pos len =
  let seg = Uuseg_grapheme_cluster.create () in
  let rec has_break i limit =
    if i >= limit then false
    else
      let b0 = Char.code (Stdlib.String.unsafe_get s i) in
      if b0 < 0x80 then is_ascii_wrap_break b0 || has_break (i + 1) limit
      else
        let d = Stdlib.String.get_utf_8_uchar s i in
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        is_unicode_wrap_break cp
        || has_break (i + Uchar.utf_decode_length d) limit
  in
  (* [prev_byte_off < 0] means no previous grapheme; [prev_class] is then a
     dummy. Plain ints avoid boxing three options per grapheme. *)
  let rec loop byte_off g_off prev_byte_off prev_g_off prev_class =
    if byte_off >= len then ()
    else
      let next = next_boundary seg s byte_off len in
      let cp = first_codepoint s byte_off in
      let curr_class = classify_word cp in
      if
        prev_byte_off >= 0
        && (is_cjk_ascii_transition prev_class curr_class
           || intercharacter && prev_class = Cjk_word && curr_class = Cjk_word
              && cp <> 0x3099 && cp <> 0x309A && cp <> 0x30A0 && cp <> 0x30FB
              && measure_sub ~width_method:`Unicode ~tab_width:2 s
                   ~pos:prev_byte_off ~len:(byte_off - prev_byte_off)
                 > 0)
      then
        f ~break_byte_offset:prev_byte_off ~next_byte_offset:byte_off
          ~grapheme_offset:prev_g_off;
      if has_break byte_off next then
        f ~break_byte_offset:byte_off ~next_byte_offset:next
          ~grapheme_offset:g_off;
      loop next (g_off + 1) byte_off g_off curr_class
  in
  loop pos 0 (-1) (-1) Other

let iter_wrap_breaks f s =
  iter_breaks ~intercharacter:true f s 0 (Stdlib.String.length s)

let iter_word_breaks f s =
  iter_breaks ~intercharacter:false f s 0 (Stdlib.String.length s)

let iter_line_breaks f s =
  let len = Stdlib.String.length s in
  let rec loop i =
    if i < len then
      let b = Char.code (Stdlib.String.unsafe_get s i) in
      if b = 0x0D then
        if i + 1 < len && Char.code (Stdlib.String.unsafe_get s (i + 1)) = 0x0A
        then (
          f ~pos:(i + 1) ~kind:`CRLF;
          loop (i + 2))
        else (
          f ~pos:i ~kind:`CR;
          loop (i + 1))
      else if b = 0x0A then (
        f ~pos:i ~kind:`LF;
        loop (i + 1))
      else loop (i + 1)
  in
  loop 0

module Layout = struct
  type affinity = Upstream | Downstream
  type caret = { grapheme : int; affinity : affinity }
  type wrap = [ `None | `Char | `Word ]

  let clamp n x = max 0 (min n x)

  let lower_bound a first last value =
    let lo = ref first and hi = ref last in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo) / 2) in
      if a.(mid) < value then lo := mid + 1 else hi := mid
    done;
    !lo

  let upper_bound a first last value =
    let lo = ref first and hi = ref last in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo) / 2) in
      if a.(mid) <= value then lo := mid + 1 else hi := mid
    done;
    !lo

  module Widths = Map.Make (Int)

  (* A source is indexed per logical line. UAX #29 breaks after and before CR,
     LF and the other controls (GB4, GB5) and keeps CR LF together (GB3), so a
     line with its terminator is segmented, measured and wrapped as if it were
     a text of its own, and an edit indexes again only the lines it touches.

     For each line a source holds its byte start, its first cluster and a
     slot, three words. A line of printable ASCII and its terminator is
     {e plain}: its clusters are its bytes and the terminator, and all but the
     terminator take one cell, so its slot holds nothing. The slot of another
     line holds the line's index once something asks about the line. A layout
     holds, for each line, the rows before it and a slot for the line's rows,
     two words, and builds a line's rows only when they are asked and the
     line does not imply them, that is when it wraps or hangs blanks. Sources
     made from one another by [edit] share the slots of the lines the edit
     leaves whole, and so do their layouts.

     A slot is an int array, so that an indexed line costs one block. [plain]
     and [unindexed] are told apart by physical equality. The index of a line
     of [count] clusters holds a key in entry 0, then the byte offsets of
     clusters 1 to [count] from the line's start (cluster 0 is at 0, and
     cluster [count], past the terminator, at the line's length). A key above
     0 says that the cells before clusters 0 to [count] follow, for one width
     method and tab width, and, if the line holds a cluster of width 0 other
     than its terminator, the first cluster at or after each one that takes a
     cell. The terminator ends every row that could skip it, so otherwise
     every cluster a row covers takes a cell and the skip is the identity. *)

  let plain = [| -1 |]
  let unindexed = [| -2 |]
  let offsets_only = 0

  let method_code = function
    | `Unicode -> 0
    | `Unicode_wide -> 1
    | `Wcwidth -> 2

  let[@inline] metrics_key ~width_method ~tab_width =
    method_code width_method + (3 * tab_width)

  let[@inline] fits a ~width_method ~tab_width =
    a.(0) > 0 && (a.(0) - 1) / 2 = metrics_key ~width_method ~tab_width

  let[@inline] has_skip a = a.(0) > 0 && (a.(0) - 1) land 1 = 1
  let[@inline] offset a j = if j = 0 then 0 else Array.unsafe_get a j

  type geometry = {
    g_method : width_method;
    g_tab : int;
    g_wrap : wrap;
    (* [`None] whenever rows do not wrap, at a width of 0 or less too. *)
    g_width : int;
    (* [0] when no row wraps or truncates. *)
    g_truncate : bool;
  }

  type source = {
    text : string;
    starts : int array;
    (* The byte start of each line, then the length of the text. *)
    firsts : int array;
    (* The first cluster of each line, then the number of clusters. *)
    slots : int array array;
    mutable layouts : t list;
    (* At most four, the latest first. *)
    mutable line : int; (* The line last found, where the next search starts. *)
  }

  and t = {
    source : source;
    geometry : geometry;
    rows : int array;
    (* The rows before each line, then the number of rows. *)
    mutable line_rows : int array array;
    (* Each line's rows, as the first and the last shown cluster of each, from
       the line's start; [[||]] until they are asked, and for a line whose one
       row it implies. [[||]] until a row is asked. *)
    mutable widths : int Widths.t option;
    (* How many lines have each widest row, counted when an edit first
       carries the layout. *)
    max_width : int;
    mutable built : int; (* The words of [line_rows]. *)
    mutable finger : int; (* The line of the row last found. *)
    (* The row last described (see [describe]), and its line's index. *)
    mutable at : int;
    mutable at_line : int;
    mutable at_base : int;
    mutable at_index : int array;
    mutable at_cells : int; (* Where the index's cells start, or -1. *)
    mutable at_skip : int; (* Where its skip starts, or -1. *)
    mutable at_first : int;
    mutable at_last : int;
    mutable at_visible : int;
    mutable at_width : int;
    mutable at_continuation : int;
  }

  (* ───── Lines of a text ───── *)

  let[@inline] is_terminator c = c = '\n' || c = '\r'

  (* The number of lines of the bytes from [pos] to [limit], which start a
     line. With [final], [limit] ends the text, whose last line has no
     terminator and may be empty; otherwise [limit] follows a terminator. *)
  let count_lines text pos limit ~final =
    let n = ref (if final then 1 else 0) in
    for i = pos to limit - 1 do
      match String.unsafe_get text i with
      | '\n' -> incr n
      | '\r' ->
          if not (i + 1 < limit && String.unsafe_get text (i + 1) = '\n') then
            incr n
      | _ -> ()
    done;
    !n

  (* [split text pos limit ~final add] calls [add start stop plain] on each of
     those lines, where [stop] follows its terminator. *)
  let split text pos limit ~final add =
    let start = ref pos and plain = ref true and i = ref pos in
    while !i < limit do
      let c = String.unsafe_get text !i in
      if is_terminator c then begin
        let stop =
          if
            c = '\r' && !i + 1 < limit && String.unsafe_get text (!i + 1) = '\n'
          then !i + 2
          else !i + 1
        in
        add !start stop !plain;
        start := stop;
        i := stop;
        plain := true
      end
      else begin
        if not (is_printable_ascii (Char.code c)) then plain := false;
        incr i
      end
    done;
    if final then add !start limit !plain

  (* A plain line's terminator is one cluster of one or two bytes. *)
  let cluster_count text start stop plain =
    if plain then
      if
        stop - start >= 2
        && String.unsafe_get text (stop - 2) = '\r'
        && String.unsafe_get text (stop - 1) = '\n'
      then stop - start - 1
      else stop - start
    else begin
      let n = ref 0 in
      iter_graphemes_sub (fun ~offset:_ ~len:_ -> incr n) text start stop;
      !n
    end

  (* The index of the [count] clusters from [start] to [stop], offsets only. *)
  let make_offsets text start stop count =
    let a = Array.make (count + 1) (stop - start) and j = ref 0 in
    a.(0) <- offsets_only;
    iter_graphemes_sub
      (fun ~offset ~len:_ ->
        if !j > 0 then a.(!j) <- offset - start;
        incr j)
      text start stop;
    a

  (* [a]'s offsets, with the cells of its clusters by [width_method] and
     [tab_width]. *)
  let make_metrics text start a ~count ~width_method ~tab_width =
    let cells = count + 1 in
    let b = Array.make (cells + count + 1) 0 and zero = ref false in
    Stdlib.Array.blit a 1 b 1 count;
    for j = 0 to count - 1 do
      let off = start + offset a j in
      let width =
        measure_sub ~width_method ~tab_width text ~pos:off
          ~len:(offset a (j + 1) - offset a j)
      in
      if width = 0 && not (is_terminator text.[off]) then zero := true;
      b.(cells + j + 1) <- b.(cells + j) + width
    done;
    let b =
      if not !zero then b
      else begin
        let c = Array.make (Array.length b + count + 1) count in
        Stdlib.Array.blit b 0 c 0 (Array.length b);
        let skip = Array.length b in
        for j = count - 1 downto 0 do
          c.(skip + j) <-
            (if b.(cells + j + 1) > b.(cells + j) then j else c.(skip + j + 1))
        done;
        c
      end
    in
    b.(0) <- 1 + Bool.to_int !zero + (2 * metrics_key ~width_method ~tab_width);
    b

  (* ───── Sources ───── *)

  let line_count s = Array.length s.slots
  let length s = s.firsts.(line_count s)
  let count s l = s.firsts.(l + 1) - s.firsts.(l)

  (* The clusters of line [l] before its terminator. *)
  let content s l = if l + 1 = line_count s then count s l else count s l - 1

  (* The last line in [0, line_count) whose entry of [a] is at most [v], from
     the line last found. *)
  let find s a v =
    let n = line_count s in
    let l = s.line in
    let l =
      if a.(l) <= v && (l + 1 = n || a.(l + 1) > v) then l
      else if l + 1 < n && a.(l + 1) <= v && (l + 2 = n || a.(l + 2) > v) then
        l + 1
      else max 0 (upper_bound a 0 n v - 1)
    in
    s.line <- l;
    l

  let line_of_cluster s g = find s s.firsts g
  let line_of_byte s b = find s s.starts b

  (* The index of a line that is not plain, made the first time it is asked. *)
  let index s l =
    let a = s.slots.(l) in
    if a != unindexed then a
    else begin
      let a = make_offsets s.text s.starts.(l) s.starts.(l + 1) (count s l) in
      s.slots.(l) <- a;
      a
    end

  let measured s l ~width_method ~tab_width =
    let a = index s l in
    if fits a ~width_method ~tab_width then a
    else begin
      let b =
        make_metrics s.text s.starts.(l) a ~count:(count s l) ~width_method
          ~tab_width
      in
      s.slots.(l) <- b;
      b
    end

  (* The byte offset of the line's cluster [j] from its start. *)
  let local_offset s l j =
    if s.slots.(l) == plain then
      if j < count s l then j else s.starts.(l + 1) - s.starts.(l)
    else offset (index s l) j

  (* The cells before the line's cluster [j]. *)
  let local_columns s l j ~width_method ~tab_width =
    if s.slots.(l) == plain then min j (content s l)
    else (measured s l ~width_method ~tab_width).(count s l + 1 + j)

  (* A source of [text] whose lines are the [prefix] first lines of [old],
     then the lines split from the bytes [pos] to [limit] of [text], then the
     lines of [old] from [l_suffix], whose bytes and clusters move. *)
  let assemble text ~old ~prefix ~pos ~limit ~final ~l_suffix =
    let fresh = count_lines text pos limit ~final in
    let suffix =
      match old with Some o -> line_count o - l_suffix | None -> 0
    in
    let n = prefix + fresh + suffix in
    let starts = Array.make (n + 1) (String.length text) in
    let firsts = Array.make (n + 1) 0 in
    let slots = Array.make n plain in
    (match old with
    | Some o ->
        Stdlib.Array.blit o.starts 0 starts 0 prefix;
        Stdlib.Array.blit o.firsts 0 firsts 0 (prefix + 1);
        Stdlib.Array.blit o.slots 0 slots 0 prefix
    | None -> ());
    let i = ref prefix and first = ref firsts.(prefix) in
    split text pos limit ~final (fun start stop is_plain ->
        starts.(!i) <- start;
        firsts.(!i) <- !first;
        if not is_plain then slots.(!i) <- unindexed;
        first := !first + cluster_count text start stop is_plain;
        incr i);
    (match old with
    | Some o when suffix > 0 ->
        let shift = String.length text - String.length o.text in
        let cshift = !first - o.firsts.(l_suffix) in
        for k = 0 to suffix - 1 do
          starts.(!i + k) <- o.starts.(l_suffix + k) + shift;
          firsts.(!i + k) <- o.firsts.(l_suffix + k) + cshift
        done;
        Stdlib.Array.blit o.slots l_suffix slots !i suffix;
        firsts.(n) <- o.firsts.(line_count o) + cshift
    | Some _ | None -> firsts.(n) <- !first);
    { text; starts; firsts; slots; layouts = []; line = 0 }

  (* ───── Rows of a line ───── *)

  (* The clusters a line of [n] truncated at [width] shows: those that fit in
     the cells before the last one, which the ellipsis takes. A width of 0 or
     less has no cell for either, and the row shows only the clusters of
     width 0 that begin the line. A plain line has no [cells] in [index]. *)
  let truncated index ~cells n width =
    let fit = max 0 (width - 1) in
    if cells < 0 then min n fit
    else max 0 (upper_bound index cells (cells + n + 1) fit - cells - 1)

  (* [build_line] cuts the [n] clusters of a line before its terminator into
     rows and calls [emit first last visible_last columns continuation] on
     each, in clusters from the line's start. A plain line's cells and offsets
     are its cluster indices; another line reads them from [index], which
     has its cells. [breaks] marks where a word-wrapped row may start; a plain
     line reads its bytes. *)
  let build_line geo text start n ~count ~index ~breaks emit =
    let plain = index == plain in
    let cells = if plain then -1 else count + 1 in
    let[@inline] p j =
      if plain then j else Array.unsafe_get index (cells + j)
    in
    let is_blank j =
      if plain then String.unsafe_get text (start + j) = ' '
      else
        let off = offset index j in
        offset index (j + 1) = off + 1
        &&
        let c = String.unsafe_get text (start + off) in
        c = ' ' || c = '\t'
    in
    let breaks_at j =
      if plain then
        j > 0
        && is_ascii_wrap_break
             (Char.code (String.unsafe_get text (start + j - 1)))
      else Bytes.get breaks j <> '\000'
    in
    let width = geo.g_width and wrap = geo.g_wrap in
    if wrap = `None || n = 0 then begin
      let natural = p n in
      let visible_last =
        if geo.g_truncate && natural > width then truncated index ~cells n width
        else n
      in
      let ellipsis = visible_last < n && width > 0 in
      emit 0 n visible_last (p visible_last + if ellipsis then 1 else 0) 0
    end
    else begin
      let start_ = ref 0 and g = ref 0 and best = ref 0 in
      let continuation = ref 0 and visible_end = ref n in
      (* A row that wraps at [stop] hangs the blanks that end it, shown in no
         cell. *)
      let wrapped stop =
        let shown = ref stop in
        while !shown > !start_ && is_blank (!shown - 1) do
          decr shown
        done;
        emit !start_ stop !shown (p !shown - p !start_) !continuation;
        incr continuation;
        start_ := stop;
        g := stop;
        best := stop
      in
      while !g < n do
        if wrap = `Word && !g > !start_ && breaks_at !g then best := !g;
        let used = p !g - p !start_ in
        let next = p (!g + 1) - p !start_ in
        if next > width && used > 0 && p (!g + 1) > p !g then
          if is_blank !g then begin
            (* The blanks a full row meets hang at its end, and the row wraps
               after them; at the end of the line it ends with them. *)
            let shown = !g in
            while !g < n && is_blank !g do
              incr g
            done;
            if !g = n then visible_end := shown else wrapped !g
          end
          else wrapped (if wrap = `Word && !best > !start_ then !best else !g)
        else incr g
      done;
      emit !start_ n !visible_end (p !visible_end - p !start_) !continuation
    end

  (* Where a word-wrapped row of a line that is not plain may start. *)
  let line_breaks text start stop index ~count =
    let breaks = Bytes.make (count + 1) '\000' and j = ref 0 in
    iter_breaks ~intercharacter:true
      (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset:_ ->
        let o = next_byte_offset - start in
        while !j < count && offset index !j < o do
          incr j
        done;
        if offset index !j = o then Bytes.set breaks !j '\001')
      text start stop;
    breaks

  (* [cut g s l emit] runs [build_line] on line [l] of [s]. A line that is not
     plain is measured from its index if that has the cells, and otherwise
     from an index made for the call, so that measuring every line keeps no
     cells. *)
  let cut g s l emit =
    let start = s.starts.(l) and n = content s l in
    let a = s.slots.(l) in
    if a == plain then
      build_line g s.text start n ~count:0 ~index:plain ~breaks:Bytes.empty emit
    else begin
      let count = count s l in
      let a =
        if a == unindexed then make_offsets s.text start s.starts.(l + 1) count
        else a
      in
      let a =
        if fits a ~width_method:g.g_method ~tab_width:g.g_tab then a
        else
          make_metrics s.text start a ~count ~width_method:g.g_method
            ~tab_width:g.g_tab
      in
      let natural = a.(count + 1 + n) in
      if g.g_wrap <> `None && natural <= g.g_width then emit 0 n n natural 0
      else
        let breaks =
          if g.g_wrap = `Word && n > 0 then
            line_breaks s.text start s.starts.(l + 1) a ~count
          else Bytes.empty
        in
        build_line g s.text start n ~count ~index:a ~breaks emit
    end

  type size = { mutable rows_ : int; mutable widest : int }

  (* The rows of line [l] and the width of its widest, in [size]. A plain line
     that fits needs no cut. *)
  let size g s l size =
    let n = content s l in
    if s.slots.(l) == plain && g.g_wrap = `None then begin
      size.rows_ <- 1;
      size.widest <- (if g.g_truncate && n > g.g_width then g.g_width else n)
    end
    else if s.slots.(l) == plain && n <= g.g_width then begin
      size.rows_ <- 1;
      size.widest <- n
    end
    else begin
      size.rows_ <- 0;
      size.widest <- 0;
      cut g s l (fun _ _ _ columns _ ->
          size.rows_ <- size.rows_ + 1;
          size.widest <- max size.widest columns)
    end

  let add_width w widths =
    Widths.update w (function None -> Some 1 | Some k -> Some (k + 1)) widths

  let remove_width w widths =
    Widths.update w
      (function Some 1 | None -> None | Some k -> Some (k - 1))
      widths

  let make source geometry rows line_rows widths max_width built =
    {
      source;
      geometry;
      rows;
      line_rows;
      widths;
      max_width;
      built;
      finger = 0;
      at = -1;
      at_line = 0;
      at_base = 0;
      at_index = plain;
      at_cells = -1;
      at_skip = -1;
      at_first = 0;
      at_last = 0;
      at_visible = 0;
      at_width = 0;
      at_continuation = 0;
    }

  let lay_out s geometry =
    let n = line_count s in
    let rows = Array.make (n + 1) 0 in
    let sz = { rows_ = 0; widest = 0 } and widest = ref 0 in
    for l = 0 to n - 1 do
      size geometry s l sz;
      rows.(l + 1) <- rows.(l) + sz.rows_;
      widest := max !widest sz.widest
    done;
    make s geometry rows [||] None !widest 0

  (* How many lines of [t] have each widest row. *)
  let widths t =
    match t.widths with
    | Some widths -> widths
    | None ->
        let n = line_count t.source and sz = { rows_ = 0; widest = 0 } in
        let each = Array.make n 0 in
        for l = 0 to n - 1 do
          size t.geometry t.source l sz;
          each.(l) <- sz.widest
        done;
        Array.sort ~cmp:Int.compare each;
        let widths = ref Widths.empty and l = ref 0 in
        while !l < n do
          let k = ref 1 in
          while !l + !k < n && each.(!l + !k) = each.(!l) do
            incr k
          done;
          widths := Widths.add each.(!l) !k !widths;
          l := !l + !k
        done;
        t.widths <- Some !widths;
        !widths

  (* A layout of [s] made from [old], a layout of the source that an edit
     made [s] from by replacing [old]'s lines [la] to [lb] by the [fresh]
     lines from [la]. The other lines keep their rows. *)
  let carry s (old : t) ~la ~lb ~fresh =
    let os = old.source and g = old.geometry in
    let n = line_count s in
    let rows = Array.make (n + 1) 0 in
    Stdlib.Array.blit old.rows 0 rows 0 (la + 1);
    let sz = { rows_ = 0; widest = 0 } in
    let widths = ref (widths old) and built = ref old.built in
    for l = la to lb do
      size g os l sz;
      widths := remove_width sz.widest !widths;
      if Array.length old.line_rows > 0 then begin
        let a = old.line_rows.(l) in
        if Array.length a > 0 then built := !built - Array.length a - 1
      end
    done;
    for l = la to la + fresh - 1 do
      size g s l sz;
      rows.(l + 1) <- rows.(l) + sz.rows_;
      widths := add_width sz.widest !widths
    done;
    let shift = rows.(la + fresh) - old.rows.(lb + 1) in
    for l = la + fresh + 1 to n do
      rows.(l) <- old.rows.(l - la - fresh + lb + 1) + shift
    done;
    let line_rows =
      if Array.length old.line_rows = 0 then [||]
      else begin
        let line_rows = Array.make n [||] in
        Stdlib.Array.blit old.line_rows 0 line_rows 0 la;
        Stdlib.Array.blit old.line_rows (lb + 1) line_rows (la + fresh)
          (n - la - fresh);
        line_rows
      end
    in
    let max_width =
      match Widths.max_binding_opt !widths with Some (w, _) -> w | None -> 0
    in
    make s g rows line_rows (Some !widths) max_width !built

  (* ───── Rows ───── *)

  (* The line of row [r], from the line last found. *)
  let line_of_row t r =
    let n = line_count t.source and rows = t.rows in
    let l = t.finger in
    let l =
      if rows.(l) <= r && rows.(l + 1) > r then l
      else if l + 1 < n && rows.(l + 1) <= r && rows.(l + 2) > r then l + 1
      else max 0 (min (n - 1) (upper_bound rows 0 n r - 1))
    in
    t.finger <- l;
    l

  (* Loads line [l]'s index into [t]'s description. *)
  let load t l =
    let s = t.source in
    t.at_line <- l;
    t.at_base <- s.firsts.(l);
    if s.slots.(l) == plain then begin
      t.at_index <- plain;
      t.at_cells <- -1;
      t.at_skip <- -1
    end
    else begin
      let a =
        measured s l ~width_method:t.geometry.g_method
          ~tab_width:t.geometry.g_tab
      in
      let count = count s l in
      t.at_index <- a;
      t.at_cells <- count + 1;
      t.at_skip <- (if has_skip a then (2 * count) + 2 else -1)
    end

  (* The cells before cluster [g] of the loaded line, from its start. *)
  let[@inline] p t g =
    let j = g - t.at_base in
    if t.at_cells < 0 then j else Array.unsafe_get t.at_index (t.at_cells + j)

  (* A line's rows, made the first time they are asked. *)
  let rows_of t l =
    if Array.length t.line_rows = 0 then
      t.line_rows <- Array.make (line_count t.source) [||];
    let a = t.line_rows.(l) in
    if Array.length a > 0 then a
    else begin
      let a = Array.make (2 * (t.rows.(l + 1) - t.rows.(l))) 0 and i = ref 0 in
      cut t.geometry t.source l (fun first _ visible _ _ ->
          a.(!i) <- first;
          a.(!i + 1) <- visible;
          i := !i + 2);
      t.line_rows.(l) <- a;
      t.built <- t.built + Array.length a + 1;
      a
    end

  (* Describes row [r], which [t] has, in the [at_] fields: its line, with the
     line's index, and its bounds and width in clusters of the source. A row
     with an ellipsis is its line's only one, and so is a row that its line
     implies: a line that does not wrap, or that wraps and fits. *)
  let describe t r =
    if t.at <> r then begin
      let l = line_of_row t r in
      load t l;
      let g = t.geometry and base = t.at_base in
      let n = content t.source l in
      let k = r - t.rows.(l) in
      let count = t.rows.(l + 1) - t.rows.(l) in
      let natural = p t (base + n) in
      if count = 1 && (g.g_wrap = `None || natural <= g.g_width) then begin
        let visible =
          if g.g_truncate && natural > g.g_width then
            truncated t.at_index ~cells:t.at_cells n g.g_width
          else n
        in
        let ellipsis = visible < n && g.g_width > 0 in
        t.at_first <- base;
        t.at_last <- base + n;
        t.at_visible <- base + visible;
        t.at_width <- (p t (base + visible) + if ellipsis then 1 else 0);
        t.at_continuation <- 0
      end
      else begin
        let a = rows_of t l in
        let first = base + a.(2 * k) and visible = base + a.((2 * k) + 1) in
        t.at_first <- first;
        t.at_last <-
          (if k + 1 < count then base + a.(2 * (k + 1)) else base + n);
        t.at_visible <- visible;
        t.at_width <- p t visible - p t first;
        t.at_continuation <- k
      end;
      t.at <- r
    end

  module Source = struct
    type t = source

    let text t = t.text
    let length = length
    let line_count = line_count

    let byte_offset t g =
      let g = clamp (length t) g in
      let l = line_of_cluster t g in
      t.starts.(l) + local_offset t l (g - t.firsts.(l))

    let grapheme_of_byte t ~affinity byte =
      let byte = clamp (String.length t.text) byte in
      let l = line_of_byte t byte in
      let o = byte - t.starts.(l) and count = count t l in
      let j =
        if t.slots.(l) == plain then
          if o < count then o
          else if o = t.starts.(l + 1) - t.starts.(l) then count
          else if affinity = Upstream then count - 1
          else count
        else if o = 0 then 0
        else
          let a = index t l in
          let j = lower_bound a 1 (count + 1) o in
          if affinity = Upstream && a.(j) <> o then j - 1 else j
      in
      t.firsts.(l) + j

    let line_start t line = t.firsts.(clamp (line_count t - 1) line)

    let line_end t line =
      let line = clamp (line_count t - 1) line in
      t.firsts.(line) + content t line

    let line_of_offset t g = line_of_cluster t (clamp (length t) g)

    let of_string text =
      assemble text ~old:None ~prefix:0 ~pos:0 ~limit:(String.length text)
        ~final:true ~l_suffix:0

    let columns ?(width_method = `Unicode) ?(tab_width = 2) t ~first ~last =
      let tab_width = check_tab_width "Layout" tab_width in
      let lo = clamp (length t) (min first last)
      and hi = clamp (length t) (max first last) in
      let l0 = line_of_cluster t lo in
      let c0 =
        local_columns t l0 (lo - t.firsts.(l0)) ~width_method ~tab_width
      in
      let l1 = line_of_cluster t hi in
      let c1 =
        local_columns t l1 (hi - t.firsts.(l1)) ~width_method ~tab_width
      in
      let total = ref (c1 - c0) in
      for l = l0 to l1 - 1 do
        total :=
          !total + local_columns t l (content t l) ~width_method ~tab_width
      done;
      if last < first then - !total else !total

    let slice t ~first ~last =
      let first = byte_offset t first and last = byte_offset t last in
      if last <= first then "" else Stdlib.String.sub t.text first (last - first)

    let edit t ~first ~last s =
      let len = String.length t.text in
      if first < 0 || first > last || last > len then
        invalid_arg "Matrix_text.Layout.Source.edit: bytes out of bounds";
      let text =
        let b = Bytes.create (len - (last - first) + String.length s) in
        Stdlib.Bytes.blit_string t.text 0 b 0 first;
        Stdlib.Bytes.blit_string s 0 b first (String.length s);
        Stdlib.Bytes.blit_string t.text last b
          (first + String.length s)
          (len - last);
        Bytes.unsafe_to_string b
      in
      let shift = String.length text - len and n = line_count t in
      (* The lines the replaced bytes touch, and the line before them if it
         ends with a CR that an LF may now follow. *)
      let la = line_of_byte t first in
      let lb = if last > first then line_of_byte t (last - 1) else la in
      let la =
        if la > 0 && t.text.[t.starts.(la) - 1] = '\r' then la - 1 else la
      in
      (* Then the lines after them, until the bytes split again end with a
         terminator that the next line cannot join. *)
      let lb = ref lb in
      let continues () =
        let e = t.starts.(!lb + 1) + shift in
        e > t.starts.(la)
        && ((not (is_terminator text.[e - 1]))
           || (text.[e - 1] = '\r' && e < String.length text && text.[e] = '\n')
           )
      in
      while !lb + 1 < n && continues () do
        incr lb
      done;
      let lb = !lb in
      let final = lb + 1 = n in
      let limit =
        if final then String.length text else t.starts.(lb + 1) + shift
      in
      let edited =
        assemble text ~old:(Some t) ~prefix:la ~pos:t.starts.(la) ~limit ~final
          ~l_suffix:(lb + 1)
      in
      (* An edit that touches every line leaves no rows to carry: the new
         source is laid out when asked, as one made from scratch. *)
      if la > 0 || lb + 1 < n then begin
        let fresh = line_count edited - (n - (lb - la + 1)) in
        edited.layouts <-
          Stdlib.List.map (carry edited ~la ~lb ~fresh) t.layouts
      end;
      edited
  end

  (* ───── Layouts ───── *)

  (* Built rows share 32 MiB. The layout asked for is kept whatever its size,
     and the others while they fit what it leaves, at most four in all. *)
  let row_budget = 32 * 1024 * 1024 / (Sys.word_size / 8)
  let capacity = 4

  let rec fits_budget active budget count = function
    | [] -> true
    | l :: rest ->
        if l == active then fits_budget active budget count rest
        else
          count < capacity && l.built <= budget
          && fits_budget active (budget - l.built) (count + 1) rest

  let retain s active =
    if not (fits_budget active (row_budget - active.built) 1 s.layouts) then begin
      let budget = ref (row_budget - active.built) and kept = ref 1 in
      s.layouts <-
        Stdlib.List.filter
          (fun l ->
            l == active
            ||
            if !kept < capacity && l.built <= !budget then begin
              incr kept;
              budget := !budget - l.built;
              true
            end
            else false)
          s.layouts
    end

  let rec cached g_method g_tab g_wrap g_width g_truncate = function
    | [] -> None
    | l :: rest ->
        let g = l.geometry in
        if
          g.g_method = g_method && g.g_tab = g_tab && g.g_wrap = g_wrap
          && g.g_width = g_width && g.g_truncate = g_truncate
        then Some l
        else cached g_method g_tab g_wrap g_width g_truncate rest

  let create ?(width_method = `Unicode) ?(tab_width = 2) ?(truncate = false)
      source ~width ~wrap =
    let tab_width = check_tab_width "Layout" tab_width in
    let none = wrap = `None || width <= 0 in
    let truncate = wrap = `None && truncate in
    let g_wrap = if none then `None else wrap in
    let g_width =
      if not none then width else if truncate then max 0 width else 0
    in
    match
      cached width_method tab_width g_wrap g_width truncate source.layouts
    with
    | Some l ->
        retain source l;
        l
    | None ->
        let geometry =
          {
            g_method = width_method;
            g_tab = tab_width;
            g_wrap;
            g_width;
            g_truncate = truncate;
          }
        in
        let l = lay_out source geometry in
        source.layouts <- l :: source.layouts;
        retain source l;
        l

  let measure ?width_method ?tab_width ?truncate source ~width ~wrap =
    let l = create ?width_method ?tab_width ?truncate source ~width ~wrap in
    (l.rows.(line_count source), l.max_width)

  let source t = t.source
  let row_count t = t.rows.(line_count t.source)
  let max_width t = t.max_width
  let row t i = describe t (clamp (row_count t - 1) i)

  let row_start t i =
    row t i;
    t.at_first

  let row_end t i =
    row t i;
    t.at_last

  let row_width t i =
    row t i;
    t.at_width

  let logical_line t i =
    row t i;
    t.at_line

  let continuation t i =
    row t i;
    t.at_continuation

  (* The width of the clusters the described row shows before its ellipsis or
     its end. A row has an ellipsis iff it is wider than that. *)
  let prefix t = p t t.at_visible - p t t.at_first

  (* [search t lo hi v ~upper] is [lower_bound] (or [upper_bound]) of [v]
     among the cells before the loaded line's clusters [lo] to [hi - 1], in
     clusters of the source. *)
  let search t lo hi v ~upper =
    let base = t.at_base in
    if t.at_cells < 0 then min hi (max lo (base + if upper then v + 1 else v))
    else
      let c = t.at_cells - base in
      (if upper then upper_bound else lower_bound)
        t.at_index (c + lo) (c + hi) v
      - c

  let iter_rows t ~first ~count f =
    let first = clamp (row_count t) first in
    let count = max 0 (min count (row_count t - first)) in
    if count > 0 then begin
      let l = ref (line_of_row t first) in
      for i = first to first + count - 1 do
        while t.rows.(!l + 1) <= i do
          incr l
        done;
        f ~row:i ~logical_line:!l ~continuation:(i - t.rows.(!l))
      done
    end

  let locate t { grapheme; affinity } =
    let s = t.source in
    let g = clamp (length s) grapheme in
    let l = line_of_cluster s g in
    let count = t.rows.(l + 1) - t.rows.(l) in
    (* The last row of [l] that starts at or before [g]; at the start of a
       continuation, [Upstream] is the end of the row before. *)
    let k =
      if count = 1 then 0
      else begin
        let a = rows_of t l and j = g - s.firsts.(l) in
        let lo = ref 0 and hi = ref (count - 1) in
        while !lo < !hi do
          let mid = !lo + ((!hi - !lo + 1) / 2) in
          if a.(2 * mid) <= j then lo := mid else hi := mid - 1
        done;
        if affinity = Upstream && !lo > 0 && a.(2 * !lo) = j then !lo - 1
        else !lo
      end
    in
    let i = t.rows.(l) + k in
    describe t i;
    let col =
      if g = t.at_last && g > t.at_visible then t.at_width
      else if g > t.at_visible then
        if affinity = Upstream then prefix t else t.at_width
      else p t g - p t t.at_first
    in
    (i, col)

  (* The boundaries of the described row at [g]'s column: the earliest if
     [Upstream], the latest if [Downstream], up to its last shown cluster. *)
  let snap t affinity g =
    if t.at_cells < 0 then g
    else if affinity = Upstream then
      search t t.at_first (g + 1) (p t g) ~upper:false
    else search t g (t.at_visible + 1) (p t g) ~upper:true - 1

  let hit ?(affinity = Downstream) t ~row:i ~column =
    if i < 0 then { grapheme = 0; affinity = Downstream }
    else if i >= row_count t then { grapheme = length t.source; affinity }
    else begin
      describe t i;
      let first = t.at_first and last = t.at_last in
      let visible = t.at_visible and width = t.at_width in
      let prefix = prefix t in
      let column = max 0 column in
      let g =
        if visible < last && column >= prefix then
          if column < width then snap t affinity visible
          else if width = prefix && width > 0 && affinity = Upstream then
            (* The row hangs blanks, which take no cell: the end of the shown
               clusters is the earliest boundary at the row's end. *)
            snap t affinity visible
          else last
        else
          let target = p t first + column in
          let next = search t first (visible + 1) target ~upper:false in
          snap t affinity
            (if next > visible then visible
             else if p t next = target || next = first then next
             else if target - p t (next - 1) < p t next - target then next - 1
             else next)
      in
      (* Downstream, the latest boundary at the end of the shown clusters of
         a row that hangs blanks is the row's end. *)
      let g =
        if
          affinity = Downstream && g = visible && g < last && width = prefix
          && width > 0
        then last
        else g
      in
      let rows = t.rows.(t.at_line + 1) - t.rows.(t.at_line) in
      let affinity =
        if g = last && t.at_continuation + 1 < rows then Upstream
        else if g = first then Downstream
        else affinity
      in
      { grapheme = g; affinity }
    end

  let coverage t ~row:i ~column =
    if i < 0 then (0, 0)
    else if i >= row_count t then
      let n = length t.source in
      (n, n)
    else begin
      describe t i;
      let first = t.at_first and last = t.at_last in
      let visible = t.at_visible in
      if column < 0 then (first, first)
      else if column >= t.at_width then (last, last)
      else if visible < last && column = prefix t then (visible, last)
      else
        let g =
          max first
            (search t first (visible + 1) (p t first + column) ~upper:true - 1)
        in
        (g, min last (g + 1))
    end

  (* The cells before cluster [g] of a line whose index is [index], from its
     start, and the first cluster at or after [g] that takes a cell. *)
  let[@inline] cells_at index cells base g =
    if cells < 0 then g - base else Array.unsafe_get index (cells + g - base)

  let[@inline] skip_at index skip base g =
    if skip < 0 then g else base + Array.unsafe_get index (skip + g - base)

  let iter_cells t ~row:i ~first_column ~last_column f =
    if i >= 0 && i < row_count t && last_column > first_column then begin
      describe t i;
      (* [f] may ask [t] about other rows, so the row's facts are copied. *)
      let first = t.at_first and last = t.at_last and visible = t.at_visible in
      let width = t.at_width and base = t.at_base in
      let index = t.at_index and cells = t.at_cells and skip = t.at_skip in
      let origin = cells_at index cells base first in
      let g =
        ref
          (max first
             (search t first (visible + 1)
                (origin + max 0 first_column)
                ~upper:true
             - 1))
      in
      g := skip_at index skip base !g;
      while
        !g < visible && cells_at index cells base !g - origin < last_column
      do
        let column = cells_at index cells base !g - origin in
        let w = cells_at index cells base (!g + 1) - (column + origin) in
        if column + w > first_column then
          f ~first:!g ~last:(!g + 1) ~column ~width:w ~ellipsis:false;
        g := skip_at index skip base (!g + 1)
      done;
      let column = cells_at index cells base visible - origin in
      if width > column && column >= first_column && column < last_column then
        f ~first:visible ~last ~column ~width:1 ~ellipsis:true
    end
end
