module Grid = Matrix_grid
open Windtrap

(* Helper to calculate linear index *)
let idx grid x y = (y * Grid.width grid) + x

(* Test-only exhaustive diff over the union of both grids' dimensions, sorted
   by row then column. The production diff is Screen's row-run walk. *)
let diff_cells prev curr =
  let diffs = ref [] in
  let max_w = max (Grid.width prev) (Grid.width curr) in
  let max_h = max (Grid.height prev) (Grid.height curr) in
  for y = max_h - 1 downto 0 do
    for x = max_w - 1 downto 0 do
      let in_prev = x < Grid.width prev && y < Grid.height prev in
      let in_curr = x < Grid.width curr && y < Grid.height curr in
      let differs =
        match (in_prev, in_curr) with
        | false, false -> false
        | true, false | false, true -> true
        | true, true ->
            not
              (Grid.cells_equal prev
                 ((y * Grid.width prev) + x)
                 curr
                 ((y * Grid.width curr) + x))
      in
      if differs then diffs := (x, y) :: !diffs
    done
  done;
  Array.of_list !diffs

(* Updated: Use Grid.get_text and predicates instead of Cell matching *)
let read_char grid x y =
  let i = idx grid x y in
  if Grid.is_empty grid i then 32 (* Space *)
  else if Grid.is_continuation grid i then 32 (* Treat continuation as space *)
  else
    let text = Grid.get_text grid i in
    if String.length text = 0 then 32
    else
      let decoder = String.get_utf_8_uchar text 0 in
      Uchar.to_int (Uchar.utf_decode_uchar decoder)

(* Updated: Use Grid.cell_width *)
let read_width grid x y =
  let i = idx grid x y in
  if Grid.is_empty grid i then 1 else Grid.cell_width grid i

(* Updated: Use Grid.attrs *)
let read_attr grid x y = Grid.get_attrs grid (idx grid x y)

let read_fg grid x y =
  let i = idx grid x y in
  Ansi.Color.to_rgba (Grid.get_fg grid i)

let read_bg grid x y =
  let i = idx grid x y in
  Ansi.Color.to_rgba (Grid.get_bg grid i)

let rgba = pair int (pair int (pair int int))

(* Updated: Iterate width and use predicates/get_text *)
let row_to_string grid y =
  let width = Grid.width grid in
  let buf = Buffer.create width in
  for x = 0 to width - 1 do
    let i = idx grid x y in
    if Grid.is_empty grid i then Buffer.add_char buf ' '
    else if Grid.is_continuation grid i then ()
    else Buffer.add_string buf (Grid.get_text grid i)
  done;
  Buffer.contents buf

let trim_right s =
  let len = String.length s in
  let rec loop i =
    if i < 0 then ""
    else if s.[i] = ' ' then loop (i - 1)
    else String.sub s 0 (i + 1)
  in
  if len = 0 then s else loop (len - 1)

let row_trimmed grid y = trim_right (row_to_string grid y)

(* Scroll rows [top] to [bottom] up by [n], as a terminal does. *)
let scroll grid ~top ~bottom n =
  Grid.shift grid
    { x = 0; y = top; width = Grid.width grid; height = bottom - top + 1 }
    ~dx:0 ~dy:(-n) ~color:Ansi.Color.transparent

let expect_invalid_arg msg f =
  match f () with
  | () -> fail (msg ^ ": expected Invalid_argument")
  | exception Invalid_argument _ -> ()

let assert_valid_spans grid =
  for y = 0 to Grid.height grid - 1 do
    let covered_until = ref (-1) in
    for x = 0 to Grid.width grid - 1 do
      let i = idx grid x y in
      if Grid.is_continuation grid i then
        is_true
          ~msg:(Printf.sprintf "orphan continuation at (%d,%d)" x y)
          (x <= !covered_until)
      else
        let w = Grid.cell_width grid i in
        if w > 1 then begin
          is_true
            ~msg:(Printf.sprintf "wide span at (%d,%d) fits row" x y)
            (x + w <= Grid.width grid);
          for dx = 1 to w - 1 do
            is_true
              ~msg:
                (Printf.sprintf "continuation at (%d,%d) for start (%d,%d)"
                   (x + dx) y x y)
              (Grid.is_continuation grid (idx grid (x + dx) y))
          done;
          covered_until := max !covered_until (x + w - 1)
        end
    done
  done

(* --- Tests --- *)

(* Combining marks after ASCII bases must stay in the base's cluster all the
   way through the drawing path (regression: the segmentation fast path once
   dropped them, so "e" ^ U+0301 rendered as a bare "e"). *)
let combining_mark_stays_with_ascii_base () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"e\u{0301}x";
  equal ~msg:"cell 0 holds the full cluster" string "e\u{0301}"
    (Grid.get_text grid (Grid.idx grid ~x:0 ~y:0));
  equal ~msg:"cell 1 holds the following char" string "x"
    (Grid.get_text grid (Grid.idx grid ~x:1 ~y:0));
  equal ~msg:"cluster occupies one column" int 1
    (Grid.cell_width grid (Grid.idx grid ~x:0 ~y:0))

(* Every cell of a grapheme gives its span, and the spans tile the row. *)
let span_of_every_cell () =
  let grid = Grid.create ~width:12 ~height:2 () in
  (* a, 中 (one scalar, two cells), a ZWJ sequence of two cells, b *)
  Grid.draw_text grid ~x:0 ~y:1 ~text:"a\u{4E2D}\u{1F469}\u{200D}\u{1F4BB}b";
  let spans y =
    List.init (Grid.width grid) (fun x ->
        let start, width = Grid.span grid (idx grid x y) in
        Printf.sprintf "%d+%d" (start - idx grid 0 y) width)
    |> String.concat " "
  in
  equal ~msg:"wide graphemes" string
    "0+1 1+2 1+2 3+2 3+2 5+1 6+1 7+1 8+1 9+1 10+1 11+1" (spans 1);
  equal ~msg:"a blank row" string
    "0+1 1+1 2+1 3+1 4+1 5+1 6+1 7+1 8+1 9+1 10+1 11+1" (spans 0);
  let start, width = Grid.span grid (idx grid 4 1) in
  equal ~msg:"the start cell's width" int (Grid.cell_width grid start) width;
  (* A grapheme as wide as the row. *)
  let row = Grid.create ~width:3 ~height:1 () in
  let glyph = "\u{4E2D}" in
  Grid.set_grapheme row ~x:0 ~y:0 ~width:3 ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty glyph ~pos:0
    ~len:(String.length glyph);
  equal ~msg:"three cells"
    (list (pair int int))
    [ (0, 3); (0, 3); (0, 3) ]
    (List.init 3 (Grid.span row));
  expect_invalid_arg "negative index" (fun () -> ignore (Grid.span row (-1)));
  expect_invalid_arg "past the end" (fun () -> ignore (Grid.span row 3))

let thai_spacing_mark_cell_placement () =
  List.iter
    (fun width_method ->
      let grid = Grid.create ~width:5 ~height:1 ~width_method () in
      Grid.draw_text grid ~x:0 ~y:0 ~text:"น้ำXว่Y";
      equal ~msg:"first cell retains the Thai cluster" string "น้ำ"
        (Grid.get_text grid 0);
      equal ~msg:"spacing mark makes the cluster two cells wide" int 2
        (Grid.cell_width grid 0);
      is_true ~msg:"second cell is a continuation" (Grid.is_continuation grid 1);
      equal ~msg:"ASCII starts after both occupied cells" string "X"
        (Grid.get_text grid 2);
      equal ~msg:"tone mark stays with its base" string "ว่"
        (Grid.get_text grid 3);
      equal ~msg:"tone mark cluster stays one cell wide" int 1
        (Grid.cell_width grid 3);
      equal ~msg:"next character follows the tone-mark cluster" string "Y"
        (Grid.get_text grid 4))
    [ `Unicode; `Unicode_wide ]

(* A wcwidth terminal gives SARA AM, a spacing mark of width 1, a cell of its
   own, after its base and the tone mark. *)
let thai_spacing_mark_wcwidth_units () =
  let grid = Grid.create ~width:5 ~height:1 ~width_method:`Wcwidth () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"น้ำXว่Y";
  equal ~msg:"cells" (list string)
    [ "น้"; "ำ"; "X"; "ว่"; "Y" ]
    (List.init 5 (Grid.get_text grid))

let inherit_bg_on_unwritten_ascii () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  let r, g, b, a = read_bg grid 0 0 in
  equal ~msg:"bg r" int 0 r;
  equal ~msg:"bg g" int 0 g;
  equal ~msg:"bg b" int 0 b;
  equal ~msg:"bg a" int 0 a;
  ()

let unicode_inherit_bg_on_unwritten_cell () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  let r0, g0, b0, a0 = read_bg grid 0 0 in
  equal ~msg:"bg r0" int 0 r0;
  equal ~msg:"bg g0" int 0 g0;
  equal ~msg:"bg b0" int 0 b0;
  equal ~msg:"bg a0" int 0 a0;
  let r1, g1, b1, a1 = read_bg grid 1 0 in
  equal ~msg:"bg r1" int 0 r1;
  equal ~msg:"bg g1" int 0 g1;
  equal ~msg:"bg b1" int 0 b1;
  equal ~msg:"bg a1" int 0 a1;
  ()

let overflow_respects_scissor_for_wide_grapheme () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.push_clip grid { x = 0; y = 0; width = 3; height = 1 };
  Grid.draw_text grid ~x:2 ~y:0 ~text:"中";
  Grid.pop_clip grid;
  is_true ~msg:"x=3 is continuation" (Grid.is_continuation grid (idx grid 3 0));
  assert_valid_spans grid

let alpha_blit_orphan_continuation_draws_space () =
  let src = Grid.create ~width:3 ~height:1 () in
  let style = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 0 255 0 128) () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中" ~style;
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_region ~src ~dst ~src_x:1 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"dst(0,0) is space" int (Char.code ' ') (read_char dst 0 0)

let cross_store_blit_remaps_graphemes () =
  let src = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"😊";
  let dst = Grid.create ~width:2 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:2 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"text copied" string "😊" (row_trimmed dst 0);
  let start_idx = idx dst 0 0 in
  let cont_idx = idx dst 1 0 in
  equal ~msg:"width preserved" int 2 (Grid.cell_width dst start_idx);
  is_true ~msg:"continuation copied" (Grid.is_continuation dst cont_idx)

(* Regression: draw_text used to intern graphemes before scissor and bounds
   checks, leaking one refcount-0 store slot per distinct clipped grapheme
   until the store's id space was exhausted. *)
let transparent_graphemes_do_not_leak_store () =
  let src = Grid.create ~width:4 ~height:1 () in
  let dst = Grid.create ~width:4 ~height:1 () in
  let style =
    Ansi.Style.make ~fg:Ansi.Color.transparent ~bg:Ansi.Color.transparent ()
  in
  List.iter
    (fun text ->
      Grid.draw_text ~style dst ~x:0 ~y:0 ~text;
      Grid.clear src;
      Grid.draw_text src ~x:0 ~y:0 ~text;
      Grid.push_opacity dst 0.001;
      Grid.draw_region ~src ~dst ~src_x:0 ~src_y:0 ~width:4 ~height:1 ~dst_x:0
        ~dst_y:0;
      Grid.draw_text dst ~x:0 ~y:0 ~text;
      Grid.pop_opacity dst)
    [ "👩‍🚀"; "👨‍💻"; "é" ];
  equal ~msg:"invisible graphemes never occupy storage" (pair int int) (0, 0)
    (Grid.grapheme_stats dst)

let scissored_graphemes_do_not_leak_store () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let _, slots0 = Grid.grapheme_stats grid in
  Grid.push_clip grid { x = 0; y = 0; width = 1; height = 1 };
  let buf = Buffer.create 8 in
  for i = 0 to 199 do
    Buffer.clear buf;
    (* Distinct flag emoji per iteration, fully outside the scissor *)
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i mod 26)));
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i / 26)));
    Grid.draw_text grid ~x:2 ~y:0 ~text:(Buffer.contents buf)
  done;
  Grid.pop_clip grid;
  let live, slots = Grid.grapheme_stats grid in
  equal ~msg:"no live payloads" int 0 live;
  equal ~msg:"no leaked slots" int slots0 slots

let overflowing_wide_graphemes_do_not_leak_store () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let _, slots0 = Grid.grapheme_stats grid in
  let buf = Buffer.create 8 in
  for i = 0 to 199 do
    Buffer.clear buf;
    (* Distinct flag emoji per iteration, overflowing the right edge *)
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i mod 26)));
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i / 26)));
    Grid.draw_text grid ~x:3 ~y:0 ~text:(Buffer.contents buf)
  done;
  let live, slots = Grid.grapheme_stats grid in
  equal ~msg:"no live payloads" int 0 live;
  equal ~msg:"no leaked slots" int slots0 slots

(* Grapheme store lifecycle: refcount balance and slot reuse under churn.
   Slot usage must track the set of distinct graphemes, not the number of
   frames; live count must return to zero once every reference is released. *)
let grapheme_store_churn_stays_balanced () =
  let width = 20 and height = 6 in
  let grid = Grid.create ~width ~height () in
  let rng = Random.State.make [| 0x5eed |] in
  let flag i =
    let buf = Buffer.create 8 in
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i mod 26)));
    Buffer.add_utf_8_uchar buf (Uchar.of_int (0x1F1E6 + (i / 26 mod 26)));
    Buffer.contents buf
  in
  let max_slots = ref 0 in
  for frame = 0 to 99 do
    for _ = 0 to 14 do
      let x = Random.State.int rng width in
      let y = Random.State.int rng height in
      Grid.draw_text grid ~x ~y ~text:(flag (Random.State.int rng 120))
    done;
    (match frame mod 7 with
    | 0 -> scroll grid ~top:0 ~bottom:(height - 1) 1
    | 1 -> scroll grid ~top:0 ~bottom:(height - 1) (-2)
    | 2 ->
        Grid.resize grid ~width:(width - 4) ~height;
        Grid.resize grid ~width ~height
    | 3 -> Grid.clear grid
    | _ -> ());
    let live, slots = Grid.grapheme_stats grid in
    max_slots := max !max_slots slots;
    satisfies
      ~claim:
        (Printf.sprintf "live graphemes bounded by the %d cells" (width * height))
      int
      (fun n -> n <= width * height)
      live
  done;
  Grid.clear grid;
  let live, _slots = Grid.grapheme_stats grid in
  equal ~msg:"clear releases every payload" int 0 live;
  (* At most 120 distinct flags are ever drawn, so slot usage must stay in
     that order instead of growing with the frame count. *)
  is_true
    ~msg:(Printf.sprintf "slots stay bounded (max %d)" !max_slots)
    (!max_slots <= 120)

let copied_payloads_survive_source_reuse () =
  let src = Grid.create ~width:4 ~height:1 () in
  let dst = Grid.create ~width:4 ~height:1 () in
  let flag = "🇫🇷" in
  Grid.draw_text src ~x:0 ~y:0 ~text:flag
    ~style:(Ansi.Style.make ~link:"https://example.com/flag" ());
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:2 ~height:1 ~dst_x:2
    ~dst_y:0;
  for _ = 1 to 5 do
    Grid.clear src;
    Grid.draw_text src ~x:0 ~y:0 ~text:"🇩🇪"
  done;
  equal ~msg:"copied grapheme owns its payload" string flag
    (Grid.get_text dst 2);
  equal ~msg:"copied hyperlink belongs to destination" string
    "https://example.com/flag" (Grid.get_link dst 2);
  assert_valid_spans dst

(* Clearing one grid of a sharing group must not release payloads still
   referenced by a sibling. *)
let shared_store_clear_keeps_sibling_payloads () =
  let fr =
    "\xF0\x9F\x87\xAB\xF0\x9F\x87\xB7"
    (* FR flag *)
  in
  let a = Grid.create ~width:4 ~height:1 () in
  let b = Grid.create_like a ~width:4 ~height:1 in
  Grid.draw_text a ~x:0 ~y:0 ~text:fr;
  Grid.draw_text b ~x:0 ~y:0 ~text:fr;
  let live, _ = Grid.grapheme_stats a in
  equal ~msg:"one shared payload" int 1 live;
  Grid.clear b;
  equal ~msg:"payload survives sibling clear" string fr (Grid.get_text a 0);
  let live, _ = Grid.grapheme_stats a in
  equal ~msg:"payload still live" int 1 live;
  Grid.clear a;
  let live, _ = Grid.grapheme_stats a in
  equal ~msg:"all payloads released" int 0 live

let[@inline never] populate_hyperlink_churn grid =
  let urls = Weak.create 64 in
  for i = 0 to 63 do
    let url =
      Printf.sprintf "https://example.com/churn/%d/%s" i (String.make 128 'x')
    in
    Weak.set urls i (Some url);
    Grid.draw_text
      ~style:(Ansi.Style.make ~link:url ())
      grid ~x:0 ~y:0 ~text:"link"
  done;
  urls

let overwritten_hyperlinks_are_collected () =
  let grid = Grid.create ~width:8 ~height:1 () in
  let urls = populate_hyperlink_churn grid in
  let copy = Grid.copy grid in
  Grid.clear grid;
  Gc.full_major ();
  Gc.full_major ();
  for i = 0 to Weak.length urls - 2 do
    is_false ~msg:"overwritten URLs are not retained by a live grid"
      (Weak.check urls i)
  done;
  is_true ~msg:"copied cells keep their URL alive" (Weak.check urls 63);
  Grid.clear copy;
  Gc.full_major ();
  Gc.full_major ();
  is_false ~msg:"clearing the last copy releases its URL" (Weak.check urls 63);
  equal ~msg:"source remains usable after collection" string ""
    (Grid.get_link grid 0);
  equal ~msg:"copy remains usable after collection" string ""
    (Grid.get_link copy 0)

(* A resize recounts the cells' graphemes in the grid's own tracker, so the
   store's counts stay exact with no GC, and no tracker is left for the GC to
   finalise. *)
let resized_grid_keeps_exact_grapheme_counts () =
  let grid = Grid.create ~width:8 ~height:2 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"🇫🇷👩‍🚀";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"🇫🇷";
  for i = 1 to 8 do
    Grid.resize grid ~width:(8 + (i mod 2)) ~height:(2 + (i mod 2));
    equal ~msg:"both graphemes still counted" int 2
      (fst (Grid.grapheme_stats grid))
  done;
  Grid.resize grid ~width:3 ~height:1;
  equal ~msg:"the cut grapheme is given back" int 1
    (fst (Grid.grapheme_stats grid));
  Grid.clear grid;
  equal ~msg:"clear gives back the rest" int 0 (fst (Grid.grapheme_stats grid))

let[@inline never] abandon_shared_graphemes owner =
  let grid = Grid.create_like owner ~width:4 ~height:1 in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"🇫🇷👩‍🚀";
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some grid);
  weak

let collected_grid_releases_shared_graphemes () =
  let owner = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text owner ~x:0 ~y:0 ~text:"🇫🇷";
  for _ = 1 to 16 do
    let abandoned = abandon_shared_graphemes owner in
    Gc.full_major ();
    Gc.full_major ();
    is_false ~msg:"sibling grid is unreachable" (Weak.check abandoned 0);
    equal ~msg:"collected sibling releases its payload and slots are reused"
      (pair int int) (1, 2)
      (Grid.grapheme_stats owner)
  done;
  equal ~msg:"shared surviving payload remains valid" string "🇫🇷"
    (Grid.get_text owner 0);
  Grid.clear owner;
  equal ~msg:"last grid clear releases the shared payload" int 0
    (fst (Grid.grapheme_stats owner))

let shared_storage_cells_equal () =
  let fr =
    "\xF0\x9F\x87\xAB\xF0\x9F\x87\xB7"
    (* FR flag *)
  in
  let de =
    "\xF0\x9F\x87\xA9\xF0\x9F\x87\xAA"
    (* DE flag *)
  in
  let a = Grid.create ~width:2 ~height:1 () in
  let b = Grid.create_like a ~width:2 ~height:1 in
  Grid.draw_text a ~x:0 ~y:0 ~text:fr;
  Grid.draw_text b ~x:0 ~y:0 ~text:de;
  is_false ~msg:"different graphemes differ" (Grid.cells_equal a 0 b 0);
  Grid.clear b;
  Grid.draw_text b ~x:0 ~y:0 ~text:fr;
  is_true ~msg:"identical graphemes equal" (Grid.cells_equal a 0 b 0)

let shared_storage_links_equal () =
  let a = Grid.create ~width:4 ~height:1 () in
  let b = Grid.create_like a ~width:4 ~height:1 in
  let draw grid url =
    Grid.draw_text grid ~x:0 ~y:0 ~text:"x"
      ~style:(Ansi.Style.make ~link:url ())
  in
  draw a "http://a";
  draw b "http://b";
  is_false ~msg:"different links differ" (Grid.cells_equal a 0 b 0);
  Grid.clear b;
  draw b "http://a";
  is_true ~msg:"identical links equal" (Grid.cells_equal a 0 b 0)

let independent_storage_cells_equal () =
  let a = Grid.create ~width:4 ~height:1 () in
  let b = Grid.create ~width:4 ~height:1 () in
  let draw grid text url =
    Grid.draw_text grid ~x:0 ~y:0 ~text ~style:(Ansi.Style.make ~link:url ())
  in
  draw a "🇫🇷" "https://a";
  draw b "🇩🇪" "https://b";
  is_false ~msg:"colliding storage IDs are not equal" (Grid.cells_equal a 0 b 0);
  Grid.clear b;
  draw b "🇫🇷" "https://a";
  for i = 0 to 1 do
    is_true ~msg:"text and links compare across independent stores"
      (Grid.cells_equal a i b i)
  done;
  Grid.clear b;
  draw b "🇫🇷" "https://b";
  is_false ~msg:"equal text with different URL differs"
    (Grid.cells_equal a 0 b 0)

let scalar_writes_preserve_spans () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let draw x uchar =
    Grid.set_uchar grid ~x ~y:0 ~uchar ~fg:Ansi.Color.white ~bg:Ansi.Color.black
      ~attrs:Ansi.Attr.empty ()
  in
  Grid.clip grid
    Grid.{ x = 1; y = 0; width = 1; height = 1 }
    (fun () -> draw 1 (Uchar.of_int 0x4E2D));
  equal ~msg:"wide scalar start" string "中" (Grid.get_text grid 1);
  is_true ~msg:"continuation extends beyond clip" (Grid.is_continuation grid 2);
  assert_valid_spans grid;
  draw 2 (Uchar.of_char 'x');
  equal ~msg:"overwriting continuation clears its start" string " "
    (Grid.get_text grid 1);
  draw 3 (Uchar.of_int 0x4E2D);
  equal ~msg:"right-edge overflow becomes space" string " "
    (Grid.get_text grid 3);
  List.iter
    (fun cp ->
      draw 0 (Uchar.of_char 'a');
      draw 1 (Uchar.of_char 'b');
      draw 1 (Uchar.of_int cp);
      is_true ~msg:"control or zero-width scalar empties its target"
        (Grid.is_empty grid 1);
      equal ~msg:"scalar does not extend preceding text" string "a"
        (Grid.get_text grid 0);
      equal ~msg:"empty scalar has no text" string "" (Grid.get_text grid 1))
    [ 0; 9; 10; 27; 127; 0x0301 ];
  assert_valid_spans grid

(* A grapheme [width] cells wide at [(x, 0)], as a terminal places text it
   is told the size of (OSC 66): no text measures wider than two cells. *)
let span grid ~x width =
  Grid.set_grapheme grid ~x ~y:0 ~width ~fg:Ansi.Color.default_fg
    ~bg:Ansi.Color.default_bg ~attrs:Ansi.Attr.empty "w" ~pos:0 ~len:1

let clipped_multi_column_grapheme () =
  let src = Grid.create ~width:6 ~height:1 () in
  span src ~x:0 4;
  equal ~msg:"fixture spans four columns" int 4 (Grid.cell_width src 0);
  List.iter
    (fun transfer ->
      let dst = Grid.create ~width:3 ~height:1 () in
      transfer ~src ~dst ~src_x:0 ~src_y:0 ~width:3 ~height:1 ~dst_x:0 ~dst_y:0;
      assert_valid_spans dst;
      equal ~msg:"entire clipped grapheme becomes spaces" string "   "
        (Grid.to_text ~trim:false dst))
    [ Grid.blit_region; Grid.draw_region ];
  Grid.draw_text src ~x:4 ~y:0 ~text:"x";
  let dst = Grid.create ~width:2 ~height:1 () in
  Grid.push_opacity dst 0.;
  Grid.blit_region ~src ~dst ~src_x:4 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  Grid.draw_region ~src ~dst ~src_x:4 ~src_y:0 ~width:1 ~height:1 ~dst_x:1
    ~dst_y:0;
  Grid.pop_opacity dst;
  equal ~msg:"copy ignores drawing opacity, composition observes it" string "x "
    (Grid.to_text ~trim:false dst)

let overlapping_wide_spans () =
  let grid = Grid.create ~width:8 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"a中🇫🇷bc";
  Grid.blit_region ~src:grid ~dst:grid ~src_x:0 ~src_y:0 ~width:7 ~height:1
    ~dst_x:1 ~dst_y:0;
  assert_valid_spans grid;
  equal ~msg:"right shift preserves complete graphemes" string "aa中🇫🇷bc"
    (Grid.to_text grid);
  Grid.blit_region ~src:grid ~dst:grid ~src_x:1 ~src_y:0 ~width:7 ~height:1
    ~dst_x:0 ~dst_y:0;
  assert_valid_spans grid;
  equal ~msg:"left shift preserves complete graphemes" string "a中🇫🇷bcc"
    (Grid.to_text grid)

let blit_bulk_tracks_graphemes () =
  let src = Grid.create ~width:4 ~height:1 () in
  let dst = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"a😊a";
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:4 ~height:1 ~dst_x:0
    ~dst_y:0;
  Grid.clear src;
  (* Updated: use Grid.get_text directly *)
  let text = Grid.get_text dst (idx dst 1 0) in
  equal ~msg:"emoji text" string "😊" text

let overlap_blit_direction_correctness () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ABCDE";
  Grid.blit_region ~src:grid ~dst:grid ~src_x:0 ~src_y:0 ~width:4 ~height:1
    ~dst_x:1 ~dst_y:0;
  equal ~msg:"overlap result" string "AABCD" (row_trimmed grid 0)

let box_title_left_aligned () =
  let grid = Grid.create ~width:12 ~height:3 () in
  let u = Uchar.of_int in
  let border_chars : Grid.Border.t =
    {
      top_left = u 0x250C;
      (* ┌ *)
      top_right = u 0x2510;
      (* ┐ *)
      bottom_left = u 0x2514;
      (* └ *)
      bottom_right = u 0x2518;
      (* ┘ *)
      horizontal = u 0x2500;
      (* ─ *)
      vertical = u 0x2502;
      (* │ *)
      top_t = u 0;
      bottom_t = u 0;
      left_t = u 0;
      right_t = u 0;
      cross = u 0;
    }
  in
  let style = Ansi.Style.default in
  Grid.draw_box grid ~x:0 ~y:0 ~width:12 ~height:3 ~border:border_chars
    ~sides:[ `Top; `Left; `Right ] ~style ~title:"Title" ();
  equal ~msg:"T at x=2" int (Char.code 'T') (read_char grid 2 0)

let diff_detects_single_rgb_step () =
  let a = Grid.create ~width:1 ~height:1 () in
  let b = Grid.copy a in
  (* Cell comparison is exact on the packed color representation, so a single
     1/255 RGB step must register as a difference. *)
  let minimal_diff_color = Ansi.Color.of_rgba 1 1 1 1 in
  Grid.set_uchar b ~x:0 ~y:0 ~uchar:(Uchar.of_char ' ') ~fg:Ansi.Color.white
    ~bg:minimal_diff_color ~attrs:Ansi.Attr.empty ();
  let diffs = diff_cells a b in
  equal ~msg:"diffs include cell when RGB changes by 1 step"
    (list (pair int int))
    [ (0, 0) ]
    (Array.to_list diffs)

let alpha_blit_blends_fg_bg () =
  let src = Grid.create ~width:1 ~height:1 () in
  let dst = Grid.create ~width:1 ~height:1 () in
  let blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:blue;
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  let style = Ansi.Style.make ~fg:semi_red ~bg:semi_green () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"C" ~style;
  Grid.draw_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r_bg, g_bg, b_bg, a_bg = read_bg dst 0 0 in
  (* Source-over increases green and retains some destination blue. *)
  satisfies ~claim:"bg green increased (> 0)" int (fun g -> g > 0) g_bg;
  satisfies ~claim:"bg blue decreased (< 255)" int (fun b -> b < 255) b_bg;
  equal ~msg:"bg red stays 0" int 0 r_bg;
  equal ~msg:"bg alpha remains opaque" int 255 a_bg;
  let r_fg, _g_fg, _b_fg, a_fg = read_fg dst 0 0 in
  (* Foreground remains visible after composition onto an opaque grid. *)
  satisfies ~claim:"foreground retains red" int (fun r -> r > 0) r_fg;
  equal ~msg:"fg alpha 255" int 255 a_fg

let resize_shrink_clips_continuation () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.resize grid ~width:1 ~height:1;
  equal ~msg:"row after shrink clears truncated grapheme" string ""
    (row_trimmed grid 0);
  ignore (read_char grid 0 0)

let resize_shrink_clips_stored_grapheme () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"👩‍🚀";
  Grid.resize grid ~width:1 ~height:1;
  assert_valid_spans grid;
  equal ~msg:"row after shrink clears truncated complex grapheme" string ""
    (row_trimmed grid 0);
  Grid.resize grid ~width:2 ~height:1;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"👨‍🚀";
  assert_valid_spans grid;
  equal ~msg:"stored grapheme still usable after shrink" string "👨‍🚀"
    (row_trimmed grid 0)

(* A grapheme held only by the grid's store keeps its text through a resize
   that keeps its cells, growing or shrinking either dimension. *)
let resize_keeps_stored_grapheme () =
  List.iter
    (fun (width, height) ->
      let grid = Grid.create ~width:2 ~height:1 () in
      Grid.draw_text grid ~x:0 ~y:0 ~text:"👩‍🚀";
      Grid.resize grid ~width ~height;
      assert_valid_spans grid;
      equal
        ~msg:(Printf.sprintf "text after resize to %dx%d" width height)
        string "👩‍🚀" (Grid.get_text grid 0);
      equal ~msg:"width" int 2 (Grid.cell_width grid 0))
    [ (49, 16); (2, 2); (3, 1); (2, 16) ]

let resize_truncated_grapheme_does_not_bleed () =
  let grid = Grid.create ~width:5 ~height:2 () in
  Grid.draw_text grid ~x:3 ~y:0 ~text:"中";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"B";
  Grid.resize grid ~width:4 ~height:2;
  Grid.draw_text grid ~x:3 ~y:0 ~text:"X";
  equal ~msg:"bottom row preserved" string "B" (row_trimmed grid 1)

let create_defaults () =
  let grid = Grid.create ~width:2 ~height:3 () in
  equal ~msg:"width" int 2 (Grid.width grid);
  equal ~msg:"height" int 3 (Grid.height grid);
  is_true ~msg:"width method" (Grid.width_method grid = `Unicode)

let create_with_configuration () =
  let grid = Grid.create ~width:1 ~height:1 ~width_method:`Wcwidth () in
  is_true ~msg:"width method" (Grid.width_method grid = `Wcwidth)

let set_width_method_updates () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_width_method grid `Wcwidth;
  is_true ~msg:"updated" (Grid.width_method grid = `Wcwidth)

let set_uchar_writes_all_planes () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let attrs = Ansi.Attr.bold in
  Grid.set_uchar ~blend:true grid ~x:1 ~y:0 ~uchar:(Uchar.of_char 'A')
    ~fg:Ansi.Color.red ~bg:Ansi.Color.blue ~attrs ();
  equal ~msg:"char" int (Char.code 'A') (read_char grid 1 0);
  equal ~msg:"width" int 1 (read_width grid 1 0);
  equal ~msg:"attrs" int (Ansi.Attr.pack attrs) (read_attr grid 1 0);
  let r_fg, g_fg, b_fg, a_fg = read_fg grid 1 0 in
  let er, eg, eb, ea = Ansi.Color.to_rgba Ansi.Color.red in
  equal ~msg:"fg color" rgba (er, (eg, (eb, ea))) (r_fg, (g_fg, (b_fg, a_fg)));
  let r_bg, g_bg, b_bg, a_bg = read_bg grid 1 0 in
  let br, bg, bb, ba = Ansi.Color.to_rgba Ansi.Color.blue in
  equal ~msg:"bg color" rgba (br, (bg, (bb, ba))) (r_bg, (g_bg, (b_bg, a_bg)))

let set_uchar_outside_scissor_ignored () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.push_clip grid { x = 1; y = 1; width = 1; height = 1 };
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'X')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  Grid.pop_clip grid;
  equal ~msg:"char remains empty" int 32 (read_char grid 0 0)

let with_scissor_restores_stack () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let result =
    Grid.clip grid { x = 0; y = 0; width = 1; height = 1 } (fun () ->
        Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'Y')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        42)
  in
  equal ~msg:"scoped result" int 42 result;
  equal ~msg:"inside write" int (Char.code 'Y') (read_char grid 0 0);
  (* After scope, writing outside should succeed. *)
  Grid.set_uchar ~blend:true grid ~x:1 ~y:1 ~uchar:(Uchar.of_char 'Z')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  equal ~msg:"char set" int (Char.code 'Z') (read_char grid 1 1)

let set_uchar_records_hyperlink () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'L')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty
    ~link:"https://example.com" ();
  equal ~msg:"link stored" string "https://example.com" (Grid.get_link grid 0)

let draw_text_applies_style () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let fg_color = Ansi.Color.of_rgb 50 100 150 in
  let bg_color = Ansi.Color.of_rgba 10 20 30 200 in
  let style = Ansi.Style.make ~fg:fg_color ~bg:bg_color ~bold:true () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"Hi" ~style;
  equal ~msg:"first char" int (Char.code 'H') (read_char grid 1 0);
  equal ~msg:"second char" int (Char.code 'i') (read_char grid 2 0);
  let attrs = style.Ansi.Style.attrs in
  equal ~msg:"attr first" int (Ansi.Attr.pack attrs) (read_attr grid 1 0);
  equal ~msg:"attr second" int (Ansi.Attr.pack attrs) (read_attr grid 2 0);
  let expected_fg =
    let r, g, b, a = Ansi.Color.to_rgba fg_color in
    (r, (g, (b, a)))
  in
  let expected_bg_color =
    Ansi.Color.blend ~src:bg_color ~dst:Ansi.Color.transparent
  in
  let expected_bg =
    let r, g, b, a = Ansi.Color.to_rgba expected_bg_color in
    (r, (g, (b, a)))
  in
  let r, g, b, a = read_fg grid 1 0 in
  equal ~msg:"fg first" rgba expected_fg (r, (g, (b, a)));
  let r, g, b, a = read_fg grid 2 0 in
  equal ~msg:"fg second" rgba expected_fg (r, (g, (b, a)));
  let r, g, b, a = read_bg grid 1 0 in
  equal ~msg:"bg first" rgba expected_bg (r, (g, (b, a)));
  let r, g, b, a = read_bg grid 2 0 in
  equal ~msg:"bg second" rgba expected_bg (r, (g, (b, a)))

let draw_text_inherits_existing_background () =
  let grid = Grid.create ~width:4 ~height:1 () in
  let bg_color = Ansi.Color.of_rgb 40 80 120 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:1 ~color:bg_color;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi";
  equal ~msg:"char H" int (Char.code 'H') (read_char grid 0 0);
  equal ~msg:"char i" int (Char.code 'i') (read_char grid 1 0);
  let expected =
    let r, g, b, a = Ansi.Color.to_rgba bg_color in
    (r, (g, (b, a)))
  in
  let assert_bg x =
    let r, g, b, a = read_bg grid x 0 in
    equal ~msg:(Printf.sprintf "bg cell %d" x) rgba expected (r, (g, (b, a)))
  in
  assert_bg 0;
  assert_bg 1;
  assert_bg 2;
  assert_bg 3

let draw_text_skips_newline () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A\nB";
  equal ~msg:"A at x=0" int (Char.code 'A') (read_char grid 0 0);
  equal ~msg:"B at x=1" int (Char.code 'B') (read_char grid 1 0);
  equal ~msg:"x=2 space" int (Char.code ' ') (read_char grid 2 0)

let draw_box_left_border_spans_full_height () =
  let grid = Grid.create ~width:3 ~height:4 () in
  let u = Uchar.of_int in
  let border_chars : Grid.Border.t =
    {
      top_left = u 0;
      top_right = u 0;
      bottom_left = u 0;
      bottom_right = u 0;
      horizontal = u 0;
      vertical = u 0x2502;
      top_t = u 0;
      bottom_t = u 0;
      left_t = u 0;
      right_t = u 0;
      cross = u 0;
    }
  in
  Grid.draw_box grid ~x:0 ~y:0 ~width:3 ~height:4 ~border:border_chars
    ~sides:[ `Left ]
    ~fill:(Ansi.Color.of_rgba 0 0 0 0)
    ();
  let pipe = 0x2502 in
  equal ~msg:"top cell" int pipe (read_char grid 0 0);
  equal ~msg:"middle cell" int pipe (read_char grid 0 1);
  equal ~msg:"bottom cell" int pipe (read_char grid 0 3)

let set_uchar_honours_blending () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'B')
    ~fg:Ansi.Color.blue ~bg:Ansi.Color.blue ~attrs:Ansi.Attr.empty ();
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'C')
    ~fg:semi_red ~bg:semi_green ~attrs:Ansi.Attr.empty ();
  let expected_bg = Ansi.Color.of_rgb 0 128 64 in
  let expected_fg = Ansi.Color.of_rgb 128 64 32 in
  equal ~msg:"char" int (Char.code 'C') (read_char grid 0 0);
  equal ~msg:"width" int 1 (read_width grid 0 0);
  let expected_fg =
    let r, g, b, a = Ansi.Color.to_rgba expected_fg in
    (r, (g, (b, a)))
  in
  let expected_bg =
    let r, g, b, a = Ansi.Color.to_rgba expected_bg in
    (r, (g, (b, a)))
  in
  let actual_fg =
    let r, g, b, a = read_fg grid 0 0 in
    (r, (g, (b, a)))
  in
  let actual_bg =
    let r, g, b, a = read_bg grid 0 0 in
    (r, (g, (b, a)))
  in
  equal ~msg:"fg blended" rgba expected_fg actual_fg;
  equal ~msg:"bg blended" rgba expected_bg actual_bg

let set_uchar_accumulates_alpha () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'C')
    ~fg:semi_red ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  equal ~msg:"char" int (Char.code 'C') (read_char grid 0 0);
  equal ~msg:"width" int 1 (read_width grid 0 0);
  (* Red foreground over the same translucent red background accumulates
     coverage without darkening either layer. *)
  let expected = (255, (0, (0, 192))) in
  let actual =
    let r, g, b, a = read_fg grid 0 0 in
    (r, (g, (b, a)))
  in
  equal ~msg:"fg blended" rgba expected actual

let draw_text_blends_fg_alpha_over_opaque_bg () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let style = Ansi.Style.make ~fg:semi_red () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
  let r, _g, b, a = read_fg grid 0 0 in
  (* Blended path sets fg alpha to destination bg alpha and retains some blue *)
  equal ~msg:"fg alpha promoted" int 255 a;
  satisfies ~claim:"blue component preserved (> 0)" int (fun blue -> blue > 0) b;
  satisfies ~claim:"red component applied (> 0)" int (fun red -> red > 0) r

(* A grapheme has one rendition: a terminal shows a wide character in one. *)
let same_rendition ~msg g a b =
  equal ~msg:(msg ^ ": bg") rgba
    (let r, g', b', a' = Ansi.Color.to_rgba (Grid.get_bg g a) in
     (r, (g', (b', a'))))
    (let r, g', b', a' = Ansi.Color.to_rgba (Grid.get_bg g b) in
     (r, (g', (b', a'))));
  equal ~msg:(msg ^ ": fg") rgba
    (let r, g', b', a' = Ansi.Color.to_rgba (Grid.get_fg g a) in
     (r, (g', (b', a'))))
    (let r, g', b', a' = Ansi.Color.to_rgba (Grid.get_fg g b) in
     (r, (g', (b', a'))));
  equal ~msg:(msg ^ ": attrs") int (Grid.get_attrs g a) (Grid.get_attrs g b);
  equal ~msg:(msg ^ ": link") string (Grid.get_link g a) (Grid.get_link g b)

let translucent_wide_character_has_one_background () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1
    ~color:(Ansi.Color.of_rgb 0 0 255);
  Grid.fill_rect grid ~x:1 ~y:0 ~width:1 ~height:1
    ~color:(Ansi.Color.of_rgb 255 255 0);
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"\u{4E2D}"
    ~style:(Ansi.Style.make ~bg:semi_red ());
  is_true ~msg:"the character spans two cells" (Grid.is_continuation grid 1);
  same_rendition ~msg:"drawn over two backgrounds" grid 0 1;
  equal ~msg:"blended over the start cell's background" rgba
    (let r, g, b, a =
       Ansi.Color.to_rgba
         (Ansi.Color.blend ~src:semi_red ~dst:(Ansi.Color.of_rgb 0 0 255))
     in
     (r, (g, (b, a))))
    (let r, g, b, a = Ansi.Color.to_rgba (Grid.get_bg grid 1) in
     (r, (g, (b, a))))

let translucent_fill_tints_wide_character () =
  let tinted ~x ~width =
    let grid = Grid.create ~width:4 ~height:1 () in
    Grid.draw_text grid ~x:0 ~y:0 ~text:"\u{4E2D}"
      ~style:
        (Ansi.Style.make ~fg:Ansi.Color.white
           ~bg:(Ansi.Color.of_rgb 0 0 255)
           ());
    let before = Grid.get_bg grid 0 in
    Grid.fill_rect grid ~x ~y:0 ~width ~height:1
      ~color:(Ansi.Color.of_rgba 255 0 0 128);
    (grid, before)
  in
  List.iter
    (fun (x, width, label, changed) ->
      let grid, before = tinted ~x ~width in
      equal
        ~msg:(label ^ ": the character stays")
        string "\u{4E2D}" (Grid.get_text grid 0);
      is_true ~msg:(label ^ ": whole") (Grid.is_continuation grid 1);
      same_rendition ~msg:label grid 0 1;
      equal
        ~msg:(label ^ ": tinted once by its start cell")
        bool changed
        (not (Ansi.Color.equal before (Grid.get_bg grid 0))))
    [
      (0, 2, "both cells covered", true);
      (0, 1, "the start cell covered", true);
      (1, 1, "the second cell covered", false);
    ];
  let grid, before = tinted ~x:0 ~width:2 in
  equal ~msg:"blended once" rgba
    (let r, g, b, a =
       Ansi.Color.to_rgba
         (Ansi.Color.blend ~src:(Ansi.Color.of_rgba 255 0 0 128) ~dst:before)
     in
     (r, (g, (b, a))))
    (let r, g, b, a = Ansi.Color.to_rgba (Grid.get_bg grid 1) in
     (r, (g, (b, a))))

let blit_region_copies_alpha () =
  let src = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_uchar src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'R') ~fg:Ansi.Color.white
    ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r, _g, b, a = read_bg dst 0 0 in
  equal ~msg:"char copied" int (Char.code 'R') (read_char dst 0 0);
  equal ~msg:"source red copied" int 255 r;
  equal ~msg:"destination blue overwritten" int 0 b;
  equal ~msg:"source alpha copied" int 128 a

let draw_region_blends_alpha () =
  let src = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_uchar ~blend:false src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'R')
    ~fg:Ansi.Color.white ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  Grid.draw_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r, _g, b, a = read_bg dst 0 0 in
  equal ~msg:"char copied" int (Char.code 'R') (read_char dst 0 0);
  is_true ~msg:"background blended keeps blue" (b > 0 && b < 255);
  is_true ~msg:"background blended adds red" (r > 0 && r < 255);
  equal ~msg:"opaque destination stays opaque" int 255 a

let scissor_push_intersects_parent () =
  let grid = Grid.create ~width:4 ~height:1 () in
  (* Parent scissor clips to first cell *)
  Grid.push_clip grid { x = 0; y = 0; width = 1; height = 1 };
  (* Child scissor outside parent should intersect to empty, so writes are
     clipped *)
  Grid.push_clip grid { x = 2; y = 0; width = 1; height = 1 };
  Grid.set_uchar ~blend:true grid ~x:2 ~y:0 ~uchar:(Uchar.of_char 'B')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  (* Pop child, write inside parent *)
  Grid.pop_clip grid;
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'A')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  Grid.pop_clip grid;
  equal ~msg:"child write clipped by parent" int (Char.code ' ')
    (read_char grid 2 0);
  equal ~msg:"parent restored after pop" int (Char.code 'A')
    (read_char grid 0 0)

let clear_scissor_allows_future_writes () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.push_clip grid { x = 0; y = 0; width = 1; height = 1 };
  Grid.clear_clip grid;
  Grid.set_uchar ~blend:true grid ~x:1 ~y:1 ~uchar:(Uchar.of_char 'W')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  equal ~msg:"write succeeded" int (Char.code 'W') (read_char grid 1 1)

let intersects_clip_includes_grid_bounds () =
  let grid = Grid.create ~width:4 ~height:3 () in
  is_true ~msg:"inside grid"
    (Grid.intersects_clip grid { x = 3; y = 2; width = 2; height = 2 });
  is_false ~msg:"right of grid"
    (Grid.intersects_clip grid { x = 4; y = 0; width = 1; height = 1 });
  is_false ~msg:"empty region"
    (Grid.intersects_clip grid { x = 0; y = 0; width = 0; height = 1 })

let intersects_clip_observes_nested_scissors () =
  let grid = Grid.create ~width:6 ~height:2 () in
  Grid.push_clip grid { x = 1; y = 0; width = 4; height = 2 };
  Grid.push_clip grid { x = 3; y = 0; width = 2; height = 1 };
  is_true ~msg:"inside effective clip"
    (Grid.intersects_clip grid { x = 4; y = 0; width = 2; height = 2 });
  is_false ~msg:"outside effective clip"
    (Grid.intersects_clip grid { x = 1; y = 0; width = 2; height = 2 });
  Grid.pop_clip grid;
  is_true ~msg:"parent clip restored"
    (Grid.intersects_clip grid { x = 1; y = 0; width = 2; height = 2 })

let fill_rect_fills_region () =
  let grid = Grid.create ~width:3 ~height:3 () in
  Grid.fill_rect grid ~x:1 ~y:1 ~width:2 ~height:2 ~color:Ansi.Color.green;
  for y = 0 to 2 do
    for x = 0 to 2 do
      let inside = x >= 1 && x <= 2 && y >= 1 && y <= 2 in
      if inside then (
        equal ~msg:"char" int (Char.code ' ') (read_char grid x y);
        equal ~msg:"width" int 1 (read_width grid x y);
        let r, g, b, _ = read_bg grid x y in
        let er, eg, eb, _ = Ansi.Color.to_rgba Ansi.Color.green in
        equal ~msg:"color" (triple int int int) (er, eg, eb) (r, g, b))
      else equal ~msg:"outside char" int 32 (read_char grid x y)
    done
  done

let replace_wide_grapheme_clears_continuations () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"😊";
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'A')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  equal ~msg:"continuation cleared to space" int 32 (read_char grid 1 0);
  equal ~msg:"continuation width reset" int 1 (read_width grid 1 0)

let replace_wide_grapheme_clears_continuation_colors () =
  let grid = Grid.create ~width:3 ~height:1 () in
  let style =
    Ansi.Style.make
      ~bg:(Ansi.Color.of_rgb 120 40 10)
      ~fg:(Ansi.Color.of_rgb 10 200 240)
      ()
  in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"😊" ~style;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  equal ~msg:"continuation cleared to space" int 32 (read_char grid 1 0);
  (* "A" on a transparent background keeps the background below it; the
     blank it leaves takes that written background. *)
  equal ~msg:"the written cell keeps the bg below" rgba
    (120, (40, (10, 255)))
    (let r, g, b, a = read_bg grid 0 0 in
     (r, (g, (b, a))));
  equal ~msg:"continuation bg is the written bg" rgba
    (120, (40, (10, 255)))
    (let r, g, b, a = read_bg grid 1 0 in
     (r, (g, (b, a))));
  equal ~msg:"continuation fg reset" rgba
    (255, (255, (255, 255)))
    (let r, g, b, a = read_fg grid 1 0 in
     (r, (g, (b, a))))

let fill_rect_alpha_preserves_glyph () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'X')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let overlay = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:overlay;
  equal ~msg:"char preserved" int (Char.code 'X') (read_char grid 0 0);
  (* Background and foreground retain opaque coverage. *)
  let blended_bg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.black in
  let er, eg, eb, ea = Ansi.Color.to_rgba blended_bg in
  let r, g, b, a = read_bg grid 0 0 in
  equal ~msg:"background blended" rgba (er, (eg, (eb, ea))) (r, (g, (b, a)));
  let expected_fg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.white in
  let fr, fg, fb, fa = Ansi.Color.to_rgba expected_fg in
  let r, g, b, a = read_fg grid 0 0 in
  equal ~msg:"foreground tinted" rgba (fr, (fg, (fb, fa))) (r, (g, (b, a)))

let fill_rect_transparent_is_noop () =
  let grid = Grid.create ~width:3 ~height:1 () in
  let bg_color = Ansi.Color.of_rgb 10 20 30 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:3 ~height:1 ~color:bg_color;
  Grid.draw_text grid ~x:1 ~y:0 ~text:"X"
    ~style:
      (Ansi.Style.make
         ~fg:(Ansi.Color.of_rgb 200 210 220)
         ~bg:(Ansi.Color.of_rgb 40 50 60)
         ~bold:true ());
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:3 ~height:1 ~color:transparent;
  equal ~msg:"left char preserved" int (Char.code ' ') (read_char grid 0 0);
  equal ~msg:"text preserved" int (Char.code 'X') (read_char grid 1 0);
  equal ~msg:"attrs preserved" int
    (Ansi.Attr.pack Ansi.Attr.bold)
    (read_attr grid 1 0);
  equal ~msg:"right char preserved" int (Char.code ' ') (read_char grid 2 0);
  let expected_bg x =
    let color = if x = 1 then Ansi.Color.of_rgb 40 50 60 else bg_color in
    let r, g, b, a = Ansi.Color.to_rgba color in
    (r, (g, (b, a)))
  in
  for x = 0 to 2 do
    let r, g, b, a = read_bg grid x 0 in
    equal
      ~msg:(Printf.sprintf "bg unchanged %d" x)
      rgba (expected_bg x)
      (r, (g, (b, a)))
  done

let clear_rect_resets_background () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC"
    ~style:
      (Ansi.Style.make
         ~fg:(Ansi.Color.of_rgb 200 210 220)
         ~bg:(Ansi.Color.of_rgb 10 20 30)
         ~underline:true ());
  Grid.clear_rect grid ~x:1 ~y:0 ~width:1 ~height:1;
  equal ~msg:"left preserved" int (Char.code 'A') (read_char grid 0 0);
  equal ~msg:"cleared char" int (Char.code ' ') (read_char grid 1 0);
  equal ~msg:"right preserved" int (Char.code 'C') (read_char grid 2 0);
  equal ~msg:"cleared attrs" int 0 (read_attr grid 1 0);
  equal ~msg:"cleared fg" rgba
    (255, (255, (255, 255)))
    (let r, g, b, a = read_fg grid 1 0 in
     (r, (g, (b, a))));
  equal ~msg:"cleared bg" rgba
    (0, (0, (0, 0)))
    (let r, g, b, a = read_bg grid 1 0 in
     (r, (g, (b, a))))

let scroll_uses_transparent_background () =
  let grid = Grid.create ~width:2 ~height:2 () in
  (* Write something on the first row to force a scroll source. *)
  Grid.draw_text grid ~x:0 ~y:0 ~text:"AA";
  scroll grid ~top:0 ~bottom:1 1;
  (* Bottom row is newly cleared; its background should stay transparent. *)
  let r, g, b, a = read_bg grid 0 1 in
  equal ~msg:"transparent bg" rgba (0, (0, (0, 0))) (r, (g, (b, a)));
  equal ~msg:"space char" int (Char.code ' ') (read_char grid 0 1)

let scroll_single_row_clears_only_that_row () =
  List.iter
    (fun amount ->
      let grid = Grid.create ~width:4 ~height:3 () in
      for y = 0 to 2 do
        Grid.draw_text grid ~x:0 ~y ~text:"keep"
      done;
      Grid.draw_text
        ~style:(Ansi.Style.make ~link:"https://example.com" ())
        grid ~x:0 ~y:1 ~text:"🇫🇷";
      scroll grid ~top:1 ~bottom:1 amount;
      equal ~msg:"one-row scroll clears its row in either direction" string
        "keep\n\nkeep" (Grid.to_text grid);
      equal ~msg:"scrolled row releases its hyperlink" string ""
        (Grid.get_link grid 4);
      equal ~msg:"scrolled row releases its grapheme" int 0
        (fst (Grid.grapheme_stats grid)))
    [ -1; 1 ]

let scroll_keeps_multi_codepoint_graphemes () =
  let grid = Grid.create ~width:4 ~height:3 () in
  Grid.draw_text grid ~x:0 ~y:1 ~text:"e\u{301}";
  scroll grid ~top:0 ~bottom:2 1;
  equal ~msg:"moved up" string "e\u{301}" (Grid.get_text grid (idx grid 0 0));
  scroll grid ~top:0 ~bottom:2 (-2);
  equal ~msg:"moved down" string "e\u{301}" (Grid.get_text grid (idx grid 0 2))

let draw_text_overwrite_clears_span () =
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.draw_text grid ~x:0 ~y:0 ~text:"a";
  equal ~msg:"start replaced" int (Char.code 'a') (read_char grid 0 0);
  equal ~msg:"continuation cleared" int 32 (read_char grid 1 0);
  equal ~msg:"start width" int 1 (read_width grid 0 0);
  equal ~msg:"continuation width cleared" int 1 (read_width grid 1 0)

let blit_region_skips_partial_span () =
  let src = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中a";
  let dst = Grid.create ~width:2 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:0 ~width:2 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"orphan cleared" int 32 (read_char dst 0 0);
  equal ~msg:"orphan width" int 1 (read_width dst 0 0);
  equal ~msg:"trailing char copied" int (Char.code 'a') (read_char dst 1 0);
  equal ~msg:"trailing width" int 1 (read_width dst 1 0)

let fill_rect_clears_inline_wide_start () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:Ansi.Color.red;
  assert_valid_spans grid;
  equal ~msg:"start cell reset to space" int (Char.code ' ')
    (read_char grid 0 0);
  equal ~msg:"old continuation cleared" int (Char.code ' ') (read_char grid 1 0);
  equal ~msg:"old continuation width reset" int 1 (read_width grid 1 0)

let fill_rect_clears_inline_wide_continuation () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中";
  Grid.fill_rect grid ~x:1 ~y:0 ~width:1 ~height:1 ~color:Ansi.Color.red;
  assert_valid_spans grid;
  equal ~msg:"old start cleared" int (Char.code ' ') (read_char grid 0 0);
  equal ~msg:"continuation cell reset to space" int (Char.code ' ')
    (read_char grid 1 0);
  equal ~msg:"old start width reset" int 1 (read_width grid 0 0)

let blit_region_clears_right_truncated_wide_start () =
  let src = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中a";
  let dst = Grid.create ~width:2 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  assert_valid_spans dst;
  equal ~msg:"right-truncated wide start becomes space" int (Char.code ' ')
    (read_char dst 0 0);
  equal ~msg:"width reset" int 1 (read_width dst 0 0)

let blit_region_shifts_wide_over_wide () =
  let grid = Grid.create ~width:6 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"中😀";
  Grid.blit_region ~src:grid ~dst:grid ~src_x:0 ~src_y:0 ~width:4 ~height:1
    ~dst_x:2 ~dst_y:0;
  assert_valid_spans grid;
  is_true ~msg:"moved continuation kept" (Grid.is_continuation grid 3);
  equal ~msg:"row" string "中中😀" (row_to_string grid 0)

let blit_region_copies_transparent_source () =
  let src = Grid.create ~width:1 ~height:1 () in
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.set_uchar src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'T') ~fg:transparent
    ~bg:transparent ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_text dst ~x:0 ~y:0 ~text:"D";
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"transparent source copied" int (Char.code 'T') (read_char dst 0 0);
  let _r, _g, _b, a = read_bg dst 0 0 in
  equal ~msg:"transparent background copied" int 0 a

let draw_region_skips_transparent_source () =
  let src = Grid.create ~width:1 ~height:1 () in
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.set_uchar ~blend:false src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'T')
    ~fg:transparent ~bg:transparent ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_text dst ~x:0 ~y:0 ~text:"D";
  Grid.draw_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"transparent source skipped" int (Char.code 'D')
    (read_char dst 0 0)

let blit_region_preserves_default_foreground_glyph () =
  let src = Grid.create ~width:1 ~height:1 () in
  let style = Ansi.Style.make ~fg:Ansi.Color.default_fg () in
  Grid.draw_text ~style src ~x:0 ~y:0 ~text:"T";
  equal ~msg:"source retains default foreground intent" bool true
    (Ansi.Color.equal (Grid.get_fg src 0) Ansi.Color.default_fg);
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_text dst ~x:0 ~y:0 ~text:"D";
  Grid.draw_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"default foreground glyph copied" int (Char.code 'T')
    (read_char dst 0 0);
  equal ~msg:"default foreground intent retained" bool true
    (Ansi.Color.equal (Grid.get_fg dst 0) Ansi.Color.default_fg)

let to_ansi_selects_complete_rows () =
  let grid = Grid.create ~width:1 ~height:3 () in
  List.iteri (fun y text -> Grid.draw_text grid ~x:0 ~y ~text) [ "A"; "B"; "C" ];
  let ansi = Grid.to_ansi ~complete_rows:true ~rows:2 grid in
  in_order ~msg:"selected rows are cleared, styled, and CRLF-terminated"
    ~subs:[ "\027[2K"; "A"; "\r\n"; "\027[2K"; "B"; "\r\n" ]
    ansi;
  not_contains ~msg:"unselected row omitted" ~sub:"C" ansi;
  equal ~msg:"zero rows serialize to no bytes" string ""
    (Grid.to_ansi ~rows:0 grid);
  let rejected =
    try
      ignore (Grid.to_ansi ~rows:4 grid : string);
      false
    with Invalid_argument _ -> true
  in
  is_true ~msg:"row prefix cannot exceed grid height" rejected

let to_ansi_handles_empty_glyph () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_uchar grid ~x:0 ~y:0 ~uchar:Uchar.min ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let ansi = Grid.to_ansi grid in
  is_true ~msg:"serialized empty glyph as space" (String.contains ansi ' ')

let copied_partial_span_becomes_space () =
  let src = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中";
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.blit_region ~src ~dst:grid ~src_x:1 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  assert_valid_spans grid;
  let ansi = Grid.to_ansi grid in
  is_true ~msg:"partial span serializes as space" (String.contains ansi ' ')

let to_ansi_preserves_indexed_foreground () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let style = Ansi.Style.make ~fg:(Ansi.Color.indexed 42) () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
  let ansi = Grid.to_ansi grid in
  contains ~msg:"indexed foreground serialized as palette SGR"
    ~sub:"\027[0;38;5;42m" ansi

(* The styling a row is written at: a depth reduces each color, [`Mono]
   keeps the attributes alone, and [`Plain] writes the text alone. *)
let to_ansi_at_a_styling () =
  let grid = Grid.create ~width:3 ~height:2 () in
  let style =
    Ansi.Style.make
      ~fg:(Ansi.Color.of_rgb 255 0 0)
      ~bold:true ~link:"https://a.example" ()
  in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ab" ~style;
  Grid.draw_text grid ~x:0 ~y:1 ~text:"c";
  let at styling = Grid.to_ansi ~styling grid in
  equal ~msg:"plain is the text" string "ab \nc  " (at `Plain);
  equal ~msg:"plain, complete rows" string "\027[2Kab \r\n"
    (Grid.to_ansi ~styling:`Plain ~complete_rows:true ~rows:1 grid);
  contains ~msg:"mono keeps bold" ~sub:"\027[0;1m" (at `Mono);
  not_contains ~msg:"mono drops the color" ~sub:"38;" (at `Mono);
  contains ~msg:"mono keeps the link" ~sub:"https://a.example" (at `Mono);
  contains ~msg:"16 colors" ~sub:"\027[0;91;1m" (at `Ansi16);
  contains ~msg:"256 colors" ~sub:"38;5;196" (at `Ansi256);
  equal ~msg:"24-bit by default" string (Grid.to_ansi grid) (at `Truecolor)

(* Trimming drops the cells at the end of a row that show nothing, and
   keeps a blank that shows a background or an underline. *)
let to_ansi_trims () =
  let grid = Grid.create ~width:8 ~height:3 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"a b";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"c"
    ~style:(Ansi.Style.make ~bg:Ansi.Color.blue ());
  Grid.draw_text grid ~x:1 ~y:1 ~text:"  "
    ~style:(Ansi.Style.make ~bg:Ansi.Color.blue ());
  Grid.draw_text grid ~x:0 ~y:2 ~text:"d "
    ~style:(Ansi.Style.make ~underline:true ());
  equal ~msg:"plain" string "a b\nc  \nd "
    (Grid.to_ansi ~styling:`Plain ~trim:true grid);
  equal ~msg:"untrimmed" string "a b     \nc       \nd       "
    (Grid.to_ansi ~styling:`Plain grid);
  let styled = Grid.to_ansi ~trim:true grid in
  is_true ~msg:"a row ends where its text ends"
    (String.starts_with ~prefix:"\027[0ma b\027[0m\n" styled)

let blit_preserves_indexed_background_intent () =
  let src = Grid.create ~width:1 ~height:1 () in
  Grid.set_uchar src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'X') ~fg:Ansi.Color.white
    ~bg:(Ansi.Color.indexed 99) ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit ~src ~dst;
  let ansi = Grid.to_ansi dst in
  contains ~msg:"indexed background survives blit" ~sub:"48;5;99" ansi

let draw_text_overflow_does_nothing () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"中";
  (* Wide graphemes that overflow are discarded by clearing to end-of-line with
     styled spaces, preventing partially drawn clusters. *)
  equal ~msg:"overflow clears to space" int (Char.code ' ') (read_char grid 1 0);
  equal ~msg:"width set to 1" int 1 (read_width grid 1 0)

let ambiguous_width_defaults_to_one () =
  let check_width label s =
    let width = Matrix_text.measure ~width_method:`Unicode ~tab_width:2 s in
    equal ~msg:label int 1 width
  in
  check_width "┌ width" "┌";
  check_width "┐ width" "┐";
  check_width "─ width" "─";
  check_width "│ width" "│"

let canvas_like_primitives_render () =
  let grid = Grid.create ~width:8 ~height:4 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"────────";
  for y = 0 to 3 do
    Grid.draw_text grid ~x:0 ~y ~text:"│"
  done;
  Grid.draw_text grid ~x:3 ~y:2 ~text:"x";
  equal ~msg:"row 0" string "│───────" (row_trimmed grid 0);
  equal ~msg:"row 1" string "│" (row_trimmed grid 1);
  equal ~msg:"row 2" string "│  x" (row_trimmed grid 2);
  equal ~msg:"row 3" string "│" (row_trimmed grid 3)

let canvas_like_resizing () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let write_text text ~x ~y =
    let width =
      Matrix_text.measure ~width_method:(Grid.width_method grid) ~tab_width:2
        text
    in
    let width = if width <= 0 then 1 else width in
    if x + width > Grid.width grid then
      Grid.resize grid ~width:(x + width) ~height:(Grid.height grid);
    if y + 1 > Grid.height grid then
      Grid.resize grid ~width:(Grid.width grid) ~height:(y + 1);
    Grid.draw_text grid ~x ~y ~text
  in
  for x = 0 to 7 do
    write_text "─" ~x ~y:0
  done;
  for y = 0 to 3 do
    write_text "│" ~x:0 ~y
  done;
  Grid.draw_text grid ~x:3 ~y:2 ~text:"x";
  equal ~msg:"width resized" int 8 (Grid.width grid);
  equal ~msg:"row 0 after resize" string "│───────" (row_trimmed grid 0);
  equal ~msg:"row 2 after resize" string "│  x    " (row_to_string grid 2)

let canvas_blit_into_box () =
  let canvas = Grid.create ~width:8 ~height:4 () in
  Grid.draw_text canvas ~x:0 ~y:0 ~text:"────────";
  for y = 0 to 3 do
    Grid.draw_text canvas ~x:0 ~y ~text:"│"
  done;
  Grid.draw_text canvas ~x:3 ~y:2 ~text:"x";
  let dest = Grid.create ~width:10 ~height:6 () in
  (* Draw outer border manually *)
  Grid.draw_text dest ~x:0 ~y:0 ~text:"┌";
  Grid.draw_text dest ~x:9 ~y:0 ~text:"┐";
  Grid.draw_text dest ~x:0 ~y:5 ~text:"└";
  Grid.draw_text dest ~x:9 ~y:5 ~text:"┘";
  Grid.draw_text dest ~x:1 ~y:0 ~text:"────────";
  Grid.draw_text dest ~x:1 ~y:5 ~text:"────────";
  for y = 1 to 4 do
    Grid.draw_text dest ~x:0 ~y ~text:"│";
    Grid.draw_text dest ~x:9 ~y ~text:"│"
  done;
  Grid.push_clip dest { x = 1; y = 1; width = 8; height = 4 };
  Grid.blit_region ~src:canvas ~dst:dest ~src_x:0 ~src_y:0 ~width:8 ~height:4
    ~dst_x:1 ~dst_y:1;
  Grid.pop_clip dest;
  equal ~msg:"blit row 0" string "┌────────┐" (row_trimmed dest 0);
  equal ~msg:"blit row 1" string "││───────│" (row_trimmed dest 1);
  equal ~msg:"blit row 2" string "││       │" (row_trimmed dest 2);
  equal ~msg:"blit row 3" string "││  x    │" (row_trimmed dest 3);
  equal ~msg:"blit row 4" string "││       │" (row_trimmed dest 4);
  equal ~msg:"blit row 5" string "└────────┘" (row_trimmed dest 5)

let clear_resets_grid () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.set_uchar ~blend:true grid ~x:0 ~y:0 ~uchar:(Uchar.of_char 'Q')
    ~fg:Ansi.Color.white ~bg:Ansi.Color.white ~attrs:Ansi.Attr.bold ();
  let color = Ansi.Color.of_rgb 10 20 30 in
  Grid.clear ~color grid;
  for y = 0 to 1 do
    for x = 0 to 1 do
      (* After clear, chars are spaces so blank cells mirror terminal
         defaults. *)
      equal ~msg:"char" int (Char.code ' ') (read_char grid x y);
      equal ~msg:"width" int 1 (read_width grid x y);
      let r, g, b, _ = read_bg grid x y in
      let er, eg, eb, _ = Ansi.Color.to_rgba color in
      equal ~msg:"color" (triple int int int) (er, eg, eb) (r, g, b)
    done
  done

let resize_updates_dimensions () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.resize grid ~width:4 ~height:1;
  equal ~msg:"width" int 4 (Grid.width grid);
  equal ~msg:"height" int 1 (Grid.height grid)

let blit_copies_full_buffer () =
  let src = Grid.create ~width:2 ~height:2 () in
  Grid.set_uchar ~blend:true src ~x:0 ~y:0 ~uchar:(Uchar.of_char 'A')
    ~fg:Ansi.Color.cyan ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit ~src ~dst;
  equal ~msg:"width" int 2 (Grid.width dst);
  equal ~msg:"height" int 2 (Grid.height dst);
  equal ~msg:"copied char" int (Char.code 'A') (read_char dst 0 0)

let blit_region_copies_subrect () =
  let src = Grid.create ~width:3 ~height:3 () in
  for y = 0 to 2 do
    for x = 0 to 2 do
      let code = Char.code 'a' + idx src x y in
      Grid.set_uchar ~blend:true src ~x ~y ~uchar:(Uchar.of_int code)
        ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ()
    done
  done;
  let dst = Grid.create ~width:3 ~height:3 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:1 ~width:2 ~height:2 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"copy (0,0)" int (read_char src 1 1) (read_char dst 0 0);
  equal ~msg:"copy (1,1)" int (read_char src 2 2) (read_char dst 1 1);
  equal ~msg:"outside untouched" int 32 (read_char dst 2 2)

(* Regression test: ASCII fast path produces correct output *)
let ascii_fast_path_correctness () =
  let grid = Grid.create ~width:10 ~height:1 () in
  let style =
    Ansi.Style.make
      ~fg:(Ansi.Color.of_rgb 100 150 200)
      ~bg:(Ansi.Color.of_rgb 20 30 40)
      ~bold:true ()
  in
  Grid.draw_text ~style grid ~x:0 ~y:0 ~text:"Hello";
  equal ~msg:"char H" int (Char.code 'H') (read_char grid 0 0);
  equal ~msg:"char o" int (Char.code 'o') (read_char grid 4 0);
  let r, g, _b, _a = read_fg grid 0 0 in
  equal ~msg:"fg red" int 100 r;
  equal ~msg:"fg green" int 150 g;
  let attrs = read_attr grid 0 0 |> Ansi.Attr.unpack in
  is_true ~msg:"bold set" (Ansi.Attr.mem Ansi.Attr.Bold attrs)

(* Regression test: ASCII overwrites middle of wide grapheme *)
let ascii_overwrites_wide_grapheme_middle () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"😊";
  equal ~msg:"continuation width" int 0 (read_width grid 2 0);
  Grid.draw_text grid ~x:2 ~y:0 ~text:"X";
  equal ~msg:"emoji cleared" int 32 (read_char grid 1 0);
  equal ~msg:"X written" int (Char.code 'X') (read_char grid 2 0);
  equal ~msg:"width reset" int 1 (read_width grid 1 0)

(* Regression test: Mixed ASCII and emoji both render (control flow bug) *)
let mixed_ascii_emoji_render () =
  let grid = Grid.create ~width:10 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hi";
  Grid.draw_text grid ~x:2 ~y:0 ~text:"😊";
  Grid.draw_text grid ~x:4 ~y:0 ~text:"Ok";
  equal ~msg:"H" int (Char.code 'H') (read_char grid 0 0);
  is_true ~msg:"emoji rendered" (read_char grid 2 0 <> 0);
  equal ~msg:"O" int (Char.code 'O') (read_char grid 4 0)

(* Regression test: Clear resets all cells including after graphemes *)
let clear_after_graphemes () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"😊🚀";
  is_true ~msg:"grapheme written" (read_char grid 0 0 <> 0);
  Grid.clear grid ~color:(Ansi.Color.of_rgb 10 20 30);
  (* All cells should be cleared *)
  equal ~msg:"char cleared" int (Char.code ' ') (read_char grid 0 0);
  equal ~msg:"width reset" int 1 (read_width grid 0 0);
  let r, g, b, _a = read_bg grid 0 0 in
  equal ~msg:"bg red" int 10 r;
  equal ~msg:"bg green" int 20 g;
  equal ~msg:"bg blue" int 30 b

(* Edge case: Empty string is safe no-op *)
let empty_string_is_noop () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"ABC";
  Grid.draw_text grid ~x:0 ~y:0 ~text:"";
  equal ~msg:"A preserved" int (Char.code 'A') (read_char grid 0 0)

(* Edge case: Negative x clips correctly *)
let negative_x_clips_text () =
  let grid = Grid.create ~width:5 ~height:1 () in
  Grid.draw_text grid ~x:(-2) ~y:0 ~text:"ABCDE";
  equal ~msg:"C at x=0" int (Char.code 'C') (read_char grid 0 0);
  equal ~msg:"x=3 empty" int 32 (read_char grid 3 0)

let box_drawing_characters_render () =
  let grid = Grid.create ~width:4 ~height:4 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"┌──┐";
  Grid.draw_text grid ~x:0 ~y:3 ~text:"└──┘";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"│";
  Grid.draw_text grid ~x:0 ~y:2 ~text:"│";
  Grid.draw_text grid ~x:3 ~y:1 ~text:"│";
  Grid.draw_text grid ~x:3 ~y:2 ~text:"│";
  equal ~msg:"top row" string "┌──┐" (row_to_string grid 0);
  equal ~msg:"inner row 1" string "│  │" (row_to_string grid 1);
  equal ~msg:"inner row 2" string "│  │" (row_to_string grid 2);
  equal ~msg:"bottom row" string "└──┘" (row_to_string grid 3)

let draw_text_ascii_respects_scissor () =
  let grid = Grid.create ~width:5 ~height:1 () in
  (* Only columns 2..4 are writable *)
  Grid.push_clip grid { x = 2; y = 0; width = 3; height = 1 };
  Grid.draw_text grid ~x:0 ~y:0 ~text:"Hello";
  Grid.pop_clip grid;
  (* Outside scissor should be untouched *)
  equal ~msg:"x=0 untouched" int 32 (read_char grid 0 0);
  equal ~msg:"x=1 untouched" int 32 (read_char grid 1 0);
  (* Inside scissor has clipped text: 'llo' at 2..4 *)
  equal ~msg:"x=2 l" int (Char.code 'l') (read_char grid 2 0);
  equal ~msg:"x=3 l" int (Char.code 'l') (read_char grid 3 0);
  equal ~msg:"x=4 o" int (Char.code 'o') (read_char grid 4 0)

let draw_text_tab_partially_visible_respects_scissor () =
  let base = Grid.create ~width:6 ~height:1 () in
  let grid = Grid.create ~width:6 ~height:1 () in
  let tab_bg = Ansi.Color.of_rgb 10 20 30 in
  let style = Ansi.Style.make ~bg:tab_bg ~bold:true () in
  let as_rgba (r, g, b, a) = (r, (g, (b, a))) in
  Grid.draw_text ~style ~tab_width:4 base ~x:0 ~y:0 ~text:"\t";
  let tr, tg, tb, ta = Ansi.Color.to_rgba tab_bg in
  equal ~msg:"base x=0 tab writes bg" rgba
    (tr, (tg, (tb, ta)))
    (as_rgba (read_bg base 0 0));
  equal ~msg:"base x=3 tab writes bg" rgba
    (tr, (tg, (tb, ta)))
    (as_rgba (read_bg base 3 0));
  equal ~msg:"base x=4 untouched bg" rgba
    (0, (0, (0, 0)))
    (as_rgba (read_bg base 4 0));
  Grid.push_clip grid { x = 2; y = 0; width = 3; height = 1 };
  Grid.draw_text ~style ~tab_width:4 grid ~x:0 ~y:0 ~text:"\t";
  Grid.pop_clip grid;
  equal ~msg:"x=0 untouched bg" rgba
    (0, (0, (0, 0)))
    (as_rgba (read_bg grid 0 0));
  equal ~msg:"x=1 untouched bg" rgba
    (0, (0, (0, 0)))
    (as_rgba (read_bg grid 1 0));
  equal ~msg:"x=2 styled tab space bg" rgba
    (tr, (tg, (tb, ta)))
    (as_rgba (read_bg grid 2 0));
  equal ~msg:"x=3 styled tab space bg" rgba
    (tr, (tg, (tb, ta)))
    (as_rgba (read_bg grid 3 0));
  equal ~msg:"x=4 untouched bg" rgba
    (0, (0, (0, 0)))
    (as_rgba (read_bg grid 4 0))

let fill_rect_respects_scissor () =
  let grid = Grid.create ~width:4 ~height:2 () in
  Grid.push_clip grid { x = 1; y = 0; width = 2; height = 2 };
  let color = Ansi.Color.green in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:4 ~height:2 ~color;
  Grid.pop_clip grid;
  (* Only x=1 and x=2 columns should be filled with spaces and color *)
  let er, eg, eb, _ = Ansi.Color.to_rgba color in
  for y = 0 to 1 do
    (* outside left *)
    equal
      ~msg:(Printf.sprintf "(%d, %d) untouched" 0 y)
      int 32 (read_char grid 0 y);
    (* inside *)
    equal
      ~msg:(Printf.sprintf "(%d, %d) space" 1 y)
      int (Char.code ' ') (read_char grid 1 y);
    let r, g, b, _ = read_bg grid 1 y in
    equal ~msg:"bg left in scissor" (triple int int int) (er, eg, eb) (r, g, b);
    equal
      ~msg:(Printf.sprintf "(%d, %d) space" 2 y)
      int (Char.code ' ') (read_char grid 2 y);
    let r2, g2, b2, _ = read_bg grid 2 y in
    equal ~msg:"bg right in scissor" (triple int int int) (er, eg, eb)
      (r2, g2, b2);
    (* outside right *)
    equal
      ~msg:(Printf.sprintf "(%d, %d) untouched" 3 y)
      int 32 (read_char grid 3 y)
  done

let blit_region_respects_scissor () =
  let src = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"ABC";
  let dst = Grid.create ~width:3 ~height:1 () in
  Grid.push_clip dst { x = 1; y = 0; width = 2; height = 1 };
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:3 ~height:1 ~dst_x:0
    ~dst_y:0;
  Grid.pop_clip dst;
  equal ~msg:"dst(0) untouched" int 32 (read_char dst 0 0);
  equal ~msg:"dst(1)=B" int (Char.code 'B') (read_char dst 1 0);
  equal ~msg:"dst(2)=C" int (Char.code 'C') (read_char dst 2 0)

let opacity_stack_grows_beyond_initial_capacity () =
  let grid = Grid.create ~width:1 ~height:1 () in
  for _ = 1 to 40 do
    Grid.push_opacity grid 0.5
  done;
  let expected = Float.pow 0.5 40. in
  equal ~msg:"opacity product tracks all pushes" (float 1e-18) expected
    (Grid.current_opacity grid);
  for _ = 1 to 40 do
    Grid.pop_opacity grid
  done;
  equal ~msg:"opacity restores to 1 after balanced pops" (float 1e-18) 1.0
    (Grid.current_opacity grid)

let clear_preserves_scissor_state () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.push_clip grid { x = 1; y = 0; width = 1; height = 1 };
  Grid.clear grid;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  Grid.pop_clip grid;
  equal ~msg:"write outside preserved scissor ignored" int 32
    (read_char grid 0 0)

let draw_text_skips_zero_width () =
  (* Zero-width units standing alone, and controls, take no cell and join
     none: a terminal that wants them in the line joins them itself. *)
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"a\u{200B}\u{FEFF}b\nc";
  equal ~msg:"cells" (list string) [ "a"; "b"; "c" ]
    (List.init 3 (Grid.get_text grid));
  Grid.draw_text grid ~x:0 ~y:0 ~text:"x\u{9B}2J";
  equal ~msg:"C1 control stored nowhere" (list string) [ "x"; "2"; "J" ]
    (List.init 3 (Grid.get_text grid))

let draw_text_overflow_clears_row_tail () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
  Grid.draw_text grid ~x:2 ~y:0 ~text:"中";
  equal ~msg:"overflow start cleared" int (Char.code ' ') (read_char grid 2 0);
  equal ~msg:"prefix preserved" string "XY"
    (String.sub (row_to_string grid 0) 0 2)

let wide_span_beyond_four_cells () =
  let n = 300 in
  let grid = Grid.create ~width:(n + 2) ~height:1 () in
  span grid ~x:1 n;
  equal ~msg:"start spans the grapheme" int n (Grid.cell_width grid 1);
  is_true ~msg:"last cell continues it" (Grid.is_continuation grid n);
  is_false ~msg:"cell after it is free" (Grid.is_continuation grid (n + 1));
  assert_valid_spans grid;
  Grid.draw_text grid ~x:150 ~y:0 ~text:"x";
  equal ~msg:"overwriting its middle clears it whole" string
    (String.make 150 ' ' ^ "x" ^ String.make 151 ' ')
    (Grid.to_text ~trim:false grid);
  assert_valid_spans grid

let wide_span_survives_moves () =
  let grid = Grid.create ~width:12 ~height:2 () in
  span grid ~x:0 6;
  Grid.blit_region ~src:grid ~dst:grid ~src_x:0 ~src_y:0 ~width:12 ~height:1
    ~dst_x:3 ~dst_y:1;
  equal ~msg:"copy keeps the width" int 6 (Grid.cell_width grid 15);
  assert_valid_spans grid;
  let dst = Grid.create ~width:12 ~height:1 () in
  Grid.blit_region ~src:grid ~dst ~src_x:0 ~src_y:0 ~width:5 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"a cut copy leaves no part" string "" (Grid.to_text dst);
  assert_valid_spans dst

let max_width_bounds_grid () =
  ignore (Grid.create ~width:Grid.max_width ~height:1 () : Grid.t);
  expect_invalid_arg "create" (fun () ->
      ignore (Grid.create ~width:(Grid.max_width + 1) ~height:1 () : Grid.t));
  let grid = Grid.create ~width:1 ~height:1 () in
  expect_invalid_arg "resize" (fun () ->
      Grid.resize grid ~width:(Grid.max_width + 1) ~height:1)

(* Blanks and shifts *)

let red = Ansi.Color.of_rgb 200 0 0

(* A row of four wide characters in green on blue, linked. *)
let wide_rows ?(width = 8) ?(height = 1) () =
  let grid = Grid.create ~width ~height () in
  let style =
    Ansi.Style.make ~fg:Ansi.Color.green ~bg:Ansi.Color.blue
      ~link:"https://example.com" ~bold:true ()
  in
  for y = 0 to height - 1 do
    Grid.draw_text grid ~x:0 ~y ~style ~text:"中文中文"
  done;
  grid

(* Every space of [grid] is the one blank on [color]. *)
let assert_one_blank ~msg grid color =
  assert_valid_spans grid;
  for i = 0 to (Grid.width grid * Grid.height grid) - 1 do
    if Grid.get_text grid i = " " then begin
      let at = Printf.sprintf "%s: cell %d" msg i in
      is_true ~msg:(at ^ " bg") (Ansi.Color.equal color (Grid.get_bg grid i));
      is_true ~msg:(at ^ " fg")
        (Ansi.Color.equal Ansi.Color.default_fg (Grid.get_fg grid i));
      equal ~msg:(at ^ " attrs") int 0 (Grid.get_attrs grid i);
      equal ~msg:(at ^ " link") string "" (Grid.get_link grid i)
    end
  done

let region x y width height : Grid.region = { x; y; width; height }

(* Each operation that cuts graphemes at both ends of its rectangle leaves
   one kind of blank, cut halves included. *)
let one_operation_one_blank () =
  let ops =
    [
      ( "clear_rect",
        fun g -> Grid.clear_rect ~color:red g ~x:1 ~y:0 ~width:4 ~height:1 );
      ( "fill_rect",
        fun g -> Grid.fill_rect g ~x:1 ~y:0 ~width:4 ~height:1 ~color:red );
      ( "shift left",
        fun g -> Grid.shift g (region 1 0 6 1) ~dx:(-2) ~dy:0 ~color:red );
      ( "shift right",
        fun g -> Grid.shift g (region 1 0 6 1) ~dx:3 ~dy:0 ~color:red );
      ( "shift down",
        fun g -> Grid.shift g (region 1 0 4 2) ~dx:0 ~dy:1 ~color:red );
      ( "shift up",
        fun g -> Grid.shift g (region 3 0 4 2) ~dx:0 ~dy:(-1) ~color:red );
      ( "shift diagonal",
        fun g -> Grid.shift g (region 1 0 6 2) ~dx:1 ~dy:1 ~color:red );
      ( "shift out",
        fun g -> Grid.shift g (region 1 0 4 2) ~dx:4 ~dy:0 ~color:red );
    ]
  in
  List.iter
    (fun (name, op) ->
      let grid = wide_rows ~height:2 () in
      op grid;
      assert_one_blank ~msg:name grid red)
    ops

let clear_rect_clears_whole_graphemes () =
  let grid = wide_rows () in
  Grid.clear_rect ~color:red grid ~x:1 ~y:0 ~width:4 ~height:1;
  equal ~msg:"three graphemes cleared whole" string "      文"
    (Grid.to_text grid);
  assert_one_blank ~msg:"clear_rect" grid red

let write_repairs_with_written_bg () =
  let grid = wide_rows () in
  Grid.set_uchar grid ~x:1 ~y:0 ~uchar:(Uchar.of_char 'x') ~fg:Ansi.Color.white
    ~bg:red ~attrs:Ansi.Attr.empty ();
  equal ~msg:"text" string " x文中文" (Grid.to_text grid);
  assert_one_blank ~msg:"set_uchar" grid red

let shift_moves_cells () =
  let grid = Grid.create ~width:6 ~height:3 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"abcdef";
  Grid.draw_text grid ~x:0 ~y:1 ~text:"ghijkl";
  Grid.draw_text grid ~x:0 ~y:2 ~text:"mnopqr";
  Grid.shift grid (region 1 0 4 3) ~dx:(-1) ~dy:1 ~color:red;
  equal ~msg:"moved left and down" string "a    f\ngcde l\nmijk r"
    (Grid.to_text ~trim:false grid);
  is_true ~msg:"uncovered cell is a blank on color"
    (Ansi.Color.equal red (Grid.get_bg grid (idx grid 4 1)));
  let grid = Grid.create ~width:4 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"abcd";
  Grid.shift grid (region 0 0 4 1) ~dx:0 ~dy:0 ~color:red;
  equal ~msg:"a null shift changes nothing" string "abcd" (Grid.to_text grid);
  Grid.shift grid (region (-2) 0 4 1) ~dx:1 ~dy:0 ~color:red;
  equal ~msg:"the region is clipped to the grid" string " acd"
    (Grid.to_text ~trim:false grid);
  Grid.clip grid (region 0 0 1 1) (fun () ->
      Grid.shift grid (region 0 0 4 1) ~dx:(-1) ~dy:0 ~color:red);
  equal ~msg:"the scissor does not apply" string "acd "
    (Grid.to_text ~trim:false grid)

let shift_carries_cell_contents () =
  let grid = wide_rows ~width:10 () in
  Grid.shift grid (region 0 0 10 1) ~dx:2 ~dy:0 ~color:red;
  let i = idx grid 2 0 in
  equal ~msg:"text" string "中" (Grid.get_text grid i);
  equal ~msg:"link" string "https://example.com" (Grid.get_link grid i);
  is_true ~msg:"bg" (Ansi.Color.equal Ansi.Color.blue (Grid.get_bg grid i));
  is_true ~msg:"attrs" (Grid.get_attrs grid i <> 0);
  assert_valid_spans grid

(* set_grapheme and protection *)

let put ?protected ?(bg = Ansi.Color.blue) ?(attrs = Ansi.Attr.empty) grid x
    width text =
  Grid.set_grapheme grid ~x ~y:0 ~width ~fg:Ansi.Color.green ~bg ~attrs
    ?protected text ~pos:0 ~len:(String.length text)

let set_grapheme_places_exactly () =
  let grid = Grid.create ~width:6 ~height:1 () in
  put grid 0 3 "e\u{0301}";
  equal ~msg:"caller's width" int 3 (Grid.cell_width grid 0);
  equal ~msg:"text" string "e\u{0301}" (Grid.get_text grid 0);
  put grid 3 1 "中";
  equal ~msg:"clamped wide character" int 1 (Grid.cell_width grid 3);
  put grid 4 2 "a";
  equal ~msg:"a single scalar widened" int 2 (Grid.cell_width grid 4);
  is_true ~msg:"its continuation" (Grid.is_continuation grid 5);
  assert_valid_spans grid;
  Grid.push_opacity grid 0.5;
  Grid.push_clip grid (region 0 0 1 1);
  put grid 1 1 "z";
  equal ~msg:"no scissor" string "z" (Grid.get_text grid 1);
  is_true ~msg:"no opacity, no blending"
    (Ansi.Color.equal Ansi.Color.blue (Grid.get_bg grid 1));
  equal ~msg:"cut grapheme blanked" string " " (Grid.get_text grid 0);
  is_true ~msg:"on the written background"
    (Ansi.Color.equal Ansi.Color.blue (Grid.get_bg grid 2));
  assert_valid_spans grid

let set_grapheme_rejects () =
  let grid = Grid.create ~width:4 ~height:2 () in
  let rejects msg ?(x = 0) ?(y = 0) ?(width = 1) ?(pos = 0) ?len text =
    let len = Option.value len ~default:(String.length text - pos) in
    expect_invalid_arg msg (fun () ->
        Grid.set_grapheme grid ~x ~y ~width ~fg:Ansi.Color.white
          ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty text ~pos ~len)
  in
  rejects "empty" "";
  rejects "substring" ~pos:2 ~len:1 "ab";
  rejects "width 0" ~width:0 "a";
  rejects "past the right edge" ~x:3 ~width:2 "a";
  rejects "left of the grid" ~x:(-1) "a";
  rejects "below the grid" ~y:2 "a";
  List.iter
    (fun c -> rejects (Printf.sprintf "control %S" c) c)
    [
      "\x00";
      "\x1b";
      "\t";
      "\n";
      "\x7f";
      "\u{80}";
      "\u{9B}";
      "\u{9F}";
      "a\u{0301}\u{85}";
    ];
  equal ~msg:"nothing written" string "\n" (Grid.to_text grid);
  put grid 0 1 "\u{A0}";
  equal ~msg:"U+00A0 is no control" string "\u{A0}" (Grid.get_text grid 0)

let protection_is_invisible () =
  let bold = Ansi.Attr.bold in
  let a = Grid.create ~width:2 ~height:1 () in
  let b = Grid.create ~width:2 ~height:1 () in
  put ~protected:true ~attrs:bold a 0 2 "中";
  put ~attrs:bold b 0 2 "中";
  is_true ~msg:"protected" (Grid.is_protected a 0 && Grid.is_protected a 1);
  is_false ~msg:"unprotected" (Grid.is_protected b 0);
  equal ~msg:"attrs" int (Grid.get_attrs b 0) (Grid.get_attrs a 0);
  is_true ~msg:"cells_equal" (Grid.cells_equal a 0 b 0);
  equal ~msg:"to_ansi" string (Grid.to_ansi b) (Grid.to_ansi a);
  is_true ~msg:"style"
    (Ansi.Style.equal (Grid.get_style b 0) (Grid.get_style a 0))

let protection_moves_with_cells () =
  let grid = Grid.create ~width:6 ~height:2 () in
  put ~protected:true grid 0 1 "p";
  Grid.shift grid (region 0 0 6 2) ~dx:2 ~dy:1 ~color:red;
  is_true ~msg:"moved" (Grid.is_protected grid (idx grid 2 1));
  is_false ~msg:"the blank left behind" (Grid.is_protected grid 0);
  let copy = Grid.copy grid in
  is_true ~msg:"copied" (Grid.is_protected copy (idx grid 2 1));
  Grid.resize grid ~width:4 ~height:2;
  is_true ~msg:"kept by resize" (Grid.is_protected grid (idx grid 2 1));
  Grid.draw_text grid ~x:2 ~y:1 ~text:"q";
  is_false ~msg:"replaced by a write" (Grid.is_protected grid (idx grid 2 1));
  put ~protected:true grid 0 2 "中";
  Grid.clear_rect grid ~x:1 ~y:0 ~width:1 ~height:1;
  is_false ~msg:"erased" (Grid.is_protected grid 0)

let erase_unprotected_keeps_rendition () =
  let grid = Grid.create ~width:8 ~height:1 () in
  let bold = Ansi.Attr.bold in
  Grid.set_grapheme grid ~x:0 ~y:0 ~width:2 ~fg:Ansi.Color.green ~bg:red
    ~attrs:bold ~link:"https://example.com" "中" ~pos:0 ~len:3;
  put ~protected:true grid 2 1 "P";
  put grid 3 2 "文";
  put grid 5 1 "x";
  Grid.erase_unprotected grid (region 1 0 3 1);
  equal ~msg:"text" string "  P  x" (Grid.to_text grid);
  List.iter
    (fun i ->
      is_true ~msg:"keeps bg" (Ansi.Color.equal red (Grid.get_bg grid i));
      is_true ~msg:"keeps fg"
        (Ansi.Color.equal Ansi.Color.green (Grid.get_fg grid i));
      equal ~msg:"keeps attrs" int (Ansi.Attr.pack bold) (Grid.get_attrs grid i);
      equal ~msg:"drops link" string "" (Grid.get_link grid i))
    [ 0; 1 ];
  is_true ~msg:"keeps the bg of the cut grapheme"
    (Ansi.Color.equal Ansi.Color.blue (Grid.get_bg grid 4));
  is_false ~msg:"no continuation left" (Grid.is_continuation grid 4);
  equal ~msg:"no grapheme left" int 0 (fst (Grid.grapheme_stats grid));
  assert_valid_spans grid

(* A cell's text is what [get_text] gives: nothing for an empty cell and for
   the continuation cells of a wide grapheme. *)
let cell_text_of_empty_and_continuation_cells () =
  let grid = Grid.create ~width:6 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"\u{4E2D}\u{1F469}\u{200D}\u{1F680}";
  Grid.set_uchar grid ~x:4 ~y:0 ~uchar:Uchar.min ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let buf = Bytes.make 16 'x' in
  List.iter
    (fun (name, x) ->
      equal ~msg:(name ^ ": get_text") string "" (Grid.get_text grid x);
      equal ~msg:(name ^ ": length") int 0 (Grid.cell_text_length grid x);
      equal ~msg:(name ^ ": blit") int 0 (Grid.blit_cell_text grid x buf ~pos:0))
    [ ("inline continuation", 1); ("stored continuation", 3); ("empty", 4) ];
  equal ~msg:"nothing written" string (String.make 16 'x') (Bytes.to_string buf)

(* A grid holds as many distinct graphemes as it has cells. Each cell here
   holds "a" and three combining marks that spell its index, 300,000 in all,
   more than the 262,143 a store once held. *)
let many_distinct_graphemes () =
  let width = 1000 and height = 300 in
  let grid = Grid.create ~width ~height () in
  let mark n =
    let b = Buffer.create 2 in
    Buffer.add_utf_8_uchar b (Uchar.of_int (0x300 + n));
    Buffer.contents b
  in
  let text i =
    "a" ^ mark (i mod 100) ^ mark (i / 100 mod 100) ^ mark (i / 10000)
  in
  for i = 0 to (width * height) - 1 do
    Grid.draw_text grid ~x:(i mod width) ~y:(i / width) ~text:(text i)
  done;
  let last = (width * height) - 1 in
  equal ~msg:"first cell" string (text 0) (Grid.get_text grid 0);
  equal ~msg:"last cell" string (text last) (Grid.get_text grid last)

(* Regression: draw_text kept the text under a space whose background was
   not opaque, the default style's included, so drawing "  " over a word
   erased nothing. Text drawing writes every unit; tinting the cells under
   a translucent color is fill_rect's, and keeping the cells under a layer's
   blanks is draw_region's. *)
let spaces_draw_and_layers_composite () =
  let over style =
    let grid = Grid.create ~width:3 ~height:1 () in
    Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
    Grid.draw_text grid ~x:0 ~y:0 ~text:" b" ?style;
    grid
  in
  equal ~msg:"default style" string " bZ" (Grid.to_text (over None));
  let tinted = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 255 0 0 128) () in
  equal ~msg:"translucent background" string " bZ"
    (Grid.to_text (over (Some tinted)));
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
  Grid.fill_rect grid ~x:0 ~y:0 ~width:3 ~height:1
    ~color:(Ansi.Color.of_rgba 255 0 0 128);
  equal ~msg:"a translucent fill keeps the text" string "XYZ"
    (Grid.to_text grid);
  let layer = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text layer ~x:1 ~y:0 ~text:"b";
  Grid.draw_region ~src:layer ~dst:grid ~src_x:0 ~src_y:0 ~width:3 ~height:1
    ~dst_x:0 ~dst_y:0;
  equal ~msg:"a layer's blanks keep the cells under them" string "XbZ"
    (Grid.to_text grid)

(* ───── Joined lines ───── *)

module Lines = Grid.Lines

let lines_text ?glyphs ~width ~height f =
  let l = Lines.create ~x:0 ~y:0 ~width ~height in
  f l;
  let g = Grid.create ~width ~height () in
  Grid.draw_lines ?glyphs g l;
  Grid.to_text g

(* A frame of weight [outer] around four rows, with light rules between them
   and light column lines broken where a row's cell spans columns. *)
let frame_with_spans ~outer =
  lines_text ~width:23 ~height:9 (fun l ->
      Lines.hline ~weight:outer l ~x:0 ~y:0 ~length:23;
      Lines.hline ~weight:outer l ~x:0 ~y:8 ~length:23;
      Lines.vline ~weight:outer l ~x:0 ~y:0 ~length:9;
      Lines.vline ~weight:outer l ~x:22 ~y:0 ~length:9;
      List.iter (fun y -> Lines.hline l ~x:0 ~y ~length:23) [ 2; 4; 6 ];
      (* Row 1 has three cells, row 3 two (the last spans two columns), row 5
         one, row 7 three. *)
      Lines.vline l ~x:7 ~y:0 ~length:5;
      Lines.vline l ~x:16 ~y:0 ~length:3;
      Lines.vline l ~x:7 ~y:6 ~length:3;
      Lines.vline l ~x:16 ~y:6 ~length:3)

let lines_tests =
  [
    test "inner tees under a double frame are single" (fun () ->
        equal ~msg:"frame" string
          "╔══════╤════════╤═════╗\n\
           ║      │        │     ║\n\
           ╟──────┼────────┴─────╢\n\
           ║      │              ║\n\
           ╟──────┴──────────────╢\n\
           ║                     ║\n\
           ╟──────┬────────┬─────╢\n\
           ║      │        │     ║\n\
           ╚══════╧════════╧═════╝"
          (frame_with_spans ~outer:`Double));
    test "inner tees under a heavy frame are light" (fun () ->
        equal ~msg:"frame" string
          "┏━━━━━━┯━━━━━━━━┯━━━━━┓\n\
           ┃      │        │     ┃\n\
           ┠──────┼────────┴─────┨\n\
           ┃      │              ┃\n\
           ┠──────┴──────────────┨\n\
           ┃                     ┃\n\
           ┠──────┬────────┬─────┨\n\
           ┃      │        │     ┃\n\
           ┗━━━━━━┷━━━━━━━━┷━━━━━┛"
          (frame_with_spans ~outer:`Heavy));
    test "every combination of arms has a box-drawing character" (fun () ->
        let weights = [ None; Some `Light; Some `Heavy; Some `Double ] in
        List.iter
          (fun up ->
            List.iter
              (fun down ->
                List.iter
                  (fun left ->
                    List.iter
                      (fun right ->
                        let l = Lines.create ~x:0 ~y:0 ~width:1 ~height:1 in
                        let add dir = function
                          | None -> ()
                          | Some weight -> Lines.arm ~weight l ~x:0 ~y:0 dir
                        in
                        add `Up up;
                        add `Down down;
                        add `Left left;
                        add `Right right;
                        let g = Grid.create ~width:1 ~height:1 () in
                        Grid.draw_lines g l;
                        let cp = Grid.get_codepoint g 0 in
                        if
                          List.exists Option.is_some [ up; down; left; right ]
                          && (cp < 0x2500 || cp > 0x257F)
                        then failf "no box-drawing character: U+%04X" cp)
                      weights)
                  weights)
              weights)
          weights);
    test "mixed weights take Unicode's mixed characters" (fun () ->
        equal ~msg:"light rule into a heavy edge" string "┠──┨"
          (lines_text ~width:4 ~height:1 (fun l ->
               Lines.vline ~weight:`Heavy l ~x:0 ~y:(-1) ~length:3;
               Lines.vline ~weight:`Heavy l ~x:3 ~y:(-1) ~length:3;
               Lines.hline l ~x:0 ~y:0 ~length:4));
        equal ~msg:"double rule across a single line" string "═╪═"
          (lines_text ~width:3 ~height:1 (fun l ->
               Lines.vline l ~x:1 ~y:(-1) ~length:3;
               Lines.hline ~weight:`Double l ~x:0 ~y:0 ~length:3));
        equal ~msg:"heavy meets double as double" string "═╬═"
          (lines_text ~width:3 ~height:1 (fun l ->
               Lines.vline ~weight:`Heavy l ~x:1 ~y:(-1) ~length:3;
               Lines.hline ~weight:`Double l ~x:0 ~y:0 ~length:3));
        equal ~msg:"a line light on one side and heavy on the other" string
          "─╼━"
          (lines_text ~width:3 ~height:1 (fun l ->
               Lines.hline l ~x:0 ~y:0 ~length:2;
               Lines.hline ~weight:`Heavy l ~x:1 ~y:0 ~length:2)));
    test "a line's ends and a one-cell line are drawn whole" (fun () ->
        equal ~msg:"ends" string "──\n │\n │"
          (lines_text ~width:2 ~height:3 (fun l ->
               Lines.hline l ~x:0 ~y:0 ~length:2;
               Lines.vline l ~x:1 ~y:1 ~length:2));
        equal ~msg:"one cell" string "─\n│"
          (lines_text ~width:1 ~height:2 (fun l ->
               Lines.hline l ~x:0 ~y:0 ~length:1;
               Lines.vline l ~x:0 ~y:1 ~length:1)));
    test "rounded and ASCII sets" (fun () ->
        let box l =
          Lines.hline l ~x:0 ~y:0 ~length:3;
          Lines.hline l ~x:0 ~y:2 ~length:3;
          Lines.vline l ~x:0 ~y:0 ~length:3;
          Lines.vline l ~x:2 ~y:0 ~length:3;
          Lines.hline l ~x:0 ~y:1 ~length:3;
          Lines.vline l ~x:1 ~y:0 ~length:3
        in
        equal ~msg:"rounded" string "╭┬╮\n├┼┤\n╰┴╯"
          (lines_text ~glyphs:Lines.Rounded ~width:3 ~height:3 box);
        equal ~msg:"ascii" string "+++\n+++\n+++"
          (lines_text ~glyphs:Lines.ascii ~width:3 ~height:3 box);
        equal ~msg:"heavy is never rounded" string "┏━┓\n┃ ┃\n┗━┛"
          (lines_text ~glyphs:Lines.Rounded ~width:3 ~height:3 (fun l ->
               Lines.hline ~weight:`Heavy l ~x:0 ~y:0 ~length:3;
               Lines.hline ~weight:`Heavy l ~x:0 ~y:2 ~length:3;
               Lines.vline ~weight:`Heavy l ~x:0 ~y:0 ~length:3;
               Lines.vline ~weight:`Heavy l ~x:2 ~y:0 ~length:3)));
    test "a border's set draws a box as draw_box does" (fun () ->
        let border = Grid.Border.double in
        let expected = Grid.create ~width:5 ~height:3 () in
        Grid.draw_box expected ~x:0 ~y:0 ~width:5 ~height:3 ~border ();
        equal ~msg:"box" string (Grid.to_text expected)
          (lines_text ~glyphs:(Lines.of_border border) ~width:5 ~height:3
             (fun l ->
               Lines.hline l ~x:0 ~y:0 ~length:5;
               Lines.hline l ~x:0 ~y:2 ~length:5;
               Lines.vline l ~x:0 ~y:0 ~length:3;
               Lines.vline l ~x:4 ~y:0 ~length:3)));
    test "an arm joins a line to a mark beside it" (fun () ->
        equal ~msg:"tick" string " │\n─┤\n │"
          (lines_text ~width:2 ~height:3 (fun l ->
               Lines.vline l ~x:1 ~y:0 ~length:3;
               Lines.arm l ~x:1 ~y:1 `Left;
               Lines.hline l ~x:0 ~y:1 ~length:1)));
    test "a cell takes the style of the first line through it" (fun () ->
        let red = Ansi.Style.make ~fg:Ansi.Color.red ()
        and blue = Ansi.Style.make ~fg:Ansi.Color.blue () in
        let l = Lines.create ~x:0 ~y:0 ~width:3 ~height:3 in
        Lines.hline ~style:red l ~x:0 ~y:1 ~length:3;
        Lines.vline ~style:blue l ~x:1 ~y:0 ~length:3;
        let g = Grid.create ~width:3 ~height:3 () in
        Grid.draw_lines g l;
        let fg x y = Grid.get_fg g (idx g x y) in
        is_true ~msg:"crossing" (Ansi.Color.equal (fg 1 1) Ansi.Color.red);
        is_true ~msg:"vertical" (Ansi.Color.equal (fg 1 0) Ansi.Color.blue);
        Lines.clear l;
        Lines.vline ~style:blue l ~x:1 ~y:0 ~length:3;
        Lines.hline ~style:red l ~x:0 ~y:1 ~length:3;
        let g = Grid.create ~width:3 ~height:3 () in
        Grid.draw_lines g l;
        is_true ~msg:"after clear"
          (Ansi.Color.equal (Grid.get_fg g (idx g 1 1)) Ansi.Color.blue));
    test "lines leaving the rectangle keep their arms" (fun () ->
        equal ~msg:"cut" string "┼"
          (lines_text ~width:1 ~height:1 (fun l ->
               Lines.hline l ~x:(-2) ~y:0 ~length:5;
               Lines.vline l ~x:0 ~y:(-2) ~length:5)));
    test "a wide or composed glyph is drawn as text" (fun () ->
        let set =
          {
            Lines.horizontal = "=";
            vertical = "🟦";
            top_left = "+";
            top_right = "+";
            bottom_left = "+";
            bottom_right = "+";
            top_t = "+";
            bottom_t = "+";
            left_t = "+";
            right_t = "+";
            cross = "+";
          }
        in
        equal ~msg:"wide" string "🟦\n🟦"
          (lines_text ~glyphs:(Lines.Set set) ~width:2 ~height:2 (fun l ->
               Lines.vline l ~x:0 ~y:0 ~length:2)));
    test "a set covers its rectangle from its origin" (fun () ->
        let l = Lines.create ~x:2 ~y:1 ~width:3 ~height:1 in
        Lines.hline l ~x:0 ~y:1 ~length:8;
        Lines.vline l ~x:3 ~y:0 ~length:2;
        Lines.hline l ~x:0 ~y:0 ~length:8;
        let g = Grid.create ~width:8 ~height:2 () in
        Grid.draw_lines g l;
        equal ~msg:"cut to the rectangle, arms kept" string "\n  ─┴─"
          (Grid.to_text g);
        equal ~msg:"origin" (pair int int) (2, 1) (Lines.x l, Lines.y l));
    test "create rejects a negative size" (fun () ->
        raises_match (Exn.invalid_arg ~substring:"Lines.create") (fun () ->
            ignore (Lines.create ~x:0 ~y:0 ~width:(-1) ~height:1)));
  ]

let tests =
  [
    test "spaces draw, layers composite" spaces_draw_and_layers_composite;
    test "to_ansi ends each row with one reset" (fun () ->
        let grid = Grid.create ~width:2 ~height:2 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ab"
          ~style:(Ansi.Style.make ~bold:true ());
        let ansi = Grid.to_ansi grid in
        let reset = "\027[0m" in
        is_true ~msg:"ends with a reset" (String.ends_with ~suffix:reset ansi);
        is_false ~msg:"ends with no second reset"
          (String.ends_with ~suffix:(reset ^ reset) ansi));
    test "draw_box rejects a wide border character" (fun () ->
        let grid = Grid.create ~width:6 ~height:3 () in
        let border =
          { Grid.Border.single with horizontal = Uchar.of_int 0x4E2D }
        in
        raises_match ~msg:"a wide horizontal"
          (Exn.invalid_arg ~substring:"draw_box") (fun () ->
            Grid.draw_box grid ~x:0 ~y:0 ~width:6 ~height:3 ~border ()));
    test "accessors check their index" (fun () ->
        let grid = Grid.create ~width:3 ~height:2 () in
        let buf = Bytes.create 8 in
        List.iter
          (fun idx ->
            let raises name f =
              raises_match ~msg:(Printf.sprintf "%s %d" name idx)
                (Exn.invalid_arg ~substring:name) (fun () ->
                  ignore (f () : string))
            in
            let str f () = string_of_int (f ()) in
            let bool f () = string_of_bool (f ()) in
            raises "get_codepoint" (str (fun () -> Grid.get_codepoint grid idx));
            raises "get_attrs" (str (fun () -> Grid.get_attrs grid idx));
            raises "get_link" (fun () -> Grid.get_link grid idx);
            raises "get_fg" (fun () -> Ansi.Color.to_hex (Grid.get_fg grid idx));
            raises "get_bg" (fun () -> Ansi.Color.to_hex (Grid.get_bg grid idx));
            raises "get_text" (fun () -> Grid.get_text grid idx);
            raises "cell_text_length"
              (str (fun () -> Grid.cell_text_length grid idx));
            raises "blit_cell_text"
              (str (fun () -> Grid.blit_cell_text grid idx buf ~pos:0));
            raises "get_style" (fun () ->
                Format.asprintf "%a" Ansi.Style.pp (Grid.get_style grid idx));
            raises "is_empty" (bool (fun () -> Grid.is_empty grid idx));
            raises "is_continuation"
              (bool (fun () -> Grid.is_continuation grid idx));
            raises "cell_width" (str (fun () -> Grid.cell_width grid idx));
            raises "cells_equal"
              (bool (fun () -> Grid.cells_equal grid idx grid 0));
            raises "is_protected" (bool (fun () -> Grid.is_protected grid idx)))
          [ -1; 6; max_int ]);
    test "draw_text rejects a tab width below 1" (fun () ->
        let grid = Grid.create ~width:4 ~height:1 () in
        raises_match ~msg:"tab width 0" (Exn.invalid_arg ~substring:"tab width")
          (fun () -> Grid.draw_text grid ~x:0 ~y:0 ~tab_width:0 ~text:"\t"));
    test "many distinct graphemes" many_distinct_graphemes;
    test "cell text of empty and continuation cells"
      cell_text_of_empty_and_continuation_cells;
    test "span of every cell" span_of_every_cell;
    test "overwritten hyperlinks are collected"
      overwritten_hyperlinks_are_collected;
    test "collected grids release shared graphemes"
      collected_grid_releases_shared_graphemes;
    test "resized grids keep exact grapheme counts"
      resized_grid_keeps_exact_grapheme_counts;
    test "combining mark stays with ascii base"
      combining_mark_stays_with_ascii_base;
    test "Thai spacing mark cell placement" thai_spacing_mark_cell_placement;
    test "Thai spacing mark wcwidth units" thai_spacing_mark_wcwidth_units;
    test "wide span beyond four cells" wide_span_beyond_four_cells;
    test "translucent wide character has one background"
      translucent_wide_character_has_one_background;
    test "translucent fill tints a wide character"
      translucent_fill_tints_wide_character;
    test "wide span survives moves" wide_span_survives_moves;
    test "max width bounds the grid" max_width_bounds_grid;
    test "one operation, one blank" one_operation_one_blank;
    test "clear_rect clears whole graphemes" clear_rect_clears_whole_graphemes;
    test "a write repairs with the written bg" write_repairs_with_written_bg;
    test "shift moves cells" shift_moves_cells;
    test "shift carries cell contents" shift_carries_cell_contents;
    test "set_grapheme places exactly" set_grapheme_places_exactly;
    test "set_grapheme rejects" set_grapheme_rejects;
    test "protection is invisible" protection_is_invisible;
    test "protection moves with cells" protection_moves_with_cells;
    test "erase_unprotected keeps rendition" erase_unprotected_keeps_rendition;
    test "create defaults" create_defaults;
    test "create with configuration" create_with_configuration;
    test "set width method" set_width_method_updates;
    test "set cell writes all planes" set_uchar_writes_all_planes;
    test "set cell stores hyperlink" set_uchar_records_hyperlink;
    test "set cell outside scissor" set_uchar_outside_scissor_ignored;
    test "with scissor restores" with_scissor_restores_stack;
    test "clear preserves scissor" clear_preserves_scissor_state;
    test "draw text applies style" draw_text_applies_style;
    test "draw text inherits background" draw_text_inherits_existing_background;
    test "draw text skips newline" draw_text_skips_newline;
    test "clear scissor" clear_scissor_allows_future_writes;
    test "set cell blends" set_uchar_honours_blending;
    test "set uchar accumulates alpha" set_uchar_accumulates_alpha;
    test "fill rect" fill_rect_fills_region;
    test "replace wide grapheme clears continuations"
      replace_wide_grapheme_clears_continuations;
    test "replace wide grapheme clears continuation colors"
      replace_wide_grapheme_clears_continuation_colors;
    test "fill rect alpha preserves glyph" fill_rect_alpha_preserves_glyph;
    test "fill rect transparent is noop" fill_rect_transparent_is_noop;
    test "clear_rect resets background" clear_rect_resets_background;
    test "scroll uses transparent background" scroll_uses_transparent_background;
    test "single-row scroll clears only that row"
      scroll_single_row_clears_only_that_row;
    test "scroll keeps multi-codepoint graphemes"
      scroll_keeps_multi_codepoint_graphemes;
    test "draw text overwrites grapheme span" draw_text_overwrite_clears_span;
    test "draw text overflow clears row tail" draw_text_overflow_clears_row_tail;
    test "draw text skips zero-width graphemes" draw_text_skips_zero_width;
    test "blit region skips partial spans" blit_region_skips_partial_span;
    test "fill_rect clears inline wide start" fill_rect_clears_inline_wide_start;
    test "fill_rect clears inline wide continuation"
      fill_rect_clears_inline_wide_continuation;
    test "blit_region clears right-truncated wide start"
      blit_region_clears_right_truncated_wide_start;
    test "blit_region shifts wide over wide" blit_region_shifts_wide_over_wide;
    test "blit_region copies transparent source"
      blit_region_copies_transparent_source;
    test "draw_region skips transparent source"
      draw_region_skips_transparent_source;
    test "blit_region preserves terminal-default foreground glyph"
      blit_region_preserves_default_foreground_glyph;
    test "to_ansi selects complete rows" to_ansi_selects_complete_rows;
    test "to_ansi handles empty glyph" to_ansi_handles_empty_glyph;
    test "copied partial span becomes space" copied_partial_span_becomes_space;
    test "to_ansi at a styling" to_ansi_at_a_styling;
    test "to_ansi trims" to_ansi_trims;
    test "to_ansi preserves indexed foreground"
      to_ansi_preserves_indexed_foreground;
    test "blit preserves indexed background intent"
      blit_preserves_indexed_background_intent;
    test "draw text overflow does nothing" draw_text_overflow_does_nothing;
    test "box left border spans full height"
      draw_box_left_border_spans_full_height;
    test "canvas-like resizing" canvas_like_resizing;
    test "clear" clear_resets_grid;
    test "resize" resize_updates_dimensions;
    test "resize truncated grapheme does not bleed"
      resize_truncated_grapheme_does_not_bleed;
    test "blit" blit_copies_full_buffer;
    test "blit region" blit_region_copies_subrect;
    (* Regression tests for optimization bugs *)
    test "ascii fast path correctness" ascii_fast_path_correctness;
    test "ascii overwrites wide grapheme middle"
      ascii_overwrites_wide_grapheme_middle;
    test "mixed ascii emoji render" mixed_ascii_emoji_render;
    test "clear after graphemes" clear_after_graphemes;
    test "empty string is noop" empty_string_is_noop;
    test "negative x clips text" negative_x_clips_text;
    test "ambiguous width defaults to one" ambiguous_width_defaults_to_one;
    test "canvas-like primitives render" canvas_like_primitives_render;
    test "canvas blit into box" canvas_blit_into_box;
    test "box drawing characters render" box_drawing_characters_render;
    (* Scissor for fast paths *)
    test "draw_text ASCII respects scissor" draw_text_ascii_respects_scissor;
    test "draw_text tab partially visible respects scissor"
      draw_text_tab_partially_visible_respects_scissor;
    test "fill_rect respects scissor" fill_rect_respects_scissor;
    test "blit_region respects scissor" blit_region_respects_scissor;
    test "intersects_clip includes grid bounds"
      intersects_clip_includes_grid_bounds;
    test "intersects_clip observes nested scissors"
      intersects_clip_observes_nested_scissors;
    test "opacity stack grows beyond initial capacity"
      opacity_stack_grows_beyond_initial_capacity;
    test "inherit bg on unwritten ascii" inherit_bg_on_unwritten_ascii;
    test "unicode inherit bg on unwritten cell"
      unicode_inherit_bg_on_unwritten_cell;
    test "overflow respects scissor for wide grapheme"
      overflow_respects_scissor_for_wide_grapheme;
    test "alpha blit orphan continuation draws space"
      alpha_blit_orphan_continuation_draws_space;
    test "cross-store blit remaps graphemes" cross_store_blit_remaps_graphemes;
    test "scissored graphemes do not leak store"
      scissored_graphemes_do_not_leak_store;
    test "transparent graphemes do not leak store"
      transparent_graphemes_do_not_leak_store;
    test "overflowing wide graphemes do not leak store"
      overflowing_wide_graphemes_do_not_leak_store;
    test "shared storage cells equal" shared_storage_cells_equal;
    test "independent storage cells equal" independent_storage_cells_equal;
    test "scalar writes preserve spans" scalar_writes_preserve_spans;
    test "overlapping wide spans" overlapping_wide_spans;
    test "clipped multi-column grapheme" clipped_multi_column_grapheme;
    test "shared storage links equal" shared_storage_links_equal;
    test "grapheme store churn stays balanced"
      grapheme_store_churn_stays_balanced;
    test "copied payloads survive source reuse"
      copied_payloads_survive_source_reuse;
    test "shared store clear keeps sibling payloads"
      shared_store_clear_keeps_sibling_payloads;
    test "same-store bulk blit tracks graphemes" blit_bulk_tracks_graphemes;
    test "overlap blit direction correctness" overlap_blit_direction_correctness;
    test "box title left aligned" box_title_left_aligned;
    test "diff detects single RGB step" diff_detects_single_rgb_step;
    test "alpha blit blends fg and bg" alpha_blit_blends_fg_bg;
    test "resize shrink clips continuation" resize_shrink_clips_continuation;
    test "resize shrink clips stored grapheme"
      resize_shrink_clips_stored_grapheme;
    test "resize keeps stored grapheme" resize_keeps_stored_grapheme;
    test "draw_text blends FG alpha over opaque BG"
      draw_text_blends_fg_alpha_over_opaque_bg;
    test "blit_region copies alpha" blit_region_copies_alpha;
    test "draw_region blends alpha" draw_region_blends_alpha;
    test "scissor push intersects parent" scissor_push_intersects_parent;
    (* Alpha overlay semantics *)
    test "semi-transparent overlay preserves text and tints fg" (fun () ->
        let grid = Grid.create ~width:1 ~height:1 () in
        (* Draw text with white fg on black bg *)
        let white = Ansi.Color.of_rgba 255 255 255 255 in
        let black = Ansi.Color.of_rgba 0 0 0 255 in
        let style = Ansi.Style.make ~fg:white ~bg:black () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
        (* Overlay with semi-transparent red background *)
        let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:semi_red;
        (* Text should be preserved *)
        equal ~msg:"char preserved" int (Char.code 'X') (read_char grid 0 0);
        (* FG should be tinted (white + red overlay) *)
        let r_fg, g_fg, b_fg, a_fg = read_fg grid 0 0 in
        satisfies ~claim:"fg red increased (> 128)" int (fun r -> r > 128) r_fg;
        (* Should be blended *)
        satisfies ~claim:"fg green decreased (< 255)" int
          (fun g -> g < 255)
          g_fg;
        satisfies ~claim:"fg blue decreased (< 255)" int (fun b -> b < 255) b_fg;
        equal ~msg:"fg alpha preserved" int 255 a_fg;
        (* BG should be blended (red over black) *)
        let r_bg, g_bg, b_bg, a_bg = read_bg grid 0 0 in
        is_true ~msg:"bg red blended" (r_bg = 128);
        equal ~msg:"bg green blended" int 0 g_bg;
        equal ~msg:"bg blue blended" int 0 b_bg;
        equal ~msg:"bg alpha stays opaque" int 255 a_bg);
    test "semi-transparent overlay on space doesn't preserve" (fun () ->
        let grid = Grid.create ~width:1 ~height:1 () in
        (* Fill with white background (space) *)
        let white = Ansi.Color.of_rgba 255 255 255 255 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:white;
        (* Overlay with semi-transparent red *)
        let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
        Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:semi_red;
        (* Should remain a space, no text preservation *)
        equal ~msg:"remains space" int (Char.code ' ') (read_char grid 0 0);
        (* BG should be blended (red over white = pink) *)
        let r_bg, g_bg, b_bg, a_bg = read_bg grid 0 0 in
        equal ~msg:"bg red full" int 255 r_bg;
        is_true ~msg:"bg green blended" (g_bg > 0 && g_bg < 255);
        is_true ~msg:"bg blue blended" (b_bg > 0 && b_bg < 255);
        equal ~msg:"bg alpha stays opaque" int 255 a_bg);
    (* Box clipping tests *)
    test "box partially off left edge uses correct corners" (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:(-1) ~y:0 ~width:4 ~height:3 ~border:border_chars
          ();
        (* Box is clipped on left, so no left corners are drawn *)
        equal ~msg:"horizontal at (0,0)" int
          (Uchar.to_int border_chars.horizontal)
          (read_char grid 0 0);
        equal ~msg:"top-right corner at (2,0)" int
          (Uchar.to_int border_chars.top_right)
          (read_char grid 2 0);
        equal ~msg:"horizontal at (0,2)" int
          (Uchar.to_int border_chars.horizontal)
          (read_char grid 0 2);
        equal ~msg:"bottom-right corner at (2,2)" int
          (Uchar.to_int border_chars.bottom_right)
          (read_char grid 2 2));
    test "box partially off top edge extends verticals down" (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:0 ~y:(-1) ~width:3 ~height:4 ~border:border_chars
          ~sides:[ `Top; `Left ] ();
        (* Top not drawn, so verticals should extend to top of screen *)
        equal ~msg:"left border at (0,0)" int
          (Uchar.to_int border_chars.vertical)
          (read_char grid 0 0);
        equal ~msg:"left border at (0,1)" int
          (Uchar.to_int border_chars.vertical)
          (read_char grid 0 1);
        equal ~msg:"left border at (0,2)" int
          (Uchar.to_int border_chars.vertical)
          (read_char grid 0 2));
    test "box partially off right edge uses correct right corners" (fun () ->
        let grid = Grid.create ~width:3 ~height:3 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:1 ~y:0 ~width:3 ~height:3 ~border:border_chars ();
        (* Box extends beyond right edge, so no right corners are drawn *)
        equal ~msg:"horizontal at (2,0)" int
          (Uchar.to_int border_chars.horizontal)
          (read_char grid 2 0);
        equal ~msg:"horizontal at (2,2)" int
          (Uchar.to_int border_chars.horizontal)
          (read_char grid 2 2));
    test "box fully inside grid works normally" (fun () ->
        let grid = Grid.create ~width:5 ~height:5 () in
        let border_chars = Grid.Border.single in
        Grid.draw_box grid ~x:1 ~y:1 ~width:3 ~height:3 ~border:border_chars ();
        equal ~msg:"top-left corner" int
          (Uchar.to_int border_chars.top_left)
          (read_char grid 1 1);
        equal ~msg:"top-right corner" int
          (Uchar.to_int border_chars.top_right)
          (read_char grid 3 1);
        equal ~msg:"bottom-left corner" int
          (Uchar.to_int border_chars.bottom_left)
          (read_char grid 1 3);
        equal ~msg:"bottom-right corner" int
          (Uchar.to_int border_chars.bottom_right)
          (read_char grid 3 3));
    (* Diff tests *)
    test "diff identical grids produces no diffs" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        let diffs = diff_cells a b in
        equal ~msg:"no diffs" int 0 (Array.length diffs));
    test "diff detects single char change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_uchar ~blend:true b ~x:1 ~y:1 ~uchar:(Uchar.of_char 'X')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        let diffs = diff_cells a b in
        equal ~msg:"single diff at changed cell"
          (list (pair int int))
          [ (1, 1) ]
          (Array.to_list diffs));
    test "diff detects single color change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.fill_rect b ~x:0 ~y:0 ~width:1 ~height:1 ~color:Ansi.Color.red;
        let diffs = diff_cells a b in
        equal ~msg:"single diff at colored cell"
          (list (pair int int))
          [ (0, 0) ]
          (Array.to_list diffs));
    test "diff detects hyperlink change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_uchar ~blend:true b ~x:0 ~y:0 ~uchar:(Uchar.of_char 'A')
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty
          ~link:"http://example.com" ();
        let diffs = diff_cells a b in
        equal ~msg:"single diff at linked cell"
          (list (pair int int))
          [ (0, 0) ]
          (Array.to_list diffs));
    (* Resize tests *)
    test "resize preserves overlapping content" (fun () ->
        let grid = Grid.create ~width:4 ~height:2 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"ABCD";
        Grid.draw_text grid ~x:0 ~y:1 ~text:"EFGH";
        (* Resize to smaller - should preserve top-left content *)
        Grid.resize grid ~width:2 ~height:1;
        equal ~msg:"width after resize" int 2 (Grid.width grid);
        equal ~msg:"height after resize" int 1 (Grid.height grid);
        equal ~msg:"preserved content" string "AB" (row_trimmed grid 0));
    test "resize up fills new areas with spaces" (fun () ->
        let grid = Grid.create ~width:2 ~height:1 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"AB";
        Grid.resize grid ~width:4 ~height:2;
        equal ~msg:"width after resize" int 4 (Grid.width grid);
        equal ~msg:"height after resize" int 2 (Grid.height grid);
        equal ~msg:"original content preserved" string "AB  "
          (row_to_string grid 0);
        equal ~msg:"new row is spaces" string "    " (row_to_string grid 1));
    test "resize_clear clears when dimensions change" (fun () ->
        let grid = Grid.create ~width:2 ~height:1 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"AB";
        Grid.resize_clear grid ~width:4 ~height:2;
        equal ~msg:"width after resize_clear" int 4 (Grid.width grid);
        equal ~msg:"height after resize_clear" int 2 (Grid.height grid);
        equal ~msg:"old row cleared" string "    " (row_to_string grid 0);
        equal ~msg:"new row cleared" string "    " (row_to_string grid 1));
    (* Active height *)
    test "active_height counts rows that show only a background" (fun () ->
        let grid = Grid.create ~width:3 ~height:5 () in
        Grid.draw_text grid ~x:0 ~y:0 ~text:"a";
        equal ~msg:"text" int 1 (Grid.active_height grid);
        Grid.fill_rect grid ~x:0 ~y:1 ~width:3 ~height:1 ~color:Ansi.Color.blue;
        equal ~msg:"background" int 2 (Grid.active_height grid);
        Grid.draw_text grid ~x:1 ~y:2 ~text:" "
          ~style:(Ansi.Style.make ~underline:true ());
        equal ~msg:"attribute" int 3 (Grid.active_height grid);
        Grid.draw_text grid ~x:1 ~y:3 ~text:" "
          ~style:(Ansi.Style.make ~link:"http://a" ());
        equal ~msg:"link" int 4 (Grid.active_height grid);
        Grid.fill_rect grid ~x:0 ~y:4 ~width:3 ~height:1
          ~color:Ansi.Color.default_bg;
        Grid.draw_text grid ~x:2 ~y:4 ~text:" "
          ~style:(Ansi.Style.make ~fg:Ansi.Color.red ());
        equal ~msg:"default background and an unseen foreground" int 4
          (Grid.active_height grid));
  ]

(* A picture whose pixels are all [r, g, b], opaque. *)
let solid ~width ~height (r, g, b) =
  Grid.picture ~width ~height
    (String.init
       (4 * width * height)
       (fun i ->
         Char.chr (match i mod 4 with 0 -> r | 1 -> g | 2 -> b | _ -> 255)))

let picture_tests =
  [
    test "picture checks its pixels" (fun () ->
        raises_match (Exn.invalid_arg ~substring:"Matrix_grid.picture")
          (fun () -> ignore (Grid.picture ~width:2 ~height:1 "1234"));
        raises_match (Exn.invalid_arg ~substring:"Matrix_grid.picture")
          (fun () -> ignore (Grid.picture ~width:0 ~height:1 ""));
        equal ~msg:"size" (pair int int) (3, 2)
          (Grid.picture_size (solid ~width:3 ~height:2 (0, 0, 0))));
    test "a picture cell reads as U+FFFC and keeps its background" (fun () ->
        let g = Grid.create ~width:4 ~height:2 () in
        Grid.fill_rect g ~x:0 ~y:0 ~width:4 ~height:2 ~color:Ansi.Color.blue;
        Grid.push_clip g { x = 0; y = 0; width = 3; height = 2 };
        Grid.draw_picture g ~x:1 ~y:0 ~width:3 ~height:1
          (solid ~width:1 ~height:1 (255, 0, 0));
        equal ~msg:"text" string " \u{FFFC}\u{FFFC}\n" (Grid.to_text g);
        equal ~msg:"cut by the scissor" string " " (Grid.get_text g 3);
        is_true ~msg:"background kept"
          (Ansi.Color.equal Ansi.Color.blue (Grid.get_bg g 1));
        equal ~msg:"one cell wide" int 1 (Grid.cell_width g 1));
    test "a picture cell is text to the other operations" (fun () ->
        let p = solid ~width:2 ~height:2 (0, 255, 0) in
        let a = Grid.create ~width:3 ~height:1 () in
        Grid.draw_text a ~x:0 ~y:0 ~text:"\u{4E00}x";
        Grid.draw_picture a ~x:1 ~y:0 ~width:2 ~height:1 p;
        equal ~msg:"a cut wide character becomes a blank" string
          " \u{FFFC}\u{FFFC}" (Grid.to_text a);
        let b = Grid.create ~width:3 ~height:1 () in
        Grid.blit ~src:a ~dst:b;
        is_true ~msg:"a copy to another storage is equal"
          (Grid.cells_equal a 1 b 1);
        is_false ~msg:"tiles differ by place" (Grid.cells_equal a 1 b 2);
        Grid.fill_rect a ~x:0 ~y:0 ~width:3 ~height:1
          ~color:(Ansi.Color.of_rgba 255 0 0 128);
        equal ~msg:"a translucent fill keeps the tile" string
          " \u{FFFC}\u{FFFC}" (Grid.to_text a);
        Grid.draw_text a ~x:1 ~y:0 ~text:"y";
        equal ~msg:"text replaces a tile" string " y\u{FFFC}" (Grid.to_text a);
        is_false ~msg:"another picture differs"
          (let c = Grid.copy b in
           Grid.draw_picture c ~x:1 ~y:0 ~width:2 ~height:1
             (solid ~width:2 ~height:2 (0, 255, 0));
           Grid.cells_equal b 1 c 1));
    test "a picture is let go with its last cell" (fun () ->
        let g = Grid.create ~width:2 ~height:1 () in
        let collected = ref false in
        (let p = solid ~width:64 ~height:64 (1, 2, 3) in
         Gc.finalise_last (fun () -> collected := true) p;
         Grid.draw_picture g ~x:0 ~y:0 ~width:2 ~height:1 p);
        Grid.clear g;
        Gc.full_major ();
        is_true ~msg:"collected" !collected);
  ]

let () =
  run "matrix.grid"
    [
      group "grid" tests;
      group "lines" lines_tests;
      group "pictures" picture_tests;
    ]
