open Windtrap

(* Helper to calculate linear index *)
let idx grid x y = (y * Grid.width grid) + x

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

let contains_substring s sub =
  let len = String.length s in
  let sublen = String.length sub in
  if sublen = 0 then true
  else if sublen > len then false
  else
    let rec matches_at i j =
      j = sublen
      || String.unsafe_get s (i + j) = String.unsafe_get sub j
         && matches_at i (j + 1)
    in
    let rec loop i = i <= len - sublen && (matches_at i 0 || loop (i + 1)) in
    loop 0

let row_trimmed grid y = trim_right (row_to_string grid y)

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
  let src = Grid.create ~width:3 ~height:1 ~respect_alpha:true () in
  let style = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 0 255 0 128) () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中" ~style;
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit_region ~src ~dst ~src_x:1 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
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

let blit_preserves_respect_alpha () =
  let src = Grid.create ~width:2 ~height:2 ~respect_alpha:true () in
  let dst = Grid.create ~width:2 ~height:2 () in
  Grid.blit ~src ~dst;
  is_true ~msg:"respect alpha copied" (Grid.respect_alpha dst)

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
  (* Instead of hacking raw arrays, we set the cell with a minimal color
     difference. 1/255 is approx 0.0039, which is > the internal epsilon
     (0.00001). *)
  let minimal_diff_color = Ansi.Color.of_rgba 1 1 1 1 in
  Grid.set_cell b ~x:0 ~y:0 ~cell:(Grid.get_cell a 0) ~fg:Ansi.Color.white
    ~bg:minimal_diff_color ~attrs:Ansi.Attr.empty ();
  let diffs = Grid.diff_cells a b in
  equal ~msg:"diffs include cell when RGB changes by 1 step"
    (list (pair int int))
    [ (0, 0) ]
    (Array.to_list diffs)

let alpha_blit_blends_fg_bg () =
  let src = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  let dst = Grid.create ~width:1 ~height:1 () in
  let blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:blue;
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  let style = Ansi.Style.make ~fg:semi_red ~bg:semi_green () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"C" ~style;
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r_bg, g_bg, b_bg, a_bg = read_bg dst 0 0 in
  (* Validate qualitative blend: green increases from 0, blue decreases from
     255. Background alpha should match the overlay value (128) rather than
     compositing with the destination alpha. *)
  is_true ~msg:"bg green increased" (g_bg > 0);
  is_true ~msg:"bg blue decreased" (b_bg < 255);
  equal ~msg:"bg red stays 0" int 0 r_bg;
  equal ~msg:"bg alpha is 128" int 128 a_bg;
  let r_fg, _g_fg, _b_fg, a_fg = read_fg dst 0 0 in
  (* With src FG alpha resolved to 0 in the source buffer, FG tint contribution
     is zero; alpha uses destination background (255). *)
  equal ~msg:"fg red stays 0" int 0 r_fg;
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
  is_false ~msg:"respect alpha" (Grid.respect_alpha grid);
  is_true ~msg:"width method" (Grid.width_method grid = `Unicode)

let create_with_configuration () =
  let grid =
    Grid.create ~width:1 ~height:1 ~width_method:`Wcwidth ~respect_alpha:true ()
  in
  is_true ~msg:"width method" (Grid.width_method grid = `Wcwidth);
  is_true ~msg:"respect alpha" (Grid.respect_alpha grid)

let set_width_method_updates () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_width_method grid `Wcwidth;
  is_true ~msg:"updated" (Grid.width_method grid = `Wcwidth)

let set_respect_alpha_updates () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_respect_alpha grid true;
  is_true ~msg:"updated" (Grid.respect_alpha grid)

let set_cell_writes_all_planes () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let attrs = Ansi.Attr.bold in
  Grid.set_cell ~blend:true grid ~x:1 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'A'))
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

let set_cell_outside_scissor_ignored () =
  let grid = Grid.create ~width:2 ~height:2 () in
  Grid.push_clip grid { x = 1; y = 1; width = 1; height = 1 };
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'X'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  Grid.pop_clip grid;
  equal ~msg:"char remains empty" int 32 (read_char grid 0 0)

let with_scissor_restores_stack () =
  let grid = Grid.create ~width:2 ~height:2 () in
  let result =
    Grid.clip grid { x = 0; y = 0; width = 1; height = 1 } (fun () ->
        Grid.set_cell ~blend:true grid ~x:0 ~y:0
          ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'Y'))
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        42)
  in
  equal ~msg:"scoped result" int 42 result;
  equal ~msg:"inside write" int (Char.code 'Y') (read_char grid 0 0);
  (* After scope, writing outside should succeed. *)
  Grid.set_cell ~blend:true grid ~x:1 ~y:1
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'Z'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  equal ~msg:"char set" int (Char.code 'Z') (read_char grid 1 1)

let set_cell_records_hyperlink () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'L'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty
    ~link:"https://example.com" ();
  (* Use the new zero-alloc accessor to get the ID *)
  let id = Grid.get_link grid 0 in
  match Grid.hyperlink_url grid id with
  | Some url -> equal ~msg:"link stored" string "https://example.com" url
  | None -> failwith "expected hyperlink"

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
    Ansi.Color.blend ~src:bg_color ~dst:Ansi.Color.default ()
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

let set_cell_honours_blending () =
  let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'B'))
    ~fg:Ansi.Color.blue ~bg:Ansi.Color.blue ~attrs:Ansi.Attr.empty ();
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  let semi_green = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'C'))
    ~fg:semi_red ~bg:semi_green ~attrs:Ansi.Attr.empty ();
  let expected_fg = Ansi.Color.blend ~src:semi_red ~dst:Ansi.Color.blue () in
  (* Background alpha is replaced by the overlay alpha instead of being blended,
     matching the renderer's contract for translucent backgrounds. *)
  let expected_bg =
    let blended = Ansi.Color.blend ~src:semi_green ~dst:Ansi.Color.blue () in
    let r, g, b, _ = Ansi.Color.to_rgba blended in
    (* overlay alpha = 128 *)
    Ansi.Color.of_rgba r g b 128
  in
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

let set_cell_without_respect_still_blends () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'C'))
    ~fg:semi_red ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  equal ~msg:"char" int (Char.code 'C') (read_char grid 0 0);
  equal ~msg:"width" int 1 (read_width grid 0 0);
  (* {!Grid.set_cell ~blend:true} always blends and keeps the destination alpha
     for the foreground channel, so the default background alpha (0)
     persists. *)
  let expected =
    let blended = Ansi.Color.blend ~src:semi_red ~dst:Ansi.Color.default () in
    let r, g, b, _ = Ansi.Color.to_rgba blended in
    (r, (g, (b, 0)))
  in
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
  is_true ~msg:"blue component preserved" (b > 0);
  is_true ~msg:"red component applied" (r > 0)

let blit_region_copies_alpha_when_source_ignores_alpha () =
  let src = Grid.create ~width:1 ~height:1 () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_cell src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'R'))
    ~fg:Ansi.Color.white ~bg:semi_red ~attrs:Ansi.Attr.empty ();
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

let blit_region_blends_when_source_respects_alpha () =
  let src = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  let semi_red = Ansi.Color.of_rgba 255 0 0 128 in
  Grid.set_cell ~blend:false src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'R'))
    ~fg:Ansi.Color.white ~bg:semi_red ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  let opaque_blue = Ansi.Color.of_rgba 0 0 255 255 in
  Grid.fill_rect dst ~x:0 ~y:0 ~width:1 ~height:1 ~color:opaque_blue;
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  let r, _g, b, a = read_bg dst 0 0 in
  equal ~msg:"char copied" int (Char.code 'R') (read_char dst 0 0);
  is_true ~msg:"background blended keeps blue" (b > 0 && b < 255);
  is_true ~msg:"background blended adds red" (r > 0 && r < 255);
  equal ~msg:"matrix blend alpha policy" int 128 a

let scissor_push_intersects_parent () =
  let grid = Grid.create ~width:4 ~height:1 () in
  (* Parent scissor clips to first cell *)
  Grid.push_clip grid { x = 0; y = 0; width = 1; height = 1 };
  (* Child scissor outside parent should intersect to empty, so writes are
     clipped *)
  Grid.push_clip grid { x = 2; y = 0; width = 1; height = 1 };
  Grid.set_cell ~blend:true grid ~x:2 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'B'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  (* Pop child, write inside parent *)
  Grid.pop_clip grid;
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'A'))
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
  Grid.set_cell ~blend:true grid ~x:1 ~y:1
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'W'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  equal ~msg:"write succeeded" int (Char.code 'W') (read_char grid 1 1)

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
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'A'))
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
  equal ~msg:"continuation bg reset" rgba
    (0, (0, (0, 0)))
    (let r, g, b, a = read_bg grid 1 0 in
     (r, (g, (b, a))));
  equal ~msg:"continuation fg reset" rgba
    (255, (255, (255, 255)))
    (let r, g, b, a = read_fg grid 1 0 in
     (r, (g, (b, a))))

let fill_rect_alpha_preserves_glyph () =
  let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'X'))
    ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let overlay = Ansi.Color.of_rgba 0 255 0 128 in
  Grid.fill_rect grid ~x:0 ~y:0 ~width:1 ~height:1 ~color:overlay;
  equal ~msg:"char preserved" int (Char.code 'X') (read_char grid 0 0);
  (* BG alpha becomes overlay alpha with RGB blended against dest BG. *)
  let blended_bg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.black () in
  let er, eg, eb, _ = Ansi.Color.to_rgba blended_bg in
  let ea = 128 in
  let r, g, b, a = read_bg grid 0 0 in
  equal ~msg:"background blended" rgba (er, (eg, (eb, ea))) (r, (g, (b, a)));
  let expected_fg = Ansi.Color.blend ~src:overlay ~dst:Ansi.Color.white () in
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
  Grid.scroll grid ~top:0 ~bottom:1 1;
  (* Bottom row is newly cleared; its background should stay transparent. *)
  let r, g, b, a = read_bg grid 0 1 in
  equal ~msg:"transparent bg" rgba (0, (0, (0, 0))) (r, (g, (b, a)));
  equal ~msg:"space char" int (Char.code ' ') (read_char grid 0 1)

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

let blit_region_copies_transparent_source_without_respect_alpha () =
  let src = Grid.create ~width:1 ~height:1 () in
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.set_cell src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'T'))
    ~fg:transparent ~bg:transparent ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_text dst ~x:0 ~y:0 ~text:"D";
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"transparent source copied" int (Char.code 'T') (read_char dst 0 0);
  let _r, _g, _b, a = read_bg dst 0 0 in
  equal ~msg:"transparent background copied" int 0 a

let blit_region_skips_transparent_source_with_respect_alpha () =
  let src = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
  let transparent = Ansi.Color.of_rgba 0 0 0 0 in
  Grid.set_cell ~blend:false src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'T'))
    ~fg:transparent ~bg:transparent ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.draw_text dst ~x:0 ~y:0 ~text:"D";
  Grid.blit_region ~src ~dst ~src_x:0 ~src_y:0 ~width:1 ~height:1 ~dst_x:0
    ~dst_y:0;
  equal ~msg:"transparent source skipped" int (Char.code 'D')
    (read_char dst 0 0)

let to_ansi_handles_empty_glyph () =
  let grid = Grid.create ~width:1 ~height:1 () in
  Grid.set_cell grid ~x:0 ~y:0 ~cell:Grid.Cell.empty ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let ansi = Grid.to_ansi ~reset:false grid in
  is_true ~msg:"serialized empty glyph as space" (String.contains ansi ' ')

let to_ansi_handles_orphan_continuation () =
  let src = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text src ~x:0 ~y:0 ~text:"中";
  let grid = Grid.create ~width:1 ~height:1 () in
  let cont = Grid.get_cell src (Grid.idx src ~x:1 ~y:0) in
  Grid.set_cell grid ~x:0 ~y:0 ~cell:cont ~fg:Ansi.Color.white
    ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
  let ansi = Grid.to_ansi ~reset:false grid in
  is_true ~msg:"serialized orphan continuation as space"
    (String.contains ansi ' ')

let to_ansi_preserves_indexed_foreground () =
  let grid = Grid.create ~width:1 ~height:1 () in
  let style = Ansi.Style.make ~fg:(Ansi.Color.indexed 42) () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"X" ~style;
  let ansi = Grid.to_ansi ~reset:false grid in
  is_true ~msg:"indexed foreground serialized as palette SGR"
    (contains_substring ansi "\027[0;38;5;42m")

let blit_preserves_indexed_background_intent () =
  let src = Grid.create ~width:1 ~height:1 () in
  Grid.set_cell src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'X'))
    ~fg:Ansi.Color.white ~bg:(Ansi.Color.indexed 99) ~attrs:Ansi.Attr.empty ();
  let dst = Grid.create ~width:1 ~height:1 () in
  Grid.blit ~src ~dst;
  let ansi = Grid.to_ansi ~reset:false dst in
  is_true ~msg:"indexed background survives blit"
    (contains_substring ansi "48;5;99")

let draw_text_overflow_does_nothing () =
  let grid = Grid.create ~width:2 ~height:1 () in
  Grid.draw_text grid ~x:1 ~y:0 ~text:"中";
  (* Wide graphemes that overflow are discarded by clearing to end-of-line with
     styled spaces, preventing partially drawn clusters. *)
  equal ~msg:"overflow clears to space" int (Char.code ' ') (read_char grid 1 0);
  equal ~msg:"width set to 1" int 1 (read_width grid 1 0)

let ambiguous_width_defaults_to_one () =
  let check_width label s =
    let width = Text.measure ~width_method:`Unicode ~tab_width:2 s in
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
      Text.measure ~width_method:(Grid.width_method grid) ~tab_width:2 text
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
  Grid.set_cell ~blend:true grid ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'Q'))
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
  Grid.set_cell ~blend:true src ~x:0 ~y:0
    ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'A'))
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
      Grid.set_cell ~blend:true src ~x ~y
        ~cell:(Grid.Cell.of_uchar (Uchar.of_int code))
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
  is_true ~msg:"opacity product tracks all pushes"
    (Float.abs (Grid.current_opacity grid -. expected) < 1e-18);
  for _ = 1 to 40 do
    Grid.pop_opacity grid
  done;
  is_true ~msg:"opacity restores to 1 after balanced pops"
    (Float.abs (Grid.current_opacity grid -. 1.0) < 1e-18)

let clear_preserves_scissor_state () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.push_clip grid { x = 1; y = 0; width = 1; height = 1 };
  Grid.clear grid;
  Grid.draw_text grid ~x:0 ~y:0 ~text:"A";
  Grid.pop_clip grid;
  equal ~msg:"write outside preserved scissor ignored" int 32
    (read_char grid 0 0)

let draw_text_overflow_clears_row_tail () =
  let grid = Grid.create ~width:3 ~height:1 () in
  Grid.draw_text grid ~x:0 ~y:0 ~text:"XYZ";
  Grid.draw_text grid ~x:2 ~y:0 ~text:"中";
  equal ~msg:"overflow start cleared" int (Char.code ' ') (read_char grid 2 0);
  equal ~msg:"prefix preserved" string "XY"
    (String.sub (row_to_string grid 0) 0 2)

let tests =
  [
    test "create defaults" create_defaults;
    test "create with configuration" create_with_configuration;
    test "set width method" set_width_method_updates;
    test "set respect alpha" set_respect_alpha_updates;
    test "set cell writes all planes" set_cell_writes_all_planes;
    test "set cell stores hyperlink" set_cell_records_hyperlink;
    test "set cell outside scissor" set_cell_outside_scissor_ignored;
    test "with scissor restores" with_scissor_restores_stack;
    test "clear preserves scissor" clear_preserves_scissor_state;
    test "draw text applies style" draw_text_applies_style;
    test "draw text inherits background" draw_text_inherits_existing_background;
    test "draw text skips newline" draw_text_skips_newline;
    test "clear scissor" clear_scissor_allows_future_writes;
    test "set cell blends" set_cell_honours_blending;
    test "set cell without respect_alpha still blends"
      set_cell_without_respect_still_blends;
    test "fill rect" fill_rect_fills_region;
    test "replace wide grapheme clears continuations"
      replace_wide_grapheme_clears_continuations;
    test "replace wide grapheme clears continuation colors"
      replace_wide_grapheme_clears_continuation_colors;
    test "fill rect alpha preserves glyph" fill_rect_alpha_preserves_glyph;
    test "fill rect transparent is noop" fill_rect_transparent_is_noop;
    test "clear_rect resets background" clear_rect_resets_background;
    test "scroll uses transparent background" scroll_uses_transparent_background;
    test "draw text overwrites grapheme span" draw_text_overwrite_clears_span;
    test "draw text overflow clears row tail" draw_text_overflow_clears_row_tail;
    test "blit region skips partial spans" blit_region_skips_partial_span;
    test "fill_rect clears inline wide start" fill_rect_clears_inline_wide_start;
    test "fill_rect clears inline wide continuation"
      fill_rect_clears_inline_wide_continuation;
    test "blit_region clears right-truncated wide start"
      blit_region_clears_right_truncated_wide_start;
    test "blit_region copies transparent source without respect_alpha"
      blit_region_copies_transparent_source_without_respect_alpha;
    test "blit_region skips transparent source with respect_alpha"
      blit_region_skips_transparent_source_with_respect_alpha;
    test "to_ansi handles empty glyph" to_ansi_handles_empty_glyph;
    test "to_ansi handles orphan continuation"
      to_ansi_handles_orphan_continuation;
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
    test "blit preserves respect alpha" blit_preserves_respect_alpha;
    test "same-store bulk blit tracks graphemes" blit_bulk_tracks_graphemes;
    test "overlap blit direction correctness" overlap_blit_direction_correctness;
    test "box title left aligned" box_title_left_aligned;
    test "diff detects single RGB step" diff_detects_single_rgb_step;
    test "alpha blit blends fg and bg" alpha_blit_blends_fg_bg;
    test "resize shrink clips continuation" resize_shrink_clips_continuation;
    test "resize shrink clips stored grapheme"
      resize_shrink_clips_stored_grapheme;
    test "draw_text blends FG alpha over opaque BG"
      draw_text_blends_fg_alpha_over_opaque_bg;
    test "blit_region copies alpha when source ignores alpha"
      blit_region_copies_alpha_when_source_ignores_alpha;
    test "blit_region blends when source respects alpha"
      blit_region_blends_when_source_respects_alpha;
    test "scissor push intersects parent" scissor_push_intersects_parent;
    (* Alpha overlay semantics *)
    test "semi-transparent overlay preserves text and tints fg" (fun () ->
        let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
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
        is_true ~msg:"fg red increased" (r_fg > 128);
        (* Should be blended *)
        is_true ~msg:"fg green decreased" (g_fg < 255);
        is_true ~msg:"fg blue decreased" (b_fg < 255);
        equal ~msg:"fg alpha preserved" int 255 a_fg;
        (* BG should be blended (red over black) *)
        let r_bg, g_bg, b_bg, a_bg = read_bg grid 0 0 in
        is_true ~msg:"bg red blended" (r_bg > 128 && r_bg < 255);
        equal ~msg:"bg green blended" int 0 g_bg;
        equal ~msg:"bg blue blended" int 0 b_bg;
        equal ~msg:"bg alpha is overlay" int 128 a_bg);
    test "semi-transparent overlay on space doesn't preserve" (fun () ->
        let grid = Grid.create ~width:1 ~height:1 ~respect_alpha:true () in
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
        equal ~msg:"bg alpha is overlay" int 128 a_bg);
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
        let diffs = Grid.diff_cells a b in
        equal ~msg:"no diffs" int 0 (Array.length diffs));
    test "diff detects single char change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_cell ~blend:true b ~x:1 ~y:1
          ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'X'))
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty ();
        let diffs = Grid.diff_cells a b in
        equal ~msg:"single diff at changed cell"
          (list (pair int int))
          [ (1, 1) ]
          (Array.to_list diffs));
    test "diff detects single color change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.fill_rect b ~x:0 ~y:0 ~width:1 ~height:1 ~color:Ansi.Color.red;
        let diffs = Grid.diff_cells a b in
        equal ~msg:"single diff at colored cell"
          (list (pair int int))
          [ (0, 0) ]
          (Array.to_list diffs));
    test "diff detects hyperlink change" (fun () ->
        let a = Grid.create ~width:2 ~height:2 () in
        let b = Grid.copy a in
        Grid.set_cell ~blend:true b ~x:0 ~y:0
          ~cell:(Grid.Cell.of_uchar (Uchar.of_char 'A'))
          ~fg:Ansi.Color.white ~bg:Ansi.Color.black ~attrs:Ansi.Attr.empty
          ~link:"http://example.com" ();
        let diffs = Grid.diff_cells a b in
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
  ]

let () = run "matrix.grid" [ group "grid" tests ]
