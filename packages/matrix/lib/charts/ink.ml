(* Ink: the cells of a plot as marks paint them, before they reach a grid.

   A cell has two planes, a glyph with its foreground and attributes, and a
   background. A write replaces the plane it paints, except that dots of the
   resolution, color and attributes already in the cell merge into its glyph.
   Each plane remembers the mark and row that last wrote it (its owner), which
   hit-testing reads. *)

type res = Half | Quadrant | Sextant | Braille | Octant

let cols = function Half -> 1 | Quadrant | Sextant | Braille | Octant -> 2

let rows = function
  | Half | Quadrant -> 2
  | Sextant -> 3
  | Braille | Octant -> 4

let res_code = function
  | Half -> 1
  | Quadrant -> 2
  | Sextant -> 3
  | Braille -> 4
  | Octant -> 5

(* Glyph plane kinds. *)
let k_none = 0
let k_dots = 1
let k_text = 2
let k_cont = 3

type t = {
  width : int;
  height : int;
  kind : Bytes.t;
  res : Bytes.t;
  bits : int array;
  text : string array;
  span : int array;
  fg : int array;
  attrs : int array;
  gowner : int array;
  has_bg : Bytes.t;
  bg : int array;
  bowner : int array;
}

let create ~width ~height =
  let width = max 0 width and height = max 0 height in
  let n = width * height in
  {
    width;
    height;
    kind = Bytes.make n '\000';
    res = Bytes.make n '\000';
    bits = Array.make n 0;
    text = Array.make n "";
    span = Array.make n 1;
    fg = Array.make n 0;
    attrs = Array.make n 0;
    gowner = Array.make n (-1);
    has_bg = Bytes.make n '\000';
    bg = Array.make n 0;
    bowner = Array.make n (-1);
  }

let width t = t.width
let height t = t.height
let owner ~mark ~row = (mark lsl 40) lor row
let owner_mark o = o lsr 40
let owner_row o = o land ((1 lsl 40) - 1)
let inside t x y = x >= 0 && y >= 0 && x < t.width && y < t.height
let kind t i = Char.code (Bytes.unsafe_get t.kind i)
let color_int (c : Ansi.Color.t) = Ansi.Color.Packed.encode c
let attrs_int (a : Ansi.Attr.t) = Ansi.Attr.pack a

(* A write over part of a wide text clears the rest of it. *)
let clear_span t i =
  match kind t i with
  | k when k = k_text && t.span.(i) > 1 ->
      for j = 1 to t.span.(i) - 1 do
        Bytes.unsafe_set t.kind (i + j) '\000'
      done
  | k when k = k_cont ->
      let j = ref i in
      while !j > 0 && kind t !j = k_cont do
        Bytes.unsafe_set t.kind !j '\000';
        decr j
      done;
      Bytes.unsafe_set t.kind !j '\000'
  | _ -> ()

let dot t r ~x ~y ~fg ~attrs ~owner =
  if x >= 0 && y >= 0 then
    (* the cell and the dot in it, without dividing by 2 or 4 *)
    let c = cols r in
    let cx = if c = 2 then x lsr 1 else x
    and sx = if c = 2 then x land 1 else 0 in
    let cy, sy =
      match r with
      | Half | Quadrant -> (y lsr 1, y land 1)
      | Braille | Octant -> (y lsr 2, y land 3)
      | Sextant -> (y / 3, y mod 3)
    in
    if cx < t.width && cy < t.height then begin
      let i = (cy * t.width) + cx in
      let bit = 1 lsl ((sy * c) + sx) in
      let code = res_code r in
      let fg = color_int fg and attrs = attrs_int attrs in
      if
        kind t i = k_dots
        && Char.code (Bytes.unsafe_get t.res i) = code
        && t.fg.(i) = fg
        && t.attrs.(i) = attrs
      then t.bits.(i) <- t.bits.(i) lor bit
      else begin
        clear_span t i;
        Bytes.unsafe_set t.kind i (Char.unsafe_chr k_dots);
        Bytes.unsafe_set t.res i (Char.unsafe_chr code);
        t.bits.(i) <- bit;
        t.fg.(i) <- fg;
        t.attrs.(i) <- attrs
      end;
      t.gowner.(i) <- owner
    end

let set_text t i s w ~fg ~attrs ~owner =
  clear_span t i;
  Bytes.unsafe_set t.kind i (Char.unsafe_chr k_text);
  t.text.(i) <- s;
  t.span.(i) <- w;
  t.fg.(i) <- fg;
  t.attrs.(i) <- attrs;
  t.gowner.(i) <- owner;
  for j = 1 to w - 1 do
    clear_span t (i + j);
    Bytes.unsafe_set t.kind (i + j) (Char.unsafe_chr k_cont);
    t.gowner.(i + j) <- owner
  done

let text t ~width_method ~x ~y s ~fg ~attrs ~owner =
  if y >= 0 && y < t.height then begin
    let fg = color_int fg and attrs = attrs_int attrs in
    let col = ref x in
    Matrix_text.iter_units ~width_method ~tab_width:1
      (fun ~offset ~len ~width:w ->
        let c = !col in
        col := c + w;
        if c >= 0 && c + w <= t.width then
          set_text t
            ((y * t.width) + c)
            (String.sub s offset len) w ~fg ~attrs ~owner)
      s
  end

let glyph t ~x ~y s ~fg ~attrs ~owner =
  if inside t x y then
    set_text t
      ((y * t.width) + x)
      s 1 ~fg:(color_int fg) ~attrs:(attrs_int attrs) ~owner

let fill t ~x ~y ~bg ~owner =
  if inside t x y then begin
    let i = (y * t.width) + x in
    Bytes.unsafe_set t.has_bg i '\001';
    t.bg.(i) <- color_int bg;
    t.bowner.(i) <- owner
  end

let glyph_owner t ~x ~y =
  if not (inside t x y) then -1
  else
    let i = (y * t.width) + x in
    if kind t i <> k_none then t.gowner.(i) else -1

let owner_at t ~x ~y =
  if not (inside t x y) then -1
  else
    let i = (y * t.width) + x in
    if kind t i <> k_none then t.gowner.(i)
    else if Bytes.unsafe_get t.has_bg i <> '\000' then t.bowner.(i)
    else -1

(* {1 Glyphs} *)

let half = [| 0x20; 0x2580; 0x2584; 0x2588 |]

let quadrant =
  [|
    0x20;
    0x2598;
    0x259D;
    0x2580;
    0x2596;
    0x258C;
    0x259E;
    0x259B;
    0x2597;
    0x259A;
    0x2590;
    0x259C;
    0x2584;
    0x2599;
    0x259F;
    0x2588;
  |]

let sextant n =
  if n = 0 then 0x20
  else if n = 63 then 0x2588
  else if n = 21 then 0x258C
  else if n = 42 then 0x2590
  else 0x1FB00 + n - 1 - (if n > 21 then 1 else 0) - if n > 42 then 1 else 0

(* Bit [2r + c] of a cell's dots is the dot of row [r] and column [c]; a
   braille pattern numbers its dots column by column (U+2800). *)
let braille =
  let map = [| 0x01; 0x08; 0x02; 0x10; 0x04; 0x20; 0x40; 0x80 |] in
  Array.init 256 (fun n ->
      let b = ref 0 in
      for i = 0 to 7 do
        if n land (1 lsl i) <> 0 then b := !b lor map.(i)
      done;
      0x2800 + !b)

(* The octants are U+1CD00 to U+1CDE5 in the order of their patterns, less
   the 26 patterns Unicode encodes elsewhere as blocks, quadrants and quarter
   blocks (the U+1CC00 chart). *)
let octant =
  [|
    0x20;
    0x1CEA8;
    0x1CEAB;
    0x1FB82;
    0x1CD00;
    0x2598;
    0x1CD01;
    0x1CD02;
    0x1CD03;
    0x1CD04;
    0x259D;
    0x1CD05;
    0x1CD06;
    0x1CD07;
    0x1CD08;
    0x2580;
    0x1CD09;
    0x1CD0A;
    0x1CD0B;
    0x1CD0C;
    0x1FBE6;
    0x1CD0D;
    0x1CD0E;
    0x1CD0F;
    0x1CD10;
    0x1CD11;
    0x1CD12;
    0x1CD13;
    0x1CD14;
    0x1CD15;
    0x1CD16;
    0x1CD17;
    0x1CD18;
    0x1CD19;
    0x1CD1A;
    0x1CD1B;
    0x1CD1C;
    0x1CD1D;
    0x1CD1E;
    0x1CD1F;
    0x1FBE7;
    0x1CD20;
    0x1CD21;
    0x1CD22;
    0x1CD23;
    0x1CD24;
    0x1CD25;
    0x1CD26;
    0x1CD27;
    0x1CD28;
    0x1CD29;
    0x1CD2A;
    0x1CD2B;
    0x1CD2C;
    0x1CD2D;
    0x1CD2E;
    0x1CD2F;
    0x1CD30;
    0x1CD31;
    0x1CD32;
    0x1CD33;
    0x1CD34;
    0x1CD35;
    0x1FB85;
    0x1CEA3;
    0x1CD36;
    0x1CD37;
    0x1CD38;
    0x1CD39;
    0x1CD3A;
    0x1CD3B;
    0x1CD3C;
    0x1CD3D;
    0x1CD3E;
    0x1CD3F;
    0x1CD40;
    0x1CD41;
    0x1CD42;
    0x1CD43;
    0x1CD44;
    0x2596;
    0x1CD45;
    0x1CD46;
    0x1CD47;
    0x1CD48;
    0x258C;
    0x1CD49;
    0x1CD4A;
    0x1CD4B;
    0x1CD4C;
    0x259E;
    0x1CD4D;
    0x1CD4E;
    0x1CD4F;
    0x1CD50;
    0x259B;
    0x1CD51;
    0x1CD52;
    0x1CD53;
    0x1CD54;
    0x1CD55;
    0x1CD56;
    0x1CD57;
    0x1CD58;
    0x1CD59;
    0x1CD5A;
    0x1CD5B;
    0x1CD5C;
    0x1CD5D;
    0x1CD5E;
    0x1CD5F;
    0x1CD60;
    0x1CD61;
    0x1CD62;
    0x1CD63;
    0x1CD64;
    0x1CD65;
    0x1CD66;
    0x1CD67;
    0x1CD68;
    0x1CD69;
    0x1CD6A;
    0x1CD6B;
    0x1CD6C;
    0x1CD6D;
    0x1CD6E;
    0x1CD6F;
    0x1CD70;
    0x1CEA0;
    0x1CD71;
    0x1CD72;
    0x1CD73;
    0x1CD74;
    0x1CD75;
    0x1CD76;
    0x1CD77;
    0x1CD78;
    0x1CD79;
    0x1CD7A;
    0x1CD7B;
    0x1CD7C;
    0x1CD7D;
    0x1CD7E;
    0x1CD7F;
    0x1CD80;
    0x1CD81;
    0x1CD82;
    0x1CD83;
    0x1CD84;
    0x1CD85;
    0x1CD86;
    0x1CD87;
    0x1CD88;
    0x1CD89;
    0x1CD8A;
    0x1CD8B;
    0x1CD8C;
    0x1CD8D;
    0x1CD8E;
    0x1CD8F;
    0x2597;
    0x1CD90;
    0x1CD91;
    0x1CD92;
    0x1CD93;
    0x259A;
    0x1CD94;
    0x1CD95;
    0x1CD96;
    0x1CD97;
    0x2590;
    0x1CD98;
    0x1CD99;
    0x1CD9A;
    0x1CD9B;
    0x259C;
    0x1CD9C;
    0x1CD9D;
    0x1CD9E;
    0x1CD9F;
    0x1CDA0;
    0x1CDA1;
    0x1CDA2;
    0x1CDA3;
    0x1CDA4;
    0x1CDA5;
    0x1CDA6;
    0x1CDA7;
    0x1CDA8;
    0x1CDA9;
    0x1CDAA;
    0x1CDAB;
    0x2582;
    0x1CDAC;
    0x1CDAD;
    0x1CDAE;
    0x1CDAF;
    0x1CDB0;
    0x1CDB1;
    0x1CDB2;
    0x1CDB3;
    0x1CDB4;
    0x1CDB5;
    0x1CDB6;
    0x1CDB7;
    0x1CDB8;
    0x1CDB9;
    0x1CDBA;
    0x1CDBB;
    0x1CDBC;
    0x1CDBD;
    0x1CDBE;
    0x1CDBF;
    0x1CDC0;
    0x1CDC1;
    0x1CDC2;
    0x1CDC3;
    0x1CDC4;
    0x1CDC5;
    0x1CDC6;
    0x1CDC7;
    0x1CDC8;
    0x1CDC9;
    0x1CDCA;
    0x1CDCB;
    0x1CDCC;
    0x1CDCD;
    0x1CDCE;
    0x1CDCF;
    0x1CDD0;
    0x1CDD1;
    0x1CDD2;
    0x1CDD3;
    0x1CDD4;
    0x1CDD5;
    0x1CDD6;
    0x1CDD7;
    0x1CDD8;
    0x1CDD9;
    0x1CDDA;
    0x2584;
    0x1CDDB;
    0x1CDDC;
    0x1CDDD;
    0x1CDDE;
    0x2599;
    0x1CDDF;
    0x1CDE0;
    0x1CDE1;
    0x1CDE2;
    0x259F;
    0x1CDE3;
    0x2586;
    0x1CDE4;
    0x1CDE5;
    0x2588;
  |]

let codepoint code bits =
  match code with
  | 1 -> half.(bits land 3)
  | 2 -> quadrant.(bits land 15)
  | 3 -> sextant (bits land 63)
  | 4 -> braille.(bits land 255)
  | _ -> octant.(bits land 255)

let commit t grid ~x ~y =
  for cy = 0 to t.height - 1 do
    for cx = 0 to t.width - 1 do
      let i = (cy * t.width) + cx in
      let k = kind t i in
      let has_bg = Bytes.unsafe_get t.has_bg i <> '\000' in
      let bg =
        if has_bg then Ansi.Color.Packed.decode t.bg.(i)
        else Ansi.Color.transparent
      in
      if k = k_dots then
        Matrix_grid.set_uchar grid ~x:(x + cx) ~y:(y + cy)
          ~uchar:
            (Uchar.of_int
               (codepoint (Char.code (Bytes.unsafe_get t.res i)) t.bits.(i)))
          ~fg:(Ansi.Color.Packed.decode t.fg.(i))
          ~bg
          ~attrs:(Ansi.Attr.unpack t.attrs.(i))
          ~blend:true ()
      else if k = k_text then
        let style =
          Ansi.Style.make ~fg:(Ansi.Color.Packed.decode t.fg.(i)) ~bg ()
          |> Ansi.Style.with_attrs (Ansi.Attr.unpack t.attrs.(i))
        in
        Matrix_grid.draw_text grid ~style ~x:(x + cx) ~y:(y + cy)
          ~text:t.text.(i)
      else if k = k_none && has_bg then
        Matrix_grid.fill_rect grid ~x:(x + cx) ~y:(y + cy) ~width:1 ~height:1
          ~color:bg
    done
  done
