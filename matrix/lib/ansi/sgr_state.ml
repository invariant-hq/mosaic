type t = {
  mutable fg_r : float;
  mutable fg_g : float;
  mutable fg_b : float;
  mutable fg_a : float;
  mutable bg_r : float;
  mutable bg_g : float;
  mutable bg_b : float;
  mutable bg_a : float;
  mutable attrs : int;
  mutable link : string; (* "" = no link *)
  mutable link_open : bool;
}

let create () =
  {
    fg_r = -1.;
    fg_g = -1.;
    fg_b = -1.;
    fg_a = -1.;
    bg_r = -1.;
    bg_g = -1.;
    bg_b = -1.;
    bg_a = -1.;
    attrs = -1;
    link = "";
    link_open = false;
  }

let reset t =
  t.fg_r <- -1.;
  t.fg_g <- -1.;
  t.fg_b <- -1.;
  t.fg_a <- -1.;
  t.bg_r <- -1.;
  t.bg_g <- -1.;
  t.bg_b <- -1.;
  t.bg_a <- -1.;
  t.attrs <- -1;
  t.link <- "";
  t.link_open <- false

let[@inline] floats_neq a b = Float.abs (a -. b) > 0.00001

let[@inline] float_to_u8 f =
  let v = Float.round (f *. 255.) |> int_of_float in
  if v < 0 then 0 else if v > 255 then 255 else v

let update t w ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b ~bg_a ~attrs ~link =
  (* Handle hyperlink transitions first - "" means no link *)
  if not (String.equal link t.link) then (
    (* Close existing hyperlink if open *)
    if t.link_open then Escape.hyperlink_close w;
    (* Open new hyperlink if present *)
    if link <> "" then (
      Escape.hyperlink_open w link;
      t.link <- link;
      t.link_open <- true)
    else (
      t.link <- "";
      t.link_open <- false));

  (* Handle SGR state *)
  let fg_visible = fg_a > 0.0001 in
  let prev_fg_visible = t.fg_a > 0.0001 in
  let fg_diff =
    fg_visible <> prev_fg_visible
    || fg_visible
       && (floats_neq fg_r t.fg_r || floats_neq fg_g t.fg_g
         || floats_neq fg_b t.fg_b)
  in
  let bg_visible = bg_a > 0.0001 in
  let prev_bg_visible = t.bg_a > 0.0001 in
  let bg_diff =
    bg_visible <> prev_bg_visible
    || bg_visible
       && (floats_neq bg_r t.bg_r || floats_neq bg_g t.bg_g
         || floats_neq bg_b t.bg_b)
  in
  let attr_diff = attrs <> t.attrs in

  if fg_diff || bg_diff || attr_diff then (
    (* Zero-allocation SGR emission using low-level primitives *)
    Escape.sgr_open w;
    Escape.sgr_code w 0;

    (* Foreground color *)
    if fg_visible then (
      Escape.sgr_sep w;
      Escape.sgr_code w 38;
      Escape.sgr_sep w;
      Escape.sgr_code w 2;
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 fg_r);
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 fg_g);
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 fg_b));

    (* Background color *)
    if bg_visible then (
      Escape.sgr_sep w;
      Escape.sgr_code w 48;
      Escape.sgr_sep w;
      Escape.sgr_code w 2;
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 bg_r);
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 bg_g);
      Escape.sgr_sep w;
      Escape.sgr_code w (float_to_u8 bg_b));

    (* Attributes - unrolled for zero allocation. Bit positions: Bold=0, Dim=1,
       Italic=2, Underline=3, Blink=4, Inverse=5, Hidden=6, Strikethrough=7,
       Double_underline=8, Overline=9, Framed=10, Encircled=11 *)
    if attrs <> 0 then (
      if attrs land 0x001 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 1);
      if attrs land 0x002 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 2);
      if attrs land 0x004 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 3);
      if attrs land 0x008 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 4);
      if attrs land 0x010 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 5);
      if attrs land 0x020 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 7);
      if attrs land 0x040 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 8);
      if attrs land 0x080 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 9);
      if attrs land 0x100 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 21);
      if attrs land 0x200 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 53);
      if attrs land 0x400 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 51);
      if attrs land 0x800 <> 0 then (
        Escape.sgr_sep w;
        Escape.sgr_code w 52));

    Escape.sgr_close w;

    t.fg_r <- fg_r;
    t.fg_g <- fg_g;
    t.fg_b <- fg_b;
    t.fg_a <- fg_a;
    t.bg_r <- bg_r;
    t.bg_g <- bg_g;
    t.bg_b <- bg_b;
    t.bg_a <- bg_a;
    t.attrs <- attrs)

let close_link t w =
  if t.link_open then (
    Escape.hyperlink_close w;
    t.link <- "";
    t.link_open <- false)
