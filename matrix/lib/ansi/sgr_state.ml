type t = {
  mutable fg_color : int;
  mutable bg_color : int;
  mutable attrs : int;
  mutable link : string; (* "" = no link *)
  mutable link_open : bool;
}

let create () =
  { fg_color = -1; bg_color = -1; attrs = -1; link = ""; link_open = false }

let reset t =
  t.fg_color <- -1;
  t.bg_color <- -1;
  t.attrs <- -1;
  t.link <- "";
  t.link_open <- false

let update_link t w link =
  if not (String.equal link t.link) then (
    if t.link_open then Escape.hyperlink_close w;
    if link <> "" then (
      Escape.hyperlink_open w link;
      t.link <- link;
      t.link_open <- true)
    else (
      t.link <- "";
      t.link_open <- false))

let emit_attrs w attrs =
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
      Escape.sgr_code w 52))

let update t w ~fg ~bg ~attrs ~link =
  update_link t w link;
  let fg = Color.Packed.encode fg in
  let bg = Color.Packed.encode bg in
  if fg <> t.fg_color || bg <> t.bg_color || attrs <> t.attrs then (
    Escape.sgr_open w;
    Escape.sgr_code w 0;
    Color.Packed.emit_sgr w ~bg:false fg;
    Color.Packed.emit_sgr w ~bg:true bg;
    emit_attrs w attrs;
    Escape.sgr_close w;
    t.fg_color <- fg;
    t.bg_color <- bg;
    t.attrs <- attrs)

let close_link t w =
  if t.link_open then (
    Escape.hyperlink_close w;
    t.link <- "";
    t.link_open <- false)
