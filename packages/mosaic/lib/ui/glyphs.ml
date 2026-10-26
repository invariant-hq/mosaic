type t = {
  cursor : string;
  cursor_inactive : string;
  checked : string;
  unchecked : string;
  expand : string;
  collapse : string;
  leaf : string;
  ellipsis : string;
  thumb : string;
  more_before : string;
  more_after : string;
  underline : string;
  arrow_up : string;
  arrow_down : string;
  arrow_left : string;
  arrow_right : string;
}

let unicode =
  {
    cursor = "\u{276F}";
    cursor_inactive = "\u{203A}";
    checked = "\u{2611}";
    unchecked = "\u{2610}";
    expand = "\u{25B6}";
    collapse = "\u{25BC}";
    leaf = " ";
    ellipsis = "\u{2026}";
    thumb = "\u{2588}";
    more_before = "\u{2039}";
    more_after = "\u{203A}";
    underline = "\u{25AC}";
    arrow_up = "\u{25B2}";
    arrow_down = "\u{25BC}";
    arrow_left = "\u{25C0}";
    arrow_right = "\u{25B6}";
  }

let ascii =
  {
    cursor = ">";
    cursor_inactive = "-";
    checked = "[x]";
    unchecked = "[ ]";
    expand = "+";
    collapse = "-";
    leaf = " ";
    ellipsis = "...";
    thumb = "#";
    more_before = "<";
    more_after = ">";
    underline = "-";
    arrow_up = "^";
    arrow_down = "v";
    arrow_left = "<";
    arrow_right = ">";
  }

(* Every field is a string, so structural equality compares the glyphs. *)
let equal (g : t) g' = g = g'
