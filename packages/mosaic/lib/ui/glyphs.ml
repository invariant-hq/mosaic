type progress = { filled : string; half : string; empty : string }
type spinner = { frames : string array; period : float }

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
  progress_horizontal : progress;
  progress_vertical : progress;
  spinner : spinner;
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
    progress_horizontal =
      { filled = "\u{2588}"; half = "\u{258C}"; empty = "\u{2500}" };
    progress_vertical =
      { filled = "\u{2588}"; half = "\u{2584}"; empty = "\u{2502}" };
    spinner =
      {
        frames =
          [|
            "\u{280B}";
            "\u{2819}";
            "\u{2839}";
            "\u{2838}";
            "\u{283C}";
            "\u{2834}";
            "\u{2826}";
            "\u{2827}";
            "\u{2807}";
            "\u{280F}";
          |];
        period = 0.08;
      };
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
    progress_horizontal = { filled = "#"; half = "="; empty = "-" };
    progress_vertical = { filled = "#"; half = "="; empty = "|" };
    spinner = { frames = [| "-"; "\\"; "|"; "/" |]; period = 0.13 };
  }

(* Every field is made of strings and periods, so structural equality
   compares the glyphs. *)
let equal (g : t) g' = g = g'
