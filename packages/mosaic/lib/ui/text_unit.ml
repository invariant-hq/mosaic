module Source = Matrix.Text.Layout.Source

type t = Grapheme | Word | Line

let of_clicks n = if n <= 1 then Grapheme else if n = 2 then Word else Line

(* The kinds of cluster words are made of. Runs form of [Blank] and of
   [Other] only: a boundary character is a word of its own, and a line
   terminator joins nothing. *)
type kind = Blank | Boundary | Break | Other

let kind_of_uchar u =
  match Uchar.to_int u with
  | 0x0A | 0x0D -> Break
  | 0x09 | 0x0B | 0x0C | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2028 | 0x2029 | 0x202F
  | 0x205F | 0x3000 ->
      Blank
  | c when c >= 0x2000 && c <= 0x200A -> Blank
  (* Ghostty's boundary characters, as OpenTUI copied them. *)
  | 0x22 | 0x24 | 0x27 | 0x28 | 0x29 | 0x2C | 0x3A | 0x3B | 0x3C | 0x3E | 0x5B
  | 0x5D | 0x60 | 0x7B | 0x7C | 0x7D | 0x2502 ->
      Boundary
  | _ -> Other

(* The kind of cluster [g], from its first code point. *)
let kind_at source g =
  let text = Source.text source in
  let b = Source.byte_offset source g in
  kind_of_uchar (Uchar.utf_decode_uchar (String.get_utf_8_uchar text b))

let joins a b =
  match (a, b) with
  | Blank, Blank | Other, Other -> true
  | (Blank | Boundary | Break | Other), _ -> false

(* The word of cluster [g]. *)
let word source g =
  let kind = kind_at source g in
  match kind with
  | Boundary | Break -> (g, g + 1)
  | Blank | Other ->
      let first = ref g in
      while !first > 0 && joins (kind_at source (!first - 1)) kind do
        decr first
      done;
      let n = Source.length source in
      let last = ref (g + 1) in
      while !last < n && joins (kind_at source !last) kind do
        incr last
      done;
      (!first, !last)

let span unit source ~first ~last =
  match unit with
  | Grapheme -> (first, last)
  | Word ->
      if first < last then
        (fst (word source first), snd (word source (last - 1)))
      else if
        first > 0
        && first < Source.length source
        && joins (kind_at source (first - 1)) (kind_at source first)
      then word source first
      else (first, last)
  | Line ->
      let line = Source.line_of_offset source in
      let last_line = if last > first then line (last - 1) else line first in
      (Source.line_start source (line first), Source.line_end source last_line)
