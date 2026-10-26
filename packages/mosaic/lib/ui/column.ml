type cell =
  | Plain of { text : string; style : Ansi.Style.t option }
  | Rich of Text.fragment list

let text ?style text = Plain { text; style }
let rich fragments = Rich fragments
let empty = Plain { text = ""; style = None }

let cell_equal a b =
  match (a, b) with
  | Plain a, Plain b ->
      String.equal a.text b.text
      && Option.equal Ansi.Style.equal a.style b.style
  | Rich a, Rich b -> Text.fragments_equal a b
  | _ -> false

(* The text of a cell, for measuring it. *)
let plain_text = function
  | Plain { text; _ } -> text
  | Rich fragments ->
      let buf = Buffer.create 32 in
      let rec collect = function
        | [] -> ()
        | Text.Text { text; _ } :: rest ->
            Buffer.add_string buf text;
            collect rest
        | Text.Span { children; _ } :: rest ->
            collect children;
            collect rest
      in
      collect fragments;
      Buffer.contents buf

type width = [ `Auto | `Fixed of int | `Fraction of float ]
type alignment = [ `Left | `Center | `Right ]
type overflow = [ `Ellipsis | `Crop ]

type 'a t = {
  header : string;
  width : width;
  alignment : alignment;
  overflow : overflow;
  min_width : int option;
  max_width : int option;
  cell : 'a -> cell;
}

let make ?(header = "") ?(width = `Auto) ?(alignment = `Left)
    ?(overflow = `Ellipsis) ?min_width ?max_width cell =
  { header; width; alignment; overflow; min_width; max_width; cell }

(* ───── Widths ───── *)

(* Table.compute_column_widths resolves widths by the same rule, which this
   module states; the two meet in a test until lists replace tables. *)

let measure ~width_method s = Matrix.Text.measure ~width_method ~tab_width:2 s

(* The widest of [column]'s header and cells over [items]. *)
let content_width ~width_method column items =
  let widest = ref (measure ~width_method column.header) in
  Items.iter
    (fun item ->
      let w = measure ~width_method (plain_text (column.cell item)) in
      if w > !widest then widest := w)
    items;
  !widest

let bound ~pad column w =
  let w = match column.min_width with Some m -> max w (m + pad) | None -> w in
  match column.max_width with Some m -> min w (m + pad) | None -> w

let widths ~width_method ?(padding = 0) ~available columns items =
  let columns = Array.of_list columns in
  let count = Array.length columns in
  let pad = 2 * max 0 padding in
  let widths = Array.make count 0 in
  let usable = max 0 (available - max 0 (count - 1)) in
  let taken = ref 0 and fractions = ref 0. in
  Array.iteri
    (fun i column ->
      match column.width with
      | `Fixed n ->
          widths.(i) <- max 1 (n + pad);
          taken := !taken + widths.(i)
      | `Auto ->
          let w = max 1 (content_width ~width_method column items + pad) in
          widths.(i) <- max 1 (bound ~pad column w);
          taken := !taken + widths.(i)
      | `Fraction f -> fractions := !fractions +. f)
    columns;
  let remaining = float (max 0 (usable - !taken)) in
  if !fractions > 0. then
    Array.iteri
      (fun i column ->
        match column.width with
        | `Fraction f ->
            let w = max 1 (int_of_float (remaining *. f /. !fractions)) in
            widths.(i) <- bound ~pad column (max w (pad + 1))
        | `Fixed _ | `Auto -> ())
      columns;
  widths
