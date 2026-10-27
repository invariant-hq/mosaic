type t = {
  foreground : int * int * int;
  background : int * int * int;
  cursor : (int * int * int) option;
  entries : (int * int * int) option array;
}

(* The colours of Color.indexed, white (7) on black (0), and the cursor the
   foreground's. *)
let default =
  let entries =
    Array.init 256 (fun i -> Some (Color.to_rgb (Color.indexed i)))
  in
  let white = Color.to_rgb (Color.indexed 7) in
  {
    foreground = white;
    background = Color.to_rgb (Color.indexed 0);
    cursor = Some white;
    entries;
  }

let entry p i =
  if i >= 0 && i < 256 && i < Array.length p.entries then p.entries.(i)
  else None

let rgb (r, g, b) = Color.of_rgb r g b

(* The RGB [color] shows on a terminal of palette [p]: SGR writes the
   default color, and a transparent RGB color, as [39] or [49], and the
   terminal draws its own [default]. *)
let shown p ~default color =
  match Color.intent color with
  | Default -> Some default
  | Rgb when Color.alpha color = 0. -> Some default
  | Indexed i -> entry p i
  | Rgb -> Some (Color.to_rgb color)

let contrast p ~fg ~bg =
  match
    (shown p ~default:p.foreground fg, shown p ~default:p.background bg)
  with
  | Some fg, Some bg -> Some (Color.contrast (rgb fg) (rgb bg))
  | _ -> None

(* The legibility gate. Every table of styles by purpose that takes palette
   entries for text, mosaic's theme and its syntax style among them, calls
   this function, so that they all choose the same entries. *)
let legible ?target p i =
  let target =
    match target with
    | Some target -> target
    | None ->
        Float.min 4.5 (Color.contrast (rgb p.foreground) (rgb p.background))
  in
  let reaches i =
    match entry p i with
    | Some e -> Color.contrast (rgb e) (rgb p.background) >= target
    | None -> false
  in
  if reaches i then Some (Color.indexed i)
  else if 0 <= i && i < 8 && reaches (i + 8) then Some (Color.indexed (i + 8))
  else None

let equal a b =
  let rec entries i = i > 255 || (entry a i = entry b i && entries (i + 1)) in
  a.foreground = b.foreground
  && a.background = b.background
  && a.cursor = b.cursor && entries 0

let pp ppf p =
  let rgb ppf (r, g, b) = Format.fprintf ppf "#%02x%02x%02x" r g b in
  let known ppf = function
    | Some c -> rgb ppf c
    | None -> Format.pp_print_char ppf '-'
  in
  (* Up to the last entry known, so that equal palettes print alike. *)
  let rec last i = if i < 0 || entry p i <> None then i else last (i - 1) in
  let entries = List.init (last 255 + 1) (entry p) in
  Format.fprintf ppf "@[<1>{fg %a;@ bg %a;@ cursor %a;@ entries [@[%a@]]}@]" rgb
    p.foreground rgb p.background known p.cursor
    (Format.pp_print_list ~pp_sep:Format.pp_print_space known)
    entries
