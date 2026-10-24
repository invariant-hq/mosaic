module Grid = Matrix_grid
module Color = Ansi.Color
module Style = Ansi.Style
module Attr = Ansi.Attr

let err fn fmt =
  Format.kasprintf (fun s -> invalid_arg ("Matrix_charts." ^ fn ^ ": " ^ s)) fmt

let utf8 u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

(* {1 Scales} *)

type skind = Linear | Log of float | Time of int | Band

(* A scale specification. Unset properties take what the marks reading the
   scale imply, then the defaults. *)
type spec = {
  s_kind : skind;
  s_domain : (float * float) option;
  s_cats : string list option;
  s_nice : bool option;
  s_zero : bool option;
  s_reverse : bool option;
  s_ticks : float list option;
  s_colors : Color.t list option;
  s_padding : float option;
  s_step : int option;
}

module Scale = struct
  type 'd t = spec

  let make ?domain ?cats ?nice ?zero ?reverse ?ticks ?colors ?padding ?step kind
      =
    {
      s_kind = kind;
      s_domain = domain;
      s_cats = cats;
      s_nice = nice;
      s_zero = zero;
      s_reverse = reverse;
      s_ticks = ticks;
      s_colors = colors;
      s_padding = padding;
      s_step = step;
    }

  let check_domain fn = function
    | Some (a, b) when not (Float.is_finite a && Float.is_finite b && a <= b) ->
        err fn "domain (%g, %g) is not finite and increasing" a b
    | _ -> ()

  let check_colors fn = function Some [] -> err fn "colors is empty" | _ -> ()

  let linear ?domain ?nice ?zero ?reverse ?ticks ?colors () =
    check_domain "Scale.linear" domain;
    check_colors "Scale.linear" colors;
    make ?domain ?nice ?zero ?reverse ?ticks ?colors Linear

  let log ?(base = 10.) ?domain ?nice ?reverse ?ticks ?colors () =
    if not (Float.is_finite base && base > 1.) then
      err "Scale.log" "base %g is not greater than 1" base;
    check_domain "Scale.log" domain;
    check_colors "Scale.log" colors;
    (match domain with
    | Some (a, _) when a <= 0. ->
        err "Scale.log" "domain starts at %g, not above 0" a
    | _ -> ());
    make ?domain ?nice ?reverse ?ticks ?colors (Log base)

  let time ?domain ?nice ?(offset_s = 0) () =
    check_domain "Scale.time" domain;
    make ?domain ?nice (Time offset_s)

  let band ?domain ?padding ?step ?reverse ?colors () =
    (match padding with
    | Some p when not (p >= 0. && p < 1.) ->
        err "Scale.band" "padding %g is not in [0;1)" p
    | _ -> ());
    (match step with
    | Some s when s < 1 -> err "Scale.band" "step %d is below 1" s
    | _ -> ());
    check_colors "Scale.band" colors;
    make ?cats:domain ?padding ?step ?reverse ?colors Band
end

(* {1 Figures} *)

type dots = [ `Cell | `Half | `Quadrant | `Sextant | `Braille | `Octant ]
type dash = [ `Solid | `Dashed | `Dotted ]

type 'r paint =
  | Const of Color.t
  | Series of Color.t option * string
  | Cat of ('r -> string)

let const c = Const c
let series ?color s = Series (color, s)
let cat f = Cat f

type kind =
  | K_line of { dots : [ dots | `Steps ] option; dash : dash }
  | K_area of { dots : dots option }
  | K_dot of { dots : [ dots | `Density ] option; symbol : string option }
  | K_rect
  | K_bar of { dots : [ `Eighths | `Cell ]; horizontal : bool }
  | K_hist
  | K_raster of {
      dots : [ `Cell | `Half | `Shade | `Glyph of string ];
      reduce : [ `Last | `Mean | `Max ];
      interpolate : [ `None | `Bilinear ];
    }
  | K_text of { anchor : [ `Left | `Center | `Right ] }
  | K_arrow of { head : [ `Arrow | `Dot | `None ] }
  | K_spark of { dots : [ `Eighths | `Braille ] }
  | K_rule of { dash : dash; horizontal : bool }
  | K_candle of {
      width : int;
      hollow : bool;
      rising : Color.t option;
      falling : Color.t option;
    }

(* A channel reads a position from a row. *)
type 'r chan = No | Num of ('r -> float) | Str of ('r -> string) | Index

type 'r mark = {
  kind : kind;
  rows : 'r array;
  x : 'r chan;
  y : 'r chan;
  x2 : 'r chan;
  y2 : 'r chan;
  v : ('r -> float) option;
  w : ('r -> float) option;
  text : ('r -> string) option;
  paint : 'r paint option;
  row_of : int -> int;
}

type side = [ `Left | `Right | `Top | `Bottom ]
type grid = [ `None | `Ticks | `Minor of int | `Every of int ]

type axis = {
  a_scale : string;
  a_side : side option;
  a_show : bool;
  a_title : string option;
  a_grid : grid;
  a_format : (int -> float -> string) option;
  a_spacing : int;
  a_style : Style.t option;
  a_tick_style : Style.t option;
  a_label_style : Style.t option;
  a_title_style : Style.t option;
}

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
type legend_side = [ side | `Inside of corner ]

type legend = {
  l_scale : string;
  l_side : legend_side;
  l_show : bool;
  l_title : string option;
  l_direction : [ `Rows | `Columns ];
}

type margins = { top : int; right : int; bottom : int; left : int; inset : int }

type t =
  | Mark : 'r mark -> t
  | Layer of t list
  | Title of string * t
  | Name of string * t
  | Spec of string * spec * t
  | Rename of string * string * t
  | Attrs of Attr.t * t
  | Margins of margins * t
  | Axis of axis
  | Legend of legend
  | Frame

type figure = t
type id = string list

let mark ?(x = No) ?(y = No) ?(x2 = No) ?(y2 = No) ?v ?w ?text ?paint
    ?(row_of = Fun.id) kind rows =
  Mark { kind; rows; x; y; x2; y2; v; w; text; paint; row_of }

let opt_num = function None -> No | Some f -> Num f
let x_or_index = function None -> Index | Some f -> Num f

let line ?dots ?(dash = `Solid) ?color ?x ~y rows =
  mark ~x:(x_or_index x) ~y:(Num y) ?paint:color (K_line { dots; dash }) rows

let line_gaps ?dots ?dash ?color ?x ~y rows =
  line ?dots ?dash ?color ?x
    ~y:(fun r -> match y r with Some v -> v | None -> Float.nan)
    rows

let area ?dots ?color ?x ?y2 ~y rows =
  mark ~x:(x_or_index x) ~y:(Num y) ~y2:(opt_num y2) ?paint:color
    (K_area { dots })
    rows

let dot ?dots ?symbol ?color ~x ~y rows =
  mark ~x:(Num x) ~y:(Num y) ?paint:color (K_dot { dots; symbol }) rows

let rect ?color ?x ?x2 ?y ?y2 rows =
  mark ~x:(opt_num x) ~x2:(opt_num x2) ~y:(opt_num y) ~y2:(opt_num y2)
    ?paint:color K_rect rows

let bar_y ?(dots = `Eighths) ?color ?label ~x ~y rows =
  mark ~x:(Str x) ~y:(Num y) ?text:label ?paint:color
    (K_bar { dots; horizontal = false })
    rows

let bar_x ?(dots = `Eighths) ?color ?label ~y ~x rows =
  mark ~x:(Num x) ~y:(Str y) ?text:label ?paint:color
    (K_bar { dots; horizontal = true })
    rows

let raster ?(dots = `Cell) ?(reduce = `Last) ?(interpolate = `None) ~x ~y ~value
    rows =
  mark ~x:(Num x) ~y:(Num y) ~v:value
    (K_raster { dots; reduce; interpolate })
    rows

let text ?color ?(anchor = `Left) ~x ~y ~text rows =
  mark ~x:(Num x) ~y:(Num y) ~text ?paint:color (K_text { anchor }) rows

let arrow ?color ?(head = `Arrow) ~x ~y ~x2 ~y2 rows =
  mark ~x:(Num x) ~y:(Num y) ~x2:(Num x2) ~y2:(Num y2) ?paint:color
    (K_arrow { head })
    rows

let spark ?(dots = `Eighths) ?color values =
  let rows = Array.of_list (List.rev values) in
  let n = Array.length rows in
  mark
    ~y:(Num (fun v -> Float.max 0. v))
    ?paint:(Option.map const color)
    ~row_of:(fun i -> n - 1 - i)
    (K_spark { dots })
    rows

let rule ?(dash = `Solid) ?color ~horizontal v =
  let c = if horizontal then `Y else `X in
  let paint = Option.map const color in
  match c with
  | `Y -> mark ~y:(Num Fun.id) ?paint (K_rule { dash; horizontal }) [| v |]
  | `X -> mark ~x:(Num Fun.id) ?paint (K_rule { dash; horizontal }) [| v |]

let hline ?dash ?color v = rule ?dash ?color ~horizontal:true v
let vline ?dash ?color v = rule ?dash ?color ~horizontal:false v

type ohlc = {
  time : float;
  open_ : float;
  high : float;
  low : float;
  close : float;
}

let candles ?(width = 1) ?(hollow = false) ?rising ?falling rows =
  if width < 1 || width > 2 then err "candles" "width %d is not 1 or 2" width;
  mark
    ~x:(Num (fun c -> c.time))
    ~y:(Num (fun c -> c.high))
    ~y2:(Num (fun c -> c.low))
    ~v:(fun c -> c.open_)
    ~w:(fun c -> c.close)
    (K_candle { width; hollow; rising; falling })
    rows

let histogram ?(bins = `Count 10) ?(height = `Count) ?color values =
  let finite = List.filter Float.is_finite (Array.to_list values) in
  let edges =
    match bins with
    | `Edges e ->
        let e = Array.copy e in
        Array.sort Float.compare e;
        e
    | (`Count _ | `Width _) as b -> (
        match finite with
        | [] -> [||]
        | v :: _ ->
            let lo = List.fold_left Float.min v finite
            and hi = List.fold_left Float.max v finite in
            let lo, hi = if lo = hi then (lo -. 0.5, hi +. 0.5) else (lo, hi) in
            let n =
              match b with
              | `Count n ->
                  if n < 1 then err "histogram" "`Count %d is below 1" n;
                  n
              | `Width w ->
                  if not (w > 0. && Float.is_finite w) then
                    err "histogram" "`Width %g is not positive" w;
                  max 1 (Float.to_int (Float.ceil ((hi -. lo) /. w)))
            in
            let w = (hi -. lo) /. Float.of_int n in
            Array.init (n + 1) (fun i ->
                if i = n then hi else lo +. (Float.of_int i *. w)))
  in
  let nb = max 0 (Array.length edges - 1) in
  let counts = Array.make nb 0 in
  let find v =
    (* The bin [i] holds [edges.(i) <= v < edges.(i + 1)], the last one its
       upper edge too. *)
    let rec go lo hi =
      if lo >= hi then lo
      else
        let mid = (lo + hi + 1) / 2 in
        if edges.(mid) <= v then go mid hi else go lo (mid - 1)
    in
    if nb = 0 || v < edges.(0) || v > edges.(nb) then -1
    else min (nb - 1) (go 0 nb)
  in
  List.iter
    (fun v ->
      let i = find v in
      if i >= 0 then counts.(i) <- counts.(i) + 1)
    finite;
  let total = Float.of_int (List.length finite) in
  let rows =
    Array.init nb (fun i ->
        let c = Float.of_int counts.(i) in
        let w = edges.(i + 1) -. edges.(i) in
        let h =
          match height with
          | `Count -> c
          | `Probability -> if total = 0. then 0. else c /. total
          | `Density -> if total = 0. || w = 0. then 0. else c /. total /. w
        in
        (edges.(i), edges.(i + 1), h))
  in
  mark
    ~x:(Num (fun (a, _, _) -> a))
    ~x2:(Num (fun (_, b, _) -> b))
    ~y:(Num (fun (_, _, h) -> h))
    ?paint:color K_hist rows

let circle ?color ~x ~y ~r rows =
  let k = 48 in
  let n = Array.length rows in
  let pts =
    Array.init
      (n * (k + 2))
      (fun i ->
        let c = i / (k + 2) and j = i mod (k + 2) in
        if j = k + 1 then (c, Float.nan, Float.nan)
        else
          let row = rows.(c) in
          let a = 2. *. Float.pi *. Float.of_int j /. Float.of_int k in
          let rad = r row in
          (c, x row +. (rad *. cos a), y row +. (rad *. sin a)))
  in
  let paint =
    match color with
    | None -> None
    | Some (Const c) -> Some (Const c)
    | Some (Series (c, s)) -> Some (Series (c, s))
    | Some (Cat f) -> Some (Cat (fun (c, _, _) -> f rows.(c)))
  in
  mark
    ~x:(Num (fun (_, px, _) -> px))
    ~y:(Num (fun (_, _, py) -> py))
    ?paint
    ~row_of:(fun i -> i / (k + 2))
    (K_line { dots = None; dash = `Solid })
    pts

let layer l = Layer l
let title s f = Title (s, f)
let name s f = Name (s, f)
let scale n s f = Spec (n, s, f)
let rename a b f = Rename (a, b, f)
let attrs a f = Attrs (a, f)

let margins ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) ?(inset = 0) f =
  if top < 0 || right < 0 || bottom < 0 || left < 0 || inset < 0 then
    err "margins" "a margin is negative";
  Margins ({ top; right; bottom; left; inset }, f)

let axis ?side ?(show = true) ?title ?(grid = `None) ?format ?(spacing = 0)
    ?style ?tick_style ?label_style ?title_style name =
  (match grid with
  | `Minor n when n < 2 -> err "axis" "`Minor %d is below 2" n
  | `Every n when n < 1 -> err "axis" "`Every %d is below 1" n
  | _ -> ());
  if spacing < 0 then err "axis" "spacing %d is negative" spacing;
  Axis
    {
      a_scale = name;
      a_side = side;
      a_show = show;
      a_title = title;
      a_grid = grid;
      a_format = format;
      a_spacing = spacing;
      a_style = style;
      a_tick_style = tick_style;
      a_label_style = label_style;
      a_title_style = title_style;
    }

let legend ?(side = `Inside `Top_right) ?(show = true) ?title
    ?(direction = `Rows) name =
  Legend
    {
      l_scale = name;
      l_side = side;
      l_show = show;
      l_title = title;
      l_direction = direction;
    }

let frame = Frame

(* {1 Themes} *)

module Theme = struct
  type glyphs = {
    box : Grid.Border.t;
    dashed : string * string;
    dotted : string * string;
    diagonals : string * string;
    point : string;
    marker : string;
    shades : string array;
    blocks : bool;
  }

  type t = {
    palette : Color.t list;
    colors : Color.t list;
    background : Color.t option;
    axis : Style.t;
    label : Style.t;
    title : Style.t;
    grid : Style.t;
    minor_grid : Style.t;
    grid_dash : dash;
    tip : Style.t;
    tip_border : Style.t option;
    tip_padding : int;
    crosshair : Style.t;
    crosshair_dash : dash;
    marker : Style.t;
    glyphs : glyphs;
    dots : dots;
    tick_length : int;
    label_padding : int;
    aspect : float;
  }

  let unicode box =
    {
      box;
      dashed = ("┄", "┆");
      dotted = ("┈", "┊");
      diagonals = ("╱", "╲");
      point = "∙";
      marker = "●";
      shades = [| "░"; "▒"; "▓"; "█" |];
      blocks = true;
    }

  let light_glyphs = unicode Grid.Border.single

  let heavy_glyphs =
    {
      (unicode Grid.Border.heavy) with
      dashed = ("┅", "┇");
      dotted = ("┉", "┋");
    }

  let rounded_glyphs = unicode Grid.Border.rounded

  let ascii_glyphs =
    {
      box = Grid.Border.ascii;
      dashed = ("-", "|");
      dotted = (".", ":");
      diagonals = ("/", "\\");
      point = "*";
      marker = "O";
      shades = [| "."; "o"; "O"; "#" |];
      blocks = false;
    }

  let palette =
    [
      Color.cyan;
      Color.magenta;
      Color.yellow;
      Color.green;
      Color.blue;
      Color.red;
      Color.indexed 33;
      Color.indexed 39;
      Color.indexed 45;
    ]

  (* Viridis, sampled at ten points (van der Walt and Smith, matplotlib). *)
  let viridis =
    List.map
      (fun h -> Color.of_hex_exn h)
      [
        "#440154";
        "#482878";
        "#3e4989";
        "#31688e";
        "#26828e";
        "#1f9e89";
        "#35b779";
        "#6ece58";
        "#b5de2b";
        "#fde725";
      ]

  let gray level = Style.make ~fg:(Color.grayscale ~level) ()

  let dark =
    {
      palette;
      colors = viridis;
      background = None;
      axis = gray 13;
      label = gray 14;
      title = Style.make ~fg:(Color.grayscale ~level:20) ~bold:true ();
      grid = gray 5;
      minor_grid = gray 3;
      grid_dash = `Dotted;
      tip = Style.make ~fg:Color.white ~bg:(Color.grayscale ~level:4) ();
      tip_border = Some (gray 10);
      tip_padding = 1;
      crosshair = gray 10;
      crosshair_dash = `Solid;
      marker = Style.make ~fg:Color.yellow ~bold:true ();
      glyphs = light_glyphs;
      dots = `Braille;
      tick_length = 0;
      label_padding = 1;
      aspect = 2.;
    }

  let light =
    {
      dark with
      axis = gray 2;
      label = gray 1;
      title = Style.make ~fg:(Color.grayscale ~level:0) ~bold:true ();
      grid = gray 18;
      minor_grid = gray 20;
      tip = Style.make ~fg:Color.black ~bg:(Color.grayscale ~level:20) ();
      tip_border = Some (gray 8);
      crosshair = gray 8;
      marker = Style.make ~fg:Color.red ~bold:true ();
    }
end

(* {1 Views} *)

module View0 = struct
  type t = (string * (float * float)) list

  let empty = []
  let window n (v : t) = List.assoc_opt n v

  let set n w (v : t) =
    let v = List.remove_assoc n v in
    match w with
    | None -> v
    | Some (a, b) ->
        if not (Float.is_finite a && Float.is_finite b && a < b) then
          err "View.set" "window (%g, %g) of %S is not finite and increasing" a
            b n;
        List.sort (fun (a, _) (b, _) -> String.compare a b) ((n, (a, b)) :: v)
end

(* {1 Resolving a figure} *)

(* A mark with the context of its place in the figure. *)
type placed_mark =
  | P : {
      m : 'r mark;
      id : string list;
      attrs : Attr.t;
      sx : string;
      sy : string;
      sc : string;
    }
      -> placed_mark

type flat = {
  mutable marks : placed_mark list;
  mutable specs : (string * spec) list;
  mutable axes : axis list;
  mutable legends : legend list;
  mutable fig_title : string option;
  mutable fig_margins : margins;
  mutable fig_frame : bool;
}

let flatten fig =
  let f =
    {
      marks = [];
      specs = [];
      axes = [];
      legends = [];
      fig_title = None;
      fig_margins = { top = 0; right = 0; bottom = 0; left = 0; inset = 0 };
      fig_frame = false;
    }
  in
  let rec go ~path ~slot ~rn ~attrs = function
    | Mark m ->
        f.marks <-
          P
            {
              m;
              id = List.rev path;
              attrs;
              sx = rn "x";
              sy = rn "y";
              sc = rn "color";
            }
          :: f.marks
    | Layer l ->
        List.iteri
          (fun i c ->
            go ~path:(string_of_int i :: path) ~slot:true ~rn ~attrs c)
          l
    | Title (s, c) ->
        (match f.fig_title with
        | Some t -> err "layout" "two titles, %S and %S" t s
        | None -> f.fig_title <- Some s);
        go ~path ~slot ~rn ~attrs c
    | Name (s, c) ->
        let path = if slot then s :: List.tl path else s :: path in
        go ~path ~slot:true ~rn ~attrs c
    | Spec (n, s, c) ->
        let n = rn n in
        if List.mem_assoc n f.specs then
          err "layout" "two specifications of scale %S" n;
        f.specs <- (n, s) :: f.specs;
        go ~path ~slot ~rn ~attrs c
    | Rename (a, b, c) ->
        let rn n = rn (if String.equal n a then b else n) in
        go ~path ~slot ~rn ~attrs c
    | Attrs (a, c) -> go ~path ~slot ~rn ~attrs:(Attr.union attrs a) c
    | Margins (m, c) ->
        let o = f.fig_margins in
        f.fig_margins <-
          {
            top = max o.top m.top;
            right = max o.right m.right;
            bottom = max o.bottom m.bottom;
            left = max o.left m.left;
            inset = max o.inset m.inset;
          };
        go ~path ~slot ~rn ~attrs c
    | Axis a ->
        let a = { a with a_scale = rn a.a_scale } in
        if List.exists (fun b -> String.equal b.a_scale a.a_scale) f.axes then
          err "layout" "two axes of scale %S" a.a_scale;
        f.axes <- a :: f.axes
    | Legend l ->
        let l = { l with l_scale = rn l.l_scale } in
        if List.exists (fun k -> String.equal k.l_scale l.l_scale) f.legends
        then err "layout" "two legends of scale %S" l.l_scale;
        f.legends <- l :: f.legends
    | Frame -> f.fig_frame <- true
  in
  go ~path:[] ~slot:false ~rn:Fun.id ~attrs:Attr.empty fig;
  f.marks <- List.rev f.marks;
  f.axes <- List.rev f.axes;
  f.legends <- List.rev f.legends;
  f

(* An evaluated channel: numbers, or categories before their scale numbers
   them. *)
type ech = E_none | E_num of float array | E_str of string array

type epaint =
  | P_none
  | P_const of Color.t
  | P_series of Color.t option * string
  | P_cat of string array

(* A mark whose rows are read: every accessor is called once per layout. *)
type ev = {
  kind : kind;
  mi : int;
  id : string list;
  attrs : Attr.t;
  n : int;
  row_of : int -> int;
  sx : string;
  sy : string;
  sc : string;
  mutable ex : ech;
  mutable ey : ech;
  mutable ex2 : ech;
  mutable ey2 : ech;
  ev_v : float array;
  ev_w : float array;
  texts : string array;
  paint : epaint;
}

let eval_chan rows = function
  | No -> E_none
  | Num f -> E_num (Array.map f rows)
  | Str f -> E_str (Array.map f rows)
  | Index -> E_num (Array.init (Array.length rows) Float.of_int)

let evaluate mi (P { m; id; attrs; sx; sy; sc }) =
  let rows = m.rows in
  let n = Array.length rows in
  let floats = function None -> [||] | Some f -> Array.map f rows in
  {
    kind = m.kind;
    mi;
    id;
    attrs;
    n;
    row_of = m.row_of;
    sx;
    sy;
    sc;
    ex = eval_chan rows m.x;
    ey = eval_chan rows m.y;
    ex2 = eval_chan rows m.x2;
    ey2 = eval_chan rows m.y2;
    ev_v = floats m.v;
    ev_w = floats m.w;
    texts = (match m.text with None -> [||] | Some f -> Array.map f rows);
    paint =
      (match m.paint with
      | None -> P_none
      | Some (Const c) -> P_const c
      | Some (Series (c, s)) -> P_series (c, s)
      | Some (Cat f) -> P_cat (Array.map f rows));
  }

type dir = X | Y | C

type scale = {
  name : string;
  kind : skind;
  dir : dir;
  dlo : float;
  dhi : float;
  lo : float;
  hi : float;
  cats : string array;
  reverse : bool;
  padding : float;
  step : int option;
  ticks : float list option;
  colors : Color.t list option;
  hidden : bool;
}

(* What the marks reading a scale say of it. *)
type usage = {
  mutable u_dir : dir option;
  mutable nums : bool;
  mutable strs : bool;
  mutable zero : bool;
  mutable no_nice : bool;
  mutable u_hidden : bool;
  mutable u_padding : float option;
  mutable u_cats : string list;
  seen : (string, unit) Hashtbl.t;
  mutable hull_lo : float;
  mutable hull_hi : float;
}

let placeable kind v =
  Float.is_finite v && match kind with Log _ -> v > 0. | _ -> true

type resolved = {
  evs : ev array;
  scales : scale list;
  axes : axis list;
  legends : legend list;
  fig_title : string option;
  fig_margins : margins;
  fig_frame : bool;
  mark_color : Color.t array;
  cat_colors : (string * (string * Color.t) list) list;
  swatches : (string * string) list;
}

let find_scale (r : resolved) n =
  List.find_opt (fun (s : scale) -> String.equal s.name n) r.scales

let band_index (s : scale) c =
  let rec go i =
    if i >= Array.length s.cats then Float.nan
    else if String.equal s.cats.(i) c then Float.of_int i
    else go (i + 1)
  in
  go 0

(* [norm s v] is [v] normalized by the continuous scale [s], over its shown
   window. A constant window normalizes every value to [0.5]. *)
let tf (s : scale) v =
  match s.kind with
  | Log _ -> if v > 0. then Float.log v else Float.nan
  | _ -> v

let untf (s : scale) u = match s.kind with Log _ -> Float.exp u | _ -> u

let norm (s : scale) v =
  let a = tf s s.lo and b = tf s s.hi in
  let p = if a = b then 0.5 else (tf s v -. a) /. (b -. a) in
  if s.reverse then 1. -. p else p

let unnorm (s : scale) p =
  let p = if s.reverse then 1. -. p else p in
  let a = tf s s.lo and b = tf s s.hi in
  untf s (a +. (p *. (b -. a)))

(* [span s] is the shown window of [s] in its transform's space. *)
let span (s : scale) = (tf s s.lo, tf s s.hi)

let family_of_kind = function
  | Linear | Band -> Ticks.Linear
  | Log b -> Ticks.Log b
  | Time o -> Ticks.Time o

let family (s : scale) = family_of_kind s.kind

let resolve ?(view = View0.empty) ~(theme : Theme.t) fig =
  let f = flatten fig in
  let evs = Array.of_list (List.mapi evaluate f.marks) in
  let usages : (string * usage) list ref = ref [] in
  let usage n =
    match List.assoc_opt n !usages with
    | Some u -> u
    | None ->
        let u =
          {
            u_dir = None;
            nums = false;
            strs = false;
            zero = false;
            no_nice = false;
            u_hidden = false;
            u_padding = None;
            u_cats = [];
            seen = Hashtbl.create 8;
            hull_lo = infinity;
            hull_hi = neg_infinity;
          }
        in
        usages := !usages @ [ (n, u) ];
        u
  in
  let use n d ch =
    match ch with
    | E_none -> ()
    | _ -> (
        let u = usage n in
        (match u.u_dir with
        | Some d' when d' <> d ->
            err "layout" "scale %S is read as more than one of x, y and color" n
        | _ -> u.u_dir <- Some d);
        match ch with
        | E_num _ -> u.nums <- true
        | E_str a ->
            u.strs <- true;
            Array.iter
              (fun c ->
                if not (Hashtbl.mem u.seen c) then (
                  Hashtbl.add u.seen c ();
                  u.u_cats <- c :: u.u_cats))
              a
        | E_none -> ())
  in
  Array.iter
    (fun (e : ev) ->
      use e.sx X e.ex;
      use e.sx X e.ex2;
      use e.sy Y e.ey;
      use e.sy Y e.ey2;
      (match e.paint with
      | P_series (_, s) -> use e.sc C (E_str [| s |])
      | P_cat a -> use e.sc C (E_str a)
      | P_none | P_const _ -> ());
      (match e.kind with K_raster _ -> use e.sc C (E_num e.ev_v) | _ -> ());
      let u n = usage n in
      match e.kind with
      | K_bar { horizontal; _ } ->
          let q = u (if horizontal then e.sx else e.sy)
          and b = u (if horizontal then e.sy else e.sx) in
          q.zero <- true;
          b.u_padding <- Some 0.2
      | K_area _ when e.ey2 = E_none -> (u e.sy).zero <- true
      | K_hist -> (u e.sy).zero <- true
      | K_spark _ ->
          let q = u e.sy in
          q.zero <- true;
          q.no_nice <- true;
          q.u_hidden <- true
      | _ -> ())
    evs;
  (* Scales and their kinds *)
  List.iter
    (fun (n, _) ->
      if not (List.mem_assoc n !usages) then
        err "layout" "scale %S has a specification but no role reads it" n)
    f.specs;
  let kind_of n (u : usage) =
    match List.assoc_opt n f.specs with
    | Some s ->
        (match (s.s_kind, u.strs, u.nums) with
        | Band, _, true -> err "layout" "band scale %S is read by quantities" n
        | (Linear | Log _ | Time _), true, _ ->
            err "layout" "scale %S of quantities is read by categories" n
        | _ -> ());
        (s.s_kind, Some s)
    | None ->
        if u.strs && u.nums then
          err "layout" "scale %S is read by quantities and categories" n
        else if u.strs then (Band, None)
        else (Linear, None)
  in
  let kinds = List.map (fun (n, u) -> (n, kind_of n u)) !usages in
  (* Band channels become category indices, and bars stack. *)
  let band_scale n =
    match List.assoc_opt n kinds with
    | Some (Band, spec) ->
        let u = List.assoc n !usages in
        let cats =
          match spec with
          | Some { s_cats = Some c; _ } -> Array.of_list c
          | _ -> Array.of_list (List.rev u.u_cats)
        in
        Some cats
    | _ -> None
  in
  let bands =
    List.filter_map
      (fun (n, _) -> Option.map (fun c -> (n, c)) (band_scale n))
      !usages
  in
  let index n a =
    match List.assoc_opt n bands with
    | None -> E_num (Array.map (fun _ -> Float.nan) a)
    | Some cats ->
        let tbl = Hashtbl.create (Array.length cats) in
        Array.iteri
          (fun i c -> if not (Hashtbl.mem tbl c) then Hashtbl.add tbl c i)
          cats;
        E_num
          (Array.map
             (fun c ->
               match Hashtbl.find_opt tbl c with
               | Some i -> Float.of_int i
               | None -> Float.nan)
             a)
  in
  Array.iter
    (fun (e : ev) ->
      let num n = function E_str a -> index n a | ch -> ch in
      e.ex <- num e.sx e.ex;
      e.ex2 <- num e.sx e.ex2;
      e.ey <- num e.sy e.ey;
      e.ey2 <- num e.sy e.ey2;
      match e.kind with
      | K_bar { horizontal; _ } -> (
          let cat_ch, val_ch =
            if horizontal then (e.ey, e.ex) else (e.ex, e.ey)
          in
          match (cat_ch, val_ch) with
          | E_num cats, E_num vals ->
              let pos = Hashtbl.create 8 and neg = Hashtbl.create 8 in
              let tops = Array.make e.n Float.nan
              and bases = Array.make e.n Float.nan in
              for i = 0 to e.n - 1 do
                let c = cats.(i) and v = vals.(i) in
                if Float.is_finite c && Float.is_finite v then begin
                  let tbl = if v >= 0. then pos else neg in
                  let base =
                    Option.value (Hashtbl.find_opt tbl c) ~default:0.
                  in
                  bases.(i) <- base;
                  tops.(i) <- base +. v;
                  Hashtbl.replace tbl c (base +. v)
                end
              done;
              if horizontal then (
                e.ex <- E_num tops;
                e.ex2 <- E_num bases)
              else (
                e.ey <- E_num tops;
                e.ey2 <- E_num bases)
          | _ -> ())
      | _ -> ())
    evs;
  (* Hulls of the continuous scales. *)
  let hull n ch =
    match (ch, List.assoc_opt n kinds) with
    | E_num a, Some (((Linear | Log _ | Time _) as k), _) ->
        let u = List.assoc n !usages in
        Array.iter
          (fun v ->
            if placeable k v then (
              if v < u.hull_lo then u.hull_lo <- v;
              if v > u.hull_hi then u.hull_hi <- v))
          a
    | _ -> ()
  in
  Array.iter
    (fun (e : ev) ->
      hull e.sx e.ex;
      hull e.sx e.ex2;
      hull e.sy e.ey;
      hull e.sy e.ey2;
      match e.kind with K_raster _ -> hull e.sc (E_num e.ev_v) | _ -> ())
    evs;
  let scales =
    List.map
      (fun (n, (u : usage)) ->
        let kind, spec = List.assoc n kinds in
        let get f d =
          match spec with Some s -> Option.value (f s) ~default:d | None -> d
        in
        let dir = Option.value u.u_dir ~default:X in
        let fitted () =
          let lo = u.hull_lo and hi = u.hull_hi in
          let empty = not (lo <= hi) in
          let lo, hi =
            if empty then match kind with Log _ -> (1., 10.) | _ -> (0., 1.)
            else (lo, hi)
          in
          let zero =
            match kind with
            | Log _ -> false
            | _ -> get (fun s -> s.s_zero) u.zero
          in
          let lo, hi =
            if zero && not empty then (Float.min lo 0., Float.max hi 0.)
            else (lo, hi)
          in
          let lo, hi =
            if zero && lo = 0. && hi = 0. then (0., 1.) else (lo, hi)
          in
          let nice = get (fun s -> s.s_nice) (dir <> C && not u.no_nice) in
          if nice && not empty then Ticks.nice (family_of_kind kind) (lo, hi)
          else (lo, hi)
        in
        let dlo, dhi =
          match kind with
          | Band -> (0., 1.)
          | _ -> (
              match spec with
              | Some { s_domain = Some d; _ } -> d
              | _ -> fitted ())
        in
        let lo, hi =
          match (kind, View0.window n view) with
          | Band, _ | _, None -> (dlo, dhi)
          | _, Some w -> if dir = C then (dlo, dhi) else w
        in
        let cats =
          match List.assoc_opt n bands with Some c -> c | None -> [||]
        in
        {
          name = n;
          kind;
          dir;
          dlo;
          dhi;
          lo;
          hi;
          cats;
          reverse = get (fun s -> s.s_reverse) (kind = Band && dir = Y);
          padding =
            get (fun s -> s.s_padding) (Option.value u.u_padding ~default:0.1);
          step = (match spec with Some s -> s.s_step | None -> None);
          ticks = (match spec with Some s -> s.s_ticks | None -> None);
          colors = (match spec with Some s -> s.s_colors | None -> None);
          hidden = u.u_hidden;
        })
      !usages
  in
  List.iter
    (fun a ->
      match List.find_opt (fun s -> String.equal s.name a.a_scale) scales with
      | Some { dir = X | Y; _ } -> ()
      | Some { dir = C; _ } ->
          err "layout" "axis of %S, a color scale" a.a_scale
      | None -> err "layout" "axis of %S, which no role reads" a.a_scale)
    f.axes;
  List.iter
    (fun l ->
      match List.find_opt (fun s -> String.equal s.name l.l_scale) scales with
      | Some { dir = C; kind = Band; _ } -> ()
      | Some _ ->
          err "layout" "legend of %S, which is not a scale of categories"
            l.l_scale
      | None -> err "layout" "legend of %S, which no role reads" l.l_scale)
    f.legends;
  (* Colors: marks without a color and new categories take the palette's
     next color, in figure order. *)
  let palette = Array.of_list theme.palette in
  let next = ref 0 in
  let take () =
    let c =
      if Array.length palette = 0 then Color.default_fg
      else palette.(!next mod Array.length palette)
    in
    incr next;
    c
  in
  let cat_colors : (string * (string * Color.t) list ref) list ref = ref [] in
  let swatches = ref [] in
  let cat_color sc c ~pinned ~swatch =
    let s = List.find (fun (s : scale) -> String.equal s.name sc) scales in
    let tbl =
      match List.assoc_opt sc !cat_colors with
      | Some t -> t
      | None ->
          let t = ref [] in
          cat_colors := !cat_colors @ [ (sc, t) ];
          t
    in
    match List.assoc_opt c !tbl with
    | Some col -> col
    | None ->
        let i = band_index s c in
        if Float.is_nan i then Color.default_fg
        else
          let col =
            match (s.colors, pinned) with
            | Some l, _ ->
                let a = Array.of_list l in
                a.(Float.to_int i mod Array.length a)
            | None, Some p -> p
            | None, None -> take ()
          in
          tbl := !tbl @ [ (c, col) ];
          if not (List.mem_assoc (sc ^ "\000" ^ c) !swatches) then
            swatches := (sc ^ "\000" ^ c, swatch) :: !swatches;
          col
  in
  let swatch_of kind (g : Theme.glyphs) =
    match kind with
    | K_line _ | K_rule _ | K_spark { dots = `Braille } -> utf8 g.box.horizontal
    | K_dot { symbol = Some s; _ } -> s
    | K_dot _ | K_text _ | K_arrow _ -> g.point
    | _ -> if g.blocks then "█" else "#"
  in
  let mark_color =
    Array.map
      (fun (e : ev) ->
        let swatch = swatch_of e.kind theme.glyphs in
        match e.paint with
        | P_const c -> c
        | P_series (pinned, s) -> cat_color e.sc s ~pinned ~swatch
        | P_cat a ->
            Array.iter
              (fun c -> ignore (cat_color e.sc c ~pinned:None ~swatch))
              a;
            Color.default_fg
        | P_none -> (
            match e.kind with
            | K_text _ | K_arrow _ ->
                Option.value theme.label.fg ~default:Color.default_fg
            | K_rule _ -> Option.value theme.axis.fg ~default:Color.default_fg
            | K_raster _ | K_candle _ -> Color.default_fg
            | _ -> take ()))
      evs
  in
  {
    evs;
    scales;
    axes = f.axes;
    legends = f.legends;
    fig_title = f.fig_title;
    fig_margins = f.fig_margins;
    fig_frame = f.fig_frame;
    mark_color;
    cat_colors = List.map (fun (n, t) -> (n, !t)) !cat_colors;
    swatches = !swatches;
  }

(* {1 Laying out} *)

(* [bin l p] is the cell of the [l]-cell axis holding the normalized [p]. *)
let bin l p =
  let x = p *. Float.of_int l in
  if x < 1. then 0 else min (l - 1) (Float.to_int x)

type placed_axis = {
  s : scale;
  a : axis;
  side : side;
  ticks : Ticks.t;
  cells : int list;
  label_w : int;
  label_rows : int;
  heading : string option;
}

type placed_legend = {
  lg : legend;
  entries : (string * Color.t * string) list;
  lx : int;
  ly : int;
}

type layout = {
  r : resolved;
  theme : Theme.t;
  depth : Color.depth;
  wm : Matrix_text.width_method;
  width : int;
  height : int;
  plot : Grid.region;
  placed : placed_axis list;
  legends_placed : placed_legend list;
  title_at : (int * int) option;
  heading_row : int;
  lines : bool;
  view : View0.t;
}

let measure wm s = Matrix_text.measure ~width_method:wm ~tab_width:1 s

(* Band geometry on an axis of [l] cells: the extent the bands take, and the
   first and last cell of category [i]'s bar, counted from the axis's
   start. *)
let band_extent (s : scale) l =
  let n = Array.length s.cats in
  match s.step with Some st -> min l (n * st) | None -> l

let band_order (s : scale) i =
  if s.reverse then Array.length s.cats - 1 - i else i

let band_span (s : scale) l i =
  let n = max 1 (Array.length s.cats) in
  let bw = Float.of_int (band_extent s l) /. Float.of_int n in
  let k = Float.of_int (band_order s i) in
  let inner = bw *. (1. -. s.padding) in
  let a = (k *. bw) +. ((bw -. inner) /. 2.) in
  let c0 = Float.to_int (Float.round a) in
  let c1 = max c0 (Float.to_int (Float.round (a +. inner)) - 1) in
  (min c0 (l - 1), min c1 (l - 1))

let band_center (s : scale) l i =
  let n = max 1 (Array.length s.cats) in
  let bw = Float.of_int (band_extent s l) /. Float.of_int n in
  min (l - 1)
    (Float.to_int (Float.floor ((Float.of_int (band_order s i) +. 0.5) *. bw)))

let band_pos (s : scale) l i =
  let n = max 1 (Array.length s.cats) in
  (Float.of_int (band_order s i) +. 0.5)
  /. Float.of_int n
  *. Float.of_int (band_extent s l)
  /. Float.of_int (max 1 l)

let implied_axis s =
  {
    a_scale = s.name;
    a_side = None;
    a_show = not s.hidden;
    a_title = None;
    a_grid = `None;
    a_format = None;
    a_spacing = 0;
    a_style = None;
    a_tick_style = None;
    a_label_style = None;
    a_title_style = None;
  }

let axes_of (r : resolved) =
  let ys = ref 0 in
  List.filter_map
    (fun (s : scale) ->
      match s.dir with
      | C -> None
      | (X | Y) as d ->
          let a =
            match
              List.find_opt (fun a -> String.equal a.a_scale s.name) r.axes
            with
            | Some a -> a
            | None -> implied_axis s
          in
          let side =
            match (a.a_side, d) with
            | Some ((`Bottom | `Top) as sd), X -> sd
            | Some ((`Left | `Right) as sd), Y -> sd
            | Some _, _ ->
                err "layout" "axis of %S on a side across its direction" s.name
            | None, X -> `Bottom
            | None, _ ->
                incr ys;
                if !ys = 1 then `Left
                else if !ys = 2 then `Right
                else err "layout" "a third y scale %S without a side" s.name
          in
          Some (s, a, side))
    r.scales

let empty_ticks = { Ticks.major = []; note = None }

let axis_ticks wm (s : scale) a ~horizontal ~length =
  if not a.a_show then empty_ticks
  else
    let length = Float.of_int (max 1 length) in
    let measure =
      if horizontal then fun l -> Float.of_int (measure wm l + 2)
      else fun _ -> 2.
    in
    match s.kind with
    | Band ->
        let n = Array.length s.cats in
        let k =
          if n = 0 then 1
          else
            let l = Float.to_int length in
            let spread = Float.of_int (band_extent s l) in
            Ticks.stride ~length:spread ~measure s.cats
        in
        {
          Ticks.major =
            List.filter_map
              (fun i ->
                if i mod k = 0 then
                  Some
                    {
                      Ticks.value = Float.of_int i;
                      label = s.cats.(i);
                      context = None;
                    }
                else None)
              (List.init n Fun.id);
          note = None;
        }
    | _ -> (
        let labels =
          match a.a_format with
          | Some f -> fun vs -> (Array.mapi (fun i v -> (f i v, None)) vs, None)
          | None -> Ticks.label (family s)
        in
        match s.ticks with
        | Some vs -> Ticks.of_values ~norm:(norm s) ~labels vs (s.lo, s.hi)
        | None ->
            Ticks.choose (family s) ~norm:(norm s) ~labels ~length
              ~spacing:(Float.of_int a.a_spacing) ~measure (s.lo, s.hi))

(* Which parts of the guides take room. A region too small drops them from
   the outermost in. *)
type tiers = {
  t_title : bool;
  t_legends : bool;
  t_titles : bool;
  t_labels : bool;
  t_lines : bool;
  t_margins : bool;
}

let all_tiers =
  {
    t_title = true;
    t_legends = true;
    t_titles = true;
    t_labels = true;
    t_lines = true;
    t_margins = true;
  }

let drop t =
  if t.t_title then Some { t with t_title = false }
  else if t.t_legends then Some { t with t_legends = false }
  else if t.t_titles then Some { t with t_titles = false }
  else if t.t_labels then Some { t with t_labels = false }
  else if t.t_lines then Some { t with t_lines = false }
  else if t.t_margins then Some { t with t_margins = false }
  else None

let legend_entries (r : resolved) sc =
  match List.assoc_opt sc r.cat_colors with
  | None -> []
  | Some l ->
      List.map
        (fun (c, col) ->
          let sw =
            Option.value
              (List.assoc_opt (sc ^ "\000" ^ c) r.swatches)
              ~default:"█"
          in
          (c, col, sw))
        l

let legends_of (r : resolved) =
  List.filter_map
    (fun (s : scale) ->
      match (s.dir, s.kind) with
      | C, Band ->
          let lg =
            match
              List.find_opt (fun l -> String.equal l.l_scale s.name) r.legends
            with
            | Some l -> l
            | None ->
                {
                  l_scale = s.name;
                  l_side = `Inside `Top_right;
                  l_show = true;
                  l_title = None;
                  l_direction = `Rows;
                }
          in
          if lg.l_show then Some lg else None
      | _ -> None)
    r.scales

(* The size of a legend's block: its entries as "swatch label", in rows or on
   one row two cells apart, under its title. *)
let legend_size wm lg entries =
  let ew (l, _, sw) = measure wm sw + 1 + measure wm l in
  let tw = match lg.l_title with Some t -> measure wm t | None -> 0 in
  let th = match lg.l_title with Some _ -> 1 | None -> 0 in
  match lg.l_direction with
  | `Rows ->
      ( max tw (List.fold_left (fun m e -> max m (ew e)) 0 entries),
        th + List.length entries )
  | `Columns ->
      let w =
        List.fold_left (fun m e -> m + ew e) 0 entries
        + (2 * max 0 (List.length entries - 1))
      in
      (max tw w, th + 1)

let place ~theme ~depth ~wm ~width ~height ~view (r : resolved) =
  let width = max 0 width and height = max 0 height in
  let axes = axes_of r in
  let legends =
    List.map (fun lg -> (lg, legend_entries r lg.l_scale)) (legends_of r)
  in
  let th = theme in
  let solve tiers =
    let shown (_, a, _) = a.a_show && tiers.t_lines in
    let on side =
      List.filter (fun ((_, _, sd) as ax) -> sd = side && shown ax) axes
    in
    let has side = on side <> [] in
    let m =
      if tiers.t_margins then r.fig_margins
      else { top = 0; right = 0; bottom = 0; left = 0; inset = 0 }
    in
    let inset = m.inset in
    let frame side = r.fig_frame && tiers.t_lines && not (has side) in
    let title_rows = if tiers.t_title && r.fig_title <> None then 1 else 0 in
    let outside side =
      if not tiers.t_legends then (0, 0)
      else
        List.fold_left
          (fun (w, h) (lg, es) ->
            if lg.l_side = side then
              let lw, lh = legend_size wm lg es in
              match side with
              | `Left | `Right -> (w + lw + 1, h)
              | _ -> (w, h + lh)
            else (w, h))
          (0, 0) legends
    in
    (* An axis's heading is its title, then its labels' note. The note goes
       with the labels, since they read wrong without it. *)
    let heading a (t : Ticks.t) =
      let title = if tiers.t_titles then a.a_title else None in
      let note = if tiers.t_labels then t.note else None in
      match (title, note) with
      | None, None -> None
      | Some x, None -> Some x
      | None, Some n -> Some n
      | Some x, Some n -> Some (x ^ " " ^ n)
    in
    let tick_offset (s : scale) l v =
      match s.kind with
      | Band -> band_center s l (Float.to_int v)
      | _ -> bin l (norm s v)
    in
    let label_width (t : Ticks.t) =
      List.fold_left
        (fun m (k : Ticks.tick) ->
          let w = measure wm k.label in
          max m
            (match k.context with Some c -> w + 1 + measure wm c | None -> w))
        0 t.major
    in
    let label_rows (t : Ticks.t) =
      if List.exists (fun (k : Ticks.tick) -> k.context <> None) t.major then 2
      else 1
    in
    (* The cells an axis takes beside the plot: its line and ticks, its
       labels, and an x axis's heading. *)
    let room ticks side =
      List.fold_left
        (fun acc (((_, a, sd) as ax), (t : Ticks.t)) ->
          if sd <> side || not (shown ax) then acc
          else
            let lines = 1 + th.Theme.tick_length in
            let labels = tiers.t_labels && t.major <> [] in
            match side with
            | `Left | `Right ->
                max acc
                  (lines
                  + if labels then th.label_padding + label_width t else 0)
            | `Top | `Bottom ->
                let titled = heading a t <> None in
                max acc
                  (lines
                  + (if labels then label_rows t else 0)
                  + if titled then 1 else 0))
        0 ticks
    in
    let margins ticks =
      let y_heading =
        List.exists
          (fun (((s : scale), a, sd), t) ->
            (sd = `Left || sd = `Right)
            && shown (s, a, sd)
            && heading a t <> None)
          ticks
      in
      let lw, _ = outside `Left and rw, _ = outside `Right in
      let _, tl = outside `Top and _, bl = outside `Bottom in
      let base side =
        let n = room ticks side in
        if n > 0 then n + inset else if frame side then 1 + inset else 0
      in
      let left = base `Left + lw and right = base `Right + rw in
      let bottom = base `Bottom + bl in
      let top = base `Top + tl + title_rows + if y_heading then 1 else 0 in
      (* x labels that run past the plot's ends widen the margins they
         cross. *)
      let pw = width - left - right in
      let over_l = ref 0 and over_r = ref 0 in
      if tiers.t_labels && pw > 0 then
        List.iter
          (fun (((s : scale), _, sd), (t : Ticks.t)) ->
            if sd = `Bottom || sd = `Top then
              List.iter
                (fun (k : Ticks.tick) ->
                  let c = tick_offset s pw k.value in
                  let w =
                    max (measure wm k.label)
                      (match k.context with
                      | Some c -> measure wm c
                      | None -> 0)
                  in
                  let start = c - ((w - 1) / 2) in
                  over_l := max !over_l (-start);
                  over_r := max !over_r (start + w - pw))
                t.major)
          ticks;
      {
        top = max top m.top;
        bottom = max bottom m.bottom;
        left = max (left + max 0 (!over_l - left)) m.left;
        right = max (right + max 0 (!over_r - right)) m.right;
        inset;
      }
    in
    (* The search runs once per axis and length: the margins converge in a
       few rounds over the same lengths. *)
    let chosen = Hashtbl.create 8 in
    let ticks_at (mg : margins) =
      let pw = max 1 (width - mg.left - mg.right)
      and ph = max 1 (height - mg.top - mg.bottom) in
      List.mapi
        (fun i ((s, a, sd) as ax) ->
          let horizontal = sd = `Bottom || sd = `Top in
          let length = if horizontal then pw else ph in
          let t =
            if not (tiers.t_labels || a.a_grid <> `None) then empty_ticks
            else
              match Hashtbl.find_opt chosen (i, length) with
              | Some t -> t
              | None ->
                  let t = axis_ticks wm s a ~horizontal ~length in
                  Hashtbl.add chosen (i, length) t;
                  t
          in
          (ax, t))
        axes
    in
    let zero =
      { top = title_rows; right = 0; bottom = 0; left = 0; inset = 0 }
    in
    let rec iterate mg seen n =
      let ticks = ticks_at mg in
      let mg' = margins ticks in
      if mg' = mg || n = 0 then (mg, ticks)
      else
        let biggest =
          List.fold_left
            (fun a b ->
              {
                top = max a.top b.top;
                right = max a.right b.right;
                bottom = max a.bottom b.bottom;
                left = max a.left b.left;
                inset = max a.inset b.inset;
              })
            mg' seen
        in
        if n = 1 then (biggest, ticks_at biggest)
        else iterate mg' (mg' :: seen) (n - 1)
    in
    let mg, ticks = iterate (margins (ticks_at zero)) [] 4 in
    let mg, ticks =
      let mg' = margins ticks in
      if mg' = mg then (mg, ticks)
      else
        let mg =
          {
            top = max mg.top mg'.top;
            right = max mg.right mg'.right;
            bottom = max mg.bottom mg'.bottom;
            left = max mg.left mg'.left;
            inset = mg.inset;
          }
        in
        (mg, ticks_at mg)
    in
    let plot =
      {
        Grid.x = mg.left;
        y = mg.top;
        width = width - mg.left - mg.right;
        height = height - mg.top - mg.bottom;
      }
    in
    (tiers, plot, mg, ticks, heading)
  in
  let rec fit tiers =
    let ((_, plot, _, _, _) as sol) = solve tiers in
    if plot.width >= 1 && plot.height >= 1 then sol
    else match drop tiers with Some t -> fit t | None -> sol
  in
  let tiers, plot, mg, ticks, heading = fit all_tiers in
  let plot =
    { plot with Grid.width = max 0 plot.width; height = max 0 plot.height }
  in
  let placed =
    List.map
      (fun ((s, a, side), (t : Ticks.t)) ->
        let horizontal = side = `Bottom || side = `Top in
        let l = if horizontal then plot.width else plot.height in
        let cells =
          if l <= 0 then []
          else
            List.map
              (fun (k : Ticks.tick) ->
                match s.kind with
                | Band ->
                    let o = band_center s l (Float.to_int k.value) in
                    if horizontal then plot.x + o else plot.y + l - 1 - o
                | _ ->
                    let o = bin l (norm s k.value) in
                    if horizontal then plot.x + o else plot.y + l - 1 - o)
              t.major
        in
        let label_w =
          List.fold_left
            (fun m (k : Ticks.tick) -> max m (measure wm k.label))
            0 t.major
        in
        let label_rows =
          if List.exists (fun (k : Ticks.tick) -> k.context <> None) t.major
          then 2
          else 1
        in
        let a = if tiers.t_lines then a else { a with a_show = false } in
        let t =
          if tiers.t_labels then t
          else
            { t with Ticks.major = (if a.a_grid <> `None then t.major else []) }
        in
        {
          s;
          a;
          side;
          ticks = t;
          cells;
          label_w;
          label_rows;
          heading = heading a t;
        })
      ticks
  in
  (* Legends *)
  let legends_placed =
    if not tiers.t_legends then []
    else
      let off = Hashtbl.create 4 in
      let bump side n =
        let v = Option.value (Hashtbl.find_opt off side) ~default:0 in
        Hashtbl.replace off side (v + n);
        v
      in
      List.filter_map
        (fun (lg, entries) ->
          let lw, lh = legend_size wm lg entries in
          let at =
            match lg.l_side with
            | `Inside c ->
                if lw > plot.width || lh > plot.height then None
                else
                  let x =
                    match c with
                    | `Top_left | `Bottom_left -> plot.x
                    | _ -> plot.x + plot.width - lw
                  in
                  let y =
                    match c with
                    | `Top_left | `Top_right -> plot.y
                    | _ -> plot.y + plot.height - lh
                  in
                  Some (x, y)
            | `Right ->
                Some (width - mg.right + 1 + bump `Right (lw + 1), plot.y)
            | `Left -> Some (bump `Left (lw + 1), plot.y)
            | `Top ->
                Some
                  ( plot.x,
                    (if tiers.t_title && r.fig_title <> None then 1 else 0)
                    + bump `Top lh )
            | `Bottom ->
                Some
                  (plot.x, height - mg.bottom + mg.bottom - lh - bump `Bottom lh)
          in
          Option.map (fun (lx, ly) -> { lg; entries; lx; ly }) at)
        legends
  in
  let title_at =
    match r.fig_title with
    | Some t when tiers.t_title && plot.width > 0 ->
        let w = measure wm t in
        Some (max 0 (plot.x + ((plot.width - w) / 2)), 0)
    | _ -> None
  in
  let heading_row =
    let top_axis = List.exists (fun p -> p.side = `Top && p.a.a_show) placed in
    let top_rows =
      List.fold_left
        (fun m p ->
          if p.side = `Top && p.a.a_show then
            max m
              (1 + theme.Theme.tick_length
              + if p.ticks.major <> [] then p.label_rows else 0)
          else m)
        0 placed
    in
    let frame_top = r.fig_frame && tiers.t_lines && not top_axis in
    plot.y - mg.inset - top_rows - (if frame_top then 1 else 0) - 1
  in
  {
    r;
    theme;
    depth;
    wm;
    width;
    height;
    plot;
    placed;
    legends_placed;
    title_at;
    heading_row;
    lines = tiers.t_lines;
    view;
  }

(* {1 Color maps} *)

(* Oklab (Björn Ottosson, 2020) of an sRGB color. *)
let oklab c =
  let r, g, b = Color.to_rgb c in
  let lin v =
    let v = Float.of_int v /. 255. in
    if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4
  in
  let r = lin r and g = lin g and b = lin b in
  let l = (0.4122214708 *. r) +. (0.5363325363 *. g) +. (0.0514459929 *. b) in
  let m = (0.2119034982 *. r) +. (0.6806995451 *. g) +. (0.1073969566 *. b) in
  let s = (0.0883024619 *. r) +. (0.2817188376 *. g) +. (0.6299787005 *. b) in
  let l = Float.cbrt l and m = Float.cbrt m and s = Float.cbrt s in
  ( (0.2104542553 *. l) +. (0.7936177850 *. m) -. (0.0040720468 *. s),
    (1.9779984951 *. l) -. (2.4285922050 *. m) +. (0.4505937099 *. s),
    (0.0259040371 *. l) +. (0.7827717662 *. m) -. (0.8086757660 *. s) )

let of_oklab (l, a, b) =
  let l' = l +. (0.3963377774 *. a) +. (0.2158037573 *. b)
  and m' = l -. (0.1055613458 *. a) -. (0.0638541728 *. b)
  and s' = l -. (0.0894841775 *. a) -. (1.2914855480 *. b) in
  let l = l' *. l' *. l' and m = m' *. m' *. m' and s = s' *. s' *. s' in
  let r = (4.0767416621 *. l) -. (3.3077115913 *. m) +. (0.2309699292 *. s)
  and g = (-1.2684380046 *. l) +. (2.6097574011 *. m) -. (0.3413193965 *. s)
  and b = (-0.0041960863 *. l) -. (0.7034186147 *. m) +. (1.7076147010 *. s) in
  let enc v =
    let v = Float.max 0. (Float.min 1. v) in
    let v =
      if v <= 0.0031308 then 12.92 *. v
      else (1.055 *. Float.pow v (1. /. 2.4)) -. 0.055
    in
    Float.to_int (Float.round (v *. 255.))
  in
  Color.of_rgb (enc r) (enc g) (enc b)

(* The xterm palette's entries 16 to 255, as [Ansi.Color] states them. *)
let entry_rgb idx =
  let level = function
    | 0 -> 0
    | 1 -> 95
    | 2 -> 135
    | 3 -> 175
    | 4 -> 215
    | _ -> 255
  in
  if idx < 232 then
    let n = idx - 16 in
    (level (n / 36), level (n / 6 mod 6), level (n mod 6))
  else
    let g = 8 + ((idx - 232) * 10) in
    (g, g, g)

let entries_oklab =
  lazy
    (Array.init 240 (fun i ->
         let r, g, b = entry_rgb (i + 16) in
         oklab (Color.of_rgb r g b)))

(* A continuous color map: [t] in \[[0];[1]\] to a color, interpolating
   [stops] in Oklab. At 256 colors the map is quantized as a sequence: its
   samples, walked from dark to light, each take the nearest palette entry
   no darker than the previous one, so lightness never reverses. *)
let color_map ~depth stops =
  let stops = Array.of_list (List.map oklab stops) in
  let n = Array.length stops in
  let at t =
    if n = 1 then stops.(0)
    else
      let t = Float.max 0. (Float.min 1. t) in
      let x = t *. Float.of_int (n - 1) in
      let i = min (n - 2) (Float.to_int x) in
      let f = x -. Float.of_int i in
      let l0, a0, b0 = stops.(i) and l1, a1, b1 = stops.(i + 1) in
      (l0 +. (f *. (l1 -. l0)), a0 +. (f *. (a1 -. a0)), b0 +. (f *. (b1 -. b0)))
  in
  match depth with
  | `Ansi256 ->
      let k = 64 in
      let samples =
        Array.init k (fun i -> at (Float.of_int i /. Float.of_int (k - 1)))
      in
      let rising =
        let l0, _, _ = samples.(0) and l1, _, _ = samples.(k - 1) in
        l1 >= l0
      in
      let entries = Lazy.force entries_oklab in
      let chosen = Array.make k 16 in
      let floor_l = ref neg_infinity in
      let order =
        if rising then List.init k Fun.id else List.init k (fun i -> k - 1 - i)
      in
      List.iter
        (fun i ->
          let l, a, b = samples.(i) in
          let best = ref (-1) and bd = ref infinity in
          Array.iteri
            (fun j (l', a', b') ->
              if l' >= !floor_l then
                let d =
                  ((l -. l') ** 2.) +. ((a -. a') ** 2.) +. ((b -. b') ** 2.)
                in
                if d < !bd then (
                  bd := d;
                  best := j))
            entries;
          let j = if !best < 0 then 239 else !best in
          let l', _, _ = entries.(j) in
          floor_l := l';
          chosen.(i) <- j + 16)
        order;
      fun t ->
        let t = Float.max 0. (Float.min 1. t) in
        Color.indexed
          chosen.(Float.to_int (Float.round (t *. Float.of_int (k - 1))))
  | _ -> fun t -> of_oklab (at t)

(* {1 Marks into ink} *)

type ctx = { lay : layout; ink : Ink.t; pw : int; ph : int }

let scale_named (l : layout) n = find_scale l.r n

(* The normalized position of [v] on [s], over an axis of [len] cells. *)
let pos (s : scale) len v =
  match s.kind with
  | Band ->
      if Float.is_nan v then Float.nan else band_pos s len (Float.to_int v)
  | _ -> norm s v

(* [positions s len a] is [pos s len] of each value of [a], in one loop that
   allocates the result only. *)
let positions (s : scale) len a =
  let n = Array.length a in
  let out = Array.create_float n in
  (match s.kind with
  | Band ->
      for i = 0 to n - 1 do
        let v = Array.unsafe_get a i in
        out.(i) <-
          (if Float.is_nan v then Float.nan else band_pos s len (Float.to_int v))
      done
  | Log _ ->
      let lo = Float.log s.lo and hi = Float.log s.hi in
      let inv = if hi = lo then 0. else 1. /. (hi -. lo) in
      for i = 0 to n - 1 do
        let v = Array.unsafe_get a i in
        out.(i) <-
          (if not (v > 0.) then Float.nan
           else if inv = 0. then 0.5
           else (Float.log v -. lo) *. inv)
      done
  | Linear | Time _ ->
      let lo = s.lo and hi = s.hi in
      let inv = if hi = lo then 0. else 1. /. (hi -. lo) in
      for i = 0 to n - 1 do
        let v = Array.unsafe_get a i in
        out.(i) <-
          (if inv = 0. then if Float.is_finite v then 0.5 else v
           else (v -. lo) *. inv)
      done);
  if s.reverse && s.kind <> Band then
    for i = 0 to n - 1 do
      out.(i) <- 1. -. out.(i)
    done;
  out

let ch_get = function E_num a -> a | _ -> [||]

let stroke (th : Theme.t) d =
  let d =
    match d with
    | Some d -> d
    | None -> (th.dots :> [ dots | `Steps | `Density ])
  in
  if not th.glyphs.blocks then `Cell
  else
    match d with
    | `Cell -> `Cell
    | `Steps -> `Steps
    | `Density -> `Density
    | `Half -> `Res Ink.Half
    | `Quadrant -> `Res Ink.Quadrant
    | `Sextant -> `Res Ink.Sextant
    | `Braille -> `Res Ink.Braille
    | `Octant -> `Res Ink.Octant

let row_colors (l : layout) e =
  let base = l.r.mark_color.(e.mi) in
  match e.paint with
  | P_cat a ->
      let tbl = Option.value (List.assoc_opt e.sc l.r.cat_colors) ~default:[] in
      Some
        (Array.map
           (fun c -> Option.value (List.assoc_opt c tbl) ~default:base)
           a)
  | _ -> None

let color_of colors base i = match colors with Some a -> a.(i) | None -> base

(* Liang-Barsky clipping of a segment to the unit square. *)
let clip x0 y0 x1 y1 =
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let t0 = ref 0. and t1 = ref 1. in
  let ok = ref true in
  let edge p q =
    if p = 0. then (if q < 0. then ok := false)
    else
      let r = q /. p in
      if p < 0. then (if r > !t1 then ok := false else if r > !t0 then t0 := r)
      else if r < !t0 then ok := false
      else if r < !t1 then t1 := r
  in
  edge (-.dx) x0;
  edge dx (1. -. x0);
  edge (-.dy) y0;
  edge dy (1. -. y0);
  if !ok then
    Some
      ( x0 +. (!t0 *. dx),
        y0 +. (!t0 *. dy),
        x0 +. (!t1 *. dx),
        y0 +. (!t1 *. dy) )
  else None

let bresenham x0 y0 x1 y1 f =
  let dx = abs (x1 - x0) and dy = -abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 and sy = if y0 < y1 then 1 else -1 in
  let rec go x y err =
    f x y;
    if x <> x1 || y <> y1 then
      let e2 = 2 * err in
      let x, err = if e2 >= dy then (x + sx, err + dy) else (x, err) in
      let y, err = if e2 <= dx then (y + sy, err + dx) else (y, err) in
      go x y err
  in
  go x0 y0 (dx + dy)

(* Dashes count dots or cells along a path: on, then off. *)
let dash_pattern = function
  | `Solid -> (1, 0)
  | `Dashed -> (4, 3)
  | `Dotted -> (1, 2)

let inside p = p >= 0. && p <= 1.

(* [path ctx res e ~xs ~ys f] calls [f x y i] for each dot (or cell) of the
   line through the rows of [e], [i] the row nearer it, breaking at missing
   rows. *)
let path c ~cols ~rows ~xs ~ys f =
  let w = c.pw * cols and h = c.ph * rows in
  let dx p = bin w p and dy p = h - 1 - bin h p in
  let n = Array.length xs in
  let valid i = Float.is_finite xs.(i) && Float.is_finite ys.(i) in
  for i = 0 to n - 1 do
    if valid i then begin
      let alone =
        (i = 0 || not (valid (i - 1))) && (i = n - 1 || not (valid (i + 1)))
      in
      if alone && inside xs.(i) && inside ys.(i) then
        f (dx xs.(i)) (dy ys.(i)) i;
      if i > 0 && valid (i - 1) then
        match clip xs.(i - 1) ys.(i - 1) xs.(i) ys.(i) with
        | None -> ()
        | Some (x0, y0, x1, y1) ->
            let a = dx x0 and b = dy y0 and a' = dx x1 and b' = dy y1 in
            let len = max (abs (a' - a)) (abs (b' - b)) in
            let k = ref 0 in
            bresenham a b a' b' (fun x y ->
                let row = if 2 * !k < len then i - 1 else i in
                incr k;
                f x y row)
    end
  done

let line_glyph (g : Theme.glyphs) dash ~dx ~dy =
  let h, v =
    match dash with
    | `Solid -> (utf8 g.box.horizontal, utf8 g.box.vertical)
    | `Dashed -> g.dashed
    | `Dotted -> g.dotted
  in
  let ax = abs dx and ay = 2 * abs dy in
  if ay * 2 < ax || (dx = 0 && dy = 0) then h
  else if ax * 2 < ay then v
  else if dx > 0 = (dy < 0) then fst g.diagonals
  else snd g.diagonals

(* Lines at cell resolution: each cell of a segment takes the glyph of the
   segment's direction. *)
let cell_line c ~dash ~xs ~ys ~fg ~attrs ~owner =
  let g = c.lay.theme.glyphs in
  let on, off = dash_pattern dash in
  let count = ref 0 in
  let n = Array.length xs in
  let valid i = Float.is_finite xs.(i) && Float.is_finite ys.(i) in
  let cx p = bin c.pw p and cy p = c.ph - 1 - bin c.ph p in
  for i = 0 to n - 1 do
    if valid i then
      if i > 0 && valid (i - 1) then
        match clip xs.(i - 1) ys.(i - 1) xs.(i) ys.(i) with
        | None -> ()
        | Some (x0, y0, x1, y1) ->
            let a = cx x0 and b = cy y0 and a' = cx x1 and b' = cy y1 in
            let s = line_glyph g `Solid ~dx:(a' - a) ~dy:(b' - b) in
            let s =
              if s = utf8 g.box.horizontal || s = utf8 g.box.vertical then
                line_glyph g dash ~dx:(a' - a) ~dy:(b' - b)
              else s
            in
            bresenham a b a' b' (fun x y ->
                if off = 0 || !count mod (on + off) < on then
                  Ink.glyph c.ink ~x ~y s ~fg:(fg i) ~attrs ~owner:(owner i);
                incr count)
      else if
        (i = n - 1 || not (valid (i + 1))) && inside xs.(i) && inside ys.(i)
      then
        Ink.glyph c.ink
          ~x:(cx xs.(i))
          ~y:(cy ys.(i))
          g.point ~fg:(fg i) ~attrs ~owner:(owner i)
  done

(* The glyph of a cell whose line arms point up, down, left and right. *)
let arms_glyph (b : Grid.Border.t) arms =
  let u = arms land 1 <> 0
  and d = arms land 2 <> 0
  and l = arms land 4 <> 0
  and r = arms land 8 <> 0 in
  let g =
    match (u, d, l, r) with
    | true, true, true, true -> b.cross
    | false, true, true, true -> b.top_t
    | true, false, true, true -> b.bottom_t
    | true, true, false, true -> b.left_t
    | true, true, true, false -> b.right_t
    | false, true, false, true -> b.top_left
    | false, true, true, false -> b.top_right
    | true, false, false, true -> b.bottom_left
    | true, false, true, false -> b.bottom_right
    | true, _, false, false | false, true, false, false -> b.vertical
    | _ -> b.horizontal
  in
  utf8 g

(* Lines of steps: horizontal from a point to the next one's column, then
   vertical to its row, joined with box-drawing corners. *)
let step_line c ~xs ~ys ~fg ~attrs ~owner =
  let arms = Array.make (c.pw * c.ph) 0
  and who = Array.make (c.pw * c.ph) (-1) in
  let add x y a i =
    if x >= 0 && y >= 0 && x < c.pw && y < c.ph then (
      let k = (y * c.pw) + x in
      arms.(k) <- arms.(k) lor a;
      who.(k) <- i)
  in
  let cx p = bin c.pw (Float.max 0. (Float.min 1. p))
  and cy p = c.ph - 1 - bin c.ph (Float.max 0. (Float.min 1. p)) in
  let n = Array.length xs in
  let valid i = Float.is_finite xs.(i) && Float.is_finite ys.(i) in
  for i = 0 to n - 1 do
    if valid i then
      let x = cx xs.(i) and y = cy ys.(i) in
      if i > 0 && valid (i - 1) then begin
        let x0 = cx xs.(i - 1) and y0 = cy ys.(i - 1) in
        let lo = min x0 x and hi = max x0 x in
        for k = lo to hi do
          let a = (if k > lo then 4 else 0) lor if k < hi then 8 else 0 in
          add k y0 (if lo = hi then 0 else a) (i - 1)
        done;
        if y <> y0 then begin
          let lo = min y0 y and hi = max y0 y in
          for k = lo to hi do
            let a = (if k > lo then 1 else 0) lor if k < hi then 2 else 0 in
            add x k a i
          done
        end
        else add x y 0 i
      end
      else add x y 0 i
  done;
  Array.iteri
    (fun k i ->
      if i >= 0 then
        let a = arms.(k) in
        let a = if a = 0 then 12 else a in
        Ink.glyph c.ink ~x:(k mod c.pw) ~y:(k / c.pw)
          (arms_glyph c.lay.theme.glyphs.box a)
          ~fg:(fg i) ~attrs ~owner:(owner i))
    who

let lower = [| ""; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]
let upper = [| ""; "▔"; "🮂"; "🮃"; "▀"; "🮄"; "🮅"; "🮆"; "█" |]
let left = [| ""; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
let right = [| ""; "▕"; "🮇"; "🮈"; "▐"; "🮉"; "🮊"; "🮋"; "█" |]

(* A bar from [e0] (its base) to [e1], in eighths of a cell counted along the
   axis from its start, over the [across] cells [c0..c1]. A partial cell at
   the base of a segment stacked on another paints the background, so the
   glyph of the segment below shows in it. *)
let bar c ~horizontal ~eighths ~c0 ~c1 ~e0 ~e1 ~first ~fg ~attrs ~owner =
  let len = if horizontal then c.pw else c.ph in
  let round e = if eighths then e else 8 * ((e + 4) / 8) in
  let e0 = round e0 and e1 = round e1 in
  let lo = max 0 (min e0 e1) and hi = min (8 * len) (max e0 e1) in
  let positive = e1 >= e0 in
  let shades = c.lay.theme.glyphs.shades in
  let full = shades.(Array.length shades - 1) in
  let grow = if horizontal then left else lower in
  let shrink = if horizontal then right else upper in
  (* Cell [k] along the axis and [a] across it, written without closures:
     sparks draw a bar per column each frame. *)
  let fill k a =
    if horizontal then Ink.fill c.ink ~x:k ~y:a ~bg:fg ~owner
    else Ink.fill c.ink ~x:a ~y:(len - 1 - k) ~bg:fg ~owner
  in
  let glyph k a s =
    if horizontal then Ink.glyph c.ink ~x:k ~y:a s ~fg ~attrs ~owner
    else Ink.glyph c.ink ~x:a ~y:(len - 1 - k) s ~fg ~attrs ~owner
  in
  (* The cell of a diverging bar's zero holds the other side's glyph. *)
  let free k a =
    let o =
      if horizontal then Ink.glyph_owner c.ink ~x:k ~y:a
      else Ink.glyph_owner c.ink ~x:a ~y:(len - 1 - k)
    in
    o < 0 || Ink.owner_mark o <> Ink.owner_mark owner
  in
  if hi > lo then
    for k = lo / 8 to (hi - 1) / 8 do
      let a = max lo (8 * k) and b = min hi ((8 * k) + 8) in
      let cov = b - a in
      for x = c0 to c1 do
        if cov = 8 then glyph k x full
        else if a = 8 * k then
          (* touches the cell's start *)
          if (not positive) && b = e0 && not (first && free k x) then fill k x
          else glyph k x grow.(cov)
        else if b = (8 * k) + 8 then
          if positive && a = e0 && not (first && free k x) then fill k x
          else glyph k x shrink.(cov)
        else glyph k x grow.(b - (8 * k))
      done
    done

let contrast col =
  let r, g, b = Color.to_rgb col in
  if (299 * r) + (587 * g) + (114 * b) > 140_000 then Color.black
  else Color.white

let ink_mark c (e : ev) =
  let l = c.lay in
  let th = l.theme in
  let sx = scale_named l e.sx and sy = scale_named l e.sy in
  let px v = match sx with Some s -> pos s c.pw v | None -> Float.nan in
  let py v = match sy with Some s -> pos s c.ph v | None -> Float.nan in
  let base = l.r.mark_color.(e.mi) in
  let colors = row_colors l e in
  let fg i = color_of colors base i in
  let attrs = e.attrs in
  let owner i = Ink.owner ~mark:e.mi ~row:(e.row_of i) in
  let pxs a =
    match sx with
    | Some s -> positions s c.pw a
    | None -> Array.map (fun _ -> Float.nan) a
  in
  let pys a =
    match sy with
    | Some s -> positions s c.ph a
    | None -> Array.map (fun _ -> Float.nan) a
  in
  let xs () = pxs (ch_get e.ex) and ys () = pys (ch_get e.ey) in
  match e.kind with
  | K_line { dots; dash } -> (
      let xs = xs () and ys = ys () in
      match stroke th (dots :> [ dots | `Steps | `Density ] option) with
      | `Res r ->
          let on, off = dash_pattern dash in
          let k = ref 0 in
          path c ~cols:(Ink.cols r) ~rows:(Ink.rows r) ~xs ~ys (fun x y i ->
              if off = 0 || !k mod (on + off) < on then
                Ink.dot c.ink r ~x ~y ~fg:(fg i) ~attrs ~owner:(owner i);
              incr k)
      | `Steps -> step_line c ~xs ~ys ~fg ~attrs ~owner
      | `Cell | `Density -> cell_line c ~dash ~xs ~ys ~fg ~attrs ~owner)
  | K_area { dots } ->
      let xs = xs () and ys = ys () in
      let y2s =
        match e.ey2 with
        | E_num a -> pys a
        | _ ->
            let z =
              match sy with
              | Some s -> Float.max 0. (Float.min 1. (pos s c.ph 0.))
              | None -> 0.
            in
            Array.make e.n z
      in
      let cols, rows, res =
        match
          stroke th
            (Some (Option.value dots ~default:`Cell)
              :> [ dots | `Steps | `Density ] option)
        with
        | `Res r -> (Ink.cols r, Ink.rows r, Some r)
        | _ -> (1, 1, None)
      in
      let w = c.pw * cols and h = c.ph * rows in
      let paint x yt yb i =
        let clamp p = Float.max 0. (Float.min 1. p) in
        let t = h - 1 - bin h (clamp yt) and b = h - 1 - bin h (clamp yb) in
        let t, b = (min t b, max t b) in
        for y = t to b do
          match res with
          | Some r -> Ink.dot c.ink r ~x ~y ~fg:(fg i) ~attrs ~owner:(owner i)
          | None -> Ink.fill c.ink ~x ~y ~bg:(fg i) ~owner:(owner i)
        done
      in
      let valid i =
        Float.is_finite xs.(i)
        && Float.is_finite ys.(i)
        && Float.is_finite y2s.(i)
      in
      for i = 1 to e.n - 1 do
        if valid i && valid (i - 1) then
          let x0 = xs.(i - 1) and x1 = xs.(i) in
          let d0 = bin w (Float.max 0. (Float.min 1. x0))
          and d1 = bin w (Float.max 0. (Float.min 1. x1)) in
          if Float.max x0 x1 >= 0. && Float.min x0 x1 <= 1. then
            for d = min d0 d1 to max d0 d1 do
              let xm = (Float.of_int d +. 0.5) /. Float.of_int w in
              let f = if x1 = x0 then 0. else (xm -. x0) /. (x1 -. x0) in
              let f = Float.max 0. (Float.min 1. f) in
              let yt = ys.(i - 1) +. (f *. (ys.(i) -. ys.(i - 1))) in
              let yb = y2s.(i - 1) +. (f *. (y2s.(i) -. y2s.(i - 1))) in
              paint d yt yb (if f < 0.5 then i - 1 else i)
            done
      done;
      if e.n = 1 && valid 0 && inside xs.(0) then
        paint (bin w xs.(0)) ys.(0) y2s.(0) 0
  | K_dot { dots; symbol } -> (
      let xs = xs () and ys = ys () in
      let ok i = inside xs.(i) && inside ys.(i) in
      match stroke th (dots :> [ dots | `Steps | `Density ] option) with
      | `Res r ->
          let w = c.pw * Ink.cols r and h = c.ph * Ink.rows r in
          for i = 0 to e.n - 1 do
            if ok i then
              Ink.dot c.ink r
                ~x:(bin w xs.(i))
                ~y:(h - 1 - bin h ys.(i))
                ~fg:(fg i) ~attrs ~owner:(owner i)
          done
      | `Density ->
          let counts = Array.make (c.pw * c.ph) 0
          and last = Array.make (c.pw * c.ph) 0 in
          for i = 0 to e.n - 1 do
            if ok i then (
              let k = ((c.ph - 1 - bin c.ph ys.(i)) * c.pw) + bin c.pw xs.(i) in
              counts.(k) <- counts.(k) + 1;
              last.(k) <- i)
          done;
          let most = Array.fold_left max 0 counts in
          let shades = th.glyphs.shades in
          let levels = Array.length shades in
          Array.iteri
            (fun k n ->
              if n > 0 then
                let lv = min (levels - 1) (((n * levels) - 1) / most) in
                Ink.glyph c.ink ~x:(k mod c.pw) ~y:(k / c.pw) shades.(lv)
                  ~fg:(fg last.(k))
                  ~attrs
                  ~owner:(owner last.(k)))
            counts
      | `Cell | `Steps ->
          let s = Option.value symbol ~default:th.glyphs.point in
          for i = 0 to e.n - 1 do
            if ok i then
              Ink.text c.ink ~width_method:l.wm
                ~x:(bin c.pw xs.(i))
                ~y:(c.ph - 1 - bin c.ph ys.(i))
                s ~fg:(fg i) ~attrs ~owner:(owner i)
          done)
  | K_rect | K_hist ->
      let range len n0 n1 posf =
        (* the cells of [n0, n1], the whole axis when absent *)
        match (n0, n1) with
        | E_none, E_none -> fun _ -> Some (0, len - 1)
        | _ ->
            let a0 = ch_get n0 and a1 = ch_get n1 in
            fun i ->
              let v0 = if a0 = [||] then a1.(i) else a0.(i) in
              let v1 = if a1 = [||] then v0 else a1.(i) in
              let p0 = posf v0 and p1 = posf v1 in
              if not (Float.is_finite p0 && Float.is_finite p1) then None
              else
                let p0, p1 = (Float.min p0 p1, Float.max p0 p1) in
                if p1 < 0. || p0 > 1. then None
                else
                  let p0 = Float.max 0. p0 and p1 = Float.min 1. p1 in
                  let a = bin len p0 in
                  let b =
                    max a
                      (Float.to_int (Float.ceil (p1 *. Float.of_int len)) - 1)
                  in
                  Some (a, min (len - 1) b)
      in
      let cols = range c.pw e.ex e.ex2 px in
      if e.kind = K_hist then
        let ys = ch_get e.ey in
        let z = match sy with Some s -> pos s c.ph 0. | None -> 0. in
        for i = 0 to e.n - 1 do
          match cols i with
          | None -> ()
          | Some (c0, c1) ->
              let p = py ys.(i) in
              if Float.is_finite p then
                let eighth p =
                  Float.to_int
                    (Float.round
                       (Float.max 0. (Float.min 1. p) *. Float.of_int (8 * c.ph)))
                in
                bar c ~horizontal:false ~eighths:th.glyphs.blocks ~c0 ~c1
                  ~e0:(eighth z) ~e1:(eighth p) ~first:true ~fg:(fg i) ~attrs
                  ~owner:(owner i)
        done
      else
        let rows = range c.ph e.ey e.ey2 py in
        for i = 0 to e.n - 1 do
          match (cols i, rows i) with
          | Some (c0, c1), Some (r0, r1) ->
              for x = c0 to c1 do
                for k = r0 to r1 do
                  Ink.fill c.ink ~x
                    ~y:(c.ph - 1 - k)
                    ~bg:(fg i) ~owner:(owner i)
                done
              done
          | _ -> ()
        done
  | K_bar { dots; horizontal } -> (
      let cat_s, val_s = if horizontal then (sy, sx) else (sx, sy) in
      match (cat_s, val_s) with
      | Some cs, Some vs ->
          let along = if horizontal then c.pw else c.ph in
          let across = if horizontal then c.ph else c.pw in
          let cats = ch_get (if horizontal then e.ey else e.ex) in
          let tops = ch_get (if horizontal then e.ex else e.ey) in
          let bases = ch_get (if horizontal then e.ex2 else e.ey2) in
          let eighth v =
            let p = pos vs along v in
            Float.to_int
              (Float.round
                 (Float.max 0. (Float.min 1. p) *. Float.of_int (8 * along)))
          in
          let eighths = dots = `Eighths && th.glyphs.blocks in
          for i = 0 to e.n - 1 do
            let ci = cats.(i) in
            if Float.is_finite ci && Float.is_finite tops.(i) then begin
              let a, b = band_span cs across (Float.to_int ci) in
              (* across cells count from the left for vertical bars, from
                 the top for horizontal ones *)
              let a, b =
                if horizontal then (across - 1 - b, across - 1 - a) else (a, b)
              in
              let a, b = if horizontal then (a, b) else (a, b) in
              let e0 = eighth bases.(i) and e1 = eighth tops.(i) in
              let first = bases.(i) = 0. in
              let c0, c1 =
                if horizontal then (c.ph - 1 - b, c.ph - 1 - a) else (a, b)
              in
              let c0, c1 =
                if horizontal then (c.ph - 1 - c1, c.ph - 1 - c0) else (c0, c1)
              in
              bar c ~horizontal ~eighths ~c0 ~c1 ~e0 ~e1 ~first ~fg:(fg i)
                ~attrs ~owner:(owner i);
              if Array.length e.texts > 0 then begin
                let s = e.texts.(i) in
                let tw = measure l.wm s in
                let lo = min e0 e1 and hi = max e0 e1 in
                let k0 = (lo + 7) / 8 and k1 = (hi / 8) - 1 in
                let fgc = contrast (fg i) in
                let label x y =
                  for j = 0 to tw - 1 do
                    Ink.fill c.ink ~x:(x + j) ~y ~bg:(fg i) ~owner:(owner i)
                  done;
                  Ink.text c.ink ~width_method:l.wm ~x ~y s ~fg:fgc ~attrs
                    ~owner:(owner i)
                in
                if horizontal then (
                  if k1 - k0 + 1 >= tw then
                    label (k0 + ((k1 - k0 + 1 - tw) / 2)) ((c0 + c1) / 2))
                else if k1 >= k0 && c1 - c0 + 1 >= tw then
                  label
                    (c0 + ((c1 - c0 + 1 - tw) / 2))
                    (c.ph - 1 - ((k0 + k1) / 2))
              end
            end
          done
      | _ -> ())
  | K_raster { dots; reduce; interpolate } -> (
      match (sx, sy, scale_named l e.sc) with
      | Some sx, Some sy, Some sc ->
          let xs = ch_get e.ex and ys = ch_get e.ey and vs = e.ev_v in
          let cmap =
            color_map ~depth:l.depth
              (match sc.colors with Some c -> c | None -> th.colors)
          in
          let shades_only =
            match l.depth with `Ansi16 | `Ansi8 | `Mono -> true | _ -> false
          in
          let dots = if shades_only then `Shade else dots in
          let dots =
            if (not th.glyphs.blocks) && dots = `Half then `Cell else dots
          in
          let sub = match dots with `Half -> 2 | _ -> 1 in
          let w = c.pw and h = c.ph * sub in
          let acc = Array.make (w * h) Float.nan
          and cnt = Array.make (w * h) 0 in
          let who = Array.make (w * h) (-1) in
          let put k v i =
            (match reduce with
            | `Last -> acc.(k) <- v
            | `Max ->
                acc.(k) <- (if cnt.(k) = 0 then v else Float.max acc.(k) v)
            | `Mean -> acc.(k) <- (if cnt.(k) = 0 then v else acc.(k) +. v));
            cnt.(k) <- cnt.(k) + 1;
            who.(k) <- i
          in
          (match interpolate with
          | `None ->
              for i = 0 to e.n - 1 do
                let p = pos sx w xs.(i) and q = pos sy h ys.(i) in
                if inside p && inside q && Float.is_finite vs.(i) then
                  put (((h - 1 - bin h q) * w) + bin w p) vs.(i) i
              done;
              if reduce = `Mean then
                Array.iteri
                  (fun k n ->
                    if n > 0 then acc.(k) <- acc.(k) /. Float.of_int n)
                  cnt
          | `Bilinear ->
              let uniq a =
                let l =
                  List.sort_uniq Float.compare
                    (List.filter Float.is_finite (Array.to_list a))
                in
                Array.of_list l
              in
              let gx = uniq xs and gy = uniq ys in
              let nx = Array.length gx and ny = Array.length gy in
              let find g v =
                let rec go lo hi =
                  if lo >= hi then lo
                  else
                    let mid = (lo + hi + 1) / 2 in
                    if g.(mid) <= v then go mid hi else go lo (mid - 1)
                in
                go 0 (Array.length g - 1)
              in
              let gv = Array.make (nx * ny) Float.nan
              and gn = Array.make (nx * ny) 0 in
              let gw = Array.make (nx * ny) (-1) in
              for i = 0 to e.n - 1 do
                if
                  Float.is_finite xs.(i)
                  && Float.is_finite ys.(i)
                  && Float.is_finite vs.(i)
                then (
                  let k = (find gy ys.(i) * nx) + find gx xs.(i) in
                  (match reduce with
                  | `Last -> gv.(k) <- vs.(i)
                  | `Max ->
                      gv.(k) <-
                        (if gn.(k) = 0 then vs.(i) else Float.max gv.(k) vs.(i))
                  | `Mean ->
                      gv.(k) <-
                        (if gn.(k) = 0 then vs.(i) else gv.(k) +. vs.(i)));
                  gn.(k) <- gn.(k) + 1;
                  gw.(k) <- i)
              done;
              if reduce = `Mean then
                Array.iteri
                  (fun k n -> if n > 0 then gv.(k) <- gv.(k) /. Float.of_int n)
                  gn;
              if nx >= 1 && ny >= 1 then
                for k = 0 to (w * h) - 1 do
                  let cx = k mod w and cy = k / w in
                  let x = unnorm sx ((Float.of_int cx +. 0.5) /. Float.of_int w)
                  and y =
                    unnorm sy
                      ((Float.of_int (h - 1 - cy) +. 0.5) /. Float.of_int h)
                  in
                  if
                    x >= gx.(0)
                    && x <= gx.(nx - 1)
                    && y >= gy.(0)
                    && y <= gy.(ny - 1)
                  then
                    let i = min (find gx x) (max 0 (nx - 2))
                    and j = min (find gy y) (max 0 (ny - 2)) in
                    let i1 = min (nx - 1) (i + 1)
                    and j1 = min (ny - 1) (j + 1) in
                    let fx =
                      if i1 = i then 0. else (x -. gx.(i)) /. (gx.(i1) -. gx.(i))
                    in
                    let fy =
                      if j1 = j then 0. else (y -. gy.(j)) /. (gy.(j1) -. gy.(j))
                    in
                    let g a b = gv.((b * nx) + a) in
                    let v =
                      ((1. -. fx) *. (1. -. fy) *. g i j)
                      +. (fx *. (1. -. fy) *. g i1 j)
                      +. ((1. -. fx) *. fy *. g i j1)
                      +. (fx *. fy *. g i1 j1)
                    in
                    if Float.is_finite v then (
                      acc.(k) <- v;
                      cnt.(k) <- 1;
                      let near =
                        gw.(((if fy < 0.5 then j else j1) * nx)
                            + if fx < 0.5 then i else i1)
                      in
                      who.(k) <-
                        (if near >= 0 then near else max 0 gw.((j * nx) + i)))
                done);
          let color v = cmap (norm sc v) in
          let shades = th.glyphs.shades in
          let levels = Array.length shades in
          for cy = 0 to c.ph - 1 do
            for cx = 0 to c.pw - 1 do
              match dots with
              | `Half ->
                  let u = (2 * cy * w) + cx and d = (((2 * cy) + 1) * w) + cx in
                  let hu = cnt.(u) > 0 and hd = cnt.(d) > 0 in
                  if hu && hd then (
                    Ink.fill c.ink ~x:cx ~y:cy
                      ~bg:(color acc.(d))
                      ~owner:(owner who.(d));
                    Ink.glyph c.ink ~x:cx ~y:cy "▀"
                      ~fg:(color acc.(u))
                      ~attrs
                      ~owner:(owner who.(u)))
                  else if hu then
                    Ink.glyph c.ink ~x:cx ~y:cy "▀"
                      ~fg:(color acc.(u))
                      ~attrs
                      ~owner:(owner who.(u))
                  else if hd then
                    Ink.glyph c.ink ~x:cx ~y:cy "▄"
                      ~fg:(color acc.(d))
                      ~attrs
                      ~owner:(owner who.(d))
              | (`Cell | `Shade | `Glyph _) as d -> (
                  let k = (cy * w) + cx in
                  if cnt.(k) > 0 then
                    let v = acc.(k) in
                    match d with
                    | `Cell ->
                        Ink.fill c.ink ~x:cx ~y:cy ~bg:(color v)
                          ~owner:(owner who.(k))
                    | `Glyph s ->
                        Ink.glyph c.ink ~x:cx ~y:cy s ~fg:(color v) ~attrs
                          ~owner:(owner who.(k))
                    | `Shade ->
                        let p = Float.max 0. (Float.min 1. (norm sc v)) in
                        let lv =
                          min (levels - 1)
                            (Float.to_int (p *. Float.of_int levels))
                        in
                        let fg =
                          if shades_only then Color.default_fg else color v
                        in
                        Ink.glyph c.ink ~x:cx ~y:cy shades.(lv) ~fg ~attrs
                          ~owner:(owner who.(k)))
            done
          done
      | _ -> ())
  | K_text { anchor } ->
      let xs = xs () and ys = ys () in
      for i = 0 to e.n - 1 do
        if inside xs.(i) && inside ys.(i) then
          let s = e.texts.(i) in
          let tw = measure l.wm s in
          let x = bin c.pw xs.(i) in
          let x =
            match anchor with
            | `Left -> x
            | `Center -> x - ((tw - 1) / 2)
            | `Right -> x - tw + 1
          in
          Ink.text c.ink ~width_method:l.wm ~x
            ~y:(c.ph - 1 - bin c.ph ys.(i))
            s ~fg:(fg i) ~attrs ~owner:(owner i)
      done
  | K_arrow { head } ->
      let xs = xs () and ys = ys () in
      let x2s = pxs (ch_get e.ex2) and y2s = pys (ch_get e.ey2) in
      let g = th.glyphs in
      for i = 0 to e.n - 1 do
        cell_line c ~dash:`Solid
          ~xs:[| xs.(i); x2s.(i) |]
          ~ys:[| ys.(i); y2s.(i) |]
          ~fg:(fun _ -> fg i)
          ~attrs
          ~owner:(fun _ -> owner i);
        if
          inside x2s.(i)
          && inside y2s.(i)
          && Float.is_finite xs.(i)
          && Float.is_finite ys.(i)
        then
          let x0 = bin c.pw (Float.max 0. (Float.min 1. xs.(i)))
          and y0 = c.ph - 1 - bin c.ph (Float.max 0. (Float.min 1. ys.(i))) in
          let x1 = bin c.pw x2s.(i) and y1 = c.ph - 1 - bin c.ph y2s.(i) in
          let dx = x1 - x0 and dy = y1 - y0 in
          let s =
            match head with
            | `None -> None
            | `Dot -> Some g.marker
            | `Arrow ->
                let ascii = not g.blocks in
                let ax = abs dx and ay = 2 * abs dy in
                Some
                  (if ay * 2 < ax then
                     if dx >= 0 then if ascii then ">" else "→"
                     else if ascii then "<"
                     else "←"
                   else if ax * 2 < ay then
                     if dy < 0 then if ascii then "^" else "↑"
                     else if ascii then "v"
                     else "↓"
                   else if ascii then "*"
                   else if dx > 0 && dy < 0 then "↗"
                   else if dx > 0 then "↘"
                   else if dy < 0 then "↖"
                   else "↙")
          in
          Option.iter
            (fun s ->
              Ink.glyph c.ink ~x:x1 ~y:y1 s ~fg:(fg i) ~attrs ~owner:(owner i))
            s
      done
  | K_spark { dots } -> (
      match sy with
      | None -> ()
      | Some s ->
          let vals = ch_get e.ey in
          let n = Array.length vals in
          let vis = min n c.pw in
          let first = n - vis in
          let col j = c.pw - vis + j in
          let use_braille = dots = `Braille && th.glyphs.blocks in
          if use_braille || dots = `Braille then
            let xs =
              Array.init n (fun i ->
                  if i < first then Float.nan
                  else
                    (Float.of_int (col (i - first)) +. 0.25)
                    /. Float.of_int c.pw)
            in
            let ys = positions s c.ph vals in
            if use_braille then
              path c ~cols:2 ~rows:4 ~xs ~ys (fun x y i ->
                  Ink.dot c.ink Ink.Braille ~x ~y ~fg:(fg i) ~attrs
                    ~owner:(owner i))
            else cell_line c ~dash:`Solid ~xs ~ys ~fg ~attrs ~owner
          else
            let z = pos s c.ph 0. in
            let eighth p =
              Float.to_int
                (Float.round
                   (Float.max 0. (Float.min 1. p) *. Float.of_int (8 * c.ph)))
            in
            for i = first to n - 1 do
              let p = pos s c.ph vals.(i) in
              if Float.is_finite p then
                let x = col (i - first) in
                bar c ~horizontal:false ~eighths:th.glyphs.blocks ~c0:x ~c1:x
                  ~e0:(eighth z) ~e1:(eighth p) ~first:true ~fg:(fg i) ~attrs
                  ~owner:(owner i)
            done)
  | K_rule { dash; horizontal } ->
      let g = th.glyphs in
      let h, v =
        match dash with
        | `Solid -> (utf8 g.box.horizontal, utf8 g.box.vertical)
        | `Dashed -> g.dashed
        | `Dotted -> g.dotted
      in
      if horizontal then (
        let p = py (ch_get e.ey).(0) in
        if inside p then
          let y = c.ph - 1 - bin c.ph p in
          for x = 0 to c.pw - 1 do
            Ink.glyph c.ink ~x ~y h ~fg:(fg 0) ~attrs ~owner:(owner 0)
          done)
      else
        let p = px (ch_get e.ex).(0) in
        if inside p then
          let x = bin c.pw p in
          for y = 0 to c.ph - 1 do
            Ink.glyph c.ink ~x ~y v ~fg:(fg 0) ~attrs ~owner:(owner 0)
          done
  | K_candle { width; hollow; rising; falling } ->
      let xs = xs () in
      let highs = pys (ch_get e.ey) and lows = pys (ch_get e.ey2) in
      let opens = pys e.ev_v and closes = pys e.ev_w in
      let blocks = th.glyphs.blocks in
      for i = 0 to e.n - 1 do
        if
          inside xs.(i)
          && Float.is_finite highs.(i)
          && Float.is_finite lows.(i)
          && Float.is_finite opens.(i)
          && Float.is_finite closes.(i)
        then begin
          let up = e.ev_w.(i) >= e.ev_v.(i) in
          let fg =
            if up then Option.value rising ~default:Color.green
            else Option.value falling ~default:Color.red
          in
          let row p = c.ph - 1 - bin c.ph (Float.max 0. (Float.min 1. p)) in
          let x = bin c.pw xs.(i) in
          let x = if width = 2 then min x (c.pw - 2) else x in
          let wick, body =
            if not blocks then ("|", if hollow && up then "|" else "#")
            else if width = 1 then ("│", "┃")
            else ("▕▏", if hollow && up then "▐▌" else "██")
          in
          let wick, body =
            if (not blocks) && width = 2 then (wick ^ " ", body ^ body)
            else (wick, body)
          in
          let bt = row (Float.max opens.(i) closes.(i))
          and bb = row (Float.min opens.(i) closes.(i)) in
          let wt = row highs.(i) and wb = row lows.(i) in
          for y = min wt bt to max wt bt do
            Ink.text c.ink ~width_method:l.wm ~x ~y wick ~fg ~attrs
              ~owner:(owner i)
          done;
          for y = min bb wb to max bb wb do
            Ink.text c.ink ~width_method:l.wm ~x ~y wick ~fg ~attrs
              ~owner:(owner i)
          done;
          for y = min bt bb to max bt bb do
            Ink.text c.ink ~width_method:l.wm ~x ~y body ~fg ~attrs
              ~owner:(owner i)
          done
        end
      done

let ink_layout (l : layout) =
  let ink = Ink.create ~width:l.plot.width ~height:l.plot.height in
  let c = { lay = l; ink; pw = l.plot.width; ph = l.plot.height } in
  if c.pw > 0 && c.ph > 0 then Array.iter (ink_mark c) l.r.evs;
  ink

(* {1 Drawing} *)

type overlay =
  | Crosshair of (int * int)
  | Marker of (int * int)
  | Tip of {
      at : int * int;
      anchor : [ `Auto | `Left | `Right | `Top | `Bottom ];
      lines : string list;
    }

let text_at grid ~wm ~style ~x ~y s =
  Grid.draw_text grid ~style ~width_method:wm ~x ~y ~text:s

let edges (l : layout) =
  let p = l.plot in
  let inset = l.r.fig_margins.inset in
  ( p.x - 1 - inset,
    p.x + p.width + inset,
    p.y - 1 - inset,
    p.y + p.height + inset )

let axis_style (l : layout) a = Option.value a.a_style ~default:l.theme.axis

(* Gridlines cross the plot behind the marks. A gridline in a cell beside an
   axis line or the frame is left out, so that the line is not drawn
   twice. *)
let draw_grid (l : layout) grid ~ox ~oy =
  if List.exists (fun pa -> pa.a.a_grid <> `None) l.placed then (
    let th = l.theme and p = l.plot in
    let w = p.width and h = p.height in
    (* per cell: 1 a horizontal line, 2 a vertical one, 4 a major one *)
    let cells = Array.make (w * h) 0 in
    let line_on side =
      (l.r.fig_frame && l.lines)
      || List.exists (fun pa -> pa.side = side && pa.a.a_show) l.placed
    in
    let mark major horiz o =
      let skip =
        if horiz then (o = h - 1 && line_on `Bottom) || (o = 0 && line_on `Top)
        else (o = 0 && line_on `Left) || (o = w - 1 && line_on `Right)
      in
      if not skip then
        let bit = (if horiz then 1 else 2) lor if major then 4 else 0 in
        if horiz then
          for x = 0 to w - 1 do
            cells.((o * w) + x) <- cells.((o * w) + x) lor bit
          done
        else
          for y = 0 to h - 1 do
            cells.((y * w) + o) <- cells.((y * w) + o) lor bit
          done
    in
    List.iter
      (fun pa ->
        let vertical = pa.side = `Bottom || pa.side = `Top in
        let len = if vertical then w else h in
        (* [o] counts from the axis's start: the left for x, the bottom for y *)
        let at major o =
          if o >= 0 && o < len then
            mark major (not vertical) (if vertical then o else len - 1 - o)
        in
        match pa.a.a_grid with
        | `None -> ()
        | `Every n ->
            let k = ref 0 in
            while !k * n < len do
              mark true (not vertical) (!k * n);
              incr k
            done
        | (`Ticks | `Minor _) as g -> (
            match pa.s.kind with
            | Band ->
                let n = Array.length pa.s.cats in
                let e = band_extent pa.s len in
                for i = 1 to n - 1 do
                  at true (i * e / n)
                done
            | _ ->
                (match (g, pa.ticks.major) with
                | `Minor k, a :: b :: _ ->
                    let a = tf pa.s a.value and b = tf pa.s b.value in
                    let lo, hi = span pa.s in
                    let lo, hi = (Float.min lo hi, Float.max lo hi) in
                    let step = (b -. a) /. Float.of_int k in
                    let v =
                      ref (a -. (Float.floor ((a -. lo) /. step) *. step))
                    in
                    while !v <= hi +. (Float.abs step *. 1e-9) do
                      let q = norm pa.s (untf pa.s !v) in
                      if inside q then at false (bin len q);
                      v := !v +. step
                    done
                | _ -> ());
                List.iter
                  (fun (t : Ticks.tick) ->
                    at true (bin len (norm pa.s t.value)))
                  pa.ticks.major))
      l.placed;
    let b = th.glyphs.box in
    let hs, vs =
      match th.grid_dash with
      | `Solid -> (utf8 b.horizontal, utf8 b.vertical)
      | `Dashed -> th.glyphs.dashed
      | `Dotted -> th.glyphs.dotted
    in
    let cross = utf8 b.cross in
    Array.iteri
      (fun i c ->
        if c <> 0 then
          let style = if c land 4 <> 0 then th.grid else th.minor_grid in
          let g = match c land 3 with 3 -> cross | 1 -> hs | _ -> vs in
          Grid.draw_text grid ~style
            ~x:(ox + p.x + (i mod w))
            ~y:(oy + p.y + (i / w))
            ~text:g)
      cells)

let draw_axes (l : layout) grid ~ox ~oy =
  (* A figure without guides, such as a spark, draws none of this. *)
  if List.exists (fun pa -> pa.a.a_show) l.placed || (l.r.fig_frame && l.lines)
  then (
    let th = l.theme and p = l.plot in
    let lc, rc, tr, br = edges l in
    let shown side =
      List.exists (fun pa -> pa.side = side && pa.a.a_show) l.placed
    in
    let frame = l.r.fig_frame && l.lines in
    let line_on side = shown side || frame in
    let style_of side =
      match
        List.find_opt (fun pa -> pa.side = side && pa.a.a_show) l.placed
      with
      | Some pa -> axis_style l pa.a
      | None -> th.axis
    in
    let lines = Grid.Lines.create ~x:ox ~y:oy ~width:l.width ~height:l.height in
    let top = if line_on `Top then tr else p.y
    and bottom = if line_on `Bottom then br else p.y + p.height - 1 in
    let left = if line_on `Left then lc else p.x
    and right = if line_on `Right then rc else p.x + p.width - 1 in
    if p.width > 0 && p.height > 0 then begin
      if line_on `Left then
        Grid.Lines.vline ~style:(style_of `Left) lines ~x:(ox + lc) ~y:(oy + top)
          ~length:(bottom - top + 1);
      if line_on `Right then
        Grid.Lines.vline ~style:(style_of `Right) lines ~x:(ox + rc)
          ~y:(oy + top)
          ~length:(bottom - top + 1);
      if line_on `Bottom then
        Grid.Lines.hline ~style:(style_of `Bottom) lines ~x:(ox + left)
          ~y:(oy + br)
          ~length:(right - left + 1);
      if line_on `Top then
        Grid.Lines.hline ~style:(style_of `Top) lines ~x:(ox + left) ~y:(oy + tr)
          ~length:(right - left + 1);
      (* A free end keeps the axis's direction, so that a tick there joins as
       a tee. *)
      let free_v side x =
        if line_on side then begin
          if not (line_on `Top) then
            Grid.Lines.arm ~style:(style_of side) lines ~x:(ox + x)
              ~y:(oy + top) `Up;
          if not (line_on `Bottom) then
            Grid.Lines.arm ~style:(style_of side) lines ~x:(ox + x)
              ~y:(oy + bottom) `Down
        end
      and free_h side y =
        if line_on side then begin
          if not (line_on `Left) then
            Grid.Lines.arm ~style:(style_of side) lines ~x:(ox + left)
              ~y:(oy + y) `Left;
          if not (line_on `Right) then
            Grid.Lines.arm ~style:(style_of side) lines ~x:(ox + right)
              ~y:(oy + y) `Right
        end
      in
      free_v `Left lc;
      free_v `Right rc;
      free_h `Bottom br;
      free_h `Top tr
    end;
    let tl = th.tick_length and pad = th.label_padding in
    List.iter
      (fun pa ->
        if pa.a.a_show then begin
          let tstyle =
            Option.value pa.a.a_tick_style ~default:(axis_style l pa.a)
          in
          let lstyle = Option.value pa.a.a_label_style ~default:th.label in
          let label = text_at grid ~wm:l.wm ~style:lstyle in
          List.iter2
            (fun (k : Ticks.tick) cell ->
              let w = measure l.wm k.label in
              match pa.side with
              | `Left ->
                  if tl = 0 then
                    Grid.Lines.arm ~style:tstyle lines ~x:(ox + lc)
                      ~y:(oy + cell) `Left
                  else
                    Grid.Lines.hline ~style:tstyle lines
                      ~x:(ox + lc - tl)
                      ~y:(oy + cell) ~length:(tl + 1);
                  let s =
                    match k.context with
                    | Some c -> k.label ^ " " ^ c
                    | None -> k.label
                  in
                  let w = measure l.wm s in
                  label ~x:(ox + lc - tl - pad - w) ~y:(oy + cell) s
              | `Right ->
                  if tl = 0 then
                    Grid.Lines.arm ~style:tstyle lines ~x:(ox + rc)
                      ~y:(oy + cell) `Right
                  else
                    Grid.Lines.hline ~style:tstyle lines ~x:(ox + rc)
                      ~y:(oy + cell) ~length:(tl + 1);
                  let s =
                    match k.context with
                    | Some c -> k.label ^ " " ^ c
                    | None -> k.label
                  in
                  label ~x:(ox + rc + tl + pad + 1) ~y:(oy + cell) s
              | `Bottom ->
                  if tl = 0 then
                    Grid.Lines.arm ~style:tstyle lines ~x:(ox + cell)
                      ~y:(oy + br) `Down
                  else
                    Grid.Lines.vline ~style:tstyle lines ~x:(ox + cell)
                      ~y:(oy + br) ~length:(tl + 1);
                  let row = br + tl + 1 in
                  label ~x:(ox + cell - ((w - 1) / 2)) ~y:(oy + row) k.label;
                  Option.iter
                    (fun c ->
                      let cw = measure l.wm c in
                      label ~x:(ox + cell - ((cw - 1) / 2)) ~y:(oy + row + 1) c)
                    k.context
              | `Top ->
                  if tl = 0 then
                    Grid.Lines.arm ~style:tstyle lines ~x:(ox + cell)
                      ~y:(oy + tr) `Up
                  else
                    Grid.Lines.vline ~style:tstyle lines ~x:(ox + cell)
                      ~y:(oy + tr - tl)
                      ~length:(tl + 1);
                  let row = tr - tl - 1 in
                  label ~x:(ox + cell - ((w - 1) / 2)) ~y:(oy + row) k.label;
                  Option.iter
                    (fun c ->
                      let cw = measure l.wm c in
                      label ~x:(ox + cell - ((cw - 1) / 2)) ~y:(oy + row - 1) c)
                    k.context)
            pa.ticks.major pa.cells;
          match pa.heading with
          | None -> ()
          | Some h ->
              let hstyle = Option.value pa.a.a_title_style ~default:th.label in
              let hw = measure l.wm h in
              let rows = if pa.ticks.major = [] then 0 else pa.label_rows in
              let x, y =
                match pa.side with
                | `Bottom -> (p.x + ((p.width - hw) / 2), br + tl + rows + 1)
                | `Top -> (p.x + ((p.width - hw) / 2), tr - tl - rows - 1)
                | `Left -> (max 0 (lc - tl - pad - pa.label_w), l.heading_row)
                | `Right -> (rc + tl + pad + pa.label_w - hw + 1, l.heading_row)
              in
              text_at grid ~wm:l.wm ~style:hstyle ~x:(ox + x) ~y:(oy + y) h
        end)
      l.placed;
    Grid.draw_lines ~glyphs:(Grid.Lines.of_border th.glyphs.box) grid lines)

(* A legend paints every cell of its block, so the marks under it do not
   show between its entries. *)
let draw_legends (l : layout) grid ~ox ~oy =
  let th = l.theme in
  List.iter
    (fun pl ->
      let lw, lh = legend_size l.wm pl.lg pl.entries in
      let blank = String.make lw ' ' in
      for k = 0 to lh - 1 do
        text_at grid ~wm:l.wm ~style:th.label ~x:(ox + pl.lx)
          ~y:(oy + pl.ly + k)
          blank
      done;
      let y = ref pl.ly in
      (match pl.lg.l_title with
      | Some t ->
          text_at grid ~wm:l.wm ~style:th.label ~x:(ox + pl.lx) ~y:(oy + !y) t;
          incr y
      | None -> ());
      let x = ref pl.lx in
      List.iter
        (fun (lab, col, sw) ->
          let sws = measure l.wm sw in
          text_at grid ~wm:l.wm ~style:(Style.make ~fg:col ()) ~x:(ox + !x)
            ~y:(oy + !y) sw;
          text_at grid ~wm:l.wm ~style:th.label
            ~x:(ox + !x + sws + 1)
            ~y:(oy + !y) lab;
          match pl.lg.l_direction with
          | `Rows -> incr y
          | `Columns -> x := !x + sws + 1 + measure l.wm lab + 2)
        pl.entries)
    l.legends_placed

let draw_overlay (l : layout) grid ~ox ~oy = function
  | Crosshair (cx, cy) -> (
      let p = l.plot and th = l.theme in
      if cx >= p.x && cx < p.x + p.width && cy >= p.y && cy < p.y + p.height
      then
        match th.crosshair_dash with
        | `Solid ->
            let lines =
              Grid.Lines.create ~x:(ox + p.x) ~y:(oy + p.y) ~width:p.width
                ~height:p.height
            in
            Grid.Lines.vline ~style:th.crosshair lines ~x:(ox + cx)
              ~y:(oy + p.y) ~length:p.height;
            Grid.Lines.hline ~style:th.crosshair lines ~x:(ox + p.x)
              ~y:(oy + cy) ~length:p.width;
            Grid.draw_lines
              ~glyphs:(Grid.Lines.of_border th.glyphs.box)
              grid lines
        | (`Dashed | `Dotted) as d ->
            let h, v =
              if d = `Dashed then th.glyphs.dashed else th.glyphs.dotted
            in
            for x = p.x to p.x + p.width - 1 do
              Grid.draw_text grid ~style:th.crosshair ~x:(ox + x) ~y:(oy + cy)
                ~text:h
            done;
            for y = p.y to p.y + p.height - 1 do
              Grid.draw_text grid ~style:th.crosshair ~x:(ox + cx) ~y:(oy + y)
                ~text:v
            done)
  | Marker (cx, cy) ->
      let p = l.plot in
      if cx >= p.x && cx < p.x + p.width && cy >= p.y && cy < p.y + p.height
      then
        text_at grid ~wm:l.wm ~style:l.theme.marker ~x:(ox + cx) ~y:(oy + cy)
          l.theme.glyphs.marker
  | Tip { at = cx, cy; anchor; lines } ->
      let p = l.plot and th = l.theme in
      let b = if th.tip_border = None then 0 else 1 in
      let pad = th.tip_padding in
      let tw = List.fold_left (fun m s -> max m (measure l.wm s)) 0 lines in
      let w = tw + (2 * pad) + (2 * b) and h = List.length lines + (2 * b) in
      let box = function
        | `Right -> (cx + 2, cy - (h / 2))
        | `Left -> (cx - 1 - w, cy - (h / 2))
        | `Top -> (cx - (w / 2), cy - 1 - h)
        | `Bottom -> (cx - (w / 2), cy + 2)
      in
      let clipped (x, y) =
        let ix = max 0 (min (x + w) (p.x + p.width) - max x p.x)
        and iy = max 0 (min (y + h) (p.y + p.height) - max y p.y) in
        (w * h) - (ix * iy)
      in
      let moved (x, y) =
        let x = max p.x (min x (p.x + p.width - w))
        and y = max p.y (min y (p.y + p.height - h)) in
        (x, y)
      in
      let covers (x, y) = cx >= x && cx < x + w && cy >= y && cy < y + h in
      let x, y =
        match anchor with
        | (`Left | `Right | `Top | `Bottom) as a -> moved (box a)
        | `Auto ->
            let all = [ `Right; `Left; `Top; `Bottom ] in
            let best l =
              List.fold_left
                (fun acc a ->
                  let c = clipped (box a) in
                  match acc with
                  | Some (_, c') when c' <= c -> acc
                  | _ -> Some (a, c))
                None l
            in
            let free =
              List.filter (fun a -> not (covers (moved (box a)))) all
            in
            let a =
              match best (if free = [] then all else free) with
              | Some (a, _) -> a
              | None -> `Right
            in
            moved (box a)
      in
      let bg = match th.tip.bg with Some c -> c | None -> Color.transparent in
      (match th.tip_border with
      | Some bs ->
          Grid.draw_box grid ~x:(ox + x) ~y:(oy + y) ~width:w ~height:h
            ~border:th.glyphs.box ~style:bs ~fill:bg ()
      | None ->
          Grid.fill_rect grid ~x:(ox + x) ~y:(oy + y) ~width:w ~height:h
            ~color:bg);
      List.iteri
        (fun i s ->
          text_at grid ~wm:l.wm ~style:th.tip
            ~x:(ox + x + b + pad)
            ~y:(oy + y + b + i)
            s)
        lines

let draw ?(overlays = []) (l : layout) grid ~x ~y =
  if l.width > 0 && l.height > 0 then
    Grid.clip grid { Grid.x; y; width = l.width; height = l.height } (fun () ->
        Option.iter
          (fun c ->
            Grid.fill_rect grid ~x ~y ~width:l.width ~height:l.height ~color:c)
          l.theme.background;
        let p = l.plot in
        if p.width > 0 && p.height > 0 then begin
          draw_grid l grid ~ox:x ~oy:y;
          Ink.commit (ink_layout l) grid ~x:(x + p.x) ~y:(y + p.y)
        end;
        draw_axes l grid ~ox:x ~oy:y;
        draw_legends l grid ~ox:x ~oy:y;
        (match (l.title_at, l.r.fig_title) with
        | Some (tx, ty), Some t ->
            text_at grid ~wm:l.wm ~style:l.theme.title ~x:(x + tx) ~y:(y + ty) t
        | _ -> ());
        List.iter (draw_overlay l grid ~ox:x ~oy:y) overlays)

(* {1 Layouts} *)

type hit = { id : id; row : int; cell : int * int; distance : float }

let continuous (s : scale) = s.kind <> Band

let layout ?(view = View0.empty) ?(theme = Theme.dark) ?(depth = `Truecolor)
    ?(width_method = `Unicode) ~width ~height fig =
  let r = resolve ~view ~theme fig in
  place ~theme ~depth ~wm:width_method ~width ~height ~view r

let set_window view (s : scale) a b =
  let lo = untf s a and hi = untf s b in
  if Float.is_finite lo && Float.is_finite hi && lo < hi then
    View0.set s.name (Some (lo, hi)) view
  else view

let on_axes axes (s : scale) =
  continuous s
  &&
  match (axes, s.dir) with
  | `Both, (X | Y) -> true
  | `X, X | `Y, Y -> true
  | _ -> false

module Layout = struct
  type t = layout
  type value = Num of float | Cat of string

  let plot (l : t) = l.plot

  let scale fn (l : t) n =
    match find_scale l.r n with
    | Some ({ dir = X | Y; _ } as s) -> s
    | Some _ -> err fn "scale %S is a color scale" n
    | None -> err fn "no role reads scale %S" n

  (* The cell's offset from the axis's start, and the axis's length. *)
  let along (l : t) (s : scale) (cx, cy) =
    let p = l.plot in
    match s.dir with
    | Y -> (p.y + p.height - 1 - cy, p.height)
    | _ -> (cx - p.x, p.width)

  let in_plot (l : t) (cx, cy) =
    let p = l.plot in
    cx >= p.x && cx < p.x + p.width && cy >= p.y && cy < p.y + p.height

  let value (l : t) n cell =
    let s = scale "Layout.value" l n in
    if not (in_plot l cell) then None
    else
      let off, len = along l s cell in
      match s.kind with
      | Band ->
          let cats = Array.length s.cats in
          let e = band_extent s len in
          if off >= e || cats = 0 then None
          else
            let k = off * cats / e in
            let i = if s.reverse then cats - 1 - k else k in
            Some (Cat s.cats.(i))
      | _ ->
          Some (Num (unnorm s ((Float.of_int off +. 0.5) /. Float.of_int len)))

  let position (l : t) n v =
    let s = scale "Layout.position" l n in
    let p = l.plot in
    let len = match s.dir with Y -> p.height | _ -> p.width in
    let off =
      match (s.kind, v) with
      | Band, Cat c ->
          let i = band_index s c in
          if Float.is_nan i then
            err "Layout.position" "%S is not a category of %S" c n;
          band_center s (max 1 len) (Float.to_int i)
      | Band, Num _ -> err "Layout.position" "scale %S holds categories" n
      | _, Cat _ -> err "Layout.position" "scale %S holds quantities" n
      | _, Num x ->
          let q = norm s x in
          let q =
            if Float.is_nan q then 0. else Float.max 0. (Float.min 1. q)
          in
          bin (max 1 len) q
    in
    match s.dir with Y -> p.y + len - 1 - off | _ -> p.x + off

  let domain (l : t) n =
    let s = scale "Layout.domain" l n in
    match s.kind with
    | Band -> Array.to_list (Array.map (fun c -> Cat c) s.cats)
    | _ -> [ Num s.lo; Num s.hi ]

  let zoom ?(axes = `Both) (l : t) ~at k =
    if not (Float.is_finite k && k > 0.) then
      err "Layout.zoom" "factor %g is not finite and positive" k;
    let p = l.plot in
    let cx, cy = at in
    let at =
      ( max p.x (min (p.x + p.width - 1) cx),
        max p.y (min (p.y + p.height - 1) cy) )
    in
    List.fold_left
      (fun view (s : scale) ->
        if not (on_axes axes s) then view
        else
          let off, len = along l s at in
          let a, b = span s in
          let u =
            tf s
              (unnorm s ((Float.of_int off +. 0.5) /. Float.of_int (max 1 len)))
          in
          set_window view s (u -. ((u -. a) /. k)) (u +. ((b -. u) /. k)))
      l.view l.r.scales

  let pan (l : t) ~by:(dx, dy) =
    let p = l.plot in
    List.fold_left
      (fun view (s : scale) ->
        if (not (continuous s)) || s.dir = C then view
        else
          let a, b = span s in
          let len, d =
            match s.dir with Y -> (p.height, -dy) | _ -> (p.width, dx)
          in
          let shift = Float.of_int d *. (b -. a) /. Float.of_int (max 1 len) in
          let shift = if s.reverse then -.shift else shift in
          if d = 0 then view else set_window view s (a -. shift) (b -. shift))
      l.view l.r.scales

  let hit ?(radius = 3.) ?(metric = `Euclidean) (l : t) (cx, cy) =
    if (not (in_plot l (cx, cy))) || not (radius >= 0.) then None
    else
      let ink = ink_layout l in
      let p = l.plot in
      let x0 = cx - p.x and y0 = cy - p.y in
      let aspect = l.theme.aspect in
      let best = ref None in
      let consider x y d =
        let o = Ink.owner_at ink ~x ~y in
        if o >= 0 then
          match !best with
          | Some (d', o', _, _) when d' < d || (d' = d && o' >= o) -> ()
          | _ -> best := Some (d, o, x, y)
      in
      let r = Float.to_int (Float.ceil radius) in
      let ry = Float.to_int (Float.ceil (radius /. aspect)) in
      let rows lo hi f =
        for y = max 0 lo to min (p.height - 1) hi do
          f y
        done
      in
      let cols lo hi f =
        for x = max 0 lo to min (p.width - 1) hi do
          f x
        done
      in
      (match metric with
      | `Euclidean ->
          rows (y0 - ry) (y0 + ry) (fun y ->
              cols (x0 - r) (x0 + r) (fun x ->
                  let dx = Float.of_int (x - x0)
                  and dy = aspect *. Float.of_int (y - y0) in
                  let d = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
                  if d <= radius then consider x y d))
      | `X ->
          rows 0 (p.height - 1) (fun y ->
              cols (x0 - r) (x0 + r) (fun x ->
                  let dx = Float.abs (Float.of_int (x - x0)) in
                  if dx <= radius then
                    consider x y
                      (dx
                      +. (aspect *. Float.abs (Float.of_int (y - y0)) /. 100.))))
      | `Y ->
          rows (y0 - ry) (y0 + ry) (fun y ->
              let dy = aspect *. Float.abs (Float.of_int (y - y0)) in
              if dy <= radius then
                cols 0 (p.width - 1) (fun x ->
                    consider x y
                      (dy +. (Float.abs (Float.of_int (x - x0)) /. 100.)))));
      Option.map
        (fun (d, o, x, y) ->
          let e = l.r.evs.(Ink.owner_mark o) in
          {
            id = e.id;
            row = Ink.owner_row o;
            cell = (p.x + x, p.y + y);
            distance = d;
          })
        !best

  let legend (l : t) =
    List.concat_map
      (fun (s : scale) ->
        match (s.dir, s.kind) with
        | C, Band -> legend_entries l.r s.name
        | _ -> [])
      l.r.scales
end

module View = struct
  include View0

  let resolved fig v = resolve ~view:v ~theme:Theme.dark fig

  let zoom ?(axes = `Both) fig k v =
    if not (Float.is_finite k && k > 0.) then
      err "View.zoom" "factor %g is not finite and positive" k;
    let r = resolved fig v in
    List.fold_left
      (fun v (s : scale) ->
        if not (on_axes axes s) then v
        else
          let a, b = span s in
          let c = (a +. b) /. 2. and h = (b -. a) /. 2. /. k in
          set_window v s (c -. h) (c +. h))
      v r.scales

  let pan fig ~by:(fx, fy) v =
    let r = resolved fig v in
    List.fold_left
      (fun v (s : scale) ->
        if (not (continuous s)) || s.dir = C then v
        else
          let a, b = span s in
          let f = match s.dir with Y -> fy | _ -> fx in
          if f = 0. then v
          else
            let d = f *. (b -. a) in
            set_window v s (a +. d) (b +. d))
      v r.scales

  let window_opt (s : scale) v =
    if continuous s && s.dir <> C then View0.window s.name v else None

  let clamp fig v =
    let r = resolved fig v in
    List.fold_left
      (fun v (s : scale) ->
        match window_opt s v with
        | None -> v
        | Some _ ->
            let a, b = span s in
            let da = tf s s.dlo and db = tf s s.dhi in
            if (not (da < db)) || b -. a >= db -. da then
              View0.set s.name None v
            else
              let shift =
                if a < da then da -. a else if b > db then db -. b else 0.
              in
              set_window v s (a +. shift) (b +. shift))
      v r.scales
end

(* {1 Smoothing} *)

let ema alpha data =
  let n = Array.length data in
  if n = 0 then [||]
  else
    let result = Array.make n (0., 0.) in
    let x0, y0 = data.(0) in
    result.(0) <- (x0, y0);
    let prev_y = ref y0 in
    for i = 1 to n - 1 do
      let x, y = data.(i) in
      let smoothed = (alpha *. y) +. ((1. -. alpha) *. !prev_y) in
      result.(i) <- (x, smoothed);
      prev_y := smoothed
    done;
    result

let sma window data =
  let n = Array.length data in
  if n = 0 || window <= 0 then [||]
  else
    let window = min window n in
    let result = Array.make n (0., 0.) in
    let sum = ref 0. in
    for i = 0 to window - 1 do
      let _, y = data.(i) in
      sum := !sum +. y
    done;
    let x0, _ = data.(window - 1) in
    result.(window - 1) <- (x0, !sum /. float window);
    for i = window to n - 1 do
      let _, y_old = data.(i - window) in
      let x, y_new = data.(i) in
      sum := !sum -. y_old +. y_new;
      result.(i) <- (x, !sum /. float window)
    done;
    let partial_sum = ref 0. in
    for i = 0 to window - 2 do
      let x, y = data.(i) in
      partial_sum := !partial_sum +. y;
      result.(i) <- (x, !partial_sum /. float (i + 1))
    done;
    result

let gaussian sigma data =
  let n = Array.length data in
  if n = 0 || sigma <= 0. then data
  else
    let radius = int_of_float (Float.ceil (3. *. sigma)) in
    let kernel_size = (2 * radius) + 1 in
    let kernel =
      Array.init kernel_size (fun i ->
          let d = float (i - radius) in
          Float.exp (-.(d *. d) /. (2. *. sigma *. sigma)))
    in
    Array.init n (fun i ->
        let x, _ = data.(i) in
        let sum = ref 0. and weight_sum = ref 0. in
        for k = 0 to kernel_size - 1 do
          let j = i + k - radius in
          if j >= 0 && j < n then (
            let _, y = data.(j) in
            sum := !sum +. (kernel.(k) *. y);
            weight_sum := !weight_sum +. kernel.(k))
        done;
        (x, if !weight_sum > 0. then !sum /. !weight_sum else 0.))
