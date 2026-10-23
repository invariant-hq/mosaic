module Lines = Matrix_grid.Lines

module Table = struct
  type column = { floor : int; min : int; max : int; grow : int }

  (* [share total w] splits [total] cells by the weights [w], which sum to
     more than [0], by largest remainders, ties to the earlier index. *)
  let share total w =
    let n = Array.length w in
    let sum = Array.fold_left ( + ) 0 w in
    let out = Array.make n 0 and rem = Array.make n 0 in
    let given = ref 0 in
    for i = 0 to n - 1 do
      let p = total * w.(i) in
      out.(i) <- p / sum;
      rem.(i) <- p mod sum;
      given := !given + out.(i)
    done;
    for _ = 1 to total - !given do
      let best = ref (-1) in
      for i = 0 to n - 1 do
        if w.(i) > 0 && (!best < 0 || rem.(i) > rem.(!best)) then best := i
      done;
      out.(!best) <- out.(!best) + 1;
      rem.(!best) <- -1
    done;
    out

  let widths cols ~room =
    Array.iter
      (fun c ->
        if c.floor < 0 || c.min < c.floor || c.max < c.min || c.grow < 0 then
          invalid_arg
            (Printf.sprintf
               "Matrix_cli.Table.widths: column { floor = %d; min = %d; max = \
                %d; grow = %d }"
               c.floor c.min c.max c.grow))
      cols;
    let sum f = Array.fold_left (fun a c -> a + f c) 0 cols in
    (* Each column's [base], and a share of the room beyond their sum by
       [weight] when some weight is positive. *)
    let between base weight =
      let extra = room - sum base in
      let w = Array.map weight cols in
      if extra <= 0 || Array.for_all (fun w -> w = 0) w then Array.map base cols
      else
        let s = share extra w in
        Array.mapi (fun i c -> base c + s.(i)) cols
    in
    if room >= sum (fun c -> c.max) then
      between (fun c -> c.max) (fun c -> c.grow)
    else if room >= sum (fun c -> c.min) then
      between (fun c -> c.min) (fun c -> c.max - c.min)
    else between (fun c -> c.floor) (fun c -> c.min - c.floor)

  type frame = {
    edge : Lines.weight option;
    line : Lines.weight option;
    pad : int;
  }

  type row = Rule of Lines.weight | Row of int | Spanned of int * int list

  let edge f = if Option.is_some f.edge then 1 else 0
  let side f = if Option.is_some f.edge then 1 + f.pad else 0
  let gap f = (2 * f.pad) + if Option.is_some f.line then 1 else 0
  let chrome f n = if n = 0 then 2 * edge f else (2 * side f) + ((n - 1) * gap f)
  let width f ws = chrome f (Array.length ws) + Array.fold_left ( + ) 0 ws
  let row_height = function Rule _ -> 1 | Row h | Spanned (h, _) -> max 0 h

  let height f rows =
    (2 * edge f) + List.fold_left (fun a r -> a + row_height r) 0 rows

  type placed = { row : Matrix_grid.region; cells : Matrix_grid.region array }

  let place ~x ~y f ws rows =
    let n = Array.length ws in
    let total = width f ws in
    let xs = Array.make n 0 in
    for i = 0 to n - 1 do
      xs.(i) <- (if i = 0 then x + side f else xs.(i - 1) + ws.(i - 1) + gap f)
    done;
    let cell ~y ~h a b =
      {
        Matrix_grid.x = xs.(a);
        y;
        width = xs.(b) + ws.(b) - xs.(a);
        height = h;
      }
    in
    let misfit () =
      invalid_arg "Matrix_cli.Table.place: spans do not fit the columns"
    in
    let spanned ~y ~h spans =
      let cells = Array.make (List.length spans) (cell ~y ~h 0 0) in
      let k = ref 0 and col = ref 0 in
      List.iter
        (fun span ->
          if span <= 0 || !col + span > n then misfit ();
          cells.(!k) <- cell ~y ~h !col (!col + span - 1);
          incr k;
          col := !col + span)
        spans;
      if !col <> n then misfit ();
      cells
    in
    let _, placed =
      List.fold_left
        (fun (y, acc) r ->
          let h = row_height r in
          let cells =
            match r with
            | Rule _ -> [||]
            | Row _ -> Array.init n (fun i -> cell ~y ~h i i)
            | Spanned (_, s) ->
                if n = 0 && s = [] then [||] else spanned ~y ~h s
          in
          ( y + h,
            { row = { Matrix_grid.x; y; width = total; height = h }; cells }
            :: acc ))
        (y + edge f, [])
        rows
    in
    List.rev placed

  let draw ?glyphs ?style g ~x ~y f ws rows =
    let w = width f ws and h = height f rows in
    let l = Lines.create ~x ~y ~width:w ~height:h in
    Option.iter
      (fun weight ->
        Lines.hline ~weight ?style l ~x ~y ~length:w;
        Lines.hline ~weight ?style l ~x ~y:(y + h - 1) ~length:w;
        Lines.vline ~weight ?style l ~x ~y ~length:h;
        Lines.vline ~weight ?style l ~x:(x + w - 1) ~y ~length:h)
      f.edge;
    (* A row's lines between its cells reach into the rule or edge above and
       below it, so that they join it. *)
    let rec go ~above = function
      | [] -> ()
      | (r, (p : placed)) :: rest ->
          (match r with
          | Rule weight -> Lines.hline ~weight ?style l ~x ~y:p.row.y ~length:w
          | Row _ | Spanned _ -> (
              match f.line with
              | None -> ()
              | Some weight ->
                  let below =
                    match rest with
                    | (Rule _, _) :: _ -> true
                    | [] -> Option.is_some f.edge
                    | _ -> false
                  in
                  let top = if above then p.row.y - 1 else p.row.y in
                  let bottom =
                    p.row.y + p.row.height - if below then 0 else 1
                  in
                  Array.iteri
                    (fun i (c : Matrix_grid.region) ->
                      if i > 0 then
                        Lines.vline ~weight ?style l
                          ~x:(c.x - f.pad - 1)
                          ~y:top
                          ~length:(bottom - top + 1))
                    p.cells));
          go ~above:(match r with Rule _ -> true | _ -> false) rest
    in
    go ~above:(Option.is_some f.edge)
      (List.combine rows (place ~x ~y f ws rows));
    Matrix_grid.draw_lines ?glyphs g l

  (* ───── Tables of text ───── *)

  type align = [ `Left | `Center | `Right ]
  type overflow = [ `Wrap | `Truncate ]
  type text_column = { align : align; overflow : overflow }
  type cell = { text : string; style : Ansi.Style.t }

  let cell ?(style = Ansi.Style.default) text = { text; style }

  type text_row = Cells of cell array | Separator of Lines.weight

  let tab_width = 2

  (* The widest of the pieces of [s]'s lines between their wrap breaks,
     without the blanks that end them, which hang where a row wraps. *)
  let widest_word ~width_method s =
    let widest = ref 0 in
    let line l =
      let start = ref 0 in
      let piece stop =
        let e = ref stop in
        while !e > !start && (l.[!e - 1] = ' ' || l.[!e - 1] = '\t') do
          decr e
        done;
        widest :=
          max !widest
            (Matrix_text.measure_sub ~width_method ~tab_width l ~pos:!start
               ~len:(!e - !start));
        start := stop
      in
      Matrix_text.iter_wrap_breaks
        (fun ~break_byte_offset:_ ~next_byte_offset ~grapheme_offset:_ ->
          piece next_byte_offset)
        l;
      piece (String.length l)
    in
    List.iter line (String.split_on_char '\n' s);
    !widest

  let max_content ~width_method s =
    Matrix_text.Layout.max_width
      (Matrix_text.Layout.create ~width_method ~tab_width
         (Matrix_text.Layout.Source.of_string s)
         ~width:0 ~wrap:`None)

  let layout ~width_method c width text =
    let src = Matrix_text.Layout.Source.of_string text in
    match c.overflow with
    | `Wrap ->
        Matrix_text.Layout.create ~width_method ~tab_width src ~width
          ~wrap:`Word
    | `Truncate ->
        Matrix_text.Layout.create ~width_method ~tab_width ~truncate:true src
          ~width ~wrap:`None

  let grid ?glyphs ?style ?(width_method = `Unicode) f ~room cols rows =
    let n = Array.length cols in
    let cells =
      List.filter_map
        (function
          | Cells cs ->
              if Array.length cs <> n then
                invalid_arg
                  (Printf.sprintf
                     "Matrix_cli.Table.grid: a row of %d cells in %d columns"
                     (Array.length cs) n);
              Some cs
          | Separator _ -> None)
        rows
    in
    let need i c =
      let fold f = List.fold_left (fun a cs -> max a (f cs.(i).text)) 0 cells in
      let max_ = fold (max_content ~width_method) in
      let min_ =
        match c.overflow with
        | `Truncate -> max_
        | `Wrap -> min max_ (fold (widest_word ~width_method))
      in
      { floor = min 1 min_; min = min_; max = max_; grow = 0 }
    in
    let needs = Array.mapi need cols and room = room - chrome f n in
    (* Below the min-content widths, the wrapped columns narrow first, the
       truncated ones held at theirs, and the truncated ones only once the
       wrapped ones are at their floors. *)
    let truncated i = cols.(i).overflow = `Truncate in
    let held =
      Array.mapi
        (fun i c -> if truncated i then { c with floor = c.min } else c)
        needs
    in
    let ws =
      if room >= Array.fold_left (fun a c -> a + c.floor) 0 held then
        widths held ~room
      else
        widths ~room
          (Array.mapi
             (fun i c ->
               if truncated i then c
               else { c with min = c.floor; max = c.floor })
             needs)
    in
    let rows_of cs =
      let h = ref 1 in
      Array.iteri
        (fun i c ->
          h :=
            max !h
              (Matrix_text.Layout.row_count
                 (layout ~width_method cols.(i) ws.(i) c.text)))
        cs;
      !h
    in
    let shape =
      List.map
        (function Cells cs -> Row (rows_of cs) | Separator w -> Rule w)
        rows
    in
    let g =
      Matrix_grid.create ~width_method
        ~width:(max 1 (width f ws))
        ~height:(max 1 (height f shape))
        ()
    in
    draw ?glyphs ?style g ~x:0 ~y:0 f ws shape;
    List.iter2
      (fun r (p : placed) ->
        match r with
        | Separator _ -> ()
        | Cells cs ->
            Array.iteri
              (fun i (reg : Matrix_grid.region) ->
                let c = cs.(i) and col = cols.(i) in
                let l = layout ~width_method col reg.width c.text in
                let src = Matrix_text.Layout.source l in
                for row = 0 to Matrix_text.Layout.row_count l - 1 do
                  (* A line is aligned by its cells up to its last one that is
                     not a blank. *)
                  let ink = ref 0 in
                  Matrix_text.Layout.iter_cells l ~row ~first_column:0
                    ~last_column:reg.width
                    (fun ~first ~last ~column ~width ~ellipsis ->
                      let t =
                        Matrix_text.Layout.Source.slice src ~first ~last
                      in
                      if ellipsis || (t <> " " && t <> "\t") then
                        ink := column + width);
                  let free = reg.width - !ink in
                  let x =
                    reg.x
                    +
                    match col.align with
                    | `Left -> 0
                    | `Center -> max 0 free / 2
                    | `Right -> max 0 free
                  in
                  Matrix_text.Layout.iter_cells l ~row ~first_column:0
                    ~last_column:reg.width
                    (fun ~first ~last ~column ~width:_ ~ellipsis ->
                      let text =
                        if ellipsis then "…"
                        else Matrix_text.Layout.Source.slice src ~first ~last
                      in
                      Matrix_grid.draw_text ~style:c.style g ~x:(x + column)
                        ~y:(reg.y + row) ~text)
                done)
              p.cells)
      rows
      (place ~x:0 ~y:0 f ws shape);
    g
end

module Tree = struct
  type guide = { branch : string; last : string; pipe : string; space : string }

  let single = { branch = "├── "; last = "└── "; pipe = "│   "; space = "    " }

  let ascii = { branch = "|-- "; last = "`-- "; pipe = "|   "; space = "    " }

  let fold ~children f roots acc =
    let rec node lasts x acc = siblings lasts (children x) (f lasts x acc)
    and siblings lasts xs acc =
      match xs with
      | [] -> acc
      | [ x ] -> node (true :: lasts) x acc
      | x :: xs -> siblings lasts xs (node (false :: lasts) x acc)
    in
    List.fold_left (fun acc r -> node [] r acc) acc roots

  let rails g lasts =
    String.concat ""
      (List.rev_map (fun last -> if last then g.space else g.pipe) lasts)

  let prefix g = function
    | [] -> ""
    | own :: up -> rails g up ^ if own then g.last else g.branch
end
