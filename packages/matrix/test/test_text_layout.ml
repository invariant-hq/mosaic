(* Properties of Matrix_text.Layout's hit testing, the geometry every text
   consumer asks: paint, selection, editor movement, gutters and diff.

   Two contracts, with different preconditions:
   - An exact round trip, [hit (locate c)] = [c], holds only for a boundary
     that is visible (not inside an omitted suffix) and unambiguous (no other
     boundary of its row shares its column). Zero-width runs and ellipses have
     no unique cell, so elsewhere there is nothing to invert.
   - Everywhere, hit snaps canonically: for a fixed affinity, a caret that hit
     returned is returned again from its own location, on the clicked row, at
     a boundary nearest to the clicked cell's left edge. *)

open Windtrap
module Layout = Matrix_text.Layout

(* ───── Generators ───── *)

(* Fragments that stress geometry: zero-width clusters, a combining mark,
   wide and ZWJ clusters, every line terminator, tabs, invalid UTF-8 and word
   separators. *)
let fragment =
  Gen.of_list
    [
      "a";
      "b";
      "word";
      " ";
      "\t";
      "\n";
      "\r";
      "\r\n";
      "\xe2\x80\x8b" (* U+200B ZERO WIDTH SPACE *);
      "e\xcc\x81" (* e U+0301 *);
      "\xcc\x81" (* a lone U+0301 *);
      "\xe6\x97\xa5" (* 日 *);
      "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" (* 👩‍💻 *);
      "\xff";
    ]

type case = {
  text : string;
  width : int;
  wrap : Layout.wrap;
  truncate : bool;
  width_method : Matrix_text.width_method;
  tab_width : int;
}

let pp_case ppf c =
  Format.fprintf ppf
    "{ text = %S; width = %d; wrap = %s; truncate = %b; width_method = %s; \
     tab_width = %d }"
    c.text c.width
    (match c.wrap with `None -> "`None" | `Char -> "`Char" | `Word -> "`Word")
    c.truncate
    (match c.width_method with
    | `Unicode -> "`Unicode"
    | `Unicode_wide -> "`Unicode_wide"
    | `Wcwidth -> "`Wcwidth")
    c.tab_width

let gen_case =
  let open Gen in
  let text = map (String.concat "") (list ~size:(int_range 0 12) fragment) in
  let wrap = of_list [ `None; `Char; `Word ] in
  let width_method = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  with_pp pp_case
    (map
       (fun (text, (width, wrap), (truncate, width_method), tab_width) ->
         { text; width; wrap; truncate; width_method; tab_width })
       (quad text
          (pair (int_range (-1) 8) wrap)
          (pair bool width_method) (int_range 1 3)))

let layout c =
  Layout.create ~width_method:c.width_method ~tab_width:c.tab_width
    ~truncate:c.truncate
    (Layout.Source.of_string c.text)
    ~width:c.width ~wrap:c.wrap

(* ───── Row facts, from the public queries ───── *)

let columns c l ~first ~last =
  Layout.Source.columns ~width_method:c.width_method ~tab_width:c.tab_width
    (Layout.source l) ~first ~last

let is_blank l g =
  let s = Layout.Source.slice (Layout.source l) ~first:g ~last:(g + 1) in
  s = " " || s = "\t"

(* The last boundary before a row's omitted suffix, or the row's end. A row
   narrower than its clusters without an ellipsis either hangs the blanks it
   wraps at, or was truncated at a width of 0 or less and shows only the
   clusters of width 0 that begin it. *)
let visible_last c l row =
  let first = Layout.row_start l row and last = ref (Layout.row_end l row) in
  Layout.iter_cells l ~row ~first_column:0 ~last_column:(Layout.row_width l row)
    (fun ~first ~last:_ ~column:_ ~width:_ ~ellipsis ->
      if ellipsis then last := first);
  if c.width > 0 && Layout.row_width l row < columns c l ~first ~last:!last then
    while
      !last > first
      && is_blank l (!last - 1)
      && Layout.row_width l row < columns c l ~first ~last:!last
    do
      decr last
    done
  else if Layout.row_width l row < columns c l ~first ~last:!last then begin
    let end_ = !last in
    last := first;
    while !last < end_ && columns c l ~first ~last:(!last + 1) = 0 do
      incr last
    done
  end;
  !last

let column_of c l row g = columns c l ~first:(Layout.row_start l row) ~last:g

let carets l =
  let n = Layout.Source.length (Layout.source l) in
  List.concat_map
    (fun g ->
      [
        Layout.{ grapheme = g; affinity = Upstream };
        Layout.{ grapheme = g; affinity = Downstream };
      ])
    (List.init (n + 1) Fun.id)

let pp_caret ppf (c : Layout.caret) =
  Format.fprintf ppf "{ grapheme = %d; affinity = %s }" c.grapheme
    (match c.affinity with
    | Upstream -> "Upstream"
    | Downstream -> "Downstream")

let caret = Testable.make ~pp:pp_caret ~equal:( = )
let position = pair int int

(* ───── Exact round trip ───── *)

(* Visible: not past the row's ellipsis. Unambiguous: neither neighbor on the
   row sits at the same column, and the row shows a cell, since a row that
   shows none puts every boundary at column 0, with its end. The end of the
   shown clusters of a row that hangs blanks sits at the column of the row's
   end. *)
let round_trips c l (k : Layout.caret) =
  let row, _ = Layout.locate l k in
  let g = k.grapheme in
  let first = Layout.row_start l row and last = visible_last c l row in
  g <= last
  && (last = Layout.row_end l row || Layout.row_width l row > 0)
  && (g < last
     || last = Layout.row_end l row
     || Layout.row_width l row > columns c l ~first ~last)
  && (g = first || columns c l ~first:(g - 1) ~last:g > 0)
  && (g = last || columns c l ~first:g ~last:(g + 1) > 0)

let exact_round_trip c =
  let l = layout c in
  List.iter
    (fun (k : Layout.caret) ->
      if round_trips c l k then begin
        let row, column = Layout.locate l k in
        let back = Layout.hit ~affinity:k.affinity l ~row ~column in
        equal ~msg:"same boundary" int k.grapheme back.grapheme;
        equal ~msg:"same position" position (row, column) (Layout.locate l back)
      end)
    (carets l)

(* ───── Canonical snapping ───── *)

let canonical_snapping c =
  let l = layout c in
  let n = Layout.Source.length (Layout.source l) in
  let rows = Layout.row_count l in
  for row = -1 to rows do
    for column = -1 to Layout.max_width l + 2 do
      List.iter
        (fun affinity ->
          let k = Layout.hit ~affinity l ~row ~column in
          is_true ~msg:"in the source" (k.grapheme >= 0 && k.grapheme <= n);
          if row < 0 then equal ~msg:"above snaps to start" int 0 k.grapheme
          else if row >= rows then
            equal ~msg:"below snaps to end" int n k.grapheme
          else begin
            let row', column' = Layout.locate l k in
            equal ~msg:"a fixed point" caret k
              (Layout.hit ~affinity l ~row:row' ~column:column');
            equal ~msg:"keeps the clicked row" int row row';
            (* Nearest to the cell's left edge, ties forward, among the
               boundaries before any ellipsis. *)
            let last = visible_last c l row in
            if last = Layout.row_end l row then begin
              let target = max 0 column in
              for g = Layout.row_start l row to last do
                let col = column_of c l row g in
                let d = abs (col - target) and d' = abs (column' - target) in
                is_true ~msg:"nearest boundary"
                  (d' < d || (d' = d && column' >= col))
              done
            end
          end)
        [ Layout.Upstream; Downstream ]
    done
  done

(* ───── Order ───── *)

let locate_is_monotonic c =
  let l = layout c in
  let rec check = function
    | a :: (b :: _ as rest) ->
        is_true ~msg:"later boundaries locate no earlier"
          (compare (Layout.locate l a) (Layout.locate l b) <= 0);
        check rest
    | _ -> ()
  in
  List.iter
    (fun affinity ->
      check
        (List.filter
           (fun (k : Layout.caret) -> k.affinity = affinity)
           (carets l)))
    [ Layout.Upstream; Downstream ]

(* ───── Regressions the properties found ───── *)

(* ───── Cells ───── *)

(* From any first column, a row's cells are its clusters that take a cell and
   end past that column, in order, at their columns: zero-width clusters and
   line terminators are skipped, whichever the text holds. *)
let cells_skip_zero_width c =
  let l = layout c in
  for row = 0 to Layout.row_count l - 1 do
    let first = Layout.row_start l row and last = visible_last c l row in
    let row_width = Layout.row_width l row in
    for first_column = 0 to row_width - 1 do
      let expected =
        List.filter_map
          (fun g ->
            let column = column_of c l row g
            and width = columns c l ~first:g ~last:(g + 1) in
            if width > 0 && column + width > first_column then
              Some (g, column, width)
            else None)
          (List.init (last - first) (fun i -> first + i))
      in
      let visited = ref [] in
      Layout.iter_cells l ~row ~first_column ~last_column:row_width
        (fun ~first ~last:_ ~column ~width ~ellipsis ->
          if not ellipsis then visited := (first, column, width) :: !visited);
      equal
        ~msg:(Printf.sprintf "row %d from column %d" row first_column)
        (list (triple int int int))
        expected (List.rev !visited)
    done
  done

let plain ?(width = 20) ?(truncate = false) text =
  Layout.create ~truncate (Layout.Source.of_string text) ~width ~wrap:`None

let zwsp = "\xe2\x80\x8b"

let past_row_end_snaps_zero_width_run () =
  let l = plain ("ab" ^ zwsp) in
  equal int 2 (Layout.hit ~affinity:Upstream l ~row:0 ~column:5).grapheme;
  equal int 3 (Layout.hit ~affinity:Downstream l ~row:0 ~column:5).grapheme

let nearest_boundary_snaps_zero_width_run () =
  (* The click at column 4 falls inside 日; its following edge is shared with
     the zero-width space. *)
  let l = plain ("aaa\xe6\x97\xa5" ^ zwsp) in
  equal int 4 (Layout.hit ~affinity:Upstream l ~row:0 ~column:4).grapheme;
  equal int 5 (Layout.hit ~affinity:Downstream l ~row:0 ~column:4).grapheme

let ellipsis_edge_snaps_zero_width_run () =
  let l = plain ~width:3 ~truncate:true ("\xe6\x97\xa5" ^ zwsp ^ "aaa") in
  equal int 1 (Layout.hit ~affinity:Upstream l ~row:0 ~column:2).grapheme;
  equal int 2 (Layout.hit ~affinity:Downstream l ~row:0 ~column:2).grapheme;
  (* An ellipsis at column 0: a click left of it is a click on it. *)
  let l = plain ~width:1 ~truncate:true (zwsp ^ "aa") in
  equal int 0 (Layout.hit ~affinity:Upstream l ~row:0 ~column:(-1)).grapheme;
  equal int 0 (Layout.hit ~affinity:Upstream l ~row:0 ~column:0).grapheme;
  equal int 1 (Layout.hit ~affinity:Downstream l ~row:0 ~column:(-1)).grapheme

let omitted_suffix_end_locates_after_ellipsis () =
  let l = plain ~width:3 ~truncate:true "abcdef" in
  List.iter
    (fun affinity ->
      equal position (0, 3) (Layout.locate l Layout.{ grapheme = 6; affinity }))
    [ Layout.Upstream; Downstream ];
  equal position (0, 2)
    (Layout.locate l Layout.{ grapheme = 4; affinity = Upstream })

(* At a width of 0 or less a truncated line has no cell for its clusters or
   for the ellipsis, so its row shows nothing. *)
let truncation_without_cells () =
  List.iter
    (fun width ->
      let msg what = Printf.sprintf "%s at width %d" what width in
      let l = plain ~width ~truncate:true (zwsp ^ "abc\nd") in
      equal ~msg:(msg "rows") int 2 (Layout.row_count l);
      equal ~msg:(msg "max width") int 0 (Layout.max_width l);
      equal ~msg:(msg "row width") int 0 (Layout.row_width l 0);
      equal ~msg:(msg "row end") int 4 (Layout.row_end l 0);
      Layout.iter_cells l ~row:0 ~first_column:(-2) ~last_column:2
        (fun ~first:_ ~last:_ ~column:_ ~width:_ ~ellipsis:_ ->
          fail (msg "a cell"));
      equal ~msg:(msg "locate") position (0, 0)
        (Layout.locate l Layout.{ grapheme = 2; affinity = Upstream });
      equal ~msg:(msg "hit") int 4 (Layout.hit l ~row:0 ~column:0).grapheme)
    [ 0; -2 ]

(* A row out of range stands for the start or the end of the source, in
   coverage as in hit. *)
let coverage_maps_rows_outside () =
  let l =
    Layout.create (Layout.Source.of_string "ab\ncd") ~width:0 ~wrap:`None
  in
  let range = pair int int in
  equal ~msg:"a row before the first" range (0, 0)
    (Layout.coverage l ~row:(-1) ~column:1);
  equal ~msg:"hit there" int 0 (Layout.hit l ~row:(-1) ~column:1).grapheme;
  equal ~msg:"a row after the last" range (5, 5)
    (Layout.coverage l ~row:2 ~column:0);
  equal ~msg:"hit there" int 5 (Layout.hit l ~row:2 ~column:0).grapheme

(* ───── Word wrap ───── *)

let rows l =
  let src = Layout.source l in
  List.init (Layout.row_count l) (fun r ->
      ( Layout.Source.slice src ~first:(Layout.row_start l r)
          ~last:(Layout.row_end l r),
        Layout.row_width l r ))

(* A word that fills its row is followed by the blank it wraps at, which
   hangs at the row's end: it gave a row of its own. *)
let blanks_hang_where_a_word_fills_a_row () =
  let wrapped ~width text =
    rows (Layout.create (Layout.Source.of_string text) ~width ~wrap:`Word)
  in
  let rows_list = list (pair string int) in
  equal ~msg:"a word fills the row" rows_list
    [ ("Non-", 4); ("blocking ", 8); ("streamin", 8); ("g", 1) ]
    (wrapped ~width:8 "Non-blocking streaming");
  equal ~msg:"blanks at the end of the line" rows_list
    [ ("aaaa  ", 4) ]
    (wrapped ~width:4 "aaaa  ");
  equal ~msg:"a run of blanks and a TAB" rows_list
    [ ("ab \t ", 2); ("cd", 2) ]
    (wrapped ~width:2 "ab \t cd");
  equal ~msg:"a blank that fits hangs too" rows_list
    [ ("aaa ", 3); ("bbb  ", 3); ("c", 1) ]
    (wrapped ~width:4 "aaa bbb  c");
  equal ~msg:"blanks that fit at the end of a line stay" rows_list
    [ ("ab  ", 4) ]
    (wrapped ~width:4 "ab  ");
  equal ~msg:"character wrap hangs them as well" rows_list
    [ ("ab ", 2); ("cd", 2) ]
    (rows
       (Layout.create (Layout.Source.of_string "ab cd") ~width:2 ~wrap:`Char));
  let l =
    Layout.create (Layout.Source.of_string "aaaa bb") ~width:4 ~wrap:`Word
  in
  equal ~msg:"the hanging blank is shown in no cell" (list int) [ 0; 1; 2; 3 ]
    (let cs = ref [] in
     Layout.iter_cells l ~row:0 ~first_column:0 ~last_column:10
       (fun ~first ~last:_ ~column:_ ~width:_ ~ellipsis:_ -> cs := first :: !cs);
     List.rev !cs);
  equal ~msg:"a caret after it is at the row's end" position (0, 4)
    (Layout.locate l Layout.{ grapheme = 5; affinity = Upstream })

(* When rows wrap, no continuation starts with a blank, a row followed by a
   continuation shows no blank at its end, and a row is no wider than the
   width unless it holds a single cluster of positive width. *)
let wrapped_rows_fit c =
  if c.wrap <> `None && c.width > 0 then begin
    let l = layout c in
    let src = Layout.source l in
    for row = 0 to Layout.row_count l - 1 do
      let first = Layout.row_start l row in
      let text = Layout.Source.slice src ~first ~last:(first + 1) in
      if Layout.continuation l row > 0 then
        is_true
          ~msg:(Printf.sprintf "row %d starts with %S" row text)
          (text <> " " && text <> "\t");
      let last = visible_last c l row in
      if
        row + 1 < Layout.row_count l
        && Layout.continuation l (row + 1) > 0
        && last > first
      then
        is_true
          ~msg:(Printf.sprintf "row %d shows a blank at its end" row)
          (not (is_blank l (last - 1)));
      let wide =
        List.length
          (List.filter
             (fun g -> columns c l ~first:g ~last:(g + 1) > 0)
             (List.init (last - first) (fun i -> first + i)))
      in
      if Layout.row_width l row > c.width then
        is_true
          ~msg:(Printf.sprintf "row %d is %d wide" row (Layout.row_width l row))
          (wide = 1)
    done
  end

(* ───── Edits ─────

   A source made by edits answers every query as the source of the same text
   made from scratch, and so do its layouts, carried from those of the
   sources it was made from. Before each edit the source is laid out at each
   geometry, and every other step it is fully read, so that sources with and
   without indexed lines and built rows are edited. *)

type edit = Replace of int * int * string | Undo

type geometry = {
  g_width : int;
  g_wrap : Layout.wrap;
  g_truncate : bool;
  g_method : Matrix_text.width_method;
  g_tab : int;
}

type session = { start : string; geometries : geometry list; edits : edit list }

let pp_session ppf s =
  Format.fprintf ppf "{ start = %S; geometries = [%s]; edits = [%s] }" s.start
    (String.concat "; "
       (List.map
          (fun g ->
            Printf.sprintf "(%d, %s, %b, %s, %d)" g.g_width
              (match g.g_wrap with
              | `None -> "`None"
              | `Char -> "`Char"
              | `Word -> "`Word")
              g.g_truncate
              (match g.g_method with
              | `Unicode -> "`Unicode"
              | `Unicode_wide -> "`Unicode_wide"
              | `Wcwidth -> "`Wcwidth")
              g.g_tab)
          s.geometries))
    (String.concat "; "
       (List.map
          (function
            | Replace (at, removed, s) ->
                Printf.sprintf "Replace (%d, %d, %S)" at removed s
            | Undo -> "Undo")
          s.edits))

let gen_session =
  let open Gen in
  let piece =
    one_of
      [
        fragment;
        of_list
          [
            "\xe2\x82" (* a truncated scalar *);
            "\xf0\x9f\x87\xab" (* a regional indicator *);
            "\xd8\x80" (* U+0600, which joins what follows *);
            "x\r";
            "\ny";
          ];
      ]
  in
  let text size = map (String.concat "") (list ~size piece) in
  let edit =
    frequency
      [
        ( 4,
          map
            (fun (at, removed, s) -> Replace (at, removed, s))
            (triple (int_range 0 60) (int_range 0 6) (text (int_range 0 3))) );
        (1, pure Undo);
      ]
  in
  let geometry =
    of_list
      [
        {
          g_width = 7;
          g_wrap = `Word;
          g_truncate = false;
          g_method = `Unicode;
          g_tab = 2;
        };
        {
          g_width = 7;
          g_wrap = `Char;
          g_truncate = false;
          g_method = `Wcwidth;
          g_tab = 4;
        };
        {
          g_width = 5;
          g_wrap = `None;
          g_truncate = true;
          g_method = `Unicode;
          g_tab = 2;
        };
        {
          g_width = 1;
          g_wrap = `Word;
          g_truncate = false;
          g_method = `Unicode_wide;
          g_tab = 3;
        };
        {
          g_width = 0;
          g_wrap = `None;
          g_truncate = false;
          g_method = `Unicode;
          g_tab = 1;
        };
        {
          g_width = 3;
          g_wrap = `None;
          g_truncate = true;
          g_method = `Unicode_wide;
          g_tab = 2;
        };
        {
          g_width = -1;
          g_wrap = `Word;
          g_truncate = false;
          g_method = `Wcwidth;
          g_tab = 2;
        };
      ]
  in
  with_pp pp_session
    (map
       (fun (start, geometries, edits) -> { start; geometries; edits })
       (triple
          (text (int_range 0 16))
          (list ~size:(int_range 1 4) geometry)
          (list ~size:(int_range 1 16) edit)))

let lay_out source g =
  Layout.create ~width_method:g.g_method ~tab_width:g.g_tab
    ~truncate:g.g_truncate source ~width:g.g_width ~wrap:g.g_wrap

(* Everything a reader asks of a source and its layouts. *)
let agree ~msg geometries edited =
  let module S = Layout.Source in
  let text = S.text edited in
  let fresh = S.of_string text in
  let msg what = Printf.sprintf "%s: %s of %S" msg what text in
  let ints what f = equal ~msg:(msg what) (list int) (f fresh) (f edited) in
  let upto n f = List.init (n + 1) f in
  ints "lines" (fun s ->
      S.length s :: S.line_count s
      :: List.concat
           (List.init (S.line_count s) (fun l ->
                [ S.line_start s l; S.line_end s l ])));
  ints "boundaries" (fun s ->
      upto (S.length s) (fun g -> S.byte_offset s g)
      @ upto (S.length s) (fun g -> S.line_of_offset s g));
  ints "bytes" (fun s ->
      upto (String.length text) (fun b ->
          S.grapheme_of_byte s ~affinity:Upstream b
          + (1000 * S.grapheme_of_byte s ~affinity:Downstream b)));
  List.iter
    (fun g ->
      ints "columns" (fun s ->
          List.init (S.line_count s) (fun l ->
              S.columns ~width_method:g.g_method ~tab_width:g.g_tab s
                ~first:(S.line_start s l) ~last:(S.line_end s l))
          @ [
              S.columns ~width_method:g.g_method ~tab_width:g.g_tab s
                ~first:(S.length s) ~last:0;
            ]);
      let a = lay_out fresh g and b = lay_out edited g in
      let rows l =
        Layout.row_count l :: Layout.max_width l
        :: List.concat
             (List.init (Layout.row_count l) (fun r ->
                  let cells = ref [] in
                  Layout.iter_cells l ~row:r ~first_column:0
                    ~last_column:(Layout.max_width l + 1)
                    (fun ~first ~last ~column ~width ~ellipsis ->
                      cells :=
                        first :: last :: column :: width :: Bool.to_int ellipsis
                        :: !cells);
                  Layout.row_start l r :: Layout.row_end l r
                  :: Layout.row_width l r :: Layout.logical_line l r
                  :: Layout.continuation l r :: !cells))
      in
      equal ~msg:(msg "rows") (list int) (rows a) (rows b);
      let n = S.length fresh in
      List.iter
        (fun affinity ->
          let carets l =
            List.concat
              (upto n (fun grapheme ->
                   let row, column = Layout.locate l { grapheme; affinity } in
                   [ row; column ]))
            @ List.concat
                (List.init
                   (Layout.row_count l + 2)
                   (fun row ->
                     List.concat
                       (List.init
                          (Layout.max_width l + 3)
                          (fun column ->
                            let k =
                              Layout.hit ~affinity l ~row:(row - 1)
                                ~column:(column - 1)
                            in
                            let first, last =
                              Layout.coverage l ~row:(row - 1)
                                ~column:(column - 1)
                            in
                            [
                              k.grapheme;
                              Bool.to_int (k.affinity = Upstream);
                              first;
                              last;
                            ]))))
          in
          equal ~msg:(msg "carets") (list int) (carets a) (carets b))
        [ Layout.Upstream; Downstream ])
    geometries

let edits_match_fresh_indexing s =
  let source = ref (Layout.Source.of_string s.start) and undo = ref [] in
  let last_step = List.length s.edits - 1 in
  List.iteri
    (fun step edit ->
      List.iter
        (fun g ->
          let l = lay_out !source g in
          ignore (Layout.row_start l (Layout.row_count l / 2) : int))
        s.geometries;
      let text = Layout.Source.text !source in
      let first, last, inserted =
        match (edit, !undo) with
        | Replace (at, removed, inserted), _ ->
            let first = at mod (String.length text + 1) in
            let last = min (String.length text) (first + removed) in
            undo :=
              ( first,
                first + String.length inserted,
                String.sub text first (last - first) )
              :: !undo;
            (first, last, inserted)
        | Undo, change :: rest ->
            undo := rest;
            change
        | Undo, [] -> (0, 0, "")
      in
      source := Layout.Source.edit !source ~first ~last inserted;
      let expected =
        String.sub text 0 first ^ inserted
        ^ String.sub text last (String.length text - last)
      in
      equal ~msg:"edited text" string expected (Layout.Source.text !source);
      if step mod 2 = 1 || step = last_step then
        agree ~msg:(Printf.sprintf "edit %d" step) s.geometries !source)
    s.edits

let edit_bounds () =
  let source = Layout.Source.of_string "ab" in
  raises
    (Invalid_argument "Matrix_text.Layout.Source.edit: bytes out of bounds")
    (fun () -> ignore (Layout.Source.edit source ~first:1 ~last:3 "x"));
  raises
    (Invalid_argument "Matrix_text.Layout.Source.edit: bytes out of bounds")
    (fun () -> ignore (Layout.Source.edit source ~first:2 ~last:1 "x"));
  equal ~msg:"the source edited is unchanged" string "ab"
    (Layout.Source.text source)

let () =
  run "matrix.text_layout"
    [
      group "Regressions"
        [
          test "past a row's end snaps a zero-width run"
            past_row_end_snaps_zero_width_run;
          test "nearest boundary snaps a zero-width run"
            nearest_boundary_snaps_zero_width_run;
          test "ellipsis edge snaps a zero-width run"
            ellipsis_edge_snaps_zero_width_run;
          test "omitted suffix end locates after the ellipsis"
            omitted_suffix_end_locates_after_ellipsis;
          test "coverage maps rows outside as hit does"
            coverage_maps_rows_outside;
          test "truncation without cells" truncation_without_cells;
          test "blanks hang where a word fills a row"
            blanks_hang_where_a_word_fills_a_row;
        ];
      group "Hit and locate"
        [
          prop ~count:1000 "visible unambiguous boundaries round-trip" gen_case
            exact_round_trip;
          prop ~count:1000 "hit snaps canonically" gen_case canonical_snapping;
          prop ~count:1000 "locate is monotonic" gen_case locate_is_monotonic;
        ];
      group "Wrapping"
        [
          prop ~count:1000 "blanks hang where rows wrap" gen_case
            wrapped_rows_fit;
        ];
      group "Cells"
        [
          prop ~count:1000 "cells skip zero-width clusters" gen_case
            cells_skip_zero_width;
        ];
      group "Edits"
        [
          test "bytes out of bounds" edit_bounds;
          prop ~count:500 "edits match indexing from scratch" gen_session
            edits_match_fresh_indexing;
        ];
    ]
