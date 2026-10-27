(* ───── Column ───── *)

type alignment = [ `Left | `Center | `Right ]
type width = [ `Fixed of int | `Auto | `Flex of float ]
type overflow = [ `Ellipsis | `Crop ]

type column = {
  header : string;
  width : width;
  alignment : alignment;
  overflow : overflow;
  min_width : int option;
  max_width : int option;
}

let column ?(width = `Auto) ?(alignment = `Left) ?(overflow = `Ellipsis)
    ?min_width ?max_width header =
  { header; width; alignment; overflow; min_width; max_width }

let column_equal (a : column) (b : column) =
  String.equal a.header b.header
  && a.width = b.width && a.alignment = b.alignment && a.overflow = b.overflow
  && Option.equal Int.equal a.min_width b.min_width
  && Option.equal Int.equal a.max_width b.max_width

let rec columns_equal xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> column_equal x y && columns_equal xs ys
  | _ -> false

(* ───── Cell ───── *)

type cell =
  | Plain of { text : string; style : Ansi.Style.t option }
  | Rich of Text.fragment list

let cell ?style s = Plain { text = s; style }
let rich fs = Rich fs

let cell_equal a b =
  match (a, b) with
  | Plain a, Plain b ->
      String.equal a.text b.text
      && Option.equal Ansi.Style.equal a.style b.style
  | Rich a, Rich b -> Text.fragments_equal a b
  | _ -> false

let cell_plain_text = function
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

let row_equal (a : cell array) (b : cell array) =
  let la = Array.length a and lb = Array.length b in
  if la <> lb then false
  else
    let rec loop i = i >= la || (cell_equal a.(i) b.(i) && loop (i + 1)) in
    loop 0

(* ───── Props ───── *)

type action =
  | Move_up
  | Move_down
  | Move_up_fast
  | Move_down_fast
  | Page_up
  | Page_down
  | Activate

type key_binding = Shortcut.t * action

let default_bindings =
  Shortcut.
    [
      (up, Move_up);
      (down, Move_down);
      (char 'k', Move_up);
      (char 'j', Move_down);
      (key ~shift:true Matrix_input.Keystroke.Up, Move_up_fast);
      (key ~shift:true Matrix_input.Keystroke.Down, Move_down_fast);
      (key Matrix_input.Keystroke.Page_up, Page_up);
      (key Matrix_input.Keystroke.Page_down, Page_down);
      (enter, Activate);
      (ctrl 'j', Activate);
    ]

let default_keymap = Keymap.make ~defaults:default_bindings ()

let keymap = function
  | [] -> default_keymap
  | custom -> Keymap.make ~defaults:default_bindings ~custom ()

(* [Props.make] is the one place the table's labels get their
   defaults: [Mosaic.table] passes its labels on unset. *)
module Props = struct
  type t = {
    key_bindings : key_binding list;
    columns : column list;
    rows : cell array Items.t;
    selected_row : int;
    border : bool;
    border_style : Matrix_grid.Border.t;
    show_header : bool;
    show_column_separator : bool;
    show_row_separator : bool;
    cell_padding : int;
    header_color : Ansi.Color.t option;
    header_background : Ansi.Color.t option;
    text_color : Ansi.Color.t option;
    background : Ansi.Color.t option;
    selected_text_color : Ansi.Color.t option;
    selected_background : Ansi.Color.t option;
    focused_selected_text_color : Ansi.Color.t option;
    focused_selected_background : Ansi.Color.t option;
    selection_visible : bool;
    show_scroll_indicator : bool;
    activate_on_click : bool;
    wheel_navigation : bool;
    row_styles : Ansi.Style.t list;
    wrap_selection : bool;
    fast_scroll_step : int;
  }

  let make ?(columns = []) ?(rows = []) ?(selected_row = 0) ?(border = true)
      ?(border_style = Matrix_grid.Border.single) ?(show_header = true)
      ?(show_column_separator = false) ?(show_row_separator = false)
      ?(cell_padding = 0) ?header_color ?header_background ?text_color
      ?background ?selected_text_color ?selected_background
      ?focused_selected_text_color ?focused_selected_background
      ?(selection_visible = true) ?(show_scroll_indicator = false)
      ?(activate_on_click = false) ?(wheel_navigation = true) ?(row_styles = [])
      ?(wrap_selection = false) ?(key_bindings = []) ?(fast_scroll_step = 5) ()
      =
    {
      key_bindings;
      columns;
      rows = Items.of_list rows;
      selected_row = max (-1) selected_row;
      border;
      border_style;
      show_header;
      show_column_separator;
      show_row_separator;
      cell_padding = max 0 cell_padding;
      header_color;
      header_background;
      text_color;
      background;
      selected_text_color;
      selected_background;
      focused_selected_text_color;
      focused_selected_background;
      selection_visible;
      show_scroll_indicator;
      activate_on_click;
      wheel_navigation;
      row_styles;
      wrap_selection;
      fast_scroll_step = max 1 fast_scroll_step;
    }

  let default = make ()

  let rec styles_equal xs ys =
    match (xs, ys) with
    | [], [] -> true
    | x :: xs, y :: ys -> Ansi.Style.equal x y && styles_equal xs ys
    | _ -> false

  let equal a b =
    List.equal (Keymap.binding_equal ( = )) a.key_bindings b.key_bindings
    && columns_equal a.columns b.columns
    && Items.equal row_equal a.rows b.rows
    && Int.equal a.selected_row b.selected_row
    && Bool.equal a.border b.border
    && a.border_style = b.border_style
    && Bool.equal a.show_header b.show_header
    && Bool.equal a.show_column_separator b.show_column_separator
    && Bool.equal a.show_row_separator b.show_row_separator
    && Int.equal a.cell_padding b.cell_padding
    && Option.equal Ansi.Color.equal a.header_color b.header_color
    && Option.equal Ansi.Color.equal a.header_background b.header_background
    && Option.equal Ansi.Color.equal a.text_color b.text_color
    && Option.equal Ansi.Color.equal a.background b.background
    && Option.equal Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Option.equal Ansi.Color.equal a.selected_background b.selected_background
    && Option.equal Ansi.Color.equal a.focused_selected_text_color
         b.focused_selected_text_color
    && Option.equal Ansi.Color.equal a.focused_selected_background
         b.focused_selected_background
    && Bool.equal a.selection_visible b.selection_visible
    && Bool.equal a.show_scroll_indicator b.show_scroll_indicator
    && Bool.equal a.activate_on_click b.activate_on_click
    && Bool.equal a.wheel_navigation b.wheel_navigation
    && styles_equal a.row_styles b.row_styles
    && Bool.equal a.wrap_selection b.wrap_selection
    && Int.equal a.fast_scroll_step b.fast_scroll_step

  let with_rows_of f l props = { props with rows = Items.map_list f l }
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable keymap : action Keymap.t;
  mutable col_specs : column array;
  mutable data_rows : cell array array;
  mutable selected_row : int;
  mutable pointer : (int * int) option;
      (* The pointer's last position over the table, in terminal cells, or
         [None] once it has left. *)
  mutable hovered : (int * cell array) option;
      (* The data row last reported under the pointer, and its cells. *)
  mutable auto_widths : (Matrix.Text.width_method * int array) option;
      (* Cached per-column content widths for [`Auto] columns, excluding
         padding, tagged with the width method used to measure them. Reset to
         [None] whenever columns or rows change. *)
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
  mutable on_hover : (int option -> unit) option;
}

let node t = t.node

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let row_count t = Array.length t.data_rows

(* The data rows the table's height shows below its border and header. *)
let max_visible_rows t =
  let border_v = if t.props.border then 2 else 0 in
  let header_total =
    if t.props.show_header then 1 + if t.props.border then 1 else 0 else 0
  in
  let content_height = Renderable.height t.node - header_total - border_v in
  let row_height = if t.props.show_row_separator then 2 else 1 in
  max 1 ((content_height + row_height - 1) / row_height)

let scroll_indicator_visible t =
  t.props.show_scroll_indicator && row_count t > max_visible_rows t

let clamp_index t idx =
  let len = row_count t in
  if len = 0 then -1 else max (-1) (min (len - 1) idx)

(* ───── Column Width Computation ───── *)

(* Measure with the width method of the screen the table renders into, so
   column sizing agrees with how Matrix_grid.draw_text lays cells down. *)
let text_width ~width_method s =
  Matrix.Text.measure ~width_method ~tab_width:2 s

(* Content width (max of header and every cell) per [`Auto] column, excluding
   padding. Measuring every cell of every row is the expensive part of column
   sizing and only depends on the data, so it is cached on [t] and invalidated
   by [set_rows]/[set_columns]; hover- and selection-driven renders then reuse
   the measurements. *)
let auto_content_widths t ~width_method =
  let ncols = Array.length t.col_specs in
  match t.auto_widths with
  | Some (m, widths) when m = width_method && Array.length widths = ncols ->
      widths
  | _ ->
      let widths = Array.make ncols 0 in
      Array.iteri
        (fun i col ->
          match col.width with
          | `Auto ->
              let header_w = text_width ~width_method col.header in
              let max_cell_w =
                Array.fold_left
                  (fun acc row ->
                    if i < Array.length row then
                      max acc
                        (text_width ~width_method (cell_plain_text row.(i)))
                    else acc)
                  0 t.data_rows
              in
              widths.(i) <- max header_w max_cell_w
          | `Fixed _ | `Flex _ -> ())
        t.col_specs;
      t.auto_widths <- Some (width_method, widths);
      widths

let compute_column_widths t ~width_method ~available_width =
  let ncols = Array.length t.col_specs in
  if ncols = 0 then [||]
  else
    let auto_widths = auto_content_widths t ~width_method in
    let widths = Array.make ncols 0 in
    let pad = t.props.cell_padding in
    let pad2 = 2 * pad in
    (* Always reserve 1 char gap between adjacent columns *)
    let gap_width = if ncols > 1 then ncols - 1 else 0 in
    let border_width = if t.props.border then 2 else 0 in
    let usable = max 0 (available_width - gap_width - border_width) in
    let fixed_total = ref 0 in
    let flex_total = ref 0.0 in
    (* Pass 1: resolve Auto and Fixed (widths include padding) *)
    Array.iteri
      (fun i col ->
        match col.width with
        | `Fixed n ->
            widths.(i) <- max 1 (n + pad2);
            fixed_total := !fixed_total + widths.(i)
        | `Auto ->
            let content_w = auto_widths.(i) in
            let w = max 1 (content_w + pad2) in
            (* Apply min/max constraints *)
            let w =
              match col.min_width with Some m -> max w (m + pad2) | None -> w
            in
            let w =
              match col.max_width with Some m -> min w (m + pad2) | None -> w
            in
            widths.(i) <- max 1 w;
            fixed_total := !fixed_total + widths.(i)
        | `Flex f -> flex_total := !flex_total +. f)
      t.col_specs;
    (* Pass 2: distribute remaining space among Flex columns *)
    let remaining = max 0 (usable - !fixed_total) in
    if !flex_total > 0.0 then
      Array.iteri
        (fun i col ->
          match col.width with
          | `Flex f ->
              let w =
                max 1 (int_of_float (float remaining *. f /. !flex_total))
              in
              let w = max w (pad2 + 1) in
              let w =
                match col.min_width with
                | Some m -> max w (m + pad2)
                | None -> w
              in
              let w =
                match col.max_width with
                | Some m -> min w (m + pad2)
                | None -> w
              in
              widths.(i) <- w
          | _ -> ())
        t.col_specs;
    widths

(* ───── Scrolling ───── *)

(* The first visible row. It centers the selected row when possible, clamped so
   that it never goes negative or past the last page. It is a function of the
   accepted selection, the rows and the geometry, never stored, so a remounted
   table shows the same rows and only an accepted selection scrolls. *)
let scroll_offset t =
  let max_visible = max_visible_rows t in
  let max_off = max 0 (row_count t - max_visible) in
  max 0 (min (t.selected_row - (max_visible / 2)) max_off)

(* Reports [row] as the hovered row when it differs from the last report, by
   its index or by its cells: after the rows change, an index can name a row
   with other content. Rows rebuilt equal report nothing. *)
let report_hover t row =
  let hovered = Option.map (fun index -> (index, t.data_rows.(index))) row in
  let same (i, a) (j, b) =
    Int.equal i j
    && Array.length a = Array.length b
    && Array.for_all2 cell_equal a b
  in
  if not (Option.equal same hovered t.hovered) then (
    t.hovered <- hovered;
    (match t.on_hover with None -> () | Some f -> f row);
    request t)

let set_selected_row_internal t idx =
  let len = row_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_row then
      match t.on_change with None -> () | Some f -> f idx

let data_row_at t ~x ~y =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if x < 0 || x >= width || y < 0 || y >= height then None
  else if t.props.border && (x = 0 || x = width - 1 || y = 0 || y = height - 1)
  then None
  else
    let border_top = if t.props.border then 1 else 0 in
    let header_rows =
      if t.props.show_header then 1 + if t.props.border then 1 else 0 else 0
    in
    let row_y = y - border_top - header_rows in
    let indicator_x = width - if t.props.border then 2 else 1 in
    if row_y < 0 || (scroll_indicator_visible t && x = indicator_x) then None
    else
      let row_height = if t.props.show_row_separator then 2 else 1 in
      if row_y mod row_height <> 0 then None
      else
        let index = scroll_offset t + (row_y / row_height) in
        if index < row_count t then Some index else None

(* The row under the pointer's last position. The rows, the selection that
   scrolls them and the table's geometry can all change under a still
   pointer, so each change reports this again. *)
let row_under_pointer t =
  match t.pointer with
  | None -> None
  | Some (x, y) ->
      data_row_at t ~x:(x - Renderable.x t.node) ~y:(y - Renderable.y t.node)

let refresh_hover t = report_hover t (row_under_pointer t)

(* ───── Public Accessors ───── *)

let columns t = Array.to_list t.col_specs
let rows t = Array.to_list t.data_rows
let selected_row t = t.selected_row

(* ───── Data ───── *)

let set_columns t cols =
  t.col_specs <- Array.of_list cols;
  t.auto_widths <- None;
  Renderable.mark_dirty t.node;
  request t

let set_source t rows =
  t.props <- { t.props with rows };
  t.data_rows <- Items.to_array rows;
  t.auto_widths <- None;
  t.selected_row <- clamp_index t t.selected_row;
  refresh_hover t;
  Renderable.mark_dirty t.node;
  request t

let set_rows t data = set_source t (Items.of_list data)

let set_selected_row t idx =
  let idx = clamp_index t idx in
  if idx <> t.selected_row then (
    t.selected_row <- idx;
    refresh_hover t;
    request t)

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = row_count t in
  if len = 0 then ()
  else if t.selected_row < 0 then set_selected_row_internal t (len - 1)
  else
    let new_index = t.selected_row - steps in
    if new_index >= 0 then set_selected_row_internal t new_index
    else if t.props.wrap_selection then set_selected_row_internal t (len - 1)
    else set_selected_row_internal t 0

let move_down ?(steps = 1) t =
  let len = row_count t in
  if len = 0 then ()
  else if t.selected_row < 0 then set_selected_row_internal t 0
  else
    let new_index = t.selected_row + steps in
    if new_index < len then set_selected_row_internal t new_index
    else if t.props.wrap_selection then set_selected_row_internal t 0
    else set_selected_row_internal t (len - 1)

let page_up t =
  set_selected_row_internal t
    (if t.selected_row < 0 then row_count t - 1
     else max 0 (t.selected_row - max_visible_rows t))

let page_down t =
  let last_row = row_count t - 1 in
  set_selected_row_internal t
    (if t.selected_row < 0 then 0
     else min last_row (t.selected_row + max_visible_rows t))

(* ───── Display Setters ───── *)

let set_border t v =
  if t.props.border <> v then (
    t.props <- { t.props with border = v };
    Renderable.mark_dirty t.node;
    request t)

let set_border_style t s =
  if t.props.border_style <> s then (
    t.props <- { t.props with border_style = s };
    request t)

let set_show_header t v =
  if t.props.show_header <> v then (
    t.props <- { t.props with show_header = v };
    Renderable.mark_dirty t.node;
    request t)

let set_show_column_separator t v =
  if t.props.show_column_separator <> v then (
    t.props <- { t.props with show_column_separator = v };
    request t)

let set_show_row_separator t v =
  if t.props.show_row_separator <> v then (
    t.props <- { t.props with show_row_separator = v };
    Renderable.mark_dirty t.node;
    request t)

(* ───── Color Setters ───── *)

let set_header_color t c =
  if not (Option.equal Ansi.Color.equal t.props.header_color (Some c)) then (
    t.props <- { t.props with header_color = Some c };
    request t)

let set_header_background t c =
  if not (Option.equal Ansi.Color.equal t.props.header_background (Some c)) then (
    t.props <- { t.props with header_background = Some c };
    request t)

let set_text_color t c =
  if not (Option.equal Ansi.Color.equal t.props.text_color (Some c)) then (
    t.props <- { t.props with text_color = Some c };
    request t)

let set_background t c =
  if not (Option.equal Ansi.Color.equal t.props.background (Some c)) then (
    t.props <- { t.props with background = Some c };
    request t)

let set_selected_text_color t c =
  if not (Option.equal Ansi.Color.equal t.props.selected_text_color (Some c))
  then (
    t.props <- { t.props with selected_text_color = Some c };
    request t)

let set_selected_background t c =
  if not (Option.equal Ansi.Color.equal t.props.selected_background (Some c))
  then (
    t.props <- { t.props with selected_background = Some c };
    request t)

let set_selection_visible t visible =
  if t.props.selection_visible <> visible then (
    t.props <- { t.props with selection_visible = visible };
    request t)

let set_show_scroll_indicator t show =
  if t.props.show_scroll_indicator <> show then (
    t.props <- { t.props with show_scroll_indicator = show };
    Renderable.mark_dirty t.node;
    request t)

let set_activate_on_click t activate =
  if t.props.activate_on_click <> activate then (
    t.props <- { t.props with activate_on_click = activate };
    request t)

let set_wheel_navigation t enabled =
  if t.props.wheel_navigation <> enabled then (
    t.props <- { t.props with wheel_navigation = enabled };
    request t)

(* ───── Padding & Row Styles Setters ───── *)

let set_cell_padding t n =
  let n = max 0 n in
  if t.props.cell_padding <> n then (
    t.props <- { t.props with cell_padding = n };
    Renderable.mark_dirty t.node;
    request t)

let set_row_styles t styles =
  if not (Props.styles_equal t.props.row_styles styles) then (
    t.props <- { t.props with row_styles = styles };
    request t)

(* ───── Behavior Setters ───── *)

let set_wrap_selection t v =
  if t.props.wrap_selection <> v then (
    t.props <- { t.props with wrap_selection = v };
    request t)

let set_fast_scroll_step t n =
  let n = max 1 n in
  if t.props.fast_scroll_step <> n then (
    t.props <- { t.props with fast_scroll_step = n };
    request t)

(* ───── Callbacks ───── *)

let set_on_change t cb = t.on_change <- cb
let set_on_activate t cb = t.on_activate <- cb
let set_on_hover t cb = t.on_hover <- cb

(* ───── Key Handling ───── *)

let handle_key t (event : Event.key) =
  match Keymap.action t.keymap event with
  | None -> ()
  | Some action -> (
      Event.Key.prevent_default event;
      match action with
      | Move_up -> move_up t
      | Move_down -> move_down t
      | Move_up_fast -> move_up ~steps:t.props.fast_scroll_step t
      | Move_down_fast -> move_down ~steps:t.props.fast_scroll_step t
      | Page_up -> page_up t
      | Page_down -> page_down t
      | Activate ->
          if t.selected_row >= 0 then
            Option.iter (fun f -> f t.selected_row) t.on_activate)

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  (match Event.Mouse.kind event with
  | Out -> t.pointer <- None
  | _ -> t.pointer <- Some (Event.Mouse.x event, Event.Mouse.y event));
  match Event.Mouse.kind event with
  | Down { button = Left } -> (
      let row = row_under_pointer t in
      let selected_row = t.selected_row in
      let on_change = t.on_change in
      let on_activate =
        if t.props.activate_on_click then t.on_activate else None
      in
      (* Callbacks may replace rows and callbacks synchronously. Keep the
         original row and its mapping for the whole press. *)
      report_hover t row;
      match row with
      | None -> ()
      | Some index ->
          if not (Renderable.destroyed t.node) then begin
            if index <> selected_row then
              Option.iter (fun f -> f index) on_change;
            if not (Renderable.destroyed t.node) then
              Option.iter (fun f -> f index) on_activate
          end;
          Event.Mouse.stop_propagation event)
  | Move | Over _ | Out -> refresh_hover t
  | Scroll { direction; delta } when t.props.wheel_navigation -> (
      match direction with
      | Event.Mouse.Scroll_up when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Event.Mouse.Scroll_down when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | Down _ | Scroll _ | Up _ | Drag _ | Drag_end _ | Drop _ -> ()

(* ───── Rendering Helpers ───── *)

let draw_cell grid ~x ~y ~fg ~bg uch =
  Matrix_grid.set_uchar grid ~x ~y ~uchar:uch ~fg ~bg ~attrs:Ansi.Attr.empty ()

let draw_hline grid ~border ~x ~y ~width ~left_cap ~right_cap ~cross
    ~show_col_sep ~col_widths ~fg ~bg =
  let right_edge = x + width - 1 in
  let horiz = border.Matrix_grid.Border.horizontal in
  draw_cell grid ~x ~y ~fg ~bg left_cap;
  let cx = ref (x + 1) in
  let ncols = Array.length col_widths in
  for c = 0 to ncols - 1 do
    for _ = 1 to col_widths.(c) do
      if !cx < right_edge then (
        draw_cell grid ~x:!cx ~y ~fg ~bg horiz;
        incr cx)
    done;
    (* Always draw a junction/horizontal between adjacent columns *)
    if c < ncols - 1 && !cx < right_edge then (
      draw_cell grid ~x:!cx ~y ~fg ~bg (if show_col_sep then cross else horiz);
      incr cx)
  done;
  while !cx < right_edge do
    draw_cell grid ~x:!cx ~y ~fg ~bg horiz;
    incr cx
  done;
  draw_cell grid ~x:right_edge ~y ~fg ~bg right_cap

(* ───── Grapheme-Aware Text Truncation ───── *)

let apply_overflow ~width_method ~overflow text target_width =
  let tw = text_width ~width_method text in
  if tw <= target_width then text
  else
    match overflow with
    | `Ellipsis ->
        Text_crop.truncate_with_ellipsis ~width_method text target_width
    | `Crop -> Text_crop.crop_to_width ~width_method text target_width

(* ───── Text Drawing ───── *)

let draw_text_aligned ~width_method grid ~x ~y ~width ~alignment ~overflow
    ~style ~text =
  let clipped = apply_overflow ~width_method ~overflow text width in
  let tw = text_width ~width_method clipped in
  let offset =
    match alignment with
    | `Left -> 0
    | `Center -> max 0 ((width - tw) / 2)
    | `Right -> max 0 (width - tw)
  in
  Matrix_grid.draw_text ~style grid ~x:(x + offset) ~y ~text:clipped

let draw_cell_content ~width_method grid ~x ~y ~col_width ~padding ~alignment
    ~overflow ~default_style cell =
  let content_width = max 0 (col_width - (2 * padding)) in
  let content_x = x + padding in
  match cell with
  | Plain { text; style } ->
      let st =
        match style with
        | None -> default_style
        | Some overlay -> Ansi.Style.merge ~base:default_style ~overlay
      in
      draw_text_aligned ~width_method grid ~x:content_x ~y ~width:content_width
        ~alignment ~overflow ~style:st ~text
  | Rich fragments ->
      let plain = cell_plain_text (Rich fragments) in
      let tw = text_width ~width_method plain in
      let base_x =
        match alignment with
        | `Left -> content_x
        | `Center -> content_x + max 0 ((content_width - tw) / 2)
        | `Right -> content_x + max 0 (content_width - tw)
      in
      let right_bound = content_x + content_width in
      let cx = ref base_x in
      (* Mirror Text's collect_flat_spans: span styles merge into their
         children, and leaf styles overlay the inherited style. *)
      let merge_style base = function
        | None -> base
        | Some overlay -> Ansi.Style.merge ~base ~overlay
      in
      let rec draw_fragments ~base frags =
        List.iter
          (fun frag ->
            match frag with
            | Text.Text { text; style } ->
                let st = merge_style base style in
                let w = text_width ~width_method text in
                if !cx + w <= right_bound then (
                  Matrix_grid.draw_text ~style:st grid ~x:!cx ~y ~text;
                  cx := !cx + w)
                else
                  let avail = right_bound - !cx in
                  if avail > 0 then (
                    let truncated =
                      Text_crop.crop_to_width ~width_method text avail
                    in
                    Matrix_grid.draw_text ~style:st grid ~x:!cx ~y
                      ~text:truncated;
                    cx := right_bound)
            | Text.Span { children; style } ->
                draw_fragments ~base:(merge_style base style) children)
          frags
      in
      draw_fragments ~base:default_style fragments

(* ───── Rendering ───── *)

let first_some a b = match a with Some _ -> a | None -> b

let labelled color style =
  match color with Some c -> Ansi.Style.fg c style | None -> style

let render t _self grid ~dt:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let width_method = Renderable.Private.width_method t.node in
    let focused = Renderable.focused t.node in
    let border = t.props.border in
    let border_style = t.props.border_style in
    let show_header = t.props.show_header in
    let show_col_sep = t.props.show_column_separator in
    let show_row_sep = t.props.show_row_separator in
    let ncols = Array.length t.col_specs in
    (* Labels first, then roles: lines in the [Border] role, the header in
       [Emphasis], cells in [Plain] with each cell's style merged over its
       row, and the selected row on the [Cursor] surface while focused and
       the [Selected] surface otherwise. *)
    let theme = Renderable.theme t.node in
    let border_fg =
      Option.value (Theme.style theme Border).fg ~default:Ansi.Color.default_fg
    in
    let background =
      Option.value t.props.background ~default:Ansi.Color.transparent
    in
    let border_bg = background in

    (* Background fill *)
    Matrix_grid.clear_rect ~color:background grid ~x:0 ~y:0 ~width ~height;

    (* Compute layout positions *)
    let border_left = if border then 1 else 0 in
    let cur_y = ref 0 in

    refresh_hover t;
    let show_scroll_indicator = scroll_indicator_visible t in
    let table_width = width - if show_scroll_indicator then 1 else 0 in
    let col_widths =
      compute_column_widths t ~width_method ~available_width:table_width
    in

    (* Top border *)
    if border then (
      draw_hline grid ~border:border_style ~x:0 ~y:!cur_y ~width
        ~left_cap:border_style.top_left ~right_cap:border_style.top_right
        ~cross:border_style.top_t ~show_col_sep ~col_widths ~fg:border_fg
        ~bg:border_bg;
      incr cur_y);

    let pad = t.props.cell_padding in

    (* Header row *)
    if show_header && ncols > 0 then (
      Option.iter
        (fun color ->
          Matrix_grid.fill_rect grid ~x:border_left ~y:!cur_y
            ~width:(width - (2 * border_left))
            ~height:1 ~color)
        t.props.header_background;
      if border then
        draw_cell grid ~x:0 ~y:!cur_y ~fg:border_fg ~bg:border_bg
          border_style.vertical;
      let cx = ref border_left in
      for c = 0 to ncols - 1 do
        if c < Array.length col_widths then (
          let cw = col_widths.(c) in
          draw_text_aligned ~width_method grid ~x:(!cx + pad) ~y:!cur_y
            ~width:(max 0 (cw - (2 * pad)))
            ~alignment:t.col_specs.(c).alignment
            ~overflow:t.col_specs.(c).overflow
            ~style:(labelled t.props.header_color (Theme.style theme Emphasis))
            ~text:t.col_specs.(c).header;
          cx := !cx + cw;
          if c < ncols - 1 then (
            if show_col_sep then
              draw_cell grid ~x:!cx ~y:!cur_y ~fg:border_fg ~bg:border_bg
                border_style.vertical;
            incr cx))
      done;
      if border then
        draw_cell grid ~x:(width - 1) ~y:!cur_y ~fg:border_fg ~bg:border_bg
          border_style.vertical;
      incr cur_y;
      (* Header separator *)
      if border then (
        draw_hline grid ~border:border_style ~x:0 ~y:!cur_y ~width
          ~left_cap:border_style.left_t ~right_cap:border_style.right_t
          ~cross:border_style.cross ~show_col_sep ~col_widths ~fg:border_fg
          ~bg:border_bg;
        incr cur_y));

    (* Data rows *)
    let data_y = !cur_y in
    let start_index = scroll_offset t in
    let end_index = min (row_count t) (start_index + max_visible_rows t) in
    let default_text_style =
      labelled t.props.text_color (Theme.style theme Plain)
    in
    (* The selected row's style, made only for a frame that shows it. *)
    let selected_style () =
      let sel_bg =
        if focused then
          first_some t.props.focused_selected_background
            t.props.selected_background
        else t.props.selected_background
      in
      let sel_fg =
        if focused then
          first_some t.props.focused_selected_text_color
            t.props.selected_text_color
        else t.props.selected_text_color
      in
      let surface =
        match sel_bg with
        | Some bg -> Ansi.Style.make ~bg ()
        | None -> Theme.style theme (if focused then Cursor else Selected)
      in
      labelled sel_fg
        (Ansi.Style.merge ~base:surface ~overlay:default_text_style)
    in
    let row_styles = t.props.row_styles in
    let n_row_styles = List.length row_styles in
    for i = start_index to end_index - 1 do
      if !cur_y < height - if border then 1 else 0 then (
        let is_selected = t.props.selection_visible && i = t.selected_row in
        (* Alternating row style *)
        let alt_style =
          if n_row_styles > 0 then
            Some (List.nth row_styles (i mod n_row_styles))
          else None
        in
        (* Row background: selection takes priority, then alternating style *)
        let selected_style =
          if is_selected then selected_style () else default_text_style
        in
        (if is_selected then
           Matrix_grid.draw_text ~style:selected_style grid ~x:border_left
             ~y:!cur_y
             ~text:(String.make (max 0 (width - (2 * border_left))) ' ')
         else
           match alt_style with
           | Some s when Option.is_some s.Ansi.Style.bg ->
               Matrix_grid.fill_rect grid ~x:border_left ~y:!cur_y
                 ~width:(width - (2 * border_left))
                 ~height:1
                 ~color:(Option.get s.Ansi.Style.bg)
           | _ -> ());
        (* Left border *)
        if border then
          draw_cell grid ~x:0 ~y:!cur_y ~fg:border_fg ~bg:border_bg
            border_style.vertical;
        (* Draw cells *)
        let row = t.data_rows.(i) in
        let cx = ref border_left in
        for c = 0 to ncols - 1 do
          if c < Array.length col_widths then (
            let cw = col_widths.(c) in
            let cell_content =
              if c < Array.length row then row.(c)
              else Plain { text = ""; style = None }
            in
            let row_style =
              if is_selected then selected_style
              else
                match alt_style with
                | Some s -> Ansi.Style.merge ~base:default_text_style ~overlay:s
                | None -> default_text_style
            in
            draw_cell_content ~width_method grid ~x:!cx ~y:!cur_y ~col_width:cw
              ~padding:pad ~alignment:t.col_specs.(c).alignment
              ~overflow:t.col_specs.(c).overflow ~default_style:row_style
              cell_content;
            cx := !cx + cw;
            if c < ncols - 1 then (
              if show_col_sep then
                draw_cell grid ~x:!cx ~y:!cur_y ~fg:border_fg ~bg:border_bg
                  border_style.vertical;
              incr cx))
        done;
        (* Right border *)
        if border then
          draw_cell grid ~x:(width - 1) ~y:!cur_y ~fg:border_fg ~bg:border_bg
            border_style.vertical;
        incr cur_y;
        (* Row separator *)
        if show_row_sep && i < end_index - 1 then
          if !cur_y < height - if border then 1 else 0 then (
            let left_cap =
              if border then border_style.left_t else border_style.horizontal
            in
            let right_cap =
              if border then border_style.right_t else border_style.horizontal
            in
            draw_hline grid ~border:border_style ~x:0 ~y:!cur_y ~width ~left_cap
              ~right_cap ~cross:border_style.cross ~show_col_sep ~col_widths
              ~fg:border_fg ~bg:border_bg;
            incr cur_y))
    done;

    (* The indicator is derived from the table's actual viewport. It therefore
       stays honest when row separators, borders, headers, or layout height
       change, without callers predicting how many rows fit. *)
    (if show_scroll_indicator then
       let visible_rows = end_index - start_index in
       let indicator_x = width - if border then 2 else 1 in
       if indicator_x >= border_left then
         let row_height = if show_row_sep then 2 else 1 in
         let hidden_above = start_index > 0 in
         let hidden_below = end_index < row_count t in
         let style = default_text_style in
         for offset = 0 to visible_rows - 1 do
           let text =
             if visible_rows = 1 && hidden_above && hidden_below then "↕"
             else if offset = 0 && hidden_above then "↑"
             else if offset = visible_rows - 1 && hidden_below then "↓"
             else "│"
           in
           Matrix_grid.draw_text ~style grid ~x:indicator_x
             ~y:(data_y + (offset * row_height))
             ~text
         done);

    (* Bottom border *)
    if border then
      if !cur_y < height then
        draw_hline grid ~border:border_style ~x:0 ~y:(height - 1) ~width
          ~left_cap:border_style.bottom_left
          ~right_cap:border_style.bottom_right ~cross:border_style.bottom_t
          ~show_col_sep ~col_widths ~fg:border_fg ~bg:border_bg

(* ───── Resize ───── *)

let on_resize t _node =
  refresh_hover t;
  request t

(* ───── Measure ───── *)

let intrinsic_width t =
  let ncols = Array.length t.col_specs in
  if ncols = 0 then 0
  else
    let width_method = Renderable.Private.width_method t.node in
    let auto_widths = auto_content_widths t ~width_method in
    let pad = t.props.cell_padding in
    let pad2 = 2 * pad in
    let gap_width = if ncols > 1 then ncols - 1 else 0 in
    let border_width = if t.props.border then 2 else 0 in
    let col_total = ref 0 in
    Array.iteri
      (fun i col ->
        let w =
          match col.width with
          | `Fixed n -> max 1 (n + pad2)
          | `Auto ->
              let content_w = auto_widths.(i) in
              let w = max 1 (content_w + pad2) in
              let w =
                match col.min_width with
                | Some m -> max w (m + pad2)
                | None -> w
              in
              let w =
                match col.max_width with
                | Some m -> min w (m + pad2)
                | None -> w
              in
              max 1 w
          | `Flex _ -> max 1 (pad2 + 1)
        in
        col_total := !col_total + w)
      t.col_specs;
    !col_total + gap_width + border_width

let intrinsic_height t =
  let nrows = Array.length t.data_rows in
  let border_v = if t.props.border then 2 else 0 in
  let header_rows =
    if t.props.show_header then 1 + if t.props.border then 1 else 0 else 0
  in
  let row_sep =
    if t.props.show_row_separator && nrows > 1 then nrows - 1 else 0
  in
  border_v + header_rows + nrows + row_sep

let measure t ~known_dimensions ~available_space:_ ~style:_ =
  Toffee.Geometry.Size.
    {
      width =
        (match known_dimensions.width with
        | Some w -> w
        | None -> Float.of_int (intrinsic_width t));
      height =
        (match known_dimensions.height with
        | Some h -> h
        | None -> Float.of_int (intrinsic_height t));
    }

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    (props : Props.t) =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let col_specs = Array.of_list props.columns in
  let data_rows = Items.to_array props.rows in
  let initial_selected =
    let len = Array.length data_rows in
    if len = 0 then -1 else max (-1) (min (len - 1) props.selected_row)
  in
  let t =
    {
      node;
      props;
      keymap = keymap props.key_bindings;
      col_specs;
      data_rows;
      selected_row = initial_selected;
      pointer = None;
      hovered = None;
      auto_widths = None;
      on_change = None;
      on_activate = None;
      on_hover = None;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_buffered node true;
  Renderable.set_focusable node true;
  Renderable.set_key_behavior node (Some (handle_key t));
  Renderable.on_mouse node (handle_mouse t);
  Renderable.set_on_resize node (Some (on_resize t));
  request t;
  t

(* ───── Layout ───── *)

let set_style t style = Renderable.set_style t.node style

(* ───── Apply Props ───── *)

(* Controlled application: move the selection without echoing [on_change] —
   the value came from the app, not from a user gesture. *)
let apply_props t (props : Props.t) =
  if
    not
      (List.equal
         (Keymap.binding_equal ( = ))
         t.props.key_bindings props.key_bindings)
  then begin
    t.keymap <- keymap props.key_bindings;
    t.props <- { t.props with key_bindings = props.key_bindings }
  end;
  if not (columns_equal (Array.to_list t.col_specs) props.columns) then
    set_columns t props.columns;
  (* Rows equal to the current ones are kept, with the new source, which a
     later declaration of the same list is then the same as. *)
  if not (Items.same t.props.rows props.rows) then
    if Items.equal row_equal t.props.rows props.rows then
      t.props <- { t.props with rows = props.rows }
    else set_source t props.rows;
  set_selected_row t props.selected_row;
  set_border t props.border;
  set_border_style t props.border_style;
  set_show_header t props.show_header;
  set_show_column_separator t props.show_column_separator;
  set_show_row_separator t props.show_row_separator;
  set_cell_padding t props.cell_padding;
  let same a b = Option.equal Ansi.Color.equal a b in
  if
    not
      (same t.props.header_color props.header_color
      && same t.props.header_background props.header_background
      && same t.props.text_color props.text_color
      && same t.props.background props.background
      && same t.props.selected_text_color props.selected_text_color
      && same t.props.selected_background props.selected_background
      && same t.props.focused_selected_text_color
           props.focused_selected_text_color
      && same t.props.focused_selected_background
           props.focused_selected_background)
  then begin
    t.props <-
      {
        t.props with
        header_color = props.header_color;
        header_background = props.header_background;
        text_color = props.text_color;
        background = props.background;
        selected_text_color = props.selected_text_color;
        selected_background = props.selected_background;
        focused_selected_text_color = props.focused_selected_text_color;
        focused_selected_background = props.focused_selected_background;
      };
    request t
  end;
  set_selection_visible t props.selection_visible;
  set_show_scroll_indicator t props.show_scroll_indicator;
  set_activate_on_click t props.activate_on_click;
  set_wheel_navigation t props.wheel_navigation;
  set_row_styles t props.row_styles;
  set_wrap_selection t props.wrap_selection;
  set_fast_scroll_step t props.fast_scroll_step

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Table(%s, %d/%d" (Renderable.id t.node) t.selected_row
    (row_count t);
  if t.props.border then Format.pp_print_string ppf ", border";
  if t.props.wrap_selection then Format.pp_print_string ppf ", wrap";
  Format.pp_print_char ppf ')'
