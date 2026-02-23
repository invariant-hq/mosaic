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

let rec rows_equal xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> row_equal x y && rows_equal xs ys
  | _ -> false

(* ───── Props ───── *)

module Props = struct
  type t = {
    columns : column list;
    rows : cell array list;
    selected_row : int;
    border : bool;
    border_style : Grid.Border.t;
    show_header : bool;
    show_column_separator : bool;
    show_row_separator : bool;
    cell_padding : int;
    header_color : Ansi.Color.t;
    header_background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    background : Ansi.Color.t;
    selected_text_color : Ansi.Color.t;
    selected_background : Ansi.Color.t;
    focused_selected_text_color : Ansi.Color.t;
    focused_selected_background : Ansi.Color.t;
    row_styles : Ansi.Style.t list;
    wrap_selection : bool;
    fast_scroll_step : int;
  }

  let make ?(columns = []) ?(rows = []) ?(selected_row = 0) ?(border = true)
      ?(border_style = Grid.Border.single) ?(show_header = true)
      ?(show_column_separator = false) ?(show_row_separator = false)
      ?(cell_padding = 0) ?(header_color = Ansi.Color.of_rgb 255 255 255)
      ?(header_background = Ansi.Color.of_rgb 51 51 51)
      ?(text_color = Ansi.Color.of_rgb 255 255 255)
      ?(background = Ansi.Color.of_rgba 0 0 0 0)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?focused_selected_text_color ?focused_selected_background
      ?(row_styles = []) ?(wrap_selection = false) ?(fast_scroll_step = 5) () =
    let focused_selected_text_color =
      match focused_selected_text_color with
      | Some c -> c
      | None -> selected_text_color
    in
    let focused_selected_background =
      match focused_selected_background with
      | Some c -> c
      | None -> selected_background
    in
    {
      columns;
      rows;
      selected_row = max 0 selected_row;
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
    columns_equal a.columns b.columns
    && rows_equal a.rows b.rows
    && Int.equal a.selected_row b.selected_row
    && Bool.equal a.border b.border
    && a.border_style = b.border_style
    && Bool.equal a.show_header b.show_header
    && Bool.equal a.show_column_separator b.show_column_separator
    && Bool.equal a.show_row_separator b.show_row_separator
    && Int.equal a.cell_padding b.cell_padding
    && Ansi.Color.equal a.header_color b.header_color
    && Ansi.Color.equal a.header_background b.header_background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Ansi.Color.equal a.selected_background b.selected_background
    && Ansi.Color.equal a.focused_selected_text_color
         b.focused_selected_text_color
    && Ansi.Color.equal a.focused_selected_background
         b.focused_selected_background
    && styles_equal a.row_styles b.row_styles
    && Bool.equal a.wrap_selection b.wrap_selection
    && Int.equal a.fast_scroll_step b.fast_scroll_step
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable col_specs : column array;
  mutable data_rows : cell array array;
  mutable selected_row : int;
  mutable scroll_offset : int;
  mutable max_visible_rows : int;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
}

let node t = t.node

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let row_count t = Array.length t.data_rows

let clamp_index t idx =
  let len = row_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

(* ───── Column Width Computation ───── *)

let text_width s = Glyph.String.measure ~width_method:`Unicode ~tab_width:2 s

let compute_column_widths t ~available_width =
  let ncols = Array.length t.col_specs in
  if ncols = 0 then [||]
  else
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
            let header_w = text_width col.header in
            let max_cell_w =
              Array.fold_left
                (fun acc row ->
                  if i < Array.length row then
                    max acc (text_width (cell_plain_text row.(i)))
                  else acc)
                0 t.data_rows
            in
            let content_w = max header_w max_cell_w in
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

let recalc_max_visible t ~content_height =
  let row_height = if t.props.show_row_separator then 2 else 1 in
  t.max_visible_rows <- max 1 ((content_height + row_height - 1) / row_height)

let update_scroll_offset t =
  let len = row_count t in
  if len = 0 then t.scroll_offset <- 0
  else
    let half = max 0 (t.max_visible_rows / 2) in
    let max_off = max 0 (len - t.max_visible_rows) in
    let desired = t.selected_row - half in
    t.scroll_offset <- max 0 (min desired max_off)

let set_selected_row_internal t idx =
  let len = row_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_row then (
      t.selected_row <- idx;
      update_scroll_offset t;
      (match t.on_change with None -> () | Some f -> f t.selected_row);
      request t)

(* ───── Public Accessors ───── *)

let columns t = Array.to_list t.col_specs
let rows t = Array.to_list t.data_rows
let selected_row t = t.selected_row

(* ───── Data ───── *)

let set_columns t cols =
  t.col_specs <- Array.of_list cols;
  Renderable.mark_dirty t.node;
  request t

let set_rows t data =
  t.data_rows <- Array.of_list data;
  t.selected_row <- clamp_index t t.selected_row;
  update_scroll_offset t;
  Renderable.mark_dirty t.node;
  request t

let set_selected_row t idx = set_selected_row_internal t idx

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = row_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_row - steps in
    if new_index >= 0 then set_selected_row_internal t new_index
    else if t.props.wrap_selection then set_selected_row_internal t (len - 1)
    else set_selected_row_internal t 0

let move_down ?(steps = 1) t =
  let len = row_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_row + steps in
    if new_index < len then set_selected_row_internal t new_index
    else if t.props.wrap_selection then set_selected_row_internal t 0
    else set_selected_row_internal t (len - 1)

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
  if not (Ansi.Color.equal t.props.header_color c) then (
    t.props <- { t.props with header_color = c };
    request t)

let set_header_background t c =
  if not (Ansi.Color.equal t.props.header_background c) then (
    t.props <- { t.props with header_background = c };
    request t)

let set_text_color t c =
  if not (Ansi.Color.equal t.props.text_color c) then (
    t.props <- { t.props with text_color = c };
    request t)

let set_background t c =
  if not (Ansi.Color.equal t.props.background c) then (
    t.props <- { t.props with background = c };
    request t)

let set_selected_text_color t c =
  if not (Ansi.Color.equal t.props.selected_text_color c) then (
    t.props <- { t.props with selected_text_color = c };
    request t)

let set_selected_background t c =
  if not (Ansi.Color.equal t.props.selected_background c) then (
    t.props <- { t.props with selected_background = c };
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

(* ───── Key Handling ───── *)

let handle_key t (event : Event.key) =
  let kev = Event.Key.data event in
  let shift = kev.modifier.shift in
  let consumed =
    match kev.key with
    | Up ->
        move_up ~steps:(if shift then t.props.fast_scroll_step else 1) t;
        true
    | Down ->
        move_down ~steps:(if shift then t.props.fast_scroll_step else 1) t;
        true
    | Char c when Uchar.equal c (Uchar.of_char 'k') ->
        move_up t;
        true
    | Char c when Uchar.equal c (Uchar.of_char 'j') ->
        move_down t;
        true
    | Enter | KP_enter ->
        (if row_count t > 0 then
           match t.on_activate with None -> () | Some f -> f t.selected_row);
        true
    | _ -> false
  in
  if consumed then Event.Key.prevent_default event

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  let x = Event.Mouse.x event - Renderable.x t.node in
  let y = Event.Mouse.y event - Renderable.y t.node in
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      if x >= 0 && x < width && y >= 0 && y < height then
        (* Compute the y offset where data rows start *)
        let border_top = if t.props.border then 1 else 0 in
        let header_rows =
          if t.props.show_header then 1 + if t.props.border then 1 else 0 else 0
        in
        let data_start = border_top + header_rows in
        let row_y = y - data_start in
        if row_y >= 0 then
          let row_height = if t.props.show_row_separator then 2 else 1 in
          let index = t.scroll_offset + (row_y / row_height) in
          if index < row_count t then (
            set_selected_row_internal t index;
            Event.Mouse.stop_propagation event)
  | Scroll { direction; delta } -> (
      match direction with
      | Input.Mouse.Scroll_up when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Input.Mouse.Scroll_down when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | _ -> ()

(* ───── Rendering Helpers ───── *)

let draw_glyph grid ~x ~y ~fg ~bg uch =
  let glyph = Glyph.of_uchar uch in
  Grid.set_cell grid ~x ~y ~glyph ~fg ~bg ~attrs:Ansi.Attr.empty ()

let draw_hline grid ~border ~x ~y ~width ~left_cap ~right_cap ~cross
    ~show_col_sep ~col_widths ~fg ~bg =
  let right_edge = x + width - 1 in
  let horiz = border.Grid.Border.horizontal in
  draw_glyph grid ~x ~y ~fg ~bg left_cap;
  let cx = ref (x + 1) in
  let ncols = Array.length col_widths in
  for c = 0 to ncols - 1 do
    for _ = 1 to col_widths.(c) do
      if !cx < right_edge then (
        draw_glyph grid ~x:!cx ~y ~fg ~bg horiz;
        incr cx)
    done;
    (* Always draw a junction/horizontal between adjacent columns *)
    if c < ncols - 1 && !cx < right_edge then (
      draw_glyph grid ~x:!cx ~y ~fg ~bg (if show_col_sep then cross else horiz);
      incr cx)
  done;
  while !cx < right_edge do
    draw_glyph grid ~x:!cx ~y ~fg ~bg horiz;
    incr cx
  done;
  draw_glyph grid ~x:right_edge ~y ~fg ~bg right_cap

(* ───── Grapheme-Aware Text Truncation ───── *)

let crop_to_width text target_width =
  if target_width <= 0 then ""
  else
    let result = Buffer.create (String.length text) in
    let current_width = ref 0 in
    let stop = ref false in
    Glyph.String.iter_graphemes
      (fun ~offset ~len ->
        if not !stop then
          let g = String.sub text offset len in
          let gw = Glyph.String.measure ~width_method:`Unicode ~tab_width:2 g in
          if !current_width + gw <= target_width then (
            Buffer.add_string result g;
            current_width := !current_width + gw)
          else stop := true)
      text;
    Buffer.contents result

let truncate_with_ellipsis text target_width =
  let tw = text_width text in
  if tw <= target_width then text
  else if target_width <= 3 then crop_to_width text target_width
  else
    let prefix = crop_to_width text (target_width - 3) in
    prefix ^ "..."

let apply_overflow ~overflow text target_width =
  let tw = text_width text in
  if tw <= target_width then text
  else
    match overflow with
    | `Ellipsis -> truncate_with_ellipsis text target_width
    | `Crop -> crop_to_width text target_width

(* ───── Text Drawing ───── *)

let draw_text_aligned grid ~x ~y ~width ~alignment ~overflow ~style ~text =
  let clipped = apply_overflow ~overflow text width in
  let tw = text_width clipped in
  let offset =
    match alignment with
    | `Left -> 0
    | `Center -> max 0 ((width - tw) / 2)
    | `Right -> max 0 (width - tw)
  in
  Grid.draw_text ~style grid ~x:(x + offset) ~y ~text:clipped

let draw_cell_content grid ~x ~y ~col_width ~padding ~alignment ~overflow
    ~default_style cell =
  let content_width = max 0 (col_width - (2 * padding)) in
  let content_x = x + padding in
  match cell with
  | Plain { text; style } ->
      let st = Option.value style ~default:default_style in
      draw_text_aligned grid ~x:content_x ~y ~width:content_width ~alignment
        ~overflow ~style:st ~text
  | Rich fragments ->
      let plain = cell_plain_text (Rich fragments) in
      let tw = text_width plain in
      let base_x =
        match alignment with
        | `Left -> content_x
        | `Center -> content_x + max 0 ((content_width - tw) / 2)
        | `Right -> content_x + max 0 (content_width - tw)
      in
      let right_bound = content_x + content_width in
      let cx = ref base_x in
      let rec draw_fragments frags =
        List.iter
          (fun frag ->
            match frag with
            | Text.Text { text; style } ->
                let st = Option.value style ~default:default_style in
                let w = text_width text in
                if !cx + w <= right_bound then (
                  Grid.draw_text ~style:st grid ~x:!cx ~y ~text;
                  cx := !cx + w)
                else
                  let avail = right_bound - !cx in
                  if avail > 0 then (
                    let truncated = crop_to_width text avail in
                    Grid.draw_text ~style:st grid ~x:!cx ~y ~text:truncated;
                    cx := right_bound)
            | Text.Span { children; style } ->
                let _parent_style = style in
                draw_fragments children)
          frags
      in
      draw_fragments fragments

(* ───── Rendering ───── *)

let render t _self grid ~delta:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in
    let border = t.props.border in
    let border_style = t.props.border_style in
    let show_header = t.props.show_header in
    let show_col_sep = t.props.show_column_separator in
    let show_row_sep = t.props.show_row_separator in
    let ncols = Array.length t.col_specs in
    let col_widths = compute_column_widths t ~available_width:width in
    let border_fg = Ansi.Color.of_rgb 229 229 229 in
    let border_bg = t.props.background in

    (* Background fill *)
    Grid.fill_rect grid ~x:0 ~y:0 ~width ~height ~color:t.props.background;

    (* Compute layout positions *)
    let border_left = if border then 1 else 0 in
    let cur_y = ref 0 in

    (* Recompute scroll metrics *)
    let header_total = if show_header then 1 + if border then 1 else 0 else 0 in
    let border_v = if border then 2 else 0 in
    let content_height = height - header_total - border_v in
    recalc_max_visible t ~content_height;
    update_scroll_offset t;

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
      Grid.fill_rect grid ~x:border_left ~y:!cur_y
        ~width:(width - (2 * border_left))
        ~height:1 ~color:t.props.header_background;
      if border then
        draw_glyph grid ~x:0 ~y:!cur_y ~fg:border_fg ~bg:border_bg
          border_style.vertical;
      let cx = ref border_left in
      for c = 0 to ncols - 1 do
        if c < Array.length col_widths then (
          let cw = col_widths.(c) in
          draw_text_aligned grid ~x:(!cx + pad) ~y:!cur_y
            ~width:(max 0 (cw - (2 * pad)))
            ~alignment:t.col_specs.(c).alignment
            ~overflow:t.col_specs.(c).overflow
            ~style:(Ansi.Style.make ~fg:t.props.header_color ~bold:true ())
            ~text:t.col_specs.(c).header;
          cx := !cx + cw;
          if c < ncols - 1 then (
            if show_col_sep then
              draw_glyph grid ~x:!cx ~y:!cur_y ~fg:border_fg ~bg:border_bg
                border_style.vertical;
            incr cx))
      done;
      if border then
        draw_glyph grid ~x:(width - 1) ~y:!cur_y ~fg:border_fg ~bg:border_bg
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
    let start_index = t.scroll_offset in
    let end_index = min (row_count t) (start_index + t.max_visible_rows) in
    let default_text_style = Ansi.Style.make ~fg:t.props.text_color () in
    let row_styles = t.props.row_styles in
    let n_row_styles = List.length row_styles in
    for i = start_index to end_index - 1 do
      if !cur_y < height - if border then 1 else 0 then (
        let is_selected = i = t.selected_row in
        let sel_bg =
          if focused then t.props.focused_selected_background
          else t.props.selected_background
        in
        let sel_fg =
          if focused then t.props.focused_selected_text_color
          else t.props.selected_text_color
        in
        (* Alternating row style *)
        let alt_style =
          if n_row_styles > 0 then
            Some (List.nth row_styles (i mod n_row_styles))
          else None
        in
        (* Row background: selection takes priority, then alternating style *)
        (if is_selected then
           Grid.fill_rect grid ~x:border_left ~y:!cur_y
             ~width:(width - (2 * border_left))
             ~height:1 ~color:sel_bg
         else
           match alt_style with
           | Some s when Option.is_some s.Ansi.Style.bg ->
               Grid.fill_rect grid ~x:border_left ~y:!cur_y
                 ~width:(width - (2 * border_left))
                 ~height:1
                 ~color:(Option.get s.Ansi.Style.bg)
           | _ -> ());
        (* Left border *)
        if border then
          draw_glyph grid ~x:0 ~y:!cur_y ~fg:border_fg ~bg:border_bg
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
              if is_selected then Ansi.Style.make ~fg:sel_fg ()
              else
                match alt_style with
                | Some s -> Ansi.Style.merge ~base:default_text_style ~overlay:s
                | None -> default_text_style
            in
            draw_cell_content grid ~x:!cx ~y:!cur_y ~col_width:cw ~padding:pad
              ~alignment:t.col_specs.(c).alignment
              ~overflow:t.col_specs.(c).overflow ~default_style:row_style
              cell_content;
            cx := !cx + cw;
            if c < ncols - 1 then (
              if show_col_sep then
                draw_glyph grid ~x:!cx ~y:!cur_y ~fg:border_fg ~bg:border_bg
                  border_style.vertical;
              incr cx))
        done;
        (* Right border *)
        if border then
          draw_glyph grid ~x:(width - 1) ~y:!cur_y ~fg:border_fg ~bg:border_bg
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

    (* Bottom border *)
    if border then
      if !cur_y < height then
        draw_hline grid ~border:border_style ~x:0 ~y:(height - 1) ~width
          ~left_cap:border_style.bottom_left
          ~right_cap:border_style.bottom_right ~cross:border_style.bottom_t
          ~show_col_sep ~col_widths ~fg:border_fg ~bg:border_bg

(* ───── Resize ───── *)

let on_resize t _node =
  let h = Renderable.height t.node in
  let border_v = if t.props.border then 2 else 0 in
  let header_total =
    if t.props.show_header then 1 + if t.props.border then 1 else 0 else 0
  in
  let content_height = h - header_total - border_v in
  recalc_max_visible t ~content_height;
  update_scroll_offset t;
  request t

(* ───── Measure ───── *)

let intrinsic_width t =
  let ncols = Array.length t.col_specs in
  if ncols = 0 then 0
  else
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
              let header_w = text_width col.header in
              let max_cell_w =
                Array.fold_left
                  (fun best row ->
                    if i < Array.length row then
                      max best (text_width (cell_plain_text row.(i)))
                    else best)
                  0 t.data_rows
              in
              let content_w = max header_w max_cell_w in
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

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?columns ?rows
    ?selected_row ?border ?border_style ?show_header ?show_column_separator
    ?show_row_separator ?cell_padding ?header_color ?header_background
    ?text_color ?background ?selected_text_color ?selected_background
    ?focused_selected_text_color ?focused_selected_background ?row_styles
    ?wrap_selection ?fast_scroll_step () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?columns ?rows ?selected_row ?border ?border_style ?show_header
      ?show_column_separator ?show_row_separator ?cell_padding ?header_color
      ?header_background ?text_color ?background ?selected_text_color
      ?selected_background ?focused_selected_text_color
      ?focused_selected_background ?row_styles ?wrap_selection ?fast_scroll_step
      ()
  in
  let col_specs = Array.of_list props.columns in
  let data_rows = Array.of_list props.rows in
  let initial_selected =
    let len = Array.length data_rows in
    if len = 0 then 0 else max 0 (min (len - 1) props.selected_row)
  in
  let t =
    {
      node;
      props;
      col_specs;
      data_rows;
      selected_row = initial_selected;
      scroll_offset = 0;
      max_visible_rows = 1;
      on_change = None;
      on_activate = None;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_buffered node true;
  Renderable.set_focusable node true;
  Renderable.on_key node (handle_key t);
  Renderable.on_mouse node (handle_mouse t);
  Renderable.set_on_resize node (Some (on_resize t));
  request t;
  t

(* ───── Layout ───── *)

let set_style t style = Renderable.set_style t.node style

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  if not (columns_equal (Array.to_list t.col_specs) props.columns) then
    set_columns t props.columns;
  if not (rows_equal (Array.to_list t.data_rows) props.rows) then
    set_rows t props.rows;
  if props.selected_row <> t.selected_row then
    set_selected_row t props.selected_row;
  set_border t props.border;
  set_border_style t props.border_style;
  set_show_header t props.show_header;
  set_show_column_separator t props.show_column_separator;
  set_show_row_separator t props.show_row_separator;
  set_cell_padding t props.cell_padding;
  set_header_color t props.header_color;
  set_header_background t props.header_background;
  set_text_color t props.text_color;
  set_background t props.background;
  set_selected_text_color t props.selected_text_color;
  set_selected_background t props.selected_background;
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
