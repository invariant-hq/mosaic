module Cell = struct
  type t = { text : string; style : Ansi.Style.t option }

  let empty = { text = ""; style = None }
  let make ?style text = { text; style }
end

type cell = Cell.t
type row = { cells : Cell.t list; style : Ansi.Style.t option }

let cell = Cell.make
let row ?style cells = { cells; style }

type justify = [ `Left | `Center | `Right | `Full ]
type vertical_align = [ `Top | `Middle | `Bottom ]
type overflow = [ `Ellipsis | `Crop | `Fold ]
type padding = int * int * int * int

type box_style =
  | No_box
  | Simple
  | Rounded
  | Heavy
  | Heavy_head
  | Double
  | Double_edge
  | Ascii
  | Minimal_heavy_head
  | Minimal_double_head
  | Minimal
  | Square
  | Square_double_head

type column = {
  header : Cell.t option;
  footer : Cell.t option;
  header_style : Ansi.Style.t option;
  footer_style : Ansi.Style.t option;
  style : Ansi.Style.t option;
  justify : justify;
  vertical : vertical_align;
  overflow : overflow;
  width : int option;
  min_width : int option;
  max_width : int option;
  ratio : int option;
  no_wrap : bool;
}

let column ?header ?footer ?style ?header_style ?footer_style ?(justify = `Left)
    ?(vertical = `Top) ?(overflow = `Ellipsis) ?width ?min_width ?max_width
    ?ratio ?(no_wrap = false) _name =
  let width =
    match width with Some (`Fixed w) -> Some w | Some `Auto | None -> None
  in
  {
    header;
    footer;
    header_style;
    footer_style;
    style;
    justify;
    vertical;
    overflow;
    width;
    min_width;
    max_width;
    ratio;
    no_wrap;
  }

type options = {
  columns : column list;
  rows : row list;
  title : Cell.t option;
  caption : Cell.t option;
  box_style : box_style;
  safe_box : bool;
  padding : padding;
  collapse_padding : bool;
  pad_edge : bool;
  expand : bool;
  show_header : bool;
  show_footer : bool;
  show_edge : bool;
  show_lines : bool;
  leading : int;
  cell_style : Ansi.Style.t;
  row_styles : Ansi.Style.t list;
  header_style : Ansi.Style.t;
  footer_style : Ansi.Style.t;
  border_style : Ansi.Style.t;
  title_style : Ansi.Style.t;
  caption_style : Ansi.Style.t;
  title_justify : justify;
  caption_justify : justify;
  width : int option;
  min_width : int option;
}

type styled_line = (string * Ansi.Style.t) list
type render_plan = styled_line list

type t = {
  surface : Text_surface.t;
  mutable columns : column list;
  mutable rows : row list;
  mutable options : options;
}

let node t = Text_surface.node t.surface

let width_of_string text =
  Glyph.String.measure ~width_method:`Unicode ~tab_width:2 text

let merge_style base overlay =
  match overlay with
  | None -> base
  | Some overlay -> Ansi.Style.merge ~base ~overlay

let merge_styles base overlays =
  List.fold_left (fun acc style -> merge_style acc style) base overlays

let default_options =
  {
    columns = [];
    rows = [];
    title = None;
    caption = None;
    box_style = Heavy_head;
    safe_box = false;
    padding = (0, 1, 0, 1);
    collapse_padding = false;
    pad_edge = true;
    expand = false;
    show_header = true;
    show_footer = false;
    show_edge = true;
    show_lines = false;
    leading = 0;
    cell_style = Ansi.Style.default;
    row_styles = [];
    header_style = Ansi.Style.make ~bold:true ();
    footer_style = Ansi.Style.default;
    border_style = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 200 200 200) ();
    title_style = Ansi.Style.default;
    caption_style = Ansi.Style.default;
    title_justify = `Center;
    caption_justify = `Center;
    width = None;
    min_width = None;
  }

module Props = struct
  type t = options

  let make ?box_style ?safe_box ?padding ?collapse_padding ?pad_edge ?expand
      ?show_header ?show_footer ?show_edge ?show_lines ?leading ?cell_style
      ?row_styles ?header_style ?footer_style ?border_style ?title ?title_style
      ?title_justify ?caption ?caption_style ?caption_justify ?width ?min_width
      ~columns ~rows () =
    {
      columns;
      rows;
      title;
      caption;
      box_style = Option.value box_style ~default:default_options.box_style;
      safe_box = Option.value safe_box ~default:default_options.safe_box;
      padding = Option.value padding ~default:default_options.padding;
      collapse_padding =
        Option.value collapse_padding ~default:default_options.collapse_padding;
      pad_edge = Option.value pad_edge ~default:default_options.pad_edge;
      expand = Option.value expand ~default:default_options.expand;
      show_header =
        Option.value show_header ~default:default_options.show_header;
      show_footer =
        Option.value show_footer ~default:default_options.show_footer;
      show_edge = Option.value show_edge ~default:default_options.show_edge;
      show_lines = Option.value show_lines ~default:default_options.show_lines;
      leading = Option.value leading ~default:default_options.leading;
      cell_style = Option.value cell_style ~default:default_options.cell_style;
      row_styles = Option.value row_styles ~default:default_options.row_styles;
      header_style =
        Option.value header_style ~default:default_options.header_style;
      footer_style =
        Option.value footer_style ~default:default_options.footer_style;
      border_style =
        Option.value border_style ~default:default_options.border_style;
      title_style =
        Option.value title_style ~default:default_options.title_style;
      caption_style =
        Option.value caption_style ~default:default_options.caption_style;
      title_justify =
        Option.value title_justify ~default:default_options.title_justify;
      caption_justify =
        Option.value caption_justify ~default:default_options.caption_justify;
      width;
      min_width;
    }

  let equal a b =
    let rec list_eq eq xs ys =
      match (xs, ys) with
      | [], [] -> true
      | x :: xs, y :: ys -> eq x y && list_eq eq xs ys
      | _ -> false
    in
    Option.equal
      (fun (x : Cell.t) (y : Cell.t) ->
        String.equal x.text y.text
        && Option.equal Ansi.Style.equal x.style y.style)
      a.title b.title
    && Option.equal
         (fun (x : Cell.t) (y : Cell.t) ->
           String.equal x.text y.text
           && Option.equal Ansi.Style.equal x.style y.style)
         a.caption b.caption
    && a.box_style = b.box_style
    && Bool.equal a.safe_box b.safe_box
    && a.padding = b.padding
    && Bool.equal a.collapse_padding b.collapse_padding
    && Bool.equal a.pad_edge b.pad_edge
    && Bool.equal a.expand b.expand
    && Bool.equal a.show_header b.show_header
    && Bool.equal a.show_footer b.show_footer
    && Bool.equal a.show_edge b.show_edge
    && Bool.equal a.show_lines b.show_lines
    && Int.equal a.leading b.leading
    && Ansi.Style.equal a.cell_style b.cell_style
    && list_eq Ansi.Style.equal a.row_styles b.row_styles
    && Ansi.Style.equal a.header_style b.header_style
    && Ansi.Style.equal a.footer_style b.footer_style
    && Ansi.Style.equal a.border_style b.border_style
    && Ansi.Style.equal a.title_style b.title_style
    && Ansi.Style.equal a.caption_style b.caption_style
    && a.title_justify = b.title_justify
    && a.caption_justify = b.caption_justify
    && Option.equal Int.equal a.width b.width
    && Option.equal Int.equal a.min_width b.min_width
end

type box_chars = {
  head_left : string;
  head_right : string;
  head_vertical : string;
  head_horizontal : string;
  row_left : string;
  row_right : string;
  row_vertical : string;
  row_horizontal : string;
  mid_left : string;
  mid_right : string;
  mid_vertical : string;
  mid_horizontal : string;
  foot_left : string;
  foot_right : string;
  foot_vertical : string;
  foot_horizontal : string;
  vertical : string;
}

let empty_box_chars =
  {
    head_left = "";
    head_right = "";
    head_vertical = "";
    head_horizontal = "";
    row_left = "";
    row_right = "";
    row_vertical = "";
    row_horizontal = "";
    mid_left = "";
    mid_right = "";
    mid_vertical = "";
    mid_horizontal = "";
    foot_left = "";
    foot_right = "";
    foot_vertical = "";
    foot_horizontal = "";
    vertical = "";
  }

let ascii_chars =
  {
    head_left = "+";
    head_right = "+";
    head_vertical = "+";
    head_horizontal = "-";
    row_left = "+";
    row_right = "+";
    row_vertical = "+";
    row_horizontal = "-";
    mid_left = "+";
    mid_right = "+";
    mid_vertical = "+";
    mid_horizontal = "-";
    foot_left = "+";
    foot_right = "+";
    foot_vertical = "+";
    foot_horizontal = "-";
    vertical = "|";
  }

let minimal =
  {
    head_left = "";
    head_right = "";
    head_vertical = " ";
    head_horizontal = "─";
    row_left = "";
    row_right = "";
    row_vertical = " ";
    row_horizontal = "";
    mid_left = "";
    mid_right = "";
    mid_vertical = " ";
    mid_horizontal = "";
    foot_left = "";
    foot_right = "";
    foot_vertical = " ";
    foot_horizontal = "";
    vertical = " ";
  }

let get_box_chars style safe =
  let use_ascii = safe in
  match style with
  | No_box -> empty_box_chars
  | Ascii -> ascii_chars
  | Simple ->
      if use_ascii then ascii_chars
      else
        {
          ascii_chars with
          head_horizontal = "─";
          row_horizontal = "─";
          mid_horizontal = "─";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Rounded ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "╭";
          head_right = "╮";
          head_vertical = "┬";
          head_horizontal = "─";
          row_left = "│";
          row_right = "│";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "╰";
          foot_right = "╯";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Heavy ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "┏";
          head_right = "┓";
          head_vertical = "┳";
          head_horizontal = "━";
          row_left = "┣";
          row_right = "┫";
          row_vertical = "╋";
          row_horizontal = "━";
          mid_left = "┣";
          mid_right = "┫";
          mid_vertical = "╋";
          mid_horizontal = "━";
          foot_left = "┗";
          foot_right = "┛";
          foot_vertical = "┻";
          foot_horizontal = "━";
          vertical = "┃";
        }
  | Heavy_head ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "┏";
          head_right = "┓";
          head_vertical = "┳";
          head_horizontal = "━";
          row_left = "├";
          row_right = "┤";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "└";
          foot_right = "┘";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Double ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "╔";
          head_right = "╗";
          head_vertical = "╦";
          head_horizontal = "═";
          row_left = "╠";
          row_right = "╣";
          row_vertical = "╬";
          row_horizontal = "═";
          mid_left = "╠";
          mid_right = "╣";
          mid_vertical = "╬";
          mid_horizontal = "═";
          foot_left = "╚";
          foot_right = "╝";
          foot_vertical = "╩";
          foot_horizontal = "═";
          vertical = "║";
        }
  | Double_edge ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "╔";
          head_right = "╗";
          head_vertical = "╤";
          head_horizontal = "═";
          row_left = "│";
          row_right = "│";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "╚";
          foot_right = "╝";
          foot_vertical = "╧";
          foot_horizontal = "═";
          vertical = "║";
        }
  | Minimal_heavy_head ->
      if use_ascii then ascii_chars
      else { ascii_chars with head_horizontal = "━"; head_vertical = " " }
  | Minimal_double_head ->
      if use_ascii then minimal
      else { minimal with head_horizontal = "═"; head_vertical = " " }
  | Minimal -> minimal
  | Square ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "┌";
          head_right = "┐";
          head_vertical = "┬";
          head_horizontal = "─";
          row_left = "├";
          row_right = "┤";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "└";
          foot_right = "┘";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Square_double_head ->
      if use_ascii then ascii_chars
      else
        {
          head_left = "╒";
          head_right = "╕";
          head_vertical = "╤";
          head_horizontal = "═";
          row_left = "╞";
          row_right = "╡";
          row_vertical = "╪";
          row_horizontal = "═";
          mid_left = "╞";
          mid_right = "╡";
          mid_vertical = "╪";
          mid_horizontal = "═";
          foot_left = "╘";
          foot_right = "╛";
          foot_vertical = "╧";
          foot_horizontal = "═";
          vertical = "│";
        }

type grapheme = { text : string; width : int }

let graphemes text =
  let acc = ref [] in
  Glyph.String.iter_graphemes
    (fun ~offset:off ~len ->
      let text = String.sub text off len in
      let width =
        Glyph.String.measure ~width_method:`Unicode ~tab_width:2 text
      in
      let width = if width < 0 then 0 else width in
      acc := { text; width } :: !acc)
    text;
  List.rev !acc

let rec take_width_with acc current target = function
  | [] -> (List.rev acc, [])
  | g :: rest ->
      if current + g.width > target then (List.rev acc, g :: rest)
      else take_width_with (g :: acc) (current + g.width) target rest

let take_width acc target graphemes = take_width_with acc 0 target graphemes

let take_string text width =
  let items, _ = take_width [] width (graphemes text) in
  String.concat "" (List.map (fun g -> g.text) items)

let rec split_by_width acc width graphemes =
  if graphemes = [] then List.rev acc
  else
    let taken, rest = take_width [] width graphemes in
    let chunk = String.concat "" (List.map (fun g -> g.text) taken) in
    split_by_width (chunk :: acc) width rest

let truncate_with_suffix text width suffix =
  let suffix_width = width_of_string suffix in
  if width_of_string text <= width then text
  else if width <= suffix_width then take_string text width
  else
    let target = width - suffix_width in
    let prefix = take_string text target in
    prefix ^ suffix

let crop_to_width text width = take_string text width
let chop text width = split_by_width [] width (graphemes text)

let normalize_words text =
  text |> String.split_on_char ' ' |> List.filter (fun w -> w <> "")

let word_wrap text width =
  if width <= 0 then [ "" ]
  else
    let words = normalize_words text in
    let rec loop acc current current_width = function
      | [] ->
          let acc =
            if current = [] then acc
            else String.concat " " (List.rev current) :: acc
          in
          List.rev acc
      | word :: rest ->
          let word_width = width_of_string word in
          let extra = if current = [] then 0 else 1 in
          if word_width + current_width + extra <= width then
            loop acc (word :: current) (current_width + word_width + extra) rest
          else if word_width >= width then
            let acc =
              if current = [] then acc
              else String.concat " " (List.rev current) :: acc
            in
            let pieces = chop word width in
            loop (List.rev_append pieces acc) [] 0 rest
          else
            loop
              (String.concat " " (List.rev current) :: acc)
              [ word ] word_width rest
    in
    loop [] [] 0 words

let rec justify_string line width = function
  | `Left ->
      let pad = max 0 (width - width_of_string line) in
      line ^ String.make pad ' '
  | `Right ->
      let pad = max 0 (width - width_of_string line) in
      String.make pad ' ' ^ line
  | `Center ->
      let pad_total = max 0 (width - width_of_string line) in
      let left = pad_total / 2 in
      let right = pad_total - left in
      String.make left ' ' ^ line ^ String.make right ' '
  | `Full ->
      let words = normalize_words line in
      let word_count = List.length words in
      if word_count <= 1 then justify_string line width `Left
      else
        let words_width =
          List.fold_left (fun acc w -> acc + width_of_string w) 0 words
        in
        let remaining = max 0 (width - words_width) in
        let gaps = word_count - 1 in
        let base = remaining / gaps in
        let extra = remaining mod gaps in
        let _, result =
          List.fold_left
            (fun (idx, acc) word ->
              let sep =
                if idx = 0 then ""
                else
                  let fill = base + if idx <= extra then 1 else 0 in
                  String.make fill ' '
              in
              (idx + 1, acc ^ sep ^ word))
            (0, "") words
        in
        result

type measurement = { min : int; max : int }

let max_word_width text =
  match normalize_words text with
  | [] -> 1
  | words ->
      List.fold_left (fun acc word -> max acc (width_of_string word)) 0 words

let cell_measurement cell column =
  let text = cell.Cell.text in
  if text = "" then { min = 0; max = 0 }
  else
    let lines = String.split_on_char '\n' text in
    match column.overflow with
    | `Crop | `Ellipsis ->
        let width =
          List.fold_left
            (fun acc line -> max acc (width_of_string line))
            0 lines
        in
        { min = width; max = width }
    | `Fold ->
        let measure_line line =
          if column.no_wrap then { min = 1; max = width_of_string line }
          else { min = max_word_width line; max = width_of_string line }
        in
        List.fold_left
          (fun acc line ->
            let m = measure_line line in
            { min = max acc.min m.min; max = max acc.max m.max })
          { min = 0; max = 0 } lines

let ratio_distribute amount ratios mins =
  let total_r = List.fold_left ( + ) 0 ratios in
  if total_r = 0 then List.map (fun m -> m) mins
  else
    let parts = List.map (fun r -> r * amount / total_r) ratios in
    let distributed = List.fold_left ( + ) 0 parts in
    let remain = amount - distributed in
    let parts = List.mapi (fun i p -> if i < remain then p + 1 else p) parts in
    List.map2 max parts mins

let ratio_reduce amount ratios maxs values =
  let values = List.map2 min values maxs in
  let total_r = List.fold_left ( + ) 0 ratios in
  if total_r = 0 then values
  else
    let rec loop values excess =
      if excess <= 0 then values
      else
        let total = List.fold_left ( + ) 0 values in
        let excess = total - amount in
        if excess <= 0 then values
        else
          let deductions = List.map (fun r -> r * excess / total_r) ratios in
          let remaining = ref excess in
          let adjusted =
            List.mapi
              (fun i v ->
                let d = List.nth deductions i in
                if d > 0 then (
                  let new_v = max 0 (v - d) in
                  remaining := !remaining - (v - new_v);
                  new_v)
                else v)
              values
          in
          loop adjusted !remaining
    in
    let total = List.fold_left ( + ) 0 values in
    loop values (total - amount)

let collapse_widths widths wrapable max_width =
  let total = List.fold_left ( + ) 0 widths in
  let excess = total - max_width in
  let rec loop widths excess =
    if excess <= 0 || not (List.exists (fun b -> b) wrapable) then widths
    else
      let max_col =
        List.fold_left2
          (fun acc w wrappable -> if wrappable then max acc w else acc)
          0 widths wrapable
      in
      let second =
        List.fold_left2
          (fun acc w wrappable ->
            if wrappable && w <> max_col then max acc w else acc)
          0 widths wrapable
      in
      let diff = max_col - second in
      if diff <= 0 then widths
      else
        let ratios =
          List.map2
            (fun w wrappable -> if wrappable && w = max_col then 1 else 0)
            widths wrapable
        in
        let reduced = ratio_reduce max_width ratios widths widths in
        let new_total = List.fold_left ( + ) 0 reduced in
        loop reduced (new_total - max_width)
  in
  loop widths excess

let get_effective_padding top right bottom left is_first_col is_last_col
    is_first_row is_last_row collapse pad_edge =
  let e_top = ref top in
  let e_right = ref right in
  let e_bottom = ref bottom in
  let e_left = ref left in
  if collapse then (
    if not is_first_col then e_left := 0;
    if not is_first_row then e_top := 0);
  if not pad_edge then (
    if is_first_col then e_left := 0;
    if is_last_col then e_right := 0;
    if is_first_row then e_top := 0;
    if is_last_row then e_bottom := 0);
  (!e_top, !e_right, !e_bottom, !e_left)

let column_measurement column idx show_header show_footer rows pad_width =
  let measure_cell cell = cell_measurement cell column in
  let header =
    if show_header then
      match column.header with
      | None -> { min = 0; max = 0 }
      | Some h -> measure_cell h
    else { min = 0; max = 0 }
  in
  let footer =
    if show_footer then
      match column.footer with
      | None -> { min = 0; max = 0 }
      | Some f -> measure_cell f
    else { min = 0; max = 0 }
  in
  let data =
    List.map
      (fun row ->
        match List.nth_opt row.cells idx with
        | Some cell -> measure_cell cell
        | None -> { min = 0; max = 0 })
      rows
  in
  let combined = header :: footer :: data in
  let min_width =
    List.fold_left (fun acc meas -> max acc meas.min) 0 combined + pad_width
  in
  let max_width =
    List.fold_left (fun acc meas -> max acc meas.max) 0 combined + pad_width
  in
  match column.width with
  | Some w -> { min = w; max = w }
  | None ->
      let min_width =
        match column.min_width with
        | Some v -> max min_width v
        | None -> min_width
      in
      let max_width =
        match column.max_width with
        | Some v -> min max_width v
        | None -> max_width
      in
      { min = min_width; max = max_width }

let calculate_column_widths columns rows available show_header show_footer
    expand box_style show_edge top right bottom left collapse pad_edge =
  let column_count = List.length columns in
  if column_count = 0 then []
  else
    let extra =
      if box_style = No_box then 0
      else
        let borders = if show_edge then 2 else 0 in
        borders + max 0 (column_count - 1)
    in
    let available_columns = available - extra in
    let measurements =
      List.mapi
        (fun idx column ->
          let max_pad =
            get_effective_padding top right bottom left (idx = 0)
              (idx = column_count - 1)
              true false collapse pad_edge
            |> fun (_, pr, _, pl) -> pl + pr
          in
          column_measurement column idx show_header show_footer rows max_pad)
        columns
    in
    let widths = List.map (fun m -> m.max) measurements in
    let total_width = List.fold_left ( + ) 0 widths in
    let flexible = List.map (fun col -> Option.is_some col.ratio) columns in
    let widths =
      if
        total_width < available_columns
        && expand
        && List.exists (fun x -> x) flexible
      then
        let ratios =
          List.map (fun column -> Option.value column.ratio ~default:0) columns
        in
        let mins =
          List.map2
            (fun m (col : column) ->
              match col.width with None -> m.min | Some w -> w)
            measurements columns
        in
        let fixed_total =
          List.fold_left2
            (fun acc measurement is_flex ->
              if is_flex then acc else acc + measurement.max)
            0 measurements flexible
        in
        let flex_available = max 0 (available_columns - fixed_total) in
        let distributed = ratio_distribute flex_available ratios mins in
        let idx = ref 0 in
        List.mapi
          (fun i measurement ->
            if List.nth flexible i then (
              let value = List.nth distributed !idx in
              incr idx;
              max measurement.min value)
            else measurement.max)
          measurements
      else widths
    in
    let total = List.fold_left ( + ) 0 widths in
    let widths =
      if total > available_columns then
        let wrapable =
          List.map
            (fun (column : column) -> column.width = None && not column.no_wrap)
            columns
        in
        collapse_widths widths wrapable available_columns
      else widths
    in
    let final_total = List.fold_left ( + ) 0 widths in
    if final_total < available_columns && expand then
      let extra_space = available_columns - final_total in
      let growth =
        ratio_distribute extra_space
          (List.map (fun w -> w) widths)
          (List.init column_count (fun _ -> 0))
      in
      List.map2 ( + ) widths growth
    else widths

type prepared_cell = {
  lines : string list;
  padding_top : int;
  padding_bottom : int;
  padding_left : int;
  padding_right : int;
  width : int;
  style : Ansi.Style.t;
  vertical : vertical_align;
}

let effective_cell_style options (column : column) cell overlays =
  let base = options.cell_style in
  let base = merge_style base column.style in
  let base = merge_styles base overlays in
  merge_style base cell.Cell.style

let apply_overflow cell column width =
  let text = cell.Cell.text in
  match column.overflow with
  | `Crop ->
      text |> String.split_on_char '\n'
      |> List.map (fun line -> crop_to_width line width)
  | `Ellipsis ->
      text |> String.split_on_char '\n'
      |> List.map (fun line -> truncate_with_suffix line width "...")
  | `Fold ->
      let lines = String.split_on_char '\n' text in
      List.concat
        (List.map
           (fun line ->
             if column.no_wrap then chop line width else word_wrap line width)
           lines)

let distribute_line justify width is_last line =
  let target =
    if is_last then match justify with `Full -> `Left | other -> other
    else justify
  in
  justify_string line width target

let get_lines cell column width justify =
  let lines = apply_overflow cell column width in
  let total = List.length lines in
  List.mapi
    (fun idx line ->
      let is_last = idx = total - 1 in
      distribute_line justify width is_last line)
    lines

let prepare_cell column cell column_width options ~is_first_col ~is_last_col
    ~is_first_row ~is_last_row overlays =
  let top, right, bottom, left =
    let pt, pr, pb, pl = options.padding in
    get_effective_padding pt pr pb pl is_first_col is_last_col is_first_row
      is_last_row options.collapse_padding options.pad_edge
  in
  let content_width = column_width - left - right in
  let justify = column.justify in
  let style = effective_cell_style options column cell overlays in
  let lines = get_lines cell column content_width justify in
  {
    lines;
    padding_top = top;
    padding_bottom = bottom;
    padding_left = left;
    padding_right = right;
    width = column_width;
    style;
    vertical = column.vertical;
  }

let add_padding line left right =
  String.make left ' ' ^ line ^ String.make right ' '

let realize_cell_lines cell row_height =
  let content_height = List.length cell.lines in
  let total_height = content_height + cell.padding_top + cell.padding_bottom in
  let extra = max 0 (row_height - total_height) in
  let top_extra, bottom_extra =
    match cell.vertical with
    | `Top -> (0, extra)
    | `Bottom -> (extra, 0)
    | `Middle ->
        let top = extra / 2 in
        (top, extra - top)
  in
  let top_pad = cell.padding_top + top_extra in
  let bottom_pad = cell.padding_bottom + bottom_extra in
  let blank = String.make cell.width ' ' in
  let top_lines = List.init top_pad (fun _ -> blank) in
  let bottom_lines = List.init bottom_pad (fun _ -> blank) in
  let content_lines =
    List.map
      (fun line -> add_padding line cell.padding_left cell.padding_right)
      cell.lines
  in
  top_lines @ content_lines @ bottom_lines

let nth_row_style options idx =
  match options.row_styles with
  | [] -> None
  | styles ->
      let len = List.length styles in
      let style = List.nth styles (idx mod len) in
      Some style

let prepare_header columns column_widths options =
  if not options.show_header then None
  else
    let cells =
      List.mapi
        (fun idx column ->
          let cell = Option.value column.header ~default:Cell.empty in
          let overlays = [ Some options.header_style; column.header_style ] in
          prepare_cell column cell
            (List.nth column_widths idx)
            options ~is_first_col:(idx = 0)
            ~is_last_col:(idx = List.length columns - 1)
            ~is_first_row:true ~is_last_row:false overlays)
        columns
    in
    Some cells

let prepare_footer columns column_widths options =
  if not options.show_footer then None
  else
    let cells =
      List.mapi
        (fun idx column ->
          let cell = Option.value column.footer ~default:Cell.empty in
          let overlays = [ Some options.footer_style; column.footer_style ] in
          prepare_cell column cell
            (List.nth column_widths idx)
            options ~is_first_col:(idx = 0)
            ~is_last_col:(idx = List.length columns - 1)
            ~is_first_row:false ~is_last_row:true overlays)
        columns
    in
    Some cells

let prepare_body columns column_widths options rows =
  let column_count = List.length columns in
  let total_rows = List.length rows in
  List.mapi
    (fun row_idx row ->
      let row_cells = row.cells in
      let options_row_style = nth_row_style options row_idx in
      List.mapi
        (fun col_idx column ->
          let cell =
            match List.nth_opt row_cells col_idx with
            | Some cell -> cell
            | None -> Cell.empty
          in
          let overlays = [ options_row_style; row.style ] in
          prepare_cell column cell
            (List.nth column_widths col_idx)
            options ~is_first_col:(col_idx = 0)
            ~is_last_col:(col_idx = column_count - 1)
            ~is_first_row:(row_idx = 0 && not options.show_header)
            ~is_last_row:(row_idx = total_rows - 1 && not options.show_footer)
            overlays)
        columns)
    rows

let row_height cells =
  List.fold_left
    (fun acc cell ->
      max acc (cell.padding_top + cell.padding_bottom + List.length cell.lines))
    0 cells

let repeat_char ch count =
  if ch = "" || count <= 0 then ""
  else
    let buf = Buffer.create (String.length ch * count) in
    for _ = 1 to count do
      Buffer.add_string buf ch
    done;
    Buffer.contents buf

let horizontal_chars box_chars = function
  | `Head ->
      ( box_chars.head_left,
        box_chars.head_right,
        box_chars.head_vertical,
        box_chars.head_horizontal )
  | `Row ->
      ( box_chars.row_left,
        box_chars.row_right,
        box_chars.row_vertical,
        box_chars.row_horizontal )
  | `Mid ->
      ( box_chars.mid_left,
        box_chars.mid_right,
        box_chars.mid_vertical,
        box_chars.mid_horizontal )
  | `Foot ->
      ( box_chars.foot_left,
        box_chars.foot_right,
        box_chars.foot_vertical,
        box_chars.foot_horizontal )

let build_horizontal_line kind widths options box_chars =
  let left, right, divider, horizontal = horizontal_chars box_chars kind in
  if options.box_style = No_box then None
  else
    let segments =
      let cells = List.map (fun width -> repeat_char horizontal width) widths in
      let content =
        if options.show_edge then
          let inner = String.concat divider cells in
          left ^ inner ^ right
        else String.concat divider cells
      in
      Some [ (content, options.border_style) ]
    in
    segments

let build_row_lines cells options (box_chars : box_chars) =
  let height = row_height cells in
  let realized =
    List.map (fun cell -> (cell, realize_cell_lines cell height)) cells
  in
  List.init height (fun line_idx ->
      let segments = ref [] in
      if options.box_style <> No_box && options.show_edge then
        segments := (box_chars.vertical, options.border_style) :: !segments;
      List.iteri
        (fun idx (cell, lines) ->
          let content = List.nth lines line_idx in
          segments := !segments @ [ (content, cell.style) ];
          if options.box_style <> No_box && idx < List.length realized - 1 then
            segments :=
              !segments @ [ (box_chars.vertical, options.border_style) ])
        realized;
      if options.box_style <> No_box && options.show_edge then
        segments := !segments @ [ (box_chars.vertical, options.border_style) ];
      !segments)

let blank_row_lines widths options (box_chars : box_chars) =
  let total = List.fold_left ( + ) 0 widths in
  if options.box_style = No_box then
    [ [ (String.make total ' ', options.cell_style) ] ]
  else
    let horizontal = String.make total ' ' in
    let line =
      let middle =
        if options.show_edge then
          (box_chars.vertical, options.border_style)
          :: (horizontal, options.cell_style)
          :: [ (box_chars.vertical, options.border_style) ]
        else [ (horizontal, options.cell_style) ]
      in
      middle
    in
    [ line ]

let total_extra_width options box_style column_count =
  if box_style = No_box then 0
  else if options.show_edge then 2 + max 0 (column_count - 1)
  else max 0 (column_count - 1)

let build_plan columns rows options =
  let column_count = List.length columns in
  let top, right, bottom, left = options.padding in
  let max_width_for_measure =
    match options.width with Some w -> w | None -> max_int
  in
  let natural_widths =
    calculate_column_widths columns rows max_width_for_measure
      options.show_header options.show_footer false options.box_style
      options.show_edge top right bottom left options.collapse_padding
      options.pad_edge
  in
  let box_chars = get_box_chars options.box_style options.safe_box in
  let natural_inner = List.fold_left ( + ) 0 natural_widths in
  let natural_total =
    natural_inner + total_extra_width options options.box_style column_count
  in
  let target_width =
    match options.width with Some w -> w | None -> natural_total
  in
  let target_width =
    match options.min_width with
    | Some min_w -> max target_width min_w
    | None -> target_width
  in
  let final_widths =
    calculate_column_widths columns rows target_width options.show_header
      options.show_footer options.expand options.box_style options.show_edge top
      right bottom left options.collapse_padding options.pad_edge
  in
  let content_inner = List.fold_left ( + ) 0 final_widths in
  let total_width =
    content_inner + total_extra_width options options.box_style column_count
  in
  let header_cells = prepare_header columns final_widths options in
  let body_cells = prepare_body columns final_widths options rows in
  let footer_cells = prepare_footer columns final_widths options in
  let lines = ref [] in
  let append_line line = lines := !lines @ [ line ] in
  let append_lines new_lines = lines := !lines @ new_lines in
  let append_horizontal kind =
    match build_horizontal_line kind final_widths options box_chars with
    | Some line -> append_line line
    | None -> ()
  in
  Option.iter
    (fun cell ->
      let content =
        justify_string cell.Cell.text total_width options.title_justify
      in
      let style = merge_style options.title_style cell.Cell.style in
      append_line [ (content, style) ])
    options.title;
  if options.box_style <> No_box && options.show_edge then
    append_horizontal `Head;
  (match header_cells with
  | Some cells ->
      append_lines (build_row_lines cells options box_chars);
      if options.box_style <> No_box then append_horizontal `Row
  | None -> ());
  let body_row_count = List.length body_cells in
  List.iteri
    (fun idx row ->
      append_lines (build_row_lines row options box_chars);
      if idx < body_row_count - 1 then (
        (if options.leading > 0 then
           let blank = blank_row_lines final_widths options box_chars in
           for _ = 1 to options.leading do
             append_lines blank
           done);
        if options.show_lines && options.box_style <> No_box then
          append_horizontal `Row))
    body_cells;
  (match footer_cells with
  | Some cells ->
      if options.box_style <> No_box then append_horizontal `Row;
      append_lines (build_row_lines cells options box_chars)
  | None -> ());
  if options.box_style <> No_box && options.show_edge then
    append_horizontal `Foot;
  Option.iter
    (fun cell ->
      let content =
        justify_string cell.Cell.text total_width options.caption_justify
      in
      let style = merge_style options.caption_style cell.Cell.style in
      append_line [ (content, style) ])
    options.caption;
  !lines

let write_span buffer style text =
  if text = "" then ()
  else
    let chunk =
      Text_buffer.Chunk.
        {
          text = Bytes.of_string text;
          fg = style.Ansi.Style.fg;
          bg = style.Ansi.Style.bg;
          attrs = style.Ansi.Style.attrs;
          link = style.Ansi.Style.link;
        }
    in
    ignore (Text_buffer.write_chunk buffer chunk)

let write_newline buffer =
  let chunk =
    Text_buffer.Chunk.
      {
        text = Bytes.of_string "\n";
        fg = None;
        bg = None;
        attrs = Ansi.Attr.empty;
        link = None;
      }
  in
  ignore (Text_buffer.write_chunk buffer chunk)

let apply_plan t (plan : render_plan) =
  Text_surface.replace_content t.surface (fun buffer ->
      List.iteri
        (fun idx line ->
          if idx > 0 then write_newline buffer;
          List.iter (fun (text, style) -> write_span buffer style text) line)
        plan)

let rebuild t =
  let plan = build_plan t.columns t.rows t.options in
  Text_surface.set_default_style t.surface t.options.cell_style;
  apply_plan t plan;
  ignore (Renderable.mark_layout_dirty (node t));
  Renderable.request_render (node t)

let columns t = t.columns
let rows t = t.rows

let set_columns t columns =
  if columns <> t.columns then (
    t.columns <- columns;
    rebuild t)

let set_rows t rows =
  if rows <> t.rows then (
    t.rows <- rows;
    rebuild t)

let apply_props t (props : Props.t) =
  (* Update full options snapshot, including columns and rows, then rebuild the
     render plan in a single pass. *)
  t.options <- props;
  t.columns <- props.columns;
  t.rows <- props.rows;
  rebuild t

let mount ?(props = Props.make ~columns:[] ~rows:[] ()) node =
  let surface =
    Text_surface.mount
      ~props:
        (Text_surface.Props.make ~wrap_mode:`None
           ~default_style:props.cell_style ())
      node
  in
  let table =
    { surface; columns = props.columns; rows = props.rows; options = props }
  in
  rebuild table;
  table
