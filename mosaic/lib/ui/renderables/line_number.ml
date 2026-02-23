(* ───── Types ───── *)

type line_color = { gutter : Ansi.Color.t; content : Ansi.Color.t option }

type line_sign = {
  before : string option;
  after : string option;
  before_color : Ansi.Color.t option;
  after_color : Ansi.Color.t option;
}

(* ───── Props ───── *)

module Props = struct
  let default_fg = Ansi.Color.grayscale ~level:12

  type t = {
    fg : Ansi.Color.t;
    bg : Ansi.Color.t option;
    min_width : int;
    padding_right : int;
    show_line_numbers : bool;
    line_number_offset : int;
    line_colors : (int * line_color) list;
    line_signs : (int * line_sign) list;
    line_numbers : (int * int) list;
    hidden_line_numbers : int list;
  }

  let make ?(fg = default_fg) ?bg ?(min_width = 3) ?(padding_right = 1)
      ?(show_line_numbers = true) ?(line_number_offset = 0) ?(line_colors = [])
      ?(line_signs = []) ?(line_numbers = []) ?(hidden_line_numbers = []) () =
    {
      fg;
      bg;
      min_width;
      padding_right;
      show_line_numbers;
      line_number_offset;
      line_colors;
      line_signs;
      line_numbers;
      hidden_line_numbers;
    }

  let default = make ()

  let equal_line_color a b =
    Ansi.Color.equal a.gutter b.gutter
    && Option.equal Ansi.Color.equal a.content b.content

  let equal_line_sign a b =
    Option.equal String.equal a.before b.before
    && Option.equal String.equal a.after b.after
    && Option.equal Ansi.Color.equal a.before_color b.before_color
    && Option.equal Ansi.Color.equal a.after_color b.after_color

  let equal a b =
    Ansi.Color.equal a.fg b.fg
    && Option.equal Ansi.Color.equal a.bg b.bg
    && a.min_width = b.min_width
    && a.padding_right = b.padding_right
    && a.show_line_numbers = b.show_line_numbers
    && a.line_number_offset = b.line_number_offset
    && List.compare_length_with a.line_colors (List.length b.line_colors) = 0
    && List.for_all2
         (fun (i1, c1) (i2, c2) -> i1 = i2 && equal_line_color c1 c2)
         a.line_colors b.line_colors
    && List.compare_length_with a.line_signs (List.length b.line_signs) = 0
    && List.for_all2
         (fun (i1, s1) (i2, s2) -> i1 = i2 && equal_line_sign s1 s2)
         a.line_signs b.line_signs
    && List.compare_length_with a.line_numbers (List.length b.line_numbers) = 0
    && List.for_all2
         (fun (i1, n1) (i2, n2) -> i1 = i2 && n1 = n2)
         a.line_numbers b.line_numbers
    && List.compare_length_with a.hidden_line_numbers
         (List.length b.hidden_line_numbers)
       = 0
    && List.for_all2 Int.equal a.hidden_line_numbers b.hidden_line_numbers
end

(* ───── Line Number Widget ───── *)

type t = {
  node : Renderable.t;
  gutter : Renderable.t;
  content : Renderable.t;
  mutable props : Props.t;
}

let node t = t.node

(* ───── Helpers ───── *)

let digits n =
  if n < 10 then 1
  else if n < 100 then 2
  else if n < 1000 then 3
  else if n < 10000 then 4
  else if n < 100000 then 5
  else if n < 1000000 then 6
  else 7

let display_width s = Glyph.String.measure ~width_method:`Unicode ~tab_width:2 s

let darken_color (c : Ansi.Color.t) : Ansi.Color.t =
  let r, g, b = Ansi.Color.to_rgb c in
  let scale v = v * 4 / 5 in
  Ansi.Color.of_rgb (scale r) (scale g) (scale b)

let find_line_color props line = List.assoc_opt line props.Props.line_colors
let find_line_sign props line = List.assoc_opt line props.Props.line_signs
let find_line_number props line = List.assoc_opt line props.Props.line_numbers
let is_hidden props line = List.mem line props.Props.hidden_line_numbers

(* ───── Target Discovery ───── *)

let find_line_info_child (content_node : Renderable.t) :
    Renderable.line_info option =
  let children = Renderable.children content_node in
  let rec search = function
    | [] -> None
    | child :: rest -> (
        match Renderable.line_info child with
        | Some info -> Some info
        | None -> search rest)
  in
  search children

(* ───── Gutter Width Calculation ───── *)

let max_before_width props =
  List.fold_left
    (fun acc (_, (sign : line_sign)) ->
      match sign.before with None -> acc | Some s -> max acc (display_width s))
    0 props.Props.line_signs

let max_after_width props =
  List.fold_left
    (fun acc (_, (sign : line_sign)) ->
      match sign.after with None -> acc | Some s -> max acc (display_width s))
    0 props.Props.line_signs

let compute_gutter_width props line_count =
  if not props.Props.show_line_numbers then
    let bw = max_before_width props in
    let aw = max_after_width props in
    max props.min_width (bw + aw + props.padding_right)
  else
    let max_line = line_count + props.line_number_offset in
    let max_line =
      List.fold_left
        (fun acc (_, custom_num) -> max acc custom_num)
        max_line props.line_numbers
    in
    let num_digits = digits (max 1 max_line) in
    let bw = max_before_width props in
    let aw = max_after_width props in
    (* +1 for left padding *)
    max props.min_width (bw + num_digits + aw + props.padding_right + 1)

(* ───── Gutter Rendering ───── *)

let render_gutter t _self grid ~delta:_ =
  let info = find_line_info_child t.content in
  let gutter_w = Renderable.width t.gutter in
  let gutter_h = Renderable.height t.gutter in
  let gx = Renderable.x t.gutter in
  let gy = Renderable.y t.gutter in
  if gutter_w <= 0 || gutter_h <= 0 then ()
  else
    let ( line_count,
          display_line_count,
          line_sources,
          line_wrap_indices,
          scroll_y ) =
      match info with
      | None -> (0, 0, [||], [||], 0)
      | Some i ->
          ( i.line_count,
            i.display_line_count,
            i.line_sources,
            i.line_wrap_indices,
            i.scroll_y )
    in
    (* Draw gutter background *)
    (match t.props.bg with
    | Some bg ->
        Grid.fill_rect grid ~x:gx ~y:gy ~width:gutter_w ~height:gutter_h
          ~color:bg
    | None -> ());
    let bw = max_before_width t.props in
    let num_width =
      if t.props.show_line_numbers then
        let max_line = line_count + t.props.line_number_offset in
        let max_line =
          List.fold_left
            (fun acc (_, custom_num) -> max acc custom_num)
            max_line t.props.line_numbers
        in
        digits (max 1 max_line)
      else 0
    in
    for row = 0 to gutter_h - 1 do
      let display_line = scroll_y + row in
      if display_line < display_line_count then begin
        let logical_line = line_sources.(display_line) in
        let wrap_index = line_wrap_indices.(display_line) in
        (* Line color: apply gutter background for this row *)
        (match find_line_color t.props logical_line with
        | Some lc ->
            Grid.fill_rect grid ~x:gx ~y:(gy + row) ~width:gutter_w ~height:1
              ~color:lc.gutter
        | None -> ());
        (* Only render number/signs on the first visual line of a logical
           line *)
        if wrap_index = 0 && not (is_hidden t.props logical_line) then begin
          let col = ref 0 in
          (* Before sign — right-aligned within max before width *)
          (match find_line_sign t.props logical_line with
          | Some sign -> (
              match sign.before with
              | Some s ->
                  let sw = display_width s in
                  let padding = bw - sw in
                  col := !col + padding;
                  let fg = Option.value ~default:t.props.fg sign.before_color in
                  let style = Ansi.Style.make ~fg () in
                  Grid.draw_text ~style grid ~x:(gx + !col) ~y:(gy + row)
                    ~text:s;
                  col := !col + sw
              | None -> col := !col + bw)
          | None -> col := !col + bw);
          (* Line number — right-aligned with 1 col left padding *)
          if t.props.show_line_numbers then begin
            col := !col + 1;
            let line_num =
              match find_line_number t.props logical_line with
              | Some custom -> custom
              | None -> logical_line + 1 + t.props.line_number_offset
            in
            let num_str = string_of_int line_num in
            let pad = num_width - String.length num_str in
            col := !col + pad;
            let style = Ansi.Style.make ~fg:t.props.fg () in
            Grid.draw_text ~style grid ~x:(gx + !col) ~y:(gy + row)
              ~text:num_str;
            col := !col + String.length num_str
          end;
          (* After sign *)
          match find_line_sign t.props logical_line with
          | Some sign -> (
              match sign.after with
              | Some s ->
                  let fg = Option.value ~default:t.props.fg sign.after_color in
                  let style = Ansi.Style.make ~fg () in
                  Grid.draw_text ~style grid ~x:(gx + !col) ~y:(gy + row)
                    ~text:s
              | None -> ())
          | None -> ()
        end
      end
    done

(* ───── Content Line Color Rendering ───── *)

let render_content_colors t _self grid ~delta:_ =
  let info = find_line_info_child t.content in
  match info with
  | None -> ()
  | Some info ->
      let cx = Renderable.x t.content in
      let cy = Renderable.y t.content in
      let cw = Renderable.width t.content in
      let ch = Renderable.height t.content in
      if cw <= 0 || ch <= 0 then ()
      else
        for row = 0 to ch - 1 do
          let display_line = info.scroll_y + row in
          if display_line < info.display_line_count then begin
            let logical_line = info.line_sources.(display_line) in
            match find_line_color t.props logical_line with
            | Some lc ->
                let bg =
                  match lc.content with
                  | Some c -> c
                  | None -> darken_color lc.gutter
                in
                Grid.fill_rect grid ~x:cx ~y:(cy + row) ~width:cw ~height:1
                  ~color:bg
            | None -> ()
          end
        done

(* ───── Gutter Measure Function ───── *)

let gutter_measure t ~known_dimensions ~available_space:_ ~style:_ =
  let info = find_line_info_child t.content in
  let line_count = match info with None -> 0 | Some i -> i.line_count in
  let w = compute_gutter_width t.props line_count in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some w -> w
    | None -> Float.of_int w
  in
  let height =
    match known_dimensions.Toffee.Geometry.Size.height with
    | Some h -> h
    | None -> 0.
  in
  Toffee.Geometry.Size.make width height

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?fg ?bg
    ?min_width ?padding_right ?show_line_numbers ?line_number_offset
    ?line_colors ?line_signs ?line_numbers ?hidden_line_numbers () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* Set root to flex-row *)
  let root_style =
    Renderable.style node
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row
    |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Stretch)
  in
  Renderable.set_style node root_style;
  (* Create gutter node *)
  let gutter = Renderable.create ~parent:node () in
  let gutter_style =
    Renderable.style gutter |> Toffee.Style.set_flex_shrink 0.
  in
  Renderable.set_style gutter gutter_style;
  (* Create content node with flex-grow *)
  let content = Renderable.create ~parent:node () in
  let zero = Toffee.Style.Dimension.length 0. in
  let content_style =
    Renderable.style content
    |> Toffee.Style.set_flex_grow 1.
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
  in
  Renderable.set_style content content_style;
  (* Route children to content node *)
  Renderable.set_child_target node (Some content);
  let props =
    Props.make ?fg ?bg ?min_width ?padding_right ?show_line_numbers
      ?line_number_offset ?line_colors ?line_signs ?line_numbers
      ?hidden_line_numbers ()
  in
  let t = { node; gutter; content; props } in
  (* Register gutter measure function *)
  Renderable.set_measure gutter (Some (gutter_measure t));
  (* Register gutter render callback *)
  Renderable.set_render gutter (render_gutter t);
  (* Register content line color render_before *)
  Renderable.set_render_before content (Some (render_content_colors t));
  (* Re-layout gutter when content resizes (e.g. line count changes) *)
  Renderable.set_on_resize content
    (Some
       (fun _self ->
         Renderable.mark_dirty t.gutter;
         Renderable.request_render t.node));
  (* Hide gutter when line numbers are disabled *)
  if not props.show_line_numbers then Renderable.set_visible gutter false;
  t

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  let changed = not (Props.equal t.props props) in
  let visibility_changed =
    t.props.show_line_numbers <> props.show_line_numbers
  in
  t.props <- props;
  if visibility_changed then
    Renderable.set_visible t.gutter props.show_line_numbers;
  if changed then begin
    Renderable.mark_dirty t.gutter;
    Renderable.request_render t.node
  end
