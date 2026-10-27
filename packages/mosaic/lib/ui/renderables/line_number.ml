(* ───── Types ───── *)

type line_color = { gutter : Ansi.Color.t; content : Ansi.Color.t option }

type line_sign = {
  before : string option;
  after : string option;
  before_color : Ansi.Color.t option;
  after_color : Ansi.Color.t option;
}

(* ───── Props ───── *)

(* [Props.make] is the one place the gutter's labels get their defaults:
   [Mosaic.line_number] passes its labels on unset. *)
module Props = struct
  type t = {
    fg : Ansi.Color.t option;
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

  let make ?fg ?bg ?(min_width = 3) ?(padding_right = 1)
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

  let rec assoc_equal eq a b =
    match (a, b) with
    | [], [] -> true
    | (i1, v1) :: a, (i2, v2) :: b ->
        Int.equal i1 i2 && eq v1 v2 && assoc_equal eq a b
    | _ -> false

  let rec int_list_equal a b =
    match (a, b) with
    | [], [] -> true
    | x :: a, y :: b -> Int.equal x y && int_list_equal a b
    | _ -> false

  let equal a b =
    Option.equal Ansi.Color.equal a.fg b.fg
    && Option.equal Ansi.Color.equal a.bg b.bg
    && a.min_width = b.min_width
    && a.padding_right = b.padding_right
    && a.show_line_numbers = b.show_line_numbers
    && a.line_number_offset = b.line_number_offset
    && assoc_equal equal_line_color a.line_colors b.line_colors
    && assoc_equal equal_line_sign a.line_signs b.line_signs
    && assoc_equal Int.equal a.line_numbers b.line_numbers
    && int_list_equal a.hidden_line_numbers b.hidden_line_numbers
end

(* ───── Line Number Widget ───── *)

(* Per-line props arrive as index-keyed lists (a diff pushes one entry per
   patch line); walking them per visible row per frame is O(viewport x total
   lines). They are folded once into int-keyed tables plus the sign-width
   maxima, and rebuilt only when the props change. *)
type lookup = {
  colors : (int, line_color) Hashtbl.t;
  signs : (int, line_sign) Hashtbl.t;
  numbers : (int, int) Hashtbl.t;
  hidden : (int, unit) Hashtbl.t;
  max_before : int;
  max_after : int;
  max_custom_number : int;
}

type t = {
  node : Renderable.t;
  gutter : Renderable.t;
  content : Renderable.t;
  mutable props : Props.t;
  mutable lookup : (Matrix.Text.width_method * lookup) option;
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

let display_width ~width_method s =
  Matrix.Text.measure ~width_method ~tab_width:2 s

let darken_color (c : Ansi.Color.t) : Ansi.Color.t =
  let r, g, b = Ansi.Color.to_rgb c in
  let scale v = v * 4 / 5 in
  Ansi.Color.of_rgb (scale r) (scale g) (scale b)

let build_lookup ~width_method (props : Props.t) =
  (* First binding wins, matching List.assoc_opt on duplicate indices. *)
  let of_assoc entries =
    let tbl = Hashtbl.create (List.length entries) in
    List.iter
      (fun (i, v) -> if not (Hashtbl.mem tbl i) then Hashtbl.add tbl i v)
      entries;
    tbl
  in
  let hidden = Hashtbl.create (List.length props.hidden_line_numbers) in
  List.iter (fun i -> Hashtbl.replace hidden i ()) props.hidden_line_numbers;
  let max_before, max_after =
    List.fold_left
      (fun (b, a) (_, (sign : line_sign)) ->
        let b =
          match sign.before with
          | None -> b
          | Some s -> max b (display_width ~width_method s)
        in
        let a =
          match sign.after with
          | None -> a
          | Some s -> max a (display_width ~width_method s)
        in
        (b, a))
      (0, 0) props.line_signs
  in
  let max_custom_number =
    List.fold_left (fun acc (_, n) -> max acc n) 0 props.line_numbers
  in
  {
    colors = of_assoc props.line_colors;
    signs = of_assoc props.line_signs;
    numbers = of_assoc props.line_numbers;
    hidden;
    max_before;
    max_after;
    max_custom_number;
  }

let lookup t ~width_method =
  match t.lookup with
  | Some (m, lk) when m = width_method -> lk
  | Some _ | None ->
      let lk = build_lookup ~width_method t.props in
      t.lookup <- Some (width_method, lk);
      lk

let find_line_color lk line = Hashtbl.find_opt lk.colors line
let find_line_sign lk line = Hashtbl.find_opt lk.signs line
let find_line_number lk line = Hashtbl.find_opt lk.numbers line
let is_hidden lk line = Hashtbl.mem lk.hidden line

(* ───── Target Discovery ───── *)

let find_text_rows_child (content_node : Renderable.t) :
    Renderable.text_rows option =
  List.find_map Renderable.text_rows (Renderable.children content_node)

let line_count (rows : Renderable.text_rows option) =
  match rows with
  | None -> 0
  | Some rows ->
      Matrix.Text.Layout.Source.line_count
        (Matrix.Text.Layout.source rows.layout)

(* Visits the content rows that the grid's clip shows in a node at [y] of
   [height] rows, as node-relative rows. *)
let iter_clipped_rows (rows : Renderable.text_rows) grid ~y ~height f =
  let clip = Matrix_grid.clip_region grid in
  let first = max 0 (clip.y - y) in
  let last = min height (clip.y + clip.height - y) in
  if last > first then
    Matrix.Text.Layout.iter_rows rows.layout ~first:(rows.scroll_y + first)
      ~count:(last - first) (fun ~row ~logical_line ~continuation ->
        f ~row:(row - rows.scroll_y) ~logical_line ~continuation)

(* ───── Gutter Width Calculation ───── *)

let compute_gutter_width lk (props : Props.t) line_count =
  if not props.show_line_numbers then
    max props.min_width (lk.max_before + lk.max_after + props.padding_right)
  else
    let max_line = line_count + props.line_number_offset in
    let max_line = max max_line lk.max_custom_number in
    let num_digits = digits (max 1 max_line) in
    (* +1 for left padding *)
    max props.min_width
      (lk.max_before + num_digits + lk.max_after + props.padding_right + 1)

(* ───── Gutter Rendering ───── *)

let render_gutter t _self grid ~dt:_ =
  let rows = find_text_rows_child t.content in
  let gutter_w = Renderable.width t.gutter in
  let gutter_h = Renderable.height t.gutter in
  let gx = Renderable.x t.gutter in
  let gy = Renderable.y t.gutter in
  if gutter_w <= 0 || gutter_h <= 0 then ()
  else begin
    (* Draw gutter background *)
    (match t.props.bg with
    | Some bg ->
        Matrix_grid.fill_rect grid ~x:gx ~y:gy ~width:gutter_w ~height:gutter_h
          ~color:bg
    | None -> ());
    let width_method = Renderable.Private.width_method t.gutter in
    let lk = lookup t ~width_method in
    let bw = lk.max_before in
    (* [fg] first, then the [Muted] role; a sign's color first, then the
       numbers'. *)
    let number_style =
      let muted = Theme.style (Renderable.theme t.gutter) Muted in
      match t.props.fg with Some c -> Ansi.Style.fg c muted | None -> muted
    in
    let sign_style = function
      | Some c -> Ansi.Style.fg c number_style
      | None -> number_style
    in
    let num_width =
      if t.props.show_line_numbers then
        let max_line = line_count rows + t.props.line_number_offset in
        digits (max 1 (max max_line lk.max_custom_number))
      else 0
    in
    Option.iter
      (fun rows ->
        iter_clipped_rows rows grid ~y:gy ~height:gutter_h
          (fun ~row ~logical_line ~continuation ->
            (* Line color: apply gutter background for this row *)
            (match find_line_color lk logical_line with
            | Some lc ->
                Matrix_grid.fill_rect grid ~x:gx ~y:(gy + row) ~width:gutter_w
                  ~height:1 ~color:lc.gutter
            | None -> ());
            (* Only render number/signs on the first visual line of a logical
               line *)
            if continuation = 0 && not (is_hidden lk logical_line) then begin
              let col = ref 0 in
              (* Before sign — right-aligned within max before width *)
              (match find_line_sign lk logical_line with
              | Some sign -> (
                  match sign.before with
                  | Some s ->
                      let sw = display_width ~width_method s in
                      let padding = bw - sw in
                      col := !col + padding;
                      let style = sign_style sign.before_color in
                      Matrix_grid.draw_text ~style ~width_method grid
                        ~x:(gx + !col) ~y:(gy + row) ~text:s;
                      col := !col + sw
                  | None -> col := !col + bw)
              | None -> col := !col + bw);
              (* Line number — right-aligned with 1 col left padding *)
              if t.props.show_line_numbers then begin
                col := !col + 1;
                let line_num =
                  match find_line_number lk logical_line with
                  | Some custom -> custom
                  | None -> logical_line + 1 + t.props.line_number_offset
                in
                let num_str = string_of_int line_num in
                let pad = num_width - String.length num_str in
                col := !col + pad;
                Matrix_grid.draw_text ~style:number_style ~width_method grid
                  ~x:(gx + !col) ~y:(gy + row) ~text:num_str;
                col := !col + String.length num_str
              end;
              (* After sign *)
              match find_line_sign lk logical_line with
              | Some sign -> (
                  match sign.after with
                  | Some s ->
                      let style = sign_style sign.after_color in
                      Matrix_grid.draw_text ~style ~width_method grid
                        ~x:(gx + !col) ~y:(gy + row) ~text:s
                  | None -> ())
              | None -> ()
            end))
      rows
  end

(* ───── Content Line Color Rendering ───── *)

let render_content_colors t _self grid ~dt:_ =
  match find_text_rows_child t.content with
  | None -> ()
  | Some rows ->
      let cx = Renderable.x t.content in
      let cy = Renderable.y t.content in
      let cw = Renderable.width t.content in
      let ch = Renderable.height t.content in
      if cw > 0 && ch > 0 then
        let lk =
          lookup t ~width_method:(Renderable.Private.width_method t.content)
        in
        iter_clipped_rows rows grid ~y:cy ~height:ch
          (fun ~row ~logical_line ~continuation:_ ->
            match find_line_color lk logical_line with
            | Some lc ->
                let bg =
                  match lc.content with
                  | Some c -> c
                  | None -> darken_color lc.gutter
                in
                Matrix_grid.fill_rect grid ~x:cx ~y:(cy + row) ~width:cw
                  ~height:1 ~color:bg
            | None -> ())

(* ───── Gutter Measure Function ───── *)

let gutter_measure t ~known_dimensions ~available_space:_ ~style:_ =
  let line_count = line_count (find_text_rows_child t.content) in
  let width_method = Renderable.Private.width_method t.gutter in
  let w = compute_gutter_width (lookup t ~width_method) t.props line_count in
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

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    (props : Props.t) =
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
  let t = { node; gutter; content; props; lookup = None } in
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
  if changed then t.lookup <- None;
  if visibility_changed then
    Renderable.set_visible t.gutter props.show_line_numbers;
  if changed then begin
    Renderable.mark_dirty t.gutter;
    Renderable.request_render t.node
  end
