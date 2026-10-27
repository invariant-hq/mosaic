(* ───── Item ───── *)

type item = { label : string; description : string }

let item ~label ?(description = "") () = { label; description }

(* ───── Props ───── *)

type action = Move_left | Move_right | Activate
type key_binding = Shortcut.t * action

let default_bindings =
  Shortcut.
    [
      (left, Move_left);
      (right, Move_right);
      (char '[', Move_left);
      (char ']', Move_right);
      (enter, Activate);
      (ctrl 'j', Activate);
    ]

let default_keymap = Keymap.make ~defaults:default_bindings ()

let keymap = function
  | [] -> default_keymap
  | custom -> Keymap.make ~defaults:default_bindings ~custom ()

(* [Props.make] is the one place the tab select's labels get their
   defaults: [Mosaic.tab_select] passes its labels on unset. *)
module Props = struct
  type t = {
    key_bindings : key_binding list;
    options : item Items.t;
    selected : int;
    tab_width : int;
    background : Ansi.Color.t option;
    text_color : Ansi.Color.t option;
    focused_background : Ansi.Color.t option;
    focused_text_color : Ansi.Color.t option;
    selected_background : Ansi.Color.t option;
    selected_text_color : Ansi.Color.t option;
    description_color : Ansi.Color.t option;
    selected_description_color : Ansi.Color.t option;
    show_underline : bool;
    show_description : bool;
    show_scroll_arrows : bool;
    wrap_selection : bool;
  }

  let make ~options ?(selected = 0) ?(tab_width = 12) ?background ?text_color
      ?focused_background ?focused_text_color ?selected_background
      ?selected_text_color ?description_color ?selected_description_color
      ?(show_underline = true) ?(show_description = false)
      ?(show_scroll_arrows = true) ?(wrap_selection = false)
      ?(key_bindings = []) () =
    {
      key_bindings;
      options = Items.of_list options;
      selected = max (-1) selected;
      tab_width = max 1 tab_width;
      background;
      text_color;
      focused_background;
      focused_text_color;
      selected_background;
      selected_text_color;
      description_color;
      selected_description_color;
      show_underline;
      show_description;
      show_scroll_arrows;
      wrap_selection;
    }

  let default = make ~options:[] ()
  let equal_item a b = a.label = b.label && a.description = b.description

  let equal a b =
    List.equal (Keymap.binding_equal ( = )) a.key_bindings b.key_bindings
    && Items.equal equal_item a.options b.options
    && Int.equal a.selected b.selected
    && a.tab_width = b.tab_width
    && Option.equal Ansi.Color.equal a.background b.background
    && Option.equal Ansi.Color.equal a.text_color b.text_color
    && Option.equal Ansi.Color.equal a.focused_background b.focused_background
    && Option.equal Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Option.equal Ansi.Color.equal a.selected_background b.selected_background
    && Option.equal Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Option.equal Ansi.Color.equal a.description_color b.description_color
    && Option.equal Ansi.Color.equal a.selected_description_color
         b.selected_description_color
    && a.show_underline = b.show_underline
    && a.show_description = b.show_description
    && a.show_scroll_arrows = b.show_scroll_arrows
    && a.wrap_selection = b.wrap_selection

  let with_options_of f l props = { props with options = Items.map_list f l }
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable items : item list;  (** The options of [props]. *)
  mutable keymap : action Keymap.t;
  mutable selected_index : int;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
}

let node t = t.node

(* ───── Helpers ───── *)

let request_render t = Renderable.request_render t.node

let clamp_selected items idx =
  let n = List.length items in
  if n = 0 then -1 else max (-1) (min idx (n - 1))

let intrinsic_height props =
  1
  + (if props.Props.show_underline then 1 else 0)
  + if props.Props.show_description then 1 else 0

(* The first visible tab. It centers the selected tab when possible. It is a
   function of the accepted selection, the options and the width, never
   stored, so a remounted tab strip shows the same tabs. *)
let scroll_offset t =
  let w = Renderable.width t.node in
  let tw = t.props.tab_width in
  let n = List.length t.items in
  if tw <= 0 || w <= 0 then 0
  else
    let max_visible = max 1 (w / tw) in
    let half = max_visible / 2 in
    max 0 (min (t.selected_index - half) (max 0 (n - max_visible)))

let list_drop n l =
  let rec loop n l =
    if n <= 0 then l else match l with [] -> [] | _ :: xs -> loop (n - 1) xs
  in
  loop n l

(* Truncate by grapheme and display column, never by byte: byte-based cuts
   split UTF-8 sequences and treat wide glyphs as one column. *)
let truncate_text ~width_method text max_width =
  if max_width <= 0 then ""
  else
    Text_crop.truncate_with_ellipsis ~width_method ~ellipsis:"\xe2\x80\xa6" text
      max_width

(* ───── Measure ───── *)

let measure t ~known_dimensions ~available_space ~style:_ =
  let content_width =
    Float.of_int (List.length t.items * max 1 t.props.tab_width)
  in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some w when w > 0. -> w
    | _ -> (
        match
          Toffee.Available_space.to_option
            available_space.Toffee.Geometry.Size.width
        with
        | Some w when w > 0. -> w
        | _ -> content_width)
  in
  let h = Float.of_int (intrinsic_height t.props) in
  Toffee.Geometry.Size.
    {
      width;
      height = (match known_dimensions.height with Some h -> h | None -> h);
    }

(* ───── Rendering ───── *)

(* Labels first, then roles: tab labels in the [Plain] role; the selected
   tab on the [Selected] surface, and also on the [Cursor] surface while
   focused, with a mark in its first cell, [cursor] while focused and
   [cursor_inactive] otherwise, so the selected tab and the focus show by a
   glyph whatever the styling; the underline and scroll arrows
   are glyphs of the theme in scope, the arrows and the description in the
   [Muted] role. *)
let labelled color style =
  match color with Some c -> Ansi.Style.fg c style | None -> style

let first_some a b = match a with Some _ -> a | None -> b

let rec render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else
    let props = t.props in
    let width_method = Renderable.width_method t.node in
    let focused = Renderable.focused t.node in
    let theme = Renderable.theme t.node in
    let bg =
      if focused then first_some props.focused_background props.background
      else props.background
    in
    Matrix_grid.clear_rect
      ~color:(Option.value bg ~default:Ansi.Color.transparent)
      grid ~x:0 ~y:0 ~width:w ~height:h;
    let tw = props.tab_width in
    let n = List.length t.items in
    let max_visible = max 1 (w / tw) in
    let scroll = scroll_offset t in
    let text =
      labelled
        (if focused then first_some props.focused_text_color props.text_color
         else props.text_color)
        (Theme.style theme Plain)
    in
    let selected =
      match props.selected_background with
      | Some bg -> Ansi.Style.make ~bg ()
      | None ->
          let s = Theme.style theme Selected in
          if focused then
            Ansi.Style.merge ~base:s ~overlay:(Theme.style theme Cursor)
          else s
    in
    let selected =
      labelled props.selected_text_color
        (Ansi.Style.merge ~base:selected ~overlay:(Theme.style theme Plain))
    in
    render_tabs t grid ~theme ~width_method ~focused ~w ~h ~tw ~max_visible
      ~scroll ~text ~selected;
    render_scroll_arrows grid ~theme ~props ~w ~n ~max_visible ~scroll;
    render_description grid ~theme ~width_method ~focused ~props ~items:t.items
      ~w ~h ~selected_index:t.selected_index

and render_tabs t grid ~theme ~width_method ~focused ~w ~h ~tw ~max_visible
    ~scroll ~text ~selected =
  let glyphs = Theme.glyphs theme in
  let visible_opts = list_drop scroll t.items in
  let rec loop lst col =
    if col >= max_visible then ()
    else
      match lst with
      | [] -> ()
      | item :: rest ->
          let is_sel = scroll + col = t.selected_index in
          let tab_x = col * tw in
          let actual_tw = min tw (w - tab_x) in
          if actual_tw > 0 then (
            let style = if is_sel then selected else text in
            if is_sel then begin
              Matrix_grid.draw_text ~style grid ~x:tab_x ~y:0
                ~text:(String.make actual_tw ' ');
              Matrix_grid.draw_text ~style grid ~x:tab_x ~y:0
                ~text:
                  (if focused then glyphs.cursor else glyphs.cursor_inactive)
            end;
            let label =
              truncate_text ~width_method item.label (actual_tw - 2)
            in
            Matrix_grid.draw_text ~style grid ~x:(tab_x + 1) ~y:0 ~text:label;
            if t.props.show_underline && is_sel && h >= 2 then
              let ul_style =
                match t.props.selected_background with
                | Some bg ->
                    Ansi.Style.bg bg (labelled t.props.selected_text_color text)
                | None -> labelled t.props.selected_text_color text
              in
              let ul_text =
                String.concat ""
                  (List.init actual_tw (fun _ -> glyphs.underline))
              in
              Matrix_grid.draw_text ~style:ul_style grid ~x:tab_x ~y:1
                ~text:ul_text);
          loop rest (col + 1)
  in
  loop visible_opts 0

and render_scroll_arrows grid ~theme ~props ~w ~n ~max_visible ~scroll =
  if props.Props.show_scroll_arrows then (
    let glyphs = Theme.glyphs theme in
    let style = Theme.style theme Muted in
    if scroll > 0 then
      Matrix_grid.draw_text ~style grid ~x:0 ~y:0 ~text:glyphs.more_before;
    if scroll + max_visible < n then
      Matrix_grid.draw_text ~style grid ~x:(w - 1) ~y:0 ~text:glyphs.more_after)

and render_description grid ~theme ~width_method ~focused ~props ~items ~w ~h
    ~selected_index =
  if props.Props.show_description && selected_index >= 0 then
    let desc_y = 1 + if props.show_underline then 1 else 0 in
    if desc_y < h then
      match List.nth_opt items selected_index with
      | Some item when item.description <> "" ->
          let desc = truncate_text ~width_method item.description (w - 1) in
          let style =
            labelled
              (if focused then
                 first_some props.selected_description_color
                   props.description_color
               else props.description_color)
              (Theme.style theme Muted)
          in
          Matrix_grid.draw_text ~style grid ~x:0 ~y:desc_y ~text:desc
      | _ -> ()

(* ───── Query ───── *)

let selected_index t = t.selected_index

let selected_item t =
  if t.selected_index < 0 then None else List.nth_opt t.items t.selected_index

let options t = t.items

(* ───── Navigation ───── *)

let move t ~next_index =
  let n = List.length t.items in
  if n > 0 then
    let idx = t.selected_index in
    let next = next_index ~n ~idx ~wrap:t.props.wrap_selection in
    if next <> idx then match t.on_change with Some f -> f next | None -> ()

let move_left t =
  move t ~next_index:(fun ~n ~idx ~wrap ->
      if idx < 0 then n - 1
      else if idx > 0 then idx - 1
      else if wrap then n - 1
      else idx)

let move_right t =
  move t ~next_index:(fun ~n ~idx ~wrap ->
      if idx < n - 1 then idx + 1 else if wrap then 0 else idx)

let select_current t =
  if t.selected_index >= 0 then
    match t.on_activate with Some f -> f t.selected_index | None -> ()

(* ───── Key Handler ───── *)

let handle_key t (event : Event.key) =
  if t.items <> [] then
    match Keymap.action t.keymap event with
    | None -> ()
    | Some action -> (
        Event.Key.prevent_default event;
        match action with
        | Move_left -> move_left t
        | Move_right -> move_right t
        | Activate -> select_current t)

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?on_change
    ?on_activate (props : Props.t) =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let options = Items.to_list props.Props.options in
  let selected_index = clamp_selected options props.selected in
  let t =
    {
      node;
      props;
      items = options;
      keymap = keymap props.key_bindings;
      selected_index;
      on_change;
      on_activate;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_focusable node true;
  Renderable.set_buffered node true;
  Renderable.set_key_behavior node (Some (handle_key t));
  Renderable.set_on_resize node (Some (fun _node -> request_render t));
  t

(* ───── Setters ───── *)

let set_source t options =
  t.props <- { t.props with options };
  t.items <- Items.to_list options;
  t.selected_index <- clamp_selected t.items t.selected_index;
  Renderable.mark_dirty t.node;
  request_render t

let set_options t items =
  if not (List.equal Props.equal_item t.items items) then
    set_source t (Items.of_list items)

let set_selected t i =
  let idx = clamp_selected t.items i in
  if idx <> t.selected_index then (
    t.selected_index <- idx;
    request_render t)

let set_tab_width t w =
  if w >= 1 && w <> t.props.tab_width then (
    t.props <- { t.props with tab_width = w };
    request_render t)

let set_background t color =
  if not (Option.equal Ansi.Color.equal t.props.background (Some color)) then (
    t.props <- { t.props with background = Some color };
    request_render t)

let set_text_color t color =
  if not (Option.equal Ansi.Color.equal t.props.text_color (Some color)) then (
    t.props <- { t.props with text_color = Some color };
    request_render t)

let set_focused_background t color =
  if not (Option.equal Ansi.Color.equal t.props.focused_background (Some color))
  then (
    t.props <- { t.props with focused_background = Some color };
    if Renderable.focused t.node then request_render t)

let set_focused_text_color t color =
  if not (Option.equal Ansi.Color.equal t.props.focused_text_color (Some color))
  then (
    t.props <- { t.props with focused_text_color = Some color };
    if Renderable.focused t.node then request_render t)

let set_selected_background t color =
  if
    not (Option.equal Ansi.Color.equal t.props.selected_background (Some color))
  then (
    t.props <- { t.props with selected_background = Some color };
    request_render t)

let set_selected_text_color t color =
  if
    not (Option.equal Ansi.Color.equal t.props.selected_text_color (Some color))
  then (
    t.props <- { t.props with selected_text_color = Some color };
    request_render t)

let set_description_color t color =
  if not (Option.equal Ansi.Color.equal t.props.description_color (Some color))
  then (
    t.props <- { t.props with description_color = Some color };
    if t.props.show_description then request_render t)

let set_selected_description_color t color =
  if
    not
      (Option.equal Ansi.Color.equal t.props.selected_description_color
         (Some color))
  then (
    t.props <- { t.props with selected_description_color = Some color };
    if t.props.show_description then request_render t)

let set_show_underline t v =
  if t.props.show_underline <> v then (
    t.props <- { t.props with show_underline = v };
    Renderable.mark_dirty t.node;
    request_render t)

let set_show_description t v =
  if t.props.show_description <> v then (
    t.props <- { t.props with show_description = v };
    Renderable.mark_dirty t.node;
    request_render t)

let set_show_scroll_arrows t v =
  if t.props.show_scroll_arrows <> v then (
    t.props <- { t.props with show_scroll_arrows = v };
    request_render t)

let set_wrap_selection t v =
  if t.props.wrap_selection <> v then
    t.props <- { t.props with wrap_selection = v }

let set_on_change t f = t.on_change <- f
let set_on_activate t f = t.on_activate <- f

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
  (* Options equal to the current ones are kept, with the new source, which a
     later declaration of the same list is then the same as. *)
  if not (Items.same t.props.options props.options) then
    if Items.equal Props.equal_item t.props.options props.options then
      t.props <- { t.props with options = props.options }
    else set_source t props.options;
  set_selected t props.selected;
  set_wrap_selection t props.wrap_selection;
  set_tab_width t props.tab_width;
  set_show_underline t props.show_underline;
  set_show_description t props.show_description;
  set_show_scroll_arrows t props.show_scroll_arrows;
  let same a b = Option.equal Ansi.Color.equal a b in
  if
    not
      (same t.props.background props.background
      && same t.props.text_color props.text_color
      && same t.props.focused_background props.focused_background
      && same t.props.focused_text_color props.focused_text_color
      && same t.props.selected_background props.selected_background
      && same t.props.selected_text_color props.selected_text_color
      && same t.props.description_color props.description_color
      && same t.props.selected_description_color
           props.selected_description_color)
  then begin
    t.props <-
      {
        t.props with
        background = props.background;
        text_color = props.text_color;
        focused_background = props.focused_background;
        focused_text_color = props.focused_text_color;
        selected_background = props.selected_background;
        selected_text_color = props.selected_text_color;
        description_color = props.description_color;
        selected_description_color = props.selected_description_color;
      };
    request_render t
  end

(* ───── Pretty-printing ───── *)

let pp ppf t =
  let n = List.length t.items in
  Format.fprintf ppf "Tab_select(%s, %d/%d" (Renderable.id t.node)
    t.selected_index n;
  if t.props.show_underline then Format.pp_print_string ppf ", underline";
  if t.props.show_description then Format.pp_print_string ppf ", desc";
  Format.pp_print_char ppf ')'
