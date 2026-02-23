(* ───── Item ───── *)

type item = { label : string; description : string option }

let item_equal (a : item) (b : item) =
  String.equal a.label b.label
  && Option.equal String.equal a.description b.description

let items_equal = List.equal item_equal

(* ───── Props ───── *)

module Props = struct
  type t = {
    options : item list;
    selected_index : int;
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    focused_background : Ansi.Color.t;
    focused_text_color : Ansi.Color.t;
    selected_background : Ansi.Color.t;
    selected_text_color : Ansi.Color.t;
    description_color : Ansi.Color.t;
    selected_description_color : Ansi.Color.t;
    show_description : bool;
    show_scroll_indicator : bool;
    wrap_selection : bool;
    item_spacing : int;
    fast_scroll_step : int;
  }

  let make ?(options = []) ?(selected_index = 0) ?background ?text_color
      ?focused_background ?focused_text_color
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?(description_color = Ansi.Color.of_rgb 136 136 136)
      ?(selected_description_color = Ansi.Color.of_rgb 204 204 204)
      ?(show_description = true) ?(show_scroll_indicator = false)
      ?(wrap_selection = false) ?(item_spacing = 0) ?(fast_scroll_step = 5) () =
    let background_opt = background in
    let background =
      Option.value background ~default:(Ansi.Color.of_rgba 0 0 0 0)
    in
    let text_color =
      Option.value text_color ~default:(Ansi.Color.of_rgb 255 255 255)
    in
    (* When no focused_background is given: inherit from the caller's
       background, or fall back to a subtle dark gray so focused state is
       visually distinguishable from unfocused transparent. *)
    let focused_background =
      match focused_background with
      | Some c -> c
      | None ->
          Option.value background_opt ~default:(Ansi.Color.of_rgb 26 26 26)
    in
    let focused_text_color =
      Option.value focused_text_color ~default:text_color
    in
    {
      options;
      selected_index = max 0 selected_index;
      background;
      text_color;
      focused_background;
      focused_text_color;
      selected_background;
      selected_text_color;
      description_color;
      selected_description_color;
      show_description;
      show_scroll_indicator;
      wrap_selection;
      item_spacing = max 0 item_spacing;
      fast_scroll_step = max 1 fast_scroll_step;
    }

  let default = make ()

  let equal a b =
    items_equal a.options b.options
    && Int.equal a.selected_index b.selected_index
    && Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.focused_background b.focused_background
    && Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Ansi.Color.equal a.selected_background b.selected_background
    && Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Ansi.Color.equal a.description_color b.description_color
    && Ansi.Color.equal a.selected_description_color
         b.selected_description_color
    && Bool.equal a.show_description b.show_description
    && Bool.equal a.show_scroll_indicator b.show_scroll_indicator
    && Bool.equal a.wrap_selection b.wrap_selection
    && Int.equal a.item_spacing b.item_spacing
    && Int.equal a.fast_scroll_step b.fast_scroll_step
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable items : item array;
  mutable selected_index : int;
  mutable scroll_offset : int;
  mutable lines_per_item : int;
  mutable max_visible_items : int;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
}

let node t = t.node

(* ───── Constants ───── *)

let indicator_selected = "\xe2\x96\xb6 " (* ▶ + space *)
let indicator_blank = "  "
let scroll_block = "\xe2\x96\x88" (* █ *)
let uchar_j = Uchar.of_char 'j'
let uchar_k = Uchar.of_char 'k'

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let option_count t = Array.length t.items

let clamp_index t idx =
  let len = option_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

let effective_bg t focused =
  if focused then t.props.focused_background else t.props.background

let effective_fg t focused =
  if focused then t.props.focused_text_color else t.props.text_color

(* Each item occupies 1 line (label) or 2 lines (label + description), plus any
   inter-item spacing. This drives both viewport capacity and mouse click ->
   item index mapping. *)
let recalc_lines_per_item t =
  let base = if t.props.show_description then 2 else 1 in
  t.lines_per_item <- base + t.props.item_spacing

let recalc_max_visible t height =
  let h = max 0 height in
  let lpi = max 1 t.lines_per_item in
  t.max_visible_items <- max 1 (h / lpi)

(* Center the selected item within the visible viewport when possible, clamped
   so the scroll offset never goes negative or past the last page. *)
let update_scroll_offset t =
  let len = option_count t in
  if len = 0 then t.scroll_offset <- 0
  else
    let half = max 0 (t.max_visible_items / 2) in
    let max_off = max 0 (len - t.max_visible_items) in
    let desired = t.selected_index - half in
    t.scroll_offset <- max 0 (min desired max_off)

let set_selected_index t idx =
  let len = option_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then (
      t.selected_index <- idx;
      update_scroll_offset t;
      (match t.on_change with None -> () | Some f -> f t.selected_index);
      request t)

(* ───── Public Accessors ───── *)

let options t = Array.to_list t.items
let selected_index t = t.selected_index

let selected_item t =
  let len = option_count t in
  if len = 0 then None else Some t.items.(t.selected_index)

(* ───── Options ───── *)

let set_options t opts =
  t.items <- Array.of_list opts;
  t.selected_index <- clamp_index t t.selected_index;
  update_scroll_offset t;
  request t

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index - steps in
    if new_index >= 0 then set_selected_index t new_index
    else if t.props.wrap_selection then set_selected_index t (len - 1)
    else set_selected_index t 0

let move_down ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index + steps in
    if new_index < len then set_selected_index t new_index
    else if t.props.wrap_selection then set_selected_index t 0
    else set_selected_index t (len - 1)

(* ───── Display Setters ───── *)

let set_show_description t flag =
  if t.props.show_description <> flag then (
    t.props <- { t.props with show_description = flag };
    recalc_lines_per_item t;
    update_scroll_offset t;
    request t)

let set_show_scroll_indicator t flag =
  if t.props.show_scroll_indicator <> flag then (
    t.props <- { t.props with show_scroll_indicator = flag };
    request t)

let set_item_spacing t n =
  let n = max 0 n in
  if t.props.item_spacing <> n then (
    t.props <- { t.props with item_spacing = n };
    recalc_lines_per_item t;
    update_scroll_offset t;
    request t)

(* ───── Behavior Setters ───── *)

let set_wrap_selection t flag =
  if t.props.wrap_selection <> flag then (
    t.props <- { t.props with wrap_selection = flag };
    request t)

let set_fast_scroll_step t n =
  let n = max 1 n in
  if t.props.fast_scroll_step <> n then (
    t.props <- { t.props with fast_scroll_step = n };
    request t)

(* ───── Color Setters ───── *)

let set_background t c =
  if not (Ansi.Color.equal t.props.background c) then (
    t.props <- { t.props with background = c };
    request t)

let set_text_color t c =
  if not (Ansi.Color.equal t.props.text_color c) then (
    t.props <- { t.props with text_color = c };
    request t)

let set_focused_background t c =
  if not (Ansi.Color.equal t.props.focused_background c) then (
    t.props <- { t.props with focused_background = c };
    request t)

let set_focused_text_color t c =
  if not (Ansi.Color.equal t.props.focused_text_color c) then (
    t.props <- { t.props with focused_text_color = c };
    request t)

let set_selected_background t c =
  if not (Ansi.Color.equal t.props.selected_background c) then (
    t.props <- { t.props with selected_background = c };
    request t)

let set_selected_text_color t c =
  if not (Ansi.Color.equal t.props.selected_text_color c) then (
    t.props <- { t.props with selected_text_color = c };
    request t)

let set_description_color t c =
  if not (Ansi.Color.equal t.props.description_color c) then (
    t.props <- { t.props with description_color = c };
    request t)

let set_selected_description_color t c =
  if not (Ansi.Color.equal t.props.selected_description_color c) then (
    t.props <- { t.props with selected_description_color = c };
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
    | Char c when Uchar.equal c uchar_k ->
        move_up t;
        true
    | Char c when Uchar.equal c uchar_j ->
        move_down t;
        true
    | Enter | KP_enter ->
        (if option_count t > 0 then
           match t.on_activate with None -> () | Some f -> f t.selected_index);
        true
    | _ -> false
  in
  if consumed then Event.Key.prevent_default event

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  let x = Event.Mouse.x event in
  let y = Event.Mouse.y event in
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      if x >= 0 && x < width && y >= 0 && y < height then
        (* Map pixel row to item index via lines_per_item stride *)
        let index = t.scroll_offset + (y / max 1 t.lines_per_item) in
        if index < option_count t then (
          set_selected_index t index;
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

(* ───── Rendering ───── *)

let render t _self grid ~delta:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in
    let base_bg = effective_bg t focused in

    (* Clear the entire area first so spacing rows and empty regions below the
       last item don't show stale content. *)
    Grid.fill_rect grid ~x:0 ~y:0 ~width ~height ~color:base_bg;

    (* Recompute layout metrics from current props and viewport size. Done each
       frame because the viewport may have resized between renders without
       triggering on_resize (e.g. flex reflow). *)
    recalc_lines_per_item t;
    recalc_max_visible t height;
    update_scroll_offset t;

    let max_visible = t.max_visible_items in
    let start_index = t.scroll_offset in
    let end_index = min (option_count t) (start_index + max_visible) in
    let base_text = effective_fg t focused in

    (* Draw visible items *)
    for i = 0 to max_visible - 1 do
      let actual_index = start_index + i in
      if actual_index < end_index then
        let it = t.items.(actual_index) in
        let item_y = i * t.lines_per_item in
        if item_y < height then (
          let is_selected = actual_index = t.selected_index in
          (* Highlight covers only the content rows (label + description), not
             the inter-item spacing rows. *)
          let content_height = if t.props.show_description then 2 else 1 in
          if is_selected then
            Grid.fill_rect grid ~x:0 ~y:item_y ~width ~height:content_height
              ~color:t.props.selected_background;
          let indicator =
            if is_selected then indicator_selected else indicator_blank
          in
          let label_color =
            if is_selected then t.props.selected_text_color else base_text
          in
          let label_text = indicator ^ it.label in
          Grid.draw_text
            ~style:(Ansi.Style.make ~fg:label_color ())
            grid ~x:1 ~y:item_y ~text:label_text;
          if t.props.show_description then
            match it.description with
            | None -> ()
            | Some desc ->
                if item_y + 1 < height then
                  let desc_color =
                    if is_selected then t.props.selected_description_color
                    else t.props.description_color
                  in
                  (* Description indented past the indicator column *)
                  Grid.draw_text
                    ~style:(Ansi.Style.make ~fg:desc_color ())
                    grid ~x:3 ~y:(item_y + 1) ~text:desc)
    done;

    (* Scroll indicator: single block character in the rightmost column,
       positioned proportionally to the selection within the list. *)
    if t.props.show_scroll_indicator && option_count t > max_visible then
      let len = max 1 (option_count t - 1) in
      let scroll_percent = float t.selected_index /. float len in
      (* Reserve first and last row, place indicator in the range [1,
         height-2] *)
      let indicator_height = max 1 (height - 2) in
      let indicator_y =
        1 + int_of_float (floor (scroll_percent *. float indicator_height))
      in
      let indicator_x = width - 1 in
      Grid.draw_text
        ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 102 102 102) ())
        grid ~x:indicator_x ~y:indicator_y ~text:scroll_block

(* ───── Resize ───── *)

let on_resize t _node =
  let h = Renderable.height t.node in
  recalc_max_visible t h;
  update_scroll_offset t;
  request t

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?options
    ?selected_index ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_description
    ?show_scroll_indicator ?wrap_selection ?item_spacing ?fast_scroll_step () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?options ?selected_index ?background ?text_color
      ?focused_background ?focused_text_color ?selected_background
      ?selected_text_color ?description_color ?selected_description_color
      ?show_description ?show_scroll_indicator ?wrap_selection ?item_spacing
      ?fast_scroll_step ()
  in
  let items = Array.of_list props.options in
  let t =
    {
      node;
      props;
      items;
      selected_index = 0;
      scroll_offset = 0;
      lines_per_item = 1;
      max_visible_items = 1;
      on_change = None;
      on_activate = None;
    }
  in
  t.selected_index <- clamp_index t props.selected_index;
  recalc_lines_per_item t;
  recalc_max_visible t (Renderable.height node);
  update_scroll_offset t;
  (* Wire up rendering and event handlers. Buffered rendering avoids redundant
     redraws when multiple properties change in a single frame. *)
  Renderable.set_render node (render t);
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
  if not (items_equal (Array.to_list t.items) props.options) then
    set_options t props.options;
  if props.selected_index <> t.selected_index then
    set_selected_index t props.selected_index;
  set_wrap_selection t props.wrap_selection;
  set_show_description t props.show_description;
  set_show_scroll_indicator t props.show_scroll_indicator;
  set_item_spacing t props.item_spacing;
  set_fast_scroll_step t props.fast_scroll_step;
  set_background t props.background;
  set_text_color t props.text_color;
  set_focused_background t props.focused_background;
  set_focused_text_color t props.focused_text_color;
  set_selected_background t props.selected_background;
  set_selected_text_color t props.selected_text_color;
  set_description_color t props.description_color;
  set_selected_description_color t props.selected_description_color

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Select(%s, %d/%d" (Renderable.id t.node) t.selected_index
    (option_count t);
  if t.props.wrap_selection then Format.pp_print_string ppf ", wrap";
  Format.pp_print_char ppf ')'
