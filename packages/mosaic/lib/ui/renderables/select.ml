(* ───── Item ───── *)

type item = { label : string; description : string option }

let item_equal (a : item) (b : item) =
  String.equal a.label b.label
  && Option.equal String.equal a.description b.description

type action = Move_up | Move_down | Move_up_fast | Move_down_fast | Activate

let key_bindings_equal = List.equal (Keymap.binding_equal ( = ))

let default_key_bindings =
  [
    (Shortcut.up, Move_up);
    (Shortcut.down, Move_down);
    (Shortcut.char 'k', Move_up);
    (Shortcut.char 'j', Move_down);
    (Shortcut.key ~shift:true Up, Move_up_fast);
    (Shortcut.key ~shift:true Down, Move_down_fast);
    (Shortcut.enter, Activate);
  ]

let default_keymap = Keymap.make ~defaults:default_key_bindings ()

let keymap = function
  | [] -> default_keymap
  | custom -> Keymap.make ~defaults:default_key_bindings ~custom ()

(* ───── Props ───── *)

module Props = struct
  type t = {
    options : item Items.t;
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
    key_bindings : action Keymap.binding list;
  }

  let make ?(options = []) ?(selected_index = 0) ?background ?text_color
      ?focused_background ?focused_text_color
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?(description_color = Ansi.Color.of_rgb 136 136 136)
      ?(selected_description_color = Ansi.Color.of_rgb 204 204 204)
      ?(show_description = true) ?(show_scroll_indicator = false)
      ?(wrap_selection = false) ?(item_spacing = 0) ?(fast_scroll_step = 5)
      ?(key_bindings = []) () =
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
      options = Items.of_list options;
      selected_index = max (-1) selected_index;
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
      key_bindings;
    }

  let default = make ()
  let with_options_of f l props = { props with options = Items.map_list f l }

  let equal a b =
    Items.equal item_equal a.options b.options
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
    && key_bindings_equal a.key_bindings b.key_bindings
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable items : item array;
  mutable selected_index : int;
  mutable content_widths : (Matrix.Text.width_method * int * int) option;
  mutable keymap : action Keymap.t;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
}

let node t = t.node

(* ───── Constants ───── *)

let indicator_selected = "\xe2\x96\xb6 " (* ▶ + space *)
let indicator_blank = "  "
let scroll_block = "\xe2\x96\x88" (* █ *)

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let option_count t = Array.length t.items

let clamp_index t idx =
  let len = option_count t in
  if len = 0 then -1 else max (-1) (min (len - 1) idx)

let effective_bg t focused =
  if focused then t.props.focused_background else t.props.background

let effective_fg t focused =
  if focused then t.props.focused_text_color else t.props.text_color

(* Each item occupies 1 line (label) or 2 lines (label + description), plus any
   inter-item spacing. This drives both viewport capacity and mouse click ->
   item index mapping. *)
let lines_per_item t =
  (if t.props.show_description then 2 else 1) + t.props.item_spacing

let max_visible_items t =
  let h = max 0 (Renderable.height t.node) in
  max 1 ((h + t.props.item_spacing) / lines_per_item t)

(* The first visible item. It centers the selected item when possible, clamped
   so that it never goes negative or past the last page. It is a function of
   the accepted selection, the options and the height, never stored, so a
   remounted select shows the same items and only an accepted selection
   scrolls. *)
let scroll_offset t =
  let len = option_count t in
  let max_visible = max_visible_items t in
  let max_off = max 0 (len - max_visible) in
  max 0 (min (t.selected_index - (max_visible / 2)) max_off)

let propose_selected_index t idx =
  let len = option_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then
      match t.on_change with None -> () | Some f -> f idx

(* ───── Public Accessors ───── *)

let options t = Array.to_list t.items
let selected_index t = t.selected_index

let selected_item t =
  let len = option_count t in
  if len = 0 || t.selected_index < 0 then None
  else Some t.items.(t.selected_index)

(* ───── Options ───── *)

let set_source t options =
  t.props <- { t.props with options };
  t.items <- Items.to_array options;
  t.content_widths <- None;
  Renderable.mark_dirty t.node;
  t.selected_index <- clamp_index t t.selected_index;
  request t

let set_options t opts = set_source t (Items.of_list opts)

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else if t.selected_index < 0 then propose_selected_index t (len - 1)
  else
    let new_index = t.selected_index - steps in
    if new_index >= 0 then propose_selected_index t new_index
    else if t.props.wrap_selection then propose_selected_index t (len - 1)
    else propose_selected_index t 0

let move_down ?(steps = 1) t =
  let len = option_count t in
  if len = 0 then ()
  else if t.selected_index < 0 then propose_selected_index t 0
  else
    let new_index = t.selected_index + steps in
    if new_index < len then propose_selected_index t new_index
    else if t.props.wrap_selection then propose_selected_index t 0
    else propose_selected_index t (len - 1)

(* ───── Display Setters ───── *)

let set_show_description t flag =
  if t.props.show_description <> flag then (
    t.props <- { t.props with show_description = flag };
    Renderable.mark_dirty t.node;
    request t)

let set_show_scroll_indicator t flag =
  if t.props.show_scroll_indicator <> flag then (
    t.props <- { t.props with show_scroll_indicator = flag };
    Renderable.mark_dirty t.node;
    request t)

let set_item_spacing t n =
  let n = max 0 n in
  if t.props.item_spacing <> n then (
    t.props <- { t.props with item_spacing = n };
    Renderable.mark_dirty t.node;
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
  if not (Event.Key.default_prevented event) then
    match Keymap.action t.keymap (Event.Key.data event) with
    | None -> ()
    | Some action -> (
        Event.Key.prevent_default event;
        match action with
        | Move_up -> move_up t
        | Move_down -> move_down t
        | Move_up_fast -> move_up ~steps:t.props.fast_scroll_step t
        | Move_down_fast -> move_down ~steps:t.props.fast_scroll_step t
        | Activate ->
            if t.selected_index >= 0 then
              Option.iter (fun f -> f t.selected_index) t.on_activate)

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  let x = Event.Mouse.x event - Renderable.x t.node in
  let y = Event.Mouse.y event - Renderable.y t.node in
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      if x >= 0 && x < width && y >= 0 && y < height then
        (* Map pixel row to item index via lines_per_item stride *)
        let index = scroll_offset t + (y / lines_per_item t) in
        if index < option_count t then (
          propose_selected_index t index;
          Event.Mouse.stop_propagation event)
  | Scroll { direction; delta } -> (
      match direction with
      | Event.Mouse.Scroll_up when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Event.Mouse.Scroll_down when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | _ -> ()

(* ───── Rendering ───── *)

let render t _self grid ~dt:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in
    let base_bg = effective_bg t focused in

    (* Clear the entire area first so spacing rows and empty regions below the
       last item don't show stale content. *)
    Matrix_grid.clear_rect ~color:base_bg grid ~x:0 ~y:0 ~width ~height;

    let max_visible = max_visible_items t in
    let start_index = scroll_offset t in
    let end_index = min (option_count t) (start_index + max_visible) in
    let base_text = effective_fg t focused in

    (* Draw visible items *)
    for i = 0 to max_visible - 1 do
      let actual_index = start_index + i in
      if actual_index < end_index then
        let it = t.items.(actual_index) in
        let item_y = i * lines_per_item t in
        if item_y < height then (
          let is_selected = actual_index = t.selected_index in
          (* Highlight covers only the content rows (label + description), not
             the inter-item spacing rows. *)
          let content_height = if t.props.show_description then 2 else 1 in
          if is_selected then
            Matrix_grid.fill_rect grid ~x:0 ~y:item_y ~width
              ~height:content_height ~color:t.props.selected_background;
          let indicator =
            if is_selected then indicator_selected else indicator_blank
          in
          let label_color =
            if is_selected then t.props.selected_text_color else base_text
          in
          let label_text = indicator ^ it.label in
          Matrix_grid.draw_text
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
                  Matrix_grid.draw_text
                    ~style:(Ansi.Style.make ~fg:desc_color ())
                    grid ~x:3 ~y:(item_y + 1) ~text:desc)
    done;

    (* Scroll indicator: single block character in the rightmost column,
       positioned proportionally to the selection within the list. *)
    if t.props.show_scroll_indicator && option_count t > max_visible then
      let len = max 1 (option_count t - 1) in
      let scroll_percent = float (max 0 t.selected_index) /. float len in
      (* Reserve first and last row, place indicator in the range [1,
         height-2] *)
      let indicator_height = max 1 (height - 2) in
      let indicator_y =
        1 + int_of_float (floor (scroll_percent *. float indicator_height))
      in
      let indicator_x = width - 1 in
      Matrix_grid.draw_text
        ~style:(Ansi.Style.make ~fg:(Ansi.Color.of_rgb 102 102 102) ())
        grid ~x:indicator_x ~y:indicator_y ~text:scroll_block

(* ───── Resize ───── *)

let on_resize t _node = request t

(* ───── Measure ───── *)

let content_widths t =
  let width_method = Renderable.Private.width_method t.node in
  match t.content_widths with
  | Some ((cached_method, _, _) as widths) when cached_method = width_method ->
      widths
  | None | Some _ ->
      let label = ref 0 in
      let description = ref 0 in
      for i = 0 to Array.length t.items - 1 do
        let item = t.items.(i) in
        label :=
          max !label (Matrix.Text.measure ~width_method ~tab_width:2 item.label);
        match item.description with
        | None -> ()
        | Some text ->
            description :=
              max !description
                (Matrix.Text.measure ~width_method ~tab_width:2 text)
      done;
      let widths = (width_method, !label, !description) in
      t.content_widths <- Some widths;
      widths

let measured_dimension known available intrinsic =
  match known with
  | Some value -> value
  | None -> (
      match available with
      | Toffee.Available_space.Definite limit -> min intrinsic (max 0. limit)
      | Min_content | Max_content -> intrinsic)

let measure t ~known_dimensions ~available_space ~style:_ =
  let count = option_count t in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some width -> width
    | None ->
        let intrinsic =
          if count = 0 then 0.
          else
            let _, label, description = content_widths t in
            let content =
              if t.props.show_description then max label description else label
            in
            float_of_int
              (3 + content + if t.props.show_scroll_indicator then 1 else 0)
        in
        measured_dimension None available_space.Toffee.Geometry.Size.width
          intrinsic
  in
  let height =
    let intrinsic =
      if count = 0 then 0.
      else float_of_int ((count * lines_per_item t) - t.props.item_spacing)
    in
    measured_dimension known_dimensions.height available_space.height intrinsic
  in
  Toffee.Geometry.Size.{ width; height }

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?options
    ?selected_index ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_description
    ?show_scroll_indicator ?wrap_selection ?item_spacing ?fast_scroll_step
    ?key_bindings () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?options ?selected_index ?background ?text_color
      ?focused_background ?focused_text_color ?selected_background
      ?selected_text_color ?description_color ?selected_description_color
      ?show_description ?show_scroll_indicator ?wrap_selection ?item_spacing
      ?fast_scroll_step ?key_bindings ()
  in
  let items = Items.to_array props.options in
  let t =
    {
      node;
      props;
      items;
      selected_index = 0;
      content_widths = None;
      keymap = keymap props.key_bindings;
      on_change = None;
      on_activate = None;
    }
  in
  t.selected_index <- clamp_index t props.selected_index;
  (* Wire up rendering and event handlers. Buffered rendering avoids redundant
     redraws when multiple properties change in a single frame. *)
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
let set_selected_index t idx =
  let idx = clamp_index t idx in
  if idx <> t.selected_index then begin
    t.selected_index <- idx;
    request t
  end

let set_key_bindings t bindings =
  if not (key_bindings_equal t.props.key_bindings bindings) then begin
    t.props <- { t.props with key_bindings = bindings };
    t.keymap <- keymap bindings
  end

let apply_props t (props : Props.t) =
  (* Options equal to the current ones are kept, with the new source, which a
     later declaration of the same list is then the same as. *)
  if not (Items.same t.props.options props.options) then
    if Items.equal item_equal t.props.options props.options then
      t.props <- { t.props with options = props.options }
    else set_source t props.options;
  set_selected_index t props.selected_index;
  set_wrap_selection t props.wrap_selection;
  set_show_description t props.show_description;
  set_show_scroll_indicator t props.show_scroll_indicator;
  set_item_spacing t props.item_spacing;
  set_fast_scroll_step t props.fast_scroll_step;
  set_key_bindings t props.key_bindings;
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
