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

(* [Props.make] is the one place the select's labels get their
   defaults: [Mosaic.select] passes its labels on unset. *)
module Props = struct
  type t = {
    options : item Items.t;
    selected_index : int;
    background : Ansi.Color.t option;
    text_color : Ansi.Color.t option;
    focused_background : Ansi.Color.t option;
    focused_text_color : Ansi.Color.t option;
    selected_background : Ansi.Color.t option;
    selected_text_color : Ansi.Color.t option;
    description_color : Ansi.Color.t option;
    selected_description_color : Ansi.Color.t option;
    show_description : bool;
    show_scroll_indicator : bool;
    wrap_selection : bool;
    item_spacing : int;
    fast_scroll_step : int;
    key_bindings : action Keymap.binding list;
  }

  let make ?(options = []) ?(selected_index = 0) ?background ?text_color
      ?focused_background ?focused_text_color ?selected_background
      ?selected_text_color ?description_color ?selected_description_color
      ?(show_description = true) ?(show_scroll_indicator = false)
      ?(wrap_selection = false) ?(item_spacing = 0) ?(fast_scroll_step = 5)
      ?(key_bindings = []) () =
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
    && Option.equal Ansi.Color.equal a.background b.background
    && Option.equal Ansi.Color.equal a.text_color b.text_color
    && Option.equal Ansi.Color.equal a.focused_background b.focused_background
    && Option.equal Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Option.equal Ansi.Color.equal a.selected_background b.selected_background
    && Option.equal Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Option.equal Ansi.Color.equal a.description_color b.description_color
    && Option.equal Ansi.Color.equal a.selected_description_color
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

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let option_count t = Array.length t.items

let clamp_index t idx =
  let len = option_count t in
  if len = 0 then -1 else max (-1) (min (len - 1) idx)

(* The colors the labels give, the focused ones falling back to the
   others; a color no label gives is the role's. *)
let first_some a b = match a with Some _ -> a | None -> b

let effective_bg t focused =
  if focused then first_some t.props.focused_background t.props.background
  else t.props.background

let effective_fg t focused =
  if focused then first_some t.props.focused_text_color t.props.text_color
  else t.props.text_color

let labelled color style =
  match color with Some c -> Ansi.Style.fg c style | None -> style

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
  if not (Option.equal Ansi.Color.equal t.props.background (Some c)) then (
    t.props <- { t.props with background = Some c };
    request t)

let set_text_color t c =
  if not (Option.equal Ansi.Color.equal t.props.text_color (Some c)) then (
    t.props <- { t.props with text_color = Some c };
    request t)

let set_focused_background t c =
  if not (Option.equal Ansi.Color.equal t.props.focused_background (Some c))
  then (
    t.props <- { t.props with focused_background = Some c };
    request t)

let set_focused_text_color t c =
  if not (Option.equal Ansi.Color.equal t.props.focused_text_color (Some c))
  then (
    t.props <- { t.props with focused_text_color = Some c };
    request t)

let set_selected_background t c =
  if not (Option.equal Ansi.Color.equal t.props.selected_background (Some c))
  then (
    t.props <- { t.props with selected_background = Some c };
    request t)

let set_selected_text_color t c =
  if not (Option.equal Ansi.Color.equal t.props.selected_text_color (Some c))
  then (
    t.props <- { t.props with selected_text_color = Some c };
    request t)

let set_description_color t c =
  if not (Option.equal Ansi.Color.equal t.props.description_color (Some c)) then (
    t.props <- { t.props with description_color = Some c };
    request t)

let set_selected_description_color t c =
  if
    not
      (Option.equal Ansi.Color.equal t.props.selected_description_color (Some c))
  then (
    t.props <- { t.props with selected_description_color = Some c };
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

(* The select's paint, resolved against the theme in scope: labels first,
   then roles. Text is the [Plain] role, descriptions the [Muted] role, and
   the selected item's row the [Cursor] surface while focused, where it
   shows no surface unfocused; its mark, [cursor] or [cursor_inactive],
   shows the selected item and the focus whatever the styling. *)
let render t _self grid ~dt:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in
    let theme = Renderable.theme t.node in
    let glyphs = Theme.glyphs theme in
    let fg = labelled (effective_fg t focused) (Theme.style theme Plain) in
    let muted = Theme.style theme Muted in
    let base_bg =
      Option.value (effective_bg t focused) ~default:Ansi.Color.transparent
    in
    (* Clear the entire area first so spacing rows and empty regions below the
       last item don't show stale content. *)
    Matrix_grid.clear_rect ~color:base_bg grid ~x:0 ~y:0 ~width ~height;
    let row_surface =
      match t.props.selected_background with
      | Some bg -> Ansi.Style.make ~bg ()
      | None -> if focused then Theme.style theme Cursor else Ansi.Style.default
    in
    let selected_fg =
      labelled t.props.selected_text_color
        (Ansi.Style.merge ~base:row_surface ~overlay:fg)
    in
    let mark = if focused then glyphs.cursor else glyphs.cursor_inactive in
    let max_visible = max_visible_items t in
    let start_index = scroll_offset t in
    let end_index = min (option_count t) (start_index + max_visible) in
    for i = 0 to max_visible - 1 do
      let actual_index = start_index + i in
      if actual_index < end_index then
        let it = t.items.(actual_index) in
        let item_y = i * lines_per_item t in
        if item_y < height then (
          let is_selected = actual_index = t.selected_index in
          (* The row's surface covers only the content rows (label and
             description), not the spacing rows. *)
          let content_height = if t.props.show_description then 2 else 1 in
          if is_selected then
            for r = 0 to min content_height (height - item_y) - 1 do
              Matrix_grid.draw_text ~style:row_surface grid ~x:0 ~y:(item_y + r)
                ~text:(String.make width ' ')
            done;
          let style = if is_selected then selected_fg else fg in
          if is_selected then
            Matrix_grid.draw_text ~style grid ~x:1 ~y:item_y ~text:mark;
          Matrix_grid.draw_text ~style grid ~x:3 ~y:item_y ~text:it.label;
          if t.props.show_description then
            match it.description with
            | None -> ()
            | Some desc ->
                if item_y + 1 < height then
                  let style =
                    if is_selected then
                      labelled t.props.selected_description_color
                        (Ansi.Style.merge ~base:row_surface ~overlay:muted)
                    else labelled t.props.description_color muted
                  in
                  (* Description indented past the mark's column *)
                  Matrix_grid.draw_text ~style grid ~x:3 ~y:(item_y + 1)
                    ~text:desc)
    done;
    (* Scroll indicator: one thumb glyph in the rightmost column, placed
       proportionally to the selection within the list. *)
    if t.props.show_scroll_indicator && option_count t > max_visible then
      let len = max 1 (option_count t - 1) in
      let scroll_percent = float (max 0 t.selected_index) /. float len in
      (* Reserve first and last row, place indicator in the range [1,
         height-2] *)
      let indicator_height = max 1 (height - 2) in
      let indicator_y =
        1 + int_of_float (floor (scroll_percent *. float indicator_height))
      in
      Matrix_grid.draw_text ~style:muted grid ~x:(width - 1) ~y:indicator_y
        ~text:glyphs.thumb

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

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    (props : Props.t) =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
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
    request t
  end

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Select(%s, %d/%d" (Renderable.id t.node) t.selected_index
    (option_count t);
  if t.props.wrap_selection then Format.pp_print_string ppf ", wrap";
  Format.pp_print_char ppf ')'
