type cursor_style = [ `Block | `Line | `Underline ]

let default_max_length = 1000

module Props = struct
  type t = {
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    focused_background : Ansi.Color.t;
    focused_text_color : Ansi.Color.t;
    placeholder : string;
    placeholder_color : Ansi.Color.t;
    cursor_color : Ansi.Color.t;
    cursor_style : cursor_style;
    cursor_blinking : bool;
    max_length : int;
    value : string;
    autofocus : bool;
  }

  let make ?background ?text_color ?focused_background ?focused_text_color
      ?(placeholder = "") ?(placeholder_color = Ansi.Color.of_rgb 102 102 102)
      ?(cursor_color = Ansi.Color.of_rgb 255 255 255)
      ?(cursor_style = (`Block : cursor_style)) ?(cursor_blinking = true)
      ?(max_length = default_max_length) ?(value = "") ?(autofocus = false) () =
    let transparent = Ansi.Color.of_rgba 0 0 0 0 in
    let white = Ansi.Color.of_rgb 255 255 255 in
    let default_focused_bg = Ansi.Color.of_rgb 26 26 26 in
    let background_val = Option.value background ~default:transparent in
    let text_color_val = Option.value text_color ~default:white in
    let focused_background_val =
      match focused_background with
      | Some c -> c
      | None -> (
          match background with Some c -> c | None -> default_focused_bg)
    in
    let focused_text_color_val =
      match focused_text_color with
      | Some c -> c
      | None -> ( match text_color with Some c -> c | None -> white)
    in
    let max_length =
      if max_length <= 0 then default_max_length else max_length
    in
    {
      background = background_val;
      text_color = text_color_val;
      focused_background = focused_background_val;
      focused_text_color = focused_text_color_val;
      placeholder;
      placeholder_color;
      cursor_color;
      cursor_style;
      cursor_blinking;
      max_length;
      value;
      autofocus;
    }

  let default = make ()

  let equal a b =
    Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.focused_background b.focused_background
    && Ansi.Color.equal a.focused_text_color b.focused_text_color
    && String.equal a.placeholder b.placeholder
    && Ansi.Color.equal a.placeholder_color b.placeholder_color
    && Ansi.Color.equal a.cursor_color b.cursor_color
    && a.cursor_style = b.cursor_style
    && Bool.equal a.cursor_blinking b.cursor_blinking
    && Int.equal a.max_length b.max_length
    && String.equal a.value b.value
    && Bool.equal a.autofocus b.autofocus
end

let clamp v ~min ~max = if v < min then min else if v > max then max else v

let grapheme_count s =
  let count = ref 0 in
  Glyph.String.iter_graphemes (fun ~offset:_ ~len:_ -> incr count) s;
  !count

let byte_offset_of_index s index =
  if index <= 0 then 0
  else
    let len = String.length s in
    let result = ref len in
    let count = ref 0 in
    try
      Glyph.String.iter_graphemes
        (fun ~offset ~len:_ ->
          if !count = index then (
            result := offset;
            raise Exit)
          else incr count)
        s;
      !result
    with Exit -> !result

let clamp_index s idx = clamp idx ~min:0 ~max:(grapheme_count s)

let substring_by_graphemes s ~start ~stop =
  if start >= stop then ""
  else
    let start = clamp_index s start in
    let stop = clamp_index s stop in
    if start >= stop then ""
    else
      let byte_start = byte_offset_of_index s start in
      let byte_stop = byte_offset_of_index s stop in
      String.sub s byte_start (byte_stop - byte_start)

let insert_text_at s index text =
  let index = clamp_index s index in
  let prefix = substring_by_graphemes s ~start:0 ~stop:index in
  let suffix = substring_by_graphemes s ~start:index ~stop:(grapheme_count s) in
  prefix ^ text ^ suffix

let remove_range s ~start ~stop =
  let start = clamp_index s start in
  let stop = clamp_index s stop in
  if start >= stop then s
  else
    let prefix = substring_by_graphemes s ~start:0 ~stop:start in
    let suffix =
      substring_by_graphemes s ~start:stop ~stop:(grapheme_count s)
    in
    prefix ^ suffix

let width_of_grapheme widths line idx =
  let open Text_buffer.Virtual_line in
  let big_idx = line.start_index + idx in
  let width = Bigarray.Array1.unsafe_get widths big_idx in
  if width <= 0 then 1 else width

let rec width_between widths line start stop acc =
  if start >= stop then acc
  else
    let w = width_of_grapheme widths line start in
    width_between widths line (start + 1) stop (acc + w)

let placeholder_style color =
  Ansi.Style.make ~fg:color ~bg:(Ansi.Color.of_rgba 0 0 0 0) ()

type callbacks = {
  mutable on_input : (string -> unit) list;
  mutable on_change : (string -> unit) list;
  mutable on_submit : (string -> unit) list;
}

let callbacks () = { on_input = []; on_change = []; on_submit = [] }

let notify handlers variant value =
  let callbacks =
    match variant with
    | `Input -> handlers.on_input
    | `Change -> handlers.on_change
    | `Submit -> handlers.on_submit
  in
  List.iter (fun f -> f value) (List.rev callbacks)

type t = {
  surface : Text_surface.t;
  mutable props : Props.t;
  (* Last value provided via props; used to detect external value changes. *)
  mutable prop_value : string;
  mutable value : string;
  (* Cached grapheme count for the current [value] *)
  mutable graphemes : int;
  mutable cursor : int;
  mutable view_offset : int;
  mutable last_committed : string;
  callbacks : callbacks;
  mutable was_focused : bool;
  mutable buffer_dirty : bool;
  (* Cached placeholder truncation for the last width *)
  mutable placeholder_cached_source : string;
  mutable placeholder_cached_width : int;
  mutable placeholder_cached_text : string;
}

let clamp_index_cached t idx = clamp idx ~min:0 ~max:t.graphemes
let surface_node t = Text_surface.node t.surface
let node t = surface_node t
let request_render t = Text_surface.request_render t.surface
let text_buffer t = Text_surface.buffer t.surface
let text_view t = Text_surface.view t.surface

let update_buffer t =
  (* Refresh the underlying buffer content without triggering layout
     recalculation. Keep this cheap for frequent edits. *)
  let buffer = text_buffer t in
  Text_buffer.reset buffer;
  if t.value <> "" then (
    let chunk =
      Text_buffer.Chunk.
        {
          text = Bytes.of_string t.value;
          fg = Some t.props.text_color;
          bg = None;
          attrs = Ansi.Attr.empty;
          link = None;
        }
    in
    ignore (Text_buffer.write_chunk buffer chunk);
    t.buffer_dirty <- true)

let measure t ~known_dimensions ~available_space ~style:_ =
  ignore t;
  (* Respect layout hints; avoid content-based width which causes reflow on
     edits. *)
  let width_hint =
    match known_dimensions with
    | Toffee.Geometry.Size.{ width = Some w; _ } when w > 0. -> Some w
    | _ -> (
        match available_space with
        | Toffee.Geometry.Size.{ width; _ } ->
            Toffee.Available_space.to_option width)
  in
  let measured_width =
    match width_hint with
    | Some w when w > 0. -> Float.floor w |> int_of_float |> max 1
    | _ -> 1
  in
  Toffee.Geometry.Size.{ width = float measured_width; height = 1. }

let set_view_offset_for_cursor t line widths visible_width =
  let open Text_buffer.Virtual_line in
  let cursor = clamp t.cursor ~min:0 ~max:line.length in
  let current_offset = clamp t.view_offset ~min:0 ~max:cursor in
  let cursor_cells = width_between widths line 0 cursor 0 in
  let offset_cells = width_between widths line 0 current_offset 0 in
  let max_visible = max 1 visible_width in
  let offset = ref current_offset in
  let offset_cells_ref = ref offset_cells in
  while cursor_cells < !offset_cells_ref && !offset > 0 do
    offset := !offset - 1;
    offset_cells_ref := width_between widths line 0 !offset 0
  done;
  while cursor_cells - !offset_cells_ref >= max_visible do
    offset := !offset + 1;
    offset_cells_ref := width_between widths line 0 !offset 0
  done;
  t.view_offset <- clamp !offset ~min:0 ~max:cursor

let compute_placeholder t maxw =
  if t.props.placeholder = "" || maxw <= 0 then ""
  else if
    t.placeholder_cached_width = maxw
    && String.equal t.placeholder_cached_source t.props.placeholder
  then t.placeholder_cached_text
  else
    (* Build a substring up to available cell width in O(n). *)
    let placeholder = t.props.placeholder in
    let acc = ref 0 in
    let end_offset = ref 0 in
    (try
       Glyph.String.iter_graphemes
         (fun ~offset ~len ->
           let grapheme = String.sub placeholder offset len in
           let w =
             Glyph.String.measure ~width_method:`Unicode ~tab_width:2 grapheme
             |> max 1
           in
           if !acc + w <= maxw then (
             acc := !acc + w;
             end_offset := offset + len)
           else raise Exit)
         placeholder
     with Exit -> ());
    let text = String.sub placeholder 0 !end_offset in
    t.placeholder_cached_source <- placeholder;
    t.placeholder_cached_width <- maxw;
    t.placeholder_cached_text <- text;
    text

let draw_placeholder t ~x ~y ~width grid =
  if t.props.placeholder = "" then ()
  else
    let style = placeholder_style t.props.placeholder_color in
    let maxw = max 0 (width - 1) in
    let text = compute_placeholder t maxw in
    if text <> "" then Grid.draw_text ~style grid ~x ~y ~text

let notify_input t = notify t.callbacks `Input t.value
let notify_change t = notify t.callbacks `Change t.value
let notify_submit t = notify t.callbacks `Submit t.value
let focus t = Renderable.focus (surface_node t)

let blur t =
  (* Commit immediately on blur. *)
  if String.compare t.value t.last_committed <> 0 then (
    t.last_committed <- t.value;
    notify_change t);
  Renderable.blur (surface_node t)

let max_length_limit t =
  if t.props.max_length <= 0 then default_max_length else t.props.max_length

let truncate_to_max t value =
  let max_length = max_length_limit t in
  substring_by_graphemes value ~start:0 ~stop:max_length

let set_value_internal t value ~notify =
  let value = truncate_to_max t value in
  if String.equal value t.value then ()
  else (
    t.value <- value;
    t.graphemes <- grapheme_count value;
    (* Preserve caret; clamp within new bounds. *)
    t.cursor <- clamp_index_cached t t.cursor;
    update_buffer t;
    request_render t;
    if notify then notify_input t)

let set_value t value =
  (* Programmatic value set preserves caret (clamped). *)
  set_value_internal t value ~notify:true

let hardware_cursor t =
  let rnode = surface_node t in
  if not (Renderable.focused rnode) then None
  else
    let lx = Renderable.x rnode in
    let ly = Renderable.y rnode in
    let lw = Renderable.width rnode in
    let lh = Renderable.height rnode in
    if lw <= 0 || lh <= 0 then None
    else if t.value = "" then
      Some
        ( lx + 1,
          ly + 1,
          t.props.cursor_color,
          t.props.cursor_style,
          t.props.cursor_blinking )
    else
      let buffer = text_buffer t in
      Text_buffer.finalise buffer;
      let lines = Text_buffer_view.virtual_lines (text_view t) in
      if Array.length lines = 0 then
        Some
          ( lx + 1,
            ly + 1,
            t.props.cursor_color,
            t.props.cursor_style,
            t.props.cursor_blinking )
      else
        let open Text_buffer.Virtual_line in
        let line = lines.(0) in
        let widths = Text_buffer.drawing_widths buffer in
        let cursor_index = clamp t.cursor ~min:0 ~max:line.length in
        let cursor_column =
          width_between widths line t.view_offset cursor_index 0
        in
        let dest_x = lx + cursor_column in
        Some
          ( dest_x + 1,
            ly + 1,
            t.props.cursor_color,
            t.props.cursor_style,
            t.props.cursor_blinking )

let insert_text t text =
  if text <> "" then
    let current = t.graphemes in
    let inserted = grapheme_count text in
    let capacity =
      let limit = max_length_limit t in
      limit - current
    in
    if capacity <= 0 then ()
    else
      let to_insert = if inserted <= capacity then text else "" in
      if to_insert <> "" then (
        let new_value = insert_text_at t.value t.cursor to_insert in
        set_value_internal t new_value ~notify:true;
        t.cursor <- clamp_index_cached t (t.cursor + grapheme_count to_insert))

let delete_backward t =
  if t.cursor > 0 then (
    let original_cursor = t.cursor in
    let new_value =
      remove_range t.value ~start:(original_cursor - 1) ~stop:original_cursor
    in
    set_value_internal t new_value ~notify:true;
    t.cursor <- clamp_index_cached t (original_cursor - 1))

let delete_forward t =
  if t.cursor < t.graphemes then (
    let new_value = remove_range t.value ~start:t.cursor ~stop:(t.cursor + 1) in
    set_value_internal t new_value ~notify:true;
    ())

let commit_value t =
  if String.compare t.value t.last_committed <> 0 then (
    t.last_committed <- t.value;
    notify_change t)

let handle_key t (event : Event.key) =
  let event = Event.Key.data event in
  let is_ascii_printable s =
    let len = String.length s in
    if len <> 1 then false
    else
      let c = int_of_char s.[0] in
      c >= 32 && c <= 126
  in
  (* Only handle Press/Repeat; ignore Release. *)
  match event.event_type with
  | Release -> false
  | Press | Repeat -> (
      match event.key with
      | Char _uchar
        when (not event.modifier.ctrl) && (not event.modifier.alt)
             && (not event.modifier.meta)
             && event.associated_text <> "" ->
          if is_ascii_printable event.associated_text then
            insert_text t event.associated_text;
          true
      | Char uchar
        when (not event.modifier.ctrl) && (not event.modifier.alt)
             && not event.modifier.meta ->
          let buf = Stdlib.Buffer.create 4 in
          Stdlib.Buffer.add_utf_8_uchar buf uchar;
          let s = Stdlib.Buffer.contents buf in
          if is_ascii_printable s then insert_text t s;
          true
      | Backspace ->
          delete_backward t;
          true
      | Delete ->
          delete_forward t;
          true
      | Left ->
          t.cursor <- clamp_index_cached t (t.cursor - 1);
          request_render t;
          true
      | Right ->
          t.cursor <- clamp_index_cached t (t.cursor + 1);
          request_render t;
          true
      | Home ->
          t.cursor <- 0;
          request_render t;
          true
      | End ->
          t.cursor <- t.graphemes;
          request_render t;
          true
      | Enter | KP_enter | Line_feed ->
          commit_value t;
          notify_submit t;
          true
      | _ -> false)

let render_input t renderable grid ~delta:_ =
  (* Use local buffer coordinates (0,0) for buffered rendering. *)
  let lx = 0 in
  let ly = 0 in
  let lw = Renderable.width renderable in
  let lh = Renderable.height renderable in
  if lw <= 0 || lh <= 0 then ()
  else
    let focused = Renderable.focused renderable in
    if focused <> t.was_focused then (
      if (not focused) && t.was_focused then commit_value t;
      t.was_focused <- focused);
    let buffer = text_buffer t in
    let view = text_view t in
    if t.buffer_dirty then (
      Text_buffer_view.set_wrap_mode view `Char;
      Text_buffer_view.set_wrap_width view None;
      Text_buffer.finalise buffer;
      t.buffer_dirty <- false);
    (* Align buffer width method with grid when buffer is empty (safe to
       change). *)
    (if Text_buffer.length buffer = 0 then
       let gwm = Grid.width_method grid in
       Text_buffer.set_width_method buffer gwm);
    let bg =
      if focused then t.props.focused_background else t.props.background
    in
    Grid.fill_rect grid ~x:lx ~y:ly ~width:lw ~height:lh ~color:bg;
    if t.value = "" then draw_placeholder t ~x:lx ~y:ly ~width:lw grid
    else
      let lines = Text_buffer_view.virtual_lines view in
      if Array.length lines > 0 then (
        let open Text_buffer.Virtual_line in
        let line = lines.(0) in
        let widths = Text_buffer.drawing_widths buffer in
        let content_width = max 0 (lw - 1) in
        set_view_offset_for_cursor t line widths content_width;
        let chars = Text_buffer.drawing_chars buffer in
        let rec loop i column =
          if i >= line.length || column >= content_width then ()
          else
            let idx = line.start_index + i in
            let code = Bigarray.Array1.unsafe_get chars idx in
            let width = width_of_grapheme widths line i in
            let dest_x = lx + column in
            let dest_y = ly in
            if dest_x >= 0 && dest_x < Grid.width grid then (
              let fg =
                if focused then t.props.focused_text_color
                else t.props.text_color
              in
              Grid.set_cell grid ~x:dest_x ~y:dest_y
                ~glyph:(Glyph.unsafe_of_int code) ~fg ~bg ~attrs:Ansi.Attr.empty
                ~blend:true ();
              loop (i + 1) (column + max 1 width))
        in
        loop t.view_offset 0)

let mount ?(props = Props.default) (rnode : Renderable.t) =
  let default_style = Ansi.Style.make ~fg:props.text_color () in
  let surface =
    Text_surface.mount
      ~props:(Text_surface.Props.make ~wrap_mode:`Char ~default_style ())
      rnode
  in
  let callbacks = callbacks () in
  let input =
    {
      surface;
      props;
      prop_value = props.value;
      value = props.value;
      graphemes = grapheme_count props.value;
      cursor = grapheme_count props.value;
      view_offset = 0;
      last_committed = props.value;
      callbacks;
      was_focused = false;
      buffer_dirty = true;
      placeholder_cached_source = "";
      placeholder_cached_width = -1;
      placeholder_cached_text = "";
    }
  in
  update_buffer input;
  let renderable = surface_node input in
  Renderable.set_render renderable (render_input input);
  Renderable.set_measure renderable (Some (measure input));
  Renderable.set_buffer renderable `Self;
  Renderable.set_focusable renderable true;
  (* Register the default key handler in the third tier so user on_key handlers
     (tier 2) can call key_prevent_default to suppress it. When we handle a key,
     mark the event as consumed via prevent_default so that TEA subscriptions
     using on_key (not on_key_all) will skip it. *)
  Renderable.set_default_key_handler renderable
    (Some
       (fun event ->
         if handle_key input event then Event.Key.prevent_default event));
  Renderable.set_hardware_cursor_provider renderable
    (Some
       (fun _ ->
         match hardware_cursor input with
         | None -> None
         | Some (x, y, color, style, blinking) ->
             Some { Renderable.x; y; color; style; blinking }));
  (match input.props.autofocus with
  | true -> ignore (Renderable.focus renderable)
  | false -> ());
  request_render input;
  input

let value t = t.value
let cursor t = t.cursor

let set_placeholder t placeholder =
  if t.props.placeholder <> placeholder then (
    t.props <- { t.props with placeholder };
    (* Invalidate cached placeholder rendering *)
    t.placeholder_cached_source <- "";
    t.placeholder_cached_width <- -1;
    t.placeholder_cached_text <- "";
    request_render t)

let set_cursor t index =
  let index = clamp_index_cached t index in
  if t.cursor <> index then (
    t.cursor <- index;
    request_render t)

let set_max_length t max_length =
  let normalized = if max_length <= 0 then default_max_length else max_length in
  t.props <- { t.props with max_length = normalized };
  if t.graphemes > normalized then
    let truncated = substring_by_graphemes t.value ~start:0 ~stop:normalized in
    (* Truncate without emitting INPUT. *)
    set_value_internal t truncated ~notify:false

let set_background t color =
  if not (Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    request_render t)

let set_focused_background t color =
  if not (Ansi.Color.equal t.props.focused_background color) then (
    t.props <- { t.props with focused_background = color };
    request_render t)

let set_text_color t color =
  if not (Ansi.Color.equal t.props.text_color color) then (
    t.props <- { t.props with text_color = color };
    Text_surface.set_default_style t.surface (Ansi.Style.make ~fg:color ());
    update_buffer t;
    request_render t)

let set_focused_text_color t color =
  if not (Ansi.Color.equal t.props.focused_text_color color) then (
    t.props <- { t.props with focused_text_color = color };
    request_render t)

let set_placeholder_color t color =
  if not (Ansi.Color.equal t.props.placeholder_color color) then (
    t.props <- { t.props with placeholder_color = color };
    request_render t)

let set_cursor_color t color =
  if not (Ansi.Color.equal t.props.cursor_color color) then (
    t.props <- { t.props with cursor_color = color };
    (* Only request a render when focused, as cursor color affects the hardware
       cursor. *)
    if Renderable.focused (surface_node t) then request_render t)

let set_cursor_style t style =
  if t.props.cursor_style <> style then (
    t.props <- { t.props with cursor_style = style };
    (* Only request a render when focused. *)
    if Renderable.focused (surface_node t) then request_render t)

let set_cursor_blinking t blinking =
  if t.props.cursor_blinking <> blinking then (
    t.props <- { t.props with cursor_blinking = blinking };
    (* Only request a render when focused. *)
    if Renderable.focused (surface_node t) then request_render t)

let set_callbacks t ?on_input ?on_change ?on_submit () =
  let to_list = function None -> [] | Some f -> [ f ] in
  t.callbacks.on_input <- to_list on_input;
  t.callbacks.on_change <- to_list on_change;
  t.callbacks.on_submit <- to_list on_submit

let on_input t handler = t.callbacks.on_input <- handler :: t.callbacks.on_input

let on_change t handler =
  t.callbacks.on_change <- handler :: t.callbacks.on_change

let on_submit t handler =
  t.callbacks.on_submit <- handler :: t.callbacks.on_submit

let apply_props t (props : Props.t) =
  if t.props.autofocus <> props.autofocus then
    t.props <- { t.props with autofocus = props.autofocus };
  (* Colors and background *)
  set_background t props.background;
  set_text_color t props.text_color;
  set_focused_background t props.focused_background;
  set_focused_text_color t props.focused_text_color;
  (* Placeholder and its color *)
  set_placeholder t props.placeholder;
  set_placeholder_color t props.placeholder_color;
  (* Cursor appearance *)
  set_cursor_color t props.cursor_color;
  set_cursor_style t props.cursor_style;
  set_cursor_blinking t props.cursor_blinking;
  (* Length limit and value *)
  set_max_length t props.max_length;
  if not (String.equal props.value t.prop_value) then (
    t.prop_value <- props.value;
    set_value t props.value)
