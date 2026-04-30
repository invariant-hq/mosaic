(* Defaults *)

let default_text_color = Ansi.Color.white
let default_background_color = Ansi.Color.default
let default_focused_text_color = Ansi.Color.white
let default_focused_background_color = Ansi.Color.default
let default_placeholder_color = Ansi.Color.bright_black
let default_selection_color = Ansi.Color.blue
let default_cursor_style = `Block
let default_cursor_color = Ansi.Color.white
let default_cursor_blinking = true
let default_ghost_text_color = Ansi.Color.grayscale ~level:12

(* Props *)

type mode = [ `Multiline | `Single_line ]

module Action = struct
  type t =
    | Move_left
    | Move_right
    | Move_up
    | Move_down
    | Select_left
    | Select_right
    | Select_up
    | Select_down
    | Line_home
    | Line_end
    | Select_line_home
    | Select_line_end
    | Visual_line_home
    | Visual_line_end
    | Select_visual_line_home
    | Select_visual_line_end
    | Buffer_home
    | Buffer_end
    | Select_buffer_home
    | Select_buffer_end
    | Delete_line
    | Delete_to_line_end
    | Delete_to_line_start
    | Backspace
    | Delete
    | Newline
    | Undo
    | Redo
    | Word_forward
    | Word_backward
    | Select_word_forward
    | Select_word_backward
    | Delete_word_forward
    | Delete_word_backward
    | Select_all
    | Submit

  let b = Keymap.binding

  let defaults =
    [
      b "left" Move_left;
      b "right" Move_right;
      b "up" Move_up;
      b "down" Move_down;
      b ~shift:true "left" Select_left;
      b ~shift:true "right" Select_right;
      b ~shift:true "up" Select_up;
      b ~shift:true "down" Select_down;
      b "home" Buffer_home;
      b "end" Buffer_end;
      b ~shift:true "home" Select_buffer_home;
      b ~shift:true "end" Select_buffer_end;
      b ~ctrl:true "a" Line_home;
      b ~ctrl:true "e" Line_end;
      b ~ctrl:true ~shift:true "a" Select_line_home;
      b ~ctrl:true ~shift:true "e" Select_line_end;
      b ~alt:true "a" Visual_line_home;
      b ~alt:true "e" Visual_line_end;
      b ~alt:true ~shift:true "a" Select_visual_line_home;
      b ~alt:true ~shift:true "e" Select_visual_line_end;
      b ~ctrl:true "f" Move_right;
      b ~ctrl:true "b" Move_left;
      b ~ctrl:true "w" Delete_word_backward;
      b ~ctrl:true "backspace" Delete_word_backward;
      b ~alt:true "d" Delete_word_forward;
      b ~alt:true "delete" Delete_word_forward;
      b ~ctrl:true "delete" Delete_word_forward;
      b ~ctrl:true ~shift:true "d" Delete_line;
      b ~ctrl:true "k" Delete_to_line_end;
      b ~ctrl:true "u" Delete_to_line_start;
      b "backspace" Backspace;
      b ~shift:true "backspace" Backspace;
      b ~ctrl:true "d" Delete;
      b "delete" Delete;
      b ~shift:true "delete" Delete;
      b "return" Newline;
      b "linefeed" Newline;
      b ~alt:true "return" Submit;
      b ~ctrl:true "return" Submit;
      b ~super:true "return" Submit;
      b ~alt:true "linefeed" Submit;
      b ~ctrl:true "linefeed" Submit;
      b ~super:true "linefeed" Submit;
      b ~ctrl:true "-" Undo;
      b ~ctrl:true "." Redo;
      b ~ctrl:true "z" Undo;
      b ~ctrl:true ~shift:true "z" Redo;
      b ~super:true "z" Undo;
      b ~super:true ~shift:true "z" Redo;
      b ~alt:true "f" Word_forward;
      b ~alt:true "b" Word_backward;
      b ~alt:true "right" Word_forward;
      b ~alt:true "left" Word_backward;
      b ~ctrl:true "right" Word_forward;
      b ~ctrl:true "left" Word_backward;
      b ~alt:true ~shift:true "f" Select_word_forward;
      b ~alt:true ~shift:true "b" Select_word_backward;
      b ~alt:true ~shift:true "right" Select_word_forward;
      b ~alt:true ~shift:true "left" Select_word_backward;
      b ~alt:true "backspace" Delete_word_backward;
      b ~super:true "left" Visual_line_home;
      b ~super:true "right" Visual_line_end;
      b ~super:true "up" Buffer_home;
      b ~super:true "down" Buffer_end;
      b ~super:true ~shift:true "left" Select_visual_line_home;
      b ~super:true ~shift:true "right" Select_visual_line_end;
      b ~super:true ~shift:true "up" Select_buffer_home;
      b ~super:true ~shift:true "down" Select_buffer_end;
      b ~super:true "a" Select_all;
    ]

  let input_overrides = [ b "return" Submit; b "linefeed" Submit ]

  let keymap = function
    | `Multiline -> Keymap.make ~defaults ()
    | `Single_line -> Keymap.make ~defaults ~custom:input_overrides ()
end

module Props = struct
  type t = {
    value : string;
    cursor : int option;
    selection : (int * int) option option;
    spans : Text_buffer.span list;
    ghost_text : string option;
    ghost_text_color : Ansi.Color.t;
    placeholder : string;
    wrap : Text_surface.wrap;
    text_color : Ansi.Color.t;
    background_color : Ansi.Color.t;
    focused_text_color : Ansi.Color.t;
    focused_background_color : Ansi.Color.t;
    placeholder_color : Ansi.Color.t;
    selection_color : Ansi.Color.t;
    selection_fg : Ansi.Color.t option;
    cursor_style : [ `Block | `Line | `Underline ];
    cursor_color : Ansi.Color.t;
    cursor_blinking : bool;
    selectable : bool;
    show_cursor : bool;
  }

  let make ?(value = "") ?cursor ?selection ?(spans = []) ?ghost_text
      ?(ghost_text_color = default_ghost_text_color) ?(placeholder = "")
      ?(wrap = `Word) ?(text_color = default_text_color)
      ?(background_color = default_background_color)
      ?(focused_text_color = default_focused_text_color)
      ?(focused_background_color = default_focused_background_color)
      ?(placeholder_color = default_placeholder_color)
      ?(selection_color = default_selection_color) ?selection_fg
      ?(cursor_style = default_cursor_style)
      ?(cursor_color = default_cursor_color)
      ?(cursor_blinking = default_cursor_blinking) ?(selectable = true)
      ?(show_cursor = true) () =
    {
      value;
      cursor;
      selection;
      spans;
      ghost_text;
      ghost_text_color;
      placeholder;
      wrap;
      text_color;
      background_color;
      focused_text_color;
      focused_background_color;
      placeholder_color;
      selection_color;
      selection_fg;
      cursor_style;
      cursor_color;
      cursor_blinking;
      selectable;
      show_cursor;
    }

  let default = make ()

  let spans_equal a b =
    List.compare_length_with a (List.length b) = 0
    && List.for_all2
         (fun (a : Text_buffer.span) (b : Text_buffer.span) ->
           String.equal a.text b.text && Ansi.Style.equal a.style b.style)
         a b

  let equal a b =
    String.equal a.value b.value
    && Option.equal Int.equal a.cursor b.cursor
    && Option.equal
         (Option.equal (fun (a1, a2) (b1, b2) -> a1 = b1 && a2 = b2))
         a.selection b.selection
    && spans_equal a.spans b.spans
    && Option.equal String.equal a.ghost_text b.ghost_text
    && Ansi.Color.equal a.ghost_text_color b.ghost_text_color
    && String.equal a.placeholder b.placeholder
    && a.wrap = b.wrap
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.background_color b.background_color
    && Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Ansi.Color.equal a.focused_background_color b.focused_background_color
    && Ansi.Color.equal a.placeholder_color b.placeholder_color
    && Ansi.Color.equal a.selection_color b.selection_color
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && a.cursor_style = b.cursor_style
    && Ansi.Color.equal a.cursor_color b.cursor_color
    && a.cursor_blinking = b.cursor_blinking
    && a.selectable = b.selectable
    && a.show_cursor = b.show_cursor
end

(* Types *)

type t = {
  node : Renderable.t;
  buf : Edit_buffer.t;
  text_buf : Text_buffer.t;
  surface : Text_surface.t;
  mutable props : Props.t;
  mutable was_focused : bool;
  mutable last_committed_value : string;
  mutable preferred_col : int option;
  mutable on_input : (string -> unit) option;
  mutable on_change : (string -> unit) option;
  mutable on_submit : (string -> unit) option;
  mutable on_cursor :
    (cursor:int -> selection:(int * int) option -> unit) option;
  mutable last_cursor : int;
  mutable last_selection : (int * int) option;
  mode : mode;
  keymap : Action.t Keymap.t;
}

(* Accessors *)

let node t = t.node
let buffer t = t.buf
let surface t = t.surface
let value t = Edit_buffer.text t.buf
let cursor t = Edit_buffer.cursor t.buf
let selection t = Edit_buffer.selection t.buf

let sanitize_text t s =
  match t.mode with
  | `Multiline -> s
  | `Single_line -> Edit_buffer.strip_newlines s

(* Line info *)

let register_line_info t =
  Renderable.set_line_info_provider t.node
    (Some
       (fun () ->
         let di = Text_surface.display_info t.surface in
         {
           Renderable.line_count = Edit_buffer.line_count t.buf;
           display_line_count = Array.length di.lines;
           line_sources = di.line_sources;
           line_wrap_indices = di.line_wrap_indices;
           scroll_y = Text_surface.scroll_y t.surface;
         }))

(* Callbacks *)

let set_on_input t h = t.on_input <- h
let set_on_change t h = t.on_change <- h
let set_on_submit t h = t.on_submit <- h
let set_on_cursor t h = t.on_cursor <- h
let fire_on_input t = match t.on_input with Some f -> f (value t) | None -> ()

let fire_on_change t =
  let v = value t in
  if not (String.equal v t.last_committed_value) then begin
    t.last_committed_value <- v;
    match t.on_change with Some f -> f v | None -> ()
  end

let fire_on_submit t =
  fire_on_change t;
  match t.on_submit with Some f -> f (value t) | None -> ()

let fire_on_cursor t =
  let c = cursor t in
  let s = selection t in
  if c <> t.last_cursor || s <> t.last_selection then begin
    t.last_cursor <- c;
    t.last_selection <- s;
    (match t.on_cursor with Some f -> f ~cursor:c ~selection:s | None -> ());
    true
  end
  else false

let apply_selection t sel_range =
  let len = Edit_buffer.length t.buf in
  let normalize (a, b) =
    let clamp x = Int.max 0 (Int.min len x) in
    let lo = clamp (Int.min a b) in
    let hi = clamp (Int.max a b) in
    if lo < hi then Some (lo, hi) else None
  in
  let before_cursor = cursor t in
  let before_selection = selection t in
  (match normalize sel_range with
  | None ->
      if Option.is_some before_selection then
        Edit_buffer.set_cursor t.buf before_cursor
  | Some (lo, hi) ->
      Edit_buffer.set_cursor t.buf lo;
      Edit_buffer.set_cursor_offset ~select:true t.buf hi);
  before_cursor <> cursor t || before_selection <> selection t

(* Sync *)

let spans_text_equals spans text =
  let total =
    List.fold_left
      (fun acc (span : Text_buffer.span) -> acc + String.length span.text)
      0 spans
  in
  if total <> String.length text then false
  else
    let buf = Buffer.create total in
    List.iter
      (fun (span : Text_buffer.span) -> Buffer.add_string buf span.text)
      spans;
    String.equal (Buffer.contents buf) text

let sync_content t =
  let text = Edit_buffer.text t.buf in
  if t.props.spans <> [] && spans_text_equals t.props.spans text then
    Text_buffer.set_styled_text t.text_buf t.props.spans
  else Text_buffer.set_text t.text_buf text

let sync t =
  sync_content t;
  Text_surface.invalidate t.surface

let sync_style t ~focused =
  let fg = if focused then t.props.focused_text_color else t.props.text_color in
  let bg =
    if focused then t.props.focused_background_color
    else t.props.background_color
  in
  Text_buffer.set_default_style t.text_buf (Ansi.Style.make ~fg ~bg ());
  sync_content t;
  Text_surface.invalidate t.surface

(* Display line mapping *)

let find_cursor_display_line t =
  let di = Text_surface.display_info t.surface in
  let n = Array.length di.line_grapheme_offsets in
  if n = 0 then 0
  else
    let cursor = Edit_buffer.cursor t.buf in
    let lo = ref 0 in
    let hi = ref (n - 1) in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo + 1) / 2) in
      if di.line_grapheme_offsets.(mid) <= cursor then lo := mid
      else hi := mid - 1
    done;
    !lo

let cursor_visual_col t display_line =
  let di = Text_surface.display_info t.surface in
  let line_start = di.line_grapheme_offsets.(display_line) in
  let cursor = Edit_buffer.cursor t.buf in
  let count = cursor - line_start in
  if count <= 0 then 0
  else
    let text =
      Text_buffer.text_in_range t.text_buf ~start:line_start ~len:count
    in
    let tab_width = Text_buffer.tab_width t.text_buf in
    let width_method = Text_buffer.width_method t.text_buf in
    Matrix.Text.measure ~width_method ~tab_width text

let display_line_end_offset t display_line =
  let di = Text_surface.display_info t.surface in
  let n = Array.length di.line_grapheme_offsets in
  let next_start =
    if display_line + 1 < n then di.line_grapheme_offsets.(display_line + 1)
    else Text_buffer.grapheme_count t.text_buf
  in
  if
    display_line + 1 < n
    && di.line_sources.(display_line) <> di.line_sources.(display_line + 1)
  then next_start - 1
  else next_start

let offset_at_col t target_line target_col =
  let di = Text_surface.display_info t.surface in
  let line_start = di.line_grapheme_offsets.(target_line) in
  let line_end = display_line_end_offset t target_line in
  let count = line_end - line_start in
  if count <= 0 then line_start
  else
    let text =
      Text_buffer.text_in_range t.text_buf ~start:line_start ~len:count
    in
    let tab_width = Text_buffer.tab_width t.text_buf in
    let width_method = Text_buffer.width_method t.text_buf in
    let result = ref line_start in
    let col = ref 0 in
    Matrix.Text.iter_grapheme_info ~width_method ~tab_width
      (fun ~offset:_ ~len:_ ~width ->
        if !col + width <= target_col then begin
          col := !col + width;
          incr result
        end)
      text;
    !result

(* Scroll *)

let ensure_cursor_visible t =
  let h = Renderable.height t.node in
  if h > 0 then begin
    let dl = find_cursor_display_line t in
    let sy = Text_surface.scroll_y t.surface in
    if dl < sy then Text_surface.set_scroll_y t.surface dl
    else if dl >= sy + h then Text_surface.set_scroll_y t.surface (dl - h + 1)
  end;
  if Text_surface.wrap t.surface = `None then begin
    let w = Renderable.width t.node in
    if w > 0 then begin
      let dl = find_cursor_display_line t in
      let cc = cursor_visual_col t dl in
      let sx = Text_surface.scroll_x t.surface in
      if cc < sx then Text_surface.set_scroll_x t.surface cc
      else if cc >= sx + w then
        Text_surface.set_scroll_x_for_cursor t.surface (cc - w + 1)
    end
  end

let handle_scroll t direction delta =
  let delta = Int.max 0 delta in
  match direction with
  | Input.Mouse.Scroll_up ->
      Text_surface.set_scroll_y t.surface
        (Text_surface.scroll_y t.surface - delta)
  | Scroll_down ->
      Text_surface.set_scroll_y t.surface
        (Text_surface.scroll_y t.surface + delta)
  | Scroll_left when Text_surface.wrap t.surface = `None ->
      Text_surface.set_scroll_x t.surface
        (Text_surface.scroll_x t.surface - delta)
  | Scroll_right when Text_surface.wrap t.surface = `None ->
      Text_surface.set_scroll_x t.surface
        (Text_surface.scroll_x t.surface + delta)
  | Scroll_left | Scroll_right -> ()

let measure_single_line t ~known_dimensions ~available_space:_ ~style:_ =
  let content_width = Edit_buffer.display_width t.buf in
  let placeholder_width =
    Matrix.Text.measure ~width_method:`Unicode ~tab_width:2 t.props.placeholder
  in
  let intrinsic_width = Float.of_int (max content_width placeholder_width) in
  let width =
    match known_dimensions.Toffee.Geometry.Size.width with
    | Some w -> w
    | None -> intrinsic_width
  in
  let height =
    match known_dimensions.Toffee.Geometry.Size.height with
    | Some h -> h
    | None -> 1.0
  in
  Toffee.Geometry.Size.make width height

(* Rendering *)

let render_before t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else begin
    let x0 = Renderable.x t.node in
    let y0 = Renderable.y t.node in
    let focused = Renderable.focused t.node in
    if focused && not t.was_focused then begin
      t.last_committed_value <- value t;
      sync_style t ~focused:true
    end
    else if (not focused) && t.was_focused then begin
      fire_on_change t;
      sync_style t ~focused:false
    end;
    t.was_focused <- focused;
    let bg =
      if focused then t.props.focused_background_color
      else t.props.background_color
    in
    Grid.clear_rect ~color:bg grid ~x:x0 ~y:y0 ~width:w ~height:h;
    if Edit_buffer.is_empty t.buf && String.length t.props.placeholder > 0 then begin
      let style = Ansi.Style.make ~fg:t.props.placeholder_color ~bg () in
      Grid.clip grid { x = x0; y = y0; width = w; height = h } (fun () ->
          Grid.draw_text ~style grid ~x:x0 ~y:y0 ~text:t.props.placeholder)
    end;
    match Edit_buffer.selection t.buf with
    | Some (lo, hi) ->
        Text_surface.set_selection_bg t.surface (Some t.props.selection_color);
        Text_surface.set_selection_fg t.surface t.props.selection_fg;
        ignore (Text_surface.set_selection t.surface ~start:lo ~end_:hi : bool)
    | None -> Text_surface.reset_selection t.surface
  end

let render_after t _self grid ~delta:_ =
  if not (Renderable.focused t.node) then ()
  else
    match t.props.ghost_text with
    | None -> ()
    | Some ghost
      when String.length ghost = 0
           || Option.is_some (Edit_buffer.selection t.buf) ->
        ()
    | Some ghost ->
        let dl = find_cursor_display_line t in
        let sy = Text_surface.scroll_y t.surface in
        let sx = Text_surface.scroll_x t.surface in
        let row = dl - sy in
        let col = cursor_visual_col t dl - sx in
        let x0 = Renderable.x t.node in
        let y0 = Renderable.y t.node in
        let w = Renderable.width t.node in
        let h = Renderable.height t.node in
        if row >= 0 && row < h && col >= 0 && col < w then
          let bg =
            if Renderable.focused t.node then t.props.focused_background_color
            else t.props.background_color
          in
          let style =
            Ansi.Style.make ~fg:t.props.ghost_text_color ~bg ~italic:true ()
          in
          Grid.clip grid { x = x0; y = y0; width = w; height = h } (fun () ->
              Grid.draw_text ~style grid ~x:(x0 + col) ~y:(y0 + row) ~text:ghost)

(* Cursor *)

let cursor_provider t _self =
  if (not t.props.show_cursor) || not (Renderable.focused t.node) then None
  else
    let dl = find_cursor_display_line t in
    let sy = Text_surface.scroll_y t.surface in
    let sx = Text_surface.scroll_x t.surface in
    let row = dl - sy in
    let col = cursor_visual_col t dl - sx in
    let x0 = Renderable.x t.node in
    let y0 = Renderable.y t.node in
    let w = Renderable.width t.node in
    let h = Renderable.height t.node in
    if row >= 0 && row < h && col >= 0 && col < w then
      Some
        {
          Renderable.x = x0 + col;
          y = y0 + row;
          style = t.props.cursor_style;
          color = t.props.cursor_color;
          blinking = t.props.cursor_blinking;
        }
    else None

(* Selection *)

let sync_buffer_selection_from_surface t =
  match Text_surface.selection t.surface with
  | None -> Edit_buffer.clear_selection t.buf
  | Some (lo, hi) ->
      Edit_buffer.set_cursor t.buf lo;
      Edit_buffer.set_cursor_offset ~select:true t.buf hi

let register_selection t =
  if t.props.selectable then
    Renderable.set_selection t.node
      ~should_start:(fun ~x ~y ->
        let nx = Renderable.x t.node in
        let ny = Renderable.y t.node in
        let w = Renderable.width t.node in
        let h = Renderable.height t.node in
        x >= nx && x < nx + w && y >= ny && y < ny + h)
      ~on_change:(fun sel ->
        match sel with
        | None ->
            Text_surface.reset_selection t.surface;
            Edit_buffer.clear_selection t.buf;
            ignore (fire_on_cursor t : bool);
            true
        | Some sel ->
            let nx = Renderable.x t.node in
            let ny = Renderable.y t.node in
            let anchor = Selection.anchor sel in
            let focus = Selection.focus sel in
            let ax = anchor.x - nx and ay = anchor.y - ny in
            let fx = focus.x - nx and fy = focus.y - ny in
            let changed =
              if Selection.is_start sel then
                Text_surface.set_local_selection t.surface ~anchor_x:ax
                  ~anchor_y:ay ~focus_x:fx ~focus_y:fy
              else
                Text_surface.update_local_selection t.surface ~anchor_x:ax
                  ~anchor_y:ay ~focus_x:fx ~focus_y:fy
            in
            if changed then begin
              sync_buffer_selection_from_surface t;
              ignore (fire_on_cursor t : bool);
              ensure_cursor_visible t;
              Renderable.request_render t.node
            end;
            Edit_buffer.has_selection t.buf)
      ~clear:(fun () ->
        Text_surface.reset_selection t.surface;
        Edit_buffer.clear_selection t.buf;
        ignore (fire_on_cursor t : bool))
      ~get_text:(fun () -> Edit_buffer.selected_text t.buf)
  else begin
    Text_surface.reset_selection t.surface;
    Edit_buffer.clear_selection t.buf;
    ignore (fire_on_cursor t : bool);
    Renderable.unset_selection t.node
  end

(* Vertical movement *)

let move_vertical t ~select ~delta =
  let di = Text_surface.display_info t.surface in
  let n = Array.length di.line_grapheme_offsets in
  if n > 0 then begin
    let dl = find_cursor_display_line t in
    let target_line = dl + delta in
    if target_line >= 0 && target_line < n then begin
      let col =
        match t.preferred_col with
        | Some c -> c
        | None ->
            let c = cursor_visual_col t dl in
            t.preferred_col <- Some c;
            c
      in
      let target = offset_at_col t target_line col in
      Edit_buffer.set_cursor_offset ~select t.buf target;
      true
    end
    else false
  end
  else false

(* Visual line navigation *)

let move_visual_line_start t ~select =
  let di = Text_surface.display_info t.surface in
  let n = Array.length di.line_grapheme_offsets in
  if n > 0 then begin
    let dl = find_cursor_display_line t in
    let target = di.line_grapheme_offsets.(dl) in
    Edit_buffer.set_cursor_offset ~select t.buf target;
    true
  end
  else false

let move_visual_line_end t ~select =
  let di = Text_surface.display_info t.surface in
  let n = Array.length di.line_grapheme_offsets in
  if n > 0 then begin
    let dl = find_cursor_display_line t in
    let target = display_line_end_offset t dl in
    Edit_buffer.set_cursor_offset ~select t.buf target;
    true
  end
  else false

(* Key handling *)

type key_result = {
  handled : bool;
  changed : bool;
  moved : bool;
  vertical : bool;
}

let key_result ?(changed = false) ?(moved = false) ?(vertical = false) () =
  { handled = true; changed; moved; vertical }

let unhandled =
  { handled = false; changed = false; moved = false; vertical = false }

let line_home t ~select =
  if Edit_buffer.move_line_start ~select t.buf then true
  else
    let line = Edit_buffer.cursor_line t.buf in
    line > 0 && Edit_buffer.move_left ~select t.buf

let line_end t ~select =
  if Edit_buffer.move_line_end ~select t.buf then true
  else
    let line = Edit_buffer.cursor_line t.buf in
    line < Edit_buffer.line_count t.buf - 1
    && Edit_buffer.move_right ~select t.buf

let select_all_changed t =
  let old_cursor = cursor t in
  let old_selection = selection t in
  Edit_buffer.select_all t.buf;
  old_cursor <> cursor t || old_selection <> selection t

let regular_text data c =
  let m = data.Input.Key.modifier in
  if m.ctrl || m.alt || m.super || m.meta || m.hyper then None
  else
    let text =
      if String.length data.associated_text > 0 then data.associated_text
      else
        let buf = Buffer.create 4 in
        Buffer.add_utf_8_uchar buf c;
        Buffer.contents buf
    in
    if String.length text = 0 then None
    else
      let code = Char.code text.[0] in
      if code < 32 || code = 127 then None else Some text

let run_action t action =
  let open Action in
  match action with
  | Move_left -> key_result ~moved:(Edit_buffer.move_left t.buf) ()
  | Move_right -> key_result ~moved:(Edit_buffer.move_right t.buf) ()
  | Move_up when t.mode = `Multiline ->
      key_result
        ~moved:(move_vertical t ~select:false ~delta:(-1))
        ~vertical:true ()
  | Move_down when t.mode = `Multiline ->
      key_result
        ~moved:(move_vertical t ~select:false ~delta:1)
        ~vertical:true ()
  | Move_up | Move_down -> unhandled
  | Select_left ->
      key_result ~moved:(Edit_buffer.move_left ~select:true t.buf) ()
  | Select_right ->
      key_result ~moved:(Edit_buffer.move_right ~select:true t.buf) ()
  | Select_up when t.mode = `Multiline ->
      key_result
        ~moved:(move_vertical t ~select:true ~delta:(-1))
        ~vertical:true ()
  | Select_down when t.mode = `Multiline ->
      key_result
        ~moved:(move_vertical t ~select:true ~delta:1)
        ~vertical:true ()
  | Select_up | Select_down -> unhandled
  | Line_home -> key_result ~moved:(line_home t ~select:false) ()
  | Line_end -> key_result ~moved:(line_end t ~select:false) ()
  | Select_line_home -> key_result ~moved:(line_home t ~select:true) ()
  | Select_line_end -> key_result ~moved:(line_end t ~select:true) ()
  | Visual_line_home ->
      key_result ~moved:(move_visual_line_start t ~select:false) ()
  | Visual_line_end ->
      key_result ~moved:(move_visual_line_end t ~select:false) ()
  | Select_visual_line_home ->
      key_result ~moved:(move_visual_line_start t ~select:true) ()
  | Select_visual_line_end ->
      key_result ~moved:(move_visual_line_end t ~select:true) ()
  | Buffer_home -> key_result ~moved:(Edit_buffer.move_home t.buf) ()
  | Buffer_end -> key_result ~moved:(Edit_buffer.move_end t.buf) ()
  | Select_buffer_home ->
      key_result ~moved:(Edit_buffer.move_home ~select:true t.buf) ()
  | Select_buffer_end ->
      key_result ~moved:(Edit_buffer.move_end ~select:true t.buf) ()
  | Delete_line -> key_result ~changed:(Edit_buffer.delete_line t.buf) ()
  | Delete_to_line_end ->
      key_result ~changed:(Edit_buffer.delete_to_line_end t.buf) ()
  | Delete_to_line_start ->
      key_result ~changed:(Edit_buffer.delete_to_line_start t.buf) ()
  | Backspace -> key_result ~changed:(Edit_buffer.delete_backward t.buf) ()
  | Delete -> key_result ~changed:(Edit_buffer.delete_forward t.buf) ()
  | Newline when t.mode = `Multiline ->
      key_result ~changed:(Edit_buffer.insert t.buf "\n") ()
  | Newline ->
      fire_on_submit t;
      key_result ()
  | Undo -> key_result ~changed:(Edit_buffer.undo t.buf) ()
  | Redo -> key_result ~changed:(Edit_buffer.redo t.buf) ()
  | Word_forward -> key_result ~moved:(Edit_buffer.move_word_forward t.buf) ()
  | Word_backward -> key_result ~moved:(Edit_buffer.move_word_backward t.buf) ()
  | Select_word_forward ->
      key_result ~moved:(Edit_buffer.move_word_forward ~select:true t.buf) ()
  | Select_word_backward ->
      key_result ~moved:(Edit_buffer.move_word_backward ~select:true t.buf) ()
  | Delete_word_forward ->
      key_result ~changed:(Edit_buffer.delete_word_forward t.buf) ()
  | Delete_word_backward ->
      key_result ~changed:(Edit_buffer.delete_word_backward t.buf) ()
  | Select_all -> key_result ~moved:(select_all_changed t) ()
  | Submit ->
      fire_on_submit t;
      key_result ()

let key_result_of_event t data =
  match Keymap.action t.keymap data with
  | Some action -> run_action t action
  | None -> (
      match data.Input.Key.key with
      | Input.Key.Char c -> (
          match regular_text data c with
          | Some text ->
              key_result
                ~changed:(Edit_buffer.insert t.buf (sanitize_text t text))
                ()
          | None -> unhandled)
      | _ -> unhandled)

let apply_key_result t ev result =
  if result.handled then begin
    Event.Key.prevent_default ev;
    if result.changed then sync t;
    if result.changed then fire_on_input t;
    let cursor_changed = fire_on_cursor t in
    if result.changed || result.moved || cursor_changed then
      ensure_cursor_visible t;
    if not result.vertical then t.preferred_col <- None;
    Renderable.request_render t.node
  end

let handle_key t (ev : Event.key) =
  let data = Event.Key.data ev in
  let open Input.Key in
  match data.event_type with
  | Release -> ()
  | _ when Event.Key.default_prevented ev -> ()
  | _ -> apply_key_result t ev (key_result_of_event t data)

(* Mouse handling *)

let handle_mouse t ev =
  if Event.Mouse.default_prevented ev then ()
  else
    match Event.Mouse.kind ev with
    | Scroll { direction; delta } ->
        handle_scroll t direction delta;
        Event.Mouse.prevent_default ev
    | _ -> ()

(* Paste *)

let handle_paste t text =
  let text = sanitize_text t (Ansi.strip text) in
  if Edit_buffer.insert t.buf text then begin
    sync t;
    fire_on_input t;
    ignore (fire_on_cursor t : bool);
    ensure_cursor_visible t;
    t.preferred_col <- None;
    Renderable.request_render t.node
  end

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?value ?cursor
    ?selection ?spans ?ghost_text ?ghost_text_color ?placeholder ?wrap
    ?text_color ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?(mode = `Multiline)
    ?max_length ?on_input ?on_change ?on_submit ?on_cursor () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?value ?cursor ?selection ?spans ?ghost_text ?ghost_text_color
      ?placeholder ?wrap ?text_color ?background_color ?focused_text_color
      ?focused_background_color ?placeholder_color ?selection_color
      ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ?selectable
      ?show_cursor ()
  in
  let max_length =
    match max_length with
    | Some max_length -> max_length
    | None -> ( match mode with `Multiline -> max_int | `Single_line -> 1000)
  in
  let initial_value =
    match mode with
    | `Multiline -> props.value
    | `Single_line -> Edit_buffer.strip_newlines props.value
  in
  let buf = Edit_buffer.create ~max_length initial_value in
  Option.iter (Edit_buffer.set_cursor buf) props.cursor;
  (match props.selection with
  | None | Some None -> ()
  | Some (Some (a, b)) ->
      let len = Edit_buffer.length buf in
      let clamp x = Int.max 0 (Int.min len x) in
      let lo = clamp (Int.min a b) in
      let hi = clamp (Int.max a b) in
      if lo < hi then begin
        Edit_buffer.set_cursor buf lo;
        Edit_buffer.set_cursor_offset ~select:true buf hi
      end);
  let text_buf = Text_buffer.create () in
  let surface = Text_surface.create node text_buf in
  Text_surface.set_wrap surface props.wrap;
  let initial_value = Edit_buffer.text buf in
  let initial_cursor = Edit_buffer.cursor buf in
  let initial_selection = Edit_buffer.selection buf in
  let t =
    {
      node;
      buf;
      text_buf;
      surface;
      props;
      was_focused = false;
      last_committed_value = initial_value;
      preferred_col = None;
      on_input;
      on_change;
      on_submit;
      on_cursor;
      last_cursor = initial_cursor;
      last_selection = initial_selection;
      mode;
      keymap = Action.keymap mode;
    }
  in
  Renderable.set_render_before node (Some (render_before t));
  Renderable.set_render_after node (Some (render_after t));
  Renderable.set_cursor_provider node (cursor_provider t);
  Renderable.set_default_key_handler node (Some (handle_key t));
  (match mode with
  | `Multiline -> ()
  | `Single_line -> Renderable.set_measure node (Some (measure_single_line t)));
  Renderable.on_mouse node (handle_mouse t);
  register_selection t;
  register_line_info t;
  sync_style t ~focused:false;
  t

(* Value *)

let set_value t s =
  Edit_buffer.set_text t.buf (sanitize_text t s);
  ignore (fire_on_cursor t : bool);
  sync t;
  ensure_cursor_visible t;
  t.preferred_col <- None;
  Renderable.request_render t.node

let edit t f =
  if f t.buf then begin
    sync t;
    fire_on_input t;
    ignore (fire_on_cursor t : bool);
    ensure_cursor_visible t;
    t.preferred_col <- None;
    Renderable.request_render t.node
  end

let set_max_length t n =
  let before_value = value t in
  let before_cursor = cursor t in
  let before_selection = selection t in
  Edit_buffer.set_max_length t.buf n;
  let value_changed = not (String.equal before_value (value t)) in
  let cursor_changed =
    before_cursor <> cursor t || before_selection <> selection t
  in
  if value_changed then sync t;
  if cursor_changed then ignore (fire_on_cursor t : bool);
  if value_changed || cursor_changed then begin
    ensure_cursor_visible t;
    t.preferred_col <- None;
    Renderable.request_render t.node
  end

(* Apply props *)

let apply_props t (props : Props.t) =
  let spans_changed = not (Props.spans_equal t.props.spans props.spans) in
  let selectable_changed = t.props.selectable <> props.selectable in
  let style_changed =
    (not (Ansi.Color.equal t.props.text_color props.text_color))
    || (not (Ansi.Color.equal t.props.background_color props.background_color))
    || (not
          (Ansi.Color.equal t.props.focused_text_color props.focused_text_color))
    || not
         (Ansi.Color.equal t.props.focused_background_color
            props.focused_background_color)
  in
  let value_replaced = ref false in
  if
    (not (String.equal t.props.value props.value))
    && not (String.equal (Edit_buffer.text t.buf) (sanitize_text t props.value))
  then begin
    Edit_buffer.set_text t.buf (sanitize_text t props.value);
    value_replaced := true;
    t.preferred_col <- None
  end;
  let cursor_changed =
    match props.cursor with
    | Some c when Edit_buffer.cursor t.buf <> c ->
        Edit_buffer.set_cursor t.buf c;
        true
    | _ -> false
  in
  let selection_changed =
    match props.selection with
    | None -> false
    | Some None ->
        let had_selection = Option.is_some (selection t) in
        if had_selection then Edit_buffer.set_cursor t.buf (cursor t);
        had_selection
    | Some (Some sel) -> apply_selection t sel
  in
  if t.props.wrap <> props.wrap then Text_surface.set_wrap t.surface props.wrap;
  t.props <- props;
  if selectable_changed then register_selection t;
  if style_changed then sync_style t ~focused:(Renderable.focused t.node)
  else if !value_replaced || spans_changed then sync t;
  let state_changed = fire_on_cursor t in
  if !value_replaced || cursor_changed || selection_changed || state_changed
  then ensure_cursor_visible t;
  Renderable.request_render t.node

(* Formatting *)

let pp ppf t =
  Format.fprintf ppf "Edit_surface(%s" (Renderable.id t.node);
  let v = value t in
  if String.length v > 0 then begin
    let display =
      if String.length v > 20 then String.sub v 0 20 ^ "..." else v
    in
    Format.fprintf ppf ", %S" display
  end;
  if String.length t.props.placeholder > 0 then
    Format.fprintf ppf ", placeholder=%S" t.props.placeholder;
  Format.pp_print_char ppf ')'
