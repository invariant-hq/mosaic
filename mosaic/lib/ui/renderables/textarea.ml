(* ───── Defaults ───── *)

let default_text_color = Ansi.Color.White
let default_background_color = Ansi.Color.default
let default_focused_text_color = Ansi.Color.White
let default_focused_background_color = Ansi.Color.default
let default_placeholder_color = Ansi.Color.Bright_black
let default_selection_color = Ansi.Color.Blue
let default_cursor_style = `Block
let default_cursor_color = Ansi.Color.White
let default_cursor_blinking = true
let default_ghost_text_color = Ansi.Color.grayscale ~level:12

(* ───── Props ───── *)

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
      ?(cursor_blinking = default_cursor_blinking) () =
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
end

(* ───── Types ───── *)

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
}

(* ───── Accessors ───── *)

let node t = t.node
let buffer t = t.buf
let surface t = t.surface
let value t = Edit_buffer.text t.buf
let cursor t = Edit_buffer.cursor t.buf
let selection t = Edit_buffer.selection t.buf

(* ───── Line Info Provider ───── *)

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

(* ───── Callbacks ───── *)

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

(* ───── Sync ───── *)

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

(* ───── Display line mapping ───── *)

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
    Glyph.String.measure ~width_method ~tab_width text

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
    Glyph.String.iter_grapheme_info ~width_method ~tab_width
      (fun ~offset:_ ~len:_ ~width ->
        if !col + width <= target_col then begin
          col := !col + width;
          incr result
        end)
      text;
    !result

(* ───── Scroll ───── *)

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
      else if cc >= sx + w then Text_surface.set_scroll_x t.surface (cc - w + 1)
    end
  end

(* ───── Rendering ───── *)

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
    Grid.fill_rect grid ~x:x0 ~y:y0 ~width:w ~height:h ~color:bg;
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

(* ───── Cursor Provider ───── *)

let cursor_provider t _self =
  if not (Renderable.focused t.node) then None
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

(* ───── Vertical movement ───── *)

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

(* ───── Visual line navigation ───── *)

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

(* ───── Key Handling ───── *)

let normalize_modified_char_code (m : Input.Key.modifier) c =
  let code = Uchar.to_int c in
  if
    (m.ctrl || m.alt || m.super || m.meta || m.hyper)
    && code >= Char.code 'A'
    && code <= Char.code 'Z'
  then code + 32
  else code

let handle_key t (ev : Event.key) =
  let data = Event.Key.data ev in
  if data.event_type = Release then ()
  else if Event.Key.default_prevented ev then ()
  else begin
    let m = data.modifier in
    let changed = ref false in
    let handled = ref true in
    let moved = ref false in
    (match data.key with
    (* Cursor movement *)
    | Left when m.super -> moved := move_visual_line_start t ~select:m.shift
    | Left when m.ctrl || m.alt ->
        moved := Edit_buffer.move_word_backward ~select:m.shift t.buf
    | Left -> moved := Edit_buffer.move_left ~select:m.shift t.buf
    | Right when m.super -> moved := move_visual_line_end t ~select:m.shift
    | Right when m.ctrl || m.alt ->
        moved := Edit_buffer.move_word_forward ~select:m.shift t.buf
    | Right -> moved := Edit_buffer.move_right ~select:m.shift t.buf
    | Up when m.super -> moved := Edit_buffer.move_home ~select:m.shift t.buf
    | Down when m.super -> moved := Edit_buffer.move_end ~select:m.shift t.buf
    | Up -> moved := move_vertical t ~select:m.shift ~delta:(-1)
    | Down -> moved := move_vertical t ~select:m.shift ~delta:1
    | Home -> moved := Edit_buffer.move_home ~select:m.shift t.buf
    | End -> moved := Edit_buffer.move_end ~select:m.shift t.buf
    (* Deletion *)
    | Backspace when m.ctrl || m.alt ->
        changed := Edit_buffer.delete_word_backward t.buf
    | Backspace -> changed := Edit_buffer.delete_backward t.buf
    | Delete when m.ctrl || m.alt ->
        changed := Edit_buffer.delete_word_forward t.buf
    | Delete -> changed := Edit_buffer.delete_forward t.buf
    (* Submit: Cmd/Ctrl/Alt+Enter *)
    | (Enter | Line_feed) when m.super || m.ctrl || m.alt -> fire_on_submit t
    (* Newline: Enter *)
    | Enter | Line_feed -> changed := Edit_buffer.insert t.buf "\n"
    (* Char-based shortcuts *)
    | Char c ->
        let code = normalize_modified_char_code m c in
        if m.super && code = 0x61 (* a *) then Edit_buffer.select_all t.buf
        else if m.ctrl then begin
          match code with
          | 0x61 (* a *) ->
              if not (Edit_buffer.move_line_start ~select:m.shift t.buf) then begin
                let line = Edit_buffer.cursor_line t.buf in
                if line > 0 then
                  moved := Edit_buffer.move_left ~select:m.shift t.buf
              end
              else moved := true
          | 0x65 (* e *) ->
              if not (Edit_buffer.move_line_end ~select:m.shift t.buf) then begin
                let line = Edit_buffer.cursor_line t.buf in
                if line < Edit_buffer.line_count t.buf - 1 then
                  moved := Edit_buffer.move_right ~select:m.shift t.buf
              end
              else moved := true
          | 0x62 (* b *) -> moved := Edit_buffer.move_left ~select:m.shift t.buf
          | 0x66 (* f *) ->
              moved := Edit_buffer.move_right ~select:m.shift t.buf
          | 0x64 (* d *) when m.shift ->
              changed := Edit_buffer.delete_line t.buf
          | 0x64 (* d *) -> changed := Edit_buffer.delete_forward t.buf
          | 0x6B (* k *) -> changed := Edit_buffer.delete_to_line_end t.buf
          | 0x75 (* u *) -> changed := Edit_buffer.delete_to_line_start t.buf
          | 0x77 (* w *) -> changed := Edit_buffer.delete_word_backward t.buf
          | 0x2D (* - *) -> changed := Edit_buffer.undo t.buf
          | 0x2E (* . *) -> changed := Edit_buffer.redo t.buf
          | _ ->
              if m.shift then begin
                match code with
                | 0x7A (* z *) -> changed := Edit_buffer.redo t.buf
                | _ -> handled := false
              end
              else begin
                match code with
                | 0x7A (* z *) -> changed := Edit_buffer.undo t.buf
                | _ -> handled := false
              end
        end
        else if m.alt then begin
          match code with
          | 0x61 (* a *) -> moved := move_visual_line_start t ~select:m.shift
          | 0x65 (* e *) -> moved := move_visual_line_end t ~select:m.shift
          | 0x62 (* b *) ->
              moved := Edit_buffer.move_word_backward ~select:m.shift t.buf
          | 0x66 (* f *) ->
              moved := Edit_buffer.move_word_forward ~select:m.shift t.buf
          | 0x64 (* d *) -> changed := Edit_buffer.delete_word_forward t.buf
          | _ -> handled := false
        end
        else if m.super then begin
          match code with
          | 0x7A (* z *) when m.shift -> changed := Edit_buffer.redo t.buf
          | 0x7A (* z *) -> changed := Edit_buffer.undo t.buf
          | _ -> handled := false
        end
        else begin
          let text_to_insert =
            if String.length data.associated_text > 0 then data.associated_text
            else
              let buf = Buffer.create 4 in
              Buffer.add_utf_8_uchar buf c;
              Buffer.contents buf
          in
          changed := Edit_buffer.insert t.buf text_to_insert
        end
    | _ -> handled := false);
    if !handled then begin
      Event.Key.prevent_default ev;
      if !changed then sync t;
      if !changed then fire_on_input t;
      let cursor_changed = fire_on_cursor t in
      if !changed || !moved || cursor_changed then ensure_cursor_visible t;
      let is_vertical = match data.key with Up | Down -> true | _ -> false in
      if not is_vertical then t.preferred_col <- None;
      Renderable.request_render t.node
    end
  end

(* ───── Paste Handling ───── *)

let handle_paste t text =
  if Edit_buffer.insert t.buf text then begin
    sync t;
    fire_on_input t;
    ignore (fire_on_cursor t : bool);
    ensure_cursor_visible t;
    t.preferred_col <- None;
    Renderable.request_render t.node
  end

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?value ?cursor
    ?selection ?spans ?ghost_text ?ghost_text_color ?placeholder ?wrap
    ?text_color ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit ?on_cursor ()
    =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?value ?cursor ?selection ?spans ?ghost_text
      ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
      ?focused_text_color ?focused_background_color ?placeholder_color
      ?selection_color ?selection_fg ?cursor_style ?cursor_color
      ?cursor_blinking ()
  in
  let buf = Edit_buffer.create ~max_length:max_int props.value in
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
    }
  in
  Renderable.set_render_before node (Some (render_before t));
  Renderable.set_render_after node (Some (render_after t));
  Renderable.set_cursor_provider node (cursor_provider t);
  Renderable.set_default_key_handler node (Some (handle_key t));
  register_line_info t;
  sync_style t ~focused:false;
  t

(* ───── Value ───── *)

let set_value t s =
  Edit_buffer.set_text t.buf s;
  ignore (fire_on_cursor t : bool);
  sync t;
  ensure_cursor_visible t;
  t.preferred_col <- None;
  Renderable.request_render t.node

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  let spans_changed =
    not (Props.spans_equal t.props.spans props.spans)
  in
  let value_replaced = ref false in
  if
    (not (String.equal t.props.value props.value))
    && not (String.equal (Edit_buffer.text t.buf) props.value)
  then begin
    Edit_buffer.set_text t.buf props.value;
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
  if !value_replaced || spans_changed then sync t;
  let state_changed = fire_on_cursor t in
  if !value_replaced || cursor_changed || selection_changed || state_changed
  then ensure_cursor_visible t;
  Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Textarea(%s" (Renderable.id t.node);
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
