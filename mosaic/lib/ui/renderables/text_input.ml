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

(* ───── Props ───── *)

module Props = struct
  type t = {
    value : string;
    cursor : int option;
    selection : (int * int) option option;
    placeholder : string;
    max_length : int;
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

  let make ?(value = "") ?cursor ?selection ?(placeholder = "")
      ?(max_length = 1000) ?(text_color = default_text_color)
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
      placeholder;
      max_length;
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

  let equal a b =
    String.equal a.value b.value
    && Option.equal Int.equal a.cursor b.cursor
    && Option.equal
         (Option.equal (fun (a1, a2) (b1, b2) -> a1 = b1 && a2 = b2))
         a.selection b.selection
    && String.equal a.placeholder b.placeholder
    && a.max_length = b.max_length
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
  mutable props : Props.t;
  mutable scroll_x : int;
  mutable was_focused : bool;
  mutable last_committed_value : string;
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
let value t = Edit_buffer.text t.buf
let cursor t = Edit_buffer.cursor t.buf
let selection t = Edit_buffer.selection t.buf

(* ───── Callbacks ───── *)

let set_on_input t h = t.on_input <- h
let set_on_change t h = t.on_change <- h
let set_on_submit t h = t.on_submit <- h
let set_on_cursor t h = t.on_cursor <- h
let fire_on_input t = match t.on_input with Some f -> f (value t) | None -> ()

(* on_change fires only when the value has actually changed since the last
   commit point (focus-gain or previous on_change). This prevents duplicate
   notifications when the user blurs without editing. *)
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

(* ───── Scroll ───── *)

let ensure_cursor_visible t =
  let w = Renderable.width t.node in
  if w <= 0 then ()
  else begin
    let cursor_col = Edit_buffer.cursor_display_offset t.buf in
    if cursor_col < t.scroll_x then t.scroll_x <- cursor_col
    else if cursor_col >= t.scroll_x + w then t.scroll_x <- cursor_col - w + 1
  end

(* ───── Measure ───── *)

let measure t ~known_dimensions ~available_space:_ ~style:_ =
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

(* ───── Rendering ───── *)

let render t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else begin
    let x0 = Renderable.x t.node in
    let y0 = Renderable.y t.node in
    let focused = Renderable.focused t.node in
    (* Commit-on-blur: snapshot the value on focus-gain so we can detect real
       edits, and fire on_change on blur only if something changed. *)
    if focused && not t.was_focused then t.last_committed_value <- value t
    else if (not focused) && t.was_focused then fire_on_change t;
    t.was_focused <- focused;
    let bg =
      if focused then t.props.focused_background_color
      else t.props.background_color
    in
    Grid.clear_rect ~color:bg grid ~x:x0 ~y:y0 ~width:w ~height:h;
    let text = value t in
    if String.length text = 0 && String.length t.props.placeholder > 0 then begin
      let style = Ansi.Style.make ~fg:t.props.placeholder_color ~bg () in
      Grid.clip grid { x = x0; y = y0; width = w; height = h } (fun () ->
          Grid.draw_text ~style grid ~x:x0 ~y:y0 ~text:t.props.placeholder)
    end
    else if String.length text > 0 then begin
      let fg =
        if focused then t.props.focused_text_color else t.props.text_color
      in
      let sel = Edit_buffer.selection t.buf in
      Grid.clip grid { x = x0; y = y0; width = w; height = h } (fun () ->
          let draw_x = x0 - t.scroll_x in
          match sel with
          | None ->
              let style = Ansi.Style.make ~fg ~bg () in
              Grid.draw_text ~style grid ~x:draw_x ~y:y0 ~text
          | Some (sel_start, sel_end) ->
              (* Draw text in three segments: before selection, selection,
                 after *)
              let cache_ref = ref [] in
              Matrix.Text.iter_grapheme_info ~width_method:`Unicode ~tab_width:2
                (fun ~offset ~len ~width ->
                  cache_ref := (offset, len, width) :: !cache_ref)
                text;
              let graphemes = Array.of_list (List.rev !cache_ref) in
              let col = ref 0 in
              for i = 0 to Array.length graphemes - 1 do
                let offset, len, gwidth = graphemes.(i) in
                let s = String.sub text offset len in
                let in_selection = i >= sel_start && i < sel_end in
                let style =
                  if in_selection then
                    let sel_fg =
                      match t.props.selection_fg with Some c -> c | None -> fg
                    in
                    Ansi.Style.make ~fg:sel_fg ~bg:t.props.selection_color ()
                  else Ansi.Style.make ~fg ~bg ()
                in
                Grid.draw_text ~style grid ~x:(draw_x + !col) ~y:y0 ~text:s;
                col := !col + gwidth
              done)
    end
  end

(* ───── Cursor Provider ───── *)

let cursor_provider t _self =
  if Renderable.focused t.node then
    let x0 = Renderable.x t.node in
    let y0 = Renderable.y t.node in
    let cursor_col = Edit_buffer.cursor_display_offset t.buf in
    let screen_x = x0 + cursor_col - t.scroll_x in
    let w = Renderable.width t.node in
    if screen_x >= x0 && screen_x < x0 + w then
      Some
        {
          Renderable.x = screen_x;
          y = y0;
          style = t.props.cursor_style;
          color = t.props.cursor_color;
          blinking = t.props.cursor_blinking;
        }
    else None
  else None

(* ───── Key Handling ───── *)

let normalize_modified_char_code (m : Input.Modifier.t) c =
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
    (match data.key with
    (* Cursor movement *)
    | Left when m.super ->
        ignore (Edit_buffer.move_home ~select:m.shift t.buf : bool)
    | Left when m.ctrl || m.alt ->
        ignore (Edit_buffer.move_word_backward ~select:m.shift t.buf : bool)
    | Left -> ignore (Edit_buffer.move_left ~select:m.shift t.buf : bool)
    | Right when m.super ->
        ignore (Edit_buffer.move_end ~select:m.shift t.buf : bool)
    | Right when m.ctrl || m.alt ->
        ignore (Edit_buffer.move_word_forward ~select:m.shift t.buf : bool)
    | Right -> ignore (Edit_buffer.move_right ~select:m.shift t.buf : bool)
    | Up when m.super ->
        ignore (Edit_buffer.move_home ~select:m.shift t.buf : bool)
    | Down when m.super ->
        ignore (Edit_buffer.move_end ~select:m.shift t.buf : bool)
    | Home -> ignore (Edit_buffer.move_home ~select:m.shift t.buf : bool)
    | End -> ignore (Edit_buffer.move_end ~select:m.shift t.buf : bool)
    (* Deletion *)
    | Backspace when m.ctrl || m.alt ->
        changed := Edit_buffer.delete_word_backward t.buf
    | Backspace -> changed := Edit_buffer.delete_backward t.buf
    | Delete when m.ctrl || m.alt ->
        changed := Edit_buffer.delete_word_forward t.buf
    | Delete -> changed := Edit_buffer.delete_forward t.buf
    (* Submit *)
    | Enter | Line_feed -> fire_on_submit t
    (* Char-based shortcuts *)
    | Char c ->
        let code = normalize_modified_char_code m c in
        if m.super && code = 0x61 (* a *) then Edit_buffer.select_all t.buf
        else if m.ctrl then begin
          match code with
          | 0x61 (* a *) ->
              ignore (Edit_buffer.move_home ~select:m.shift t.buf : bool)
          | 0x65 (* e *) ->
              ignore (Edit_buffer.move_end ~select:m.shift t.buf : bool)
          | 0x62 (* b *) ->
              ignore (Edit_buffer.move_left ~select:m.shift t.buf : bool)
          | 0x66 (* f *) ->
              ignore (Edit_buffer.move_right ~select:m.shift t.buf : bool)
          | 0x64 (* d *) when m.shift ->
              changed := Edit_buffer.delete_line t.buf
          | 0x64 (* d *) -> changed := Edit_buffer.delete_forward t.buf
          | 0x6B (* k *) -> changed := Edit_buffer.delete_to_end t.buf
          | 0x75 (* u *) -> changed := Edit_buffer.delete_to_start t.buf
          | 0x77 (* w *) -> changed := Edit_buffer.delete_word_backward t.buf
          | 0x2D (* - *) -> changed := Edit_buffer.undo t.buf
          | 0x2E (* . *) -> changed := Edit_buffer.redo t.buf
          | _ ->
              if m.shift then begin
                (* Ctrl+Shift shortcuts *)
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
          | 0x62 (* b *) ->
              ignore
                (Edit_buffer.move_word_backward ~select:m.shift t.buf : bool)
          | 0x66 (* f *) ->
              ignore
                (Edit_buffer.move_word_forward ~select:m.shift t.buf : bool)
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
          (* Regular character input *)
          let text_to_insert =
            Edit_buffer.strip_newlines
              (if String.length data.associated_text > 0 then
                 data.associated_text
               else
                 let buf = Buffer.create 4 in
                 Buffer.add_utf_8_uchar buf c;
                 Buffer.contents buf)
          in
          changed := Edit_buffer.insert t.buf text_to_insert
        end
    | _ -> handled := false);
    if !handled then begin
      Event.Key.prevent_default ev;
      if !changed then fire_on_input t;
      let cursor_changed = fire_on_cursor t in
      if !changed || cursor_changed then ensure_cursor_visible t;
      Renderable.request_render t.node
    end
  end

(* ───── Paste Handling ───── *)

let handle_paste t text =
  let text = Edit_buffer.strip_newlines text in
  if Edit_buffer.insert t.buf text then begin
    ensure_cursor_visible t;
    fire_on_input t;
    ignore (fire_on_cursor t : bool);
    Renderable.request_render t.node
  end

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?value ?cursor
    ?selection ?placeholder ?max_length ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?on_input ?on_change ?on_submit ?on_cursor () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?value ?cursor ?selection ?placeholder ?max_length ?text_color
      ?background_color ?focused_text_color ?focused_background_color
      ?placeholder_color ?selection_color ?selection_fg ?cursor_style
      ?cursor_color ?cursor_blinking ()
  in
  let buf =
    Edit_buffer.create ~max_length:props.max_length
      (Edit_buffer.strip_newlines props.value)
  in
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
  let initial_value = Edit_buffer.text buf in
  let initial_cursor = Edit_buffer.cursor buf in
  let initial_selection = Edit_buffer.selection buf in
  let t =
    {
      node;
      buf;
      props;
      scroll_x = 0;
      was_focused = false;
      last_committed_value = initial_value;
      on_input;
      on_change;
      on_submit;
      on_cursor;
      last_cursor = initial_cursor;
      last_selection = initial_selection;
    }
  in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_cursor_provider node (cursor_provider t);
  Renderable.set_default_key_handler node (Some (handle_key t));
  t

(* ───── Value ───── *)

let set_value t s =
  Edit_buffer.set_text t.buf (Edit_buffer.strip_newlines s);
  ignore (fire_on_cursor t : bool);
  t.scroll_x <- 0;
  ensure_cursor_visible t;
  Renderable.request_render t.node

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  let value_replaced = ref false in
  if not (String.equal t.props.value props.value) then begin
    Edit_buffer.set_text t.buf (Edit_buffer.strip_newlines props.value);
    value_replaced := true;
    t.scroll_x <- 0;
    ensure_cursor_visible t
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
  if t.props.max_length <> props.max_length then
    Edit_buffer.set_max_length t.buf props.max_length;
  t.props <- props;
  let state_changed = fire_on_cursor t in
  if !value_replaced || cursor_changed || selection_changed || state_changed
  then ensure_cursor_visible t;
  Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Input(%s" (Renderable.id t.node);
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
