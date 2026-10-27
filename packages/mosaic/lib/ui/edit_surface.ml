module Layout = Matrix.Text.Layout

(* Defaults *)

let default_cursor_style = `Block
let default_cursor_blinking = true

(* Props *)

type mode = [ `Multiline | `Single_line ]

type action =
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

module Action = struct
  let defaults =
    [
      (Shortcut.key Matrix_input.Keystroke.Left, Move_left);
      (Shortcut.key Matrix_input.Keystroke.Right, Move_right);
      (Shortcut.key Matrix_input.Keystroke.Up, Move_up);
      (Shortcut.key Matrix_input.Keystroke.Down, Move_down);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Left, Select_left);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Right, Select_right);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Up, Select_up);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Down, Select_down);
      (Shortcut.key Matrix_input.Keystroke.Home, Buffer_home);
      (Shortcut.key Matrix_input.Keystroke.End, Buffer_end);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Home, Select_buffer_home);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.End, Select_buffer_end);
      (Shortcut.char ~ctrl:true 'a', Line_home);
      (Shortcut.char ~ctrl:true 'e', Line_end);
      (Shortcut.char ~ctrl:true ~shift:true 'a', Select_line_home);
      (Shortcut.char ~ctrl:true ~shift:true 'e', Select_line_end);
      (Shortcut.char ~alt:true 'a', Visual_line_home);
      (Shortcut.char ~alt:true 'e', Visual_line_end);
      (Shortcut.char ~alt:true ~shift:true 'a', Select_visual_line_home);
      (Shortcut.char ~alt:true ~shift:true 'e', Select_visual_line_end);
      (Shortcut.char ~ctrl:true 'f', Move_right);
      (Shortcut.char ~ctrl:true 'b', Move_left);
      (Shortcut.char ~ctrl:true 'w', Delete_word_backward);
      ( Shortcut.key ~ctrl:true Matrix_input.Keystroke.Backspace,
        Delete_word_backward );
      (Shortcut.char ~alt:true 'd', Delete_word_forward);
      (Shortcut.key ~alt:true Matrix_input.Keystroke.Delete, Delete_word_forward);
      ( Shortcut.key ~ctrl:true Matrix_input.Keystroke.Delete,
        Delete_word_forward );
      (Shortcut.char ~ctrl:true ~shift:true 'd', Delete_line);
      (Shortcut.char ~ctrl:true 'k', Delete_to_line_end);
      (Shortcut.char ~ctrl:true 'u', Delete_to_line_start);
      (Shortcut.key Matrix_input.Keystroke.Backspace, Backspace);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Backspace, Backspace);
      (Shortcut.char ~ctrl:true 'd', Delete);
      (Shortcut.key Matrix_input.Keystroke.Delete, Delete);
      (Shortcut.key ~shift:true Matrix_input.Keystroke.Delete, Delete);
      (Shortcut.key Matrix_input.Keystroke.Enter, Newline);
      (Shortcut.char ~ctrl:true 'j', Newline);
      (Shortcut.key ~alt:true Matrix_input.Keystroke.Enter, Submit);
      (Shortcut.key ~ctrl:true Matrix_input.Keystroke.Enter, Submit);
      (Shortcut.key ~super:true Matrix_input.Keystroke.Enter, Submit);
      (Shortcut.char ~ctrl:true ~alt:true 'j', Submit);
      (Shortcut.char ~ctrl:true ~super:true 'j', Submit);
      (Shortcut.char ~ctrl:true '-', Undo);
      (Shortcut.char ~ctrl:true '.', Redo);
      (Shortcut.char ~ctrl:true 'z', Undo);
      (Shortcut.char ~ctrl:true ~shift:true 'z', Redo);
      (Shortcut.char ~super:true 'z', Undo);
      (Shortcut.char ~super:true ~shift:true 'z', Redo);
      (Shortcut.char ~alt:true 'f', Word_forward);
      (Shortcut.char ~alt:true 'b', Word_backward);
      (Shortcut.key ~alt:true Matrix_input.Keystroke.Right, Word_forward);
      (Shortcut.key ~alt:true Matrix_input.Keystroke.Left, Word_backward);
      (Shortcut.key ~ctrl:true Matrix_input.Keystroke.Right, Word_forward);
      (Shortcut.key ~ctrl:true Matrix_input.Keystroke.Left, Word_backward);
      (Shortcut.char ~alt:true ~shift:true 'f', Select_word_forward);
      (Shortcut.char ~alt:true ~shift:true 'b', Select_word_backward);
      ( Shortcut.key ~alt:true ~shift:true Matrix_input.Keystroke.Right,
        Select_word_forward );
      ( Shortcut.key ~alt:true ~shift:true Matrix_input.Keystroke.Left,
        Select_word_backward );
      ( Shortcut.key ~alt:true Matrix_input.Keystroke.Backspace,
        Delete_word_backward );
      (Shortcut.key ~super:true Matrix_input.Keystroke.Left, Visual_line_home);
      (Shortcut.key ~super:true Matrix_input.Keystroke.Right, Visual_line_end);
      (Shortcut.key ~super:true Matrix_input.Keystroke.Up, Buffer_home);
      (Shortcut.key ~super:true Matrix_input.Keystroke.Down, Buffer_end);
      ( Shortcut.key ~super:true ~shift:true Matrix_input.Keystroke.Left,
        Select_visual_line_home );
      ( Shortcut.key ~super:true ~shift:true Matrix_input.Keystroke.Right,
        Select_visual_line_end );
      ( Shortcut.key ~super:true ~shift:true Matrix_input.Keystroke.Up,
        Select_buffer_home );
      ( Shortcut.key ~super:true ~shift:true Matrix_input.Keystroke.Down,
        Select_buffer_end );
      (Shortcut.char ~super:true 'a', Select_all);
    ]

  let input_overrides =
    [
      (Shortcut.key Matrix_input.Keystroke.Enter, Submit);
      (Shortcut.char ~ctrl:true 'j', Submit);
    ]

  let multiline = Keymap.make ~defaults ()
  let single_line = Keymap.make ~defaults ~custom:input_overrides ()

  let keymap ?(custom = []) mode =
    match (custom, mode) with
    | [], `Multiline -> multiline
    | [], `Single_line -> single_line
    | _, `Multiline -> Keymap.make ~defaults ~custom ()
    | _, `Single_line ->
        Keymap.make ~defaults ~custom:(input_overrides @ custom) ()
end

type key_binding = Shortcut.t * action

(* [Props.make] is [Textarea.Props.make], the one place the textarea's labels
   get their defaults: [Mosaic.textarea] passes its labels
   on unset. [Text_input] makes these props from its own, every field set. *)
module Props = struct
  type t = {
    state : Editor.t;
    annotations : Text_buffer.Annotations.t option;
    syntax : Syntax_session.syntax option;
    ghost_text : string option;
    ghost_text_color : Ansi.Color.t option;
    placeholder : string;
    wrap : Text_surface.wrap;
    text_color : Ansi.Color.t option;
    background_color : Ansi.Color.t option;
    focused_text_color : Ansi.Color.t option;
    focused_background_color : Ansi.Color.t option;
    placeholder_color : Ansi.Color.t option;
    selection_color : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    cursor_style : [ `Block | `Line | `Underline ];
    cursor_color : Ansi.Color.t;
    cursor_blinking : bool;
    selectable : bool;
    show_cursor : bool;
    key_bindings : key_binding list;
  }

  let make ~state ?annotations ?syntax ?ghost_text ?ghost_text_color
      ?(placeholder = "") ?(wrap = `Word) ?text_color ?background_color
      ?focused_text_color ?focused_background_color ?placeholder_color
      ?selection_color ?selection_fg ?(cursor_style = default_cursor_style)
      ?(cursor_color = Ansi.Color.default_fg)
      ?(cursor_blinking = default_cursor_blinking) ?(selectable = true)
      ?(show_cursor = true) ?(key_bindings = []) () =
    (match annotations with
    | None -> ()
    | Some annotations ->
        let content = Text_buffer.Annotations.content annotations in
        let accepted = Editor.text state in
        if content != accepted && not (String.equal content accepted) then
          invalid_arg
            "Edit_surface.Props.make: annotations do not match editor source");
    {
      state;
      annotations;
      syntax;
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
      key_bindings;
    }

  let default = make ~state:Editor.empty ()

  let equal a b =
    a.state == b.state
    && Option.equal ( == ) a.annotations b.annotations
    && Option.equal Syntax_session.equal a.syntax b.syntax
    && Option.equal String.equal a.ghost_text b.ghost_text
    && Option.equal Ansi.Color.equal a.ghost_text_color b.ghost_text_color
    && String.equal a.placeholder b.placeholder
    && a.wrap = b.wrap
    && Option.equal Ansi.Color.equal a.text_color b.text_color
    && Option.equal Ansi.Color.equal a.background_color b.background_color
    && Option.equal Ansi.Color.equal a.focused_text_color b.focused_text_color
    && Option.equal Ansi.Color.equal a.focused_background_color
         b.focused_background_color
    && Option.equal Ansi.Color.equal a.placeholder_color b.placeholder_color
    && Option.equal Ansi.Color.equal a.selection_color b.selection_color
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && a.cursor_style = b.cursor_style
    && Ansi.Color.equal a.cursor_color b.cursor_color
    && a.cursor_blinking = b.cursor_blinking
    && a.selectable = b.selectable
    && a.show_cursor = b.show_cursor
    && List.equal (Keymap.binding_equal ( = )) a.key_bindings b.key_bindings
end

(* Types *)

type t = {
  node : Renderable.t;
  text_buf : Text_buffer.t;
  session : Syntax_session.t;
  mutable styled_content : bool;
  mutable shown_style : Syntax_style.t;
  mutable styled : bool;
      (* Whether captures are shown, and the syntax style they were last shown
         in, to show them again when the theme in scope gives another. *)
  surface : Text_surface.t;
  mutable props : Props.t;
  mutable on_change : Editor.t -> unit;
  mutable on_submit : (string -> unit) option;
  mode : mode;
  mutable keymap : action Keymap.t;
}

(* Accessors *)

let node t = t.node
let state t = t.props.state
let surface t = t.surface
let value t = Editor.text t.props.state
let cursor t = Editor.cursor t.props.state
let selection t = Editor.selection t.props.state

(* Single-line editors drop CR, LF and CRLF from inserted text. Text without
   them is returned as is. *)
let strip_newlines s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let c = String.unsafe_get s !i in
    if c = '\r' then
      begin if !i + 1 < len && String.unsafe_get s (!i + 1) = '\n' then
        i := !i + 2
      else incr i
      end
    else if c = '\n' then incr i
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  if Buffer.length buf = len then s else Buffer.contents buf

let sanitize_text t s =
  match t.mode with `Multiline -> s | `Single_line -> strip_newlines s

(* Text rows *)

let register_text_rows t =
  Renderable.set_text_rows_provider t.node
    (Some
       (fun () ->
         {
           Renderable.layout = Text_surface.layout t.surface;
           scroll_y = Text_surface.scroll_y t.surface;
         }))

(* Callbacks *)

let set_on_change t h = t.on_change <- h
let set_on_submit t h = t.on_submit <- h

let fire_on_submit t =
  match t.on_submit with Some f -> f (value t) | None -> ()

(* An edit follows its caret from the viewport the surface shows, not from the
   one the accepted value declared. *)
let propose t ~base next =
  if
    t.props.state == base && next != base
    && (t.mode <> `Single_line
       || Layout.Source.line_count (Editor.source next) = 1)
  then
    let next =
      match Editor.viewport next with
      | _, _, true ->
          Editor.with_viewport
            ~x:(Text_surface.scroll_x t.surface)
            ~y:(Text_surface.scroll_y t.surface)
            ~follow:true next
      | _, _, false -> next
    in
    t.on_change next

(* Sync *)

(* The paint of the accepted text: the captures the syntax shows for it, without
   concealment as editable text shows every byte, with the given annotations
   merged over them. *)
let syntax_style t syntax =
  match Syntax_session.style syntax with
  | Some style -> style
  | None -> Theme.syntax (Renderable.theme t.node)

let paint t =
  let syntax =
    Option.bind t.props.syntax (fun syntax ->
        let style = syntax_style t syntax in
        Option.map
          (fun captures ->
            t.shown_style <- style;
            t.styled <- true;
            Syntax_highlight.to_annotations ~style captures)
          (Syntax_session.shown t.session ~conceal:false))
  in
  match (syntax, t.props.annotations) with
  | Some base, Some overlay ->
      Some (Text_buffer.Annotations.merge ~base ~overlay)
  | Some paint, None | None, Some paint -> Some paint
  | None, None -> None

let sync_content t =
  let source = Editor.source t.props.state in
  t.styled <- false;
  let paint = paint t in
  t.styled_content <- Option.is_some paint;
  Text_buffer.set_text t.text_buf (Editor.text t.props.state);
  Text_buffer.set_source t.text_buf source;
  Option.iter (Text_buffer.set_annotations t.text_buf) paint

let sync t =
  sync_content t;
  Text_surface.invalidate t.surface

(* The colors the labels give, the focused ones falling back to the others.
   A color no label gives is the [Field] surface's, which the text surface
   and [render_before] resolve at paint. *)
let label_fg t ~focused =
  match t.props.focused_text_color with
  | Some _ as c when focused -> c
  | _ -> t.props.text_color

let label_bg t ~focused =
  match t.props.focused_background_color with
  | Some _ as c when focused -> c
  | _ -> t.props.background_color

let sync_style t ~focused =
  let fg = label_fg t ~focused and bg = label_bg t ~focused in
  Text_buffer.set_default_style t.text_buf (Ansi.Style.make ?fg ?bg ());
  if not t.styled_content then
    Text_buffer.set_text t.text_buf (Editor.text t.props.state);
  Renderable.request_render t.node

let handle_focus t = sync_style t ~focused:true
let handle_blur t = sync_style t ~focused:false

(* Display line mapping *)

let find_cursor_display_line t =
  fst
    (Layout.locate (Text_surface.layout t.surface) (Editor.caret t.props.state))

let cursor_visual_col t display_line =
  let row, column =
    Layout.locate (Text_surface.layout t.surface) (Editor.caret t.props.state)
  in
  if row = display_line then column else 0

let display_line_end_offset t display_line =
  Layout.row_end (Text_surface.layout t.surface) display_line

let caret_at_col t target_line target_col =
  Layout.hit (Text_surface.layout t.surface) ~row:target_line ~column:target_col

(* Viewport *)

(* The accepted value declares the viewport. The surface's offsets are its
   resolution against the current geometry, recomputed from the value alone,
   so a remounted surface shows the same lines. A value that follows its caret
   moves from its declared position as little as needed to show the caret.
   Both offsets are resolved before either is set, so a paint that resolves
   to the offsets already shown changes nothing and requests no render. *)
let show_viewport t =
  let s = t.surface in
  let x, y, follow = Editor.viewport t.props.state in
  let clamp hi v = max 0 (min v hi) in
  let max_x = Text_surface.max_scroll_x s in
  let max_cursor_x = Text_surface.max_cursor_scroll_x s in
  let max_y = Text_surface.max_scroll_y s in
  let unwrapped = Text_surface.wrap s = `None in
  let sx =
    clamp
      (if Renderable.focused t.node && unwrapped then max_cursor_x else max_x)
      x
  in
  let sy = clamp max_y y in
  let sx, sy =
    if not follow then (sx, sy)
    else
      let w = Renderable.width t.node in
      let h = Renderable.height t.node in
      let dl = find_cursor_display_line t in
      let sy =
        if h <= 0 then sy
        else if dl < sy then clamp max_y dl
        else if dl >= sy + h then clamp max_y (dl - h + 1)
        else sy
      in
      let sx =
        if w <= 0 || not unwrapped then sx
        else
          let cc = cursor_visual_col t dl in
          if cc < sx then clamp max_x cc
          else if cc >= sx + w then clamp max_cursor_x (cc - w + 1)
          else sx
      in
      (sx, sy)
  in
  Text_surface.set_scroll_y s sy;
  (* [sx] is within the cursor's range, which this setter leaves as is. *)
  Text_surface.set_scroll_x_for_cursor s sx

let handle_resize t _node = show_viewport t

let measure_single_line t ~known_dimensions ~available_space:_ ~style:_ =
  let source = Editor.source t.props.state in
  let content_width =
    Layout.Source.columns source
      ~width_method:(Renderable.Private.width_method t.node)
      ~tab_width:(Text_buffer.tab_width t.text_buf)
      ~first:0
      ~last:(Layout.Source.length source)
  in
  let placeholder_width =
    Matrix.Text.measure
      ~width_method:(Renderable.Private.width_method t.node)
      ~tab_width:(Text_buffer.tab_width t.text_buf)
      t.props.placeholder
  in
  (* Reserve one cell for the caret: the cursor sits after the last grapheme,
     so an intrinsically-sized input must be one cell wider than its content
     for the caret to remain inside the node's bounds. *)
  let intrinsic_width =
    Float.of_int (max (content_width + 1) placeholder_width)
  in
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

(* The bytes of [source] that a highlighter is asked for: those of the
   logical lines of the rows the surface shows and of a screen above and below
   them. [source] is the text the surface shows, or the next one, whose lines
   moved little. *)
let view_range t source =
  let layout = Text_surface.layout t.surface in
  let height = max 1 (Renderable.height t.node) in
  let top = Text_surface.scroll_y t.surface in
  let first = Layout.logical_line layout (top - height) in
  let last = Layout.logical_line layout (top + (2 * height) - 1) in
  ( Layout.Source.byte_offset source (Layout.Source.line_start source first),
    Layout.Source.byte_offset source (Layout.Source.line_end source last) )

(* The style of text that is not the editor's own, the placeholder and the
   ghost text: the [Muted] role over the [Field] surface, a label's colors
   over both. *)
let muted_style theme ?fg ?bg ~italic () =
  let s =
    Ansi.Style.merge ~base:(Theme.style theme Field)
      ~overlay:(Theme.style theme Muted)
  in
  let s = Option.fold ~none:s ~some:(fun c -> Ansi.Style.fg c s) fg in
  let s = Option.fold ~none:s ~some:(fun c -> Ansi.Style.bg c s) bg in
  if italic then Ansi.Style.with_italic true s else s

let render_before t _self grid ~dt:_ =
  (* A style the theme gives that differs from the one shown restyles the
     captures, without asking for them again. *)
  let restyled =
    match t.props.syntax with
    | Some syntax when t.styled ->
        let style = syntax_style t syntax in
        style != t.shown_style && not (Syntax_style.equal style t.shown_style)
    | _ -> false
  in
  if Syntax_session.poll t.session || restyled then sync_content t;
  show_viewport t;
  if
    Syntax_session.needs_range t.session
    && Syntax_session.view t.session
         ~range:(view_range t (Editor.source t.props.state))
  then sync_content t;
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else begin
    let x0 = Renderable.x t.node in
    let y0 = Renderable.y t.node in
    let focused = Renderable.focused t.node in
    let theme = Renderable.theme t.node in
    let bg = label_bg t ~focused in
    let color =
      match bg with
      | Some c -> c
      | None ->
          Option.value (Theme.style theme Field).bg
            ~default:Ansi.Color.transparent
    in
    Matrix_grid.clear_rect ~color grid ~x:x0 ~y:y0 ~width:w ~height:h;
    if Editor.length t.props.state = 0 && String.length t.props.placeholder > 0
    then begin
      let style =
        muted_style theme ?fg:t.props.placeholder_color ?bg ~italic:false ()
      in
      Matrix_grid.clip grid { x = x0; y = y0; width = w; height = h } (fun () ->
          Matrix_grid.draw_text ~style grid ~x:x0 ~y:y0
            ~text:t.props.placeholder)
    end;
    match Editor.selection t.props.state with
    | Some (lo, hi) ->
        Text_surface.set_selection_bg t.surface t.props.selection_color;
        Text_surface.set_selection_fg t.surface t.props.selection_fg;
        ignore (Text_surface.set_selection t.surface ~start:lo ~end_:hi : bool)
    | None -> Text_surface.reset_selection t.surface
  end

let render_after t _self grid ~dt:_ =
  if not (Renderable.focused t.node) then ()
  else
    match t.props.ghost_text with
    | None -> ()
    | Some ghost
      when String.length ghost = 0
           || Option.is_some (Editor.selection t.props.state) ->
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
          let bg = label_bg t ~focused:(Renderable.focused t.node) in
          let style =
            muted_style (Renderable.theme t.node) ?fg:t.props.ghost_text_color
              ?bg ~italic:true ()
          in
          Matrix_grid.clip grid { x = x0; y = y0; width = w; height = h }
            (fun () ->
              Matrix_grid.draw_text ~style grid ~x:(x0 + col) ~y:(y0 + row)
                ~text:ghost)

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

let contains t ~x ~y =
  let nx = Renderable.x t.node in
  let ny = Renderable.y t.node in
  x >= nx
  && x < nx + Renderable.width t.node
  && y >= ny
  && y < ny + Renderable.height t.node

(* The caret and the selection are the editor's value, so only a selection
   that starts inside the editor moves them. The renderer also tells the
   selectables beside the one pressed, and those keep their value. *)
let register_selection t =
  if t.props.selectable then
    Renderable.set_selection t.node
      ~should_start:(fun ~x ~y -> contains t ~x ~y)
      ~on_change:(fun sel ->
        let base = t.props.state in
        match sel with
        | None ->
            propose t ~base (Editor.clear_selection base);
            Option.is_some (Editor.selection t.props.state)
        | Some sel ->
            let anchor = Selection.anchor sel and focus = Selection.focus sel in
            if not (contains t ~x:anchor.x ~y:anchor.y) then false
            else
              let nx = Renderable.x t.node and ny = Renderable.y t.node in
              let anchor =
                Text_surface.local_caret t.surface ~x:(anchor.x - nx)
                  ~y:(anchor.y - ny)
              in
              let focus =
                Text_surface.local_caret t.surface ~x:(focus.x - nx)
                  ~y:(focus.y - ny)
              in
              propose t ~base (Editor.select_carets ~anchor ~cursor:focus base);
              Option.is_some (Editor.selection t.props.state))
      ~clear:(fun () ->
        let base = t.props.state in
        propose t ~base (Editor.clear_selection base))
      ~get_text:(fun () -> Editor.selected_text t.props.state)
  else Renderable.unset_selection t.node

(* Vertical movement *)

let move_vertical t ~select ~delta =
  let base = t.props.state in
  let layout = Text_surface.layout t.surface in
  let line = find_cursor_display_line t in
  let target_line = line + delta in
  if target_line < 0 || target_line >= Layout.row_count layout then base
  else
    let col =
      match Editor.preferred_col base with
      | Some col -> col
      | None -> cursor_visual_col t line
    in
    let next = Editor.set_caret ~select (caret_at_col t target_line col) base in
    Editor.propose ~base ~preferred_col:(Some col) next

let move_visual_line_start t ~select =
  let layout = Text_surface.layout t.surface in
  if Layout.row_count layout = 0 then t.props.state
  else
    let grapheme = Layout.row_start layout (find_cursor_display_line t) in
    Editor.set_caret ~select
      Layout.{ grapheme; affinity = Downstream }
      t.props.state

let move_visual_line_end t ~select =
  let layout = Text_surface.layout t.surface in
  if Layout.row_count layout = 0 then t.props.state
  else
    let grapheme = display_line_end_offset t (find_cursor_display_line t) in
    Editor.set_caret ~select
      Layout.{ grapheme; affinity = Upstream }
      t.props.state

(* Key handling *)

let line_home t ~select =
  let base = t.props.state in
  let next = Editor.move_line_start ~select base in
  if
    ((not select) && Option.is_some (Editor.selection base))
    || Editor.cursor next <> Editor.cursor base
  then next
  else if
    Layout.Source.line_of_offset (Editor.source base) (Editor.cursor base) > 0
  then Editor.move_left ~select next
  else next

let line_end t ~select =
  let base = t.props.state in
  let next = Editor.move_line_end ~select base in
  if
    ((not select) && Option.is_some (Editor.selection base))
    || Editor.cursor next <> Editor.cursor base
  then next
  else if
    Layout.Source.line_of_offset (Editor.source base) (Editor.cursor base)
    < Layout.Source.line_count (Editor.source base) - 1
  then Editor.move_right ~select next
  else next

let run_action t action =
  let base = t.props.state in
  match action with
  | Move_left -> Some (Editor.move_left base)
  | Move_right -> Some (Editor.move_right base)
  | Move_up when t.mode = `Multiline ->
      Some (move_vertical t ~select:false ~delta:(-1))
  | Move_down when t.mode = `Multiline ->
      Some (move_vertical t ~select:false ~delta:1)
  | Move_up | Move_down -> None
  | Select_left -> Some (Editor.move_left ~select:true base)
  | Select_right -> Some (Editor.move_right ~select:true base)
  | Select_up when t.mode = `Multiline ->
      Some (move_vertical t ~select:true ~delta:(-1))
  | Select_down when t.mode = `Multiline ->
      Some (move_vertical t ~select:true ~delta:1)
  | Select_up | Select_down -> None
  | Line_home -> Some (line_home t ~select:false)
  | Line_end -> Some (line_end t ~select:false)
  | Select_line_home -> Some (line_home t ~select:true)
  | Select_line_end -> Some (line_end t ~select:true)
  | Visual_line_home -> Some (move_visual_line_start t ~select:false)
  | Visual_line_end -> Some (move_visual_line_end t ~select:false)
  | Select_visual_line_home -> Some (move_visual_line_start t ~select:true)
  | Select_visual_line_end -> Some (move_visual_line_end t ~select:true)
  | Buffer_home -> Some (Editor.move_home base)
  | Buffer_end -> Some (Editor.move_end base)
  | Select_buffer_home -> Some (Editor.move_home ~select:true base)
  | Select_buffer_end -> Some (Editor.move_end ~select:true base)
  | Delete_line -> Some (Editor.delete_line base)
  | Delete_to_line_end -> Some (Editor.delete_to_line_end base)
  | Delete_to_line_start -> Some (Editor.delete_to_line_start base)
  | Backspace -> Some (Editor.delete_backward base)
  | Delete -> Some (Editor.delete_forward base)
  | Newline when t.mode = `Multiline -> Some (Editor.insert "\n" base)
  | Newline | Submit ->
      fire_on_submit t;
      Some (Editor.propose ~base ~preferred_col:None base)
  | Undo -> Some (Editor.undo base)
  | Redo -> Some (Editor.redo base)
  | Word_forward -> Some (Editor.move_word_forward base)
  | Word_backward -> Some (Editor.move_word_backward base)
  | Select_word_forward -> Some (Editor.move_word_forward ~select:true base)
  | Select_word_backward -> Some (Editor.move_word_backward ~select:true base)
  | Delete_word_forward -> Some (Editor.delete_word_forward base)
  | Delete_word_backward -> Some (Editor.delete_word_backward base)
  | Select_all -> Some (Editor.select_all base)

(* An unbound key inserts the text it types, which is empty for a chord, a
   control and a key that types nothing. *)
let key_result_of_event t (data : Matrix_input.Keystroke.t) =
  match Keymap.action t.keymap data with
  | Some action -> run_action t action
  | None when data.text <> "" ->
      Some (Editor.insert (sanitize_text t data.text) t.props.state)
  | None -> None

let handle_key t (ev : Event.key) =
  if not (Event.Key.default_prevented ev) then
    let base = t.props.state in
    match key_result_of_event t (Event.Key.data ev) with
    | None -> ()
    | Some next ->
        Event.Key.prevent_default ev;
        let next =
          Editor.propose ~base ~preferred_col:(Editor.preferred_col next) next
        in
        propose t ~base next

(* Mouse handling *)

(* The wheel proposes a viewport that no longer follows the caret. *)
let handle_scroll t direction delta =
  let delta = Int.max 0 delta in
  let sx = Text_surface.scroll_x t.surface
  and sy = Text_surface.scroll_y t.surface in
  let clamp maximum v = Int.max 0 (Int.min maximum v) in
  let x, y =
    match direction with
    | Event.Mouse.Scroll_up -> (sx, sy - delta)
    | Scroll_down -> (sx, sy + delta)
    | Scroll_left when Text_surface.wrap t.surface = `None -> (sx - delta, sy)
    | Scroll_right when Text_surface.wrap t.surface = `None -> (sx + delta, sy)
    | Scroll_left | Scroll_right -> (sx, sy)
  in
  let x = clamp (Text_surface.max_scroll_x t.surface) x
  and y = clamp (Text_surface.max_scroll_y t.surface) y in
  if x <> sx || y <> sy then
    let base = t.props.state in
    let next = Editor.with_viewport ~x ~y ~follow:false base in
    propose t ~base
      (Editor.propose ~base ~preferred_col:(Editor.preferred_col base) next)

let handle_mouse t ev =
  if Event.Mouse.default_prevented ev then ()
  else
    match Event.Mouse.kind ev with
    | Scroll { direction; delta } ->
        Event.Mouse.prevent_default ev;
        handle_scroll t direction delta
    | _ -> ()

(* Paste *)

let handle_paste t text =
  let base = t.props.state in
  let text = sanitize_text t (Ansi.strip text) in
  propose t ~base (Editor.insert text base)

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ?(mode = `Multiline) ~on_change ?on_submit (props : Props.t) =
  let state = props.state in
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let text_buf = Text_buffer.create () in
  let surface = Text_surface.create node text_buf in
  Text_surface.set_ground surface (Some Field);
  Text_surface.set_wrap surface props.wrap;
  let session =
    Syntax_session.create
      ~notify:(fun () -> Renderable.request_render node)
      ~off_loop:(Renderable.off_loop node)
  in
  (* A view's first request asks for the whole content. *)
  Syntax_session.set session ~content:(Editor.text state) ~range:(0, 0)
    props.syntax;
  let t =
    {
      node;
      text_buf;
      session;
      styled_content = false;
      shown_style = Syntax_style.plain;
      styled = false;
      surface;
      props;
      on_change;
      on_submit;
      mode;
      keymap = Action.keymap ~custom:props.key_bindings mode;
    }
  in
  Renderable.set_render_before node (Some (render_before t));
  Renderable.set_render_after node (Some (render_after t));
  Renderable.set_cursor_provider node (cursor_provider t);
  Renderable.set_on_resize node (Some (handle_resize t));
  Renderable.set_on_focus node (Some (fun _ -> handle_focus t));
  Renderable.set_on_blur node (Some (fun _ -> handle_blur t));
  Renderable.set_key_behavior node (Some (handle_key t));
  Renderable.set_paste_behavior node
    (Some
       (fun ev ->
         if not (Event.Paste.default_prevented ev) then begin
           (* Mark consumed before synchronous proposal callbacks run. *)
           Event.Paste.prevent_default ev;
           handle_paste t (Event.Paste.text ev)
         end));
  (match mode with
  | `Multiline -> ()
  | `Single_line -> Renderable.set_measure node (Some (measure_single_line t)));
  Renderable.on_mouse node (handle_mouse t);
  register_selection t;
  register_text_rows t;
  Renderable.set_pending_provider node
    (Some
       (fun () ->
         if Syntax_session.is_highlighting session then
           let label = Option.bind t.props.syntax Syntax_session.language in
           Some (Renderable.Pending.make ?label ~kind:"textarea.highlight" ())
         else None));
  Renderable.Private.set_cleanup node (fun () -> Syntax_session.cancel session);
  sync t;
  sync_style t ~focused:false;
  t

(* Apply props *)

let apply_props t (props : Props.t) =
  let annotations_changed =
    not (Option.equal ( == ) t.props.annotations props.annotations)
  in
  let syntax_changed =
    not (Option.equal Syntax_session.equal t.props.syntax props.syntax)
  in
  let selectable_changed = t.props.selectable <> props.selectable in
  let keymap_changed =
    not
      (List.equal
         (Keymap.binding_equal ( = ))
         t.props.key_bindings props.key_bindings)
  in
  let style_changed =
    let same = Option.equal Ansi.Color.equal in
    (not (same t.props.text_color props.text_color))
    || (not (same t.props.background_color props.background_color))
    || (not (same t.props.focused_text_color props.focused_text_color))
    || not
         (same t.props.focused_background_color props.focused_background_color)
  in
  let state_changed = t.props.state != props.state in
  let source_changed =
    Editor.source t.props.state != Editor.source props.state
  in
  if t.props.wrap <> props.wrap then Text_surface.set_wrap t.surface props.wrap;
  t.props <- props;
  if keymap_changed then
    t.keymap <- Action.keymap ~custom:props.key_bindings t.mode;
  if selectable_changed then register_selection t;
  if source_changed || syntax_changed then
    Syntax_session.set t.session ~content:(Editor.text props.state)
      ~range:
        (if Syntax_session.needs_range t.session then
           view_range t (Editor.source props.state)
         else (0, 0))
      props.syntax;
  if style_changed then sync_style t ~focused:(Renderable.focused t.node);
  if source_changed || annotations_changed || syntax_changed then sync t;
  if state_changed then show_viewport t;
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
