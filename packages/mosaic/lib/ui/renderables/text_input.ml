(* Defaults *)

let default_cursor_style = `Block
let default_cursor_blinking = true

(* Props *)

type action = Textarea.action =
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

type key_binding = Shortcut.t * action

(* [Props.make] is the one place the input's labels get their defaults:
   [create] and [Mosaic.input] pass their labels on unset. *)
module Props = struct
  type t = {
    state : Editor.t;
    placeholder : string;
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

  let make ~state ?(placeholder = "") ?text_color ?background_color
      ?focused_text_color ?focused_background_color ?placeholder_color
      ?selection_color ?selection_fg ?(cursor_style = default_cursor_style)
      ?(cursor_color = Ansi.Color.default_fg)
      ?(cursor_blinking = default_cursor_blinking) ?(selectable = true)
      ?(show_cursor = true) ?(key_bindings = []) () =
    if Matrix.Text.Layout.Source.line_count (Editor.source state) <> 1 then
      invalid_arg "Mosaic_ui.Text_input: state must not contain CR or LF";
    {
      state;
      placeholder;
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
    && String.equal a.placeholder b.placeholder
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

type t = { surface : Edit_surface.t; mutable props : Props.t }

let surface_props (props : Props.t) =
  Edit_surface.Props.make ~state:props.state ~placeholder:props.placeholder
    ~wrap:`None ?text_color:props.text_color
    ?background_color:props.background_color
    ?focused_text_color:props.focused_text_color
    ?focused_background_color:props.focused_background_color
    ?placeholder_color:props.placeholder_color
    ?selection_color:props.selection_color ?selection_fg:props.selection_fg
    ~cursor_style:props.cursor_style ~cursor_color:props.cursor_color
    ~cursor_blinking:props.cursor_blinking ~selectable:props.selectable
    ~show_cursor:props.show_cursor ~key_bindings:props.key_bindings ()

let node t = Edit_surface.node t.surface
let state t = Edit_surface.state t.surface
let value t = Edit_surface.value t.surface
let cursor t = Edit_surface.cursor t.surface
let selection t = Edit_surface.selection t.surface
let set_on_change t h = Edit_surface.set_on_change t.surface h
let set_on_submit t h = Edit_surface.set_on_submit t.surface h
let handle_paste t text = Edit_surface.handle_paste t.surface text

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ~state
    ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?key_bindings ~on_change ?on_submit () =
  let props =
    Props.make ~state ?placeholder ?text_color ?background_color
      ?focused_text_color ?focused_background_color ?placeholder_color
      ?selection_color ?selection_fg ?cursor_style ?cursor_color
      ?cursor_blinking ?selectable ?show_cursor ?key_bindings ()
  in
  let surface =
    Edit_surface.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~mode:`Single_line ~on_change ?on_submit (surface_props props)
  in
  { surface; props }

let apply_props t (props : Props.t) =
  Edit_surface.apply_props t.surface (surface_props props);
  t.props <- props

let pp ppf t =
  Format.fprintf ppf "Input(%s" (Renderable.id (node t));
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
