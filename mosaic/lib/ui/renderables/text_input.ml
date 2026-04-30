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

(* Props *)

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

type t = { surface : Edit_surface.t; mutable props : Props.t }

let surface_props (props : Props.t) =
  Edit_surface.Props.make
    ~value:(Edit_buffer.strip_newlines props.value)
    ?cursor:props.cursor ?selection:props.selection
    ~placeholder:props.placeholder ~wrap:`None ~text_color:props.text_color
    ~background_color:props.background_color
    ~focused_text_color:props.focused_text_color
    ~focused_background_color:props.focused_background_color
    ~placeholder_color:props.placeholder_color
    ~selection_color:props.selection_color ?selection_fg:props.selection_fg
    ~cursor_style:props.cursor_style ~cursor_color:props.cursor_color
    ~cursor_blinking:props.cursor_blinking ()

let node t = Edit_surface.node t.surface
let buffer t = Edit_surface.buffer t.surface
let value t = Edit_surface.value t.surface
let cursor t = Edit_surface.cursor t.surface
let selection t = Edit_surface.selection t.surface
let set_on_input t h = Edit_surface.set_on_input t.surface h
let set_on_change t h = Edit_surface.set_on_change t.surface h
let set_on_submit t h = Edit_surface.set_on_submit t.surface h
let set_on_cursor t h = Edit_surface.set_on_cursor t.surface h
let handle_paste t text = Edit_surface.handle_paste t.surface text

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?value ?cursor
    ?selection ?placeholder ?max_length ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?on_input ?on_change ?on_submit ?on_cursor () =
  let props =
    Props.make ?value ?cursor ?selection ?placeholder ?max_length ?text_color
      ?background_color ?focused_text_color ?focused_background_color
      ?placeholder_color ?selection_color ?selection_fg ?cursor_style
      ?cursor_color ?cursor_blinking ()
  in
  let surface =
    Edit_surface.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~value:(Edit_buffer.strip_newlines props.value)
      ?cursor:props.cursor ?selection:props.selection
      ~placeholder:props.placeholder ~wrap:`None ~text_color:props.text_color
      ~background_color:props.background_color
      ~focused_text_color:props.focused_text_color
      ~focused_background_color:props.focused_background_color
      ~placeholder_color:props.placeholder_color
      ~selection_color:props.selection_color ?selection_fg:props.selection_fg
      ~cursor_style:props.cursor_style ~cursor_color:props.cursor_color
      ~cursor_blinking:props.cursor_blinking ~mode:`Single_line
      ~max_length:props.max_length ?on_input ?on_change ?on_submit ?on_cursor ()
  in
  { surface; props }

let set_value t s = Edit_surface.set_value t.surface s

let apply_props t (props : Props.t) =
  if t.props.max_length <> props.max_length then
    Edit_surface.set_max_length t.surface props.max_length;
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
