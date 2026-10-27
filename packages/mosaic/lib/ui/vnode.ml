(* ───── Handlers ───── *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg option) option;
  on_key : (Event.key -> 'msg option) option;
  on_paste : (Event.paste -> 'msg option) option;
  on_click : (Event.mouse -> 'msg option) option;
  on_focus_request : (unit -> 'msg) option;
  on_size : (w:int -> h:int -> 'msg option) option;
}

let no_handlers =
  {
    on_mouse = None;
    on_key = None;
    on_paste = None;
    on_click = None;
    on_focus_request = None;
    on_size = None;
  }

(* ───── Element Kind ───── *)

type kind =
  | Box of Box.Props.t
  | Text of Text.Props.t
  | Slider of Slider.Props.t
  | Text_input of Text_input.Props.t
  | Select of Select.Props.t
  | Tab_select of Tab_select.Props.t
  | Canvas
  | Picture of Matrix_grid.picture
  | Spinner of Spinner.Props.t
  | Progress_bar of Progress_bar.Props.t
  | Scroll_bar of Scroll_bar.Props.t
  | Scroll_box of Scroll_box.Props.t
  | Textarea of Textarea.Props.t
  | Table of Table.Props.t
  | Code of Code.Props.t
  | Line_number of Line_number.Props.t
  | Markdown of Markdown.Props.t
  | Diff of Diff.Props.t
  | Tree of Tree.Props.t

(* ───── Widget Callbacks ───── *)

type 'msg widget_callbacks =
  | No_callbacks
  | Slider_callbacks of { on_value_change : (float -> 'msg) option }
  | Input_callbacks of {
      on_change : (Editor.t -> 'msg) option;
      on_submit : (string -> 'msg) option;
    }
  | Select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Tab_select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Canvas_callbacks of { on_draw : (Canvas.t -> dt:float -> unit) option }
  | Scroll_bar_callbacks of { on_change : (int -> 'msg) option }
  | Scroll_box_callbacks of { on_scroll : (Scroll.t -> 'msg) option }
  | Textarea_callbacks of {
      on_change : (Editor.t -> 'msg) option;
      on_submit : (string -> 'msg) option;
    }
  | Code_callbacks of {
      on_selection : ((int * int) option -> 'msg option) option;
    }
  | Markdown_callbacks of {
      on_selection : (string option -> 'msg option) option;
    }
  | Diff_callbacks of { on_line_click : (Diff.line_hit -> 'msg option) option }
  | Table_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
      on_hover : (int option -> 'msg option) option;
    }
  | Tree_callbacks of {
      on_change : (Tree.State.t -> 'msg) option;
      on_activate : (Tree.item -> 'msg) option;
    }
  | Modal_callbacks of { on_close : (unit -> 'msg) option }

(* ───── Attributes ───── *)

type attrs = {
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  focused : bool;
  buffered : bool;
  live : bool;
}

(* ───── Virtual Nodes ───── *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Viewport_switch of 'msg viewport_switch
  | Memo of 'msg memo
  | Embed of Renderable.t
  | Themed of (Theme.t -> Theme.t) * 'msg t
  | Empty

and 'msg memo = {
  key : Obj.t;
  view : Obj.t;
  build : unit -> 'msg t;
  mappings : Obj.t list;
}

and 'msg element = {
  kind : kind;
  key : string option;
  attrs : attrs;
  handlers : 'msg handlers;
  callbacks : 'msg widget_callbacks;
  children : 'msg t list;
}

and 'msg viewport_switch = {
  at_least_width : int;
  wide : 'msg t;
  narrow : 'msg t;
}

(* ───── Constructors ───── *)

let empty = Empty
let fragment children = Fragment children
let embed node = Embed node
let themed f child = Themed (f, child)

(* A memo key is compared by representation (see [memo] in the interface).
   Functions, objects and abstract blocks have no comparable representation and
   are rejected where the key is given. *)
let rec check_memo_key v =
  if Obj.is_block v then
    let tag = Obj.tag v in
    if
      tag = Obj.closure_tag || tag = Obj.infix_tag || tag = Obj.object_tag
      || tag = Obj.abstract_tag || tag = Obj.cont_tag
    then invalid_arg "Vnode.memo: key contains a function or an abstract value"
    else if tag < Obj.no_scan_tag then
      for i = 0 to Obj.size v - 1 do
        check_memo_key (Obj.field v i)
      done

let float_bits v i = Int64.bits_of_float (Obj.double_field v i)

let rec same_memo_key a b =
  a == b
  || Obj.is_block a && Obj.is_block b
     &&
     let tag = Obj.tag a and size = Obj.size a in
     tag = Obj.tag b
     && size = Obj.size b
     &&
     if tag = Obj.double_tag then
       Int64.equal
         (Int64.bits_of_float (Obj.obj a))
         (Int64.bits_of_float (Obj.obj b))
     else if tag = Obj.double_array_tag then
       let rec loop i =
         i = size
         || (Int64.equal (float_bits a i) (float_bits b i) && loop (i + 1))
       in
       loop 0
     else if tag = Obj.string_tag then String.equal (Obj.obj a) (Obj.obj b)
     else if tag < Obj.no_scan_tag then
       let rec loop i =
         i = size
         || (same_memo_key (Obj.field a i) (Obj.field b i) && loop (i + 1))
       in
       loop 0
     else compare a b = 0

let memo ~key view =
  let key = Obj.repr key in
  check_memo_key key;
  Memo
    {
      key;
      view = Obj.repr view;
      build = (fun () -> view (Obj.obj key));
      mappings = [];
    }

let memo_equal a b =
  let rec same_mappings xs ys =
    match (xs, ys) with
    | [], [] -> true
    | x :: xs, y :: ys -> x == y && same_mappings xs ys
    | _ -> false
  in
  a.view == b.view
  && same_mappings a.mappings b.mappings
  && same_memo_key a.key b.key

let memo_view m = m.build ()

let viewport_switch ~at_least_width ~wide ~narrow =
  if at_least_width < 1 then
    invalid_arg "Vnode.viewport_switch: at_least_width must be positive";
  Viewport_switch { at_least_width; wide; narrow }

let box ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?inert ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?border ?border_style ?border_sides ?border_width
    ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment children =
  let kind =
    Box
      (Box.Props.make ?inert ?border ?border_style ?border_sides ?border_width
         ?border_color ?focused_border_color ?background ?fill ?title
         ?title_alignment ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element { kind; key; attrs; handlers; callbacks = No_callbacks; children }

(* In the top layer a modal is laid out against the root, which its size
   fills, and centres its child, as HTML centres a modal dialog in the
   viewport. In its parent's layout it is out of flow. *)
let modal_style =
  let full = Toffee.Style.Dimension.percent 1. in
  Toffee.Style.default
  |> Toffee.Style.set_position Toffee.Style.Position.Absolute
  |> Toffee.Style.set_size { width = full; height = full }
  |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Center)
  |> Toffee.Style.set_justify_content (Some Toffee.Style.Align_content.Center)

let modal ?key ?on_close child =
  match box ?key ~style:modal_style [ child ] with
  | Element e ->
      Element
        {
          e with
          callbacks =
            Modal_callbacks
              { on_close = Option.map (fun message () -> message) on_close };
        }
  | _ -> assert false

let text ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?text_style ?wrap ?selectable ?selection_bg ?selection_fg
    ?tab_width ?truncate content =
  let kind =
    Text
      (Text.Props.make ~content ?text_style ?wrap ?selectable ?selection_bg
         ?selection_fg ?tab_width ?truncate ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let rich_text ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?text_style ?wrap ?selectable ?selection_bg
    ?selection_fg ?tab_width ?truncate content =
  let kind =
    Text
      (Text.Props.make_rich ?text_style ?wrap ?selectable ?selection_bg
         ?selection_fg ?tab_width ?truncate content)
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let slider ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?orientation ?value ?min ?max ?viewport_size ?track_color
    ?thumb_color ?on_value_change () =
  let kind =
    Slider
      (Slider.Props.make ?orientation ?value ?min ?max ?viewport_size
         ?track_color ?thumb_color ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Slider_callbacks { on_value_change } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let input ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = true) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?placeholder ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?selectable ?show_cursor
    ?key_bindings ~on_change ?on_submit ~state () =
  let kind =
    Text_input
      (Text_input.Props.make ~state ?placeholder ?text_color ?background_color
         ?focused_text_color ?focused_background_color ?placeholder_color
         ?selection_color ?selection_fg ?cursor_style ?cursor_color
         ?cursor_blinking ?selectable ?show_cursor ?key_bindings ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Input_callbacks { on_change = Some on_change; on_submit } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let select ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = true) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?options ?selected_index ?background ?text_color
    ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_description ?show_scroll_indicator ?wrap_selection ?key_bindings
    ?item_spacing ?fast_scroll_step ?on_change ?on_activate () =
  let kind =
    Select
      (Select.Props.make ?options ?selected_index ?background ?text_color
         ?focused_background ?focused_text_color ?selected_background
         ?selected_text_color ?description_color ?selected_description_color
         ?show_description ?show_scroll_indicator ?wrap_selection ?key_bindings
         ?item_spacing ?fast_scroll_step ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Select_callbacks { on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let tab_select ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ~options ?selected ?tab_width ?background
    ?text_color ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_underline ?show_description ?show_scroll_arrows ?wrap_selection
    ?key_bindings ?on_change ?on_activate () =
  let kind =
    Tab_select
      (Tab_select.Props.make ~options ?selected ?tab_width ?background
         ?text_color ?focused_background ?focused_text_color
         ?selected_background ?selected_text_color ?description_color
         ?selected_description_color ?show_underline ?show_description
         ?show_scroll_arrows ?wrap_selection ?key_bindings ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Tab_select_callbacks { on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let canvas ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?on_draw () =
  let kind = Canvas in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Canvas_callbacks { on_draw } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let picture ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size picture =
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    {
      kind = Picture picture;
      key;
      attrs;
      handlers;
      callbacks = No_callbacks;
      children = [];
    }

let spinner ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = true) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?frame_set ?color () =
  let kind = Spinner (Spinner.Props.make ?frame_set ?color ()) in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let progress_bar ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?value ?min ?max ?orientation ?filled_color
    ?empty_color () =
  let kind =
    Progress_bar
      (Progress_bar.Props.make ?value ?min ?max ?orientation ?filled_color
         ?empty_color ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let scroll_bar ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?position ?scroll_size ?viewport_size
    ?orientation ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg
    ?on_change () =
  let kind =
    Scroll_bar
      (Scroll_bar.Props.make ?position ?scroll_size ?viewport_size ?orientation
         ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Scroll_bar_callbacks { on_change } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let scroll_box ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?scroll_x ?scroll_y ?background ?show_scrollbars
    ?scrollbar_style ?vertical_bar_style ?horizontal_bar_style ~on_scroll
    ~scroll children =
  let kind =
    Scroll_box
      (Scroll_box.Props.make ~scroll ?scroll_x ?scroll_y ?background
         ?show_scrollbars ?scrollbar_style ?vertical_bar_style
         ?horizontal_bar_style ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Scroll_box_callbacks { on_scroll = Some on_scroll } in
  Element { kind; key; attrs; handlers; callbacks; children }

let textarea ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?annotations ?syntax ?ghost_text
    ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?selectable ?show_cursor ?key_bindings ~on_change ?on_submit ~state () =
  let kind =
    Textarea
      (Textarea.Props.make ~state ?annotations ?syntax ?ghost_text
         ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
         ?focused_text_color ?focused_background_color ?placeholder_color
         ?selection_color ?selection_fg ?cursor_style ?cursor_color
         ?cursor_blinking ?selectable ?show_cursor ?key_bindings ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks =
    Textarea_callbacks { on_change = Some on_change; on_submit }
  in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let table ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = true) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?columns ?rows ?selected_row ?border ?border_style ?show_header
    ?show_column_separator ?show_row_separator ?cell_padding ?header_color
    ?header_background ?text_color ?background ?selected_text_color
    ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?selection_visible ?show_scroll_indicator
    ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
    ?key_bindings ?fast_scroll_step ?on_change ?on_activate ?on_hover () =
  let kind =
    Table
      (Table.Props.make ?columns ?rows ?selected_row ?border ?border_style
         ?show_header ?show_column_separator ?show_row_separator ?cell_padding
         ?header_color ?header_background ?text_color ?background
         ?selected_text_color ?selected_background ?focused_selected_text_color
         ?focused_selected_background ?selection_visible ?show_scroll_indicator
         ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
         ?key_bindings ?fast_scroll_step ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Table_callbacks { on_change; on_activate; on_hover } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let code ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?syntax ?conceal ?text_style ?wrap ?tab_width ?truncate ?selectable
    ?selection_bg ?selection_fg ?on_selection content =
  let kind =
    Code
      (Code.Props.make ~content ?syntax ?conceal ?text_style ?wrap ?tab_width
         ?truncate ?selectable ?selection_bg ?selection_fg ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Code_callbacks { on_selection } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let line_number ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?fg ?bg ?min_width ?padding_right
    ?show_line_numbers ?line_number_offset ?line_colors ?line_signs
    ?line_numbers ?hidden_line_numbers child =
  let kind =
    Line_number
      (Line_number.Props.make ?fg ?bg ?min_width ?padding_right
         ?show_line_numbers ?line_number_offset ?line_colors ?line_signs
         ?line_numbers ?hidden_line_numbers ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  Element
    {
      kind;
      key;
      attrs;
      handlers;
      callbacks = No_callbacks;
      children = [ child ];
    }

let markdown ?key ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(focused = false)
    ?(buffered = false) ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click
    ?on_focus_request ?on_size ?md_style ?conceal ?streaming ?selectable
    ?selection_bg ?selection_fg ?highlighter ?syntax_style ?on_selection content
    =
  let kind =
    Markdown
      (Markdown.Props.make ~content ?style:md_style ?conceal ?streaming
         ?selectable ?selection_bg ?selection_fg ?highlighter ?syntax_style ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Markdown_callbacks { on_selection } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let diff ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = false) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?layout ?theme ?highlight ?line_highlights ?line_spans ?line_signs
    ?show_line_numbers ?wrap ?selectable ?text_style ?on_line_click patch =
  let kind =
    Diff
      (Diff.Props.make ~patch ?layout ?theme ?highlight ?line_highlights
         ?line_spans ?line_signs ?show_line_numbers ?wrap ?selectable
         ?text_style ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Diff_callbacks { on_line_click } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let tree ?key ?(style = Toffee.Style.default) ?(visible = true) ?(z_index = 0)
    ?(opacity = 1.0) ?(focusable = true) ?(focused = false) ?(buffered = false)
    ?(live = false) ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ?items ?indent_size ?show_guides ?guide_style ?expand_icon
    ?collapse_icon ?leaf_icon ?background ?text_color ?selected_background
    ?selected_text_color ?focused_selected_background
    ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
    ?key_bindings ?fast_scroll_step ~on_change ?on_activate ~state () =
  let kind =
    Tree
      (Tree.Props.make ~state ?items ?indent_size ?show_guides ?guide_style
         ?expand_icon ?collapse_icon ?leaf_icon ?background ?text_color
         ?selected_background ?selected_text_color ?focused_selected_background
         ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
         ?key_bindings ?fast_scroll_step ())
  in
  let attrs =
    {
      style;
      visible;
      z_index;
      opacity;
      focusable = focusable || Option.is_some on_focus_request;
      focused;
      buffered;
      live;
    }
  in
  let handlers =
    {
      on_mouse;
      on_key;
      on_paste;
      on_click;
      on_size;
      on_focus_request = Option.map (fun message () -> message) on_focus_request;
    }
  in
  let callbacks = Tree_callbacks { on_change = Some on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

(* ───── Transformation ───── *)

let map_handlers (f : 'a -> 'b) (h : 'a handlers) : 'b handlers =
  {
    on_mouse = Option.map (fun g ev -> Option.map f (g ev)) h.on_mouse;
    on_key = Option.map (fun g ev -> Option.map f (g ev)) h.on_key;
    on_paste = Option.map (fun g ev -> Option.map f (g ev)) h.on_paste;
    on_click = Option.map (fun g ev -> Option.map f (g ev)) h.on_click;
    on_focus_request = Option.map (fun g () -> f (g ())) h.on_focus_request;
    on_size = Option.map (fun g ~w ~h -> Option.map f (g ~w ~h)) h.on_size;
  }

let map_callbacks (f : 'a -> 'b) : 'a widget_callbacks -> 'b widget_callbacks =
  function
  | No_callbacks -> No_callbacks
  | Slider_callbacks { on_value_change } ->
      Slider_callbacks
        { on_value_change = Option.map (fun g v -> f (g v)) on_value_change }
  | Input_callbacks { on_change; on_submit } ->
      Input_callbacks
        {
          on_change = Option.map (fun g value -> f (g value)) on_change;
          on_submit = Option.map (fun g value -> f (g value)) on_submit;
        }
  | Select_callbacks { on_change; on_activate } ->
      Select_callbacks
        {
          on_change = Option.map (fun g i -> f (g i)) on_change;
          on_activate = Option.map (fun g i -> f (g i)) on_activate;
        }
  | Tab_select_callbacks { on_change; on_activate } ->
      Tab_select_callbacks
        {
          on_change = Option.map (fun g i -> f (g i)) on_change;
          on_activate = Option.map (fun g i -> f (g i)) on_activate;
        }
  | Canvas_callbacks { on_draw } -> Canvas_callbacks { on_draw }
  | Scroll_bar_callbacks { on_change } ->
      Scroll_bar_callbacks
        { on_change = Option.map (fun g i -> f (g i)) on_change }
  | Scroll_box_callbacks { on_scroll } ->
      Scroll_box_callbacks
        { on_scroll = Option.map (fun g value -> f (g value)) on_scroll }
  | Textarea_callbacks { on_change; on_submit } ->
      Textarea_callbacks
        {
          on_change = Option.map (fun g value -> f (g value)) on_change;
          on_submit = Option.map (fun g value -> f (g value)) on_submit;
        }
  | Code_callbacks { on_selection } ->
      Code_callbacks
        {
          on_selection =
            Option.map (fun g sel -> Option.map f (g sel)) on_selection;
        }
  | Markdown_callbacks { on_selection } ->
      Markdown_callbacks
        {
          on_selection =
            Option.map (fun g sel -> Option.map f (g sel)) on_selection;
        }
  | Diff_callbacks { on_line_click } ->
      Diff_callbacks
        {
          on_line_click =
            Option.map (fun g hit -> Option.map f (g hit)) on_line_click;
        }
  | Table_callbacks { on_change; on_activate; on_hover } ->
      Table_callbacks
        {
          on_change = Option.map (fun g i -> f (g i)) on_change;
          on_activate = Option.map (fun g i -> f (g i)) on_activate;
          on_hover = Option.map (fun g i -> Option.map f (g i)) on_hover;
        }
  | Tree_callbacks { on_change; on_activate } ->
      Tree_callbacks
        {
          on_change = Option.map (fun g value -> f (g value)) on_change;
          on_activate = Option.map (fun g value -> f (g value)) on_activate;
        }
  | Modal_callbacks { on_close } ->
      Modal_callbacks { on_close = Option.map (fun g () -> f (g ())) on_close }

let rec map (f : 'a -> 'b) (vnode : 'a t) : 'b t =
  match vnode with
  | Empty -> Empty
  | Memo m ->
      Memo
        {
          m with
          build = (fun () -> map f (m.build ()));
          mappings = Obj.repr f :: m.mappings;
        }
  | Embed node -> Embed node
  | Themed (theme, child) -> Themed (theme, map f child)
  | Fragment children -> Fragment (List.map (map f) children)
  | Viewport_switch { at_least_width; wide; narrow } ->
      Viewport_switch
        { at_least_width; wide = map f wide; narrow = map f narrow }
  | Element e ->
      Element
        {
          kind = e.kind;
          key = e.key;
          attrs = e.attrs;
          handlers = map_handlers f e.handlers;
          callbacks = map_callbacks f e.callbacks;
          children = List.map (map f) e.children;
        }

(* ───── Layout Helpers ───── *)

let px n = Toffee.Style.Dimension.length (Float.of_int n)
let pct n = Toffee.Style.Dimension.pct (Float.of_int n)
let auto = Toffee.Style.Dimension.auto
let size ~width ~height = Toffee.Geometry.Size.make (px width) (px height)

let gap n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Size.make lp lp

let padding n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lp

let margin n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa

let inset n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa
