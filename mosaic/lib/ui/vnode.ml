(* ───── Handlers ───── *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg) option;
  on_key : (Event.key -> 'msg) option;
  on_paste : (Event.paste -> 'msg) option;
}

let no_handlers = { on_mouse = None; on_key = None; on_paste = None }

(* ───── Element Kind ───── *)

type kind =
  | Box of Box.Props.t
  | Text of Text.Props.t
  | Slider of Slider.Props.t
  | Text_input of Text_input.Props.t
  | Select of Select.Props.t
  | Tab_select of Tab_select.Props.t
  | Canvas of Canvas.Props.t
  | Spinner of Spinner.Props.t
  | Progress_bar of Progress_bar.Props.t
  | Scroll_bar of Scroll_bar.Props.t
  | Scroll_box of Scroll_box.Props.t
  | Textarea of Textarea.Props.t
  | Table of Table.Props.t
  | Code of Code.Props.t
  | Line_number of Line_number.Props.t
  | Markdown of Markdown.Props.t
  | Tree of Tree.Props.t

(* ───── Widget Callbacks ───── *)

type 'msg widget_callbacks =
  | No_callbacks
  | Slider_callbacks of { on_value_change : (float -> 'msg) option }
  | Input_callbacks of {
      on_input : (string -> 'msg) option;
      on_change : (string -> 'msg) option;
      on_submit : (string -> 'msg) option;
      on_cursor : (cursor:int -> selection:(int * int) option -> 'msg) option;
    }
  | Select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Tab_select_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Canvas_callbacks of { on_draw : (Canvas.t -> delta:float -> unit) option }
  | Scroll_bar_callbacks of { on_change : (int -> 'msg) option }
  | Scroll_box_callbacks of { on_scroll : (x:int -> y:int -> 'msg) option }
  | Textarea_callbacks of {
      on_input : (string -> 'msg) option;
      on_change : (string -> 'msg) option;
      on_submit : (string -> 'msg) option;
      on_cursor : (cursor:int -> selection:(int * int) option -> 'msg) option;
    }
  | Code_callbacks of { on_selection : ((int * int) option -> 'msg) option }
  | Table_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
    }
  | Tree_callbacks of {
      on_change : (int -> 'msg) option;
      on_activate : (int -> 'msg) option;
      on_expand : (int -> bool -> 'msg) option;
    }

(* ───── Attributes ───── *)

type attrs = {
  id : string option;
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  autofocus : bool;
  buffered : bool;
  live : bool;
  ref : (Renderable.t -> unit) option;
}

(* ───── Virtual Nodes ───── *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Embed of Renderable.t
  | Empty

and 'msg element = {
  kind : kind;
  key : string option;
  attrs : attrs;
  handlers : 'msg handlers;
  callbacks : 'msg widget_callbacks;
  children : 'msg t list;
}

(* ───── Constructors ───── *)

let empty = Empty
let fragment children = Fragment children
let embed node = Embed node

let box ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment children =
  let kind =
    Box
      (Box.Props.make ?border ?border_style ?border_sides ?border_color
         ?focused_border_color ?background ?fill ?title ?title_alignment ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element { kind; key; attrs; handlers; callbacks = No_callbacks; children }

let text ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?text_style ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width
    ?truncate content =
  let kind =
    Text
      (Text.Props.make ~content ?text_style ?wrap ?selectable ?selection_bg
         ?selection_fg ?tab_width ?truncate ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let slider ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?orientation ?value ?min ?max ?viewport_size ?track_color ?thumb_color
    ?on_value_change () =
  let kind =
    Slider
      (Slider.Props.make ?orientation ?value ?min ?max ?viewport_size
         ?track_color ?thumb_color ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Slider_callbacks { on_value_change } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let input ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?value
    ?cursor ?selection ?placeholder ?max_length ?text_color ?background_color
    ?focused_text_color ?focused_background_color ?placeholder_color
    ?selection_color ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking
    ?on_input ?on_change ?on_submit ?on_cursor () =
  let kind =
    Text_input
      (Text_input.Props.make ?value ?cursor ?selection ?placeholder ?max_length
         ?text_color ?background_color ?focused_text_color
         ?focused_background_color ?placeholder_color ?selection_color
         ?selection_fg ?cursor_style ?cursor_color ?cursor_blinking ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks =
    Input_callbacks { on_input; on_change; on_submit; on_cursor }
  in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let select ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?options ?selected_index ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_description
    ?show_scroll_indicator ?wrap_selection ?item_spacing ?fast_scroll_step
    ?on_change ?on_activate () =
  let kind =
    Select
      (Select.Props.make ?options ?selected_index ?background ?text_color
         ?focused_background ?focused_text_color ?selected_background
         ?selected_text_color ?description_color ?selected_description_color
         ?show_description ?show_scroll_indicator ?wrap_selection ?item_spacing
         ?fast_scroll_step ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Select_callbacks { on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let tab_select ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ~options ?selected ?tab_width ?background ?text_color ?focused_background
    ?focused_text_color ?selected_background ?selected_text_color
    ?description_color ?selected_description_color ?show_underline
    ?show_description ?show_scroll_arrows ?wrap_selection ?on_change
    ?on_activate () =
  let kind =
    Tab_select
      (Tab_select.Props.make ~options ?selected ?tab_width ?background
         ?text_color ?focused_background ?focused_text_color
         ?selected_background ?selected_text_color ?description_color
         ?selected_description_color ?show_underline ?show_description
         ?show_scroll_arrows ?wrap_selection ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Tab_select_callbacks { on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let canvas ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?respect_alpha ?on_draw () =
  let kind = Canvas (Canvas.Props.make ?respect_alpha ()) in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Canvas_callbacks { on_draw } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let spinner ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = true) ?ref ?on_mouse ?on_key ?on_paste
    ?frame_set ?color () =
  let kind = Spinner (Spinner.Props.make ?frame_set ?color ()) in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let progress_bar ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?value
    ?min ?max ?orientation ?filled_color ?empty_color () =
  let kind =
    Progress_bar
      (Progress_bar.Props.make ?value ?min ?max ?orientation ?filled_color
         ?empty_color ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let scroll_bar ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?orientation ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg
    ?on_change () =
  let kind =
    Scroll_bar
      (Scroll_bar.Props.make ?orientation ?show_arrows ?track_color ?thumb_color
         ?arrow_fg ?arrow_bg ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Scroll_bar_callbacks { on_change } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let scroll_box ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?scroll_x ?scroll_y ?sticky_scroll ?sticky_start ?background ?on_scroll
    children =
  let kind =
    Scroll_box
      (Scroll_box.Props.make ?scroll_x ?scroll_y ?sticky_scroll ?sticky_start
         ?background ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Scroll_box_callbacks { on_scroll } in
  Element { kind; key; attrs; handlers; callbacks; children }

let textarea ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?value
    ?cursor ?selection ?highlights ?ghost_text ?ghost_text_color ?placeholder
    ?wrap ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit
    ?on_cursor () =
  let kind =
    Textarea
      (Textarea.Props.make ?value ?cursor ?selection ?highlights ?ghost_text
         ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
         ?focused_text_color ?focused_background_color ?placeholder_color
         ?selection_color ?selection_fg ?cursor_style ?cursor_color
         ?cursor_blinking ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks =
    Textarea_callbacks { on_input; on_change; on_submit; on_cursor }
  in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let table ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?columns ?rows ?selected_row ?border ?border_style ?show_header
    ?show_column_separator ?show_row_separator ?cell_padding ?header_color
    ?header_background ?text_color ?background ?selected_text_color
    ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?row_styles ?wrap_selection ?fast_scroll_step
    ?on_change ?on_activate () =
  let kind =
    Table
      (Table.Props.make ?columns ?rows ?selected_row ?border ?border_style
         ?show_header ?show_column_separator ?show_row_separator ?cell_padding
         ?header_color ?header_background ?text_color ?background
         ?selected_text_color ?selected_background ?focused_selected_text_color
         ?focused_selected_background ?row_styles ?wrap_selection
         ?fast_scroll_step ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Table_callbacks { on_change; on_activate } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let code ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?highlights ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection content =
  let kind =
    Code
      (Code.Props.make ~content ?highlights ?text_style ?wrap ?tab_width
         ?truncate ?selectable ?selection_bg ?selection_fg ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Code_callbacks { on_selection } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

let line_number ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?fg ?bg
    ?min_width ?padding_right ?show_line_numbers ?line_number_offset
    ?line_colors ?line_signs ?line_numbers ?hidden_line_numbers child =
  let kind =
    Line_number
      (Line_number.Props.make ?fg ?bg ?min_width ?padding_right
         ?show_line_numbers ?line_number_offset ?line_colors ?line_signs
         ?line_numbers ?hidden_line_numbers ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element
    {
      kind;
      key;
      attrs;
      handlers;
      callbacks = No_callbacks;
      children = [ child ];
    }

let markdown ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = false) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste
    ?md_style ?conceal ?streaming content =
  let kind =
    Markdown
      (Markdown.Props.make ~content ?style:md_style ?conceal ?streaming ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  Element
    { kind; key; attrs; handlers; callbacks = No_callbacks; children = [] }

let tree ?key ?id ?(style = Toffee.Style.default) ?(visible = true)
    ?(z_index = 0) ?(opacity = 1.0) ?(focusable = true) ?(autofocus = false)
    ?(buffered = false) ?(live = false) ?ref ?on_mouse ?on_key ?on_paste ?items
    ?selected_index ?expand_depth ?indent_size ?show_guides ?guide_style
    ?expand_icon ?collapse_icon ?leaf_icon ?background ?text_color
    ?selected_background ?selected_text_color ?focused_selected_background
    ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
    ?fast_scroll_step ?on_change ?on_activate ?on_expand () =
  let kind =
    Tree
      (Tree.Props.make ?items ?selected_index ?expand_depth ?indent_size
         ?show_guides ?guide_style ?expand_icon ?collapse_icon ?leaf_icon
         ?background ?text_color ?selected_background ?selected_text_color
         ?focused_selected_background ?focused_selected_text_color ?guide_color
         ?icon_color ?wrap_selection ?fast_scroll_step ())
  in
  let attrs =
    {
      id;
      style;
      visible;
      z_index;
      opacity;
      focusable;
      autofocus;
      buffered;
      live;
      ref;
    }
  in
  let handlers = { on_mouse; on_key; on_paste } in
  let callbacks = Tree_callbacks { on_change; on_activate; on_expand } in
  Element { kind; key; attrs; handlers; callbacks; children = [] }

(* ───── Transformation ───── *)

let map_handlers (f : 'a -> 'b) (h : 'a handlers) : 'b handlers =
  {
    on_mouse = Option.map (fun g ev -> f (g ev)) h.on_mouse;
    on_key = Option.map (fun g ev -> f (g ev)) h.on_key;
    on_paste = Option.map (fun g ev -> f (g ev)) h.on_paste;
  }

let map_callbacks (f : 'a -> 'b) : 'a widget_callbacks -> 'b widget_callbacks =
  function
  | No_callbacks -> No_callbacks
  | Slider_callbacks { on_value_change } ->
      Slider_callbacks
        { on_value_change = Option.map (fun g v -> f (g v)) on_value_change }
  | Input_callbacks { on_input; on_change; on_submit; on_cursor } ->
      Input_callbacks
        {
          on_input = Option.map (fun g s -> f (g s)) on_input;
          on_change = Option.map (fun g s -> f (g s)) on_change;
          on_submit = Option.map (fun g s -> f (g s)) on_submit;
          on_cursor =
            Option.map
              (fun g ~cursor ~selection -> f (g ~cursor ~selection))
              on_cursor;
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
        { on_scroll = Option.map (fun g ~x ~y -> f (g ~x ~y)) on_scroll }
  | Textarea_callbacks { on_input; on_change; on_submit; on_cursor } ->
      Textarea_callbacks
        {
          on_input = Option.map (fun g s -> f (g s)) on_input;
          on_change = Option.map (fun g s -> f (g s)) on_change;
          on_submit = Option.map (fun g s -> f (g s)) on_submit;
          on_cursor =
            Option.map
              (fun g ~cursor ~selection -> f (g ~cursor ~selection))
              on_cursor;
        }
  | Code_callbacks { on_selection } ->
      Code_callbacks
        { on_selection = Option.map (fun g sel -> f (g sel)) on_selection }
  | Table_callbacks { on_change; on_activate } ->
      Table_callbacks
        {
          on_change = Option.map (fun g i -> f (g i)) on_change;
          on_activate = Option.map (fun g i -> f (g i)) on_activate;
        }
  | Tree_callbacks { on_change; on_activate; on_expand } ->
      Tree_callbacks
        {
          on_change = Option.map (fun g i -> f (g i)) on_change;
          on_activate = Option.map (fun g i -> f (g i)) on_activate;
          on_expand =
            Option.map (fun g i expanded -> f (g i expanded)) on_expand;
        }

let rec map (f : 'a -> 'b) (vnode : 'a t) : 'b t =
  match vnode with
  | Empty -> Empty
  | Embed node -> Embed node
  | Fragment children -> Fragment (List.map (map f) children)
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
