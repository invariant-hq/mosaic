open Mosaic_ui

(* ───── Handlers ───── *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg option) option;
  on_key : (Event.key -> 'msg option) option;
  on_paste : (Event.paste -> 'msg option) option;
  on_click : (Event.mouse -> 'msg option) option;
  on_focus_request : (unit -> 'msg) option;
  on_size : (w:int -> h:int -> 'msg option) option;
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
  | Text_callbacks of {
      on_selection : ((int * int) option -> 'msg option) option;
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

(* A memo key is compared by representation (see [Mosaic.memo]).
   Functions, objects and abstract blocks have no comparable representation and
   are rejected where the key is given. *)
let rec check_memo_key v =
  if Obj.is_block v then
    let tag = Obj.tag v in
    if
      tag = Obj.closure_tag || tag = Obj.infix_tag || tag = Obj.object_tag
      || tag = Obj.abstract_tag || tag = Obj.cont_tag
    then invalid_arg "Mosaic.memo: key contains a function or an abstract value"
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
    invalid_arg "Mosaic.viewport_switch: at_least_width must be positive";
  Viewport_switch { at_least_width; wide; narrow }

(* ───── Elements ───── *)

(* Every element's attributes and handlers. [focusable] is the kind's
   default, and an element that asks for focus is focusable whatever its
   kind. *)
let element ?key ~style ?(visible = true) ?(z_index = 0) ?(opacity = 1.0)
    ~focusable ?(focused = false) ?(buffered = false) ?(live = false) ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ~callbacks kind
    children =
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
  Element { kind; key; attrs; handlers; callbacks; children }

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
  | Text_callbacks { on_selection } ->
      Text_callbacks
        {
          on_selection =
            Option.map (fun g sel -> Option.map f (g sel)) on_selection;
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
