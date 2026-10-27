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
  | Map : ('a -> 'msg) * 'a t -> 'msg t
  | Empty

and 'msg memo = { key : Obj.t; view : Obj.t; build : unit -> 'msg t }

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
  Memo { key; view = Obj.repr view; build = (fun () -> view (Obj.obj key)) }

let memo_equal a b = a.view == b.view && same_memo_key a.key b.key
let memo_view m = m.build ()

let viewport_switch ~at_least_width ~wide ~narrow =
  if at_least_width < 1 then
    invalid_arg "Mosaic.viewport_switch: at_least_width must be positive";
  Viewport_switch { at_least_width; wide; narrow }

(* ───── Elements ───── *)

(* The attributes of an element that declares none, for each default of
   [focusable], and the handlers of one that declares none. Every such
   element shares them, so the reconciler's physical comparison finds them
   unchanged. *)
let unfocusable_attrs =
  {
    style = Toffee.Style.default;
    visible = true;
    z_index = 0;
    opacity = 1.0;
    focusable = false;
    focused = false;
    buffered = false;
    live = false;
  }

let focusable_attrs = { unfocusable_attrs with focusable = true }

let no_handlers =
  {
    on_mouse = None;
    on_key = None;
    on_paste = None;
    on_click = None;
    on_focus_request = None;
    on_size = None;
  }

(* Every element's attributes and handlers. [focusable] is the kind's
   default, and an element that asks for focus is focusable whatever its
   kind. *)
let element ?key ~style ?visible ?z_index ?opacity ~focusable ?focused ?buffered
    ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~callbacks kind children =
  let attrs =
    match
      (visible, z_index, opacity, focused, buffered, live, on_focus_request)
    with
    | None, None, None, None, None, None, None
      when style == Toffee.Style.default ->
        if focusable then focusable_attrs else unfocusable_attrs
    | _ ->
        {
          style;
          visible = Option.value visible ~default:true;
          z_index = Option.value z_index ~default:0;
          opacity = Option.value opacity ~default:1.0;
          focusable = focusable || Option.is_some on_focus_request;
          focused = Option.value focused ~default:false;
          buffered = Option.value buffered ~default:false;
          live = Option.value live ~default:false;
        }
  in
  let handlers =
    match (on_mouse, on_key, on_paste, on_click, on_focus_request, on_size) with
    | None, None, None, None, None, None -> no_handlers
    | _ ->
        {
          on_mouse;
          on_key;
          on_paste;
          on_click;
          on_size;
          on_focus_request =
            Option.map (fun message () -> message) on_focus_request;
        }
  in
  Element { kind; key; attrs; handlers; callbacks; children }

(* ───── Transformation ───── *)

let map f vnode = Map (f, vnode)
